{-# LANGUAGE CPP, FlexibleInstances #-}
{-|
Overview

This module implements the shared Acton compilation pipeline that both the
acton CLI and the LSP server drive. It builds a dependency graph across
projects, runs the front passes (parse, kinds, types) to produce .tydb interface
files and diagnostics, and then emits the back passes (normalizer through
codegen) as separate jobs.

The design is intentionally incremental and event-friendly. While the compiler
scheduler is running and actively compiling modules, we can receive a new event
about a modified module, cancel running tasks and restart compilation of that
module from that point. This is used by watch mode and LSP where updates to
files or in-editor buffers trigger recompilation. Dependencies are considered so
that updates to a module only cause the affected part of the subgraph, i.e. the
modified module and downstream dependencies to be recompiled. Cached .tydb headers
are reused when public module interfaces are unchanged. Per-name pub/impl hashes
stored in the header drive front-pass vs back-pass decisions, and moduleImplHash
is embedded in generated .c/.h to detect stale codegen. Inter-module
dependencies only depend on the front passes completion, which is why they are
separated from back jobs. As soon as the front passes of a module have
completed, dependent modules can start compilation while the back pass job is
scheduled in the background. This maximizes parallelism. It is only the front
passes that can generate user facing errors, which means that the LSP server can
return with feedback to the user (module type checked successfully) immediately
after the front passes are done while the back passes can be run asynchronously
in the background. This maximizes responsiveness of the LSP server.

The CLI/LSP layers handle progress UI, file watching, and event sources; this
module focuses on deterministic compilation and structured callbacks.

Call flow:
  - Shared pipeline (acton build, acton watch, LSP):
    1) prepareCompilePlan (via prepareCompileContext, readModuleTask/
       buildGlobalTasks/readImports, and selectNeededTasks/selectAffectedTasks)
       builds a CompilePlan for the requested subgraph. Parsing is performed
       through parseActSource/parseActSnapshot/parseActFile so SourceProvider
       overlays can supply unsaved buffers.
    2) runCompilePlan drives compileTasks. Whole-module BackJobs can enter the
       scheduler's BackQueue as soon as their front passes finish and overlap
       remaining front work. Selective jobs retain only reloadable metadata;
       after the required fronts have committed their interfaces, one global
       reachability closure prepares the deferred batch.
    3) Callers either wait for backQueueWait (CLI builds) or let back jobs run
       in the background. CLI builds wait before invoking Zig.
    4) runBackJobs/runBackPasses can be used for standalone back-pass execution
       when a caller already has BackJobs to run.
  - Triggering:
    - acton build runs the pipeline once for the requested files/project.
    - acton watch and LSP call startCompile on each event; callers can gate
      output with generation checks (e.g. whenCurrentGen). BackQueue ignores
      back jobs for stale generations, and front-output jobs are skipped for
      stale generations before writing.
    - Callers may supply a delay (debounce) before startCompile runs (LSP uses
      debounceMicros on change events; acton watch uses 0).
  - Finalization:
    - CLI waits on backQueueWait before running Zig build
    - LSP does not implicitly run the Zig build, it is only run explicitly when
      the user asks to build / run a module

State and orchestration:
  - Acton.Compile holds only small in-process caches (tyPathCache, pubHashCache,
    and nameHashCache) to speed up .tydb lookups; all orchestration state is
    owned by the caller.
  - Callers allocate and hold a CompileScheduler from this module. The
    scheduler encapsulates mutable state (generation id, cancelable async
    handle, build-spec stamp, and a shared back-queue); callers pass it into
    startCompile and backQueueEnqueue rather than mutating it directly.
  - LSP additionally keeps overlaysRef for in-memory buffers and builds an
    overlay-aware SourceProvider on top of disk reads. acton watch reads
    directly from disk.
  - Each event bumps the generation via startCompile; callers may pass a delay
    (LSP uses debounceMicros, acton watch uses 0). Back jobs and front-output
    writes are filtered by generation; front-pass diagnostics should be gated
    by the caller if needed.
  - CLI builds share the same pipeline. Whole back jobs can overlap front work;
    selective jobs are prepared after the required front passes finish.

TODO:
  - Make generation invalidation more precise so unrelated in-flight modules
    (e.g. long-running B) are not discarded when A changes.
-}
module Acton.Compile
  ( Paths(..)
  , ProjCtx(..)
  , TaskKey(..)
  , GlobalTask(..)
  , CompileTask(..)
  , BackInput(..)
  , BackJob(..)
  , DeferredBackJob(..)
  , FrontResult(..)
  , FrontOutputKind(..)
  , FrontOutputProgress(..)
  , FrontOutputJob
  , waitFrontOutputJobs
  , FrontTiming(..)
  , TypeStmtTiming(..)
  , InferredSignature(..)
  , BackTiming(..)
  , BackPass(..)
  , BackPassProgress(..)
  , backPassName
  , FrontPass(..)
  , FrontPassProgress(..)
  , ParseProgress(..)
  , CompileCallbacks(..)
  , defaultCompileCallbacks
  , BackJobCallbacks(..)
  , defaultBackJobCallbacks
  , BackQueue
  , newBackQueue
  , backQueueEnqueue
  , backQueueWait
  , CompileScheduler(..)
  , newCompileScheduler
  , startCompile
  , whenCurrentGen
  , BuildSpecStamp(..)
  , readBuildSpecStamp
  , checkBuildSpecChange
  , CompileContext(..)
  , prepareCompileContext
  , CompilePlan(..)
  , prepareCompilePlan
  , prepareCompilePlanFromContext
  , CompileHooks(..)
  , defaultCompileHooks
  , runCompilePlan
  , compileOutputLockPaths
  , withCompileOutputLockPaths
  , CompileFailure(..)
  , compileFailureMessage
  , BackPassFailure(..)
  , backPassFailureMessage
  , BackJobResult(..)
  , defaultCompileOptions
  , ProjectError(..)
  , throwProjectError
  , fetchDependencies
  , parseActSource
  , parseActSnapshot
  , parseActFile
  , readModuleTask
  , readModuleDoc
  , readModuleDocIndexEntry
  , readImports
  , special_projects
  , buildGlobalTasks
  , selectNeededTasks
  , selectAffectedTasks
  , libraryBoundaryTasks
  , compileTasks
  , runFrontPasses
  , docNameCountThreshold
  , shouldGenerateDocOutput
  , runBackPasses
  , runBackJobs
  , findProjectDir
  , BackgroundCompilerLock
  , backgroundCompilerLockPath
  , tryBackgroundCompilerLock
  , releaseBackgroundCompilerLock
  , projectLockPath
  , withProjectLockOnWait
  , withProjectLock
  , findPaths
  , discoverProjects
  , pathsForModule
  , searchPathForProject
  , moduleNameFromFile
  , enumerateProjectModules
  , normalizeDepOverrides
  , applyDepOverrides
  , collectDepTypePaths
  , depTypePathsFromMap
  , resolveDepBase
  , loadBuildSpec
  , filterActFile
  , srcFile
  , outBase
  , tyDbPath
  , srcBase
  , getModPath
  , modNameToFilename
  , modNameToString
  , nameToString
  , addProjPrefix
  , dropProjPrefix
  , dropProjPrefixOrLib
  , importsOf
  , quiet
  , altOutput
  , missingIfaceDiagnostics
  , getPubHashCached
  , getImplHashCached
  , updatePubHashCache
  , rootEligible
  , normalizePathSafe
  , isAbsolutePath
  , collapseDots
  , rebasePath
  ) where

import Prelude hiding (readFile, writeFile)

import qualified Acton.Parser
import Acton.Parser (CustomParseError, CustomParseException, ChunkScanError(..), ContextError, IndentationError)
import qualified Acton.Syntax as A
import qualified Acton.NameInfo as I
import Text.Megaparsec.Error (ParseErrorBundle)
import qualified Acton.CommandLineParser as C
import Acton.Printer ()
import qualified Acton.Env
import qualified Acton.TypeEnv
import Acton.Env (simp, define, setMod)
import qualified Acton.Hashing as Hashing
import qualified Acton.InterfaceRows as InterfaceRows
import qualified Acton.InterfaceRowsBuilder as InterfaceRowsBuilder
import qualified Acton.ReachabilityRowsBuilder as ReachabilityRowsBuilder
import qualified Acton.ReachabilityTypes as Reachability
import qualified Acton.SelectiveBack as SelectiveBack
import qualified Acton.SelectiveWorklist as SelectiveWorklist
import qualified Acton.Names as Names
import qualified Acton.Kinds
import qualified Acton.Types
import qualified Acton.Converter as Converter
import qualified Acton.Normalizer
import qualified Acton.CPS
import qualified Acton.Deactorizer
import qualified Acton.LambdaLifter
import Acton.Analytics
import qualified Acton.Boxing
import qualified Acton.CodeGen
import Acton.Builtin (mBuiltin)
import Acton.Prim (mPrim)
import qualified Acton.BuildSpec as BuildSpec
import qualified Acton.DocPrinter as DocP
import qualified Acton.Fingerprint as Fingerprint
import qualified Acton.Diagnostics as Diag
import qualified Acton.SourceProvider as Source
import Utils
import qualified Pretty
import Pretty hiding ((<>))
import qualified InterfaceFiles

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent (forkIO, myThreadId, threadCapability, threadDelay)
import Control.Concurrent.STM (TChan, TVar, atomically, check, modifyTVar', newTChanIO, newTVarIO, readTChan, readTVar, writeTChan)
import Control.DeepSeq (rnf)
import Control.Exception (Exception, IOException, SomeAsyncException, SomeException, bracketOnError, catch, displayException, evaluate, finally, fromException, mask_, throwIO, try)
import Control.Monad
import Data.Bits (shiftL, shiftR, (.|.))
import Data.Char (isAlpha, isDigit, isHexDigit, isSpace)
import Data.Either (partitionEithers)
import Data.Graph
import Data.List (find, foldl', intercalate, intersperse, isPrefixOf, isSuffixOf, nub, partition)
import qualified Data.List
import Data.IORef
import Data.Maybe (catMaybes, isJust, isNothing, listToMaybe, mapMaybe)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import Data.Ord (Down(..))
import qualified Data.Set
import Data.Time.Clock (UTCTime)
import Data.Word (Word32, Word64)
import Error.Diagnose (Diagnostic)
import GHC.Conc (getNumCapabilities)
import Acton.ArchiveDownload (downloadArchive)
import Acton.HttpFetch (isHttpUrl, proxyEnvReportLines, readProxyEnv)
import qualified Acton.Zon as Zon
import System.Clock
import System.Directory
import System.Directory.Recursive (getFilesRecursive)
import System.Environment (getExecutablePath, lookupEnv)
import System.FileLock (FileLock, SharedExclusive(Exclusive), tryLockFile, unlockFile, withFileLock)
import System.FilePath ((</>))
import System.FilePath.Posix
import System.Exit (ExitCode(..))
import System.IO hiding (readFile, writeFile)
import System.IO.Temp (createTempDirectory, withSystemTempDirectory)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Files (FileStatus, deviceID, fileID, fileSize, getFileStatus, modificationTimeHiRes, statusChangeTimeHiRes)
import System.Process (CreateProcess(cwd), readCreateProcessWithExitCode, proc)
import System.Random (randomRIO)
import Text.PrettyPrint (renderStyle, style, Style(..), Mode(PageMode))
import Text.Show.Pretty (ppDoc)
import Text.Printf

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Base16 as Base16
import qualified Crypto.Hash.SHA256 as SHA256


newtype ProjectError = ProjectError String deriving (Show)

instance Exception ProjectError where
  displayException (ProjectError msg) = msg

newtype FrontOutputError = FrontOutputError String deriving (Show)

instance Exception FrontOutputError where
  displayException (FrontOutputError msg) = msg

-- | Raise a ProjectError for library callers.
-- Used by project discovery and path helpers to stop on unrecoverable errors.
throwProjectError :: String -> IO a
throwProjectError msg = throwIO (ProjectError msg)


data CompileFailure
  = CompileCycleFailure String
  | CompileBuiltinFailure
  | CompileInternalFailure String
  deriving (Eq, Show)

-- | Render internal compile failures as user-facing messages.
-- Centralizes wording so CLI output and tests stay consistent.
compileFailureMessage :: CompileFailure -> String
compileFailureMessage (CompileCycleFailure msg) = msg
compileFailureMessage CompileBuiltinFailure = "Builtin compilation failed"
compileFailureMessage (CompileInternalFailure msg) = msg


data BackPassFailure = BackPassFailure
  { bpfKey :: TaskKey
  , bpfMessage :: String
  } deriving (Eq, Show)

backPassFailureMessage :: BackPassFailure -> String
backPassFailureMessage (BackPassFailure key msg) =
  "Back pass failed for " ++ tkProj key ++ "/" ++ modNameToString (tkMod key) ++ ": " ++ msg

data BackJobResult
  = BackJobOk (Maybe TimeSpec) (Maybe BackTiming)
  | BackJobFailed BackPassFailure
  deriving (Eq, Show)

data BackPass
  = BackPassNormalize
  | BackPassDeactorize
  | BackPassCPS
  | BackPassLLift
  | BackPassBoxing
  | BackPassCodeGen
  | BackPassRender
  | BackPassWrite
  deriving (Eq, Show)

backPassName :: BackPass -> String
backPassName BackPassNormalize  = "normalize"
backPassName BackPassDeactorize = "deactorize"
backPassName BackPassCPS        = "cps"
backPassName BackPassLLift      = "llift"
backPassName BackPassBoxing     = "boxing"
backPassName BackPassCodeGen    = "codegen"
backPassName BackPassRender     = "render"
backPassName BackPassWrite      = "write"

data BackPassProgress
  = BackPassStarted BackPass Int Int
  | BackPassFinished BackPass Int Int TimeSpec
  | BackPassSkipped BackPass Int Int
  deriving (Eq, Show)

data FrontPass
  = FrontPassKinds
  | FrontPassTypes
  | FrontPassHash
  deriving (Eq, Show)

data FrontPassProgress = FrontPassProgress
  { fppPass :: FrontPass
  , fppCompleted :: Int
  , fppTotal :: Int
  , fppCurrent :: Maybe String
  } deriving (Eq, Show)

data ParseProgress = ParseProgress
  { ppCompleted :: Int
  , ppTotal :: Int
  } deriving (Eq, Show)

data TypeStmtTiming = TypeStmtTiming
  { tstCompleted :: Int
  , tstTotal :: Int
  , tstLabel :: String
  , tstNames :: [String]
  , tstTime :: TimeSpec
  } deriving (Eq, Show)

data InferredSignature = InferredSignature
  { isigNames :: [String]
  , isigSignature :: String
  } deriving (Eq, Show)

data FrontOutputKind
  = FrontOutputTydb
  | FrontOutputTydbCopy
  | FrontOutputDoc
  deriving (Eq, Ord, Show)

data FrontOutputProgress = FrontOutputProgress
  { fopLabel :: String
  , fopRatio :: Double
  } deriving (Eq, Show)

data FrontTiming = FrontTiming
  { ftEnv :: TimeSpec
  , ftKinds :: TimeSpec
  , ftTypes :: TimeSpec
  , ftTypeReconstruct :: TimeSpec
  , ftTypeAfterProgress :: TimeSpec
  , ftTypeForce :: TimeSpec
  , ftTypeHash :: TimeSpec
  , ftTypeStmtTimings :: [TypeStmtTiming]
  } deriving (Eq, Show)

data BackTiming = BackTiming
  { btNormalize :: TimeSpec
  , btDeactorize :: TimeSpec
  , btCPS :: TimeSpec
  , btLLift :: TimeSpec
  , btBoxing :: TimeSpec
  , btCodeGen :: TimeSpec
  , btRender :: TimeSpec
  , btWrite :: Maybe TimeSpec
  } deriving (Eq, Show)


data CompileCallbacks = CompileCallbacks
  { ccOnDiagnostics :: GlobalTask -> C.CompileOptions -> [Diagnostic String] -> IO ()
  , ccOnParseStart :: GlobalTask -> C.CompileOptions -> IO ()
  , ccOnParseProgress :: GlobalTask -> C.CompileOptions -> ParseProgress -> IO ()
  , ccOnParseDone :: GlobalTask -> C.CompileOptions -> Maybe TimeSpec -> IO ()
  , ccOnFrontResult :: GlobalTask -> C.CompileOptions -> FrontResult -> IO ()
  , ccOnFrontStart :: GlobalTask -> C.CompileOptions -> IO ()
  , ccOnFrontDone :: GlobalTask -> C.CompileOptions -> IO ()
  , ccOnFrontProgress :: GlobalTask -> C.CompileOptions -> FrontPassProgress -> IO ()
  , ccOnFrontOutputStart :: TaskKey -> FrontOutputKind -> IO ()
  , ccOnFrontOutputProgress :: TaskKey -> FrontOutputKind -> FrontOutputProgress -> IO ()
  , ccOnFrontOutputDone :: TaskKey -> FrontOutputKind -> Maybe TimeSpec -> IO ()
  , ccShouldWriteFrontOutput :: IO Bool
  , ccOnBackJob :: BackJob -> IO ()
  , ccOnBackSkipped :: TaskKey -> IO ()
  , ccOnReachability :: Data.Set.Set A.ModName -> SelectiveWorklist.Selection -> IO ()
  , ccOnInfo :: String -> IO ()
  }

-- | Default no-op callbacks for the compilation pipeline.
-- Callers can start from this and override only the events they care about.
defaultCompileCallbacks :: CompileCallbacks
defaultCompileCallbacks = CompileCallbacks
  { ccOnDiagnostics = \_ _ _ -> return ()
  , ccOnParseStart = \_ _ -> return ()
  , ccOnParseProgress = \_ _ _ -> return ()
  , ccOnParseDone = \_ _ _ -> return ()
  , ccOnFrontResult = \_ _ _ -> return ()
  , ccOnFrontStart = \_ _ -> return ()
  , ccOnFrontDone = \_ _ -> return ()
  , ccOnFrontProgress = \_ _ _ -> return ()
  , ccOnFrontOutputStart = \_ _ -> return ()
  , ccOnFrontOutputProgress = \_ _ _ -> return ()
  , ccOnFrontOutputDone = \_ _ _ -> return ()
  , ccShouldWriteFrontOutput = return True
  , ccOnBackJob = \_ -> return ()
  , ccOnBackSkipped = \_ -> return ()
  , ccOnReachability = \_ _ -> return ()
  , ccOnInfo = \_ -> return ()
  }

data BackJobCallbacks = BackJobCallbacks
  { bjcOnStart :: BackJob -> IO ()
  , bjcOnProgress :: BackJob -> BackPassProgress -> IO ()
  , bjcOnDone :: BackJob -> BackJobResult -> IO ()
  }

-- | Default no-op callbacks for back-pass execution.
defaultBackJobCallbacks :: BackJobCallbacks
defaultBackJobCallbacks =
  BackJobCallbacks
    { bjcOnStart = \_ -> return ()
    , bjcOnProgress = \_ _ -> return ()
    , bjcOnDone = \_ _ -> return ()
    }

data BackQueue = BackQueue
  { backQueueEnqueue :: Int -> BackJob -> BackJobCallbacks -> IO Bool
  , backQueueWait :: Int -> IO (Maybe BackPassFailure)
  }

data BuildSpecStamp = BuildSpecStamp
  { bssBuildAct :: Maybe UTCTime
  } deriving (Eq, Show)

readBuildSpecStamp :: FilePath -> IO BuildSpecStamp
readBuildSpecStamp projDir = do
  buildAct <- stampFor "Build.act"
  return BuildSpecStamp
    { bssBuildAct = buildAct
    }
  where
    stampFor name = do
      let path = projDir </> name
      exists <- doesFileExist path
      if exists
        then Just <$> getModificationTime path
        else return Nothing

checkBuildSpecChange :: CompileScheduler -> BuildSpecStamp -> IO Bool
checkBuildSpecChange sched stamp =
  atomicModifyIORef' (csBuildStampRef sched) $ \prev ->
    let changed = prev /= Just stamp
    in (Just stamp, changed)

-- | Create a concurrent back-pass queue gated by a generation counter.
-- Each job is tagged with a generation id; stale jobs are skipped.
newBackQueue :: IORef Int -> MVar () -> C.GlobalOptions -> Int -> IO BackQueue
newBackQueue genRef generationLock gopts maxPar = do
  queue <- newTChanIO
  counts <- newTVarIO M.empty
  failures <- newTVarIO M.empty
  let incPending gen = M.insertWith (+) gen 1
      decPending gen m =
        case M.lookup gen m of
          Nothing -> m
          Just n ->
            let n' = n - 1
            in if n' <= 0
                 then M.delete gen m
                 else M.insert gen n' m
      recordFailure gen failure =
        M.insertWith (\_ old -> old) gen failure
      enqueue gen job callbacks = do
        current <- readIORef genRef
        if current /= gen
          then return False
          else do
            atomically $ do
              modifyTVar' counts (incPending gen)
              writeTChan queue (gen, job, callbacks)
            return True
      waitDone gen = atomically $ do
        pending <- readTVar counts
        check (M.findWithDefault 0 gen pending == 0)
        failuresNow <- readTVar failures
        modifyTVar' failures (M.delete gen)
        return (M.lookup gen failuresNow)
      worker = forever $ do
        (gen, job, callbacks) <- atomically $ readTChan queue
        process gen job callbacks
          `finally` atomically (modifyTVar' counts $ decPending gen)

      process gen job callbacks = do
        current <- readIORef genRef
        when (current == gen) $ do
          failed <- M.member gen <$> atomically (readTVar failures)
          unless failed $ do
            let commitWrite action = withMVar generationLock $ \_ -> do
                  currentWrite <- readIORef genRef
                  if currentWrite == gen
                    then action >> return True
                    else return False
            res <- (try $ do
                      bjcOnStart callbacks job
                      result <- runBackPasses
                        gopts (bjOpts job) (bjPaths job) (bjInput job)
                        commitWrite (bjcOnProgress callbacks job)
                      bjValidate job
                      return result)
                    :: IO (Either SomeException (Maybe TimeSpec, Maybe BackTiming))
            currentDone <- readIORef genRef
            when (currentDone == gen) $ publish gen job callbacks res

      publish gen job callbacks res = do
        result <- case res of
          Left err -> do
            let failure = jobFailure job err
            atomically $ modifyTVar' failures (recordFailure gen failure)
            return (BackJobFailed failure)
          Right (t,bt) -> return (BackJobOk t bt)
        done <- try (bjcOnDone callbacks job result) :: IO (Either SomeException ())
        case done of
          Left err -> atomically $ modifyTVar' failures
            (recordFailure gen $ jobFailure job err)
          Right () -> return ()

      jobFailure job err = BackPassFailure key (displayException err)
        where key = TaskKey
                (projPath $ bjPaths job)
                (A.modname $ biTypedMod $ bjInput job)
  let workers = max 1 maxPar
  replicateM_ workers (forkIO worker)
  return BackQueue
    { backQueueEnqueue = enqueue
    , backQueueWait = waitDone
    }

data CompileScheduler = CompileScheduler
  { csGenRef :: IORef Int
  , csGenerationLock :: MVar ()
  , csAsyncRef :: MVar (Maybe (Async ()))
  , csBackQueue :: BackQueue
  , csBuildStampRef :: IORef (Maybe BuildSpecStamp)
  }

-- | Create a scheduler with generation tracking and a shared back queue.
newCompileScheduler :: C.GlobalOptions -> Int -> IO CompileScheduler
newCompileScheduler gopts maxPar = do
  genRef <- newIORef 0
  generationLock <- newMVar ()
  asyncRef <- newMVar Nothing
  backQueue <- newBackQueue genRef generationLock gopts maxPar
  buildStampRef <- newIORef Nothing
  return CompileScheduler
    { csGenRef = genRef
    , csGenerationLock = generationLock
    , csAsyncRef = asyncRef
    , csBackQueue = backQueue
    , csBuildStampRef = buildStampRef
    }

-- | Start a new compile action, canceling and joining any in-flight run.
-- Returns the generation id associated with this run.
startCompile :: CompileScheduler -> Int -> (Int -> IO ()) -> IO Int
startCompile sched delay run = do
  gen <- withMVar (csGenerationLock sched) $ \_ ->
    atomicModifyIORef' (csGenRef sched) $ \g -> let g' = g + 1 in (g', g')
  modifyMVar_ (csAsyncRef sched) $ \m -> do
    forM_ m $ \old -> do
      -- The canceled action drains front-output jobs in its finalizer; wait so
      -- stale writes cannot overlap the next generation.
      cancel old
      waitCatch old >> return ()
    a <- async $ do
      when (delay > 0) $ threadDelay delay
      current <- readIORef (csGenRef sched)
      when (current == gen) $ run gen
    return (Just a)
  return gen

-- | Run an action only if the generation still matches.
whenCurrentGen :: CompileScheduler -> Int -> IO () -> IO ()
whenCurrentGen sched gen action = do
  current <- readIORef (csGenRef sched)
  when (current == gen) action

data CompileContext = CompileContext
  { ccOpts :: C.CompileOptions
  , ccDepOverrides :: [(String, FilePath)]
  , ccPathsRoot :: Paths
  , ccRootProj :: FilePath
  , ccSysAbs :: FilePath
  , ccBuildStamp :: BuildSpecStamp
  }

prepareCompileContext :: C.CompileOptions -> [FilePath] -> IO CompileContext
prepareCompileContext opts srcFiles = do
  when (null srcFiles) $
    throwProjectError "No source files found"
  cwd <- getCurrentDirectory
  maybeRoot <- findProjectDir (takeDirectory (head srcFiles))
  let baseForOverrides = maybe cwd id maybeRoot
  depOverrides <- normalizeDepOverrides baseForOverrides (C.dep_overrides opts)
  let opts' = opts { C.dep_overrides = depOverrides }
  pathsRoot <- findPaths (head srcFiles) opts'
  rootProj <- normalizePathSafe (projPath pathsRoot)
  sysAbs <- normalizePathSafe (sysPath pathsRoot)
  buildStamp <- readBuildSpecStamp (projPath pathsRoot)
  return CompileContext
    { ccOpts = opts'
    , ccDepOverrides = depOverrides
    , ccPathsRoot = pathsRoot
    , ccRootProj = rootProj
    , ccSysAbs = sysAbs
    , ccBuildStamp = buildStamp
    }

-- | Prepare a compile plan, refreshing dependencies if the build spec changed.
prepareCompilePlan :: Source.SourceProvider
                   -> C.GlobalOptions
                   -> CompileScheduler
                   -> C.CompileOptions
                   -> [FilePath]
                   -> Bool
                   -> Maybe [FilePath]
                   -> IO CompilePlan
prepareCompilePlan sp gopts sched opts srcFiles allowPrune mChangedPaths = do
  ctx <- prepareCompileContext opts srcFiles
  specChanged <- checkBuildSpecChange sched (ccBuildStamp ctx)
  when specChanged $
    fetchDependencies gopts (ccPathsRoot ctx) (ccDepOverrides ctx)
  let mChanged = if specChanged then Nothing else mChangedPaths
  prepareCompilePlanFromContext sp gopts ctx srcFiles allowPrune mChanged

data CompilePlan = CompilePlan
  { cpContext :: CompileContext
  , cpProjMap :: M.Map FilePath ProjCtx
  , cpGlobalTasks :: [GlobalTask]
  , cpNeededTasks :: [GlobalTask]
  , cpDbpBlocked :: Data.Set.Set TaskKey
  , cpRootTasks :: [CompileTask]
  , cpRootTaskKeys :: Data.Set.Set TaskKey
  , cpRequestedTasks :: Data.Set.Set TaskKey
  , cpRootPins :: M.Map String BuildSpec.PkgDep
  , cpIncremental :: Bool
  , cpAllowPrune :: Bool
  , cpChangedPaths :: Maybe [FilePath]
  , cpSrcFiles :: [FilePath]
  }

prepareCompilePlanFromContext :: Source.SourceProvider
                              -> C.GlobalOptions
                              -> CompileContext
                              -> [FilePath]
                              -> Bool
                              -> Maybe [FilePath]
                              -> IO CompilePlan
prepareCompilePlanFromContext sp gopts ctx srcFiles allowPrune mChangedPaths = do
  let opts' = ccOpts ctx
      depOverrides = ccDepOverrides ctx
      pathsRoot = ccPathsRoot ctx
      rootProj = ccRootProj ctx
      sysAbs = ccSysAbs ctx
      incremental = isJust mChangedPaths
      allowPrune' = allowPrune && not incremental
  projMap <- if isTmp pathsRoot
    then do
      let ctx' = ProjCtx
            { projRoot = rootProj
            , projOutDir = projOut pathsRoot
            , projTypesDir = projTypes pathsRoot
            , projSrcDir = srcDir pathsRoot
            , projSysPath = sysAbs
            , projSysTypes = joinPath [sysAbs, "base", "out", "types"]
            , projBuildSpec = scratchBuildSpec rootProj
            , projDeps = []
            }
      return (M.singleton rootProj ctx')
    else discoverProjects gopts sysAbs rootProj depOverrides
  -- Keep generated module artifacts in sync with source removals.
  -- For full builds, scan and prune all orphan outputs.
  -- For incremental builds, prune only modules whose changed .act paths are now missing.
  if incremental
    then maybe (return ()) (pruneMissingChangedModuleOutputs (M.elems projMap)) mChangedPaths
    else mapM_ pruneMissingModuleOutputs (M.elems projMap)
  let hasBuildLibraries = any (not . M.null . BuildSpec.libraries . projBuildSpec) (M.elems projMap)
  (globalTasks, _) <- buildGlobalTasks sp gopts opts' projMap
    (if incremental || allowPrune || hasBuildLibraries then Nothing else Just srcFiles)
  let dbpBlocked = libraryBoundaryTasks projMap globalTasks
  requestedKeys <- taskKeysForFiles pathsRoot rootProj globalTasks srcFiles
  neededTasks0 <- case mChangedPaths of
    Nothing -> selectNeededTasks pathsRoot rootProj globalTasks srcFiles
    Just changed -> selectAffectedTasks rootProj opts' globalTasks dbpBlocked changed
  let neededTasks = expandBuildLibraryTasks projMap globalTasks neededTasks0
      rootTasks = [ gtTask t | t <- neededTasks0, tkProj (gtKey t) == rootProj ]
      rootTaskKeys = Data.Set.fromList
        [ gtKey t | t <- neededTasks0, tkProj (gtKey t) == rootProj ]
      requestedTasks
        | allowPrune = Data.Set.empty
        | otherwise = Data.Set.fromList requestedKeys
      rootPins = maybe M.empty (BuildSpec.dependencies . projBuildSpec) (M.lookup rootProj projMap)
  return CompilePlan
    { cpContext = ctx
    , cpProjMap = projMap
    , cpGlobalTasks = globalTasks
    , cpNeededTasks = neededTasks
    , cpDbpBlocked = dbpBlocked
    , cpRootTasks = rootTasks
    , cpRootTaskKeys = rootTaskKeys
    , cpRequestedTasks = requestedTasks
    , cpRootPins = rootPins
    , cpIncremental = incremental
    , cpAllowPrune = allowPrune'
    , cpChangedPaths = mChangedPaths
    , cpSrcFiles = srcFiles
    }

data CompileHooks = CompileHooks
  { chOnDiagnostics :: GlobalTask -> C.CompileOptions -> [Diagnostic String] -> IO ()
  , chOnParseStart :: GlobalTask -> IO ()
  , chOnParseProgress :: GlobalTask -> ParseProgress -> IO ()
  , chOnParseDone :: GlobalTask -> Maybe TimeSpec -> IO ()
  , chOnFrontStart :: GlobalTask -> IO ()
  , chOnFrontDone :: GlobalTask -> IO ()
  , chOnFrontProgress :: GlobalTask -> FrontPassProgress -> IO ()
  , chOnFrontResult :: GlobalTask -> FrontResult -> IO ()
  , chOnFrontOutputStart :: TaskKey -> FrontOutputKind -> IO ()
  , chOnFrontOutputProgress :: TaskKey -> FrontOutputKind -> FrontOutputProgress -> IO ()
  , chOnFrontOutputDone :: TaskKey -> FrontOutputKind -> Maybe TimeSpec -> IO ()
  , chOnBackQueued :: TaskKey -> Bool -> IO ()
  , chOnBackSkipped :: TaskKey -> IO ()
  , chOnBackStart :: BackJob -> IO ()
  , chOnBackProgress :: BackJob -> BackPassProgress -> IO ()
  , chOnBackDone :: BackJob -> BackJobResult -> IO ()
  , chOnInfo :: String -> IO ()
  }

defaultCompileHooks :: CompileHooks
defaultCompileHooks =
  CompileHooks
    { chOnDiagnostics = \_ _ _ -> return ()
    , chOnParseStart = \_ -> return ()
    , chOnParseProgress = \_ _ -> return ()
    , chOnParseDone = \_ _ -> return ()
    , chOnFrontStart = \_ -> return ()
    , chOnFrontDone = \_ -> return ()
    , chOnFrontProgress = \_ _ -> return ()
    , chOnFrontResult = \_ _ -> return ()
    , chOnFrontOutputStart = \_ _ -> return ()
    , chOnFrontOutputProgress = \_ _ _ -> return ()
    , chOnFrontOutputDone = \_ _ _ -> return ()
    , chOnBackQueued = \_ _ -> return ()
    , chOnBackSkipped = \_ -> return ()
    , chOnBackStart = \_ -> return ()
    , chOnBackProgress = \_ _ -> return ()
    , chOnBackDone = \_ _ -> return ()
    , chOnInfo = \_ -> return ()
    }

runCompilePlan :: Source.SourceProvider
               -> C.GlobalOptions
               -> CompilePlan
               -> CompileScheduler
               -> Int
               -> CompileHooks
               -> IO (Either CompileFailure (Acton.Env.Env0, Bool))
runCompilePlan sp gopts plan sched gen hooks0 = withAnalytics $ \ana -> do
  let hooks = instrumentHooksForAnalytics ana hooks0
      ctx = cpContext plan
      opts' = ccOpts ctx
      pathsRoot = ccPathsRoot ctx
      rootProj = ccRootProj ctx
      backQueue = csBackQueue sched
      backCallbacks = BackJobCallbacks
        { bjcOnStart = chOnBackStart hooks
        , bjcOnProgress = chOnBackProgress hooks
        , bjcOnDone = chOnBackDone hooks
        }
      callbacks = defaultCompileCallbacks
        { ccOnDiagnostics = \t optsT diags -> chOnDiagnostics hooks t optsT diags
        , ccOnParseStart = \t _ -> chOnParseStart hooks t
        , ccOnParseProgress = \t _ p -> chOnParseProgress hooks t p
        , ccOnParseDone = \t _ mtime -> chOnParseDone hooks t mtime
        , ccOnFrontResult = \t _ fr -> chOnFrontResult hooks t fr
        , ccOnFrontStart = \t _ -> chOnFrontStart hooks t
        , ccOnFrontDone = \t _ -> chOnFrontDone hooks t
        , ccOnFrontProgress = \t _ p -> chOnFrontProgress hooks t p
        , ccOnFrontOutputStart = chOnFrontOutputStart hooks
        , ccOnFrontOutputProgress = chOnFrontOutputProgress hooks
        , ccOnFrontOutputDone = chOnFrontOutputDone hooks
        , ccShouldWriteFrontOutput = do
            current <- readIORef (csGenRef sched)
            return (current == gen)
        , ccOnBackJob = \job -> do
            let key = TaskKey (projPath (bjPaths job)) (A.modname (biTypedMod (bjInput job)))
            enqueued <- backQueueEnqueue backQueue gen job backCallbacks
            chOnBackQueued hooks key enqueued
        , ccOnBackSkipped = chOnBackSkipped hooks
        , ccOnInfo = chOnInfo hooks
        }
  compileTasks sp gopts opts' pathsRoot rootProj
    (cpRootTaskKeys plan) (cpRequestedTasks plan)
    (cpNeededTasks plan) (cpDbpBlocked plan) callbacks

-- | Selective C/H is keyed by one consumer universe but lives in the provider
-- project's output tree. Hold every participating output lock, in canonical
-- order, through back passes and native consumption so concurrent root builds
-- cannot overwrite a shared provider projection underneath each other.
-- Discover the output locks before reading any shared interface state.
-- Dependency fetches happen before this point; project discovery itself is
-- read-only with respect to compiler outputs.
compileOutputLockPaths :: C.GlobalOptions -> CompileContext -> IO [FilePath]
compileOutputLockPaths gopts ctx
  | isTmp paths = return [lockPath root]
  | otherwise = do
      projects <- discoverProjects gopts (ccSysAbs ctx) root (ccDepOverrides ctx)
      return [ lockPath (projRoot project) | project <- M.elems projects ]
  where
    paths = ccPathsRoot ctx
    root = ccRootProj ctx
    lockPath projectRoot = joinPath [projectRoot, ".acton.output.lock"]

withCompileOutputLockPaths :: [FilePath] -> IO a -> IO a
withCompileOutputLockPaths = lockAll . Data.List.sort . nub
  where
    lockAll [] action = action
    lockAll (path:paths) action =
      withFileLock path Exclusive $ \_ -> lockAll paths action

-- | Tap the generic progress hooks to feed the analytics sampler. Each pass
-- already reports through these callbacks, so a single wrapper covers parse,
-- type check and the back passes without touching any individual pass.
instrumentHooksForAnalytics :: Analytics -> CompileHooks -> CompileHooks
instrumentHooksForAnalytics ana hooks = hooks
  { chOnParseDone = \t mtime -> do
      recordParseDone ana (taskMod t)
      chOnParseDone hooks t mtime
  , chOnFrontProgress = \t p -> do
      when (fppPass p == FrontPassTypes) $
        recordType ana (taskMod t) (fppCompleted p) (fppTotal p)
      chOnFrontProgress hooks t p
  , chOnBackProgress = \job bp -> do
      case bp of
        BackPassFinished pass _ _ _ -> recordBackPass ana (backMod job) (backPassName pass)
        _                           -> return ()
      chOnBackProgress hooks job bp
  }
  where taskMod t   = modNameToString (tkMod (gtKey t))
        backMod job = modNameToString (A.modname (biTypedMod (bjInput job)))

-- | Baseline compile options for internal helpers and tests.
-- This mirrors CLI defaults so path discovery and parsing behave predictably
-- when no command-line flags are present.
defaultCompileOptions :: C.CompileOptions
defaultCompileOptions =
  C.CompileOptions
    { C.alwaysbuild = False
    , C.ignore_compiler_version = False
    , C.db = False
    , C.parse = False
    , C.parse_ast = False
    , C.kinds = False
    , C.types = False
    , C.sigs = False
    , C.norm = False
    , C.deact = False
    , C.cps = False
    , C.llift = False
    , C.box = False
    , C.hgen = False
    , C.cgen = False
    , C.ccmd = False
    , C.tydb = False
    , C.cpedantic = False
    , C.dbg_no_lines = False
    , C.no_dbp = False
    , C.optimize = C.Debug
    , C.only_build = False
    , C.skip_build = False
    , C.watch = False
    , C.no_threads = False
    , C.parse_serial = False
    , C.root = ""
    , C.tempdir = ""
    , C.syspath = ""
    , C.target = C.defTarget
    , C.cpu = ""
    , C.test = False
    , C.searchpath = []
    , C.dep_overrides = []
    }

-- | Debug helper for pass dumps.
-- Prints a header and footer around the given text so pass output is easy to
-- spot in verbose logs.
dump :: A.ModName -> String -> String -> IO ()
dump mn h txt =
  putStrLn ("\n\n== " ++ h ++ ": " ++ modNameToString mn ++ " ================================\n" ++ txt
            ++ '\n' : replicate (38 + length h + length (modNameToString mn)) '=' ++ "\n")

-- | Compute the subdirectory for a module within a types root.
-- Used when creating directories before writing .tydb files.
getModPath :: FilePath -> A.ModName -> FilePath
getModPath path mn =
  joinPath [path, joinPath $ init $ A.modPath mn]

-- Global caches (process‑wide) to reduce repeated .tydb lookups during parallel builds
--
-- We deliberately create top‑level MVars via unsafePerformIO and mark them
-- NOINLINE so their initializer runs exactly once. Without NOINLINE, GHC could
-- inline or float the initializer, accidentally creating multiple MVars under
-- optimization. This guarantees a single cache per process.
--
-- Thread‑safety: all access goes through modifyMVar/modifyMVar_ so read/modify/
-- write is atomic. These caches are best‑effort for performance; correctness
-- does not depend on them. On a miss we fall back to reading .tydb headers from
-- disk. After a successful compile we update pubHashCache so dependents in
-- this process see the new public hash.
--
-- tyPathCache    :: ModName -> absolute .tydb path (resolved from searchPath)
-- pubHashCache   :: ModName -> current public hash (from header or compile)
-- implHashCache  :: ModName -> current implementation hash (from header or compile)
-- nameHashCache  :: ModName -> per-name hash info (from header or compile)
{-# NOINLINE tyPathCache #-}
tyPathCache :: MVar (M.Map A.ModName FilePath)
tyPathCache = unsafePerformIO (newMVar M.empty)

{-# NOINLINE pubHashCache #-}
pubHashCache :: MVar (M.Map A.ModName B.ByteString)
pubHashCache = unsafePerformIO (newMVar M.empty)

{-# NOINLINE implHashCache #-}
implHashCache :: MVar (M.Map A.ModName B.ByteString)
implHashCache = unsafePerformIO (newMVar M.empty)

{-# NOINLINE nameHashCache #-}
nameHashCache :: MVar (M.Map A.ModName (M.Map A.Name InterfaceFiles.NameHashInfo))
nameHashCache = unsafePerformIO (newMVar M.empty)

-- | Per-name results of targeted name-hash reads. Kept apart from
-- nameHashCache, whose per-module maps must stay complete.
{-# NOINLINE nameHashOneCache #-}
nameHashOneCache :: MVar (M.Map (A.ModName, A.Name) (Maybe InterfaceFiles.NameHashInfo))
nameHashOneCache = unsafePerformIO (newMVar M.empty)

-- | Resolve the on-disk .tydb path for a module, using a process-wide cache.
-- Avoids repeated filesystem walks when many modules share dependencies.
getTyFileCached :: [FilePath] -> A.ModName -> IO (Maybe FilePath)
getTyFileCached spaths mn = modifyMVar tyPathCache $ \m -> do
  normSearch <- mapM normalizePathSafe spaths
  let inSearchPath p =
        let pNorm = normalise p
        in any (\dir ->
                  let dirNorm = addTrailingPathSeparator (normalise dir)
                  in Data.List.isPrefixOf dirNorm pNorm)
               normSearch
      refresh cache = do
        mty <- Acton.Env.findTyFile spaths mn
        case mty of
          Just p  -> return (M.insert mn p cache, Just p)
          Nothing -> return (M.delete mn cache, Nothing)
  case M.lookup mn m of
    Just p -> do
      pNorm <- normalizePathSafe p
      exists <- InterfaceFiles.interfaceExists pNorm
      if exists && inSearchPath pNorm
        then return (m, Just pNorm)
        else refresh m
    Nothing -> refresh m

-- | Read a module's public hash using the cache and .tydb header.
-- This drives dependency invalidation when an imported interface changes.
getPubHashCached :: Paths -> A.ModName -> IO (Maybe B.ByteString)
getPubHashCached paths mn = modifyMVar pubHashCache $ \m -> do
  case M.lookup mn m of
    Just ih -> return (m, Just ih)
    Nothing -> do
      mty <- getTyFileCached (searchPath paths) mn
      case mty of
        Just ty -> do
          hashes <- InterfaceFiles.readModuleHashesMaybe ty
          case hashes of
            Just (_srcH, ih, _implH) -> return (M.insert mn ih m, Just ih)
            Nothing -> return (m, Nothing)
        Nothing -> return (m, Nothing)

-- | Update the public-hash cache after a successful compile.
-- Keeps in-process dependency checks consistent for later modules.
updatePubHashCache :: A.ModName -> B.ByteString -> IO ()
updatePubHashCache mn ih = modifyMVar_ pubHashCache $ \m -> return (M.insert mn ih m)

-- | Read a module's implementation hash using the cache and .tydb header.
getImplHashCached :: Paths -> A.ModName -> IO (Maybe B.ByteString)
getImplHashCached paths mn = modifyMVar implHashCache $ \m -> do
  case M.lookup mn m of
    Just ih -> return (m, Just ih)
    Nothing -> do
      mty <- getTyFileCached (searchPath paths) mn
      case mty of
        Just ty -> do
          hashes <- InterfaceFiles.readModuleHashesMaybe ty
          case hashes of
            Just (_srcH, _pubH, ih) -> return (M.insert mn ih m, Just ih)
            Nothing -> return (m, Nothing)
        Nothing -> return (m, Nothing)

-- | Update the implementation-hash cache after a successful compile.
updateImplHashCache :: A.ModName -> B.ByteString -> IO ()
updateImplHashCache mn ih = modifyMVar_ implHashCache $ \m -> return (M.insert mn ih m)

-- | Build a name hash map keyed by Name.
nameHashMapFromList :: [InterfaceFiles.NameHashInfo] -> M.Map A.Name InterfaceFiles.NameHashInfo
nameHashMapFromList infos = M.fromList [ (InterfaceFiles.nhName i, i) | i <- infos ]

publicIfaceTE :: [(A.Name, I.NameInfo)] -> [(A.Name, I.NameInfo)]
publicIfaceTE = filter (Names.isPublicName . fst)

publicNameHashes :: [InterfaceFiles.NameHashInfo] -> [InterfaceFiles.NameHashInfo]
publicNameHashes = filter (Names.isPublicName . InterfaceFiles.nhName)

-- | Read a module's per-name hash map using the cache and .tydb header.
getNameHashMapCached :: Paths -> A.ModName -> IO (Maybe (M.Map A.Name InterfaceFiles.NameHashInfo))
getNameHashMapCached paths mn = modifyMVar nameHashCache $ \m -> do
  case M.lookup mn m of
    Just hm -> return (m, Just hm)
    Nothing -> do
      mty <- getTyFileCached (searchPath paths) mn
      case mty of
        Just ty -> do
          hdr <- InterfaceFiles.readHeaderMaybe ty
          case hdr of
            Just (_sourceMetaH, _srcH, _ih, _implH, _impsH, _depModulesH, nameHashes, _rootsH, _testsH, _docH) -> do
              let hm = nameHashMapFromList (publicNameHashes nameHashes)
              return (M.insert mn hm m, Just hm)
            Nothing -> return (m, Nothing)
        Nothing -> return (m, Nothing)

-- | Read one cached name hash without decoding all per-name hash rows.
getNameHashCached :: Paths -> A.ModName -> A.Name -> IO (Maybe InterfaceFiles.NameHashInfo)
getNameHashCached paths mn n = do
  m <- readMVar nameHashCache
  case M.lookup mn m of
    Just hm -> return (M.lookup n hm)
    Nothing -> modifyMVar nameHashOneCache $ \c ->
      case M.lookup (mn, n) c of
        Just r -> return (c, r)
        Nothing -> do
          mty <- getTyFileCached (searchPath paths) mn
          r <- case mty of
                 Just ty -> InterfaceFiles.readNameHashMaybe ty n
                 Nothing -> return Nothing
          return (M.insert (mn, n) r c, r)

-- | Update the name-hash cache after a successful compile. Cached modules
-- carry no name hashes; their entries must not shadow targeted reads.
updateNameHashCache :: A.ModName -> [InterfaceFiles.NameHashInfo] -> IO ()
updateNameHashCache mn infos =
  when (not (null infos)) $
    modifyMVar_ nameHashCache $ \m -> return (M.insert mn (nameHashMapFromList infos) m)


-- Handling Acton files -----------------------------------------------------------------------------

-- | Keep only .act files when scanning directories.
-- Returns Just the file path for Acton sources and Nothing otherwise.
filterActFile :: FilePath -> Maybe FilePath
filterActFile file =
    case fileExt of
        ".act" -> Just file
        _ -> Nothing
  where (fileBody, fileExt) = splitExtension $ takeFileName file

-- | Turn a list of (location, message) pairs into diagnostics.
-- Used to normalize errors from different compiler subsystems.
errsToDiagnostics :: String -> FilePath -> String -> [(SrcLoc, String)] -> [Diagnostic String]
errsToDiagnostics errKind filename src errs =
    [ Diag.actErrToDiagnostic errKind filename src loc msg | (loc, msg) <- errs ]

-- | Emit diagnostics when a dependency .tydb file is missing or unreadable.
-- Anchors the error to the owning module's filename for consistent reporting.
missingIfaceDiagnostics :: A.ModName -> String -> A.ModName -> [Diagnostic String]
missingIfaceDiagnostics ownerMn src missingMn =
    errsToDiagnostics "Compilation error" (modNameToFilename ownerMn) src
      [(NoLoc, "Type interface file not found or unreadable for " ++ modNameToString missingMn)]

-- | A synchronous .tydb write failure is this module's front failure: the
-- module cannot report complete without committed rows, and shaping it as a
-- diagnostic keeps it on the structured error path (callers expect Left, not
-- an escaped exception -- the task loop's waitAny would rethrow one).
tydbWriteDiagnostics :: A.ModName -> SomeException -> [Diagnostic String]
tydbWriteDiagnostics mn err =
    errsToDiagnostics "Compilation error" (modNameToFilename mn) ""
      [(NoLoc, "Failed to write .tydb for " ++ modNameToString mn ++ ": " ++ displayException err)]

-- | Parse a module from source text, returning diagnostics on failure.
-- Wraps parser, context, and indentation errors into a uniform format.
parseActSource :: C.CompileOptions -> A.ModName -> FilePath -> String -> Maybe (ParseProgress -> IO ()) -> IO (Either [Diagnostic String] A.Module)
parseActSource opts mn actFile srcContent mOnProgress = do
  let parseModule
        | C.parse_serial opts = Acton.Parser.parseModuleSerial
        | otherwise             = Acton.Parser.parseModule
  (Right <$> parseModule mn actFile srcContent (fmap wrapProgress mOnProgress))
    `catch` handleParseBundle
    `catch` handleCustomParse
    `catch` handleChunkScanError
    `catch` handleContextError
    `catch` handleIndentationError
  where
    wrapProgress onProgress completed total =
      onProgress (ParseProgress completed total)

    handleParseBundle :: ParseErrorBundle String CustomParseError -> IO (Either [Diagnostic String] A.Module)
    handleParseBundle bundle =
      return $ Left [Diag.parseDiagnosticFromBundle actFile srcContent bundle]

    handleCustomParse :: CustomParseException -> IO (Either [Diagnostic String] A.Module)
    handleCustomParse err =
      return $ Left [Diag.customParseExceptionToDiagnostic actFile srcContent err]

    handleChunkScanError :: ChunkScanError -> IO (Either [Diagnostic String] A.Module)
    handleChunkScanError (ChunkScanError loc msg) =
      return $ Left (errsToDiagnostics "Parse error" actFile srcContent [(loc, msg)])

    handleContextError :: ContextError -> IO (Either [Diagnostic String] A.Module)
    handleContextError err =
      return $ Left (errsToDiagnostics "Context error" (modNameToFilename mn) srcContent (Acton.Parser.contextError err))

    handleIndentationError :: IndentationError -> IO (Either [Diagnostic String] A.Module)
    handleIndentationError err =
      return $ Left (errsToDiagnostics "Indentation error" (modNameToFilename mn) srcContent (Acton.Parser.indentationError err))

-- | Parse only the module header from a SourceSnapshot.
parseActHeaderSnapshot :: A.ModName
                       -> FilePath
                       -> Source.SourceSnapshot
                       -> IO (Either [Diagnostic String] ([A.ModName], Maybe String))
parseActHeaderSnapshot mn actFile snap = do
  cwd <- getCurrentDirectory
  let displayFile = makeRelative cwd actFile
      srcContent = Source.ssText snap
  (Right <$> do
      (imps, mdoc) <- Acton.Parser.parseModuleHeader displayFile srcContent
      return (A.importsOf (A.Module mn imps Nothing []), mdoc))
    `catch` handleParseBundle displayFile srcContent
    `catch` handleCustomParse displayFile srcContent
    `catch` handleContextError srcContent
    `catch` handleIndentationError srcContent
  where
    handleParseBundle file srcContent bundle =
      return $ Left [Diag.parseDiagnosticFromBundle file srcContent bundle]

    handleCustomParse file srcContent err =
      return $ Left [Diag.customParseExceptionToDiagnostic file srcContent err]

    handleContextError srcContent err =
      return $ Left (errsToDiagnostics "Context error" (modNameToFilename mn) srcContent (Acton.Parser.contextError err))

    handleIndentationError srcContent err =
      return $ Left (errsToDiagnostics "Indentation error" (modNameToFilename mn) srcContent (Acton.Parser.indentationError err))

-- | Parse a SourceSnapshot with a display path relative to cwd.
-- Keeps diagnostics stable regardless of absolute paths or overlays.
parseActSnapshot :: C.CompileOptions -> A.ModName -> FilePath -> Source.SourceSnapshot -> IO (Either [Diagnostic String] A.Module)
parseActSnapshot opts mn actFile snap = do
  cwd <- getCurrentDirectory
  let displayFile = makeRelative cwd actFile
  parseActSource opts mn displayFile (Source.ssText snap) Nothing

-- | Read and parse a source file via SourceProvider (overlay-aware).
-- Returns both the snapshot and the parsed AST for reuse by callers.
parseActFile :: C.CompileOptions
             -> Source.SourceProvider
             -> A.ModName
             -> FilePath
             -> Maybe (ParseProgress -> IO ())
             -> IO (Either [Diagnostic String] (Source.SourceSnapshot, A.Module))
parseActFile opts sp mn actFile mOnProgress = do
  snap <- Source.readSource sp actFile
  cwd <- getCurrentDirectory
  let displayFile = makeRelative cwd actFile
  emod <- parseActSource opts mn displayFile (Source.ssText snap) mOnProgress
  return $ fmap (\m -> (snap, m)) emod

-- | Read stable source metadata used for quick .tydb reuse checks.
-- Device/inode act as file identity on POSIX so metadata drift from copies or
-- restores falls back to content hashing instead of blindly trusting mtimes.
readSourceFileMeta :: FilePath -> IO InterfaceFiles.SourceFileMeta
readSourceFileMeta path = do
  st <- getFileStatus path
  let mtimeNs = fileStatusMTimeNs st
      ctimeNs = fileStatusCTimeNs st
  return InterfaceFiles.SourceFileMeta
    { InterfaceFiles.sfmMTimeNs = mtimeNs
    , InterfaceFiles.sfmCTimeNs = ctimeNs
    , InterfaceFiles.sfmSize = fromIntegral (fileSize st)
    , InterfaceFiles.sfmDevice = Just (fromIntegral (deviceID st))
    , InterfaceFiles.sfmInode = Just (fromIntegral (fileID st))
    }
  where
    fileStatusMTimeNs st = floor (toRational (modificationTimeHiRes st) * 1000000000)
    fileStatusCTimeNs st = floor (toRational (statusChangeTimeHiRes st) * 1000000000)

-- | Read the source paired with a cached interface, rejecting a concurrent
-- edit instead of generating output from source text that does not match the
-- interface and codegen hash selected for this compile generation.
readMatchingSource :: Source.SourceProvider
                   -> A.ModName
                   -> B.ByteString
                   -> FilePath
                   -> IO Source.SourceSnapshot
readMatchingSource sp mn expected path = do
  snap <- Source.readSource sp path
  unless (SHA256.hash (Source.ssBytes snap) == expected) $
    throwIO $ ProjectError $
      "Source changed while preparing back passes for " ++ modNameToString mn
  return snap


-- Compilation tasks, chasing imported modules, compilation and building executables -----------------

data BackInput = BackInput
  { biTypeEnv   :: Acton.Env.Env0
  , biTypedMod  :: A.Module
  , biDeclarations :: [A.Name]
  , biSrc       :: Maybe String
  , biCodegenHash :: B.ByteString
  }

data BackJob = BackJob
  { bjPaths :: Paths
  , bjOpts  :: C.CompileOptions
  , bjInput :: BackInput
  -- Deferred batches share a completion barrier which revalidates their
  -- captured interfaces after the last generated output is complete.
  , bjValidate :: IO ()
  }

-- Deferred jobs deliberately keep only reloadable metadata so large typed
-- modules can be released after front passes finish.
data DeferredBackJob = DeferredBackJob
  { dbjPaths :: Paths
  , dbjOpts :: C.CompileOptions
  , dbjMod :: A.ModName
  }

data FrontOutputJob = FrontOutputJob
  { fojKey   :: TaskKey
  , fojKind  :: FrontOutputKind
  , fojAsync :: Async ()
  }

data FrontResult = FrontResult
  { frIfaceTE  :: [(A.Name, I.NameInfo)]
  , frImps     :: [A.ModName]
  , frDoc      :: Maybe String
  , frModuleInfo :: Maybe Acton.Env.ModuleInfo
  , frPubHash :: B.ByteString
  , frImplHash :: B.ByteString
  , frNameHashes :: [InterfaceFiles.NameHashInfo]
  , frFrontTime :: Maybe TimeSpec
  , frFrontTiming :: Maybe FrontTiming
  , frInferredSigs :: [InferredSignature]
  , frBackJob  :: Maybe BackJob
  , frDeferredBackJob :: Maybe DeferredBackJob
  , frOutputJobs :: [FrontOutputJob]
  }

frontResultModuleInfo :: A.ModName -> FrontResult -> Acton.Env.ModuleInfo
frontResultModuleInfo mn fr =
    fromMaybe (Acton.Env.mkModuleInfo mn (frImps fr) (frIfaceTE fr) (frDoc fr)) (frModuleInfo fr)

docNameCountThreshold :: Int
docNameCountThreshold = 10000

shouldGenerateDocOutput :: C.CompileOptions -> Bool -> Int -> Bool
shouldGenerateDocOutput opts tmp nameCount =
  not (C.skip_build opts) && not tmp && nameCount <= docNameCountThreshold

shouldRunBackPass :: Bool -> Bool -> C.CompileOptions -> [A.Name] -> Bool
shouldRunBackPass dbpBlocked requested opts roots =
  dbpBlocked || altOutput opts || not requested || not (null roots)

selectiveDeferredBackJob :: Bool
                         -> Bool
                         -> C.CompileOptions
                         -> Paths
                         -> A.ModName
                         -> Maybe DeferredBackJob
selectiveDeferredBackJob blocked hasNotImpl opts paths mn
  | C.no_dbp opts = Nothing
  -- Persistence restores classes by stored id, outside static reachability.
  | C.db opts = Nothing
  | C.only_build opts = Nothing
  | altOutput opts = Nothing
  | mn == A.modName ["__builtin__"] = Nothing
  | blocked = Nothing
  -- A module with NotImplemented bodies is implemented by a hand-written
  -- .ext.c and must be rendered whole, like __builtin__ -- its statement
  -- rows deliberately opt out of selective reads.
  | hasNotImpl = Nothing
  | otherwise =
      Just DeferredBackJob
        { dbjPaths = paths
        , dbjOpts = opts
        , dbjMod = mn
        }

startFrontOutputJob :: (TaskKey -> FrontOutputKind -> IO ())
                    -> (TaskKey -> FrontOutputKind -> Maybe TimeSpec -> IO ())
                    -> IO Bool
                    -> TaskKey
                    -> FrontOutputKind
                    -> IO ()
                    -> IO FrontOutputJob
startFrontOutputJob onStart onDone shouldWrite key kind action = do
  current <- shouldWrite
  a <-
    if current
      then do
        onStart key kind
        asyncWithUnmask $ \unmask ->
          runFrontOutputAction onDone shouldWrite key kind (unmask action)
      else async (return ())
  return FrontOutputJob
    { fojKey = key
    , fojKind = kind
    , fojAsync = a
    }

-- Run a front output SYNCHRONOUSLY: used for the .tydb commit, so that a
-- module reporting its front as complete IMPLIES its rows are on disk --
-- readers never wait for, or race, a write. Same start/done reporting and
-- skip-write gating as the background jobs; a failure propagates like any
-- other front error (and no longer only when someone waits on a job).
runFrontOutputSync :: (TaskKey -> FrontOutputKind -> IO ())
                   -> (TaskKey -> FrontOutputKind -> Maybe TimeSpec -> IO ())
                   -> IO Bool
                   -> TaskKey
                   -> FrontOutputKind
                   -> IO ()
                   -> IO ()
runFrontOutputSync onStart onDone shouldWrite key kind action = do
  current <- shouldWrite
  when current $ do
    onStart key kind
    runStartedFrontOutputAction onDone key kind action

runFrontOutputAction :: (TaskKey -> FrontOutputKind -> Maybe TimeSpec -> IO ())
                     -> IO Bool
                     -> TaskKey
                     -> FrontOutputKind
                     -> IO ()
                     -> IO ()
runFrontOutputAction onDone shouldWrite key kind action = do
  current <- shouldWrite
  when current $
    runStartedFrontOutputAction onDone key kind action

runStartedFrontOutputAction :: (TaskKey -> FrontOutputKind -> Maybe TimeSpec -> IO ())
                            -> TaskKey
                            -> FrontOutputKind
                            -> IO ()
                            -> IO ()
runStartedFrontOutputAction onDone key kind action = do
  t0 <- getTime Monotonic
  res <- (try action :: IO (Either SomeException ()))
  t1 <- getTime Monotonic
  let elapsed = t1 - t0
  onDone key kind (either (const Nothing) (const (Just elapsed)) res) `catch` ignoreProgressException
  either throwIO return res
  where
    ignoreProgressException :: SomeException -> IO ()
    ignoreProgressException err
      | isJust (fromException err :: Maybe SomeAsyncException) = throwIO err
      | otherwise = return ()

frontOutputKindName :: FrontOutputKind -> String
frontOutputKindName FrontOutputTydb     = "tydb"
frontOutputKindName FrontOutputTydbCopy = "tydb-copy"
frontOutputKindName FrontOutputDoc      = "doc"

frontOutputTyDbProgress :: InterfaceFiles.TyDbWriteProgress -> FrontOutputProgress
frontOutputTyDbProgress p =
  FrontOutputProgress
    { fopLabel = InterfaceFiles.tyDbWriteProgressLabel p
    , fopRatio = InterfaceFiles.tyDbWriteProgressRatio p
    }

copyTydbInterface :: C.CompileOptions -> Paths -> A.ModName -> IO ()
copyTydbInterface opts paths mn =
  when (C.tydb opts) $
    InterfaceFiles.copyInterface tySrc tyDst
  where
    tySrc = tyDbPath paths mn
    tyDst = srcBase paths mn ++ InterfaceFiles.interfaceExt

waitFrontOutputJobs :: FrontResult -> IO ()
waitFrontOutputJobs fr =
  waitFrontOutputJobList (frOutputJobs fr)

waitFrontOutputJobList :: [FrontOutputJob] -> IO ()
waitFrontOutputJobList jobs = do
  failure <- waitFrontOutputJobFailures jobs
  maybe (return ()) throwFrontOutputFailure failure

throwFrontOutputFailure :: CompileFailure -> IO a
throwFrontOutputFailure failure =
  throwIO (FrontOutputError (compileFailureMessage failure))

frontOutputException :: FrontOutputJob -> IO (Maybe SomeException)
frontOutputException job = do
  res <- waitCatch (fojAsync job)
  case res of
    Right () -> return Nothing
    Left err -> return (Just err)

frontOutputFailureMessage :: FrontOutputJob -> SomeException -> String
frontOutputFailureMessage job err =
  "Front output job failed for "
  ++ tkProj key ++ "/" ++ modNameToString (tkMod key)
  ++ " (" ++ frontOutputKindName (fojKind job) ++ "): " ++ displayException err
  where
    key = fojKey job

frontOutputFailure :: FrontOutputJob -> IO (Maybe CompileFailure)
frontOutputFailure job = do
  mErr <- frontOutputException job
  case mErr of
    Nothing -> return Nothing
    Just err
      | isJust (fromException err :: Maybe SomeAsyncException) -> throwIO err
      | otherwise -> return (Just (CompileInternalFailure (frontOutputFailureMessage job err)))

rememberFrontOutputJobList :: IORef [FrontOutputJob] -> [FrontOutputJob] -> IO ()
rememberFrontOutputJobList ref jobs =
  atomicModifyIORef' ref $ \old -> (jobs ++ old, ())

waitFrontOutputJobsRef :: IORef [FrontOutputJob] -> IO (Maybe CompileFailure)
waitFrontOutputJobsRef ref = do
  jobs <- reverse <$> readIORef ref
  waitFrontOutputJobFailures jobs

waitFrontOutputJobFailures :: [FrontOutputJob] -> IO (Maybe CompileFailure)
waitFrontOutputJobFailures jobs = do
  failures <- mapM frontOutputFailure jobs
  return (listToMaybe (catMaybes failures))

data CompileTask        = ParseTask { name :: A.ModName, src :: String, srcBytes :: B.ByteString, sourceMeta :: Maybe InterfaceFiles.SourceFileMeta, parseImports :: [A.ModName] }
                        | ActonTask { name :: A.ModName, src :: String, srcBytes :: B.ByteString, sourceMeta :: Maybe InterfaceFiles.SourceFileMeta, atree:: A.Module }
                        | TyTask    { name :: A.ModName
                                    , tyHash :: B.ByteString               -- raw source bytes hash
                                    , tyPubHash :: B.ByteString          -- module public hash
                                    , tyImplHash :: B.ByteString         -- module impl hash
                                    , tyImports :: [(A.ModName, B.ByteString)] -- imports with pub hash used
                                    , tyDepModules :: [InterfaceFiles.DepModuleInfo]
                                    , tyNameHashes :: [InterfaceFiles.NameHashInfo]
                                    , tyNameCount :: Int
                                    , tyRoots :: [A.Name]
                                    , tyTests :: [String]
                                    , tyDoc :: Maybe String
                                    , iface :: I.NModule
                                    , typed :: A.Module
                                    }
                        | ParseErrorTask { name :: A.ModName, parseDiagnostics :: [Diagnostic String] }

instance Show CompileTask where
  show ParseTask{ name = mn } = "ParseTask " ++ modNameToString mn
  show ActonTask{ name = mn } = "ActonTask " ++ modNameToString mn
  show TyTask{ name = mn } = "TyTask " ++ modNameToString mn
  show ParseErrorTask{ name = mn, parseDiagnostics = ds } =
    "ParseErrorTask " ++ modNameToString mn ++ " (" ++ show (length ds) ++ " diagnostics)"

-- TODO: replace binName String type with ModName just like for CompileTask.
-- ModName is a array so a hierarchy with submodules is represented, we can then
-- get it use joinPath (modPath) to get a path or modName to get a string
-- representation. We need both of BinTask when generating build.zig, so it
-- would be more robust to use that type rather than a hacky character
-- replacement (replaceDot in genBuildZigExe)

data TaskKey = TaskKey { tkProj :: FilePath, tkMod :: A.ModName } deriving (Eq, Ord, Show)

data StageKey = ParseStage TaskKey | FrontStage TaskKey deriving (Eq, Ord, Show)

data StageSuccess = StageParsed CompileTask (Maybe TimeSpec) | StageFronted FrontResult

forceHTEnv :: I.HTEnv -> ()
forceHTEnv hte                  = HM.foldl' forceNameInfo () hte
  where forceNameInfo () ni    = ni `seq` ()

forceTypeResult :: I.NModule -> A.Module -> Acton.Env.EnvF x -> [String] -> IO ()
forceTypeResult nmod tchecked typeEnv tests = do
  evaluate (rnf nmod)
  evaluate (rnf tchecked)
  evaluate (forceHTEnv (Acton.Env.hnames typeEnv))
  evaluate (rnf tests)

data GlobalTask = GlobalTask
  { gtKey             :: TaskKey
  , gtPaths           :: Paths
  , gtTask            :: CompileTask
  , gtImportProviders :: M.Map A.ModName TaskKey   -- resolved in-graph providers (if any) for imports
  }

-- | Extract imported module names from a CompileTask.
-- TyTask uses header imports, ActonTask uses the parsed AST, and parse errors
-- yield no imports.
importsOf :: CompileTask -> [A.ModName]
importsOf (ParseTask _ _ _ _ imps) = imps
importsOf (ActonTask _ _ _ _ m) = A.importsOf m
importsOf (TyTask { tyImports = ms }) = map fst ms
importsOf (ParseErrorTask _ _) = []

restoredImportsOf paths TyTask{ tyImports = ms } = map (dropProjPrefix paths . fst) ms
restoredImportsOf paths task = importsOf task

checkImportPrefixes key proj modSets imps
  | proj `elem` special_projects
                        = return ()
  | not $ null prefixed = throwProjectError ("Do not use prefix '" ++ proj ++ "' when referring to modules in that project (" ++ prstrs prefixed ++ ")")
  | otherwise           = return ()
  where p0              = tkProj key
        Just mods0      = M.lookup p0 modSets
        prefixed        = [ mn | mn <- imps, Data.Set.member mn mods0 ]

-- | Resolve imports to in-graph providers using project search order.
-- This chooses the first project in the search order that declares the module,
-- producing TaskKeys for dependency edges.
resolveProviders :: TaskKey -> String -> [FilePath] -> M.Map FilePath (Data.Set.Set A.ModName) -> Data.Set.Set ProjName -> [A.ModName] -> M.Map A.ModName TaskKey
resolveProviders key proj order modSets deps imps =
    --trace ("#### resolveProviders " ++ prstr key) $
    --trace ("   # proj: " ++ proj) $
--    trace ("   # order: " ++ prstrs order) $
--    trace ("   # modset: " ++ prstrs (case M.lookup (tkProj key) modSets of Nothing -> []; Just s -> Data.Set.toList s)) $
    --trace ("   # imps: " ++ prstrs imps) $
    --trace ("   # imps': " ++ prstrs imps') $
    --trace ("  ## map: "  ++ prstr result) $
    result
  where result          = M.fromList $ concat $ map findProvider imps'
        imps'           = [ A.modName ns | mn <- imps, let n:ns = A.modPath mn, n == proj ] ++ map dropLib imps
        p0              = tkProj key
        Just mods0      = M.lookup p0 modSets
        findProvider mn
          | Data.Set.member mn0 mods0
                        = [ (mn, TaskKey p0 mn0) ]
          | otherwise   = [ (mn, TaskKey p mn') | p <- order, mn' <- [mn,addLib mn], maybe False (Data.Set.member mn') (M.lookup p modSets) ]
          where mn0     = if proj `elem` special_projects then mn else A.modName (proj : A.modPath mn)
        addLib mn       = case A.modPath mn of
                            [n] | Data.Set.member n deps -> A.modName [n, "lib"]
                            _ -> mn
        dropLib mn      = case A.modPath mn of
                            [n,"lib"] | Data.Set.member n deps -> A.modName [n]
                            _ -> mn

special_projects = ["", "base", "acton_scratch"]

type ProjDir = FilePath
type ActFile = FilePath
type ProjName = String

-- | Build GlobalTasks for all discovered projects.
-- Crawls project sources, resolves provider edges, and optionally limits the
-- seed set to specific files to keep the DAG small for incremental builds.
buildGlobalTasks :: Source.SourceProvider
                 -> C.GlobalOptions
                 -> C.CompileOptions
                 -> M.Map FilePath ProjCtx
                 -> Maybe [String]                  -- optional seed source files; Nothing = all modules
                 -> IO ([GlobalTask], M.Map FilePath (Data.Set.Set A.ModName))
buildGlobalTasks sp gopts opts projMap mSeeds = do
    -- perProj :: [(ProjCtx, (ActFile,A.ModName))]
    perProj <- forM (M.elems projMap) $ \ctx -> do
                  mods <- enumerateProjectModules ctx
                  return (ctx, mods)
    let -- Declared modules mapped to their source files (per project)
        modMaps :: M.Map ProjDir (M.Map A.ModName ActFile)
        modMaps = M.fromList [ (projRoot ctx, M.fromList [ (mn, actFile) | (actFile, mn) <- mods ]) | (ctx, mods) <- perProj ]
        -- Declared modules (per project)
        modSets :: M.Map ProjDir (Data.Set.Set A.ModName)
        modSets = M.map (Data.Set.fromList . M.keys) modMaps
        -- Project dependencies, in their BuildSpec order (per project)
        allDeps :: M.Map ProjDir (Data.Set.Set ProjName)
        allDeps = M.fromList [ (projRoot ctx, Data.Set.fromList (map fst $ projDeps ctx)) | (ctx,_) <- perProj ]
        orderCache :: M.Map ProjDir [ProjDir]
        orderCache = M.fromList [ (projRoot ctx, projDepClosure projMap (projRoot ctx)) | (ctx, _) <- perProj ]
        -- Declared modules in all reachable projects (paired with their project paths)
        allKeys = [ TaskKey (projRoot ctx) mn | (ctx, mods) <- perProj, (_, mn) <- mods ]
    seedKeys <- case mSeeds of
                  Nothing -> return allKeys
                  Just files -> do
                    absFiles <- mapM canonicalizePath files
                    let pathIndex = M.fromList [ (actFile, TaskKey (projRoot ctx) mn) | (ctx, mods) <- perProj, (actFile, mn) <- mods ]
                        found = mapMaybe (`M.lookup` pathIndex) absFiles
                    return (if null found then allKeys else found)
    tasks <- go modMaps modSets allDeps orderCache Data.Set.empty seedKeys []
    return (reverse tasks, modSets)
  where
    go modMaps modSets allDeps orderCache seen [] acc = return acc
    go modMaps modSets allDeps orderCache seen (k:qs) acc
      | Data.Set.member k seen = go modMaps modSets allDeps orderCache seen qs acc
      | otherwise =
          case M.lookup (tkProj k) modMaps >>= M.lookup (tkMod k) of
            Nothing -> go modMaps modSets allDeps orderCache (Data.Set.insert k seen) qs acc
            Just actFile -> do
              let ctx = projMap M.! tkProj k
              paths <- pathsForModule opts projMap ctx (tkMod k)
              task  <- readModuleTask sp gopts opts paths actFile
              let order = M.findWithDefault [tkProj k] (tkProj k) orderCache
                  deps = M.findWithDefault Data.Set.empty (tkProj k) allDeps
                  imps = restoredImportsOf paths task
              checkImportPrefixes k (projName paths) modSets imps
              let providers = resolveProviders k (projName paths) order modSets deps imps
                  newKeys = M.elems providers
                  acc' = GlobalTask { gtKey = k
                                    , gtPaths = paths
                                    , gtTask = task
                                    , gtImportProviders = providers
                                    } : acc
              go modMaps modSets allDeps orderCache (Data.Set.insert k seen) (qs ++ newKeys) acc'


-- | Select the subgraph needed for a given build request.
-- Maps file paths to TaskKeys, adds __builtin__, and computes the reachable
-- dependency closure.
taskKeysForFiles :: Paths -> FilePath -> [GlobalTask] -> [FilePath] -> IO [TaskKey]
taskKeysForFiles pathsRoot rootProj globalTasks srcFiles = do
    requested <- catMaybes <$> mapM lookupTaskKey srcFiles
    let wantedNames = map takeFileName srcFiles
    return $ if null requested
      then [ gtKey task
           | task <- globalTasks
           , takeFileName (srcFile (gtPaths task) (tkMod $ gtKey task)) `elem` wantedNames
           ]
      else requested
  where
    lookupTaskKey file = do
      absolute <- canonicalizePath file
      case listToMaybe
             [ gtKey task
             | task <- globalTasks
             , srcFile (gtPaths task) (tkMod $ gtKey task) == absolute
             ] of
        Just key -> return (Just key)
        Nothing -> do
          mn <- moduleNameFromFile (srcDir pathsRoot) (projName pathsRoot) absolute
          return $ listToMaybe
            [ gtKey task
            | task <- globalTasks
            , tkProj (gtKey task) == rootProj
            , tkMod (gtKey task) == mn
            ]

selectNeededTasks :: Paths -> FilePath -> [GlobalTask] -> [FilePath] -> IO [GlobalTask]
selectNeededTasks pathsRoot rootProj globalTasks srcFiles = do
    requestedKeys <- taskKeysForFiles pathsRoot rootProj globalTasks srcFiles
    let builtinKeys = [ gtKey t | t <- globalTasks, tkMod (gtKey t) == A.modName ["__builtin__"] ]
        startKeys   = if null requestedKeys then map gtKey globalTasks else requestedKeys ++ builtinKeys
        depMapSet   = M.fromList [ (gtKey t, Data.Set.fromList (M.elems (gtImportProviders t))) | t <- globalTasks ]
        neededKeys  = reachable depMapSet (Data.Set.fromList startKeys)
    return [ t | t <- globalTasks, Data.Set.member (gtKey t) neededKeys ]
  where
    reachable depMap start = go (Data.Set.toList start) Data.Set.empty
      where
        go [] seen = seen
        go (k:ks) seen =
          if Data.Set.member k seen
            then go ks seen
            else
              let deps = Data.Set.toList (M.findWithDefault Data.Set.empty k depMap)
              in go (deps ++ ks) (Data.Set.insert k seen)

-- | Select the minimal subgraph affected by a set of changed files.
-- Computes reverse dependencies so we rebuild dependents while keeping
-- unrelated modules untouched. DBP-eligible providers on dependency paths from
-- affected modules are kept too, along with their reverse dependents, because
-- every consumer of a deferred provider can contribute selected-name interest.
selectAffectedTasks :: FilePath -> C.CompileOptions -> [GlobalTask] -> Data.Set.Set TaskKey -> [FilePath] -> IO [GlobalTask]
selectAffectedTasks rootProj opts globalTasks dbpBlocked changedFiles = do
    if null changedFiles
      then return globalTasks
      else do
        absChanged <- mapM normalizePathSafe changedFiles
        taskPaths <- forM globalTasks $ \t -> do
          let k = gtKey t
              pths = gtPaths t
          p <- normalizePathSafe (srcFile pths (tkMod k))
          return (p, k)
        let pathIndex = M.fromList taskPaths
            changedKeys = catMaybes [ M.lookup p pathIndex | p <- absChanged ]
        if null changedKeys
          then return globalTasks
          else do
            let taskKeys = Data.Set.fromList (map gtKey globalTasks)
                depMap = M.fromList
                  [ (gtKey t, filter (`Data.Set.member` taskKeys) (M.elems (gtImportProviders t)))
                  | t <- globalTasks
                  ]
                revMap = foldl' (\acc (k, ds) ->
                                  foldl' (\a d -> M.insertWith (++) d [k] a) acc ds)
                                M.empty
                                (M.toList depMap)
                affected = reverseClosure revMap (Data.Set.fromList changedKeys)
                dbpProviderPaths = dbpProviderPathClosure rootProj opts globalTasks dbpBlocked depMap revMap affected
                selected = Data.Set.union affected dbpProviderPaths
            -- Keep original provider mappings so unchanged imports can still
            -- resolve via cached interfaces during incremental checks.
            return [ t | t <- globalTasks, Data.Set.member (gtKey t) selected ]
  where
    reverseClosure revMap start = go start (Data.Set.toList start)
      where
        go seen [] = seen
        go seen (k:ks) =
          let ds = M.findWithDefault [] k revMap
              new = filter (`Data.Set.notMember` seen) ds
              seen' = foldl' (flip Data.Set.insert) seen new
          in go seen' (ks ++ new)

dbpProviderPathClosure :: FilePath
                       -> C.CompileOptions
                       -> [GlobalTask]
                       -> Data.Set.Set TaskKey
                       -> M.Map TaskKey [TaskKey]
                       -> M.Map TaskKey [TaskKey]
                       -> Data.Set.Set TaskKey
                       -> Data.Set.Set TaskKey
dbpProviderPathClosure rootProj opts globalTasks dbpBlocked depMap revMap affected =
    Data.Set.union pathKeys dependentKeys
  where
    taskMap = M.fromList [ (gtKey t, t) | t <- globalTasks ]
    depOpts = opts { C.skip_build = True, C.test = False }
    pathKeys = go Data.Set.empty (Data.Set.toList affected)
    dbpProviders = Data.Set.filter isDbpProvider pathKeys
    dependentKeys = reverseReachable Data.Set.empty (Data.Set.toList dbpProviders)

    optsFor k = if tkProj k == rootProj then opts else depOpts

    isDbpProvider k =
      case M.lookup k taskMap of
        Just t ->
          case gtTask t of
            TyTask{} ->
              isJust (selectiveDeferredBackJob (Data.Set.member k dbpBlocked) False
                        (optsFor k) (gtPaths t) (tkMod k))
            _ -> False
        Nothing -> False

    reachesDbp = reaches Data.Set.empty
      where
        reaches seen k
          | Data.Set.member k seen = False
          | isDbpProvider k = True
          | otherwise =
              let seen' = Data.Set.insert k seen
              in any (reaches seen') (M.findWithDefault [] k depMap)

    go seen [] = seen
    go seen (k:ks) =
      let deps = [ d | d <- M.findWithDefault [] k depMap, reachesDbp d ]
          new = filter (`Data.Set.notMember` seen) deps
          seen' = foldl' (flip Data.Set.insert) seen new
      in go seen' (ks ++ new)

    reverseReachable seen [] = seen
    reverseReachable seen (k:ks) =
      if Data.Set.member k seen
        then reverseReachable seen ks
        else
          let deps = M.findWithDefault [] k revMap
              seen' = Data.Set.insert k seen
          in reverseReachable seen' (ks ++ deps)

-- | A declared library's exposed modules keep their complete generated
-- surface. Internal providers remain selective: the persisted whole-module
-- reach summary of each exposed module supplies their exact interest.
libraryBoundaryTasks :: M.Map FilePath ProjCtx -> [GlobalTask] -> Data.Set.Set TaskKey
libraryBoundaryTasks projMap globalTasks =
    Data.Set.unions $ map exposedModules libraryGroups
  where
    taskKeys = Data.Set.fromList (map gtKey globalTasks)
    revMap = foldl'
      (\acc t ->
        foldl'
          (\m dep -> M.insertWith (++) dep [gtKey t] m)
          acc
          (filter (`Data.Set.member` taskKeys) (M.elems (gtImportProviders t))))
      M.empty
      globalTasks
    libraryGroups = libraryTaskGroups projMap globalTasks

    exposedModules group = Data.Set.filter exposed group
      where
        exposed key =
          null internalConsumers || not (null externalConsumers)
          where
            consumers = M.findWithDefault [] key revMap
            (internalConsumers,externalConsumers) = partition
              (`Data.Set.member` group) consumers

moduleStringToName :: String -> A.ModName
moduleStringToName = A.modName . splitMod
  where
    splitMod s =
      case break (== '.') s of
        (p, []) -> [p]
        (p, _:rest) -> p : splitMod rest

libraryTaskGroups :: M.Map FilePath ProjCtx -> [GlobalTask] -> [Data.Set.Set TaskKey]
libraryTaskGroups projMap globalTasks =
    [ Data.Set.fromList keys
    | ctx <- M.elems projMap
    , lib <- M.elems (BuildSpec.libraries (projBuildSpec ctx))
    , let keys = [ gtKey t
                 | modName <- BuildSpec.libModules lib
                 , let mn = moduleStringToName (proj ++ "." ++ modName)
                 , t <- globalTasks
                 , tkMod (gtKey t) == mn
                 , taskInProject ctx t
                 ]
          proj = BuildSpec.specName $ projBuildSpec ctx
    , not (null keys)
    ]
  where
    taskInProject ctx t =
      tkProj (gtKey t) == projRoot ctx
        || projPath (gtPaths t) == projRoot ctx
        || projTypes (gtPaths t) == projTypesDir ctx
        || srcUnderProject ctx t

    srcUnderProject ctx t =
      let srcRoot = addTrailingPathSeparator (projSrcDir ctx)
          k = gtKey t
      in srcRoot `isPrefixOf` srcFile (gtPaths t) (tkMod k)

-- | Declared build libraries are artifact units: selecting any member selects
-- every module in that library, plus their dependencies.
expandBuildLibraryTasks :: M.Map FilePath ProjCtx -> [GlobalTask] -> [GlobalTask] -> [GlobalTask]
expandBuildLibraryTasks projMap globalTasks selectedTasks =
    [ t | t <- globalTasks, Data.Set.member (gtKey t) selectedKeys ]
  where
    taskKeys = Data.Set.fromList (map gtKey globalTasks)
    depMap = M.fromList
      [ (gtKey t, Data.Set.fromList (filter (`Data.Set.member` taskKeys) (M.elems (gtImportProviders t))))
      | t <- globalTasks
      ]
    libraryGroups = libraryTaskGroups projMap globalTasks
    selectedKeys = close (Data.Set.fromList (map gtKey selectedTasks))

    close keys =
      let keys' = addLibraries (reachable depMap (addLibraries keys))
      in if keys' == keys then keys else close keys'

    addLibraries keys =
      foldl' add keys libraryGroups
      where
        add acc group
          | Data.Set.null (Data.Set.intersection acc group) = acc
          | otherwise = Data.Set.union acc group

    reachable deps startKeys = go (Data.Set.toList startKeys) Data.Set.empty
      where
        go [] seen = seen
        go (k:ks) seen =
          if Data.Set.member k seen
            then go ks seen
            else
              let ds = Data.Set.toList (M.findWithDefault Data.Set.empty k deps)
              in go (ds ++ ks) (Data.Set.insert k seen)

data ModuleHead
  = TyHead
      { mhName       :: A.ModName
      , mhSrcHash    :: B.ByteString
      , mhPubHash    :: B.ByteString
      , mhImplHash   :: B.ByteString
      , mhTyImports  :: [(A.ModName, B.ByteString)]
      , mhDepModules :: [InterfaceFiles.DepModuleInfo]
      , mhNameHashes :: [InterfaceFiles.NameHashInfo]
      , mhNameCount  :: Int
      , mhRoots      :: [A.Name]
      , mhTests      :: [String]
      , mhDoc        :: Maybe String
      }
  | SrcHead
      { mhName       :: A.ModName
      , mhSrc        :: String
      , mhBytes      :: B.ByteString
      , mhSourceMeta :: Maybe InterfaceFiles.SourceFileMeta
      , mhSrcImports :: [A.ModName]
      , mhDoc        :: Maybe String
      }
  | HeadError
      { mhName        :: A.ModName
      , mhDiagnostics :: [Diagnostic String]
      }

-- | Read the cheap module header used by discovery and doc indexing.
-- Reuse a valid .tydb header when possible; otherwise read the source snapshot
-- and parse only the module docstring plus imports.
readModuleHeader :: Source.SourceProvider
                 -> C.GlobalOptions
                 -> C.CompileOptions
                 -> Paths
                 -> String
                 -> IO ModuleHead
readModuleHeader sp gopts opts paths actFile = do
    let mn      = modName paths
        tyFile  = tyDbPath paths mn
    tyExists <- InterfaceFiles.interfaceExists tyFile
    if not tyExists
      then readSourceHead mn
      else do
        -- .tydb exists: read the cached header and validate it against compiler
        -- compatibility plus source metadata/content as needed.
        hdrE <- (try :: IO a -> IO (Either SomeException a)) $ InterfaceFiles.readHeaderSummary tyFile
        case hdrE of
          Left _ -> readSourceHead mn
          Right (cachedSourceMeta, moduleSrcBytesHash, modulePubHash, moduleImplHash, imps, depModules, nameCount, roots, tests, mdoc) -> do
            tyMTimeNs <- InterfaceFiles.interfaceModifiedTimeNs tyFile
            newCompiler <- compilerNewerThan tyMTimeNs
            mOverlay <- Source.spReadOverlay sp actFile
            case mOverlay of
              Just snap ->
                if newCompiler
                  then sourceHeadFromSnapshot mn snap
                  else verifyOrParse mn snap moduleSrcBytesHash modulePubHash moduleImplHash cachedSourceMeta Nothing imps depModules nameCount roots tests mdoc
              Nothing -> do
                if newCompiler
                  then readSourceHead mn
                  else do
                    currentSourceMeta <- readSourceFileMeta actFile
                    if canReuseHeader cachedSourceMeta currentSourceMeta tyMTimeNs
                      then return (mkTyHead mn moduleSrcBytesHash modulePubHash moduleImplHash imps depModules nameCount roots tests mdoc)
                      else do
                        snap <- Source.spReadFile sp actFile
                        verifyOrParse mn snap moduleSrcBytesHash modulePubHash moduleImplHash cachedSourceMeta (Just currentSourceMeta) imps depModules nameCount roots tests mdoc
  where
    fileStatusMTimeNs st = floor (toRational (modificationTimeHiRes st) * 1000000000)

    mkTyHead mn moduleSrcBytesHash modulePubHash moduleImplHash imps depModules nameCount roots tests mdoc =
      TyHead
        { mhName       = mn
        , mhSrcHash    = moduleSrcBytesHash
        , mhPubHash    = modulePubHash
        , mhImplHash   = moduleImplHash
        , mhTyImports  = imps
        , mhDepModules = depModules
        , mhNameHashes = []
        , mhNameCount  = nameCount
        , mhRoots      = roots
        , mhTests      = tests
        , mhDoc        = mdoc
        }

    metadataMatches cached current =
      case cached of
        Nothing -> False
        Just meta -> meta == current

    canReuseHeader cached current tyMTimeNs =
      metadataMatches cached current
      && InterfaceFiles.sfmMTimeNs current < tyMTimeNs

    readSourceHead mn = do
      snap <- Source.readSource sp actFile
      sourceHeadFromSnapshot mn snap

    sourceHeadFromSnapshot mn snap = do
      headerRes <- parseActHeaderSnapshot mn actFile snap
      case headerRes of
        Left diags -> return $ HeadError mn diags
        Right (imps, mdoc) -> do
          mSourceMeta <- snapshotSourceMeta snap
          return $ SrcHead mn (Source.ssText snap) (Source.ssBytes snap) mSourceMeta imps mdoc

    snapshotSourceMeta snap
      | Source.ssIsOverlay snap = return Nothing
      | otherwise = Just <$> readSourceFileMeta actFile

    compilerNewerThan tyMTimeNs
      | C.ignore_compiler_version opts = return False
      | otherwise = do
          exePathE <- (try getExecutablePath :: IO (Either SomeException FilePath))
          case exePathE of
            Left _ -> return False
            Right exePath -> do
              exeStatusE <- (try (getFileStatus exePath) :: IO (Either SomeException FileStatus))
              case exeStatusE of
                Left _ -> return False
                Right exeStatus -> return (fileStatusMTimeNs exeStatus > tyMTimeNs)

    -- Surgical single-row update: a full readFile+writeFile here would re-derive
    -- the dependency index rows from the stripped per-name hashes and silently
    -- lose their users and hashes (degrading later per-name change detection),
    -- besides rewriting a possibly multi-GB file for a pure mtime drift.
    refreshCachedSourceMeta currentSourceMeta = do
      let tyFilePath = tyDbPath paths (modName paths)
      res <- (try :: IO a -> IO (Either SomeException a)) $
        InterfaceFiles.updateSourceMeta tyFilePath currentSourceMeta
      case res of
        Left _  -> return ()
        Right _ -> return ()

    verifyOrParse mn snap moduleSrcBytesHash modulePubHash moduleImplHash cachedSourceMeta currentSourceMeta imps depModules nameCount roots tests mdoc = do
      let curHash = SHA256.hash (Source.ssBytes snap)
          short8 bs = take 8 (B.unpack $ Base16.encode bs)
          same = curHash == moduleSrcBytesHash
      when (C.verbose gopts && Source.ssIsOverlay snap) $ do
        actTime <- Source.spGetModTime sp actFile
        tyTime <- InterfaceFiles.interfaceModifiedTime (tyDbPath paths mn)
        let len = B.length (Source.ssBytes snap)
        putStrLn ("[debug] readModuleHeader " ++ modNameToString mn
                  ++ " act=" ++ show actTime
                  ++ " ty=" ++ show tyTime
                  ++ " hash=" ++ short8 curHash
                  ++ " header=" ++ short8 moduleSrcBytesHash
                  ++ " len=" ++ show len
                  ++ if same then " (match)" else " (diff)")
      if same
        then do
          when (currentSourceMeta /= Nothing && currentSourceMeta /= cachedSourceMeta) $
            refreshCachedSourceMeta currentSourceMeta
          return (mkTyHead mn moduleSrcBytesHash modulePubHash moduleImplHash imps depModules nameCount roots tests mdoc)
        else sourceHeadFromSnapshot mn snap

-- | Prepare a task for dependency graph construction.
-- The full source parse is deferred for modules whose .tydb header cannot be
-- reused, but discovery still gets accurate imports from the source header.
readModuleTask :: Source.SourceProvider -> C.GlobalOptions -> C.CompileOptions -> Paths -> String -> IO CompileTask
readModuleTask sp gopts opts paths actFile = do
  h <- readModuleHeader sp gopts opts paths actFile
  return $ case h of
    TyHead{ mhName = mn
          , mhSrcHash = srcHash
          , mhPubHash = pubHash
          , mhImplHash = implHash
          , mhTyImports = imps
          , mhDepModules = depModules
          , mhNameHashes = nameHashes
          , mhNameCount = nameCount
          , mhRoots = roots
          , mhTests = tests
          , mhDoc = mdoc
          } ->
      let nmodStub = I.NModule [] [] mdoc
          tmodStub = A.Module mn [] mdoc []
      in TyTask { name      = mn
                , tyHash     = srcHash
                , tyPubHash  = pubHash
                , tyImplHash = implHash
                , tyImports  = imps
                , tyDepModules = depModules
                , tyNameHashes = nameHashes
                , tyNameCount = nameCount
                , tyRoots   = roots
                , tyTests   = tests
                , tyDoc     = mdoc
                , iface     = nmodStub
                , typed     = tmodStub }
    SrcHead{ mhName = mn
           , mhSrc = srcContent
           , mhBytes = bytes
           , mhSourceMeta = mSourceMeta
           , mhSrcImports = imps
           } ->
      ParseTask mn srcContent bytes mSourceMeta imps
    HeadError{ mhName = mn, mhDiagnostics = diags } ->
      ParseErrorTask mn diags

readModuleDoc :: Source.SourceProvider
              -> C.GlobalOptions
              -> C.CompileOptions
              -> Paths
              -> String
              -> IO (Maybe (A.ModName, Maybe String))
readModuleDoc sp gopts opts paths actFile = do
  h <- readModuleHeader sp gopts opts paths actFile
  return $ case h of
    TyHead{ mhName = mn, mhDoc = mdoc } -> Just (mn, mdoc)
    SrcHead{ mhName = mn, mhDoc = mdoc } -> Just (mn, mdoc)
    HeadError{} -> Nothing

readModuleDocIndexEntry :: Source.SourceProvider
                        -> C.GlobalOptions
                        -> C.CompileOptions
                        -> Paths
                        -> String
                        -> IO (Maybe (A.ModName, Maybe String, Bool))
readModuleDocIndexEntry sp gopts opts paths actFile = do
  h <- readModuleHeader sp gopts opts paths actFile
  return $ case h of
    TyHead{ mhName = mn, mhNameCount = nameCount, mhDoc = mdoc } ->
      Just (mn, mdoc, shouldGenerateDocOutput opts (isTmp paths) nameCount)
    SrcHead{ mhName = mn, mhDoc = mdoc } -> Just (mn, mdoc, True)
    HeadError{} -> Nothing

-- | Materialize a source-backed task into a fully parsed ActonTask.
-- ParseTask reuses the snapshot captured during graph discovery; TyTask reparses
-- from the current source file because the cached header was deemed stale.
materializeTask :: C.CompileOptions
                -> Source.SourceProvider
                -> A.ModName
                -> FilePath
                -> M.Map A.ModName TaskKey
                -> Maybe (ParseProgress -> IO ())
                -> CompileTask
                -> IO CompileTask
materializeTask opts sp mn actFile providers mOnProgress task =
  case task of
    ActonTask{} -> return task
    ParseErrorTask{} -> return task
    ParseTask{ src = srcContent, srcBytes = bytes, sourceMeta = mSourceMeta } -> do
      emod <- parseStoredSource mn actFile srcContent
      case emod of
        Left diags -> return $ ParseErrorTask mn diags
        Right m -> return $ ActonTask mn srcContent bytes mSourceMeta (adjustImports providers m)
    TyTask{} -> do
      parsedRes <- parseActFile opts sp mn actFile mOnProgress
      case parsedRes of
        Left diags -> return (ParseErrorTask mn diags)
        Right (snap, m) -> do
          mSourceMeta <- if Source.ssIsOverlay snap then return Nothing else Just <$> readSourceFileMeta actFile
          return $ ActonTask mn (Source.ssText snap) (Source.ssBytes snap) mSourceMeta (adjustImports providers m)
  where
    parseStoredSource mn' file srcContent = do
      cwd <- getCurrentDirectory
      let displayFile = makeRelative cwd file
      parseActSource opts mn' displayFile srcContent mOnProgress

instance Pretty (A.ModName, TaskKey) where
    pretty (m, tk) = pretty m <+> text "-->" <+> pretty tk

instance Pretty TaskKey where
    pretty tk = pretty (tkMod tk) <+> parens (pretty (tkProj tk))

instance Pretty (M.Map A.ModName TaskKey) where
    pretty prov = nest 4 $ vcat $ map pretty (M.toList prov)

adjustImports :: M.Map A.ModName TaskKey -> A.Module -> A.Module
adjustImports providers m = --trace ("#### adjust for " ++ prstr (A.modname m) ++ "\n    old: " ++ prstrs (A.imps m) ++ "\n    new: " ++ prstrs imps') $
                            --trace ("  ## providers:\n" ++ render (pretty providers))
                            m{ A.imps = imps' }
  where imps'                        = map adjust (A.imps m)
        adjust (A.Import l ms)       = A.Import l (map adj ms)
        adjust (A.FromImport l m i)  = A.FromImport l (adj' m) i
        adjust (A.FromImportAll l m) = A.FromImportAll l (adj' m)
        adj mi@(A.ModuleItem m mbn)
          | Just m' <- adjusted m   = case mbn of
                                         Nothing -> A.ModuleItem m' (Just m)
                                         Just n  -> A.ModuleItem m' (Just n)
          | otherwise               = mi
        adj' m
          | Just m' <- adjusted m   = m'
          | otherwise               = m
        adjusted m = case M.lookup m providers of
                        Just tk | tkMod tk /= m -> Just (tkMod tk)
                        _                       -> Nothing

-- | Recursively read imports for a set of tasks within the same project.
-- Any missing module is added by parsing or reading its .tydb header via
-- readModuleTask, yielding a self-contained task list.
readImports :: Source.SourceProvider -> C.GlobalOptions -> C.CompileOptions -> Paths -> [CompileTask] -> IO [CompileTask]
readImports sp gopts opts paths tasks = do
    let tasks' = filter (\t -> name t /= modName paths) tasks
    chaseRecursively tasks' [] (concatMap importsOf tasks')
  where
    readAFile tasks mn = case lookUp mn tasks of
      Just _ -> return Nothing
      Nothing -> do
        let actFile = srcFile paths mn
        ok <- doesFileExist actFile
        if ok
          then do
            p <- findPaths actFile opts
            t <- readModuleTask sp gopts opts p actFile
            return (Just t)
          else return Nothing

    lookUp mn (t:ts)
      | name t == mn = Just t
      | otherwise = lookUp mn ts
    lookUp _ [] = Nothing

    chaseRecursively tasks mns [] = return tasks
    chaseRecursively tasks mns (imn:imns)
      | imn `elem` mns = chaseRecursively tasks mns imns
      | otherwise = do
          t <- readAFile tasks imn
          chaseRecursively (maybe tasks (:tasks) t)
                           (imn:mns)
                           (imns ++ concatMap importsOf (maybe [] (:[]) t))


-- | Decide whether to suppress per-module timing/log output.
-- We also treat any alternate output mode (parse/sigs/cgen/etc) as quiet.
quiet :: C.GlobalOptions -> C.CompileOptions -> Bool
quiet gopts opts = C.quiet gopts || altOutput opts

-- | Read a reusable interface shell from a .tydb file and return its hashes.
-- This is used when a module is deemed fresh and we want to avoid reparsing.
readIfaceFromTy :: Paths -> A.ModName -> String -> Maybe B.ByteString -> IO (Either [Diagnostic String] ([A.ModName], [(A.Name, I.NameInfo)], Maybe String, B.ByteString, B.ByteString, Maybe Acton.Env.ModuleInfo))
readIfaceFromTy paths mn src mHash = do
    mty <- Acton.Env.findTyFile (searchPath paths) mn
    case mty of
      Nothing -> return $ Left (missingIfaceDiagnostics mn src mn)
      Just tyF -> do
        hashes <- InterfaceFiles.readModuleHashesMaybe tyF
        mdb <- InterfaceFiles.openInterfaceDBMaybe tyF
        case (hashes, mdb) of
          (Just (_srcH, pubH, implH), Just db) -> do
            (ms, mdoc) <- InterfaceFiles.readInterfaceDBModuleInfo db
            ih <- case mHash of
                    Just h -> return h
                    Nothing -> return pubH
            return $ Right (ms, [], mdoc, ih, implH, Just (Acton.Env.mkTyFileModuleInfo mn ms mdoc db))
          _ -> return $ Left (missingIfaceDiagnostics mn src mn)


-- | Snapshot of expected and recorded hashes for generated code.
data CodegenStatus = CodegenStatus
  { csExpected :: String
  , csC :: Maybe String
  , csH :: Maybe String
  }

-- | Header prefix used to tag generated code with its exact codegen hash.
codegenHashTag :: String
codegenHashTag = "/* Acton codegen hash:"

-- | Parse a tagged hash line from generated output.
extractCodegenHash :: String -> Maybe String
extractCodegenHash line = do
  rest <- Data.List.stripPrefix codegenHashTag line
  let rest' = dropWhile isSpace rest
      hex = takeWhile isHexDigit rest'
  if null hex then Nothing else Just hex

-- | Read the tagged hash from a generated file, if present.
readCodegenHash :: FilePath -> IO (Maybe String)
readCodegenHash path = do
  exists <- doesFileExist path
  if not exists
    then return Nothing
    else do
      res <- (try :: IO a -> IO (Either SomeException a)) $ withFile path ReadMode hGetLine
      case res of
        Left _ -> return Nothing
        Right line -> return (extractCodegenHash line)

-- | Collect codegen hash status for a module.
codegenStatus :: Paths -> A.ModName -> B.ByteString -> IO CodegenStatus
codegenStatus paths mn codegenHash = do
  let expected = B.unpack $ Base16.encode codegenHash
      cFile = outBase paths mn ++ ".c"
      hFile = outBase paths mn ++ ".h"
  cHash <- readCodegenHash cFile
  hHash <- readCodegenHash hFile
  return CodegenStatus { csExpected = expected, csC = cHash, csH = hHash }

-- | Check whether generated .c/.h hashes match the expected codegen hash.
codegenUpToDate :: CodegenStatus -> Bool
codegenUpToDate status =
  csC status == Just (csExpected status) && csH status == Just (csExpected status)

-- | Format a short delta message for stale codegen.
formatCodegenDelta :: CodegenStatus -> String
formatCodegenDelta status =
  let expected = take 8 (csExpected status)
      short actual = maybe "missing" (take 8) actual
  in case (csC status, csH status) of
       (Just c, Just h) | c == h ->
         " {codegen " ++ take 8 c ++ " -> " ++ expected ++ "}"
       _ ->
         " {codegen c " ++ short (csC status) ++ " -> " ++ expected
         ++ ", h " ++ short (csH status) ++ " -> " ++ expected ++ "}"

-- | Run the front passes for a single module.
-- Builds the environment, runs kinds/types, computes per-name src/pub/impl
-- hashes plus module pub/impl hashes, and starts the post-front output jobs.
-- Returns the front result plus a BackJob for later passes when compilation is
-- needed. Callers must wait for frOutputJobs before treating the compile as
-- finished.

runFrontPasses :: C.GlobalOptions
               -> C.CompileOptions
               -> Bool
               -> Bool
               -> Paths
               -> Acton.Env.Env0
               -> A.Module
               -> String
               -> B.ByteString
               -> Maybe InterfaceFiles.SourceFileMeta
               -> (A.ModName -> IO (Maybe B.ByteString))
               -> (A.ModName -> IO (Maybe B.ByteString))
               -> (A.ModName -> A.Name -> IO (Maybe InterfaceFiles.NameHashInfo))
               -> (FrontPassProgress -> IO ())
               -> (TaskKey -> FrontOutputKind -> IO ())
               -> (TaskKey -> FrontOutputKind -> FrontOutputProgress -> IO ())
               -> (TaskKey -> FrontOutputKind -> Maybe TimeSpec -> IO ())
               -> IO Bool
               -> ([FrontOutputJob] -> IO ())
               -> IO (Either [Diagnostic String] FrontResult)
runFrontPasses gopts opts dbpBlocked requested paths env0 parsed srcContent srcBytes sourceMeta resolveImportHash resolveImportImplHash resolveNameHash onFrontProgress onFrontOutputStart onFrontOutputProgress onFrontOutputDone shouldWriteFrontOutput recordFrontOutputJobs = do
  createDirectoryIfMissing True (getModPath (projTypes paths) mn)
  core
    `catch` handleGeneral
    `catch` handleCompilation
    `catch` handleTypeErrors
    `catch` handleTypeError
  where
    mn = A.modname parsed
    filename = modNameToFilename (dropProjPrefix paths mn)
    outbase = outBase paths mn
    absSrcBase = srcBase paths mn
    actFile = absSrcBase ++ ".act"
    prettyAstStyle = style { mode = PageMode, lineLength = 120, ribbonsPerLine = 1.0 }

    handleGeneral :: GeneralError -> IO (Either [Diagnostic String] FrontResult)
    handleGeneral err =
      return $ Left (errsToDiagnostics "Compilation error" filename srcContent (generalError err))

    handleCompilation :: Acton.Env.CompilationError -> IO (Either [Diagnostic String] FrontResult)
    handleCompilation err =
      return $ Left (errsToDiagnostics "Compilation error" filename srcContent (Acton.Env.compilationError err))

    handleTypeError :: Acton.TypeEnv.TypeError -> IO (Either [Diagnostic String] FrontResult)
    handleTypeError err =
      return $ Left [Acton.TypeEnv.mkErrorDiagnostic filename srcContent (Acton.TypeEnv.typeReport err filename srcContent)]

    handleTypeErrors :: Acton.Types.TypeErrors -> IO (Either [Diagnostic String] FrontResult)
    handleTypeErrors (Acton.Types.TypeErrors errs) =
      return $ Left [ Acton.TypeEnv.mkErrorDiagnostic filename srcContent (Acton.TypeEnv.typeReport err filename srcContent)
                    | err <- errs
                    ]

    resolveImportHashes :: [A.ModName] -> IO (Either [Diagnostic String] [(A.ModName, B.ByteString)])
    resolveImportHashes mrefs = do
      resolved <- forM mrefs $ \mref -> do
        mh <- resolveImportHash mref
        case mh of
          Just ih -> return (Right (mref, ih))
          Nothing -> return (Left (missingIfaceDiagnostics mn srcContent mref))
      let (errs, vals) = partitionEithers resolved
      if null errs
        then return (Right vals)
        else return (Left (concat errs))

    resolveDepModuleHashes :: [A.ModName] -> IO (Either [Diagnostic String] [InterfaceFiles.DepModuleInfo])
    resolveDepModuleHashes mrefs = do
      resolved <- forM mrefs $ \mref -> do
        mpub <- resolveImportHash mref
        mimpl <- resolveImportImplHash mref
        case (mpub, mimpl) of
          (Just pubH, Just implH) ->
            return (Right InterfaceFiles.DepModuleInfo
                      { InterfaceFiles.dmiModule = mref
                      , InterfaceFiles.dmiPubHash = pubH
                      , InterfaceFiles.dmiImplHash = implH
                      })
          _ -> return (Left (missingIfaceDiagnostics mn srcContent mref))
      let (errs, vals) = partitionEithers resolved
      if null errs
        then return (Right vals)
        else return (Left (concat errs))

    missingNameHashDiagnostics :: A.QName -> [Diagnostic String]
    missingNameHashDiagnostics qn =
      errsToDiagnostics "Compilation error" filename srcContent
        [(NoLoc, "Hash info missing for " ++ prstr qn)]

    missingDepHashDiagnostics :: String -> String -> SrcLoc -> A.QName -> [Diagnostic String]
    missingDepHashDiagnostics label owner loc qn =
      errsToDiagnostics "Compilation error" filename srcContent
        [(loc, label ++ " hash missing for " ++ prstr qn ++ " (used by " ++ owner ++ ")")]

    resolveDepHash :: String
                   -> (InterfaceFiles.NameHashInfo -> B.ByteString)
                   -> String
                   -> SrcLoc
                   -> A.QName
                   -> IO (Either [Diagnostic String] (A.QName, B.ByteString))
    resolveDepHash label getHash owner loc qn = case qn of
      A.GName m n -> lookupName m n
      A.QName m n -> lookupName m n
      A.NoQ _ -> return (Left (missingNameHashDiagnostics qn))
      where
        lookupName m n = do
          mInfo <- resolveNameHash m n
          return $ case mInfo of
            Just info
              | not (B.null (getHash info)) -> Right (A.GName m n, getHash info)
              | otherwise -> Left (missingDepHashDiagnostics label owner loc (A.GName m n))
            Nothing -> Left (missingNameHashDiagnostics (A.GName m n))

    resolveDepHashes :: String
                     -> (InterfaceFiles.NameHashInfo -> B.ByteString)
                     -> M.Map A.Name [A.QName]
                     -> M.Map A.Name SrcLoc
                     -> IO (Either [Diagnostic String] (M.Map A.Name [(A.QName, B.ByteString)]))
    resolveDepHashes label getHash deps nameLocs = do
      resolved <- forM (M.toList deps) $ \(owner, qns) -> do
        let loc = M.findWithDefault NoLoc owner nameLocs
        resolvedQns <- forM qns (resolveDepHash label getHash (A.nstr owner) loc)
        let (errs, vals) = partitionEithers resolvedQns
        return $ if null errs
          then Right (owner, vals)
          else Left (concat errs)
      let (errs, vals) = partitionEithers resolved
      return $ if null errs then Right (M.fromList vals) else Left (concat errs)

    resolveModuleDepHashes :: String
                           -> (InterfaceFiles.NameHashInfo -> B.ByteString)
                           -> [A.QName]
                           -> IO (Either [Diagnostic String] [(A.QName, B.ByteString)])
    resolveModuleDepHashes label getHash qns = do
      resolved <- mapM (resolveDepHash label getHash "module initialization" NoLoc) qns
      let (errs, vals) = partitionEithers resolved
      return $ if null errs then Right vals else Left (concat errs)

    emitFrontProgress pass completed total current =
      onFrontProgress FrontPassProgress
        { fppPass = pass
        , fppCompleted = completed
        , fppTotal = total
        , fppCurrent = current
        }

    emitHashProgress completed total current =
      emitFrontProgress FrontPassHash completed total current

    frontProgressMinIntervalNanos = 50000000 :: Integer

    timeSpecNanos t =
      fromIntegral (sec t) * 1000000000 + fromIntegral (nsec t)

    core = do
      timeStart <- getTime Monotonic
      let isRoot = mn == modName paths
      when (C.parse opts && isRoot) $
        dump mn "parse" (Pretty.print parsed)
      when (C.parse_ast opts && isRoot) $
        dump mn "parse-ast" (renderStyle prettyAstStyle (ppDoc parsed))

      typeStmtTimingsRef <- newIORef ([] :: [TypeStmtTiming])
      inferredSigsRef <- newIORef ([] :: [InferredSignature])
      typeActiveRef <- newIORef Nothing
      typeProgressDoneRef <- newIORef Nothing
      typeProgressLastRef <- newIORef Nothing
      hashProgressLastRef <- newIORef Nothing
      let collectTypeStmtTimings = C.timing gopts && C.verbose gopts
      let shouldEmitPaced force lastRef now completed total = do
            mLast <- readIORef lastRef
            let done = min total (max 0 completed)
                shouldEmit =
                  force ||
                  done >= total ||
                  maybe True (\lastEmit -> timeSpecNanos now - timeSpecNanos lastEmit >= frontProgressMinIntervalNanos) mLast
            when shouldEmit $
              writeIORef lastRef (Just now)
            pure (shouldEmit, done)
          emitTypeProgressPaced force now completed total current = do
            (shouldEmit, done) <- shouldEmitPaced force typeProgressLastRef now completed total
            when shouldEmit $
              emitFrontProgress FrontPassTypes done total current
      let onTypeProgress total completed current names _weight = do
            now <- getTime Monotonic
            when (total > 0 && completed >= total && isNothing current) $ do
              mDone <- readIORef typeProgressDoneRef
              when (isNothing mDone) $
                writeIORef typeProgressDoneRef (Just now)
            when collectTypeStmtTimings $ do
              mActive <- readIORef typeActiveRef
              forM_ mActive $ \(label, bindNames, activeTotal, t0) ->
                modifyIORef' typeStmtTimingsRef
                  ( TypeStmtTiming
                      { tstCompleted = completed
                      , tstTotal = activeTotal
                      , tstLabel = label
                      , tstNames = bindNames
                      , tstTime = now - t0
                      }
                  : )
              case current of
                Just label -> writeIORef typeActiveRef (Just (label, names, total, now))
                Nothing -> writeIORef typeActiveRef Nothing
            emitTypeProgressPaced False now completed total current
          onInferredSignature names sig =
            modifyIORef' inferredSigsRef (InferredSignature names sig :)
          inferredSignatureCb =
            if C.timing gopts || C.verbose gopts
              then Just onInferredSignature
              else Nothing
          emitHashProgressPaced force completed total current = do
            now <- getTime Monotonic
            (shouldEmit, done) <- shouldEmitPaced force hashProgressLastRef now completed total
            when shouldEmit $
              emitHashProgress done total current

      env <- Acton.Env.mkEnv (searchPath paths) env0 parsed
      timeEnv <- getTime Monotonic

      emitFrontProgress FrontPassKinds 0 1 Nothing
      kchecked <- Acton.Kinds.check env parsed
      emitFrontProgress FrontPassKinds 1 1 Nothing
      iff (C.kinds opts && isRoot) $ dump mn "kinds" (Pretty.print kchecked)
      timeKindsCheck <- getTime Monotonic

      -- Type-check and return both the typed AST and the interface NameInfo.
      (nmod,tchecked,typeEnv,tests) <- Acton.Types.reconstruct (Just onTypeProgress) inferredSignatureCb env kchecked (Just (modNameToString (dropProjPrefix paths mn)))
      timeTypeReconstruct <- getTime Monotonic
      forceTypeResult nmod tchecked typeEnv tests
      timeTypeForce <- getTime Monotonic
      -- Store roots so later builds can discover entry points without reparse.
      let I.NModule imps fullIface mdoc = nmod
          publicIface = publicIfaceTE fullIface
      let roots = [ n | (n,i) <- fullIface, rootEligible i ]
          hashEnv = setMod mn env
          -- Constructor partitioning follows the backend tree, whose imported
          -- protocols have already become generated sibling classes.
          rowEnv = define fullIface (setMod mn typeEnv)
          moduleRows =
            case InterfaceRowsBuilder.prepareInterfaceRows rowEnv tchecked of
              Left (InterfaceRows.RowError msg) ->
                error ("Internal error while preparing .tydb rows for " ++
                       modNameToString mn ++ ": " ++ msg)
              Right rows -> rows
          reachabilityRows =
            case ReachabilityRowsBuilder.prepareReachabilityRows typeEnv fullIface tchecked moduleRows of
              Left (InterfaceRows.RowError msg) ->
                error ("Internal error while preparing reachability rows for " ++
                       modNameToString mn ++ ": " ++ msg)
              Right rows -> rows
          topLevelOwners = map InterfaceRows.storedStmtNames (InterfaceRows.rowStatements moduleRows)
          moduleStatements =
            [ stmt
            | InterfaceRows.StoredWhole [] stmt <- InterfaceRows.rowStatements moduleRows
            ]
          -- Parsed and typed fragments use the same ownership partition that
          -- is persisted for selective reconstruction.
          srcItems = Hashing.sourceTopLevelItems parsed
          implItems = Hashing.topLevelItems topLevelOwners tchecked
          -- NameInfo defines the full local environment for this module.
          nameInfoMap = M.fromList fullIface
          nameLocsParsed = M.fromListWith (\a _ -> a)
            [ (n, A.dloc d) | Hashing.TLDecl n d <- srcItems ] `M.union`
            M.fromListWith (\a _ -> a)
            [ (n, A.sloc s) | Hashing.TLStmt n s <- srcItems ]
          nameLocsTyped = M.fromListWith (\a _ -> a)
            [ (n, A.dloc d) | Hashing.TLDecl n d <- implItems ] `M.union`
            M.fromListWith (\a _ -> a)
            [ (n, A.sloc s) | Hashing.TLStmt n s <- implItems ]
          nameLocs = M.union nameLocsParsed nameLocsTyped
          sourceHashWork = length srcItems
          implHashWork = length implItems
          ifaceHashWork = M.size nameInfoMap
          ifaceDepWork = M.size nameInfoMap
          implDepWork = length implItems
          afterSourceHashes = sourceHashWork
          afterImplHashes = afterSourceHashes + implHashWork
          afterIfaceHashes = afterImplHashes + ifaceHashWork
          afterIfaceDeps = afterIfaceHashes + ifaceDepWork
          afterImplDeps = afterIfaceDeps + implDepWork
          hashProgressTotal = afterImplDeps + 1
          hashProgressCheckStride = max 1 (hashProgressTotal `div` 1000)
          hashProgress base done = do
            let completed = base + done
            when (completed `rem` hashProgressCheckStride == 0) $
              emitHashProgressPaced False completed hashProgressTotal Nothing
      evaluate (rnf moduleRows)
      evaluate (rnf reachabilityRows)
      evaluate (sourceHashWork + implHashWork + ifaceHashWork + ifaceDepWork + implDepWork + M.size nameLocs)
      emitHashProgressPaced True 0 hashProgressTotal Nothing
      -- Module-level src hash uses raw bytes so any source edit forces re-parse.
      let moduleSrcBytesHash = SHA256.hash srcBytes
      evaluate (rnf moduleSrcBytesHash)
      -- Import hashes are recorded in the .tydb header so dep changes can be detected.
      let hashImps =
            if mn == mBuiltin
              then imps
              else nub (mBuiltin : imps)
      impsRes <- resolveImportHashes hashImps
      case impsRes of
        Left diags -> return (Left diags)
        Right impsWithHash -> do
          nameSrcHashes <-
            Hashing.nameHashesFromItemsWithProgress
              (hashProgress 0)
              srcItems
          evaluate (rnf nameSrcHashes)
          nameImplHashes <-
            Hashing.nameHashesFromItemsWithProgress
              (hashProgress afterSourceHashes)
              implItems
          evaluate (rnf nameImplHashes)
          let nameKeys = M.keysSet nameSrcHashes `Data.Set.union` M.keysSet nameImplHashes
              moduleOwnHash = Hashing.moduleOwnImplHash (A.importsOf tchecked) moduleStatements
              (moduleLocalDeps, moduleImplExtDeps) =
                Hashing.moduleImplSplitDeps mn hashEnv nameKeys moduleStatements
              modulePubExtDeps = Hashing.publicImplDeps moduleImplExtDeps
          evaluate (rnf (nameKeys, moduleOwnHash, moduleLocalDeps, modulePubExtDeps, moduleImplExtDeps))
          selfPubHashes <-
            Hashing.nameInfoHashesWithProgress
              (hashProgress afterImplHashes)
              nameInfoMap
          evaluate (rnf selfPubHashes)
          -- Split deps into local (same module) vs external (qualified) names.
          (pubSigLocalDeps, pubSigExtDeps) <-
            Hashing.pubSigSplitDepsFromNameInfoMapWithProgress
              (hashProgress afterIfaceHashes)
              mn hashEnv nameKeys nameInfoMap
          evaluate (rnf (pubSigLocalDeps, pubSigExtDeps))
          -- implDeps: term-level deps from typed bodies.
          (implLocalDeps, implExtDeps) <-
            Hashing.implSplitDepsFromItemsWithProgress
              (hashProgress afterIfaceDeps)
              mn hashEnv nameKeys implItems
          evaluate (rnf (implLocalDeps, implExtDeps))
          -- pubDeps include signature deps plus term-level deps for reuse
          -- in pubHash. Derived names are internal and should never require
          -- a pub hash.
          let (pubLocalDeps, pubExtDeps) =
                Hashing.mergePubDeps pubSigLocalDeps pubSigExtDeps implLocalDeps implExtDeps
              extMods = Data.Set.toList $
                Hashing.externalModules (concat $ M.elems pubExtDeps) `Data.Set.union`
                Hashing.externalModules (concat $ M.elems implExtDeps) `Data.Set.union`
                Hashing.externalModules modulePubExtDeps `Data.Set.union`
                Hashing.externalModules moduleImplExtDeps
          evaluate (rnf (pubLocalDeps, pubExtDeps, extMods))
          depModulesRes <- resolveDepModuleHashes extMods
          case depModulesRes of
            Left diags -> return (Left diags)
            Right depModules -> do
              -- Resolve external deps to their recorded hashes.
              pubSigExtRes <- resolveDepHashes "pub" InterfaceFiles.nhPubHash pubSigExtDeps nameLocs
              pubExtRes <- resolveDepHashes "pub" InterfaceFiles.nhPubHash pubExtDeps nameLocs
              implExtRes <- resolveDepHashes "impl" InterfaceFiles.nhImplHash implExtDeps nameLocs
              modulePubExtRes <- resolveModuleDepHashes "pub" InterfaceFiles.nhPubHash modulePubExtDeps
              moduleImplExtRes <- resolveModuleDepHashes "impl" InterfaceFiles.nhImplHash moduleImplExtDeps
              case (pubSigExtRes, pubExtRes, implExtRes, modulePubExtRes, moduleImplExtRes) of
                (Left diags, _, _, _, _) -> return (Left diags)
                (_, Left diags, _, _, _) -> return (Left diags)
                (_, _, Left diags, _, _) -> return (Left diags)
                (_, _, _, Left diags, _) -> return (Left diags)
                (_, _, _, _, Left diags) -> return (Left diags)
                (Right pubSigExtHashes, Right pubExtHashes, Right implExtHashes,
                 Right modulePubExtHashes, Right moduleImplExtHashes) -> do
                  evaluate (rnf (pubSigExtHashes, pubExtHashes, implExtHashes,
                                 modulePubExtHashes, moduleImplExtHashes))
                  let pubHashes = Hashing.computeHashesSortedDeps selfPubHashes pubSigLocalDeps pubSigExtHashes
                      implHashes = Hashing.computeHashesSortedDeps nameImplHashes implLocalDeps implExtHashes
                      moduleHashInfo = Hashing.finishModuleHash
                        implHashes moduleOwnHash topLevelOwners moduleLocalDeps
                        modulePubExtHashes moduleImplExtHashes
                      (modulePubHash, moduleImplHash) =
                        Hashing.moduleHashesFromHashMaps nmod moduleHashInfo nameKeys pubHashes implHashes
                  evaluate (rnf (pubHashes, implHashes, moduleHashInfo, modulePubHash, moduleImplHash))
                  let nameHashes =
                        Hashing.assembleNameHashes
                          nameKeys
                          nameSrcHashes
                          pubHashes
                          implHashes
                          nameImplHashes
                          pubLocalDeps
                          implLocalDeps
                          pubExtHashes
                          implExtHashes
                  evaluate (rnf nameHashes)
                  evaluate (rnf (moduleSrcBytesHash, modulePubHash, moduleImplHash, sourceMeta, impsWithHash))
                  evaluate (rnf (nameHashes, roots, tests, mdoc))
                  emitHashProgressPaced True hashProgressTotal hashProgressTotal Nothing
                  timeTypeHash <- getTime Monotonic

                  iff (C.types opts && isRoot) $ dump mn "types" (Pretty.print tchecked)
                  iff (C.sigs opts && isRoot) $ dump mn "sigs" (Acton.Types.prettySigs env mn imps fullIface)
                  timeTypeCheck <- getTime Monotonic

                  let writeTyDb outputKey = do
                        InterfaceFiles.writeFile
                          (\p -> onFrontOutputProgress outputKey FrontOutputTydb (frontOutputTyDbProgress p))
                          (tyDbPath paths mn)
                          InterfaceFiles.InterfaceContents
                            { InterfaceFiles.ifcSourceHash = moduleSrcBytesHash
                            , InterfaceFiles.ifcPublicHash = modulePubHash
                            , InterfaceFiles.ifcImplementationHash = moduleImplHash
                            , InterfaceFiles.ifcModuleHashInfo = moduleHashInfo
                            , InterfaceFiles.ifcSourceMeta = sourceMeta
                            , InterfaceFiles.ifcImports = impsWithHash
                            , InterfaceFiles.ifcDependencies = depModules
                            , InterfaceFiles.ifcNameHashes = nameHashes
                            , InterfaceFiles.ifcRoots = roots
                            , InterfaceFiles.ifcTests = tests
                            , InterfaceFiles.ifcDoc = mdoc
                            , InterfaceFiles.ifcModule = nmod
                            , InterfaceFiles.ifcRows = moduleRows
                            , InterfaceFiles.ifcReachabilityRows = reachabilityRows
                            }
                      docOutputActions =
                        if shouldGenerateDocOutput opts (isTmp paths) (length nameHashes)
                          then
                            let writeDoc = do
                                  let docDir = joinPath [projPath paths, "out", "doc"]
                                      modPathList = A.modPath mn
                                      docFile = if null modPathList
                                                then docDir </> "unnamed" <.> "html"
                                                else joinPath (docDir : init modPathList) </> last modPathList <.> "html"
                                      docFileDir = takeDirectory docFile
                                      -- Get the type environment for this module
                                      modTypeEnv = case Acton.Env.lookupModuleInfo mn typeEnv of
                                        Just mi -> Acton.Env.modulePublicTEnv mi
                                        Nothing -> publicIface
                                      -- Apply the same simplification as --sigs uses
                                      env1 = define publicIface $ setMod mn env
                                      simplifiedTypeEnv = simp env1 modTypeEnv
                                  createDirectoryIfMissing True docFileDir
                                  -- Use parsed (original AST) to preserve docstrings
                                  let htmlDoc = DocP.printHtmlDoc (I.NModule imps simplifiedTypeEnv mdoc) parsed
                                  writeFile docFile htmlDoc
                            in [(FrontOutputDoc, writeDoc)]
                          else []
                  typeStmtTimings <- reverse <$> readIORef typeStmtTimingsRef
                  typeProgressDone <- readIORef typeProgressDoneRef

                  timeFrontEnd <- getTime Monotonic
                  inferredSigs <- reverse <$> readIORef inferredSigsRef
                  let typeAfterProgress =
                        case typeProgressDone of
                          Just t -> timeTypeReconstruct - t
                          Nothing -> timeTypeReconstruct - timeTypeReconstruct
                  let frontTime = timeFrontEnd - timeStart
                      frontTimeMaybe = if not (quiet gopts opts)
                                         then Just frontTime
                                         else Nothing
                      frontTimingMaybe =
                        if C.timing gopts
                          then Just FrontTiming
                                 { ftEnv = timeEnv - timeStart
                                 , ftKinds = timeKindsCheck - timeEnv
                                 , ftTypes = timeTypeCheck - timeKindsCheck
                                 , ftTypeReconstruct = timeTypeReconstruct - timeKindsCheck
                                 , ftTypeAfterProgress = typeAfterProgress
                                 , ftTypeForce = timeTypeForce - timeTypeReconstruct
                                 , ftTypeHash = timeTypeHash - timeTypeForce
                                 , ftTypeStmtTimings = typeStmtTimings
                                 }
                          else Nothing
                      backPassNeeded = shouldRunBackPass dbpBlocked requested opts roots
                      deferredBackJob
                        | backPassNeeded = selectiveDeferredBackJob dbpBlocked
                            (A.hasNotImpl (A.mbody tchecked)) opts paths mn
                        | otherwise = Nothing
                      backJob =
                        case deferredBackJob of
                          Just _ -> Nothing
                          Nothing | backPassNeeded ->
                            Just BackJob { bjPaths = paths
                                         , bjOpts = opts
                                         , bjInput = BackInput { biTypeEnv = typeEnv
                                                              , biTypedMod = tchecked
                                                              , biDeclarations = []
                                                              , biSrc = Just srcContent
                                                              , biCodegenHash = Hashing.wholeCodegenHash
                                                                  (not $ C.dbg_no_lines opts)
                                                                  moduleImplHash moduleSrcBytesHash
                                                              }
                                         , bjValidate = return ()
                                         }
                          Nothing -> Nothing
                  evaluate backJob
                  evaluate docOutputActions
                  do
                    let outputKey = TaskKey (projPath paths) mn
                    -- The .tydb commit is synchronous with front completion:
                    -- once this module reports done, its rows are on disk.
                    -- A write failure (disk full, permissions) fails THIS
                    -- module's front on the structured diagnostics path.
                    -- Docs and the interface copy are never read back within
                    -- this build, so they stay in the background.
                    writeRes <- try (runFrontOutputSync onFrontOutputStart onFrontOutputDone shouldWriteFrontOutput outputKey FrontOutputTydb (writeTyDb outputKey))
                    case writeRes of
                      Left err | isJust (fromException err :: Maybe SomeAsyncException) -> throwIO err
                               | otherwise -> return (Left (tydbWriteDiagnostics mn err))
                      Right () -> do
                        outputJobs <- mask_ $ do
                          let startOutput =
                                startFrontOutputJob onFrontOutputStart onFrontOutputDone shouldWriteFrontOutput outputKey
                          docJobs <-
                            forM docOutputActions $ uncurry startOutput
                          tyJobs <-
                            if C.tydb opts
                              then (:[]) <$> startOutput FrontOutputTydbCopy (copyTydbInterface opts paths mn)
                              else return []
                          let jobs = docJobs ++ tyJobs
                          recordFrontOutputJobs jobs
                          return jobs
                        ifaceRes <- readIfaceFromTy paths mn srcContent (Just modulePubHash)
                        case ifaceRes of
                          Left diags -> return (Left diags)
                          Right (_,_,_,_,_,moduleInfo) ->
                            return $ Right FrontResult
                              { frIfaceTE = []
                              , frImps = imps
                              , frDoc = mdoc
                              , frModuleInfo = moduleInfo
                              , frPubHash = modulePubHash
                              , frImplHash = moduleImplHash
                              , frNameHashes = publicNameHashes nameHashes
                              , frFrontTime = frontTimeMaybe
                              , frFrontTiming = frontTimingMaybe
                              , frInferredSigs = inferredSigs
                              , frBackJob = backJob
                              , frDeferredBackJob = deferredBackJob
                              , frOutputJobs = outputJobs
                              }

-- | Run the back passes for a single module.
-- Executes normalization through codegen, writes .c/.h output as needed, and
-- returns the back-pass elapsed time for logging.
runBackPasses :: C.GlobalOptions
              -> C.CompileOptions
              -> Paths
              -> BackInput
              -> (IO () -> IO Bool)
              -> (BackPassProgress -> IO ())
              -> IO (Maybe TimeSpec, Maybe BackTiming)
runBackPasses gopts opts paths backInput commitWrite onProgress = do
      let mn = A.modname (biTypedMod backInput)
          outbase = outBase paths mn
          relSrcBase = makeRelative (projPath paths) (srcBase paths mn)
          writesOutput = not (altOutput opts)
          backPasses =
            [ BackPassNormalize
            , BackPassDeactorize
            , BackPassCPS
            , BackPassLLift
            , BackPassBoxing
            , BackPassCodeGen
            , BackPassRender
            ] ++ [BackPassWrite | writesOutput]
          backPassTotal = length backPasses
          backPassIndex pass =
            case Data.List.elemIndex pass backPasses of
              Just ix -> ix
              Nothing -> backPassTotal
          emitStarted pass =
            onProgress (BackPassStarted pass (backPassIndex pass) backPassTotal)
          emitFinished pass t0 t1 =
            onProgress (BackPassFinished pass (backPassIndex pass + 1) backPassTotal (t1 - t0))
          emitSkipped pass =
            onProgress (BackPassSkipped pass (backPassIndex pass + 1) backPassTotal)
      timeStart <- getTime Monotonic
      let timedBackPass pass action = do
            emitStarted pass
            passStart <- getTime Monotonic
            result <- action
            passEnd <- getTime Monotonic
            emitFinished pass passStart passEnd
            return (result, passEnd - passStart)
          finish tNormalize tDeactorize tCPS tLLift tBoxing tCodeGen tRender mWriteTime = do
            timeEnd <- getTime Monotonic
            let backTime = timeEnd - timeStart
            let backTimingMaybe =
                  if C.timing gopts
                    then Just BackTiming
                           { btNormalize = tNormalize
                           , btDeactorize = tDeactorize
                           , btCPS = tCPS
                           , btLLift = tLLift
                           , btBoxing = tBoxing
                           , btCodeGen = tCodeGen
                           , btRender = tRender
                           , btWrite = mWriteTime
                           }
                    else Nothing
            if not (quiet gopts opts)
              then return (Just backTime, backTimingMaybe)
              else return (Nothing, backTimingMaybe)
          forceOut s = evaluate (rnf s)

      ((normalized, normEnv), tNormalize) <- timedBackPass BackPassNormalize $ do
        res@(normalized', _) <- Acton.Normalizer.normalize (biTypeEnv backInput) (biTypedMod backInput)
        iff (C.norm opts && mn == (modName paths)) $ dump mn "norm" (Pretty.print normalized')
        return res

      ((deacted,deactEnv), tDeactorize) <- timedBackPass BackPassDeactorize $ do
        res@(deacted', _) <- Acton.Deactorizer.deactorize normEnv normalized
        iff (C.deact opts && mn == (modName paths)) $ dump mn "deact" (Pretty.print deacted')
        return res

      ((cpstyled,cpsEnv), tCPS) <- timedBackPass BackPassCPS $ do
        res@(cpstyled', _) <- Acton.CPS.convert deactEnv deacted
        iff (C.cps opts && mn == (modName paths)) $ dump mn "cps" (Pretty.print cpstyled')
        return res

      ((lifted,liftEnv), tLLift) <- timedBackPass BackPassLLift $ do
        res@(lifted', _) <- Acton.LambdaLifter.liftModule cpsEnv cpstyled
        iff (C.llift opts && mn == (modName paths)) $ dump mn "llift" (Pretty.print lifted')
        return res

      (boxed, tBoxing) <- timedBackPass BackPassBoxing $ do
        res <- Acton.Boxing.doBoxing liftEnv lifted
        iff (C.box opts && mn == (modName paths)) $ dump mn "box" (Pretty.print res)
        return res

      ((_,h,c), tCodeGen) <- timedBackPass BackPassCodeGen $ do
        let hexHash = B.unpack $ Base16.encode (biCodegenHash backInput)
            srcText = fromMaybe "" (biSrc backInput)
            emitLines = isJust (biSrc backInput) && not (C.dbg_no_lines opts)
        Acton.CodeGen.generate liftEnv (biDeclarations backInput)
          relSrcBase srcText emitLines boxed hexHash

      let finishBack = finish tNormalize tDeactorize tCPS tLLift tBoxing tCodeGen
      result <- if C.hgen opts
        then do
          (_, tRender) <- timedBackPass BackPassRender (forceOut h)
          putStrLn h
          finishBack tRender Nothing
        else if C.cgen opts
          then do
            (_, tRender) <- timedBackPass BackPassRender (forceOut c)
            putStrLn c
            finishBack tRender Nothing
          else do
            (_, tRender) <- timedBackPass BackPassRender (forceOut h >> forceOut c)
            mWriteTime <-
              if not writesOutput
                then return Nothing
                else do
                  writeTime <- newIORef Nothing
                  wrote <- commitWrite $ do
                    (_, tWrite) <- timedBackPass BackPassWrite $ do
                      let cFile = outbase ++ ".c"
                          hFile = outbase ++ ".h"
                      writeFile hFile h
                      writeFile cFile c
                    writeIORef writeTime (Just tWrite)
                  if not wrote
                    then do
                      emitSkipped BackPassWrite
                      return Nothing
                    else readIORef writeTime
            finishBack tRender mWriteTime
      return result


-- | Compile a set of GlobalTasks using a parallel, dependency-aware scheduler.
-- This drives front passes, emits diagnostics via callbacks, and collects
-- back-pass jobs for later execution.
--
-- Total build graph scheduler: this compiles all tasks across all projects,
-- i.e. dependencies of the main project are also compiled here, as well as
-- their dependencies, etc. We construct a large DAG of all modules to be
-- compiled and then compile in topological order using concurrent async
-- workers.
--
-- What we build the graph from:
--   - Input is a list of GlobalTask. Each carries a CompileTask (ActonTask or
--     TyTask), its Paths, and a pre-resolved provider map (gtImportProviders ::
--     Map ModName TaskKey) telling us which in-graph node satisfies a given
--     import. TaskKey is (projectRoot, modName).
--   - We always read an initial set of modules (either all src/ files in the
--     root project for a project build or the user-specified files). From those
--     keys we chase imports via gtImportProviders to pull in exactly the
--     reachable dependencies across projects. __builtin__ is compiled first if
--     present, i.e. we are building the base lib.
--
--   1) Construct a dependency graph over non-builtin tasks and topologically order it.
--   2) Compile __builtin__ first if present.
--   3) Walk modules in topo order, deciding per module whether to compile or reuse cached .tydb,
--      and update the shared environment.
--   4) Track per-name pub/impl hashes and only redo the work that changed:
--      pub changes trigger front passes; impl changes trigger back passes (with
--      an impl-hash refresh); codegen hash mismatches trigger back passes only.
--
-- Key ideas (ActonTask vs TyTask caching):
--   - TyTask is lightweight, read from the .tydb header: moduleSrcBytesHash, modulePubHash,
--     moduleImplHash, imports annotated with the pub hash used, per-name hashes (src/pub/impl
--     + deps), roots, tests, and docstring. It avoids decoding heavy sections.
--   - ParseTask carries source text plus discovered imports; full parsing is
--     deferred until front passes are needed.
--   - ActonTask carries parsed source and must be compiled.
--   - pubMap :: Map TaskKey ByteString and nameMap :: Map TaskKey (Map Name NameHashInfo) are
--     maintained during the run. TyTask compares recorded dependency hashes against current
--     provider hashes (in-graph via pubMap/nameMap, otherwise via cached .tydb headers). Pub deltas
--     trigger front passes; impl deltas trigger back passes with an impl-hash refresh.
--   - When a TyTask is stale for public reasons (or altOutput on the root), we convert it to an
--     ActonTask by parsing the .act file and run front passes; when it is fresh we reuse the
--     header and carry forward the recorded hashes.
--
-- Scheduling:
--   - Critical-path heuristic (file size proxy) picks among ready tasks; runs up to job cap.
--   - Each worker uses an env snapshot; coordinator merges the resulting interface/doc and
--     updates indegrees/ready sets. Dependents receive public hashes via pubMap.
--   - Non-root projects are compiled with depOpts (skip_build/test) while the root keeps user opts.
compileTasks :: Source.SourceProvider
             -> C.GlobalOptions
             -> C.CompileOptions
             -> Paths                     -- root project paths (for alt output root selection)
             -> FilePath                  -- root project path
             -> Data.Set.Set TaskKey       -- root-project modules that can produce requested binaries
             -> Data.Set.Set TaskKey       -- explicitly requested modules
             -> [GlobalTask]
             -> Data.Set.Set TaskKey
             -> CompileCallbacks
             -> IO (Either CompileFailure (Acton.Env.Env0, Bool))
compileTasks sp gopts opts rootPaths rootProj rootTaskKeys requestedTasks tasks dbpBlocked callbacks = do
    runningRef <- newIORef []
    frontOutputRef <- newIORef []
    let cancelRunning = do
          running <- readIORef runningRef
          mapM_ cancel running
          mapM_ (\a -> waitCatch a >> return ()) running
        waitFrontOutputs = do
          _ <- waitFrontOutputJobsRef frontOutputRef
          return ()
        waitFrontOutputsOnExit =
          waitFrontOutputs `catch` ignoreFrontOutputExitFailure
        ignoreFrontOutputExitFailure :: SomeException -> IO ()
        ignoreFrontOutputExitFailure err
          | isJust (fromException err :: Maybe SomeAsyncException) = throwIO err
          | otherwise = return ()
        finishWithFrontOutputs res = do
          outputFailure <- waitFrontOutputJobsRef frontOutputRef
          case res of
            Left err -> return (Left err)
            Right ok ->
              case outputFailure of
                Just err -> return (Left err)
                Nothing -> return (Right ok)
    let compileMain = do
          -- Reject cycles
          if not (null cycles)
            then return $ Left (CompileCycleFailure ("Cyclic imports: " ++ concatMap showTaskGraph cycles))
            else do
              -- Compile __builtin__ first if present anywhere in the graph
              case builtinOrder of
                [t] -> do
                  res <- compileBuiltin frontOutputRef t
                  case res of
                    Left err -> return (Left err)
                    Right () -> continue frontOutputRef runningRef
                _ -> continue frontOutputRef runningRef
    ((compileMain >>= finishWithFrontOutputs) `finally` cancelRunning) `finally` waitFrontOutputsOnExit
  where
    continue frontOutputRef runningRef = do
      baseEnv <- Acton.Env.initEnv builtinPath False

      costMap <- fmap M.fromList $ forM otherOrder $ \t -> do
                    let mn = name (gtTask t)
                        pth = gtPaths t
                        fp  = srcFile pth mn
                    ok <- doesFileExist fp
                    sz <- if ok then getFileSize fp else return 0
                    return (gtKey t, sz)
      let cwMap = computeCriticalWeights costMap

      nCaps <- getNumCapabilities
      let maxParallel = max 1 (if C.jobs gopts > 0 then C.jobs gopts else nCaps)

      loop frontOutputRef runningRef stageInitialReady [] M.empty M.empty M.empty M.empty M.empty stageIndeg stagePending0 baseEnv False maxParallel cwMap

    -- Basic maps/sets ----------------------------------------------------
    taskMap = M.fromList [ (gtKey t, t) | t <- tasks ]
    isDbpBlocked k = Data.Set.member k dbpBlocked
    isBuiltinKey k = tkMod k == A.modName ["__builtin__"]
    builtinOrder = [ t | t <- tasks, isBuiltinKey (gtKey t) ]
    nonBuiltinTasks = [ t | t <- tasks, not (isBuiltinKey (gtKey t)) ]
    nonBuiltinKeys = Data.Set.fromList [ gtKey t | t <- nonBuiltinTasks ]
    depsOf t = nub [ d | d <- M.elems (gtImportProviders t), Data.Set.member d nonBuiltinKeys ]
    nodes    = [ (t, gtKey t, depsOf t) | t <- nonBuiltinTasks ]
    sccs     = stronglyConnComp nodes
    cycles   = [ ts | ts@(CyclicSCC _) <- sccs ]
    order    = [ t | AcyclicSCC t <- sccs ]
    showTaskGraph (CyclicSCC ts) = "\n" ++ concatMap fmt ts
    showTaskGraph _              = ""
    fmt t = tkProj (gtKey t) ++ ":" ++ modNameToString (name (gtTask t)) ++ " "

    otherOrder = order
    revMap :: M.Map TaskKey [TaskKey]
    revMap = foldl' (\acc (k, ds) -> foldl' (\a d -> M.insertWith (++) d [k] a) acc ds) M.empty (M.toList depMap)
    depMap :: M.Map TaskKey [TaskKey]
    depMap = M.fromList [ (gtKey t, depsOf t) | t <- order ]
    indeg  = M.map length depMap

    depOpts = opts { C.skip_build = True, C.test = False }
    optsFor k = if tkProj k == rootProj then opts else depOpts

    formatTaskKey k = modNameToString (dropProjPrefix rootPaths (tkMod k))

    needsParseStage t =
      case gtTask t of
        ParseTask{} -> not (C.only_build (optsFor (gtKey t)))
        _ -> False

    stageDepsOf t =
      let k = gtKey t
          parseDeps = [ParseStage k | needsParseStage t]
          frontDeps = map FrontStage (depsOf t)
      in (parseDeps ++ frontDeps)

    stageDepMap :: M.Map StageKey [StageKey]
    stageDepMap =
      M.fromList $
        [ (ParseStage (gtKey t), [])
        | t <- order
        , needsParseStage t
        ] ++
        [ (FrontStage (gtKey t), stageDepsOf t)
        | t <- order
        ]

    stageRevMap :: M.Map StageKey [StageKey]
    stageRevMap =
      foldl'
        (\acc (k, ds) -> foldl' (\a d -> M.insertWith (++) d [k] a) acc ds)
        M.empty
        (M.toList stageDepMap)

    stageIndeg :: M.Map StageKey Int
    stageIndeg = M.map length stageDepMap

    stageInitialReady :: [StageKey]
    stageInitialReady = [ k | (k, d) <- M.toList stageIndeg, d == 0 ]

    stagePending0 :: Data.Set.Set StageKey
    stagePending0 = Data.Set.fromList (M.keys stageIndeg)

    rootAlt = modName rootPaths
    rootAltKey = TaskKey rootProj rootAlt

    rootCandidates :: [(TaskKey,A.Name)]
    rootCandidates
      | C.test opts =
          [ (key,A.name "test_main") | key <- Data.Set.toAscList rootTaskKeys ]
      | not (null $ C.root opts) = [explicitRootCandidate]
      | otherwise =
          [ (key,A.name "main") | key <- Data.Set.toAscList rootTaskKeys ]

    explicitRootCandidate =
      let parts = A.modPath (moduleStringToName $ C.root opts)
          moduleParts = init parts
          project = projName rootPaths
          canonicalModule
            | null moduleParts = rootAlt
            | project `elem` special_projects = A.modName moduleParts
            | otherwise = A.modName (project : moduleParts)
      in (TaskKey rootProj canonicalModule,A.name $ last parts)

    builtinPath =
      case builtinOrder of
        (t:_) -> projTypes (gtPaths t)
        _     -> sysTypes rootPaths

    computeCriticalWeights :: M.Map TaskKey Integer -> M.Map TaskKey Integer
    computeCriticalWeights cm = cwMap
      where
        costOf m = M.findWithDefault 0 m cm
        depsOfK m = M.findWithDefault [] m revMap
        cwMap    = M.fromList [ (m, costOf m + max0 [ weight d | d <- depsOfK m ]) | m <- M.keys indeg ]
        weight m = M.findWithDefault 0 m cwMap
        max0 []  = 0
        max0 xs  = maximum xs

    dependentClosure :: TaskKey -> Data.Set.Set TaskKey
    dependentClosure k = go Data.Set.empty [k]
      where
        go seen [] = seen
        go seen (x:xs) =
          let ds = M.findWithDefault [] x revMap
              new = filter (`Data.Set.notMember` seen) ds
              seen' = foldl' (flip Data.Set.insert) seen new
          in go seen' (new ++ xs)

    flushDeferredBacks :: M.Map TaskKey DeferredBackJob
                       -> IO (Either CompileFailure ())
    flushDeferredBacks deferredBacks | M.null deferredBacks = do
      ccOnReachability callbacks Data.Set.empty SelectiveWorklist.emptySelection
      return (Right ())
    flushDeferredBacks deferredBacks = do
      prepared <- (try prepareAll :: IO (Either SomeException [Maybe BackJob]))
      case prepared of
        Left err
          | isJust (fromException err :: Maybe SomeAsyncException) -> throwIO err
          | otherwise -> return (Left (CompileInternalFailure (displayException err)))
        Right jobs -> do
          mapM_ (ccOnBackJob callbacks) (catMaybes jobs)
          return (Right ())
      where
        deferred = M.toAscList deferredBacks
        taskInterfaces = M.fromListWith (++)
          [ (tkMod key, [tyDbPath (gtPaths task) (tkMod key)])
          | task <- tasks
          , let key = gtKey task
          ]
        searchPaths = nub (concatMap (searchPath . gtPaths) tasks)

        resolveInterfaces mn =
          case M.findWithDefault [] mn taskInterfaces of
            [] -> maybe [] (:[]) <$> Acton.Env.findTyFile searchPaths mn
            paths -> return (nub paths)

        prepareAll = do
          let byModule = M.fromListWith (++)
                [ (dbjMod dbj, [key]) | (key,dbj) <- deferred ]
              ambiguous = [ (mn,keys) | (mn,keys) <- M.toAscList byModule, length keys > 1 ]
          unless (null ambiguous) $
            throwIO $ ProjectError
              ("Selective back-pass module identity is ambiguous: " ++ show ambiguous)

          let deferredKeys = Data.Set.fromList (map fst deferred)
              selectableModules = Data.Set.fromList
                [ dbjMod dbj | (_,dbj) <- deferred ]
              wholeKeys0 = M.keysSet taskMap `Data.Set.difference` deferredKeys
              wholeModules0 = Data.Set.fromList
                [ tkMod key | key <- Data.Set.toAscList wholeKeys0 ]
              snapshotModules = selectableModules `Data.Set.union` wholeModules0
              rootModules =
                [ (tkMod key,root)
                | (key,root) <- rootCandidates
                , M.member key taskMap
                ]
          snapshots <- SelectiveBack.captureInterfaceClosure
            resolveInterfaces snapshotModules
          let rootlessModules = SelectiveBack.snapshotRootlessModules snapshots
              noBackKeys
                | altOutput opts = Data.Set.empty
                | otherwise = Data.Set.filter
                    (\key -> Data.Set.notMember key dbpBlocked &&
                             Data.Set.member key requestedTasks &&
                             Data.Set.member (tkMod key) rootlessModules)
                    wholeKeys0
              wholeKeys = wholeKeys0 `Data.Set.difference` noBackKeys
              wholeModules = Data.Set.fromList
                [ tkMod key | key <- Data.Set.toAscList wholeKeys ]
              notImplementedModules =
                SelectiveBack.snapshotNotImplementedModules snapshots
              nativeRoots = Data.Set.filter
                (\key -> Data.Set.member (tkMod key) notImplementedModules)
                wholeKeys
              nativeForcedKeys = providerClosure nativeRoots
                `Data.Set.intersection` deferredKeys
              (wholeDeferred,selectedDeferred) = partition
                (\(key,_) -> Data.Set.member key nativeForcedKeys)
                deferred
              forcedWholeModules = Data.Set.fromList
                [ dbjMod dbj | (_,dbj) <- wholeDeferred ]
              interestWholeModules = wholeModules
                `Data.Set.union` forcedWholeModules
          rootSeeds <- SelectiveBack.snapshotRootSeeds snapshots rootModules
          wholeSeeds <- SelectiveBack.snapshotWholeModuleSeeds
            resolveInterfaces snapshots interestWholeModules
          selectedPreparation <- prepareSelected snapshots selectedDeferred
            (rootSeeds ++ wholeSeeds)
          case selectedPreparation of
            Left () -> do
              ccOnReachability callbacks
                (interestWholeModules `Data.Set.union` selectableModules)
                SelectiveWorklist.emptySelection
              wholeJobs <- prepareWholeModules snapshots deferred
              attachBatchValidation snapshots wholeJobs
            Right (selectedSnapshots,selection,selectiveJobs) -> do
              ccOnReachability callbacks interestWholeModules selection
              wholeJobs <- prepareWholeModules snapshots wholeDeferred
              attachBatchValidation
                (fromMaybe snapshots selectedSnapshots)
                (wholeJobs ++ selectiveJobs)

        prepareSelected _ [] _ =
          return (Right (Nothing,SelectiveWorklist.emptySelection,[]))
        prepareSelected _ selectedDeferred [] = do
          mapM_ (ccOnBackSkipped callbacks . fst) selectedDeferred
          return (Right (Nothing,SelectiveWorklist.emptySelection,[]))
        prepareSelected snapshots selectedDeferred seeds = do
          let selectableModules = Data.Set.fromList
                [ dbjMod dbj | (_,dbj) <- selectedDeferred ]
          selectedResult <- SelectiveBack.selectFromSnapshots
            resolveInterfaces snapshots selectableModules seeds
          case selectedResult of
            Left SelectiveWorklist.DynamicSerializationRequiresWhole ->
              return (Left ())
            Left err -> throwIO $ ProjectError
              ("Selective back-pass reachability failed: " ++ show err)
            Right selectedProgram -> Right <$> prepareProjection selectedProgram
          where
            prepareProjection selectedProgram0 = do
              (selectedProgram,opaqueHashes) <-
                SelectiveBack.captureSelectedOpaqueHashes resolveInterfaces selectedProgram0
              let activeSnapshots = SelectiveBack.selectedProgramSnapshots selectedProgram
                  selection = SelectiveBack.selectedProgramSelection selectedProgram
                  selectedModules = Data.Set.fromList
                    [ mn
                    | SelectiveWorklist.TopKey mn _ <-
                        Data.Set.toAscList
                          ( SelectiveWorklist.selectedTops selection
                            `Data.Set.union` SelectiveWorklist.selectedDeclarations selection
                          )
                    ]
                  projectionModules = selectedModules `Data.Set.union`
                    Data.Set.fromList [ dbjMod dbj | (_,dbj) <- selectedDeferred ]
              projections <- SelectiveBack.materializeInterfaceProjections
                resolveInterfaces (Data.Set.toAscList projectionModules) selectedProgram
              let projectionMap = M.fromList
                    [ (A.modname $ SelectiveBack.projectionModule projection,projection)
                    | projection <- projections
                    ]
              keyedProjections <- forM selectedDeferred $ \(key,dbj) ->
                case M.lookup (dbjMod dbj) projectionMap of
                  Nothing -> throwIO $ ProjectError
                    ("Missing selective projection for " ++ modNameToString (dbjMod dbj))
                  Just projection -> return (key,dbj,projection)

              let universeHash = SelectiveBack.bindInterfaceSnapshots
                    (SelectiveBack.projectionUniverseHash selection projections opaqueHashes)
                    activeSnapshots
              snapshotEnv <- SelectiveBack.snapshotEnvironment
                resolveInterfaces activeSnapshots
              let selectiveEnv = SelectiveBack.restrictEnvironmentPublicNames
                    selection snapshotEnv
                  projectedEnv = foldl' installProjection selectiveEnv projections
                  installProjection env projection =
                    let mn = A.modname $ SelectiveBack.projectionModule projection
                        info = SelectiveBack.projectionModuleInfo env projection
                    in Acton.Env.addModuleInfo mn info env

              jobs <- forM keyedProjections $ \(key,dbj,projection) -> do
                let paths = dbjPaths dbj
                    mn = dbjMod dbj
                    typed = SelectiveBack.projectionModule projection
                    codegenHash = SelectiveBack.projectionCodegenHash universeHash mn
                status <- codegenStatus paths mn codegenHash
                if codegenUpToDate status
                  then do
                    logSelection dbj projection "generated code up to date"
                    ccOnBackSkipped callbacks key
                    return Nothing
                  else do
                    importedEnv <- Acton.Env.mkEnv (searchPath paths) projectedEnv typed
                    let declarations = SelectiveBack.projectionDeclarations projection
                        env = Acton.Env.defineClosed declarations importedEnv
                    logSelection dbj projection "generated code out of date"
                    return $ Just BackJob
                      { bjPaths = paths
                      , bjOpts = dbjOpts dbj
                      , bjInput = BackInput
                          { biTypeEnv = Converter.convEnvProtos env
                          , biTypedMod = typed
                          , biDeclarations = map fst declarations
                          , biSrc = Nothing
                          , biCodegenHash = codegenHash
                          }
                      , bjValidate = return ()
                      }
              return (Just activeSnapshots,selection,jobs)

        attachBatchValidation snapshots jobs = do
          let validate = SelectiveBack.validateInterfaceSnapshots
                resolveInterfaces snapshots
              count = length (catMaybes jobs)
          validate
          if count == 0
            then return jobs
            else do
              remaining <- newMVar count
              let complete = modifyMVar_ remaining $ \pending ->
                    if pending == 1
                      then validate >> return 0
                      else return (pending - 1)
                  attach job = job{ bjValidate = complete }
              return (map (fmap attach) jobs)

        providerClosure initial = go (Data.Set.toAscList initial) initial
          where
            go [] seen = seen
            go (key:pending) seen =
              let new = filter (`Data.Set.notMember` seen) $
                    M.findWithDefault [] key depMap
                  seen' = foldl' (flip Data.Set.insert) seen new
              in go (pending ++ new) seen'

        prepareWholeModules _ [] = return []
        prepareWholeModules snapshots wholeDeferred = do
          snapshotEnv <- SelectiveBack.snapshotEnvironment resolveInterfaces snapshots
          mapM (prepareWhole snapshots snapshotEnv) wholeDeferred

        prepareWhole snapshots snapshotEnv (key,dbj) = do
          let paths = dbjPaths dbj
              mn = dbjMod dbj
          implHash <- SelectiveBack.snapshotImplementationHash snapshots mn
          sourceHash <- SelectiveBack.snapshotSourceHash snapshots mn
          let wholeHash = Hashing.wholeCodegenHash
                (not $ C.dbg_no_lines $ dbjOpts dbj) implHash sourceHash
              codegenHash = SelectiveBack.bindInterfaceSnapshots wholeHash snapshots
          status <- codegenStatus paths mn codegenHash
          if codegenUpToDate status
            then do
              logWhole dbj "generated code up to date"
              ccOnBackSkipped callbacks key
              return Nothing
            else do
              (typed,storedImplHash) <- SelectiveBack.materializeWholeModule
                resolveInterfaces snapshots mn
              unless (storedImplHash == implHash) $
                throwIO $ ProjectError $
                  "Interface changed while preparing full back pass for " ++
                  modNameToString mn
              env <- Acton.Env.mkEnv (searchPath paths) snapshotEnv typed
              snap <- readMatchingSource sp mn sourceHash (srcFile paths mn)
              logWhole dbj "generated code out of date"
              return $ Just BackJob
                { bjPaths = paths
                , bjOpts = dbjOpts dbj
                , bjInput = BackInput
                    { biTypeEnv = Converter.convEnvProtos env
                    , biTypedMod = typed
                    , biDeclarations = []
                    , biSrc = Just (Source.ssText snap)
                    , biCodegenHash = codegenHash
                    }
                , bjValidate = return ()
                }

        logSelection dbj projection reason =
          when (C.verbose gopts) $
            ccOnInfo callbacks $
              "  Selective back " ++
              modNameToString (dropProjPrefix (dbjPaths dbj) (dbjMod dbj)) ++
              ": " ++ show (SelectiveBack.projectionTopCount projection) ++ " tops, " ++
              show (SelectiveBack.projectionMemberCount projection) ++ " members, " ++ reason

        logWhole dbj reason =
          when (C.verbose gopts) $
            ccOnInfo callbacks $
              "  Full back " ++
              modNameToString (dropProjPrefix (dbjPaths dbj) (dbjMod dbj)) ++
              ": full-module closure, " ++ reason


    -- TODO: can we reintegrate this into the normal loop to avoid duplication?
    -- NOTE: FYI, it was originally part of the main loop but factored out for
    -- clarity when we changed to use async, front/back jobs etc. There were so
    -- many other changes so at the time it was easier to separate builtin
    -- compilation but perhaps we can find a way to merge it back in to one
    -- general loop.
    compileBuiltin :: IORef [FrontOutputJob] -> GlobalTask -> IO (Either CompileFailure ())
    compileBuiltin frontOutputRef t = do
      let bPaths = gtPaths t
          mn = name (gtTask t)
          optsBuiltin = optsFor (gtKey t)
          actFile = srcFile bPaths mn
          forceAlt = altOutput optsBuiltin && gtKey t == rootAltKey
      if C.only_build optsBuiltin
        then return (Right ())
        else do
          t' <- case gtTask t of
            TyTask{} | forceAlt -> materializeTask optsBuiltin sp mn actFile M.empty Nothing (gtTask t)
            ParseTask{} -> materializeTask optsBuiltin sp mn actFile M.empty Nothing (gtTask t)
            _ -> return (gtTask t)
          case t' of
            ParseErrorTask{ parseDiagnostics = diags } -> do
              ccOnDiagnostics callbacks t optsBuiltin diags
              return (Left CompileBuiltinFailure)
            TyTask{} -> return (Right ())
            ParseTask{} -> error ("Internal error: unmaterialized ParseTask " ++ modNameToString mn)
            ActonTask{ src = srcContent, srcBytes = srcBytes, sourceMeta = mSourceMeta, atree = m } -> do
              ccOnFrontStart callbacks t optsBuiltin
              builtinEnv0 <- Acton.Env.initEnv (projTypes bPaths) True
              res <- runFrontPasses
                gopts
                optsBuiltin
                False
                False
                bPaths
                builtinEnv0
                m
                srcContent
                srcBytes
                mSourceMeta
                (getPubHashCached bPaths)
                (getImplHashCached bPaths)
                (getNameHashCached bPaths)
                (\p -> ccOnFrontProgress callbacks t optsBuiltin p)
                (ccOnFrontOutputStart callbacks)
                (ccOnFrontOutputProgress callbacks)
                (ccOnFrontOutputDone callbacks)
                (ccShouldWriteFrontOutput callbacks)
                (rememberFrontOutputJobList frontOutputRef)
              case res of
                Left diags -> do
                  ccOnFrontDone callbacks t optsBuiltin
                  ccOnDiagnostics callbacks t optsBuiltin diags
                  return (Left CompileBuiltinFailure)
                Right fr -> do
                  outputFailure <- waitFrontOutputJobsRef frontOutputRef
                  case outputFailure of
                    Just err -> return (Left err)
                    Nothing -> do
                      ccOnFrontDone callbacks t optsBuiltin
                      ccOnFrontResult callbacks t optsBuiltin fr
                      updatePubHashCache mn (frPubHash fr)
                      updateImplHashCache mn (frImplHash fr)
                      updateNameHashCache mn (frNameHashes fr)
                      forM_ (frBackJob fr) $ ccOnBackJob callbacks
                      return (Right ())

    -- One module ---------------------------------------------------------
    doOne :: Acton.Env.Env0
          -> IORef [FrontOutputJob]
          -> M.Map TaskKey B.ByteString
          -> M.Map TaskKey B.ByteString
          -> M.Map TaskKey (M.Map A.Name InterfaceFiles.NameHashInfo)
          -> M.Map TaskKey CompileTask
          -> TaskKey
          -> IO (TaskKey, Either [Diagnostic String] FrontResult)
    doOne envSnap frontOutputRef pubMap implMap nameMap parsedTasks key = do
      t <- case M.lookup key taskMap of
             Just x -> return x
             Nothing -> error ("Internal error: missing task for key " ++ show key)
      let taskCurrent = M.findWithDefault (gtTask t) key parsedTasks
          paths = gtPaths t
          mn    = name (gtTask t)
          optsT = optsFor key
          providers = gtImportProviders t
          actFile = srcFile paths mn
          tyFile = tyDbPath paths mn
          short8 bs   = take 8 (B.unpack $ Base16.encode bs)
          mkFrontResult imps ifaceTE mdoc pubHash implHash nameHashes backJob deferredBackJob =
            FrontResult
              { frIfaceTE = ifaceTE
              , frImps = imps
              , frDoc = mdoc
              , frModuleInfo = Nothing
              , frPubHash = pubHash
              , frImplHash = implHash
              , frNameHashes = nameHashes
              , frFrontTime = Nothing
              , frFrontTiming = Nothing
              , frInferredSigs = []
              , frBackJob = backJob
              , frDeferredBackJob = deferredBackJob
              , frOutputJobs = []
              }
          emptyFrontResult =
            FrontResult
              { frIfaceTE = []
              , frImps = []
              , frDoc = Nothing
              , frModuleInfo = Nothing
              , frPubHash = B.empty
              , frImplHash = B.empty
              , frNameHashes = []
              , frFrontTime = Nothing
              , frFrontTiming = Nothing
              , frInferredSigs = []
              , frBackJob = Nothing
              , frDeferredBackJob = Nothing
              , frOutputJobs = []
              }
          addCachedTyOutputJob fr
            | not (C.tydb optsT) = return fr
            | not (null (frOutputJobs fr)) = return fr
            | B.null (frPubHash fr) = return fr
            | otherwise = mask_ $ do
                job <- startFrontOutputJob (ccOnFrontOutputStart callbacks)
                                           (ccOnFrontOutputDone callbacks)
                                           (ccShouldWriteFrontOutput callbacks)
                                           (TaskKey (projPath paths) mn)
                                           FrontOutputTydbCopy $
                  copyTydbInterface optsT paths mn
                rememberFrontOutputJobList frontOutputRef [job]
                return fr{ frOutputJobs = [job] }
          cacheFrontResult fr0 = do
            fr <- addCachedTyOutputJob fr0
            updatePubHashCache mn (frPubHash fr)
            updateImplHashCache mn (frImplHash fr)
            updateNameHashCache mn (frNameHashes fr)
            return (key, Right fr)
          readTyFile = do
            tyRes <- InterfaceFiles.readFileMaybe tyFile
            case tyRes of
              Nothing -> return (Left (missingIfaceDiagnostics mn "" mn))
              Just ty -> return (Right ty)
          mkBackJob env1 tmod srcText codegenHash =
            BackJob
              { bjPaths = paths
              , bjOpts = optsT
              , bjInput = BackInput
                  { biTypeEnv = Converter.convEnvProtos env1
                  , biTypedMod = tmod
                  , biDeclarations = []
                  , biSrc = Just srcText
                  , biCodegenHash = codegenHash
                  }
              , bjValidate = return ()
              }

          resolveImportHash m =
            case M.lookup m providers of
              Just depKey ->
                case M.lookup depKey pubMap of
                  Just h  -> return (Just h)
                  Nothing
                    | isBuiltinKey depKey -> getPubHashCached paths m
                    | M.member depKey taskMap -> error ("Internal error: missing pub hash for dep " ++ modNameToString m)
                    | otherwise -> getPubHashCached paths m
              Nothing -> getPubHashCached paths m
          resolveImportImplHash m =
            case M.lookup m providers of
              Just depKey ->
                case M.lookup depKey implMap of
                  Just h  -> return (Just h)
                  Nothing
                    | isBuiltinKey depKey -> getImplHashCached paths m
                    | M.member depKey taskMap -> error ("Internal error: missing impl hash for dep " ++ modNameToString m)
                    | otherwise -> getImplHashCached paths m
              Nothing -> getImplHashCached paths m
          resolveNameHash' m n =
            case M.lookup m providers of
              Just depKey ->
                case M.lookup depKey nameMap of
                  Just hm ->
                    case M.lookup n hm of
                      Just info -> return (Just info)
                      Nothing
                        | providerIsCachedTy depKey -> getNameHashCached paths m n
                        | otherwise -> return Nothing
                  Nothing
                    | isBuiltinKey depKey -> getNameHashCached paths m n
                    | M.member depKey taskMap -> error ("Internal error: missing name hashes for dep " ++ modNameToString m)
                    | otherwise -> getNameHashCached paths m n
              Nothing -> getNameHashCached paths m n
          providerIsCachedTy depKey =
            case M.lookup depKey taskMap of
              Just GlobalTask{ gtTask = TyTask{} } -> True
              _ -> False

          missingNameHashDiagnostics qn =
            errsToDiagnostics "Compilation error" (modNameToFilename (dropProjPrefix paths mn)) ""
              [(NoLoc, "Hash info missing for " ++ prstr qn)]

          missingDepHashDiagnostics label qn users =
            errsToDiagnostics "Compilation error" (modNameToFilename (dropProjPrefix paths mn)) ""
              [(NoLoc, label ++ " hash missing for " ++ prstr qn ++ users)]

          checkImportHashes imps = do
            userSearchAbs <- mapM normalizePathSafe (C.searchpath optsT)
            systemTypesAbs <- mapM normalizePathSafe (systemTypePaths (sysPath paths) (sysTypes paths))
            searchAbs <- mapM normalizePathSafe (searchPath paths)
            let userSearchSet = Data.Set.fromList (map normalise userSearchAbs)
                systemTypeSet = Data.Set.fromList (map normalise systemTypesAbs)
                managedTypeDirs =
                  [ p
                  | p <- searchAbs
                  , let pNorm = normalise p
                  , Data.Set.notMember pNorm systemTypeSet
                  , Data.Set.notMember pNorm userSearchSet
                  ]
                isUnder dir path =
                  let dir' = addTrailingPathSeparator (normalise dir)
                      path' = normalise path
                  in Data.List.isPrefixOf dir' path'
                isManagedTyPath path = any (\dir -> isUnder dir path) managedTypeDirs
            (missing, changed) <- foldM
              (\(missingAcc, changedAcc) (depMn, depHash) -> do
                 mh <- resolveImportHash depMn
                 case mh of
                   Just currentHash
                     | currentHash == depHash -> return (missingAcc, changedAcc)
                     | otherwise -> return (missingAcc, Data.Set.insert depMn changedAcc)
                   Nothing ->
                     if M.member depMn providers
                       then return (missingAcc, Data.Set.insert depMn changedAcc)
                       else do
                         mTy <- getTyFileCached (searchPath paths) depMn
                         case mTy of
                           Nothing -> return (Data.Set.insert depMn missingAcc, changedAcc)
                           Just tyPath -> do
                             tyAbs <- normalizePathSafe tyPath
                             if isManagedTyPath tyAbs
                               then return (Data.Set.insert depMn missingAcc, changedAcc)
                               else return (missingAcc, Data.Set.insert depMn changedAcc)
              )
              (Data.Set.empty, Data.Set.empty)
              imps
            if Data.Set.null missing
              then return (Right changed)
              else do
                let missingSorted = Data.List.sortOn modNameToString (Data.Set.toList missing)
                    diags = concatMap (\depMn -> missingIfaceDiagnostics mn "" depMn) missingSorted
                return (Left diags)

          collectDiags results =
            let (errs, vals) = partitionEithers results
            in if null errs
                 then Right vals
                 else Left (concat errs)
          traverseDiags f items = collectDiags <$> mapM f items

          fmtUsers users qn =
            case M.lookup qn users of
              Nothing -> ""
              Just ns ->
                let uniq = Data.Set.toList (Data.Set.fromList ns)
                    names = map A.nstr (Data.List.sortOn Hashing.nameKey uniq)
                in if null names
                     then ""
                     else " (used by " ++ intercalate ", " names ++ ")"

          depModulesFromHashes moduleHashInfo nameHashes = do
            let depMods =
                  Data.List.sort $
                  Data.Set.toList $
                  Data.Set.fromList
                    [ depMn
                    | (qn, _) <-
                        InterfaceFiles.mhPubDeps moduleHashInfo ++
                        InterfaceFiles.mhImplDeps moduleHashInfo ++
                        concatMap (\nh -> InterfaceFiles.nhPubDeps nh ++ InterfaceFiles.nhImplDeps nh) nameHashes
                    , depMn <- case qn of
                        A.GName m _ -> [m]
                        A.QName m _ -> [m]
                        A.NoQ{} -> []
                    ]
            resolved <- traverseDiags resolveDepModule depMods
            return resolved
            where
              resolveDepModule depMn = do
                mpub <- resolveImportHash depMn
                mimpl <- resolveImportImplHash depMn
                return $ case (mpub, mimpl) of
                  (Just pubH, Just implH) ->
                    Right InterfaceFiles.DepModuleInfo
                      { InterfaceFiles.dmiModule = depMn
                      , InterfaceFiles.dmiPubHash = pubH
                      , InterfaceFiles.dmiImplHash = implH
                      }
                  _ -> Left (missingIfaceDiagnostics mn "" depMn)

          nameHashSummary prevMap newInfos =
            let newMap = nameHashMapFromList newInfos
                oldKeys = Data.Set.fromList (M.keys prevMap)
                newKeys = Data.Set.fromList (M.keys newMap)
                added = Data.Set.toList (Data.Set.difference newKeys oldKeys)
                removed = Data.Set.toList (Data.Set.difference oldKeys newKeys)
                shared = Data.Set.toList (Data.Set.intersection oldKeys newKeys)
                fieldChange label getHash old new
                  | getHash old == getHash new = Nothing
                  | otherwise = Just (label ++ " " ++ short8 (getHash old) ++ " -> " ++ short8 (getHash new))
                changesFor n =
                  let oldInfo = prevMap M.! n
                      newInfo = newMap M.! n
                      fields = catMaybes
                        [ fieldChange "src" InterfaceFiles.nhSrcHash oldInfo newInfo
                        , fieldChange "pub" InterfaceFiles.nhPubHash oldInfo newInfo
                        , fieldChange "impl" InterfaceFiles.nhImplHash oldInfo newInfo
                        ]
                  in if null fields
                       then Nothing
                       else Just ("~" ++ A.nstr n ++ "{" ++ intercalate ", " fields ++ "}")
                addedItems = [ "+" ++ A.nstr n | n <- Data.List.sortOn Hashing.nameKey added ]
                removedItems = [ "-" ++ A.nstr n | n <- Data.List.sortOn Hashing.nameKey removed ]
                changedItems = mapMaybe changesFor (Data.List.sortOn Hashing.nameKey shared)
                items = addedItems ++ removedItems ++ changedItems
            in if null items then Nothing else Just (intercalate ", " items)

          resolveNameHashInfo m n = do
            mInfo <- resolveNameHash' m n
            return $ case mInfo of
              Just info -> Right info
              Nothing -> Left (missingNameHashDiagnostics (A.GName m n))

          resolveQNameHash label getHash users qn =
            case qn of
              A.GName m n -> resolveNameHashInfo m n >>= \res ->
                return $ case res of
                  Left diags -> Left diags
                  Right info ->
                    let h = getHash info
                    in if B.null h
                         then Left (missingDepHashDiagnostics label (A.GName m n) users)
                         else Right h
              A.QName m n -> resolveNameHashInfo m n >>= \res ->
                return $ case res of
                  Left diags -> Left diags
                  Right info ->
                    let h = getHash info
                    in if B.null h
                         then Left (missingDepHashDiagnostics label (A.GName m n) users)
                         else Right h
              A.NoQ _ -> return (Left (missingNameHashDiagnostics qn))

          resolveDepHashes label getHash deps = do
            resolved <- traverseDiags (\(n, qns) -> do
              let users = " (used by " ++ A.nstr n ++ ")"
              resolvedQns <- traverseDiags (\qn -> do
                currE <- resolveQNameHash label getHash users qn
                return (fmap (\curr -> (qn, curr)) currE)) qns
              return (fmap (\vals -> (n, vals)) resolvedQns)) (M.toList deps)
            return (fmap M.fromList resolved)

          resolveModuleDepHashes label getHash qns =
            traverseDiags (\qn -> do
              currE <- resolveQNameHash label getHash " (used by module initialization)" qn
              return (fmap (\curr -> (qn, curr)) currE)) qns

          checkDepModuleRows depModules = do
            resolved <- traverseDiags checkOne depModules
            return $ fmap foldRows resolved
            where
              checkOne depInfo = do
                let depMn = InterfaceFiles.dmiModule depInfo
                mpub <- resolveImportHash depMn
                mimpl <- resolveImportImplHash depMn
                return $ case (mpub, mimpl) of
                  (Just pubH, Just implH) ->
                    Right ( depMn
                          , pubH /= InterfaceFiles.dmiPubHash depInfo
                          , implH /= InterfaceFiles.dmiImplHash depInfo
                          )
                  _ ->
                    -- Stale dep rows may mention a removed transitive provider;
                    -- the per-name rows decide whether any used name is affected.
                    Right (depMn, True, True)

              foldRows rows =
                ( Data.Set.fromList [ depMn | (depMn, pubChanged, _implChanged) <- rows, pubChanged ]
                , Data.Set.fromList [ depMn | (depMn, _pubChanged, implChanged) <- rows, implChanged ]
                )

          checkDepNameRows changedPubMods changedImplMods = do
            let changedMods =
                  Data.List.sortOn modNameToString $
                  Data.Set.toList (Data.Set.union changedPubMods changedImplMods)
            foldM checkModule ([], [], [], [], M.empty, M.empty, Data.Set.empty) changedMods
            where
              checkModule acc depMn = do
                depNames <- InterfaceFiles.readDepNames tyFile depMn
                if null depNames
                  then return (noteMissingRows depMn acc)
                  else foldM (checkName depMn) acc depNames

              noteMissingRows depMn acc@(pubDeltas, implDeltas, pubMissing, implMissing, pubUsers, implUsers, rowMissingMods)
                | Data.Set.member depMn changedPubMods || Data.Set.member depMn changedImplMods =
                    (pubDeltas, implDeltas, pubMissing, implMissing, pubUsers, implUsers, Data.Set.insert depMn rowMissingMods)
                | otherwise = acc

              checkName depMn acc depInfo = do
                let depName = InterfaceFiles.dniName depInfo
                    qn = A.GName depMn depName
                    current getHash info =
                      let h = getHash info
                      in if B.null h then Nothing else Just h
                    checkPub = Data.Set.member depMn changedPubMods && not (B.null (InterfaceFiles.dniPubHash depInfo))
                    checkImpl = Data.Set.member depMn changedImplMods && not (B.null (InterfaceFiles.dniImplHash depInfo))
                mInfo <- resolveNameHash' depMn depName
                acc1 <- if checkPub
                          then addPub depMn depName qn (InterfaceFiles.dniPubHash depInfo) (mInfo >>= current InterfaceFiles.nhPubHash) acc
                          else return acc
                if checkImpl
                  then addImpl depMn depName qn (InterfaceFiles.dniImplHash depInfo) (mInfo >>= current InterfaceFiles.nhImplHash) acc1
                  else return acc1

              addPub depMn depName qn old mNew acc@(pubDeltas, implDeltas, pubMissing, implMissing, pubUsers, implUsers, rowMissingMods) =
                case mNew of
                  Just new | new == old -> return acc
                  Just new -> do
                    users <- InterfaceFiles.readDepUsers tyFile depMn depName
                    return ((qn, old, new) : pubDeltas, implDeltas, pubMissing, implMissing, M.insert qn (InterfaceFiles.duPubUsers users) pubUsers, implUsers, rowMissingMods)
                  Nothing -> do
                    users <- InterfaceFiles.readDepUsers tyFile depMn depName
                    return (pubDeltas, implDeltas, qn : pubMissing, implMissing, M.insert qn (InterfaceFiles.duPubUsers users) pubUsers, implUsers, rowMissingMods)

              addImpl depMn depName qn old mNew acc@(pubDeltas, implDeltas, pubMissing, implMissing, pubUsers, implUsers, rowMissingMods) =
                case mNew of
                  Just new | new == old -> return acc
                  Just new -> do
                    users <- InterfaceFiles.readDepUsers tyFile depMn depName
                    return (pubDeltas, (qn, old, new) : implDeltas, pubMissing, implMissing, pubUsers, M.insert qn (InterfaceFiles.duImplUsers users) implUsers, rowMissingMods)
                  Nothing -> do
                    users <- InterfaceFiles.readDepUsers tyFile depMn depName
                    return (pubDeltas, implDeltas, pubMissing, qn : implMissing, pubUsers, M.insert qn (InterfaceFiles.duImplUsers users) implUsers, rowMissingMods)

      case taskCurrent of
        ParseErrorTask{ parseDiagnostics = diags } -> return (key, Left diags)
        _ | C.only_build optsT -> do
              ifaceRes <- case taskCurrent of
                            TyTask{ tyPubHash = h } -> readIfaceFromTy paths mn "" (Just h)
                            ParseTask{ src = srcContent } -> readIfaceFromTy paths mn srcContent Nothing
                            ActonTask{ src = srcContent } -> readIfaceFromTy paths mn srcContent Nothing
              case ifaceRes of
                Right (imps, ifaceTE, mdoc, ih, implH, mModuleInfo) -> do
                  let cachedFullNameHashes = case taskCurrent of
                        TyTask{ tyNameHashes = nhs } -> nhs
                        _ -> []
                      cachedNameHashes = publicNameHashes cachedFullNameHashes
                  let fr = (mkFrontResult imps ifaceTE mdoc ih implH cachedNameHashes Nothing Nothing)
                             { frModuleInfo = mModuleInfo }
                  cacheFrontResult fr
                Left _ ->
                  return (key, Right emptyFrontResult)
        _ -> do
          -- For cached .tydb tasks, compare recorded dep hashes against current deps.
          -- This is the up-to-date check that decides if we can skip work. If
          -- any implDeps have changed we need to rerun our back passes and if
          -- any pubDeps have changed we need to rerun front passes (and back
          -- passes). Unchanged import pub hashes let us skip checking public
          -- name deps from those modules.
          needByDepsRes <- case taskCurrent of
            TyTask{ tyImports = imps, tyDepModules = depModules } -> do
              importHashRes <- checkImportHashes imps
              case importHashRes of
                Left diags -> return (Left diags)
                Right _ -> do
                  depModuleRes <- checkDepModuleRows depModules
                  case depModuleRes of
                    Left diags -> return (Left diags)
                    Right (changedPubMods, changedImplMods) ->
                      if Data.Set.null changedPubMods && Data.Set.null changedImplMods
                        then return (Right ([], [], [], [], M.empty, M.empty, Data.Set.empty))
                        else Right <$> checkDepNameRows changedPubMods changedImplMods
            -- Source tasks always run front passes, so deps are irrelevant.
            _ -> return (Right ([], [], [], [], M.empty, M.empty, Data.Set.empty))

          case needByDepsRes of
            Left diags -> return (key, Left diags)
            Right (pubDeltas, implDeltas, pubMissing, implMissing, pubUsers, implUsers, rowMissingMods) -> do
              let needBySource = case taskCurrent of { ParseTask{} -> True; ActonTask{} -> True; _ -> False }
                  -- Public deltas require front passes; impl deltas only need back jobs.
                  needByPub = not (null pubDeltas)
                  needByMissing = not (null pubMissing) || not (null implMissing) || not (Data.Set.null rowMissingMods)
                  needByImpl = not (null implDeltas)
                  forceAlt    = altOutput optsT && key == rootAltKey
                  forceAlways = C.alwaysbuild optsT
                  -- Front passes run on source or API changes, or when forced.
                  needFront = needBySource || needByPub || needByMissing || forceAlt || forceAlways
                  mCodegenHash = case taskCurrent of
                    TyTask{ tyHash = sourceHash, tyImplHash = implHash } ->
                      Just (Hashing.wholeCodegenHash
                        (not $ C.dbg_no_lines optsT) implHash sourceHash)
                    _ -> Nothing
                  backPassNeeded = case taskCurrent of
                    TyTask{ tyRoots = roots } ->
                      shouldRunBackPass (isDbpBlocked key)
                        (Data.Set.member key requestedTasks) optsT roots
                    _ -> True
              cachedDeferredBackJob <- case taskCurrent of
                    TyTask{} | backPassNeeded -> do
                      hasNotImpl <- InterfaceFiles.readStmtHasNotImpl tyFile
                      return (selectiveDeferredBackJob (isDbpBlocked key) hasNotImpl optsT paths mn)
                    _ -> return Nothing
              let isCachedDbp =
                    case cachedDeferredBackJob of
                      Just _ -> True
                      Nothing -> False
              let canCheckCodegen = backPassNeeded && not needFront && not needByImpl && not (altOutput optsT) && not isCachedDbp
              mCodegenStatus <- case mCodegenHash of
                Just codegenHash | canCheckCodegen -> Just <$> codegenStatus paths mn codegenHash
                _ -> return Nothing
              let needByCodegen = maybe False (not . codegenUpToDate) mCodegenStatus
              let runFront = do
                    --traceM ("\n## runFront " ++ prstr mn ++ "\n")
                    prevNameHashes <- if C.verbose gopts
                      then case taskCurrent of
                        TyTask{ tyNameHashes = nhs } | not (null nhs) -> return (Just (nameHashMapFromList (publicNameHashes nhs)))
                        _ -> getNameHashMapCached paths mn
                      else return Nothing
                    when (C.verbose gopts) $ do
                      if needBySource
                        then ccOnInfo callbacks ("  Stale " ++ modNameToString (dropProjPrefix paths mn) ++ ": source changed")
                        else do
                          when needByPub $ do
                            let fmtDelta (qn, old, new) = prstr qn ++ " " ++ short8 old ++ " → " ++ short8 new ++ fmtUsers pubUsers qn
                            ccOnInfo callbacks ("  Stale " ++ modNameToString (dropProjPrefix paths mn) ++ ": pub changes in " ++ Data.List.intercalate ", " (map fmtDelta pubDeltas))
                          when needByMissing $ do
                            let fmtMissing users qn = prstr qn ++ fmtUsers users qn
                                pubMissingItems =
                                  [ "pub " ++ fmtMissing pubUsers qn
                                  | qn <- Data.List.sortOn Hashing.qnameKey pubMissing
                                  ]
                                implMissingItems =
                                  [ "impl " ++ fmtMissing implUsers qn
                                  | qn <- Data.List.sortOn Hashing.qnameKey implMissing
                                  ]
                                rowMissingItems =
                                  [ "rows " ++ modNameToString depMn
                                  | depMn <- Data.List.sortOn modNameToString (Data.Set.toList rowMissingMods)
                                  ]
                            ccOnInfo callbacks ("  Stale " ++ modNameToString (dropProjPrefix paths mn) ++ ": missing dep hashes in " ++ Data.List.intercalate ", " (pubMissingItems ++ implMissingItems ++ rowMissingItems))
                    t' <- case taskCurrent of
                            ActonTask{} -> return taskCurrent
                            _ -> materializeTask optsT sp mn actFile providers Nothing taskCurrent
                    case t' of
                      ParseErrorTask{ parseDiagnostics = diags } -> return (key, Left diags)
                      ActonTask{ src = srcContent, srcBytes = srcBytes, sourceMeta = mSourceMeta, atree = m } -> do
                        res <- runFrontPasses
                          gopts
                          optsT
                          (isDbpBlocked key)
                          (Data.Set.member key requestedTasks)
                          paths
                          envSnap
                          (adjustImports providers m)
                          srcContent
                          srcBytes
                          mSourceMeta
                          resolveImportHash
                          resolveImportImplHash
                          resolveNameHash'
                          (\p -> ccOnFrontProgress callbacks t optsT p)
                          (ccOnFrontOutputStart callbacks)
                          (ccOnFrontOutputProgress callbacks)
                          (ccOnFrontOutputDone callbacks)
                          (ccShouldWriteFrontOutput callbacks)
                          (rememberFrontOutputJobList frontOutputRef)
                        case res of
                          Left diags -> return (key, Left diags)
                          Right fr -> do
                            when (C.verbose gopts) $
                              forM_ prevNameHashes $ \prevMap ->
                                forM_ (nameHashSummary prevMap (frNameHashes fr)) $ \summary ->
                                  ccOnInfo callbacks ("  Hash deltas " ++ modNameToString (dropProjPrefix paths mn) ++ ": " ++ summary)
                            cacheFrontResult fr
                      ParseTask{} -> error ("Internal error: unmaterialized ParseTask " ++ modNameToString mn)
                      _ -> error ("Internal error: unexpected task " ++ show t')
                  runImplRefresh = do
                    --traceM ("\n## runImplRefresh " ++ prstr mn ++ "\n")
                    let rerunFront = do
                          when (C.verbose gopts) $
                            ccOnInfo callbacks ("  Stale " ++ modNameToString (dropProjPrefix paths mn) ++ ": impl refresh encountered unresolved dep hashes; rerunning front passes")
                          runFront
                        handleSyncFailure :: SomeException -> IO (TaskKey, Either [Diagnostic String] FrontResult)
                        handleSyncFailure err =
                          if isJust (fromException err :: Maybe SomeAsyncException)
                            then throwIO err
                            else rerunFront
                        handleImplRefreshException :: SomeException -> IO (TaskKey, Either [Diagnostic String] FrontResult)
                        handleImplRefreshException = handleSyncFailure
                    (do
                      when (C.verbose gopts) $ do
                        let fmtDelta (qn, old, new) = prstr qn ++ " " ++ short8 old ++ " → " ++ short8 new ++ fmtUsers implUsers qn
                        ccOnInfo callbacks ("  Stale " ++ modNameToString (dropProjPrefix paths mn) ++ ": impl changes in " ++ Data.List.intercalate ", " (map fmtDelta implDeltas))
                      -- The refresh is pure row math: every input is already stored.
                      -- Own impl-hash components (nhOwnImplHash) and local deps come
                      -- from the per-name rows, the external dep NAMES from the dep
                      -- index rows (only their hashes changed -- the module's source
                      -- is unchanged), and the new dep hashes are resolved fresh.
                      -- No re-read of the typed module, no re-parse, no dep re-walk;
                      -- the .tydb is updated surgically. The typed module is read
                      -- only when the refresh must feed an EAGER back job -- then it
                      -- is the compilation input.
                      case taskCurrent of
                        TyTask{ tyHash = expectedSourceHash
                              , tyPubHash = expectedPublicHash
                              , tyImplHash = expectedImplHash
                              } -> do
                          refreshInput <- InterfaceFiles.readImplRefreshInput tyFile
                          unless (InterfaceFiles.iriSourceHash refreshInput == expectedSourceHash
                                  && InterfaceFiles.iriPublicHash refreshInput == expectedPublicHash
                                  && InterfaceFiles.iriImplementationHash refreshInput == expectedImplHash) $
                            throwIO (InterfaceFiles.ImplRefreshStale
                              "Interface changed after dependency freshness was checked")
                          let storedNameHashes = InterfaceFiles.iriNameHashes refreshInput
                              storedModuleHash = InterfaceFiles.iriModuleHashInfo refreshInput
                              moduleHasNotImpl = InterfaceFiles.iriHasNotImpl refreshInput
                              storedPubHash = InterfaceFiles.iriPublicHash refreshInput
                              roots = InterfaceFiles.iriRoots refreshInput
                          -- Only names with an impl item participate in the refresh --
                          -- exactly the names the front pass gave a (non-empty) own
                          -- impl-hash component. A signature-only name's impl hash is
                          -- invariant under dependency changes.
                          let refreshables =
                                [ nh | nh <- storedNameHashes
                                     , not (B.null (InterfaceFiles.nhOwnImplHash nh)) ]
                              ownImplHashes = M.fromList
                                [ (InterfaceFiles.nhName nh, InterfaceFiles.nhOwnImplHash nh) | nh <- refreshables ]
                              implLocalDeps = M.fromList
                                [ (InterfaceFiles.nhName nh, InterfaceFiles.nhImplLocalDeps nh) | nh <- refreshables ]
                              implExtDeps = M.fromList
                                [ (InterfaceFiles.nhName nh, map fst (InterfaceFiles.nhImplDeps nh)) | nh <- refreshables ]
                          do
                              implExtRes <- (try :: IO a -> IO (Either SomeException a)) $ do
                                nameDeps <- resolveDepHashes "impl" InterfaceFiles.nhImplHash implExtDeps
                                moduleDeps <- resolveModuleDepHashes "impl" InterfaceFiles.nhImplHash
                                  (map fst (InterfaceFiles.mhImplDeps storedModuleHash))
                                return (nameDeps, moduleDeps)
                              case implExtRes of
                                Left err -> handleSyncFailure err
                                Right (Left _, _) -> rerunFront
                                Right (_, Left _) -> rerunFront
                                Right (Right implExtHashes, Right moduleImplExtHashes) -> do
                                  let updatedNameHashes =
                                        Hashing.refreshImplHashes storedNameHashes ownImplHashes implLocalDeps implExtHashes
                                      updatedImplHashes = M.fromList
                                        [ (InterfaceFiles.nhName nh, InterfaceFiles.nhImplHash nh)
                                        | nh <- updatedNameHashes
                                        ]
                                      updatedModuleHash = Hashing.finishModuleHash
                                        updatedImplHashes
                                        (InterfaceFiles.mhOwnImplHash storedModuleHash)
                                        (InterfaceFiles.mhStatementOwners storedModuleHash)
                                        (InterfaceFiles.mhImplLocalDeps storedModuleHash)
                                        (InterfaceFiles.mhPubDeps storedModuleHash)
                                        moduleImplExtHashes
                                      moduleImplHash =
                                        Hashing.moduleImplHashFromNameHashes updatedModuleHash updatedNameHashes
                                  depModulesRes <- depModulesFromHashes updatedModuleHash updatedNameHashes
                                  case depModulesRes of
                                    Left _ -> rerunFront
                                    Right updatedDepModules -> do
                                      evaluate (rnf (moduleImplHash, updatedModuleHash, updatedDepModules, updatedNameHashes))
                                      let outputKey = TaskKey (projPath paths) mn
                                          -- Synchronous, like the full write: completion implies
                                          -- the refreshed rows are committed. A write failure is
                                          -- THIS module's front failure on the diagnostics path --
                                          -- returned, not thrown, so handleImplRefreshException
                                          -- cannot misread it as stale dep hashes and rerun the
                                          -- front against a persistent environmental error.
                                          writeRefreshedRows = do
                                            res <- try (runFrontOutputSync (ccOnFrontOutputStart callbacks)
                                                                           (ccOnFrontOutputDone callbacks)
                                                                           (ccShouldWriteFrontOutput callbacks)
                                                                           outputKey
                                                                           FrontOutputTydb $
                                                          InterfaceFiles.updateImplRefresh tyFile refreshInput
                                                            InterfaceFiles.ImplRefreshOutput
                                                              { InterfaceFiles.iroImplementationHash = moduleImplHash
                                                              , InterfaceFiles.iroModuleHashInfo = updatedModuleHash
                                                              , InterfaceFiles.iroDependencies = updatedDepModules
                                                              , InterfaceFiles.iroNameHashes = updatedNameHashes
                                                              })
                                            case res of
                                              Left err | isJust (fromException err :: Maybe SomeAsyncException) -> throwIO err
                                                       | InterfaceFiles.isImplRefreshStale err -> throwIO err
                                                       | otherwise -> return (Just (tydbWriteDiagnostics mn err))
                                              Right () -> return Nothing
                                          startOutputJobs =
                                            if C.tydb optsT
                                              then (:[]) <$> startFrontOutputJob (ccOnFrontOutputStart callbacks)
                                                                                 (ccOnFrontOutputDone callbacks)
                                                                                 (ccShouldWriteFrontOutput callbacks)
                                                                                 outputKey
                                                                                 FrontOutputTydbCopy
                                                                                 (copyTydbInterface optsT paths mn)
                                              else return []
                                          backPassNeeded0 = shouldRunBackPass (isDbpBlocked key)
                                            (Data.Set.member key requestedTasks) optsT roots
                                          deferredBackJob0
                                            | backPassNeeded0 = selectiveDeferredBackJob
                                                (isDbpBlocked key) moduleHasNotImpl optsT paths mn
                                            | otherwise = Nothing
                                      if not backPassNeeded0 || isJust deferredBackJob0
                                        then do
                                          ifaceRes <- readIfaceFromTy paths mn "" (Just storedPubHash)
                                          case ifaceRes of
                                            Left diags -> return (key, Left diags)
                                            Right (imps, ifaceTE, mdocI, storedPubH, _storedImplH, mModuleInfo) -> do
                                              writeFailure <- writeRefreshedRows
                                              case writeFailure of
                                                Just diags -> return (key, Left diags)
                                                Nothing -> mask_ $ do
                                                  outputJobs <- startOutputJobs
                                                  rememberFrontOutputJobList frontOutputRef outputJobs
                                                  let deferredBackJob = deferredBackJob0
                                                      fr = (mkFrontResult imps ifaceTE mdocI storedPubH moduleImplHash (publicNameHashes updatedNameHashes) Nothing deferredBackJob)
                                                        { frOutputJobs = outputJobs
                                                        , frModuleInfo = mModuleInfo
                                                        }
                                                  cacheFrontResult fr
                                        else do
                                          tyRes <- readTyFile
                                          case tyRes of
                                            Left diags -> return (key, Left diags)
                                            Right (_ms, nmod, tmod, _sourceMeta, moduleSrcBytesHash, modulePubHash, _moduleImplHash, _imps, _depModules, _nameHashes, _roots, _tests, mdoc) -> do
                                              snap <- readMatchingSource
                                                sp mn moduleSrcBytesHash actFile
                                              envRes <- (try :: IO Acton.Env.Env0 -> IO (Either SomeException Acton.Env.Env0)) $
                                                Acton.Env.mkEnv (searchPath paths) envSnap tmod
                                              case envRes of
                                                Left err -> handleSyncFailure err
                                                Right env1 -> do
                                                  evaluate (rnf nmod)
                                                  evaluate (rnf tmod)
                                                  writeFailure <- writeRefreshedRows
                                                  case writeFailure of
                                                    Just diags -> return (key, Left diags)
                                                    Nothing -> mask_ $ do
                                                      outputJobs <- startOutputJobs
                                                      rememberFrontOutputJobList frontOutputRef outputJobs
                                                      let I.NModule imps ifaceFull _mdoc = nmod
                                                          ifaceTE = publicIfaceTE ifaceFull
                                                          codegenHash = Hashing.wholeCodegenHash
                                                            (not $ C.dbg_no_lines optsT)
                                                            moduleImplHash moduleSrcBytesHash
                                                          backJob = Just (mkBackJob env1 tmod (Source.ssText snap) codegenHash)
                                                          fr = (mkFrontResult imps ifaceTE mdoc modulePubHash moduleImplHash (publicNameHashes updatedNameHashes) backJob Nothing)
                                                            { frOutputJobs = outputJobs }
                                                      cacheFrontResult fr
                        _ -> rerunFront
                      ) `catch` handleImplRefreshException
                  runCodegenRefresh = do
                    --traceM ("\n## runCodegenRefresh " ++ prstr mn ++ "\n")
                    when (C.verbose gopts) $ do
                      let suffix = maybe "" formatCodegenDelta mCodegenStatus
                      ccOnInfo callbacks ("  Stale " ++ modNameToString (dropProjPrefix paths mn) ++ ": generated code out of date" ++ suffix)
                    tyRes <- readTyFile
                    case tyRes of
                      Left diags -> return (key, Left diags)
                      Right (_ms, nmod, tmod, _sourceMeta, moduleSrcBytesHash, modulePubHash, moduleImplHashStored, _imps, _depModules, nameHashes, roots, _tests, mdoc) -> do
                        snap <- readMatchingSource
                          sp mn moduleSrcBytesHash actFile
                        env1 <- Acton.Env.mkEnv (searchPath paths) envSnap tmod
                        let I.NModule imps ifaceFull _mdoc = nmod
                            ifaceTE = publicIfaceTE ifaceFull
                            backPassNeeded0 = shouldRunBackPass (isDbpBlocked key)
                              (Data.Set.member key requestedTasks) optsT roots
                            deferredBackJob
                              | backPassNeeded0 = selectiveDeferredBackJob
                                  (isDbpBlocked key) (A.hasNotImpl (A.mbody tmod)) optsT paths mn
                              | otherwise = Nothing
                            backJob =
                              case deferredBackJob of
                                Just _ -> Nothing
                                Nothing | backPassNeeded0 -> Just (mkBackJob env1 tmod (Source.ssText snap)
                                  (Hashing.wholeCodegenHash
                                    (not $ C.dbg_no_lines optsT)
                                    moduleImplHashStored moduleSrcBytesHash))
                                Nothing -> Nothing
                            fr = mkFrontResult imps ifaceTE mdoc modulePubHash moduleImplHashStored (publicNameHashes nameHashes) backJob deferredBackJob
                        cacheFrontResult fr
                  runReuse = do
                    --traceM ("\n## runReuse " ++ prstr mn ++ "\n")
                    when (C.verbose gopts) $
                      ccOnInfo callbacks ("  Fresh " ++ modNameToString (dropProjPrefix paths mn) ++ ": using cached .tydb")
                    ifaceRes <- case taskCurrent of
                                  TyTask{ tyPubHash = h } -> readIfaceFromTy paths mn "" (Just h)
                                  _ -> readIfaceFromTy paths mn "" Nothing
                    case ifaceRes of
                      Left diags -> return (key, Left diags)
                      Right (imps, ifaceTE, mdoc, ih, implH, mModuleInfo) -> do
                        let cachedFullNameHashes = case taskCurrent of
                              TyTask{ tyNameHashes = nhs } -> nhs
                              _ -> []
                            cachedNameHashes = publicNameHashes cachedFullNameHashes
                        let fr = (mkFrontResult imps ifaceTE mdoc ih implH cachedNameHashes Nothing cachedDeferredBackJob)
                                   { frModuleInfo = mModuleInfo }
                        cacheFrontResult fr
              case () of
                _ | needFront -> runFront
                _ | needByImpl -> runImplRefresh
                _ | needByCodegen -> runCodegenRefresh
                _ -> runReuse

    stageTaskKey :: StageKey -> TaskKey
    stageTaskKey sk =
      case sk of
        ParseStage k -> k
        FrontStage k -> k

    stagePriority :: M.Map TaskKey Integer -> StageKey -> (Int, Integer)
    stagePriority cw sk =
      let phase = case sk of
                    FrontStage _ -> 1
                    ParseStage _ -> 0
      in (phase, M.findWithDefault 0 (stageTaskKey sk) cw)

    runParseStage :: TaskKey -> IO (StageKey, Either [Diagnostic String] StageSuccess)
    runParseStage key = do
      t <- case M.lookup key taskMap of
             Just x -> return x
             Nothing -> error ("Internal error: missing task for key " ++ show key)
      let optsT = optsFor key
          mn = name (gtTask t)
          actFile = srcFile (gtPaths t) mn
          onProgress p = ccOnParseProgress callbacks t optsT p
          providers = gtImportProviders t
      timeStart <- getTime Monotonic
      parsed <- if C.only_build optsT
                  then return (gtTask t)
                  else case gtTask t of
                         ParseTask{} -> materializeTask optsT sp mn actFile providers (Just onProgress) (gtTask t)
                         _ -> return (gtTask t)
      case parsed of
        ParseTask{} -> return (ParseStage key, Right (StageParsed parsed Nothing))
        ActonTask{ atree = m } -> do
          _ <- evaluate (rnf m)
          timeEnd <- getTime Monotonic
          let parseTime = if not (quiet gopts optsT) then Just (timeEnd - timeStart) else Nothing
          return (ParseStage key, Right (StageParsed parsed parseTime))
        ParseErrorTask{} -> return (ParseStage key, Right (StageParsed parsed Nothing))
        _ -> error ("Internal error: parse stage did not materialize " ++ modNameToString mn)

    runFrontStage :: Acton.Env.Env0
                  -> IORef [FrontOutputJob]
                  -> M.Map TaskKey B.ByteString
                  -> M.Map TaskKey B.ByteString
                  -> M.Map TaskKey (M.Map A.Name InterfaceFiles.NameHashInfo)
                  -> M.Map TaskKey CompileTask
                  -> TaskKey
                  -> IO (StageKey, Either [Diagnostic String] StageSuccess)
    runFrontStage envSnap frontOutputRef res implRes nameRes parsedTasks key = do
      (doneKey, outcome) <- doOne envSnap frontOutputRef res implRes nameRes parsedTasks key
      return (FrontStage doneKey, fmap StageFronted outcome)

    scheduleMore :: Int -> [StageKey]
                 -> [(Async (StageKey, Either [Diagnostic String] StageSuccess), StageKey)]
                 -> IORef [FrontOutputJob]
                 -> M.Map TaskKey B.ByteString
                 -> M.Map TaskKey B.ByteString
                 -> M.Map TaskKey (M.Map A.Name InterfaceFiles.NameHashInfo)
                 -> M.Map TaskKey CompileTask
                 -> Acton.Env.Env0
                 -> M.Map TaskKey Integer
                 -> IO ([StageKey]
                       , [(Async (StageKey, Either [Diagnostic String] StageSuccess), StageKey)])
    scheduleMore k rdy running frontOutputRef res implRes nameRes parsedTasks envSnap cw = do
      let rdySorted = Data.List.sortOn (Down . stagePriority cw) rdy
          (toStart, rdy') = splitAt k rdySorted
      new <- forM toStart $ \sk -> do
                case sk of
                  FrontStage key ->
                    case M.lookup key taskMap of
                      Just t -> ccOnFrontStart callbacks t (optsFor key)
                      Nothing -> return ()
                  ParseStage key ->
                    case M.lookup key taskMap of
                      Just t -> ccOnParseStart callbacks t (optsFor key)
                      Nothing -> return ()
                a <- async $
                  case sk of
                    ParseStage key -> runParseStage key
                    FrontStage key -> runFrontStage envSnap frontOutputRef res implRes nameRes parsedTasks key
                return (a, sk)
      return (rdy', new ++ running)

    loop :: IORef [FrontOutputJob]
         -> IORef [Async (StageKey, Either [Diagnostic String] StageSuccess)]
         -> [StageKey]
         -> [(Async (StageKey, Either [Diagnostic String] StageSuccess), StageKey)]
         -> M.Map TaskKey B.ByteString
         -> M.Map TaskKey B.ByteString
         -> M.Map TaskKey (M.Map A.Name InterfaceFiles.NameHashInfo)
         -> M.Map TaskKey CompileTask
         -> M.Map TaskKey DeferredBackJob
         -> M.Map StageKey Int
         -> Data.Set.Set StageKey
         -> Acton.Env.Env0
         -> Bool
         -> Int
         -> M.Map TaskKey Integer
         -> IO (Either CompileFailure (Acton.Env.Env0, Bool))
    loop frontOutputRef runningRef rdy running res implRes nameRes parsedTasks deferredBacks ind pend envAcc hadErrors maxPar cw = do
      (rdy1, running1) <- mask_ $ do
        res@(rdy1', running1') <- scheduleMore (maxPar - length running) rdy running frontOutputRef res implRes nameRes parsedTasks envAcc cw
        writeIORef runningRef (map fst running1')
        return res
      if null running1 && null rdy1
        then if Data.Set.null pend
                 then if hadErrors
                   then return (Right (envAcc, True))
                   else do
                     flushRes <- flushDeferredBacks deferredBacks
                     case flushRes of
                       Left err -> return (Left err)
                       Right () -> do
                         writeIORef runningRef []
                         return (Right (envAcc, False))
                 else return (Right (envAcc, True))
        else do
          (doneA, (stageDone, outcome)) <- waitAny $ map fst running1
          let running2 = filter ((/= doneA) . fst) running1
          writeIORef runningRef (map fst running2)
          let keyDone = stageTaskKey stageDone
              tDone = taskMap M.! keyDone
              optsDone = optsFor keyDone
          case outcome of
            Left diags -> do
              case stageDone of
                FrontStage _ -> ccOnFrontDone callbacks tDone optsDone
                ParseStage _ -> ccOnParseDone callbacks tDone optsDone Nothing
              ccOnDiagnostics callbacks tDone optsDone diags
              let blockedMods = dependentClosure keyDone
                  blockedStages =
                    Data.Set.fromList $
                      concatMap (\m -> [ParseStage m, FrontStage m]) (Data.Set.toList blockedMods)
                  (blockedRunning, running3) =
                    partition (\(_, sk) -> Data.Set.member sk blockedStages) running2
              mapM_ (cancel . fst) blockedRunning
              writeIORef runningRef (map fst running3)
              let pend2 = Data.Set.delete stageDone (pend `Data.Set.difference` blockedStages)
                  rdy2 = filter (`Data.Set.notMember` blockedStages) rdy1
                  dropKeys = keyDone : Data.Set.toList blockedMods
                  parsedTasks2 = foldl' (flip M.delete) parsedTasks dropKeys
              loop frontOutputRef runningRef rdy2 running3 res implRes nameRes parsedTasks2 deferredBacks ind pend2 envAcc True maxPar cw
            Right success -> do
              let pend2 = Data.Set.delete stageDone pend
                  ind2  = case M.lookup stageDone stageRevMap of
                            Nothing -> ind
                            Just ds -> foldl' (\m d -> M.adjust (\x -> x-1) d m) ind ds
                  runningKeys = map snd running2
                  newlyReady = [ sk | sk <- Data.Set.toList pend2
                                    , M.findWithDefault 0 sk ind2 == 0
                                    , not (sk `elem` rdy1)
                                    , not (sk `elem` runningKeys)
                                    ]
                  rdy2 = rdy1 ++ newlyReady
              case success of
                StageParsed parsed mParseTime -> do
                  ccOnParseDone callbacks tDone optsDone mParseTime
                  let parsedTasks2 = M.insert keyDone parsed parsedTasks
                  loop frontOutputRef runningRef rdy2 running2 res implRes nameRes parsedTasks2 deferredBacks ind2 pend2 envAcc hadErrors maxPar cw
                StageFronted fr -> do
                  ccOnFrontDone callbacks tDone optsDone
                  ccOnFrontResult callbacks tDone optsDone fr
                  forM_ (frBackJob fr) $ ccOnBackJob callbacks
                  let res2  = M.insert keyDone (frPubHash fr) res
                      implRes2 = M.insert keyDone (frImplHash fr) implRes
                      nameRes2 = M.insert keyDone (nameHashMapFromList (frNameHashes fr)) nameRes
                      parsedTasks2 = M.delete keyDone parsedTasks
                      envAcc' = Acton.Env.addModuleInfo (tkMod keyDone) (frontResultModuleInfo (tkMod keyDone) fr) envAcc
                      deferredBacks1 =
                        case frDeferredBackJob fr of
                          Nothing -> deferredBacks
                          Just dbj -> M.insert keyDone dbj deferredBacks
                  loop frontOutputRef runningRef rdy2 running2 res2 implRes2 nameRes2 parsedTasks2 deferredBacks1 ind2 pend2 envAcc' hadErrors maxPar cw


-- | Execute back-pass jobs in parallel while keeping output order stable.
-- Jobs are started up to the concurrency limit, then completions are buffered
-- and flushed in job order so logs are deterministic.
runBackJobs :: C.GlobalOptions -> Int -> (BackJob -> IO ()) -> (BackJob -> Maybe TimeSpec -> IO ()) -> [BackJob] -> IO ()
runBackJobs _ _ _ _ [] = return ()
runBackJobs gopts maxPar onStart onDone jobs = do
  runningRef <- newIORef []
  let cancelRunning = readIORef runningRef >>= mapM_ cancel
  let runMain = loopBack runningRef indexed [] M.empty 0
  runMain `finally` cancelRunning
  where
    indexed = zip [0..] (orderBackJobs jobs)

    backJobKey :: BackJob -> TaskKey
    backJobKey job =
      TaskKey (projPath (bjPaths job)) (A.modname (biTypedMod (bjInput job)))

    orderBackJobs :: [BackJob] -> [BackJob]
    orderBackJobs js = Data.List.sortOn backJobKey js

    loopBack runningRef pending running results nextIx = do
      let capacity = maxPar - length running
          (toStart, pending') = splitAt capacity pending
      running' <- mask_ $ do
        new <- forM toStart $ \(ix, job) ->
                 async $ do
                   onStart job
                   (res, _timing) <- runBackPasses
                     gopts (bjOpts job) (bjPaths job) (bjInput job)
                     (\action -> action >> return True) (\_ -> return ())
                   return (ix, job, res)
        let running' = running ++ new
        writeIORef runningRef running'
        return running'
      if null running' && null pending'
        then do
          (_, _) <- flushReady results nextIx
          return ()
        else do
          (doneA, (ix, job, res)) <- waitAny running'
          let running'' = filter (/= doneA) running'
              results' = M.insert ix (job, res) results
          writeIORef runningRef running''
          (results'', nextIx') <- flushReady results' nextIx
          loopBack runningRef pending' running'' results'' nextIx'

    flushReady :: M.Map Int (BackJob, Maybe TimeSpec) -> Int -> IO (M.Map Int (BackJob, Maybe TimeSpec), Int)
    flushReady res ix =
      case M.lookup ix res of
        Nothing -> return (res, ix)
        Just (job, mline) -> do
          onDone job mline
          flushReady (M.delete ix res) (ix + 1)


-- Paths handling -------------------------------------------------------------------------------------

data Paths      = Paths {
                    searchPath  :: [FilePath],
                    sysPath     :: FilePath,
                    sysTypes    :: FilePath,
                    projPath    :: FilePath,
                    projOut     :: FilePath,
                    projTypes   :: FilePath,
                    binDir      :: FilePath,
                    srcDir      :: FilePath,
                    isTmp       :: Bool,
                    fileExt     :: String,
                    modName     :: A.ModName,
                    projName    :: String
                  }

-- Per-project context used for multi-project orchestration.
-- Identified by projRoot (absolute path). Keeps directories and BuildSpec if present.
data ProjCtx = ProjCtx {
                     projRoot    :: FilePath,
                     projOutDir  :: FilePath,
                     projTypesDir:: FilePath,
                     projSrcDir  :: FilePath,
                     projSysPath :: FilePath,
                     projSysTypes:: FilePath,
                     projBuildSpec :: BuildSpec.BuildSpec,
                     projDeps     :: [(String, FilePath)]          -- resolved dependency roots (abs paths)
                   } deriving (Show)

systemTypePaths :: FilePath -> FilePath -> [FilePath]
systemTypePaths sys types =
    [joinPath [sys, "std", "out", "types"], types]

type FingerprintMap = M.Map String FilePath

scratchBuildSpec :: FilePath -> BuildSpec.BuildSpec
scratchBuildSpec projRoot =
    let name = "acton_scratch"
        prefix = Fingerprint.fingerprintPrefixForName name
        suffix = Fingerprint.fingerprintPrefixForName projRoot
        fp = Fingerprint.formatFingerprint (Fingerprint.updateFingerprintPrefix prefix (fromIntegral suffix))
    in BuildSpec.BuildSpec
         { BuildSpec.specName = name
         , BuildSpec.specDescription = Nothing
         , BuildSpec.fingerprint = fp
         , BuildSpec.dependencies = M.empty
         , BuildSpec.zig_dependencies = M.empty
         , BuildSpec.libraries = M.empty
         }


normalizeFingerprintKey :: String -> Maybe String
normalizeFingerprintKey raw =
    Fingerprint.formatFingerprint <$> Fingerprint.parseFingerprint raw

fingerprintKeyFromSpec :: BuildSpec.BuildSpec -> String
fingerprintKeyFromSpec spec =
    case normalizeFingerprintKey (BuildSpec.fingerprint spec) of
      Just fp -> fp
      Nothing -> BuildSpec.fingerprint spec

applyFingerprint :: FilePath -> BuildSpec.BuildSpec -> FingerprintMap -> (FilePath, FingerprintMap, String)
applyFingerprint path spec fpMap =
    let fp = fingerprintKeyFromSpec spec
    in case M.lookup fp fpMap of
         Just canonical -> (canonical, fpMap, fp)
         Nothing -> (path, M.insert fp path fpMap, fp)

-- | Discover all projects reachable from a root project.
-- Follows Build.act dependencies, applies overrides/pins, and
-- returns a map from project root to ProjCtx while skipping duplicates.
discoverProjects :: C.GlobalOptions -> FilePath -> FilePath -> [(String, FilePath)] -> IO (M.Map FilePath ProjCtx)
discoverProjects gopts sysAbs rootProj depOverrides = do
    rootAbs <- normalizePathSafe rootProj
    rootSpec0 <- loadBuildSpec rootAbs
    rootSpec  <- applyDepOverrides rootAbs depOverrides rootSpec0
    let rootPins = BuildSpec.dependencies rootSpec
        (_, fpMap0, _) = applyFingerprint rootAbs rootSpec M.empty
    fst <$> go rootAbs Data.Set.empty M.empty fpMap0 rootPins rootAbs (Just rootSpec)
  where
    go root seen acc fpMap pins dir mSpec = do
      dirAbs <- normalizePathSafe dir
      if Data.Set.member dirAbs seen
        then return (acc, fpMap)
        else do
          spec0 <- case mSpec of
                     Just s -> return s
                     Nothing -> loadBuildSpec dirAbs
          spec <- applyDepOverrides dirAbs depOverrides spec0
          let (_, fpMap1, _) = applyFingerprint dirAbs spec fpMap
          (deps, fpMap2) <- do
            let depsList = M.toList (BuildSpec.dependencies spec)
            foldM (collectDep pins dirAbs) ([], fpMap1) depsList
          let outDir   = joinPath [dirAbs, "out"]
              typesDir = joinPath [outDir, "types"]
              srcDir'  = joinPath [dirAbs, "src"]
              ctx = ProjCtx { projRoot = dirAbs
                            , projOutDir = outDir
                            , projTypesDir = typesDir
                            , projSrcDir = srcDir'
                            , projSysPath = sysAbs
                            , projSysTypes = joinPath [sysAbs, "base", "out", "types"]
                            , projBuildSpec = spec
                            , projDeps = [ (n, p) | (n, p, _) <- reverse deps ]
                            }
              acc' = M.insert dirAbs ctx acc
              seen' = Data.Set.insert dirAbs seen
          foldM (step root seen' pins) (acc', fpMap2) (reverse deps)

    collectDep pins base (accDeps, fpMap) (depName, dep) = do
      let (chosenDep, conflict) =
            case M.lookup depName pins of
              Nothing -> (dep, Nothing)
              Just pinDep ->
                if pinDep == dep
                  then (dep, Nothing)
                  else (pinDep, Just dep)
      when (isJust conflict) $
        unless (C.quiet gopts) $
          putStrLn ("Warning: dependency '" ++ depName ++ "' in " ++ base
                    ++ " overridden by root pin")
      depBase <- resolveDepBase base depName chosenDep
      depAbs  <- normalizePathSafe depBase
      (depPath, fpMap', depSpec) <- canonicalizeDep depAbs fpMap
      return ((depName, depPath, depSpec) : accDeps, fpMap')

    canonicalizeDep depAbs fpMap = do
      spec0 <- loadBuildSpec depAbs
      spec <- applyDepOverrides depAbs depOverrides spec0
      let (canonPath, fpMap', fp) = applyFingerprint depAbs spec fpMap
      when (canonPath /= depAbs) $
        unless (C.quiet gopts) $
          putStrLn ("Warning: dependency fingerprint " ++ fp
                    ++ " at " ++ depAbs ++ " deduplicated to " ++ canonPath)
      let depSpec = if canonPath == depAbs then Just spec else Nothing
      return (canonPath, fpMap', depSpec)

    step root seen pins (acc, fpMap) (_, depBase, mSpec) =
      go root seen acc fpMap pins depBase mSpec

-- Given a FILE and optionally --syspath PATH:
-- 'sysPath' is the path to the system directory as given by PATH, defaulting to the acton executable directory.
-- 'sysTypes' is directory "types" under 'sysPath'.
-- 'projPath' is the closest parent directory of FILE that contains a Build.act
-- file, or a temporary directory in "/tmp" if no such parent exists.
-- 'projOut' is directory "out" under 'projPath'.
-- 'projTypes' is directory "types" under 'projOut'.
-- 'binDir' is the directory prefix of FILE if 'projPath' is temporary, otherwise it is directory "bin" under 'projOut'
-- 'srcDir' is the directory prefix of FILE if 'projPath' is temporary, otherwise it is directory "src" under 'projPath'
-- 'fileExt' is file suffix of FILE.
-- 'modName' is the module name of FILE (its path after 'src' except 'fileExt', split at every '/')

-- | Compute the source file path for a module under its project src dir.
srcFile                 :: Paths -> A.ModName -> FilePath
srcFile paths mn        = srcBase paths mn ++ ".act"

-- | Compute the output base path (without extension) for a module.
-- Used to locate .tydb/.c/.h output under the project's types directory.
outBase                 :: Paths -> A.ModName -> FilePath
outBase paths mn        = joinPath (projTypes paths : A.modPath mn)

tyDbPath                :: Paths -> A.ModName -> FilePath
tyDbPath paths mn       = outBase paths mn ++ InterfaceFiles.interfaceExt

-- | Compute the module path without extension under the project's src dir.
-- Used to derive the .act path or related per-module files.
srcBase                 :: Paths -> A.ModName -> FilePath
srcBase paths mn        = --trace ("## srcBase " ++ prstr mn ++ " --> " ++ fpath) $
                          fpath
  where fpath           = joinPath (srcDir paths : names)
        names           = case A.modPath mn of
                            n:ns | n == projName paths -> ns
                            ns -> ns


-- | Walk upward from a path to find a project root.
-- A project root is identified by Build.act plus a
-- src/ directory; returns Nothing if we reach filesystem root.

-- | Check whether a directory is an Acton project root.
-- Requires Build.act and a src/ directory.
isActonProjectRoot :: FilePath -> IO Bool
isActonProjectRoot path = do
    runacton <- lookupEnv "ACTON_RUNACTON"
    if isJust runacton
      then return False
      else do
        hasBuildAct <- doesFileExist (path </> "Build.act")
        hasSrcDir <- doesDirectoryExist (path </> "src")
        return (hasBuildAct && hasSrcDir)

findProjectDir :: FilePath -> IO (Maybe FilePath)
findProjectDir path = do
    isProjectRoot <- isActonProjectRoot path
    if isProjectRoot
        then return (Just path)
        else if path == takeDirectory path  -- Check if we're at root
            then return Nothing
            else findProjectDir (takeDirectory path)


-- | Opaque handle for the lifetime lock of a long-running project compiler.
data BackgroundCompilerLock = BackgroundCompilerLock FileLock FilePath

-- | Path to the lock held for the lifetime of a long-running project compiler.
--
-- This lock only establishes unique ownership of a background compiler
-- process such as LSP or watch mode. It does not imply that project state
-- is current, so all compile/build work must still take 'withProjectLock'.
backgroundCompilerLockPath :: FilePath -> FilePath
backgroundCompilerLockPath projDir =
    joinPath [projDir, ".acton.compile.lock"]

-- | Acquire the long-running compiler lock for a project root, if available.
tryBackgroundCompilerLock :: FilePath -> IO (Maybe BackgroundCompilerLock)
tryBackgroundCompilerLock projDir = do
    let lockPath = backgroundCompilerLockPath projDir
    mlock <- tryLockFile lockPath Exclusive
    return ((\lock -> BackgroundCompilerLock lock lockPath) <$> mlock)

-- | Release the long-running compiler lock and remove its lock file if possible.
releaseBackgroundCompilerLock :: BackgroundCompilerLock -> IO ()
releaseBackgroundCompilerLock (BackgroundCompilerLock lock lockPath) = do
    unlockFile lock
    mlock <- tryLockFile lockPath Exclusive
    case mlock of
      Nothing -> return ()
      Just lock2 -> do
        removeFile lockPath `catch` handleNotExists
        unlockFile lock2
  where
    handleNotExists :: IOException -> IO ()
    handleNotExists _ = return ()

-- | Path to the single per-project compiler work lock.
projectLockPath :: FilePath -> FilePath
projectLockPath projDir =
    joinPath [projDir, ".acton.lock"]

-- | Run an action while holding the single per-project compiler work lock.
--
-- If the lock is not immediately available, run the callback once before
-- blocking until the lock can be acquired.
withProjectLockOnWait :: FilePath -> IO () -> IO a -> IO a
withProjectLockOnWait projDir onWait action = do
    let lockPath = projectLockPath projDir
    mlock <- tryLockFile lockPath Exclusive
    case mlock of
      Just lock -> action `finally` unlockFile lock
      Nothing -> do
        onWait
        withFileLock lockPath Exclusive (\_ -> action)

-- | Run an action while holding the single per-project compiler work lock.
withProjectLock :: FilePath -> IO a -> IO a
withProjectLock projDir action =
    withProjectLockOnWait projDir (return ()) action


-- | Compute Paths for a given source file and compile options.
-- Resolves the project root (or temp root), output dirs, and search path,
-- creating required directories along the way.
findPaths               :: FilePath -> C.CompileOptions -> IO Paths
findPaths actFile opts  = do execDir <- takeDirectory <$> getExecutablePath
                             sysPath <- canonicalizePath (if null $ C.syspath opts then execDir ++ "/.." else C.syspath opts)
                             absSrcFile <- canonicalizePath actFile
                             (isTmp, projPath, dirInSrc) <- analyze (takeDirectory absSrcFile) []
                             let sysTypes = joinPath [sysPath, "base", "out", "types"]
                                 srcDir  = if isTmp then takeDirectory absSrcFile else joinPath [projPath, "src"]
                                 projOut = joinPath [projPath, "out"]
                                 projTypes = joinPath [projOut, "types"]
                                 binDir  = if isTmp then srcDir else joinPath [projOut, "bin"]
                                 modName = A.modName $ dirInSrc ++ [fileBody]
                             -- join the search paths from command line options with the ones found in the deps directory
                             depOverrides <- normalizeDepOverrides projPath (C.dep_overrides opts)
                             (projName,depTypePaths) <- if isTmp then return ("",[]) else collectDepTypePaths projPath depOverrides
                             let sPaths = [projTypes] ++ depTypePaths ++ (C.searchpath opts) ++ systemTypePaths sysPath sysTypes
                                 modName = A.modName $ (if isTmp then dirInSrc else projName:dirInSrc) ++ [fileBody]
                             InterfaceFiles.registerSystemTypeRoots (systemTypePaths sysPath sysTypes)
                             createDirectoryIfMissing True binDir
                             createDirectoryIfMissing True projOut
                             createDirectoryIfMissing True projTypes
                             createDirectoryIfMissing True (getModPath projTypes modName)
                             return $ Paths sPaths sysPath sysTypes projPath projOut projTypes binDir srcDir isTmp fileExt modName projName
  where (fileBody,fileExt) = splitExtension $ takeFileName actFile

        analyze "/" ds  = do tmp <- canonicalizePath (C.tempdir opts)
                             return (True, tmp, [])
        analyze pre ds  = do isProjectRoot <- isActonProjectRoot pre
                             if isProjectRoot
                                then case ds of
                                    [] -> return $ (False, pre, [])
                                    "src":dirs -> return $ (False, pre, dirs)
                                    "out":"types":dirs -> return $ (False, pre, dirs)
                                    _ -> throwProjectError ("Source file is not in a valid project directory: " ++ joinPath ds)
                                else analyze (takeDirectory pre) (takeFileName pre : ds)

-- Module helpers for multi-project builds ---------------------------------------------------------

-- | Derive a module name from a file path under a project's src root.
-- The result uses path segments and strips the .act extension.
moduleNameFromFile :: FilePath -> String -> FilePath -> IO A.ModName
moduleNameFromFile srcBase proj actFile = do
    base <- normalizePathSafe srcBase
    file <- normalizePathSafe actFile
    let rel = dropExtension (makeRelative base file)
        names = splitDirectories rel
        mn = A.modName $ if proj `elem` special_projects then names else proj:names
    --traceM ("## from file " ++ actFile ++ " --> " ++ prstr mn)
    return mn

-- | Enumerate all .act files in a project and pair them with module names.
-- Used to seed the project module index for graph construction.
enumerateProjectModules :: ProjCtx -> IO [(FilePath, A.ModName)]
enumerateProjectModules ctx = do
    exists <- doesDirectoryExist (projSrcDir ctx)
    if not exists
      then return []
      else do
        files <- getFilesRecursive (projSrcDir ctx)
        let actFiles = filter (\f -> takeExtension f == ".act") files
            proj = BuildSpec.specName $ projBuildSpec ctx
            rootName = head . splitDirectories . makeRelative (projSrcDir ctx) . dropExtension
            depNames = map fst $ projDeps ctx
            depOverlaps = filter ((`elem` depNames) . rootName) actFiles

        when (not $ null depOverlaps) $
            throwProjectError ("Source files in project " ++ projRoot ctx ++ " overlap with declared dependencies:\n" ++
                               concat ["    " ++ makeRelative (projRoot ctx) f ++ "\n" | f <- depOverlaps])

        forM actFiles $ \f -> do
          mn <- moduleNameFromFile (projSrcDir ctx) proj f
          return (f, mn)

-- | Remove stale generated module artifacts when source modules disappear.
-- Prunes orphan .tydb/.c/.h outputs under out/types before task planning so
-- cached headers cannot mask deleted .act modules.
pruneMissingModuleOutputs :: ProjCtx -> IO ()
pruneMissingModuleOutputs ctx = do
    let srcRoot = projSrcDir ctx
        typesRoot = projTypesDir ctx
    typesExists <- doesDirectoryExist typesRoot
    when typesExists $ do
      srcExists <- doesDirectoryExist srcRoot
      srcMods <- if srcExists
                   then do
                     srcFiles <- getFilesRecursive srcRoot
                     let actFiles = filter (\f -> takeExtension f == ".act") srcFiles
                         modBases = map (normalise . dropExtension . makeRelative srcRoot) actFiles
                     return (Data.Set.fromList modBases)
                   else return Data.Set.empty
      outFiles <- getFilesRecursive typesRoot
      mapM_ (pruneFile srcMods typesRoot) outFiles
      tyDbs <- InterfaceFiles.listInterfaceDirsRecursive typesRoot
      mapM_ (pruneTyDb srcMods typesRoot) tyDbs
  where
    isRootStub rel ext =
      ext == ".c" &&
      (".root" `Data.List.isSuffixOf` dropExtension rel ||
       ".test_root" `Data.List.isSuffixOf` dropExtension rel)

    moduleBase rel ext
      | isRootStub rel ext = dropExtension (dropExtension rel)
      | otherwise = dropExtension rel

    pruneFile srcMods typesRoot absFile = do
      let ext = takeExtension absFile
      when (ext == ".c" || ext == ".h") $ do
        let rel = normalise (makeRelative typesRoot absFile)
            base = dropProjDir (moduleBase rel ext)
        unless (Data.Set.member base srcMods) $
          removeFile absFile `catch` ignoreNotExists

    pruneTyDb srcMods typesRoot absDir = do
      let rel = normalise (makeRelative typesRoot absDir)
          base = dropProjDir (dropExtension rel)
      unless (Data.Set.member base srcMods) $ do
        removePathForcibly absDir `catch` ignoreNotExists

    proj = BuildSpec.specName $ projBuildSpec ctx

    dropProjDir rel = case splitDirectories $ normalise (dropExtension rel) of
                        [n] -> n
                        n:ns | n == proj -> joinPath ns
                        ns -> joinPath ns

    ignoreNotExists :: IOException -> IO ()
    ignoreNotExists _ = return ()

-- | Incremental variant: prune only modules whose changed .act file no longer exists.
pruneMissingChangedModuleOutputs :: [ProjCtx] -> [FilePath] -> IO ()
pruneMissingChangedModuleOutputs ctxs changedPaths = do
    absChanged <- mapM normalizePathSafe changedPaths
    let actPaths = filter (\p -> takeExtension p == ".act") absChanged
    forM_ actPaths $ \actPath -> do
      exists <- doesFileExist actPath
      unless exists $
        mapM_ (pruneForCtx actPath) ctxs
  where
    pruneForCtx actPath ctx = do
      srcRoot <- normalizePathSafe (projSrcDir ctx)
      let srcRoot' = addTrailingPathSeparator (normalise srcRoot)
          actPath' = normalise actPath
      when (Data.List.isPrefixOf srcRoot' actPath') $ do
        let modBase = normalise (dropExtension (makeRelative srcRoot actPath'))
            outBase = projTypesDir ctx </> modBase
        removePathForcibly (outBase ++ InterfaceFiles.interfaceExt) `catch` ignoreNotExists
        mapM_ (\ext -> removeFile (outBase ++ ext) `catch` ignoreNotExists) [".c", ".h"]

    ignoreNotExists :: IOException -> IO ()
    ignoreNotExists _ = return ()

-- | Build a search path for module interfaces for a project.
-- Includes the project's types dir, dependency types, user searchpath, and
-- the system types directory.
searchPathForProject :: C.CompileOptions -> M.Map FilePath ProjCtx -> ProjCtx -> [FilePath]
searchPathForProject opts projMap ctx =
    let deps = depTypePathsFromMap projMap (projRoot ctx)
    in [projTypesDir ctx] ++ deps ++ (C.searchpath opts) ++ systemTypePaths (projSysPath ctx) (projSysTypes ctx)

-- | Construct a Paths record for a module within a project context.
-- Creates output directories and ensures the types directory exists.
pathsForModule :: C.CompileOptions -> M.Map FilePath ProjCtx -> ProjCtx -> A.ModName -> IO Paths
pathsForModule opts projMap ctx mn = do
    let sPaths = searchPathForProject opts projMap ctx
        bin = joinPath [projOutDir ctx, "bin"]
        src = projSrcDir ctx
        proj = BuildSpec.specName $ projBuildSpec ctx
        p = Paths sPaths (projSysPath ctx) (projSysTypes ctx) (projRoot ctx) (projOutDir ctx) (projTypesDir ctx) bin src False ".act" mn proj
    InterfaceFiles.registerSystemTypeRoots (systemTypePaths (projSysPath ctx) (projSysTypes ctx))
    createDirectoryIfMissing True bin
    createDirectoryIfMissing True (projOutDir ctx)
    createDirectoryIfMissing True (projTypesDir ctx)
    createDirectoryIfMissing True (getModPath (projTypesDir ctx) mn)
    return p


-- | Load a BuildSpec from Build.act.
-- Throws ProjectError on parse or validation failure.
loadBuildSpec :: FilePath -> IO BuildSpec.BuildSpec
loadBuildSpec dir = do
    let actPath = joinPath [dir, "Build.act"]
    actExists <- doesFileExist actPath
    if actExists
      then do
        content <- readFile actPath
        case BuildSpec.parseBuildActDetailed content of
          Left err ->
            case err of
              BuildSpec.MissingFingerprint name -> do
                suggestion <- suggestFingerprint name
                throwProjectError ("Missing fingerprint in " ++ actPath ++ ".\n"
                                   ++ "ERROR: Build.act requires `fingerprint`. For example: fingerprint = " ++ suggestion ++ "\n"
                                   ++ "HINT: Fingerprint = CRC32(name) in the high 32 bits + random low 32 bits. You may choose a different value.")
              BuildSpec.InvalidFingerprint name raw -> do
                suggestion <- suggestFingerprint name
                throwProjectError ("Invalid fingerprint " ++ raw ++ " in " ++ actPath
                                   ++ " (project name: " ++ show name ++ ").\n"
                                   ++ "Expected an unquoted 64-bit hex fingerprint like 0x1234abcd5678ef00.\n"
                                   ++ "Suggested fingerprint: " ++ suggestion)
              BuildSpec.MissingProjectName -> do
                suggestion <- suggestProjectName dir
                throwProjectError ("Missing project name in " ++ actPath ++ ".\n"
                                   ++ "Add: name = " ++ show suggestion)
              _ ->
                throwProjectError ("Failed to parse Build.act in " ++ dir ++ ":\n"
                                   ++ BuildSpec.renderBuildSpecParseError err)
          Right (spec, _, _) -> validateBuildSpec actPath spec
      else
        throwProjectError ("Missing Build.act in " ++ dir ++ ".\n"
                           ++ "Create Build.act with required name and fingerprint fields.")

validateBuildSpec :: FilePath -> BuildSpec.BuildSpec -> IO BuildSpec.BuildSpec
validateBuildSpec sourcePath spec = do
    validateProjectName sourcePath (BuildSpec.specName spec)
    validateFingerprint sourcePath (BuildSpec.specName spec) (BuildSpec.fingerprint spec)
    return spec

validateProjectName :: FilePath -> String -> IO ()
validateProjectName sourcePath name =
    if not (isProjectIdent name)
      then throwProjectError ("Invalid project name '" ++ name ++ "' in " ++ sourcePath ++ ".\n"
                              ++ "The name must be a valid Acton project name (letters, digits, underscore; cannot start with a digit).")
      else if length name > projectNameMax
        then throwProjectError ("Invalid project name '" ++ name ++ "' in " ++ sourcePath ++ ".\n"
                                ++ "The name must be at most " ++ show projectNameMax ++ " characters.")
        else return ()
  where
    isProjectIdent [] = False
    isProjectIdent (c:cs) =
      (isAlpha c || c == '_') && all isIdentChar cs
      where
        isIdentChar x = isAlpha x || isDigit x || x == '_'

suggestProjectName :: FilePath -> IO String
suggestProjectName dir = do
    resolved <- normalizePathSafe dir
    let base = takeBaseName resolved
    if null base || base == "."
      then return "my_project"
      else return base

suggestFingerprint :: String -> IO String
suggestFingerprint name = do
    let prefix = Fingerprint.fingerprintPrefixForName name
    low <- randomRIO (1, maxBound :: Word32)
    let fp = (fromIntegral prefix `shiftL` 32) .|. (fromIntegral low :: Word64)
    return (Fingerprint.formatFingerprint fp)

validateFingerprint :: FilePath -> String -> String -> IO ()
validateFingerprint sourcePath name fpRaw =
    case Fingerprint.parseFingerprint fpRaw of
      Nothing -> do
        suggestion <- suggestFingerprint name
        throwProjectError ("Invalid fingerprint '" ++ fpRaw ++ "' in " ++ sourcePath
                           ++ " (project name: " ++ show name ++ ").\n"
                           ++ "Expected an unquoted 64-bit hex fingerprint like 0x1234abcd5678ef00.\n"
                           ++ "Suggested fingerprint: " ++ suggestion)
      Just fp -> do
        let formatted = Fingerprint.formatFingerprint fp
            expectedPrefix = Fingerprint.fingerprintPrefixForName name
            expectedPrefixHex = Fingerprint.formatFingerprintPrefix expectedPrefix
            actualPrefix = fromIntegral (fp `shiftR` 32)
        if formatted == Fingerprint.fingerprintPlaceholder
          then do
            suggestion <- suggestFingerprint name
            throwProjectError ("Fingerprint placeholder " ++ formatted ++ " in " ++ sourcePath
                               ++ " is not valid.\n"
                               ++ "Suggested fingerprint: " ++ suggestion)
          else if expectedPrefix == actualPrefix
            then return ()
            else do
              suggestion <- suggestFingerprint name
              throwProjectError ("Fingerprint mismatch in " ++ sourcePath
                                 ++ " for project name " ++ show name ++ ".\n"
                                 ++ "Expected prefix: " ++ expectedPrefixHex
                                 ++ " (CRC32 of name).\n"
                                 ++ "Current fingerprint: " ++ formatted ++ "\n"
                                 ++ "Renames and forks require a new fingerprint for this name.\n"
                                 ++ "Suggested fingerprint: " ++ suggestion)


projectNameMax :: Int
projectNameMax = 32

-- | Treat drive-letter paths as absolute in addition to POSIX roots.
-- This keeps path normalization consistent on Windows hosts.
isAbsolutePath :: FilePath -> Bool
isAbsolutePath p =
    isAbsolute p ||
    case p of
      (c:':':_) -> isAlpha c
      _         -> False

-- | Normalize a path without failing if it does not exist.
-- Falls back to normalise for non-existent paths (useful for temporary builds).
normalizePathSafe :: FilePath -> IO FilePath
normalizePathSafe p = do
    res <- try (canonicalizePath p) :: IO (Either IOException FilePath)
    return $ either (const (normalise p)) id res

-- | Trim leading and trailing whitespace from a string.
-- Used when parsing or normalizing BuildSpec inputs.
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

-- | Collapse "." and ".." segments without dropping leading ".." on relative paths.
-- Example: "a/b/../c/./d" becomes "a/c/d".
collapseDots :: FilePath -> FilePath
collapseDots p =
    let parts = splitDirectories p
        (root, rest) = case parts of
                         (r:xs) | isRoot r -> (Just r, xs)
                         xs                -> (Nothing, xs)
        (revAcc, ups) = foldl step ([], 0) rest
        cleaned = replicate ups ".." ++ reverse revAcc
        prefix = maybe [] (\r -> [r]) root
    in joinPath (prefix ++ cleaned)
  where
    isRoot r = r == "/" || (length r >= 2 && r !! 1 == ':')
    step (acc, ups) comp =
      case comp of
        "."  -> (acc, ups)
        ""   -> (acc, ups)
        ".." -> case acc of
                  (_:as) -> (as, ups)
                  []     -> (acc, ups + 1)
        _    -> (comp:acc, ups)

-- | Rebase a path against a base directory and normalize it.
-- Absolute paths are left as-is; relative paths are joined to the base.
rebasePath :: FilePath -> FilePath -> FilePath
rebasePath base p
  | isAbsolutePath p = normalise p
  | otherwise        = normalise (joinPath [base, p])

-- | Normalize --dep overrides relative to a base directory.
-- Absolute override paths are kept, relative paths are rebased and normalized.
normalizeDepOverrides :: FilePath -> [(String, FilePath)] -> IO [(String, FilePath)]
normalizeDepOverrides base overrides =
  mapM (\(n,p) -> do
           let absP0 = if isAbsolutePath p then p else joinPath [base, p]
           p' <- normalizePathSafe absP0
           return (n, p'))
       overrides

-- | Apply --dep overrides (NAME=PATH) to a BuildSpec.
-- Resolves override paths relative to the given base directory when needed.
applyDepOverrides :: FilePath -> [(String, FilePath)] -> BuildSpec.BuildSpec -> IO BuildSpec.BuildSpec
applyDepOverrides base overrides spec = do
    deps' <- foldM applyOne (BuildSpec.dependencies spec) overrides
    return spec { BuildSpec.dependencies = deps' }
  where
    applyOne depsMap (depName, depPath) =
      case M.lookup depName depsMap of
        Nothing -> return depsMap
        Just dep -> do
          let absP0 = if isAbsolutePath depPath then depPath else joinPath [base, depPath]
          absP <- normalizePathSafe absP0
          validateDepOverridePath depName absP
          let dep' = dep { BuildSpec.path = Just absP }
          return (M.insert depName dep' depsMap)

validateDepOverridePath :: String -> FilePath -> IO ()
validateDepOverridePath depName depPath = do
    exists <- doesDirectoryExist depPath
    unless exists $
      throwProjectError ("Dependency " ++ depName ++ " path does not exist: " ++ depPath ++ "\n"
                         ++ "Hint: Local dependency paths must point to an Acton project root\n"
                         ++ "(directory with src/ and Build.act).")
    isProjectRoot <- isActonProjectRoot depPath
    unless isProjectRoot $
      throwProjectError ("Dependency " ++ depName ++ " path is not an Acton project root: " ++ depPath ++ "\n"
                         ++ "Hint: Local dependency paths must point to an Acton project root\n"
                         ++ "(directory with src/ and Build.act).")

fetchDependencies :: C.GlobalOptions -> Paths -> [(String, FilePath)] -> IO ()
fetchDependencies gopts paths depOverrides = do
    if isTmp paths
      then return ()
      else do
        rootSpec0 <- loadBuildSpec (projPath paths)
        rootSpec <- applyDepOverrides (projPath paths) depOverrides rootSpec0
        unless (C.quiet gopts) $
          putStrLn "Resolving dependencies (fetching if missing)..."
        when (C.verbose gopts) $ do
          env <- readProxyEnv
          putStr (unlines (proxyEnvReportLines "" env))
        home <- getHomeDirectory
        let zigExe      = joinPath [sysPath paths, "zig", "zig"]
            globalCache = joinPath [home, ".cache", "acton", "zig-global-cache"]
            depsCache   = joinPath [home, ".cache", "acton", "deps"]
            cacheDir h  = joinPath [globalCache, "p", h]
        createDirectoryIfMissing True globalCache
        createDirectoryIfMissing True depsCache
        -- Tracks zig package dirs / hashes already pre-seeded, so the transitive
        -- walk over build.zig.zon files terminates on cycles and shared deps.
        zigSeen <- newIORef Data.Set.empty
        let rootPins = BuildSpec.dependencies rootSpec
        _ <- walkProject rootPins cacheDir zigExe globalCache depsCache zigSeen
                         Data.Set.empty (projPath paths) rootSpec
        return ()
  where
    walkProject rootPins cacheDir zigExe globalCache depsCache zigSeen seen projDir spec = do
      projAbs <- normalizePathSafe projDir
      if Data.Set.member projAbs seen
        then return seen
        else do
          selectedDeps <- forM (M.toList (BuildSpec.dependencies spec)) $
            selectDependency rootPins projAbs
          let pkgFetches = catMaybes
                [ mkPkgFetch cacheDir zigExe globalCache name dep | (name, dep) <- selectedDeps ]
              zigFetches = catMaybes
                [ mkZigFetch cacheDir zigExe globalCache name dep
                | (name, dep) <- M.toList (BuildSpec.zig_dependencies spec)
                ]
          results <- mapConcurrently id (pkgFetches ++ zigFetches)
          let errs = [ e | Left e <- results ]
          unless (null errs) $ throwProjectError (unlines errs)
          forM_ selectedDeps $ \(name, dep) -> copyPkgDep cacheDir depsCache name dep
          -- TODO: remove this transitive pre-seeding (this forM_ and
          -- seedTransitiveZigDeps) once zig can fetch through an HTTP CONNECT
          -- proxy. It exists only to work around zig writing an absolute-form
          -- request URI into the tunnel, which the origin rejects; when zig is
          -- fixed upstream / in our bundled toolchain, drop this and let zig
          -- resolve transitive deps itself.
          --
          -- A zig dependency given as a local path may pull in transitive .url
          -- dependencies via its build.zig.zon; seed those into zig's cache now
          -- (through the proxy-aware HTTP client) so the build never lets zig
          -- fetch them. A url+hash zig dependency is a self-contained archive,
          -- already seeded above by mkZigFetch (see seedTransitiveZigDeps for why
          -- url packages are not recursed into).
          forM_ (M.toList (BuildSpec.zig_dependencies spec)) $ \(_, zdep) ->
            case BuildSpec.zpath zdep of
              Just p | not (null p) ->
                seedTransitiveZigDeps zigSeen cacheDir zigExe globalCache (projAbs </> p)
              _ -> return ()
          let seen' = Data.Set.insert projAbs seen
          foldM (walkDependency rootPins cacheDir zigExe globalCache depsCache zigSeen projAbs)
                seen' selectedDeps

    selectDependency rootPins base (depName, dep) = do
      let (chosenDep, conflict) =
            case M.lookup depName rootPins of
              Nothing -> (dep, Nothing)
              Just pinDep ->
                if pinDep == dep
                  then (dep, Nothing)
                  else (pinDep, Just dep)
      when (isJust conflict) $
        unless (C.quiet gopts) $
          putStrLn ("Warning: dependency '" ++ depName ++ "' in " ++ base
                    ++ " overridden by root pin")
      return (depName, chosenDep)

    walkDependency rootPins cacheDir zigExe globalCache depsCache zigSeen base seen (depName, dep) = do
      depBase <- resolveDepBase base depName dep
      depAbs <- normalizePathSafe depBase
      depExists <- doesDirectoryExist depAbs
      if not depExists
        then case BuildSpec.path dep of
               Just p | not (null p) ->
                 throwProjectError ("Dependency " ++ depName ++ " path does not exist: " ++ depAbs ++ "\n"
                                    ++ "Hint: Local dependency paths must point to an Acton project root\n"
                                    ++ "(directory with src/ and Build.act).")
               _ -> return seen
        else do
          depSpec0 <- loadBuildSpec depAbs
          depSpec <- applyDepOverrides depAbs depOverrides depSpec0
          walkProject rootPins cacheDir zigExe globalCache depsCache zigSeen seen depAbs depSpec

    mkPkgFetch cacheDir zigExe globalCache name dep =
      case BuildSpec.path dep of
        Just p | not (null p) -> Nothing
        _ -> case (BuildSpec.url dep, BuildSpec.hash dep) of
               (Just u, Just h) ->
                 Just (fetchOne "pkg" name u (Just h) cacheDir zigExe globalCache)
               (Just _, Nothing) ->
                 Just (return (Left ("Dependency " ++ name ++ " is missing hash")))
               _ -> Nothing

    mkZigFetch cacheDir zigExe globalCache name dep =
      case BuildSpec.zpath dep of
        Just p | not (null p) -> Nothing
        _ -> case (BuildSpec.zurl dep, BuildSpec.zhash dep) of
               (Just u, Just h) ->
                 Just (fetchOne "zig" name u (Just h) cacheDir zigExe globalCache)
               (Just _, Nothing) ->
                 Just (return (Left ("Zig dependency " ++ name ++ " is missing hash")))
               _ -> Nothing

    copyPkgDep cacheDir depsCache name dep =
      case BuildSpec.path dep of
        Just p | not (null p) -> return ()
        _ -> case BuildSpec.hash dep of
               Nothing -> return ()
               Just h -> do
                 let src = cacheDir h
                     srcArchive = cacheArchivePath cacheDir h
                     dst = joinPath [depsCache, name ++ "-" ++ h]
                 exists <- doesDirectoryExist dst
                 unless exists $ do
                   srcOk <- doesDirectoryExist src
                   archiveOk <- doesFileExist srcArchive
                   unless (srcOk || archiveOk) $
                     throwProjectError ("Dependency " ++ name ++ " not present in Zig cache after fetch: " ++ src)
                   when (C.verbose gopts) $
                     putStrLn ("Copying dependency " ++ name ++ " (" ++ h ++ ") from Zig cache")
                   if srcOk
                     then copyTree src dst
                     else extractCachedArchive srcArchive dst

    -- Pre-seed zig's package cache for the transitive dependencies reachable from
    -- a zig package's build.zig.zon. Path deps are followed in place, recursing
    -- into their own build.zig.zon; url+hash deps are downloaded through the
    -- proxy-aware HTTP client (via fetchOne) so zig finds them cached. Lazy deps
    -- are left for zig to fetch on demand: eagerly seeding them would download
    -- archives a normal build never uses.
    --
    -- A url dependency is seeded but not recursed into: the package archives that
    -- occur in practice (lmdb, libssh, zlib upstream) are leaf C sources with no
    -- nested build.zig.zon, and reading a manifest back out of every seeded
    -- tarball would cost a per-build extraction on the common path.
    seedTransitiveZigDeps zigSeen cacheDir zigExe globalCache = goDir
      where
        goDir dir = do
          dirAbs <- normalizePathSafe dir
          fresh <- claimVisited zigSeen ("d:" ++ dirAbs)
          when fresh $ do
            let zonPath = dirAbs </> "build.zig.zon"
            edeps <- Zon.readZonDependencies zonPath
            case edeps of
              Left err ->
                when (C.verbose gopts) $
                  putStrLn ("Note: could not parse " ++ zonPath
                            ++ " to pre-seed transitive zig dependencies: " ++ err)
              Right deps -> forM_ deps (goDep dirAbs)

        goDep pkgDir (name, dep)
          | Zon.zdLazy dep                          = return ()
          | Just p <- Zon.zdPath dep, not (null p)  = goDir (pkgDir </> p)
          | Just u <- Zon.zdUrl dep
          , Just h <- Zon.zdHash dep                = do
              fresh <- claimVisited zigSeen ("h:" ++ h)
              when fresh $ do
                present <- cacheEntryExists cacheDir h
                unless present $ do
                  res <- fetchOne "zig" name u (Just h) cacheDir zigExe globalCache
                  case res of
                    -- Non-fatal: if pre-seeding fails, let zig try to fetch it
                    -- during the build (its pre-change behaviour). On a direct
                    -- build that just works; behind a proxy zig fails with its own
                    -- diagnostic -- either way a seeding hiccup never breaks a
                    -- build the old code would have completed.
                    Left e  -> unless (C.quiet gopts) $
                                 putStrLn ("Warning: could not pre-fetch transitive zig dependency "
                                           ++ name ++ ": " ++ e)
                    Right _ -> return ()
          | otherwise                               = return ()

    -- Atomically record a key; returns True the first time it is seen.
    claimVisited ref key = atomicModifyIORef' ref $ \s ->
      if Data.Set.member key s then (s, False) else (Data.Set.insert key s, True)

    fetchOne kind name url mh cacheDir zigExe globalCache = do
      case mh of
        Just h -> do
          present <- cacheEntryExists cacheDir h
          if present
            then do
              unless (C.quiet gopts) $
                putStrLn ("Using cached " ++ kind ++ " dependency " ++ name ++ " (" ++ h ++ ")")
              return (Right h)
            else runFetch kind name url mh cacheDir zigExe globalCache
        Nothing ->
          runFetch kind name url mh cacheDir zigExe globalCache

    runFetch kind name url mh cacheDir zigExe globalCache = do
      unless (C.quiet gopts) $
        putStrLn ("Fetching " ++ kind ++ " dependency " ++ name ++ " from " ++ url)
      if isHttpUrl url
        then fetchViaDownloadedArchive kind name url mh cacheDir zigExe globalCache
        else do -- other URLs, like file:// - not very common, maybe we want to constrain this somehow?
          res <- runZigFetch zigExe globalCache url
          case res of
            Left ex -> return (Left ("Failed to fetch dependency " ++ name ++ ": " ++ displayException ex))
            Right (ExitSuccess, out, _) ->
              validateFetchOutput name mh cacheDir out
            Right (ExitFailure _, _, err) ->
              return (Left ("Failed to fetch dependency " ++ name ++ ":\n" ++ err))

    runZigFetch :: FilePath -> FilePath -> FilePath -> IO (Either SomeException (ExitCode, String, String))
    runZigFetch zigExe globalCache target = do
      createDirectoryIfMissing True (globalCache </> "tmp")
      withSystemTempDirectory "acton-zig-fetch" $ \tmp -> do
        writeFile (tmp </> "build.zig") zigFetchBuildZig
        let cmd = (proc zigExe ["fetch", "--global-cache-dir", globalCache, target]) { cwd = Just tmp }
        try (readCreateProcessWithExitCode cmd "") :: IO (Either SomeException (ExitCode, String, String))

    zigFetchBuildZig :: String
    zigFetchBuildZig = unlines
      [ "const std = @import(\"std\");"
      , "pub fn build(b: *std.Build) void { _ = b; }"
      ]

    validateFetchOutput :: String -> Maybe String -> (String -> FilePath) -> String -> IO (Either String String)
    validateFetchOutput name mh cacheDir out = do
      let hashVal = trim out
      case mh of
        Just h | h /= hashVal ->
          return (Left ("Hash mismatch for dependency " ++ name ++ " (expected " ++ h ++ ", got " ++ hashVal ++ ")"))
        _ -> do
          exists <- cacheEntryExists cacheDir hashVal
          if exists
            then return (Right hashVal)
            else return (Left ("Dependency " ++ name ++ " not present in Zig cache after fetch: " ++ cacheDir hashVal))

    cacheEntryExists :: (String -> FilePath) -> String -> IO Bool
    cacheEntryExists cacheDir hashVal = do
      dirExists <- doesDirectoryExist (cacheDir hashVal)
      archiveExists <- doesFileExist (cacheArchivePath cacheDir hashVal)
      return (dirExists || archiveExists)

    cacheArchivePath :: (String -> FilePath) -> String -> FilePath
    cacheArchivePath cacheDir hashVal = cacheDir hashVal ++ ".tar.gz"

    extractCachedArchive :: FilePath -> FilePath -> IO ()
    extractCachedArchive archive dst = do
      bracketOnError (createTempDirectory (takeDirectory dst) ".acton-dep-extract")
                     removeDirectoryRecursive $ \tmp -> do
        let cmd = proc "tar" ["-xzf", archive, "-C", tmp, "--strip-components", "1"]
        (code, out, err) <- readCreateProcessWithExitCode cmd ""
        case code of
          ExitSuccess -> renameDirectory tmp dst
          ExitFailure _ ->
            throwProjectError ("Failed to extract cached dependency archive " ++ archive ++ ":\n" ++ out ++ err)

    fetchViaDownloadedArchive kind name depUrl mh cacheDir zigExe globalCache = do
      -- depUrl is always http(s) here (runFetch's isHttpUrl guard), so
      -- downloadArchive (proxy-aware http-client) is the right path. Do NOT fall
      -- back to `zig fetch <depUrl>`: zig's network stack cannot traverse an HTTP
      -- CONNECT proxy, so it would hang and bury the actionable proxy diagnostic.
      dl <- downloadArchive depUrl
      case dl of
        Left dlErr ->
          return (Left ("Failed to fetch dependency " ++ name ++ ":\n" ++ dlErr))
        Right localArchive -> do
          fetched <- runZigFetch zigExe globalCache localArchive
          _ <- try (removeFile localArchive) :: IO (Either IOException ())
          case fetched of
            Left ex ->
              return (Left ("Failed to fetch dependency " ++ name ++ ": " ++ displayException ex))
            Right (ExitSuccess, out, _) ->
              validateFetchOutput name mh cacheDir out
            Right (ExitFailure _, _, err) ->
              return (Left ("Failed to fetch dependency " ++ name ++ ":\n" ++ err))

    copyTree :: FilePath -> FilePath -> IO ()
    copyTree src dst = do
      exists <- doesDirectoryExist src
      unless exists $ throwProjectError ("Source path for copyTree does not exist: " ++ src)
      createDirectoryIfMissing True dst
      entries <- listDirectory src
      forM_ entries $ \e -> do
        let s = src </> e
            d = dst </> e
        isDir <- doesDirectoryExist s
        if isDir
          then copyTree s d
          else do
            createDirectoryIfMissing True (takeDirectory d)
            copyFile s d

-- | Collect dependency type directories using an existing ProjCtx map.
-- Traverses dependency edges so downstream lookups can build search paths.
depTypePathsFromMap :: M.Map FilePath ProjCtx -> FilePath -> [FilePath]
depTypePathsFromMap ctxs root = snd (go Data.Set.empty root)
  where
    go seen node =
      case M.lookup node ctxs of
        Nothing  -> (seen, [])
        Just ctx ->
          foldl' step (Data.Set.insert node seen, []) (projDeps ctx)

    step (seen, acc) (_, depRoot) =
      case M.lookup depRoot ctxs of
        Nothing -> (Data.Set.insert depRoot seen, acc)
        Just depCtx ->
          if Data.Set.member depRoot seen
            then (seen, acc)
            else
              let seen' = Data.Set.insert depRoot seen
                  (seenNext, sub) = go seen' depRoot
              in (seenNext, acc ++ [projTypesDir depCtx] ++ sub)

-- | Resolve a dependency's base directory from a BuildSpec PkgDep.
-- Prefers explicit path, otherwise uses the hashed cache location.
resolveDepBase :: FilePath -> String -> BuildSpec.PkgDep -> IO FilePath
resolveDepBase base name dep =
    case BuildSpec.path dep of
      Just p | not (null p) -> normalizePathSafe (rebasePath base p)
      _ -> case BuildSpec.hash dep of
             Just h -> do
               home <- getHomeDirectory
               normalizePathSafe (joinPath [home, ".cache", "acton", "deps", name ++ "-" ++ h])
             Nothing -> throwProjectError ("Dependency " ++ name ++ " has no path or hash")

-- | Recursively collect out/types paths for all declared dependencies.
-- Reads Build.act and follows dependency edges.
collectDepTypePaths :: FilePath -> [(String, FilePath)] -> IO (String,[FilePath])
collectDepTypePaths projDir overrides = do
  root <- normalizePathSafe projDir
  spec <- loadBuildSpec root
  (_, _, paths) <- go Data.Set.empty M.empty root spec
  return (BuildSpec.specName spec, paths)
  where
    go seen fpMap dir spec0 = do
      spec <- applyDepOverrides dir overrides spec0
      let (_, fpMap1, _) = applyFingerprint dir spec fpMap
      foldM (step dir) (seen, fpMap1, []) (M.toList (BuildSpec.dependencies spec))

    step base (seen, fpMap, acc) (depName, dep) = do
      depBase <- resolveDepBase base depName dep
      depAbs  <- normalizePathSafe depBase
      depExists <- doesDirectoryExist depAbs
      if not depExists
        then case BuildSpec.path dep of
               Just p | not (null p) ->
                 throwProjectError ("Dependency " ++ depName ++ " path does not exist: " ++ depAbs ++ "\n"
                                    ++ "Hint: Local dependency paths must point to an Acton project root\n"
                                    ++ "(directory with src/ and Build.act).")
               _ -> return (seen, fpMap, acc)
        else do
          (depPath, fpMap') <- canonicalizeDep depAbs fpMap
          let typesDir = joinPath [depPath, "out", "types"]
          if Data.Set.member depPath seen
            then return (seen, fpMap', acc)
            else do
              let seen' = Data.Set.insert depPath seen
              spec <- loadBuildSpec depPath
              (seenNext, fpMapNext, sub) <- go seen' fpMap' depPath spec
              return (seenNext, fpMapNext, acc ++ [typesDir] ++ sub)

    canonicalizeDep depAbs fpMap = do
      spec0 <- loadBuildSpec depAbs
      spec <- applyDepOverrides depAbs overrides spec0
      let (canonPath, fpMap', _) = applyFingerprint depAbs spec fpMap
      return (canonPath, fpMap')


-- | Convert a module name to its source filename (path + .act).
-- This is used for diagnostics and display output.
modNameToFilename :: A.ModName -> String
modNameToFilename mn = joinPath (map nameToString names) ++ ".act"
  where
    A.ModName names = mn

-- | Render a module name as a dotted string (foo.bar.baz).
modNameToString :: A.ModName -> String
modNameToString (A.ModName names) = intercalate "." (map nameToString names)

-- | Render a name identifier to a plain string.
nameToString :: A.Name -> String
nameToString (A.Name _ s) = s

addProjPrefix paths mn      = A.modName $ if proj `elem` special_projects then ns else proj : ns
  where proj                = projName paths
        ns                  = A.modPath mn

dropProjPrefix paths mn     = A.modName $ if n == projName paths then ns else n:ns
  where n:ns                = A.modPath mn

dropProjPrefixOrLib paths mn
  | n == projName paths     = A.modName ns
  | ns == ["lib"]           = A.modName [n]
  | otherwise               = A.modName (n:ns)
  where n:ns                = A.modPath mn


-- | Check whether a NameInfo represents a root-eligible actor.
-- Used to decide which roots to include in .tydb headers and root generation.
rootEligible :: I.NameInfo -> Bool
rootEligible (I.NAct [] p k _ _) = case (p,k) of
                                      (A.TNil{}, A.TRow _ _ _ t A.TNil{}) ->
                                        prstr t == "Env" || prstr t == "None" ||
                                        prstr t == "__builtin__.Env" || prstr t == "__builtin__.None"
                                      _ -> False
rootEligible _ = False

-- | Determine whether any non-standard output mode is enabled.
-- Used to suppress normal timing/output when dumping parse/sigs/cgen, etc.
altOutput :: C.CompileOptions -> Bool
altOutput opts =
  (C.parse opts) || (C.parse_ast opts) || (C.kinds opts) || (C.types opts) || (C.sigs opts) || (C.norm opts) || (C.deact opts) || (C.cps opts) || (C.llift opts) || (C.box opts) || (C.hgen opts) || (C.cgen opts)

-- | Read a UTF-8 text file with explicit encoding.
-- Keeps compiler IO consistent across platforms.
readFile :: FilePath -> IO String
readFile f = do
    h <- openFile f ReadMode
    hSetEncoding h utf8
    c <- hGetContents h
    return c

-- | Write a UTF-8 text file atomically.
-- Used for generated sources and BuildSpec parsing.
writeFile :: FilePath -> String -> IO ()
writeFile = writeFileUtf8Atomic

-- | Format a TimeSpec as a fixed-width seconds string.
-- Used for stable logging and snapshot test output.
fmtTime :: TimeSpec -> String
fmtTime t =
    printf "%6.3f s" secs
  where
    secs :: Float
    secs = (fromIntegral(sec t)) + (fromIntegral (nsec t) / 1000000000)

-- | Topologically order projects so dependencies come first.
-- Used to build search paths and providers in dependency order.
projDepClosure :: M.Map FilePath ProjCtx -> FilePath -> [FilePath]
projDepClosure ctxs root = reverse (dfs Data.Set.empty [] root)
  where
    depsOf p = maybe [] (map snd . projDeps) (M.lookup p ctxs)

    dfs seen acc node
      | Data.Set.member node seen = acc
      | otherwise =
          let seen' = Data.Set.insert node seen
              acc'  = foldl' (\a n -> dfs seen' a n) acc (depsOf node)
          in node : acc'
