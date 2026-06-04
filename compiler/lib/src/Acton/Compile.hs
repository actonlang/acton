{-# LANGUAGE CPP #-}
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
    2) runCompilePlan drives compileTasks. As each module finishes its front
       passes, the CompileHooks callbacks (for example chOnFrontResult)
       enqueue a BackJob into the scheduler's
       BackQueue, so back passes begin as soon as they are ready and can overlap
       remaining front passes.
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
  - CLI builds share the same pipeline and enqueue back jobs as soon as front
    passes finish, overlapping work without needing a watch event source.

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
  , FrontOutputJob
  , waitFrontOutputJobs
  , FrontTiming(..)
  , FrontHashTiming(..)
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
  , readImports
  , buildGlobalTasks
  , selectNeededTasks
  , selectAffectedTasks
  , libraryBoundaryTasks
  , compileTasks
  , runFrontPasses
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
import qualified Acton.Names as Names
import qualified Acton.Kinds
import qualified Acton.Types
import qualified Acton.Converter as Converter
import qualified Acton.Normalizer
import qualified Acton.CPS
import qualified Acton.Deactorizer
import qualified Acton.LambdaLifter
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
import qualified InterfaceFiles

import Control.Applicative ((<|>))
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent (forkIO, myThreadId, threadCapability, threadDelay)
import Control.Concurrent.STM (TChan, TVar, atomically, check, modifyTVar', newTChanIO, newTVarIO, readTChan, readTVar, writeTChan)
import Control.DeepSeq (rnf)
import Control.Exception (Exception, IOException, SomeAsyncException, SomeException, bracketOnError, catch, displayException, evaluate, finally, fromException, mask_, onException, throwIO, try)
import Control.Monad
import Data.Binary (encode)
import Data.Bits (shiftL, shiftR, (.|.))
import Data.Char (isAlpha, isDigit, isHexDigit, isSpace, toLower)
import Data.Either (partitionEithers)
import Data.Graph
import Data.List (find, foldl', intercalate, intersperse, isPrefixOf, isSuffixOf, nub, partition)
import qualified Data.List
import Data.IORef
import qualified Data.IntMap.Strict as IM
import Data.Maybe (catMaybes, isJust, isNothing, listToMaybe, mapMaybe)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import Data.Ord (Down(..))
import qualified Data.Set
import Data.Time.Clock (UTCTime)
import Data.Word (Word8, Word32, Word64)
import Error.Diagnose (Diagnostic)
import GHC.Conc (getNumCapabilities)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header (hContentDisposition, hContentType)
import Network.HTTP.Types.Status (statusCode)
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
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base16 as Base16
import qualified Crypto.Hash.SHA256 as SHA256


newtype ProjectError = ProjectError String deriving (Show)

instance Exception ProjectError

newtype DbpSelectionError = DbpSelectionError String deriving (Show)

instance Exception DbpSelectionError where
  displayException (DbpSelectionError msg) = msg

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

data FrontTiming = FrontTiming
  { ftEnv :: TimeSpec
  , ftKinds :: TimeSpec
  , ftTypes :: TimeSpec
  , ftTypeReconstruct :: TimeSpec
  , ftTypeAfterProgress :: TimeSpec
  , ftTypeForce :: TimeSpec
  , ftTypeHash :: TimeSpec
  , ftTypeHashDetail :: FrontHashTiming
  , ftTypeStmtTimings :: [TypeStmtTiming]
  } deriving (Eq, Show)

data FrontHashTiming = FrontHashTiming
  { fhtSourceFacts :: TimeSpec
  , fhtImplFacts :: TimeSpec
  , fhtPublicFacts :: TimeSpec
  , fhtDepSplit :: TimeSpec
  , fhtImportHashes :: TimeSpec
  , fhtExternalHashes :: TimeSpec
  , fhtDepHashes :: TimeSpec
  , fhtNameHashes :: TimeSpec
  , fhtModuleHashes :: TimeSpec
  , fhtStmtEntries :: TimeSpec
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
  , ccOnFrontOutputDone :: TaskKey -> FrontOutputKind -> Maybe TimeSpec -> IO ()
  , ccShouldWriteFrontOutput :: IO Bool
  , ccOnBackJob :: BackJob -> IO ()
  , ccOnBackSkipped :: TaskKey -> IO ()
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
  , ccOnFrontOutputDone = \_ _ _ -> return ()
  , ccShouldWriteFrontOutput = return True
  , ccOnBackJob = \_ -> return ()
  , ccOnBackSkipped = \_ -> return ()
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
newBackQueue :: IORef Int -> C.GlobalOptions -> Int -> IO BackQueue
newBackQueue genRef gopts maxPar = do
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
        failuresNow <- readTVar failures
        case M.lookup gen failuresNow of
          Just failure -> do
            modifyTVar' failures (M.delete gen)
            return (Just failure)
          Nothing -> do
            pending <- readTVar counts
            let n = M.findWithDefault 0 gen pending
            check (n == 0)
            return Nothing
      worker = forever $ do
        (gen, job, callbacks) <- atomically $ readTChan queue
        current <- readIORef genRef
        if current /= gen
          then atomically $ modifyTVar' counts (decPending gen)
          else do
            failed <- atomically $ do
              failuresNow <- readTVar failures
              return (M.member gen failuresNow)
            if failed
              then atomically $ modifyTVar' counts (decPending gen)
              else do
                let shouldWrite = do
                      currentWrite <- readIORef genRef
                      return (currentWrite == gen)
                bjcOnStart callbacks job
                res <- (try $
                          runBackPassesWithProgress
                            gopts (bjOpts job) (bjPaths job) (bjInput job)
                            shouldWrite (bjcOnProgress callbacks job))
                        :: IO (Either SomeException (Maybe TimeSpec, Maybe BackTiming))
                currentDone <- readIORef genRef
                when (currentDone == gen) $
                  case res of
                    Left err -> do
                      let key = TaskKey (projPath (bjPaths job)) (A.modname (biTypedMod (bjInput job)))
                          failure = BackPassFailure key (displayException err)
                      atomically $ modifyTVar' failures (recordFailure gen failure)
                      bjcOnDone callbacks job (BackJobFailed failure)
                    Right (t, bt) -> bjcOnDone callbacks job (BackJobOk t bt)
                atomically $ modifyTVar' counts (decPending gen)
  let workers = max 1 maxPar
  replicateM_ workers (forkIO worker)
  return BackQueue
    { backQueueEnqueue = enqueue
    , backQueueWait = waitDone
    }

data CompileScheduler = CompileScheduler
  { csGenRef :: IORef Int
  , csAsyncRef :: MVar (Maybe (Async ()))
  , csBackQueue :: BackQueue
  , csBuildStampRef :: IORef (Maybe BuildSpecStamp)
  }

-- | Create a scheduler with generation tracking and a shared back queue.
newCompileScheduler :: C.GlobalOptions -> Int -> IO CompileScheduler
newCompileScheduler gopts maxPar = do
  genRef <- newIORef 0
  asyncRef <- newMVar Nothing
  backQueue <- newBackQueue genRef gopts maxPar
  buildStampRef <- newIORef Nothing
  return CompileScheduler
    { csGenRef = genRef
    , csAsyncRef = asyncRef
    , csBackQueue = backQueue
    , csBuildStampRef = buildStampRef
    }

-- | Start a new compile action, canceling and joining any in-flight run.
-- Returns the generation id associated with this run.
startCompile :: CompileScheduler -> Int -> (Int -> IO ()) -> IO Int
startCompile sched delay run = do
  gen <- atomicModifyIORef' (csGenRef sched) $ \g -> let g' = g + 1 in (g', g')
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
  neededTasks0 <- case mChangedPaths of
    Nothing -> selectNeededTasks pathsRoot rootProj globalTasks srcFiles
    Just changed -> selectAffectedTasks rootProj opts' globalTasks dbpBlocked changed
  let neededTasks = expandBuildLibraryTasks projMap globalTasks neededTasks0
      rootTasks = [ gtTask t | t <- neededTasks0, tkProj (gtKey t) == rootProj ]
      rootPins = maybe M.empty (BuildSpec.dependencies . projBuildSpec) (M.lookup rootProj projMap)
  return CompilePlan
    { cpContext = ctx
    , cpProjMap = projMap
    , cpGlobalTasks = globalTasks
    , cpNeededTasks = neededTasks
    , cpDbpBlocked = dbpBlocked
    , cpRootTasks = rootTasks
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
runCompilePlan sp gopts plan sched gen hooks = do
  let ctx = cpContext plan
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
  compileTasks sp gopts opts' pathsRoot rootProj (cpNeededTasks plan) (cpDbpBlocked plan) callbacks
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
    , C.dbp = []
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
          hdr <- InterfaceFiles.readHeaderMaybe ty
          case hdr of
            Just (_sourceMetaH, _srcH, ih, _implH, _impsH, _depModulesH, _nameHashesH, _rootsH, _testsH, _docH) -> return (M.insert mn ih m, Just ih)
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
          hdr <- InterfaceFiles.readHeaderMaybe ty
          case hdr of
            Just (_sourceMetaH, _srcH, _pubH, ih, _impsH, _depModulesH, _nameHashesH, _rootsH, _testsH, _docH) -> return (M.insert mn ih m, Just ih)
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

-- | Update the name-hash cache after a successful compile.
updateNameHashCache :: A.ModName -> [InterfaceFiles.NameHashInfo] -> IO ()
updateNameHashCache mn infos =
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


-- Compilation tasks, chasing imported modules, compilation and building executables -----------------

data BackInput = BackInput
  { biTypeEnv   :: Acton.Env.Env0
  , biTypedMod  :: A.Module
  , biSrc       :: String
  , biImplHash  :: B.ByteString
  }

data BackJob = BackJob
  { bjPaths :: Paths
  , bjOpts  :: C.CompileOptions
  , bjInput :: BackInput
  }

-- Deferred jobs deliberately keep only reloadable metadata so large typed
-- modules can be released after front passes finish.
data DeferredBackJob = DeferredBackJob
  { dbjPaths :: Paths
  , dbjOpts :: C.CompileOptions
  , dbjMod :: A.ModName
  , dbjImplHash :: B.ByteString
  , dbjNameCount :: Int
  , dbjSeeds :: Data.Set.Set A.Name
  , dbjReason :: String
  , dbjOutputJobs :: [FrontOutputJob]
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
  , frPubHash :: B.ByteString
  , frImplHash :: B.ByteString
  , frNameHashes :: [InterfaceFiles.NameHashInfo]
  , frInterestDeps :: Data.Set.Set (A.ModName, A.Name)
  , frFrontTime :: Maybe TimeSpec
  , frFrontTiming :: Maybe FrontTiming
  , frInferredSigs :: [InferredSignature]
  , frBackJob  :: Maybe BackJob
  , frDeferredBackJob :: Maybe DeferredBackJob
  , frOutputJobs :: [FrontOutputJob]
  }

data DbpRequest = DbpRequest
  { drMod :: A.ModName
  , drSeeds :: Data.Set.Set A.Name
  }

type InterestMap = M.Map A.ModName (Data.Set.Set A.Name)

dbpNameCountThreshold :: Int
-- Conservative initial heuristic; --dbp can force smaller modules.
dbpNameCountThreshold = 10000

parseDbpSpec :: String -> Maybe DbpRequest
parseDbpSpec raw = do
  let (modPart0, seedPart0) = break (== ':') raw
      modPart = map (\c -> if c == '/' then '.' else c) modPart0
      modPieces = filter (not . null) (splitChar '.' modPart)
  guard (not (null modPieces))
  let seedPart = case seedPart0 of
        ':' : rest -> rest
        _ -> ""
      seeds = Data.Set.fromList
        [ A.Name NoLoc s
        | s <- splitChar ',' seedPart
        , not (null s)
        ]
  return DbpRequest { drMod = A.modName modPieces, drSeeds = seeds }

parseDbpRequests :: C.CompileOptions -> M.Map A.ModName (Data.Set.Set A.Name)
parseDbpRequests opts =
  M.fromListWith Data.Set.union
    [ (drMod req, drSeeds req)
    | raw <- C.dbp opts
    , Just req <- [parseDbpSpec raw]
    ]

splitChar :: Char -> String -> [String]
splitChar sep = go
  where
    go "" = [""]
    go s =
      let (x, rest) = break (== sep) s
      in case rest of
           [] -> [x]
           _ : ys -> x : go ys

dbpDeferredBackJob :: Bool
                   -> C.CompileOptions
                   -> Paths
                   -> A.ModName
                   -> B.ByteString
                   -> [InterfaceFiles.NameHashInfo]
                   -> Maybe DeferredBackJob
dbpDeferredBackJob blocked opts paths mn moduleImplHash nameHashes
  | C.no_dbp opts = Nothing
  | C.only_build opts = Nothing
  | altOutput opts = Nothing
  | mn == A.modName ["__builtin__"] = Nothing
  | blocked = Nothing
  | forced || nameCount >= dbpNameCountThreshold =
      Just DeferredBackJob
        { dbjPaths = paths
        , dbjOpts = opts
        , dbjMod = mn
        , dbjImplHash = moduleImplHash
        , dbjNameCount = nameCount
        , dbjSeeds = seeds
        , dbjReason = reason
        , dbjOutputJobs = []
        }
  | otherwise = Nothing
  where
    nameCount = length nameHashes
    reqs = parseDbpRequests opts
    forced = M.member mn reqs
    seeds = M.findWithDefault Data.Set.empty mn reqs
    reason
      | forced = "forced by --dbp"
      | otherwise = "name count " ++ show nameCount ++ " >= " ++ show dbpNameCountThreshold

interestDepsFromNameHashes :: [InterfaceFiles.NameHashInfo] -> Data.Set.Set (A.ModName, A.Name)
interestDepsFromNameHashes nameHashes =
  Data.Set.fromList
    [ target
    | nh <- nameHashes
    , (qn, _) <- InterfaceFiles.nhPubDeps nh ++ InterfaceFiles.nhImplDeps nh
    , Just target <- [interestTarget qn]
    ]
  where
    interestTarget qn =
      case qn of
        A.GName m n -> Just (m, n)
        A.QName m n -> Just (m, n)
        A.NoQ{} -> Nothing

addInterestDeps :: Data.Set.Set (A.ModName, A.Name) -> InterestMap -> InterestMap
addInterestDeps deps im =
  foldl'
    (\acc (mn, n) -> M.insertWith Data.Set.union mn (Data.Set.singleton n) acc)
    im
    (Data.Set.toList deps)

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

startDependentFrontOutputJob :: (TaskKey -> FrontOutputKind -> IO ())
                             -> (TaskKey -> FrontOutputKind -> Maybe TimeSpec -> IO ())
                             -> IO Bool
                             -> FrontOutputJob
                             -> TaskKey
                             -> FrontOutputKind
                             -> IO ()
                             -> IO FrontOutputJob
startDependentFrontOutputJob onStart onDone shouldWrite dep key kind action = do
  a <- asyncWithUnmask $ \unmask -> do
         depRes <- unmask (waitCatch (fojAsync dep))
         case depRes of
           Right () -> do
             current <- unmask shouldWrite
             when current $ do
               onStart key kind
               runFrontOutputAction onDone shouldWrite key kind (unmask action)
           Left err
             | isJust (fromException err :: Maybe SomeAsyncException) -> throwIO err
             | otherwise -> return ()
  return FrontOutputJob
    { fojKey = key
    , fojKind = kind
    , fojAsync = a
    }

runFrontOutputAction :: (TaskKey -> FrontOutputKind -> Maybe TimeSpec -> IO ())
                     -> IO Bool
                     -> TaskKey
                     -> FrontOutputKind
                     -> IO ()
                     -> IO ()
runFrontOutputAction onDone shouldWrite key kind action = do
  current <- shouldWrite
  when current $ do
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
                                    , tyRoots :: [A.Name]
                                    , tyTests :: [String]
                                    , tyDoc :: Maybe String
                                    , iface :: I.NameInfo
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
forceHTEnv hte                  = HM.foldl' forceHNameInfo () hte
  where forceHNameInfo () (I.HNModule _ te _) = forceHTEnv te
        forceHNameInfo () hni  = hni `seq` ()

forceModuleHeader :: A.Module -> ()
forceModuleHeader (A.Module m i mdoc _) =
  rnf m `seq` rnf i `seq` rnf mdoc

forceTypeEnv :: Acton.Env.EnvF x -> IO ()
forceTypeEnv typeEnv = do
  evaluate (forceHTEnv (Acton.Env.hnames typeEnv))
  evaluate (forceHTEnv (Acton.Env.hmodules typeEnv))

prepareTypeEnv :: Acton.Env.Env0 -> IO Acton.Env.Env0
prepareTypeEnv env = do
  let typeEnv = Converter.convEnvProtos env
  forceTypeEnv typeEnv
  return typeEnv

forceTypeResult :: I.NameInfo -> A.Module -> IO ()
forceTypeResult (I.NModule imps iface mdoc) tchecked = do
  evaluate (rnf imps)
  evaluate (rnf mdoc)
  forceTEnvParallel iface
  evaluate (forceModuleHeader tchecked)
forceTypeResult nmod tchecked = do
  evaluate (rnf nmod)
  evaluate (forceModuleHeader tchecked)

data FrontTypeFacts = FrontTypeFacts
  { ftfImplBytes :: M.Map A.Name B.ByteString
  , ftfPubSigDeps :: Maybe FrontSplitDeps
  , ftfImplDeps :: FrontDeps
  , ftfImplLocs :: M.Map A.Name SrcLoc
  , ftfStmtEntries :: [B.ByteString]
  }

data FrontDeps
  = FrontDepsRaw (M.Map A.Name [A.QName])
  | FrontDepsSplit FrontSplitDeps

data FrontSplitDeps = FrontSplitDeps
  { fsdLocalDeps :: M.Map A.Name [A.Name]
  , fsdExtDeps :: M.Map A.Name [A.QName]
  }

data FrontTypeFactsResult = FrontTypeFactsResult
  { ftfrFacts :: FrontTypeFacts
  , ftfrImplFactsTime :: TimeSpec
  , ftfrStmtEntriesTime :: TimeSpec
  }

data FrontTypeFactRows = FrontTypeFactRows
  { ftrImplByteRows :: [(A.Name, B.ByteString)]
  , ftrPubSigDepRows :: FrontDepRows
  , ftrImplDepRows :: FrontDepRows
  , ftrImplLocRows :: [(A.Name, SrcLoc)]
  , ftrStmtEntryRows :: [B.ByteString]
  }

data FrontDepRows = FrontDepRows
  { fdrOwners :: [A.Name]
  , fdrLocalRows :: [(A.Name, A.Name)]
  , fdrExtRows :: [(A.Name, A.QName)]
  }

frontTypeFactsFromStmts :: [A.Stmt] -> IO FrontTypeFacts
frontTypeFactsFromStmts ss = do
  let items = concatMap Hashing.topLevelStmtItems ss
      facts =
        FrontTypeFacts
          { ftfImplBytes = Hashing.nameBytesFromItems items
          , ftfPubSigDeps = Nothing
          , ftfImplDeps = FrontDepsRaw (Hashing.implDepsFromItems items)
          , ftfImplLocs = Hashing.nameLocsFromItems items
          , ftfStmtEntries = map InterfaceFiles.encodeStmtEntry ss
          }
  evaluate (rnfFrontTypeFacts facts)
  return facts

frontTypeFactRowsFromStmts :: A.ModName
                           -> Acton.Env.Env0
                           -> Data.Set.Set A.Name
                           -> I.TEnv
                           -> [A.Stmt]
                           -> IO FrontTypeFactRows
frontTypeFactRowsFromStmts mn env sourceLocalNames te ss = do
  let items = concatMap Hashing.topLevelStmtItems ss
      pubSigDeps = M.fromList
        [ (n, dropDerivedQNames (Names.freeQ info)) | (n, info) <- te ]
      implDeps = Hashing.implDepsFromItems items
      rows = FrontTypeFactRows
        { ftrImplByteRows = M.toList (Hashing.nameBytesFromItems items)
        , ftrPubSigDepRows = splitDepRows mn env sourceLocalNames pubSigDeps
        , ftrImplDepRows = splitDepRows mn env sourceLocalNames implDeps
        , ftrImplLocRows = M.toList (Hashing.nameLocsFromItems items)
        , ftrStmtEntryRows = map InterfaceFiles.encodeStmtEntry ss
        }
  evaluate (rnfFrontTypeFactRows rows)
  return rows

frontTypeFactsFromChecked :: Data.Set.Set A.Name -> IORef (IM.IntMap FrontTypeFactRows) -> IO FrontTypeFactsResult
frontTypeFactsFromChecked sourceLocalNames factsRef = do
  timeStart <- getTime Monotonic
  rowsByIx <- readIORef factsRef
  let facts = frontTypeFactsFromRows sourceLocalNames (IM.elems rowsByIx)
  evaluate (rnfFrontTypeFacts facts)
  timeDone <- getTime Monotonic
  return FrontTypeFactsResult
    { ftfrFacts = facts
    , ftfrImplFactsTime = timeDone - timeStart
    , ftfrStmtEntriesTime = TimeSpec 0 0
    }

frontTypeFactsFromRows :: Data.Set.Set A.Name -> [FrontTypeFactRows] -> FrontTypeFacts
frontTypeFactsFromRows sourceLocalNames rows =
  let implBytes = foldl' addByte M.empty (concatMap ftrImplByteRows rows)
      implLocs = foldl' addLoc M.empty (concatMap ftrImplLocRows rows)
      localNames = Data.Set.union sourceLocalNames (M.keysSet implBytes)
  in
    FrontTypeFacts
      { ftfImplBytes = implBytes
      , ftfPubSigDeps = Just (frontSplitDepsFromLastRows localNames (map ftrPubSigDepRows rows))
      , ftfImplDeps = FrontDepsSplit (frontSplitDepsFromRows localNames (map ftrImplDepRows rows))
      , ftfImplLocs = implLocs
      , ftfStmtEntries = concatMap ftrStmtEntryRows rows
      }
  where
    addByte acc (n, bs) =
      M.insertWith (\new old -> old `B.append` B.cons '\n' new) n bs acc
    addLoc acc (n, loc) =
      M.insertWith (\new _ -> new) n loc acc

splitDepRows :: A.ModName
             -> Acton.Env.Env0
             -> Data.Set.Set A.Name
             -> M.Map A.Name [A.QName]
             -> FrontDepRows
splitDepRows mn env localNames depMap =
  M.foldlWithKey' splitOwner emptyRows depMap
  where
    emptyRows = FrontDepRows (M.keys depMap) [] []
    splitOwner rows owner qns = foldl' (splitQName owner) rows qns
    splitQName owner rows (A.NoQ n)
      | Data.Set.member n localNames = addLocal owner n rows
    splitQName owner rows qn =
      case Acton.Env.unalias env qn of
        A.GName m _ | m == mPrim -> rows
        A.QName m _ | m == mPrim -> rows
        A.GName m n -> splitQualified owner m n rows
        A.QName m n -> splitQualified owner m n rows
        A.NoQ n
          | Data.Set.member n localNames -> addLocal owner n rows
          | otherwise -> rows
    splitQualified owner m n rows
      | m == mn && Data.Set.member n localNames = addLocal owner n rows
      | m == mn = rows
      | otherwise = addExternal owner (A.GName m n) rows
    addLocal owner dep rows =
      rows { fdrLocalRows = (owner, dep) : fdrLocalRows rows }
    addExternal owner dep rows =
      rows { fdrExtRows = (owner, dep) : fdrExtRows rows }

frontSplitDepsFromRows :: Data.Set.Set A.Name -> [FrontDepRows] -> FrontSplitDeps
frontSplitDepsFromRows _localNames rows =
  let owners = Data.Set.fromList (concatMap fdrOwners rows)
      emptySets = M.fromSet (const Data.Set.empty) owners
      localSets0 = foldl' addLocal emptySets (concatMap fdrLocalRows rows)
      extSets = foldl' addExternal emptySets (concatMap fdrExtRows rows)
  in FrontSplitDeps
       { fsdLocalDeps = M.map Data.Set.toList localSets0
       , fsdExtDeps = M.map Data.Set.toList extSets
       }
  where
    addLocal acc (owner, dep) =
      M.insertWith Data.Set.union owner (Data.Set.singleton dep) acc
    addExternal acc (owner, dep) =
      M.insertWith Data.Set.union owner (Data.Set.singleton dep) acc

frontSplitDepsFromLastRows :: Data.Set.Set A.Name -> [FrontDepRows] -> FrontSplitDeps
frontSplitDepsFromLastRows localNames =
  foldl' add FrontSplitDeps { fsdLocalDeps = M.empty, fsdExtDeps = M.empty }
  where
    add acc rows =
      let deps = frontSplitDepsFromRows localNames [rows]
      in FrontSplitDeps
           { fsdLocalDeps = fsdLocalDeps deps `M.union` fsdLocalDeps acc
           , fsdExtDeps = fsdExtDeps deps `M.union` fsdExtDeps acc
           }

isDerivedName :: A.Name -> Bool
isDerivedName n = case n of
  A.Derived{} -> True
  _ -> False

isDerivedQName :: A.QName -> Bool
isDerivedQName qn = case qn of
  A.GName _ n -> isDerivedName n
  A.QName _ n -> isDerivedName n
  A.NoQ n -> isDerivedName n

dropDerivedQNames :: [A.QName] -> [A.QName]
dropDerivedQNames =
  filter (not . isDerivedQName)

sourceLocalNamesFromModule :: A.Module -> Data.Set.Set A.Name
sourceLocalNamesFromModule =
  Data.Set.fromList . map itemName . Hashing.topLevelItems
  where
    itemName item = case item of
      Hashing.TLDecl n _ -> n
      Hashing.TLStmt n _ -> n

sourceNameFactsFromModule :: A.Module -> IO (M.Map A.Name B.ByteString, M.Map A.Name SrcLoc)
sourceNameFactsFromModule (A.Module _ _ _ ss) = do
  chunks <- frontFactChunks ss
  parts <- mapConcurrently sourceFactsFromChunk chunks
  let (nameBytes, nameLocs) = foldl' mergeSourceFacts (M.empty, M.empty) parts
  nameHashes <- nameHashesFromBytesParallel nameBytes
  evaluate (rnf (nameHashes, nameLocs))
  return (nameHashes, nameLocs)
  where
    sourceFactsFromChunk chunk =
      let items = concatMap Hashing.topLevelStmtItems chunk
          facts =
            ( Hashing.nameBytesFromItems items
            , Hashing.nameLocsFromItems items
            )
      in evaluate (rnf facts) >> return facts
    mergeSourceFacts (bytesA, locsA) (bytesB, locsB) =
      ( Hashing.mergeNameBytes bytesA bytesB
      , M.union locsA locsB
      )

frontTypeFactsFromModule :: A.Module -> IO FrontTypeFactsResult
frontTypeFactsFromModule (A.Module _ _ _ ss) = do
  chunks <- frontFactChunks ss
  withAsync (implFacts chunks) $ \implAsync ->
    withAsync (stmtEntries chunks) $ \stmtAsync -> do
      (implBytes, implDeps, implLocs, implTime) <- wait implAsync
      (entries, stmtTime) <- wait stmtAsync
      let facts = FrontTypeFacts
            { ftfImplBytes = implBytes
            , ftfPubSigDeps = Nothing
            , ftfImplDeps = FrontDepsRaw implDeps
            , ftfImplLocs = implLocs
            , ftfStmtEntries = entries
            }
      evaluate (rnfFrontTypeFacts facts)
      return FrontTypeFactsResult
        { ftfrFacts = facts
        , ftfrImplFactsTime = implTime
        , ftfrStmtEntriesTime = stmtTime
        }
  where implFacts chunks = do
          timeStart <- getTime Monotonic
          parts <- mapConcurrently implFactsFromChunk chunks
          let facts = foldl' mergeImplFacts (M.empty, M.empty, M.empty) parts
          evaluate (rnf facts)
          timeDone <- getTime Monotonic
          let (implBytes, implDeps, implLocs) = facts
          return (implBytes, implDeps, implLocs, timeDone - timeStart)
        stmtEntries chunks = do
          timeStart <- getTime Monotonic
          entries <- concat <$> mapConcurrently encodeStmtChunk chunks
          evaluate (rnf entries)
          timeDone <- getTime Monotonic
          return (entries, timeDone - timeStart)
        implFactsFromChunk chunk =
          let items = concatMap Hashing.topLevelStmtItems chunk
              facts =
                ( Hashing.nameBytesFromItems items
                , Hashing.implDepsFromItems items
                , Hashing.nameLocsFromItems items
                )
          in evaluate (rnf facts) >> return facts
        mergeImplFacts (bytesA, depsA, locsA) (bytesB, depsB, locsB) =
          ( Hashing.mergeNameBytes bytesA bytesB
          , M.unionWith (flip (++)) depsA depsB
          , M.union locsA locsB
          )
        encodeStmtChunk chunk = do
          let entries = map InterfaceFiles.encodeStmtEntry chunk
          evaluate (rnf entries)
          return entries

frontFactChunks :: [a] -> IO [[a]]
frontFactChunks xs = do
  ncap <- getNumCapabilities
  let len = length xs
      workers = max 1 (min ncap len)
      chunkSize = max 1 ((len + workers - 1) `div` workers)
  return (chunksOf chunkSize xs)

nameHashesFromBytesParallel :: M.Map A.Name B.ByteString -> IO (M.Map A.Name B.ByteString)
nameHashesFromBytesParallel bytes = do
  chunks <- frontFactChunks (M.toList bytes)
  parts <- mapConcurrently hashChunk chunks
  let hashes = M.fromList (concat parts)
  evaluate (rnf hashes)
  return hashes
  where hashChunk chunk = do
          let hashes = [ (n, SHA256.hash bs) | (n, bs) <- chunk ]
          evaluate (rnf hashes)
          return hashes

rnfFrontTypeFacts :: FrontTypeFacts -> ()
rnfFrontTypeFacts facts =
  rnf (ftfImplBytes facts) `seq`
  rnfMaybeFrontSplitDeps (ftfPubSigDeps facts) `seq`
  rnfFrontDeps (ftfImplDeps facts) `seq`
  rnf (ftfImplLocs facts) `seq`
  rnf (ftfStmtEntries facts)

rnfFrontTypeFactRows :: FrontTypeFactRows -> ()
rnfFrontTypeFactRows rows =
  rnf (ftrImplByteRows rows) `seq`
  rnfFrontDepRows (ftrPubSigDepRows rows) `seq`
  rnfFrontDepRows (ftrImplDepRows rows) `seq`
  rnf (ftrImplLocRows rows) `seq`
  rnf (ftrStmtEntryRows rows)

rnfMaybeFrontSplitDeps :: Maybe FrontSplitDeps -> ()
rnfMaybeFrontSplitDeps Nothing = ()
rnfMaybeFrontSplitDeps (Just deps) = rnfFrontSplitDeps deps

rnfFrontDeps :: FrontDeps -> ()
rnfFrontDeps (FrontDepsRaw deps) = rnf deps
rnfFrontDeps (FrontDepsSplit deps) = rnfFrontSplitDeps deps

rnfFrontSplitDeps :: FrontSplitDeps -> ()
rnfFrontSplitDeps deps =
  rnf (fsdLocalDeps deps) `seq`
  rnf (fsdExtDeps deps)

rnfFrontDepRows :: FrontDepRows -> ()
rnfFrontDepRows rows =
  rnf (fdrOwners rows) `seq`
  rnf (fdrLocalRows rows) `seq`
  rnf (fdrExtRows rows)

forceTEnvParallel :: [(A.Name, I.NameInfo)] -> IO ()
forceTEnvParallel [] = return ()
forceTEnvParallel te = do
  ncap <- getNumCapabilities
  let len = length te
      workers = max 1 (min ncap len)
      chunkSize = max 1 ((len + workers - 1) `div` workers)
  if workers == 1 || len < 2 * workers
    then evaluate (rnf te)
    else mapConcurrently_ (evaluate . rnf) (chunksOf chunkSize te)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
  let (chunk, rest) = splitAt n xs
  in chunk : chunksOf n rest

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


-- | Resolve imports to in-graph providers using project search order.
-- This chooses the first project in the search order that declares the module,
-- producing TaskKeys for dependency edges.
resolveProviders :: [FilePath] -> M.Map FilePath (Data.Set.Set A.ModName) -> [A.ModName] -> M.Map A.ModName TaskKey
resolveProviders order modSets imps =
    M.fromList $ catMaybes $ map (\mn -> fmap (\p -> (mn, TaskKey p mn)) (findProvider mn)) imps
  where
    findProvider mn = listToMaybe [ p | p <- order, maybe False (Data.Set.member mn) (M.lookup p modSets) ]


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
    perProj <- forM (M.elems projMap) $ \ctx -> do
                  mods <- enumerateProjectModules ctx
                  return (ctx, mods)
    let modMaps = M.fromList [ (projRoot ctx, M.fromList [ (mn, actFile) | (actFile, mn) <- mods ]) | (ctx, mods) <- perProj ]
        modSets = M.map Data.Set.fromList (M.map M.keys modMaps)
        orderCache = M.fromList [ (projRoot ctx, projRoot ctx : projDepClosure projMap (projRoot ctx)) | (ctx, _) <- perProj ]
        allKeys = [ TaskKey (projRoot ctx) mn | (ctx, mods) <- perProj, (_, mn) <- mods ]
    seedKeys <- case mSeeds of
                  Nothing -> return allKeys
                  Just files -> do
                    absFiles <- mapM canonicalizePath files
                    let pathIndex = M.fromList [ (actFile, TaskKey (projRoot ctx) mn) | (ctx, mods) <- perProj, (actFile, mn) <- mods ]
                        found = mapMaybe (`M.lookup` pathIndex) absFiles
                    return (if null found then allKeys else found)
    tasks <- go modMaps modSets orderCache Data.Set.empty seedKeys []
    return (reverse tasks, modSets)
  where
    go modMaps modSets orderCache seen [] acc = return acc
    go modMaps modSets orderCache seen (k:qs) acc
      | Data.Set.member k seen = go modMaps modSets orderCache seen qs acc
      | otherwise =
          case M.lookup (tkProj k) modMaps >>= M.lookup (tkMod k) of
            Nothing -> go modMaps modSets orderCache (Data.Set.insert k seen) qs acc
            Just actFile -> do
              let ctx = projMap M.! tkProj k
              paths <- pathsForModule opts projMap ctx (tkMod k)
              task  <- readModuleTask sp gopts opts paths actFile
              let order = M.findWithDefault [tkProj k] (tkProj k) orderCache
                  providers = resolveProviders order modSets (importsOf task)
                  newKeys = M.elems providers
                  acc' = GlobalTask { gtKey = k
                                    , gtPaths = paths
                                    , gtTask = task
                                    , gtImportProviders = providers
                                    } : acc
              go modMaps modSets orderCache (Data.Set.insert k seen) (qs ++ newKeys) acc'


-- | Select the subgraph needed for a given build request.
-- Maps file paths to TaskKeys, adds __builtin__, and computes the reachable
-- dependency closure.
selectNeededTasks :: Paths -> FilePath -> [GlobalTask] -> [FilePath] -> IO [GlobalTask]
selectNeededTasks pathsRoot rootProj globalTasks srcFiles = do
    requestedKeys <- catMaybes <$> mapM (lookupTaskKey globalTasks) srcFiles
    let wantedNames   = map takeFileName srcFiles
        requestedKeys' = if null requestedKeys
                           then [ gtKey t
                                | t <- globalTasks
                                , takeFileName (srcFile (gtPaths t) (tkMod (gtKey t))) `elem` wantedNames
                                ]
                           else requestedKeys
        builtinKeys = [ gtKey t | t <- globalTasks, tkMod (gtKey t) == A.modName ["__builtin__"] ]
        startKeys   = if null requestedKeys' then map gtKey globalTasks else requestedKeys' ++ builtinKeys
        depMapSet   = M.fromList [ (gtKey t, Data.Set.fromList (M.elems (gtImportProviders t))) | t <- globalTasks ]
        neededKeys  = reachable depMapSet (Data.Set.fromList startKeys)
    return [ t | t <- globalTasks, Data.Set.member (gtKey t) neededKeys ]
  where
    lookupTaskKey ts f = do
      absF <- canonicalizePath f
      let byPath = listToMaybe [ gtKey t
                               | t <- ts
                               , let k = gtKey t
                                     pths = gtPaths t
                               , srcFile pths (tkMod k) == absF
                               ]
      case byPath of
        Just k -> return (Just k)
        Nothing -> do
          mn <- moduleNameFromFile (srcDir pathsRoot) absF
          return $ listToMaybe [ gtKey t
                               | t <- ts
                               , tkProj (gtKey t) == rootProj
                               , tkMod (gtKey t) == mn
                               ]

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
            TyTask{ tyImplHash = implHash, tyNameHashes = nhs } ->
              isJust (dbpDeferredBackJob (Data.Set.member k dbpBlocked) (optsFor k) (gtPaths t) (tkMod k) implHash nhs)
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

-- | DBP may prune generated C/H for internal library modules, but a module at
-- an explicit library boundary must keep its full generated surface.
libraryBoundaryTasks :: M.Map FilePath ProjCtx -> [GlobalTask] -> Data.Set.Set TaskKey
libraryBoundaryTasks projMap globalTasks =
    Data.Set.unions (map boundaryFor libraryGroups)
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

    boundaryFor group =
      Data.Set.filter hasOutsideConsumer group
      where
        hasOutsideConsumer k =
          any (`Data.Set.notMember` group) (M.findWithDefault [] k revMap)

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
                 , let mn = moduleStringToName modName
                 , t <- globalTasks
                 , tkMod (gtKey t) == mn
                 , taskInProject ctx t
                 ]
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
        hdrE <- (try :: IO a -> IO (Either SomeException a)) $ InterfaceFiles.readHeader tyFile
        case hdrE of
          Left _ -> readSourceHead mn
          Right (cachedSourceMeta, moduleSrcBytesHash, modulePubHash, moduleImplHash, imps, depModules, nameHashes, roots, tests, mdoc) -> do
            tyMTimeNs <- InterfaceFiles.interfaceModifiedTimeNs tyFile
            newCompiler <- compilerNewerThan tyMTimeNs
            mOverlay <- Source.spReadOverlay sp actFile
            case mOverlay of
              Just snap ->
                if newCompiler
                  then sourceHeadFromSnapshot mn snap
                  else verifyOrParse mn snap moduleSrcBytesHash modulePubHash moduleImplHash cachedSourceMeta Nothing imps depModules nameHashes roots tests mdoc
              Nothing -> do
                if newCompiler
                  then readSourceHead mn
                  else do
                    currentSourceMeta <- readSourceFileMeta actFile
                    if canReuseHeader cachedSourceMeta currentSourceMeta tyMTimeNs
                      then return (mkTyHead mn moduleSrcBytesHash modulePubHash moduleImplHash imps depModules nameHashes roots tests mdoc)
                      else do
                        snap <- Source.spReadFile sp actFile
                        verifyOrParse mn snap moduleSrcBytesHash modulePubHash moduleImplHash cachedSourceMeta (Just currentSourceMeta) imps depModules nameHashes roots tests mdoc
  where
    fileStatusMTimeNs st = floor (toRational (modificationTimeHiRes st) * 1000000000)

    mkTyHead mn moduleSrcBytesHash modulePubHash moduleImplHash imps depModules nameHashes roots tests mdoc =
      TyHead
        { mhName       = mn
        , mhSrcHash    = moduleSrcBytesHash
        , mhPubHash    = modulePubHash
        , mhImplHash   = moduleImplHash
        , mhTyImports  = imps
        , mhDepModules = depModules
        , mhNameHashes = nameHashes
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

    refreshCachedSourceMeta currentSourceMeta = do
      let tyFilePath = tyDbPath paths (modName paths)
      tyRes <- (try :: IO a -> IO (Either SomeException a)) $ InterfaceFiles.readFile tyFilePath
      case tyRes of
        Left _ -> return ()
        Right (_ms, nmod, tmod, _oldSourceMeta, srcHash, pubHash, implHash, imps, depModules, nameHashes, roots, tests, mdoc) ->
          InterfaceFiles.writeFile tyFilePath srcHash pubHash implHash currentSourceMeta imps depModules nameHashes roots tests mdoc nmod tmod

    verifyOrParse mn snap moduleSrcBytesHash modulePubHash moduleImplHash cachedSourceMeta currentSourceMeta imps depModules nameHashes roots tests mdoc = do
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
          return (mkTyHead mn moduleSrcBytesHash modulePubHash moduleImplHash imps depModules nameHashes roots tests mdoc)
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

-- | Materialize a source-backed task into a fully parsed ActonTask.
-- ParseTask reuses the snapshot captured during graph discovery; TyTask reparses
-- from the current source file because the cached header was deemed stale.
materializeTask :: C.CompileOptions
                -> Source.SourceProvider
                -> A.ModName
                -> FilePath
                -> Maybe (ParseProgress -> IO ())
                -> CompileTask
                -> IO CompileTask
materializeTask opts sp mn actFile mOnProgress task =
  case task of
    ActonTask{} -> return task
    ParseErrorTask{} -> return task
    ParseTask{ src = srcContent, srcBytes = bytes, sourceMeta = mSourceMeta } -> do
      emod <- parseStoredSource mn actFile srcContent
      case emod of
        Left diags -> return (ParseErrorTask mn diags)
        Right m -> return (ActonTask mn srcContent bytes mSourceMeta m)
    TyTask{} -> do
      parsedRes <- parseActFile opts sp mn actFile mOnProgress
      case parsedRes of
        Left diags -> return (ParseErrorTask mn diags)
        Right (snap, m) -> do
          mSourceMeta <- if Source.ssIsOverlay snap then return Nothing else Just <$> readSourceFileMeta actFile
          return $ ActonTask mn (Source.ssText snap) (Source.ssBytes snap) mSourceMeta m
  where
    parseStoredSource mn' file srcContent = do
      cwd <- getCurrentDirectory
      let displayFile = makeRelative cwd file
      parseActSource opts mn' displayFile srcContent mOnProgress


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

-- | Read an interface from a .tydb file and return its NameInfo and module hashes.
-- This is used when a module is deemed fresh and we want to avoid reparsing.
readIfaceFromTy :: Paths -> A.ModName -> String -> Maybe B.ByteString -> IO (Either [Diagnostic String] ([A.ModName], [(A.Name, I.NameInfo)], Maybe String, B.ByteString, B.ByteString))
readIfaceFromTy paths mn src mHash = do
    mty <- Acton.Env.findTyFile (searchPath paths) mn
    case mty of
      Nothing -> return $ Left (missingIfaceDiagnostics mn src mn)
      Just tyF -> do
        fileRes <- InterfaceFiles.readFileMaybe tyF
        case fileRes of
          Nothing -> return $ Left (missingIfaceDiagnostics mn src mn)
          Just (_ms, nmod, _tmod, _sourceMeta, _srcH, pubH, implH, _imps, _depModules, _nameHashes, _roots, _tests, _tm) -> do
            let I.NModule ms teFull mdoc = nmod
                te = publicIfaceTE teFull
            ih <- case mHash of
                    Just h -> return h
                    Nothing -> return pubH
            return $ Right (ms, te, mdoc, ih, implH)


-- | Snapshot of expected/recorded impl hashes for generated code.
data CodegenStatus = CodegenStatus
  { csExpected :: String
  , csC :: Maybe String
  , csH :: Maybe String
  }

-- | Header prefix used to tag generated code with the module impl hash.
codegenHashTag :: String
codegenHashTag = "/* Acton impl hash:"

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
codegenStatus paths mn implHash = do
  let expected = B.unpack $ Base16.encode implHash
      cFile = outBase paths mn ++ ".c"
      hFile = outBase paths mn ++ ".h"
  cHash <- readCodegenHash cFile
  hHash <- readCodegenHash hFile
  return CodegenStatus { csExpected = expected, csC = cHash, csH = hHash }

-- | Check whether generated .c/.h hashes match the expected impl hash.
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
         " {impl " ++ take 8 c ++ " -> " ++ expected ++ "}"
       _ ->
         " {impl c " ++ short (csC status) ++ " -> " ++ expected
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
               -> Paths
               -> Acton.Env.Env0
               -> A.Module
               -> String
               -> B.ByteString
               -> Maybe InterfaceFiles.SourceFileMeta
               -> (A.ModName -> IO (Maybe B.ByteString))
               -> (A.ModName -> IO (Maybe B.ByteString))
               -> (A.ModName -> IO (Maybe (M.Map A.Name InterfaceFiles.NameHashInfo)))
               -> (FrontPassProgress -> IO ())
               -> (TaskKey -> FrontOutputKind -> IO ())
               -> (TaskKey -> FrontOutputKind -> Maybe TimeSpec -> IO ())
               -> IO Bool
               -> ([FrontOutputJob] -> IO ())
               -> IO (Either [Diagnostic String] FrontResult)
runFrontPasses gopts opts dbpBlocked paths env0 parsed srcContent srcBytes sourceMeta resolveImportHash resolveImportImplHash resolveNameHashMap onFrontProgress onFrontOutputStart onFrontOutputDone shouldWriteFrontOutput recordFrontOutputJobs = do
  createDirectoryIfMissing True (getModPath (projTypes paths) mn)
  core
    `catch` handleGeneral
    `catch` handleCompilation
    `catch` handleTypeErrors
    `catch` handleTypeError
  where
    mn = A.modname parsed
    filename = modNameToFilename mn
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

    missingDepHashDiagnostics :: String -> A.Name -> SrcLoc -> A.QName -> [Diagnostic String]
    missingDepHashDiagnostics label owner loc qn =
      errsToDiagnostics "Compilation error" filename srcContent
        [(loc, label ++ " hash missing for " ++ prstr qn ++ " (used by " ++ A.nstr owner ++ ")")]

    resolveNameHashMaps :: [A.ModName] -> IO (Either [Diagnostic String] (M.Map A.ModName (M.Map A.Name InterfaceFiles.NameHashInfo)))
    resolveNameHashMaps mrefs = do
      resolved <- forM mrefs $ \mref -> do
        mh <- resolveNameHashMap mref
        case mh of
          Just hm -> return (Right (mref, hm))
          Nothing -> return (Left (missingIfaceDiagnostics mn srcContent mref))
      let (errs, vals) = partitionEithers resolved
      if null errs
        then return (Right (M.fromList vals))
        else return (Left (concat errs))

    resolveDepHashes :: String
                     -> (InterfaceFiles.NameHashInfo -> B.ByteString)
                     -> M.Map A.Name [A.QName]
                     -> M.Map A.ModName (M.Map A.Name InterfaceFiles.NameHashInfo)
                     -> M.Map A.Name SrcLoc
                     -> Either [Diagnostic String] (M.Map A.Name [(A.QName, B.ByteString)])
    resolveDepHashes label getHash deps extMaps nameLocs =
      let resolveQName owner qn = case qn of
            A.GName m n -> lookupName owner m n
            A.QName m n -> lookupName owner m n
            A.NoQ _ -> Left (missingNameHashDiagnostics qn)
          lookupName owner m n =
            case M.lookup m extMaps of
              Nothing -> Left (missingIfaceDiagnostics mn srcContent m)
              Just hm ->
                case M.lookup n hm of
                  Just info ->
                    let h = getHash info
                        loc = M.findWithDefault NoLoc owner nameLocs
                    in if B.null h
                         then Left (missingDepHashDiagnostics label owner loc (A.GName m n))
                         else Right (Just (A.GName m n, h))
                  Nothing -> Left (missingNameHashDiagnostics (A.GName m n))
          resolveForName (n, qns) =
            let qnsSorted = Data.List.sortOn Hashing.qnameKey (Data.Set.toList (Data.Set.fromList qns))
                resolved = map (resolveQName n) qnsSorted
                (errs, vals) = partitionEithers resolved
            in if null errs then Right (n, catMaybes vals) else Left (concat errs)
          (errs, vals) = partitionEithers (map resolveForName (M.toList deps))
      in if null errs then Right (M.fromList vals) else Left (concat errs)

    emitFrontProgress pass completed total current =
      onFrontProgress FrontPassProgress
        { fppPass = pass
        , fppCompleted = completed
        , fppTotal = total
        , fppCurrent = current
        }

    sourceHashFacts = do
      timeStart <- getTime Monotonic
      (moduleSrcBytesHash, (nameSrcHashes, nameLocsParsed)) <-
        withAsync (evaluate (SHA256.hash srcBytes)) $ \moduleHashAsync ->
          withAsync (sourceNameFactsFromModule parsed) $ \nameFactsAsync -> do
            moduleSrcBytesHash <- wait moduleHashAsync
            nameFacts <- wait nameFactsAsync
            return (moduleSrcBytesHash, nameFacts)
      timeDone <- getTime Monotonic
      return ((moduleSrcBytesHash, nameSrcHashes, nameLocsParsed), timeDone - timeStart)

    core = do
      timeStart <- getTime Monotonic
      let isRoot = mn == modName paths
      when (C.parse opts && isRoot) $
        dump mn "parse" (Pretty.print parsed)
      when (C.parse_ast opts && isRoot) $
        dump mn "parse-ast" (renderStyle prettyAstStyle (ppDoc parsed))
      withAsync sourceHashFacts (coreWithSourceFacts timeStart isRoot)

    coreWithSourceFacts timeStart isRoot sourceHashFactsAsync = do
      typeStmtTimingsRef <- newIORef ([] :: [TypeStmtTiming])
      typeFactsRef <- newIORef IM.empty
      inferredSigsRef <- newIORef ([] :: [InferredSignature])
      typeActiveRef <- newIORef Nothing
      typeProgressDoneRef <- newIORef Nothing
      let collectTypeStmtTimings = C.timing gopts && C.verbose gopts
      let onTypeProgress total completed current names _weight = do
            when (total > 0 && completed >= total && isNothing current) $ do
              mDone <- readIORef typeProgressDoneRef
              when (isNothing mDone) $ do
                now <- getTime Monotonic
                writeIORef typeProgressDoneRef (Just now)
            when collectTypeStmtTimings $ do
              now <- getTime Monotonic
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
            emitFrontProgress FrontPassTypes completed total current
          onInferredSignature names sig =
            modifyIORef' inferredSigsRef (InferredSignature names sig :)
          inferredSignatureCb =
            if C.timing gopts || C.verbose gopts
              then Just onInferredSignature
              else Nothing

      env <- Acton.Env.mkEnv (searchPath paths) env0 parsed
      timeEnv <- getTime Monotonic
      let hashEnv = setMod mn env
          sourceLocalNames = sourceLocalNamesFromModule parsed
          onTypeChecked ix te ss = do
            rows <- frontTypeFactRowsFromStmts mn hashEnv sourceLocalNames te ss
            atomicModifyIORef' typeFactsRef $ \rowsByIx ->
              (IM.insert ix rows rowsByIx, ())

      emitFrontProgress FrontPassKinds 0 1 Nothing
      kchecked <- Acton.Kinds.check env parsed
      emitFrontProgress FrontPassKinds 1 1 Nothing
      iff (C.kinds opts && isRoot) $ dump mn "kinds" (Pretty.print kchecked)
      timeKindsCheck <- getTime Monotonic

      -- Type-check first; compile-side finalization derives output facts from
      -- the typed module without making Types.reconstruct a hashing API.
      typeEnvAsync <- async (prepareTypeEnv env)
      (nmod,tchecked,_typeEnv,tests) <-
        Acton.Types.reconstruct (Just onTypeProgress) inferredSignatureCb (Just onTypeChecked) env kchecked
          `onException` cancel typeEnvAsync
      timeTypeReconstruct <- getTime Monotonic
      (timeTypeForce, typeEnv) <-
        ( do forceTypeResult nmod tchecked
             typeEnv <- wait typeEnvAsync
             timeTypeForce <- getTime Monotonic
             return (timeTypeForce, typeEnv)
        ) `onException` cancel typeEnvAsync
      typeFactsResult <-
        if null tests
          then frontTypeFactsFromChecked sourceLocalNames typeFactsRef
          else frontTypeFactsFromModule tchecked
      let typeFacts = ftfrFacts typeFactsResult
      ((moduleSrcBytesHash, nameSrcHashes, nameLocsParsed), sourceFactsTime) <- wait sourceHashFactsAsync
      -- Store roots so later builds can discover entry points without reparse.
      let I.NModule imps fullIface mdoc = nmod
          publicIface = publicIfaceTE fullIface
      let roots = [ n | (n,i) <- fullIface, rootEligible i ]
      -- Import hashes are recorded in the .tydb header so dep changes can be detected.
      let hashImps =
            if mn == mBuiltin
              then imps
              else nub (mBuiltin : imps)
      timeImportHashesStart <- getTime Monotonic
      impsRes <- resolveImportHashes hashImps
      timeImportHashesDone <- getTime Monotonic
      case impsRes of
        Left diags -> return (Left diags)
        Right impsWithHash -> do
          -- Source hashes use parsed AST fragments; implementation facts
          -- are derived from the typed module by compile-side finalization.
          timePublicFactsStart <- getTime Monotonic
          let nameInfoMap = M.fromList fullIface
              nameSrcKeys = M.keysSet nameSrcHashes
              nameImplKeys = M.keysSet (ftfImplBytes typeFacts)
              nameKeys = Data.Set.union nameSrcKeys nameImplKeys
              nameLocs = M.union nameLocsParsed (ftfImplLocs typeFacts)
              -- pubSigDeps: signature-level deps from NameInfo (types only).
              pubSigDeps = ftfPubSigDeps typeFacts
              pubSigDepsRaw = case pubSigDeps of
                Just _ -> M.empty
                Nothing ->
                  M.fromList
                    [ (n, dropDerivedQNames (Names.freeQ info)) | (n, info) <- M.toList nameInfoMap ]
              -- implDeps: term-level deps from typed bodies.
              implDeps = ftfImplDeps typeFacts
              implDepsRaw = case implDeps of
                FrontDepsRaw deps -> deps
                FrontDepsSplit _ -> M.empty
          evaluate (rnf (nameKeys, nameLocs, pubSigDepsRaw, implDepsRaw))
          timePublicFactsDone <- getTime Monotonic

          timeDepSplitStart <- getTime Monotonic
          let (pubSigLocalDeps, pubSigExtDeps) =
                case pubSigDeps of
                  Just deps -> (fsdLocalDeps deps, fsdExtDeps deps)
                  Nothing -> Hashing.splitDeps mn hashEnv nameKeys pubSigDepsRaw
              (implLocalDeps, implExtDeps) =
                case implDeps of
                  FrontDepsRaw deps -> Hashing.splitDeps mn hashEnv nameKeys deps
                  FrontDepsSplit deps -> (fsdLocalDeps deps, fsdExtDeps deps)
              -- pub deps include signature deps plus term-level deps for reuse
              -- in stale checks. Derived names are internal and should never
              -- require a public hash.
              pubLocalDeps = M.unionWith (++) pubSigLocalDeps (M.map (filter (not . isDerivedName)) implLocalDeps)
              pubExtDeps = M.unionWith (++) pubSigExtDeps (M.map (filter (not . isDerivedQName)) implExtDeps)
              -- Load .tydb maps for any external modules referenced by deps.
              extMods =
                Data.Set.toList
                  (Hashing.externalModules pubExtDeps `Data.Set.union` Hashing.externalModules implExtDeps)
          evaluate (rnf (pubSigLocalDeps, pubSigExtDeps, pubLocalDeps, pubExtDeps, implLocalDeps, implExtDeps, extMods))
          timeDepSplitDone <- getTime Monotonic

          timeExternalHashesStart <- getTime Monotonic
          extMapsRes <- resolveNameHashMaps extMods
          depModulesRes <- resolveDepModuleHashes extMods
          timeExternalHashesDone <- getTime Monotonic
          case (extMapsRes, depModulesRes) of
            (Left diags, _) -> return (Left diags)
            (_, Left diags) -> return (Left diags)
            (Right extMaps, Right depModules) -> do
              -- Resolve external deps to their recorded hashes.
              timeDepHashesStart <- getTime Monotonic
              let pubSigExtRes = resolveDepHashes "pub" InterfaceFiles.nhPubHash pubSigExtDeps extMaps nameLocs
                  pubExtRes = resolveDepHashes "pub" InterfaceFiles.nhPubHash pubExtDeps extMaps nameLocs
                  implExtRes = resolveDepHashes "impl" InterfaceFiles.nhImplHash implExtDeps extMaps nameLocs
              case (pubSigExtRes, pubExtRes, implExtRes) of
                (Left diags, _, _) -> return (Left diags)
                (_, Left diags, _) -> return (Left diags)
                (_, _, Left diags) -> return (Left diags)
                (Right pubSigExtHashes, Right pubExtHashes, Right implExtHashes) -> do
                  evaluate (rnf (pubSigExtHashes, pubExtHashes, implExtHashes))
                  timeDepHashesDone <- getTime Monotonic

                  -- Build per-name hash records (src/pub/impl + deps) for .tydb.
                  timeNameHashesStart <- getTime Monotonic
                  nameImplHashes <- nameHashesFromBytesParallel (ftfImplBytes typeFacts)
                  let nameHashes =
                        Hashing.buildNameHashes
                          nameKeys
                          nameSrcHashes
                          nameImplHashes
                          nameInfoMap
                          pubSigLocalDeps
                          pubSigExtHashes
                          pubLocalDeps
                          implLocalDeps
                          implExtHashes
                          pubExtHashes
                  evaluate (rnf (nameImplHashes, nameHashes))
                  timeNameHashesDone <- getTime Monotonic

                  timeModuleHashesStart <- getTime Monotonic
                  let modulePubHash = Hashing.modulePubHashFromIface nmod nameHashes
                      moduleImplHash = Hashing.moduleImplHashFromNameHashes nameHashes
                  evaluate (rnf (moduleSrcBytesHash, modulePubHash, moduleImplHash))
                  timeModuleHashesDone <- getTime Monotonic
                  timeTypeHash <- getTime Monotonic

                  iff (C.types opts && isRoot) $ dump mn "types" (Pretty.print tchecked)
                  iff (C.sigs opts && isRoot) $ dump mn "sigs" (Acton.Types.prettySigs env mn imps fullIface)
                  timeTypeCheck <- getTime Monotonic

                  let writeTyDb = do
                        InterfaceFiles.writeFileWithStmtEntries
                          (tyDbPath paths mn)
                          moduleSrcBytesHash
                          modulePubHash
                          moduleImplHash
                          sourceMeta
                          impsWithHash
                          depModules
                          nameHashes
                          roots
                          tests
                          mdoc
                          nmod
                          tchecked
                          (ftfStmtEntries typeFacts)
                      writeDoc = do
                        let docDir = joinPath [projPath paths, "out", "doc"]
                            modPathList = A.modPath mn
                            docFile = if null modPathList
                                      then docDir </> "unnamed" <.> "html"
                                      else joinPath (docDir : init modPathList) </> last modPathList <.> "html"
                            docFileDir = takeDirectory docFile
                            -- Get the type environment for this module
                            modTypeEnv = case Acton.Env.lookupMod mn typeEnv of
                              Just te -> te
                              Nothing -> publicIface
                            -- Apply the same simplification as --sigs uses
                            env1 = define publicIface $ setMod mn env
                            simplifiedTypeEnv = simp env1 modTypeEnv
                        createDirectoryIfMissing True docFileDir
                        -- Use parsed (original AST) to preserve docstrings
                        let htmlDoc = DocP.printHtmlDoc (I.NModule imps simplifiedTypeEnv mdoc) parsed
                        writeFile docFile htmlDoc
                      docOutputActions =
                        if not (C.skip_build opts) && not (isTmp paths)
                          then [(FrontOutputDoc, writeDoc)]
                          else []
                  typeStmtTimings <- reverse <$> readIORef typeStmtTimingsRef
                  typeProgressDone <- readIORef typeProgressDoneRef

                  timeFrontEnd <- getTime Monotonic
                  inferredSigs <- reverse <$> readIORef inferredSigsRef
                  let typeAfterProgress =
                        case typeProgressDone of
                          Just t -> timeTypeReconstruct - t
                          Nothing -> timeTypeReconstruct - timeTypeReconstruct
                      frontTime = timeFrontEnd - timeStart
                      hashTiming = FrontHashTiming
                        { fhtSourceFacts = sourceFactsTime
                        , fhtImplFacts = ftfrImplFactsTime typeFactsResult
                        , fhtPublicFacts = timePublicFactsDone - timePublicFactsStart
                        , fhtDepSplit = timeDepSplitDone - timeDepSplitStart
                        , fhtImportHashes = timeImportHashesDone - timeImportHashesStart
                        , fhtExternalHashes = timeExternalHashesDone - timeExternalHashesStart
                        , fhtDepHashes = timeDepHashesDone - timeDepHashesStart
                        , fhtNameHashes = timeNameHashesDone - timeNameHashesStart
                        , fhtModuleHashes = timeModuleHashesDone - timeModuleHashesStart
                        , fhtStmtEntries = ftfrStmtEntriesTime typeFactsResult
                        }
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
                                 , ftTypeHashDetail = hashTiming
                                 , ftTypeStmtTimings = typeStmtTimings
                                 }
                          else Nothing
                      deferredBackJob = dbpDeferredBackJob dbpBlocked opts paths mn moduleImplHash nameHashes
                      backJob =
                        case deferredBackJob of
                          Just _ -> Nothing
                          Nothing ->
                            Just BackJob { bjPaths = paths
                                         , bjOpts = opts
                                         , bjInput = BackInput { biTypeEnv = typeEnv
                                                              , biTypedMod = tchecked
                                                              , biSrc = srcContent
                                                              , biImplHash = moduleImplHash
                                                              }
                                         }
                  mask_ $ do
                    let outputKey = TaskKey (projPath paths) mn
                        startOutput =
                          startFrontOutputJob onFrontOutputStart onFrontOutputDone shouldWriteFrontOutput outputKey
                        startDependentOutput =
                          startDependentFrontOutputJob onFrontOutputStart onFrontOutputDone shouldWriteFrontOutput
                    tyDbJob <- startOutput FrontOutputTydb writeTyDb
                    docJobs <-
                      forM docOutputActions $ uncurry startOutput
                    tyJobs <-
                      if C.tydb opts
                        then
                          (:[]) <$>
                            startDependentOutput tyDbJob outputKey FrontOutputTydbCopy (copyTydbInterface opts paths mn)
                        else return []
                    let outputJobs = tyDbJob : docJobs ++ tyJobs
                    recordFrontOutputJobs outputJobs
                    let deferredBackJob' =
                          fmap (\dbj -> dbj{ dbjOutputJobs = filter ((== FrontOutputTydb) . fojKind) outputJobs }) deferredBackJob
                    return $ Right FrontResult { frIfaceTE = publicIface
                                               , frImps = imps
                                               , frDoc = mdoc
                                               , frPubHash = modulePubHash
                                               , frImplHash = moduleImplHash
                                               , frNameHashes = publicNameHashes nameHashes
                                               , frInterestDeps = interestDepsFromNameHashes nameHashes
                                               , frFrontTime = frontTimeMaybe
                                               , frFrontTiming = frontTimingMaybe
                                               , frInferredSigs = inferredSigs
                                               , frBackJob = backJob
                                               , frDeferredBackJob = deferredBackJob'
                                               , frOutputJobs = outputJobs
                                               }

data DbpSelection = DbpSelection
  { dbsModule :: A.Module
  , dbsInterestedCount :: Int
  , dbsSelectedCount :: Int
  , dbsFallbackReason :: Maybe String
  }

data DbpNameSelection = DbpNameSelection
  { dnsSelectedNames :: Data.Set.Set A.Name
  }

prepareDeferredBackJob :: Source.SourceProvider
                       -> C.GlobalOptions
                       -> CompileCallbacks
                       -> Acton.Env.Env0
                       -> InterestMap
                       -> DeferredBackJob
                       -> IO (Maybe BackJob)
prepareDeferredBackJob sp gopts callbacks envAcc interestMap dbj = do
  waitFrontOutputJobList (dbjOutputJobs dbj)
  let paths = dbjPaths dbj
      mn = dbjMod dbj
      tyFile = tyDbPath paths mn
      actFile = srcFile paths mn
  (_sourceMeta, _moduleSrcBytesHash, _modulePubHash, _moduleImplHashStored, _imps, _depModules, nameHashes, roots, _tests, _mdoc) <- InterfaceFiles.readHeader tyFile
  let explicitSeeds = dbjSeeds dbj
      interested =
        if Data.Set.null explicitSeeds
          then M.findWithDefault Data.Set.empty mn interestMap
          else explicitSeeds
      rootSeeds = Data.Set.fromList roots
      selectedSeeds = Data.Set.union interested rootSeeds
  nameSelection <- selectDbpNames mn tyFile selectedSeeds nameHashes
  let codegenHash = dbpCodegenHash (dbjImplHash dbj) (dnsSelectedNames nameSelection)
  codegen <- codegenStatus paths mn codegenHash
  if codegenUpToDate codegen
    then do
      logDbpSelection gopts callbacks mn dbj (length nameHashes) (Data.Set.size interested) (Data.Set.size rootSeeds) (Data.Set.size (dnsSelectedNames nameSelection)) Nothing "generated code up to date"
      return Nothing
    else do
      (_ms, _nmod, tmod, _sourceMetaFull, _moduleSrcBytesHashFull, _modulePubHashFull, _moduleImplHashStoredFull, _impsFull, _depModulesFull, _nameHashesFull, _rootsFull, _testsFull, _mdocFull) <- InterfaceFiles.readFile tyFile
      snap <- Source.readSource sp actFile
      env1 <- Acton.Env.mkEnv (searchPath paths) envAcc tmod
      let selection = selectDbpModule (length nameHashes) (Data.Set.size interested) nameSelection tmod
      logDbpSelection gopts callbacks mn dbj (length nameHashes) (Data.Set.size interested) (Data.Set.size rootSeeds) (dbsSelectedCount selection) (dbsFallbackReason selection) "generated code out of date"
      return $ Just BackJob
        { bjPaths = paths
        , bjOpts = dbjOpts dbj
        , bjInput = BackInput
            { biTypeEnv = Converter.convEnvProtos env1
            , biTypedMod = dbsModule selection
            , biSrc = Source.ssText snap
            , biImplHash = codegenHash
            }
        }

logDbpSelection :: C.GlobalOptions
                -> CompileCallbacks
                -> A.ModName
                -> DeferredBackJob
                -> Int
                -> Int
                -> Int
                -> Int
                -> Maybe String
                -> String
                -> IO ()
logDbpSelection gopts callbacks mn dbj totalNames interestedCount rootCount selectedCount fallbackReason codegenReason =
  when (C.verbose gopts) $
    ccOnInfo callbacks $
      "  DBP " ++ modNameToString mn
      ++ ": " ++ dbjReason dbj
      ++ ", total names " ++ show totalNames
      ++ ", interested names " ++ show interestedCount
      ++ ", root names " ++ show rootCount
      ++ ", selected closure " ++ show selectedCount
      ++ maybe "" (\reason -> ", fallback: " ++ reason) fallbackReason
      ++ ", " ++ codegenReason

dbpCodegenHash :: B.ByteString -> Data.Set.Set A.Name -> B.ByteString
dbpCodegenHash moduleImplHash selected =
  SHA256.hash (BL.toStrict (encode ("dbp" :: String, moduleImplHash, selectedNames)))
  where
    selectedNames =
      [ Hashing.nameKey n
      | n <- Data.List.sortOn Hashing.nameKey (Data.Set.toList selected)
      ]

selectDbpNames :: A.ModName
               -> FilePath
               -> Data.Set.Set A.Name
               -> [InterfaceFiles.NameHashInfo]
               -> IO DbpNameSelection
selectDbpNames mn tyFile seeds nameHashes
  | Data.Set.null seeds =
      return DbpNameSelection
        { dnsSelectedNames = Data.Set.empty
        }
  | otherwise =
      case ( traverse (dbpOwningName topNames) (Data.Set.toList seeds)
           , dbpLocalDepsFromNameHashes topNames nameHashes
           ) of
        (Left reason, _) -> dbpSelectionError mn reason
        (_, Left reason) -> dbpSelectionError mn reason
        (Right roots, Right localDeps) -> do
          selected <- dbpNameClosure (dbpExtensionsForName mn tyFile topNames) localDeps (Data.Set.fromList roots)
          return DbpNameSelection
            { dnsSelectedNames = selected
            }
  where
    topNames = dbpTopNamesFromNameHashes nameHashes

dbpSelectionError :: A.ModName -> String -> IO a
dbpSelectionError mn reason =
  throwIO (DbpSelectionError ("DBP selection failed for " ++ modNameToString mn ++ ": " ++ reason))

selectDbpModule :: Int
                -> Int
                -> DbpNameSelection
                -> A.Module
                -> DbpSelection
selectDbpModule totalNames interestedCount nameSelection tmod@(A.Module loc imps mdoc suite)
  | A.hasNotImpl suite = fallback "module contains NotImplemented/native extension hooks"
  | otherwise =
      let selected = dnsSelectedNames nameSelection
          suite' = mapMaybe (dbpPruneTopStmt selected) suite
      in DbpSelection
           { dbsModule = A.Module loc imps mdoc suite'
           , dbsInterestedCount = interestedCount
           , dbsSelectedCount = Data.Set.size selected
           , dbsFallbackReason = Nothing
           }
  where
    fallback reason =
      DbpSelection
        { dbsModule = tmod
        , dbsInterestedCount = interestedCount
        , dbsSelectedCount = totalNames
        , dbsFallbackReason = Just reason
        }

dbpTopNamesFromNameHashes :: [InterfaceFiles.NameHashInfo] -> Data.Set.Set A.Name
dbpTopNamesFromNameHashes nameHashes =
  Data.Set.fromList [ InterfaceFiles.nhName nh | nh <- nameHashes ]

dbpOwningName :: Data.Set.Set A.Name -> A.Name -> Either String A.Name
dbpOwningName topNames n
  | Data.Set.member n topNames = Right n
  | otherwise =
      case n of
        A.Derived base _ -> dbpOwningName topNames base
        _ | Names.isWitness n -> Left ("unresolved witness owner for " ++ nameToString n)
        _ -> Left ("no top-level owner for " ++ nameToString n)

dbpLocalDepsFromNameHashes :: Data.Set.Set A.Name
                           -> [InterfaceFiles.NameHashInfo]
                           -> Either String (M.Map A.Name [A.Name])
dbpLocalDepsFromNameHashes topNames nameHashes =
  M.fromList <$> traverse depsFor nameHashes
  where
    depsFor nh = do
      deps <- traverse
                (dbpOwningName topNames)
                (InterfaceFiles.nhPubLocalDeps nh ++ InterfaceFiles.nhImplLocalDeps nh)
      return
        ( InterfaceFiles.nhName nh
        , unionNames deps []
        )
    unionNames xs ys = Data.List.sortOn Hashing.nameKey (Data.List.nub (xs ++ ys))

dbpExtensionsForName :: A.ModName -> FilePath -> Data.Set.Set A.Name -> A.Name -> IO [A.Name]
dbpExtensionsForName mn tyFile topNames n = do
  byClass <- InterfaceFiles.readExtensionsByClass tyFile n
  byProtocol <- InterfaceFiles.readExtensionsByProtocol tyFile n
  let exts = unionNames byClass byProtocol
      missing = [ ext | ext <- exts, Data.Set.notMember ext topNames ]
  if null missing
    then return exts
    else dbpSelectionError mn $
           "extension index for " ++ nameToString n
           ++ " points at non-top-level extension(s) "
           ++ Data.List.intercalate ", " (map nameToString missing)
  where
    unionNames xs ys = Data.List.sortOn Hashing.nameKey (Data.List.nub (xs ++ ys))

dbpNameClosure :: (A.Name -> IO [A.Name])
               -> M.Map A.Name [A.Name]
               -> Data.Set.Set A.Name
               -> IO (Data.Set.Set A.Name)
dbpNameClosure extensionsForName localDeps roots = go roots (Data.Set.toList roots)
  where
    go seen [] = return seen
    go seen (n:ns) = do
      exts <- extensionsForName n
      let deps = unionNames (M.findWithDefault [] n localDeps) exts
          new = filter (`Data.Set.notMember` seen) deps
          seen' = foldl' (flip Data.Set.insert) seen new
      go seen' (new ++ ns)

    unionNames xs ys = Data.List.sortOn Hashing.nameKey (Data.List.nub (xs ++ ys))

dbpPruneTopStmt :: Data.Set.Set A.Name -> A.Stmt -> Maybe A.Stmt
dbpPruneTopStmt selected stmt =
  case stmt of
    A.Decl l ds ->
      case filter (\d -> Data.Set.member (Names.dname' d) selected) ds of
        [] -> Nothing
        ds' -> Just (A.Decl l ds')
    A.Signature l ns typ dec ->
      case filter (`Data.Set.member` selected) ns of
        [] -> Nothing
        ns' -> Just (A.Signature l ns' typ dec)
    A.Assign{} ->
      if any (`Data.Set.member` selected) (Names.bound stmt)
        then Just stmt
        else Nothing
    A.VarAssign{} ->
      if any (`Data.Set.member` selected) (Names.bound stmt)
        then Just stmt
        else Nothing
    A.Pass{} -> Nothing
    -- Source-level modules only admit Decl, Signature, and Assign at the top
    -- level. VarAssign and Pass are compiler-introduced typed forms handled
    -- above; other statement forms should not appear in a typed module suite.
    _ -> Nothing


-- | Run the back passes for a single module.
-- Executes normalization through codegen, writes .c/.h output as needed, and
-- returns the back-pass elapsed time for logging.
runBackPasses :: C.GlobalOptions -> C.CompileOptions -> Paths -> BackInput -> IO Bool -> IO (Maybe TimeSpec, Maybe BackTiming)
runBackPasses gopts opts paths backInput shouldWrite =
      runBackPassesWithProgress gopts opts paths backInput shouldWrite (\_ -> return ())

runBackPassesWithProgress :: C.GlobalOptions
                          -> C.CompileOptions
                          -> Paths
                          -> BackInput
                          -> IO Bool
                          -> (BackPassProgress -> IO ())
                          -> IO (Maybe TimeSpec, Maybe BackTiming)
runBackPassesWithProgress gopts opts paths backInput shouldWrite onProgress = do
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
        let hexHash = B.unpack $ Base16.encode (biImplHash backInput)
            emitLines = not (C.dbg_no_lines opts)
        Acton.CodeGen.generate liftEnv relSrcBase (biSrc backInput) emitLines boxed hexHash

      let finishBack = finish tNormalize tDeactorize tCPS tLLift tBoxing tCodeGen
      if C.hgen opts
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
                  ok <- shouldWrite
                  if not ok
                    then do
                      emitSkipped BackPassWrite
                      return Nothing
                    else do
                      (_, tWrite) <- timedBackPass BackPassWrite $ do
                        let cFile = outbase ++ ".c"
                            hFile = outbase ++ ".h"
                        writeFile hFile h
                        writeFile cFile c
                      return (Just tWrite)
            finishBack tRender mWriteTime


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
             -> [GlobalTask]
             -> Data.Set.Set TaskKey
             -> CompileCallbacks
             -> IO (Either CompileFailure (Acton.Env.Env0, Bool))
compileTasks sp gopts opts rootPaths rootProj tasks dbpBlocked callbacks = do
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

      logForcedDbpBoundaryExclusions
      loop frontOutputRef runningRef stageInitialReady [] M.empty M.empty M.empty M.empty M.empty Data.Set.empty M.empty stageIndeg stagePending0 baseEnv False maxParallel cwMap

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

    forcedDbpMods = M.keysSet (parseDbpRequests opts)

    formatTaskKey k = tkProj k ++ ":" ++ modNameToString (tkMod k)

    logForcedDbpBoundaryExclusions =
      unless (C.no_dbp opts) $
        forM_ forcedDbpBoundaryKeys $ \k ->
          ccOnInfo callbacks ("  DBP disabled for " ++ formatTaskKey k ++ ": explicit library boundary")
      where
        forcedDbpBoundaryKeys =
          [ k
          | k <- Data.Set.toList dbpBlocked
          , M.member k taskMap
          , Data.Set.member (tkMod k) forcedDbpMods
          ]

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

    flushReadyDeferredBacks :: Acton.Env.Env0
                            -> InterestMap
                            -> Data.Set.Set TaskKey
                            -> M.Map TaskKey (DeferredBackJob, Data.Set.Set TaskKey)
                            -> IO (Either CompileFailure (M.Map TaskKey (DeferredBackJob, Data.Set.Set TaskKey)))
    flushReadyDeferredBacks envAcc interestMap frontDone deferredBacks = do
      let (ready, waiting) =
            M.partitionWithKey
              (\_ (_dbj, waitSet) -> waitSet `Data.Set.isSubsetOf` frontDone)
              deferredBacks
      prepared <- (try $
        forM (M.elems ready) $ \(dbj, _waitSet) -> do
          mJob <- prepareDeferredBackJob sp gopts callbacks envAcc interestMap dbj
          case mJob of
            Nothing -> ccOnBackSkipped callbacks (TaskKey (projPath (dbjPaths dbj)) (dbjMod dbj))
            Just _ -> return ()
          return mJob)
        :: IO (Either SomeException [Maybe BackJob])
      case prepared of
        Left err ->
          if isJust (fromException err :: Maybe SomeAsyncException)
            then throwIO err
            else return (Left (CompileInternalFailure (displayException err)))
        Right jobs -> do
          mapM_ (ccOnBackJob callbacks) (catMaybes jobs)
          return (Right waiting)

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
          forceAlt = altOutput optsBuiltin && mn == rootAlt
      if C.only_build optsBuiltin
        then return (Right ())
        else do
          t' <- case gtTask t of
            TyTask{} | forceAlt -> materializeTask optsBuiltin sp mn actFile Nothing (gtTask t)
            ParseTask{} -> materializeTask optsBuiltin sp mn actFile Nothing (gtTask t)
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
                bPaths
                builtinEnv0
                m
                srcContent
                srcBytes
                mSourceMeta
                (getPubHashCached bPaths)
                (getImplHashCached bPaths)
                (getNameHashMapCached bPaths)
                (\p -> ccOnFrontProgress callbacks t optsBuiltin p)
                (ccOnFrontOutputStart callbacks)
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
          mkFrontResult imps ifaceTE mdoc pubHash implHash nameHashes interestNameHashes backJob deferredBackJob =
            FrontResult
              { frIfaceTE = ifaceTE
              , frImps = imps
              , frDoc = mdoc
              , frPubHash = pubHash
              , frImplHash = implHash
              , frNameHashes = nameHashes
              , frInterestDeps = interestDepsFromNameHashes interestNameHashes
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
              , frPubHash = B.empty
              , frImplHash = B.empty
              , frNameHashes = []
              , frInterestDeps = Data.Set.empty
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
          mkBackJob env1 tmod srcText moduleImplHash =
            BackJob
              { bjPaths = paths
              , bjOpts = optsT
              , bjInput = BackInput
                  { biTypeEnv = Converter.convEnvProtos env1
                  , biTypedMod = tmod
                  , biSrc = srcText
                  , biImplHash = moduleImplHash
                  }
              }

          resolveImportHash m =
            case M.lookup m providers of
              Just depKey ->
                case M.lookup depKey pubMap of
                  Just h  -> return (Just h)
                  Nothing ->
                    if M.member depKey taskMap
                      then error ("Internal error: missing pub hash for dep " ++ modNameToString m)
                      else getPubHashCached paths m
              Nothing -> getPubHashCached paths m
          resolveImportImplHash m =
            case M.lookup m providers of
              Just depKey ->
                case M.lookup depKey implMap of
                  Just h  -> return (Just h)
                  Nothing ->
                    if M.member depKey taskMap
                      then error ("Internal error: missing impl hash for dep " ++ modNameToString m)
                      else getImplHashCached paths m
              Nothing -> getImplHashCached paths m
          resolveNameHashMap' m =
            case M.lookup m providers of
              Just depKey ->
                case M.lookup depKey nameMap of
                  Just hm -> return (Just hm)
                  Nothing ->
                    if M.member depKey taskMap
                      then error ("Internal error: missing name hashes for dep " ++ modNameToString m)
                      else getNameHashMapCached paths m
              Nothing -> getNameHashMapCached paths m

          missingNameHashDiagnostics qn =
            errsToDiagnostics "Compilation error" (modNameToFilename mn) ""
              [(NoLoc, "Hash info missing for " ++ prstr qn)]

          missingDepHashDiagnostics label qn users =
            errsToDiagnostics "Compilation error" (modNameToFilename mn) ""
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

          interestDepsFromDepRows depModules = do
            depSets <- forM depModules $ \depInfo -> do
              let depMn = InterfaceFiles.dmiModule depInfo
              depNames <- InterfaceFiles.readDepNames tyFile depMn
              return (Data.Set.fromList [ (depMn, InterfaceFiles.dniName depName) | depName <- depNames ])
            return (Data.Set.unions depSets)

          cachedInterestDeps = case taskCurrent of
            TyTask{ tyDepModules = depModules } -> interestDepsFromDepRows depModules
            _ -> return Data.Set.empty

          restorePubDepsFromRows depModules nameHashes = do
            byOwner <- foldM addModule M.empty depModules
            return
              [ nh { InterfaceFiles.nhPubDeps = M.findWithDefault [] (InterfaceFiles.nhName nh) byOwner }
              | nh <- nameHashes
              ]
            where
              addModule acc depInfo = do
                let depMn = InterfaceFiles.dmiModule depInfo
                depNames <- InterfaceFiles.readDepNames tyFile depMn
                foldM (addName depMn) acc depNames

              addName depMn acc depInfo = do
                users <- InterfaceFiles.readDepUsers tyFile depMn (InterfaceFiles.dniName depInfo)
                let dep =
                      ( A.GName depMn (InterfaceFiles.dniName depInfo)
                      , InterfaceFiles.dniPubHash depInfo
                      )
                    addUser m user =
                      M.insertWith (++) user [dep] m
                if B.null (InterfaceFiles.dniPubHash depInfo)
                  then return acc
                  else return (foldl' addUser acc (InterfaceFiles.duPubUsers users))

          depModulesFromNameHashes nameHashes = do
            let depMods =
                  Data.List.sortOn modNameToString $
                  Data.Set.toList $
                  Data.Set.fromList
                    [ depMn
                    | nh <- nameHashes
                    , (qn, _) <- InterfaceFiles.nhPubDeps nh ++ InterfaceFiles.nhImplDeps nh
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
            hm <- resolveNameHashMap' m
            case hm of
              Nothing -> return (Left (missingIfaceDiagnostics mn "" m))
              Just hmap ->
                case M.lookup n hmap of
                  Just info -> return (Right info)
                  Nothing -> return (Left (missingNameHashDiagnostics (A.GName m n)))

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
              let qnsSorted = Data.List.sortOn Hashing.qnameKey (Data.Set.toList (Data.Set.fromList qns))
                  users = " (used by " ++ A.nstr n ++ ")"
              resolvedQns <- traverseDiags (\qn -> do
                currE <- resolveQNameHash label getHash users qn
                return (fmap (\curr -> (qn, curr)) currE)) qnsSorted
              return (fmap (\vals -> (n, vals)) resolvedQns)) (M.toList deps)
            return (fmap M.fromList resolved)

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
                  _ -> Right (depMn, True, True)

              foldRows rows =
                ( Data.Set.fromList [ depMn | (depMn, pubChanged, _implChanged) <- rows, pubChanged ]
                , Data.Set.fromList [ depMn | (depMn, _pubChanged, implChanged) <- rows, implChanged ]
                )

          checkDepNameRows changedPubMods changedImplMods = do
            let changedMods =
                  Data.List.sortOn modNameToString $
                  Data.Set.toList (Data.Set.union changedPubMods changedImplMods)
            foldM checkModule ([], [], [], [], M.empty, M.empty, False) changedMods
            where
              checkModule acc depMn = do
                depNames <- InterfaceFiles.readDepNames tyFile depMn
                if null depNames && Data.Set.member depMn changedImplMods
                  then return (markImplRefresh acc)
                  else do
                    mhm <- resolveNameHashMap' depMn
                    foldM (checkName depMn mhm) acc depNames

              checkName depMn mhm acc depInfo = do
                let depName = InterfaceFiles.dniName depInfo
                    qn = A.GName depMn depName
                    current getHash =
                      case mhm >>= M.lookup depName of
                        Nothing -> Nothing
                        Just info ->
                          let h = getHash info
                          in if B.null h then Nothing else Just h
                    checkPub = Data.Set.member depMn changedPubMods && not (B.null (InterfaceFiles.dniPubHash depInfo))
                    checkImpl = Data.Set.member depMn changedImplMods && not (B.null (InterfaceFiles.dniImplHash depInfo))
                acc1 <- if checkPub
                          then addPub depMn depName qn (InterfaceFiles.dniPubHash depInfo) (current InterfaceFiles.nhPubHash) acc
                          else return acc
                if checkImpl
                  then addImpl depMn depName qn (InterfaceFiles.dniImplHash depInfo) (current InterfaceFiles.nhImplHash) acc1
                  else return acc1

              markImplRefresh (pubDeltas, implDeltas, pubMissing, implMissing, pubUsers, implUsers, _) =
                (pubDeltas, implDeltas, pubMissing, implMissing, pubUsers, implUsers, True)

              addPub depMn depName qn old mNew acc@(pubDeltas, implDeltas, pubMissing, implMissing, pubUsers, implUsers, needImplRefresh) =
                case mNew of
                  Just new | new == old -> return acc
                  Just new -> do
                    users <- InterfaceFiles.readDepUsers tyFile depMn depName
                    return ((qn, old, new) : pubDeltas, implDeltas, pubMissing, implMissing, M.insert qn (InterfaceFiles.duPubUsers users) pubUsers, implUsers, needImplRefresh)
                  Nothing -> do
                    users <- InterfaceFiles.readDepUsers tyFile depMn depName
                    return (pubDeltas, implDeltas, qn : pubMissing, implMissing, M.insert qn (InterfaceFiles.duPubUsers users) pubUsers, implUsers, needImplRefresh)

              addImpl depMn depName qn old mNew acc@(pubDeltas, implDeltas, pubMissing, implMissing, pubUsers, implUsers, needImplRefresh) =
                case mNew of
                  Just new | new == old -> return acc
                  Just new -> do
                    users <- InterfaceFiles.readDepUsers tyFile depMn depName
                    return (pubDeltas, (qn, old, new) : implDeltas, pubMissing, implMissing, pubUsers, M.insert qn (InterfaceFiles.duImplUsers users) implUsers, needImplRefresh)
                  Nothing -> do
                    users <- InterfaceFiles.readDepUsers tyFile depMn depName
                    return (pubDeltas, implDeltas, pubMissing, qn : implMissing, pubUsers, M.insert qn (InterfaceFiles.duImplUsers users) implUsers, needImplRefresh)

      case taskCurrent of
        ParseErrorTask{ parseDiagnostics = diags } -> return (key, Left diags)
        _ | C.only_build optsT -> do
              ifaceRes <- case taskCurrent of
                            TyTask{ tyPubHash = h } -> readIfaceFromTy paths mn "" (Just h)
                            ParseTask{ src = srcContent } -> readIfaceFromTy paths mn srcContent Nothing
                            ActonTask{ src = srcContent } -> readIfaceFromTy paths mn srcContent Nothing
              case ifaceRes of
                Right (imps, ifaceTE, mdoc, ih, implH) -> do
                  let cachedFullNameHashes = case taskCurrent of
                        TyTask{ tyNameHashes = nhs } -> nhs
                        _ -> []
                      cachedNameHashes = publicNameHashes cachedFullNameHashes
                  interestDeps <- cachedInterestDeps
                  let fr = (mkFrontResult imps ifaceTE mdoc ih implH cachedNameHashes cachedFullNameHashes Nothing Nothing)
                             { frInterestDeps = interestDeps }
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
                        then return (Right ([], [], [], [], M.empty, M.empty, False))
                        else Right <$> checkDepNameRows changedPubMods changedImplMods
            -- Source tasks always run front passes, so deps are irrelevant.
            _ -> return (Right ([], [], [], [], M.empty, M.empty, False))

          case needByDepsRes of
            Left diags -> return (key, Left diags)
            Right (pubDeltas, implDeltas, pubMissing, implMissing, pubUsers, implUsers, needImplRefresh) -> do
              let needBySource = case taskCurrent of { ParseTask{} -> True; ActonTask{} -> True; _ -> False }
                  -- Public deltas require front passes; impl deltas only need back jobs.
                  needByPub = not (null pubDeltas)
                  needByMissing = not (null pubMissing) || not (null implMissing)
                  needByImpl = needImplRefresh || not (null implDeltas)
                  forceAlt    = altOutput optsT && mn == rootAlt
                  forceAlways = C.alwaysbuild optsT
                  -- Front passes run on source or API changes, or when forced.
                  needFront = needBySource || needByPub || needByMissing || forceAlt || forceAlways
                  mModuleImplHash = case taskCurrent of
                    TyTask{ tyImplHash = implHash } -> Just implHash
                    _ -> Nothing
                  cachedDeferredBackJob = case taskCurrent of
                    TyTask{ tyImplHash = implHash, tyNameHashes = nhs } -> dbpDeferredBackJob (isDbpBlocked key) optsT paths mn implHash nhs
                    _ -> Nothing
                  isCachedDbp =
                    case cachedDeferredBackJob of
                      Just _ -> True
                      Nothing -> False
              let canCheckCodegen = not needFront && not needByImpl && not (altOutput optsT) && not isCachedDbp
              mCodegenStatus <- case mModuleImplHash of
                Just implHash | canCheckCodegen -> Just <$> codegenStatus paths mn implHash
                _ -> return Nothing
              let needByCodegen = maybe False (not . codegenUpToDate) mCodegenStatus
              let runFront = do
                    prevNameHashes <- if C.verbose gopts
                      then case taskCurrent of
                        TyTask{ tyNameHashes = nhs } -> return (Just (nameHashMapFromList (publicNameHashes nhs)))
                        _ -> getNameHashMapCached paths mn
                      else return Nothing
                    when (C.verbose gopts) $ do
                      if needBySource
                        then ccOnInfo callbacks ("  Stale " ++ modNameToString mn ++ ": source changed")
                        else do
                          when needByPub $ do
                            let fmtDelta (qn, old, new) = prstr qn ++ " " ++ short8 old ++ " → " ++ short8 new ++ fmtUsers pubUsers qn
                            ccOnInfo callbacks ("  Stale " ++ modNameToString mn ++ ": pub changes in " ++ Data.List.intercalate ", " (map fmtDelta pubDeltas))
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
                            ccOnInfo callbacks ("  Stale " ++ modNameToString mn ++ ": missing dep hashes in " ++ Data.List.intercalate ", " (pubMissingItems ++ implMissingItems))
                    t' <- case taskCurrent of
                            ActonTask{} -> return taskCurrent
                            _ -> materializeTask optsT sp mn actFile Nothing taskCurrent
                    case t' of
                      ParseErrorTask{ parseDiagnostics = diags } -> return (key, Left diags)
                      ActonTask{ src = srcContent, srcBytes = srcBytes, sourceMeta = mSourceMeta, atree = m } -> do
                        res <- runFrontPasses
                          gopts
                          optsT
                          (isDbpBlocked key)
                          paths
                          envSnap
                          m
                          srcContent
                          srcBytes
                          mSourceMeta
                          resolveImportHash
                          resolveImportImplHash
                          resolveNameHashMap'
                          (\p -> ccOnFrontProgress callbacks t optsT p)
                          (ccOnFrontOutputStart callbacks)
                          (ccOnFrontOutputDone callbacks)
                          (ccShouldWriteFrontOutput callbacks)
                          (rememberFrontOutputJobList frontOutputRef)
                        case res of
                          Left diags -> return (key, Left diags)
                          Right fr -> do
                            when (C.verbose gopts) $
                              forM_ prevNameHashes $ \prevMap ->
                                forM_ (nameHashSummary prevMap (frNameHashes fr)) $ \summary ->
                                  ccOnInfo callbacks ("  Hash deltas " ++ modNameToString mn ++ ": " ++ summary)
                            cacheFrontResult fr
                      ParseTask{} -> error ("Internal error: unmaterialized ParseTask " ++ modNameToString mn)
                      _ -> error ("Internal error: unexpected task " ++ show t')
                  runImplRefresh = do
                    let rerunFront = do
                          when (C.verbose gopts) $
                            ccOnInfo callbacks ("  Stale " ++ modNameToString mn ++ ": impl refresh encountered unresolved dep hashes; rerunning front passes")
                          runFront
                        handleSyncFailure :: SomeException -> IO (TaskKey, Either [Diagnostic String] FrontResult)
                        handleSyncFailure err =
                          if isJust (fromException err :: Maybe SomeAsyncException)
                            then throwIO err
                            else rerunFront
                        handleImplRefreshException :: SomeException -> IO (TaskKey, Either [Diagnostic String] FrontResult)
                        handleImplRefreshException = handleSyncFailure
                    (do
                      when (C.verbose gopts && not (null implDeltas)) $ do
                        let fmtDelta (qn, old, new) = prstr qn ++ " " ++ short8 old ++ " → " ++ short8 new ++ fmtUsers implUsers qn
                        ccOnInfo callbacks ("  Stale " ++ modNameToString mn ++ ": impl changes in " ++ Data.List.intercalate ", " (map fmtDelta implDeltas))
                      tyRes <- readTyFile
                      case tyRes of
                        Left diags -> return (key, Left diags)
                        Right (_ms, nmod, tmod, sourceMeta, moduleSrcBytesHash, modulePubHash, _moduleImplHash, imps, depModules, nameHashes, roots, tests, mdoc) -> do
                          parsedRes <- parseActFile optsT sp mn actFile Nothing
                          case parsedRes of
                            Left diags -> return (key, Left diags)
                            Right (snap, parsedMod) -> do
                              let nameSrcHashes =
                                    M.fromList [ (InterfaceFiles.nhName nh, InterfaceFiles.nhSrcHash nh)
                                               | nh <- nameHashes
                                               ]
                                  nameKeys = M.keysSet nameSrcHashes
                                  nameImplHashes0 = Hashing.nameHashesFromItems (Hashing.topLevelItems tmod)
                                  nameImplHashes = M.filterWithKey (\k _ -> Data.Set.member k nameKeys) nameImplHashes0
                                  localNames = nameKeys
                                  implDepsRaw0 = Hashing.implDepsFromItems (Hashing.topLevelItems tmod)
                                  implDepsRaw = M.fromList
                                    [ (n, M.findWithDefault [] n implDepsRaw0)
                                    | n <- M.keys nameSrcHashes
                                    ]
                              envRes <- (try :: IO Acton.Env.Env0 -> IO (Either SomeException Acton.Env.Env0)) $
                                Acton.Env.mkEnv (searchPath paths) envSnap parsedMod
                              case envRes of
                                Left err -> handleSyncFailure err
                                Right env1 -> do
                                  depRes <- (try :: IO a -> IO (Either SomeException a)) $ do
                                    let hashEnv = setMod mn env1
                                        (implLocalDeps, implExtDeps) = Hashing.splitDeps mn hashEnv localNames implDepsRaw
                                        depCount = sum (map length (M.elems implLocalDeps))
                                                 + sum (map length (M.elems implExtDeps))
                                    -- Force dep maps now so stale aliases/missing names
                                    -- are handled via the front-pass fallback path.
                                    _ <- evaluate depCount
                                    implExtRes <- resolveDepHashes "impl" InterfaceFiles.nhImplHash implExtDeps
                                    return (implLocalDeps, implExtRes)
                                  case depRes of
                                    Left err -> handleSyncFailure err
                                    Right (_, Left _) -> rerunFront
                                    Right (implLocalDeps, Right implExtHashes) -> do
                                      nameHashesWithPubDeps <- restorePubDepsFromRows depModules nameHashes
                                      let updatedNameHashes =
                                            Hashing.refreshImplHashes nameHashesWithPubDeps nameImplHashes implLocalDeps implExtHashes
                                          moduleImplHash = Hashing.moduleImplHashFromNameHashes updatedNameHashes
                                      depModulesRes <- depModulesFromNameHashes updatedNameHashes
                                      case depModulesRes of
                                        Left _ -> rerunFront
                                        Right updatedDepModules -> mask_ $ do
                                          outputJob <- startFrontOutputJob (ccOnFrontOutputStart callbacks)
                                                                           (ccOnFrontOutputDone callbacks)
                                                                           (ccShouldWriteFrontOutput callbacks)
                                                                           (TaskKey (projPath paths) mn)
                                                                           FrontOutputTydb $ do
                                            InterfaceFiles.writeFile tyFile moduleSrcBytesHash modulePubHash moduleImplHash sourceMeta imps updatedDepModules updatedNameHashes roots tests mdoc nmod tmod
                                          tyJobs <-
                                            if C.tydb optsT
                                              then (:[]) <$> startDependentFrontOutputJob (ccOnFrontOutputStart callbacks)
                                                                                         (ccOnFrontOutputDone callbacks)
                                                                                         (ccShouldWriteFrontOutput callbacks)
                                                                                         outputJob
                                                                                         (TaskKey (projPath paths) mn)
                                                                                         FrontOutputTydbCopy
                                                                                         (copyTydbInterface optsT paths mn)
                                              else return []
                                          let outputJobs = outputJob : tyJobs
                                          rememberFrontOutputJobList frontOutputRef outputJobs
                                          let I.NModule imps ifaceFull _mdoc = nmod
                                              ifaceTE = publicIfaceTE ifaceFull
                                              deferredBackJob0 = dbpDeferredBackJob (isDbpBlocked key) optsT paths mn moduleImplHash updatedNameHashes
                                              deferredBackJob = fmap (\dbj -> dbj{ dbjOutputJobs = [outputJob] }) deferredBackJob0
                                              backJob =
                                                case deferredBackJob of
                                                  Just _ -> Nothing
                                                  Nothing -> Just (mkBackJob env1 tmod (Source.ssText snap) moduleImplHash)
                                              fr = (mkFrontResult imps ifaceTE mdoc modulePubHash moduleImplHash (publicNameHashes updatedNameHashes) updatedNameHashes backJob deferredBackJob)
                                                { frOutputJobs = outputJobs }
                                          cacheFrontResult fr
                      ) `catch` handleImplRefreshException
                  runCodegenRefresh = do
                    when (C.verbose gopts) $ do
                      let suffix = maybe "" formatCodegenDelta mCodegenStatus
                      ccOnInfo callbacks ("  Stale " ++ modNameToString mn ++ ": generated code out of date" ++ suffix)
                    tyRes <- readTyFile
                    case tyRes of
                      Left diags -> return (key, Left diags)
                      Right (_ms, nmod, tmod, _sourceMeta, _moduleSrcBytesHash, modulePubHash, moduleImplHashStored, _imps, _depModules, nameHashes, _roots, _tests, mdoc) -> do
                        snap <- Source.readSource sp actFile
                        env1 <- Acton.Env.mkEnv (searchPath paths) envSnap tmod
                        interestDeps <- cachedInterestDeps
                        let I.NModule imps ifaceFull _mdoc = nmod
                            ifaceTE = publicIfaceTE ifaceFull
                            deferredBackJob = dbpDeferredBackJob (isDbpBlocked key) optsT paths mn moduleImplHashStored nameHashes
                            backJob =
                              case deferredBackJob of
                                Just _ -> Nothing
                                Nothing -> Just (mkBackJob env1 tmod (Source.ssText snap) moduleImplHashStored)
                            fr = (mkFrontResult imps ifaceTE mdoc modulePubHash moduleImplHashStored (publicNameHashes nameHashes) nameHashes backJob deferredBackJob)
                                   { frInterestDeps = interestDeps }
                        cacheFrontResult fr
                  runReuse = do
                    when (C.verbose gopts) $
                      ccOnInfo callbacks ("  Fresh " ++ modNameToString mn ++ ": using cached .tydb")
                    ifaceRes <- case taskCurrent of
                                  TyTask{ tyPubHash = h } -> readIfaceFromTy paths mn "" (Just h)
                                  _ -> readIfaceFromTy paths mn "" Nothing
                    case ifaceRes of
                      Left diags -> return (key, Left diags)
                      Right (imps, ifaceTE, mdoc, ih, implH) -> do
                        let cachedFullNameHashes = case taskCurrent of
                              TyTask{ tyNameHashes = nhs } -> nhs
                              _ -> []
                            cachedNameHashes = publicNameHashes cachedFullNameHashes
                        interestDeps <- cachedInterestDeps
                        let fr = (mkFrontResult imps ifaceTE mdoc ih implH cachedNameHashes cachedFullNameHashes Nothing cachedDeferredBackJob)
                                   { frInterestDeps = interestDeps }
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
      timeStart <- getTime Monotonic
      parsed <- if C.only_build optsT
                  then return (gtTask t)
                  else case gtTask t of
                         ParseTask{} -> materializeTask optsT sp mn actFile (Just onProgress) (gtTask t)
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
         -> InterestMap
         -> Data.Set.Set TaskKey
         -> M.Map TaskKey (DeferredBackJob, Data.Set.Set TaskKey)
         -> M.Map StageKey Int
         -> Data.Set.Set StageKey
         -> Acton.Env.Env0
         -> Bool
         -> Int
         -> M.Map TaskKey Integer
         -> IO (Either CompileFailure (Acton.Env.Env0, Bool))
    loop frontOutputRef runningRef rdy running res implRes nameRes parsedTasks interestMap frontDone deferredBacks ind pend envAcc hadErrors maxPar cw = do
      (rdy1, running1) <- mask_ $ do
        res@(rdy1', running1') <- scheduleMore (maxPar - length running) rdy running frontOutputRef res implRes nameRes parsedTasks envAcc cw
        writeIORef runningRef (map fst running1')
        return res
      if null running1 && null rdy1
        then if Data.Set.null pend
                 then do
                   flushRes <- flushReadyDeferredBacks envAcc interestMap frontDone deferredBacks
                   case flushRes of
                     Left err -> return (Left err)
                     Right deferredBacks' -> do
                       writeIORef runningRef []
                       return (Right (envAcc, hadErrors || not (M.null deferredBacks')))
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
              loop frontOutputRef runningRef rdy2 running3 res implRes nameRes parsedTasks2 interestMap frontDone deferredBacks ind pend2 envAcc True maxPar cw
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
                  loop frontOutputRef runningRef rdy2 running2 res implRes nameRes parsedTasks2 interestMap frontDone deferredBacks ind2 pend2 envAcc hadErrors maxPar cw
                StageFronted fr -> do
                  ccOnFrontDone callbacks tDone optsDone
                  ccOnFrontResult callbacks tDone optsDone fr
                  forM_ (frBackJob fr) $ ccOnBackJob callbacks
                  let res2  = M.insert keyDone (frPubHash fr) res
                      implRes2 = M.insert keyDone (frImplHash fr) implRes
                      nameRes2 = M.insert keyDone (nameHashMapFromList (frNameHashes fr)) nameRes
                      parsedTasks2 = M.delete keyDone parsedTasks
                      envAcc' = Acton.Env.addMod (tkMod keyDone) (frImps fr) (frIfaceTE fr) (frDoc fr) envAcc
                      interestMap2 = addInterestDeps (frInterestDeps fr) interestMap
                      frontDone2 = Data.Set.insert keyDone frontDone
                      deferredBacks1 =
                        case frDeferredBackJob fr of
                          Nothing -> deferredBacks
                          Just dbj -> M.insert keyDone (dbj, dependentClosure keyDone) deferredBacks
                  flushRes <- flushReadyDeferredBacks envAcc' interestMap2 frontDone2 deferredBacks1
                  case flushRes of
                    Left err -> return (Left err)
                    Right deferredBacks2 ->
                      loop frontOutputRef runningRef rdy2 running2 res2 implRes2 nameRes2 parsedTasks2 interestMap2 frontDone2 deferredBacks2 ind2 pend2 envAcc' hadErrors maxPar cw


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
                   (res, _timing) <- runBackPasses gopts (bjOpts job) (bjPaths job) (bjInput job) (return True)
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
                    modName     :: A.ModName
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
srcFile paths mn        = joinPath (srcDir paths : A.modPath mn) ++ ".act"

-- | Compute the output base path (without extension) for a module.
-- Used to locate .tydb/.c/.h output under the project's types directory.
outBase                 :: Paths -> A.ModName -> FilePath
outBase paths mn        = joinPath (projTypes paths : A.modPath mn)

tyDbPath                :: Paths -> A.ModName -> FilePath
tyDbPath paths mn       = outBase paths mn ++ InterfaceFiles.interfaceExt

-- | Compute the module path without extension under the project's src dir.
-- Used to derive the .act path or related per-module files.
srcBase                 :: Paths -> A.ModName -> FilePath
srcBase paths mn        = joinPath (srcDir paths : A.modPath mn)


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
                             depTypePaths <- if isTmp then return [] else collectDepTypePaths projPath depOverrides
                             let sPaths = [projTypes] ++ depTypePaths ++ (C.searchpath opts) ++ systemTypePaths sysPath sysTypes
                             createDirectoryIfMissing True binDir
                             createDirectoryIfMissing True projOut
                             createDirectoryIfMissing True projTypes
                             createDirectoryIfMissing True (getModPath projTypes modName)
                             return $ Paths sPaths sysPath sysTypes projPath projOut projTypes binDir srcDir isTmp fileExt modName
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
moduleNameFromFile :: FilePath -> FilePath -> IO A.ModName
moduleNameFromFile srcBase actFile = do
    base <- normalizePathSafe srcBase
    file <- normalizePathSafe actFile
    let rel = dropExtension (makeRelative base file)
    return $ A.modName (splitDirectories rel)

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
        forM actFiles $ \f -> do
          mn <- moduleNameFromFile (projSrcDir ctx) f
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
            base = normalise (moduleBase rel ext)
        unless (Data.Set.member base srcMods) $
          removeFile absFile `catch` ignoreNotExists

    pruneTyDb srcMods typesRoot absDir = do
      let rel = normalise (makeRelative typesRoot absDir)
          base = normalise (dropExtension rel)
      unless (Data.Set.member base srcMods) $
        removePathForcibly absDir `catch` ignoreNotExists

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
        p = Paths sPaths (projSysPath ctx) (projSysTypes ctx) (projRoot ctx) (projOutDir ctx) (projTypesDir ctx) bin src False ".act" mn
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
              BuildSpec.ParseError msg ->
                throwProjectError ("Failed to parse Build.act in " ++ dir ++ ":\n" ++ msg)
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
        home <- getHomeDirectory
        let zigExe      = joinPath [sysPath paths, "zig", "zig"]
            globalCache = joinPath [home, ".cache", "acton", "zig-global-cache"]
            depsCache   = joinPath [home, ".cache", "acton", "deps"]
            cacheDir h  = joinPath [globalCache, "p", h]
        createDirectoryIfMissing True globalCache
        createDirectoryIfMissing True depsCache
        let rootPins = BuildSpec.dependencies rootSpec
        _ <- walkProject rootPins cacheDir zigExe globalCache depsCache
                         Data.Set.empty (projPath paths) rootSpec
        return ()
  where
    walkProject rootPins cacheDir zigExe globalCache depsCache seen projDir spec = do
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
          let seen' = Data.Set.insert projAbs seen
          foldM (walkDependency rootPins cacheDir zigExe globalCache depsCache projAbs)
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

    walkDependency rootPins cacheDir zigExe globalCache depsCache base seen (depName, dep) = do
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
          walkProject rootPins cacheDir zigExe globalCache depsCache seen depAbs depSpec

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
      dl <- downloadToLocalArchive kind name depUrl
      case dl of
        Left dlErr -> do
          direct <- runZigFetch zigExe globalCache depUrl
          case direct of
            Left ex ->
              return (Left ("Failed to fetch dependency " ++ name ++ ":\nDownload step failed: " ++ dlErr
                            ++ "\nDirect zig fetch failed: " ++ displayException ex))
            Right (ExitSuccess, out, _) ->
              validateFetchOutput name mh cacheDir out
            Right (ExitFailure _, _, err) ->
              return (Left ("Failed to fetch dependency " ++ name ++ ":\nDownload step failed: " ++ dlErr
                            ++ "\nDirect zig fetch failed:\n" ++ err))
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

    isHttpUrl :: String -> Bool
    isHttpUrl depUrl =
      let u = map toLower depUrl
      in "http://" `isPrefixOf` u || "https://" `isPrefixOf` u

    downloadToLocalArchive :: String -> String -> String -> IO (Either String FilePath)
    downloadToLocalArchive _kind _name depUrl = do
      parsedReq <- try (HTTP.parseRequest depUrl) :: IO (Either SomeException HTTP.Request)
      case parsedReq of
        Left ex -> return (Left ("Invalid dependency URL: " ++ displayException ex))
        Right req -> do
          let settings = HTTP.managerSetProxy (HTTP.proxyEnvironment Nothing) tlsManagerSettings
          manager <- HTTP.newManager settings
          opened <- try (HTTP.responseOpen req manager) :: IO (Either SomeException (HTTP.Response HTTP.BodyReader))
          case opened of
            Left ex -> return (Left (displayException ex))
            Right response -> do
              let code = statusCode (HTTP.responseStatus response)
              if code < 200 || code >= 300
                then do
                  _ <- try (HTTP.responseClose response) :: IO (Either SomeException ())
                  return (Left ("HTTP error " ++ show code))
                else do
                  tmpDir <- getTemporaryDirectory
                  (tmpPath, tmpHandle) <- openBinaryTempFile tmpDir "acton-fetch"
                  let initialSuffix = archiveSuffixForRequestResponse req response
                  streamRes <- try (streamDownloadToFile (HTTP.responseBody response) tmpHandle) :: IO (Either SomeException BS.ByteString)
                  _ <- try (hClose tmpHandle) :: IO (Either IOException ())
                  _ <- try (HTTP.responseClose response) :: IO (Either SomeException ())
                  case streamRes of
                    Left ex -> do
                      _ <- try (removeFile tmpPath) :: IO (Either IOException ())
                      return (Left ("Unable to save downloaded archive: " ++ displayException ex))
                    Right sniffBytes ->
                      case initialSuffix <|> detectArchiveSuffixFromBytes sniffBytes of
                        Nothing -> do
                          _ <- try (removeFile tmpPath) :: IO (Either IOException ())
                          return (Left "Could not determine archive type from URL path, response headers, or file bytes")
                        Just suffix -> do
                          let finalPath = tmpPath ++ suffix
                          moveRes <- try (renameFile tmpPath finalPath) :: IO (Either IOException ())
                          case moveRes of
                            Left ex -> do
                              _ <- try (removeFile tmpPath) :: IO (Either IOException ())
                              return (Left ("Unable to finalize downloaded archive path: " ++ displayException ex))
                            Right _ ->
                              return (Right finalPath)

    streamDownloadToFile :: HTTP.BodyReader -> Handle -> IO BS.ByteString
    streamDownloadToFile bodyReader outHandle =
      loop BS.empty
      where
        sniffLimit = 600
        loop sniff = do
          chunk <- HTTP.brRead bodyReader
          if BS.null chunk
            then return sniff
            else do
              BS.hPut outHandle chunk
              let sniff' =
                    if BS.length sniff >= sniffLimit
                      then sniff
                      else BS.take sniffLimit (sniff <> chunk)
              loop sniff'

    archiveSuffixForRequestResponse :: HTTP.Request -> HTTP.Response HTTP.BodyReader -> Maybe String
    archiveSuffixForRequestResponse req response =
      case archiveSuffixFromPath (B.unpack (HTTP.path req)) of
        Just suffix -> Just suffix
        Nothing ->
          case archiveSuffixFromContentDisposition (lookup hContentDisposition (HTTP.responseHeaders response)) of
            Just suffix -> Just suffix
            Nothing -> archiveSuffixFromContentType (lookup hContentType (HTTP.responseHeaders response))

    archiveSuffixFromPath :: String -> Maybe String
    archiveSuffixFromPath rawPath =
      let p = map toLower rawPath
      in if ".tar.gz" `isSuffixOf` p then Just ".tar.gz"
         else if ".tgz" `isSuffixOf` p then Just ".tgz"
         else if ".tar.xz" `isSuffixOf` p then Just ".tar.xz"
         else if ".txz" `isSuffixOf` p then Just ".txz"
         else if ".tar.zst" `isSuffixOf` p then Just ".tar.zst"
         else if ".tzst" `isSuffixOf` p then Just ".tzst"
         else if ".tar" `isSuffixOf` p then Just ".tar"
         else if ".zip" `isSuffixOf` p then Just ".zip"
         else if ".jar" `isSuffixOf` p then Just ".jar"
         else Nothing

    archiveSuffixFromContentType :: Maybe B.ByteString -> Maybe String
    archiveSuffixFromContentType mType =
      case map toLower . trim . takeWhile (/= ';') . B.unpack <$> mType of
        Just "application/x-tar" -> Just ".tar"
        Just "application/gzip" -> Just ".tar.gz"
        Just "application/x-gzip" -> Just ".tar.gz"
        Just "application/tar+gzip" -> Just ".tar.gz"
        Just "application/x-tar-gz" -> Just ".tar.gz"
        Just "application/x-gtar-compressed" -> Just ".tar.gz"
        Just "application/x-xz" -> Just ".tar.xz"
        Just "application/zstd" -> Just ".tar.zst"
        Just "application/zip" -> Just ".zip"
        Just "application/x-zip-compressed" -> Just ".zip"
        Just "application/java-archive" -> Just ".zip"
        _ -> Nothing

    archiveSuffixFromContentDisposition :: Maybe B.ByteString -> Maybe String
    archiveSuffixFromContentDisposition mVal = do
      headerVal <- mVal
      filenameVal <- extractField (B.pack "filename*=") headerVal <|> extractField (B.pack "filename=") headerVal
      archiveSuffixFromPath (B.unpack filenameVal)
      where
        extractField key raw =
          let lowerRaw = B.map toLower raw
              (prefix, rest) = B.breakSubstring key lowerRaw
          in if B.null rest
               then Nothing
               else
                 let origRest = B.drop (B.length prefix) raw
                     val0 = B.drop (B.length key) origRest
                     val1 = B.takeWhile (/= ';') val0
                     val2 = stripQuotes (B.dropWhile isSpace (trimBS val1))
                     val3 =
                       case B.breakSubstring (B.pack "''") val2 of
                         (_, t) | B.null t -> val2
                         (_, t) -> B.drop 2 t
                 in if B.null val3 then Nothing else Just val3
        trimBS = B.dropWhileEnd isSpace . B.dropWhile isSpace
        stripQuotes s
          | B.length s >= 2 && B.head s == '"' && B.last s == '"' = B.tail (B.init s)
          | otherwise = s

    detectArchiveSuffixFromBytes :: BS.ByteString -> Maybe String
    detectArchiveSuffixFromBytes bytes
      | startsWith [0x50, 0x4b, 0x03, 0x04] bytes = Just ".zip"
      | startsWith [0x50, 0x4b, 0x05, 0x06] bytes = Just ".zip"
      | startsWith [0x50, 0x4b, 0x07, 0x08] bytes = Just ".zip"
      | startsWith [0x1f, 0x8b] bytes = Just ".tar.gz"
      | startsWith [0xfd, 0x37, 0x7a, 0x58, 0x5a, 0x00] bytes = Just ".tar.xz"
      | startsWith [0x28, 0xb5, 0x2f, 0xfd] bytes = Just ".tar.zst"
      | hasUstar bytes = Just ".tar"
      | otherwise = Nothing
      where
        startsWith :: [Word8] -> BS.ByteString -> Bool
        startsWith sig bs =
          BS.length bs >= length sig && BS.take (length sig) bs == BS.pack sig
        hasUstar bs =
          BS.length bs >= 262 && BS.take 5 (BS.drop 257 bs) == BS.pack [0x75, 0x73, 0x74, 0x61, 0x72]

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
collectDepTypePaths :: FilePath -> [(String, FilePath)] -> IO [FilePath]
collectDepTypePaths projDir overrides = do
  root <- normalizePathSafe projDir
  (_, _, paths) <- go Data.Set.empty M.empty root Nothing
  return paths
  where
    go seen fpMap dir mSpec = do
      spec0 <- case mSpec of
                 Just s -> return s
                 Nothing -> loadBuildSpec dir
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
              (seenNext, fpMapNext, sub) <- go seen' fpMap' depPath Nothing
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
