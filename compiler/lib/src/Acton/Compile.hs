{-# LANGUAGE CPP #-}
{-|
Overview

This module implements the shared Acton compilation pipeline that both the
acton CLI and the LSP server drive. It builds a dependency graph across
projects, runs the front passes (parse, kinds, types) to produce .ty interface
files and diagnostics, and then emits the back passes (normalizer through
codegen) as separate jobs.

The design is intentionally incremental and event-friendly. While the compiler
scheduler is running and actively compiling modules, we can receive a new event
about a modified module, cancel running tasks and restart compilation of that
module from that point. This is used by watch mode and LSP where updates to
files or in-editor buffers trigger recompilation. Dependencies are considered so
that updates to a module only cause the affected part of the subgraph, i.e. the
modified module and downstream dependencies to be recompiled. Cached .ty headers
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
      output with generation checks (e.g. whenCurrentGen), and BackQueue ignores
      back jobs for stale generations.
    - Callers may supply a delay (debounce) before startCompile runs (LSP uses
      debounceMicros on change events; acton watch uses 0).
  - Finalization:
    - CLI waits on backQueueWait before running Zig build
    - LSP does not implicitly run the Zig build, it is only run explicitly when
      the user asks to build / run a module

State and orchestration:
  - Acton.Compile holds only small in-process caches (tyPathCache, pubHashCache,
    and nameHashCache) to speed up .ty lookups; all orchestration state is
    owned by the caller.
  - Callers allocate and hold a CompileScheduler from this module. The
    scheduler encapsulates mutable state (generation id, cancelable async
    handle, build-spec stamp, and a shared back-queue); callers pass it into
    startCompile and backQueueEnqueue rather than mutating it directly.
  - LSP additionally keeps overlaysRef for in-memory buffers and builds an
    overlay-aware SourceProvider on top of disk reads. acton watch reads
    directly from disk.
  - Each event bumps the generation via startCompile; callers may pass a delay
    (LSP uses debounceMicros, acton watch uses 0). Back jobs are filtered by
    generation inside BackQueue; front-pass diagnostics should be gated by the
    caller if needed.
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
  , FrontResult(..)
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
  , readImports
  , buildGlobalTasks
  , selectNeededTasks
  , selectAffectedTasks
  , compileTasks
  , runFrontPasses
  , runBackPasses
  , runBackJobs
  , findProjectDir
  , compileOwnerLockPath
  , tryCompileOwnerLock
  , releaseCompileOwnerLock
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
  , updatePubHashCache
  , rootEligible
  , normalizePathSafe
  , isAbsolutePath
  , collapseDots
  , rebasePath
  ) where

import Prelude hiding (readFile, writeFile)

import qualified Acton.Parser
import Acton.Parser (CustomParseError, CustomParseException, ContextError, IndentationError)
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
import Acton.Prim (mPrim)
import qualified Acton.BuildSpec as BuildSpec
import qualified Acton.DocPrinter as DocP
import qualified Acton.Diagnostics as Diag
import qualified Acton.SourceProvider as Source
import Utils
import qualified Pretty
import qualified InterfaceFiles

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent (forkIO, myThreadId, threadCapability, threadDelay)
import Control.Concurrent.STM (TChan, TVar, atomically, check, modifyTVar', newTChanIO, newTVarIO, readTChan, readTVar, writeTChan)
import Control.DeepSeq (rnf)
import Control.Exception (Exception, IOException, SomeException, catch, displayException, evaluate, finally, mask_, throwIO, try)
import Control.Monad
import Data.Char (isAlpha, isHexDigit, isSpace)
import Data.Either (partitionEithers)
import Data.Graph
import Data.List (find, foldl', intercalate, intersperse, nub)
import qualified Data.List
import Data.IORef
import Data.Maybe (catMaybes, isJust, listToMaybe, mapMaybe)
import qualified Data.Map as M
import Data.Ord (Down(..))
import qualified Data.Set
import Data.Time.Clock (UTCTime)
import Error.Diagnose (Diagnostic)
import GHC.Conc (getNumCapabilities)
import System.Clock
import System.Directory
import System.Directory.Recursive
import System.Environment (getExecutablePath)
import System.FileLock (FileLock, SharedExclusive(Exclusive), tryLockFile, unlockFile)
import System.FilePath ((</>))
import System.FilePath.Posix
import System.Exit (ExitCode(..))
import System.IO hiding (readFile, writeFile)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readCreateProcessWithExitCode, proc)
import Text.PrettyPrint (renderStyle, style, Style(..), Mode(PageMode))
import Text.Show.Pretty (ppDoc)
import Text.Printf

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base16 as Base16
import qualified Crypto.Hash.SHA256 as SHA256


newtype ProjectError = ProjectError String deriving (Show)

instance Exception ProjectError

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
  = BackJobOk (Maybe TimeSpec)
  | BackJobFailed BackPassFailure
  deriving (Eq, Show)


data CompileCallbacks = CompileCallbacks
  { ccOnDiagnostics :: GlobalTask -> C.CompileOptions -> [Diagnostic String] -> IO ()
  , ccOnFrontResult :: GlobalTask -> C.CompileOptions -> FrontResult -> IO ()
  , ccOnFrontStart :: GlobalTask -> C.CompileOptions -> IO ()
  , ccOnFrontDone :: GlobalTask -> C.CompileOptions -> IO ()
  , ccOnBackJob :: BackJob -> IO ()
  , ccOnInfo :: String -> IO ()
  }

-- | Default no-op callbacks for the compilation pipeline.
-- Callers can start from this and override only the events they care about.
defaultCompileCallbacks :: CompileCallbacks
defaultCompileCallbacks = CompileCallbacks
  { ccOnDiagnostics = \_ _ _ -> return ()
  , ccOnFrontResult = \_ _ _ -> return ()
  , ccOnFrontStart = \_ _ -> return ()
  , ccOnFrontDone = \_ _ -> return ()
  , ccOnBackJob = \_ -> return ()
  , ccOnInfo = \_ -> return ()
  }

data BackJobCallbacks = BackJobCallbacks
  { bjcOnStart :: BackJob -> IO ()
  , bjcOnDone :: BackJob -> BackJobResult -> IO ()
  }

-- | Default no-op callbacks for back-pass execution.
defaultBackJobCallbacks :: BackJobCallbacks
defaultBackJobCallbacks =
  BackJobCallbacks
    { bjcOnStart = \_ -> return ()
    , bjcOnDone = \_ _ -> return ()
    }

data BackQueue = BackQueue
  { backQueueEnqueue :: Int -> BackJob -> BackJobCallbacks -> IO Bool
  , backQueueWait :: Int -> IO (Maybe BackPassFailure)
  }

data BuildSpecStamp = BuildSpecStamp
  { bssActonToml :: Maybe UTCTime
  , bssBuildAct :: Maybe UTCTime
  , bssBuildJson :: Maybe UTCTime
  } deriving (Eq, Show)

readBuildSpecStamp :: FilePath -> IO BuildSpecStamp
readBuildSpecStamp projDir = do
  actonToml <- stampFor "Acton.toml"
  buildAct <- stampFor "Build.act"
  buildJson <- stampFor "build.act.json"
  return BuildSpecStamp
    { bssActonToml = actonToml
    , bssBuildAct = buildAct
    , bssBuildJson = buildJson
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
                res <- (try $ runBackPasses gopts (bjOpts job) (bjPaths job) (bjInput job) shouldWrite)
                        :: IO (Either SomeException (Maybe TimeSpec))
                currentDone <- readIORef genRef
                when (currentDone == gen) $
                  case res of
                    Left err -> do
                      let key = TaskKey (projPath (bjPaths job)) (A.modname (biTypedMod (bjInput job)))
                          failure = BackPassFailure key (displayException err)
                      atomically $ modifyTVar' failures (recordFailure gen failure)
                      bjcOnDone callbacks job (BackJobFailed failure)
                    Right t -> bjcOnDone callbacks job (BackJobOk t)
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

-- | Start a new compile action, canceling any in-flight run.
-- Returns the generation id associated with this run.
startCompile :: CompileScheduler -> Int -> (Int -> IO ()) -> IO Int
startCompile sched delay run = do
  gen <- atomicModifyIORef' (csGenRef sched) $ \g -> let g' = g + 1 in (g', g')
  modifyMVar_ (csAsyncRef sched) $ \m -> do
    forM_ m cancel
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
            , projBuildSpec = Nothing
            , projLocks = joinPath [projPath pathsRoot, ".actonc.lock"]
            , projDeps = []
            }
      return (M.singleton rootProj ctx')
    else discoverProjects sysAbs rootProj depOverrides
  (globalTasks, _) <- buildGlobalTasks sp gopts opts' projMap
    (if incremental || allowPrune then Nothing else Just srcFiles)
  neededTasks <- case mChangedPaths of
    Nothing -> selectNeededTasks pathsRoot rootProj globalTasks srcFiles
    Just changed -> selectAffectedTasks globalTasks changed
  let rootTasks = [ gtTask t | t <- neededTasks, tkProj (gtKey t) == rootProj ]
      rootPins = maybe M.empty BuildSpec.dependencies (projBuildSpec =<< M.lookup rootProj projMap)
  return CompilePlan
    { cpContext = ctx
    , cpProjMap = projMap
    , cpGlobalTasks = globalTasks
    , cpNeededTasks = neededTasks
    , cpRootTasks = rootTasks
    , cpRootPins = rootPins
    , cpIncremental = incremental
    , cpAllowPrune = allowPrune'
    , cpChangedPaths = mChangedPaths
    , cpSrcFiles = srcFiles
    }

data CompileHooks = CompileHooks
  { chOnDiagnostics :: GlobalTask -> C.CompileOptions -> [Diagnostic String] -> IO ()
  , chOnFrontStart :: GlobalTask -> IO ()
  , chOnFrontDone :: GlobalTask -> IO ()
  , chOnFrontResult :: GlobalTask -> FrontResult -> IO ()
  , chOnBackQueued :: TaskKey -> Bool -> IO ()
  , chOnBackStart :: BackJob -> IO ()
  , chOnBackDone :: BackJob -> BackJobResult -> IO ()
  , chOnInfo :: String -> IO ()
  }

defaultCompileHooks :: CompileHooks
defaultCompileHooks =
  CompileHooks
    { chOnDiagnostics = \_ _ _ -> return ()
    , chOnFrontStart = \_ -> return ()
    , chOnFrontDone = \_ -> return ()
    , chOnFrontResult = \_ _ -> return ()
    , chOnBackQueued = \_ _ -> return ()
    , chOnBackStart = \_ -> return ()
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
        , bjcOnDone = chOnBackDone hooks
        }
      callbacks = defaultCompileCallbacks
        { ccOnDiagnostics = \t optsT diags -> chOnDiagnostics hooks t optsT diags
        , ccOnFrontResult = \t _ fr -> chOnFrontResult hooks t fr
        , ccOnFrontStart = \t _ -> chOnFrontStart hooks t
        , ccOnFrontDone = \t _ -> chOnFrontDone hooks t
        , ccOnBackJob = \job -> do
            let key = TaskKey (projPath (bjPaths job)) (A.modname (biTypedMod (bjInput job)))
            enqueued <- backQueueEnqueue backQueue gen job backCallbacks
            chOnBackQueued hooks key enqueued
        , ccOnInfo = chOnInfo hooks
        }
  compileTasks sp gopts opts' pathsRoot rootProj (cpNeededTasks plan) callbacks
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
    , C.ty = False
    , C.cpedantic = False
    , C.dbg_no_lines = False
    , C.optimize = C.Debug
    , C.only_build = False
    , C.skip_build = False
    , C.watch = False
    , C.no_threads = False
    , C.root = ""
    , C.tempdir = ""
    , C.syspath = ""
    , C.target = C.defTarget
    , C.cpu = ""
    , C.test = False
    , C.print_test_bins = True
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
-- Used when creating directories before writing .ty files.
getModPath :: FilePath -> A.ModName -> FilePath
getModPath path mn =
  joinPath [path, joinPath $ init $ A.modPath mn]

-- Global caches (process‑wide) to reduce repeated .ty lookups during parallel builds
--
-- We deliberately create top‑level MVars via unsafePerformIO and mark them
-- NOINLINE so their initializer runs exactly once. Without NOINLINE, GHC could
-- inline or float the initializer, accidentally creating multiple MVars under
-- optimization. This guarantees a single cache per process.
--
-- Thread‑safety: all access goes through modifyMVar/modifyMVar_ so read/modify/
-- write is atomic. These caches are best‑effort for performance; correctness
-- does not depend on them. On a miss we fall back to reading .ty headers from
-- disk. After a successful compile we update pubHashCache so dependents in
-- this process see the new public hash.
--
-- tyPathCache    :: ModName -> absolute .ty path (resolved from searchPath)
-- pubHashCache :: ModName -> current public hash (from header or compile)
-- nameHashCache  :: ModName -> per-name hash info (from header or compile)
{-# NOINLINE tyPathCache #-}
tyPathCache :: MVar (M.Map A.ModName FilePath)
tyPathCache = unsafePerformIO (newMVar M.empty)

{-# NOINLINE pubHashCache #-}
pubHashCache :: MVar (M.Map A.ModName B.ByteString)
pubHashCache = unsafePerformIO (newMVar M.empty)

{-# NOINLINE nameHashCache #-}
nameHashCache :: MVar (M.Map A.ModName (M.Map A.Name InterfaceFiles.NameHashInfo))
nameHashCache = unsafePerformIO (newMVar M.empty)

-- | Resolve the on-disk .ty path for a module, using a process-wide cache.
-- Avoids repeated filesystem walks when many modules share dependencies.
getTyFileCached :: [FilePath] -> A.ModName -> IO (Maybe FilePath)
getTyFileCached spaths mn = modifyMVar tyPathCache $ \m -> do
  case M.lookup mn m of
    Just p  -> return (m, Just p)
    Nothing -> do
      mty <- Acton.Env.findTyFile spaths mn
      case mty of
        Just p  -> return (M.insert mn p m, Just p)
        Nothing -> return (m, Nothing)

-- | Read a module's public hash using the cache and .ty header.
-- This drives dependency invalidation when an imported interface changes.
getPubHashCached :: Paths -> A.ModName -> IO (Maybe B.ByteString)
getPubHashCached paths mn = modifyMVar pubHashCache $ \m -> do
  case M.lookup mn m of
    Just ih -> return (m, Just ih)
    Nothing -> do
      mty <- getTyFileCached (searchPath paths) mn
      case mty of
        Just ty -> do
          hdrE <- (try :: IO a -> IO (Either SomeException a)) $ InterfaceFiles.readHeader ty
          case hdrE of
            Right (_srcH, ih, _implH, _impsH, _nameHashesH, _rootsH, _docH) -> return (M.insert mn ih m, Just ih)
            _ -> return (m, Nothing)
        Nothing -> return (m, Nothing)

-- | Update the public-hash cache after a successful compile.
-- Keeps in-process dependency checks consistent for later modules.
updatePubHashCache :: A.ModName -> B.ByteString -> IO ()
updatePubHashCache mn ih = modifyMVar_ pubHashCache $ \m -> return (M.insert mn ih m)

-- | Build a name hash map keyed by Name.
nameHashMapFromList :: [InterfaceFiles.NameHashInfo] -> M.Map A.Name InterfaceFiles.NameHashInfo
nameHashMapFromList infos = M.fromList [ (InterfaceFiles.nhName i, i) | i <- infos ]

-- | Read a module's per-name hash map using the cache and .ty header.
getNameHashMapCached :: Paths -> A.ModName -> IO (Maybe (M.Map A.Name InterfaceFiles.NameHashInfo))
getNameHashMapCached paths mn = modifyMVar nameHashCache $ \m -> do
  case M.lookup mn m of
    Just hm -> return (m, Just hm)
    Nothing -> do
      mty <- getTyFileCached (searchPath paths) mn
      case mty of
        Just ty -> do
          hdrE <- (try :: IO a -> IO (Either SomeException a)) $ InterfaceFiles.readHeader ty
          case hdrE of
            Right (_srcH, _ih, _implH, _impsH, nameHashes, _rootsH, _docH) -> do
              let hm = nameHashMapFromList nameHashes
              return (M.insert mn hm m, Just hm)
            _ -> return (m, Nothing)
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

-- | Emit diagnostics when a dependency .ty file is missing or unreadable.
-- Anchors the error to the owning module's filename for consistent reporting.
missingIfaceDiagnostics :: A.ModName -> String -> A.ModName -> [Diagnostic String]
missingIfaceDiagnostics ownerMn src missingMn =
    errsToDiagnostics "Compilation error" (modNameToFilename ownerMn) src
      [(NoLoc, "Type interface file not found or unreadable for " ++ modNameToString missingMn)]

-- | Parse a module from source text, returning diagnostics on failure.
-- Wraps parser, context, and indentation errors into a uniform format.
parseActSource :: A.ModName -> FilePath -> String -> IO (Either [Diagnostic String] A.Module)
parseActSource mn actFile srcContent = do
  (Right <$> Acton.Parser.parseModule mn actFile srcContent)
    `catch` handleParseBundle
    `catch` handleCustomParse
    `catch` handleContextError
    `catch` handleIndentationError
  where
    handleParseBundle :: ParseErrorBundle String CustomParseError -> IO (Either [Diagnostic String] A.Module)
    handleParseBundle bundle =
      return $ Left [Diag.parseDiagnosticFromBundle actFile srcContent bundle]

    handleCustomParse :: CustomParseException -> IO (Either [Diagnostic String] A.Module)
    handleCustomParse err =
      return $ Left [Diag.customParseExceptionToDiagnostic actFile srcContent err]

    handleContextError :: ContextError -> IO (Either [Diagnostic String] A.Module)
    handleContextError err =
      return $ Left (errsToDiagnostics "Context error" (modNameToFilename mn) srcContent (Acton.Parser.contextError err))

    handleIndentationError :: IndentationError -> IO (Either [Diagnostic String] A.Module)
    handleIndentationError err =
      return $ Left (errsToDiagnostics "Indentation error" (modNameToFilename mn) srcContent (Acton.Parser.indentationError err))

-- | Parse a SourceSnapshot with a display path relative to cwd.
-- Keeps diagnostics stable regardless of absolute paths or overlays.
parseActSnapshot :: A.ModName -> FilePath -> Source.SourceSnapshot -> IO (Either [Diagnostic String] A.Module)
parseActSnapshot mn actFile snap = do
  cwd <- getCurrentDirectory
  let displayFile = makeRelative cwd actFile
  parseActSource mn displayFile (Source.ssText snap)

-- | Read and parse a source file via SourceProvider (overlay-aware).
-- Returns both the snapshot and the parsed AST for reuse by callers.
parseActFile :: Source.SourceProvider -> A.ModName -> FilePath -> IO (Either [Diagnostic String] (Source.SourceSnapshot, A.Module))
parseActFile sp mn actFile = do
  snap <- Source.readSource sp actFile
  emod <- parseActSnapshot mn actFile snap
  return $ fmap (\m -> (snap, m)) emod


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

data FrontResult = FrontResult
  { frIfaceTE  :: [(A.Name, I.NameInfo)]
  , frDoc      :: Maybe String
  , frPubHash :: B.ByteString
  , frNameHashes :: [InterfaceFiles.NameHashInfo]
  , frFrontTime :: Maybe TimeSpec
  , frBackJob  :: Maybe BackJob
  }

data CompileTask        = ActonTask { name :: A.ModName, src :: String, srcBytes :: B.ByteString, atree:: A.Module }
                        | TyTask    { name :: A.ModName
                                    , tyHash :: B.ByteString               -- raw source bytes hash
                                    , tyPubHash :: B.ByteString          -- module public hash
                                    , tyImplHash :: B.ByteString         -- module impl hash
                                    , tyImports :: [(A.ModName, B.ByteString)] -- imports with pub hash used
                                    , tyNameHashes :: [InterfaceFiles.NameHashInfo]
                                    , tyRoots :: [A.Name]
                                    , tyDoc :: Maybe String
                                    , iface :: I.NameInfo
                                    , typed :: A.Module
                                    }
                        | ParseErrorTask { name :: A.ModName, parseDiagnostics :: [Diagnostic String] }

instance Show CompileTask where
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
importsOf (ActonTask _ _ _ m) = A.importsOf m
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
-- unrelated modules untouched.
selectAffectedTasks :: [GlobalTask] -> [FilePath] -> IO [GlobalTask]
selectAffectedTasks globalTasks changedFiles = do
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
                keepProviders t =
                  t { gtImportProviders = M.filter (`Data.Set.member` affected) (gtImportProviders t) }
            return [ keepProviders t | t <- globalTasks, Data.Set.member (gtKey t) affected ]
  where
    reverseClosure revMap start = go start (Data.Set.toList start)
      where
        go seen [] = seen
        go seen (k:ks) =
          let ds = M.findWithDefault [] k revMap
              new = filter (`Data.Set.notMember` seen) ds
              seen' = foldl' (flip Data.Set.insert) seen new
          in go seen' (ks ++ new)


-- | Prepare a task for dependency graph construction.
-- Prefer reading imports from .ty header; if no .ty, parse the source.
-- Decide how to represent a module for the graph:
-- 1) If .ty is missing/unreadable -> parse .act to obtain imports (ActonTask).
-- 2) If .ty exists and .act mtime < .ty mtime (and the acton binary isn't newer
--    than the .ty unless --ignore-compiler-version is set) -> trust header imports (TyTask).
-- 2b) If an in-memory overlay exists, skip source mtime and compare its hash to header
--     (unless the compiler is newer and --ignore-compiler-version is not set).
-- 3) If .act appears newer than .ty -> verify by content hash:
--      - If stored moduleSrcBytesHash == current hash -> header is still valid (TyTask)
--      - Else -> parse .act to obtain accurate imports (ActonTask)
-- Returns either a header-only TyTask stub or an ActonTask; no heavy decoding.
--
-- This is the core of incremental graph construction: it avoids parsing when
-- cached interfaces are reliable, while still falling back to parsing when
-- source changes are detected.
readModuleTask :: Source.SourceProvider -> C.GlobalOptions -> C.CompileOptions -> Paths -> String -> IO CompileTask
readModuleTask sp gopts opts paths actFile = do
    let mn      = modName paths
        tyFile  = outBase paths mn ++ ".ty"
    tyExists <- doesFileExist tyFile
    if not tyExists
      then parseForImports mn
      else do
        -- .ty exists: read header for hashes/imports and compare timestamps
        hdrE <- (try :: IO a -> IO (Either SomeException a)) $ InterfaceFiles.readHeader tyFile
        case hdrE of
          Left _ -> parseForImports mn
          Right (moduleSrcBytesHash, modulePubHash, moduleImplHash, imps, nameHashes, roots, mdoc) -> do
            tyTime <- getModificationTime tyFile
            newCompiler <- compilerNewerThan tyTime
            mOverlay <- Source.spReadOverlay sp actFile
            case mOverlay of
              Just snap ->
                if newCompiler
                  then parseFromSnapshot mn snap
                  else verifyOrParse mn snap moduleSrcBytesHash modulePubHash moduleImplHash imps nameHashes roots mdoc
              Nothing -> do
                actTime <- Source.spGetModTime sp actFile
                -- Equal mtimes are ambiguous with coarse timestamp resolution,
                -- so treat them as stale and re-hash the source.
                if newCompiler
                  then parseForImports mn
                  else if actTime < tyTime
                    then return (mkTyTask mn moduleSrcBytesHash modulePubHash moduleImplHash imps nameHashes roots mdoc)
                    else do
                      snap <- Source.spReadFile sp actFile
                      verifyOrParse mn snap moduleSrcBytesHash modulePubHash moduleImplHash imps nameHashes roots mdoc
  where
    mkTyTask mn moduleSrcBytesHash modulePubHash moduleImplHash imps nameHashes roots mdoc =
      let nmodStub = I.NModule [] mdoc
          tmodStub = A.Module mn [] []
      in TyTask { name      = mn
                , tyHash     = moduleSrcBytesHash
                , tyPubHash  = modulePubHash
                , tyImplHash = moduleImplHash
                , tyImports  = imps
                , tyNameHashes = nameHashes
                , tyRoots   = roots
                , tyDoc     = mdoc
                , iface     = nmodStub
                , typed     = tmodStub }

    parseForImports mn = do
      parsedRes <- parseActFile sp mn actFile
      case parsedRes of
        Left diags -> return $ ParseErrorTask mn diags
        Right (snap, m) -> return $ ActonTask mn (Source.ssText snap) (Source.ssBytes snap) m

    parseFromSnapshot mn snap = do
      emod <- parseActSnapshot mn actFile snap
      case emod of
        Left diags -> return $ ParseErrorTask mn diags
        Right m -> return $ ActonTask mn (Source.ssText snap) (Source.ssBytes snap) m

    compilerNewerThan tyTime
      | C.ignore_compiler_version opts = return False
      | otherwise = do
          exePathE <- (try getExecutablePath :: IO (Either SomeException FilePath))
          case exePathE of
            Left _ -> return False
            Right exePath -> do
              exeTimeE <- (try (getModificationTime exePath) :: IO (Either SomeException UTCTime))
              case exeTimeE of
                Left _ -> return False
                Right exeTime -> return (exeTime > tyTime)

    verifyOrParse mn snap moduleSrcBytesHash modulePubHash moduleImplHash imps nameHashes roots mdoc = do
      let curHash = SHA256.hash (Source.ssBytes snap)
          short8 bs = take 8 (B.unpack $ Base16.encode bs)
          same = curHash == moduleSrcBytesHash
      when (C.verbose gopts && Source.ssIsOverlay snap) $ do
        actTime <- Source.spGetModTime sp actFile
        tyTime <- getModificationTime (outBase paths mn ++ ".ty")
        let len = B.length (Source.ssBytes snap)
        putStrLn ("[debug] readModuleTask " ++ modNameToString mn
                  ++ " act=" ++ show actTime
                  ++ " ty=" ++ show tyTime
                  ++ " hash=" ++ short8 curHash
                  ++ " header=" ++ short8 moduleSrcBytesHash
                  ++ " len=" ++ show len
                  ++ if same then " (match)" else " (diff)")
      if same
        then return (mkTyTask mn moduleSrcBytesHash modulePubHash moduleImplHash imps nameHashes roots mdoc)
        else parseFromSnapshot mn snap


-- | Recursively read imports for a set of tasks within the same project.
-- Any missing module is added by parsing or reading its .ty header via
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

-- | Read an interface from a .ty file and return its NameInfo and public hash.
-- This is used when a module is deemed fresh and we want to avoid reparsing.
readIfaceFromTy :: Paths -> A.ModName -> String -> Maybe B.ByteString -> IO (Either [Diagnostic String] ([(A.Name, I.NameInfo)], Maybe String, B.ByteString))
readIfaceFromTy paths mn src mHash = do
    mty <- Acton.Env.findTyFile (searchPath paths) mn
    case mty of
      Nothing -> return $ Left (missingIfaceDiagnostics mn src mn)
      Just tyF -> do
        fileRes <- (try :: IO a -> IO (Either SomeException a)) $ InterfaceFiles.readFile tyF
        case fileRes of
          Left _ -> return $ Left (missingIfaceDiagnostics mn src mn)
          Right (_ms, nmod, _tmod, _si, _ti, _implH, _ni, _nameHashes, _te, _tm) -> do
            let I.NModule te mdoc = nmod
            ih <- case mHash of
                    Just h -> return h
                    Nothing -> do
                      hdrE <- (try :: IO a -> IO (Either SomeException a)) $ InterfaceFiles.readHeader tyF
                      case hdrE of
                        Right (_srcH, ihash, _implH, _impsH, _nameHashesH, _rootsH, _docH) -> return ihash
                        _ -> return B.empty
            return $ Right (te, mdoc, ih)


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
-- hashes plus module pub/impl hashes, and writes the .ty header. Returns the
-- front result plus a BackJob for later passes when compilation is needed.
runFrontPasses :: C.GlobalOptions
               -> C.CompileOptions
               -> Paths
               -> Acton.Env.Env0
               -> A.Module
               -> String
               -> B.ByteString
               -> (A.ModName -> IO (Maybe B.ByteString))
               -> (A.ModName -> IO (Maybe (M.Map A.Name InterfaceFiles.NameHashInfo)))
               -> IO (Either [Diagnostic String] FrontResult)
runFrontPasses gopts opts paths env0 parsed srcContent srcBytes resolveImportHash resolveNameHashMap = do
  createDirectoryIfMissing True (getModPath (projTypes paths) mn)
  core
    `catch` handleGeneral
    `catch` handleCompilation
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

    core = do
      timeStart <- getTime Monotonic
      let isRoot = mn == modName paths
      when (C.parse opts && isRoot) $
        dump mn "parse" (Pretty.print parsed)
      when (C.parse_ast opts && isRoot) $
        dump mn "parse-ast" (renderStyle prettyAstStyle (ppDoc parsed))

      env <- Acton.Env.mkEnv (searchPath paths) env0 parsed
      timeEnv <- getTime Monotonic
      iff (C.timing gopts) $ putStrLn("    Pass: Make environment: " ++ fmtTime (timeEnv - timeStart))

      kchecked <- Acton.Kinds.check env parsed
      iff (C.kinds opts && isRoot) $ dump mn "kinds" (Pretty.print kchecked)
      timeKindsCheck <- getTime Monotonic
      iff (C.timing gopts) $ putStrLn("    Pass: Kinds check     : " ++ fmtTime (timeKindsCheck - timeEnv))

      -- Type-check and return both the typed AST and the interface NameInfo.
      (nmod,tchecked,typeEnv,mrefs) <- Acton.Types.reconstruct env kchecked
      -- Module-level src hash uses raw bytes so any source edit forces re-parse.
      let moduleSrcBytesHash = SHA256.hash srcBytes
      -- Store roots so later builds can discover entry points without reparse.
      let I.NModule iface mdoc = nmod
      let roots = [ n | (n,i) <- iface, rootEligible i ]
      -- Import hashes are recorded in the .ty header so dep changes can be detected.
      impsRes <- resolveImportHashes mrefs
      case impsRes of
        Left diags -> return (Left diags)
        Right impsWithHash -> do
          -- Extract top-level items from parsed and typed ASTs for per-name hashes.
          let srcItems = Hashing.topLevelItems parsed
              implItems = Hashing.topLevelItems tchecked
              -- src hashes come only from parsed AST fragments.
              nameSrcHashes = Hashing.nameHashesFromItems srcItems
              -- impl hashes are derived from the typed AST bodies. this is the
              -- local function hash, implDeps are added later
              nameImplHashes = Hashing.nameHashesFromItems implItems
              -- NameInfo defines the public interface (signatures, classes, protocols).
              nameInfoMap = M.fromList iface
              nameSrcKeys = M.keysSet nameSrcHashes
              nameImplKeys = M.keysSet nameImplHashes
              nameKeys = Data.Set.union nameSrcKeys nameImplKeys
              nameLocsParsed = M.fromListWith (\a _ -> a)
                [ (n, A.dloc d) | Hashing.TLDecl n d <- srcItems ] `M.union`
                M.fromListWith (\a _ -> a)
                [ (n, A.sloc s) | Hashing.TLStmt n s <- srcItems ]
              nameLocsTyped = M.fromListWith (\a _ -> a)
                [ (n, A.dloc d) | Hashing.TLDecl n d <- implItems ] `M.union`
                M.fromListWith (\a _ -> a)
                [ (n, A.sloc s) | Hashing.TLStmt n s <- implItems ]
              nameLocs = M.union nameLocsParsed nameLocsTyped
          -- pubSigDeps: signature-level deps from NameInfo (types only).
          let isDerivedName n = case n of
                A.Derived{} -> True
                _ -> False
              isDerivedQName qn = case qn of
                A.GName _ n -> isDerivedName n
                A.QName _ n -> isDerivedName n
                A.NoQ n -> isDerivedName n
              dropDerived = filter (not . isDerivedQName)
              pubSigDepsRaw = M.fromList
                [ (n, dropDerived (Names.freeQ info)) | (n, info) <- M.toList nameInfoMap ]
              -- implDeps: term-level deps from typed bodies.
              implDepsRaw = Hashing.implDepsFromItems implItems
              -- pubDeps include signature deps plus any term-level deps for reuse in pubHash.
              -- Derived names are internal and should never require a pub hash.
              pubDepsRaw = M.map dropDerived (M.unionWith (++) pubSigDepsRaw implDepsRaw)
              hashEnv = setMod mn env
              -- Split deps into local (same module) vs external (qualified) names.
              (pubSigLocalDeps, pubSigExtDeps) = Hashing.splitDeps mn hashEnv nameKeys pubSigDepsRaw
              (_, pubExtDeps) = Hashing.splitDeps mn hashEnv nameKeys pubDepsRaw
              (implLocalDeps, implExtDeps) = Hashing.splitDeps mn hashEnv nameKeys implDepsRaw
              -- Load .ty maps for any external modules referenced by deps.
              extMods = Data.Set.toList (Hashing.externalModules pubExtDeps `Data.Set.union` Hashing.externalModules implExtDeps)
          extMapsRes <- resolveNameHashMaps extMods
          case extMapsRes of
            Left diags -> return (Left diags)
            Right extMaps -> do
              -- Resolve external deps to their recorded hashes.
              let pubSigExtRes = resolveDepHashes "pub" InterfaceFiles.nhPubHash pubSigExtDeps extMaps nameLocs
                  pubExtRes = resolveDepHashes "pub" InterfaceFiles.nhPubHash pubExtDeps extMaps nameLocs
                  implExtRes = resolveDepHashes "impl" InterfaceFiles.nhImplHash implExtDeps extMaps nameLocs
              case (pubSigExtRes, pubExtRes, implExtRes) of
                (Left diags, _, _) -> return (Left diags)
                (_, Left diags, _) -> return (Left diags)
                (_, _, Left diags) -> return (Left diags)
                (Right pubSigExtHashes, Right pubExtHashes, Right implExtHashes) -> do
                  -- Build per-name hash records (src/pub/impl + deps) for .ty.
                  let nameHashes =
                        Hashing.buildNameHashes
                          nameKeys
                          nameSrcHashes
                          nameImplHashes
                          nameInfoMap
                          pubSigLocalDeps
                          pubSigExtHashes
                          implLocalDeps
                          implExtHashes
                          pubExtHashes

                  -- Module-level hashes summarize the per-name hashes.
                  let modulePubHash = Hashing.modulePubHashFromIface nmod nameHashes
                      moduleImplHash = Hashing.moduleImplHashFromNameHashes nameHashes
                  -- Write .ty now so later builds can reuse this front-pass work.
                  InterfaceFiles.writeFile (outbase ++ ".ty") moduleSrcBytesHash modulePubHash moduleImplHash impsWithHash nameHashes roots mdoc nmod tchecked

                  iff (C.types opts && isRoot) $ dump mn "types" (Pretty.print tchecked)
                  iff (C.sigs opts && isRoot) $ dump mn "sigs" (Acton.Types.prettySigs env mn iface)

                  -- Generate documentation, if building for a project
                  when (not (C.skip_build opts) && not (isTmp paths)) $ do
                    let docDir = joinPath [projPath paths, "out", "doc"]
                        modPathList = A.modPath mn
                        docFile = if null modPathList
                                  then docDir </> "unnamed" <.> "html"
                                  else joinPath (docDir : init modPathList) </> last modPathList <.> "html"
                        docFileDir = takeDirectory docFile
                        -- Get the type environment for this module
                        modTypeEnv = case Acton.Env.lookupMod mn typeEnv of
                          Just te -> te
                          Nothing -> iface
                        -- Apply the same simplification as --sigs uses
                        env1 = define iface $ setMod mn env
                        simplifiedTypeEnv = simp env1 modTypeEnv
                    createDirectoryIfMissing True docFileDir
                    -- Use parsed (original AST) to preserve docstrings
                    let htmlDoc = DocP.printHtmlDoc (I.NModule simplifiedTypeEnv mdoc) parsed
                    writeFile docFile htmlDoc

                  timeTypeCheck <- getTime Monotonic
                  iff (C.timing gopts) $ putStrLn("    Pass: Type check      : " ++ fmtTime (timeTypeCheck - timeKindsCheck))

                  timeFrontEnd <- getTime Monotonic
                  let frontTime = timeFrontEnd - timeStart
                      frontTimeMaybe = if not (quiet gopts opts)
                                         then Just frontTime
                                         else Nothing
                      backJob = Just BackJob { bjPaths = paths
                                             , bjOpts = opts
                                             , bjInput = BackInput { biTypeEnv = typeEnv
                                                                  , biTypedMod = tchecked
                                                                  , biSrc = srcContent
                                                                  , biImplHash = moduleImplHash
                                                                  }
                                             }
                  return $ Right FrontResult { frIfaceTE = iface
                                             , frDoc = mdoc
                                             , frPubHash = modulePubHash
                                             , frNameHashes = nameHashes
                                             , frFrontTime = frontTimeMaybe
                                             , frBackJob = backJob
                                             }


-- | Run the back passes for a single module.
-- Executes normalization through codegen, writes .c/.h output as needed, and
-- returns the back-pass elapsed time for logging.
runBackPasses :: C.GlobalOptions -> C.CompileOptions -> Paths -> BackInput -> IO Bool -> IO (Maybe TimeSpec)
runBackPasses gopts opts paths backInput shouldWrite = do
      let mn = A.modname (biTypedMod backInput)
          outbase = outBase paths mn
          relSrcBase = makeRelative (projPath paths) (srcBase paths mn)
      timeStart <- getTime Monotonic

      (normalized, normEnv) <- Acton.Normalizer.normalize (biTypeEnv backInput) (biTypedMod backInput)
      iff (C.norm opts && mn == (modName paths)) $ dump mn "norm" (Pretty.print normalized)
      timeNormalized <- getTime Monotonic
      iff (C.timing gopts) $ putStrLn("    Pass: Normalizer      : " ++ fmtTime (timeNormalized - timeStart))

      (deacted,deactEnv) <- Acton.Deactorizer.deactorize normEnv normalized
      iff (C.deact opts && mn == (modName paths)) $ dump mn "deact" (Pretty.print deacted)
      timeDeactorizer <- getTime Monotonic
      iff (C.timing gopts) $ putStrLn("    Pass: Deactorizer     : " ++ fmtTime (timeDeactorizer - timeNormalized))

      (cpstyled,cpsEnv) <- Acton.CPS.convert deactEnv deacted
      iff (C.cps opts && mn == (modName paths)) $ dump mn "cps" (Pretty.print cpstyled)
      timeCPS <- getTime Monotonic
      iff (C.timing gopts) $ putStrLn("    Pass: CPS             : " ++ fmtTime (timeCPS - timeDeactorizer))

      (lifted,liftEnv) <- Acton.LambdaLifter.liftModule cpsEnv cpstyled
      iff (C.llift opts && mn == (modName paths)) $ dump mn "llift" (Pretty.print lifted)
      timeLLift <- getTime Monotonic
      iff (C.timing gopts) $ putStrLn("    Pass: Lambda Lifting  : " ++ fmtTime (timeLLift - timeCPS))

      boxed <- Acton.Boxing.doBoxing liftEnv lifted
      iff (C.box opts && mn == (modName paths)) $ dump mn "box" (Pretty.print boxed)
      timeBoxing <- getTime Monotonic
      iff (C.timing gopts) $ putStrLn("    Pass: Boxing :          " ++ fmtTime (timeBoxing - timeLLift))

      let hexHash = B.unpack $ Base16.encode (biImplHash backInput)
          emitLines = not (C.dbg_no_lines opts)
      (n,h,c) <- Acton.CodeGen.generate liftEnv relSrcBase (biSrc backInput) emitLines boxed hexHash
      timeCodeGen <- getTime Monotonic
      iff (C.timing gopts) $ putStrLn("    Pass: Generating code : " ++ fmtTime (timeCodeGen - timeBoxing))
      let finish = do
            timeEnd <- getTime Monotonic
            let backTime = timeEnd - timeStart
            if not (quiet gopts opts)
              then return (Just backTime)
              else return Nothing
          forceOut s = evaluate (rnf s)

      if C.hgen opts
        then do
          forceOut h
          putStrLn h
          finish
        else if C.cgen opts
          then do
            forceOut c
            putStrLn c
            finish
          else do
            forceOut h
            forceOut c
            iff (not (altOutput opts)) (do
                ok <- shouldWrite
                when ok $ do
                  let cFile = outbase ++ ".c"
                      hFile = outbase ++ ".h"

                  writeFile hFile h
                  writeFile cFile c
                  let tyFileName = modNameToString(modName paths) ++ ".ty"
                  iff (C.ty opts) $
                       copyFileWithMetadata (joinPath [projTypes paths, tyFileName]) (joinPath [srcDir paths, tyFileName])

                  timeCodeWrite <- getTime Monotonic
                  iff (C.timing gopts) $ putStrLn("    Pass: Writing code    : " ++ fmtTime (timeCodeWrite - timeCodeGen))
                                     )
            finish


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
--   3) Walk modules in topo order, deciding per module whether to compile or reuse cached .ty,
--      and update the shared environment.
--   4) Track per-name pub/impl hashes and only redo the work that changed:
--      pub changes trigger front passes; impl changes trigger back passes (with
--      an impl-hash refresh); codegen hash mismatches trigger back passes only.
--
-- Key ideas (ActonTask vs TyTask caching):
--   - TyTask is lightweight, read from the .ty header: moduleSrcBytesHash, modulePubHash,
--     moduleImplHash, imports annotated with the pub hash used, per-name hashes (src/pub/impl
--     + deps), roots, and docstring. It avoids decoding heavy sections.
--   - ActonTask carries parsed source and must be compiled.
--   - pubMap :: Map TaskKey ByteString and nameMap :: Map TaskKey (Map Name NameHashInfo) are
--     maintained during the run. TyTask compares recorded dependency hashes against current
--     provider hashes (in-graph via pubMap/nameMap, otherwise via cached .ty headers). Pub deltas
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
             -> CompileCallbacks
             -> IO (Either CompileFailure (Acton.Env.Env0, Bool))
compileTasks sp gopts opts rootPaths rootProj tasks callbacks = do
    runningRef <- newIORef []
    let cancelRunning = readIORef runningRef >>= mapM_ cancel
    let compileMain = do
          -- Reject cycles
          if not (null cycles)
            then return $ Left (CompileCycleFailure ("Cyclic imports: " ++ concatMap showTaskGraph cycles))
            else do
              -- Compile __builtin__ first if present anywhere in the graph
              case builtinOrder of
                [t] -> do
                  res <- compileBuiltin t
                  case res of
                    Left err -> return (Left err)
                    Right () -> continue runningRef
                _ -> continue runningRef
    compileMain `finally` cancelRunning
  where
    continue runningRef = do
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

      (envFinal, hadErrors) <- loop runningRef initialReady [] M.empty M.empty indeg pending0 baseEnv False maxParallel cwMap
      return (Right (envFinal, hadErrors))

    -- Basic maps/sets ----------------------------------------------------
    taskMap = M.fromList [ (gtKey t, t) | t <- tasks ]
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
    initialReady = [ k | (k,d) <- M.toList indeg, d == 0 ]
    pending0 = Data.Set.fromList (M.keys indeg)

    depOpts = opts { C.skip_build = True, C.test = False }
    optsFor k = if tkProj k == rootProj then opts else depOpts
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

    -- TODO: can we reintegrate this into the normal loop to avoid duplication?
    -- NOTE: FYI, it was originally part of the main loop but factored out for
    -- clarity when we changed to use async, front/back jobs etc. There were so
    -- many other changes so at the time it was easier to separate builtin
    -- compilation but perhaps we can find a way to merge it back in to one
    -- general loop.
    compileBuiltin :: GlobalTask -> IO (Either CompileFailure ())
    compileBuiltin t = do
      let bPaths = gtPaths t
          mn = name (gtTask t)
          optsBuiltin = optsFor (gtKey t)
          actFile = srcFile bPaths mn
          forceAlt = altOutput optsBuiltin && mn == rootAlt
      if C.only_build optsBuiltin
        then return (Right ())
        else do
          t' <- case gtTask t of
            TyTask{} | forceAlt -> do
              parsedRes <- parseActFile sp mn actFile
              case parsedRes of
                Left diags -> return (ParseErrorTask mn diags)
                Right (snap, m) -> return $ ActonTask mn (Source.ssText snap) (Source.ssBytes snap) m
            _ -> return (gtTask t)
          case t' of
            ParseErrorTask{ parseDiagnostics = diags } -> do
              ccOnDiagnostics callbacks t optsBuiltin diags
              return (Left CompileBuiltinFailure)
            TyTask{} -> return (Right ())
            ActonTask{ src = srcContent, srcBytes = srcBytes, atree = m } -> do
              ccOnFrontStart callbacks t optsBuiltin
              builtinEnv0 <- Acton.Env.initEnv (projTypes bPaths) True
              res <- runFrontPasses gopts optsBuiltin bPaths builtinEnv0 m srcContent srcBytes (getPubHashCached bPaths) (getNameHashMapCached bPaths)
              case res of
                Left diags -> do
                  ccOnFrontDone callbacks t optsBuiltin
                  ccOnDiagnostics callbacks t optsBuiltin diags
                  return (Left CompileBuiltinFailure)
                Right fr -> do
                  ccOnFrontDone callbacks t optsBuiltin
                  ccOnFrontResult callbacks t optsBuiltin fr
                  updatePubHashCache mn (frPubHash fr)
                  updateNameHashCache mn (frNameHashes fr)
                  forM_ (frBackJob fr) $ ccOnBackJob callbacks
                  return (Right ())

    -- One module ---------------------------------------------------------
    doOne :: Acton.Env.Env0
          -> M.Map TaskKey B.ByteString
          -> M.Map TaskKey (M.Map A.Name InterfaceFiles.NameHashInfo)
          -> TaskKey
          -> IO (TaskKey, Either [Diagnostic String] FrontResult)
    doOne envSnap pubMap nameMap key = do
      t <- case M.lookup key taskMap of
             Just x -> return x
             Nothing -> error ("Internal error: missing task for key " ++ show key)
      let paths = gtPaths t
          mn    = name (gtTask t)
          optsT = optsFor key
          providers = gtImportProviders t
          actFile = srcFile paths mn
          tyFile = outBase paths mn ++ ".ty"
          short8 bs   = take 8 (B.unpack $ Base16.encode bs)
          mkFrontResult ifaceTE mdoc pubHash nameHashes backJob =
            FrontResult
              { frIfaceTE = ifaceTE
              , frDoc = mdoc
              , frPubHash = pubHash
              , frNameHashes = nameHashes
              , frFrontTime = Nothing
              , frBackJob = backJob
              }
          emptyFrontResult =
            FrontResult
              { frIfaceTE = []
              , frDoc = Nothing
              , frPubHash = B.empty
              , frNameHashes = []
              , frFrontTime = Nothing
              , frBackJob = Nothing
              }
          cacheFrontResult fr = do
            updatePubHashCache mn (frPubHash fr)
            updateNameHashCache mn (frNameHashes fr)
            return (key, Right fr)
          readTyFile = do
            tyRes <- (try :: IO a -> IO (Either SomeException a)) $ InterfaceFiles.readFile tyFile
            case tyRes of
              Left _ -> return (Left (missingIfaceDiagnostics mn "" mn))
              Right ty -> return (Right ty)
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
                  Nothing -> error ("Internal error: missing pub hash for dep " ++ modNameToString m)
              Nothing -> getPubHashCached paths m
          resolveNameHashMap' m =
            case M.lookup m providers of
              Just depKey ->
                case M.lookup depKey nameMap of
                  Just hm -> return (Just hm)
                  Nothing -> error ("Internal error: missing name hashes for dep " ++ modNameToString m)
              Nothing -> getNameHashMapCached paths m

          missingNameHashDiagnostics qn =
            errsToDiagnostics "Compilation error" (modNameToFilename mn) ""
              [(NoLoc, "Hash info missing for " ++ prstr qn)]

          missingDepHashDiagnostics label qn users =
            errsToDiagnostics "Compilation error" (modNameToFilename mn) ""
              [(NoLoc, label ++ " hash missing for " ++ prstr qn ++ users)]

          collectDiags results =
            let (errs, vals) = partitionEithers results
            in if null errs
                 then Right vals
                 else Left (concat errs)
          traverseDiags f items = collectDiags <$> mapM f items

          depMap getDeps infos =
            M.fromListWith (\a _ -> a) (concatMap getDeps infos)

          depUsers getDeps infos =
            foldl' add M.empty infos
            where
              add acc info =
                foldl' (\m (qn, _) -> M.insertWith (++) qn [InterfaceFiles.nhName info] m) acc (getDeps info)

          fmtUsers users qn =
            case M.lookup qn users of
              Nothing -> ""
              Just ns ->
                let uniq = Data.Set.toList (Data.Set.fromList ns)
                    names = map A.nstr (Data.List.sortOn Hashing.nameKey uniq)
                in if null names
                     then ""
                     else " (used by " ++ intercalate ", " names ++ ")"

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

          checkDeps label getHash users deps = do
            resolved <- traverseDiags (\(qn, recorded) -> do
              let userNote = fmtUsers users qn
              currE <- resolveQNameHash label getHash userNote qn
              return (fmap (\curr -> (qn, recorded, curr)) currE)) (M.toList deps)
            return (fmap (\triples -> [ (qn, old, new) | (qn, old, new) <- triples, old /= new ]) resolved)

      case gtTask t of
        ParseErrorTask{ parseDiagnostics = diags } -> return (key, Left diags)
        _ | C.only_build optsT -> do
              ifaceRes <- case gtTask t of
                            TyTask{ tyPubHash = h } -> readIfaceFromTy paths mn "" (Just h)
                            ActonTask{ src = srcContent } -> readIfaceFromTy paths mn srcContent Nothing
              case ifaceRes of
                Right (ifaceTE, mdoc, ih) -> do
                  let cachedNameHashes = case gtTask t of
                        TyTask{ tyNameHashes = nhs } -> nhs
                        _ -> []
                      fr = mkFrontResult ifaceTE mdoc ih cachedNameHashes Nothing
                  cacheFrontResult fr
                Left _ ->
                  return (key, Right emptyFrontResult)
        _ -> do
          -- For cached .ty tasks, compare recorded dep hashes against current deps.
          -- This is the up-to-date check that decides if we can skip work. If
          -- any implDeps have changed we need to rerun out back passes and if
          -- any pubDeps have changed we need to rerun front passes (and back
          -- passes)
          needByDepsRes <- case gtTask t of
            TyTask{ tyNameHashes = nameHashes } -> do
              -- Build dep maps and reverse "used by" index for logging.
              let pubDeps = depMap InterfaceFiles.nhPubDeps nameHashes
                  implDeps = depMap InterfaceFiles.nhImplDeps nameHashes
                  pubUsers = depUsers InterfaceFiles.nhPubDeps nameHashes
                  implUsers = depUsers InterfaceFiles.nhImplDeps nameHashes
              -- Resolve current hashes for each dep and report any deltas.
              pubRes <- checkDeps "pub" InterfaceFiles.nhPubHash pubUsers pubDeps
              implRes <- checkDeps "impl" InterfaceFiles.nhImplHash implUsers implDeps
              case (pubRes, implRes) of
                (Left diags, _) -> return (Left diags)
                (_, Left diags) -> return (Left diags)
                (Right pubDeltas, Right implDeltas) ->
                  return (Right (pubDeltas, implDeltas, pubUsers, implUsers))
            -- Source tasks always run front passes, so deps are irrelevant.
            _ -> return (Right ([], [], M.empty, M.empty))

          case needByDepsRes of
            Left diags -> return (key, Left diags)
            Right (pubDeltas, implDeltas, pubUsers, implUsers) -> do
              let needBySource = case gtTask t of { ActonTask{} -> True; _ -> False }
                  -- Public deltas require front passes; impl deltas only need back jobs.
                  needByPub = not (null pubDeltas)
                  needByImpl = not (null implDeltas)
                  forceAlt    = altOutput optsT && mn == rootAlt
                  forceAlways = C.alwaysbuild optsT
                  -- Front passes run on source or API changes, or when forced.
                  needFront = needBySource || needByPub || forceAlt || forceAlways
                  mModuleImplHash = case gtTask t of
                    TyTask{ tyImplHash = implHash } -> Just implHash
                    _ -> Nothing
              let canCheckCodegen = not needFront && not needByImpl && not (altOutput optsT)
              mCodegenStatus <- case mModuleImplHash of
                Just implHash | canCheckCodegen -> Just <$> codegenStatus paths mn implHash
                _ -> return Nothing
              let needByCodegen = maybe False (not . codegenUpToDate) mCodegenStatus
              let runFront = do
                    prevNameHashes <- if C.verbose gopts
                      then case gtTask t of
                        TyTask{ tyNameHashes = nhs } -> return (Just (nameHashMapFromList nhs))
                        _ -> getNameHashMapCached paths mn
                      else return Nothing
                    when (C.verbose gopts) $ do
                      if needBySource
                        then ccOnInfo callbacks ("  Stale " ++ modNameToString mn ++ ": source changed")
                        else when needByPub $ do
                          let fmtDelta (qn, old, new) = prstr qn ++ " " ++ short8 old ++ " → " ++ short8 new ++ fmtUsers pubUsers qn
                          ccOnInfo callbacks ("  Stale " ++ modNameToString mn ++ ": pub changes in " ++ Data.List.intercalate ", " (map fmtDelta pubDeltas))
                    t' <- case gtTask t of
                            ActonTask{} -> return (gtTask t)
                            TyTask{}    -> do
                              parsedRes <- parseActFile sp mn actFile
                              case parsedRes of
                                Left diags -> return (ParseErrorTask mn diags)
                                Right (snap, m) -> return $ ActonTask mn (Source.ssText snap) (Source.ssBytes snap) m
                    case t' of
                      ParseErrorTask{ parseDiagnostics = diags } -> return (key, Left diags)
                      ActonTask{ src = srcContent, srcBytes = srcBytes, atree = m } -> do
                        res <- runFrontPasses gopts optsT paths envSnap m srcContent srcBytes resolveImportHash resolveNameHashMap'
                        case res of
                          Left diags -> return (key, Left diags)
                          Right fr -> do
                            when (C.verbose gopts) $
                              forM_ prevNameHashes $ \prevMap ->
                                forM_ (nameHashSummary prevMap (frNameHashes fr)) $ \summary ->
                                  ccOnInfo callbacks ("  Hash deltas " ++ modNameToString mn ++ ": " ++ summary)
                            cacheFrontResult fr
                      _ -> error ("Internal error: unexpected task " ++ show t')
                  runImplRefresh = do
                    when (C.verbose gopts) $ do
                      let fmtDelta (qn, old, new) = prstr qn ++ " " ++ short8 old ++ " → " ++ short8 new ++ fmtUsers implUsers qn
                      ccOnInfo callbacks ("  Stale " ++ modNameToString mn ++ ": impl changes in " ++ Data.List.intercalate ", " (map fmtDelta implDeltas))
                    tyRes <- readTyFile
                    case tyRes of
                      Left diags -> return (key, Left diags)
                      Right (_ms, nmod, tmod, moduleSrcBytesHash, modulePubHash, _moduleImplHash, imps, nameHashes, roots, mdoc) -> do
                        parsedRes <- parseActFile sp mn actFile
                        case parsedRes of
                          Left diags -> return (key, Left diags)
                          Right (snap, parsedMod) -> do
                            env1 <- Acton.Env.mkEnv (searchPath paths) envSnap parsedMod
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
                                hashEnv = setMod mn env1
                                (implLocalDeps, implExtDeps) = Hashing.splitDeps mn hashEnv localNames implDepsRaw
                            implExtRes <- resolveDepHashes "impl" InterfaceFiles.nhImplHash implExtDeps
                            case implExtRes of
                              Left diags -> return (key, Left diags)
                              Right implExtHashes -> do
                                let updatedNameHashes =
                                      Hashing.refreshImplHashes nameHashes nameImplHashes implLocalDeps implExtHashes
                                    moduleImplHash = Hashing.moduleImplHashFromNameHashes updatedNameHashes
                                InterfaceFiles.writeFile tyFile moduleSrcBytesHash modulePubHash moduleImplHash imps updatedNameHashes roots mdoc nmod tmod
                                let I.NModule ifaceTE _mdoc = nmod
                                    backJob = Just (mkBackJob env1 tmod (Source.ssText snap) moduleImplHash)
                                    fr = mkFrontResult ifaceTE mdoc modulePubHash updatedNameHashes backJob
                                cacheFrontResult fr
                  runCodegenRefresh = do
                    when (C.verbose gopts) $ do
                      let suffix = maybe "" formatCodegenDelta mCodegenStatus
                      ccOnInfo callbacks ("  Stale " ++ modNameToString mn ++ ": generated code out of date" ++ suffix)
                    tyRes <- readTyFile
                    case tyRes of
                      Left diags -> return (key, Left diags)
                      Right (_ms, nmod, tmod, _moduleSrcBytesHash, modulePubHash, moduleImplHashStored, _imps, nameHashes, _roots, mdoc) -> do
                        snap <- Source.readSource sp actFile
                        env1 <- Acton.Env.mkEnv (searchPath paths) envSnap tmod
                        let I.NModule ifaceTE _mdoc = nmod
                            backJob = Just (mkBackJob env1 tmod (Source.ssText snap) moduleImplHashStored)
                            fr = mkFrontResult ifaceTE mdoc modulePubHash nameHashes backJob
                        cacheFrontResult fr
                  runReuse = do
                    when (C.verbose gopts) $
                      ccOnInfo callbacks ("  Fresh " ++ modNameToString mn ++ ": using cached .ty")
                    ifaceRes <- case gtTask t of
                                  TyTask{ tyPubHash = h } -> readIfaceFromTy paths mn "" (Just h)
                                  _ -> readIfaceFromTy paths mn "" Nothing
                    case ifaceRes of
                      Left diags -> return (key, Left diags)
                      Right (ifaceTE, mdoc, ih) -> do
                        let cachedNameHashes = case gtTask t of
                              TyTask{ tyNameHashes = nhs } -> nhs
                              _ -> []
                            fr = mkFrontResult ifaceTE mdoc ih cachedNameHashes Nothing
                        cacheFrontResult fr
              case () of
                _ | needFront -> runFront
                _ | needByImpl -> runImplRefresh
                _ | needByCodegen -> runCodegenRefresh
                _ -> runReuse

    scheduleMore :: Int -> [TaskKey]
                 -> [(Async (TaskKey, Either [Diagnostic String] FrontResult), TaskKey)]
                 -> M.Map TaskKey B.ByteString
                 -> M.Map TaskKey (M.Map A.Name InterfaceFiles.NameHashInfo)
                 -> Acton.Env.Env0
                 -> M.Map TaskKey Integer
                 -> IO ([TaskKey]
                       , [(Async (TaskKey, Either [Diagnostic String] FrontResult), TaskKey)])
    scheduleMore k rdy running res nameRes envSnap cw = do
      let prio m = M.findWithDefault 0 m cw
          rdySorted = Data.List.sortOn (Down . prio) rdy
          (toStart, rdy') = splitAt k rdySorted
      new <- forM toStart $ \mn -> do
                case M.lookup mn taskMap of
                  Just t -> ccOnFrontStart callbacks t (optsFor mn)
                  Nothing -> return ()
                a <- async (doOne envSnap res nameRes mn)
                return (a, mn)
      return (rdy', new ++ running)

    loop :: IORef [Async (TaskKey, Either [Diagnostic String] FrontResult)]
         -> [TaskKey]
         -> [(Async (TaskKey, Either [Diagnostic String] FrontResult), TaskKey)]
         -> M.Map TaskKey B.ByteString
         -> M.Map TaskKey (M.Map A.Name InterfaceFiles.NameHashInfo)
         -> M.Map TaskKey Int
         -> Data.Set.Set TaskKey
         -> Acton.Env.Env0
         -> Bool
         -> Int
         -> M.Map TaskKey Integer
         -> IO (Acton.Env.Env0, Bool)
    loop runningRef rdy running res nameRes ind pend envAcc hadErrors maxPar cw = do
      (rdy1, running1) <- mask_ $ do
        res@(rdy1', running1') <- scheduleMore (maxPar - length running) rdy running res nameRes envAcc cw
        writeIORef runningRef (map fst running1')
        return res
      if null running1 && null rdy1
        then if Data.Set.null pend
               then do
                 writeIORef runningRef []
                 return (envAcc, hadErrors)
               else return (envAcc, True)
        else do
          (doneA, (mnDone, outcome)) <- waitAny $ map fst running1
          let running2 = filter ((/= doneA) . fst) running1
          writeIORef runningRef (map fst running2)
          let tDone = taskMap M.! mnDone
              optsDone = optsFor mnDone
          ccOnFrontDone callbacks tDone optsDone
          case outcome of
            Left diags -> do
              ccOnDiagnostics callbacks tDone optsDone diags
              let blocked = dependentClosure mnDone
                  pend2 = Data.Set.delete mnDone (pend `Data.Set.difference` blocked)
                  rdy2 = filter (`Data.Set.notMember` blocked) rdy1
              loop runningRef rdy2 running2 res nameRes ind pend2 envAcc True maxPar cw
            Right fr -> do
              ccOnFrontResult callbacks tDone optsDone fr
              forM_ (frBackJob fr) $ ccOnBackJob callbacks
              let res2  = M.insert mnDone (frPubHash fr) res
                  nameRes2 = M.insert mnDone (nameHashMapFromList (frNameHashes fr)) nameRes
                  pend2 = Data.Set.delete mnDone pend
                  ind2  = case M.lookup mnDone revMap of
                            Nothing -> ind
                            Just ds -> foldl' (\m d -> M.adjust (\x -> x-1) d m) ind ds
                  newlyReady = [ m | m <- Data.Set.toList pend2
                                   , M.findWithDefault 0 m ind2 == 0
                                   , not (m `elem` rdy1)
                                   , not (m `elem` map snd running2)
                                   ]
                  rdy2 = rdy1 ++ newlyReady
                  envAcc' = Acton.Env.addMod (tkMod mnDone) (frIfaceTE fr) (frDoc fr) envAcc
              loop runningRef rdy2 running2 res2 nameRes2 ind2 pend2 envAcc' hadErrors maxPar cw


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
                   res <- runBackPasses gopts (bjOpts job) (bjPaths job) (bjInput job) (return True)
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
                     projBuildSpec :: Maybe BuildSpec.BuildSpec,
                     projLocks    :: FilePath,
                     projDeps     :: [(String, FilePath)]          -- resolved dependency roots (abs paths)
                   } deriving (Show)

-- | Discover all projects reachable from a root project.
-- Follows Build.act/build.act.json dependencies, applies overrides/pins, and
-- returns a map from project root to ProjCtx while skipping duplicates.
discoverProjects :: FilePath -> FilePath -> [(String, FilePath)] -> IO (M.Map FilePath ProjCtx)
discoverProjects sysAbs rootProj depOverrides = do
    rootAbs <- normalizePathSafe rootProj
    rootSpec0 <- loadBuildSpec rootAbs
    rootSpec  <- traverse (applyDepOverrides rootAbs depOverrides) rootSpec0
    let rootPins = maybe M.empty BuildSpec.dependencies rootSpec
    go rootAbs Data.Set.empty M.empty rootPins rootAbs rootSpec
  where
    go root seen acc pins dir mSpec = do
      dirAbs <- normalizePathSafe dir
      if Data.Set.member dirAbs seen
        then return acc
        else do
          rawSpec <- case mSpec of
                       Just s | dirAbs == root -> return (Just s)
                       _ -> loadBuildSpec dirAbs
          mspec <- traverse (applyDepOverrides dirAbs depOverrides) rawSpec
          deps <- case mspec of
                    Nothing -> return []
                    Just spec -> forM (M.toList (BuildSpec.dependencies spec)) $ \(depName, dep) -> do
                                   let (chosenDep, conflict) =
                                         case M.lookup depName pins of
                                           Nothing -> (dep, Nothing)
                                           Just pinDep ->
                                             if pinDep == dep
                                               then (dep, Nothing)
                                               else (pinDep, Just dep)
                                   when (isJust conflict) $
                                     putStrLn ("Warning: dependency '" ++ depName ++ "' in " ++ dirAbs
                                               ++ " overridden by root pin")
                                   depBase <- resolveDepBase dirAbs depName chosenDep
                                   depAbs  <- normalizePathSafe depBase
                                   return (depName, depAbs)
          let outDir   = joinPath [dirAbs, "out"]
              typesDir = joinPath [outDir, "types"]
              srcDir'  = joinPath [dirAbs, "src"]
              lockPath = joinPath [dirAbs, ".actonc.lock"]
              ctx = ProjCtx { projRoot = dirAbs
                            , projOutDir = outDir
                            , projTypesDir = typesDir
                            , projSrcDir = srcDir'
                            , projSysPath = sysAbs
                            , projSysTypes = joinPath [sysAbs, "base", "out", "types"]
                            , projBuildSpec = mspec
                            , projLocks = lockPath
                            , projDeps = deps
                            }
              acc' = M.insert dirAbs ctx acc
              seen' = Data.Set.insert dirAbs seen
          foldM (step root seen' pins) acc' deps

    step root seen pins acc (_, depBase) =
      go root seen acc pins depBase Nothing

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
-- Used to locate .ty/.c/.h output under the project's types directory.
outBase                 :: Paths -> A.ModName -> FilePath
outBase paths mn        = joinPath (projTypes paths : A.modPath mn)

-- | Compute the module path without extension under the project's src dir.
-- Used to derive the .act path or related per-module files.
srcBase                 :: Paths -> A.ModName -> FilePath
srcBase paths mn        = joinPath (srcDir paths : A.modPath mn)


-- | Walk upward from a path to find a project root.
-- A project root is identified by Build.act/build.act.json/Acton.toml plus a
-- src/ directory; returns Nothing if we reach filesystem root.
findProjectDir :: FilePath -> IO (Maybe FilePath)
findProjectDir path = do
    let projectFiles = ["Build.act", "build.act.json", "Acton.toml"]
    hasProjectFile <- or <$> mapM (\file -> doesFileExist (path </> file)) projectFiles
    hasSrcDir <- doesDirectoryExist (path </> "src")
    if hasProjectFile && hasSrcDir
        then return (Just path)
        else if path == takeDirectory path  -- Check if we're at root
            then return Nothing
            else findProjectDir (takeDirectory path)


-- | Path to the compile-owner lock for a project root.
compileOwnerLockPath :: FilePath -> FilePath
compileOwnerLockPath projDir =
    joinPath [projDir, ".acton.compile.lock"]

-- | Acquire the compile-owner lock for a project root, if available.
tryCompileOwnerLock :: FilePath -> IO (Maybe (FileLock, FilePath))
tryCompileOwnerLock projDir = do
    let lockPath = compileOwnerLockPath projDir
    mlock <- tryLockFile lockPath Exclusive
    return ((\lock -> (lock, lockPath)) <$> mlock)

-- | Release a compile-owner lock and remove the lock file if possible.
releaseCompileOwnerLock :: (FileLock, FilePath) -> IO ()
releaseCompileOwnerLock (lock, lockPath) = do
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
                             depTypePaths <- if isTmp then return [] else collectDepTypePaths projPath (C.dep_overrides opts)
                             let sPaths = [projTypes] ++ depTypePaths ++ (C.searchpath opts) ++ [sysTypes]
                             createDirectoryIfMissing True binDir
                             createDirectoryIfMissing True projOut
                             createDirectoryIfMissing True projTypes
                             createDirectoryIfMissing True (getModPath projTypes modName)
                             return $ Paths sPaths sysPath sysTypes projPath projOut projTypes binDir srcDir isTmp fileExt modName
  where (fileBody,fileExt) = splitExtension $ takeFileName actFile

        analyze "/" ds  = do tmp <- canonicalizePath (C.tempdir opts)
                             return (True, tmp, [])
        analyze pre ds  = do let projectFiles = ["Build.act", "build.act.json", "Acton.toml"]
                             hasProjectFile <- or <$> mapM (\file -> doesFileExist (joinPath [pre, file])) projectFiles
                             hasSrcDir <- doesDirectoryExist (joinPath [pre, "src"])
                             if hasProjectFile && hasSrcDir
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

-- | Build a search path for module interfaces for a project.
-- Includes the project's types dir, dependency types, user searchpath, and
-- the system types directory.
searchPathForProject :: C.CompileOptions -> M.Map FilePath ProjCtx -> ProjCtx -> [FilePath]
searchPathForProject opts projMap ctx =
    let deps = depTypePathsFromMap projMap (projRoot ctx)
    in [projTypesDir ctx] ++ deps ++ (C.searchpath opts) ++ [projSysTypes ctx]

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


-- | Load a BuildSpec from Build.act (preferred) or build.act.json.
-- Throws ProjectError on parse failure to keep callers in a single error path.
loadBuildSpec :: FilePath -> IO (Maybe BuildSpec.BuildSpec)
loadBuildSpec dir = do
    let actPath  = joinPath [dir, "Build.act"]
        jsonPath = joinPath [dir, "build.act.json"]
    actExists <- doesFileExist actPath
    if actExists
      then do
        content <- readFile actPath
        case BuildSpec.parseBuildAct content of
          Left err -> throwProjectError ("Failed to parse Build.act in " ++ dir ++ ":\n" ++ err)
          Right (spec, _, _) -> return (Just spec)
      else do
        jsonExists <- doesFileExist jsonPath
        if jsonExists
          then do
            json <- BL.readFile jsonPath
            case BuildSpec.parseBuildSpecJSON json of
              Left err   -> throwProjectError ("Failed to parse build.act.json in " ++ dir ++ ":\n" ++ err)
              Right spec -> return (Just spec)
          else return Nothing

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
          let dep' = dep { BuildSpec.path = Just absP }
          return (M.insert depName dep' depsMap)

fetchDependencies :: C.GlobalOptions -> Paths -> [(String, FilePath)] -> IO ()
fetchDependencies gopts paths depOverrides = do
    when (isTmp paths) $ return ()
    mspec <- loadBuildSpec (projPath paths)
    case mspec of
      Nothing -> return ()
      Just spec0 -> do
        spec <- applyDepOverrides (projPath paths) depOverrides spec0
        unless (C.quiet gopts) $
          putStrLn "Resolving dependencies (fetching if missing)..."
        home <- getHomeDirectory
        let zigExe      = joinPath [sysPath paths, "zig", "zig"]
            globalCache = joinPath [home, ".cache", "acton", "zig-global-cache"]
            depsCache   = joinPath [home, ".cache", "acton", "deps"]
            cacheDir h  = joinPath [globalCache, "p", h]
        createDirectoryIfMissing True globalCache
        createDirectoryIfMissing True depsCache

        let pkgFetches = catMaybes
              [ mkPkgFetch name dep | (name, dep) <- M.toList (BuildSpec.dependencies spec) ]
            zigFetches = catMaybes
              [ mkZigFetch name dep | (name, dep) <- M.toList (BuildSpec.zig_dependencies spec) ]

            mkPkgFetch name dep =
              case BuildSpec.path dep of
                Just p | not (null p) -> Nothing
                _ -> case (BuildSpec.url dep, BuildSpec.hash dep) of
                       (Just u, Just h) ->
                         Just (fetchOne "pkg" name u (Just h) cacheDir zigExe globalCache)
                       (Just _, Nothing) ->
                         Just (return (Left ("Dependency " ++ name ++ " is missing hash")))
                       _ -> Nothing

            mkZigFetch name dep =
              case BuildSpec.zpath dep of
                Just p | not (null p) -> Nothing
                _ -> case (BuildSpec.zurl dep, BuildSpec.zhash dep) of
                       (Just u, Just h) ->
                         Just (fetchOne "zig" name u (Just h) cacheDir zigExe globalCache)
                       (Just _, Nothing) ->
                         Just (return (Left ("Zig dependency " ++ name ++ " is missing hash")))
                       _ -> Nothing

        results <- mapConcurrently id (pkgFetches ++ zigFetches)
        let errs = [ e | Left e <- results ]
        unless (null errs) $ throwProjectError (unlines errs)

        forM_ (M.toList (BuildSpec.dependencies spec)) $ \(name, dep) -> do
          case BuildSpec.path dep of
            Just p | not (null p) -> return ()
            _ -> case BuildSpec.hash dep of
                   Nothing -> return ()
                   Just h -> do
                     let src = cacheDir h
                         dst = joinPath [depsCache, name ++ "-" ++ h]
                     exists <- doesDirectoryExist dst
                     unless exists $ do
                       srcOk <- doesDirectoryExist src
                       unless srcOk $
                         throwProjectError ("Dependency " ++ name ++ " not present in Zig cache after fetch: " ++ src)
                       when (C.verbose gopts) $
                         putStrLn ("Copying dependency " ++ name ++ " (" ++ h ++ ") from Zig cache")
                       copyTree src dst
          return ()
  where
    fetchOne kind name url mh cacheDir zigExe globalCache = do
      case mh of
        Just h -> do
          present <- doesDirectoryExist (cacheDir h)
          if present
            then do
              putStrLn ("Using cached " ++ kind ++ " dependency " ++ name ++ " (" ++ h ++ ")")
              return (Right h)
            else runFetch kind name url mh cacheDir zigExe globalCache
        Nothing ->
          runFetch kind name url mh cacheDir zigExe globalCache

    runFetch kind name url mh cacheDir zigExe globalCache = do
      putStrLn ("Fetching " ++ kind ++ " dependency " ++ name ++ " from " ++ url)
      let cmd = proc zigExe ["fetch", "--global-cache-dir", globalCache, url]
      res <- try (readCreateProcessWithExitCode cmd "") :: IO (Either SomeException (ExitCode, String, String))
      case res of
        Left ex -> return (Left ("Failed to fetch dependency " ++ name ++ ": " ++ displayException ex))
        Right (code, out, err) ->
          case code of
            ExitSuccess -> do
              let hashVal = trim out
              case mh of
                Just h | h /= hashVal ->
                  return (Left ("Hash mismatch for dependency " ++ name ++ " (expected " ++ h ++ ", got " ++ hashVal ++ ")"))
                _ -> do
                  exists <- doesDirectoryExist (cacheDir hashVal)
                  if exists
                    then return (Right hashVal)
                    else return (Left ("Dependency " ++ name ++ " not present in Zig cache after fetch: " ++ cacheDir hashVal))
            ExitFailure _ ->
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
-- Reads Build.act/build.act.json and follows dependency edges.
collectDepTypePaths :: FilePath -> [(String, FilePath)] -> IO [FilePath]
collectDepTypePaths projDir overrides = do
  root <- normalizePathSafe projDir
  snd <$> go Data.Set.empty root Nothing
  where
    go seen dir mSpec = do
      mspec <- case mSpec of
                 Just s -> return (Just s)
                 Nothing -> loadBuildSpec dir
      mspec' <- traverse (applyDepOverrides dir overrides) mspec
      case mspec' of
        Nothing   -> return (seen, [])
        Just spec -> foldM (step dir) (seen, []) (M.toList (BuildSpec.dependencies spec))

    step base (seen, acc) (depName, dep) = do
      depBase <- resolveDepBase base depName dep
      let seen' = Data.Set.insert depBase seen
          typesDir = joinPath [depBase, "out", "types"]
      if Data.Set.member depBase seen
        then return (seen', acc)
        else do
          (seenNext, sub) <- go seen' depBase Nothing
          return (seenNext, acc ++ [typesDir] ++ sub)


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
-- Used to decide which roots to include in .ty headers and root generation.
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
