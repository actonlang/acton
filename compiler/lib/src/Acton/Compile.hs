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
       buildGlobalTasks, and selectNeededTasks/selectAffectedTasks) builds a
       CompilePlan for the requested subgraph. Parsing is performed through
       parseActSource/parseActFile so SourceProvider overlays can supply
       unsaved buffers.
    2) runCompilePlan drives compileTasks. As each module finishes its front
       passes, the CompileHooks callbacks (for example chOnFrontResult)
       enqueue a BackJob into the scheduler's
       BackQueue, so back passes begin as soon as they are ready and can overlap
       remaining front passes.
    3) Callers either wait for waitBackJobs (CLI builds) or let back jobs run
       in the background. CLI builds wait before invoking Zig.
    4) runBackPasses can be used for standalone back-pass execution when a
       caller already has a BackJob to run.
  - Triggering:
    - acton build runs the pipeline once for the requested files/project.
    - acton watch and LSP call startCompile on each event; callers can gate
      output with generation checks (e.g. whenCurrent). BackQueue ignores
      back jobs for stale generations, and front-output jobs are skipped for
      stale generations before writing.
    - Callers may supply a delay (debounce) before startCompile runs (LSP uses
      debounceMicros on change events; acton watch uses 0).
  - Finalization:
    - CLI waits on waitBackJobs before running Zig build
    - LSP does not implicitly run the Zig build, it is only run explicitly when
      the user asks to build / run a module

State and orchestration:
  - Acton.Compile holds only one in-process ModuleCache (.tydb paths, module
    hashes and name hashes) to speed up .tydb lookups; all orchestration state
    is owned by the caller.
  - Callers allocate and hold a CompileScheduler from this module. The
    scheduler encapsulates mutable state (generation id, cancelable async
    handle, build-spec stamp, and a shared back-queue); callers pass it into
    startCompile, runCompilePlan and waitBackJobs rather than mutating it
    directly.
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
  ( TaskKey(..)
  , GlobalTask(..)
  , CompileTask(..)
  , ModuleSource(..)
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
  , BackTiming
  , BackPass(..)
  , BackPassProgress(..)
  , BackPassEvent(..)
  , backPassName
  , FrontPass(..)
  , FrontPassProgress(..)
  , ParseProgress(..)
  , CompileCallbacks(..)
  , defaultCompileCallbacks
  , CompileScheduler
  , newCompileScheduler
  , compileParallelism
  , startCompile
  , stopCompile
  , Generation
  , generationNumber
  , currentGeneration
  , isCurrent
  , whenCurrent
  , waitBackJobs
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
  , clearModuleCaches
  , parseActSource
  , parseActFile
  , readModuleTask
  , docIndexEntry
  , buildGlobalTasks
  , importGraph
  , ModuleGraph
  , moduleGraph
  , selectNeededTasks
  , selectAffectedTasks
  , libraryBoundaryTasks
  , compileTasks
  , runFrontPasses
  , docNameCountThreshold
  , shouldGenerateDocOutput
  , runBackPasses
  , importsOf
  , taskSourceFile
  , hasParseStage
  , quiet
  , missingIfaceDiagnostics
  , ModuleCache
  , moduleCache
  , HashKind(..)
  , lookupModuleHash
  , rootEligible
  ) where

import qualified Acton.Parser
import Acton.Parser (CustomParseError, CustomParseException, ChunkScanError(..), ContextError, IndentationError)
import qualified Acton.Syntax as A
import Acton.Syntax (modNameToString)
import qualified Acton.NameInfo as I
import Text.Megaparsec.Error (ParseErrorBundle)
import qualified Acton.CommandLineParser as C
import Acton.Printer ()
import qualified Acton.Env
import qualified Acton.EscapeAnalysis
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
import Acton.Analytics
import qualified Acton.Boxing
import qualified Acton.CodeGen
import Acton.Builtin (mBuiltin)
import qualified Acton.BuildSpec as BuildSpec
import qualified Acton.DocPrinter as DocP
import Acton.Fetch
import Acton.Graph
import Acton.Project
import qualified Acton.Diagnostics as Diag
import qualified Acton.SourceProvider as Source
import Utils
import qualified Pretty
import Pretty hiding ((<>))
import qualified InterfaceFiles

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TChan, TVar, atomically, check, modifyTVar', newTChanIO, newTVarIO, readTChan, readTVar, readTVarIO, writeTChan, writeTVar)
import Control.DeepSeq (force, rnf)
import Control.Exception (Exception, Handler(..), SomeException, catch, catches, displayException, evaluate, finally, mask_, throwIO, try)
import Control.Monad
import Data.Binary (encode)
import Data.Char (isHexDigit, isSpace)
import Data.Either (partitionEithers)
import Data.List (foldl', intercalate, nub, partition)
import qualified Data.List
import Data.IORef
import Data.Maybe (catMaybes, isJust, isNothing, listToMaybe, mapMaybe)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import Data.Ord (Down(..))
import qualified Data.Set
import Error.Diagnose (Diagnostic)
import GHC.Conc (getNumCapabilities)
import System.Clock
import System.Directory
import System.Environment (getExecutablePath)
import System.FilePath ((</>))
import System.FilePath.Posix
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Files (deviceID, fileID, fileSize, getFileStatus, modificationTimeHiRes, statusChangeTimeHiRes)
import Text.PrettyPrint (renderStyle, style, Style(..), Mode(PageMode))
import Text.Show.Pretty (ppDoc)

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base16 as Base16
import qualified Crypto.Hash.SHA256 as SHA256


newtype DbpSelectionError = DbpSelectionError String deriving (Show)

instance Exception DbpSelectionError where
  displayException (DbpSelectionError msg) = msg

newtype FrontOutputError = FrontOutputError String deriving (Show)

instance Exception FrontOutputError where
  displayException (FrontOutputError msg) = msg


-- | Why a run could not compile its modules.
data CompileFailure
  = ImportCycles [[TaskKey]]                      -- the modules of each import cycle
  | BuiltinFailed                                 -- __builtin__ had errors, already reported as diagnostics
  | OutputFailed TaskKey FrontOutputKind String   -- the first failed output job in start order, with its exception text
  | DeferredBackFailed String                     -- the exception text from preparing ready deferred back jobs
  deriving (Eq, Show)

-- | The text a user sees for each failure.
compileFailureMessage                   :: CompileFailure -> String
compileFailureMessage (ImportCycles cs) = "Cyclic imports: " ++ concat [ '\n' : concat [ tkProj k ++ ":" ++ modNameToString (tkMod k) ++ " " | k <- c ] | c <- cs ]
compileFailureMessage BuiltinFailed     = "Builtin compilation failed"
compileFailureMessage (OutputFailed k kind msg)
                                        = "Front output job failed for " ++ tkProj k ++ "/" ++ modNameToString (tkMod k)
                                          ++ " (" ++ frontOutputKindName kind ++ "): " ++ msg
compileFailureMessage (DeferredBackFailed msg)
                                        = msg


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

-- | The back passes in the order they run.  A pass's index (fromEnum) is
-- the number of passes completed before it starts.
data BackPass
  = BackPassNormalize
  | BackPassDeactorize
  | BackPassCPS
  | BackPassLLift
  | BackPassBoxing
  | BackPassCodeGen
  | BackPassRender
  | BackPassWrite
  deriving (Eq, Enum, Bounded, Show)

backPassName :: BackPass -> String
backPassName BackPassNormalize  = "normalize"
backPassName BackPassDeactorize = "deactorize"
backPassName BackPassCPS        = "cps"
backPassName BackPassLLift      = "llift"
backPassName BackPassBoxing     = "boxing"
backPassName BackPassCodeGen    = "codegen"
backPassName BackPassRender     = "render"
backPassName BackPassWrite      = "write"

-- | The dump a back pass prints, and the flag that asks for it.
backPassDump                    :: BackPass -> Maybe (C.CompileOptions -> Bool, String)
backPassDump BackPassNormalize  = Just (C.norm,  "norm")
backPassDump BackPassDeactorize = Just (C.deact, "deact")
backPassDump BackPassCPS        = Just (C.cps,   "cps")
backPassDump BackPassLLift      = Just (C.llift, "llift")
backPassDump BackPassBoxing     = Just (C.box,   "box")
backPassDump _                  = Nothing

data BackPassProgress = BackPassProgress
  { bppPass      :: BackPass
  , bppCompleted :: Int                 -- the pass's index when it starts, index + 1 after
  , bppTotal     :: Int                 -- 8 when the job writes files, 7 otherwise
  , bppEvent     :: BackPassEvent
  } deriving (Eq, Show)

data BackPassEvent = BackPassStarted | BackPassFinished TimeSpec | BackPassSkipped
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
  , ftTypeEscape :: TimeSpec
  , ftTypeHash :: TimeSpec
  , ftTypeStmtTimings :: [TypeStmtTiming]
  } deriving (Eq, Show)

-- | Each back pass's time in pass order; the write only when it wrote.
type BackTiming = [(BackPass, TimeSpec)]


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
  , ccOnInfo = \_ -> return ()
  }

data BackJobCallbacks = BackJobCallbacks
  { bjcOnStart :: BackJob -> IO ()
  , bjcOnProgress :: BackJob -> BackPassProgress -> IO ()
  , bjcOnDone :: BackJob -> BackJobResult -> IO ()
  }

checkBuildSpecChange :: CompileScheduler -> BuildSpecStamp -> IO Bool
checkBuildSpecChange sched stamp =
  atomicModifyIORef' (schedulerBuildSpec sched) $ \prev ->
    let changed = prev /= Just stamp
    in (Just stamp, changed)

-- | A compile run's place in its scheduler's sequence of runs. A run is
-- current until a newer one starts. A stale run writes no front output and
-- queues no back job, and its queued back jobs neither write nor report an
-- end.
data Generation = Generation
  { generationNumber  :: Int
  , generationCounter :: IORef Int      -- the scheduler's newest generation
  }

isCurrent :: Generation -> IO Bool
isCurrent gen = (== generationNumber gen) <$> readIORef (generationCounter gen)

whenCurrent :: Generation -> IO () -> IO ()
whenCurrent gen action = isCurrent gen >>= \current -> when current action

-- | The generation a scheduler is at, which a one-shot build runs as. A
-- fresh scheduler is at generation 0, and it stays current until a run is
-- started with startCompile.
currentGeneration :: CompileScheduler -> IO Generation
currentGeneration sched = do
  number <- readIORef (schedulerCounter sched)
  return (Generation number (schedulerCounter sched))

-- | A long-lived compiler. The back passes of every generation share one
-- pool of workers.
data CompileScheduler = CompileScheduler
  { schedulerCounter   :: IORef Int                     -- the newest generation
  , schedulerRun       :: MVar (Maybe (Async ()))       -- the newest generation's run
  , schedulerBackQueue :: BackQueue
  , schedulerBuildSpec :: IORef (Maybe BuildSpecStamp)  -- Build.act at the last plan
  }

-- | Back jobs waiting for a worker, with the state waitBackJobs reads.
data BackQueue = BackQueue
  { queuedJobs    :: TChan QueuedJob
  , pendingCounts :: TVar (M.Map Int Int)               -- per generation: queued or running jobs
  , firstFailures :: TVar (M.Map Int BackPassFailure)   -- per generation: the first failure
  }

data QueuedJob = QueuedJob Generation BackJobCallbacks BackJob

-- | Create a scheduler at generation 0. Its back passes run on the given
-- number of workers (at least one) with the given global options, which
-- decide whether back passes report their time and timing.
newCompileScheduler :: C.GlobalOptions -> Int -> IO CompileScheduler
newCompileScheduler gopts workers = do
  counter <- newIORef 0
  queue <- BackQueue <$> newTChanIO <*> newTVarIO M.empty <*> newTVarIO M.empty
  replicateM_ (max 1 workers) (forkIO (backWorker gopts queue))
  CompileScheduler counter <$> newMVar Nothing <*> pure queue <*> newIORef Nothing

-- | How many modules compile at once: --jobs when given, else one per
-- capability.
compileParallelism                      :: C.GlobalOptions -> IO Int
compileParallelism gopts
  | C.jobs gopts > 0                    = return (C.jobs gopts)
  | otherwise                           = max 1 <$> getNumCapabilities

-- | Start a generation and return it. The counter moves first, so the
-- running generation's writes see themselves stale at once. The previous
-- run is then cancelled under the MVar, and cancel waits for its finalizer,
-- which drains its front-output jobs, so stale writes never overlap the new
-- run. After the delay the run starts only if no newer generation began.
startCompile :: CompileScheduler -> Int -> (Generation -> IO ()) -> IO Generation
startCompile sched delay run = do
  number <- atomicModifyIORef' (schedulerCounter sched) (\newest -> (newest + 1, newest + 1))
  let gen = Generation number (schedulerCounter sched)
  modifyMVar_ (schedulerRun sched) $ \previous -> do
    mapM_ cancel previous
    Just <$> async (do when (delay > 0) (threadDelay delay)
                       whenCurrent gen (run gen))
  return gen

-- | Cancel the newest generation's run and wait for its finalizer.
stopCompile :: CompileScheduler -> IO ()
stopCompile sched = modifyMVar_ (schedulerRun sched) (\running -> Nothing <$ mapM_ cancel running)

-- | Queue a back job of a current generation. A stale generation's job is
-- dropped. The pending count rises in the transaction that queues the job,
-- so waitBackJobs never sees zero while a job of its generation is queued.
enqueueBack :: BackQueue -> Generation -> BackJobCallbacks -> BackJob -> IO ()
enqueueBack queue gen callbacks job = whenCurrent gen $ atomically $ do
  modifyTVar' (pendingCounts queue) (M.insertWith (+) (generationNumber gen) 1)
  writeTChan (queuedJobs queue) (QueuedJob gen callbacks job)

-- | Run queued back jobs, one at a time. A job runs only while its
-- generation is current and has no failure yet. The write gate is asked
-- right before the generated files are written. The end is reported, and a
-- failure recorded, only while the generation is current, so a job that
-- goes stale while it runs reports its start but no end. The first failure
-- of a generation is kept. The progress callback runs inside the try, so an
-- exception from progress reporting fails the job. The start and end
-- callbacks run outside it, so an exception from them ends the worker. The
-- pending count falls after every job, whether it ran or was dropped.
backWorker :: C.GlobalOptions -> BackQueue -> IO ()
backWorker gopts queue = forever $ do
  QueuedJob gen callbacks job <- atomically (readTChan (queuedJobs queue))
  current <- isCurrent gen
  failed <- M.member (generationNumber gen) <$> readTVarIO (firstFailures queue)
  when (current && not failed) $ do
    bjcOnStart callbacks job
    result <- try (runBackPasses gopts job (isCurrent gen) (bjcOnProgress callbacks job))
                :: IO (Either SomeException (Maybe TimeSpec, Maybe BackTiming))
    whenCurrent gen $ case result of
      Left err -> do
        let failure = BackPassFailure (bjKey job) (displayException err)
        atomically (modifyTVar' (firstFailures queue) (M.insertWith (\_ first -> first) (generationNumber gen) failure))
        bjcOnDone callbacks job (BackJobFailed failure)
      Right (time, timing) -> bjcOnDone callbacks job (BackJobOk time timing)
  atomically (modifyTVar' (pendingCounts queue) (M.update decrement (generationNumber gen)))
  where
    decrement count = if count <= 1 then Nothing else Just (count - 1)

-- | Wait for a generation's back jobs. Returns its first failure as soon as
-- there is one, and forgets it, without waiting for the remaining jobs.
-- Otherwise returns Nothing once none of its jobs is queued or running.
-- Stale jobs drain without running, so waiting on a superseded generation
-- returns Nothing, and callers check the generation again afterwards.
waitBackJobs :: CompileScheduler -> Generation -> IO (Maybe BackPassFailure)
waitBackJobs sched gen = atomically $ do
  failures <- readTVar (firstFailures queue)
  case M.lookup number failures of
    Just failure -> Just failure <$ writeTVar (firstFailures queue) (M.delete number failures)
    Nothing      -> do pending <- readTVar (pendingCounts queue)
                       check (M.findWithDefault 0 number pending == 0)
                       return Nothing
  where
    queue  = schedulerBackQueue sched
    number = generationNumber gen

data CompileContext = CompileContext
  { ccOpts :: C.CompileOptions
  , ccDepOverrides :: [(String, FilePath)]
  , ccPathsRoot :: Paths
  , ccRootProj :: FilePath
  , ccSysAbs :: FilePath
  , ccBuildStamp :: BuildSpecStamp
  }

-- | A module's options: dependency projects build neither their tests nor a
-- final binary.
optionsFor                              :: CompileContext -> TaskKey -> C.CompileOptions
optionsFor ctx k
  | tkProj k == ccRootProj ctx          = ccOpts ctx
  | otherwise                           = (ccOpts ctx) { C.skip_build = True, C.test = False }

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
  , cpRootModules :: [A.ModName]                  -- selected root-project modules, before library expansion
  , cpIncremental :: Bool
  , cpAllowPrune :: Bool
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
  projMap0 <- if isTmp pathsRoot
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
  -- Test selection follows imports, not production library membership.
  -- Library groups must not pull unrelated test modules into compilation.
  let projMap = if C.test opts'
                  then M.adjust (\pctx -> pctx { projBuildSpec = (projBuildSpec pctx) { BuildSpec.libraries = M.empty } }) rootProj projMap0
                  else projMap0
  -- Keep generated module artifacts in sync with source removals.
  -- For full builds, scan and prune all orphan outputs.
  -- For incremental builds, prune only modules whose changed .act paths are now missing.
  if incremental
    then maybe (return ()) (pruneMissingChangedModuleOutputs opts' projMap) mChangedPaths
    else mapM_ (pruneMissingModuleOutputs opts' projMap) (M.elems projMap)
  let hasBuildLibraries = any (not . M.null . BuildSpec.libraries . projBuildSpec) (M.elems projMap)
  globalTasks <- buildGlobalTasks sp gopts opts' projMap
    (if incremental || allowPrune || hasBuildLibraries then Nothing else Just srcFiles)
  let modules = moduleGraph projMap globalTasks
      dbpBlocked = libraryBoundaryTasks modules
  neededTasks0 <- case mChangedPaths of
    Nothing -> selectNeededTasks pathsRoot rootProj modules srcFiles
    Just changed -> selectAffectedTasks ctx modules dbpBlocked changed
  let neededTasks = expandBuildLibraryTasks modules neededTasks0
      rootModules = [ tkMod (gtKey t) | t <- neededTasks0, tkProj (gtKey t) == rootProj ]
  return CompilePlan
    { cpContext = ctx
    , cpProjMap = projMap
    , cpGlobalTasks = globalTasks
    , cpNeededTasks = neededTasks
    , cpDbpBlocked = dbpBlocked
    , cpRootModules = rootModules
    , cpIncremental = incremental
    , cpAllowPrune = allowPrune'
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
  , chOnBackQueued :: TaskKey -> IO ()
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
    , chOnBackQueued = \_ -> return ()
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
               -> Generation
               -> CompileHooks
               -> IO (Either CompileFailure (Acton.Env.Env0, Bool))
runCompilePlan sp gopts plan sched gen hooks0 = withAnalytics $ \ana -> do
  let hooks = instrumentHooksForAnalytics ana hooks0
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
        , ccShouldWriteFrontOutput = isCurrent gen
        , ccOnBackJob = \job -> do
            enqueueBack (schedulerBackQueue sched) gen backCallbacks job
            chOnBackQueued hooks (bjKey job)
        , ccOnBackSkipped = chOnBackSkipped hooks
        , ccOnInfo = chOnInfo hooks
        }
  compileTasks sp gopts moduleCache (cpContext plan) (cpNeededTasks plan) (cpDbpBlocked plan) callbacks

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
  , chOnBackProgress = \job p -> do
      case bppEvent p of
        BackPassFinished _ -> recordBackPass ana (modNameToString (tkMod (bjKey job))) (backPassName (bppPass p))
        _                  -> return ()
      chOnBackProgress hooks job p
  }
  where taskMod t   = modNameToString (tkMod (gtKey t))

-- | Debug helper for pass dumps.
-- Prints a header and footer around the given text so pass output is easy to
-- spot in verbose logs.
dump :: A.ModName -> String -> String -> IO ()
dump mn h txt =
  putStrLn ("\n\n== " ++ h ++ ": " ++ modNameToString mn ++ " ================================\n" ++ txt
            ++ '\n' : replicate (38 + length h + length (modNameToString mn)) '=' ++ "\n")

-- | Dump a module's text when the options ask for it.  The text is built
-- only then.
dumpWhen                                :: C.CompileOptions -> A.ModName -> (C.CompileOptions -> Bool) -> String -> String -> IO ()
dumpWhen opts mn flag label text        = when (flag opts) (dump mn label text)

-- Module cache -------------------------------------------------------------------------------------

-- | Interface hashes read from .tydb files, keyed by module name.  Every
-- table is read and written under its own lock.  A hash or name table is
-- always locked before the path table, never after.
data ModuleCache = ModuleCache
  { tydbPathTable :: MVar (M.Map A.ModName FilePath)
  , pubHashTable  :: MVar (M.Map A.ModName B.ByteString)                                      -- found answers only
  , implHashTable :: MVar (M.Map A.ModName B.ByteString)                                      -- found answers only
  , nameMapTable  :: MVar (M.Map A.ModName (M.Map A.Name InterfaceFiles.NameHashInfo))         -- complete public maps
  , nameReadTable :: MVar (M.Map (A.ModName, A.Name) (Maybe InterfaceFiles.NameHashInfo))      -- single reads, misses kept
  }

-- | The two hashes of a module or name: its public interface and its
-- implementation.
data HashKind = Pub | Impl deriving (Eq, Ord, Show)

hashTable                               :: HashKind -> ModuleCache -> MVar (M.Map A.ModName B.ByteString)
hashTable Pub                           = pubHashTable
hashTable Impl                          = implHashTable

newModuleCache :: IO ModuleCache
newModuleCache = ModuleCache <$> newMVar M.empty <*> newMVar M.empty <*> newMVar M.empty <*> newMVar M.empty <*> newMVar M.empty

-- | The process-wide module cache.  NOINLINE keeps GHC from inlining or
-- floating the initializer, so it runs once and the process has exactly one.
{-# NOINLINE moduleCache #-}
moduleCache :: ModuleCache
moduleCache = unsafePerformIO newModuleCache

-- | Empty the process-wide cache before an independent build of another
-- source tree.  Its tables are keyed by module name, so they cannot tell two
-- revisions apart.  No compile job may be running.
clearModuleCaches :: IO ()
clearModuleCaches = do
    clear tydbPathTable
    clear pubHashTable
    clear implHashTable
    clear nameMapTable
    clear nameReadTable
  where
    clear :: (ModuleCache -> MVar (M.Map k v)) -> IO ()
    clear table = modifyMVar_ (table moduleCache) (const (return M.empty))

-- | Look a key up, loading and remembering it on a miss.  Only found values
-- are kept, so a miss is loaded again next time.  The table stays locked
-- while loading.
memoFound :: Ord k => MVar (M.Map k v) -> k -> IO (Maybe v) -> IO (Maybe v)
memoFound table key load = modifyMVar table $ \known -> case M.lookup key known of
    Just v  -> return (known, Just v)
    Nothing -> do found <- load
                  return (maybe known (\v -> M.insert key v known) found, found)

-- | The .tydb of a module on a search path.  A remembered path counts only
-- while it exists and lies under the search path; its search order is not
-- checked again.  A module without a .tydb is forgotten.
lookupTydbPath :: ModuleCache -> [FilePath] -> A.ModName -> IO (Maybe FilePath)
lookupTydbPath cache spaths mn = modifyMVar (tydbPathTable cache) $ \m -> do
  normSearch <- mapM normalizePathSafe spaths
  let refresh known = do
        mty <- Acton.Env.findTyFile spaths mn
        case mty of
          Just p  -> return (M.insert mn p known, Just p)
          Nothing -> return (M.delete mn known, Nothing)
  case M.lookup mn m of
    Just p -> do
      pNorm <- normalizePathSafe p
      exists <- InterfaceFiles.interfaceExists pNorm
      if exists && any (pNorm `isUnder`) normSearch
        then return (m, Just pNorm)
        else refresh m
    Nothing -> refresh m

-- | Read from a module's .tydb on the search path, when it has one.
withTydb :: ModuleCache -> [FilePath] -> A.ModName -> (FilePath -> IO (Maybe a)) -> IO (Maybe a)
withTydb cache spath m readTydb = lookupTydbPath cache spath m >>= maybe (return Nothing) readTydb

-- | A module's current pub or impl hash, from its .tydb header unless a
-- finished module or an earlier read remembered it.
lookupModuleHash :: ModuleCache -> [FilePath] -> HashKind -> A.ModName -> IO (Maybe B.ByteString)
lookupModuleHash cache spath kind m = memoFound (hashTable kind cache) m $
    withTydb cache spath m (fmap (fmap pick) . InterfaceFiles.readModuleHashesMaybe)
  where
    pick (_, pub, impl) = case kind of
      Pub  -> pub
      Impl -> impl

-- | A module's complete public name map, from its .tydb header unless a
-- finished module or an earlier read remembered it.
lookupNameMap :: ModuleCache -> [FilePath] -> A.ModName -> IO (Maybe (M.Map A.Name InterfaceFiles.NameHashInfo))
lookupNameMap cache spath m = memoFound (nameMapTable cache) m $
    withTydb cache spath m (fmap (fmap publicNameMap) . InterfaceFiles.readHeaderMaybe)
  where
    publicNameMap (_, _, _, _, _, _, nameHashes, _, _, _) = nameHashMapFromList (publicNameHashes nameHashes)

-- | One name's hashes: from the module's complete map when there is one,
-- else by a single-name read of its .tydb, whose absence is remembered too.
lookupNameHash :: ModuleCache -> [FilePath] -> A.ModName -> A.Name -> IO (Maybe InterfaceFiles.NameHashInfo)
lookupNameHash cache spath m n = do
    complete <- readMVar (nameMapTable cache)
    case M.lookup m complete of
      Just names -> return (M.lookup n names)
      Nothing    -> modifyMVar (nameReadTable cache) $ \singleReads -> case M.lookup (m, n) singleReads of
        Just r  -> return (singleReads, r)
        Nothing -> do r <- withTydb cache spath m (`InterfaceFiles.readNameHashMaybe` n)
                      return (M.insert (m, n) r singleReads, r)

-- | Make a finished module's hashes visible to later lookups.  A result
-- without name hashes (a module reused from its .tydb) must not enter the
-- complete maps: it would hide the single-name reads of its .tydb.
rememberFront :: ModuleCache -> A.ModName -> FrontResult -> IO ()
rememberFront cache m fr = do
    modifyMVar_ (pubHashTable cache)  (return . M.insert m (frPubHash fr))
    modifyMVar_ (implHashTable cache) (return . M.insert m (frImplHash fr))
    unless (null (frNameHashes fr)) $
      modifyMVar_ (nameMapTable cache) (return . M.insert m (nameHashMapFromList (frNameHashes fr)))

-- | Build a name hash map keyed by Name.
nameHashMapFromList :: [InterfaceFiles.NameHashInfo] -> M.Map A.Name InterfaceFiles.NameHashInfo
nameHashMapFromList infos = M.fromList [ (InterfaceFiles.nhName i, i) | i <- infos ]

publicIfaceTE :: [(A.Name, I.NameInfo)] -> [(A.Name, I.NameInfo)]
publicIfaceTE = filter (Names.isPublicName . fst)

publicNameHashes :: [InterfaceFiles.NameHashInfo] -> [InterfaceFiles.NameHashInfo]
publicNameHashes = filter (Names.isPublicName . InterfaceFiles.nhName)


-- Handling Acton files -----------------------------------------------------------------------------

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

-- | The name diagnostics give a source file: its path relative to the
-- current directory, which is read on every call.
displayPath                             :: FilePath -> IO FilePath
displayPath file                        = (\cwd -> makeRelative cwd file) <$> getCurrentDirectory

-- | Parser exceptions as diagnostics: parse errors name the parsed file,
-- context and indentation errors the module's file.  Other exceptions
-- propagate.
parserHandlers :: A.ModName -> FilePath -> String -> [Handler (Either [Diagnostic String] a)]
parserHandlers mn file src =
    [ Handler $ \e -> return (Left [Diag.parseDiagnosticFromBundle file src (e :: ParseErrorBundle String CustomParseError)])
    , Handler $ \e -> return (Left [Diag.customParseExceptionToDiagnostic file src (e :: CustomParseException)])
    , Handler $ \e -> return (Left (errsToDiagnostics "Context error"     (modNameToFilename mn) src (Acton.Parser.contextError (e :: ContextError))))
    , Handler $ \e -> return (Left (errsToDiagnostics "Indentation error" (modNameToFilename mn) src (Acton.Parser.indentationError (e :: IndentationError)))) ]

-- | Parse a module from source text.  A chunk scan error is a parse error at
-- the parsed file.
parseActSource :: C.CompileOptions -> A.ModName -> FilePath -> String -> Maybe (ParseProgress -> IO ()) -> IO (Either [Diagnostic String] A.Module)
parseActSource opts mn actFile srcContent mOnProgress =
    (Right <$> parseModule mn actFile srcContent (fmap wrapProgress mOnProgress))
      `catches` (chunkScanError : parserHandlers mn actFile srcContent)
  where
    parseModule
      | C.parse_serial opts             = Acton.Parser.parseModuleSerial
      | otherwise                       = Acton.Parser.parseModule
    wrapProgress onProgress completed total = onProgress (ParseProgress completed total)
    chunkScanError = Handler $ \(ChunkScanError loc msg) -> return (Left (errsToDiagnostics "Parse error" actFile srcContent [(loc, msg)]))

-- | Parse only the module header from a SourceSnapshot.
parseActHeaderSnapshot :: A.ModName -> FilePath -> Source.SourceSnapshot -> IO (Either [Diagnostic String] ([A.ModName], Maybe String))
parseActHeaderSnapshot mn actFile snap = do
    display <- displayPath actFile
    (Right <$> header display) `catches` parserHandlers mn display src
  where
    src = Source.ssText snap
    header display = do
      (imps, mdoc) <- Acton.Parser.parseModuleHeader display src
      return (A.importsOf (A.Module mn imps Nothing []), mdoc)

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
  display <- displayPath actFile
  emod <- parseActSource opts mn display (Source.ssText snap) mOnProgress
  return $ fmap (\m -> (snap, m)) emod

-- | Read stable source metadata used for quick .tydb reuse checks.
-- Device/inode act as file identity on POSIX so metadata drift from copies or
-- restores falls back to content hashing instead of blindly trusting mtimes.
readSourceFileMeta :: FilePath -> IO InterfaceFiles.SourceFileMeta
readSourceFileMeta path = do
  st <- getFileStatus path
  return InterfaceFiles.SourceFileMeta
    { InterfaceFiles.sfmMTimeNs = InterfaceFiles.posixNanoseconds (modificationTimeHiRes st)
    , InterfaceFiles.sfmCTimeNs = InterfaceFiles.posixNanoseconds (statusChangeTimeHiRes st)
    , InterfaceFiles.sfmSize = fromIntegral (fileSize st)
    , InterfaceFiles.sfmDevice = Just (fromIntegral (deviceID st))
    , InterfaceFiles.sfmInode = Just (fromIntegral (fileID st))
    }


-- Compilation tasks, chasing imported modules, compilation and building executables -----------------

-- | Generating C for one module.
data BackJob = BackJob
  { bjKey      :: TaskKey
  , bjPaths    :: Paths
  , bjOpts     :: C.CompileOptions
  , bjTypeEnv  :: Acton.Env.Env0        -- protocols converted
  , bjModule   :: A.Module              -- typed; only the selected names for a deferred module
  , bjSource   :: String                -- for #line directives
  , bjImplHash :: B.ByteString          -- tagged into the first line of each generated file
  }

-- | Back work that waits for every dependent's interest.  It keeps only
-- reloadable metadata, so the typed module is collected after the front passes.
data DeferredBackJob = DeferredBackJob
  { deferredKey       :: TaskKey
  , deferredPaths     :: Paths
  , deferredOptions   :: C.CompileOptions
  , deferredImplHash  :: B.ByteString
  , deferredNameCount :: Int
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
  , frInterestDeps :: Data.Set.Set (A.ModName, A.Name)
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

-- | An impl refresh computed from stored rows, before anything is written.
data Recomputation
  = Recomputed [InterfaceFiles.DepModuleInfo] [InterfaceFiles.NameHashInfo] FrontResult
  | Unresolved                          -- a dependency hash is unavailable: run the front passes
  | InterfaceGone                       -- the module's own .tydb cannot be read: the module fails

-- | The ModuleInfo of a module whose result is rebuilt from its .tydb
-- without a front pass, carrying the escape summaries stored there.
-- Modules checked later in the same build read them for their
-- Hashable.hash contracts.
storedEscapeModuleInfo :: FilePath -> A.ModName -> [A.ModName] -> [(A.Name, I.NameInfo)] -> Maybe String -> IO Acton.Env.ModuleInfo
storedEscapeModuleInfo tyFile mn imps ifaceTE mdoc = do
  summaries <- InterfaceFiles.openInterfaceDB tyFile >>= InterfaceFiles.readInterfaceDBEscapeSummaries
  evaluate (rnf summaries)
  return $ Acton.Env.withEscapeSummaries summaries (Acton.Env.mkModuleInfo mn imps ifaceTE mdoc)

type InterestMap = M.Map A.ModName (Data.Set.Set A.Name)

docNameCountThreshold :: Int
docNameCountThreshold = 10000

shouldGenerateDocOutput :: C.CompileOptions -> Bool -> Int -> Bool
shouldGenerateDocOutput opts tmp nameCount =
  not (C.skip_build opts) && not tmp && nameCount <= docNameCountThreshold

dbpDeferredBackJob :: Bool
                   -> Bool
                   -> C.CompileOptions
                   -> Paths
                   -> TaskKey
                   -> B.ByteString
                   -> Int
                   -> Maybe DeferredBackJob
dbpDeferredBackJob blocked hasNotImpl opts paths key moduleImplHash nameCount
  | C.no_dbp opts = Nothing
  | C.only_build opts = Nothing
  | C.altOutput opts = Nothing
  | isBuiltin key = Nothing
  | blocked = Nothing
  -- A module with NotImplemented bodies is implemented by a hand-written
  -- .ext.c and must be rendered whole, like __builtin__ -- its statement
  -- rows deliberately opt out of selective reads.
  | hasNotImpl = Nothing
  | otherwise =
      Just DeferredBackJob
        { deferredKey = key
        , deferredPaths = paths
        , deferredOptions = opts
        , deferredImplHash = moduleImplHash
        , deferredNameCount = nameCount
        }

interestDepsFromNameHashes :: [InterfaceFiles.NameHashInfo] -> Data.Set.Set (A.ModName, A.Name)
interestDepsFromNameHashes nameHashes =
  Data.Set.fromList
    [ target
    | nh <- nameHashes
    , (qn, _) <- InterfaceFiles.nhPubDeps nh ++ InterfaceFiles.nhImplDeps nh
    , Just target <- [qnameTarget qn]
    ]

-- | The module and name a qualified name refers to.
qnameTarget                             :: A.QName -> Maybe (A.ModName, A.Name)
qnameTarget (A.GName m n)               = Just (m, n)
qnameTarget (A.QName m n)               = Just (m, n)
qnameTarget (A.NoQ _)                   = Nothing

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
  (res, elapsed) <- timed (try action :: IO (Either SomeException ()))
  onDone key kind (either (const Nothing) (const (Just elapsed)) res) `catch` ignoreProgressException
  either throwIO return res
  where
    ignoreProgressException :: SomeException -> IO ()
    ignoreProgressException err = when (isAsyncException err) (throwIO err)

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

frontOutputFailure :: FrontOutputJob -> IO (Maybe CompileFailure)
frontOutputFailure job = do
  mErr <- frontOutputException job
  case mErr of
    Nothing -> return Nothing
    Just err
      | isAsyncException err -> throwIO err
      | otherwise -> return (Just (OutputFailed (fojKey job) (fojKind job) (displayException err)))

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

-- | A module as discovery found it.
data CompileTask
  = TyTask         InterfaceFiles.HeaderSummary              -- a .tydb header trusted while the source is unchanged
  | ParseTask      ModuleSource [A.ModName] (Maybe String)   -- header imports and docstring
  | ActonTask      ModuleSource A.Module                     -- parsed; imports as written until the front stage adjusts them
  | ParseErrorTask [Diagnostic String]

-- | The text and bytes of one read of a module's source (two handles for a
-- disk read): the text is parsed, the bytes are hashed, and the stat is
-- recorded in its .tydb.
data ModuleSource = ModuleSource
  { sourceText  :: String
  , sourceBytes :: B.ByteString
  , sourceMeta  :: Maybe InterfaceFiles.SourceFileMeta     -- Nothing for an editor overlay
  }

data TaskKey = TaskKey { tkProj :: FilePath, tkMod :: A.ModName } deriving (Eq, Ord, Show)

isBuiltin                               :: TaskKey -> Bool
isBuiltin k                             = tkMod k == mBuiltin

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

taskSourceFile                          :: GlobalTask -> FilePath
taskSourceFile t                        = srcFile (gtPaths t) (tkMod (gtKey t))

-- | Whether a module is parsed in a stage of its own: a source module is,
-- except __builtin__, which is parsed where it is compiled, and except under
-- --only-build, which compiles no .act file.
hasParseStage                           :: CompileContext -> GlobalTask -> Bool
hasParseStage ctx t                     = case gtTask t of
                                            ParseTask{} -> not (isBuiltin (gtKey t)) && not (C.only_build (optionsFor ctx (gtKey t)))
                                            _           -> False

-- | The modules a task imports: a cached header's imports are canonical, a
-- source's as written.
importsOf                               :: CompileTask -> [A.ModName]
importsOf (TyTask h)                    = map fst (InterfaceFiles.summaryImports h)
importsOf (ParseTask _ imps _)          = imps
importsOf (ActonTask _ m)               = A.importsOf m
importsOf (ParseErrorTask _)            = []

restoredImportsOf                       :: Paths -> CompileTask -> [A.ModName]
restoredImportsOf paths (TyTask h)      = map (dropProjPrefix paths . fst) (InterfaceFiles.summaryImports h)
restoredImportsOf paths task            = importsOf task

checkImportPrefixes key proj modSets imps
  | proj `elem` special_projects
                        = return ()
  | not $ null prefixed = throwProjectError ("Do not use prefix '" ++ proj ++ "' when referring to modules in that project (" ++ prstrs prefixed ++ ")")
  | otherwise           = return ()
  where p0              = tkProj key
        Just mods0      = M.lookup p0 modSets
        prefixed        = [ mn | mn <- imps, Data.Set.member mn mods0 ]

-- | Source imports may name only local modules or directly declared projects.
-- Transitive interfaces remain available for types mentioned by imported APIs.
checkImportDependencies :: Paths -> M.Map FilePath ProjCtx -> M.Map FilePath (Data.Set.Set A.ModName) -> [A.ModName] -> IO ()
checkImportDependencies paths projects modSets imps =
    forM_ imps $ \mn ->
      case A.modPath mn of
        n:_ | Data.Set.member n projectNames
            , n `notElem` directNames
            , not (Data.Set.member (addProjPrefix paths mn) localMods) ->
          throwProjectError ("Dependency '" ++ n ++ "' imported by " ++ srcFile paths (modName paths)
                             ++ " is not declared in " ++ projPath paths ++ "/Build.act.\n"
                             ++ "Add a direct dependency, using follows to share another dependency's selection.")
        _ -> return ()
  where
    projectNames = Data.Set.fromList [ BuildSpec.specName (projBuildSpec ctx) | ctx <- M.elems projects ]
    directNames = "std" : [ BuildSpec.specName (projBuildSpec ctx)
                          | root <- maybe [] (map snd . projDeps) (M.lookup (projPath paths) projects)
                          , Just ctx <- [M.lookup root projects] ]
    localMods = M.findWithDefault Data.Set.empty (projPath paths) modSets

-- | Resolve imports to in-graph providers using project search order.
-- This chooses the first project in the search order that declares the module,
-- producing TaskKeys for dependency edges.
resolveProviders :: TaskKey -> String -> [FilePath] -> M.Map FilePath (Data.Set.Set A.ModName) -> Data.Set.Set ProjName -> [A.ModName] -> M.Map A.ModName TaskKey
resolveProviders key proj order modSets deps imps =
    M.fromList $ concat $ map findProvider imps'
  where imps'           = [ A.modName ns | mn <- imps, let n:ns = A.modPath mn, n == proj ] ++ map dropLib imps
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
                 -> IO [GlobalTask]
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
        orderCache = M.fromList [ (projRoot ctx, projRoot ctx : map snd (projDeps ctx)) | (ctx, _) <- perProj ]
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
    return (reverse tasks)
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
              task0 <- readModuleTask sp gopts opts paths actFile
              -- Cached imports have canonical names: bare dep imports are stored
              -- as dep.lib. If that namespace has become local, recover the
              -- source spelling before resolving it and rebuild its interface.
              task <- case task0 of
                        TyTask _ | any (overlapsLocal paths modSets) (importsOf task0) ->
                          either ParseErrorTask (uncurry ActonTask) <$> materialize sp opts (tkMod k) actFile Nothing task0
                        _ -> return task0
              let order = M.findWithDefault [tkProj k] (tkProj k) orderCache
                  deps = M.findWithDefault Data.Set.empty (tkProj k) allDeps
                  imps = restoredImportsOf paths task
              checkImportPrefixes k (projName paths) modSets imps
              checkImportDependencies paths projMap modSets imps
              let providers = resolveProviders k (projName paths) order modSets deps imps
                  newKeys = M.elems providers
                  acc' = GlobalTask { gtKey = k
                                    , gtPaths = paths
                                    , gtTask = task
                                    , gtImportProviders = providers
                                    } : acc
              go modMaps modSets allDeps orderCache (Data.Set.insert k seen) (qs ++ newKeys) acc'


    overlapsLocal paths modSets mn =
      case A.modPath mn of
        n:ns | n /= projName paths ->
          any (`Data.Set.member` localMods)
              (addProjPrefix paths mn : [ addProjPrefix paths (A.modName [n]) | ns == ["lib"] ])
        _ -> False
      where localMods = M.findWithDefault Data.Set.empty (projPath paths) modSets


-- | The import graph of the tasks: each task's edges go to its import
-- providers among the tasks.
importGraph :: [GlobalTask] -> Graph TaskKey
importGraph tasks = graph [ (gtKey t, M.elems (gtImportProviders t)) | t <- tasks ]

-- | A plan's discovered modules as one graph, built once per plan.
data ModuleGraph = ModuleGraph
  { graphTasks     :: [GlobalTask]                      -- in discovery order
  , graphIndex     :: M.Map TaskKey GlobalTask
  , graphImports   :: Graph TaskKey
  , graphLibraries :: [Data.Set.Set TaskKey]            -- the modules of each declared build library
  }

moduleGraph :: M.Map FilePath ProjCtx -> [GlobalTask] -> ModuleGraph
moduleGraph projMap tasks = ModuleGraph
    { graphTasks     = tasks
    , graphIndex     = M.fromList [ (gtKey t, t) | t <- tasks ]
    , graphImports   = importGraph tasks
    , graphLibraries = libraryTaskGroups projMap tasks
    }

-- | The graph's tasks whose keys are in the set, in discovery order.
restrictTasks :: ModuleGraph -> Data.Set.Set TaskKey -> [GlobalTask]
restrictTasks g keys = [ t | t <- graphTasks g, Data.Set.member (gtKey t) keys ]

-- | Select the subgraph needed for a given build request.
-- Maps file paths to TaskKeys, adds __builtin__, and computes the reachable
-- dependency closure.
selectNeededTasks :: Paths -> FilePath -> ModuleGraph -> [FilePath] -> IO [GlobalTask]
selectNeededTasks pathsRoot rootProj g srcFiles = do
    requestedKeys <- catMaybes <$> mapM lookupTaskKey srcFiles
    let wantedNames   = map takeFileName srcFiles
        requestedKeys' = if null requestedKeys
                           then [ gtKey t
                                | t <- graphTasks g
                                , takeFileName (taskSourceFile t) `elem` wantedNames
                                ]
                           else requestedKeys
        builtinKeys = [ gtKey t | t <- graphTasks g, isBuiltin (gtKey t) ]
        startKeys   = if null requestedKeys' then map gtKey (graphTasks g) else requestedKeys' ++ builtinKeys
    return (restrictTasks g (closure (depsOf (graphImports g)) startKeys))
  where
    lookupTaskKey f = do
      absF <- canonicalizePath f
      let byPath = listToMaybe [ gtKey t | t <- graphTasks g, taskSourceFile t == absF ]
      case byPath of
        Just k -> return (Just k)
        Nothing -> do
          mn <- moduleNameFromFile (srcDir pathsRoot) (projName pathsRoot) absF
          return $ listToMaybe [ gtKey t
                               | t <- graphTasks g
                               , tkProj (gtKey t) == rootProj
                               , tkMod (gtKey t) == mn
                               ]

-- | Select the modules an edit of the changed files affects: every dependent
-- of a changed module, every module on a dependency path from those to a DBP
-- provider, and every dependent of the providers on those paths, because
-- every consumer of a deferred provider can contribute selected-name
-- interest.  Changes that match no module select everything.  Safe on import
-- cycles, which the run reports later.  The tasks keep their full provider
-- maps, so unchanged imports still resolve through cached interfaces.
selectAffectedTasks :: CompileContext -> ModuleGraph -> Data.Set.Set TaskKey -> [FilePath] -> IO [GlobalTask]
selectAffectedTasks ctx g dbpBlocked changedFiles
  | null changedFiles = return (graphTasks g)
  | otherwise = do
      absChanged <- mapM normalizePathSafe changedFiles
      taskPaths <- forM (graphTasks g) $ \t -> do
        p <- normalizePathSafe (taskSourceFile t)
        return (p, gtKey t)
      let pathIndex = M.fromList taskPaths
      return $ case mapMaybe (`M.lookup` pathIndex) absChanged of
        []      -> graphTasks g
        changed -> restrictTasks g (affectedBy changed)
  where
    imports = graphImports g
    affectedBy changed = Data.Set.unions [affected, onPaths, dependents imports providers]
      where affected  = dependents imports changed
            onPaths   = pathsTo imports isDbpProvider (Data.Set.toList affected)
            providers = filter isDbpProvider (Data.Set.toList onPaths)

    isDbpProvider k =
      case gtTask t of
        TyTask h ->
          isJust (dbpDeferredBackJob (Data.Set.member k dbpBlocked) False (optionsFor ctx k) (gtPaths t) k
                                     (InterfaceFiles.summaryImplHash h) (InterfaceFiles.summaryNameCount h))
        _ -> False
      where t = graphIndex g M.! k

-- | DBP may prune generated C/H for internal library modules, but a module at
-- an explicit library boundary must keep its full generated surface.  A
-- member is at the boundary when a module outside its library imports it, so
-- members nobody imports stay DBP-eligible.
libraryBoundaryTasks :: ModuleGraph -> Data.Set.Set TaskKey
libraryBoundaryTasks g =
    Data.Set.unions [ Data.Set.filter (importedOutside group) group | group <- graphLibraries g ]
  where importedOutside group = any (`Data.Set.notMember` group) . usersOf (graphImports g)

moduleStringToName :: String -> A.ModName
moduleStringToName = A.modName . splitMod
  where
    splitMod s =
      case break (== '.') s of
        (p, []) -> [p]
        (p, _:rest) -> p : splitMod rest

-- | The modules of each declared build library, for every project's
-- libraries.  A library module is the task of that name that belongs to the
-- declaring project.
libraryTaskGroups :: M.Map FilePath ProjCtx -> [GlobalTask] -> [Data.Set.Set TaskKey]
libraryTaskGroups projMap globalTasks =
    [ Data.Set.fromList keys
    | ctx <- M.elems projMap
    , lib <- M.elems (BuildSpec.libraries (projBuildSpec ctx))
    , let keys = [ gtKey t
                 | modName <- BuildSpec.libModules lib
                 , t <- M.findWithDefault [] (moduleStringToName (proj ++ "." ++ modName)) tasksByModule
                 , taskInProject ctx t
                 ]
          proj = BuildSpec.specName $ projBuildSpec ctx
    , not (null keys)
    ]
  where
    tasksByModule = M.fromListWith (flip (++)) [ (tkMod (gtKey t), [t]) | t <- globalTasks ]

    taskInProject ctx t =
      tkProj (gtKey t) == projRoot ctx
        || projPath (gtPaths t) == projRoot ctx
        || projTypes (gtPaths t) == projTypesDir ctx
        || srcUnderProject ctx t

    srcUnderProject ctx t = addTrailingPathSeparator (projSrcDir ctx) `isPrefixOf` taskSourceFile t

-- | Declared build libraries are artifact units: selecting any member selects
-- every module in that library, plus their dependencies.
expandBuildLibraryTasks :: ModuleGraph -> [GlobalTask] -> [GlobalTask]
expandBuildLibraryTasks g selectedTasks =
    restrictTasks g (close (Data.Set.fromList (map gtKey selectedTasks)))
  where
    close keys =
      let keys' = addLibraries (closure (depsOf (graphImports g)) (Data.Set.toList (addLibraries keys)))
      in if keys' == keys then keys else close keys'

    addLibraries keys =
      foldl' add keys (graphLibraries g)
      where
        add acc group
          | Data.Set.null (Data.Set.intersection acc group) = acc
          | otherwise = Data.Set.union acc group

-- | A module in the cheapest form discovery can use.  Its .tydb header is
-- reused when the compiler binary is not newer than the .tydb and the source
-- is unchanged: by recorded stat with the source older than the .tydb, else by
-- content hash (then the recorded stat is refreshed in place).  An editor
-- overlay is compared by content only.  Otherwise only the source header is
-- parsed.  Reading can write: the stat refresh.
readModuleTask :: Source.SourceProvider -> C.GlobalOptions -> C.CompileOptions -> Paths -> FilePath -> IO CompileTask
readModuleTask sp gopts opts paths file = do
    reusable <- reusableHeader opts tydb
    case reusable of
      Nothing               -> fromSource =<< Source.readSource sp file
      Just (header, tydbNs) -> do
        overlay <- Source.spReadOverlay sp file
        case overlay of
          Just snap -> verify header snap Nothing
          Nothing   -> do
            meta <- readSourceFileMeta file
            if InterfaceFiles.summarySourceMeta header == Just meta && InterfaceFiles.sfmMTimeNs meta < tydbNs
              then return (TyTask header)
              else Source.spReadFile sp file >>= \snap -> verify header snap (Just meta)
  where
    mn   = modName paths
    tydb = tyDbPath paths mn

    fromSource snap = do
      parsed <- parseActHeaderSnapshot mn file snap
      case parsed of
        Left diags        -> return (ParseErrorTask diags)
        Right (imps, doc) -> (\source -> ParseTask source imps doc) <$> moduleSource file snap

    -- The stat refresh updates one row, failures ignored.  Rewriting the
    -- whole .tydb would re-derive its dependency rows from the stored
    -- per-name hashes, whose external dependencies are stripped, and so lose
    -- their users and hashes.
    verify header snap meta = do
      let hash = SHA256.hash (Source.ssBytes snap)
          same = hash == InterfaceFiles.summarySourceHash header
      when (C.verbose gopts && Source.ssIsOverlay snap) $ do
        actTime <- Source.spGetModTime sp file
        tyTime  <- InterfaceFiles.interfaceModifiedTime tydb
        putStrLn ("[debug] readModuleHeader " ++ modNameToString mn
                  ++ " act=" ++ show actTime
                  ++ " ty=" ++ show tyTime
                  ++ " hash=" ++ shortHash hash
                  ++ " header=" ++ shortHash (InterfaceFiles.summarySourceHash header)
                  ++ " len=" ++ show (B.length (Source.ssBytes snap))
                  ++ if same then " (match)" else " (diff)")
      if not same then fromSource snap else do
        when (isJust meta && meta /= InterfaceFiles.summarySourceMeta header) $
          void (trySync (InterfaceFiles.updateSourceMeta tydb meta))
        return (TyTask header)

-- | The .tydb header and the .tydb's modification time, unless the .tydb is
-- missing, its header unreadable or the compiler binary newer.  A failure to
-- stat the .tydb itself propagates.
reusableHeader :: C.CompileOptions -> FilePath -> IO (Maybe (InterfaceFiles.HeaderSummary, Integer))
reusableHeader opts tydb = do
    exists <- InterfaceFiles.interfaceExists tydb
    header <- if exists then either (const Nothing) Just <$> trySync (InterfaceFiles.readHeaderSummary tydb)
                        else return Nothing
    case header of
      Nothing -> return Nothing
      Just h  -> do
        tydbNs <- InterfaceFiles.interfaceModifiedTimeNs tydb
        newer  <- compilerNewerThan opts tydbNs
        return (if newer then Nothing else Just (h, tydbNs))

-- | Whether the running compiler binary was modified after a time, in POSIX
-- nanoseconds.  Checked on every call.  A failure to find or stat the binary
-- counts as not newer.
compilerNewerThan :: C.CompileOptions -> Integer -> IO Bool
compilerNewerThan opts ns
  | C.ignore_compiler_version opts      = return False
  | otherwise                           = do
      status <- trySync (getExecutablePath >>= getFileStatus)
      return (either (const False) (\st -> InterfaceFiles.posixNanoseconds (modificationTimeHiRes st) > ns) status)

-- | A module source from a snapshot: an overlay records no stat.
moduleSource :: FilePath -> Source.SourceSnapshot -> IO ModuleSource
moduleSource file snap = ModuleSource (Source.ssText snap) (Source.ssBytes snap) <$> meta
  where meta | Source.ssIsOverlay snap  = return Nothing
             | otherwise                = Just <$> readSourceFileMeta file

-- | A module's documentation index entry: its name, its docstring and
-- whether it gets doc output.
docIndexEntry                           :: C.CompileOptions -> Paths -> CompileTask -> Maybe (A.ModName, Maybe String, Bool)
docIndexEntry opts paths (TyTask h)     = Just (modName paths, InterfaceFiles.summaryDoc h,
                                                shouldGenerateDocOutput opts (isTmp paths) (InterfaceFiles.summaryNameCount h))
docIndexEntry _ paths (ParseTask _ _ d) = Just (modName paths, d, True)
docIndexEntry _ _ _                     = Nothing

-- | The parsed source of a module.  A ParseTask parses the text read at
-- discovery, whose hash decided it; a TyTask reads its source again
-- (overlay-aware).  Imports stay as written: the front stage adjusts them.
materialize :: Source.SourceProvider -> C.CompileOptions -> A.ModName -> FilePath -> Maybe (ParseProgress -> IO ())
            -> CompileTask -> IO (Either [Diagnostic String] (ModuleSource, A.Module))
materialize sp opts mn file progress task = case task of
    ActonTask source m   -> return (Right (source, m))
    ParseErrorTask diags -> return (Left diags)
    ParseTask source _ _ -> do display <- displayPath file
                               fmap ((,) source) <$> parseActSource opts mn display (sourceText source) progress
    TyTask _             -> do parsed <- parseActFile opts sp mn file progress
                               forM parsed $ \(snap, m) -> (\source -> (source, m)) <$> moduleSource file snap

-- | A parsed module whose imports name the modules of their in-graph
-- providers.  A plain import keeps its written name as its alias.
-- Idempotent.
adjustImports :: M.Map A.ModName TaskKey -> A.Module -> A.Module
adjustImports providers m = m{ A.imps = imps' }
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

-- | Decide whether to suppress per-module timing/log output.
-- We also treat any alternate output mode (parse/sigs/cgen/etc) as quiet.
quiet :: C.GlobalOptions -> C.CompileOptions -> Bool
quiet gopts opts = C.quiet gopts || C.altOutput opts

-- | Read a reusable interface shell from the module's own .tydb and return
-- its hashes.  This is used when a module is deemed fresh and we want to
-- avoid reparsing.
readIfaceFromTy :: Paths -> A.ModName -> String -> Maybe B.ByteString -> IO (Either [Diagnostic String] ([A.ModName], [(A.Name, I.NameInfo)], Maybe String, B.ByteString, B.ByteString, Maybe Acton.Env.ModuleInfo))
readIfaceFromTy paths mn src mHash = do
    hashes <- InterfaceFiles.readModuleHashesMaybe tyFile
    mdb <- InterfaceFiles.openInterfaceDBMaybe tyFile
    case (hashes, mdb) of
      (Just (_srcH, pubH, implH), Just db) -> do
        (ms, mdoc) <- InterfaceFiles.readInterfaceDBModuleInfo db
        ih <- case mHash of
                Just h -> return h
                Nothing -> return pubH
        return $ Right (ms, [], mdoc, ih, implH, Just (Acton.Env.mkTyFileModuleInfo mn ms mdoc db))
      _ -> return $ Left (missingIfaceDiagnostics mn src mn)
  where tyFile = tyDbPath paths mn


-- | Snapshot of expected/recorded impl hashes for generated code.
data CodegenStatus = CodegenStatus
  { csExpected :: String
  , csC :: Maybe String
  , csH :: Maybe String
  , csSupport :: Maybe String
  }

-- | A hash in lowercase hex, as generated code is tagged with it.
hashHex                                 :: B.ByteString -> String
hashHex                                 = B.unpack . Base16.encode

-- | The first eight hex digits of a hash, as verbose lines show it.
shortHash                               :: B.ByteString -> String
shortHash                               = take 8 . hashHex

-- | Parse a tagged hash line from generated output.
extractCodegenHash :: String -> Maybe String
extractCodegenHash line = do
  rest <- Data.List.stripPrefix Acton.CodeGen.implHashTag line
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
      res <- trySync (withFile path ReadMode hGetLine)
      case res of
        Left _ -> return Nothing
        Right line -> return (extractCodegenHash line)

-- | Collect codegen hash status for a module.
codegenStatus :: Paths -> A.ModName -> B.ByteString -> IO CodegenStatus
codegenStatus paths mn implHash = do
  let expected = hashHex implHash
      cFile = outBase paths mn ++ ".c"
      hFile = outBase paths mn ++ ".h"
  cHash <- readCodegenHash cFile
  hHash <- readCodegenHash hFile
  supportHash <- readCodegenHash (Acton.CodeGen.supportFile (outBase paths mn))
  return CodegenStatus { csExpected = expected, csC = cHash, csH = hHash, csSupport = supportHash }

-- | Check every generated artifact against the expected impl hash.
codegenUpToDate :: CodegenStatus -> Bool
codegenUpToDate status =
  all (== Just (csExpected status)) [csC status,csH status,csSupport status]

-- | Format a short delta message for stale codegen.
formatCodegenDelta :: CodegenStatus -> String
formatCodegenDelta status =
  let expected = take 8 (csExpected status)
      short actual = maybe "missing" (take 8) actual
  in case (csC status, csH status, csSupport status) of
       (Just c, Just h, Just support) | c == h && h == support ->
         " {impl " ++ take 8 c ++ " -> " ++ expected ++ "}"
       _ ->
         " {impl c " ++ short (csC status) ++ " -> " ++ expected
         ++ ", h " ++ short (csH status) ++ " -> " ++ expected
         ++ ", support " ++ short (csSupport status) ++ " -> " ++ expected ++ "}"

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
               -> (A.ModName -> A.Name -> IO (Maybe InterfaceFiles.NameHashInfo))
               -> (FrontPassProgress -> IO ())
               -> (TaskKey -> FrontOutputKind -> IO ())
               -> (TaskKey -> FrontOutputKind -> FrontOutputProgress -> IO ())
               -> (TaskKey -> FrontOutputKind -> Maybe TimeSpec -> IO ())
               -> IO Bool
               -> ([FrontOutputJob] -> IO ())
               -> IO (Either [Diagnostic String] FrontResult)
runFrontPasses gopts opts dbpBlocked paths env0 parsed srcContent srcBytes sourceMeta resolveImportHash resolveImportImplHash resolveNameHash onFrontProgress onFrontOutputStart onFrontOutputProgress onFrontOutputDone shouldWriteFrontOutput recordFrontOutputJobs = do
  createDirectoryIfMissing True (moduleTypesDir (projTypes paths) mn)
  core
    `catch` handleGeneral
    `catch` handleCompilation
    `catch` handleTypeErrors
    `catch` handleTypeError
  where
    mn = A.modname parsed
    filename = modNameToFilename (dropProjPrefix paths mn)
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

    resolveDepHashes :: String
                     -> (InterfaceFiles.NameHashInfo -> B.ByteString)
                     -> M.Map A.Name [A.QName]
                     -> M.Map A.Name SrcLoc
                     -> IO (Either [Diagnostic String] (M.Map A.Name [(A.QName, B.ByteString)]))
    resolveDepHashes label getHash deps nameLocs = do
      -- Many owners share the same few external deps; resolve each once.
      memo <- newIORef HM.empty
      let resolveMemo m n = do
            cache <- readIORef memo
            case HM.lookup (m, n) cache of
              Just r -> return r
              Nothing -> do
                r <- resolveNameHash m n
                modifyIORef' memo (HM.insert (m, n) r)
                return r
          lookupName owner m n = do
            mInfo <- resolveMemo m n
            return $ case mInfo of
              Just info ->
                let h = getHash info
                    loc = M.findWithDefault NoLoc owner nameLocs
                in if B.null h
                     then Left (missingDepHashDiagnostics label owner loc (A.GName m n))
                     else Right (Just (A.GName m n, h))
              Nothing -> Left (missingNameHashDiagnostics (A.GName m n))
      resolved <- forM (M.toList deps) $ \(owner, qns) -> do
        resolvedQns <- forM qns $ \qn -> case qnameTarget qn of
          Just (m, n) -> lookupName owner m n
          Nothing     -> return (Left (missingNameHashDiagnostics qn))
        let (errs, vals) = partitionEithers resolvedQns
        return $ if null errs
          then Right (owner, catMaybes vals)
          else Left (concat errs)
      let (errs, vals) = partitionEithers resolved
      return $ if null errs then Right (M.fromDistinctAscList vals) else Left (concat errs)

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
      dumpWhen opts mn C.parse "parse" (Pretty.print parsed)
      dumpWhen opts mn C.parse_ast "parse-ast" (renderStyle prettyAstStyle (ppDoc parsed))

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
              forM_ mActive $ \(bindNames, activeTotal, t0) ->
                modifyIORef' typeStmtTimingsRef
                  ( TypeStmtTiming
                      { tstCompleted = completed
                      , tstTotal = activeTotal
                      , tstNames = bindNames
                      , tstTime = now - t0
                      }
                  : )
              case current of
                Just _ -> writeIORef typeActiveRef (Just (names, total, now))
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
      dumpWhen opts mn C.kinds "kinds" (Pretty.print kchecked)
      timeKindsCheck <- getTime Monotonic

      -- Type-check and return both the typed AST and the interface NameInfo.
      (nmod,tchecked,typeEnv,tests) <- Acton.Types.reconstruct (Just onTypeProgress) inferredSignatureCb env kchecked (Just (modNameToString (dropProjPrefix paths mn)))
      timeTypeReconstruct <- getTime Monotonic
      forceTypeResult nmod tchecked typeEnv tests
      timeTypeForce <- getTime Monotonic
      -- Escape summaries are part of the internal compilation interface.  In
      -- particular, every Hashable.hash implementation is checked here before
      -- B_hash may pass it an automatically allocated hasher.  The command
      -- line flag controls only the diagnostic dump.
      let importedEscapeSummary qn = do
            (m,n) <- resolveImportedName qn
            mi <- Acton.Env.lookupModuleInfo m env
            stored <- Acton.Env.moduleEscapeSummary mi [n]
            return [ (pn, if mayEscape then Acton.EscapeAnalysis.MayEscape else Acton.EscapeAnalysis.NoEscape)
                   | (pn,mayEscape) <- stored ]
          resolveImportedName (A.NoQ n) =
            case Acton.Env.lookupName n env of
              Just (I.NAlias qn') -> resolveImportedName qn'
              _ -> Nothing
          resolveImportedName qn = qnameTarget qn
          escapeReport = Acton.EscapeAnalysis.analyzeModuleWithImports importedEscapeSummary tchecked
          escapeSummaries = Acton.EscapeAnalysis.interfaceSummaries escapeReport
      -- Run the analysis to completion for every function of every module,
      -- here rather than in whatever later reads a result, such as the
      -- interface write.  The analysis evaluates each function's summary in
      -- full as part of producing the list.
      evaluate (length (Acton.EscapeAnalysis.reportFunctions escapeReport))
      evaluate (rnf escapeSummaries)
      dumpWhen opts mn C.escape_analysis "escape-analysis" (Acton.EscapeAnalysis.renderEscapeReport escapeReport)
      case Acton.EscapeAnalysis.reportContractViolations escapeReport of
        [] -> return ()
        ((fid@(Acton.EscapeAnalysis.FunctionId path), parameter):_) ->
          let directReasons =
                [ reason
                | (fid', parameters) <- Acton.EscapeAnalysis.reportFunctions escapeReport
                , fid' == fid
                , summary <- parameters
                , Acton.EscapeAnalysis.parameterName summary == parameter
                , reason <- Acton.EscapeAnalysis.parameterDirectUses summary
                ]
              reasonLoc reason = case reason of
                Acton.EscapeAnalysis.Returned l -> l
                Acton.EscapeAnalysis.Stored l -> l
                Acton.EscapeAnalysis.Raised l -> l
                Acton.EscapeAnalysis.Yielded l -> l
                Acton.EscapeAnalysis.Captured l -> l
                Acton.EscapeAnalysis.Asynchronous l -> l
                Acton.EscapeAnalysis.ImportedCall l -> l
                Acton.EscapeAnalysis.UnknownCall l -> l
                Acton.EscapeAnalysis.UnavailableBody l -> l
              errorLoc = case [ l | reason <- directReasons, let l = reasonLoc reason, l /= NoLoc ] of
                l:_ -> l
                [] -> loc parameter
          in Acton.Env.err errorLoc $
            "Hashable.hash implementation " ++ intercalate "." (map A.nstr path) ++
            " may let its hasher parameter '" ++ A.nstr parameter ++
            "' escape; Hashable.hash must consume the hasher only during the call"
      timeTypeEscape <- getTime Monotonic
      -- Store roots so later builds can discover entry points without reparse.
      let I.NModule imps fullIface mdoc = nmod
          publicIface = publicIfaceTE fullIface
      let roots = [ n | (n,i) <- fullIface, rootEligible i ]
      -- Extract top-level items from parsed and typed ASTs for per-name hashes.
      let srcItems = Hashing.topLevelItems parsed
          implItems = Hashing.topLevelItems tchecked
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
          hashEnv = setMod mn env
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
      evaluate (sourceHashWork + implHashWork + ifaceHashWork + ifaceDepWork + implDepWork)
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
          evaluate (rnf nameKeys)
          selfPubHashes <-
            Hashing.nameInfoHashesWithProgress
              (hashProgress afterImplHashes)
              nameInfoMap
          evaluate (rnf selfPubHashes)
          -- Importers rely on a top-level function's escape summary when they
          -- check their own Hashable.hash implementations, so the summary is
          -- part of that function's public interface.  Folding it into the
          -- name's public hash turns a summary change into a public delta for
          -- exactly the dependents that use the name.
          let escapeBits = M.fromList
                [ (n, B.pack [ if mayEscape then '1' else '0' | (_, mayEscape) <- ps ])
                | ([n], ps) <- escapeSummaries ]
              selfPubHashesEsc = M.mapWithKey
                (\n h -> maybe h (\bits -> SHA256.hash (h <> B.pack "escape:" <> bits)) (M.lookup n escapeBits))
                selfPubHashes
          evaluate (rnf selfPubHashesEsc)
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
              extMods = Data.Set.toList (Hashing.externalModules pubExtDeps `Data.Set.union` Hashing.externalModules implExtDeps)
          evaluate (rnf (pubLocalDeps, pubExtDeps, extMods))
          depModulesRes <- resolveDepModuleHashes extMods
          case depModulesRes of
            Left diags -> return (Left diags)
            Right depModules -> do
              -- Resolve external deps to their recorded hashes.
              pubSigExtRes <- resolveDepHashes "pub" InterfaceFiles.nhPubHash pubSigExtDeps nameLocs
              pubExtRes <- resolveDepHashes "pub" InterfaceFiles.nhPubHash pubExtDeps nameLocs
              implExtRes <- resolveDepHashes "impl" InterfaceFiles.nhImplHash implExtDeps nameLocs
              case (pubSigExtRes, pubExtRes, implExtRes) of
                (Left diags, _, _) -> return (Left diags)
                (_, Left diags, _) -> return (Left diags)
                (_, _, Left diags) -> return (Left diags)
                (Right pubSigExtHashes, Right pubExtHashes, Right implExtHashes) -> do
                  evaluate (rnf (pubSigExtHashes, pubExtHashes, implExtHashes))
                  -- The pub and impl passes are independent, and so are the
                  -- module hashes and the assembly that both use them.
                  (pubHashes, implHashes) <- concurrently
                    (evaluate (force (Hashing.computeHashesSortedDeps selfPubHashesEsc pubSigLocalDeps pubSigExtHashes)))
                    (evaluate (force (Hashing.computeHashesSortedDeps nameImplHashes implLocalDeps implExtHashes)))
                  let nameHashes0 =
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
                  ((modulePubHash, moduleImplHash), nameHashes) <- concurrently
                    (evaluate (force (Hashing.moduleHashesFromHashMaps nmod nameKeys pubHashes implHashes)))
                    (evaluate (force nameHashes0))
                  evaluate (rnf (moduleSrcBytesHash, modulePubHash, moduleImplHash, sourceMeta, impsWithHash))
                  evaluate (rnf (nameHashes, roots, tests, mdoc))
                  emitHashProgressPaced True hashProgressTotal hashProgressTotal Nothing
                  timeTypeHash <- getTime Monotonic

                  dumpWhen opts mn C.types "types" (Pretty.print tchecked)
                  dumpWhen opts mn C.sigs "sigs" (Acton.Types.prettySigs env mn imps fullIface)
                  timeTypeCheck <- getTime Monotonic

                  let writeTyDb outputKey = do
                        InterfaceFiles.writeFileWithProgress
                          (\p -> onFrontOutputProgress outputKey FrontOutputTydb (frontOutputTyDbProgress p))
                          A.version
                          (tyDbPath paths mn)
                          moduleSrcBytesHash modulePubHash moduleImplHash sourceMeta impsWithHash depModules nameHashes escapeSummaries roots tests mdoc nmod tchecked
                      writeDoc = do
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
                        writeFileUtf8Atomic docFile htmlDoc
                      docOutputActions =
                        if shouldGenerateDocOutput opts (isTmp paths) (length nameHashes)
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
                                 , ftTypeEscape = timeTypeEscape - timeTypeForce
                                 , ftTypeHash = timeTypeHash - timeTypeEscape
                                 , ftTypeStmtTimings = typeStmtTimings
                                 }
                          else Nothing
                      key = TaskKey (projPath paths) mn
                      deferredBackJob = dbpDeferredBackJob dbpBlocked (A.hasNotImpl (A.mbody tchecked)) opts paths key moduleImplHash (length nameHashes)
                      backJob =
                        case deferredBackJob of
                          Just _ -> Nothing
                          Nothing ->
                            Just BackJob { bjKey = key
                                         , bjPaths = paths
                                         , bjOpts = opts
                                         , bjTypeEnv = typeEnv
                                         , bjModule = tchecked
                                         , bjSource = srcContent
                                         , bjImplHash = moduleImplHash
                                         }
                  do
                    -- The .tydb commit is synchronous with front completion:
                    -- once this module reports done, its rows are on disk.
                    -- A write failure (disk full, permissions) fails THIS
                    -- module's front on the structured diagnostics path.
                    -- Docs and the interface copy are never read back within
                    -- this build, so they stay in the background.
                    writeRes <- trySync (runFrontOutputSync onFrontOutputStart onFrontOutputDone shouldWriteFrontOutput key FrontOutputTydb (writeTyDb key))
                    case writeRes of
                      Left err -> return (Left (tydbWriteDiagnostics mn err))
                      Right () -> do
                        outputJobs <- mask_ $ do
                          let startOutput =
                                startFrontOutputJob onFrontOutputStart onFrontOutputDone shouldWriteFrontOutput key
                          docJobs <-
                            forM docOutputActions $ uncurry startOutput
                          tyJobs <-
                            if C.tydb opts
                              then (:[]) <$> startOutput FrontOutputTydbCopy (copyTydbInterface opts paths mn)
                              else return []
                          let jobs = docJobs ++ tyJobs
                          recordFrontOutputJobs jobs
                          return jobs
                        return $ Right FrontResult { frIfaceTE = publicIface
                                                   , frImps = imps
                                                   , frDoc = mdoc
                                                   , frModuleInfo = Just $ Acton.Env.withEscapeSummaries escapeSummaries $
                                                              Acton.Env.mkModuleInfo mn imps publicIface mdoc
                                                   , frPubHash = modulePubHash
                                                   , frImplHash = moduleImplHash
                                                   , frNameHashes = publicNameHashes nameHashes
                                                   , frInterestDeps = interestDepsFromNameHashes nameHashes
                                                   , frFrontTime = frontTimeMaybe
                                                   , frFrontTiming = frontTimingMaybe
                                                   , frInferredSigs = inferredSigs
                                                   , frBackJob = backJob
                                                   , frDeferredBackJob = deferredBackJob
                                                   , frOutputJobs = outputJobs
                                                   }

data DbpSelection = DbpSelection
  { dbsModule :: A.Module
  , dbsSelectedCount :: Int
  , dbsFallbackReason :: Maybe String
  }

data DbpNameSelection = DbpNameSelection
  { dnsSelectedNames :: Data.Set.Set A.Name
  , dnsNameHashes :: [InterfaceFiles.NameHashInfo]
  }

prepareDeferredBackJob :: Source.SourceProvider
                       -> C.GlobalOptions
                       -> CompileCallbacks
                       -> Acton.Env.Env0
                       -> InterestMap
                       -> DeferredBackJob
                       -> IO (Maybe BackJob)
prepareDeferredBackJob sp gopts callbacks envAcc interestMap deferred = do
  let paths = deferredPaths deferred
      mn = tkMod (deferredKey deferred)
      tyFile = tyDbPath paths mn
      actFile = srcFile paths mn
  roots <- InterfaceFiles.readRoots tyFile
  let interested = M.findWithDefault Data.Set.empty mn interestMap
      rootSeeds = Data.Set.fromList roots
      selectedSeeds = Data.Set.union interested rootSeeds
      totalNames = deferredNameCount deferred
  nameSelection <- selectDbpNames paths mn tyFile selectedSeeds
  let codegenHash = dbpCodegenHash (deferredImplHash deferred) (dnsSelectedNames nameSelection)
  codegen <- codegenStatus paths mn codegenHash
  if codegenUpToDate codegen
    then do
      logDbpSelection gopts callbacks mn deferred totalNames (Data.Set.size interested) (Data.Set.size rootSeeds) (Data.Set.size (dnsSelectedNames nameSelection)) Nothing "generated code up to date"
      return Nothing
    else do
      selectedTmod <- InterfaceFiles.readSelectedModule tyFile (dnsNameHashes nameSelection) (dnsSelectedNames nameSelection)
      -- Same-version .tydb files always carry statement indices, so a failed
      -- selective read means a corrupt or hand-doctored file -- an eager
      -- whole-module fallback would mask that (and read gigabytes doing it).
      selection <- case selectedTmod of
        Just tmod -> return $ selectDbpModule totalNames nameSelection tmod
        Nothing -> error ("Internal error: missing statement rows in " ++ tyFile)
      snap <- Source.readSource sp actFile
      env1 <- Acton.Env.mkEnv (searchPath paths) envAcc (dbsModule selection)
      logDbpSelection gopts callbacks mn deferred totalNames (Data.Set.size interested) (Data.Set.size rootSeeds) (dbsSelectedCount selection) (dbsFallbackReason selection) "generated code out of date"
      return $ Just BackJob
        { bjKey = deferredKey deferred
        , bjPaths = paths
        , bjOpts = deferredOptions deferred
        , bjTypeEnv = Converter.convEnvProtos env1
        , bjModule = dbsModule selection
        , bjSource = Source.ssText snap
        , bjImplHash = codegenHash
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
logDbpSelection gopts callbacks mn deferred totalNames interestedCount rootCount selectedCount fallbackReason codegenReason =
  when (C.verbose gopts) $
    ccOnInfo callbacks $
      "  DBP " ++ modNameToString (dropProjPrefix (deferredPaths deferred) mn)
      ++ ": total names " ++ show totalNames
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
      [ A.nstr n
      | n <- Data.List.sortOn A.nameText (Data.Set.toList selected)
      ]

selectDbpNames :: Paths
               -> A.ModName
               -> FilePath
               -> Data.Set.Set A.Name
               -> IO DbpNameSelection
selectDbpNames paths mn tyFile seeds
  | Data.Set.null seeds =
      return DbpNameSelection
        { dnsSelectedNames = Data.Set.empty
        , dnsNameHashes = []
        }
  | otherwise = do
      roots <- traverse (dbpReadOwningName paths mn tyFile) (Data.Set.toList seeds)
      selected <- dbpNameHashClosure paths mn tyFile M.empty roots
      return DbpNameSelection
        { dnsSelectedNames = Data.Set.fromList (M.keys selected)
        , dnsNameHashes = M.elems selected
        }

dbpSelectionError :: Paths -> A.ModName -> String -> IO a
dbpSelectionError paths mn reason =
  throwIO (DbpSelectionError ("DBP selection failed for " ++ modNameToString (dropProjPrefix paths mn) ++ ": " ++ reason))

selectDbpModule :: Int
                -> DbpNameSelection
                -> A.Module
                -> DbpSelection
selectDbpModule totalNames nameSelection tmod@(A.Module loc imps mdoc suite)
  | A.hasNotImpl suite = fallback "module contains NotImplemented/native extension hooks"
  | otherwise =
      let selected = dnsSelectedNames nameSelection
          suite' = mapMaybe (dbpPruneTopStmt selected) suite
      in DbpSelection
           { dbsModule = A.Module loc imps mdoc suite'
           , dbsSelectedCount = Data.Set.size selected
           , dbsFallbackReason = Nothing
           }
  where
    fallback reason =
      DbpSelection
        { dbsModule = tmod
        , dbsSelectedCount = totalNames
        , dbsFallbackReason = Just reason
        }

dbpReadNameHash :: Paths -> A.ModName -> FilePath -> A.Name -> IO InterfaceFiles.NameHashInfo
dbpReadNameHash paths mn tyFile n = do
  mnh <- InterfaceFiles.readNameHashMaybe tyFile n
  case mnh of
    Just nh -> return nh
    Nothing -> dbpSelectionError paths mn ("hash info missing for " ++ A.nstr n)

dbpReadOwningName :: Paths -> A.ModName -> FilePath -> A.Name -> IO A.Name
dbpReadOwningName paths mn tyFile n = do
  mnh <- InterfaceFiles.readNameHashMaybe tyFile n
  case mnh of
    Just _ -> return n
    Nothing ->
      case n of
        A.Derived base _ -> dbpReadOwningName paths mn tyFile base
        _ | Names.isWitness n -> dbpSelectionError paths mn ("unresolved witness owner for " ++ A.nstr n)
        _ -> dbpSelectionError paths mn ("no top-level owner for " ++ A.nstr n)

dbpNameHashClosure :: Paths
                   -> A.ModName
                   -> FilePath
                   -> M.Map A.Name InterfaceFiles.NameHashInfo
                   -> [A.Name]
                   -> IO (M.Map A.Name InterfaceFiles.NameHashInfo)
dbpNameHashClosure _ _ _ selected [] = return selected
dbpNameHashClosure paths mn tyFile selected (n:ns)
  | M.member n selected = dbpNameHashClosure paths mn tyFile selected ns
  | otherwise = do
      nh <- dbpReadNameHash paths mn tyFile n
      localDeps <- traverse (dbpReadOwningName paths mn tyFile)
                     (InterfaceFiles.nhPubLocalDeps nh ++ InterfaceFiles.nhImplLocalDeps nh)
      exts <- dbpExtensionsForName tyFile n
      extDeps <- traverse (dbpReadOwningName paths mn tyFile) exts
      let deps = unionNames localDeps extDeps
          selected' = M.insert n nh selected
          new = filter (`M.notMember` selected') deps
      dbpNameHashClosure paths mn tyFile selected' (new ++ ns)
  where
    unionNames xs ys = Data.List.sortOn A.nameText (Data.List.nub (xs ++ ys))

dbpExtensionsForName :: FilePath -> A.Name -> IO [A.Name]
dbpExtensionsForName tyFile n = do
  byClass <- InterfaceFiles.readExtensionsByClass tyFile n
  byProtocol <- InterfaceFiles.readExtensionsByProtocol tyFile n
  return (Data.List.sortOn A.nameText (Data.List.nub (byClass ++ byProtocol)))

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


-- | What a job does with its generated code.
data BackOutput = PrintHeader | PrintCode | RenderOnly | WriteFiles deriving Eq

-- | --hgen prints the header and --cgen the C file (--hgen wins).  Any other
-- alternative output renders the code and discards it.  A normal build
-- writes the generated files.
backOutput                      :: C.CompileOptions -> BackOutput
backOutput opts
  | C.hgen opts                 = PrintHeader
  | C.cgen opts                 = PrintCode
  | C.altOutput opts            = RenderOnly
  | otherwise                   = WriteFiles

-- | Run the back passes of one job, from the normalizer to code generation,
-- then print or write the generated code.
--
-- The passes return lazily: their work happens in Render, or earlier in a
-- dump, so a pass's failure surfaces after Render has started.  Nothing is
-- forced between passes.  A dump prints inside its pass.  shouldWrite is
-- asked once, right before writing, and the closure support file, the .h
-- and the .c are written in that order.
-- The elapsed time is Nothing when quiet, and the timing is collected only
-- with --timing.
runBackPasses :: C.GlobalOptions -> BackJob -> IO Bool -> (BackPassProgress -> IO ()) -> IO (Maybe TimeSpec, Maybe BackTiming)
runBackPasses gopts job shouldWrite onProgress = do
    laps <- newIORef []
    let report :: BackPass -> Int -> BackPassEvent -> IO ()
        report p completed event = onProgress (BackPassProgress p completed total event)

        pass :: BackPass -> IO a -> IO a
        pass p work = do
          report p (fromEnum p) BackPassStarted
          (result, elapsed) <- timed work
          report p (fromEnum p + 1) (BackPassFinished elapsed)
          modifyIORef' laps ((p, elapsed) :)
          return result

        dumpPass :: BackPass -> A.Module -> IO ()
        dumpPass p m = forM_ (backPassDump p) $ \(flag, label) -> dumpWhen opts mn flag label (Pretty.print m)

        transform :: BackPass -> IO (A.Module, Acton.Env.Env0) -> IO (A.Module, Acton.Env.Env0)
        transform p work = pass p $ do
          result@(m, _) <- work
          dumpPass p m
          return result

        render :: String -> IO ()
        render text = evaluate (rnf text)

    ((), elapsed) <- timed $ do
      (normalized, env1)  <- transform BackPassNormalize  (Acton.Normalizer.normalize (bjTypeEnv job) (bjModule job))
      (deactorized, env2) <- transform BackPassDeactorize (Acton.Deactorizer.deactorize env1 normalized)
      (converted, env3)   <- transform BackPassCPS        (Acton.CPS.convert env2 deactorized)
      (lifted, env4)      <- transform BackPassLLift      (Acton.LambdaLifter.liftModule env3 converted)
      boxed               <- pass BackPassBoxing          (do m <- Acton.Boxing.doBoxing env4 lifted
                                                              dumpPass BackPassBoxing m
                                                              return m)
      (_, h, c, support)  <- pass BackPassCodeGen         (Acton.CodeGen.generateCode (output == WriteFiles) env4 relSrcBase (bjSource job) (not (C.dbg_no_lines opts)) boxed (hashHex (bjImplHash job)))
      case output of
        PrintHeader -> pass BackPassRender (render h) >> putStrLn h
        PrintCode   -> pass BackPassRender (render c) >> putStrLn c
        RenderOnly  -> pass BackPassRender (render h >> render c >> render support)
        WriteFiles  -> do
          pass BackPassRender (render h >> render c >> render support)
          current <- shouldWrite
          if current
            then pass BackPassWrite (writeFileUtf8Atomic (Acton.CodeGen.supportFile base) support >> writeFileUtf8Atomic (base ++ ".h") h >> writeFileUtf8Atomic (base ++ ".c") c)
            else report BackPassWrite (fromEnum BackPassWrite + 1) BackPassSkipped
    timing <- reverse <$> readIORef laps
    return ( if quiet gopts opts then Nothing else Just elapsed
           , if C.timing gopts then Just timing else Nothing )
  where
    opts       = bjOpts job
    paths      = bjPaths job
    mn         = tkMod (bjKey job)
    output     = backOutput opts
    base       = outBase paths mn
    relSrcBase = makeRelative (projPath paths) (srcBase paths mn)
    total      = fromEnum (maxBound :: BackPass) + (if output == WriteFiles then 1 else 0)


-- | Source size plus the heaviest chain of modules waiting for the module.
-- The map is defined in terms of itself: sound because the graph is acyclic
-- (checked before) and Data.Map is the lazy one.
criticalWeights                         :: Graph TaskKey -> M.Map TaskKey Integer -> M.Map TaskKey Integer
criticalWeights g sizes                 = weights
  where weights                         = M.fromList [ (k, M.findWithDefault 0 k sizes + maximum (0 : map (weights M.!) (usersOf g k))) | k <- nodes g ]

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
--   1) Construct a dependency graph over non-builtin tasks and reject import cycles.
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
--   - Each module is compiled with optionsFor: dependency projects skip their
--     tests and final binary while the root keeps the user's options.
compileTasks :: Source.SourceProvider
             -> C.GlobalOptions
             -> ModuleCache
             -> CompileContext
             -> [GlobalTask]
             -> Data.Set.Set TaskKey
             -> CompileCallbacks
             -> IO (Either CompileFailure (Acton.Env.Env0, Bool))
compileTasks sp gopts cache ctx tasks dbpBlocked callbacks = do
    runningRef <- newIORef []
    frontOutputRef <- newIORef []
    let cancelRunning = readIORef runningRef >>= mapM_ cancel
        waitFrontOutputsOnExit =
          void (trySync (waitFrontOutputJobsRef frontOutputRef))
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
          if not (null importCycles)
            then return $ Left (ImportCycles importCycles)
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
      baseEnv <- Acton.Env.initEnvWithEscapeSummaries True builtinPath False

      costMap <- fmap M.fromList $ forM nonBuiltinTasks $ \t -> do
                    let fp = taskSourceFile t
                    ok <- doesFileExist fp
                    sz <- if ok then getFileSize fp else return 0
                    return (gtKey t, sz)
      let cwMap = criticalWeights modules costMap

      maxParallel <- compileParallelism gopts

      loop frontOutputRef runningRef stageInitialReady [] M.empty M.empty M.empty M.empty M.empty Data.Set.empty M.empty stageIndeg stagePending0 baseEnv False maxParallel cwMap

    -- Basic maps/sets ----------------------------------------------------
    taskMap = M.fromList [ (gtKey t, t) | t <- tasks ]
    isDbpBlocked k = Data.Set.member k dbpBlocked
    builtinOrder = [ t | t <- tasks, isBuiltin (gtKey t) ]
    nonBuiltinTasks = [ t | t <- tasks, not (isBuiltin (gtKey t)) ]
    modules = importGraph nonBuiltinTasks
    importCycles = cycles modules

    -- A front stage runs after its providers' front stages and, when its
    -- source is parsed separately, after its own parse stage.
    stages :: Graph StageKey
    stages = graph $
         [ (ParseStage k, [])                                                                     | t <- nonBuiltinTasks, let k = gtKey t, hasParseStage ctx t ]
      ++ [ (FrontStage k, [ ParseStage k | hasParseStage ctx t ] ++ map FrontStage (depsOf modules k)) | t <- nonBuiltinTasks, let k = gtKey t ]

    stageIndeg :: M.Map StageKey Int
    stageIndeg = M.fromList [ (s, length (depsOf stages s)) | s <- nodes stages ]

    stageInitialReady :: [StageKey]
    stageInitialReady = [ k | (k, d) <- M.toList stageIndeg, d == 0 ]

    stagePending0 :: Data.Set.Set StageKey
    stagePending0 = Data.Set.fromList (M.keys stageIndeg)

    rootAlt = modName (ccPathsRoot ctx)

    builtinPath =
      case builtinOrder of
        (t:_) -> projTypes (gtPaths t)
        _     -> sysTypes (ccPathsRoot ctx)

    flushReadyDeferredBacks :: Acton.Env.Env0
                            -> InterestMap
                            -> Data.Set.Set TaskKey
                            -> M.Map TaskKey (DeferredBackJob, Data.Set.Set TaskKey)
                            -> IO (Either CompileFailure (M.Map TaskKey (DeferredBackJob, Data.Set.Set TaskKey)))
    flushReadyDeferredBacks envAcc interestMap frontDone deferredBacks = do
      let (ready, waiting) =
            M.partitionWithKey
              (\_ (_deferred, waitSet) -> waitSet `Data.Set.isSubsetOf` frontDone)
              deferredBacks
      prepared <- trySync $
        forM (M.elems ready) $ \(deferred, _waitSet) -> do
          mJob <- prepareDeferredBackJob sp gopts callbacks envAcc interestMap deferred
          case mJob of
            Nothing -> ccOnBackSkipped callbacks (deferredKey deferred)
            Just _ -> return ()
          return mJob
      case prepared of
        Left err -> return (Left (DeferredBackFailed (displayException err)))
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
          mn = tkMod (gtKey t)
          optsBuiltin = optionsFor ctx (gtKey t)
          actFile = taskSourceFile t
          forceAlt = C.altOutput optsBuiltin && mn == rootAlt
      case gtTask t of
        _ | C.only_build optsBuiltin -> return (Right ())
        TyTask _ | not forceAlt -> return (Right ())
        task -> do
          parsed <- materialize sp optsBuiltin mn actFile Nothing task
          case parsed of
            Left diags -> do
              ccOnDiagnostics callbacks t optsBuiltin diags
              return (Left BuiltinFailed)
            Right (source, m) -> do
              ccOnFrontStart callbacks t optsBuiltin
              builtinEnv0 <- Acton.Env.initEnv (projTypes bPaths) True
              res <- runFrontPasses
                gopts
                optsBuiltin
                False
                bPaths
                builtinEnv0
                m
                (sourceText source)
                (sourceBytes source)
                (sourceMeta source)
                (lookupModuleHash cache (searchPath bPaths) Pub)
                (lookupModuleHash cache (searchPath bPaths) Impl)
                (lookupNameHash cache (searchPath bPaths))
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
                  return (Left BuiltinFailed)
                Right fr -> do
                  outputFailure <- waitFrontOutputJobsRef frontOutputRef
                  case outputFailure of
                    Just err -> return (Left err)
                    Nothing -> do
                      ccOnFrontDone callbacks t optsBuiltin
                      ccOnFrontResult callbacks t optsBuiltin fr
                      rememberFront cache mn fr
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
          mn    = tkMod key
          optsT = optionsFor ctx key
          providers = gtImportProviders t
          actFile = taskSourceFile t
          tyFile = tyDbPath paths mn
          mkFrontResult imps ifaceTE mdoc pubHash implHash nameHashes interestNameHashes backJob deferredBackJob =
            FrontResult
              { frIfaceTE = ifaceTE
              , frImps = imps
              , frDoc = mdoc
              , frModuleInfo = Nothing
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
              , frModuleInfo = Nothing
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
            rememberFront cache mn fr
            return (key, Right fr)
          readTyFile = do
            tyRes <- InterfaceFiles.readFileMaybe tyFile
            case tyRes of
              Nothing -> return (Left (missingIfaceDiagnostics mn "" mn))
              Just ty -> return (Right ty)
          mkBackJob env1 tmod srcText moduleImplHash =
            BackJob
              { bjKey = key
              , bjPaths = paths
              , bjOpts = optsT
              , bjTypeEnv = Converter.convEnvProtos env1
              , bjModule = tmod
              , bjSource = srcText
              , bjImplHash = moduleImplHash
              }

          resolveImportHash m =
            case M.lookup m providers of
              Just depKey ->
                case M.lookup depKey pubMap of
                  Just h  -> return (Just h)
                  Nothing
                    | isBuiltin depKey -> lookupModuleHash cache (searchPath paths) Pub m
                    | M.member depKey taskMap -> error ("Internal error: missing pub hash for dep " ++ modNameToString m)
                    | otherwise -> lookupModuleHash cache (searchPath paths) Pub m
              Nothing -> lookupModuleHash cache (searchPath paths) Pub m
          resolveImportImplHash m =
            case M.lookup m providers of
              Just depKey ->
                case M.lookup depKey implMap of
                  Just h  -> return (Just h)
                  Nothing
                    | isBuiltin depKey -> lookupModuleHash cache (searchPath paths) Impl m
                    | M.member depKey taskMap -> error ("Internal error: missing impl hash for dep " ++ modNameToString m)
                    | otherwise -> lookupModuleHash cache (searchPath paths) Impl m
              Nothing -> lookupModuleHash cache (searchPath paths) Impl m
          resolveNameHash' m n =
            case M.lookup m providers of
              Just depKey ->
                case M.lookup depKey nameMap of
                  Just hm ->
                    case M.lookup n hm of
                      Just info -> return (Just info)
                      Nothing
                        | providerIsCachedTy depKey -> lookupNameHash cache (searchPath paths) m n
                        | otherwise -> return Nothing
                  Nothing
                    | isBuiltin depKey -> lookupNameHash cache (searchPath paths) m n
                    | M.member depKey taskMap -> error ("Internal error: missing name hashes for dep " ++ modNameToString m)
                    | otherwise -> lookupNameHash cache (searchPath paths) m n
              Nothing -> lookupNameHash cache (searchPath paths) m n
          providerIsCachedTy depKey =
            case M.lookup depKey taskMap of
              Just GlobalTask{ gtTask = TyTask _ } -> True
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
                isManagedTyPath path = any (path `isUnder`) managedTypeDirs
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
                         mTy <- lookupTydbPath cache (searchPath paths) depMn
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
                    names = map A.nstr (Data.List.sortOn A.nameText uniq)
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
            TyTask h -> interestDepsFromDepRows (InterfaceFiles.summaryDepModules h)
            _ -> return Data.Set.empty

          -- Restore BOTH the pub and the impl per-name external deps from the
          -- module-level dep rows (the per-name rows on disk are stripped, see
          -- InterfaceFiles.stripExternalDeps). The impl half matters: a header
          -- rewrite (impl-hash refresh) re-derives the dep rows from these name
          -- hashes, and losing the impl users would erase the per-name impl
          -- change tracking for every dependency of this module.
          restorePubDepsFromRows depModules nameHashes = do
            (pubByOwner, implByOwner) <- foldM addModule (M.empty, M.empty) depModules
            -- Canonical order: the hash combinators (computeHashesSortedDeps)
            -- require dep lists sorted by name, exactly as the front pass
            -- stores them -- row iteration order must not leak into hashes.
            return
              [ nh { InterfaceFiles.nhPubDeps = Data.List.sortOn fst (M.findWithDefault [] (InterfaceFiles.nhName nh) pubByOwner)
                   , InterfaceFiles.nhImplDeps = Data.List.sortOn fst (M.findWithDefault [] (InterfaceFiles.nhName nh) implByOwner)
                   }
              | nh <- nameHashes
              ]
            where
              addModule acc depInfo = do
                let depMn = InterfaceFiles.dmiModule depInfo
                depNames <- InterfaceFiles.readDepNames tyFile depMn
                foldM (addName depMn) acc depNames

              addName depMn (pubAcc, implAcc) depInfo = do
                users <- InterfaceFiles.readDepUsers tyFile depMn (InterfaceFiles.dniName depInfo)
                let mkDep h =
                      ( A.GName depMn (InterfaceFiles.dniName depInfo)
                      , h
                      )
                    addUser dep m user =
                      M.insertWith (++) user [dep] m
                    pubAcc'
                      | B.null (InterfaceFiles.dniPubHash depInfo) = pubAcc
                      | otherwise = foldl' (addUser (mkDep (InterfaceFiles.dniPubHash depInfo))) pubAcc (InterfaceFiles.duPubUsers users)
                    implAcc'
                      | B.null (InterfaceFiles.dniImplHash depInfo) = implAcc
                      | otherwise = foldl' (addUser (mkDep (InterfaceFiles.dniImplHash depInfo))) implAcc (InterfaceFiles.duImplUsers users)
                return (pubAcc', implAcc')

          depModulesFromNameHashes nameHashes = do
            let depMods =
                  Data.List.sortOn modNameToString $
                  Data.Set.toList $
                  Data.Set.fromList
                    [ depMn
                    | nh <- nameHashes
                    , (qn, _) <- InterfaceFiles.nhPubDeps nh ++ InterfaceFiles.nhImplDeps nh
                    , Just (depMn, _) <- [qnameTarget qn]
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
                  | otherwise = Just (label ++ " " ++ shortHash (getHash old) ++ " -> " ++ shortHash (getHash new))
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
                addedItems = [ "+" ++ A.nstr n | n <- Data.List.sortOn A.nameText added ]
                removedItems = [ "-" ++ A.nstr n | n <- Data.List.sortOn A.nameText removed ]
                changedItems = mapMaybe changesFor (Data.List.sortOn A.nameText shared)
                items = addedItems ++ removedItems ++ changedItems
            in if null items then Nothing else Just (intercalate ", " items)

          resolveNameHashInfo m n = do
            mInfo <- resolveNameHash' m n
            return $ case mInfo of
              Just info -> Right info
              Nothing -> Left (missingNameHashDiagnostics (A.GName m n))

          resolveQNameHash label getHash users qn =
            case qnameTarget qn of
              Just (m, n) -> resolveNameHashInfo m n >>= \res ->
                return $ case res of
                  Left diags -> Left diags
                  Right info ->
                    let h = getHash info
                    in if B.null h
                         then Left (missingDepHashDiagnostics label (A.GName m n) users)
                         else Right h
              Nothing -> return (Left (missingNameHashDiagnostics qn))

          resolveDepHashes label getHash deps = do
            resolved <- traverseDiags (\(n, qns) -> do
              let users = " (used by " ++ A.nstr n ++ ")"
              resolvedQns <- traverseDiags (\qn -> do
                currE <- resolveQNameHash label getHash users qn
                return (fmap (\curr -> (qn, curr)) currE)) qns
              return (fmap (\vals -> (n, vals)) resolvedQns)) (M.toList deps)
            return (fmap M.fromList resolved)

          checkDepModuleRows depModules = do
            rows <- mapM checkOne depModules
            return (foldRows rows)
            where
              checkOne depInfo = do
                let depMn = InterfaceFiles.dmiModule depInfo
                mpub <- resolveImportHash depMn
                mimpl <- resolveImportImplHash depMn
                return $ case (mpub, mimpl) of
                  (Just pubH, Just implH) ->
                    ( depMn
                    , pubH /= InterfaceFiles.dmiPubHash depInfo
                    , implH /= InterfaceFiles.dmiImplHash depInfo
                    )
                  _ ->
                    -- Stale dep rows may mention a removed transitive provider;
                    -- the per-name rows decide whether any used name is affected.
                    (depMn, True, True)

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

              noteMissingRows depMn (pubDeltas, implDeltas, pubMissing, implMissing, pubUsers, implUsers, rowMissingMods) =
                (pubDeltas, implDeltas, pubMissing, implMissing, pubUsers, implUsers, Data.Set.insert depMn rowMissingMods)

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
        ParseErrorTask diags -> return (key, Left diags)
        _ | C.only_build optsT -> do
              ifaceRes <- case taskCurrent of
                            TyTask h -> readIfaceFromTy paths mn "" (Just (InterfaceFiles.summaryPubHash h))
                            ParseTask source _ _ -> readIfaceFromTy paths mn (sourceText source) Nothing
                            ActonTask source _ -> readIfaceFromTy paths mn (sourceText source) Nothing
              case ifaceRes of
                Right (imps, ifaceTE, mdoc, ih, implH, mModuleInfo) -> do
                  interestDeps <- cachedInterestDeps
                  let fr = (mkFrontResult imps ifaceTE mdoc ih implH [] [] Nothing Nothing)
                             { frModuleInfo = mModuleInfo
                             , frInterestDeps = interestDeps
                             }
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
            TyTask h -> do
              importHashRes <- checkImportHashes (InterfaceFiles.summaryImports h)
              case importHashRes of
                Left diags -> return (Left diags)
                Right _ -> do
                  (changedPubMods, changedImplMods) <- checkDepModuleRows (InterfaceFiles.summaryDepModules h)
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
                  forceAlt    = C.altOutput optsT && mn == rootAlt
                  forceAlways = C.alwaysbuild optsT
                  -- Front passes run on source or API changes, or when forced.
                  needFront = needBySource || needByPub || needByMissing || forceAlt || forceAlways
                  mModuleImplHash = case taskCurrent of
                    TyTask h -> Just (InterfaceFiles.summaryImplHash h)
                    _ -> Nothing
              cachedDeferredBackJob <- case taskCurrent of
                    TyTask h -> do
                      hasNotImpl <- InterfaceFiles.readStmtHasNotImpl tyFile
                      return (dbpDeferredBackJob (isDbpBlocked key) hasNotImpl optsT paths key
                                                 (InterfaceFiles.summaryImplHash h) (InterfaceFiles.summaryNameCount h))
                    _ -> return Nothing
              let isCachedDbp =
                    case cachedDeferredBackJob of
                      Just _ -> True
                      Nothing -> False
              let canCheckCodegen = not needFront && not needByImpl && not (C.altOutput optsT) && not isCachedDbp
              mCodegenStatus <- case mModuleImplHash of
                Just implHash | canCheckCodegen -> Just <$> codegenStatus paths mn implHash
                _ -> return Nothing
              let needByCodegen = maybe False (not . codegenUpToDate) mCodegenStatus
              let runFront = do
                    prevNameHashes <- if C.verbose gopts
                      then lookupNameMap cache (searchPath paths) mn
                      else return Nothing
                    when (C.verbose gopts) $ do
                      if needBySource
                        then ccOnInfo callbacks ("  Stale " ++ modNameToString (dropProjPrefix paths mn) ++ ": source changed")
                        else do
                          when needByPub $ do
                            let fmtDelta (qn, old, new) = prstr qn ++ " " ++ shortHash old ++ " → " ++ shortHash new ++ fmtUsers pubUsers qn
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
                    parsed <- materialize sp optsT mn actFile Nothing taskCurrent
                    case parsed of
                      Left diags -> return (key, Left diags)
                      Right (source, m) -> do
                        res <- runFrontPasses
                          gopts
                          optsT
                          (isDbpBlocked key)
                          paths
                          envSnap
                          (adjustImports providers m)
                          (sourceText source)
                          (sourceBytes source)
                          (sourceMeta source)
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
                  -- Only the attempt falls back to the front passes, and only
                  -- for a synchronous exception or an unresolved hash.  The
                  -- write comes after every read, so a fallback never runs on
                  -- top of a committed refresh.  A failed write and a missing
                  -- own interface fail the module instead.
                  runImplRefresh = do
                    when (C.verbose gopts) $ do
                      let fmtDelta (qn, old, new) = prstr qn ++ " " ++ shortHash old ++ " → " ++ shortHash new ++ fmtUsers implUsers qn
                      ccOnInfo callbacks ("  Stale " ++ modNameToString (dropProjPrefix paths mn) ++ ": impl changes in " ++ Data.List.intercalate ", " (map fmtDelta implDeltas))
                    attempt <- trySync recomputeImpl
                    case attempt of
                      Right (Recomputed updatedDepModules updatedNameHashes fr) -> do
                        -- Synchronous, like the full write: completion implies
                        -- the refreshed rows are committed.  A write failure is
                        -- this module's diagnostics, returned and not thrown.
                        written <- trySync (runFrontOutputSync (ccOnFrontOutputStart callbacks)
                                                               (ccOnFrontOutputDone callbacks)
                                                               (ccShouldWriteFrontOutput callbacks)
                                                               outputKey
                                                               FrontOutputTydb $
                                              InterfaceFiles.updateImplRefresh tyFile (frImplHash fr) updatedDepModules updatedNameHashes)
                        case written of
                          Left err -> return (key, Left (tydbWriteDiagnostics mn err))
                          Right () -> mask_ $ do
                            outputJobs <- if C.tydb optsT
                                            then (:[]) <$> startFrontOutputJob (ccOnFrontOutputStart callbacks)
                                                                               (ccOnFrontOutputDone callbacks)
                                                                               (ccShouldWriteFrontOutput callbacks)
                                                                               outputKey
                                                                               FrontOutputTydbCopy
                                                                               (copyTydbInterface optsT paths mn)
                                            else return []
                            rememberFrontOutputJobList frontOutputRef outputJobs
                            cacheFrontResult fr{ frOutputJobs = outputJobs }
                      Right InterfaceGone -> return (key, Left (missingIfaceDiagnostics mn "" mn))
                      _ -> do
                        when (C.verbose gopts) $
                          ccOnInfo callbacks ("  Stale " ++ modNameToString (dropProjPrefix paths mn) ++ ": impl refresh encountered unresolved dep hashes; rerunning front passes")
                        runFront
                    where outputKey = TaskKey (projPath paths) mn
                  -- The refresh is pure row math: every input is already stored.
                  -- Own impl-hash components (nhOwnImplHash) and local deps come
                  -- from the per-name rows, the external dep NAMES from the dep
                  -- index rows (only their hashes changed -- the module's source
                  -- is unchanged), and the new dep hashes are resolved fresh.
                  -- No re-read of the typed module, no re-parse, no dep re-walk;
                  -- the .tydb is updated surgically. The typed module is read
                  -- only when the refresh must feed an EAGER back job -- then it
                  -- is the compilation input.  Every read, the escape summaries
                  -- included, and every forcing happen here, before the write.
                  recomputeImpl =
                    case taskCurrent of
                      TyTask h -> do
                        let depModules    = InterfaceFiles.summaryDepModules h
                            storedPubHash = InterfaceFiles.summaryPubHash h
                        -- The task header carries only the name COUNT (selective
                        -- reads); the refresh needs the module's own hash rows.
                        storedNameHashes <- InterfaceFiles.readNameHashes tyFile
                        moduleHasNotImpl <- InterfaceFiles.readStmtHasNotImpl tyFile
                        restored <- restorePubDepsFromRows depModules storedNameHashes
                        -- Only names with an impl item participate in the refresh --
                        -- exactly the names the front pass gave a (non-empty) own
                        -- impl-hash component. A signature-only name's impl hash is
                        -- invariant under dependency changes.
                        let refreshables =
                              [ nh | nh <- restored
                                   , not (B.null (InterfaceFiles.nhOwnImplHash nh)) ]
                            ownImplHashes = M.fromList
                              [ (InterfaceFiles.nhName nh, InterfaceFiles.nhOwnImplHash nh) | nh <- refreshables ]
                            implLocalDeps = M.fromList
                              [ (InterfaceFiles.nhName nh, InterfaceFiles.nhImplLocalDeps nh) | nh <- refreshables ]
                            implExtDeps = M.fromList
                              [ (InterfaceFiles.nhName nh, map fst (InterfaceFiles.nhImplDeps nh)) | nh <- refreshables ]
                        implExtRes <- resolveDepHashes "impl" InterfaceFiles.nhImplHash implExtDeps
                        case implExtRes of
                          Left _ -> return Unresolved
                          Right implExtHashes -> do
                            let updatedNameHashes =
                                  Hashing.refreshImplHashes restored ownImplHashes implLocalDeps implExtHashes
                                moduleImplHash = Hashing.moduleImplHashFromNameHashes updatedNameHashes
                            depModulesRes <- depModulesFromNameHashes updatedNameHashes
                            case depModulesRes of
                              Left _ -> return Unresolved
                              Right updatedDepModules -> do
                                evaluate (rnf (moduleImplHash, updatedDepModules, updatedNameHashes))
                                let deferredBackJob = dbpDeferredBackJob (isDbpBlocked key) moduleHasNotImpl optsT paths key moduleImplHash (length updatedNameHashes)
                                case deferredBackJob of
                                  Just _ -> do
                                    ifaceRes <- readIfaceFromTy paths mn "" (Just storedPubHash)
                                    return $ case ifaceRes of
                                      Left _ -> InterfaceGone
                                      Right (imps, ifaceTE, mdoc, pubHash, _storedImplHash, mModuleInfo) ->
                                        let fr = (mkFrontResult imps ifaceTE mdoc pubHash moduleImplHash (publicNameHashes updatedNameHashes) updatedNameHashes Nothing deferredBackJob)
                                                   { frModuleInfo = mModuleInfo }
                                        in Recomputed updatedDepModules updatedNameHashes fr
                                  Nothing -> do
                                    storedRes <- InterfaceFiles.readFileMaybe tyFile
                                    case storedRes of
                                      Nothing -> return InterfaceGone
                                      Just stored -> do
                                        let nmod          = InterfaceFiles.storedInterface stored
                                            tmod          = InterfaceFiles.storedTyped stored
                                            mdoc          = InterfaceFiles.storedDoc stored
                                            modulePubHash = InterfaceFiles.storedPubHash stored
                                        snap <- Source.readSource sp actFile
                                        env1 <- Acton.Env.mkEnv (searchPath paths) envSnap tmod
                                        evaluate (rnf nmod)
                                        evaluate (rnf tmod)
                                        let I.NModule imps ifaceFull _mdoc = nmod
                                            ifaceTE = publicIfaceTE ifaceFull
                                            backJob = Just (mkBackJob env1 tmod (Source.ssText snap) moduleImplHash)
                                        moduleInfo <- storedEscapeModuleInfo tyFile mn imps ifaceTE mdoc
                                        let fr = (mkFrontResult imps ifaceTE mdoc modulePubHash moduleImplHash (publicNameHashes updatedNameHashes) updatedNameHashes backJob Nothing)
                                                   { frModuleInfo = Just moduleInfo }
                                        return (Recomputed updatedDepModules updatedNameHashes fr)
                      _ -> return Unresolved
                  runCodegenRefresh = do
                    when (C.verbose gopts) $ do
                      let suffix = maybe "" formatCodegenDelta mCodegenStatus
                      ccOnInfo callbacks ("  Stale " ++ modNameToString (dropProjPrefix paths mn) ++ ": generated code out of date" ++ suffix)
                    tyRes <- readTyFile
                    case tyRes of
                      Left diags -> return (key, Left diags)
                      Right stored -> do
                        let nmod                 = InterfaceFiles.storedInterface stored
                            tmod                 = InterfaceFiles.storedTyped stored
                            mdoc                 = InterfaceFiles.storedDoc stored
                            modulePubHash        = InterfaceFiles.storedPubHash stored
                            moduleImplHashStored = InterfaceFiles.storedImplHash stored
                            nameHashes           = InterfaceFiles.storedNames stored
                        snap <- Source.readSource sp actFile
                        env1 <- Acton.Env.mkEnv (searchPath paths) envSnap tmod
                        interestDeps <- cachedInterestDeps
                        let I.NModule imps ifaceFull _mdoc = nmod
                            ifaceTE = publicIfaceTE ifaceFull
                            deferredBackJob = dbpDeferredBackJob (isDbpBlocked key) (A.hasNotImpl (A.mbody tmod)) optsT paths key moduleImplHashStored (length nameHashes)
                            backJob =
                              case deferredBackJob of
                                Just _ -> Nothing
                                Nothing -> Just (mkBackJob env1 tmod (Source.ssText snap) moduleImplHashStored)
                        moduleInfo <- storedEscapeModuleInfo tyFile mn imps ifaceTE mdoc
                        let fr = (mkFrontResult imps ifaceTE mdoc modulePubHash moduleImplHashStored (publicNameHashes nameHashes) nameHashes backJob deferredBackJob)
                                   { frInterestDeps = interestDeps
                                   , frModuleInfo = Just moduleInfo
                                   }
                        cacheFrontResult fr
                  runReuse = do
                    when (C.verbose gopts) $
                      ccOnInfo callbacks ("  Fresh " ++ modNameToString (dropProjPrefix paths mn) ++ ": using cached .tydb")
                    ifaceRes <- case taskCurrent of
                                  TyTask h -> readIfaceFromTy paths mn "" (Just (InterfaceFiles.summaryPubHash h))
                                  _ -> readIfaceFromTy paths mn "" Nothing
                    case ifaceRes of
                      Left diags -> return (key, Left diags)
                      Right (imps, ifaceTE, mdoc, ih, implH, mModuleInfo) -> do
                        interestDeps <- cachedInterestDeps
                        let fr = (mkFrontResult imps ifaceTE mdoc ih implH [] [] Nothing cachedDeferredBackJob)
                                   { frModuleInfo = mModuleInfo
                                   , frInterestDeps = interestDeps
                                   }
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
      let optsT = optionsFor ctx key
          mn = tkMod key
          actFile = taskSourceFile t
          onProgress p = ccOnParseProgress callbacks t optsT p
      timeStart <- getTime Monotonic
      -- A parse stage exists only for a ParseTask.  A parse error becomes a
      -- ParseErrorTask, which the module's front stage reports.
      parsed <- materialize sp optsT mn actFile (Just onProgress) (gtTask t)
      case parsed of
        Right (source, m) -> do
          _ <- evaluate (rnf m)
          timeEnd <- getTime Monotonic
          let parseTime = if not (quiet gopts optsT) then Just (timeEnd - timeStart) else Nothing
          return (ParseStage key, Right (StageParsed (ActonTask source m) parseTime))
        Left diags -> return (ParseStage key, Right (StageParsed (ParseErrorTask diags) Nothing))

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
                      Just t -> ccOnFrontStart callbacks t (optionsFor ctx key)
                      Nothing -> return ()
                  ParseStage key ->
                    case M.lookup key taskMap of
                      Just t -> ccOnParseStart callbacks t (optionsFor ctx key)
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
              optsDone = optionsFor ctx keyDone
          case outcome of
            -- Only a front stage fails: a parse stage reports a parse error as
            -- a ParseErrorTask, which its front stage turns into diagnostics.
            Left diags -> do
              ccOnFrontDone callbacks tDone optsDone
              ccOnDiagnostics callbacks tDone optsDone diags
              let blockedMods = strictDependents modules keyDone
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
                  ind2  = foldl' (\m d -> M.adjust (\x -> x-1) d m) ind (usersOf stages stageDone)
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
                      envAcc' = Acton.Env.addModuleInfo (tkMod keyDone) (frontResultModuleInfo (tkMod keyDone) fr) envAcc
                      interestMap2 = addInterestDeps (frInterestDeps fr) interestMap
                      frontDone2 = Data.Set.insert keyDone frontDone
                      deferredBacks1 =
                        case frDeferredBackJob fr of
                          Nothing -> deferredBacks
                          Just deferred -> M.insert keyDone (deferred, strictDependents modules keyDone) deferredBacks
                  flushRes <- flushReadyDeferredBacks envAcc' interestMap2 frontDone2 deferredBacks1
                  case flushRes of
                    Left err -> return (Left err)
                    Right deferredBacks2 ->
                      loop frontOutputRef runningRef rdy2 running2 res2 implRes2 nameRes2 parsedTasks2 interestMap2 frontDone2 deferredBacks2 ind2 pend2 envAcc' hadErrors maxPar cw


-- | Check whether a NameInfo represents a root-eligible actor.
-- Used to decide which roots to include in .tydb headers and root generation.
rootEligible :: I.NameInfo -> Bool
rootEligible (I.NAct [] p k _ _) = case (p,k) of
                                      (A.TNil{}, A.TRow _ _ _ t _ A.TNil{}) ->
                                        prstr t == "Env" || prstr t == "None" ||
                                        prstr t == "__builtin__.Env" || prstr t == "__builtin__.None"
                                      _ -> False
rootEligible _ = False
