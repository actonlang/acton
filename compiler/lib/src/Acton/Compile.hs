{-# LANGUAGE CPP, FlexibleInstances, ScopedTypeVariables #-}
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
module focuses on deterministic compilation and reports what it does as
BuildEvent values.

Call flow:
  - Shared pipeline (acton build, acton watch, LSP):
    1) prepareCompilePlan (via prepareCompileContext, readModuleTask/
       buildGlobalTasks, and selectNeededTasks/selectAffectedTasks) builds a
       CompilePlan for the requested subgraph. Parsing is performed through
       parseActSource/parseActFile so SourceProvider overlays can supply
       unsaved buffers.
    2) runCompilePlan drives compileModules. As each module finishes its front
       passes, its BackReady event enqueues its BackJob into the scheduler's
       BackQueue, so back passes begin as soon as they are ready and can overlap
       remaining front passes.
    3) Callers either wait for waitBackJobs (CLI builds) or let back jobs run
       in the background. CLI builds wait before invoking Zig.
    4) runBackPasses can be used for standalone back-pass execution when a
       caller already has a BackJob to run.
  - Triggering:
    - acton build runs the pipeline once for the requested files/project.
    - acton watch and LSP call startCompile on each event. runCompilePlan
      hands an event to the caller's handler only if the run's generation is
      current when the event is reported. BackQueue ignores back jobs for
      stale generations, and front-output jobs are skipped for stale
      generations before writing.
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
    (LSP uses debounceMicros, acton watch uses 0). Back jobs, front-output
    writes and the events a handler sees are filtered by generation. Effects
    a caller performs outside its event handler are gated by the caller.
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
  , DeferredBackJob
  , BackPlan(..)
  , FrontResult(..)
  , ModuleHashes(..)
  , FrontOutputKind(..)
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
  , BuildEvent(..)
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
  , runCompilePlan
  , runStandalonePlan
  , BuildEnv
  , standaloneBuild
  , checkModule
  , awaitOutputs
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
  , docNameCountThreshold
  , shouldGenerateDocOutput
  , runBackPasses
  , importsOf
  , taskSourceFile
  , hasParseStage
  , quiet
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
import Control.DeepSeq (NFData, force, rnf)
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


-- | Why a run could not compile its modules.
data CompileFailure
  = ImportCycles [[TaskKey]]                      -- the modules of each import cycle
  | BuiltinFailed                                 -- __builtin__ had errors, already reported as diagnostics
  | OutputFailed TaskKey FrontOutputKind String   -- the first failed output in awaitOutputs order, with its exception text
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

-- | What a compile run reports.  A module whose source is parsed in a stage
-- of its own reports ParseStarted, ParseProgressed and ParseFinished.  Each
-- module then reports FrontStarted, FrontProgressed and FrontFinished,
-- followed by Diagnostics when it failed or BackReady when its back job is
-- eager.  A deferred back job reports BackReady or BackSkipped once every
-- module depending on it finished its front stage.  A module depending on a
-- failed module reports nothing after that failure.  __builtin__ reports
-- nothing when its cached interface is used, and a parse error of it is a
-- Diagnostics event alone.  Progress, Output, Info and the events of a
-- running back job arrive on any thread.
data BuildEvent
  = ParseStarted     GlobalTask
  | ParseProgressed  GlobalTask ParseProgress
  | ParseFinished    GlobalTask (Maybe TimeSpec)                -- Nothing when quiet or the parse failed
  | FrontStarted     GlobalTask
  | FrontProgressed  GlobalTask FrontPassProgress
  | FrontFinished    GlobalTask (Maybe FrontResult)             -- the result when the module succeeded
  | Diagnostics      GlobalTask [Diagnostic String]
  | OutputStarted    TaskKey FrontOutputKind
  | OutputProgressed TaskKey FrontOutputKind InterfaceFiles.TyDbWriteProgress
  | OutputFinished   TaskKey FrontOutputKind (Maybe TimeSpec)   -- Nothing when the write failed
  | BackReady        BackJob                                    -- routed to the back queue by runCompilePlan
  | BackSkipped      TaskKey                                    -- a deferred module whose generated code is current
  | BackStarted      BackJob
  | BackProgressed   BackJob BackPassProgress
  | BackFinished     BackJob BackJobResult
  | Info             String                                     -- a --verbose line; tests match its text

-- | One compile run's services.  Read-only.
data BuildEnv = BuildEnv
  { buildSources    :: Source.SourceProvider
  , buildOptions    :: C.GlobalOptions
  , buildGeneration :: Generation                       -- front outputs are written only while it is current
  , emit            :: BuildEvent -> IO ()
  , buildCache      :: ModuleCache
  , buildOutputs    :: OutputJobs                       -- the background outputs started so far
  }

-- | Report a line that only --verbose shows.
info                                    :: BuildEnv -> String -> IO ()
info build line                         = when (C.verbose (buildOptions build)) (emit build (Info line))

checkBuildSpecChange :: CompileScheduler -> BuildSpecStamp -> IO Bool
checkBuildSpecChange sched stamp =
  atomicModifyIORef' (schedulerBuildSpec sched) $ \prev ->
    let changed = prev /= Just stamp
    in (Just stamp, changed)

-- | A compile run's place in its scheduler's sequence of runs. A run is
-- current until a newer one starts. A stale run writes no front output,
-- queues no back job and reports no event to runCompilePlan's handler, and
-- its queued back jobs neither write nor report an end.
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

-- | A back job with its generation and the sink its events go to.
data QueuedJob = QueuedJob Generation (BuildEvent -> IO ()) BackJob

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
-- which waits for its background outputs, so stale writes never overlap the
-- new run. After the delay the run starts only if no newer generation began.
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
enqueueBack :: BackQueue -> Generation -> (BuildEvent -> IO ()) -> BackJob -> IO ()
enqueueBack queue gen events job = whenCurrent gen $ atomically $ do
  modifyTVar' (pendingCounts queue) (M.insertWith (+) (generationNumber gen) 1)
  writeTChan (queuedJobs queue) (QueuedJob gen events job)

-- | Run queued back jobs, one at a time. A job runs only while its
-- generation is current and has no failure yet. The write gate is asked
-- right before the generated files are written. The end is reported, and a
-- failure recorded, only while the generation is current, so a job that
-- goes stale while it runs reports its start but no end. The first failure
-- of a generation is kept. Progress is reported inside the try, so an
-- exception from reporting progress fails the job. The start and the end
-- are reported outside it, so an exception from reporting them ends the
-- worker. The pending count falls after every job, whether it ran or was
-- dropped.
backWorker :: C.GlobalOptions -> BackQueue -> IO ()
backWorker gopts queue = forever $ do
  QueuedJob gen events job <- atomically (readTChan (queuedJobs queue))
  current <- isCurrent gen
  failed <- M.member (generationNumber gen) <$> readTVarIO (firstFailures queue)
  when (current && not failed) $ do
    events (BackStarted job)
    result <- try (runBackPasses gopts job (isCurrent gen) (events . BackProgressed job))
                :: IO (Either SomeException (Maybe TimeSpec, Maybe BackTiming))
    whenCurrent gen $ case result of
      Left err -> do
        let failure = BackPassFailure (bjKey job) (displayException err)
        atomically (modifyTVar' (firstFailures queue) (M.insertWith (\_ first -> first) (generationNumber gen) failure))
        events (BackFinished job (BackJobFailed failure))
      Right (time, timing) -> events (BackFinished job (BackJobOk time timing))
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

-- | Compile a plan as a generation of the scheduler. Its back jobs run on
-- the scheduler's workers: wait for them with waitBackJobs. Each event is
-- recorded for analytics first, whatever the generation. A BackReady event
-- then queues its job, which enqueueBack refuses for a stale generation.
-- The handler sees the event last, and only if the generation is current
-- at that moment, so a handler needs no generation check of its own.
runCompilePlan :: Source.SourceProvider
               -> C.GlobalOptions
               -> CompileScheduler
               -> Generation
               -> CompilePlan
               -> (BuildEvent -> IO ())
               -> IO (Either CompileFailure (Acton.Env.Env0, Bool))
runCompilePlan sp gopts sched gen plan onEvent = withAnalytics $ \analytics -> do
  let events event = do
        recordAnalytics analytics event
        case event of
          BackReady job -> enqueueBack (schedulerBackQueue sched) gen events job
          _             -> return ()
        whenCurrent gen (onEvent event)
  outputs <- OutputJobs <$> newIORef []
  compileModules (BuildEnv sp gopts gen events moduleCache outputs) plan

-- | The services of a run outside any scheduler: a generation that stays
-- current, the process-wide module cache, no back queue, so BackReady
-- reaches only the handler, and a record of its own background outputs,
-- which awaitOutputs waits for.
standaloneBuild :: Source.SourceProvider -> C.GlobalOptions -> (BuildEvent -> IO ()) -> IO BuildEnv
standaloneBuild sp gopts onEvent = do
  counter <- newIORef 0
  outputs <- OutputJobs <$> newIORef []
  return (BuildEnv sp gopts (Generation 0 counter) onEvent moduleCache outputs)

-- | Compile a plan outside any scheduler, as acton sig does. Analytics do
-- not sample it.
runStandalonePlan :: Source.SourceProvider
                  -> C.GlobalOptions
                  -> CompilePlan
                  -> (BuildEvent -> IO ())
                  -> IO (Either CompileFailure (Acton.Env.Env0, Bool))
runStandalonePlan sp gopts plan onEvent = do
  build <- standaloneBuild sp gopts onEvent
  compileModules build plan

-- | Feed the analytics sampler: finished parses, type-check progress and
-- finished back passes.
recordAnalytics                                 :: Analytics -> BuildEvent -> IO ()
recordAnalytics analytics (ParseFinished t _)   = recordParseDone analytics (modNameToString (tkMod (gtKey t)))
recordAnalytics analytics (FrontProgressed t p)
  | fppPass p == FrontPassTypes                 = recordType analytics (modNameToString (tkMod (gtKey t))) (fppCompleted p) (fppTotal p)
recordAnalytics analytics (BackProgressed job p)
  | BackPassFinished _ <- bppEvent p            = recordBackPass analytics (modNameToString (tkMod (bjKey job))) (backPassName (bppPass p))
recordAnalytics _ _                             = return ()

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

hashKindName                            :: HashKind -> String
hashKindName Pub                        = "pub"
hashKindName Impl                       = "impl"

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
    withTydb cache spath m (fmap (fmap (publicNameMap . names)) . InterfaceFiles.readHeaderMaybe)
  where
    names (_, _, _, _, _, _, nameHashes, _, _, _) = nameHashes

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
    modifyMVar_ (pubHashTable cache)  (return . M.insert m (mhPub hashes))
    modifyMVar_ (implHashTable cache) (return . M.insert m (mhImpl hashes))
    unless (M.null (mhNames hashes)) $
      modifyMVar_ (nameMapTable cache) (return . M.insert m (mhNames hashes))
  where hashes = frHashes fr

publicIfaceTE :: [(A.Name, I.NameInfo)] -> [(A.Name, I.NameInfo)]
publicIfaceTE = filter (Names.isPublicName . fst)

-- | The hashes of a module's public names, by name.
publicNameMap :: [InterfaceFiles.NameHashInfo] -> M.Map A.Name InterfaceFiles.NameHashInfo
publicNameMap names = M.fromList [ (InterfaceFiles.nhName i, i) | i <- names, Names.isPublicName (InterfaceFiles.nhName i) ]


-- Module environment and resolver ------------------------------------------------------------------

-- | One module's front stage, as of the moment it was scheduled.  It starts
-- only after its providers finished, so this snapshot holds everything it
-- reads.  Fields stay lazy: the environment is forced by the stage's thread,
-- never by the coordinator.
data ModuleEnv = ModuleEnv
  { moduleBuild    :: BuildEnv
  , moduleTask     :: GlobalTask
  , moduleCurrent  :: CompileTask                       -- the parse stage's result, else the discovered task
  , moduleOptions  :: C.CompileOptions                  -- root or dependency options
  , moduleResolver :: Resolver
  , checkedModules :: Acton.Env.Env0                    -- __builtin__ and every module checked before this one
  , moduleBlocked  :: Bool                              -- a library boundary: its back passes are not deferred
  , moduleForced   :: Bool                              -- alternative output for the requested module, or --always-build
  }

keyOf                                   :: ModuleEnv -> TaskKey
keyOf                                   = gtKey . moduleTask

nameOf                                  :: ModuleEnv -> A.ModName
nameOf                                  = tkMod . keyOf

pathsOf                                 :: ModuleEnv -> Paths
pathsOf                                 = gtPaths . moduleTask

-- | The module's own .tydb.
tydbOf                                  :: ModuleEnv -> FilePath
tydbOf env                              = tyDbPath (pathsOf env) (nameOf env)

-- | The module's name without its project prefix, as --verbose lines show it.
labelOf                                 :: ModuleEnv -> String
labelOf env                             = modNameToString (dropProjPrefix (pathsOf env) (nameOf env))

-- | What importers read of a finished module.  The results of the front
-- passes, of an impl refresh and of a codegen refresh carry its public
-- names.  A module reused from its .tydb without a refresh, or given the
-- empty result under --only-build, carries no name hashes of its own.
data ModuleHashes = ModuleHashes
  { mhPub   :: B.ByteString
  , mhImpl  :: B.ByteString
  , mhNames :: M.Map A.Name InterfaceFiles.NameHashInfo   -- public names
  }

moduleHash                              :: HashKind -> ModuleHashes -> B.ByteString
moduleHash Pub                          = mhPub
moduleHash Impl                         = mhImpl

-- | The current hashes of the modules a module imports.
data Resolver = Resolver
  { resolveModule :: HashKind -> A.ModName -> IO (Maybe B.ByteString)
  , resolveName   :: A.ModName -> A.Name -> IO (Maybe InterfaceFiles.NameHashInfo)
  }

-- | Where an import's hashes come from in a stage-graph run.
data Provider = Finished ModuleHashes | Outside | Unfinished

-- | Lookups for a module in a stage-graph run, given the hashes of the
-- modules whose front stage finished.  A provider in the run finished before
-- the module started, and one that did not is an internal error.
-- __builtin__, modules outside the run and imports without a provider are
-- read through the cache.  A provider discovered as a cached TyTask may
-- have been reused without its name hashes, so a name its hashes lack is
-- read from its .tydb.
inRunResolver :: ModuleCache -> M.Map TaskKey GlobalTask -> M.Map TaskKey ModuleHashes -> GlobalTask -> Resolver
inRunResolver cache tasks finished t = Resolver moduleHashOf nameHashOf
  where
    spath = searchPath (gtPaths t)
    moduleHashOf kind m = case provider m of
      Finished hashes -> return (Just (moduleHash kind hashes))
      Outside         -> lookupModuleHash cache spath kind m
      Unfinished      -> error ("Internal error: missing " ++ hashKindName kind ++ " hash for dep " ++ modNameToString m)
    nameHashOf m n = case provider m of
      Finished hashes
        | Just i <- M.lookup n (mhNames hashes) -> return (Just i)
        | discoveredAsCached m                   -> lookupNameHash cache spath m n
        | otherwise                              -> return Nothing
      Outside    -> lookupNameHash cache spath m n
      Unfinished -> error ("Internal error: missing name hashes for dep " ++ modNameToString m)
    provider m = case M.lookup m (gtImportProviders t) of
      Nothing -> Outside
      Just k | Just hashes <- M.lookup k finished -> Finished hashes
             | isBuiltin k || M.notMember k tasks  -> Outside
             | otherwise                           -> Unfinished
    discoveredAsCached m = case M.lookup m (gtImportProviders t) >>= (`M.lookup` tasks) of
      Just GlobalTask{ gtTask = TyTask _ } -> True
      _                                    -> False

-- | Lookups for a module outside a stage graph: every import is read
-- through the cache.
cacheResolver :: ModuleCache -> Paths -> Resolver
cacheResolver cache paths = Resolver (lookupModuleHash cache spath) (lookupNameHash cache spath)
  where spath = searchPath paths

-- | A dependency module row: the module's current pub and impl hash.  Both
-- are looked up.
dependencyModule :: Resolver -> A.ModName -> IO (Maybe InterfaceFiles.DepModuleInfo)
dependencyModule resolver m = do
    pub  <- resolveModule resolver Pub m
    impl <- resolveModule resolver Impl m
    return (InterfaceFiles.DepModuleInfo m <$> pub <*> impl)

nameHash                                :: HashKind -> InterfaceFiles.NameHashInfo -> B.ByteString
nameHash Pub                            = InterfaceFiles.nhPubHash
nameHash Impl                           = InterfaceFiles.nhImplHash

-- | The current hash of every external dependency of each owning name, in
-- their given order, owners without dependencies included.  Otherwise every
-- unavailable one, owner by owner: a name without hashes, or one without a
-- hash of this kind, located at the owning name.  Each name is resolved once
-- per call.
externalHashes :: Resolver -> HashKind -> M.Map A.Name SrcLoc -> M.Map A.Name [A.QName]
               -> IO (Either [ModuleError] (M.Map A.Name [(A.QName, B.ByteString)]))
externalHashes resolver kind locations deps = do
    lookupName <- memoized (uncurry (resolveName resolver))
    owners <- forM (M.toList deps) $ \(owner, qns) -> (,) owner <$> mapM (dependency lookupName owner) qns
    return $ case [ e | (_, results) <- owners, Left e <- results ] of
      []   -> Right (M.fromDistinctAscList [ (owner, [ d | Right d <- results ]) | (owner, results) <- owners ])
      errs -> Left errs
  where
    dependency lookupName owner qn = case qnameTarget qn of
      Nothing     -> return (Left (MissingNameHash qn))
      Just (m, n) -> do
        found <- lookupName (m, n)
        return $ case found of
          Nothing                           -> Left (MissingNameHash (A.GName m n))
          Just i | B.null (nameHash kind i) -> Left (MissingDependencyHash kind owner (M.findWithDefault NoLoc owner locations) (A.GName m n))
                 | otherwise                -> Right (A.GName m n, nameHash kind i)

-- | A function that remembers each answer it gave, for as long as it is
-- kept.  Not for concurrent use.
memoized :: Ord k => (k -> IO v) -> IO (k -> IO v)
memoized f = do
    known <- newIORef M.empty
    return $ \k -> do
      remembered <- M.lookup k <$> readIORef known
      case remembered of
        Just v  -> return v
        Nothing -> do v <- f k
                      modifyIORef' known (M.insert k v)
                      return v


-- Module errors ------------------------------------------------------------------------------------

-- | A module's own failures.  Thrown inside its stage and reported as its
-- diagnostics at the stage boundary.
data ModuleError
  = MissingInterface      A.ModName                         -- an import or dependency module without a readable .tydb
  | MissingNameHash       A.QName
  | MissingDependencyHash HashKind A.Name SrcLoc A.QName    -- the kind, the module's name using it, its location, the name
  | InterfaceWriteFailed  String                            -- the module's own .tydb
  deriving (Show)

newtype ModuleErrors = ModuleErrors [ModuleError] deriving (Show)

instance Exception ModuleErrors

-- | The diagnostic for a module error, shown with the given source text.  A
-- missing interface or a failed write names the module's full file, a hash
-- error its file without the project prefix.  A failed write shows no
-- source.
explainError :: ModuleEnv -> String -> ModuleError -> Diagnostic String
explainError env src err = case err of
    MissingInterface m                    -> compilationError fullFile  src NoLoc ("Type interface file not found or unreadable for " ++ modNameToString m)
    InterfaceWriteFailed e                -> compilationError fullFile  ""  NoLoc ("Failed to write .tydb for " ++ modNameToString (nameOf env) ++ ": " ++ e)
    MissingNameHash qn                    -> compilationError labelFile src NoLoc ("Hash info missing for " ++ prstr qn)
    MissingDependencyHash kind owner l qn -> compilationError labelFile src l
                                               (hashKindName kind ++ " hash missing for " ++ prstr qn ++ " (used by " ++ A.nstr owner ++ ")")
  where
    fullFile  = modNameToFilename (nameOf env)
    labelFile = modNameToFilename (dropProjPrefix (pathsOf env) (nameOf env))

compilationError                        :: FilePath -> String -> SrcLoc -> String -> Diagnostic String
compilationError                        = Diag.actErrToDiagnostic "Compilation error"

moduleFailure                           :: [ModuleError] -> IO a
moduleFailure                           = throwIO . ModuleErrors

failOnAny                               :: [ModuleError] -> IO ()
failOnAny errs                          = unless (null errs) (moduleFailure errs)

-- | Resolve every item, and fail the module with one error per item that
-- does not resolve.
resolveAll :: (a -> IO (Maybe b)) -> (a -> ModuleError) -> [a] -> IO [b]
resolveAll resolve problem items = do
    results <- forM items $ \x -> maybe (Left (problem x)) Right <$> resolve x
    failOnAny [ e | Left e <- results ]
    return [ b | Right b <- results ]

-- | The failures that the front passes report as the module's own
-- diagnostics, with its source text.  Every other exception propagates.
frontFailures :: ModuleEnv -> String -> [Handler (Either [Diagnostic String] a)]
frontFailures env src =
    [ Handler $ \(ModuleErrors errs)               -> failed (map (explainError env src) errs)
    , Handler $ \(e :: GeneralError)               -> failed (located (generalError e))
    , Handler $ \(e :: Acton.Env.CompilationError) -> failed (located (Acton.Env.compilationError e))
    , Handler $ \(Acton.Types.TypeErrors errs)     -> failed (map typed errs)
    , Handler $ \(e :: Acton.TypeEnv.TypeError)    -> failed [typed e] ]
  where
    failed       = return . Left
    file         = modNameToFilename (dropProjPrefix (pathsOf env) (nameOf env))
    located errs = [ compilationError file src l msg | (l, msg) <- errs ]
    typed e      = Acton.TypeEnv.mkErrorDiagnostic file src (Acton.TypeEnv.typeReport e file src)

-- | A module stage outside its front passes: only module errors are its
-- own, and they are shown without source text.
moduleBoundary :: ModuleEnv -> IO (Either [Diagnostic String] a) -> IO (Either [Diagnostic String] a)
moduleBoundary env run = run `catch` \(ModuleErrors errs) -> return (Left (map (explainError env "") errs))


-- Handling Acton files -----------------------------------------------------------------------------

-- | Turn a list of (location, message) pairs into diagnostics.
-- Used to normalize errors from different compiler subsystems.
errsToDiagnostics :: String -> FilePath -> String -> [(SrcLoc, String)] -> [Diagnostic String]
errsToDiagnostics errKind filename src errs =
    [ Diag.actErrToDiagnostic errKind filename src loc msg | (loc, msg) <- errs ]

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


-- Outputs ------------------------------------------------------------------------------------------

-- | The background outputs of a run, newest batch first.  Outputs are never
-- cancelled: a run waits for every one of them before it ends.
newtype OutputJobs = OutputJobs (IORef [OutputJob])

-- | One background output: the module it belongs to, its kind and its thread.
data OutputJob = OutputJob TaskKey FrontOutputKind (Async ())

frontOutputKindName                     :: FrontOutputKind -> String
frontOutputKindName FrontOutputTydb     = "tydb"
frontOutputKindName FrontOutputTydbCopy = "tydb-copy"
frontOutputKindName FrontOutputDoc      = "doc"

-- | Write a module's .tydb before its front stage reports done, so readers
-- never wait for a write.  The write is given its progress reporter.  A
-- stale generation writes nothing and reports nothing.  A failed write, its
-- start event included, fails the module.  Asynchronous exceptions are
-- rethrown.
commitTydb :: ModuleEnv -> ((InterfaceFiles.TyDbWriteProgress -> IO ()) -> IO ()) -> IO ()
commitTydb env write = do
    result <- trySync $ whenCurrent (buildGeneration build) $ do
                emit build (OutputStarted key FrontOutputTydb)
                runOutput build key FrontOutputTydb (write (emit build . OutputProgressed key FrontOutputTydb))
    either (\e -> moduleFailure [InterfaceWriteFailed (displayException e)]) return result
  where
    build = moduleBuild env
    key   = keyOf env

-- | Start background outputs.  Starting and recording happen under one
-- mask, so a cancelled stage cannot leave a write the run does not wait
-- for.  A stale generation starts and records nothing.  Each write checks
-- the generation again in its own thread, where only the write itself runs
-- unmasked.
startOutputs :: BuildEnv -> TaskKey -> [(FrontOutputKind, IO ())] -> IO ()
startOutputs build key outputs = mask_ $ do
    jobs <- catMaybes <$> mapM start outputs
    atomicModifyIORef' ref (\old -> (jobs ++ old, ()))
  where
    OutputJobs ref = buildOutputs build
    gen            = buildGeneration build
    start (kind, write) = do
      current <- isCurrent gen
      if not current
        then return Nothing
        else do emit build (OutputStarted key kind)
                thread <- asyncWithUnmask (\unmask -> whenCurrent gen (runOutput build key kind (unmask write)))
                return (Just (OutputJob key kind thread))

-- | Run a started output and report its end: its time, or Nothing when it
-- failed.  A failure to report is ignored unless asynchronous.  The
-- output's own failure is rethrown after the report.
runOutput :: BuildEnv -> TaskKey -> FrontOutputKind -> IO () -> IO ()
runOutput build key kind write = do
    (result, elapsed) <- timed (try write :: IO (Either SomeException ()))
    void (trySync (emit build (OutputFinished key kind (either (const Nothing) (const (Just elapsed)) result))))
    either throwIO return result

-- | Wait for every output started so far, and return the first failure in
-- recorded order: batches in start order, the last started first within a
-- batch.  An asynchronous exception found in an output is rethrown.
awaitOutputs :: BuildEnv -> IO (Maybe CompileFailure)
awaitOutputs build = do
    let OutputJobs ref = buildOutputs build
    jobs <- reverse <$> readIORef ref
    listToMaybe . catMaybes <$> mapM failure jobs
  where
    failure (OutputJob key kind thread) = do
      result <- waitCatch thread
      case result of
        Right ()                    -> return Nothing
        Left e | isAsyncException e -> throwIO e
               | otherwise          -> return (Just (OutputFailed key kind (displayException e)))

-- | The module's .tydb copied next to its source, under --tydb.
tydbCopy                                :: ModuleEnv -> [(FrontOutputKind, IO ())]
tydbCopy env                            = [ (FrontOutputTydbCopy, InterfaceFiles.copyInterface (tydbOf env) (srcBase (pathsOf env) (nameOf env) ++ InterfaceFiles.interfaceExt))
                                          | C.tydb (moduleOptions env) ]


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

-- | A module's back work: none, a job to queue now, or a job that waits for
-- every module depending on it.
data BackPlan = NoBack | EagerBack BackJob | DeferredBack DeferredBackJob

-- | A module whose front stage succeeded.
data FrontResult = FrontResult
  { frModuleInfo   :: Acton.Env.ModuleInfo              -- with escape summaries
  , frHashes       :: ModuleHashes
  , frInterest     :: Data.Set.Set (A.ModName, A.Name)  -- other modules' names its code uses, private uses included
  , frBack         :: BackPlan
  , frFrontTime    :: Maybe TimeSpec                    -- Nothing when quiet
  , frFrontTiming  :: Maybe FrontTiming                 -- with --timing
  , frInferredSigs :: [InferredSignature]
  }

-- | A result rebuilt from a module's .tydb, without front passes.
storedResult :: Acton.Env.ModuleInfo -> ModuleHashes -> Data.Set.Set (A.ModName, A.Name) -> BackPlan -> FrontResult
storedResult moduleInfo hashes interest back = FrontResult
    { frModuleInfo = moduleInfo, frHashes = hashes, frInterest = interest, frBack = back
    , frFrontTime = Nothing, frFrontTiming = Nothing, frInferredSigs = [] }

-- | The result of a module without a readable .tydb under --only-build: an
-- empty interface, no hashes, no back work.
emptyResult                             :: ModuleEnv -> FrontResult
emptyResult env                         = storedResult (Acton.Env.mkModuleInfo (nameOf env) [] [] Nothing) (ModuleHashes B.empty B.empty M.empty) Data.Set.empty NoBack

-- | An impl refresh computed from stored rows, before anything is written.
data Recomputation
  = Recomputed [InterfaceFiles.DepModuleInfo] [InterfaceFiles.NameHashInfo] FrontResult
  | Unresolved                          -- a dependency hash is unavailable: run the front passes
  | InterfaceGone                       -- the module's own .tydb cannot be read: the module fails

-- | The interface of a module read back whole from its .tydb, with the
-- escape summaries stored there.  Modules checked later in the same build
-- read them for their Hashable.hash contracts.
storedModuleInfo :: FilePath -> A.ModName -> InterfaceFiles.StoredModule -> IO Acton.Env.ModuleInfo
storedModuleInfo tydb mn module' = do
    summaries <- InterfaceFiles.openInterfaceDB tydb >>= InterfaceFiles.readInterfaceDBEscapeSummaries
    evaluate (rnf summaries)
    return (Acton.Env.withEscapeSummaries summaries (Acton.Env.mkModuleInfo mn imports (publicIfaceTE iface) doc))
  where I.NModule imports iface doc = InterfaceFiles.storedInterface module'

-- | A module's interface shell from its own .tydb: its interface, read on
-- demand and with its escape summaries, and its module hashes.
data InterfaceShell = InterfaceShell
  { shellModuleInfo :: Acton.Env.ModuleInfo
  , shellPubHash    :: B.ByteString
  , shellImplHash   :: B.ByteString
  }

-- | Read a module's interface shell from its own .tydb, without its typed
-- module.  A given pub hash, the one its header recorded, stands in for the
-- stored one.  Nothing when the .tydb cannot be read.
readInterfaceShell :: ModuleEnv -> Maybe B.ByteString -> IO (Maybe InterfaceShell)
readInterfaceShell env pubHash = do
    hashes <- InterfaceFiles.readModuleHashesMaybe tydb
    opened <- InterfaceFiles.openInterfaceDBMaybe tydb
    case (hashes, opened) of
      (Just (_, pub, impl), Just db) -> do
        (imports, doc) <- InterfaceFiles.readInterfaceDBModuleInfo db
        return (Just InterfaceShell { shellModuleInfo = Acton.Env.mkTyFileModuleInfo (nameOf env) imports doc db
                                    , shellPubHash    = fromMaybe pub pubHash
                                    , shellImplHash   = impl })
      _ -> return Nothing
  where tydb = tydbOf env

-- | A back job for a typed module read back from a .tydb: its source read
-- again, its environment rebuilt with protocols converted.
loadBackJob :: BuildEnv -> Acton.Env.Env0 -> TaskKey -> Paths -> C.CompileOptions -> A.Module -> B.ByteString -> IO BackJob
loadBackJob build env key paths opts typed implHash = do
    snap  <- Source.readSource (buildSources build) (srcFile paths (tkMod key))
    scope <- Acton.Env.mkEnv (searchPath paths) env typed
    return BackJob { bjKey = key, bjPaths = paths, bjOpts = opts, bjTypeEnv = Converter.convEnvProtos scope
                   , bjModule = typed, bjSource = Source.ssText snap, bjImplHash = implHash }

-- | The back job of the typed module that a module's own .tydb stores, in
-- the environment of the modules checked before it, tagged with the given
-- impl hash.
storedBackJob :: ModuleEnv -> InterfaceFiles.StoredModule -> B.ByteString -> IO BackJob
storedBackJob env module' = loadBackJob (moduleBuild env) (checkedModules env) (keyOf env) (pathsOf env) (moduleOptions env) (InterfaceFiles.storedTyped module')

type InterestMap = M.Map A.ModName (Data.Set.Set A.Name)

docNameCountThreshold :: Int
docNameCountThreshold = 10000

shouldGenerateDocOutput :: C.CompileOptions -> Bool -> Int -> Bool
shouldGenerateDocOutput opts tmp nameCount =
  not (C.skip_build opts) && not tmp && nameCount <= docNameCountThreshold

-- | Whether a module's back passes wait for its dependents and generate only
-- the names they use.  NotImplemented bodies come from a hand-written .ext.c,
-- so such a module is generated whole, like __builtin__: its statement rows
-- opt out of selective reads.  Planning asks with hasNotImpl False.
deferrable                              :: C.CompileOptions -> Bool -> Bool -> TaskKey -> Bool
deferrable opts blocked hasNotImpl k
  | C.no_dbp opts                       = False
  | C.only_build opts                   = False
  | C.altOutput opts                    = False
  | isBuiltin k                         = False
  | blocked                             = False
  | hasNotImpl                          = False
  | otherwise                           = True

-- | A module's deferred back job, given whether it has NotImplemented bodies,
-- its impl hash and its name count, when its back passes can wait.
deferredJob :: ModuleEnv -> Bool -> B.ByteString -> Int -> Maybe DeferredBackJob
deferredJob env hasNotImpl implHash nameCount
  | deferrable (moduleOptions env) (moduleBlocked env) hasNotImpl (keyOf env)
              = Just (DeferredBackJob (keyOf env) (pathsOf env) (moduleOptions env) implHash nameCount)
  | otherwise = Nothing

-- | The other modules' names that a module's names use, through their pub
-- and impl dependencies.
interestDeps :: [InterfaceFiles.NameHashInfo] -> Data.Set.Set (A.ModName, A.Name)
interestDeps names =
  Data.Set.fromList
    [ target
    | nh <- names
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
        TyTask _ -> deferrable (optionsFor ctx k) (Data.Set.member k dbpBlocked) False k
        _        -> False
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

-- Front passes -------------------------------------------------------------------------------------

-- | Check one module outside a stage graph (__builtin__, benchmarks): its
-- imports resolve through the module cache, and it is checked in the given
-- environment.
checkModule :: BuildEnv -> GlobalTask -> C.CompileOptions -> Acton.Env.Env0 -> ModuleSource -> A.Module -> IO (Either [Diagnostic String] FrontResult)
checkModule build t opts base = runFrontPasses ModuleEnv
    { moduleBuild = build, moduleTask = t, moduleCurrent = gtTask t, moduleOptions = opts
    , moduleResolver = cacheResolver (buildCache build) (gtPaths t), checkedModules = base
    , moduleBlocked = False, moduleForced = False }

-- | A module after its kinds, types and escape analysis.
data Checked = Checked
  { checkedScope     :: Acton.Env.Env0                   -- the environment it was checked in
  , checkedInterface :: I.NModule
  , checkedModule    :: A.Module                         -- typed
  , checkedTypeEnv   :: Acton.Env.Env0                   -- reconstruct's, protocols converted
  , checkedTests     :: [String]
  , checkedEscapes   :: InterfaceFiles.EscapeSummaries
  }

-- | A checked module's hashes, as its .tydb records them.
data Hashed = Hashed
  { hashedSource       :: B.ByteString                   -- of the source bytes
  , hashedPub          :: B.ByteString
  , hashedImpl         :: B.ByteString
  , hashedImports      :: [(A.ModName, B.ByteString)]    -- each header import's pub hash
  , hashedDependencies :: [InterfaceFiles.DepModuleInfo]
  , hashedNames        :: [InterfaceFiles.NameHashInfo]  -- every name, private ones included
  , hashedRoots        :: [A.Name]
  }

-- | What the type checker reports while it runs.
data TypeObserver = TypeObserver
  { typeProgress   :: Acton.Types.TypeProgressCallback
  , typeSignatures :: Maybe Acton.Types.TypeInferredCallback
  , typeReport     :: IO (Maybe TimeSpec, [TypeStmtTiming], [InferredSignature])  -- when progress first reached its total, statement timings, signatures
  }

-- | When the last event of one progress stream passed.
newtype Pacer = Pacer (IORef (Maybe TimeSpec))

-- | Check, hash and commit one parsed module, then start its doc page and
-- --tydb copy in the background, recorded in the build for awaitOutputs.
-- Progress and outputs are reported as the module's events.  The module's
-- own errors become its diagnostics, and every other exception propagates.
runFrontPasses :: ModuleEnv -> ModuleSource -> A.Module -> IO (Either [Diagnostic String] FrontResult)
runFrontPasses env source parsed = do
    createDirectoryIfMissing True (moduleTypesDir (projTypes (pathsOf env)) mn)
    (Right <$> passes) `catches` frontFailures env (sourceText source)
  where
    gopts    = buildOptions (moduleBuild env)
    opts     = moduleOptions env
    mn       = nameOf env
    now      = getTime Monotonic
    astStyle = style { mode = PageMode, lineLength = 120, ribbonsPerLine = 1.0 }
    passes = do
      start           <- now
      dumpWhen opts mn C.parse     "parse"     (Pretty.print parsed)
      dumpWhen opts mn C.parse_ast "parse-ast" (renderStyle astStyle (ppDoc parsed))
      observer        <- newTypeObserver env
      scope           <- Acton.Env.mkEnv (searchPath (pathsOf env)) (checkedModules env) parsed
      envDone         <- now
      kinded          <- kindsPass env scope parsed
      kindsDone       <- now
      (interface, typed, typeEnv, tests)
                      <- Acton.Types.reconstruct (Just (typeProgress observer)) (typeSignatures observer) scope kinded (Just (labelOf env))
      reconstructDone <- now
      forceTypeResult interface typed typeEnv tests
      forceDone       <- now
      escapes         <- escapePass env scope typed
      escapeDone      <- now
      let checked = Checked { checkedScope = scope, checkedInterface = interface, checkedModule = typed
                            , checkedTypeEnv = typeEnv, checkedTests = tests, checkedEscapes = escapes }
          I.NModule imports iface doc = interface
      hashed          <- hashPass env source parsed checked
      hashDone        <- now
      dumpWhen opts mn C.types "types" (Pretty.print typed)
      dumpWhen opts mn C.sigs  "sigs"  (Acton.Types.prettySigs scope mn imports iface)
      dumpsDone       <- now
      (progressDone, statements, signatures) <- typeReport observer
      end             <- now
      -- Once this module reports done, its rows are on disk.  Docs and the
      -- --tydb copy are never read back within this build, so they are
      -- written in the background.
      commitTydb env (writeTydb env source checked hashed)
      startOutputs (moduleBuild env) (keyOf env) (docOutputs env parsed checked hashed ++ tydbCopy env)
      let names  = hashedNames hashed
          -- reconstruct's typeEnv is already protocol-converted.
          job    = BackJob { bjKey = keyOf env, bjPaths = pathsOf env, bjOpts = opts, bjTypeEnv = typeEnv
                           , bjModule = typed, bjSource = sourceText source, bjImplHash = hashedImpl hashed }
          -- ftTypes spans everything from the end of the kinds check to the
          -- end of the type dumps.
          timing = FrontTiming
            { ftEnv               = envDone - start
            , ftKinds             = kindsDone - envDone
            , ftTypes             = dumpsDone - kindsDone
            , ftTypeReconstruct   = reconstructDone - kindsDone
            , ftTypeAfterProgress = maybe 0 (reconstructDone -) progressDone
            , ftTypeForce         = forceDone - reconstructDone
            , ftTypeEscape        = escapeDone - forceDone
            , ftTypeHash          = hashDone - escapeDone
            , ftTypeStmtTimings   = statements }
      return FrontResult
        { frModuleInfo   = Acton.Env.withEscapeSummaries escapes (Acton.Env.mkModuleInfo mn imports (publicIfaceTE iface) doc)
        , frHashes       = ModuleHashes { mhPub = hashedPub hashed, mhImpl = hashedImpl hashed, mhNames = publicNameMap names }
        , frInterest     = interestDeps names
        , frBack         = maybe (EagerBack job) DeferredBack (deferredJob env (A.hasNotImpl (A.mbody typed)) (hashedImpl hashed) (length names))
        , frFrontTime    = if quiet gopts opts then Nothing else Just (end - start)
        , frFrontTiming  = if C.timing gopts then Just timing else Nothing
        , frInferredSigs = signatures }

-- | Evaluate the type check's results here, inside the module's
-- diagnostics: a lazy type error becomes the module's diagnostic.  The
-- interface, the typed module and the tests in full, the type environment's
-- names to weak head normal form.
forceTypeResult :: I.NModule -> A.Module -> Acton.Env.EnvF x -> [String] -> IO ()
forceTypeResult interface typed typeEnv tests = do
    evaluate (rnf interface)
    evaluate (rnf typed)
    evaluate (forceHTEnv (Acton.Env.hnames typeEnv))
    evaluate (rnf tests)

forceHTEnv                              :: I.HTEnv -> ()
forceHTEnv                              = HM.foldl' (\() nameInfo -> nameInfo `seq` ()) ()

-- | The kinds check, reported as one step: 0/1 before it, 1/1 after.
kindsPass :: ModuleEnv -> Acton.Env.Env0 -> A.Module -> IO A.Module
kindsPass env scope parsed = do
    frontProgress env (FrontPassProgress FrontPassKinds 0 1 Nothing)
    kinded <- Acton.Kinds.check scope parsed
    frontProgress env (FrontPassProgress FrontPassKinds 1 1 Nothing)
    dumpWhen (moduleOptions env) (nameOf env) C.kinds "kinds" (Pretty.print kinded)
    return kinded

-- | Watch Types.reconstruct: its progress as paced FrontPassTypes events,
-- the first moment progress reached its total, statement timings (with
-- --timing and --verbose) and inferred signatures (with --timing or
-- --verbose).  The report gives timings and signatures in emission order.
newTypeObserver :: ModuleEnv -> IO TypeObserver
newTypeObserver env = do
    pacer      <- newPacer
    reachedRef <- newIORef Nothing
    activeRef  <- newIORef Nothing
    timingsRef <- newIORef []
    sigsRef    <- newIORef []
    let onProgress total completed current names _weight = do
          t <- getTime Monotonic
          when (total > 0 && completed >= total && isNothing current) $ do
            reached <- readIORef reachedRef
            when (isNothing reached) (writeIORef reachedRef (Just t))
          -- A statement's time runs from the event that names it as
          -- current to the next event.
          when statementTimings $ do
            active <- readIORef activeRef
            forM_ active $ \(activeNames, activeTotal, started) ->
              modifyIORef' timingsRef (TypeStmtTiming { tstCompleted = completed, tstTotal = activeTotal
                                                      , tstNames = activeNames, tstTime = t - started } :)
            writeIORef activeRef ((names, total, t) <$ current)
          passed <- pace pacer False t completed total
          forM_ passed $ \done -> frontProgress env (FrontPassProgress FrontPassTypes done total current)
        onSignature names signature = modifyIORef' sigsRef (InferredSignature names signature :)
    return TypeObserver
      { typeProgress   = onProgress
      , typeSignatures = if C.timing gopts || C.verbose gopts then Just onSignature else Nothing
      , typeReport     = (,,) <$> readIORef reachedRef <*> (reverse <$> readIORef timingsRef) <*> (reverse <$> readIORef sigsRef) }
  where
    gopts            = buildOptions (moduleBuild env)
    statementTimings = C.timing gopts && C.verbose gopts

-- | The escape analysis of every function, run to completion here rather
-- than in whatever later reads a result, such as the interface write: the
-- analysis evaluates each function's summary in full as part of producing
-- its list of functions.  The summaries are part of the module's
-- interface, and every Hashable.hash implementation is checked here before
-- B_hash may pass it an automatically allocated hasher.  The command line
-- flag controls only the dump.
escapePass :: ModuleEnv -> Acton.Env.Env0 -> A.Module -> IO InterfaceFiles.EscapeSummaries
escapePass env scope typed = do
    _ <- evaluate (length (Acton.EscapeAnalysis.reportFunctions report))
    evaluate (rnf summaries)
    dumpWhen (moduleOptions env) (nameOf env) C.escape_analysis "escape-analysis" (Acton.EscapeAnalysis.renderEscapeReport report)
    forM_ (Acton.EscapeAnalysis.contractViolation report) $ \(l, message) -> throwIO (Acton.Env.OtherError l message)
    return summaries
  where
    report    = Acton.EscapeAnalysis.analyzeModuleWithImports (importedEscapeSummary scope) typed
    summaries = Acton.EscapeAnalysis.interfaceSummaries report

-- | The escape summary that a module checked in this environment reads for
-- an imported function, following import aliases.
importedEscapeSummary :: Acton.Env.Env0 -> A.QName -> Maybe [(A.Name, Acton.EscapeAnalysis.Escape)]
importedEscapeSummary scope qn = do
    (m, n)     <- target qn
    moduleInfo <- Acton.Env.lookupModuleInfo m scope
    summary    <- Acton.Env.moduleEscapeSummary moduleInfo [n]
    return [ (p, if mayEscape then Acton.EscapeAnalysis.MayEscape else Acton.EscapeAnalysis.NoEscape) | (p, mayEscape) <- summary ]
  where
    target (A.NoQ n) = case Acton.Env.lookupName n scope of
                         Just (I.NAlias aliased) -> target aliased
                         _                       -> Nothing
    target q         = qnameTarget q

-- | Hash a checked module.  Its header imports resolve first, and every
-- missing one fails the module.  Then come the per-name source, impl and
-- pub hashes (escape bits folded into pub), the dependencies split into
-- local and external names, and the modules of the external ones, every
-- missing one failing the module.  Last come the hashes of the external
-- names for signatures, pub and impl: all three groups are resolved before
-- the first failing one fails the module.  Every step is forced here, inside
-- the module's diagnostics and the hash timing window.  Progress counts the
-- items of every step: a forced 0 first, at most one event per 50 ms after,
-- and a forced total last.
hashPass :: ModuleEnv -> ModuleSource -> A.Module -> Checked -> IO Hashed
hashPass env source parsed checked = do
    pacer <- newPacer
    let report forcedEvent completed = do
          t <- getTime Monotonic
          passed <- pace pacer forcedEvent t completed total
          forM_ passed $ \done -> frontProgress env (FrontPassProgress FrontPassHash done total Nothing)
        step base done = when ((base + done) `rem` stride == 0) (report False (base + done))
    _            <- evaluate afterImplDeps
    report True 0
    -- Of the raw bytes, so any edit of the source counts as a change.
    sourceHash   <- forced (SHA256.hash (sourceBytes source))
    imports      <- resolveAll (\m -> fmap ((,) m) <$> resolveModule resolver Pub m) MissingInterface headerImports
    srcHashes    <- forced =<< Hashing.nameHashesFromItemsWithProgress (step 0) srcItems
    implOwn      <- forced =<< Hashing.nameHashesFromItemsWithProgress (step afterSource) implItems
    keys         <- forced (M.keysSet srcHashes `Data.Set.union` M.keysSet implOwn)
    selfPub      <- forced =<< Hashing.nameInfoHashesWithProgress (step afterImpl) infos
    selfPubBits  <- forced (Hashing.withEscapeBits (checkedEscapes checked) selfPub)
    (sigLocal, sigExternal)   <- forced =<< Hashing.pubSigSplitDepsFromNameInfoMapWithProgress (step afterInfos) mn hashEnv keys infos
    (implLocal, implExternal) <- forced =<< Hashing.implSplitDepsFromItemsWithProgress (step afterInfoDeps) mn hashEnv keys implItems
    -- Public dependencies are the signature dependencies and the
    -- implementation dependencies together.
    let (pubLocal, pubExternal) = Hashing.mergePubDeps sigLocal sigExternal implLocal implExternal
        externalModules         = Data.Set.toList (Hashing.externalModules pubExternal `Data.Set.union` Hashing.externalModules implExternal)
    _            <- forced (pubLocal, pubExternal, externalModules)
    dependencies <- resolveAll (dependencyModule resolver) MissingInterface externalModules
    sigGroup     <- externalHashes resolver Pub  locations sigExternal
    pubGroup     <- externalHashes resolver Pub  locations pubExternal
    implGroup    <- externalHashes resolver Impl locations implExternal
    sigResolved  <- either moduleFailure return sigGroup
    pubResolved  <- either moduleFailure return pubGroup
    implResolved <- either moduleFailure return implGroup
    _            <- forced (sigResolved, pubResolved, implResolved)
    -- The pub and impl hashes are independent, and so are the module hashes
    -- and the name rows that both use them.
    (pubHashes, implHashes) <- concurrently (forced (Hashing.computeHashesSortedDeps selfPubBits sigLocal sigResolved))
                                            (forced (Hashing.computeHashesSortedDeps implOwn implLocal implResolved))
    ((pub, impl), names)    <- concurrently (forced (Hashing.moduleHashesFromHashMaps (checkedInterface checked) keys pubHashes implHashes))
                                            (forced (Hashing.assembleNameHashes keys srcHashes pubHashes implHashes implOwn pubLocal implLocal pubResolved implResolved))
    _            <- forced (sourceHash, pub, impl, sourceMeta source, imports)
    _            <- forced (names, roots, checkedTests checked, doc)
    report True total
    return Hashed { hashedSource = sourceHash, hashedPub = pub, hashedImpl = impl, hashedImports = imports
                  , hashedDependencies = dependencies, hashedNames = names, hashedRoots = roots }
  where
    resolver    = moduleResolver env
    mn          = nameOf env
    hashEnv     = setMod mn (checkedScope checked)
    I.NModule moduleImports iface doc = checkedInterface checked
    headerImports
      | mn == mBuiltin = moduleImports
      | otherwise      = nub (mBuiltin : moduleImports)
    srcItems    = Hashing.topLevelItems parsed
    implItems   = Hashing.topLevelItems (checkedModule checked)
    infos       = M.fromList iface                      -- every local name
    locations   = nameLocations srcItems `M.union` nameLocations implItems
    roots       = [ n | (n, i) <- iface, rootEligible i ]
    -- Progress steps: source items, impl items, interface names (hashes),
    -- interface names (dependencies), impl items (dependencies), and the
    -- final step.
    afterSource   = length srcItems
    afterImpl     = afterSource + length implItems
    afterInfos    = afterImpl + M.size infos
    afterInfoDeps = afterInfos + M.size infos
    afterImplDeps = afterInfoDeps + length implItems
    total         = afterImplDeps + 1
    stride        = max 1 (total `div` 1000)

-- | Where each top-level name is defined: at its declaration, else at its
-- statement.
nameLocations :: [Hashing.TopLevelItem] -> M.Map A.Name SrcLoc
nameLocations items = M.fromList [ (n, A.dloc d) | Hashing.TLDecl n d <- items ]
            `M.union` M.fromList [ (n, A.sloc s) | Hashing.TLStmt n s <- items ]

-- | Write a checked and hashed module's .tydb, reporting its progress.
writeTydb :: ModuleEnv -> ModuleSource -> Checked -> Hashed -> (InterfaceFiles.TyDbWriteProgress -> IO ()) -> IO ()
writeTydb env source checked hashed onProgress =
    InterfaceFiles.writeFileWithProgress onProgress A.version (tydbOf env)
      (hashedSource hashed) (hashedPub hashed) (hashedImpl hashed) (sourceMeta source) (hashedImports hashed)
      (hashedDependencies hashed) (hashedNames hashed) (checkedEscapes checked) (hashedRoots hashed) (checkedTests checked)
      doc (checkedInterface checked) (checkedModule checked)
  where I.NModule _ _ doc = checkedInterface checked

-- | The module's HTML documentation page, unless the module has too many
-- names, --skip-build is given or the build is temporary.  The page shows
-- the public names as --sigs simplifies them, with the docstrings of the
-- parsed module.
docOutputs :: ModuleEnv -> A.Module -> Checked -> Hashed -> [(FrontOutputKind, IO ())]
docOutputs env parsed checked hashed =
    [ (FrontOutputDoc, writeDoc) | shouldGenerateDocOutput (moduleOptions env) (isTmp paths) (length (hashedNames hashed)) ]
  where
    paths       = pathsOf env
    mn          = nameOf env
    I.NModule imports iface doc = checkedInterface checked
    publicIface = publicIfaceTE iface
    docDir      = joinPath [projPath paths, "out", "doc"]
    docFile     = case A.modPath mn of
                    []   -> docDir </> "unnamed" <.> "html"
                    path -> joinPath (docDir : init path) </> last path <.> "html"
    moduleTypes = maybe publicIface Acton.Env.modulePublicTEnv (Acton.Env.lookupModuleInfo mn (checkedTypeEnv checked))
    simplified  = simp (define publicIface (setMod mn (checkedScope checked))) moduleTypes
    writeDoc    = do
      createDirectoryIfMissing True (takeDirectory docFile)
      writeFileUtf8Atomic docFile (DocP.printHtmlDoc (I.NModule imports simplified doc) parsed)

newPacer                                :: IO Pacer
newPacer                                = Pacer <$> newIORef Nothing

-- | Pace one progress stream: an event passes when forced, when it reaches
-- the total, or when 50 ms have passed since the last one that passed.  The
-- answer, for an event that passes, is its count clamped to the total.
-- Events from concurrent threads may both pass: the last time is read and
-- written separately.
pace :: Pacer -> Bool -> TimeSpec -> Int -> Int -> IO (Maybe Int)
pace (Pacer lastPassed) forcedEvent t completed total = do
    previous <- readIORef lastPassed
    let done   = min total (max 0 completed)
        passes = forcedEvent || done >= total || maybe True (\p -> toNanoSecs t - toNanoSecs p >= 50000000) previous
    when passes (writeIORef lastPassed (Just t))
    return (if passes then Just done else Nothing)

frontProgress                           :: ModuleEnv -> FrontPassProgress -> IO ()
frontProgress env p                     = emit (moduleBuild env) (FrontProgressed (moduleTask env) p)

-- | A value evaluated in full, here.
forced                                  :: NFData a => a -> IO a
forced                                  = evaluate . force

data DbpSelection = DbpSelection
  { dbsModule :: A.Module
  , dbsSelectedCount :: Int
  , dbsFallbackReason :: Maybe String
  }

data DbpNameSelection = DbpNameSelection
  { dnsSelectedNames :: Data.Set.Set A.Name
  , dnsNameHashes :: [InterfaceFiles.NameHashInfo]
  }

prepareDeferredBackJob :: BuildEnv
                       -> Acton.Env.Env0
                       -> InterestMap
                       -> DeferredBackJob
                       -> IO (Maybe BackJob)
prepareDeferredBackJob build envAcc interestMap deferred = do
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
      logDbpSelection build mn deferred totalNames (Data.Set.size interested) (Data.Set.size rootSeeds) (Data.Set.size (dnsSelectedNames nameSelection)) Nothing "generated code up to date"
      return Nothing
    else do
      selectedTmod <- InterfaceFiles.readSelectedModule tyFile (dnsNameHashes nameSelection) (dnsSelectedNames nameSelection)
      -- Same-version .tydb files always carry statement indices, so a failed
      -- selective read means a corrupt or hand-doctored file -- an eager
      -- whole-module fallback would mask that (and read gigabytes doing it).
      selection <- case selectedTmod of
        Just tmod -> return $ selectDbpModule totalNames nameSelection tmod
        Nothing -> error ("Internal error: missing statement rows in " ++ tyFile)
      snap <- Source.readSource (buildSources build) actFile
      env1 <- Acton.Env.mkEnv (searchPath paths) envAcc (dbsModule selection)
      logDbpSelection build mn deferred totalNames (Data.Set.size interested) (Data.Set.size rootSeeds) (dbsSelectedCount selection) (dbsFallbackReason selection) "generated code out of date"
      return $ Just BackJob
        { bjKey = deferredKey deferred
        , bjPaths = paths
        , bjOpts = deferredOptions deferred
        , bjTypeEnv = Converter.convEnvProtos env1
        , bjModule = dbsModule selection
        , bjSource = Source.ssText snap
        , bjImplHash = codegenHash
        }

logDbpSelection :: BuildEnv
                -> A.ModName
                -> DeferredBackJob
                -> Int
                -> Int
                -> Int
                -> Int
                -> Maybe String
                -> String
                -> IO ()
logDbpSelection build mn deferred totalNames interestedCount rootCount selectedCount fallbackReason codegenReason =
  info build $
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

-- | Compile a plan's needed tasks using a parallel, dependency-aware
-- scheduler. This drives front passes and reports diagnostics, progress and
-- ready back-pass jobs as events of the BuildEnv.
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
--   - The ModuleHashes of every module whose front stage finished are kept during the run.
--     TyTask compares recorded dependency hashes against current provider hashes (in-graph
--     via those ModuleHashes, otherwise via cached .tydb headers). Pub deltas trigger front
--     passes; impl deltas trigger back passes with an impl-hash refresh.
--   - When a TyTask is stale for public reasons (or altOutput on the root), we convert it to an
--     ActonTask by parsing the .act file and run front passes; when it is fresh we reuse the
--     header and carry forward the recorded hashes.
--
-- Scheduling:
--   - Critical-path heuristic (file size proxy) picks among ready tasks; runs up to job cap.
--   - Each worker uses an env snapshot; coordinator merges the resulting interface/doc and
--     updates indegrees/ready sets. Dependents receive hashes through their ModuleEnv's resolver.
--   - Each module is compiled with optionsFor: dependency projects skip their
--     tests and final binary while the root keeps the user's options.
compileModules :: BuildEnv -> CompilePlan -> IO (Either CompileFailure (Acton.Env.Env0, Bool))
compileModules build plan = do
    runningRef <- newIORef []
    let cancelRunning = readIORef runningRef >>= mapM_ cancel
        -- A failed output fails a run that otherwise succeeded.
        withOutputFailure res = do
          failure <- awaitOutputs build
          return (case (res, failure) of
                    (Right _, Just f) -> Left f
                    _                 -> res)
    let compileMain = do
          -- Reject cycles
          if not (null importCycles)
            then return $ Left (ImportCycles importCycles)
            else do
              -- Compile __builtin__ first if present anywhere in the graph
              case builtinOrder of
                [t] -> do
                  res <- compileBuiltin t
                  case res of
                    Left err -> return (Left err)
                    Right () -> continue runningRef
                _ -> continue runningRef
    -- Whatever ends the run, running stages are cancelled first and every
    -- output is awaited after: outputs are never cancelled, and startCompile
    -- waits for this before it starts the next generation.
    ((compileMain >>= withOutputFailure) `finally` cancelRunning) `finally` void (awaitOutputs build)
  where
    continue runningRef = do
      baseEnv <- Acton.Env.initEnvWithEscapeSummaries True builtinPath False

      costMap <- fmap M.fromList $ forM nonBuiltinTasks $ \t -> do
                    let fp = taskSourceFile t
                    ok <- doesFileExist fp
                    sz <- if ok then getFileSize fp else return 0
                    return (gtKey t, sz)
      let cwMap = criticalWeights modules costMap

      maxParallel <- compileParallelism gopts

      loop runningRef stageInitialReady [] M.empty M.empty M.empty M.empty stageIndeg stagePending0 baseEnv False maxParallel cwMap

    -- Basic maps/sets ----------------------------------------------------
    sp         = buildSources build
    gopts      = buildOptions build
    cache      = buildCache build
    ctx        = cpContext plan
    tasks      = cpNeededTasks plan
    dbpBlocked = cpDbpBlocked plan

    taskMap = M.fromList [ (gtKey t, t) | t <- tasks ]
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
                            -> M.Map TaskKey ModuleHashes
                            -> M.Map TaskKey (DeferredBackJob, Data.Set.Set TaskKey)
                            -> IO (Either CompileFailure (M.Map TaskKey (DeferredBackJob, Data.Set.Set TaskKey)))
    flushReadyDeferredBacks envAcc interestMap finished deferredBacks = do
      let (ready, waiting) =
            M.partitionWithKey
              (\_ (_deferred, waitSet) -> waitSet `Data.Set.isSubsetOf` M.keysSet finished)
              deferredBacks
      prepared <- trySync $
        forM (M.elems ready) $ \(deferred, _waitSet) -> do
          mJob <- prepareDeferredBackJob build envAcc interestMap deferred
          case mJob of
            Nothing -> emit build (BackSkipped (deferredKey deferred))
            Just _ -> return ()
          return mJob
      case prepared of
        Left err -> return (Left (DeferredBackFailed (displayException err)))
        Right jobs -> do
          mapM_ (emit build . BackReady) (catMaybes jobs)
          return (Right waiting)

    -- TODO: can we reintegrate this into the normal loop to avoid duplication?
    -- NOTE: FYI, it was originally part of the main loop but factored out for
    -- clarity when we changed to use async, front/back jobs etc. There were so
    -- many other changes so at the time it was easier to separate builtin
    -- compilation but perhaps we can find a way to merge it back in to one
    -- general loop.
    compileBuiltin :: GlobalTask -> IO (Either CompileFailure ())
    compileBuiltin t = do
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
              emit build (Diagnostics t diags)
              return (Left BuiltinFailed)
            Right (source, m) -> do
              emit build (FrontStarted t)
              builtinEnv0 <- Acton.Env.initEnv (projTypes bPaths) True
              res <- checkModule build t optsBuiltin builtinEnv0 source m
              case res of
                Left diags -> do
                  emit build (FrontFinished t Nothing)
                  emit build (Diagnostics t diags)
                  return (Left BuiltinFailed)
                Right fr -> do
                  outputFailure <- awaitOutputs build
                  case outputFailure of
                    Just err -> return (Left err)
                    Nothing -> do
                      emit build (FrontFinished t (Just fr))
                      rememberFront cache mn fr
                      case frBack fr of
                        EagerBack job -> emit build (BackReady job)
                        _             -> return ()
                      return (Right ())

    -- One module ---------------------------------------------------------
    doOne :: Acton.Env.Env0
          -> M.Map TaskKey ModuleHashes
          -> M.Map TaskKey CompileTask
          -> TaskKey
          -> IO (TaskKey, Either [Diagnostic String] FrontResult)
    doOne envSnap finished parsedTasks key = do
      t <- case M.lookup key taskMap of
             Just x -> return x
             Nothing -> error ("Internal error: missing task for key " ++ show key)
      let optsT = optionsFor ctx key
          env = ModuleEnv
            { moduleBuild    = build
            , moduleTask     = t
            , moduleCurrent  = M.findWithDefault (gtTask t) key parsedTasks
            , moduleOptions  = optsT
            , moduleResolver = inRunResolver cache taskMap finished t
            , checkedModules = envSnap
            , moduleBlocked  = Data.Set.member key dbpBlocked
            , moduleForced   = (C.altOutput optsT && tkMod key == rootAlt) || C.alwaysbuild optsT
            }
          taskCurrent = moduleCurrent env
          paths = pathsOf env
          mn    = nameOf env
          resolver = moduleResolver env
          providers = gtImportProviders t
          actFile = taskSourceFile t
          tyFile = tydbOf env
          -- The module succeeded: its hashes become visible to reads.
          rememberResult fr = do
            rememberFront cache mn fr
            return (key, Right fr)
          -- The last steps of a result rebuilt from the .tydb, in the
          -- stage's thread: the --tydb copy starts, then the hashes become
          -- visible to reads.
          finishStored fr = do
            startOutputs build key (tydbCopy env)
            rememberResult fr
          -- The pub hash a cached module's header recorded.
          headerPubHash = case taskCurrent of
            TyTask h -> Just (InterfaceFiles.summaryPubHash h)
            _        -> Nothing
          -- A result from the interface shell.  Its interest comes from the
          -- dependency rows: the per-name rows on disk carry no external
          -- dependencies.
          shellResult back shell = do
            interest <- cachedInterestDeps
            finishStored (storedResult (shellModuleInfo shell)
                            ModuleHashes { mhPub = shellPubHash shell, mhImpl = shellImplHash shell, mhNames = M.empty }
                            interest back)
          missingOwnInterface = return (key, Left [explainError env "" (MissingInterface mn)])

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
                 mh <- resolveModule resolver Pub depMn
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
                return (Left (map (explainError env "" . MissingInterface) missingSorted))

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
              resolveDepModule depMn =
                maybe (Left [explainError env "" (MissingInterface depMn)]) Right <$> dependencyModule resolver depMn

          nameHashSummary prevMap newMap =
            let oldKeys = Data.Set.fromList (M.keys prevMap)
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
            mInfo <- resolveName resolver m n
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
                current <- dependencyModule resolver depMn
                return $ case current of
                  Just currentInfo ->
                    ( depMn
                    , InterfaceFiles.dmiPubHash currentInfo /= InterfaceFiles.dmiPubHash depInfo
                    , InterfaceFiles.dmiImplHash currentInfo /= InterfaceFiles.dmiImplHash depInfo
                    )
                  Nothing ->
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
            foldM checkDepModule ([], [], [], [], M.empty, M.empty, Data.Set.empty) changedMods
            where
              checkDepModule acc depMn = do
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
                mInfo <- resolveName resolver depMn depName
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
              shell <- readInterfaceShell env headerPubHash
              maybe (return (key, Right (emptyResult env))) (shellResult NoBack) shell
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
                  -- Front passes run on source or API changes, or when forced.
                  needFront = needBySource || needByPub || needByMissing || moduleForced env
                  mModuleImplHash = case taskCurrent of
                    TyTask h -> Just (InterfaceFiles.summaryImplHash h)
                    _ -> Nothing
              cachedDeferredBackJob <- case taskCurrent of
                    TyTask h -> do
                      hasNotImpl <- InterfaceFiles.readStmtHasNotImpl tyFile
                      return (deferredJob env hasNotImpl (InterfaceFiles.summaryImplHash h) (InterfaceFiles.summaryNameCount h))
                    _ -> return Nothing
              let isCachedDbp = isJust cachedDeferredBackJob
              let canCheckCodegen = not needFront && not needByImpl && not (C.altOutput optsT) && not isCachedDbp
              mCodegenStatus <- case mModuleImplHash of
                Just implHash | canCheckCodegen -> Just <$> codegenStatus paths mn implHash
                _ -> return Nothing
              let needByCodegen = maybe False (not . codegenUpToDate) mCodegenStatus
              let runFront = do
                    prevNameHashes <- if C.verbose gopts
                      then lookupNameMap cache (searchPath paths) mn
                      else return Nothing
                    if needBySource
                      then info build ("  Stale " ++ labelOf env ++ ": source changed")
                      else do
                        when needByPub $ do
                          let fmtDelta (qn, old, new) = prstr qn ++ " " ++ shortHash old ++ " → " ++ shortHash new ++ fmtUsers pubUsers qn
                          info build ("  Stale " ++ labelOf env ++ ": pub changes in " ++ Data.List.intercalate ", " (map fmtDelta pubDeltas))
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
                          info build ("  Stale " ++ labelOf env ++ ": missing dep hashes in " ++ Data.List.intercalate ", " (pubMissingItems ++ implMissingItems ++ rowMissingItems))
                    parsed <- materialize sp optsT mn actFile Nothing taskCurrent
                    case parsed of
                      Left diags -> return (key, Left diags)
                      Right (source, m) -> do
                        res <- runFrontPasses env source (adjustImports providers m)
                        case res of
                          Left diags -> return (key, Left diags)
                          Right fr -> do
                            -- prevNameHashes is Nothing unless --verbose.
                            forM_ prevNameHashes $ \prevMap ->
                              forM_ (nameHashSummary prevMap (mhNames (frHashes fr))) $ \summary ->
                                info build ("  Hash deltas " ++ labelOf env ++ ": " ++ summary)
                            rememberResult fr
                  -- Only the attempt falls back to the front passes, and only
                  -- for a synchronous exception or an unresolved hash.  The
                  -- write comes after every read, so a fallback never runs on
                  -- top of a committed refresh.  A failed write and a missing
                  -- own interface fail the module instead.
                  runImplRefresh = do
                    let fmtDelta (qn, old, new) = prstr qn ++ " " ++ shortHash old ++ " → " ++ shortHash new ++ fmtUsers implUsers qn
                    info build ("  Stale " ++ labelOf env ++ ": impl changes in " ++ Data.List.intercalate ", " (map fmtDelta implDeltas))
                    attempt <- trySync recomputeImpl
                    case attempt of
                      Right (Recomputed updatedDepModules updatedNameHashes fr) -> do
                        -- The refreshed rows are committed before the module
                        -- reports done.  A write failure is this module's
                        -- diagnostics.
                        committed <- moduleBoundary env $ Right <$>
                          commitTydb env (\_ -> InterfaceFiles.updateImplRefresh tyFile (mhImpl (frHashes fr)) updatedDepModules updatedNameHashes)
                        case committed of
                          Left diags -> return (key, Left diags)
                          Right () -> finishStored fr
                      Right InterfaceGone -> missingOwnInterface
                      _ -> do
                        info build ("  Stale " ++ labelOf env ++ ": impl refresh encountered unresolved dep hashes; rerunning front passes")
                        runFront
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
                                let hashes pub = ModuleHashes { mhPub = pub, mhImpl = moduleImplHash, mhNames = publicNameMap updatedNameHashes }
                                    interest   = interestDeps updatedNameHashes
                                case deferredJob env moduleHasNotImpl moduleImplHash (length updatedNameHashes) of
                                  Just job -> do
                                    shell <- readInterfaceShell env (Just storedPubHash)
                                    return $ case shell of
                                      Nothing -> InterfaceGone
                                      Just s  -> Recomputed updatedDepModules updatedNameHashes
                                                   (storedResult (shellModuleInfo s) (hashes (shellPubHash s)) interest (DeferredBack job))
                                  Nothing -> do
                                    stored <- InterfaceFiles.readFileMaybe tyFile
                                    case stored of
                                      Nothing -> return InterfaceGone
                                      Just module' -> do
                                        job <- storedBackJob env module' moduleImplHash
                                        -- Forced here, so a corrupt row fails the
                                        -- attempt before anything is written.
                                        evaluate (rnf (InterfaceFiles.storedInterface module'))
                                        evaluate (rnf (InterfaceFiles.storedTyped module'))
                                        moduleInfo <- storedModuleInfo tyFile mn module'
                                        return (Recomputed updatedDepModules updatedNameHashes
                                                  (storedResult moduleInfo (hashes (InterfaceFiles.storedPubHash module')) interest (EagerBack job)))
                      _ -> return Unresolved
                  -- The codegen check skips a module whose back passes can
                  -- wait, judged by the NotImplemented flag stored with its
                  -- typed module, so this job is always eager.
                  runCodegenRefresh = do
                    info build ("  Stale " ++ labelOf env ++ ": generated code out of date" ++ maybe "" formatCodegenDelta mCodegenStatus)
                    stored <- InterfaceFiles.readFileMaybe tyFile
                    case stored of
                      Nothing -> missingOwnInterface
                      Just module' -> do
                        job <- storedBackJob env module' (InterfaceFiles.storedImplHash module')
                        interest <- cachedInterestDeps
                        moduleInfo <- storedModuleInfo tyFile mn module'
                        finishStored (storedResult moduleInfo
                                        ModuleHashes { mhPub   = InterfaceFiles.storedPubHash module'
                                                     , mhImpl  = InterfaceFiles.storedImplHash module'
                                                     , mhNames = publicNameMap (InterfaceFiles.storedNames module') }
                                        interest (EagerBack job))
                  runReuse = do
                    info build ("  Fresh " ++ labelOf env ++ ": using cached .tydb")
                    shell <- readInterfaceShell env headerPubHash
                    maybe missingOwnInterface (shellResult (maybe NoBack DeferredBack cachedDeferredBackJob)) shell
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
          onProgress = emit build . ParseProgressed t
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
                  -> M.Map TaskKey ModuleHashes
                  -> M.Map TaskKey CompileTask
                  -> TaskKey
                  -> IO (StageKey, Either [Diagnostic String] StageSuccess)
    runFrontStage envSnap finished parsedTasks key = do
      (doneKey, outcome) <- doOne envSnap finished parsedTasks key
      return (FrontStage doneKey, fmap StageFronted outcome)

    scheduleMore :: Int -> [StageKey]
                 -> [(Async (StageKey, Either [Diagnostic String] StageSuccess), StageKey)]
                 -> M.Map TaskKey ModuleHashes
                 -> M.Map TaskKey CompileTask
                 -> Acton.Env.Env0
                 -> M.Map TaskKey Integer
                 -> IO ([StageKey]
                       , [(Async (StageKey, Either [Diagnostic String] StageSuccess), StageKey)])
    scheduleMore k rdy running finished parsedTasks envSnap cw = do
      let rdySorted = Data.List.sortOn (Down . stagePriority cw) rdy
          (toStart, rdy') = splitAt k rdySorted
      new <- forM toStart $ \sk -> do
                case sk of
                  FrontStage key ->
                    case M.lookup key taskMap of
                      Just t -> emit build (FrontStarted t)
                      Nothing -> return ()
                  ParseStage key ->
                    case M.lookup key taskMap of
                      Just t -> emit build (ParseStarted t)
                      Nothing -> return ()
                a <- async $
                  case sk of
                    ParseStage key -> runParseStage key
                    FrontStage key -> runFrontStage envSnap finished parsedTasks key
                return (a, sk)
      return (rdy', new ++ running)

    loop :: IORef [Async (StageKey, Either [Diagnostic String] StageSuccess)]
         -> [StageKey]
         -> [(Async (StageKey, Either [Diagnostic String] StageSuccess), StageKey)]
         -> M.Map TaskKey ModuleHashes
         -> M.Map TaskKey CompileTask
         -> InterestMap
         -> M.Map TaskKey (DeferredBackJob, Data.Set.Set TaskKey)
         -> M.Map StageKey Int
         -> Data.Set.Set StageKey
         -> Acton.Env.Env0
         -> Bool
         -> Int
         -> M.Map TaskKey Integer
         -> IO (Either CompileFailure (Acton.Env.Env0, Bool))
    loop runningRef rdy running finished parsedTasks interestMap deferredBacks ind pend envAcc hadErrors maxPar cw = do
      (rdy1, running1) <- mask_ $ do
        res@(rdy1', running1') <- scheduleMore (maxPar - length running) rdy running finished parsedTasks envAcc cw
        writeIORef runningRef (map fst running1')
        return res
      if null running1 && null rdy1
        then if Data.Set.null pend
                 then do
                   flushRes <- flushReadyDeferredBacks envAcc interestMap finished deferredBacks
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
          case outcome of
            -- Only a front stage fails: a parse stage reports a parse error as
            -- a ParseErrorTask, which its front stage turns into diagnostics.
            Left diags -> do
              emit build (FrontFinished tDone Nothing)
              emit build (Diagnostics tDone diags)
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
              loop runningRef rdy2 running3 finished parsedTasks2 interestMap deferredBacks ind pend2 envAcc True maxPar cw
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
                  emit build (ParseFinished tDone mParseTime)
                  let parsedTasks2 = M.insert keyDone parsed parsedTasks
                  loop runningRef rdy2 running2 finished parsedTasks2 interestMap deferredBacks ind2 pend2 envAcc hadErrors maxPar cw
                StageFronted fr -> do
                  emit build (FrontFinished tDone (Just fr))
                  case frBack fr of
                    EagerBack job -> emit build (BackReady job)
                    _             -> return ()
                  let finished2 = M.insert keyDone (frHashes fr) finished
                      parsedTasks2 = M.delete keyDone parsedTasks
                      envAcc' = Acton.Env.addModuleInfo (tkMod keyDone) (frModuleInfo fr) envAcc
                      interestMap2 = addInterestDeps (frInterest fr) interestMap
                      deferredBacks1 =
                        case frBack fr of
                          DeferredBack deferred -> M.insert keyDone (deferred, strictDependents modules keyDone) deferredBacks
                          _                     -> deferredBacks
                  flushRes <- flushReadyDeferredBacks envAcc' interestMap2 finished2 deferredBacks1
                  case flushRes of
                    Left err -> return (Left err)
                    Right deferredBacks2 ->
                      loop runningRef rdy2 running2 finished2 parsedTasks2 interestMap2 deferredBacks2 ind2 pend2 envAcc' hadErrors maxPar cw


-- | Check whether a NameInfo represents a root-eligible actor.
-- Used to decide which roots to include in .tydb headers and root generation.
rootEligible :: I.NameInfo -> Bool
rootEligible (I.NAct [] p k _ _) = case (p,k) of
                                      (A.TNil{}, A.TRow _ _ _ t _ A.TNil{}) ->
                                        prstr t == "Env" || prstr t == "None" ||
                                        prstr t == "__builtin__.Env" || prstr t == "__builtin__.None"
                                      _ -> False
rootEligible _ = False
