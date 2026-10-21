{-# LANGUAGE CPP, FlexibleInstances, ScopedTypeVariables #-}
{-|
The compilation pipeline that the acton CLI and the LSP server share.  It
plans which modules a build compiles, runs their front passes (parse,
kinds, types, escape analysis, hashing), which write the .tydb interface
files and report diagnostics, and runs their back passes (normalizer
through code generation) as separate jobs.  The project model, Build.act
and paths live in Acton.Project, dependency fetching in Acton.Fetch and
graph traversals in Acton.Graph.

A back pass finds no error in a module's source.  It fails only on a
compiler bug or a failed write, which the run reports as a
BackPassFailure.  A module depends on another module only through that
module's front result.  So a module's dependents start as soon as its
front stage finishes while its back passes run in the background, and the
LSP server reports a module as checked before its code is generated.

A build is fast when it does no work twice.  A module's .tydb records what
its last compilation used: the hash and the stat of its source, its
public interface hash (pub) and implementation hash (impl), the pub and
impl hash of each of its names with the names they depend on, and, for
every module it depends on, that module's hashes and the hashes and users
of the names it uses.  A name's pub hash covers its signature without
docstrings, its escape summary and the pub hashes of the names its
signature uses.  Its impl hash covers its implementation and the impl
hashes of the names it uses.  Acton.Hashing defines both.  The first line
of a module's generated .c, .h and closure support file is tagged with its
impl hash, or, for a deferred back job, with a hash of its impl hash and
its selected names.  The test result cache keys each test on its name's
impl hash.

Plan
----

prepareCompileContext finds the paths of the file that roots a build and
makes the --dep overrides absolute.  prepareCompilePlan fetches the
dependencies on a scheduler's first plan and whenever Build.act changed
since its previous plan.  It discovers the projects (buildProjects) and
prunes the outputs of their deleted sources, so that a cached header
cannot bring a deleted module back.  It then discovers their modules
(buildGlobalTasks) and selects the modules to compile (selectPlan): the
requested modules with everything they import or, for an incremental
plan, the modules that the changed files affect.  A declared build
library is compiled whole.  The LSP server plans an edit of a project it
already discovered by reading the edited module again (readModuleTask)
and selecting from the modules it keeps.

Discovery starts from the requested modules, or from every module when the
selection may need any of them, and goes breadth first.  It reads each
module in the cheapest form a build can use (readModuleTask), checks its
imports against the import policy and queues the modules that provide
them.  A discovered module is a CompileTask:

  TyTask          its .tydb header, trusted while its source is unchanged
  ParseTask       its source, with only the import header parsed
  ActonTask       its parsed source
  ParseErrorTask  the diagnostics of a source that does not parse

A header can be trusted when the .tydb exists, its header reads and the
compiler binary is not newer than the .tydb (--ignore-compiler-version
skips that check).  It is trusted without reading the source when the
source's stat (device, inode, size, mtime, ctime) equals the recorded one
and its mtime is strictly older than the .tydb's.  Strictly, because on a
file system with coarse timestamps a changed source and the .tydb written
from its previous version can show the same time.  Otherwise it is
trusted when the content hash of the source equals the recorded one, and
the recorded stat is then refreshed in place.  An editor overlay is
compared by content hash only.  A module whose header is not trusted is
read from its source.

Run
---

runCompilePlan compiles a plan as one generation of a CompileScheduler.
compileModules rejects import cycles, compiles __builtin__, whose
interface is the base environment of every other module, and then runs a
stage graph over the import graph: a parse stage for each other
ParseTask, and a front stage for every module, which starts after its
parse stage and its providers' front stages.  Up to the build's
parallelism (--jobs, else one per capability) stages run at once: front
stages first, then the stage with the heaviest chain of dependents by
source size.  When a front stage fails, every module depending on it is
dropped.  The run returns the environment of the checked modules, which
the LSP server completes names from, and whether any module failed.  An
import cycle, a failure of __builtin__, a failed output and a failed
preparation of a deferred back job fail the run itself (CompileFailure).

A module's front stage decides what the module needs (assess), reports why
under --verbose (describe) and does it (perform).  A source that changed
runs the front passes (compileModule).  A TyTask is checked in this order:

  1. An import whose interface is gone fails the module.
  2. Its dependency rows are compared with the current hashes.  A changed
     pub hash, a missing hash or a changed module without name rows runs
     the front passes, and so does a forced module: the requested module
     under alternative output, or any module under --always-build.
  3. A changed impl hash alone computes the impl hashes again from the
     stored rows (refreshImpl), or runs the front passes when a hash it
     needs is unavailable.
  4. A module whose code is generated whole, in a build without
     alternative output, compares the tags of its .c, .h and closure
     support file with its impl hash, and generates its code again from
     the stored typed module when they differ (refreshCodegen).
  5. Otherwise the .tydb is used as it is (reuse).

Under --only-build, every module's stored interface is trusted and no
front pass runs.

A front result carries the module's back work.  An eager BackJob is queued
as soon as the front stage finishes.  A deferred back job (DBP) generates
only the module's roots and the names that other modules use, so it waits
for the modules depending on it.  A module's back passes are deferred
unless --no-dbp, --only-build or alternative output is given, the module
is __builtin__, a module outside its build library imports it, or it has
NotImplemented bodies, whose code a hand-written .ext.c provides.

Events
------

A run reports everything it does as BuildEvents through one sink, the emit
field of its BuildEnv.  runCompilePlan's sink records analytics, queues
the job of a BackReady event on the scheduler's back queue, and then calls
the caller's handler while the run's generation is current.  BuildEvent
states the order of a module's events.

Generations
-----------

A CompileScheduler numbers its runs.  acton watch and the LSP server start
a run for each change with startCompile, the LSP server after a debounce
delay: the counter moves, the previous run is cancelled and awaited, and
the new run starts unless a newer one began during the delay.  A one-shot
build runs as the scheduler's current generation, 0 on a fresh scheduler.
A run whose generation is no longer current is stale.  A stale run writes
no front output, queues no back job and reports no event to the handler,
and its queued back jobs neither write their code nor report their end.
A new generation cancels the whole previous run, including the stages of
modules that the change does not affect.

Outputs
-------

A module's .tydb is written in its front stage (commitTydb).  Its
documentation page and its --tydb copy are written in the background
(startOutputs), and the run waits for them (awaitOutputs).

Back queue
----------

The scheduler's workers run the back jobs of every generation, one job at
a time each.  waitBackJobs waits for a generation's jobs, or returns its
first failure.  The CLI waits before its Zig build.  The LSP server
reports completion first and waits after.  A run outside any scheduler
(runStandalonePlan for acton sig, standaloneBuild for the benchmarks) has
no back queue: its BackReady events reach only its handler.

Module cache
------------

The scheduler owns one ModuleCache for its lifetime, and every run shares
it: the .tydb path, the module hashes and the public name hashes of each
module that a run read from disk or finished, and the single names it
read.  It is keyed by module name, so the trees one scheduler builds
share the entries of every module name they have in common.  A module
that a run finishes replaces its own hashes before it reports done, so
every later read sees them.  A standalone build has a cache of its own.

Invariants
----------

The .tydb commit is synchronous.  A module's .tydb is on disk before its
front stage reports done, so its readers never wait for a write: the base
environment reads __builtin__'s, a deferred back job selects its module's
statements from it, the caller reads roots and tests from it after the
run, and later plans trust its header.  A stale generation writes nothing
and still returns the module's result.  A failed write fails the module
with a diagnostic.  An impl refresh reads everything before it writes, so
its fallback to the front passes never runs on top of a committed
refresh.

Outputs are awaited and never cancelled.  Whatever ends a run,
compileModules first cancels the running stages and then waits for every
background output that any stage started.  A stage starts and records its
outputs under one mask, so a cancelled stage cannot leave a write that the
run does not wait for.  startCompile's cancel waits for this, so a stale
write never overlaps the next generation.  A failed output fails a run
that otherwise succeeded.

Deferred jobs wait for their dependents.  A deferred back job is prepared
only when every module that depends on its module, directly or not, has
finished its front stage in the run.  Only then is the interest in its
names complete, and the interest decides the selected names and the
codegen hash.  When a dependent fails, the job never becomes due, and the
run ends with errors.

A front stage reads a snapshot.  Its ModuleEnv is the coordinator's state
when the stage starts: the hashes of the finished modules, the environment
of the checked modules and its parse stage's result.  That state holds
everything the stage reads from the run, because the stage became ready
only after each of its providers' front stages completed, and the
coordinator folds a completion into its state before it starts the next
stages.  Modules outside the run are read through the module cache.

Handlers need no generation check, and helpers outside them do.
runCompilePlan calls the handler only while the run's generation is
current, so the handler and the helpers that only it calls do not check
the generation.  A helper that the caller also calls outside the handler,
before or after the run, checks the generation itself (whenCurrent), and
so does the work the caller does after the run, such as its Zig build.
-}
module Acton.Compile
  ( TaskKey(..)
  , GlobalTask(..)
  , taskSourceFile
  , CompileTask(..)
  , importsOf
  , ModuleSource(..)
  , BackJob(..)
  , DeferredBackJob
  , BackPlan(..)
  , ModuleHashes(..)
  , FrontResult(..)
  , CompileFailure(..)
  , compileFailureMessage
  , BackPassFailure(..)
  , backPassFailureMessage
  , BackJobResult(..)
  , BackPass(..)
  , backPassName
  , BackPassProgress(..)
  , BackPassEvent(..)
  , FrontPass(..)
  , FrontPassProgress(..)
  , ParseProgress(..)
  , TypeStmtTiming(..)
  , InferredSignature(..)
  , FrontOutputKind(..)
  , FrontTiming(..)
  , BackTiming
  , BuildEvent(..)
  , BuildEnv
  , quiet
  , Generation
  , generationNumber
  , isCurrent
  , ifCurrent
  , whenCurrent
  , currentGeneration
  , CompileScheduler
  , newCompileScheduler
  , compileParallelism
  , startCompile
  , stopCompile
  , waitBackJobs
  , runCompilePlan
  , standaloneBuild
  , runStandalonePlan
  , CompileContext(..)
  , hasParseStage
  , prepareCompileContext
  , Scope(..)
  , PlanRequest(..)
  , CompilePlan(..)
  , prepareCompilePlan
  , planFetched
  , selectPlan
  , readModuleTask
  , docIndexEntry
  , parseActSource
  , parseActFile
  , awaitOutputs
  , checkModule
  , rootEligible
  , docNameCountThreshold
  , shouldGenerateDocOutput
  , runBackPasses
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
import qualified Data.List
import Data.IORef
import qualified Data.Map as M
import Data.Ord (Down(..))
import qualified Data.Sequence as Seq
import qualified Data.Set
import Error.Diagnose (Diagnostic)
import GHC.Conc (getNumCapabilities)
import System.Clock
import System.Directory
import System.Environment (getExecutablePath)
import System.FilePath.Posix
import System.IO
import System.Posix.Files (deviceID, fileID, fileSize, getFileStatus, modificationTimeHiRes, statusChangeTimeHiRes)
import Text.Show.Pretty (ppDoc)

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import qualified Crypto.Hash.SHA256 as SHA256


-- Tasks, results and events ------------------------------------------------------------------------

-- | A module of a build: its project's root directory and its name, which
-- starts with the project's name except in the special projects.
data TaskKey = TaskKey { tkProj :: FilePath, tkMod :: A.ModName } deriving (Eq, Ord, Show)

isBuiltin                               :: TaskKey -> Bool
isBuiltin k                             = tkMod k == mBuiltin

-- | A module as failure messages name it: its project's root directory, a
-- slash, and its dotted name.
taskKeyText                             :: TaskKey -> String
taskKeyText k                           = tkProj k ++ "/" ++ modNameToString (tkMod k)

-- | A discovered module: its key, its project's paths, the form discovery
-- read it in and the in-graph providers of its imports.
data GlobalTask = GlobalTask
  { gtKey             :: TaskKey
  , gtPaths           :: Paths
  , gtTask            :: CompileTask
  , gtImportProviders :: M.Map A.ModName TaskKey   -- resolved in-graph providers (if any) for imports
  }

taskSourceFile                          :: GlobalTask -> FilePath
taskSourceFile t                        = srcFile (gtPaths t) (tkMod (gtKey t))

-- | A module as discovery found it.
data CompileTask
  = TyTask         InterfaceFiles.HeaderSummary              -- a .tydb header trusted while the source is unchanged
  | ParseTask      ModuleSource [A.ModName] (Maybe String)   -- header imports and docstring
  | ActonTask      ModuleSource A.Module                     -- parsed; imports as written until the front stage adjusts them
  | ParseErrorTask [Diagnostic String]

-- | The modules a task imports: a cached header's imports are canonical, a
-- source's as written.
importsOf                               :: CompileTask -> [A.ModName]
importsOf (TyTask h)                    = map fst (InterfaceFiles.summaryImports h)
importsOf (ParseTask _ imps _)          = imps
importsOf (ActonTask _ m)               = A.importsOf m
importsOf (ParseErrorTask _)            = []

-- | The text and bytes of one read of a module's source (two handles for a
-- disk read): the text is parsed, the bytes are hashed, and the stat is
-- recorded in its .tydb.
data ModuleSource = ModuleSource
  { sourceText  :: String
  , sourceBytes :: B.ByteString
  , sourceMeta  :: Maybe InterfaceFiles.SourceFileMeta     -- Nothing for an editor overlay
  }

-- | Generating C for one module.
data BackJob = BackJob
  { bjKey         :: TaskKey
  , bjPaths       :: Paths
  , bjOpts        :: C.CompileOptions
  , bjTypeEnv     :: Acton.Env.Env0     -- protocols converted
  , bjModule      :: A.Module           -- typed; only the selected names for a deferred module
  , bjSource      :: String             -- for #line directives
  , bjCodegenHash :: B.ByteString       -- tagged into the first line of each generated file
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

-- | What importers read of a finished module.  The results of the front
-- passes, of an impl refresh and of a codegen refresh carry its public
-- names.  A module reused from its .tydb without a refresh, or given the
-- empty result under --only-build, carries no name hashes of its own.
data ModuleHashes = ModuleHashes
  { mhPub   :: B.ByteString
  , mhImpl  :: B.ByteString
  , mhNames :: M.Map A.Name InterfaceFiles.NameHashInfo   -- public names
  }

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

-- | Why a run could not compile its modules.
data CompileFailure
  = ImportCycles [[TaskKey]]                      -- the modules of each import cycle
  | BuiltinFailed                                 -- __builtin__ had errors, already reported as diagnostics
  | OutputFailed TaskKey FrontOutputKind String   -- the first failed output in start order, with its exception text
  | DeferredBackFailed String                     -- the exception text from preparing ready deferred back jobs
  deriving (Eq, Show)

-- | The text a user sees for each failure.
compileFailureMessage                   :: CompileFailure -> String
compileFailureMessage (ImportCycles cs) = "Cyclic imports: " ++ concat [ '\n' : concat [ tkProj k ++ ":" ++ modNameToString (tkMod k) ++ " " | k <- c ] | c <- cs ]
compileFailureMessage BuiltinFailed     = "Builtin compilation failed"
compileFailureMessage (OutputFailed k kind msg)
                                        = "Front output job failed for " ++ taskKeyText k ++ " (" ++ frontOutputKindName kind ++ "): " ++ msg
compileFailureMessage (DeferredBackFailed msg)
                                        = msg

-- | A back job that threw: its module and the exception's text.
data BackPassFailure = BackPassFailure
  { bpfKey     :: TaskKey
  , bpfMessage :: String
  } deriving (Eq, Show)

-- | The text a user sees for a failed back job.
backPassFailureMessage :: BackPassFailure -> String
backPassFailureMessage (BackPassFailure key msg) =
  "Back pass failed for " ++ taskKeyText key ++ ": " ++ msg

-- | How a back job ended: BackJobOk with its time, Nothing when quiet, and
-- each pass's time, only with --timing, or BackJobFailed.
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

-- | The name timing lines and analytics give a back pass.
backPassName :: BackPass -> String
backPassName BackPassNormalize  = "normalize"
backPassName BackPassDeactorize = "deactorize"
backPassName BackPassCPS        = "cps"
backPassName BackPassLLift      = "llift"
backPassName BackPassBoxing     = "boxing"
backPassName BackPassCodeGen    = "codegen"
backPassName BackPassRender     = "render"
backPassName BackPassWrite      = "write"

-- | A back pass of a running job started, finished, or was skipped.
data BackPassProgress = BackPassProgress
  { bppPass      :: BackPass
  , bppCompleted :: Int                 -- the pass's index when it starts, index + 1 after
  , bppTotal     :: Int                 -- 8 when the job writes files, 7 otherwise
  , bppEvent     :: BackPassEvent
  } deriving (Eq, Show)

-- | A pass started, finished in the given time, or was skipped: the write
-- of a job that may no longer write, such as one whose generation went
-- stale.
data BackPassEvent = BackPassStarted | BackPassFinished TimeSpec | BackPassSkipped
  deriving (Eq, Show)

-- | The front passes that report their progress.
data FrontPass
  = FrontPassKinds
  | FrontPassTypes
  | FrontPassHash
  deriving (Eq, Show)

-- | A front pass's progress: fppCompleted of fppTotal in the pass's own
-- units.  The kinds check reports 0 and then 1 of 1, the type check the
-- names bound by the top-level statements it finished, and hashing its
-- work items.
data FrontPassProgress = FrontPassProgress
  { fppPass      :: FrontPass
  , fppCompleted :: Int
  , fppTotal     :: Int
  , fppCurrent   :: Maybe String        -- a description of the statements the type check has in progress, Nothing when there are none
  } deriving (Eq, Show)

-- | The parser's progress: ppCompleted of ppTotal characters of the source
-- text.
data ParseProgress = ParseProgress
  { ppCompleted :: Int
  , ppTotal     :: Int
  } deriving (Eq, Show)

-- | One step of the type check, collected with --timing and --verbose: the
-- time from a progress event with statements in progress to the next
-- event.
data TypeStmtTiming = TypeStmtTiming
  { tstCompleted :: Int                 -- the type check's progress at the next event
  , tstTotal     :: Int                 -- its total
  , tstNames     :: [String]            -- the names the statements in progress bind
  , tstTime      :: TimeSpec
  } deriving (Eq, Show)

-- | The signature the type checker inferred for the names of a top-level
-- statement without a complete signature, collected with --timing or
-- --verbose.
data InferredSignature = InferredSignature
  { isigNames     :: [String]
  , isigSignature :: String
  } deriving (Eq, Show)

-- | A module's front outputs: its .tydb, the --tydb copy next to its source
-- and its documentation page.
data FrontOutputKind
  = FrontOutputTydb
  | FrontOutputTydbCopy
  | FrontOutputDoc
  deriving (Eq, Ord, Show)

frontOutputKindName                     :: FrontOutputKind -> String
frontOutputKindName FrontOutputTydb     = "tydb"
frontOutputKindName FrontOutputTydbCopy = "tydb-copy"
frontOutputKindName FrontOutputDoc      = "doc"

-- | Where a module's front passes spent their time, collected with
-- --timing.
data FrontTiming = FrontTiming
  { ftEnv               :: TimeSpec             -- the parse dumps and building the environment the module is checked in
  , ftKinds             :: TimeSpec             -- the kinds check and its dump
  , ftTypes             :: TimeSpec             -- from the end of the kinds check to the end of the type dumps
  , ftTypeReconstruct   :: TimeSpec             -- the type check
  , ftTypeAfterProgress :: TimeSpec             -- the type check after its progress first reached its total, 0 when it never did
  , ftTypeForce         :: TimeSpec             -- forcing the type check's results
  , ftTypeEscape        :: TimeSpec             -- the escape analysis
  , ftTypeHash          :: TimeSpec             -- hashing
  , ftTypeStmtTimings   :: [TypeStmtTiming]     -- with --verbose too
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


-- Build environment and generations ----------------------------------------------------------------

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

-- | Whether a build reports no per-module times: under --quiet and under
-- every alternative output.  The CLI then prints no done or timing lines.
quiet :: C.GlobalOptions -> C.CompileOptions -> Bool
quiet gopts opts = C.quiet gopts || C.altOutput opts

-- | A module's time as its build reports it: Nothing when the build is quiet.
reportedTime :: C.GlobalOptions -> C.CompileOptions -> TimeSpec -> Maybe TimeSpec
reportedTime gopts opts time = if quiet gopts opts then Nothing else Just time

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

-- | The action's result if the generation is current when it starts, else
-- the given value.  The action is not stopped if the generation goes stale.
ifCurrent :: Generation -> a -> IO a -> IO a
ifCurrent gen stale action = isCurrent gen >>= \current -> if current then action else return stale

whenCurrent :: Generation -> IO () -> IO ()
whenCurrent gen = ifCurrent gen ()

-- | The generation a scheduler is at, which a one-shot build runs as. A
-- fresh scheduler is at generation 0, and it stays current until a run is
-- started with startCompile.
currentGeneration :: CompileScheduler -> IO Generation
currentGeneration sched = do
  number <- readIORef (schedulerCounter sched)
  return (Generation number (schedulerCounter sched))


-- Scheduler, back queue and entry points -----------------------------------------------------------

-- | A long-lived compiler. The back passes of every generation share one
-- pool of workers, and every run shares one module cache, so the interface
-- hashes a run reads from disk or finishes are remembered for the
-- scheduler's lifetime. The cache is keyed by module name, so it cannot
-- tell two source trees apart. Two trees that hold modules of the same
-- name, such as two checkouts of one project, need schedulers of their
-- own. Within one tree, a module that a run finishes replaces its own
-- entries.
data CompileScheduler = CompileScheduler
  { schedulerCounter   :: IORef Int                     -- the newest generation
  , schedulerRun       :: MVar (Maybe (Async ()))       -- the newest generation's run
  , schedulerBackQueue :: BackQueue
  , schedulerBuildSpec :: IORef (Maybe BuildSpecStamp)  -- Build.act at the last plan
  , schedulerCache     :: ModuleCache
  }

-- | Back jobs waiting for a worker, with the state waitBackJobs reads.
data BackQueue = BackQueue
  { queuedJobs    :: TChan QueuedJob
  , pendingCounts :: TVar (M.Map Int Int)               -- per generation: queued or running jobs
  , firstFailures :: TVar (M.Map Int BackPassFailure)   -- per generation: the first failure
  }

-- | A back job with its generation and the sink its events go to.
data QueuedJob = QueuedJob Generation (BuildEvent -> IO ()) BackJob

-- | Create a scheduler at generation 0 with an empty module cache. Its back
-- passes run on one worker per compileParallelism gopts, with the given
-- global options, which decide whether back passes report their time and
-- timing.
newCompileScheduler :: C.GlobalOptions -> IO CompileScheduler
newCompileScheduler gopts = do
  workers <- compileParallelism gopts
  counter <- newIORef 0
  queue <- BackQueue <$> newTChanIO <*> newTVarIO M.empty <*> newTVarIO M.empty
  replicateM_ workers (forkIO (backWorker gopts queue))
  CompileScheduler counter <$> newMVar Nothing <*> pure queue <*> newIORef Nothing <*> newModuleCache

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
-- worker without lowering that job's pending count. Otherwise the pending
-- count falls after every job, whether it ran or was dropped.
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
  compileModules (BuildEnv sp gopts gen events (schedulerCache sched) outputs) plan

-- | The services of a run outside any scheduler: a generation that stays
-- current, a module cache of its own, no back queue, so BackReady reaches
-- only the handler, and a record of its own background outputs, which
-- awaitOutputs waits for.
standaloneBuild :: Source.SourceProvider -> C.GlobalOptions -> (BuildEvent -> IO ()) -> IO BuildEnv
standaloneBuild sp gopts onEvent = do
  counter <- newIORef 0
  cache <- newModuleCache
  outputs <- OutputJobs <$> newIORef []
  return (BuildEnv sp gopts (Generation 0 counter) onEvent cache outputs)

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


-- Planning and selection ---------------------------------------------------------------------------

-- | What every plan of a build shares.
data CompileContext = CompileContext
  { ccOpts       :: C.CompileOptions     -- --dep overrides made absolute
  , ccPathsRoot  :: Paths                -- the paths of the file that roots the build
  , ccRootModule :: A.ModName            -- the module of that file, whose alternative output a build forces
  , ccRootProj   :: FilePath             -- the root project's directory, normalized
  , ccSysAbs     :: FilePath             -- the system directory, normalized
  , ccBuildStamp :: BuildSpecStamp       -- the root project's Build.act when the context was made
  }

-- | A module's options: dependency projects build neither their tests nor a
-- final binary.
optionsFor                              :: CompileContext -> TaskKey -> C.CompileOptions
optionsFor ctx k
  | tkProj k == ccRootProj ctx          = ccOpts ctx
  | otherwise                           = (ccOpts ctx) { C.skip_build = True, C.test = False }

-- | Whether a module is parsed in a stage of its own: a source module is,
-- except __builtin__, which is parsed where it is compiled, and except under
-- --only-build, which compiles no .act file.
hasParseStage                           :: CompileContext -> GlobalTask -> Bool
hasParseStage ctx t                     = case gtTask t of
                                            ParseTask{} -> not (isBuiltin (gtKey t)) && not (C.only_build (optionsFor ctx (gtKey t)))
                                            _           -> False

-- | The context of a build of the given files, rooted at the first.  The
-- --dep overrides are made absolute against the first file's project
-- directory, or the working directory outside a project, before the paths
-- are found, so that the paths and discovery see absolute overrides.  The
-- Build.act stamp is read at the project path as the paths give it.
prepareCompileContext :: C.CompileOptions -> [FilePath] -> IO CompileContext
prepareCompileContext _ []              = throwProjectError "No source files found"
prepareCompileContext opts (first : _)  = do
    cwd         <- getCurrentDirectory
    base        <- fromMaybe cwd <$> findProjectDir (takeDirectory first)
    overrides   <- normalizeDepOverrides base (C.dep_overrides opts)
    let opts'   = opts { C.dep_overrides = overrides }
    (paths, mn) <- findPaths first opts'
    root        <- normalizePathSafe (projPath paths)
    sys         <- normalizePathSafe (sysPath paths)
    stamp       <- readBuildSpecStamp (projPath paths)
    return CompileContext { ccOpts = opts', ccPathsRoot = paths, ccRootModule = mn, ccRootProj = root, ccSysAbs = sys, ccBuildStamp = stamp }

-- | How much of a project a build asks for.  A whole-project build seeds
-- discovery with every module and lets the final build prune stale
-- outputs.
data Scope = SelectedFiles | WholeProject deriving (Eq)

-- | What a build asks for.
data PlanRequest = PlanRequest
  { requestFiles   :: [FilePath]          -- named by the user or the editor
  , requestScope   :: Scope
  , requestChanged :: Maybe [FilePath]    -- changed since the previous plan: an incremental plan
  }

data CompilePlan = CompilePlan
  { cpContext     :: CompileContext
  , cpRequest     :: PlanRequest              -- as planned: a Build.act change drops requestChanged
  , cpProjMap     :: M.Map FilePath ProjCtx
  , cpGlobalTasks :: [GlobalTask]             -- every discovered module, in discovery order
  , cpNeededTasks :: [GlobalTask]             -- the modules to compile, closed under imports, in discovery order
  , cpDbpBlocked  :: Data.Set.Set TaskKey     -- library members used from outside their library
  , cpRootModules :: [A.ModName]              -- selected root-project modules, before library expansion
  , cpAllowPrune  :: Bool                     -- a whole-project, non-incremental plan
  , cpSourceSizes :: M.Map TaskKey Integer    -- each needed module's source size, 0 when missing
  }

-- | Plan a build.  A scheduler's first plan, and any plan after Build.act
-- changed, fetches dependencies and plans in full: the request's changed
-- paths are dropped.  The stamp is recorded before fetching.
prepareCompilePlan :: Source.SourceProvider -> C.GlobalOptions -> CompileScheduler -> CompileContext -> PlanRequest -> IO CompilePlan
prepareCompilePlan sp gopts sched ctx request = do
    specChanged <- atomicModifyIORef' (schedulerBuildSpec sched) (\previous -> (stamp, previous /= stamp))
    when specChanged $
      fetchDependencies gopts (ccPathsRoot ctx) (C.dep_overrides (ccOpts ctx))
    planFetched sp gopts ctx (if specChanged then request { requestChanged = Nothing } else request)
  where
    stamp = Just (ccBuildStamp ctx)

-- | Plan a build whose dependencies are in place.  Orphan outputs are pruned
-- before discovery, so that a cached header cannot resurrect a deleted
-- module: in every project for a full plan, and for an incremental plan
-- only those of changed sources that no longer exist.
planFetched :: Source.SourceProvider -> C.GlobalOptions -> CompileContext -> PlanRequest -> IO CompilePlan
planFetched sp gopts ctx request = do
    projects <- buildProjects gopts ctx
    let paths     = buildPaths ctx projects
        withPaths = M.elems (M.intersectionWith (,) projects paths)
    case requestChanged request of
      Nothing      -> mapM_ (uncurry pruneMissingModuleOutputs) withPaths
      Just changed -> pruneMissingChangedModuleOutputs withPaths changed
    tasks    <- buildGlobalTasks sp gopts opts projects paths (seeds projects)
    selectPlan ctx projects tasks request
  where
    opts = ccOpts ctx
    -- Discovery starts from the requested files only when the selection
    -- needs no other module: an incremental plan looks for dependents, a
    -- whole-project plan compiles every module, and library expansion and
    -- library boundaries need the whole graph.
    seeds projects
      | isJust (requestChanged request)           = Nothing
      | requestScope request == WholeProject      = Nothing
      | any declaresLibraries (M.elems projects)  = Nothing
      | otherwise                                 = Just (requestFiles request)
    declaresLibraries = not . M.null . BuildSpec.libraries . projBuildSpec

-- | The projects of a build: one scratch project without dependencies for a
-- temporary build, else the root project and every project it depends on.
-- Tests are selected by their imports, so under --test the root project
-- declares no build libraries, whose members would pull unrelated test
-- modules into the build.
buildProjects :: C.GlobalOptions -> CompileContext -> IO (M.Map FilePath ProjCtx)
buildProjects gopts ctx = withoutTestLibraries <$> projects
  where
    opts  = ccOpts ctx
    paths = ccPathsRoot ctx
    root  = ccRootProj ctx
    sys   = ccSysAbs ctx
    projects
      | isTmp paths = return (M.singleton root scratch)
      | otherwise   = discoverProjects gopts sys root (C.dep_overrides opts)
    scratch = mkProjCtx sys root (srcDir paths) (scratchBuildSpec root) []
    withoutTestLibraries
      | C.test opts = M.adjust (\p -> p { projBuildSpec = (projBuildSpec p) { BuildSpec.libraries = M.empty } }) root
      | otherwise   = id

-- | The paths of each project of a build.  The one project of a scratch
-- build has the paths findPaths gave the file that roots the build.
buildPaths :: CompileContext -> M.Map FilePath ProjCtx -> M.Map FilePath Paths
buildPaths ctx projects
  | isTmp (ccPathsRoot ctx)     = ccPathsRoot ctx <$ projects
  | otherwise                   = M.map (projectPaths (ccOpts ctx) projects) projects

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
    , graphIndex     = index
    , graphImports   = importGraph tasks
    , graphLibraries = libraryTaskGroups projMap (M.keysSet index)
    }
  where index = M.fromList [ (gtKey t, t) | t <- tasks ]

-- | The graph's tasks whose keys are in the set, in discovery order.
restrictTasks :: ModuleGraph -> Data.Set.Set TaskKey -> [GlobalTask]
restrictTasks g keys = [ t | t <- graphTasks g, Data.Set.member (gtKey t) keys ]

-- | What a request compiles from the discovered modules of its projects:
-- the requested modules with what they import, or, for an incremental
-- request, the modules its changes affect, expanded by expandLibraries.
-- The root modules are the selected root-project modules before that
-- expansion.
selectPlan :: CompileContext -> M.Map FilePath ProjCtx -> [GlobalTask] -> PlanRequest -> IO CompilePlan
selectPlan ctx projects tasks request = do
    selected <- maybe (selectRequested ctx g (requestFiles request)) (selectAffected ctx g blocked) (requestChanged request)
    let needed = restrictTasks g (expandLibraries g selected)
    sizes    <- sourceSizes needed
    return CompilePlan
      { cpContext     = ctx
      , cpRequest     = request
      , cpProjMap     = projects
      , cpGlobalTasks = tasks
      , cpNeededTasks = needed
      , cpDbpBlocked  = blocked
      , cpRootModules = [ tkMod (gtKey t) | t <- restrictTasks g selected, tkProj (gtKey t) == ccRootProj ctx ]
      , cpAllowPrune  = requestScope request == WholeProject && isNothing (requestChanged request)
      , cpSourceSizes = sizes
      }
  where
    g       = moduleGraph projects tasks
    blocked = libraryBoundary g

-- | The requested modules with everything they import, and __builtin__.  A
-- file names a module by its source path, else by its module name in the
-- root project.  When no file names a module that way, every module whose
-- source has a requested file name is requested, in any project.  When
-- nothing is requested, every module is selected.
selectRequested :: CompileContext -> ModuleGraph -> [FilePath] -> IO (Data.Set.Set TaskKey)
selectRequested ctx g files = do
    named <- catMaybes <$> mapM lookupTaskKey files
    let requested = if null named then byFileName else named
        start     = if null requested then map gtKey (graphTasks g) else requested ++ builtinKeys
    return (closure (depsOf (graphImports g)) start)
  where
    paths       = ccPathsRoot ctx
    byFileName  = [ gtKey t | t <- graphTasks g, takeFileName (taskSourceFile t) `elem` map takeFileName files ]
    builtinKeys = [ gtKey t | t <- graphTasks g, isBuiltin (gtKey t) ]
    lookupTaskKey file = do
      absolute <- canonicalizePath file
      case [ gtKey t | t <- graphTasks g, taskSourceFile t == absolute ] of
        k : _ -> return (Just k)
        []    -> do mn <- moduleNameFromFile (srcDir paths) (projName paths) absolute
                    return (gtKey <$> M.lookup (TaskKey (ccRootProj ctx) mn) (graphIndex g))

-- | The modules an edit of the changed files affects: every dependent of a
-- changed module, every module on a dependency path from those to a DBP
-- provider, and every dependent of the providers on those paths, because
-- every consumer of a deferred provider can contribute selected-name
-- interest.  Changes that match no module select everything.  Safe on
-- import cycles, which the run reports later.  The selected tasks keep
-- their full provider maps, so unchanged imports still resolve through
-- cached interfaces.
selectAffected :: CompileContext -> ModuleGraph -> Data.Set.Set TaskKey -> [FilePath] -> IO (Data.Set.Set TaskKey)
selectAffected ctx g blocked files
  | null files = return everything
  | otherwise  = do
      edited  <- mapM normalizePathSafe files
      sources <- M.fromList <$> forM (graphTasks g) (\t -> (\path -> (path, gtKey t)) <$> normalizePathSafe (taskSourceFile t))
      return $ case mapMaybe (`M.lookup` sources) edited of
        []      -> everything
        changed -> affectedBy (graphImports g) isDbpProvider changed
  where
    everything = M.keysSet (graphIndex g)
    isDbpProvider k =
      case gtTask t of
        TyTask _ -> deferrable (optionsFor ctx k) (Data.Set.member k blocked) False k
        _        -> False
      where t = graphIndex g M.! k

-- | DBP may prune generated C/H for internal library modules, but a module at
-- an explicit library boundary must keep its full generated surface.  A
-- member is at the boundary when a module outside its library imports it, so
-- members nobody imports stay DBP-eligible.
libraryBoundary :: ModuleGraph -> Data.Set.Set TaskKey
libraryBoundary g =
    Data.Set.unions [ Data.Set.filter (importedOutside group) group | group <- graphLibraries g ]
  where importedOutside group = any (`Data.Set.notMember` group) . usersOf (graphImports g)

-- | The modules of each declared build library: the discovered modules of
-- the declaring project that the library names.  A named module that
-- discovery did not reach is left out, so every key has a task.
libraryTaskGroups :: M.Map FilePath ProjCtx -> Data.Set.Set TaskKey -> [Data.Set.Set TaskKey]
libraryTaskGroups projMap discovered =
    [ Data.Set.intersection discovered (Data.Set.fromList (map (libraryKey ctx) (BuildSpec.libModules lib)))
    | ctx <- M.elems projMap
    , lib <- M.elems (BuildSpec.libraries (projBuildSpec ctx))
    ]
  where
    libraryKey ctx name = TaskKey (projRoot ctx) (A.dottedModName (BuildSpec.specName (projBuildSpec ctx) ++ "." ++ name))

-- | Declared build libraries are artifact units: the selection grows to the
-- least superset of the keys that is closed under imports and in which a
-- selected member of a library selects every module of that library.
expandLibraries :: ModuleGraph -> Data.Set.Set TaskKey -> Data.Set.Set TaskKey
expandLibraries g = closure next . Data.Set.toList
  where
    next k = depsOf (graphImports g) k
             ++ [ member | library <- graphLibraries g, Data.Set.member k library, member <- Data.Set.toList library ]

-- | Each module's source size in bytes, 0 when the source is missing: a
-- module's weight in scheduling and in progress reports.
sourceSizes :: [GlobalTask] -> IO (M.Map TaskKey Integer)
sourceSizes tasks = M.fromList <$> forM tasks (\t -> (,) (gtKey t) <$> sourceSize (taskSourceFile t))
  where sourceSize file = do exists <- doesFileExist file
                             if exists then getFileSize file else return 0


-- Discovery and parsing ----------------------------------------------------------------------------

-- | The imports discovery checks and resolves: a source's as written, and
-- a cached header's canonical imports with the module's own project prefix
-- dropped.
restoredImportsOf                       :: Paths -> CompileTask -> [A.ModName]
restoredImportsOf paths (TyTask h)      = map (dropProjPrefix paths . fst) (InterfaceFiles.summaryImports h)
restoredImportsOf _ task                = importsOf task

-- | The declared modules of a plan's projects, which discovery reads and
-- resolves imports against.
data ModuleIndex = ModuleIndex
  { indexProjects :: M.Map FilePath ProjCtx     -- every project, by its root
  , indexSources  :: M.Map TaskKey FilePath     -- every declared module and its source file
  }

-- | Whether a project declares a module.
declares                                :: ModuleIndex -> FilePath -> A.ModName -> Bool
declares index project mn               = M.member (TaskKey project mn) (indexSources index)

-- | The dependencies that a module's own project declares, by name and
-- root, in name order.  A module's paths are those of its project, so
-- their projPath is a project root of the index.
directDependencies                      :: ModuleIndex -> Paths -> [(String, FilePath)]
directDependencies index paths          = projDeps (indexProjects index M.! projPath paths)

-- | Whether a cached canonical import names a namespace that the module's
-- own project now declares: the import itself, or, for a dependency's lib
-- module, the dependency's bare name.
overlapsLocal                           :: ModuleIndex -> Paths -> A.ModName -> Bool
overlapsLocal index paths mn            = case A.modPath mn of
                                            n:ns | n /= projName paths
                                              -> any (declares index (projPath paths))
                                                     (addProjPrefix paths mn : [ addProjPrefix paths (A.modName [n]) | ns == ["lib"] ])
                                            _ -> False

-- | Imports name the modules of their own project without the project's
-- prefix.
checkImportPrefixes                     :: ModuleIndex -> Paths -> [A.ModName] -> Either String ()
checkImportPrefixes index paths imports = unless (isSpecialProject (projName paths) || null prefixed) $
                                            Left ("Do not use prefix '" ++ projName paths ++ "' when referring to modules in that project (" ++ prstrs prefixed ++ ")")
  where prefixed                        = filter (declares index (projPath paths)) imports

-- | The source imports of a module may name only local modules or directly
-- declared projects.  Transitive interfaces remain available for types
-- mentioned by imported APIs.  The first offending import is reported.
checkImportDependencies :: ModuleIndex -> Paths -> A.ModName -> [A.ModName] -> Either String ()
checkImportDependencies index paths importer = mapM_ check
  where
    check mn = case A.modPath mn of
        n:_ | Data.Set.member n projectNames
            , n `notElem` directNames
            , not (declares index (projPath paths) (addProjPrefix paths mn)) ->
          Left ("Dependency '" ++ n ++ "' imported by " ++ srcFile paths importer
                ++ " is not declared in " ++ projPath paths ++ "/Build.act.\n"
                ++ "Add a direct dependency, using follows to share another dependency's selection.")
        _ -> Right ()
    projects     = indexProjects index
    projectNames = Data.Set.fromList [ BuildSpec.specName (projBuildSpec ctx) | ctx <- M.elems projects ]
    directNames  = "std" : [ BuildSpec.specName (projBuildSpec ctx)
                           | (_, root) <- directDependencies index paths
                           , Just ctx <- [M.lookup root projects] ]

-- | The in-graph provider of each import, keyed by its spellings: the
-- import itself, or the dependency's name for an import of a declared
-- dependency's lib module, and, for an import that starts with the
-- module's project name, the import without that name.  Only spellings
-- that a module of the build provides are keys, so a lookup by another
-- name, such as the canonical dep.lib name of a dependency import, misses.
-- A module of the importing project shadows every other provider.
-- Otherwise the importing project and then its direct dependencies, in
-- name order, are searched for the spelling and, when it is a bare
-- declared dependency name, for that dependency's lib module; transitive
-- projects and std are never searched.  When several modules match a
-- spelling, the last one found wins.
resolveProviders :: ModuleIndex -> Paths -> [A.ModName] -> M.Map A.ModName TaskKey
resolveProviders index paths imports = M.fromList (concatMap provide spellings)
  where
    declared      = directDependencies index paths
    dependencies  = map fst declared
    searchOrder   = projPath paths : map snd declared
    spellings     = [ A.modName ns | n:ns <- map A.modPath imports, n == projName paths ] ++ map dropLib imports
    provide mn
      | declares index (projPath paths) local
                  = [ (mn, TaskKey (projPath paths) local) ]
      | otherwise = [ (mn, TaskKey p name) | p <- searchOrder, name <- [mn, addLib mn], declares index p name ]
      where local = if isSpecialProject (projName paths)     -- in a special project, the spelling itself, locations included
                      then mn
                      else addProjPrefix paths mn
    addLib mn     = case A.modPath mn of
                      [n] | n `elem` dependencies       -> A.modName [n, "lib"]
                      _                                 -> mn
    dropLib mn    = case A.modPath mn of
                      [n, "lib"] | n `elem` dependencies -> A.modName [n]
                      _                                  -> mn

-- | The modules a build reaches, in the order discovery first reaches them:
-- breadth first, first in first out, from the seeds.  The seeds are the
-- modules of the given source files, or, when no file is given or none is a
-- declared module, every declared module in project order and then
-- enumeration order.  Each module is read in the cheapest form it can be
-- used in, its imports are checked against the import policy, and the
-- modules that provide them are queued.  Output directories are created for
-- every discovered module.
buildGlobalTasks :: Source.SourceProvider
                 -> C.GlobalOptions
                 -> C.CompileOptions
                 -> M.Map FilePath ProjCtx
                 -> M.Map FilePath Paths            -- each project's paths
                 -> Maybe [FilePath]                -- seed source files; Nothing seeds every module
                 -> IO [GlobalTask]
buildGlobalTasks sp gopts opts projects pathsByProject seedFiles = do
    modules <- concat <$> mapM projectModules (M.elems projects)
    seeds   <- maybe (return []) (mapM canonicalizePath) seedFiles
    let index    = ModuleIndex { indexProjects = projects, indexSources = M.fromList modules }
        bySource = M.fromList [ (file, key) | (key, file) <- modules ]
        start    = case mapMaybe (`M.lookup` bySource) seeds of
                     []   -> map fst modules
                     keys -> keys
    visit index Data.Set.empty (Seq.fromList start) []
  where
    projectModules ctx = map (\(file, mn) -> (TaskKey (projRoot ctx) mn, file)) <$> enumerateProjectModules ctx

    -- Every queued module is a declared one: seeds come from the index and
    -- resolveProviders yields declared modules only.
    visit index seen queue tasks = case queue of
        Seq.Empty                       -> return (reverse tasks)
        key Seq.:<| rest
          | Data.Set.member key seen    -> visit index seen rest tasks
          | otherwise                   -> do task <- discover index key
                                              let providers = Seq.fromList (M.elems (gtImportProviders task))
                                              visit index (Data.Set.insert key seen) (rest Seq.>< providers) (task : tasks)

    discover index key = do
        createModuleDirectories paths (tkMod key)
        discovered <- readModuleTask sp gopts opts paths (tkMod key) file
        -- A cached header stores canonical imports, which spell a bare
        -- dependency import as dep.lib.  When such a namespace has become
        -- local, the source is parsed again, so that its spelling resolves to
        -- the local module and its interface is rebuilt.
        task       <- case discovered of
                        TyTask _ | any (overlapsLocal index paths) (importsOf discovered)
                          -> either ParseErrorTask (uncurry ActonTask) <$> materialize sp opts (tkMod key) file Nothing discovered
                        _ -> return discovered
        let imports = restoredImportsOf paths task
        either throwProjectError return (checkImportPrefixes index paths imports >> checkImportDependencies index paths (tkMod key) imports)
        return GlobalTask { gtKey = key, gtPaths = paths, gtTask = task, gtImportProviders = resolveProviders index paths imports }
      where file  = indexSources index M.! key
            paths = pathsByProject M.! tkProj key

-- | A module in the cheapest form discovery can use.  Its .tydb header is
-- reused when the compiler binary is not newer than the .tydb and the source
-- is unchanged: by recorded stat with the source older than the .tydb, else by
-- content hash (then the recorded stat is refreshed in place).  An editor
-- overlay is compared by content only.  Otherwise only the source header is
-- parsed.  Reading can write: the stat refresh.
readModuleTask :: Source.SourceProvider -> C.GlobalOptions -> C.CompileOptions -> Paths -> A.ModName -> FilePath -> IO CompileTask
readModuleTask sp gopts opts paths mn file = do
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
    tydb = tyDbPath paths mn

    fromSource snap = do
      parsed <- parseSourceHeader mn file snap
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
        putStrLn ("[debug] readModuleTask " ++ modNameToString mn
                  ++ " act=" ++ show actTime
                  ++ " ty=" ++ show tyTime
                  ++ " hash=" ++ Hashing.shortHash hash
                  ++ " header=" ++ Hashing.shortHash (InterfaceFiles.summarySourceHash header)
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

-- | A module's documentation index entry: its name, its docstring and
-- whether it gets doc output.
docIndexEntry                           :: C.CompileOptions -> Paths -> A.ModName -> CompileTask -> Maybe (A.ModName, Maybe String, Bool)
docIndexEntry opts paths mn (TyTask h)  = Just (mn, InterfaceFiles.summaryDoc h,
                                                shouldGenerateDocOutput opts (isTmp paths) (InterfaceFiles.summaryNameCount h))
docIndexEntry _ _ mn (ParseTask _ _ d)  = Just (mn, d, True)
docIndexEntry _ _ _ _                   = Nothing

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
adjustImports providers m = m { A.imps = map adjust (A.imps m) }
  where adjust (A.Import l items)        = A.Import l (map adjustItem items)
        adjust (A.FromImport l mn names) = A.FromImport l (fromMaybe mn (providerName mn)) names
        adjust (A.FromImportAll l mn)    = A.FromImportAll l (fromMaybe mn (providerName mn))
        adjustItem item@(A.ModuleItem mn alias)
                                         = maybe item (\target -> A.ModuleItem target (Just (fromMaybe mn alias))) (providerName mn)
        -- The written name, with its locations, stays when the provider has
        -- the same name.
        providerName mn                  = case M.lookup mn providers of
                                             Just k | tkMod k /= mn -> Just (tkMod k)
                                             _                      -> Nothing

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
-- context and indentation errors the module's file.
parserHandlers :: A.ModName -> FilePath -> String -> [Handler [Diagnostic String]]
parserHandlers mn file src =
    [ Handler $ \(e :: ParseErrorBundle String CustomParseError) -> return [Diag.parseDiagnosticFromBundle file src e]
    , Handler $ \(e :: CustomParseException)                     -> return [Diag.customParseExceptionToDiagnostic file src e]
    , Handler $ \(e :: ContextError)                             -> return (errsToDiagnostics "Context error"     moduleFile src (Acton.Parser.contextError e))
    , Handler $ \(e :: IndentationError)                         -> return (errsToDiagnostics "Indentation error" moduleFile src (Acton.Parser.indentationError e)) ]
  where moduleFile = modNameToFilename mn

-- | Parse a module from source text.  A chunk scan error is a parse error at
-- the parsed file.
parseActSource :: C.CompileOptions -> A.ModName -> FilePath -> String -> Maybe (ParseProgress -> IO ()) -> IO (Either [Diagnostic String] A.Module)
parseActSource opts mn actFile srcContent mOnProgress =
    tryDiagnostics (chunkScanError : parserHandlers mn actFile srcContent)
                   (parseModule mn actFile srcContent (fmap wrapProgress mOnProgress))
  where
    parseModule
      | C.parse_serial opts             = Acton.Parser.parseModuleSerial
      | otherwise                       = Acton.Parser.parseModule
    wrapProgress onProgress completed total = onProgress (ParseProgress completed total)
    chunkScanError = Handler $ \(ChunkScanError loc msg) -> return (errsToDiagnostics "Parse error" actFile srcContent [(loc, msg)])

-- | Parse only the header of a module's source: the modules it imports and
-- its docstring.
parseSourceHeader :: A.ModName -> FilePath -> Source.SourceSnapshot -> IO (Either [Diagnostic String] ([A.ModName], Maybe String))
parseSourceHeader mn actFile snap = do
    display <- displayPath actFile
    tryDiagnostics (parserHandlers mn display src) (header display)
  where
    src = Source.ssText snap
    header display = do
      (imps, mdoc) <- Acton.Parser.parseModuleHeader display src
      return (A.importedModules imps, mdoc)

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


-- Module cache -------------------------------------------------------------------------------------

-- | What runs read from .tydb files or finished: each module's .tydb path,
-- its pub and impl hashes (read together) and complete public name map,
-- and single name reads.  Every table is read and written under its own
-- lock.  A hash or name table is always locked before the path table,
-- never after.
data ModuleCache = ModuleCache
  { tydbPathTable   :: MVar (M.Map A.ModName FilePath)                                       -- found paths only
  , moduleHashTable :: MVar (M.Map A.ModName (B.ByteString, B.ByteString))                   -- pub and impl, found answers only
  , nameMapTable    :: MVar (M.Map A.ModName (M.Map A.Name InterfaceFiles.NameHashInfo))     -- complete public maps
  , nameReadTable   :: MVar (M.Map (A.ModName, A.Name) (Maybe InterfaceFiles.NameHashInfo))  -- single reads, misses kept
  }

-- | The two hashes of a module or name: its public interface and its
-- implementation.
data HashKind = Pub | Impl deriving (Eq, Ord, Show)

hashKindName                            :: HashKind -> String
hashKindName Pub                        = "pub"
hashKindName Impl                       = "impl"

newModuleCache :: IO ModuleCache
newModuleCache = ModuleCache <$> newMVar M.empty <*> newMVar M.empty <*> newMVar M.empty <*> newMVar M.empty

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
lookupTydbPath cache searchPaths mn = modifyMVar (tydbPathTable cache) $ \known -> do
  normalizedSearch <- mapM normalizePathSafe searchPaths
  let refresh = do
        found <- Acton.Env.findTyFile searchPaths mn
        return (maybe (M.delete mn known) (\path -> M.insert mn path known) found, found)
  case M.lookup mn known of
    Just path -> do
      normalized <- normalizePathSafe path
      exists <- InterfaceFiles.interfaceExists normalized
      if exists && any (normalized `isUnder`) normalizedSearch
        then return (known, Just normalized)
        else refresh
    Nothing -> refresh

-- | Read from a module's .tydb on the search path, when it has one.
withTydb :: ModuleCache -> [FilePath] -> A.ModName -> (FilePath -> IO (Maybe a)) -> IO (Maybe a)
withTydb cache searchPaths m readTydb = lookupTydbPath cache searchPaths m >>= maybe (return Nothing) readTydb

-- | A module's current pub or impl hash.  Both come from one read of its
-- .tydb header, unless a finished module or an earlier read remembered them.
lookupModuleHash :: ModuleCache -> [FilePath] -> HashKind -> A.ModName -> IO (Maybe B.ByteString)
lookupModuleHash cache searchPaths kind m = fmap pick <$> memoFound (moduleHashTable cache) m
    (withTydb cache searchPaths m (fmap (fmap (\(_, pub, impl) -> (pub, impl))) . InterfaceFiles.readModuleHashesMaybe))
  where
    pick (pub, impl) = case kind of
      Pub  -> pub
      Impl -> impl

-- | A module's complete public name map, from its .tydb header unless a
-- finished module or an earlier read remembered it.
lookupNameMap :: ModuleCache -> [FilePath] -> A.ModName -> IO (Maybe (M.Map A.Name InterfaceFiles.NameHashInfo))
lookupNameMap cache searchPaths m = memoFound (nameMapTable cache) m $
    withTydb cache searchPaths m (fmap (fmap (publicNameMap . names)) . InterfaceFiles.readHeaderMaybe)
  where
    names (_, _, _, _, _, _, nameHashes, _, _, _) = nameHashes

-- | One name's hashes: from the module's complete map when there is one,
-- else by a single-name read of its .tydb, which is remembered whether it
-- found the name or not: the load answers Just of the read.
lookupNameHash :: ModuleCache -> [FilePath] -> A.ModName -> A.Name -> IO (Maybe InterfaceFiles.NameHashInfo)
lookupNameHash cache searchPaths m n = do
    complete <- readMVar (nameMapTable cache)
    case M.lookup m complete of
      Just names -> return (M.lookup n names)
      Nothing    -> join <$> memoFound (nameReadTable cache) (m, n) (Just <$> withTydb cache searchPaths m (`InterfaceFiles.readNameHashMaybe` n))

-- | Make a finished module's hashes visible to later lookups.  A result
-- without name hashes (a module reused from its .tydb) must not enter the
-- complete maps: it would hide the single-name reads of its .tydb.
rememberFront :: ModuleCache -> A.ModName -> FrontResult -> IO ()
rememberFront cache m fr = do
    modifyMVar_ (moduleHashTable cache) (return . M.insert m (mhPub hashes, mhImpl hashes))
    unless (M.null (mhNames hashes)) $
      modifyMVar_ (nameMapTable cache) (return . M.insert m (mhNames hashes))
  where hashes = frHashes fr

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
  , moduleBase     :: Acton.Env.Env0                    -- __builtin__ and every module checked before this one
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
labelOf env                             = moduleLabel (pathsOf env) (nameOf env)

-- | The module's file without its project prefix, as the diagnostics of its
-- code name it.
labelFileOf                             :: ModuleEnv -> FilePath
labelFileOf env                         = modNameToFilename (dropProjPrefix (pathsOf env) (nameOf env))

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
    searchPaths = searchPath (gtPaths t)
    moduleHashOf kind m = case provider m of
      Finished hashes -> return (Just (moduleHash kind hashes))
      Outside         -> lookupModuleHash cache searchPaths kind m
      Unfinished      -> error ("Internal error: missing " ++ hashKindName kind ++ " hash for dep " ++ modNameToString m)
    nameHashOf m n = case provider m of
      Finished hashes
        | Just i <- M.lookup n (mhNames hashes) -> return (Just i)
        | discoveredAsCached m                   -> lookupNameHash cache searchPaths m n
        | otherwise                              -> return Nothing
      Outside    -> lookupNameHash cache searchPaths m n
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
cacheResolver cache paths = Resolver (lookupModuleHash cache searchPaths) (lookupNameHash cache searchPaths)
  where searchPaths = searchPath paths

-- | A dependency module row: the module's current pub and impl hash.  Both
-- are looked up.
dependencyModule :: Resolver -> A.ModName -> IO (Maybe InterfaceFiles.DepModuleInfo)
dependencyModule resolver m = do
    pub  <- resolveModule resolver Pub m
    impl <- resolveModule resolver Impl m
    return (InterfaceFiles.DepModuleInfo m <$> pub <*> impl)

moduleHash                              :: HashKind -> ModuleHashes -> B.ByteString
moduleHash Pub                          = mhPub
moduleHash Impl                         = mhImpl

nameHash                                :: HashKind -> InterfaceFiles.NameHashInfo -> B.ByteString
nameHash Pub                            = InterfaceFiles.nhPubHash
nameHash Impl                           = InterfaceFiles.nhImplHash

depModuleHash                           :: HashKind -> InterfaceFiles.DepModuleInfo -> B.ByteString
depModuleHash Pub                       = InterfaceFiles.dmiPubHash
depModuleHash Impl                      = InterfaceFiles.dmiImplHash

depNameHash                             :: HashKind -> InterfaceFiles.DepNameInfo -> B.ByteString
depNameHash Pub                         = InterfaceFiles.dniPubHash
depNameHash Impl                        = InterfaceFiles.dniImplHash

depUsers                                :: HashKind -> InterfaceFiles.DepUsers -> [A.Name]
depUsers Pub                            = InterfaceFiles.duPubUsers
depUsers Impl                           = InterfaceFiles.duImplUsers

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

-- | The module and name a qualified name refers to.
qnameTarget                             :: A.QName -> Maybe (A.ModName, A.Name)
qnameTarget (A.GName m n)               = Just (m, n)
qnameTarget (A.QName m n)               = Just (m, n)
qnameTarget (A.NoQ _)                   = Nothing

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
    labelFile = labelFileOf env

compilationError                        :: FilePath -> String -> SrcLoc -> String -> Diagnostic String
compilationError                        = Diag.actErrToDiagnostic "Compilation error"

moduleFailure                           :: [ModuleError] -> IO a
moduleFailure                           = throwIO . ModuleErrors

failOnAny                               :: [ModuleError] -> IO ()
failOnAny errs                          = unless (null errs) (moduleFailure errs)

-- | Fail a module whose own .tydb cannot be read.
missingOwnInterface                     :: ModuleEnv -> IO a
missingOwnInterface env                 = moduleFailure [MissingInterface (nameOf env)]

-- | Resolve every item, and fail the module with one error per item that
-- does not resolve.
resolveAll :: (a -> IO (Maybe b)) -> (a -> ModuleError) -> [a] -> IO [b]
resolveAll resolve problem items = do
    results <- forM items $ \x -> maybe (Left (problem x)) Right <$> resolve x
    failOnAny [ e | Left e <- results ]
    return [ b | Right b <- results ]

-- | Run an action, with the exceptions the handlers know as its
-- diagnostics.  Every other exception propagates.
tryDiagnostics :: [Handler [Diagnostic String]] -> IO a -> IO (Either [Diagnostic String] a)
tryDiagnostics handlers action = (Right <$> action) `catches` map (fmap Left) handlers

-- | The failures that the front passes report as the module's own
-- diagnostics, with its source text.
frontFailures :: ModuleEnv -> String -> [Handler [Diagnostic String]]
frontFailures env src =
    [ Handler $ \(ModuleErrors errs)               -> return (map (explainError env src) errs)
    , Handler $ \(e :: GeneralError)               -> return (located (generalError e))
    , Handler $ \(e :: Acton.Env.CompilationError) -> return (located (Acton.Env.compilationError e))
    , Handler $ \(Acton.Types.TypeErrors errs)     -> return (map typed errs)
    , Handler $ \(e :: Acton.TypeEnv.TypeError)    -> return [typed e] ]
  where
    file         = labelFileOf env
    located errs = [ compilationError file src l msg | (l, msg) <- errs ]
    typed e      = Acton.TypeEnv.mkErrorDiagnostic file src (Acton.TypeEnv.typeReport e file src)

-- | A module stage outside its front passes: only module errors are its
-- own, and they are shown without source text.
moduleBoundary :: ModuleEnv -> IO (Either [Diagnostic String] a) -> IO (Either [Diagnostic String] a)
moduleBoundary env run = run `catch` \(ModuleErrors errs) -> return (Left (map (explainError env "") errs))


-- Outputs ------------------------------------------------------------------------------------------

-- | The background outputs of a run, newest first.  Outputs are never
-- cancelled: a run waits for every one of them before it ends.
newtype OutputJobs = OutputJobs (IORef [OutputJob])

-- | One background output: the module it belongs to, its kind and its thread.
data OutputJob = OutputJob TaskKey FrontOutputKind (Async ())

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

-- | Start background outputs, each recorded in the build as soon as its
-- thread exists.  Starting runs under mask_, so a cancel interrupts it only
-- while an OutputStarted report blocks.  Nothing between creating a thread
-- and recording it is interruptible, so the run waits for every write it
-- started.  An output of a stale generation is neither started nor
-- recorded.  Each write checks the generation again in its own thread,
-- where only the write itself runs unmasked.
startOutputs :: BuildEnv -> TaskKey -> [(FrontOutputKind, IO ())] -> IO ()
startOutputs build key outputs = mask_ (mapM_ start outputs)
  where
    OutputJobs ref      = buildOutputs build
    gen                 = buildGeneration build
    start (kind, write) = whenCurrent gen $ do
      emit build (OutputStarted key kind)
      thread <- asyncWithUnmask (\unmask -> whenCurrent gen (runOutput build key kind (unmask write)))
      atomicModifyIORef' ref (\old -> (OutputJob key kind thread : old, ()))

-- | Run a started output and report its end: its time, or Nothing when it
-- failed.  A failure to report is ignored unless asynchronous.  The
-- output's own failure is rethrown after the report.
runOutput :: BuildEnv -> TaskKey -> FrontOutputKind -> IO () -> IO ()
runOutput build key kind write = do
    (result, elapsed) <- timed (try write :: IO (Either SomeException ()))
    void (trySync (emit build (OutputFinished key kind (either (const Nothing) (const (Just elapsed)) result))))
    either throwIO return result

-- | Wait for every output started so far, and return the first failure in
-- start order.  An asynchronous exception found in an output is rethrown.
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


-- The module stage ---------------------------------------------------------------------------------

-- | A dependency a cached module recorded that is not as recorded now.
data DepChange
  = Changed     HashKind A.QName B.ByteString B.ByteString [A.Name]   -- the recorded hash, the current one, the module's names using it
  | Missing     HashKind A.QName [A.Name]                            -- no current hash, or an empty one; the module's names using it
  | RowsMissing A.ModName                                            -- a changed dependency module without name rows

-- | Why a module runs its front passes.
data Reason = SourceChanged | DependencyChanges [DepChange] | Forced | ImplRefreshFailed

-- | How a module's front stage brings it up to date, in priority order.
-- An impl refresh carries the module's NotImplemented flag as assess read
-- it.
data Action
  = RunFront       Reason
  | RefreshImpl    InterfaceFiles.HeaderSummary Bool [DepChange]
  | RefreshCodegen InterfaceFiles.HeaderSummary CodegenStatus
  | Reuse          InterfaceFiles.HeaderSummary (Maybe DeferredBackJob)

-- | A module's front stage: its parse errors, what --only-build trusts, or
-- the action its state calls for.  Its module errors become its
-- diagnostics, and every other exception propagates.
frontStage :: ModuleEnv -> IO (Either [Diagnostic String] FrontResult)
frontStage env = moduleBoundary env $ case moduleCurrent env of
    ParseErrorTask diags                    -> return (Left diags)
    task | C.only_build (moduleOptions env) -> Right <$> onlyBuild env task
    TyTask header                           -> assess env header >>= act env
    _                                       -> act env (RunFront SourceChanged)

-- | What a cached module needs.  An import without an interface fails it
-- at once.  Then its dependency rows are compared with the current hashes
-- and its NotImplemented flag is read.  Its generated code is checked only
-- when nothing else needs doing, its code is generated whole and the build
-- has no alternative output.
assess :: ModuleEnv -> InterfaceFiles.HeaderSummary -> IO Action
assess env header = do
    failOnAny . map MissingInterface =<< missingImports env header
    changes    <- dependencyChanges env header
    hasNotImpl <- InterfaceFiles.readStmtHasNotImpl (tydbOf env)
    let deferred = deferredJob env hasNotImpl (InterfaceFiles.summaryImplHash header) (InterfaceFiles.summaryNameCount header)
    case dependencyAction env header hasNotImpl changes of
      Just action -> return action
      Nothing
        | C.altOutput (moduleOptions env) || isJust deferred -> return (Reuse header deferred)
        | otherwise -> do
            status <- codegenStatus (pathsOf env) (nameOf env) (InterfaceFiles.summaryImplHash header)
            return (if codegenUpToDate status then Reuse header deferred else RefreshCodegen header status)

-- | The action a cached module's dependency changes call for, if any.  A
-- changed pub hash or a missing hash runs the front passes, and so does a
-- forced module.  A changed impl hash refreshes the impl hashes.
dependencyAction                        :: ModuleEnv -> InterfaceFiles.HeaderSummary -> Bool -> [DepChange] -> Maybe Action
dependencyAction env header hasNotImpl changes
  | any needsFront changes              = Just (RunFront (DependencyChanges changes))
  | moduleForced env                    = Just (RunFront Forced)
  | any (changedIn Impl) changes        = Just (RefreshImpl header hasNotImpl changes)
  | otherwise                           = Nothing

-- | A change the front passes must see: any change but a changed impl hash.
needsFront                              :: DepChange -> Bool
needsFront (Changed kind _ _ _ _)       = kind == Pub
needsFront _                            = True

changedIn                               :: HashKind -> DepChange -> Bool
changedIn kind (Changed k _ _ _ _)      = k == kind
changedIn _    _                        = False

isMissing                               :: DepChange -> Bool
isMissing Changed{}                     = False
isMissing _                             = True

-- | Module names, each once, sorted by their dotted text.
sortedModules                           :: Data.Set.Set A.ModName -> [A.ModName]
sortedModules                           = Data.List.sortOn modNameToString . Data.Set.toList

-- | Names, each once, sorted by their text.
sortedNames                             :: Data.Set.Set A.Name -> [A.Name]
sortedNames                             = Data.List.sortOn A.nameText . Data.Set.toList

-- | The imports a cached module's header records whose interface is gone,
-- sorted by name: no current pub hash, no provider under the header's
-- name, and a .tydb that is missing or lies in a managed types directory.
-- Any other unresolved import is left to the dependency rows.
missingImports :: ModuleEnv -> InterfaceFiles.HeaderSummary -> IO [A.ModName]
missingImports env header = do
    managed <- managedTypeDirs (moduleOptions env) (pathsOf env)
    gone    <- filterM (isGone managed) (map fst (InterfaceFiles.summaryImports header))
    return (sortedModules (Data.Set.fromList gone))
  where
    isGone managed m = do
      current <- resolveModule (moduleResolver env) Pub m
      if isJust current || M.member m (gtImportProviders (moduleTask env))
        then return False
        else do found <- lookupTydbPath (buildCache (moduleBuild env)) (searchPath (pathsOf env)) m
                case found of
                  Nothing   -> return True
                  Just tydb -> (\path -> any (path `isUnder`) managed) <$> normalizePathSafe tydb

-- | The search path's managed types directories, normalized: its entries
-- except the system types and the user's --searchpath, which leaves the
-- project's own types directory and its dependencies'.
managedTypeDirs :: C.CompileOptions -> Paths -> IO [FilePath]
managedTypeDirs opts paths = do
    user   <- normalized (C.searchpath opts)
    system <- normalized (systemTypePaths (sysPath paths))
    search <- mapM normalizePathSafe (searchPath paths)
    return [ dir | dir <- search, Data.Set.notMember (normalise dir) system, Data.Set.notMember (normalise dir) user ]
  where normalized dirs = Data.Set.fromList . map normalise <$> mapM normalizePathSafe dirs

-- | How a cached module's recorded dependencies differ from now.  A
-- dependency module whose module hash changed is compared name by name for
-- each changed kind.  One that cannot be resolved counts as changed in
-- both: a stale row may name a removed provider, and only its name rows
-- tell.  Modules come sorted by name, rows in stored order, pub before
-- impl.  Every row of such a module is resolved.  A row without a stored
-- hash of a kind is not compared for it, and an empty current hash is
-- missing.  Users are read only for a changed or missing row.
dependencyChanges :: ModuleEnv -> InterfaceFiles.HeaderSummary -> IO [DepChange]
dependencyChanges env header = do
    compared <- forM (InterfaceFiles.summaryDepModules header) $ \recorded ->
                  (,) recorded <$> dependencyModule resolver (InterfaceFiles.dmiModule recorded)
    let changedModules = [ (kind, Data.Set.fromList [ InterfaceFiles.dmiModule recorded
                                                     | (recorded, current) <- compared
                                                     , maybe True (\now -> depModuleHash kind now /= depModuleHash kind recorded) current ])
                         | kind <- [Pub, Impl] ]
        kindsOf m      = [ kind | (kind, modules) <- changedModules, Data.Set.member m modules ]
    concat <$> mapM (\m -> moduleChanges m (kindsOf m))
                    (sortedModules (Data.Set.unions (map snd changedModules)))
  where
    resolver = moduleResolver env
    tydb     = tydbOf env
    moduleChanges m kinds = do
      rows <- InterfaceFiles.readDepNames tydb m
      if null rows then return [RowsMissing m] else concat <$> mapM (rowChanges m kinds) rows
    rowChanges m kinds row = do
      current <- resolveName resolver m (InterfaceFiles.dniName row)
      catMaybes <$> sequence [ compareRow m row current kind | kind <- kinds, not (B.null (depNameHash kind row)) ]
    compareRow m row current kind = case mfilter (not . B.null) (nameHash kind <$> current) of
        Just now | now == recorded -> return Nothing
        Just now                   -> Just . Changed kind qn recorded now <$> users
        Nothing                    -> Just . Missing kind qn <$> users
      where recorded = depNameHash kind row
            n        = InterfaceFiles.dniName row
            qn       = A.GName m n
            users    = depUsers kind <$> InterfaceFiles.readDepUsers tydb m n

-- | The --verbose lines of an action.  Tests match this text.  A source
-- change says only that, and a forced module says nothing.  Deltas come
-- newest first, the reverse of the order they were found in.  Missing items
-- come pub, then impl, each sorted by name, then the modules without rows.
-- A module's dependency rows hold each name once, so no two missing items
-- of one kind have the same name.
describe :: ModuleEnv -> Action -> [String]
describe env action = case action of
    RunFront SourceChanged          -> [ stale "source changed" ]
    RunFront (DependencyChanges cs) -> [ stale ("pub changes in " ++ deltas Pub cs)      | any (changedIn Pub) cs ]
                                    ++ [ stale ("missing dep hashes in " ++ missing cs) | any isMissing cs ]
    RunFront Forced                 -> []
    RunFront ImplRefreshFailed      -> [ stale "impl refresh encountered unresolved dep hashes; rerunning front passes" ]
    RefreshImpl _ _ cs              -> [ stale ("impl changes in " ++ deltas Impl cs) ]
    RefreshCodegen _ status         -> [ stale ("generated code out of date" ++ formatCodegenDelta status) ]
    Reuse _ _                       -> [ "  Fresh " ++ labelOf env ++ ": using cached .tydb" ]
  where
    stale reason                    = "  Stale " ++ labelOf env ++ ": " ++ reason
    deltas kind cs                  = intercalate ", " [ prstr qn ++ " " ++ Hashing.shortHash old ++ " → " ++ Hashing.shortHash new ++ usedBy users
                                                       | Changed k qn old new users <- reverse cs, k == kind ]
    missing cs                      = intercalate ", " $
                                           [ hashKindName kind ++ " " ++ prstr qn ++ usedBy users
                                           | kind <- [Pub, Impl]
                                           , (qn, users) <- Data.List.sortOn (Hashing.qnameKey . fst) [ (qn, users) | Missing k qn users <- cs, k == kind ] ]
                                        ++ [ "rows " ++ modNameToString m | RowsMissing m <- cs ]
    usedBy users                    = case map A.nstr (sortedNames (Data.Set.fromList users)) of
                                        []    -> ""
                                        names -> " (used by " ++ intercalate ", " names ++ ")"

-- | Report an action under --verbose, then perform it.
act :: ModuleEnv -> Action -> IO (Either [Diagnostic String] FrontResult)
act env action = do
    mapM_ (info (moduleBuild env)) (describe env action)
    perform env action

perform                                       :: ModuleEnv -> Action -> IO (Either [Diagnostic String] FrontResult)
perform env (RunFront _)                      = compileModule env
perform env (RefreshImpl header hasNotImpl _) = refreshImpl env header hasNotImpl
perform env (RefreshCodegen header _)         = Right <$> refreshCodegen env header
perform env (Reuse header deferred)           = Right <$> reuse env header deferred

-- | Check a module from source: parse it unless its parse stage did, run
-- its front passes with its imports renamed to their providers, and under
-- --verbose report how its public name hashes changed.  Its hashes become
-- visible to reads before it reports.
compileModule :: ModuleEnv -> IO (Either [Diagnostic String] FrontResult)
compileModule env = do
    previous <- if C.verbose (buildOptions build) then lookupNameMap (buildCache build) (searchPath (pathsOf env)) (nameOf env) else return Nothing
    parsed   <- materialize (buildSources build) (moduleOptions env) (nameOf env) (taskSourceFile (moduleTask env)) Nothing (moduleCurrent env)
    case parsed of
      Left diags        -> return (Left diags)
      Right (source, m) -> do
        result <- runFrontPasses env source (adjustImports (gtImportProviders (moduleTask env)) m)
        forM_ result $ \fr -> do
          forM_ (previous >>= \old -> nameHashSummary old (mhNames (frHashes fr))) $ \summary ->
            info build ("  Hash deltas " ++ labelOf env ++ ": " ++ summary)
          rememberFront (buildCache build) (nameOf env) fr
        return result
  where build = moduleBuild env

-- | How a module's public name hashes changed, if they did: the added, the
-- removed and the changed names, each group sorted by name.
nameHashSummary :: M.Map A.Name InterfaceFiles.NameHashInfo -> M.Map A.Name InterfaceFiles.NameHashInfo -> Maybe String
nameHashSummary old new
  | null items                          = Nothing
  | otherwise                           = Just (intercalate ", " items)
  where
    items   = [ "+" ++ A.nstr n | n <- sortedNames (M.keysSet (M.difference new old)) ]
           ++ [ "-" ++ A.nstr n | n <- sortedNames (M.keysSet (M.difference old new)) ]
           ++ mapMaybe changes (sortedNames (M.keysSet (M.intersection old new)))
    changes n = case mapMaybe (field (old M.! n) (new M.! n)) [ ("src", InterfaceFiles.nhSrcHash), ("pub", InterfaceFiles.nhPubHash), ("impl", InterfaceFiles.nhImplHash) ] of
                  []     -> Nothing
                  fields -> Just ("~" ++ A.nstr n ++ "{" ++ intercalate ", " fields ++ "}")
    field before after (label, hashOf)
      | hashOf before == hashOf after   = Nothing
      | otherwise                       = Just (label ++ " " ++ Hashing.shortHash (hashOf before) ++ " -> " ++ Hashing.shortHash (hashOf after))

-- | An impl refresh computed from stored rows, before anything is written.
data Recomputation
  = Recomputed [InterfaceFiles.DepModuleInfo] [InterfaceFiles.NameHashInfo] FrontResult
  | Unresolved                          -- a dependency hash is unavailable: run the front passes
  | InterfaceGone                       -- the module's own .tydb cannot be read: the module fails

-- | New impl hashes from the stored rows, when only providers'
-- implementations changed.  Only the attempt falls back to the front
-- passes, for a synchronous exception or an unresolved hash.  The write
-- comes after every read, so a fallback never runs on top of a committed
-- refresh.  A failed write and a missing own interface fail the module.
refreshImpl :: ModuleEnv -> InterfaceFiles.HeaderSummary -> Bool -> IO (Either [Diagnostic String] FrontResult)
refreshImpl env header hasNotImpl = do
    attempt <- trySync (recomputeImpl env header hasNotImpl)
    case attempt of
      Right (Recomputed dependencies names fr) -> do
        commitTydb env (\_ -> InterfaceFiles.updateImplRefresh (tydbOf env) (mhImpl (frHashes fr)) dependencies names)
        Right <$> finishStored env fr
      Right InterfaceGone -> missingOwnInterface env
      _                   -> act env (RunFront ImplRefreshFailed)

-- | A cached module's impl hashes computed again from its stored rows: the
-- own impl-hash components and local dependencies from its name rows, the
-- names of its external dependencies from its dependency rows, and their
-- current hashes from the resolver.  Its source is unchanged, so nothing is
-- parsed or checked.  Its NotImplemented flag, which decides between a
-- deferred and an eager back job, is the one assess read.  The typed module
-- is read only for an eager back job, whose input it is.  Every read and
-- every forcing happens before anything is written.  An unavailable input
-- is an outcome, never a module error.
recomputeImpl :: ModuleEnv -> InterfaceFiles.HeaderSummary -> Bool -> IO Recomputation
recomputeImpl env header hasNotImpl = do
    stored   <- InterfaceFiles.readNameHashes tydb
    restored <- restoreExternalDeps tydb (InterfaceFiles.summaryDepModules header) stored
    -- Only names with an impl item are refreshed: exactly the names the
    -- front passes gave an own impl-hash component.  The impl hash of a
    -- name with only a signature does not depend on other modules.
    let refreshable = [ nh | nh <- restored, not (B.null (InterfaceFiles.nhOwnImplHash nh)) ]
        byName f    = M.fromList [ (InterfaceFiles.nhName nh, f nh) | nh <- refreshable ]
    external <- externalHashes resolver Impl M.empty (byName (map fst . InterfaceFiles.nhImplDeps))
    case external of
      Left _       -> return Unresolved
      Right hashes -> do
        let names    = Hashing.refreshImplHashes restored (byName InterfaceFiles.nhOwnImplHash) (byName InterfaceFiles.nhImplLocalDeps) hashes
            implHash = Hashing.moduleImplHashFromNameHashes names
            interest = interestDeps names
        resolved <- mapM (dependencyModule resolver) (sortedModules (Data.Set.map fst interest))
        case sequence resolved of
          Nothing           -> return Unresolved
          Just dependencies -> do
            evaluate (rnf (implHash, dependencies, names))
            let result moduleInfo pub back = Recomputed dependencies names
                                               (storedResult moduleInfo ModuleHashes { mhPub = pub, mhImpl = implHash, mhNames = publicNameMap names } interest back)
            case deferredJob env hasNotImpl implHash (length names) of
              Just job -> maybe InterfaceGone (\shell -> result (shellModuleInfo shell) (shellPubHash shell) (DeferredBack job))
                            <$> readInterfaceShell env (Just (InterfaceFiles.summaryPubHash header))
              Nothing  -> do
                full <- InterfaceFiles.readFileMaybe tydb
                case full of
                  Nothing      -> return InterfaceGone
                  Just module' -> do
                    job <- storedBackJob env module' implHash
                    -- Forced here, so a corrupt row fails the attempt before
                    -- anything is written.
                    evaluate (rnf (InterfaceFiles.storedInterface module'))
                    evaluate (rnf (InterfaceFiles.storedTyped module'))
                    moduleInfo <- storedModuleInfo tydb (nameOf env) module'
                    return (result moduleInfo (InterfaceFiles.storedPubHash module') (EagerBack job))
  where
    resolver = moduleResolver env
    tydb     = tydbOf env

-- | A module's stored name rows with their external dependencies restored
-- from its dependency rows, which the .tydb strips from the name rows.
-- Both kinds are restored: an impl refresh writes the dependency rows again
-- from these names, and without the impl users every per-name impl change
-- of the module's dependencies would be lost.  Each list is sorted by name,
-- as the front passes store it and the hash combinators require, and a row
-- without a stored hash of a kind is left out of that kind.
restoreExternalDeps :: FilePath -> [InterfaceFiles.DepModuleInfo] -> [InterfaceFiles.NameHashInfo] -> IO [InterfaceFiles.NameHashInfo]
restoreExternalDeps tydb dependencies names = do
    (pubByOwner, implByOwner) <- foldM addModule (M.empty, M.empty) dependencies
    return [ nh { InterfaceFiles.nhPubDeps  = sorted pubByOwner nh
                , InterfaceFiles.nhImplDeps = sorted implByOwner nh }
           | nh <- names ]
  where
    sorted byOwner nh = Data.List.sortOn fst (M.findWithDefault [] (InterfaceFiles.nhName nh) byOwner)
    addModule byOwner dependency = do
      let m = InterfaceFiles.dmiModule dependency
      rows <- InterfaceFiles.readDepNames tydb m
      foldM (addRow m) byOwner rows
    addRow m (pubByOwner, implByOwner) row = do
      users <- InterfaceFiles.readDepUsers tydb m (InterfaceFiles.dniName row)
      let add kind byOwner
            | B.null (depNameHash kind row) = byOwner
            | otherwise                     = foldl' (\acc user -> M.insertWith (++) user [(A.GName m (InterfaceFiles.dniName row), depNameHash kind row)] acc)
                                                     byOwner (depUsers kind users)
      return (add Pub pubByOwner, add Impl implByOwner)

-- | Generate a cached module's code again from its stored typed module.
-- Assess sends only a module whose code is generated whole, so its back
-- job is eager.
refreshCodegen :: ModuleEnv -> InterfaceFiles.HeaderSummary -> IO FrontResult
refreshCodegen env header = do
    module'    <- InterfaceFiles.readFileMaybe tydb >>= maybe (missingOwnInterface env) return
    job        <- storedBackJob env module' (InterfaceFiles.storedImplHash module')
    interest   <- storedInterest tydb (InterfaceFiles.summaryDepModules header)
    moduleInfo <- storedModuleInfo tydb (nameOf env) module'
    finishStored env (storedResult moduleInfo
                        ModuleHashes { mhPub   = InterfaceFiles.storedPubHash module'
                                     , mhImpl  = InterfaceFiles.storedImplHash module'
                                     , mhNames = publicNameMap (InterfaceFiles.storedNames module') }
                        interest (EagerBack job))
  where tydb = tydbOf env

-- | Use a cached module's .tydb as it is.
reuse :: ModuleEnv -> InterfaceFiles.HeaderSummary -> Maybe DeferredBackJob -> IO FrontResult
reuse env header deferred = do
    shell <- readInterfaceShell env (Just (InterfaceFiles.summaryPubHash header))
               >>= maybe (missingOwnInterface env) return
    shellResult env (InterfaceFiles.summaryDepModules header) (maybe NoBack DeferredBack deferred) shell

-- | --only-build trusts the stored interfaces.  A module without a readable
-- .tydb gets an empty interface and publishes nothing.
onlyBuild :: ModuleEnv -> CompileTask -> IO FrontResult
onlyBuild env task = do
    shell <- readInterfaceShell env (InterfaceFiles.summaryPubHash <$> header)
    maybe (return (emptyResult env)) (shellResult env (maybe [] InterfaceFiles.summaryDepModules header) NoBack) shell
  where header = case task of
                   TyTask h -> Just h
                   _        -> Nothing

-- | A result from a module's interface shell.  Its interest comes from the
-- given dependency rows: the per-name rows on disk carry no external
-- dependencies.
shellResult :: ModuleEnv -> [InterfaceFiles.DepModuleInfo] -> BackPlan -> InterfaceShell -> IO FrontResult
shellResult env dependencies back shell = do
    interest <- storedInterest (tydbOf env) dependencies
    finishStored env (storedResult (shellModuleInfo shell)
                        ModuleHashes { mhPub = shellPubHash shell, mhImpl = shellImplHash shell, mhNames = M.empty }
                        interest back)

-- | The last steps of a result rebuilt from the .tydb, in the stage's
-- thread: the --tydb copy starts, then the hashes become visible to reads.
finishStored :: ModuleEnv -> FrontResult -> IO FrontResult
finishStored env fr = do
    startOutputs (moduleBuild env) (keyOf env) (tydbCopy env)
    rememberFront (buildCache (moduleBuild env)) (nameOf env) fr
    return fr

-- | The other modules' names that a cached module's code uses, from its
-- dependency rows.
storedInterest :: FilePath -> [InterfaceFiles.DepModuleInfo] -> IO (Data.Set.Set (A.ModName, A.Name))
storedInterest tydb dependencies = Data.Set.unions <$> mapM rows dependencies
  where rows dependency = do
          let m = InterfaceFiles.dmiModule dependency
          names <- InterfaceFiles.readDepNames tydb m
          return (Data.Set.fromList [ (m, InterfaceFiles.dniName row) | row <- names ])

-- | A result rebuilt from a module's .tydb, without front passes.
storedResult :: Acton.Env.ModuleInfo -> ModuleHashes -> Data.Set.Set (A.ModName, A.Name) -> BackPlan -> FrontResult
storedResult moduleInfo hashes interest back = FrontResult
    { frModuleInfo = moduleInfo, frHashes = hashes, frInterest = interest, frBack = back
    , frFrontTime = Nothing, frFrontTiming = Nothing, frInferredSigs = [] }

-- | What importers see of a checked module: its interface, of which
-- mkModuleInfo keeps the public names, with its escape summaries.
interfaceModuleInfo :: A.ModName -> I.NModule -> InterfaceFiles.EscapeSummaries -> Acton.Env.ModuleInfo
interfaceModuleInfo mn (I.NModule imports iface doc) summaries =
    Acton.Env.withEscapeSummaries summaries (Acton.Env.mkModuleInfo mn imports iface doc)

-- | The result of a module without a readable .tydb under --only-build: an
-- empty interface, no hashes, no back work.
emptyResult                             :: ModuleEnv -> FrontResult
emptyResult env                         = storedResult (Acton.Env.mkModuleInfo (nameOf env) [] [] Nothing) (ModuleHashes B.empty B.empty M.empty) Data.Set.empty NoBack

-- | The interface of a module read back whole from its .tydb, with the
-- escape summaries stored there.  Modules checked later in the same build
-- read them for their Hashable.hash contracts.
storedModuleInfo :: FilePath -> A.ModName -> InterfaceFiles.StoredModule -> IO Acton.Env.ModuleInfo
storedModuleInfo tydb mn module' = do
    summaries <- InterfaceFiles.openInterfaceDB tydb >>= InterfaceFiles.readInterfaceDBEscapeSummaries
    evaluate (rnf summaries)
    return (interfaceModuleInfo mn (InterfaceFiles.storedInterface module') summaries)

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

-- | The back job of the typed module that a module's own .tydb stores, in
-- the environment of the modules checked before it, tagged with the given
-- impl hash.
storedBackJob :: ModuleEnv -> InterfaceFiles.StoredModule -> B.ByteString -> IO BackJob
storedBackJob env module' = loadBackJob (moduleBuild env) (moduleBase env) (keyOf env) (pathsOf env) (moduleOptions env) (InterfaceFiles.storedTyped module')

-- | A back job for a typed module read back from a .tydb: its source read
-- again, its environment rebuilt with protocols converted.
loadBackJob :: BuildEnv -> Acton.Env.Env0 -> TaskKey -> Paths -> C.CompileOptions -> A.Module -> B.ByteString -> IO BackJob
loadBackJob build base key paths opts typed codegenHash = do
    snap  <- Source.readSource (buildSources build) (srcFile paths (tkMod key))
    scope <- Acton.Env.mkEnv (searchPath paths) base typed
    return BackJob { bjKey = key, bjPaths = paths, bjOpts = opts, bjTypeEnv = Converter.convEnvProtos scope
                   , bjModule = typed, bjSource = Source.ssText snap, bjCodegenHash = codegenHash }


-- Front passes -------------------------------------------------------------------------------------

-- | Check one module outside a stage graph (__builtin__, benchmarks): its
-- imports resolve through the module cache, and it is checked in the given
-- environment.
checkModule :: BuildEnv -> GlobalTask -> C.CompileOptions -> Acton.Env.Env0 -> ModuleSource -> A.Module -> IO (Either [Diagnostic String] FrontResult)
checkModule build t opts base = runFrontPasses ModuleEnv
    { moduleBuild = build, moduleTask = t, moduleCurrent = gtTask t, moduleOptions = opts
    , moduleResolver = cacheResolver (buildCache build) (gtPaths t), moduleBase = base
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

-- | Check, hash and commit one parsed module, then start its doc page and
-- --tydb copy in the background, recorded in the build for awaitOutputs.
-- Progress and outputs are reported as the module's events.  The module's
-- own errors become its diagnostics, and every other exception propagates.
runFrontPasses :: ModuleEnv -> ModuleSource -> A.Module -> IO (Either [Diagnostic String] FrontResult)
runFrontPasses env source parsed = do
    createDirectoryIfMissing True (moduleTypesDir (projTypes (pathsOf env)) mn)
    tryDiagnostics (frontFailures env (sourceText source)) passes
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
      scope           <- Acton.Env.mkEnv (searchPath (pathsOf env)) (moduleBase env) parsed
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
          I.NModule imports iface _ = interface
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
                           , bjModule = typed, bjSource = sourceText source, bjCodegenHash = hashedImpl hashed }
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
        { frModuleInfo   = interfaceModuleInfo mn interface escapes
        , frHashes       = ModuleHashes { mhPub = hashedPub hashed, mhImpl = hashedImpl hashed, mhNames = publicNameMap names }
        , frInterest     = interestDeps names
        , frBack         = maybe (EagerBack job) DeferredBack (deferredJob env (A.hasNotImpl (A.mbody typed)) (hashedImpl hashed) (length names))
        , frFrontTime    = reportedTime gopts opts (end - start)
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
    evaluate (I.forceHTEnv (Acton.Env.hnames typeEnv))
    evaluate (rnf tests)

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
    _            <- forced (sourceMeta source, imports, roots)
    report True total
    return Hashed { hashedSource = sourceHash, hashedPub = pub, hashedImpl = impl, hashedImports = imports
                  , hashedDependencies = dependencies, hashedNames = names, hashedRoots = roots }
  where
    resolver    = moduleResolver env
    mn          = nameOf env
    hashEnv     = setMod mn (checkedScope checked)
    I.NModule moduleImports iface _ = checkedInterface checked
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

-- | Whether a name is an actor that can be a program's root: one without
-- type parameters, with an empty positional row and a keyword row of one
-- parameter, whose type is Env or None.
rootEligible                                                       :: I.NameInfo -> Bool
rootEligible (I.NAct [] A.TNil{} (A.TRow _ _ _ t _ A.TNil{}) _ _) = prstr t `elem` ["Env", "None", "__builtin__.Env", "__builtin__.None"]
rootEligible _                                                     = False

-- | Write a checked and hashed module's .tydb, reporting its progress.
writeTydb :: ModuleEnv -> ModuleSource -> Checked -> Hashed -> (InterfaceFiles.TyDbWriteProgress -> IO ()) -> IO ()
writeTydb env source checked hashed onProgress =
    InterfaceFiles.writeFileWithProgress onProgress A.version (tydbOf env)
      (hashedSource hashed) (hashedPub hashed) (hashedImpl hashed) (sourceMeta source) (hashedImports hashed)
      (hashedDependencies hashed) (hashedNames hashed) (checkedEscapes checked) (hashedRoots hashed) (checkedTests checked)
      doc (checkedInterface checked) (checkedModule checked)
  where I.NModule _ _ doc = checkedInterface checked

-- | The most names a module can have and still get a documentation page.
docNameCountThreshold :: Int
docNameCountThreshold = 10000

-- | Whether a module gets a documentation page: not under --skip-build,
-- not in a scratch build, and with at most docNameCountThreshold names.
shouldGenerateDocOutput :: C.CompileOptions -> Bool -> Int -> Bool
shouldGenerateDocOutput opts temporary nameCount =
  not (C.skip_build opts) && not temporary && nameCount <= docNameCountThreshold

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
    publicIface = Acton.Env.publicTEnv iface
    docDir      = joinPath [projPath paths, "out", "doc"]
    docFile     = case A.modPath mn of
                    []   -> docDir </> "unnamed" <.> "html"
                    path -> joinPath (docDir : init path) </> last path <.> "html"
    moduleTypes = maybe publicIface Acton.Env.modulePublicTEnv (Acton.Env.lookupModuleInfo mn (checkedTypeEnv checked))
    simplified  = simp (define publicIface (setMod mn (checkedScope checked))) moduleTypes
    writeDoc    = do
      createDirectoryIfMissing True (takeDirectory docFile)
      writeFileUtf8Atomic docFile (DocP.printHtmlDoc (I.NModule imports simplified doc) parsed)

-- | When the last event of one progress stream passed.
newtype Pacer = Pacer (IORef (Maybe TimeSpec))

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

-- | Print a module's dump when its flag is set: a header that names the
-- dump and the module, the text, and a rule as wide as the header.  The
-- text is built only then.
dumpWhen                                :: C.CompileOptions -> A.ModName -> (C.CompileOptions -> Bool) -> String -> String -> IO ()
dumpWhen opts mn flag label text        = when (flag opts) $ putStrLn ("\n\n" ++ header ++ "\n" ++ text ++ "\n" ++ replicate (length header) '=' ++ "\n")
  where header                          = "== " ++ label ++ ": " ++ modNameToString mn ++ " " ++ replicate 32 '='


-- Deferred back jobs -------------------------------------------------------------------------------

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

type InterestMap = M.Map A.ModName (Data.Set.Set A.Name)

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

addInterestDeps :: Data.Set.Set (A.ModName, A.Name) -> InterestMap -> InterestMap
addInterestDeps deps im =
  foldl'
    (\acc (mn, n) -> M.insertWith Data.Set.union mn (Data.Set.singleton n) acc)
    im
    (Data.Set.toList deps)

-- | The back job of a deferred module, once every module depending on it has
-- finished its front passes, so the interest in its names is complete.  The
-- selected names are the module's roots and every name other modules use,
-- closed over local dependencies and extensions.  Their codegen hash is
-- compared with the tags of the generated files before any statement is
-- read, and the job is Nothing when the generated code is up to date.
prepareDeferredBackJob :: BuildEnv -> Acton.Env.Env0 -> InterestMap -> DeferredBackJob -> IO (Maybe BackJob)
prepareDeferredBackJob build base interestMap job = do
    roots    <- Data.Set.fromList <$> InterfaceFiles.readRoots tydb
    let interested = M.findWithDefault Data.Set.empty mn interestMap
    selected <- selectedRows tydb label (Data.Set.union interested roots)
    let names       = M.keysSet selected
        codegenHash = dbpCodegenHash (deferredImplHash job) names
        stats       = DbpStats { totalNames      = deferredNameCount job
                               , interestedNames = Data.Set.size interested
                               , rootNames       = Data.Set.size roots
                               , selectedClosure = Data.Set.size names }
    status   <- codegenStatus paths mn codegenHash
    if codegenUpToDate status
      then Nothing <$ info build (describeDbp label stats True)
      else do
        -- Same-version .tydb files always carry statement indices, so a
        -- failed selective read means a corrupt or hand-doctored file.  A
        -- whole-module read would mask that, and read gigabytes doing it.
        typed <- InterfaceFiles.readSelectedModule tydb (M.elems selected) names
                   >>= maybe (error ("Internal error: missing statement rows in " ++ tydb)) return
        back  <- loadBackJob build base (deferredKey job) paths (deferredOptions job) (pruneModule names typed) codegenHash
        info build (describeDbp label stats False)
        return (Just back)
  where paths = deferredPaths job
        mn    = tkMod (deferredKey job)
        tydb  = tyDbPath paths mn
        label = moduleLabel paths mn

-- | The name counts of a deferred module's selection.
data DbpStats = DbpStats
  { totalNames      :: Int              -- every name of the module
  , interestedNames :: Int              -- the names other modules use
  , rootNames       :: Int              -- the module's roots
  , selectedClosure :: Int              -- the interested and root names, closed over local dependencies and extensions
  }

-- | The verbose line of a deferred module, given whether its generated code
-- is up to date.
describeDbp :: String -> DbpStats -> Bool -> String
describeDbp label stats upToDate =
    "  DBP " ++ label ++ ": total names " ++ show (totalNames stats) ++ ", interested names " ++ show (interestedNames stats)
    ++ ", root names " ++ show (rootNames stats) ++ ", selected closure " ++ show (selectedClosure stats)
    ++ ", " ++ (if upToDate then "generated code up to date" else "generated code out of date")

-- | The hash a deferred module's generated code is tagged with: it changes
-- with the module's impl hash and with the set of selected names.
dbpCodegenHash :: B.ByteString -> Data.Set.Set A.Name -> B.ByteString
dbpCodegenHash moduleImplHash selected =
    SHA256.hash (BL.toStrict (encode ("dbp" :: String, moduleImplHash, map A.nstr (sortedNames selected))))

-- | A selection of a deferred module's names that found no row for a name,
-- with the text that names the module and the name.
newtype DbpSelectionError = DbpSelectionError String deriving (Show)

instance Exception DbpSelectionError where
  displayException (DbpSelectionError msg) = msg

-- | The rows of the names the seeds need, keyed by name.  Each seed and each
-- dependency resolves to the name owning its row, through the Derived chain,
-- and keeps the row that resolution read.  The search is depth first: a
-- name's local dependencies and extensions, in name order, are visited
-- before the names still waiting.  A name without an owner fails the
-- selection with a DbpSelectionError naming the module by its label.
selectedRows :: FilePath -> String -> Data.Set.Set A.Name -> IO (M.Map A.Name InterfaceFiles.NameHashInfo)
selectedRows tydb label seeds = traverse owner (Data.Set.toList seeds) >>= close M.empty
  where
    close selected []   = return selected
    close selected ((n, row) : pending)
      | M.member n selected = close selected pending
      | otherwise = do
          localDeps     <- traverse owner (InterfaceFiles.nhPubLocalDeps row ++ InterfaceFiles.nhImplLocalDeps row)
          extensions    <- unionOn id <$> InterfaceFiles.readExtensionsByClass tydb n <*> InterfaceFiles.readExtensionsByProtocol tydb n
          extensionDeps <- traverse owner extensions
          let selected' = M.insert n row selected
          close selected' (filter ((`M.notMember` selected') . fst) (unionOn fst localDeps extensionDeps) ++ pending)
    owner :: A.Name -> IO (A.Name, InterfaceFiles.NameHashInfo)
    owner n = do
      row <- InterfaceFiles.readNameHashMaybe tydb n
      case (row, n) of
        (Just found, _)         -> return (n, found)
        (_, A.Derived base _)   -> owner base
        _ | Names.isWitness n   -> failure ("unresolved witness owner for " ++ A.nstr n)
          | otherwise           -> failure ("no top-level owner for " ++ A.nstr n)
    -- The items of both lists, the first one of each name, in name order.
    unionOn :: (a -> A.Name) -> [a] -> [a] -> [a]
    unionOn name xs ys = Data.List.sortOn (A.nameText . name) (Data.List.nubBy (\x y -> name x == name y) (xs ++ ys))
    failure :: String -> IO a
    failure reason = throwIO (DbpSelectionError ("DBP selection failed for " ++ label ++ ": " ++ reason))

-- | A typed module with only the top-level statements that bind a selected
-- name.
pruneModule :: Data.Set.Set A.Name -> A.Module -> A.Module
pruneModule selected (A.Module loc imps mdoc suite) = A.Module loc imps mdoc (mapMaybe (pruneTopStmt selected) suite)

-- | A typed module's top-level statement restricted to the selected names,
-- or Nothing when it binds none of them.
pruneTopStmt :: Data.Set.Set A.Name -> A.Stmt -> Maybe A.Stmt
pruneTopStmt selected stmt = case stmt of
    A.Decl l ds            | ds'@(_:_) <- filter (isSelected . Names.dname') ds -> Just (A.Decl l ds')
    A.Signature l ns t dec | ns'@(_:_) <- filter isSelected ns                 -> Just (A.Signature l ns' t dec)
    A.Assign{}             | any isSelected (Names.bound stmt)                 -> Just stmt
    A.VarAssign{}          | any isSelected (Names.bound stmt)                 -> Just stmt
    _                                                                          -> Nothing
  where isSelected = (`Data.Set.member` selected)


-- Back passes --------------------------------------------------------------------------------------

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

-- | The dump a back pass prints, and the flag that asks for it.
backPassDump                    :: BackPass -> Maybe (C.CompileOptions -> Bool, String)
backPassDump BackPassNormalize  = Just (C.norm,  "norm")
backPassDump BackPassDeactorize = Just (C.deact, "deact")
backPassDump BackPassCPS        = Just (C.cps,   "cps")
backPassDump BackPassLLift      = Just (C.llift, "llift")
backPassDump BackPassBoxing     = Just (C.box,   "box")
backPassDump _                  = Nothing

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

        transform :: BackPass -> (a -> A.Module) -> IO a -> IO a
        transform p moduleOf work = pass p $ do
          result <- work
          forM_ (backPassDump p) $ \(flag, label) -> dumpWhen opts mn flag label (Pretty.print (moduleOf result))
          return result

        render :: String -> IO ()
        render text = evaluate (rnf text)

    ((), elapsed) <- timed $ do
      (normalized, env1)  <- transform BackPassNormalize  fst (Acton.Normalizer.normalize (bjTypeEnv job) (bjModule job))
      (deactorized, env2) <- transform BackPassDeactorize fst (Acton.Deactorizer.deactorize env1 normalized)
      (converted, env3)   <- transform BackPassCPS        fst (Acton.CPS.convert env2 deactorized)
      (lifted, env4)      <- transform BackPassLLift      fst (Acton.LambdaLifter.liftModule env3 converted)
      boxed               <- transform BackPassBoxing     id  (Acton.Boxing.doBoxing env4 lifted)
      (_, h, c, support)  <- pass BackPassCodeGen             (Acton.CodeGen.generateCode (output == WriteFiles) env4 relSrcBase (bjSource job) (not (C.dbg_no_lines opts)) boxed (Hashing.hashHex (bjCodegenHash job)))
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
    return ( reportedTime gopts opts elapsed
           , if C.timing gopts then Just timing else Nothing )
  where
    opts       = bjOpts job
    paths      = bjPaths job
    mn         = tkMod (bjKey job)
    output     = backOutput opts
    base       = outBase paths mn
    relSrcBase = makeRelative (projPath paths) (srcBase paths mn)
    total      = fromEnum (maxBound :: BackPass) + (if output == WriteFiles then 1 else 0)


-- The run ------------------------------------------------------------------------------------------

data StageKey = ParseStage TaskKey | FrontStage TaskKey deriving (Eq, Ord, Show)

-- | What a finished stage tells the coordinator.  A parse stage cannot
-- fail: a parse error becomes a ParseErrorTask, which the module's front
-- stage reports once its providers are done.
data StageOutcome
  = ParseDone TaskKey CompileTask (Maybe TimeSpec)                     -- the parse time, Nothing when quiet or the parse failed
  | FrontDone TaskKey (Either [Diagnostic String] FrontResult)

-- | A run's constants while its stages are scheduled.
data Run = Run
  { runBuild       :: BuildEnv
  , runContext     :: CompileContext
  , runTasks       :: M.Map TaskKey GlobalTask                          -- every task of the plan, __builtin__ included
  , runModuleGraph :: Graph TaskKey                                     -- the import graph of the plan's tasks except __builtin__
  , runStageGraph  :: Graph StageKey
  , runBlocked     :: Data.Set.Set TaskKey                              -- library boundaries: their back passes are not deferred
  , runWeights     :: M.Map TaskKey Integer                             -- critical weights
  , runJobs        :: Int                                               -- the most stages running at once
  , runStarted     :: IORef [Async StageOutcome]                        -- what the exit cancels: the running stages, recorded as each one starts
  }

-- | The coordinator's state between two stage completions.
data BuildState = BuildState
  { readyStages       :: [StageKey]                                     -- startable; equal priorities in the order they became startable, ties in stage order
  , runningStages     :: [(Async StageOutcome, StageKey)]               -- the last batch started first, each batch in start order
  , pendingStages     :: Data.Set.Set StageKey                          -- not completed, running ones included
  , prerequisitesLeft :: M.Map StageKey Int                             -- per stage: its prerequisites not completed
  , finishedModules   :: M.Map TaskKey ModuleHashes                     -- every module whose front stage succeeded
  , parsedModules     :: M.Map TaskKey CompileTask                      -- parse stage results waiting for their front stage
  , accumulatedEnv    :: Acton.Env.Env0                                 -- __builtin__ and every module whose front stage succeeded
  , interest          :: InterestMap                                    -- the names finished modules use, by the module defining them
  , deferredBacks     :: M.Map TaskKey (DeferredBackJob, Data.Set.Set TaskKey)  -- with the dependents it waits for
  , hadErrors         :: Bool                                           -- a front stage failed; implied by any deferred back job still waiting at the end
  }

taskOf                                  :: Run -> TaskKey -> GlobalTask
taskOf run k                            = runTasks run M.! k

-- | Whether alternative output asks for a module: it applies to the module
-- the build was started for.
altOutputFor                            :: CompileContext -> TaskKey -> Bool
altOutputFor ctx k                      = C.altOutput (optionsFor ctx k) && tkMod k == ccRootModule ctx

-- | Compile a plan's modules, across every project of the plan: reject
-- import cycles, compile __builtin__, then run every other module's parse
-- and front stages over the import graph.  Whatever ends the run, running
-- stages are cancelled first and every output is awaited after: outputs are
-- never cancelled, and startCompile waits for this before it starts the
-- next generation.
compileModules :: BuildEnv -> CompilePlan -> IO (Either CompileFailure (Acton.Env.Env0, Bool))
compileModules build plan = do
    started <- newIORef []
    ((compileAll started `finally` (readIORef started >>= mapM_ cancel)) >>= withOutputFailure)
      `finally` void (awaitOutputs build)
  where
    ctx                = cpContext plan
    (builtins, others) = partition (isBuiltin . gtKey) (cpNeededTasks plan)
    modules            = importGraph others

    compileAll started
      | found@(_ : _) <- cycles modules = return (Left (ImportCycles found))
      | otherwise = do
          bootstrapped <- case builtins of
                            [t] -> compileBuiltin build ctx t
                            _   -> return (Right ())
          case bootstrapped of
            Left failure -> return (Left failure)
            Right ()     -> stages started

    -- The base environment reads __builtin__'s .tydb, committed by now.
    stages started = do
      base <- Acton.Env.initEnvWithEscapeSummaries True builtinTypes False
      jobs <- compileParallelism (buildOptions build)
      let run = Run { runBuild       = build
                    , runContext     = ctx
                    , runTasks       = M.fromList [ (gtKey t, t) | t <- cpNeededTasks plan ]
                    , runModuleGraph = modules
                    , runStageGraph  = stageGraph ctx others modules
                    , runBlocked     = cpDbpBlocked plan
                    , runWeights     = criticalWeights modules (cpSourceSizes plan)
                    , runJobs        = jobs
                    , runStarted     = started }
      coordinate run (initialState run base)

    builtinTypes = case builtins of
                     t : _ -> projTypes (gtPaths t)
                     []    -> sysTypes (ccPathsRoot ctx)

    -- Every output is awaited once the stages are cancelled, so no output
    -- starts during the wait.  A failed output fails a run that otherwise
    -- succeeded.
    withOutputFailure result = do
      failed <- awaitOutputs build
      return (case (result, failed) of
                (Right _, Just failure) -> Left failure
                _                       -> result)

-- | __builtin__ defines the environment every other module is checked in,
-- so it compiles first, in its own environment, with its imports read
-- through the module cache.  Its cached interface serves as it is unless
-- alternative output asks for it, and --only-build compiles nothing.  A
-- failure fails the run.  Its outputs finish and its hashes become visible
-- before it reports success.
compileBuiltin :: BuildEnv -> CompileContext -> GlobalTask -> IO (Either CompileFailure ())
compileBuiltin build ctx t
  | C.only_build opts                              = return (Right ())
  | TyTask _ <- gtTask t, not (altOutputFor ctx k) = return (Right ())
  | otherwise                                      = do
      parsed <- materialize (buildSources build) opts (tkMod k) (taskSourceFile t) Nothing (gtTask t)
      case parsed of
        Left diags        -> Left BuiltinFailed <$ emit build (Diagnostics t diags)
        Right (source, m) -> do
          emit build (FrontStarted t)
          base   <- Acton.Env.initEnv (projTypes (gtPaths t)) True
          result <- checkModule build t opts base source m
          case result of
            Left diags -> Left BuiltinFailed <$ reportFrontFailure build t diags
            Right fr   -> do
              failure <- awaitOutputs build
              case failure of
                Just f  -> return (Left f)
                Nothing -> do rememberFront (buildCache build) (tkMod k) fr
                              Right () <$ reportFrontSuccess build t fr
  where k    = gtKey t
        opts = optionsFor ctx k

-- | A front stage per module, after its providers' front stages and, when
-- its source is parsed in a stage of its own, after its parse stage.
stageGraph :: CompileContext -> [GlobalTask] -> Graph TaskKey -> Graph StageKey
stageGraph ctx tasks modules = graph $
       [ (ParseStage k, [])                                                                     | t <- tasks, let k = gtKey t, hasParseStage ctx t ]
    ++ [ (FrontStage k, [ ParseStage k | hasParseStage ctx t ] ++ map FrontStage (depsOf modules k)) | t <- tasks, let k = gtKey t ]

-- | Source size plus the heaviest chain of modules waiting for the module.
-- The map is defined in terms of itself: sound because the graph is acyclic
-- (checked before) and Data.Map is the lazy one.  The sizes have a key for
-- every node.
criticalWeights                         :: Graph TaskKey -> M.Map TaskKey Integer -> M.Map TaskKey Integer
criticalWeights g sizes                 = weights
  where weights                         = M.fromList [ (k, sizes M.! k + maximum (0 : map (weights M.!) (usersOf g k))) | k <- nodes g ]

-- | Before any stage starts: the stages without prerequisites are ready, in
-- stage order.
initialState :: Run -> Acton.Env.Env0 -> BuildState
initialState run base = BuildState
    { readyStages       = [ s | (s, 0) <- M.toList waiting ]
    , runningStages     = []
    , pendingStages     = M.keysSet waiting
    , prerequisitesLeft = waiting
    , finishedModules   = M.empty
    , parsedModules     = M.empty
    , accumulatedEnv    = base
    , interest          = M.empty
    , deferredBacks     = M.empty
    , hadErrors         = False
    }
  where waiting = M.fromList [ (s, length (depsOf (runStageGraph run) s)) | s <- nodes (runStageGraph run) ]

-- | Start ready stages, fold each completion into the state, repeat.  The
-- run finishes when no stage is pending.  When nothing runs and nothing is
-- ready but stages are still pending, the run is stuck: it ends with errors.
coordinate :: Run -> BuildState -> IO (Either CompileFailure (Acton.Env.Env0, Bool))
coordinate run state0 = do
    state <- mask_ (startReady run state0)
    case (runningStages state, readyStages state) of
      ([], []) | Data.Set.null (pendingStages state) -> return (Right (accumulatedEnv state, hadErrors state))
               | otherwise                           -> return (Right (accumulatedEnv state, True))
      _ -> do
        (done, outcome) <- waitAny (map fst (runningStages state))
        let state1 = state { runningStages = filter ((/= done) . fst) (runningStages state) }
        case outcome of
          ParseDone k task time    -> do emit (runBuild run) (ParseFinished (taskOf run k) time)
                                         coordinate run (parseDone run k task state1)
          FrontDone k (Left diags) -> frontFailed run k diags state1 >>= coordinate run
          FrontDone k (Right fr)   -> do reportFrontSuccess (runBuild run) (taskOf run k) fr
                                         flushDeferred run (frontSucceeded run k fr state1) >>= either (return . Left) (coordinate run)

-- | Start ready stages up to the parallelism, which parse and front stages
-- share: front stages first, then the heaviest chain, and equal ones in
-- their ready order (a stable sort).  Each start is reported before its
-- thread exists, and each thread is recorded in runStarted as soon as it
-- exists.  coordinate runs this masked, and nothing between creating a
-- thread and recording it is interruptible, so an exception can arrive only
-- while every started stage is recorded: reporting a start runs the
-- caller's handler, which may block interruptibly or throw.  Stage threads
-- inherit the mask and run masked-interruptible: a cancel reaches them only
-- at interruptible points such as blocking I/O.
startReady :: Run -> BuildState -> IO BuildState
startReady run state = startRest [] now
  where
    -- Record the running stages, then start the next stage of the batch.
    startRest started stages    = do
      let running = started ++ runningStages state
      writeIORef (runStarted run) (map fst running)
      case stages of
        []           -> return state { readyStages = later, runningStages = running }
        stage : rest -> do emit (runBuild run) (stageStarted stage)
                           thread <- async (runStage stage)
                           startRest (started ++ [(thread, stage)]) rest
    (now, later)                = splitAt (runJobs run - length (runningStages state)) (Data.List.sortOn (Down . priority) (readyStages state))
    priority (FrontStage k)     = (1 :: Int, weight k)
    priority (ParseStage k)     = (0, weight k)
    weight k                    = runWeights run M.! k
    stageStarted (ParseStage k) = ParseStarted (taskOf run k)
    stageStarted (FrontStage k) = FrontStarted (taskOf run k)
    runStage (ParseStage k)     = uncurry (ParseDone k) <$> parseStage (runBuild run) (optionsFor (runContext run) k) (taskOf run k)
    runStage (FrontStage k)     = FrontDone k <$> frontStage (moduleEnv run state k)

-- | A stage completed: it is no longer pending, and the stages whose last
-- prerequisite it was become ready, appended in stage order.
stageCompleted :: Run -> StageKey -> BuildState -> BuildState
stageCompleted run stage state = state { pendingStages = pending, prerequisitesLeft = waiting, readyStages = readyStages state ++ newlyReady }
  where pending    = Data.Set.delete stage (pendingStages state)
        stageUsers = usersOf (runStageGraph run) stage
        waiting    = foldl' (flip (M.adjust (subtract 1))) (prerequisitesLeft state) stageUsers
        newlyReady = Data.Set.toAscList (Data.Set.fromList [ u | u <- stageUsers, Data.Set.member u pending, M.lookup u waiting == Just 0 ])

-- | A parse stage completed: its module's front stage checks what it parsed.
parseDone                               :: Run -> TaskKey -> CompileTask -> BuildState -> BuildState
parseDone run k task state              = (stageCompleted run (ParseStage k) state) { parsedModules = M.insert k task (parsedModules state) }

-- | A module's front stage succeeded: later stages see its hashes, its
-- interface and the names it uses, and its deferred back job waits for
-- every module depending on it.
frontSucceeded :: Run -> TaskKey -> FrontResult -> BuildState -> BuildState
frontSucceeded run k fr state = (stageCompleted run (FrontStage k) state)
    { finishedModules = M.insert k (frHashes fr) (finishedModules state)
    , parsedModules   = M.delete k (parsedModules state)
    , accumulatedEnv  = Acton.Env.addModuleInfo (tkMod k) (frModuleInfo fr) (accumulatedEnv state)
    , interest        = addInterestDeps (frInterest fr) (interest state)
    , deferredBacks   = case frBack fr of
                          DeferredBack job -> M.insert k (job, strictDependents (runModuleGraph run) k) (deferredBacks state)
                          _                -> deferredBacks state
    }

-- | A module's front stage failed.  Every module depending on it is dropped
-- without events, and their running stages (parse stages only) are
-- cancelled.  cancel waits for each thread to end.
frontFailed :: Run -> TaskKey -> [Diagnostic String] -> BuildState -> IO BuildState
frontFailed run k diags state = do
    reportFrontFailure (runBuild run) (taskOf run k) diags
    mapM_ (cancel . fst) cancelled
    return state { runningStages = kept
                 , pendingStages = Data.Set.delete (FrontStage k) (pendingStages state `Data.Set.difference` blockedStages)
                 , readyStages   = filter (`Data.Set.notMember` blockedStages) (readyStages state)
                 , parsedModules = foldl' (flip M.delete) (parsedModules state) (k : Data.Set.toList blocked)
                 , hadErrors     = True }
  where blocked           = strictDependents (runModuleGraph run) k
        blockedStages     = Data.Set.fromList [ s | b <- Data.Set.toList blocked, s <- [ParseStage b, FrontStage b] ]
        (cancelled, kept) = partition ((`Data.Set.member` blockedStages) . snd) (runningStages state)

-- | A successful front stage, as handlers see it.
reportFrontSuccess :: BuildEnv -> GlobalTask -> FrontResult -> IO ()
reportFrontSuccess build t fr = do
    emit build (FrontFinished t (Just fr))
    case frBack fr of
      EagerBack job -> emit build (BackReady job)
      _             -> return ()

-- | A failed front stage, as handlers see it.
reportFrontFailure :: BuildEnv -> GlobalTask -> [Diagnostic String] -> IO ()
reportFrontFailure build t diags = do
    emit build (FrontFinished t Nothing)
    emit build (Diagnostics t diags)

-- | Prepare the deferred back jobs whose dependents all finished their
-- front stages: only then is the interest in their names complete.
-- Preparation runs on the coordinator.  A skipped job is reported as it is
-- prepared, prepared jobs once the whole batch is.  A synchronous failure
-- stops the run.
flushDeferred :: Run -> BuildState -> IO (Either CompileFailure BuildState)
flushDeferred run state = do
    prepared <- trySync (mapM prepare (M.elems due))
    case prepared of
      Left e     -> return (Left (DeferredBackFailed (displayException e)))
      Right jobs -> do mapM_ (emit (runBuild run) . BackReady) (catMaybes jobs)
                       return (Right state { deferredBacks = waiting })
  where
    (due, waiting)   = M.partition ((`Data.Set.isSubsetOf` M.keysSet (finishedModules state)) . snd) (deferredBacks state)
    prepare (job, _) = do
      back <- prepareDeferredBackJob (runBuild run) (accumulatedEnv state) (interest state) job
      when (isNothing back) $ emit (runBuild run) (BackSkipped (deferredKey job))
      return back

-- | A front stage's view of the run, taken from the state it starts in: its
-- providers finished before it became ready, so that state holds
-- everything it reads.
moduleEnv :: Run -> BuildState -> TaskKey -> ModuleEnv
moduleEnv run state k = ModuleEnv
    { moduleBuild    = runBuild run
    , moduleTask     = t
    , moduleCurrent  = M.findWithDefault (gtTask t) k (parsedModules state)
    , moduleOptions  = opts
    , moduleResolver = inRunResolver (buildCache (runBuild run)) (runTasks run) (finishedModules state) t
    , moduleBase     = accumulatedEnv state
    , moduleBlocked  = Data.Set.member k (runBlocked run)
    , moduleForced   = altOutputFor (runContext run) k || C.alwaysbuild opts
    }
  where t    = taskOf run k
        opts = optionsFor (runContext run) k

-- | Parse a module's source in a stage of its own, in parallel with its
-- providers' front stages.  The syntax tree is forced here, so the parse
-- runs on the stage's thread.  A parse error completes the stage as a
-- ParseErrorTask, which the module's front stage reports.
parseStage :: BuildEnv -> C.CompileOptions -> GlobalTask -> IO (CompileTask, Maybe TimeSpec)
parseStage build opts t = do
    (parsed, elapsed) <- timed $ do
      result <- materialize (buildSources build) opts (tkMod (gtKey t)) (taskSourceFile t) (Just (emit build . ParseProgressed t)) (gtTask t)
      mapM_ (evaluate . rnf . snd) result
      return result
    return $ case parsed of
      Left diags        -> (ParseErrorTask diags, Nothing)
      Right (source, m) -> (ActonTask source m, reportedTime (buildOptions build) opts elapsed)


-- Codegen status -----------------------------------------------------------------------------------

-- | The hash a module's generated code should be tagged with, in hex, and
-- the tags its .c, .h and closure support file carry, Nothing for a file that
-- cannot be read or is not tagged.
data CodegenStatus = CodegenStatus
  { csExpected :: String
  , csC        :: Maybe String
  , csH        :: Maybe String
  , csSupport  :: Maybe String
  }

-- | The hash tagged into a generated file's first line, when the file can
-- be read and its first line is tagged.
readCodegenHash :: FilePath -> IO (Maybe String)
readCodegenHash path = either (const Nothing) taggedHash <$> trySync (withFile path ReadMode hGetLine)
  where taggedHash line = do rest <- Data.List.stripPrefix Acton.CodeGen.implHashTag line
                             case takeWhile isHexDigit (dropWhile isSpace rest) of
                               ""  -> Nothing
                               hex -> Just hex

-- | The hash a module's generated code should be tagged with, and the tags
-- its .c, .h and closure support file carry, read in that order.
codegenStatus :: Paths -> A.ModName -> B.ByteString -> IO CodegenStatus
codegenStatus paths mn expected = CodegenStatus (Hashing.hashHex expected) <$> readCodegenHash (base ++ ".c") <*> readCodegenHash (base ++ ".h") <*> readCodegenHash (Acton.CodeGen.supportFile base)
  where base = outBase paths mn

-- | Whether the .c, the .h and the closure support file are all tagged with
-- the expected hash.
codegenUpToDate :: CodegenStatus -> Bool
codegenUpToDate status =
  all (== Just (csExpected status)) [csC status,csH status,csSupport status]

-- | The verbose suffix of a module whose generated code is out of date: the
-- first eight hex digits of each file's tag, or missing, and of the expected
-- hash.  The .c, the .h and the closure support file share one pair when
-- they carry the same tag.
formatCodegenDelta :: CodegenStatus -> String
formatCodegenDelta status
  | Just _ <- csC status, csC status == csH status, csH status == csSupport status
                                                   = " {impl " ++ delta (csC status) ++ "}"
  | otherwise                                      = " {impl c " ++ delta (csC status) ++ ", h " ++ delta (csH status) ++ ", support " ++ delta (csSupport status) ++ "}"
  where delta tag                                  = maybe "missing" Hashing.shortHex tag ++ " -> " ++ Hashing.shortHex (csExpected status)
