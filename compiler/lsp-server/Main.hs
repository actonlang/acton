{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

import Control.Exception (AsyncException, SomeException, displayException, evaluate, finally, fromException, throwIO, try)
import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, tryPutMVar)
import Control.Monad (forM, forM_, forever, void, when)
import Control.Monad.IO.Class
import Data.Aeson (toJSON)
import Data.Char (ord)
import Data.List (find)
import Data.IORef
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Sequence (Seq, ViewL(..), (|>))
import qualified Data.Sequence as Seq
import qualified Data.Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Conc (getNumCapabilities)
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>), takeExtension, takeFileName)
import qualified System.FSNotify as FS
import System.IO.Unsafe (unsafePerformIO)

import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types hiding (Diagnostic, Position)
import qualified Language.LSP.Protocol.Types as LSP
import Language.LSP.Server

import qualified Acton.BuildSpec as BuildSpec
import qualified Acton.Compile as Compile
import qualified Acton.CommandLineParser as C
import qualified Acton.Completion as Completion
import qualified Acton.Env as Env
import qualified Acton.SourceProvider as Source
import qualified Acton.Syntax as S

import Error.Diagnose (Diagnostic)
import Error.Diagnose.Diagnostic (reportsOf)
import Error.Diagnose.Report (Marker(..), Note(..), Report(Warn, Err))
import Error.Diagnose.Position (Position(..))


type OverlayMap = HM.HashMap FilePath Source.SourceSnapshot
type UriMap = HM.HashMap FilePath Uri
type BackgroundCompilerLockMap = HM.HashMap FilePath Compile.BackgroundCompilerLock
type BackgroundCompilerWarned = HM.HashMap FilePath ()
type CompletionStateMap = HM.HashMap FilePath CompletionState
type CompletionEnvCacheMap = HM.HashMap FilePath CompletionEnvCache
type ProjectBuildCacheMap = HM.HashMap FilePath ProjectBuildCache
type ProjectWatcherMap = HM.HashMap (FilePath, FilePath) ThreadId

data CompletionState = CompletionState
  { completionEnv :: Env.Env0
  , completionSearchPath :: [FilePath]
  , completionModuleName :: S.ModName
  , completionStateGen :: Int
  }

data CompletionEnvCache = CompletionEnvCache
  { cachedCompletionStateGen :: Int
  , cachedCompletionImportKey :: String
  , cachedCompletionEnv :: Env.Env0
  }

data ProjectBuildCache = ProjectBuildCache
  { cachedCompileContext :: Compile.CompileContext
  , cachedProjectMap :: M.Map FilePath Compile.ProjCtx
  , cachedGlobalTasks :: [Compile.GlobalTask]
  , cachedDbpBlocked :: Data.Set.Set Compile.TaskKey
  , cachedRootPins :: M.Map String BuildSpec.PkgDep
  , cachedImportKeys :: HM.HashMap FilePath String
  }

data PlanOrigin = PlanFromCache | PlanFromDiscovery deriving (Eq, Show)

data LspProgress = LspProgress
  { lspProgressToken :: ProgressToken
  , lspProgressQueue :: IORef (Seq T.Text)
  , lspProgressSignal :: MVar ()
  , lspProgressWorker :: ThreadId
  }

-- | Debounce delay (microseconds) before recompiling on edits.
debounceMicros :: Int
debounceMicros = 200000

progressIntervalMicros :: Int
progressIntervalMicros = 250000

progressQueueLimit :: Int
progressQueueLimit = 100

{-# NOINLINE overlaysRef #-}
-- | Global in-memory buffer map used as a source overlay.
overlaysRef :: IORef OverlayMap
overlaysRef = unsafePerformIO (newIORef HM.empty)

{-# NOINLINE uriByPathRef #-}
-- | Map canonical paths to the client-provided URIs for diagnostics.
uriByPathRef :: IORef UriMap
uriByPathRef = unsafePerformIO (newIORef HM.empty)

{-# NOINLINE backgroundCompilerLocksRef #-}
-- | Map project roots to held background-compiler locks.
backgroundCompilerLocksRef :: IORef BackgroundCompilerLockMap
backgroundCompilerLocksRef = unsafePerformIO (newIORef HM.empty)

{-# NOINLINE backgroundCompilerWarnedRef #-}
-- | Remember which projects have already warned about another background compiler.
backgroundCompilerWarnedRef :: IORef BackgroundCompilerWarned
backgroundCompilerWarnedRef = unsafePerformIO (newIORef HM.empty)

{-# NOINLINE completionStatesRef #-}
-- | Latest successful front-end environment for member completion.
completionStatesRef :: IORef CompletionStateMap
completionStatesRef = unsafePerformIO (newIORef HM.empty)

{-# NOINLINE completionEnvCacheRef #-}
completionEnvCacheRef :: IORef CompletionEnvCacheMap
completionEnvCacheRef = unsafePerformIO (newIORef HM.empty)

{-# NOINLINE projectBuildCachesRef #-}
projectBuildCachesRef :: IORef ProjectBuildCacheMap
projectBuildCachesRef = unsafePerformIO (newIORef HM.empty)

{-# NOINLINE projectWatchersRef #-}
projectWatchersRef :: IORef ProjectWatcherMap
projectWatchersRef = unsafePerformIO (newIORef HM.empty)

{-# NOINLINE compileScheduler #-}
-- | Shared compile scheduler for LSP events and back jobs.
compileScheduler :: Compile.CompileScheduler
compileScheduler = unsafePerformIO $ do
  nCaps <- getNumCapabilities
  let maxParallel = max 1 (if C.jobs lspGlobalOpts > 0 then C.jobs lspGlobalOpts else nCaps)
  Compile.newCompileScheduler lspGlobalOpts maxParallel

-- | Global compiler options for the LSP server (quiet, no color).
lspGlobalOpts :: C.GlobalOptions
lspGlobalOpts =
  C.GlobalOptions
    { color = C.Never
    , quiet = True
    , noProgress = True
    , timing = False
    , tty = False
    , verbose = False
    , verboseZig = False
    , jobs = 0
    }

-- | Compile options for LSP runs (front passes only, no final build).
lspCompileOpts :: C.CompileOptions
lspCompileOpts =
  Compile.defaultCompileOptions
    { C.skip_build = True
    , C.only_build = False
    , C.test = False
    }


-- | SourceProvider that reads overlays first, then falls back to disk.
overlaySourceProvider :: IORef OverlayMap -> Source.SourceProvider
overlaySourceProvider ref =
  let disk = Source.diskSourceProvider
  in Source.SourceProvider
       { Source.spReadOverlay = \path -> HM.lookup path <$> readIORef ref
       , Source.spReadFile = Source.spReadFile disk
       , Source.spGetModTime = Source.spGetModTime disk
       }

-- | Update the in-memory overlay for a document.
updateOverlay :: FilePath -> T.Text -> IO ()
updateOverlay path txt =
  let snap = Source.SourceSnapshot
        { Source.ssText = T.unpack txt
        , Source.ssBytes = TE.encodeUtf8 txt
        , Source.ssIsOverlay = True
        }
  in atomicModifyIORef' overlaysRef $ \m -> (HM.insert path snap m, ())

-- | Remove a document overlay when it closes.
removeOverlay :: FilePath -> IO ()
removeOverlay path =
  atomicModifyIORef' overlaysRef $ \m -> (HM.delete path m, ())

-- | Publish diagnostics for a file to the LSP client.
publishDiagnosticsFor :: FilePath -> [LSP.Diagnostic] -> LspM () ()
publishDiagnosticsFor path diags = do
  uri <- liftIO $ lookupUri path
  sendNotification SMethod_TextDocumentPublishDiagnostics (PublishDiagnosticsParams uri Nothing diags)
rememberUri :: FilePath -> Uri -> IO ()
rememberUri path uri =
  atomicModifyIORef' uriByPathRef $ \m -> (HM.insert path uri m, ())

forgetUri :: FilePath -> IO ()
forgetUri path =
  atomicModifyIORef' uriByPathRef $ \m -> (HM.delete path m, ())

lookupUri :: FilePath -> IO Uri
lookupUri path = do
  m <- readIORef uriByPathRef
  return (fromMaybe (filePathToUri path) (HM.lookup path m))

ensureBackgroundCompilerLock :: FilePath -> IO Bool
ensureBackgroundCompilerLock projRoot = do
  locks <- readIORef backgroundCompilerLocksRef
  case HM.lookup projRoot locks of
    Just _ -> return True
    Nothing -> do
      mlock <- Compile.tryBackgroundCompilerLock projRoot
      case mlock of
        Nothing -> return False
        Just lock -> do
          atomicModifyIORef' backgroundCompilerLocksRef $ \m -> (HM.insert projRoot lock m, ())
          atomicModifyIORef' backgroundCompilerWarnedRef $ \m -> (HM.delete projRoot m, ())
          return True

releaseBackgroundCompilerLocks :: IO ()
releaseBackgroundCompilerLocks = do
  locks <- readIORef backgroundCompilerLocksRef
  forM_ (HM.elems locks) Compile.releaseBackgroundCompilerLock
  writeIORef backgroundCompilerLocksRef HM.empty

rememberCompletionStates :: Int -> Compile.CompilePlan -> Env.Env0 -> IO ()
rememberCompletionStates gen plan env = do
  entries <- forM (Compile.cpGlobalTasks plan) $ \t -> do
    let key = Compile.gtKey t
        paths = Compile.gtPaths t
        mn = Compile.tkMod key
    path <- Compile.normalizePathSafe (Compile.srcFile paths mn)
    return (path, CompletionState env (Compile.searchPath paths) mn gen)
  atomicModifyIORef' completionStatesRef $ \m ->
    (foldr (uncurry HM.insert) m entries, ())

lookupCompletionState :: FilePath -> IO (Maybe CompletionState)
lookupCompletionState path = HM.lookup path <$> readIORef completionStatesRef

cacheCompletionState :: FilePath -> CompletionState -> IO ()
cacheCompletionState path state =
  atomicModifyIORef' completionStatesRef $ \m -> (HM.insert path state m, ())

loadDiskCompletionState :: FilePath -> IO CompletionState
loadDiskCompletionState path = do
  paths <- Compile.findPaths path lspCompileOpts
  env <- Env.initEnv (Compile.sysTypes paths) False
  return CompletionState
    { completionEnv = env
    , completionSearchPath = Compile.searchPath paths
    , completionModuleName = Compile.modName paths
    , completionStateGen = 0
    }

loadCompletionStateFor :: FilePath -> IO (Maybe CompletionState)
loadCompletionStateFor path = do
  mstate <- lookupCompletionState path
  case mstate of
    Just state -> return (Just state)
    Nothing -> do
      res <- (try (loadDiskCompletionState path) :: IO (Either SomeException CompletionState))
      case res of
        Right state -> do
          cacheCompletionState path state
          return (Just state)
        Left _ ->
          return Nothing

cacheCompilePlan :: Compile.CompilePlan -> IO ()
cacheCompilePlan plan = do
  importKeys <- globalTaskImportKeys (Compile.cpGlobalTasks plan)
  let ctx = Compile.cpContext plan
      rootProj = Compile.ccRootProj ctx
      cache = ProjectBuildCache
        { cachedCompileContext = ctx
        , cachedProjectMap = Compile.cpProjMap plan
        , cachedGlobalTasks = Compile.cpGlobalTasks plan
        , cachedDbpBlocked = Compile.cpDbpBlocked plan
        , cachedRootPins = Compile.cpRootPins plan
        , cachedImportKeys = importKeys
        }
  atomicModifyIORef' projectBuildCachesRef $ \m ->
    (HM.insert rootProj cache m, ())
  ensureProjectWatchers rootProj (M.keys (Compile.cpProjMap plan))

globalTaskImportKeys :: [Compile.GlobalTask] -> IO (HM.HashMap FilePath String)
globalTaskImportKeys tasks =
  HM.fromList <$> mapM taskImportKeyEntry tasks

taskImportKeyEntry :: Compile.GlobalTask -> IO (FilePath, String)
taskImportKeyEntry task = do
  path <- Compile.normalizePathSafe $
    Compile.srcFile (Compile.gtPaths task) (Compile.tkMod (Compile.gtKey task))
  return (path, taskImportKey task)

taskImportKey :: Compile.GlobalTask -> String
taskImportKey task =
  show (Compile.importsOf (Compile.gtTask task))

invalidateProjectCache :: FilePath -> IO ()
invalidateProjectCache rootProj = do
  atomicModifyIORef' projectBuildCachesRef $ \m ->
    (HM.delete rootProj m, ())
  invalidateCompletionEnvCache

ensureProjectWatchers :: FilePath -> [FilePath] -> IO ()
ensureProjectWatchers rootProj roots =
  forM_ roots $ \watchedRoot -> do
    let key = (rootProj, watchedRoot)
    existing <- HM.lookup key <$> readIORef projectWatchersRef
    case existing of
      Just _ -> return ()
      Nothing -> do
        tid <- forkIO (watchProjectShape rootProj watchedRoot)
        atomicModifyIORef' projectWatchersRef $ \m ->
          (HM.insert key tid m, ())

watchProjectShape :: FilePath -> FilePath -> IO ()
watchProjectShape rootProj watchedRoot =
  FS.withManager $ \mgr -> do
    let srcRoot = watchedRoot </> "src"
        invalidate _ = invalidateProjectCache rootProj
        isActEvent ev = takeExtension (FS.eventPath ev) == ".act"
        isBuildActEvent ev = takeFileName (FS.eventPath ev) == "Build.act"
    srcExists <- doesDirectoryExist srcRoot
    when srcExists $
      void $ FS.watchTree mgr srcRoot isActEvent invalidate
    void $ FS.watchDir mgr watchedRoot isBuildActEvent invalidate
    forever $ threadDelay maxBound

prepareLspCompilePlan
  :: Source.SourceProvider
  -> C.GlobalOptions
  -> C.CompileOptions
  -> FilePath
  -> LspM () (Either String (PlanOrigin, Compile.CompilePlan))
prepareLspCompilePlan sp gopts opts path = do
  planE <- liftIO ((try $ do
    ctx <- Compile.prepareCompileContext opts [path]
    path' <- Compile.normalizePathSafe path
    mcache <- HM.lookup (Compile.ccRootProj ctx) <$> readIORef projectBuildCachesRef
    case mcache of
      Just cache
        | Compile.ccBuildStamp (cachedCompileContext cache) == Compile.ccBuildStamp ctx -> do
            mplan <- compilePlanFromCache ctx path' cache
            case mplan of
              Just plan -> return (PlanFromCache, plan)
              Nothing -> freshPlan
      _ -> freshPlan
    ) :: IO (Either SomeException (PlanOrigin, Compile.CompilePlan)))
  case planE of
    Left err ->
      case fromException err of
        Just (Compile.ProjectError msg) -> return (Left msg)
        Nothing -> return (Left (displayException err))
    Right plan -> return (Right plan)
  where
    freshPlan = do
      plan <- Compile.prepareCompilePlan sp gopts compileScheduler opts [path] False (Just [path])
      cacheCompilePlan plan
      return (PlanFromDiscovery, plan)

compilePlanFromCache :: Compile.CompileContext -> FilePath -> ProjectBuildCache -> IO (Maybe Compile.CompilePlan)
compilePlanFromCache ctx changedPath cache = do
  refreshed <- refreshChangedGlobalTask changedPath (Compile.ccOpts ctx) cache
  case refreshed of
    Nothing -> return Nothing
    Just (globalTasks, importKeys) -> do
      let dbpBlocked = Compile.libraryBoundaryTasks (cachedProjectMap cache) globalTasks
      neededTasks <- Compile.selectAffectedTasks
        (Compile.ccRootProj ctx)
        (Compile.ccOpts ctx)
        globalTasks
        dbpBlocked
        [changedPath]
      let rootProj = Compile.ccRootProj ctx
          rootTasks =
            [ Compile.gtTask t
            | t <- neededTasks
            , Compile.tkProj (Compile.gtKey t) == rootProj
            ]
          cache' = cache
            { cachedCompileContext = ctx
            , cachedGlobalTasks = globalTasks
            , cachedDbpBlocked = dbpBlocked
            , cachedImportKeys = importKeys
            }
      atomicModifyIORef' projectBuildCachesRef $ \m ->
        (HM.insert rootProj cache' m, ())
      return $ Just Compile.CompilePlan
        { Compile.cpContext = ctx
        , Compile.cpProjMap = cachedProjectMap cache
        , Compile.cpGlobalTasks = globalTasks
        , Compile.cpNeededTasks = neededTasks
        , Compile.cpDbpBlocked = dbpBlocked
        , Compile.cpRootTasks = rootTasks
        , Compile.cpRootPins = cachedRootPins cache
        , Compile.cpIncremental = True
        , Compile.cpAllowPrune = False
        , Compile.cpChangedPaths = Just [changedPath]
        , Compile.cpSrcFiles = [changedPath]
        }

refreshChangedGlobalTask
  :: FilePath
  -> C.CompileOptions
  -> ProjectBuildCache
  -> IO (Maybe ([Compile.GlobalTask], HM.HashMap FilePath String))
refreshChangedGlobalTask changedPath opts cache =
  case find isChanged (cachedGlobalTasks cache) of
    Nothing -> return Nothing
    Just old -> do
      newTask <- Compile.readModuleTask
        (overlaySourceProvider overlaysRef)
        lspGlobalOpts
        opts
        (Compile.gtPaths old)
        changedPath
      let new = old { Compile.gtTask = newTask }
          newKey = taskImportKey new
      case HM.lookup changedPath (cachedImportKeys cache) of
        Just oldKey | oldKey /= newKey ->
          return Nothing
        _ -> do
          let tasks' =
                [ if Compile.gtKey t == Compile.gtKey old then new else t
                | t <- cachedGlobalTasks cache
                ]
              importKeys' = HM.insert changedPath newKey (cachedImportKeys cache)
          return (Just (tasks', importKeys'))
  where
    isChanged t =
      Compile.srcFile (Compile.gtPaths t) (Compile.tkMod (Compile.gtKey t)) == changedPath

progressToken :: Int -> FilePath -> ProgressToken
progressToken gen path =
  ProgressToken (InR (T.pack ("acton-lsp-" ++ show gen ++ "-" ++ takeFileName path)))

startLspProgress :: Int -> FilePath -> LspM () LspProgress
startLspProgress gen path = do
  env <- getLspEnv
  queue <- liftIO $ newIORef Seq.empty
  signal <- liftIO newEmptyMVar
  let token = progressToken gen path
      title = T.pack ("Acton: " ++ takeFileName path)
      dequeue = atomicModifyIORef' queue $ \msgs ->
        case Seq.viewl msgs of
          EmptyL -> (msgs, Nothing)
          msg :< rest -> (rest, Just msg)
      drain = do
        mmsg <- dequeue
        case mmsg of
          Nothing -> return ()
          Just msg -> do
            runLspT env $ progressReportToken token msg Nothing
            threadDelay progressIntervalMicros
            drain
      loop = forever $ takeMVar signal >> drain
  worker <- liftIO $ forkIO loop
  let progress = LspProgress token queue signal worker
  void $ sendRequest SMethod_WindowWorkDoneProgressCreate
    (WorkDoneProgressCreateParams token)
    (\_ -> pure ())
  progressBegin progress title "Preparing diagnostics and completion"
  return progress

progressBegin :: LspProgress -> T.Text -> T.Text -> LspM () ()
progressBegin progress title msg =
  sendNotification SMethod_Progress $
    ProgressParams (lspProgressToken progress) $
      toJSON (WorkDoneProgressBegin AString title (Just False) (Just msg) Nothing)

progressReportToken :: ProgressToken -> T.Text -> Maybe Int -> LspM () ()
progressReportToken token msg pct =
  sendNotification SMethod_Progress $
    ProgressParams token $
      toJSON (WorkDoneProgressReport AString (Just False) (Just msg) (fromIntegral <$> pct))

progressReport :: LspProgress -> T.Text -> Maybe Int -> LspM () ()
progressReport progress =
  progressReportToken (lspProgressToken progress)

clearProgressQueue :: LspProgress -> LspM () ()
clearProgressQueue progress =
  liftIO $ writeIORef (lspProgressQueue progress) Seq.empty

progressReportImmediate :: LspProgress -> T.Text -> Maybe Int -> LspM () ()
progressReportImmediate progress msg pct = do
  clearProgressQueue progress
  progressReport progress msg pct

progressReportQueued :: LspProgress -> T.Text -> LspM () ()
progressReportQueued progress msg =
  liftIO $ do
    atomicModifyIORef' (lspProgressQueue progress) $ \msgs ->
      let msgs' = msgs |> msg
      in (dropOldestProgress msgs', ())
    void $ tryPutMVar (lspProgressSignal progress) ()

dropOldestProgress :: Seq T.Text -> Seq T.Text
dropOldestProgress msgs =
  Seq.drop (max 0 (Seq.length msgs - progressQueueLimit)) msgs

progressEnd :: LspProgress -> T.Text -> LspM () ()
progressEnd progress msg =
  sendNotification SMethod_Progress $
    ProgressParams (lspProgressToken progress) $
      toJSON (WorkDoneProgressEnd AString (Just msg))

finishProgressOnce :: IORef Bool -> LspProgress -> T.Text -> LspM () ()
finishProgressOnce finished progress msg = do
  alreadyFinished <- liftIO $
    atomicModifyIORef' finished $ \done -> (True, done)
  when (not alreadyFinished) $ do
    clearProgressQueue progress
    liftIO $ killThread (lspProgressWorker progress)
    progressEnd progress msg

moduleLabel :: Compile.GlobalTask -> String
moduleLabel task = Compile.modNameToString (Compile.tkMod (Compile.gtKey task))

backJobLabel :: Compile.BackJob -> String
backJobLabel job =
  Compile.modNameToString (S.modname (Compile.biTypedMod (Compile.bjInput job)))

frontProgressMessage :: Compile.GlobalTask -> Compile.FrontPassProgress -> T.Text
frontProgressMessage task p =
  T.pack $
    passName (Compile.fppPass p) ++ " " ++ moduleLabel task ++ current
  where
    current =
      case Compile.fppCurrent p of
        Nothing -> ""
        Just label -> " (" ++ label ++ ")"
    passName Compile.FrontPassKinds = "Checking kinds for"
    passName Compile.FrontPassTypes = "Typechecking"
    passName Compile.FrontPassHash = "Hashing"

backProgressMessage :: Compile.BackJob -> Compile.BackJobResult -> T.Text
backProgressMessage job result =
  T.pack $
    case result of
      Compile.BackJobOk{} -> "Generated " ++ backJobLabel job
      Compile.BackJobFailed{} -> "Codegen failed for " ++ backJobLabel job

backPassProgressMessage :: Compile.BackJob -> Compile.BackPassProgress -> Maybe T.Text
backPassProgressMessage job (Compile.BackPassStarted pass _ _) =
  Just $
    T.pack ("Generating " ++ backJobLabel job ++ " (" ++ Compile.backPassName pass ++ ")")
backPassProgressMessage _ _ = Nothing

warnBackgroundCompilerLocked :: FilePath -> LspM () ()
warnBackgroundCompilerLocked projRoot = do
  first <- liftIO $ atomicModifyIORef' backgroundCompilerWarnedRef $ \m ->
    if HM.member projRoot m
      then (m, False)
      else (HM.insert projRoot () m, True)
  when first $
    sendNotification SMethod_WindowShowMessage
      (ShowMessageParams MessageType_Warning
        (T.pack ("Another long-running Acton compiler is already running in " ++ projRoot ++
                 "; LSP compilation is disabled until it exits.")))

-- | Run an action only if the compile generation is current.
whenCurrentGen :: Int -> LspM () () -> LspM () ()
whenCurrentGen gen action = do
  current <- liftIO $ readIORef (Compile.csGenRef compileScheduler)
  when (current == gen) action

-- | Notify the client of a compile error for the current generation.
notifyCompileError :: Int -> String -> LspM () ()
notifyCompileError gen msg =
  whenCurrentGen gen $
    sendNotification SMethod_WindowShowMessage
      (ShowMessageParams MessageType_Error (T.pack msg))

-- | Convert Acton positions to LSP ranges with zero-based offsets.
reportRange :: Position -> Range
reportRange (Position (bl, bc) (el, ec) _) =
  let l0 = max 0 (bl - 1)
      c0 = max 0 (bc - 1)
      l1 = max 0 (el - 1)
      c1 = max 0 (ec - 1)
      endC = max c1 (c0 + 1)
  in mkRange (fromIntegral l0) (fromIntegral c0) (fromIntegral l1) (fromIntegral endC)

-- | Select a primary marker position for a diagnostic.
pickPosition :: [(Position, Marker msg)] -> Maybe Position
pickPosition markers =
  case [pos | (pos, This _) <- markers] of
    (p:_) -> Just p
    [] -> fmap fst (listToMaybe markers)

-- | Render notes and hints into a diagnostic message.
notesText :: [Note String] -> String
notesText [] = ""
notesText ns = "\n" ++ unlines (map noteLine ns)
  where
    noteLine (Hint h) = "Hint: " ++ h
    noteLine (Note n) = "Note: " ++ n

-- | Translate an Acton report into an LSP diagnostic.
reportToLsp :: Report String -> LSP.Diagnostic
reportToLsp rep =
  let (sev, msg, markers, notes) = case rep of
        Err _ m ms ns -> (DiagnosticSeverity_Error, m, ms, ns)
        Warn _ m ms ns -> (DiagnosticSeverity_Warning, m, ms, ns)
      pos = pickPosition markers
      range = maybe (mkRange 0 0 0 0) reportRange pos
      fullMsg = T.pack (msg ++ notesText notes)
  in LSP.Diagnostic
       { _range = range
       , _severity = Just sev
       , _code = Nothing
       , _codeDescription = Nothing
       , _source = Just "acton"
       , _message = fullMsg
       , _tags = Nothing
       , _relatedInformation = Nothing
       , _data_ = Nothing
       }

-- | Convert Diagnose diagnostics to LSP diagnostics.
lspDiagnosticsFrom :: [Diagnostic String] -> [LSP.Diagnostic]
lspDiagnosticsFrom diags = concatMap (map reportToLsp . reportsOf) diags

-- | Prepare a compile plan for a file, run it, and publish diagnostics.
runCompile :: Int -> FilePath -> LspM () ()
runCompile gen path = do
  env <- getLspEnv
  progress <- startLspProgress gen path
  finished <- liftIO $ newIORef False
  let finish = finishProgressOnce finished progress
      body = do
        let gopts = lspGlobalOpts
            opts = lspCompileOpts
            sp = overlaySourceProvider overlaysRef
        rootProj <- liftIO $ resolveCompileRoot path opts
        lockOk <- liftIO $ ensureBackgroundCompilerLock rootProj
        if lockOk
          then do
            progressReportImmediate progress "Running Acton front passes" Nothing
            ok <- runCompilePlanWithHooks gen rootProj path sp gopts opts progress
            if ok
              then finish "Acton ready"
              else finish "Acton failed"
          else do
            progressReportImmediate progress "Waiting for another Acton compiler for this project" Nothing
            whenCurrentGen gen $ warnBackgroundCompilerLocked rootProj
            finish "Acton blocked by another compiler"
  liftIO $
    runLspT env body `finally` runLspT env (finish "Acton stopped")

-- | Resolve the project root used for compile locking.
resolveCompileRoot :: FilePath -> C.CompileOptions -> IO FilePath
resolveCompileRoot path opts = do
  mproj <- Compile.findProjectDir path
  case mproj of
    Just proj -> Compile.normalizePathSafe proj
    Nothing -> do
      paths <- Compile.findPaths path opts
      Compile.normalizePathSafe (Compile.projPath paths)

-- | Prepare and run a compile while holding the per-run project work lock.
runCompilePlanWithHooks :: Int -> FilePath -> FilePath -> Source.SourceProvider -> C.GlobalOptions -> C.CompileOptions -> LspProgress -> LspM () Bool
runCompilePlanWithHooks gen rootProj path sp gopts opts progress = do
  env <- getLspEnv
  let taskPath t = Compile.srcFile (Compile.gtPaths t) (Compile.tkMod (Compile.gtKey t))
      publishFor t diags =
        runLspT env $ whenCurrentGen gen $
          publishDiagnosticsFor (taskPath t) diags
      progressFor msg =
        runLspT env $ whenCurrentGen gen $
          progressReportImmediate progress msg Nothing
      progressForQueued msg =
        runLspT env $ whenCurrentGen gen $
          progressReportQueued progress msg
      hooks = Compile.defaultCompileHooks
        { Compile.chOnDiagnostics = \t _ diags ->
            publishFor t (lspDiagnosticsFrom diags)
        , Compile.chOnParseStart = \t ->
            progressForQueued (T.pack ("Parsing " ++ moduleLabel t))
        , Compile.chOnParseDone = \t _ ->
            progressForQueued (T.pack ("Parsed " ++ moduleLabel t))
        , Compile.chOnFrontStart = \t ->
            progressFor (T.pack ("Checking " ++ moduleLabel t))
        , Compile.chOnFrontProgress = \t p ->
            progressForQueued (frontProgressMessage t p)
        , Compile.chOnFrontResult = \t _ ->
            publishFor t [] >>
            progressForQueued (T.pack ("Checked " ++ moduleLabel t))
        , Compile.chOnBackStart = \job ->
            progressForQueued (T.pack ("Generating " ++ backJobLabel job))
        , Compile.chOnBackProgress = \job p ->
            forM_ (backPassProgressMessage job p) progressForQueued
        , Compile.chOnBackDone = \job result ->
            progressForQueued (backProgressMessage job result)
        , Compile.chOnInfo = \_ -> return ()
        }
  progressReportImmediate progress "Preparing Acton project" Nothing
  compileRes <- liftIO $
    Compile.withProjectLock rootProj $
      do
        planE <- runLspT env $
          prepareLspCompilePlan sp gopts opts path
        case planE of
          Left msg ->
            return (Left msg)
          Right (origin, plan) -> do
            progressFor $
              case origin of
                PlanFromCache -> "Acton cached project ready"
                PlanFromDiscovery -> "Acton compile plan ready"
            runRes <- Compile.runCompilePlan sp gopts plan compileScheduler gen hooks
            case runRes of
              Left err ->
                return (Left (Compile.compileFailureMessage err))
              Right (envAcc, _) -> do
                rememberCompletionStates gen plan envAcc
                progressFor "Completion ready"
                let opts' = Compile.ccOpts (Compile.cpContext plan)
                backFailure <-
                  if C.only_build opts'
                    then return Nothing
                    else Compile.backQueueWait (Compile.csBackQueue compileScheduler) gen
                case backFailure of
                  Just failure ->
                    return (Left (Compile.backPassFailureMessage failure))
                  Nothing ->
                    return (Right ())
  case compileRes of
    Left msg -> notifyCompileError gen msg >> return False
    Right () -> return True

-- | Schedule a compile run with a debounce delay.
scheduleCompile :: Int -> FilePath -> LspM () ()
scheduleCompile delay path = do
  env <- getLspEnv
  void $ liftIO $ Compile.startCompile compileScheduler delay $ \gen ->
    runLspT env (runCompile gen path)

-- | Convert an LSP position to a source offset. LSP columns are UTF-16 code
-- units; parser/source offsets are Haskell character offsets.
positionToOffset :: String -> LSP.Position -> Int
positionToOffset src (LSP.Position line character) =
  goLine (fromIntegral line) 0 src
  where
    targetCol = fromIntegral character

    goLine 0 off rest = off + columnOffset targetCol rest
    goLine _ off [] = off
    goLine n off (c:cs)
      | c == '\n' = goLine (n - 1) (off + 1) cs
      | otherwise = goLine n (off + 1) cs

    columnOffset 0 _ = 0
    columnOffset _ [] = 0
    columnOffset col (c:cs)
      | c == '\n' = 0
      | units > col = 0
      | otherwise = 1 + columnOffset (col - units) cs
      where units = if ord c > 0xffff then 2 else 1

completionKindToLsp :: Completion.CompletionKind -> CompletionItemKind
completionKindToLsp kind =
  case kind of
    Completion.CompletionField -> CompletionItemKind_Field
    Completion.CompletionMethod -> CompletionItemKind_Method
    Completion.CompletionProperty -> CompletionItemKind_Property
    Completion.CompletionValue -> CompletionItemKind_Value
    Completion.CompletionKeyword -> CompletionItemKind_Keyword

lspCompletionItem :: Completion.Completion -> CompletionItem
lspCompletionItem item =
  CompletionItem
    { _label = T.pack (Completion.completionLabel item)
    , _labelDetails = Nothing
    , _kind = Just (completionKindToLsp (Completion.completionKind item))
    , _tags = Nothing
    , _detail = T.pack <$> Completion.completionDetail item
    , _documentation = Nothing
    , _deprecated = Nothing
    , _preselect = Nothing
    , _sortText = Nothing
    , _filterText = Nothing
    , _insertText = completionInsertText item
    , _insertTextFormat = completionInsertTextFormat item
    , _insertTextMode = Nothing
    , _textEdit = Nothing
    , _textEditText = Nothing
    , _additionalTextEdits = Nothing
    , _commitCharacters = Nothing
    , _command = completionCommand item
    , _data_ = Nothing
    }

completionInsertText :: Completion.Completion -> Maybe T.Text
completionInsertText item =
  case Completion.completionKind item of
    Completion.CompletionMethod ->
      Just (T.pack (Completion.completionLabel item ++ "($0)"))
    Completion.CompletionKeyword ->
      Just (T.pack (Completion.completionLabel item ++ "$0"))
    _ -> Nothing

completionInsertTextFormat :: Completion.Completion -> Maybe InsertTextFormat
completionInsertTextFormat item =
  case Completion.completionKind item of
    Completion.CompletionMethod -> Just InsertTextFormat_Snippet
    Completion.CompletionKeyword -> Just InsertTextFormat_Snippet
    _ -> Nothing

completionCommand :: Completion.Completion -> Maybe Command
completionCommand item =
  case Completion.completionKind item of
    Completion.CompletionMethod ->
      Just Command
        { _title = "Trigger parameter hints"
        , _command = "editor.action.triggerParameterHints"
        , _arguments = Nothing
        }
    _ -> Nothing

forceLspCompletions :: [Completion.Completion] -> [Completion.Completion]
forceLspCompletions items =
  sum [ length (Completion.completionLabel item)
      + maybe 0 length (Completion.completionDetail item)
      + completionKindCode (Completion.completionKind item)
      | item <- items
      ] `seq` items

forceLspSignatures :: [Completion.CallSignature] -> [Completion.CallSignature]
forceLspSignatures sigs =
  sum [ length (Completion.callSignatureLabel sig)
      + Completion.callSignatureActiveParameter sig
      + sum (map (length . Completion.signatureParameterLabel) (Completion.callSignatureParameters sig))
      | sig <- sigs
      ] `seq` sigs

forceLspHover :: Maybe Completion.HoverInfo -> Maybe Completion.HoverInfo
forceLspHover info =
  case info of
    Nothing -> Nothing
    Just hover ->
      length (Completion.hoverLabel hover)
      + length (Completion.hoverDetail hover)
      + maybe 0 length (Completion.hoverDocumentation hover) `seq` info

completionKindCode :: Completion.CompletionKind -> Int
completionKindCode kind =
  case kind of
    Completion.CompletionField -> 1
    Completion.CompletionMethod -> 2
    Completion.CompletionProperty -> 3
    Completion.CompletionValue -> 4
    Completion.CompletionKeyword -> 5

tryLspIO :: IO a -> IO (Maybe a)
tryLspIO action = do
  res <- try action
  case res of
    Left err
      | Just (_ :: AsyncException) <- fromException err ->
          throwIO err
      | otherwise ->
          return Nothing
    Right value -> return (Just value)

signatureHelpFor :: FilePath -> LSP.Position -> LspM () (SignatureHelp |? Null)
signatureHelpFor path pos = do
  mstate <- liftIO $ loadCompletionStateFor path
  case mstate of
    Nothing -> return (InR Null)
    Just state -> do
      snapRes <- liftIO $
        (try (Source.readSource (overlaySourceProvider overlaysRef) path) :: IO (Either IOError Source.SourceSnapshot))
      case snapRes of
        Left _ -> return (InR Null)
        Right snap -> do
          let src = Source.ssText snap
              cursor = positionToOffset src pos
          msigs <- liftIO $
            tryLspIO $ do
              env <- completionEnvFor state path src
              evaluate (forceLspSignatures (Completion.callSignaturesWithEnv env src cursor))
          return $
            case msigs of
              Just (sig:_) -> InL (lspSignatureHelp sig)
              _ -> InR Null

hoverFor :: FilePath -> LSP.Position -> LspM () (Hover |? Null)
hoverFor path pos = do
  mstate <- liftIO $ loadCompletionStateFor path
  case mstate of
    Nothing -> return (InR Null)
    Just state -> do
      snapRes <- liftIO $
        (try (Source.readSource (overlaySourceProvider overlaysRef) path) :: IO (Either IOError Source.SourceSnapshot))
      case snapRes of
        Left _ -> return (InR Null)
        Right snap -> do
          let src = Source.ssText snap
              cursor = positionToOffset src pos
          minfo <- liftIO $
            tryLspIO $ do
              env <- completionEnvFor state path src
              evaluate (forceLspHover (Completion.hoverInfoWithEnv env src cursor))
          return $
            case minfo of
              Just (Just info) -> InL (lspHover info)
              _ -> InR Null

lspHover :: Completion.HoverInfo -> Hover
lspHover info =
  Hover
    { _contents = InL MarkupContent
        { _kind = MarkupKind_Markdown
        , _value = T.pack (hoverMarkdown info)
        }
    , _range = Nothing
    }
  where
    hoverMarkdown hover =
      "```acton\n"
      ++ Completion.hoverDetail hover
      ++ "\n```"
      ++ hoverDoc hover

    hoverDoc hover =
      case Completion.hoverDocumentation hover of
        Nothing -> ""
        Just doc -> "\n\n" ++ doc

lspSignatureHelp :: Completion.CallSignature -> SignatureHelp
lspSignatureHelp sig =
  SignatureHelp
    { _signatures = [lspSignatureInformation sig]
    , _activeSignature = Just 0
    , _activeParameter = active
    }
  where
    params = Completion.callSignatureParameters sig
    active =
      if null params
        then Nothing
        else Just (InL (fromIntegral (Completion.callSignatureActiveParameter sig)))

lspSignatureInformation :: Completion.CallSignature -> SignatureInformation
lspSignatureInformation sig =
  SignatureInformation
    { _label = T.pack (Completion.callSignatureLabel sig)
    , _documentation = Nothing
    , _parameters = Just (map lspParameterInformation (Completion.callSignatureParameters sig))
    , _activeParameter = active
    }
  where
    params = Completion.callSignatureParameters sig
    active =
      if null params
        then Nothing
        else Just (InL (fromIntegral (Completion.callSignatureActiveParameter sig)))

lspParameterInformation :: Completion.SignatureParameter -> ParameterInformation
lspParameterInformation param =
  ParameterInformation
    { _label = InL (T.pack (Completion.signatureParameterLabel param))
    , _documentation = Nothing
    }

completionEnvFor :: CompletionState -> FilePath -> String -> IO Env.Env0
completionEnvFor state path src = do
  importKey <- Completion.completionImportKey path src
  mcache <- HM.lookup path <$> readIORef completionEnvCacheRef
  case mcache of
    Just cache
      | cachedCompletionStateGen cache == completionStateGen state
      , cachedCompletionImportKey cache == importKey ->
          return (cachedCompletionEnv cache)
    _ -> do
      env <- Completion.prepareCompletionEnv
        (completionEnv state)
        (completionSearchPath state)
        (completionModuleName state)
        path
        src
      let cache = CompletionEnvCache
            { cachedCompletionStateGen = completionStateGen state
            , cachedCompletionImportKey = importKey
            , cachedCompletionEnv = env
            }
      atomicModifyIORef' completionEnvCacheRef $ \m ->
        (HM.insert path cache m, ())
      return env

invalidateCompletionEnvCache :: IO ()
invalidateCompletionEnvCache =
  writeIORef completionEnvCacheRef HM.empty

completionItemsFor :: FilePath -> LSP.Position -> LspM () [CompletionItem]
completionItemsFor path pos = do
  mstate <- liftIO $ loadCompletionStateFor path
  case mstate of
    Nothing -> return []
    Just state -> do
      snapRes <- liftIO $
        (try (Source.readSource (overlaySourceProvider overlaysRef) path) :: IO (Either IOError Source.SourceSnapshot))
      case snapRes of
        Left _ -> return []
        Right snap -> do
          let src = Source.ssText snap
              cursor = positionToOffset src pos
          mitems <- liftIO $
            tryLspIO $ do
              env <- completionEnvFor state path src
              let items =
                    Completion.memberCompletionsWithEnv env src cursor ++
                    Completion.argumentCompletionsWithEnv env src cursor
              evaluate (forceLspCompletions items)
          return (maybe [] (map lspCompletionItem) mitems)

-- | Resolve and normalize a document URI to a file path.
resolvePath :: Uri -> LspM () (Maybe FilePath)
resolvePath uri =
  case uriToFilePath uri of
    Nothing -> return Nothing
    Just path -> liftIO $ do
      tryLspIO (Compile.normalizePathSafe path)

-- | LSP notification handlers for open/change/close events.
handlers :: Handlers (LspM ())
handlers =
  mconcat
    [ notificationHandler SMethod_Initialized $ \_ -> pure ()
    , notificationHandler SMethod_WorkspaceDidChangeConfiguration $ \_ -> pure ()
    , requestHandler SMethod_TextDocumentCompletion $ \(TRequestMessage _ _ _ params) respond -> do
        let CompletionParams (TextDocumentIdentifier uri) pos _ _ _ = params
        items <- resolvePath uri >>= \case
          Nothing -> return []
          Just path -> completionItemsFor path pos
        respond (Right (InL items))
    , requestHandler SMethod_TextDocumentSignatureHelp $ \(TRequestMessage _ _ _ params) respond -> do
        let SignatureHelpParams (TextDocumentIdentifier uri) pos _ _ = params
        help <- resolvePath uri >>= \case
          Nothing -> return (InR Null)
          Just path -> signatureHelpFor path pos
        respond (Right help)
    , requestHandler SMethod_TextDocumentHover $ \(TRequestMessage _ _ _ params) respond -> do
        let HoverParams (TextDocumentIdentifier uri) pos _ = params
        hover <- resolvePath uri >>= \case
          Nothing -> return (InR Null)
          Just path -> hoverFor path pos
        respond (Right hover)
    , notificationHandler SMethod_TextDocumentDidOpen $ \(TNotificationMessage _ _ (DidOpenTextDocumentParams doc)) -> do
        let TextDocumentItem{_uri=uri,_text=txt} = doc
        resolvePath uri >>= \case
          Nothing -> pure ()
          Just path -> do
            liftIO $ rememberUri path uri
            liftIO $ updateOverlay path txt
            scheduleCompile 0 path
    , notificationHandler SMethod_TextDocumentDidChange $ \(TNotificationMessage _ _ (DidChangeTextDocumentParams vdoc changes)) -> do
        let VersionedTextDocumentIdentifier{_uri=uri} = vdoc
        resolvePath uri >>= \case
          Nothing -> pure ()
          Just path ->
            case changes of
              [] -> pure ()
              cs -> do
                let TextDocumentContentChangeEvent change = last cs
                    txt = case change of
                      InL TextDocumentContentChangePartial{_text=txt'} -> txt'
                      InR TextDocumentContentChangeWholeDocument{_text=txt'} -> txt'
                liftIO $ rememberUri path uri
                liftIO $ updateOverlay path txt
                scheduleCompile debounceMicros path
    , notificationHandler SMethod_TextDocumentDidClose $ \(TNotificationMessage _ _ (DidCloseTextDocumentParams (TextDocumentIdentifier uri))) -> do
        resolvePath uri >>= \case
          Nothing -> pure ()
          Just path -> do
            liftIO $ removeOverlay path
            publishDiagnosticsFor path []
            liftIO $ forgetUri path
    ]

-- | Start the Acton LSP server with the configured handlers.
main :: IO Int
main =
  runServer serverDef `finally` releaseBackgroundCompilerLocks
  where
    serverDef =
      ServerDefinition
        { parseConfig = const $ const $ Right ()
        , onConfigChange = const $ pure ()
        , defaultConfig = ()
        , configSection = "acton"
        , doInitialize = \env _req -> pure $ Right env
        , staticHandlers = \_caps -> handlers
        , interpretHandler = \env -> Iso (runLspT env) liftIO
        , options = defaultOptions
            { optTextDocumentSync = Just syncOptions
            , optCompletionTriggerCharacters = Just ['.', '(', ',']
            , optSignatureHelpTriggerCharacters = Just ['(', ',']
            , optSignatureHelpRetriggerCharacters = Just [',']
            }
        }
    syncOptions = TextDocumentSyncOptions
      { _openClose = Just True
      , _change = Just TextDocumentSyncKind_Full
      , _willSave = Nothing
      , _willSaveWaitUntil = Nothing
      , _save = Nothing
      }
