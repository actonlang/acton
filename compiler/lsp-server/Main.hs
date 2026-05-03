{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

import Control.Exception (SomeException, finally, try)
import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, tryPutMVar)
import Control.Monad (forM, forM_, forever, void, when)
import Control.Monad.IO.Class
import Data.Aeson (toJSON)
import Data.Char (ord)
import Data.IORef
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Sequence (Seq, ViewL(..), (|>))
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Conc (getNumCapabilities)
import System.FilePath (takeFileName)
import System.IO.Unsafe (unsafePerformIO)

import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types hiding (Diagnostic, Position)
import qualified Language.LSP.Protocol.Types as LSP
import Language.LSP.Server

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

data CompletionState = CompletionState
  { completionEnv :: Env.Env0
  , completionSearchPath :: [FilePath]
  , completionModuleName :: S.ModName
  }

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

rememberCompletionStates :: Compile.CompilePlan -> Env.Env0 -> IO ()
rememberCompletionStates plan env = do
  entries <- forM (Compile.cpGlobalTasks plan) $ \t -> do
    let key = Compile.gtKey t
        paths = Compile.gtPaths t
        mn = Compile.tkMod key
    path <- Compile.normalizePathSafe (Compile.srcFile paths mn)
    return (path, CompletionState env (Compile.searchPath paths) mn)
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

backProgressMessage :: Compile.BackJob -> Compile.BackJobResult -> T.Text
backProgressMessage job result =
  T.pack $
    case result of
      Compile.BackJobOk{} -> "Generated " ++ backJobLabel job
      Compile.BackJobFailed{} -> "Codegen failed for " ++ backJobLabel job

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
        , Compile.chOnBackDone = \job result ->
            progressForQueued (backProgressMessage job result)
        , Compile.chOnInfo = \_ -> return ()
        }
  progressReportImmediate progress "Discovering Acton project" Nothing
  compileRes <- liftIO $
    Compile.withProjectLock rootProj $
      do
        planE <- (try $ do
          Compile.prepareCompilePlan sp gopts compileScheduler opts [path] False (Just [path])
          ) :: IO (Either Compile.ProjectError Compile.CompilePlan)
        case planE of
          Left (Compile.ProjectError msg) ->
            return (Left msg)
          Right plan -> do
            progressFor "Acton compile plan ready"
            runRes <- Compile.runCompilePlan sp gopts plan compileScheduler gen hooks
            case runRes of
              Left err ->
                return (Left (Compile.compileFailureMessage err))
              Right (envAcc, _) -> do
                rememberCompletionStates plan envAcc
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
    _ -> Nothing

completionInsertTextFormat :: Completion.Completion -> Maybe InsertTextFormat
completionInsertTextFormat item =
  case Completion.completionKind item of
    Completion.CompletionMethod -> Just InsertTextFormat_Snippet
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
          sigs <- liftIO $
            Completion.callSignatures
              (completionEnv state)
              (completionSearchPath state)
              (completionModuleName state)
              path
              src
              cursor
          return $
            case sigs of
              [] -> InR Null
              sig:_ -> InL (lspSignatureHelp sig)

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
          items <- liftIO $
            Completion.memberCompletions
              (completionEnv state)
              (completionSearchPath state)
              (completionModuleName state)
              path
              src
              cursor
          return (map lspCompletionItem items)

-- | Resolve and normalize a document URI to a file path.
resolvePath :: Uri -> LspM () (Maybe FilePath)
resolvePath uri =
  case uriToFilePath uri of
    Nothing -> return Nothing
    Just path -> liftIO $ Just <$> Compile.normalizePathSafe path

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
            , optCompletionTriggerCharacters = Just ['.']
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
