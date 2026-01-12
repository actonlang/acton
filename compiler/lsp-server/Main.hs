{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (try, finally)
import Control.Monad (void, when, forM_)
import Control.Monad.IO.Class
import Data.IORef
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Conc (getNumCapabilities)
import System.IO.Unsafe (unsafePerformIO)
import System.FileLock (FileLock)

import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types hiding (Diagnostic, Position)
import qualified Language.LSP.Protocol.Types as LSP
import Language.LSP.Server

import qualified Acton.Compile as Compile
import qualified Acton.CommandLineParser as C
import qualified Acton.SourceProvider as Source

import Error.Diagnose (Diagnostic)
import Error.Diagnose.Diagnostic (reportsOf)
import Error.Diagnose.Report (Marker(..), Note(..), Report(Warn, Err))
import Error.Diagnose.Position (Position(..))


type OverlayMap = HM.HashMap FilePath Source.SourceSnapshot
type UriMap = HM.HashMap FilePath Uri
type CompileOwnerMap = HM.HashMap FilePath (FileLock, FilePath)
type CompileOwnerWarned = HM.HashMap FilePath ()

-- | Debounce delay (microseconds) before recompiling on edits.
debounceMicros :: Int
debounceMicros = 200000

{-# NOINLINE overlaysRef #-}
-- | Global in-memory buffer map used as a source overlay.
overlaysRef :: IORef OverlayMap
overlaysRef = unsafePerformIO (newIORef HM.empty)

{-# NOINLINE uriByPathRef #-}
-- | Map canonical paths to the client-provided URIs for diagnostics.
uriByPathRef :: IORef UriMap
uriByPathRef = unsafePerformIO (newIORef HM.empty)

{-# NOINLINE compileOwnerLocksRef #-}
-- | Map project roots to held compile-owner locks.
compileOwnerLocksRef :: IORef CompileOwnerMap
compileOwnerLocksRef = unsafePerformIO (newIORef HM.empty)

{-# NOINLINE compileOwnerWarnedRef #-}
-- | Remember which projects have already warned about an active compiler.
compileOwnerWarnedRef :: IORef CompileOwnerWarned
compileOwnerWarnedRef = unsafePerformIO (newIORef HM.empty)

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

ensureCompileOwnerLock :: FilePath -> IO Bool
ensureCompileOwnerLock projRoot = do
  locks <- readIORef compileOwnerLocksRef
  case HM.lookup projRoot locks of
    Just _ -> return True
    Nothing -> do
      mlock <- Compile.tryCompileOwnerLock projRoot
      case mlock of
        Nothing -> return False
        Just lockInfo -> do
          atomicModifyIORef' compileOwnerLocksRef $ \m -> (HM.insert projRoot lockInfo m, ())
          atomicModifyIORef' compileOwnerWarnedRef $ \m -> (HM.delete projRoot m, ())
          return True

releaseCompileOwnerLocks :: IO ()
releaseCompileOwnerLocks = do
  locks <- readIORef compileOwnerLocksRef
  forM_ (HM.elems locks) Compile.releaseCompileOwnerLock
  writeIORef compileOwnerLocksRef HM.empty

warnCompileOwnerLocked :: FilePath -> LspM () ()
warnCompileOwnerLocked projRoot = do
  first <- liftIO $ atomicModifyIORef' compileOwnerWarnedRef $ \m ->
    if HM.member projRoot m
      then (m, False)
      else (HM.insert projRoot () m, True)
  when first $
    sendNotification SMethod_WindowShowMessage
      (ShowMessageParams MessageType_Warning
        (T.pack ("Another Acton compiler is already running in " ++ projRoot ++
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
  let gopts = lspGlobalOpts
      opts = lspCompileOpts
      sp = overlaySourceProvider overlaysRef
  planE <- liftIO $ (try $ do
    Compile.prepareCompilePlan sp gopts compileScheduler opts [path] False (Just [path])
    ) :: LspM () (Either Compile.ProjectError Compile.CompilePlan)
  case planE of
    Left (Compile.ProjectError msg) -> notifyCompileError gen msg
    Right plan -> do
      let rootProj = Compile.ccRootProj (Compile.cpContext plan)
      withCompileOwnerLock gen rootProj $
        runCompilePlanWithHooks gen sp gopts plan

-- | Run a compile plan after verifying we own the project lock.
runCompilePlanWithHooks :: Int -> Source.SourceProvider -> C.GlobalOptions -> Compile.CompilePlan -> LspM () ()
runCompilePlanWithHooks gen sp gopts plan = do
  env <- getLspEnv
  let taskPath t = Compile.srcFile (Compile.gtPaths t) (Compile.tkMod (Compile.gtKey t))
      publishFor t diags =
        runLspT env $ whenCurrentGen gen $
          publishDiagnosticsFor (taskPath t) diags
      hooks = Compile.defaultCompileHooks
        { Compile.chOnDiagnostics = \t _ diags ->
            publishFor t (lspDiagnosticsFrom diags)
        , Compile.chOnFrontResult = \t _ ->
            publishFor t []
        , Compile.chOnInfo = \_ -> return ()
        }
  compileRes <- liftIO $ Compile.runCompilePlan sp gopts plan compileScheduler gen hooks
  case compileRes of
    Left err -> notifyCompileError gen (Compile.compileFailureMessage err)
    Right _ -> return ()

-- | Execute an action only when the compile-owner lock is available.
withCompileOwnerLock :: Int -> FilePath -> LspM () () -> LspM () ()
withCompileOwnerLock gen rootProj action = do
  lockOk <- liftIO $ ensureCompileOwnerLock rootProj
  if lockOk
    then action
    else whenCurrentGen gen $ warnCompileOwnerLocked rootProj

-- | Schedule a compile run with a debounce delay.
scheduleCompile :: Int -> FilePath -> LspM () ()
scheduleCompile delay path = do
  env <- getLspEnv
  void $ liftIO $ Compile.startCompile compileScheduler delay $ \gen ->
    runLspT env (runCompile gen path)

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
  runServer serverDef `finally` releaseCompileOwnerLocks
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
        , options = defaultOptions { optTextDocumentSync = Just syncOptions }
        }
    syncOptions = TextDocumentSyncOptions
      { _openClose = Just True
      , _change = Just TextDocumentSyncKind_Full
      , _willSave = Nothing
      , _willSaveWaitUntil = Nothing
      , _save = Nothing
      }
