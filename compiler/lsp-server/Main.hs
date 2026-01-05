{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (try)
import Control.Monad (void, when)
import Control.Monad.IO.Class
import Data.IORef
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Conc (getNumCapabilities)
import System.FilePath.Posix (joinPath)
import System.IO.Unsafe (unsafePerformIO)

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

debounceMicros :: Int
debounceMicros = 200000

{-# NOINLINE overlaysRef #-}
overlaysRef :: IORef OverlayMap
overlaysRef = unsafePerformIO (newIORef HM.empty)

{-# NOINLINE uriByPathRef #-}
-- | Map canonical paths to the client-provided URIs for diagnostics.
uriByPathRef :: IORef UriMap
uriByPathRef = unsafePerformIO (newIORef HM.empty)

{-# NOINLINE compileScheduler #-}
compileScheduler :: Compile.CompileScheduler
compileScheduler = unsafePerformIO $ do
  nCaps <- getNumCapabilities
  let maxParallel = max 1 (if C.jobs lspGlobalOpts > 0 then C.jobs lspGlobalOpts else nCaps)
  Compile.newCompileScheduler lspGlobalOpts maxParallel

lspGlobalOpts :: C.GlobalOptions
lspGlobalOpts =
  C.GlobalOptions
    { color = C.Never
    , quiet = True
    , sub = False
    , timing = False
    , tty = False
    , verbose = False
    , verboseZig = False
    , jobs = 0
    }

lspCompileOpts :: C.CompileOptions
lspCompileOpts =
  Compile.defaultCompileOptions
    { C.skip_build = True
    , C.only_build = False
    , C.test = False
    }


overlaySourceProvider :: IORef OverlayMap -> Source.SourceProvider
overlaySourceProvider ref =
  let disk = Source.diskSourceProvider
  in Source.SourceProvider
       { Source.spReadOverlay = \path -> HM.lookup path <$> readIORef ref
       , Source.spReadFile = Source.spReadFile disk
       , Source.spGetModTime = Source.spGetModTime disk
       }

updateOverlay :: FilePath -> T.Text -> IO ()
updateOverlay path txt =
  let snap = Source.SourceSnapshot
        { Source.ssText = T.unpack txt
        , Source.ssBytes = TE.encodeUtf8 txt
        , Source.ssIsOverlay = True
        }
  in atomicModifyIORef' overlaysRef $ \m -> (HM.insert path snap m, ())

removeOverlay :: FilePath -> IO ()
removeOverlay path =
  atomicModifyIORef' overlaysRef $ \m -> (HM.delete path m, ())

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
-- | Run an action only if the compile generation is current.
whenCurrentGen :: Int -> LspM () () -> LspM () ()
whenCurrentGen gen action = do
  current <- liftIO $ readIORef (Compile.csGenRef compileScheduler)
  when (current == gen) action

reportRange :: Position -> Range
reportRange (Position (bl, bc) (el, ec) _) =
  let l0 = max 0 (bl - 1)
      c0 = max 0 (bc - 1)
      l1 = max 0 (el - 1)
      c1 = max 0 (ec - 1)
      endC = max c1 (c0 + 1)
  in mkRange (fromIntegral l0) (fromIntegral c0) (fromIntegral l1) (fromIntegral endC)

pickPosition :: [(Position, Marker msg)] -> Maybe Position
pickPosition markers =
  case [pos | (pos, This _) <- markers] of
    (p:_) -> Just p
    [] -> fmap fst (listToMaybe markers)

notesText :: [Note String] -> String
notesText [] = ""
notesText ns = "\n" ++ unlines (map noteLine ns)
  where
    noteLine (Hint h) = "Hint: " ++ h
    noteLine (Note n) = "Note: " ++ n

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

lspDiagnosticsFrom :: [Diagnostic String] -> [LSP.Diagnostic]
lspDiagnosticsFrom diags = concatMap (map reportToLsp . reportsOf) diags

runCompile :: Int -> FilePath -> LspM () ()
runCompile gen path = do
  let gopts = lspGlobalOpts
      opts = lspCompileOpts
      sp = overlaySourceProvider overlaysRef
  res <- liftIO $ (try $ do
    ctx <- Compile.prepareCompileContext opts [path]
    specChanged <- Compile.checkBuildSpecChange compileScheduler (Compile.ccBuildStamp ctx)
    when specChanged $
      Compile.fetchDependencies gopts (Compile.ccPathsRoot ctx) (Compile.ccDepOverrides ctx)
    let mChanged = if specChanged then Nothing else Just [path]
    Compile.prepareCompilePlan sp gopts ctx [path] False mChanged
    ) :: LspM () (Either Compile.ProjectError Compile.CompilePlan)
  case res of
    Left (Compile.ProjectError msg) ->
      whenCurrentGen gen $
        sendNotification SMethod_WindowShowMessage (ShowMessageParams MessageType_Error (T.pack msg))
    Right plan -> do
      env <- getLspEnv
      let hooks = Compile.defaultCompileHooks
            { Compile.chOnDiagnostics = \t _ diags ->
                runLspT env $ whenCurrentGen gen $ do
                  let filePath = Compile.srcFile (Compile.gtPaths t) (Compile.tkMod (Compile.gtKey t))
                  publishDiagnosticsFor filePath (lspDiagnosticsFrom diags)
            , Compile.chOnFrontResult = \t _ ->
                runLspT env $ whenCurrentGen gen $ do
                  let filePath = Compile.srcFile (Compile.gtPaths t) (Compile.tkMod (Compile.gtKey t))
                  publishDiagnosticsFor filePath []
            , Compile.chOnInfo = \_ -> return ()
            }
      compileRes <- liftIO $ Compile.runCompilePlan sp gopts plan compileScheduler gen hooks
      case compileRes of
        Left err -> whenCurrentGen gen $
          sendNotification SMethod_WindowShowMessage (ShowMessageParams MessageType_Error (T.pack (Compile.compileFailureMessage err)))
        Right _ -> return ()

scheduleCompile :: Int -> FilePath -> LspM () ()
scheduleCompile delay path = do
  env <- getLspEnv
  void $ liftIO $ Compile.startCompile compileScheduler delay $ \gen ->
    runLspT env (runCompile gen path)

resolvePath :: Uri -> LspM () (Maybe FilePath)
resolvePath uri =
  case uriToFilePath uri of
    Nothing -> return Nothing
    Just path -> liftIO $ Just <$> Compile.normalizePathSafe path

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

main :: IO Int
main = do
  runServer $
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
  where
    syncOptions = TextDocumentSyncOptions
      { _openClose = Just True
      , _change = Just TextDocumentSyncKind_Full
      , _willSave = Nothing
      , _willSaveWaitUntil = Nothing
      , _save = Nothing
      }
