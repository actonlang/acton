{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar
import Control.Exception (SomeException, try)
import Control.Monad (forever, forM_, void, when)
import Control.Monad.IO.Class
import Data.IORef
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
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

debounceMicros :: Int
debounceMicros = 200000

{-# NOINLINE overlaysRef #-}
overlaysRef :: IORef OverlayMap
overlaysRef = unsafePerformIO (newIORef HM.empty)

{-# NOINLINE compileAsyncRef #-}
compileAsyncRef :: MVar (Maybe (Async ()))
compileAsyncRef = unsafePerformIO (newMVar Nothing)

{-# NOINLINE compileGenRef #-}
compileGenRef :: IORef Int
compileGenRef = unsafePerformIO (newIORef 0)

{-# NOINLINE backQueue #-}
backQueue :: Chan (Int, Compile.BackJob)
backQueue = unsafePerformIO newChan

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

startBackWorker :: IO ()
startBackWorker = void $ forkIO $ forever $ do
  (gen, job) <- readChan backQueue
  current <- readIORef compileGenRef
  when (gen == current) $ do
    _ <- (try $ void $ Compile.runBackPasses lspGlobalOpts (Compile.bjOpts job) (Compile.bjPaths job) (Compile.bjInput job))
            :: IO (Either SomeException ())
    return ()

whenCurrentGen :: Int -> LspM () () -> LspM () ()
whenCurrentGen gen action = do
  current <- liftIO $ readIORef compileGenRef
  when (current == gen) action

publishDiagnosticsFor :: FilePath -> [LSP.Diagnostic] -> LspM () ()
publishDiagnosticsFor path diags = do
  let uri = filePathToUri path
  sendNotification SMethod_TextDocumentPublishDiagnostics (PublishDiagnosticsParams uri Nothing diags)

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
    pathsRoot <- Compile.findPaths path opts
    rootProj <- Compile.normalizePathSafe (Compile.projPath pathsRoot)
    sysAbs <- Compile.normalizePathSafe (Compile.sysPath pathsRoot)
    projMap <- if Compile.isTmp pathsRoot
      then do
        let ctx = Compile.ProjCtx
              { Compile.projRoot = rootProj
              , Compile.projOutDir = Compile.projOut pathsRoot
              , Compile.projTypesDir = Compile.projTypes pathsRoot
              , Compile.projSrcDir = Compile.srcDir pathsRoot
              , Compile.projSysPath = sysAbs
              , Compile.projSysTypes = joinPath [sysAbs, "base", "out", "types"]
              , Compile.projBuildSpec = Nothing
              , Compile.projLocks = joinPath [Compile.projPath pathsRoot, ".actonc.lock"]
              , Compile.projDeps = []
              }
        return (M.singleton rootProj ctx)
      else Compile.discoverProjects sysAbs rootProj (C.dep_overrides opts)
    (globalTasks, _) <- Compile.buildGlobalTasks sp gopts opts projMap (Just [path])
    neededTasks <- Compile.selectNeededTasks pathsRoot rootProj globalTasks [path]
    return (pathsRoot, rootProj, neededTasks)
    ) :: LspM () (Either Compile.ProjectError (Compile.Paths, FilePath, [Compile.GlobalTask]))
  case res of
    Left (Compile.ProjectError msg) ->
      whenCurrentGen gen $
        sendNotification SMethod_WindowShowMessage (ShowMessageParams MessageType_Error (T.pack msg))
    Right (pathsRoot, rootProj, neededTasks) -> do
      env <- getLspEnv
      let callbacks = Compile.defaultCompileCallbacks
            { Compile.ccOnDiagnostics = \t _ diags ->
                runLspT env $ whenCurrentGen gen $ do
                  let filePath = Compile.srcFile (Compile.gtPaths t) (Compile.tkMod (Compile.gtKey t))
                  publishDiagnosticsFor filePath (lspDiagnosticsFrom diags)
            , Compile.ccOnFrontResult = \t _ _ ->
                runLspT env $ whenCurrentGen gen $ do
                  let filePath = Compile.srcFile (Compile.gtPaths t) (Compile.tkMod (Compile.gtKey t))
                  publishDiagnosticsFor filePath []
            , Compile.ccOnBackJob = \job -> do
                current <- readIORef compileGenRef
                when (current == gen) $ writeChan backQueue (gen, job)
            , Compile.ccOnInfo = \_ -> return ()
            }
      compileRes <- liftIO $ Compile.compileTasks sp gopts opts pathsRoot rootProj neededTasks callbacks
      case compileRes of
        Left err -> whenCurrentGen gen $
          sendNotification SMethod_WindowShowMessage (ShowMessageParams MessageType_Error (T.pack (Compile.compileFailureMessage err)))
        Right _ -> return ()

startCompile :: Int -> FilePath -> LspM () ()
startCompile delay path = do
  env <- getLspEnv
  gen <- liftIO $ atomicModifyIORef' compileGenRef $ \g -> let g' = g + 1 in (g', g')
  liftIO $ modifyMVar_ compileAsyncRef $ \m -> do
    forM_ m cancel
    a <- async $ do
      when (delay > 0) $ threadDelay delay
      current <- readIORef compileGenRef
      when (current == gen) $ runLspT env (runCompile gen path)
    return (Just a)

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
            liftIO $ updateOverlay path txt
            startCompile 0 path
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
                liftIO $ updateOverlay path txt
                startCompile debounceMicros path
    , notificationHandler SMethod_TextDocumentDidClose $ \(TNotificationMessage _ _ (DidCloseTextDocumentParams (TextDocumentIdentifier uri))) -> do
        resolvePath uri >>= \case
          Nothing -> pure ()
          Just path -> do
            liftIO $ removeOverlay path
            publishDiagnosticsFor path []
    ]

main :: IO Int
main = do
  startBackWorker
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
