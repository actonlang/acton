{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class
import qualified Control.Exception as E
import qualified Control.Monad.State.Strict as St
import qualified Data.List.NonEmpty as NE
import Data.List (last)
import Data.Maybe (fromMaybe)
import Data.Aeson (toJSON, Value(..))
import qualified Data.Aeson.KeyMap as KM
import Data.IORef
import qualified Data.HashMap.Strict as HM
import Data.Time.Clock.POSIX (getPOSIXTime, POSIXTime)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text as T
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Text.Megaparsec (runParser, PosState(..), reachOffset)
import Text.Megaparsec.Error (ParseErrorBundle(..), ParseError, errorOffset, bundleErrors, parseErrorTextPretty)
import Text.Megaparsec.Pos (SourcePos(..), mkPos, unPos)

import qualified Acton.Syntax as S
import qualified Acton.Parser as P
import Utils (SrcLoc(..))
import qualified Acton.Diagnostics as AD
import Error.Diagnose.Report (Note(..))

-- Convert a Megaparsec error offset into a 0-based LSP Range (single char)
mkRangeFromOffset :: Int -> String -> Range
mkRangeFromOffset off src =
  let initial = PosState { pstateInput = src
                         , pstateOffset = 0
                         , pstateSourcePos = SourcePos "" (mkPos 1) (mkPos 1)
                         , pstateTabWidth = mkPos 8
                         , pstateLinePrefix = ""
                         }
      (_, st) = reachOffset off initial
      sp = pstateSourcePos st
      l0 = max 0 (unPos (sourceLine sp) - 1)
      c0 = max 0 (unPos (sourceColumn sp) - 1)
  in mkRange (fromIntegral l0) (fromIntegral c0) (fromIntegral l0) (fromIntegral (c0 + 1))

-- Create a range from a SrcLoc (start,end offsets)
rangeFromLoc :: SrcLoc -> String -> Range
rangeFromLoc NoLoc src = mkRangeFromOffset 0 src
rangeFromLoc (Loc startOff endOff) src =
  let initial = PosState { pstateInput = src
                         , pstateOffset = 0
                         , pstateSourcePos = SourcePos "" (mkPos 1) (mkPos 1)
                         , pstateTabWidth = mkPos 8
                         , pstateLinePrefix = ""
                         }
      (_, stS) = reachOffset startOff initial
      (_, stE) = reachOffset endOff initial
      sPos = pstateSourcePos stS
      ePos = pstateSourcePos stE
      l0 = max 0 (unPos (sourceLine sPos) - 1)
      c0 = max 0 (unPos (sourceColumn sPos) - 1)
      l1 = max 0 (unPos (sourceLine ePos) - 1)
      c1 = max 0 (unPos (sourceColumn ePos) - 1)
  in mkRange (fromIntegral l0) (fromIntegral c0) (fromIntegral l1) (fromIntegral (max c1 (c0 + 1)))

diagFromParseError :: String -> ParseError String P.CustomParseError -> Diagnostic
diagFromParseError src perr =
  let msg = T.pack (parseErrorTextPretty perr)
      off = errorOffset perr
      rng = mkRangeFromOffset off src
  in Diagnostic { _range = rng
                , _severity = Just DiagnosticSeverity_Error
                , _code = Nothing
                , _codeDescription = Nothing
                , _source = Nothing
                , _message = msg
                , _tags = Nothing
                , _relatedInformation = Nothing
                , _data_ = Nothing
                }

publishParseDiagnostics :: Uri -> String -> LspM () ()
publishParseDiagnostics uri src = do
  let fname = T.unpack (case uri of Uri u -> u)
      contentWithNewline = if null src || last src == '\n' then src else src ++ "\n"
      parsed = runParser (St.evalStateT P.file_input P.initState) fname contentWithNewline
  -- Evaluate in IO to catch CustomParseException thrown from the parser
  eres <- liftIO $ (E.try (E.evaluate parsed) :: IO (Either P.CustomParseException (Either (ParseErrorBundle String P.CustomParseError) ([S.Import], S.Suite))))
  case eres of
    Left (P.CustomParseException loc customErr) -> do
      let rng = rangeFromLoc loc contentWithNewline
          (msg0, notes) = AD.customParseErrorToDiagnostic customErr
          notesTxt = case notes of
                       [] -> ""
                       ns -> "\n" ++ unlines (map noteLine ns)
          fullMsg = T.pack ("Syntax error: " ++ msg0 ++ notesTxt)
          diag = Diagnostic rng (Just DiagnosticSeverity_Error) Nothing Nothing Nothing fullMsg Nothing Nothing Nothing
      sendNotification SMethod_TextDocumentPublishDiagnostics (PublishDiagnosticsParams uri Nothing [diag])
    Right (Left bundle) -> do
      let diags = fmap (diagFromParseError contentWithNewline) (NE.toList (bundleErrors bundle))
      sendNotification SMethod_TextDocumentPublishDiagnostics (PublishDiagnosticsParams uri Nothing diags)
    Right (Right _) -> do
      sendNotification SMethod_TextDocumentPublishDiagnostics (PublishDiagnosticsParams uri Nothing [])

  where
    noteLine (Hint h) = "Hint: " ++ h
    noteLine (Note n) = "Note: " ++ n

handlers :: Handlers (LspM ())
handlers =
  mconcat
    [ notificationHandler SMethod_Initialized $ \_ -> pure ()
    , notificationHandler SMethod_WorkspaceDidChangeConfiguration $ \_ -> pure ()
    , notificationHandler SMethod_TextDocumentDidOpen $ \(TNotificationMessage _ _ (DidOpenTextDocumentParams doc)) -> do
        let TextDocumentItem{_uri=uri,_text=txt} = doc
        publishParseDiagnostics uri (T.unpack txt)
    , notificationHandler SMethod_TextDocumentDidChange $ \(TNotificationMessage _ _ (DidChangeTextDocumentParams vdoc changes)) -> do
        let VersionedTextDocumentIdentifier{_uri=uri} = vdoc
        case changes of
          [] -> pure ()
          cs -> do
            let txt = case toJSON (last cs) of
                        Object o -> case KM.lookup "text" o of
                                      Just (String t) -> t
                                      _ -> ""
                        _ -> ""
            if T.null txt
              then pure ()
              else do
                ok <- shouldParseNow uri
                if ok then publishParseDiagnostics uri (T.unpack txt) else pure ()
    , notificationHandler SMethod_TextDocumentDidClose $ \(TNotificationMessage _ _ (DidCloseTextDocumentParams (TextDocumentIdentifier uri))) -> do
        -- Clear diagnostics when a document is closed
        sendNotification SMethod_TextDocumentPublishDiagnostics (PublishDiagnosticsParams uri Nothing [])
        -- Remove from debounce map
        liftIO $ atomicModifyIORef' lastParseRef $ \m -> (HM.delete (case uri of Uri u -> u) m, ())
    ]

main :: IO Int
main =
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

-- Simple time-based gate to avoid parsing on every single keystroke.
-- If changes arrive within debounceMs, we skip until a later event.
{-# NOINLINE lastParseRef #-}
lastParseRef :: IORef (HM.HashMap T.Text POSIXTime)
lastParseRef = unsafePerformIO (newIORef HM.empty)

debounceMs :: POSIXTime
debounceMs = 0.20  -- 200 ms

shouldParseNow :: Uri -> LspM () Bool
shouldParseNow (Uri u) = do
  now <- liftIO getPOSIXTime
  liftIO $ atomicModifyIORef' lastParseRef $ \m ->
    let prev = HM.lookup u m
    in case prev of
         Nothing -> (HM.insert u now m, True)
         Just t  -> if now - t >= debounceMs
                      then (HM.insert u now m, True)
                      else (m, False)
