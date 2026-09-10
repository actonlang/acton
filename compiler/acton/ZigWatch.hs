-- A watch session owns the Zig runner and all of its compiler children. Builds
-- are requested only after Acton has finished publishing a generation's files.
{-# LANGUAGE ScopedTypeVariables #-}
module ZigWatch (Session, newSession, stopSession, build) where

import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.IORef
import Data.List (nub)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Encoding.Error (lenientDecode)
import Data.Word (Word64)
import System.Directory (canonicalizePath, getCurrentDirectory)
import System.Environment (getEnvironment)
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error (isDoesNotExistError)
import System.Process
import System.Random (randomIO)
import Text.Printf (printf)
import Text.Read (readMaybe)
import ProcessUtil (stopProcessGroup)

newtype Session = Session (MVar (Maybe Runner))
data Runner = Runner
  { runnerKey :: Maybe String
  , runnerProcess :: ProcessHandle
  , runnerStop :: IO ()
  , runnerInput :: Handle
  , runnerOutput :: Handle
  , runnerReader :: Async ()
  , runnerEvents :: Chan Event
  }
data Event = Ready | Done Int Bool String | Closed String

newSession :: IO Session
newSession = Session <$> newMVar Nothing

stopSession :: Session -> IO ()
stopSession (Session state) = modifyMVarMasked_ state $ \old -> do
    mapM_ stopRunner old
    return Nothing

-- Snapshot supplied graph inputs before launching Zig: build helpers are read
-- while compiling the runner, before its build() functions can run.
build :: Session -> Int -> CreateProcess -> [FilePath] -> IO (ExitCode, String, String)
build (Session state) generation cp graphFiles = do
    result <- mask $ \restore -> modifyMVarMasked state $ \old -> do
      owned <- newIORef old
      let stopOwned = do
            current <- readIORef owned
            mapM_ stopRunner current
            writeIORef owned Nothing
          start key = do
            stopOwned
            runner <- startRunner key cp
            writeIORef owned (Just runner)
            return runner
          acquire key = do
            runner <- start key
            current <- restore (graphKey cp graphFiles)
            if current == key then return runner else do
              replacement <- start current
              latest <- restore (graphKey cp graphFiles)
              if latest == current then return replacement else
                ioError (userError "Zig build configuration changed again during startup")
      outcome <- try $ do
        key <- restore (graphKey cp graphFiles)
        current <- case old of
          Just runner | key /= Nothing && runnerKey runner == key -> do
            code <- getProcessExitCode (runnerProcess runner)
            case code of
              Nothing -> return runner
              Just _ -> acquire key
          _ -> acquire key
        output <- restore (request current)
        return (current, output)
      case outcome of
        Right (current, output) -> return (Just current, Right output)
        Left (err :: SomeException) -> do
          stopOwned
          return (Nothing, Left err)
    either throwIO return result
  where
    request runner = do
      hPutStrLn (runnerInput runner) ("build " ++ show generation)
      hFlush (runnerInput runner)
      event <- readChan (runnerEvents runner)
      case event of
        Done gen success output | gen == generation ->
          return (if success then ExitSuccess else ExitFailure 1, "", output)
        Closed output -> ioError (userError ("Zig watch runner exited before completing the build:\n" ++ output))
        _ -> ioError (userError "Unexpected response from Zig watch runner")

graphKey :: CreateProcess -> [FilePath] -> IO (Maybe String)
graphKey cp graphFiles = handle (\(_ :: IOException) -> return Nothing) $ do
    directory <- maybe getCurrentDirectory return (cwd cp) >>= canonicalizePath
    environment <- maybe getEnvironment return (env cp)
    files <- mapM (canonicalizePath . (directory </>)) graphFiles
    contents <- forM (nub files) $ \path -> do
      bytes <- missing (BS.readFile path)
      return (path, SHA256.hash <$> bytes)
    return (Just (show (cmdspec cp, directory, environment, contents)))

missing :: IO a -> IO (Maybe a)
missing action = (Just <$> action) `catch` \err ->
    if isDoesNotExistError err then return Nothing else throwIO (err :: IOException)

startRunner :: Maybe String -> CreateProcess -> IO Runner
startRunner key cp = mask $ \restore -> do
    words <- replicateM 2 (randomIO :: IO Word64)
    let token = concatMap (printf "%016x") words
    environment <- maybe getEnvironment return (env cp)
    (output, writer) <- createPipe
    let closePipes = ignoreIO (hClose output) >> ignoreIO (hClose writer)
    (Just input, _, _, ph) <- createProcess cp
      { std_in = CreatePipe, std_out = UseHandle writer, std_err = UseHandle writer
      , create_group = True, close_fds = True
      , env = Just (("ACTON_ZIG_WATCH_TOKEN", token) : filter ((/= "ACTON_ZIG_WATCH_TOKEN") . fst) environment)
      } `onException` closePipes
    pid <- getPid ph
    stopped <- newMVar False
    let stop = modifyMVarMasked_ stopped $ \done -> do
          unless done (stopProcessGroup ph pid)
          return True
        cleanup = do
          ignoreIO (hClose input)
          stop
          closePipes
    (events, reader) <- (do
      hClose writer
      events <- newChan
      reader <- asyncWithUnmask $ \unmask -> unmask $
        -- An exited runner can leave children holding the output pipe open.
        -- Stop its group while the reader drains the ordered buffered output.
        withAsync (waitForProcess ph >> stop) $ \_ ->
          readEvents output token events `finally` stop
      return (events, reader)) `onException` cleanup
    let runner = Runner key ph stop input output reader events
    restore (do
      event <- readChan events
      case event of
        Ready -> return runner
        Closed text -> ioError (userError ("Could not start Zig watch runner:\n" ++ text))
        _ -> ioError (userError "Unexpected Zig watch protocol version")
      ) `onException` stopRunner runner

-- A per-runner nonce separates protocol frames from arbitrary build output.
-- Scan bytes, not lines: diagnostics need not end in a newline, and either a
-- frame or a UTF-8 character may be split across reads.
readEvents :: Handle -> String -> Chan Event -> IO ()
readEvents handle token events = go BS.empty []
  where
    prefix = BS8.pack ("\0ACTON_ZIG " ++ token ++ " ")
    decode = Text.unpack . Text.decodeUtf8With lenientDecode . BS.concat . reverse
    go pending output =
      let (before, frame) = BS.breakSubstring prefix pending
      in if BS.null frame
        then do
          -- Retain enough bytes for a prefix that straddles the next read.
          let count = max 0 (BS.length pending - BS.length prefix + 1)
          more (BS.drop count pending) (BS.take count pending : output)
        else
          let payload = BS.drop (BS.length prefix) frame
          in case BS.elemIndex 0 payload of
            Nothing | BS.length payload <= 128 -> more frame (before : output)
            Just end | end <= 128 -> do
              let message = BS8.unpack (BS.take end payload)
                  rest = BS.drop (end + 1) payload
                  collected = before : output
              case words message of
                ["ready", "1"] -> writeChan events Ready >> go rest collected
                ["ready", _] -> writeChan events (Closed ("Unsupported Zig watch protocol: " ++ message))
                ["done", gen, status] | Just n <- readMaybe gen, status `elem` ["ok", "failed"] ->
                  writeChan events (Done n (status == "ok") (decode collected)) >> go rest []
                _ -> writeChan events (Closed ("Invalid Zig watch response: " ++ message))
            _ -> writeChan events (Closed "Zig watch response exceeds 128 bytes")
    more pending output = do
      chunk <- try (BS.hGetSome handle 4096)
      case chunk of
        Left (_ :: IOException) -> writeChan events (Closed (decode (pending : output)))
        Right bytes | BS.null bytes -> writeChan events (Closed (decode (pending : output)))
                    | otherwise -> go (pending <> bytes) output

stopRunner :: Runner -> IO ()
stopRunner runner = mask_ $ do
    ignoreIO (hClose (runnerInput runner))
    runnerStop runner
    cancel (runnerReader runner)
    ignoreIO (hClose (runnerOutput runner))

ignoreIO :: IO () -> IO ()
ignoreIO action = action `catch` \(_ :: IOException) -> return ()
