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
import Data.List (nub, sort)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Encoding.Error (lenientDecode)
import Data.Word (Word64)
import qualified Acton.Zon as Zon
import System.Directory (canonicalizePath, getCurrentDirectory, listDirectory)
import System.Environment (getEnvironment)
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error (isDoesNotExistError)
import System.Process
import qualified System.Posix.Files as Files
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

-- Snapshot graph inputs before launching Zig: imported build helpers are read
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
    let args = case cmdspec cp of RawCommand _ xs -> xs; ShellCommand _ -> []
        option names = listToMaybe [value | (name, value) <- reverse (zip args (drop 1 args)), name `elem` names]
        absolute path = canonicalizePath (directory </> path)
        globalCache = option ["--global-cache-dir"] `orElse` lookup "ZIG_GLOBAL_CACHE_DIR" environment
        cache = fromMaybe ".zig-cache" (option ["--cache-dir"] `orElse` lookup "ZIG_LOCAL_CACHE_DIR" environment)
        dest = lookup "DESTDIR" environment
        prefix = fromMaybe (if dest == Nothing then "zig-out" else "/usr") (option ["--prefix", "-p"])
        under root path = root </> dropWhile isPathSeparator path
        install = maybe prefix (`under` prefix) dest
        installDir name flag = case option [flag] of
          Just path | isAbsolute path -> maybe path (`under` path) dest
          path -> install </> fromMaybe name path
    excluded <- Set.fromList <$> mapM absolute
      (cache : catMaybes [globalCache] ++ [installDir "bin" "--prefix-exe-dir", installDir "lib" "--prefix-lib-dir", installDir "include" "--prefix-include-dir"])
    files <- mapM absolute graphFiles
    let roots = Set.fromList (directory : [takeDirectory path | path <- files, takeFileName path `elem` ["build.zig", "build.zig.zon"]])
    seen <- newIORef Set.empty
    complete <- newIORef True
    let visit path = do
          root <- absolute path
          visited <- readIORef seen
          if Set.member root visited then return [] else do
            modifyIORef' seen (Set.insert root)
            contents <- missing (BS.readFile (root </> "build.zig.zon"))
            deps <- case contents of
              Nothing -> return []
              Just bytes -> case Zon.zonDependencies <$> Zon.parseZon (Text.unpack (Text.decodeUtf8With lenientDecode bytes)) of
                -- An unused lazy dependency need not have a valid manifest.
                -- Let Zig decide whether it matters, but do not reuse a graph
                -- whose complete dependency closure we could not discover.
                Left _ -> writeIORef complete False >> return []
                Right deps -> return deps
            native <- if Set.member root roots then return Nothing else stampPath excluded Set.empty root
            children <- forM deps $ \(_, dep) -> case Zon.zdPath dep of
              Just relative -> visit (root </> relative)
              Nothing -> case Zon.zdHash dep of
                Nothing -> writeIORef complete False >> return []
                Just hash -> case globalCache of
                  Just cache -> visit (cache </> "p" </> hash)
                  Nothing -> writeIORef complete False >> return []
            return ((root, SHA256.hash <$> contents, native) : concat children)
    packages <- concat <$> mapM visit (Set.toList roots)
    contents <- forM (nub files) $ \path -> do
      bytes <- missing (BS.readFile path)
      return (path, SHA256.hash <$> bytes)
    known <- readIORef complete
    return (if known then Just (show (cmdspec cp, directory, environment, excluded, contents, packages)) else Nothing)
  where
    orElse (Just value) _ = Just value
    orElse Nothing other = other

-- Directory entry names detect additions/removals. Ignore directory timestamps
-- so writing an excluded cache or install directory does not invalidate inputs.
stampPath excluded ancestors path = do
    link <- missing (Files.getSymbolicLinkStatus path)
    -- Package roots and directory children already have canonical parents.
    -- Resolve only links; resolving every regular file repeats directory I/O.
    canonical <- case link of
      Just stat | Files.isSymbolicLink stat -> canonicalizePath path
      _ -> return path
    if Set.member canonical excluded then return Nothing else do
      target <- case link of
        Just stat | Files.isSymbolicLink stat -> missing (Files.getFileStatus path)
        _ -> return link
      children <- case target of
        Just stat | Files.isDirectory stat && not (Set.member canonical ancestors) -> do
          names <- sort <$> listDirectory path
          catMaybes <$> mapM (stampPath excluded (Set.insert canonical ancestors) . (canonical </>)) names
        _ -> return []
      return (Just (SHA256.hash (BS8.pack (show (path, canonical, attrs <$> link, attrs <$> target)) <> BS.concat children)))
  where
    attrs stat = (Files.deviceID stat, Files.fileID stat, Files.fileMode stat,
                  if Files.isDirectory stat then Nothing else Just (Files.fileSize stat, Files.modificationTimeHiRes stat, Files.statusChangeTimeHiRes stat))

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
