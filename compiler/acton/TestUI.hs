module TestUI
  ( TestKey(..)
  , TestProgressUI(..)
  , initTestProgressUI
  , testUiStart
  , testUiAppendFinal
  , testUiUpdateLive
  , testUiFinalize
  , testUiUpdateFinal
  , testUiInsertDetails
  , testUiProgressPercent
  , testUiProgressClear
  , queuePendingDetails
  , flushPendingDetails
  ) where

import qualified Acton.CommandLineParser as C
import TerminalProgress
import Control.Concurrent (ThreadId, forkIO, killThread, myThreadId, threadDelay)
import Control.Concurrent.MVar
import Control.Monad
import Data.IORef
import qualified Data.List
import Data.Ord (Down(..))
import qualified Data.Map as M
import qualified Data.Set as Set
import System.Environment (lookupEnv)
import System.IO (hFlush, hIsTerminalDevice, stdout)

data TestKey = TestKey
  { tkModule :: String
  , tkName :: String
  } deriving (Eq, Ord, Show)

data TestProgressUI = TestProgressUI
  { tpuEnabled :: Bool
  , tpuRows :: Int
  , tpuTotalLinesRef :: IORef Int
  , tpuLinesRef :: IORef [String]
  , tpuLineIndexRef :: IORef (M.Map TestKey Int)
  , tpuLiveOrderRef :: IORef [TestKey]
  , tpuLiveSetRef :: IORef (Set.Set TestKey)
  , tpuLiveLineRef :: IORef (M.Map TestKey String)
  , tpuPrintedModulesRef :: IORef (Set.Set String)
  , tpuPendingDetailsRef :: IORef (M.Map TestKey [String])
  , tpuSpinnerRef :: IORef Int
  , tpuSpinnerThreadRef :: IORef (Maybe ThreadId)
  , tpuTermProgress :: TermProgress
  , tpuLock :: MVar ()
  , tpuNameWidth :: Int
  , tpuUseColor :: Bool
  , tpuShowLog :: Bool
  }

readMaybeInt :: String -> Maybe Int
readMaybeInt s =
    case reads s of
      [(n, "")] -> Just n
      _ -> Nothing

getTerminalRows :: IO Int
getTerminalRows = do
    mRows <- lookupEnv "LINES"
    case mRows >>= readMaybeInt of
      Just n | n > 0 -> return n
      _ -> return 24

initTestProgressUI :: C.GlobalOptions -> Int -> Bool -> Bool -> IO TestProgressUI
initTestProgressUI gopts nameWidth showLog useColorOut = do
    tty <- hIsTerminalDevice stdout
    let enabled = (tty || C.tty gopts) && not (C.quiet gopts)
    rows <- if enabled then getTerminalRows else return 0
    totalLinesRef <- newIORef 0
    linesRef <- newIORef []
    lineIndexRef <- newIORef M.empty
    liveOrderRef <- newIORef []
    liveSetRef <- newIORef Set.empty
    liveLineRef <- newIORef M.empty
    printedModulesRef <- newIORef Set.empty
    pendingDetailsRef <- newIORef M.empty
    spinnerRef <- newIORef 0
    spinnerThreadRef <- newIORef Nothing
    termProgress <- initTermProgress gopts
    lock <- newMVar ()
    return TestProgressUI
      { tpuEnabled = enabled
      , tpuRows = rows
      , tpuTotalLinesRef = totalLinesRef
      , tpuLinesRef = linesRef
      , tpuLineIndexRef = lineIndexRef
      , tpuLiveOrderRef = liveOrderRef
      , tpuLiveSetRef = liveSetRef
      , tpuLiveLineRef = liveLineRef
      , tpuPrintedModulesRef = printedModulesRef
      , tpuPendingDetailsRef = pendingDetailsRef
      , tpuSpinnerRef = spinnerRef
      , tpuSpinnerThreadRef = spinnerThreadRef
      , tpuTermProgress = termProgress
      , tpuLock = lock
      , tpuNameWidth = nameWidth
      , tpuUseColor = useColorOut
      , tpuShowLog = showLog
      }

withTestProgressLock :: TestProgressUI -> IO a -> IO a
withTestProgressLock ui action =
    if not (tpuEnabled ui)
      then action
      else withMVar (tpuLock ui) (\_ -> action)

testUiProgressPercent :: TestProgressUI -> Int -> IO ()
testUiProgressPercent ui pct =
    withTestProgressLock ui (termProgressPercent (tpuTermProgress ui) pct)

testUiProgressClear :: TestProgressUI -> IO ()
testUiProgressClear ui =
    withTestProgressLock ui (termProgressClear (tpuTermProgress ui))

testSpinnerTickMicros :: Int
testSpinnerTickMicros = 80000

spinnerChars :: [Char]
spinnerChars = "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏"

testSpinnerChar :: TestProgressUI -> IO Char
testSpinnerChar ui = do
    ix <- readIORef (tpuSpinnerRef ui)
    return (spinnerChars !! (ix `mod` length spinnerChars))

startTestSpinnerTicker :: TestProgressUI -> IO ()
startTestSpinnerTicker ui =
    when (tpuEnabled ui) $ do
      m <- readIORef (tpuSpinnerThreadRef ui)
      case m of
        Just _ -> return ()
        Nothing -> do
          tid <- forkIO (testSpinnerLoop ui)
          writeIORef (tpuSpinnerThreadRef ui) (Just tid)

stopTestSpinnerTicker :: TestProgressUI -> IO ()
stopTestSpinnerTicker ui = do
    m <- atomicModifyIORef' (tpuSpinnerThreadRef ui) (\cur -> (Nothing, cur))
    forM_ m killThread

refreshTestSpinnersUnlocked :: TestProgressUI -> IO ()
refreshTestSpinnersUnlocked ui = do
    idxMap <- readIORef (tpuLineIndexRef ui)
    liveLines <- readIORef (tpuLiveLineRef ui)
    spinner <- testSpinnerChar ui
    forM_ (M.toList liveLines) $ \(key, baseLine) ->
      case M.lookup key idxMap of
        Just idx -> updateLineAtUnlocked ui idx (renderLiveLine spinner baseLine)
        Nothing -> return ()
    termProgressHeartbeat (tpuTermProgress ui)

testSpinnerLoop :: TestProgressUI -> IO ()
testSpinnerLoop ui = do
    tid <- myThreadId
    let loop = do
          threadDelay testSpinnerTickMicros
          keep <- withTestProgressLock ui $ do
            liveSet <- readIORef (tpuLiveSetRef ui)
            if Set.null liveSet || not (tpuEnabled ui)
              then return False
              else do
                modifyIORef' (tpuSpinnerRef ui) (+ 1)
                refreshTestSpinnersUnlocked ui
                return True
          when keep loop
    loop
    atomicModifyIORef' (tpuSpinnerThreadRef ui) $ \cur ->
      if cur == Just tid then (Nothing, ()) else (cur, ())

renderLiveLine :: Char -> String -> String
renderLiveLine spinner line =
    case line of
      ' ':' ':' ':rest -> ' ' : spinner : ' ' : rest
      _ -> ' ' : spinner : ' ' : line

moduleHeaderLine :: String -> String
moduleHeaderLine modName
  | null modName = "Tests"
  | otherwise = "Tests - module " ++ modName ++ ":"

appendLineUnlocked :: TestProgressUI -> String -> IO Int
appendLineUnlocked ui line = do
    idx <- readIORef (tpuTotalLinesRef ui)
    putStrLn line
    modifyIORef' (tpuLinesRef ui) (\xs -> xs ++ [line])
    writeIORef (tpuTotalLinesRef ui) (idx + 1)
    return idx

updateLineListAt :: Int -> String -> [String] -> [String]
updateLineListAt idx line xs =
    case splitAt idx xs of
      (prefix, _ : rest) -> prefix ++ [line] ++ rest
      _ -> xs

insertLinesListAfter :: Int -> [String] -> [String] -> [String]
insertLinesListAfter idx newLines xs =
    let (prefix, rest) = splitAt (idx + 1) xs
    in prefix ++ newLines ++ rest

ensureModuleHeaderUnlocked :: TestProgressUI -> String -> IO ()
ensureModuleHeaderUnlocked ui modName = do
    printed <- readIORef (tpuPrintedModulesRef ui)
    unless (Set.member modName printed) $ do
      total <- readIORef (tpuTotalLinesRef ui)
      when (total > 0) $
        void (appendLineUnlocked ui "")
      _ <- appendLineUnlocked ui (moduleHeaderLine modName)
      writeIORef (tpuPrintedModulesRef ui) (Set.insert modName printed)

canAppendLinesUnlocked :: TestProgressUI -> Int -> IO Bool
canAppendLinesUnlocked ui n
    | not (tpuEnabled ui) = return True
    | n <= 0 = return True
    | tpuRows ui <= 0 = return True
    | otherwise = do
        liveOrder <- readIORef (tpuLiveOrderRef ui)
        case liveOrder of
          [] -> return True
          (oldest:_) -> do
            total <- readIORef (tpuTotalLinesRef ui)
            idxMap <- readIORef (tpuLineIndexRef ui)
            case M.lookup oldest idxMap of
              Nothing -> return True
              Just idx -> do
                let offset = total - idx
                return (offset + n < tpuRows ui)

testUiStart :: TestProgressUI -> TestKey -> String -> String -> IO Bool
testUiStart ui key modName line = withTestProgressLock ui $ do
    if not (tpuEnabled ui)
      then return True
      else do
        printed <- readIORef (tpuPrintedModulesRef ui)
        total <- readIORef (tpuTotalLinesRef ui)
        let headerNeeded = not (Set.member modName printed)
            headerLines = if headerNeeded then if total > 0 then 2 else 1 else 0
            linesNeeded = headerLines + 1
        ok <- canAppendLinesUnlocked ui linesNeeded
        if not ok
          then return False
          else do
            ensureModuleHeaderUnlocked ui modName
            spinner <- testSpinnerChar ui
            idx <- appendLineUnlocked ui (renderLiveLine spinner line)
            modifyIORef' (tpuLineIndexRef ui) (M.insert key idx)
            modifyIORef' (tpuLiveOrderRef ui) (\xs -> if key `elem` xs then xs else xs ++ [key])
            modifyIORef' (tpuLiveSetRef ui) (Set.insert key)
            modifyIORef' (tpuLiveLineRef ui) (M.insert key line)
            startTestSpinnerTicker ui
            return True

testUiAppendFinal :: TestProgressUI -> TestKey -> String -> String -> IO Bool
testUiAppendFinal ui key modName line = withTestProgressLock ui $ do
    if not (tpuEnabled ui)
      then return True
      else do
        printed <- readIORef (tpuPrintedModulesRef ui)
        total <- readIORef (tpuTotalLinesRef ui)
        let headerNeeded = not (Set.member modName printed)
            headerLines = if headerNeeded then if total > 0 then 2 else 1 else 0
            linesNeeded = headerLines + 1
        ok <- canAppendLinesUnlocked ui linesNeeded
        if not ok
          then return False
          else do
            ensureModuleHeaderUnlocked ui modName
            idx <- appendLineUnlocked ui line
            modifyIORef' (tpuLineIndexRef ui) (M.insert key idx)
            return True

updateLineAtUnlocked :: TestProgressUI -> Int -> String -> IO ()
updateLineAtUnlocked ui idx line = do
    modifyIORef' (tpuLinesRef ui) (updateLineListAt idx line)
    total <- readIORef (tpuTotalLinesRef ui)
    let offset = total - idx
    when (offset > 0 && (tpuRows ui <= 0 || offset < tpuRows ui)) $ do
      putStr ("\ESC[" ++ show offset ++ "A")
      putStr "\r\ESC[2K"
      putStr line
      putStr ("\ESC[" ++ show offset ++ "B")
      putStr "\r"
      hFlush stdout

testUiUpdateLive :: TestProgressUI -> TestKey -> String -> IO ()
testUiUpdateLive ui key line = withTestProgressLock ui $ do
    when (tpuEnabled ui) $ do
      idxMap <- readIORef (tpuLineIndexRef ui)
      case M.lookup key idxMap of
        Nothing -> return ()
        Just idx -> do
          modifyIORef' (tpuLiveLineRef ui) (M.insert key line)
          spinner <- testSpinnerChar ui
          updateLineAtUnlocked ui idx (renderLiveLine spinner line)

testUiFinalize :: TestProgressUI -> TestKey -> String -> IO Bool
testUiFinalize ui key line = withTestProgressLock ui $ do
    when (tpuEnabled ui) $ do
      idxMap <- readIORef (tpuLineIndexRef ui)
      case M.lookup key idxMap of
        Nothing -> return ()
        Just idx -> updateLineAtUnlocked ui idx line
    modifyIORef' (tpuLiveLineRef ui) (M.delete key)
    liveSet <- readIORef (tpuLiveSetRef ui)
    if Set.member key liveSet
      then do
        let liveSet' = Set.delete key liveSet
        writeIORef (tpuLiveSetRef ui) liveSet'
        modifyIORef' (tpuLiveOrderRef ui) (filter (/= key))
        when (tpuEnabled ui && Set.null liveSet') $
          stopTestSpinnerTicker ui
        return True
      else return False

testUiUpdateFinal :: TestProgressUI -> TestKey -> String -> IO ()
testUiUpdateFinal ui key line = withTestProgressLock ui $ do
    when (tpuEnabled ui) $ do
      idxMap <- readIORef (tpuLineIndexRef ui)
      case M.lookup key idxMap of
        Nothing -> return ()
        Just idx -> updateLineAtUnlocked ui idx line

canInsertLinesUnlocked :: TestProgressUI -> Int -> Int -> IO Bool
canInsertLinesUnlocked ui insertIdx n
    | not (tpuEnabled ui) = return True
    | n <= 0 = return True
    | tpuRows ui <= 0 = return True
    | otherwise = do
        liveSet <- readIORef (tpuLiveSetRef ui)
        idxMap <- readIORef (tpuLineIndexRef ui)
        let idxs =
              [ idx
              | key <- Set.toList liveSet
              , Just idx <- [M.lookup key idxMap]
              , idx <= insertIdx
              ]
        case idxs of
          [] -> return True
          _ -> do
            total <- readIORef (tpuTotalLinesRef ui)
            return ((total + n - minimum idxs) < tpuRows ui)

insertLinesAfterUnlocked :: TestProgressUI -> Int -> [String] -> IO Bool
insertLinesAfterUnlocked ui idx lines = do
    let n = length lines
    if n <= 0
      then return True
      else do
        total <- readIORef (tpuTotalLinesRef ui)
        liveSet <- readIORef (tpuLiveSetRef ui)
        let startIdx = idx + 1
            offsetNew = (total + n) - startIdx
            visibleOk =
              Set.null liveSet
              || not (tpuEnabled ui)
              || tpuRows ui <= 0
              || offsetNew < tpuRows ui
        ok <- canInsertLinesUnlocked ui idx n
        if not ok || not visibleOk
          then return False
          else do
            modifyIORef' (tpuLinesRef ui) (insertLinesListAfter idx lines)
            modifyIORef' (tpuTotalLinesRef ui) (+ n)
            modifyIORef' (tpuLineIndexRef ui) (M.map (\i -> if i > idx then i + n else i))
            if not (tpuEnabled ui)
              then mapM_ putStrLn lines
              else rerenderFromUnlocked ui startIdx total
            return True

rerenderFromUnlocked :: TestProgressUI -> Int -> Int -> IO ()
rerenderFromUnlocked ui startIdx oldTotal = do
    let offset = oldTotal - startIdx
    lines <- readIORef (tpuLinesRef ui)
    if offset <= 0
      then do
        forM_ (drop startIdx lines) $ \line -> do
          putStr "\r\ESC[2K"
          putStr line
          putStr "\n"
        hFlush stdout
      else when (tpuRows ui <= 0 || offset < tpuRows ui) $ do
        putStr ("\ESC[" ++ show offset ++ "A")
        putStr "\r\ESC[J"
        forM_ (drop startIdx lines) $ \line -> do
          putStr "\r\ESC[2K"
          putStr line
          putStr "\n"
        hFlush stdout

testUiInsertDetails :: TestProgressUI -> TestKey -> [String] -> IO Bool
testUiInsertDetails ui key lines = withTestProgressLock ui $ do
    if null lines
      then return True
      else do
        if not (tpuEnabled ui)
          then return True
          else do
            idxMap <- readIORef (tpuLineIndexRef ui)
            case M.lookup key idxMap of
              Nothing -> return False
              Just idx -> insertLinesAfterUnlocked ui idx lines

queuePendingDetails :: TestProgressUI -> TestKey -> [String] -> IO ()
queuePendingDetails ui key lines = withTestProgressLock ui $ do
    unless (null lines) $
      modifyIORef' (tpuPendingDetailsRef ui) (M.insert key lines)

flushPendingDetails :: TestProgressUI -> IO ()
flushPendingDetails ui = withTestProgressLock ui $ do
    pending <- readIORef (tpuPendingDetailsRef ui)
    unless (M.null pending) $ do
      idxMap <- readIORef (tpuLineIndexRef ui)
      let withIdx =
            Data.List.sortOn (\(idx, _, _) -> Down idx)
              [ (idx, key, lines)
              | (key, lines) <- M.toList pending
              , Just idx <- [M.lookup key idxMap]
              ]
      let go cur [] = return cur
          go cur ((idx, key, lines):rest) = do
            ok <- insertLinesAfterUnlocked ui idx lines
            if ok
              then go (M.delete key cur) rest
              else go cur rest
      pending' <- go pending withIdx
      writeIORef (tpuPendingDetailsRef ui) pending'
