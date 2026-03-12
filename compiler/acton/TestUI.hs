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
import Control.Concurrent (ThreadId, forkIO, killThread, myThreadId, threadDelay)
import Control.Concurrent.MVar
import Control.Monad
import Data.IORef
import qualified Data.List
import qualified Data.Map as M
import Data.Ord (Down(..))
import qualified Data.Set as Set
import System.IO (hFlush, hIsTerminalDevice, stdout)
import TerminalProgress
import TerminalSize

data TestKey = TestKey
  { tkModule :: String
  , tkName :: String
  } deriving (Eq, Ord, Show)

type TestLine = Int -> String

data TestProgressUI = TestProgressUI
  { tpuEnabled :: Bool
  , tpuTotalLinesRef :: IORef Int
  , tpuLinesRef :: IORef [String]
  , tpuLineRenderRef :: IORef [TestLine]
  , tpuLineIndexRef :: IORef (M.Map TestKey Int)
  , tpuLiveOrderRef :: IORef [TestKey]
  , tpuLiveSetRef :: IORef (Set.Set TestKey)
  , tpuLiveLineRef :: IORef (M.Map TestKey TestLine)
  , tpuPrintedModulesRef :: IORef (Set.Set String)
  , tpuPendingDetailsRef :: IORef (M.Map TestKey [String])
  , tpuSpinnerRef :: IORef Int
  , tpuTickerThreadRef :: IORef (Maybe ThreadId)
  , tpuTermProgress :: TermProgress
  , tpuTermSize :: TermSize
  , tpuLock :: MVar ()
  , tpuNameWidth :: Int
  , tpuUseColor :: Bool
  , tpuShowLog :: Bool
  }

initTestProgressUI :: C.GlobalOptions -> Int -> Bool -> Bool -> IO TestProgressUI
initTestProgressUI gopts nameWidth showLog useColorOut = do
    tty <- hIsTerminalDevice stdout
    let enabled = (tty || C.tty gopts) && not (C.quiet gopts)
    totalLinesRef <- newIORef 0
    linesRef <- newIORef []
    lineRenderRef <- newIORef []
    lineIndexRef <- newIORef M.empty
    liveOrderRef <- newIORef []
    liveSetRef <- newIORef Set.empty
    liveLineRef <- newIORef M.empty
    printedModulesRef <- newIORef Set.empty
    pendingDetailsRef <- newIORef M.empty
    spinnerRef <- newIORef 0
    tickerThreadRef <- newIORef Nothing
    termProgress <- initTermProgress gopts
    termSize <- initTermSize enabled
    lock <- newMVar ()
    return TestProgressUI
      { tpuEnabled = enabled
      , tpuTotalLinesRef = totalLinesRef
      , tpuLinesRef = linesRef
      , tpuLineRenderRef = lineRenderRef
      , tpuLineIndexRef = lineIndexRef
      , tpuLiveOrderRef = liveOrderRef
      , tpuLiveSetRef = liveSetRef
      , tpuLiveLineRef = liveLineRef
      , tpuPrintedModulesRef = printedModulesRef
      , tpuPendingDetailsRef = pendingDetailsRef
      , tpuSpinnerRef = spinnerRef
      , tpuTickerThreadRef = tickerThreadRef
      , tpuTermProgress = termProgress
      , tpuTermSize = termSize
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

testTickMicros :: Int
testTickMicros = 80000

testSpinnerThreshold :: Int
testSpinnerThreshold = 25

testSpinnerChars :: [Char]
testSpinnerChars = "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏"

testSpinnerEnabled :: Int -> Bool
testSpinnerEnabled cols = cols >= testSpinnerThreshold

testSpinnerChar :: TestProgressUI -> IO Char
testSpinnerChar ui = do
    ix <- readIORef (tpuSpinnerRef ui)
    return (testSpinnerChars !! (ix `mod` length testSpinnerChars))

moduleHeaderLine :: String -> String
moduleHeaderLine modName
  | null modName = "Tests"
  | otherwise = "Tests - module " ++ modName ++ ":"

safeLiveWidth :: Int -> Int
safeLiveWidth width
  | width <= 0 = 0
  | width == 1 = 1
  | otherwise = width - 1

staticLine :: String -> TestLine
staticLine line cols = termFitAnsiRight cols line

visibleLineCapacity :: Int -> Int
visibleLineCapacity rows = max 0 (rows - 1)

visibleStartIndex :: Int -> Int -> Int
visibleStartIndex rows total = max 0 (total - visibleLineCapacity rows)

startTestTicker :: TestProgressUI -> IO ()
startTestTicker ui =
    when (tpuEnabled ui) $ do
      m <- readIORef (tpuTickerThreadRef ui)
      case m of
        Just _ -> return ()
        Nothing -> do
          tid <- forkIO (testTickerLoop ui)
          writeIORef (tpuTickerThreadRef ui) (Just tid)

stopTestTicker :: TestProgressUI -> IO ()
stopTestTicker ui = do
    m <- atomicModifyIORef' (tpuTickerThreadRef ui) (\cur -> (Nothing, cur))
    forM_ m killThread

testTickerLoop :: TestProgressUI -> IO ()
testTickerLoop ui = do
    tid <- myThreadId
    let loop = do
          threadDelay testTickMicros
          keep <- withTestProgressLock ui $ do
            liveSet <- readIORef (tpuLiveSetRef ui)
            if Set.null liveSet || not (tpuEnabled ui)
              then return False
              else do
                (_, cols) <- ensureViewportUnlocked ui
                when (testSpinnerEnabled cols) $ do
                  modifyIORef' (tpuSpinnerRef ui) (+ 1)
                  refreshTestSpinnersUnlocked ui cols
                termProgressHeartbeat (tpuTermProgress ui)
                return True
          when keep loop
    loop
    atomicModifyIORef' (tpuTickerThreadRef ui) $ \cur ->
      if cur == Just tid then (Nothing, ()) else (cur, ())

currentViewportUnlocked :: TestProgressUI -> IO (Int, Int)
currentViewportUnlocked ui = termSizeCurrent (tpuTermSize ui)

ensureViewportUnlocked :: TestProgressUI -> IO (Int, Int)
ensureViewportUnlocked ui
  | not (tpuEnabled ui) = return (0, 0)
  | otherwise = do
      (oldRows, _) <- termSizeCurrent (tpuTermSize ui)
      (changed, rows, cols) <- termSizeSync (tpuTermSize ui)
      when changed $
        rerenderVisibleUnlocked ui oldRows rows cols
      return (rows, cols)

appendLineUnlocked :: TestProgressUI -> TestLine -> IO Int
appendLineUnlocked ui lineFn = do
    idx <- readIORef (tpuTotalLinesRef ui)
    rendered <- renderIndexedLineUnlocked ui idx lineFn
    putStrLn rendered
    modifyIORef' (tpuLinesRef ui) (\xs -> xs ++ [rendered])
    modifyIORef' (tpuLineRenderRef ui) (\xs -> xs ++ [lineFn])
    writeIORef (tpuTotalLinesRef ui) (idx + 1)
    return idx

updateLineListAt :: Int -> a -> [a] -> [a]
updateLineListAt idx line xs =
    case splitAt idx xs of
      (prefix, _ : rest) -> prefix ++ [line] ++ rest
      _ -> xs

insertLinesListAfter :: Int -> [a] -> [a] -> [a]
insertLinesListAfter idx newLines xs =
    let (prefix, rest) = splitAt (idx + 1) xs
    in prefix ++ newLines ++ rest

rerenderVisibleUnlocked :: TestProgressUI -> Int -> Int -> Int -> IO ()
rerenderVisibleUnlocked ui rowsBefore rowsAfter cols = do
    total <- readIORef (tpuTotalLinesRef ui)
    lineFns <- readIORef (tpuLineRenderRef ui)
    renderedAll <- mapM (\(idx, lineFn) -> renderIndexedLineWithColsUnlocked ui cols idx lineFn) (zip [0..] lineFns)
    let
        oldStartIdx = visibleStartIndex rowsBefore total
        newStartIdx = visibleStartIndex rowsAfter total
        oldVisibleCount = max 0 (total - oldStartIdx)
        newVisible = drop newStartIdx renderedAll
    when (oldVisibleCount > 0) $
      putStr ("\ESC[" ++ show oldVisibleCount ++ "A")
    when (oldVisibleCount > 0 || not (null newVisible)) $ do
      putStr "\r\ESC[J"
      forM_ newVisible $ \line -> do
        putStr "\r\ESC[2K"
        putStr line
        putStr "\n"
      hFlush stdout
    writeIORef (tpuLinesRef ui) renderedAll

refreshTestSpinnersUnlocked :: TestProgressUI -> Int -> IO ()
refreshTestSpinnersUnlocked ui cols = do
    idxMap <- readIORef (tpuLineIndexRef ui)
    liveLines <- readIORef (tpuLiveLineRef ui)
    forM_ (M.toList liveLines) $ \(key, lineFn) ->
      case M.lookup key idxMap of
        Just idx -> updateLineAtUnlockedWithCols ui cols idx lineFn
        Nothing -> return ()

renderLiveLine :: Int -> Char -> String -> String
renderLiveLine cols spinner line
  | not (testSpinnerEnabled cols) = line
  | otherwise =
      case line of
        ' ':' ':' ':rest -> ' ' : spinner : ' ' : rest
        _ -> line

isLiveLineIndexUnlocked :: TestProgressUI -> Int -> IO Bool
isLiveLineIndexUnlocked ui idx = do
    liveSet <- readIORef (tpuLiveSetRef ui)
    idxMap <- readIORef (tpuLineIndexRef ui)
    return (any (\key -> M.lookup key idxMap == Just idx) (Set.toList liveSet))

renderIndexedLineWithColsUnlocked :: TestProgressUI -> Int -> Int -> TestLine -> IO String
renderIndexedLineWithColsUnlocked ui cols idx lineFn = do
    let base = lineFn (safeLiveWidth cols)
    live <- isLiveLineIndexUnlocked ui idx
    if live
      then do
        spinner <- testSpinnerChar ui
        return (renderLiveLine cols spinner base)
      else return base

renderIndexedLineUnlocked :: TestProgressUI -> Int -> TestLine -> IO String
renderIndexedLineUnlocked ui idx lineFn = do
    (_, cols) <- currentViewportUnlocked ui
    renderIndexedLineWithColsUnlocked ui cols idx lineFn

ensureModuleHeaderUnlocked :: TestProgressUI -> String -> IO ()
ensureModuleHeaderUnlocked ui modName = do
    printed <- readIORef (tpuPrintedModulesRef ui)
    unless (Set.member modName printed) $ do
      total <- readIORef (tpuTotalLinesRef ui)
      when (total > 0) $
        void (appendLineUnlocked ui (staticLine ""))
      _ <- appendLineUnlocked ui (staticLine (moduleHeaderLine modName))
      writeIORef (tpuPrintedModulesRef ui) (Set.insert modName printed)

canAppendLinesUnlocked :: TestProgressUI -> Int -> IO Bool
canAppendLinesUnlocked ui n
    | not (tpuEnabled ui) = return True
    | n <= 0 = return True
    | otherwise = do
        (rows, _) <- currentViewportUnlocked ui
        if rows <= 0
          then return True
          else do
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
                    return (offset + n < rows)

testUiStart :: TestProgressUI -> TestKey -> String -> TestLine -> IO Bool
testUiStart ui key modName lineFn = withTestProgressLock ui $ do
    if not (tpuEnabled ui)
      then return True
      else do
        void (ensureViewportUnlocked ui)
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
            idx <- readIORef (tpuTotalLinesRef ui)
            modifyIORef' (tpuLineIndexRef ui) (M.insert key idx)
            modifyIORef' (tpuLiveOrderRef ui) (\xs -> if key `elem` xs then xs else xs ++ [key])
            modifyIORef' (tpuLiveSetRef ui) (Set.insert key)
            modifyIORef' (tpuLiveLineRef ui) (M.insert key lineFn)
            _ <- appendLineUnlocked ui lineFn
            startTestTicker ui
            return True

testUiAppendFinal :: TestProgressUI -> TestKey -> String -> TestLine -> IO Bool
testUiAppendFinal ui key modName lineFn = withTestProgressLock ui $ do
    if not (tpuEnabled ui)
      then return True
      else do
        void (ensureViewportUnlocked ui)
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
            idx <- appendLineUnlocked ui lineFn
            modifyIORef' (tpuLineIndexRef ui) (M.insert key idx)
            return True

updateLineAtUnlocked :: TestProgressUI -> Int -> TestLine -> IO ()
updateLineAtUnlocked ui idx lineFn = do
    (_, cols) <- currentViewportUnlocked ui
    updateLineAtUnlockedWithCols ui cols idx lineFn

updateLineAtUnlockedWithCols :: TestProgressUI -> Int -> Int -> TestLine -> IO ()
updateLineAtUnlockedWithCols ui cols idx lineFn = do
    rendered <- renderIndexedLineWithColsUnlocked ui cols idx lineFn
    modifyIORef' (tpuLineRenderRef ui) (updateLineListAt idx lineFn)
    modifyIORef' (tpuLinesRef ui) (updateLineListAt idx rendered)
    total <- readIORef (tpuTotalLinesRef ui)
    (rows, _) <- currentViewportUnlocked ui
    let offset = total - idx
    when (offset > 0 && (rows <= 0 || offset < rows)) $ do
      putStr ("\ESC[" ++ show offset ++ "A")
      putStr "\r\ESC[2K"
      putStr rendered
      putStr ("\ESC[" ++ show offset ++ "B")
      putStr "\r"
      hFlush stdout

testUiUpdateLive :: TestProgressUI -> TestKey -> TestLine -> IO ()
testUiUpdateLive ui key lineFn = withTestProgressLock ui $ do
    when (tpuEnabled ui) $ do
      void (ensureViewportUnlocked ui)
      idxMap <- readIORef (tpuLineIndexRef ui)
      case M.lookup key idxMap of
        Nothing -> return ()
        Just idx -> do
          modifyIORef' (tpuLiveLineRef ui) (M.insert key lineFn)
          updateLineAtUnlocked ui idx lineFn

testUiFinalize :: TestProgressUI -> TestKey -> TestLine -> IO Bool
testUiFinalize ui key lineFn = withTestProgressLock ui $ do
    liveSet <- readIORef (tpuLiveSetRef ui)
    let wasLive = Set.member key liveSet
    when wasLive $ do
      writeIORef (tpuLiveSetRef ui) (Set.delete key liveSet)
      modifyIORef' (tpuLiveOrderRef ui) (filter (/= key))
      modifyIORef' (tpuLiveLineRef ui) (M.delete key)
    when (tpuEnabled ui) $ do
      void (ensureViewportUnlocked ui)
      idxMap <- readIORef (tpuLineIndexRef ui)
      case M.lookup key idxMap of
        Nothing -> return ()
        Just idx -> updateLineAtUnlocked ui idx lineFn
    if wasLive
      then do
        liveSet' <- readIORef (tpuLiveSetRef ui)
        when (tpuEnabled ui && Set.null liveSet') $
          stopTestTicker ui
        return True
      else return False

testUiUpdateFinal :: TestProgressUI -> TestKey -> TestLine -> IO ()
testUiUpdateFinal ui key lineFn = withTestProgressLock ui $ do
    when (tpuEnabled ui) $ do
      void (ensureViewportUnlocked ui)
      idxMap <- readIORef (tpuLineIndexRef ui)
      case M.lookup key idxMap of
        Nothing -> return ()
        Just idx -> updateLineAtUnlocked ui idx lineFn

canInsertLinesUnlocked :: TestProgressUI -> Int -> Int -> IO Bool
canInsertLinesUnlocked ui insertIdx n
    | not (tpuEnabled ui) = return True
    | n <= 0 = return True
    | otherwise = do
        (rows, _) <- currentViewportUnlocked ui
        if rows <= 0
          then return True
          else do
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
                return ((total + n - minimum idxs) < rows)

insertLinesAfterUnlocked :: TestProgressUI -> Int -> [String] -> IO Bool
insertLinesAfterUnlocked ui idx lines = do
    let n = length lines
    if n <= 0
      then return True
      else do
        (rows, cols) <- currentViewportUnlocked ui
        total <- readIORef (tpuTotalLinesRef ui)
        liveSet <- readIORef (tpuLiveSetRef ui)
        let lineFns = map staticLine lines
            rendered = map ($ safeLiveWidth cols) lineFns
            startIdx = idx + 1
            offsetNew = (total + n) - startIdx
            visibleOk =
              Set.null liveSet
              || not (tpuEnabled ui)
              || rows <= 0
              || offsetNew < rows
        ok <- canInsertLinesUnlocked ui idx n
        if not ok || not visibleOk
          then return False
          else do
            modifyIORef' (tpuLineRenderRef ui) (insertLinesListAfter idx lineFns)
            modifyIORef' (tpuLinesRef ui) (insertLinesListAfter idx rendered)
            modifyIORef' (tpuTotalLinesRef ui) (+ n)
            modifyIORef' (tpuLineIndexRef ui) (M.map (\i -> if i > idx then i + n else i))
            if not (tpuEnabled ui)
              then mapM_ putStrLn rendered
              else rerenderFromUnlocked ui startIdx total
            return True

rerenderFromUnlocked :: TestProgressUI -> Int -> Int -> IO ()
rerenderFromUnlocked ui startIdx oldTotal = do
    let offset = oldTotal - startIdx
    renderedLines <- readIORef (tpuLinesRef ui)
    (rows, _) <- currentViewportUnlocked ui
    let renderedTail = drop startIdx renderedLines
    if offset <= 0
      then do
        forM_ renderedTail $ \line -> do
          putStr "\r\ESC[2K"
          putStr line
          putStr "\n"
        hFlush stdout
      else when (rows <= 0 || offset < rows) $ do
        putStr ("\ESC[" ++ show offset ++ "A")
        putStr "\r\ESC[J"
        forM_ renderedTail $ \line -> do
          putStr "\r\ESC[2K"
          putStr line
          putStr "\n"
        hFlush stdout

testUiInsertDetails :: TestProgressUI -> TestKey -> [String] -> IO Bool
testUiInsertDetails ui key lines = withTestProgressLock ui $ do
    if null lines
      then return True
      else if not (tpuEnabled ui)
        then return True
        else do
          void (ensureViewportUnlocked ui)
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
    void (ensureViewportUnlocked ui)
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
