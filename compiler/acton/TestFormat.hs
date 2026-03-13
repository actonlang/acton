module TestFormat
  ( formatTestStatus
  , formatTestStatusLive
  , formatTestLineWith
  , formatTestLineFitted
  , formatTestFinalLineRenderer
  , formatTestLiveLineRenderer
  , formatTestDetailLines
  , testColorApply
  , testColorBold
  , testColorRed
  , testColorGreen
  , testColorYellow
  , testColorReset
  ) where

import Acton.Testing (TestResult(..))
import Data.Char (isSpace)
import Data.List (foldl', isPrefixOf, isInfixOf, intercalate)
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe, mapMaybe)
import qualified Data.Map as M
import TerminalSize (termFitPlainRight, termVisibleLength)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKM
import Text.Printf (printf)

-- | Compute the status label (OK/FAIL/ERR/FLAKY) for a test.
formatTestStatus :: TestResult -> String
formatTestStatus res =
    let ok = trSuccess res == Just True && trException res == Nothing && not (trSkipped res)
        base
          | trSnapshotUpdated res = "UPDATED"
          | trSkipped res = "SKIP"
          | ok = "OK"
          | trNumErrors res > 0 && trNumFailures res > 0 = "ERR/FAIL"
          | trNumErrors res > 0 = "ERR"
          | trNumFailures res > 0 = "FAIL"
          | trSuccess res == Just False = "FAIL"
          | otherwise = "ERR"
        prefix = if not ok && trFlaky res then "FLAKY " else ""
    in prefix ++ base

-- | Compute a live status label for an in-progress test.
formatTestStatusLive :: TestResult -> String
formatTestStatusLive res
  | trSnapshotUpdated res = "UPDATED"
  | trSkipped res = "SKIP"
  | isJust (trException res) = "ERR"
  | trFlaky res = "FLAKY"
  | trNumErrors res > 0 && trNumFailures res > 0 = "ERR/FAIL"
  | trNumErrors res > 0 = "ERR"
  | trNumFailures res > 0 = "FAIL"
  | trSuccess res == Just False = "FAIL"
  | trNumIterations res > 0 || trSuccess res == Just True = "OK"
  | otherwise = "RUN"

testColorReset :: String
testColorReset = "\ESC[0m"

testColorBold :: String
testColorBold = "\ESC[1m"

testColorRed :: String
testColorRed = "\ESC[31m"

testColorGreen :: String
testColorGreen = "\ESC[32m"

testColorYellow :: String
testColorYellow = "\ESC[33m"

testColorApply :: Bool -> [String] -> String -> String
testColorApply useColor codes msg =
    if useColor
      then concat codes ++ msg ++ testColorReset
      else msg

testStatusWidth :: Int
testStatusWidth = (maximum (map length
  [ "RUN"
  , "OK"
  , "SKIP"
  , "UPDATED"
  , "FAIL"
  , "ERR"
  , "ERR/FAIL"
  , "FLAKY FAIL"
  , "FLAKY ERR"
  , "FLAKY ERR/FAIL"
  ])) + 1

colorizeStatusPart :: Bool -> Bool -> String -> String -> String
colorizeStatusPart useColor cached statusRaw runs =
    let suffix = if cached then "*" else ""
        statusWithStar = statusRaw ++ suffix
        pad = replicate (max 0 (testStatusWidth - length statusWithStar)) ' '
        strip pref s = if pref `isPrefixOf` s then drop (length pref) s else s
        core = strip "FLAKY " statusRaw
        statusColored = case core of
          "RUN" -> testColorApply useColor [testColorYellow] statusRaw
          "SKIP" -> testColorApply useColor [testColorYellow] statusRaw
          "OK" -> testColorApply useColor [testColorGreen] statusRaw
          "UPDATED" -> testColorApply useColor [testColorYellow] statusRaw
          _ -> testColorApply useColor [testColorBold, testColorRed] statusRaw
        star =
          if cached
            then if useColor then testColorYellow ++ "*" ++ testColorReset else "*"
            else ""
    in statusColored ++ star ++ pad ++ ": " ++ runs

renderStatusField :: Bool -> Bool -> String -> (String, String)
renderStatusField useColor cached statusRaw =
    let (statusPlain, statusRendered) = renderStatusToken useColor cached statusRaw
        pad = replicate (max 0 (testStatusWidth - length statusPlain)) ' '
    in (statusPlain ++ pad ++ ":", statusRendered ++ pad ++ ":")

renderStatusToken :: Bool -> Bool -> String -> (String, String)
renderStatusToken useColor cached statusRaw =
    let suffix = if cached then "*" else ""
        strip pref s = if pref `isPrefixOf` s then drop (length pref) s else s
        core = strip "FLAKY " statusRaw
        statusColored = case core of
          "RUN" -> testColorApply useColor [testColorYellow] statusRaw
          "SKIP" -> testColorApply useColor [testColorYellow] statusRaw
          "OK" -> testColorApply useColor [testColorGreen] statusRaw
          "UPDATED" -> testColorApply useColor [testColorYellow] statusRaw
          _ -> testColorApply useColor [testColorBold, testColorRed] statusRaw
        star =
          if cached
            then if useColor then testColorYellow ++ "*" ++ testColorReset else "*"
            else ""
    in (statusRaw ++ suffix, statusColored ++ star)

formatSecondsCompact :: Double -> String
formatSecondsCompact ms
  | ms <= 0 = "0s"
  | ms < 1000 = "<1s"
  | otherwise =
      let secs = ms / 1000
      in show (max 1 (round secs :: Int)) ++ "s"

formatSecondsCompactPadded :: Double -> Double -> String
formatSecondsCompactPadded expectedMs actualMs =
    let expected = formatSecondsCompact expectedMs
        actual = formatSecondsCompact actualMs
        width = max (length expected) (length actual)
    in replicate (max 0 (width - length actual)) ' ' ++ actual

formatMillisPadded :: Double -> Double -> String
formatMillisPadded expectedMs actualMs =
    let digits ms = max 1 (length (show (max 0 (floor ms :: Int))))
        width = max (digits expectedMs) (digits actualMs) + 4
    in printf ("%*.*fms" :: String) width (3 :: Int) actualMs

fitTestDisplay :: Int -> String -> String
fitTestDisplay width display
  | width <= 0 = ""
  | length display <= width = display
  | width <= 3 = take width display
  | otherwise = termFitPlainRight (width - 3) display ++ "..."

-- | Format a single test result line with alignment and timing.
formatTestLineWith :: Bool -> (TestResult -> String) -> Double -> Int -> String -> TestResult -> String
formatTestLineWith useColor statusFn expectedDurationMs nameWidth display res =
    let prefix0 = "   " ++ display ++ ": "
        padding = replicate (max 0 (nameWidth - length prefix0)) ' '
        statusRaw = statusFn res
        runs = printf "%4d runs in %s @ %6.1f/s" (trNumIterations res) (formatMillisPadded expectedDurationMs (trTestDuration res)) (testsPerSecond (trNumIterations res) (trTestDuration res))
        statusPart = colorizeStatusPart useColor (trCached res) statusRaw runs
        stressPart =
          case stressWorkerOverview res of
            Just txt -> " | " ++ txt
            Nothing -> ""
    in prefix0 ++ padding ++ statusPart ++ stressPart

stressWorkerOverview :: TestResult -> Maybe String
stressWorkerOverview res =
    case trRaw res of
      Aeson.Object obj ->
        let mEstMs = lookupDouble obj "stress_est_iteration_ms"
            mPhaseResMs = lookupDouble obj "stress_phase_resolution_ms"
            mSweep = lookupInt obj "stress_target_sweep_iters"
            mCalib = lookupInt obj "stress_calibrating_workers"
            mCovSeen = lookupInt obj "stress_phase_bins_seen"
            mCovTotal = lookupInt obj "stress_phase_bins_total"
            extraParts =
              catMaybes
                [ case mEstMs of
                    Just est | est > 0 -> Just (printf "iter~%0.3fms" est)
                    _ -> Nothing
                , case mPhaseResMs of
                    Just resMs | resMs > 0 -> Just (printf "coarse~%0.3fms" resMs)
                    _ -> Nothing
                , case mSweep of
                    Just sweep | sweep > 0 -> Just ("sweep=" ++ show sweep)
                    _ -> Nothing
                , case mCalib of
                    Just calib | calib > 0 -> Just ("calib=" ++ show calib)
                    _ -> Nothing
                , case (mCovSeen, mCovTotal) of
                    (Just seen, Just total) | total > 0 ->
                      let pct :: Double
                          pct = (fromIntegral seen * 100.0) / fromIntegral total
                      in Just (printf "cov=%d/%d(%0.1f%%)" seen total pct)
                    _ -> Nothing
                ]
        in if null extraParts
             then Nothing
             else Just (unwords extraParts)
      _ -> Nothing
  where
    lookupInt :: Aeson.Object -> String -> Maybe Int
    lookupInt o key =
      case AesonKM.lookup (AesonKey.fromString key) o of
        Just v -> AesonTypes.parseMaybe Aeson.parseJSON v
        _ -> Nothing

    lookupDouble :: Aeson.Object -> String -> Maybe Double
    lookupDouble o key =
      case AesonKM.lookup (AesonKey.fromString key) o of
        Just v -> AesonTypes.parseMaybe Aeson.parseJSON v
        _ -> Nothing

testsPerSecond :: Int -> Double -> Double
testsPerSecond iterations durationMs
  | iterations <= 0 = 0
  | durationMs <= 0 = 0
  | otherwise = (fromIntegral iterations * 1000.0) / durationMs

-- | Format a live test line to the current terminal width.
formatTestLineFitted :: Bool -> (TestResult -> String) -> Double -> Int -> Int -> String -> TestResult -> String
formatTestLineFitted useColor statusFn expectedDurationMs nameWidth width display res
  | width <= 0 = ""
  | otherwise =
      fromMaybe fallback (firstFit (legacyLine : map alignedLine summaries ++ map compactLine summaries))
  where
    indent = if width >= 4 then "   " else ""
    statusRaw = statusFn res
    (statusPlain, statusRendered) = renderStatusToken useColor (trCached res) statusRaw
    (statusFieldPlain, statusFieldRendered) = renderStatusField useColor (trCached res) statusRaw
    duration = formatSecondsCompactPadded expectedDurationMs (trTestDuration res)
    summaryFull = show (trNumIterations res) ++ " runs " ++ duration
    summaryCompact = show (trNumIterations res) ++ "r " ++ duration
    summaries = [Just summaryFull, Just summaryCompact, Nothing]
    legacyRendered = formatTestLineWith useColor statusFn expectedDurationMs nameWidth display res
    legacyLine
      | termVisibleLength legacyRendered <= width = Just legacyRendered
      | otherwise = Nothing
    prefix0 = indent ++ display ++ ": "
    alignedPrefix = prefix0 ++ replicate (max 0 (nameWidth - length prefix0)) ' '
    alignedLine mSummary =
      let summaryPad = maybe 0 (\s -> 1 + length s) mSummary
          fixed = length alignedPrefix + length statusFieldPlain + summaryPad
      in if fixed <= width
           then Just (alignedPrefix
                      ++ statusFieldRendered
                      ++ maybe "" (\s -> " " ++ s) mSummary)
           else Nothing
    compactLine mSummary =
      let summaryPad = maybe 0 (\s -> 1 + length s) mSummary
          fixed = length indent + 2 + length statusPlain + summaryPad
          nameBudget = width - fixed
      in if nameBudget >= 1
           then Just (indent
                      ++ fitTestDisplay nameBudget display
                      ++ ": "
                      ++ statusRendered
                      ++ maybe "" (\s -> " " ++ s) mSummary)
           else Nothing
    firstFit = listToMaybe . mapMaybe id
    fallback
      | width >= length statusPlain = statusRendered
      | otherwise = fitTestDisplay width display

formatTestFinalLineRenderer :: Bool -> Double -> Int -> String -> TestResult -> Int -> String
formatTestFinalLineRenderer useColor expectedDurationMs nameWidth display res cols =
    formatTestLineFitted useColor formatTestStatus expectedDurationMs nameWidth cols display res

formatTestLiveLineRenderer :: Bool -> Double -> Int -> String -> TestResult -> Int -> String
formatTestLiveLineRenderer useColor expectedDurationMs nameWidth display res cols =
    formatTestLineFitted useColor formatTestStatusLive expectedDurationMs nameWidth cols display res

formatTestDetailLines :: Bool -> Bool -> TestResult -> [String]
formatTestDetailLines useColor showLog res =
    let skipped = trSkipped res
        ok = trSuccess res == Just True && trException res == Nothing && not skipped
        wantDetails = showLog || skipped || not ok
        skipLines = case trSkipReason res of
          Just reason ->
            [ testColorApply useColor [testColorYellow] ("    skipped: " ++ reason)
            ]
          Nothing -> []
        excLines = case trException res of
          Just exc | not skipped ->
            [ testColorApply useColor [testColorRed] ("    " ++ line)
            | line <- lines exc
            ]
          Nothing -> []
        outputLines =
          if wantDetails
            then formatCombinedLogLines (trStdOut res) (trStdErr res)
            else []
    in if wantDetails
         then skipLines ++ excLines ++ outputLines
         else []
  where
    formatCombinedLogLines mOut mErr =
      let out = maybe "" id mOut
          err = maybe "" id mErr
      in if not (testOutputMeaningful out) && not (testOutputMeaningful err)
           then []
           else
             let chunks = dedupCombinedOutput out err
                 multi = length chunks > 1
             in concatMap (renderChunk multi) chunks
    renderChunk multi (chunk, count) =
      let header =
            if multi
              then ["    == " ++ show count ++ " test runs with this output:"]
              else []
          body = map ("    " ++) (lines chunk)
      in header ++ body ++ [""]
    testOutputMeaningful msgs =
      any (\line -> not (all isSpace line) && not ("== Running test," `isPrefixOf` line)) (lines msgs)
    splitTestOutput buf =
      let ls = lines buf
          isMarker line = "== Running test, iteration:" `isInfixOf` stripAnsi (trim line)
          step (chunks, current, seenMarker) line
            | isMarker line =
                if seenMarker
                  then (chunks ++ [trim current], "", True)
                  else (chunks, "", True)
            | otherwise =
                let current' = if null current then line else current ++ "\n" ++ line
                in (chunks, current', seenMarker)
          (chunks0, current0, seenMarker) = foldl' step ([], "", False) ls
          chunks1 =
            if seenMarker
              then chunks0 ++ [trim current0]
              else if null (trim buf) then [] else [trim buf]
      in chunks1
    renderIterationOutput out err =
      let out' = trim out
          err' = trim err
          renderSection label content =
            label ++ ":\n" ++ unlines (map ("  " ++) (lines content))
          parts = catMaybes
            [ if null out' then Nothing else Just (renderSection "STDOUT" out')
            , if null err' then Nothing else Just (renderSection "STDERR" err')
            ]
      in intercalate "\n" parts
    dedupCombinedOutput out err =
      let outChunks = splitTestOutput out
          errChunks = splitTestOutput err
          n = max (length outChunks) (length errChunks)
          getChunk xs i = if i < length xs then xs !! i else ""
          combined = [ renderIterationOutput (getChunk outChunks i) (getChunk errChunks i) | i <- [0..n-1] ]
          parts = filter (not . null . trim) combined
          stepCount (order, acc) chunk =
            let acc' = M.insertWith (+) chunk 1 acc
                order' = if M.member chunk acc then order else order ++ [chunk]
            in (order', acc')
          (order, acc) = foldl' stepCount ([], M.empty) parts
      in [ (chunk, M.findWithDefault 0 chunk acc) | chunk <- order ]
    trim s =
      let dropEnd = reverse . dropWhile isSpace . reverse
      in dropWhile isSpace (dropEnd s)
    stripAnsi [] = []
    stripAnsi ('\ESC':'[':xs) = stripAnsi (dropAnsi xs)
    stripAnsi (x:xs) = x : stripAnsi xs
    dropAnsi [] = []
    dropAnsi (c:cs)
      | c == 'm' = cs
      | otherwise = dropAnsi cs
