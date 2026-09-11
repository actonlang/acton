module TestFormat
  ( formatTestStatus
  , formatTestStatusLive
  , formatTestLineWith
  , formatTestLineFitted
  , formatTestFinalLineRenderer
  , formatTestLiveLineRenderer
  , formatTestDetailLines
  , formatTestPerfLines
  , testColorApply
  , testColorBold
  , testColorRed
  , testColorGreen
  , testColorYellow
  , testColorReset
  ) where

import Acton.Testing (TestResult(..))
import TestPerf
import Data.Char (isSpace)
import Data.List (foldl', isPrefixOf, isInfixOf, intercalate)
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe, mapMaybe)
import qualified Data.Map as M
import qualified Data.Text as T
import TerminalSize (termFitAnsiRight, termFitPlainRight, termVisibleLength)
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

-- | Render a performance table. Keep the row count stable when the terminal
-- resizes, dropping outliers, range and spread before the mean and comparison.
formatTestPerfLines :: Bool -> Maybe Aeson.Object -> TestResult -> [Int -> String]
formatTestPerfLines useColor baseline res =
    case testPerfData res of
      Just obj -> table obj ++ footers obj
      Nothing -> []
  where
    paint color = testColorApply (useColor && not (null color)) [color]
    dim = paint "\ESC[2m"
    missing = dim "—"
    interval metric obj = baseline >>= (\old -> perfMeanInterval metric old obj)
    percentage previous value = case previous of
      Nothing -> (Nothing, "—")
      Just 0 -> (Nothing, if value == 0 then "+0.0%" else "from 0")
      Just old ->
        let pct = (value - old) / abs old * 100
        in if isNaN pct || isInfinite pct then (Nothing, "n/a")
           else (Just pct, fitNumber 11 [printf "%+.1f%%" pct, printf "%+.2e%%" pct, printf "%+.0e%%" pct])
    meanChange obj metric value =
      let previous = do
            old <- baseline
            if perfComparable metric old obj then perfNumber old (perfStatKey metric "avg") else Nothing
          (pct, text) = percentage previous value
          bounds = if metric == "ipc" then Nothing else interval metric obj
          (icon, color) = case (pct, bounds) of
            (Just _, Just (lo, _)) | lo > 0 -> ("💩", "\ESC[91m")
            (Just _, Just (_, hi)) | hi < 0 -> ("⚡", "\ESC[92m")
            _ -> ("", "\ESC[2m")
      in padLeft 11 (paint color text) ++ " "
          ++ (if null icon then "  " else paint (if icon == "⚡" then testColorYellow else color) icon)
    footerChange obj key value = case baseline >>= (\old -> if perfComparable key old obj then perfNumber old key else Nothing) of
      Nothing -> ""
      Just old ->
        let (pct, text) = percentage (Just old) value
            color = case pct of
              Just p | p > 0 -> "\ESC[91m"
              Just p | p < 0 -> "\ESC[92m"
              _ -> "\ESC[2m"
        in " (" ++ paint color text ++ ")"
    fitNumber width choices = fromMaybe (last choices) (listToMaybe [s | s <- choices, length s <= width])
    padLeft width s = replicate (max 0 (width - termVisibleLength s)) ' ' ++ s
    padRight width s = s ++ replicate (max 0 (width - termVisibleLength s)) ' '
    single color unit value =
      let (scale, suffix) = perfScale unit value
      in paint color (perfDigits (value / scale)) ++ dim suffix
    pair unit separator (colorA, colorB) a b =
      single colorA unit a ++ separator ++ single colorB unit b
    quantity width color unit value =
      let (scale, suffix) = perfScale unit value
          scaled = value / scale
          digits = fitNumber width [perfDigits scaled, printf "%.1e" scaled, printf "%.0e" scaled]
      in padLeft width (paint color digits) ++ padRight 2 (dim suffix)
    row obj (metric, label, unit) = do
      value <- perfNumber obj (perfStatKey metric "avg")
      return $ \numberWidth showSpread ->
        let q = quantity numberWidth
            meanValue = q "\ESC[92m" unit value
            mean = if not showSpread then meanValue else case perfNumber obj (perfStatKey metric "stdev") of
              Just sd | sd >= 0 && trNumIterations res > 1 -> meanValue ++ " ± " ++ q testColorGreen unit sd
              _ -> padRight (2 * (numberWidth + 2) + 3) meanValue
            range = case (perfNumber obj (perfStatKey metric "min"), perfNumber obj (perfStatKey metric "max")) of
              (Just lo, Just hi) -> q "\ESC[36m" unit lo ++ " … " ++ q "\ESC[35m" unit hi
              _ -> padLeft (numberWidth + 2) missing
            outliers = case perfNumber obj (perfStatKey metric "outlier_count") of
              Just n ->
                let pct = n / fromIntegral (trNumIterations res) * 100
                    count = fitNumber 14 [printf "%.0f (%.0f%%)" n pct, printf "%.1e (%.0f%%)" n pct]
                in paint (if pct >= 10 then testColorYellow else "\ESC[2m") count
              _ -> missing
        in [label, mean, range, outliers, meanChange obj metric value]
    table obj =
      let headings numberWidth showSpread =
            let title color = padLeft (numberWidth + 2) . paint color
                mean = title "\ESC[92m" "mean"
            in [ paint testColorBold "measurement"
               , mean ++ if showSpread then " ± " ++ title testColorGreen "σ" else ""
               , title "\ESC[36m" "min" ++ " … " ++ title "\ESC[35m" "max"
               , paint testColorYellow "outliers"
               , paint testColorBold "delta" ++ "   "
               ]
          rows = headings : mapMaybe (row obj) perfMetrics
          -- Choose widths only from the viewport, so benchmarks stay aligned.
          -- Reserve comparison space even when this test has no baseline.
          layouts = [ (16, 10, 4, [0,1,2,3,4], True)
                    , (13, 7, 2, [0,1,2,3,4], True)
                    , (13, 7, 2, [0,1,2,4], True)
                    , (13, 7, 2, [0,1,4], True)
                    , (13, 7, 1, [0,1,4], False)
                    ]
          widths (labelWidth, numberWidth, _, _, showSpread) =
            let q = numberWidth + 2
            in [labelWidth, if showSpread then 2 * q + 3 else q, 2 * q + 3, 14, 14]
          width layout@(_, _, gap, indexes, _) = 2 + sum [widths layout !! i | i <- indexes] + gap * (length indexes - 1)
          render cells cols =
            let layout@(labelWidth, numberWidth, gap, indexes, showSpread) =
                  fromMaybe (last layouts) (listToMaybe [l | l <- layouts, width l <= cols])
                columnWidths = widths layout
                values = cells numberWidth showSpread
                cell i =
                  let w = if i == 0 then max 1 (labelWidth - max 0 (width layout - cols)) else columnWidths !! i
                      s = values !! i
                  in if i == 0 then padRight w (termFitAnsiRight w s)
                     else if i == 1 || i == 2 then padRight w s
                     else padLeft w s
                shown = [i | i <- indexes, i /= 4 || isJust baseline]
            in termFitAnsiRight cols ("  " ++ intercalate (replicate gap ' ') (map cell shown))
      in map render rows
    footers obj = map (\line cols -> termFitAnsiRight cols line) $ catMaybes
      [ do info <- perfInfo obj
           workers <- perfNumber info "workers"
           scope <- if AesonKM.lookup (AesonKey.fromString "loop") info == Just (Aeson.Bool True)
             then do scale <- perfNumber info "scale"
                     iterations <- perfNumber obj "loop_iterations"
                     return (printf "per loop iteration at scale %.0f; %d runs; %.0f loop iterations" scale (trNumIterations res) iterations)
             else return (printf "whole invocation; %d runs" (trNumIterations res))
           return ("  " ++ scope ++ printf "; %.0f runtime workers" workers)
      , do info <- perfInfo obj
           raw <- AesonKM.lookup (AesonKey.fromString "calibration") info
           points <- AesonTypes.parseMaybe Aeson.parseJSON raw :: Maybe [Aeson.Object]
           let point obj = do
                 scale <- perfNumber obj "scale"
                 duration <- perfNumber obj "wall_ms"
                 return (printf "%.0f:" scale ++ single "" "ms" duration)
               observed = mapMaybe point points
               shown = if length observed > 4
                 then take 2 observed ++ ["…"] ++ drop (length observed - 2) observed
                 else observed
           if null observed then Nothing
             else Just ("  calibration: " ++ intercalate "; " shown)
      , if trNumIterations res < 4
             then Just "  limited samples: use a larger --time budget for more complete runs"
             else Nothing
      , do info <- perfInfo obj
           duration <- perfNumber info "warmup_duration_ms"
           return ("  warmup: " ++ single "" "ms" duration ++ " (excluded)")
      , do info <- perfInfo obj
           duration <- perfNumber info "measurement_duration_ms"
           measured <- perfNumber info "measurement_ms"
           return ("  measurement: " ++ single "" "ms" measured ++ " timed, " ++ single "" "ms" duration ++ " elapsed including setup and teardown")
      , do value <- perfNumber obj "median_wall_duration"
           return ("  wall median: " ++ single "" "ms" value ++ footerChange obj "median_wall_duration" value)
      , do value <- perfNumber obj "peak_rss"
           return ("  process peak RSS: " ++ single "" "B" value)
      , do (lo, hi) <- interval "wall_duration" obj
           return ("  wall mean delta (approx. 95% CI): " ++ pair "ms" " … " ("", "") lo hi)
      , do reason <- case baseline of
             Nothing -> Just "no recorded baseline"
             Just old -> perfComparisonReason "wall_duration" old obj
           return ("  comparison unavailable: " ++ reason)
      , do info <- perfCounterInfo obj
           let scope = case counterText info "scope" of
                 Just "process:user" -> "user only"
                 _ -> "user + kernel"
               hardware = if counterText info "status" == Just "available" then "; hardware: " ++ scope else ""
           return ("  CPU measurements: all process threads, including GC" ++ hardware)
      , do info <- perfCounterInfo obj
           status <- counterText info "status"
           if status == "available" then Nothing
             else Just ("  hardware counters unavailable: " ++ status)
      , do old <- baseline
           _ <- perfNumber old "avg_instructions"
           _ <- perfNumber obj "avg_instructions"
           reason <- perfComparisonReason "instructions" old obj
           if perfComparable "wall_duration" old obj
             then Just ("  hardware deltas unavailable: " ++ reason)
             else Nothing
      , Just ("  total: " ++ single "" "ms" (trTestDuration res) ++ case perfInfo obj >>= (\info -> perfNumber info "time_budget_ms") of
           Just budget -> " / " ++ single "" "ms" budget ++ " budget"
           Nothing -> "")
      ] ++ [""]
    counterText info key = case AesonKM.lookup (AesonKey.fromString key) info of
      Just (Aeson.String s) -> Just (T.unpack s)
      _ -> Nothing

-- Scale each quantity with decimal prefixes, including signed memory changes.
-- A small spread keeps its own unit instead of rounding to zero beside a mean.
perfScale :: String -> Double -> (Double, String)
perfScale unit value =
    fromMaybe fallback (listToMaybe [entry | entry@(scale, _) <- scales, magnitude >= scale])
  where
    magnitude = abs value
    scales = case unit of
      "ms" -> [(1e6, "ks"), (1e3, "s"), (1, "ms"), (1e-3, "µs"), (1e-6, "ns")]
      "count" -> [(1e12, "T"), (1e9, "G"), (1e6, "M"), (1e3, "K"), (1, "")]
      "ratio" -> [(1, "")]
      _ -> [(1e12, "TB"), (1e9, "GB"), (1e6, "MB"), (1e3, "KB"), (1, "B")]
    fallback
      | unit == "ms" && magnitude > 0 = (1e-6, "ns")
      | unit `elem` ["count", "ratio"] = (1, "")
      | otherwise = (1, unit)

perfDigits :: Double -> String
perfDigits value
  | abs value >= 10000 = printf "%.2e" value
  | value /= 0 && abs value < 1 = printf "%.2e" value
  | abs value >= 100 = printf "%.0f" value
  | abs value >= 10 = printf "%.1f" value
  | otherwise = printf "%.2f" value

-- | Format a single test result line with alignment and timing.
formatTestLineWith :: Bool -> (TestResult -> String) -> Double -> Int -> String -> TestResult -> String
formatTestLineWith useColor statusFn expectedDurationMs nameWidth display res =
    let prefix0 = "   " ++ display ++ ": "
        padding = replicate (max 0 (nameWidth - length prefix0)) ' '
        statusRaw = statusFn res
        runs = case perfPhase res of
          Just phase -> phase ++ printf ": %d measured samples, %s elapsed" (trNumIterations res) (formatSecondsCompact (trTestDuration res))
          Nothing -> printf "%4d runs in %s @ %6.1f/s" (trNumIterations res) (formatMillisPadded expectedDurationMs (trTestDuration res)) (testsPerSecond (trNumIterations res) (trTestDuration res))
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
    summaryFull = maybe "" (++ " ") (perfPhase res) ++ show (trNumIterations res) ++ " runs " ++ duration
    summaryCompact = maybe "" (++ " ") (perfPhase res) ++ show (trNumIterations res) ++ "r " ++ duration
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

formatTestFinalLineRenderer :: Bool -> Bool -> Double -> Int -> String -> TestResult -> Int -> String
formatTestFinalLineRenderer useColor perfMode expectedDurationMs nameWidth display res cols
  | perfMode && isJust (testPerfData res) =
      termFitAnsiRight cols (testColorApply useColor [testColorBold] "Benchmark"
        ++ testColorApply useColor ["\ESC[2m"] (printf " (%d runs)" (trNumIterations res))
        ++ ": " ++ display)
  | otherwise = formatTestLineFitted useColor formatTestStatus expectedDurationMs nameWidth cols display res

formatTestLiveLineRenderer :: Bool -> Double -> Int -> String -> TestResult -> Int -> String
formatTestLiveLineRenderer useColor expectedDurationMs nameWidth display res cols =
    formatTestLineFitted useColor formatTestStatusLive expectedDurationMs nameWidth cols display res

perfPhase :: TestResult -> Maybe String
perfPhase res
  | trComplete res = Nothing
  | otherwise = case trRaw res of
      Aeson.Object obj -> case AesonKM.lookup (AesonKey.fromString "perf_phase") obj of
        Just (Aeson.String phase) -> Just (T.unpack phase)
        _ -> Nothing
      _ -> Nothing

formatTestDetailLines :: Bool -> Bool -> TestResult -> [String]
formatTestDetailLines useColor showLog res =
    let skipped = trSkipped res
        ok = trSuccess res == Just True && trException res == Nothing && not skipped
        wantDetails = showLog || skipped || not ok
        outcomeLines = case formatOutcomeSummaryLine useColor res of
          Just line -> [line]
          Nothing -> []
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
         then outcomeLines ++ skipLines ++ excLines ++ outputLines
         else []
  where
    formatOutcomeSummaryLine useColor' result
      | trNumIterations result <= 1 = Nothing
      | trNumFailures result <= 0 && trNumErrors result <= 0 && trNumSkipped result <= 0 = Nothing
      | otherwise =
          let (numSuccesses, numFailures, numErrors, numSkipped) = outcomeCounts result
              parts = catMaybes
                [ formatOutcomePart useColor' [testColorGreen] numSuccesses "ok"
                , formatOutcomePart useColor' [testColorRed] numFailures "fail"
                , formatOutcomePart useColor' [testColorBold, testColorRed] numErrors "err"
                , formatOutcomePart useColor' [testColorYellow] numSkipped "skip"
                ]
          in if null parts
               then Nothing
               else Just ("    outcomes: " ++ intercalate ", " parts)
    outcomeCounts result =
      let numSkipped = max 0 (trNumSkipped result)
          numFailures = max 0 (trNumFailures result)
          numErrors = max 0 (trNumErrors result)
          numSuccesses = max 0 (trNumIterations result - numSkipped - numFailures - numErrors)
      in (numSuccesses, numFailures, numErrors, numSkipped)
    formatOutcomePart useColor' styles count label
      | count <= 0 = Nothing
      | otherwise = Just (testColorApply useColor' styles (show count ++ " " ++ label))
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
