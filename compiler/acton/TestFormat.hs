module TestFormat
  ( formatTestStatus
  , formatTestStatusLive
  , formatTestLineWith
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
import Data.Maybe (isJust, catMaybes)
import qualified Data.Map as M
import Text.Printf (printf)

-- | Compute the status label (OK/FAIL/ERR/FLAKY) for a test.
formatTestStatus :: TestResult -> String
formatTestStatus res =
    let ok = trSuccess res == Just True && trException res == Nothing
        base
          | trSnapshotUpdated res = "UPDATED"
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
          "OK" -> testColorApply useColor [testColorGreen] statusRaw
          "UPDATED" -> testColorApply useColor [testColorYellow] statusRaw
          _ -> testColorApply useColor [testColorBold, testColorRed] statusRaw
        star =
          if cached
            then if useColor then testColorYellow ++ "*" ++ testColorReset else "*"
            else ""
    in statusColored ++ star ++ pad ++ ": " ++ runs

-- | Format a single test result line with alignment and timing.
formatTestLineWith :: Bool -> (TestResult -> String) -> Int -> String -> TestResult -> String
formatTestLineWith useColor statusFn nameWidth display res =
    let prefix0 = "   " ++ display ++ ": "
        padding = replicate (max 0 (nameWidth - length prefix0)) ' '
        statusRaw = statusFn res
        runs = printf "%4d runs in %3.3fms" (trNumIterations res) (trTestDuration res)
        statusPart = colorizeStatusPart useColor (trCached res) statusRaw runs
    in prefix0 ++ padding ++ statusPart

formatTestDetailLines :: Bool -> Bool -> TestResult -> [String]
formatTestDetailLines useColor showLog res =
    let ok = trSuccess res == Just True && trException res == Nothing
        wantDetails = showLog || not ok
        excLines = case trException res of
          Just exc ->
            [ testColorApply useColor [testColorRed] ("    " ++ line)
            | line <- lines exc
            ]
          Nothing -> []
        outputLines =
          if wantDetails
            then formatCombinedLogLines (trStdOut res) (trStdErr res)
            else []
    in if wantDetails
         then excLines ++ outputLines
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
