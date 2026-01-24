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
import Data.List (foldl', isPrefixOf)
import Data.Maybe (isJust)
import qualified Data.Map as M
import qualified Data.Set as Set
import Data.List.Split (splitOn)
import Text.Printf (printf)

-- | Compute the status label (OK/FAIL/ERR/FLAKY) for a test.
formatTestStatus :: TestResult -> String
formatTestStatus res =
    let ok = trSuccess res == Just True && trException res == Nothing
        base
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
          _ -> testColorApply useColor [testColorBold, testColorRed] statusRaw
        star =
          if cached
            then if useColor then testColorYellow ++ "*" ++ testColorReset else "*"
            else ""
    in statusColored ++ star ++ pad ++ ": " ++ runs

-- | Format a single test result line with alignment and timing.
formatTestLineWith :: Bool -> (TestResult -> String) -> Int -> String -> TestResult -> String
formatTestLineWith useColor statusFn nameWidth display res =
    let prefix0 = "  " ++ display ++ ": "
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
            then concat
              [ formatLogLines "STDOUT" (trStdOut res)
              , formatLogLines "STDERR" (trStdErr res)
              ]
            else []
    in if wantDetails
         then excLines ++ outputLines
         else []
  where
    formatLogLines label mOut =
      case mOut of
        Just out | testOutputMeaningful out ->
          let chunks = dedupTestOutput (trim out)
              multi = length chunks > 1
              header = ["    " ++ label ++ ":"]
              body = concatMap (renderChunk multi) chunks
          in header ++ body
        _ -> []
    renderChunk multi (chunk, runs) =
      let prefix = "      "
          labelLines =
            if multi
              then [prefix ++ "== " ++ show (length runs) ++ " test runs with this output:"]
              else []
          chunkLines = map (prefix ++) (lines chunk)
      in labelLines ++ chunkLines ++ [""]
    testOutputMeaningful msgs =
      any (\line -> not (all isSpace line) && not ("== Running test," `isPrefixOf` line)) (lines msgs)
    dedupTestOutput buf =
      let parts = filter (not . null . trim) (splitOn "== Running test, iteration: " buf)
          step (order, acc) part =
            let trimmed = trim part
                (numLine, rest) = break (== '\n') trimmed
                content = case rest of
                  [] -> ""
                  (_:xs) -> trim xs
            in case readMaybeInt numLine of
                 Nothing -> (order, acc)
                 Just n ->
                   let acc' = M.insertWith Set.union content (Set.singleton n) acc
                       order' = if M.member content acc then order else order ++ [content]
                   in (order', acc')
          (order, acc) = foldl' step ([], M.empty) parts
      in [ (chunk, Set.toList (M.findWithDefault Set.empty chunk acc)) | chunk <- order ]
    trim s =
      let dropEnd = reverse . dropWhile isSpace . reverse
      in dropWhile isSpace (dropEnd s)

readMaybeInt :: String -> Maybe Int
readMaybeInt s =
    case reads s of
      [(n, "")] -> Just n
      _ -> Nothing
