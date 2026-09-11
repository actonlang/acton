module ScaleOptionTests (scaleOptionTests) where

import Control.Monad (forM_)
import Data.List (isInfixOf)
import qualified Acton.CommandLineParser as C
import qualified Options.Applicative as O
import System.Exit (ExitCode(..))
import Test.Tasty
import Test.Tasty.HUnit

scaleOptionTests :: TestTree
scaleOptionTests = testGroup "performance scaling options"
  [ testCase "scale captures explicit resource limits" $ do
      opts <- parseScale ["--start-scale", "1000", "--end-scale", "100000", "--max-memory", "50%", "--max-time", "2h"]
      assertEqual "starting scale" (Just 1000) (C.testStartScale opts)
      assertEqual "inclusive endpoint" (Just 100000) (C.testEndScale opts)
      assertEqual "memory percentage" (Just (C.MemoryPercent 50)) (C.testMaxMemory opts)
      assertEqual "duration is stored in milliseconds" 7200000 (C.testMaxTime opts)
      assertBool "the total limit is explicit" (C.testMaxTimeSet opts)
  , testCase "scale needs no flags and defaults to release builds" $ do
      opts <- parseScale []
      assertEqual "release by default" C.ReleaseFast (C.optimize (C.testCompile opts))
      assertEqual "default starting scale is chosen by the runner" Nothing (C.testStartScale opts)
      assertEqual "no endpoint means automatic exploration" Nothing (C.testEndScale opts)
      assertEqual "default memory limit is chosen by the runner" Nothing (C.testMaxMemory opts)
      assertBool "default total limit is chosen by the runner" (not (C.testMaxTimeSet opts))
      explicit <- parseScale ["--optimize", "Debug"]
      assertEqual "explicit debug remains available" C.Debug (C.optimize (C.testCompile explicit))
  , testCase "memory limits preserve exact byte units and finite percentages" $ do
      forM_ [("1", C.MemoryBytes 1), ("256B", C.MemoryBytes 256),
             ("2MiB", C.MemoryBytes (2 * 1024^2)), ("3gib", C.MemoryBytes (3 * 1024^3)),
             ("1TB", C.MemoryBytes (1000^4)), ("1TiB", C.MemoryBytes (1024^4)),
             ("0.5%", C.MemoryPercent 0.5), ("100%", C.MemoryPercent 100)] $ \(value, expected) ->
        assertEqual value (Just expected) . C.testMaxMemory =<< parseScale ["--max-memory", value]
      forM_ ["0", "-1MiB", "1.5GiB", "1XB", "0%", "-1%", "100.1%", "NaN%", "1e1000%"] $ \value ->
        rejects ["test", "scale", "--max-memory", value]
  , testCase "study durations are finite and starting scales are positive" $ do
      forM_ [("250ms", 250), ("1.5s", 1500), ("2m", 120000), ("0.5H", 1800000)] $ \(value, expected) ->
        assertEqual value expected . C.testMaxTime =<< parseScale ["--max-time", value]
      forM_ ["0ms", "-1s", "0.1ms", "10", "1d", "NaNs", "1e100h"] $ \value ->
        rejects ["test", "scale", "--max-time", value]
      forM_ [1, maxBound :: Int] $ \value ->
        assertEqual "positive starting scale" (Just value) . C.testStartScale =<< parseScale ["--start-scale", show value]
      forM_ ["--start-scale", "--end-scale"] $ \option ->
        forM_ ["0", "-1", "1.5", show (toInteger (maxBound :: Int) + 1)] $ \value ->
          rejects ["test", "scale", option, value]
  , testCase "scale and perf reject each other's workload controls" $ do
      forM_ [["--scale", "7"], ["--time", "1s"], ["--iter", "1"],
             ["--min-iter", "1"], ["--max-iter", "1"], ["--min-time", "1"],
             ["--stress-workers", "1"], ["--scaling"], ["--min-scale", "1000"]] $ \args ->
        rejects (["test", "scale"] ++ args)
      forM_ [["--start-scale", "1"], ["--end-scale", "1000"], ["--max-memory", "50%"],
             ["--max-time", "1h"], ["--scaling"]] $ \args ->
        rejects (["test", "perf"] ++ args)
  , testCase "ordinary limits keep milliseconds and reject scaling controls" $ do
      forM_ [[], ["list"], ["stress"]] $ \mode ->
        forM_ [["--scaling"], ["--start-scale", "1"], ["--end-scale", "1000"], ["--max-memory", "50%"]] $ \args ->
          rejects (["test"] ++ mode ++ args)
      case parseOptions ["test", "stress", "--max-time", "0"] of
        O.Success (C.CmdOpt _ (C.Test (C.TestStress opts))) ->
          assertEqual "ordinary zero retains the unlimited convention" (0, True) (C.testMaxTime opts, C.testMaxTimeSet opts)
        _ -> assertFailure "expected ordinary millisecond limit"
  , testCase "global options and test selection work throughout scale commands" $ do
      let selection = ["--module", "timers", "--name", "increasing", "--tag", "input"]
      forM_
        [ ["test", "--color", "never", "scale", "--start-scale", "7"] ++ selection
        , ["test", "scale", "--color", "never", "--start-scale", "7"] ++ selection
        , ["test", "scale", "--start-scale", "7", "--color", "never"] ++ selection
        , ["test", "scale", "--start-scale", "7"] ++ selection ++ ["--color", "never"]
        ] $ \args -> case parseOptions args of
          O.Success (C.CmdOpt globals (C.Test (C.TestScale opts))) -> do
            assertEqual (unwords args) C.Never (C.color globals)
            assertEqual "scale and selection survive global options"
              (Just 7, ["timers"], ["increasing"], ["input"])
              (C.testStartScale opts, C.testModules opts, C.testNames opts, C.testTags opts)
          O.Failure failure -> assertFailure (fst (O.renderFailure failure "acton"))
          _ -> assertFailure ("unexpected command: " ++ unwords args)
  , testCase "saved reports are independent of study options" $ do
      forM_ [["test", "scale", "--report", "run.jsonl", "--color", "never"],
             ["test", "--color", "never", "scale", "--report", "run.jsonl"]] $ \args ->
        case parseOptions args of
          O.Success (C.CmdOpt globals (C.Test (C.TestScaleReport path Nothing))) -> do
            assertEqual "recording path" "run.jsonl" path
            assertEqual "display options apply" C.Never (C.color globals)
          _ -> assertFailure ("expected saved report: " ++ unwords args)
      forM_ [["--max-time", "1s"], ["--name", "dct"], ["--start-scale", "2"], ["--end-scale", "2"], ["--json"], ["--record"]] $ \args ->
        rejects (["test", "scale", "--report", "run.jsonl"] ++ args)
  , testCase "comparison always takes one baseline file in live and report modes" $ do
      forM_ [["--compare", "before.jsonl"], ["--name", "dct", "--compare", "before.jsonl"],
             ["--compare", "before.jsonl", "--max-time", "2m"]] $ \args -> do
        opts <- parseScale args
        assertEqual "live baseline" (Just "before.jsonl") (C.testCompare opts)
      forM_ [["--report", "after.jsonl", "--compare", "before.jsonl"],
             ["--compare", "before.jsonl", "--report", "after.jsonl"]] $ \args ->
        case parseOptions (["test", "scale"] ++ args) of
          O.Success (C.CmdOpt _ (C.Test (C.TestScaleReport path baseline))) ->
            assertEqual "current and baseline have fixed roles" ("after.jsonl", Just "before.jsonl") (path, baseline)
          O.Failure failure -> assertFailure (fst (O.renderFailure failure "acton"))
          _ -> assertFailure "expected comparison report"
      forM_ [["--report", "one", "two"], ["--compare", "one", "two"],
             ["--compare", "one", "--compare", "two"], ["--report", "one", "--report", "two"]] $ \args ->
        rejects (["test", "scale"] ++ args)
      forM_ [[], ["perf"], ["list"], ["stress"]] $ \mode ->
        rejects (["test"] ++ mode ++ ["--compare", "before.jsonl"])
  , testCase "scale help exposes study and report controls" $
      case parseOptions ["test", "scale", "--help"] of
        O.Failure failure -> do
          let (text, code) = O.renderFailure failure "acton"
          assertEqual text ExitSuccess code
          forM_ ["--start-scale N", "--end-scale N", "--max-memory LIMIT", "--max-time DURATION", "--report FILE", "--compare FILE"] $ \option ->
            assertBool text (option `isInfixOf` unwords (words text))
          assertBool text (not (any (`isInfixOf` text) ["--scaling", "--scale N", "--time DURATION", "--iter", "--min-time"]))
        _ -> assertFailure "expected scale help"
  ]

parseOptions :: [String] -> O.ParserResult C.CmdLineOptions
parseOptions = O.execParserPure C.cmdLinePrefs (O.info (C.cmdLineParser O.<**> O.helper) mempty)

parseScale :: [String] -> IO C.TestOptions
parseScale args = case parseOptions (["test", "scale"] ++ args) of
    O.Success (C.CmdOpt _ (C.Test (C.TestScale opts))) -> return opts
    O.Failure failure -> assertFailure (fst (O.renderFailure failure "acton")) >> fail "invalid options"
    _ -> assertFailure "expected scale options" >> fail "invalid command"

rejects :: [String] -> Assertion
rejects args = case parseOptions args of
    O.Failure _ -> return ()
    _ -> assertFailure ("must reject " ++ unwords args)
