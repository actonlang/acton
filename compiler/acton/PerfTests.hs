{-# LANGUAGE OverloadedStrings #-}
module PerfTests (perfTests, perfIntegrationTests) where

import Control.Monad
import qualified Acton.CommandLineParser as C
import qualified Options.Applicative as O
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.List (isInfixOf, isSuffixOf, find, elemIndices, nub)
import Data.IORef
import TerminalSize (termVisibleLength, termFitAnsiRight, termRenderedRows)
import qualified Data.Map as M
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Utils (fillBytes)
import System.Directory
import System.Environment (getEnvironment)
import System.Exit
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import System.Process
import Test.Tasty
import Test.Tasty.HUnit

import qualified Acton.Fingerprint as Fingerprint
import Acton.Testing (TestResult(..))
import TestFormat (formatTestPerfLines)
import TestRunner (runPerfPairs, perfPairOrders)
import TestPerf (testPerfData, perfComparable, perfComparisonReason, perfBaselineScale, aggregatePerfRuns, perfPairedCount, perfMeanInterval, perfJson)

perfTests :: TestTree
perfTests = testGroup "performance baselines"
  [ testCase "performance defaults use release and one total time budget" $ do
      opts <- parsePerfOptions []
      assertEqual "release by default" C.ReleaseFast (C.optimize (C.testCompile opts))
      assertEqual "five seconds per benchmark" 5000 (C.testTime opts)
      assertEqual "scale is calibrated by default" Nothing (C.testScale opts)
      explicit <- parsePerfOptions ["--optimize", "Debug", "--time", "250ms"]
      assertEqual "explicit debug remains available" C.Debug (C.optimize (C.testCompile explicit))
      assertEqual "milliseconds are accepted" 250 (C.testTime explicit)
      forM_ [("5s", 5000), ("1.5s", 1500), ("0.001s", 1)] $ \(duration, expected) ->
        assertEqual duration expected . C.testTime =<< parsePerfOptions ["--time", duration]
      forM_ ["0ms", "-1s", "0.1ms", "100", "1e100s", "NaNs"] $ \duration ->
        case parseOptions ["test", "perf", "--time", duration] of
          O.Failure _ -> return ()
          _ -> assertFailure ("--time must reject " ++ duration)
      forM_ ["--iter", "--min-iter", "--max-iter", "--min-time", "--max-time", "--warmup"] $ \option ->
        case parseOptions ["test", "perf", option, "1"] of
          O.Failure _ -> return ()
          _ -> assertFailure ("perf must reject " ++ option)
      forM_ [1, 50000, maxBound :: Int] $ \scale ->
        assertEqual "positive workload scales are accepted" (Just scale) . C.testScale =<< parsePerfOptions ["--scale", show scale]
      forM_ ["0", "-1", "1.5", show (toInteger (maxBound :: Int) + 1)] $ \scale ->
        case parseOptions ["test", "perf", "--scale", scale] of
          O.Failure _ -> return ()
          _ -> assertFailure ("--scale must reject " ++ scale)
      forM_ [[], ["stress"]] $ \mode ->
        case parseOptions (["test"] ++ mode ++ ["--scale", "1"]) of
          O.Failure _ -> return ()
          _ -> assertFailure ("--scale must reject mode " ++ show mode)
      case parseOptions ["test"] of
        O.Success (C.CmdOpt _ (C.Test (C.TestRun normal))) -> assertEqual "ordinary tests keep debug" C.Debug (C.optimize (C.testCompile normal))
        _ -> assertFailure "ordinary test options failed to parse"
      case parseOptions ["test", "--iter", "2", "--min-time", "10"] of
        O.Success (C.CmdOpt _ (C.Test (C.TestRun normal))) ->
          assertEqual "ordinary test limits remain available" (2, 10) (C.testIter normal, C.testMinTime normal)
        _ -> assertFailure "ordinary test limits failed to parse"
      case parseOptions ["test", "stress", "--max-iter", "9", "--max-time", "20", "--stress-workers", "3"] of
        O.Success (C.CmdOpt _ (C.Test (C.TestStress stress))) ->
          assertEqual "stress test limits remain available" (9, 20, 3) (C.testMaxIter stress, C.testMaxTime stress, C.testStressWorkers stress)
        _ -> assertFailure "stress test limits failed to parse"
  , testCase "performance comparison targets and options compose" $ do
      forM_ ["git:main", "before.perf_data", "./git:main", "main"] $ \target ->
        forM_ [["--compare", target, "--name", "sample", "--scale", "50", "--time", "10s", "--record"],
               ["--name", "sample", "--record", "--time", "10s", "--scale", "50", "--compare", target]] $ \args -> do
          opts <- parsePerfOptions args
          assertEqual "preserve the comparison target" (Just target) (C.testCompare opts)
          assertEqual "preserve measurement options" (["sample"], Just 50, 10000, True)
            (C.testNames opts, C.testScale opts, C.testTime opts, C.testRecord opts)
      forM_ [["--compare", "one", "two"], ["--compare", "one", "--compare", "two"]] $ \args ->
        case parseOptions (["test", "perf"] ++ args) of
          O.Failure _ -> return ()
          _ -> assertFailure ("comparison must reject " ++ unwords args)
      case parseOptions ["test", "perf", "--help"] of
        O.Failure failure -> assertBool "help shows file and Git comparisons"
          ("--compare FILE|git:REF" `isInfixOf` fst (O.renderFailure failure "acton"))
        _ -> assertFailure "expected help text"
  , testCase "global options can appear throughout performance commands" $ do
      forM_
        [ ["test", "--color", "never", "perf", "--time", "100ms", "--scale", "7", "--name", "sample"]
        , ["test", "perf", "--color", "never", "--time", "100ms", "--scale", "7", "--name", "sample"]
        , ["test", "perf", "--time", "100ms", "--scale", "7", "--color", "never", "--name", "sample"]
        , ["test", "perf", "--time", "100ms", "--scale", "7", "--name", "sample", "--color", "never"]
        ] $ \args -> case parseOptions args of
          O.Success (C.CmdOpt globals (C.Test (C.TestPerf opts))) -> do
            assertEqual (unwords args) C.Never (C.color globals)
            assertEqual "performance options survive global options" (100, Just 7, ["sample"]) (C.testTime opts, C.testScale opts, C.testNames opts)
          O.Failure failure -> assertFailure (fst (O.renderFailure failure "acton"))
          _ -> assertFailure ("unexpected command: " ++ unwords args)
      case parseOptions ["test", "--quiet", "perf", "--time", "100ms", "--jobs", "2", "--name", "sample", "--no-progress", "--tag", "input"] of
        O.Success (C.CmdOpt globals (C.Test (C.TestPerf opts))) -> do
          assertEqual "several global options retain their values" (True, 2, True) (C.quiet globals, C.jobs globals, C.noProgress globals)
          assertEqual "leaf parsing resumes after every global option" ["input"] (C.testTags opts)
        O.Failure failure -> assertFailure (fst (O.renderFailure failure "acton"))
        _ -> assertFailure "expected performance options"
      case parseOptions ["test", "stress", "--iter", "2", "--color", "never", "--name", "sample"] of
        O.Success (C.CmdOpt globals (C.Test (C.TestStress opts))) ->
          assertEqual "stress options also mix with global options" (C.Never, 2, ["sample"]) (C.color globals, C.testIter opts, C.testNames opts)
        O.Failure failure -> assertFailure (fst (O.renderFailure failure "acton"))
        _ -> assertFailure "expected stress options"
  , testCase "performance help exposes time and workload scale controls" $
      case parseOptions ["test", "perf", "--help"] of
        O.Failure failure -> do
          let (text, code) = O.renderFailure failure "acton"
          assertEqual text ExitSuccess code
          assertBool text ("--time DURATION" `isInfixOf` unwords (words text))
          assertBool text ("--scale N" `isInfixOf` unwords (words text))
          assertBool text (not (any (`isInfixOf` text) ["--iter", "--min-time", "--max-time", "--warmup", "--scaling", "--start-scale", "--max-memory"]))
        _ -> assertFailure "expected performance help"
  , testCase "all metrics require the same complete performance identity" $ do
      let old = sample 10 0 10
          new = sample 12 0 10
          changed key value = changeIdentity key value old
          incompatible =
            [ ("machine", Aeson.String "another machine")
            , ("machine", Aeson.Null)
            , ("version", Aeson.String "1")
            , ("build", Aeson.String "ReleaseFast")
            , ("tags", Aeson.toJSON (["different"] :: [String]))
            , ("gc", Aeson.String "forced")
            , ("scale", Aeson.Number 8)
            , ("loop", Aeson.Bool False)
            , ("workers", Aeson.Number 2)
            ]
      forM_ incompatible $ \(key, value) -> forM_ ["wall_duration", "gc_duration", "cpu_user", "mem_usage_delta", "peak_rss", "median_wall_duration", "instructions"] $ \metric -> do
        assertBool (show key ++ " " ++ metric) (not (perfComparable metric (changed key value) new))
        assertBool "the rejected comparison has a reason" (perfComparisonReason metric (changed key value) new /= Nothing)
      forM_ (KM.keys identity) $ \key ->
        let missing = KM.insert "perf_info" (Aeson.Object (KM.delete key identity)) old
        in assertBool (show key) (not (perfComparable "wall_duration" missing new))
      assertBool "old recordings require a new identity" (not (perfComparable "wall_duration" (KM.delete "perf_info" old) new))
      build <- requireObject "build" identity
      forM_ ["no_threads", "db", "no_dbp"] $ \key -> do
        assertBool (show key ++ " changes the runtime condition")
          (not (perfComparable "wall_duration" (changeIdentity "build" (Aeson.Object (KM.insert key (Aeson.Bool True) build)) old) new))
        assertBool (show key ++ " must be recorded")
          (not (perfComparable "wall_duration" (changeIdentity "build" (Aeson.Object (KM.delete key build)) old) new))
      let longer = changeIdentity "measurement_ms" (Aeson.Number 9000)
            (changeIdentity "measurement_duration_ms" (Aeson.Number 9500)
              (changeIdentity "time_budget_ms" (Aeson.Number 10000) (sample 12 0 20)))
      assertBool "time budgets, realized durations and sample counts are not identity" (perfComparable "wall_duration" old longer)
      let footerOld = old `KM.union` KM.fromList [("median_wall_duration", Aeson.Number 10), ("peak_rss", Aeson.Number 100)]
          footerNew = new `KM.union` KM.fromList [("median_wall_duration", Aeson.Number 12), ("peak_rss", Aeson.Number 200)]
          text = unlines (renderPerf 119 False (Just (changeIdentity "machine" Aeson.Null footerOld)) (result footerNew))
      assertBool text ("comparison unavailable: machine identity is unavailable" `isInfixOf` text)
      assertBool "footer values cannot bypass identity checks" (not (any (`isInfixOf` text) ["+20.0%", "+100.0%", "⚡", "💩"]))
      case perfJson (Just (KM.delete "perf_info" old)) (result new) of
        Just (Aeson.Object report) -> do
          assertEqual "no legacy confidence interval" (Just Aeson.Null) (KM.lookup "mean_difference_ci95_ms" report)
          assertBool "JSON explains eligibility" (KM.lookup "comparison_unavailable_reason" report /= Just Aeson.Null)
        _ -> assertFailure "expected comparison metadata"
  , testCase "performance baselines accept zero runtime workers" $ do
      build <- requireObject "build" identity
      let noThreads = changeIdentity "build" (Aeson.Object (KM.insert "no_threads" (Aeson.Bool True) build))
            . changeIdentity "workers" (Aeson.Number 0)
          old = noThreads (sample 10 0 10)
          new = noThreads (sample 12 0 10)
          rendered = unlines (renderPerf 119 False (Just old) (result new))
      assertEqual "matching runtimes without threads are comparable" Nothing (perfComparisonReason "wall_duration" old new)
      assertEqual "zero workers retain the mean interval" (Just (2, 2)) (perfMeanInterval "wall_duration" old new)
      assertBool rendered ("+20.0%" `isInfixOf` rendered)
      assertEqual "worker counts must still match" (Just "runtime worker count differs")
        (perfComparisonReason "wall_duration" old (changeIdentity "workers" (Aeson.Number 1) new))
      forM_ [("workers", [-1, 0.5]), ("scale", [0, -1, 0.5])] $ \(key, invalid) ->
        forM_ invalid $ \n ->
          assertBool (show (key, n)) (not (perfComparable "wall_duration"
            (changeIdentity key (Aeson.Number n) old) (changeIdentity key (Aeson.Number n) new)))
  , testCase "recorded scale is reused only for known compatible loops" $ do
      let old = sample 10 0 10
      assertEqual "reuse the recorded workload" (Just 7) (perfBaselineScale old identity)
      forM_ [changeIdentity "machine" Aeson.Null old, changeIdentity "scale" (Aeson.Number 0) old,
             changeIdentity "scaling" (Aeson.Bool True) old,
             changeIdentity "loop" (Aeson.Bool False) old, changeIdentity "version" (Aeson.String "2") old,
             KM.delete "perf_info" old] $ \baseline ->
        assertEqual "cannot reuse a missing or incompatible scale" Nothing (perfBaselineScale baseline identity)
      assertEqual "worker count is checked after execution" (Just 7)
        (perfBaselineScale (changeIdentity "workers" (Aeson.Number 2) old) identity)
      assertEqual "fixed-work samples cannot compare with time-budgeted samples" (Just "measurement sampling mode differs")
        (perfComparisonReason "wall_duration" (changeIdentity "scaling" (Aeson.Bool True) old) old)
  , testCase "all randomized pair schedules balance order and share an explicit scale" $ do
      assertEqual "all balanced schedules are distinct" 6 (length (nub perfPairOrders))
      assertEqual "six possible schedules" 6 (length perfPairOrders)
      forM_ perfPairOrders $ \order -> do
        launches <- newIORef []
        measured <- runPerfPairs order 5000 (Just 50000) $ \old scale budget -> do
          modifyIORef' launches (++ [(old, scale, budget)])
          return (result (changeIdentity "scale" (Aeson.Number 50000) (sample (if old then 10 else 12) 0 10)))
            { trTestDuration = fromIntegral budget }
        calls <- readIORef launches
        assertEqual "the chosen schedule controls execution" (concatMap (\old -> [old, not old]) order) [old | (old, _, _) <- calls]
        assertEqual "four pairs per schedule" 4 (length order)
        assertEqual "each version goes first twice" 2 (length (filter id order))
        assertEqual "both versions always receive the same explicit scale" (replicate 8 (Just 50000)) [scale | (_, scale, _) <- calls]
        forM_ [False, True] $ \side ->
          assertEqual "each revision receives the full requested budget" 5000 (sum [budget | (old, _, budget) <- calls, old == side])
        case measured of
          Right pairs -> do
            assertEqual "four complete pairs" 4 (length pairs)
            forM_ pairs $ \(old, new) -> do
              assertEqual "baseline stays first in stored pairs" 10 =<< (requirePerfData old >>= requireNumber "avg_wall_duration")
              assertEqual "current stays second in stored pairs" 12 =<< (requirePerfData new >>= requireNumber "avg_wall_duration")
          Left res -> assertFailure (show res)
  , testCase "automatic pair calibration runs once and stays within the budget" $ do
      launches <- newIORef []
      measured <- runPerfPairs [True, False, True, False] 5000 Nothing $ \old scale budget -> do
        modifyIORef' launches (++ [(old, scale, budget)])
        return (result (sample 10 0 10)) { trTestDuration = fromIntegral budget }
      calls <- readIORef launches
      assertEqual "one excluded baseline pilot" [(True, Nothing, 625)] (take 1 calls)
      assertEqual "all measurements reuse the pilot's scale" (replicate 8 (Just 7)) [scale | (_, scale, _) <- drop 1 calls]
      forM_ [False, True] $ \side ->
        assertBool "pilot time counts against its revision budget" (sum [budget | (old, _, budget) <- calls, old == side] <= 5000)
      case measured of
        Right pairs -> assertEqual "the pilot is not a measured pair" 4 (length pairs)
        Left res -> assertFailure (show res)
  , testCase "failed performance processes stop before an incomplete pair is counted" $ do
      launches <- newIORef (0 :: Int)
      measured <- runPerfPairs [True, False, True, False] 5000 (Just 7) $ \_ _ budget -> do
        n <- atomicModifyIORef' launches (\n -> (n + 1, n + 1))
        return (result (sample 10 0 10)) { trTestDuration = fromIntegral budget, trSuccess = Just (n < 4) }
      assertEqual "stop on the first failed process" 4 =<< readIORef launches
      case measured of
        Left failed -> assertEqual "return the failed process without aggregate results" (Just False) (trSuccess failed)
        Right _ -> assertFailure "a failed comparison cannot produce accepted pairs"
  , testCase "whole-invocation comparisons do not force a loop scale" $ do
      launches <- newIORef []
      measured <- runPerfPairs [True, False, True, False] 5000 Nothing $ \_ scale budget -> do
        modifyIORef' launches (++ [scale])
        return (result (changeIdentity "loop" (Aeson.Bool False) (sample 10 0 10)))
          { trTestDuration = fromIntegral budget }
      assertEqual "the pilot and both revisions can measure whole invocations" (replicate 9 Nothing) =<< readIORef launches
      case measured of
        Right pairs -> assertEqual "whole invocations still form independent pairs" 4 (length pairs)
        Left res -> assertFailure (show res)
  , testCase "performance preparation retries at the selected scale" $ do
      forM_ [False, True] $ \retryFails -> do
        launches <- newIORef []
        measured <- runPerfPairs [True, False, True, False] 5000 (Just 7) $ \old scale budget -> do
          previous <- readIORef launches
          modifyIORef' launches (++ [(old, scale, budget)])
          let obj = if null previous then changeIdentity "preparation_exhausted" (Aeson.Bool True) (sample 10 0 10)
                                    else sample 10 0 10
          return (result obj) { trTestDuration = fromIntegral budget
                              , trSuccess = Just (not (null previous) && not retryFails) }
        calls <- readIORef launches
        assertEqual "retry uses the remaining budget without recalibrating"
          [(True, Just 7, 1250), (True, Just 7, 3750)] (take 2 calls)
        case measured of
          Right pairs | not retryFails -> do
            assertEqual "only completed retries contribute to the pair" 1 (length pairs)
            assertEqual "current still runs after successful preparation" 3 (length calls)
          Left _ | retryFails -> assertEqual "a real failure ends retries" 2 (length calls)
          _ -> assertFailure "unexpected preparation retry result"
  , testCase "randomized pairs retain orientation when the second process retries" $ do
      launches <- newIORef []
      measured <- runPerfPairs [False, True, False, True] 5000 (Just 7) $ \old _ budget -> do
        previous <- readIORef launches
        modifyIORef' launches (++ [old])
        let n = length previous + 1
            obj = changeIdentity "preparation_exhausted" (Aeson.Bool (n == 2)) $
              KM.insert "sequence" (Aeson.toJSON n) (sample 10 0 10)
        return (result obj) { trTestDuration = fromIntegral budget, trSuccess = Just (n /= 2) }
      assertEqual "retry only the second side" [False, True, True] =<< readIORef launches
      case measured of
        Right [(old, new)] -> do
          assertEqual "accepted baseline follows its discarded preparation" 3 =<< (requirePerfData old >>= requireNumber "sequence")
          assertEqual "current keeps its earlier measurement" 1 =<< (requirePerfData new >>= requireNumber "sequence")
        _ -> assertFailure "expected one complete pair after retry"
  , testCase "later preparation exhaustion retains complete pairs" $ do
      launches <- newIORef (0 :: Int)
      measured <- runPerfPairs [True, False, True, False] 5000 (Just 7) $ \_ _ budget -> do
        n <- atomicModifyIORef' launches (\n -> (n + 1, n + 1))
        let obj = if n == 3 then changeIdentity "preparation_exhausted" (Aeson.Bool True) (sample 10 0 10)
                           else sample 10 0 10
        return (result obj) { trTestDuration = if n == 3 then 3000 else fromIntegral budget
                           , trSuccess = Just (n /= 3) }
      assertEqual "do not start the other side of an exhausted pair" 3 =<< readIORef launches
      case measured of
        Right pairs -> assertEqual "previous complete pairs remain usable" 1 (length pairs)
        Left res -> assertFailure (show res)
  , testCase "slow processes produce fewer complete pairs" $ do
      launches <- newIORef (0 :: Int)
      measured <- runPerfPairs [True, False, True, False] 5000 (Just 7) $ \_ _ budget -> do
        modifyIORef' launches (+ 1)
        return (result (sample 3000 0 1)) { trTestDuration = fromIntegral (max 3000 budget) }
      assertEqual "do not launch a pair that cannot fit its known preparation time" 2 =<< readIORef launches
      case measured of
        Right pairs -> assertEqual "long indivisible invocations reduce the number of pairs" 1 (length pairs)
        Left res -> assertFailure (show res)
  , testCase "process aggregation gives each process equal weight" $ do
      let process mean n = result $ KM.insert "loop_iterations" (Aeson.toJSON n)
            (changeIdentity "time_budget_ms" (Aeson.Number 1250) (sample mean 999 n))
          runs = zipWith process [10, 20, 30, 40] [1, 10, 100, 1000]
      combined <- requireAggregate "comparison" runs
      obj <- requirePerfData combined
      assertEqual "count processes, not inner iterations" 4 (trNumIterations combined)
      assertEqual "retain elapsed runtime" 400 (trTestDuration combined)
      assertEqual "equal process weights" 25 =<< requireNumber "avg_wall_duration" obj
      assertEqual "raw count also denotes processes" 4 =<< requireNumber "num_iterations" obj
      assertEqual "retain actual inner loop count" 1111 =<< requireNumber "loop_iterations" obj
      assertEqual "minimum is a process mean" 10 =<< requireNumber "min_wall_duration" obj
      assertEqual "maximum is a process mean" 40 =<< requireNumber "max_wall_duration" obj
      assertEqual "median interpolates between processes" 25 =<< requireNumber "median_wall_duration" obj
      sd <- requireNumber "stdev_wall_duration" obj
      assertBool "spread is between process means" (abs (sd - sqrt (500 / 3)) < 0.000001)
      info <- requireObject "perf_info" obj
      assertEqual "budgets sum across processes" 5000 =<< requireNumber "time_budget_ms" info
      assertEqual "retain ordered raw process measurements" (Just (Aeson.toJSON (map trRaw runs))) (KM.lookup "process_samples" obj)
  , testCase "process aggregation rejects incomplete or incompatible runs" $ do
      let good = result (sample 10 1 10)
          rejected runs = assertBool "cannot aggregate invalid processes" (case aggregatePerfRuns "comparison" runs of Nothing -> True; _ -> False)
      rejected []
      forM_ [good { trComplete = False }, good { trSuccess = Just False }, good { trCached = True },
             good { trNumFailures = 1 }, good { trSkipped = True }, good { trName = "other" },
             result (changeIdentity "scale" (Aeson.Number 8) (sample 10 1 10)),
             result (KM.delete "avg_wall_duration" (sample 10 1 10))] $ \bad -> rejected [good, bad]
      combined <- requireAggregate "comparison" [good, good]
      rejected [combined, combined]
      assertBool "pair IDs cannot be empty" (case aggregatePerfRuns "" [good] of Nothing -> True; _ -> False)
  , testCase "build options compare across configurations without mixing process samples" $ do
      let options = Aeson.toJSON (M.singleton ("lto" :: String) ("true" :: String))
          plain = sample 10 1 10
          tuned = changeIdentity "build_options" options (sample 9 1 10)
      assertBool "configuration changes can be measured" (perfComparable "wall_duration" plain tuned)
      old <- requireAggregate "options" (replicate 2 (result plain)) >>= requirePerfData
      newResult <- requireAggregate "options" (replicate 2 (result tuned))
      new <- requirePerfData newResult
      info <- requireObject "perf_info" new
      assertEqual "aggregation retains the selected options" (Just options) (KM.lookup "build_options" info)
      assertEqual "each side keeps a separate configuration" (Just 2) (perfPairedCount old new)
      assertBool "an aggregate cannot mix build configurations"
        (case aggregatePerfRuns "options" [result plain, result tuned] of Nothing -> True; _ -> False)
      case perfJson (Just old) newResult of
        Just (Aeson.Object report) -> do
          measurements <- requireObject "measurements" report
          recordedInfo <- requireObject "perf_info" measurements
          assertEqual "JSON retains build option provenance" (Just options) (KM.lookup "build_options" recordedInfo)
        _ -> assertFailure "expected performance report"
  , testCase "process aggregation omits partial metrics and uses maximum RSS" $ do
      let first = result $ KM.insert "peak_rss" (Aeson.Number 300) $ sample 10 1 10 `KM.union` KM.fromList
            [("mem_usage_delta_avg", Aeson.Number 1000), ("min_mem_usage_delta", Aeson.Number 1),
             ("stdev_mem_usage_delta", Aeson.Number 2), ("avg_instructions", Aeson.Number 100)]
          second = result $ KM.insert "peak_rss" (Aeson.Number 100) (sample 12 1 10)
      combined <- requireAggregate "comparison" [first, second]
      obj <- requirePerfData combined
      assertEqual "peak RSS is the maximum process footprint" 300 =<< requireNumber "peak_rss" obj
      forM_ ["mem_usage_delta_avg", "min_mem_usage_delta", "stdev_mem_usage_delta", "avg_instructions"] $ \key ->
        assertEqual "a partial metric cannot inherit one process's value" Nothing (KM.lookup key obj)
  , testCase "paired differences cancel shared process drift" $ do
      old <- requireAggregate "pair-id" (map (result . (\mean -> sample mean 999 1000)) [10, 100, 30, 200]) >>= requirePerfData
      new <- requireAggregate "pair-id" (map (result . (\mean -> sample mean 999 1000)) [12, 102, 32, 202])
      obj <- requirePerfData new
      assertEqual "four independent pairs" (Just 4) (perfPairedCount old obj)
      assertEqual "common drift cancels in paired differences" (Just (2, 2)) (perfMeanInterval "wall_duration" old obj)
      case perfJson (Just old) new of
        Just (Aeson.Object report) -> forM_ ["measurements", "baseline"] $ \key -> do
          measurement <- requireObject key report
          forM_ ["comparison_id", "process_samples"] $ \field ->
            assertBool "recorded JSON preserves pairing" (KM.member field measurement)
        _ -> assertFailure "expected paired JSON"
  , testCase "aggregate counter status explains incomplete process coverage" $ do
      let available = sample 10 0 10 `KM.union` KM.insert "avg_cycles" (Aeson.Number 50) (counterSample 100 2)
      info <- requireObject "counter_info" available
      let unavailable = foldr KM.delete (KM.insert "counter_info"
            (Aeson.Object (KM.insert "status" (Aeson.String "permission denied") info)) available)
            ["avg_instructions", "avg_cycles", "avg_ipc"]
      combined <- requireAggregate "comparison" [result available, result unavailable]
      obj <- requirePerfData combined
      counters <- requireObject "counter_info" obj
      assertEqual "the first process cannot advertise complete hardware coverage"
        (Just (Aeson.String "incomplete process counters")) (KM.lookup "status" counters)
      forM_ ["avg_instructions", "avg_cycles", "avg_ipc"] $ \key ->
        assertEqual "partial hardware means remain absent" Nothing (KM.lookup key obj)
      assertBool "the table explains why hardware measurements are absent"
        ("hardware counters unavailable: incomplete process counters" `isInfixOf` unlines (renderPerf 119 False Nothing combined))
  , testCase "unrelated aggregate recordings do not become paired samples" $ do
      old <- requireAggregate "old-id" (map (result . (\mean -> sample mean 0 10000)) [10, 100, 30, 200]) >>= requirePerfData
      new <- requireAggregate "new-id" (map (result . (\mean -> sample mean 0 10000)) [12, 102, 32, 202]) >>= requirePerfData
      assertEqual "matching process counts do not establish pairing" Nothing (perfPairedCount old new)
      case perfMeanInterval "wall_duration" old new of
        Just (lo, hi) -> assertBool "historical uncertainty uses process spread" (lo < 0 && hi > 0)
        Nothing -> assertFailure "expected an unpaired process interval"
      assertEqual "malformed sample counts cannot establish pairs" Nothing
        (perfPairedCount old (KM.insert "comparison_id" (Aeson.String "old-id") (KM.insert "num_iterations" (Aeson.Number 3) new)))
  , testCase "paired tables show both means and explain weak evidence" $ do
      old <- requireAggregate "comparison" [result (sample 10 0 10000)] >>= requirePerfData
      new <- requireAggregate "comparison" [result (sample 12 0 10000)]
      obj <- requirePerfData new
      assertEqual "inner iterations cannot replace process pairs" Nothing (perfMeanInterval "wall_duration" old obj)
      forM_ [39, 79, 119, 160] $ \cols -> forM_ [False, True] $ \color -> do
        let rows = renderPerf cols color (Just old) new
            text = unlines rows
        assertBool text (all ((<= cols) . termVisibleLength) rows)
        assertBool "uncertainty needs multiple pairs" (not (any (`isInfixOf` text) ["⚡", "💩"]))
        assertBool text ("inconclusive" `isInfixOf` text)
        when (cols >= 79) $ do
          assertBool text (all (`isInfixOf` head rows) ["baseline", "current", "delta"])
          assertBool text ("1 process pair;" `isInfixOf` text)
          let wall = maybe "" id (find ("wall time" `isInfixOf`) rows)
          assertBool wall (all (`isInfixOf` wall) ["10.0", "12.0", "+20.0%"])
      noisyOld <- requireAggregate "noisy" (map (result . (\mean -> sample mean 0 10)) [10, 100, 30, 200]) >>= requirePerfData
      noisyNew <- requireAggregate "noisy" (map (result . (\mean -> sample mean 0 10)) [11, 103, 27, 205])
      assertBool "overlapping paired intervals are explicitly inconclusive"
        ("inconclusive; interval includes no change" `isInfixOf` unlines (renderPerf 119 False (Just noisyOld) noisyNew))
  , testCase "timing and allocation changes use the recorded measurement" $ do
      let old = identified $ KM.fromList [("avg_wall_duration", Aeson.Number 10), ("mem_usage_delta_avg", Aeson.Number 100)]
          new = identified $ KM.fromList [("avg_wall_duration", Aeson.Number 12), ("mem_usage_delta_avg", Aeson.Number 50)]
          rendered = unlines (renderPerf 79 False (Just old) (result new))
      assertBool rendered ("+20.0%" `isInfixOf` rendered)
      assertBool rendered ("-50.0%" `isInfixOf` rendered)
  , testCase "mean interval accounts for unequal variance and sample counts" $ do
      let old = sample 8 4 5
          new = sample 10 1 20
          half = 2.777 * sqrt 3.25
      case perfMeanInterval "wall_duration" old new of
        Just (lo, hi) -> do
          assertBool (show (lo, hi)) (abs (lo - (2 - half)) < 0.000001)
          assertBool (show (lo, hi)) (abs (hi - (2 + half)) < 0.000001)
        Nothing -> assertFailure "expected a Welch interval"
  , testCase "mean interval handles missing and constant samples" $ do
      assertEqual "old baselines do not invent variance" Nothing (perfMeanInterval "wall_duration" KM.empty (sample 1 1 10))
      assertEqual "one sample cannot estimate variance" Nothing (perfMeanInterval "wall_duration" (sample 1 1 10) (sample 1 0 1))
      assertEqual "negative variance is invalid" Nothing (perfMeanInterval "wall_duration" (sample 1 (-1) 10) (sample 1 1 10))
      assertEqual "constant samples have zero estimated uncertainty" (Just (2, 2))
        (perfMeanInterval "wall_duration" (sample 1 0 10) (sample 3 0 10))
      assertEqual "zero baseline still supports an absolute interval" (Just (1, 1))
        (perfMeanInterval "wall_duration" (sample 0 0 10) (sample 1 0 10))
  , testCase "uncertain mean changes are not colored as regressions" $ do
      let old = sample 10 10 5
          res = (result (sample 11 10 5)) { trNumIterations = 5 }
          rendered = unlines (renderPerf 79 True (Just old) res)
      assertBool rendered ("wall mean delta (approx. 95% CI):" `isInfixOf` rendered)
      assertBool rendered (not ("\ESC[91m" `isInfixOf` rendered))
  , testCase "each mean comparison uses its own variance" $ do
      let old = sample 10 0 10 `KM.union` KM.fromList
            [("mem_usage_delta_avg", Aeson.Number 1000), ("stdev_mem_usage_delta", Aeson.Number 1000)]
          new = sample 12 0 10 `KM.union` KM.fromList
            [("mem_usage_delta_avg", Aeson.Number 1100), ("stdev_mem_usage_delta", Aeson.Number 1000)]
          rows = renderPerf 120 True (Just old) ((result new) { trNumIterations = 10 })
      assertBool (unlines rows) (maybe False (isInfixOf "\ESC[91m+20.0%") (find ("wall time" `isInfixOf`) rows))
      assertBool (unlines rows) (maybe False (isInfixOf "\ESC[2m+10.0%") (find ("allocated" `isInfixOf`) rows))
      case perfMeanInterval "mem_usage_delta" old new of
        Just (lo, hi) -> assertBool (show (lo, hi)) (lo < 0 && hi > 0 && abs (lo + hi - 200) < 0.000001)
        Nothing -> assertFailure "expected an allocation mean interval"
  , testCase "performance table headlines natural wall time and omits non-GC memory" $ do
      let old = KM.insert "peak_rss" (Aeson.Number 24000000) (sample 10 0 10)
          rows = renderPerf 120 False (Just old) tableResult
          timing = find ("wall time" `isInfixOf`) rows
      assertEqual "wall distribution and mean comparison"
        (Just ["wall", "time", "12.0ms", "±", "1.00ms", "10.0ms", "…", "14.0ms", "1", "(10%)", "+20.0%", "💩"])
        (words <$> timing)
      assertBool (unlines rows) (not (any (\label -> any (label `isInfixOf`) rows) ["time excl. GC", "non-GC change"]))
      forM_ [False, True] $ \color ->
        assertEqual "process peak RSS has no baseline delta or comparison color"
          (Just ("  process peak RSS: 12.0" ++ if color then "\ESC[2mMB\ESC[0m" else "MB"))
          (find ("process peak RSS:" `isInfixOf`) (renderPerf 120 color (Just old) tableResult))
      assertBool "no ANSI escapes when color is disabled" (all (not . isInfixOf "\ESC[") rows)
      assertBool "the colored header keeps its full label"
        ("measurement" `isInfixOf` head (renderPerf 79 True (Just old) tableResult))
  , testCase "each measurement row shows its own distribution" $ do
      let rows = renderPerf 120 False Nothing fullTableResult
      forM_
        [ ("wall time", ["wall", "time", "12.0ms", "±", "2.00ms", "8.00ms", "…", "16.0ms", "0", "(0%)"])
        , ("GC time", ["GC", "time", "0.00ms", "±", "0.00ms", "0.00ms", "…", "0.00ms", "0", "(0%)"])
        , ("allocated", ["allocated", "1.54KB", "±", "128B", "1.02KB", "…", "2.05KB", "2", "(20%)"])
        ] $ \(label, expected) ->
          assertEqual label (Just expected) (words <$> find (label `isInfixOf`) rows)
  , testCase "JSON retains distributions in current and baseline measurements" $ do
      case trRaw fullTableResult of
        Aeson.Object obj -> case perfJson (Just obj) fullTableResult of
          Just (Aeson.Object report) -> forM_ ["measurements", "baseline"] $ \key ->
            let measured = KM.filterWithKey (\name _ -> not ("non_gc_mem_usage_delta" `isInfixOf` AesonKey.toString name)) obj
            in assertEqual (show key ++ " omits the unmeasured legacy non-GC estimate") (Just (Aeson.Object measured)) (KM.lookup key report)
          _ -> assertFailure "expected a performance report"
        _ -> assertFailure "expected measurements"
  , testCase "CPU rows scale counts and keep IPC comparisons neutral" $ do
      let old = counterSample 1000000 1
          new = counterSample 2000000 2
          rows = renderPerf 119 True (Just old) ((result new) { trNumIterations = 10 })
          instructions = maybe "" id (find ("instructions" `isInfixOf`) rows)
          ipc = maybe "" id (find ("IPC" `isInfixOf`) rows)
      assertBool instructions ("2.00" `isInfixOf` instructions && "M" `isInfixOf` instructions)
      assertBool instructions (not ("MB" `isInfixOf` instructions))
      assertBool instructions ("💩" `isInfixOf` instructions)
      assertBool ipc ("+100.0%" `isInfixOf` ipc && not (any (`isInfixOf` ipc) ["⚡", "💩", "ratio", "\ESC[91m"]))
      forM_ [39, 79, 119, 160] $ \cols ->
        assertBool (show cols) (all ((<= cols) . termVisibleLength) (renderPerf cols True (Just old) (result new)))
      case perfJson (Just old) (result new) of
        Just (Aeson.Object report) -> do
          assertEqual "measurements retain counters and metadata" (Just (Aeson.Object new)) (KM.lookup "measurements" report)
          assertEqual "baseline retains counters and metadata" (Just (Aeson.Object old)) (KM.lookup "baseline" report)
        _ -> assertFailure "expected a counter report"
  , testCase "different counter scope or machine cannot produce a performance delta" $ do
      let old = counterSample 1000000 1
          new = counterSample 2000000 2
          alter key value = KM.insert "counter_info" (case KM.lookup "counter_info" old of
            Just (Aeson.Object info) -> Aeson.Object (KM.insert key (Aeson.String value) info)
            _ -> Aeson.Null) old
      assertBool "matching counters can compare" (perfComparable "instructions" old new)
      assertBool "hardware permissions do not change CPU-time accounting"
        (perfComparable "cpu_user" (alter "scope" "process:user") new)
      forM_ [alter "scope" "process:user", alter "version" "2", KM.delete "counter_info" old] $ \baseline -> do
        assertEqual "no confidence interval for incomparable counters" Nothing (perfMeanInterval "instructions" baseline new)
        let rows = renderPerf 119 False (Just baseline) (result new)
            line = maybe "" id (find ("instructions" `isInfixOf`) rows)
        assertBool line (not ("+100.0%" `isInfixOf` line))
        assertBool line ("2.00M" `isInfixOf` line)
        assertBool "hardware scope does not invalidate wall time" (perfComparable "wall_duration" baseline new)
  , testCase "small spreads and range endpoints keep their precision" $ do
      let obj = sample 1 0.001 10 `KM.union` KM.fromList [("min_wall_duration", Aeson.toJSON (0.001 :: Double)), ("max_wall_duration", Aeson.Number 1)]
          rendered = unlines (renderPerf 79 False Nothing (result obj))
      assertBool rendered ("1.00ms ± 1.00µs" `isInfixOf` unwords (words rendered))
      assertBool rendered ("1.00µs … 1.00ms" `isInfixOf` unwords (words rendered))
  , testCase "performance table resizes without changing rows or hiding the mean delta" $ do
      let old = Just (sample 10 1 10)
          full = renderPerf 79 False old tableResult
          narrow = renderPerf 39 False old tableResult
      assertEqual "stable row count" (length full) (length narrow)
      assertBool (unlines narrow) (not ("outliers" `isInfixOf` head narrow))
      assertBool (unlines narrow) ("delta" `isInfixOf` head narrow)
      assertBool (unlines narrow) (any (\line -> "12.0ms" `isInfixOf` line && "+20.0%" `isInfixOf` line) narrow)
      assertBool (unlines narrow) (not ("σ" `isInfixOf` head narrow))
      forM_ [tableResult, fullTableResult] $ \res ->
        forM_ [1, 20, 39, 79, 100, 119, 160] $ \cols -> do
          let plain = renderPerf cols False old res
              colored = renderPerf cols True old res
          assertBool (show cols ++ ": " ++ unlines colored) (all ((<= cols) . termVisibleLength) colored)
          assertEqual "color does not change column alignment" (map termVisibleLength plain) (map termVisibleLength colored)
  , testCase "columns and separators stay aligned across benchmark units" $ do
      let old = Just (sample 10 1 10)
          tiny = (result (sample 0.000001 0.000000001 10 `KM.union` KM.fromList
            [("min_wall_duration", Aeson.toJSON (0.000000001 :: Double)), ("max_wall_duration", Aeson.Number 1)])) { trNumIterations = 10 }
          huge = (result (sample 1e300 1e200 10 `KM.union` KM.fromList
            [ ("min_wall_duration", Aeson.toJSON (-1e300 :: Double)), ("max_wall_duration", Aeson.toJSON (1e300 :: Double))
            , ("outlier_count_wall_duration", Aeson.Number 2000000000)
            ])) { trNumIterations = 2000000000 }
          timing rows = maybe "" id (find ("wall time" `isInfixOf`) rows)
      forM_ [79, 100, 119, 160] $ \cols -> do
        let reference = renderPerf cols False old fullTableResult
            separators s = [elemIndices '±' s, elemIndices '…' s]
        forM_ [tiny, huge, tableResult, fullTableResult] $ \res ->
          forM_ [Nothing, old] $ \baseline -> do
            let rows = renderPerf cols False baseline res
            assertEqual "baseline availability does not shift the header"
              (head (renderPerf cols False Nothing fullTableResult))
              (take (length (head (renderPerf cols False Nothing fullTableResult))) (head rows))
            assertEqual "separators do not move when units or magnitudes change"
              (separators (timing reference)) (separators (timing rows))
            assertBool (unlines rows) (all ((<= cols) . termVisibleLength) rows)
      assertBool "default report leaves room between columns"
        (termVisibleLength (head (renderPerf maxBound False old fullTableResult)) >= 110)
  , testCase "supported mean changes have aligned icons without ANSI color" $ do
      let old = sample 10 0 10
          rowsFor mean sd color = renderPerf 119 color (Just old)
            ((result (sample mean sd 10)) { trNumIterations = 10 })
          timing rows = maybe "" id (find ("wall time" `isInfixOf`) rows)
          better = timing (rowsFor 8 0 False)
          worse = timing (rowsFor 12 0 False)
          uncertain = timing (rowsFor 11 10 False)
      assertBool better ("-20.0% ⚡" `isSuffixOf` better)
      assertBool worse ("+20.0% 💩" `isSuffixOf` worse)
      assertBool uncertain (not (any (`isInfixOf` uncertain) ["⚡", "💩"]))
      assertEqual "icons share one slot" (elemIndices '⚡' better) (elemIndices '💩' worse)
      assertEqual "neutral comparisons keep the icon space" (termVisibleLength better) (termVisibleLength uncertain)
      assertEqual "color keeps the same width" (termVisibleLength better) (termVisibleLength (timing (rowsFor 8 0 True)))
  , testCase "terminal fitting counts complete two-cell icons" $ do
      forM_ ["⚡", "💩"] $ \icon -> do
        assertEqual "two terminal cells" 2 (termVisibleLength icon)
        assertEqual "do not draw half an icon" "" (termFitAnsiRight 1 icon)
        assertEqual "a complete icon fits" icon (termFitAnsiRight 2 (icon ++ "x"))
        assertEqual "one remaining cell cannot fit an icon" "x" (termFitAnsiRight 2 ("x" ++ icon))
        assertEqual "icons count toward wrapping" 2 (termRenderedRows 3 (icon ++ icon))
        let colored = "\ESC[92m" ++ icon ++ "x\ESC[0m"
        assertEqual "ANSI does not count" 3 (termVisibleLength colored)
        assertEqual "colored clipping preserves display width" 2 (termVisibleLength (termFitAnsiRight 2 colored))
      assertEqual "combining marks stay with the final character" "e\x0301" (termFitAnsiRight 1 "e\x0301x")
  , testCase "older measurements do not invent a spread or range" $ do
      let rows = renderPerf 120 False Nothing (result (KM.singleton "avg_wall_duration" (Aeson.Number 1)))
      assertEqual "only the known mean is shown" (Just ["wall", "time", "1.00ms", "—", "—"])
        (words <$> find ("wall time" `isInfixOf`) rows)
  , testCase "missing and zero baselines have defined output" $ do
      let render :: Maybe Aeson.Object -> Double -> String
          render old value = unlines $ renderPerf 79 False old
            (result (identified (KM.singleton "avg_wall_duration" (Aeson.toJSON value))))
      assertBool "no baseline has no delta" (not ("%" `isInfixOf` render Nothing 2))
      assertBool "missing metric has no delta" (not ("%" `isInfixOf` render (Just KM.empty) 2))
      assertBool "zero to zero is unchanged"
        ("+0.0%" `isInfixOf` render (Just (identified (KM.singleton "avg_wall_duration" (Aeson.Number 0)))) 0)
      assertBool "zero to nonzero has no percentage"
        ("from 0" `isInfixOf` render (Just (identified (KM.singleton "avg_wall_duration" (Aeson.Number 0)))) 2)
  , testCase "failed and incomplete runs have no performance comparison" $ do
      let res = result (KM.singleton "avg_wall_duration" (Aeson.Number 10))
      forM_ [res { trComplete = False }, res { trSuccess = Just False }, res { trSkipped = True },
             res { trException = Just "error" }, res { trNumIterations = 0 }, res { trSnapshotUpdated = True }] $ \invalid ->
        assertEqual "no performance lines" [] (renderPerf 79 False Nothing invalid)
  , testCase "record requires performance mode" $ do
      acton <- canonicalizePath "../../dist/bin/acton"
      forM_ [[], ["stress"], ["list"]] $ \mode -> do
        (code, _, err) <- readCreateProcessWithExitCode (proc acton (["test"] ++ mode ++ ["--record"])) ""
        assertBool err (code /= ExitSuccess && "--record requires acton test perf" `isInfixOf` err)
  ]

-- Real measurements need a quiet machine; enable them with make test-performance.
perfIntegrationTests :: TestTree
perfIntegrationTests =
    testCase "recording runs fresh tests and preserves unselected measurements" $
      withSystemTempDirectory "acton-perf-record" $ \proj -> do
        acton <- canonicalizePath "../../dist/bin/acton"
        let name = "perf_record"
            fp = Fingerprint.formatFingerprint
              (Fingerprint.updateFingerprintPrefix (Fingerprint.fingerprintPrefixForName name) 1)
            baseline = proj </> "perf_data"
            run args = readCreateProcessWithExitCode
              (proc acton (["test", "perf", "--color", "never"] ++
                [arg | "--time" `notElem` args, arg <- ["--time", "100ms"]] ++ args)) { cwd = Just proj } ""
            runOK args = do
              (code, out, err) <- run args
              assertEqual (unwords args ++ "\n" ++ out ++ err) ExitSuccess code
              return out
            readBaseline = do
              decoded <- Aeson.eitherDecodeFileStrict baseline
              case decoded of
                Left err -> assertFailure err >> return M.empty
                Right saved -> return (saved :: M.Map String (M.Map String Aeson.Value))
            setMean (Aeson.Object obj) = Aeson.Object (KM.insert "avg_wall_duration" (Aeson.Number 1000000000) obj)
            setMean raw = raw
            hasMachine info = case KM.lookup "machine" info of
              Just (Aeson.String value) -> value /= mempty
              _ -> False
        createDirectoryIfMissing True (proj </> "src")
        writeFile (proj </> "Build.act") $ unlines ["name = " ++ show name, "fingerprint = " ++ fp]
        writeFile (proj </> "src/sample.act") $ unlines
          [ "import testing"
          , "import acton.rts"
          , "import time"
          , ""
          , "actor _test_first(t: testing.AsyncT):"
          , "    t.success()"
          , ""
          , "actor _test_second(t: testing.AsyncT):"
          , "    t.success()"
          , ""
          , "def _test_snapshot() -> str:"
          , "    return \"snapshot value\""
          , ""
          , "actor _test_failed(t: testing.AsyncT):"
          , "    t.failure(ValueError(\"expected failure\"))"
          , ""
          , "actor _test_skipped(t: testing.AsyncT):"
          , "    t.skip(\"expected skip\")"
          , ""
          , "actor _test_slow(t: testing.AsyncT):"
          , "    after 3.5: t.success()"
          , ""
          , "actor _test_peak_rss(t: testing.EnvT):"
          , "    peak = acton.rts.get_peak_rss(t.env.syscap)"
          , "    assert peak is not None and peak < 64 * 1048576"
          , "    t.success()"
          , ""
          , "actor _test_slow_setup(t: testing.EnvT):"
          , "    acton.rts.sleep(t.env.syscap, 0.2)"
          , "    for scale in t.loop():"
          , "        assert False, \"No body should run after setup exhausts the budget\""
          , "    t.success()"
          , ""
          , "actor _test_looped(t: testing.EnvT):"
          , "    setup_scale = t.scale()"
          , "    expected_scale = t.env.getenv(\"ACTON_PERF_EXPECT_SCALE\")"
          , "    for scale in t.loop():"
          , "        assert scale == setup_scale"
          , "        if expected_scale is not None:"
          , "            assert scale == int(expected_scale)"
          , "    t.success()"
          , ""
          , "actor LoopWorker(t: testing.AsyncT):"
          , "    setup_scale = t.scale()"
          , "    for scale in t.loop():"
          , "        assert scale == setup_scale"
          , "    t.success()"
          , ""
          , "def _test_calibration_setup(t: testing.SyncT):"
          , "    data = list(range(t.scale()))"
          , "    start = time.monotonic().unix_ns()"
          , "    while time.monotonic().unix_ns() - start < t.scale() * 1000000:"
          , "        pass"
          , "    for scale in t.loop():"
          , "        assert scale == len(data)"
          , ""
          , "def _test_allocation_setup(t: testing.SyncT):"
          , "    data = \"x\" * (t.scale() * 1048576)"
          , "    for scale in t.loop():"
          , "        assert len(data) == scale * 1048576"
          , ""
          , "actor _test_helper_workload(t: testing.AsyncT):"
          , "    LoopWorker(t)"
          , ""
          , "actor _test_complete_before_loop(t: testing.AsyncT):"
          , "    iterator = t.loop()"
          , "    t.report_result(True, None, None)"
          , "    try:"
          , "        list(iterator)"
          , "    except ValueError:"
          , "        pass"
          , ""
          , "actor _test_late_loop(t: testing.AsyncT):"
          , "    t.success()"
          , "    try:"
          , "        t.loop()"
          , "    except ValueError:"
          , "        pass"
          , "    t.success()"
          , ""
          , "def _test_break_loop(t: testing.SyncT):"
          , "    for scale in t.loop():"
          , "        break"
          , ""
          , "def _test_return_loop(t: testing.SyncT):"
          , "    for scale in t.loop():"
          , "        return"
          , ""
          , "def _test_body_stop(t: testing.SyncT):"
          , "    for scale in t.loop():"
          , "        raise StopIteration(\"body stopped\")"
          , ""
          , "def _test_repeat_loop(t: testing.SyncT):"
          , "    t.loop()"
          , "    for scale in t.loop():"
          , "        pass"
          , ""
          , "def _test_loop_failure(t: testing.SyncT):"
          , "    for scale in t.loop():"
          , "        raise ValueError(\"original body failure\")"
          , ""
          , "actor _test_inconsistent_loop(t: testing.EnvT):"
          , "    if t.env.getenv(\"ACTON_PERF_TEST_LOOP_SEEN\") is None:"
          , "        t.env.setenv(\"ACTON_PERF_TEST_LOOP_SEEN\", \"1\")"
          , "        for scale in t.loop():"
          , "            assert scale > 0"
          , "    t.success()"
          , ""
          , "actor _test_timed_body(t: testing.EnvT):"
          , "    acton.rts.sleep(t.env.syscap, 0.02)"
          , "    for scale in t.loop():"
          , "        start = time.monotonic().unix_ns()"
          , "        while time.monotonic().unix_ns() - start < 1000000:"
          , "            pass"
          , "    acton.rts.sleep(t.env.syscap, 0.02)"
          , "    t.success()"
          , ""
          , "actor _test_counter_activation(t: testing.EnvT):"
          , "    assert t.scale() == 1"
          , "    assert t.env.getenv(\"ACTON_TEST_PERF\") is None"
          , "    info = acton.rts.perf_info(t.env.syscap)"
          , "    if \"perf\" in t.env.argv:"
          , "        assert info[\"status\"] != \"not enabled at process start\""
          , "    else:"
          , "        assert info[\"status\"] == \"not enabled at process start\""
          , "        assert acton.rts.perf_snapshot(t.env.syscap).instructions is None"
          , "    t.success()"
          ]
        _ <- runOK ["--name", "counter_activation"]
        -- The child must report its own peak, even after exec from a large parent.
        allocaBytes (128 * 1048576) $ \memory -> do
          fillBytes memory 1 (128 * 1048576)
          (code, out, err) <- readCreateProcessWithExitCode
            (proc (proj </> "out/bin/.test_sample")
              ["--rts-wthreads", "1", "test", "_test_peak_rss_wrapper",
               "--max-iter", "1", "--min-iter", "1", "--max-time", "1000000", "--min-time", "1"])
              { cwd = Just proj } ""
          assertEqual (out ++ err) ExitSuccess code
          let results = [info | line <- lines err,
                               Right event <- [Aeson.eitherDecode (BL8.pack line)],
                               Just (Aeson.Object info) <- [KM.lookup "test_info" event],
                               KM.lookup "complete" info == Just (Aeson.Bool True)]
          case results of
            [info] -> assertEqual (out ++ err) (Just (Aeson.Bool True)) (KM.lookup "success" info)
            _ -> assertFailure ("Expected a completed peak RSS probe\n" ++ out ++ err)
        environment <- getEnvironment
        let runLoop budget expected args = do
              (code, out, err) <- readCreateProcessWithExitCode
                (proc acton (["test", "perf", "--time", budget, "--name", "looped", "--json"] ++ args))
                  { cwd = Just proj, env = Just (("ACTON_PERF_EXPECT_SCALE", expected) : filter ((/= "ACTON_PERF_EXPECT_SCALE") . fst) environment) } ""
              assertEqual (out ++ err) ExitSuccess code
              singleJsonTest out >>= requireObject "performance"
        (ordinaryCode, ordinaryOut, ordinaryErr) <- readCreateProcessWithExitCode
          (proc acton ["test", "--iter", "1", "--name", "counter_activation", "--no-cache"])
            { cwd = Just proj, env = Just (("ACTON_TEST_PERF", "1") : filter ((/= "ACTON_TEST_PERF") . fst) environment) } ""
        assertEqual (ordinaryOut ++ ordinaryErr) ExitSuccess ordinaryCode
        _ <- runOK ["--record", "--name", "first|second"]
        saved0 <- readBaseline
        -- The old writer included null entries when a test process crashed.
        let saved = M.insert "missing" (M.singleton "_test_crashed" Aeson.Null) saved0
            tests = M.findWithDefault M.empty "perf_record.sample" saved
        BL.writeFile baseline (Aeson.encode saved)
        assertEqual "two measured tests" 2 (M.size tests)
        forM_ tests $ \raw -> case raw of
          Aeson.Object obj -> do
            samples <- requireNumber "num_iterations" obj
            assertBool "ordinary workloads retain complete measured invocations" (samples >= 1)
            info <- requireObject "perf_info" obj
            assertEqual "measurement version is recorded" (Just (Aeson.String "3")) (KM.lookup "version" info)
            assertEqual "the total requested budget is recorded" (Just (Aeson.Number 100)) (KM.lookup "time_budget_ms" info)
            assertEqual "ordinary workloads keep scale one" (Just (Aeson.Number 1)) (KM.lookup "scale" info)
            assertEqual "ordinary workloads measure complete invocations" (Just (Aeson.Bool False)) (KM.lookup "loop" info)
            assertEqual "GC remains natural" (Just (Aeson.String "natural")) (KM.lookup "gc" info)
            warmup <- requireNumber "warmup_duration_ms" info
            assertBool "warmup completed before measurement" (warmup > 0)
            build <- requireObject "build" info
            assertEqual "effective release mode is recorded" (Just (Aeson.String "ReleaseFast")) (KM.lookup "optimize" build)
            forM_ distributionKeys $ \key -> assertBool (show key ++ " missing from recording") (KM.member key obj)
            case KM.lookup "counter_info" obj of
              Just (Aeson.Object info) -> do
                assertBool "performance counters activated before runtime startup"
                  (KM.lookup "status" info /= Just (Aeson.String "not enabled at process start"))
                when (KM.lookup "status" info == Just (Aeson.String "available")) $
                  forM_ ["avg_instructions", "avg_cycles", "avg_ipc"] $ \key -> case KM.lookup key obj of
                    Just (Aeson.Number n) -> assertBool (show key) (n > 0)
                    _ -> assertFailure (show key ++ " missing from available hardware counters")
              _ -> assertFailure "missing counter metadata"
            case KM.lookup "peak_rss" obj of
              Nothing -> return ()
              Just (Aeson.Number rss) -> assertBool "peak RSS is positive when available" (rss > 0)
              Just _ -> assertFailure "peak RSS must be a number"
          _ -> assertFailure "expected recorded measurements"
        firstInfo <- case M.lookup "_test_first_wrapper" tests of
          Just (Aeson.Object obj) -> requireObject "perf_info" obj
          _ -> assertFailure "missing first recording" >> fail "missing recording"
        let canCompare = hasMachine firstInfo
            assertComparison text = do
              assertBool text ("mean ± σ" `isInfixOf` unwords (words text))
              let signedPercentage token = case dropWhile (== '(') token of
                    sign : rest -> sign `elem` ['+', '-'] && '%' `elem` rest
                    _ -> False
                  hasDelta = any signedPercentage (words text) || "from 0" `isInfixOf` text
              if canCompare
                then assertBool text hasDelta
                else do
                  assertBool text ("comparison unavailable: machine identity is unavailable" `isInfixOf` text)
                  assertBool "unknown identity cannot show a delta or confidence interval"
                    (not (hasDelta || any (`isInfixOf` text) ["wall mean delta", "⚡", "💩"]))
        bytes <- BL.readFile baseline
        out <- runOK ["--name", "first"]
        assertComparison out
        assertBool out ("allocated" `isInfixOf` out)
        assertEqual "comparison leaves the baseline intact" bytes =<< BL.readFile baseline
        ttyOut <- runOK ["--tty", "--name", "first"]
        assertComparison ttyOut
        assertEqual "terminal comparison leaves the baseline intact" bytes =<< BL.readFile baseline
        json <- runOK ["--json", "--name", "first"]
        assertBool json ("\"cached\":false" `isInfixOf` json)
        forM_ ["performance", "median_wall_duration", "stdev_wall_duration", "outlier_count_wall_duration", "avg_wall_duration", "avg_gc_duration", "mean_difference_ci95_ms", "counter_info", "avg_cpu_user", "avg_cpu_system", "loop_iterations"] $ \key ->
          assertBool (key ++ " missing from " ++ json) (("\"" ++ key ++ "\"") `isInfixOf` json)
        forM_ distributionKeys $ \key -> assertBool (show key ++ " missing from " ++ json) (show key `isInfixOf` json)
        performance <- singleJsonTest json >>= requireObject "performance"
        assertEqual "JSON reports whether the machine can be compared"
          (Just (if canCompare then Aeson.Null else Aeson.String "machine identity is unavailable"))
          (KM.lookup "comparison_unavailable_reason" performance)
        unless canCompare $
          assertEqual "unknown identity has no confidence interval" (Just Aeson.Null) (KM.lookup "mean_difference_ci95_ms" performance)
        -- Set a deterministic reference and check that --record compares with
        -- the old value before replacing only the selected measurement.
        let reference = M.adjust (M.adjust setMean "_test_first_wrapper")
              "perf_record.sample" saved
        BL.writeFile baseline (Aeson.encode reference)
        updatedOut <- runOK ["--record", "--name", "first"]
        assertComparison updatedOut
        when canCompare $ assertBool updatedOut ("-100.0%" `isInfixOf` updatedOut)
        updated <- readBaseline
        let updatedTests = M.findWithDefault M.empty "perf_record.sample" updated
        assertEqual "unselected measurement survives recording"
          (M.lookup "_test_second_wrapper" tests) (M.lookup "_test_second_wrapper" updatedTests)
        assertEqual "recording cached source must not empty the baseline" 2 (M.size updatedTests)
        assertEqual "unavailable old measurements survive recording"
          (M.lookup "missing" saved) (M.lookup "missing" updated)
        updatedBytes <- BL.readFile baseline
        (failedCode, failedOut, _) <- run ["--record", "--name", "failed", "--json", "--scale", "50000"]
        assertBool "failing test exits unsuccessfully" (failedCode /= ExitSuccess)
        failed <- singleJsonTest failedOut
        assertEqual "an initial failure aborts before any measured sample" (Just (Aeson.Number 0)) (KM.lookup "iterations" failed)
        assertEqual "failed preparation produces no performance measurement" (Just Aeson.Null) (KM.lookup "performance" failed)
        assertBool "explicit scale preserves the original failure" ("expected failure" `isInfixOf` failedOut && not ("Explicit --scale" `isInfixOf` failedOut))
        skippedOut <- runOK ["--record", "--name", "skipped", "--json", "--scale", "50000"]
        skipped <- singleJsonTest skippedOut
        assertEqual "explicit scale preserves skips" (Just (Aeson.Bool True)) (KM.lookup "skipped" skipped)
        assertBool skippedOut ("expected skip" `isInfixOf` skippedOut && not ("Explicit --scale" `isInfixOf` skippedOut))
        forM_ [("first", []), ("snapshot", ["--accept"])] $ \(test, args) -> do
          (noLoopCode, noLoopOut, noLoopErr) <- run (["--record", "--name", test, "--json", "--scale", "50000"] ++ args)
          assertBool (noLoopOut ++ noLoopErr) (noLoopCode /= ExitSuccess)
          noLoop <- singleJsonTest noLoopOut
          assertEqual "scale rejection stays failed after snapshot acceptance" (Just (Aeson.String "FAIL")) (KM.lookup "status" noLoop)
          assertEqual "explicit scale on a whole invocation has no performance report" (Just Aeson.Null) (KM.lookup "performance" noLoop)
          assertBool noLoopOut ("Explicit --scale requires a test that uses t.loop()" `isInfixOf` noLoopOut)
        _ <- runOK ["--record", "--name", "absent"]
        assertEqual "failed, skipped, incompatible and empty selections preserve the baseline" updatedBytes =<< BL.readFile baseline
        let invalidate (Aeson.Object obj) = Aeson.Object (KM.insert "success" (Aeson.Bool False) obj)
            invalidate raw = raw
            failedReference = M.adjust (M.adjust invalidate "_test_first_wrapper") "perf_record.sample" updated
        BL.writeFile baseline (Aeson.encode failedReference)
        invalidOut <- runOK ["--name", "first"]
        assertBool invalidOut ("mean ± σ" `isInfixOf` unwords (words invalidOut) && not ("delta" `isInfixOf` invalidOut))
        BL.writeFile baseline "{broken"
        (badCode, _, badErr) <- run ["--record", "--name", "first"]
        assertBool badErr (badCode /= ExitSuccess && "Cannot read performance baseline" `isInfixOf` badErr)
        assertEqual "malformed baseline is not overwritten" "{broken" =<< BL.readFile baseline
        removeFile baseline
        -- A complete invocation may exceed the requested budget. It must be
        -- allowed to finish beyond the former three-second watchdog, but a
        -- run with no time left for measurement is not a successful benchmark.
        (slowCode, slowOut, slowErr) <- run ["--name", "slow", "--json"]
        assertBool (slowOut ++ slowErr) (slowCode /= ExitSuccess)
        slow <- singleJsonTest slowOut
        assertEqual "exhausting preparation leaves no measured samples" (Just (Aeson.Number 0)) (KM.lookup "iterations" slow)
        assertEqual "exhausted preparation produces no measurement" (Just Aeson.Null) (KM.lookup "performance" slow)
        duration <- requireNumber "duration_ms" slow
        assertBool "the slow invocation completed past the former watchdog" (duration >= 3000)
        assertBool slowOut (not ("TimeoutError" `isInfixOf` slowOut))
        (setupCode, setupOut, setupErr) <- run ["--name", "slow_setup", "--json"]
        assertBool (setupOut ++ setupErr) (setupCode /= ExitSuccess)
        exhausted <- singleJsonTest setupOut
        assertEqual "setup can exhaust calibration before its first body" (Just Aeson.Null) (KM.lookup "performance" exhausted)
        assertBool setupOut (not ("No body should run" `isInfixOf` setupOut))
        looped <- runLoop "100ms" "50000" ["--record", "--scale", "50000", "--tag", "beta, alpha", "--tag", "alpha"] >>= requireObject "measurements"
        loopedInfo <- requireObject "perf_info" looped
        assertEqual "author opted into loop measurement" (Just (Aeson.Bool True)) (KM.lookup "loop" loopedInfo)
        assertEqual "explicit scale reaches every yield without a baseline" (Just (Aeson.Number 50000)) (KM.lookup "scale" loopedInfo)
        assertEqual "explicit scale skips calibration" (Just (Aeson.toJSON ([] :: [Aeson.Value]))) (KM.lookup "calibration" loopedInfo)
        assertEqual "explicit scale retains the requested time budget" (Just (Aeson.Number 100)) (KM.lookup "time_budget_ms" loopedInfo)
        warmup <- requireNumber "warmup_duration_ms" loopedInfo
        assertBool "explicit scale retains warmup" (warmup > 0)
        samples <- requireNumber "num_iterations" looped
        bodies <- requireNumber "loop_iterations" looped
        assertBool "loop samples are complete invocation averages" (samples >= 1 && samples <= 4)
        assertBool "loop bodies are counted separately from statistical samples" (bodies >= samples)
        assertEqual "tags match actual capability parsing" (Just (Aeson.toJSON (["alpha", "beta"] :: [String]))) (KM.lookup "tags" loopedInfo)
        helperOut <- runOK ["--name", "helper_workload", "--json"]
        helperInfo <- singleJsonTest helperOut >>= requireObject "performance" >>= requireObject "measurements" >>= requireObject "perf_info"
        assertEqual "a helper actor can exhaust the loop before reporting completion" (Just (Aeson.Bool True)) (KM.lookup "loop" helperInfo)
        case KM.lookup "calibration" helperInfo of
          Just (Aeson.Array observations) -> assertBool "automatic scale records calibration observations" (not (null observations))
          _ -> assertFailure "missing calibration observations"
        setupOut <- runOK ["--name", "calibration_setup", "--time", "500ms", "--json"]
        setupInfo <- singleJsonTest setupOut >>= requireObject "performance" >>= requireObject "measurements" >>= requireObject "perf_info"
        assertBool "calibration rebuilds scale-sized setup at increasing sizes" . (> 1) =<< requireNumber "scale" setupInfo
        assertBool "calibrated setup retains warmup" . (> 0) =<< requireNumber "warmup_duration_ms" setupInfo
        allocationOut <- runOK ["--name", "allocation_setup", "--scale", "8", "--time", "500ms", "--json"]
        allocation <- singleJsonTest allocationOut >>= requireObject "performance" >>= requireObject "measurements"
        assertBool "8MiB of setup allocation is excluded from the measured body" . (< 4096) =<< requireNumber "mem_usage_delta_avg" allocation
        lateOut <- runOK ["--name", "late_loop", "--json"]
        lateInfo <- singleJsonTest lateOut >>= requireObject "performance" >>= requireObject "measurements" >>= requireObject "perf_info"
        assertEqual "late loop requests and duplicate completions cannot change measurement identity"
          (Just (Aeson.Bool False)) (KM.lookup "loop" lateInfo)
        forM_
          [ ("complete_before_loop", "Benchmark loop must run to exhaustion")
          , ("break_loop", "Benchmark loop must run to exhaustion")
          , ("return_loop", "Benchmark loop must run to exhaustion")
          , ("body_stop", "Benchmark loop must run to exhaustion")
          , ("repeat_loop", "Use t.loop() once per test invocation")
          , ("loop_failure", "original body failure")
          , ("inconsistent_loop", "Call t.loop() consistently in every invocation")
          ] $ \(test, message) -> do
            (code, out, err) <- run ["--name", test, "--json"]
            assertBool (test ++ "\n" ++ out ++ err) (code /= ExitSuccess)
            invalid <- singleJsonTest out
            assertEqual (test ++ " fails before measurement") (Just (Aeson.Number 0)) (KM.lookup "iterations" invalid)
            assertEqual (test ++ " cannot produce a performance report") (Just Aeson.Null) (KM.lookup "performance" invalid)
            assertBool out (message `isInfixOf` out)
            when (test == "loop_failure") $
              assertBool "loop validation preserves the original failure"
                (not ("Benchmark loop must run to exhaustion" `isInfixOf` out))
        (timedCode, timedOut, timedErr) <- readCreateProcessWithExitCode
          (proc acton ["test", "perf", "--time", "1s", "--scale", "1", "--name", "timed_body", "--json"])
            { cwd = Just proj } ""
        assertEqual (timedOut ++ timedErr) ExitSuccess timedCode
        timed <- singleJsonTest timedOut >>= requireObject "performance" >>= requireObject "measurements"
        timedInfo <- requireObject "perf_info" timed
        mean <- requireNumber "avg_wall_duration" timed
        assertBool "body time is normalized per loop iteration" (mean >= 0.5 && mean < 15)
        work <- requireNumber "measurement_ms" timedInfo
        elapsed <- requireNumber "measurement_duration_ms" timedInfo
        runs <- requireNumber "num_iterations" timed
        assertBool ("setup and teardown are outside measured body time: " ++ show (elapsed, work, runs)) (elapsed - work >= 35 * runs)
        -- Machines without a usable identity intentionally cannot reuse a
        -- recording. The pure tests above cover that contract independently.
        when (hasMachine loopedInfo) $ do
          fixed <- BL.readFile baseline
          same <- runLoop "200ms" "50000" ["--scale", "50000", "--tag", "alpha,beta"]
          assertEqual "the same explicit scale remains comparable across budgets" (Just Aeson.Null) (KM.lookup "comparison_unavailable_reason" same)
          sameInfo <- requireObject "measurements" same >>= requireObject "perf_info"
          assertEqual "the new budget is retained as provenance" (Just (Aeson.Number 200)) (KM.lookup "time_budget_ms" sameInfo)
          different <- runLoop "200ms" "25000" ["--scale", "25000", "--tag", "alpha,beta"]
          assertEqual "explicit scale overrides the recorded scale" (Just (Aeson.String "workload scale differs")) (KM.lookup "comparison_unavailable_reason" different)
          assertEqual "different workloads have no confidence interval" (Just Aeson.Null) (KM.lookup "mean_difference_ci95_ms" different)
          differentInfo <- requireObject "measurements" different >>= requireObject "perf_info"
          assertEqual "the requested workload is measured" (Just (Aeson.Number 25000)) (KM.lookup "scale" differentInfo)
          assertEqual "comparison leaves the baseline intact" fixed =<< BL.readFile baseline
          _ <- runLoop "200ms" "25000" ["--record", "--scale", "25000", "--tag", "alpha,beta"]
          replaced <- BL.readFile baseline
          assertBool "recording replaces the workload scale" (replaced /= fixed)
          reused <- runLoop "100ms" "25000" ["--tag", "alpha,beta"]
          assertEqual "the replacement baseline is comparable by default" (Just Aeson.Null) (KM.lookup "comparison_unavailable_reason" reused)
          reusedInfo <- requireObject "measurements" reused >>= requireObject "perf_info"
          assertEqual "recorded scale reaches every loop body" (Just (Aeson.Number 25000)) (KM.lookup "scale" reusedInfo)
          assertEqual "reused scale skips calibration" (Just (Aeson.toJSON ([] :: [Aeson.Value]))) (KM.lookup "calibration" reusedInfo)
          assertEqual "default reuse leaves the new baseline intact" replaced =<< BL.readFile baseline
          _ <- runOK ["--record", "--name", "first|second"]
          localBefore <- readBaseline
          let external = proj </> "before.perf_data"
          copyFile baseline external
          externalBytes <- BL.readFile external
          BL.length externalBytes `seq` return ()
          fileOut <- runOK ["--compare", external, "--record", "--name", "first", "--json"]
          filePerf <- singleJsonTest fileOut >>= requireObject "performance"
          assertEqual "an explicit file supplies the comparison" (Just Aeson.Null) (KM.lookup "comparison_unavailable_reason" filePerf)
          assertEqual "record updates never overwrite the compared file" externalBytes =<< BL.readFile external
          localAfter <- readBaseline
          let withoutFirst = M.adjust (M.delete "_test_first_wrapper") "perf_record.sample"
          assertEqual "record preserves every unselected local entry" (withoutFirst localBefore) (withoutFirst localAfter)
          let git args = do
                (code, out, err) <- readCreateProcessWithExitCode (proc "git" args) { cwd = Just proj } ""
                assertEqual (unwords args ++ "\n" ++ out ++ err) ExitSuccess code
                return out
              processSamples obj = case KM.lookup "process_samples" obj >>= AesonTypes.parseMaybe Aeson.parseJSON of
                Just samples -> return (samples :: [Aeson.Object])
                Nothing -> assertFailure "missing ordered process measurements" >> fail "missing process samples"
          _ <- git ["init", "-q"]
          _ <- git ["add", "Build.act", "src"]
          _ <- git ["-c", "user.name=Acton tests", "-c", "user.email=tests@example.invalid",
                    "commit", "--no-gpg-sign", "-qm", "Add performance fixture"]
          appendFile (proj </> "Build.act") "\n# Staged local change\n"
          _ <- git ["add", "Build.act"]
          appendFile (proj </> "src/sample.act") "\n# Unstaged local change\n"
          sourceBefore <- readFile (proj </> "src/sample.act")
          length sourceBefore `seq` return ()
          indexBefore <- git ["ls-files", "--stage"]
          statusBefore <- git ["status", "--porcelain", "--untracked-files=no"]
          worktreesBefore <- git ["worktree", "list", "--porcelain"]
          forM_ [("first", False, 1, []), ("looped", True, 25000, ["--scale", "25000"])] $ \(test, loop, scale, args) -> do
            out <- runOK (["--compare", "git:HEAD", "--name", test, "--time", "1s", "--json"] ++ args)
            comparison <- singleJsonTest out >>= requireObject "performance"
            current <- requireObject "measurements" comparison
            old <- requireObject "baseline" comparison
            currentSamples <- processSamples current
            oldSamples <- processSamples old
            assertBool "live comparisons produce complete process pairs" (not (null currentSamples) && length currentSamples <= 4)
            assertEqual "both sides have one observation per pair" (length currentSamples) (length oldSamples)
            assertEqual "only this comparison's processes are paired" (KM.lookup "comparison_id" old) (KM.lookup "comparison_id" current)
            sequences <- forM (zip oldSamples currentSamples) $ \(a, b) ->
              (,) <$> requireNumber "sequence" a <*> requireNumber "sequence" b
            let order = [a < b | (a, b) <- sequences]
            assertBool "execution follows a randomized balanced schedule, possibly stopped early"
              (order `elem` map (take (length order)) perfPairOrders)
            assertBool "accepted pairs remain in chronological order"
              (and [max a b < min c d | ((a, b), (c, d)) <- zip sequences (drop 1 sequences)])
            forM_ (zip oldSamples currentSamples) $ \(a, b) -> do
              forM_ [a, b] $ \sample -> do
                info <- requireObject "perf_info" sample
                assertEqual "both versions use the same workload scale" (Just (Aeson.Number scale)) (KM.lookup "scale" info)
                assertEqual "whole-invocation and loop scopes are preserved" (Just (Aeson.Bool loop)) (KM.lookup "loop" info)
                assertBool "every measured process has excluded warmup" . (> 0) =<< requireNumber "warmup_duration_ms" info
            oldSource <- requireObject "source" old
            currentSource <- requireObject "source" current
            assertEqual "baseline records the committed revision" (Just (Aeson.Bool False)) (KM.lookup "git_dirty" oldSource)
            assertEqual "current includes local modifications" (Just (Aeson.Bool True)) (KM.lookup "git_dirty" currentSource)
            assertEqual "Git comparison leaves the source intact" sourceBefore =<< readFile (proj </> "src/sample.act")
            assertEqual "Git comparison preserves staged changes" indexBefore =<< git ["ls-files", "--stage"]
            assertEqual "Git comparison preserves tracked file status" statusBefore =<< git ["status", "--porcelain", "--untracked-files=no"]
            assertEqual "the baseline worktree is removed after comparison" worktreesBefore =<< git ["worktree", "list", "--porcelain"]

parseOptions :: [String] -> O.ParserResult C.CmdLineOptions
parseOptions = O.execParserPure C.cmdLinePrefs (O.info (C.cmdLineParser O.<**> O.helper) mempty)

parsePerfOptions :: [String] -> IO C.TestOptions
parsePerfOptions args = case parseOptions (["test", "perf"] ++ args) of
    O.Success (C.CmdOpt _ (C.Test (C.TestPerf opts))) -> return opts
    O.Failure failure -> assertFailure (fst (O.renderFailure failure "acton")) >> fail "invalid options"
    _ -> assertFailure "expected performance options" >> fail "invalid command"

singleJsonTest :: String -> IO Aeson.Object
singleJsonTest out = case Aeson.eitherDecode (BL8.pack out) >>= AesonTypes.parseEither parser of
    Right obj -> return obj
    Left err -> assertFailure (err ++ "\n" ++ out) >> fail "invalid test report"
  where
    parser = Aeson.withObject "test report" $ \report -> do
      tests <- report Aeson..: "tests"
      case tests of
        [Aeson.Object obj] -> return obj
        _ -> fail "expected one test"

requireObject :: Aeson.Key -> Aeson.Object -> IO Aeson.Object
requireObject key obj = case KM.lookup key obj of
    Just (Aeson.Object value) -> return value
    _ -> assertFailure ("missing object " ++ show key ++ " in " ++ show obj) >> fail "missing object"

requireNumber :: Aeson.Key -> Aeson.Object -> IO Double
requireNumber key obj = case KM.lookup key obj of
    Just (Aeson.Number value) -> return (realToFrac value)
    _ -> assertFailure ("missing number " ++ show key ++ " in " ++ show obj) >> fail "missing number"

requireAggregate :: String -> [TestResult] -> IO TestResult
requireAggregate comparison runs = case aggregatePerfRuns comparison runs of
    Just res -> return res
    Nothing -> assertFailure "expected complete comparable process measurements" >> fail "aggregation failed"

requirePerfData :: TestResult -> IO Aeson.Object
requirePerfData res = case testPerfData res of
    Just obj -> return obj
    Nothing -> assertFailure "expected performance measurements" >> fail "missing performance data"

identity :: Aeson.Object
identity = KM.fromList
  [ ("version", Aeson.String "3")
  , ("machine", Aeson.String "same machine")
  , ("scale", Aeson.Number 7)
  , ("loop", Aeson.Bool True)
  , ("workers", Aeson.Number 4)
  , ("gc", Aeson.String "natural")
  , ("tags", Aeson.toJSON ([] :: [String]))
  , ("build", Aeson.object
      [ "optimize" Aeson..= ("ReleaseFast" :: String), "target" Aeson..= ("aarch64-macos-none" :: String)
      , "cpu" Aeson..= ("" :: String), "no_threads" Aeson..= False
      , "db" Aeson..= False, "no_dbp" Aeson..= False
      ])
  ]

identified :: Aeson.Object -> Aeson.Object
identified = KM.insert "perf_info" (Aeson.Object identity)

changeIdentity :: Aeson.Key -> Aeson.Value -> Aeson.Object -> Aeson.Object
changeIdentity key value obj = KM.insert "perf_info" (case KM.lookup "perf_info" obj of
    Just (Aeson.Object info) -> Aeson.Object (KM.insert key value info)
    _ -> Aeson.Null) obj

result :: Aeson.Object -> TestResult
result obj = TestResult
  { trModule = "sample"
  , trName = "_test_sample"
  , trComplete = True
  , trSuccess = Just True
  , trSkipped = False
  , trSkipReason = Nothing
  , trException = Nothing
  , trOutput = Nothing
  , trStdOut = Nothing
  , trStdErr = Nothing
  , trFlaky = False
  , trNumSkipped = 0
  , trNumFailures = 0
  , trNumErrors = 0
  , trNumIterations = case KM.lookup "num_iterations" obj of
      Just (Aeson.Number n) -> floor n
      _ -> 3
  , trTestDuration = 100
  , trRaw = Aeson.Object obj
  , trSnapshotUpdated = False
  , trCached = False
  }

sample :: Double -> Double -> Int -> Aeson.Object
sample mean sd n = identified $ KM.fromList
  [ ("avg_wall_duration", Aeson.toJSON mean)
  , ("stdev_wall_duration", Aeson.toJSON sd)
  , ("num_iterations", Aeson.toJSON n)
  ]

tableResult :: TestResult
tableResult = (result (sample 12 1 10 `KM.union` KM.fromList
  [ ("min_wall_duration", Aeson.Number 10)
  , ("median_wall_duration", Aeson.Number 12)
  , ("max_wall_duration", Aeson.Number 14)
  , ("outlier_count_wall_duration", Aeson.Number 1)
  , ("avg_wall_duration", Aeson.Number 12)
  , ("avg_gc_duration", Aeson.Number 0)
  , ("mem_usage_delta_avg", Aeson.Number 1536)
  , ("non_gc_mem_usage_delta_avg", Aeson.Number (-2048))
  , ("peak_rss", Aeson.Number 12000000)
  ])) { trNumIterations = 10 }

fullTableResult :: TestResult
fullTableResult = tableResult { trRaw = case trRaw tableResult of
    Aeson.Object obj -> Aeson.Object (KM.fromList
      [ ("stdev_wall_duration", Aeson.Number 2)
      , ("min_wall_duration", Aeson.Number 8)
      , ("max_wall_duration", Aeson.Number 16)
      , ("outlier_count_wall_duration", Aeson.Number 0)
      , ("stdev_gc_duration", Aeson.Number 0)
      , ("min_gc_duration", Aeson.Number 0)
      , ("max_gc_duration", Aeson.Number 0)
      , ("outlier_count_gc_duration", Aeson.Number 0)
      , ("stdev_mem_usage_delta", Aeson.Number 128)
      , ("min_mem_usage_delta", Aeson.Number 1024)
      , ("max_mem_usage_delta", Aeson.Number 2048)
      , ("median_mem_usage_delta", Aeson.Number 1536)
      , ("q1_mem_usage_delta", Aeson.Number 1408)
      , ("q3_mem_usage_delta", Aeson.Number 1664)
      , ("outlier_count_mem_usage_delta", Aeson.Number 2)
      , ("stdev_non_gc_mem_usage_delta", Aeson.Number 1024)
      , ("min_non_gc_mem_usage_delta", Aeson.Number (-4096))
      , ("max_non_gc_mem_usage_delta", Aeson.Number 0)
      , ("outlier_count_non_gc_mem_usage_delta", Aeson.Number 3)
      ] `KM.union` obj)
    raw -> raw
  }

distributionKeys :: [Aeson.Key]
distributionKeys =
  [ stat <> "_" <> metric
  | metric <- ["wall_duration", "gc_duration", "mem_usage_delta", "cpu_user", "cpu_system"]
  , stat <- ["min", "max", "median", "q1", "q3", "stdev", "outlier_count"]
  ]

counterSample :: Double -> Double -> Aeson.Object
counterSample instructions ipc = identified $ KM.fromList
  [ ("avg_instructions", Aeson.toJSON instructions)
  , ("stdev_instructions", Aeson.Number 0)
  , ("avg_ipc", Aeson.toJSON ipc)
  , ("stdev_ipc", Aeson.Number 0)
  , ("num_iterations", Aeson.Number 10)
  , ("counter_info", Aeson.object
      [ "version" Aeson..= ("1" :: String), "backend" Aeson..= ("perf_event_open" :: String)
      , "scope" Aeson..= ("process:user+kernel" :: String), "os" Aeson..= ("Linux" :: String)
      , "release" Aeson..= ("6.1" :: String), "arch" Aeson..= ("x86_64" :: String)
      , "cpu" Aeson..= ("test CPU" :: String), "status" Aeson..= ("available" :: String)
      ])
  ]

renderPerf :: Int -> Bool -> Maybe Aeson.Object -> TestResult -> [String]
renderPerf cols useColor baseline res = map ($ cols) (formatTestPerfLines useColor baseline res)
