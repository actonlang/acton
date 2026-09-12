{-# LANGUAGE OverloadedStrings #-}
module PerfScalingTests (scaleOptionTests, scaleTests, perfMemoryTests, scaleIntegrationTests) where

import qualified Acton.CommandLineParser as C
import qualified Acton.Fingerprint as Fingerprint
import Acton.Testing (TestResult(..))
import Codec.Compression.Zlib (decompress)
import Control.Exception (throwIO)
import Control.Monad (forM_, when)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base64 as Base64
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.IORef
import Data.Char (isHexDigit)
import Data.List (find, foldl', isInfixOf, isPrefixOf)
import Data.Word (Word8)
import Data.Maybe (isJust)
import qualified Options.Applicative as O
import TestPerf (perfInfo)
import System.Clock (Clock(Monotonic), fromNanoSecs, getTime)
import System.Directory
import System.Exit
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import System.Process
import System.Random (mkStdGen, randoms)
import Test.Tasty
import Test.Tasty.HUnit
import PerfScaling

-- Command line options --------------------------------------------------------

scaleOptionTests :: TestTree
scaleOptionTests = testGroup "performance scaling options"
  [ testCase "scale captures explicit resource limits" $ do
      (_, opts) <- parseScale ["--start-scale", "1000", "--end-scale", "100000", "--max-memory", "50%", "--max-time", "2h"]
      assertEqual "starting scale" (Just 1000) (C.testStartScale opts)
      assertEqual "inclusive endpoint" (Just 100000) (C.testEndScale opts)
      assertEqual "memory percentage" (Just (C.MemoryPercent 50)) (C.testMaxMemory opts)
      assertEqual "duration is stored in milliseconds" 7200000 (C.testMaxTime opts)
      assertBool "the total limit is explicit" (C.testMaxTimeSet opts)
  , testCase "scale needs no flags and defaults to release builds" $ do
      (_, opts) <- parseScale []
      assertEqual "release by default" C.ReleaseFast (C.optimize (C.testCompile opts))
      assertEqual "default starting scale is chosen by the runner" Nothing (C.testStartScale opts)
      assertEqual "no endpoint means automatic exploration" Nothing (C.testEndScale opts)
      assertEqual "default memory limit is chosen by the runner" Nothing (C.testMaxMemory opts)
      assertBool "default total limit is chosen by the runner" (not (C.testMaxTimeSet opts))
      (_, explicit) <- parseScale ["--optimize", "Debug"]
      assertEqual "explicit debug remains available" C.Debug (C.optimize (C.testCompile explicit))
  , testCase "memory limits preserve exact byte units and finite percentages" $ do
      forM_ [("1", C.MemoryBytes 1), ("256B", C.MemoryBytes 256),
             ("2MiB", C.MemoryBytes (2 * 1024^2)), ("3gib", C.MemoryBytes (3 * 1024^3)),
             ("1TB", C.MemoryBytes (1000^4)), ("1TiB", C.MemoryBytes (1024^4)),
             ("0.5%", C.MemoryPercent 0.5), ("100%", C.MemoryPercent 100)] $ \(value, expected) ->
        assertEqual value (Just expected) . C.testMaxMemory . snd =<< parseScale ["--max-memory", value]
      forM_ ["0", "-1MiB", "1.5GiB", "1XB", "0%", "-1%", "100.1%", "NaN%", "1e1000%"] $ \value ->
        rejects ["test", "scale", "--max-memory", value]
  , testCase "study durations are finite and starting scales are positive" $ do
      forM_ [("250ms", 250), ("1.5s", 1500), ("2m", 120000), ("0.5H", 1800000)] $ \(value, expected) ->
        assertEqual value expected . C.testMaxTime . snd =<< parseScale ["--max-time", value]
      forM_ ["0ms", "-1s", "0.1ms", "10", "1d", "NaNs", "1e100h"] $ \value ->
        rejects ["test", "scale", "--max-time", value]
      forM_ [1, maxBound :: Int] $ \value ->
        assertEqual "positive starting scale" (Just value) . C.testStartScale . snd =<< parseScale ["--start-scale", show value]
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
        (_, opts) <- parseScale args
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
  where
    rejects :: [String] -> Assertion
    rejects args = case parseOptions args of
        O.Failure _ -> return ()
        _ -> assertFailure ("must reject " ++ unwords args)

-- Study controller ------------------------------------------------------------

scaleTests :: TestTree
scaleTests = testGroup "performance scaling studies"
  [ testCase "means and estimated errors use all measured samples" $ do
      assertEqual "empty samples have no mean" Nothing (scaleMean [])
      closeTo (13 / 3) (scaleMean [9, 1, 3])
      closeTo 4.5 (scaleMean [9, 2, 6, 1])
      closeTo (1 / (sqrt 3 * 11)) (scaleError (point 1 [10, 11, 12] 100))
      assertEqual "fewer than three samples cannot establish precision" Nothing
        (scaleError (point 1 [10, 10] 100))
      closeTo 0 (scaleError (point 1 [10, 10, 10] 100))
  , testCase "extra samples improve estimated mean precision up to seven" $ do
      let retries = [8, 12, 10, 10, 10, 10, 10]
          at n = point 1 (take n retries) 100
      forM_ [0, 3, 5] $ \n -> assertBool ("needs more at " ++ show n) (scaleNeedsSamples (at n))
      assertBool "seven samples resolve this variation" (scaleReliable (at 7))
      assertBool "the resolved point does not retry" (not (scaleNeedsSamples (at 7)))
      assertBool "five can suffice" (scaleReliable (point 1 [10, 12, 11, 11, 11] 100))
      let noisy = point 1 [1, 10, 19, 10, 10, 10, 10] 100
          tiny = point 1 [0.001, 0.001, 0.001] 100
      assertBool "seven does not imply reliability" (not (scaleReliable noisy))
      assertBool "noise cannot cause unbounded retries" (not (scaleNeedsSamples noisy))
      assertBool "tiny results advance instead of retrying" (not (scaleNeedsSamples tiny))
      assertBool "zero spread cannot rescue a tiny result" (not (scaleReliable tiny))
  , testCase "growth consistently uses means for observed work" $ do
      let first = point 10 [9.9, 10, 10.1] 100
      closeTo 1 (scaleGrowth first (point 20 [19.9, 20, 20.1] 100))
      closeTo 2 (scaleGrowth first (point 20 [39.9, 40, 40.1] 100))
      closeTo (log (61 / 30) / log 2)
        (scaleGrowth (point 10 [10, 10, 10] 100) (point 20 [20, 20, 21] 100))
      assertEqual "scale must increase" Nothing (scaleGrowth first first)
      assertEqual "scales indistinguishable as doubles have no growth estimate" Nothing
        (scaleGrowth (point (maxBound - 1) [10, 10, 10] 100) (point maxBound [20, 20, 20] 100))
  , testCase "growth suppresses noisy tiny and undersampled points" $ do
      let reliable = point 10 [10, 10, 10] 100
      forM_ [point 20 [10, 20, 30] 100, point 20 [0.01, 0.02, 0.03] 100,
             point 20 [20, 20] 100, point 20 [] 100] $ \unreliable -> do
        assertEqual "new point must be reliable" Nothing (scaleGrowth reliable unreliable)
        assertEqual "old point must be reliable" Nothing (scaleGrowth (unreliable { pointScale = 5 }) reliable)
  , testCase "stable growth needs broad coverage and five consecutive slopes" $ do
      let linear = curve fromIntegral [0..10]
      assertEqual "512-fold coverage is still too narrow" Nothing (stableGrowth (tail linear))
      assertTrend 32 1 (stableGrowth linear)
      assertEqual "old coverage cannot replace the five-slope confirmation window" Nothing
        (stableGrowth (take 4 linear ++ [point 1 [1, 1, 1] 100]))
      assertEqual "small steps near the memory limit do not confirm growth" Nothing
        (stableGrowth [point n (replicate 3 (fromIntegral n)) 100 | n <- [1600, 1500, 1400, 1300, 1200, 1100, 1]])
      let tinyStart = curve (\n -> if n < 8 then 0.001 else fromIntegral n / 8) [0..13]
      assertEqual "tiny early probes do not count toward reliable coverage" Nothing
        (stableGrowth (tail tinyStart))
      assertTrend 256 1 (stableGrowth tinyStart)
  , testCase "bends and noisy points reset the stable window" $ do
      let bent n = if n <= 256 then fromIntegral n else fromIntegral n ^ (2 :: Int) / 256
      assertEqual "a recent bend must remain visible" Nothing (stableGrowth (curve bent [0..12]))
      assertTrend 256 2 (stableGrowth (curve bent [0..13]))
      let recent = curve fromIntegral [0..10]
          noisy = point 256 [128, 256, 384] 100
      assertEqual "one noisy point breaks otherwise consistent recent growth" Nothing
        (stableGrowth (take 2 recent ++ [noisy] ++ drop 3 recent))
      let times = scanl (\t exponent -> t * 2 ** exponent) 32 [0.8, 0.9, 1, 1.1, 1.2]
          gradual = reverse (zipWith (\n t -> point n (replicate 3 t) 100) [32, 64, 128, 256, 512, 1024] times)
      assertEqual "small adjacent changes cannot hide a wider total bend" Nothing
        (stableGrowth (gradual ++ [point 1 [1, 1, 1] 100]))
  , testCase "next scale slows near memory limits and cannot overflow" $ do
      assertEqual "no data gives no next scale" Nothing (nextScale 1000 [])
      assertEqual "comfortable headroom doubles scale" (Just 200) (nextScale 1000 [point 100 [1] 100])
      assertEqual "quadratic memory growth reduces the next step" (Just 141)
        (nextScale 1000 [point 100 [1] 400, point 50 [1] 100])
      assertEqual "no useful headroom ends the sweep" Nothing (nextScale 125 [point 100 [1] 100])
      assertEqual "integer boundary ends the sweep" Nothing (nextScale 1000 [point maxBound [1] 100])
      assertEqual "the last step is clamped without wrapping" (Just maxBound)
        (nextScale 1000 [point (maxBound `div` 2 + 1) [1] 100])
  , testCase "memory ceilings retain an available-memory reserve" $ do
      let mib = 1024^2
      assertEqual "percentage uses total memory" (Right (500, 100))
        (scaleMemoryLimit (C.MemoryPercent 50) (MemoryStatus 1000 900 0))
      assertEqual "an explicit byte ceiling is retained" (Right (200, 100))
        (scaleMemoryLimit (C.MemoryBytes 200) (MemoryStatus 1000 900 0))
      assertEqual "available memory caps the child below the requested ceiling" (Right (44 * mib, 256 * mib))
        (scaleMemoryLimit (C.MemoryBytes (200 * mib)) (MemoryStatus (4000 * mib) (300 * mib) 0))
      case scaleMemoryLimit (C.MemoryPercent 50) (MemoryStatus 1000 100 0) of
        Left _ -> return ()
        Right limit -> assertFailure ("a depleted reserve must prevent launch: " ++ show limit)
  , testCase "scaling rejects recording snapshot updates and watch" $ do
      forM_ [["--record"], ["--accept"], ["--watch"], ["--start-scale", "8", "--end-scale", "7"]] $ \args -> do
        (_, opts) <- parseScale args
        assertBool (unwords args) (isJust (validateScalingOptions opts))
      forM_ [[], ["--start-scale", "1000", "--max-memory", "128MiB", "--max-time", "2s"],
             ["--end-scale", "1"], ["--start-scale", "8", "--end-scale", "8"]] $ \args ->
        assertEqual (unwords args) Nothing . validateScalingOptions . snd =<< parseScale args
  , testGroup "adaptive controller"
      [ testCase "linear quadratic logarithmic and constant curves settle over a broad range" $
          forM_ [("linear", fromIntegral, Just 1),
                 ("quadratic", \n -> fromIntegral n ^ (2 :: Int), Just 2),
                 ("nlogn", \n -> fromIntegral n * logBase 2 (fromIntegral n + 1), Nothing),
                 ("constant", const 1, Just 0)] $ \(name, model, exponent) -> do
            (code, events) <- simulate name [] (\n _ -> modelResult n (model n))
            assertEqual name 0 code
            summary <- assertSummary "stable" (1, 1024) (11, 33) events
            assertEqual "growth summarizes the recent six sizes" (Just (Aeson.Number 32)) (KM.lookup "stable_from_scale" summary)
            lo <- field "growth_min" summary
            hi <- field "growth_max" summary
            case exponent of
              Just expected -> closeTo expected (Just lo) >> closeTo expected (Just hi)
              Nothing -> assertBool "n log n is locally between linear and quadratic" (lo > 1 && hi < 1.3)
            forM_ (eventsOf "point" events) $ \p -> do
              n <- field "scale" p
              closeTo (model n) . Just =<< field "wall_mean_ms" p
              assertEqual "constant repeats use three samples" (Just (Aeson.Number 3)) (KM.lookup "samples" p)
              closeTo 0 . Just =<< field "relative_standard_error" p
              assertEqual "point precision is recorded" (Just (Aeson.Bool True)) (KM.lookup "reliable" p)
              assertBool "mean statistics replace the old median field" (not (KM.member "wall_median_ms" p))
            let references = eventsOf "reference" events
                decisions = filter (\e -> KM.lookup "event" e `elem` [Just (Aeson.String "point"), Just (Aeson.String "reference")]) events
            assertEqual "periodic checks plus a final check" 3 (length references)
            assertEqual "the final decision follows a fresh reference" (Just (Aeson.String "reference")) (KM.lookup "event" (last decisions))
            forM_ references $ \r -> assertEqual "the reference settled" (Just (Aeson.Bool True)) (KM.lookup "stable" r)
            assertSampleProvenance events
      , testCase "explicit endpoint retains the curve past automatic stability" $ do
          (code, events) <- simulate "endpoint" ["--end-scale", "100000"] (\n _ -> modelResult n (fromIntegral n))
          assertEqual "the study succeeds" 0 code
          sizes <- mapM (field "scale") (eventsOf "point" events) :: IO [Int]
          assertEqual "smaller sizes remain and the exact target is measured" ([2^n | n <- [0..16]] ++ [100000]) sizes
          summary <- requireEvent "test_end" events
          assertEqual "the requested range is completed" (Just (Aeson.String "range")) (KM.lookup "outcome" summary)
          assertEqual "the study finishes at its endpoint" (Just (Aeson.Number 100000)) (KM.lookup "max_scale" summary)
          closeTo 1 . Just =<< field "growth_min" summary
          ending <- requireEvent "end" events
          assertEqual "target completion is explicit" (Just (Aeson.Bool True)) (KM.lookup "end_scale_reached" ending)
          forM_ (eventsOf "point" events) $ \event -> do
            n <- field "scale" event :: IO Int
            forM_ ["allocated_mean_bytes", "allocated_min_bytes", "allocated_max_bytes"] $ \key ->
              assertEqual "point events retain allocation summaries" (Just (Aeson.toJSON (n * 100))) (KM.lookup key event)
      , testCase "explicit ranges finish exactly even without a stable growth estimate" $
          forM_ [(3, 73, [3,6,12,24,48,73]), (1, 1, [1]), (maxBound, maxBound, [maxBound])] $ \(start, end, expected) -> do
            (code, events) <- simulate "range" ["--start-scale", show start, "--end-scale", show end]
              (\n _ -> modelResult n 1)
            assertEqual "the requested range succeeds" 0 code
            sizes <- mapM (field "scale") (eventsOf "point" events) :: IO [Int]
            assertEqual "endpoints are inclusive without overshoot" expected sizes
            summary <- assertSummary "range" (start, end) (length expected, 3 * length expected) events
            assertEqual "range completion does not assert stable growth" (Just Aeson.Null) (KM.lookup "growth_min" summary)
      , testCase "explicit endpoint postpones noisy timing stops but never resource limits" $ do
          let target = 2^21 + 1 :: Int
          (code, events) <- simulate "tiny_endpoint" ["--end-scale", show target] (\n _ -> modelResult n 0.001)
          assertEqual "the tiny benchmark succeeds" 0 code
          summary <- requireEvent "test_end" events
          assertEqual "twenty short sizes cannot stop before the target" (Just (Aeson.toJSON target)) (KM.lookup "max_scale" summary)
          (limited, stopped) <- simulate "limited_endpoint" ["--end-scale", "100000"] (\n _ ->
            let result = modelResult n 1
                Aeson.Object raw = trRaw result
            in result { trRaw = Aeson.Object (KM.insert "peak_rss" (Aeson.Number 1073741824) raw) })
          assertEqual "memory still limits the run" 0 limited
          assertEnd "memory limit reached" stopped
          ending <- requireEvent "end" stopped
          assertEqual "unreached target is explicit" (Just (Aeson.Bool False)) (KM.lookup "end_scale_reached" ending)
      , testCase "a partial endpoint does not complete the range" $ do
          (code, events) <- simulate "partial_endpoint" ["--end-scale", "17"] $ \n i ->
            let result = modelResult n 1
                Aeson.Object raw = trRaw result
            in if n == 17 && i == 3 then
                 result { trRaw = Aeson.Object (KM.insert "peak_rss" (Aeson.Number 1073741824) raw) }
               else result
          assertEqual "the memory ceiling is a normal stop" 0 code
          assertEnd "memory limit reached" events
          assertEqual "two endpoint samples survive" 2 (length
            [s | s <- eventsOf "sample" events, KM.lookup "scale" s == Just (Aeson.Number 17),
                 KM.lookup "accepted" s == Just (Aeson.Bool True)])
          ending <- requireEvent "end" events
          assertEqual "the endpoint needs a complete sample batch" (Just (Aeson.Bool False)) (KM.lookup "end_scale_reached" ending)
      , testCase "drifting references do not shorten an explicit range" $ do
          (code, events) <- simulate "range_drift" ["--end-scale", "65537"]
            (\n i -> modelResult n (if n == 1 && i > 3 then 2 else fromIntegral n))
          assertEqual "the range succeeds despite drift" 0 code
          assertBool "multiple reference checks detect drift"
            (length [r | r <- eventsOf "reference" events, KM.lookup "stable" r == Just (Aeson.Bool False)] >= 3)
          summary <- requireEvent "test_end" events
          assertEqual "the requested endpoint is measured" (Just (Aeson.Number 65537)) (KM.lookup "max_scale" summary)
          assertEqual "drift cannot produce a stable-growth claim" (Just Aeson.Null) (KM.lookup "growth_min" summary)
      , testCase "precision controls the actual three five and seven sample batches" $ do
          let model n i = fromIntegral n * case n of
                1 -> cycle [0.9, 1.1, 1, 1, 1] !! (i - 1)
                2 -> cycle [0.8, 1.2, 1, 1, 1, 1, 1] !! (i - 1)
                _ -> 1
          (code, events) <- simulate "repeats" [] (\n i -> modelResult n (model n i))
          assertEqual "the additional samples resolve precision" 0 code
          _ <- assertSummary "stable" (1, 1024) (11, 39) events
          forM_ (eventsOf "point" events) $ \p -> do
            n <- field "scale" p :: IO Int
            let expected = if n == 1 then 5 else if n == 2 then 7 else 3
                samples = filter (\e -> KM.lookup "scale" e == Just (Aeson.toJSON n)
                                    && KM.lookup "reference" e == Just (Aeson.Bool False)) (eventsOf "sample" events)
            assertEqual "journaled point count matches the collected batch" (Just (Aeson.toJSON expected)) (KM.lookup "samples" p)
            assertEqual "the controller actually took that many samples" expected (length samples)
            closeTo (fromIntegral n) . Just =<< field "wall_mean_ms" p
          forM_ (eventsOf "reference" events) $ \r ->
            assertEqual "references use the same precision policy" (Just (Aeson.Number 5)) (KM.lookup "samples" r)
      , testCase "a bend extends exploration until the new trend is confirmed" $ do
          let model n = if n <= 256 then fromIntegral n else fromIntegral n ^ (2 :: Int) / 256
          (code, events) <- simulate "bend" [] (\n _ -> modelResult n (model n))
          assertEqual "the later segment settles" 0 code
          summary <- assertSummary "stable" (1, 8192) (14, 42) events
          assertEqual "the reported window starts at the bend" (Just (Aeson.Number 256)) (KM.lookup "stable_from_scale" summary)
          closeTo 2 . Just =<< field "growth_min" summary
          closeTo 2 . Just =<< field "growth_max" summary
      , testCase "tiny probes do not choose the reference or establish coverage" $ do
          let model n = if n < 4 then 0 else if n < 8 then 0.001 else fromIntegral n / 8
          (code, events) <- simulate "tiny_start" [] (\n _ -> modelResult n (model n))
          assertEqual "measurable work eventually settles" 0 code
          _ <- assertSummary "stable" (1, 8192) (14, 42) events
          forM_ (eventsOf "reference" events) $ \r ->
            assertEqual "the first measurable scale is the reference" (Just (Aeson.Number 8)) (KM.lookup "scale" r)
      , testCase "a fresh reference can reject an otherwise stable curve" $ do
          (code, events) <- simulate "drift" [] (\n i -> modelResult n (if n == 1 && i > 9 then 2 else fromIntegral n))
          assertEqual "unsettled drift is an inconclusive study" 0 code
          summary <- assertSummary "inconclusive" (1, 4096) (13, 39) events
          assertEqual "reference failure explains the outcome" (Just (Aeson.String "Reference measurements did not settle")) (KM.lookup "reason" summary)
          assertEqual "an inconclusive result cannot claim a growth range" (Just Aeson.Null) (KM.lookup "growth_min" summary)
          assertEqual "two periodic successes do not bypass three later failures"
            (map (Just . Aeson.Bool) [True, True, False, False, False])
            (map (KM.lookup "stable") (eventsOf "reference" events))
      , testCase "persistently tiny or noisy timing stops without an endless search" $
          forM_ [("zero", \_ -> 0, 3), ("tiny", \_ -> 0.001, 3),
                 ("noisy", \i -> cycle [0.1, 1, 1.9, 1, 1, 1, 1] !! (i - 1), 7)] $ \(name, model, repeats) -> do
            (code, events) <- simulate name [] (\n i -> modelResult n (model i))
            assertEqual name 0 code
            summary <- assertSummary "inconclusive" (1, 2^19) (20, 20 * repeats) events
            assertEqual "unreliable timing explains the outcome"
              (Just (Aeson.String "Timing stayed too short or noisy across 20 successive sizes")) (KM.lookup "reason" summary)
            assertBool "there is no reliable reference to recheck" (null (eventsOf "reference" events))
            forM_ (eventsOf "point" events) $ \p -> do
              assertEqual "unreliable samples remain visible" (Just (Aeson.Bool False)) (KM.lookup "reliable" p)
              assertEqual "no unsupported local growth is reported" (Just Aeson.Null) (KM.lookup "observed_exponent" p)
      , testCase "a result for the wrong scale cannot enter the curve" $ do
          (code, events) <- simulate "wrong_scale" [] (\n _ -> modelResult (n + 1) 1)
          assertEqual "mismatched provenance fails the study" 1 code
          sample <- requireEvent "sample" events
          assertEqual "the rejected result is still journaled" (Just (Aeson.Bool False)) (KM.lookup "accepted" sample)
          assertBool "no point uses a different workload" (null (eventsOf "point" events))
      ]
  , testCase "exited samples leave time for output capture" $
      withCreateProcess (proc "sh" ["-c", "exit 0"]) $ \_ _ _ process -> do
        pid <- fmap (fmap fromIntegral) (getPid process)
        assertBool "the process ID is captured before waiting" (isJust pid)
        _ <- waitForProcess process
        assertEqual "the process handle has been reaped" Nothing =<< getPid process
        now <- getTime Monotonic
        reason <- watchScaleProcess (ScaleLimits (now + fromNanoSecs 40000000) 1 0) pid process
        assertEqual "an exited child must not report a monitoring failure" "time limit reached" reason
  , testCase "unavailable monitoring stops the study unsuccessfully" $
      withSystemTempDirectory "acton-scaling-monitor" $ \directory -> do
        (gopts, opts) <- parseScale ["--max-memory", "1B", "--max-time", "2s"]
        let reason = "memory monitoring unavailable: injected failure"
        code <- runScalingStudy False gopts opts directory KM.empty [("sample", "test", Nothing)] Nothing
          (\_ _ _ _ -> throwIO (ScalingStopped reason))
        assertEqual "an unavailable guard cannot report success" 2 code
        files <- filter ((== ".jsonl") . takeExtension) <$> listDirectory directory
        assertEqual "one journal is retained" 1 (length files)
        events <- BL.readFile (directory </> head files) >>= decodeEvents . BL.unpack
        assertEnd reason events
        assertBool "an unguarded sample cannot contribute a point" (null (eventsOf "point" events))
  , testCase "comparison replays completed recorded sizes without adaptive early stopping" $
      forM_ [([], [1,3..49]), (["--start-scale", "8", "--end-scale", "17"], [9,11..17])] $ \(args, expected) ->
      withSystemTempDirectory "acton-scale-replay" $ \directory -> do
        (gopts, opts) <- parseScale (["--max-memory", "128MiB", "--max-time", "30s", "--json"] ++ args)
        let sizes = [1,3..49]
            points = IM.fromList [(n, (True, [ScaleSample 0.001 Nothing])) | n <- sizes]
            Aeson.Object raw = trRaw (modelResult 1 0.001)
            series = ScaleSeries (IM.insert 50 (False, [ScaleSample 0.001 Nothing]) points) (perfInfo raw) Nothing Nothing
            baseline = ScaleRecording "old.jsonl" KM.empty (M.singleton ("sample", "test") series) Nothing
            implementation = replicate 64 'a'
        calls <- newIORef []
        code <- runScalingStudy False gopts opts directory KM.empty [("sample", "test", Just implementation)] (Just baseline) $ \_ n _ _ -> do
          modifyIORef' calls (++ [n])
          return (modelResult n 0.001)
        assertEqual "all scheduled sizes completed" 0 code
        assertEqual "only completed sizes inside the requested range are replayed, regardless of timing"
          (concatMap (replicate 3) expected) =<< readIORef calls
        [name] <- listDirectory directory
        assertBool name ("sample.test__" `isInfixOf` name && "__aaaaaaaaaaaa.jsonl" `isInfixOf` name)
        recording <- readScaleRecording (directory </> name)
        assertEqual "full implementation identity is retained" (Just (Aeson.toJSON implementation))
          (KM.lookup "implementation_hash" (recordingHeader recording))
        assertEqual "completion describes replay" (Just "Completed baseline sizes remeasured")
          (seriesReason (recordingTests recording M.! ("sample", "test")))
  , testCase "each benchmark owns its journal while sharing the command budget" $
      withSystemTempDirectory "acton-scale-files" $ \directory -> do
        (gopts, opts) <- parseScale ["--max-memory", "128MiB", "--max-time", "30s", "--json"]
        code <- runScalingStudy False gopts opts directory KM.empty
          [("sample", "one", Nothing), ("sample", "two", Nothing)] Nothing $ \_ n _ name ->
            if name == "two" then throwIO (ScalingStopped "time limit reached") else return (modelResult n 1)
        assertEqual "the time ceiling is a normal stop" 0 code
        files <- listDirectory directory
        assertEqual "one journal per benchmark" 2 (length files)
        recordings <- mapM (readScaleRecording . (directory </>)) files
        let one = head [r | r <- recordings, M.member ("sample", "one") (recordingTests r)]
            two = head [r | r <- recordings, M.member ("sample", "two") (recordingTests r)]
        assertEqual "the first file only claims its own completion" (Just "benchmark finished") (recordingStopped one)
        assertEqual "the unfinished benchmark has its own reason" (Just "time limit reached") (recordingStopped two)
        assertEqual "both files identify the common command"
          (KM.lookup "run_id" (recordingHeader one)) (KM.lookup "run_id" (recordingHeader two))
  , scaleReportTests
  ]

-- Live integration ------------------------------------------------------------

-- Real measurements need a quiet machine; enable them with make test-performance.
scaleIntegrationTests :: TestTree
scaleIntegrationTests =
    testCase "scaling journals completed work when later work stops" $
      withSystemTempDirectory "acton-perf-scaling" $ \project -> do
        acton <- canonicalizePath "../../dist/bin/acton"
        let name = "scale_tests"
            fingerprint = Fingerprint.formatFingerprint
              (Fingerprint.updateFingerprintPrefix (Fingerprint.fingerprintPrefixForName name) 1)
            baseline = project </> "perf_data"
            run test memory duration = do
              (code, out, err) <- readCreateProcessWithExitCode
                (proc acton ["test", "scale", "--start-scale", "1000", "--max-time", duration,
                             "--max-memory", memory, "--json", "--name", test]) { cwd = Just project } ""
              assertBool (out ++ err) (not (null out))
              assertBool "JSON output has no terminal escapes" (not ('\ESC' `elem` out))
              events <- decodeEvents out
              study <- requireEvent "study" events
              path <- field "path" study
              assertBool "HTML is no longer generated" . not =<< doesFileExist (replaceExtension path "html")
              saved <- BL.readFile path >>= decodeEvents . BL.unpack
              assertEqual "flushed journal matches the streamed events" events saved
              assertEqual "the fixed baseline remains untouched" "{}\n" =<< readFile baseline
              return (code, events, out ++ err)
        createDirectoryIfMissing True (project </> "src")
        writeFile (project </> "Build.act") $ unlines ["name = " ++ show name, "fingerprint = " ++ fingerprint]
        writeFile baseline "{}\n"
        writeFile (project </> "src/sample.act") $ unlines
          [ "import testing"
          , "import time"
          , ""
          , "def _test_partial(t: testing.SyncT):"
          , "    for scale in t.loop():"
          , "        assert scale >= 1000"
          , "        target = 0.001 if scale == 1000 else 10.0"
          , "        sw = time.Stopwatch()"
          , "        while sw.elapsed().to_float() < target:"
          , "            pass"
          , ""
          , "def _test__test_partial(t: testing.SyncT):"
          , "    raise ValueError(\"wrong benchmark selected\")"
          , ""
          , "def _test_failure(t: testing.SyncT):"
          , "    for scale in t.loop():"
          , "        raise ValueError(\"scaling failure\")"
          , ""
          , "def _test_without_loop(t: testing.SyncT):"
          , "    pass"
          , ""
          , "def _test_slow(t: testing.SyncT):"
          , "    for scale in t.loop():"
          , "        sw = time.Stopwatch()"
          , "        while sw.elapsed().to_float() < 10.0:"
          , "            pass"
          , ""
          , "def _test_stdout(t: testing.SyncT):"
          , "    for scale in t.loop():"
          , "        print(\"x\" * 2097152, end=\"\", flush=True)"
          , ""
          , "def _test_stderr(t: testing.SyncT):"
          , "    for scale in t.loop():"
          , "        print(\"x\" * 2097152, end=\"\", err=True, flush=True)"
          ]
        (partialCode, partial, partialOutput) <- run "partial" "128MiB" "2s"
        assertEqual partialOutput ExitSuccess partialCode
        assertEnd "time limit reached" partial
        started <- requireEvent "sample_start" partial
        assertEqual "the first requested scale is journaled before execution" (Just (Aeson.Number 1000)) (KM.lookup "scale" started)
        let samples = eventsOf "sample" partial
        assertBool "completed samples survive the later timeout" (not (null samples))
        forM_ samples $ \sample -> do
          assertEqual "only validated samples are accepted" (Just (Aeson.Bool True)) (KM.lookup "accepted" sample)
          raw <- field "result" sample
          info <- field "perf_info" raw
          assertEqual "only the completed starting scale contributes samples" (Just (Aeson.Number 1000)) (KM.lookup "scale" sample)
          assertEqual "fixed scale reaches the measured invocation" (KM.lookup "scale" sample) (KM.lookup "scale" info)
          assertEqual "one measured invocation per child" (Just (Aeson.Number 1)) (KM.lookup "num_iterations" raw)
          assertEqual "one measured loop body per child" (Just (Aeson.Number 1)) (KM.lookup "loop_iterations" raw)
          assertEqual "scaling sample provenance" (Just (Aeson.Bool True)) (KM.lookup "scaling" info)
          assertEqual "fixed-scale study samples do not calibrate" (Just (Aeson.toJSON ([] :: [Int]))) (KM.lookup "calibration" info)
          warmup <- field "warmup_duration_ms" info :: IO Double
          assertBool "the child completed warmup" (warmup > 0)
        originalHeader <- requireEvent "study" partial
        originalPath <- field "path" originalHeader
        implementation <- field "implementation_hash" originalHeader :: IO String
        assertBool "a real benchmark has a full implementation digest"
          (length implementation == 64 && all isHexDigit implementation)
        assertBool "the filename includes the benchmark and short implementation digest"
          (("scale_tests.sample._test_partial__" `isInfixOf` takeFileName originalPath)
            && ("__" ++ take 12 implementation ++ ".jsonl") `isInfixOf` takeFileName originalPath)
        original <- BS.readFile originalPath
        (compareCode, compareOut, compareErr) <- readCreateProcessWithExitCode
          (proc acton ["test", "scale", "--compare", originalPath, "--end-scale", "1000", "--max-time", "2s", "--json"])
            { cwd = Just project } ""
        assertEqual (compareOut ++ compareErr) ExitSuccess compareCode
        compared <- decodeEvents compareOut
        compareHeader <- requireEvent "study" compared
        comparePath <- field "path" compareHeader
        assertBool "comparison writes a distinct journal" (originalPath /= comparePath)
        assertEqual "sampling options do not change implementation identity"
          (KM.lookup "implementation_hash" originalHeader) (KM.lookup "implementation_hash" compareHeader)
        forM_ (eventsOf "sample_start" compared) $ \event -> do
          assertEqual "recorded scale is replayed without calibration" (Just (Aeson.Number 1000)) (KM.lookup "scale" event)
          assertEqual "the recorded raw identity excludes a colliding display name"
            (Just (Aeson.String "_test_partial")) (KM.lookup "test" event)
        summary <- requireEvent "test_end" compared
        assertEqual "the recorded schedule completed" (Just (Aeson.String "compared")) (KM.lookup "outcome" summary)
        assertEqual "the old recording is unchanged" original =<< BS.readFile originalPath
        (unreachedCode, _, unreachedErr) <- readCreateProcessWithExitCode
          (proc acton ["test", "scale", "--compare", originalPath, "--end-scale", "1001", "--json"])
            { cwd = Just project } ""
        assertBool unreachedErr (unreachedCode /= ExitSuccess && "baseline has no completed point at --end-scale" `isInfixOf` unreachedErr)
        forM_ [("failure", "scaling failure"), ("without_loop", "require")] $ \(test, message) -> do
          (code, events, output) <- run test "128MiB" "2s"
          assertBool output (code /= ExitSuccess)
          assertBool output (message `isInfixOf` output)
          assertBool "failed workloads produce no accepted point" (null (eventsOf "point" events))
          forM_ (eventsOf "sample" events) $ \sample ->
            assertEqual "a failed result is not accepted" (Just (Aeson.Bool False)) (KM.lookup "accepted" sample)
          _ <- requireEvent "end" events
          return ()
        (memoryCode, memory, memoryOutput) <- run "slow" "1B" "2s"
        assertEqual memoryOutput ExitSuccess memoryCode
        assertEnd "memory limit reached" memory
        assertBool "the memory guard stops before accepting a point" (null (eventsOf "point" memory))
        forM_ ["stdout", "stderr"] $ \test -> do
          (code, events, output) <- run test "128MiB" "10s"
          assertBool output (code /= ExitSuccess)
          assertEnd "sample output exceeded 1MiB per stream" events
          assertBool "oversized unterminated output cannot contribute a point" (null (eventsOf "point" events))

-- Test helpers ----------------------------------------------------------------

point :: Int -> [Double] -> Double -> ScalePoint
point scale times rss = ScalePoint scale
    [KM.fromList [("avg_wall_duration", Aeson.toJSON t), ("peak_rss", Aeson.toJSON rss)] | t <- times]

curve :: (Int -> Double) -> [Int] -> [ScalePoint]
curve model powers = reverse [point n (replicate 3 (model n)) 100 | power <- powers, let n = 2^power]

assertTrend :: Int -> Double -> Maybe (Int, Double, Double) -> Assertion
assertTrend start exponent result = case result of
    Just (first, lo, hi) -> do
      assertEqual "the recent range starts at the oldest confirming point" start first
      closeTo exponent (Just lo)
      closeTo exponent (Just hi)
    Nothing -> assertFailure "expected a stable recent trend"

closeTo :: Double -> Maybe Double -> Assertion
closeTo expected actual = assertBool (show actual) (maybe False (\x -> abs (x - expected) < 1e-9) actual)

-- Simulated timings exercise the real controller and journal without waiting
-- for large workloads. The invocation count also identifies later references.
simulate :: String -> [String] -> (Int -> Int -> TestResult) -> IO (Int, [Aeson.Object])
simulate name options result = withSystemTempDirectory ("acton-scale-" ++ name) $ \directory -> do
    (gopts, opts) <- parseScale (["--max-memory", "128MiB", "--max-time", "30s"] ++ options)
    calls <- newIORef IM.empty
    code <- runScalingStudy False gopts opts directory KM.empty [("sample", name, Nothing)] Nothing $ \_ scale modName testName -> do
      counts <- readIORef calls
      when (sum (IM.elems counts) >= 500) (assertFailure "the simulated study did not stop")
      let invocation = IM.findWithDefault 0 scale counts + 1
      writeIORef calls (IM.insert scale invocation counts)
      return ((result scale invocation) { trModule = modName, trName = testName })
    files <- filter ((== ".jsonl") . takeExtension) <$> listDirectory directory
    assertEqual "each run writes one journal" 1 (length files)
    events <- BL.readFile (directory </> head files) >>= decodeEvents . BL.unpack
    return (code, events)

modelResult :: Int -> Double -> TestResult
modelResult scale wall = TestResult
    { trModule = "sample", trName = "model", trComplete = True, trSuccess = Just True
    , trSkipped = False, trSkipReason = Nothing, trException = Nothing
    , trOutput = Nothing, trStdOut = Nothing, trStdErr = Nothing, trFlaky = False
    , trNumSkipped = 0, trNumFailures = 0, trNumErrors = 0, trNumIterations = 1
    , trTestDuration = wall, trSnapshotUpdated = False, trCached = False
    , trRaw = Aeson.object ["complete" Aeson..= True, "success" Aeson..= True,
        "skipped" Aeson..= False, "num_iterations" Aeson..= (1 :: Int), "loop_iterations" Aeson..= (1 :: Int),
        "mem_usage_delta_avg" Aeson..= (toInteger scale * 100),
        "avg_wall_duration" Aeson..= wall, "peak_rss" Aeson..= (1048576 :: Int),
        "perf_info" Aeson..= Aeson.object ["scale" Aeson..= scale, "loop" Aeson..= True, "scaling" Aeson..= True,
          "machine" Aeson..= ("test-machine" :: String), "version" Aeson..= ("3" :: String),
          "workers" Aeson..= (2 :: Int), "gc" Aeson..= ("natural" :: String), "tags" Aeson..= ([] :: [String]),
          "build" Aeson..= Aeson.object ["target" Aeson..= ("native" :: String), "optimize" Aeson..= ("ReleaseFast" :: String),
            "cpu" Aeson..= ("" :: String), "no_threads" Aeson..= False, "db" Aeson..= False, "no_dbp" Aeson..= False]]]
    }

assertSummary :: String -> (Int, Int) -> (Int, Int) -> [Aeson.Object] -> IO Aeson.Object
assertSummary outcome (first, final) (points, samples) events = do
    summary <- requireEvent "test_end" events
    assertEqual "study outcome" (Just (Aeson.toJSON outcome)) (KM.lookup "outcome" summary)
    assertEqual "covered range begins at the first sampled scale" (Just (Aeson.toJSON first)) (KM.lookup "min_scale" summary)
    assertEqual "the controller reached the expected final scale" (Just (Aeson.toJSON final)) (KM.lookup "max_scale" summary)
    assertEqual "all curve points are counted" (Just (Aeson.toJSON points)) (KM.lookup "points" summary)
    assertEqual "curve samples exclude reference checks" (Just (Aeson.toJSON samples)) (KM.lookup "samples" summary)
    assertEqual "each reported point is journaled" points (length (eventsOf "point" events))
    assertEnd "benchmark finished" events
    return summary

assertSampleProvenance :: [Aeson.Object] -> Assertion
assertSampleProvenance events = forM_ (eventsOf "sample" events) $ \sample -> do
    assertEqual "completed valid samples are accepted" (Just (Aeson.Bool True)) (KM.lookup "accepted" sample)
    raw <- field "result" sample
    info <- field "perf_info" raw
    assertEqual "the measured workload matches the requested scale" (KM.lookup "scale" sample) (KM.lookup "scale" info)
    assertEqual "the measurement is complete" (Just (Aeson.Bool True)) (KM.lookup "complete" raw)
    assertEqual "one measured invocation" (Just (Aeson.Number 1)) (KM.lookup "num_iterations" raw)
    assertEqual "one measured loop body" (Just (Aeson.Number 1)) (KM.lookup "loop_iterations" raw)

parseOptions :: [String] -> O.ParserResult C.CmdLineOptions
parseOptions = O.execParserPure C.cmdLinePrefs (O.info (C.cmdLineParser O.<**> O.helper) mempty)

parseScale :: [String] -> IO (C.GlobalOptions, C.TestOptions)
parseScale args = case parseOptions (["test", "scale"] ++ args) of
    O.Success (C.CmdOpt gopts (C.Test (C.TestScale opts))) -> return (gopts, opts)
    O.Failure failure -> assertFailure (fst (O.renderFailure failure "acton")) >> fail "invalid options"
    _ -> assertFailure "expected scale options" >> fail "invalid command"

decodeEvents :: String -> IO [Aeson.Object]
decodeEvents text = do
    let lines = filter (not . BL.null) (BL.lines (BL.pack text))
    when (null lines) (assertFailure "expected scaling journal events")
    mapM (\line -> case Aeson.eitherDecode line of
      Right (Aeson.Object obj) -> return obj
      _ -> assertFailure ("invalid journal event: " ++ BL.unpack line) >> fail "invalid journal") lines

eventsOf :: String -> [Aeson.Object] -> [Aeson.Object]
eventsOf kind = filter ((== Just (Aeson.toJSON kind)) . KM.lookup "event")

requireEvent :: String -> [Aeson.Object] -> IO Aeson.Object
requireEvent kind events = case find ((== Just (Aeson.toJSON kind)) . KM.lookup "event") events of
    Just event -> return event
    Nothing -> assertFailure ("missing " ++ kind ++ " event: " ++ show events) >> fail "missing event"

field :: Aeson.FromJSON a => Aeson.Key -> Aeson.Object -> IO a
field key obj = case KM.lookup key obj >>= AesonTypes.parseMaybe Aeson.parseJSON of
    Just value -> return value
    Nothing -> assertFailure ("missing " ++ show key ++ ": " ++ show obj) >> fail "missing field"

assertEnd :: String -> [Aeson.Object] -> Assertion
assertEnd reason events = do
    end <- requireEvent "end" events
    assertEqual "study stop reason" (Just (Aeson.toJSON reason)) (KM.lookup "reason" end)

-- Memory probes ---------------------------------------------------------------

perfMemoryTests :: TestTree
perfMemoryTests = testGroup "live memory observation"
  [ nativeTest "host and process readings" "perf_memory_probe" ["perf_memory.c"]
  , nativeTest "Linux cgroup boundaries" "perf_memory_test" []
  ]
  where
    nativeTest label name extra = testCase label $
      withSystemTempDirectory "acton-perf-memory" $ \tmp -> do
        sources <- canonicalizePath "cbits"
        let binary = tmp </> name
            args = ["-Wall", "-Wextra", "-Werror", "-O2"]
              ++ map (sources </>) ((name ++ ".c") : extra) ++ ["-o", binary]
        (compiled, out, err) <- readProcessWithExitCode "cc" args ""
        assertEqual (out ++ err) ExitSuccess compiled
        (status, stdout, stderr) <- readProcessWithExitCode binary [tmp] ""
        assertEqual (stdout ++ stderr) ExitSuccess status

-- Recordings and terminal charts ----------------------------------------------

scaleReportTests :: TestTree
scaleReportTests = testGroup "terminal charts"
  [ testCase "charts preserve ranges and partial points, excluding reference and rejected samples" $ do
      let samples = [sample 2 t 1048576 True False | t <- [8, 10, 12]]
                 ++ [KM.fromList [("event", Aeson.String "point"), ("scale", Aeson.Number 2)]]
                 ++ [sample 4 t 2097152 True False | t <- [16, 20, 24]]
                 ++ [sample 8 32 1 False False, sample 2 9999 1 True True,
                     sample 8 (-1) 1 True False]
          points = foldl' (flip addScaleEvent) IM.empty samples
      assertEqual "linear work has constant time per scale and preserves sample ranges"
        [ Chart WallTime [ChartPoint 2 8 10 12 True, ChartPoint 4 16 20 24 False] []
        , Chart TimePerScale [ChartPoint 2 4000 5000 6000 True, ChartPoint 4 4000 5000 6000 False] []
        , Chart Allocated [] []
        ] (scaleCharts points IM.empty)
      assertEqual "summary distinguishes finished sizes and accepted partial samples"
        "1 size + 1 partial · 6 curve samples · scale 2 … 4" (scaleSummary points)
  , testCase "zero clock readings retain coverage and allocations without biasing time charts" $ do
      let events = [allocated 1024 (sample 1 t 1048576 True False) | t <- [0, 0.001, 0.002]]
                ++ [KM.fromList [("event", Aeson.String "point"), ("scale", Aeson.Number 1)],
                    allocated 2048 (sample 2 0.002 2097152 True False)]
          points = foldl' (flip addScaleEvent) IM.empty events
      assertEqual "all samples remain in the recorded coverage"
        "1 size + 1 partial · 4 curve samples · scale 1 … 2" (scaleSummary points)
      assertEqual "a time point is omitted in full, while its allocations remain visible"
        [ Chart WallTime [ChartPoint 2 0.002 0.002 0.002 False] []
        , Chart TimePerScale [ChartPoint 2 1 1 1 False] []
        , Chart Allocated [ChartPoint 1 1 1 1 True, ChartPoint 2 2 2 2 False] []
        ] (scaleCharts points IM.empty)
      withRecording (header 2 : map (KM.insert "module" (Aeson.String "alpha") .
        KM.insert "test" (Aeson.String "same")) events ++ [ending]) $ \_ run -> do
          (code, out, err) <- run
          assertEqual (out ++ err) ExitSuccess code
          assertBool out ("Time charts omit sizes with zero clock readings" `isInfixOf` out)
  , testCase "allocation charts retain zero, ranges and missing measurements" $ do
      let events = [allocated n (sample 1 1 1048576 True False) | n <- [0,1024,2048]]
                ++ [KM.fromList [("event", Aeson.String "point"), ("scale", Aeson.Number 1)],
                    allocated 0 (sample 2 2 1048576 True False),
                    allocated 1024 (sample 4 4 1048576 True False),
                    sample 4 4 1048576 True False]
          points = foldl' (flip addScaleEvent) IM.empty events
          chart = scaleCharts points IM.empty !! 2
      assertEqual "missing samples do not become zeros or partial averages"
        (Chart Allocated [ChartPoint 1 0 1 2 True, ChartPoint 2 0 0 0 False] []) chart
      let output = unlines (chartText False 67 12 chart)
      assertBool output ("0.000│" `isInfixOf` output && '○' `elem` output)
      assertEqual "zero allocation still renders graphics" (67 * 8 * 12 * 16 * 4)
        (BS.length (chartPixels True 67 12 chart))
      let small = Chart Allocated [ChartPoint 1 0 0 0 True,
                                   ChartPoint 2 (16/1024) (16/1024) (16/1024) True] []
      assertBool "small allocation volumes use the full vertical range"
        ('●' `elem` concat (take 3 (chartText False 67 12 small)))
      withRecording (header 2 : map (KM.insert "module" (Aeson.String "alpha") .
        KM.insert "test" (Aeson.String "same")) events ++ [ending]) $ \_ run -> do
          (code, out, err) <- run
          assertEqual (out ++ err) ExitSuccess code
          assertEqual "reports show three charts" 3 (length (filter ("  ◆ " `isPrefixOf`) (lines out)))
          assertBool out ("Linear allocation axis" `isInfixOf` out)
          assertBool "older journals already contain allocation samples" ("Allocated (KiB)" `isInfixOf` out)
          assertBool "known allocation data is not labelled unavailable"
            (not ("Allocation measurements are unavailable" `isInfixOf` out))
  , testCase "saved journals replay outside a project, preserving separate tests and partial sizes" $
      forM_ [1, 2] $ \version -> do
        let named modName = KM.insert "module" (Aeson.String modName) . KM.insert "test" (Aeson.String "same")
            events = header version
              : map (named "alpha") ([sample 1 t 1048576 True False | t <- [1,2,3]]
                  ++ [KM.fromList [("event", Aeson.String "point"), ("scale", Aeson.Number 1)],
                      sample 2 4 1048576 True False, sample 1 9999 1048576 True True,
                      sample 3 8888 1048576 False False,
                      KM.fromList [("event", Aeson.String "test_end"),
                                   ("reason", Aeson.String "Reference measurements did not settle")]])
              ++ map (named "beta") ([sample 1 20 1048576 True False | _ <- [1..3]]
                  ++ [KM.fromList [("event", Aeson.String "point"), ("scale", Aeson.Number 1)]])
              ++ [ending]
        withRecording events $ \path run -> do
          before <- BS.readFile path
          (code, out, err) <- run
          assertEqual (out ++ err) ExitSuccess code
          forM_ ["Scaling charts: alpha.same", "Scaling charts: beta.same",
                 "1 size + 1 partial · 4 curve samples", "1 size · 3 curve samples",
                 "Reference measurements did not settle", "Stopped: all selected studies finished"] $ \text ->
            assertBool out (text `isInfixOf` out)
          assertBool "plain reports have no terminal escapes" (notElem '\ESC' (out ++ err))
          assertEqual "each test has two charts when allocations are unavailable" 4
            (length (filter ("  ◆ " `isPrefixOf`) (lines out)))
          assertEqual "replay never changes the recording" before =<< BS.readFile path
          assertEqual "replay creates no build or measurement files" [takeFileName path] =<< listDirectory (takeDirectory path)
  , testCase "replay requires a completed point at the requested endpoint" $ do
      let named = KM.insert "module" (Aeson.String "alpha") . KM.insert "test" (Aeson.String "same")
          study = KM.insert "end_scale" (Aeson.Number 100000) (header 3)
          partial = named (sample 100000 10 1048576 True False)
          point = named (KM.fromList [("event", Aeson.String "point"), ("scale", Aeson.Number 100000)])
      forM_ [Nothing, Just 100000, Just 200000] $ \completed ->
        withRecording ([study, partial] ++ concat
          [[named (sample n 10 1048576 True False), KM.insert "scale" (Aeson.toJSON n) point] | Just n <- [completed]] ++ [ending]) $ \_ run -> do
          (code, out, err) <- run
          assertEqual (out ++ err) ExitSuccess code
          assertEqual "only the exact completed endpoint satisfies the target" (completed /= Just 100000)
            ("Requested end scale 100000 was not reached" `isInfixOf` out)
  , testCase "incomplete journals retain samples and diagnose an unfinished final record" $
      withRecording [header 2, KM.insert "module" (Aeson.String "alpha")
        (KM.insert "test" (Aeson.String "same") (sample 1 10 1048576 True False))] $ \path run -> do
          forM_ [False, True] $ \truncated -> do
            if truncated then BS.appendFile path "{\"event\":\"sample\"" else return ()
            (code, out, err) <- run
            assertEqual (out ++ err) ExitSuccess code
            assertBool out ("0 sizes + 1 partial · 1 curve sample" `isInfixOf` out)
            assertBool out ("Recording is incomplete" `isInfixOf` out)
            assertEqual "only an unfinished JSON record needs a warning" truncated ("unfinished final record" `isInfixOf` err)
          BS.appendFile path "\n"
          (code, out, err) <- run
          assertBool "a malformed complete record is an error" (code /= ExitSuccess)
          assertBool err (":3:" `isInfixOf` err)
          assertBool "invalid journals are not presented as valid charts" (not ("Scaling charts:" `isInfixOf` out))
  , testCase "empty measurements are explicit and unrelated or unsupported files are rejected" $ do
      withRecording [header 2, ending] $ \_ run -> do
        (code, out, err) <- run
        assertEqual (out ++ err) ExitSuccess code
        assertBool out ("No accepted measurements" `isInfixOf` out)
      forM_ [[], [KM.empty], [header 99], [header 2, header 2]] $ \events ->
        withRecording events $ \_ run -> do
          (code, out, err) <- run
          assertBool (out ++ err) (code /= ExitSuccess)
          assertBool "bad input has a diagnostic" (not (null err))
          assertBool "bad input is not presented as a valid report" (not ("Scaling charts:" `isInfixOf` out))
  , testCase "comparisons retain sample identity and reject a later incompatible sample" $ do
      let named = KM.insert "module" (Aeson.String "alpha") . KM.insert "test" (Aeson.String "same")
          measured machine workers n = named $ allocated (fromIntegral n * 1024) $ KM.mapWithKey
            (\key value -> case (key, value) of
              ("result", Aeson.Object result) -> Aeson.Object (KM.insert "perf_info" (identity machine workers n) result)
              _ -> value) (sample n 10 1048576 True False)
          identity machine workers n = Aeson.object
            ["machine" Aeson..= (machine :: String), "workers" Aeson..= (workers :: Int),
             "scale" Aeson..= n, "scaling" Aeson..= True, "loop" Aeson..= True,
             "version" Aeson..= ("3" :: String), "gc" Aeson..= ("natural" :: String),
             "tags" Aeson..= ([] :: [String]), "build" Aeson..= Aeson.object
               ["target" Aeson..= ("native" :: String), "optimize" Aeson..= ("ReleaseFast" :: String),
                "cpu" Aeson..= ("" :: String), "no_threads" Aeson..= False, "db" Aeson..= False, "no_dbp" Aeson..= False]]
          events = [header 3, measured "machine-a" 2 1, measured "machine-a" 2 3, ending]
      withRecording events $ \path _ -> do
        recording <- readScaleRecording path
        let series = recordingTests recording M.! ("alpha", "same")
        assertEqual "different workload sizes share one sampling identity" Nothing (scaleSeriesReason series series)
        acton <- canonicalizePath "../../dist/bin/acton"
        let baseline = takeDirectory path </> "baseline.jsonl"
            compare = readCreateProcessWithExitCode
              (proc acton ["test", "scale", "--compare", baseline, "--report", path, "--color", "never"])
                { cwd = Just (takeDirectory path) } ""
        copyFile path baseline
        (code, out, err) <- compare
        assertEqual (out ++ err) ExitSuccess code
        assertBool out ("baseline" `isInfixOf` out && "current" `isInfixOf` out)
        assertEqual "comparisons still show three charts" 3
          (length (filter ("  ◆ " `isPrefixOf`) (lines out)))
        forM_ [("machine-b", 2, "machine identity"), ("machine-a", 3, "worker count")] $ \(machine, workers, reason) -> do
          BL.writeFile path (BL.concat [Aeson.encode event <> "\n" | event <-
            [header 3, measured "machine-a" 2 1, measured machine workers 3, ending]])
          (code, out, err) <- compare
          assertBool (out ++ err) (code /= ExitSuccess && reason `isInfixOf` err)
          assertBool "no invalid overlay is displayed" (not ("Scaling charts:" `isInfixOf` out))
  , testCase "the proportional guide uses the largest completed size, without fitting or extrapolating" $ do
      let points = [ChartPoint 1 10 10 10 True, ChartPoint 100 20 20 20 True,
                    ChartPoint 200 100 100 100 False]
      assertEqual "proportional time, anchored at size 100" [(1, 0.2), (100, 20)]
        (chartGuide (Chart WallTime points []))
      forM_ [Chart WallTime [] [], Chart WallTime [head points] [],
             Chart WallTime [p {chartComplete = False} | p <- points] [],
             Chart TimePerScale points [], Chart Allocated points []] $ \chart ->
        assertEqual "only wall time with a completed span has a guide" [] (chartGuide chart)
  , testCase "a guide below the visible range is clipped rather than clamped to the axis" $ do
      let chart = Chart WallTime [ChartPoint 1 1 1 1 True, ChartPoint 1e6 1 1 1 True] []
          pixels = BS.unpack (chartPixels True 67 12 chart)
          gold = [i | (i,(r,g,b,a)) <- zip [0..] (rgbas pixels), r > g, g > b, a > 0]
      assertBool "part of the guide is visible" (not (null gold))
      assertBool "it enters the plot only in the final decade"
        (all (\i -> i `mod` 536 > 450) gold)
      let diagonal = Chart WallTime [ChartPoint 1 1 1 1 True, ChartPoint 1000 4 4 4 True] []
      assertBool "a near-diagonal guide still contains visible dashes"
        (any (\(r,g,b,a) -> r > g && g > b && a > 0)
          (rgbas (BS.unpack (chartPixels True 67 12 diagonal))))
      assertBool "monochrome keeps every visible pixel neutral"
        (all (\(r,g,b,_) -> r == g && g == b) (rgbas (BS.unpack (chartPixels False 67 12 chart))))
  , testCase "empty, singleton, constant and wide-ranging plots fit the terminal" $ do
      let curves = [[], [ChartPoint 1 1 1 1 False],
                    [ChartPoint n 1 1 1 True | n <- [1,10,100]],
                    [ChartPoint 1 0.00001 0.00001 0.00001 True, ChartPoint 1e12 1e6 1e6 1e6 True]]
      forM_ curves $ \points -> forM_ [(27,4), (67,12), (96,12)] $ \(width, height) -> do
        let chart = Chart WallTime points []
            output = chartText False width height chart
        assertEqual "title, plot, baseline and scale labels" (height + 3) (length output)
        assertBool (show output) (all ((<= width + 11) . length) output)
        assertBool "text fallback has no escape codes" (all (notElem '\ESC') output)
        assertEqual "RGBA has four bytes per pixel" (width * 8 * height * 16 * 4)
          (BS.length (chartPixels True width height chart))
      let partial = unlines (chartText False 67 12 (Chart WallTime [ChartPoint 1 1 1 1 False] []))
      assertBool "a completed sample in an unfinished point stays hollow" ('○' `elem` partial)
      let narrow = chartText False 27 8 (Chart WallTime
            [ChartPoint 1 1 1 1 True, ChartPoint 1e6 1e6 1e6 1e6 True] [])
      assertEqual "narrow axes show whole, separated decade labels"
        ["scale", "1", "100", "1e4", "1e6"] (words (last narrow))
  , testCase "recorded and new curves share axes without losing the unmatched range" $ do
      let current = [ChartPoint 1 1 2 3 True, ChartPoint 10 4 5 6 False]
          old = [ChartPoint 1 10 12 14 True, ChartPoint 1e6 800 900 1000 False]
          chart = Chart WallTime current old
          text = unlines (chartText False 67 12 chart)
          colors = rgbas (BS.unpack (chartPixels True 27 4 chart))
      forM_ ['●', '○', '◆', '◇'] $ \mark -> assertBool "both series and partial samples remain distinguishable" (mark `elem` text)
      assertBool "old sizes beyond an interrupted new curve remain visible" ("1e6" `isInfixOf` text)
      assertBool "new curve has its own color" ((56,189,248,255) `elem` colors)
      assertBool "recorded curve has its own color" ((251,146,60,255) `elem` colors)
      assertBool "monochrome preserves both styles without colored pixels"
        (all (\(r,g,b,_) -> r == g && g == b) (rgbas (BS.unpack (chartPixels False 27 4 chart))))
      assertEqual "recorded points do not change the proportional guide" [] (chartGuide chart)
      let coincident = unlines (chartText False 67 12 (Chart WallTime (take 1 current) (take 1 current)))
      assertBool "coincident means retain both series' presence" ('◈' `elem` coincident)
  , testCase "graphics selection is conservative and never applies to redirected output" $ do
      forM_ [[("TERM", "xterm-kitty")], [("TERM", "xterm-ghostty")],
             [("TERM", "xterm-256color"), ("TERM_PROGRAM", "ghostty")]] $ \env -> do
        assertBool (show env) (kittyTerminal True env)
        assertBool "redirection wins over terminal environment" (not (kittyTerminal False env))
        forM_ [[("TMUX", "/tmp/tmux")], [("STY", "123.tty")],
               [("TERM", "screen-256color")], [("TERM", "tmux-256color")], [("TERM", "dumb")]] $ \override ->
          assertBool (show (override ++ env)) (not (kittyTerminal True (override ++ env)))
      forM_ [[], [("TERM", "xterm-256color")], [("TERM_PROGRAM", "WezTerm")]] $ \env ->
        assertBool "unknown or optional graphics support uses text" (not (kittyTerminal True env))
  , testCase "Kitty transfers round-trip through multiple quiet bounded chunks" $ do
      let pixels = BS.pack (take (80 * 8 * 12 * 16 * 4) (randoms (mkStdGen 42) :: [Word8]))
          stream = kittyImage 80 12 pixels
          frames = [BS.drop 2 part | part <- BC.split '\ESC' stream, "_G" `BS.isPrefixOf` part]
          fields = map (BC.split ',' . BS.takeWhile (/= 59)) frames
          payloads = map (BS.drop 1 . BS.dropWhile (/= 59)) frames
      assertBool "fixture exercises continuation frames" (length frames > 2)
      forM_ fields $ \keys -> assertBool "no terminal replies" ("q=2" `elem` keys)
      forM_ ["a=T", "f=32", "o=z", "s=640", "v=192", "c=80", "r=12", "C=1"] $ \key ->
        assertBool (show key) (key `elem` head fields)
      forM_ payloads $ \payload -> do
        assertBool "payload <= 4096 bytes" (BS.length payload <= 4096)
        assertEqual "base64 chunk boundary" 0 (BS.length payload `mod` 4)
      forM_ (init fields) $ \keys -> assertBool "all non-final frames continue" ("m=1" `elem` keys)
      forM_ (tail fields) $ \keys -> assertEqual "continuations only carry m and q" 2 (length keys)
      assertBool "final frame terminates the transfer" ("m=0" `elem` last fields)
      compressed <- either (\err -> assertFailure err >> fail err) return (Base64.decode (BS.concat payloads))
      assertEqual "the terminal receives every RGBA pixel unchanged" pixels
        (BL.toStrict (decompress (BL.fromStrict compressed)))
  ]
  where
    rgbas :: [Word8] -> [(Word8, Word8, Word8, Word8)]
    rgbas [] = []
    rgbas (r:g:b:a:rest) = (r,g,b,a) : rgbas rest
    rgbas _ = error "incomplete RGBA pixel"
    allocated bytes = KM.mapWithKey (\key value -> case (key, value) of
      ("result", Aeson.Object result) -> Aeson.Object
        (KM.insert "mem_usage_delta_avg" (Aeson.toJSON (bytes :: Double)) result)
      _ -> value)
    header :: Int -> Aeson.Object
    header version = KM.fromList [("event", Aeson.String "study"), ("version", Aeson.toJSON version)]
    ending = KM.fromList [("event", Aeson.String "end"), ("reason", Aeson.String "all selected studies finished")]
    withRecording events action = withSystemTempDirectory "acton-scale-report" $ \directory -> do
      acton <- canonicalizePath "../../dist/bin/acton"
      let path = directory </> "recording.jsonl"
          run = readCreateProcessWithExitCode
            (proc acton ["test", "scale", "--report", path, "--color", "never"]) { cwd = Just directory } ""
      BL.writeFile path (BL.concat [Aeson.encode event <> "\n" | event <- events])
      action path run
    sample :: Int -> Double -> Double -> Bool -> Bool -> Aeson.Object
    sample n wall rss accepted reference = KM.fromList
      [ ("event", Aeson.String "sample"), ("scale", Aeson.toJSON n)
      , ("accepted", Aeson.Bool accepted), ("reference", Aeson.Bool reference)
      , ("result", Aeson.object ["avg_wall_duration" Aeson..= wall, "peak_rss" Aeson..= rss]) ]
