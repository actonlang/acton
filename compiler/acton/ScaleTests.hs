{-# LANGUAGE OverloadedStrings #-}
module ScaleTests (scaleTests) where

import qualified Acton.CommandLineParser as C
import qualified Acton.Fingerprint as Fingerprint
import Acton.Testing (TestResult(..))
import Control.Exception (throwIO)
import Control.Monad (forM_, when)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.IntMap.Strict as IM
import Data.IORef
import Data.List (find, isInfixOf)
import Data.Maybe (isJust)
import qualified Options.Applicative as O
import PerfMemory (MemoryStatus(..))
import ScaleReportTests (scaleReportTests)
import System.Clock (Clock(Monotonic), fromNanoSecs, getTime)
import System.Directory
import System.Exit
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import System.Process
import Test.Tasty
import Test.Tasty.HUnit
import TestScale

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
      forM_ [["--record"], ["--accept"], ["--watch"]] $ \args -> do
        (_, opts) <- parseScale args
        assertBool (unwords args) (isJust (validateScalingOptions opts))
      forM_ [[], ["--start-scale", "1000", "--max-memory", "128MiB", "--max-time", "2s"]] $ \args ->
        assertEqual (unwords args) Nothing . validateScalingOptions . snd =<< parseScale args
  , testGroup "adaptive controller"
      [ testCase "linear quadratic logarithmic and constant curves settle over a broad range" $
          forM_ [("linear", fromIntegral, Just 1),
                 ("quadratic", \n -> fromIntegral n ^ (2 :: Int), Just 2),
                 ("nlogn", \n -> fromIntegral n * logBase 2 (fromIntegral n + 1), Nothing),
                 ("constant", const 1, Just 0)] $ \(name, model, exponent) -> do
            (code, events) <- simulate name (\n _ -> modelResult n (model n))
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
      , testCase "precision controls the actual three five and seven sample batches" $ do
          let model n i = fromIntegral n * case n of
                1 -> cycle [0.9, 1.1, 1, 1, 1] !! (i - 1)
                2 -> cycle [0.8, 1.2, 1, 1, 1, 1, 1] !! (i - 1)
                _ -> 1
          (code, events) <- simulate "repeats" (\n i -> modelResult n (model n i))
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
          (code, events) <- simulate "bend" (\n _ -> modelResult n (model n))
          assertEqual "the later segment settles" 0 code
          summary <- assertSummary "stable" (1, 8192) (14, 42) events
          assertEqual "the reported window starts at the bend" (Just (Aeson.Number 256)) (KM.lookup "stable_from_scale" summary)
          closeTo 2 . Just =<< field "growth_min" summary
          closeTo 2 . Just =<< field "growth_max" summary
      , testCase "tiny probes do not choose the reference or establish coverage" $ do
          let model n = if n < 8 then 0.001 else fromIntegral n / 8
          (code, events) <- simulate "tiny_start" (\n _ -> modelResult n (model n))
          assertEqual "measurable work eventually settles" 0 code
          _ <- assertSummary "stable" (1, 8192) (14, 42) events
          forM_ (eventsOf "reference" events) $ \r ->
            assertEqual "the first measurable scale is the reference" (Just (Aeson.Number 8)) (KM.lookup "scale" r)
      , testCase "a fresh reference can reject an otherwise stable curve" $ do
          (code, events) <- simulate "drift" (\n i -> modelResult n (if n == 1 && i > 9 then 2 else fromIntegral n))
          assertEqual "unsettled drift is an inconclusive study" 0 code
          summary <- assertSummary "inconclusive" (1, 4096) (13, 39) events
          assertEqual "reference failure explains the outcome" (Just (Aeson.String "Reference measurements did not settle")) (KM.lookup "reason" summary)
          assertEqual "an inconclusive result cannot claim a growth range" (Just Aeson.Null) (KM.lookup "growth_min" summary)
          assertEqual "two periodic successes do not bypass three later failures"
            (map (Just . Aeson.Bool) [True, True, False, False, False])
            (map (KM.lookup "stable") (eventsOf "reference" events))
      , testCase "persistently tiny or noisy timing stops without an endless search" $
          forM_ [("tiny", \_ -> 0.001, 3),
                 ("noisy", \i -> cycle [0.1, 1, 1.9, 1, 1, 1, 1] !! (i - 1), 7)] $ \(name, model, repeats) -> do
            (code, events) <- simulate name (\n i -> modelResult n (model i))
            assertEqual name 0 code
            summary <- assertSummary "inconclusive" (1, 2^19) (20, 20 * repeats) events
            assertEqual "unreliable timing explains the outcome"
              (Just (Aeson.String "Timing stayed too short or noisy across 20 successive sizes")) (KM.lookup "reason" summary)
            assertBool "there is no reliable reference to recheck" (null (eventsOf "reference" events))
            forM_ (eventsOf "point" events) $ \p -> do
              assertEqual "unreliable samples remain visible" (Just (Aeson.Bool False)) (KM.lookup "reliable" p)
              assertEqual "no unsupported local growth is reported" (Just Aeson.Null) (KM.lookup "observed_exponent" p)
      , testCase "a result for the wrong scale cannot enter the curve" $ do
          (code, events) <- simulate "wrong_scale" (\n _ -> modelResult (n + 1) 1)
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
        code <- runScalingStudy False gopts opts directory KM.empty [("sample", "test")]
          (\_ _ _ _ -> throwIO (ScalingStopped reason))
        assertEqual "an unavailable guard cannot report success" 2 code
        files <- filter ((== ".jsonl") . takeExtension) <$> listDirectory directory
        assertEqual "one journal is retained" 1 (length files)
        events <- BL.readFile (directory </> head files) >>= decodeEvents . BL.unpack
        assertEnd reason events
        assertBool "an unguarded sample cannot contribute a point" (null (eventsOf "point" events))
  , scaleReportTests
  , testCase "scaling journals completed work when later work stops" $
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
  ]

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
simulate :: String -> (Int -> Int -> TestResult) -> IO (Int, [Aeson.Object])
simulate name result = withSystemTempDirectory ("acton-scale-" ++ name) $ \directory -> do
    (gopts, opts) <- parseScale ["--max-memory", "128MiB", "--max-time", "30s"]
    calls <- newIORef IM.empty
    code <- runScalingStudy False gopts opts directory KM.empty [("sample", name)] $ \_ scale modName testName -> do
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
        "avg_wall_duration" Aeson..= wall, "peak_rss" Aeson..= (1048576 :: Int),
        "perf_info" Aeson..= Aeson.object ["scale" Aeson..= scale, "loop" Aeson..= True, "scaling" Aeson..= True]]
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
    assertEnd "all selected studies finished" events
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

parseScale :: [String] -> IO (C.GlobalOptions, C.TestOptions)
parseScale args = case O.execParserPure C.cmdLinePrefs (O.info (C.cmdLineParser O.<**> O.helper) mempty) (["test", "scale"] ++ args) of
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
