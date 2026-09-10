{-# LANGUAGE OverloadedStrings #-}
module PerfTests (perfTests) where

import Control.Monad
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.List (isInfixOf, isSuffixOf, find, elemIndices)
import TerminalSize (termVisibleLength, termFitAnsiRight, termRenderedRows)
import qualified Data.Map as M
import System.Directory
import System.Exit
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import System.Process
import Test.Tasty
import Test.Tasty.HUnit

import qualified Acton.Fingerprint as Fingerprint
import Acton.Testing (TestResult(..))
import TestFormat (formatTestPerfLines)
import TestPerf (perfMeanInterval, perfJson)

perfTests :: TestTree
perfTests = testGroup "performance baselines"
  [ testCase "timing and allocation changes use the recorded measurement" $ do
      let old = KM.fromList [("avg_duration", Aeson.Number 10), ("mem_usage_delta_avg", Aeson.Number 100)]
          new = KM.fromList [("avg_duration", Aeson.Number 12), ("mem_usage_delta_avg", Aeson.Number 50)]
          rendered = unlines (renderPerf 79 False (Just old) (result new))
      assertBool rendered ("+20.0%" `isInfixOf` rendered)
      assertBool rendered ("-50.0%" `isInfixOf` rendered)
  , testCase "mean interval accounts for unequal variance and sample counts" $ do
      let old = sample 8 4 5
          new = sample 10 1 20
          half = 2.777 * sqrt 3.25
      case perfMeanInterval "duration" old new of
        Just (lo, hi) -> do
          assertBool (show (lo, hi)) (abs (lo - (2 - half)) < 0.000001)
          assertBool (show (lo, hi)) (abs (hi - (2 + half)) < 0.000001)
        Nothing -> assertFailure "expected a Welch interval"
  , testCase "mean interval handles missing and constant samples" $ do
      assertEqual "old baselines do not invent variance" Nothing (perfMeanInterval "duration" KM.empty (sample 1 1 10))
      assertEqual "one sample cannot estimate variance" Nothing (perfMeanInterval "duration" (sample 1 1 10) (sample 1 0 1))
      assertEqual "negative variance is invalid" Nothing (perfMeanInterval "duration" (sample 1 (-1) 10) (sample 1 1 10))
      assertEqual "constant samples have zero estimated uncertainty" (Just (2, 2))
        (perfMeanInterval "duration" (sample 1 0 10) (sample 3 0 10))
      assertEqual "zero baseline still supports an absolute interval" (Just (1, 1))
        (perfMeanInterval "duration" (sample 0 0 10) (sample 1 0 10))
  , testCase "uncertain mean changes are not colored as regressions" $ do
      let old = sample 10 10 5
          res = (result (sample 11 10 5)) { trNumIterations = 5 }
          rendered = unlines (renderPerf 79 True (Just old) res)
      assertBool rendered ("mean delta (95% CI):" `isInfixOf` rendered)
      assertBool rendered (not ("\ESC[91m" `isInfixOf` rendered))
  , testCase "each mean comparison uses its own variance" $ do
      let old = sample 10 0 10 `KM.union` KM.fromList
            [("mem_usage_delta_avg", Aeson.Number 1000), ("stdev_mem_usage_delta", Aeson.Number 1000)]
          new = sample 12 0 10 `KM.union` KM.fromList
            [("mem_usage_delta_avg", Aeson.Number 1100), ("stdev_mem_usage_delta", Aeson.Number 1000)]
          rows = renderPerf 120 True (Just old) ((result new) { trNumIterations = 10 })
      assertBool (unlines rows) (maybe False (isInfixOf "\ESC[91m+20.0%") (find ("time excl. GC" `isInfixOf`) rows))
      assertBool (unlines rows) (maybe False (isInfixOf "\ESC[2m+10.0%") (find ("allocated" `isInfixOf`) rows))
      case perfMeanInterval "mem_usage_delta" old new of
        Just (lo, hi) -> assertBool (show (lo, hi)) (lo < 0 && hi > 0 && abs (lo + hi - 200) < 0.000001)
        Nothing -> assertFailure "expected an allocation mean interval"
  , testCase "performance table groups timings and scales signed memory values" $ do
      let old = KM.fromList [("avg_duration", Aeson.Number 10), ("non_gc_mem_usage_delta_avg", Aeson.Number (-4096))]
          rows = renderPerf 120 False (Just old) tableResult
          timing = find ("time excl. GC" `isInfixOf`) rows
          memory = find ("non-GC change" `isInfixOf`) rows
      assertEqual "timing distribution and mean comparison"
        (Just ["time", "excl.", "GC", "12.0ms", "±", "1.00ms", "10.0ms", "…", "14.0ms", "1", "(10%)", "+20.0%"])
        (words <$> timing)
      assertEqual "signed memory uses the magnitude for its unit"
        (Just ["non-GC", "change", "-2.05KB", "—", "—", "+50.0%"])
        (words <$> memory)
      assertBool (unlines rows) ("  process peak RSS: 12.0MB" `elem` rows)
      assertBool "no ANSI escapes when color is disabled" (all (not . isInfixOf "\ESC[") rows)
      assertBool "the colored header keeps its full label"
        ("measurement" `isInfixOf` head (renderPerf 79 True (Just old) tableResult))
  , testCase "each measurement row shows its own distribution" $ do
      let rows = renderPerf 120 False Nothing fullTableResult
      forM_
        [ ("wall time", ["wall", "time", "12.0ms", "±", "2.00ms", "8.00ms", "…", "16.0ms", "0", "(0%)"])
        , ("GC time", ["GC", "time", "0.00ms", "±", "0.00ms", "0.00ms", "…", "0.00ms", "0", "(0%)"])
        , ("allocated", ["allocated", "1.54KB", "±", "128B", "1.02KB", "…", "2.05KB", "2", "(20%)"])
        , ("non-GC change", ["non-GC", "change", "-2.05KB", "±", "1.02KB", "-4.10KB", "…", "0.00B", "3", "(30%)"])
        ] $ \(label, expected) ->
          assertEqual label (Just expected) (words <$> find (label `isInfixOf`) rows)
  , testCase "JSON retains distributions in current and baseline measurements" $ do
      case trRaw fullTableResult of
        Aeson.Object obj -> case perfJson (Just obj) fullTableResult of
          Just (Aeson.Object report) -> forM_ ["measurements", "baseline"] $ \key ->
            assertEqual (show key) (Just (Aeson.Object obj)) (KM.lookup key report)
          _ -> assertFailure "expected a performance report"
        _ -> assertFailure "expected measurements"
  , testCase "small spreads and range endpoints keep their precision" $ do
      let obj = sample 1 0.001 10 `KM.union` KM.fromList [("min_duration", Aeson.toJSON (0.001 :: Double)), ("max_duration", Aeson.Number 1)]
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
            [("min_duration", Aeson.toJSON (0.000000001 :: Double)), ("max_duration", Aeson.Number 1)])) { trNumIterations = 10 }
          huge = (result (sample 1e300 1e200 10 `KM.union` KM.fromList
            [ ("min_duration", Aeson.toJSON (-1e300 :: Double)), ("max_duration", Aeson.toJSON (1e300 :: Double))
            , ("outlier_count", Aeson.Number 2000000000)
            ])) { trNumIterations = 2000000000 }
          timing rows = maybe "" id (find ("time excl. GC" `isInfixOf`) rows)
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
          timing rows = maybe "" id (find ("time excl. GC" `isInfixOf`) rows)
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
      let rows = renderPerf 120 False Nothing (result (KM.singleton "avg_duration" (Aeson.Number 1)))
      assertEqual "only the known mean is shown" (Just ["time", "excl.", "GC", "1.00ms", "—", "—"])
        (words <$> find ("time excl. GC" `isInfixOf`) rows)
  , testCase "missing and zero baselines have defined output" $ do
      let render :: Maybe Aeson.Object -> Double -> String
          render old value = unlines $ renderPerf 79 False old
            (result (KM.singleton "avg_duration" (Aeson.toJSON value)))
      assertBool "no baseline has no delta" (not ("%" `isInfixOf` render Nothing 2))
      assertBool "missing metric has no delta" (not ("%" `isInfixOf` render (Just KM.empty) 2))
      assertBool "zero to zero is unchanged"
        ("+0.0%" `isInfixOf` render (Just (KM.singleton "avg_duration" (Aeson.Number 0))) 0)
      assertBool "zero to nonzero has no percentage"
        ("from 0" `isInfixOf` render (Just (KM.singleton "avg_duration" (Aeson.Number 0))) 2)
  , testCase "failed and incomplete runs have no performance comparison" $ do
      let res = result (KM.singleton "avg_duration" (Aeson.Number 10))
      forM_ [res { trComplete = False }, res { trSuccess = Just False }, res { trSkipped = True },
             res { trException = Just "error" }, res { trNumIterations = 0 }, res { trSnapshotUpdated = True }] $ \invalid ->
        assertEqual "no performance lines" [] (renderPerf 79 False Nothing invalid)
  , testCase "recording runs fresh tests and preserves unselected measurements" $
      withSystemTempDirectory "acton-perf-record" $ \proj -> do
        acton <- canonicalizePath "../../dist/bin/acton"
        let name = "perf_record"
            fp = Fingerprint.formatFingerprint
              (Fingerprint.updateFingerprintPrefix (Fingerprint.fingerprintPrefixForName name) 1)
            baseline = proj </> "perf_data"
            run args = readCreateProcessWithExitCode
              (proc acton (["test", "perf", "--iter", "2", "--color", "never"] ++ args)) { cwd = Just proj } ""
            runOK args = do
              (code, out, err) <- run args
              assertEqual (unwords args ++ "\n" ++ out ++ err) ExitSuccess code
              return out
            readBaseline = do
              decoded <- Aeson.eitherDecodeFileStrict baseline
              case decoded of
                Left err -> assertFailure err >> return M.empty
                Right saved -> return (saved :: M.Map String (M.Map String Aeson.Value))
            setMean (Aeson.Object obj) = Aeson.Object (KM.insert "avg_duration" (Aeson.Number 1000000000) obj)
            setMean raw = raw
        createDirectoryIfMissing True (proj </> "src")
        writeFile (proj </> "Build.act") $ unlines ["name = " ++ show name, "fingerprint = " ++ fp]
        writeFile (proj </> "src/sample.act") $ unlines
          [ "import testing"
          , ""
          , "actor _test_first(t: testing.AsyncT):"
          , "    t.success()"
          , ""
          , "actor _test_second(t: testing.AsyncT):"
          , "    t.success()"
          , ""
          , "actor _test_failed(t: testing.AsyncT):"
          , "    t.failure(ValueError(\"expected failure\"))"
          , ""
          , "actor _test_skipped(t: testing.AsyncT):"
          , "    t.skip(\"expected skip\")"
          , ""
          , "actor _test_slow(t: testing.AsyncT):"
          , "    after 3.5: t.success()"
          ]
        _ <- runOK ["--record", "--name", "first|second"]
        saved0 <- readBaseline
        -- The old writer included null entries when a test process crashed.
        let saved = M.insert "missing" (M.singleton "_test_crashed" Aeson.Null) saved0
            tests = M.findWithDefault M.empty "perf_record.sample" saved
        BL.writeFile baseline (Aeson.encode saved)
        assertEqual "two measured tests" 2 (M.size tests)
        forM_ tests $ \raw -> case raw of
          Aeson.Object obj -> do
            forM_ distributionKeys $ \key -> assertBool (show key ++ " missing from recording") (KM.member key obj)
            case KM.lookup "peak_rss" obj of
              Nothing -> return ()
              Just (Aeson.Number rss) -> assertBool "peak RSS is positive when available" (rss > 0)
              Just _ -> assertFailure "peak RSS must be a number"
          _ -> assertFailure "expected recorded measurements"
        bytes <- BL.readFile baseline
        out <- runOK ["--name", "first"]
        assertBool out ("mean ± σ" `isInfixOf` unwords (words out) && "%" `isInfixOf` out)
        assertBool out ("allocated" `isInfixOf` out)
        assertEqual "comparison leaves the baseline intact" bytes =<< BL.readFile baseline
        ttyOut <- runOK ["--tty", "--name", "first"]
        assertBool ttyOut ("mean ± σ" `isInfixOf` unwords (words ttyOut) && "%" `isInfixOf` ttyOut)
        assertEqual "terminal comparison leaves the baseline intact" bytes =<< BL.readFile baseline
        json <- runOK ["--json", "--name", "first"]
        assertBool json ("\"cached\":false" `isInfixOf` json)
        forM_ ["performance", "median_duration", "stdev_duration", "outlier_count", "avg_wall_duration", "avg_gc_duration", "mean_difference_ci95_ms"] $ \key ->
          assertBool (key ++ " missing from " ++ json) (("\"" ++ key ++ "\"") `isInfixOf` json)
        forM_ distributionKeys $ \key -> assertBool (show key ++ " missing from " ++ json) (show key `isInfixOf` json)
        -- Set a deterministic reference and check that --record compares with
        -- the old value before replacing only the selected measurement.
        let reference = M.adjust (M.adjust setMean "_test_first_wrapper")
              "perf_record.sample" saved
        BL.writeFile baseline (Aeson.encode reference)
        updatedOut <- runOK ["--record", "--name", "first"]
        assertBool updatedOut ("-100.0%" `isInfixOf` updatedOut)
        updated <- readBaseline
        let updatedTests = M.findWithDefault M.empty "perf_record.sample" updated
        assertEqual "unselected measurement survives recording"
          (M.lookup "_test_second_wrapper" tests) (M.lookup "_test_second_wrapper" updatedTests)
        assertEqual "recording cached source must not empty the baseline" 2 (M.size updatedTests)
        assertEqual "unavailable old measurements survive recording"
          (M.lookup "missing" saved) (M.lookup "missing" updated)
        updatedBytes <- BL.readFile baseline
        (failedCode, _, _) <- run ["--record", "--name", "failed"]
        assertBool "failing test exits unsuccessfully" (failedCode /= ExitSuccess)
        _ <- runOK ["--record", "--name", "skipped"]
        _ <- runOK ["--record", "--name", "absent"]
        assertEqual "failed, skipped and empty selections preserve the baseline" updatedBytes =<< BL.readFile baseline
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
        -- Sampling limits are checked between iterations. A slow iteration
        -- must be allowed to finish beyond the former three-second watchdog.
        (slowCode, slowOut, slowErr) <- readCreateProcessWithExitCode
          (proc acton ["test", "perf", "--min-iter", "1", "--max-iter", "1", "--name", "slow", "--json"])
            { cwd = Just proj } ""
        assertEqual (slowOut ++ slowErr) ExitSuccess slowCode
  , testCase "record requires performance mode" $ do
      acton <- canonicalizePath "../../dist/bin/acton"
      forM_ [[], ["stress"], ["list"]] $ \mode -> do
        (code, _, err) <- readCreateProcessWithExitCode (proc acton (["test"] ++ mode ++ ["--record"])) ""
        assertBool err (code /= ExitSuccess && "--record requires acton test perf" `isInfixOf` err)
  ]

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
  , trNumIterations = 3
  , trTestDuration = 100
  , trRaw = Aeson.Object obj
  , trSnapshotUpdated = False
  , trCached = False
  }

sample :: Double -> Double -> Int -> Aeson.Object
sample mean sd n = KM.fromList
  [ ("avg_duration", Aeson.toJSON mean)
  , ("stdev_duration", Aeson.toJSON sd)
  , ("num_iterations", Aeson.toJSON n)
  ]

tableResult :: TestResult
tableResult = (result (sample 12 1 10 `KM.union` KM.fromList
  [ ("min_duration", Aeson.Number 10)
  , ("median_duration", Aeson.Number 12)
  , ("max_duration", Aeson.Number 14)
  , ("outlier_count", Aeson.Number 1)
  , ("avg_wall_duration", Aeson.Number 12)
  , ("avg_gc_duration", Aeson.Number 0)
  , ("mem_usage_delta_avg", Aeson.Number 1536)
  , ("non_gc_mem_usage_delta_avg", Aeson.Number (-2048))
  , ("peak_rss", Aeson.Number 12000000)
  ])) { trNumIterations = 10 }

fullTableResult :: TestResult
fullTableResult = tableResult { trRaw = case trRaw tableResult of
    Aeson.Object obj -> Aeson.Object (obj `KM.union` KM.fromList
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
      ])
    raw -> raw
  }

distributionKeys :: [Aeson.Key]
distributionKeys =
  [ stat <> "_" <> metric
  | metric <- ["wall_duration", "gc_duration", "mem_usage_delta", "non_gc_mem_usage_delta"]
  , stat <- ["min", "max", "median", "q1", "q3", "stdev", "outlier_count"]
  ]

renderPerf :: Int -> Bool -> Maybe Aeson.Object -> TestResult -> [String]
renderPerf cols useColor baseline res = map ($ cols) (formatTestPerfLines useColor baseline res)
