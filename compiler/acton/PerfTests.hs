{-# LANGUAGE OverloadedStrings #-}
module PerfTests (perfTests) where

import Control.Monad
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.List (isInfixOf)
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

perfTests :: TestTree
perfTests = testGroup "performance baselines"
  [ testCase "timing and allocation changes use the recorded measurement" $ do
      let old = KM.fromList [("avg_duration", Aeson.Number 10), ("mem_usage_delta_avg", Aeson.Number 100)]
          new = KM.fromList [("avg_duration", Aeson.Number 12), ("mem_usage_delta_avg", Aeson.Number 50)]
          rendered = unlines (formatTestPerfLines False (Just old) (result new))
      assertBool rendered ("12.000 ms (+20.00%)" `isInfixOf` rendered)
      assertBool rendered ("50 B (-50.00%)" `isInfixOf` rendered)
  , testCase "missing and zero baselines have defined output" $ do
      let render :: Maybe Aeson.Object -> Double -> String
          render old value = unlines $ formatTestPerfLines False old
            (result (KM.singleton "avg_duration" (Aeson.toJSON value)))
      assertBool "no baseline has no delta" (not ("%" `isInfixOf` render Nothing 2))
      assertBool "missing metric has no delta" (not ("%" `isInfixOf` render (Just KM.empty) 2))
      assertBool "zero to zero is unchanged"
        ("(+0.00%)" `isInfixOf` render (Just (KM.singleton "avg_duration" (Aeson.Number 0))) 0)
      assertBool "zero to nonzero has no percentage"
        ("(from 0; % n/a)" `isInfixOf` render (Just (KM.singleton "avg_duration" (Aeson.Number 0))) 2)
  , testCase "failed and incomplete runs have no performance comparison" $ do
      let res = result (KM.singleton "avg_duration" (Aeson.Number 10))
      forM_ [res { trComplete = False }, res { trSuccess = Just False }, res { trSkipped = True },
             res { trException = Just "error" }, res { trNumIterations = 0 }, res { trSnapshotUpdated = True }] $ \invalid ->
        assertEqual "no performance lines" [] (formatTestPerfLines False Nothing invalid)
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
        bytes <- BL.readFile baseline
        out <- runOK ["--name", "first"]
        assertBool out ("Mean:" `isInfixOf` out && "%" `isInfixOf` out)
        assertBool out ("Allocated / run:" `isInfixOf` out)
        assertEqual "comparison leaves the baseline intact" bytes =<< BL.readFile baseline
        ttyOut <- runOK ["--tty", "--name", "first"]
        assertBool ttyOut ("Mean:" `isInfixOf` ttyOut && "%" `isInfixOf` ttyOut)
        assertEqual "terminal comparison leaves the baseline intact" bytes =<< BL.readFile baseline
        json <- runOK ["--json", "--name", "first"]
        assertBool json ("\"cached\":false" `isInfixOf` json)
        -- Set a deterministic reference and check that --record compares with
        -- the old value before replacing only the selected measurement.
        let reference = M.adjust (M.adjust setMean "_test_first_wrapper")
              "perf_record.sample" saved
        BL.writeFile baseline (Aeson.encode reference)
        updatedOut <- runOK ["--record", "--name", "first"]
        assertBool updatedOut ("-100.00%" `isInfixOf` updatedOut)
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
        assertBool invalidOut ("Mean:" `isInfixOf` invalidOut && not ("%" `isInfixOf` invalidOut))
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
