{-# LANGUAGE OverloadedStrings #-}
module ScaleReportTests (scaleReportTests) where

import Codec.Compression.Zlib (decompress)
import Control.Monad (forM_)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as BL
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.List (foldl', isInfixOf)
import Data.Word (Word8)
import ScaleReport
import System.Directory
import System.Exit (ExitCode(..))
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import System.Process
import System.Random (mkStdGen, randoms)
import Test.Tasty
import Test.Tasty.HUnit

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
        , Chart PeakMemory [ChartPoint 2 1 1 1 True, ChartPoint 4 2 2 2 False] []
        ] (scaleCharts points IM.empty)
      assertEqual "summary distinguishes finished sizes and accepted partial samples"
        "1 size + 1 partial · 6 curve samples · scale 2 … 4" (scaleSummary points)
  , testCase "zero clock readings retain coverage and memory without biasing time charts" $ do
      let events = [sample 1 t 1048576 True False | t <- [0, 0.001, 0.002]]
                ++ [KM.fromList [("event", Aeson.String "point"), ("scale", Aeson.Number 1)],
                    sample 2 0.002 2097152 True False]
          points = foldl' (flip addScaleEvent) IM.empty events
      assertEqual "all samples remain in the recorded coverage"
        "1 size + 1 partial · 4 curve samples · scale 1 … 2" (scaleSummary points)
      assertEqual "a time point is omitted in full, while its memory remains visible"
        [ Chart WallTime [ChartPoint 2 0.002 0.002 0.002 False] []
        , Chart TimePerScale [ChartPoint 2 1 1 1 False] []
        , Chart Allocated [] []
        , Chart PeakMemory [ChartPoint 1 1 1 1 True, ChartPoint 2 2 2 2 False] []
        ] (scaleCharts points IM.empty)
      withRecording (header 2 : map (KM.insert "module" (Aeson.String "alpha") .
        KM.insert "test" (Aeson.String "same")) events ++ [ending]) $ \_ run -> do
          (code, out, err) <- run
          assertEqual (out ++ err) ExitSuccess code
          assertBool out ("Time charts omit sizes with zero clock readings" `isInfixOf` out)
  , testCase "allocation charts retain zero, ranges and missing measurements" $ do
      let allocated bytes = KM.mapWithKey (\key value -> case (key, value) of
            ("result", Aeson.Object result) -> Aeson.Object
              (KM.insert "mem_usage_delta_avg" (Aeson.toJSON (bytes :: Double)) result)
            _ -> value)
          events = [allocated n (sample 1 1 1048576 True False) | n <- [0,1024,2048]]
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
          measured machine workers n = named $ KM.mapWithKey
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
             Chart TimePerScale points [], Chart PeakMemory points []] $ \chart ->
        assertEqual "only wall time with a completed span has a guide" [] (chartGuide chart)
  , testCase "a guide below the visible range is clipped rather than clamped to the axis" $ do
      let chart = Chart WallTime [ChartPoint 1 1 1 1 True, ChartPoint 1e6 1 1 1 True] []
          pixels = BS.unpack (chartPixels True 67 12 chart)
          rgbas [] = []
          rgbas (r:g:b:a:rest) = (r,g,b,a) : rgbas rest
          rgbas _ = error "incomplete RGBA pixel"
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
          rgbas [] = []
          rgbas (r:g:b:a:rest) = (r,g,b,a) : rgbas rest
          rgbas _ = error "incomplete RGBA"
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
