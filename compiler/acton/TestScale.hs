{-# LANGUAGE OverloadedStrings #-}
module TestScale
  ( ScaleLimits(..), ScalingStopped(..), ScalePoint(..)
  , validateScalingOptions, runScalingStudy, watchScaleProcess
  , scaleMean, scaleError, scaleReliable, scaleNeedsSamples, scaleGrowth, stableGrowth
  , nextScale, scaleMemoryLimit
  ) where

import qualified Acton.CommandLineParser as C
import Acton.Testing (TestResult(..))
import TestPerf
import PerfMemory
import ScaleReport (addScaleEvent, printScaleReport)
import TerminalSize (queryTermSize, termFitAnsiRight)
import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad
import Data.Char (isAscii, isAlphaNum)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.IORef
import qualified Data.IntMap.Strict as IM
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import System.Clock
import System.Directory
import System.FilePath
import System.IO
import System.Process
import Text.Printf

data ScaleLimits = ScaleLimits
    { scaleDeadline :: TimeSpec
    , scaleMemory :: Integer
    , scaleReserve :: Integer
    }

data ScalingStopped = ScalingStopped String deriving Show
instance Exception ScalingStopped

data ScalePoint = ScalePoint
    { pointScale :: Int
    , pointSamples :: [Aeson.Object]
    } deriving Show

validateScalingOptions :: C.TestOptions -> Maybe String
validateScalingOptions opts
  | C.testRecord opts = Just "Scaling studies save every sample automatically; --record updates fixed-scale perf_data"
  | C.testSnapshotUpdate opts = Just "Scaling studies cannot update test snapshots"
  | C.watch (C.testCompile opts) = Just "Scaling studies cannot run with --watch"
  | otherwise = Nothing

-- Reserve memory for the machine as well as bounding the child itself.
scaleMemoryLimit :: C.MemoryLimit -> MemoryStatus -> Either String (Integer, Integer)
scaleMemoryLimit requested status =
    let total = memoryTotal status
        reserve = min (256 * 1024 * 1024) (total `div` 10)
        wanted = case requested of
          C.MemoryBytes n -> n
          C.MemoryPercent n -> floor (fromIntegral total * n / 100)
        cap = min wanted (memoryAvailable status - reserve)
    in if cap <= 0 then Left "Not enough available memory to start a scaling study"
       else Right (cap, reserve)

watchScaleProcess :: ScaleLimits -> Maybe Int -> ProcessHandle -> IO String
watchScaleProcess limits pid process =
    case pid of
      Nothing -> return "Memory monitoring could not obtain the child process ID"
      Just child -> loop child
  where
    loop child = do
      now <- getTime Monotonic
      if now >= scaleDeadline limits then return "time limit reached"
      else do
        exited <- getProcessExitCode process
        case exited of
          Just _ -> threadDelay 20000 >> loop child
          Nothing -> do
            status <- readMemoryStatus (Just child)
            case status of
              Left err -> do
                -- The child can exit between checking its status and sampling it.
                done <- getProcessExitCode process
                if isJust done then threadDelay 20000 >> loop child
                else return ("memory monitoring unavailable: " ++ err)
              Right memory
                | memoryProcess memory >= scaleMemory limits -> return "memory limit reached"
                | memoryAvailable memory <= scaleReserve limits -> return "available memory reserve reached"
                | otherwise -> threadDelay 50000 >> loop child

scaleMean :: [Double] -> Maybe Double
scaleMean [] = Nothing
scaleMean xs = Just (sum xs / fromIntegral (length xs))

pointValues :: String -> ScalePoint -> [Double]
pointValues metric = mapMaybe (`perfNumber` metric) . pointSamples

-- Estimated precision of the mean guides sampling. This is a stopping heuristic,
-- not a confidence interval: repeats can share noise and sampling is adaptive.
scaleError :: ScalePoint -> Maybe Double
scaleError point = do
    let values = pointValues "avg_wall_duration" point
        n = fromIntegral (length values)
    mean <- scaleMean values
    guard (n >= 3 && mean > 0)
    return (sqrt (sum [((x - mean) / mean)^2 | x <- values] / (n * (n - 1))))

scaleReliable :: ScalePoint -> Bool
scaleReliable point = maybe False (>= 0.1) (scaleMean (pointValues "avg_wall_duration" point))
                   && maybe False (<= 0.05) (scaleError point)

scaleNeedsSamples :: ScalePoint -> Bool
scaleNeedsSamples point =
    let n = length (pointSamples point)
    in n < 3 || (n < 7 && maybe False (>= 0.1) (scaleMean (pointValues "avg_wall_duration" point))
                       && not (scaleReliable point))

-- Growth is an observation over an explicit range, not an asymptotic bound.
scaleGrowth :: ScalePoint -> ScalePoint -> Maybe Double
scaleGrowth old new = do
    guard (scaleReliable old && scaleReliable new)
    a <- scaleMean (pointValues "avg_wall_duration" old)
    b <- scaleMean (pointValues "avg_wall_duration" new)
    guard (pointScale new > pointScale old)
    let scaleLog = log (fromIntegral (pointScale new) / fromIntegral (pointScale old))
        growth = log (b / a) / scaleLog
    guard (scaleLog > 0 && not (isNaN growth || isInfinite growth))
    return growth

-- Newest point first. Broad coverage prevents an early, short flat segment from
-- ending exploration. The last two growth rates confirm the preceding three;
-- bends or noisy points reset this window without discarding earlier data.
stableGrowth :: [ScalePoint] -> Maybe (Int, Double, Double)
stableGrowth points = do
    let recent = take 6 points
        reliable = filter scaleReliable points
    guard (length recent == 6 && not (null reliable))
    guard (toInteger (pointScale (head recent)) >= 1000 * toInteger (pointScale (last reliable)))
    let pairs = zip (tail recent) recent
    guard (all (\(old, new) -> 2 * toInteger (pointScale new) >= 3 * toInteger (pointScale old)) pairs)
    slopes <- mapM (uncurry scaleGrowth) pairs
    let lo = minimum slopes
        hi = maximum slopes
    guard (hi - lo <= 0.25)
    return (pointScale (last recent), lo, hi)

-- Slow down the geometric sweep near the memory boundary. Prediction is only
-- a sizing hint; the live monitor remains responsible for stopping the child.
nextScale :: Integer -> [ScalePoint] -> Maybe Int
nextScale cap _ | cap <= 0 = Nothing
nextScale _ [] = Nothing
nextScale cap (point:older) =
    let current = pointScale point
        peak p = maximum (1 : pointValues "peak_rss" p)
        exponent = case older of
          previous:_ | pointScale previous < current ->
            let scaleLog = log (fromIntegral current / fromIntegral (pointScale previous))
                growth = log (peak point / peak previous) / scaleLog
            in if scaleLog <= 0 || isNaN growth || isInfinite growth then 1 else max 1 growth
          _ -> 1
        factor = min 2 ((0.8 * fromIntegral cap / peak point) ** (1 / exponent))
        candidate = min (toInteger (maxBound :: Int)) (floor (fromIntegral current * factor))
    in if factor < 1.1 || candidate <= toInteger current then Nothing else Just (fromInteger candidate)

-- Each sample has its own process and the same amount of warmup and measured
-- work. The callback reuses the ordinary test runner's result handling.
runScalingStudy :: Bool -> C.GlobalOptions -> C.TestOptions -> FilePath -> Aeson.Object -> [(String, String)]
                -> (ScaleLimits -> Int -> String -> String -> IO TestResult) -> IO Int
runScalingStudy useColor gopts opts directory host tests runSample = do
    memory <- readMemoryStatus Nothing >>= either (ioError . userError) return
    (cap, reserve) <- either (ioError . userError) return
      (scaleMemoryLimit (fromMaybe (C.MemoryPercent 50) (C.testMaxMemory opts)) memory)
    start <- getTime Monotonic
    let duration = if C.testMaxTimeSet opts then C.testMaxTime opts else 3600000
        limits = ScaleLimits (start + fromNanoSecs (toInteger duration * 1000000)) cap reserve
    timestamp <- formatTime defaultTimeLocale "%Y%m%dT%H%M%S.%qZ" <$> getCurrentTime
    createDirectoryIfMissing True directory
    tty <- hIsTerminalDevice stdout
    let component = take 80 . map (\c -> if isAscii c && (isAlphaNum c || c `elem` ("._-" :: String)) then c else '_')
        name = case tests of
          [] -> "study"
          (modName, testName):rest -> component modName ++ "." ++ component testName
            ++ if null rest then "" else "+" ++ show (length rest) ++ "-more"
        path = directory </> (name ++ "-" ++ timestamp) <.> "jsonl"
        say = unless (C.testJson opts) . putStrLn
        progress = not (C.testJson opts || C.quiet gopts || C.noProgress gopts)
        live = progress && (tty || C.tty gopts)
        terminal text = when live (putStr text >> hFlush stdout)
        clearProgress = terminal "\r\ESC[K"
        updateProgress text = when live $ do
          (_, cols) <- fromMaybe (24, 80) <$> queryTermSize
          terminal ("\r" ++ termFitAnsiRight (max 0 (cols - 1)) text ++ "\ESC[K")
    chartData <- newIORef IM.empty
    code <- withFile path WriteMode $ \journal -> do
      let emit fields = do
            let obj = KM.fromList fields
                line = Aeson.encode (Aeson.Object obj)
            BL.hPutStrLn journal line
            hFlush journal
            when (C.testJson opts) (BL.putStrLn line >> hFlush stdout)
            unless (C.testJson opts) (modifyIORef' chartData (addScaleEvent obj))
          event kind fields = emit (("event" Aeson..= (kind :: String)) : fields)
          finish reason code = do
            now <- getTime Monotonic
            event "end" ["reason" Aeson..= reason, "elapsed_ms" Aeson..= milliseconds (now - start)]
            say ("Stopped: " ++ reason)
            say ("Results: " ++ path)
            return code
          sample modName testName scale reference = do
            now <- getTime Monotonic
            when (now >= scaleDeadline limits) (throwIO (ScalingStopped "time limit reached"))
            event "sample_start" ["module" Aeson..= modName, "test" Aeson..= testName,
                                  "scale" Aeson..= scale, "reference" Aeson..= reference]
            res <- runSample limits scale modName testName
            let raw = trRaw res
            checked <- try (case testPerfData res of
              Just obj | trNumIterations res == 1, Just info <- perfInfo obj,
                         KM.lookup "loop_iterations" obj == Just (Aeson.Number 1),
                         KM.lookup "scaling" info == Just (Aeson.Bool True),
                         KM.lookup "scale" info == Just (Aeson.toJSON scale) -> do
                when (maybe True (<= 0) (perfNumber obj "avg_wall_duration")) $
                  throwIO (ScalingStopped "wall time measurement unavailable")
                case perfNumber obj "peak_rss" of
                  Nothing -> throwIO (ScalingStopped "peak memory measurement unavailable")
                  Just peak | peak >= fromIntegral cap -> throwIO (ScalingStopped "memory limit reached")
                  _ -> return obj
              _ -> throwIO (userError (fromMaybe "Scaling requires a successful test with one measured t.loop() body" (trException res))))
                :: IO (Either SomeException Aeson.Object)
            event "sample" ["module" Aeson..= modName, "test" Aeson..= testName,
                            "scale" Aeson..= scale, "reference" Aeson..= reference, "result" Aeson..= raw,
                            "stdout" Aeson..= trStdOut res, "stderr" Aeson..= trStdErr res,
                            "exception" Aeson..= trException res,
                            "accepted" Aeson..= either (const False) (const True) checked]
            obj <- either throwIO return checked
            -- Raw results, including diagnostics, are already on disk.
            return (KM.filterWithKey (\key _ -> key `elem` ["avg_wall_duration", "peak_rss"]) obj)
          collect modName testName scale reference = do
            when (progress && not live) (say ("  sampling " ++ label))
            gather [] `finally` clearProgress
            where
              label = (if reference then "reference scale " else "scale ") ++ show scale
              gather samples = do
                let point = ScalePoint scale samples
                if scaleNeedsSamples point then do
                  let count = length samples
                      target = count + if null samples then 3 else 2
                  more <- forM [count + 1 .. target] $ \index -> do
                    updateProgress ("  " ++ label ++ " · sample " ++ show index ++ "/" ++ show target)
                    sample modName testName scale reference
                  gather (samples ++ more)
                else return point
          checkReference modName testName reference = do
            measured <- collect modName testName (pointScale reference) True
            let drift = do
                  guard (scaleReliable reference && scaleReliable measured)
                  before <- scaleMean (pointValues "avg_wall_duration" reference)
                  after <- scaleMean (pointValues "avg_wall_duration" measured)
                  return (after / before - 1)
                stable = maybe False ((<= 0.2) . abs) drift
            event "reference" ["module" Aeson..= modName, "test" Aeson..= testName,
                               "scale" Aeson..= pointScale reference, "change" Aeson..= drift,
                               "samples" Aeson..= length (pointSamples measured),
                               "relative_standard_error" Aeson..= scaleError measured,
                               "stable" Aeson..= stable]
            unless stable $ say "  Reference measurements changed or remain noisy; continuing to check the trend."
            return stable
          endTest modName testName outcome reason points trend = do
            let first = pointScale (last points)
                final = pointScale (head points)
                samples = sum (map (length . pointSamples) points)
            event "test_end" ["module" Aeson..= modName, "test" Aeson..= testName,
                              "outcome" Aeson..= (outcome :: String), "reason" Aeson..= reason,
                              "min_scale" Aeson..= first, "max_scale" Aeson..= final,
                              "points" Aeson..= length points, "samples" Aeson..= samples,
                              "stable_from_scale" Aeson..= fmap (\(start, _, _) -> start) trend,
                              "growth_min" Aeson..= fmap (\(_, lo, _) -> lo) trend,
                              "growth_max" Aeson..= fmap (\(_, _, hi) -> hi) trend]
            say ("  " ++ reason ++ "; " ++ show (length points) ++ " sizes, " ++ show samples
                 ++ " curve samples, scales " ++ show first ++ " … " ++ show final)
            forM_ trend $ \(start, lo, hi) ->
              say (printf "  Recent growth: n^%.2f … n^%.2f over scales %d … %d" lo hi start final)
          study modName testName scale points reference driftFailures = do
            point <- collect modName testName scale False
            let points' = point : points
                growth = case points of old:_ -> scaleGrowth old point; [] -> Nothing
                trend = stableGrowth points'
                reference' = case reference of
                  Nothing | scaleReliable point -> Just point
                  _ -> reference
            event "point" ["module" Aeson..= modName, "test" Aeson..= testName,
                           "scale" Aeson..= scale, "samples" Aeson..= length (pointSamples point),
                           "wall_mean_ms" Aeson..= scaleMean (pointValues "avg_wall_duration" point),
                           "relative_standard_error" Aeson..= scaleError point,
                           "reliable" Aeson..= scaleReliable point,
                           "observed_exponent" Aeson..= growth]
            say (formatPoint point growth)
            -- Start with the first measurable size, rather than a noisy tiny
            -- probe. Always recheck it before declaring the recent trend stable.
            checked <- case reference of
              Just ref | isJust trend || length points' `mod` 4 == 0 ->
                Just <$> checkReference modName testName ref
              _ -> return Nothing
            let failures = case checked of
                  Just True -> 0
                  Just False -> driftFailures + 1
                  Nothing -> driftFailures
                finish = endTest modName testName
            if isJust trend && checked == Just True then
              finish "stable" "Growth stable over the measured range" points' trend
            else if failures >= 3 then
              finish "inconclusive" "Reference measurements did not settle" points' Nothing
            else if length (takeWhile (not . scaleReliable) points') >= 20 then
              finish "inconclusive" "Timing stayed too short or noisy across 20 successive sizes" points' Nothing
            else case nextScale cap points' of
              Nothing -> finish "limited" "Next scale approaches the memory or integer limit" points' Nothing
              Just next -> study modName testName next points' reference' failures
      event "study" ["version" Aeson..= (2 :: Int), "host" Aeson..= host,
                     "max_memory_bytes" Aeson..= cap, "reserve_bytes" Aeson..= reserve,
                     "max_time_ms" Aeson..= duration, "start_scale" Aeson..= fromMaybe 1 (C.testStartScale opts),
                     "memory_protection" Aeson..= ("monitored; abrupt allocations can exceed the limit" :: String),
                     "path" Aeson..= path]
      say "Scaling study: adaptive range, 3–7 samples per size, one warmup per sample"
      say ("Safety limits: " ++ show duration ++ "ms, memory ceiling " ++ bytes cap)
      say "Memory protection is monitored; abrupt allocations can exceed the limit."
      say ("Results: " ++ path)
      outcome <- try $ flip finally (terminal "\ESC[?25h") $ do
        terminal "\ESC[?25l"
        forM_ tests $ \(modName, testName) -> do
          writeIORef chartData IM.empty
          say ("\nScaling " ++ modName ++ "." ++ testName)
          say "       scale          mean          min … max             time/scale       peak RSS       growth"
          study modName testName (fromMaybe 1 (C.testStartScale opts)) [] Nothing (0 :: Int)
            `finally` unless (C.testJson opts)
              (readIORef chartData >>= printScaleReport useColor (modName ++ "." ++ testName))
      case outcome of
        Right () -> finish "all selected studies finished" 0
        Left ex | Just (ScalingStopped reason) <- fromException ex ->
                    finish reason (if reason `elem` ["time limit reached", "memory limit reached", "available memory reserve reached"] then 0 else 2)
                | Just UserInterrupt <- fromException ex -> finish "interrupted" 130
                | Just async <- (fromException ex :: Maybe SomeAsyncException) -> throwIO async
                | otherwise -> finish (displayException (ex :: SomeException)) 1
    return code

milliseconds :: TimeSpec -> Double
milliseconds t = fromIntegral (toNanoSecs t) / 1000000

bytes :: Integer -> String
bytes n = printf "%.1f MiB" (fromIntegral n / 1048576 :: Double)

formatPoint :: ScalePoint -> Maybe Double -> String
formatPoint point growth =
    let values = pointValues "avg_wall_duration" point
        mean = fromMaybe 0 (scaleMean values)
        peak = maximum (0 : pointValues "peak_rss" point)
        trend :: String
        trend = maybe "inconclusive" (printf "n^%.2f") growth
    in printf "%12d  %10.3f ms  %9.3f … %9.3f ms  %10.3f µs  %10.1f MiB  %s"
         (pointScale point) mean (if null values then 0 else minimum values) (maximum (0:values))
         (mean * 1000 / fromIntegral (pointScale point)) (peak / 1048576) trend
