{-# LANGUAGE OverloadedStrings, ForeignFunctionInterface #-}
module PerfScaling
  ( validateScalingOptions, runScalingStudy, printScaleRecording
  , ScaleLimits(..), ScalingStopped(..), watchScaleProcess
  , ScaleRecording(..), ScaleSeries(..), readScaleRecording, scaleSeriesReason
  , ScalePoint(..), scaleMean, scaleError, scaleReliable, scaleNeedsSamples, scaleGrowth, stableGrowth
  , nextScale, MemoryStatus(..), scaleMemoryLimit
  , ScaleSample(..), addScaleEvent, scaleCharts, scaleSummary
  , Chart(..), ChartKind(..), ChartPoint(..), chartGuide, chartText, chartPixels, kittyImage, kittyTerminal
  ) where

import qualified Acton.CommandLineParser as C
import Acton.Testing (TestResult(..))
import TestPerf
import TestFormat (testColorApply, testColorBold)
import Codec.Compression.Zlib (compress)
import Control.Applicative ((<|>))
import TerminalSize (queryTermSize, termFitAnsiRight)
import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad
import Data.Char (chr, isAscii, isAlphaNum, isPrint)
import Data.Bits (bit, (.|.))
import Data.List (foldl', isPrefixOf, intercalate)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.IORef
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import Data.Word (Word64)
import Foreign (Ptr, alloca, allocaBytes, peek)
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CInt(..), CSize(..))
import System.Clock
import System.Environment (getEnvironment)
import System.Directory
import System.FilePath
import System.IO
import System.Process
import qualified System.Posix.IO as PIO
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
  | maybe False (< fromMaybe 1 (C.testStartScale opts)) (C.testEndScale opts) =
      Just "--end-scale must be at least --start-scale (default: 1)"
  | otherwise = Nothing

-- Memory limits ---------------------------------------------------------------

data MemoryStatus = MemoryStatus
  { memoryTotal :: Integer
  , memoryAvailable :: Integer
  , memoryProcess :: Integer
  } deriving (Eq, Show)

foreign import ccall safe "acton_perf_memory"
  c_perfMemory :: CInt -> Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> CString -> CSize -> IO CInt

-- | Current byte counts for the host and an optional process. Linux host values
-- respect visible cgroup limits; macOS headroom is a conservative VM estimate.
-- Nothing leaves memoryProcess at zero. A failed read, including an exited PID,
-- is an error: the caller must not continue a resource-limited run unguarded.
-- Sampling is best effort and cannot guarantee that an allocation avoids OOM.
readMemoryStatus :: Maybe Int -> IO (Either String MemoryStatus)
readMemoryStatus process
  | Just pid <- process, pid <= 0 || toInteger pid > toInteger (maxBound :: CInt) =
      return (Left "Invalid process ID for memory observation")
  | otherwise = alloca $ \total -> alloca $ \available -> alloca $ \used ->
      allocaBytes 512 $ \message -> do
        status <- c_perfMemory (maybe 0 fromIntegral process) total available used message 512
        if status /= 0
          then Left <$> peekCString message
          else do
            result <- MemoryStatus <$> (toInteger <$> peek total)
                                   <*> (toInteger <$> peek available)
                                   <*> (toInteger <$> peek used)
            return (Right result)

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

-- Sampling statistics ---------------------------------------------------------

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

-- Study runner ----------------------------------------------------------------

-- Each sample has its own process and the same amount of warmup and measured
-- work. The callback reuses the ordinary test runner's result handling.
runScalingStudy :: Bool -> C.GlobalOptions -> C.TestOptions -> FilePath -> Aeson.Object -> [(String, String, Maybe String)] -> Maybe ScaleRecording
                -> (ScaleLimits -> Int -> String -> String -> IO TestResult) -> IO Int
runScalingStudy useColor gopts opts directory host tests baseline runSample = do
    memory <- readMemoryStatus Nothing >>= either (ioError . userError) return
    (cap, reserve) <- either (ioError . userError) return
      (scaleMemoryLimit (fromMaybe (C.MemoryPercent 50) (C.testMaxMemory opts)) memory)
    start <- getTime Monotonic
    let duration = if C.testMaxTimeSet opts then C.testMaxTime opts else 3600000
        limits = ScaleLimits (start + fromNanoSecs (toInteger duration * 1000000)) cap reserve
    runId <- formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S.%qZ" <$> getCurrentTime
    createDirectoryIfMissing True directory
    tty <- hIsTerminalDevice stdout
    let say = unless (C.testJson opts) . putStrLn
        clean = map (\c -> if isPrint c then c else ' ')
        progress = not (C.testJson opts || C.quiet gopts || C.noProgress gopts)
        live = progress && (tty || C.tty gopts)
        terminal text = when live (putStr text >> hFlush stdout)
        clearProgress = terminal "\r\ESC[K"
        updateProgress text = when live $ do
          (_, cols) <- fromMaybe (24, 80) <$> queryTermSize
          terminal ("\r" ++ termFitAnsiRight (max 0 (cols - 1)) text ++ "\ESC[K")
    let runTests [] = return 0
        runTests ((modName, testName, implementation):rest) = do
          benchmarkStart <- getTime Monotonic
          recordedAt <- getCurrentTime
          let timestamp = formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S." recordedAt
                       ++ take 6 (formatTime defaultTimeLocale "%q" recordedAt) ++ "Z"
          let component = take 64 . map (\c -> if isAscii c && (isAlphaNum c || c `elem` ("._-" :: String)) then c else '_')
              name = component modName ++ "." ++ component testName
              path = directory </> (name ++ "__" ++ timestamp ++ "__" ++ maybe "unknown" (take 12) implementation) <.> "jsonl"
              old = baseline >>= M.lookup (modName, testName) . recordingTests
              oldData = maybe IM.empty seriesData old
              schedule = [n | (n, (True, _)) <- IM.toAscList oldData,
                              maybe True (n >=) (C.testStartScale opts),
                              maybe True (n <=) (C.testEndScale opts)]
              firstScale = case schedule of n:_ -> n; [] -> fromMaybe 1 (C.testStartScale opts)
          chartData <- newIORef IM.empty
          reached <- newIORef False
          (code, finished) <- bracket
            (PIO.openFd path PIO.WriteOnly PIO.defaultFileFlags { PIO.creat = Just 0o600, PIO.exclusive = True } >>= PIO.fdToHandle)
            hClose $ \journal -> do
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
                    targetReached <- readIORef reached
                    event "end" ["reason" Aeson..= reason, "elapsed_ms" Aeson..= milliseconds (now - benchmarkStart),
                                 "end_scale_reached" Aeson..= fmap (const targetReached) (C.testEndScale opts)]
                    forM_ (C.testEndScale opts) $ \target -> unless targetReached $
                      say ("Requested end scale " ++ show target ++ " was not reached")
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
                        -- A body shorter than one clock tick can measure zero.
                        -- Keep it as an unreliable probe and try larger sizes.
                        when (maybe True (< 0) (perfNumber obj "avg_wall_duration")) $
                          throwIO (ScalingStopped "wall time measurement unavailable")
                        forM_ old $ \previous -> do
                          let issue = seriesInfo previous >>= (`perfSamplingReason` info)
                          forM_ issue (throwIO . userError . ("Cannot compare: " ++))
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
                    return (KM.filterWithKey (\key _ -> key `elem` ["avg_wall_duration", "peak_rss", "mem_usage_delta_avg"]) obj)
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
                  study modName testName scale points reference driftFailures pending = do
                    point <- collect modName testName scale False
                    let points' = point : points
                        growth = case points of old:_ -> scaleGrowth old point; [] -> Nothing
                        targetReached = C.testEndScale opts == Just scale
                        automatic = not (isJust (C.testEndScale opts))
                        trend = if automatic || targetReached then stableGrowth points' else Nothing
                        allocated = mapM (`perfNumber` "mem_usage_delta_avg") (pointSamples point)
                        reference' = case reference of
                          Nothing | scaleReliable point -> Just point
                          _ -> reference
                    writeIORef reached targetReached
                    event "point" ["module" Aeson..= modName, "test" Aeson..= testName,
                                   "scale" Aeson..= scale, "samples" Aeson..= length (pointSamples point),
                                   "wall_mean_ms" Aeson..= scaleMean (pointValues "avg_wall_duration" point),
                                   "allocated_mean_bytes" Aeson..= (allocated >>= scaleMean),
                                   "allocated_min_bytes" Aeson..= fmap minimum allocated,
                                   "allocated_max_bytes" Aeson..= fmap maximum allocated,
                                   "relative_standard_error" Aeson..= scaleError point,
                                   "reliable" Aeson..= scaleReliable point,
                                   "observed_exponent" Aeson..= growth]
                    say (formatPoint point growth)
                    -- Start with the first measurable size, rather than a noisy tiny
                    -- probe. Always recheck it before declaring the recent trend stable.
                    checked <- case reference of
                      Just ref | not (isJust old) && (isJust trend || length points' `mod` 4 == 0) ->
                        Just <$> checkReference modName testName ref
                      _ -> return Nothing
                    let failures = case checked of
                          Just True -> 0
                          Just False -> driftFailures + 1
                          Nothing -> driftFailures
                        finish = endTest modName testName
                    if isJust old then case pending of
                      [] -> finish "compared" "Completed baseline sizes remeasured" points' Nothing
                      next:rest -> study modName testName next points' reference' failures rest
                    else if targetReached then
                      finish "range" ("Requested scale range measured" ++
                        if isJust trend && checked == Just True then "" else "; growth remains inconclusive")
                        points' (if checked == Just True then trend else Nothing)
                    else if isJust trend && checked == Just True then
                      finish "stable" "Growth stable over the measured range" points' trend
                    else if automatic && failures >= 3 then
                      finish "inconclusive" "Reference measurements did not settle" points' Nothing
                    else if automatic && length (takeWhile (not . scaleReliable) points') >= 20 then
                      finish "inconclusive" "Timing stayed too short or noisy across 20 successive sizes" points' Nothing
                    else case nextScale cap points' of
                      Nothing -> finish "limited" "Next scale approaches the memory or integer limit" points' Nothing
                      Just next ->
                        let chosen = case C.testEndScale opts of
                              Just target | scale < target -> min next target
                              _ -> next
                        in study modName testName chosen points' reference' failures []
              event "study" ["version" Aeson..= (3 :: Int), "host" Aeson..= host,
                             "module" Aeson..= modName, "test" Aeson..= testName,
                             "recorded_at" Aeson..= recordedAt, "run_id" Aeson..= runId,
                             "implementation_hash" Aeson..= implementation,
                             "baseline" Aeson..= fmap recordingPath baseline,
                             "max_memory_bytes" Aeson..= cap, "reserve_bytes" Aeson..= reserve,
                             "max_time_ms" Aeson..= duration, "start_scale" Aeson..= firstScale,
                             "end_scale" Aeson..= C.testEndScale opts,
                             "memory_protection" Aeson..= ("monitored; abrupt allocations can exceed the limit" :: String),
                             "path" Aeson..= path]
              say ((if isJust old then "Scaling comparison: recorded sizes"
                    else if isJust (C.testEndScale opts) then "Scaling study: requested range"
                    else "Scaling study: adaptive range")
                   ++ ", 3–7 samples per size, one warmup per sample")
              say ("Safety limits: " ++ show duration ++ "ms, memory ceiling " ++ bytes cap)
              forM_ (C.testEndScale opts) $ \target -> say ("Scale range: " ++ show firstScale ++ " … " ++ show target)
              say "Memory protection is monitored; abrupt allocations can exceed the limit."
              say ("Results: " ++ path)
              outcome <- try $ flip finally (terminal "\ESC[?25h") $ do
                terminal "\ESC[?25l"
                say ("\nScaling " ++ modName ++ "." ++ testName)
                forM_ baseline $ \previous -> say ("Baseline: " ++ clean (recordingPath previous))
                say "       scale          mean          min … max             time/scale      allocated       peak RSS       growth"
                study modName testName firstScale [] Nothing (0 :: Int) (drop 1 schedule)
                  `finally` unless (C.testJson opts) (do
                    points <- readIORef chartData
                    when (isJust old) $ do
                      say ("Current: " ++ takeFileName path)
                      forM_ baseline (say . ("Baseline: " ++) . clean . takeFileName . recordingPath)
                      let completed = length [n | n <- schedule, Just (True, _) <- [IM.lookup n points]]
                      say ("Replayed " ++ show completed ++ " of " ++ show (length schedule) ++ " recorded sizes")
                      forM_ (old >>= seriesReason) (say . ("Baseline: " ++) . clean)
                      forM_ baseline $ \previous -> say (maybe "Baseline recording is incomplete"
                        (("Baseline stopped: " ++) . clean) (recordingStopped previous))
                    printScaleReport useColor (modName ++ "." ++ testName) points oldData)
              code <- case outcome of
                Right () -> finish "benchmark finished" 0
                Left ex | Just (ScalingStopped reason) <- fromException ex ->
                            finish reason (if reason `elem` ["time limit reached", "memory limit reached", "available memory reserve reached"] then 0 else 2)
                        | Just UserInterrupt <- fromException ex -> finish "interrupted" 130
                        | Just async <- (fromException ex :: Maybe SomeAsyncException) -> throwIO async
                        | otherwise -> finish (displayException (ex :: SomeException)) 1
              return (code, case outcome of Right () -> True; _ -> False)
          if finished then runTests rest else return code
    runTests tests

milliseconds :: TimeSpec -> Double
milliseconds t = fromIntegral (toNanoSecs t) / 1000000

bytes :: Integer -> String
bytes n = printf "%.1f MiB" (fromIntegral n / 1048576 :: Double)

formatPoint :: ScalePoint -> Maybe Double -> String
formatPoint point growth =
    let values = pointValues "avg_wall_duration" point
        mean = fromMaybe 0 (scaleMean values)
        peak = maximum (0 : pointValues "peak_rss" point)
        allocated :: String
        allocated = maybe "unavailable" (\n -> printf "%.3g KiB" (n / 1024))
          (scaleMean =<< mapM (`perfNumber` "mem_usage_delta_avg") (pointSamples point))
        trend :: String
        trend = maybe "inconclusive" (printf "n^%.2f") growth
    in printf "%12d  %10.3f ms  %9.3f … %9.3f ms  %10.3f µs  %13s  %10.1f MiB  %s"
         (pointScale point) mean (if null values then 0 else minimum values) (maximum (0:values))
         (mean * 1000 / fromIntegral (pointScale point)) allocated (peak / 1048576) trend

-- Recordings ------------------------------------------------------------------

-- Only accepted curve samples are retained, with optional allocation data for
-- older recordings. Missing allocation measurements must not become zeros.
-- Diagnostics remain in the journal, not in the chart's in-memory history.
type ScaleData = IM.IntMap (Bool, [ScaleSample])

data ScaleSample = ScaleSample
    { sampleWall :: !Double
    , sampleAllocated :: !(Maybe Double)
    } deriving (Eq, Show)

data ScaleSeries = ScaleSeries
    { seriesData :: ScaleData
    , seriesInfo :: Maybe Aeson.Object
    , seriesIssue :: Maybe String
    , seriesReason :: Maybe String
    } deriving Show

data ScaleRecording = ScaleRecording
    { recordingPath :: FilePath
    , recordingHeader :: Aeson.Object
    , recordingTests :: M.Map (String, String) ScaleSeries
    , recordingStopped :: Maybe String
    } deriving Show

-- Retain chart values and measurement identity, never the raw diagnostics. Old
-- journals remain viewable even when their identity cannot support comparison.
readScaleRecording :: FilePath -> IO ScaleRecording
readScaleRecording path = withBinaryFile path ReadMode $ \input -> do
    records <- BL.split '\n' <$> BL.hGetContents input
    case records of
      first:rest -> do
        header <- decode 1 first
        case (KM.lookup "event" header, KM.lookup "version" header) of
          (Just (Aeson.String "study"), Just (Aeson.Number version)) | version `elem` [1, 2, 3] -> do
            let tests = case AesonTypes.parseMaybe key header of
                  Just name -> M.singleton name emptySeries
                  Nothing -> M.empty
            (reports, stopped) <- readRecords 2 tests Nothing rest
            return (ScaleRecording path header reports stopped)
          _ -> invalid 1 "expected a scaling study journal (version 1, 2 or 3)"
      [] -> invalid 1 "empty scaling journal"
  where
    emptySeries = ScaleSeries IM.empty Nothing Nothing Nothing
    key obj = (,) <$> obj Aeson..: "module" <*> obj Aeson..: "test"
    invalid line message = ioError (userError (path ++ ":" ++ show (line :: Int) ++ ": " ++ message))
    decode line = either (invalid line) return . Aeson.eitherDecode
    readRecords _ reports stopped [] = return (reports, stopped)
    readRecords line reports stopped (record:rest)
      | BL.null record && null rest = return (reports, stopped)
      | otherwise = line `seq` case Aeson.eitherDecode record of
          Left _ | null rest -> do
            hPutStrLn stderr (show path ++ ":" ++ show line ++ ": ignoring an unfinished final record")
            return (reports, stopped)
          Left message -> invalid line message
          Right event -> case KM.lookup "event" event of
            Just (Aeson.String kind) | kind `elem` ["sample_start", "sample", "point", "test_end"] -> do
              name <- either (invalid line) return (AesonTypes.parseEither key event)
              let previous = M.findWithDefault emptySeries name reports
                  points = addScaleEvent event (seriesData previous)
                  accepted = kind == "sample" && KM.lookup "accepted" event == Just (Aeson.Bool True)
                           && KM.lookup "reference" event /= Just (Aeson.Bool True)
                  info = case KM.lookup "result" event of
                    Just (Aeson.Object result) -> perfInfo result
                    _ -> Nothing
                  identityIssue = case info of
                    Nothing -> Just "performance identity is unavailable"
                    Just current
                      | KM.lookup "scale" current /= KM.lookup "scale" event -> Just "sample workload scale differs from its recorded size"
                      | KM.lookup "scaling" current /= Just (Aeson.Bool True)
                        || KM.lookup "loop" current /= Just (Aeson.Bool True) -> Just "sample is not a scaling measurement"
                      | otherwise -> perfSamplingReason current current
                          <|> (seriesInfo previous >>= (`perfSamplingReason` current))
              reason <- if kind == "test_end"
                then Just <$> either (invalid line) return (AesonTypes.parseEither (Aeson..: "reason") event)
                else return (seriesReason previous)
              let updated = previous
                    { seriesData = points, seriesReason = reason
                    , seriesInfo = seriesInfo previous <|> if accepted then info else Nothing
                    , seriesIssue = seriesIssue previous <|> if accepted then identityIssue else Nothing
                    }
                  reports' = M.insert name updated reports
              points `seq` seriesInfo updated `seq` seriesIssue updated `seq` reports' `seq` readRecords (line + 1) reports' stopped rest
            Just (Aeson.String "end") -> do
              reason <- either (invalid line) return (AesonTypes.parseEither (Aeson..: "reason") event)
              readRecords (line + 1) reports (Just reason) rest
            Just (Aeson.String "study") -> invalid line "unexpected second study header"
            _ -> readRecords (line + 1) reports stopped rest

scaleSeriesReason :: ScaleSeries -> ScaleSeries -> Maybe String
scaleSeriesReason old new = seriesIssue old <|> seriesIssue new <|>
    case (seriesInfo old, seriesInfo new) of
      (Just a, Just b) -> perfSamplingReason a b
      _ -> Just "performance identity is unavailable"

printScaleRecording :: Bool -> FilePath -> Maybe FilePath -> IO ()
printScaleRecording useColor path baselinePath = do
    current <- readScaleRecording path
    baseline <- mapM readScaleRecording baselinePath
    let reports = recordingTests current
        pairs = case baseline of
          Nothing -> M.map (\new -> (new, Nothing)) reports
          Just old -> M.intersectionWith (\new previous -> (new, Just previous)) reports (recordingTests old)
    forM_ baseline $ \old -> do
      when (M.null pairs) (ioError (userError "The recordings contain no matching benchmarks"))
      forM_ (M.toAscList pairs) $ \((modName, testName), (new, previous)) ->
        forM_ (previous >>= (`scaleSeriesReason` new)) $ \reason ->
          ioError (userError ("Cannot compare " ++ modName ++ "." ++ testName ++ ": " ++ reason))
      when (M.keys reports /= M.keys (recordingTests old)) $
        putStrLn "Only benchmarks present in both recordings are compared."
    putStrLn ("Recording: " ++ show path)
    forM_ baseline $ \old -> putStrLn ("Baseline: " ++ show (recordingPath old))
    when (all (IM.null . seriesData) (M.elems reports)) $
      putStrLn "No accepted measurements in this recording."
    forM_ (M.toAscList pairs) $ \((modName, testName), (new, old)) -> do
      forM_ baseline $ \previous -> do
        putStrLn ("Current: " ++ clean (takeFileName path))
        putStrLn ("Baseline: " ++ clean (takeFileName (recordingPath previous)))
      printScaleReport useColor (modName ++ "." ++ testName) (seriesData new) (maybe IM.empty seriesData old)
      endpointStatus "  " current new
      forM_ baseline $ \recording -> forM_ old (endpointStatus "  Baseline: " recording)
      forM_ (seriesReason new) (putStrLn . ("  " ++) . clean)
      forM_ (old >>= seriesReason) (putStrLn . ("  Baseline: " ++) . clean)
    putStrLn (maybe "Recording is incomplete: no completion event was recorded." (("Stopped: " ++) . clean) (recordingStopped current))
    forM_ baseline $ \old -> putStrLn
      (maybe "Baseline is incomplete: no completion event was recorded." (("Baseline stopped: " ++) . clean) (recordingStopped old))
  where
    clean = map (\c -> if isPrint c then c else ' ')
    endpointStatus prefix recording series =
      forM_ (AesonTypes.parseMaybe (Aeson..: "end_scale") (recordingHeader recording)) $ \target ->
        unless (maybe False fst (IM.lookup target (seriesData series))) $
          putStrLn (prefix ++ "Requested end scale " ++ show target ++ " was not reached")

addScaleEvent :: Aeson.Object -> ScaleData -> ScaleData
addScaleEvent event points = fromMaybe points $ do
    n <- KM.lookup "scale" event >>= AesonTypes.parseMaybe Aeson.parseJSON
    if n <= 0 then Nothing else case KM.lookup "event" event of
      Just (Aeson.String "point") -> Just (IM.adjust (\(_, xs) -> (True, xs)) n points)
      Just (Aeson.String "sample")
        | KM.lookup "accepted" event == Just (Aeson.Bool True)
        , KM.lookup "reference" event /= Just (Aeson.Bool True)
        , Just (Aeson.Object result) <- KM.lookup "result" event -> do
            wall <- perfNumber result "avg_wall_duration"
            let allocated = case perfNumber result "mem_usage_delta_avg" of
                  Just bytes | bytes >= 0 -> Just bytes
                  _ -> Nothing
            if wall < 0 then Nothing else
              let sample = ScaleSample wall allocated
              in sample `seq` Just (IM.insertWith
                (\(_, new) (done, old) -> (done, new ++ old)) n (False, [sample]) points)
      _ -> Nothing

-- Terminal charts -------------------------------------------------------------

data ChartPoint = ChartPoint
    { chartScale :: Double
    , chartMinimum :: Double
    , chartMean :: Double
    , chartMaximum :: Double
    , chartComplete :: Bool
    } deriving (Eq, Show)

data ChartKind = WallTime | TimePerScale | Allocated deriving (Eq, Show)
data Chart = Chart ChartKind [ChartPoint] [ChartPoint] deriving (Eq, Show)

chartStyle :: ChartKind -> (String, [Int])
chartStyle WallTime = ("Wall time (ms)", [56, 189, 248])
chartStyle TimePerScale = ("Time / scale (µs)", [192, 132, 252])
chartStyle Allocated = ("Allocated (KiB)", [244, 114, 182])

scaleSummary :: ScaleData -> String
scaleSummary points = case (IM.lookupMin points, IM.lookupMax points) of
    (Just (lo, _), Just (hi, _)) ->
      count complete "size" ++ (if partial == 0 then "" else " + " ++ show partial ++ " partial")
      ++ " · " ++ count samples "curve sample" ++ " · scale " ++ show lo ++ " … " ++ show hi
    _ -> "No measurements"
  where
    complete = length (filter fst (IM.elems points))
    partial = IM.size points - complete
    samples = sum (map (length . snd) (IM.elems points))
    count n noun = show n ++ " " ++ noun ++ if n == 1 then "" else "s"

-- An illustrative proportionality line, not a fit or a complexity verdict.
-- Anchor at the largest completed size; partial samples cannot set the guide.
chartGuide :: Chart -> [(Double, Double)]
chartGuide (Chart WallTime points _) = case (points, reverse (filter chartComplete points)) of
    (first:_, anchor:_) | chartScale first < chartScale anchor ->
      [(chartScale first, chartMean anchor * (chartScale first / chartScale anchor)),
       (chartScale anchor, chartMean anchor)]
    _ -> []
chartGuide _ = []

scaleCharts :: ScaleData -> ScaleData -> [Chart]
scaleCharts points baseline =
    [ chart WallTime (\_ sample -> wall sample)
    , chart TimePerScale (\n sample -> (* (1000 / n)) <$> wall sample)
    , chart Allocated (\_ sample -> (/ 1024) <$> sampleAllocated sample)
    ]
  where
    -- A logarithmic time chart cannot represent a zero clock reading. Omit
    -- the whole point, not individual samples that would bias its statistics.
    wall sample = if sampleWall sample > 0 then Just (sampleWall sample) else Nothing
    chart title value = Chart title (series value points) (series value baseline)
    series value dataPoints =
      [ ChartPoint scale (minimum ys) (sum ys / fromIntegral (length ys)) (maximum ys) done
      | (n, (done, xs)) <- IM.toAscList dataPoints, not (null xs)
      , let scale = fromIntegral n
      , Just ys <- [mapM (value scale) xs] ]

-- Be conservative without querying stdin or consuming the user's keystrokes.
-- Multiplexers get text until their graphics passthrough is supported here.
kittyTerminal :: Bool -> [(String, String)] -> Bool
kittyTerminal tty env = tty && not multiplexer && term /= "dumb"
    && (term `elem` ["xterm-kitty", "xterm-ghostty"] || program `elem` ["kitty", "ghostty"])
  where
    term = fromMaybe "" (lookup "TERM" env)
    program = fromMaybe "" (lookup "TERM_PROGRAM" env)
    multiplexer = any (\key -> maybe False (not . null) (lookup key env)) ["TMUX", "STY"]
               || any (`isPrefixOf` term) ["screen", "tmux"]

printScaleReport :: Bool -> String -> ScaleData -> ScaleData -> IO ()
printScaleReport useColor name points baseline = when (not (IM.null points && IM.null baseline)) $ do
    tty <- hIsTerminalDevice stdout
    (rows, cols) <- if tty then fromMaybe (24, 80) <$> queryTermSize else return (24, 80)
    graphics <- kittyTerminal tty <$> getEnvironment
    -- Leave room for the labels and never move above the visible screen.
    when (rows >= 10 && cols >= 40) $ do
      let width = min 96 (cols - 13)
          height = min 12 (rows - 6)
      let paint = testColorApply useColor
      putStrLn ("\n" ++ paint [testColorBold] ("Scaling charts: " ++ map (\c -> if isPrint c then c else ' ') name))
      putStrLn (scaleSummary points)
      when (not (IM.null baseline)) (putStrLn ("Baseline: " ++ scaleSummary baseline))
      putStrLn "Logarithmic axes unless labelled; means and min–max ranges; hollow markers = partial points."
      when (any (any ((== 0) . sampleWall) . snd) (IM.elems points ++ IM.elems baseline)) $
        putStrLn "Time charts omit sizes with zero clock readings (below resolution); allocation data is retained."
      let charts = scaleCharts points baseline
      forM_ [chart | chart@(Chart _ current old) <- charts, not (null current && null old)] $ \chart@(Chart kind _ old) -> do
        let rgb = snd (chartStyle kind)
            accent = "\ESC[38;2;" ++ intercalate ";" (map show rgb) ++ "m"
            style row line
              | row == 0 = paint [testColorBold, accent] line
              | row <= height = take 11 line ++ paint [accent] (drop 11 line)
              | otherwise = line
        when (not (null old)) $
          putStrLn (paint [accent] "  ● ━ current" ++ "    " ++ paint [if graphics then "\ESC[38;2;251;146;60m" else accent] "◆ ┄ baseline" ++ "    ◈ overlapping points")
        when (linearAxis chart) (putStrLn "  Linear allocation axis, including zero.")
        putStr (unlines (zipWith style [0..] (chartText graphics width height chart)))
        when graphics $ do
          -- The text reserves space first, including when output scrolls.
          -- C=1 keeps the image from moving the cursor; return below the axes.
          -- End any interrupted graphics command before restoring the cursor.
          bracket_ (putStr ("\ESC[" ++ show (height + 2) ++ "A\ESC[12G"))
            (putStr ("\ESC\\\ESC[" ++ show (height + 2) ++ "B\r") >> hFlush stdout)
            (BS.hPut stdout (kittyImage width height (chartPixels useColor width height chart)))
        case chartGuide chart of
          [_, (n, _)] -> putStrLn (paint ["\ESC[38;2;251;191;36m"] ("  ┄ time ∝ scale · anchored at scale " ++ printf "%.0f" n))
          _ -> return ()
        putStrLn ""
      putStrLn "Flat time/scale means roughly linear time over these sizes."
      putStrLn "Allocated bytes cover the measured region, not retained structure size."
      when (any (\(Chart kind current old) -> kind == Allocated && null current && null old) charts) $
        putStrLn "Allocation measurements are unavailable in this recording."
      hFlush stdout

-- Positions are fractions of the plot area. A decade grid keeps small timing
-- differences from looking like large changes. Constant series still have range.
layout :: Chart -> ([(Double, String)], [(Double, String)], [ChartPoint], [ChartPoint], [(Double, Double)])
layout (Chart _ [] []) = ([], [], [], [], [])
layout chart@(Chart _ points baseline) = (ticks xbounds, yticks, map project points, map project baseline, guide)
  where
    xbounds = bounds (map chartScale (points ++ baseline))
    values = concatMap (\p -> [chartMinimum p, chartMaximum p]) (points ++ baseline)
    ybounds = if linearAxis chart then (0, if maximum values > 0 then maximum values else 1) else bounds values
    ypos value = if linearAxis chart then value / snd ybounds else position ybounds value
    yticks = if linearAxis chart
      then [(n, printf "%.3g" (n * snd ybounds)) | n <- [0, 0.25, 0.5, 0.75, 1]]
      else ticks ybounds
    bounds values =
      let lo = logBase 10 (minimum values); hi = logBase 10 (maximum values)
          lower = fromIntegral (floor (lo + 1e-10) :: Int)
          upper = fromIntegral (ceiling (hi - 1e-10) :: Int)
      in if lower >= upper then (lo - 0.5, hi + 0.5) else (lower, upper)
    position (lo, hi) value = (logBase 10 value - lo) / (hi - lo)
    ticks limits@(lo, hi) =
      [ (position limits (10 ** fromIntegral n), if n >= 0 && n <= 3 then show (10^n :: Int) else "1e" ++ show n)
      | n <- [ceiling lo, ceiling lo + max 1 (ceiling ((hi - lo) / 4)) .. floor hi :: Int] ]
    -- Clip before rasterization: clamping every pixel would draw a false line
    -- along the bottom edge wherever the guide lies below the measured range.
    guide = case [(position xbounds n, ypos t) | (n, t) <- chartGuide chart] of
      [(ax, ay), (bx, by)] | bx > ax && by > ay ->
        let slope = (by - ay) / (bx - ax)
            left = max ax (ax - ay / slope)
            right = min bx (ax + (1 - ay) / slope)
        in [(u, ay + (u - ax) * slope) | left < right, u <- [left, right]]
      _ -> []
    project p = p { chartScale = position xbounds (chartScale p)
                  , chartMinimum = ypos (chartMinimum p)
                  , chartMean = ypos (chartMean p)
                  , chartMaximum = ypos (chartMaximum p) }

linearAxis :: Chart -> Bool
linearAxis (Chart Allocated points baseline) = any ((== 0) . chartMinimum) (points ++ baseline)
linearAxis _ = False

chartText :: Bool -> Int -> Int -> Chart -> [String]
chartText graphics width height chart@(Chart kind raw _) =
    [heading] ++
    [ pad 10 (fromMaybe "" (lookup row labels)) ++ "│" ++ plotRow row | row <- [0..height-1] ] ++
    [replicate 10 ' ' ++ "└" ++ replicate width '─', "     scale " ++ xlabels]
  where
    (xticks, yticks, points, baseline, guide) = layout chart
    title = "  ◆ " ++ fst (chartStyle kind)
    lastValue = case reverse raw of
      p:_ -> "last " ++ number (chartMean p) ++ if chartComplete p then "" else " (partial)"
      _ -> ""
    heading = title ++ if length title + length lastValue + 3 <= width + 11
                        then replicate (width + 11 - length title - length lastValue) ' ' ++ lastValue else ""
    number n | n >= 1e5 || n < 0.001 = printf "%.2e" n
             | otherwise = printf "%.*f" (max 0 (2 - floor (logBase 10 n)) :: Int) n
    labels = [(y height value, label) | (value, label) <- yticks]
    -- Give the end ticks priority and leave a gap between labels on narrow
    -- terminals. Overwriting a neighbouring label could change its number.
    xlabels = foldl' place (replicate width ' ') (case xticks of [] -> []; first:rest -> first : reverse rest)
    place line (value, label) =
      let start = max 0 (min (width - length label) (x width value - length label `div` 2))
          occupied = take (length label + 2) (drop (max 0 (start - 1)) line)
      in if any (/= ' ') occupied then line
         else take start line ++ label ++ drop (start + length label) line
    dots = drawing (2 * width) (4 * height) 2 4 (segments points)
    oldDots = drawing (2 * width) (4 * height) 2 4 (segments baseline)
    guideDots = drawing width height 1 1 (zip guide (drop 1 guide))
    marks = IM.fromList [(y height (chartMean p) * width + x width (chartScale p),
                         if chartComplete p then '●' else '○') | p <- points]
    oldMarks = IM.fromList [(y height (chartMean p) * width + x width (chartScale p), if chartComplete p then '◆' else '◇') | p <- baseline]
    mark i = case (IM.lookup i marks, IM.lookup i oldMarks) of
      (Just '●', Just _) -> Just '◈'
      (Just c, _) -> Just c
      (_, old) -> old
    plotRow row
      | graphics = replicate width ' '
      | otherwise = [fromMaybe (braille col row) (mark (row * width + col)) | col <- [0..width-1]]
    braille col row =
      let bits = foldl' (.|.) 0
            [ bit b | (dx, dy, b) <- [(0,0,0),(0,1,1),(0,2,2),(1,0,3),(1,1,4),(1,2,5),(0,3,6),(1,3,7)]
                    , let i = (row * 4 + dy) * width * 2 + col * 2 + dx
                    , IM.member i dots || ((col + row) `mod` 3 /= 2 && IM.member i oldDots) ]
      in if bits == 0 && col `mod` 3 == 0 && IM.member (row * width + col) guideDots then '·'
         else chr (0x2800 + bits)
    pad n s = replicate (max 0 (n - length s)) ' ' ++ s

x :: Int -> Double -> Int
x size value = max 0 (min (size - 1) (round (value * fromIntegral (size - 1))))

y :: Int -> Double -> Int
y size value = x size (1 - value)

-- One set of line and range geometry feeds both terminal representations.
segments :: [ChartPoint] -> [((Double, Double), (Double, Double))]
segments points = zip centres (drop 1 centres) ++
    [((chartScale p, chartMinimum p), (chartScale p, chartMaximum p)) | p <- points]
  where centres = [(chartScale p, chartMean p) | p <- points]

-- Cell centres align raster marks with the surrounding terminal text.
drawing :: Int -> Int -> Int -> Int -> [((Double, Double), (Double, Double))] -> IM.IntMap ()
drawing width height cellWidth cellHeight edges = IM.fromList
    [(py * width + px, ()) | (a, b) <- edges, (px, py) <- line (pixel a) (pixel b)]
  where
    pixel (u, v) = (round (u * fromIntegral (width - cellWidth) + fromIntegral (cellWidth - 1) / 2),
                    round ((1 - v) * fromIntegral (height - cellHeight) + fromIntegral (cellHeight - 1) / 2))
    line (ax, ay) (bx, by) =
      let steps = max 1 (max (abs (bx - ax)) (abs (by - ay)))
      in [(ax + round (fromIntegral (bx - ax) * fromIntegral i / fromIntegral steps :: Double),
           ay + round (fromIntegral (by - ay) * fromIntegral i / fromIntegral steps :: Double)) | i <- [0..steps]]

chartPixels :: Bool -> Int -> Int -> Chart -> BS.ByteString
chartPixels useColor cols rows chart@(Chart kind _ _) = BS.pack (concatMap pixel [0..width*height-1])
  where
    width = cols * 8
    height = rows * 16
    (xticks, yticks, points, baseline, guide) = layout chart
    -- A seven-pixel horizontal inset keeps the six-pixel highlight inside
    -- the image even when the latest size falls exactly on an axis limit.
    lines = drawing width height 14 16 (segments points)
    oldLines = drawing width height 14 16 (segments baseline)
    guideLine = drawing width height 14 16 (zip guide (drop 1 guide))
    oldMarks = IM.fromList
      [ ((cy + dy) * width + cx + dx, ())
      | p <- baseline
      , let cx = round (chartScale p * fromIntegral (width - 14) + 6.5)
            cy = round ((1 - chartMean p) * fromIntegral (height - 16) + 7.5)
      , dx <- [-5..5], dy <- [-5..5], (if chartComplete p then abs dx + abs dy <= 5 else abs dx + abs dy `elem` [4, 5])
      , cx + dx >= 0, cx + dx < width, cy + dy >= 0, cy + dy < height ]
    marks = IM.fromList
      [ ((cy + dy) * width + cx + dx, alpha)
      | (index, p) <- zip [0..] points
      , let cx = round (chartScale p * fromIntegral (width - 14) + 6.5)
            cy = round ((1 - chartMean p) * fromIntegral (height - 16) + 7.5)
            radius = if index == length points - 1 then 6 else 3
      , dx <- [-radius..radius], dy <- [-radius..radius]
      , cx + dx >= 0, cx + dx < width, cy + dy >= 0, cy + dy < height
      , let distance = dx*dx + dy*dy
      , distance <= 9 || (radius == 6 && distance >= 25 && distance <= 36)
      , let alpha = if distance > 9 then 150 else if distance <= 4 && not (chartComplete p) then 0 else 255 ]
    gridX = [round (v * fromIntegral (width - 14) + 6.5) | (v, _) <- xticks]
    gridY = [round ((1 - v) * fromIntegral (height - 16) + 7.5) | (v, _) <- yticks]
    pixel i = case IM.lookup i marks of
      Just alpha -> accent alpha
      Nothing | IM.member i lines -> accent 255
              | IM.member i oldMarks || (IM.member i oldLines && (i `mod` width - i `div` width) `mod` 12 < 6) -> recorded
              | IM.member i guideLine && (i `mod` width - i `div` width) `mod` 12 < 6 -> gold
              | i `mod` width `elem` gridX || i `div` width `elem` gridY -> [128, 128, 128, 50]
              | otherwise -> [0, 0, 0, 0]
    accent alpha = map fromIntegral (if useColor then snd (chartStyle kind) else [190, 190, 190]) ++ [alpha]
    recorded = (if useColor then [251, 146, 60] else [235, 235, 235]) ++ [255]
    gold = (if useColor then [251, 191, 36] else [150, 150, 150]) ++ [210]

-- Direct transmission works without a shared filesystem (including over SSH).
-- Kitty accepts zlib-compressed RGBA; SVG or a PNG encoder is unnecessary.
kittyImage :: Int -> Int -> BS.ByteString -> BS.ByteString
kittyImage cols rows pixels = chunks metadata encoded
  where
    encoded = Base64.encode (BL.toStrict (compress (BL.fromStrict pixels)))
    metadata = BC.pack ("a=T,f=32,o=z,s=" ++ show (cols * 8) ++ ",v=" ++ show (rows * 16)
                        ++ ",c=" ++ show cols ++ ",r=" ++ show rows ++ ",C=1,")
    chunks prefix bytes =
      let (chunk, rest) = BS.splitAt 4096 bytes
          more = if BS.null rest then "0" else "1"
          command = "\ESC_G" <> prefix <> "q=2,m=" <> more <> ";" <> chunk <> "\ESC\\"
      in if BS.null rest then command else command <> chunks "" rest
