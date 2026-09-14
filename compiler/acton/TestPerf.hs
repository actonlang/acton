module TestPerf
  ( testPerfData
  , perfNumber
  , perfMetrics
  , perfStatKey
  , perfInfo
  , perfCounterInfo
  , perfHostReason, perfSamplingReason
  , perfComparisonReason
  , perfBaselineScale
  , perfComparable
  , aggregatePerfRuns
  , perfPairedCount
  , perfMeanInterval
  , perfJson
  ) where

import Acton.Testing (TestResult(..))
import Control.Monad (guard)
import Data.Maybe (isJust, mapMaybe)
import Data.List (find, sort)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKM

-- | Only complete, successful measurements can be recorded or compared.
testPerfData :: TestResult -> Maybe Aeson.Object
testPerfData res
  | not (trComplete res) || trSuccess res /= Just True || isJust (trException res)
    || trSkipped res || trCached res || trSnapshotUpdated res || trNumIterations res <= 0 = Nothing
  | otherwise = case trRaw res of
      Aeson.Object obj -> Just obj
      _ -> Nothing

perfNumber :: Aeson.Object -> String -> Maybe Double
perfNumber obj key = do
    value <- AesonKM.lookup (AesonKey.fromString key) obj >>= AesonTypes.parseMaybe Aeson.parseJSON
    guard (finite value)
    return value

finite :: Double -> Bool
finite x = not (isNaN x || isInfinite x)

-- | Sample series, display labels and base units shared by the table and JSON.
perfMetrics :: [(String, String, String)]
perfMetrics =
    [ ("wall_duration", "wall time", "ms")
    , ("cpu_user", "CPU user", "ms")
    , ("cpu_system", "CPU system", "ms")
    , ("instructions", "instructions", "count")
    , ("cycles", "cycles", "count")
    , ("ipc", "IPC", "ratio")
    , ("mem_usage_delta", "allocated", "B")
    , ("gc_duration", "GC time", "ms")
    ]

-- Preserve the original mean and duration outlier keys in saved measurements.
perfStatKey :: String -> String -> String
perfStatKey "duration" "outlier_count" = "outlier_count"
perfStatKey metric "avg"
  | metric `elem` ["mem_usage_delta", "non_gc_mem_usage_delta"] = metric ++ "_avg"
perfStatKey metric stat = stat ++ "_" ++ metric

perfInfo :: Aeson.Object -> Maybe Aeson.Object
perfInfo = objectField "perf_info"

perfCounterInfo :: Aeson.Object -> Maybe Aeson.Object
perfCounterInfo = objectField "counter_info"

objectField :: String -> Aeson.Object -> Maybe Aeson.Object
objectField key obj = case AesonKM.lookup (AesonKey.fromString key) obj of
    Just (Aeson.Object info) -> Just info
    _ -> Nothing

-- Every delta needs the same workload and machine. Counter accounting scope
-- is an additional constraint for hardware measurements only.
perfComparable :: String -> Aeson.Object -> Aeson.Object -> Bool
perfComparable metric old new = not (isJust (perfComparisonReason metric old new))

perfComparisonReason :: String -> Aeson.Object -> Aeson.Object -> Maybe String
perfComparisonReason metric old new = case (perfInfo old, perfInfo new) of
    (Nothing, _) -> Just "baseline has no performance identity; record a new baseline"
    (_, Nothing) -> Just "current performance identity is unavailable"
    (Just a, Just b) -> case perfSamplingReason a b of
      Just reason -> Just reason
      Nothing -> case metadataReason ["scale"] a b of
        Just reason -> Just reason
        Nothing
          | metric `elem` ["instructions", "cycles", "ipc"] -> case (perfCounterInfo old, perfCounterInfo new) of
              (Just ca, Just cb) -> metadataReason ["version", "backend", "scope"] ca cb
              _ -> Just "hardware counter scope is unavailable"
          | otherwise -> Nothing

-- Scaling curves compare many sizes, while retaining the same environment and
-- sampling contract. Workload scale is checked separately for fixed-size deltas.
perfSamplingReason :: Aeson.Object -> Aeson.Object -> Maybe String
perfSamplingReason old new
  | scaling old /= scaling new = Just "measurement sampling mode differs"
  | otherwise = metadataReason (hostKeys ++ ["loop", "workers"]) old new
  where
    scaling info = AesonKM.lookup (AesonKey.fromString "scaling") info == Just (Aeson.Bool True)

perfHostReason :: Aeson.Object -> Aeson.Object -> Maybe String
perfHostReason = metadataReason hostKeys

-- Select the recorded workload scale before launching the process. The completed
-- run must still pass the actual worker-count and loop checks above.
perfBaselineScale :: Aeson.Object -> Aeson.Object -> Maybe Int
perfBaselineScale baseline currentInfo = do
    old <- perfInfo baseline
    guard (AesonKM.lookup (AesonKey.fromString "scaling") old /= Just (Aeson.Bool True))
    guard (not (isJust (metadataReason hostKeys old currentInfo)))
    guard (AesonKM.lookup (AesonKey.fromString "loop") old == Just (Aeson.Bool True))
    value <- AesonKM.lookup (AesonKey.fromString "scale") old >>= AesonTypes.parseMaybe Aeson.parseJSON
    guard (value > 0)
    return value

hostKeys :: [String]
hostKeys = ["machine", "version", "build", "tags", "gc"]

metadataReason :: [String] -> Aeson.Object -> Aeson.Object -> Maybe String
metadataReason keys old new = do
    key <- find (not . matches) keys
    return (label key ++ if valid key old && valid key new then " differs" else " is unavailable")
  where
    value obj key = AesonKM.lookup (AesonKey.fromString key) obj
    matches key = valid key old && valid key new && value old key == value new key
    valid key obj = case key of
      "build" -> case value obj key of
        Just (Aeson.Object build) -> all (\k -> case value build k of
            Just (Aeson.String s) -> k == "cpu" || s /= mempty
            _ -> False) ["optimize", "target", "cpu"]
          && all (\k -> case value build k of
               Just (Aeson.Bool _) -> True
               _ -> False) ["no_threads", "db", "no_dbp"]
        _ -> False
      "tags" -> isJust (value obj key >>= (AesonTypes.parseMaybe Aeson.parseJSON :: Aeson.Value -> Maybe [String]))
      "loop" -> case value obj key of
        Just (Aeson.Bool _) -> True
        _ -> False
      _ | key `elem` ["scale", "workers"] -> case perfNumber obj key of
            Just n -> n >= (if key == "workers" then 0 else 1) && n == fromInteger (floor n)
            _ -> False
        | otherwise -> case value obj key of
            Just (Aeson.String s) -> s /= mempty
            _ -> False
    label key = case key of
      "machine" -> "machine identity"
      "version" -> "measurement version"
      "build" -> "build mode"
      "tags" -> "input tags"
      "gc" -> "GC policy"
      "scale" -> "workload scale"
      "loop" -> "measurement loop usage"
      "workers" -> "runtime worker count"
      _ -> "hardware counter " ++ key

-- | Give each fresh process equal weight, regardless of its inner loop count.
-- Retain the ordered process measurements so paired comparisons survive recording.
aggregatePerfRuns :: String -> [TestResult] -> Maybe TestResult
aggregatePerfRuns comparison runs = do
    first <- case runs of
      r:_ -> Just r
      [] -> Nothing
    guard (not (null comparison))
    objects <- mapM testPerfData runs
    obj <- testPerfData first
    guard (all (\r -> trModule r == trModule first && trName r == trName first
                  && trNumFailures r == 0 && trNumErrors r == 0 && trNumSkipped r == 0) runs)
    guard (all (\other -> perfComparable "wall_duration" obj other
                  && not (AesonKM.member (key "process_samples") other)) objects)
    wall <- values "wall_duration" obj objects
    wallStats <- summarize "wall_duration" wall
    let otherStats = mapMaybe (\metric -> values metric obj objects >>= summarize metric)
          (filter (/= "wall_duration") metrics)
        sums keys objs = AesonKM.fromList $ mapMaybe (\name -> do
          samples <- mapM (\o -> perfNumber o name) objs
          let value = sum samples
          guard (finite value)
          return (key name, Aeson.toJSON value)) keys
        infoKeys = ["time_budget_ms", "warmup_duration_ms", "measurement_ms", "measurement_duration_ms"]
        info = case mapM perfInfo objects of
          Just infos@(i:_) -> sums infoKeys infos `AesonKM.union` remove infoKeys i
          _ -> AesonKM.empty
        peak = case mapM (\o -> perfNumber o "peak_rss") objects of
          Just samples -> AesonKM.singleton (key "peak_rss") (Aeson.toJSON (maximum samples))
          Nothing -> AesonKM.empty
        counters = case perfCounterInfo obj of
          Just info | AesonKM.lookup (key "status") info == Just (Aeson.toJSON ("available" :: String))
                    , not (all (\metric -> isJust (values metric obj objects)) ["instructions", "cycles", "ipc"]) ->
            AesonKM.singleton (key "counter_info") (Aeson.Object (AesonKM.insert (key "status")
              (Aeson.toJSON ("incomplete process counters" :: String)) info))
          _ -> AesonKM.empty
        fields = AesonKM.fromList
          [ (key "comparison_id", Aeson.toJSON comparison)
          , (key "process_samples", Aeson.toJSON objects)
          , (key "num_iterations", Aeson.toJSON (length runs))
          , (key "test_duration", Aeson.toJSON (sum (map trTestDuration runs)))
          , (key "perf_info", Aeson.Object info)
          ]
        raw = foldr AesonKM.union AesonKM.empty (fields : wallStats : otherStats ++
          [sums ["loop_iterations"] objects, peak, counters, remove ("sequence" : "peak_rss" : "loop_iterations" : statKeys) obj])
    return first { trRaw = Aeson.Object raw, trNumIterations = length runs
                 , trTestDuration = sum (map trTestDuration runs) }
  where
    key = AesonKey.fromString
    metrics = [metric | (metric, _, _) <- perfMetrics] ++ ["duration", "non_gc_mem_usage_delta"]
    statKeys = [perfStatKey metric stat | metric <- metrics,
      stat <- ["avg", "min", "max", "median", "q1", "q3", "stdev", "outlier_count"]]
    remove keys obj = foldr (AesonKM.delete . key) obj keys
    values metric first objects = do
      guard (all (perfComparable metric first) objects)
      mapM (\obj -> perfNumber obj (perfStatKey metric "avg")) objects

summarize :: String -> [Double] -> Maybe Aeson.Object
summarize metric samples = do
    guard (not (null samples))
    let n = length samples
        ordered = sort samples
        mean = sum (map (/ fromIntegral n) samples)
        quantile p = let pos = fromIntegral (n - 1) * p
                         lo = floor pos
                         a = ordered !! lo
                         b = ordered !! min (lo + 1) (n - 1)
                     in a + (pos - fromIntegral lo) * (b - a)
        q1 = quantile 0.25
        q3 = quantile 0.75
        outliers = length [v | v <- samples, v < q1 - 1.5 * (q3 - q1) || v > q3 + 1.5 * (q3 - q1)]
        stats = [("avg", mean), ("min", head ordered), ("max", last ordered)
                , ("median", quantile 0.5), ("q1", q1), ("q3", q3), ("outlier_count", fromIntegral outliers)] ++
          [("stdev", sqrt (sum [(v - mean) ^ (2 :: Int) | v <- samples] / fromIntegral (n - 1))) | n > 1]
    guard (all (finite . snd) stats)
    return (AesonKM.fromList [(AesonKey.fromString (perfStatKey metric stat), Aeson.toJSON value) | (stat, value) <- stats])

processSamples :: Aeson.Object -> Maybe [Aeson.Object]
processSamples obj = do
    raw <- AesonKM.lookup (AesonKey.fromString "process_samples") obj
    samples <- AesonTypes.parseMaybe Aeson.parseJSON raw
    n <- perfNumber obj "num_iterations"
    guard (not (null samples) && n == fromIntegral (length samples))
    return samples

-- | Pair only measurements produced together, in the runner's pair order.
perfPairedCount :: Aeson.Object -> Aeson.Object -> Maybe Int
perfPairedCount old new = do
    a <- AesonKM.lookup (AesonKey.fromString "comparison_id") old
    b <- AesonKM.lookup (AesonKey.fromString "comparison_id") new
    guard (a == b && case a of Aeson.String s -> s /= mempty; _ -> False)
    as <- processSamples old
    bs <- processSamples new
    guard (length as == length bs && perfComparable "wall_duration" old new)
    guard (all (perfComparable "wall_duration" old) as && all (perfComparable "wall_duration" new) bs)
    return (length as)

-- | Approximate 95% interval in the metric's units. Paired runs use differences
-- between process means; historical recordings use an unpaired Welch interval.
perfMeanInterval :: String -> Aeson.Object -> Aeson.Object -> Maybe (Double, Double)
perfMeanInterval metric old new = do
    guard (perfComparable metric old new)
    case perfPairedCount old new of
      Just n -> do
        guard (n >= 2)
        as <- processSamples old >>= mapM processMean
        bs <- processSamples new >>= mapM processMean
        let differences = zipWith (-) bs as
            mean = sum differences / fromIntegral n
            variance = sum [(d - mean) ^ (2 :: Int) | d <- differences] / fromIntegral (n - 1)
            half = critical95 (fromIntegral (n - 1)) * sqrt (variance / fromIntegral n)
        guard (all finite [mean, half])
        return (mean - half, mean + half)
      Nothing -> welchInterval metric old new
  where
    processMean obj = do
      guard (perfComparable metric old obj && perfComparable metric new obj)
      perfNumber obj (perfStatKey metric "avg")

welchInterval :: String -> Aeson.Object -> Aeson.Object -> Maybe (Double, Double)
welchInterval metric old new = do
    (mean0, s0, n0) <- sample old
    (mean1, s1, n1) <- sample new
    let v0 = s0 * s0 / n0
        v1 = s1 * s1 / n1
        variance = v0 + v1
        difference = mean1 - mean0
    guard (finite variance && finite difference)
    half <- if variance == 0
      then return 0
      else do
        let df = variance * variance / (v0 * v0 / (n0 - 1) + v1 * v1 / (n1 - 1))
        guard (finite df && df >= 1)
        return (critical95 df * sqrt variance)
    let lo = difference - half
        hi = difference + half
    guard (finite lo && finite hi)
    return (lo, hi)
  where
    sample obj = do
      mean <- perfNumber obj (perfStatKey metric "avg")
      s <- perfNumber obj (perfStatKey metric "stdev")
      n <- perfNumber obj "num_iterations"
      guard (s >= 0 && n >= 2 && n == fromInteger (floor n))
      return (mean, s, n)

-- Two-sided Student t critical values, rounded up. Use the next lower
-- tabulated degrees of freedom to keep the interval conservative.
critical95 :: Double -> Double
critical95 df = last [score | (n, score) <- table, n <= max 1 df]
  where
    table = zip [1..30]
      [ 12.707, 4.303, 3.183, 2.777, 2.571, 2.447, 2.365, 2.307, 2.263, 2.229
      , 2.201, 2.179, 2.161, 2.145, 2.132, 2.120, 2.110, 2.101, 2.094, 2.086
      , 2.080, 2.074, 2.069, 2.064, 2.060, 2.056, 2.052, 2.049, 2.046, 2.043
      ] ++ [(40, 2.022), (60, 2.001), (120, 1.980)]

-- | Keep the machine report compact and use the same quantities as the UI.
perfJson :: Maybe Aeson.Object -> TestResult -> Maybe Aeson.Value
perfJson baseline res = do
    obj <- testPerfData res
    let interval = baseline >>= (\old -> perfMeanInterval "wall_duration" old obj)
        reason = case baseline of
          Nothing -> Just "no recorded baseline"
          Just old -> perfComparisonReason "wall_duration" old obj
    return $ Aeson.object
      [ AesonKey.fromString "measurements" Aeson..= measurements obj
      , AesonKey.fromString "baseline" Aeson..= fmap measurements baseline
      , AesonKey.fromString "mean_difference_ci95_ms" Aeson..= fmap bounds interval
      , AesonKey.fromString "comparison_unavailable_reason" Aeson..= reason
      ]
  where
    keys = ["peak_rss", "num_iterations", "loop_iterations", "perf_info", "counter_info", "source", "comparison_id", "process_samples"] ++
      [ perfStatKey metric stat
      | metric <- [m | (m, _, _) <- perfMetrics] ++ ["duration"]
      , stat <- ["avg", "min", "max", "median", "q1", "q3", "stdev", "outlier_count"]
      ]
    measurements = Aeson.Object . AesonKM.filterWithKey (\key _ -> AesonKey.toString key `elem` keys)
    bounds (lo, hi) = Aeson.object
      [ AesonKey.fromString "lower" Aeson..= lo
      , AesonKey.fromString "upper" Aeson..= hi
      ]
