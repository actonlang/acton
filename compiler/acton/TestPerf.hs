module TestPerf
  ( testPerfData
  , perfNumber
  , perfMetrics
  , perfStatKey
  , perfCounterInfo
  , perfComparable
  , perfMeanInterval
  , perfJson
  ) where

import Acton.Testing (TestResult(..))
import Control.Monad (guard)
import Data.Maybe (isJust)
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
    [ ("duration", "time excl. GC", "ms")
    , ("wall_duration", "wall time", "ms")
    , ("gc_duration", "GC time", "ms")
    , ("cpu_user", "CPU user", "ms")
    , ("cpu_system", "CPU system", "ms")
    , ("instructions", "instructions", "count")
    , ("cycles", "cycles", "count")
    , ("ipc", "IPC", "ratio")
    , ("mem_usage_delta", "allocated", "B")
    , ("non_gc_mem_usage_delta", "non-GC change", "B")
    ]

-- Preserve the original mean and duration outlier keys in saved measurements.
perfStatKey :: String -> String -> String
perfStatKey "duration" "outlier_count" = "outlier_count"
perfStatKey metric "avg"
  | metric `elem` ["mem_usage_delta", "non_gc_mem_usage_delta"] = metric ++ "_avg"
perfStatKey metric stat = stat ++ "_" ++ metric

perfCounterInfo :: Aeson.Object -> Maybe Aeson.Object
perfCounterInfo obj = case AesonKM.lookup (AesonKey.fromString "counter_info") obj of
    Just (Aeson.Object info) -> Just info
    _ -> Nothing

-- Counter deltas only make sense for the same machine and accounting scope.
-- Legacy recordings can still be compared using their original metrics.
perfComparable :: String -> Aeson.Object -> Aeson.Object -> Bool
perfComparable metric old new
  | metric `notElem` ["cpu_user", "cpu_system", "instructions", "cycles", "ipc"] = True
  | otherwise = case (perfCounterInfo old, perfCounterInfo new) of
      (Just a, Just b) -> all (matches a b) keys
      _ -> False
  where
    keys = ["version", "os", "release", "arch", "cpu"] ++
      if metric `elem` ["cpu_user", "cpu_system"] then [] else ["backend", "scope"]
    matches a b key = case AesonKM.lookup (AesonKey.fromString key) a of
      Just value@(Aeson.String s) | s /= mempty -> AesonKM.lookup (AesonKey.fromString key) b == Just value
      _ -> False

-- | Approximate 95% Welch interval for a mean difference, in the metric's units.
-- The sample model assumes independent iterations; process drift is not covered.
perfMeanInterval :: String -> Aeson.Object -> Aeson.Object -> Maybe (Double, Double)
perfMeanInterval metric old new = do
    guard (perfComparable metric old new)
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
    let interval = baseline >>= (\old -> perfMeanInterval "duration" old obj)
    return $ Aeson.object
      [ AesonKey.fromString "measurements" Aeson..= measurements obj
      , AesonKey.fromString "baseline" Aeson..= fmap measurements baseline
      , AesonKey.fromString "mean_difference_ci95_ms" Aeson..= fmap bounds interval
      ]
  where
    keys = ["peak_rss", "num_iterations", "counter_info"] ++
      [ perfStatKey metric stat
      | (metric, _, _) <- perfMetrics
      , stat <- ["avg", "min", "max", "median", "q1", "q3", "stdev", "outlier_count"]
      ]
    measurements = Aeson.Object . AesonKM.filterWithKey (\key _ -> AesonKey.toString key `elem` keys)
    bounds (lo, hi) = Aeson.object
      [ AesonKey.fromString "lower" Aeson..= lo
      , AesonKey.fromString "upper" Aeson..= hi
      ]
