{-# LANGUAGE BangPatterns #-}
module Acton.LookupStats
  ( lookupStatsEnabled
  , lookupStatsEvery
  , recordLookup
  , recordLookupList
  , reportLookupStats
  ) where

import Control.Monad (when)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Ord (Down(..))
import GHC.Clock (getMonotonicTimeNSec)
import Numeric (showFFloat)
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)


data LookupStat = LookupStat {
    statCount    :: !Integer,
    statTimeNs   :: !Integer,
    statMaxNs    :: !Integer,
    statItems    :: !Integer,
    statMaxItems :: !Integer
  }

zeroStat :: LookupStat
zeroStat = LookupStat 0 0 0 0 0

oneStat :: Integer -> Integer -> LookupStat
oneStat ns items = LookupStat 1 ns ns items items

addStat :: LookupStat -> LookupStat -> LookupStat
addStat a b = LookupStat {
    statCount = statCount a + statCount b,
    statTimeNs = statTimeNs a + statTimeNs b,
    statMaxNs = max (statMaxNs a) (statMaxNs b),
    statItems = statItems a + statItems b,
    statMaxItems = max (statMaxItems a) (statMaxItems b)
  }

diffStat :: LookupStat -> LookupStat -> LookupStat
diffStat a b = LookupStat {
    statCount = statCount a - statCount b,
    statTimeNs = statTimeNs a - statTimeNs b,
    statMaxNs = statMaxNs a,
    statItems = statItems a - statItems b,
    statMaxItems = statMaxItems a
  }

statsRef :: IORef (Map.Map String LookupStat)
statsRef = unsafePerformIO $ newIORef Map.empty
{-# NOINLINE statsRef #-}

lastReportRef :: IORef (Map.Map String LookupStat)
lastReportRef = unsafePerformIO $ newIORef Map.empty
{-# NOINLINE lastReportRef #-}

lookupStatsEnabled :: Bool
lookupStatsEnabled = unsafePerformIO $ do
    v <- lookupEnv "ACTON_LOOKUP_STATS"
    return $ case v of
      Just "1" -> True
      Just "true" -> True
      Just "TRUE" -> True
      Just "yes" -> True
      Just "YES" -> True
      _ -> False
{-# NOINLINE lookupStatsEnabled #-}

lookupStatsEvery :: Maybe Int
lookupStatsEvery = unsafePerformIO $ do
    v <- lookupEnv "ACTON_LOOKUP_STATS_EVERY"
    return $ if lookupStatsEnabled
      then case v >>= readMaybe of
             Just n | n > 0 -> Just n
             _ -> Nothing
      else Nothing
{-# NOINLINE lookupStatsEvery #-}

recordLookup :: String -> a -> a
recordLookup bucket x
  | not lookupStatsEnabled = x
  | otherwise = unsafePerformIO $ do
      t0 <- getMonotonicTimeNSec
      let !y = x
      t1 <- getMonotonicTimeNSec
      addLookupStat bucket (fromIntegral (t1 - t0)) 0
      return y
{-# NOINLINE recordLookup #-}

recordLookupList :: String -> [a] -> [a]
recordLookupList bucket xs
  | not lookupStatsEnabled = xs
  | otherwise = unsafePerformIO $ do
      t0 <- getMonotonicTimeNSec
      let !n = length xs
      t1 <- getMonotonicTimeNSec
      addLookupStat bucket (fromIntegral (t1 - t0)) (fromIntegral n)
      return xs
{-# NOINLINE recordLookupList #-}

addLookupStat :: String -> Integer -> Integer -> IO ()
addLookupStat bucket ns items =
    atomicModifyIORef' statsRef $ \m ->
      let !s = oneStat ns items
          !m' = Map.insertWith addStat bucket s m
      in (m', ())

reportLookupStats :: String -> IO ()
reportLookupStats label = when lookupStatsEnabled $ do
    current <- readIORef statsRef
    previous <- atomicModifyIORef' lastReportRef $ \old -> (current, old)
    let delta = Map.filter ((> 0) . statCount) $
                  Map.mergeWithKey
                    (\_ a b -> Just (diffStat a b))
                    id
                    (const Map.empty)
                    current
                    previous
        rows = sortOn (Down . statTimeNs . snd) (Map.assocs delta)
    hPutStrLn stderr $ "ACTON_LOOKUP_STATS " ++ label
    if null rows
      then hPutStrLn stderr "  <no lookup activity>"
      else mapM_ (hPutStrLn stderr . formatRow current) rows

formatRow :: Map.Map String LookupStat -> (String, LookupStat) -> String
formatRow current (bucket, delta) =
    "  " ++ bucket ++
    " count=" ++ show (statCount delta) ++
    " dt_ms=" ++ fmtMs (statTimeNs delta) ++
    " avg_us=" ++ fmtUs (avgNs delta) ++
    " max_ms=" ++ fmtMs (statMaxNs total) ++
    " items=" ++ show (statItems delta) ++
    " max_items=" ++ show (statMaxItems total) ++
    " total_count=" ++ show (statCount total) ++
    " total_ms=" ++ fmtMs (statTimeNs total)
  where total = fromMaybe zeroStat (Map.lookup bucket current)

avgNs :: LookupStat -> Integer
avgNs s
  | statCount s == 0 = 0
  | otherwise = statTimeNs s `div` statCount s

fmtMs :: Integer -> String
fmtMs ns = showFFloat (Just 3) (fromIntegral ns / 1000000 :: Double) ""

fmtUs :: Integer -> String
fmtUs ns = showFFloat (Just 3) (fromIntegral ns / 1000 :: Double) ""
