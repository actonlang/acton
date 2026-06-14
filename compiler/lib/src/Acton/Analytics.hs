{-# LANGUAGE BangPatterns #-}
-- | Lightweight, generic build-progress analytics for long performance runs.
--
-- A background sampler thread snapshots cumulative compiler progress and RTS
-- memory at a fixed time cadence and writes one machine-readable line per tick.
-- It is pass-agnostic: any compiler pass that reports through the progress
-- callbacks feeds the same counters, so the same log covers parse, type check
-- and the back passes. Unlike the human progress UI (which shows an instant),
-- this is an append-only event log meant to be plotted or diffed after the run.
--
-- Enabled by the @ACTON_ANALYTICS@ env var (sample interval in milliseconds);
-- disabled and zero-overhead otherwise. Output goes to stderr, or to the file
-- named by @ACTON_ANALYTICS_FILE@. Memory columns require the process to run
-- with @+RTS -T@ (otherwise reported as @na@).
--
-- One line per tick:
--   ACTON-ANALYTICS t=<s> parse=<n> type=<done>/<total> type_rate=<units/s>
--                   back=<n> mem_mb=<m> live_mb=<m> gc=<n> act=<label>
-- where @type_rate@ is the instantaneous type-check throughput since the
-- previous tick -- the signal that reveals pace cliffs on large modules --
-- @mem_mb@ is the RTS memory footprint (what counts against @-M@), and
-- @live_mb@ is the live data after the most recent GC.
module Acton.Analytics
  ( Analytics
  , withAnalytics
  , recordParseDone
  , recordType
  , recordBackPass
  ) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (bracket)
import Control.Monad (when)
import Data.IORef
import qualified Data.Map.Strict as Map
import GHC.Stats
import Numeric (showFFloat)
import System.Clock
import System.Environment (lookupEnv)
import System.IO
import Text.Read (readMaybe)

data Analytics
  = AnalyticsOff
  | AnalyticsOn AState

data AState = AState
  { asStart     :: TimeSpec
  , asSink      :: Handle
  , asParseDone :: IORef Int
  , asType      :: IORef (Map.Map String (Int, Int)) -- per-module (completed,total), summed at sample time
  , asBackDone  :: IORef Int
  , asLabel     :: IORef String
  , asLast      :: IORef (Double, Int)               -- previous (elapsed seconds, cumulative type units)
  }

-- | Run an action with analytics sampling active iff @ACTON_ANALYTICS@ is set to
-- a positive integer (the sample interval in milliseconds). The sampler thread
-- is started before the action and torn down -- with one final sample -- after,
-- even on exception.
withAnalytics :: (Analytics -> IO a) -> IO a
withAnalytics action = do
  mInterval <- (>>= readMaybe) <$> lookupEnv "ACTON_ANALYTICS"
  case mInterval of
    Just ms | ms > 0 -> do
      sink <- analyticsSink
      start <- getTime Monotonic
      st <- AState start sink
              <$> newIORef 0
              <*> newIORef Map.empty
              <*> newIORef 0
              <*> newIORef "init"
              <*> newIORef (0, 0)
      hPutStrLn sink ("ACTON-ANALYTICS-START interval_ms=" ++ show ms
                      ++ " columns: t parse type type_rate back mem_mb live_mb gc act")
      bracket (forkIO (sampleLoop st (ms * 1000)))
              (\tid -> killThread tid >> emitSample st >> closeSink sink)
              (\_   -> action (AnalyticsOn st))
    _ -> action AnalyticsOff

analyticsSink :: IO Handle
analyticsSink = do
  mpath <- lookupEnv "ACTON_ANALYTICS_FILE"
  h <- case mpath of
         Just p | not (null p) -> openFile p WriteMode
         _                     -> return stderr
  hSetBuffering h LineBuffering
  return h

closeSink :: Handle -> IO ()
closeSink h = when (h /= stderr) (hClose h)

sampleLoop :: AState -> Int -> IO ()
sampleLoop st delayUs = go
  where go = threadDelay delayUs >> emitSample st >> go

emitSample :: AState -> IO ()
emitSample st = do
  now <- getTime Monotonic
  let elapsed = fromIntegral (toNanoSecs (diffTimeSpec now (asStart st))) / 1e9 :: Double
  pd  <- readIORef (asParseDone st)
  tm  <- readIORef (asType st)
  bd  <- readIORef (asBackDone st)
  lbl <- readIORef (asLabel st)
  (lastT, lastDone) <- readIORef (asLast st)
  let (tdone, ttot) = Map.foldl' (\(!a, !b) (c, d) -> (a + c, b + d)) (0, 0) tm
      dt   = elapsed - lastT
      rate = if dt > 0 then fromIntegral (tdone - lastDone) / dt else 0 :: Double
  writeIORef (asLast st) (elapsed, tdone)
  (mem, live, ngc) <- readMem
  hPutStrLn (asSink st) $ unwords
    [ "ACTON-ANALYTICS"
    , "t="         ++ showF 3 elapsed
    , "parse="     ++ show pd
    , "type="      ++ show tdone ++ "/" ++ show ttot
    , "type_rate=" ++ showF 0 rate
    , "back="      ++ show bd
    , "mem_mb="    ++ mem
    , "live_mb="   ++ live
    , "gc="        ++ ngc
    , "act="       ++ lbl
    ]

-- | RTS memory footprint and live data (MiB) after the most recent GC, plus the
-- total GC count -- or @na@ when the RTS was not started with @-T@. @mem_mb@ is
-- the memory the RTS holds (the figure that counts against @-M@); @live_mb@ is
-- the live data the GC retains.
readMem :: IO (String, String, String)
readMem = do
  enabled <- getRTSStatsEnabled
  if not enabled
    then return ("na", "na", "na")
    else do
      s <- getRTSStats
      return ( mb (gcdetails_mem_in_use_bytes (gc s))
             , mb (gcdetails_live_bytes (gc s))
             , show (gcs s) )
  where mb b = show (b `div` (1024 * 1024))

showF :: Int -> Double -> String
showF prec x = showFFloat (Just prec) x ""

-- Record helpers. Cheap no-ops when analytics is off, so the instrumented
-- callbacks add nothing measurable to a normal build.

recordParseDone :: Analytics -> String -> IO ()
recordParseDone AnalyticsOff      _ = return ()
recordParseDone (AnalyticsOn st)  m = do
  atomicModifyIORef' (asParseDone st) (\n -> (n + 1, ()))
  writeIORef (asLabel st) (m ++ ":parsed")

recordType :: Analytics -> String -> Int -> Int -> IO ()
recordType AnalyticsOff     _ _    _     = return ()
recordType (AnalyticsOn st) m done total = do
  atomicModifyIORef' (asType st) (\mp -> (Map.insert m (done, total) mp, ()))
  writeIORef (asLabel st) (m ++ ":types")

recordBackPass :: Analytics -> String -> String -> IO ()
recordBackPass AnalyticsOff     _ _    = return ()
recordBackPass (AnalyticsOn st) m pass = do
  atomicModifyIORef' (asBackDone st) (\n -> (n + 1, ()))
  writeIORef (asLabel st) (m ++ ":" ++ pass)
