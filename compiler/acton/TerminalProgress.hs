module TerminalProgress
  ( TermProgress
  , initTermProgress
  , termProgressEnabled
  , termProgressPercent
  , termProgressIndeterminate
  , termProgressClear
  , termProgressHeartbeat
  ) where

import qualified Acton.CommandLineParser as C
import Control.Monad (when)
import Data.Char (toLower)
import Data.IORef
import System.Environment (lookupEnv)
import System.IO (hFlush, hIsTerminalDevice, stdout)
import System.Clock (Clock(Monotonic), TimeSpec, getTime, toNanoSecs)

data TermProgress = TermProgress
  { tpEnabled :: Bool
  , tpLastRef :: IORef (Maybe (Int, Maybe Int))
  , tpLastSentRef :: IORef (Maybe TimeSpec)
  }

termProgressEnabled :: TermProgress -> Bool
termProgressEnabled = tpEnabled

initTermProgress :: C.GlobalOptions -> IO TermProgress
initTermProgress gopts = do
    tty <- hIsTerminalDevice stdout
    env <- lookupEnv "ACTON_OSC_PROGRESS"
    let envSetting = env >>= parseBool
        enabledByTty = (tty || C.tty gopts) && not (C.quiet gopts) && not (C.noProgress gopts)
        enabled =
          if C.noProgress gopts
            then False
            else case envSetting of
                   Just True -> not (C.quiet gopts)
                   Just False -> False
                   Nothing -> enabledByTty
    lastRef <- newIORef Nothing
    lastSentRef <- newIORef Nothing
    return TermProgress
      { tpEnabled = enabled
      , tpLastRef = lastRef
      , tpLastSentRef = lastSentRef
      }

parseBool :: String -> Maybe Bool
parseBool raw =
    case map toLower raw of
      "1" -> Just True
      "true" -> Just True
      "yes" -> Just True
      "on" -> Just True
      "0" -> Just False
      "false" -> Just False
      "no" -> Just False
      "off" -> Just False
      _ -> Nothing

termProgressPercent :: TermProgress -> Int -> IO ()
termProgressPercent tp pct = termProgressSet tp 1 (Just pct)

termProgressIndeterminate :: TermProgress -> IO ()
termProgressIndeterminate tp = termProgressSet tp 3 Nothing

termProgressClear :: TermProgress -> IO ()
termProgressClear tp = termProgressSet tp 0 Nothing

termProgressSet :: TermProgress -> Int -> Maybe Int -> IO ()
termProgressSet tp st mpr = when (tpEnabled tp) $ do
    let st' = clamp 0 4 st
        pr' = fmap (clamp 0 100) mpr
        newState = if st' == 0 then Nothing else Just (st', pr')
    prev <- readIORef (tpLastRef tp)
    when (prev /= newState) $ do
      putStr (oscSequence st' pr')
      hFlush stdout
      writeIORef (tpLastRef tp) newState
      now <- getTime Monotonic
      writeIORef (tpLastSentRef tp) (Just now)

oscSequence :: Int -> Maybe Int -> String
oscSequence st mpr =
    let esc = "\x1b"
        bel = "\x07"
        base = esc ++ "]9;4;" ++ show st
        body = case mpr of
                 Nothing -> base
                 Just p -> base ++ ";" ++ show p
    in body ++ bel

clamp :: Int -> Int -> Int -> Int
clamp lo hi v = max lo (min hi v)

keepaliveMicros :: Integer
keepaliveMicros = 1000000

termProgressHeartbeat :: TermProgress -> IO ()
termProgressHeartbeat tp = when (tpEnabled tp) $ do
    mstate <- readIORef (tpLastRef tp)
    case mstate of
      Just (st, pr) -> do
        now <- getTime Monotonic
        mlast <- readIORef (tpLastSentRef tp)
        let due = case mlast of
                    Nothing -> True
                    Just t ->
                      let elapsed = toNanoSecs now - toNanoSecs t
                      in elapsed >= keepaliveMicros * 1000
        when due $ do
          putStr (oscSequence st pr)
          hFlush stdout
          writeIORef (tpLastSentRef tp) (Just now)
      Nothing -> return ()
