{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Main where

import qualified Acton.Parser as Parser
import qualified Acton.Syntax as A

import Control.DeepSeq
import Control.Exception
import Control.Monad
import qualified Data.ByteString as BS
import Data.IORef
import Data.Int
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import GHC.Stats
import System.Clock
import System.CPUTime
import System.Environment
import System.Exit
import System.Mem
import Text.Printf

data Measurement = Measurement
  { mLabel :: String
  , mLiveBytes :: Integer
  , mExtra :: String
  }

data Timing = Timing
  { tWallNs :: Integer
  , tCpuPs :: Integer
  , tAllocated :: Integer
  }

main :: IO ()
main = do
  enabled <- getRTSStatsEnabled
  unless enabled $ do
    putStrLn "RTS stats are disabled; run with +RTS -T -RTS"
    exitFailure
  files <- getArgs
  when (null files) $ do
    putStrLn "usage: parser-heap-bench FILE.act [...]"
    exitFailure
  _ <- parseAst "<warm>" ""
  performMajorGC
  forM_ files measureFile

measureFile :: FilePath -> IO ()
measureFile file = do
  diskBytes <- BS.length <$> BS.readFile file
  putStrLn $ "file " ++ file ++ " bytes " ++ show diskBytes
  (topStmts, timing) <- measureParseTiming file
  printf "parse              wall %.3f s  cpu %.3f s  allocated %.3f MiB  %d top stmts\n"
    (nsToSeconds (tWallNs timing))
    (psToSeconds (tCpuPs timing))
    (bytesToMiB (tAllocated timing))
    topStmts
  ms <- sequence
    [ measureSource file
    , measureAstOnly file
    , measureAstAndSource file
    ]
  forM_ ms $ \m ->
    printf "%-18s %12d bytes  %.3f MiB  %s\n"
      (mLabel m)
      (mLiveBytes m)
      (bytesToMiB (mLiveBytes m))
      (mExtra m)
  putStrLn ""

measureParseTiming :: FilePath -> IO (Int, Timing)
measureParseTiming file = do
  src <- readSource file
  evaluate (rnf src)
  performMajorGC
  timed $ do
    m <- parseAst file src
    return (length (A.mbody m))

measureSource :: FilePath -> IO Measurement
measureSource file =
  measured "source" $ do
    src <- readSource file
    evaluate (rnf src)
    return (src, show (sourceLength src) ++ " chars")

measureAstOnly :: FilePath -> IO Measurement
measureAstOnly file =
  measured "AST only" $ do
    m <- parseFile file
    return (m, show (length (A.mbody m)) ++ " top stmts")

measureAstAndSource :: FilePath -> IO Measurement
measureAstAndSource file =
  measured "AST + source" $ do
    src <- readSource file
    evaluate (rnf src)
    m <- parseAst file src
    return ((m, src), show (length (A.mbody m)) ++ " top stmts")

measured :: NFData a => String -> IO (a, String) -> IO Measurement
measured label build = do
  performMajorGC
  base <- liveBytes
  (x, extra) <- build
  evaluate (rnf x)
  ref <- newIORef (Just x)
  performMajorGC
  after <- liveBytes
  keep <- readIORef ref
  evaluate (case keep of
              Just _ -> ()
              Nothing -> ())
  writeIORef ref Nothing
  performMajorGC
  return Measurement
    { mLabel = label
    , mLiveBytes = toInteger after - toInteger base
    , mExtra = extra
    }

timed :: NFData a => IO a -> IO (a, Timing)
timed action = do
  stats0 <- getRTSStats
  cpu0 <- getCPUTime
  wall0 <- getTime Monotonic
  x <- action
  evaluate (rnf x)
  wall1 <- getTime Monotonic
  cpu1 <- getCPUTime
  stats1 <- getRTSStats
  return (x, Timing
    { tWallNs = timeSpecNs (wall1 - wall0)
    , tCpuPs = toInteger (cpu1 - cpu0)
    , tAllocated = toInteger (allocated_bytes stats1 - allocated_bytes stats0)
    })

parseFile :: FilePath -> IO A.Module
parseFile file = do
  src <- readSource file
  evaluate (rnf src)
  parseAst file src

parseAst :: FilePath -> Text -> IO A.Module
parseAst file src = do
  m <- Parser.parseModule (A.modName ["heap_probe"]) file src Nothing
  evaluate (rnf m)
  return m

readSource :: FilePath -> IO Text
readSource file = TE.decodeUtf8 <$> BS.readFile file

sourceLength :: Text -> Int
sourceLength = T.length

liveBytes :: IO Int64
liveBytes = do
  stats <- getRTSStats
  return (fromIntegral (gcdetails_live_bytes (gc stats)))

timeSpecNs :: TimeSpec -> Integer
timeSpecNs t = toInteger (sec t) * 1000000000 + toInteger (nsec t)

nsToSeconds :: Integer -> Double
nsToSeconds n = fromIntegral n / 1000000000

psToSeconds :: Integer -> Double
psToSeconds n = fromIntegral n / 1000000000000

bytesToMiB :: Integer -> Double
bytesToMiB n = fromIntegral n / (1024 * 1024)
