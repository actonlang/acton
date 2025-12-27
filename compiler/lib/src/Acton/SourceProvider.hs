module Acton.SourceProvider
  ( SourceProvider(..)
  , SourceSnapshot(..)
  , diskSourceProvider
  , readSource
  ) where

import qualified Data.ByteString as B
import Data.Time.Clock (UTCTime)
import System.Directory (getModificationTime)
import System.IO (IOMode(ReadMode), hGetContents, hSetEncoding, openFile, utf8)

data SourceSnapshot = SourceSnapshot
  { ssText :: String
  , ssBytes :: B.ByteString
  , ssIsOverlay :: Bool
  }

data SourceProvider = SourceProvider
  { spReadOverlay :: FilePath -> IO (Maybe SourceSnapshot)
  , spReadFile :: FilePath -> IO SourceSnapshot
  , spGetModTime :: FilePath -> IO UTCTime
  }

readSource :: SourceProvider -> FilePath -> IO SourceSnapshot
readSource sp path = do
  mo <- spReadOverlay sp path
  case mo of
    Just snap -> return snap
    Nothing -> spReadFile sp path

diskSourceProvider :: SourceProvider
diskSourceProvider =
  SourceProvider
    { spReadOverlay = \_ -> return Nothing
    , spReadFile = \path -> do
        txt <- readFileUtf8 path
        bytes <- B.readFile path
        return SourceSnapshot
          { ssText = txt
          , ssBytes = bytes
          , ssIsOverlay = False
          }
    , spGetModTime = getModificationTime
    }

readFileUtf8 :: FilePath -> IO String
readFileUtf8 path = do
  h <- openFile path ReadMode
  hSetEncoding h utf8
  hGetContents h
