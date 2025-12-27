-- | Source access abstraction used by the compiler and LSP.
-- Provides an overlay-first view of files while still allowing disk fallback.
-- This is needed because the LSP server must read on-disk files and also
-- accept unsaved buffer contents from the editor as overlays.
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

-- | Snapshot of a file's contents.
-- The same contents are available as decoded text and raw bytes.
data SourceSnapshot = SourceSnapshot
  { ssText :: String        -- ^ UTF-8 decoded text view.
  , ssBytes :: B.ByteString -- ^ Raw bytes for hashing or byte-precise work.
  , ssIsOverlay :: Bool     -- ^ True when the snapshot comes from an overlay.
  }

-- | Interface for reading sources with optional in-memory overlays.
data SourceProvider = SourceProvider
  { spReadOverlay :: FilePath -> IO (Maybe SourceSnapshot) -- ^ Return overlay contents for a path, if any.
  , spReadFile :: FilePath -> IO SourceSnapshot            -- ^ Read contents from disk.
  , spGetModTime :: FilePath -> IO UTCTime                 -- ^ Retrieve disk modification time.
  }

-- | Read a path, preferring overlay contents when available.
readSource :: SourceProvider -> FilePath -> IO SourceSnapshot
readSource sp path = do
  mo <- spReadOverlay sp path
  case mo of
    Just snap -> return snap
    Nothing -> spReadFile sp path

-- | Default provider that reads directly from disk with no overlays.
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

-- | Read a UTF-8 text file without altering newlines or encoding.
readFileUtf8 :: FilePath -> IO String
readFileUtf8 path = do
  h <- openFile path ReadMode
  hSetEncoding h utf8
  hGetContents h
