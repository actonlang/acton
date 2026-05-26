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
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime)
import System.Directory (getModificationTime)

-- | Snapshot of a file's contents.
-- The same contents are available as decoded text and raw bytes.
data SourceSnapshot = SourceSnapshot
  { ssText :: T.Text        -- ^ UTF-8 decoded text view.
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
        bytes <- B.readFile path
        let txt = TE.decodeUtf8 bytes
        return SourceSnapshot
          { ssText = txt
          , ssBytes = bytes
          , ssIsOverlay = False
          }
    , spGetModTime = getModificationTime
    }
