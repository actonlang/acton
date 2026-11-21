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
import Data.Text.Encoding.Error (UnicodeException(DecodeError))
import Data.Time.Clock (UTCTime)
import GHC.IO.Exception (IOErrorType(InvalidArgument))
import Numeric (showHex)
import System.Directory (getModificationTime)
import System.IO.Error (ioeSetErrorString, mkIOError)

-- | Snapshot of a file's contents.
-- The same contents are available as decoded text and raw bytes.
data SourceSnapshot = SourceSnapshot
  { ssText :: String        -- ^ UTF-8 decoded text view.
  , ssBytes :: B.ByteString -- ^ Raw bytes for hashing or byte-precise work.
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

-- | Default provider that reads directly from disk with no overlays.  A file
-- is read once, and its text is its bytes decoded as UTF-8.  Bytes that are
-- not UTF-8 raise an IOError that names the file.
diskSourceProvider :: SourceProvider
diskSourceProvider =
  SourceProvider
    { spReadOverlay = \_ -> return Nothing
    , spReadFile = \path -> do
        bytes <- B.readFile path
        case TE.decodeUtf8' bytes of
          Left err  -> ioError (decodeFailure path err)
          Right txt -> return SourceSnapshot { ssText = T.unpack txt, ssBytes = bytes }
    , spGetModTime = getModificationTime
    }

-- | The error for a file whose bytes are not UTF-8.
decodeFailure :: FilePath -> UnicodeException -> IOError
decodeFailure path err = ioeSetErrorString (mkIOError InvalidArgument "" Nothing (Just path)) message
  where
    message = case err of
      DecodeError _ (Just byte) -> "cannot decode byte 0x" ++ showHex byte " as UTF-8"
      _                         -> "cannot decode as UTF-8"
