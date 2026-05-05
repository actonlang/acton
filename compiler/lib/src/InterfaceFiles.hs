-- Copyright (C) 2019-2021 Data Ductus AB
--
-- Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
--
-- 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
--
-- 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
--
-- 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

{-# LANGUAGE DeriveGeneric #-}
-- Acton Interface (.ty) Files
--
-- Purpose
-- - Cache compiled module information so later builds can avoid unnecessary
--   work by reading a small header instead of decoding large structures.
-- - If a .ty cannot be decoded or the version mismatches, the caller must
--   treat the module as out-of-date and recompile from source.
--
-- On-disk layout (Binary, in this exact order)
--   1) version            :: [Int]                         -- Acton.Syntax.version
--   2) sourceMeta         :: Maybe SourceFileMeta          -- cached source stat metadata
--   3) moduleSrcBytesHash :: ByteString                    -- SHA-256 of raw source bytes
--   4) modulePubHash      :: ByteString                    -- SHA-256 of public NameInfo (doc-free)
--                                                       -- augmented with imports' pub hashes
--   5) moduleImplHash     :: ByteString                    -- SHA-256 of per-name impl hashes
--   6) imports            :: [(A.ModName, ByteString)]     -- imported module and pub hash used
--   7) nameHashes         :: [NameHashInfo]                -- per-name src/pub/impl hashes + deps
--   8) roots              :: [A.Name]                      -- root actors (e.g., main or test_main)
--   9) tests              :: [String]                      -- discovered test names
--  10) docstring          :: Maybe String                  -- module docstring
--  11) nameInfo           :: I.NameInfo                    -- type/name environment
--  12) typedModule        :: A.Module                      -- typed module
--
-- Rationale for ordering
-- - Put cache validity fields first so callers can validate and reuse a cache
--   entry without decoding the large NameInfo and typed Module sections.
-- - Follow with the remaining module hashes used for dependency checks.
-- - Follow with small variable fields (imports, roots, docstring).
-- - Put heavy sections last (NameInfo, typed Module) to preserve laziness.

module InterfaceFiles where

import Prelude hiding (readFile, writeFile)
import Data.Binary
import qualified Control.Exception as E
import qualified Data.Binary.Get as BinaryGet
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Acton.Syntax as A
import qualified Acton.NameInfo as I
import GHC.Generics (Generic)
import System.Directory (renameFile)
import System.IO (IOMode(ReadMode), hClose, hFileSize, openBinaryFile)
import System.Posix.Process (getProcessID)

data NameHashInfo = NameHashInfo
  { nhName     :: A.Name
  , nhSrcHash  :: BS.ByteString
  , nhPubHash  :: BS.ByteString
  , nhImplHash :: BS.ByteString
  , nhPubDeps  :: [(A.QName, BS.ByteString)]
  , nhImplDeps :: [(A.QName, BS.ByteString)]
  } deriving (Show, Eq, Generic)

instance Binary NameHashInfo

data SourceFileMeta = SourceFileMeta
  { sfmMTimeNs :: Integer
  , sfmCTimeNs :: Integer
  , sfmSize    :: Integer
  , sfmDevice  :: Maybe Integer
  , sfmInode   :: Maybe Integer
  } deriving (Show, Eq, Generic)

instance Binary SourceFileMeta

type TyFile =
  ( [A.ModName]
  , I.NameInfo
  , A.Module
  , Maybe SourceFileMeta
  , BS.ByteString
  , BS.ByteString
  , BS.ByteString
  , [(A.ModName, BS.ByteString)]
  , [NameHashInfo]
  , [A.Name]
  , [String]
  , Maybe String
  )

type TyHeader =
  ( Maybe SourceFileMeta
  , BS.ByteString
  , BS.ByteString
  , BS.ByteString
  , [(A.ModName, BS.ByteString)]
  , [NameHashInfo]
  , [A.Name]
  , [String]
  , Maybe String
  )

-- Note: tests are stored in the header to support listing without compiling
--       or executing test binaries.

readTyBytes :: FilePath -> IO BL.ByteString
readTyBytes f = do
    h <- openBinaryFile f ReadMode
    size <- hFileSize h
    bs <- BS.hGet h (fromIntegral size)
    hClose h
    return (BL.fromStrict bs)

versionMismatch :: [Int] -> IO a
versionMismatch vs =
    ioError (userError (".ty version mismatch: file has " ++ show vs ++ ", expected " ++ show A.version))

readTyVersion :: BL.ByteString -> IO BL.ByteString
readTyVersion bsLazy =
    case BinaryGet.runGetOrFail (get :: BinaryGet.Get [Int]) bsLazy of
      Left _ -> ioError (userError "Failed to decode .ty version")
      Right (rest, _, vs)
        | vs == A.version -> return rest
        | otherwise -> versionMismatch vs

decodeTyPrefix :: BL.ByteString -> IO (Maybe SourceFileMeta, BS.ByteString, BS.ByteString, BS.ByteString, BL.ByteString)
decodeTyPrefix bsLazy =
    case BinaryGet.runGetOrFail getPrefix bsLazy of
      Left _ -> ioError (userError "Failed to decode .ty prefix")
      Right (rest, _, (sourceMeta, moduleSrcBytesHash, modulePubHash, moduleImplHash)) ->
        return (sourceMeta, moduleSrcBytesHash, modulePubHash, moduleImplHash, rest)
  where
    getPrefix = do
      sourceMeta <- get :: BinaryGet.Get (Maybe SourceFileMeta)
      moduleSrcBytesHash <- get :: BinaryGet.Get BS.ByteString
      modulePubHash <- get :: BinaryGet.Get BS.ByteString
      moduleImplHash <- get :: BinaryGet.Get BS.ByteString
      return (sourceMeta, moduleSrcBytesHash, modulePubHash, moduleImplHash)

writeFile :: FilePath -> BS.ByteString -> BS.ByteString -> BS.ByteString -> Maybe SourceFileMeta -> [(A.ModName, BS.ByteString)] -> [NameHashInfo] -> [A.Name] -> [String] -> Maybe String -> I.NameInfo -> A.Module -> IO ()
writeFile f moduleSrcBytesHash modulePubHash moduleImplHash sourceMeta imps nameHashes roots tests mdoc nmod tchecked = do
    pid <- getProcessID
    let tmpFile = f ++ "." ++ show pid
    BL.writeFile tmpFile (encode ((A.version, sourceMeta, moduleSrcBytesHash, modulePubHash, moduleImplHash), imps, nameHashes, roots, tests, mdoc, nmod, tchecked))
    renameFile tmpFile f

readFile :: FilePath -> IO TyFile
readFile f = do
    bsLazy <- readTyBytes f
    body0 <- readTyVersion bsLazy
    (sourceMeta, moduleSrcBytesHash, modulePubHash, moduleImplHash, body1) <- decodeTyPrefix body0
    let getBody = do
          imps <- get :: BinaryGet.Get [(A.ModName, BS.ByteString)]
          nameHashes <- get :: BinaryGet.Get [NameHashInfo]
          roots <- get :: BinaryGet.Get [A.Name]
          tests <- get :: BinaryGet.Get [String]
          mdoc <- get :: BinaryGet.Get (Maybe String)
          nmod <- get :: BinaryGet.Get I.NameInfo
          tmod <- get :: BinaryGet.Get A.Module
          return (imps, nameHashes, roots, tests, mdoc, nmod, tmod)
    case BinaryGet.runGetOrFail getBody body1 of
      Left _ -> ioError (userError "Failed to decode .ty file")
      Right (_, _, (imps, nameHashes, roots, tests, mdoc, nmod, tmod)) ->
        return (map fst imps, nmod, tmod, sourceMeta, moduleSrcBytesHash, modulePubHash, moduleImplHash, imps, nameHashes, roots, tests, mdoc)

-- Read only the cached header fields from .ty: source metadata, module hashes,
-- imports, name hashes, roots, tests, and docstring.
-- This avoids decoding the large NameInfo and typed Module sections and is
-- much faster than readFile for freshness checks and dependency discovery.
readHeader :: FilePath -> IO TyHeader
readHeader f = do
    bsLazy <- readTyBytes f
    body0 <- readTyVersion bsLazy
    (sourceMeta, moduleSrcBytesHash, modulePubHash, moduleImplHash, body1) <- decodeTyPrefix body0
    let getHdr = do
          imps  <- get :: BinaryGet.Get [(A.ModName, BS.ByteString)]
          nameHashes <- get :: BinaryGet.Get [NameHashInfo]
          roots <- get :: BinaryGet.Get [A.Name]
          tests <- get :: BinaryGet.Get [String]
          doc   <- get :: BinaryGet.Get (Maybe String)
          return (imps, nameHashes, roots, tests, doc)
    case BinaryGet.runGetOrFail getHdr body1 of
      Left _ -> ioError (userError "Failed to decode .ty header")
      Right (_, _, (imps, nameHashes, roots, tests, doc)) ->
        return (sourceMeta, moduleSrcBytesHash, modulePubHash, moduleImplHash, imps, nameHashes, roots, tests, doc)

-- Interface files are caches for most callers. If a file is missing,
-- unreadable, corrupt, or from a different compiler interface version, the
-- cache entry is not usable.
readFileMaybe :: FilePath -> IO (Maybe TyFile)
readFileMaybe = readTyMaybe readFile

readHeaderMaybe :: FilePath -> IO (Maybe TyHeader)
readHeaderMaybe = readTyMaybe readHeader

readTyMaybe :: (FilePath -> IO a) -> FilePath -> IO (Maybe a)
readTyMaybe readTy f = (Just <$> readTy f) `E.catch` tyCacheMiss

tyCacheMiss :: E.IOException -> IO (Maybe a)
tyCacheMiss _ = return Nothing
