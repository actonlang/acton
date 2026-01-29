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
--   2) moduleHashes       :: (ByteString, ByteString, ByteString)
--      - moduleSrcBytesHash :: ByteString                  -- SHA-256 of raw source bytes
--      - modulePubHash      :: ByteString                  -- SHA-256 of public NameInfo (doc-free)
--                                                       -- augmented with imports' pub hashes
--      - moduleImplHash     :: ByteString                  -- SHA-256 of per-name impl hashes
--   3) imports            :: [(A.ModName, ByteString)]     -- imported module and pub hash used
--   4) nameHashes         :: [NameHashInfo]                -- per-name src/pub/impl hashes + deps
--   5) roots              :: [A.Name]                      -- root actors (e.g., main or test_main)
--   6) tests              :: [String]                      -- discovered test names
--   7) docstring          :: Maybe String                  -- module docstring
--   8) nameInfo           :: I.NameInfo                    -- type/name environment
--   9) typedModule        :: A.Module                      -- typed module
--
-- Rationale for ordering
-- - Put small, fixed/cheap fields up front (version, moduleSrcBytesHash) to enable fast
--   header-only reads for freshness checks and dependency discovery.
-- - Follow with small variable fields (imports, roots, docstring).
-- - Put heavy sections last (NameInfo, typed Module) to preserve laziness.

module InterfaceFiles where

import Data.Binary
import qualified Data.Binary.Get as BinaryGet
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified System.Exit
import qualified Acton.Syntax as A
import qualified Acton.NameInfo as I
import GHC.Generics (Generic)
import System.IO
import System.Directory (renameFile)
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

-- Note: tests are stored in the header to support listing without compiling
--       or executing test binaries.

writeFile :: FilePath -> BS.ByteString -> BS.ByteString -> BS.ByteString -> [(A.ModName, BS.ByteString)] -> [NameHashInfo] -> [A.Name] -> [String] -> Maybe String -> I.NameInfo -> A.Module -> IO ()
writeFile f moduleSrcBytesHash modulePubHash moduleImplHash imps nameHashes roots tests mdoc nmod tchecked = do
    -- Use PID for unique temp file name
    pid <- getProcessID
    let tmpFile = f ++ "." ++ show pid
    BL.writeFile tmpFile (encode (A.version, (moduleSrcBytesHash, modulePubHash, moduleImplHash), imps, nameHashes, roots, tests, mdoc, nmod, tchecked))
    -- Atomically rename to final location
    -- This is atomic on POSIX systems and prevents partial writes or conflicts
    renameFile tmpFile f

readFile :: FilePath -> IO ([A.ModName], I.NameInfo, A.Module, BS.ByteString, BS.ByteString, BS.ByteString, [(A.ModName, BS.ByteString)], [NameHashInfo], [A.Name], [String], Maybe String)
readFile f = do
    h <- openBinaryFile f ReadMode
    size <- hFileSize h
    bs <- BS.hGet h (fromIntegral size)
    hClose h
    let bsLazy = BL.fromStrict bs
    let (vs, (moduleSrcBytesHash, modulePubHash, moduleImplHash), imps, nameHashes, roots, tests, mdoc, nmod, tmod)
          = decode bsLazy :: ([Int], (BS.ByteString, BS.ByteString, BS.ByteString), [(A.ModName, BS.ByteString)], [NameHashInfo], [A.Name], [String], Maybe String, I.NameInfo, A.Module)
    if vs == A.version
      then return (map fst imps, nmod, tmod, moduleSrcBytesHash, modulePubHash, moduleImplHash, imps, nameHashes, roots, tests, mdoc)
      else do
        hPutStrLn stderr ("Interface file has version " ++ show vs ++ "; current version is " ++ show A.version)
        System.Exit.exitFailure

-- Read only small header fields from .ty: (moduleSrcBytesHash, modulePubHash,
-- moduleImplHash, imports, nameHashes, roots, tests, docstring)
-- This avoids loading large fields and is much faster than readFile which
-- decodes everything.
readHeader :: FilePath -> IO (BS.ByteString, BS.ByteString, BS.ByteString, [(A.ModName, BS.ByteString)], [NameHashInfo], [A.Name], [String], Maybe String)
readHeader f = do
    h <- openBinaryFile f ReadMode
    size <- hFileSize h
    bs <- BS.hGet h (fromIntegral size)
    hClose h
    let bsLazy = BL.fromStrict bs
        getHdr :: BinaryGet.Get ([Int], (BS.ByteString, BS.ByteString, BS.ByteString), [(A.ModName, BS.ByteString)], [NameHashInfo], [A.Name], [String], Maybe String)
        getHdr = do
          vs    <- get :: BinaryGet.Get [Int]
          hashes <- get :: BinaryGet.Get (BS.ByteString, BS.ByteString, BS.ByteString)
          imps  <- get :: BinaryGet.Get [(A.ModName, BS.ByteString)]
          nameHashes <- get :: BinaryGet.Get [NameHashInfo]
          roots <- get :: BinaryGet.Get [A.Name]
          tests <- get :: BinaryGet.Get [String]
          doc   <- get :: BinaryGet.Get (Maybe String)
          return (vs, hashes, imps, nameHashes, roots, tests, doc)
    case BinaryGet.runGetOrFail getHdr bsLazy of
      Left _ -> ioError (userError "Failed to decode .ty header")
      Right (_, _, (vs, (moduleSrcBytesHash, modulePubHash, moduleImplHash), imps, nameHashes, roots, tests, doc)) ->
        if vs == A.version then return (moduleSrcBytesHash, modulePubHash, moduleImplHash, imps, nameHashes, roots, tests, doc)
        else ioError (userError (".ty version mismatch: file has " ++ show vs ++ ", expected " ++ show A.version))
