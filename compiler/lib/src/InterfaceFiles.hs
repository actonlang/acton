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

-- Acton Interface (.ty) Files
--
-- Purpose
-- - Cache compiled module information so later builds can avoid unnecessary
--   work by reading a small header instead of decoding large structures.
-- - If a .ty cannot be decoded or the version mismatches, the caller must
--   treat the module as out-of-date and recompile from source.
--
-- On-disk layout (Binary, in this exact order)
--   1) version      :: [Int]          -- Acton.Syntax.version
--   2) srcHash      :: ByteString     -- SHA-256 of the source bytes (raw)
--   3) imports      :: [A.ModName]    -- imported modules
--   4) roots        :: [A.Name]       -- root actors (e.g., main or __test_main)
--   5) docstring    :: Maybe String   -- module docstring
--   6) nameInfo     :: A.NameInfo     -- type/name environment
--   7) typedModule  :: A.Module       -- typed module
--
-- Rationale for ordering
-- - Put small, fixed/cheap fields up front (version, srcHash) to enable fast
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
import System.IO
import System.Directory (renameFile)
import System.Posix.Process (getProcessID)

-- Note: tests are deliberately not stored in the header anymore. We only keep
--       imports/roots/docstring as the small, frequently accessed header.

-- Write interface file with source content hash using atomic write
-- We use temp file + rename as atomic write to avoid other readers seeing partially written output.
writeFile :: FilePath -> BS.ByteString -> [A.ModName] -> [A.Name] -> Maybe String -> A.NameInfo -> A.Module -> IO ()
writeFile f srcHash ms roots mdoc nmod tchecked = do
    -- Use PID for unique temp file name
    pid <- getProcessID
    let tmpFile = f ++ "." ++ show pid
    BL.writeFile tmpFile (encode (A.version, srcHash, ms, roots, mdoc, nmod, tchecked))
    -- Atomically rename to final location
    -- This is atomic on POSIX systems and prevents partial writes or conflicts
    renameFile tmpFile f

-- Read complete interface file
readFile :: FilePath -> IO ([A.ModName], A.NameInfo, A.Module, BS.ByteString)
readFile f = do
    h <- openBinaryFile f ReadMode
    size <- hFileSize h
    bs <- BS.hGet h (fromIntegral size)
    hClose h
    let bsLazy = BL.fromStrict bs
    let (vs, srcHash, ms, _roots, _mdoc, nmod, tmod)
          = decode bsLazy :: ([Int], BS.ByteString, [A.ModName], [A.Name], Maybe String, A.NameInfo, A.Module)
    if vs == A.version
      then return (ms, nmod, tmod, srcHash)
      else do
        putStrLn ("Interface file has version " ++ show vs ++ "; current version is " ++ show A.version)
        System.Exit.exitFailure

-- Read only small header fields from .ty: (hash, imports, roots, docstring)
-- This avoids loading large fields and is much faster than readFile which
-- decodes everything.
readHeader :: FilePath -> IO (BS.ByteString, [A.ModName], [A.Name], Maybe String)
readHeader f = do
    h <- openBinaryFile f ReadMode
    size <- hFileSize h
    bs <- BS.hGet h (fromIntegral size)
    hClose h
    let bsLazy = BL.fromStrict bs
        getHdr :: BinaryGet.Get ([Int], BS.ByteString, [A.ModName], [A.Name], Maybe String)
        getHdr = do
          vs    <- get :: BinaryGet.Get [Int]
          hash  <- get :: BinaryGet.Get BS.ByteString
          imps  <- get :: BinaryGet.Get [A.ModName]
          roots <- get :: BinaryGet.Get [A.Name]
          doc   <- get :: BinaryGet.Get (Maybe String)
          return (vs, hash, imps, roots, doc)
    case BinaryGet.runGetOrFail getHdr bsLazy of
      Left _ -> ioError (userError "Failed to decode .ty header")
      Right (_, _, (vs, hash, imps, roots, doc)) ->
        if vs == A.version then return (hash, imps, roots, doc)
        else ioError (userError (".ty version mismatch: file has " ++ show vs ++ ", expected " ++ show A.version))
