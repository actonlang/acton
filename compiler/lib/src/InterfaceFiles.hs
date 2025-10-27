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

module InterfaceFiles where

import Data.Binary
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified System.Exit
import qualified Acton.Syntax
import System.IO
import System.Directory (renameFile)
import System.Posix.Process (getProcessID)

-- Write interface file with source content hash using atomic write
-- We use temp file + rename as atomic write to avoid other readers seeing partially written output.
writeFile :: FilePath -> [Acton.Syntax.ModName] -> Acton.Syntax.NameInfo -> Acton.Syntax.Module -> Data.ByteString.ByteString -> IO ()
writeFile f ms nmod tchecked srcHash = do
    -- Use PID for unique temp file name
    pid <- getProcessID
    let tmpFile = f ++ "." ++ show pid
    Data.ByteString.Lazy.writeFile tmpFile (encode (Acton.Syntax.version, ms, nmod, tchecked, srcHash))
    -- Atomically rename to final location
    -- This is atomic on POSIX systems and prevents partial writes or conflicts
    renameFile tmpFile f

-- Read interface file, returning (imports, name info, typed module, source hash)
readFile :: FilePath -> IO ([Acton.Syntax.ModName], Acton.Syntax.NameInfo, Acton.Syntax.Module, Data.ByteString.ByteString)
readFile f = do
    h <- openBinaryFile f ReadMode
    -- We minimize the time we keep the file open by reading it all at once. We
    -- always want the full content anyway
    size <- hFileSize h
    bs <- Data.ByteString.hGet h (fromIntegral size)
    hClose h
    let bsLazy = Data.ByteString.Lazy.fromStrict bs
    let (vs, ms, nmod, tmod, srcHash) = decode bsLazy
    if vs == Acton.Syntax.version
      then return (ms, nmod, tmod, srcHash)
      else do
        putStrLn ("Interface file has version " ++ show vs ++ "; current version is " ++ show Acton.Syntax.version)
        System.Exit.exitFailure
