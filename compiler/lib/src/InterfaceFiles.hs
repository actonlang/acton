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
import qualified Data.ByteString.Lazy
import Codec.Compression.Zlib
import qualified System.Exit
import qualified Acton.Syntax
import System.IO

writeFile :: FilePath -> [Acton.Syntax.ModName] -> Acton.Syntax.NameInfo -> IO ()
writeFile f ms nmod = do
    h <- openFile f WriteMode
    Data.ByteString.Lazy.hPut h (compress (encode (Acton.Syntax.version, (ms,nmod))))
    hClose h

readFile :: FilePath -> IO ([Acton.Syntax.ModName], Acton.Syntax.NameInfo)
readFile f = do
    h <- openFile f ReadMode
    bs <- Data.ByteString.Lazy.hGetContents h
    let (vs,(ms,nmod)) = decode (decompress bs)
    if vs == Acton.Syntax.version
      then do
        hClose h
        return (ms,nmod)
      else do
        putStrLn ("Interface file has version " ++ show vs ++ "; current version is " ++ show Acton.Syntax.version)
        System.Exit.exitFailure
