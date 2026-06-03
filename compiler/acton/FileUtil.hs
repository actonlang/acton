{-# LANGUAGE CPP #-}
-- Copyright (C) 2026 Centor AB
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

module FileUtil(readFile, writeFile, writeFileAtomic, writeFileChanged, writeFileIfChanged) where

import Prelude hiding (readFile, writeFile)

import Control.Exception (IOException, bracketOnError, catch, evaluate, try)
import Control.Monad (void)
import System.Directory (removeFile, renameFile)
import System.FilePath (takeDirectory)
import System.IO hiding (readFile, writeFile)
import Utils (writeFileUtf8Atomic)

readFile :: FilePath -> IO String
readFile f = do
    h <- openFile f ReadMode
    hSetEncoding h utf8
    c <- hGetContents h
    return c

writeFile :: FilePath -> String -> IO ()
writeFile = writeFileUtf8Atomic

ignoreIOException :: IOException -> IO ()
ignoreIOException _ = return ()

writeFileAtomic :: FilePath -> String -> IO ()
writeFileAtomic f c = do
    let dir = takeDirectory f
    bracketOnError
      (openTempFile dir ".acton-tmp")
      (\(tmpPath, tmpHandle) -> do
          hClose tmpHandle `catch` ignoreIOException
          removeFile tmpPath `catch` ignoreIOException)
      (\(tmpPath, tmpHandle) -> do
          hSetEncoding tmpHandle utf8
          hPutStr tmpHandle c
          hClose tmpHandle
          renameFile tmpPath f `catch` handleRenameError tmpPath)
  where
    handleRenameError :: FilePath -> IOException -> IO ()
    handleRenameError tmpPath _ = do
        removeFile f `catch` ignoreIOException
        renameFile tmpPath f

writeFileIfChanged :: FilePath -> String -> IO ()
writeFileIfChanged f c = void (writeFileChanged f c)

writeFileChanged :: FilePath -> String -> IO Bool
writeFileChanged f c = do
    same <- try (do old <- readFile f
                    evaluate (old == c)) :: IO (Either IOException Bool)
    case same of
      Right True -> return False
      _          -> writeFileAtomic f c >> return True
