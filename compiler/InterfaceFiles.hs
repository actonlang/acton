module InterfaceFiles where

import Data.Binary
import qualified Data.ByteString.Lazy
import Codec.Compression.Zlib
import qualified System.Exit
import qualified Acton.Syntax
import System.IO

writeFile :: Data.Binary.Binary a => FilePath -> a -> IO ()
writeFile f a = do h <- openFile f WriteMode
                   Data.ByteString.Lazy.hPut h (compress (encode (Acton.Syntax.version,a)))
                   hClose h

readFile :: Data.Binary.Binary a => FilePath -> IO a
readFile f = do
      h <- openFile f ReadMode
      bs <- Data.ByteString.Lazy.hGetContents h
      let (vs,a) = decode (decompress bs)
      if vs == Acton.Syntax.version then do hClose h
                                            return a
        else do putStrLn ("Interface file has version "++show vs++"; current version is "++show Acton.Syntax.version)
                System.Exit.exitFailure