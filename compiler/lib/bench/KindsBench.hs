import qualified Acton.Env as Env
import qualified Acton.Kinds as Kinds
import qualified Acton.NameInfo as NameInfo
import qualified Acton.Parser as Parser
import qualified Acton.Syntax as Syntax

import Control.DeepSeq (rnf)
import qualified Control.Exception as E
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text.IO as TIO
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import GHC.Stats
import System.Environment (getArgs)
import System.FilePath (takeBaseName)
import System.IO (BufferMode(LineBuffering), hSetBuffering, stdout)

-- Usage:
--   stack build libacton:exe:kinds-bench
--   stack exec kinds-bench -- ../dist/base/out/types path/to/file.act +RTS -T -RTS
--
-- This uses Parser.parseModule, the normal parallel parser entrypoint. The RTS
-- flag is optional. With -T, the driver also prints per-phase allocation and
-- GC deltas.

elapsed label t0 t1 = putStrLn $ label ++ " " ++ show (diffUTCTime t1 t0)

printStats label before after =
    putStrLn $ label ++ " alloc " ++ show alloc ++ " gc_elapsed " ++ show gcElapsed ++ " gcs " ++ show collections
  where alloc = allocated_bytes after - allocated_bytes before
        gcElapsed = gc_elapsed_ns after - gc_elapsed_ns before
        collections = gcs after - gcs before

getStats enabled =
    if enabled then Just <$> getRTSStats else return Nothing

printStatsMaybe label (Just before) (Just after) = printStats label before after
printStatsMaybe _ _ _                            = return ()

forceHTEnv = HashMap.foldl' forceHNameInfo () where
    forceHNameInfo () (NameInfo.HNModule _ te _) = forceHTEnv te
    forceHNameInfo () hni                        = hni `seq` ()

main = do
    hSetBuffering stdout LineBuffering
    args <- getArgs
    case args of
      [typesPath, sourcePath] -> do
        statsEnabled <- getRTSStatsEnabled
        src <- TIO.readFile sourcePath
        env0 <- Env.initEnv typesPath False
        let modName = Syntax.modName [takeBaseName sourcePath]

        s0 <- getStats statsEnabled
        t0 <- getCurrentTime
        parsed <- Parser.parseModule modName sourcePath src Nothing
        E.evaluate (rnf parsed)
        t1 <- getCurrentTime
        s1 <- getStats statsEnabled
        elapsed "parse" t0 t1
        printStatsMaybe "parse_stats" s0 s1

        env <- Env.mkEnv [typesPath] env0 parsed
        E.evaluate (forceHTEnv (Env.hnames env))
        E.evaluate (forceHTEnv (Env.hmodules env))
        t2 <- getCurrentTime
        s2 <- getStats statsEnabled
        elapsed "env" t1 t2
        printStatsMaybe "env_stats" s1 s2

        kchecked <- Kinds.check env parsed
        E.evaluate (rnf kchecked)
        t3 <- getCurrentTime
        s3 <- getStats statsEnabled
        elapsed "kinds" t2 t3
        printStatsMaybe "kinds_stats" s2 s3
      _ ->
        error "usage: kinds-bench TYPES_PATH SOURCE.act"
