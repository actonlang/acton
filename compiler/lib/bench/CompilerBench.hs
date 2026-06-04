import qualified Acton.CommandLineParser as C
import qualified Acton.Compile as Compile
import qualified Acton.Env as Env
import qualified Acton.Kinds as Kinds
import qualified Acton.NameInfo as NameInfo
import qualified Acton.Parser as Parser
import qualified Acton.Syntax as Syntax
import qualified Acton.Types as Types

import Control.DeepSeq (rnf)
import qualified Control.Exception as E
import Control.Monad (forM_)
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import GHC.Stats
import System.Clock (TimeSpec, toNanoSecs)
import System.Environment (getArgs)
import System.FilePath (takeDirectory)
import System.IO (BufferMode(LineBuffering), hSetBuffering, stdout)
import qualified InterfaceFiles

-- Usage:
--   stack build libacton:exe:compiler-bench
--   stack exec compiler-bench -- --parse TYPES_PATH SOURCE.act +RTS -T -RTS
--   stack exec compiler-bench -- --kinds TYPES_PATH SOURCE.act +RTS -T -RTS
--   stack exec compiler-bench -- --types TYPES_PATH SOURCE.act +RTS -T -RTS
--   stack exec compiler-bench -- --front TYPES_PATH SOURCE.act +RTS -T -RTS
--   stack exec compiler-bench -- --front-docs TYPES_PATH SOURCE.act +RTS -T -RTS
--   stack exec compiler-bench -- --pipeline TYPES_PATH SOURCE.act +RTS -T -RTS
--
-- The direct modes stop after the named pass. --front runs the normal compiler
-- front pass without docs, --front-docs includes project front-pass work such
-- as docs, and --pipeline runs front plus back passes.

data DirectMode = ParseOnly | KindsOnly | TypesOnly

elapsed label t0 t1 = putStrLn $ label ++ " " ++ show (diffUTCTime t1 t0)

printStats label before after =
    putStrLn $ label ++ " alloc " ++ show alloc
                    ++ " copied " ++ show copied
                    ++ " max_live " ++ show (max_live_bytes after)
                    ++ " max_mem " ++ show (max_mem_in_use_bytes after)
                    ++ " gc_elapsed " ++ show gcElapsed
                    ++ " gcs " ++ show collections
  where alloc = allocated_bytes after - allocated_bytes before
        copied = copied_bytes after - copied_bytes before
        gcElapsed = gc_elapsed_ns after - gc_elapsed_ns before
        collections = gcs after - gcs before

getStats enabled =
    if enabled then Just <$> getRTSStats else return Nothing

printStatsMaybe label (Just before) (Just after) = printStats label before after
printStatsMaybe _ _ _                            = return ()

forceHTEnv = HashMap.foldl' forceHNameInfo () where
    forceHNameInfo () (NameInfo.HNModule _ te _) = forceHTEnv te
    forceHNameInfo () hni                        = hni `seq` ()

fmtTime :: TimeSpec -> String
fmtTime t = show seconds ++ "s"
  where seconds = (fromIntegral (toNanoSecs t) / 1000000000.0 :: Double)

printFrontTiming :: Compile.FrontTiming -> IO ()
printFrontTiming ft =
    putStrLn $ "front_timing env " ++ fmtTime (Compile.ftEnv ft)
            ++ " kinds " ++ fmtTime (Compile.ftKinds ft)
            ++ " types " ++ fmtTime (Compile.ftTypes ft)
            ++ " reconstruct " ++ fmtTime (Compile.ftTypeReconstruct ft)
            ++ " reconstruct_after_progress " ++ fmtTime (Compile.ftTypeAfterProgress ft)
            ++ " force " ++ fmtTime (Compile.ftTypeForce ft)
            ++ " hash " ++ fmtTime (Compile.ftTypeHash ft)

printBackTiming :: Compile.BackTiming -> IO ()
printBackTiming bt =
    putStrLn $ "back_timing normalize " ++ fmtTime (Compile.btNormalize bt)
            ++ " deactorize " ++ fmtTime (Compile.btDeactorize bt)
            ++ " cps " ++ fmtTime (Compile.btCPS bt)
            ++ " llift " ++ fmtTime (Compile.btLLift bt)
            ++ " boxing " ++ fmtTime (Compile.btBoxing bt)
            ++ " codegen " ++ fmtTime (Compile.btCodeGen bt)
            ++ " render " ++ fmtTime (Compile.btRender bt)
            ++ maybe "" (\t -> " write " ++ fmtTime t) (Compile.btWrite bt)

sysRootFromTypesPath :: FilePath -> FilePath
sysRootFromTypesPath = takeDirectory . takeDirectory . takeDirectory

benchGopts :: C.GlobalOptions
benchGopts = C.GlobalOptions
    { C.color = C.Never
    , C.quiet = False
    , C.noProgress = True
    , C.timing = True
    , C.tty = False
    , C.verbose = False
    , C.verboseZig = False
    , C.jobs = 0
    }

benchOpts :: FilePath -> Bool -> C.CompileOptions
benchOpts typesPath skipBuild =
    Compile.defaultCompileOptions
      { C.ignore_compiler_version = True
      , C.skip_build = skipBuild
      , C.syspath = sysRootFromTypesPath typesPath
      }

nameHashMapFromHeader :: [InterfaceFiles.NameHashInfo] -> Map.Map Syntax.Name InterfaceFiles.NameHashInfo
nameHashMapFromHeader infos =
    Map.fromList [ (InterfaceFiles.nhName info, info) | info <- infos ]

resolveNameHashMap :: Compile.Paths -> Syntax.ModName -> IO (Maybe (Map.Map Syntax.Name InterfaceFiles.NameHashInfo))
resolveNameHashMap paths mn = do
    mty <- Env.findTyFile (Compile.searchPath paths) mn
    case mty of
      Nothing -> return Nothing
      Just ty -> do
        mh <- InterfaceFiles.readHeaderMaybe ty
        return $ case mh of
          Just (_, _, _, _, _, _, nameHashes, _, _, _) -> Just (nameHashMapFromHeader nameHashes)
          Nothing -> Nothing

runDirect :: DirectMode -> FilePath -> FilePath -> IO ()
runDirect mode typesPath sourcePath = do
    statsEnabled <- getRTSStatsEnabled
    src <- readFile sourcePath
    let opts = benchOpts typesPath True
    paths <- Compile.findPaths sourcePath opts
    env0 <- Env.initEnv typesPath False
    let modName = Compile.modName paths

    s0 <- getStats statsEnabled
    t0 <- getCurrentTime
    parsed <- Parser.parseModule modName sourcePath src Nothing
    E.evaluate (rnf parsed)
    t1 <- getCurrentTime
    s1 <- getStats statsEnabled
    elapsed "parse" t0 t1
    printStatsMaybe "parse_stats" s0 s1

    case mode of
      ParseOnly -> return ()
      _ -> do
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

        case mode of
          KindsOnly -> return ()
          _ -> do
            (nmod, tchecked, typeEnv, tests) <- Types.reconstruct Nothing Nothing Nothing env kchecked
            E.evaluate (rnf nmod)
            E.evaluate (rnf tchecked)
            E.evaluate (forceHTEnv (Env.hnames typeEnv))
            E.evaluate (forceHTEnv (Env.hmodules typeEnv))
            E.evaluate (length tests)
            t4 <- getCurrentTime
            s4 <- getStats statsEnabled
            elapsed "types" t3 t4
            printStatsMaybe "types_stats" s3 s4

runCompilerFront :: Bool -> Bool -> FilePath -> FilePath -> IO ()
runCompilerFront buildFront runBack typesPath sourcePath = do
    statsEnabled <- getRTSStatsEnabled
    src <- readFile sourcePath
    srcBytes <- B.readFile sourcePath
    let opts = benchOpts typesPath (not buildFront)
    paths <- Compile.findPaths sourcePath opts
    env0 <- Env.initEnv typesPath False
    let modName = Compile.modName paths

    s0 <- getStats statsEnabled
    t0 <- getCurrentTime
    parsed <- Parser.parseModule modName sourcePath src Nothing
    E.evaluate (rnf parsed)
    t1 <- getCurrentTime
    s1 <- getStats statsEnabled
    elapsed "parse" t0 t1
    printStatsMaybe "parse_stats" s0 s1

    res <- Compile.runFrontPasses
      benchGopts
      opts
      False
      paths
      env0
      parsed
      src
      srcBytes
      Nothing
      (Compile.getPubHashCached paths)
      (Compile.getImplHashCached paths)
      (resolveNameHashMap paths)
      (\_ -> return ())
      (\_ _ -> return ())
      (\_ _ _ -> return ())
      (return True)
      (\_ -> return ())
    fr <- case res of
            Left diags -> error ("front pass failed with " ++ show (length diags) ++ " diagnostics")
            Right fr -> return fr
    if runBack then return () else Compile.waitFrontOutputJobs fr
    E.evaluate (length (Compile.frIfaceTE fr) + length (Compile.frImps fr) + length (Compile.frNameHashes fr))
    t2 <- getCurrentTime
    s2 <- getStats statsEnabled
    elapsed "front" t1 t2
    printStatsMaybe "front_stats" s1 s2
    forM_ (Compile.frFrontTiming fr) printFrontTiming

    case (runBack, Compile.frBackJob fr) of
      (True, Just job) -> do
        (do
            s3 <- getStats statsEnabled
            t3 <- getCurrentTime
            (_mtime, mtiming) <- Compile.runBackPasses benchGopts (Compile.bjOpts job) (Compile.bjPaths job) (Compile.bjInput job) (return True)
            t4 <- getCurrentTime
            s4 <- getStats statsEnabled
            elapsed "back" t3 t4
            printStatsMaybe "back_stats" s3 s4
            forM_ mtiming printBackTiming)
          `E.finally` Compile.waitFrontOutputJobs fr
      (True, Nothing) -> do
        Compile.waitFrontOutputJobs fr
        error "front pass did not return a back job"
      (False, _) -> return ()

main = do
    hSetBuffering stdout LineBuffering
    args <- getArgs
    case args of
      ["--parse", typesPath, sourcePath] ->
        runDirect ParseOnly typesPath sourcePath
      ["--kinds", typesPath, sourcePath] ->
        runDirect KindsOnly typesPath sourcePath
      ["--types", typesPath, sourcePath] ->
        runDirect TypesOnly typesPath sourcePath
      ["--front", typesPath, sourcePath] ->
        runCompilerFront False False typesPath sourcePath
      ["--front-docs", typesPath, sourcePath] ->
        runCompilerFront True False typesPath sourcePath
      ["--pipeline", typesPath, sourcePath] ->
        runCompilerFront True True typesPath sourcePath
      _ ->
        error "usage: compiler-bench (--parse|--kinds|--types|--front|--front-docs|--pipeline) TYPES_PATH SOURCE.act"
