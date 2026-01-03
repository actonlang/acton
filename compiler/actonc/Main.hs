{-# LANGUAGE CPP #-}
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

module Main where

import Prelude hiding (readFile, writeFile)

import qualified Acton.Parser
import qualified Acton.Syntax as A
import Text.Megaparsec.Error (ParseErrorBundle)
import Acton.Parser (CustomParseError)
import qualified Acton.CommandLineParser as C
import Acton.Printer ()
import qualified Acton.Relabel
import qualified Acton.Env
import Acton.Env (simp, define, setMod)
import qualified Acton.QuickType
import qualified Acton.Kinds
import qualified Acton.Types
import Acton.TypeM
import qualified Acton.Solver
import qualified Acton.Normalizer
import qualified Acton.CPS
import qualified Acton.Deactorizer
import qualified Acton.LambdaLifter
import qualified Acton.Boxing
import qualified Acton.CodeGen
import qualified Acton.BuildSpec as BuildSpec
import qualified Acton.Builtin
import qualified Acton.DocPrinter as DocP
import qualified Acton.Diagnostics as Diag
import qualified Acton.SourceProvider as Source
import Acton.Compile
import Utils
import qualified Pretty
import qualified InterfaceFiles

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Exception (throw,catch,displayException,finally,IOException,ErrorCall,try,SomeException,bracket,bracket_,onException,evaluate)
import Control.Exception (bracketOnError)
import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import Control.Monad
import Data.Default.Class (def)
import Data.List.Split
import Data.IORef
import Data.Maybe (catMaybes, isJust, listToMaybe)
import Data.Monoid ((<>))
import Data.Ord
import Data.Graph
import Data.String.Utils (replace)
import Data.Version (showVersion)
import Data.Char (isAlpha, toLower, isSpace)
import qualified Data.List
import Data.Either (partitionEithers)
import qualified Data.Map as M
import qualified Data.Set
import Error.Diagnose
import Error.Diagnose.Style (defaultStyle)
import qualified Filesystem.Path.CurrentOS as Fsco
import GHC.Conc (getNumCapabilities, getNumProcessors, setNumCapabilities, myThreadId, threadCapability)
import Prettyprinter (unAnnotate)
import Prettyprinter.Render.Text (hPutDoc)
import Data.List (isPrefixOf, isSuffixOf, find, intersperse, partition, foldl', nub)
import System.Clock
import System.Directory
import System.Directory.Recursive
import System.Environment (lookupEnv)
import System.Exit
import System.FileLock
import System.FilePath ((</>), addTrailingPathSeparator)
import System.FilePath.Posix
import System.IO hiding (readFile, writeFile)
import Text.PrettyPrint (renderStyle, style, Style(..), Mode(PageMode))
import Text.Show.Pretty (ppDoc)
import System.IO.Temp
import System.IO.Unsafe (unsafePerformIO)
import System.Info
import System.Posix.Files
import System.Process
import qualified System.FSNotify as FS
import qualified System.Environment
import qualified System.Exit
import qualified Paths_actonc
import Text.Printf

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.Binary (encode)
import qualified Data.ByteString.Base16 as Base16
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKM
import Control.Concurrent.QSem (newQSem, waitQSem, signalQSem)

main = do
    hSetBuffering stdout LineBuffering
    arg <- C.parseCmdLine
    -- Ensure enough capabilities: honor --jobs if set, otherwise at least 2 or #procs
    caps0 <- getNumCapabilities
    case arg of
      C.CmdOpt gopts _ -> do
        let req = C.jobs gopts
        when (req > 0 && caps0 < req) $ setNumCapabilities req
        when (req == 0 && caps0 < 2) $ do
          procs <- getNumProcessors
          setNumCapabilities (max 2 procs)
      C.CompileOpt _ gopts _ -> do
        let req = C.jobs gopts
        when (req > 0 && caps0 < req) $ setNumCapabilities req
        when (req == 0 && caps0 < 2) $ do
          procs <- getNumProcessors
          setNumCapabilities (max 2 procs)
    let run = case arg of
          C.CmdOpt gopts (C.New opts)         -> createProject (C.file opts)
          C.CmdOpt gopts (C.Build opts)       -> buildProject gopts opts
          C.CmdOpt gopts (C.Test tcmd)        -> runTests gopts tcmd
          C.CmdOpt gopts C.Fetch             -> fetchCommand gopts
          C.CmdOpt gopts C.PkgShow           -> pkgShow gopts
          C.CmdOpt gopts (C.BuildSpecCmd o)   -> buildSpecCommand o
          C.CmdOpt gopts (C.Cloud opts)       -> undefined
          C.CmdOpt gopts (C.Doc opts)         -> printDocs gopts opts
          C.CmdOpt gopts C.Version            -> printVersion
          C.CompileOpt nms gopts opts         -> case takeExtension (head nms) of
                                                   ".act" -> buildFile gopts (applyGlobalOpts gopts opts) (head nms)
                                                   ".ty" -> printDocs gopts (C.DocOptions (head nms) (Just C.AsciiFormat) Nothing)
                                                   _ -> printErrorAndExit ("Unknown filetype: " ++ head nms)
    run `catch` \(ProjectError msg) -> printErrorAndExit msg

-- Apply global options to compile options
applyGlobalOpts :: C.GlobalOptions -> C.CompileOptions -> C.CompileOptions
applyGlobalOpts gopts opts = opts


-- Auxiliary functions ---------------------------------------------------------------------------------------

optimizeModeToZig :: C.OptimizeMode -> String
optimizeModeToZig C.Debug        = "Debug"
optimizeModeToZig C.ReleaseSafe  = "ReleaseSafe"
optimizeModeToZig C.ReleaseSmall = "ReleaseSmall"
optimizeModeToZig C.ReleaseFast  = "ReleaseFast"

zig :: Paths -> FilePath
zig paths = sysPath paths ++ "/zig/zig"


-- Try to acquire a lock, return Nothing if failed, Just (FileLock, FilePath) if succeeded
tryLock :: FilePath -> IO (Maybe (FileLock, FilePath))
tryLock lockPath = do
    maybeLock <- tryLockFile lockPath Exclusive  -- This will fail immediately if locked
    case maybeLock of
        Nothing -> return Nothing  -- Lock failed
        Just lock -> return $ Just (lock, lockPath)

-- Try locks sequentially until one succeeds or all fail
findAvailableScratch :: FilePath -> IO (Maybe (FileLock, FilePath))
findAvailableScratch basePath = go [0..31]  -- 32 possible scratch directories
  where
    go [] = return Nothing  -- All attempts failed
    go (x:xs) = do
        let lockPath = joinPath [basePath, "scratch" ++ show x ++ ".lock"]
        result <- tryLock lockPath
        case result of
            Just lockInfo -> return $ Just lockInfo
            Nothing -> go xs  -- Try next number

printErrorAndExit msg = do
                  errorWithoutStackTrace msg
                  System.Exit.exitFailure

printErrorAndCleanAndExit msg gopts opts paths = do
                  errorWithoutStackTrace msg
                  cleanup gopts opts paths
                  System.Exit.exitFailure


cleanup gopts opts paths = do
    -- Need platform free path separators
    removeFile (joinPath [projPath paths, ".actonc.lock"])
      `catch` handleNotExists
  where
    handleNotExists :: IOException -> IO ()
    handleNotExists _ = return ()

-- our own readFile & writeFile with hard-coded utf-8 encoding
readFile f = do
    h <- openFile f ReadMode
    hSetEncoding h utf8
    c <- hGetContents h
    return c

writeFile :: FilePath -> String -> IO ()
writeFile f c = do
    h <- openFile f WriteMode
    hSetEncoding h utf8
    hPutStr h c
    hClose h

fmtTime t =
    printf "%6.3f s" secs
  where
    secs :: Float
    secs = (fromIntegral(sec t)) + (fromIntegral (nsec t) / 1000000000)

padRight :: Int -> String -> String
padRight width s
    | len >= width' = s
    | otherwise = s ++ replicate (width' - len) ' '
  where
    width' = max 0 width
    len = length s

-- Version handling ------------------------------------------------------------------------------------------

printVersion = putStrLn getVer

getVer          = showVersion Paths_actonc.version

printIce errMsg = putStrLn(
                        "ERROR: internal compiler error: " ++ errMsg ++
                        "\nNOTE: this is likely a bug in actonc, please report this at:" ++
                        "\nNOTE: https://github.com/actonlang/acton/issues/new?template=ice.yaml" ++
                        "\nNOTE: acton " ++ getVer
                        )

-- Create a project ---------------------------------------------------------------------------------------------

createProject :: String -> IO ()
createProject name = do
    curDir <- getCurrentDirectory
    projDirExists <- doesDirectoryExist name
    iff (projDirExists) $
        printErrorAndExit ("Unable to create project " ++ name ++ ", directory already exists.")
    createDirectoryIfMissing True name
    writeFile (joinPath [ curDir, name, "Acton.toml" ]) ""
    paths <- findPaths (joinPath [ curDir, name, "Acton.toml" ]) defaultCompileOptions
    writeFile (joinPath [ curDir, name, ".gitignore" ]) (
      ".actonc.lock\n" ++
      "build.zig\n" ++
      "build.zig.zon\n" ++
      "out\n"
      )
    writeFile (joinPath [ curDir, name, "README.org" ]) (
      "* " ++ name ++ "\n" ++ name ++ " is a cool Acton project!\n\n\n"
      ++ "** Compile\n\n#+BEGIN_SRC shell\nactonc build\n#+END_SRC\n\n\n"
      ++ "** Run\n\n#+BEGIN_SRC shell\nout/bin/" ++ name ++ "\n#+END_SRC\n\n"
      )
    createDirectoryIfMissing True (srcDir paths)
    writeFile (joinPath [(srcDir paths), name ++ ".act"]) "#\n#\n\nactor main(env):\n    print(\"Hello World!\")\n    env.exit(0)\n"
    putStrLn("Created project " ++ name)
    putStrLn("Enter your new project directory with:\n  cd " ++ name)
    putStrLn("Compile:\n  actonc build")
    putStrLn("Run:\n  ./out/bin/" ++ name)
    gitAvailable <- isGitAvailable
    iff (gitAvailable) $ do
        putStrLn("")
        setCurrentDirectory name
        callProcess "git" ["init"]
        callProcess "git" ["add", "."]
        setCurrentDirectory curDir

-- BuildSpec -----------------------------------------------------------------------------------------------------
buildSpecCommand :: C.BuildSpecCommand -> IO ()
buildSpecCommand cmd =
  case cmd of
    C.BuildSpecUpdate jsonPath -> do
      exists <- doesFileExist "Build.act"
      unless exists $ printErrorAndExit "Build.act not found in current directory"
      let actPath = "Build.act"
      content <- readFile actPath
      json <- if jsonPath == "-"
                then BL.getContents
                else BL.readFile jsonPath
      case BuildSpec.updateBuildActFromJSON content json of
        Left err      -> printErrorAndExit ("Failed to update Build.act: \n" ++ err)
        Right updated -> writeFile actPath updated >> putStrLn "Updated Build.act"
    C.BuildSpecDump -> do
      -- Dump JSON: prefer Build.act, fallback to build.act.json
      existsBuild <- doesFileExist "Build.act"
      if existsBuild
        then do
          content <- readFile "Build.act"
          case BuildSpec.parseBuildAct content of
            Left err        -> printErrorAndExit ("Failed to parse Build.act:\n" ++ err)
            Right (spec,_,_) -> BL.putStr (BuildSpec.encodeBuildSpecJSON spec)
        else do
          jsonExists <- doesFileExist "build.act.json"
          if jsonExists
            then BL.readFile "build.act.json" >>= BL.putStr
            else printErrorAndExit "No Build.act or build.act.json found"

-- Build a project -----------------------------------------------------------------------------------------------

buildProject :: C.GlobalOptions -> C.CompileOptions -> IO ()
buildProject gopts opts = do
                iff (not (null $ (C.root opts)) && (length $ splitOn "." (C.root opts)) == 1) $
                  printErrorAndExit("Project build requires a qualified root actor name, like foo.main")
                if C.watch opts
                  then do
                    curDir <- getCurrentDirectory
                    watchProjectAt gopts opts curDir
                  else buildProjectOnce gopts opts

buildProjectOnce :: C.GlobalOptions -> C.CompileOptions -> IO ()
buildProjectOnce gopts opts = do
                let sp = Source.diskSourceProvider
                curDir <- getCurrentDirectory
                paths <- findPaths (joinPath [ curDir, "Acton.toml" ]) opts
                srcDirExists <- doesDirectoryExist (srcDir paths)
                if not srcDirExists
                  then printErrorAndExit "Missing src/ directory"
                  else do
                    iff (not(C.quiet gopts)) $ do
                      putStrLn("Building project in " ++ projPath paths)
                    withFileLock (joinPath [projPath paths, ".actonc.lock"]) Exclusive $ \_ -> do
                      allFiles <- getFilesRecursive (srcDir paths)
                      let srcFiles = catMaybes $ map filterActFile allFiles
                      compileFiles sp gopts opts srcFiles True
                      generateProjectDocIndex sp gopts opts paths srcFiles

-- Test runner -------------------------------------------------------------------------------------------------

data TestMode = TestModeRun | TestModeList | TestModePerf deriving (Eq, Show)

data TestResult = TestResult
  { trModule       :: String
  , trName         :: String
  , trComplete     :: Bool
  , trSuccess      :: Maybe Bool
  , trException    :: Maybe String
  , trOutput       :: Maybe String
  , trStdOut       :: Maybe String
  , trStdErr       :: Maybe String
  , trFlaky        :: Bool
  , trNumFailures  :: Int
  , trNumErrors    :: Int
  , trNumIterations :: Int
  , trTestDuration :: Double
  , trRaw          :: Aeson.Value
  } deriving (Show)

runTests :: C.GlobalOptions -> C.TestCommand -> IO ()
runTests gopts cmd = do
    let (mode, topts) =
          case cmd of
            C.TestRun opts  -> (TestModeRun, opts)
            C.TestList opts -> (TestModeList, opts)
            C.TestPerf opts -> (TestModePerf, opts)
        opts0 = C.testCompile topts
    let opts = opts0
          { C.test = True
          , C.print_test_bins = False
          , C.skip_build = False
          , C.only_build = False
          }
    curDir <- getCurrentDirectory
    paths <- findPaths (joinPath [ curDir, "Acton.toml" ]) opts
    when (isTmp paths) $
      printErrorAndExit "Acton.toml not found in current directory"
    if C.watch opts0
      then case mode of
             TestModeList -> runTestsOnce gopts opts topts mode paths
             _ -> runTestsWatch gopts opts topts mode paths
      else runTestsOnce gopts opts topts mode paths

runTestsOnce :: C.GlobalOptions -> C.CompileOptions -> C.TestOptions -> TestMode -> Paths -> IO ()
runTestsOnce gopts opts topts mode paths = do
    buildProjectOnce gopts opts
    modules <- listTestModules opts paths
    case mode of
      TestModeList -> listProjectTests opts paths topts modules
      _ -> do
        maxParallel <- testMaxParallel gopts
        exitCode <- runProjectTests opts paths topts mode modules maxParallel
        exitWithTestCode exitCode

runTestsWatch :: C.GlobalOptions -> C.CompileOptions -> C.TestOptions -> TestMode -> Paths -> IO ()
runTestsWatch gopts opts topts mode paths = do
    let sp = Source.diskSourceProvider
        projDir = projPath paths
        srcRoot = srcDir paths
    maxParallel <- testMaxParallel gopts
    hashesRef <- newIORef M.empty
    let runOnce mChanged = do
          iff (not (C.quiet gopts)) $
            putStrLn ("Building project in " ++ projDir)
          withFileLock (joinPath [projDir, ".actonc.lock"]) Exclusive $ \_ -> do
            allFiles <- getFilesRecursive srcRoot
            let srcFiles = catMaybes $ map filterActFile allFiles
            hadErrors <- compileFilesChanged sp gopts opts srcFiles True mChanged
            oldHashes <- readIORef hashesRef
            newHashes <- moduleHashesFromFiles paths srcFiles
            writeIORef hashesRef newHashes
            unless hadErrors $ do
              let changedModules = diffModuleHashes oldHashes newHashes
              testModules <- listTestModules opts paths
              let modulesToTest = filter (`elem` testModules) changedModules
              unless (null modulesToTest) $ do
                _ <- runProjectTests opts paths topts mode modulesToTest maxParallel
                return ()
    runWatchProject gopts projDir srcRoot runOnce

testMaxParallel :: C.GlobalOptions -> IO Int
testMaxParallel gopts = do
    nCaps <- getNumCapabilities
    return (max 1 (if C.jobs gopts > 0 then C.jobs gopts else max 1 (nCaps `div` 2)))

exitWithTestCode :: Int -> IO ()
exitWithTestCode code
  | code <= 0 = System.Exit.exitSuccess
  | otherwise = System.Exit.exitWith (ExitFailure code)

listTestModules :: C.CompileOptions -> Paths -> IO [String]
listTestModules opts paths = do
    let dir = binDir paths
    exists <- doesDirectoryExist dir
    if not exists
      then return []
      else do
        entries <- listDirectory dir
        mods <- forM entries $ \entry -> do
          let base = stripExe entry
          if ".test_" `isPrefixOf` base
            then do
              let full = dir </> entry
              isFile <- doesFileExist full
              if isFile
                then return (Just (drop (length ".test_") base))
                else return Nothing
            else return Nothing
        return (Data.List.sort (catMaybes mods))
  where
    stripExe name
      | isWindowsOS (C.target opts) && takeExtension name == ".exe" = dropExtension name
      | otherwise = name

testBinaryPath :: C.CompileOptions -> Paths -> String -> FilePath
testBinaryPath opts paths modName =
    let base = ".test_" ++ modName
        exe  = if isWindowsOS (C.target opts) then base <.> "exe" else base
    in binDir paths </> exe

listProjectTests :: C.CompileOptions -> Paths -> C.TestOptions -> [String] -> IO ()
listProjectTests opts paths topts modules = do
    let wantedModules = Data.List.sort (filterModules (C.testModules topts) modules)
    tests <- forM wantedModules $ \modName -> do
      names <- listModuleTests opts paths modName
      return (modName, names)
    if null tests
      then do
        putStrLn "No tests found"
        System.Exit.exitSuccess
      else do
        forM_ (Data.List.sortOn fst tests) $ \(modName, names) -> do
          when (not (null names)) $ do
            putStrLn ("Module " ++ modName ++ ":")
            forM_ (Data.List.sort names) $ \name -> do
              let display = displayTestName name
              if display /= name
                then putStrLn ("  " ++ display ++ " (" ++ name ++ ")")
                else putStrLn ("  " ++ display)
            putStrLn ""
        System.Exit.exitSuccess

runProjectTests :: C.CompileOptions -> Paths -> C.TestOptions -> TestMode -> [String] -> Int -> IO Int
runProjectTests opts paths topts mode modules maxParallel = do
    timeStart <- getTime Monotonic
    let wantedModules = Data.List.sort (filterModules (C.testModules topts) modules)
    testsByModule <- forM wantedModules $ \modName -> do
      names <- listModuleTests opts paths modName
      let wantedNames = Data.List.sort (filterTests (C.testNames topts) names)
      return (modName, wantedNames)
    let allTests = concatMap (\(m, names) -> map (\name -> (m, name)) names) testsByModule
    if null allTests
      then do
        putStrLn "Nothing to test"
        return 0
      else do
        putStrLn "Test results:"
        let displayNames = [ formatTestName modName testName | (modName, testName) <- allTests ]
            maxNameLen = maximum (map length displayNames)
            nameWidth = max 20 maxNameLen + 5
        resultChan <- newChan
        printer <- async (printTestResults mode nameWidth (C.testShowLog topts) resultChan)
        results <- runWithLimit maxParallel allTests $ \(modName, testName) -> do
          res <- runModuleTest opts paths topts mode modName testName
          writeChan resultChan (Just res)
          return res
        writeChan resultChan Nothing
        _ <- wait printer
        timeEnd <- getTime Monotonic
        when (C.testGoldenUpdate topts) $
          updateGoldenFiles paths results
        when (C.testRecord topts) $
          writePerfData paths results
        printTestSummary (timeEnd - timeStart) (C.testShowLog topts) results

filterModules :: [String] -> [String] -> [String]
filterModules [] mods = mods
filterModules wanted mods = filter (`elem` wanted) mods

filterTests :: [String] -> [String] -> [String]
filterTests [] names = names
filterTests wanted names =
    filter (\name -> name `elem` wanted || displayTestName name `elem` wanted) names

runWithLimit :: Int -> [a] -> (a -> IO b) -> IO [b]
runWithLimit limit items action = do
    sem <- newQSem (max 1 limit)
    forConcurrently items $ \item ->
      bracket_ (waitQSem sem) (signalQSem sem) (action item)

listModuleTests :: C.CompileOptions -> Paths -> String -> IO [String]
listModuleTests opts paths modName = do
    let binPath = testBinaryPath opts paths modName
    exists <- doesFileExist binPath
    if not exists
      then return []
      else do
        let args = ["list", "--json"]
        (exitCode, _out, err) <- readProcessWithExitCodeCancelable (proc binPath args){ cwd = Just (projPath paths) }
        case exitCode of
          ExitSuccess -> do
            let values = parseJsonLines err
            return (extractTestList values)
          ExitFailure code ->
            printErrorAndExit ("Listing tests failed for module " ++ modName ++ " (exit " ++ show code ++ ")")

runModuleTest :: C.CompileOptions -> Paths -> C.TestOptions -> TestMode -> String -> String -> IO TestResult
runModuleTest opts paths topts mode modName testName = do
    let binPath = testBinaryPath opts paths modName
        cmd = ["test", testName] ++ (if mode == TestModePerf then ["perf"] else []) ++ testCmdArgs topts
    (exitCode, _out, err) <- readProcessWithExitCodeCancelable (proc binPath cmd){ cwd = Just (projPath paths) }
    let values = parseJsonLines err
        infos = extractTestInfo values
    case pickFinalTestInfo infos of
      Just res -> do
        case exitCode of
          ExitSuccess -> return res
          ExitFailure code ->
            return res { trException = Just ("Test process exited with code " ++ show code) }
      Nothing -> do
        let fallback = TestResult
              { trModule = modName
              , trName = testName
              , trComplete = False
              , trSuccess = Nothing
              , trException = Just "No test result received"
              , trOutput = Nothing
              , trStdOut = Nothing
              , trStdErr = Nothing
              , trFlaky = False
              , trNumFailures = 0
              , trNumErrors = 1
              , trNumIterations = 0
              , trTestDuration = 0
              , trRaw = Aeson.Null
              }
        return fallback

testCmdArgs :: C.TestOptions -> [String]
testCmdArgs topts =
    let iter = C.testIter topts
    in if iter > 0
         then ["--max-iter", show iter, "--min-iter", show iter, "--max-time", show (10^6), "--min-time", "1"]
         else [ "--max-iter", show (C.testMaxIter topts)
              , "--min-iter", show (C.testMinIter topts)
              , "--max-time", show (C.testMaxTime topts)
              , "--min-time", show (C.testMinTime topts)
              ]

displayTestName :: String -> String
displayTestName name =
    let withoutPrefix =
          if "_test_" `isPrefixOf` name
            then drop 6 name
            else name
    in if "_wrapper" `isSuffixOf` withoutPrefix
         then take (length withoutPrefix - length "_wrapper") withoutPrefix
         else withoutPrefix

type ModuleHashMap = M.Map String B.ByteString

moduleHashesFromFiles :: Paths -> [FilePath] -> IO ModuleHashMap
moduleHashesFromFiles paths files = do
    pairs <- forM files $ \file -> do
      mn <- moduleNameFromFile (srcDir paths) file
      mhash <- readModuleHash paths mn
      return (fmap (\h -> (modNameToString mn, h)) mhash)
    return (M.fromList (catMaybes pairs))

readModuleHash :: Paths -> A.ModName -> IO (Maybe B.ByteString)
readModuleHash paths mn = do
    let tyFile = outBase paths mn ++ ".ty"
    exists <- doesFileExist tyFile
    if not exists
      then return Nothing
      else do
        hdrE <- (try :: IO a -> IO (Either SomeException a)) $ InterfaceFiles.readHeader tyFile
        case hdrE of
          Left _ -> return Nothing
          Right (hash, _ih, _imps, _roots, _doc) -> return (Just hash)

diffModuleHashes :: ModuleHashMap -> ModuleHashMap -> [String]
diffModuleHashes old new =
    [ modName | (modName, h) <- M.toList new, M.lookup modName old /= Just h ]

formatTestName :: String -> String -> String
formatTestName moduleName testName =
    let display = displayTestName testName
    in if null moduleName
         then display
         else moduleName ++ "." ++ display

formatTestStatus :: TestResult -> String
formatTestStatus res
  | trSuccess res == Just True && trException res == Nothing = "OK"
  | otherwise =
      let base
            | trNumErrors res > 0 && trNumFailures res > 0 = "ERR/FAIL"
            | trNumErrors res > 0 = "ERR"
            | trNumFailures res > 0 = "FAIL"
            | trSuccess res == Just False = "FAIL"
            | otherwise = "ERR"
          prefix = if trFlaky res then "FLAKY " else ""
      in prefix ++ base

formatTestLine :: TestMode -> Int -> TestResult -> String
formatTestLine _ nameWidth res =
    let name = formatTestName (trModule res) (trName res)
        prefix0 = "  " ++ name ++ ": "
        padding = replicate (max 0 (nameWidth - length prefix0)) ' '
        status = formatTestStatus res
        runs = printf "%4d runs in %3.3fms" (trNumIterations res) (trTestDuration res)
    in prefix0 ++ padding ++ status ++ ": " ++ runs

printTestResults :: TestMode -> Int -> Bool -> Chan (Maybe TestResult) -> IO ()
printTestResults mode nameWidth showLog chan = go
  where
    go = do
      evt <- readChan chan
      case evt of
        Nothing -> return ()
        Just res -> do
          putStrLn (formatTestLine mode nameWidth res)
          let ok = trSuccess res == Just True && trException res == Nothing
          unless (ok || showLog) $
            forM_ (trException res) $ \exc ->
              mapM_ (\line -> putStrLn ("    " ++ line)) (lines exc)
          go

parseJsonLines :: String -> [Aeson.Value]
parseJsonLines txt =
    [ v
    | line <- lines txt
    , let trimmed = dropWhile isSpace line
    , not (null trimmed)
    , Just v <- [Aeson.decodeStrict' (B.pack trimmed)]
    ]

extractTestList :: [Aeson.Value] -> [String]
extractTestList values =
    case listToMaybe [ obj | Aeson.Object obj <- values, AesonKM.member (AesonKey.fromString "tests") obj ] of
      Just obj ->
        case AesonKM.lookup (AesonKey.fromString "tests") obj of
          Just (Aeson.Object testsObj) -> map AesonKey.toString (AesonKM.keys testsObj)
          _ -> []
      Nothing -> []

extractTestInfo :: [Aeson.Value] -> [TestResult]
extractTestInfo values =
    catMaybes (map parseTestInfo values)

parseTestInfo :: Aeson.Value -> Maybe TestResult
parseTestInfo val =
    case val of
      Aeson.Object obj ->
        case AesonKM.lookup (AesonKey.fromString "test_info") obj of
          Just infoVal -> AesonTypes.parseMaybe parseTestInfoValue infoVal
          Nothing -> Nothing
      _ -> Nothing

parseTestInfoValue :: Aeson.Value -> AesonTypes.Parser TestResult
parseTestInfoValue = Aeson.withObject "TestInfo" $ \o -> do
    def <- o Aeson..: AesonKey.fromString "definition"
    moduleName <- def Aeson..: AesonKey.fromString "module"
    name <- def Aeson..: AesonKey.fromString "name"
    complete <- o Aeson..: AesonKey.fromString "complete"
    success <- o Aeson..:? AesonKey.fromString "success"
    exception <- o Aeson..:? AesonKey.fromString "exception"
    output <- o Aeson..:? AesonKey.fromString "output"
    stdOut <- o Aeson..:? AesonKey.fromString "std_out"
    stdErr <- o Aeson..:? AesonKey.fromString "std_err"
    flaky <- o Aeson..:? AesonKey.fromString "flaky" Aeson..!= False
    numFailures <- o Aeson..:? AesonKey.fromString "num_failures" Aeson..!= 0
    numErrors <- o Aeson..:? AesonKey.fromString "num_errors" Aeson..!= 0
    numIterations <- o Aeson..:? AesonKey.fromString "num_iterations" Aeson..!= 0
    testDuration <- o Aeson..:? AesonKey.fromString "test_duration" Aeson..!= 0
    return TestResult
      { trModule = moduleName
      , trName = name
      , trComplete = complete
      , trSuccess = success
      , trException = exception
      , trOutput = output
      , trStdOut = stdOut
      , trStdErr = stdErr
      , trFlaky = flaky
      , trNumFailures = numFailures
      , trNumErrors = numErrors
      , trNumIterations = numIterations
      , trTestDuration = testDuration
      , trRaw = Aeson.Object o
      }

pickFinalTestInfo :: [TestResult] -> Maybe TestResult
pickFinalTestInfo infos =
    case reverse infos of
      [] -> Nothing
      xs ->
        case listToMaybe [i | i <- xs, trComplete i] of
          Just i -> Just i
          Nothing -> Just (head xs)

printTestSummary :: TimeSpec -> Bool -> [TestResult] -> IO Int
printTestSummary elapsed showLog results = do
    let total = length results
        failures = length [ r | r <- results, trSuccess r == Just False ]
        errors = length [ r | r <- results, trSuccess r == Nothing ]
    case total of
      0 -> do
        putStrLn "Nothing to test"
        return 0
      _ -> do
        if errors > 0 && failures > 0
          then putStrLn (show errors ++ " error and " ++ show failures ++ " failure out of " ++ show total ++ " tests (" ++ fmtTime elapsed ++ ")")
          else if errors > 0
            then putStrLn (show errors ++ " out of " ++ show total ++ " tests errored (" ++ fmtTime elapsed ++ ")")
            else if failures > 0
              then putStrLn (show failures ++ " out of " ++ show total ++ " tests failed (" ++ fmtTime elapsed ++ ")")
              else putStrLn ("All " ++ show total ++ " tests passed (" ++ fmtTime elapsed ++ ")")
        when showLog $
          printTestDetails showLog results
        if errors > 0
          then return 2
          else if failures > 0
            then return 1
            else return 0

printTestDetails :: Bool -> [TestResult] -> IO ()
printTestDetails showLog results = do
    forM_ results $ \res -> do
      let status = case trSuccess res of
                     Just True -> "OK"
                     Just False -> "FAIL"
                     Nothing -> "ERR"
          display = displayTestName (trName res)
      when (showLog || status /= "OK") $ do
        putStrLn ("Test " ++ trModule res ++ "." ++ display ++ ": " ++ status)
        case trException res of
          Just exc -> putStrLn ("  " ++ exc)
          Nothing -> return ()
        when (showLog || status /= "OK") $ do
          case trOutput res of
            Just out | not (null out) -> putStrLn ("  Output:\n" ++ out)
            _ -> return ()
          case trStdOut res of
            Just out | not (null out) -> putStrLn ("  Stdout:\n" ++ out)
            _ -> return ()
          case trStdErr res of
            Just out | not (null out) -> putStrLn ("  Stderr:\n" ++ out)
            _ -> return ()

updateGoldenFiles :: Paths -> [TestResult] -> IO ()
updateGoldenFiles paths results =
    forM_ results $ \res -> do
      case (trException res, trOutput res) of
        (Just exc, Just out)
          | "testing.NotEqualError: Test output does not match expected golden value" `isPrefixOf` exc -> do
              let fileName = displayTestName (trName res)
                  goldenDir = joinPath [projPath paths, "test", "golden", trModule res]
              createDirectoryIfMissing True goldenDir
              writeFile (goldenDir </> fileName) out
        _ -> return ()

writePerfData :: Paths -> [TestResult] -> IO ()
writePerfData paths results = do
    let addTest acc res =
          let modKey = AesonKey.fromString (trModule res)
              testKey = AesonKey.fromString (trName res)
              entry = case AesonKM.lookup modKey acc of
                        Just (Aeson.Object obj) -> obj
                        _ -> AesonKM.empty
              entry' = AesonKM.insert testKey (trRaw res) entry
              acc' = AesonKM.insert modKey (Aeson.Object entry') acc
          in acc'
        modulesObj = foldl' addTest AesonKM.empty results
        outVal = Aeson.Object modulesObj
        outPath = joinPath [projPath paths, "perf_data"]
    BL.writeFile outPath (Aeson.encode outVal)

watchProjectAt :: C.GlobalOptions -> C.CompileOptions -> FilePath -> IO ()
watchProjectAt gopts opts projDir = do
                let sp = Source.diskSourceProvider
                paths <- findPaths (joinPath [ projDir, "Acton.toml" ]) opts
                srcDirExists <- doesDirectoryExist (srcDir paths)
                if not srcDirExists
                  then printErrorAndExit "Missing src/ directory"
                  else do
                    let runOnce mChanged = do
                          iff (not(C.quiet gopts)) $ do
                            putStrLn("Building project in " ++ projPath paths)
                          withFileLock (joinPath [projPath paths, ".actonc.lock"]) Exclusive $ \_ -> do
                            allFiles <- getFilesRecursive (srcDir paths)
                            let srcFiles = catMaybes $ map filterActFile allFiles
                            void $ compileFilesChanged sp gopts opts srcFiles True mChanged
                            when (isNothing mChanged) $
                              generateProjectDocIndex sp gopts opts paths srcFiles
                    runWatchProject gopts (projPath paths) (srcDir paths) runOnce

buildFile :: C.GlobalOptions -> C.CompileOptions -> FilePath -> IO ()
buildFile gopts opts file
    | C.watch opts = watchFile gopts opts file
    | otherwise = buildFileOnce gopts opts file

buildFileOnce :: C.GlobalOptions -> C.CompileOptions -> FilePath -> IO ()
buildFileOnce gopts opts file = do
    let sp = Source.diskSourceProvider
    absFile <- canonicalizePath file
    curDir <- getCurrentDirectory
    -- Determine if we are in a project
    projDir <- findProjectDir absFile
    case projDir of
      Just proj -> do
        let relProj = makeRelative curDir proj
        -- In a project, use project directory for compilation
        -- If we are running as a sub-compiler, we just compile directly without
        -- locking since we assume the parent compiler has already locked the
        -- project and may run multiple sub-compilers concurrently
        iff (not(C.quiet gopts)) $ do
          putStrLn("Building file " ++ file ++ " in project " ++ relProj)
        if (C.sub gopts)
          then do
            compileFiles sp gopts opts [file] False
          else do
            -- grab project lock
            let lock_file = joinPath [proj, ".actonc.lock"]
            withFileLock lock_file Exclusive $ \_ -> do
              compileFiles sp gopts opts [file] False
      Nothing -> do
        -- Not in a project, use scratch directory for compilation unless
        -- --tempdir is provided - then use that
        if (C.tempdir opts /= "")
          then do
            iff (not(C.quiet gopts)) $ do
              putStrLn("Building file " ++ file ++ " using temporary directory " ++ C.tempdir opts)
            compileFiles sp gopts opts [file] False
          else do
            home <- getHomeDirectory
            let basePath = joinPath [home, ".cache", "acton", "scratch"]
            createDirectoryIfMissing True basePath
            maybeLockInfo <- findAvailableScratch basePath
            case maybeLockInfo of
              Nothing -> error "Could not acquire any scratch directory lock"
              Just (lock, lockPath) -> do
                let scratchDir = dropExtension lockPath
                iff (not(C.quiet gopts)) $ do
                  let scratch_dir = if (C.verbose gopts) then " " ++ scratchDir else ""
                  putStrLn("Building file " ++ file ++ " using temporary scratch directory" ++ scratch_dir)
                removeDirectoryRecursive scratchDir `catch` handleNotExists
                compileFiles sp gopts (opts { C.tempdir = scratchDir }) [file] False
                unlockFile lock
  where
    handleNotExists :: IOException -> IO ()
    handleNotExists _ = return ()

watchFile :: C.GlobalOptions -> C.CompileOptions -> FilePath -> IO ()
watchFile gopts opts file = do
    absFile <- canonicalizePath file
    projDir <- findProjectDir absFile
    case projDir of
      Just proj -> watchProjectAt gopts opts proj
      Nothing -> do
        let sp = Source.diskSourceProvider
        if (C.tempdir opts /= "")
          then do
            let runOnce mChanged = void $ compileFilesChanged sp gopts opts [absFile] False mChanged
            runWatchFile gopts absFile runOnce
          else do
            home <- getHomeDirectory
            let basePath = joinPath [home, ".cache", "acton", "scratch"]
            createDirectoryIfMissing True basePath
            maybeLockInfo <- findAvailableScratch basePath
            case maybeLockInfo of
              Nothing -> error "Could not acquire any scratch directory lock"
              Just (lock, lockPath) -> do
                let scratchDir = dropExtension lockPath
                    opts' = opts { C.tempdir = scratchDir }
                removeDirectoryRecursive scratchDir `catch` handleNotExists
                let runOnce mChanged = void $ compileFilesChanged sp gopts opts' [absFile] False mChanged
                runWatchFile gopts absFile runOnce `finally` unlockFile lock
  where
    handleNotExists :: IOException -> IO ()
    handleNotExists _ = return ()

watchDebounceMicros :: Int
watchDebounceMicros = 200000

data WatchTrigger = WatchFull | WatchIncremental FilePath

dispatchWatchTrigger :: (Maybe FilePath -> IO ()) -> WatchTrigger -> IO ()
dispatchWatchTrigger notify trigger =
    case trigger of
      WatchFull -> notify Nothing
      WatchIncremental path -> notify (Just path)

actWatchTrigger :: FS.Event -> Maybe WatchTrigger
actWatchTrigger ev =
    case FS.eventIsDirectory ev of
      FS.IsDirectory ->
        case ev of
          FS.Added{} -> Just WatchFull
          FS.Removed{} -> Just WatchFull
          FS.WatchedDirectoryRemoved{} -> Just WatchFull
          FS.Unknown{} -> Just WatchFull
          _ -> Nothing
      FS.IsFile ->
        if takeExtension (FS.eventPath ev) /= ".act"
          then Nothing
          else case ev of
                 FS.Added{} -> Just WatchFull
                 FS.Removed{} -> Just WatchFull
                 FS.WatchedDirectoryRemoved{} -> Just WatchFull
                 FS.Unknown{} -> Just WatchFull
                 _ -> Just (WatchIncremental (FS.eventPath ev))

fileWatchTrigger :: FilePath -> FS.Event -> Maybe WatchTrigger
fileWatchTrigger target ev =
    let evPath = normalise (FS.eventPath ev)
    in if evPath /= target
         then Nothing
         else Just (WatchIncremental (FS.eventPath ev))

newWatchScheduler :: (Maybe [FilePath] -> IO ()) -> IO (Maybe FilePath -> IO (), IO ())
newWatchScheduler runBuild = do
    compileAsyncRef <- newMVar Nothing
    genRef <- newIORef 0
    changedRef <- newIORef Data.Set.empty
    fullRef <- newIORef False
    let schedule delay = do
          gen <- atomicModifyIORef' genRef $ \g -> let g' = g + 1 in (g', g')
          modifyMVar_ compileAsyncRef $ \m -> do
            forM_ m cancel
            a <- async $ do
              when (delay > 0) $ threadDelay delay
              current <- readIORef genRef
              when (current == gen) $ do
                full <- atomicModifyIORef' fullRef $ \f -> (False, f)
                paths <- atomicModifyIORef' changedRef $ \s -> (Data.Set.empty, Data.Set.toList s)
                if full || null paths
                  then runBuild Nothing
                  else runBuild (Just paths)
            return (Just a)
    let notify mpath = do
          case mpath of
            Nothing -> writeIORef fullRef True
            Just p -> modifyIORef' changedRef (Data.Set.insert p)
          schedule watchDebounceMicros
        startNow = do
          writeIORef fullRef True
          schedule 0
    return (notify, startNow)

runWatchProject :: C.GlobalOptions -> FilePath -> FilePath -> (Maybe [FilePath] -> IO ()) -> IO ()
runWatchProject gopts projDir srcRoot runOnce = do
    (notify, startNow) <- newWatchScheduler runOnce
    let onAct ev = forM_ (actWatchTrigger ev) (dispatchWatchTrigger notify)
        onRoot _ = notify Nothing
        isActEvent ev = isJust (actWatchTrigger ev)
        isRootEvent ev =
          let name = takeFileName (FS.eventPath ev)
          in name `elem` ["Acton.toml", "Build.act", "build.act.json"]
    FS.withManager $ \mgr -> do
      _ <- FS.watchTree mgr srcRoot isActEvent onAct
      _ <- FS.watchDir mgr projDir isRootEvent onRoot
      startNow
      unless (C.quiet gopts) $
        putStrLn ("Watching for changes in " ++ projDir)
      forever $ threadDelay maxBound

runWatchFile :: C.GlobalOptions -> FilePath -> (Maybe [FilePath] -> IO ()) -> IO ()
runWatchFile gopts absFile runOnce = do
    (notify, startNow) <- newWatchScheduler runOnce
    let watchDir = takeDirectory absFile
        watchPath = normalise absFile
        isTarget ev = normalise (FS.eventPath ev) == watchPath
        onEvent ev = forM_ (fileWatchTrigger watchPath ev) (dispatchWatchTrigger notify)
    FS.withManager $ \mgr -> do
      _ <- FS.watchDir mgr watchDir isTarget onEvent
      startNow
      unless (C.quiet gopts) $
        putStrLn ("Watching for changes in " ++ absFile)
      forever $ threadDelay maxBound

-- Fetch dependencies only (no compilation), mirroring `acton fetch`
fetchCommand :: C.GlobalOptions -> IO ()
fetchCommand gopts = do
    curDir <- getCurrentDirectory
    let actPath = joinPath [curDir, "Acton.toml"]
    actExists <- doesFileExist actPath
    unless actExists $
      printErrorAndExit "Acton.toml not found in current directory"
    srcExists <- doesDirectoryExist (joinPath [curDir, "src"])
    unless srcExists $
      printErrorAndExit "Missing src/ directory"
    paths <- findPaths actPath defaultCompileOptions
    fetchDependencies gopts paths []
    unless (C.quiet gopts) $
      putStrLn "Dependencies fetched"

-- Show dependency tree with overrides applied from root pins
pkgShow :: C.GlobalOptions -> IO ()
pkgShow gopts = do
    curDir <- getCurrentDirectory
    let actPath = joinPath [curDir, "Acton.toml"]
    actExists <- doesFileExist actPath
    unless actExists $
      printErrorAndExit "Acton.toml not found in current directory"
    mspec <- loadBuildSpec curDir
    case mspec of
      Nothing -> printErrorAndExit "No Build.act/build.act.json found"
      Just spec -> do
        let rootPins = BuildSpec.dependencies spec
        unless (C.quiet gopts) $
          putStrLn "Dependency tree (hash overrides shown):"
        showTree rootPins curDir spec 0
  where
    describeDep dep =
      case BuildSpec.hash dep of
        Just h -> "hash=" ++ h
        Nothing -> case BuildSpec.path dep of
                     Just p | not (null p) -> "path=" ++ p
                     _ -> "unversioned"

    showTree pins dir spec depth = do
      let deps = M.toList (BuildSpec.dependencies spec)
      forM_ deps $ \(depName, dep) -> do
        let (chosen, conflict) =
              case M.lookup depName pins of
                Nothing -> (dep, False)
                Just pinDep -> if pinDep == dep then (dep, False) else (pinDep, True)
            prefix = replicate (2*depth) ' ' ++ "- "
            line = prefix ++ depName ++ " (" ++ describeDep dep ++
                   (if conflict then " overridden -> " ++ describeDep chosen else "") ++ ")"
        putStrLn line
        depBase <- resolveDepBase dir depName chosen
        mspec <- loadBuildSpec depBase
        case mspec of
          Nothing -> return ()
          Just spec' -> showTree pins depBase spec' (depth + 1)

-- Print documentation -------------------------------------------------------------------------------------------

-- | Detect if we're running in a GUI environment
-- On macOS: Always assume GUI unless SSH_CONNECTION is set
-- On Linux: Check for DISPLAY variable
-- On Windows: Always assume GUI
detectGuiEnvironment :: IO Bool
detectGuiEnvironment = do
    case System.Info.os of
        "darwin" -> do
            -- On macOS, assume GUI unless we're in SSH session
            sshConn <- lookupEnv "SSH_CONNECTION"
            return $ isNothing sshConn
        "linux" -> do
            -- On Linux, check for DISPLAY
            display <- lookupEnv "DISPLAY"
            return $ isJust display && display /= Just ""
        _ ->
            -- Windows or other, assume GUI
            return True

-- | Generate and display documentation for Acton modules
--
-- Behavior summary:
-- * No format flag + GUI environment → HTML to file + open browser
-- * No format flag + terminal/SSH → ASCII to stdout
-- * Explicit format (-t, --html, --md) → Always to stdout (unless -o specified)
-- * -o flag → Always to that file
-- * -o - → Always to stdout
--
-- GUI detection:
-- * macOS: Assumes GUI unless SSH_CONNECTION is set
-- * Linux: Checks for DISPLAY variable
-- * Windows: Always assumes GUI
--
-- The command line parser just parses options without making behavior decisions.
-- All logic is centralized here in printDocs for predictable, intuitive behavior.
printDocs :: C.GlobalOptions -> C.DocOptions -> IO ()
printDocs gopts opts = do
    if null (C.inputFile opts) then do
        -- No file provided - check what to do
        case C.outputFormat opts of
            Just C.AsciiFormat -> printErrorAndExit "Terminal output requires a specific file. Usage: actonc doc -t <file.act>"
            Just C.MarkdownFormat -> printErrorAndExit "Markdown output requires a specific file. Usage: actonc doc --md <file.act>"
            _ -> do
                -- HTML or auto mode - check if we're in a project
                curDir <- getCurrentDirectory
                projDir <- findProjectDir curDir
                if isJust projDir then do
                    -- We're in a project - open the documentation index
                    let indexFile = "out/doc/index.html"
                    indexExists <- doesFileExist indexFile
                    if indexExists then do
                        -- Open the existing index
                        let openCmd = case System.Info.os of
                                "darwin" -> "open"
                                "linux" -> "xdg-open"
                                _ -> ""
                        unless (null openCmd) $ do
                            _ <- system $ openCmd ++ " " ++ indexFile
                            return ()
                    else
                        printErrorAndExit "No documentation found. Run 'actonc build' first to generate documentation."
                else
                    printErrorAndExit "Not in an Acton project. Please specify a file to document."
    else do
        let filename = C.inputFile opts
            (fileBody,fileExt) = splitExtension $ takeFileName filename

        case fileExt of
            ".ty" -> do
                paths <- findPaths filename defaultCompileOptions
                env0 <- Acton.Env.initEnv (sysTypes paths) False
                Acton.Types.showTyFile env0 (modName paths) filename

            ".act" -> do
                let modname = A.modName $ map (replace ".act" "") $ splitOn "/" $ fileBody
                paths <- findPaths filename defaultCompileOptions
                parsedRes <- parseActFile Source.diskSourceProvider modname filename
                (_snap, parsed) <- case parsedRes of
                  Left diags -> do
                    printDiagnostics gopts defaultCompileOptions diags
                    System.Exit.exitFailure
                  Right res -> return res

                -- Run compiler passes to get type information
                env0 <- Acton.Env.initEnv (sysTypes paths) False
                env <- Acton.Env.mkEnv (searchPath paths) env0 parsed
                kchecked <- Acton.Kinds.check env parsed
                (nmod, _, env', _) <- Acton.Types.reconstruct env kchecked
                let A.NModule tenv mdoc = nmod

                -- 1. If format is explicitly set (via -t, --html, --markdown), use it
                -- 2. Otherwise, check if we're in a GUI environment
                inGui <- detectGuiEnvironment
                let format = case C.outputFormat opts of
                        Just fmt -> fmt
                        Nothing -> if inGui then C.HtmlFormat else C.AsciiFormat

                docOutput <- case format of
                    C.HtmlFormat -> return $ DocP.printHtmlDoc nmod parsed
                    C.AsciiFormat -> do
                        shouldColor <- useColor gopts
                        return $ DocP.printAsciiDoc shouldColor nmod parsed
                    C.MarkdownFormat -> return $ DocP.printMdDoc nmod parsed

                -- Handle output destination
                case C.outputFile opts of
                    Just "-" ->
                        -- Explicit stdout
                        putStr docOutput
                    Just outFile -> do
                        -- Write to specified file
                        createDirectoryIfMissing True (takeDirectory outFile)
                        writeFile outFile docOutput
                        putStrLn $ "Documentation written to: " ++ outFile
                    Nothing ->
                        -- No explicit output file
                        if isJust (C.outputFormat opts) then
                            -- Format was explicitly set, write to stdout
                            putStr docOutput
                        else if format == C.AsciiFormat then
                            -- Auto-detected ASCII (no DISPLAY), write to stdout
                            putStr docOutput
                        else do
                            -- Auto-detected HTML (DISPLAY set), write to file and open browser
                            curDir <- getCurrentDirectory
                            projDir <- findProjectDir curDir
                            outputPath <- if isJust projDir then do
                                -- In project: use out/doc/
                                let modPath = map (replace ".act" "") $ splitOn "/" filename
                                    cleanPath = case modPath of
                                        "src":rest -> rest
                                        path -> path
                                    docFile = if null cleanPath
                                              then "out/doc/unnamed.html"
                                              else joinPath ("out" : "doc" : init cleanPath) </> last cleanPath <.> "html"
                                return docFile
                            else do
                                -- Outside project: use temp file
                                writeSystemTempFile "acton-doc.html" docOutput

                            -- Write the file
                            when (isJust projDir) $ do
                                createDirectoryIfMissing True (takeDirectory outputPath)
                                writeFile outputPath docOutput

                            putStrLn $ "HTML documentation written to: " ++ outputPath

                            -- Open in browser
                            let openCmd = case System.Info.os of
                                    "darwin" -> "open"
                                    "linux" -> "xdg-open"
                                    _ -> ""
                            unless (null openCmd) $ do
                                _ <- system $ openCmd ++ " " ++ outputPath
                                return ()

            _ -> printErrorAndExit ("Unknown filetype: " ++ filename)


-- Compile Acton files ---------------------------------------------------------------------------------------------

compileFiles :: Source.SourceProvider -> C.GlobalOptions -> C.CompileOptions -> [String] -> Bool -> IO ()
compileFiles sp gopts opts srcFiles allowPrune =
    void $ compileFilesChanged sp gopts opts srcFiles allowPrune Nothing

compileFilesChanged :: Source.SourceProvider -> C.GlobalOptions -> C.CompileOptions -> [String] -> Bool -> Maybe [FilePath] -> IO Bool
compileFilesChanged sp gopts opts srcFiles allowPrune mChangedPaths = do
    cwd <- getCurrentDirectory
    maybeRoot <- findProjectDir (takeDirectory (head srcFiles))
    let baseForOverrides = maybe cwd id maybeRoot
    depOverrides <- normalizeDepOverrides baseForOverrides (C.dep_overrides opts)
    let opts' = opts { C.dep_overrides = depOverrides }
        watchMode = C.watch opts'
        incremental = isJust mChangedPaths
        allowPrune' = allowPrune && not incremental
    nCaps <- getNumCapabilities
    let maxParallel = max 1 (if C.jobs gopts > 0 then C.jobs gopts else nCaps)
    progressUI <- initProgressUI gopts maxParallel
    frontState <- newProgressState
    let logLine = progressLogLine progressUI
        logDiagnostics optsT diags =
          progressWithLog progressUI (printDiagnostics gopts optsT diags)
        shouldLogInfo msg =
          not (any (`isPrefixOf` msg) [ "  Compiling "
                                      , "  Building [cap "
                                      , "  Stale "
                                      , "  Fresh "
                                      ])
    pathsRoot <- findPaths (head srcFiles) opts'
    rootProj  <- normalizePathSafe (projPath pathsRoot)
    sysAbs    <- normalizePathSafe (sysPath pathsRoot)
    fetchDependencies gopts pathsRoot depOverrides
    projMap   <- if isTmp pathsRoot
                   then do
                     let ctx = ProjCtx { projRoot = rootProj
                                       , projOutDir = projOut pathsRoot
                                       , projTypesDir = projTypes pathsRoot
                                       , projSrcDir = srcDir pathsRoot
                                       , projSysPath = sysAbs
                                       , projSysTypes = joinPath [sysAbs, "base", "out", "types"]
                                       , projBuildSpec = Nothing
                                       , projLocks = joinPath [projPath pathsRoot, ".actonc.lock"]
                                       , projDeps = []
                                       }
                     return (M.singleton rootProj ctx)
                   else discoverProjects sysAbs rootProj depOverrides
    let sysRoot = addTrailingPathSeparator sysAbs
    (globalTasks, _) <- buildGlobalTasks sp gopts opts' projMap (if incremental || allowPrune then Nothing else Just srcFiles)
    neededTasks <- case mChangedPaths of
                     Nothing -> selectNeededTasks pathsRoot rootProj globalTasks srcFiles
                     Just changed -> selectAffectedTasks globalTasks changed
    let modWidth = maximum (0 : [ length (modNameToString (tkMod (gtKey t))) | t <- neededTasks ])
        padMod mn = padRight modWidth (modNameToString mn)
        spinnerPrefixWidth = 3
        frontPrefix = "   Finished typecheck of "
        backPrefix = "   Finished compilation of "
        phaseFront = "Running front passes:"
        phaseBack = "Running back passes:"
        phaseWidth = max (length phaseFront) (length phaseBack)
        projWidth = maximum (0 : [ length (projectLabel rootProj (tkProj (gtKey t))) | t <- neededTasks ])
        prefixWidth = maximum [ length frontPrefix
                              , length backPrefix
                              , spinnerPrefixWidth + phaseWidth + 1
                              ]
        progressPrefixWidth = max 0 (prefixWidth - spinnerPrefixWidth)
        padProgressPrefix phase = padRight progressPrefixWidth (padRight phaseWidth phase ++ " ")
        projPart proj = padRight projWidth (projectLabel rootProj proj) ++ "/"
        completionPrefix prefix proj = padRight prefixWidth prefix ++ projPart proj
        frontDoneLine proj mn t = completionPrefix frontPrefix proj ++ padMod mn ++ " in  " ++ fmtTime t
        backDoneLine proj mn t = completionPrefix backPrefix proj ++ padMod mn ++ " in  " ++ fmtTime t
        progressLine phase proj mn = padProgressPrefix phase ++ projPart proj ++ modNameToString mn
        rootTasks   = [ gtTask t | t <- neededTasks, tkProj (gtKey t) == rootProj ]
        rootPins    = maybe M.empty BuildSpec.dependencies (projBuildSpec =<< M.lookup rootProj projMap)
    iff (C.listimports opts') $ do
        let module_imports = map (\t -> concat [ modNameToString (name t), ": "
                                               , (concat $ intersperse " " (map modNameToString (importsOf t))) ]) rootTasks
            output = concat $ intersperse "\n" module_imports
        putStrLn output
        System.Exit.exitSuccess
    backJobsRef <- newIORef []
    let callbacks = defaultCompileCallbacks
          { ccOnDiagnostics = \_ optsT diags -> logDiagnostics optsT diags
          , ccOnFrontResult = \t _ fr -> forM_ (frFrontTime fr) (\tFront ->
              logLine (frontDoneLine (tkProj (gtKey t)) (tkMod (gtKey t)) tFront))
          , ccOnFrontStart = \t _ ->
              let key = gtKey t
                  proj = tkProj key
                  mn = tkMod key
              in progressStartTask progressUI frontState key (progressLine phaseFront proj mn)
          , ccOnFrontDone = \t _ -> progressDoneTask progressUI frontState (gtKey t)
          , ccOnBackJob = \job -> modifyIORef' backJobsRef (job :)
          , ccOnInfo = \msg -> when (shouldLogInfo msg) (logLine msg)
          }
    compileRes <- compileTasks sp gopts opts' pathsRoot rootProj neededTasks callbacks
    progressSetLines progressUI []
    case compileRes of
      Left err -> do
        cleanup gopts opts' pathsRoot
        if watchMode
          then logLine (compileFailureMessage err)
          else printErrorAndExit (compileFailureMessage err)
        return True
      Right (env, hadErrors) -> do
        backJobs <- reverse <$> readIORef backJobsRef
        when (not (null backJobs) && not (C.only_build opts')) $ do
          backState <- newProgressState
          let backJobKey job =
                TaskKey (projPath (bjPaths job)) (A.modname (biTypedMod (bjInput job)))
              backJobLine job =
                let proj = projPath (bjPaths job)
                    mn = A.modname (biTypedMod (bjInput job))
                in progressLine phaseBack proj mn
              onBackStart job = progressStartTask progressUI backState (backJobKey job) (backJobLine job)
              onBackDone job mtime = do
                progressDoneTask progressUI backState (backJobKey job)
                forM_ mtime (\tBack ->
                  logLine (backDoneLine (projPath (bjPaths job)) (A.modname (biTypedMod (bjInput job))) tBack))
          runBackJobs gopts maxParallel onBackStart onBackDone backJobs
          progressSetLines progressUI []
        if hadErrors
          then do
            cleanup gopts opts' pathsRoot
            unless watchMode System.Exit.exitFailure
            return True
          else do
            let rootParts = splitOn "." (C.root opts')
                rootMod   = init rootParts
                guessMod  = if length rootParts == 1 then modName pathsRoot else A.modName rootMod
                binTask   = BinTask False (prstr guessMod) (A.GName guessMod (A.name $ last rootParts)) False
                preBinTasks
                  | null (C.root opts') = map (\t -> BinTask True (modNameToString (name t)) (A.GName (name t) (A.name "main")) False) rootTasks
                  | otherwise        = [binTask]
                preTestBinTasks = map (\t -> BinTask True (modNameToString (name t)) (A.GName (name t) (A.name "__test_main")) True) rootTasks
            -- Generate build.zig(.zon) for dependencies too, to satisfy Zig builder links.
            let projKeys = Data.Set.fromList (map (tkProj . gtKey) globalTasks)
            forM_ (Data.Set.toList projKeys) $ \p -> do
              let isRootProj = p == rootProj
                  isSysProj  = p == sysAbs || sysRoot `isPrefixOf` p
              unless (isRootProj || isSysProj) $
                case M.lookup p projMap of
                  Just ctx -> do
                    when (C.verbose gopts) $
                      putStrLn ("Generating build.zig for dependency project " ++ p)
                    dummyPaths <- pathsForModule opts' projMap ctx (A.modName ["__gen_build__"])
                    genBuildZigFiles rootPins depOverrides dummyPaths
                  Nothing -> return ()
            if C.skip_build opts'
              then
                logLine "  Skipping final build step"
              else
                if C.test opts'
                  then do
                    testBinTasks <- catMaybes <$> mapM (filterMainActor env pathsRoot) preTestBinTasks
                    compileBins gopts opts' pathsRoot env rootTasks testBinTasks allowPrune'
                    when (C.print_test_bins opts') $ do
                      logLine "Test executables:"
                      mapM_ (\t -> logLine (binName t)) testBinTasks
                  else do
                    compileBins gopts opts' pathsRoot env rootTasks preBinTasks allowPrune'
            return False
-- Generate documentation index for a project build by reading module docstrings
-- from the current tasks. Uses TyTask header docs when available to avoid
-- parsing/decoding; falls back to extracting from ActonTask ASTs.
generateProjectDocIndex :: Source.SourceProvider -> C.GlobalOptions -> C.CompileOptions -> Paths -> [String] -> IO ()
generateProjectDocIndex sp gopts opts paths srcFiles = do
    unless (C.skip_build opts || C.only_build opts || isTmp paths) $ do
        let docDir = joinPath [projPath paths, "out", "doc"]
        createDirectoryIfMissing True docDir
        tasks <- mapM (\f -> findPaths f opts >>= \p -> readModuleTask sp gopts opts p f) srcFiles
        entries <- catMaybes <$> forM tasks (\t -> case t of
                          ActonTask mn _src _bytes m -> return (Just (mn, DocP.extractDocstring (A.mbody m)))
                          TyTask { name = mn, tyDoc = mdoc } -> return (Just (mn, mdoc))
                          ParseErrorTask{} -> return Nothing)
        DocP.generateDocIndex docDir entries

-- Remove orphaned files in out/types.
-- Non-root files are removed if their module isn’t part of this build (i.e.
-- no corresponding source .act in this build). The 'roots' argument lists the
-- exact set of root stub files (*.root.c, *.test_root.c) to keep; others are
-- removed.
removeOrphanFiles :: Paths -> [CompileTask] -> [FilePath] -> IO ()
removeOrphanFiles paths tasks roots = do
    let dir = projTypes paths
    absOutFiles <- getFilesRecursive dir
    let allowedBases = [ outBase paths (name t) | t <- tasks ]
    forM_ absOutFiles $ \absFile -> do
        let isC  = takeExtension absFile == ".c"
            isH  = takeExtension absFile == ".h"
            isTy = takeExtension absFile == ".ty"
            bext = takeExtension (takeBaseName absFile)
            isRootStub = isC && (bext == ".root" || bext == ".test_root")
            base = dropExtension absFile
            modBase = if isRootStub then dropExtension base else base
        if isRootStub
          then when (not (absFile `elem` roots) || not (modBase `elem` allowedBases)) (removeIfExists absFile)
          else when (isC || isH || isTy) $ do
                 unless (base `elem` allowedBases) (removeIfExists absFile)
  where
    removeIfExists f = removeFile f `catch` handleNotExists
    handleNotExists :: IOException -> IO ()
    handleNotExists _ = return ()

-- Determine which root stub files should be preserved for the current build.
-- We read the freshly written .ty headers (post-compile) for each task to keep
-- whichever roots are still declared. This lets us drop stale root stubs when
-- a module loses its root actor while still retaining stubs that belong to
-- other build modes (e.g. keeping .root.c when running `acton test`).
expectedRootStubs :: Paths -> [CompileTask] -> IO [FilePath]
expectedRootStubs paths tasks = do
    roots <- forM tasks $ \t -> do
        let mn     = name t
            outbase = outBase paths mn
            tyPath = outbase ++ ".ty"
        hdrE <- (try :: IO a -> IO (Either SomeException a)) $ InterfaceFiles.readHeader tyPath
        case hdrE of
          Right (_, _, _, rs, _) -> return (map (mkStub outbase) rs)
          _ -> return []
    return (concat roots)
  where
    mkStub outbase n =
      if nameToString n == "__test_main"
        then outbase ++ ".test_root.c"
        else outbase ++ ".root.c"

data BinTask = BinTask { isDefaultRoot :: Bool, binName :: String, rootActor :: A.QName, isTest :: Bool } deriving (Show)

binTaskRoot :: Paths -> BinTask -> FilePath
binTaskRoot paths binTask =
    let A.GName m _ = rootActor binTask
        outbase = outBase paths m
    in if isTest binTask then outbase ++ ".test_root.c" else outbase ++ ".root.c"

compileBins:: C.GlobalOptions -> C.CompileOptions -> Paths -> Acton.Env.Env0 -> [CompileTask] -> [BinTask] -> Bool -> IO ()
compileBins gopts opts paths env tasks binTasks allowPrune = do
    iff (not (altOutput opts)) $ do
      zigBuild env gopts opts paths tasks binTasks allowPrune
    return ()

printDiag :: C.GlobalOptions -> C.CompileOptions -> Diagnostic String -> IO ()
printDiag gopts opts d = do
    -- TODO: change to print to stderr! current tests presume stdout so we print to stdout for now..
    shouldColor <- useColor gopts
    if shouldColor
      then printDiagnostic stdout WithUnicode (TabSize 4) defaultStyle d
      else hPutDoc stdout $ unAnnotate (prettyDiagnostic WithoutUnicode (TabSize 4) d)

printDiagnostics :: C.GlobalOptions -> C.CompileOptions -> [Diagnostic String] -> IO ()
printDiagnostics gopts opts diags =
    mapM_ (printDiag gopts opts) diags

writeRootC :: Acton.Env.Env0 -> C.GlobalOptions -> C.CompileOptions -> Paths -> [CompileTask] -> BinTask -> IO (Maybe BinTask)
writeRootC env gopts opts paths tasks binTask = do
    -- In --only-build mode, don't generate or touch any files; re-use existing artifacts.
    if C.only_build opts
      then return (Just binTask)
      else do
        let qn@(A.GName m n) = rootActor binTask
            mn = A.mname qn
            outbase = outBase paths mn
            rootFile = if (isTest binTask) then outbase ++ ".test_root.c" else outbase ++ ".root.c"
        -- Read the up-to-date roots from the on-disk .ty header (post-compile)
        -- Avoid using preloaded TyTask roots, which may be stale if the module
        -- was rebuilt during this run.
        tyPath <- Acton.Env.findTyFile (searchPath paths) m
        rootsHeader <- case tyPath of
                         Just ty -> do (_, _, _, roots, _) <- InterfaceFiles.readHeader ty; return roots
                         Nothing -> return []
        let rootsEnv = case Acton.Env.lookupMod m env of
                         Nothing -> []
                         Just te -> [ n' | (n', i) <- te, rootEligible i ]
            shouldGen = n `elem` rootsHeader || n `elem` rootsEnv
        if shouldGen
          then do
            res <- (try :: IO a -> IO (Either SomeException a)) $ do
              c <- Acton.CodeGen.genRoot env qn
              createDirectoryIfMissing True (takeDirectory rootFile)
              writeFile rootFile c
            case res of
              Right _ -> return (Just binTask)
              Left _  -> return Nothing
          else return Nothing

isWindowsOS :: String -> Bool
isWindowsOS targetTriple = case splitOn "-" targetTriple of
    (_:os:_) -> os == "windows"
    _        -> False

readProcessWithExitCodeCancelable :: CreateProcess -> IO (ExitCode, String, String)
readProcessWithExitCodeCancelable cp = do
    let cp' = cp { std_in = NoStream, std_out = CreatePipe, std_err = CreatePipe }
    withCreateProcess cp' $ \_ mOut mErr ph -> do
      outVar <- newEmptyMVar
      errVar <- newEmptyMVar
      let readHandle mH var =
            case mH of
              Nothing -> putMVar var ""
              Just h -> do
                txt <- hGetContents h
                _ <- evaluate (length txt)
                hClose h
                putMVar var txt
      _ <- forkIO $ readHandle mOut outVar
      _ <- forkIO $ readHandle mErr errVar
      code <- waitForProcess ph `onException` do
                terminateProcess ph
                void (waitForProcess ph)
      out <- takeMVar outVar
      err <- takeMVar errVar
      return (code, out, err)

runZig gopts opts zigExe zigArgs paths wd = do
    let display = showCommandForUser zigExe zigArgs
    iff (C.ccmd opts || C.verbose gopts) $ putStrLn ("zigCmd: " ++ display)
    (returnCode, zigStdout, zigStderr) <- readProcessWithExitCodeCancelable (proc zigExe zigArgs){ cwd = wd }
    case returnCode of
        ExitSuccess -> do
          iff (C.verboseZig gopts) $ putStrLn zigStderr
          return ()
        ExitFailure ret -> do
          printIce ("compilation of generated Zig code failed, returned error code" ++ show ret)
          putStrLn $ "zig stdout:\n" ++ zigStdout
          putStrLn $ "zig stderr:\n" ++ zigStderr
          cleanup gopts opts paths
          unless (C.watch opts) System.Exit.exitFailure

-- Render build.zig and build.zig.zon from templates and BuildSpec
-- rootPins: dependency pins from the main project (applied to all deps, including transitive)
genBuildZigFiles :: M.Map String BuildSpec.PkgDep -> [(String, FilePath)] -> Paths -> IO ()
genBuildZigFiles rootPins depOverrides paths = do
    let proj = projPath paths
    projAbs <- canonicalizePath proj
    let sys              = sysPath paths
        buildZigPath     = joinPath [proj, "build.zig"]
        buildZonPath     = joinPath [proj, "build.zig.zon"]
        distBuildZigPath = joinPath [sys, "builder", "build.zig"]
        distBuildZonPath = joinPath [sys, "builder", "build.zig.zon"]
    buildZigTemplate <- readFile distBuildZigPath
    buildZonTemplate <- readFile distBuildZonPath
    spec0 <- loadBuildSpec proj
    spec  <- traverse (applyDepOverrides proj depOverrides) spec0
    (transPkgs, transZigs) <- collectDepsRecursive proj rootPins depOverrides
    absSys <- canonicalizePath sys
    let relSys = relativeViaRoot projAbs absSys
    homeDir <- getHomeDirectory
    depsRootAbs <- normalizePathSafe (joinPath [homeDir, ".cache", "acton", "deps"])
    normalizedSpec <- traverse (normalizeSpecPaths proj) spec
    let applyPins deps = M.mapWithKey (\n d -> M.findWithDefault d n rootPins) deps
        mergedSpec = fmap (\s -> s { BuildSpec.dependencies     = applyPins (BuildSpec.dependencies s) `M.union` transPkgs
                                   , BuildSpec.zig_dependencies = BuildSpec.zig_dependencies s `M.union` transZigs }) normalizedSpec
    case mergedSpec of
      Nothing -> do
        writeFile buildZigPath buildZigTemplate
        writeFile buildZonPath (replace "{{syspath}}" relSys buildZonTemplate)
      Just s -> do
        writeFile buildZigPath (genBuildZig buildZigTemplate s)
        writeFile buildZonPath (genBuildZigZon buildZonTemplate relSys depsRootAbs projAbs s)

genBuildZig :: String -> BuildSpec.BuildSpec -> String
genBuildZig template spec =
    let depsDefs = concatMap pkgDepDef (M.toList (BuildSpec.dependencies spec))
        zigDefs  = concatMap zigDepDef (M.toList (BuildSpec.zig_dependencies spec))
        depsAll  = depsDefs ++ zigDefs
        libLinks = concatMap pkgLibLink (M.toList (BuildSpec.dependencies spec))
                ++ concatMap zigLibLink (M.toList (BuildSpec.zig_dependencies spec))
        exeLinks = concatMap pkgExeLink (M.toList (BuildSpec.dependencies spec))
                ++ concatMap zigExeLink (M.toList (BuildSpec.zig_dependencies spec))
        header = [ "// AUTOMATICALLY GENERATED BY ACTON BUILD SYSTEM"
                 , "// DO NOT EDIT, CHANGES WILL BE OVERWRITTEN!!!!!"
                 , ""
                 ]
        inject line =
          let sline = dropWhile (== ' ') line
          in [line]
             ++ (if sline == "// Dependencies from build.act.json" then [depsAll] else [])
             ++ (if sline == "// lib: link with dependencies / get headers from build.act.json" then [libLinks] else [])
             ++ (if sline == "// exe: link with dependencies / get headers from build.act.json" then [exeLinks] else [])
    in unlines $ header ++ concatMap inject (lines template)
  where
    pkgDepDef (name, _) = unlines [ "    const actdep_" ++ name ++ " = b.dependency(\"" ++ name ++ "\", .{"
                                  , "        .target = target,"
                                  , "        .optimize = optimize,"
                                  , "    });"
                                  ]
    pkgLibLink (name, _) = "    libActonProject.linkLibrary(actdep_" ++ name ++ ".artifact(\"ActonProject\"));\n"
    pkgExeLink (name, _) = "            executable.linkLibrary(actdep_" ++ name ++ ".artifact(\"ActonProject\"));\n"

    zigDepDef (name, dep)
      | null (BuildSpec.artifacts dep) = ""
      | otherwise =
          let opts = concat [ "        ." ++ k ++ " = " ++ v ++ ",\n" | (k, v) <- M.toList (BuildSpec.options dep) ]
          in unlines [ "    const dep_" ++ name ++ " = b.dependency(\"" ++ name ++ "\", .{"
                     , "        .target = target,"
                     , "        .optimize = optimize,"
                     , opts ++ "    });"
                     ]
    zigLibLink (name, dep) = concat [ "    libActonProject.linkLibrary(dep_" ++ name ++ ".artifact(\"" ++ art ++ "\"));\n"
                                    | art <- BuildSpec.artifacts dep ]
    zigExeLink (name, dep) = concat [ "            executable.linkLibrary(dep_" ++ name ++ ".artifact(\"" ++ art ++ "\"));\n"
                                    | art <- BuildSpec.artifacts dep ]

genBuildZigZon :: String -> String -> FilePath -> FilePath -> BuildSpec.BuildSpec -> String
genBuildZigZon template relSys depsRootAbs projAbs spec =
    let pkgDeps = concatMap (pkgToZon projAbs depsRootAbs) (M.toList (BuildSpec.dependencies spec))
        zigDeps = concatMap zigToZon (M.toList (BuildSpec.zig_dependencies spec))
        deps = pkgDeps ++ zigDeps
        replaced = map (replace "{{syspath}}" relSys) (lines template)
        header = [ "// AUTOMATICALLY GENERATED BY ACTON BUILD SYSTEM"
                 , "// DO NOT EDIT, CHANGES WILL BE OVERWRITTEN!!!!!"
                 , ""
                 ]
        inject line =
          let sline = dropWhile (== ' ') line
          in [line] ++ (if sline == "// Dependencies from build.act.json" then [deps] else [])
    in unlines $ header ++ concatMap inject replaced
  where
    pkgToZon projRoot depsRoot (name, dep) =
      let rawPath = case BuildSpec.path dep of
                      Just p | not (null p) -> p
                      _ -> case BuildSpec.hash dep of
                             Just h -> joinPath [depsRoot, name ++ "-" ++ h]
                             Nothing -> errorWithoutStackTrace ("Dependency " ++ name ++ " has no path or hash")
          pathAbs = collapseDots $
                      if isAbsolutePath rawPath
                        then normalise rawPath
                        else normalise (rebasePath projRoot rawPath)
          path = relativeViaRoot projRoot pathAbs
      in unlines [ "        ." ++ name ++ " = .{"
                 , "            .path = \"" ++ path ++ "\","
                 , "        },"
                 ]
    zigToZon (name, dep) =
      case BuildSpec.zpath dep of
        Just p ->
          let absPath = collapseDots $
                          if isAbsolutePath p
                            then normalise p
                            else normalise (rebasePath projAbs p)
              relPath = relativeViaRoot projAbs absPath
          in unlines [ "        ." ++ name ++ " = .{"
                     , "            .path = \"" ++ relPath ++ "\","
                     , "        },"
                     ]
        Nothing -> unlines [ "        ." ++ name ++ " = .{"
                           , "            .url = \"" ++ maybeEmpty (BuildSpec.zurl dep) ++ "\","
                           , "            .hash = \"" ++ maybeEmpty (BuildSpec.zhash dep) ++ "\","
                           , "        },"
                           ]
    maybeEmpty (Just s) = s
    maybeEmpty Nothing  = ""

-- TODO: replace all of this with generic+crypto?!
#if defined(darwin_HOST_OS) && defined(aarch64_HOST_ARCH)
defCpuFlag = ["-Dcpu=apple_a15"]
#elif defined(darwin_HOST_OS) && defined(x86_64_HOST_ARCH)
defCpuFlag = []
#elif defined(linux_HOST_OS) && defined(aarch64_HOST_ARCH)
defCpuFlag = ["-Dcpu=cortex_a72"]
#elif defined(linux_HOST_OS) && defined(x86_64_HOST_ARCH)
defCpuFlag = []
#else
#error "Unsupported platform"
#endif

zigBuild :: Acton.Env.Env0 -> C.GlobalOptions -> C.CompileOptions -> Paths -> [CompileTask] -> [BinTask] -> Bool -> IO ()
zigBuild env gopts opts paths tasks binTasks allowPrune = do
    allBinTasks <- mapM (writeRootC env gopts opts paths tasks) binTasks
    let realBinTasks = catMaybes allBinTasks

    let pruningAllowed = allowPrune && not (C.only_build opts)
    when pruningAllowed $ do
      -- Clean out/types: drop stray outputs and stale root stubs based on the
      -- current headers. Also keep the roots we attempted to build in this run
      -- so they survive even if a header failed to list them (defensive).
      let requestedRoots = map (binTaskRoot paths) realBinTasks
      headerRoots <- expectedRootStubs paths tasks
      let roots = nub (headerRoots ++ requestedRoots)
      removeOrphanFiles paths tasks roots
      unless (isTmp paths) $
        -- Clean old binaries from out/bin
        removeOrphanExecutables (binDir paths) (projTypes paths) realBinTasks

    iff (not (quiet gopts opts)) $ putStrLn("  Final compilation step")
    timeStart <- getTime Monotonic

    homeDir <- getHomeDirectory
    let local_cache_dir = joinPath [ homeDir, ".cache", "acton", "zig-local-cache" ]
        global_cache_dir = joinPath [ homeDir, ".cache", "acton", "zig-global-cache" ]
        no_threads = if isWindowsOS (C.target opts) then True else C.no_threads opts
    projAbs <- normalizePathSafe (projPath paths)
    sysAbs  <- normalizePathSafe (sysPath paths)
    let sysRoot   = addTrailingPathSeparator sysAbs
        isSysProj = projAbs == sysAbs || sysRoot `isPrefixOf` projAbs

    -- If actonc runs as a standalone compiler (not a sub-compiler from Acton CLI),
    -- generate build.zig and build.zig.zon directly from Build.act/build.act.json
    iff (not (C.sub gopts) && not isSysProj) $ do
      pinsSpec0 <- loadBuildSpec (projPath paths)
      pinsSpec  <- traverse (applyDepOverrides (projPath paths) (C.dep_overrides opts)) pinsSpec0
      let pins = maybe M.empty BuildSpec.dependencies pinsSpec
      genBuildZigFiles pins (C.dep_overrides opts) paths

    let zigExe = zig paths
        baseArgs = ["build","--cache-dir", local_cache_dir,
                            "--global-cache-dir", global_cache_dir] ++
                   (if (C.verboseZig gopts) then ["--verbose"] else [])
        prefixArgs = ["--prefix", projOut paths, "--prefix-exe-dir", "bin"] ++
                     (if (C.verboseZig gopts) then ["--verbose"] else [])
        targetArgs = ["-Dtarget=" ++ C.target opts]
        cpuArgs =
            if (C.cpu opts /= "") then ["-Dcpu=" ++ C.cpu opts]
            else case (splitOn "-" (C.target opts)) of
                   ("native":_)            -> defCpuFlag
                   ("aarch64":"macos":_)   -> ["-Dcpu=apple_a15"]
                   ("aarch64":"windows":_) -> ["-Dcpu=apple_a15"]
                   ("aarch64":"linux":_)   -> ["-Dcpu=cortex_a72"]
                   ("x86_64":_:_)          -> ["-Dcpu=westmere"]
                   (_:_:_)                 -> defCpuFlag
        optArgs = ["-Doptimize=" ++ optimizeModeToZig (C.optimize opts)]
        featureArgs = concat [ if C.db opts then ["-Ddb"] else []
                             , if no_threads then ["-Dno_threads"] else []
                             , if C.cpedantic opts then ["-Dcpedantic"] else []
                             ]
        zigArgs = baseArgs ++ prefixArgs ++ targetArgs ++ cpuArgs ++ optArgs ++ featureArgs

    runZig gopts opts zigExe zigArgs paths (Just (projPath paths))
    -- if we are in a temp acton project, copy the outputted binary next to the source file
    if (isTmp paths && not (null realBinTasks))
      then do
        let baseName   = binName (head binTasks)
            exeName    = if isWindowsOS (C.target opts) then baseName ++ ".exe" else baseName
            srcBinFile = joinPath [ projOut paths, "bin", exeName ]
            dstBinFile = joinPath [ binDir paths, exeName ]
        copyFile srcBinFile dstBinFile
      else return ()
    cleanup gopts opts paths
    timeEnd <- getTime Monotonic
    iff (not (quiet gopts opts)) $ putStrLn("   Finished final compilation step in  " ++ fmtTime(timeEnd - timeStart))
    return ()

-- Remove executables that no longer have corresponding root actors
removeOrphanExecutables :: FilePath -> FilePath -> [BinTask] -> IO ()
removeOrphanExecutables binDir projTypes binTasks = do
    binDirExists <- doesDirectoryExist binDir
    when binDirExists $ do
        binFiles <- listDirectory binDir
        forM_ binFiles $ \exeFile -> do
            let exeName = takeBaseName exeFile
                modPath = map (\c -> if c == '.' then '/' else c) exeName
                rootCFile = projTypes </> modPath <.> "root.c"
                testRootCFile = projTypes </> modPath <.> "test_root.c"

            let isCurrentBin = any (\t -> binName t == exeName) binTasks

            rootExists <- doesFileExist rootCFile
            testRootExists <- doesFileExist testRootCFile
            when (not isCurrentBin && not rootExists && not testRootExists) $ do
              let fileName = binDir </> exeFile
              removeFile fileName `catch` handleNotExists
  where
    handleNotExists :: IOException -> IO ()
    handleNotExists _ = return ()

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

copyTree :: FilePath -> FilePath -> IO ()
copyTree src dst = do
    exists <- doesDirectoryExist src
    unless exists $ printErrorAndExit ("Source path for copyTree does not exist: " ++ src)
    createDirectoryIfMissing True dst
    entries <- listDirectory src
    forM_ entries $ \e -> do
      let s = src </> e
          d = dst </> e
      isDir <- doesDirectoryExist s
      if isDir
        then copyTree s d
        else do
          createDirectoryIfMissing True (takeDirectory d)
          copyFile s d

makeRelativeOrAbsolute :: FilePath -> FilePath -> FilePath
makeRelativeOrAbsolute base target =
    let (bDriveRaw, bPath) = splitDrive (normalise base)
        (tDriveRaw, tPath) = splitDrive (normalise target)
        bDrive = map toLower bDriveRaw
        tDrive = map toLower tDriveRaw
        bParts = cleanParts bPath
        tParts = cleanParts tPath
        common = length (takeWhile (uncurry (==)) (zip bParts tParts))
        ups = replicate (length bParts - common) ".."
        relParts = ups ++ drop common tParts
        rel = if null relParts then "." else joinPath relParts
    in if bDrive /= tDrive && (not (null bDrive) || not (null tDrive))
          then joinPath (replicate (length bParts) ".." ++ (tDriveRaw : tParts))
          else rel
  where
    cleanParts = filter (\c -> not (null c) && c /= "/") . splitDirectories

relativeViaRoot :: FilePath -> FilePath -> FilePath
relativeViaRoot baseAbs targetAbs
  | not (isAbsolutePath targetAbs) = targetAbs
  | otherwise =
      let (bDriveRaw, bPath) = splitDrive (normalise baseAbs)
          (tDriveRaw, tPath) = splitDrive (normalise targetAbs)
          bDrive = map toLower bDriveRaw
          tDrive = map toLower tDriveRaw
      in if bDrive /= tDrive && (not (null bDrive) || not (null tDrive))
           then makeRelativeOrAbsolute baseAbs targetAbs
           else
             let ups = replicate (length (cleanParts bPath)) ".."
                 tParts = cleanParts tPath
             in joinPath (ups ++ tParts)
  where
    cleanParts = filter (\c -> not (null c) && c /= "/") . splitDirectories

collectDepsRecursive :: FilePath -> M.Map String BuildSpec.PkgDep -> [(String, FilePath)] -> IO (M.Map String BuildSpec.PkgDep, M.Map String BuildSpec.ZigDep)
collectDepsRecursive projDir pins overrides = do
  root <- normalizePathSafe projDir
  (\(_, pkgs, zigs) -> (pkgs, zigs)) <$> go root Data.Set.empty root Nothing
  where
    go root seen dir mSpec = do
      mspec <- case mSpec of
                 Just s -> return (Just s)
                 Nothing -> loadBuildSpec dir
      mspec' <- traverse (applyDepOverrides dir overrides) mspec
      case mspec' of
        Nothing   -> return (seen, M.empty, M.empty)
        Just spec -> do
          let depsHere = BuildSpec.dependencies spec
              zigsHere = M.map (rebaseZig root dir) (BuildSpec.zig_dependencies spec)
          foldM (step root dir) (seen, M.empty, zigsHere) (M.toList depsHere)

    step root base (seen, pkgAcc, zigAcc) (depName, dep) = do
      let depChosen = case M.lookup depName pins of
                        Nothing   -> dep
                        Just pdep -> pdep
      depBase <- resolveDepBase base depName depChosen
      let seen' = Data.Set.insert depBase seen
          rebasePkgPath d =
            case BuildSpec.path d of
              Just p | not (null p) ->
                let absP = rebasePath base p
                    relP = makeRelativeOrAbsolute root absP
                in d { BuildSpec.path = Just (collapseDots relP) }
              _ -> d
          dep' = rebasePkgPath depChosen
      if Data.Set.member depBase seen
        then return (seen', pkgAcc, zigAcc)
        else do
          (seenNext, subPkgs, subZigs) <- go root seen' depBase Nothing
          let pkgAcc' = M.insertWith (\_ old -> old) depName dep' pkgAcc
          return (seenNext, pkgAcc' `M.union` subPkgs, zigAcc `M.union` subZigs)

    rebaseZig root base dep =
      case BuildSpec.zpath dep of
        Just p | not (null p) ->
          let absP = rebasePath base p
              relP = makeRelativeOrAbsolute root absP
          in dep { BuildSpec.zpath = Just (collapseDots relP) }
        _ -> dep

fetchDependencies :: C.GlobalOptions -> Paths -> [(String, FilePath)] -> IO ()
fetchDependencies gopts paths depOverrides = do
    when (isTmp paths) $ return ()
    mspec <- loadBuildSpec (projPath paths)
    case mspec of
      Nothing -> return ()
      Just spec0 -> do
        spec <- applyDepOverrides (projPath paths) depOverrides spec0
        unless (C.quiet gopts) $
          putStrLn "Resolving dependencies (fetching if missing)..."
        home <- getHomeDirectory
        let zigExe      = joinPath [sysPath paths, "zig", "zig"]
            globalCache = joinPath [home, ".cache", "acton", "zig-global-cache"]
            depsCache   = joinPath [home, ".cache", "acton", "deps"]
            cacheDir h  = joinPath [globalCache, "p", h]
        createDirectoryIfMissing True globalCache
        createDirectoryIfMissing True depsCache

        let pkgFetches = catMaybes $
                [ mkPkgFetch name dep | (name, dep) <- M.toList (BuildSpec.dependencies spec) ]
            zigFetches = catMaybes $
                [ mkZigFetch name dep | (name, dep) <- M.toList (BuildSpec.zig_dependencies spec) ]

            mkPkgFetch name dep =
              case BuildSpec.path dep of
                Just p | not (null p) -> Nothing
                _ -> case (BuildSpec.url dep, BuildSpec.hash dep) of
                       (Just u, Just h) ->
                         Just (fetchOne "pkg" name u (Just h) cacheDir zigExe globalCache)
                       (Just _, Nothing) ->
                         Just (return (Left ("Dependency " ++ name ++ " is missing hash")))
                       _ -> Nothing

            mkZigFetch name dep =
              case BuildSpec.zpath dep of
                Just p | not (null p) -> Nothing
                _ -> case (BuildSpec.zurl dep, BuildSpec.zhash dep) of
                       (Just u, Just h) ->
                         Just (fetchOne "zig" name u (Just h) cacheDir zigExe globalCache)
                       (Just _, Nothing) ->
                         Just (return (Left ("Zig dependency " ++ name ++ " is missing hash")))
                       _ -> Nothing

        results <- mapConcurrently id (pkgFetches ++ zigFetches)
        let errs = [ e | Left e <- results ]
        unless (null errs) $ printErrorAndExit (unlines errs)

        forM_ (M.toList (BuildSpec.dependencies spec)) $ \(name, dep) -> do
          case BuildSpec.path dep of
            Just p | not (null p) -> return ()
            _ -> case BuildSpec.hash dep of
                   Nothing -> return ()
                   Just h -> do
                     let src = cacheDir h
                         dst = joinPath [depsCache, name ++ "-" ++ h]
                     exists <- doesDirectoryExist dst
                     unless exists $ do
                       srcOk <- doesDirectoryExist src
                       unless srcOk $
                         printErrorAndExit ("Dependency " ++ name ++ " not present in Zig cache after fetch: " ++ src)
                       when (C.verbose gopts) $
                         putStrLn ("Copying dependency " ++ name ++ " (" ++ h ++ ") from Zig cache")
                       copyTree src dst
          return ()
  where
    fetchOne kind name url mh cacheDir zigExe globalCache = do
      case mh of
        Just h -> do
          present <- doesDirectoryExist (cacheDir h)
          if present
            then do
              putStrLn ("Using cached " ++ kind ++ " dependency " ++ name ++ " (" ++ h ++ ")")
              return (Right h)
            else runFetch kind name url mh cacheDir zigExe globalCache
        Nothing ->
          runFetch kind name url mh cacheDir zigExe globalCache

    runFetch kind name url mh cacheDir zigExe globalCache = do
      putStrLn ("Fetching " ++ kind ++ " dependency " ++ name ++ " from " ++ url)
      let cmd = proc zigExe ["fetch", "--global-cache-dir", globalCache, url]
      res <- try (readCreateProcessWithExitCode cmd "") :: IO (Either SomeException (ExitCode, String, String))
      case res of
        Left ex -> return (Left ("Failed to fetch dependency " ++ name ++ ": " ++ displayException ex))
        Right (code, out, err) ->
          case code of
            ExitSuccess -> do
              let fetched = trim out
              case mh of
                 Just h | h /= fetched ->
                   return (Left ("Hash mismatch for " ++ name ++ ": expected " ++ h ++ " got " ++ fetched))
                 _ -> do
                   let dir = cacheDir fetched
                   dirExists <- doesDirectoryExist dir
                   if dirExists
                     then do
                       putStrLn ("Fetched " ++ kind ++ " dependency " ++ name ++ " (" ++ fetched ++ ")")
                       return (Right fetched)
                     else return (Left ("zig fetch reported hash " ++ fetched ++ " but cache dir missing: " ++ dir))
            ExitFailure rc ->
              return (Left ("Failed to fetch dependency " ++ name ++ " (exit " ++ show rc ++ ")\n" ++ err ++ out))

normalizeSpecPaths :: FilePath -> BuildSpec.BuildSpec -> IO BuildSpec.BuildSpec
normalizeSpecPaths base spec = do
    deps <- normalizePkgDeps base (BuildSpec.dependencies spec)
    zigs <- normalizeZigDeps base (BuildSpec.zig_dependencies spec)
    return spec { BuildSpec.dependencies = deps
                , BuildSpec.zig_dependencies = zigs }
  where
    normalizePkgDeps b = fmap M.fromList . mapM (\(k,v) -> do v' <- normalizePkgDep b v; return (k,v')) . M.toList
    normalizePkgDep b dep =
      case BuildSpec.path dep of
        Just p | not (null p) -> do
          if isAbsolutePath p
            then do p' <- normalizePathSafe p
                    return dep { BuildSpec.path = Just p' }
            else return dep { BuildSpec.path = Just (collapseDots (normalise p)) }
        _ -> return dep

    normalizeZigDeps b = fmap M.fromList . mapM (\(k,v) -> do v' <- normalizeZigDep b v; return (k,v')) . M.toList

normalizeZigDep :: FilePath -> BuildSpec.ZigDep -> IO BuildSpec.ZigDep
normalizeZigDep b dep =
  case BuildSpec.zpath dep of
    Just p | not (null p) -> do
      if isAbsolutePath p
        then do p' <- normalizePathSafe p
                return dep { BuildSpec.zpath = Just p' }
        else return dep { BuildSpec.zpath = Just (collapseDots (normalise p)) }
    _ -> return dep

filterMainActor :: Acton.Env.Env0 -> Paths -> BinTask -> IO (Maybe BinTask)
filterMainActor env paths binTask = do
    let qn@(A.GName m n) = rootActor binTask
    let checkEnv = case Acton.Env.lookupMod m env of
                     Nothing -> return Nothing
                     Just te -> do
                       let rootsEnv = [ n' | (n', i) <- te, rootEligible i ]
                       if n `elem` rootsEnv then return (Just binTask) else return Nothing
    mty <- Acton.Env.findTyFile (searchPath paths) m
    case mty of
      Just ty -> do
        hdrE <- (try :: IO a -> IO (Either SomeException a)) $ InterfaceFiles.readHeader ty
        case hdrE of
          Right (_, _, _, roots, _) | n `elem` roots -> return (Just binTask)
          _ -> checkEnv
      Nothing -> checkEnv

isGitAvailable :: IO Bool
isGitAvailable = do
    (exitCode, _, _) <- readProcessWithExitCode "git" ["--version"] ""
    return $ exitCode == ExitSuccess

useColor :: C.GlobalOptions -> IO Bool
useColor gopts = do
    noColorEnv <- lookupEnv "NO_COLOR"
    case noColorEnv of
        Just _ -> return False
        Nothing ->
            case C.color gopts of
                C.Never  -> return False
                C.Always -> return True
                C.Auto   -> do
                    tty <- hIsTerminalDevice stdout
                    return (tty || C.tty gopts)

data ProgressUI = ProgressUI
  { puEnabled :: Bool
  , puMaxLines :: Int
  , puLinesRef :: IORef [String]
  , puVisibleRef :: IORef Int
  , puSpinnerRef :: IORef Int
  , puCursorHiddenRef :: IORef Bool
  , puTickerRef :: IORef (Maybe ThreadId)
  , puLock :: MVar ()
  }

data ProgressState = ProgressState
  { psActive :: IORef (M.Map TaskKey String)
  , psOrder :: IORef [TaskKey]
  }

initProgressUI :: C.GlobalOptions -> Int -> IO ProgressUI
initProgressUI gopts maxLines = do
    tty <- hIsTerminalDevice stdout
    let enabled = (tty || C.tty gopts) && not (C.quiet gopts)
    linesRef <- newIORef []
    visibleRef <- newIORef 0
    spinnerRef <- newIORef 0
    cursorHiddenRef <- newIORef False
    tickerRef <- newIORef Nothing
    lock <- newMVar ()
    return ProgressUI
      { puEnabled = enabled
      , puMaxLines = max 1 maxLines
      , puLinesRef = linesRef
      , puVisibleRef = visibleRef
      , puSpinnerRef = spinnerRef
      , puCursorHiddenRef = cursorHiddenRef
      , puTickerRef = tickerRef
      , puLock = lock
      }

newProgressState :: IO ProgressState
newProgressState = do
    active <- newIORef M.empty
    order <- newIORef []
    return ProgressState { psActive = active, psOrder = order }

withProgressLock :: ProgressUI -> IO a -> IO a
withProgressLock ui action =
    if not (puEnabled ui)
      then action
      else withMVar (puLock ui) (\_ -> action)

progressWithLog :: ProgressUI -> IO () -> IO ()
progressWithLog ui action = withProgressLock ui $ do
    if not (puEnabled ui)
      then action
      else do
        progressClearUnlocked ui
        action
        progressRenderUnlocked ui

progressLogLine :: ProgressUI -> String -> IO ()
progressLogLine ui msg = progressWithLog ui (putStrLn msg)

progressClearUnlocked :: ProgressUI -> IO ()
progressClearUnlocked ui = do
    visible <- readIORef (puVisibleRef ui)
    when (visible > 0) $ do
      replicateM_ visible $ do
        putStr "\ESC[1A"
        putStr "\r\ESC[2K"
      hFlush stdout
    writeIORef (puVisibleRef ui) 0

spinnerChars :: [Char]
spinnerChars = "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏"

spinnerTickMicros :: Int
spinnerTickMicros = 100000

setCursorHidden :: ProgressUI -> Bool -> IO ()
setCursorHidden ui hidden =
    when (puEnabled ui) $ do
      cur <- readIORef (puCursorHiddenRef ui)
      when (cur /= hidden) $ do
        putStr (if hidden then "\ESC[?25l" else "\ESC[?25h")
        writeIORef (puCursorHiddenRef ui) hidden

progressRenderUnlocked :: ProgressUI -> IO ()
progressRenderUnlocked ui = do
    lines <- take (puMaxLines ui) <$> readIORef (puLinesRef ui)
    prevVisible <- readIORef (puVisibleRef ui)
    let newVisible = length lines
        total = max prevVisible newVisible
    if newVisible == 0
      then do
        setCursorHidden ui False
        progressClearUnlocked ui
      else do
        when (prevVisible > 0) $
          replicateM_ prevVisible (putStr "\ESC[1A")
        ix <- readIORef (puSpinnerRef ui)
        let spinner = spinnerChars !! (ix `mod` length spinnerChars)
            prefix line = ' ' : spinner : ' ' : line
            renderLine i = do
              putStr "\r\ESC[2K"
              when (i < newVisible) $
                putStr (prefix (lines !! i))
              putStr "\n"
        setCursorHidden ui True
        mapM_ renderLine [0 .. total - 1]
        hFlush stdout
        writeIORef (puVisibleRef ui) total

startSpinnerTicker :: ProgressUI -> IO ()
startSpinnerTicker ui =
    when (puEnabled ui) $ do
      m <- readIORef (puTickerRef ui)
      case m of
        Just _ -> return ()
        Nothing -> do
          tid <- forkIO (spinnerLoop ui)
          writeIORef (puTickerRef ui) (Just tid)

stopSpinnerTicker :: ProgressUI -> IO ()
stopSpinnerTicker ui = do
    m <- atomicModifyIORef' (puTickerRef ui) (\cur -> (Nothing, cur))
    forM_ m killThread

spinnerLoop :: ProgressUI -> IO ()
spinnerLoop ui = do
    tid <- myThreadId
    let loop = do
          threadDelay spinnerTickMicros
          keep <- withProgressLock ui $ do
            lines <- readIORef (puLinesRef ui)
            if null lines || not (puEnabled ui)
              then return False
              else do
                modifyIORef' (puSpinnerRef ui) (+ 1)
                progressRenderUnlocked ui
                return True
          when keep loop
    loop
    atomicModifyIORef' (puTickerRef ui) $ \cur ->
      if cur == Just tid then (Nothing, ()) else (cur, ())

progressSetLines :: ProgressUI -> [String] -> IO ()
progressSetLines ui lines = withProgressLock ui $ do
    when (puEnabled ui) $ do
      writeIORef (puLinesRef ui) (take (puMaxLines ui) lines)
      if null lines then stopSpinnerTicker ui else startSpinnerTicker ui
      progressRenderUnlocked ui

progressStartTask :: ProgressUI -> ProgressState -> TaskKey -> String -> IO ()
progressStartTask ui st key line = withProgressLock ui $ do
    modifyIORef' (psActive st) (M.insert key line)
    modifyIORef' (psOrder st) (\xs -> if key `elem` xs then xs else xs ++ [key])
    progressRefreshUnlocked ui st

progressDoneTask :: ProgressUI -> ProgressState -> TaskKey -> IO ()
progressDoneTask ui st key = withProgressLock ui $ do
    modifyIORef' (psActive st) (M.delete key)
    modifyIORef' (psOrder st) (filter (/= key))
    progressRefreshUnlocked ui st

progressRefreshUnlocked :: ProgressUI -> ProgressState -> IO ()
progressRefreshUnlocked ui st = do
    active <- readIORef (psActive st)
    order <- readIORef (psOrder st)
    let lines = [ line | key <- order, Just line <- [M.lookup key active] ]
    when (puEnabled ui) $ do
      writeIORef (puLinesRef ui) (take (puMaxLines ui) lines)
      if null lines then stopSpinnerTicker ui else startSpinnerTicker ui
      progressRenderUnlocked ui

projectLabel :: FilePath -> FilePath -> String
projectLabel root proj =
    let rel = makeRelative root proj
    in if rel == "." || ".." `isPrefixOf` rel
         then takeFileName proj
         else rel
