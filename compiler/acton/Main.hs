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
import qualified Acton.NameInfo as I
import Text.Megaparsec.Error (ParseErrorBundle)
import Acton.Parser (CustomParseError)
import qualified Acton.CommandLineParser as C
import Acton.Printer ()
import qualified Acton.Env
import Acton.Env (simp, define, setMod)
import qualified Acton.QuickType
import qualified Acton.Kinds
import qualified Acton.Types
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
import qualified Acton.Fingerprint as Fingerprint
import qualified Acton.SourceProvider as Source
import Acton.Compile
import Utils
import qualified Pretty
import qualified InterfaceFiles
import qualified PkgCommands

import Control.Concurrent.MVar
import Control.Exception (throw,catch,finally,IOException,try,SomeException,bracket,bracket_,onException,evaluate)
import Control.Exception (bracketOnError)
import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import Control.Monad
import Data.Bits
import Data.Default.Class (def)
import Data.List.Split
import Data.IORef
import Data.Maybe (catMaybes, isJust)
import Data.Monoid ((<>))
import Data.Word (Word32)
import Data.Graph
import Data.String.Utils (replace)
import Data.Version (showVersion)
import Data.Char (toLower)
import qualified Data.List
import Data.Either (partitionEithers)
import qualified Data.Map as M
import qualified Data.Set
import Data.Time.Clock (UTCTime)
import Error.Diagnose
import Error.Diagnose.Style (defaultStyle)
import qualified Filesystem.Path.CurrentOS as Fsco
import GHC.Conc (getNumCapabilities, getNumProcessors, setNumCapabilities, myThreadId, threadCapability)
import Prettyprinter (unAnnotate)
import Prettyprinter.Render.Text (hPutDoc)
import Data.List (isPrefixOf, isSuffixOf, find, partition, foldl', nub, intercalate)
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
import System.Posix.IO (createPipe, setFdOption, closeFd, FdOption(..))
import System.Process hiding (createPipe)
import qualified System.FSNotify as FS
import qualified System.Environment
import qualified System.Exit
import qualified Paths_acton
import System.Random (randomRIO)
import Text.Printf

import qualified Data.ByteString.Lazy as BL

import TestRunner
import TerminalProgress
import ZigProgress

main = do
    hSetBuffering stdout LineBuffering
    arg <- C.parseCmdLine
    let gopts = case arg of
          C.CmdOpt g _ -> g
          C.CompileOpt _ g _ -> g
    ensureCapabilities gopts
    let run = case arg of
          C.CmdOpt gopts (C.New opts)         -> createProject (C.file opts)
          C.CmdOpt gopts (C.Build bopts)      ->
              let opts = C.buildCompile bopts
                  files = C.buildFiles bopts
              in if null files
                 then buildProject gopts opts
                 else buildFiles gopts opts files
          C.CmdOpt gopts (C.Test tcmd)        -> runTests gopts tcmd
          C.CmdOpt gopts C.Fetch             -> fetchCommand gopts
          C.CmdOpt gopts C.PkgShow           -> pkgShow gopts
          C.CmdOpt gopts (C.PkgAdd opts)     -> PkgCommands.pkgAddCommand gopts opts
          C.CmdOpt gopts (C.PkgRemove opts)  -> PkgCommands.pkgRemoveCommand gopts opts
          C.CmdOpt gopts (C.PkgUpgrade opts) -> PkgCommands.pkgUpgradeCommand gopts opts
          C.CmdOpt gopts C.PkgUpdate         -> PkgCommands.pkgUpdateCommand gopts
          C.CmdOpt gopts (C.PkgSearch opts)  -> PkgCommands.pkgSearchCommand gopts opts
          C.CmdOpt gopts (C.BuildSpecCmd o)   -> buildSpecCommand o
          C.CmdOpt gopts (C.Cloud opts)       -> undefined
          C.CmdOpt gopts (C.Doc opts)         -> printDocs gopts opts
          C.CmdOpt gopts (C.ZigPkgAdd opts)  -> PkgCommands.zigPkgAddCommand gopts opts
          C.CmdOpt gopts (C.ZigPkgRemove opts) -> PkgCommands.zigPkgRemoveCommand gopts opts
          C.CmdOpt gopts C.Version            -> printVersion
          C.CompileOpt nms gopts opts         -> runFile gopts opts (head nms)
    run `catch` \(ProjectError msg) -> printErrorAndExit msg

-- Apply global options to compile options
applyGlobalOpts :: C.GlobalOptions -> C.CompileOptions -> C.CompileOptions
applyGlobalOpts gopts opts = opts

runFile :: C.GlobalOptions -> C.CompileOptions -> FilePath -> IO ()
runFile gopts opts fname =
    case takeExtension fname of
      ".act" -> buildFile gopts (applyGlobalOpts gopts opts) fname
      ".ty" -> printDocs gopts (C.DocOptions fname (Just C.AsciiFormat) Nothing)
      _ -> printErrorAndExit ("Unknown filetype: " ++ fname)

-- Ensure enough capabilities: honor --jobs if set, otherwise at least 2 or #procs.
ensureCapabilities :: C.GlobalOptions -> IO ()
ensureCapabilities gopts = do
    caps0 <- getNumCapabilities
    let req = C.jobs gopts
    when (req > 0 && caps0 < req) $ setNumCapabilities req
    when (req == 0 && caps0 < 2) $ do
      procs <- getNumProcessors
      setNumCapabilities (max 2 procs)


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

-- our own readFile & writeFile with hard-coded utf-8 encoding (atomic writes)
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

-- | Format a TimeSpec as seconds with millisecond precision.
fmtTime t =
    printf "%6.3f s" secs
  where
    secs :: Float
    secs = (fromIntegral(sec t)) + (fromIntegral (nsec t) / 1000000000)

-- | Pad a string with trailing spaces for aligned output.
padRight :: Int -> String -> String
padRight width s
    | len >= width' = s
    | otherwise = s ++ replicate (width' - len) ' '
  where
    width' = max 0 width
    len = length s

-- Version handling ------------------------------------------------------------------------------------------

printVersion = putStrLn getVer

getVer          = showVersion Paths_acton.version

printIce errMsg = putStrLn(
                        "ERROR: internal compiler error: " ++ errMsg ++
                        "\nNOTE: this is likely a bug in acton, please report this at:" ++
                        "\nNOTE: https://github.com/actonlang/acton/issues/new?template=ice.yaml" ++
                        "\nNOTE: acton " ++ getVer
                        )

-- Create a project ---------------------------------------------------------------------------------------------

-- | Create a new Acton project skeleton (files, dirs, optional git init).
createProject :: String -> IO ()
createProject name = do
    curDir <- getCurrentDirectory
    projDirExists <- doesDirectoryExist name
    iff (projDirExists) $
        printErrorAndExit ("Unable to create project " ++ name ++ ", directory already exists.")
    fp <- generateFingerprint name
    let projDir = joinPath [curDir, name]
        srcRoot = joinPath [projDir, "src"]
        buildActPath = joinPath [projDir, "Build.act"]
        buildSpec = BuildSpec.BuildSpec
          { BuildSpec.specName = name
          , BuildSpec.specDescription = Nothing
          , BuildSpec.fingerprint = fp
          , BuildSpec.dependencies = M.empty
          , BuildSpec.zig_dependencies = M.empty
          }
    createDirectoryIfMissing True srcRoot
    writeFile buildActPath (BuildSpec.renderBuildAct buildSpec)
    paths <- findPaths buildActPath defaultCompileOptions
    writeFile (joinPath [ projDir, ".gitignore" ]) (
      ".actonc.lock\n" ++
      "build.zig\n" ++
      "build.zig.zon\n" ++
      "out\n"
      )
    writeFile (joinPath [ projDir, "README.md" ]) (
      "# " ++ name ++ "\n\n"
      ++ "```sh\nacton build\n./out/bin/" ++ name ++ "\n```\n"
      )
    createDirectoryIfMissing True (srcDir paths)
    writeFile (joinPath [(srcDir paths), name ++ ".act"]) "#\n#\n\nactor main(env):\n    print(\"Hello World!\")\n    env.exit(0)\n"
    putStrLn("Created project " ++ name)
    putStrLn("Next: cd " ++ name ++ " && acton build && ./out/bin/" ++ name)
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
      spec <- loadBuildSpec "."
      BL.putStr (BuildSpec.encodeBuildSpecJSON spec)


-- Build a project -----------------------------------------------------------------------------------------------

-- | Dispatch a project build, routing to watch or single-shot compilation.
buildProject :: C.GlobalOptions -> C.CompileOptions -> IO ()
buildProject gopts opts = do
                iff (not (null $ (C.root opts)) && (length $ splitOn "." (C.root opts)) == 1) $
                  printErrorAndExit("Project build requires a qualified root actor name, like foo.main")
                if C.watch opts
                  then do
                    curDir <- getCurrentDirectory
                    watchProjectAt gopts opts curDir
                  else buildProjectOnce gopts opts

-- | Handle "acton build FILE..." by compiling multiple .act files together
-- when they share a project root; otherwise fall back to per-file handling.
buildFiles :: C.GlobalOptions -> C.CompileOptions -> [FilePath] -> IO ()
buildFiles gopts opts files =
    case files of
      [single] -> runFile gopts opts single
      _ -> do
        when (C.watch opts) $
          printErrorAndExit "Cannot use --watch with multiple files. Use `acton build --watch` for projects or `acton build FILE --watch` (or `acton FILE --watch`) for a single file."
        absFiles <- mapM canonicalizePath files
        let onlyAct = all ((== ".act") . takeExtension) files
        projDirs <- mapM findProjectDir absFiles
        let projRoots = nub (catMaybes projDirs)
        case projRoots of
          [proj] | onlyAct && all (== Just proj) projDirs -> do
            let sp = Source.diskSourceProvider
                runBuild opts' =
                  withProjectCompileLock proj $
                    compileFiles sp gopts opts' absFiles False
            withOwnerLockOrOnlyBuild gopts proj
              (runBuild opts)
              (runBuild opts { C.only_build = True })
          _ -> mapM_ (runFile gopts opts) files

-- | Compute the path to the per-project compile lock.
projectCompileLockPath :: FilePath -> FilePath
projectCompileLockPath projDir =
    joinPath [projDir, ".actonc.lock"]

-- | Run an action while holding the per-project compile lock.
withProjectCompileLock :: FilePath -> IO a -> IO a
withProjectCompileLock projDir action =
    withFileLock (projectCompileLockPath projDir) Exclusive (\_ -> action)

-- | Collect all source files in a project's src/ directory.
projectSourceFiles :: Paths -> IO [FilePath]
projectSourceFiles paths = do
    allFiles <- getFilesRecursive (srcDir paths)
    return (catMaybes (map filterActFile allFiles))

-- | Prefer the compile-owner lock, fallback to only-build when unavailable.
withOwnerLockOrOnlyBuild :: C.GlobalOptions -> FilePath -> IO a -> IO a -> IO a
withOwnerLockOrOnlyBuild gopts projDir runFull runFallback = do
    ownerLock <- tryCompileOwnerLock projDir
    case ownerLock of
      Just lock -> runFull `finally` releaseCompileOwnerLock lock
      Nothing -> do
        unless (C.quiet gopts) $
          putStrLn "Compiler already running; running final build only."
        runFallback

-- | Require the compile-owner lock or abort with a message.
withOwnerLockOrExit :: FilePath -> String -> IO a -> IO a
withOwnerLockOrExit projDir msg action = do
    ownerLock <- tryCompileOwnerLock projDir
    case ownerLock of
      Nothing -> printErrorAndExit msg
      Just lock -> action `finally` releaseCompileOwnerLock lock

withProjectLockForGen :: CompileScheduler -> Int -> FilePath -> IO () -> IO ()
withProjectLockForGen sched gen projDir action =
    whenCurrentGen sched gen $
      withProjectCompileLock projDir $
        whenCurrentGen sched gen action

requireProjectLayout :: Paths -> IO ()
requireProjectLayout paths = do
    exists <- doesDirectoryExist (srcDir paths)
    unless exists $
      printErrorAndExit "Missing src/ directory"
-- | Strict path resolution: require a project config in the given directory.
loadProjectPathsAt :: FilePath -> C.CompileOptions -> IO Paths
loadProjectPathsAt curDir opts = do
    actPath <- requireProjectConfigPath curDir
    let projDir = takeDirectory actPath
    srcExists <- doesDirectoryExist (joinPath [projDir, "src"])
    unless srcExists $
      printErrorAndExit "Missing src/ directory"
    paths <- findPaths actPath opts
    requireProjectLayout paths
    return paths

loadProjectPaths :: C.CompileOptions -> IO Paths
loadProjectPaths opts = do
    curDir <- getCurrentDirectory
    loadProjectPathsAt curDir opts

requireProjectConfigPath :: FilePath -> IO FilePath
requireProjectConfigPath curDir = do
    let candidates =
          [ joinPath [curDir, "Build.act"]
          ]
    mpath <- firstExisting candidates
    case mpath of
      Just path -> return path
      Nothing -> printErrorAndExit "Project config not found in current directory (expected Build.act)"
  where
    firstExisting [] = return Nothing
    firstExisting (p:ps) = do
      exists <- doesFileExist p
      if exists then return (Just p) else firstExisting ps

ignoreNotExists :: IOException -> IO ()
ignoreNotExists _ = return ()

withScratchDirLock :: (FilePath -> IO a) -> IO a
withScratchDirLock action = do
    home <- getHomeDirectory
    let basePath = joinPath [home, ".cache", "acton", "scratch"]
    createDirectoryIfMissing True basePath
    maybeLockInfo <- findAvailableScratch basePath
    case maybeLockInfo of
      Nothing -> error "Could not acquire any scratch directory lock"
      Just (lock, lockPath) -> do
        let scratchDir = dropExtension lockPath
        removeDirectoryRecursive scratchDir `catch` ignoreNotExists
        action scratchDir `finally` unlockFile lock

withTempDirOpts :: C.CompileOptions -> (C.CompileOptions -> Bool -> IO a) -> IO a
withTempDirOpts opts action
  | C.tempdir opts /= "" = action opts False
  | otherwise = withScratchDirLock $ \scratchDir ->
      action opts { C.tempdir = scratchDir } True

initCompileWatchContext :: C.GlobalOptions -> IO (CompileScheduler, ProgressUI, ProgressState)
initCompileWatchContext gopts = do
    maxParallel <- compileMaxParallel gopts
    sched <- newCompileScheduler gopts maxParallel
    progressUI <- initProgressUI gopts maxParallel
    progressState <- newProgressState
    return (sched, progressUI, progressState)

logProjectBuild :: C.GlobalOptions -> ProgressUI -> ProgressState -> FilePath -> IO ()
logProjectBuild gopts progressUI progressState projDir =
    iff (not(C.quiet gopts)) $ do
      progressReset progressUI progressState
      progressLogLine progressUI ("Building project in " ++ projDir)
-- | Run a single project build under lock and generate docs.
buildProjectOnce :: C.GlobalOptions -> C.CompileOptions -> IO ()
buildProjectOnce gopts opts = do
                let sp = Source.diskSourceProvider
                paths <- loadProjectPaths opts
                iff (not(C.quiet gopts)) $ do
                  putStrLn("Building project in " ++ projPath paths)
                let projDir = projPath paths
                    runBuild opts' = withProjectCompileLock projDir $ do
                      srcFiles <- projectSourceFiles paths
                      compileFiles sp gopts opts' srcFiles True
                      generateProjectDocIndex sp gopts opts' paths srcFiles
                withOwnerLockOrOnlyBuild gopts projDir
                  (runBuild opts)
                  (runBuild opts { C.only_build = True })

-- Test runner -------------------------------------------------------------------------------------------------

-- | Entry point for acton test; configures options and selects mode/watch.
runTests :: C.GlobalOptions -> C.TestCommand -> IO ()
runTests gopts cmd = do
    let (mode, topts) =
          case cmd of
            C.TestRun opts  -> (TestModeRun, opts)
            C.TestList opts -> (TestModeList, opts)
            C.TestPerf opts -> (TestModePerf, opts)
        gopts' = if C.testJson topts then gopts { C.quiet = True } else gopts
        opts0 = C.testCompile topts
    let opts = opts0
          { C.test = True
          , C.skip_build = mode == TestModeList
          , C.only_build = False
          }
    paths <- loadProjectPaths opts
    if C.watch opts0
      then case mode of
             TestModeList -> runTestsOnce gopts' opts topts mode paths
             _ -> runTestsWatch gopts' opts topts mode paths
      else runTestsOnce gopts' opts topts mode paths

-- | Build once and then list/run tests based on the selected mode.
runTestsOnce :: C.GlobalOptions -> C.CompileOptions -> C.TestOptions -> TestMode -> Paths -> IO ()
runTestsOnce gopts opts topts mode paths = do
    buildProjectOnce gopts opts
    modules <- listTestModules opts paths
    case mode of
      TestModeList -> listProjectTests opts paths topts modules
      _ -> do
        maxParallel <- testMaxParallel gopts
        useColorOut <- useColor gopts
        exitCode <- runProjectTests useColorOut gopts opts paths topts mode modules maxParallel
        exitWithTestCode exitCode

-- | Watch mode for tests that rebuilds incrementally and reruns changed modules.
runTestsWatch :: C.GlobalOptions -> C.CompileOptions -> C.TestOptions -> TestMode -> Paths -> IO ()
runTestsWatch gopts opts topts mode paths = do
    let sp = Source.diskSourceProvider
        projDir = projPath paths
        srcRoot = srcDir paths
    (sched, progressUI, progressState) <- initCompileWatchContext gopts
    testParallel <- testMaxParallel gopts
    let runOnce gen mChanged = do
          withProjectLockForGen sched gen projDir $ do
            logProjectBuild gopts progressUI progressState projDir
            srcFiles <- projectSourceFiles paths
            hadErrors <- compileFilesChanged sp gopts opts srcFiles True mChanged (Just (sched, gen)) (Just (progressUI, progressState))
            unless hadErrors $ do
              testModules <- listTestModules opts paths
              modulesToTest <- selectTestModules paths srcFiles mChanged testModules
              unless (null modulesToTest) $
                do
                  useColorOut <- useColor gopts
                  void $ runProjectTests useColorOut gopts opts paths topts mode modulesToTest testParallel
    withOwnerLockOrExit projDir "Another compiler is running; cannot start test watch." $
      runWatchProject gopts projDir srcRoot sched runOnce

selectTestModules :: Paths -> [FilePath] -> Maybe [FilePath] -> [String] -> IO [String]
selectTestModules paths srcFiles mChanged testModules =
    case mChanged of
      Nothing -> return testModules
      Just changedPaths -> do
        changedModules <- changedModulesFromPaths paths changedPaths
        affected <- dependentTestModulesFromHeaders paths srcFiles changedModules
        return (filter (`elem` testModules) affected)

-- | Compute parallelism for test runs from jobs or core count.
testMaxParallel :: C.GlobalOptions -> IO Int
testMaxParallel gopts = do
    nCaps <- getNumCapabilities
    return (max 1 (if C.jobs gopts > 0 then C.jobs gopts else max 1 (nCaps `div` 2)))

-- | Exit with a status derived from test errors and failures.
exitWithTestCode :: Int -> IO ()
exitWithTestCode code
  | code <= 0 = System.Exit.exitSuccess
  | otherwise = System.Exit.exitWith (ExitFailure code)

changedModulesFromPaths :: Paths -> [FilePath] -> IO [String]
changedModulesFromPaths paths files = do
    mods <- forM files $ \file -> do
      mn <- moduleNameFromFile (srcDir paths) file
      return (modNameToString mn)
    return (Data.List.sort (nub mods))

readModuleImports :: Paths -> A.ModName -> IO [A.ModName]
readModuleImports paths mn = do
    let tyFile = outBase paths mn ++ ".ty"
    exists <- doesFileExist tyFile
    if not exists
      then return []
      else do
        hdrE <- (try :: IO a -> IO (Either SomeException a)) $ InterfaceFiles.readHeader tyFile
        case hdrE of
          Left _ -> return []
          Right (_hash, _ih, _implH, imps, _nameHashes, _roots, _tests, _doc) -> return (map fst imps)

dependentTestModulesFromHeaders :: Paths -> [FilePath] -> [String] -> IO [String]
dependentTestModulesFromHeaders paths srcFiles changedModules = do
    depsByMod <- forM srcFiles $ \file -> do
      mn <- moduleNameFromFile (srcDir paths) file
      imps <- readModuleImports paths mn
      return (modNameToString mn, map modNameToString imps)
    let revMap = foldl'
          (\acc (mn, deps) -> foldl' (\a dep -> M.insertWith (++) dep [mn] a) acc deps)
          M.empty
          depsByMod
        affected = reverseClosure revMap (Data.Set.fromList changedModules)
    return (Data.List.sort (Data.Set.toList affected))
  where
    reverseClosure revMap start = go start (Data.Set.toList start)
      where
        go seen [] = seen
        go seen (k:ks) =
          let ds = M.findWithDefault [] k revMap
              new = filter (`Data.Set.notMember` seen) ds
              seen' = foldl' (flip Data.Set.insert) seen new
          in go seen' (ks ++ new)

-- | Watch a project directory and rebuild on source or build spec changes.
watchProjectAt :: C.GlobalOptions -> C.CompileOptions -> FilePath -> IO ()
watchProjectAt gopts opts projDir = do
                let sp = Source.diskSourceProvider
                paths <- loadProjectPathsAt projDir opts
                withOwnerLockOrExit (projPath paths) "Another compiler is running; cannot start watch." $ do
                  (sched, progressUI, progressState) <- initCompileWatchContext gopts
                  let runOnce gen mChanged =
                        withProjectLockForGen sched gen (projPath paths) $ do
                          logProjectBuild gopts progressUI progressState (projPath paths)
                          srcFiles <- projectSourceFiles paths
                          void $ compileFilesChanged sp gopts opts srcFiles True mChanged (Just (sched, gen)) (Just (progressUI, progressState))
                          when (isNothing mChanged) $
                            generateProjectDocIndex sp gopts opts paths srcFiles
                  runWatchProject gopts (projPath paths) (srcDir paths) sched runOnce

-- | Build a single file, optionally running in watch mode.
buildFile :: C.GlobalOptions -> C.CompileOptions -> FilePath -> IO ()
buildFile gopts opts file
    | C.watch opts = watchFile gopts opts file
    | otherwise = buildFileOnce gopts opts file

-- | Compile one file in project or scratch mode with locking.
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
        -- In a project, use project directory for compilation.
        iff (not(C.quiet gopts)) $ do
          putStrLn("Building file " ++ file ++ " in project " ++ relProj)
        let runBuild opts' =
              withProjectCompileLock proj $
                compileFiles sp gopts opts' [file] False
        withOwnerLockOrOnlyBuild gopts proj
          (runBuild opts)
          (runBuild opts { C.only_build = True })
      Nothing -> do
        -- Not in a project, use scratch directory for compilation unless
        -- --tempdir is provided - then use that
        withTempDirOpts opts $ \opts' usedScratch -> do
          iff (not(C.quiet gopts)) $ do
            if usedScratch
              then do
                let scratch_dir = if (C.verbose gopts) then " " ++ C.tempdir opts' else ""
                putStrLn("Building file " ++ file ++ " using temporary scratch directory" ++ scratch_dir)
              else
                putStrLn("Building file " ++ file ++ " using temporary directory " ++ C.tempdir opts')
          compileFiles sp gopts opts' [file] False

-- | Watch a single file and rebuild on changes.
watchFile :: C.GlobalOptions -> C.CompileOptions -> FilePath -> IO ()
watchFile gopts opts file = do
    absFile <- canonicalizePath file
    projDir <- findProjectDir absFile
    case projDir of
      Just proj -> watchProjectAt gopts opts proj
      Nothing -> do
        let sp = Source.diskSourceProvider
        (sched, progressUI, progressState) <- initCompileWatchContext gopts
        let runWatch opts' =
              let runOnce gen mChanged =
                    whenCurrentGen sched gen $
                      void $ compileFilesChanged sp gopts opts' [absFile] False mChanged (Just (sched, gen)) (Just (progressUI, progressState))
              in runWatchFile gopts absFile sched runOnce
        withTempDirOpts opts $ \opts' _ ->
          runWatch opts'

data WatchTrigger = WatchFull | WatchIncremental FilePath deriving (Eq, Show)

data FileStamp = FileStamp UTCTime Integer deriving (Eq, Show)

readFileStamp :: FilePath -> IO (Maybe FileStamp)
readFileStamp path = do
    res <- try (do
      exists <- doesFileExist path
      if not exists
        then return Nothing
        else do
          stampTime <- getModificationTime path
          stampSize <- getFileSize path
          return (Just (FileStamp stampTime stampSize))
      ) :: IO (Either IOException (Maybe FileStamp))
    case res of
      Left _ -> return Nothing
      Right stamp -> return stamp

shouldTriggerFile :: IORef (M.Map FilePath FileStamp) -> FilePath -> IO Bool
shouldTriggerFile stampsRef path = do
    mstamp <- readFileStamp path
    atomicModifyIORef' stampsRef $ \stamps ->
      case mstamp of
        Nothing -> (M.delete path stamps, True)
        Just stamp ->
          case M.lookup path stamps of
            Just old | old == stamp -> (stamps, False)
            _ -> (M.insert path stamp stamps, True)

-- | Translate watch triggers into optional path notifications.
dispatchWatchTrigger :: (Maybe FilePath -> IO ()) -> WatchTrigger -> IO ()
dispatchWatchTrigger notify trigger =
    case trigger of
      WatchFull -> notify Nothing
      WatchIncremental path -> notify (Just path)

-- | Classify project FS events into full or incremental rebuild triggers.
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
                 FS.ModifiedAttributes{} -> Nothing
                 FS.Added{} -> Just WatchFull
                 FS.Removed{} -> Just WatchFull
                 FS.WatchedDirectoryRemoved{} -> Just WatchFull
                 FS.Unknown{} -> Just WatchFull
                 _ -> Just (WatchIncremental (FS.eventPath ev))

-- | Classify FS events for a specific file path.
fileWatchTrigger :: FilePath -> FS.Event -> Maybe WatchTrigger
fileWatchTrigger target ev =
    let evPath = normalise (FS.eventPath ev)
    in if evPath /= target
         then Nothing
         else case ev of
                FS.ModifiedAttributes{} -> Nothing
                _ -> Just (WatchIncremental (FS.eventPath ev))

-- | Run the project watch loop and schedule compiles on events.
runWatchProject :: C.GlobalOptions
                -> FilePath
                -> FilePath
                -> CompileScheduler
                -> (Int -> Maybe [FilePath] -> IO ())
                -> IO ()
runWatchProject gopts projDir srcRoot sched runOnce = do
    stampsRef <- newIORef M.empty
    let schedule mpath = void $ startCompile sched 0 $ \gen ->
          runOnce gen (fmap (:[]) mpath)
        scheduleMaybe mpath =
          case mpath of
            Nothing -> schedule Nothing
            Just path -> do
              should <- shouldTriggerFile stampsRef path
              when should (schedule (Just path))
        onAct ev = forM_ (actWatchTrigger ev) $ \trigger -> do
          when (C.verbose gopts) $
            putStrLn ("[debug] watch event: " ++ show ev ++ " -> " ++ show trigger)
          dispatchWatchTrigger scheduleMaybe trigger
        onRoot _ = schedule Nothing
        isActEvent ev = isJust (actWatchTrigger ev)
        isRootEvent ev =
          let name = takeFileName (FS.eventPath ev)
          in name == "Build.act"
    FS.withManager $ \mgr -> do
      _ <- FS.watchTree mgr srcRoot isActEvent onAct
      _ <- FS.watchDir mgr projDir isRootEvent onRoot
      schedule Nothing
      unless (C.quiet gopts) $
        putStrLn ("Watching for changes in " ++ projDir)
      forever $ threadDelay maxBound

-- | Run the single-file watch loop and schedule compiles on events.
runWatchFile :: C.GlobalOptions
             -> FilePath
             -> CompileScheduler
             -> (Int -> Maybe [FilePath] -> IO ())
             -> IO ()
runWatchFile gopts absFile sched runOnce = do
    stampsRef <- newIORef M.empty
    let watchDir = takeDirectory absFile
        watchPath = normalise absFile
        isTarget ev = normalise (FS.eventPath ev) == watchPath
        onEvent ev = do
          forM_ (fileWatchTrigger watchPath ev) $ \trigger ->
            case trigger of
              WatchFull -> dispatchWatchTrigger schedule trigger
              WatchIncremental path -> do
                should <- shouldTriggerFile stampsRef path
                when should (dispatchWatchTrigger schedule trigger)
        schedule mpath = void $ startCompile sched 0 $ \gen ->
          runOnce gen (fmap (:[]) mpath)
    FS.withManager $ \mgr -> do
      _ <- FS.watchDir mgr watchDir isTarget onEvent
      schedule Nothing
      unless (C.quiet gopts) $
        putStrLn ("Watching for changes in " ++ absFile)
      forever $ threadDelay maxBound

-- | Fetch dependencies for the current project without compiling.
fetchCommand :: C.GlobalOptions -> IO ()
fetchCommand gopts = do
    paths <- loadProjectPaths defaultCompileOptions
    res <- try (fetchDependencies gopts paths []) :: IO (Either ProjectError ())
    case res of
      Left (ProjectError msg) -> printErrorAndExit msg
      Right () ->
        unless (C.quiet gopts) $
          putStrLn "Dependencies fetched"

-- Show dependency tree with overrides applied from root pins
pkgShow :: C.GlobalOptions -> IO ()
pkgShow gopts = do
    curDir <- getCurrentDirectory
    _ <- requireProjectConfigPath curDir
    spec <- loadBuildSpec curDir
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
        spec' <- loadBuildSpec depBase
        showTree pins depBase spec' (depth + 1)

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
openFileInGui :: FilePath -> IO ()
openFileInGui path = do
    let openCmd = case System.Info.os of
          "darwin" -> "open"
          "linux" -> "xdg-open"
          _ -> ""
    unless (null openCmd) $
      void $ system $ openCmd ++ " " ++ path

-- | Generate and display documentation based on CLI options and environment.
printDocs :: C.GlobalOptions -> C.DocOptions -> IO ()
printDocs gopts opts = do
    case C.inputFile opts of
      "" -> do
        -- No file provided - check what to do
        case C.outputFormat opts of
          Just C.AsciiFormat -> printErrorAndExit "Terminal output requires a specific file. Usage: acton doc -t <file.act>"
          Just C.MarkdownFormat -> printErrorAndExit "Markdown output requires a specific file. Usage: acton doc --md <file.act>"
          _ -> do
              -- HTML or auto mode - check if we're in a project
              curDir <- getCurrentDirectory
              projDir <- findProjectDir curDir
              case projDir of
                Just _ -> do
                  -- We're in a project - open the documentation index
                  let indexFile = "out/doc/index.html"
                  indexExists <- doesFileExist indexFile
                  if indexExists
                    then openFileInGui indexFile
                    else printErrorAndExit "No documentation found. Run 'acton build' first to generate documentation."
                Nothing ->
                  printErrorAndExit "Not in an Acton project. Please specify a file to document."
      filename -> do
        let (fileBody,fileExt) = splitExtension $ takeFileName filename

        case fileExt of
          ".ty" -> do
            paths <- findPaths filename defaultCompileOptions
            env0 <- Acton.Env.initEnv (sysTypes paths) False
            Acton.Types.showTyFile env0 (modName paths) filename (C.verbose gopts)

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
            (nmod, _, env', _, _) <- Acton.Types.reconstruct env kchecked
            let I.NModule tenv mdoc = nmod

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
              Nothing
                | isJust (C.outputFormat opts) || format == C.AsciiFormat ->
                    -- Explicit format or auto ASCII: write to stdout
                    putStr docOutput
                | otherwise -> do
                    -- Auto-detected HTML (DISPLAY set), write to file and open browser
                    curDir <- getCurrentDirectory
                    projDir <- findProjectDir curDir
                    outputPath <- case projDir of
                      Just _ -> do
                        -- In project: use out/doc/
                        let modPath = map (replace ".act" "") $ splitOn "/" filename
                            cleanPath = case modPath of
                                "src":rest -> rest
                                path -> path
                            docFile = if null cleanPath
                                      then "out/doc/unnamed.html"
                                      else joinPath ("out" : "doc" : init cleanPath) </> last cleanPath <.> "html"
                        return docFile
                      Nothing ->
                        -- Outside project: use temp file
                        writeSystemTempFile "acton-doc.html" docOutput

                    case projDir of
                      Just _ -> do
                        createDirectoryIfMissing True (takeDirectory outputPath)
                        writeFile outputPath docOutput
                      Nothing -> return ()

                    putStrLn $ "HTML documentation written to: " ++ outputPath
                    openFileInGui outputPath

          _ -> printErrorAndExit ("Unknown filetype: " ++ filename)


-- Compile Acton files ---------------------------------------------------------------------------------------------

-- | Compute parallelism for compilation based on jobs or cores.
compileMaxParallel :: C.GlobalOptions -> IO Int
compileMaxParallel gopts = do
    nCaps <- getNumCapabilities
    return (max 1 (if C.jobs gopts > 0 then C.jobs gopts else nCaps))

-- | Compile a set of files in a single-shot build.
compileFiles :: Source.SourceProvider -> C.GlobalOptions -> C.CompileOptions -> [String] -> Bool -> IO ()
compileFiles sp gopts opts srcFiles allowPrune =
    void $ compileFilesChanged sp gopts opts srcFiles allowPrune Nothing Nothing Nothing

-- | Compile with optional change set, wiring progress UI and back jobs.
compileFilesChanged :: Source.SourceProvider
                    -> C.GlobalOptions
                    -> C.CompileOptions
                    -> [String]
                    -> Bool
                    -> Maybe [FilePath]
                    -> Maybe (CompileScheduler, Int)
                    -> Maybe (ProgressUI, ProgressState)
                    -> IO Bool
compileFilesChanged sp gopts opts srcFiles allowPrune mChangedPaths mSched mProgress = do
    maxParallel <- compileMaxParallel gopts
    (progressUI, progressState) <- case mProgress of
      Just ps -> return ps
      Nothing -> do
        ui <- initProgressUI gopts maxParallel
        st <- newProgressState
        return (ui, st)
    let logLine = progressLogLine progressUI
    (sched, gen) <- case mSched of
      Just sg -> return sg
      Nothing -> do
        sched' <- newCompileScheduler gopts maxParallel
        return (sched', 0)
    let cleanupProgress = whenCurrentGen sched gen (progressReset progressUI progressState)
    cleanupProgress
    let runCompile = do
          sp' <- overlayChangedPaths sp mChangedPaths
          planRes <- try $
            prepareCompilePlan sp' gopts sched opts srcFiles allowPrune mChangedPaths
          let reportPlanError (ProjectError msg) = do
                if C.watch opts
                  then logLine msg
                  else printErrorAndExit msg
                return True
              runPlan plan = do
                let cctx = cpContext plan
                    opts' = ccOpts cctx
                    pathsRoot = ccPathsRoot cctx
                    watchMode = C.watch opts'
                cliHooks <- initCliCompileHooks progressUI progressState gopts sched gen plan
                let clearProgress = whenCurrentGen sched gen (cchClearProgress cliHooks)
                    finalizeCompile onError = do
                      clearProgress
                      cleanup gopts opts' pathsRoot
                      onError
                      return True
                    reportCompileError msg =
                      finalizeCompile $
                        if watchMode
                          then logLine msg
                          else printErrorAndExit msg
                    reportCompileErrors =
                      finalizeCompile $
                        unless watchMode System.Exit.exitFailure
                compileRes <- runCompilePlan sp gopts plan sched gen (cchHooks cliHooks)
                case compileRes of
                  Left err ->
                    reportCompileError (compileFailureMessage err)
                  Right (env, hadErrors) -> do
                    backFailure <-
                      if C.only_build opts'
                        then return Nothing
                        else backQueueWait (csBackQueue sched) gen
                    case backFailure of
                      Just failure ->
                        reportCompileError (backPassFailureMessage failure)
                      Nothing ->
                        if hadErrors
                          then reportCompileErrors
                          else do
                            clearProgress
                            whenCurrentGen sched gen (runCliPostCompile cliHooks gopts plan env)
                            return False
          either reportPlanError runPlan planRes
    runCompile `finally` cleanupProgress

overlayChangedPaths :: Source.SourceProvider -> Maybe [FilePath] -> IO Source.SourceProvider
overlayChangedPaths sp mPaths = do
    case mPaths of
      Nothing -> return sp
      Just paths -> do
        overlays <- catMaybes <$> mapM readOverlay paths
        let overlayMap = M.fromList overlays
        return sp
          { Source.spReadOverlay = \path -> do
              existing <- Source.spReadOverlay sp path
              case existing of
                Just snap -> return (Just snap)
                Nothing -> return (M.lookup (normalise path) overlayMap)
          }
  where
    readOverlay path = do
      res <- (try :: IO a -> IO (Either SomeException a)) (Source.spReadFile sp path)
      case res of
        Left _ -> return Nothing
        Right snap ->
          return (Just (normalise path, snap { Source.ssIsOverlay = True }))

data CliCompileHooks = CliCompileHooks
  { cchHooks :: CompileHooks
  , cchLogLine :: String -> IO ()
  , cchClearProgress :: IO ()
  , cchFinalStart :: IO ()
  , cchFinalDone :: Maybe TimeSpec -> IO ()
  , cchProgressUI :: ProgressUI
  }

-- | Build compile hooks and state for CLI progress and completion logs.
initCliCompileHooks :: ProgressUI
                    -> ProgressState
                    -> C.GlobalOptions
                    -> CompileScheduler
                    -> Int
                    -> CompilePlan
                    -> IO CliCompileHooks
initCliCompileHooks progressUI progressState gopts sched gen plan = do
    let neededTasks = cpNeededTasks plan
    sizeEntries <- forM neededTasks $ \t -> do
      let key = gtKey t
          path = srcFile (gtPaths t) (tkMod key)
      exists <- doesFileExist path
      sz <- if exists then getFileSize path else return 0
      return (key, sz)
    progressRef <- newIORef (0 :: Double)
    marksRef <- newIORef (M.empty :: M.Map TaskKey (Bool, Bool))
    -- sizeMap is used for the progress bar indicator to determine how much a
    -- modules compilation should contribute to the overall progress. We use
    -- file size as a heuristic for this, but if all sizes are zero (e.g. due to
    -- missing files or all files being empty), we fall back to equal weighting
    -- to avoid division by zero. Since our compilation is split into front and
    -- back passes, we also compute a share for each task that determines how
    -- much of the total progress it should contribute to, which is used to give
    -- a more accurate progress indication during the compile.
    let sizeMap = M.fromList sizeEntries
        totalSize = sum (map snd sizeEntries)
        useEqual = totalSize <= 0
        totalWeight :: Double
        totalWeight =
          if useEqual
            then fromIntegral (max 1 (length neededTasks))
            else fromIntegral totalSize
        weightFor key =
          if useEqual
            then 1 / totalWeight
            else fromIntegral (M.findWithDefault 0 key sizeMap) / totalWeight
        compilePhaseTotal = 85.0
        frontRatio = 0.7
        backRatio = 0.3
        shareMap =
          M.fromList
            [ (gtKey t, (compilePhaseTotal * weightFor (gtKey t) * frontRatio
                        , compilePhaseTotal * weightFor (gtKey t) * backRatio))
            | t <- neededTasks
            ]
    let gate = whenCurrentGen sched gen
        logLine msg = gate (progressLogLine progressUI msg)
        logDiagnostics optsT diags =
          gate (progressWithLog progressUI (printDiagnostics gopts optsT diags))
        rootProj = ccRootProj (cpContext plan)
        optsPlan = ccOpts (cpContext plan)
        termProgress = puTermProgress progressUI
        termEnabled = termProgressEnabled termProgress
        modLabel mn = modNameToString mn
        spinnerPrefixWidth = 3
        frontPrefix = "   Finished type check of"
        backPrefix = "   Finished compilation of "
        backFailPrefix = "   Failed compilation of "
        finalPrefix = "   Finished final compilation"
        phaseFront = "Running front passes:"
        phaseBack = "Running back passes:"
        phaseFinal = "Running final compilation:"
        phaseWidth = maximum [length phaseFront, length phaseBack, length phaseFinal]
        projMap = cpProjMap plan
        depNameMap =
          let rootDeps = maybe [] projDeps (M.lookup rootProj projMap)
              otherDeps = concatMap projDeps (M.elems projMap)
          in M.fromListWith (\_ old -> old) [ (p, n) | (n, p) <- rootDeps ++ otherDeps ]
        projectLabelFor proj
          | proj == rootProj = ""
          | otherwise =
              case M.lookup proj depNameMap of
                Just name -> name
                Nothing -> projectLabel rootProj proj
        projectModuleLabel proj mn =
          case projectLabelFor proj of
            "" -> modLabel mn
            label -> label ++ "." ++ modLabel mn
        labelWidth = maximum (0 : [ length (projectModuleLabel (tkProj (gtKey t)) (tkMod (gtKey t))) | t <- neededTasks ])
        prefixWidth = maximum [ length frontPrefix
                              , length backPrefix
                              , length backFailPrefix
                              , length finalPrefix
                              , spinnerPrefixWidth + phaseWidth + 1
                              ]
        progressPrefixWidth = max 0 (prefixWidth - spinnerPrefixWidth)
        padProgressPrefix phase = padRight progressPrefixWidth (padRight phaseWidth phase ++ " ")
        completionPrefix prefix = padRight prefixWidth prefix
        timeSep = "    "
        nameWidth = prefixWidth + labelWidth
        timePadWidth = nameWidth + length timeSep
        progressTimePadWidth = max 0 (timePadWidth - spinnerPrefixWidth)
        frontDoneLine proj mn t =
          let base = completionPrefix frontPrefix ++ padRight labelWidth (projectModuleLabel proj mn)
          in padRight timePadWidth base ++ fmtTime t
        backDoneLine proj mn mt =
          let base = completionPrefix backPrefix ++ padRight labelWidth (projectModuleLabel proj mn)
          in case mt of
               Just t -> padRight timePadWidth base ++ fmtTime t
               Nothing -> base
        backFailLine proj mn msg =
          let base = completionPrefix backFailPrefix ++ projectModuleLabel proj mn
          in base ++ "    " ++ msg
        finalDoneLine t =
          padRight timePadWidth finalPrefix ++ fmtTime t
        progressLine phase proj mn =
          let base = padProgressPrefix phase ++ padRight labelWidth (projectModuleLabel proj mn)
          in padRight progressTimePadWidth base
        progressFinalLine =
          padRight progressTimePadWidth (padProgressPrefix phaseFinal)
        finalKey = TaskKey rootProj (A.modName ["__final__"])
        withTerm action = when termEnabled $ gate (withProgressLock progressUI action)
        setPercent pct = withTerm (termProgressPercent termProgress pct)
        addProgress delta =
          when (termEnabled && delta > 0) $ do
            new <- atomicModifyIORef' progressRef (\x -> let x' = min compilePhaseTotal (x + delta) in (x', x'))
            setPercent (floor new)
        shareFor key = M.findWithDefault (0, 0) key shareMap
        creditFront key = gate $ do
          let (frontShare, _) = shareFor key
          delta <- atomicModifyIORef' marksRef $ \m ->
            let (frontDone, backDone) = M.findWithDefault (False, False) key m
            in if frontDone
                 then (m, 0)
                 else (M.insert key (True, backDone) m, frontShare)
          addProgress delta
        creditBack key = gate $ do
          let (_, backShare) = shareFor key
          delta <- atomicModifyIORef' marksRef $ \m ->
            let (frontDone, backDone) = M.findWithDefault (False, False) key m
            in if backDone
                 then (m, 0)
                 else (M.insert key (frontDone, True) m, backShare)
          addProgress delta
    setPercent 0
    let backJobKey job =
          TaskKey (projPath (bjPaths job)) (A.modname (biTypedMod (bjInput job)))
        onBackStart job =
          let proj = projPath (bjPaths job)
              mn = A.modname (biTypedMod (bjInput job))
          in do
            gate (progressStartTask progressUI progressState (backJobKey job) (progressLine phaseBack proj mn))
        onBackDone job result = do
          gate (progressDoneTask progressUI progressState (backJobKey job))
          creditBack (backJobKey job)
          when (not (quiet gopts optsPlan)) $
            case result of
              BackJobOk mtime ->
                logLine (backDoneLine (projPath (bjPaths job)) (A.modname (biTypedMod (bjInput job))) mtime)
              BackJobFailed failure ->
                logLine (backFailLine (projPath (bjPaths job))
                                      (A.modname (biTypedMod (bjInput job)))
                                      (bpfMessage failure))
        hooks = defaultCompileHooks
          { chOnDiagnostics = \t optsT diags -> do
              gate (progressDoneTask progressUI progressState (gtKey t))
              logDiagnostics optsT diags
          , chOnFrontResult = \t fr -> do
              forM_ (frFrontTime fr) (\tFront ->
                logLine (frontDoneLine (tkProj (gtKey t)) (tkMod (gtKey t)) tFront))
              case frBackJob fr of
                Nothing -> creditBack (gtKey t)
                Just _ -> return ()
          , chOnFrontStart = \t ->
              let key = gtKey t
                  proj = tkProj key
                  mn = tkMod key
              in gate (progressStartTask progressUI progressState key (progressLine phaseFront proj mn))
          , chOnFrontDone = \t -> do
              gate (progressDoneTask progressUI progressState (gtKey t))
              creditFront (gtKey t)
          , chOnBackQueued = \_ _ -> return ()
          , chOnBackStart = onBackStart
          , chOnBackDone = onBackDone
          , chOnInfo = logLine
          }
        onFinalStart = do
          gate (progressStartTask progressUI progressState finalKey progressFinalLine)
          setPercent 85
        onFinalDone mtime = do
          gate (progressDoneTask progressUI progressState finalKey)
          setPercent 100
          forM_ mtime $ \tFinal ->
            when (not (quiet gopts optsPlan)) $
              logLine (finalDoneLine tFinal)
    return CliCompileHooks
      { cchHooks = hooks
      , cchLogLine = logLine
      , cchClearProgress = progressReset progressUI progressState
      , cchFinalStart = onFinalStart
      , cchFinalDone = onFinalDone
      , cchProgressUI = progressUI
      }

-- | Run CLI-only post-compile steps.
runCliPostCompile :: CliCompileHooks
                  -> C.GlobalOptions
                  -> CompilePlan
                  -> Acton.Env.Env0
                  -> IO ()
runCliPostCompile cliHooks gopts plan env = do
    let logLine = cchLogLine cliHooks
    let cctx = cpContext plan
        opts' = ccOpts cctx
        pathsRoot = ccPathsRoot cctx
        rootProj = ccRootProj cctx
        sysAbs = ccSysAbs cctx
        rootTasks = cpRootTasks plan
        rootPins = cpRootPins plan
        allowPrune' = cpAllowPrune plan
        globalTasks = cpGlobalTasks plan
        projMap = cpProjMap plan
        sysRoot = addTrailingPathSeparator sysAbs
    rootSpec <- case M.lookup rootProj projMap of
                  Just ctx -> return (projBuildSpec ctx)
                  Nothing -> throwProjectError ("Missing root project context for " ++ rootProj)
    let rootParts = splitOn "." (C.root opts')
        rootMod   = init rootParts
        guessMod  = if length rootParts == 1 then modName pathsRoot else A.modName rootMod
        binTask   = BinTask False (prstr guessMod) (A.GName guessMod (A.name $ last rootParts)) False
        preBinTasks
          | null (C.root opts') = map (\t -> BinTask True (modNameToString (name t)) (A.GName (name t) (A.name "main")) False) rootTasks
          | otherwise        = [binTask]
        preTestBinTasks = map (\t -> BinTask True (modNameToString (name t)) (A.GName (name t) (A.name "test_main")) True) rootTasks
    -- Generate build.zig(.zon) for dependencies too, to satisfy Zig builder links.
    let projKeys = Data.Set.fromList (map (tkProj . gtKey) globalTasks)
    forM_ (Data.Set.toList projKeys) $ \p -> do
      let isRootProj = p == rootProj
          isSysProj  = p == sysAbs || sysRoot `isPrefixOf` p
      unless (isRootProj || isSysProj) $
        case M.lookup p projMap of
          Just pctx -> do
            when (C.verbose gopts) $
              logLine ("Generating build.zig for dependency project " ++ p)
            dummyPaths <- pathsForModule opts' projMap pctx (A.modName ["__gen_build__"])
            genBuildZigFiles (projBuildSpec pctx) rootPins (ccDepOverrides cctx) dummyPaths
          Nothing -> return ()
    let runFinal action = do
          cchFinalStart cliHooks
          mtime <- action `onException` cchFinalDone cliHooks Nothing
          cchFinalDone cliHooks (Just mtime)
    if C.skip_build opts'
      then
        logLine "  Skipping final build step"
      else
        if C.test opts'
          then do
            testBinTasks <- catMaybes <$> mapM (filterMainActor env pathsRoot) preTestBinTasks
            unless (altOutput opts') $
              runFinal (compileBins gopts opts' pathsRoot env rootSpec rootTasks testBinTasks allowPrune' (Just (cchProgressUI cliHooks)))
          else do
            unless (altOutput opts') $
              runFinal (compileBins gopts opts' pathsRoot env rootSpec rootTasks preBinTasks allowPrune' (Just (cchProgressUI cliHooks)))
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

-- | Remove orphaned files in out/types.
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

-- | Determine which root stub files should be preserved for the current build.
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
          Right (_, _, _implH, _imps, _nameHashes, rs, _tests, _) -> return (map (mkStub outbase) rs)
          _ -> return []
    return (concat roots)
  where
    mkStub outbase n =
      if nameToString n == "test_main"
        then outbase ++ ".test_root.c"
        else outbase ++ ".root.c"

data BinTask = BinTask { isDefaultRoot :: Bool, binName :: String, rootActor :: A.QName, isTest :: Bool } deriving (Show)

binTaskRoot :: Paths -> BinTask -> FilePath
binTaskRoot paths binTask =
    let A.GName m _ = rootActor binTask
        outbase = outBase paths m
    in if isTest binTask then outbase ++ ".test_root.c" else outbase ++ ".root.c"

{-
================================================================================
Build Pipeline Overview
================================================================================

We want the compiler to be fast. The primary principle by which to achieve this
is to avoid doing unnecessary work. Practically, this happens by caching
information in .ty files and only selectively reading what we need. We do not
eagerly load whole .ty files but rather read the header fields: moduleSrcBytesHash,
modulePubHash, moduleImplHash, imports, per-name hashes (src/pub/impl + deps),
roots, tests, and docstrings. This lets us quickly decide which passes to rerun and
reuse work from previous compilations.

Public hashing: each top-level name gets a pubHash computed from its doc-free
signature plus the pub hashes of its public dependencies. The modulePubHash is
then the hash of pubHash values for exported names. Doc-only edits do not affect
pubHash, and a downstream module only needs front passes when a pubHash changes.

Implementation hashing: each top-level name gets an implHash computed from its
source hash plus the impl hashes of its dependencies. The moduleImplHash is the
hash of all per-name impl hashes. We embed moduleImplHash into generated .c/.h
files so we can skip back passes when codegen is already up to date, and we use
it (with impl deps) to drive the test cache.

Terminology
- ActonTask: a module parsed from source (.act) and that needs to be compiled
- TyTask: a module loaded from the cached .ty file on disk. Note how there are
  two variants, a stubbed TyTask where only header fields are loaded and the
  full TyTask where all module content is available.

High-level Steps
1) Discover and read tasks using header-first strategy (readModuleTask)
   - For each module, try to use its .ty header to avoid parsing:
     - If .ty is missing/unreadable → parse .act to obtain imports (ActonTask).
     - If .ty exists and both .act and the acton executable mtime <= .ty mtime
       → trust .ty header imports and create a TyTask stub (no heavy decode)
       for graph building. Use --ignore-compiler-version to skip the acton part.
     - If .act appears newer than .ty → verify by content hash:
       – If stored moduleSrcBytesHash == current bytes hash → header is still valid (TyTask)
       – Else → parse .act now to get accurate imports (ActonTask)
   - This ensures that .ty is up to date with the .act source and lets us
     read module imports/roots/docstring from the header, which is much faster
     than parsing the .act file.

2) Expand to include transitive project imports (readImports)
   - For project builds, we enumerate all modules under src/ upfront, so chasing
     is typically a no-op.
   - For single-file builds (or partial sets), we chase imports from the seeded
     tasks to ensure all project-local dependencies are included. We prefer .ty
     headers to avoid parsing; we parse only when a .ty is missing/unreadable.

3) Compile and build
   - compileTasks orders modules in dependency (topological) order and decides
     rebuilds as it goes:
     - Source changed (ActonTask) → run front passes
     - Otherwise, compare each used pub dependency hash from the dependent’s
       .ty header with the provider’s current pub hash. If any differ → front.
     - Otherwise, compare each used impl dependency hash from the dependent’s
       .ty header with the provider’s current impl hash. If any differ → refresh
       impl hashes and run back passes.
     - Otherwise, if generated .c/.h hashes do not match moduleImplHash → run
       back passes.
     - Otherwise → module is fresh (no work).
   - We maintain a pubMap while walking modules in topological order. After a
     module compiles, we insert its freshly computed public hash; when a
     module is fresh (TyTask), we insert the recorded header hash. Dependents
     then consult pubMap to detect public deltas among their imports.
   - TyTask items remain lazy; code that needs only small bits (e.g., writeRootC)
     reads roots/doc from the header instead of forcing heavy loads.
   - Final Zig C compilation...

================================================================================
-}

compileBins:: C.GlobalOptions -> C.CompileOptions -> Paths -> Acton.Env.Env0 -> BuildSpec.BuildSpec -> [CompileTask] -> [BinTask] -> Bool -> Maybe ProgressUI -> IO TimeSpec
compileBins gopts opts paths env rootSpec tasks binTasks allowPrune mProgressUI =
    zigBuild env gopts opts paths rootSpec tasks binTasks allowPrune mProgressUI

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

-- | Generate a root actor C file when a root is still declared.
writeRootC :: Acton.Env.Env0 -> C.GlobalOptions -> C.CompileOptions -> Paths -> [CompileTask] -> BinTask -> IO (Maybe BinTask)
writeRootC env gopts opts paths tasks binTask = do
    let qn@(A.GName m n) = rootActor binTask
        mn = A.mname qn
        outbase = outBase paths mn
        rootFile = if (isTest binTask) then outbase ++ ".test_root.c" else outbase ++ ".root.c"
    -- In --only-build mode, reuse existing root stubs; generate only if missing.
    existing <- if C.only_build opts then doesFileExist rootFile else return False
    if C.only_build opts && existing
      then return (Just binTask)
      else do
        -- Read the up-to-date roots from the on-disk .ty header (post-compile)
        -- Avoid using preloaded TyTask roots, which may be stale if the module
        -- was rebuilt during this run.
        tyPath <- Acton.Env.findTyFile (searchPath paths) m
        rootsHeader <- case tyPath of
                         Just ty -> do (_, _, _implH, _imps, _nameHashes, roots, _tests, _) <- InterfaceFiles.readHeader ty; return roots
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

-- | Check whether a target triple refers to Windows.
isWindowsOS :: String -> Bool
isWindowsOS targetTriple = case splitOn "-" targetTriple of
    (_:os:_) -> os == "windows"
    _        -> False

-- | Run a process and capture output, canceling on exceptions.
readProcessWithExitCodeCancelable :: CreateProcess -> (ProcessHandle -> IO ()) -> IO (ExitCode, String, String)
readProcessWithExitCodeCancelable cp onStart = do
    let cp' = cp { std_in = NoStream, std_out = CreatePipe, std_err = CreatePipe }
    withCreateProcess cp' $ \_ mOut mErr ph -> do
      onStart ph
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

runZig gopts opts zigExe zigArgs paths wd mProgressUI = do
    let display = showCommandForUser zigExe zigArgs
    iff (C.ccmd opts || C.verbose gopts) $ putStrLn ("zigCmd: " ++ display)
    env0 <- System.Environment.getEnvironment
    let ignoreIO :: IOException -> IO ()
        ignoreIO _ = return ()
        withLock = case mProgressUI of
          Just ui -> withProgressLock ui
          Nothing -> \action -> action
        mTermProgress = do
          ui <- mProgressUI
          let tp = puTermProgress ui
          if termProgressEnabled tp then Just tp else Nothing
    (envOverride, onStart, onStop, closeFds) <- case mTermProgress of
      Nothing -> return (Nothing, \_ -> return (), return (), True)
      Just tp -> do
        (readFd, writeFd) <- createPipe
        setFdOption writeFd CloseOnExec False
        setFdOption readFd CloseOnExec True
        doneVar <- newEmptyMVar
        pctRef <- newIORef (85 :: Int)
        let updatePercent ratio = do
              let clamped = max 0 (min 0.999 ratio)
                  pctRaw = 85 + floor (clamped * 15)
              pct <- atomicModifyIORef' pctRef (\prev -> let next = max prev pctRaw in (next, next))
              withLock (termProgressPercent tp pct)
            reader = do
              readZigProgressStream readFd $ \msg ->
                case zigProgressRatio msg of
                  Nothing -> return ()
                  Just ratio -> updatePercent ratio
        _ <- forkIO (reader `finally` putMVar doneVar ())
        let envVal = show (fromIntegral writeFd :: Int)
            onStart' _ = closeFd writeFd `catch` ignoreIO
            onStop' = do
              closeFd writeFd `catch` ignoreIO
              closeFd readFd `catch` ignoreIO
              takeMVar doneVar
        return (Just ("ZIG_PROGRESS", envVal), onStart', onStop', False)
    let env1 = case envOverride of
          Nothing -> Nothing
          Just (k, v) -> Just ((k, v) : filter ((/= k) . fst) env0)
        cpBase = (proc zigExe zigArgs){ cwd = wd, env = env1 }
        cp = if closeFds then cpBase else cpBase { close_fds = False }
    (returnCode, zigStdout, zigStderr) <- readProcessWithExitCodeCancelable cp onStart `finally` onStop
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

generateFingerprint :: String -> IO String
generateFingerprint name = do
    ident <- randomRIO (1, 0xfffffffe :: Word32)
    let prefix = Fingerprint.fingerprintPrefixForName name
        fp = (fromIntegral prefix `shiftL` 32) .|. fromIntegral ident
    return (Fingerprint.formatFingerprint fp)

-- Render build.zig and build.zig.zon from templates and BuildSpec
-- rootPins: dependency pins from the main project (applied to all deps, including transitive)
genBuildZigFiles :: BuildSpec.BuildSpec -> M.Map String BuildSpec.PkgDep -> [(String, FilePath)] -> Paths -> IO ()
genBuildZigFiles spec rootPins depOverrides paths = do
    let proj = projPath paths
    projAbs <- canonicalizePath proj
    let sys              = sysPath paths
        buildZigPath     = joinPath [proj, "build.zig"]
        buildZonPath     = joinPath [proj, "build.zig.zon"]
        distBuildZigPath = joinPath [sys, "builder", "build.zig"]
        distBuildZonPath = joinPath [sys, "builder", "build.zig.zon"]
    buildZigTemplate <- readFile distBuildZigPath
    buildZonTemplate <- readFile distBuildZonPath
    let zonName = BuildSpec.specName spec
        fp = BuildSpec.fingerprint spec
    (transPkgs, transZigs) <- collectDepsRecursive spec proj rootPins depOverrides
    absSys <- canonicalizePath sys
    let relSys = relativeViaRoot projAbs absSys
    homeDir <- getHomeDirectory
    depsRootAbs <- normalizePathSafe (joinPath [homeDir, ".cache", "acton", "deps"])
    normalizedSpec <- normalizeSpecPaths proj spec
    let applyPins deps = M.mapWithKey (\n d -> M.findWithDefault d n rootPins) deps
        mergedSpec = normalizedSpec { BuildSpec.dependencies     = applyPins (BuildSpec.dependencies normalizedSpec) `M.union` transPkgs
                                    , BuildSpec.zig_dependencies = BuildSpec.zig_dependencies normalizedSpec `M.union` transZigs }
        zonWithFp = replace "{{fingerprint}}" fp . replace "{{name}}" zonName
    writeFile buildZigPath (genBuildZig buildZigTemplate mergedSpec)
    writeFileAtomic buildZonPath (genBuildZigZon buildZonTemplate relSys depsRootAbs projAbs fp zonName mergedSpec)

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
             ++ (if sline == "// Dependencies from Build.act" then [depsAll] else [])
             ++ (if sline == "// lib: link with dependencies / get headers from Build.act" then [libLinks] else [])
             ++ (if sline == "// exe: link with dependencies / get headers from Build.act" then [exeLinks] else [])
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

genBuildZigZon :: String -> String -> FilePath -> FilePath -> String -> String -> BuildSpec.BuildSpec -> String
genBuildZigZon template relSys depsRootAbs projAbs fingerprint zonName spec =
    let pkgDeps = concatMap (pkgToZon projAbs depsRootAbs) (M.toList (BuildSpec.dependencies spec))
        zigDeps = concatMap zigToZon (M.toList (BuildSpec.zig_dependencies spec))
        deps = pkgDeps ++ zigDeps
        replaced = map (replace "{{fingerprint}}" fingerprint
                     . replace "{{syspath}}" relSys
                     . replace "{{name}}" zonName) (lines template)
        header = [ "// AUTOMATICALLY GENERATED BY ACTON BUILD SYSTEM"
                 , "// DO NOT EDIT, CHANGES WILL BE OVERWRITTEN!!!!!"
                 , ""
                 ]
        inject line =
          let sline = dropWhile (== ' ') line
          in [line] ++ (if sline == "// Dependencies from Build.act" then [deps] else [])
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

#if defined(darwin_HOST_OS) && defined(aarch64_HOST_ARCH)
defCpuFlag = ["-Dcpu=apple_m1"]
#elif defined(aarch64_HOST_ARCH)
defCpuFlag = ["-Dcpu=generic+crypto"]
#elif defined(x86_64_HOST_ARCH)
defCpuFlag = ["-Dcpu=x86_64_v2+aes"]
#else
#error "Unsupported platform"
#endif

-- | Run zig build for generated artifacts and prune stale outputs.
zigBuild :: Acton.Env.Env0 -> C.GlobalOptions -> C.CompileOptions -> Paths -> BuildSpec.BuildSpec -> [CompileTask] -> [BinTask] -> Bool -> Maybe ProgressUI -> IO TimeSpec
zigBuild env gopts opts paths rootSpec tasks binTasks allowPrune mProgressUI = do
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

    timeStart <- getTime Monotonic

    homeDir <- getHomeDirectory
    let local_cache_dir = joinPath [ homeDir, ".cache", "acton", "zig-local-cache" ]
        global_cache_dir = joinPath [ homeDir, ".cache", "acton", "zig-global-cache" ]
        no_threads = if isWindowsOS (C.target opts) then True else C.no_threads opts
    projAbs <- normalizePathSafe (projPath paths)
    sysAbs  <- normalizePathSafe (sysPath paths)
    depOverrides <- normalizeDepOverrides (projPath paths) (C.dep_overrides opts)
    let sysRoot   = addTrailingPathSeparator sysAbs
        isSysProj = projAbs == sysAbs || sysRoot `isPrefixOf` projAbs

    -- Generate build.zig and build.zig.zon directly from Build.act.
    iff (not isSysProj) $ do
      let pins = BuildSpec.dependencies rootSpec
      genBuildZigFiles rootSpec pins depOverrides paths

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
                   ("aarch64":"macos":_)   -> ["-Dcpu=apple_m1"]
                   ("aarch64":_)           -> ["-Dcpu=generic+crypto"]
                   ("x86_64":_)            -> ["-Dcpu=x86_64_v2+aes"]
                   (_:_)                   -> defCpuFlag
                   []                      -> defCpuFlag
        optArgs = ["-Doptimize=" ++ optimizeModeToZig (C.optimize opts)]
        featureArgs = concat [ if C.db opts then ["-Ddb"] else []
                             , if no_threads then ["-Dno_threads"] else []
                             , if C.cpedantic opts then ["-Dcpedantic"] else []
                             ]
        zigArgs = baseArgs ++ prefixArgs ++ targetArgs ++ cpuArgs ++ optArgs ++ featureArgs

    runZig gopts opts zigExe zigArgs paths (Just (projPath paths)) mProgressUI
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
    return (timeEnd - timeStart)

-- Remove executables that no longer have corresponding root actors
-- | Remove binaries that no longer have corresponding roots.
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

-- | Compute a relative path when possible, otherwise a safe absolute form.
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

-- | Compute a path relative to the project root with drive handling.
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

-- | Walk BuildSpec dependencies to collect transitive packages and zig deps.
collectDepsRecursive :: BuildSpec.BuildSpec -> FilePath -> M.Map String BuildSpec.PkgDep -> [(String, FilePath)] -> IO (M.Map String BuildSpec.PkgDep, M.Map String BuildSpec.ZigDep)
collectDepsRecursive rootSpec projDir pins overrides = do
  root <- normalizePathSafe projDir
  (\(_, pkgs, zigs) -> (pkgs, zigs)) <$> go root Data.Set.empty root (Just rootSpec)
  where
    go root seen dir mSpec = do
      spec0 <- case mSpec of
                 Just s -> return s
                 Nothing -> loadBuildSpec dir
      spec <- applyDepOverrides dir overrides spec0
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

-- | Normalize dependency paths in a BuildSpec.
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

-- | Normalize a zig dependency path entry.
normalizeZigDep :: FilePath -> BuildSpec.ZigDep -> IO BuildSpec.ZigDep
normalizeZigDep b dep =
  case BuildSpec.zpath dep of
    Just p | not (null p) -> do
      if isAbsolutePath p
        then do p' <- normalizePathSafe p
                return dep { BuildSpec.zpath = Just p' }
        else return dep { BuildSpec.zpath = Just (collapseDots (normalise p)) }
    _ -> return dep

-- | Filter bin tasks to those with a valid root actor.
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
          Right (_, _, _implH, _imps, _nameHashes, roots, _tests, _) | n `elem` roots -> return (Just binTask)
          _ -> checkEnv
      Nothing -> checkEnv

-- | Check whether git is available on PATH.
isGitAvailable :: IO Bool
isGitAvailable = do
    (exitCode, _, _) <- readProcessWithExitCode "git" ["--version"] ""
    return $ exitCode == ExitSuccess

-- | Decide whether to emit colored diagnostics.
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
  , puTermProgress :: TermProgress
  , puLock :: MVar ()
  }

data ProgressState = ProgressState
  { psActive :: IORef (M.Map TaskKey ProgressTask)
  , psOrder :: IORef [TaskKey]
  }

data ProgressTask = ProgressTask
  { ptLine :: String
  , ptStart :: TimeSpec
  }

-- | Initialize terminal progress UI state and enablement.
initProgressUI :: C.GlobalOptions -> Int -> IO ProgressUI
initProgressUI gopts maxLines = do
    tty <- hIsTerminalDevice stdout
    let enabled = (tty || C.tty gopts) && not (C.quiet gopts)
    linesRef <- newIORef []
    visibleRef <- newIORef 0
    spinnerRef <- newIORef 0
    cursorHiddenRef <- newIORef False
    tickerRef <- newIORef Nothing
    termProgress <- initTermProgress gopts
    lock <- newMVar ()
    return ProgressUI
      { puEnabled = enabled
      , puMaxLines = max 1 maxLines
      , puLinesRef = linesRef
      , puVisibleRef = visibleRef
      , puSpinnerRef = spinnerRef
      , puCursorHiddenRef = cursorHiddenRef
      , puTickerRef = tickerRef
      , puTermProgress = termProgress
      , puLock = lock
      }

-- | Initialize tracking for active progress tasks.
newProgressState :: IO ProgressState
newProgressState = do
    active <- newIORef M.empty
    order <- newIORef []
    return ProgressState { psActive = active, psOrder = order }

-- | Serialize progress UI updates when rendering is enabled.
withProgressLock :: ProgressUI -> IO a -> IO a
withProgressLock ui action =
    if not (puEnabled ui)
      then action
      else withMVar (puLock ui) (\_ -> action)

-- | Clear the progress area, run a log action, then redraw.
progressWithLog :: ProgressUI -> IO () -> IO ()
progressWithLog ui action = withProgressLock ui $ do
    if not (puEnabled ui)
      then action
      else do
        progressClearUnlocked ui
        action
        progressRenderUnlocked ui

-- | Log a line while preserving the progress display.
progressLogLine :: ProgressUI -> String -> IO ()
progressLogLine ui msg = progressWithLog ui (putStrLn msg)

-- | Clear the progress area from the terminal.
progressClearUnlocked :: ProgressUI -> IO ()
progressClearUnlocked ui = do
    visible <- readIORef (puVisibleRef ui)
    when (visible > 0) $ do
      replicateM_ visible $ do
        putStr "\ESC[1A"
        putStr "\r\ESC[2K"
      hFlush stdout
    writeIORef (puVisibleRef ui) 0

-- | Spinner frames used by the progress UI.
spinnerChars :: [Char]
spinnerChars = "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏"

-- | Spinner update interval in microseconds.
spinnerTickMicros :: Int
spinnerTickMicros = 80000

-- | Hide or show the cursor during progress rendering.
setCursorHidden :: ProgressUI -> Bool -> IO ()
setCursorHidden ui hidden =
    when (puEnabled ui) $ do
      cur <- readIORef (puCursorHiddenRef ui)
      when (cur /= hidden) $ do
        putStr (if hidden then "\ESC[?25l" else "\ESC[?25h")
        writeIORef (puCursorHiddenRef ui) hidden

-- | Render the current progress lines with the spinner.
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

-- | Start the spinner update thread when needed.
startSpinnerTicker :: ProgressUI -> ProgressState -> IO ()
startSpinnerTicker ui st =
    when (puEnabled ui) $ do
      m <- readIORef (puTickerRef ui)
      case m of
        Just _ -> return ()
        Nothing -> do
          tid <- forkIO (spinnerLoop ui st)
          writeIORef (puTickerRef ui) (Just tid)

-- | Stop the spinner update thread.
stopSpinnerTicker :: ProgressUI -> IO ()
stopSpinnerTicker ui = do
    m <- atomicModifyIORef' (puTickerRef ui) (\cur -> (Nothing, cur))
    forM_ m killThread

-- | Spinner loop that ticks and redraws at a fixed cadence.
spinnerLoop :: ProgressUI -> ProgressState -> IO ()
spinnerLoop ui st = do
    tid <- myThreadId
    let loop = do
          threadDelay spinnerTickMicros
          keep <- withProgressLock ui $ do
            active <- readIORef (psActive st)
            if M.null active || not (puEnabled ui)
              then return False
              else do
                modifyIORef' (puSpinnerRef ui) (+ 1)
                progressRefreshUnlocked ui st
                return True
          when keep loop
    loop
    atomicModifyIORef' (puTickerRef ui) $ \cur ->
      if cur == Just tid then (Nothing, ()) else (cur, ())

-- | Replace progress lines and redraw the UI.
progressSetLines :: ProgressUI -> ProgressState -> [String] -> IO ()
progressSetLines ui st lines = withProgressLock ui $ do
    when (puEnabled ui) $ do
      writeIORef (puLinesRef ui) (take (puMaxLines ui) lines)
      if null lines then stopSpinnerTicker ui else startSpinnerTicker ui st
      progressRenderUnlocked ui

-- | Clear active task state and remove any rendered progress lines.
progressReset :: ProgressUI -> ProgressState -> IO ()
progressReset ui st = withProgressLock ui $ do
    writeIORef (psActive st) M.empty
    writeIORef (psOrder st) []
    writeIORef (puLinesRef ui) []
    termProgressClear (puTermProgress ui)
    when (puEnabled ui) $ do
      stopSpinnerTicker ui
      progressRenderUnlocked ui

-- | Mark a task as active in the progress UI.
progressStartTask :: ProgressUI -> ProgressState -> TaskKey -> String -> IO ()
progressStartTask ui st key line = withProgressLock ui $ do
    now <- getTime Monotonic
    modifyIORef' (psActive st) (M.insert key (ProgressTask line now))
    modifyIORef' (psOrder st) (\xs -> if key `elem` xs then xs else xs ++ [key])
    progressRefreshUnlocked ui st

-- | Remove a task from the progress UI.
progressDoneTask :: ProgressUI -> ProgressState -> TaskKey -> IO ()
progressDoneTask ui st key = withProgressLock ui $ do
    modifyIORef' (psActive st) (M.delete key)
    modifyIORef' (psOrder st) (filter (/= key))
    progressRefreshUnlocked ui st

-- | Recompute and render progress lines from active tasks.
progressRefreshUnlocked :: ProgressUI -> ProgressState -> IO ()
progressRefreshUnlocked ui st = do
    active <- readIORef (psActive st)
    order <- readIORef (psOrder st)
    now <- getTime Monotonic
    let formatLine task =
          ptLine task ++ fmtTime (diffTimeSpec now (ptStart task))
        lines = [ formatLine task | key <- order, Just task <- [M.lookup key active] ]
    when (puEnabled ui) $ do
      writeIORef (puLinesRef ui) (take (puMaxLines ui) lines)
      if null lines then stopSpinnerTicker ui else startSpinnerTicker ui st
      progressRenderUnlocked ui
      termProgressHeartbeat (puTermProgress ui)

-- | Format project labels relative to the root for logs.
projectLabel :: FilePath -> FilePath -> String
projectLabel root proj =
    let rel = makeRelative root proj
    in if rel == "." || ".." `isPrefixOf` rel
         then takeFileName proj
         else rel
