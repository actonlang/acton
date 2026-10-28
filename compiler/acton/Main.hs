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

import qualified Acton.Syntax as A
import Acton.Syntax (modNameToString)
import qualified Acton.NameInfo as I
import qualified Acton.Names
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
import qualified Acton.Zon as Zon
import Acton.Compile
import Acton.Fetch
import Acton.Graph (closure)
import Acton.Project
import Utils
import  Pretty
import qualified InterfaceFiles
import qualified PkgCommands
import qualified Repl
import FileUtil (readFile, writeFile, writeFileIfChanged)

import Control.Concurrent.MVar
import Control.Exception (throw,catch,finally,IOException,try,SomeException,onException,evaluate,bracket,mask)
import qualified Control.Concurrent.Async as Async
import ProcessUtil (stopProcessGroup)
import PerfScaling (validateScalingOptions, printScaleRecording, readScaleRecording)
import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay, throwTo)
import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import Control.Monad
import Data.Bits
import Data.Default.Class (def)
import Data.List.Split
import Data.IORef
import Data.Maybe (catMaybes, isJust, isNothing, maybeToList)
import Data.Monoid ((<>))
import Data.Word (Word32)
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
import System.IO.Temp (writeSystemTempFile)
import Text.PrettyPrint (renderStyle, style, Style(..), Mode(PageMode))
import Text.Show.Pretty (ppDoc)
import System.IO.Unsafe (unsafePerformIO)
import System.Info
import System.Posix.Files
import System.Posix.IO (createPipe, setFdOption, closeFd, FdOption(..))
import System.Posix.Resource
import System.Posix.Signals (installHandler, sigTERM, Handler(CatchOnce))
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
import TerminalSize
import ZigProgress

raiseOpenFileLimit :: IO ()
raiseOpenFileLimit =
    void (try go :: IO (Either IOException ()))
  where
    go = do
        lim <- getResourceLimit ResourceOpenFiles
#if defined(darwin_HOST_OS)
        case (softLimit lim, hardLimit lim) of
          (ResourceLimit soft, ResourceLimit hard) -> raise lim soft (min hard 10240)
          (ResourceLimit soft, ResourceLimitInfinity) -> raise lim soft 10240
          _ -> return ()
#else
        setResourceLimit ResourceOpenFiles lim{ softLimit = hardLimit lim }
#endif

    raise lim soft target =
        when (soft < target) $
            setResourceLimit ResourceOpenFiles lim{ softLimit = ResourceLimit target }

main = do
    raiseOpenFileLimit
    hSetBuffering stdout LineBuffering
    arg <- C.parseCmdLine
    let gopts = case arg of
          C.CmdOpt g _ -> g
          C.CompileOpt _ g _ -> g
    ensureCapabilities gopts
    let replHooks = Repl.Hooks
          { Repl.hooksCacheDir = actonCacheDir
          , Repl.hooksWithPersistentScratchDirLock = withPersistentScratchDirLock
          , Repl.hooksCompileFiles = compileReplHook
          }
    let run = case arg of
          C.CmdOpt gopts (C.New opts)         -> createProject (C.file opts)
          C.CmdOpt gopts (C.Build bopts)      ->
              let opts = C.buildCompile bopts
                  files = C.buildFiles bopts
              in if null files
                 then buildProject gopts opts
                 else buildFiles gopts opts files
          C.CmdOpt gopts (C.Install opts)      -> PkgCommands.installCommand gopts opts
          C.CmdOpt gopts (C.Uninstall opts)    -> PkgCommands.uninstallCommand gopts opts
          C.CmdOpt gopts (C.Sig opts)          -> sigCommand gopts opts
          C.CmdOpt gopts (C.Test tcmd)        -> runTests gopts tcmd
          C.CmdOpt gopts (C.Repl opts)        -> Repl.runRepl replHooks gopts opts
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
      ".tydb" -> printDocs gopts (C.DocOptions fname (Just C.AsciiFormat) Nothing)
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
optimizeModeToZig C.Debug        = "debug"
optimizeModeToZig C.ReleaseSafe  = "safe"
optimizeModeToZig C.ReleaseSmall = "small"
optimizeModeToZig C.ReleaseFast  = "fast"


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

-- | Format a TimeSpec as seconds with millisecond precision.
fmtTimePrecise :: TimeSpec -> String
fmtTimePrecise t =
    printf "%6.3f s" secs
  where
    secs :: Float
    secs = (fromIntegral (sec t)) + (fromIntegral (nsec t) / 1000000000)

-- | Report elapsed wall time; parallel compiler/build steps can overlap.
logTiming :: C.GlobalOptions -> C.CompileOptions -> (String -> IO ()) -> String -> TimeSpec -> IO ()
logTiming gopts opts logLine label start =
    when (C.timing gopts && not (quiet gopts opts)) $ do
      end <- getTime Monotonic
      logLine ("Timing: " ++ label ++ " " ++ fmtTimePrecise (diffTimeSpec end start))

-- | Format a TimeSpec with second granularity for compact narrow logs.
fmtTimeCompact :: TimeSpec -> String
fmtTimeCompact t =
    if wholeSecs <= 0 && remNanos > 0
      then "<1s"
      else show roundedSecs ++ "s"
  where
    wholeSecs = fromIntegral (sec t) :: Integer
    remNanos = nsec t
    roundedSecs =
      wholeSecs + if remNanos >= 500000000 then 1 else 0

timeMinWidth :: Int
timeMinWidth = length "99999.999 s"

-- | Pad a string with trailing spaces for aligned output.
padRight :: Int -> String -> String
padRight width s
    | len >= width' = s
    | otherwise = s ++ replicate (width' - len) ' '
  where
    width' = max 0 width
    len = length s

-- | Pad a string with leading spaces for aligned output.
padLeft :: Int -> String -> String
padLeft width s
    | len >= width' = s
    | otherwise = replicate (width' - len) ' ' ++ s
  where
    width' = max 0 width
    len = length s

progressSpinnerThreshold :: Int
progressSpinnerThreshold = 25

progressPrefixWidth :: Int -> Int
progressPrefixWidth cols
    | cols >= progressSpinnerThreshold = 3
    | otherwise = 0

-- | Truncate a string on the right with an ellipsis when it does not fit.
abbreviateRight :: Int -> String -> String
abbreviateRight width s
    | width <= 0 = ""
    | length s <= width = s
    | width <= 3 = take width s
    | otherwise = take (width - 3) s ++ "..."

-- | Compress a module label to preserve the root and as much of the tail as fits.
fitBuildModuleLabel :: Int -> String -> String
fitBuildModuleLabel width label
    | width <= 0 = ""
    | length label <= width = label
    | width < 10 = termFitPlainLeft width label
    | otherwise =
        case break (== '.') label of
          (root, '.':rest) ->
            let rootBudget = min (length root) (max 1 (width - 4))
                tailBudget = max 0 (width - rootBudget - 3)
            in if tailBudget <= 0
                 then termFitPlainLeft width label
                 else take rootBudget root ++ "..." ++ termFitPlainLeft tailBudget rest
          _ -> abbreviateRight width label

type StatusRenderer = Int -> Maybe String

staticStatusRenderer :: String -> String -> StatusRenderer
staticStatusRenderer full short budget
    | budget < 10 = Nothing
    | length full <= budget = Just full
    | length short <= budget = Just short
    | otherwise = Just (abbreviateRight budget full)

data BuildLineLayout = BuildLineLayout
  { bllText :: String
  , bllAligned :: Bool
  , bllLabelCols :: Int
  , bllHasStatus :: Bool
  }

emptyBuildLineLayout :: BuildLineLayout
emptyBuildLineLayout = BuildLineLayout "" False 0 False

-- | Fit a build line body to width while preserving module/status columns when possible.
fitBuildLineLayout :: Int -> Int -> Int -> Bool -> String -> StatusRenderer -> BuildLineLayout
fitBuildLineLayout width labelWidth statusWidth preserveBlankLabel modLbl renderStatus
    | width <= 0 = emptyBuildLineLayout
    | hasLabelColumn =
        case renderAligned of
          Just line -> line
          Nothing ->
            if null modLbl
              then fitStatusOnly
              else BuildLineLayout (fitBuildModuleLabel width modLbl) False (min width labelWidth) False
    | otherwise = fitStatusOnly
  where
    minStatusWidth = 10
    minLabelWidth = 10
    hasLabelColumn = preserveBlankLabel || not (null modLbl)
    renderAligned
      | width < 2 + minStatusWidth = Nothing
      | labelCols <= 0 && not (null modLbl) = Nothing
      | not (null modLbl) && labelCols < min minLabelWidth (length modLbl) = Nothing
      | statusCols < minStatusWidth = Nothing
      | otherwise =
          case renderStatus statusCols of
            Just status ->
              let labelText = if null modLbl then "" else fitBuildModuleLabel labelCols modLbl
                  core =
                    padRight labelCols labelText
                    ++ "  "
                    ++ padRight statusCols status
              in Just (BuildLineLayout core True labelCols True)
            Nothing -> Nothing
    labelCols = min labelWidth (max 0 (width - 2 - minStatusWidth))
    statusCols = min statusWidth (max 0 (width - labelCols - 2))
    fitStatusOnly =
      case renderStatus (min statusWidth width) of
        Just status -> BuildLineLayout (termFitPlainRight width status) False 0 True
        Nothing -> emptyBuildLineLayout

-- | Fit an active build line to the current terminal width.
fitBuildActiveLineAligned :: Int -> Int -> Int -> String -> StatusRenderer -> String
fitBuildActiveLineAligned width labelWidth statusWidth modLbl renderStatus =
    bllText (fitBuildLineLayout width labelWidth statusWidth False modLbl renderStatus)

safeLiveWidth :: Int -> Int
safeLiveWidth width
    | width <= 0 = 0
    | width == 1 = 1
    | otherwise = width - 1

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
        srcRoot = projectSrcDir projDir
        buildActPath = projectBuildAct projDir
        buildSpec = BuildSpec.BuildSpec
          { BuildSpec.specName = name
          , BuildSpec.specDescription = Nothing
          , BuildSpec.fingerprint = fp
          , BuildSpec.dependencies = M.empty
          , BuildSpec.zig_dependencies = M.empty
          , BuildSpec.build_options = M.empty
          , BuildSpec.libraries = M.empty
          }
    createDirectoryIfMissing True srcRoot
    writeFile buildActPath (BuildSpec.renderBuildAct buildSpec)
    (paths, _) <- findPaths buildActPath C.defaultCompileOptions
    writeFile (joinPath [ projDir, ".gitignore" ]) (
      ".acton.compile.lock\n" ++
      ".acton.lock\n" ++
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
        mapM_ requireSourceFile (filter isActFile files)
        absFiles <- mapM canonicalizePath files
        let onlyAct = all isActFile files
        projDirs <- mapM findProjectDir absFiles
        let projRoots = nub (catMaybes projDirs)
        case projRoots of
          [proj] | onlyAct && all (== Just proj) projDirs -> do
            let sp = Source.diskSourceProvider
                runBuild opts' =
                  withProjectLockNotice gopts proj $
                    compileFiles sp gopts opts' absFiles SelectedFiles
            runBuild opts
          _ -> mapM_ (runFile gopts opts) files

-- | Collect all source files in a project's src/ directory.
projectSourceFiles :: Paths -> IO [FilePath]
projectSourceFiles paths = do
    allFiles <- getFilesRecursive (srcDir paths)
    return (filter isActFile allFiles)

withBackgroundCompilerLockOrExit :: FilePath -> String -> IO a -> IO a
withBackgroundCompilerLockOrExit projDir msg =
    withBackgroundCompilerLock projDir (printErrorAndExit msg)

withProjectLockNotice :: C.GlobalOptions -> FilePath -> IO a -> IO a
withProjectLockNotice gopts projDir action =
    withProjectLockOnWait projDir onWait action
  where
    onWait =
      unless (C.quiet gopts) $
        putStrLn ("Waiting for compiler lock in " ++ projDir)

-- | Run the action under the project lock if the generation is current
-- both before and after waiting for the lock: the action's result, or
-- False when the generation went stale first.
withProjectLockForGen :: C.GlobalOptions -> Generation -> FilePath -> IO Bool -> IO Bool
withProjectLockForGen gopts gen projDir action =
    ifCurrent gen False (withProjectLockNotice gopts projDir (ifCurrent gen False action))

-- | Require a project in the given directory: exit with an error unless it
-- holds Build.act and src/, else return the Build.act path.
requireProjectAt :: FilePath -> IO FilePath
requireProjectAt curDir = do
    actPath <- requireProjectConfigPath curDir
    let projDir = takeDirectory actPath
    srcExists <- doesDirectoryExist (projectSrcDir projDir)
    unless srcExists $
      printErrorAndExit "Missing src/ directory"
    return actPath

-- | Strict path resolution: require a project config in the given directory.
loadProjectPathsAt :: FilePath -> C.CompileOptions -> IO Paths
loadProjectPathsAt curDir opts = do
    actPath <- requireProjectAt curDir
    fst <$> findPaths actPath opts

loadProjectPaths :: C.CompileOptions -> IO Paths
loadProjectPaths opts = do
    curDir <- getCurrentDirectory
    loadProjectPathsAt curDir opts

requireProjectConfigPath :: FilePath -> IO FilePath
requireProjectConfigPath curDir = do
    let candidates =
          [ projectBuildAct curDir
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

actonCacheDir :: IO FilePath
actonCacheDir = do
    home <- getHomeDirectory
    let basePath = actonCacheRoot home
    createDirectoryIfMissing True basePath
    return basePath

withScratchDirLock :: (FilePath -> IO a) -> IO a
withScratchDirLock action = do
    cacheDir <- actonCacheDir
    let basePath = cacheDir </> "scratch"
    createDirectoryIfMissing True basePath
    maybeLockInfo <- findAvailableScratch basePath
    case maybeLockInfo of
      Nothing -> error "Could not acquire any scratch directory lock"
      Just (lock, lockPath) -> do
        let scratchDir = dropExtension lockPath
        removeDirectoryRecursive scratchDir `catch` ignoreNotExists
        action scratchDir `finally` unlockFile lock

withPersistentScratchDirLock :: FilePath -> (FilePath -> IO a) -> IO a
withPersistentScratchDirLock basePath action = do
    createDirectoryIfMissing True basePath
    maybeLockInfo <- findAvailableScratch basePath
    case maybeLockInfo of
      Nothing -> error "Could not acquire any scratch directory lock"
      Just (lock, lockPath) -> do
        let scratchDir = dropExtension lockPath
        createDirectoryIfMissing True scratchDir
        action scratchDir `finally` unlockFile lock

withTempDirOpts :: C.CompileOptions -> (C.CompileOptions -> Bool -> IO a) -> IO a
withTempDirOpts opts action
  | C.tempdir opts /= "" = action opts False
  | otherwise = withScratchDirLock $ \scratchDir ->
      action opts { C.tempdir = scratchDir } True

data CliWatchContext = CliWatchContext
  { cwScheduler :: CompileScheduler
  , cwProgressUI :: ProgressUI
  , cwProgressState :: ProgressState
  }

withCompileWatchContext :: C.GlobalOptions -> (CliWatchContext -> IO a) -> IO a
withCompileWatchContext gopts action = do
    owner <- myThreadId
    bracket (installHandler sigTERM (CatchOnce (throwTo owner (ExitFailure 143))) Nothing)
            (\previous -> void (installHandler sigTERM previous Nothing)) $ \_ ->
      bracket create cleanup action
  where
    create = CliWatchContext <$> newCompileScheduler gopts <*> initProgressUI gopts <*> newProgressState
    cleanup ctx = do
      stopCompile (cwScheduler ctx)
      progressReset (cwProgressUI ctx) (cwProgressState ctx)

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
                    runBuild opts' = withProjectLockNotice gopts projDir $ do
                      srcFiles <- projectSourceFiles paths
                      compileFiles sp gopts opts' srcFiles WholeProject
                      generateProjectDocIndex sp gopts opts' paths srcFiles
                runBuild opts

-- Test runner -------------------------------------------------------------------------------------------------

-- | Entry point for acton test; configures options and selects mode/watch.
runTests :: C.GlobalOptions -> C.TestCommand -> IO ()
runTests gopts (C.TestScaleReport axes path baseline) = do
    color <- useColor gopts
    printScaleRecording color axes path baseline
runTests gopts cmd = do
    let (mode, topts) =
          case cmd of
            C.TestRun opts  -> (TestModeRun, opts)
            C.TestList opts -> (TestModeList, opts)
            C.TestPerf opts -> (TestModePerf, opts)
            C.TestScale opts -> (TestModeScale, opts)
            C.TestStress opts -> (TestModeStress, opts)
        gopts' = if C.testJson topts then gopts { C.quiet = True } else gopts
        opts0 = C.testCompile topts
    when (mode == TestModeScale) $
      mapM_ printErrorAndExit (validateScalingOptions topts)
    when (mode == TestModePerf && isJust (C.testCompare topts)) $ do
      when (C.testCompare topts == Just "git:") $
        printErrorAndExit "--compare git:REF requires a Git revision"
      when (C.testSnapshotUpdate topts) $
        printErrorAndExit "Performance comparisons cannot update test snapshots"
      when (C.watch opts0) $
        printErrorAndExit "Performance comparisons cannot run with --watch"
    when (C.testRecord topts && mode /= TestModePerf) $
      printErrorAndExit "--record requires acton test perf"
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
runTestsOnce gopts opts topts mode paths
  | mode `elem` [TestModePerf, TestModeScale], Just ref <- C.testCompare topts >>= Data.List.stripPrefix "git:" =
      runGitTestComparison gopts opts topts mode paths ref >>= exitWithTestCode
runTestsOnce gopts opts topts0 mode paths = do
    (topts, baseline) <- if mode == TestModeScale then prepareScalingComparison paths topts0 else return (topts0, Nothing)
    (sourceInfo, modules) <- withProjectLockNotice gopts (projPath paths) $ do
      sourceInfo <- if mode == TestModeScale then readScaleSourceInfo (projPath paths) else return mempty
      modules <- buildTestModules gopts opts paths topts
      return (sourceInfo, modules)
    case mode of
      TestModeList -> listProjectTests opts paths topts modules
      _ -> do
        maxParallel0 <- testMaxParallel gopts
        let maxParallel = if mode `elem` [TestModePerf, TestModeScale, TestModeStress] then 1 else maxParallel0
        useColorOut <- useColor gopts
        testStart <- getTime Monotonic
        exitCode <- if mode == TestModeScale
          then fst <$> runScalingTests useColorOut gopts opts paths topts modules (projOut paths </> "perf_scaling") sourceInfo baseline
          else runProjectTests useColorOut gopts opts paths topts mode modules maxParallel
        logTiming gopts opts putStrLn "test execution and cache" testStart
        exitWithTestCode exitCode

buildTestModules :: C.GlobalOptions -> C.CompileOptions -> Paths -> C.TestOptions -> IO [String]
buildTestModules gopts opts paths topts = do
    srcFiles <- projectSourceFiles paths
    selected <- selectTestSources gopts opts paths topts srcFiles
    unless (null selected) $
      compileFiles Source.diskSourceProvider gopts opts selected (testBuildScope srcFiles selected)
    mapM (fmap modNameToString . moduleNameFromFile (srcDir paths) (projName paths)) selected

-- | A test build that selects every project source builds the whole project.
testBuildScope :: [FilePath] -> [FilePath] -> Scope
testBuildScope srcFiles selected
  | selected == srcFiles = WholeProject
  | otherwise            = SelectedFiles

-- Build both revisions before timing. Each mode then owns its sampling policy.
runGitTestComparison :: C.GlobalOptions -> C.CompileOptions -> C.TestOptions -> TestMode -> Paths -> String -> IO Int
runGitTestComparison gopts opts topts mode paths ref = do
    search <- mapM canonicalizePath (C.searchpath opts)
    overrides <- normalizeDepOverrides (projPath paths) (C.dep_overrides opts)
    color <- useColor gopts
    cache <- actonCacheDir
    let announce = unless (C.quiet gopts || C.testJson topts) . putStrLn
    withPerfWorktree cache (projPath paths) ref $ \root baseline -> do
      oldSource <- readScaleSourceInfo baseline
      let oldProject = perfWorktreePath root baseline (projPath paths)
          buildAt checkout = do
            let rebase = perfWorktreePath root checkout
                project = rebase (projPath paths)
                compileOpts = opts { C.syspath = sysPath paths, C.searchpath = map rebase search
                                   , C.dep_overrides = [(name, rebase path) | (name, path) <- overrides] }
                studyOpts = topts { C.testCompile = compileOpts, C.testCompare = Nothing }
            withCurrentDirectory project $ do
              treePaths <- loadProjectPathsAt project compileOpts
              modules <- buildTestModules gopts compileOpts treePaths studyOpts
              names <- selectedTestNames compileOpts treePaths studyOpts modules
              return (treePaths, compileOpts, modules, [(modName, name) | (modName, tests) <- names, name <- tests])
      valid <- (&&) <$> doesFileExist (projectBuildAct oldProject) <*> doesDirectoryExist (projectSrcDir oldProject)
      unless valid $ printErrorAndExit ("The Git snapshot has no Acton project at " ++ makeRelative root (projPath paths))
      announce ("Preparing Git baseline " ++ show ref)
      (oldPaths, oldOpts, oldModules, oldTests) <-
        withProjectLockNotice gopts oldProject $ buildAt baseline
      -- Keep other Acton builds from replacing the current binary while the
      -- baseline is measured. The caller's source and Git index stay in place.
      withProjectLockNotice gopts (projPath paths) $ do
        newSource <- readScaleSourceInfo root
        announce "Preparing current working tree"
        (newPaths, newOpts, newModules, newTests) <- buildAt root
        case (oldTests, newTests) of
          ([old], [new]) | old == new -> return ()
          _ -> printErrorAndExit "Git comparison requires the same single benchmark in both revisions; select it with --module and/or --name"
        if mode == TestModePerf then do
          let (modName, testName) = head newTests
          runPerfComparison color gopts topts (oldOpts, oldPaths, oldSource)
            (newOpts, newPaths, newSource) modName testName
        else do
          directory <- makeAbsolute (projOut paths </> "perf_scaling")
          let oldStudy = topts { C.testCompile = oldOpts, C.testCompare = Nothing }
          announce "Measuring Git baseline (time and memory limits apply to each study)"
          (oldCode, recordings) <- withCurrentDirectory (projPath oldPaths) $
            runScalingTests color gopts oldOpts oldPaths oldStudy oldModules directory oldSource Nothing
          if oldCode /= 0 then return oldCode else case recordings of
            [recording] -> withCurrentDirectory (projPath newPaths) $ do
              let newStudy = topts { C.testCompile = newOpts, C.testCompare = Just recording }
              -- A resource limit may stop the baseline short of the requested end.
              -- Replay its completed sizes, then continue to the requested end.
              (compared, baseline) <- case C.testEndScale topts of
                Just _ -> do
                  baseline <- readScaleRecording recording
                  return (newStudy, Just baseline)
                Nothing -> prepareScalingComparison newPaths newStudy
              announce "Measuring current working tree (replaying baseline sizes first)"
              fst <$> runScalingTests color gopts newOpts newPaths
                compared newModules directory newSource baseline
            _ -> printErrorAndExit "The Git baseline did not produce a scaling recording"

-- | Watch mode for tests that rebuilds incrementally and reruns changed modules.
runTestsWatch :: C.GlobalOptions -> C.CompileOptions -> C.TestOptions -> TestMode -> Paths -> IO ()
runTestsWatch gopts opts topts mode paths = do
    let sp = Source.diskSourceProvider
        projDir = projPath paths
        srcRoot = srcDir paths
    withBackgroundCompilerLockOrExit projDir
      "Another long-running Acton compiler is already running; cannot start test watch." $
      withCompileWatchContext gopts $ \watchCtx -> do
        let sched = cwScheduler watchCtx
            progressUI = cwProgressUI watchCtx
            progressState = cwProgressState watchCtx
        testParallel0 <- testMaxParallel gopts
        let testParallel = if mode `elem` [TestModePerf, TestModeScale, TestModeStress] then 1 else testParallel0
        let runOnce gen mChanged = do
              withProjectLockForGen gopts gen projDir $ do
                logProjectBuild gopts progressUI progressState projDir
                srcFiles <- projectSourceFiles paths
                selected <- selectTestSources gopts opts paths topts srcFiles
                -- Link the complete selected closure; cached modules still skip
                -- unchanged compiler passes.
                hadErrors <- if null selected then return False else
                  compileFilesChanged sp gopts opts selected (testBuildScope srcFiles selected) Nothing (Just (watchCtx, gen))
                unless hadErrors $ whenCurrent gen $ do
                  selectStart <- getTime Monotonic
                  testModules <- mapM (fmap modNameToString . moduleNameFromFile (srcDir paths) (projName paths)) selected
                  modulesToTest <- selectTestModules paths srcFiles mChanged testModules
                  logTiming gopts opts (progressLogLine progressUI) "affected test selection" selectStart
                  unless (null modulesToTest) $ whenCurrent gen $
                    do
                      useColorOut <- useColor gopts
                      testStart <- getTime Monotonic
                      void $ runProjectTests useColorOut gopts opts paths topts mode modulesToTest testParallel
                      logTiming gopts opts (progressLogLine progressUI) "test execution and cache" testStart
                return (not hadErrors)
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
      mn <- moduleNameFromFile (srcDir paths) (projName paths) file
      return (modNameToString mn)
    return (Data.List.sort (nub mods))

readModuleImports :: Paths -> A.ModName -> IO [A.ModName]
readModuleImports paths mn =
    maybe [] (map fst . InterfaceFiles.summaryImports)
      <$> InterfaceFiles.tryReadHeaderSummary (tyDbPath paths mn)

dependentTestModulesFromHeaders :: Paths -> [FilePath] -> [String] -> IO [String]
dependentTestModulesFromHeaders paths srcFiles changedModules = do
    depsByMod <- forM srcFiles $ \file -> do
      mn <- moduleNameFromFile (srcDir paths) (projName paths) file
      imps <- readModuleImports paths mn
      return (modNameToString mn, map modNameToString imps)
    -- importers keeps edges to modules outside the source list: a deleted
    -- module is a changed module whose importers are affected.
    let importers = M.fromListWith (++) [ (dep, [mn]) | (mn, deps) <- depsByMod, dep <- deps ]
        affected = closure (\m -> M.findWithDefault [] m importers) changedModules
    return (Data.List.sort (Data.Set.toList affected))

-- | Watch a project directory and rebuild on source or build spec changes.
watchProjectAt :: C.GlobalOptions -> C.CompileOptions -> FilePath -> IO ()
watchProjectAt gopts opts projDir = do
                let sp = Source.diskSourceProvider
                paths <- loadProjectPathsAt projDir opts
                withBackgroundCompilerLockOrExit (projPath paths)
                  "Another long-running Acton compiler is already running; cannot start watch." $
                  withCompileWatchContext gopts $ \watchCtx -> do
                    let sched = cwScheduler watchCtx
                        progressUI = cwProgressUI watchCtx
                        progressState = cwProgressState watchCtx
                    let runOnce gen mChanged =
                          withProjectLockForGen gopts gen (projPath paths) $ do
                            logProjectBuild gopts progressUI progressState (projPath paths)
                            srcFiles <- projectSourceFiles paths
                            hadErrors <- compileFilesChanged sp gopts opts srcFiles WholeProject mChanged (Just (watchCtx, gen))
                            when (not hadErrors && isNothing mChanged) $
                              generateProjectDocIndex sp gopts opts paths srcFiles
                            return (not hadErrors)
                    runWatchProject gopts (projPath paths) (srcDir paths) sched runOnce

-- | Exit with an error unless a source file named on the command line exists.
requireSourceFile :: FilePath -> IO ()
requireSourceFile file = do
    exists <- doesFileExist file
    unless exists $
      printErrorAndExit ("File does not exist: " ++ file)

-- | Build a single file, optionally running in watch mode.
buildFile :: C.GlobalOptions -> C.CompileOptions -> FilePath -> IO ()
buildFile gopts opts file = do
    requireSourceFile file
    if C.watch opts
      then watchFile gopts opts file
      else buildFileOnce gopts opts file

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
              withProjectLockNotice gopts proj $
                compileFiles sp gopts opts' [file] SelectedFiles
        runBuild opts
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
          compileFiles sp gopts opts' [file] SelectedFiles

-- | Watch a single file and rebuild on changes.
watchFile :: C.GlobalOptions -> C.CompileOptions -> FilePath -> IO ()
watchFile gopts opts file = do
    absFile <- canonicalizePath file
    projDir <- findProjectDir absFile
    case projDir of
      Just proj -> watchProjectAt gopts opts proj
      Nothing -> do
        let sp = Source.diskSourceProvider
        withTempDirOpts opts $ \opts' _ ->
          withCompileWatchContext gopts $ \watchCtx -> do
            let sched = cwScheduler watchCtx
                runOnce gen mChanged =
                  void $ compileFilesChanged sp gopts opts' [absFile] SelectedFiles mChanged (Just (watchCtx, gen))
            runWatchFile gopts absFile sched runOnce

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
        if not (isActFile (FS.eventPath ev))
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
                -> (Generation -> Maybe [FilePath] -> IO Bool)
                -> IO ()
runWatchProject gopts projDir srcRoot sched runOnce = do
    stampsRef <- newIORef M.empty
    pendingRef <- newIORef (0 :: Int, Just [])
    let schedule mpath = do
          atomicModifyIORef' pendingRef $ \(version, pending) ->
            let changed = nub <$> ((++) <$> pending <*> fmap (:[]) mpath)
            in ((version + 1, changed), ())
          void $ startCompile sched 0 $ \gen -> do
            (version, changed) <- readIORef pendingRef
            completed <- runOnce gen changed
            -- Canceled and failed generations leave their paths pending. A
            -- later save must still run tests affected by those earlier edits.
            when completed $ atomicModifyIORef' pendingRef $ \current@(latest, _) ->
              (if latest == version then (latest, Just []) else current, ())
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
             -> (Generation -> Maybe [FilePath] -> IO ())
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
    paths <- loadProjectPaths C.defaultCompileOptions
    res <- try (fetchDependencies gopts paths []) :: IO (Either ProjectError ())
    case res of
      Left (ProjectError msg) -> printErrorAndExit msg
      Right () ->
        unless (C.quiet gopts) $
          putStrLn "Dependencies fetched"

data SigTarget
    = SigSourceTarget ProjCtx A.ModName (Maybe A.Name) FilePath
    | SigTyTarget A.ModName (Maybe A.Name) FilePath

sigCommand :: C.GlobalOptions -> C.SigOptions -> IO ()
sigCommand gopts sigOpts = do
    let opts0 = (C.sigCompile sigOpts)
          { C.skip_build = True
          , C.only_build = False
          , C.sigs = False
          }
        queryGopts = if C.verbose gopts then gopts else gopts { C.quiet = True }
    actPath <- requireProjectAt =<< getCurrentDirectory
    cctx <- prepareCompileContext opts0 [actPath]
    let opts = ccOpts cctx
        paths = ccPathsRoot cctx
        depOverrides = C.dep_overrides opts
        rootProj = ccRootProj cctx
    withProjectLockNotice queryGopts rootProj $ do
      fetchDependencies queryGopts paths depOverrides
      projMap <- discoverProjects queryGopts (ccSysAbs cctx) rootProj depOverrides
      target <- resolveSigTarget opts paths rootProj projMap (C.sigTarget sigOpts)
      tyFile <- case target of
        SigSourceTarget ctx mn _ srcPath ->
          compileSigTarget gopts queryGopts cctx ctx mn srcPath
        SigTyTarget _ _ tyPath ->
          return tyPath
      case target of
        SigSourceTarget _ mn mName _ -> printSigInterface paths mn mName tyFile
        SigTyTarget mn mName _       -> printSigInterface paths mn mName tyFile

compileSigTarget :: C.GlobalOptions
                 -> C.GlobalOptions
                 -> CompileContext
                 -> ProjCtx
                 -> A.ModName
                 -> FilePath
                 -> IO FilePath
compileSigTarget gopts queryGopts cctx targetCtx mn srcPath = do
    let sp = Source.diskSourceProvider
    plan <- planFetched sp queryGopts cctx (PlanRequest [srcPath] SelectedFiles Nothing)
    let onEvent (Diagnostics _ diags) = printDiagnostics gopts diags
        onEvent (Info msg)            = putStrLn msg      -- Info lines arrive only under --verbose
        onEvent _                     = return ()
    compileRes <- runStandalonePlan sp queryGopts plan onEvent
    case compileRes of
      Left err -> printErrorAndExit (compileFailureMessage err)
      Right (_, hadErrors) -> when hadErrors System.Exit.exitFailure
    return (tyDbPath (projectPaths (ccOpts cctx) (cpProjMap plan) targetCtx) mn)

fixupTarget locals deps paths mn
  | mn `elem` locals            = return $ addProjPrefix paths mn
  | n == projName paths         = printErrorAndExit ("Module not found: " ++ prstr mn)
  | null ns && n `elem` deps    = return $ A.modName [n,"lib"]
  | otherwise                   = return mn
  where n:ns                    = A.modPath mn

resolveSigTarget :: C.CompileOptions -> Paths -> FilePath -> M.Map FilePath ProjCtx -> String -> IO SigTarget
resolveSigTarget opts paths rootProj projMap rawTarget = do
    parts <- parseSigTarget rawTarget
    moduleIndex <- sigModuleIndex projMap rootProj
    let fullMod = A.modName parts
        locals = [ dropProjPrefix paths mn | (mn, (ctx,fp)) <- moduleIndex, projRoot ctx == rootProj ]
        deps = [ dep | (mn, (ctx,fp)) <- moduleIndex, projRoot ctx == rootProj, (dep,_) <- projDeps ctx ]
    fullMod' <- fixupTarget locals deps paths fullMod
    case lookup fullMod' moduleIndex of
        Just (ctx, srcPath) ->
            return (SigSourceTarget ctx fullMod' Nothing srcPath)
        Nothing -> do
            mFullTy <- findSigTyFile tySearchPath fullMod'
            case mFullTy of
                Just tyPath ->
                    return (SigTyTarget fullMod' Nothing tyPath)
                Nothing ->
                    resolveNameTarget parts locals deps moduleIndex
  where
    tySearchPath = sigTySearchPath opts paths rootProj projMap

    resolveNameTarget parts locals deps moduleIndex
      | length parts < 2 =
          printErrorAndExit ("Module not found: " ++ rawTarget)
      | otherwise = do
          let modParts = init parts
              namePart = last parts
              mn = A.modName modParts
              n = A.name namePart
          mn' <- fixupTarget locals deps paths mn
          case lookup mn' moduleIndex of
              Just (ctx, srcPath) -> do
                  return (SigSourceTarget ctx mn' (Just n) srcPath)
              Nothing -> do
                  mTy <- findSigTyFile tySearchPath mn'
                  case mTy of
                      Just tyPath ->
                          return (SigTyTarget mn' (Just n) tyPath)
                      Nothing -> do
                          printErrorAndExit ("Module not found: " ++ intercalate "." modParts
                                             ++ " (while resolving " ++ rawTarget ++ ")")

parseSigTarget :: String -> IO [String]
parseSigTarget rawTarget = do
    let parts = splitOn "." rawTarget
    if null rawTarget || any null parts
      then printErrorAndExit ("Invalid signature target: " ++ rawTarget)
      else return parts

sigModuleIndex :: M.Map FilePath ProjCtx -> FilePath -> IO [(A.ModName, (ProjCtx, FilePath))]
sigModuleIndex projMap rootProj = do
    let roots = sigProjectSearchOrder projMap rootProj
    fmap concat $ forM roots $ \root ->
      case M.lookup root projMap of
        Nothing -> return []
        Just ctx -> do
          mods <- enumerateProjectModules ctx
          return [ (mn, (ctx, srcPath)) | (srcPath, mn) <- mods ]

sigProjectSearchOrder :: M.Map FilePath ProjCtx -> FilePath -> [FilePath]
sigProjectSearchOrder projMap rootProj =
    rootProj : maybe [] (map snd . projDeps) (M.lookup rootProj projMap)

sigTySearchPath :: C.CompileOptions -> Paths -> FilePath -> M.Map FilePath ProjCtx -> [FilePath]
sigTySearchPath opts paths rootProj projMap =
    case M.lookup rootProj projMap of
      Just _       -> let ps = [ projectTypesDir root | root <- sigProjectSearchOrder projMap rootProj
                                                            , M.member root projMap ]
                              ++ C.searchpath opts ++ systemTypePaths (sysPath paths)
                      in ps ++ [projectTypesDir rootProj </> projName paths]        -- Add the prefixed type-dir as a last resort in case the source has been deleted
      Nothing      -> searchPath paths

findSigTyFile :: [FilePath] -> A.ModName -> IO (Maybe FilePath)
findSigTyFile = Acton.Env.findTyFile

printSigInterface :: Paths -> A.ModName -> Maybe A.Name -> FilePath -> IO ()
printSigInterface paths mn mName tyFile = do
    exists <- InterfaceFiles.interfaceExists tyFile
    unless exists $
      printErrorAndExit ("Type interface not found for " ++ modNameToString mn)
    tyRes <- (try :: IO a -> IO (Either SomeException a)) $ InterfaceFiles.readModuleIface tyFile
    case tyRes of
      Left err ->
        printErrorAndExit ("Could not read type interface for " ++ modNameToString mn ++ ": " ++ show err)
      Right (I.NModule imps te _) -> do
        env0 <- Acton.Env.initEnv (sysTypes paths) False
        let envImports = foldr Acton.Env.addImport env0 imps
            selected = case mName of
              Nothing -> te
              Just n  -> filter ((== n) . fst) te
            envForPrint = case mName of
              Nothing -> envImports
              Just _  -> define te (setMod mn envImports)
            dropPrefix = dropProjPrefixOrLib paths
        case mName of
          Just n | null selected ->
            printErrorAndExit ("Name not found: " ++ modNameToString mn ++ "." ++ A.nstr n)
          _ ->
            putStrLn (Acton.Types.prettySigs envForPrint mn (map dropPrefix imps) (Acton.Names.nmap dropPrefix selected))

-- Show dependency tree with overrides applied from root pins
pkgShow :: C.GlobalOptions -> IO ()
pkgShow gopts = do
    curDir <- getCurrentDirectory
    _ <- requireProjectConfigPath curDir
    spec <- loadBuildSpec curDir
    unless (C.quiet gopts) $
      putStrLn "Dependency tree (hash overrides shown):"
    partial <- showTree (rootPins curDir spec) curDir spec 0
    when (partial && not (C.quiet gopts)) $
      putStrLn "Note: dependency tree is partial; some dependencies are not fetched yet. Run 'acton fetch'."
  where
    describeDep dep =
      (case BuildSpec.hash dep of
        Just h -> "hash=" ++ h
        Nothing -> case localPath dep of
                     Just p  -> "path=" ++ p
                     Nothing -> "unversioned")
      ++ maybe "" (\dir -> ", subdir=" ++ dir) (BuildSpec.subdir dep)

    showTree pins dir spec depth = do
      let deps = M.toList (BuildSpec.dependencies spec)
      foldM (step pins dir depth) False deps

    step pins dir depth partial (depName, dep)
      | Just ref <- BuildSpec.follows dep = do
          putStrLn (replicate (2*depth) ' ' ++ "- " ++ depName ++ " (follows=" ++ ref ++ ")")
          return partial
      | otherwise = showConcrete pins dir depth partial depName dep

    showConcrete pins dir depth partial depName dep = do
      let (chosen, conflict) = selectDep pins dir depName dep
      found <- existingDependencyDir dir depName chosen
      putStrLn (replicate (2*depth) ' ' ++ "- " ++ depName ++ " (" ++ describeDep dep
                ++ (if conflict then " overridden -> " ++ describeDep chosen else "") ++ ")"
                ++ maybe " (not fetched)" (const "") found)
      case found of
        Nothing      -> return True
        Just depBase -> do
          spec' <- loadBuildSpec depBase
          childPartial <- showTree pins depBase spec' (depth + 1)
          return (partial || childPartial)

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
          ".tydb" -> do
            (paths, mn) <- findPaths filename C.defaultCompileOptions
            env0 <- Acton.Env.initEnv (sysTypes paths) False
            Acton.Types.showTyFile env0 mn filename (C.verbose gopts)

          ".act" -> do
            let modname = A.modName $ map (replace ".act" "") $ splitOn "/" $ fileBody
            (paths, _) <- findPaths filename C.defaultCompileOptions
            parsedRes <- parseActFile C.defaultCompileOptions Source.diskSourceProvider modname filename Nothing
            (_snap, parsed) <- case parsedRes of
              Left diags -> do
                printDiagnostics gopts diags
                System.Exit.exitFailure
              Right res -> return res

            -- Run compiler passes to get type information
            env0 <- Acton.Env.initEnv (sysTypes paths) False
            env <- Acton.Env.mkEnv (searchPath paths) env0 parsed
            kchecked <- Acton.Kinds.check env parsed
            (nmod, _, env', _) <- Acton.Types.reconstruct Nothing Nothing env kchecked (Just (moduleLabel paths modname))
            let I.NModule _ tenv mdoc = nmod

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

compileReplHook :: C.GlobalOptions -> C.CompileOptions -> FilePath -> [FilePath] -> IO Bool
compileReplHook gopts opts projDir srcFiles =
    withProjectLockNotice gopts projDir $
      compileFilesChanged Source.diskSourceProvider gopts opts srcFiles SelectedFiles Nothing Nothing

-- Compile Acton files ---------------------------------------------------------------------------------------------

-- | Compile a set of files in a single-shot build.
compileFiles :: Source.SourceProvider -> C.GlobalOptions -> C.CompileOptions -> [String] -> Scope -> IO ()
compileFiles sp gopts opts srcFiles scope =
    void $ compileFilesChanged sp gopts opts srcFiles scope Nothing Nothing

-- | Compile with optional change set, wiring progress UI and back jobs.
compileFilesChanged :: Source.SourceProvider
                    -> C.GlobalOptions
                    -> C.CompileOptions
                    -> [String]
                    -> Scope
                    -> Maybe [FilePath]
                    -> Maybe (CliWatchContext, Generation)
                    -> IO Bool
compileFilesChanged sp gopts opts srcFiles scope mChangedPaths mWatch = do
    (progressUI, progressState) <- case mWatch of
      Just (ctx, _) -> return (cwProgressUI ctx, cwProgressState ctx)
      Nothing -> do
        ui <- initProgressUI gopts
        st <- newProgressState
        return (ui, st)
    let logLine = progressLogLine progressUI
    (sched, gen) <- case mWatch of
      Just (ctx, gen) -> return (cwScheduler ctx, gen)
      Nothing -> do
        sched' <- newCompileScheduler gopts
        gen' <- currentGeneration sched'
        return (sched', gen')
    let cleanupProgress = whenCurrent gen (progressReset progressUI progressState)
    cleanupProgress
    let runCompile = do
          sp' <- overlayChangedPaths sp mChangedPaths
          planStart <- getTime Monotonic
          planRes <- try $
            prepareCompileContext opts srcFiles >>= \ctx ->
              prepareCompilePlan sp' gopts sched ctx (PlanRequest srcFiles scope mChangedPaths)
          logTiming gopts opts logLine "compile planning" planStart
          let reportPlanError (ProjectError msg) = do
                if C.watch opts
                  then logLine msg
                  else printErrorAndExit msg
                return True
              runPlan plan = do
                let cctx = cpContext plan
                    opts' = ccOpts cctx
                    watchMode = C.watch opts'
                reporter <- newCliReporter progressUI progressState gopts gen plan
                let finalizeCompile onError = do
                      cleanupProgress
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
                compileStart <- getTime Monotonic
                compileRes <- runCompilePlan sp gopts sched gen plan (reportEvent reporter)
                backFailure <- case compileRes of
                  Right _ -> waitBackJobs sched gen
                  Left _ -> return Nothing
                logTiming gopts opts' logLine "Acton compilation" compileStart
                case compileRes of
                  Left err ->
                    reportCompileError (compileFailureMessage err)
                  Right (env, hadErrors) -> do
                    case backFailure of
                      Just failure ->
                        reportCompileError (backPassFailureMessage failure)
                      Nothing ->
                        if hadErrors
                          then reportCompileErrors
                          else do
                            cleanupProgress
                            not <$> ifCurrent gen False (runCliPostCompile reporter gopts plan env)
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

-- | How the CLI reports one build: its events, its log lines and the final
-- step, on the progress UI.
data CliReporter = CliReporter
  { reportEvent        :: BuildEvent -> IO ()
  , reportLine         :: String -> IO ()
  , startFinal         :: IO ()
  , finishFinal        :: Bool -> IO ()       -- whether the final step succeeded
  , reporterProgressUI :: ProgressUI
  }

-- | The phases of a module that the CLI's progress percentage credits.
data ProgressPhase = ParsePhase | FrontPhase | BackPhase deriving (Eq, Ord)

-- | Build the compile event handler and state for CLI progress and
-- completion logs.
newCliReporter :: ProgressUI
               -> ProgressState
               -> C.GlobalOptions
               -> Generation
               -> CompilePlan
               -> IO CliReporter
newCliReporter progressUI progressState gopts gen plan = do
    let neededTasks = cpNeededTasks plan
        rootProj = ccRootProj (cpContext plan)
        rootPaths = ccPathsRoot (cpContext plan)
        optsPlan = ccOpts (cpContext plan)
    progressRef <- newIORef (0 :: Double)
    marksRef <- newIORef (M.empty :: M.Map (TaskKey, ProgressPhase) Double)
    -- sizeMap is used for the progress bar indicator to determine how much a
    -- modules compilation should contribute to the overall progress. We use
    -- file size as a heuristic for this, but if all sizes are zero (e.g. due to
    -- missing files or all files being empty), we fall back to equal weighting
    -- to avoid division by zero. Since our compilation is split into front and
    -- back passes, we also compute a share for each task that determines how
    -- much of the total progress it should contribute to, which is used to give
    -- a more accurate progress indication during the compile.
    let sizeMap = cpSourceSizes plan
        totalSize = sum (M.elems sizeMap)
        useEqual = totalSize <= 0
        totalWeight :: Double
        totalWeight =
          if useEqual
            then fromIntegral (max 1 (length neededTasks))
            else fromIntegral totalSize
        weightFor key =
          if useEqual
            then 1 / totalWeight
            else fromIntegral (sizeMap M.! key) / totalWeight
        compilePhaseTotal = 85.0
        -- The part of a module's weight that each phase carries. A module
        -- without a parse stage has no parse part.
        phaseRatio t ParsePhase
          | hasParseStage (cpContext plan) t = 0.1
          | otherwise                        = 0
        phaseRatio _ FrontPhase              = 0.6
        phaseRatio _ BackPhase               = 0.3
        totalPhaseWeight =
          sum
            [ weightFor (gtKey t) * (phaseRatio t FrontPhase + phaseRatio t BackPhase + phaseRatio t ParsePhase)
            | t <- neededTasks
            ]
        shareForPhase t ratio =
          if totalPhaseWeight <= 0
            then 0
            else compilePhaseTotal * weightFor (gtKey t) * ratio / totalPhaseWeight
        shareMap =
          M.fromList
            [ ((gtKey t, phase), shareForPhase t (phaseRatio t phase))
            | t <- neededTasks
            , phase <- [ParsePhase, FrontPhase, BackPhase]
            ]
    -- runCompilePlan calls onEvent only while gen is current, so the event
    -- arms and the helpers only they use do not check the generation.
    -- logLine, logRendered and withTerm are also called outside onEvent (by
    -- the final step, by runCliPostCompile and by the initial setPercent),
    -- so they check it themselves.
    let logLine msg = whenCurrent gen $ do
          progressLogLine progressUI msg
        logRendered render = whenCurrent gen $ do
          if puWidthAware progressUI
            then do
              (_, cols) <- termSizeRead (puTermSize progressUI)
              progressLogLine progressUI (render (safeLiveWidth cols))
            else progressLogLine progressUI (render plainLogWidth)
        logDiagnostics diags =
          progressWithLog progressUI (printDiagnostics gopts diags)
        termProgress = puTermProgress progressUI
        termEnabled = termProgressEnabled termProgress
        timeSep = "    "
        backFailStatus msg = "Compilation failed: " ++ msg
        frontOutputActiveStatus kind =
          case kind of
            FrontOutputTydb -> "Preparing .tydb"
            FrontOutputTydbCopy -> "Copying .tydb"
            FrontOutputDoc -> "Generating docs"
        frontOutputDoneStatus kind =
          case kind of
            FrontOutputTydb -> "Wrote .tydb"
            FrontOutputTydbCopy -> "Copied .tydb"
            FrontOutputDoc -> "Generated docs"
        frontOutputShortStatus kind =
          case kind of
            FrontOutputTydb -> ".tydb"
            FrontOutputTydbCopy -> "Copy .tydb"
            FrontOutputDoc -> "Docs"
        frontOutputProgressShortStatus kind p =
          case kind of
            FrontOutputTydb
              | "Preparing" `isPrefixOf` InterfaceFiles.tyDbWriteProgressLabel p -> "Prep .tydb"
              | "Writing" `isPrefixOf` InterfaceFiles.tyDbWriteProgressLabel p -> "Write .tydb"
            _ -> frontOutputShortStatus kind
        projMap = cpProjMap plan
        depNameMap =
          let rootDeps = maybe [] projDeps (M.lookup rootProj projMap)
              otherDeps = concatMap projDeps (M.elems projMap)
          in M.fromListWith (\_ old -> old) [ (p, n) | (n, p) <- rootDeps ++ otherDeps ]
        labelWidth = maximum (0 : [ length (moduleLabel rootPaths (tkMod (gtKey t))) | t <- neededTasks ])
        statusWidth = 68
        nameWidth = labelWidth + 2 + statusWidth
        timePadWidth = nameWidth + length timeSep
        plainLogWidth = timePadWidth + timeMinWidth
        detailStmtIndentWide = replicate 5 ' '
        detailBindsIndentWide = replicate 7 ' '
        detailStmtIndentNarrow = "  "
        detailBindsIndentNarrow = "    "
        plainDoneIndent = replicate 3 ' '
        plainStatusColumns modLbl status =
          padRight labelWidth modLbl
          ++ "  "
          ++ padRight statusWidth (abbreviateRight statusWidth status)
        plainDoneTimedLine modLbl status t =
          padRight timePadWidth (plainDoneIndent ++ plainStatusColumns modLbl status)
          ++ padLeft timeMinWidth (fmtTimePrecise t)
        plainDoneLine modLbl status =
          plainDoneIndent ++ plainStatusColumns modLbl status
        pickBestLine candidates =
          case foldl' betterCandidate Nothing candidates of
            Just (_, line) -> line
            Nothing -> ""
        betterCandidate best Nothing = best
        betterCandidate Nothing cand = cand
        betterCandidate best@(Just (bestScore, _)) cand@(Just (candScore, _))
          | candScore > bestScore = cand
          | otherwise = best
        buildLineCandidate width preserveBlankLabel modLbl statusRender timerRank timerWidth timer =
          let doneIndent = replicate (progressPrefixWidth width) ' '
              timerCols = if null timer then 0 else 1 + timerWidth
              bodyWidth = max 0 (width - length doneIndent - timerCols)
              layout = fitBuildLineLayout bodyWidth labelWidth statusWidth preserveBlankLabel modLbl statusRender
              body = bllText layout
              line = doneIndent ++ body ++ if null timer then "" else " " ++ padLeft timerWidth timer
              score =
                ( if bllAligned layout then 1 :: Int else 0
                , if bllHasStatus layout then 1 :: Int else 0
                , bllLabelCols layout
                , timerRank
                )
          in if null body || length line > width
               then Nothing
               else Just (score, line)
        doneStatusLine width modLbl statusRender shortStatus mt =
          let preciseTimer = maybe "" fmtTimePrecise mt
              compactTimer = maybe "" fmtTimeCompact mt
              timerWidth = max timeMinWidth (length preciseTimer)
              fullRenderer budget =
                case statusRender budget of
                  Just status
                    | length status <= budget -> Just status
                  _ -> Nothing
              shortRenderer =
                staticStatusRenderer shortStatus shortStatus
              preserveBlankLabel = null modLbl
          in pickBestLine
               [ buildLineCandidate width preserveBlankLabel modLbl fullRenderer 2 timerWidth preciseTimer
               , buildLineCandidate width preserveBlankLabel modLbl fullRenderer 1 timerWidth compactTimer
               , buildLineCandidate width preserveBlankLabel modLbl fullRenderer 0 0 ""
               , buildLineCandidate width preserveBlankLabel modLbl shortRenderer 1 timerWidth compactTimer
               , buildLineCandidate width preserveBlankLabel modLbl shortRenderer 0 0 ""
               ]
        detailLine width indentWide indentNarrow msg =
          let wide = indentWide ++ msg
          in if width >= length wide then wide else indentNarrow ++ msg
        detailTimedLine width indentWide indentNarrow msg t =
          let precise = fmtTimePrecise t
              wideBase = indentWide ++ msg
              wideLine = padRight timePadWidth wideBase ++ precise
          in if width >= length wideLine
               then wideLine
               else indentNarrow ++ msg ++ " " ++ fmtTimeCompact t
        frontTimingLine ft =
          "Front timing: env " ++ fmtTimePrecise (ftEnv ft)
          ++ ", kinds " ++ fmtTimePrecise (ftKinds ft)
          ++ ", types " ++ fmtTimePrecise (ftTypes ft)
        frontTypeTimingLine ft =
          "Front type detail: reconstruct " ++ fmtTimePrecise (ftTypeReconstruct ft)
          ++ " (after progress " ++ fmtTimePrecise (ftTypeAfterProgress ft) ++ ")"
          ++ ", force " ++ fmtTimePrecise (ftTypeForce ft)
          ++ ", escape " ++ fmtTimePrecise (ftTypeEscape ft)
          ++ ", hash " ++ fmtTimePrecise (ftTypeHash ft)
        typeStmtTimingLine st =
          "Type stmt " ++ show (tstCompleted st) ++ "/" ++ show (tstTotal st)
        typeStmtBindsLine st =
          "binds: " ++ intercalate ", " (tstNames st)
        inferredSignatureLine sig =
          "Non-total inferred signature: " ++ intercalate ", " (isigNames sig)
        backTimingLine timing =
          "Back timing: " ++ intercalate ", " [ backPassName pass ++ " " ++ fmtTimePrecise t | (pass, t) <- timing ]
        backFailRenderer msg =
          \budget ->
            if budget < 10
              then Nothing
              else Just (abbreviateRight budget (backFailStatus msg))
        -- The line of a finished step: its label, its status, or the short
        -- status where the status does not fit, and the time it took when
        -- that is known.
        doneLine width label status short mt
          | puWidthAware progressUI = doneStatusLine width label (staticStatusRenderer status short) short mt
          | otherwise               = maybe (plainDoneLine label status) (plainDoneTimedLine label status) mt
        backFailLine width mn msg =
          if puWidthAware progressUI
            then doneStatusLine width (moduleLabel rootPaths mn) (backFailRenderer msg) "Failed" Nothing
            else plainDoneLine (moduleLabel rootPaths mn) (backFailStatus msg)
        renderProjectLine mn statusRender width =
          fitBuildLineLayout width labelWidth statusWidth False (moduleLabel rootPaths mn) statusRender
        parseActiveLine mn =
          renderProjectLine mn (staticStatusRenderer "Parsing" "Parse")
        parseProgressLine mn p =
          renderProjectLine mn (parseStatusRenderer p)
        frontInitialLine mn =
          renderProjectLine mn (staticStatusRenderer "Checking module" "Check")
        frontOutputActiveLine key kind =
          renderProjectLine (tkMod key)
            (staticStatusRenderer (frontOutputActiveStatus kind) (frontOutputShortStatus kind))
        frontOutputProgressLine key kind p =
          renderProjectLine (tkMod key)
            (staticStatusRenderer (InterfaceFiles.tyDbWriteProgressLabel p) (frontOutputProgressShortStatus kind p))
        frontOutputInitialProgress kind =
          case kind of
            FrontOutputTydb -> Just 0
            _ -> Nothing
        backActiveLine mn =
          renderProjectLine mn (staticStatusRenderer "Back passes" "Back")
        backProgressLine mn p =
          renderProjectLine mn (backPassStatusRenderer p)
        finalActiveLine width =
          fitBuildLineLayout width labelWidth statusWidth True "" (staticStatusRenderer "Final compilation" "Final")
        clamp01 x = max 0 (min 1 x)
        -- The completed part of a total, between 0 and 1, and 1 when the
        -- total is not positive.
        ratio completed total
          | total <= 0 = 1
          | otherwise  = clamp01 (fromIntegral completed / fromIntegral total)
        parseProgressRatio p = ratio (ppCompleted p) (ppTotal p)
        frontPassFraction p =
          case fppPass p of
            FrontPassKinds -> 0.05 * passRatio
            FrontPassTypes -> 0.05 + 0.85 * passRatio
            FrontPassHash  -> 0.90 + 0.10 * passRatio
          where passRatio = ratio (fppCompleted p) (fppTotal p)
        frontStatusRenderer p budget
          | budget < 10 = Nothing
          | otherwise =
              let total = max 0 (fppTotal p)
                  completed = min total (max 0 (fppCompleted p))
                  countPart
                    | total > 0 = " " ++ show completed ++ "/" ++ show total
                    | otherwise = ""
                  -- The pass's word, the current name if any, abbreviated
                  -- to the room left, and the count. Where the word, a
                  -- space and the count fill the budget, the short word
                  -- and the count if they fit, else the short word.
                  passStatus word short current
                    | staticLen < budget                    = maybe word (\name -> word ++ " " ++ abbreviateRight (budget - staticLen) name) current ++ countPart
                    | length (short ++ countPart) <= budget = short ++ countPart
                    | otherwise                             = short
                    where staticLen = length word + 1 + length countPart
              in case fppPass p of
                   FrontPassKinds -> staticStatusRenderer "Kinds check" "Kinds" budget
                   FrontPassTypes -> Just (passStatus "Type checking" "Types" (fppCurrent p))
                   FrontPassHash  -> Just (passStatus "Hashing" "Hash" (fppCurrent p))
        backPassStatusRenderer p
          | bppEvent p == BackPassSkipped = staticStatusRenderer ("Back " ++ passName ++ " skipped") (passName ++ " skipped")
          | otherwise                     = staticStatusRenderer ("Back " ++ passName) passName
          where passName = backPassName (bppPass p)
        parseStatusRenderer p budget
          | budget < 10 = Nothing
          | otherwise =
              let pct = floor (100 * parseProgressRatio p :: Double)
                  full = "Parsing " ++ show pct ++ "%"
                  short = "Parse " ++ show pct ++ "%"
              in if length full <= budget
                   then Just full
                   else if length short <= budget
                          then Just short
                          else Just (show pct ++ "%")
        frontProgressLine mn p =
          renderProjectLine mn (frontStatusRenderer p)
        backProgressRatio p =
          fromIntegral (bppCompleted p) / fromIntegral (bppTotal p)
        backPassDoneLine mn pass =
          "Back " ++ backPassName pass ++ " done: " ++ moduleLabel rootPaths mn
        finalKey = ProgressTaskKey (TaskKey rootProj (A.modName ["__final__"]))
        -- A progress row: started with its line and ratio, updated, and
        -- ended, which gives its elapsed time.
        startRow row line ratio = progressStartTask progressUI progressState row line ratio
        updateRow row line ratio = progressUpdateTask progressUI progressState row line (Just ratio)
        endRow row = progressDoneTask progressUI progressState row
        showDoneLines = not (quiet gopts optsPlan)
        withTerm action = when termEnabled $ whenCurrent gen (withProgressLock progressUI action)
        setPercent pct = withTerm (termProgressPercent termProgress pct)
        addProgress delta =
          when (termEnabled && delta > 0) $ do
            new <- atomicModifyIORef' progressRef (\x -> let x' = min compilePhaseTotal (x + delta) in (x', x'))
            setPercent (floor new)
        -- Credit a module's phase as done up to the fraction: the phase's
        -- share times the part of it not credited before. A smaller
        -- fraction than one credited before adds nothing.
        credit phase key fraction = do
          let share = M.findWithDefault 0 (key, phase) shareMap
          delta <- atomicModifyIORef' marksRef $ \marks ->
            let done  = M.findWithDefault 0 (key, phase) marks
                done' = max done (clamp01 fraction)
            in (M.insert (key, phase) done' marks, share * (done' - done))
          addProgress delta
    setPercent 0
    let onEvent event = case event of
          Diagnostics t diags -> do
            void (endRow (ProgressTaskKey (gtKey t)))
            logDiagnostics diags
          ParseStarted t ->
            startRow (ProgressTaskKey (gtKey t)) (parseActiveLine (tkMod (gtKey t))) (Just 0)
          ParseProgressed t p -> do
            updateRow (ProgressTaskKey (gtKey t)) (parseProgressLine (tkMod (gtKey t)) p) (parseProgressRatio p)
            credit ParsePhase (gtKey t) (parseProgressRatio p)
          ParseFinished t mtime -> do
            void (endRow (ProgressTaskKey (gtKey t)))
            credit ParsePhase (gtKey t) 1
            when showDoneLines $
              forM_ mtime $ \tParse ->
                logRendered (\cols -> doneLine cols (moduleLabel rootPaths (tkMod (gtKey t))) "Parse done" "Parsed" (Just tParse))
          FrontStarted t ->
            startRow (ProgressTaskKey (gtKey t)) (frontInitialLine (tkMod (gtKey t))) (Just 0)
          FrontProgressed t p -> do
            updateRow (ProgressTaskKey (gtKey t)) (frontProgressLine (tkMod (gtKey t)) p) (frontPassFraction p)
            credit FrontPhase (gtKey t) (frontPassFraction p)
          FrontFinished t result -> do
            void (endRow (ProgressTaskKey (gtKey t)))
            credit FrontPhase (gtKey t) 1
            forM_ result $ \fr -> do
              forM_ (frFrontTime fr) $ \tFront -> do
                let mn = tkMod (gtKey t)
                    -- Render the whole front-result block (the module's "Type
                    -- check done" line plus any timing/inferred-signature detail)
                    -- and emit it as one atomic log write. The detail lines carry
                    -- no module name, so logging them separately would let another
                    -- module's progress interleave between them and leave the line
                    -- stream impossible to attribute. One write keeps the block
                    -- intact (and reads as one module's result either way).
                    timingLines cols
                      | C.timing gopts =
                          concat [ detailLine cols detailStmtIndentWide detailStmtIndentNarrow (frontTimingLine ft)
                                 : detailLine cols detailStmtIndentWide detailStmtIndentNarrow (frontTypeTimingLine ft)
                                 : (if C.verbose gopts
                                      then concat [ [ detailTimedLine cols detailStmtIndentWide detailStmtIndentNarrow (typeStmtTimingLine st) (tstTime st)
                                                    , detailLine cols detailBindsIndentWide detailBindsIndentNarrow (typeStmtBindsLine st)
                                                    ]
                                                  | st <- ftTypeStmtTimings ft ]
                                      else [])
                                 | ft <- maybeToList (frFrontTiming fr) ]
                      | otherwise = []
                    sigLines cols
                      | C.timing gopts || C.verbose gopts =
                          concat [ detailLine cols detailStmtIndentWide detailStmtIndentNarrow (inferredSignatureLine sig)
                                 : [ detailLine cols detailBindsIndentWide detailBindsIndentNarrow line
                                   | line <- lines (isigSignature sig) ]
                                 | sig <- frInferredSigs fr ]
                      | otherwise = []
                logRendered (\cols -> intercalate "\n" (doneLine cols (moduleLabel rootPaths mn) "Type check done" "Typed" (Just tFront) : (timingLines cols ++ sigLines cols)))
              case frBack fr of
                NoBack -> credit BackPhase (gtKey t) 1
                _      -> return ()
          OutputStarted key kind ->
            startRow (ProgressFrontOutputKey key kind) (frontOutputActiveLine key kind) (frontOutputInitialProgress kind)
          OutputProgressed key kind p ->
            updateRow (ProgressFrontOutputKey key kind) (frontOutputProgressLine key kind p) (clamp01 (InterfaceFiles.tyDbWriteProgressRatio p))
          OutputFinished key kind melapsed -> do
            void (endRow (ProgressFrontOutputKey key kind))
            when showDoneLines $
              forM_ melapsed $ \elapsed ->
                logRendered (\cols -> doneLine cols (moduleLabel rootPaths (tkMod key)) (frontOutputDoneStatus kind) (frontOutputShortStatus kind) (Just elapsed))
          BackReady _ -> return ()
          BackSkipped key -> credit BackPhase key 1
          BackStarted job ->
            startRow (ProgressTaskKey (bjKey job)) (backActiveLine (tkMod (bjKey job))) (Just 0)
          BackProgressed job p -> do
            updateRow (ProgressTaskKey (bjKey job)) (backProgressLine (tkMod (bjKey job)) p) (backProgressRatio p)
            when (showDoneLines && (C.timing gopts || C.verbose gopts)) $
              case bppEvent p of
                BackPassFinished elapsed ->
                  logRendered (\cols ->
                    detailTimedLine cols detailStmtIndentWide detailStmtIndentNarrow
                      (backPassDoneLine (tkMod (bjKey job)) (bppPass p)) elapsed)
                _ -> return ()
          BackFinished job result -> do
            void (endRow (ProgressTaskKey (bjKey job)))
            credit BackPhase (bjKey job) 1
            when showDoneLines $
              case result of
                BackJobOk mtime mtiming -> do
                  logRendered (\cols -> doneLine cols (moduleLabel rootPaths (tkMod (bjKey job))) "Compilation done" "Built" mtime)
                  when (C.timing gopts) $
                    forM_ mtiming $ \timing ->
                      logRendered (\cols -> detailLine cols detailStmtIndentWide detailStmtIndentNarrow (backTimingLine timing))
                BackJobFailed failure ->
                  logRendered (\cols -> backFailLine cols (tkMod (bjKey job)) (bpfMessage failure))
          Info msg -> logLine msg
        onFinalStart = do
          whenCurrent gen (startRow finalKey finalActiveLine Nothing)
          setPercent 85
        onFinalDone success =
          whenCurrent gen $ do
            mElapsed <- endRow finalKey
            setPercent 100
            when success $
              forM_ mElapsed $ \tFinal ->
                when showDoneLines $
                  logRendered (\cols -> doneLine cols "" "Final compilation done" "Final" (Just tFinal))
    return CliReporter
      { reportEvent = onEvent
      , reportLine = logLine
      , startFinal = onFinalStart
      , finishFinal = onFinalDone
      , reporterProgressUI = progressUI
      }

-- | Run CLI-only post-compile steps.
runCliPostCompile :: CliReporter
                  -> C.GlobalOptions
                  -> CompilePlan
                  -> Acton.Env.Env0
                  -> IO Bool
runCliPostCompile reporter gopts plan env = do
    prepStart <- getTime Monotonic
    let logLine = reportLine reporter
    let cctx = cpContext plan
        opts' = ccOpts cctx
        pathsRoot = ccPathsRoot cctx
        rootProj = ccRootProj cctx
        sysAbs = ccSysAbs cctx
        rootModules = cpRootModules plan
        allowPrune = cpAllowPrune plan
        neededTasks = cpNeededTasks plan
        projMap = cpProjMap plan
        proj = projName pathsRoot
    rootSpec <- case M.lookup rootProj projMap of
                  Just ctx -> return (projBuildSpec ctx)
                  Nothing -> throwProjectError ("Missing root project context for " ++ rootProj)
    let rootParts = splitOn "." (C.root opts')
        guessMod  = if length rootParts == 1 then ccRootModule cctx else addProjPrefix pathsRoot (A.modName (init rootParts))
        binTask   = BinTask (A.GName guessMod (A.name $ last rootParts)) False
        preBinTasks
          | null (C.root opts') = map (\mn -> BinTask (A.GName mn (A.name "main")) False) rootModules
          | otherwise        = [binTask]
    requestedModules <- mapM (moduleNameFromFile (srcDir pathsRoot) proj) (requestFiles (cpRequest plan))
    let preTestBinTasks = [ BinTask (A.GName mn (A.name "test_main")) True
                          | mn <- rootModules, mn `elem` requestedModules ]
        selectedTasksByProj = M.fromListWith (++) [ (tkProj (gtKey t), [t]) | t <- neededTasks ]
        depModuleOptsByProj = depModuleOptionsByProj selectedTasksByProj projMap
        selectedModuleSources = selectedCSourceEntriesForProj rootProj (M.findWithDefault [] rootProj selectedTasksByProj)
        rootBuildLibraries = explicitBuildLibraries selectedModuleSources rootSpec
        rootModuleSources = excludeLibrarySources rootBuildLibraries selectedModuleSources
        rootDepModuleOpts = M.findWithDefault M.empty rootProj depModuleOptsByProj
        rootDepPathOverrides = projectDepPathOverrides projMap rootProj

    -- Zig follows every declared dependency, including projects with no
    -- selected modules. Refresh their build files too so nested dependencies
    -- cannot retain module selections from an earlier build.
    forM_ (M.toList projMap) $ \(p, pctx) -> do
      let isRootProj = p == rootProj
      unless (isRootProj || isSysProject sysAbs p) $ do
        when (C.verbose gopts) $
          logLine ("Generating build.zig for dependency project " ++ p)
        let depPaths = projectPaths opts' projMap pctx
            depOpts = M.findWithDefault M.empty p depModuleOptsByProj
            depPathOverrides = projectDepPathOverrides projMap p
        createProjectDirectories depPaths
        genBuildZigFiles (projBuildSpec pctx) depPaths depOpts depPathOverrides
    logTiming gopts opts' logLine "dependency build preparation" prepStart
    let runFinal action = do
          startFinal reporter
          success <- action `onException` finishFinal reporter False
          finishFinal reporter success
          return success
    if C.skip_build opts'
      then
        logLine "  Skipping final build step" >> return True
      else if C.altOutput opts' then return True else do
        let (candidateBinTasks, explicitRoot)
              | C.test opts' = (preTestBinTasks, False)
              | otherwise    = (preBinTasks, not (null (C.root opts')))
        selected <- mapM (selectRootActor env pathsRoot explicitRoot) candidateBinTasks
        case partitionEithers selected of
          ([], binTasks) ->
            runFinal (zigBuild env gopts opts' pathsRoot rootSpec rootModules (catMaybes binTasks) allowPrune rootModuleSources rootBuildLibraries rootDepModuleOpts rootDepPathOverrides (Just (reporterProgressUI reporter)))
          (diags, _) -> do
            progressWithLog (reporterProgressUI reporter) (printDiagnostics gopts diags)
            unless (C.watch opts') System.Exit.exitFailure
            return False

-- Generate documentation index for a project build by reading module names and
-- docstrings from cached .tydb headers or source headers.
generateProjectDocIndex :: Source.SourceProvider -> C.GlobalOptions -> C.CompileOptions -> Paths -> [String] -> IO ()
generateProjectDocIndex sp gopts opts paths srcFiles = do
    unless (C.skip_build opts || C.only_build opts || isTmp paths) $ do
        let docDir = projectDocDir (projPath paths)
        createDirectoryIfMissing True docDir
        entries <- catMaybes <$> forM srcFiles (\f -> do
                     (p, mn) <- findPaths f opts
                     docIndexEntry opts p mn <$> readModuleTask sp gopts opts p mn f)
        DocP.generateDocIndex docDir entries

-- | Relative C source paths for selected tasks in one project.
selectedCSourceEntriesForProj :: FilePath -> [GlobalTask] -> [FilePath]
selectedCSourceEntriesForProj proj tasks =
    nub
      [ collapseDots (makeRelativeOrAbsolute proj (generatedPath (gtPaths t) (tkMod (gtKey t)) cFile))
      | t <- tasks
      ]

explicitBuildLibraries :: [FilePath] -> BuildSpec.BuildSpec -> [BuildLibrary]
explicitBuildLibraries selectedSources spec =
    [ BuildLibrary libName (BuildSpec.libLinkage lib) sources
    | (libName, lib) <- M.toList (BuildSpec.libraries spec)
    , let sources = map moduleCSource (BuildSpec.libModules lib)
    , any (`Data.Set.member` selectedSet) sources
    ]
  where
    selectedSet = Data.Set.fromList selectedSources
    moduleCSource modName =
      collapseDots ("out/types/" ++ BuildSpec.specName spec ++ "/" ++ map dotToSlash modName ++ ".c")
    dotToSlash c
      | c == '.'  = '/'
      | otherwise = c

excludeLibrarySources :: [BuildLibrary] -> [FilePath] -> [FilePath]
excludeLibrarySources libs sources =
    filter (`Data.Set.notMember` libSourceSet) sources
  where
    libSourceSet = Data.Set.fromList (concatMap buildLibSources libs)

-- | Relative root-stub C paths for selected binaries.
selectedRootStubEntriesForBins :: Paths -> [BinTask] -> [FilePath]
selectedRootStubEntriesForBins paths bins =
    nub
      [ collapseDots (makeRelativeOrAbsolute (projPath paths) (binTaskRoot paths b))
      | b <- bins
      ]

-- | For each consuming project, map dep name to selected C-source CSV.
depModuleOptionsByProj :: M.Map FilePath [GlobalTask] -> M.Map FilePath ProjCtx -> M.Map FilePath (M.Map String String)
depModuleOptionsByProj tasksByProj projMap =
    M.map mkForProj projMap
  where
    mkForProj ctx =
      M.fromList
        [ (depName, mkCsv depProj)
        | (depName, depProj) <- projDeps ctx
        ]
    mkCsv depProj =
      intercalate "," (selectedCSourceEntriesForProj depProj (M.findWithDefault [] depProj tasksByProj))

-- | Canonical package dependency roots reachable from one project.
--
-- These paths come from project discovery, so they already reflect root pin
-- overrides and fingerprint deduplication. Reusing them for build.zig.zon keeps
-- Zig pointed at the same dependency roots where we generated build.zig files.
projectDepPathOverrides :: M.Map FilePath ProjCtx -> FilePath -> M.Map String FilePath
projectDepPathOverrides projMap rootProj = snd (go Data.Set.empty rootProj M.empty)
  where
    go seen proj acc
      | Data.Set.member proj seen = (seen, acc)
      | otherwise =
          case M.lookup proj projMap of
            Nothing -> (Data.Set.insert proj seen, acc)
            Just ctx ->
              let seen' = Data.Set.insert proj seen
              in foldl' step (seen', acc) (projDeps ctx)

    step (seen, acc) (depName, depProj) =
      let acc' = M.insertWith (\_ old -> old) depName depProj acc
      in go seen depProj acc'

-- | Remove the root stubs and binaries of roots that their module no longer
-- declares. The .tydb headers written by this build list each module's roots,
-- so a module that loses its root actor loses its stub and its binary in
-- out/bin, while those of other build modes stay (e.g. the .root.c during
-- `acton test`). Roots built in this run stay too.
removeUndeclaredRoots :: Paths -> [A.ModName] -> [BinTask] -> IO ()
removeUndeclaredRoots paths modules bins = do
    declared <- concat <$> mapM headerRoots modules
    let live f = Data.Set.fromList [ f test mn | (test, mn) <- declared ++ map binTaskModule bins ]
        liveStubs = live (rootStubPath paths)
        liveBinaries = live (rootBinaryName paths)
        stubs = [ rootStubPath paths test mn | mn <- modules, test <- [False, True] ]
    mapM_ removePathForcibly (filter (`Data.Set.notMember` liveStubs) stubs)
    -- A scratch build's binDir is the source directory, so leave it alone.
    binDirExists <- doesDirectoryExist (binDir paths)
    when (binDirExists && not (isTmp paths)) $ do
      files <- filterM doesFileExist . map (binDir paths </>) =<< listDirectory (binDir paths)
      mapM_ removeFile (filter ((`Data.Set.notMember` liveBinaries) . binaryOf . takeFileName) files)
  where
    headerRoots mn =
      maybe [] (\header -> [ (A.nstr n == "test_main", mn) | n <- InterfaceFiles.summaryRoots header ])
        <$> InterfaceFiles.tryReadHeaderSummary (tyDbPath paths mn)
    -- Windows binaries end in .exe, and their debug info in .pdb.
    binaryOf f | takeExtension f `elem` [".exe", ".pdb"] = dropExtension f
               | otherwise = f

data BinTask = BinTask { rootActor :: A.QName, isTest :: Bool } deriving (Show)

data BuildLibrary = BuildLibrary { buildLibName :: String, buildLibLinkage :: String, buildLibSources :: [FilePath] } deriving (Show)

encodeBuildLibraries :: [BuildLibrary] -> String
encodeBuildLibraries libs =
    intercalate "|" [ buildLibName lib ++ ":" ++ buildLibLinkage lib ++ ":" ++ intercalate "," (buildLibSources lib)
                    | lib <- libs
                    ]

-- | Whether a bin task builds a test, and the module of its root actor.
binTaskModule :: BinTask -> (Bool, A.ModName)
binTaskModule binTask = (isTest binTask, m)
  where A.GName m _ = rootActor binTask

binTaskRoot :: Paths -> BinTask -> FilePath
binTaskRoot paths = uncurry (rootStubPath paths) . binTaskModule

printDiag :: C.GlobalOptions -> Diagnostic String -> IO ()
printDiag gopts d = do
    -- TODO: change to print to stderr! current tests presume stdout so we print to stdout for now..
    shouldColor <- useColor gopts
    if shouldColor
      then printDiagnostic stdout WithUnicode (TabSize 4) defaultStyle d
      else hPutDoc stdout $ unAnnotate (prettyDiagnostic WithoutUnicode (TabSize 4) d)

printDiagnostics :: C.GlobalOptions -> [Diagnostic String] -> IO ()
printDiagnostics gopts diags =
    mapM_ (printDiag gopts) diags

-- | Look up the root name of a bin task in the .tydb of its module. The bin
-- task builds when the name is an actor that can be the root actor. An actor
-- that cannot is an error at its definition, and so is any other definition
-- of a name given with --root. A module that does not define the default root
-- name (main, or test_main in a test build) as an actor builds no binary,
-- while a name given with --root must be defined.
selectRootActor :: Acton.Env.Env0 -> Paths -> Bool -> BinTask -> IO (Either (Diagnostic String) (Maybe BinTask))
selectRootActor env paths explicitRoot binTask = do
    let A.GName m n = rootActor binTask
        mn = dropProjPrefix paths m
    mty <- Acton.Env.findTyFile (searchPath paths) m
    db <- case mty of
            Just ty -> InterfaceFiles.openInterfaceDB ty
            Nothing -> throwProjectError ("Root module " ++ prstr mn ++ " not found")
    entry <- InterfaceFiles.readInterfaceDBNameInfoMaybe db n
    case entry of
      Just (n', i) -> case rootActorProblem (simp (setMod m env)) i of
        Nothing -> return (Right (Just binTask))
        Just problem
          | explicitRoot || isActor i -> do
              src <- readFile (srcFile paths m)
              return (Left (Diag.actErrToDiagnostic "Compilation error" (modNameToFilename mn) src (A.nloc n')
                              ("Cannot use " ++ A.nstr n ++ " as the root actor: " ++ problem)))
          | otherwise -> return (Right Nothing)
      Nothing
        | explicitRoot -> throwProjectError ("Cannot use " ++ prstr (A.GName mn n) ++ " as the root actor: module "
                                             ++ prstr mn ++ " does not define " ++ A.nstr n)
        | otherwise -> return (Right Nothing)
  where isActor I.NAct{} = True
        isActor _        = False

-- | Generate the root actor C file of a bin task that selectRootActor kept.
writeRootC :: Acton.Env.Env0 -> C.GlobalOptions -> C.CompileOptions -> Paths -> BinTask -> IO (Maybe BinTask)
writeRootC env gopts opts paths binTask = do
    let qn = rootActor binTask
        rootFile = binTaskRoot paths binTask
    -- In --only-build mode, reuse existing root stubs; generate only if missing.
    existing <- if C.only_build opts then doesFileExist rootFile else return False
    if C.only_build opts && existing
      then return (Just binTask)
      else do
        res <- (try :: IO a -> IO (Either SomeException a)) $ do
          c <- Acton.CodeGen.genRoot env qn
          createDirectoryIfMissing True (takeDirectory rootFile)
          writeFileIfChanged rootFile c
        case res of
          Right _ -> return (Just binTask)
          Left _  -> return Nothing

-- | Check whether a target triple refers to Windows.
isWindowsOS :: String -> Bool
isWindowsOS targetTriple = case splitOn "-" targetTriple of
    (_:os:_) -> os == "windows"
    _        -> False

-- | Run a process and capture output, canceling on exceptions.
readProcessWithExitCodeCancelable :: CreateProcess -> (ProcessHandle -> IO ()) -> IO (ExitCode, String, String)
readProcessWithExitCodeCancelable cp onStart = mask $ \restore -> do
    let cp' = cp { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe, create_group = True }
    withCreateProcess cp' $ \mIn mOut mErr ph -> do
      -- Zig's maker process inherits stdin, so keep the descriptor open at EOF.
      mapM_ hClose mIn
      pid <- getPid ph
      groupOwned <- newIORef True
      let stop = mask $ \_ -> do
            owned <- readIORef groupOwned
            when owned $ do
              stopProcessGroup ph pid
              writeIORef groupOwned False
          readHandle Nothing = return ""
          readHandle (Just handle) = do
            txt <- hGetContents handle
            _ <- evaluate (length txt)
            hClose handle
            return txt
      (restore $ do
          onStart ph
          Async.withAsync (readHandle mOut) $ \outReader ->
            Async.withAsync (readHandle mErr) $ \errReader -> do
              code <- waitForProcess ph
              stop
              out <- Async.wait outReader
              err <- Async.wait errReader
              return (code, out, err)) `finally` stop

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
    homeDir <- getHomeDirectory
    let zigEnv = [ ("ZIG_LOCAL_CACHE_DIR", zigLocalCacheDir homeDir)
                 , ("ZIG_GLOBAL_CACHE_DIR", zigGlobalCacheDir homeDir)
                 ]
        envWithZigCache = foldr (\(k, v) acc -> (k, v) : filter ((/= k) . fst) acc) env0 zigEnv
        env2 = case envOverride of
          Nothing -> Just envWithZigCache
          Just (k, v) -> Just ((k, v) : filter ((/= k) . fst) envWithZigCache)
        cpBase = (proc zigExe zigArgs){ cwd = wd, env = env2 }
        cp = if closeFds then cpBase else cpBase { close_fds = False }
    zigStart <- getTime Monotonic
    (returnCode, zigStdout, zigStderr) <- readProcessWithExitCodeCancelable cp onStart `finally` onStop
    let logLine = maybe putStrLn progressLogLine mProgressUI
    logTiming gopts opts logLine "Zig build" zigStart
    case returnCode of
        ExitSuccess -> do
          iff (C.verboseZig gopts) $ putStrLn zigStderr
          when (C.timing gopts && not (quiet gopts opts) && not (C.verboseZig gopts)) $
            -- Zig's summary includes cache hits and elapsed C/link build steps.
            -- Their durations overlap when Zig runs them in parallel.
            mapM_ logLine (dropWhile (not . isPrefixOf "Build Summary:") (lines zigStderr))
          return True
        ExitFailure ret -> do
          printIce ("compilation of generated Zig code failed, returned error code" ++ show ret)
          putStrLn $ "zig stdout:\n" ++ zigStdout
          putStrLn $ "zig stderr:\n" ++ zigStderr
          unless (C.watch opts) System.Exit.exitFailure
          return False

generateFingerprint :: String -> IO String
generateFingerprint name = do
    ident <- randomRIO (1, 0xfffffffe :: Word32)
    let prefix = Fingerprint.fingerprintPrefixForName name
        fp = (fromIntegral prefix `shiftL` 32) .|. fromIntegral ident
    return (Fingerprint.formatFingerprint fp)

-- | Whether a project is part of the Acton distribution (base, std, ...).
isSysProject :: FilePath -> FilePath -> Bool
isSysProject sysAbs projAbs =
    projAbs == sysAbs || addTrailingPathSeparator sysAbs `isPrefixOf` projAbs

-- | Directory holding the build.zig and build.zig.zon that zig builds a
-- project from. Distribution projects have checked-in build files at their
-- root. For every other project we generate them into out/zig, keeping the
-- project root free of build system files. The builder template
-- (builder/build.zig) takes the project root to be two levels up from it.
zigBuildDir :: FilePath -> FilePath -> FilePath
zigBuildDir sysAbs projAbs
  | isSysProject sysAbs projAbs = projAbs
  | otherwise                   = projectOutDir projAbs </> "zig"

-- Render build.zig and build.zig.zon from templates and BuildSpec.
-- Dependency paths come from the selected, deduplicated project graph.
genBuildZigFiles :: BuildSpec.BuildSpec -> Paths -> M.Map String String -> M.Map String FilePath -> IO ()
genBuildZigFiles spec paths depModuleOpts depPathOverrides = do
    let proj = projPath paths
    projAbs <- canonicalizePath proj
    let sys              = sysPath paths
        distBuildZigPath = joinPath [sys, "builder", "build.zig"]
        distBuildZonPath = joinPath [sys, "builder", "build.zig.zon"]
    buildZigTemplate <- readFile distBuildZigPath
    buildZonTemplate <- readFile distBuildZonPath
    let zonName = BuildSpec.specName spec
        fp = BuildSpec.fingerprint spec
    (transPkgs, transZigs) <- collectDepsRecursive spec proj depPathOverrides
    absSys <- canonicalizePath sys
    let buildDir = zigBuildDir absSys projAbs
        relSys = relativeViaRoot buildDir absSys
    homeDir <- getHomeDirectory
    depsRootAbs <- normalizePathSafe (depCacheDir homeDir)
    normalizedSpec <- normalizeSpecPaths proj spec
    let directZigs = [ ZigDepRef depName (rebaseZigDep projAbs proj dep)
                     | (depName, dep) <- M.toList (BuildSpec.zig_dependencies normalizedSpec)
                     ]
        mergedSpec0 = normalizedSpec { BuildSpec.dependencies = BuildSpec.dependencies normalizedSpec `M.union` transPkgs }
        mergedSpec1 = applyPkgDepPathOverrides projAbs depPathOverrides mergedSpec0
        mergedSpec = addImplicitStdDependency absSys mergedSpec1
        resolvedZigs = resolveZigDepRefs (directZigs ++ transZigs)
        zonWithFp = replace "{{fingerprint}}" fp . replace "{{name}}" zonName
    createDirectoryIfMissing True buildDir
    writeFileIfChanged (buildDir </> "build.zig") (genBuildZig buildZigTemplate absSys mergedSpec resolvedZigs depModuleOpts)
    writeFileIfChanged (buildDir </> "build.zig.zon") (genBuildZigZon buildZonTemplate relSys depsRootAbs absSys projAbs buildDir fp zonName mergedSpec resolvedZigs)

addImplicitStdDependency :: FilePath -> BuildSpec.BuildSpec -> BuildSpec.BuildSpec
addImplicitStdDependency sys spec
  | BuildSpec.specName spec == "std" = spec
  | M.member "std" deps = spec
  | otherwise = spec { BuildSpec.dependencies = M.insert "std" stdDep deps }
  where
    deps = BuildSpec.dependencies spec
    stdDep = BuildSpec.PkgDep Nothing Nothing (Just (joinPath [sys, "std"])) Nothing Nothing Nothing Nothing

applyPkgDepPathOverrides :: FilePath -> M.Map String FilePath -> BuildSpec.BuildSpec -> BuildSpec.BuildSpec
applyPkgDepPathOverrides projRoot depPathOverrides spec =
    spec { BuildSpec.dependencies = M.mapWithKey rewriteDep (BuildSpec.dependencies spec) }
  where
    rewriteDep depName dep =
      case M.lookup depName depPathOverrides of
        Nothing -> dep
        Just depPath ->
          dep { BuildSpec.path = Just (collapseDots (makeRelativeOrAbsolute projRoot depPath))
              , BuildSpec.follows = Nothing }

data ZigDepRef = ZigDepRef
  { zigDepRefName :: String
  , zigDepRefDep :: BuildSpec.ZigDep
  }

data ZigDepResolved = ZigDepResolved
  { zigDepResolvedVarName :: String
  , zigDepResolvedPkgName :: String
  , zigDepResolvedDep :: BuildSpec.ZigDep
  }

data ZigDepIdentity
  = ZigDepPathIdentity FilePath (M.Map String String)
  | ZigDepHashIdentity String (M.Map String String)
  | ZigDepUrlIdentity (Maybe String) (M.Map String String)
  deriving (Eq, Ord, Show)

zigDepIdentity :: BuildSpec.ZigDep -> ZigDepIdentity
zigDepIdentity dep =
  case localZigPath dep of
    Just p  -> ZigDepPathIdentity p (BuildSpec.options dep)
    Nothing -> case BuildSpec.zhash dep of
           Just h | not (null h) -> ZigDepHashIdentity h (BuildSpec.options dep)
           _ -> ZigDepUrlIdentity (BuildSpec.zurl dep) (BuildSpec.options dep)

mergeUniqueStrings :: [String] -> [String] -> [String]
mergeUniqueStrings xs ys =
    xs ++ [ y | y <- ys, y `notElem` xs ]

mergeZigDeps :: BuildSpec.ZigDep -> BuildSpec.ZigDep -> BuildSpec.ZigDep
mergeZigDeps dep0 dep1 =
    dep0 { BuildSpec.artifacts = mergeUniqueStrings (BuildSpec.artifacts dep0) (BuildSpec.artifacts dep1) }

dedupZigDepRefs :: [ZigDepRef] -> [(String, BuildSpec.ZigDep)]
dedupZigDepRefs refs =
    let (orderRev, byIdentity) = foldl' step ([], M.empty) refs
    in [ let ref = byIdentity M.! ident
         in (zigDepRefName ref, zigDepRefDep ref)
       | ident <- reverse orderRev
       ]
  where
    step (orderRev, byIdentity) ref =
      let ident = zigDepIdentity (zigDepRefDep ref)
      in case M.lookup ident byIdentity of
           Nothing ->
             (ident : orderRev, M.insert ident ref byIdentity)
           Just prevRef ->
             let mergedRef = prevRef { zigDepRefDep = mergeZigDeps (zigDepRefDep prevRef) (zigDepRefDep ref) }
             in (orderRev, M.insert ident mergedRef byIdentity)

nextAvailableName :: Data.Set.Set String -> String -> String
nextAvailableName usedNames baseName = go 0
  where
    go 0
      | Data.Set.member baseName usedNames = go 2
      | otherwise = baseName
    go n =
      let candidate = baseName ++ "_" ++ show n
      in if Data.Set.member candidate usedNames
           then go (n + 1)
           else candidate

pkgDepName :: String -> String
pkgDepName name = "acton_pkg_" ++ name

-- | Acton and Zig dependencies use separate package key prefixes. Distinct
-- Zig packages with the same logical name still need distinct keys.
resolveZigDepRefs :: [ZigDepRef] -> [ZigDepResolved]
resolveZigDepRefs refs =
    reverse resolvedRev
  where
    uniqueRefs = dedupZigDepRefs refs
    (_, _, resolvedRev) = foldl' assign (Data.Set.empty, Data.Set.empty, []) uniqueRefs

    assign (usedPkgNames, usedVarNames, acc) (depName, dep) =
      let pkgName = nextAvailableName usedPkgNames ("acton_zig_" ++ depName)
          varName = nextAvailableName usedVarNames depName
          resolved = ZigDepResolved
            { zigDepResolvedVarName = varName
            , zigDepResolvedPkgName = pkgName
            , zigDepResolvedDep = dep
            }
      in ( Data.Set.insert pkgName usedPkgNames
         , Data.Set.insert varName usedVarNames
         , resolved : acc
         )

genBuildZig :: String -> FilePath -> BuildSpec.BuildSpec -> [ZigDepResolved] -> M.Map String String -> String
genBuildZig template sys spec zigDeps depModuleOpts =
    let
        depsDefs = concatMap pkgDepDef (M.toList (BuildSpec.dependencies spec))
        zigDefs  = concatMap zigDepDef zigDeps
        depsAll  = depsDefs ++ zigDefs
        libLinks = concatMap pkgLibLink (M.toList (BuildSpec.dependencies spec))
                ++ concatMap zigLibLink zigDeps
        exeLinks = concatMap pkgExeLink (M.toList (BuildSpec.dependencies spec))
                ++ concatMap zigExeLink zigDeps
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
    sysDepsPath = sys </> "deps"

    pkgDepDef (name, dep) =
      let selectedCsv = M.findWithDefault "" name depModuleOpts
          -- Distribution packages have a checked-in build.zig instead of one
          -- generated from the builder template (see zigBuildDir). The only
          -- one that is a package dependency is std, which always builds all
          -- of its modules and, like base, declares only the options above.
          -- Zig reports any other option as invalid.
          moduleOpts
            | maybe False (isSysProject sys) (BuildSpec.path dep) = []
            | otherwise = [ "        .acton_modules = " ++ show selectedCsv ++ ","
                          , "        .acton_root_stubs = \"\","
                          ]
      in unlines $ [ "    const actdep_" ++ name ++ " = b.dependency(\"" ++ pkgDepName name ++ "\", .{"
                   , "        .target = target,"
                   , "        .optimize = optimize,"
                   , "        .no_threads = no_threads,"
                   , "        .db = db,"
                   , "        .gc_use_mark_bits = gc_use_mark_bits,"
                   , "        .gc_mark_bit_per_object = gc_mark_bit_per_object,"
                   , "        .gc_dirty_tracking_backend = gc_dirty_tracking_backend,"
                   , "        .gc_page_hash_table_log2 = gc_page_hash_table_log2,"
                   , "        .gc_heap_growth_divisor = gc_heap_growth_divisor,"
                   , "        .gc_alloc_budget_percent = gc_alloc_budget_percent,"
                   , "        .gc_memory_limit = gc_memory_limit,"
                   , "        .gc_heap_ceiling_percent = gc_heap_ceiling_percent,"
                   , "        .gc_block_size = gc_block_size,"
                   , "        .gc_mark_range_stealing = gc_mark_range_stealing,"
                   , "        .gc_bytes_per_marker = gc_bytes_per_marker,"
                   , "        .gc_initial_mark_stack_size = gc_initial_mark_stack_size,"
                   , "        .gc_no_end_padding = gc_no_end_padding,"
                   , "        .gc_thread_local_size_limit = gc_thread_local_size_limit,"
                   , "        .gc_realloc_no_free = gc_realloc_no_free,"
                   , "        .gc_no_thread_local_warmup = gc_no_thread_local_warmup,"
                   , "        .gc_thread_local_refill_blocks = gc_thread_local_refill_blocks,"
                   , "        .gc_disable_thp = gc_disable_thp,"
                   , "        .max_wthreads = max_wthreads,"
                   ]
                   ++ moduleOpts
                   ++ [ "    });" ]
    pkgLibLink (name, _) =
      "    libActonProject.root_module.linkLibrary(actdep_" ++ name ++ ".artifact(\"ActonProject\"));\n"
      ++ "    for (explicit_static_libraries.items) |libActonExplicit| {\n"
      ++ "        libActonExplicit.root_module.linkLibrary(actdep_" ++ name ++ ".artifact(\"ActonProject\"));\n"
      ++ "    }\n"
    pkgExeLink (name, _) = "            executable.root_module.linkLibrary(actdep_" ++ name ++ ".artifact(\"ActonProject\"));\n"

    zigDepDef resolved
      | null (BuildSpec.artifacts dep) = ""
      | otherwise =
          -- {sysdeps} is a sentinel a project can place in a zig dependency's
          -- options to receive the toolchain's bundled-deps path (<dist>/deps).
          -- In Build.act it is written "{{sysdeps}}" (Acton string escaping turns
          -- {{ }} into single braces, so it reaches us here as {sysdeps}). We
          -- substitute the quoted absolute path so the wrapper can, e.g., add an
          -- mbedtls include path without hardcoding the toolchain location.
          let opts = concat [ "        ." ++ k ++ " = " ++ replace "{sysdeps}" ("\"" ++ Zon.escapeString sysDepsPath ++ "\"") v ++ ",\n" | (k, v) <- M.toList (BuildSpec.options dep) ]
          in unlines [ "    const dep_" ++ zigDepResolvedVarName resolved ++ " = b.dependency(\"" ++ zigDepResolvedPkgName resolved ++ "\", .{"
                     , "        .target = target,"
                     , "        .optimize = optimize,"
                     , opts ++ "    });"
                     ]
      where dep = zigDepResolvedDep resolved
    zigLibLink resolved = concat [ "    libActonProject.root_module.linkLibrary(dep_" ++ zigDepResolvedVarName resolved ++ ".artifact(\"" ++ art ++ "\"));\n"
                                   ++ "    for (explicit_static_libraries.items) |libActonExplicit| {\n"
                                   ++ "        libActonExplicit.root_module.linkLibrary(dep_" ++ zigDepResolvedVarName resolved ++ ".artifact(\"" ++ art ++ "\"));\n"
                                   ++ "    }\n"
                                 | art <- BuildSpec.artifacts (zigDepResolvedDep resolved) ]
    zigExeLink resolved = concat [ "            executable.root_module.linkLibrary(dep_" ++ zigDepResolvedVarName resolved ++ ".artifact(\"" ++ art ++ "\"));\n"
                                 | art <- BuildSpec.artifacts (zigDepResolvedDep resolved) ]

-- Dependency paths in Build.act are relative to the project root, while zig
-- resolves the paths in build.zig.zon relative to the build directory, so we
-- rebase against the former and emit paths relative to the latter. An Acton
-- package dependency points at that package's own build directory.
genBuildZigZon :: String -> String -> FilePath -> FilePath -> FilePath -> FilePath -> String -> String -> BuildSpec.BuildSpec -> [ZigDepResolved] -> String
genBuildZigZon template relSys depsRootAbs sysAbs projAbs buildDir fingerprint zonName spec zigDepsResolved =
    let
        pkgDeps = concatMap (pkgToZon projAbs depsRootAbs) (M.toList (BuildSpec.dependencies spec))
        zigDeps = concatMap zigToZon zigDepsResolved
        deps = pkgDeps ++ zigDeps
        replaced = map (replace "{{fingerprint}}" fingerprint
                     . replace "{{syspath}}" (Zon.escapeString relSys)
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
      let rawPath = case localPath dep of
                      Just p  -> p
                      Nothing -> case BuildSpec.hash dep of
                             Just h -> joinPath (depCacheEntry depsRoot name h : maybeToList (BuildSpec.subdir dep))
                             Nothing -> errorWithoutStackTrace ("Dependency " ++ name ++ " has no path or hash")
          pathAbs = collapseDots $
                      if isAbsolutePath rawPath
                        then normalise rawPath
                        else normalise (rebasePath projRoot rawPath)
          path = relativeViaRoot buildDir (zigBuildDir sysAbs pathAbs)
      in unlines [ "        ." ++ pkgDepName name ++ " = .{"
                 , "            .path = \"" ++ Zon.escapeString path ++ "\","
                 , "        },"
                 ]
    zigToZon resolved =
      case BuildSpec.zpath dep of
        Just p ->
          let absPath = collapseDots $
                          if isAbsolutePath p
                            then normalise p
                            else normalise (rebasePath projAbs p)
              relPath = relativeViaRoot buildDir absPath
          in unlines [ "        ." ++ zigDepResolvedPkgName resolved ++ " = .{"
                     , "            .path = \"" ++ Zon.escapeString relPath ++ "\","
                     , "        },"
                     ]
        Nothing -> unlines [ "        ." ++ zigDepResolvedPkgName resolved ++ " = .{"
                           , "            .url = \"" ++ Zon.escapeString (maybeEmpty (BuildSpec.zurl dep)) ++ "\","
                           , "            .hash = \"" ++ Zon.escapeString (maybeEmpty (BuildSpec.zhash dep)) ++ "\","
                           , "        },"
                           ]
      where
        dep = zigDepResolvedDep resolved
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
zigBuild :: Acton.Env.Env0 -> C.GlobalOptions -> C.CompileOptions -> Paths -> BuildSpec.BuildSpec -> [A.ModName] -> [BinTask] -> Bool -> [FilePath] -> [BuildLibrary] -> M.Map String String -> M.Map String FilePath -> Maybe ProgressUI -> IO Bool
zigBuild env gopts opts paths rootSpec rootModules binTasks allowPrune moduleSources buildLibraries depModuleOpts depPathOverrides mProgressUI = do
    prepStart <- getTime Monotonic
    allBinTasks <- mapM (writeRootC env gopts opts paths) binTasks
    let realBinTasks = catMaybes allBinTasks

    let pruningAllowed = allowPrune && not (C.only_build opts)
    when pruningAllowed $
      removeUndeclaredRoots paths rootModules realBinTasks

    let no_threads = if isWindowsOS (C.target opts) then True else C.no_threads opts
    projAbs <- normalizePathSafe (projPath paths)
    sysAbs  <- normalizePathSafe (sysPath paths)

    -- Generate build.zig and build.zig.zon directly from Build.act.
    iff (not (isSysProject sysAbs projAbs)) $
      genBuildZigFiles rootSpec paths depModuleOpts depPathOverrides

    let zigExe = zigExecutable (sysPath paths)
        buildFile = zigBuildDir sysAbs projAbs </> "build.zig"
        baseArgs = ["build","--build-file", buildFile] ++
                   (if (C.verboseZig gopts) then ["--verbose"] else []) ++
                   (if C.timing gopts && not (quiet gopts opts) then ["--summary", "all", "--color", "off"] else [])
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
        moduleSourcesCsv = intercalate "," moduleSources
        rootStubsCsv = intercalate "," (selectedRootStubEntriesForBins paths realBinTasks)
        buildLibrariesSpec = encodeBuildLibraries buildLibraries
        moduleArgs = [ "-Dacton_modules=" ++ moduleSourcesCsv
                     , "-Dacton_root_stubs=" ++ rootStubsCsv
                     , "-Dacton_libraries=" ++ buildLibrariesSpec
                     -- Zig keys the configure cache on -D options and on
                     -- build root relative paths, see builder/build.zig
                     , "-Dacton_project_root=" ++ projAbs
                     ]
        featureArgs = concat [ if C.db opts then ["-Ddb"] else []
                             , if no_threads then ["-Dno_threads"] else []
                             , if C.cpedantic opts then ["-Dcpedantic"] else []
                             ]
        buildOptionArgs = ["-D" ++ key ++ "=" ++ value | (key, value) <- M.toList (BuildSpec.build_options rootSpec)]
        zigArgs = baseArgs ++ prefixArgs ++ targetArgs ++ cpuArgs ++ optArgs ++ moduleArgs ++ featureArgs ++ buildOptionArgs

    logTiming gopts opts (maybe putStrLn progressLogLine mProgressUI) "root and build file preparation" prepStart
    success <- runZig gopts opts zigExe zigArgs paths (Just (projPath paths)) mProgressUI
    -- if we are in a temp acton project, copy the outputted binary next to the source file
    if (success && isTmp paths && not (null realBinTasks))
      then do
        let baseName   = uncurry (rootBinaryName paths) (binTaskModule (head binTasks))
            exeName    = if isWindowsOS (C.target opts) then baseName ++ ".exe" else baseName
            srcBinFile = projectBinDir (projPath paths) </> exeName
            dstBinFile = joinPath [ binDir paths, exeName ]
        copyFile srcBinFile dstBinFile
      else return ()
    return success

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

-- | Collect build inputs from the canonical paths selected during discovery.
-- References and version conflicts have already been resolved in that graph.
collectDepsRecursive :: BuildSpec.BuildSpec -> FilePath -> M.Map String FilePath -> IO (M.Map String BuildSpec.PkgDep, [ZigDepRef])
collectDepsRecursive rootSpec projDir depPaths = do
  root <- normalizePathSafe projDir
  (\(_, pkgs, zigs) -> (pkgs, zigs)) <$> foldM (step root)
    (Data.Set.singleton root, M.empty, []) (M.toList (BuildSpec.dependencies rootSpec))
  where
    go root seen dir = do
      spec <- loadBuildSpec dir
      let zigsHere = [ ZigDepRef depName (rebaseZigDep root dir dep)
                     | (depName, dep) <- M.toList (BuildSpec.zig_dependencies spec)
                     ]
      foldM (step root) (seen, M.empty, zigsHere) (M.toList (BuildSpec.dependencies spec))

    step root (seen, pkgAcc, zigAcc) (depName, dep) = do
      depBase <- case M.lookup depName depPaths of
                   Just dir -> return dir
                   Nothing -> throwProjectError ("Missing selected dependency '" ++ depName ++ "' for " ++ root)
      let dep' = dep { BuildSpec.path = Just (collapseDots (makeRelativeOrAbsolute root depBase))
                     , BuildSpec.follows = Nothing }
          pkgAcc' = M.insertWith (\_ old -> old) depName dep' pkgAcc
      if Data.Set.member depBase seen
        then return (seen, pkgAcc', zigAcc)
        else do
          (seenNext, subPkgs, subZigs) <- go root (Data.Set.insert depBase seen) depBase
          return (seenNext, pkgAcc' `M.union` subPkgs, zigAcc ++ subZigs)

rebaseZigDep :: FilePath -> FilePath -> BuildSpec.ZigDep -> BuildSpec.ZigDep
rebaseZigDep root base dep =
  case localZigPath dep of
    Just p ->
      let absP = rebasePath base p
          relP = makeRelativeOrAbsolute root absP
      in dep { BuildSpec.zpath = Just (collapseDots relP) }
    Nothing -> dep

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
      case localPath dep of
        Just p -> do
          if isAbsolutePath p
            then do p' <- normalizePathSafe p
                    return dep { BuildSpec.path = Just p' }
            else return dep { BuildSpec.path = Just (collapseDots (normalise p)) }
        Nothing -> return dep

    normalizeZigDeps b = fmap M.fromList . mapM (\(k,v) -> do v' <- normalizeZigDep b v; return (k,v')) . M.toList

-- | Normalize a zig dependency path entry.
normalizeZigDep :: FilePath -> BuildSpec.ZigDep -> IO BuildSpec.ZigDep
normalizeZigDep b dep =
  case localZigPath dep of
    Just p -> do
      if isAbsolutePath p
        then do p' <- normalizePathSafe p
                return dep { BuildSpec.zpath = Just p' }
        else return dep { BuildSpec.zpath = Just (collapseDots (normalise p)) }
    Nothing -> return dep

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
  , puWidthAware :: Bool
  , puMaxLines :: Int
  , puLinesRef :: IORef [String]
  , puVisibleRef :: IORef Int
  , puSpinnerRef :: IORef Int
  , puUseColor :: Bool
  , puCursorHiddenRef :: IORef Bool
  , puTickerRef :: IORef (Maybe ThreadId)
  , puTermProgress :: TermProgress
  , puTermSize :: TermSize
  , puLock :: MVar ()
  }

data ProgressKey
  = ProgressTaskKey TaskKey
  | ProgressFrontOutputKey TaskKey FrontOutputKind
  deriving (Eq, Ord, Show)

data ProgressState = ProgressState
  { psActive :: IORef (M.Map ProgressKey ProgressTask)
  , psOrder :: IORef [ProgressKey]
  }

data ProgressTask = ProgressTask
  { ptRenderLine :: Int -> BuildLineLayout
  , ptStart :: TimeSpec
  , ptProgress :: Maybe Double
  }

-- | Initialize terminal progress UI state and enablement.  It shows at most
-- compileParallelism gopts live lines.
initProgressUI :: C.GlobalOptions -> IO ProgressUI
initProgressUI gopts = do
    maxLines <- compileParallelism gopts
    tty <- hIsTerminalDevice stdout
    let widthAware = (tty || C.tty gopts) && not (C.quiet gopts)
        enabled = widthAware && not (C.noProgress gopts)
    useColorOut <- useColor gopts
    linesRef <- newIORef []
    visibleRef <- newIORef 0
    spinnerRef <- newIORef 0
    cursorHiddenRef <- newIORef False
    tickerRef <- newIORef Nothing
    termProgress <- initTermProgress gopts
    termSize <- initTermSize widthAware
    lock <- newMVar ()
    return ProgressUI
      { puEnabled = enabled
      , puWidthAware = widthAware
      , puMaxLines = maxLines
      , puLinesRef = linesRef
      , puVisibleRef = visibleRef
      , puSpinnerRef = spinnerRef
      , puUseColor = useColorOut
      , puCursorHiddenRef = cursorHiddenRef
      , puTickerRef = tickerRef
      , puTermProgress = termProgress
      , puTermSize = termSize
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

-- | Progress heartbeat interval in microseconds.
progressTickMicros :: Int
progressTickMicros = 80000

-- | Spinner frames used by the progress UI.
spinnerChars :: [Char]
spinnerChars = "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏"

-- | Hide or show the cursor during progress rendering.
setCursorHidden :: ProgressUI -> Bool -> IO ()
setCursorHidden ui hidden =
    when (puEnabled ui) $ do
      cur <- readIORef (puCursorHiddenRef ui)
      when (cur /= hidden) $ do
        putStr (if hidden then "\ESC[?25l" else "\ESC[?25h")
        writeIORef (puCursorHiddenRef ui) hidden

-- | Render the current progress lines with one terminal row per live line.
progressRenderUnlocked :: ProgressUI -> IO ()
progressRenderUnlocked ui = do
    (_, rows, cols) <- termSizeSync (puTermSize ui)
    allLines <- readIORef (puLinesRef ui)
    prevVisible <- readIORef (puVisibleRef ui)
    let renderWidth = safeLiveWidth cols
        visibleCap = min (puMaxLines ui) (max 0 (rows - 1))
        lines = take visibleCap allLines
        newVisible = length lines
    if newVisible == 0
      then do
        setCursorHidden ui False
        progressClearUnlocked ui
      else do
        when (prevVisible > 0) $
          replicateM_ prevVisible (putStr "\ESC[1A")
        spinnerIx <- readIORef (puSpinnerRef ui)
        let spinner = spinnerChars !! (spinnerIx `mod` length spinnerChars)
            useSpinner = progressPrefixWidth cols > 0
            prefix line
              | useSpinner = termFitAnsiRight renderWidth (' ' : spinner : ' ' : line)
              | otherwise = termFitAnsiRight renderWidth line
        let renderLine i = do
              putStr "\r\ESC[2K"
              when (i < newVisible) $
                putStr (prefix (lines !! i))
              putStr "\n"
        setCursorHidden ui True
        mapM_ renderLine [0 .. max prevVisible newVisible - 1]
        hFlush stdout
        writeIORef (puVisibleRef ui) (max prevVisible newVisible)

-- | Start the progress heartbeat thread when needed.
startProgressTicker :: ProgressUI -> ProgressState -> IO ()
startProgressTicker ui st =
    when (puEnabled ui) $ do
      m <- readIORef (puTickerRef ui)
      case m of
        Just _ -> return ()
        Nothing -> do
          tid <- forkIO (progressTickerLoop ui st)
          writeIORef (puTickerRef ui) (Just tid)

-- | Stop the progress heartbeat thread.
stopProgressTicker :: ProgressUI -> IO ()
stopProgressTicker ui = do
    m <- atomicModifyIORef' (puTickerRef ui) (\cur -> (Nothing, cur))
    forM_ m killThread

-- | Heartbeat loop that redraws when the terminal is resized.
progressTickerLoop :: ProgressUI -> ProgressState -> IO ()
progressTickerLoop ui st = do
    tid <- myThreadId
    let loop = do
          threadDelay progressTickMicros
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
      writeIORef (puLinesRef ui) lines
      if null lines then stopProgressTicker ui else startProgressTicker ui st
      progressRenderUnlocked ui

-- | Clear active task state and remove any rendered progress lines.
progressReset :: ProgressUI -> ProgressState -> IO ()
progressReset ui st = withProgressLock ui $ do
    writeIORef (psActive st) M.empty
    writeIORef (psOrder st) []
    writeIORef (puLinesRef ui) []
    termProgressClear (puTermProgress ui)
    when (puEnabled ui) $ do
      stopProgressTicker ui
      progressRenderUnlocked ui

-- | Mark a task as active in the progress UI.
progressStartTask :: ProgressUI -> ProgressState -> ProgressKey -> (Int -> BuildLineLayout) -> Maybe Double -> IO ()
progressStartTask ui st key renderLine mprog = withProgressLock ui $ do
    now <- getTime Monotonic
    modifyIORef' (psActive st) (M.insert key (ProgressTask renderLine now (fmap (\x -> max 0 (min 1 x)) mprog)))
    modifyIORef' (psOrder st) (\xs -> if key `elem` xs then xs else xs ++ [key])
    progressRefreshUnlocked ui st

-- | Update the rendered line for an active progress task.
progressUpdateTask :: ProgressUI -> ProgressState -> ProgressKey -> (Int -> BuildLineLayout) -> Maybe Double -> IO ()
progressUpdateTask ui st key renderLine mprog = withProgressLock ui $ do
    modifyIORef' (psActive st) (M.adjust (\task ->
      task { ptRenderLine = renderLine
           , ptProgress = fmap (\x -> max 0 (min 1 x)) mprog
           }) key)
    progressRefreshUnlocked ui st

-- | Remove a task from the progress UI and return the elapsed live-task time.
progressDoneTask :: ProgressUI -> ProgressState -> ProgressKey -> IO (Maybe TimeSpec)
progressDoneTask ui st key = withProgressLock ui $ do
    active <- readIORef (psActive st)
    now <- getTime Monotonic
    let mElapsed = fmap (\task -> diffTimeSpec now (ptStart task)) (M.lookup key active)
    modifyIORef' (psActive st) (M.delete key)
    modifyIORef' (psOrder st) (filter (/= key))
    progressRefreshUnlocked ui st
    return mElapsed

-- | Recompute and render progress lines from active tasks.
progressRefreshUnlocked :: ProgressUI -> ProgressState -> IO ()
progressRefreshUnlocked ui st = do
    active <- readIORef (psActive st)
    order <- readIORef (psOrder st)
    now <- getTime Monotonic
    (_, _, cols) <- termSizeSync (puTermSize ui)
    let renderCols = safeLiveWidth cols
        spinnerPrefixWidth = progressPrefixWidth cols
    let progressDone = "\ESC[48;5;24m"
        progressReset = "\ESC[0m"
        paintProgressLine mprog line =
          case mprog of
            Just frac0 | puUseColor ui ->
              let n = length line
                  frac = max 0 (min 1 frac0)
                  doneChars = max 0 (min n (floor (frac * fromIntegral n)))
                  (done, todo) = splitAt doneChars line
              in if null done
                   then todo
                   else progressDone ++ done ++ progressReset ++ todo
            _ -> line
        betterLine best Nothing = best
        betterLine Nothing cand = cand
        betterLine best@(Just (bestScore, _)) cand@(Just (candScore, _))
          | candScore > bestScore = cand
          | otherwise = best
        formatLine task =
          let elapsedPrecise = fmtTimePrecise (diffTimeSpec now (ptStart task))
              elapsedCompact = fmtTimeCompact (diffTimeSpec now (ptStart task))
              timerWidth = max timeMinWidth (length elapsedPrecise)
              liveCandidate timerRank timer =
                let timerCols = if null timer then 0 else 1 + timerWidth
                    bodyWidth = max 0 (renderCols - spinnerPrefixWidth - timerCols)
                    layout = ptRenderLine task bodyWidth
                    body = bllText layout
                    line = body ++ if null timer then "" else " " ++ padLeft timerWidth timer
                    score =
                      ( if bllAligned layout then 1 :: Int else 0
                      , if bllHasStatus layout then 1 :: Int else 0
                      , bllLabelCols layout
                      , timerRank
                      )
                in if null body || renderCols < spinnerPrefixWidth + length line
                     then Nothing
                     else Just (score, line)
              line =
                case foldl' betterLine Nothing
                       [ liveCandidate 2 elapsedPrecise
                       , liveCandidate 1 elapsedCompact
                       , liveCandidate 0 ""
                       ]
                of
                  Just (_, bestLine) -> bestLine
                  Nothing -> bllText (ptRenderLine task (max 0 (renderCols - spinnerPrefixWidth)))
          in paintProgressLine (ptProgress task) line
        lines = [ formatLine task | key <- order, Just task <- [M.lookup key active] ]
    when (puEnabled ui) $ do
      writeIORef (puLinesRef ui) lines
      if null lines then stopProgressTicker ui else startProgressTicker ui st
      progressRenderUnlocked ui
      termProgressHeartbeat (puTermProgress ui)

-- | Format project labels relative to the root for logs.
projectLabel :: FilePath -> FilePath -> String
projectLabel root proj =
    let rel = makeRelative root proj
    in if rel == "." || ".." `isPrefixOf` rel
         then takeFileName proj
         else rel
