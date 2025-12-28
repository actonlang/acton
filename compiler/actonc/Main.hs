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
import Control.Exception (throw,catch,displayException,finally,IOException,ErrorCall,try,SomeException,bracket,onException,evaluate)
import Control.Exception (bracketOnError)
import Control.Concurrent (forkIO, threadDelay)
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
                            compileFilesChanged sp gopts opts srcFiles True mChanged
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
            let runOnce mChanged = compileFilesChanged sp gopts opts [absFile] False mChanged
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
                let runOnce mChanged = compileFilesChanged sp gopts opts' [absFile] False mChanged
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
    compileFilesChanged sp gopts opts srcFiles allowPrune Nothing

compileFilesChanged :: Source.SourceProvider -> C.GlobalOptions -> C.CompileOptions -> [String] -> Bool -> Maybe [FilePath] -> IO ()
compileFilesChanged sp gopts opts srcFiles allowPrune mChangedPaths = do
    cwd <- getCurrentDirectory
    maybeRoot <- findProjectDir (takeDirectory (head srcFiles))
    let baseForOverrides = maybe cwd id maybeRoot
    depOverrides <- normalizeDepOverrides baseForOverrides (C.dep_overrides opts)
    let opts' = opts { C.dep_overrides = depOverrides }
        watchMode = C.watch opts'
        incremental = isJust mChangedPaths
        allowPrune' = allowPrune && not incremental
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
    let rootTasks   = [ gtTask t | t <- neededTasks, tkProj (gtKey t) == rootProj ]
        rootPins    = maybe M.empty BuildSpec.dependencies (projBuildSpec =<< M.lookup rootProj projMap)
    iff (C.listimports opts') $ do
        let module_imports = map (\t -> concat [ modNameToString (name t), ": "
                                               , (concat $ intersperse " " (map modNameToString (importsOf t))) ]) rootTasks
            output = concat $ intersperse "\n" module_imports
        putStrLn output
        System.Exit.exitSuccess
    backJobsRef <- newIORef []
    let callbacks = defaultCompileCallbacks
          { ccOnDiagnostics = \t optsT diags -> printDiagnostics gopts optsT diags
          , ccOnFrontResult = \_ _ fr -> forM_ (frFrontDone fr) putStrLn
          , ccOnBackJob = \job -> modifyIORef' backJobsRef (job :)
          , ccOnInfo = putStrLn
          }
    compileRes <- compileTasks sp gopts opts' pathsRoot rootProj neededTasks callbacks
    case compileRes of
      Left err -> do
        cleanup gopts opts' pathsRoot
        if watchMode
          then putStrLn (compileFailureMessage err)
          else printErrorAndExit (compileFailureMessage err)
      Right (env, hadErrors) -> do
        backJobs <- reverse <$> readIORef backJobsRef
        when (not (null backJobs) && not (C.only_build opts')) $ do
          nCaps <- getNumCapabilities
          let maxParallel = max 1 (if C.jobs gopts > 0 then C.jobs gopts else nCaps)
          runBackJobs gopts maxParallel (\mline -> maybe (return ()) putStrLn mline) backJobs
        if hadErrors
          then do
            cleanup gopts opts' pathsRoot
            unless watchMode System.Exit.exitFailure
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
                putStrLn "  Skipping final build step"
              else
                if C.test opts'
                  then do
                    testBinTasks <- catMaybes <$> mapM (filterMainActor env pathsRoot) preTestBinTasks
                    compileBins gopts opts' pathsRoot env rootTasks testBinTasks allowPrune'
                    putStrLn "Test executables:"
                    mapM_ (\t -> putStrLn (binName t)) testBinTasks
                  else do
                    compileBins gopts opts' pathsRoot env rootTasks preBinTasks allowPrune'
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
