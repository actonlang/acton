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
import Utils
import qualified Pretty
import qualified InterfaceFiles

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Exception (throw,catch,displayException,IOException,ErrorCall,try,SomeException)
import Control.Exception (bracketOnError)
import Control.Concurrent (forkIO)
import Control.Monad
import Data.Default.Class (def)
import Data.List.Split
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
    case arg of
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

defaultOpts   = C.CompileOptions False False False False False False False False False False False False False False
                                 False False False False False C.Debug False False False False
                                 "" "" "" C.defTarget "" False [] []

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


dump mn h txt      = putStrLn ("\n\n== " ++ h ++ ": " ++ modNameToString mn ++ " ================================\n" ++ txt
                      ++'\n':replicate (38 + length h + length (modNameToString mn)) '=' ++ "\n")

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

getModPath :: FilePath -> A.ModName -> FilePath
getModPath path mn =
     joinPath [path, joinPath $ init $ A.modPath mn]

-- Global caches (process‑wide) to reduce repeated .ty lookups during parallel builds
--
-- We deliberately create top‑level MVars via unsafePerformIO and mark them
-- NOINLINE so their initializer runs exactly once. Without NOINLINE, GHC could
-- inline or float the initializer, accidentally creating multiple MVars under
-- optimization. This guarantees a single cache per process.
--
-- Thread‑safety: all access goes through modifyMVar/modifyMVar_ so read/modify/
-- write is atomic. These caches are best‑effort for performance; correctness
-- does not depend on them. On a miss we fall back to reading .ty headers from
-- disk. After a successful compile we update ifaceHashCache so dependents in
-- this process see the new interface hash.
--
-- tyPathCache    :: ModName -> absolute .ty path (resolved from searchPath)
-- ifaceHashCache :: ModName -> current interface hash (from header or compile)
{-# NOINLINE tyPathCache #-}
tyPathCache :: MVar (M.Map A.ModName FilePath)
tyPathCache = unsafePerformIO (newMVar M.empty)

{-# NOINLINE ifaceHashCache #-}
ifaceHashCache :: MVar (M.Map A.ModName B.ByteString)
ifaceHashCache = unsafePerformIO (newMVar M.empty)

getTyFileCached :: [FilePath] -> A.ModName -> IO (Maybe FilePath)
getTyFileCached spaths mn = modifyMVar tyPathCache $ \m -> do
  case M.lookup mn m of
    Just p  -> return (m, Just p)
    Nothing -> do
      mty <- Acton.Env.findTyFile spaths mn
      case mty of
        Just p  -> return (M.insert mn p m, Just p)
        Nothing -> return (m, Nothing)

getIfaceHashCached :: Paths -> A.ModName -> IO (Maybe B.ByteString)
getIfaceHashCached paths mn = modifyMVar ifaceHashCache $ \m -> do
  case M.lookup mn m of
    Just ih -> return (m, Just ih)
    Nothing -> do
      mty <- getTyFileCached (searchPath paths) mn
      case mty of
        Just ty -> do
          hdrE <- (try :: IO a -> IO (Either SomeException a)) $ InterfaceFiles.readHeader ty
          case hdrE of
            Right (_srcH, ih, _impsH, _rootsH, _docH) -> return (M.insert mn ih m, Just ih)
            _ -> return (m, Nothing)
        Nothing -> return (m, Nothing)

updateIfaceHashCache :: A.ModName -> B.ByteString -> IO ()
updateIfaceHashCache mn ih = modifyMVar_ ifaceHashCache $ \m -> return (M.insert mn ih m)


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
    paths <- findPaths (joinPath [ curDir, name, "Acton.toml" ]) defaultOpts
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

                -- find all .act files in src/ directory, parse into tasks and
                -- submit for compilation
                curDir <- getCurrentDirectory
                paths <- findPaths (joinPath [ curDir, "Acton.toml" ]) opts
                srcDirExists <- doesDirectoryExist (srcDir paths)
                if not srcDirExists
                  then printErrorAndExit "Missing src/ directory"
                  else do
                    iff (not(C.quiet gopts)) $ do
                      putStrLn("Building project in " ++ projPath paths)
                    -- grab project lock
                    withFileLock (joinPath [projPath paths, ".actonc.lock"]) Exclusive $ \_ -> do
                      allFiles <- getFilesRecursive (srcDir paths)
                      let srcFiles = catMaybes $ map filterActFile allFiles
                      compileFiles gopts opts srcFiles True
                      -- After a project build, (re)generate the documentation index
                      generateProjectDocIndex gopts opts paths srcFiles

buildFile :: C.GlobalOptions -> C.CompileOptions -> FilePath -> IO ()
buildFile gopts opts file = do
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
            compileFiles gopts opts [file] False
          else do
            -- grab project lock
            let lock_file = joinPath [proj, ".actonc.lock"]
            withFileLock lock_file Exclusive $ \_ -> do
              compileFiles gopts opts [file] False
      Nothing -> do
        -- Not in a project, use scratch directory for compilation unless
        -- --tempdir is provided - then use that
        if (C.tempdir opts /= "")
          then do
            iff (not(C.quiet gopts)) $ do
              putStrLn("Building file " ++ file ++ " using temporary directory " ++ C.tempdir opts)
            compileFiles gopts opts [file] False
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
                compileFiles gopts (opts { C.tempdir = scratchDir }) [file] False
                unlockFile lock
  where
    handleNotExists :: IOException -> IO ()
    handleNotExists _ = return ()

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
    paths <- findPaths actPath defaultOpts
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
                paths <- findPaths filename defaultOpts
                env0 <- Acton.Env.initEnv (sysTypes paths) False
                Acton.Types.showTyFile env0 (modName paths) filename

            ".act" -> do
                let modname = A.modName $ map (replace ".act" "") $ splitOn "/" $ fileBody
                paths <- findPaths filename defaultOpts
                parsedRes <- parseActFile modname filename
                (_srcContent, parsed) <- case parsedRes of
                  Left diags -> do
                    printDiagnostics gopts defaultOpts diags
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

findTyFile spaths mn = go spaths
  where
    go []     = return Nothing
    go (p:ps) = do
      let fullPath = joinPath (p : A.modPath mn) ++ ".ty"
      exists <- doesFileExist fullPath
      if exists
        then return (Just fullPath)
        else go ps

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
      if nameToString n == "test_main"
        then outbase ++ ".test_root.c"
        else outbase ++ ".root.c"

binTaskRoot :: Paths -> BinTask -> FilePath
binTaskRoot paths binTask =
    let A.GName m _ = rootActor binTask
        outbase = outBase paths m
    in if isTest binTask then outbase ++ ".test_root.c" else outbase ++ ".root.c"

errsToDiagnostics :: String -> FilePath -> String -> [(SrcLoc, String)] -> [Diagnostic String]
errsToDiagnostics errKind filename src errs =
    [ Diag.actErrToDiagnostic errKind filename src loc msg | (loc, msg) <- errs ]

missingIfaceDiagnostics :: A.ModName -> String -> A.ModName -> [Diagnostic String]
missingIfaceDiagnostics ownerMn src missingMn =
    errsToDiagnostics "Compilation error" (modNameToFilename ownerMn) src
      [(NoLoc, "Type interface file not found or unreadable for " ++ modNameToString missingMn)]

parseActSource :: A.ModName -> FilePath -> String -> IO (Either [Diagnostic String] A.Module)
parseActSource mn actFile srcContent = do
  (Right <$> Acton.Parser.parseModule mn actFile srcContent)
    `catch` handleParseBundle
    `catch` handleCustomParse
    `catch` handleContextError
    `catch` handleIndentationError
  where
    handleParseBundle :: ParseErrorBundle String CustomParseError -> IO (Either [Diagnostic String] A.Module)
    handleParseBundle bundle =
      return $ Left [Diag.parseDiagnosticFromBundle actFile srcContent bundle]

    handleCustomParse :: Acton.Parser.CustomParseException -> IO (Either [Diagnostic String] A.Module)
    handleCustomParse err =
      return $ Left [Diag.customParseExceptionToDiagnostic actFile srcContent err]

    handleContextError :: Acton.Parser.ContextError -> IO (Either [Diagnostic String] A.Module)
    handleContextError err =
      return $ Left (errsToDiagnostics "Context error" (modNameToFilename mn) srcContent (Acton.Parser.contextError err))

    handleIndentationError :: Acton.Parser.IndentationError -> IO (Either [Diagnostic String] A.Module)
    handleIndentationError err =
      return $ Left (errsToDiagnostics "Indentation error" (modNameToFilename mn) srcContent (Acton.Parser.indentationError err))

parseActFile :: A.ModName -> FilePath -> IO (Either [Diagnostic String] (String, A.Module))
parseActFile mn actFile = do
  srcContent <- readFile actFile
  cwd <- getCurrentDirectory
  let displayFile = makeRelative cwd actFile
  emod <- parseActSource mn displayFile srcContent
  return $ fmap (\m -> (srcContent, m)) emod


{-
================================================================================
Build Pipeline Overview
================================================================================

We want the compiler to be fast. The primary principle by which to achieve this
is to avoid doing unnecessary work. Practically, this happens by caching
information in .ty files and only selectively reading what we need. We do not
eagerly load whole .ty files but rather read in "header information" such as
which imports, the hash of the source code, which root actors and docstring.
This way we can quickly determine what needs to be recompiled and what can be
reused from previous compilations. We also use content hashing of the public
interface and imports to make rebuild decisions precise and transitive:
- Public interface hash is computed from the doc-free NameInfo, so doc-only
  edits never cause dependents to rebuild.
- The interface hash is augmented with the interface hashes of the module’s
  imports (sorted). If an import’s interface changes, this module’s interface
  hash changes too, which cleanly propagates rebuilds.

Terminology
- ActonTask: a module parsed from source (.act) and that needs to be compiled
- TyTask: a module loaded from the cached .ty file on disk. Note how there are
  two variants, a stubbed TyTask where only header fields are loaded and the
  full TyTask where all module content is available.

High-level Steps
1) Discover and read tasks using header-first strategy (readModuleTask)
   - For each module, try to use its .ty header to avoid parsing:
     - If .ty is missing/unreadable → parse .act to obtain imports (ActonTask).
     - If .ty exists and both .act and actonc mtime <= .ty mtime → trust .ty
       header imports and create a TyTask stub (no heavy decode) for graph
       building. Use --ignore-compiler-version to skip the actonc part.
     - If .act appears newer than .ty → verify by content hash:
       – If stored srcHash == current srcHash → header is still valid (TyTask)
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
     - Source changed (ActonTask) → compile now
     - Otherwise, compare each project import’s recorded interface hash from the
       dependent’s .ty header with the provider’s current interface hash. The
       current hash comes directly from the compile result for modules built in
       this run, or from a fresh provider’s .ty header. Only if a hash differs
       do we rebuild the dependent.
   - We maintain an ifaceMap while walking modules in topological order. After a
     module compiles, we insert its freshly computed interface hash; when a
     module is fresh (TyTask), we insert the recorded header hash. Dependents
     then consult ifaceMap to detect interface deltas among their imports.
   - TyTask items remain lazy; code that needs only small bits (e.g., writeRootC)
     reads roots/doc from the header instead of forcing heavy loads.
   - Final Zig C compilation...

================================================================================
-}

-- Resolve imports to in-graph providers using project search order and module index.
resolveProviders :: [FilePath] -> M.Map FilePath (Data.Set.Set A.ModName) -> [A.ModName] -> M.Map A.ModName TaskKey
resolveProviders order modSets imps =
    M.fromList $ catMaybes $ map (\mn -> fmap (\p -> (mn, TaskKey p mn)) (findProvider mn)) imps
  where
    findProvider mn = listToMaybe [ p | p <- order, maybe False (Data.Set.member mn) (M.lookup p modSets) ]

-- Build GlobalTask list for all discovered projects. Also returns a module index per project.
buildGlobalTasks :: C.GlobalOptions
                 -> C.CompileOptions
                 -> M.Map FilePath ProjCtx
                 -> Maybe [String]                  -- optional seed source files; Nothing = all modules
                 -> IO ([GlobalTask], M.Map FilePath (Data.Set.Set A.ModName))
buildGlobalTasks gopts opts projMap mSeeds = do
    perProj <- forM (M.elems projMap) $ \ctx -> do
                  mods <- enumerateProjectModules ctx
                  return (ctx, mods)
    let modMaps = M.fromList [ (projRoot ctx, M.fromList [ (mn, actFile) | (actFile, mn) <- mods ]) | (ctx, mods) <- perProj ]
        modSets = M.map Data.Set.fromList (M.map M.keys modMaps)
        orderCache = M.fromList [ (projRoot ctx, projRoot ctx : projDepClosure projMap (projRoot ctx)) | (ctx, _) <- perProj ]
        allKeys = [ TaskKey (projRoot ctx) mn | (ctx, mods) <- perProj, (_, mn) <- mods ]
    seedKeys <- case mSeeds of
                  Nothing -> return allKeys
                  Just files -> do
                    absFiles <- mapM canonicalizePath files
                    let pathIndex = M.fromList [ (actFile, TaskKey (projRoot ctx) mn) | (ctx, mods) <- perProj, (actFile, mn) <- mods ]
                        found = mapMaybe (`M.lookup` pathIndex) absFiles
                    return (if null found then allKeys else found)
    tasks <- go modMaps modSets orderCache Data.Set.empty seedKeys []
    return (reverse tasks, modSets)
  where
    go modMaps modSets orderCache seen [] acc = return acc
    go modMaps modSets orderCache seen (k:qs) acc
      | Data.Set.member k seen = go modMaps modSets orderCache seen qs acc
      | otherwise =
          case M.lookup (tkProj k) modMaps >>= M.lookup (tkMod k) of
            Nothing -> go modMaps modSets orderCache (Data.Set.insert k seen) qs acc
            Just actFile -> do
              let ctx = projMap M.! tkProj k
              paths <- pathsForModule opts projMap ctx (tkMod k)
              task  <- readModuleTask gopts opts paths actFile
              let order = M.findWithDefault [tkProj k] (tkProj k) orderCache
                  providers = resolveProviders order modSets (importsOf task)
                  newKeys = M.elems providers
                  acc' = GlobalTask { gtKey = k
                                    , gtPaths = paths
                                    , gtTask = task
                                    , gtImportProviders = providers
                                    } : acc
              go modMaps modSets orderCache (Data.Set.insert k seen) (qs ++ newKeys) acc'


compileFiles :: C.GlobalOptions -> C.CompileOptions -> [String] -> Bool -> IO ()
compileFiles gopts opts srcFiles allowPrune = do
    cwd <- getCurrentDirectory
    maybeRoot <- findProjectDir (takeDirectory (head srcFiles))
    let baseForOverrides = maybe cwd id maybeRoot
    depOverrides <- normalizeDepOverrides baseForOverrides (C.dep_overrides opts)
    let opts' = opts { C.dep_overrides = depOverrides }
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
    let lookupTaskKey ts f = do
          absF <- canonicalizePath f
          let byPath = listToMaybe [ gtKey t
                                   | t <- ts
                                   , let k = gtKey t
                                         pths = gtPaths t
                                   , srcFile pths (tkMod k) == absF
                                   ]
          case byPath of
            Just k -> return (Just k)
            Nothing -> do
              mn <- moduleNameFromFile (srcDir pathsRoot) absF
              return $ listToMaybe [ gtKey t
                                   | t <- ts
                                   , tkProj (gtKey t) == rootProj
                                   , tkMod (gtKey t) == mn
                                   ]
    (globalTasks, _) <- buildGlobalTasks gopts opts' projMap (if allowPrune then Nothing else Just srcFiles)
    requestedKeys <- catMaybes <$> mapM (lookupTaskKey globalTasks) srcFiles
    let wantedNames   = map takeFileName srcFiles
        requestedKeys' = if null requestedKeys
                           then [ gtKey t
                                | t <- globalTasks
                                , takeFileName (srcFile (gtPaths t) (tkMod (gtKey t))) `elem` wantedNames
                                ]
                           else requestedKeys
        builtinKeys = [ gtKey t | t <- globalTasks, tkMod (gtKey t) == A.modName ["__builtin__"] ]
        startKeys   = if null requestedKeys' then map gtKey globalTasks else requestedKeys' ++ builtinKeys
        depMapSet   = M.fromList [ (gtKey t, Data.Set.fromList (M.elems (gtImportProviders t))) | t <- globalTasks ]
        neededKeys  = reachable depMapSet (Data.Set.fromList startKeys)
        neededTasks = [ t | t <- globalTasks, Data.Set.member (gtKey t) neededKeys ]
        rootTasks   = [ gtTask t | t <- neededTasks, tkProj (gtKey t) == rootProj ]
        rootPins    = maybe M.empty BuildSpec.dependencies (projBuildSpec =<< M.lookup rootProj projMap)
    iff (C.listimports opts') $ do
        let module_imports = map (\t -> concat [ modNameToString (name t), ": "
                                               , (concat $ intersperse " " (map modNameToString (importsOf t))) ]) rootTasks
            output = concat $ intersperse "\n" module_imports
        putStrLn output
        System.Exit.exitSuccess
    (env, hadErrors) <- compileTasks gopts opts' pathsRoot rootProj neededTasks
    when hadErrors $ do
      cleanup gopts opts' pathsRoot
      System.Exit.exitFailure
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
            compileBins gopts opts' pathsRoot env rootTasks testBinTasks allowPrune
            putStrLn "Test executables:"
            mapM_ (\t -> putStrLn (binName t)) testBinTasks
          else do
            compileBins gopts opts' pathsRoot env rootTasks preBinTasks allowPrune
  where
    reachable depMap start = go (Data.Set.toList start) Data.Set.empty
      where
        go [] seen = seen
        go (k:ks) seen =
          if Data.Set.member k seen
            then go ks seen
            else
              let deps = Data.Set.toList (M.findWithDefault Data.Set.empty k depMap)
              in go (deps ++ ks) (Data.Set.insert k seen)

-- Total build graph scheduler: this compiles all tasks across all projects,
-- i.e. dependencies of the main project are also compiled here, as well as
-- their dependencies, etc. We construct a large DAG of all modules to be
-- compiled and then compile in topological order using concurrent async
-- workers.
--
-- What we build the graph from:
--   - Input is a list of GlobalTask. Each carries a CompileTask (ActonTask or
--     TyTask), its Paths, and a pre-resolved provider map (gtImportProviders ::
--     Map ModName TaskKey) telling us which in-graph node satisfies a given
--     import. TaskKey is (projectRoot, modName).
--   - We always read an initial set of modules (either all src/ files in the
--     root project for a project build or the user-specified files). From those
--     keys we chase imports via gtImportProviders to pull in exactly the
--     reachable dependencies across projects. __builtin__ is compiled first if
--     present, i.e. we are building the base lib.
--
--   1) Construct a dependency graph over non-builtin tasks and topologically order it.
--   2) Compile __builtin__ first if present.
--   3) Walk modules in topo order, deciding per module whether to compile or reuse cached .ty,
--      and update the shared environment.
--   4) Track interface-hash changes and only rebuild when an imported module’s interface hash
--      differs from what the dependent recorded.
--
-- Key ideas (ActonTask vs TyTask caching):
--   - TyTask is lightweight, read from the .ty header: source hash, interface hash, imports
--     annotated with the iface hash that was used, roots, docstring. It avoids decoding heavy
--     sections.
--   - ActonTask carries parsed source and must be compiled.
--   - ifaceMap :: Map TaskKey ByteString is maintained during the run. For any already-processed
--     module (compiled or confirmed fresh) it holds the current interface hash. TyTask compares
--     its recorded import hashes against ifaceMap for in-graph deps, or against cached .ty headers
--     for external deps; any delta forces a recompile. altOutput on the root module also forces.
--   - When a TyTask is deemed stale (iface delta or altOutput root) we convert it to an ActonTask
--     by parsing the .act file, and then compile from there as normal; when it is fresh we do not
--     parse or compile, we just reuse the header and carry forward its recorded iface hash.
--
-- Scheduling:
--   - Critical-path heuristic (file size proxy) picks among ready tasks; runs up to job cap.
--   - Each worker uses an env snapshot; coordinator merges the resulting interface/doc and
--     updates indegrees/ready sets. Dependents receive iface hashes via ifaceMap.
--   - Non-root projects are compiled with depOpts (skip_build/test) while the root keeps user opts.
compileTasks :: C.GlobalOptions
             -> C.CompileOptions
             -> Paths                     -- root project paths (for alt output root selection)
             -> FilePath                  -- root project path
             -> [GlobalTask]
             -> IO (Acton.Env.Env0, Bool)
compileTasks gopts opts rootPaths rootProj tasks = do
    -- Reject cycles
    unless (null cycles) $
      printErrorAndCleanAndExit ("Cyclic imports: " ++ concatMap showTaskGraph cycles) gopts opts rootPaths

    -- Compile __builtin__ first if present anywhere in the graph
    case builtinOrder of
      [t] -> compileBuiltin t
      _ -> return ()

    baseEnv <- Acton.Env.initEnv builtinPath False

    costMap <- fmap M.fromList $ forM otherOrder $ \t -> do
                  let mn = name (gtTask t)
                      pth = gtPaths t
                      fp  = srcFile pth mn
                  ok <- doesFileExist fp
                  sz <- if ok then getFileSize fp else return 0
                  return (gtKey t, sz)
    let cwMap = computeCriticalWeights costMap

    nCaps <- getNumCapabilities
    let maxParallel = max 1 (if C.jobs gopts > 0 then C.jobs gopts else nCaps)

    (envFinal, hadErrors, backJobs) <- loop initialReady [] M.empty indeg pending0 baseEnv [] False maxParallel cwMap
    when (not (null backJobs) && not (C.only_build opts)) $
      runBackJobs gopts (orderBackJobs backJobs) maxParallel
    return (envFinal, hadErrors)
  where
    -- Basic maps/sets ----------------------------------------------------
    taskMap = M.fromList [ (gtKey t, t) | t <- tasks ]
    isBuiltinKey k = tkMod k == A.modName ["__builtin__"]
    builtinOrder = [ t | t <- tasks, isBuiltinKey (gtKey t) ]
    nonBuiltinTasks = [ t | t <- tasks, not (isBuiltinKey (gtKey t)) ]
    nonBuiltinKeys = Data.Set.fromList [ gtKey t | t <- nonBuiltinTasks ]
    depsOf t = nub [ d | d <- M.elems (gtImportProviders t), Data.Set.member d nonBuiltinKeys ]
    nodes    = [ (t, gtKey t, depsOf t) | t <- nonBuiltinTasks ]
    sccs     = stronglyConnComp nodes
    cycles   = [ ts | ts@(CyclicSCC _) <- sccs ]
    order    = [ t | AcyclicSCC t <- sccs ]
    showTaskGraph (CyclicSCC ts) = "\n" ++ concatMap fmt ts
    showTaskGraph _              = ""
    fmt t = tkProj (gtKey t) ++ ":" ++ modNameToString (name (gtTask t)) ++ " "

    otherOrder = order
    revMap :: M.Map TaskKey [TaskKey]
    revMap = foldl' (\acc (k, ds) -> foldl' (\a d -> M.insertWith (++) d [k] a) acc ds) M.empty (M.toList depMap)
    depMap :: M.Map TaskKey [TaskKey]
    depMap = M.fromList [ (gtKey t, depsOf t) | t <- order ]
    indeg  = M.map length depMap
    initialReady = [ k | (k,d) <- M.toList indeg, d == 0 ]
    pending0 = Data.Set.fromList (M.keys indeg)

    depOpts = opts { C.skip_build = True, C.test = False }
    optsFor k = if tkProj k == rootProj then opts else depOpts
    rootAlt = modName rootPaths

    builtinPath =
      case builtinOrder of
        (t:_) -> projTypes (gtPaths t)
        _     -> sysTypes rootPaths

    getMyCap = do
      tid <- myThreadId
      (cap, _) <- threadCapability tid
      return cap

    computeCriticalWeights :: M.Map TaskKey Integer -> M.Map TaskKey Integer
    computeCriticalWeights cm = cwMap
      where
        costOf m = M.findWithDefault 0 m cm
        depsOfK m = M.findWithDefault [] m revMap
        cwMap    = M.fromList [ (m, costOf m + max0 [ weight d | d <- depsOfK m ]) | m <- M.keys indeg ]
        weight m = M.findWithDefault 0 m cwMap
        max0 []  = 0
        max0 xs  = maximum xs

    dependentClosure :: TaskKey -> Data.Set.Set TaskKey
    dependentClosure k = go Data.Set.empty [k]
      where
        go seen [] = seen
        go seen (x:xs) =
          let ds = M.findWithDefault [] x revMap
              new = filter (`Data.Set.notMember` seen) ds
              seen' = foldl' (flip Data.Set.insert) seen new
          in go seen' (new ++ xs)

    compileBuiltin :: GlobalTask -> IO ()
    compileBuiltin t = do
      let bPaths = gtPaths t
          mn = name (gtTask t)
          optsBuiltin = optsFor (gtKey t)
          actFile = srcFile bPaths mn
          compileMsg = makeRelative (srcDir bPaths) actFile
      unless (C.only_build optsBuiltin) $
        case gtTask t of
          ParseErrorTask{ parseDiagnostics = diags } -> do
            printDiagnostics gopts optsBuiltin diags
            cleanup gopts optsBuiltin bPaths
            System.Exit.exitFailure
          TyTask{} -> return ()
          ActonTask{ src = srcContent, atree = m } -> do
            iff (not (quiet gopts optsBuiltin)) $
              putStrLn ("  Compiling " ++ compileMsg ++ " with " ++ show (C.optimize optsBuiltin))
            builtinEnv0 <- Acton.Env.initEnv (projTypes bPaths) True
            res <- runFrontPasses gopts optsBuiltin bPaths builtinEnv0 m srcContent (getIfaceHashCached bPaths)
            case res of
              Left diags -> do
                printDiagnostics gopts optsBuiltin diags
                cleanup gopts optsBuiltin bPaths
                System.Exit.exitFailure
              Right fr ->
                do
                  case frFrontDone fr of
                    Just line -> putStrLn line
                    Nothing -> pure ()
                  forM_ (frBackJob fr) $ \job -> do
                    finish <- runBackPasses gopts (bjOpts job) (bjPaths job) (bjInput job)
                    case finish of
                      Just line -> putStrLn line
                      Nothing -> pure ()

    -- One module ---------------------------------------------------------
    doOne :: Acton.Env.Env0 -> M.Map TaskKey B.ByteString -> TaskKey -> IO (TaskKey, Either [Diagnostic String] FrontResult)
    doOne envSnap ifaceMap key = do
      t <- case M.lookup key taskMap of
             Just x -> return x
             Nothing -> error ("Internal error: missing task for key " ++ show key)
      let paths = gtPaths t
          mn    = name (gtTask t)
          optsT = optsFor key
          providers = gtImportProviders t
          actFile = srcFile paths mn
          compileMsg = makeRelative (srcDir paths) actFile
          short8 bs   = take 8 (B.unpack $ Base16.encode bs)

          resolveImportHash m =
            case M.lookup m providers of
              Just depKey ->
                case M.lookup depKey ifaceMap of
                  Just h  -> return (Just h)
                  Nothing -> error ("Internal error: missing iface hash for dep " ++ modNameToString m)
              Nothing -> getIfaceHashCached paths m

      case gtTask t of
        ParseErrorTask{ parseDiagnostics = diags } -> return (key, Left diags)
        _ | C.only_build optsT -> do
              ifaceRes <- case gtTask t of
                            TyTask{ tyIfaceHash = h } -> readIfaceFromTy paths mn "" (Just h)
                            ActonTask{ src = srcContent } -> readIfaceFromTy paths mn srcContent Nothing
              case ifaceRes of
                Right (ifaceTE, mdoc, ih) -> do
                  updateIfaceHashCache mn ih
                  return (key, Right FrontResult { frIfaceTE = ifaceTE
                                                 , frDoc = mdoc
                                                 , frIfaceHash = ih
                                                 , frFrontDone = Nothing
                                                 , frBackJob = Nothing
                                                 })
                Left _ ->
                  return (key, Right FrontResult { frIfaceTE = []
                                                 , frDoc = Nothing
                                                 , frIfaceHash = B.empty
                                                 , frFrontDone = Nothing
                                                 , frBackJob = Nothing
                                                 })
        _ -> do
          needByImportsRes <- case gtTask t of
            TyTask{ tyImports = imps } -> do
              triples <- forM imps $ \(m, recorded) -> do
                curr <- resolveImportHash m
                case curr of
                  Just ih -> return (Right (m, recorded, ih))
                  Nothing -> return (Left (missingIfaceDiagnostics mn "" m))
              let (errs, resolved) = partitionEithers triples
              if null errs
                then do
                  let ds = [ (m, old, new) | (m, old, new) <- resolved, old /= new ]
                  return (Right (not (null ds), ds))
                else return (Left (concat errs))
            _ -> return (Right (False, []))

          case needByImportsRes of
            Left diags -> return (key, Left diags)
            Right (needByImports, deltas) -> do
              let needBySource = case gtTask t of { ActonTask{} -> True; _ -> False }
                  forceAlt    = altOutput optsT && mn == rootAlt
                  forceAlways = C.alwaysbuild optsT
                  needCompile = needBySource || needByImports || forceAlt || forceAlways
              if needCompile
                then do
                  when (C.verbose gopts) $ do
                    if needBySource
                      then putStrLn ("  Stale " ++ modNameToString mn ++ ": source changed")
                      else when needByImports $ do
                        let fmtDelta (m, old, new) = modNameToString m ++ " " ++ short8 old ++ " → " ++ short8 new
                        putStrLn ("  Stale " ++ modNameToString mn ++ ": iface changes in " ++ Data.List.intercalate ", " (map fmtDelta deltas))
                    cap <- getMyCap
                    putStrLn ("  Building [cap " ++ show cap ++ "] " ++ modNameToString mn ++ " (" ++ tkProj key ++ ")")
                  iff (not (quiet gopts optsT)) $
                    putStrLn ("  Compiling " ++ compileMsg ++ " with " ++ show (C.optimize optsT))
                  t' <- case gtTask t of
                          ActonTask{} -> return (gtTask t)
                          TyTask{}    -> do
                            parsedRes <- parseActFile mn actFile
                            case parsedRes of
                              Left diags -> return (ParseErrorTask mn diags)
                              Right (srcContent, m) -> return $ ActonTask mn srcContent m
                  case t' of
                    ParseErrorTask{ parseDiagnostics = diags } -> return (key, Left diags)
                    ActonTask{ src = srcContent, atree = m } -> do
                      res <- runFrontPasses gopts optsT paths envSnap m srcContent resolveImportHash
                      case res of
                        Left diags -> return (key, Left diags)
                        Right fr -> do
                          updateIfaceHashCache mn (frIfaceHash fr)
                          return (key, Right fr)
                    _ -> error ("Internal error: unexpected task " ++ show t')
                else do
                  when (C.verbose gopts) $
                    putStrLn ("  Fresh " ++ modNameToString mn ++ ": using cached .ty")
                  ifaceRes <- case gtTask t of
                                TyTask{ tyIfaceHash = h } -> readIfaceFromTy paths mn "" (Just h)
                                _ -> readIfaceFromTy paths mn "" Nothing
                  case ifaceRes of
                    Left diags -> return (key, Left diags)
                    Right (ifaceTE, mdoc, ih) -> do
                      updateIfaceHashCache mn ih
                      return (key, Right FrontResult { frIfaceTE = ifaceTE
                                                     , frDoc = mdoc
                                                     , frIfaceHash = ih
                                                     , frFrontDone = Nothing
                                                     , frBackJob = Nothing
                                                     })

    scheduleMore :: Int -> [TaskKey]
                 -> [(Async (TaskKey, Either [Diagnostic String] FrontResult), TaskKey)]
                 -> M.Map TaskKey B.ByteString -> Acton.Env.Env0 -> M.Map TaskKey Integer
                 -> IO ([TaskKey]
                       , [(Async (TaskKey, Either [Diagnostic String] FrontResult), TaskKey)])
    scheduleMore k rdy running res envSnap cw = do
      let prio m = M.findWithDefault 0 m cw
          rdySorted = Data.List.sortOn (Data.Ord.Down . prio) rdy
          (toStart, rdy') = splitAt k rdySorted
      new <- forM toStart $ \mn -> do
                a <- async (doOne envSnap res mn)
                return (a, mn)
      return (rdy', new ++ running)

    loop :: [TaskKey]
         -> [(Async (TaskKey, Either [Diagnostic String] FrontResult), TaskKey)]
         -> M.Map TaskKey B.ByteString
         -> M.Map TaskKey Int
         -> Data.Set.Set TaskKey
         -> Acton.Env.Env0
         -> [BackJob]
         -> Bool
         -> Int
         -> M.Map TaskKey Integer
         -> IO (Acton.Env.Env0, Bool, [BackJob])
    loop rdy running res ind pend envAcc backJobs hadErrors maxPar cw = do
      (rdy1, running1) <- scheduleMore (maxPar - length running) rdy running res envAcc cw
      if null running1 && null rdy1
        then if Data.Set.null pend
               then return (envAcc, hadErrors, backJobs)
               else printErrorAndCleanAndExit "Internal error: scheduler deadlock (non-empty pending with no ready)." gopts opts rootPaths
        else do
          (doneA, (mnDone, outcome)) <- waitAny $ map fst running1
          let running2 = filter ((/= doneA) . fst) running1
          case outcome of
            Left diags -> do
              printDiagnostics gopts (optsFor mnDone) diags
              let blocked = dependentClosure mnDone
                  pend2 = Data.Set.delete mnDone (pend `Data.Set.difference` blocked)
                  rdy2 = filter (`Data.Set.notMember` blocked) rdy1
              loop rdy2 running2 res ind pend2 envAcc backJobs True maxPar cw
            Right fr -> do
              case frFrontDone fr of
                Just line -> putStrLn line
                Nothing -> pure ()
              let res2  = M.insert mnDone (frIfaceHash fr) res
                  pend2 = Data.Set.delete mnDone pend
                  ind2  = case M.lookup mnDone revMap of
                            Nothing -> ind
                            Just ds -> foldl' (\m d -> M.adjust (\x -> x-1) d m) ind ds
                  newlyReady = [ m | m <- Data.Set.toList pend2
                                   , M.findWithDefault 0 m ind2 == 0
                                   , not (m `elem` rdy1)
                                   , not (m `elem` map snd running2)
                                   ]
                  rdy2 = rdy1 ++ newlyReady
                  envAcc' = Acton.Env.addMod (tkMod mnDone) (frIfaceTE fr) (frDoc fr) envAcc
                  backJobs' = maybe backJobs (:backJobs) (frBackJob fr)
              loop rdy2 running2 res2 ind2 pend2 envAcc' backJobs' hadErrors maxPar cw

    backJobKey :: BackJob -> TaskKey
    backJobKey job =
      TaskKey (projPath (bjPaths job)) (A.modname (biTypedMod (bjInput job)))

    orderBackJobs :: [BackJob] -> [BackJob]
    orderBackJobs jobs =
      Data.List.sortOn backJobKey jobs

    runBackJobs :: C.GlobalOptions -> [BackJob] -> Int -> IO ()
    runBackJobs _ [] _ = return ()
    runBackJobs gopts jobs maxPar = loopBack indexed [] M.empty 0
      where
        indexed = zip [0..] jobs

        loopBack pending running results nextIx = do
          let capacity = maxPar - length running
              (toStart, pending') = splitAt capacity pending
          new <- forM toStart $ \(ix, job) ->
                   async $ do
                     res <- runBackPasses gopts (bjOpts job) (bjPaths job) (bjInput job)
                     return (ix, res)
          let running' = running ++ new
          if null running' && null pending'
            then do
              (_, _) <- flushReady results nextIx
              return ()
            else do
              (doneA, (ix, res)) <- waitAny running'
              let running'' = filter (/= doneA) running'
                  results' = M.insert ix res results
              (results'', nextIx') <- flushReady results' nextIx
              loopBack pending' running'' results'' nextIx'

        flushReady :: M.Map Int (Maybe String) -> Int -> IO (M.Map Int (Maybe String), Int)
        flushReady res ix =
          case M.lookup ix res of
            Nothing -> return (res, ix)
            Just mline -> do
              case mline of
                Just line -> putStrLn line
                Nothing -> pure ()
              flushReady (M.delete ix res) (ix + 1)

-- Topologically order projects so dependencies come first.
topoProjects :: FilePath -> M.Map FilePath ProjCtx -> [FilePath]
topoProjects root ctxs = reverse (dfs Data.Set.empty [] root)
  where
    depsOf p = maybe [] (map snd . projDeps) (M.lookup p ctxs)

    dfs seen acc node
      | Data.Set.member node seen = acc
      | otherwise =
          let seen' = Data.Set.insert node seen
              acc'  = foldl' (\a n -> dfs seen' a n) acc (depsOf node)
          in node : acc'

-- Remove executables that no longer have corresponding root actors
removeOrphanExecutables :: FilePath -> FilePath -> [BinTask] -> IO ()
removeOrphanExecutables binDir projTypes binTasks = do
    -- Get all executables in bin directory
    binDirExists <- doesDirectoryExist binDir
    when binDirExists $ do
        binFiles <- listDirectory binDir
        forM_ binFiles $ \exeFile -> do
            -- Convert executable name back to module path format
            let exeName = takeBaseName exeFile  -- Remove .exe if present
                modPath = map (\c -> if c == '.' then '/' else c) exeName
                rootCFile = projTypes </> modPath <.> "root.c"
                testRootCFile = projTypes </> modPath <.> "test_root.c"

            -- Check if this executable is in the current binTasks
            let isCurrentBin = any (\t -> binName t == exeName) binTasks

            -- If not in current binTasks and no root.c file exists, remove the executable
            rootExists <- doesFileExist rootCFile
            testRootExists <- doesFileExist testRootCFile
            when (not isCurrentBin && not rootExists && not testRootExists) $ do
              let file_name = binDir </> exeFile
              removeFile (file_name)
                  `catch` handleNotExists
  where
        handleNotExists :: IOException -> IO ()
        handleNotExists _ = return ()


-- Paths handling -------------------------------------------------------------------------------------

data Paths      = Paths {
                    searchPath  :: [FilePath],
                    sysPath     :: FilePath,
                    sysTypes    :: FilePath,
                    projPath    :: FilePath,
                    projOut     :: FilePath,
                    projTypes   :: FilePath,
                    binDir      :: FilePath,
                    srcDir      :: FilePath,
                    isTmp       :: Bool,
                    fileExt     :: String,
                    modName     :: A.ModName
                  }

-- Per-project context used for multi-project orchestration.
-- Identified by projRoot (absolute path). Keeps directories and BuildSpec if present.
data ProjCtx = ProjCtx {
                     projRoot    :: FilePath,
                     projOutDir  :: FilePath,
                     projTypesDir:: FilePath,
                     projSrcDir  :: FilePath,
                     projSysPath :: FilePath,
                     projSysTypes:: FilePath,
                     projBuildSpec :: Maybe BuildSpec.BuildSpec,
                     projLocks    :: FilePath,
                     projDeps     :: [(String, FilePath)]          -- resolved dependency roots (abs paths)
                   } deriving (Show)

-- Discover all projects reachable from a root project by following Build.act/build.act.json dependencies.
-- Returns a map from project root to ProjCtx; skips duplicates.
discoverProjects :: FilePath -> FilePath -> [(String, FilePath)] -> IO (M.Map FilePath ProjCtx)
discoverProjects sysAbs rootProj depOverrides = do
    rootAbs <- normalizePathSafe rootProj
    rootSpec0 <- loadBuildSpec rootAbs
    rootSpec  <- traverse (applyDepOverrides rootAbs depOverrides) rootSpec0
    let rootPins = maybe M.empty BuildSpec.dependencies rootSpec
    go rootAbs Data.Set.empty M.empty rootPins rootAbs rootSpec
  where
    go root seen acc pins dir mSpec = do
      dirAbs <- normalizePathSafe dir
      if Data.Set.member dirAbs seen
        then return acc
        else do
          rawSpec <- case mSpec of
                       Just s | dirAbs == root -> return (Just s)
                       _ -> loadBuildSpec dirAbs
          mspec <- traverse (applyDepOverrides dirAbs depOverrides) rawSpec
          deps <- case mspec of
                    Nothing -> return []
                    Just spec -> forM (M.toList (BuildSpec.dependencies spec)) $ \(depName, dep) -> do
                                   let (chosenDep, conflict) =
                                         case M.lookup depName pins of
                                           Nothing -> (dep, Nothing)
                                           Just pinDep ->
                                             if pinDep == dep
                                               then (dep, Nothing)
                                               else (pinDep, Just dep)
                                   when (isJust conflict) $
                                     putStrLn ("Warning: dependency '" ++ depName ++ "' in " ++ dirAbs
                                               ++ " overridden by root pin")
                                   depBase <- resolveDepBase dirAbs depName chosenDep
                                   depAbs  <- normalizePathSafe depBase
                                   return (depName, depAbs)
          let outDir   = joinPath [dirAbs, "out"]
              typesDir = joinPath [outDir, "types"]
              srcDir'  = joinPath [dirAbs, "src"]
              lockPath = joinPath [dirAbs, ".actonc.lock"]
              ctx = ProjCtx { projRoot = dirAbs
                            , projOutDir = outDir
                            , projTypesDir = typesDir
                            , projSrcDir = srcDir'
                            , projSysPath = sysAbs
                            , projSysTypes = joinPath [sysAbs, "base", "out", "types"]
                            , projBuildSpec = mspec
                            , projLocks = lockPath
                            , projDeps = deps
                            }
              acc' = M.insert dirAbs ctx acc
              seen' = Data.Set.insert dirAbs seen
          foldM (step root seen' pins) acc' deps

    step root seen pins acc (_, depBase) =
      go root seen acc pins depBase Nothing

-- Given a FILE and optionally --syspath PATH:
-- 'sysPath' is the path to the system directory as given by PATH, defaulting to the actonc executable directory.
-- 'sysTypes' is directory "types" under 'sysPath'.
-- 'projPath' is the closest parent directory of FILE that contains an 'Acton.toml' file, or a temporary directory in "/tmp" if no such parent exists.
-- 'projOut' is directory "out" under 'projPath'.
-- 'projTypes' is directory "types" under 'projOut'.
-- 'binDir' is the directory prefix of FILE if 'projPath' is temporary, otherwise it is directory "bin" under 'projOut'
-- 'srcDir' is the directory prefix of FILE if 'projPath' is temporary, otherwise it is directory "src" under 'projPath'
-- 'fileExt' is file suffix of FILE.
-- 'modName' is the module name of FILE (its path after 'src' except 'fileExt', split at every '/')

srcFile                 :: Paths -> A.ModName -> FilePath
srcFile paths mn        = joinPath (srcDir paths : A.modPath mn) ++ ".act"

outBase                 :: Paths -> A.ModName -> FilePath
outBase paths mn        = joinPath (projTypes paths : A.modPath mn)

srcBase                 :: Paths -> A.ModName -> FilePath
srcBase paths mn        = joinPath (srcDir paths : A.modPath mn)


findProjectDir :: FilePath -> IO (Maybe FilePath)
findProjectDir path = do
    let projectFiles = ["Acton.toml", "Build.act", "build.act.json"]
    hasProjectFile <- or <$> mapM (\file -> doesFileExist (path </> file)) projectFiles
    hasSrcDir <- doesDirectoryExist (path </> "src")
    if hasProjectFile && hasSrcDir
        then return (Just path)
        else if path == takeDirectory path  -- Check if we're at root
            then return Nothing
            else findProjectDir (takeDirectory path)


findPaths               :: FilePath -> C.CompileOptions -> IO Paths
findPaths actFile opts  = do execDir <- takeDirectory <$> System.Environment.getExecutablePath
                             sysPath <- canonicalizePath (if null $ C.syspath opts then execDir ++ "/.." else C.syspath opts)
                             absSrcFile <- canonicalizePath actFile
                             (isTmp, projPath, dirInSrc) <- analyze (takeDirectory absSrcFile) []
                             let sysTypes = joinPath [sysPath, "base", "out", "types"]
                                 srcDir  = if isTmp then takeDirectory absSrcFile else joinPath [projPath, "src"]
                                 projOut = joinPath [projPath, "out"]
                                 projTypes = joinPath [projOut, "types"]
                                 binDir  = if isTmp then srcDir else joinPath [projOut, "bin"]
                                 modName = A.modName $ dirInSrc ++ [fileBody]
                             -- join the search paths from command line options with the ones found in the deps directory
                             depTypePaths <- if isTmp then return [] else collectDepTypePaths projPath (C.dep_overrides opts)
                             let sPaths = [projTypes] ++ depTypePaths ++ (C.searchpath opts) ++ [sysTypes]
                             createDirectoryIfMissing True binDir
                             createDirectoryIfMissing True projOut
                             createDirectoryIfMissing True projTypes
                             createDirectoryIfMissing True (getModPath projTypes modName)
                             return $ Paths sPaths sysPath sysTypes projPath projOut projTypes binDir srcDir isTmp fileExt modName
  where (fileBody,fileExt) = splitExtension $ takeFileName actFile

        analyze "/" ds  = do tmp <- canonicalizePath (C.tempdir opts)
                             return (True, tmp, [])
        analyze pre ds  = do let projectFiles = ["Acton.toml", "Build.act", "build.act.json"]
                             hasProjectFile <- or <$> mapM (\file -> doesFileExist (joinPath [pre, file])) projectFiles
                             hasSrcDir <- doesDirectoryExist (joinPath [pre, "src"])
                             if hasProjectFile && hasSrcDir
                                then case ds of
                                    [] -> return $ (False, pre, [])
                                    "src":dirs -> return $ (False, pre, dirs)
                                    "out":"types":dirs -> return $ (False, pre, dirs)
                                    _ -> error ("************* Source file is not in a valid project directory: " ++ joinPath ds)
                                else analyze (takeDirectory pre) (takeFileName pre : ds)

-- Module helpers for multi-project builds ---------------------------------------------------------

moduleNameFromFile :: FilePath -> FilePath -> IO A.ModName
moduleNameFromFile srcBase actFile = do
    base <- normalizePathSafe srcBase
    file <- normalizePathSafe actFile
    let rel = dropExtension (makeRelative base file)
    return $ A.modName (splitDirectories rel)

enumerateProjectModules :: ProjCtx -> IO [(FilePath, A.ModName)]
enumerateProjectModules ctx = do
    exists <- doesDirectoryExist (projSrcDir ctx)
    if not exists
      then return []
      else do
        files <- getFilesRecursive (projSrcDir ctx)
        let actFiles = filter (\f -> takeExtension f == ".act") files
        forM actFiles $ \f -> do
          mn <- moduleNameFromFile (projSrcDir ctx) f
          return (f, mn)

searchPathForProject :: C.CompileOptions -> M.Map FilePath ProjCtx -> ProjCtx -> [FilePath]
searchPathForProject opts projMap ctx =
    let deps = depTypePathsFromMap projMap (projRoot ctx)
    in [projTypesDir ctx] ++ deps ++ (C.searchpath opts) ++ [projSysTypes ctx]

pathsForModule :: C.CompileOptions -> M.Map FilePath ProjCtx -> ProjCtx -> A.ModName -> IO Paths
pathsForModule opts projMap ctx mn = do
    let sPaths = searchPathForProject opts projMap ctx
        bin = joinPath [projOutDir ctx, "bin"]
        src = projSrcDir ctx
        p = Paths sPaths (projSysPath ctx) (projSysTypes ctx) (projRoot ctx) (projOutDir ctx) (projTypesDir ctx) bin src False ".act" mn
    createDirectoryIfMissing True bin
    createDirectoryIfMissing True (projOutDir ctx)
    createDirectoryIfMissing True (projTypesDir ctx)
    createDirectoryIfMissing True (getModPath (projTypesDir ctx) mn)
    return p


-- Load BuildSpec from Build.act (preferred) or build.act.json if present
loadBuildSpec :: FilePath -> IO (Maybe BuildSpec.BuildSpec)
loadBuildSpec dir = do
    let actPath  = joinPath [dir, "Build.act"]
        jsonPath = joinPath [dir, "build.act.json"]
    actExists <- doesFileExist actPath
    if actExists
      then do
        content <- readFile actPath
        case BuildSpec.parseBuildAct content of
          Left err -> printErrorAndExit ("Failed to parse Build.act in " ++ dir ++ ":\n" ++ err)
          Right (spec, _, _) -> return (Just spec)
      else do
        jsonExists <- doesFileExist jsonPath
        if jsonExists
          then do
            json <- BL.readFile jsonPath
            case BuildSpec.parseBuildSpecJSON json of
              Left err   -> printErrorAndExit ("Failed to parse build.act.json in " ++ dir ++ ":\n" ++ err)
              Right spec -> return (Just spec)
          else return Nothing

-- Treat drive-letter paths as absolute too (for Windows hosts)
isAbsolutePath :: FilePath -> Bool
isAbsolutePath p =
    isAbsolute p ||
    case p of
      (c:':':_) -> isAlpha c
      _         -> False

-- Normalize a path without failing if it does not exist
normalizePathSafe :: FilePath -> IO FilePath
normalizePathSafe p = do
    res <- try (canonicalizePath p) :: IO (Either IOException FilePath)
    return $ either (const (normalise p)) id res

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

-- Collapse "." / ".." segments without touching leading ".." for relative paths
-- e.g. "a/b/../c/./d" -> "a/c/d"
collapseDots :: FilePath -> FilePath
collapseDots p =
    let parts = splitDirectories p
        (root, rest) = case parts of
                         (r:xs) | isRoot r -> (Just r, xs)
                         xs                -> (Nothing, xs)
        (revAcc, ups) = foldl step ([], 0) rest
        cleaned = replicate ups ".." ++ reverse revAcc
        prefix = maybe [] (\r -> [r]) root
    in joinPath (prefix ++ cleaned)
  where
    isRoot r = r == "/" || (length r >= 2 && r !! 1 == ':')
    step (acc, ups) comp =
      case comp of
        ""  -> (acc, ups)
        "." -> (acc, ups)
        ".." -> case acc of
                  (x:xs) | x /= ".." -> (xs, ups)
                  _                  -> (acc, ups + 1)
        _   -> (comp:acc, ups)

-- Rebase a relative path against a directory and normalize it
rebasePath :: FilePath -> FilePath -> FilePath
rebasePath base p
  | isAbsolutePath p = normalise p
  | otherwise        = normalise (joinPath [base, p])

-- Prefer a relative path when possible; otherwise keep the absolute target
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
          -- Different drives: fall back to relative from base root
          then joinPath (replicate (length bParts) ".." ++ (tDriveRaw : tParts))
          else rel
  where
    cleanParts = filter (\c -> not (null c) && c /= "/") . splitDirectories

-- Produce a relative path that first walks to the filesystem root and then
-- descends to the absolute target. This avoids embedding absolute paths while
-- still resolving to the same location.
relativeViaRoot :: FilePath -> FilePath -> FilePath
relativeViaRoot baseAbs targetAbs
  | not (isAbsolutePath targetAbs) = targetAbs
  | otherwise =
      let (bDriveRaw, bPath) = splitDrive (normalise baseAbs)
          (tDriveRaw, tPath) = splitDrive (normalise targetAbs)
          bDrive = map toLower bDriveRaw
          tDrive = map toLower tDriveRaw
      in if bDrive /= tDrive && (not (null bDrive) || not (null tDrive))
           then makeRelativeOrAbsolute baseAbs targetAbs  -- fall back if drives differ
           else
             let ups = replicate (length (cleanParts bPath)) ".."
                 tParts = cleanParts tPath
             in joinPath (ups ++ tParts)
  where
    cleanParts = filter (\c -> not (null c) && c /= "/") . splitDirectories

-- Compute dependency closure (pre-order) for a project using already discovered ProjCtx entries.
-- Returns dependency roots in a deterministic order, skipping duplicates.
projDepClosure :: M.Map FilePath ProjCtx -> FilePath -> [FilePath]
projDepClosure ctxs root = snd (go Data.Set.empty root)
  where
    go seen node =
      case M.lookup node ctxs of
        Nothing  -> (seen, [])
        Just ctx ->
          foldl' step (Data.Set.insert node seen, []) (projDeps ctx)
    step (seen, acc) (_, depRoot) =
      if Data.Set.member depRoot seen
        then (seen, acc)
        else
          let seen' = Data.Set.insert depRoot seen
              (seenNext, sub) = go seen' depRoot
          in (seenNext, acc ++ [depRoot] ++ sub)

-- Collect dependency type directories recursively using already-discovered ProjCtx map.
depTypePathsFromMap :: M.Map FilePath ProjCtx -> FilePath -> [FilePath]
depTypePathsFromMap ctxs root = snd (go Data.Set.empty root)
  where
    go seen node =
      case M.lookup node ctxs of
        Nothing  -> (seen, [])
        Just ctx ->
          foldl' step (Data.Set.insert node seen, []) (projDeps ctx)

    step (seen, acc) (_, depRoot) =
      case M.lookup depRoot ctxs of
        Nothing -> (Data.Set.insert depRoot seen, acc)
        Just depCtx ->
          if Data.Set.member depRoot seen
            then (seen, acc)
            else
              let seen' = Data.Set.insert depRoot seen
                  (seenNext, sub) = go seen' depRoot
              in (seenNext, acc ++ [projTypesDir depCtx] ++ sub)

-- Resolve dependency base directory from PkgDep, preferring path then cache/hash
resolveDepBase :: FilePath -> String -> BuildSpec.PkgDep -> IO FilePath
resolveDepBase base name dep =
    case BuildSpec.path dep of
      Just p | not (null p) -> normalizePathSafe (rebasePath base p)
      _ -> case BuildSpec.hash dep of
             Just h -> do
               home <- getHomeDirectory
               normalizePathSafe (joinPath [home, ".cache", "acton", "deps", name ++ "-" ++ h])
             Nothing -> printErrorAndExit ("Dependency " ++ name ++ " has no path or hash")

showPkgDep :: BuildSpec.PkgDep -> String
showPkgDep dep =
  let fields = catMaybes
                 [ fmap (\p -> "path=" ++ p) (BuildSpec.path dep)
                 , fmap (\h -> "hash=" ++ h) (BuildSpec.hash dep)
                 , fmap (\u -> "url="  ++ u) (BuildSpec.url dep)
                 , fmap (\u -> "repo_url=" ++ u) (BuildSpec.repo_url dep)
                 , fmap (\r -> "repo_ref=" ++ r) (BuildSpec.repo_ref dep)
                 ]
  in "{" ++ Data.List.intercalate "," fields ++ "}"

-- Recursively collect out/types paths for all dependencies declared in Build.act/build.act.json
collectDepTypePaths :: FilePath -> [(String, FilePath)] -> IO [FilePath]
collectDepTypePaths projDir overrides = do
  root <- normalizePathSafe projDir
  snd <$> go Data.Set.empty root Nothing
  where
    go seen dir mSpec = do
      mspec <- case mSpec of
                 Just s -> return (Just s)
                 Nothing -> loadBuildSpec dir
      mspec' <- traverse (applyDepOverrides dir overrides) mspec
      case mspec' of
        Nothing   -> return (seen, [])
        Just spec -> foldM (step dir) (seen, []) (M.toList (BuildSpec.dependencies spec))

    step base (seen, acc) (depName, dep) = do
      depBase <- resolveDepBase base depName dep
      let seen' = Data.Set.insert depBase seen
          typesDir = joinPath [depBase, "out", "types"]
      if Data.Set.member depBase seen
        then return (seen', acc)
        else do
          (seenNext, sub) <- go seen' depBase Nothing
          return (seenNext, acc ++ [typesDir] ++ sub)

-- Collect package and zig dependencies recursively, keeping existing entries on conflict
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
          -- Rebase dep path to project root and collapse redundant segments
          rebasePkgPath d =
            case BuildSpec.path d of
              Just p | not (null p) ->
                let absP = rebasePath base p
                    relP = makeRelativeOrAbsolute root absP
                in d { BuildSpec.path = Just (collapseDots relP) }
              _ -> d
          rebaseZigPath d =
            case BuildSpec.zpath d of
              Just p | not (null p) ->
                let absP = rebasePath base p
                    relP = makeRelativeOrAbsolute root absP
                in d { BuildSpec.zpath = Just (collapseDots relP) }
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

-- Fetch dependencies (pkg + zig) into Zig global cache and materialize pkg deps
-- into ~/.cache/acton/deps/<name>-<hash> when using URL-based dependencies.
-- depOverrides: optional path overrides for direct dependencies (NAME=PATH).
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

        -- Copy fetched pkg deps into ~/.cache/acton/deps/<name>-<hash> for discovery/build.
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

-- Normalize direct dependency paths in Build.act/build.act.json relative to a base directory.
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
normalizeZigDep b dep =
  case BuildSpec.zpath dep of
    Just p | not (null p) -> do
      if isAbsolutePath p
        then do p' <- normalizePathSafe p
                return dep { BuildSpec.zpath = Just p' }
        else return dep { BuildSpec.zpath = Just (collapseDots (normalise p)) }
    _ -> return dep

-- Resolve --dep overrides relative to a base directory, returning absolute
-- normalized paths. If an override path is already absolute we just normalize it.
normalizeDepOverrides :: FilePath -> [(String, FilePath)] -> IO [(String, FilePath)]
normalizeDepOverrides base overrides =
  mapM (\(n,p) -> do
           let absP0 = if isAbsolutePath p then p else joinPath [base, p]
           p' <- normalizePathSafe absP0
           return (n, p'))
       overrides

-- Apply --dep overrides (NAME=PATH) to a BuildSpec by forcing path on matching deps.
-- Paths are resolved relative to the given base directory when not absolute.
applyDepOverrides :: FilePath -> [(String, FilePath)] -> BuildSpec.BuildSpec -> IO BuildSpec.BuildSpec
applyDepOverrides base overrides spec = do
    deps' <- foldM applyOne (BuildSpec.dependencies spec) overrides
    return spec { BuildSpec.dependencies = deps' }
  where
    applyOne depsMap (depName, depPath) =
      case M.lookup depName depsMap of
        Nothing -> return depsMap
        Just dep -> do
          let absP0 = if isAbsolutePath depPath then depPath else joinPath [base, depPath]
          absP <- normalizePathSafe absP0
          let dep' = dep { BuildSpec.path = Just absP }
          return (M.insert depName dep' depsMap)

-- Handling Acton files -----------------------------------------------------------------------------

filterActFile :: FilePath -> Maybe FilePath
filterActFile file =
    case fileExt of
        ".act" -> Just file
        _ -> Nothing
  where (fileBody, fileExt) = splitExtension $ takeFileName file

-- Prepare a task for dependency graph construction.
-- Prefer reading imports from .ty header; if no .ty, parse the source.
-- Decide how to represent a module for the graph:
-- 1) If .ty is missing/unreadable -> parse .act to obtain imports (ActonTask).
-- 2) If .ty exists and both .act and actonc mtime <= .ty mtime -> trust header imports (TyTask).
--    (Use --ignore-compiler-version to skip the actonc check.)
-- 3) If actonc is newer than .ty -> recompile from source (ActonTask).
-- 4) If .act appears newer than .ty -> verify by content hash:
--      - If stored srcHash == current srcHash -> header is still valid (TyTask)
--      - Else -> parse .act to obtain accurate imports (ActonTask)
-- Returns either a header-only TyTask stub or an ActonTask; no heavy decoding.
readModuleTask :: C.GlobalOptions -> C.CompileOptions -> Paths -> String -> IO CompileTask
readModuleTask _gopts _opts paths actFile = do
    let mn      = modName paths
        tyFile  = outBase paths mn ++ ".ty"
    tyExists <- System.Directory.doesFileExist tyFile
    if not tyExists
      then parseForImports mn
      else do
        -- .ty exists: read header for hashes/imports and compare timestamps
        hdrE <- (try :: IO a -> IO (Either SomeException a)) $ InterfaceFiles.readHeader tyFile
        case hdrE of
          Left _ -> parseForImports mn
          Right (hash, ihash, imps, roots, mdoc) -> do
            actTime <- System.Directory.getModificationTime actFile
            tyTime  <- System.Directory.getModificationTime tyFile
            actoncTime <- if C.ignore_compiler_mtime opts
              then return Nothing
              else do
                actoncTimeE <- (try :: IO a -> IO (Either SomeException a)) $ do
                                 exe <- System.Environment.getExecutablePath
                                 System.Directory.getModificationTime exe
                return (either (const Nothing) Just actoncTimeE)
            let srcTime = maybe actTime (max actTime) actoncTime
                compilerNewer = maybe False (> tyTime) actoncTime
            if srcTime <= tyTime
              then do
                let nmodStub = A.NModule [] mdoc
                    tmodStub = A.Module mn [] []
                return $ TyTask { name      = mn
                                , tyHash    = hash
                                , tyIfaceHash = ihash
                                , tyImports = imps
                                , tyRoots   = roots
                                , tyDoc     = mdoc
                                , iface     = nmodStub
                                , typed     = tmodStub }
              else if compilerNewer
                then parseForImports mn
                else do
                  -- Verify by hash to handle touched-but-unchanged sources
                  srcBytes <- B.readFile actFile
                  let curHash = SHA256.hash srcBytes
                  if curHash == hash
                    then do
                      let nmodStub = A.NModule [] mdoc
                          tmodStub = A.Module mn [] []
                      return $ TyTask { name      = mn
                                      , tyHash    = hash
                                      , tyIfaceHash = ihash
                                      , tyImports = imps
                                      , tyRoots   = roots
                                      , tyDoc     = mdoc
                                      , iface     = nmodStub
                                      , typed     = tmodStub }
                    else parseForImports mn
  where
    parseForImports mn = do
      parsedRes <- parseActFile mn actFile
      case parsedRes of
        Left diags -> return $ ParseErrorTask mn diags
        Right (srcContent, m) -> return $ ActonTask mn srcContent m


-- Compilation tasks, chasing imported modules, compilation and building executables -------------------------------------------

data BackInput = BackInput
  { biTypeEnv   :: Acton.Env.Env0
  , biTypedMod  :: A.Module
  , biSrc       :: String
  , biSrcHash   :: B.ByteString
  , biFrontTime :: TimeSpec
  }

data BackJob = BackJob
  { bjPaths :: Paths
  , bjOpts  :: C.CompileOptions
  , bjInput :: BackInput
  }

data FrontResult = FrontResult
  { frIfaceTE  :: [(A.Name, A.NameInfo)]
  , frDoc      :: Maybe String
  , frIfaceHash :: B.ByteString
  , frFrontDone :: Maybe String
  , frBackJob  :: Maybe BackJob
  }

data CompileTask        = ActonTask { name :: A.ModName, src :: String, atree:: A.Module }
                        | TyTask    { name :: A.ModName
                                    , tyHash :: B.ByteString               -- source content hash
                                    , tyIfaceHash :: B.ByteString          -- public interface hash
                                    , tyImports :: [(A.ModName, B.ByteString)] -- imports with iface hash used
                                    , tyRoots :: [A.Name]
                                    , tyDoc :: Maybe String
                                    , iface :: A.NameInfo
                                    , typed :: A.Module
                                    }
                        | ParseErrorTask { name :: A.ModName, parseDiagnostics :: [Diagnostic String] }

instance Show CompileTask where
  show ActonTask{ name = mn } = "ActonTask " ++ modNameToString mn
  show TyTask{ name = mn } = "TyTask " ++ modNameToString mn
  show ParseErrorTask{ name = mn, parseDiagnostics = ds } =
    "ParseErrorTask " ++ modNameToString mn ++ " (" ++ show (length ds) ++ " diagnostics)"

-- TODO: replace binName String type with ModName just like for CompileTask.
-- ModName is a array so a hierarchy with submodules is represented, we can then
-- get it use joinPath (modPath) to get a path or modName to get a string
-- representation. We need both of BinTask when generating build.zig, so it
-- would be more robust to use that type rather than a hacky character
-- replacement (replaceDot in genBuildZigExe)
data BinTask            = BinTask { isDefaultRoot :: Bool, binName :: String, rootActor :: A.QName, isTest :: Bool } deriving (Show)

-- Global multi-project task identity and payload
data TaskKey = TaskKey { tkProj :: FilePath, tkMod :: A.ModName } deriving (Eq, Ord, Show)

data GlobalTask = GlobalTask
  { gtKey             :: TaskKey
  , gtPaths           :: Paths
  , gtTask            :: CompileTask
  , gtImportProviders :: M.Map A.ModName TaskKey   -- resolved in-graph providers (if any) for imports
  }


-- Shared predicate: is a NameInfo root-eligible (actor with env/no params)?
rootEligible :: A.NameInfo -> Bool
rootEligible (A.NAct [] p k _ _) = case (p,k) of
                                              (A.TNil{}, A.TRow _ _ _ t A.TNil{}) ->
                                                prstr t == "Env" || prstr t == "None" ||
                                                prstr t == "__builtin__.Env" || prstr t == "__builtin__.None"
                                              _ -> False
rootEligible _ = False

-- return task where the specified root actor exists
filterMainActor :: Acton.Env.Env0 -> Paths -> BinTask -> IO (Maybe BinTask)
filterMainActor env paths binTask = do
    -- Prefer .ty header roots; fall back to checking the in-memory env
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
  where

importsOf :: CompileTask -> [A.ModName]
importsOf (ActonTask _ _ m) = A.importsOf m
importsOf (TyTask { tyImports = ms }) = map fst ms
importsOf ParseErrorTask{} = []
compileBins:: C.GlobalOptions -> C.CompileOptions -> Paths -> Acton.Env.Env0 -> [CompileTask] -> [BinTask] -> Bool -> IO ()
compileBins gopts opts paths env tasks binTasks allowPrune = do
    iff (not (altOutput opts)) $ do
      zigBuild env gopts opts paths tasks binTasks allowPrune
    return ()


-- Generate documentation index for a project build by reading module docstrings
-- from the current tasks. Uses TyTask header docs when available to avoid
-- parsing/decoding; falls back to extracting from ActonTask ASTs.
generateProjectDocIndex :: C.GlobalOptions -> C.CompileOptions -> Paths -> [String] -> IO ()
generateProjectDocIndex gopts opts paths srcFiles = do
    unless (C.skip_build opts || C.only_build opts || isTmp paths) $ do
        let docDir = joinPath [projPath paths, "out", "doc"]
        createDirectoryIfMissing True docDir
        tasks <- mapM (\f -> findPaths f opts >>= \p -> readModuleTask gopts opts p f) srcFiles
        entries <- catMaybes <$> forM tasks (\t -> case t of
                          ActonTask mn _src m -> return (Just (mn, DocP.extractDocstring (A.mbody m)))
                          TyTask { name = mn, tyDoc = mdoc } -> return (Just (mn, mdoc))
                          ParseErrorTask{} -> return Nothing)
        DocP.generateDocIndex docDir entries


-- Expand the set of tasks by reading project-local imports recursively.
-- Uses readModuleTask so we always load modules via .ty header (when valid) or
-- parse .act as needed. For full project builds (all src/), this is typically a
-- no-op; it matters when specific source files are specified
readImports :: C.GlobalOptions -> C.CompileOptions -> Paths -> [CompileTask] -> IO [CompileTask]
readImports gopts opts paths itasks
                            = do
                                 let itasks_imps = concatMap importsOf itasks
                                 newtasks <- catMaybes <$> mapM (readAFile itasks) itasks_imps
                                 chaseRecursively (itasks ++ newtasks) (map name newtasks) (concatMap importsOf newtasks)

  where
        readAFile tasks mn = case lookUp mn tasks of
          Just _ -> return Nothing
          Nothing -> do
            let actFile = srcFile paths mn
            ok <- System.Directory.doesFileExist actFile
            if ok
              then do
                p <- findPaths actFile opts
                task <- readModuleTask gopts opts p actFile
                return (Just task)
              else return Nothing

        lookUp mn (t : ts)
          | name t == mn     = Just t
          | otherwise        = lookUp mn ts
        lookUp _ []          = Nothing

        chaseRecursively tasks mns []
                             = return tasks
        chaseRecursively tasks mns (imn : imns)
                             = if imn `elem` mns
                                then chaseRecursively tasks mns imns
                                else do t <- readAFile tasks imn
                                        chaseRecursively (maybe tasks (:tasks) t)
                                                         (imn:mns)
                                                         (imns ++ concatMap importsOf t)


quiet :: C.GlobalOptions -> C.CompileOptions -> Bool
quiet gopts opts = C.quiet gopts || altOutput opts

readIfaceFromTy :: Paths -> A.ModName -> String -> Maybe B.ByteString -> IO (Either [Diagnostic String] ([(A.Name, A.NameInfo)], Maybe String, B.ByteString))
readIfaceFromTy paths mn src mHash = do
    mty <- Acton.Env.findTyFile (searchPath paths) mn
    case mty of
      Nothing -> return $ Left (missingIfaceDiagnostics mn src mn)
      Just tyF -> do
        fileRes <- (try :: IO a -> IO (Either SomeException a)) $ InterfaceFiles.readFile tyF
        case fileRes of
          Left _ -> return $ Left (missingIfaceDiagnostics mn src mn)
          Right (_ms, nmod, _tmod, _si, _ti, _ni, _te, _tm) -> do
            let A.NModule te mdoc = nmod
            ih <- case mHash of
                    Just h -> return h
                    Nothing -> do
                      hdrE <- (try :: IO a -> IO (Either SomeException a)) $ InterfaceFiles.readHeader tyF
                      case hdrE of
                        Right (_srcH, ihash, _impsH, _rootsH, _docH) -> return ihash
                        _ -> return B.empty
            return $ Right (te, mdoc, ih)

runFrontPasses :: C.GlobalOptions
               -> C.CompileOptions
               -> Paths
               -> Acton.Env.Env0
               -> A.Module
               -> String
               -> (A.ModName -> IO (Maybe B.ByteString))
               -> IO (Either [Diagnostic String] FrontResult)
runFrontPasses gopts opts paths env0 parsed srcContent resolveImportHash = do
  createDirectoryIfMissing True (getModPath (projTypes paths) mn)
  core
    `catch` handleGeneral
    `catch` handleCompilation
    `catch` handleTypeError
  where
    mn = A.modname parsed
    filename = modNameToFilename mn
    outbase = outBase paths mn
    absSrcBase = srcBase paths mn
    actFile = absSrcBase ++ ".act"
    prettyAstStyle = style { mode = PageMode, lineLength = 120, ribbonsPerLine = 1.0 }

    handleGeneral :: GeneralError -> IO (Either [Diagnostic String] FrontResult)
    handleGeneral err =
      return $ Left (errsToDiagnostics "Compilation error" filename srcContent (generalError err))

    handleCompilation :: Acton.Env.CompilationError -> IO (Either [Diagnostic String] FrontResult)
    handleCompilation err =
      return $ Left (errsToDiagnostics "Compilation error" filename srcContent (Acton.Env.compilationError err))

    handleTypeError :: TypeError -> IO (Either [Diagnostic String] FrontResult)
    handleTypeError err =
      return $ Left [mkErrorDiagnostic filename srcContent (Acton.TypeM.typeReport err filename srcContent)]

    resolveImportHashes :: [A.ModName] -> IO (Either [Diagnostic String] [(A.ModName, B.ByteString)])
    resolveImportHashes mrefs = do
      resolved <- forM mrefs $ \mref -> do
        mh <- resolveImportHash mref
        case mh of
          Just ih -> return (Right (mref, ih))
          Nothing -> return (Left (missingIfaceDiagnostics mn srcContent mref))
      let (errs, vals) = partitionEithers resolved
      if null errs
        then return (Right vals)
        else return (Left (concat errs))

    core = do
      timeStart <- getTime Monotonic
      when (C.parse opts && mn == (modName paths)) $
        dump mn "parse" (Pretty.print parsed)
      when (C.parse_ast opts && mn == (modName paths)) $
        dump mn "parse-ast" (renderStyle prettyAstStyle (ppDoc parsed))

      env <- Acton.Env.mkEnv (searchPath paths) env0 parsed
      timeEnv <- getTime Monotonic
      iff (C.timing gopts) $ putStrLn("    Pass: Make environment: " ++ fmtTime (timeEnv - timeStart))

      kchecked <- Acton.Kinds.check env parsed
      iff (C.kinds opts && mn == (modName paths)) $ dump mn "kinds" (Pretty.print kchecked)
      timeKindsCheck <- getTime Monotonic
      iff (C.timing gopts) $ putStrLn("    Pass: Kinds check     : " ++ fmtTime (timeKindsCheck - timeEnv))

      (nmod,tchecked,typeEnv,mrefs) <- Acton.Types.reconstruct env kchecked
      -- Compute hash of source content from raw file bytes (stable across encoding)
      srcBytes <- B.readFile actFile
      let srcHash = SHA256.hash srcBytes
      -- Pre-compute list of root-eligible actors and store in .ty header
      let roots = case nmod of
                     A.NModule te _ -> [ n | (n,i) <- te, rootEligible i ]
                     _              -> []
      let mdoc = case nmod of A.NModule _ d -> d; _ -> Nothing
      impsRes <- resolveImportHashes mrefs
      case impsRes of
        Left diags -> return (Left diags)
        Right impsWithHash -> do
          -- Compute interface hash over doc-free NameInfo,
          -- augmented with current imports' interface hashes. This ensures that
          -- changes in a dependency's public interface are reflected in this
          -- module's interface hash, propagating rebuilds transitively.
          let selfIfaceBytes  = BL.toStrict $ encode (A.stripDocsNI nmod)
              depHashesSorted = map snd $ Data.List.sortOn (modNameToString . fst) impsWithHash
              depBytes        = BL.toStrict $ encode depHashesSorted
              ifaceHash       = SHA256.hash (B.append selfIfaceBytes depBytes)
          InterfaceFiles.writeFile (outbase ++ ".ty") srcHash ifaceHash impsWithHash roots mdoc nmod tchecked

          let A.NModule iface mdoc = nmod
          iff (C.types opts && mn == (modName paths)) $ dump mn "types" (Pretty.print tchecked)
          iff (C.sigs opts && mn == (modName paths)) $ dump mn "sigs" (Acton.Types.prettySigs env mn iface)

          -- Generate documentation, if building for a project
          when (not (C.skip_build opts) && not (isTmp paths)) $ do
              let docDir = joinPath [projPath paths, "out", "doc"]
                  modPathList = A.modPath mn
                  docFile = if null modPathList
                            then docDir </> "unnamed" <.> "html"
                            else joinPath (docDir : init modPathList) </> last modPathList <.> "html"
                  docFileDir = takeDirectory docFile
                  -- Get the type environment for this module
                  modTypeEnv = case Acton.Env.lookupMod mn typeEnv of
                      Just te -> te
                      Nothing -> iface
                  -- Apply the same simplification as --sigs uses
                  env1 = define iface $ setMod mn env
                  simplifiedTypeEnv = simp env1 modTypeEnv
              createDirectoryIfMissing True docFileDir
              -- Use parsed (original AST) to preserve docstrings
              let htmlDoc = DocP.printHtmlDoc (A.NModule simplifiedTypeEnv mdoc) parsed
              writeFile docFile htmlDoc

          timeTypeCheck <- getTime Monotonic
          iff (C.timing gopts) $ putStrLn("    Pass: Type check      : " ++ fmtTime (timeTypeCheck - timeKindsCheck))

          timeFrontEnd <- getTime Monotonic
          let frontTime = timeFrontEnd - timeStart
              frontDone = if not (quiet gopts opts)
                            then Just ("   Finished typecheck of " ++ modNameToString mn ++ " in  " ++ fmtTime frontTime)
                            else Nothing
              backJob = if C.only_build opts
                          then Nothing
                          else Just BackJob { bjPaths = paths
                                             , bjOpts = opts
                                             , bjInput = BackInput { biTypeEnv = typeEnv
                                                                  , biTypedMod = tchecked
                                                                  , biSrc = srcContent
                                                                  , biSrcHash = srcHash
                                                                  , biFrontTime = frontTime
                                                                  }
                                             }
          return $ Right FrontResult { frIfaceTE = iface
                                     , frDoc = mdoc
                                     , frIfaceHash = ifaceHash
                                     , frFrontDone = frontDone
                                     , frBackJob = backJob
                                     }

runBackPasses :: C.GlobalOptions -> C.CompileOptions -> Paths -> BackInput -> IO (Maybe String)
runBackPasses gopts opts paths backInput = do
      let mn = A.modname (biTypedMod backInput)
          outbase = outBase paths mn
          relSrcBase = makeRelative (projPath paths) (srcBase paths mn)
      timeStart <- getTime Monotonic

      (normalized, normEnv) <- Acton.Normalizer.normalize (biTypeEnv backInput) (biTypedMod backInput)
      iff (C.norm opts && mn == (modName paths)) $ dump mn "norm" (Pretty.print normalized)
      timeNormalized <- getTime Monotonic
      iff (C.timing gopts) $ putStrLn("    Pass: Normalizer      : " ++ fmtTime (timeNormalized - timeStart))

      (deacted,deactEnv) <- Acton.Deactorizer.deactorize normEnv normalized
      iff (C.deact opts && mn == (modName paths)) $ dump mn "deact" (Pretty.print deacted)
      timeDeactorizer <- getTime Monotonic
      iff (C.timing gopts) $ putStrLn("    Pass: Deactorizer     : " ++ fmtTime (timeDeactorizer - timeNormalized))

      (cpstyled,cpsEnv) <- Acton.CPS.convert deactEnv deacted
      iff (C.cps opts && mn == (modName paths)) $ dump mn "cps" (Pretty.print cpstyled)
      timeCPS <- getTime Monotonic
      iff (C.timing gopts) $ putStrLn("    Pass: CPS             : " ++ fmtTime (timeCPS - timeDeactorizer))

      (lifted,liftEnv) <- Acton.LambdaLifter.liftModule cpsEnv cpstyled
      iff (C.llift opts && mn == (modName paths)) $ dump mn "llift" (Pretty.print lifted)
      timeLLift <- getTime Monotonic
      iff (C.timing gopts) $ putStrLn("    Pass: Lambda Lifting  : " ++ fmtTime (timeLLift - timeCPS))

      boxed <- Acton.Boxing.doBoxing liftEnv lifted
      iff (C.box opts && mn == (modName paths)) $ dump mn "box" (Pretty.print boxed)
      timeBoxing <- getTime Monotonic
      iff (C.timing gopts) $ putStrLn("    Pass: Boxing :          " ++ fmtTime (timeBoxing - timeLLift))

      let hexHash = B.unpack $ Base16.encode (biSrcHash backInput)
          emitLines = not (C.dbg_no_lines opts)
      (n,h,c) <- Acton.CodeGen.generate liftEnv relSrcBase (biSrc backInput) emitLines boxed hexHash
      timeCodeGen <- getTime Monotonic
      iff (C.timing gopts) $ putStrLn("    Pass: Generating code : " ++ fmtTime (timeCodeGen - timeBoxing))

      iff (C.hgen opts) $ do
          putStrLn(h)
          System.Exit.exitSuccess
      iff (C.cgen opts) $ do
          putStrLn(c)
          System.Exit.exitSuccess

      iff (not (altOutput opts)) (do
          let cFile = outbase ++ ".c"
              hFile = outbase ++ ".h"

          writeFile hFile h
          writeFile cFile c
          let tyFileName = modNameToString(modName paths) ++ ".ty"
          iff (C.ty opts) $
               copyFileWithMetadata (joinPath [projTypes paths, tyFileName]) (joinPath [srcDir paths, tyFileName])

          timeCodeWrite <- getTime Monotonic
          iff (C.timing gopts) $ putStrLn("    Pass: Writing code    : " ++ fmtTime (timeCodeWrite - timeCodeGen))
                               )

      timeEnd <- getTime Monotonic
      let totalTime = biFrontTime backInput + (timeEnd - timeStart)
      if not (quiet gopts opts)
        then return (Just ("   Finished compilation of " ++ modNameToString mn ++ " in  " ++ fmtTime totalTime))
        else return Nothing


isGitAvailable :: IO Bool
isGitAvailable = do
    (exitCode, _, _) <- readProcessWithExitCode "git" ["--version"] ""
    return $ exitCode == ExitSuccess


-- Check if any other non-standard output is enabled, like --cgen or --sigs
altOutput opts =
  (C.parse opts) || (C.parse_ast opts) || (C.kinds opts) || (C.types opts) || (C.sigs opts) || (C.norm opts) || (C.deact opts) || (C.cps opts) || (C.llift opts) || (C.box opts) || (C.hgen opts) || (C.cgen opts)

modNameToFilename :: A.ModName -> String
modNameToFilename mn = joinPath (map nameToString names) ++ ".act"
  where
    A.ModName names = mn

-- Determine if we should use color output
useColor :: C.GlobalOptions -> IO Bool
useColor gopts = do
    -- First check NO_COLOR environment variable
    noColorEnv <- lookupEnv "NO_COLOR"
    case noColorEnv of
        Just _ -> return False  -- NO_COLOR is set to any value, disable color
        Nothing ->
            -- Check command-line option
            case C.color gopts of
                C.Never  -> return False
                C.Always -> return True
                C.Auto   -> do
                    -- Auto-detect based on TTY
                    tty <- hIsTerminalDevice stdout
                    return (tty || C.tty gopts)

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

modNameToString :: A.ModName -> String
modNameToString (A.ModName names) = intercalate "." (map nameToString names)

nameToString :: A.Name -> String
nameToString (A.Name _ s) = s

isWindowsOS :: String -> Bool
isWindowsOS targetTriple = case splitOn "-" targetTriple of
    (_:os:_) -> os == "windows"
    _        -> False

runZig gopts opts zigExe zigArgs paths wd = do
    let display = showCommandForUser zigExe zigArgs
    iff (C.ccmd opts || C.verbose gopts) $ putStrLn ("zigCmd: " ++ display)
    (returnCode, zigStdout, zigStderr) <- readCreateProcessWithExitCode (proc zigExe zigArgs){ cwd = wd } ""
    case returnCode of
        ExitSuccess -> do
          iff (C.verboseZig gopts) $ putStrLn zigStderr
          return ()
        ExitFailure ret -> do
          printIce ("compilation of generated Zig code failed, returned error code" ++ show ret)
          putStrLn $ "zig stdout:\n" ++ zigStdout
          putStrLn $ "zig stderr:\n" ++ zigStderr
          cleanup gopts opts paths
          System.Exit.exitFailure

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

#if defined(aarch64_HOST_ARCH)
defCpuFlag = ["-Dcpu=generic+crypto"]
#elif defined(x86_64_HOST_ARCH)
defCpuFlag = ["-Dcpu=x86_64_v2+aes"]
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
                   ("native":_)   -> defCpuFlag
                   ("aarch64":_)  -> ["-Dcpu=generic+crypto"]
                   ("x86_64":_)   -> ["-Dcpu=x86_64_v2+aes"]
                   (_:_)          -> defCpuFlag
                   []             -> defCpuFlag
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
