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
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Data.Ord
import Data.Graph
import Data.String.Utils (replace)
import Data.Version (showVersion)
import qualified Data.List
import qualified Data.Set
import Error.Diagnose
import Error.Diagnose.Style (defaultStyle)
import qualified Filesystem.Path.CurrentOS as Fsco
import Prettyprinter (unAnnotate)
import Prettyprinter.Render.Text (hPutDoc)
import System.Clock
import System.Directory
import System.Directory.Recursive
import System.Environment (lookupEnv)
import System.Exit
import System.FileLock
import System.FilePath ((</>))
import System.FilePath.Posix
import System.IO hiding (readFile, writeFile)
import System.IO.Temp
import System.Info
import System.Posix.Files
import System.Process
import qualified System.Environment
import qualified System.Exit
import qualified Paths_actonc
import Text.Printf

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Base16 as Base16
import qualified Crypto.Hash.SHA256 as SHA256

main = do
    hSetBuffering stdout LineBuffering
    arg <- C.parseCmdLine
    case arg of
        C.CmdOpt gopts (C.New opts)   -> createProject (C.file opts)
        C.CmdOpt gopts (C.Build opts) -> buildProject gopts opts
        C.CmdOpt gopts (C.Cloud opts) -> undefined
        C.CmdOpt gopts (C.Doc opts)   -> printDocs gopts opts
        C.CmdOpt gopts C.Version      -> printVersion
        C.CompileOpt nms gopts opts   -> case takeExtension (head nms) of
                                     ".act" -> buildFile gopts (applyGlobalOpts gopts opts) (head nms)
                                     ".ty" -> printDocs gopts (C.DocOptions (head nms) (Just C.AsciiFormat) Nothing)
                                     _ -> printErrorAndExit ("Unknown filetype: " ++ head nms)

defaultOpts   = C.CompileOptions False False False False False False False False False False False False
                                 False False False False C.Debug False False False False
                                 "" "" "" C.defTarget "" False []

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

cc :: Paths -> C.CompileOptions -> FilePath
cc paths opts = zig paths ++ " cc -target " ++ C.target opts

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
                      compileFiles gopts opts srcFiles

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
            compileFiles gopts opts [file]
          else do
            -- grab project lock
            let lock_file = joinPath [proj, ".actonc.lock"]
            withFileLock lock_file Exclusive $ \_ -> do
              compileFiles gopts opts [file]
      Nothing -> do
        -- Not in a project, use scratch directory for compilation unless
        -- --tempdir is provided - then use that
        if (C.tempdir opts /= "")
          then do
            iff (not(C.quiet gopts)) $ do
              putStrLn("Building file " ++ file ++ " using temporary directory " ++ C.tempdir opts)
            compileFiles gopts opts [file]
          else do
            iff (not(C.quiet gopts)) $ do
              putStrLn("Building file " ++ file ++ " using temporary scratch directory")
            home <- getHomeDirectory
            let basePath = joinPath [home, ".cache", "acton", "scratch"]
            createDirectoryIfMissing True basePath
            maybeLockInfo <- findAvailableScratch basePath
            case maybeLockInfo of
              Nothing -> error "Could not acquire any scratch directory lock"
              Just (lock, lockPath) -> do
                let scratchDir = dropExtension lockPath
                removeDirectoryRecursive scratchDir `catch` handleNotExists
                compileFiles gopts (opts { C.tempdir = scratchDir }) [file]
                unlockFile lock
  where
    handleNotExists :: IOException -> IO ()
    handleNotExists _ = return ()

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
                src <- readFile filename
                let modname = A.modName $ map (replace ".act" "") $ splitOn "/" $ fileBody
                parsed <- Acton.Parser.parseModule modname filename src

                -- Run compiler passes to get type information
                paths <- findPaths filename defaultOpts
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

removeOrphanFiles :: FilePath -> IO ()
removeOrphanFiles dir = do
    -- Recursively get all files in the "out" directory.
    absOutFiles <- getFilesRecursive dir
    let outFiles = map (makeRelative dir) absOutFiles

    -- Map over each file.
    forM_ outFiles $ \file -> do
        -- Remove the file ending.
        let fileNoExt = dropExtension (dropExtension file)
            srcFile = ("src" </> fileNoExt <.> "act")

        -- If the file is a .root.c file, always remove it and generate a new
        -- one later, if necessary. Only an .act file with a root actor should
        -- have a .root.c file but we cannot judge from here if the .act file
        -- actually has a root actor, so the only safe choice is to remove the
        -- file and let it be recreated if necessary. Should be very cheap
        -- anyway since the file is so small. So check if filename ends with
        -- ".root.c" and remove it!
        if takeExtension file == ".c" && takeExtension (takeBaseName file) == ".root"
          then do
            removeIfExists (dir </> file)
          else do
            -- Check if there is a corresponding .act file in the "src" directory.
            srcExists <- doesFileExist srcFile
            -- If the .act file doesn't exist, remove the file in the "out" directory.
            when (not srcExists) $ removeFile (dir </> file)
              `catch` handleNotExists
  where
        removeIfExists f = removeFile f `catch` handleNotExists
        handleNotExists :: IOException -> IO ()
        handleNotExists _ = return ()



compileFiles :: C.GlobalOptions -> C.CompileOptions -> [String] -> IO ()
compileFiles gopts opts srcFiles = do
    -- it is ok to get paths from just the first file here since at this point
    -- we only care about project level path stuff and all source files are
    -- known to be in the same project
    paths <- findPaths (head srcFiles) opts

    when (C.verbose gopts) $ do
        putStrLn ("  Paths:")
        putStrLn ("    sysPath  : " ++ sysPath paths)
        putStrLn ("    sysTypes : " ++ sysTypes paths)
        putStrLn ("    projPath : " ++ projPath paths)
        putStrLn ("    projOut  : " ++ projOut paths)
        putStrLn ("    projTypes: " ++ projTypes paths)
        putStrLn ("    binDir   : " ++ binDir paths)
        putStrLn ("    srcDir   : " ++ srcDir paths)
        iff (length srcFiles == 1) (putStrLn ("    modName  : " ++ prstr (modName paths)))

    -- remove files in out that do not have corresponding source files!
    removeOrphanFiles (projTypes paths)

    tasks <- mapM (parseActFile gopts opts paths) srcFiles
    iff (C.listimports opts) $ do
        let module_imports = map (\t -> concat [ modNameToString (name t), ": ", (concat $ intersperse " " (map (modNameToString) (importsOf t))) ] ) tasks
        let output = concat $ intersperse "\n" module_imports
        putStrLn output
        System.Exit.exitSuccess

    -- figure out binTasks, if --root is provided, use that, otherwise
    -- presumptuously use all non-stub source compile tasks, which get filtered
    -- out later on (after we parsed the project source files) in case they
    -- don't have a main actor, see filterMainActor
    let rootParts = splitOn "." (C.root opts)
        rootMod   = init rootParts
        guessMod  = if length rootParts == 1 then modName paths else A.modName rootMod
        binTask   = BinTask False (prstr guessMod) (A.GName guessMod (A.name $ last rootParts)) False
        preBinTasks
          | null (C.root opts) = map (\t -> BinTask True (modNameToString (name t)) (A.GName (name t) (A.name "main")) False) tasks
          | otherwise        = [binTask]
        preTestBinTasks = map (\t -> BinTask True (modNameToString (name t)) (A.GName (name t) (A.name "__test_main")) True) tasks
    env <- compileTasks gopts opts paths tasks
    -- Generate project documentation index
    unless (C.skip_build opts || isTmp paths) $ do
        let docDir = joinPath [projPath paths, "out", "doc"]
        createDirectoryIfMissing True docDir
        DocP.generateDocIndex docDir (map (\t -> (name t, src t, atree t, False)) tasks)
    if C.skip_build opts
      then
        putStrLn "  Skipping final build step"
      else
        if C.test opts
          then do
            testBinTasks <- catMaybes <$> mapM (filterMainActor env opts paths) preTestBinTasks
            compileBins gopts opts paths env tasks testBinTasks
            putStrLn "Test executables:"
            mapM_ (\t -> putStrLn (binName t)) testBinTasks
          else do
            compileBins gopts opts paths env tasks preBinTasks
    return ()


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

searchPaths :: C.CompileOptions -> [FilePath] -> IO [FilePath]
searchPaths opts deps = do
  -- append /out/types to each dep
  let deps_paths = map (\d -> joinPath [d, "out", "types"]) deps
  return $ deps_paths

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
                             let sPaths = [projTypes] ++ (C.searchpath opts) ++ [sysTypes]
                             --putStrLn ("Search paths: " ++ show sPaths)
                             createDirectoryIfMissing True binDir
                             createDirectoryIfMissing True projOut
                             createDirectoryIfMissing True projTypes
                             createDirectoryIfMissing True (getModPath projTypes modName)
                             return $ Paths sPaths sysPath sysTypes projPath projOut projTypes binDir srcDir isTmp fileExt modName
  where (fileBody,fileExt) = splitExtension $ takeFileName actFile

        analyze "/" ds  = do tmp <- canonicalizePath (C.tempdir opts)
                             return (True, tmp, [])
        analyze pre ds  = do let projectFiles = ["Acton.toml", "build.act", "build.act.json"]
                             hasProjectFile <- or <$> mapM (\file -> doesFileExist (joinPath [pre, file])) projectFiles
                             hasSrcDir <- doesDirectoryExist (joinPath [pre, "src"])
                             if hasProjectFile && hasSrcDir
                                then case ds of
                                    [] -> return $ (False, pre, [])
                                    "src":dirs -> return $ (False, pre, dirs)
                                    "out":"types":dirs -> return $ (False, pre, dirs)
                                    _ -> error ("************* Source file is not in a valid project directory: " ++ joinPath ds)
                                else analyze (takeDirectory pre) (takeFileName pre : ds)


-- Handling Acton files -----------------------------------------------------------------------------

filterActFile :: FilePath -> Maybe FilePath
filterActFile file =
    case fileExt of
        ".act" -> Just file
        _ -> Nothing
  where (fileBody, fileExt) = splitExtension $ takeFileName file

parseActFile :: C.GlobalOptions -> C.CompileOptions -> Paths -> String -> IO CompileTask
parseActFile gopts opts paths actFile = do
                    timeStart <- getTime Monotonic
                    paths <- findPaths actFile opts
                    srcContent <- readFile actFile
                    timeRead <- getTime Monotonic
                    iff (C.timing gopts) $ putStrLn("Reading file " ++ makeRelative (srcDir paths) actFile
                                                   ++ ": " ++ fmtTime(timeRead - timeStart))
                    m <- Acton.Parser.parseModule (modName paths) actFile srcContent
                      -- Parse errors from MegaParsec
                      `catch` handleParseError srcContent
                      -- Custom parse errors from Acton.Parser, thrown directly by parseException
                      `catch` (\err -> handleDiagnostic gopts opts paths (modName paths) $ Diag.customParseExceptionToDiagnostic actFile srcContent err)
                      `catch` handle gopts opts "Context error" Acton.Parser.contextError srcContent paths (modName paths)
                      `catch` handle gopts opts "Indentation error" Acton.Parser.indentationError srcContent paths (modName paths)
                    iff (C.parse opts) $ dump (modName paths) "parse" (Pretty.print m)
                    timeParse <- getTime Monotonic
                    iff (C.timing gopts) $ putStrLn("Parsing file " ++ makeRelative (srcDir paths) actFile
                                                                   ++ ": " ++ fmtTime(timeParse - timeRead))
                    return $ ActonTask (modName paths) srcContent m
    where handleParseError :: String -> ParseErrorBundle String CustomParseError -> IO A.Module
          handleParseError srcContent bundle =
                    handleDiagnostic gopts opts paths (modName paths) $ Diag.parseDiagnosticFromBundle actFile srcContent bundle


-- Compilation tasks, chasing imported modules, compilation and building executables -------------------------------------------

data CompileTask        = ActonTask { name :: A.ModName, src :: String, atree:: A.Module } deriving (Show)
-- TODO: replace binName String type with ModName just like for CompileTask.
-- ModName is a array so a hierarchy with submodules is represented, we can then
-- get it use joinPath (modPath) to get a path or modName to get a string
-- representation. We need both of BinTask when generating build.zig, so it
-- would be more robust to use that type rather than a hacky character
-- replacement (replaceDot in genBuildZigExe)
data BinTask            = BinTask { isDefaultRoot :: Bool, binName :: String, rootActor :: A.QName, isTest :: Bool } deriving (Show)

-- return task where the specified root actor exists
filterMainActor env opts paths binTask
                         = case Acton.Env.lookupMod m env of
                             Just te -> case lookup n te of
                               Just (A.NAct [] A.TNil{} (A.TRow _ _ _ t A.TNil{}) _ _)
                                   | prstr t == "Env" || prstr t == "None"
                                      || prstr t == "__builtin__.Env"|| prstr t == "__builtin__.None"-> do   -- TODO: proper check of parameter type
                                      return (Just binTask)
                                   | otherwise -> return Nothing
                               Just t -> return Nothing
                               Nothing -> return Nothing
                             Nothing -> return Nothing
  where mn                  = A.mname qn
        qn@(A.GName m n)    = rootActor binTask
        (sc,_)              = Acton.QuickType.schemaOf env (A.eQVar qn)

importsOf :: CompileTask -> [A.ModName]
importsOf t = A.importsOf (atree t)

compileTasks :: C.GlobalOptions -> C.CompileOptions -> Paths -> [CompileTask] -> IO Acton.Env.Env0
compileTasks gopts opts paths tasks
                       = do tasks <- chaseImportedFiles gopts opts paths tasks
                            -- We sort out the order of imports etc and split
                            -- out __builtin__, if it's part of the tasks, so we
                            -- can deal with it first
                            let sccs = stronglyConnComp  [(t, name t, importsOf t) | t <- tasks]
                                (builtinSccs, otherSccs) = partition containsBuiltin sccs
                                (as,cs) = Data.List.partition isAcyclic otherSccs

                            -- Seprate compile of __builtin__, if it's part of this project
                            case builtinSccs of
                                [AcyclicSCC t] -> do
                                        builtinEnv0 <- Acton.Env.initEnv (sysTypes paths) True
                                        doTask gopts opts paths builtinEnv0 t
                                        return ()
                                _ -> do return ()
                            let builtinPath = if null builtinSccs then sysTypes paths else projTypes paths

                            -- Compile all the other modules, reinitializing the env from disk
                            if null cs
                             then do env0 <- Acton.Env.initEnv builtinPath False
                                     env1 <- foldM (doTask gopts opts paths) env0 [t | AcyclicSCC t <- as]
                                     return env1
                              else printErrorAndCleanAndExit ("Cyclic imports: "++concatMap showTaskGraph cs) gopts opts paths
  where isAcyclic (AcyclicSCC _) = True
        isAcyclic _              = False
        showTaskGraph ts         = "\n"++concatMap (\t-> concat (intersperse "." (A.modPath (name t)))++" ") ts
        containsBuiltin (AcyclicSCC task) = name task == (A.modName ["__builtin__"])

compileBins:: C.GlobalOptions -> C.CompileOptions -> Paths -> Acton.Env.Env0 -> [CompileTask] -> [BinTask] -> IO ()
compileBins gopts opts paths env tasks binTasks = do
    iff (not (altOutput opts)) $ do
      zigBuild env gopts opts paths tasks binTasks
    return ()
  where
    handleNotExists :: IOException -> IO ()
    handleNotExists _ = return ()


chaseImportedFiles :: C.GlobalOptions -> C.CompileOptions -> Paths -> [CompileTask] -> IO [CompileTask]
chaseImportedFiles gopts opts paths itasks
                            = do
                                 let itasks_imps = concatMap importsOf itasks
                                 newtasks <- catMaybes <$> mapM (readAFile itasks) itasks_imps
                                 chaseRecursively (itasks ++ newtasks) (map name newtasks) (concatMap importsOf newtasks)

  where readAFile tasks mn  = case lookUp mn tasks of    -- read and parse file mn in the project directory, unless it is already in tasks
                                 Just t -> return Nothing
                                 Nothing -> do let actFile = srcFile paths mn
                                               ok <- System.Directory.doesFileExist actFile
                                               if ok then do
                                                   task <- parseActFile gopts opts paths actFile
                                                   return $ Just task
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

doTask :: C.GlobalOptions -> C.CompileOptions -> Paths -> Acton.Env.Env0 -> CompileTask -> IO Acton.Env.Env0
doTask gopts opts paths env t@(ActonTask mn src m) = do
    iff (not (quiet gopts opts))  (putStrLn("  Compiling " ++ makeRelative (srcDir paths) actFile
              ++ " with " ++ show (C.optimize opts)))

    timeStart <- getTime Monotonic

    let outFiles = [tyFile, hFile, cFile]
    timeBeforeCheck <- getTime Monotonic
    uptoDateResult <- checkUptoDate gopts opts paths actFile outFiles (importsOf t)
    timeAfterCheck <- getTime Monotonic
    iff (C.timing gopts) $ putStrLn("   Check up-to-date & read .ty file: " ++ fmtTime(timeAfterCheck - timeBeforeCheck))
    case uptoDateResult of
      Just (ms, nmod) | C.only_build opts || not (mn == (modName paths) && (forceCompilation opts)) -> do
        timeEnd <- getTime Monotonic
        iff (not (quiet gopts opts)) $ putStrLn("   Already up to date, in   " ++ fmtTime(timeEnd - timeStart))
        let A.NModule te mdoc = nmod
        return (Acton.Env.addMod mn te mdoc env)
      _ -> do
        createDirectoryIfMissing True (getModPath (projTypes paths) mn)
        env' <- runRestPasses gopts opts paths env m src
          `catch` handle gopts opts "Compilation error" generalError src paths mn
          `catch` handle gopts opts "Compilation error" Acton.Env.compilationError src paths mn
          `catch` (\err -> handleDiagnostic gopts opts paths (modName paths) $ mkErrorDiagnostic filename src $ Acton.TypeM.typeReport err filename src)
        timeEnd <- getTime Monotonic
        iff (not (quiet gopts opts)) $ putStrLn("   Finished compilation in  " ++ fmtTime(timeEnd - timeStart))
        return env'
  where actFile             = srcFile paths mn
        filename            = modNameToFilename mn
        outbase             = outBase paths mn
        tyFile              = outbase ++ ".ty"
        hFile               = outbase ++ ".h"
        cFile               = outbase ++ ".c"
        forceCompilation :: C.CompileOptions -> Bool
        forceCompilation args = (C.alwaysbuild args) || (C.parse args) || (C.kinds args) || (C.types args) || (C.sigs args)
                                || (C.norm args) || (C.deact args) || (C.cps args) || (C.llift args) || (C.hgen args) ||(C.cgen args)


-- | Check if a module is up-to-date and return its type interface if it is.
-- Returns Nothing if the module needs recompilation (not up-to-date or .ty file unreadable).
-- Returns Just (imports, nameInfo) if the module is up-to-date with a valid .ty file.
--
-- The function checks:
-- 1. All output files exist
-- 2. Output files are newer than source files (fast path)
-- 3. If source appears newer, check content hash to detect false positives
-- 4. Import dependencies are up-to-date
-- 5. The .ty file (first in outFiles) can be successfully read. Torn writes
--    (actonc was killed while working) or compiler updates can cause .ty files
--    to be unreadable and thus we should consider it out-of-date and recompile.
--    These days torn writes should be very unlikely since we do atomic write
--    with write to tmp + rename
checkUptoDate :: C.GlobalOptions -> C.CompileOptions -> Paths -> FilePath -> [FilePath] -> [A.ModName] -> IO (Maybe ([A.ModName], A.NameInfo))
checkUptoDate gopts opts paths actFile outFiles imps = do
    iff (C.verbose gopts) (putStrLn ("    Checking " ++ makeRelative (srcDir paths) actFile ++ " is up to date..."))
    -- get the path to the actonc binary, i.e. ourself
    actoncBin <- System.Environment.getExecutablePath
    -- get path to `acton` which is the actonc binary without the `c` at the end
    let actonBin = take (length actoncBin - 1) actoncBin
    let potSrcFiles     = [actonBin, actoncBin, actFile, extCFile, srcCFile, srcHFile]
    srcFiles  <- filterM System.Directory.doesFileExist potSrcFiles
    outExists <- mapM System.Directory.doesFileExist outFiles

    if not (and outExists)
        then do
            iff (C.verbose gopts) (putStrLn ("    Missing output files: " ++ show outExists ++ " for " ++ show outFiles))
            return Nothing
        else do
            -- get the time of the last modified source file
            srcTime  <- head <$> sortBy (comparing Down) <$> mapM System.Directory.getModificationTime srcFiles
            outTiming <- mapM System.Directory.getModificationTime outFiles
            impsOK   <- mapM (impOK (head outTiming)) imps

            if all (srcTime <) outTiming && and impsOK
                then do
                    -- Fast path: if output files are newer and imports are OK
                    -- All timestamps check out, read the .ty file to ensure it is readable
                    let tyFile = head outFiles
                    tyResult <- (try :: IO a -> IO (Either SomeException a)) $ InterfaceFiles.readFile tyFile
                    case tyResult of
                        Left e -> do
                            iff (C.verbose gopts) (putStrLn ("    .ty file is unreadable (will recompile): " ++ displayException e))
                            return Nothing
                        Right (ms, nmod, _, _) -> return (Just (ms, nmod))
                else if not (and impsOK)
                    then do
                        iff (C.verbose gopts) (putStrLn ("    Import dependencies are newer than output files"))
                        return Nothing
                    else do
                        -- Source appears newer - check content hash to be sure
                        let tyFile = head outFiles
                        tyResult <- (try :: IO a -> IO (Either SomeException a)) $ InterfaceFiles.readFile tyFile
                        case tyResult of
                            Left e -> do
                                iff (C.verbose gopts) (putStrLn ("    .ty file is unreadable: " ++ displayException e))
                                return Nothing
                            Right (ms, nmod, _, storedHash) -> do
                                -- Compute current source file hash
                                currentSrcContent <- B.readFile actFile
                                let currentHash = SHA256.hash currentSrcContent
                                if currentHash == storedHash
                                    then do
                                        iff (C.verbose gopts) (putStrLn ("    Source file unchanged (hash match), using cached compilation"))
                                        return (Just (ms, nmod))
                                    else do
                                        iff (C.verbose gopts) (putStrLn ("    Source file content changed (hash mismatch)"))
                                        return Nothing
  where
        srcBase         = joinPath [takeDirectory actFile, takeBaseName actFile]
        srcCFile        = srcBase ++ ".c"
        srcHFile        = srcBase ++ ".h"
        extCFile        = srcBase ++ ".ext.c"
        -- except for actFile, these are *potential* source files which might
        -- not actually exist...
        impOK iTime mn  = do
                             impFile <- findTyFile (searchPath paths) mn
                             case impFile of
                               Nothing -> return False
                               Just impFile -> do
                                 impfileTime <- System.Directory.getModificationTime impFile
                                 return (impfileTime < iTime)
        -- find .ty file by looking both in local project and in stdlib
        findTy paths mn = do
                             let localImpName = outBase paths mn ++ ".ty"
                                 stdlibImpName = joinPath (sysTypes paths : A.modPath mn) ++ ".ty"
                             projExist <- System.Directory.doesFileExist localImpName
                             stdlibExist <- System.Directory.doesFileExist stdlibImpName
                             let filePath = case (projExist, stdlibExist) of
                                   (True, True) -> localImpName
                                   (True, False) -> localImpName
                                   (False, True) -> stdlibImpName
                                   (False, False) -> error("ERROR: Unable to find interface file")
                             return filePath

isGitAvailable :: IO Bool
isGitAvailable = do
    (exitCode, _, _) <- readProcessWithExitCode "git" ["--version"] ""
    return $ exitCode == ExitSuccess


-- Check if any other non-standard output is enabled, like --cgen or --sigs
altOutput opts =
  (C.parse opts) || (C.kinds opts) || (C.types opts) || (C.sigs opts) || (C.norm opts) || (C.deact opts) || (C.cps opts) || (C.llift opts) || (C.box opts) || (C.hgen opts) || (C.cgen opts)

runRestPasses :: C.GlobalOptions -> C.CompileOptions -> Paths -> Acton.Env.Env0 -> A.Module -> String -> IO Acton.Env.Env0
runRestPasses gopts opts paths env0 parsed srcContent = do
                      let mn = A.modname parsed
                      let outbase = outBase paths mn
                      let absSrcBase = srcBase paths mn
                      let relSrcBase = makeRelative (projPath paths) (srcBase paths mn)
                      let actFile = absSrcBase ++ ".act"

                      timeStart <- getTime Monotonic

                      env <- Acton.Env.mkEnv (searchPath paths) env0 parsed
                      --traceM ("#################### initial env0:")
                      --traceM (Pretty.render (Pretty.pretty env))
                      timeEnv <- getTime Monotonic
                      iff (C.timing gopts) $ putStrLn("    Pass: Make environment: " ++ fmtTime (timeEnv - timeStart))

                      kchecked <- Acton.Kinds.check env parsed
                      iff (C.kinds opts && mn == (modName paths)) $ dump mn "kinds" (Pretty.print kchecked)
                      timeKindsCheck <- getTime Monotonic
                      iff (C.timing gopts) $ putStrLn("    Pass: Kinds check     : " ++ fmtTime (timeKindsCheck - timeEnv))

                      (nmod,tchecked,typeEnv,mrefs) <- Acton.Types.reconstruct env kchecked
                      -- Compute hash of source content. TODO: ideally replace with hash of AST, not .act content
                      let srcHash = SHA256.hash (B.pack srcContent)
                      InterfaceFiles.writeFile (outbase ++ ".ty") mrefs nmod tchecked srcHash

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

                      --traceM ("#################### typed env0:")
                      --traceM (Pretty.render (Pretty.pretty typeEnv))
                      timeTypeCheck <- getTime Monotonic
                      iff (C.timing gopts) $ putStrLn("    Pass: Type check      : " ++ fmtTime (timeTypeCheck - timeKindsCheck))

                      (normalized, normEnv) <- Acton.Normalizer.normalize typeEnv tchecked
                      iff (C.norm opts && mn == (modName paths)) $ dump mn "norm" (Pretty.print normalized)
                      --traceM ("#################### normalized env0:")
                      --traceM (Pretty.render (Pretty.pretty normEnv))
                      timeNormalized <- getTime Monotonic
                      iff (C.timing gopts) $ putStrLn("    Pass: Normalizer      : " ++ fmtTime (timeNormalized - timeTypeCheck))

                      (deacted,deactEnv) <- Acton.Deactorizer.deactorize normEnv normalized
                      iff (C.deact opts && mn == (modName paths)) $ dump mn "deact" (Pretty.print deacted)
                      --traceM ("#################### deacted env0:")
                      --traceM (Pretty.render (Pretty.pretty deactEnv))
                      timeDeactorizer <- getTime Monotonic
                      iff (C.timing gopts) $ putStrLn("    Pass: Deactorizer     : " ++ fmtTime (timeDeactorizer - timeNormalized))

                      (cpstyled,cpsEnv) <- Acton.CPS.convert deactEnv deacted
                      iff (C.cps opts && mn == (modName paths)) $ dump mn "cps" (Pretty.print cpstyled)
                      --traceM ("#################### cps'ed env0:")
                      --traceM (Pretty.render (Pretty.pretty cpsEnv))
                      timeCPS <- getTime Monotonic
                      iff (C.timing gopts) $ putStrLn("    Pass: CPS             : " ++ fmtTime (timeCPS - timeDeactorizer))

                      (lifted,liftEnv) <- Acton.LambdaLifter.liftModule cpsEnv cpstyled
                      iff (C.llift opts && mn == (modName paths)) $ dump mn "llift" (Pretty.print lifted)
                      --traceM ("#################### lifteded env0:")
                      --traceM (Pretty.render (Pretty.pretty liftEnv))
                      timeLLift <- getTime Monotonic
                      iff (C.timing gopts) $ putStrLn("    Pass: Lambda Lifting  : " ++ fmtTime (timeLLift - timeCPS))

                      boxed <- Acton.Boxing.doBoxing liftEnv lifted
                      iff (C.box opts && mn == (modName paths)) $ dump mn "box" (Pretty.print boxed)
                      timeBoxing <- getTime Monotonic
                      iff (C.timing gopts) $ putStrLn("    Pass: Boxing :          " ++ fmtTime (timeBoxing - timeLLift))

                      -- Convert hash to hex string for comment
                      let hexHash = B.unpack $ Base16.encode srcHash
                      (n,h,c) <- Acton.CodeGen.generate liftEnv relSrcBase boxed hexHash
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

                      return $ Acton.Env.addMod mn iface mdoc (env0 `Acton.Env.withModulesFrom` env)

handle gopts opts errKind f src paths mn ex = do
    let actFile = modNameToFilename mn
    let errors = f ex
    -- Convert the first error to a diagnostic
    case errors of
        [] -> do
            -- Fallback if no error location
            putStrLn ("\nERROR: Error when compiling " ++ (prstr mn) ++ " module: " ++ errKind)
        ((loc, msg):_) -> do
            let diagnostic = Diag.actErrToDiagnostic errKind actFile src loc msg
            printDiag gopts opts diagnostic
    handleCleanup paths mn

modNameToFilename :: A.ModName -> String
modNameToFilename mn = joinPath (map nameToString names) ++ ".act"
  where
    A.ModName names = mn

-- | Handle errors by converting them to diagnostics and printing them
-- This is meant to eventually replace the `handle` function by being a more
-- modern alternative that natively works with the Diagnose library we use to
-- print errors
handleDiagnostic :: C.GlobalOptions -> C.CompileOptions -> Paths -> A.ModName -> Diagnostic String -> IO a
handleDiagnostic gopts opts paths mn diagnostic = do
    printDiag gopts opts diagnostic
    handleCleanup paths mn

handleCleanup paths mn = do
    removeIfExists (outbase++".ty")
    System.Exit.exitFailure
  where outbase        = outBase paths mn
        removeIfExists f = removeFile f `catch` handleNotExists
        handleNotExists :: IOException -> IO ()
        handleNotExists _ = return ()

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

writeRootC :: Acton.Env.Env0 -> C.GlobalOptions -> C.CompileOptions -> Paths -> BinTask -> IO (Maybe BinTask)
writeRootC env gopts opts paths binTask = do
    let qn@(A.GName m n) = rootActor binTask
        mn = A.mname qn
        outbase = outBase paths mn
        rootFile = if (isTest binTask) then outbase ++ ".test_root.c" else outbase ++ ".root.c"
    case Acton.Env.lookupMod m env of
        Nothing -> return Nothing  -- Handle the case where module lookup fails
        Just modEnv ->
            case lookup n modEnv of
                Just (A.NAct [] A.TNil{} (A.TRow _ _ _ t A.TNil{}) _ _)
                    | prstr t == "Env" || prstr t == "None"
                        || prstr t == "__builtin__.Env"|| prstr t == "__builtin__.None" -> do
                        c <- Acton.CodeGen.genRoot env qn
                        createDirectoryIfMissing True (takeDirectory rootFile)
                        writeFile rootFile c
                        return (Just binTask)
                    | otherwise -> handleDiagnostic gopts opts paths (modName paths) $ mkErrorDiagnostic "" "" $ typeReport
                        (Acton.TypeM.TypeError NoLoc ("Illegal type "++ prstr t ++ " of parameter to root actor " ++ prstr qn)) "" ""
                Just t -> handleDiagnostic gopts opts paths (modName paths) $ mkErrorDiagnostic "" "" $ typeReport
                    (Acton.TypeM.TypeError NoLoc (prstr qn ++ " has not actor type.")) "" ""
                Nothing -> return Nothing

modNameToString :: A.ModName -> String
modNameToString (A.ModName names) = intercalate "." (map nameToString names)

nameToString :: A.Name -> String
nameToString (A.Name _ s) = s

isWindowsOS :: String -> Bool
isWindowsOS targetTriple = case splitOn "-" targetTriple of
    (_:os:_) -> os == "windows"
    _        -> False

runZig gopts opts zigCmd paths wd = do
    iff (C.ccmd opts) $ putStrLn zigCmd
    (returnCode, zigStdout, zigStderr) <- readCreateProcessWithExitCode (shell $ zigCmd){ cwd = wd } ""
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

makeAlwaysRelative :: FilePath -> FilePath -> FilePath
makeAlwaysRelative base target =
    case makeRelative base target of
        path | isAbsolute path ->  -- Still an absolute path, so no overlap found
               let baseCount = length $ filter (/= "./") $ splitPath base
                   targetPath = dropDrive path  -- Remove the drive part of the absolute path
               in joinPath (replicate baseCount "..") </> targetPath
            | otherwise -> path  -- makeRelative found overlap, use its result

-- TODO: replace all of this with generic+crypto?!
#if defined(darwin_HOST_OS) && defined(aarch64_HOST_ARCH)
defCpu = " -Dcpu=apple_a15 "
#elif defined(darwin_HOST_OS) && defined(x86_64_HOST_ARCH)
defCpu = ""
#elif defined(linux_HOST_OS) && defined(aarch64_HOST_ARCH)
defCpu = " -Dcpu=cortex_a72 "
#elif defined(linux_HOST_OS) && defined(x86_64_HOST_ARCH)
defCpu = ""
#else
#error "Unsupported platform"
#endif

zigBuild :: Acton.Env.Env0 -> C.GlobalOptions -> C.CompileOptions -> Paths -> [CompileTask] -> [BinTask] -> IO ()
zigBuild env gopts opts paths tasks binTasks = do
    allBinTasks <- mapM (writeRootC env gopts opts paths) binTasks
    let realBinTasks = catMaybes allBinTasks
    iff (not (quiet gopts opts)) $ putStrLn("  Final compilation step")
    timeStart <- getTime Monotonic

    homeDir <- getHomeDirectory
    let local_cache_dir = joinPath [ homeDir, ".cache", "acton", "zig-local-cache" ]
        global_cache_dir = joinPath [ homeDir, ".cache", "acton", "zig-global-cache" ]
        no_threads = if isWindowsOS (C.target opts) then True else C.no_threads opts
        target_cpu = if (C.cpu opts /= "")
                       then " -Dcpu=" ++ C.cpu opts
                       else
                         case (splitOn "-" (C.target opts)) of
                           ("native":_)            -> defCpu
                           ("aarch64":"macos":_)   -> " -Dcpu=apple_a15 "
    -- TODO: how do we do better here? Windows presumably runs on many CPUs that are not aarch64. We really just want to enable AES
                           ("aarch64":"windows":_) -> " -Dcpu=apple_a15 "
                           ("aarch64":"linux":_)   -> " -Dcpu=cortex_a72 "
                           ("x86_64":_:_)          -> " -Dcpu=westmere "
                           (_:_:_)                 -> defCpu


    -- If actonc runs as a standalone compiler (not a sub-compiler from Acton CLI),
    -- then we may need to generate build.zig and build.zig.zon
    iff (not (C.sub gopts)) $ do
        let buildZigPath     = joinPath [projPath paths, "build.zig"]
            buildZonPath     = joinPath [projPath paths, "build.zig.zon"]
            -- Compute relative path from current directory (projPath paths), since zig likes relative paths
            relativeSysPath  = makeAlwaysRelative (projPath paths) (sysPath paths)
            distBuildZigPath = joinPath [(sysPath paths), "builder", "build.zig"]

        buildZonExists <- doesFileExist buildZonPath
        copyFile distBuildZigPath buildZigPath
        let distBuildZonPath = joinPath [(sysPath paths), "builder", "build.zig.zon"]
        distBuildZon <- readFile distBuildZonPath
        let buildZon = replace "{{syspath}}" relativeSysPath distBuildZon
        writeFile buildZonPath buildZon

    let zigCmdBase = zig paths ++ " build " ++ " --cache-dir " ++ local_cache_dir ++
                 " --global-cache-dir " ++ global_cache_dir ++
                 (if (C.verboseZig gopts) then " --verbose " else "")
    let zigCmd = zigCmdBase ++
                 " --prefix " ++ projOut paths ++ " --prefix-exe-dir 'bin'" ++
                 (if (C.verboseZig gopts) then " --verbose " else "") ++
                 " -Dtarget=" ++ (C.target opts) ++
                 target_cpu ++
                 " -Doptimize=" ++ optimizeModeToZig (C.optimize opts) ++
                 (if (C.db opts) then " -Ddb " else "") ++
                 (if no_threads then " -Dno_threads " else "") ++
                 (if (C.cpedantic opts) then " -Dcpedantic " else "")

    iff (C.verbose gopts) $ putStrLn ("zigCmd: " ++ zigCmd)
    runZig gopts opts zigCmd paths (Just (projPath paths))
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
  where
    handleNotExists :: IOException -> IO ()
    handleNotExists _ = return ()
