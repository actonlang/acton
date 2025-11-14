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
import qualified Data.Map as M
import qualified Data.Set
import Error.Diagnose
import Error.Diagnose.Style (defaultStyle)
import qualified Filesystem.Path.CurrentOS as Fsco
import GHC.Conc (getNumCapabilities, getNumProcessors, setNumCapabilities, myThreadId, threadCapability)
import Prettyprinter (unAnnotate)
import Prettyprinter.Render.Text (hPutDoc)
import Data.List (isPrefixOf, isSuffixOf, find, intersperse, partition, foldl')
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

-- Shared predicate: is a NameInfo root-eligible (actor with env/no params)?
rootEligible :: A.NameInfo -> Bool
rootEligible (A.NAct [] p k _ _) = case (p,k) of
                                              (A.TNil{}, A.TRow _ _ _ t A.TNil{}) ->
                                                prstr t == "Env" || prstr t == "None" ||
                                                prstr t == "__builtin__.Env" || prstr t == "__builtin__.None"
                                              _ -> False
rootEligible _ = False

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
                                 False False False False False C.Debug False False False False
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
                let modname = A.modName $ map (replace ".act" "") $ splitOn "/" $ fileBody
                paths <- findPaths filename defaultOpts
                (_src, parsed) <- parseActFile gopts defaultOpts paths modname filename

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
        if isRootStub
          then when (not (absFile `elem` roots)) (removeIfExists absFile)
          else when (isC || isH || isTy) $ do
                 let base = dropExtension absFile
                 unless (base `elem` allowedBases) (removeIfExists absFile)
  where
    removeIfExists f = removeFile f `catch` handleNotExists
    handleNotExists :: IOException -> IO ()
    handleNotExists _ = return ()

parseActSource :: C.GlobalOptions -> C.CompileOptions -> Paths -> A.ModName -> FilePath -> String -> IO A.Module
parseActSource gopts opts projPaths mn actFile srcContent = do
  Acton.Parser.parseModule mn actFile srcContent
    `catch` (\bundle -> handleDiagnostic gopts opts projPaths mn $ Diag.parseDiagnosticFromBundle actFile srcContent bundle)
    `catch` (\err -> handleDiagnostic gopts opts projPaths mn $ Diag.customParseExceptionToDiagnostic actFile srcContent err)
    `catch` handle gopts opts "Context error" Acton.Parser.contextError srcContent projPaths mn
    `catch` handle gopts opts "Indentation error" Acton.Parser.indentationError srcContent projPaths mn

parseActFile :: C.GlobalOptions -> C.CompileOptions -> Paths -> A.ModName -> FilePath -> IO (String, A.Module)
parseActFile gopts opts projPaths mn actFile = do
  srcContent <- readFile actFile
  m <- parseActSource gopts opts projPaths mn actFile srcContent
  return (srcContent, m)


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
     - If .ty exists and .act mtime <= .ty mtime → trust .ty header imports and
       create a TyTask stub (no heavy decode) for graph building.
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

    -- Build initial tasks using .ty headers when possible; parse only if .ty missing
    tasks0 <- mapM (readModuleTask gopts opts paths) srcFiles
    -- Expand set to include imports starting from the initial set. For project
    -- builds, this usually amounts to a no-op since all source files were in
    -- the initial set (srcFiles).
    tasks1 <- readImports gopts opts paths tasks0
    -- Then order tasks in dependency (topological) order
    let tasks = tasks1
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
    if C.skip_build opts
      then
        putStrLn "  Skipping final build step"
      else
        if C.test opts
          then do
            testBinTasks <- catMaybes <$> mapM (filterMainActor env paths) preTestBinTasks
            compileBins gopts opts paths env tasks testBinTasks
            putStrLn "Test executables:"
            mapM_ (\t -> putStrLn (binName t)) testBinTasks
          else do
            compileBins gopts opts paths env tasks preBinTasks
    return ()

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

-- Prepare a task for dependency graph construction.
-- Prefer reading imports from .ty header; if no .ty, parse the source.
-- Decide how to represent a module for the graph:
-- 1) If .ty is missing/unreadable -> parse .act to obtain imports (ActonTask).
-- 2) If .ty exists and .act mtime <= .ty mtime -> trust header imports (TyTask).
-- 3) If .act appears newer than .ty -> verify by content hash:
--      - If stored srcHash == current srcHash -> header is still valid (TyTask)
--      - Else -> parse .act to obtain accurate imports (ActonTask)
-- Returns either a header-only TyTask stub or an ActonTask; no heavy decoding.
readModuleTask :: C.GlobalOptions -> C.CompileOptions -> Paths -> String -> IO CompileTask
readModuleTask gopts opts projPaths actFile = do
    p <- findPaths actFile opts
    let mn      = modName p
        tyFile  = outBase projPaths mn ++ ".ty"
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
            if actTime <= tyTime
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
      (srcContent, m) <- parseActFile gopts opts projPaths mn actFile
      return $ ActonTask mn srcContent m


-- Compilation tasks, chasing imported modules, compilation and building executables -------------------------------------------

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
                        deriving (Show)

-- TODO: replace binName String type with ModName just like for CompileTask.
-- ModName is a array so a hierarchy with submodules is represented, we can then
-- get it use joinPath (modPath) to get a path or modName to get a string
-- representation. We need both of BinTask when generating build.zig, so it
-- would be more robust to use that type rather than a hacky character
-- replacement (replaceDot in genBuildZigExe)
data BinTask            = BinTask { isDefaultRoot :: Bool, binName :: String, rootActor :: A.QName, isTest :: Bool } deriving (Show)

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

-- compileTasks
--
-- Build and compile the given module tasks in dependency order. This is the
-- core driver for a project or single-file build once tasks have been read by
-- readModuleTask and (if needed) imports expanded by readImports.
--
-- Inputs
--   - tasks: a mixed list of ActonTask (source needs compiling) and TyTask
--     (up-to-date .ty header available). The list may be in any order.
--
-- Responsibilities
--   1) Construct a project-local dependency graph from the tasks and compute a
--      topological order. Dependencies outside the project (stdlib/deps) are
--      ignored for ordering and staleness propagation.
--   2) Optionally compile __builtin__ first if it is part of the project.
--   3) Walk modules in topological order, deciding per module whether to
--      compile or reuse the cached .ty, and update the environment.
--   4) Track interface-hash changes for project modules and propagate
--      staleness only when an imported module’s interface hash differs from
--      the hash that was recorded when the dependent was last compiled.
--
-- Key ideas
--   - TyTask carries only a lightweight header read from the module’s .ty
--     file: the source hash, the interface hash, its imports annotated with
--     the interface hash that was used, roots and docstring. TyTask avoids
--     decoding heavy sections.
--   - ActonTask carries parsed source and must be compiled.
--   - We maintain ifaceMap :: Map ModName ByteString during the fold. For any
--     project module that has been compiled in this run (or confirmed fresh),
--     ifaceMap holds the interface hash that downstream modules should compare
--     against. If a dependent TyTask recorded a different hash for an import,
--     it is deemed stale and compiled.
--   - When a TyTask is deemed stale (e.g., any imported module’s interface hash
--     differs from the recorded one) stepModule first converts it to an
--     ActonTask by parsing the source (parseActFile), then compiles it via
--     doTask. For fresh TyTasks we do not parse or compile; we simply reuse the
--     cached .ty and carry forward its recorded interface hash.
--   - The fold is the “inner loop”: foldM (stepModule projMods) (env0, startIface)
--     otherOrder. stepModule decides compile vs reuse, updates the environment
--     and propagates new interface hashes for already-processed modules.
compileTasks :: C.GlobalOptions -> C.CompileOptions -> Paths -> [CompileTask] -> IO Acton.Env.Env0
compileTasks gopts opts paths tasks = do
    -- Reject cycles
    unless (null cycles) $ printErrorAndCleanAndExit ("Cyclic imports: " ++ concatMap showTaskGraph cycles) gopts opts paths

    -- Compile __builtin__ first if present in this project
    case builtinOrder of
      [t] -> do
        builtinEnv0 <- Acton.Env.initEnv (sysTypes paths) True
        void (doTask gopts opts paths builtinEnv0 t)
      _ -> return ()

    baseEnv <- Acton.Env.initEnv builtinPath False

    -- cost(m): cheap proxy for compile effort = size of m.act
    -- Used by the critical‑path heuristic below to prioritize ready modules.
    costMap <- fmap M.fromList $ forM otherOrder $ \t -> do
                  let mn = name t
                  let p  = srcFile paths mn
                  ok <- doesFileExist p
                  sz <- if ok then getFileSize p else return 0
                  return (mn, sz)
    let cwMap = computeCriticalWeights costMap

    -- Figure out max concurrency
    nCaps <- getNumCapabilities
    let maxParallel = max 1 (if C.jobs gopts > 0 then C.jobs gopts else nCaps)

    -- Actually compile modules, with maximum concurrency
    envFinal <- loop initialReady [] M.empty indeg pending0 baseEnv maxParallel cwMap
    return envFinal
  where
    -- Graph construction --------------------------------------------------
    allTasks   = tasks
    projMap    = M.fromList [ (name t, t) | t <- allTasks ]
    nodes      = [ (t, name t, [ m | m <- importsOf t, M.member m projMap ]) | t <- allTasks ]
    sccs       = stronglyConnComp nodes
    cycles     = [ ts | ts@(CyclicSCC _) <- sccs ]
    order      = [ t | AcyclicSCC t <- sccs ]
    showTaskGraph (CyclicSCC ts) = "\n" ++ concatMap (\t-> concat (intersperse "." (A.modPath (name t))) ++ " ") ts
    showTaskGraph _              = ""

    isBuiltin t = name t == (A.modName ["__builtin__"])
    (builtinOrder, otherOrder) = partition isBuiltin order
    projMods   = Data.Set.fromList (map name otherOrder)
    builtinPath = if null builtinOrder then sysTypes paths else projTypes paths

    depMap :: M.Map A.ModName [A.ModName]
    depMap = M.fromList [ (name t, [ m | m <- importsOf t, Data.Set.member m projMods ]) | t <- otherOrder ]
    revMap :: M.Map A.ModName [A.ModName]
    revMap = M.foldlWithKey' (\acc k ds -> foldl' (\a d -> M.insertWith (++) d [k] a) acc ds) M.empty depMap
    indeg  = M.map length depMap
    initialReady = [ m | (m,d) <- M.toList indeg, d == 0 ]
    pending0 = Data.Set.fromList (M.keys indeg)

    getMyCap = do
      tid <- myThreadId
      (cap, _) <- threadCapability tid
      return cap

    -- Critical‑path weights
    -- We approximate the remaining work under each module by a critical‑path
    -- style metric over the project DAG. We want to start the modules that
    -- unlock the most remaining work as early as possible. Intuitively it is
    -- better to start with large module, but a small module at the head of a
    -- long/heavy chain is more urgent than a big ones. That helps avoid a late
    -- long-tail.
    --
    -- How we score each module (cheap, good‑enough heuristic):
    --   weight(m) = cost(m) + max weight(d) over its dependents d
    --   where cost(m) ≈ size of m.act (proxy for compile effort).
    -- This approximates the length of the critical path: how much work must
    -- follow after m. We then sort the ready set by descending weight so heavy
    -- chains start early and overall makespan improves.
    --   - cost(m) is a cheap proxy for the time to compile m (we use .act file size).
    --   - max weight(d) estimates the heaviest chain that must follow after m
    --     finishes. Thus weight(m) approximates the longest remaining path
    --     (a.k.a. the “critical path”) if we were to start m now.
    -- We then pick among ready modules by descending weight so that long/heavy
    -- chains are started early, reducing the risk of a late long‑tail where one
    -- big module finishes much later than the rest.
    computeCriticalWeights :: M.Map A.ModName Integer -> M.Map A.ModName Integer
    computeCriticalWeights cm = cwMap
      where
        costOf m = M.findWithDefault 0 m cm
        depsOf m = M.findWithDefault [] m revMap   -- dependents (who imports m)
        cwMap    = M.fromList [ (m, costOf m + max0 [ weight d | d <- depsOf m ]) | m <- M.keys indeg ]
        weight m = M.findWithDefault 0 m cwMap
        max0 []  = 0
        max0 xs  = maximum xs

    -- Scheduler helpers ---------------------------------------------------
    -- Compile or reuse a single module using a read‑only environment snapshot.
    -- It decides staleness by comparing recorded interface hashes of imports
    -- with those of already-finished project modules (ifaceMap) or with header
    -- hashes for external deps. For stale tasks it may parse the source and
    -- compile; for fresh tasks it avoids compilation. Returns (module name,
    -- public interface TEnv, module docstring, interface hash). No shared state
    -- is mutated here; the compiled output of the module is returned and
    -- integrated by the coordinator into the accumulated Env.
    -- Compile or reuse a single module using a read‑only env snapshot.
    -- Logic:
    -- - ActonTask always compiles (needBySource = True).
    -- - TyTask: compare recorded iface hashes for each project import against
    --   ifaceMap (from already‑processed project modules); for external deps
    --   read the provider’s .ty header. Any difference → recompile
    --   (needByImports = True). Missing imports is an error.
    -- - On compile: upgrade TyTask by parsing .act; doTask runs the pipeline
    --   and writes .ty; return (iface TEnv, doc, ifaceHash).
    -- - On fresh TyTask: do not decode .ty here to keep no‑op fast; return
    --   (Nothing, doc, recorded ifaceHash). Coordinator extends Env only for
    --   compiled modules; roots codegen lazily loads NameInfo if needed.
    -- - After compile: update ifaceMap with the new ifaceHash for dependents.
    doOne :: Acton.Env.Env0 -> M.Map A.ModName B.ByteString -> A.ModName -> IO (A.ModName, [(A.Name, A.NameInfo)], Maybe String, B.ByteString)
    doOne envSnap ifaceMap mn = do
      let t = case M.lookup mn projMap of
                Just x -> x
                Nothing -> error ("Internal error: missing task for " ++ modNameToString mn)
          needBySource = case t of { ActonTask{} -> True; _ -> False }
      (needByImports, deltas) <- case t of
                         TyTask{ tyImports = imps } -> do
                           triples <- forM imps $ \(m, recorded) -> do
                             curr <- if Data.Set.member m projMods
                                       then case M.lookup m ifaceMap of
                                              Just h  -> return h
                                              Nothing -> error ("Internal error: missing iface hash for dep " ++ modNameToString m)
                                       else do
                                         mih <- getIfaceHashCached paths m
                                         case mih of
                                           Just ih -> return ih
                                           Nothing -> printErrorAndCleanAndExit ("Type interface file not found or unreadable for " ++ modNameToString m) gopts opts paths
                             return (m, recorded, curr)
                           let ds = [ (m, old, new) | (m, old, new) <- triples, old /= new ]
                           return (not (null ds), ds)
                         _ -> return (False, [])
      let forceAlt    = altOutput opts && mn == (modName paths)
          needCompile = needBySource || needByImports || forceAlt
          short8 bs   = take 8 (B.unpack $ Base16.encode bs)
      if needCompile
        then do
          when (C.verbose gopts) $ do
            if needBySource
              then putStrLn ("  Stale " ++ modNameToString mn ++ ": source changed")
              else when needByImports $ do
                let fmt (m, old, new) = modNameToString m ++ " " ++ short8 old ++ " → " ++ short8 new
                putStrLn ("  Stale " ++ modNameToString mn ++ ": iface changes in " ++ Data.List.intercalate ", " (map fmt deltas))
            cap <- getMyCap
            putStrLn ("  Building [cap " ++ show cap ++ "] " ++ modNameToString mn)
          t' <- case t of
                  ActonTask{} -> return t
                  TyTask{}    -> do
                    let actFile = srcFile paths mn
                    (srcContent, m) <- parseActFile gopts opts paths mn actFile
                    return $ ActonTask mn srcContent m
          (ifaceTE, mdoc, ih) <- doTask gopts opts paths envSnap t'
          updateIfaceHashCache mn ih
          return (mn, ifaceTE, mdoc, ih)
        else do
          when (C.verbose gopts) $ do
            putStrLn ("  Fresh " ++ modNameToString mn ++ ": using cached .ty")
          (ifaceTE, mdoc, ih) <- doTask gopts opts paths envSnap t
          updateIfaceHashCache mn ih
          return (mn, ifaceTE, mdoc, ih)

    -- Start up to k modules from the ready list, each as an Async worker using
    -- the provided environment snapshot. Returns the remaining ready list and
    -- the extended set of running workers (Async handle paired with its module
    -- name).
    scheduleMore :: Int -> [A.ModName]
                 -> [(Async (A.ModName, [(A.Name, A.NameInfo)], Maybe String, B.ByteString), A.ModName)]
                 -> M.Map A.ModName B.ByteString -> Acton.Env.Env0 -> M.Map A.ModName Integer
                 -> IO ([A.ModName]
                       , [(Async (A.ModName, [(A.Name, A.NameInfo)], Maybe String, B.ByteString), A.ModName)])
    scheduleMore k rdy running res envSnap cw = do
      let prio m = M.findWithDefault 0 m cw
          rdySorted = Data.List.sortOn (Data.Ord.Down . prio) rdy
          (toStart, rdy') = splitAt k rdySorted
      new <- forM toStart $ \mn -> do
                a <- async (doOne envSnap res mn)
                return (a, mn)
      return (rdy', new ++ running)

    -- Coordinator loop for the concurrent scheduler. It tops up workers to the
    -- concurrency budget, waits for one completion, merges the finished
    -- module’s interface into envAcc, updates indegrees/ready/pending, and
    -- repeats until all modules are processed. Returns the final Env.
    loop :: [A.ModName]
         -> [(Async (A.ModName, [(A.Name, A.NameInfo)], Maybe String, B.ByteString), A.ModName)]
         -> M.Map A.ModName B.ByteString
         -> M.Map A.ModName Int
         -> Data.Set.Set A.ModName
         -> Acton.Env.Env0
         -> Int -> M.Map A.ModName Integer
         -> IO Acton.Env.Env0
    loop rdy running res ind pend envAcc maxPar cw = do
      -- Top up the running set with as many ready modules as available slots.
      (rdy1, running1) <- scheduleMore (maxPar - length running) rdy running res envAcc cw
      -- If there’s nothing running and nothing ready:
      if null running1 && null rdy1
        then if Data.Set.null pend
               then return envAcc
               else printErrorAndCleanAndExit "Internal error: scheduler deadlock (non-empty pending with no ready)." gopts opts paths
        else do
          -- Wait for one worker to finish, obtain (module, iface TEnv, doc, ifaceHash).
          (doneA, (mnDone, ifaceTE, mdoc, ih)) <- waitAny $ map fst running1
          -- Remove the finished worker from the running set.
          let running2 = filter ((/= doneA) . fst) running1
              -- Update ifaceMap with the just-computed interface hash.
              res2     = M.insert mnDone ih res
              -- Remove mnDone from the pending set.
              pend2    = Data.Set.delete mnDone pend
              -- For each dependent of mnDone, decrement indegree by 1.
              ind2     = case M.lookup mnDone revMap of
                           Nothing -> ind
                           Just ds -> foldl' (\m d -> M.adjust (\x -> x-1) d m) ind ds
              -- Newly ready modules are those whose indegree just dropped to 0, excluding ones
              -- already in the ready list or currently running.
              newlyReady = [ m | m <- Data.Set.toList pend2
                               , M.findWithDefault 0 m ind2 == 0
                               , not (m `elem` rdy1)
                               , not (m `elem` map snd running2)
                               ]
              -- Extend the ready list with the new wave of ready modules.
              rdy2 = rdy1 ++ newlyReady
              -- Extend the accumulated environment with this module’s interface (public TEnv + doc).
              envAcc' = Acton.Env.addMod mnDone ifaceTE mdoc envAcc
          -- Tail recurse with updated ready/running/indeg/pending/env state.
          loop rdy2 running2 res2 ind2 pend2 envAcc' maxPar cw

compileBins:: C.GlobalOptions -> C.CompileOptions -> Paths -> Acton.Env.Env0 -> [CompileTask] -> [BinTask] -> IO ()
compileBins gopts opts paths env tasks binTasks = do
    iff (not (altOutput opts)) $ do
      zigBuild env gopts opts paths tasks binTasks
    return ()


-- Generate documentation index for a project build by reading module docstrings
-- from the current tasks. Uses TyTask header docs when available to avoid
-- parsing/decoding; falls back to extracting from ActonTask ASTs.
generateProjectDocIndex :: C.GlobalOptions -> C.CompileOptions -> Paths -> [String] -> IO ()
generateProjectDocIndex gopts opts paths srcFiles = do
    unless (C.skip_build opts || C.only_build opts || isTmp paths) $ do
        let docDir = joinPath [projPath paths, "out", "doc"]
        createDirectoryIfMissing True docDir
        tasks <- mapM (readModuleTask gopts opts paths) srcFiles
        entries <- forM tasks $ \t -> case t of
                          ActonTask mn _src m -> return (mn, DocP.extractDocstring (A.mbody m))
                          TyTask { name = mn, tyDoc = mdoc } -> return (mn, mdoc)
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
                task <- readModuleTask gopts opts paths actFile
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

doTask :: C.GlobalOptions -> C.CompileOptions -> Paths -> Acton.Env.Env0 -> CompileTask -> IO ([(A.Name, A.NameInfo)], Maybe String, B.ByteString)
doTask gopts opts paths env TyTask{ name = mn, tyIfaceHash = h } = do
    -- Fresh module: read interface (TEnv + doc) from .ty header to extend Env in coordinator
    mty <- Acton.Env.findTyFile (searchPath paths) mn
    case mty of
      Just tyF -> do
        (_ms, nmod, _tmod, _si, _ti, _ni, _te, _tm) <- InterfaceFiles.readFile tyF
        let A.NModule te mdoc = nmod
        return (te, mdoc, h)
      Nothing -> printErrorAndCleanAndExit ("Type interface file not found for " ++ modNameToString mn) gopts opts paths
doTask gopts opts paths env t@(ActonTask mn src m) = do
    -- In --only-build mode, do not run compilation passes or write files.
    if C.only_build opts then do
        iff (not (quiet gopts opts)) (putStrLn ("  Skipping compilation of " ++ makeRelative (srcDir paths) actFile ++ " (--only-build)"))
        -- Return current interface (TEnv + doc) and iface hash from existing .ty
        mty <- Acton.Env.findTyFile (searchPath paths) mn
        case mty of
          Just tyF -> do
            (_ms, nmod, _tmod, _si, _ti, _ni, _te, _tm) <- InterfaceFiles.readFile tyF
            let A.NModule te mdoc = nmod
            hdrE <- (try :: IO a -> IO (Either SomeException a)) $ InterfaceFiles.readHeader tyF
            ih <- case hdrE of
                    Right (_srcH, ihash, _impsH, _rootsH, _docH) -> return ihash
                    _ -> return B.empty
            return (te, mdoc, ih)
          Nothing -> return ([], Nothing, B.empty)
    else do
      iff (not (quiet gopts opts))  (putStrLn("  Compiling " ++ makeRelative (srcDir paths) actFile
              ++ " with " ++ show (C.optimize opts)))

      timeStart <- getTime Monotonic

      createDirectoryIfMissing True (getModPath (projTypes paths) mn)
      (_env', ifaceHash, ifaceTE, mdoc) <- runRestPasses gopts opts paths env m src
        `catch` handle gopts opts "Compilation error" generalError src paths mn
        `catch` handle gopts opts "Compilation error" Acton.Env.compilationError src paths mn
        `catch` (\err -> handleDiagnostic gopts opts paths (modName paths) $ mkErrorDiagnostic filename src $ Acton.TypeM.typeReport err filename src)
      timeEnd <- getTime Monotonic
      iff (not (quiet gopts opts)) $ putStrLn ("   Finished compilation of " ++ modNameToString mn ++ " in  " ++ fmtTime(timeEnd - timeStart))
      return (ifaceTE, mdoc, ifaceHash)
  where actFile             = srcFile paths mn
        filename            = modNameToFilename mn
        outbase             = outBase paths mn
        tyFile              = outbase ++ ".ty"
        hFile               = outbase ++ ".h"
        cFile               = outbase ++ ".c"


isGitAvailable :: IO Bool
isGitAvailable = do
    (exitCode, _, _) <- readProcessWithExitCode "git" ["--version"] ""
    return $ exitCode == ExitSuccess


-- Check if any other non-standard output is enabled, like --cgen or --sigs
altOutput opts =
  (C.parse opts) || (C.kinds opts) || (C.types opts) || (C.sigs opts) || (C.norm opts) || (C.deact opts) || (C.cps opts) || (C.llift opts) || (C.box opts) || (C.hgen opts) || (C.cgen opts)

runRestPasses :: C.GlobalOptions -> C.CompileOptions -> Paths -> Acton.Env.Env0 -> A.Module -> String -> IO (Acton.Env.Env0, B.ByteString, [(A.Name, A.NameInfo)], Maybe String)
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
                      -- Compute hash of source content from raw file bytes (stable across encoding)
                      srcBytes <- B.readFile actFile
                      let srcHash = SHA256.hash srcBytes
                      -- Pre-compute list of root-eligible actors and store in .ty header
                      let roots = case nmod of
                                     A.NModule te _ -> [ n | (n,i) <- te, rootEligible i ]
                                     _              -> []
                      let mdoc = case nmod of A.NModule _ d -> d; _ -> Nothing
                      -- Resolve import interface hashes from headers; missing entries are hard errors
                      impsWithHash <- forM mrefs $ \mref -> do
                        mih <- getIfaceHashCached paths mref
                        case mih of
                          Just ih -> return (mref, ih)
                          Nothing -> printErrorAndCleanAndExit ("Type interface file not found or unreadable for " ++ modNameToString mref) gopts opts paths
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
                      let emitLines = not (C.dbg_no_lines opts)
                      (n,h,c) <- Acton.CodeGen.generate liftEnv relSrcBase srcContent emitLines boxed hexHash
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

                      return ( Acton.Env.addMod mn iface mdoc (env0 `Acton.Env.withModulesFrom` env)
                             , ifaceHash
                             , iface
                             , mdoc )

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
        if n `elem` rootsHeader then do
          c <- Acton.CodeGen.genRoot env qn
          createDirectoryIfMissing True (takeDirectory rootFile)
          writeFile rootFile c
          return (Just binTask)
        else do
          -- No slow fallback: if header does not list the root, skip generating root C for this task
          return Nothing

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

zigBuild :: Acton.Env.Env0 -> C.GlobalOptions -> C.CompileOptions -> Paths -> [CompileTask] -> [BinTask] -> IO ()
zigBuild env gopts opts paths tasks binTasks = do
    allBinTasks <- mapM (writeRootC env gopts opts paths tasks) binTasks
    let realBinTasks = catMaybes allBinTasks
    -- Clean out/types: drop stray outputs but preserve all root stubs so that
    -- switching between `acton build` and `acton test` never deletes the other
    -- mode’s executables. We preserve every existing *.root.c and *.test_root.c
    -- under out/types regardless of whether this run produced them.
    absOutFiles <- getFilesRecursive (projTypes paths)
    let isRootStub f =
            let ext  = takeExtension f
                bext = takeExtension (takeBaseName f)
            in ext == ".c" && (bext == ".root" || bext == ".test_root")
        roots = filter isRootStub absOutFiles
    removeOrphanFiles paths tasks roots
    iff (not (quiet gopts opts)) $ putStrLn("  Final compilation step")
    -- Remove orphaned executables, i.e. .act files that used to have a root
    -- actor and thus outputted a binary executable but no longer has a root
    -- actor. We only do this for projects, not temp / scratch dir builds.
    iff (not (isTmp paths)) $ removeOrphanExecutables (binDir paths) (projTypes paths) realBinTasks
    timeStart <- getTime Monotonic

    homeDir <- getHomeDirectory
    let local_cache_dir = joinPath [ homeDir, ".cache", "acton", "zig-local-cache" ]
        global_cache_dir = joinPath [ homeDir, ".cache", "acton", "zig-global-cache" ]
        no_threads = if isWindowsOS (C.target opts) then True else C.no_threads opts


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
