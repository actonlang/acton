module Acton.Project
  ( ProjectError(..)
  , throwProjectError
  , Paths(..)
  , ProjCtx(..)
  , BackgroundCompilerLock
  , backgroundCompilerLockPath
  , tryBackgroundCompilerLock
  , releaseBackgroundCompilerLock
  , projectLockPath
  , withProjectLockOnWait
  , withProjectLock
  , systemTypePaths
  , scratchBuildSpec
  , findProjectDir
  , isActonProjectRoot
  , findPaths
  , discoverProjects
  , pathsForModule
  , searchPathForProject
  , moduleNameFromFile
  , enumerateProjectModules
  , pruneMissingModuleOutputs
  , pruneMissingChangedModuleOutputs
  , normalizeDepOverrides
  , applyDepOverrides
  , collectDepTypePaths
  , depTypePathsFromMap
  , resolveDepBase
  , loadBuildSpec
  , srcFile
  , outBase
  , tyDbPath
  , srcBase
  , getModPath
  , special_projects
  , normalizePathSafe
  , isAbsolutePath
  , collapseDots
  , rebasePath
  ) where

import Prelude hiding (readFile)

import qualified Acton.BuildSpec as BuildSpec
import qualified Acton.CommandLineParser as C
import qualified Acton.Fingerprint as Fingerprint
import qualified Acton.Syntax as A
import qualified InterfaceFiles

import Control.Exception (Exception, IOException, catch, finally, throwIO, try)
import Control.Monad (foldM, forM, forM_, unless, when)
import Data.Bits (shiftL, shiftR, (.|.))
import Data.Char (isAlpha, isDigit)
import Data.List (foldl', isSuffixOf)
import qualified Data.List
import qualified Data.Map as M
import Data.Maybe (isJust)
import qualified Data.Set
import Data.Word (Word32, Word64)
import System.Directory
import System.Directory.Recursive (getFilesRecursive)
import System.Environment (getExecutablePath, lookupEnv)
import System.FileLock (FileLock, SharedExclusive(Exclusive), tryLockFile, unlockFile, withFileLock)
import System.FilePath.Posix
import System.IO (IOMode(ReadMode), hGetContents, hSetEncoding, openFile, utf8)
import System.Random (randomRIO)

newtype ProjectError = ProjectError String deriving (Show)

instance Exception ProjectError

-- | Raise a ProjectError for library callers.
throwProjectError :: String -> IO a
throwProjectError msg = throwIO (ProjectError msg)


-- | Compute the subdirectory for a module within a types root.
-- Used when creating directories before writing .tydb files.
getModPath :: FilePath -> A.ModName -> FilePath
getModPath path mn =
  joinPath [path, joinPath $ init $ A.modPath mn]

special_projects :: [String]
special_projects = ["", "base", "acton_scratch"]

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
                    modName     :: A.ModName,
                    projName    :: String
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
                     projBuildSpec :: BuildSpec.BuildSpec,
                     projDeps     :: [(String, FilePath)]          -- resolved dependency roots (abs paths)
                   } deriving (Show)

systemTypePaths :: FilePath -> FilePath -> [FilePath]
systemTypePaths sys types =
    [joinPath [sys, "std", "out", "types"], types]

type FingerprintMap = M.Map String FilePath

scratchBuildSpec :: FilePath -> BuildSpec.BuildSpec
scratchBuildSpec projRoot =
    let name = "acton_scratch"
        prefix = Fingerprint.fingerprintPrefixForName name
        suffix = Fingerprint.fingerprintPrefixForName projRoot
        fp = Fingerprint.formatFingerprint (Fingerprint.updateFingerprintPrefix prefix (fromIntegral suffix))
    in BuildSpec.BuildSpec
         { BuildSpec.specName = name
         , BuildSpec.specDescription = Nothing
         , BuildSpec.fingerprint = fp
         , BuildSpec.dependencies = M.empty
         , BuildSpec.zig_dependencies = M.empty
         , BuildSpec.libraries = M.empty
         }


normalizeFingerprintKey :: String -> Maybe String
normalizeFingerprintKey raw =
    Fingerprint.formatFingerprint <$> Fingerprint.parseFingerprint raw

fingerprintKeyFromSpec :: BuildSpec.BuildSpec -> String
fingerprintKeyFromSpec spec =
    case normalizeFingerprintKey (BuildSpec.fingerprint spec) of
      Just fp -> fp
      Nothing -> BuildSpec.fingerprint spec

applyFingerprint :: FilePath -> BuildSpec.BuildSpec -> FingerprintMap -> (FilePath, FingerprintMap, String)
applyFingerprint path spec fpMap =
    let fp = fingerprintKeyFromSpec spec
    in case M.lookup fp fpMap of
         Just canonical -> (canonical, fpMap, fp)
         Nothing -> (path, M.insert fp path fpMap, fp)

-- | Discover all projects reachable from a root project.
-- Follows Build.act dependencies, applies overrides/pins, and
-- returns a map from project root to ProjCtx while skipping duplicates.
discoverProjects :: C.GlobalOptions -> FilePath -> FilePath -> [(String, FilePath)] -> IO (M.Map FilePath ProjCtx)
discoverProjects gopts sysAbs rootProj depOverrides = do
    rootAbs <- normalizePathSafe rootProj
    rootSpec0 <- loadBuildSpec rootAbs
    rootSpec  <- applyDepOverrides rootAbs depOverrides rootSpec0
    let rootPins = BuildSpec.dependencies rootSpec
        (_, fpMap0, _) = applyFingerprint rootAbs rootSpec M.empty
    fst <$> go rootAbs Data.Set.empty M.empty fpMap0 rootPins rootAbs (Just rootSpec)
  where
    go root seen acc fpMap pins dir mSpec = do
      dirAbs <- normalizePathSafe dir
      if Data.Set.member dirAbs seen
        then return (acc, fpMap)
        else do
          spec0 <- case mSpec of
                     Just s -> return s
                     Nothing -> loadBuildSpec dirAbs
          spec <- applyDepOverrides dirAbs depOverrides spec0
          let (_, fpMap1, _) = applyFingerprint dirAbs spec fpMap
          (deps, fpMap2) <- do
            let depsList = M.toList (BuildSpec.dependencies spec)
            foldM (collectDep pins dirAbs) ([], fpMap1) depsList
          let outDir   = joinPath [dirAbs, "out"]
              typesDir = joinPath [outDir, "types"]
              srcDir'  = joinPath [dirAbs, "src"]
              ctx = ProjCtx { projRoot = dirAbs
                            , projOutDir = outDir
                            , projTypesDir = typesDir
                            , projSrcDir = srcDir'
                            , projSysPath = sysAbs
                            , projSysTypes = joinPath [sysAbs, "base", "out", "types"]
                            , projBuildSpec = spec
                            , projDeps = [ (n, p) | (n, p, _) <- reverse deps ]
                            }
              acc' = M.insert dirAbs ctx acc
              seen' = Data.Set.insert dirAbs seen
          foldM (step root seen' pins) (acc', fpMap2) (reverse deps)

    collectDep pins base (accDeps, fpMap) (depName, dep) = do
      let (chosenDep, conflict) =
            case M.lookup depName pins of
              Nothing -> (dep, Nothing)
              Just pinDep ->
                if pinDep == dep
                  then (dep, Nothing)
                  else (pinDep, Just dep)
      when (isJust conflict) $
        unless (C.quiet gopts) $
          putStrLn ("Warning: dependency '" ++ depName ++ "' in " ++ base
                    ++ " overridden by root pin")
      depBase <- resolveDepBase base depName chosenDep
      depAbs  <- normalizePathSafe depBase
      (depPath, fpMap', depSpec) <- canonicalizeDep depAbs fpMap
      return ((depName, depPath, depSpec) : accDeps, fpMap')

    canonicalizeDep depAbs fpMap = do
      spec0 <- loadBuildSpec depAbs
      spec <- applyDepOverrides depAbs depOverrides spec0
      let (canonPath, fpMap', fp) = applyFingerprint depAbs spec fpMap
      when (canonPath /= depAbs) $
        unless (C.quiet gopts) $
          putStrLn ("Warning: dependency fingerprint " ++ fp
                    ++ " at " ++ depAbs ++ " deduplicated to " ++ canonPath)
      let depSpec = if canonPath == depAbs then Just spec else Nothing
      return (canonPath, fpMap', depSpec)

    step root seen pins (acc, fpMap) (_, depBase, mSpec) =
      go root seen acc fpMap pins depBase mSpec

-- Given a FILE and optionally --syspath PATH:
-- 'sysPath' is the path to the system directory as given by PATH, defaulting to the acton executable directory.
-- 'sysTypes' is directory "types" under 'sysPath'.
-- 'projPath' is the closest parent directory of FILE that contains a Build.act
-- file, or a temporary directory in "/tmp" if no such parent exists.
-- 'projOut' is directory "out" under 'projPath'.
-- 'projTypes' is directory "types" under 'projOut'.
-- 'binDir' is the directory prefix of FILE if 'projPath' is temporary, otherwise it is directory "bin" under 'projOut'
-- 'srcDir' is the directory prefix of FILE if 'projPath' is temporary, otherwise it is directory "src" under 'projPath'
-- 'fileExt' is file suffix of FILE.
-- 'modName' is the module name of FILE (its path after 'src' except 'fileExt', split at every '/')

-- | Compute the source file path for a module under its project src dir.
srcFile                 :: Paths -> A.ModName -> FilePath
srcFile paths mn        = srcBase paths mn ++ ".act"

-- | Compute the output base path (without extension) for a module.
-- Used to locate .tydb/.c/.h output under the project's types directory.
outBase                 :: Paths -> A.ModName -> FilePath
outBase paths mn        = joinPath (projTypes paths : A.modPath mn)

tyDbPath                :: Paths -> A.ModName -> FilePath
tyDbPath paths mn       = outBase paths mn ++ InterfaceFiles.interfaceExt

-- | Compute the module path without extension under the project's src dir.
-- Used to derive the .act path or related per-module files.
srcBase                 :: Paths -> A.ModName -> FilePath
srcBase paths mn        = --trace ("## srcBase " ++ prstr mn ++ " --> " ++ fpath) $
                          fpath
  where fpath           = joinPath (srcDir paths : names)
        names           = case A.modPath mn of
                            n:ns | n == projName paths -> ns
                            ns -> ns


-- | Walk upward from a path to find a project root.
-- A project root is identified by Build.act plus a
-- src/ directory; returns Nothing if we reach filesystem root.

-- | Check whether a directory is an Acton project root.
-- Requires Build.act and a src/ directory.
isActonProjectRoot :: FilePath -> IO Bool
isActonProjectRoot path = do
    runacton <- lookupEnv "ACTON_RUNACTON"
    if isJust runacton
      then return False
      else do
        hasBuildAct <- doesFileExist (path </> "Build.act")
        hasSrcDir <- doesDirectoryExist (path </> "src")
        return (hasBuildAct && hasSrcDir)

findProjectDir :: FilePath -> IO (Maybe FilePath)
findProjectDir path = do
    isProjectRoot <- isActonProjectRoot path
    if isProjectRoot
        then return (Just path)
        else if path == takeDirectory path  -- Check if we're at root
            then return Nothing
            else findProjectDir (takeDirectory path)


-- | Opaque handle for the lifetime lock of a long-running project compiler.
data BackgroundCompilerLock = BackgroundCompilerLock FileLock FilePath

-- | Path to the lock held for the lifetime of a long-running project compiler.
--
-- This lock only establishes unique ownership of a background compiler
-- process such as LSP or watch mode. It does not imply that project state
-- is current, so all compile/build work must still take 'withProjectLock'.
backgroundCompilerLockPath :: FilePath -> FilePath
backgroundCompilerLockPath projDir =
    joinPath [projDir, ".acton.compile.lock"]

-- | Acquire the long-running compiler lock for a project root, if available.
tryBackgroundCompilerLock :: FilePath -> IO (Maybe BackgroundCompilerLock)
tryBackgroundCompilerLock projDir = do
    let lockPath = backgroundCompilerLockPath projDir
    mlock <- tryLockFile lockPath Exclusive
    return ((\lock -> BackgroundCompilerLock lock lockPath) <$> mlock)

-- | Release the long-running compiler lock and remove its lock file if possible.
releaseBackgroundCompilerLock :: BackgroundCompilerLock -> IO ()
releaseBackgroundCompilerLock (BackgroundCompilerLock lock lockPath) = do
    unlockFile lock
    mlock <- tryLockFile lockPath Exclusive
    case mlock of
      Nothing -> return ()
      Just lock2 -> do
        removeFile lockPath `catch` handleNotExists
        unlockFile lock2
  where
    handleNotExists :: IOException -> IO ()
    handleNotExists _ = return ()

-- | Path to the single per-project compiler work lock.
projectLockPath :: FilePath -> FilePath
projectLockPath projDir =
    joinPath [projDir, ".acton.lock"]

-- | Run an action while holding the single per-project compiler work lock.
--
-- If the lock is not immediately available, run the callback once before
-- blocking until the lock can be acquired.
withProjectLockOnWait :: FilePath -> IO () -> IO a -> IO a
withProjectLockOnWait projDir onWait action = do
    let lockPath = projectLockPath projDir
    mlock <- tryLockFile lockPath Exclusive
    case mlock of
      Just lock -> action `finally` unlockFile lock
      Nothing -> do
        onWait
        withFileLock lockPath Exclusive (\_ -> action)

-- | Run an action while holding the single per-project compiler work lock.
withProjectLock :: FilePath -> IO a -> IO a
withProjectLock projDir action =
    withProjectLockOnWait projDir (return ()) action


-- | Compute Paths for a given source file and compile options.
-- Resolves the project root (or temp root), output dirs, and search path,
-- creating required directories along the way.
findPaths               :: FilePath -> C.CompileOptions -> IO Paths
findPaths actFile opts  = do execDir <- takeDirectory <$> getExecutablePath
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
                             depOverrides <- normalizeDepOverrides projPath (C.dep_overrides opts)
                             (projName,depTypePaths) <- if isTmp then return ("",[]) else collectDepTypePaths projPath depOverrides
                             let sPaths = [projTypes] ++ depTypePaths ++ (C.searchpath opts) ++ systemTypePaths sysPath sysTypes
                                 modName = A.modName $ (if isTmp then dirInSrc else projName:dirInSrc) ++ [fileBody]
                             InterfaceFiles.registerSystemTypeRoots (systemTypePaths sysPath sysTypes)
                             createDirectoryIfMissing True binDir
                             createDirectoryIfMissing True projOut
                             createDirectoryIfMissing True projTypes
                             createDirectoryIfMissing True (getModPath projTypes modName)
                             return $ Paths sPaths sysPath sysTypes projPath projOut projTypes binDir srcDir isTmp fileExt modName projName
  where (fileBody,fileExt) = splitExtension $ takeFileName actFile

        analyze "/" ds  = do tmp <- canonicalizePath (C.tempdir opts)
                             return (True, tmp, [])
        analyze pre ds  = do isProjectRoot <- isActonProjectRoot pre
                             if isProjectRoot
                                then case ds of
                                    [] -> return $ (False, pre, [])
                                    "src":dirs -> return $ (False, pre, dirs)
                                    "out":"types":dirs -> return $ (False, pre, dirs)
                                    _ -> throwProjectError ("Source file is not in a valid project directory: " ++ joinPath ds)
                                else analyze (takeDirectory pre) (takeFileName pre : ds)

-- Module helpers for multi-project builds ---------------------------------------------------------

-- | Derive a module name from a file path under a project's src root.
-- The result uses path segments and strips the .act extension.
moduleNameFromFile :: FilePath -> String -> FilePath -> IO A.ModName
moduleNameFromFile srcBase proj actFile = do
    base <- normalizePathSafe srcBase
    file <- normalizePathSafe actFile
    let rel = dropExtension (makeRelative base file)
        names = splitDirectories rel
        mn = A.modName $ if proj `elem` special_projects then names else proj:names
    --traceM ("## from file " ++ actFile ++ " --> " ++ prstr mn)
    return mn

-- | Enumerate all .act files in a project and pair them with module names.
-- Used to seed the project module index for graph construction.
enumerateProjectModules :: ProjCtx -> IO [(FilePath, A.ModName)]
enumerateProjectModules ctx = do
    exists <- doesDirectoryExist (projSrcDir ctx)
    if not exists
      then return []
      else do
        files <- getFilesRecursive (projSrcDir ctx)
        let actFiles = filter (\f -> takeExtension f == ".act") files
            proj = BuildSpec.specName $ projBuildSpec ctx
            rootName = head . splitDirectories . makeRelative (projSrcDir ctx) . dropExtension
            depNames = map fst $ projDeps ctx
            depOverlaps = filter ((`elem` depNames) . rootName) actFiles

        when (not $ null depOverlaps) $
            throwProjectError ("Source files in project " ++ projRoot ctx ++ " overlap with declared dependencies:\n" ++
                               concat ["    " ++ makeRelative (projRoot ctx) f ++ "\n" | f <- depOverlaps])

        forM actFiles $ \f -> do
          mn <- moduleNameFromFile (projSrcDir ctx) proj f
          return (f, mn)

-- | Remove stale generated module artifacts when source modules disappear.
-- Prunes orphan .tydb/.c/.h outputs under out/types before task planning so
-- cached headers cannot mask deleted .act modules.
pruneMissingModuleOutputs :: ProjCtx -> IO ()
pruneMissingModuleOutputs ctx = do
    let srcRoot = projSrcDir ctx
        typesRoot = projTypesDir ctx
    typesExists <- doesDirectoryExist typesRoot
    when typesExists $ do
      srcExists <- doesDirectoryExist srcRoot
      srcMods <- if srcExists
                   then do
                     srcFiles <- getFilesRecursive srcRoot
                     let actFiles = filter (\f -> takeExtension f == ".act") srcFiles
                         modBases = map (normalise . dropExtension . makeRelative srcRoot) actFiles
                     return (Data.Set.fromList modBases)
                   else return Data.Set.empty
      outFiles <- getFilesRecursive typesRoot
      mapM_ (pruneFile srcMods typesRoot) outFiles
      tyDbs <- InterfaceFiles.listInterfaceDirsRecursive typesRoot
      mapM_ (pruneTyDb srcMods typesRoot) tyDbs
  where
    isRootStub rel ext =
      ext == ".c" &&
      (".root" `Data.List.isSuffixOf` dropExtension rel ||
       ".test_root" `Data.List.isSuffixOf` dropExtension rel)

    moduleBase rel ext
      | isRootStub rel ext = dropExtension (dropExtension rel)
      | otherwise = dropExtension rel

    pruneFile srcMods typesRoot absFile = do
      let ext = takeExtension absFile
      when (ext == ".c" || ext == ".h") $ do
        let rel = normalise (makeRelative typesRoot absFile)
            base = dropProjDir (moduleBase rel ext)
        unless (Data.Set.member base srcMods) $
          removeFile absFile `catch` ignoreNotExists

    pruneTyDb srcMods typesRoot absDir = do
      let rel = normalise (makeRelative typesRoot absDir)
          base = dropProjDir (dropExtension rel)
      unless (Data.Set.member base srcMods) $ do
        removePathForcibly absDir `catch` ignoreNotExists

    proj = BuildSpec.specName $ projBuildSpec ctx

    dropProjDir rel = case splitDirectories $ normalise (dropExtension rel) of
                        [n] -> n
                        n:ns | n == proj -> joinPath ns
                        ns -> joinPath ns

    ignoreNotExists :: IOException -> IO ()
    ignoreNotExists _ = return ()

-- | Incremental variant: prune only modules whose changed .act file no longer exists.
pruneMissingChangedModuleOutputs :: [ProjCtx] -> [FilePath] -> IO ()
pruneMissingChangedModuleOutputs ctxs changedPaths = do
    absChanged <- mapM normalizePathSafe changedPaths
    let actPaths = filter (\p -> takeExtension p == ".act") absChanged
    forM_ actPaths $ \actPath -> do
      exists <- doesFileExist actPath
      unless exists $
        mapM_ (pruneForCtx actPath) ctxs
  where
    pruneForCtx actPath ctx = do
      srcRoot <- normalizePathSafe (projSrcDir ctx)
      let srcRoot' = addTrailingPathSeparator (normalise srcRoot)
          actPath' = normalise actPath
      when (Data.List.isPrefixOf srcRoot' actPath') $ do
        let modBase = normalise (dropExtension (makeRelative srcRoot actPath'))
            outBase = projTypesDir ctx </> modBase
        removePathForcibly (outBase ++ InterfaceFiles.interfaceExt) `catch` ignoreNotExists
        mapM_ (\ext -> removeFile (outBase ++ ext) `catch` ignoreNotExists) [".c", ".h"]

    ignoreNotExists :: IOException -> IO ()
    ignoreNotExists _ = return ()

-- | Build a search path for module interfaces for a project.
-- Includes the project's types dir, dependency types, user searchpath, and
-- the system types directory.
searchPathForProject :: C.CompileOptions -> M.Map FilePath ProjCtx -> ProjCtx -> [FilePath]
searchPathForProject opts projMap ctx =
    let deps = depTypePathsFromMap projMap (projRoot ctx)
    in [projTypesDir ctx] ++ deps ++ (C.searchpath opts) ++ systemTypePaths (projSysPath ctx) (projSysTypes ctx)

-- | Construct a Paths record for a module within a project context.
-- Creates output directories and ensures the types directory exists.
pathsForModule :: C.CompileOptions -> M.Map FilePath ProjCtx -> ProjCtx -> A.ModName -> IO Paths
pathsForModule opts projMap ctx mn = do
    let sPaths = searchPathForProject opts projMap ctx
        bin = joinPath [projOutDir ctx, "bin"]
        src = projSrcDir ctx
        proj = BuildSpec.specName $ projBuildSpec ctx
        p = Paths sPaths (projSysPath ctx) (projSysTypes ctx) (projRoot ctx) (projOutDir ctx) (projTypesDir ctx) bin src False ".act" mn proj
    InterfaceFiles.registerSystemTypeRoots (systemTypePaths (projSysPath ctx) (projSysTypes ctx))
    createDirectoryIfMissing True bin
    createDirectoryIfMissing True (projOutDir ctx)
    createDirectoryIfMissing True (projTypesDir ctx)
    createDirectoryIfMissing True (getModPath (projTypesDir ctx) mn)
    return p


-- | Load a BuildSpec from Build.act.
-- Throws ProjectError on parse or validation failure.
loadBuildSpec :: FilePath -> IO BuildSpec.BuildSpec
loadBuildSpec dir = do
    let actPath = joinPath [dir, "Build.act"]
    actExists <- doesFileExist actPath
    if actExists
      then do
        content <- readFile actPath
        case BuildSpec.parseBuildActDetailed content of
          Left err ->
            case err of
              BuildSpec.MissingFingerprint name -> do
                suggestion <- suggestFingerprint name
                throwProjectError ("Missing fingerprint in " ++ actPath ++ ".\n"
                                   ++ "ERROR: Build.act requires `fingerprint`. For example: fingerprint = " ++ suggestion ++ "\n"
                                   ++ "HINT: Fingerprint = CRC32(name) in the high 32 bits + random low 32 bits. You may choose a different value.")
              BuildSpec.InvalidFingerprint name raw -> do
                suggestion <- suggestFingerprint name
                throwProjectError ("Invalid fingerprint " ++ raw ++ " in " ++ actPath
                                   ++ " (project name: " ++ show name ++ ").\n"
                                   ++ "Expected an unquoted 64-bit hex fingerprint like 0x1234abcd5678ef00.\n"
                                   ++ "Suggested fingerprint: " ++ suggestion)
              BuildSpec.MissingProjectName -> do
                suggestion <- suggestProjectName dir
                throwProjectError ("Missing project name in " ++ actPath ++ ".\n"
                                   ++ "Add: name = " ++ show suggestion)
              _ ->
                throwProjectError ("Failed to parse Build.act in " ++ dir ++ ":\n"
                                   ++ BuildSpec.renderBuildSpecParseError err)
          Right (spec, _, _) -> validateBuildSpec actPath spec
      else
        throwProjectError ("Missing Build.act in " ++ dir ++ ".\n"
                           ++ "Create Build.act with required name and fingerprint fields.")

validateBuildSpec :: FilePath -> BuildSpec.BuildSpec -> IO BuildSpec.BuildSpec
validateBuildSpec sourcePath spec = do
    validateProjectName sourcePath (BuildSpec.specName spec)
    validateFingerprint sourcePath (BuildSpec.specName spec) (BuildSpec.fingerprint spec)
    return spec

validateProjectName :: FilePath -> String -> IO ()
validateProjectName sourcePath name =
    if not (isProjectIdent name)
      then throwProjectError ("Invalid project name '" ++ name ++ "' in " ++ sourcePath ++ ".\n"
                              ++ "The name must be a valid Acton project name (letters, digits, underscore; cannot start with a digit).")
      else if length name > projectNameMax
        then throwProjectError ("Invalid project name '" ++ name ++ "' in " ++ sourcePath ++ ".\n"
                                ++ "The name must be at most " ++ show projectNameMax ++ " characters.")
        else return ()
  where
    isProjectIdent [] = False
    isProjectIdent (c:cs) =
      (isAlpha c || c == '_') && all isIdentChar cs
      where
        isIdentChar x = isAlpha x || isDigit x || x == '_'

suggestProjectName :: FilePath -> IO String
suggestProjectName dir = do
    resolved <- normalizePathSafe dir
    let base = takeBaseName resolved
    if null base || base == "."
      then return "my_project"
      else return base

suggestFingerprint :: String -> IO String
suggestFingerprint name = do
    let prefix = Fingerprint.fingerprintPrefixForName name
    low <- randomRIO (1, maxBound :: Word32)
    let fp = (fromIntegral prefix `shiftL` 32) .|. (fromIntegral low :: Word64)
    return (Fingerprint.formatFingerprint fp)

validateFingerprint :: FilePath -> String -> String -> IO ()
validateFingerprint sourcePath name fpRaw =
    case Fingerprint.parseFingerprint fpRaw of
      Nothing -> do
        suggestion <- suggestFingerprint name
        throwProjectError ("Invalid fingerprint '" ++ fpRaw ++ "' in " ++ sourcePath
                           ++ " (project name: " ++ show name ++ ").\n"
                           ++ "Expected an unquoted 64-bit hex fingerprint like 0x1234abcd5678ef00.\n"
                           ++ "Suggested fingerprint: " ++ suggestion)
      Just fp -> do
        let formatted = Fingerprint.formatFingerprint fp
            expectedPrefix = Fingerprint.fingerprintPrefixForName name
            expectedPrefixHex = Fingerprint.formatFingerprintPrefix expectedPrefix
            actualPrefix = fromIntegral (fp `shiftR` 32)
        if formatted == Fingerprint.fingerprintPlaceholder
          then do
            suggestion <- suggestFingerprint name
            throwProjectError ("Fingerprint placeholder " ++ formatted ++ " in " ++ sourcePath
                               ++ " is not valid.\n"
                               ++ "Suggested fingerprint: " ++ suggestion)
          else if expectedPrefix == actualPrefix
            then return ()
            else do
              suggestion <- suggestFingerprint name
              throwProjectError ("Fingerprint mismatch in " ++ sourcePath
                                 ++ " for project name " ++ show name ++ ".\n"
                                 ++ "Expected prefix: " ++ expectedPrefixHex
                                 ++ " (CRC32 of name).\n"
                                 ++ "Current fingerprint: " ++ formatted ++ "\n"
                                 ++ "Renames and forks require a new fingerprint for this name.\n"
                                 ++ "Suggested fingerprint: " ++ suggestion)


projectNameMax :: Int
projectNameMax = 32

-- | Treat drive-letter paths as absolute in addition to POSIX roots.
-- This keeps path normalization consistent on Windows hosts.
isAbsolutePath :: FilePath -> Bool
isAbsolutePath p =
    isAbsolute p ||
    case p of
      (c:':':_) -> isAlpha c
      _         -> False

-- | Normalize a path without failing if it does not exist.
-- Falls back to normalise for non-existent paths (useful for temporary builds).
normalizePathSafe :: FilePath -> IO FilePath
normalizePathSafe p = do
    res <- try (canonicalizePath p) :: IO (Either IOException FilePath)
    return $ either (const (normalise p)) id res

-- | Collapse "." and ".." segments without dropping leading ".." on relative paths.
-- Example: "a/b/../c/./d" becomes "a/c/d".
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
        "."  -> (acc, ups)
        ""   -> (acc, ups)
        ".." -> case acc of
                  (_:as) -> (as, ups)
                  []     -> (acc, ups + 1)
        _    -> (comp:acc, ups)

-- | Rebase a path against a base directory and normalize it.
-- Absolute paths are left as-is; relative paths are joined to the base.
rebasePath :: FilePath -> FilePath -> FilePath
rebasePath base p
  | isAbsolutePath p = normalise p
  | otherwise        = normalise (joinPath [base, p])

-- | Normalize --dep overrides relative to a base directory.
-- Absolute override paths are kept, relative paths are rebased and normalized.
normalizeDepOverrides :: FilePath -> [(String, FilePath)] -> IO [(String, FilePath)]
normalizeDepOverrides base overrides =
  mapM (\(n,p) -> do
           let absP0 = if isAbsolutePath p then p else joinPath [base, p]
           p' <- normalizePathSafe absP0
           return (n, p'))
       overrides

-- | Apply --dep overrides (NAME=PATH) to a BuildSpec.
-- Resolves override paths relative to the given base directory when needed.
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
          validateDepOverridePath depName absP
          let dep' = dep { BuildSpec.path = Just absP }
          return (M.insert depName dep' depsMap)

validateDepOverridePath :: String -> FilePath -> IO ()
validateDepOverridePath depName depPath = do
    exists <- doesDirectoryExist depPath
    unless exists $
      throwProjectError ("Dependency " ++ depName ++ " path does not exist: " ++ depPath ++ "\n"
                         ++ "Hint: Local dependency paths must point to an Acton project root\n"
                         ++ "(directory with src/ and Build.act).")
    isProjectRoot <- isActonProjectRoot depPath
    unless isProjectRoot $
      throwProjectError ("Dependency " ++ depName ++ " path is not an Acton project root: " ++ depPath ++ "\n"
                         ++ "Hint: Local dependency paths must point to an Acton project root\n"
                         ++ "(directory with src/ and Build.act).")

-- | Collect dependency type directories using an existing ProjCtx map.
-- Traverses dependency edges so downstream lookups can build search paths.
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

-- | Resolve a dependency's base directory from a BuildSpec PkgDep.
-- Prefers explicit path, otherwise uses the hashed cache location.
resolveDepBase :: FilePath -> String -> BuildSpec.PkgDep -> IO FilePath
resolveDepBase base name dep =
    case BuildSpec.path dep of
      Just p | not (null p) -> normalizePathSafe (rebasePath base p)
      _ -> case BuildSpec.hash dep of
             Just h -> do
               home <- getHomeDirectory
               normalizePathSafe (joinPath [home, ".cache", "acton", "deps", name ++ "-" ++ h])
             Nothing -> throwProjectError ("Dependency " ++ name ++ " has no path or hash")

-- | Recursively collect out/types paths for all declared dependencies.
-- Reads Build.act and follows dependency edges.
collectDepTypePaths :: FilePath -> [(String, FilePath)] -> IO (String,[FilePath])
collectDepTypePaths projDir overrides = do
  root <- normalizePathSafe projDir
  spec <- loadBuildSpec root
  (_, _, paths) <- go Data.Set.empty M.empty root spec
  return (BuildSpec.specName spec, paths)
  where
    go seen fpMap dir spec0 = do
      spec <- applyDepOverrides dir overrides spec0
      let (_, fpMap1, _) = applyFingerprint dir spec fpMap
      foldM (step dir) (seen, fpMap1, []) (M.toList (BuildSpec.dependencies spec))

    step base (seen, fpMap, acc) (depName, dep) = do
      depBase <- resolveDepBase base depName dep
      depAbs  <- normalizePathSafe depBase
      depExists <- doesDirectoryExist depAbs
      if not depExists
        then case BuildSpec.path dep of
               Just p | not (null p) ->
                 throwProjectError ("Dependency " ++ depName ++ " path does not exist: " ++ depAbs ++ "\n"
                                    ++ "Hint: Local dependency paths must point to an Acton project root\n"
                                    ++ "(directory with src/ and Build.act).")
               _ -> return (seen, fpMap, acc)
        else do
          (depPath, fpMap') <- canonicalizeDep depAbs fpMap
          let typesDir = joinPath [depPath, "out", "types"]
          if Data.Set.member depPath seen
            then return (seen, fpMap', acc)
            else do
              let seen' = Data.Set.insert depPath seen
              spec <- loadBuildSpec depPath
              (seenNext, fpMapNext, sub) <- go seen' fpMap' depPath spec
              return (seenNext, fpMapNext, acc ++ [typesDir] ++ sub)

    canonicalizeDep depAbs fpMap = do
      spec0 <- loadBuildSpec depAbs
      spec <- applyDepOverrides depAbs overrides spec0
      let (canonPath, fpMap', _) = applyFingerprint depAbs spec fpMap
      return (canonPath, fpMap')



-- | Read a UTF-8 text file with explicit encoding.
readFile :: FilePath -> IO String
readFile f = do
    h <- openFile f ReadMode
    hSetEncoding h utf8
    c <- hGetContents h
    return c
