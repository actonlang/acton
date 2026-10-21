-- | Acton projects on disk: the Paths of a project, the per-project context of
-- a multi-project build, loading and validating Build.act, discovering the
-- projects a project depends on, the project locks, and the paths of a
-- module's source and generated files.
module Acton.Project
  ( ProjectError(..)
  , throwProjectError
  , Paths(..)
  , ProjCtx(..)
  , mkProjCtx
  , BuildSpecStamp
  , readBuildSpecStamp
  , findPaths
  , findProjectDir
  , discoverProjects
  , RootPins
  , rootPins
  , selectDep
  , selectDepAndWarn
  , localPath
  , localZigPath
  , resolveDepBase
  , dependencyPathError
  , depCacheDir
  , depCacheEntry
  , zigGlobalCacheDir
  , projectPaths
  , createProjectDirectories
  , createModuleDirectories
  , systemTypePaths
  , moduleTypesDir
  , scratchBuildSpec
  , moduleNameFromFile
  , enumerateProjectModules
  , pruneMissingModuleOutputs
  , pruneMissingChangedModuleOutputs
  , srcFile
  , srcBase
  , outBase
  , tyDbPath
  , rootStubPath
  , rootBinaryName
  , filterActFile
  , modNameToFilename
  , isSpecialProject
  , addProjPrefix
  , dropProjPrefix
  , dropProjPrefixOrLib
  , BackgroundCompilerLock
  , backgroundCompilerLockPath
  , tryBackgroundCompilerLock
  , releaseBackgroundCompilerLock
  , withBackgroundCompilerLock
  , projectLockPath
  , withProjectLockOnWait
  , withProjectLock
  , loadBuildSpec
  , loadOverriddenSpec
  , normalizeDepOverrides
  , normalizePathSafe
  , isUnder
  , isAbsolutePath
  , collapseDots
  , rebasePath
  ) where

import qualified Acton.BuildSpec as BuildSpec
import qualified Acton.CodeGen
import qualified Acton.CommandLineParser as C
import qualified Acton.Fingerprint as Fingerprint
import qualified Acton.Syntax as A
import Acton.Syntax (modNameToString)
import qualified InterfaceFiles
import Utils (readFileUtf8)

import Control.Exception (Exception, IOException, catch, finally, throwIO, try)
import Control.Monad
import Data.Bits (shiftL, shiftR, (.|.))
import Data.Char (isAlpha, isDigit)
import Data.List (foldl', intercalate, isPrefixOf)
import Data.Maybe (isJust)
import qualified Data.Map as M
import qualified Data.Set
import Data.Time.Clock (UTCTime)
import Data.Word (Word32, Word64)
import System.Directory
import System.Directory.Recursive (getFilesRecursive)
import System.Environment (getExecutablePath, lookupEnv)
import System.FileLock (FileLock, SharedExclusive(Exclusive), tryLockFile, unlockFile, withFileLock)
import System.FilePath.Posix
import System.Random (randomRIO)


newtype ProjectError = ProjectError String deriving (Show)

instance Exception ProjectError

-- | Raise a ProjectError for library callers.
-- Used by project discovery and path helpers to stop on unrecoverable errors.
throwProjectError :: String -> IO a
throwProjectError msg = throwIO (ProjectError msg)

-- | A project's Build.act as of a moment: its modification time, Nothing
-- when the project has no Build.act.  Two equal stamps mean Build.act did
-- not change in between.
newtype BuildSpecStamp = BuildSpecStamp (Maybe UTCTime) deriving (Eq)

readBuildSpecStamp :: FilePath -> IO BuildSpecStamp
readBuildSpecStamp projDir = do
    exists <- doesFileExist buildAct
    BuildSpecStamp <$> if exists then Just <$> getModificationTime buildAct else return Nothing
  where buildAct = projDir </> "Build.act"

-- | Compute the subdirectory for a module within a types root.
-- Used when creating directories before writing .tydb files.
moduleTypesDir :: FilePath -> A.ModName -> FilePath
moduleTypesDir path mn =
  joinPath [path, joinPath $ init $ A.modPath mn]

-- | Keep only .act files when scanning directories.
-- Returns Just the file path for Acton sources and Nothing otherwise.
filterActFile :: FilePath -> Maybe FilePath
filterActFile file =
    case takeExtension file of
        ".act" -> Just file
        _ -> Nothing

-- | Whether the modules of the project named 'proj' go without the
-- project's name in front: the base library, and a scratch build.  A
-- scratch build has no Build.act.  Its Paths have an empty project name,
-- and the BuildSpec that scratchBuildSpec gives it is named acton_scratch.
isSpecialProject :: String -> Bool
isSpecialProject proj = proj `elem` ["", "base", "acton_scratch"]


-- Paths handling -------------------------------------------------------------------------------------

-- | The directories of a project's build.  A project root holds Build.act
-- and src/.  A scratch build builds a source file outside any project, with
-- the --tempdir directory as its root.
data Paths = Paths
  { searchPath :: [FilePath]    -- projTypes, dependency types, --searchpath, then the system types
  , sysPath    :: FilePath      -- --syspath, or the parent of the acton executable's directory
  , sysTypes   :: FilePath      -- base/out/types under sysPath
  , projPath   :: FilePath      -- the project root, or the --tempdir directory of a scratch build
  , projOut    :: FilePath      -- out under projPath
  , projTypes  :: FilePath      -- types under projOut
  , binDir     :: FilePath      -- bin under projOut, or the source file's directory for a scratch build
  , srcDir     :: FilePath      -- src under projPath, or the source file's directory for a scratch build
  , isTmp      :: Bool          -- a scratch build
  , projName   :: String        -- the name in Build.act, empty for a scratch build
  }

-- | One project of a multi-project build, identified by its absolute root
-- projRoot: its directories, its Build.act and its dependencies.
data ProjCtx = ProjCtx
  { projRoot      :: FilePath
  , projOutDir    :: FilePath
  , projTypesDir  :: FilePath
  , projSrcDir    :: FilePath
  , projSysPath   :: FilePath
  , projSysTypes  :: FilePath
  , projBuildSpec :: BuildSpec.BuildSpec
  , projDeps      :: [(String, FilePath)]   -- each dependency's name and absolute root
  } deriving (Show)

-- | The context of the project rooted at 'root' with its sources in 'src',
-- using the system directory 'sys'. Outputs go to out/ and out/types/ under
-- the root. A discovered project keeps its sources in src/ under the root,
-- and a scratch build keeps them next to the source file it was given.
mkProjCtx :: FilePath -> FilePath -> FilePath -> BuildSpec.BuildSpec -> [(String, FilePath)] -> ProjCtx
mkProjCtx sys root src spec deps =
    ProjCtx { projRoot      = root
            , projOutDir    = projectOutDir root
            , projTypesDir  = projectTypesDir root
            , projSrcDir    = src
            , projSysPath   = sys
            , projSysTypes  = sysTypesDir sys
            , projBuildSpec = spec
            , projDeps      = deps
            }

-- | The output directory of the project rooted at 'root'.
projectOutDir :: FilePath -> FilePath
projectOutDir root = joinPath [root, "out"]

-- | The directory of the interface files of the project rooted at 'root'.
projectTypesDir :: FilePath -> FilePath
projectTypesDir root = joinPath [projectOutDir root, "types"]

-- | The source directory of the project rooted at 'root'.
projectSrcDir :: FilePath -> FilePath
projectSrcDir root = joinPath [root, "src"]

-- | The types directory of the base library in the system directory 'sys'.
sysTypesDir :: FilePath -> FilePath
sysTypesDir sys = joinPath [sys, "base", "out", "types"]

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
         , BuildSpec.build_options = M.empty
         , BuildSpec.libraries = M.empty
         }


-- | The key of a project's fingerprint in a FingerprintMap: the fingerprint
-- in its formatted form, so that case and leading zeros do not matter, or as
-- written when it does not parse.
fingerprintKeyFromSpec :: BuildSpec.BuildSpec -> String
fingerprintKeyFromSpec spec =
    maybe raw Fingerprint.formatFingerprint (Fingerprint.parseFingerprint raw)
  where raw = BuildSpec.fingerprint spec

-- | Register the fingerprint of the project at 'path' unless a path is
-- registered for it already. The first path registered for a fingerprint is
-- the canonical path of every project with that fingerprint.
registerFingerprint :: FilePath -> BuildSpec.BuildSpec -> FingerprintMap -> FingerprintMap
registerFingerprint path spec =
    M.insertWith (\_ registered -> registered) (fingerprintKeyFromSpec spec) path

-- | Load the Build.act of the dependency at 'depAbs' with the --dep
-- overrides and register its fingerprint. Returns the canonical path of the
-- fingerprint, the updated map and the loaded spec. The canonical path is
-- another path when that path registered the fingerprint first.
canonicalDep :: [(String, FilePath)] -> FingerprintMap -> FilePath -> IO (FilePath, FingerprintMap, BuildSpec.BuildSpec)
canonicalDep overrides fpMap depAbs = do
    spec <- loadOverriddenSpec overrides depAbs
    let fpMap' = registerFingerprint depAbs spec fpMap
    return (fpMap' M.! fingerprintKeyFromSpec spec, fpMap', spec)

-- | The root pins of a build: the root project's concrete dependencies with
-- their local paths made absolute. Every project in the build uses the pin
-- of a name in place of its own declaration of that name.
type RootPins = M.Map String BuildSpec.PkgDep

-- | The root pins taken from the spec of the root project at 'root'.
rootPins :: FilePath -> BuildSpec.BuildSpec -> RootPins
rootPins root spec = M.map (rebaseDepPath root) (BuildSpec.concreteDependencies spec)

-- | The dependency that the project at 'base' uses for its declaration
-- 'dep' of 'name': the root pin of that name if there is one, otherwise the
-- declaration. The flag is True when the pin replaces a declaration that
-- differs from it.
selectDep :: RootPins -> FilePath -> String -> BuildSpec.PkgDep -> (BuildSpec.PkgDep, Bool)
selectDep pins base name dep =
    case M.lookup name pins of
      Nothing  -> (dep, False)
      Just pin -> (pin, pin /= rebaseDepPath base dep)

-- | The dependency 'selectDep' selects, after a warning, unless quiet, when
-- the root pin replaces a different declaration.
selectDepAndWarn :: C.GlobalOptions -> RootPins -> FilePath -> String -> BuildSpec.PkgDep -> IO BuildSpec.PkgDep
selectDepAndWarn gopts pins base name dep = do
    let (selected, replaced) = selectDep pins base name dep
    when (replaced && not (C.quiet gopts)) $
      putStrLn ("Warning: dependency '" ++ name ++ "' in " ++ base ++ " overridden by root pin")
    return selected

-- | Discover all projects reachable from a root project.
-- Follows Build.act dependencies, applies overrides/pins, and
-- returns a map from project root to ProjCtx while skipping duplicates.
discoverProjects :: C.GlobalOptions -> FilePath -> FilePath -> [(String, FilePath)] -> IO (M.Map FilePath ProjCtx)
discoverProjects gopts sysAbs rootProj depOverrides = do
    rootAbs <- normalizePathSafe rootProj
    rootSpec <- loadOverriddenSpec depOverrides rootAbs
    (projects, _) <- go Data.Set.empty M.empty (registerFingerprint rootAbs rootSpec M.empty)
                        (rootPins rootAbs rootSpec) rootAbs (Just rootSpec)
    resolveFollows projects
  where
    go seen acc fpMap pins dir mSpec = do
      dirAbs <- normalizePathSafe dir
      if Data.Set.member dirAbs seen
        then return (acc, fpMap)
        else do
          spec <- maybe (loadOverriddenSpec depOverrides dirAbs) return mSpec
          let fpMap1 = registerFingerprint dirAbs spec fpMap
          (deps, fpMap2) <- foldM (collectDep pins dirAbs) ([], fpMap1) (M.toList (BuildSpec.concreteDependencies spec))
          let ctx = mkProjCtx sysAbs dirAbs (projectSrcDir dirAbs) spec [ (n, p) | (n, p, _) <- reverse deps ]
              acc' = M.insert dirAbs ctx acc
              seen' = Data.Set.insert dirAbs seen
          foldM (step seen' pins) (acc', fpMap2) (reverse deps)

    collectDep pins base (accDeps, fpMap) (depName, dep) = do
      selected <- selectDepAndWarn gopts pins base depName dep
      depBase <- resolveDepBase base depName selected
      depAbs  <- normalizePathSafe depBase
      (depPath, fpMap', spec) <- canonicalDep depOverrides fpMap depAbs
      when (depPath /= depAbs && not (C.quiet gopts)) $
        putStrLn ("Warning: dependency fingerprint " ++ fingerprintKeyFromSpec spec
                  ++ " at " ++ depAbs ++ " deduplicated to " ++ depPath)
      let depSpec = if depPath == depAbs then Just spec else Nothing
      return ((depName, depPath, depSpec) : accDeps, fpMap')

    step seen pins (acc, fpMap) (_, depBase, mSpec) =
      go seen acc fpMap pins depBase mSpec

-- | Bind passive dependency references after concrete selection and deduplication.
-- A follows entry contributes neither a source nor a root pin. Every segment
-- traverses the selected project's own dependency declarations.
resolveFollows :: M.Map FilePath ProjCtx -> IO (M.Map FilePath ProjCtx)
resolveFollows projects = M.traverseWithKey resolveProject projects
  where
    resolveProject root ctx = do
      deps <- forM (M.keys (BuildSpec.dependencies (projBuildSpec ctx))) $ \name -> do
        target <- resolve [] root name
        return (name, target)
      return ctx { projDeps = deps }

    resolve seen root name
      | (root, name) `elem` seen =
          throwProjectError ("Cyclic dependency follows: " ++ intercalate " -> "
                             [ p ++ ":" ++ n | (p, n) <- reverse ((root, name) : seen) ])
      | otherwise =
          case M.lookup root projects of
            Nothing -> throwProjectError ("Missing project for dependency follows: " ++ root)
            Just ctx ->
              case M.lookup name (BuildSpec.dependencies (projBuildSpec ctx)) of
                Nothing -> throwProjectError ("Dependency follows target '" ++ name
                                               ++ "' is not declared in " ++ root ++ "/Build.act")
                Just dep ->
                  case BuildSpec.follows dep of
                    Nothing -> case lookup name (projDeps ctx) of
                                 Just target -> return target
                                 Nothing -> throwProjectError ("Unresolved dependency '" ++ name ++ "' in " ++ root)
                    Just ref -> do
                      target <- foldM (resolve ((root, name) : seen)) root (A.dottedNames ref)
                                `catch` \(ProjectError msg) ->
                                  throwProjectError ("Dependency '" ++ name ++ "' follows '" ++ ref
                                                     ++ "' in " ++ root ++ "/Build.act:\n" ++ msg)
                      let targetName = BuildSpec.specName (projBuildSpec (projects M.! target))
                      unless (name == targetName) $
                        throwProjectError ("Dependency '" ++ name ++ "' follows '" ++ ref
                                           ++ "', which resolves to project '" ++ targetName
                                           ++ "'. Use the project name as the dependency name.")
                      return target

-- | Compute the source file path for a module under its project src dir.
srcFile                 :: Paths -> A.ModName -> FilePath
srcFile paths mn        = srcBase paths mn ++ ".act"

-- | Compute the output base path (without extension) for a module.
-- Used to locate .tydb/.c/.h output under the project's types directory.
outBase                 :: Paths -> A.ModName -> FilePath
outBase paths mn        = joinPath (projTypes paths : A.modPath mn)

tyDbPath                :: Paths -> A.ModName -> FilePath
tyDbPath paths mn       = outBase paths mn ++ InterfaceFiles.interfaceExt

-- | Compute the root stub path for a module's root actor, or its test root.
-- Stubs drop the project prefix since build.zig names binaries after them.
rootStubPath            :: Paths -> Bool -> A.ModName -> FilePath
rootStubPath paths test mn = outBase paths (dropProjPrefix paths mn) ++ (if test then ".test_root.c" else ".root.c")

-- | Name of the binary in out/bin that build.zig builds from a root stub: the
-- stub's module path joined with dots, with .test_ in front for a test root.
rootBinaryName          :: Paths -> Bool -> A.ModName -> String
rootBinaryName paths test mn = (if test then ".test_" else "") ++ modNameToString (dropProjPrefix paths mn)

-- | All files generated for a module.
moduleOutputs           :: Paths -> A.ModName -> [FilePath]
moduleOutputs paths mn  = [ outBase paths mn ++ ext | ext <- [InterfaceFiles.interfaceExt, ".c", ".h"] ] ++
                          [ rootStubPath paths test mn | test <- [False, True] ] ++
                          [ Acton.CodeGen.supportFile (outBase paths mn) ]

-- | Compute the module path without extension under the project's src dir.
-- Used to derive the .act path or related per-module files.
srcBase                 :: Paths -> A.ModName -> FilePath
srcBase paths mn        = joinPath (srcDir paths : A.modPath (dropProjPrefix paths mn))


-- | Whether 'path' holds Build.act and a src/ directory. Always False when
-- ACTON_RUNACTON is set, so that bin/runacton builds every file as a
-- scratch build.
isActonProjectRoot :: FilePath -> IO Bool
isActonProjectRoot path = do
    runacton <- lookupEnv "ACTON_RUNACTON"
    if isJust runacton
      then return False
      else do
        hasBuildAct <- doesFileExist (path </> "Build.act")
        hasSrcDir <- doesDirectoryExist (projectSrcDir path)
        return (hasBuildAct && hasSrcDir)

-- | The nearest directory at or above 'path' that is an Acton project root,
-- Nothing when there is none up to the filesystem root.
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

-- | Run 'action' while holding the long-running compiler lock of a project
-- root and release the lock afterwards. Run 'busy' instead when another
-- process holds the lock.
withBackgroundCompilerLock :: FilePath -> IO a -> IO a -> IO a
withBackgroundCompilerLock projDir busy action = do
    mlock <- tryBackgroundCompilerLock projDir
    case mlock of
      Nothing   -> busy
      Just lock -> action `finally` releaseBackgroundCompilerLock lock

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


-- | The paths of the project of a source file, and the file's module: its
-- path under src/ without the extension, prefixed with the project name.  A
-- file at the project root names a module at the top of src/, and a file
-- under out/types the module at the same place under src/.  A file anywhere
-- else in a project is an error.  A file outside any project builds as a
-- scratch build, and its module has no prefix.  The dependencies' types
-- directories are those collectDepTypePaths reads from the Build.act files,
-- which leaves out dependencies fetched by hash that are not fetched yet.
-- Creates the module's output directories.
findPaths               :: FilePath -> C.CompileOptions -> IO (Paths, A.ModName)
findPaths actFile opts  = do execDir <- takeDirectory <$> getExecutablePath
                             sysPath <- canonicalizePath (if null $ C.syspath opts then execDir ++ "/.." else C.syspath opts)
                             absSrcFile <- canonicalizePath actFile
                             (isTmp, projPath, dirInSrc) <- analyze (takeDirectory absSrcFile) []
                             let srcDir  = if isTmp then takeDirectory absSrcFile else projectSrcDir projPath
                                 projOut = projectOutDir projPath
                             depOverrides <- normalizeDepOverrides projPath (C.dep_overrides opts)
                             (projName,depTypePaths) <- if isTmp then return ("",[]) else collectDepTypePaths projPath depOverrides
                             let paths = Paths { searchPath = interfaceSearchPath opts sysPath (projectTypesDir projPath) depTypePaths
                                               , sysPath    = sysPath
                                               , sysTypes   = sysTypesDir sysPath
                                               , projPath   = projPath
                                               , projOut    = projOut
                                               , projTypes  = projectTypesDir projPath
                                               , binDir     = if isTmp then srcDir else joinPath [projOut, "bin"]
                                               , srcDir     = srcDir
                                               , isTmp      = isTmp
                                               , projName   = projName
                                               }
                                 mn    = A.modName $ (if isTmp then dirInSrc else projName:dirInSrc) ++ [takeBaseName actFile]
                             createModuleDirectories paths mn
                             return (paths, mn)
  where analyze "/" ds  = do tmp <- canonicalizePath (C.tempdir opts)
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

-- | The module of the source file 'actFile' in the source directory
-- 'srcRoot' of the project named 'proj': the file's path under 'srcRoot'
-- without the extension, as 'projectModName' names it.
moduleNameFromFile :: FilePath -> String -> FilePath -> IO A.ModName
moduleNameFromFile srcRoot proj actFile = do
    base <- normalizePathSafe srcRoot
    file <- normalizePathSafe actFile
    let rel = dropExtension (makeRelative base file)
        names = splitDirectories rel
    return (projectModName proj names)

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

-- | Remove the generated outputs under a project's types directory that
-- belong to no module the project declares, so that a cached header cannot
-- resurrect a deleted module.
pruneMissingModuleOutputs :: ProjCtx -> Paths -> IO ()
pruneMissingModuleOutputs ctx paths = do
    let typesRoot = projTypesDir ctx
    typesExists <- doesDirectoryExist typesRoot
    when typesExists $ do
      mods <- enumerateProjectModules ctx
      let keep = Data.Set.fromList (concatMap (moduleOutputs paths . snd) mods)
      files <- getFilesRecursive typesRoot
      tyDbs <- InterfaceFiles.listInterfaceDirsRecursive typesRoot
      let outputs = filter ((`elem` [".c", ".h", ".inc"]) . takeExtension) files ++ tyDbs
      mapM_ removePathForcibly (filter (`Data.Set.notMember` keep) outputs)

-- | Remove the outputs of every changed .act path that no longer exists,
-- from each project whose source directory holds it.
pruneMissingChangedModuleOutputs :: [(ProjCtx, Paths)] -> [FilePath] -> IO ()
pruneMissingChangedModuleOutputs projects changedPaths = do
    absChanged <- mapM normalizePathSafe changedPaths
    forM_ (filter ((== ".act") . takeExtension) absChanged) $ \actPath -> do
      exists <- doesFileExist actPath
      unless exists $ forM_ projects $ \(ctx, paths) -> do
        srcRoot <- normalizePathSafe (projSrcDir ctx)
        when (actPath `isUnder` srcRoot) $ do
          mn <- moduleNameFromFile srcRoot (BuildSpec.specName (projBuildSpec ctx)) actPath
          mapM_ removePathForcibly (moduleOutputs paths mn)

-- | The interface search path of a project with the system directory
-- 'sys' and the types directory 'types', given its dependencies' types
-- directories in search order: the project's own interfaces, its
-- dependencies', those in the --searchpath directories, then the system's.
interfaceSearchPath :: C.CompileOptions -> FilePath -> FilePath -> [FilePath] -> [FilePath]
interfaceSearchPath opts sys types deps =
    [types] ++ deps ++ C.searchpath opts ++ systemTypePaths sys (sysTypesDir sys)

-- | The paths of a project of a multi-project build.  The dependencies'
-- types directories are those depTypePathsFromMap finds in the discovered
-- projects.
projectPaths :: C.CompileOptions -> M.Map FilePath ProjCtx -> ProjCtx -> Paths
projectPaths opts projMap ctx =
    Paths { searchPath = interfaceSearchPath opts (projSysPath ctx) (projTypesDir ctx) (depTypePathsFromMap projMap (projRoot ctx))
          , sysPath    = projSysPath ctx
          , sysTypes   = projSysTypes ctx
          , projPath   = projRoot ctx
          , projOut    = projOutDir ctx
          , projTypes  = projTypesDir ctx
          , binDir     = joinPath [projOutDir ctx, "bin"]
          , srcDir     = projSrcDir ctx
          , isTmp      = False
          , projName   = BuildSpec.specName (projBuildSpec ctx)
          }

-- | Create a project's binary and types directories, and register its
-- system type directories with the interface reader.
createProjectDirectories :: Paths -> IO ()
createProjectDirectories paths = do
    InterfaceFiles.registerSystemTypeRoots (systemTypePaths (sysPath paths) (sysTypes paths))
    createDirectoryIfMissing True (binDir paths)
    createDirectoryIfMissing True (projTypes paths)

-- | Create the directories of a module's outputs: those of its project and
-- the module's directory under the types directory.
createModuleDirectories :: Paths -> A.ModName -> IO ()
createModuleDirectories paths mn = do
    createProjectDirectories paths
    createDirectoryIfMissing True (moduleTypesDir (projTypes paths) mn)


-- | Load a BuildSpec from Build.act.
-- Throws ProjectError on parse or validation failure.
loadBuildSpec :: FilePath -> IO BuildSpec.BuildSpec
loadBuildSpec dir = do
    let actPath = joinPath [dir, "Build.act"]
    actExists <- doesFileExist actPath
    if actExists
      then do
        content <- readFileUtf8 actPath
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

-- | Load the Build.act of the project at 'dir' and apply the --dep
-- overrides to its dependencies.
loadOverriddenSpec :: [(String, FilePath)] -> FilePath -> IO BuildSpec.BuildSpec
loadOverriddenSpec overrides dir = loadBuildSpec dir >>= applyDepOverrides dir overrides

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

-- | Whether a path lies inside a directory, the directory itself excluded.
-- Both sides are normalised and the directory gets a trailing separator, so
-- a sibling whose name merely starts with the directory's name is outside.
isUnder                                 :: FilePath -> FilePath -> Bool
path `isUnder` dir                      = addTrailingPathSeparator (normalise dir) `isPrefixOf` normalise path

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

-- | The local path of an Acton dependency. An empty path is no path.
localPath :: BuildSpec.PkgDep -> Maybe FilePath
localPath dep = mfilter (not . null) (BuildSpec.path dep)

-- | The local path of a Zig dependency. An empty path is no path.
localZigPath :: BuildSpec.ZigDep -> Maybe FilePath
localZigPath dep = mfilter (not . null) (BuildSpec.zpath dep)

-- | Keep a concrete source path relative to the project that declares it.
rebaseDepPath :: FilePath -> BuildSpec.PkgDep -> BuildSpec.PkgDep
rebaseDepPath base dep =
    case localPath dep of
      Just p  -> dep { BuildSpec.path = Just (rebasePath base p) }
      Nothing -> dep

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
        Just dep | isJust (BuildSpec.follows dep) -> return depsMap
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
      dependencyPathError depName "does not exist" depPath
    isProjectRoot <- isActonProjectRoot depPath
    unless isProjectRoot $
      dependencyPathError depName "is not an Acton project root" depPath

-- | Fail because the local path 'path' of the dependency 'name' has the
-- problem 'problem', with a hint on what a local dependency path must be.
dependencyPathError :: String -> String -> FilePath -> IO a
dependencyPathError name problem path =
    throwProjectError ("Dependency " ++ name ++ " path " ++ problem ++ ": " ++ path ++ "\n"
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
-- Prefers an explicit project path, otherwise selects subdir in the cached archive.
resolveDepBase :: FilePath -> String -> BuildSpec.PkgDep -> IO FilePath
resolveDepBase base name dep =
    case localPath dep of
      Just p  -> normalizePathSafe (rebasePath base p)
      Nothing -> case BuildSpec.hash dep of
             Just h -> do
               home <- getHomeDirectory
               root <- normalizePathSafe (depCacheEntry (depCacheDir home) name h)
               case BuildSpec.subdir dep of
                 Nothing -> return root
                 Just dir -> do
                   selected <- normalizePathSafe (root </> dir)
                   unless (selected == root || selected `isUnder` root) $
                     throwProjectError ("Dependency " ++ name ++ " subdir " ++ show dir ++ " escapes the archive")
                   cached <- doesDirectoryExist root
                   when cached $ do
                     isProject <- isActonProjectRoot selected
                     unless isProject $
                       throwProjectError ("Dependency " ++ name ++ " subdir " ++ show dir
                                          ++ " is not an Acton project root: " ++ selected
                                          ++ "\nExpected Build.act and src/ in the selected directory.")
                   return selected
             Nothing -> throwProjectError ("Dependency " ++ name ++ " has no path or hash")

-- | The directory under the home directory 'home' that holds the Acton
-- packages fetched by hash, each in its own 'depCacheEntry'.
depCacheDir :: FilePath -> FilePath
depCacheDir home = joinPath [home, ".cache", "acton", "deps"]

-- | The directory in the dependency cache 'cache' that holds the package
-- 'name' fetched with hash 'hash'.
depCacheEntry :: FilePath -> String -> String -> FilePath
depCacheEntry cache name hash = joinPath [cache, name ++ "-" ++ hash]

-- | The Zig global cache under the home directory 'home', which holds the
-- fetched package archives of Acton and Zig dependencies.
zigGlobalCacheDir :: FilePath -> FilePath
zigGlobalCacheDir home = joinPath [home, ".cache", "acton", "zig-global-cache"]

-- | Recursively collect out/types paths for all declared dependencies.
-- Reads Build.act and follows dependency edges.
collectDepTypePaths :: FilePath -> [(String, FilePath)] -> IO (String,[FilePath])
collectDepTypePaths projDir overrides = do
  root <- normalizePathSafe projDir
  spec <- loadOverriddenSpec overrides root
  (_, _, paths) <- go Data.Set.empty M.empty root spec
  return (BuildSpec.specName spec, paths)
  where
    go seen fpMap dir spec =
      foldM (step dir) (seen, registerFingerprint dir spec fpMap, []) (M.toList (BuildSpec.concreteDependencies spec))

    step base (seen, fpMap, acc) (depName, dep) = do
      depBase <- resolveDepBase base depName dep
      depAbs  <- normalizePathSafe depBase
      depExists <- doesDirectoryExist depAbs
      if not depExists
        then case localPath dep of
               Just _  -> dependencyPathError depName "does not exist" depAbs
               Nothing -> return (seen, fpMap, acc)
        else do
          (depPath, fpMap', spec) <- canonicalDep overrides fpMap depAbs
          let typesDir = projectTypesDir depPath
          if Data.Set.member depPath seen
            then return (seen, fpMap', acc)
            else do
              let seen' = Data.Set.insert depPath seen
              depSpec <- if depPath == depAbs then return spec else loadOverriddenSpec overrides depPath
              (seenNext, fpMapNext, sub) <- go seen' fpMap' depPath depSpec
              return (seenNext, fpMapNext, acc ++ [typesDir] ++ sub)


-- | Convert a module name to its source filename (path + .act).
-- This is used for diagnostics and display output.
modNameToFilename :: A.ModName -> String
modNameToFilename mn = joinPath (A.modPath mn) ++ ".act"

-- | The module of the path 'ns' in the project named 'proj': the path with
-- the project's name in front, except in a special project.
projectModName              :: String -> [String] -> A.ModName
projectModName proj ns
  | isSpecialProject proj   = A.modName ns
  | otherwise               = A.modName (proj : ns)

-- | The name 'mn' with its project's name in front, except in a special
-- project.
addProjPrefix               :: Paths -> A.ModName -> A.ModName
addProjPrefix paths mn      = projectModName (projName paths) (A.modPath mn)

-- | The name 'mn' without its project's name in front, when it starts with
-- that name.
dropProjPrefix              :: Paths -> A.ModName -> A.ModName
dropProjPrefix paths mn     = case A.modPath mn of
                                n:ns | n == projName paths -> A.modName ns
                                ns                         -> A.modName ns

dropProjPrefixOrLib         :: Paths -> A.ModName -> A.ModName
dropProjPrefixOrLib paths mn
  | n == projName paths     = A.modName ns
  | ns == ["lib"]           = A.modName [n]
  | otherwise               = A.modName (n:ns)
  where n:ns                = A.modPath mn
