-- | Download the dependencies of a project into the user's cache: package
-- archives go to the Zig global cache under ~/.cache/acton, and Acton
-- packages are copied from there into ~/.cache/acton/deps, where project
-- discovery finds them.
module Acton.Fetch
  ( fetchDependencies
  ) where

import qualified Acton.Artifact as Artifact
import qualified Acton.BuildSpec as BuildSpec
import qualified Acton.CommandLineParser as C
import Acton.ArchiveDownload (downloadArchive)
import Acton.HttpFetch (isHttpUrl, proxyEnvReportLines, readProxyEnv)
import Acton.Project ( Paths(..), RootPins, depCacheDir, depCacheEntry, existingDependencyDir, loadOverriddenSpec, localPath
                     , localZigPath, normalizePathSafe, rootPins, selectDepAndWarn, throwProjectError, zigExecutable
                     , zigGlobalCacheDir, isActonArtifactOnlyRoot )
import Acton.ZigFetch (fetchZigPackage)
import qualified Acton.Zon as Zon

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (SomeException, displayException, IOException, bracketOnError, catch, throwIO, try)
import Control.Monad
import Data.Either (lefts, partitionEithers)
import Data.List (isPrefixOf, isSuffixOf)
import Data.IORef
import Data.Maybe (catMaybes, isJust)
import qualified Data.Map as M
import qualified Data.Set
import System.Directory
import System.Exit (ExitCode(..))
import System.FilePath.Posix
import System.IO.Temp (createTempDirectory)
import System.Random (randomRIO)
import System.Process (CreateProcess(cwd), readCreateProcessWithExitCode, proc)


-- | Fetch every dependency reachable from the project of 'paths', with the
-- --dep overrides 'depOverrides', absolute and normalised as
-- normalizeDepOverrides gives them, applied to each Build.act. A scratch
-- project has no dependencies to fetch.
fetchDependencies :: C.GlobalOptions -> Paths -> [(String, FilePath)] -> [String] -> IO ()
fetchDependencies gopts paths depOverrides artifactRepos =
    unless (isTmp paths) $ do
      rootSpec <- loadOverriddenSpec depOverrides (projPath paths)
      unless (C.quiet gopts) $
        putStrLn "Resolving dependencies (fetching if missing)..."
      when (C.verbose gopts) $ do
        proxies <- readProxyEnv
        putStr (unlines (proxyEnvReportLines "" proxies))
      home <- getHomeDirectory
      walkedSet <- newIORef Data.Set.empty
      seededSet <- newIORef Data.Set.empty
      let env = FetchEnv { options     = gopts
                         , overrides   = depOverrides
                         , artifactRepos = artifactRepos
                         , pins        = rootPins (projPath paths) rootSpec
                         , zigExe      = zigExecutable (sysPath paths)
                         , globalCache = zigGlobalCacheDir home
                         , depsCache   = depCacheDir home
                         , walked      = walkedSet
                         , seeded      = seededSet
                         }
      createDirectoryIfMissing True (globalCache env)
      createDirectoryIfMissing True (depsCache env)
      walkProject env (projPath paths) rootSpec

-- | The settings and shared state of one 'fetchDependencies' run.
data FetchEnv = FetchEnv
  { options     :: C.GlobalOptions
  , overrides   :: [(String, FilePath)]           -- ^ the --dep overrides, applied to every Build.act
  , artifactRepos :: [String]
  , pins        :: RootPins
  , zigExe      :: FilePath
  , globalCache :: FilePath                       -- ^ the Zig global cache, which holds every fetched package
  , depsCache   :: FilePath                       -- ^ the cache of Acton packages that project discovery reads
  , walked      :: IORef (Data.Set.Set FilePath)  -- ^ the projects the walk reached, so it ends on cycles and
                                                  --   shared dependencies
  , seeded      :: IORef (Data.Set.Set Seed)      -- ^ the Zig package directories and hashes already seeded, so
                                                  --   the transitive seeding walk over build.zig.zon files ends
                                                  --   on cycles and shared dependencies
  }

-- | A Zig package directory or hash that seeding claims once.
data Seed = SeedDirectory FilePath | SeedHash String deriving (Eq, Ord)

-- | Fetch the dependencies of the project at 'dir' with spec 'spec', then
-- walk its Acton dependencies, unless the walk reached the project
-- already. Within one project the order is: fetch every direct Acton and
-- Zig package concurrently and fail with all errors; copy the Acton
-- packages into the dependency cache one at a time; seed the transitive
-- Zig packages of the local Zig dependencies; walk the selected Acton
-- dependencies in name order. The copy comes before the walk, because
-- resolving a dependency's subdirectory checks it only when the
-- dependency's cache entry exists. The walk applies root pins and --dep
-- overrides but does not deduplicate by fingerprint, so every version of a
-- package is fetched.
walkProject :: FetchEnv -> FilePath -> BuildSpec.BuildSpec -> IO ()
walkProject env dir spec = do
    -- Every directory the walk visits is absolute and normalised: the root
    -- as findPaths gives it, and every dependency by resolveDepBase.
    fresh <- claim (walked env) dir
    when fresh $ do
        selectedDeps <- forM (M.toList (BuildSpec.concreteDependencies spec)) $ \(name, dep) ->
          (,) name <$> selectDepAndWarn (options env) (pins env) dir name dep
        let zigDeps = M.toList (BuildSpec.zig_dependencies spec)
            fetches = catMaybes $
                 [ actonDependencyFetch env name dep
                 | (name, dep) <- selectedDeps ]
              ++ [ dependencyFetch env ZigPackage   name (localZigPath dep) (BuildSpec.zurl dep) (BuildSpec.zhash dep)
                 | (name, dep) <- zigDeps ]
        errs <- lefts <$> mapConcurrently id fetches
        unless (null errs) $ throwProjectError (unlines errs)
        forM_ selectedDeps (uncurry (copyPackage env))
        -- TODO: remove this transitive pre-seeding (this forM_ and
        -- seedTransitiveZigDeps) once zig can fetch through an HTTP CONNECT
        -- proxy. It exists only to work around zig writing an absolute-form
        -- request URI into the tunnel, which the origin rejects; when zig is
        -- fixed upstream / in our bundled toolchain, drop this and let zig
        -- resolve transitive deps itself.
        --
        -- A zig dependency given as a local path may pull in transitive .url
        -- dependencies via its build.zig.zon; seed those into zig's cache now
        -- (through the proxy-aware HTTP client) so the build never lets zig
        -- fetch them. A url+hash zig dependency is a self-contained archive,
        -- already fetched above with the direct dependencies (see
        -- seedTransitiveZigDeps for why url packages are not recursed into).
        forM_ zigDeps $ \(_, dep) ->
          forM_ (localZigPath dep) $ \p -> seedTransitiveZigDeps env (dir </> p)
        forM_ selectedDeps (walkDependency env dir)

-- | Walk the Acton dependency 'name' of the project at 'base' in its
-- 'existingDependencyDir'. copyPackage has installed the cache entry of
-- every dependency fetched by hash, so its directory exists. A missing
-- local directory is an error.
walkDependency :: FetchEnv -> FilePath -> (String, BuildSpec.PkgDep) -> IO ()
walkDependency env base (name, dep) =
    existingDependencyDir base name dep
      >>= mapM_ (\dir -> loadOverriddenSpec (overrides env) dir >>= walkProject env dir)

-- | The two kinds of dependency a project fetches by url and hash. Both are
-- fetched into the Zig global cache.
data PackageKind = ActonPackage | ZigPackage

-- | The kind's name in fetch messages.
kindName :: PackageKind -> String
kindName ActonPackage = "pkg"
kindName ZigPackage   = "zig"

-- | The error for the dependency 'name' that has a url but no hash.
missingHash :: PackageKind -> String -> String
missingHash ActonPackage name = "Dependency " ++ name ++ " is missing hash"
missingHash ZigPackage   name = "Zig dependency " ++ name ++ " is missing hash"

-- | Whether the Zig global cache holds the package of hash 'h'. A Zig
-- package counts as cached only as an archive, the form zig's build reads.
-- An Acton package also counts as cached when it is extracted, because it
-- is copied into the dependency cache in either form.
isCached :: FetchEnv -> PackageKind -> String -> IO Bool
isCached env ZigPackage   h = doesFileExist (cachedArchive env h)
isCached env ActonPackage h = do
    archived <- doesFileExist (cachedArchive env h)
    if archived then return True else doesDirectoryExist (cachedPackage env h)

-- | Where to fetch the package of hash 'h' from when the cache lacks it.
-- Zig 0.16 left Zig packages extracted in the global cache, and zig fetch
-- repacks such a directory into an archive, so the cache stays usable
-- offline. An extracted Acton package counts as cached, so an Acton package
-- is always fetched from its 'url'.
fetchSource :: FetchEnv -> PackageKind -> String -> String -> IO String
fetchSource _   ActonPackage url _ = return url
fetchSource env ZigPackage   url h = do
    extracted <- doesDirectoryExist (cachedPackage env h)
    return (if extracted then cachedPackage env h else url)

-- | The fetch of the dependency 'name' of kind 'kind', given its local
-- path, url and hash. A local dependency and a dependency without a url
-- need no fetch. A url without a hash is an error.
dependencyFetch :: FetchEnv -> PackageKind -> String -> Maybe FilePath -> Maybe String -> Maybe String
                -> Maybe (IO (Either String ()))
dependencyFetch env kind name local url hash
  | isJust local                  = Nothing
  | Just u <- url, Just h <- hash = Just (fetchPackage env kind name u h)
  | isJust url                    = Just (return (Left (missingHash kind name)))
  | otherwise                     = Nothing

-- | Fetch the package 'name' of hash 'h' from 'url', unless the cache holds
-- it already.
fetchPackage :: FetchEnv -> PackageKind -> String -> String -> String -> IO (Either String ())
fetchPackage env kind name url h = do
    cached <- isCached env kind h
    if cached
      then do
        report env ("Using cached " ++ kindName kind ++ " dependency " ++ name ++ " (" ++ h ++ ")")
        return (Right ())
      else fetchMissing env kind name url h

-- | Fetch the package 'name' of hash 'h', which the cache lacks, from its
-- 'fetchSource'. The fetch fails when zig computes another hash, or when
-- the cache still lacks the package afterwards.
fetchMissing :: FetchEnv -> PackageKind -> String -> String -> String -> IO (Either String ())
fetchMissing env kind name url h = do
    source <- fetchSource env kind url h
    report env ("Fetching " ++ kindName kind ++ " dependency " ++ name ++ " from " ++ source)
    fetched <- zigFetch env source
    case fetched of
      Left err -> return (Left ("Failed to fetch dependency " ++ name ++ ":\n" ++ err))
      Right got
        | got /= h  -> return (Left ("Hash mismatch for dependency " ++ name ++ " (expected " ++ h ++ ", got " ++ got ++ ")"))
        | otherwise -> do
            cached <- isCached env kind h
            return (if cached then Right () else Left (missingAfterFetch env name h))

-- | Fetch 'source' into the Zig global cache with zig fetch, giving the
-- package hash. An http(s) source is downloaded with the proxy-aware HTTP
-- client, zig fetch gets the downloaded archive, and the archive is removed
-- afterwards. An http(s) URL never goes to zig fetch: zig's network stack
-- cannot traverse an HTTP CONNECT proxy, so it would hang and bury the
-- actionable proxy diagnostic. Any other source, such as a file:// URL or a
-- directory, goes to zig fetch as it is.
zigFetch :: FetchEnv -> String -> IO (Either String String)
zigFetch env source
  | isHttpUrl source = do
      downloaded <- downloadArchive source
      case downloaded of
        Left err      -> return (Left err)
        Right archive -> do
          fetched <- fetchZigPackage (zigExe env) (globalCache env) archive
          _ <- try (removeFile archive) :: IO (Either IOException ())
          return fetched
  | otherwise = fetchZigPackage (zigExe env) (globalCache env) source

-- | Copy the Acton package 'name', fetched by hash, from the Zig global
-- cache into its entry in the dependency cache, unless that entry exists.
-- The global cache holds the package extracted or as an archive. The entry
-- is created complete or not at all, so an existing entry holds the whole
-- package. A local dependency and a dependency without a hash are not
-- copied.
copyPackage :: FetchEnv -> String -> BuildSpec.PkgDep -> IO ()
copyPackage env name dep =
    case (localPath dep, BuildSpec.hash dep) of
      (Nothing, Just h) -> do
        let dst = depCacheEntry (depsCache env) name h
        exists <- doesDirectoryExist dst
        unless exists $ do
          extracted <- doesDirectoryExist (cachedPackage env h)
          archived <- doesFileExist (cachedArchive env h)
          unless (extracted || archived) $
            throwProjectError (missingAfterFetch env name h)
          when (C.verbose (options env)) $
            putStrLn ("Copying dependency " ++ name ++ " (" ++ h ++ ") from Zig cache")
          installDirectory dst $
            if extracted
              then copyTree (cachedPackage env h)
              else extractArchive (cachedArchive env h)
      _ -> return ()

-- | Pre-seed zig's package cache for the transitive dependencies reachable
-- from the build.zig.zon of the zig package in a directory. Path deps are
-- followed in place, recursing into their own build.zig.zon; url+hash deps
-- are downloaded through the proxy-aware HTTP client so zig finds them
-- cached. Lazy deps are left for zig to fetch on demand: eagerly seeding
-- them would download archives a normal build never uses.
--
-- A url dependency is seeded but not recursed into: the package archives
-- that occur in practice (lmdb, libssh, zlib upstream) are leaf C sources
-- with no nested build.zig.zon, and reading a manifest back out of every
-- seeded tarball would cost a per-build extraction on the common path.
--
-- Seeding is best-effort. When a seed fails, zig fetches the package during
-- the build: a direct build succeeds that way, and behind a proxy zig fails
-- with its own diagnostic. A failed seed is a warning, never an error.
seedTransitiveZigDeps :: FetchEnv -> FilePath -> IO ()
seedTransitiveZigDeps env = seedDirectory
  where
    seedDirectory dir = do
      dirAbs <- normalizePathSafe dir
      fresh <- claim (seeded env) (SeedDirectory dirAbs)
      when fresh $ do
        let zonPath = dirAbs </> "build.zig.zon"
        parsed <- Zon.readZonDependencies zonPath
        case parsed of
          Left err ->
            when (C.verbose (options env)) $
              putStrLn ("Note: could not parse " ++ zonPath
                        ++ " to pre-seed transitive zig dependencies: " ++ err)
          Right deps -> forM_ deps (seedDependency dirAbs)

    seedDependency pkgDir (name, dep)
      | Zon.zdLazy dep                         = return ()
      | Just p <- Zon.zdPath dep, not (null p) = seedDirectory (pkgDir </> p)
      | Just u <- Zon.zdUrl dep
      , Just h <- Zon.zdHash dep               = do
          fresh <- claim (seeded env) (SeedHash h)
          when fresh $ do
            cached <- isCached env ZigPackage h
            unless cached $ do
              fetched <- fetchMissing env ZigPackage name u h
              case fetched of
                Left e  -> report env ("Warning: could not pre-fetch transitive zig dependency " ++ name ++ ": " ++ e)
                Right _ -> return ()
      | otherwise                              = return ()

-- | Record 'key' in the set 'ref'. True the first time 'key' is recorded.
claim :: Ord a => IORef (Data.Set.Set a) -> a -> IO Bool
claim ref key = atomicModifyIORef' ref $ \s ->
    if Data.Set.member key s then (s, False) else (Data.Set.insert key s, True)

-- | The extracted package of hash 'h' in the Zig global cache.
cachedPackage :: FetchEnv -> String -> FilePath
cachedPackage env h = joinPath [globalCache env, "p", h]

-- | The package archive of hash 'h' in the Zig global cache.
cachedArchive :: FetchEnv -> String -> FilePath
cachedArchive env h = cachedPackage env h ++ ".tar.gz"

-- | The error for the dependency 'name' of hash 'h' that the Zig global
-- cache lacks after its fetch.
missingAfterFetch :: FetchEnv -> String -> String -> String
missingAfterFetch env name h = "Dependency " ++ name ++ " not present in Zig cache after fetch: " ++ cachedPackage env h

-- | Print a line unless the options ask for quiet output.
report :: FetchEnv -> String -> IO ()
report env line = unless (C.quiet (options env)) $ putStrLn line

-- | Create the new directory 'dst' complete or not at all. 'fill' fills a
-- temporary directory next to 'dst', which is renamed to 'dst' once 'fill'
-- returns. When 'fill' throws, the temporary directory is removed and 'dst'
-- is not created. When the rename fails and 'dst' exists, another run
-- installed 'dst' meanwhile: that 'dst' counts as installed and the
-- temporary directory is removed.
installDirectory :: FilePath -> (FilePath -> IO ()) -> IO ()
installDirectory dst fill =
    bracketOnError (createTempDirectory (takeDirectory dst) ".acton-dep-extract")
                   removeDirectoryRecursive $ \tmp -> do
      fill tmp
      renameDirectory tmp dst `catch` \e -> do
        installed <- doesDirectoryExist dst
        unless installed $ throwIO (e :: IOException)
        removeDirectoryRecursive tmp

-- | Extract the package archive 'archive' into the directory 'dir',
-- dropping the archive's top directory.
extractArchive :: FilePath -> FilePath -> IO ()
extractArchive archive dir = do
    let cmd = proc "tar" ["-xzf", archive, "-C", dir, "--strip-components", "1"]
    (code, out, err) <- readCreateProcessWithExitCode cmd ""
    case code of
      ExitSuccess   -> return ()
      ExitFailure _ -> throwProjectError ("Failed to extract cached dependency archive " ++ archive ++ ":\n" ++ out ++ err)

-- | Copy the entries of the directory 'src' into the existing directory
-- 'dir'.
copyTree :: FilePath -> FilePath -> IO ()
copyTree src dir = do
    entries <- listDirectory src
    forM_ entries $ \e -> do
      let s = src </> e
          d = dir </> e
      isDir <- doesDirectoryExist s
      if isDir then createDirectory d >> copyTree s d
               else copyFile s d


-- | Resolve an Acton dependency through an optional output artifact before
-- fetching its source. Local dependencies are always compiled from source.
actonDependencyFetch :: FetchEnv -> String -> BuildSpec.PkgDep -> Maybe (IO (Either String ()))
actonDependencyFetch env name dep
    | isJust (localPath dep) = Nothing
    | Just url <- BuildSpec.url dep, Just h <- BuildSpec.hash dep =
        Just (fetchActonPackage env name dep url h)
    | isJust (BuildSpec.url dep) = Just (return (Left (missingHash ActonPackage name)))
    | otherwise = Nothing

fetchActonPackage :: FetchEnv -> String -> BuildSpec.PkgDep -> String -> String -> IO (Either String ())
fetchActonPackage env name dep url h = do
    let dst = depCacheEntry (depsCache env) name h
    present <- doesDirectoryExist dst
    if present
      then do
        cachedArtifact <- validateCachedArtifactDir env name h dst
        case cachedArtifact of
          Left err -> return (Left err)
          Right True -> useCachedArtifact env name h
          Right False -> do
            stillPresent <- doesDirectoryExist dst
            if stillPresent
              then usePkgOrPreferredArtifact env name dep h dst
              else fetchActonPackageFresh env name dep url h dst
      else fetchActonPackageFresh env name dep url h dst

-- | Explicit artifact repositories prefer artifacts to cached source, so a
-- producer can consume the artifact it published in the same environment.
usePkgOrPreferredArtifact :: FetchEnv -> String -> BuildSpec.PkgDep -> String -> FilePath -> IO (Either String ())
usePkgOrPreferredArtifact env name dep h dst
    | null (artifactRepos env) = useCachedPkg env name h
    | otherwise = do
        artifact <- fetchPkgArtifact env name dep h dst
        case artifact of
          Left err -> return (Left err)
          Right True -> return (Right ())
          Right False -> useCachedPkg env name h

useCachedArtifact :: FetchEnv -> String -> String -> IO (Either String ())
useCachedArtifact env name h = do
    unless (C.quiet (options env)) $
      putStrLn ("Using cached Acton artifact for dependency " ++ name ++ " (" ++ h ++ ")")
    return (Right ())

useCachedPkg :: FetchEnv -> String -> String -> IO (Either String ())
useCachedPkg env name h = do
    unless (C.quiet (options env)) $
      putStrLn ("Using cached pkg dependency " ++ name ++ " (" ++ h ++ ")")
    return (Right ())

fetchActonPackageFresh :: FetchEnv -> String -> BuildSpec.PkgDep -> String -> String -> FilePath -> IO (Either String ())
fetchActonPackageFresh env name dep url h dst = do
    sourcePresent <- isCached env ActonPackage h
    if sourcePresent && null (artifactRepos env)
      then fetchPackage env ActonPackage name url h
      else do
        artifact <- fetchPkgArtifact env name dep h dst
        case artifact of
          Left err -> return (Left err)
          Right True -> return (Right ())
          Right False -> fetchPackage env ActonPackage name url h

validateCachedArtifactDir :: FetchEnv -> String -> String -> FilePath -> IO (Either String Bool)
validateCachedArtifactDir env name h dst = do
    artifactRoot <- isActonArtifactOnlyRoot dst
    if not artifactRoot
      then return (Right False)
      else do
        valid <- validateArtifactDir name h dst
        case valid of
          Right () -> return (Right True)
          Left err -> do
            when (C.verbose (options env)) $
              putStrLn ("Ignoring cached Acton artifact for " ++ name ++ ": " ++ err)
            rm <- try (removePathForcibly dst) :: IO (Either IOException ())
            case rm of
              Right _ -> return (Right False)
              Left ex -> return (Left ("Failed to remove stale Acton artifact " ++ dst ++ ": " ++ displayException ex))

fetchPkgArtifact :: FetchEnv -> String -> BuildSpec.PkgDep -> String -> FilePath -> IO (Either String Bool)
fetchPkgArtifact env name dep h dst = do
    refs <- artifactRefs env dep h
    case refs of
      Left err -> return (Left err)
      Right allRefs -> tryArtifactRefs env name h dst (localArtifactRefs allRefs ++ remoteArtifactRefs allRefs)

localArtifactRefs :: [String] -> [String]
localArtifactRefs refs = filter Artifact.ociRefIsLocal refs

remoteArtifactRefs :: [String] -> [String]
remoteArtifactRefs refs = filter (not . Artifact.ociRefIsLocal) refs

artifactRefs :: FetchEnv -> BuildSpec.PkgDep -> String -> IO (Either String [String])
artifactRefs env dep h =
    let (repoErrors, repoRefs) = partitionEithers [ artifactRepoRef repo h | repo <- artifactRepos env ]
    in case repoErrors of
         err:_ -> return (Left err)
         [] -> return (Right (repoRefs ++ derivedArtifactRefs dep h))
    where
      artifactRepoRef repo h =
        case Artifact.ociRefForRepository repo h of
          Just ref -> Right ref
          Nothing  -> Left ("Invalid Acton artifact repository " ++ repo)

      derivedArtifactRefs dep h =
        catMaybes [ BuildSpec.repo_url dep >>= \repoUrl -> Artifact.deriveOciRef repoUrl h ]

tryArtifactRefs :: FetchEnv -> String -> String -> FilePath -> [String] -> IO (Either String Bool)
tryArtifactRefs env name h dst refs =
    case refs of
      [] -> return (Right False)
      ref:rest -> do
        unless (C.quiet (options env)) $
          putStrLn ("Trying Acton artifact " ++ name ++ " from " ++ ref)
        tmpRoot <- getTemporaryDirectory
        nonce <- randomRIO (0, maxBound :: Int)
        let tmpDir = joinPath [tmpRoot, "acton-oci-" ++ show nonce]
            pullDir = joinPath [tmpDir, "pull"]
            stageDir = joinPath [tmpDir, "stage"]
            refArg = Artifact.ociRefOrasTarget ref
            pullArgs = ["pull"] ++ Artifact.ociRefOrasOptions ref ++ [refArg, "--output", pullDir]
        createDirectoryIfMissing True pullDir
        pullRes <- runProcessCapture "oras" pullArgs Nothing
        case pullRes of
          Left _ -> cleanupTmp tmpDir >> tryArtifactRefs env name h dst rest
          Right (ExitFailure _, _, _) -> cleanupTmp tmpDir >> tryArtifactRefs env name h dst rest
          Right (ExitSuccess, _, _) -> do
            let archive = pullDir </> Artifact.artifactArchiveFile
            archiveExists <- doesFileExist archive
            if not archiveExists
              then do
                skipArtifact env name h dst ref tmpDir rest ("did not contain " ++ Artifact.artifactArchiveFile)
              else do
                createDirectoryIfMissing True stageDir
                extracted <- extractArtifactArchive ref archive stageDir
                case extracted of
                  Left err -> skipArtifact env name h dst ref tmpDir rest err
                  Right () -> do
                    valid <- validateArtifactDir name h stageDir
                    case valid of
                      Left err -> skipArtifact env name h dst ref tmpDir rest err
                      Right () -> do
                        createDirectoryIfMissing True (depsCache env)
                        dstExists <- doesDirectoryExist dst
                        materialized <- if dstExists
                                          then try (removePathForcibly dst >> renameDirectory stageDir dst) :: IO (Either IOException ())
                                          else try (renameDirectory stageDir dst) :: IO (Either IOException ())
                        case materialized of
                          Right _ -> do
                            cleanupTmp tmpDir
                            unless (C.quiet (options env)) $
                              putStrLn ("Using Acton artifact for dependency " ++ name ++ " (" ++ h ++ ")")
                            return (Right True)
                          Left ex -> do
                            cleanupTmp tmpDir
                            return (Left ("Failed to materialize Acton artifact " ++ ref ++ ": " ++ displayException ex))

skipArtifact :: FetchEnv -> String -> String -> FilePath -> String -> FilePath -> [String] -> String -> IO (Either String Bool)
skipArtifact env name h dst ref tmpDir rest reason = do
    when (C.verbose (options env)) $
      putStrLn ("Ignoring Acton artifact " ++ ref ++ ": " ++ reason)
    cleanupTmp tmpDir
    tryArtifactRefs env name h dst rest

validateArtifactDir :: String -> String -> FilePath -> IO (Either String ())
validateArtifactDir name h dir = do
    manifestE <- Artifact.readManifest dir
    case manifestE of
      Left err -> return (Left ("Invalid Acton artifact manifest for " ++ name ++ ": " ++ err))
      Right manifest ->
        case Artifact.validateManifest h manifest of
          Left err -> return (Left ("Invalid Acton artifact manifest for " ++ name ++ ": " ++ err))
          Right () -> do
            hasBuildAct <- doesFileExist (dir </> "Build.act")
            hasTypesDir <- doesDirectoryExist (dir </> "out" </> "types")
            if hasBuildAct && hasTypesDir
              then return (Right ())
              else return (Left ("Acton artifact for " ++ name ++ " must contain Build.act and out/types"))

extractArtifactArchive :: String -> FilePath -> FilePath -> IO (Either String ())
extractArtifactArchive ref archive stageDir = do
    safe <- validateArtifactArchiveEntries archive
    case safe of
      Left err ->
        return (Left ("Unsafe Acton artifact " ++ ref ++ ": " ++ err))
      Right () -> do
        extractRes <- runProcessCapture "tar" ["-xzf", archive, "-C", stageDir] Nothing
        case extractRes of
          Left ex ->
            return (Left ("Failed to extract Acton artifact " ++ ref ++ ": " ++ displayException ex))
          Right (ExitFailure _, _, err) ->
            return (Left ("Failed to extract Acton artifact " ++ ref ++ ":\n" ++ err))
          Right (ExitSuccess, _, _) -> return (Right ())

validateArtifactArchiveEntries :: FilePath -> IO (Either String ())
validateArtifactArchiveEntries archive = do
    namesRes <- runProcessCapture "tar" ["-tzf", archive] Nothing
    case namesRes of
      Left ex -> return (Left ("failed to list archive entries: " ++ displayException ex))
      Right (ExitFailure _, _, err) -> return (Left ("failed to list archive entries:\n" ++ err))
      Right (ExitSuccess, out, _) -> do
        let entries = filter (not . null) (lines out)
            badPaths = filter (not . safeArtifactPath) entries
        if not (null badPaths)
          then return (Left ("invalid archive path " ++ head badPaths))
          else do
            typesRes <- runProcessCapture "tar" ["-tvzf", archive] Nothing
            case typesRes of
              Left ex -> return (Left ("failed to inspect archive entries: " ++ displayException ex))
              Right (ExitFailure _, _, err) -> return (Left ("failed to inspect archive entries:\n" ++ err))
              Right (ExitSuccess, typeOut, _) -> do
                let badTypes = filter (not . safeArtifactTypeLine) (filter (not . null) (lines typeOut))
                if null badTypes
                  then return (Right ())
                  else return (Left ("unsupported archive entry type " ++ take 1 (head badTypes)))

safeArtifactPath :: FilePath -> Bool
safeArtifactPath p =
    not (isAbsolute p) &&
    ".." `notElem` splitDirectories p &&
    (p == Artifact.artifactManifestFile ||
     p == "Build.act" ||
     p == "out" ||
     p == "out/types" ||
     "out/types/" `isPrefixOf` p ||
     ("src/" `isPrefixOf` p && (".ext.c" `isSuffixOf` p || hasTrailingPathSeparator p)))

safeArtifactTypeLine :: String -> Bool
safeArtifactTypeLine [] = True
safeArtifactTypeLine (c:_) = c == '-' || c == 'd'

runProcessCapture :: FilePath -> [String] -> Maybe FilePath -> IO (Either SomeException (ExitCode, String, String))
runProcessCapture exe args mcwd = do
    let cmd = (proc exe args){ cwd = mcwd }
    try (readCreateProcessWithExitCode cmd "") :: IO (Either SomeException (ExitCode, String, String))

cleanupTmp :: FilePath -> IO ()
cleanupTmp dir =
    removePathForcibly dir `catch` ignoreCleanup

ignoreCleanup :: IOException -> IO ()
ignoreCleanup _ = return ()
