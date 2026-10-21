-- | Download the dependencies of a project into the user's cache: package
-- archives go to the Zig global cache under ~/.cache/acton, and Acton
-- packages are copied from there into ~/.cache/acton/deps, where project
-- discovery finds them.
module Acton.Fetch
  ( fetchDependencies
  ) where

import qualified Acton.BuildSpec as BuildSpec
import qualified Acton.CommandLineParser as C
import Acton.ArchiveDownload (downloadArchive)
import Acton.HttpFetch (isHttpUrl, proxyEnvReportLines, readProxyEnv)
import Acton.Project (Paths(..), applyDepOverrides, depCacheDir, depCacheEntry, loadBuildSpec, normalizePathSafe, rebaseDepPath, resolveDepBase, throwProjectError, zigGlobalCacheDir)
import Acton.ZigFetch (fetchZigPackage)
import qualified Acton.Zon as Zon

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (IOException, bracketOnError, try)
import Control.Monad
import Data.IORef
import Data.Maybe (catMaybes, isJust)
import qualified Data.Map as M
import qualified Data.Set
import System.Directory
import System.Exit (ExitCode(..))
import System.FilePath.Posix
import System.IO.Temp (createTempDirectory)
import System.Process (readCreateProcessWithExitCode, proc)


fetchDependencies :: C.GlobalOptions -> Paths -> [(String, FilePath)] -> IO ()
fetchDependencies gopts paths depOverrides = do
    if isTmp paths
      then return ()
      else do
        rootSpec0 <- loadBuildSpec (projPath paths)
        rootSpec <- applyDepOverrides (projPath paths) depOverrides rootSpec0
        unless (C.quiet gopts) $
          putStrLn "Resolving dependencies (fetching if missing)..."
        when (C.verbose gopts) $ do
          env <- readProxyEnv
          putStr (unlines (proxyEnvReportLines "" env))
        home <- getHomeDirectory
        let zigExe      = joinPath [sysPath paths, "zig", "zig"]
            globalCache = zigGlobalCacheDir home
            depsCache   = depCacheDir home
            cacheDir h  = joinPath [globalCache, "p", h]
        createDirectoryIfMissing True globalCache
        createDirectoryIfMissing True depsCache
        -- Tracks zig package dirs / hashes already pre-seeded, so the transitive
        -- walk over build.zig.zon files terminates on cycles and shared deps.
        zigSeen <- newIORef Data.Set.empty
        let rootPins = M.map (rebaseDepPath (projPath paths)) (BuildSpec.concreteDependencies rootSpec)
        _ <- walkProject rootPins cacheDir zigExe globalCache depsCache zigSeen
                         Data.Set.empty (projPath paths) rootSpec
        return ()
  where
    walkProject rootPins cacheDir zigExe globalCache depsCache zigSeen seen projDir spec = do
      projAbs <- normalizePathSafe projDir
      if Data.Set.member projAbs seen
        then return seen
        else do
          selectedDeps <- forM (M.toList (BuildSpec.concreteDependencies spec)) $
            selectDependency rootPins projAbs
          let pkgFetches = catMaybes
                [ mkPkgFetch cacheDir zigExe globalCache name dep | (name, dep) <- selectedDeps ]
              zigFetches = catMaybes
                [ mkZigFetch cacheDir zigExe globalCache name dep
                | (name, dep) <- M.toList (BuildSpec.zig_dependencies spec)
                ]
          results <- mapConcurrently id (pkgFetches ++ zigFetches)
          let errs = [ e | Left e <- results ]
          unless (null errs) $ throwProjectError (unlines errs)
          forM_ selectedDeps $ \(name, dep) -> copyPkgDep cacheDir depsCache name dep
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
          -- already seeded above by mkZigFetch (see seedTransitiveZigDeps for why
          -- url packages are not recursed into).
          forM_ (M.toList (BuildSpec.zig_dependencies spec)) $ \(_, zdep) ->
            case BuildSpec.zpath zdep of
              Just p | not (null p) ->
                seedTransitiveZigDeps zigSeen cacheDir zigExe globalCache (projAbs </> p)
              _ -> return ()
          let seen' = Data.Set.insert projAbs seen
          foldM (walkDependency rootPins cacheDir zigExe globalCache depsCache zigSeen projAbs)
                seen' selectedDeps

    selectDependency rootPins base (depName, dep) = do
      let (chosenDep, conflict) =
            case M.lookup depName rootPins of
              Nothing -> (dep, Nothing)
              Just pinDep ->
                if pinDep == rebaseDepPath base dep
                  then (pinDep, Nothing)
                  else (pinDep, Just dep)
      when (isJust conflict) $
        unless (C.quiet gopts) $
          putStrLn ("Warning: dependency '" ++ depName ++ "' in " ++ base
                    ++ " overridden by root pin")
      return (depName, chosenDep)

    walkDependency rootPins cacheDir zigExe globalCache depsCache zigSeen base seen (depName, dep) = do
      depBase <- resolveDepBase base depName dep
      depAbs <- normalizePathSafe depBase
      depExists <- doesDirectoryExist depAbs
      if not depExists
        then case BuildSpec.path dep of
               Just p | not (null p) ->
                 throwProjectError ("Dependency " ++ depName ++ " path does not exist: " ++ depAbs ++ "\n"
                                    ++ "Hint: Local dependency paths must point to an Acton project root\n"
                                    ++ "(directory with src/ and Build.act).")
               _ -> return seen
        else do
          depSpec0 <- loadBuildSpec depAbs
          depSpec <- applyDepOverrides depAbs depOverrides depSpec0
          walkProject rootPins cacheDir zigExe globalCache depsCache zigSeen seen depAbs depSpec

    mkPkgFetch cacheDir zigExe globalCache name dep =
      case BuildSpec.path dep of
        Just p | not (null p) -> Nothing
        _ -> case (BuildSpec.url dep, BuildSpec.hash dep) of
               (Just u, Just h) ->
                 Just (fetchOne "pkg" name u (Just h) cacheDir zigExe globalCache)
               (Just _, Nothing) ->
                 Just (return (Left ("Dependency " ++ name ++ " is missing hash")))
               _ -> Nothing

    mkZigFetch cacheDir zigExe globalCache name dep =
      case BuildSpec.zpath dep of
        Just p | not (null p) -> Nothing
        _ -> case (BuildSpec.zurl dep, BuildSpec.zhash dep) of
               (Just u, Just h) ->
                 Just (fetchOne "zig" name u (Just h) cacheDir zigExe globalCache)
               (Just _, Nothing) ->
                 Just (return (Left ("Zig dependency " ++ name ++ " is missing hash")))
               _ -> Nothing

    copyPkgDep cacheDir depsCache name dep =
      case BuildSpec.path dep of
        Just p | not (null p) -> return ()
        _ -> case BuildSpec.hash dep of
               Nothing -> return ()
               Just h -> do
                 let src = cacheDir h
                     srcArchive = cacheArchivePath cacheDir h
                     dst = depCacheEntry depsCache name h
                 exists <- doesDirectoryExist dst
                 unless exists $ do
                   srcOk <- doesDirectoryExist src
                   archiveOk <- doesFileExist srcArchive
                   unless (srcOk || archiveOk) $
                     throwProjectError ("Dependency " ++ name ++ " not present in Zig cache after fetch: " ++ src)
                   when (C.verbose gopts) $
                     putStrLn ("Copying dependency " ++ name ++ " (" ++ h ++ ") from Zig cache")
                   if srcOk
                     then copyTree src dst
                     else extractCachedArchive srcArchive dst

    -- Pre-seed zig's package cache for the transitive dependencies reachable from
    -- a zig package's build.zig.zon. Path deps are followed in place, recursing
    -- into their own build.zig.zon; url+hash deps are downloaded through the
    -- proxy-aware HTTP client (via fetchOne) so zig finds them cached. Lazy deps
    -- are left for zig to fetch on demand: eagerly seeding them would download
    -- archives a normal build never uses.
    --
    -- A url dependency is seeded but not recursed into: the package archives that
    -- occur in practice (lmdb, libssh, zlib upstream) are leaf C sources with no
    -- nested build.zig.zon, and reading a manifest back out of every seeded
    -- tarball would cost a per-build extraction on the common path.
    seedTransitiveZigDeps zigSeen cacheDir zigExe globalCache = goDir
      where
        goDir dir = do
          dirAbs <- normalizePathSafe dir
          fresh <- claimVisited zigSeen ("d:" ++ dirAbs)
          when fresh $ do
            let zonPath = dirAbs </> "build.zig.zon"
            edeps <- Zon.readZonDependencies zonPath
            case edeps of
              Left err ->
                when (C.verbose gopts) $
                  putStrLn ("Note: could not parse " ++ zonPath
                            ++ " to pre-seed transitive zig dependencies: " ++ err)
              Right deps -> forM_ deps (goDep dirAbs)

        goDep pkgDir (name, dep)
          | Zon.zdLazy dep                          = return ()
          | Just p <- Zon.zdPath dep, not (null p)  = goDir (pkgDir </> p)
          | Just u <- Zon.zdUrl dep
          , Just h <- Zon.zdHash dep                = do
              fresh <- claimVisited zigSeen ("h:" ++ h)
              when fresh $ do
                present <- cacheEntryExists "zig" cacheDir h
                unless present $ do
                  res <- fetchOne "zig" name u (Just h) cacheDir zigExe globalCache
                  case res of
                    -- Non-fatal: if pre-seeding fails, let zig try to fetch it
                    -- during the build (its pre-change behaviour). On a direct
                    -- build that just works; behind a proxy zig fails with its own
                    -- diagnostic -- either way a seeding hiccup never breaks a
                    -- build the old code would have completed.
                    Left e  -> unless (C.quiet gopts) $
                                 putStrLn ("Warning: could not pre-fetch transitive zig dependency "
                                           ++ name ++ ": " ++ e)
                    Right _ -> return ()
          | otherwise                               = return ()

    -- Atomically record a key; returns True the first time it is seen.
    claimVisited ref key = atomicModifyIORef' ref $ \s ->
      if Data.Set.member key s then (s, False) else (Data.Set.insert key s, True)

    fetchOne kind name url mh cacheDir zigExe globalCache = do
      case mh of
        Just h -> do
          present <- cacheEntryExists kind cacheDir h
          if present
            then do
              unless (C.quiet gopts) $
                putStrLn ("Using cached " ++ kind ++ " dependency " ++ name ++ " (" ++ h ++ ")")
              return (Right h)
            else do
              -- Zig 0.16 stored extracted packages in the global cache. Repack
              -- them locally so upgrading preserves offline cache reuse.
              legacy <- if kind == "zig" then doesDirectoryExist (cacheDir h) else return False
              runFetch kind name (if legacy then cacheDir h else url) mh cacheDir zigExe globalCache
        Nothing ->
          runFetch kind name url mh cacheDir zigExe globalCache

    runFetch kind name url mh cacheDir zigExe globalCache = do
      unless (C.quiet gopts) $
        putStrLn ("Fetching " ++ kind ++ " dependency " ++ name ++ " from " ++ url)
      if isHttpUrl url
        then fetchViaDownloadedArchive kind name url mh cacheDir zigExe globalCache
        else do -- other URLs, like file:// - not very common, maybe we want to constrain this somehow?
          res <- fetchZigPackage zigExe globalCache url
          case res of
            Left err -> return (Left ("Failed to fetch dependency " ++ name ++ ":\n" ++ err))
            Right hashVal -> validateFetchedHash kind name mh cacheDir hashVal

    validateFetchedHash :: String -> String -> Maybe String -> (String -> FilePath) -> String -> IO (Either String String)
    validateFetchedHash kind name mh cacheDir hashVal = do
      case mh of
        Just h | h /= hashVal ->
          return (Left ("Hash mismatch for dependency " ++ name ++ " (expected " ++ h ++ ", got " ++ hashVal ++ ")"))
        _ -> do
          exists <- cacheEntryExists kind cacheDir hashVal
          if exists
            then return (Right hashVal)
            else return (Left ("Dependency " ++ name ++ " not present in Zig cache after fetch: " ++ cacheDir hashVal))

    cacheEntryExists :: String -> (String -> FilePath) -> String -> IO Bool
    cacheEntryExists kind cacheDir hashVal = do
      archiveExists <- doesFileExist (cacheArchivePath cacheDir hashVal)
      if archiveExists || kind == "zig"
        then return archiveExists
        else doesDirectoryExist (cacheDir hashVal)

    cacheArchivePath :: (String -> FilePath) -> String -> FilePath
    cacheArchivePath cacheDir hashVal = cacheDir hashVal ++ ".tar.gz"

    extractCachedArchive :: FilePath -> FilePath -> IO ()
    extractCachedArchive archive dst = do
      bracketOnError (createTempDirectory (takeDirectory dst) ".acton-dep-extract")
                     removeDirectoryRecursive $ \tmp -> do
        let cmd = proc "tar" ["-xzf", archive, "-C", tmp, "--strip-components", "1"]
        (code, out, err) <- readCreateProcessWithExitCode cmd ""
        case code of
          ExitSuccess -> renameDirectory tmp dst
          ExitFailure _ ->
            throwProjectError ("Failed to extract cached dependency archive " ++ archive ++ ":\n" ++ out ++ err)

    fetchViaDownloadedArchive kind name depUrl mh cacheDir zigExe globalCache = do
      -- depUrl is always http(s) here (runFetch's isHttpUrl guard), so
      -- downloadArchive (proxy-aware http-client) is the right path. Do NOT fall
      -- back to `zig fetch <depUrl>`: zig's network stack cannot traverse an HTTP
      -- CONNECT proxy, so it would hang and bury the actionable proxy diagnostic.
      dl <- downloadArchive depUrl
      case dl of
        Left dlErr ->
          return (Left ("Failed to fetch dependency " ++ name ++ ":\n" ++ dlErr))
        Right localArchive -> do
          fetched <- fetchZigPackage zigExe globalCache localArchive
          _ <- try (removeFile localArchive) :: IO (Either IOException ())
          case fetched of
            Left err ->
              return (Left ("Failed to fetch dependency " ++ name ++ ":\n" ++ err))
            Right hashVal -> validateFetchedHash kind name mh cacheDir hashVal

    copyTree :: FilePath -> FilePath -> IO ()
    copyTree src dst = do
      exists <- doesDirectoryExist src
      unless exists $ throwProjectError ("Source path for copyTree does not exist: " ++ src)
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
