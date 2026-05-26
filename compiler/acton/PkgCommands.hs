{-# LANGUAGE OverloadedStrings #-}
module PkgCommands
  ( installCommand
  , uninstallCommand
  , pkgAddCommand
  , pkgRemoveCommand
  , pkgUpgradeCommand
  , pkgUpdateCommand
  , pkgSearchCommand
  , zigPkgAddCommand
  , zigPkgRemoveCommand
  , PackageEntry(..)
  , RepoInfo(..)
  , githubCloneUrl
  , isGithubCommitSha
  , parseGithubRepoUrl
  , decodePackageIndex
  , decodeAppPackageIndex
  , matchesAllTerms
  ) where

import Prelude hiding (readFile, writeFile)

import qualified Acton.BuildSpec as BuildSpec
import qualified Acton.CommandLineParser as C
import Acton.Compile (loadBuildSpec, throwProjectError)

import Control.Exception (IOException, SomeException, try, displayException, evaluate)
import Control.Concurrent (threadDelay)
import Control.Monad (filterM, forM, forM_, unless, when)
import Data.Char (isHexDigit, isSpace)
import Data.Foldable (toList)
import Data.List (dropWhileEnd, isPrefixOf, isSuffixOf, sortOn)
import Data.List.Split (splitOn)
import Data.Maybe (isJust)
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKM
import qualified Data.Text as T
import Network.HTTP.Client (Manager, Response, httpLbs, parseRequest, requestHeaders, responseBody, responseStatus)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Header (Header)
import Network.HTTP.Types.Status (statusCode)
import System.Directory (Permissions, canonicalizePath, copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, doesPathExist, getCurrentDirectory, getHomeDirectory, getPermissions, listDirectory, removeFile, setPermissions)
import System.Environment (getExecutablePath, lookupEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeDirectory)
import System.IO (IOMode(ReadMode, WriteMode), hClose, hGetContents, hPutStr, hPutStrLn, hSetEncoding, openFile, stderr, utf8)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess(cwd), proc, readCreateProcessWithExitCode)
import qualified Text.Regex.TDFA as TDFA

data PackageEntry = PackageEntry
    { pkgName        :: String
    , pkgDescription :: String
    , pkgRepoUrl     :: String
    } deriving (Eq, Show)

data PackageIndexEntry = PackageIndexEntry
    { indexName        :: String
    , indexKinds       :: [String]
    , indexDescription :: String
    , indexRepoUrl     :: String
    } deriving (Eq, Show)

data InstallManifest = InstallManifest
    { manifestName      :: String
    , manifestRepoUrl   :: String
    , manifestRepoRef   :: Maybe String
    , manifestCommit    :: String
    , manifestHash      :: String
    , manifestSourceDir :: FilePath
    , manifestBinaries  :: [String]
    } deriving (Eq, Show)

data RepoInfo = RepoInfo
    { repoOwner :: String
    , repoName  :: String
    , repoRef   :: Maybe String
    } deriving (Eq, Show)

installCommand :: C.GlobalOptions -> C.InstallOptions -> IO ()
installCommand gopts opts = do
    let appName = C.installName opts
        repoUrlArg = C.installRepoUrl opts
        repoRefArg = normalizeMaybe (C.installRepoRef opts)
        pkgNameArg = C.installPkgName opts
    validateInstallName appName
    manager <- newTlsManager
    token <- resolveGithubToken (normalizeMaybe (C.installGithubToken opts))
    repoUrl <-
      if null repoUrlArg
        then lookupRepoUrlFromIndexByKind "app" appName pkgNameArg
        else do
          ensureGithubUrl "install" repoUrlArg
          return repoUrlArg
    archiveUrl <- requireRight =<< resolveGithubArchiveUrl manager token repoUrl repoRefArg
    commitSha <- requireRight (commitShaFromArchiveUrl archiveUrl)
    zigExe <- getZigExe
    archiveHash <- requireRight =<< zigFetchHash zigExe archiveUrl
    sourceDir <- prepareInstallSource appName repoUrl commitSha
    unless (C.quiet gopts) $
      putStrLn ("Building " ++ appName ++ " with acton build --release")
    actonExe <- getExecutablePath
    runProcessChecked (Just sourceDir) actonExe ["build", "--release"]
    binaries <- discoverBuiltBinaries sourceDir
    installBuiltBinaries appName repoUrl repoRefArg commitSha archiveHash sourceDir binaries
    unless (C.quiet gopts) $ do
      home <- getHomeDirectory
      let binDir = home </> ".acton" </> "bin"
      putStrLn ("Installed " ++ appName ++ " to " ++ binDir)

uninstallCommand :: C.GlobalOptions -> C.UninstallOptions -> IO ()
uninstallCommand gopts opts = do
    let appName = C.uninstallName opts
    validateInstallName appName
    home <- getHomeDirectory
    let binDir = home </> ".acton" </> "bin"
        installedDir = home </> ".acton" </> "installed"
        manifestPath = installedDir </> appName ++ ".json"
    exists <- doesFileExist manifestPath
    if not exists
      then unless (C.quiet gopts) $
        putStrLn ("Application package " ++ appName ++ " is not installed. Nothing to do.")
      else do
        manifest <- readInstallManifest manifestPath
        when (manifestName manifest /= appName) $
          throwProjectError ("ERROR: Install manifest " ++ manifestPath ++ " belongs to " ++ manifestName manifest)
        manifests <- readInstallManifests installedDir
        let otherOwners = M.fromList
              [ (bin, manifestName other)
              | other <- manifests
              , manifestName other /= appName
              , bin <- manifestBinaries other
              ]
        forM_ (manifestBinaries manifest) $ \binName ->
          case M.lookup binName otherOwners of
            Just owner ->
              throwProjectError ("ERROR: Cannot uninstall " ++ appName ++ ": " ++ binName ++ " is also owned by " ++ owner)
            Nothing -> do
              let dest = binDir </> binName
              pathExists <- doesPathExist dest
              fileExists <- doesFileExist dest
              when (pathExists && not fileExists) $
                throwProjectError ("ERROR: Cannot uninstall " ++ appName ++ ": " ++ dest ++ " is not a regular file")
              when fileExists (removeFile dest)
        removeFile manifestPath
        unless (C.quiet gopts) $
          putStrLn ("Uninstalled " ++ appName)

pkgAddCommand :: C.GlobalOptions -> C.PkgAddOptions -> IO ()
pkgAddCommand _ opts = do
    let depName = C.pkgAddName opts
    validateDepName depName
    cwd <- getCurrentDirectory
    spec0 <- loadBuildSpec cwd
    manager <- newTlsManager
    token <- resolveGithubToken (normalizeMaybe (C.pkgAddGithubToken opts))
    let urlArg = C.pkgAddUrl opts
        repoUrlArg = C.pkgAddRepoUrl opts
        repoRefArg = normalizeMaybe (C.pkgAddRepoRef opts)
        pkgNameArg = C.pkgAddPkgName opts
    (depUrl, depRepoUrl, depRepoRef) <- decideUrl manager token depName urlArg repoUrlArg repoRefArg pkgNameArg
    zigExe <- getZigExe
    hash <- requireRight =<< zigFetchHash zigExe depUrl
    let (spec1, msgs) = upsertPkgDep spec0 depName depUrl hash depRepoUrl depRepoRef
    mapM_ putStrLn msgs
    writeBuildSpec spec1
  where
    decideUrl manager token depName urlArg repoUrlArg repoRefArg pkgNameArg
      | not (null urlArg) && not (null repoUrlArg) =
          throwProjectError "ERROR: Specify either --url or --repo-url, not both."
      | not (null urlArg) =
          return (urlArg, Nothing, Nothing)
      | not (null repoUrlArg) = do
          ensureGithubUrl "pkg add" repoUrlArg
          archive <- requireRight =<< resolveGithubArchiveUrl manager token repoUrlArg repoRefArg
          return (archive, Just repoUrlArg, repoRefArg)
      | otherwise = do
          repoUrl <- lookupRepoUrlFromIndex depName pkgNameArg
          ensureGithubUrl "pkg add" repoUrl
          archive <- requireRight =<< resolveGithubArchiveUrl manager token repoUrl repoRefArg
          return (archive, Just repoUrl, repoRefArg)

pkgRemoveCommand :: C.GlobalOptions -> C.PkgRemoveOptions -> IO ()
pkgRemoveCommand _ opts = do
    let depName = C.pkgRemoveName opts
    validateDepName depName
    cwd <- getCurrentDirectory
    spec <- loadBuildSpec cwd
    let deps = BuildSpec.dependencies spec
    if M.member depName deps
      then do
        putStrLn ("Removed package dependency " ++ depName)
        let spec' = spec { BuildSpec.dependencies = M.delete depName deps }
        writeBuildSpec spec'
      else putStrLn ("Dependency " ++ depName ++ " not found in build configuration. Nothing to do.")

pkgUpgradeCommand :: C.GlobalOptions -> C.PkgUpgradeOptions -> IO ()
pkgUpgradeCommand _ opts = do
    cwd <- getCurrentDirectory
    spec <- loadBuildSpec cwd
    let deps = BuildSpec.dependencies spec
    manager <- newTlsManager
    token <- resolveGithubToken (normalizeMaybe (C.pkgUpgradeGithubToken opts))
    resolved <- mapM (resolveDep manager token) (M.toList deps)
    let newUrls = M.fromList [ (n,u) | Just (n,u) <- resolved ]
    if M.null newUrls
      then putStrLn "No dependencies to upgrade"
      else do
        zigExe <- getZigExe
        hashes <- mapM (fetchHash zigExe deps) (M.toList newUrls)
        let newHashes = M.fromList [ (n,h) | Just (n,h) <- hashes ]
        if M.null newHashes
          then putStrLn "No dependencies to upgrade"
          else do
            let (spec', updated) = applyUpgrades spec newUrls newHashes
            if updated
              then do
                putStrLn "Wrote changes to Build.act"
                writeBuildSpec spec'
              else putStrLn "No changes to Build.act"
  where
    resolveDep manager token (depName, dep) =
      case BuildSpec.repo_url dep of
        Nothing -> do
          hPutStrLn stderr (depName ++ " - skipping upgrade: repo_url not set")
          return Nothing
        Just repoUrl -> do
          if not (isGithubUrl repoUrl)
            then do
              hPutStrLn stderr (depName ++ " - skipping upgrade: Unsupported git forge URL for " ++ depName ++ ": " ++ repoUrl ++ " only https://github.com is supported")
              return Nothing
            else do
              putStrLn (depName ++ " - fetching ref from " ++ repoUrl)
              res <- resolveGithubArchiveUrl manager token repoUrl (BuildSpec.repo_ref dep)
              case res of
                Left err -> do
                  hPutStrLn stderr ("Error fetching ref for " ++ depName ++ ": " ++ err)
                  return Nothing
                Right url -> return (Just (depName, url))

    fetchHash zigExe deps (depName, url) =
      case M.lookup depName deps of
        Nothing -> return Nothing
        Just dep ->
          case BuildSpec.url dep of
            Just oldUrl | oldUrl == url -> return Nothing
            _ -> do
              res <- zigFetchHash zigExe url
              case res of
                Left err -> do
                  putStrLn ("Error fetching updated hash for " ++ depName ++ " " ++ err)
                  return Nothing
                Right h -> return (Just (depName, h))

    applyUpgrades spec newUrls newHashes =
      let deps = BuildSpec.dependencies spec
          (deps', updated) = M.foldlWithKey' applyOne (deps, False) deps
      in (spec { BuildSpec.dependencies = deps' }, updated)
      where
        applyOne (acc, changed) depName dep =
          case (M.lookup depName newUrls, M.lookup depName newHashes) of
            (Just newUrl, Just newHash) ->
              if BuildSpec.url dep /= Just newUrl || BuildSpec.hash dep /= Just newHash
                then let dep' = dep { BuildSpec.url = Just newUrl, BuildSpec.hash = Just newHash }
                     in (M.insert depName dep' acc, True)
                else (acc, changed)
            _ -> (acc, changed)

pkgUpdateCommand :: C.GlobalOptions -> IO ()
pkgUpdateCommand _ = do
    let indexUrl = "https://actonlang.github.io/package-index/index.json"
    manager <- newTlsManager
    body <- requireRightWith ("ERROR: Failed to download package index from " ++ indexUrl ++ ": ") =<< httpGet manager [] indexUrl
    home <- getHomeDirectory
    let indexDir = home </> ".cache" </> "acton"
        indexPath = indexDir </> "package-index.json"
    createDirectoryIfMissing True indexDir
    BL.writeFile indexPath body
    putStrLn ("Updated package index at " ++ indexPath)

pkgSearchCommand :: C.GlobalOptions -> C.PkgSearchOptions -> IO ()
pkgSearchCommand _ opts = do
    home <- getHomeDirectory
    let indexPath = home </> ".cache" </> "acton" </> "package-index.json"
    exists <- doesFileExist indexPath
    unless exists $
      throwProjectError ("No package index found at " ++ indexPath ++ "\nHINT: Run 'acton pkg update' to create or refresh it.")
    content <- readIndexFile indexPath
    pkgs <- requireRight (decodePackageIndex content)
    let terms = C.pkgSearchTerms opts
    regexes <- compileTerms terms
    let matched = filter (matchesAllRegexes regexes) pkgs
    let printPkg pkg = do
          putStrLn (pkgName pkg)
          putStrLn ("  " ++ pkgDescription pkg)
          putStrLn ("  " ++ pkgRepoUrl pkg)
          putStrLn ""
    if null matched
      then putStrLn "No packages matched your search."
      else forM_ (sortOn pkgName matched) printPkg

zigPkgAddCommand :: C.GlobalOptions -> C.ZigPkgAddOptions -> IO ()
zigPkgAddCommand _ opts = do
    let depName = C.zigPkgAddName opts
        depUrl = C.zigPkgAddUrl opts
        depArtifacts = C.zigPkgAddArtifacts opts
    validateDepName depName
    cwd <- getCurrentDirectory
    spec0 <- loadBuildSpec cwd
    zigExe <- getZigExe
    hash <- requireRight =<< zigFetchHash zigExe depUrl
    let (spec1, msgs) = upsertZigDep spec0 depName depUrl hash depArtifacts
    mapM_ putStrLn msgs
    writeBuildSpec spec1

zigPkgRemoveCommand :: C.GlobalOptions -> C.ZigPkgRemoveOptions -> IO ()
zigPkgRemoveCommand _ opts = do
    let depName = C.zigPkgRemoveName opts
    validateDepName depName
    cwd <- getCurrentDirectory
    spec <- loadBuildSpec cwd
    let deps = BuildSpec.zig_dependencies spec
    if M.member depName deps
      then do
        putStrLn ("Removed Zig package dependency " ++ depName)
        let spec' = spec { BuildSpec.zig_dependencies = M.delete depName deps }
        writeBuildSpec spec'
      else putStrLn ("Zig dependency " ++ depName ++ " not found in build configuration. Nothing to do.")

prepareInstallSource :: String -> String -> String -> IO FilePath
prepareInstallSource appName repoUrl commitSha = do
    home <- getHomeDirectory
    let appsDir = home </> ".cache" </> "acton" </> "apps"
        sourceDir = appsDir </> appName ++ "-" ++ take 12 commitSha
        cloneUrl = githubCloneUrl repoUrl
    createDirectoryIfMissing True appsDir
    exists <- doesDirectoryExist sourceDir
    unless exists $
      runProcessChecked Nothing "git" ["clone", "--quiet", "--no-checkout", cloneUrl, sourceDir]
    runProcessChecked (Just sourceDir) "git" ["fetch", "--quiet", "origin", commitSha]
    runProcessChecked (Just sourceDir) "git" ["checkout", "--quiet", "--force", "--detach", commitSha]
    runProcessChecked (Just sourceDir) "git" ["clean", "-fdx", "--quiet"]
    return sourceDir

githubCloneUrl :: String -> String
githubCloneUrl = takeWhile (/= '#')

discoverBuiltBinaries :: FilePath -> IO [(FilePath, String, Permissions)]
discoverBuiltBinaries sourceDir = do
    let binDir = sourceDir </> "out" </> "bin"
    exists <- doesDirectoryExist binDir
    unless exists $
      throwProjectError ("ERROR: Build produced no out/bin directory in " ++ sourceDir)
    names <- listDirectory binDir
    files <- filterM (\name -> doesFileExist (binDir </> name)) names
    when (null files) $
      throwProjectError ("ERROR: Build produced no binaries in " ++ binDir)
    forM (sortOn id files) $ \name -> do
      let src = binDir </> name
      perms <- getPermissions src
      return (src, name, perms)

installBuiltBinaries :: String -> String -> Maybe String -> String -> String -> FilePath -> [(FilePath, String, Permissions)] -> IO ()
installBuiltBinaries appName repoUrl repoRefArg commitSha archiveHash sourceDir binaries = do
    home <- getHomeDirectory
    let actonDir = home </> ".acton"
        binDir = actonDir </> "bin"
        installedDir = actonDir </> "installed"
        manifestPath = installedDir </> appName ++ ".json"
    createDirectoryIfMissing True binDir
    createDirectoryIfMissing True installedDir
    manifests <- readInstallManifests installedDir
    let ownerByBin = M.fromList
          [ (bin, manifestName manifest)
          | manifest <- manifests
          , bin <- manifestBinaries manifest
          ]
        oldManifest = findManifest appName manifests
    forM_ binaries $ \(_, binName, _) -> do
      let dest = binDir </> binName
      case M.lookup binName ownerByBin of
        Just owner | owner /= appName ->
          throwProjectError ("ERROR: Cannot install " ++ appName ++ ": " ++ dest ++ " is owned by " ++ owner)
        _ -> return ()
      exists <- doesPathExist dest
      when (exists && M.lookup binName ownerByBin == Nothing) $
        throwProjectError ("ERROR: Cannot install " ++ appName ++ ": " ++ dest ++ " already exists and is not managed by Acton")
    forM_ binaries $ \(src, binName, perms) -> do
      let dest = binDir </> binName
      copyFile src dest
      setPermissions dest perms
    case oldManifest of
      Nothing -> return ()
      Just manifest -> do
        let newBins = map (\(_, binName, _) -> binName) binaries
            staleBins = filter (`notElem` newBins) (manifestBinaries manifest)
        forM_ staleBins $ \binName -> do
          let dest = binDir </> binName
          exists <- doesFileExist dest
          when exists (removeFile dest)
    writeInstallManifest manifestPath InstallManifest
      { manifestName = appName
      , manifestRepoUrl = repoUrl
      , manifestRepoRef = repoRefArg
      , manifestCommit = commitSha
      , manifestHash = archiveHash
      , manifestSourceDir = sourceDir
      , manifestBinaries = map (\(_, binName, _) -> binName) binaries
      }

findManifest :: String -> [InstallManifest] -> Maybe InstallManifest
findManifest appName = go
  where
    go [] = Nothing
    go (manifest:rest)
      | manifestName manifest == appName = Just manifest
      | otherwise = go rest

readInstallManifests :: FilePath -> IO [InstallManifest]
readInstallManifests installedDir = do
    exists <- doesDirectoryExist installedDir
    if not exists
      then return []
      else do
        names <- listDirectory installedDir
        let jsonFiles = filter (".json" `isSuffixOf`) names
        mapM (readInstallManifest . (installedDir </>)) jsonFiles

readInstallManifest :: FilePath -> IO InstallManifest
readInstallManifest path = do
    content <- readIndexFile path
    case Aeson.eitherDecode content of
      Left err -> throwProjectError ("ERROR: Failed to parse install manifest " ++ path ++ ": " ++ err)
      Right (Aeson.Object obj) ->
        case ( lookupString "name" obj
             , lookupString "repo_url" obj
             , lookupString "commit" obj
             , lookupString "hash" obj
             , lookupString "source_dir" obj
             , lookupStringList "binaries" obj
             ) of
          (Just n, Just ru, Just c, Just h, Just src, Just bins) ->
            return InstallManifest
              { manifestName = n
              , manifestRepoUrl = ru
              , manifestRepoRef = lookupString "repo_ref" obj
              , manifestCommit = c
              , manifestHash = h
              , manifestSourceDir = src
              , manifestBinaries = bins
              }
          _ ->
            throwProjectError ("ERROR: Invalid install manifest " ++ path)
      Right _ ->
        throwProjectError ("ERROR: Invalid install manifest " ++ path)

writeInstallManifest :: FilePath -> InstallManifest -> IO ()
writeInstallManifest path manifest =
    BL.writeFile path (Aeson.encode (Aeson.object (manifestFields manifest)))
  where
    manifestFields manifest =
      [ "name" Aeson..= manifestName manifest
      , "repo_url" Aeson..= manifestRepoUrl manifest
      , "commit" Aeson..= manifestCommit manifest
      , "hash" Aeson..= manifestHash manifest
      , "source_dir" Aeson..= manifestSourceDir manifest
      , "binaries" Aeson..= manifestBinaries manifest
      ] ++ repoRefField manifest
    repoRefField manifest =
      case manifestRepoRef manifest of
        Nothing -> []
        Just repoRef -> ["repo_ref" Aeson..= repoRef]

upsertPkgDep :: BuildSpec.BuildSpec -> String -> String -> String -> Maybe String -> Maybe String -> (BuildSpec.BuildSpec, [String])
upsertPkgDep spec depName depUrl depHash depRepoUrl depRepoRef =
    case M.lookup depName (BuildSpec.dependencies spec) of
      Just dep ->
        let (msgs1, dep1) = updateUrl dep
            (msgs2, dep2) = updateHash dep1
            dep3 = case depRepoUrl of
                     Nothing -> dep2
                     Just ru -> dep2 { BuildSpec.repo_url = Just ru, BuildSpec.repo_ref = depRepoRef }
            deps' = M.insert depName dep3 (BuildSpec.dependencies spec)
        in (spec { BuildSpec.dependencies = deps' }, msgs1 ++ msgs2)
      Nothing ->
        let newDep = BuildSpec.PkgDep
              { BuildSpec.url = Just depUrl
              , BuildSpec.hash = Just depHash
              , BuildSpec.path = Nothing
              , BuildSpec.repo_url = depRepoUrl
              , BuildSpec.repo_ref = depRepoRef
              }
            deps' = M.insert depName newDep (BuildSpec.dependencies spec)
        in (spec { BuildSpec.dependencies = deps' }, ["Added new package dependency " ++ depName ++ " with hash " ++ depHash])
  where
    updateUrl dep =
      case BuildSpec.url dep of
        Just old | old /= depUrl ->
          (["Updated existing dependency " ++ depName ++ " with new URL " ++ depUrl ++ " (old " ++ showMaybe (Just old) ++ ")"], dep { BuildSpec.url = Just depUrl })
        Nothing ->
          (["Updated existing dependency " ++ depName ++ " with new URL " ++ depUrl ++ " (old " ++ showMaybe Nothing ++ ")"], dep { BuildSpec.url = Just depUrl })
        _ -> ([], dep)

    updateHash dep =
      case BuildSpec.hash dep of
        Just old | old == depHash ->
          (["Dependency " ++ depName ++ " is already up to date, hash: " ++ depHash], dep)
        Just old ->
          (["Updated existing dependency " ++ depName ++ " with new hash " ++ depHash ++ " (old " ++ showMaybe (Just old) ++ ")"], dep { BuildSpec.hash = Just depHash })
        Nothing ->
          (["Updated existing dependency " ++ depName ++ " with new hash " ++ depHash ++ " (old " ++ showMaybe Nothing ++ ")"], dep { BuildSpec.hash = Just depHash })

upsertZigDep :: BuildSpec.BuildSpec -> String -> String -> String -> [String] -> (BuildSpec.BuildSpec, [String])
upsertZigDep spec depName depUrl depHash depArtifacts =
    case M.lookup depName (BuildSpec.zig_dependencies spec) of
      Just dep ->
        let (msgs1, dep1) = updateUrl dep
            (msgs2, dep2) = updateHash dep1
            deps' = M.insert depName dep2 (BuildSpec.zig_dependencies spec)
        in (spec { BuildSpec.zig_dependencies = deps' }, msgs1 ++ msgs2)
      Nothing ->
        let newDep = BuildSpec.ZigDep
              { BuildSpec.zurl = Just depUrl
              , BuildSpec.zhash = Just depHash
              , BuildSpec.zpath = Nothing
              , BuildSpec.options = M.empty
              , BuildSpec.artifacts = depArtifacts
              }
            deps' = M.insert depName newDep (BuildSpec.zig_dependencies spec)
        in (spec { BuildSpec.zig_dependencies = deps' }, ["Added new Zig package dependency " ++ depName ++ " with hash " ++ depHash])
  where
    updateUrl dep =
      case BuildSpec.zurl dep of
        Just old | old /= depUrl ->
          (["Updated existing dependency " ++ depName ++ " with new URL " ++ depUrl ++ " (old " ++ showMaybe (Just old) ++ ")"], dep { BuildSpec.zurl = Just depUrl })
        Nothing ->
          (["Updated existing dependency " ++ depName ++ " with new URL " ++ depUrl ++ " (old " ++ showMaybe Nothing ++ ")"], dep { BuildSpec.zurl = Just depUrl })
        _ -> ([], dep)

    updateHash dep =
      case BuildSpec.zhash dep of
        Just old | old == depHash ->
          (["Dependency " ++ depName ++ " is already up to date, hash: " ++ depHash], dep)
        Just old ->
          (["Updated existing dependency " ++ depName ++ " with new hash " ++ depHash ++ " (old " ++ showMaybe (Just old) ++ ")"], dep { BuildSpec.zhash = Just depHash })
        Nothing ->
          (["Updated existing dependency " ++ depName ++ " with new hash " ++ depHash ++ " (old " ++ showMaybe Nothing ++ ")"], dep { BuildSpec.zhash = Just depHash })

writeBuildSpec :: BuildSpec.BuildSpec -> IO ()
writeBuildSpec spec = do
    buildActExists <- doesFileExist "Build.act"
    if not buildActExists
      then throwProjectError "Build.act not found in current directory"
      else do
        content <- readFile "Build.act"
        let jsonDoc = BuildSpec.encodeBuildSpecJSON spec
        case BuildSpec.updateBuildActFromJSON content jsonDoc of
          Left err -> throwProjectError ("Failed to update Build.act: \n" ++ err)
          Right updated -> writeFile "Build.act" updated

lookupRepoUrlFromIndex :: String -> String -> IO String
lookupRepoUrlFromIndex = lookupRepoUrlFromIndexByKind "library"

lookupRepoUrlFromIndexByKind :: String -> String -> String -> IO String
lookupRepoUrlFromIndexByKind wantedKind depName pkgNameArg = do
    home <- getHomeDirectory
    let indexPath = home </> ".cache" </> "acton" </> "package-index.json"
    exists <- doesFileExist indexPath
    unless exists $
      throwProjectError ("ERROR: Package index not found at " ++ indexPath ++ "\nHINT: Run: acton pkg update")
    content <- readIndexFile indexPath
    entries <- requireRight (decodePackageIndexEntries content)
    let pkgName = if null pkgNameArg then depName else pkgNameArg
        matchEntry e = indexName e == pkgName
    case filter matchEntry entries of
      [] -> throwProjectError ("ERROR: Package " ++ pkgName ++ " not found in package index")
      entriesForName ->
        case filter (elem wantedKind . indexKinds) entriesForName of
          (entry:_) -> return (indexRepoUrl entry)
          [] -> throwProjectError ("ERROR: Package " ++ pkgName ++ " is not an acton-" ++ wantedKind ++ " package")

decodePackageIndex :: BL.ByteString -> Either String [PackageEntry]
decodePackageIndex = decodePackageIndexByKind "library"

decodeAppPackageIndex :: BL.ByteString -> Either String [PackageEntry]
decodeAppPackageIndex = decodePackageIndexByKind "app"

decodePackageIndexByKind :: String -> BL.ByteString -> Either String [PackageEntry]
decodePackageIndexByKind wantedKind content = do
    entries <- decodePackageIndexEntries content
    let pkgs = [ PackageEntry n d r
               | PackageIndexEntry n ks d r <- entries
               , wantedKind `elem` ks
               ]
    return pkgs

decodePackageIndexEntries :: BL.ByteString -> Either String [PackageIndexEntry]
decodePackageIndexEntries content =
    case Aeson.eitherDecode content of
      Left err -> Left ("ERROR: Failed to parse package index JSON: " ++ err)
      Right (Aeson.Object obj) ->
        case AesonKM.lookup (AesonKey.fromString "packages") obj of
          Just (Aeson.Array arr) -> mapM parseEntry (zip [0 :: Int ..] (toList arr))
          _ -> Left "ERROR: Invalid package index: top-level 'packages' list missing or wrong type"
      Right _ ->
        Left "ERROR: Invalid package index: top-level 'packages' list missing or wrong type"
  where
    parseEntry (idx, Aeson.Object o) =
      case ( lookupString "name" o
           , lookupStringList "kinds" o
           , lookupString "description" o
           , lookupString "repo_url" o
           ) of
        (Just n, Just ks, Just d, Just r)
          | null ks ->
              Left ("ERROR: Invalid package index entry " ++ show idx ++ ": expected at least one kind")
          | all validPackageKind ks -> Right (PackageIndexEntry n ks d r)
          | otherwise ->
              Left ("ERROR: Invalid package index entry " ++ show idx ++ ": unsupported kind in 'kinds'")
        _ ->
          Left ("ERROR: Invalid package index entry " ++ show idx ++ ": expected name, kinds, description, and repo_url")
    parseEntry (idx, _) =
      Left ("ERROR: Invalid package index entry " ++ show idx ++ ": expected object")

    validPackageKind k = k == "library" || k == "app"

matchesAllTerms :: [String] -> PackageEntry -> IO Bool
matchesAllTerms terms pkg = do
    regexes <- compileTerms terms
    return (matchesAllRegexes regexes pkg)

compileTerms :: [String] -> IO [TDFA.Regex]
compileTerms terms =
    mapM compileRegex (filter (not . null) terms)

compileRegex :: String -> IO TDFA.Regex
compileRegex pattern = do
    res <- try (evaluate (TDFA.makeRegex ("^" ++ pattern) :: TDFA.Regex)) :: IO (Either SomeException TDFA.Regex)
    case res of
      Left err -> throwProjectError ("ERROR: Invalid regex '" ++ pattern ++ "': " ++ displayException err)
      Right re -> return re

matchesAllRegexes :: [TDFA.Regex] -> PackageEntry -> Bool
matchesAllRegexes regexes pkg =
    let haystack = pkgName pkg ++ " " ++ pkgDescription pkg ++ " " ++ pkgRepoUrl pkg
    in all (\re -> isJust (TDFA.matchOnceText re haystack)) regexes

parseGithubRepoUrl :: String -> Either String RepoInfo
parseGithubRepoUrl url =
    case stripPrefixGithub url of
      Nothing -> Left "Unsupported git forge URL"
      Just rest -> do
        let (pathPart, refPart) = break (== '#') rest
            refVal = if null refPart then Nothing else Just (drop 1 refPart)
            path = dropWhile (== '/') (dropGitSuffix pathPart)
            parts = filter (not . null) (splitOn "/" path)
        if length parts < 2
          then Left "No path in URL"
          else do
            let owner = parts !! (length parts - 2)
                repo = parts !! (length parts - 1)
            Right (RepoInfo owner repo refVal)
  where
    stripPrefixGithub s
      | prefix `isPrefixOf` s = Just (drop (length prefix) s)
      | otherwise = Nothing
      where
        prefix = "https://github.com" :: String

    dropGitSuffix s =
      if ".git" `isSuffixOf` s then take (length s - 4) s else s

commitShaFromArchiveUrl :: String -> Either String String
commitShaFromArchiveUrl url =
    case splitOn "/archive/" url of
      [_, archiveName]
        | ".zip" `isSuffixOf` archiveName ->
            let zipSuffix = ".zip" :: String
                sha = take (length archiveName - length zipSuffix) archiveName
            in if null sha
                 then Left ("Unable to determine commit SHA from " ++ url)
                 else Right sha
      _ -> Left ("Unable to determine commit SHA from " ++ url)

resolveGithubArchiveUrl :: Manager -> Maybe String -> String -> Maybe String -> IO (Either String String)
resolveGithubArchiveUrl manager token repoUrl repoRefArg = do
    case parseGithubRepoUrl repoUrl of
      Left err -> return (Left err)
      Right info -> do
        ref <- selectRef manager token info repoRefArg
        case ref of
          Left err -> return (Left err)
          Right refName -> do
            sha <- fetchRefSha manager token info refName
            case sha of
              Left err -> return (Left err)
              Right shaVal ->
                return (Right ("https://github.com/" ++ repoOwner info ++ "/" ++ repoName info ++ "/archive/" ++ shaVal ++ ".zip"))
  where
    selectRef manager token info refArg =
      case refArg of
        Just refVal ->
          case repoRef info of
            Just urlRef | urlRef /= refVal -> return (Left "Ref mismatch")
            _ -> return (Right refVal)
        Nothing ->
          case repoRef info of
            Just urlRef -> return (Right urlRef)
            Nothing -> fetchDefaultBranch manager token info

fetchDefaultBranch :: Manager -> Maybe String -> RepoInfo -> IO (Either String String)
fetchDefaultBranch manager token info = do
    let url = "https://api.github.com/repos/" ++ repoOwner info ++ "/" ++ repoName info
    obj <- fetchGithubObject manager token url
    case obj of
      Left err -> return (Left err)
      Right o ->
        case lookupMessage o of
          Just msg -> return (Left ("Unable to retrieve branch information: " ++ msg))
          Nothing ->
            case lookupString "default_branch" o of
              Just branch -> return (Right branch)
              Nothing -> return (Left ("No default branch:" ++ B.unpack (BL.toStrict (Aeson.encode (Aeson.Object o)))))

fetchRefSha :: Manager -> Maybe String -> RepoInfo -> String -> IO (Either String String)
fetchRefSha manager token info refName = do
    if isGithubCommitSha refName
      then return (Right refName)
      else do
        branch <- fetchGitRefSha manager token info ("heads/" ++ refName)
        case branch of
          Right shaVal -> return (Right shaVal)
          Left branchErr -> do
            tag <- fetchGitRefSha manager token info ("tags/" ++ refName)
            case tag of
              Right shaVal -> return (Right shaVal)
              Left tagErr -> do
                commit <- fetchCommitSha manager token info refName
                case commit of
                  Right shaVal -> return (Right shaVal)
                  Left commitErr ->
                    return (Left ("Unable to resolve ref '" ++ refName ++ "' as branch, tag, or commit SHA: "
                               ++ branchErr ++ "; " ++ tagErr ++ "; " ++ commitErr))

isGithubCommitSha :: String -> Bool
isGithubCommitSha refName =
    length refName == 40 && all isHexDigit refName

fetchGitRefSha :: Manager -> Maybe String -> RepoInfo -> String -> IO (Either String String)
fetchGitRefSha manager token info refName = do
    let url = "https://api.github.com/repos/" ++ repoOwner info ++ "/" ++ repoName info ++ "/git/refs/" ++ refName
    obj <- fetchGithubObject manager token url
    case obj of
      Left err -> return (Left err)
      Right o ->
        case lookupMessage o of
          Just msg -> return (Left ("Unable to retrieve ref information: " ++ msg))
          Nothing ->
            case AesonKM.lookup (AesonKey.fromString "object") o of
              Just (Aeson.Object obj) ->
                case (lookupString "type" obj, lookupString "sha" obj) of
                  (Just objType, Just shaVal) -> resolveGitObjectSha manager token info objType shaVal
                  (_, Nothing) -> return (Left "No SHA")
                  (Nothing, _) -> return (Left "No object type")
              _ -> return (Left "No object")

resolveGitObjectSha :: Manager -> Maybe String -> RepoInfo -> String -> String -> IO (Either String String)
resolveGitObjectSha manager token info objType shaVal
  | objType == "commit" = return (Right shaVal)
  | objType == "tag" = fetchTagTargetSha manager token info shaVal 5
  | otherwise = return (Left ("Unsupported Git object type " ++ objType))

fetchTagTargetSha :: Manager -> Maybe String -> RepoInfo -> String -> Int -> IO (Either String String)
fetchTagTargetSha _ _ _ _ 0 =
    return (Left "Annotated tag nesting too deep")
fetchTagTargetSha manager token info tagSha depth = do
    let url = "https://api.github.com/repos/" ++ repoOwner info ++ "/" ++ repoName info ++ "/git/tags/" ++ tagSha
    obj <- fetchGithubObject manager token url
    case obj of
      Left err -> return (Left err)
      Right o ->
        case lookupMessage o of
          Just msg -> return (Left ("Unable to retrieve tag information: " ++ msg))
          Nothing ->
            case AesonKM.lookup (AesonKey.fromString "object") o of
              Just (Aeson.Object target) ->
                case (lookupString "type" target, lookupString "sha" target) of
                  (Just objType, Just shaVal)
                    | objType == "tag" -> fetchTagTargetSha manager token info shaVal (depth - 1)
                    | otherwise -> resolveGitObjectSha manager token info objType shaVal
                  (_, Nothing) -> return (Left "No tag target SHA")
                  (Nothing, _) -> return (Left "No tag target object type")
              _ -> return (Left "No tag target object")

fetchCommitSha :: Manager -> Maybe String -> RepoInfo -> String -> IO (Either String String)
fetchCommitSha manager token info refName = do
    let url = "https://api.github.com/repos/" ++ repoOwner info ++ "/" ++ repoName info ++ "/commits/" ++ refName
    obj <- fetchGithubObject manager token url
    case obj of
      Left err -> return (Left err)
      Right o ->
        case lookupMessage o of
          Just msg -> return (Left ("Unable to retrieve commit information: " ++ msg))
          Nothing ->
            case lookupString "sha" o of
              Just shaVal -> return (Right shaVal)
              Nothing -> return (Left "No commit SHA")

fetchGithubObject :: Manager -> Maybe String -> String -> IO (Either String Aeson.Object)
fetchGithubObject manager token url = do
    let authHeader =
          case token of
            Just t -> [("Authorization", B.pack ("Bearer " ++ t))]
            Nothing -> []
        headers = ("Accept", "application/vnd.github.v3+json") : authHeader
    body <- httpGet manager headers url
    case body of
      Left err -> return (Left ("Failed to download " ++ url ++ ": " ++ err))
      Right bs ->
        case Aeson.eitherDecode bs of
          Left err -> return (Left err)
          Right (Aeson.Object obj) -> return (Right obj)
          Right _ -> return (Left "Invalid JSON response")

httpGet :: Manager -> [Header] -> String -> IO (Either String BL.ByteString)
httpGet manager extraHeaders url = do
    req0 <- parseRequest url
    let headers = ("User-Agent", "acton") : extraHeaders
        req = req0 { requestHeaders = headers ++ requestHeaders req0 }
    let maxAttempts = 10
        baseDelay = 500000
        maxDelay = 120000000
    let go attempt delay = do
          res <- try (httpLbs req manager) :: IO (Either SomeException (Response BL.ByteString))
          case res of
            Left err ->
              retryOrFail attempt delay (displayException err)
            Right response -> do
              let status = statusCode (responseStatus response)
                  body = responseBody response
              if status >= 200 && status < 300
                then return (Right body)
                else
                  if shouldRetry status
                    then retryOrFail attempt delay (httpErrorMessage status body)
                    else return (Left (httpErrorMessage status body))
        retryOrFail attempt delay errMsg
          | attempt >= maxAttempts = return (Left errMsg)
          | otherwise = do
              threadDelay delay
              go (attempt + 1) (min maxDelay (delay * 2))
    go 1 baseDelay
  where
    shouldRetry status = status == 403 || status == 429 || status >= 500
    httpErrorMessage status body =
      let raw = trim (B.unpack (BL.toStrict body))
          clipped = take 200 raw
          suffix = if length raw > length clipped then "..." else ""
          detail = if null clipped then "" else ": " ++ clipped ++ suffix
      in "HTTP " ++ show status ++ detail

lookupString :: String -> Aeson.Object -> Maybe String
lookupString key obj =
    case AesonKM.lookup (AesonKey.fromString key) obj of
      Just (Aeson.String s) -> Just (T.unpack s)
      _ -> Nothing

lookupStringList :: String -> Aeson.Object -> Maybe [String]
lookupStringList key obj =
    case AesonKM.lookup (AesonKey.fromString key) obj of
      Just (Aeson.Array arr) -> mapM valueString (toList arr)
      _ -> Nothing
  where
    valueString (Aeson.String s) = Just (T.unpack s)
    valueString _ = Nothing

lookupMessage :: Aeson.Object -> Maybe String
lookupMessage = lookupString "message"

normalizeMaybe :: String -> Maybe String
normalizeMaybe s
    | null s = Nothing
    | otherwise = Just s

resolveGithubToken :: Maybe String -> IO (Maybe String)
resolveGithubToken tokenOpt = do
    envToken <- lookupEnv "GITHUB_TOKEN"
    return (selectToken tokenOpt envToken)
  where
    normalizeToken Nothing = Nothing
    normalizeToken (Just s) = normalizeMaybe (trim s)
    selectToken opt env =
      case normalizeToken opt of
        Just token -> Just token
        Nothing -> normalizeToken env

showMaybe :: Maybe String -> String
showMaybe = maybe "None" id

isGithubUrl :: String -> Bool
isGithubUrl url = "https://github.com" `isPrefixOf` url

ensureGithubUrl :: String -> String -> IO ()
ensureGithubUrl cmd url =
    unless (isGithubUrl url) $
      throwProjectError ("ERROR: Unsupported git forge URL for " ++ cmd ++ ": " ++ url ++ " only https://github.com is supported")

requireRight :: Either String a -> IO a
requireRight (Left err) = throwProjectError err
requireRight (Right val) = return val

requireRightWith :: String -> Either String a -> IO a
requireRightWith prefix (Left err) = throwProjectError (prefix ++ err)
requireRightWith _ (Right val) = return val

readIndexFile :: FilePath -> IO BL.ByteString
readIndexFile path = do
    res <- try (BL.readFile path) :: IO (Either IOException BL.ByteString)
    case res of
      Left err -> throwProjectError ("ERROR: Failed to read package index at " ++ path ++ ": " ++ displayException err)
      Right content -> return content

zigFetchHash :: FilePath -> String -> IO (Either String String)
zigFetchHash zigExe depUrl = do
    home <- getHomeDirectory
    let globalCache = home </> ".cache" </> "acton" </> "zig-global-cache"
    createDirectoryIfMissing True globalCache
    createDirectoryIfMissing True (globalCache </> "tmp")
    withSystemTempDirectory "acton-zig-fetch" $ \tmp -> do
      writeFile (tmp </> "build.zig") zigFetchBuildZig
      let cmd = (proc zigExe ["fetch", "--global-cache-dir", globalCache, depUrl]) { cwd = Just tmp }
      let maxAttempts = 10
          baseDelay = 500000
          maxDelay = 120000000
      let go attempt delay = do
            res <- try (readCreateProcessWithExitCode cmd "") :: IO (Either SomeException (ExitCode, String, String))
            case res of
              Left err ->
                retryOrFail attempt delay ("Error fetching " ++ displayException err)
              Right (ExitSuccess, out, _) -> return (Right (trim out))
              Right (ExitFailure _, _, err) ->
                retryOrFail attempt delay ("Error fetching " ++ trim err)
          retryOrFail attempt delay errMsg
            | attempt >= maxAttempts = return (Left errMsg)
            | otherwise = do
                threadDelay delay
                go (attempt + 1) (min maxDelay (delay * 2))
      go 1 baseDelay

zigFetchBuildZig :: String
zigFetchBuildZig = unlines
    [ "const std = @import(\"std\");"
    , "pub fn build(b: *std.Build) void { _ = b; }"
    ]

runProcessChecked :: Maybe FilePath -> FilePath -> [String] -> IO ()
runProcessChecked cwdOpt exe args = do
    res <- try (readCreateProcessWithExitCode command "") :: IO (Either SomeException (ExitCode, String, String))
    case res of
      Left err ->
        throwProjectError ("ERROR: Failed to run " ++ unwords (exe:args) ++ ": " ++ displayException err)
      Right (ExitSuccess, _, _) ->
        return ()
      Right (ExitFailure code, out, err) ->
        throwProjectError $
          "ERROR: Command failed (" ++ show code ++ "): " ++ unwords (exe:args)
          ++ renderOutput "stdout" out
          ++ renderOutput "stderr" err
  where
    command = (proc exe args) { cwd = cwdOpt }
    renderOutput label output =
      let body = trim output
      in if null body then "" else "\n" ++ label ++ ":\n" ++ body

getZigExe :: IO FilePath
getZigExe = do
    execDir <- takeDirectory <$> getExecutablePath
    sysPath <- canonicalizePath (execDir </> "..")
    return (sysPath </> "zig" </> "zig")

validateDepName :: String -> IO ()
validateDepName name =
    unless (isValidDepName name) $
      throwProjectError ("Invalid dependency name '" ++ name ++ "', must start with a letter and only contain letters, numbers and underscores")

validateInstallName :: String -> IO ()
validateInstallName name =
    unless (isValidInstallName name) $
      throwProjectError ("Invalid application package name '" ++ name ++ "', must only contain letters, numbers, '.', '_' and '-'")

isValidInstallName :: String -> Bool
isValidInstallName name =
    not (null name)
    && name /= "."
    && name /= ".."
    && all isValidInstallNameChar name

isValidInstallNameChar :: Char -> Bool
isValidInstallNameChar c =
    isAsciiAlphaNumUnderscore c || c == '-' || c == '.'

isValidDepName :: String -> Bool
isValidDepName [] = False
isValidDepName (c:cs) = isAsciiAlpha c && all isAsciiAlphaNumUnderscore cs

isAsciiAlpha :: Char -> Bool
isAsciiAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

isAsciiAlphaNumUnderscore :: Char -> Bool
isAsciiAlphaNumUnderscore c = isAsciiAlpha c || (c >= '0' && c <= '9') || c == '_'

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

readFile :: FilePath -> IO String
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
