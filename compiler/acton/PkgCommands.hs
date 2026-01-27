{-# LANGUAGE OverloadedStrings #-}
module PkgCommands
  ( pkgAddCommand
  , pkgRemoveCommand
  , pkgUpgradeCommand
  , pkgUpdateCommand
  , pkgSearchCommand
  , zigPkgAddCommand
  , zigPkgRemoveCommand
  , PackageEntry(..)
  , RepoInfo(..)
  , parseGithubRepoUrl
  , decodePackageIndex
  , matchesAllTerms
  ) where

import Prelude hiding (readFile, writeFile)

import qualified Acton.BuildSpec as BuildSpec
import qualified Acton.CommandLineParser as C
import Acton.Compile (loadBuildSpec, throwProjectError)

import Control.Exception (IOException, SomeException, try, displayException, evaluate)
import Control.Concurrent (threadDelay)
import Control.Monad (forM_, unless)
import Data.Char (isSpace)
import Data.Foldable (toList)
import Data.List (dropWhileEnd, isPrefixOf, isSuffixOf, sortOn)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, isJust)
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
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesFileExist, getCurrentDirectory, getHomeDirectory)
import System.Environment (getExecutablePath, lookupEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeDirectory)
import System.IO (IOMode(ReadMode, WriteMode), hClose, hGetContents, hPutStr, hPutStrLn, hSetEncoding, openFile, stderr, utf8)
import System.Process (proc, readCreateProcessWithExitCode)
import qualified Text.Regex.TDFA as TDFA

data PackageEntry = PackageEntry
    { pkgName        :: String
    , pkgDescription :: String
    , pkgRepoUrl     :: String
    } deriving (Eq, Show)

data PackageEntryRaw = PackageEntryRaw
    { rawName        :: Maybe String
    , rawDescription :: Maybe String
    , rawRepoUrl     :: Maybe String
    } deriving (Eq, Show)

data RepoInfo = RepoInfo
    { repoOwner :: String
    , repoName  :: String
    , repoRef   :: Maybe String
    } deriving (Eq, Show)

pkgAddCommand :: C.GlobalOptions -> C.PkgAddOptions -> IO ()
pkgAddCommand _ opts = do
    let depName = C.pkgAddName opts
    validateDepName depName
    cwd <- getCurrentDirectory
    spec0 <- loadSpecOrEmpty cwd
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
    mspec <- loadBuildSpec cwd
    case mspec of
      Nothing -> putStrLn "No Build.act or build.act.json file found, nothing to do."
      Just spec -> do
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
    mspec <- loadBuildSpec cwd
    case mspec of
      Nothing -> putStrLn "No Build.act or build.act.json file found, nothing to upgrade."
      Just spec -> do
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
                    putStrLn "Wrote changes to build.act.json"
                    writeBuildSpec spec'
                  else putStrLn "No changes to build.act.json"
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
    spec0 <- loadSpecOrEmpty cwd
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
    mspec <- loadBuildSpec cwd
    case mspec of
      Nothing -> putStrLn "No Build.act or build.act.json file found, nothing to do."
      Just spec -> do
        let deps = BuildSpec.zig_dependencies spec
        if M.member depName deps
          then do
            putStrLn ("Removed Zig package dependency " ++ depName)
            let spec' = spec { BuildSpec.zig_dependencies = M.delete depName deps }
            writeBuildSpec spec'
          else putStrLn ("Zig dependency " ++ depName ++ " not found in build configuration. Nothing to do.")

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

loadSpecOrEmpty :: FilePath -> IO BuildSpec.BuildSpec
loadSpecOrEmpty dir = do
    mspec <- loadBuildSpec dir
    return (fromMaybe emptySpec mspec)

emptySpec :: BuildSpec.BuildSpec
emptySpec = BuildSpec.BuildSpec Nothing Nothing M.empty M.empty

writeBuildSpec :: BuildSpec.BuildSpec -> IO ()
writeBuildSpec spec = do
    buildActExists <- doesFileExist "Build.act"
    if buildActExists
      then do
        content <- readFile "Build.act"
        let jsonDoc = BuildSpec.encodeBuildSpecJSON spec
        case BuildSpec.updateBuildActFromJSON content jsonDoc of
          Left err -> throwProjectError ("Failed to update Build.act: \n" ++ err)
          Right updated -> writeFile "Build.act" updated
      else BL.writeFile "build.act.json" (BuildSpec.encodeBuildSpecJSONPretty spec <> "\n")

lookupRepoUrlFromIndex :: String -> String -> IO String
lookupRepoUrlFromIndex depName pkgNameArg = do
    home <- getHomeDirectory
    let indexPath = home </> ".cache" </> "acton" </> "package-index.json"
    exists <- doesFileExist indexPath
    unless exists $
      throwProjectError ("ERROR: Package index not found at " ++ indexPath ++ "\nHINT: Run: acton pkg update")
    content <- readIndexFile indexPath
    entries <- requireRight (decodePackageIndexRaw content)
    let pkgName = if null pkgNameArg then depName else pkgNameArg
        matchEntry e = rawName e == Just pkgName
    case filter matchEntry entries of
      [] -> throwProjectError ("ERROR: Package " ++ pkgName ++ " not found in package index")
      (entry:_) ->
        case rawRepoUrl entry of
          Just url | not (null url) -> return url
          _ -> throwProjectError ("ERROR: Package " ++ pkgName ++ " in index is missing 'repo_url'")

decodePackageIndex :: BL.ByteString -> Either String [PackageEntry]
decodePackageIndex content = do
    raws <- decodePackageIndexRaw content
    let pkgs = [ PackageEntry n d r
               | PackageEntryRaw (Just n) (Just d) (Just r) <- raws
               ]
    return pkgs

decodePackageIndexRaw :: BL.ByteString -> Either String [PackageEntryRaw]
decodePackageIndexRaw content =
    case Aeson.eitherDecode content of
      Left err -> Left ("ERROR: Failed to parse package index JSON: " ++ err)
      Right (Aeson.Object obj) ->
        case AesonKM.lookup (AesonKey.fromString "packages") obj of
          Just (Aeson.Array arr) -> Right (map parseEntry (toList arr))
          _ -> Left "ERROR: Invalid package index: top-level 'packages' list missing or wrong type"
      Right _ ->
        Left "ERROR: Invalid package index: top-level 'packages' list missing or wrong type"
  where
    parseEntry (Aeson.Object o) =
      PackageEntryRaw
        { rawName = lookupString "name" o
        , rawDescription = lookupString "description" o
        , rawRepoUrl = lookupString "repo_url" o
        }
    parseEntry _ = PackageEntryRaw Nothing Nothing Nothing

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
    let url = "https://api.github.com/repos/" ++ repoOwner info ++ "/" ++ repoName info ++ "/git/refs/heads/" ++ refName
    obj <- fetchGithubObject manager token url
    case obj of
      Left err -> return (Left err)
      Right o ->
        case lookupMessage o of
          Just msg -> return (Left ("Unable to retrieve branch information: " ++ msg))
          Nothing ->
            case AesonKM.lookup (AesonKey.fromString "object") o of
              Just (Aeson.Object obj) ->
                case lookupString "sha" obj of
                  Just shaVal -> return (Right shaVal)
                  Nothing -> return (Left "No SHA")
              _ -> return (Left "No object")

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
    let cmd = proc zigExe ["fetch", "--global-cache-dir", globalCache, depUrl]
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

getZigExe :: IO FilePath
getZigExe = do
    execDir <- takeDirectory <$> getExecutablePath
    sysPath <- canonicalizePath (execDir </> "..")
    return (sysPath </> "zig" </> "zig")

validateDepName :: String -> IO ()
validateDepName name =
    unless (isValidDepName name) $
      throwProjectError ("Invalid dependency name '" ++ name ++ "', must start with a letter and only contain letters, numbers and underscores")

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
