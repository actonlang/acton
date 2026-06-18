module Acton.FetchDependencies
  ( fetchDependencies
  , redactUrlForDiagnostics
  , noProxyMatchesHost
  , selectedProxyForDiagnostics
  ) where

import qualified Acton.BuildSpec as BuildSpec
import qualified Acton.CommandLineParser as C
import qualified Acton.Project as Project

import Control.Applicative ((<|>))
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (Exception, IOException, SomeException, bracketOnError, displayException, fromException, try)
import Control.Monad (foldM, forM, forM_, unless, when)
import Data.Char (isSpace, toLower)
import Data.List (foldl', isInfixOf, isPrefixOf, isSuffixOf)
import Data.Maybe (catMaybes, isJust)
import qualified Data.Map as M
import qualified Data.Set as Set
import Data.Word (Word8)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header (hContentDisposition, hContentType)
import Network.HTTP.Types.Status (statusCode)
import System.Clock
import System.Directory
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), joinPath, takeDirectory)
import System.IO (Handle, hClose, openBinaryTempFile)
import System.IO.Temp (createTempDirectory, withSystemTempDirectory)
import System.Process (CreateProcess(cwd), proc, readCreateProcessWithExitCode)
import Text.Printf (printf)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B

fetchDependencies :: C.GlobalOptions -> Project.Paths -> [(String, FilePath)] -> IO ()
fetchDependencies gopts paths depOverrides = do
    if Project.isTmp paths
      then return ()
      else do
        rootSpec0 <- Project.loadBuildSpec (Project.projPath paths)
        rootSpec <- Project.applyDepOverrides (Project.projPath paths) depOverrides rootSpec0
        unless (C.quiet gopts) $
          putStrLn "Resolving dependencies (fetching if missing)..."
        home <- getHomeDirectory
        let zigExe      = joinPath [Project.sysPath paths, "zig", "zig"]
            globalCache = joinPath [home, ".cache", "acton", "zig-global-cache"]
            depsCache   = joinPath [home, ".cache", "acton", "deps"]
            cacheDir h  = joinPath [globalCache, "p", h]
        createDirectoryIfMissing True globalCache
        createDirectoryIfMissing True depsCache
        let rootPins = BuildSpec.dependencies rootSpec
        _ <- walkProject rootPins cacheDir zigExe globalCache depsCache
                         Set.empty (Project.projPath paths) rootSpec
        return ()
  where
    walkProject rootPins cacheDir zigExe globalCache depsCache seen projDir spec = do
      projAbs <- Project.normalizePathSafe projDir
      if Set.member projAbs seen
        then return seen
        else do
          selectedDeps <- forM (M.toList (BuildSpec.dependencies spec)) $
            selectDependency rootPins projAbs
          let pkgFetches = catMaybes
                [ mkPkgFetch cacheDir zigExe globalCache depsCache name dep | (name, dep) <- selectedDeps ]
              zigFetches = catMaybes
                [ mkZigFetch cacheDir zigExe globalCache name dep
                | (name, dep) <- M.toList (BuildSpec.zig_dependencies spec)
                ]
          results <- mapConcurrently id (pkgFetches ++ zigFetches)
          let errs = [ e | Left e <- results ]
          unless (null errs) $ Project.throwProjectError (unlines errs)
          forM_ selectedDeps $ \(name, dep) -> copyPkgDep cacheDir depsCache name dep
          let seen' = Set.insert projAbs seen
          foldM (walkDependency rootPins cacheDir zigExe globalCache depsCache projAbs)
                seen' selectedDeps

    selectDependency rootPins base (depName, dep) = do
      let (chosenDep, conflict) =
            case M.lookup depName rootPins of
              Nothing -> (dep, Nothing)
              Just pinDep ->
                if pinDep == dep
                  then (dep, Nothing)
                  else (pinDep, Just dep)
      when (isJust conflict) $
        unless (C.quiet gopts) $
          putStrLn ("Warning: dependency '" ++ depName ++ "' in " ++ base
                    ++ " overridden by root pin")
      return (depName, chosenDep)

    walkDependency rootPins cacheDir zigExe globalCache depsCache base seen (depName, dep) = do
      depBase <- Project.resolveDepBase base depName dep
      depAbs <- Project.normalizePathSafe depBase
      depExists <- doesDirectoryExist depAbs
      if not depExists
        then case BuildSpec.path dep of
               Just p | not (null p) ->
                 Project.throwProjectError
                   ("Dependency " ++ depName ++ " path does not exist: " ++ depAbs ++ "\n"
                    ++ "Hint: Local dependency paths must point to an Acton project root\n"
                    ++ "(directory with src/ and Build.act).")
               _ -> return seen
        else do
          depSpec0 <- Project.loadBuildSpec depAbs
          depSpec <- Project.applyDepOverrides depAbs depOverrides depSpec0
          walkProject rootPins cacheDir zigExe globalCache depsCache seen depAbs depSpec

    mkPkgFetch cacheDir zigExe globalCache depsCache name dep =
      case BuildSpec.path dep of
        Just p | not (null p) -> Nothing
        _ -> case (BuildSpec.url dep, BuildSpec.hash dep) of
               (Just u, Just h) ->
                 Just (fetchOne "pkg" name u (Just h) cacheDir zigExe globalCache (Just depsCache))
               (Just _, Nothing) ->
                 Just (return (Left ("Dependency " ++ name ++ " is missing hash")))
               _ -> Nothing

    mkZigFetch cacheDir zigExe globalCache name dep =
      case BuildSpec.zpath dep of
        Just p | not (null p) -> Nothing
        _ -> case (BuildSpec.zurl dep, BuildSpec.zhash dep) of
               (Just u, Just h) ->
                 Just (fetchOne "zig" name u (Just h) cacheDir zigExe globalCache Nothing)
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
                     dst = joinPath [depsCache, name ++ "-" ++ h]
                 exists <- doesDirectoryExist dst
                 unless exists $ do
                   srcOk <- doesDirectoryExist src
                   archiveOk <- doesFileExist srcArchive
                   unless (srcOk || archiveOk) $
                     Project.throwProjectError ("Dependency " ++ name ++ " not present in Zig cache after fetch: " ++ src)
                   when (C.verbose gopts) $
                     putStrLn ("Copying dependency " ++ name ++ " (" ++ h ++ ") from Zig cache")
                   if srcOk
                     then copyTree src dst
                     else extractCachedArchive srcArchive dst

    fetchOne kind name url mh cacheDir zigExe globalCache mDepsCache = do
      case mh of
        Just h -> do
          present <- cacheEntryExists cacheDir h
          if present
            then do
              unless (C.quiet gopts) $
                putStrLn ("Using cached " ++ kind ++ " dependency " ++ name ++ " (" ++ h ++ ")")
              return (Right h)
            else runFetch kind name url mh cacheDir zigExe globalCache mDepsCache
        Nothing ->
          runFetch kind name url mh cacheDir zigExe globalCache mDepsCache

    runFetch kind name url mh cacheDir zigExe globalCache mDepsCache = do
      unless (C.quiet gopts) $
        putStrLn ("Fetching " ++ kind ++ " dependency " ++ name ++ " from " ++ url)
      if isHttpUrl url
        then fetchViaDownloadedArchive kind name url mh cacheDir zigExe globalCache mDepsCache
        else do -- other URLs, like file:// - not very common, maybe we want to constrain this somehow?
          res <- runZigFetch zigExe globalCache url
          case res of
            Left ex -> do
              msg <- renderFetchFailure kind name url mh cacheDir mDepsCache
                         "zig-fallback-failed" []
                         (renderZigDiagnostics "Zig fetch" zigExe globalCache url Nothing
                            ("failed to start: " ++ displayException ex) Nothing)
              return (Left msg)
            Right (ExitSuccess, out, _) -> do
              validated <- validateFetchOutput name mh cacheDir out
              case validated of
                Right h -> return (Right h)
                Left err -> do
                  msg <- renderFetchFailure kind name url mh cacheDir mDepsCache
                           (classifyValidationError err) []
                           (renderZigDiagnostics "Zig fetch" zigExe globalCache url (Just ExitSuccess)
                              err (Just out))
                  return (Left msg)
            Right (code@(ExitFailure _), _, err) -> do
              msg <- renderFetchFailure kind name url mh cacheDir mDepsCache
                       "zig-fallback-failed" []
                       (renderZigDiagnostics "Zig fetch" zigExe globalCache url (Just code)
                          err Nothing)
              return (Left msg)

    runZigFetch :: FilePath -> FilePath -> FilePath -> IO (Either SomeException (ExitCode, String, String))
    runZigFetch zigExe globalCache target = do
      createDirectoryIfMissing True (globalCache </> "tmp")
      withSystemTempDirectory "acton-zig-fetch" $ \tmp -> do
        writeFile (tmp </> "build.zig") zigFetchBuildZig
        let cmd = (proc zigExe ["fetch", "--global-cache-dir", globalCache, target]) { cwd = Just tmp }
        try (readCreateProcessWithExitCode cmd "") :: IO (Either SomeException (ExitCode, String, String))

    zigFetchBuildZig :: String
    zigFetchBuildZig = unlines
      [ "const std = @import(\"std\");"
      , "pub fn build(b: *std.Build) void { _ = b; }"
      ]

    validateFetchOutput :: String -> Maybe String -> (String -> FilePath) -> String -> IO (Either String String)
    validateFetchOutput name mh cacheDir out = do
      let hashVal = trim out
      case mh of
        Just h | h /= hashVal ->
          return (Left ("Hash mismatch for dependency " ++ name ++ " (expected " ++ h ++ ", got " ++ hashVal ++ ")"))
        _ -> do
          exists <- cacheEntryExists cacheDir hashVal
          if exists
            then return (Right hashVal)
            else return (Left ("Dependency " ++ name ++ " not present in Zig cache after fetch: " ++ cacheDir hashVal))

    classifyValidationError err
      | "Hash mismatch" `isPrefixOf` err = "hash-mismatch"
      | "not present in Zig cache" `isInfixOf` err = "cache-missing"
      | otherwise = "fetch-validation-failed"

    renderFetchFailure kind name url mh cacheDir mDepsCache classification httpLines zigLines = do
      cacheLines <- renderCacheDiagnostics name mh cacheDir mDepsCache
      return $ unlines $
        [ "Failed to fetch dependency " ++ name
        , ""
        , "Fetch diagnostics:"
        , "  dependency: " ++ name ++ " (" ++ kind ++ ")"
        , "  url: " ++ redactUrlForDiagnostics url
        , "  expected hash: " ++ maybe "unknown" id mh
        , "  classification: " ++ classification
        , ""
        ] ++ httpLines ++ cacheLines ++ zigLines

    renderCacheDiagnostics name mh cacheDir mDepsCache =
      case mh of
        Nothing ->
          return [ ""
                 , "  cache:"
                 , "    expected hash: unknown"
                 ]
        Just h -> do
          dirOk <- doesDirectoryExist (cacheDir h)
          archiveOk <- doesFileExist (cacheArchivePath cacheDir h)
          depState <- case mDepsCache of
                        Nothing -> return []
                        Just depsCache -> do
                          let depPath = joinPath [depsCache, name ++ "-" ++ h]
                          depOk <- doesDirectoryExist depPath
                          return ["    dependency cache: " ++ depPath ++ " (" ++ presentMissing depOk ++ ")"]
          return $
            [ ""
            , "  cache:"
            , "    zig package cache: " ++ cacheDir h ++ " (" ++ presentMissing (dirOk || archiveOk) ++ ")"
            , "    zig package archive: " ++ cacheArchivePath cacheDir h ++ " (" ++ presentMissing archiveOk ++ ")"
            ] ++ depState

    presentMissing True  = "present"
    presentMissing False = "missing"

    renderZigDiagnostics title zigExe globalCache target mCode err mOut =
      [ ""
      , "  " ++ title ++ ":"
      , "    result: " ++ zigResult
      , "    command: " ++ zigExe ++ " fetch --global-cache-dir " ++ globalCache ++ " " ++ redactUrlForDiagnostics target
      ] ++ codeLines ++ noteLines ++ errLines ++ outLines
      where
        zigResult =
          case mCode of
            Just ExitSuccess     -> "validation-failed"
            Just (ExitFailure _) -> "failed"
            Nothing              -> "failed"
        codeLines =
          case mCode of
            Nothing -> []
            Just code -> ["    exit code: " ++ show code]
        noteLines =
          if title == "Zig fallback"
            then ["    note: fallback uses Zig's network stack, not Acton's Haskell proxy selection"]
            else []
        errLines =
          if null (trim err)
            then []
            else ["    stderr: " ++ clipDiagnostic 800 (sanitizeDiagnosticText target err)]
        outLines =
          case mOut of
            Nothing -> []
            Just out | null (trim out) -> []
            Just out -> ["    stdout: " ++ clipDiagnostic 400 (sanitizeDiagnosticText target out)]

    cacheEntryExists :: (String -> FilePath) -> String -> IO Bool
    cacheEntryExists cacheDir hashVal = do
      dirExists <- doesDirectoryExist (cacheDir hashVal)
      archiveExists <- doesFileExist (cacheArchivePath cacheDir hashVal)
      return (dirExists || archiveExists)

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
            Project.throwProjectError ("Failed to extract cached dependency archive " ++ archive ++ ":\n" ++ out ++ err)

    fetchViaDownloadedArchive kind name depUrl mh cacheDir zigExe globalCache mDepsCache = do
      dl <- downloadToLocalArchive kind name depUrl
      case dl of
        Left (dlClass, dlLines) -> do
          direct <- runZigFetch zigExe globalCache depUrl
          case direct of
            Left ex -> do
              msg <- renderFetchFailure kind name depUrl mh cacheDir mDepsCache dlClass dlLines
                       (renderZigDiagnostics "Zig fallback" zigExe globalCache depUrl Nothing
                          ("failed to start: " ++ displayException ex) Nothing)
              return (Left msg)
            Right (ExitSuccess, out, _) -> do
              validated <- validateFetchOutput name mh cacheDir out
              case validated of
                Right h -> return (Right h)
                Left err -> do
                  msg <- renderFetchFailure kind name depUrl mh cacheDir mDepsCache
                           (classifyValidationError err) dlLines
                           (renderZigDiagnostics "Zig fallback" zigExe globalCache depUrl (Just ExitSuccess)
                              err (Just out))
                  return (Left msg)
            Right (code@(ExitFailure _), _, err) -> do
              msg <- renderFetchFailure kind name depUrl mh cacheDir mDepsCache dlClass dlLines
                       (renderZigDiagnostics "Zig fallback" zigExe globalCache depUrl (Just code)
                          err Nothing)
              return (Left msg)
        Right localArchive -> do
          fetched <- runZigFetch zigExe globalCache localArchive
          _ <- try (removeFile localArchive) :: IO (Either IOException ())
          case fetched of
            Left ex -> do
              msg <- renderFetchFailure kind name depUrl mh cacheDir mDepsCache
                       "zig-fallback-failed" []
                       (renderZigDiagnostics "Zig local archive" zigExe globalCache localArchive Nothing
                          ("failed to start: " ++ displayException ex) Nothing)
              return (Left msg)
            Right (ExitSuccess, out, _) -> do
              validated <- validateFetchOutput name mh cacheDir out
              case validated of
                Right h -> return (Right h)
                Left err -> do
                  msg <- renderFetchFailure kind name depUrl mh cacheDir mDepsCache
                           (classifyValidationError err) []
                           (renderZigDiagnostics "Zig local archive" zigExe globalCache localArchive (Just ExitSuccess)
                              err (Just out))
                  return (Left msg)
            Right (code@(ExitFailure _), _, err) -> do
              msg <- renderFetchFailure kind name depUrl mh cacheDir mDepsCache
                       "zig-fallback-failed" []
                       (renderZigDiagnostics "Zig local archive" zigExe globalCache localArchive (Just code)
                          err Nothing)
              return (Left msg)

    isHttpUrl :: String -> Bool
    isHttpUrl depUrl =
      let u = map toLower depUrl
      in "http://" `isPrefixOf` u || "https://" `isPrefixOf` u

    downloadToLocalArchive :: String -> String -> String -> IO (Either (String, [String]) FilePath)
    downloadToLocalArchive _kind _name depUrl = do
      env <- getEnvironment
      started <- getTime Monotonic
      parsedReq <- try (HTTP.parseRequest depUrl) :: IO (Either SomeException HTTP.Request)
      case parsedReq of
        Left ex ->
          downloadFailure env Nothing started "invalid-url" (exceptionSummaryForDiagnostics depUrl ex)
        Right req -> do
          let settings = HTTP.managerSetProxy (HTTP.proxyEnvironment Nothing) tlsManagerSettings
          manager <- HTTP.newManager settings
          opened <- try (HTTP.responseOpen req manager) :: IO (Either SomeException (HTTP.Response HTTP.BodyReader))
          case opened of
            Left ex -> do
              downloadFailure env (Just req) started (classifyFetchException ex)
                (exceptionSummaryForDiagnostics depUrl ex)
            Right response -> do
              let code = statusCode (HTTP.responseStatus response)
              if code < 200 || code >= 300
                then do
                  _ <- try (HTTP.responseClose response) :: IO (Either SomeException ())
                  downloadFailure env (Just req) started "http-status" ("HTTP error " ++ show code)
                else do
                  tmpDir <- getTemporaryDirectory
                  (tmpPath, tmpHandle) <- openBinaryTempFile tmpDir "acton-fetch"
                  let initialSuffix = archiveSuffixForRequestResponse req response
                  streamRes <- try (streamDownloadToFile (HTTP.responseBody response) tmpHandle) :: IO (Either SomeException BS.ByteString)
                  _ <- try (hClose tmpHandle) :: IO (Either IOException ())
                  _ <- try (HTTP.responseClose response) :: IO (Either SomeException ())
                  case streamRes of
                    Left ex -> do
                      _ <- try (removeFile tmpPath) :: IO (Either IOException ())
                      downloadFailure env (Just req) started "archive-save-failed"
                        ("Unable to save downloaded archive: " ++ exceptionSummaryForDiagnostics depUrl ex)
                    Right sniffBytes ->
                      case initialSuffix <|> detectArchiveSuffixFromBytes sniffBytes of
                        Nothing -> do
                          _ <- try (removeFile tmpPath) :: IO (Either IOException ())
                          downloadFailure env (Just req) started "archive-type-unknown"
                            "Could not determine archive type from URL path, response headers, or file bytes"
                        Just suffix -> do
                          let finalPath = tmpPath ++ suffix
                          moveRes <- try (renameFile tmpPath finalPath) :: IO (Either IOException ())
                          case moveRes of
                            Left ex -> do
                              _ <- try (removeFile tmpPath) :: IO (Either IOException ())
                              downloadFailure env (Just req) started "archive-save-failed"
                                ("Unable to finalize downloaded archive path: " ++ exceptionSummaryForDiagnostics depUrl ex)
                            Right _ ->
                              return (Right finalPath)

    downloadFailure env mReq started result err = do
      finished <- getTime Monotonic
      return (Left (result, renderHttpDownloadDiagnostics env mReq result err (finished - started)))

    streamDownloadToFile :: HTTP.BodyReader -> Handle -> IO BS.ByteString
    streamDownloadToFile bodyReader outHandle =
      loop BS.empty
      where
        sniffLimit = 600
        loop sniff = do
          chunk <- HTTP.brRead bodyReader
          if BS.null chunk
            then return sniff
            else do
              BS.hPut outHandle chunk
              let sniff' =
                    if BS.length sniff >= sniffLimit
                      then sniff
                      else BS.take sniffLimit (sniff <> chunk)
              loop sniff'

    archiveSuffixForRequestResponse :: HTTP.Request -> HTTP.Response HTTP.BodyReader -> Maybe String
    archiveSuffixForRequestResponse req response =
      case archiveSuffixFromPath (B.unpack (HTTP.path req)) of
        Just suffix -> Just suffix
        Nothing ->
          case archiveSuffixFromContentDisposition (lookup hContentDisposition (HTTP.responseHeaders response)) of
            Just suffix -> Just suffix
            Nothing -> archiveSuffixFromContentType (lookup hContentType (HTTP.responseHeaders response))

    archiveSuffixFromPath :: String -> Maybe String
    archiveSuffixFromPath rawPath =
      let p = map toLower rawPath
      in if ".tar.gz" `isSuffixOf` p then Just ".tar.gz"
         else if ".tgz" `isSuffixOf` p then Just ".tgz"
         else if ".tar.xz" `isSuffixOf` p then Just ".tar.xz"
         else if ".txz" `isSuffixOf` p then Just ".txz"
         else if ".tar.zst" `isSuffixOf` p then Just ".tar.zst"
         else if ".tzst" `isSuffixOf` p then Just ".tzst"
         else if ".tar" `isSuffixOf` p then Just ".tar"
         else if ".zip" `isSuffixOf` p then Just ".zip"
         else if ".jar" `isSuffixOf` p then Just ".jar"
         else Nothing

    archiveSuffixFromContentType :: Maybe B.ByteString -> Maybe String
    archiveSuffixFromContentType mType =
      case map toLower . trim . takeWhile (/= ';') . B.unpack <$> mType of
        Just "application/x-tar" -> Just ".tar"
        Just "application/gzip" -> Just ".tar.gz"
        Just "application/x-gzip" -> Just ".tar.gz"
        Just "application/tar+gzip" -> Just ".tar.gz"
        Just "application/x-tar-gz" -> Just ".tar.gz"
        Just "application/x-gtar-compressed" -> Just ".tar.gz"
        Just "application/x-xz" -> Just ".tar.xz"
        Just "application/zstd" -> Just ".tar.zst"
        Just "application/zip" -> Just ".zip"
        Just "application/x-zip-compressed" -> Just ".zip"
        Just "application/java-archive" -> Just ".zip"
        _ -> Nothing

    archiveSuffixFromContentDisposition :: Maybe B.ByteString -> Maybe String
    archiveSuffixFromContentDisposition mVal = do
      headerVal <- mVal
      filenameVal <- extractField (B.pack "filename*=") headerVal <|> extractField (B.pack "filename=") headerVal
      archiveSuffixFromPath (B.unpack filenameVal)
      where
        extractField key raw =
          let lowerRaw = B.map toLower raw
              (prefix, rest) = B.breakSubstring key lowerRaw
          in if B.null rest
               then Nothing
               else
                 let origRest = B.drop (B.length prefix) raw
                     val0 = B.drop (B.length key) origRest
                     val1 = B.takeWhile (/= ';') val0
                     val2 = stripQuotes (B.dropWhile isSpace (trimBS val1))
                     val3 =
                       case B.breakSubstring (B.pack "''") val2 of
                         (_, t) | B.null t -> val2
                         (_, t) -> B.drop 2 t
                 in if B.null val3 then Nothing else Just val3
        trimBS = B.dropWhileEnd isSpace . B.dropWhile isSpace
        stripQuotes s
          | B.length s >= 2 && B.head s == '"' && B.last s == '"' = B.tail (B.init s)
          | otherwise = s

    detectArchiveSuffixFromBytes :: BS.ByteString -> Maybe String
    detectArchiveSuffixFromBytes bytes
      | startsWith [0x50, 0x4b, 0x03, 0x04] bytes = Just ".zip"
      | startsWith [0x50, 0x4b, 0x05, 0x06] bytes = Just ".zip"
      | startsWith [0x50, 0x4b, 0x07, 0x08] bytes = Just ".zip"
      | startsWith [0x1f, 0x8b] bytes = Just ".tar.gz"
      | startsWith [0xfd, 0x37, 0x7a, 0x58, 0x5a, 0x00] bytes = Just ".tar.xz"
      | startsWith [0x28, 0xb5, 0x2f, 0xfd] bytes = Just ".tar.zst"
      | hasUstar bytes = Just ".tar"
      | otherwise = Nothing
      where
        startsWith :: [Word8] -> BS.ByteString -> Bool
        startsWith sig bs =
          BS.length bs >= length sig && BS.take (length sig) bs == BS.pack sig
        hasUstar bs =
          BS.length bs >= 262 && BS.take 5 (BS.drop 257 bs) == BS.pack [0x75, 0x73, 0x74, 0x61, 0x72]

    copyTree :: FilePath -> FilePath -> IO ()
    copyTree src dst = do
      exists <- doesDirectoryExist src
      unless exists $ Project.throwProjectError ("Source path for copyTree does not exist: " ++ src)
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

redactUrlForDiagnostics :: String -> String
redactUrlForDiagnostics =
    redactUserInfo . stripUrlSecretSuffix

selectedProxyForDiagnostics :: [(String, String)] -> Bool -> String -> Maybe String
selectedProxyForDiagnostics env secureReq host
  | noProxyMatchesEnv env host = Nothing
  | otherwise                  = lookupFirst proxyNames
  where
    proxyNames
      | secureReq  = ["https_proxy", "HTTPS_PROXY", "all_proxy", "ALL_PROXY"]
      | otherwise  = ["http_proxy", "HTTP_PROXY", "all_proxy", "ALL_PROXY"]
    lookupFirst []       = Nothing
    lookupFirst (n : ns) =
      case lookup n env of
        Just v | not (null (trim v)) -> Just v
        _                            -> lookupFirst ns

noProxyMatchesHost :: String -> String -> Bool
noProxyMatchesHost raw host =
    any (matches . normalizeNoProxyToken) (splitOnChar ',' raw)
  where
    h = map toLower (stripHostPort (trim host))
    matches tok
      | null tok             = False
      | tok == "*"           = True
      | "." `isPrefixOf` tok =
          let suffix = dropWhile (== '.') tok
          in h == suffix || ("." ++ suffix) `isSuffixOf` h
      | otherwise            = h == tok || ("." ++ tok) `isSuffixOf` h

stripUrlSecretSuffix :: String -> String
stripUrlSecretSuffix = takeWhile (\c -> c /= '?' && c /= '#')

redactUserInfo :: String -> String
redactUserInfo raw =
    case breakString "://" raw of
      Just (scheme, rest) -> scheme ++ "://" ++ redactAuthority rest
      Nothing             -> redactAuthority raw
  where
    redactAuthority s =
      let (authority, suffix) = break (== '/') s
      in case breakLastChar '@' authority of
           Nothing             -> s
           Just (userinfo, hst) -> redactUserInfoPart userinfo ++ "@" ++ hst ++ suffix
    redactUserInfoPart userinfo =
      case break (== ':') userinfo of
        (user, ':' : _) | not (null user) -> user ++ ":***"
        _                                -> "***"

breakString :: String -> String -> Maybe (String, String)
breakString needle haystack =
    go "" haystack
  where
    go _ [] = Nothing
    go revPrefix rest
      | needle `isPrefixOf` rest = Just (reverse revPrefix, drop (length needle) rest)
      | otherwise =
          case rest of
            (c : cs) -> go (c : revPrefix) cs

breakLastChar :: Char -> String -> Maybe (String, String)
breakLastChar needle s =
    case break (== needle) (reverse s) of
      (_, [])             -> Nothing
      (revAfter, _ : revBefore) -> Just (reverse revBefore, reverse revAfter)

splitOnChar :: Char -> String -> [String]
splitOnChar _ [] = [""]
splitOnChar ch s =
    case break (== ch) s of
      (chunk, [])       -> [chunk]
      (chunk, _ : rest) -> chunk : splitOnChar ch rest

normalizeNoProxyToken :: String -> String
normalizeNoProxyToken =
    map toLower . stripHostPort . trim

stripHostPort :: String -> String
stripHostPort s
  | "[" `isPrefixOf` s =
      case break (== ']') s of
        ('[' : host, ']' : _) -> host
        _                    -> s
  | countChar ':' s == 1 =
      takeWhile (/= ':') s
  | otherwise = s

countChar :: Char -> String -> Int
countChar c = length . filter (== c)

noProxyMatchesEnv :: [(String, String)] -> String -> Bool
noProxyMatchesEnv env host =
    any (`noProxyMatchesHost` host) vals
  where
    vals = [ v | n <- ["no_proxy", "NO_PROXY"], Just v <- [lookup n env] ]

renderProxyEnvironmentDiagnostics :: [(String, String)] -> Maybe HTTP.Request -> [String]
renderProxyEnvironmentDiagnostics env mReq =
    "  proxy environment:" :
    map renderEnvVar proxyEnvNames ++ renderSelection
  where
    proxyEnvNames = ["http_proxy", "https_proxy", "all_proxy", "HTTP_PROXY", "HTTPS_PROXY", "ALL_PROXY", "no_proxy", "NO_PROXY"]
    renderEnvVar name =
      "    " ++ name ++ "=" ++
        case lookup name env of
          Nothing -> "unset"
          Just "" -> "(empty)"
          Just v
            | name == "no_proxy" || name == "NO_PROXY" -> v
            | otherwise                                -> redactUrlForDiagnostics v
    renderSelection =
      case mReq of
        Nothing -> [ "    selected proxy: unknown"
                   , "    no_proxy matched target: unknown"
                   ]
        Just req ->
          let host = B.unpack (HTTP.host req)
              selected = selectedProxyForDiagnostics env (HTTP.secure req) host
              matched = noProxyMatchesEnv env host
          in [ "    selected proxy: " ++ maybe "direct" redactUrlForDiagnostics selected
             , "    no_proxy matched " ++ host ++ ": " ++ yesNo matched
             ]

renderHttpDownloadDiagnostics :: [(String, String)] -> Maybe HTTP.Request -> String -> String -> TimeSpec -> [String]
renderHttpDownloadDiagnostics env mReq result err elapsed =
    renderProxyEnvironmentDiagnostics env mReq ++
    [ ""
    , "  HTTP download:"
    , "    result: " ++ result
    , "    request: " ++ maybe "unknown" requestLineForDiagnostics mReq
    , "    elapsed: " ++ trim (fmtTime elapsed)
    , "    error: " ++ clipDiagnostic 600 err
    ]

requestLineForDiagnostics :: HTTP.Request -> String
requestLineForDiagnostics req =
    B.unpack (HTTP.method req) ++ " " ++ B.unpack (HTTP.host req) ++ ":" ++ show (HTTP.port req)
    ++ " " ++ clipDiagnostic 160 (B.unpack (HTTP.path req))

classifyFetchException :: SomeException -> String
classifyFetchException ex =
    case fromException ex of
      Just (HTTP.InvalidUrlException _ _) ->
        "invalid-url"
      Just (HTTP.HttpExceptionRequest _ content) ->
        classifyHttpExceptionContent content
      Nothing ->
        classifyFetchExceptionText (displayException ex)

classifyHttpExceptionContent :: HTTP.HttpExceptionContent -> String
classifyHttpExceptionContent content =
  case content of
    HTTP.ConnectionTimeout        -> "connection-timeout"
    HTTP.ResponseTimeout          -> "connection-timeout"
    HTTP.ConnectionFailure _      -> "connection-failed"
    HTTP.InternalException inner  -> classifyFetchExceptionText (displayException inner)
    HTTP.StatusCodeException _ _  -> "http-status"
    HTTP.ProxyConnectException {} -> "proxy-connect-failed"
    _                             -> classifyFetchExceptionText (show content)

classifyFetchExceptionText :: String -> String
classifyFetchExceptionText msg
  | "ConnectionTimeout" `isInfixOf` msg   = "connection-timeout"
  | "ConnectionFailure" `isInfixOf` msg   = "connection-failed"
  | "HostCannotConnect" `isInfixOf` msg   = "connection-failed"
  | "ResponseTimeout" `isInfixOf` msg     = "connection-timeout"
  | "HandshakeFailed" `isInfixOf` msg     = "tls-certificate-error"
  | "certificate" `isInfixOf` lowerMsg    = "tls-certificate-error"
  | "InvalidUrlException" `isInfixOf` msg = "invalid-url"
  | otherwise                             = "download-failed"
  where
    lowerMsg = map toLower msg

exceptionSummaryForDiagnostics :: Exception e => String -> e -> String
exceptionSummaryForDiagnostics depUrl ex =
    let msg = displayException ex
    in if "ConnectionTimeout" `isInfixOf` msg
         then "ConnectionTimeout"
         else clipDiagnostic 600 (sanitizeDiagnosticText depUrl msg)

sanitizeDiagnosticText :: String -> String -> String
sanitizeDiagnosticText depUrl txt =
    foldl' redactSecret (replaceAll depUrl (redactUrlForDiagnostics depUrl) txt) secretParts
  where
    noFragment = takeWhile (/= '#') depUrl
    query =
      case break (== '?') noFragment of
        (_, '?' : q) -> q
        _            -> ""
    fragment =
      case break (== '#') depUrl of
        (_, '#' : f) -> f
        _            -> ""
    secretParts =
      filter (not . null)
        [ query
        , if null query then "" else "?" ++ query
        , fragment
        , if null fragment then "" else "#" ++ fragment
        ]
    redactSecret acc secret = replaceAll secret "[redacted]" acc

replaceAll :: String -> String -> String -> String
replaceAll needle replacement haystack
  | null needle = haystack
  | otherwise   = go haystack
  where
    go [] = []
    go s
      | needle `isPrefixOf` s = replacement ++ go (drop (length needle) s)
      | otherwise =
          case s of
            (c : cs) -> c : go cs

clipDiagnostic :: Int -> String -> String
clipDiagnostic limit txt =
    clipped ++ suffix
  where
    raw = trim txt
    clipped = take limit raw
    suffix = if length raw > limit then "..." else ""

yesNo :: Bool -> String
yesNo True  = "yes"
yesNo False = "no"

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

fmtTime :: TimeSpec -> String
fmtTime t =
    printf "%6.3f s" secs
  where
    secs :: Float
    secs = (fromIntegral (sec t)) + (fromIntegral (nsec t) / 1000000000)
