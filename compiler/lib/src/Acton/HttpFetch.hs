{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Proxy-aware HTTP, shared by every network path in the compiler: dependency
--   archive downloads (via Acton.ArchiveDownload) and the package-command
--   metadata calls (the GitHub API and the package index).
--
--   It owns the single proxy-aware Manager (http-client's proxyEnvironment honours
--   http(s)_proxy and tunnels https via CONNECT), the proxy diagnostics, and two
--   primitives: httpGetBytes (a body into memory, for JSON) and downloadToFile
--   (stream to a local file, for archives). Centralising this means one proxy
--   mechanism and one set of diagnostics everywhere, instead of per-call-site
--   managers and raw http-client error dumps.
module Acton.HttpFetch
  ( newProxyManager
  , httpGetBytes
  , downloadToFile
  , isHttpUrl
  , readProxyEnv
  , proxyEnvReportLines
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, SomeAsyncException, SomeException, displayException, fromException, throwIO, try)
import Data.Char (isSpace, toLower)
import Data.List (intercalate, isPrefixOf)
import Data.Maybe (isJust, listToMaybe)
import System.Directory (getTemporaryDirectory, removeFile)
import System.Environment (getEnvironment)
import System.IO (Handle, hClose, openBinaryTempFile)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header (Header)
import Network.HTTP.Types.Status (statusCode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL

-- | The one proxy-aware Manager. http-client's proxyEnvironment reads
--   http(s)_proxy / no_proxy from the environment and CONNECT-tunnels https.
newProxyManager :: IO HTTP.Manager
newProxyManager =
  HTTP.newManager (HTTP.managerSetProxy (HTTP.proxyEnvironment Nothing) tlsManagerSettings)

-- | Re-throw async exceptions (Ctrl-C, timeout/concurrent-fetch cancellation)
--   instead of swallowing them into a proxy diagnostic. Matches the rethrow-async
--   convention used elsewhere (see InterfaceFiles.tyCacheMiss). Call it first in
--   any handler that catches SomeException from a network call.
rethrowAsync :: SomeException -> IO ()
rethrowAsync ex
  | Just (_ :: SomeAsyncException) <- fromException ex = throwIO ex
  | otherwise                                          = return ()

-- | Is this an http(s) URL (as opposed to file:// or a local path)?
isHttpUrl :: String -> Bool
isHttpUrl url =
  let u = map toLower url
  in "http://" `isPrefixOf` u || "https://" `isPrefixOf` u

-- Retry policy for transient server responses (GitHub rate-limit / 5xx), shared by
-- the metadata and archive-download paths. Connection-level failures are NOT
-- retried -- they fail fast with a proxy diagnostic instead.
retryMaxAttempts :: Int
retryMaxAttempts = 10

retryBaseDelay, retryMaxDelay :: Int
retryBaseDelay = 500000        -- 0.5s
retryMaxDelay  = 120000000     -- 120s

retryableStatus :: Int -> Bool
retryableStatus s = s == 403 || s == 429 || s >= 500

-- | GET a URL into memory, for JSON metadata (GitHub API, package index). Retries
--   GitHub rate-limit / server statuses (403/429/5xx); on a connection-level
--   failure it returns immediately with a clean proxy diagnostic rather than
--   retrying a misconfigured proxy for minutes.
httpGetBytes :: HTTP.Manager -> [Header] -> String -> IO (Either String BL.ByteString)
httpGetBytes manager extraHeaders url = do
    parsed <- try (HTTP.parseRequest url) :: IO (Either SomeException HTTP.Request)
    case parsed of
      Left ex -> return (Left ("Invalid URL: " ++ displayException ex))
      Right req0 -> do
        let req = req0 { HTTP.requestHeaders =
                           ("User-Agent", "acton") : extraHeaders ++ HTTP.requestHeaders req0 }
        go req 1 retryBaseDelay
  where
    go req attempt delay = do
      res <- try (HTTP.httpLbs req manager) :: IO (Either SomeException (HTTP.Response BL.ByteString))
      case res of
        Left ex -> do
          rethrowAsync ex
          env <- readProxyEnv
          return (Left (describeFetchException env ex))
        Right response -> do
          let status = statusCode (HTTP.responseStatus response)
              body   = HTTP.responseBody response
          if status >= 200 && status < 300
            then return (Right body)
            else if retryableStatus status && attempt < retryMaxAttempts
                   then do threadDelay delay
                           go req (attempt + 1) (min retryMaxDelay (delay * 2))
                   else return (Left (httpErrorMessage status body))
    httpErrorMessage status body =
      let raw     = trim (B.unpack (BL.toStrict body))
          clipped = take 200 raw
          suffix  = if length raw > length clipped then "..." else ""
          detail  = if null clipped then "" else ": " ++ clipped ++ suffix
      in "HTTP " ++ show status ++ detail

-- | Stream a URL to a fresh temporary file. Returns the temp path, the response
--   headers, and the first bytes of the body (for callers that sniff content,
--   e.g. archive-type detection). Proxy-aware; a connection failure yields a
--   clean proxy diagnostic.
downloadToFile :: HTTP.Manager -> String -> IO (Either String (FilePath, [Header], BS.ByteString))
downloadToFile manager url = do
  parsed <- try (HTTP.parseRequest url) :: IO (Either SomeException HTTP.Request)
  case parsed of
    Left ex -> return (Left ("Invalid URL: " ++ displayException ex))
    Right req -> go req 1 retryBaseDelay
  where
    go req attempt delay = do
      opened <- try (HTTP.responseOpen req manager) :: IO (Either SomeException (HTTP.Response HTTP.BodyReader))
      case opened of
        Left ex -> do
          rethrowAsync ex
          env <- readProxyEnv
          return (Left (describeFetchException env ex))
        Right response -> do
          let code = statusCode (HTTP.responseStatus response)
          if code >= 200 && code < 300
            then do
              tmpDir <- getTemporaryDirectory
              (tmpPath, tmpHandle) <- openBinaryTempFile tmpDir "acton-fetch"
              streamRes <- try (streamBodyToFile (HTTP.responseBody response) tmpHandle) :: IO (Either SomeException BS.ByteString)
              _ <- try (hClose tmpHandle) :: IO (Either IOException ())
              _ <- try (HTTP.responseClose response) :: IO (Either SomeException ())
              case streamRes of
                Left ex -> do
                  _ <- try (removeFile tmpPath) :: IO (Either IOException ())
                  rethrowAsync ex
                  return (Left ("Unable to save downloaded file: " ++ displayException ex))
                Right sniffBytes ->
                  return (Right (tmpPath, HTTP.responseHeaders response, sniffBytes))
            else do
              _ <- try (HTTP.responseClose response) :: IO (Either SomeException ())
              if retryableStatus code && attempt < retryMaxAttempts
                then do threadDelay delay
                        go req (attempt + 1) (min retryMaxDelay (delay * 2))
                else return (Left ("HTTP error " ++ show code))

streamBodyToFile :: HTTP.BodyReader -> Handle -> IO BS.ByteString
streamBodyToFile bodyReader outHandle =
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

-- Proxy diagnostics -----------------------------------------------------------
-- Reported when a fetch fails so a missing/unexported https_proxy (e.g. under
-- sudo or a non-interactive make/CI shell) is obvious rather than hidden behind a
-- raw http-client request-record dump.

proxyEnvVarNames :: [String]
proxyEnvVarNames =
  [ "http_proxy", "HTTP_PROXY"
  , "https_proxy", "HTTPS_PROXY"
  , "all_proxy", "ALL_PROXY"
  , "no_proxy", "NO_PROXY"
  ]

-- | Snapshot the proxy environment acton actually sees. Reads via getEnvironment
--   (the `environ` array), the SAME source http-client's proxyEnvironment
--   consults -- NOT lookupEnv/getenv, which can disagree on Linux binaries hit by
--   the `environ` copy-relocation bug.
readProxyEnv :: IO [(String, Maybe String)]
readProxyEnv = do
  env <- getEnvironment
  return [ (n, lookup n env) | n <- proxyEnvVarNames ]

formatProxyEnvVar :: (String, Maybe String) -> String
formatProxyEnvVar (n, v) =
  n ++ "=" ++ maybe "(unset)" (\x -> if null x then "(empty)" else maskProxyUrl x) v

-- | Indented "proxy environment acton sees" block for use in messages.
proxyEnvReportLines :: String -> [(String, Maybe String)] -> [String]
proxyEnvReportLines indent env =
  (indent ++ "proxy environment acton sees:")
  : [ indent ++ "  " ++ formatProxyEnvVar kv | kv <- env ]

-- | Mask user:password@ credentials embedded in a proxy URL.
maskProxyUrl :: String -> String
maskProxyUrl url =
  let (scheme, afterScheme) = case findSubstring "://" url of
                                Just i  -> (take (i + 3) url, drop (i + 3) url)
                                Nothing -> ("", url)
      (authority, rest)     = break (== '/') afterScheme
  in case break (== '@') authority of
       (_, '@':hostPort) -> scheme ++ "***@" ++ hostPort ++ rest
       _                 -> url

findSubstring :: String -> String -> Maybe Int
findSubstring needle = go 0
  where go _ []              = Nothing
        go i hay@(_:t)
          | needle `isPrefixOf` hay = Just i
          | otherwise               = go (i + 1) t

-- | The proxy variable http-client consults for a request of this scheme, and its
--   value if set. Protocol-specific (https_proxy for https); ALL_PROXY excluded
--   because http-client does not honor it.
applicableProxy :: Bool -> [(String, Maybe String)] -> Maybe (String, String)
applicableProxy secure env =
  listToMaybe [ (n, v) | n <- names, Just (Just v) <- [lookup n env], not (null v) ]
  where names = if secure then ["https_proxy", "HTTPS_PROXY"] else ["http_proxy", "HTTP_PROXY"]

-- | Turn an http-client fetch exception into a clean, actionable message.
describeFetchException :: [(String, Maybe String)] -> SomeException -> String
describeFetchException env ex =
  case fromException ex :: Maybe HTTP.HttpException of
    Just (HTTP.HttpExceptionRequest req content) ->
      let secure  = HTTP.secure req
          scheme  = if secure then "https" else "http"
          target  = B.unpack (HTTP.host req) ++ ":" ++ show (HTTP.port req)
          vars    = if secure then "https_proxy/HTTPS_PROXY" else "http_proxy/HTTP_PROXY"
          proxyLn = case applicableProxy secure env of
                      Just (n, v) -> "  proxy for this " ++ scheme ++ " request: "
                                     ++ maskProxyUrl v ++ " (from " ++ n ++ ")"
                      Nothing     -> "  proxy for this " ++ scheme ++ " request: none ("
                                     ++ vars ++ " unset)"
      in intercalate "\n"
           ( [ describeFetchContent content
             , "  request: " ++ scheme ++ " to " ++ target
             , proxyLn
             ]
             ++ proxyEnvReportLines "  " env
             ++ proxyFailureHint secure env )
    Just other -> displayException other
    Nothing    -> displayException ex

-- | One-line summary of an http-client failure cause (no Request record dump).
describeFetchContent :: HTTP.HttpExceptionContent -> String
describeFetchContent c = case c of
  HTTP.ConnectionTimeout            -> "connection timed out"
  HTTP.ConnectionFailure e          -> "connection failed: " ++ show e
  HTTP.ResponseTimeout              -> "response timed out"
  HTTP.ProxyConnectException h p st -> "proxy CONNECT to " ++ B.unpack h ++ ":" ++ show p
                                       ++ " failed (status " ++ show (statusCode st) ++ ")"
  HTTP.StatusCodeException res _    -> "unexpected HTTP status "
                                       ++ show (statusCode (HTTP.responseStatus res))
  HTTP.InternalException e          -> "connection failed: " ++ show e
  other                             -> show other

-- | Hint shown when a fetch failed and no proxy variable applies to its scheme.
proxyFailureHint :: Bool -> [(String, Maybe String)] -> [String]
proxyFailureHint secure env
  | isJust (applicableProxy secure env) = []
  | otherwise =
      [ "  hint: no " ++ vars ++ " is set in acton's environment, so this " ++ scheme
      , "        download was attempted directly. If you use a proxy, make sure "
        ++ varLower ++ " is"
      , "        exported in the environment that runs the build. sudo/root shells and"
      , "        many make/CI setups do not inherit the proxy variables from your"
      , "        interactive shell." ]
  where scheme   = if secure then "https" else "http"
        vars     = if secure then "https_proxy/HTTPS_PROXY" else "http_proxy/HTTP_PROXY"
        varLower = if secure then "https_proxy" else "http_proxy"

-- Local string trim (kept here so the module is self-contained).
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace
