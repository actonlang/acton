-- | Archive download: the archive-specific layer on top of Acton.HttpFetch.
--   Downloads a URL to a local file (proxy-aware, via HttpFetch.downloadToFile)
--   and names it with a suffix reflecting the archive type, so `zig fetch` can
--   identify the format. Shared by dependency fetching (Acton.Compile) and the
--   package commands (PkgCommands.zigFetchHash).
module Acton.ArchiveDownload
  ( downloadArchive
  ) where

import Acton.HttpFetch (downloadToFile, newProxyManager)
import Control.Applicative ((<|>))
import Control.Exception (IOException, displayException, try)
import Data.Char (isSpace, toLower)
import Data.List (isSuffixOf)
import Data.Word (Word8)
import System.Directory (removeFile, renameFile)
import Network.HTTP.Types.Header (Header, hContentDisposition, hContentType)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B

-- | Download an http(s) archive URL to a temporary local file named with a
--   suffix that reflects its type. Returns the local file path, or a diagnostic.
downloadArchive :: String -> IO (Either String FilePath)
downloadArchive url = do
  manager <- newProxyManager
  res <- downloadToFile manager url
  case res of
    Left err -> return (Left err)
    Right (tmpPath, headers, sniff) ->
      case archiveSuffix url headers sniff of
        Nothing -> do
          _ <- try (removeFile tmpPath) :: IO (Either IOException ())
          return (Left "Could not determine archive type from URL path, response headers, or file bytes")
        Just suffix -> do
          let finalPath = tmpPath ++ suffix
          moveRes <- try (renameFile tmpPath finalPath) :: IO (Either IOException ())
          case moveRes of
            Left ex -> do
              _ <- try (removeFile tmpPath) :: IO (Either IOException ())
              return (Left ("Unable to finalize downloaded archive path: " ++ displayException ex))
            Right _ -> return (Right finalPath)

-- | Pick an archive suffix from the URL path, then the response headers, then the
--   leading bytes -- in that order of preference.
archiveSuffix :: String -> [Header] -> BS.ByteString -> Maybe String
archiveSuffix url headers sniff =
      archiveSuffixFromPath (takeWhile (/= '?') url)
  <|> archiveSuffixFromContentDisposition (lookup hContentDisposition headers)
  <|> archiveSuffixFromContentType (lookup hContentType headers)
  <|> detectArchiveSuffixFromBytes sniff

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

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace
