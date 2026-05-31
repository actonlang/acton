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

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Acton.Artifact
  ( ActonOutManifest(..)
  , artifactFormatVersion
  , artifactManifestFile
  , artifactArchiveFile
  , artifactRepositoryName
  , artifactMediaType
  , artifactType
  , currentTargetTuple
  , currentInterfaceVersion
  , currentInterfaceVersionText
  , expectedManifest
  , validateManifest
  , encodeManifest
  , decodeManifest
  , writeManifest
  , readManifest
  , ociTagForSourceHash
  , ociRefForRepository
  , deriveOciRef
  , ociRefWithoutScheme
  , ociRefIsLocal
  , ociRefOrasOptions
  , ociRefOrasTarget
  ) where

import GHC.Generics (Generic)
import Control.Exception (SomeException, displayException, try)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.=))
import qualified Data.Aeson as Ae
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlphaNum)
import Data.List (intercalate, isPrefixOf, isSuffixOf)
import qualified Acton.Syntax as A
import System.FilePath ((</>))

data ActonOutManifest = ActonOutManifest
  { manifestArtifactFormat   :: Int
  , manifestSourceHash       :: String
  , manifestInterfaceVersion :: [Int]
  , manifestTargetTuple      :: String
  , manifestContents         :: [String]
  } deriving (Eq, Show, Generic)

instance FromJSON ActonOutManifest where
  parseJSON = Ae.withObject "ActonOutManifest" $ \o ->
    ActonOutManifest <$> o .: "artifact_format"
                     <*> o .: "source_hash"
                     <*> o .: "interface_version"
                     <*> o .: "target_tuple"
                     <*> o .: "contents"

instance ToJSON ActonOutManifest where
  toJSON m = Ae.object
    [ "artifact_format"   .= manifestArtifactFormat m
    , "source_hash"       .= manifestSourceHash m
    , "interface_version" .= manifestInterfaceVersion m
    , "target_tuple"      .= manifestTargetTuple m
    , "contents"          .= manifestContents m
    ]

artifactFormatVersion :: Int
artifactFormatVersion = 1

artifactManifestFile :: FilePath
artifactManifestFile = "acton-artifact.json"

artifactArchiveFile :: FilePath
artifactArchiveFile = "acton-out.tar.gz"

artifactRepositoryName :: String
artifactRepositoryName = "acton-out"

artifactMediaType :: String
artifactMediaType = "application/vnd.acton.out.v1+tar+gzip"

artifactType :: String
artifactType = "application/vnd.acton.out.v1"

currentTargetTuple :: String
currentTargetTuple = ""

currentInterfaceVersion :: [Int]
currentInterfaceVersion = A.version

currentInterfaceVersionText :: String
currentInterfaceVersionText = intercalate "." (map show currentInterfaceVersion)

expectedManifest :: String -> ActonOutManifest
expectedManifest sourceHash =
  ActonOutManifest
    { manifestArtifactFormat = artifactFormatVersion
    , manifestSourceHash = sourceHash
    , manifestInterfaceVersion = currentInterfaceVersion
    , manifestTargetTuple = currentTargetTuple
    , manifestContents = [artifactManifestFile, "Build.act", "out/types"]
    }

validateManifest :: String -> ActonOutManifest -> Either String ()
validateManifest sourceHash manifest
  | manifestArtifactFormat manifest /= artifactFormatVersion =
      Left ("unsupported artifact format "
            ++ show (manifestArtifactFormat manifest)
            ++ ", expected "
            ++ show artifactFormatVersion)
  | manifestSourceHash manifest /= sourceHash =
      Left ("artifact source hash "
            ++ manifestSourceHash manifest
            ++ " does not match dependency source hash "
            ++ sourceHash)
  | manifestInterfaceVersion manifest /= currentInterfaceVersion =
      Left ("artifact interface version "
            ++ show (manifestInterfaceVersion manifest)
            ++ " does not match compiler interface version "
            ++ show currentInterfaceVersion)
  | manifestTargetTuple manifest /= currentTargetTuple =
      Left ("artifact target tuple "
            ++ show (manifestTargetTuple manifest)
            ++ " does not match expected "
            ++ show currentTargetTuple)
  | otherwise = Right ()

encodeManifest :: ActonOutManifest -> BL.ByteString
encodeManifest = Ae.encode

decodeManifest :: BL.ByteString -> Either String ActonOutManifest
decodeManifest = Ae.eitherDecode'

writeManifest :: FilePath -> ActonOutManifest -> IO ()
writeManifest dir manifest =
  BL.writeFile (dir </> artifactManifestFile) (encodeManifest manifest)

readManifest :: FilePath -> IO (Either String ActonOutManifest)
readManifest dir = do
  bytes <- try (BL.readFile (dir </> artifactManifestFile)) :: IO (Either SomeException BL.ByteString)
  case bytes of
    Left ex -> return (Left (displayException ex))
    Right bs -> return (decodeManifest bs)

ociTagForSourceHash :: String -> Maybe String
ociTagForSourceHash sourceHash =
  let tag = sourceHash ++ "-iface" ++ currentInterfaceVersionText
  in if validOciTag tag then Just tag else Nothing

deriveOciRef :: String -> String -> Maybe String
deriveOciRef repoUrl sourceHash =
  case parseRepoPath repoUrl of
    Just ("github.com", [owner, repo]) ->
      ociRefForRepository ("ghcr.io/" ++ owner ++ "/" ++ repo ++ "/" ++ artifactRepositoryName) sourceHash
    Just ("gitlab.com", parts@(_:_)) ->
      ociRefForRepository ("registry.gitlab.com/" ++ intercalate "/" parts ++ "/" ++ artifactRepositoryName) sourceHash
    _ -> Nothing

ociRefForRepository :: String -> String -> Maybe String
ociRefForRepository repo sourceHash = do
  tag <- ociTagForSourceHash sourceHash
  case localOciLayoutPath repo of
    Just path ->
      let path' = dropTrailingSlash path
      in if null path'
           then Nothing
           else Just ("oci-layout://" ++ path' ++ ":" ++ tag)
    Nothing ->
      let repo' = dropTrailingSlash (ociRefWithoutScheme repo)
      in if null repo' || taggedRepository repo'
           then Nothing
           else Just ("oci://" ++ repo' ++ ":" ++ tag)

ociRefWithoutScheme :: String -> String
ociRefWithoutScheme ref
  | "oci-layout://" `isPrefixOf` ref = drop (length ("oci-layout://" :: String)) ref
  | "oci://" `isPrefixOf` ref = drop (length ("oci://" :: String)) ref
  | otherwise = ref

ociRefIsLocal :: String -> Bool
ociRefIsLocal ref = "oci-layout://" `isPrefixOf` ref

ociRefOrasOptions :: String -> [String]
ociRefOrasOptions ref
  | ociRefIsLocal ref = ["--oci-layout"]
  | otherwise = []

ociRefOrasTarget :: String -> String
ociRefOrasTarget = ociRefWithoutScheme

localOciLayoutPath :: String -> Maybe FilePath
localOciLayoutPath repo
  | "oci-layout://" `isPrefixOf` repo =
      Just (drop (length ("oci-layout://" :: String)) repo)
  | "/" `isPrefixOf` repo = Just repo
  | "./" `isPrefixOf` repo = Just repo
  | "../" `isPrefixOf` repo = Just repo
  | otherwise = Nothing

taggedRepository :: String -> Bool
taggedRepository ref =
  let lastPart = reverse (takeWhile (/= '/') (reverse ref))
  in ':' `elem` lastPart

validOciTag :: String -> Bool
validOciTag [] = False
validOciTag tag =
  length tag <= 128 && validFirst (head tag) && all validRest tag
  where
    validFirst c = isAlphaNum c || c == '_'
    validRest c = isAlphaNum c || c == '_' || c == '.' || c == '-'

parseRepoPath :: String -> Maybe (String, [String])
parseRepoPath raw =
  let noScheme = dropPrefix "https://" (dropPrefix "http://" raw)
      noFragment = takeWhile (/= '#') noScheme
      (host, rest0) = break (== '/') noFragment
      rest = dropWhile (== '/') rest0
      parts = pathParts (dropGitSuffix (dropTrailingSlash rest))
  in case host of
       "github.com" | length parts >= 2 -> Just (host, take 2 parts)
       "gitlab.com" | length parts >= 2 -> Just (host, parts)
       _ -> Nothing

dropPrefix :: String -> String -> String
dropPrefix prefix s
  | prefix `isPrefixOf` s = drop (length prefix) s
  | otherwise = s

dropGitSuffix :: String -> String
dropGitSuffix s
  | ".git" `isSuffixOf` s = take (length s - 4) s
  | otherwise = s

dropTrailingSlash :: String -> String
dropTrailingSlash s
  | "/" `isSuffixOf` s = dropTrailingSlash (take (length s - 1) s)
  | otherwise = s

pathParts :: String -> [String]
pathParts "" = []
pathParts s =
  let (x, rest) = break (== '/') s
      rest' = dropWhile (== '/') rest
  in if null x then pathParts rest' else x : pathParts rest'
