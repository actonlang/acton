module Acton.Testing
  ( TestResult(..)
  , TestRunContext(..)
  , TestCachedResult(..)
  , TestCacheEntry(..)
  , TestCache(..)
  , TestHashInfo(..)
  , testCacheVersion
  , cachedResultFromTest
  , testResultFromCache
  , testCachePath
  , readTestCache
  , writeTestCache
  , contextHashBytes
  , hashRun
  , hashDeps
  , formatTestCacheContext
  , formatTestCacheLog
  , buildTestHashInfos
  , classifyCachedTests
  , updateTestCacheEntry
  , mkTestKey
  , testNameCandidates
  , lookupTestInfo
  , readModuleNameHashes
  , readModuleNameHashesByModName
  , readModuleNameHashesCached
  , resolveDepImplHash
  , resolveTestImplDeps
  , buildTestHashInfo
  ) where

import qualified Acton.Compile as Compile
import qualified Acton.Env
import qualified Acton.Hashing as Hashing
import qualified Acton.Syntax as A
import qualified InterfaceFiles
import Utils (prstr)

import Control.Exception (SomeException, try)
import Control.Monad (forM)
import Data.Binary (encode)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as BL
import Data.Either (partitionEithers)
import Data.IORef
import Data.List (intercalate, isPrefixOf, isSuffixOf, nub)
import qualified Data.List
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import qualified Data.Map as M
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import System.FilePath ((</>), takeDirectory)


data TestResult = TestResult
  { trModule       :: String
  , trName         :: String
  , trComplete     :: Bool
  , trSuccess      :: Maybe Bool
  , trSkipped      :: Bool
  , trSkipReason   :: Maybe String
  , trException    :: Maybe String
  , trOutput       :: Maybe String
  , trStdOut       :: Maybe String
  , trStdErr       :: Maybe String
  , trFlaky        :: Bool
  , trNumFailures  :: Int
  , trNumErrors    :: Int
  , trNumIterations :: Int
  , trTestDuration :: Double
  , trRaw          :: Aeson.Value
  , trSnapshotUpdated :: Bool
  , trCached       :: Bool
  } deriving (Show)

testCacheVersion :: Int
testCacheVersion = 2

data TestRunContext = TestRunContext
  { trcCompilerVersion :: String
  , trcTarget          :: String
  , trcOptimize        :: String
  , trcMode            :: String
  , trcArgs            :: [String]
  } deriving (Show, Eq)

instance Aeson.ToJSON TestRunContext where
  toJSON ctx = Aeson.object
    [ AesonKey.fromString "compilerVersion" Aeson..= trcCompilerVersion ctx
    , AesonKey.fromString "target" Aeson..= trcTarget ctx
    , AesonKey.fromString "optimize" Aeson..= trcOptimize ctx
    , AesonKey.fromString "mode" Aeson..= trcMode ctx
    , AesonKey.fromString "args" Aeson..= trcArgs ctx
    ]

instance Aeson.FromJSON TestRunContext where
  parseJSON = Aeson.withObject "TestRunContext" $ \o ->
    TestRunContext
      <$> o Aeson..: AesonKey.fromString "compilerVersion"
      <*> o Aeson..: AesonKey.fromString "target"
      <*> o Aeson..: AesonKey.fromString "optimize"
      <*> o Aeson..: AesonKey.fromString "mode"
      <*> o Aeson..: AesonKey.fromString "args"

data TestCachedResult = TestCachedResult
  { tcrComplete     :: Bool
  , tcrSuccess      :: Maybe Bool
  , tcrSkipped      :: Bool
  , tcrSkipReason   :: Maybe String
  , tcrException    :: Maybe String
  , tcrOutput       :: Maybe String
  , tcrStdOut       :: Maybe String
  , tcrStdErr       :: Maybe String
  , tcrFlaky        :: Bool
  , tcrNumFailures  :: Int
  , tcrNumErrors    :: Int
  , tcrNumIterations :: Int
  , tcrTestDuration :: Double
  } deriving (Show, Eq)

instance Aeson.ToJSON TestCachedResult where
  toJSON res = Aeson.object
    [ AesonKey.fromString "complete" Aeson..= tcrComplete res
    , AesonKey.fromString "success" Aeson..= tcrSuccess res
    , AesonKey.fromString "skipped" Aeson..= tcrSkipped res
    , AesonKey.fromString "skip_reason" Aeson..= tcrSkipReason res
    , AesonKey.fromString "exception" Aeson..= tcrException res
    , AesonKey.fromString "output" Aeson..= tcrOutput res
    , AesonKey.fromString "std_out" Aeson..= tcrStdOut res
    , AesonKey.fromString "std_err" Aeson..= tcrStdErr res
    , AesonKey.fromString "flaky" Aeson..= tcrFlaky res
    , AesonKey.fromString "num_failures" Aeson..= tcrNumFailures res
    , AesonKey.fromString "num_errors" Aeson..= tcrNumErrors res
    , AesonKey.fromString "num_iterations" Aeson..= tcrNumIterations res
    , AesonKey.fromString "test_duration" Aeson..= tcrTestDuration res
    ]

instance Aeson.FromJSON TestCachedResult where
  parseJSON = Aeson.withObject "TestCachedResult" $ \o ->
    TestCachedResult
      <$> o Aeson..: AesonKey.fromString "complete"
      <*> o Aeson..:? AesonKey.fromString "success"
      <*> o Aeson..:? AesonKey.fromString "skipped" Aeson..!= False
      <*> o Aeson..:? AesonKey.fromString "skip_reason"
      <*> o Aeson..:? AesonKey.fromString "exception"
      <*> o Aeson..:? AesonKey.fromString "output"
      <*> o Aeson..:? AesonKey.fromString "std_out"
      <*> o Aeson..:? AesonKey.fromString "std_err"
      <*> o Aeson..:? AesonKey.fromString "flaky" Aeson..!= False
      <*> o Aeson..:? AesonKey.fromString "num_failures" Aeson..!= 0
      <*> o Aeson..:? AesonKey.fromString "num_errors" Aeson..!= 0
      <*> o Aeson..:? AesonKey.fromString "num_iterations" Aeson..!= 0
      <*> o Aeson..:? AesonKey.fromString "test_duration" Aeson..!= 0

data TestCacheEntry = TestCacheEntry
  { tceRunHash  :: String
  , tceImplHash :: Maybe String
  , tceResult   :: TestCachedResult
  } deriving (Show, Eq)

instance Aeson.ToJSON TestCacheEntry where
  toJSON entry = Aeson.object
    [ AesonKey.fromString "runHash" Aeson..= tceRunHash entry
    , AesonKey.fromString "implHash" Aeson..= tceImplHash entry
    , AesonKey.fromString "result" Aeson..= tceResult entry
    ]

instance Aeson.FromJSON TestCacheEntry where
  parseJSON = Aeson.withObject "TestCacheEntry" $ \o ->
    TestCacheEntry
      <$> o Aeson..: AesonKey.fromString "runHash"
      <*> o Aeson..:? AesonKey.fromString "implHash"
      <*> o Aeson..: AesonKey.fromString "result"

data TestCache = TestCache
  { tcVersion :: Int
  , tcContext :: TestRunContext
  , tcTests   :: M.Map String TestCacheEntry
  } deriving (Show, Eq)

instance Aeson.ToJSON TestCache where
  toJSON tc = Aeson.object
    [ AesonKey.fromString "version" Aeson..= tcVersion tc
    , AesonKey.fromString "context" Aeson..= tcContext tc
    , AesonKey.fromString "tests" Aeson..= tcTests tc
    ]

instance Aeson.FromJSON TestCache where
  parseJSON = Aeson.withObject "TestCache" $ \o ->
    TestCache
      <$> o Aeson..: AesonKey.fromString "version"
      <*> o Aeson..: AesonKey.fromString "context"
      <*> o Aeson..:? AesonKey.fromString "tests" Aeson..!= M.empty

-- | Convert a live test result into a cacheable payload.
cachedResultFromTest :: TestResult -> TestCachedResult
cachedResultFromTest res = TestCachedResult
  { tcrComplete = trComplete res
  , tcrSuccess = trSuccess res
  , tcrSkipped = trSkipped res
  , tcrSkipReason = trSkipReason res
  , tcrException = trException res
  , tcrOutput = trOutput res
  , tcrStdOut = trStdOut res
  , tcrStdErr = trStdErr res
  , tcrFlaky = trFlaky res
  , tcrNumFailures = trNumFailures res
  , tcrNumErrors = trNumErrors res
  , tcrNumIterations = trNumIterations res
  , tcrTestDuration = trTestDuration res
  }

-- | Rehydrate a cached payload into a TestResult.
testResultFromCache :: String -> String -> TestCachedResult -> TestResult
testResultFromCache modName testName res = TestResult
  { trModule = modName
  , trName = testName
  , trComplete = tcrComplete res
  , trSuccess = tcrSuccess res
  , trSkipped = tcrSkipped res
  , trSkipReason = tcrSkipReason res
  , trException = tcrException res
  , trOutput = tcrOutput res
  , trStdOut = tcrStdOut res
  , trStdErr = tcrStdErr res
  , trFlaky = tcrFlaky res
  , trNumFailures = tcrNumFailures res
  , trNumErrors = tcrNumErrors res
  , trNumIterations = tcrNumIterations res
  , trTestDuration = tcrTestDuration res
  , trRaw = Aeson.Null
  , trSnapshotUpdated = False
  , trCached = True
  }

data TestHashInfo = TestHashInfo
  { thiImplHash    :: Maybe B.ByteString
  , thiRunHash     :: Maybe String
  , thiResolvedName :: Maybe String
  , thiImplDeps    :: Maybe [(A.QName, B.ByteString)]
  } deriving (Show, Eq)

-- | Build an empty cache with the current context.
emptyTestCache :: TestRunContext -> TestCache
emptyTestCache ctx = TestCache
  { tcVersion = testCacheVersion
  , tcContext = ctx
  , tcTests = M.empty
  }

-- | Resolve the on-disk test cache path for a project.
testCachePath :: Compile.Paths -> FilePath
testCachePath paths = Compile.projPath paths </> "out" </> "test" </> "cache.json"

-- | Read the cache file, returning an empty cache on error/mismatch.
readTestCache :: FilePath -> TestRunContext -> IO TestCache
readTestCache path ctx = do
    exists <- doesFileExist path
    if not exists
      then return (emptyTestCache ctx)
      else do
        res <- (try :: IO a -> IO (Either SomeException a)) $ BL.readFile path
        case res of
          Left _ -> return (emptyTestCache ctx)
          Right bs ->
            case Aeson.decode bs of
              Just tc | tcVersion tc == testCacheVersion -> return tc
              _ -> return (emptyTestCache ctx)

-- | Write the cache file atomically.
writeTestCache :: FilePath -> TestCache -> IO ()
writeTestCache path tc = do
    createDirectoryIfMissing True (takeDirectory path)
    let tmpPath = path ++ ".tmp"
    BL.writeFile tmpPath (Aeson.encode tc)
    renameFile tmpPath path

-- | Hash the test run context to include in cache keys.
contextHashBytes :: TestRunContext -> B.ByteString
contextHashBytes ctx = SHA256.hash (BL.toStrict (Aeson.encode ctx))

-- | Hex-encode a hash.
toHex :: B.ByteString -> String
toHex = B.unpack . Base16.encode

-- | Hash impl/deps/context into a cache run hash string.
hashRun :: B.ByteString -> B.ByteString -> B.ByteString -> String
hashRun implHash depsHash ctxHash = toHex (SHA256.hash (B.concat [implHash, depsHash, ctxHash]))

-- | Hash a dependency list with a stable ordering.
hashDeps :: [(A.QName, B.ByteString)] -> B.ByteString
hashDeps deps =
    let sorted = Data.List.sortOn (Hashing.qnameKey . fst) deps
    in SHA256.hash (BL.toStrict (encode sorted))

-- | Shorten a hash to 8 hex characters.
shortHash :: B.ByteString -> String
shortHash bs = take 8 (toHex bs)

-- | Shorten a hex hash string to 8 characters.
shortHashStr :: String -> String
shortHashStr s = take 8 s

-- | Format the verbose test cache context header.
formatTestCacheContext :: B.ByteString -> FilePath -> String
formatTestCacheContext ctxHash cachePath =
    "[test-cache] context=" ++ shortHash ctxHash ++ " path=" ++ cachePath

-- | Format a dependency list with shortened hashes.
formatDepsShort :: [(A.QName, B.ByteString)] -> String
formatDepsShort deps =
    let entries = [ prstr qn ++ ":" ++ shortHash h | (qn, h) <- deps ]
        shown = take 6 entries
        suffix = if length entries > 6 then " +" ++ show (length entries - 6) ++ " more" else ""
    in "[" ++ intercalate ", " shown ++ suffix ++ "]"

-- | Format a verbose cache log line for a test.
formatTestCacheLog :: String -> String -> TestHashInfo -> Maybe TestCacheEntry -> String
formatTestCacheLog modName testName info entry =
    let testLabel = formatTestName modName testName
        candidates = intercalate ", " (testNameCandidates testName)
        cacheStatus =
          case (thiRunHash info, entry) of
            (Just runHash, Just e) | tceRunHash e == runHash -> "hit"
            (Just _, Just _) -> "miss (hash)"
            (Just _, Nothing) -> "miss (none)"
            (Nothing, _) -> "miss (no hash)"
        base = "[test-cache] " ++ testLabel
        implDeps = maybe "" (\deps -> " deps=" ++ formatDepsShort deps) (thiImplDeps info)
    in case info of
         TestHashInfo (Just implHash) (Just runHash) resolved _ ->
           base
             ++ " name=" ++ maybe "?" id resolved
             ++ " impl=" ++ shortHash implHash
             ++ " run=" ++ shortHashStr runHash
             ++ implDeps
             ++ " cache=" ++ cacheStatus
         _ ->
           base
             ++ " no impl hash"
             ++ " candidates=[" ++ candidates ++ "]"
             ++ " cache=" ++ cacheStatus

-- | Build TestHashInfo records for a list of tests.
buildTestHashInfos :: Compile.Paths
                   -> B.ByteString
                   -> [(String, [String])]
                   -> IO (M.Map String TestHashInfo)
buildTestHashInfos paths ctxHash testsByModule = do
    moduleHashes <- forM testsByModule $ \(modName, _) -> do
      nameHashes <- readModuleNameHashes paths modName
      return (modName, nameHashes)
    let nameHashesByModule = M.fromList moduleHashes
        seedCache = M.fromList
          [ (A.modName (splitOnDot modName), nameMap)
          | (modName, nameMap) <- M.toList nameHashesByModule
          ]
        allTests =
          concatMap (\(modName, names) -> map (\name -> (modName, name)) names) testsByModule
    depCacheRef <- newIORef seedCache
    M.fromList <$> forM allTests (\(modName, testName) -> do
      let nameMap = M.findWithDefault M.empty modName nameHashesByModule
      info <- buildTestHashInfo depCacheRef paths ctxHash nameMap testName
      return (mkTestKey modName testName, info))

-- | Split tests into cached results and ones to run.
classifyCachedTests :: (String -> IO ())
                    -> M.Map String TestCacheEntry
                    -> M.Map String TestHashInfo
                    -> [(String, String)]
                    -> IO ([TestResult], [(String, String)])
classifyCachedTests logLine cacheEntries testHashInfos allTests = do
    classified <- forM allTests $ \(modName, testName) -> do
      let key = mkTestKey modName testName
          info = M.findWithDefault (TestHashInfo Nothing Nothing Nothing Nothing) key testHashInfos
          entry = M.lookup key cacheEntries
      logLine (formatTestCacheLog modName testName info entry)
      case entry of
        Just cached
          | Just runHash <- thiRunHash info
          , tceRunHash cached == runHash ->
              return (Left (testResultFromCache modName testName (tceResult cached)))
        _ -> return (Right (modName, testName))
    return (partitionEithers classified)

-- | Build a cache key from module and test name.
mkTestKey :: String -> String -> String
mkTestKey modName testName = modName ++ ":" ++ testName

-- | Drop a prefix once if present.
stripPrefixOnce :: String -> String -> Maybe String
stripPrefixOnce pref s
  | pref `isPrefixOf` s = Just (drop (length pref) s)
  | otherwise = Nothing

-- | Drop a suffix once if present.
stripSuffixOnce :: String -> String -> Maybe String
stripSuffixOnce suff s
  | suff `isSuffixOf` s = Just (take (length s - length suff) s)
  | otherwise = Nothing

-- | Generate alternate test names for wrapper/prefix variants.
testNameCandidates :: String -> [String]
testNameCandidates name =
    nub $ catMaybes
      [ Just name
      , stripSuffixOnce "_wrapper" name
      , stripPrefixOnce "_test_" name
      , stripPrefixOnce "_test_" =<< stripSuffixOnce "_wrapper" name
      , stripSuffixOnce "_wrapper" =<< stripPrefixOnce "_test_" name
      ]

-- | Resolve a test name to the stored NameHashInfo, if any.
lookupTestInfo :: M.Map String InterfaceFiles.NameHashInfo -> String -> Maybe (String, InterfaceFiles.NameHashInfo)
lookupTestInfo nameMap testName =
    listToMaybe [ (cand, info)
                | cand <- testNameCandidates testName
                , Just info <- [M.lookup cand nameMap]
                ]

-- | Read the name hash map for a module name string.
readModuleNameHashes :: Compile.Paths -> String -> IO (M.Map String InterfaceFiles.NameHashInfo)
readModuleNameHashes paths modName =
    readModuleNameHashesByModName paths (A.modName (splitOnDot modName))

-- | Split a module name string on dots.
splitOnDot :: String -> [String]
splitOnDot = splitOnChar '.'

-- | Split a string on a single delimiter character.
splitOnChar :: Char -> String -> [String]
splitOnChar ch input = case break (== ch) input of
  (chunk, []) -> [chunk]
  (chunk, _ : rest) -> chunk : splitOnChar ch rest

-- | Read the name hash map for a module name.
readModuleNameHashesByModName :: Compile.Paths -> A.ModName -> IO (M.Map String InterfaceFiles.NameHashInfo)
readModuleNameHashesByModName paths mn = do
    mty <- Acton.Env.findTyFile (Compile.searchPath paths) mn
    case mty of
      Nothing -> return M.empty
      Just tyFile -> do
        hdrE <- (try :: IO a -> IO (Either SomeException a)) $ InterfaceFiles.readHeader tyFile
        case hdrE of
          Left _ -> return M.empty
          Right (_srcH, _ih, _implH, _imps, nameHashes, _roots, _tests, _doc) ->
            return $ M.fromList [ (A.nstr (InterfaceFiles.nhName nh), nh) | nh <- nameHashes ]

-- | Cache-aware wrapper for reading name hashes.
readModuleNameHashesCached :: IORef (M.Map A.ModName (M.Map String InterfaceFiles.NameHashInfo))
                           -> Compile.Paths
                           -> A.ModName
                           -> IO (M.Map String InterfaceFiles.NameHashInfo)
readModuleNameHashesCached cacheRef paths mn = do
    cache <- readIORef cacheRef
    case M.lookup mn cache of
      Just nameMap -> return nameMap
      Nothing -> do
        nameMap <- readModuleNameHashesByModName paths mn
        atomicModifyIORef' cacheRef (\m -> (M.insert mn nameMap m, ()))
        return nameMap

-- | Resolve a dependency QName to its current impl hash, if any.
resolveDepImplHash :: IORef (M.Map A.ModName (M.Map String InterfaceFiles.NameHashInfo))
                   -> Compile.Paths
                   -> A.QName
                   -> IO (Maybe (A.QName, B.ByteString))
resolveDepImplHash cacheRef paths qn =
    case qn of
      A.GName m n -> resolve m n (A.GName m n)
      A.QName m n -> resolve m n (A.GName m n)
      A.NoQ _ -> return Nothing
  where
    resolve m n qn' = do
      nameMap <- readModuleNameHashesCached cacheRef paths m
      return $ (\info -> (qn', InterfaceFiles.nhImplHash info)) <$> M.lookup (A.nstr n) nameMap

-- | Resolve recorded impl deps to current hashes, preserving fallbacks.
resolveTestImplDeps :: IORef (M.Map A.ModName (M.Map String InterfaceFiles.NameHashInfo))
                    -> Compile.Paths
                    -> InterfaceFiles.NameHashInfo
                    -> IO [(A.QName, B.ByteString)]
resolveTestImplDeps cacheRef paths info = do
    let deps = InterfaceFiles.nhImplDeps info
    forM deps $ \(qn, recorded) -> do
      resolved <- resolveDepImplHash cacheRef paths qn
      return (fromMaybe (qn, recorded) resolved)

-- | Build the hash info for a single test name.
buildTestHashInfo :: IORef (M.Map A.ModName (M.Map String InterfaceFiles.NameHashInfo))
                  -> Compile.Paths
                  -> B.ByteString
                  -> M.Map String InterfaceFiles.NameHashInfo
                  -> String
                  -> IO TestHashInfo
buildTestHashInfo cacheRef paths ctxHash nameMap testName = do
    case lookupTestInfo nameMap testName of
      Nothing -> return (TestHashInfo Nothing Nothing Nothing Nothing)
      Just (resolved, info) -> do
        let implHash = InterfaceFiles.nhImplHash info
        deps <- resolveTestImplDeps cacheRef paths info
        let depsHash = hashDeps deps
            runHash = hashRun implHash depsHash ctxHash
        return (TestHashInfo (Just implHash) (Just runHash) (Just resolved) (Just deps))

-- | Update the cache map with a new test result.
updateTestCacheEntry :: M.Map String TestHashInfo
                     -> M.Map String TestCacheEntry
                     -> TestResult
                     -> M.Map String TestCacheEntry
updateTestCacheEntry testHashInfos acc res =
    let key = mkTestKey (trModule res) (trName res)
        info = M.lookup key testHashInfos
        runHash = maybe "" (\ti -> fromMaybe "" (thiRunHash ti)) info
        implHashHex = info >>= fmap toHex . thiImplHash
        entry = TestCacheEntry
          { tceRunHash = runHash
          , tceImplHash = implHashHex
          , tceResult = cachedResultFromTest res
          }
    in M.insert key entry acc

-- | Format a module+test display name.
formatTestName :: String -> String -> String
formatTestName modName testName = modName ++ "." ++ testName
