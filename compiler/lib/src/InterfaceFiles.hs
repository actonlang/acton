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

{-# LANGUAGE DeriveGeneric, ScopedTypeVariables #-}
-- Acton Interface (.tydb) Files
--
-- Purpose
-- - Cache compiled module information so later builds can avoid unnecessary
--   work by reading a small header instead of decoding large structures.
-- - If a .tydb cannot be decoded or the version mismatches, the caller must
--   treat the module as out-of-date and recompile from source.
--
-- On-disk layout
-- A .tydb is an LMDB directory environment (containing data.mdb and lock.mdb)
-- holding a single unnamed database. Each field below is one key; values are
-- encoded with persist. The persist format is used as a trusted local cache
-- encoding, not as a stable cross-machine interchange format. Where the old
-- .ty format wrote one blob in a fixed order, here every field is an
-- independently addressable key.
--
--   Module-level keys (one each):
--     "version"        :: [Int]                       -- Acton.Syntax.version
--     "meta"           :: ( Maybe SourceFileMeta      -- cached source stat metadata
--                         , ByteString                -- moduleSrcBytesHash: SHA-256 of raw source
--                         , ByteString                -- modulePubHash: structural hash of public NameInfo
--                                                     --   (doc-free) + imports' pub hashes
--                         , ByteString )              -- moduleImplHash: structural hash of per-name impl hashes
--     "imports"        :: [(A.ModName, ByteString)]   -- dependency module and pub hash used
--     "deps"           :: [DepModuleInfo]              -- dependency modules with pub/impl hashes
--     "roots"          :: [A.Name]                    -- root actors (e.g. main or test_main)
--     "tests"          :: [String]                    -- discovered test names
--     "doc"            :: Maybe String                -- module docstring
--     "name-count"     :: Int                         -- number of NameInfo entries (see name-order)
--     "stmt-count"     :: Int                         -- number of typed top-level statements
--     "module-header"  :: (A.ModName, Imports, Maybe String)
--                                                     -- typed module name, imports, docstring
--
--   Per-name keys (NameInfo / TEnv, one set per name):
--     "name-order/<NNN>"   :: ByteString              -- name-key suffix, in TEnv order (NNN = padIndex)
--     "name-info/<suffix>" :: (A.Name, I.NameInfo)    -- the name and its type/name environment entry
--     "name-hash/<suffix>" :: NameHashInfo            -- per-name src/pub/impl hashes + local deps
--
--   Per-dependency keys:
--     "deps/<module>"          :: [DepNameInfo]        -- dependency names with pub/impl hashes
--     "deps/<module>/<suffix>" :: DepUsers             -- local names that use one dependency name
--
--   Per-extension keys:
--     "ext-by-class/<suffix>"       :: (A.Name, [A.Name]) -- class name to extension names
--     "ext-by-protocol/<suffix>"    :: (A.Name, [A.Name]) -- protocol name to extension names
--
--   Per-statement keys (typed Module body):
--     "stmt/<NNN>"     :: A.Stmt                      -- one typed top-level statement (NNN = padIndex)
--
-- <suffix> encodes the semantic name: "p/<text>" for plain safe names (used as
-- the key verbatim) or "h/<sha256hex>" for long or unsafe names. See nameKeySuffix.
--
-- Rationale for the keyed layout
-- - Keep the small validity/header fields (version, meta, dependency hashes, roots, tests,
--   doc, name-hashes) as their own keys so callers can validate and reuse a cache
--   entry, or do freshness/dependency checks, without decoding the large NameInfo
--   and typed Module sections (readHeader vs readFile).
-- - Store NameInfo and the typed module body as explicitly-ordered entries
--   (name-order/... and stmt/...) rather than relying on LMDB cursor order, so a
--   full read reconstructs the exact original ordering -- and so later work can
--   move from full reconstruction to selective per-name lookup without changing
--   the outer cache boundary.

module InterfaceFiles
  ( NameHashInfo(..)
  , DepModuleInfo(..)
  , DepNameInfo(..)
  , DepUsers(..)
  , SourceFileMeta(..)
  , TyFile
  , TyHeader
  , interfaceExt
  , interfacePath
  , interfaceExists
  , interfaceModifiedTime
  , interfaceModifiedTimeNs
  , copyInterface
  , listInterfaceDirsRecursive
  , keyNameInfo
  , keyNameHash
  , readDepNames
  , readDepUsers
  , readNameHashMaybe
  , readModuleHashesMaybe
  , readFile
  , readHeader
  , readExtensionsByClass
  , readExtensionsByProtocol
  , readFileMaybe
  , readHeaderMaybe
  , TyDbWriteProgress(..)
  , writeFile
  , writeFileWithProgress
  , writeFileWithVersion
  ) where

import Prelude hiding (readFile, writeFile)
import Control.DeepSeq (NFData, rnf)
import qualified Control.Exception as E
import Control.Concurrent (getNumCapabilities, runInBoundThread, threadDelay)
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, withMVar)
import Control.Monad (forM, forM_, unless, when)
import Data.IORef (atomicModifyIORef', newIORef)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import qualified Data.List
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import qualified Data.Persist as Persist
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime)
import qualified Database.LMDB.Raw as LMDB
import qualified Acton.Syntax as A
import qualified Acton.NameInfo as I
import Foreign.Ptr (castPtr)
import Foreign.Storable (peek)
import GHC.Generics (Generic)
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getFileSize, getModificationTime, listDirectory, removeFile, removePathForcibly)
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeExtension)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Files (deviceID, fileAccess, fileID, getFileStatus, modificationTimeHiRes, setFileMode)

data NameHashInfo = NameHashInfo
  { nhName     :: A.Name
  , nhSrcHash  :: BS.ByteString
  , nhPubHash  :: BS.ByteString
  , nhImplHash :: BS.ByteString
  , nhPubLocalDeps  :: [A.Name]
  , nhImplLocalDeps :: [A.Name]
  , nhPubDeps  :: [(A.QName, BS.ByteString)]
  , nhImplDeps :: [(A.QName, BS.ByteString)]
  } deriving (Show, Eq, Generic)

instance Persist.Persist NameHashInfo
instance NFData NameHashInfo

data DepModuleInfo = DepModuleInfo
  { dmiModule   :: A.ModName
  , dmiPubHash  :: BS.ByteString
  , dmiImplHash :: BS.ByteString
  } deriving (Show, Eq, Generic)

instance Persist.Persist DepModuleInfo
instance NFData DepModuleInfo

data DepNameInfo = DepNameInfo
  { dniName     :: A.Name
  , dniPubHash  :: BS.ByteString
  , dniImplHash :: BS.ByteString
  } deriving (Show, Eq, Generic)

instance Persist.Persist DepNameInfo
instance NFData DepNameInfo

data DepUsers = DepUsers
  { duPubUsers  :: [A.Name]
  , duImplUsers :: [A.Name]
  } deriving (Show, Eq, Generic)

instance Persist.Persist DepUsers
instance NFData DepUsers

emptyDepUsers :: DepUsers
emptyDepUsers = DepUsers [] []

data ExtensionIndex = ExtensionIndex
  { extByClass      :: Map.Map A.Name [A.Name]
  , extByProtocol   :: Map.Map A.Name [A.Name]
  } deriving (Show, Eq)

data SourceFileMeta = SourceFileMeta
  { sfmMTimeNs :: Integer
  , sfmCTimeNs :: Integer
  , sfmSize    :: Integer
  , sfmDevice  :: Maybe Integer
  , sfmInode   :: Maybe Integer
  } deriving (Show, Eq, Generic)

instance Persist.Persist SourceFileMeta
instance NFData SourceFileMeta

data TyDbWriteProgress = TyDbWriteProgress
  { tyDbWriteProgressLabel :: String
  , tyDbWriteProgressRatio :: Double
  } deriving (Show, Eq)

type TyFile =
  ( [A.ModName]
  , I.NameInfo
  , A.Module
  , Maybe SourceFileMeta
  , BS.ByteString
  , BS.ByteString
  , BS.ByteString
  , [(A.ModName, BS.ByteString)]
  , [DepModuleInfo]
  , [NameHashInfo]
  , [A.Name]
  , [String]
  , Maybe String
  )

type TyHeader =
  ( Maybe SourceFileMeta
  , BS.ByteString
  , BS.ByteString
  , BS.ByteString
  , [(A.ModName, BS.ByteString)]
  , [DepModuleInfo]
  , [NameHashInfo]
  , [A.Name]
  , [String]
  , Maybe String
  )

type TyMeta =
  ( Maybe SourceFileMeta
  , BS.ByteString
  , BS.ByteString
  , BS.ByteString
  )

-- Note: tests are stored in the header to support listing without compiling
--       or executing test binaries.

interfaceExt :: String
interfaceExt = ".tydb"

interfacePath :: FilePath -> A.ModName -> FilePath
interfacePath root mn = foldl (</>) root (A.modPath mn) ++ interfaceExt

dataFilePath :: FilePath -> FilePath
dataFilePath path = path </> "data.mdb"

lockFilePath :: FilePath -> FilePath
lockFilePath path = path </> "lock.mdb"

-- The raw LMDB binding can race when several threads open environments at once.
lmdbOpenLock :: MVar ()
{-# NOINLINE lmdbOpenLock #-}
lmdbOpenLock = unsafePerformIO (newMVar ())

-- LMDB does not support opening the same environment twice in one process.
-- Exclusive users (writers, env copies) serialize on a per-path lock and own
-- the only environment while they run. Readers share one cached read-only
-- environment per path, tracked in sharedEnvs with an active-reader count;
-- exclusive users retire the cached environment and wait for its readers to
-- drain before opening their own, so the process never holds two
-- environments for one path.
interfaceLocks :: MVar (Map.Map FilePath (MVar ()))
{-# NOINLINE interfaceLocks #-}
interfaceLocks = unsafePerformIO (newMVar Map.empty)

data SharedEnv = SharedEnv
  { seEnv :: LMDB.MDB_env
  , seIdent :: Maybe (Integer, Integer)
  , seReaders :: Int
  , seRetiring :: Bool
  }

data SharedEnvClaim = Claimed SharedEnv | Retiring | Absent

-- Identify the data file behind a path, so a cached environment whose file
-- was removed and recreated (a new inode) is not mistaken for current.
envFileIdent :: FilePath -> IO (Maybe (Integer, Integer))
envFileIdent path =
    (do st <- getFileStatus (dataFilePath path)
        return (Just (fromIntegral (deviceID st), fromIntegral (fileID st))))
      `E.catch` \e -> if isDoesNotExistError e then return Nothing else E.throwIO e

sharedEnvs :: MVar (Map.Map FilePath SharedEnv)
{-# NOINLINE sharedEnvs #-}
sharedEnvs = unsafePerformIO (newMVar Map.empty)

traceTydbReads :: Bool
traceTydbReads =
    unsafePerformIO $ maybe False (not . null) <$> lookupEnv "ACTON_TYDB_TRACE_READS"
{-# NOINLINE traceTydbReads #-}

traceTydbRead :: String -> FilePath -> String -> IO ()
traceTydbRead op path detail =
    when traceTydbReads $
      hPutStrLn stderr (unwords ["tydb-read", op, path, detail])

interfaceExists :: FilePath -> IO Bool
interfaceExists = doesDirectoryExist

interfaceModifiedTime :: FilePath -> IO UTCTime
interfaceModifiedTime path = getModificationTime (dataFilePath path)

interfaceModifiedTimeNs :: FilePath -> IO Integer
interfaceModifiedTimeNs path = do
    st <- getFileStatus (dataFilePath path)
    return (floor (toRational (modificationTimeHiRes st) * 1000000000))

copyInterface :: FilePath -> FilePath -> IO ()
copyInterface src dst = do
    removePathForcibly dst `E.catch` ignoreMissing
    createDirectoryIfMissing True dst
    mapSize <- readMapSize src
    runInLmdbThread $
      withEnv src True mapSize $ \env ->
        LMDB.mdb_env_copy env dst
    setReadableInterfacePermissions dst
  where
    ignoreMissing :: E.IOException -> IO ()
    ignoreMissing _ = return ()

listInterfaceDirsRecursive :: FilePath -> IO [FilePath]
listInterfaceDirsRecursive root = do
    entries <- listDirectory root
    fmap concat $ forM entries $ \entry -> do
      let path = root </> entry
      isDir <- doesDirectoryExist path
      if not isDir
        then return []
        else if takeExtension path == interfaceExt
               then return [path]
               else listInterfaceDirsRecursive path

-- | A .tydb cache entry that is absent, stale, or structurally unusable -- a
-- genuine cache miss that should trigger recompilation. Deliberately distinct
-- from a transient or environmental failure to read the cache (e.g. a
-- concurrent LMDB error): those must propagate rather than be silently mistaken
-- for "interface missing", which is what produced misleading
-- "not found or unreadable" diagnostics.
newtype TyCacheInvalid = TyCacheInvalid String deriving Show

instance E.Exception TyCacheInvalid

versionMismatch :: [Int] -> IO a
versionMismatch vs =
    E.throwIO (TyCacheInvalid (".tydb version mismatch: file has " ++ show vs ++ ", expected " ++ show A.version))

decodeStrict :: Persist.Persist a => String -> BS.ByteString -> IO a
decodeStrict label bs =
    case Persist.decode bs of
      Left err -> E.throwIO (TyCacheInvalid ("Failed to decode .tydb " ++ label ++ ": " ++ err))
      Right v -> return v

encodeStrict :: Persist.Persist a => a -> BS.ByteString
encodeStrict = Persist.encode

key :: String -> BS.ByteString
key = B.pack

keyVersion, keyMeta, keyImports, keyDeps, keyRoots, keyTests, keyDoc, keyNameCount, keyStmtCount, keyModuleHeader :: BS.ByteString
keyVersion      = key "version"
keyMeta         = key "meta"
keyImports      = key "imports"
keyDeps         = key "deps"
keyRoots        = key "roots"
keyTests        = key "tests"
keyDoc          = key "doc"
keyNameCount    = key "name-count"
keyStmtCount    = key "stmt-count"
keyModuleHeader = key "module-header"

padIndex :: Int -> String
padIndex i =
    let s = show i
    in replicate (12 - length s) '0' ++ s

plainNameKeyLimit :: Int
plainNameKeyLimit = 400

plainKeyRaw :: BS.ByteString -> Bool
plainKeyRaw raw =
    BS.length raw <= plainNameKeyLimit && BS.all safe raw
  where
    safe w =
        w > 32 && w < 127 && w /= 47

safeKeySuffix :: BS.ByteString -> BS.ByteString
safeKeySuffix raw =
    if plainKeyRaw raw
      then B.concat [key "p/", raw]
      else B.concat [key "h/", Base16.encode (SHA256.hash raw)]

nameKeySuffix :: A.Name -> BS.ByteString
nameKeySuffix n =
    safeKeySuffix (TE.encodeUtf8 (T.pack (A.rawstr n)))

moduleKeySuffix :: A.ModName -> BS.ByteString
moduleKeySuffix mn =
    let raw = TE.encodeUtf8 (T.pack (Data.List.intercalate "." (A.modPath mn)))
    in if plainKeyRaw raw
         then raw
         else B.concat [key "h/", Base16.encode (SHA256.hash raw)]

keyNameInfo :: A.Name -> BS.ByteString
keyNameInfo n = B.concat [key "name-info/", nameKeySuffix n]

keyNameInfoSuffix :: BS.ByteString -> BS.ByteString
keyNameInfoSuffix suffix = B.concat [key "name-info/", suffix]

keyNameOrder :: Int -> BS.ByteString
keyNameOrder i = B.pack ("name-order/" ++ padIndex i)

keyNameHashPrefix :: BS.ByteString
keyNameHashPrefix = key "name-hash/"

keyNameHash :: A.Name -> BS.ByteString
keyNameHash n = B.concat [keyNameHashPrefix, nameKeySuffix n]

keyDepModule :: A.ModName -> BS.ByteString
keyDepModule mn = B.concat [key "deps/", moduleKeySuffix mn]

keyDepName :: A.ModName -> A.Name -> BS.ByteString
keyDepName mn n = B.concat [keyDepModule mn, key "/", nameKeySuffix n]

keyExtByClassPrefix, keyExtByProtocolPrefix :: BS.ByteString
keyExtByClassPrefix      = key "ext-by-class/"
keyExtByProtocolPrefix   = key "ext-by-protocol/"

keyExtByClass :: A.Name -> BS.ByteString
keyExtByClass n = B.concat [keyExtByClassPrefix, nameKeySuffix n]

keyExtByProtocol :: A.Name -> BS.ByteString
keyExtByProtocol n = B.concat [keyExtByProtocolPrefix, nameKeySuffix n]

keyStmt :: Int -> BS.ByteString
keyStmt i = B.pack ("stmt/" ++ padIndex i)

withVal :: BS.ByteString -> (LMDB.MDB_val -> IO a) -> IO a
withVal bs f =
    BS.useAsCStringLen bs $ \(ptr, len) ->
      f (LMDB.MDB_val (fromIntegral len) (castPtr ptr))

copyVal :: LMDB.MDB_val -> IO BS.ByteString
copyVal (LMDB.MDB_val len ptr) =
    BS.packCStringLen (castPtr ptr, fromIntegral len)

withEnv :: FilePath -> Bool -> Int -> (LMDB.MDB_env -> IO a) -> IO a
withEnv path readOnly mapSize action = do
    cpath <- canonicalizePath path
    withInterfaceLockC cpath $ do
      evictSharedEnv cpath
      E.bracket (openEnvWithRetry path readOnly mapSize) LMDB.mdb_env_close action

openEnvWithRetry :: FilePath -> Bool -> Int -> IO LMDB.MDB_env
openEnvWithRetry path readOnly mapSize =
    withMVar lmdbOpenLock $ \_ -> openWithRetry lmdbTransientMaxAttempts
  where
    -- Concurrent opens of the same env race on lock.mdb setup, which surfaces as
    -- a transient OS-level mdb_env_open failure (e.g. ENOENT) that resolves on
    -- retry. LMDB-semantic failures (corruption, version mismatch) are not
    -- transient and propagate immediately to the cache-miss / recompile path.
    openWithRetry attempt = do
      env <- LMDB.mdb_env_create
      let configure = do LMDB.mdb_env_set_mapsize env mapSize
                         openEnv env
      r <- E.try (configure `E.onException` LMDB.mdb_env_close env)
      case r of
        Right () -> return env
        Left (err :: LMDB.LMDB_Error)
          | attempt > 1 && isTransientLmdbOsError err ->
              threadDelay lmdbTransientRetryDelayUs >> openWithRetry (attempt - 1)
          | otherwise -> E.throwIO err
    openEnv env
      | readOnly  = do
          flags <- readOnlyOpenFlags path
          LMDB.mdb_env_open env path flags
      | otherwise = LMDB.mdb_env_open env path []

-- A transient LMDB failure is an OS-level error (Either Left, a raw errno) from
-- lock-table setup/use under concurrent readers. LMDB-semantic errors (Either
-- Right MDB_*) are not transient and must not be retried.
isTransientLmdbOsError :: LMDB.LMDB_Error -> Bool
isTransientLmdbOsError err =
    case LMDB.e_code err of
      Left _  -> True
      Right _ -> False

-- Choose read-only open flags. A writable cache may be rewritten by a
-- concurrent acton process, so it MUST keep LMDB's lock table for reader/writer
-- safety (the lock.mdb open race there is handled by retrying the open). Only an
-- installed cache the user cannot write -- where no writer can exist -- uses
-- MDB_NOLOCK, so it stays readable even when lock.mdb cannot be created in a
-- read-only directory.
readOnlyOpenFlags :: FilePath -> IO [LMDB.MDB_EnvFlag]
readOnlyOpenFlags path = do
    useLock <- canUseLockFile path
    noLock <- canReadWithoutLock path
    return $ if useLock || not noLock
               then [LMDB.MDB_RDONLY]
               else [LMDB.MDB_RDONLY, LMDB.MDB_NOLOCK]

canUseLockFile :: FilePath -> IO Bool
canUseLockFile path = do
    let lockPath = lockFilePath path
    exists <- doesFileExist lockPath
    if exists
      then fileAccess lockPath False True False
      else fileAccess path False True True

canReadWithoutLock :: FilePath -> IO Bool
canReadWithoutLock path = do
    let dataPath = dataFilePath path
    exists <- doesFileExist dataPath
    if not exists
      then return False
      else do
        dataWritable <- fileAccess dataPath False True False
        dirWritable <- fileAccess path False True True
        return (not dataWritable && not dirWritable)

lmdbTransientMaxAttempts :: Int
lmdbTransientMaxAttempts = 10

lmdbTransientRetryDelayUs :: Int
lmdbTransientRetryDelayUs = 20000

withInterfaceLock :: FilePath -> IO a -> IO a
withInterfaceLock path action = do
    cpath <- canonicalizePath path
    withInterfaceLockC cpath action

withInterfaceLockC :: FilePath -> IO a -> IO a
withInterfaceLockC cpath action = do
    lock <- modifyMVar interfaceLocks $ \locks ->
      case Map.lookup cpath locks of
        Just lock -> return (locks, lock)
        Nothing -> do
          lock <- newMVar ()
          return (Map.insert cpath lock locks, lock)
    withMVar lock $ \_ -> action

-- Claim the shared read environment for a path, opening it on first use. The
-- open runs under the per-path lock so it cannot race an exclusive user; a
-- retiring entry is waited out, since its last reader closes it.
acquireSharedEnv :: FilePath -> FilePath -> IO LMDB.MDB_env
acquireSharedEnv cpath path = do
    ident <- envFileIdent path
    claim <- claimSharedEnv cpath
    case claim of
      Claimed se
        | seIdent se == ident -> return (seEnv se)
        | otherwise -> do
            -- The file behind the cached environment was replaced.
            retireSharedEnv cpath
            releaseSharedEnv cpath
            acquireSharedEnv cpath path
      Retiring -> threadDelay lmdbTransientRetryDelayUs >> acquireSharedEnv cpath path
      Absent -> do
        mopened <- withInterfaceLockC cpath $ do
          claim' <- claimSharedEnv cpath
          case claim' of
            Claimed se -> return (Just (seEnv se))
            Retiring -> return Nothing
            Absent -> do
              -- The binding adds MDB_NOTLS implicitly, so reader slots follow
              -- transactions rather than the transient bound threads they run
              -- on.
              mapSize <- readMapSize path
              env <- openEnvWithRetry path True mapSize
              ident' <- envFileIdent path
              modifyMVar_ sharedEnvs (return . Map.insert cpath (SharedEnv env ident' 1 False))
              return (Just env)
        case mopened of
          Just env -> return env
          Nothing -> threadDelay lmdbTransientRetryDelayUs >> acquireSharedEnv cpath path

claimSharedEnv :: FilePath -> IO SharedEnvClaim
claimSharedEnv cpath =
    modifyMVar sharedEnvs $ \envs ->
      case Map.lookup cpath envs of
        Just se | not (seRetiring se) ->
          return (Map.insert cpath se{ seReaders = seReaders se + 1 } envs, Claimed se)
        Just _ -> return (envs, Retiring)
        Nothing -> return (envs, Absent)

releaseSharedEnv :: FilePath -> IO ()
releaseSharedEnv cpath = do
    mclose <- modifyMVar sharedEnvs $ \envs ->
      case Map.lookup cpath envs of
        Just se
          | seReaders se <= 1 && seRetiring se -> return (Map.delete cpath envs, Just (seEnv se))
          | otherwise -> return (Map.insert cpath se{ seReaders = seReaders se - 1 } envs, Nothing)
        Nothing -> return (envs, Nothing)
    mapM_ LMDB.mdb_env_close mclose

-- Mark the shared environment stale (e.g. the map was grown by another
-- process); the last reader closes it and the next acquire reopens.
retireSharedEnv :: FilePath -> IO ()
retireSharedEnv cpath = do
    mclose <- modifyMVar sharedEnvs $ \envs ->
      case Map.lookup cpath envs of
        Just se
          | seReaders se <= 0 -> return (Map.delete cpath envs, Just (seEnv se))
          | otherwise -> return (Map.insert cpath se{ seRetiring = True } envs, Nothing)
        Nothing -> return (envs, Nothing)
    mapM_ LMDB.mdb_env_close mclose

-- Exclusive users call this under the per-path lock: retire the shared
-- environment and wait until its active readers have drained and closed it.
evictSharedEnv :: FilePath -> IO ()
evictSharedEnv cpath = do
    retireSharedEnv cpath
    waitGone
  where
    waitGone = do
      gone <- withMVar sharedEnvs (return . Map.notMember cpath)
      unless gone (threadDelay lmdbTransientRetryDelayUs >> waitGone)

withReadTxn :: FilePath -> (LMDB.MDB_txn -> LMDB.MDB_dbi -> IO a) -> IO a
withReadTxn path action = do
    cpath <- canonicalizePath path
    -- mdb_txn_begin can hit a transient lock-table OS error, and another
    -- process may have grown the map since the shared environment was opened;
    -- both retire the cached environment and retry with a fresh one.
    runInLmdbThread $ readTxnWithRetry lmdbTransientMaxAttempts cpath
  where
    readTxnWithRetry attempt cpath = do
      env <- acquireSharedEnv cpath path
      res <- (E.try $
               E.bracket (LMDB.mdb_txn_begin env Nothing True) LMDB.mdb_txn_abort $ \txn -> do
                 dbi <- LMDB.mdb_dbi_open txn Nothing []
                 action txn dbi)
             `E.onException` releaseSharedEnv cpath
      case res of
        Right x -> releaseSharedEnv cpath >> return x
        Left (err :: LMDB.LMDB_Error)
          | attempt > 1 && (isTransientLmdbOsError err || isMapResized err) -> do
              retireSharedEnv cpath
              releaseSharedEnv cpath
              threadDelay lmdbTransientRetryDelayUs
              readTxnWithRetry (attempt - 1) cpath
          | otherwise -> releaseSharedEnv cpath >> E.throwIO err

isMapResized :: LMDB.LMDB_Error -> Bool
isMapResized err =
    case LMDB.e_code err of
      Right LMDB.MDB_MAP_RESIZED -> True
      _ -> False

withWriteTxn :: FilePath -> Int -> (LMDB.MDB_txn -> LMDB.MDB_dbi -> IO a) -> IO a
withWriteTxn path mapSize action =
    runInLmdbThread $
      withEnv path False mapSize $ \env ->
        E.mask $ \restore -> do
          txn <- LMDB.mdb_txn_begin env Nothing False
          r <- restore (LMDB.mdb_dbi_open txn Nothing [] >>= action txn) `E.onException` LMDB.mdb_txn_abort txn
          LMDB.mdb_txn_commit txn
          return r

-- The raw LMDB binding requires transaction setup from a bound thread; this
-- applies to reads too because of its Haskell-side lock guard.
runInLmdbThread :: IO a -> IO a
runInLmdbThread = runInBoundThread

readMapSize :: FilePath -> IO Int
readMapSize path = do
    exists <- doesFileExist (dataFilePath path)
    if exists
      then max minMapSize . fromIntegral <$> getFileSize (dataFilePath path)
      else return minMapSize

minMapSize :: Int
minMapSize = 1024 * 1024

entryMapSize :: [(BS.ByteString, BS.ByteString)] -> IO Int
entryMapSize entries = do
    let payload = sum [ BS.length k + BS.length v | (k, v) <- entries ]
        wanted = max minMapSize (payload * 4 + minMapSize)
    return (nextPowerOfTwo wanted)

nextPowerOfTwo :: Int -> Int
nextPowerOfTwo n = go minMapSize
  where
    go x | x >= n = x
         | otherwise = go (x * 2)

getValue :: Persist.Persist a => String -> LMDB.MDB_txn -> LMDB.MDB_dbi -> BS.ByteString -> IO a
getValue label txn dbi k = do
    mv <- withVal k (LMDB.mdb_get txn dbi)
    case mv of
      Nothing -> E.throwIO (TyCacheInvalid ("Missing .tydb key: " ++ label))
      Just v -> copyVal v >>= decodeStrict label

getMaybeValue :: Persist.Persist a => String -> LMDB.MDB_txn -> LMDB.MDB_dbi -> BS.ByteString -> IO (Maybe a)
getMaybeValue label txn dbi k = do
    mv <- withVal k (LMDB.mdb_get txn dbi)
    case mv of
      Nothing -> return Nothing
      Just v -> Just <$> (copyVal v >>= decodeStrict label)

getValuesWithPrefix :: LMDB.MDB_txn -> LMDB.MDB_dbi -> BS.ByteString -> IO [BS.ByteString]
getValuesWithPrefix txn dbi prefix =
    E.bracket (LMDB.mdb_cursor_open txn dbi) LMDB.mdb_cursor_close $ \cursor ->
      withVal prefix $ \start ->
        withVal BS.empty $ \empty ->
          LMDB.withKVPtrs start empty $ \kp vp -> do
            found <- LMDB.mdb_cursor_get LMDB.MDB_SET_RANGE cursor kp vp
            go cursor kp vp found
  where
    go _ _ _ False = return []
    go cursor kp vp True = do
      k <- peek kp >>= copyVal
      if not (prefix `BS.isPrefixOf` k)
        then return []
        else do
          v <- peek vp >>= copyVal
          found <- LMDB.mdb_cursor_get LMDB.MDB_NEXT cursor kp vp
          (v :) <$> go cursor kp vp found

putValue :: LMDB.MDB_txn -> LMDB.MDB_dbi -> BS.ByteString -> BS.ByteString -> IO ()
putValue txn dbi k v =
    withVal k $ \kv ->
      withVal v $ \vv -> do
        _ <- LMDB.mdb_put (LMDB.compileWriteFlags []) txn dbi kv vv
        return ()

isMapFull :: E.SomeException -> Bool
isMapFull err =
    case E.fromException err of
      Just LMDB.LMDB_Error{ LMDB.e_code = Right LMDB.MDB_MAP_FULL } -> True
      _ -> False

isCorruptEnv :: E.SomeException -> Bool
isCorruptEnv err =
    case E.fromException err of
      Just LMDB.LMDB_Error{ LMDB.e_code = Right code } ->
        code `elem` [ LMDB.MDB_PAGE_NOTFOUND
                    , LMDB.MDB_CORRUPTED
                    , LMDB.MDB_PANIC
                    , LMDB.MDB_VERSION_MISMATCH
                    , LMDB.MDB_INVALID
                    ]
      _ -> False

writeEntries :: FilePath -> [(BS.ByteString, BS.ByteString)] -> IO ()
writeEntries = writeEntriesWithProgress (\_ -> return ())

writeEntriesWithProgress :: (Double -> IO ()) -> FilePath -> [(BS.ByteString, BS.ByteString)] -> IO ()
writeEntriesWithProgress onProgress path entries = do
    fileExists <- doesFileExist path
    when fileExists (removeFile path)
    createDirectoryIfMissing True path
    entrySize <- entryMapSize entries >>= E.evaluate
    existingSize <- readMapSize path
    go False (max entrySize existingSize)
  where
    go replaced size = do
      res <- E.try $ withWriteTxn path size $ \txn dbi -> do
        LMDB.mdb_clear txn dbi
        putEntries txn dbi
      case res of
        Right () -> return ()
        Left err | isMapFull err -> go replaced (size * 2)
        Left err | isCorruptEnv err && not replaced -> do
          removePathForcibly path
          createDirectoryIfMissing True path
          go True size
        Left (err :: E.SomeException) -> E.throwIO err
    putEntries txn dbi = do
      let total = length entries
          step = max 1 (total `div` 20)
      if total <= 0
        then onProgress 1
        else forM_ (zip [(1 :: Int)..] entries) $ \(i, (k, v)) -> do
          putValue txn dbi k v
          when (i == total || i `mod` step == 0) $
            onProgress (fromIntegral i / fromIntegral total)

setReadableInterfacePermissions :: FilePath -> IO ()
setReadableInterfacePermissions path = do
    setFileMode path 0o755
    setFileMode (dataFilePath path) 0o644
    optional (setFileMode (lockFilePath path) 0o644)
  where
    optional :: IO () -> IO ()
    optional action =
        action `E.catch` \err ->
          if isDoesNotExistError err
            then return ()
            else E.throwIO (err :: E.IOException)

validateVersion :: LMDB.MDB_txn -> LMDB.MDB_dbi -> IO ()
validateVersion txn dbi = do
    vs <- getValue "version" txn dbi keyVersion
    unless (vs == A.version) (versionMismatch vs)

readMeta :: LMDB.MDB_txn -> LMDB.MDB_dbi -> IO TyMeta
readMeta txn dbi = do
    validateVersion txn dbi
    getValue "meta" txn dbi keyMeta

readModuleHashes :: FilePath -> IO (BS.ByteString, BS.ByteString, BS.ByteString)
readModuleHashes f =
    withReadTxn f $ \txn dbi -> do
      (_sourceMeta, moduleSrcBytesHash, modulePubHash, moduleImplHash) <- readMeta txn dbi
      traceTydbRead "module-hashes" f "meta"
      return (moduleSrcBytesHash, modulePubHash, moduleImplHash)

readNameEntries :: LMDB.MDB_txn -> LMDB.MDB_dbi -> IO ([(A.Name, I.NameInfo)], [NameHashInfo])
readNameEntries txn dbi = do
    te <- readNameInfoEntries txn dbi
    nameHashes <- readNameHashEntries txn dbi
    return (te, nameHashes)

readNameInfoEntries :: LMDB.MDB_txn -> LMDB.MDB_dbi -> IO [(A.Name, I.NameInfo)]
readNameInfoEntries txn dbi = do
    nameCount <- getValue "name-count" txn dbi keyNameCount
    forM [0 .. nameCount - 1] $ \i -> do
      suffix <- getValue ("name-order " ++ show i) txn dbi (keyNameOrder i)
      getValue ("name-info " ++ show i) txn dbi (keyNameInfoSuffix suffix)

readNameHashEntries :: LMDB.MDB_txn -> LMDB.MDB_dbi -> IO [NameHashInfo]
readNameHashEntries txn dbi = do
    vals <- getValuesWithPrefix txn dbi keyNameHashPrefix
    infos <- forM (zip [0..] vals) $ \(i, v) ->
      decodeStrict ("name-hash " ++ show (i :: Int)) v
    return (Data.List.sortOn (A.nstr . nhName) infos)

readStmtEntries :: LMDB.MDB_txn -> LMDB.MDB_dbi -> IO [A.Stmt]
readStmtEntries txn dbi = do
    count <- getValue "stmt-count" txn dbi keyStmtCount
    forM [0 .. count - 1] $ \i ->
      getValue ("stmt " ++ show i) txn dbi (keyStmt i)

emptyExtensionIndex :: ExtensionIndex
emptyExtensionIndex =
    ExtensionIndex
      { extByClass = Map.empty
      , extByProtocol = Map.empty
      }

extensionIndexFromNameInfo :: A.ModName -> I.NameInfo -> ExtensionIndex
extensionIndexFromNameInfo mn nmod =
    case nmod of
      I.NModule _ te _ -> foldl addExt emptyExtensionIndex te
      _ -> emptyExtensionIndex
  where
    addExt acc (ext, I.NExt _ c ps _ _ _) =
      let cls = localQName (A.tcname c)
          protos = [ p | (_, pcon) <- ps, Just p <- [localQName (A.tcname pcon)] ]
          withClass =
            case cls of
              Nothing -> acc
              Just n -> acc { extByClass = insertMany n [ext] (extByClass acc) }
          withProtos =
            foldl
              (\idx p -> idx { extByProtocol = insertMany p [ext] (extByProtocol idx) })
              withClass
              protos
      in withProtos
    addExt acc _ = acc

    localQName qn =
      case qn of
        A.NoQ n -> Just n
        A.QName m n | m == mn -> Just n
        A.GName m n | m == mn -> Just n
        _ -> Nothing

    insertMany n exts =
      Map.insertWith unionNames n (sortNames exts)

    unionNames xs ys = sortNames (xs ++ ys)
    sortNames = Data.List.sortOn A.nstr . Data.List.nub

extensionIndexEntries :: ExtensionIndex -> [(BS.ByteString, BS.ByteString)]
extensionIndexEntries index =
    [ (keyExtByClass n, encodeStrict (n, exts))
    | (n, exts) <- Map.toList (extByClass index)
    ]
    ++
    [ (keyExtByProtocol n, encodeStrict (n, exts))
    | (n, exts) <- Map.toList (extByProtocol index)
    ]

forceEntries :: [(BS.ByteString, BS.ByteString)] -> IO [(BS.ByteString, BS.ByteString)]
forceEntries entries = do
    E.evaluate (rnf entries)
    return entries

entryEncodeChunkMin :: Int
entryEncodeChunkMin = 256

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n ys = let (as, bs) = splitAt n ys in as : chunksOf n bs

entryChunks :: Int -> [a] -> [[a]]
entryChunks _ [] = []
entryChunks caps xs =
    chunksOf chunkSize xs
  where
    n = length xs
    jobs = min caps ((n + entryEncodeChunkMin - 1) `div` entryEncodeChunkMin)
    chunkSize = max 1 ((n + jobs - 1) `div` jobs)

parallelEntries :: (String -> IO ()) -> String -> (a -> [(BS.ByteString, BS.ByteString)]) -> [[a]] -> IO [(BS.ByteString, BS.ByteString)]
parallelEntries mark label mk chunks =
    concat <$> mapConcurrently encodeChunk chunks
  where
    encodeChunk chunk = do
      entries <- forceEntries (concatMap mk chunk)
      mark label
      return entries

stripExternalDeps :: NameHashInfo -> NameHashInfo
stripExternalDeps nh =
    nh { nhPubDeps = [], nhImplDeps = [] }

depIndexEntries :: [DepModuleInfo] -> [NameHashInfo] -> [(BS.ByteString, BS.ByteString)]
depIndexEntries depModules nameHashes =
    (keyDeps, encodeStrict depModules)
    : [ (keyDepModule mn, encodeStrict infos)
      | (mn, infos) <- moduleRows
      ]
    ++
    [ (keyDepName mn n, encodeStrict users)
    | ((mn, n), users) <- userRows
    ]
  where
    moduleNameKey = A.modPath
    nameKey = A.nstr
    sortModRows = Data.List.sortOn (moduleNameKey . fst)
    sortNameInfos = Data.List.sortOn (nameKey . dniName)
    sortUserRows = Data.List.sortOn (\((mn, n), _) -> (moduleNameKey mn, nameKey n))

    depTarget qn =
      case qn of
        A.GName m n -> Just (m, n)
        A.QName m n -> Just (m, n)
        A.NoQ{} -> Nothing

    addDep isPub owner acc (qn, h) =
      case depTarget qn of
        Nothing -> acc
        Just key' -> Map.alter (Just . addToEntry) key' acc
      where
        addToEntry Nothing =
          if isPub
            then (Just h, Nothing, [owner], [])
            else (Nothing, Just h, [], [owner])
        addToEntry (Just (pubH, implH, pubUsers, implUsers)) =
          if isPub
            then (Just h, implH, owner : pubUsers, implUsers)
            else (pubH, Just h, pubUsers, owner : implUsers)

    depMap =
      foldl' addInfo Map.empty nameHashes

    addInfo acc nh =
      let owner = nhName nh
          withPub = foldl' (addDep True owner) acc (nhPubDeps nh)
      in foldl' (addDep False owner) withPub (nhImplDeps nh)

    depNameInfo ((mn, n), (pubH, implH, _pubUsers, _implUsers)) =
      (mn, DepNameInfo n (maybe BS.empty id pubH) (maybe BS.empty id implH))

    moduleRows =
      [ (mn, sortNameInfos infos)
      | (mn, infos) <-
          sortModRows $
          Map.toList $
          Map.fromListWith (++)
            [ (mn, [info])
            | entry <- Map.toList depMap
            , let (mn, info) = depNameInfo entry
            ]
      ]

    cleanNames = Data.List.sortOn nameKey . Data.List.nub

    userRows =
      sortUserRows
        [ (key', DepUsers (cleanNames pubUsers) (cleanNames implUsers))
        | (key', (_pubH, _implH, pubUsers, implUsers)) <- Map.toList depMap
        ]

interfaceEntries :: (String -> Double -> IO ()) -> [Int] -> BS.ByteString -> BS.ByteString -> BS.ByteString -> Maybe SourceFileMeta -> [(A.ModName, BS.ByteString)] -> [DepModuleInfo] -> [NameHashInfo] -> [A.Name] -> [String] -> Maybe String -> I.NameInfo -> A.Module -> IO [(BS.ByteString, BS.ByteString)]
interfaceEntries onProgress version moduleSrcBytesHash modulePubHash moduleImplHash sourceMeta imps depModules nameHashes roots tests mdoc nmod tchecked = do
    caps <- getNumCapabilities
    let header =
          [ (keyVersion, encodeStrict version)
          , (keyMeta, encodeStrict (sourceMeta, moduleSrcBytesHash, modulePubHash, moduleImplHash))
          , (keyImports, encodeStrict imps)
          , (keyRoots, encodeStrict roots)
          , (keyTests, encodeStrict tests)
          , (keyDoc, encodeStrict mdoc)
          , (keyNameCount, encodeStrict (length te))
          , (keyStmtCount, encodeStrict (length body))
          , (keyModuleHeader, encodeStrict (tmn, timps, tdoc))
          ]
        nameChunks = entryChunks caps (zip [0..] te)
        nameHashChunks = entryChunks caps nameHashes
        stmtChunks = entryChunks caps (zip [0..] body)
        totalPrep = 3 + length nameChunks + length nameHashChunks + length stmtChunks
    prepDone <- newIORef (0 :: Int)
    let mark label = do
          done <- atomicModifyIORef' prepDone $ \n ->
            let n' = n + 1 in (n', n')
          onProgress label (fromIntegral done / fromIntegral totalPrep)
    onProgress "Preparing .tydb" 0
    headerEntries <- forceEntries header
    mark "Preparing .tydb header"
    depEntries <- forceEntries (depIndexEntries depModules nameHashes)
    mark "Preparing .tydb deps"
    nameEntries <- parallelEntries mark "Preparing .tydb names" nameInfoEntries nameChunks
    nameHashEntries <- parallelEntries mark "Preparing .tydb hashes" nameHashEntry nameHashChunks
    extEntries <- forceEntries (extensionIndexEntries (extensionIndexFromNameInfo tmn nmod))
    mark "Preparing .tydb extensions"
    stmtEntries <- parallelEntries mark "Preparing .tydb statements" stmtEntry stmtChunks
    return (headerEntries ++ depEntries ++ nameEntries ++ nameHashEntries ++ extEntries ++ stmtEntries)
  where
    I.NModule _ te _ = nmod
    A.Module tmn timps tdoc body = tchecked
    nameInfoEntries (i, (n, info)) =
      [ (keyNameOrder i, encodeStrict suffix)
      , (keyNameInfoSuffix suffix, encodeStrict (n, info))
      ]
      where
        suffix = nameKeySuffix n
    nameHashEntry nh = [(keyNameHash (nhName nh), encodeStrict (stripExternalDeps nh))]
    stmtEntry (i, stmt) = [(keyStmt i, encodeStrict stmt)]

writeFile :: FilePath -> BS.ByteString -> BS.ByteString -> BS.ByteString -> Maybe SourceFileMeta -> [(A.ModName, BS.ByteString)] -> [DepModuleInfo] -> [NameHashInfo] -> [A.Name] -> [String] -> Maybe String -> I.NameInfo -> A.Module -> IO ()
writeFile = writeFileWithVersion A.version

writeFileWithVersion :: [Int] -> FilePath -> BS.ByteString -> BS.ByteString -> BS.ByteString -> Maybe SourceFileMeta -> [(A.ModName, BS.ByteString)] -> [DepModuleInfo] -> [NameHashInfo] -> [A.Name] -> [String] -> Maybe String -> I.NameInfo -> A.Module -> IO ()
writeFileWithVersion version f moduleSrcBytesHash modulePubHash moduleImplHash sourceMeta imps depModules nameHashes roots tests mdoc nmod tchecked =
    writeFileWithProgress (\_ -> return ()) version f moduleSrcBytesHash modulePubHash moduleImplHash sourceMeta imps depModules nameHashes roots tests mdoc nmod tchecked

writeFileWithProgress :: (TyDbWriteProgress -> IO ()) -> [Int] -> FilePath -> BS.ByteString -> BS.ByteString -> BS.ByteString -> Maybe SourceFileMeta -> [(A.ModName, BS.ByteString)] -> [DepModuleInfo] -> [NameHashInfo] -> [A.Name] -> [String] -> Maybe String -> I.NameInfo -> A.Module -> IO ()
writeFileWithProgress onProgress version f moduleSrcBytesHash modulePubHash moduleImplHash sourceMeta imps depModules nameHashes roots tests mdoc nmod tchecked = do
    entries <- interfaceEntries prepProgress version moduleSrcBytesHash modulePubHash moduleImplHash sourceMeta imps depModules nameHashes roots tests mdoc nmod tchecked
    writeProgress 0
    writeEntriesWithProgress writeProgress f entries
  where
    prepShare = 0.80
    writeShare = 0.20
    prepProgress label frac =
      onProgress (TyDbWriteProgress label (prepShare * max 0 (min 1 frac)))
    writeProgress frac =
      onProgress (TyDbWriteProgress "Writing .tydb" (prepShare + writeShare * max 0 (min 1 frac)))

readFile :: FilePath -> IO TyFile
readFile f =
    withReadTxn f $ \txn dbi -> do
      (sourceMeta, moduleSrcBytesHash, modulePubHash, moduleImplHash) <- readMeta txn dbi
      imps <- getValue "imports" txn dbi keyImports
      depModules <- getValue "deps" txn dbi keyDeps
      roots <- getValue "roots" txn dbi keyRoots
      tests <- getValue "tests" txn dbi keyTests
      mdoc <- getValue "doc" txn dbi keyDoc
      (te, nameHashes) <- readNameEntries txn dbi
      (tmn, timps, tdoc) <- getValue "module-header" txn dbi keyModuleHeader
      stmts <- readStmtEntries txn dbi
      traceTydbRead "all" f ("names " ++ show (length te) ++ " stmts " ++ show (length stmts))
      let tmod = A.Module tmn timps tdoc stmts
          sourceImps = A.importsOf tmod
          nmod = I.NModule sourceImps te mdoc
      return (sourceImps, nmod, tmod, sourceMeta, moduleSrcBytesHash, modulePubHash, moduleImplHash, imps, depModules, nameHashes, roots, tests, mdoc)

-- Read only cached header fields from .tydb. This avoids decoding the large
-- NameInfo and typed Module statement sections and is much faster than readFile
-- for freshness checks and dependency discovery.
readHeader :: FilePath -> IO TyHeader
readHeader f =
    withReadTxn f $ \txn dbi -> do
      (sourceMeta, moduleSrcBytesHash, modulePubHash, moduleImplHash) <- readMeta txn dbi
      imps <- getValue "imports" txn dbi keyImports
      depModules <- getValue "deps" txn dbi keyDeps
      roots <- getValue "roots" txn dbi keyRoots
      tests <- getValue "tests" txn dbi keyTests
      doc <- getValue "doc" txn dbi keyDoc
      nameHashes <- readNameHashEntries txn dbi
      traceTydbRead "header" f "cached"
      traceTydbRead "name-hash-all" f (show (length nameHashes))
      return (sourceMeta, moduleSrcBytesHash, modulePubHash, moduleImplHash, imps, depModules, nameHashes, roots, tests, doc)

readDepNames :: FilePath -> A.ModName -> IO [DepNameInfo]
readDepNames f mn =
    withReadTxn f $ \txn dbi -> do
      validateVersion txn dbi
      mDeps <- getMaybeValue ("deps/" ++ Data.List.intercalate "." (A.modPath mn)) txn dbi (keyDepModule mn)
      let deps = maybe [] id mDeps
      traceTydbRead "dep-names" f (Data.List.intercalate "." (A.modPath mn) ++ " " ++ show (length deps))
      return deps

readDepUsers :: FilePath -> A.ModName -> A.Name -> IO DepUsers
readDepUsers f mn n =
    withReadTxn f $ \txn dbi -> do
      validateVersion txn dbi
      mUsers <- getMaybeValue ("deps/" ++ Data.List.intercalate "." (A.modPath mn) ++ "/" ++ A.rawstr n) txn dbi (keyDepName mn n)
      traceTydbRead "dep-users" f (Data.List.intercalate "." (A.modPath mn) ++ "." ++ A.rawstr n)
      return (maybe emptyDepUsers id mUsers)

readNameHash :: FilePath -> A.Name -> IO (Maybe NameHashInfo)
readNameHash f n =
    withReadTxn f $ \txn dbi -> do
      validateVersion txn dbi
      mInfo <- getMaybeValue ("name-hash/" ++ A.rawstr n) txn dbi (keyNameHash n)
      traceTydbRead (case mInfo of { Just _ -> "name-hash"; Nothing -> "name-hash-miss" }) f (A.rawstr n)
      return mInfo

readNameHashMaybe :: FilePath -> A.Name -> IO (Maybe NameHashInfo)
readNameHashMaybe f n =
    readNameHash f n `E.catch` tyCacheMiss

readModuleHashesMaybe :: FilePath -> IO (Maybe (BS.ByteString, BS.ByteString, BS.ByteString))
readModuleHashesMaybe =
    readTyMaybe readModuleHashes

readExtensionsByClass :: FilePath -> A.Name -> IO [A.Name]
readExtensionsByClass f n =
    withReadTxn f $ \txn dbi -> do
      validateVersion txn dbi
      entry <- getMaybeValue "ext-by-class" txn dbi (keyExtByClass n) :: IO (Maybe (A.Name, [A.Name]))
      return $ case entry of
        Nothing -> []
        Just (_, exts) -> exts

readExtensionsByProtocol :: FilePath -> A.Name -> IO [A.Name]
readExtensionsByProtocol f n =
    withReadTxn f $ \txn dbi -> do
      validateVersion txn dbi
      entry <- getMaybeValue "ext-by-protocol" txn dbi (keyExtByProtocol n) :: IO (Maybe (A.Name, [A.Name]))
      return $ case entry of
        Nothing -> []
        Just (_, exts) -> exts

-- Interface files are caches for most callers. If a file is missing,
-- unreadable, corrupt, or from a different compiler interface version, the
-- cache entry is not usable.
readFileMaybe :: FilePath -> IO (Maybe TyFile)
readFileMaybe = readTyMaybe readFile

readHeaderMaybe :: FilePath -> IO (Maybe TyHeader)
readHeaderMaybe = readTyMaybe readHeader

readTyMaybe :: (FilePath -> IO a) -> FilePath -> IO (Maybe a)
readTyMaybe readTy f = (Just <$> readTy f) `E.catch` tyCacheMiss

tyCacheMiss :: E.SomeException -> IO (Maybe a)
tyCacheMiss err
  | Just (_ :: E.SomeAsyncException) <- E.fromException err = E.throwIO err
  | isTyCacheMiss err = return Nothing
  | otherwise = E.throwIO err

-- A read failure counts as a cache miss only when the entry is genuinely absent
-- or structurally unusable, in which case the caller recompiles from source
-- (which overwrites a corrupt entry, see writeEntries). Transient or
-- environmental errors are NOT misses: they propagate so the real cause is
-- surfaced instead of being silently reported as "interface missing".
isTyCacheMiss :: E.SomeException -> Bool
isTyCacheMiss err
  | Just (_ :: TyCacheInvalid) <- E.fromException err = True
  | Just (ioe :: E.IOException) <- E.fromException err = isDoesNotExistError ioe
  | isCorruptEnv err = True
  | otherwise = False
