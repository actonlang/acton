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
-- encoded with Data.Binary. Where the old .ty format wrote one blob in a fixed
-- order, here every field is an independently addressable key.
--
--   Module-level keys (one each):
--     "version"        :: [Int]                       -- Acton.Syntax.version
--     "meta"           :: ( Maybe SourceFileMeta      -- cached source stat metadata
--                         , ByteString                -- moduleSrcBytesHash: SHA-256 of raw source
--                         , ByteString                -- modulePubHash: SHA-256 of public NameInfo
--                                                     --   (doc-free) + imports' pub hashes
--                         , ByteString )              -- moduleImplHash: SHA-256 of per-name impl hashes
--     "imports"        :: [(A.ModName, ByteString)]   -- dependency module and pub hash used
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
--     "name-hash/<suffix>" :: NameHashInfo            -- per-name src/pub/impl hashes + local/external deps
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
  , readFile
  , readHeader
  , readExtensionsByClass
  , readExtensionsByProtocol
  , readFileMaybe
  , readHeaderMaybe
  , writeFile
  , writeFileWithVersion
  ) where

import Prelude hiding (readFile, writeFile)
import Control.DeepSeq (NFData)
import Data.Binary
import qualified Control.Exception as E
import Control.Concurrent (runInBoundThread, threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, withMVar)
import Control.Monad (forM, forM_, unless, when)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import qualified Data.List
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Lazy as BL
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
import System.FilePath ((</>), takeExtension)
import System.IO.Error (isDoesNotExistError)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Files (fileAccess, getFileStatus, modificationTimeHiRes, setFileMode)

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

instance Binary NameHashInfo
instance NFData NameHashInfo

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

instance Binary SourceFileMeta

type TyFile =
  ( [A.ModName]
  , I.NameInfo
  , A.Module
  , Maybe SourceFileMeta
  , BS.ByteString
  , BS.ByteString
  , BS.ByteString
  , [(A.ModName, BS.ByteString)]
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

-- LMDB does not support opening the same environment twice in one process, so
-- keep one in-process lock per .tydb path while an environment handle is open.
interfaceLocks :: MVar (Map.Map FilePath (MVar ()))
{-# NOINLINE interfaceLocks #-}
interfaceLocks = unsafePerformIO (newMVar Map.empty)

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

decodeStrict :: Binary a => String -> BS.ByteString -> IO a
decodeStrict label bs =
    case decodeOrFail (BL.fromStrict bs) of
      Left (_, _, err) -> E.throwIO (TyCacheInvalid ("Failed to decode .tydb " ++ label ++ ": " ++ err))
      Right (_, _, v) -> return v

encodeStrict :: Binary a => a -> BS.ByteString
encodeStrict = BL.toStrict . encode

key :: String -> BS.ByteString
key = B.pack

keyVersion, keyMeta, keyImports, keyRoots, keyTests, keyDoc, keyNameCount, keyStmtCount, keyModuleHeader :: BS.ByteString
keyVersion      = key "version"
keyMeta         = key "meta"
keyImports      = key "imports"
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

nameKeySuffix :: A.Name -> BS.ByteString
nameKeySuffix n =
    let raw = TE.encodeUtf8 (T.pack (A.rawstr n))
    in if plainNameKey raw
         then B.concat [key "p/", raw]
         else B.concat [key "h/", Base16.encode (SHA256.hash raw)]
  where
    plainNameKey raw =
        BS.length raw <= plainNameKeyLimit && BS.all safe raw
    safe w =
        w > 32 && w < 127 && w /= 47

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
withEnv path readOnly mapSize action =
    withInterfaceLock path $
      E.bracket open LMDB.mdb_env_close action
  where
    open = withMVar lmdbOpenLock $ \_ -> openWithRetry envOpenMaxAttempts
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
          | attempt > 1 && isTransientOpenError err ->
              threadDelay envOpenRetryDelayUs >> openWithRetry (attempt - 1)
          | otherwise -> E.throwIO err
    openEnv env
      | readOnly  = do
          flags <- readOnlyOpenFlags path
          LMDB.mdb_env_open env path flags
      | otherwise = LMDB.mdb_env_open env path []

-- A transient env-open failure is an OS-level error (Either Left, a raw errno)
-- from a concurrent lock.mdb setup race -- retrying succeeds once the winning
-- opener has created it. LMDB-semantic errors (Either Right MDB_*) are not
-- transient and must not be retried.
isTransientOpenError :: LMDB.LMDB_Error -> Bool
isTransientOpenError err =
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

envOpenMaxAttempts :: Int
envOpenMaxAttempts = 5

envOpenRetryDelayUs :: Int
envOpenRetryDelayUs = 2000

withInterfaceLock :: FilePath -> IO a -> IO a
withInterfaceLock path action = do
    cpath <- canonicalizePath path
    lock <- modifyMVar interfaceLocks $ \locks ->
      case Map.lookup cpath locks of
        Just lock -> return (locks, lock)
        Nothing -> do
          lock <- newMVar ()
          return (Map.insert cpath lock locks, lock)
    withMVar lock $ \_ -> action

withReadTxn :: FilePath -> (LMDB.MDB_txn -> LMDB.MDB_dbi -> IO a) -> IO a
withReadTxn path action = do
    mapSize <- readMapSize path
    runInLmdbThread $
      withEnv path True mapSize $ \env ->
        E.bracket (LMDB.mdb_txn_begin env Nothing True) LMDB.mdb_txn_abort $ \txn -> do
          dbi <- LMDB.mdb_dbi_open txn Nothing []
          action txn dbi

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

getValue :: Binary a => String -> LMDB.MDB_txn -> LMDB.MDB_dbi -> BS.ByteString -> IO a
getValue label txn dbi k = do
    mv <- withVal k (LMDB.mdb_get txn dbi)
    case mv of
      Nothing -> E.throwIO (TyCacheInvalid ("Missing .tydb key: " ++ label))
      Just v -> copyVal v >>= decodeStrict label

getMaybeValue :: Binary a => String -> LMDB.MDB_txn -> LMDB.MDB_dbi -> BS.ByteString -> IO (Maybe a)
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
writeEntries path entries = do
    fileExists <- doesFileExist path
    when fileExists (removeFile path)
    createDirectoryIfMissing True path
    entrySize <- entryMapSize entries
    existingSize <- readMapSize path
    go False (max entrySize existingSize)
  where
    go replaced size = do
      res <- E.try $ withWriteTxn path size $ \txn dbi -> do
        LMDB.mdb_clear txn dbi
        forM_ entries $ \(k, v) -> putValue txn dbi k v
      case res of
        Right () -> return ()
        Left err | isMapFull err -> go replaced (size * 2)
        Left err | isCorruptEnv err && not replaced -> do
          removePathForcibly path
          createDirectoryIfMissing True path
          go True size
        Left (err :: E.SomeException) -> E.throwIO err

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

interfaceEntries :: [Int] -> BS.ByteString -> BS.ByteString -> BS.ByteString -> Maybe SourceFileMeta -> [(A.ModName, BS.ByteString)] -> [NameHashInfo] -> [A.Name] -> [String] -> Maybe String -> I.NameInfo -> A.Module -> [(BS.ByteString, BS.ByteString)]
interfaceEntries version moduleSrcBytesHash modulePubHash moduleImplHash sourceMeta imps nameHashes roots tests mdoc nmod tchecked =
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
    ++ concat [ [ (keyNameOrder i, encodeStrict suffix)
                , (keyNameInfoSuffix suffix, encodeStrict (n, info))
                ]
              | (i, (n, info)) <- zip [0..] te
              , let suffix = nameKeySuffix n
              ]
    ++ [ (keyNameHash (nhName nh), encodeStrict nh) | nh <- nameHashes ]
    ++ extensionIndexEntries (extensionIndexFromNameInfo tmn nmod)
    ++ [ (keyStmt i, encodeStrict stmt) | (i, stmt) <- zip [0..] body ]
  where
    I.NModule _ te _ = nmod
    A.Module tmn timps tdoc body = tchecked

writeFile :: FilePath -> BS.ByteString -> BS.ByteString -> BS.ByteString -> Maybe SourceFileMeta -> [(A.ModName, BS.ByteString)] -> [NameHashInfo] -> [A.Name] -> [String] -> Maybe String -> I.NameInfo -> A.Module -> IO ()
writeFile = writeFileWithVersion A.version

writeFileWithVersion :: [Int] -> FilePath -> BS.ByteString -> BS.ByteString -> BS.ByteString -> Maybe SourceFileMeta -> [(A.ModName, BS.ByteString)] -> [NameHashInfo] -> [A.Name] -> [String] -> Maybe String -> I.NameInfo -> A.Module -> IO ()
writeFileWithVersion version f moduleSrcBytesHash modulePubHash moduleImplHash sourceMeta imps nameHashes roots tests mdoc nmod tchecked =
    writeEntries f (interfaceEntries version moduleSrcBytesHash modulePubHash moduleImplHash sourceMeta imps nameHashes roots tests mdoc nmod tchecked)

readFile :: FilePath -> IO TyFile
readFile f =
    withReadTxn f $ \txn dbi -> do
      (sourceMeta, moduleSrcBytesHash, modulePubHash, moduleImplHash) <- readMeta txn dbi
      imps <- getValue "imports" txn dbi keyImports
      roots <- getValue "roots" txn dbi keyRoots
      tests <- getValue "tests" txn dbi keyTests
      mdoc <- getValue "doc" txn dbi keyDoc
      (te, nameHashes) <- readNameEntries txn dbi
      (tmn, timps, tdoc) <- getValue "module-header" txn dbi keyModuleHeader
      stmts <- readStmtEntries txn dbi
      let tmod = A.Module tmn timps tdoc stmts
          sourceImps = A.importsOf tmod
          nmod = I.NModule sourceImps te mdoc
      return (sourceImps, nmod, tmod, sourceMeta, moduleSrcBytesHash, modulePubHash, moduleImplHash, imps, nameHashes, roots, tests, mdoc)

-- Read only cached header fields from .tydb. This avoids decoding the large
-- NameInfo and typed Module statement sections and is much faster than readFile
-- for freshness checks and dependency discovery.
readHeader :: FilePath -> IO TyHeader
readHeader f =
    withReadTxn f $ \txn dbi -> do
      (sourceMeta, moduleSrcBytesHash, modulePubHash, moduleImplHash) <- readMeta txn dbi
      imps <- getValue "imports" txn dbi keyImports
      roots <- getValue "roots" txn dbi keyRoots
      tests <- getValue "tests" txn dbi keyTests
      doc <- getValue "doc" txn dbi keyDoc
      nameHashes <- readNameHashEntries txn dbi
      return (sourceMeta, moduleSrcBytesHash, modulePubHash, moduleImplHash, imps, nameHashes, roots, tests, doc)

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
