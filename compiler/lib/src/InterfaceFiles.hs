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
-- On-disk layout (LMDB directory)
--   meta keys: version, source metadata, module src/pub/impl hashes, imports,
--              roots, tests, docstring, and typed module header
--   name keys: one semantic-name key per NameInfo and NameHashInfo entry, plus
--              explicit order keys for full TEnv reconstruction
--   stmt keys: one ordered key per typed top-level statement
--
-- Rationale for ordering
-- - Keep metadata separate so callers can validate and reuse a cache entry
--   without decoding the large NameInfo and typed Module sections.
-- - Store NameInfo and typed module bodies as ordered entries so later work can
--   move from full reconstruction to selective lookup without changing the
--   outer cache boundary.

module InterfaceFiles
  ( NameHashInfo(..)
  , TypeIndexKind(..)
  , TypeIndexInfo(..)
  , SourceFileMeta(..)
  , TyFile
  , TyHeader
  , InterfaceDB
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
  , readFileMaybe
  , readHeaderMaybe
  , openInterfaceDB
  , closeInterfaceDB
  , readInterfaceDBImportInfo
  , readInterfaceDBNameInfoMaybe
  , readInterfaceDBNameInfoEntries
  , readInterfaceDBTypeIndexEntries
  , writeFile
  , writeFileWithVersion
  ) where

import Prelude hiding (readFile, writeFile)
import Data.Binary
import qualified Control.Exception as E
import Control.Concurrent (runInBoundThread)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, withMVar)
import Control.Monad (forM, forM_, unless, when)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import qualified Data.List
import Data.Maybe (mapMaybe)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime)
import qualified Database.LMDB.Raw as LMDB
import qualified Acton.Syntax as A
import qualified Acton.NameInfo as I
import qualified Acton.Names as Names
import Foreign.Ptr (castPtr)
import Foreign.Storable (peek)
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getFileSize, getModificationTime, listDirectory, removeFile, removePathForcibly)
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeExtension)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.Weak (addFinalizer)
import System.Posix.Files (getFileStatus, modificationTimeHiRes)

data NameHashInfo = NameHashInfo
  { nhName     :: A.Name
  , nhSrcHash  :: BS.ByteString
  , nhPubHash  :: BS.ByteString
  , nhImplHash :: BS.ByteString
  , nhPubDeps  :: [(A.QName, BS.ByteString)]
  , nhImplDeps :: [(A.QName, BS.ByteString)]
  } deriving (Show, Eq, Generic)

instance Binary NameHashInfo

data TypeIndexKind = TypeIndexActor | TypeIndexClass | TypeIndexProto
  deriving (Show, Eq, Generic)

instance Binary TypeIndexKind

data TypeIndexInfo = TypeIndexInfo
  { tiiName  :: A.Name
  , tiiKind  :: TypeIndexKind
  , tiiBinds :: A.QBinds
  , tiiAttrs :: [A.Name]
  } deriving (Show, Eq, Generic)

instance Binary TypeIndexInfo

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

data InterfaceDB = InterfaceDB FilePath LMDB.MDB_env (MVar Bool)

instance Show InterfaceDB where
    show (InterfaceDB path _ _) = "InterfaceDB " ++ path

-- Note: tests are stored in the header to support listing without compiling
--       or executing test binaries.

interfaceExt :: String
interfaceExt = ".tydb"

interfacePath :: FilePath -> A.ModName -> FilePath
interfacePath root mn = foldl (</>) root (A.modPath mn) ++ interfaceExt

dataFilePath :: FilePath -> FilePath
dataFilePath path = path </> "data.mdb"

-- The raw LMDB binding can race when several threads open the same environment
-- at once; serialize only env opening, not the read transactions themselves.
lmdbOpenLock :: MVar ()
{-# NOINLINE lmdbOpenLock #-}
lmdbOpenLock = unsafePerformIO (newMVar ())

traceNameReads :: Bool
{-# NOINLINE traceNameReads #-}
traceNameReads = unsafePerformIO $ maybe False (not . null) <$> lookupEnv "ACTON_TYDB_TRACE_READS"

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

versionMismatch :: [Int] -> IO a
versionMismatch vs =
    ioError (userError (".tydb version mismatch: file has " ++ show vs ++ ", expected " ++ show A.version))

decodeStrict :: Binary a => String -> BS.ByteString -> IO a
decodeStrict label bs =
    case decodeOrFail (BL.fromStrict bs) of
      Left (_, _, err) -> ioError (userError ("Failed to decode .tydb " ++ label ++ ": " ++ err))
      Right (_, _, v) -> return v

encodeStrict :: Binary a => a -> BS.ByteString
encodeStrict = BL.toStrict . encode

key :: String -> BS.ByteString
key = B.pack

keyVersion, keyMeta, keyImports, keyRoots, keyTests, keyDoc, keyNameCount, keyTypeIndexCount, keyStmtCount, keyModuleHeader :: BS.ByteString
keyVersion      = key "version"
keyMeta         = key "meta"
keyImports      = key "imports"
keyRoots        = key "roots"
keyTests        = key "tests"
keyDoc          = key "doc"
keyNameCount    = key "name-count"
keyTypeIndexCount = key "type-index-count"
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

keyTypeIndexPrefix :: BS.ByteString
keyTypeIndexPrefix = key "type-index/"

keyTypeIndex :: A.Name -> BS.ByteString
keyTypeIndex n = B.concat [keyTypeIndexPrefix, nameKeySuffix n]

keyStmt :: Int -> BS.ByteString
keyStmt i = B.pack ("stmt/" ++ padIndex i)

withVal :: BS.ByteString -> (LMDB.MDB_val -> IO a) -> IO a
withVal bs f =
    BS.useAsCStringLen bs $ \(ptr, len) ->
      f (LMDB.MDB_val (fromIntegral len) (castPtr ptr))

copyVal :: LMDB.MDB_val -> IO BS.ByteString
copyVal (LMDB.MDB_val len ptr) =
    BS.packCStringLen (castPtr ptr, fromIntegral len)

openEnv :: FilePath -> Bool -> Int -> IO LMDB.MDB_env
openEnv path readOnly mapSize =
    withMVar lmdbOpenLock $ \_ -> do
      env <- LMDB.mdb_env_create
      (do LMDB.mdb_env_set_mapsize env mapSize
          LMDB.mdb_env_open env path (if readOnly then [LMDB.MDB_RDONLY] else [])
          return env)
        `E.onException` LMDB.mdb_env_close env

withEnv :: FilePath -> Bool -> Int -> (LMDB.MDB_env -> IO a) -> IO a
withEnv path readOnly mapSize action =
    E.bracket (openEnv path readOnly mapSize) LMDB.mdb_env_close action

withReadTxn :: FilePath -> (LMDB.MDB_txn -> LMDB.MDB_dbi' -> IO a) -> IO a
withReadTxn path action = do
    mapSize <- readMapSize path
    runInLmdbThread $
      withEnv path True mapSize $ \env ->
        E.bracket (LMDB.mdb_txn_begin env Nothing True) LMDB.mdb_txn_abort $ \txn -> do
          dbi <- LMDB.mdb_dbi_open' txn Nothing []
          action txn dbi

withWriteTxn :: FilePath -> Int -> (LMDB.MDB_txn -> LMDB.MDB_dbi' -> IO a) -> IO a
withWriteTxn path mapSize action =
    runInLmdbThread $
      withEnv path False mapSize $ \env -> do
        txn <- LMDB.mdb_txn_begin env Nothing False
        r <- (LMDB.mdb_dbi_open' txn Nothing [] >>= action txn) `E.onException` LMDB.mdb_txn_abort txn
        LMDB.mdb_txn_commit txn
        return r

openInterfaceDB :: FilePath -> IO InterfaceDB
openInterfaceDB path = do
    mapSize <- readMapSize path
    env <- runInLmdbThread $ openEnv path True mapSize
    closed <- newMVar False
    let db = InterfaceDB path env closed
        close = closeInterfaceDBEnv env closed
    addFinalizer env close
    (withInterfaceDBReadTxn db $ \txn dbi -> do
      validateVersion txn dbi) `E.onException` close
    return db

closeInterfaceDB :: InterfaceDB -> IO ()
closeInterfaceDB (InterfaceDB _ env closed) = closeInterfaceDBEnv env closed

closeInterfaceDBEnv :: LMDB.MDB_env -> MVar Bool -> IO ()
closeInterfaceDBEnv env closed =
    modifyMVar_ closed $ \done -> do
      unless done (LMDB.mdb_env_close env)
      return True

withInterfaceDBReadTxn :: InterfaceDB -> (LMDB.MDB_txn -> LMDB.MDB_dbi' -> IO a) -> IO a
withInterfaceDBReadTxn (InterfaceDB _ env _) action =
    runInLmdbThread $
      E.bracket (LMDB.mdb_txn_begin env Nothing True) LMDB.mdb_txn_abort $ \txn -> do
        dbi <- LMDB.mdb_dbi_open' txn Nothing []
        action txn dbi

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

getValue :: Binary a => String -> LMDB.MDB_txn -> LMDB.MDB_dbi' -> BS.ByteString -> IO a
getValue label txn dbi k = do
    mv <- withVal k (LMDB.mdb_get' txn dbi)
    case mv of
      Nothing -> ioError (userError ("Missing .tydb key: " ++ label))
      Just v -> copyVal v >>= decodeStrict label

getValueMaybe :: Binary a => String -> LMDB.MDB_txn -> LMDB.MDB_dbi' -> BS.ByteString -> IO (Maybe a)
getValueMaybe label txn dbi k = do
    mv <- withVal k (LMDB.mdb_get' txn dbi)
    case mv of
      Nothing -> return Nothing
      Just v -> Just <$> (copyVal v >>= decodeStrict label)

getValuesWithPrefix :: LMDB.MDB_txn -> LMDB.MDB_dbi' -> BS.ByteString -> IO [BS.ByteString]
getValuesWithPrefix txn dbi prefix =
    E.bracket (LMDB.mdb_cursor_open' txn dbi) LMDB.mdb_cursor_close' $ \cursor ->
      withVal prefix $ \start ->
        withVal BS.empty $ \empty ->
          LMDB.withKVPtrs start empty $ \kp vp -> do
            found <- LMDB.mdb_cursor_get' LMDB.MDB_SET_RANGE cursor kp vp
            go cursor kp vp found
  where
    go _ _ _ False = return []
    go cursor kp vp True = do
      k <- peek kp >>= copyVal
      if not (prefix `BS.isPrefixOf` k)
        then return []
        else do
          v <- peek vp >>= copyVal
          found <- LMDB.mdb_cursor_get' LMDB.MDB_NEXT cursor kp vp
          (v :) <$> go cursor kp vp found

putValue :: LMDB.MDB_txn -> LMDB.MDB_dbi' -> BS.ByteString -> BS.ByteString -> IO ()
putValue txn dbi k v =
    withVal k $ \kv ->
      withVal v $ \vv -> do
        _ <- LMDB.mdb_put' (LMDB.compileWriteFlags []) txn dbi kv vv
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
        LMDB.mdb_clear' txn dbi
        forM_ entries $ \(k, v) -> putValue txn dbi k v
      case res of
        Right () -> return ()
        Left err | isMapFull err -> go replaced (size * 2)
        Left err | isCorruptEnv err && not replaced -> do
          removePathForcibly path
          createDirectoryIfMissing True path
          go True size
        Left (err :: E.SomeException) -> E.throwIO err

validateVersion :: LMDB.MDB_txn -> LMDB.MDB_dbi' -> IO ()
validateVersion txn dbi = do
    vs <- getValue "version" txn dbi keyVersion
    unless (vs == A.version) (versionMismatch vs)

readMeta :: LMDB.MDB_txn -> LMDB.MDB_dbi' -> IO TyMeta
readMeta txn dbi = do
    validateVersion txn dbi
    getValue "meta" txn dbi keyMeta

readNameEntries :: LMDB.MDB_txn -> LMDB.MDB_dbi' -> IO ([(A.Name, I.NameInfo)], [NameHashInfo])
readNameEntries txn dbi = do
    te <- readNameInfoEntries txn dbi
    nameHashes <- readNameHashEntries txn dbi
    return (te, nameHashes)

readNameInfoEntries :: LMDB.MDB_txn -> LMDB.MDB_dbi' -> IO [(A.Name, I.NameInfo)]
readNameInfoEntries txn dbi = do
    nameCount <- getValue "name-count" txn dbi keyNameCount
    forM [0 .. nameCount - 1] $ \i -> do
      suffix <- getValue ("name-order " ++ show i) txn dbi (keyNameOrder i)
      getValue ("name-info " ++ show i) txn dbi (keyNameInfoSuffix suffix)

readInterfaceDBImportInfo :: InterfaceDB -> IO ([(A.ModName, BS.ByteString)], Maybe String)
readInterfaceDBImportInfo db =
    withInterfaceDBReadTxn db $ \txn dbi -> do
      _ <- readMeta txn dbi
      imps <- getValue "imports" txn dbi keyImports
      doc <- getValue "doc" txn dbi keyDoc
      return (imps, doc)

readInterfaceDBNameInfoMaybe :: InterfaceDB -> A.Name -> IO (Maybe (A.Name, I.NameInfo))
readInterfaceDBNameInfoMaybe db@(InterfaceDB path _ _) n = do
    mi <- withInterfaceDBReadTxn db $ \txn dbi -> do
      getValueMaybe ("name-info " ++ A.nstr n) txn dbi (keyNameInfo n)
    when traceNameReads $
      hPutStrLn stderr ("tydb-read " ++ hit mi ++ " " ++ path ++ " " ++ A.nstr n)
    return mi
  where hit Nothing = "miss"
        hit Just{}  = "hit"

readInterfaceDBNameInfoEntries :: InterfaceDB -> IO [(A.Name, I.NameInfo)]
readInterfaceDBNameInfoEntries db@(InterfaceDB path _ _) = do
    entries <- withInterfaceDBReadTxn db readNameInfoEntries
    when traceNameReads $
      hPutStrLn stderr ("tydb-read-all " ++ path ++ " " ++ show (length entries))
    return entries

readInterfaceDBTypeIndexEntries :: InterfaceDB -> IO [TypeIndexInfo]
readInterfaceDBTypeIndexEntries db@(InterfaceDB path _ _) = do
    entries <- withInterfaceDBReadTxn db $ \txn dbi -> do
      present <- getValueMaybe "type-index-count" txn dbi keyTypeIndexCount
      case (present :: Maybe Int) of
        Just _  -> readTypeIndexEntries txn dbi
        Nothing -> typeIndex <$> readNameInfoEntries txn dbi
    when traceNameReads $
      hPutStrLn stderr ("tydb-read-type-index " ++ path ++ " " ++ show (length entries))
    return entries

readNameHashEntries :: LMDB.MDB_txn -> LMDB.MDB_dbi' -> IO [NameHashInfo]
readNameHashEntries txn dbi = do
    vals <- getValuesWithPrefix txn dbi keyNameHashPrefix
    infos <- forM (zip [0..] vals) $ \(i, v) ->
      decodeStrict ("name-hash " ++ show (i :: Int)) v
    return (Data.List.sortOn (A.nstr . nhName) infos)

readTypeIndexEntries :: LMDB.MDB_txn -> LMDB.MDB_dbi' -> IO [TypeIndexInfo]
readTypeIndexEntries txn dbi = do
    vals <- getValuesWithPrefix txn dbi keyTypeIndexPrefix
    infos <- forM (zip [0..] vals) $ \(i, v) ->
      decodeStrict ("type-index " ++ show (i :: Int)) v
    return (Data.List.sortOn (A.nstr . tiiName) infos)

readStmtEntries :: LMDB.MDB_txn -> LMDB.MDB_dbi' -> IO [A.Stmt]
readStmtEntries txn dbi = do
    count <- getValue "stmt-count" txn dbi keyStmtCount
    forM [0 .. count - 1] $ \i ->
      getValue ("stmt " ++ show i) txn dbi (keyStmt i)

interfaceEntries :: [Int] -> BS.ByteString -> BS.ByteString -> BS.ByteString -> Maybe SourceFileMeta -> [(A.ModName, BS.ByteString)] -> [NameHashInfo] -> [A.Name] -> [String] -> Maybe String -> I.NameInfo -> A.Module -> [(BS.ByteString, BS.ByteString)]
interfaceEntries version moduleSrcBytesHash modulePubHash moduleImplHash sourceMeta imps nameHashes roots tests mdoc nmod tchecked =
    [ (keyVersion, encodeStrict version)
    , (keyMeta, encodeStrict (sourceMeta, moduleSrcBytesHash, modulePubHash, moduleImplHash))
    , (keyImports, encodeStrict imps)
    , (keyRoots, encodeStrict roots)
    , (keyTests, encodeStrict tests)
    , (keyDoc, encodeStrict mdoc)
    , (keyNameCount, encodeStrict (length te))
    , (keyTypeIndexCount, encodeStrict (length typeEntries))
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
    ++ [ (keyTypeIndex (tiiName ti), encodeStrict ti) | ti <- typeEntries ]
    ++ [ (keyStmt i, encodeStrict stmt) | (i, stmt) <- zip [0..] body ]
  where
    I.NModule _ te _ = nmod
    A.Module tmn timps tdoc body = tchecked
    typeEntries = typeIndex te

typeIndex :: I.TEnv -> [TypeIndexInfo]
typeIndex te = mapMaybe entry te
  where
    entry (n, i)
      | not (Names.isPublicName n) = Nothing
      | otherwise                  = case i of
          I.NAct q _ _ ate _       -> Just (TypeIndexInfo n TypeIndexActor q (attrs ate))
          I.NClass q _ ate _       -> Just (TypeIndexInfo n TypeIndexClass q (attrs ate))
          I.NProto q _ ate _       -> Just (TypeIndexInfo n TypeIndexProto q (attrs ate))
          _                        -> Nothing
    attrs = Data.List.nub . map fst

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
      let nmod = I.NModule (map fst imps) te mdoc
          tmod = A.Module tmn timps tdoc stmts
      return (map fst imps, nmod, tmod, sourceMeta, moduleSrcBytesHash, modulePubHash, moduleImplHash, imps, nameHashes, roots, tests, mdoc)

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

isTyCacheMiss :: E.SomeException -> Bool
isTyCacheMiss err
  | Just (_ :: E.IOException) <- E.fromException err = True
  | Just (_ :: LMDB.LMDB_Error) <- E.fromException err = True
  | otherwise = False
