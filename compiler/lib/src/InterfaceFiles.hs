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
--     "public-names"   :: [A.Name]                    -- public top-level names, in TEnv order
--     "constructors"   :: [A.Name]                    -- public class/protocol/actor names
--     "actors"         :: [A.Name]                    -- public actor names
--     "stmt-count"     :: Int                         -- number of typed top-level statements
--     "stmt-mandatory" :: [Int]                       -- ownerless top-level statement indexes
--     "stmt-has-not-impl" :: Bool                     -- any statement contains NotImplemented
--     "module-hash"    :: ModuleHashInfo              -- ownerless module-init hash and deps
--     "module-header"  :: (A.ModName, Imports, Maybe String)
--                                                     -- typed module name, imports, docstring
--
--   Per-name keys (NameInfo / TEnv, one set per name):
--     "name-order/<NNN>"       :: ByteString           -- exact ordered-row key (NNN = padIndex)
--     "name-info/order/<NNN>"  :: (A.Name, I.NameInfo) -- every TEnv occurrence, preserving order
--     "name-info/<suffix>"     :: (A.Name, I.NameInfo) -- last occurrence for direct lookup
--     "name-hash/<suffix>" :: NameHashInfo            -- per-name src/pub/impl hashes + local deps
--                                                     -- + owned statement indexes
--
--   Per-dependency keys:
--     "deps/<module>"          :: [DepNameInfo]        -- dependency names with pub/impl hashes
--     "deps/name/<hash>"       :: DepUsers             -- local names that use one dependency name;
--                                                     -- hash covers the complete module/name pair
--
--   Per-extension keys:
--     "ext-by-class/<suffix>"       :: (A.Name, [A.Name]) -- class name to extension names
--     "ext-by-protocol/<suffix>"    :: (A.Name, [A.Name]) -- protocol name to extension names
--
--   Per-query index keys:
--     "con-attr/<suffix>"     :: (A.Name, [A.Name])    -- class/actor names declaring an attribute
--     "proto-attr/<suffix>"   :: (A.Name, [A.Name])    -- protocol names declaring an attribute
--     "descendants/<suffix>"  :: (A.QName, [A.Name])   -- class/protocol names below a constructor
--     "ext-proto/<suffix>"    :: (A.QName, [A.Name])   -- extension names implementing a protocol
--     "ext-type/<suffix>"     :: (A.QName, [A.Name])   -- extension names for a type/class
--
--   Typed module content keys:
--     "stmt/<NNN>"          :: StoredStmt             -- ordered top-level row (NNN = padIndex)
--     "shape/<suffix>"      :: ContainerShape         -- compact container header/ABI slots
--     "body/member/<owner-hash>/<member-hash>"
--                            :: MemberContentRow       -- independently addressable method/attribute content
--     "reach/top/<top-hash>" :: ReachTopRow
--     "reach/module/<module-hash>" :: ReachModuleRow
--     "reach/member/<top-hash>/<member-hash>" :: ReachMemberRow
--     "reach/shape/<top-hash>" :: ReachShapeRow
--     "reach/slot/<top-hash>/<member-ref-hash>" :: ReachSlotRow
--     "reach/reflection/<top-hash>" :: ReachReflectionRow
--
-- Name and QName suffixes hash a location-free structural encoding. Module
-- suffixes use their readable path when it is short and LMDB-safe, and a hash
-- otherwise.
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
  , MemberKey(..)
  , MemberContent(..)
  , ContainerShape(..)
  , StoredStmt(..)
  , StoredDecl(..)
  , InterfaceRows(..)
  , DepModuleInfo(..)
  , DepNameInfo(..)
  , DepUsers(..)
  , ModuleHashInfo(..)
  , emptyModuleHashInfo
  , ImplRefreshInput(..)
  , ImplRefreshOutput(..)
  , ImplRefreshStale(..)
  , SourceFileMeta(..)
  , InterfaceContents(..)
  , ModuleSnapshot(..)
  , TyFile
  , TyHeader
  , TyHeaderSummary
  , InterfaceDB
  , interfaceExt
  , interfacePath
  , interfaceExists
  , interfaceModifiedTime
  , interfaceModifiedTimeNs
  , copyInterface
  , listInterfaceDirsRecursive
  , registerSystemTypeRoots
  , keyNameInfo
  , keyNameHash
  , keyContainerShape
  , keyMemberBody
  , keyReachModule
  , keyReachTop
  , keyReachMember
  , keyReachShape
  , keyReachSlot
  , keyReachReflection
  , readDepNames
  , readDepUsers
  , readNameHashMaybe
  , readContainerShape
  , readMemberContent
  , readReachModule
  , readReachWholeModule
  , readReachTop
  , readReachTopMaybe
  , readReachMember
  , readReachMemberMaybe
  , readReachShape
  , readReachShapeMaybe
  , readReachSlot
  , readReachSlotMaybe
  , readReachSlots
  , readReachReflection
  , readReachReflectionMaybe
  , readModuleRows
  , readModuleHashesMaybe
  , readModuleSnapshotMaybe
  , readImplRefreshInput
  , readFile
  , readModuleIface
  , readNameHashes
  , readModuleHashInfo
  , readStmtHasNotImpl
  , readHeader
  , readHeaderSummary
  , readRoots
  , readExtensionsByClass
  , readExtensionsByProtocol
  , readFileMaybe
  , readHeaderMaybe
  , readHeaderSummaryMaybe
  , openInterfaceDB
  , openInterfaceDBAtGeneration
  , openInterfaceDBMaybe
  , readInterfaceDBIface
  , readInterfaceDBModuleInfo
  , readInterfaceDBNameInfoMaybe
  , readInterfaceDBPublicNames
  , readInterfaceDBConstructors
  , readInterfaceDBActors
  , readInterfaceDBConAttr
  , readInterfaceDBProtoAttr
  , readInterfaceDBDescendants
  , readInterfaceDBExtByProto
  , readInterfaceDBExtByType
  , readModuleSelection
  , TyDbWriteProgress(..)
  , writeFile
  , writeVersionedFile
  , updateSourceMeta
  , updateSourceHashAndNameHashes
  , updateImplRefresh
  , isImplRefreshStale
  , updateVersion
  ) where

import Prelude hiding (readFile, writeFile)
import Control.DeepSeq (NFData, rnf)
import qualified Control.Exception as E
import Control.Concurrent (getNumCapabilities, runInBoundThread, threadDelay)
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, withMVar)
import Control.Monad (foldM, forM, forM_, replicateM, unless, when)
import Data.IORef (atomicModifyIORef', newIORef)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import qualified Data.List
import Data.List (foldl')
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Persist as Persist
import qualified Data.Set as Set
import Data.Time.Clock (UTCTime)
import qualified Database.LMDB.Raw as LMDB
import qualified Acton.Syntax as A
import Acton.InterfaceRows
  ( MemberKey(..)
  , MemberContent(..)
  , ContainerShape(..)
  , StoredStmt(..)
  , StoredDecl(..)
  , InterfaceRows(..)
  )
import qualified Acton.InterfaceRows as Rows
import qualified Acton.NameInfo as I
import qualified Acton.ReachabilityRows as ReachRows
import qualified Acton.ReachabilityTypes as Reach
import Acton.Names (isPublicName)
import qualified Acton.Names as Names
import Utils (SrcLoc(NoLoc), chunksOf)
import Foreign.Ptr (castPtr)
import Foreign.Storable (peek)
import GHC.Generics (Generic)
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getFileSize, getModificationTime, listDirectory, removeFile, removePathForcibly)
import System.Environment (getExecutablePath, lookupEnv)
import System.FilePath ((</>), addTrailingPathSeparator, normalise, takeDirectory, takeExtension)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Files (deviceID, fileAccess, fileID, getFileStatus, modificationTimeHiRes, setFileMode)
import System.Random (randomIO)

data NameHashInfo = NameHashInfo
  { nhName     :: A.Name
  , nhSrcHash  :: BS.ByteString
  , nhPubHash  :: BS.ByteString
  , nhImplHash :: BS.ByteString
  -- The name's OWN contribution to nhImplHash, before combining with the
  -- hashes of its local and external deps. Storing it lets an impl-hash
  -- refresh recombine hashes from rows alone -- no re-read of the typed
  -- module, no re-parse and no dependency re-walk.
  , nhOwnImplHash :: BS.ByteString
  , nhPubLocalDeps  :: [A.Name]
  , nhImplLocalDeps :: [A.Name]
  , nhPubDeps  :: [(A.QName, BS.ByteString)]
  , nhImplDeps :: [(A.QName, BS.ByteString)]
  , nhStmtIndices :: [Int]
  } deriving (Show, Eq, Generic)

instance Persist.Persist NameHashInfo
instance NFData NameHashInfo

data MemberContentRow = MemberContentRow A.Name MemberKey MemberContent deriving (Show, Eq, Generic)

instance Persist.Persist MemberContentRow
instance NFData MemberContentRow

data ReachTopRow = ReachTopRow ReachRows.TopKey ReachRows.TopInfo deriving (Show, Eq, Generic)

instance Persist.Persist ReachTopRow
instance NFData ReachTopRow

data ReachModuleRow = ReachModuleRow
  A.ModName Reach.ReachSummary Reach.ReachSummary
  deriving (Show, Eq, Generic)

instance Persist.Persist ReachModuleRow
instance NFData ReachModuleRow

data ReachMemberRow = ReachMemberRow ReachRows.TopKey MemberKey ReachRows.MemberInfo deriving (Show, Eq, Generic)

instance Persist.Persist ReachMemberRow
instance NFData ReachMemberRow

data ReachShapeRow = ReachShapeRow ReachRows.TopKey ReachRows.ShapeInfo deriving (Show, Eq, Generic)

instance Persist.Persist ReachShapeRow
instance NFData ReachShapeRow

data ReachSlotRow = ReachSlotRow ReachRows.TopKey Reach.MemberRef ReachRows.SlotInfo deriving (Show, Eq, Generic)

instance Persist.Persist ReachSlotRow
instance NFData ReachSlotRow

data ReachReflectionRow = ReachReflectionRow ReachRows.TopKey ReachRows.ReflectableAttrs deriving (Show, Eq, Generic)

instance Persist.Persist ReachReflectionRow
instance NFData ReachReflectionRow

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

-- | The implementation component owned by the module rather than a top-level
-- name. The compact statement-owner schedule preserves initialization order
-- across independently hashed names; mandatory ownerless statements keep
-- their own structural hash and exact dependency rows. Together these rows
-- are sufficient for an implementation refresh without loading typed syntax.
data ModuleHashInfo = ModuleHashInfo
  { mhOwnImplHash    :: BS.ByteString
  , mhStatementOwners :: [[A.Name]]
  , mhImplHash       :: BS.ByteString
  , mhImplLocalDeps  :: [A.Name]
  , mhPubDeps        :: [(A.QName, BS.ByteString)]
  , mhImplDeps       :: [(A.QName, BS.ByteString)]
  } deriving (Show, Eq, Generic)

instance Persist.Persist ModuleHashInfo
instance NFData ModuleHashInfo

emptyModuleHashInfo :: ModuleHashInfo
emptyModuleHashInfo = ModuleHashInfo BS.empty [] BS.empty [] [] []

-- | The complete stored input to an implementation-only hash refresh. All
-- fields are captured in one read transaction and the identity fields are
-- checked again by 'updateImplRefresh' before it mutates the interface.
data ImplRefreshInput = ImplRefreshInput
  { iriGeneration         :: BS.ByteString
  , iriSourceHash         :: BS.ByteString
  , iriPublicHash         :: BS.ByteString
  , iriImplementationHash :: BS.ByteString
  , iriModuleHashInfo     :: ModuleHashInfo
  , iriDependencies       :: [DepModuleInfo]
  , iriNameHashes         :: [NameHashInfo]
  , iriRoots              :: [A.Name]
  , iriHasNotImpl         :: Bool
  } deriving (Show, Eq)

-- | Newly computed hash rows for an implementation-only refresh.
data ImplRefreshOutput = ImplRefreshOutput
  { iroImplementationHash :: BS.ByteString
  , iroModuleHashInfo     :: ModuleHashInfo
  , iroDependencies       :: [DepModuleInfo]
  , iroNameHashes         :: [NameHashInfo]
  } deriving (Show, Eq)

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

data InterfaceContents = InterfaceContents
  { ifcSourceHash         :: BS.ByteString
  , ifcPublicHash         :: BS.ByteString
  , ifcImplementationHash :: BS.ByteString
  , ifcModuleHashInfo     :: ModuleHashInfo
  , ifcSourceMeta         :: Maybe SourceFileMeta
  , ifcImports            :: [(A.ModName, BS.ByteString)]
  , ifcDependencies       :: [DepModuleInfo]
  , ifcNameHashes         :: [NameHashInfo]
  , ifcRoots              :: [A.Name]
  , ifcTests              :: [String]
  , ifcDoc                :: Maybe String
  , ifcModule             :: I.NModule
  , ifcRows               :: InterfaceRows
  , ifcReachabilityRows   :: ReachRows.ReachabilityRows
  }

data TyDbWriteProgress = TyDbWriteProgress
  { tyDbWriteProgressLabel :: String
  , tyDbWriteProgressRatio :: Double
  } deriving (Show, Eq)

type TyFile =
  ( [A.ModName]
  , I.NModule
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

type TyHeaderSummary =
  ( Maybe SourceFileMeta
  , BS.ByteString
  , BS.ByteString
  , BS.ByteString
  , [(A.ModName, BS.ByteString)]
  , [DepModuleInfo]
  , Int
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

-- | The small, generation-coherent interface facts needed to prepare a
-- deferred back pass. Source imports reconstruct ModuleInfo, while closure
-- imports include implicit dependencies such as __builtin__.
data ModuleSnapshot = ModuleSnapshot
  { msGeneration     :: BS.ByteString
  , msSourceHash     :: BS.ByteString
  , msPublicHash     :: BS.ByteString
  , msImplementationHash :: BS.ByteString
  , msModuleName     :: A.ModName
  , msSourceImports  :: [A.ModName]
  , msClosureImports :: [A.ModName]
  , msRoots          :: [A.Name]
  , msHasNotImpl     :: Bool
  , msDoc            :: Maybe String
  } deriving (Show, Eq)

-- | A handle for selective per-name and per-index lookups in one module's
-- .tydb: the version is validated once at open, and each lookup runs a short
-- read transaction on the shared per-path environment.
data InterfaceDB = InterfaceDB FilePath (Maybe BS.ByteString)

instance Show InterfaceDB where
    show (InterfaceDB path _) = "InterfaceDB " ++ path

interfaceDBPath :: InterfaceDB -> FilePath
interfaceDBPath (InterfaceDB path _) = path

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

systemTypeRoots :: MVar [FilePath]
{-# NOINLINE systemTypeRoots #-}
systemTypeRoots = unsafePerformIO (newMVar [])

registerSystemTypeRoots :: [FilePath] -> IO ()
registerSystemTypeRoots roots =
    modifyMVar systemTypeRoots $ \old ->
      return (foldl' addRoot old (map normalise roots), ())
  where
    addRoot acc root
      | root `elem` acc = acc
      | otherwise       = root : acc

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
    renewInterfaceGeneration dst
    removeFile (lockFilePath dst) `E.catch` ignoreMissing
    setReadableInterfacePermissions dst
  where
    ignoreMissing :: E.IOException -> IO ()
    ignoreMissing _ = return ()

renewInterfaceGeneration :: FilePath -> IO ()
renewInterfaceGeneration f = do
    generation <- newInterfaceGeneration
    size <- readMapSize f
    withWriteTxn f size $ \txn dbi -> do
      validateVersion txn dbi
      putValue txn dbi keyGeneration (encodeStrict generation)

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

newtype ImplRefreshStale = ImplRefreshStale String deriving Show

instance E.Exception ImplRefreshStale

isImplRefreshStale :: E.SomeException -> Bool
isImplRefreshStale err =
    case E.fromException err :: Maybe ImplRefreshStale of
      Just _  -> True
      Nothing -> False

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

keyVersion, keyGeneration, keyMeta, keyImports, keyDeps, keyRoots, keyTests, keyDoc, keyNameCount, keyPublicNames, keyConstructors, keyActors, keyStmtCount, keyStmtMandatory, keyStmtHasNotImpl, keyModuleHash, keyModuleHeader :: BS.ByteString
keyVersion      = key "version"
keyGeneration   = key "generation"
keyMeta         = key "meta"
keyImports      = key "imports"
keyDeps         = key "deps"
keyRoots        = key "roots"
keyTests        = key "tests"
keyDoc          = key "doc"
keyNameCount    = key "name-count"
keyPublicNames  = key "public-names"
keyConstructors = key "constructors"
keyActors       = key "actors"
keyStmtCount    = key "stmt-count"
keyStmtMandatory = key "stmt-mandatory"
keyStmtHasNotImpl = key "stmt-has-not-impl"
keyModuleHash   = key "module-hash"
keyModuleHeader = key "module-header"

-- Every semantic commit receives a fresh nonce.  Unlike content hashes this
-- changes across an A -> B -> A rewrite, so selective readers can detect that
-- separate LMDB read transactions did not observe one committed generation.
newInterfaceGeneration :: IO BS.ByteString
newInterfaceGeneration = BS.pack <$> replicateM 32 randomIO

padIndex :: Int -> String
padIndex i =
    let s = show i
    in replicate (12 - length s) '0' ++ s

-- rawstr is a C-symbol rendering, not a one-to-one semantic encoding: a plain
-- source name can spell the same text as a Derived name.  Direct indexes must
-- therefore key the location-free Name structure itself.
nameKeySuffix :: A.Name -> BS.ByteString
nameKeySuffix n =
    B.concat [key "h/", semanticDigest (stripNameKeyLocs n)]

moduleKeySuffix :: A.ModName -> BS.ByteString
moduleKeySuffix = semanticDigest . stripModNameKeyLocs

keyNameInfo :: A.Name -> BS.ByteString
keyNameInfo n = B.concat [key "name-info/", nameKeySuffix n]

keyNameInfoOrder :: Int -> BS.ByteString
keyNameInfoOrder i = B.pack ("name-info/order/" ++ padIndex i)

keyNameOrder :: Int -> BS.ByteString
keyNameOrder i = B.pack ("name-order/" ++ padIndex i)

keyNameHashPrefix :: BS.ByteString
keyNameHashPrefix = key "name-hash/"

keyNameHash :: A.Name -> BS.ByteString
keyNameHash n = B.concat [keyNameHashPrefix, nameKeySuffix n]

keyContainerShape :: A.Name -> BS.ByteString
keyContainerShape n = B.concat [key "shape/", nameKeySuffix n]

keyMemberBodyPrefix :: A.Name -> BS.ByteString
keyMemberBodyPrefix owner =
    B.concat [key "body/member/", semanticDigest (stripNameKeyLocs owner), key "/"]

semanticDigest :: Persist.Persist a => a -> BS.ByteString
semanticDigest = Base16.encode . SHA256.hash . encodeStrict

keyMemberBody :: A.Name -> MemberKey -> BS.ByteString
keyMemberBody owner member =
    B.concat [keyMemberBodyPrefix owner, semanticDigest (stripMemberKeyLocs member)]

stripMemberKeyLocs :: MemberKey -> MemberKey
stripMemberKeyLocs (Method n) = Method (stripNameKeyLocs n)
stripMemberKeyLocs (Attr n)   = Attr (stripNameKeyLocs n)
stripMemberKeyLocs InitRest   = InitRest

stripTopKeyLocs :: ReachRows.TopKey -> ReachRows.TopKey
stripTopKeyLocs (ReachRows.TopKey mn n) =
    ReachRows.TopKey (stripModNameKeyLocs mn) (stripNameKeyLocs n)

stripMemberRefLocs :: Reach.MemberRef -> Reach.MemberRef
stripMemberRefLocs (Reach.MethodRef n) = Reach.MethodRef (stripNameKeyLocs n)
stripMemberRefLocs (Reach.AttrRef n)   = Reach.AttrRef (stripNameKeyLocs n)

reachOwnerPrefix :: BS.ByteString -> ReachRows.TopKey -> BS.ByteString
reachOwnerPrefix prefix owner =
    B.concat [prefix, semanticDigest (stripTopKeyLocs owner), key "/"]

keyReachTop :: ReachRows.TopKey -> BS.ByteString
keyReachTop owner =
    B.concat [key "reach/top/", semanticDigest (stripTopKeyLocs owner)]

keyReachMember :: ReachRows.TopKey -> MemberKey -> BS.ByteString
keyReachMember owner member =
    B.concat
      [ reachOwnerPrefix (key "reach/member/") owner
      , semanticDigest (stripMemberKeyLocs member)
      ]

keyReachModule :: A.ModName -> BS.ByteString
keyReachModule mn =
    B.concat [key "reach/module/", semanticDigest (stripModNameKeyLocs mn)]

keyReachShape :: ReachRows.TopKey -> BS.ByteString
keyReachShape owner =
    B.concat [key "reach/shape/", semanticDigest (stripTopKeyLocs owner)]

keyReachSlot :: ReachRows.TopKey -> Reach.MemberRef -> BS.ByteString
keyReachSlot owner member =
    B.concat
      [ reachOwnerPrefix (key "reach/slot/") owner
      , semanticDigest (stripMemberRefLocs member)
      ]

keyReachReflection :: ReachRows.TopKey -> BS.ByteString
keyReachReflection owner =
    B.concat [key "reach/reflection/", semanticDigest (stripTopKeyLocs owner)]

keyDepModule :: A.ModName -> BS.ByteString
keyDepModule mn = B.concat [key "deps/", moduleKeySuffix mn]

keyDepName :: A.ModName -> A.Name -> BS.ByteString
keyDepName mn n = B.concat
    [ key "deps/name/"
    , semanticDigest (stripModNameKeyLocs mn, stripNameKeyLocs n)
    ]

keyExtByClassPrefix, keyExtByProtocolPrefix :: BS.ByteString
keyExtByClassPrefix      = key "ext-by-class/"
keyExtByProtocolPrefix   = key "ext-by-protocol/"

keyExtByClass :: A.Name -> BS.ByteString
keyExtByClass n = B.concat [keyExtByClassPrefix, nameKeySuffix n]

keyExtByProtocol :: A.Name -> BS.ByteString
keyExtByProtocol n = B.concat [keyExtByProtocolPrefix, nameKeySuffix n]

keyStmt :: Int -> BS.ByteString
keyStmt i = B.pack ("stmt/" ++ padIndex i)

keyConAttr :: A.Name -> BS.ByteString
keyConAttr n = B.concat [key "con-attr/", nameKeySuffix n]

keyProtoAttr :: A.Name -> BS.ByteString
keyProtoAttr n = B.concat [key "proto-attr/", nameKeySuffix n]

keyDescendants :: A.QName -> BS.ByteString
keyDescendants qn = B.concat [key "descendants/", qNameKeySuffix qn]

keyExtProto :: A.QName -> BS.ByteString
keyExtProto qn = B.concat [key "ext-proto/", qNameKeySuffix qn]

keyExtType :: A.QName -> BS.ByteString
keyExtType qn = B.concat [key "ext-type/", qNameKeySuffix qn]

-- QName keys hash a location-free encoding so the same semantic name always
-- maps to the same key regardless of where it occurred in the source.
qNameKeySuffix :: A.QName -> BS.ByteString
qNameKeySuffix qn = Base16.encode (SHA256.hash (encodeStrict (stripQNameKeyLocs qn)))

stripQNameKeyLocs :: A.QName -> A.QName
stripQNameKeyLocs (A.QName m n) = A.QName (stripModNameKeyLocs m) (stripNameKeyLocs n)
stripQNameKeyLocs (A.NoQ n)     = A.NoQ (stripNameKeyLocs n)
stripQNameKeyLocs (A.GName m n) = A.GName (stripModNameKeyLocs m) (stripNameKeyLocs n)

stripModNameKeyLocs :: A.ModName -> A.ModName
stripModNameKeyLocs (A.ModName ns) = A.ModName (map stripNameKeyLocs ns)

stripNameKeyLocs :: A.Name -> A.Name
stripNameKeyLocs (A.Name _ s)     = A.Name NoLoc s
stripNameKeyLocs (A.Derived n n') = A.Derived (stripNameKeyLocs n) (stripNameKeyLocs n')
stripNameKeyLocs n@A.Internal{}   = n

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

-- Choose read-only open flags. Project and dependency caches may be rewritten
-- by a concurrent acton process, so they MUST keep LMDB's lock table for
-- reader/writer safety (the lock.mdb open race there is handled by retrying the
-- open). Selected Acton system type roots are immutable runtime input, so
-- bundled base/std interfaces are read without lock-table state.
readOnlyOpenFlags :: FilePath -> IO [LMDB.MDB_EnvFlag]
readOnlyOpenFlags path = do
    noLock <- isUnderImmutableInterfaceRoot path
    return $ if noLock
               then [LMDB.MDB_RDONLY, LMDB.MDB_NOLOCK]
               else [LMDB.MDB_RDONLY]

actonDistributionRoot :: FilePath
{-# NOINLINE actonDistributionRoot #-}
actonDistributionRoot =
    unsafePerformIO $ do
      exe <- getExecutablePath
      canonicalizePath (takeDirectory exe </> "..")

isUnderImmutableInterfaceRoot :: FilePath -> IO Bool
isUnderImmutableInterfaceRoot path = do
    cpath <- canonicalizePath path
    roots <- withMVar systemTypeRoots return
    return (any (`containsPath` cpath) (actonDistributionRoot : roots))
  where
    containsPath root child =
      let rootDir = addTrailingPathSeparator root
      in child == root || rootDir `Data.List.isPrefixOf` child

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
    -- A genuinely absent data file is a cache miss, not an environmental error:
    -- surface it as TyCacheInvalid so readFileMaybe/readHeaderMaybe report a miss
    -- instead of letting mdb_env_open raise a raw "No such file or directory".
    exists <- doesFileExist (dataFilePath path)
    unless exists $
      E.throwIO (TyCacheInvalid ("Missing .tydb data file: " ++ dataFilePath path))
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

openInterfaceDB :: FilePath -> IO InterfaceDB
openInterfaceDB path = do
    withReadTxn path validateVersion
    return (InterfaceDB path Nothing)

-- | Open a handle whose every lookup must observe one captured interface
-- generation. The generation check and requested row read share an LMDB
-- transaction, so a lazy ModuleInfo can never memoize data from another
-- committed generation.
openInterfaceDBAtGeneration :: FilePath -> BS.ByteString -> IO InterfaceDB
openInterfaceDBAtGeneration path generation = do
    withReadTxn path $ \txn dbi -> do
      validateVersion txn dbi
      validateGeneration generation txn dbi
    return (InterfaceDB path $ Just generation)

-- | Like openInterfaceDB, but treats a missing, corrupt, or version-mismatched
-- cache as a miss, mirroring readFileMaybe.
openInterfaceDBMaybe :: FilePath -> IO (Maybe InterfaceDB)
openInterfaceDBMaybe = readTyMaybe openInterfaceDB

withInterfaceDBReadTxn :: InterfaceDB -> (LMDB.MDB_txn -> LMDB.MDB_dbi -> IO a) -> IO a
withInterfaceDBReadTxn (InterfaceDB path expected) action =
    withReadTxn path $ \txn dbi -> do
      forM_ expected $ \generation -> validateGeneration generation txn dbi
      action txn dbi

validateGeneration :: BS.ByteString -> LMDB.MDB_txn -> LMDB.MDB_dbi -> IO ()
validateGeneration expected txn dbi = do
    actual <- getValue "generation" txn dbi keyGeneration
    unless (actual == expected) $
      E.throwIO (TyCacheInvalid "Interface generation changed during deferred back pass")

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

getEntriesWithPrefix :: LMDB.MDB_txn
                     -> LMDB.MDB_dbi
                     -> BS.ByteString
                     -> IO [(BS.ByteString, BS.ByteString)]
getEntriesWithPrefix txn dbi prefix =
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
          ((k, v) :) <$> go cursor kp vp found

getKeysWithPrefix :: LMDB.MDB_txn -> LMDB.MDB_dbi -> BS.ByteString -> IO [BS.ByteString]
getKeysWithPrefix txn dbi prefix =
    map fst <$> getEntriesWithPrefix txn dbi prefix

putValue :: LMDB.MDB_txn -> LMDB.MDB_dbi -> BS.ByteString -> BS.ByteString -> IO ()
putValue txn dbi k v =
    withVal k $ \kv ->
      withVal v $ \vv -> do
        _ <- LMDB.mdb_put (LMDB.compileWriteFlags []) txn dbi kv vv
        return ()

deleteValue :: LMDB.MDB_txn -> LMDB.MDB_dbi -> BS.ByteString -> IO ()
deleteValue txn dbi k =
    withVal k $ \kv -> do
      _ <- LMDB.mdb_del txn dbi kv Nothing
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
    validateEntryKeys entries
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

lmdbKeyLimit :: Int
lmdbKeyLimit = 511

validateEntryKeys :: [(BS.ByteString, BS.ByteString)] -> IO ()
validateEntryKeys entries = do
    let keys = map fst entries
        tooLong = filter ((> lmdbKeyLimit) . BS.length) keys
        counts = Map.fromListWith (+) [ (k,1 :: Int) | k <- keys ]
        duplicates = [ (k,n) | (k,n) <- Map.toAscList counts, n > 1 ]
    unless (null tooLong) $
      invalidModuleRows
        ("LMDB key exceeds " ++ show lmdbKeyLimit ++ " bytes: " ++
         show (maximum $ map BS.length tooLong))
    unless (null duplicates) $
      invalidModuleRows
        ("storage keys are not globally unique: " ++
         Data.List.intercalate ", "
           [ show (B.unpack k) ++ " (" ++ show n ++ " entries)"
           | (k,n) <- duplicates
           ])

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
    _ <- getValue "module-hash" txn dbi keyModuleHash :: IO ModuleHashInfo
    return ()

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

readModuleSnapshot :: FilePath -> IO ModuleSnapshot
readModuleSnapshot f =
    withReadTxn f $ \txn dbi -> do
      (_sourceMeta, srcHash, pubHash, implHash) <- readMeta txn dbi
      generation <- getValue "generation" txn dbi keyGeneration
      hashedImports <- getValue "imports" txn dbi keyImports ::
        IO [(A.ModName, BS.ByteString)]
      roots <- getValue "roots" txn dbi keyRoots
      hasNotImpl <- getValue "stmt-has-not-impl" txn dbi keyStmtHasNotImpl
      (moduleName, imports, sourceDoc) <-
        getValue "module-header" txn dbi keyModuleHeader
      doc <- getValue "doc" txn dbi keyDoc
      traceTydbRead "module-snapshot" f "generation meta imports"
      return ModuleSnapshot
        { msGeneration = generation
        , msSourceHash = srcHash
        , msPublicHash = pubHash
        , msImplementationHash = implHash
        , msModuleName = moduleName
        , msSourceImports = A.importsOf (A.Module moduleName imports sourceDoc [])
        , msClosureImports = map fst hashedImports
        , msRoots = roots
        , msHasNotImpl = hasNotImpl
        , msDoc = doc
        }

-- | Read every row used to recompute implementation hashes in one committed
-- interface generation. Typed declarations and statement bodies stay lazy.
readImplRefreshInput :: FilePath -> IO ImplRefreshInput
readImplRefreshInput f =
    withReadTxn f $ \txn dbi -> do
      (_sourceMeta, srcHash, pubHash, implHash) <- readMeta txn dbi
      generation <- getValue "generation" txn dbi keyGeneration
      moduleHashInfo <- getValue "module-hash" txn dbi keyModuleHash
      dependencies <- getValue "deps" txn dbi keyDeps
      storedNameHashes <- readNameHashEntries txn dbi
      nameHashes <- restoreNameHashDeps txn dbi dependencies storedNameHashes
      roots <- getValue "roots" txn dbi keyRoots
      hasNotImpl <- getValue "stmt-has-not-impl" txn dbi keyStmtHasNotImpl
      traceTydbRead "impl-refresh" f
        ("generation names " ++ show (length nameHashes))
      return ImplRefreshInput
        { iriGeneration = generation
        , iriSourceHash = srcHash
        , iriPublicHash = pubHash
        , iriImplementationHash = implHash
        , iriModuleHashInfo = moduleHashInfo
        , iriDependencies = dependencies
        , iriNameHashes = nameHashes
        , iriRoots = roots
        , iriHasNotImpl = hasNotImpl
        }

-- Per-name rows omit external deps; restore them from the dependency indexes
-- while the caller's read transaction still pins the interface generation.
restoreNameHashDeps :: LMDB.MDB_txn
                    -> LMDB.MDB_dbi
                    -> [DepModuleInfo]
                    -> [NameHashInfo]
                    -> IO [NameHashInfo]
restoreNameHashDeps txn dbi depModules nameHashes = do
    (pubByOwner, implByOwner) <- foldM addModule (Map.empty, Map.empty) depModules
    return
      [ nh { nhPubDeps = Data.List.sortOn fst (Map.findWithDefault [] (nhName nh) pubByOwner)
           , nhImplDeps = Data.List.sortOn fst (Map.findWithDefault [] (nhName nh) implByOwner)
           }
      | nh <- nameHashes
      ]
  where
    addModule acc depInfo = do
      let depMn = dmiModule depInfo
      depNames <- readDepNameEntries txn dbi depMn
      foldM (addName depMn) acc depNames

    addName depMn (pubAcc, implAcc) depInfo = do
      users <- readDepUsersEntry txn dbi depMn (dniName depInfo)
      let mkDep h = (A.GName depMn (dniName depInfo), h)
          addUser dep m user = Map.insertWith (++) user [dep] m
          pubAcc'
            | BS.null (dniPubHash depInfo) = pubAcc
            | otherwise = foldl' (addUser (mkDep $ dniPubHash depInfo)) pubAcc (duPubUsers users)
          implAcc'
            | BS.null (dniImplHash depInfo) = implAcc
            | otherwise = foldl' (addUser (mkDep $ dniImplHash depInfo)) implAcc (duImplUsers users)
      return (pubAcc', implAcc')

readNameEntries :: LMDB.MDB_txn -> LMDB.MDB_dbi -> IO ([(A.Name, I.NameInfo)], [NameHashInfo])
readNameEntries txn dbi = do
    te <- readNameInfoEntries txn dbi
    nameHashes <- readNameHashEntries txn dbi
    return (te, nameHashes)

readNameInfoEntries :: LMDB.MDB_txn -> LMDB.MDB_dbi -> IO [(A.Name, I.NameInfo)]
readNameInfoEntries txn dbi = do
    nameCount <- getValue "name-count" txn dbi keyNameCount
    forM [0 .. nameCount - 1] $ \i -> do
      rowKey <- getValue ("name-order " ++ show i) txn dbi (keyNameOrder i)
      unless (rowKey == keyNameInfoOrder i) $
        invalidContentRow ("name-info order/key mismatch at " ++ show i)
      getValue ("name-info " ++ show i) txn dbi rowKey

readNameInfoEntryMaybe :: LMDB.MDB_txn -> LMDB.MDB_dbi -> A.Name -> IO (Maybe (A.Name, I.NameInfo))
readNameInfoEntryMaybe txn dbi n = do
    entry <- getMaybeValue ("name-info " ++ A.nstr n) txn dbi (keyNameInfo n)
    forM_ entry $ \(stored,_) ->
      unless (stored == n) $
        invalidContentRow ("name-info key/value mismatch for " ++ A.rawstr n)
    return entry

readNameInfoEntriesByNames :: LMDB.MDB_txn -> LMDB.MDB_dbi -> [A.Name] -> IO I.TEnv
readNameInfoEntriesByNames txn dbi ns =
    forM ns $ \n -> do
      entry@(stored,_) <-
        getValue ("name-info " ++ A.nstr n) txn dbi (keyNameInfo n)
      unless (stored == n) $
        invalidContentRow ("name-info key/value mismatch for " ++ A.rawstr n)
      return entry

readInterfaceDBModuleInfo :: InterfaceDB -> IO ([A.ModName], Maybe String)
readInterfaceDBModuleInfo db =
    withInterfaceDBReadTxn db $ \txn dbi -> do
      (tmn, timps, tdoc) <- getValue "module-header" txn dbi keyModuleHeader
      doc <- getValue "doc" txn dbi keyDoc
      return (A.importsOf (A.Module tmn timps tdoc []), doc)

readInterfaceDBIface :: InterfaceDB -> IO ([A.ModName], I.NModule)
readInterfaceDBIface db = do
    result@(_, I.NModule _ te _) <-
      withInterfaceDBReadTxn db readModuleIfaceEntries
    traceTydbRead "iface" (interfaceDBPath db) ("names " ++ show (length te))
    return result

readInterfaceDBNameInfoMaybe :: InterfaceDB -> A.Name -> IO (Maybe (A.Name, I.NameInfo))
readInterfaceDBNameInfoMaybe db n = do
    mi <- withInterfaceDBReadTxn db $ \txn dbi ->
            readNameInfoEntryMaybe txn dbi n
    traceTydbRead (case mi of Just _ -> "name-hit"; Nothing -> "name-miss")
      (interfaceDBPath db) (A.nstr n)
    return mi

readInterfaceDBPublicNames :: InterfaceDB -> IO [A.Name]
readInterfaceDBPublicNames db = do
    ns <- withInterfaceDBReadTxn db $ \txn dbi ->
            getValue "public-names" txn dbi keyPublicNames
    traceTydbRead "public-names" (interfaceDBPath db) (show (length ns))
    return ns

readInterfaceDBConstructors :: InterfaceDB -> IO I.TEnv
readInterfaceDBConstructors db = do
    te <- withInterfaceDBReadTxn db $ \txn dbi -> do
            ns <- getValue "constructors" txn dbi keyConstructors
            readNameInfoEntriesByNames txn dbi ns
    traceTydbRead "constructors" (interfaceDBPath db) (show (length te))
    return te

readInterfaceDBActors :: InterfaceDB -> IO I.TEnv
readInterfaceDBActors db = do
    te <- withInterfaceDBReadTxn db $ \txn dbi -> do
            ns <- getValue "actors" txn dbi keyActors
            readNameInfoEntriesByNames txn dbi ns
    traceTydbRead "actors" (interfaceDBPath db) (show (length te))
    return te

readInterfaceDBConAttr :: InterfaceDB -> A.Name -> IO I.TEnv
readInterfaceDBConAttr db n =
    readNameTEnvIndex db ("con-attr " ++ A.nstr n) (keyConAttr n) n

readInterfaceDBProtoAttr :: InterfaceDB -> A.Name -> IO I.TEnv
readInterfaceDBProtoAttr db n =
    readNameTEnvIndex db ("proto-attr " ++ A.nstr n) (keyProtoAttr n) n

readInterfaceDBDescendants :: InterfaceDB -> A.QName -> IO I.TEnv
readInterfaceDBDescendants db qn =
    readQNameTEnvIndex db ("descendants " ++ show qn) (keyDescendants qn) qn

readInterfaceDBExtByProto :: InterfaceDB -> A.QName -> IO I.TEnv
readInterfaceDBExtByProto db qn =
    readQNameTEnvIndex db ("ext-proto " ++ show qn) (keyExtProto qn) qn

readInterfaceDBExtByType :: InterfaceDB -> A.QName -> IO I.TEnv
readInterfaceDBExtByType db qn =
    readQNameTEnvIndex db ("ext-type " ++ show qn) (keyExtType qn) qn

readNameTEnvIndex :: InterfaceDB -> String -> BS.ByteString -> A.Name -> IO I.TEnv
readNameTEnvIndex db label k expected = do
    te <- withInterfaceDBReadTxn db $ \txn dbi -> do
            entry <- getMaybeValue label txn dbi k :: IO (Maybe (A.Name, [A.Name]))
            ns <- case entry of
              Nothing -> return []
              Just (stored,names)
                | stored == expected -> return names
                | otherwise -> invalidContentRow (label ++ " key/value mismatch")
            readNameInfoEntriesByNames txn dbi ns
    traceTydbRead "index" (interfaceDBPath db) (label ++ " " ++ show (length te))
    return te

readQNameTEnvIndex :: InterfaceDB -> String -> BS.ByteString -> A.QName -> IO I.TEnv
readQNameTEnvIndex db label k expected = do
    te <- withInterfaceDBReadTxn db $ \txn dbi -> do
            entry <- getMaybeValue label txn dbi k :: IO (Maybe (A.QName, [A.Name]))
            ns <- case entry of
              Nothing -> return []
              Just (stored,names)
                | stored == stripQNameKeyLocs expected -> return names
                | otherwise -> invalidContentRow (label ++ " key/value mismatch")
            readNameInfoEntriesByNames txn dbi ns
    traceTydbRead "index" (interfaceDBPath db) (label ++ " " ++ show (length te))
    return te

readNameHashEntries :: LMDB.MDB_txn -> LMDB.MDB_dbi -> IO [NameHashInfo]
readNameHashEntries txn dbi = do
    entries <- getEntriesWithPrefix txn dbi keyNameHashPrefix
    infos <- forM (zip [0..] entries) $ \(i, (storedKey, bytes)) -> do
      info <- decodeStrict ("name-hash " ++ show (i :: Int)) bytes
      unless (keyNameHash (nhName info) == storedKey) $
        invalidContentRow ("name-hash key/value mismatch for " ++ A.rawstr (nhName info))
      return info
    return (Data.List.sortOn nhName infos)

invalidContentRow :: String -> IO a
invalidContentRow msg =
    E.throwIO (TyCacheInvalid ("Invalid .tydb content row: " ++ msg))

rowsIO :: Rows.RowResult a -> IO a
rowsIO result =
    case result of
      Left (Rows.RowError msg) -> invalidContentRow msg
      Right value              -> return value

memberLabel :: A.Name -> MemberKey -> String
memberLabel owner member = A.rawstr owner ++ "." ++ Rows.memberLabel member

validateRowOwners :: [NameHashInfo] -> InterfaceRows -> IO ()
validateRowOwners nameHashes rows = do
    let hashNames = Set.fromList (map nhName nameHashes)
        ownerNames = Set.fromList (concatMap Rows.storedStmtNames (rowStatements rows))
        unknown = Set.toAscList (ownerNames `Set.difference` hashNames)
    unless (null unknown) $
      invalidContentRow
        ("statement owners have no name hash: " ++
         Data.List.intercalate ", " (map A.rawstr unknown))

readContainerShapeEntry :: LMDB.MDB_txn -> LMDB.MDB_dbi -> A.Name -> IO ContainerShape
readContainerShapeEntry txn dbi name = do
    shape <- getValue ("shape " ++ A.rawstr name) txn dbi (keyContainerShape name)
    when (shapeName shape /= name) $
      invalidContentRow ("shape key/value mismatch for " ++ A.rawstr name)
    rowsIO (Rows.validateContainerShape shape)
    return shape

readMemberContentEntry :: LMDB.MDB_txn -> LMDB.MDB_dbi -> A.Name -> MemberKey -> IO MemberContent
readMemberContentEntry txn dbi owner member = do
    MemberContentRow storedOwner storedMember content <-
      getValue ("body/member " ++ memberLabel owner member) txn dbi (keyMemberBody owner member)
    when (storedOwner /= owner || storedMember /= member) $
      invalidContentRow ("member content key/value mismatch for " ++ memberLabel owner member)
    rowsIO (Rows.validateMemberContent member content)
    return content

topKeyLabel :: ReachRows.TopKey -> String
topKeyLabel (ReachRows.TopKey mn name) =
    Data.List.intercalate "." (A.modPath mn) ++ "." ++ A.rawstr name

memberRefLabel :: Reach.MemberRef -> String
memberRefLabel (Reach.MethodRef name) = "method:" ++ A.rawstr name
memberRefLabel (Reach.AttrRef name)   = "attr:" ++ A.rawstr name

readReachModuleEntry :: LMDB.MDB_txn
                     -> LMDB.MDB_dbi
                     -> A.ModName
                     -> IO (Reach.ReachSummary, Reach.ReachSummary)
readReachModuleEntry txn dbi moduleName = do
    ReachModuleRow storedModule mandatory whole <-
      getValue label txn dbi (keyReachModule moduleName)
    unless (storedModule == moduleName) $
      invalidContentRow (label ++ " key/value mismatch")
    return (mandatory, whole)
  where
    label = "reach/module " ++ Data.List.intercalate "." (A.modPath moduleName)

readReachTopEntry :: LMDB.MDB_txn -> LMDB.MDB_dbi -> ReachRows.TopKey -> IO ReachRows.TopInfo
readReachTopEntry txn dbi owner = do
    row <- getValue label txn dbi (keyReachTop owner)
    validateReachTopRow owner row
  where
    label = "reach/top " ++ topKeyLabel owner

readReachTopEntryMaybe :: LMDB.MDB_txn
                       -> LMDB.MDB_dbi
                       -> ReachRows.TopKey
                       -> IO (Maybe ReachRows.TopInfo)
readReachTopEntryMaybe txn dbi owner = do
    row <- getMaybeValue label txn dbi (keyReachTop owner)
    traverse (validateReachTopRow owner) row
  where
    label = "reach/top " ++ topKeyLabel owner

validateReachTopRow :: ReachRows.TopKey -> ReachTopRow -> IO ReachRows.TopInfo
validateReachTopRow owner (ReachTopRow storedOwner info) = do
    unless (storedOwner == owner) $
      invalidContentRow ("reach top key/value mismatch for " ++ topKeyLabel owner)
    return info

readReachMemberEntry :: LMDB.MDB_txn
                     -> LMDB.MDB_dbi
                     -> ReachRows.TopKey
                     -> MemberKey
                     -> IO ReachRows.MemberInfo
readReachMemberEntry txn dbi owner member = do
    row <- getValue label txn dbi (keyReachMember owner member)
    validateReachMemberRow owner member row
  where
    label = "reach/member " ++ topKeyLabel owner ++ "." ++ Rows.memberLabel member

readReachMemberEntryMaybe :: LMDB.MDB_txn
                          -> LMDB.MDB_dbi
                          -> ReachRows.TopKey
                          -> MemberKey
                          -> IO (Maybe ReachRows.MemberInfo)
readReachMemberEntryMaybe txn dbi owner member = do
    row <- getMaybeValue label txn dbi (keyReachMember owner member)
    traverse (validateReachMemberRow owner member) row
  where
    label = "reach/member " ++ topKeyLabel owner ++ "." ++ Rows.memberLabel member

validateReachMemberRow :: ReachRows.TopKey
                       -> MemberKey
                       -> ReachMemberRow
                       -> IO ReachRows.MemberInfo
validateReachMemberRow owner member (ReachMemberRow storedOwner storedMember info) = do
    unless (storedOwner == owner && storedMember == member) $
      invalidContentRow (label ++ " key/value mismatch")
    return info
  where
    label = "reach/member " ++ topKeyLabel owner ++ "." ++ Rows.memberLabel member

readReachShapeEntry :: LMDB.MDB_txn -> LMDB.MDB_dbi -> ReachRows.TopKey -> IO ReachRows.ShapeInfo
readReachShapeEntry txn dbi owner = do
    row <- getValue label txn dbi (keyReachShape owner)
    validateReachShapeRow owner row
  where
    label = "reach/shape " ++ topKeyLabel owner

readReachShapeEntryMaybe :: LMDB.MDB_txn
                         -> LMDB.MDB_dbi
                         -> ReachRows.TopKey
                         -> IO (Maybe ReachRows.ShapeInfo)
readReachShapeEntryMaybe txn dbi owner = do
    row <- getMaybeValue label txn dbi (keyReachShape owner)
    traverse (validateReachShapeRow owner) row
  where
    label = "reach/shape " ++ topKeyLabel owner

validateReachShapeRow :: ReachRows.TopKey -> ReachShapeRow -> IO ReachRows.ShapeInfo
validateReachShapeRow owner (ReachShapeRow storedOwner info) = do
    unless (storedOwner == owner && ReachRows.shapeName info == owner) $
      invalidContentRow (label ++ " key/value mismatch")
    return info
  where
    label = "reach/shape " ++ topKeyLabel owner

readReachSlotEntry :: LMDB.MDB_txn
                   -> LMDB.MDB_dbi
                   -> ReachRows.TopKey
                   -> Reach.MemberRef
                   -> IO ReachRows.SlotInfo
readReachSlotEntry txn dbi owner member = do
    row <- getValue label txn dbi (keyReachSlot owner member)
    validateReachSlotRow owner member row
  where
    label = "reach/slot " ++ topKeyLabel owner ++ "." ++ memberRefLabel member

readReachSlotEntryMaybe :: LMDB.MDB_txn
                        -> LMDB.MDB_dbi
                        -> ReachRows.TopKey
                        -> Reach.MemberRef
                        -> IO (Maybe ReachRows.SlotInfo)
readReachSlotEntryMaybe txn dbi owner member = do
    row <- getMaybeValue label txn dbi (keyReachSlot owner member)
    traverse (validateReachSlotRow owner member) row
  where
    label = "reach/slot " ++ topKeyLabel owner ++ "." ++ memberRefLabel member

validateReachSlotRow :: ReachRows.TopKey
                     -> Reach.MemberRef
                     -> ReachSlotRow
                     -> IO ReachRows.SlotInfo
validateReachSlotRow owner member (ReachSlotRow storedOwner storedMember info) = do
    unless (storedOwner == owner && storedMember == member) $
      invalidContentRow (label ++ " key/value mismatch")
    return info
  where
    label = "reach/slot " ++ topKeyLabel owner ++ "." ++ memberRefLabel member

readReachSlotsEntry :: LMDB.MDB_txn
                    -> LMDB.MDB_dbi
                    -> ReachRows.TopKey
                    -> IO [(Reach.MemberRef, ReachRows.SlotInfo)]
readReachSlotsEntry txn dbi owner = do
    rows <- getEntriesWithPrefix txn dbi
      (reachOwnerPrefix (key "reach/slot/") owner)
    entries <- forM (zip [0 :: Int ..] rows) $ \(i,(storedKey,bytes)) -> do
      row@(ReachSlotRow _ storedMember _) <- decodeStrict
        ("reach/slot " ++ topKeyLabel owner ++ " #" ++ show i) bytes
      unless (storedKey == keyReachSlot owner storedMember) $
        invalidContentRow
          ("reach/slot key/value mismatch for " ++ topKeyLabel owner)
      info <- validateReachSlotRow owner storedMember row
      return (storedMember,info)
    let slots = Map.fromList entries
    when (length entries /= Map.size slots) $
      invalidContentRow ("duplicate reach/slot rows for " ++ topKeyLabel owner)
    return (Map.toAscList slots)

readReachReflectionEntry :: LMDB.MDB_txn
                         -> LMDB.MDB_dbi
                         -> ReachRows.TopKey
                         -> IO ReachRows.ReflectableAttrs
readReachReflectionEntry txn dbi owner = do
    row <- getValue label txn dbi (keyReachReflection owner)
    validateReachReflectionRow owner row
  where
    label = "reach/reflection " ++ topKeyLabel owner

readReachReflectionEntryMaybe :: LMDB.MDB_txn
                              -> LMDB.MDB_dbi
                              -> ReachRows.TopKey
                              -> IO (Maybe ReachRows.ReflectableAttrs)
readReachReflectionEntryMaybe txn dbi owner = do
    row <- getMaybeValue label txn dbi (keyReachReflection owner)
    traverse (validateReachReflectionRow owner) row
  where
    label = "reach/reflection " ++ topKeyLabel owner

validateReachReflectionRow :: ReachRows.TopKey
                           -> ReachReflectionRow
                           -> IO ReachRows.ReflectableAttrs
validateReachReflectionRow owner (ReachReflectionRow storedOwner attrs) = do
    unless (storedOwner == owner) $
      invalidContentRow (label ++ " key/value mismatch")
    return attrs
  where
    label = "reach/reflection " ++ topKeyLabel owner

readMemberContentsForOwner :: LMDB.MDB_txn
                           -> LMDB.MDB_dbi
                           -> A.Name
                           -> IO (Map.Map MemberKey MemberContent)
readMemberContentsForOwner txn dbi owner = do
    rows <- getEntriesWithPrefix txn dbi (keyMemberBodyPrefix owner)
    entries <- forM (zip [0 :: Int ..] rows) $ \(i, (storedKey, bytes)) -> do
      MemberContentRow storedOwner member content <-
        decodeStrict ("body/member " ++ A.rawstr owner ++ " #" ++ show i) bytes
      when (storedOwner /= owner) $
        invalidContentRow ("member owner key/value mismatch for " ++ A.rawstr owner)
      unless (storedKey == keyMemberBody owner member) $
        invalidContentRow ("member content key/value mismatch for " ++ memberLabel owner member)
      rowsIO (Rows.validateMemberContent member content)
      return (member, content)
    when (length entries /= Map.size (Map.fromList entries)) $
      invalidContentRow ("duplicate semantic member rows for " ++ A.rawstr owner)
    return (Map.fromList entries)

readContainerEntry :: LMDB.MDB_txn -> LMDB.MDB_dbi -> A.Name -> IO A.Decl
readContainerEntry txn dbi name = do
    shape <- readContainerShapeEntry txn dbi name
    members <- readMemberContentsForOwner txn dbi name
    rowsIO (Rows.restoreExactContainer shape members)

readSelectedContainerEntry :: LMDB.MDB_txn
                           -> LMDB.MDB_dbi
                           -> A.Name
                           -> Set.Set MemberKey
                           -> Set.Set A.Name
                           -> Set.Set A.Name
                           -> IO A.Decl
readSelectedContainerEntry txn dbi name requested staticInitializers instanceInitializers = do
    shape <- readContainerShapeEntry txn dbi name
    (members, selected) <- loadSelected Map.empty requested
    rowsIO (Rows.restoreSelectedContainer shape members selected
      staticInitializers instanceInitializers)
  where
    loadSelected loaded needed = do
      loaded' <- foldM loadOne loaded (Set.toAscList (needed `Set.difference` Map.keysSet loaded))
      expansion <- rowsIO (Rows.expandMemberSelection loaded' requested)
      case expansion of
        Rows.SelectionComplete selected -> return (loaded', selected)
        Rows.SelectionNeeds more -> loadSelected loaded' more

    loadOne loaded member = do
      content <- readMemberContentEntry txn dbi name member
      return (Map.insert member content loaded)

restoreStoredStmt :: LMDB.MDB_txn
                  -> LMDB.MDB_dbi
                  -> Maybe (Set.Set A.Name)
                  -> StoredStmt
                  -> IO (Maybe A.Stmt)
restoreStoredStmt _ _ _ (StoredWhole _ stmt) = return (Just stmt)
restoreStoredStmt txn dbi selected (StoredDecls loc decls) = do
    restored <- forM decls $ \stored ->
      case stored of
        StoredInline decl
          | wanted (Names.dname' decl) -> return (Just decl)
          | otherwise                  -> return Nothing
        StoredContainer name
          | wanted name -> Just <$> readContainerEntry txn dbi name
          | otherwise   -> return Nothing
    case [ decl | Just decl <- restored ] of
      [] -> return Nothing
      kept -> return (Just (A.Decl loc kept))
  where
    wanted name = maybe True (Set.member name) selected

restoreSelectedStoredStmt :: LMDB.MDB_txn
                          -> LMDB.MDB_dbi
                          -> Set.Set A.Name
                          -> Map.Map A.Name (Set.Set MemberKey)
                          -> Map.Map A.Name (Set.Set A.Name)
                          -> Map.Map A.Name (Set.Set A.Name)
                          -> StoredStmt
                          -> IO (Maybe A.Stmt)
restoreSelectedStoredStmt _ _ _ _ _ _ (StoredWhole _ stmt) = return (Just stmt)
restoreSelectedStoredStmt txn dbi selected interests staticInitializerInterests instanceInitializerInterests (StoredDecls loc decls) = do
    restored <- forM decls $ \stored ->
      case stored of
        StoredInline decl
          | Set.member (Names.dname' decl) selected -> return (Just decl)
          | otherwise -> return Nothing
        StoredContainer name
          | Set.member name selected ->
              Just <$> readSelectedContainerEntry txn dbi name
                (Map.findWithDefault Set.empty name interests)
                (Map.findWithDefault Set.empty name staticInitializerInterests)
                (Map.findWithDefault Set.empty name instanceInitializerInterests)
          | otherwise -> return Nothing
    case [ decl | Just decl <- restored ] of
      [] -> return Nothing
      kept -> return (Just (A.Decl loc kept))

readStoredStmtEntry :: LMDB.MDB_txn -> LMDB.MDB_dbi -> Int -> IO StoredStmt
readStoredStmtEntry txn dbi i =
    getValue ("stmt " ++ show i) txn dbi (keyStmt i)

readStmtEntries :: LMDB.MDB_txn -> LMDB.MDB_dbi -> IO [A.Stmt]
readStmtEntries txn dbi = do
    count <- getValue "stmt-count" txn dbi keyStmtCount
    rows <- mapM (readStoredStmtEntry txn dbi) [0 .. count - 1]
    restored <- mapM (restoreStoredStmt txn dbi Nothing) rows
    return [ stmt | Just stmt <- restored ]

-- | Read the persisted content rows without materializing container bodies.
-- Metadata refreshes use this to preserve the exact storage partition.
readModuleRows :: FilePath -> IO InterfaceRows
readModuleRows f =
    withReadTxn f $ \txn dbi -> do
      validateVersion txn dbi
      (moduleName, imports, doc) <- getValue "module-header" txn dbi keyModuleHeader
      count <- getValue "stmt-count" txn dbi keyStmtCount
      statements <- mapM (readStoredStmtEntry txn dbi) [0 .. count - 1]
      hasNotImpl <- getValue "stmt-has-not-impl" txn dbi keyStmtHasNotImpl
      nameHashes <- readNameHashEntries txn dbi
      let shapeNames =
            [ name
            | StoredDecls _ decls <- statements
            , StoredContainer name <- decls
            ]
      shapes <- mapM (readContainerShapeEntry txn dbi) shapeNames
      members <- forM shapes $ \shape -> do
        contents <- readMemberContentsForOwner txn dbi (shapeName shape)
        return (shapeName shape, contents)
      let rows = InterfaceRows
            { rowModuleName = moduleName
            , rowImports = imports
            , rowDoc = doc
            , rowHasNotImpl = hasNotImpl
            , rowStatements = statements
            , rowShapes = Map.fromList [ (shapeName shape, shape) | shape <- shapes ]
            , rowMembers = Map.fromList members
            }
      validateRowOwners nameHashes rows
      rowsIO (Rows.validateInterfaceRows rows)
      traceTydbRead "module-rows" f
        ("statements " ++ show (length statements) ++
         " containers " ++ show (length shapes))
      return rows

-- | Materialize the exact top/member projection selected by the global
-- reachability worklist. Only the requested member rows are read; method ABI
-- slots come from the compact shape, and attribute declarations remain
-- distinct from the constructor initializers activated for them.
readModuleSelection :: FilePath
                    -> [NameHashInfo]
                    -> Set.Set A.Name
                    -> Map.Map A.Name (Set.Set MemberKey)
                    -> Map.Map A.Name (Set.Set A.Name)
                    -> Map.Map A.Name (Set.Set A.Name)
                    -> IO A.Module
readModuleSelection f nameHashes selected interests staticInitializerInterests instanceInitializerInterests =
    withReadTxn f $ \txn dbi -> do
      validateVersion txn dbi
      (moduleName, imports, doc) <- getValue "module-header" txn dbi keyModuleHeader
      hasNotImpl <- getValue "stmt-has-not-impl" txn dbi keyStmtHasNotImpl
      mandatory <- getValue "stmt-mandatory" txn dbi keyStmtMandatory
      when hasNotImpl $
        invalidModuleRows "native module requested through selective materialization"
      let stmtMap = Map.fromList [ (nhName nh, nhStmtIndices nh) | nh <- nameHashes ]
          owners = [ (name, Map.lookup name stmtMap) | name <- Set.toAscList selected ]
          unusable = [ name | (name, indices) <- owners, maybe True null indices ]
      unless (null unusable) $
        invalidModuleRows
          ("selected names have no statement rows: " ++
           Data.List.intercalate ", " (map A.rawstr unusable))
      let indices = IntSet.toAscList $ IntSet.fromList $
            mandatory ++ concat [ owned | (_, Just owned) <- owners ]
      stored <- mapM (readStoredStmtEntry txn dbi) indices
      restored <- mapM
        (restoreSelectedStoredStmt txn dbi selected interests
          staticInitializerInterests instanceInitializerInterests)
        stored
      let statements = [ stmt | Just stmt <- restored ]
      traceTydbRead "selection" f
        (show (Set.size selected) ++ " tops, " ++
         show (sum (map Set.size (Map.elems interests))) ++ " members")
      return (A.Module moduleName imports doc statements)

extensionIndexFromNameInfo :: A.ModName -> I.NModule -> ExtensionIndex
extensionIndexFromNameInfo mn (I.NModule _ te _) =
    ExtensionIndex
      { extByClass = Map.map Set.toAscList classes
      , extByProtocol = Map.map Set.toAscList protocols
      }
  where
    (classes,protocols) = foldl' addExt (Map.empty,Map.empty) te

    addExt (classes0,protocols0) (ext, I.NExt _ c ps _ _ _) =
      let cls = localQName (A.tcname c)
          protos = [ p | (_, pcon) <- ps, Just p <- [localQName (A.tcname pcon)] ]
          classes1 = maybe classes0 (\n -> insert n ext classes0) cls
          protocols1 = foldl' (\index p -> insert p ext index) protocols0 protos
      in classes1 `seq` protocols1 `seq` (classes1,protocols1)
    addExt indexes _ = indexes

    localQName qn =
      case qn of
        A.NoQ n -> Just n
        A.QName m n | m == mn -> Just n
        A.GName m n | m == mn -> Just n
        _ -> Nothing

    insert name ext = Map.insertWith Set.union name (Set.singleton ext)

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

depIndexEntries :: [DepModuleInfo] -> [NameHashInfo] -> ModuleHashInfo -> [(BS.ByteString, BS.ByteString)]
depIndexEntries depModules nameHashes moduleHash =
    (keyDeps, encodeStrict depModules)
    : [ (keyDepModule mn, encodeStrict infos)
      | (mn, infos) <- moduleRows
      ]
    ++
    [ (keyDepName mn n, encodeStrict users)
    | ((mn, n), users) <- userRows
    ]
  where
    sortModRows = Data.List.sortOn fst
    sortNameInfos = Data.List.sortOn dniName
    sortUserRows = Data.List.sortOn fst

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
            then (Just h, Nothing, Set.singleton owner, Set.empty)
            else (Nothing, Just h, Set.empty, Set.singleton owner)
        addToEntry (Just (pubH, implH, pubUsers, implUsers)) =
          if isPub
            then (mergeHash "pub" qn pubH h, implH, Set.insert owner pubUsers, implUsers)
            else (pubH, mergeHash "impl" qn implH h, pubUsers, Set.insert owner implUsers)

    addModuleDep isPub acc (qn, h) =
      case depTarget qn of
        Nothing -> acc
        Just key' -> Map.alter (Just . addToEntry) key' acc
      where
        addToEntry Nothing =
          if isPub
            then (Just h, Nothing, Set.empty, Set.empty)
            else (Nothing, Just h, Set.empty, Set.empty)
        addToEntry (Just (pubH, implH, pubUsers, implUsers)) =
          if isPub
            then (mergeHash "pub" qn pubH h, implH, pubUsers, implUsers)
            else (pubH, mergeHash "impl" qn implH h, pubUsers, implUsers)

    mergeHash _ _ Nothing h = Just h
    mergeHash label qn (Just old) h
      | old == h = Just old
      | otherwise = error ("Inconsistent " ++ label ++ " dependency hashes for " ++ show qn)

    depMap =
      foldl' (addModuleDep False)
        (foldl' (addModuleDep True)
          (foldl' addInfo Map.empty nameHashes)
          (mhPubDeps moduleHash))
        (mhImplDeps moduleHash)

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

    cleanNames = Set.toList

    userRows =
      sortUserRows
        [ (key', DepUsers (cleanNames pubUsers) (cleanNames implUsers))
        | (key', (_pubH, _implH, pubUsers, implUsers)) <- Map.toList depMap
        ]

-- Narrow per-module query indexes, so dependent modules can answer solver and
-- environment queries (attribute owners, descendants, extensions) without
-- enumerating the whole public interface.
data QueryIndexes = QueryIndexes
  { qiPublicNames :: [A.Name]
  , qiConstructors :: [A.Name]
  , qiActors :: [A.Name]
  , qiConAttrs :: Map.Map A.Name [A.Name]
  , qiProtoAttrs :: Map.Map A.Name [A.Name]
  , qiDescendants :: Map.Map A.QName [A.Name]
  , qiExtProtos :: Map.Map A.QName [A.Name]
  , qiExtTypes :: Map.Map A.QName [A.Name]
  }

queryIndexes :: I.NModule -> QueryIndexes
queryIndexes (I.NModule _ te _) = QueryIndexes (map fst pte) cons actors conattrs protoattrs descendants extprotos exttypes
  where pte                    = filter (isPublicName . fst) te
        cons                   = [ n | (n, i) <- pte, isCons i ]
        actors                 = [ n | (n, I.NAct{}) <- pte ]
        conattrs               = indexMap [ (a, n) | (n, i) <- pte, isCon i, a <- attrs i ]
        protoattrs             = indexMap [ (a, n) | (n, i@I.NProto{}) <- pte, a <- attrs i ]
        descendants            = indexMap [ (A.tcname c, n) | (n, i) <- pte, (_, c) <- ancestry i ]
        extprotos              = indexMap [ (A.tcname p, n) | (n, I.NExt _ _ ps _ _ _) <- pte, (_, p) <- ps ]
        exttypes               = indexMap [ (A.tcname c, n) | (n, I.NExt _ c _ _ _ _) <- pte ]
        isCons I.NClass{}      = True
        isCons I.NProto{}      = True
        isCons I.NAct{}        = True
        isCons I.NType{}       = True
        isCons _               = False
        isCon I.NClass{}       = True
        isCon I.NAct{}         = True
        isCon _                = False
        attrs (I.NClass _ _ te' _) = map fst te'
        attrs (I.NProto _ _ te' _) = map fst te'
        attrs (I.NAct _ _ _ te' _) = map fst te'
        attrs _                = []
        ancestry (I.NClass _ us _ _) = us
        ancestry (I.NProto _ us _ _) = us
        ancestry _             = []

indexMap :: Ord k => [(k, A.Name)] -> Map.Map k [A.Name]
indexMap xs = Map.fromListWith (++) [ (k, [v]) | (k, v) <- reverse xs ]

queryIndexEntries :: I.NModule -> [(BS.ByteString, BS.ByteString)]
queryIndexEntries nmod =
    [ (keyPublicNames, encodeStrict (qiPublicNames ix))
    , (keyConstructors, encodeStrict (qiConstructors ix))
    , (keyActors, encodeStrict (qiActors ix))
    ]
    ++ [ (keyConAttr n, encodeStrict (n,ns)) | (n, ns) <- Map.toList (qiConAttrs ix) ]
    ++ [ (keyProtoAttr n, encodeStrict (n,ns)) | (n, ns) <- Map.toList (qiProtoAttrs ix) ]
    ++ [ (keyDescendants qn, encodeStrict (stripQNameKeyLocs qn,ns)) | (qn, ns) <- Map.toList (qiDescendants ix) ]
    ++ [ (keyExtProto qn, encodeStrict (stripQNameKeyLocs qn,ns)) | (qn, ns) <- Map.toList (qiExtProtos ix) ]
    ++ [ (keyExtType qn, encodeStrict (stripQNameKeyLocs qn,ns)) | (qn, ns) <- Map.toList (qiExtTypes ix) ]
  where ix = queryIndexes nmod

-- Statement ownership is prepared before container declarations are replaced
-- by shape references. Each top-level name records the indexes of its original
-- typed statements, preserving the existing atomicity of multi-name rows.
addStmtIndices :: [[A.Name]] -> [NameHashInfo] -> [NameHashInfo]
addStmtIndices stmtOwners nameHashes =
    [ nh { nhStmtIndices = Map.findWithDefault [] (nhName nh) owners }
    | nh <- nameHashes
    ]
  where
    owners = Map.map IntSet.toAscList $
             foldl' addStmt Map.empty (zip [0..] stmtOwners)
    addStmt acc (i, ns) =
      foldl' (\m n -> Map.insertWith IntSet.union n (IntSet.singleton i) m) acc ns

invalidModuleRows :: String -> IO a
invalidModuleRows msg =
    E.throwIO (E.ErrorCall ("Invalid prepared .tydb rows: " ++ msg))

validateModuleRows :: [NameHashInfo] -> InterfaceRows -> IO ()
validateModuleRows nameHashes rows = do
    case Rows.validateInterfaceRows rows of
      Left (Rows.RowError msg) -> invalidModuleRows msg
      Right () -> return ()
    let bodyMembers =
          [ (owner, member)
          | (owner, members) <- Map.toList (rowMembers rows)
          , member <- Map.keys members
          ]
        shapeStorageKeys = map keyContainerShape (Map.keys (rowShapes rows))
        memberStorageKeys = [ keyMemberBody owner member | (owner, member) <- bodyMembers ]
    when (length shapeStorageKeys /= Set.size (Set.fromList shapeStorageKeys)) $
      invalidModuleRows "semantic container shape keys collide in storage"
    when (length memberStorageKeys /= Set.size (Set.fromList memberStorageKeys)) $
      invalidModuleRows "semantic member keys collide in storage"
    let hashNames = Set.fromList (map nhName nameHashes)
        ownerNames = Set.fromList (concatMap Rows.storedStmtNames (rowStatements rows))
        unknown = Set.toAscList (ownerNames `Set.difference` hashNames)
    unless (null unknown) $
      invalidModuleRows
        ("statement owners have no name hash: " ++
         Data.List.intercalate ", " (map A.rawstr unknown))

reachRowOwners :: ReachRows.ReachabilityRows -> [ReachRows.TopKey]
reachRowOwners rows =
    Map.keys (ReachRows.reachTopRows rows) ++
    [ owner | (owner, _) <- Map.keys (ReachRows.reachMemberRows rows) ] ++
    Map.keys (ReachRows.reachShapeRows rows) ++
    [ owner | (owner, _) <- Map.keys (ReachRows.reachSlotRows rows) ] ++
    Map.keys (ReachRows.reachReflectableRows rows)

reachStorageKeys :: A.ModName -> ReachRows.ReachabilityRows -> [BS.ByteString]
reachStorageKeys moduleName rows =
    keyReachModule moduleName :
    [ keyReachTop owner
    | owner <- Map.keys (ReachRows.reachTopRows rows)
    ] ++
    [ keyReachMember owner member
    | (owner, member) <- Map.keys (ReachRows.reachMemberRows rows)
    ] ++
    [ keyReachShape owner
    | owner <- Map.keys (ReachRows.reachShapeRows rows)
    ] ++
    [ keyReachSlot owner member
    | (owner, member) <- Map.keys (ReachRows.reachSlotRows rows)
    ] ++
    [ keyReachReflection owner
    | owner <- Map.keys (ReachRows.reachReflectableRows rows)
    ]

validateReachabilityRows :: [NameHashInfo]
                         -> A.ModName
                         -> ReachRows.ReachabilityRows
                         -> IO ()
validateReachabilityRows nameHashes moduleName rows = do
    let owners = Set.fromList (reachRowOwners rows)
        foreignOwners =
          [ owner
          | owner@(ReachRows.TopKey ownerModule _) <- Set.toAscList owners
          , ownerModule /= moduleName
          ]
        knownNames = Set.fromList (map nhName nameHashes)
        unknownOwners =
          [ owner
          | owner@(ReachRows.TopKey _ name) <- Set.toAscList owners
          , not (Set.member name knownNames)
          ]
        storageKeys = reachStorageKeys moduleName rows
    unless (null foreignOwners) $
      invalidModuleRows
        ("reachability rows belong to another module: " ++
         Data.List.intercalate ", " (map topKeyLabel foreignOwners))
    unless (null unknownOwners) $
      invalidModuleRows
        ("reachability owners have no name hash: " ++
         Data.List.intercalate ", " (map topKeyLabel unknownOwners))
    forM_ (Map.toList (ReachRows.reachShapeRows rows)) $ \(owner, info) ->
      unless (ReachRows.shapeName info == owner) $
        invalidModuleRows ("reachability shape owner mismatch for " ++ topKeyLabel owner)
    when (length storageKeys /= Set.size (Set.fromList storageKeys)) $
      invalidModuleRows "semantic reachability keys collide in storage"

interfaceEntries :: (String -> Double -> IO ()) -> [Int] -> InterfaceContents -> IO [(BS.ByteString, BS.ByteString)]
interfaceEntries onProgress version contents = do
    validateModuleRows nameHashes rows
    validateReachabilityRows nameHashes (rowModuleName rows) reachRows
    generation <- newInterfaceGeneration
    caps <- getNumCapabilities
    let header =
          [ (keyVersion, encodeStrict version)
          , (keyGeneration, encodeStrict generation)
          , (keyMeta, encodeStrict (sourceMeta, moduleSrcBytesHash, modulePubHash, moduleImplHash))
          , (keyImports, encodeStrict imps)
          , (keyRoots, encodeStrict roots)
          , (keyTests, encodeStrict tests)
          , (keyDoc, encodeStrict mdoc)
          , (keyNameCount, encodeStrict (length te))
          , (keyStmtCount, encodeStrict (length statements))
          , (keyStmtMandatory, encodeStrict mandatoryStatementIndices)
          , (keyStmtHasNotImpl, encodeStrict (rowHasNotImpl rows))
          , (keyModuleHash, encodeStrict moduleHashInfo)
          , (keyModuleHeader, encodeStrict (tmn, timps, tdoc))
          ]
        nameChunks = entryChunks caps (zip [0..] te)
        nameHashChunks = entryChunks caps (addStmtIndices (map Rows.storedStmtNames statements) nameHashes)
        shapeChunks = entryChunks caps (Map.elems (rowShapes rows))
        memberBodyChunks = entryChunks caps memberContents
        reachTopChunks = entryChunks caps (Map.toList (ReachRows.reachTopRows reachRows))
        reachMemberChunks = entryChunks caps (Map.toList (ReachRows.reachMemberRows reachRows))
        reachShapeChunks = entryChunks caps (Map.toList (ReachRows.reachShapeRows reachRows))
        reachSlotChunks = entryChunks caps (Map.toList (ReachRows.reachSlotRows reachRows))
        reachReflectionChunks = entryChunks caps (Map.toList (ReachRows.reachReflectableRows reachRows))
        stmtChunks = entryChunks caps (zip [0..] statements)
        totalPrep = 5 + length nameChunks + length nameHashChunks
                      + length shapeChunks + length memberBodyChunks
                      + length reachTopChunks
                      + length reachMemberChunks
                      + length reachShapeChunks + length reachSlotChunks
                      + length reachReflectionChunks + length stmtChunks
    prepDone <- newIORef (0 :: Int)
    let mark label = do
          done <- atomicModifyIORef' prepDone $ \n ->
            let n' = n + 1 in (n', n')
          onProgress label (fromIntegral done / fromIntegral totalPrep)
    onProgress "Preparing .tydb" 0
    headerEntries <- forceEntries header
    mark "Preparing .tydb header"
    depEntries <- forceEntries (depIndexEntries depModules nameHashes moduleHashInfo)
    mark "Preparing .tydb deps"
    queryEntries <- forceEntries (queryIndexEntries nmod)
    mark "Preparing .tydb query indexes"
    reachModuleEntries <- forceEntries
      [(keyReachModule tmn,
        encodeStrict (ReachModuleRow tmn
          (ReachRows.reachModuleSummary reachRows)
          (ReachRows.reachWholeSummary reachRows)))]
    mark "Preparing .tydb module reachability"
    nameEntries <- parallelEntries mark "Preparing .tydb names" nameInfoEntries nameChunks
    nameHashEntries <- parallelEntries mark "Preparing .tydb hashes" nameHashEntry nameHashChunks
    shapeEntries <- parallelEntries mark "Preparing .tydb shapes" shapeEntry shapeChunks
    memberBodyEntries <- parallelEntries mark "Preparing .tydb member bodies" memberBodyEntry memberBodyChunks
    reachTopEntries <- parallelEntries mark "Preparing .tydb reachability tops" reachTopEntry reachTopChunks
    reachMemberEntries <- parallelEntries mark "Preparing .tydb reachability members" reachMemberEntry reachMemberChunks
    reachShapeEntries <- parallelEntries mark "Preparing .tydb reachability shapes" reachShapeEntry reachShapeChunks
    reachSlotEntries <- parallelEntries mark "Preparing .tydb reachability slots" reachSlotEntry reachSlotChunks
    reachReflectionEntries <- parallelEntries mark "Preparing .tydb reachability reflection" reachReflectionEntry reachReflectionChunks
    extEntries <- forceEntries (extensionIndexEntries (extensionIndexFromNameInfo tmn nmod))
    mark "Preparing .tydb extensions"
    stmtEntries <- parallelEntries mark "Preparing .tydb statements" stmtEntry stmtChunks
    return (headerEntries ++ depEntries ++ queryEntries ++ nameEntries ++ nameHashEntries
            ++ shapeEntries ++ memberBodyEntries
            ++ reachModuleEntries ++ reachTopEntries
            ++ reachMemberEntries ++ reachShapeEntries
            ++ reachSlotEntries ++ reachReflectionEntries
            ++ extEntries ++ stmtEntries)
  where
    moduleSrcBytesHash = ifcSourceHash contents
    modulePubHash = ifcPublicHash contents
    moduleImplHash = ifcImplementationHash contents
    moduleHashInfo = ifcModuleHashInfo contents
    sourceMeta = ifcSourceMeta contents
    imps = ifcImports contents
    depModules = ifcDependencies contents
    nameHashes = ifcNameHashes contents
    roots = ifcRoots contents
    tests = ifcTests contents
    mdoc = ifcDoc contents
    nmod = ifcModule contents
    rows = ifcRows contents
    reachRows = ifcReachabilityRows contents
    I.NModule _ te _ = nmod
    tmn = rowModuleName rows
    timps = rowImports rows
    tdoc = rowDoc rows
    statements = rowStatements rows
    mandatoryStatementIndices :: [Int]
    mandatoryStatementIndices =
      [ i
      | (i, StoredWhole owners _) <- zip [0..] statements
      , null owners
      ]
    memberContents =
      [ (owner, member, content)
      | (owner, members) <- Map.toList (rowMembers rows)
      , (member, content) <- Map.toList members
      ]
    -- ModuleInfo's keyed environment, like the previous LMDB overwrite
    -- layout, gives the last top-level occurrence precedence.  Keep the
    -- direct index identical while the ordered rows preserve every
    -- signature/definition occurrence for full reconstruction.
    primaryNameIndices = Map.fromListWith max
      [ (name,i) | (i,(name,_)) <- zip [0..] te ]
    nameInfoEntries (i, entry@(n, _)) =
      [ (keyNameOrder i, encodeStrict rowKey)
      , (rowKey, encodeStrict entry)
      ] ++
      [ (keyNameInfo n, encodeStrict entry)
      | Map.lookup n primaryNameIndices == Just i
      ]
      where
        rowKey = keyNameInfoOrder i
    nameHashEntry nh = [(keyNameHash (nhName nh), encodeStrict (stripExternalDeps nh))]
    shapeEntry shape =
      [(keyContainerShape (shapeName shape), encodeStrict shape)]
    memberBodyEntry (owner, member, content) =
      [(keyMemberBody owner member, encodeStrict (MemberContentRow owner member content))]
    reachTopEntry (owner, info) =
      [(keyReachTop owner, encodeStrict (ReachTopRow owner info))]
    reachMemberEntry ((owner, member), info) =
      [(keyReachMember owner member, encodeStrict (ReachMemberRow owner member info))]
    reachShapeEntry (owner, info) =
      [(keyReachShape owner, encodeStrict (ReachShapeRow owner info))]
    reachSlotEntry ((owner, member), info) =
      [(keyReachSlot owner member, encodeStrict (ReachSlotRow owner member info))]
    reachReflectionEntry (owner, attrs) =
      [(keyReachReflection owner, encodeStrict (ReachReflectionRow owner attrs))]
    stmtEntry (i, stmt) = [(keyStmt i, encodeStrict stmt)]

-- | Update only the cached source-file metadata in the header, leaving every
-- other row (name hashes, dep index rows, statements) untouched. Rewriting the
-- whole file for a pure metadata drift would be both wasteful (the module
-- content is unchanged; the file can be gigabytes) and LOSSY: a full write
-- re-derives the dependency index rows from the per-name hashes, whose external
-- deps are stripped on disk, so the rewritten rows would silently lose their
-- users and hashes -- degrading later per-name change detection.
updateSourceMeta :: FilePath -> Maybe SourceFileMeta -> IO ()
updateSourceMeta f sourceMeta = do
    size <- readMapSize f
    withWriteTxn f size $ \txn dbi -> do
      validateVersion txn dbi
      (_oldMeta, srcH, pubH, implH) <- readMeta txn dbi
      putValue txn dbi keyMeta (encodeStrict (sourceMeta, srcH, pubH, implH))

-- | Update the source hash and its complete per-name dependency/hash rows
-- without rewriting typed content. The semantic name set must stay unchanged:
-- this operation deliberately updates metadata for existing content rather
-- than pretending a different module body is stored under the same interface.
updateSourceHashAndNameHashes :: FilePath
                              -> BS.ByteString
                              -> [DepModuleInfo]
                              -> [NameHashInfo]
                              -> IO ()
updateSourceHashAndNameHashes f moduleSrcBytesHash depModules nameHashes = do
    moduleHashInfo <- readModuleHashInfo f
    update moduleHashInfo
  where
    update moduleHashInfo = do
      validateEntryKeys (depEntries ++ nameEntries)
      generation <- newInterfaceGeneration
      extra <- entryMapSize (depEntries ++ nameEntries)
      existing <- readMapSize f
      go generation (existing + extra)
      where
        depPrefix = key "deps/"
        depEntries = depIndexEntries depModules nameHashes moduleHashInfo
        desiredDepKeys = Set.fromList
          [ k | (k, _) <- depEntries, depPrefix `BS.isPrefixOf` k ]
        nameEntries =
          [ (keyNameHash (nhName nh), encodeStrict (stripExternalDeps nh))
          | nh <- nameHashes
          ]
        desiredNameKeys = Set.fromList (map fst nameEntries)

        go generation size = do
          res <- E.try $ withWriteTxn f size $ \txn dbi -> do
            validateVersion txn dbi
            existingNameKeys <- Set.fromList <$> getKeysWithPrefix txn dbi keyNameHashPrefix
            unless (existingNameKeys == desiredNameKeys) $
              invalidModuleRows "source-hash refresh changes the semantic name set"
            existingDepKeys <- Set.fromList <$> getKeysWithPrefix txn dbi depPrefix
            let staleDepKeys = Set.toAscList (existingDepKeys `Set.difference` desiredDepKeys)
            (sourceMeta, _oldSrcH, pubH, implH) <- readMeta txn dbi
            putValueIfChanged txn dbi keyMeta
              (encodeStrict (sourceMeta :: Maybe SourceFileMeta, moduleSrcBytesHash, pubH, implH))
            mapM_ (deleteValue txn dbi) staleDepKeys
            mapM_ (uncurry (putValueIfChanged txn dbi)) depEntries
            mapM_ (uncurry (putValueIfChanged txn dbi)) nameEntries
            putValue txn dbi keyGeneration (encodeStrict generation)
          case res of
            Right () -> return ()
            Left err | isMapFull err -> go generation (size * 2)
            Left (err :: E.SomeException) -> E.throwIO err

-- | Apply an impl-hash refresh surgically: update the meta row, upsert the
-- dependency index rows and rewrite only the per-name rows whose stored
-- encoding actually changed. Everything else (module header, statements,
-- imports, roots, tests, doc) is left untouched. An impl refresh changes
-- hashes, never content, so rewriting the whole file -- gigabytes for large
-- generated modules -- is pure waste. The dep index row KEYS are stable here
-- (the module's own source is unchanged, so it depends on the same names);
-- only hash values inside the rows move, hence upserting is complete.
updateImplRefresh :: FilePath -> ImplRefreshInput -> ImplRefreshOutput -> IO ()
updateImplRefresh f input output = do
    validateEntryKeys updatedEntries
    generation <- newInterfaceGeneration
    extra <- entryMapSize updatedEntries
    existing <- readMapSize f
    go generation (existing + extra)
  where
    moduleImplHash = iroImplementationHash output
    moduleHashInfo = iroModuleHashInfo output
    depModules = iroDependencies output
    nameHashes = iroNameHashes output
    updatedEntries = (keyModuleHash, encodeStrict moduleHashInfo) : depEntries ++ nameEntries
    depEntries = depIndexEntries depModules nameHashes moduleHashInfo
    nameEntries = [ (keyNameHash (nhName nh), encodeStrict (stripExternalDeps nh)) | nh <- nameHashes ]
    go generation size = do
      res <- E.try $ withWriteTxn f size $ \txn dbi -> do
        (oldMeta, srcH, pubH, oldImplH) <- readMeta txn dbi
        oldGeneration <- getValue "generation" txn dbi keyGeneration
        unless (oldGeneration == iriGeneration input
                && srcH == iriSourceHash input
                && pubH == iriPublicHash input
                && oldImplH == iriImplementationHash input) $
          E.throwIO (ImplRefreshStale
            "Interface changed while implementation hashes were refreshed")
        putValue txn dbi keyMeta (encodeStrict (oldMeta :: Maybe SourceFileMeta, srcH, pubH, moduleImplHash))
        putValueIfChanged txn dbi keyModuleHash (encodeStrict moduleHashInfo)
        mapM_ (uncurry (putValueIfChanged txn dbi)) depEntries
        mapM_ (uncurry (putValueIfChanged txn dbi)) nameEntries
        putValue txn dbi keyGeneration (encodeStrict generation)
      case res of
        Right () -> return ()
        Left err | isMapFull err -> go generation (size * 2)
        Left (err :: E.SomeException) -> E.throwIO err

-- | Change the cache format version and generation marker, preserving every
-- semantic content and index row. This is intentionally narrow and primarily
-- useful to verify stale-file handling without materializing and rewriting a
-- potentially huge interface.
updateVersion :: FilePath -> [Int] -> IO ()
updateVersion f version = do
    generation <- newInterfaceGeneration
    extra <- entryMapSize [versionEntry,generationEntry generation]
    existing <- readMapSize f
    go generation (existing + extra)
  where
    versionEntry = (keyVersion, encodeStrict version)
    generationEntry generation = (keyGeneration, encodeStrict generation)
    go generation size = do
      res <- E.try $ withWriteTxn f size $ \txn dbi -> do
        validateVersion txn dbi
        uncurry (putValueIfChanged txn dbi) versionEntry
        uncurry (putValue txn dbi) (generationEntry generation)
      case res of
        Right () -> return ()
        Left err | isMapFull err -> go generation (size * 2)
        Left (err :: E.SomeException) -> E.throwIO err

-- | Write a row only when its bytes differ from what is stored, keeping
-- surgical updates from touching (and growing) unchanged pages.
putValueIfChanged :: LMDB.MDB_txn -> LMDB.MDB_dbi -> BS.ByteString -> BS.ByteString -> IO ()
putValueIfChanged txn dbi k v = do
    mOld <- withVal k (LMDB.mdb_get txn dbi)
    changed <- case mOld of
      Nothing -> return True
      Just old -> (/= v) <$> copyVal old
    when changed (putValue txn dbi k v)

writeFile :: (TyDbWriteProgress -> IO ()) -> FilePath -> InterfaceContents -> IO ()
writeFile onProgress = writeFileVersioned onProgress A.version

writeVersionedFile :: [Int] -> FilePath -> InterfaceContents -> IO ()
writeVersionedFile = writeFileVersioned (\_ -> return ())

writeFileVersioned :: (TyDbWriteProgress -> IO ()) -> [Int] -> FilePath -> InterfaceContents -> IO ()
writeFileVersioned onProgress version f contents = do
    entries <- interfaceEntries prepProgress version contents
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
      _mandatory <- getValue "stmt-mandatory" txn dbi keyStmtMandatory :: IO [Int]
      _hasNotImpl <- getValue "stmt-has-not-impl" txn dbi keyStmtHasNotImpl :: IO Bool
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

-- | Read whether the module contains NotImplemented statements (implemented
-- by a hand-written .ext.c); such modules are rendered whole, never DBP'd.
readStmtHasNotImpl :: FilePath -> IO Bool
readStmtHasNotImpl f =
    withReadTxn f $ \txn dbi -> do
      validateVersion txn dbi
      hasNotImpl <- getValue "stmt-has-not-impl" txn dbi keyStmtHasNotImpl
      traceTydbRead "stmt-has-not-impl" f (show hasNotImpl)
      return hasNotImpl

-- | Read every per-name hash row of a module (its own rows, not a
-- dependency's) without touching the interface or statement rows.
readNameHashes :: FilePath -> IO [NameHashInfo]
readNameHashes f =
    withReadTxn f $ \txn dbi -> do
      validateVersion txn dbi
      nameHashes <- readNameHashEntries txn dbi
      traceTydbRead "name-hash-all" f (show (length nameHashes))
      return nameHashes

-- | Read the mandatory module-initialization hash component without loading
-- any typed statement or per-name hash rows.
readModuleHashInfo :: FilePath -> IO ModuleHashInfo
readModuleHashInfo f =
    withReadTxn f $ \txn dbi -> do
      validateVersion txn dbi
      info <- getValue "module-hash" txn dbi keyModuleHash
      traceTydbRead "module-hash" f "cached"
      return info

-- | Read a module's interface (imports and name infos) without decoding the
-- typed statement rows -- for consumers that only need the NModule, an eager
-- readFile pays for deserializing the entire typed module.
readModuleIface :: FilePath -> IO ([A.ModName], I.NModule)
readModuleIface f =
    withReadTxn f $ \txn dbi -> do
      validateVersion txn dbi
      result@(_, I.NModule _ te _) <- readModuleIfaceEntries txn dbi
      traceTydbRead "iface" f ("names " ++ show (length te))
      return result

readModuleIfaceEntries :: LMDB.MDB_txn
                       -> LMDB.MDB_dbi
                       -> IO ([A.ModName], I.NModule)
readModuleIfaceEntries txn dbi = do
    _mandatory <- getValue "stmt-mandatory" txn dbi keyStmtMandatory :: IO [Int]
    _hasNotImpl <- getValue "stmt-has-not-impl" txn dbi keyStmtHasNotImpl :: IO Bool
    mdoc <- getValue "doc" txn dbi keyDoc
    (te, _nameHashes) <- readNameEntries txn dbi
    (tmn, timps, tdoc) <- getValue "module-header" txn dbi keyModuleHeader
    let sourceImps = A.importsOf (A.Module tmn timps tdoc [])
    return (sourceImps, I.NModule sourceImps te mdoc)

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
      _mandatory <- getValue "stmt-mandatory" txn dbi keyStmtMandatory :: IO [Int]
      _hasNotImpl <- getValue "stmt-has-not-impl" txn dbi keyStmtHasNotImpl :: IO Bool
      nameHashes <- readNameHashEntries txn dbi
      traceTydbRead "header" f "cached"
      traceTydbRead "name-hash-all" f (show (length nameHashes))
      return (sourceMeta, moduleSrcBytesHash, modulePubHash, moduleImplHash, imps, depModules, nameHashes, roots, tests, doc)

-- Like readHeader, but reads the stored name count instead of decoding every
-- per-name hash row, so cache checks stay independent of module size.
readHeaderSummary :: FilePath -> IO TyHeaderSummary
readHeaderSummary f =
    withReadTxn f $ \txn dbi -> do
      (sourceMeta, moduleSrcBytesHash, modulePubHash, moduleImplHash) <- readMeta txn dbi
      imps <- getValue "imports" txn dbi keyImports
      depModules <- getValue "deps" txn dbi keyDeps
      roots <- getValue "roots" txn dbi keyRoots
      tests <- getValue "tests" txn dbi keyTests
      doc <- getValue "doc" txn dbi keyDoc
      nameCount <- getValue "name-count" txn dbi keyNameCount
      _mandatory <- getValue "stmt-mandatory" txn dbi keyStmtMandatory :: IO [Int]
      _hasNotImpl <- getValue "stmt-has-not-impl" txn dbi keyStmtHasNotImpl :: IO Bool
      traceTydbRead "header" f "cached"
      return (sourceMeta, moduleSrcBytesHash, modulePubHash, moduleImplHash, imps, depModules, nameCount, roots, tests, doc)

readRoots :: FilePath -> IO [A.Name]
readRoots f =
    withReadTxn f $ \txn dbi -> do
      validateVersion txn dbi
      getValue "roots" txn dbi keyRoots

readDepNames :: FilePath -> A.ModName -> IO [DepNameInfo]
readDepNames f mn =
    withReadTxn f $ \txn dbi -> do
      validateVersion txn dbi
      deps <- readDepNameEntries txn dbi mn
      traceTydbRead "dep-names" f (Data.List.intercalate "." (A.modPath mn) ++ " " ++ show (length deps))
      return deps

readDepNameEntries :: LMDB.MDB_txn -> LMDB.MDB_dbi -> A.ModName -> IO [DepNameInfo]
readDepNameEntries txn dbi mn = do
    mDeps <- getMaybeValue
      ("deps/" ++ Data.List.intercalate "." (A.modPath mn))
      txn dbi (keyDepModule mn)
    return (maybe [] id mDeps)

readDepUsers :: FilePath -> A.ModName -> A.Name -> IO DepUsers
readDepUsers f mn n =
    withReadTxn f $ \txn dbi -> do
      validateVersion txn dbi
      users <- readDepUsersEntry txn dbi mn n
      traceTydbRead "dep-users" f (Data.List.intercalate "." (A.modPath mn) ++ "." ++ A.rawstr n)
      return users

readDepUsersEntry :: LMDB.MDB_txn -> LMDB.MDB_dbi -> A.ModName -> A.Name -> IO DepUsers
readDepUsersEntry txn dbi mn n = do
    mUsers <- getMaybeValue
      ("deps/" ++ Data.List.intercalate "." (A.modPath mn) ++ "/" ++ A.rawstr n)
      txn dbi (keyDepName mn n)
    return (maybe emptyDepUsers id mUsers)

readNameHash :: FilePath -> A.Name -> IO (Maybe NameHashInfo)
readNameHash f n =
    withReadTxn f $ \txn dbi -> do
      validateVersion txn dbi
      mInfo <- getMaybeValue ("name-hash/" ++ A.rawstr n) txn dbi (keyNameHash n)
      forM_ mInfo $ \info ->
        unless (nhName info == n) $
          invalidContentRow ("name-hash key/value mismatch for " ++ A.rawstr n)
      traceTydbRead (case mInfo of { Just _ -> "name-hash"; Nothing -> "name-hash-miss" }) f (A.rawstr n)
      return mInfo

readNameHashMaybe :: FilePath -> A.Name -> IO (Maybe NameHashInfo)
readNameHashMaybe f n =
    readNameHash f n `E.catch` tyCacheMiss

readContainerShape :: FilePath -> A.Name -> IO ContainerShape
readContainerShape f n =
    withReadTxn f $ \txn dbi -> do
      validateVersion txn dbi
      shape <- readContainerShapeEntry txn dbi n
      traceTydbRead "shape" f (A.rawstr n)
      return shape

readMemberContent :: FilePath -> A.Name -> MemberKey -> IO MemberContent
readMemberContent f owner member =
    withReadTxn f $ \txn dbi -> do
      validateVersion txn dbi
      content <- readMemberContentEntry txn dbi owner member
      traceTydbRead "body-member" f (memberLabel owner member)
      return content

readReachModule :: FilePath -> A.ModName -> IO Reach.ReachSummary
readReachModule f moduleName =
    withReadTxn f $ \txn dbi -> do
      validateVersion txn dbi
      (summary, _) <- readReachModuleEntry txn dbi moduleName
      traceTydbRead "reach-module" f
        (Data.List.intercalate "." $ A.modPath moduleName)
      return summary

readReachWholeModule :: FilePath -> A.ModName -> IO Reach.ReachSummary
readReachWholeModule f moduleName =
    withReadTxn f $ \txn dbi -> do
      validateVersion txn dbi
      (_, summary) <- readReachModuleEntry txn dbi moduleName
      traceTydbRead "reach-module-whole" f
        (Data.List.intercalate "." $ A.modPath moduleName)
      return summary

readReachTop :: FilePath -> ReachRows.TopKey -> IO ReachRows.TopInfo
readReachTop f owner =
    withReadTxn f $ \txn dbi -> do
      validateVersion txn dbi
      info <- readReachTopEntry txn dbi owner
      traceTydbRead "reach-top" f (topKeyLabel owner)
      return info

readReachTopMaybe :: FilePath -> ReachRows.TopKey -> IO (Maybe ReachRows.TopInfo)
readReachTopMaybe f owner =
    withReadTxn f $ \txn dbi -> do
      validateVersion txn dbi
      info <- readReachTopEntryMaybe txn dbi owner
      traceTydbRead (maybe "reach-top-miss" (const "reach-top") info) f (topKeyLabel owner)
      return info

readReachMember :: FilePath -> ReachRows.TopKey -> MemberKey -> IO ReachRows.MemberInfo
readReachMember f owner member =
    withReadTxn f $ \txn dbi -> do
      validateVersion txn dbi
      info <- readReachMemberEntry txn dbi owner member
      traceTydbRead "reach-member" f
        (topKeyLabel owner ++ "." ++ Rows.memberLabel member)
      return info

readReachMemberMaybe :: FilePath
                     -> ReachRows.TopKey
                     -> MemberKey
                     -> IO (Maybe ReachRows.MemberInfo)
readReachMemberMaybe f owner member =
    withReadTxn f $ \txn dbi -> do
      validateVersion txn dbi
      info <- readReachMemberEntryMaybe txn dbi owner member
      traceTydbRead (maybe "reach-member-miss" (const "reach-member") info) f
        (topKeyLabel owner ++ "." ++ Rows.memberLabel member)
      return info

readReachShape :: FilePath -> ReachRows.TopKey -> IO ReachRows.ShapeInfo
readReachShape f owner =
    withReadTxn f $ \txn dbi -> do
      validateVersion txn dbi
      info <- readReachShapeEntry txn dbi owner
      traceTydbRead "reach-shape" f (topKeyLabel owner)
      return info

readReachShapeMaybe :: FilePath -> ReachRows.TopKey -> IO (Maybe ReachRows.ShapeInfo)
readReachShapeMaybe f owner =
    withReadTxn f $ \txn dbi -> do
      validateVersion txn dbi
      info <- readReachShapeEntryMaybe txn dbi owner
      traceTydbRead (maybe "reach-shape-miss" (const "reach-shape") info) f (topKeyLabel owner)
      return info

readReachSlot :: FilePath
              -> ReachRows.TopKey
              -> Reach.MemberRef
              -> IO ReachRows.SlotInfo
readReachSlot f owner member =
    withReadTxn f $ \txn dbi -> do
      validateVersion txn dbi
      info <- readReachSlotEntry txn dbi owner member
      traceTydbRead "reach-slot" f (topKeyLabel owner ++ "." ++ memberRefLabel member)
      return info

readReachSlotMaybe :: FilePath
                   -> ReachRows.TopKey
                   -> Reach.MemberRef
                   -> IO (Maybe ReachRows.SlotInfo)
readReachSlotMaybe f owner member =
    withReadTxn f $ \txn dbi -> do
      validateVersion txn dbi
      info <- readReachSlotEntryMaybe txn dbi owner member
      traceTydbRead (maybe "reach-slot-miss" (const "reach-slot") info) f
        (topKeyLabel owner ++ "." ++ memberRefLabel member)
      return info

readReachSlots :: FilePath
               -> ReachRows.TopKey
               -> IO [(Reach.MemberRef, ReachRows.SlotInfo)]
readReachSlots f owner =
    withReadTxn f $ \txn dbi -> do
      validateVersion txn dbi
      slots <- readReachSlotsEntry txn dbi owner
      traceTydbRead "reach-slots" f (topKeyLabel owner)
      return slots

readReachReflection :: FilePath -> ReachRows.TopKey -> IO ReachRows.ReflectableAttrs
readReachReflection f owner =
    withReadTxn f $ \txn dbi -> do
      validateVersion txn dbi
      attrs <- readReachReflectionEntry txn dbi owner
      traceTydbRead "reach-reflection" f (topKeyLabel owner)
      return attrs

readReachReflectionMaybe :: FilePath
                         -> ReachRows.TopKey
                         -> IO (Maybe ReachRows.ReflectableAttrs)
readReachReflectionMaybe f owner =
    withReadTxn f $ \txn dbi -> do
      validateVersion txn dbi
      attrs <- readReachReflectionEntryMaybe txn dbi owner
      traceTydbRead (maybe "reach-reflection-miss" (const "reach-reflection") attrs) f
        (topKeyLabel owner)
      return attrs

readModuleHashesMaybe :: FilePath -> IO (Maybe (BS.ByteString, BS.ByteString, BS.ByteString))
readModuleHashesMaybe =
    readTyMaybe readModuleHashes

readModuleSnapshotMaybe :: FilePath -> IO (Maybe ModuleSnapshot)
readModuleSnapshotMaybe =
    readTyMaybe readModuleSnapshot

readExtensionsByClass :: FilePath -> A.Name -> IO [A.Name]
readExtensionsByClass f n =
    withReadTxn f $ \txn dbi -> do
      validateVersion txn dbi
      entry <- getMaybeValue "ext-by-class" txn dbi (keyExtByClass n) :: IO (Maybe (A.Name, [A.Name]))
      case entry of
        Nothing -> return []
        Just (stored, exts)
          | stored == n -> return exts
          | otherwise -> invalidContentRow
              ("ext-by-class key/value mismatch for " ++ A.rawstr n)

readExtensionsByProtocol :: FilePath -> A.Name -> IO [A.Name]
readExtensionsByProtocol f n =
    withReadTxn f $ \txn dbi -> do
      validateVersion txn dbi
      entry <- getMaybeValue "ext-by-protocol" txn dbi (keyExtByProtocol n) :: IO (Maybe (A.Name, [A.Name]))
      case entry of
        Nothing -> return []
        Just (stored, exts)
          | stored == n -> return exts
          | otherwise -> invalidContentRow
              ("ext-by-protocol key/value mismatch for " ++ A.rawstr n)

-- Interface files are caches for most callers. If a file is missing,
-- unreadable, corrupt, or from a different compiler interface version, the
-- cache entry is not usable.
readFileMaybe :: FilePath -> IO (Maybe TyFile)
readFileMaybe = readTyMaybe readFile

readHeaderMaybe :: FilePath -> IO (Maybe TyHeader)
readHeaderMaybe = readTyMaybe readHeader

readHeaderSummaryMaybe :: FilePath -> IO (Maybe TyHeaderSummary)
readHeaderSummaryMaybe = readTyMaybe readHeaderSummary

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
