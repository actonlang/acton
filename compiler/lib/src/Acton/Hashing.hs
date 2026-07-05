module Acton.Hashing
  ( TopLevelItem(..)
  , topLevelItems
  , NameHashInputs(..)
    -- Production hashing pipeline
  , prepareNameHashInputs
  , assembleNameHashes
  , mergePubDeps
  , refreshImplHashes
  , moduleHashesFromHashMaps
  , modulePubHashFromIface
  , moduleImplHashFromNameHashes
    -- Benchmarks and tests use these to compare individual stages.
  , nameHashesFromItems
  , pubSigDepsFromNameInfoMap
  , pubSigSplitDepsFromNameInfoMap
  , implDepsFromItems
  , implSplitDepsFromItems
  , codeLocalDepsFromItems
  , codeMethodDepsFromItems
  , frontSplitDepsFromItems
  , frontSplitDepsFromItemsWithProgress
  , splitInitByAttr
  , splitDeps
  , externalModules
  , computeHashes
  , computeHashesSortedDeps
  , mergeSortedDistinct
  , nameInfoHashes
  , nameInfoHashesWithProgress
  , nameHashesFromItemsWithProgress
  , pubSigSplitDepsFromNameInfoMapWithProgress
  , implSplitDepsFromItemsWithProgress
  , buildNameHashes
  , nameKey
  , qnameKey
  ) where

import qualified Acton.Env as Env
import qualified Acton.NameInfo as I
import qualified Acton.Names as Names
import Acton.Prim (mPrim)
import qualified Acton.Syntax as A
import qualified InterfaceFiles
import Utils (chunksOf)

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Unsafe as BU
import Control.Monad (foldM, when)
import Data.Bits (shiftR, (.&.), (.|.))
import Data.Char (ord)
import Data.Graph (SCC(..), stronglyConnComp)
import qualified Data.HashSet as HashSet
import Data.IORef (IORef, newIORef, readIORef, writeIORef, atomicModifyIORef')
import Control.Concurrent.Async (mapConcurrently)
import GHC.Conc (getNumCapabilities)
import Data.List (foldl', intercalate, nub)
import qualified Data.List
import qualified Data.Map as M
import Data.Maybe (mapMaybe, isJust)
import Acton.Builtin (initKW)
import qualified Data.Set
import Data.Word (Word8, Word64)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peek, peekByteOff, poke, pokeByteOff)
import GHC.Float (castDoubleToWord64)
import System.IO.Unsafe (unsafePerformIO)


data TopLevelItem = TLDecl A.Name A.Decl | TLStmt A.Name A.Stmt

data TopLevelFragments = One !TopLevelItem | Many [TopLevelItem]

data NameHashInputs = NameHashInputs
  { nhiNameKeys        :: Data.Set.Set A.Name
  , nhiNameSrcHashes   :: M.Map A.Name B.ByteString
  , nhiNameImplHashes  :: M.Map A.Name B.ByteString
  , nhiSelfPubHashes   :: M.Map A.Name B.ByteString
  , nhiPubSigLocalDeps :: M.Map A.Name [A.Name]
  , nhiPubSigExtDeps   :: M.Map A.Name [A.QName]
  , nhiImplLocalDeps   :: M.Map A.Name [A.Name]
  , nhiImplExtDeps     :: M.Map A.Name [A.QName]
  }

-- | Render a local name as a stable string key.
nameKey :: A.Name -> String
nameKey = A.nstr

-- | Render a qualified name as a stable string key.
qnameKey :: A.QName -> String
qnameKey qn = case qn of
  A.GName m n -> modNameToString m ++ "." ++ A.nstr n
  A.QName m n -> modNameToString m ++ "." ++ A.nstr n
  A.NoQ n -> A.nstr n

-- | Render a module name as dot-separated text.
modNameToString :: A.ModName -> String
modNameToString m = intercalate "." (A.modPath m)

-- | Extract hashable top-level items from a module.
topLevelItems :: A.Module -> [TopLevelItem]
topLevelItems (A.Module _ _ _ suite) = concatMap items suite
  where
    items stmt = case stmt of
      A.Decl _ ds ->
        [ TLDecl (Names.dname' d) d | d <- ds ]
      A.Signature _ ns _ _ ->
        [ TLStmt n stmt | n <- ns ]
      A.Assign _ ps _ ->
        [ TLStmt n stmt | n <- nub (Names.bound ps) ]
      A.VarAssign _ ps _ ->
        [ TLStmt n stmt | n <- nub (Names.bound ps) ]
      _ -> []

-- The feed-operation format is versioned: small buffered writes, bulk hash
-- bytes, and little-endian words are distinct operations by design. Changing
-- this format must come with a .tydb version bump so old interface hashes are
-- not reused.
data HashSink = HashSink !(IORef SHA256.Ctx) !(Ptr Word8) !(Ptr Int)

type HashFeed = HashSink -> IO ()

-- | Hash each name's normalized AST fragments without pretty-printing them.
nameHashesFromItems :: [TopLevelItem] -> M.Map A.Name B.ByteString
nameHashesFromItems items =
  unsafePerformIO $ nameHashesFromItemsWithProgress (\_ -> pure ()) items

-- Each name's fragments hash independently, so the groups are split into one
-- chunk per capability, each with its own hashing scratch, and hashed
-- concurrently. The maps are disjoint, so the merged result is identical to
-- the sequential fold; progress counts through a shared atomic counter.
nameHashesFromItemsWithProgress :: (Int -> IO ()) -> [TopLevelItem] -> IO (M.Map A.Name B.ByteString)
nameHashesFromItemsWithProgress onProgress items = do
  caps <- getNumCapabilities
  let groups = M.toAscList grouped
      chunkSize = max 16 ((length groups + caps - 1) `max` 1 `div` max 1 caps)
      chunks = chunksOf (max 1 chunkSize) groups
  doneRef <- newIORef 0
  maps <- mapConcurrently (hashChunk doneRef) chunks
  pure (M.unions maps)
  where
    grouped = topLevelItemsByName items

    hashChunk doneRef chunk =
      withHashScratch $ \sink ->
        M.fromList <$> mapM (hashOne sink doneRef) chunk

    hashOne sink doneRef (n, frags) = do
      h <- hashTopLevelFragmentsWith sink frags
      done <- atomicModifyIORef' doneRef $ \d ->
        let d' = d + topLevelFragmentsLength frags in (d', d')
      onProgress done
      pure (n, h)

topLevelFragmentsLength :: TopLevelFragments -> Int
topLevelFragmentsLength frags = case frags of
  One _ -> 1
  Many items -> length items

topLevelItemsByName :: [TopLevelItem] -> M.Map A.Name TopLevelFragments
topLevelItemsByName = foldl' addFrag M.empty
  where
    addFrag acc item =
      M.insertWith addTopLevelFragment (topLevelItemName item) (One item) acc

    addTopLevelFragment new old = case (new, old) of
      (One item, One oldItem) -> Many [item, oldItem]
      (One item, Many oldItems) -> Many (item : oldItems)
      (Many items, One oldItem) -> Many (items ++ [oldItem])
      (Many items, Many oldItems) -> Many (items ++ oldItems)

topLevelItemName :: TopLevelItem -> A.Name
topLevelItemName item = case item of
  TLDecl name _ -> name
  TLStmt name _ -> name

feedTopLevelItem :: TopLevelItem -> HashFeed
feedTopLevelItem item sink = case item of
  TLDecl _ decl -> feedTLDecl decl sink
  TLStmt _ stmt -> feedTLStmt stmt sink

feedTopLevelFragments :: TopLevelFragments -> HashFeed
feedTopLevelFragments frags sink =
  feedTag 4 sink >> feedFragments frags >> feedTag 5 sink
  where
    feedFragments (One item) = feedTopLevelItem item sink
    feedFragments (Many items) = mapM_ (\item -> feedTopLevelItem item sink) (reverse items)

hashTopLevelFragmentsWith :: HashSink -> TopLevelFragments -> IO B.ByteString
hashTopLevelFragmentsWith sink frags =
  hashFeedWith sink (feedTopLevelFragments frags)

hashFeed :: HashFeed -> B.ByteString
hashFeed feed = unsafePerformIO $ withHashScratch $ \sink ->
  hashFeedWith sink feed
{-# NOINLINE hashFeed #-}

withHashScratch :: (HashSink -> IO a) -> IO a
withHashScratch action =
  do
    statep <- newIORef SHA256.init
    allocaBytes hashBufferSize $ \buf ->
      alloca $ \offp -> do
        poke offp 0
        action (HashSink statep buf offp)

hashFeedWith :: HashSink -> HashFeed -> IO B.ByteString
hashFeedWith sink@(HashSink statep _ offp) feed = do
  writeIORef statep SHA256.init
  poke offp 0
  feed sink
  sinkFlush sink
  SHA256.finalize <$> readIORef statep

feedTLDecl :: A.Decl -> HashFeed
feedTLDecl decl sink = feedTag 0 sink >> feedDecl decl sink

feedTLStmt :: A.Stmt -> HashFeed
feedTLStmt stmt sink = feedTag 1 sink >> feedStmt stmt sink

feedTag :: Int -> HashFeed
feedTag tag sink
  | tag >= 0 && tag <= 255 = sinkWriteWord8 sink (fromIntegral tag)
  | otherwise = error ("Acton.Hashing.feedTag: tag out of range: " ++ show tag)
{-# INLINE feedTag #-}

feedMaybe :: (a -> HashFeed) -> Maybe a -> HashFeed
feedMaybe _ Nothing  sink = feedTag 2 sink
feedMaybe f (Just x) sink = feedTag 3 sink >> f x sink

feedList :: (a -> HashFeed) -> [a] -> HashFeed
feedList f xs sink =
  feedTag 4 sink >> mapM_ (\x -> f x sink) xs >> feedTag 5 sink

feedString :: String -> HashFeed
feedString s sink = sinkWriteString sink s

feedInt :: Int -> HashFeed
feedInt i sink = sinkWriteInt sink i

feedInteger :: Integer -> HashFeed
feedInteger i sink = sinkWriteInteger sink i

feedDouble :: Double -> HashFeed
feedDouble d sink = sinkWriteWord64LE sink (castDoubleToWord64 d)

feedBool :: Bool -> HashFeed
feedBool b sink = sinkWriteWord8 sink (if b then 1 else 0)
{-# INLINE feedBool #-}

hashBufferSize :: Int
hashBufferSize = 32768

sinkFlush :: HashSink -> IO ()
sinkFlush (HashSink statep buf offp) = do
  off <- peek offp
  when (off > 0) $ do
    st <- readIORef statep
    -- SHA256.update consumes the bytes immediately, so the alloca-backed
    -- scratch buffer only needs to live for this call.
    bs <- BU.unsafePackCStringLen (castPtr buf, off)
    writeIORef statep $! SHA256.update st bs
    poke offp 0

sinkEnsure :: HashSink -> Int -> IO Int
sinkEnsure sink@(HashSink _ _ offp) n = do
  off <- peek offp
  if off + n <= hashBufferSize
    then pure off
    else sinkFlush sink >> pure 0

sinkWriteWord8 :: HashSink -> Word8 -> IO ()
sinkWriteWord8 sink@(HashSink _ buf offp) w = do
  off <- peek offp
  if off < hashBufferSize
    then do
      pokeByteOff buf off w
      poke offp (off + 1)
    else do
      sinkFlush sink
      pokeByteOff buf 0 w
      poke offp 1
{-# INLINE sinkWriteWord8 #-}

sinkWriteBytes :: HashSink -> B.ByteString -> IO ()
sinkWriteBytes sink@(HashSink statep _ _) bs = do
  sinkFlush sink
  st <- readIORef statep
  writeIORef statep $! SHA256.update st bs

sinkWriteInteger :: HashSink -> Integer -> IO ()
sinkWriteInteger sink i
  | i < 0     = sinkWriteWord8 sink 1 >> sinkWriteNatural sink (negate i)
  | otherwise = sinkWriteWord8 sink 0 >> sinkWriteNatural sink i

sinkWriteInt :: HashSink -> Int -> IO ()
sinkWriteInt sink i
  | i < 0     = sinkWriteWord8 sink 1 >> sinkWriteNatural sink (negate (toInteger i))
  | otherwise = sinkWriteWord8 sink 0 >> sinkWriteNatural sink (toInteger i)

sinkWriteNatural :: HashSink -> Integer -> IO ()
sinkWriteNatural sink i
  | i < 128   = sinkWriteWord8 sink (fromInteger i)
  | otherwise = do
      sinkWriteWord8 sink (fromInteger ((i .&. 0x7f) .|. 0x80))
      sinkWriteNatural sink (i `shiftR` 7)

sinkWriteWord64LE :: HashSink -> Word64 -> IO ()
sinkWriteWord64LE sink@(HashSink _ buf offp) w = do
  off <- sinkEnsure sink 8
  pokeByteOff buf off     (byteAt 0)
  pokeByteOff buf (off+1) (byteAt 8)
  pokeByteOff buf (off+2) (byteAt 16)
  pokeByteOff buf (off+3) (byteAt 24)
  pokeByteOff buf (off+4) (byteAt 32)
  pokeByteOff buf (off+5) (byteAt 40)
  pokeByteOff buf (off+6) (byteAt 48)
  pokeByteOff buf (off+7) (byteAt 56)
  poke offp (off + 8)
  where
    byteAt shift = fromIntegral ((w `shiftR` shift) .&. 0xff) :: Word8

sinkWriteString :: HashSink -> String -> IO ()
sinkWriteString sink@(HashSink _ buf offp) s = do
  off <- peek offp
  off' <- go off s
  off'' <- writeStringTerm off'
  poke offp off''
  where
    go off [] = pure off
    go off ('\0':cs) = do
      off' <- ensureLocal off 2
      pokeByteOff buf off' zeroByte
      pokeByteOff buf (off'+1) zeroByte
      go (off' + 2) cs
    go off (c:cs)
      | n <= 0x7f = do
          off' <- ensureLocal off 1
          pokeByteOff buf off' (fromIntegral n :: Word8)
          go (off' + 1) cs
      | otherwise = do
          off' <- ensureLocal off 4
          off'' <- writeUtf8Char buf off' c
          go off'' cs
      where
        n = ord c

    writeStringTerm off = do
      off' <- ensureLocal off 2
      pokeByteOff buf off' zeroByte
      pokeByteOff buf (off'+1) oneByte
      pure (off' + 2)

    ensureLocal off n
      | off + n <= hashBufferSize = pure off
      | otherwise = do
          poke offp off
          sinkFlush sink
          pure 0

    zeroByte = 0 :: Word8
    oneByte = 1 :: Word8

writeUtf8Char :: Ptr Word8 -> Int -> Char -> IO Int
writeUtf8Char p off c
  | n <= 0x7f = do
      pokeByteOff p off (fromIntegral n :: Word8)
      pure (off + 1)
  | n <= 0x7ff = do
      pokeByteOff p off     (fromIntegral (0xc0 + (n `shiftR` 6)) :: Word8)
      pokeByteOff p (off+1) (fromIntegral (0x80 + (n .&. 0x3f)) :: Word8)
      pure (off + 2)
  | n <= 0xffff = do
      pokeByteOff p off     (fromIntegral (0xe0 + (n `shiftR` 12)) :: Word8)
      pokeByteOff p (off+1) (fromIntegral (0x80 + ((n `shiftR` 6) .&. 0x3f)) :: Word8)
      pokeByteOff p (off+2) (fromIntegral (0x80 + (n .&. 0x3f)) :: Word8)
      pure (off + 3)
  | otherwise = do
      pokeByteOff p off     (fromIntegral (0xf0 + (n `shiftR` 18)) :: Word8)
      pokeByteOff p (off+1) (fromIntegral (0x80 + ((n `shiftR` 12) .&. 0x3f)) :: Word8)
      pokeByteOff p (off+2) (fromIntegral (0x80 + ((n `shiftR` 6) .&. 0x3f)) :: Word8)
      pokeByteOff p (off+3) (fromIntegral (0x80 + (n .&. 0x3f)) :: Word8)
      pure (off + 4)
  where
    n = ord c

-- Keep the structural feed, public dependency walker, implementation
-- dependency walker, and Names.freeQ aligned when adding AST constructors.
feedStmt :: A.Stmt -> HashFeed
feedStmt stmt sink = case stmt of
  A.Expr _ e             -> feedTag 10 sink >> feedExpr e sink
  A.Assign _ ps e        -> feedTag 11 sink >> feedList feedPattern ps sink >> feedExpr e sink
  A.MutAssign _ t e      -> feedTag 12 sink >> feedExpr t sink >> feedExpr e sink
  A.AugAssign _ t op e   -> feedTag 13 sink >> feedExpr t sink >> feedAug op sink >> feedExpr e sink
  A.Assert _ e mbe       -> feedTag 14 sink >> feedExpr e sink >> feedMaybe feedExpr mbe sink
  A.Pass _               -> feedTag 15 sink
  A.Delete _ t           -> feedTag 16 sink >> feedExpr t sink
  A.Return _ mbe         -> feedTag 17 sink >> feedMaybe feedExpr mbe sink
  A.Raise _ e            -> feedTag 18 sink >> feedExpr e sink
  A.Break _              -> feedTag 19 sink
  A.Continue _           -> feedTag 20 sink
  A.If _ bs els          -> feedTag 21 sink >> feedList feedBranch bs sink >> feedSuite els sink
  A.While _ e b els      -> feedTag 22 sink >> feedExpr e sink >> feedSuite b sink >> feedSuite els sink
  A.For _ p e b els      -> feedTag 23 sink >> feedPattern p sink >> feedExpr e sink >> feedSuite b sink >> feedSuite els sink
  A.Try _ b hs els fin   -> feedTag 24 sink >> feedSuite b sink >> feedList feedHandler hs sink >> feedSuite els sink >> feedSuite fin sink
  A.With _ ws b          -> feedTag 25 sink >> feedList feedWithItem ws sink >> feedSuite b sink
  A.Data _ mbp b         -> feedTag 26 sink >> feedMaybe feedPattern mbp sink >> feedSuite b sink
  A.VarAssign _ ps e     -> feedTag 27 sink >> feedList feedPattern ps sink >> feedExpr e sink
  A.After _ e e'         -> feedTag 28 sink >> feedExpr e sink >> feedExpr e' sink
  A.Signature _ ns t dec -> feedTag 29 sink >> feedList feedName ns sink >> feedTSchema t sink >> feedDeco dec sink
  A.Decl _ ds            -> feedTag 30 sink >> feedList feedDecl ds sink

feedSuite :: A.Suite -> HashFeed
feedSuite = feedList feedStmt

feedDecl :: A.Decl -> HashFeed
feedDecl decl sink = case decl of
  A.Def _ n q p k a b dec fx doc ->
    feedTag 40 sink >> feedName n sink >> feedQBinds q sink >> feedPosPar p sink >>
    feedKwdPar k sink >> feedMaybe feedType a sink >> feedSuite b sink >>
    feedDeco dec sink >> feedType fx sink >> feedMaybe feedString doc sink
  A.Actor _ n q p k b doc ->
    feedTag 41 sink >> feedName n sink >> feedQBinds q sink >> feedPosPar p sink >>
    feedKwdPar k sink >> feedSuite b sink >> feedMaybe feedString doc sink
  A.Class _ n q cs b doc ->
    feedTag 42 sink >> feedName n sink >> feedQBinds q sink >> feedList feedTCon cs sink >>
    feedSuite b sink >> feedMaybe feedString doc sink
  A.Protocol _ n q ps b doc ->
    feedTag 43 sink >> feedName n sink >> feedQBinds q sink >> feedList feedTCon ps sink >>
    feedSuite b sink >> feedMaybe feedString doc sink
  A.Extension _ q c ps b doc ->
    feedTag 44 sink >> feedQBinds q sink >> feedTCon c sink >> feedList feedTCon ps sink >>
    feedSuite b sink >> feedMaybe feedString doc sink

feedExpr :: A.Expr -> HashFeed
feedExpr expr sink = case expr of
  A.Var _ qn            -> feedTag 50 sink >> feedQName qn sink
  A.Int _ i s           -> feedTag 51 sink >> feedInteger i sink >> feedString s sink
  A.Float _ d s         -> feedTag 52 sink >> feedDouble d sink >> feedString s sink
  A.Imaginary _ d s     -> feedTag 53 sink >> feedDouble d sink >> feedString s sink
  A.Bool _ b            -> feedTag 54 sink >> feedBool b sink
  A.None _              -> feedTag 55 sink
  A.NotImplemented _    -> feedTag 56 sink
  A.Ellipsis _          -> feedTag 57 sink
  A.Strings _ ss        -> feedTag 58 sink >> feedList feedString ss sink
  A.BStrings _ ss       -> feedTag 59 sink >> feedList feedString ss sink
  A.Call _ f ps ks      -> feedTag 60 sink >> feedExpr f sink >> feedPosArg ps sink >> feedKwdArg ks sink
  A.Let _ ss e          -> feedTag 61 sink >> feedSuite ss sink >> feedExpr e sink
  A.TApp _ f ts         -> feedTag 62 sink >> feedExpr f sink >> feedList feedType ts sink
  A.Async _ e           -> feedTag 63 sink >> feedExpr e sink
  A.Await _ e           -> feedTag 64 sink >> feedExpr e sink
  A.Index _ e ix        -> feedTag 65 sink >> feedExpr e sink >> feedExpr ix sink
  A.Slice _ e sl        -> feedTag 66 sink >> feedExpr e sink >> feedSliz sl sink
  A.Cond _ e c e'       -> feedTag 67 sink >> feedExpr e sink >> feedExpr c sink >> feedExpr e' sink
  A.IsInstance _ e c    -> feedTag 68 sink >> feedExpr e sink >> feedQName c sink
  A.BinOp _ e op e'     -> feedTag 69 sink >> feedExpr e sink >> feedBinary op sink >> feedExpr e' sink
  A.CompOp _ e ops      -> feedTag 70 sink >> feedExpr e sink >> feedList feedOpArg ops sink
  A.UnOp _ op e         -> feedTag 71 sink >> feedUnary op sink >> feedExpr e sink
  A.Dot _ e n           -> feedTag 72 sink >> feedExpr e sink >> feedName n sink
  A.Rest _ e n          -> feedTag 73 sink >> feedExpr e sink >> feedName n sink
  A.DotI _ e i          -> feedTag 74 sink >> feedExpr e sink >> feedInteger i sink
  A.RestI _ e i         -> feedTag 75 sink >> feedExpr e sink >> feedInteger i sink
  A.Opt _ e b           -> feedTag 76 sink >> feedExpr e sink >> feedBool b sink
  A.OptChain _ e        -> feedTag 77 sink >> feedExpr e sink
  A.Lambda _ p k e fx   -> feedTag 78 sink >> feedPosPar p sink >> feedKwdPar k sink >> feedExpr e sink >> feedType fx sink
  A.Yield _ mbe         -> feedTag 79 sink >> feedMaybe feedExpr mbe sink
  A.YieldFrom _ e       -> feedTag 80 sink >> feedExpr e sink
  A.Tuple _ ps ks       -> feedTag 81 sink >> feedPosArg ps sink >> feedKwdArg ks sink
  A.List _ es           -> feedTag 82 sink >> feedList feedElem es sink
  A.ListComp _ e c      -> feedTag 83 sink >> feedElem e sink >> feedComp c sink
  A.Dict _ as           -> feedTag 84 sink >> feedList feedAssoc as sink
  A.DictComp _ a c      -> feedTag 85 sink >> feedAssoc a sink >> feedComp c sink
  A.Set _ es            -> feedTag 86 sink >> feedList feedElem es sink
  A.SetComp _ e c       -> feedTag 87 sink >> feedElem e sink >> feedComp c sink
  A.Paren _ e           -> feedTag 88 sink >> feedExpr e sink
  A.Box t e             -> feedTag 89 sink >> feedType t sink >> feedExpr e sink
  A.UnBox t e           -> feedTag 90 sink >> feedType t sink >> feedExpr e sink

feedPattern :: A.Pattern -> HashFeed
feedPattern pat sink = case pat of
  A.PWild _ mt       -> feedTag 100 sink >> feedMaybe feedType mt sink
  A.PVar _ n mt      -> feedTag 101 sink >> feedName n sink >> feedMaybe feedType mt sink
  A.PParen _ p       -> feedTag 102 sink >> feedPattern p sink
  A.PTuple _ ps ks   -> feedTag 103 sink >> feedPosPat ps sink >> feedKwdPat ks sink
  A.PList _ ps tailp -> feedTag 104 sink >> feedList feedPattern ps sink >> feedMaybe feedPattern tailp sink
  A.PData _ n ixs    -> feedTag 105 sink >> feedName n sink >> feedList feedExpr ixs sink

feedType :: A.Type -> HashFeed
feedType t sink = case t of
  A.TUni _ u          -> feedTag 110 sink >> feedTUni u sink
  A.TVar _ tv         -> feedTag 111 sink >> feedTVar tv sink
  A.TCon _ tc         -> feedTag 112 sink >> feedTCon tc sink
  A.TFun _ fx p k r   -> feedTag 113 sink >> feedType fx sink >> feedType p sink >> feedType k sink >> feedType r sink
  A.TTuple _ p k      -> feedTag 114 sink >> feedType p sink >> feedType k sink
  A.TOpt _ opt        -> feedTag 115 sink >> feedType opt sink
  A.TNone _           -> feedTag 116 sink
  A.TWild _           -> feedTag 117 sink
  A.TNil _ k          -> feedTag 118 sink >> feedKind k sink
  A.TRow _ k n ty row -> feedTag 119 sink >> feedKind k sink >> feedName n sink >> feedType ty sink >> feedType row sink
  A.TStar _ k row     -> feedTag 120 sink >> feedKind k sink >> feedType row sink
  A.TFX _ fx          -> feedTag 121 sink >> feedFX fx sink
  A.TUnboxed _ ty     -> feedTag 122 sink >> feedType ty sink

feedTSchema :: A.TSchema -> HashFeed
feedTSchema (A.TSchema _ q t) sink = feedTag 130 sink >> feedQBinds q sink >> feedType t sink

feedTCon :: A.TCon -> HashFeed
feedTCon (A.TC qn ts) sink = feedTag 131 sink >> feedQName qn sink >> feedList feedType ts sink

feedQBind :: A.QBind -> HashFeed
feedQBind (A.QBind tv cs) sink = feedTag 132 sink >> feedTVar tv sink >> feedList feedTCon cs sink

feedQBinds :: A.QBinds -> HashFeed
feedQBinds = feedList feedQBind

feedTVar :: A.TVar -> HashFeed
feedTVar (A.TV k n) sink = feedTag 133 sink >> feedKind k sink >> feedName n sink

feedTUni :: A.TUni -> HashFeed
feedTUni (A.UV k l i) sink = feedTag 134 sink >> feedKind k sink >> feedInt l sink >> feedInt i sink

feedKind :: A.Kind -> HashFeed
feedKind k sink = case k of
  A.KType     -> feedTag 140 sink
  A.KProto    -> feedTag 141 sink
  A.KFX       -> feedTag 142 sink
  A.PRow      -> feedTag 143 sink
  A.KRow      -> feedTag 144 sink
  A.KFun ks r -> feedTag 145 sink >> feedList feedKind ks sink >> feedKind r sink
  A.KUni i    -> feedTag 146 sink >> feedInt i sink
  A.KWild     -> feedTag 147 sink

feedPosPar :: A.PosPar -> HashFeed
feedPosPar p sink = case p of
  A.PosPar n mt me rest -> feedTag 150 sink >> feedName n sink >> feedMaybe feedType mt sink >> feedMaybe feedExpr me sink >> feedPosPar rest sink
  A.PosSTAR n mt        -> feedTag 151 sink >> feedName n sink >> feedMaybe feedType mt sink
  A.PosNIL              -> feedTag 152 sink

feedKwdPar :: A.KwdPar -> HashFeed
feedKwdPar k sink = case k of
  A.KwdPar n mt me rest -> feedTag 153 sink >> feedName n sink >> feedMaybe feedType mt sink >> feedMaybe feedExpr me sink >> feedKwdPar rest sink
  A.KwdSTAR n mt        -> feedTag 154 sink >> feedName n sink >> feedMaybe feedType mt sink
  A.KwdNIL              -> feedTag 155 sink

feedPosArg :: A.PosArg -> HashFeed
feedPosArg ps sink = case ps of
  A.PosArg e rest -> feedTag 156 sink >> feedExpr e sink >> feedPosArg rest sink
  A.PosStar e     -> feedTag 157 sink >> feedExpr e sink
  A.PosNil        -> feedTag 158 sink

feedKwdArg :: A.KwdArg -> HashFeed
feedKwdArg ks sink = case ks of
  A.KwdArg n e rest -> feedTag 159 sink >> feedName n sink >> feedExpr e sink >> feedKwdArg rest sink
  A.KwdStar e       -> feedTag 160 sink >> feedExpr e sink
  A.KwdNil          -> feedTag 161 sink

feedPosPat :: A.PosPat -> HashFeed
feedPosPat ps sink = case ps of
  A.PosPat p rest -> feedTag 162 sink >> feedPattern p sink >> feedPosPat rest sink
  A.PosPatStar p  -> feedTag 163 sink >> feedPattern p sink
  A.PosPatNil     -> feedTag 164 sink

feedKwdPat :: A.KwdPat -> HashFeed
feedKwdPat ks sink = case ks of
  A.KwdPat n p rest -> feedTag 165 sink >> feedName n sink >> feedPattern p sink >> feedKwdPat rest sink
  A.KwdPatStar p    -> feedTag 166 sink >> feedPattern p sink
  A.KwdPatNil       -> feedTag 167 sink

feedBranch :: A.Branch -> HashFeed
feedBranch (A.Branch e ss) sink = feedTag 168 sink >> feedExpr e sink >> feedSuite ss sink

feedHandler :: A.Handler -> HashFeed
feedHandler (A.Handler ex ss) sink = feedTag 169 sink >> feedExcept ex sink >> feedSuite ss sink

feedExcept :: A.Except -> HashFeed
feedExcept ex sink = case ex of
  A.ExceptAll _    -> feedTag 170 sink
  A.Except _ qn    -> feedTag 171 sink >> feedQName qn sink
  A.ExceptAs _ q n -> feedTag 172 sink >> feedQName q sink >> feedName n sink

feedElem :: A.Elem -> HashFeed
feedElem elem1 sink = case elem1 of
  A.Elem e -> feedTag 173 sink >> feedExpr e sink
  A.Star e -> feedTag 174 sink >> feedExpr e sink

feedAssoc :: A.Assoc -> HashFeed
feedAssoc assoc sink = case assoc of
  A.Assoc k v  -> feedTag 175 sink >> feedExpr k sink >> feedExpr v sink
  A.StarStar e -> feedTag 176 sink >> feedExpr e sink

feedOpArg :: A.OpArg -> HashFeed
feedOpArg (A.OpArg op e) sink = feedTag 177 sink >> feedComparison op sink >> feedExpr e sink

feedSliz :: A.Sliz -> HashFeed
feedSliz (A.Sliz _ a b c) sink = feedTag 178 sink >> feedMaybe feedExpr a sink >> feedMaybe feedExpr b sink >> feedMaybe feedExpr c sink

feedComp :: A.Comp -> HashFeed
feedComp comp sink = case comp of
  A.CompFor _ p e c -> feedTag 179 sink >> feedPattern p sink >> feedExpr e sink >> feedComp c sink
  A.CompIf _ e c    -> feedTag 180 sink >> feedExpr e sink >> feedComp c sink
  A.NoComp          -> feedTag 181 sink

feedWithItem :: A.WithItem -> HashFeed
feedWithItem (A.WithItem e p) sink = feedTag 182 sink >> feedExpr e sink >> feedMaybe feedPattern p sink

feedQName :: A.QName -> HashFeed
feedQName qn sink = case qn of
  A.QName mn n -> feedTag 183 sink >> feedModName mn sink >> feedName n sink
  A.NoQ n      -> feedTag 184 sink >> feedName n sink
  A.GName mn n -> feedTag 185 sink >> feedModName mn sink >> feedName n sink

feedModName :: A.ModName -> HashFeed
feedModName (A.ModName ns) sink = feedTag 186 sink >> feedList feedName ns sink

feedName :: A.Name -> HashFeed
feedName n sink = case n of
  A.Name _ s       -> feedTag 187 sink >> feedString s sink
  A.Derived n1 n2  -> feedTag 188 sink >> feedName n1 sink >> feedName n2 sink
  A.Internal p s i -> feedTag 189 sink >> feedPrefix p sink >> feedString s sink >> feedInt i sink

feedPrefix :: A.Prefix -> HashFeed
feedPrefix p sink = feedTag (case p of
  A.Globvar  -> 190
  A.Xistvar  -> 191
  A.Tempvar  -> 192
  A.Witness  -> 193
  A.NormPass -> 194
  A.CPSPass  -> 195
  A.LLiftPass -> 196
  A.BoxPass  -> 197) sink

feedUnary :: A.Unary -> HashFeed
feedUnary op sink = feedTag (case op of
  A.Not    -> 198
  A.UPlus  -> 199
  A.UMinus -> 200
  A.BNot   -> 201) sink

feedBinary :: A.Binary -> HashFeed
feedBinary op sink = feedTag (case op of
  A.Or     -> 202
  A.And    -> 203
  A.Plus   -> 204
  A.Minus  -> 205
  A.Mult   -> 206
  A.Pow    -> 207
  A.Div    -> 208
  A.Mod    -> 209
  A.EuDiv  -> 210
  A.BOr    -> 211
  A.BXor   -> 212
  A.BAnd   -> 213
  A.ShiftL -> 214
  A.ShiftR -> 215
  A.MMult  -> 216) sink

feedAug :: A.Aug -> HashFeed
feedAug op sink = feedTag (case op of
  A.PlusA   -> 217
  A.MinusA  -> 218
  A.MultA   -> 219
  A.PowA    -> 220
  A.DivA    -> 221
  A.ModA    -> 222
  A.EuDivA  -> 223
  A.BOrA    -> 224
  A.BXorA   -> 225
  A.BAndA   -> 226
  A.ShiftLA -> 227
  A.ShiftRA -> 228
  A.MMultA  -> 229) sink

feedComparison :: A.Comparison -> HashFeed
feedComparison op sink = feedTag (case op of
  A.Eq    -> 230
  A.NEq   -> 231
  A.LtGt  -> 232
  A.Lt    -> 233
  A.Gt    -> 234
  A.GE    -> 235
  A.LE    -> 236
  A.In    -> 237
  A.NotIn -> 238
  A.Is    -> 239
  A.IsNot -> 240) sink

feedDeco :: A.Deco -> HashFeed
feedDeco dec sink = feedTag (case dec of
  A.NoDec    -> 241
  A.Property -> 242
  A.Static   -> 243) sink

feedFX :: A.FX -> HashFeed
feedFX fx sink = feedTag (case fx of
  A.FXPure   -> 244
  A.FXMut    -> 245
  A.FXProc   -> 246
  A.FXAction -> 247) sink

feedNameInfo :: I.NameInfo -> HashFeed
feedNameInfo info sink = feedTag 249 sink >> case info of
  I.NVar t           -> feedTag 0 sink >> feedType t sink
  I.NSVar t          -> feedTag 1 sink >> feedType t sink
  I.NDef sc dec _    -> feedTag 2 sink >> feedTSchema sc sink >> feedDeco dec sink
  I.NSig sc dec _    -> feedTag 3 sink >> feedTSchema sc sink >> feedDeco dec sink
  I.NAct q p k te _  -> feedTag 4 sink >> feedQBinds q sink >> feedType p sink >> feedType k sink >> feedTEnv te sink
  I.NClass q cs te _ -> feedTag 5 sink >> feedQBinds q sink >> feedList feedWTCon cs sink >> feedTEnv te sink
  I.NProto q ps te _ -> feedTag 6 sink >> feedQBinds q sink >> feedList feedWTCon ps sink >> feedTEnv te sink
  I.NExt q c ps te o _ ->
    feedTag 7 sink >> feedQBinds q sink >> feedTCon c sink >> feedList feedWTCon ps sink >>
    feedTEnv te sink >> feedList feedName o sink
  I.NTVar k c ps     -> feedTag 8 sink >> feedKind k sink >> feedTCon c sink >> feedList feedTCon ps sink
  I.NAlias qn        -> feedTag 9 sink >> feedQName qn sink
  I.NReserved        -> feedTag 12 sink

feedTEnv :: I.TEnv -> HashFeed
feedTEnv = feedList feedTEnvBind

feedTEnvBind :: (A.Name, I.NameInfo) -> HashFeed
feedTEnvBind (n, info) sink = feedTag 250 sink >> feedName n sink >> feedNameInfo info sink

feedWTCon :: I.WTCon -> HashFeed
feedWTCon (wpath, pcon) sink = feedTag 251 sink >> feedList feedWStep wpath sink >> feedTCon pcon sink

feedWStep :: Either A.QName A.QName -> HashFeed
feedWStep step sink = case step of
  Left qn  -> feedTag 252 sink >> feedQName qn sink
  Right qn -> feedTag 253 sink >> feedQName qn sink

feedHashBytes :: B.ByteString -> HashFeed
feedHashBytes bs sink = feedInt (B.length bs) sink >> sinkWriteBytes sink bs

feedResolvedDepHashes :: (A.Name -> Maybe B.ByteString)
                      -> [A.Name]
                      -> [(A.QName, B.ByteString)]
                      -> HashFeed
-- Local and external deps are sorted before this point.
feedResolvedDepHashes lookupLocal locals externals sink = do
  feedTag 4 sink
  mapM_ feedLocal locals
  mapM_ feedExternal externals
  feedTag 5 sink
  where
    feedLocal n =
      case lookupLocal n of
        Just h  -> feedHashBytes h sink
        Nothing -> pure ()
    feedExternal (_, h) = feedHashBytes h sink

hashSelfWithDepsFromWith :: HashSink
                         -> B.ByteString
                         -> (A.Name -> Maybe B.ByteString)
                         -> [A.Name]
                         -> [(A.QName, B.ByteString)]
                         -> IO B.ByteString
hashSelfWithDepsFromWith sink self lookupLocal locals externals =
  hashFeedWith sink $ \sink' ->
    feedTag 254 sink' >> feedTag 0 sink' >> feedHashBytes self sink' >>
    feedResolvedDepHashes lookupLocal locals externals sink'

hashCycleGroupFromWith :: HashSink
                       -> [B.ByteString]
                       -> (A.Name -> Maybe B.ByteString)
                       -> [A.Name]
                       -> [(A.QName, B.ByteString)]
                       -> IO B.ByteString
hashCycleGroupFromWith sink selfHashes lookupLocal locals externals =
  hashFeedWith sink $ \sink' ->
    feedTag 254 sink' >> feedTag 1 sink' >> feedList feedHashBytes selfHashes sink' >>
    feedResolvedDepHashes lookupLocal locals externals sink'

hashCycleMemberWith :: HashSink -> B.ByteString -> B.ByteString -> A.Name -> IO B.ByteString
hashCycleMemberWith sink self groupHash n =
  hashFeedWith sink $ \sink' ->
    feedTag 254 sink' >> feedTag 2 sink' >> feedHashBytes self sink' >> feedHashBytes groupHash sink' >> feedString (nameKey n) sink'

hashNameHashEntries :: Int -> [(String, B.ByteString)] -> B.ByteString
hashNameHashEntries tag entries =
  hashNameHashEntriesWith tag entries $ \(n, h) sink ->
    feedString n sink >> feedHashBytes h sink

hashNameHashEntriesWith :: Int -> [a] -> (a -> HashFeed) -> B.ByteString
hashNameHashEntriesWith tag entries feedEntry =
  hashFeed $ \sink -> do
    feedTag 254 sink
    feedTag tag sink
    feedTag 4 sink
    mapM_ (\entry -> feedEntry entry sink) entries
    feedTag 5 sink
{-# INLINE hashNameHashEntriesWith #-}

hashNameHashMapEntriesForNames :: Int -> M.Map A.Name B.ByteString -> [A.Name] -> B.ByteString
hashNameHashMapEntriesForNames tag hashes names =
  hashNameHashEntriesWith tag names $ \n sink ->
    feedString (nameKey n) sink >> feedHashBytes (M.findWithDefault B.empty n hashes) sink

pubSigDepsFromNameInfoMap :: M.Map A.Name I.NameInfo -> M.Map A.Name [A.QName]
pubSigDepsFromNameInfoMap nameInfoMap =
  M.map (Data.Set.toList . Data.Set.fromList . filter (not . isDerivedQName) . Names.freeQ) nameInfoMap

pubSigSplitDepsFromNameInfoMap :: A.ModName
                               -> Env.Env0
                               -> Data.Set.Set A.Name
                               -> M.Map A.Name I.NameInfo
                               -> (M.Map A.Name [A.Name], M.Map A.Name [A.QName])
pubSigSplitDepsFromNameInfoMap mn env localNames =
  finishHashSplitDeps . M.map (pubSigSplitDepsFromNameInfo mn env localNames)

pubSigSplitDepsFromNameInfoMapWithProgress :: (Int -> IO ())
                                           -> A.ModName
                                           -> Env.Env0
                                           -> Data.Set.Set A.Name
                                           -> M.Map A.Name I.NameInfo
                                           -> IO (M.Map A.Name [A.Name], M.Map A.Name [A.QName])
pubSigSplitDepsFromNameInfoMapWithProgress onProgress mn env localNames infos = do
  (deps, _) <- foldM addDeps (M.empty, 0) (M.toAscList infos)
  pure (finishHashSplitDeps deps)
  where
    addDeps (acc, done) (n, info) = do
      let deps = pubSigSplitDepsFromNameInfo mn env localNames info
          acc' = M.insert n deps acc
          done' = done + 1
      depSplitSize deps `seq` onProgress done'
      pure (acc', done')

pubSigSplitDepsFromNameInfo :: A.ModName
                            -> Env.Env0
                            -> Data.Set.Set A.Name
                            -> I.NameInfo
                            -> DepSplit
pubSigSplitDepsFromNameInfo mn env localNames info =
  foldNameInfoDeps addPubSigDep info emptyDepSplitDirect
  where
    addPubSigDep qn deps
      | isDerivedQName qn = deps
      | otherwise         = addSplitDepHash mn env localNames deps qn

isDerivedName :: A.Name -> Bool
isDerivedName n = case n of
  A.Derived{} -> True
  _           -> False

isDerivedQName :: A.QName -> Bool
isDerivedQName qn = case qn of
  A.GName _ n -> isDerivedName n
  A.QName _ n -> isDerivedName n
  A.NoQ n     -> isDerivedName n

-- | Public hashes include signature deps plus implementation deps, but derived
-- implementation names are internal and should not require public hashes.
mergePubDeps :: M.Map A.Name [A.Name]
             -> M.Map A.Name [A.QName]
             -> M.Map A.Name [A.Name]
             -> M.Map A.Name [A.QName]
             -> (M.Map A.Name [A.Name], M.Map A.Name [A.QName])
mergePubDeps pubSigLocalDeps pubSigExtDeps implLocalDeps implExtDeps =
  ( M.unionWith mergeSortedDistinct pubSigLocalDeps implPubLocalDeps
  , M.unionWith mergeSortedDistinct pubSigExtDeps implPubExtDeps
  )
  where
    implPubLocalDeps = M.map (filter (not . isDerivedName)) implLocalDeps
    implPubExtDeps = M.map (filter (not . isDerivedQName)) implExtDeps

foldNameInfoDeps :: (A.QName -> acc -> acc) -> I.NameInfo -> acc -> acc
foldNameInfoDeps add info acc = case info of
  I.NVar t             -> foldDepsType add t acc
  I.NSVar t            -> foldDepsType add t acc
  I.NDef sc _ _        -> foldDepsTSchema add sc acc
  I.NSig sc _ _        -> foldDepsTSchema add sc acc
  I.NAct q p k te _    -> foldDepsTEnv add te (foldDepsType add k (foldDepsType add p (foldDepsList (foldDepsQBind add) q acc)))
  I.NClass q ws te _   -> foldDepsTEnv add te (foldDepsList (foldDepsWTCon add) ws (foldDepsList (foldDepsQBind add) q acc))
  I.NProto q ws te _   -> foldDepsTEnv add te (foldDepsList (foldDepsWTCon add) ws (foldDepsList (foldDepsQBind add) q acc))
  I.NExt q c ws te _ _ -> foldDepsTEnv add te (foldDepsList (foldDepsWTCon add) ws (foldDepsTCon add c (foldDepsList (foldDepsQBind add) q acc)))
  I.NTVar _ c ps       -> foldDepsList (foldDepsTCon add) ps (foldDepsTCon add c acc)
  I.NAlias qn          -> add qn acc
  I.NReserved          -> acc

foldDepsTEnv :: (A.QName -> acc -> acc) -> I.TEnv -> acc -> acc
foldDepsTEnv add te acc =
  foldl' (\acc' (_, info) -> foldNameInfoDeps add info acc') acc te

foldDepsWTCon :: (A.QName -> acc -> acc) -> I.WTCon -> acc -> acc
foldDepsWTCon add (wpath, pcon) acc =
  foldDepsTCon add pcon (foldDepsWPath add wpath acc)

foldDepsWPath :: (A.QName -> acc -> acc) -> I.WPath -> acc -> acc
foldDepsWPath add wpath acc =
  foldl' (\acc' step -> add (stepQName step) acc') acc wpath
  where
    stepQName step = case step of
      Left qn  -> qn
      Right qn -> qn

-- | Collect qualified-name dependencies for each item body.
implDepsFromItems :: [TopLevelItem] -> M.Map A.Name [A.QName]
implDepsFromItems items =
  foldl' addDeps M.empty items
  where
    addDeps acc item =
      let (n, deps) = case item of
            TLDecl name decl -> (name, Names.freeQ decl)
            TLStmt name stmt -> (name, Names.freeQ stmt)
      in M.insertWith (++) n deps acc

type QNameSet = Data.Set.Set A.QName
type NameSet  = Data.Set.Set A.Name

boundMaybe :: (a -> NameSet -> NameSet) -> Maybe a -> NameSet -> NameSet
boundMaybe _ Nothing  acc = acc
boundMaybe f (Just x) acc = f x acc

boundList :: (a -> NameSet -> NameSet) -> [a] -> NameSet -> NameSet
boundList f xs acc = foldl' (\acc' x -> f x acc') acc xs

foldDepsTSchema :: (A.QName -> acc -> acc) -> A.TSchema -> acc -> acc
foldDepsTSchema add (A.TSchema _ q t) acc =
  foldDepsType add t (foldDepsList (foldDepsQBind add) q acc)

foldDepsQBind :: (A.QName -> acc -> acc) -> A.QBind -> acc -> acc
foldDepsQBind add (A.QBind _ cs) acc =
  foldDepsList (foldDepsTCon add) cs acc

foldDepsTCon :: (A.QName -> acc -> acc) -> A.TCon -> acc -> acc
foldDepsTCon add (A.TC qn ts) acc =
  foldDepsList (foldDepsType add) ts (add qn acc)

foldDepsType :: (A.QName -> acc -> acc) -> A.Type -> acc -> acc
foldDepsType add t acc = case t of
  A.TVar _ tv         -> foldDepsTVar add tv acc
  A.TUni _ u          -> foldDepsTUni add u acc
  A.TCon _ tc         -> foldDepsTCon add tc acc
  A.TFun _ fx p k r   -> foldDepsType add r (foldDepsType add k (foldDepsType add p (foldDepsType add fx acc)))
  A.TTuple _ p k      -> foldDepsType add k (foldDepsType add p acc)
  A.TOpt _ opt        -> foldDepsType add opt acc
  A.TNone{}           -> acc
  A.TWild{}           -> acc
  A.TNil{}            -> acc
  A.TRow _ _ _ ty row -> foldDepsType add row (foldDepsType add ty acc)
  A.TStar _ _ row     -> foldDepsType add row acc
  A.TFX{}             -> acc
  A.TUnboxed _ ty     -> foldDepsType add ty acc

foldDepsTVar :: (A.QName -> acc -> acc) -> A.TVar -> acc -> acc
foldDepsTVar _ _ acc = acc

foldDepsTUni :: (A.QName -> acc -> acc) -> A.TUni -> acc -> acc
foldDepsTUni _ _ acc = acc

foldDepsList :: (a -> acc -> acc) -> [a] -> acc -> acc
foldDepsList f xs acc = foldl' (\acc' x -> f x acc') acc xs

boundStmt :: A.Stmt -> NameSet -> NameSet
boundStmt stmt acc = case stmt of
  A.Assign _ ps _      -> boundList boundPattern ps acc
  A.VarAssign _ ps _   -> boundList boundPattern ps acc
  A.AugAssign _ t _ _  -> depsNameSetInto (Names.free t) acc
  A.Decl _ ds          -> boundList boundDecl ds acc
  A.Signature _ ns _ _ -> depsNameSetInto ns acc
  A.If _ bs els        -> boundSuite els (boundList boundBranch bs acc)
  A.While _ _ b els    -> boundSuite els (boundSuite b acc)
  A.With _ _ b         -> boundSuite b acc
  _                    -> acc

boundSuite :: A.Suite -> NameSet -> NameSet
boundSuite = boundList boundStmt

boundDecl :: A.Decl -> NameSet -> NameSet
boundDecl decl acc = case decl of
  A.Def _ n _ _ _ _ _ _ _ _ -> Data.Set.insert n acc
  A.Actor _ n _ _ _ _ _     -> Data.Set.insert n acc
  A.Class _ n _ _ _ _       -> Data.Set.insert n acc
  A.Protocol _ n _ _ _ _    -> Data.Set.insert n acc
  A.Extension{}             -> acc

boundBranch :: A.Branch -> NameSet -> NameSet
boundBranch (A.Branch _ ss) = boundSuite ss

boundHandler :: A.Handler -> NameSet -> NameSet
boundHandler (A.Handler ex ss) acc = boundExcept ex (boundSuite ss acc)

boundExcept :: A.Except -> NameSet -> NameSet
boundExcept ex acc = case ex of
  A.ExceptAs _ _ n -> Data.Set.insert n acc
  _                -> acc

boundPattern :: A.Pattern -> NameSet -> NameSet
boundPattern pat acc = case pat of
  A.PVar _ n _      -> Data.Set.insert n acc
  A.PTuple _ ps ks  -> boundKwdPat ks (boundPosPat ps acc)
  A.PList _ ps p    -> boundMaybe boundPattern p (boundList boundPattern ps acc)
  A.PParen _ p      -> boundPattern p acc
  A.PData _ n _     -> Data.Set.insert n acc
  _                 -> acc

boundPosPar :: A.PosPar -> NameSet -> NameSet
boundPosPar p acc = case p of
  A.PosPar n _ _ rest -> boundPosPar rest (Data.Set.insert n acc)
  A.PosSTAR n _       -> Data.Set.insert n acc
  A.PosNIL            -> acc

boundKwdPar :: A.KwdPar -> NameSet -> NameSet
boundKwdPar k acc = case k of
  A.KwdPar n _ _ rest -> boundKwdPar rest (Data.Set.insert n acc)
  A.KwdSTAR n _       -> Data.Set.insert n acc
  A.KwdNIL            -> acc

boundPosPat :: A.PosPat -> NameSet -> NameSet
boundPosPat ps acc = case ps of
  A.PosPat p rest -> boundPosPat rest (boundPattern p acc)
  A.PosPatStar p  -> boundPattern p acc
  A.PosPatNil     -> acc

boundKwdPat :: A.KwdPat -> NameSet -> NameSet
boundKwdPat ks acc = case ks of
  A.KwdPat _ p rest -> boundKwdPat rest (boundPattern p acc)
  A.KwdPatStar p    -> boundPattern p acc
  A.KwdPatNil       -> acc

boundWithItem :: A.WithItem -> NameSet -> NameSet
boundWithItem (A.WithItem _ p) = boundMaybe boundPattern p

boundComp :: A.Comp -> NameSet -> NameSet
boundComp comp acc = case comp of
  A.CompFor _ pat _ c -> boundComp c (boundPattern pat acc)
  A.CompIf _ _ c      -> boundComp c acc
  A.NoComp            -> acc

boundQBind :: A.QBind -> NameSet -> NameSet
boundQBind _ acc = acc

assignedSuite :: A.Suite -> NameSet -> NameSet
assignedSuite = boundList assignedStmt

assignedStmt :: A.Stmt -> NameSet -> NameSet
assignedStmt stmt acc = case stmt of
  A.While _ _ b els   -> assignedSuite els (assignedSuite b acc)
  A.For _ p _ b els   -> boundPattern p (assignedSuite els (assignedSuite b acc))
  A.With _ ws b       -> boundList boundWithItem ws (assignedSuite b acc)
  A.Try _ b hs els fin ->
    assignedSuite fin (assignedSuite els (boundList assignedHandler hs (assignedSuite b acc)))
  A.If _ bs els       -> assignedSuite els (boundList assignedBranch bs acc)
  A.Assign _ ps _     -> boundList boundPattern ps acc
  A.VarAssign _ ps _  -> boundList boundPattern ps acc
  _                   -> boundStmt stmt acc

assignedBranch :: A.Branch -> NameSet -> NameSet
assignedBranch (A.Branch _ b) = assignedSuite b

assignedHandler :: A.Handler -> NameSet -> NameSet
assignedHandler (A.Handler ex b) acc = assignedSuite b (boundExcept ex acc)

depsNameSet :: [A.Name] -> NameSet
depsNameSet ns = depsNameSetInto ns Data.Set.empty

depsNameSetInto :: [A.Name] -> NameSet -> NameSet
depsNameSetInto ns acc = foldl' (\acc' n -> Data.Set.insert n acc') acc ns

-- (local-name deps, external-qname deps, called method names).
-- A split walk accumulates local references bucketed BY POSITION so that a
-- single descent yields both views downstream needs:
--   1. codeLocals : locals from code positions (calls/constructions) -> the
--                   deferred-back "base" deps (type-only refs must not cascade)
--   2. typeLocals : locals from type positions (annotations/returns/used types)
--   3. externals  : external qualified-name deps (from either position)
--   4. called     : attribute names from Dot nodes (method calls / accesses),
--                   so DBP can follow per-method deps only for called methods
-- The impl/hash view is codeLocals ∪ typeLocals (what the old includeTypes=True
-- walk produced); the code-only view is codeLocals (old includeTypes=False).
type DepSplit = (HashSet.HashSet A.Name, HashSet.HashSet A.Name, HashSet.HashSet A.QName, HashSet.HashSet A.Name)

emptyDepSplitDirect :: DepSplit
emptyDepSplitDirect = (HashSet.empty, HashSet.empty, HashSet.empty, HashSet.empty)

depSplitSize :: DepSplit -> Int
depSplitSize (cl, tl, es, _) = HashSet.size cl + HashSet.size tl + HashSet.size es

unionDepSplit :: DepSplit -> DepSplit -> DepSplit
unionDepSplit (cl, tl, es, cs) (cl', tl', es', cs') =
  (HashSet.union cl cl', HashSet.union tl tl', HashSet.union es es', HashSet.union cs cs')

addCalledMethod :: A.Name -> DepSplit -> DepSplit
addCalledMethod n (cl, tl, es, cs) = (cl, tl, es, HashSet.insert n cs)

-- Code-position reference: a local dep lands in codeLocals.
insertQNameDep :: A.ModName -> Env.Env0 -> Data.Set.Set A.Name -> NameSet -> A.QName -> DepSplit -> DepSplit
insertQNameDep mn env localNames bound qn@(A.NoQ n) acc
  | Data.Set.member n bound = acc
  | otherwise = addSplitDepHash mn env localNames acc qn
insertQNameDep mn env localNames _ qn acc =
  addSplitDepHash mn env localNames acc qn

-- Type-position reference: a local dep lands in typeLocals instead, so the
-- code-only view can exclude it (keeping ?Child container fields from cascading).
insertQNameDepType :: A.ModName -> Env.Env0 -> Data.Set.Set A.Name -> NameSet -> A.QName -> DepSplit -> DepSplit
insertQNameDepType mn env localNames bound qn@(A.NoQ n) acc
  | Data.Set.member n bound = acc
  | otherwise = addSplitDepHashType mn env localNames acc qn
insertQNameDepType mn env localNames _ qn acc =
  addSplitDepHashType mn env localNames acc qn

splitMaybeInto :: (a -> DepSplit -> DepSplit) -> Maybe a -> DepSplit -> DepSplit
splitMaybeInto _ Nothing  acc = acc
splitMaybeInto f (Just x) acc = f x acc

splitListInto :: (a -> DepSplit -> DepSplit) -> [a] -> DepSplit -> DepSplit
splitListInto f xs acc = foldl' (\acc' x -> f x acc') acc xs

implSplitDepsFromItems :: A.ModName
                       -> Env.Env0
                       -> Data.Set.Set A.Name
                       -> [TopLevelItem]
                       -> (M.Map A.Name [A.Name], M.Map A.Name [A.QName])
implSplitDepsFromItems mn env localNames items =
  finishHashSplitDeps (foldl' addDeps M.empty (map (implItemSplitDeps mn env localNames) items))
  where
    addDeps acc (n, deps) = M.insertWith unionDepSplit n deps acc

-- | Local deps from code positions only (calls/constructions), excluding type
-- annotations. Same walk as the impl deps but reading only the codeLocals bucket.
codeLocalDepsFromItems :: A.ModName -> Env.Env0 -> Data.Set.Set A.Name -> [TopLevelItem] -> M.Map A.Name [A.Name]
codeLocalDepsFromItems mn env localNames items =
  finishCodeSplitDeps (foldl' addDeps M.empty (map (implItemSplitDeps mn env localNames) items))
  where
    addDeps acc (n, deps) = M.insertWith unionDepSplit n deps acc

-- Class body, method defs, and the method-stripped item: shared by the front
-- dep walk and its progress variant.
classBodyOf :: A.Decl -> Maybe A.Suite
classBodyOf (A.Class _ _ _ _ body _) = Just body
classBodyOf _                        = Nothing

classMethodDefs :: A.Decl -> [A.Decl]
classMethodDefs decl = case classBodyOf decl of
  Just body -> [ d | A.Decl _ ds <- body, d@A.Def{} <- ds ]
  Nothing   -> []

-- The local names the whole-class walk binds for the class body: the class name
-- plus everything assigned in the body (fields, method names, nested decls).
-- Walking class fields and methods separately loses these bindings, so a method
-- referencing its own class/sibling/field by bare name (e.g. a self-constructing
-- factory or clone) would record a spurious local dep. Subtracting this set from
-- each separately-walked split restores a single-whole-class-walk result. (qbind
-- type vars never appear in local deps, so they are omitted.)
classBoundFor :: A.Decl -> Data.Set.Set A.Name
classBoundFor decl = case classBodyOf decl of
  Just body -> Data.Set.insert (A.dname decl) (assignedSuite body Data.Set.empty)
  Nothing   -> Data.Set.empty

isExtDecl, isClassDecl :: TopLevelItem -> Bool
isExtDecl (TLDecl _ (A.Extension{})) = True
isExtDecl _                          = False
isClassDecl (TLDecl _ d)             = case classBodyOf d of { Just _ -> True; _ -> False }
isClassDecl _                        = False

stripClassMethods :: TopLevelItem -> TopLevelItem
stripClassMethods (TLDecl n (A.Class l cn q cs body kd)) =
    TLDecl n (A.Class l cn q cs (mapMaybe dropDefs body) kd)
  where dropDefs (A.Decl l' ds) = case filter (not . isDef) ds of
                                    []  -> Nothing
                                    ds' -> Just (A.Decl l' ds')
        dropDefs s              = Just s
        isDef A.Def{}           = True
        isDef _                 = False
stripClassMethods item = item

-- | The five dep views the front pass needs, assembled from per-group split maps
-- produced by a SINGLE position-tagged descent (each method body walked once):
--
--   * impl local / impl ext : full (code ∪ type) deps per top-level name, used
--                             for the impl hash — identical to implSplitDepsFromItems
--   * base map  : code-only deps with class methods removed (extensions keep
--                 type deps too, since the pruner emits them verbatim); always
--                 followed by the deferred-back closure
--   * method map: className -> [(methodName, that method's full deps)], followed
--                 only when the method is called somewhere in the program
--   * called map: name -> method names (Dot attributes) used in its body
frontSplitDepsFromItems :: A.ModName -> Env.Env0 -> Data.Set.Set A.Name -> [TopLevelItem]
                        -> ( M.Map A.Name [A.Name]
                           , M.Map A.Name [A.QName]
                           , M.Map A.Name [A.Name]
                           , M.Map A.Name [(A.Name, [A.Name])]
                           , M.Map A.Name [(A.Name, [A.Name])]
                           , M.Map A.Name [A.Name] )
frontSplitDepsFromItems mn env localNames items =
    assembleFrontDeps fieldsSplit classWalks plainSplit extSplit
  where
    (extItems, nonExtItems)  = Data.List.partition isExtDecl items
    (classItems, plainItems) = Data.List.partition isClassDecl nonExtItems
    walk1 its = foldl' (\acc item -> let (n, deps) = implItemSplitDeps mn env localNames item
                                     in M.insertWith unionDepSplit n deps acc) M.empty its
    plainSplit  = walk1 plainItems
    extSplit    = walk1 extItems
    -- Walk class fields and methods separately but pre-seed the class-level bound,
    -- so the result matches a single whole-class walk: a member referencing its
    -- own class/sibling/field by bare name is dropped before classification (and
    -- so never lands in any bucket, including externals when the name shadows an
    -- import alias).
    fieldsSplit = M.fromList
      [ (cn, snd (implItemSplitDepsBound (classBoundFor decl) mn env localNames
                    (stripClassMethods (TLDecl cn decl))))
      | TLDecl cn decl <- classItems ]
    classWalks  =
      [ (cn, map fst entries,
            foldl' (\acc (d', dp) -> M.insertWith unionDepSplit (A.dname d') dp acc) M.empty entries)
      | TLDecl cn decl <- classItems
      , let entries = concatMap (methodWalkEntries mn env localNames decl) (classMethodDefs decl) ]

-- | Partition an __init__ body into per-attribute construction groups and an
-- imperative tail, as statement-index lists. The type checker hoists
-- `self.x = (v if v is not None else T())` into `$tmp = (...T()); self.x = $tmp`,
-- so an attribute's construction may sit in a single-use local that precedes the
-- write. Each `self.attr = rhs` -- together with the local that builds rhs, when
-- that local is read only there -- forms one group keyed by attr; every other
-- statement is the tail. Groups and tail together partition the body, so the
-- union of their deps equals the whole-__init__ deps and the impl hash is
-- unchanged. Deferred-back selection follows a group only when attr is read
-- (attr in CN); codegen drops a whole group when attr is unread -- sound because
-- acton_malloc zero-fills, leaving an unwritten, never-read field as NULL.
splitInitByAttr :: A.Name -> [A.Stmt] -> ([(A.Name, [Int])], [Int])
splitInitByAttr selfp body = (groups, tailIxs)
  where
    idxBody = zip [0 ..] body
    defIx   = M.fromList [ (v, i) | (i, A.Assign _ [A.PVar _ v _] _) <- idxBody ]
    wInfo   = [ (attr, i, owned)
              | (i, s) <- idxBody, Just attr <- [selfAttr s]
              , let owned = ownedTemp i s ]
    ownedTemp wi (A.MutAssign _ _ (A.Var _ (A.NoQ v)))
      | Just di <- M.lookup v defIx
      , and [ v `notElem` Names.free s | (j, s) <- idxBody, j /= wi, j /= di ]
                            = Just di
    ownedTemp _ _           = Nothing
    consumed = Data.Set.fromList ([ i | (_, i, _) <- wInfo ] ++ [ di | (_, _, Just di) <- wInfo ])
    groups   = [ (attr, maybe [] (: []) owned ++ [i]) | (attr, i, owned) <- wInfo ]
    tailIxs  = [ i | (i, _) <- idxBody, not (i `Data.Set.member` consumed) ]
    selfAttr (A.MutAssign _ (A.Dot _ (A.Var _ (A.NoQ x)) attr) _) | x == selfp = Just attr
    selfAttr _ = Nothing

-- One method walk, keyed by method name -- except __init__, whose declarative
-- attribute initializers are split per assigned attribute (see splitInitByAttr),
-- so the deferred-back closure follows a `self.x = X()` only when x is read (x in
-- CN). The imperative tail keeps initKW (always followed). Aggregated over all
-- entries this equals the whole-method walk (union is the same), so impl deps and
-- the impl hash are unchanged; only the per-method breakdown gets finer.
methodWalkEntries :: A.ModName -> Env.Env0 -> Data.Set.Set A.Name -> A.Decl -> A.Decl -> [(A.Decl, DepSplit)]
methodWalkEntries mn env localNames decl d
  -- Only user classes: a compiler-generated protocol witness has an __init__ that
  -- constructs its inherited sub-witnesses, some of which the dep walk records in
  -- type position; splitting/zeroing those would drop a sub-witness and leave its
  -- constructor undeclared. Witnesses are small, so keep them whole. A witness /
  -- extension class is Derived-named (e.g. `BarProtoD_Widget`); `Names.isWitness`
  -- only catches `Internal Witness`, so exclude Derived names too or the sub-witness
  -- construction in the witness __init__ is dropped.
  | A.dname d == initKW
  , Just selfp <- selfParName d
  , not (Names.isWitness (A.dname decl) || isDerivedName (A.dname decl))
  = let body              = A.dbody d
        (groups, tailIxs) = splitInitByAttr selfp body
        pick ixs          = [ body !! i | i <- ixs ]
        walk dd           = (dd, snd (implItemSplitDepsBound (classBoundFor decl) mn env localNames
                                        (TLDecl (A.dname dd) dd)))
        -- __init__ is constructor code: its only real code-view deps are the
        -- values it constructs. Writes (`self.x = ...`) and the param-default
        -- temps the type checker hoists also drag in self's whole type and every
        -- field/param type, which would defeat per-attribute pruning. Drop the
        -- type-locals from every __init__ entry -- those types are already
        -- recorded by the class field declarations (fieldsSplit), so the impl
        -- deps/hash are unchanged; only the per-attribute code view gets sharpened.
        walkCode dd       = let (dd', (cl, _tl, es, cs)) = walk dd
                            in (dd', (cl, HashSet.empty, es, cs))
    in [ walkCode (d { A.dname = attr, A.dbody = pick ixs }) | (attr, ixs) <- groups ]
       ++ [ walkCode (d { A.dbody = pick tailIxs }) ]
  | otherwise
  = [ (d, snd (implItemSplitDepsBound (classBoundFor decl) mn env localNames (TLDecl (A.dname d) d))) ]
  where
    selfParName dd = case A.pos dd of
                       A.PosPar n _ _ _ -> Just n
                       _                -> Nothing

-- IO variant that reports one progress tick per top-level item (the same
-- granularity the old impl-deps walk used), while doing the single descent.
frontSplitDepsFromItemsWithProgress
  :: (Int -> IO ()) -> A.ModName -> Env.Env0 -> Data.Set.Set A.Name -> [TopLevelItem]
  -> IO ( M.Map A.Name [A.Name]
        , M.Map A.Name [A.QName]
        , M.Map A.Name [A.Name]
        , M.Map A.Name [(A.Name, [A.Name])]
        , M.Map A.Name [(A.Name, [A.Name])]
        , M.Map A.Name [A.Name] )
frontSplitDepsFromItemsWithProgress onProgress mn env localNames items = do
    let (extItems, nonExtItems)  = Data.List.partition isExtDecl items
        (classItems, plainItems) = Data.List.partition isClassDecl nonExtItems
        walk1 its = foldl' (\acc item -> let (n, deps) = implItemSplitDeps mn env localNames item
                                         in M.insertWith unionDepSplit n deps acc) M.empty its
        tickItem (mp, done) item = do
          let (n, deps) = implItemSplitDeps mn env localNames item
              done'     = done + 1
          depSplitSize deps `seq` onProgress done'
          pure (M.insertWith unionDepSplit n deps mp, done')
        tickClass (fs, cws, done) item@(TLDecl cn decl) = do
          let cb         = classBoundFor decl
              fieldsDeps = snd (implItemSplitDepsBound cb mn env localNames (stripClassMethods item))
              entries    = concatMap (methodWalkEntries mn env localNames decl) (classMethodDefs decl)
              mds        = map fst entries
              msplit     = foldl' (\acc (d', dp) -> M.insertWith unionDepSplit (A.dname d') dp acc) M.empty entries
              done'      = done + 1
          (depSplitSize fieldsDeps + sum (map depSplitSize (M.elems msplit))) `seq` onProgress done'
          pure (M.insert cn fieldsDeps fs, (cn, mds, msplit) : cws, done')
        tickClass acc _ = pure acc
    (plainSplit, d1)            <- foldM tickItem (M.empty, 0) plainItems
    (extSplit, d2)             <- foldM tickItem (M.empty, d1) extItems
    (fieldsSplit, classWalks, _) <- foldM tickClass (M.empty, [], d2) classItems
    pure (assembleFrontDeps fieldsSplit classWalks plainSplit extSplit)

-- Pure combination of the per-group split maps into the five views. Class fields
-- ⊎ class methods reconstructs each class's full impl deps (so they need not be
-- re-walked); base deps read codeLocals for classes/functions and code∪type for
-- extensions; method deps are the per-method full deps; called is read straight
-- off the full split.
assembleFrontDeps :: M.Map A.Name DepSplit
                  -> [(A.Name, [A.Decl], M.Map A.Name DepSplit)]
                  -> M.Map A.Name DepSplit
                  -> M.Map A.Name DepSplit
                  -> ( M.Map A.Name [A.Name]
                     , M.Map A.Name [A.QName]
                     , M.Map A.Name [A.Name]
                     , M.Map A.Name [(A.Name, [A.Name])]
                     , M.Map A.Name [(A.Name, [A.Name])]
                     , M.Map A.Name [A.Name] )
assembleFrontDeps fieldsSplit classWalks plainSplit extSplit =
    (implLocal, implExt, baseDeps, methodDeps, methodCalled, called)
  where
    mergeNub a b = Data.List.sort (Data.List.nub (a ++ b))
    classMethodsAgg = M.fromList
      [ (cn, foldl' unionDepSplit emptyDepSplitDirect (M.elems msplit))
      | (cn, _mds, msplit) <- classWalks ]
    classFullSplit = M.unionWith unionDepSplit fieldsSplit classMethodsAgg
    allFullSplit   = foldl' (M.unionWith unionDepSplit) classFullSplit [plainSplit, extSplit]
    (implLocal, implExt) = finishHashSplitDeps allFullSplit
    baseDeps = foldl' (M.unionWith mergeNub) (finishCodeSplitDeps fieldsSplit)
                 [ finishCodeSplitDeps plainSplit
                 , fst (finishHashSplitDeps extSplit) ]
    methodDeps = M.fromList
      [ (cn, [ (A.dname d, M.findWithDefault [] (A.dname d) mmap) | d <- mds ])
      | (cn, mds, msplit) <- classWalks, not (null mds)
      , let mmap = fst (finishHashSplitDeps msplit) ]
    -- Per-method called-method breakdown (4th DepSplit element), keyed exactly
    -- like methodDeps. Lets deferred-back CN growth follow only reached methods.
    methodCalled = M.fromList
      [ (cn, [ (A.dname d, M.findWithDefault [] (A.dname d) cmap) | d <- mds ])
      | (cn, mds, msplit) <- classWalks, not (null mds)
      , let cmap = calledMethodsFromSplit msplit ]
    called = calledMethodsFromSplit allFullSplit

-- Backwards-compatible 3-tuple (base, method, called) for callers that don't
-- need the impl deps.
codeMethodDepsFromItems :: A.ModName -> Env.Env0 -> Data.Set.Set A.Name -> [TopLevelItem]
                        -> ( M.Map A.Name [A.Name]
                           , M.Map A.Name [(A.Name, [A.Name])]
                           , M.Map A.Name [A.Name] )
codeMethodDepsFromItems mn env localNames items =
  let (_, _, baseDeps, methodDeps, _, called) = frontSplitDepsFromItems mn env localNames items
  in (baseDeps, methodDeps, called)

implSplitDepsFromItemsWithProgress :: (Int -> IO ())
                                   -> A.ModName
                                   -> Env.Env0
                                   -> Data.Set.Set A.Name
                                   -> [TopLevelItem]
                                   -> IO (M.Map A.Name [A.Name], M.Map A.Name [A.QName])
implSplitDepsFromItemsWithProgress onProgress mn env localNames items = do
  (deps, _) <- foldM addDeps (M.empty, 0) (map (implItemSplitDeps mn env localNames) items)
  pure (finishHashSplitDeps deps)
  where
    addDeps (acc, done) (n, deps) = do
      let acc' = M.insertWith unionDepSplit n deps acc
          done' = done + 1
      depSplitSize deps `seq` onProgress done'
      pure (acc', done')

-- A single position-tagged descent over a top-level item: code-position refs go
-- to codeLocals, type-position refs to typeLocals (see DepSplit). Both the
-- impl-hash view and the code-only view are derived from this one walk.
implItemSplitDeps :: A.ModName -> Env.Env0 -> Data.Set.Set A.Name -> TopLevelItem -> (A.Name, DepSplit)
implItemSplitDeps = implItemSplitDepsBound Data.Set.empty

-- As implItemSplitDeps but with an initial bound set. Used to walk a class's
-- fields and methods separately while matching the whole-class walk: that walk
-- threads the class-level bound into every member body, so a member referencing
-- a class name/field/sibling by bare name is dropped BEFORE classification (and
-- thus never lands in any bucket, including externals when the name shadows an
-- import alias). Pre-seeding here reproduces that exactly.
implItemSplitDepsBound :: NameSet -> A.ModName -> Env.Env0 -> Data.Set.Set A.Name -> TopLevelItem -> (A.Name, DepSplit)
implItemSplitDepsBound bound0 mn env localNames item =
  case item of
    TLDecl name decl -> (name, splitDeclDirect bound0 decl emptyDepSplitDirect)
    TLStmt name stmt -> (name, splitStmtDirect bound0 stmt emptyDepSplitDirect)
  where
    splitQName = insertQNameDep mn env localNames
    splitQNameType = insertQNameDepType mn env localNames

    -- A write target `e.attr` reads e but *writes* attr, so the written attribute
    -- is not a use and is NOT recorded in CN -- otherwise an attribute's own
    -- initializer (`self.x = ...`) would mark x used and the per-attribute
    -- __init__ dead-code elimination could never drop it. The receiver e is a
    -- genuine read and is split normally.
    splitWriteTargetDirect bound (A.Dot _ e _) acc = splitExprDirect bound e acc
    splitWriteTargetDirect bound t acc             = splitExprDirect bound t acc

    splitStmtDirect bound stmt acc = case stmt of
      A.Expr _ e             -> splitExprDirect bound e acc
      A.Assign _ ps e        -> splitExprDirect bound e (splitListInto (splitPatternDirect bound) ps acc)
      A.MutAssign _ t e      -> splitExprDirect bound e (splitWriteTargetDirect bound t acc)
      A.AugAssign _ t _ e    -> splitExprDirect bound e (splitExprDirect bound t acc)
      A.Assert _ e mbe       -> splitMaybeInto (splitExprDirect bound) mbe (splitExprDirect bound e acc)
      A.Pass _               -> acc
      A.Delete _ t           -> splitExprDirect bound t acc
      A.Return _ mbe         -> splitMaybeInto (splitExprDirect bound) mbe acc
      A.Raise _ e            -> splitExprDirect bound e acc
      A.Break _              -> acc
      A.Continue _           -> acc
      A.If _ bs els          -> splitSuiteDirect bound els (splitListInto (splitBranchDirect bound) bs acc)
      A.While _ e b els      -> splitSuiteDirect bound els (splitSuiteDirect bound b (splitExprDirect bound e acc))
      A.For _ p e b els      ->
        let bound' = boundPattern p bound
        in splitSuiteDirect bound els (splitSuiteDirect bound' b (splitExprDirect bound e (splitPatternDirect bound p acc)))
      A.Try _ b hs els fin   -> splitSuiteDirect bound fin (splitSuiteDirect bound els (splitListInto (splitHandlerDirect bound) hs (splitSuiteDirect bound b acc)))
      A.With _ ws b          ->
        let bound' = boundList boundWithItem ws bound
        in splitSuiteDirect bound' b (splitListInto (splitWithItemDirect bound) ws acc)
      A.Data _ mbp b         -> splitSuiteDirect bound b (splitMaybeInto (splitPatternDirect bound) mbp acc)
      A.VarAssign _ ps e     -> splitExprDirect bound e (splitListInto (splitPatternDirect bound) ps acc)
      A.After _ e e'         -> splitExprDirect bound e' (splitExprDirect bound e acc)
      A.Signature _ _ t _    -> splitTSchemaDirect bound t acc
      A.Decl _ ds            -> splitListInto (splitDeclDirect bound) ds acc

    splitSuiteDirect bound ss acc = splitListInto (splitStmtDirect bound) ss acc

    splitDeclDirect bound decl acc = case decl of
      A.Def _ n q ps ks _ b _ fx _ ->
        let bound' =
              Data.Set.insert n
                (assignedSuite b (boundKwdPar ks (boundPosPar ps (boundList boundQBind q bound))))
        in splitTypeDirect bound' fx (splitSuiteDirect bound' b (splitKwdParDirect bound' ks (splitPosParDirect bound' ps acc)))
      A.Actor _ n q ps ks b _ ->
        let bound' =
              Data.Set.insert n
                (Data.Set.insert Names.self
                  (assignedSuite b (boundKwdPar ks (boundPosPar ps (boundList boundQBind q bound)))))
        in splitSuiteDirect bound' b (splitKwdParDirect bound' ks (splitPosParDirect bound' ps acc))
      A.Class _ n q cs b _ ->
        let bound' = Data.Set.insert n (assignedSuite b (boundList boundQBind q bound))
        in splitSuiteDirect bound' b (splitListInto (splitTConDirect bound') cs acc)
      A.Protocol _ n q ps b _ ->
        let bound' = Data.Set.insert n (assignedSuite b (boundList boundQBind q bound))
        in splitSuiteDirect bound' b (splitListInto (splitTConDirect bound') ps acc)
      A.Extension _ q c ps b _ ->
        let bound' = assignedSuite b (boundList boundQBind q bound)
        in splitSuiteDirect bound' b (splitListInto (splitTConDirect bound') ps (splitTConDirect bound' c acc))

    splitBranchDirect bound (A.Branch e ss) acc =
      splitSuiteDirect bound ss (splitExprDirect bound e acc)

    splitHandlerDirect bound (A.Handler ex ss) acc =
      let bound' = boundExcept ex bound
      in splitSuiteDirect bound' ss (splitExceptDirect bound ex acc)

    splitExprDirect bound expr acc = case expr of
      A.Var _ qn            -> splitQName bound qn acc
      A.Int{}               -> acc
      A.Float{}             -> acc
      A.Imaginary{}         -> acc
      A.Bool{}              -> acc
      A.None{}              -> acc
      A.NotImplemented{}    -> acc
      A.Ellipsis{}          -> acc
      A.Strings{}           -> acc
      A.BStrings{}          -> acc
      A.Call _ f ps ks      -> splitKwdArgDirect bound ks (splitPosArgDirect bound ps (splitExprDirect bound f acc))
      A.Let _ ss e          ->
        let bound' = boundSuite ss bound
        in splitExprDirect bound' e (splitSuiteDirect bound ss acc)
      A.TApp _ f ts         -> splitListInto (splitTypeDirect bound) ts (splitExprDirect bound f acc)
      A.Async _ e           -> splitExprDirect bound e acc
      A.Await _ e           -> splitExprDirect bound e acc
      A.Index _ e ix        -> splitExprDirect bound ix (splitExprDirect bound e acc)
      A.Slice _ e sl        -> splitSlizDirect bound sl (splitExprDirect bound e acc)
      A.Cond _ e c e'       -> splitExprDirect bound e' (splitExprDirect bound c (splitExprDirect bound e acc))
      A.IsInstance _ e c    -> splitQName bound c (splitExprDirect bound e acc)
      A.BinOp _ e _ e'      -> splitExprDirect bound e' (splitExprDirect bound e acc)
      A.CompOp _ e ops      -> splitListInto (splitOpArgDirect bound) ops (splitExprDirect bound e acc)
      A.UnOp _ _ e          -> splitExprDirect bound e acc
      A.Dot _ e n           -> splitExprDirect bound e (addCalledMethod n acc)
      A.Rest _ e _          -> splitExprDirect bound e acc
      A.DotI _ e _          -> splitExprDirect bound e acc
      A.RestI _ e _         -> splitExprDirect bound e acc
      A.Opt _ e _           -> splitExprDirect bound e acc
      A.OptChain _ e        -> splitExprDirect bound e acc
      A.Lambda _ ps ks e _  ->
        let bound' = boundKwdPar ks (boundPosPar ps bound)
        in splitExprDirect bound' e (splitKwdParDirect bound ks (splitPosParDirect bound ps acc))
      A.Yield _ mbe         -> splitMaybeInto (splitExprDirect bound) mbe acc
      A.YieldFrom _ e       -> splitExprDirect bound e acc
      A.Tuple _ ps ks       -> splitKwdArgDirect bound ks (splitPosArgDirect bound ps acc)
      A.List _ es           -> splitListInto (splitElemDirect bound) es acc
      A.ListComp _ e c      ->
        splitCompDirect bound c (splitElemDirect (boundComp c bound) e acc)
      A.Dict _ as           -> splitListInto (splitAssocDirect bound) as acc
      A.DictComp _ a c      ->
        splitCompDirect bound c (splitAssocDirect (boundComp c bound) a acc)
      A.Set _ es            -> splitListInto (splitElemDirect bound) es acc
      A.SetComp _ e c       ->
        splitCompDirect bound c (splitElemDirect (boundComp c bound) e acc)
      A.Paren _ e           -> splitExprDirect bound e acc
      A.UnBox _ e           -> splitExprDirect bound e acc
      A.Box _ e             -> splitExprDirect bound e acc

    splitPatternDirect bound pat acc = case pat of
      A.PWild{}        -> acc
      A.PVar{}         -> acc
      A.PParen _ p     -> splitPatternDirect bound p acc
      A.PTuple _ ps ks -> splitKwdPatDirect bound ks (splitPosPatDirect bound ps acc)
      A.PList _ ps p   -> splitMaybeInto (splitPatternDirect bound) p (splitListInto (splitPatternDirect bound) ps acc)
      A.PData _ _ ixs  -> splitListInto (splitExprDirect bound) ixs acc

    splitExceptDirect bound ex acc = case ex of
      A.ExceptAll _    -> acc
      A.Except _ qn    -> splitQName bound qn acc
      A.ExceptAs _ q _ -> splitQName bound q acc

    splitPosParDirect bound p acc = case p of
      A.PosPar _ mt me rest -> splitPosParDirect bound rest (splitMaybeInto (splitExprDirect bound) me (splitMaybeInto (splitTypeDirect bound) mt acc))
      A.PosSTAR _ mt        -> splitMaybeInto (splitTypeDirect bound) mt acc
      A.PosNIL              -> acc

    splitKwdParDirect bound k acc = case k of
      A.KwdPar _ mt me rest -> splitKwdParDirect bound rest (splitMaybeInto (splitExprDirect bound) me (splitMaybeInto (splitTypeDirect bound) mt acc))
      A.KwdSTAR _ mt        -> splitMaybeInto (splitTypeDirect bound) mt acc
      A.KwdNIL              -> acc

    splitPosArgDirect bound ps acc = case ps of
      A.PosArg e rest -> splitPosArgDirect bound rest (splitExprDirect bound e acc)
      A.PosStar e     -> splitExprDirect bound e acc
      A.PosNil        -> acc

    splitKwdArgDirect bound ks acc = case ks of
      A.KwdArg _ e rest -> splitKwdArgDirect bound rest (splitExprDirect bound e acc)
      A.KwdStar e       -> splitExprDirect bound e acc
      A.KwdNil          -> acc

    splitPosPatDirect bound ps acc = case ps of
      A.PosPat p rest -> splitPosPatDirect bound rest (splitPatternDirect bound p acc)
      A.PosPatStar p  -> splitPatternDirect bound p acc
      A.PosPatNil     -> acc

    splitKwdPatDirect bound ks acc = case ks of
      A.KwdPat _ p rest -> splitKwdPatDirect bound rest (splitPatternDirect bound p acc)
      A.KwdPatStar p    -> splitPatternDirect bound p acc
      A.KwdPatNil       -> acc

    splitElemDirect bound elem1 acc = case elem1 of
      A.Elem e -> splitExprDirect bound e acc
      A.Star e -> splitExprDirect bound e acc

    splitAssocDirect bound assoc acc = case assoc of
      A.Assoc k v  -> splitExprDirect bound v (splitExprDirect bound k acc)
      A.StarStar e -> splitExprDirect bound e acc

    splitOpArgDirect bound (A.OpArg _ e) acc = splitExprDirect bound e acc

    splitSlizDirect bound (A.Sliz _ a b c) acc =
      splitMaybeInto (splitExprDirect bound) c (splitMaybeInto (splitExprDirect bound) b (splitMaybeInto (splitExprDirect bound) a acc))

    splitCompDirect bound comp acc = case comp of
      A.CompFor _ pat e c ->
        let bound' = boundPattern pat bound
        in splitCompDirect bound' c (splitExprDirect bound' e acc)
      A.CompIf _ e c      -> splitCompDirect bound c (splitExprDirect bound e acc)
      A.NoComp            -> acc

    splitWithItemDirect bound (A.WithItem e p) acc =
      splitMaybeInto (splitPatternDirect bound) p (splitExprDirect bound e acc)

    splitTSchemaDirect bound t acc =
      case t of
        A.TSchema _ q ty -> splitTypeDirect bound ty (splitListInto (splitQBindDirect bound) q acc)

    splitQBindDirect bound (A.QBind _ cs) acc =
      splitListInto (splitTConDirect bound) cs acc

    -- All type-name references flow through here, routed to typeLocals (the
    -- type-position bucket) so the code-only view can exclude them.
    splitTConDirect bound tc acc =
          case tc of
            A.TC qn ts -> splitListInto (splitTypeDirect bound) ts (splitQNameType bound qn acc)

    splitTypeDirect bound t acc = case t of
      A.TVar{}           -> acc
      A.TUni{}           -> acc
      A.TCon _ tc        -> splitTConDirect bound tc acc
      A.TFun _ fx p k r  -> splitTypeDirect bound r (splitTypeDirect bound k (splitTypeDirect bound p (splitTypeDirect bound fx acc)))
      A.TTuple _ p k     -> splitTypeDirect bound k (splitTypeDirect bound p acc)
      A.TOpt _ opt       -> splitTypeDirect bound opt acc
      A.TNone{}          -> acc
      A.TWild{}          -> acc
      A.TNil{}           -> acc
      A.TRow _ _ _ ty row -> splitTypeDirect bound row (splitTypeDirect bound ty acc)
      A.TStar _ _ row    -> splitTypeDirect bound row acc
      A.TFX{}            -> acc
      A.TUnboxed _ ty    -> splitTypeDirect bound ty acc

-- | Split deps into locals and external qualified names for hashing.
splitDeps :: A.ModName
          -> Env.Env0
          -> Data.Set.Set A.Name
          -> M.Map A.Name [A.QName]
          -> (M.Map A.Name [A.Name], M.Map A.Name [A.QName])
splitDeps mn env localNames depMap =
  finishSplitDeps (M.map (Data.Set.foldl' (addSplitDep mn env localNames) emptyDepSplit . Data.Set.fromList) depMap)

emptyDepSplit :: (NameSet, QNameSet)
emptyDepSplit = (Data.Set.empty, Data.Set.empty)

addSplitDep :: A.ModName -> Env.Env0 -> Data.Set.Set A.Name -> (NameSet, QNameSet) -> A.QName -> (NameSet, QNameSet)
addSplitDep mn env localNames (locals, externals) (A.NoQ n)
  | Data.Set.member n localNames = (Data.Set.insert n locals, externals)
addSplitDep mn env localNames (locals, externals) qn =
  case Env.unalias env qn of
    A.GName m _ | m == mPrim -> (locals, externals)
    A.QName m _ | m == mPrim -> (locals, externals)
    A.GName m n
      | m == mn && Data.Set.member n localNames -> (Data.Set.insert n locals, externals)
      | m == mn -> (locals, externals)
      | otherwise -> (locals, Data.Set.insert (A.GName m n) externals)
    A.QName m n
      | m == mn && Data.Set.member n localNames -> (Data.Set.insert n locals, externals)
      | m == mn -> (locals, externals)
      | otherwise -> (locals, Data.Set.insert (A.GName m n) externals)
    A.NoQ n
      | Data.Set.member n localNames -> (Data.Set.insert n locals, externals)
      | otherwise -> (locals, externals)

-- Classify a qualified name as a local dep (Left), an external dep (Right), or
-- neither (prim / non-local same-module). Shared by the code- and type-position
-- inserters so they agree on locality and differ only in which local bucket.
classifyQNameDep :: A.ModName -> Env.Env0 -> Data.Set.Set A.Name -> A.QName -> Maybe (Either A.Name A.QName)
classifyQNameDep _ _ localNames (A.NoQ n)
  | Data.Set.member n localNames = Just (Left n)
classifyQNameDep mn env localNames qn =
  case Env.unalias env qn of
    A.GName m _ | m == mPrim -> Nothing
    A.QName m _ | m == mPrim -> Nothing
    A.GName m n
      | m == mn && Data.Set.member n localNames -> Just (Left n)
      | m == mn -> Nothing
      | otherwise -> Just (Right (A.GName m n))
    A.QName m n
      | m == mn && Data.Set.member n localNames -> Just (Left n)
      | m == mn -> Nothing
      | otherwise -> Just (Right (A.GName m n))
    A.NoQ n
      | Data.Set.member n localNames -> Just (Left n)
      | otherwise -> Nothing

addSplitDepHash :: A.ModName -> Env.Env0 -> Data.Set.Set A.Name -> DepSplit -> A.QName -> DepSplit
addSplitDepHash mn env localNames acc@(cl, tl, es, called) qn =
  case classifyQNameDep mn env localNames qn of
    Just (Left n)  -> (HashSet.insert n cl, tl, es, called)
    Just (Right q) -> (cl, tl, HashSet.insert q es, called)
    Nothing        -> acc

addSplitDepHashType :: A.ModName -> Env.Env0 -> Data.Set.Set A.Name -> DepSplit -> A.QName -> DepSplit
addSplitDepHashType mn env localNames acc@(cl, tl, es, called) qn =
  case classifyQNameDep mn env localNames qn of
    Just (Left n)  -> (cl, HashSet.insert n tl, es, called)
    Just (Right q) -> (cl, tl, HashSet.insert q es, called)
    Nothing        -> acc

finishSplitDeps :: M.Map A.Name (NameSet, QNameSet) -> (M.Map A.Name [A.Name], M.Map A.Name [A.QName])
finishSplitDeps pairs =
  ( M.map (Data.Set.toList . fst) pairs
  , M.map (Data.Set.toList . snd) pairs
  )

-- | Impl/hash view: full local deps (code ∪ type positions) plus externals.
-- This reproduces exactly what the old includeTypes=True walk produced.
finishHashSplitDeps :: M.Map A.Name DepSplit -> (M.Map A.Name [A.Name], M.Map A.Name [A.QName])
finishHashSplitDeps pairs =
  ( M.map (\(cl, tl, _, _) -> Data.List.sort (HashSet.toList (HashSet.union cl tl))) pairs
  , M.map (\(_, _, es, _) -> Data.List.sort (HashSet.toList es)) pairs
  )

-- | Code-only view: just code-position local deps (the old includeTypes=False
-- locals), for deferred-back base deps that must not cascade through type refs.
finishCodeSplitDeps :: M.Map A.Name DepSplit -> M.Map A.Name [A.Name]
finishCodeSplitDeps = M.map (\(cl, _, _, _) -> Data.List.sort (HashSet.toList cl))

-- | Extract the called-method names (Dot attributes) collected during a split walk.
calledMethodsFromSplit :: M.Map A.Name DepSplit -> M.Map A.Name [A.Name]
calledMethodsFromSplit = M.map (\(_, _, _, cs) -> Data.List.sort (HashSet.toList cs))

-- | Collect referenced external modules from dependency lists.
externalModules :: M.Map A.Name [A.QName] -> Data.Set.Set A.ModName
externalModules deps =
  Data.Set.fromList $ mapMaybe modOf (concat (M.elems deps))
  where
    modOf qn = case qn of
      A.GName m _ -> Just m
      A.QName m _ -> Just m
      A.NoQ _ -> Nothing

mergeSortedDistinct :: Ord a => [a] -> [a] -> [a]
mergeSortedDistinct [] ys = ys
mergeSortedDistinct xs [] = xs
mergeSortedDistinct xs@(x:xs') ys@(y:ys') =
  case compare x y of
    LT -> x : mergeSortedDistinct xs' ys
    EQ -> x : mergeSortedDistinct xs' ys'
    GT -> y : mergeSortedDistinct xs ys'

-- | Compute final hashes by folding in local and external deps.
computeHashes :: M.Map A.Name B.ByteString
              -> M.Map A.Name [A.Name]
              -> M.Map A.Name [(A.QName, B.ByteString)]
              -> M.Map A.Name B.ByteString
computeHashes selfHashes localDeps extDeps =
  computeHashesSortedDeps selfHashes (M.map Data.List.sort localDeps) (M.map sortExtDeps extDeps)
  where
    sortExtDeps = Data.List.sortOn fst

-- | Compute final hashes when local and external dep lists are already sorted.
computeHashesSortedDeps :: M.Map A.Name B.ByteString
                        -> M.Map A.Name [A.Name]
                        -> M.Map A.Name [(A.QName, B.ByteString)]
                        -> M.Map A.Name B.ByteString
computeHashesSortedDeps selfHashes localDeps extDeps =
  unsafePerformIO $ withHashScratch $ \sink -> do
    fast <- computeAcyclic sink
    case fast of
      Just hashes -> pure hashes
      Nothing     -> computeScc sink
  where
    computeAcyclic sink = go M.empty (M.keys selfHashes)
      where
        go acc [] = pure (Just acc)
        go acc (n:ns) = do
          acc' <- visit 0 HashSet.empty acc n
          case acc' of
            Just m  -> go m ns
            Nothing -> pure Nothing

        visit depth visiting acc n
          | M.member n acc = pure (Just acc)
          | M.notMember n selfHashes = pure (Just acc)
          | depth > maxAcyclicHashDepth = pure Nothing
          | HashSet.member n visiting = pure Nothing
          | otherwise = do
              let visiting' = HashSet.insert n visiting
                  locals = M.findWithDefault [] n localDeps
                  externals = M.findWithDefault [] n extDeps
              acc' <- foldM (visitDep (depth + 1) visiting') (Just acc) locals
              case acc' of
                Nothing -> pure Nothing
                Just m  -> do
                  finalHash <-
                    hashSelfWithDepsFromWith
                      sink
                      (selfHashes M.! n)
                      (lookupLocalHash m)
                      locals
                      externals
                  pure (Just (M.insert n finalHash m))

        visitDep _ _ Nothing _ = pure Nothing
        visitDep depth visiting (Just acc) n = visit depth visiting acc n

    computeScc sink =
      foldM (addScc sink) M.empty (stronglyConnComp nodes)

    nodes = [ (n, n, M.findWithDefault [] n localDeps) | n <- M.keys selfHashes ]

    addScc sink acc scc = case scc of
      AcyclicSCC n ->
        let lookupHash = lookupLocalHash acc
        in do
          finalHash <-
            hashSelfWithDepsFromWith
              sink
              (selfHashes M.! n)
              lookupHash
              (M.findWithDefault [] n localDeps)
              (M.findWithDefault [] n extDeps)
          pure (M.insert n finalHash acc)
      CyclicSCC ns ->
        let nsSet = Data.Set.fromList ns
            selfHashesSorted = [ selfHashes M.! n | n <- Data.List.sortOn nameKey ns ]
            outsideDeps = Data.Set.toList $ Data.Set.fromList
              [ d | n <- ns, d <- M.findWithDefault [] n localDeps, Data.Set.notMember d nsSet ]
            externalDeps = Data.Set.toList $ Data.Set.fromList (concat [ M.findWithDefault [] n extDeps | n <- ns ])
            lookupHash = lookupLocalHash acc
        in do
          groupHash <- hashCycleGroupFromWith sink selfHashesSorted lookupHash outsideDeps externalDeps
          let insertOne m n = do
                finalHash <- hashCycleMemberWith sink (selfHashes M.! n) groupHash n
                pure (M.insert n finalHash m)
          foldM insertOne acc ns

    lookupLocalHash acc n =
      case M.lookup n acc of
        Just h -> Just h
        Nothing -> M.lookup n selfHashes

maxAcyclicHashDepth :: Int
maxAcyclicHashDepth = 10000

-- | Build NameHashInfo entries from per-name hashes and deps.
nameInfoHashes :: M.Map A.Name I.NameInfo -> M.Map A.Name B.ByteString
nameInfoHashes infos =
  unsafePerformIO $ nameInfoHashesWithProgress (\_ -> pure ()) infos

nameInfoHashesWithProgress :: (Int -> IO ()) -> M.Map A.Name I.NameInfo -> IO (M.Map A.Name B.ByteString)
nameInfoHashesWithProgress onProgress infos = do
  caps <- getNumCapabilities
  let groups = M.toAscList infos
      chunkSize = max 16 ((length groups + caps - 1) `max` 1 `div` max 1 caps)
      chunks = chunksOf (max 1 chunkSize) groups
  doneRef <- newIORef 0
  maps <- mapConcurrently (hashChunk doneRef) chunks
  pure (M.unions maps)
  where
    hashChunk doneRef chunk =
      withHashScratch $ \sink ->
        M.fromList <$> mapM (hashOne sink doneRef) chunk

    hashOne sink doneRef (n, info) = do
      h <- hashFeedWith sink (feedNameInfo info)
      done <- atomicModifyIORef' doneRef $ \d -> (d + 1, d + 1)
      onProgress done
      pure (n, h)

prepareNameHashInputs :: A.ModName
                      -> Env.Env0
                      -> [TopLevelItem]
                      -> [TopLevelItem]
                      -> M.Map A.Name I.NameInfo
                      -> NameHashInputs
prepareNameHashInputs mn env srcItems implItems nameInfoMap =
  let nameSrcHashes = nameHashesFromItems srcItems
      nameImplHashes = nameHashesFromItems implItems
      nameKeys = M.keysSet nameSrcHashes `Data.Set.union` M.keysSet nameImplHashes
      selfPubHashes = nameInfoHashes nameInfoMap
      (pubSigLocalDeps, pubSigExtDeps) = pubSigSplitDepsFromNameInfoMap mn env nameKeys nameInfoMap
      (implLocalDeps, implExtDeps) = implSplitDepsFromItems mn env nameKeys implItems
  in
  finishNameHashInputs nameKeys nameSrcHashes nameImplHashes selfPubHashes pubSigLocalDeps pubSigExtDeps implLocalDeps implExtDeps

finishNameHashInputs :: Data.Set.Set A.Name
                     -> M.Map A.Name B.ByteString
                     -> M.Map A.Name B.ByteString
                     -> M.Map A.Name B.ByteString
                     -> M.Map A.Name [A.Name]
                     -> M.Map A.Name [A.QName]
                     -> M.Map A.Name [A.Name]
                     -> M.Map A.Name [A.QName]
                     -> NameHashInputs
finishNameHashInputs nameKeys nameSrcHashes nameImplHashes selfPubHashes pubSigLocalDeps pubSigExtDeps implLocalDeps implExtDeps =
  NameHashInputs
    { nhiNameKeys = nameKeys
    , nhiNameSrcHashes = nameSrcHashes
    , nhiNameImplHashes = nameImplHashes
    , nhiSelfPubHashes = selfPubHashes
    , nhiPubSigLocalDeps = pubSigLocalDeps
    , nhiPubSigExtDeps = pubSigExtDeps
    , nhiImplLocalDeps = implLocalDeps
    , nhiImplExtDeps = implExtDeps
    }

assembleNameHashes :: Data.Set.Set A.Name
                   -> M.Map A.Name B.ByteString
                   -> M.Map A.Name B.ByteString
                   -> M.Map A.Name B.ByteString
                   -> M.Map A.Name B.ByteString
                   -> M.Map A.Name [A.Name]
                   -> M.Map A.Name [A.Name]
                   -> M.Map A.Name [(A.QName, B.ByteString)]
                   -> M.Map A.Name [(A.QName, B.ByteString)]
                   -> Maybe (M.Map A.Name [A.Name])
                   -> Maybe (M.Map A.Name [(A.Name, [A.Name])])
                   -> Maybe (M.Map A.Name [(A.Name, [A.Name])])
                   -> Maybe (M.Map A.Name [A.Name])
                   -> [InterfaceFiles.NameHashInfo]
assembleNameHashes nameKeys nameSrcHashes pubHashes implHashes ownImplHashes pubLocalDeps implLocalDeps pubExtHashes implExtHashes mCodeLocalDeps mMethodCodeDeps mMethodCalled mCalledMethods =
  let namesSorted = Data.List.sortOn nameKey (Data.Set.toList nameKeys)
      localDeps m n = Data.List.sortOn nameKey (M.findWithDefault [] n m)
  in
    [ InterfaceFiles.NameHashInfo
        { InterfaceFiles.nhName = n
        , InterfaceFiles.nhSrcHash = M.findWithDefault B.empty n nameSrcHashes
        , InterfaceFiles.nhPubHash = M.findWithDefault B.empty n pubHashes
        , InterfaceFiles.nhImplHash = M.findWithDefault B.empty n implHashes
        , InterfaceFiles.nhOwnImplHash = M.findWithDefault B.empty n ownImplHashes
        , InterfaceFiles.nhPubLocalDeps = localDeps pubLocalDeps n
        , InterfaceFiles.nhImplLocalDeps = localDeps implLocalDeps n
        , InterfaceFiles.nhCodeLocalDeps = fmap (\m -> localDeps m n) mCodeLocalDeps
        , InterfaceFiles.nhMethodCodeDeps = fmap (\m -> M.findWithDefault [] n m) mMethodCodeDeps
        , InterfaceFiles.nhCalledMethods = fmap (\m -> localDeps m n) mCalledMethods
        , InterfaceFiles.nhMethodCalledMethods = fmap (\m -> M.findWithDefault [] n m) mMethodCalled
        , InterfaceFiles.nhPubDeps = M.findWithDefault [] n pubExtHashes
        , InterfaceFiles.nhImplDeps = M.findWithDefault [] n implExtHashes
        , InterfaceFiles.nhStmtIndices = []
        }
    | n <- namesSorted
    ]

buildNameHashes :: Data.Set.Set A.Name
                -> M.Map A.Name B.ByteString
                -> M.Map A.Name B.ByteString
                -> M.Map A.Name I.NameInfo
                -> M.Map A.Name [A.Name]
                -> M.Map A.Name [(A.QName, B.ByteString)]
                -> M.Map A.Name [A.Name]
                -> M.Map A.Name [A.Name]
                -> M.Map A.Name [(A.QName, B.ByteString)]
                -> M.Map A.Name [(A.QName, B.ByteString)]
                -> [InterfaceFiles.NameHashInfo]
buildNameHashes nameKeys nameSrcHashes nameImplHashes nameInfoMap pubSigLocalDeps pubSigExtHashes pubLocalDeps implLocalDeps implExtHashes pubExtHashes =
  let selfPubHashes = nameInfoHashes nameInfoMap
      selfImplHashes = nameImplHashes
      pubHashes = computeHashes selfPubHashes pubSigLocalDeps pubSigExtHashes
      implHashes = computeHashes selfImplHashes implLocalDeps implExtHashes
  in assembleNameHashes nameKeys nameSrcHashes pubHashes implHashes selfImplHashes pubLocalDeps implLocalDeps pubExtHashes implExtHashes Nothing Nothing Nothing Nothing

-- | Refresh impl hashes and impl deps for existing name hashes.
refreshImplHashes :: [InterfaceFiles.NameHashInfo]
                  -> M.Map A.Name B.ByteString
                  -> M.Map A.Name [A.Name]
                  -> M.Map A.Name [(A.QName, B.ByteString)]
                  -> [InterfaceFiles.NameHashInfo]
refreshImplHashes nameHashes nameImplHashes implLocalDeps implExtHashes =
  let implHashes = computeHashesSortedDeps nameImplHashes implLocalDeps implExtHashes
  in
    -- Return EVERY input name, refreshed or not: the result feeds the module
    -- impl hash, the dependency index rows and the dependency module list,
    -- all of which must cover the whole module. A name outside the refresh
    -- domain (no impl item, empty own hash) passes through unchanged -- its
    -- stored (empty) impl hash is exactly what the front pass would produce,
    -- and its pub/impl deps must keep their places in the regenerated rows.
    [ if M.member n nameImplHashes
        then nh { InterfaceFiles.nhImplHash = M.findWithDefault B.empty n implHashes
                , InterfaceFiles.nhImplLocalDeps = Data.List.sortOn nameKey (M.findWithDefault [] n implLocalDeps)
                , InterfaceFiles.nhImplDeps = M.findWithDefault [] n implExtHashes
                }
        else nh
    | nh <- nameHashes
    , let n = InterfaceFiles.nhName nh
    ]

-- | Hash module-level pub/impl summaries from the final per-name hash maps.
moduleHashesFromHashMaps :: I.NModule
                         -> Data.Set.Set A.Name
                         -> M.Map A.Name B.ByteString
                         -> M.Map A.Name B.ByteString
                         -> (B.ByteString, B.ByteString)
moduleHashesFromHashMaps nmod nameKeys pubHashes implHashes =
  (modulePubHashFromHashMap nmod pubHashes, moduleImplHashFromHashMap nameKeys implHashes)

-- | Hash the module public interface entries.
modulePubHashFromIface :: I.NModule -> [InterfaceFiles.NameHashInfo] -> B.ByteString
modulePubHashFromIface nmod nameHashes =
  modulePubHashFromHashMap nmod $
    M.fromList
        [ (InterfaceFiles.nhName nh, InterfaceFiles.nhPubHash nh)
        | nh <- nameHashes
        ]

modulePubHashFromHashMap :: I.NModule -> M.Map A.Name B.ByteString -> B.ByteString
modulePubHashFromHashMap nmod pubHashes =
  let I.NModule _ iface _ = nmod
      pubNamesSorted =
        Data.List.sortOn nameKey [ n | (n, _) <- iface, Names.isPublicName n ]
  in hashNameHashMapEntriesForNames 3 pubHashes pubNamesSorted

-- | Hash the module impl entries from per-name impl hashes.
moduleImplHashFromNameHashes :: [InterfaceFiles.NameHashInfo] -> B.ByteString
moduleImplHashFromNameHashes infos =
  hashNameHashEntriesWith 4 infosSorted $ \nh sink ->
    feedString (nameKey (InterfaceFiles.nhName nh)) sink >>
    feedHashBytes (InterfaceFiles.nhImplHash nh) sink
  where
    infosSorted = Data.List.sortOn (nameKey . InterfaceFiles.nhName) infos

moduleImplHashFromHashMap :: Data.Set.Set A.Name -> M.Map A.Name B.ByteString -> B.ByteString
moduleImplHashFromHashMap nameKeys implHashes =
  let namesSorted = Data.List.sortOn nameKey (Data.Set.toList nameKeys)
  in hashNameHashMapEntriesForNames 4 implHashes namesSorted
