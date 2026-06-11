module Acton.Hashing
  ( TopLevelItem(..)
  , topLevelItems
  , nameHashesFromItems
  , implDepsFromItems
  , splitDeps
  , externalModules
  , computeHashes
  , buildNameHashes
  , refreshImplHashes
  , modulePubHashFromIface
  , moduleImplHashFromNameHashes
  , nameKey
  , qnameKey
  ) where

import qualified Acton.Env as Env
import qualified Acton.NameInfo as I
import qualified Acton.Names as Names
import Acton.Prim (mPrim)
import qualified Acton.Syntax as A
import qualified InterfaceFiles
import Utils (SrcLoc(..))

import qualified Data.Persist as Persist
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Char8 as B
import Data.Graph (SCC(..), stronglyConnComp)
import Data.List (foldl', intercalate, nub)
import qualified Data.List
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Set


data TopLevelItem = TLDecl A.Name A.Decl | TLStmt A.Name A.Stmt

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

-- | Collect binary-encoded, location-free AST fragments per top-level name.
nameBytesFromItems :: [TopLevelItem] -> M.Map A.Name B.ByteString
nameBytesFromItems items =
  foldl' addFrag M.empty items
  where
    addFrag acc item =
      let (n, frag) = case item of
            TLDecl name decl -> (name, astFragBytes 0 (stripLocsDecl decl))
            TLStmt name stmt -> (name, astFragBytes 1 (stripLocsStmt stmt))
      in M.insertWith appendFrag n frag acc
    appendFrag new old = old `B.append` B.cons '\n' new

    astFragBytes :: Persist.Persist a => Int -> a -> B.ByteString
    astFragBytes tag x = Persist.encode (tag, x)

-- | Hash each name's normalized AST fragments.
nameHashesFromItems :: [TopLevelItem] -> M.Map A.Name B.ByteString
nameHashesFromItems items =
  M.map SHA256.hash (nameBytesFromItems items)

stripLocsStmt :: A.Stmt -> A.Stmt
stripLocsStmt stmt = case stmt of
  A.Expr _ e             -> A.Expr NoLoc (stripLocsExpr e)
  A.Assign _ ps e        -> A.Assign NoLoc (map stripLocsPattern ps) (stripLocsExpr e)
  A.MutAssign _ t e      -> A.MutAssign NoLoc (stripLocsExpr t) (stripLocsExpr e)
  A.AugAssign _ t op e   -> A.AugAssign NoLoc (stripLocsExpr t) op (stripLocsExpr e)
  A.Assert _ e mbe       -> A.Assert NoLoc (stripLocsExpr e) (fmap stripLocsExpr mbe)
  A.Pass _               -> A.Pass NoLoc
  A.Delete _ t           -> A.Delete NoLoc (stripLocsExpr t)
  A.Return _ mbe         -> A.Return NoLoc (fmap stripLocsExpr mbe)
  A.Raise _ e            -> A.Raise NoLoc (stripLocsExpr e)
  A.Break _              -> A.Break NoLoc
  A.Continue _           -> A.Continue NoLoc
  A.If _ bs els          -> A.If NoLoc (map stripLocsBranch bs) (stripLocsSuite els)
  A.While _ e b els      -> A.While NoLoc (stripLocsExpr e) (stripLocsSuite b) (stripLocsSuite els)
  A.For _ p e b els      -> A.For NoLoc (stripLocsPattern p) (stripLocsExpr e) (stripLocsSuite b) (stripLocsSuite els)
  A.Try _ b hs els fin   -> A.Try NoLoc (stripLocsSuite b) (map stripLocsHandler hs) (stripLocsSuite els) (stripLocsSuite fin)
  A.With _ ws b          -> A.With NoLoc (map stripLocsWithItem ws) (stripLocsSuite b)
  A.Data _ mbp b         -> A.Data NoLoc (fmap stripLocsPattern mbp) (stripLocsSuite b)
  A.VarAssign _ ps e     -> A.VarAssign NoLoc (map stripLocsPattern ps) (stripLocsExpr e)
  A.After _ e e'         -> A.After NoLoc (stripLocsExpr e) (stripLocsExpr e')
  A.Signature _ ns t dec -> A.Signature NoLoc (map stripLocsName ns) (stripLocsTSchema t) dec
  A.Decl _ ds            -> A.Decl NoLoc (map stripLocsDecl ds)

stripLocsSuite :: A.Suite -> A.Suite
stripLocsSuite = map stripLocsStmt

stripLocsDecl :: A.Decl -> A.Decl
stripLocsDecl decl = case decl of
  A.Def _ n q p k a b dec fx doc ->
    A.Def NoLoc (stripLocsName n) (stripLocsQBinds q) (stripLocsPosPar p) (stripLocsKwdPar k)
      (fmap stripLocsType a) (stripLocsSuite b) dec (stripLocsType fx) doc
  A.Actor _ n q p k b doc ->
    A.Actor NoLoc (stripLocsName n) (stripLocsQBinds q) (stripLocsPosPar p) (stripLocsKwdPar k)
      (stripLocsSuite b) doc
  A.Class _ n q cs b doc ->
    A.Class NoLoc (stripLocsName n) (stripLocsQBinds q) (map stripLocsTCon cs) (stripLocsSuite b) doc
  A.Protocol _ n q ps b doc ->
    A.Protocol NoLoc (stripLocsName n) (stripLocsQBinds q) (map stripLocsTCon ps) (stripLocsSuite b) doc
  A.Extension _ q c ps b doc ->
    A.Extension NoLoc (stripLocsQBinds q) (stripLocsTCon c) (map stripLocsTCon ps) (stripLocsSuite b) doc

stripLocsExpr :: A.Expr -> A.Expr
stripLocsExpr expr = case expr of
  A.Var _ qn            -> A.Var NoLoc (stripLocsQName qn)
  A.Int _ i s           -> A.Int NoLoc i s
  A.Float _ d s         -> A.Float NoLoc d s
  A.Imaginary _ d s     -> A.Imaginary NoLoc d s
  A.Bool _ b            -> A.Bool NoLoc b
  A.None _              -> A.None NoLoc
  A.NotImplemented _    -> A.NotImplemented NoLoc
  A.Ellipsis _          -> A.Ellipsis NoLoc
  A.Strings _ ss        -> A.Strings NoLoc ss
  A.BStrings _ ss       -> A.BStrings NoLoc ss
  A.Call _ f ps ks      -> A.Call NoLoc (stripLocsExpr f) (stripLocsPosArg ps) (stripLocsKwdArg ks)
  A.Let _ ss e          -> A.Let NoLoc (stripLocsSuite ss) (stripLocsExpr e)
  A.TApp _ f ts         -> A.TApp NoLoc (stripLocsExpr f) (map stripLocsType ts)
  A.Async _ e           -> A.Async NoLoc (stripLocsExpr e)
  A.Await _ e           -> A.Await NoLoc (stripLocsExpr e)
  A.Index _ e ix        -> A.Index NoLoc (stripLocsExpr e) (stripLocsExpr ix)
  A.Slice _ e sl        -> A.Slice NoLoc (stripLocsExpr e) (stripLocsSliz sl)
  A.Cond _ e c e'       -> A.Cond NoLoc (stripLocsExpr e) (stripLocsExpr c) (stripLocsExpr e')
  A.IsInstance _ e c    -> A.IsInstance NoLoc (stripLocsExpr e) (stripLocsQName c)
  A.BinOp _ e op e'     -> A.BinOp NoLoc (stripLocsExpr e) op (stripLocsExpr e')
  A.CompOp _ e ops      -> A.CompOp NoLoc (stripLocsExpr e) (map stripLocsOpArg ops)
  A.UnOp _ op e         -> A.UnOp NoLoc op (stripLocsExpr e)
  A.Dot _ e n           -> A.Dot NoLoc (stripLocsExpr e) (stripLocsName n)
  A.Rest _ e n          -> A.Rest NoLoc (stripLocsExpr e) (stripLocsName n)
  A.DotI _ e i          -> A.DotI NoLoc (stripLocsExpr e) i
  A.RestI _ e i         -> A.RestI NoLoc (stripLocsExpr e) i
  A.Opt _ e b           -> A.Opt NoLoc (stripLocsExpr e) b
  A.OptChain _ e        -> A.OptChain NoLoc (stripLocsExpr e)
  A.Lambda _ p k e fx   -> A.Lambda NoLoc (stripLocsPosPar p) (stripLocsKwdPar k) (stripLocsExpr e) (stripLocsType fx)
  A.Yield _ mbe         -> A.Yield NoLoc (fmap stripLocsExpr mbe)
  A.YieldFrom _ e       -> A.YieldFrom NoLoc (stripLocsExpr e)
  A.Tuple _ ps ks       -> A.Tuple NoLoc (stripLocsPosArg ps) (stripLocsKwdArg ks)
  A.List _ es           -> A.List NoLoc (map stripLocsElem es)
  A.ListComp _ e c      -> A.ListComp NoLoc (stripLocsElem e) (stripLocsComp c)
  A.Dict _ as           -> A.Dict NoLoc (map stripLocsAssoc as)
  A.DictComp _ a c      -> A.DictComp NoLoc (stripLocsAssoc a) (stripLocsComp c)
  A.Set _ es            -> A.Set NoLoc (map stripLocsElem es)
  A.SetComp _ e c       -> A.SetComp NoLoc (stripLocsElem e) (stripLocsComp c)
  A.Paren _ e           -> A.Paren NoLoc (stripLocsExpr e)
  A.Box t e             -> A.Box (stripLocsType t) (stripLocsExpr e)
  A.UnBox t e           -> A.UnBox (stripLocsType t) (stripLocsExpr e)

stripLocsPattern :: A.Pattern -> A.Pattern
stripLocsPattern pat = case pat of
  A.PWild _ mt       -> A.PWild NoLoc (fmap stripLocsType mt)
  A.PVar _ n mt      -> A.PVar NoLoc (stripLocsName n) (fmap stripLocsType mt)
  A.PParen _ p       -> A.PParen NoLoc (stripLocsPattern p)
  A.PTuple _ ps ks   -> A.PTuple NoLoc (stripLocsPosPat ps) (stripLocsKwdPat ks)
  A.PList _ ps tailp -> A.PList NoLoc (map stripLocsPattern ps) (fmap stripLocsPattern tailp)
  A.PData _ n ixs    -> A.PData NoLoc (stripLocsName n) (map stripLocsExpr ixs)

stripLocsType :: A.Type -> A.Type
stripLocsType t = case t of
  A.TUni _ u          -> A.TUni NoLoc (stripLocsTUni u)
  A.TVar _ tv         -> A.TVar NoLoc (stripLocsTVar tv)
  A.TCon _ tc         -> A.TCon NoLoc (stripLocsTCon tc)
  A.TFun _ fx p k r   -> A.TFun NoLoc (stripLocsType fx) (stripLocsType p) (stripLocsType k) (stripLocsType r)
  A.TTuple _ p k      -> A.TTuple NoLoc (stripLocsType p) (stripLocsType k)
  A.TOpt _ opt        -> A.TOpt NoLoc (stripLocsType opt)
  A.TNone _           -> A.TNone NoLoc
  A.TWild _           -> A.TWild NoLoc
  A.TNil _ k          -> A.TNil NoLoc (stripLocsKind k)
  A.TRow _ k n ty row -> A.TRow NoLoc (stripLocsKind k) (stripLocsName n) (stripLocsType ty) (stripLocsType row)
  A.TStar _ k row     -> A.TStar NoLoc (stripLocsKind k) (stripLocsType row)
  A.TFX _ fx          -> A.TFX NoLoc fx
  A.TUnboxed _ ty     -> A.TUnboxed NoLoc (stripLocsType ty)

stripLocsTSchema :: A.TSchema -> A.TSchema
stripLocsTSchema (A.TSchema _ q t) = A.TSchema NoLoc (stripLocsQBinds q) (stripLocsType t)

stripLocsTCon :: A.TCon -> A.TCon
stripLocsTCon (A.TC qn ts) = A.TC (stripLocsQName qn) (map stripLocsType ts)

stripLocsQBind :: A.QBind -> A.QBind
stripLocsQBind (A.QBind tv cs) = A.QBind (stripLocsTVar tv) (map stripLocsTCon cs)

stripLocsQBinds :: A.QBinds -> A.QBinds
stripLocsQBinds = map stripLocsQBind

stripLocsTVar :: A.TVar -> A.TVar
stripLocsTVar (A.TV k n) = A.TV (stripLocsKind k) (stripLocsName n)

stripLocsTUni :: A.TUni -> A.TUni
stripLocsTUni (A.UV k l i) = A.UV (stripLocsKind k) l i

stripLocsKind :: A.Kind -> A.Kind
stripLocsKind (A.KFun ks k) = A.KFun (map stripLocsKind ks) (stripLocsKind k)
stripLocsKind k            = k

stripLocsPosPar :: A.PosPar -> A.PosPar
stripLocsPosPar p = case p of
  A.PosPar n mt me rest -> A.PosPar (stripLocsName n) (fmap stripLocsType mt) (fmap stripLocsExpr me) (stripLocsPosPar rest)
  A.PosSTAR n mt        -> A.PosSTAR (stripLocsName n) (fmap stripLocsType mt)
  A.PosNIL              -> A.PosNIL

stripLocsKwdPar :: A.KwdPar -> A.KwdPar
stripLocsKwdPar k = case k of
  A.KwdPar n mt me rest -> A.KwdPar (stripLocsName n) (fmap stripLocsType mt) (fmap stripLocsExpr me) (stripLocsKwdPar rest)
  A.KwdSTAR n mt        -> A.KwdSTAR (stripLocsName n) (fmap stripLocsType mt)
  A.KwdNIL              -> A.KwdNIL

stripLocsPosArg :: A.PosArg -> A.PosArg
stripLocsPosArg ps = case ps of
  A.PosArg e rest -> A.PosArg (stripLocsExpr e) (stripLocsPosArg rest)
  A.PosStar e     -> A.PosStar (stripLocsExpr e)
  A.PosNil        -> A.PosNil

stripLocsKwdArg :: A.KwdArg -> A.KwdArg
stripLocsKwdArg ks = case ks of
  A.KwdArg n e rest -> A.KwdArg (stripLocsName n) (stripLocsExpr e) (stripLocsKwdArg rest)
  A.KwdStar e       -> A.KwdStar (stripLocsExpr e)
  A.KwdNil          -> A.KwdNil

stripLocsPosPat :: A.PosPat -> A.PosPat
stripLocsPosPat ps = case ps of
  A.PosPat p rest -> A.PosPat (stripLocsPattern p) (stripLocsPosPat rest)
  A.PosPatStar p  -> A.PosPatStar (stripLocsPattern p)
  A.PosPatNil     -> A.PosPatNil

stripLocsKwdPat :: A.KwdPat -> A.KwdPat
stripLocsKwdPat ks = case ks of
  A.KwdPat n p rest -> A.KwdPat (stripLocsName n) (stripLocsPattern p) (stripLocsKwdPat rest)
  A.KwdPatStar p    -> A.KwdPatStar (stripLocsPattern p)
  A.KwdPatNil       -> A.KwdPatNil

stripLocsBranch :: A.Branch -> A.Branch
stripLocsBranch (A.Branch e ss) = A.Branch (stripLocsExpr e) (stripLocsSuite ss)

stripLocsHandler :: A.Handler -> A.Handler
stripLocsHandler (A.Handler ex ss) = A.Handler (stripLocsExcept ex) (stripLocsSuite ss)

stripLocsExcept :: A.Except -> A.Except
stripLocsExcept ex = case ex of
  A.ExceptAll _    -> A.ExceptAll NoLoc
  A.Except _ qn    -> A.Except NoLoc (stripLocsQName qn)
  A.ExceptAs _ q n -> A.ExceptAs NoLoc (stripLocsQName q) (stripLocsName n)

stripLocsElem :: A.Elem -> A.Elem
stripLocsElem elem1 = case elem1 of
  A.Elem e -> A.Elem (stripLocsExpr e)
  A.Star e -> A.Star (stripLocsExpr e)

stripLocsAssoc :: A.Assoc -> A.Assoc
stripLocsAssoc assoc = case assoc of
  A.Assoc k v  -> A.Assoc (stripLocsExpr k) (stripLocsExpr v)
  A.StarStar e -> A.StarStar (stripLocsExpr e)

stripLocsOpArg :: A.OpArg -> A.OpArg
stripLocsOpArg (A.OpArg op e) = A.OpArg op (stripLocsExpr e)

stripLocsSliz :: A.Sliz -> A.Sliz
stripLocsSliz (A.Sliz _ a b c) = A.Sliz NoLoc (fmap stripLocsExpr a) (fmap stripLocsExpr b) (fmap stripLocsExpr c)

stripLocsComp :: A.Comp -> A.Comp
stripLocsComp comp = case comp of
  A.CompFor _ p e c -> A.CompFor NoLoc (stripLocsPattern p) (stripLocsExpr e) (stripLocsComp c)
  A.CompIf _ e c    -> A.CompIf NoLoc (stripLocsExpr e) (stripLocsComp c)
  A.NoComp          -> A.NoComp

stripLocsWithItem :: A.WithItem -> A.WithItem
stripLocsWithItem (A.WithItem e p) = A.WithItem (stripLocsExpr e) (fmap stripLocsPattern p)

stripLocsQName :: A.QName -> A.QName
stripLocsQName qn = case qn of
  A.QName mn n -> A.QName (stripLocsModName mn) (stripLocsName n)
  A.NoQ n      -> A.NoQ (stripLocsName n)
  A.GName mn n -> A.GName (stripLocsModName mn) (stripLocsName n)

stripLocsModName :: A.ModName -> A.ModName
stripLocsModName (A.ModName ns) = A.ModName (map stripLocsName ns)

stripLocsName :: A.Name -> A.Name
stripLocsName n = case n of
  A.Name _ s      -> A.Name NoLoc s
  A.Derived n1 n2 -> A.Derived (stripLocsName n1) (stripLocsName n2)
  A.Internal{}    -> n

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

-- | Split deps into locals and external qualified names for hashing.
splitDeps :: A.ModName
          -> Env.Env0
          -> Data.Set.Set A.Name
          -> M.Map A.Name [A.QName]
          -> (M.Map A.Name [A.Name], M.Map A.Name [A.QName])
splitDeps mn env localNames depMap =
  let toLocalExt qns = Data.Set.foldl' step (Data.Set.empty, Data.Set.empty) (Data.Set.fromList qns)
      step (locals, externals) (A.NoQ n)
        | Data.Set.member n localNames = (Data.Set.insert n locals, externals)
      step (locals, externals) qn =
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
      pairs = M.map toLocalExt depMap
      localMap = M.map (Data.Set.toList . fst) pairs
      extMap = M.map (Data.Set.toList . snd) pairs
  in (localMap, extMap)

-- | Collect referenced external modules from dependency lists.
externalModules :: M.Map A.Name [A.QName] -> Data.Set.Set A.ModName
externalModules deps =
  Data.Set.fromList $ mapMaybe modOf (concat (M.elems deps))
  where
    modOf qn = case qn of
      A.GName m _ -> Just m
      A.QName m _ -> Just m
      A.NoQ _ -> Nothing

-- | Compute final hashes by folding in local and external deps.
computeHashes :: M.Map A.Name B.ByteString
              -> M.Map A.Name [A.Name]
              -> M.Map A.Name [(A.QName, B.ByteString)]
              -> M.Map A.Name B.ByteString
computeHashes selfHashes localDeps extDeps =
  foldl' addScc M.empty (stronglyConnComp nodes)
  where
    nodes = [ (n, n, M.findWithDefault [] n localDeps) | n <- M.keys selfHashes ]

    addScc acc scc = case scc of
      AcyclicSCC n ->
        let depHashes = depHashList acc (M.findWithDefault [] n localDeps) (M.findWithDefault [] n extDeps)
            finalHash = SHA256.hash (Persist.encode (selfHashes M.! n, depHashes))
        in M.insert n finalHash acc
      CyclicSCC ns ->
        let nsSet = Data.Set.fromList ns
            selfHashesSorted = [ selfHashes M.! n | n <- Data.List.sortOn nameKey ns ]
            outsideDeps = Data.Set.toList $ Data.Set.fromList
              [ d | n <- ns, d <- M.findWithDefault [] n localDeps, Data.Set.notMember d nsSet ]
            externalDeps = Data.Set.toList $ Data.Set.fromList (concat [ M.findWithDefault [] n extDeps | n <- ns ])
            depHashes = depHashList acc outsideDeps externalDeps
            groupHash = SHA256.hash (Persist.encode (selfHashesSorted, depHashes))
            insertOne m n =
              let finalHash = SHA256.hash (Persist.encode (selfHashes M.! n, groupHash, nameKey n))
              in M.insert n finalHash m
        in foldl' insertOne acc ns

    depHashList acc locals externals =
      let lookupHash n = case M.lookup n acc of
                           Just h -> Just h
                           Nothing -> M.lookup n selfHashes
          localHashes =
            [ h | n <- Data.List.sortOn nameKey locals
                , Just h <- [lookupHash n] ]
          externalHashes =
            [ h | (_, h) <- Data.List.sortOn (qnameKey . fst) externals ]
      in localHashes ++ externalHashes

-- | Build NameHashInfo entries from per-name hashes and deps.
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
  let hashNameInfo info = SHA256.hash (Persist.encode (I.stripLocsNI (I.stripDocsNI info)))
      selfPubHashes = M.map hashNameInfo nameInfoMap
      selfImplHashes = nameImplHashes
      pubHashes = computeHashes selfPubHashes pubSigLocalDeps pubSigExtHashes
      implHashes = computeHashes selfImplHashes implLocalDeps implExtHashes
      namesSorted = Data.List.sortOn nameKey (Data.Set.toList nameKeys)
      localDeps m n = Data.List.sortOn nameKey (Data.List.nub (M.findWithDefault [] n m))
  in
    [ InterfaceFiles.NameHashInfo
        { InterfaceFiles.nhName = n
        , InterfaceFiles.nhSrcHash = M.findWithDefault B.empty n nameSrcHashes
        , InterfaceFiles.nhPubHash = M.findWithDefault B.empty n pubHashes
        , InterfaceFiles.nhImplHash = M.findWithDefault B.empty n implHashes
        , InterfaceFiles.nhPubLocalDeps = localDeps pubLocalDeps n
        , InterfaceFiles.nhImplLocalDeps = localDeps implLocalDeps n
        , InterfaceFiles.nhPubDeps = M.findWithDefault [] n pubExtHashes
        , InterfaceFiles.nhImplDeps = M.findWithDefault [] n implExtHashes
        }
    | n <- namesSorted
    ]

-- | Refresh impl hashes and impl deps for existing name hashes.
refreshImplHashes :: [InterfaceFiles.NameHashInfo]
                  -> M.Map A.Name B.ByteString
                  -> M.Map A.Name [A.Name]
                  -> M.Map A.Name [(A.QName, B.ByteString)]
                  -> [InterfaceFiles.NameHashInfo]
refreshImplHashes nameHashes nameImplHashes implLocalDeps implExtHashes =
  let implHashes = computeHashes nameImplHashes implLocalDeps implExtHashes
      infoMap = M.fromList [ (InterfaceFiles.nhName nh, nh) | nh <- nameHashes ]
      namesSorted = Data.List.sortOn nameKey (M.keys nameImplHashes)
  in
    [ let nh = infoMap M.! n
      in nh { InterfaceFiles.nhImplHash = M.findWithDefault B.empty n implHashes
            , InterfaceFiles.nhImplLocalDeps = Data.List.sortOn nameKey (Data.List.nub (M.findWithDefault [] n implLocalDeps))
            , InterfaceFiles.nhImplDeps = M.findWithDefault [] n implExtHashes
            }
    | n <- namesSorted
    ]

-- | Hash the module public interface entries.
modulePubHashFromIface :: I.NameInfo -> [InterfaceFiles.NameHashInfo] -> B.ByteString
modulePubHashFromIface nmod nameHashes =
  let I.NModule _ iface _ = nmod
      pubHashMap = M.fromList
        [ (InterfaceFiles.nhName nh, InterfaceFiles.nhPubHash nh)
        | nh <- nameHashes
        ]
      pubNamesSorted = Data.List.sortOn nameKey
        [ n | (n, _) <- iface, Names.isPublicName n ]
      pubEntries = [ (nameKey n, M.findWithDefault B.empty n pubHashMap) | n <- pubNamesSorted ]
  in SHA256.hash (Persist.encode pubEntries)

-- | Hash the module impl entries from per-name impl hashes.
moduleImplHashFromNameHashes :: [InterfaceFiles.NameHashInfo] -> B.ByteString
moduleImplHashFromNameHashes infos =
  let items =
        [ (nameKey (InterfaceFiles.nhName nh), InterfaceFiles.nhImplHash nh)
        | nh <- Data.List.sortOn (nameKey . InterfaceFiles.nhName) infos
        ]
  in SHA256.hash (Persist.encode items)
