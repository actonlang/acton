module Acton.Hashing
  ( TopLevelItem(..)
  , topLevelItems
  , nameSrcHashesFromItems
  , implDepsFromItems
  , qnamesInNameInfo
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
import qualified Acton.Names as Names
import Acton.Prim (mPrim)
import qualified Acton.Syntax as A
import qualified InterfaceFiles
import qualified Pretty

import Data.Binary (encode)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
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
topLevelItems (A.Module _ _ suite) = concatMap items suite
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

-- | Collect pretty-printed fragments per top-level name.
nameFragmentsFromItems :: [TopLevelItem] -> M.Map A.Name [String]
nameFragmentsFromItems items =
  foldl' addFrag M.empty items
  where
    addFrag acc item =
      let (n, frag) = case item of
            TLDecl name decl -> (name, Pretty.print decl)
            TLStmt name stmt -> (name, Pretty.print stmt)
      in M.insertWith (flip (++)) n [frag] acc

-- | Hash each name's pretty-printed fragments.
nameSrcHashesFromItems :: [TopLevelItem] -> M.Map A.Name B.ByteString
nameSrcHashesFromItems items =
  M.map (SHA256.hash . B.pack . intercalate "\n") (nameFragmentsFromItems items)

-- | Collect qualified-name dependencies for each item body.
implDepsFromItems :: [TopLevelItem] -> M.Map A.Name [A.QName]
implDepsFromItems items =
  foldl' addDeps M.empty items
  where
    addDeps acc item =
      let (n, deps) = case item of
            TLDecl name decl -> (name, qnamesInDecl decl)
            TLStmt name stmt -> (name, qnamesInStmt stmt)
      in M.insertWith (++) n deps acc

-- | Collect qualified names referenced by a declaration body.
qnamesInDecl :: A.Decl -> [A.QName]
qnamesInDecl d = case d of
  A.Def _ _ _ _ _ _ body _ _ _ -> qnamesInSuite body
  A.Actor _ _ _ _ _ body _ -> qnamesInSuite body
  A.Class _ _ _ _ body _ -> qnamesInSuite body
  A.Protocol _ _ _ _ body _ -> qnamesInSuite body
  A.Extension _ _ _ _ body _ -> qnamesInSuite body

-- | Collect qualified names referenced in a suite.
qnamesInSuite :: A.Suite -> [A.QName]
qnamesInSuite = concatMap qnamesInStmt

-- | Collect qualified names referenced in a statement.
qnamesInStmt :: A.Stmt -> [A.QName]
qnamesInStmt s = case s of
  A.Expr _ e -> qnamesInExpr e
  A.Assign _ ps e -> qnamesInPatterns ps ++ qnamesInExpr e
  A.MutAssign _ t e -> qnamesInExpr t ++ qnamesInExpr e
  A.AugAssign _ t _ e -> qnamesInExpr t ++ qnamesInExpr e
  A.Assert _ e mbe -> qnamesInExpr e ++ maybe [] qnamesInExpr mbe
  A.Pass _ -> []
  A.Delete _ t -> qnamesInExpr t
  A.Return _ mbe -> maybe [] qnamesInExpr mbe
  A.Raise _ e -> qnamesInExpr e
  A.Break _ -> []
  A.Continue _ -> []
  A.If _ bs els -> concatMap qnamesInBranch bs ++ qnamesInSuite els
  A.While _ e b els -> qnamesInExpr e ++ qnamesInSuite b ++ qnamesInSuite els
  A.For _ pat e b els -> qnamesInPattern pat ++ qnamesInExpr e ++ qnamesInSuite b ++ qnamesInSuite els
  A.Try _ b hs els fin -> qnamesInSuite b ++ concatMap qnamesInHandler hs ++ qnamesInSuite els ++ qnamesInSuite fin
  A.With _ items b -> concatMap qnamesInWithItem items ++ qnamesInSuite b
  A.Data _ mp b -> maybe [] qnamesInPattern mp ++ qnamesInSuite b
  A.VarAssign _ ps e -> qnamesInPatterns ps ++ qnamesInExpr e
  A.After _ e e2 -> qnamesInExpr e ++ qnamesInExpr e2
  A.Signature _ _ _ _ -> []
  A.Decl _ ds -> concatMap qnamesInDecl ds

-- | Collect qualified names referenced in an if/elif branch.
qnamesInBranch :: A.Branch -> [A.QName]
qnamesInBranch (A.Branch e ss) = qnamesInExpr e ++ qnamesInSuite ss

-- | Collect qualified names referenced in an exception handler.
qnamesInHandler :: A.Handler -> [A.QName]
qnamesInHandler (A.Handler ex ss) = qnamesInExcept ex ++ qnamesInSuite ss

-- | Collect qualified names referenced in an except clause.
qnamesInExcept :: A.Except -> [A.QName]
qnamesInExcept ex = case ex of
  A.ExceptAll _ -> []
  A.Except _ qn -> [qn]
  A.ExceptAs _ qn _ -> [qn]

-- | Collect qualified names referenced in a with-item.
qnamesInWithItem :: A.WithItem -> [A.QName]
qnamesInWithItem (A.WithItem e mp) = qnamesInExpr e ++ maybe [] qnamesInPattern mp

-- | Collect qualified names referenced in a list of patterns.
qnamesInPatterns :: [A.Pattern] -> [A.QName]
qnamesInPatterns = concatMap qnamesInPattern

-- | Collect qualified names referenced in a pattern.
qnamesInPattern :: A.Pattern -> [A.QName]
qnamesInPattern p = case p of
  A.PWild _ _ -> []
  A.PVar _ _ _ -> []
  A.PParen _ pat -> qnamesInPattern pat
  A.PTuple _ ps ks -> qnamesInPosPat ps ++ qnamesInKwdPat ks
  A.PList _ ps mp -> qnamesInPatterns ps ++ maybe [] qnamesInPattern mp
  A.PData _ n ixs -> A.NoQ n : concatMap qnamesInExpr ixs

-- | Collect qualified names referenced in positional pattern args.
qnamesInPosPat :: A.PosPat -> [A.QName]
qnamesInPosPat ps = case ps of
  A.PosPat p rest -> qnamesInPattern p ++ qnamesInPosPat rest
  A.PosPatStar p -> qnamesInPattern p
  A.PosPatNil -> []

-- | Collect qualified names referenced in keyword pattern args.
qnamesInKwdPat :: A.KwdPat -> [A.QName]
qnamesInKwdPat ks = case ks of
  A.KwdPat _ p rest -> qnamesInPattern p ++ qnamesInKwdPat rest
  A.KwdPatStar p -> qnamesInPattern p
  A.KwdPatNil -> []

-- | Collect qualified names referenced in an expression.
qnamesInExpr :: A.Expr -> [A.QName]
qnamesInExpr e = case e of
  A.Var _ qn -> [qn]
  A.Int{} -> []
  A.Float{} -> []
  A.Imaginary{} -> []
  A.Bool{} -> []
  A.None{} -> []
  A.NotImplemented{} -> []
  A.Ellipsis{} -> []
  A.Strings{} -> []
  A.BStrings{} -> []
  A.Call _ f ps ks -> qnamesInExpr f ++ qnamesInPosArg ps ++ qnamesInKwdArg ks
  A.TApp _ f _ -> qnamesInExpr f
  A.Async _ e1 -> qnamesInExpr e1
  A.Await _ e1 -> qnamesInExpr e1
  A.Index _ e1 ix -> qnamesInExpr e1 ++ qnamesInExpr ix
  A.Slice _ e1 sl -> qnamesInExpr e1 ++ qnamesInSliz sl
  A.Cond _ e1 c e2 -> qnamesInExpr e1 ++ qnamesInExpr c ++ qnamesInExpr e2
  A.IsInstance _ e1 c -> qnamesInExpr e1 ++ [c]
  A.BinOp _ e1 _ e2 -> qnamesInExpr e1 ++ qnamesInExpr e2
  A.CompOp _ e1 ops -> qnamesInExpr e1 ++ qnamesInOpArgs ops
  A.UnOp _ _ e1 -> qnamesInExpr e1
  A.Dot _ e1 _ -> qnamesInExpr e1
  A.Rest _ e1 _ -> qnamesInExpr e1
  A.DotI _ e1 _ -> qnamesInExpr e1
  A.RestI _ e1 _ -> qnamesInExpr e1
  A.Lambda _ _ _ e1 _ -> qnamesInExpr e1
  A.Yield _ me -> maybe [] qnamesInExpr me
  A.YieldFrom _ e1 -> qnamesInExpr e1
  A.Tuple _ ps ks -> qnamesInPosArg ps ++ qnamesInKwdArg ks
  A.List _ es -> qnamesInElems es
  A.ListComp _ e1 c -> qnamesInElem e1 ++ qnamesInComp c
  A.Dict _ es -> qnamesInAssocs es
  A.DictComp _ a c -> qnamesInAssoc a ++ qnamesInComp c
  A.Set _ es -> qnamesInElems es
  A.SetComp _ e1 c -> qnamesInElem e1 ++ qnamesInComp c
  A.Paren _ e1 -> qnamesInExpr e1
  A.Box _ e1 -> qnamesInExpr e1
  A.UnBox _ e1 -> qnamesInExpr e1

-- | Collect qualified names referenced in positional call args.
qnamesInPosArg :: A.PosArg -> [A.QName]
qnamesInPosArg ps = case ps of
  A.PosArg e rest -> qnamesInExpr e ++ qnamesInPosArg rest
  A.PosStar e -> qnamesInExpr e
  A.PosNil -> []

-- | Collect qualified names referenced in keyword call args.
qnamesInKwdArg :: A.KwdArg -> [A.QName]
qnamesInKwdArg ks = case ks of
  A.KwdArg _ e rest -> qnamesInExpr e ++ qnamesInKwdArg rest
  A.KwdStar e -> qnamesInExpr e
  A.KwdNil -> []

-- | Collect qualified names referenced in operator args.
qnamesInOpArgs :: [A.OpArg] -> [A.QName]
qnamesInOpArgs = concatMap qnamesInOpArg

-- | Collect qualified names referenced in a single operator arg.
qnamesInOpArg :: A.OpArg -> [A.QName]
qnamesInOpArg (A.OpArg _ e) = qnamesInExpr e

-- | Collect qualified names referenced in list/set elements.
qnamesInElems :: [A.Elem] -> [A.QName]
qnamesInElems = concatMap qnamesInElem

-- | Collect qualified names referenced in a list/set element.
qnamesInElem :: A.Elem -> [A.QName]
qnamesInElem el = case el of
  A.Elem e -> qnamesInExpr e
  A.Star e -> qnamesInExpr e

-- | Collect qualified names referenced in dict associations.
qnamesInAssocs :: [A.Assoc] -> [A.QName]
qnamesInAssocs = concatMap qnamesInAssoc

-- | Collect qualified names referenced in a dict association.
qnamesInAssoc :: A.Assoc -> [A.QName]
qnamesInAssoc a = case a of
  A.Assoc k v -> qnamesInExpr k ++ qnamesInExpr v
  A.StarStar e -> qnamesInExpr e

-- | Collect qualified names referenced in a slice.
qnamesInSliz :: A.Sliz -> [A.QName]
qnamesInSliz (A.Sliz _ e1 e2 e3) =
  maybe [] qnamesInExpr e1 ++ maybe [] qnamesInExpr e2 ++ maybe [] qnamesInExpr e3

-- | Collect qualified names referenced in a comprehension.
qnamesInComp :: A.Comp -> [A.QName]
qnamesInComp c = case c of
  A.CompFor _ pat e rest -> qnamesInPattern pat ++ qnamesInExpr e ++ qnamesInComp rest
  A.CompIf _ e rest -> qnamesInExpr e ++ qnamesInComp rest
  A.NoComp -> []

-- | Collect qualified names referenced in a type.
qnamesInType :: A.Type -> [A.QName]
qnamesInType t = case t of
  A.TUni{} -> []
  A.TVar{} -> []
  A.TCon _ tc -> qnamesInTCon tc
  A.TFun _ fx p k rt -> qnamesInType fx ++ qnamesInType p ++ qnamesInType k ++ qnamesInType rt
  A.TTuple _ p k -> qnamesInType p ++ qnamesInType k
  A.TOpt _ t1 -> qnamesInType t1
  A.TNone{} -> []
  A.TWild{} -> []
  A.TNil{} -> []
  A.TRow _ _ _ t1 r -> qnamesInType t1 ++ qnamesInType r
  A.TStar _ _ r -> qnamesInType r
  A.TFX{} -> []

-- | Collect qualified names referenced in a type constructor.
qnamesInTCon :: A.TCon -> [A.QName]
qnamesInTCon (A.TC qn ts) = qn : concatMap qnamesInType ts

-- | Collect qualified names referenced in a quantified binding.
qnamesInQBind :: A.QBind -> [A.QName]
qnamesInQBind (A.Quant _ cs) = concatMap qnamesInTCon cs

-- | Collect qualified names referenced in quantified bindings.
qnamesInQBinds :: A.QBinds -> [A.QName]
qnamesInQBinds = concatMap qnamesInQBind

-- | Collect qualified names referenced in a type schema.
qnamesInTSchema :: A.TSchema -> [A.QName]
qnamesInTSchema (A.TSchema _ q t) = qnamesInQBinds q ++ qnamesInType t

-- | Collect qualified names referenced in a witness path.
qnamesInWPath :: A.WPath -> [A.QName]
qnamesInWPath = concatMap step
  where
    step (Left qn) = [qn]
    step (Right qn) = [qn]

-- | Collect qualified names referenced in a witness type constructor.
qnamesInWTCon :: A.WTCon -> [A.QName]
qnamesInWTCon (wpath, pcon) = qnamesInWPath wpath ++ qnamesInTCon pcon

-- | Collect qualified names referenced in a type environment.
qnamesInTEnv :: A.TEnv -> [A.QName]
qnamesInTEnv = concatMap (qnamesInNameInfo . snd)

-- | Collect qualified names referenced in name info.
qnamesInNameInfo :: A.NameInfo -> [A.QName]
qnamesInNameInfo ni = case ni of
  A.NVar t -> qnamesInType t
  A.NSVar t -> qnamesInType t
  A.NDef sc _ _ -> qnamesInTSchema sc
  A.NSig sc _ _ -> qnamesInTSchema sc
  A.NAct q p k te _ -> qnamesInQBinds q ++ qnamesInType p ++ qnamesInType k ++ qnamesInTEnv te
  A.NClass q us te _ -> qnamesInQBinds q ++ concatMap qnamesInWTCon us ++ qnamesInTEnv te
  A.NProto q us te _ -> qnamesInQBinds q ++ concatMap qnamesInWTCon us ++ qnamesInTEnv te
  A.NExt q c us te _ _ -> qnamesInQBinds q ++ qnamesInTCon c ++ concatMap qnamesInWTCon us ++ qnamesInTEnv te
  A.NTVar _ c ps -> qnamesInTCon c ++ concatMap qnamesInTCon ps
  A.NAlias qn -> [qn]
  A.NMAlias _ -> []
  A.NModule te _ -> qnamesInTEnv te
  A.NReserved -> []

-- | Split deps into locals and external qualified names for hashing.
splitDeps :: A.ModName
          -> Env.Env0
          -> Data.Set.Set A.Name
          -> M.Map A.Name [A.QName]
          -> (M.Map A.Name [A.Name], M.Map A.Name [A.QName])
splitDeps mn env localNames depMap =
  let toLocalExt qns = foldl' step (Data.Set.empty, Data.Set.empty) qns
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
            finalHash = SHA256.hash (BL.toStrict (encode (selfHashes M.! n, depHashes)))
        in M.insert n finalHash acc
      CyclicSCC ns ->
        let nsSet = Data.Set.fromList ns
            selfHashesSorted = [ selfHashes M.! n | n <- Data.List.sortOn nameKey ns ]
            outsideDeps = Data.Set.toList $ Data.Set.fromList
              [ d | n <- ns, d <- M.findWithDefault [] n localDeps, Data.Set.notMember d nsSet ]
            externalDeps = Data.Set.toList $ Data.Set.fromList (concat [ M.findWithDefault [] n extDeps | n <- ns ])
            depHashes = depHashList acc outsideDeps externalDeps
            groupHash = SHA256.hash (BL.toStrict (encode (selfHashesSorted, depHashes)))
            insertOne m n =
              let finalHash = SHA256.hash (BL.toStrict (encode (selfHashes M.! n, groupHash, nameKey n)))
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
buildNameHashes :: M.Map A.Name B.ByteString
                -> M.Map A.Name A.NameInfo
                -> M.Map A.Name [A.Name]
                -> M.Map A.Name [(A.QName, B.ByteString)]
                -> M.Map A.Name [A.Name]
                -> M.Map A.Name [(A.QName, B.ByteString)]
                -> M.Map A.Name [(A.QName, B.ByteString)]
                -> [InterfaceFiles.NameHashInfo]
buildNameHashes nameSrcHashes nameInfoMap pubSigLocalDeps pubSigExtHashes implLocalDeps implExtHashes pubExtHashes =
  let hashNameInfo info = SHA256.hash (BL.toStrict $ encode (A.stripDocsNI info))
      selfPubHashes = M.fromList
        [ (n, hashNameInfo (nameInfoMap M.! n))
        | n <- M.keys nameSrcHashes
        ]
      selfImplHashes = nameSrcHashes
      pubHashes = computeHashes selfPubHashes pubSigLocalDeps pubSigExtHashes
      implHashes = computeHashes selfImplHashes implLocalDeps implExtHashes
      namesSorted = Data.List.sortOn nameKey (M.keys nameSrcHashes)
  in
    [ InterfaceFiles.NameHashInfo
        { InterfaceFiles.nhName = n
        , InterfaceFiles.nhSrcHash = nameSrcHashes M.! n
        , InterfaceFiles.nhPubHash = M.findWithDefault B.empty n pubHashes
        , InterfaceFiles.nhImplHash = M.findWithDefault B.empty n implHashes
        , InterfaceFiles.nhPubDeps = M.findWithDefault [] n pubExtHashes
        , InterfaceFiles.nhImplDeps = M.findWithDefault [] n implExtHashes
        }
    | n <- namesSorted
    ]

-- | Refresh impl hashes and impl deps for existing name hashes.
refreshImplHashes :: [InterfaceFiles.NameHashInfo]
                  -> M.Map A.Name [A.Name]
                  -> M.Map A.Name [(A.QName, B.ByteString)]
                  -> [InterfaceFiles.NameHashInfo]
refreshImplHashes nameHashes implLocalDeps implExtHashes =
  let nameSrcHashes = M.fromList
        [ (InterfaceFiles.nhName nh, InterfaceFiles.nhSrcHash nh)
        | nh <- nameHashes
        ]
      implHashes = computeHashes nameSrcHashes implLocalDeps implExtHashes
      infoMap = M.fromList [ (InterfaceFiles.nhName nh, nh) | nh <- nameHashes ]
      namesSorted = Data.List.sortOn nameKey (M.keys nameSrcHashes)
  in
    [ let nh = infoMap M.! n
      in nh { InterfaceFiles.nhImplHash = M.findWithDefault B.empty n implHashes
            , InterfaceFiles.nhImplDeps = M.findWithDefault [] n implExtHashes
            }
    | n <- namesSorted
    ]

-- | Hash the module public interface entries.
modulePubHashFromIface :: A.NameInfo -> [InterfaceFiles.NameHashInfo] -> B.ByteString
modulePubHashFromIface nmod nameHashes =
  let A.NModule iface _ = nmod
      pubHashMap = M.fromList
        [ (InterfaceFiles.nhName nh, InterfaceFiles.nhPubHash nh)
        | nh <- nameHashes
        ]
      pubNamesSorted = Data.List.sortOn nameKey (map fst iface)
      pubEntries = [ (n, M.findWithDefault B.empty n pubHashMap) | n <- pubNamesSorted ]
  in SHA256.hash (BL.toStrict $ encode pubEntries)

-- | Hash the module impl entries from per-name impl hashes.
moduleImplHashFromNameHashes :: [InterfaceFiles.NameHashInfo] -> B.ByteString
moduleImplHashFromNameHashes infos =
  let items =
        [ (InterfaceFiles.nhName nh, InterfaceFiles.nhImplHash nh)
        | nh <- Data.List.sortOn (nameKey . InterfaceFiles.nhName) infos
        ]
  in SHA256.hash (BL.toStrict (encode items))
