{-# LANGUAGE FlexibleInstances, DeriveGeneric #-}
module Acton.Env where

import qualified Control.Exception
import qualified Data.Binary
import GHC.Generics (Generic)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Control.Monad.State.Strict
import Data.Typeable
import System.FilePath.Posix (joinPath,takeDirectory)
import System.Directory (doesFileExist)
import System.Environment (getExecutablePath)
import Control.Monad

import Acton.Syntax
import Acton.Builtin
import Acton.Printer
import Acton.Names
import Utils
import Pretty
import InterfaceFiles
import Prelude hiding ((<>))




mkEnv                       :: (FilePath,FilePath) -> Env -> Module -> IO Env
mkEnv paths env modul       = getImps paths (setDefaultMod m env) imps
  where Module m imps _     = modul


type Schemas                = [(Name, TSchema)]

type TEnv                   = [(Name, NameInfo)]

data Env                    = Env {
                                names      :: TEnv,
                                witnesses  :: [(QName,Witness)],
                                modules    :: [(ModName,TEnv)],
                                defaultmod :: ModName,
                                actorstate :: Maybe Type,
                                indecl     :: Bool }
                            deriving (Show)

data NameInfo               = NVar      Type
                            | NSVar     Type
                            | NDef      TSchema Deco
                            | NSig      TSchema Deco
                            | NAct      QBinds PosRow KwdRow TEnv
                            | NClass    QBinds [WTCon] TEnv
                            | NProto    QBinds [WTCon] TEnv
                            | NExt      QName QBinds [WTCon] TEnv
                            | NTVar     Kind (Maybe TCon)
                            | NAlias    QName
                            | NMAlias   ModName
                            | NModule   TEnv
                            | NReserved
                            | NBlocked
                            deriving (Eq,Show,Read,Generic)

data Witness                = WClass    { binds::QBinds, proto::TCon, wname::QName, wsteps::[Maybe QName] }
                            | WInst     { proto::TCon, wname::QName, wsteps::[Maybe QName] }
                            deriving (Show)

type WTCon                  = ([Maybe QName],TCon)

instance Data.Binary.Binary NameInfo


wmatch env (x,a)      (y,b)         = qmatch env x y && match a b
  where match a@WClass{} b@WClass{} = qmatch env (tcname (proto a)) (tcname (proto b))
        match a@WInst{}  b@WInst{}  = qmatch env (tcname (proto a)) (tcname (proto b))
        match a          b          = False

qmatch env a b                      = match (unalias env a) (unalias env b)
  where match a@QName{}  b@QName{}  = mname a == mname b && noq a == noq b
        match a@NoQ{}    b@NoQ{}    = noq a == noq b
        match a@NoQ{}    b@QName{}  = defaultmod env == mname b && noq a == noq b
        match a@QName{}  b@NoQ{}    = mname a == defaultmod env && noq a == noq b


instance Pretty (QName,Witness) where
    pretty (n, WClass q p w ws) = text "WClass" <+> pretty n <+> nonEmpty brackets commaList q <+> parens (pretty p) <+>
                                  equals <+> pretty (wexpr ws (eCall (eQVar w) []))
    pretty (n, WInst p w ws)    = text "WInst" <+> pretty n <+> parens (pretty p) <+>
                                  equals <+> pretty (wexpr ws (eQVar w))
        
instance Pretty TEnv where
    pretty tenv                 = vcat (map pretty tenv)

instance Pretty Env where
    pretty env                  = vcat (map pretty (names env)) $+$
                                  text "---"  $+$
                                  vcat (map pretty (witnesses env)) $+$
                                  text "."

instance Pretty (Name,NameInfo) where
    pretty (n, NVar t)          = pretty n <+> colon <+> pretty t
    pretty (n, NSVar t)         = text "var" <+> pretty n <+> colon <+> pretty t
    pretty (n, NDef t d)        = prettyDec d $ pretty n <+> colon <+> pretty t
    pretty (n, NSig t d)        = prettyDec d $ pretty n <+> text ":" <+> pretty t
    pretty (n, NAct q p k te)   = text "actor" <+> pretty n <+> nonEmpty brackets commaList q <+>
                                  parens (prettyFunRow p k) <> colon $+$ (nest 4 $ pretty te)
    pretty (n, NClass q us [])  = text "class" <+> pretty n <+> nonEmpty brackets commaList q <+>
                                  nonEmpty parens commaList us
    pretty (n, NClass q us te)  = text "class" <+> pretty n <+> nonEmpty brackets commaList q <+>
                                  nonEmpty parens commaList us <> colon $+$ (nest 4 $ pretty te)
    pretty (n, NProto q us [])  = text "protocol" <+> pretty n <+> nonEmpty brackets commaList q <+>
                                  nonEmpty parens commaList us
    pretty (n, NProto q us te)  = text "protocol" <+> pretty n <+> nonEmpty brackets commaList q <+>
                                  nonEmpty parens commaList us <> colon $+$ (nest 4 $ pretty te)
    pretty (w, NExt n [] ps te) = pretty w  <+> colon <+> text "extension" <+> pretty n <+> parens (commaList ps) <>
                                  colon $+$ (nest 4 $ pretty te)
    pretty (w, NExt n q ps te)  = pretty w  <+> colon <+> pretty q <+> text "=>" <+> text "extension" <+> pretty n <> 
                                  brackets (commaList $ tybound q) <+> parens (commaList ps) <>
                                  colon $+$ (nest 4 $ pretty te)
    pretty (n, NTVar k mba)     = pretty n <> maybe empty (parens . pretty) mba
    pretty (n, NAlias qn)       = text "alias" <+> pretty n <+> equals <+> pretty qn
    pretty (n, NMAlias m)       = text "module" <+> pretty n <+> equals <+> pretty m
    pretty (n, NModule te)      = text "module" <+> pretty n <> colon $+$ nest 4 (pretty te)
    pretty (n, NReserved)       = pretty n <+> text "(reserved)"
    pretty (n, NBlocked)        = pretty n <+> text "(blocked)"

instance Pretty WTCon where
--    pretty (ws,u)               = pretty u
    pretty (ws,u)               = dotCat pretty (catMaybes ws) <+> colon <+> pretty u

instance Subst Env where
    msubst env                  = do ne <- msubst (names env)
                                     we <- msubst (witnesses env)
                                     as <- msubst (actorstate env)
                                     return env{ names = ne, witnesses = we, actorstate = as }
    tyfree env                  = tyfree (names env) ++ tyfree (witnesses env) ++ tyfree (actorstate env)

instance Subst NameInfo where
    msubst (NVar t)             = NVar <$> msubst t
    msubst (NSVar t)            = NSVar <$> msubst t
    msubst (NDef t d)           = NDef <$> msubst t <*> return d
    msubst (NSig t d)           = NSig <$> msubst t <*> return d
    msubst (NAct q p k te)      = NAct <$> msubst q <*> msubst p <*> msubst k <*> msubst te
    msubst (NClass q us te)     = NClass <$> msubst q <*> msubst us <*> msubst te
    msubst (NProto q us te)     = NProto <$> msubst q <*> msubst us <*> msubst te
    msubst (NExt n q ps te)     = NExt n <$> msubst q <*> msubst ps <*> msubst te
    msubst (NTVar k mba)        = NTVar k <$> msubst mba
    msubst (NAlias qn)          = NAlias <$> return qn
    msubst (NMAlias m)          = NMAlias <$> return m
    msubst (NModule te)         = NModule <$> return te     -- actually msubst te, but te has no free variables (top-level)
    msubst NReserved            = return NReserved
    msubst NBlocked             = return NBlocked

    tyfree (NVar t)             = tyfree t
    tyfree (NSVar t)            = tyfree t
    tyfree (NDef t d)           = tyfree t
    tyfree (NSig t d)           = tyfree t
    tyfree (NAct q p k te)      = (tyfree q ++ tyfree p ++ tyfree k ++ tyfree te) \\ (tvSelf : tybound q)
    tyfree (NClass q us te)     = (tyfree q ++ tyfree us ++ tyfree te) \\ (tvSelf : tybound q)
    tyfree (NProto q us te)     = (tyfree q ++ tyfree us ++ tyfree te) \\ (tvSelf : tybound q)
    tyfree (NExt n q ps te)     = (tyfree q ++ tyfree ps ++ tyfree te) \\ (tvSelf : tybound q)
    tyfree (NTVar k mba)        = tyfree mba
    tyfree (NAlias qn)          = []
    tyfree (NMAlias qn)         = []
    tyfree (NModule te)         = []        -- actually tyfree te, but a module has no free variables on the top level
    tyfree NReserved            = []
    tyfree NBlocked             = []

instance Subst (QName,Witness) where
    msubst (n, w@WClass{})      = return (n, w)         -- A WClass (i.e., an extension) can't have any free type variables
    msubst (n, w@WInst{})       = do p <- msubst (proto w)
                                     return (n, w{ proto = p })
    
    tyfree (n, w@WClass{})      = []
    tyfree (n, w@WInst{})       = filter univar $ tyfree (proto w)
    

instance Subst WTCon where
    msubst (w,u)                = (,) <$> return w <*> msubst u
    
    tyfree (w,u)                = tyfree u


-------------------------------------------------------------------------------------------------------------------

class Unalias a where
    unalias                         :: Env -> a -> a
    unalias env                     = id

instance (Unalias a) => Unalias [a] where
    unalias env                     = map (unalias env)

instance (Unalias a) => Unalias (Maybe a) where
    unalias env                     = fmap (unalias env)

instance Unalias ModName where
    unalias env m | m == mBuiltin   = m
    unalias env (ModName ns0)       = norm (names env) [] ns0
      where
        norm te pre []              = ModName (reverse pre)
        norm te pre (n:ns)          = case lookup n te of
                                        Just (NModule te') -> norm te' (n:pre) ns
                                        Just (NMAlias m) -> m
                                        _ -> noModule (ModName ns0)

instance Unalias QName where
    unalias env (QName m n)         = case lookup m' (modules env) of
                                        Just te -> case lookup n te of
                                                      Just (NAlias qn) -> qn
                                                      Just _ -> QName m' n
                                                      _ -> noItem m n
                                        Nothing | inBuiltin env -> QName m n
      where m'                      = unalias env m
    unalias env (NoQ n)             = case lookup n (names env) of
                                        Just (NAlias qn) -> qn
                                        Just _ -> QName (defaultmod env) n
                                        _ -> NoQ n  -- trace ("#unalias") $ nameNotFound n
                                    
instance Unalias TSchema where
    unalias env (TSchema l q t)     = TSchema l (unalias env q) (unalias env t)

instance Unalias TCon where
    unalias env (TC qn ts)          = TC (unalias env qn) (unalias env ts)

instance Unalias QBind where
    unalias env (Quant tv cs)       = Quant tv (unalias env cs)

instance Unalias Type where
    unalias env (TCon l c)          = TCon l (unalias env c)
    unalias env (TFun l e p r t)    = TFun l (unalias env e) (unalias env p) (unalias env r) (unalias env t)
    unalias env (TTuple l p k)      = TTuple l (unalias env p) (unalias env k)
    unalias env (TOpt l t)          = TOpt l (unalias env t)
    unalias env (TRow l k n t r)    = TRow l k n (unalias env t) (unalias env r)
    unalias env t                   = t

instance Unalias NameInfo where
    unalias env (NVar t)            = NVar (unalias env t)
    unalias env (NSVar t)           = NSVar (unalias env t)
    unalias env (NDef t d)          = NDef (unalias env t) d
    unalias env (NSig t d)          = NSig (unalias env t) d
    unalias env (NAct q p k te)     = NAct (unalias env q) (unalias env p) (unalias env k) (unalias env te)
    unalias env (NClass q us te)    = NClass (unalias env q) (unalias env us) (unalias env te)
    unalias env (NProto q us te)    = NProto (unalias env q) (unalias env us) (unalias env te)
    unalias env (NExt n q ps te)    = NExt (unalias env n) (unalias env q) (unalias env ps) (unalias env te)
    unalias env (NTVar k mba)       = NTVar k (unalias env mba)
    unalias env (NAlias qn)         = NAlias (unalias env qn)
    unalias env (NModule te)        = NModule (unalias env te)
    unalias env NReserved           = NReserved
    unalias env NBlocked            = NBlocked

instance Unalias WTCon where
    unalias env (w,u)               = (unalias env w, unalias env u)

instance Unalias (Name,NameInfo) where
    unalias env (n,i)               = (n, unalias env i)
    
-- TEnv filters --------------------------------------------------------------------------------------------------------

nSigs                       :: TEnv -> TEnv
nSigs te                    = [ (n,i) | (n, i@(NSig sc dec)) <- te ]

splitSigs                   :: TEnv -> (TEnv, TEnv)
splitSigs te                = partition isSig te
  where isSig (_, NSig{})   = True
        isSig _             = False

nTerms                      :: TEnv -> TEnv
nTerms te                   = [ (n,i) | (n,i) <- te, isTerm i ]
  where isTerm NDef{}       = True
        isTerm NVar{}       = True
        isTerm _            = False

noDefs                      :: TEnv -> TEnv
noDefs te                   = [ (n,i) | (n,i) <- te, not $ isDef i ]
  where isDef NDef{}        = True
        isDef NAct{}        = True
        isDef _             = False

sigTerms                    :: TEnv -> (TEnv, TEnv)
sigTerms te                 = (nSigs te, nTerms te)

propSigs                    :: TEnv -> TEnv
propSigs te                 = [ (n,i) | (n, i@(NSig sc dec)) <- te, isProp dec sc ]

isProp                      :: Deco -> TSchema -> Bool
isProp Property _           = True
isProp NoDec sc             = case sctype sc of TFun{} -> False; _ -> True
isProp _ _                  = False

nSchemas                    :: TEnv -> Schemas
nSchemas []                 = []
nSchemas ((n,NVar t):te)    = (n, monotype t) : nSchemas te
nSchemas ((n,NDef sc d):te) = (n, sc) : nSchemas te
nSchemas (_:te)             = nSchemas te

parentTEnv                  :: Env -> [WTCon] -> TEnv
parentTEnv env us           = concatMap (snd . findCon env . snd) us

splitTEnv                   :: [Name] -> TEnv -> (TEnv, TEnv)
splitTEnv vs te             = partition ((`elem` vs) . fst) te

unSig                       :: TEnv -> TEnv
unSig te                    = map f te
  where f (n, NSig (TSchema _ [] t) Property)   = (n, NVar t)
        f (n, NSig sc@(TSchema _ _ TFun{}) dec) = (n, NDef sc dec)
        f (n, NSig (TSchema _ _ t) _)           = (n, NVar t)
        f (n, i)                                = (n, i)

-- Env construction and modification -------------------------------------------------------------------------------------------

initEnv                    :: Bool -> IO Env
initEnv nobuiltin           = if nobuiltin
                                then return $ Env{names = [], witnesses = [], modules = [], defaultmod = mBuiltin, actorstate = Nothing, indecl = False}
                                else do path <- getExecutablePath
                                        envBuiltin <- InterfaceFiles.readFile (joinPath [takeDirectory path,"__builtin__.ty"])
                                        let env0    = Env{names = [(nBuiltin,NModule envBuiltin)],
                                                          witnesses = [],
                                                          modules = [(mBuiltin,envBuiltin)],
                                                          defaultmod = mBuiltin,
                                                          actorstate = Nothing,
                                                          indecl = False}
                                            env     = importAll mBuiltin envBuiltin $ importWits mBuiltin envBuiltin $ env0
                                        return env
                                        
setDefaultMod               :: ModName -> Env -> Env
setDefaultMod m env         = env{ defaultmod = m }

setActorFX                  :: Type -> Env -> Env
setActorFX st env           = env{ actorstate = Just st }

setInDecl                   :: Bool -> Env -> Env
setInDecl f env             = env{ indecl = f }

addWit                      :: Env -> (QName,Witness) -> Env
addWit env cwit
  | exists                  = env
  | otherwise               = env{ witnesses = cwit : witnesses env }
  where exists              = any (wmatch env cwit) (witnesses env)

addMod                      :: ModName -> TEnv -> Env -> Env
addMod m te env             = env{ modules = (m,te) : modules env }

reserve                     :: [Name] -> Env -> Env
reserve xs env              = env{ names = [ (x, NReserved) | x <- nub xs ] ++ names env }

block                       :: [Name] -> Env -> Env
block xs env                = env{ names = [ (x, NBlocked) | x <- nub xs ] ++ names env }

define                      :: TEnv -> Env -> Env
define te env               = foldl addWit env1 ws
  where env1                = env{ names = reverse te ++ exclude (dom te) (names env) }
        ws                  = [ (c, WClass q p (NoQ w) ws) | (w, NExt c q ps te') <- te, (ws,p) <- ps ]

defineTVars                 :: QBinds -> Env -> Env
defineTVars q env           = foldr f env q
  where f (Quant tv us) env = foldl addWit env{ names = (tvname tv, NTVar (tvkind tv) mbc) : names env } wits
          where (mbc,ps)    = case mro2 env us of ([],_) -> (Nothing, us); _ -> (Just $ head us, tail us)   -- Just check that the mro exists, don't store it
                wits        = [ (NoQ (tvname tv), WInst p (NoQ $ tvarWit tv p0) wchain) | p0 <- ps, (wchain,p) <- findAncestry env p0 ]

defineSelf                  :: QName -> QBinds -> Env -> Env
defineSelf qn q env         = defineTVars [Quant tvSelf [tc]] env
  where tc                  = TC qn [ tVar tv | Quant tv _ <- q ]

defineSelfOpaque            :: Env -> Env
defineSelfOpaque env        = defineTVars [Quant tvSelf []] env


defineMod                   :: ModName -> TEnv -> Env -> Env
defineMod m te env          = define [(n, defmod ns $ te1)] env
  where ModName (n:ns)      = m
        te1                 = case lookup n (names env) of Just (NModule te1) -> te1; _ -> []
        defmod [] te1       = NModule $ te
        defmod (n:ns) te1   = NModule $ (n, defmod ns te2) : exclude [n] te1
          where te2         = case lookup n te1 of Just (NModule te2) -> te2; _ -> []


-- General Env queries -----------------------------------------------------------------------------------------------------------

inBuiltin                   :: Env -> Bool
inBuiltin env               = null $ modules env

actorFX                     :: Env -> SrcLoc -> Type
actorFX env l               = case actorstate env of
                                Just st -> fxAct st
                                Nothing -> err l "Actor scope expected"

inDecl                      :: Env -> Bool
inDecl env                  = indecl env

stateScope                  :: Env -> [Name]
stateScope env              = [ z | (z, NSVar _) <- names env ]

tvarScope                   :: Env -> [TVar]
tvarScope env               = [ TV k n | (n, NTVar k _) <- names env ]

-- Name queries -------------------------------------------------------------------------------------------------------------------

findQName                   :: QName -> Env -> NameInfo 
findQName (QName m n) env   = case maybeFindMod (unalias env m) env of
                                Just te -> case lookup n te of
                                    Just (NAlias qn) -> findQName qn env
                                    Just i -> i
                                    _ -> noItem m n
                                Nothing | inBuiltin env -> findQName (NoQ n) env
                                        | otherwise -> noModule m
findQName (NoQ n) env       = case lookup n (names env) of
                                Just (NAlias qn) -> findQName qn env
                                Just info -> info
                                Nothing -> nameNotFound n

findName n env              = findQName (NoQ n) env

maybeFindMod                :: ModName -> Env -> Maybe TEnv
maybeFindMod (ModName ns) env = f ns (names env)
  where f [] te             = Just te
        f (n:ns) te         = case lookup n te of
                                Just (NModule te') -> f ns te'
                                Just (NMAlias m) -> maybeFindMod m env
                                _ -> Nothing

isMod                       :: Env -> [Name] -> Bool
isMod env ns                = maybe False (const True) (maybeFindMod (ModName ns) env)


tconKind                    :: QName -> Env -> Kind
tconKind n env              = case findQName n env of
                                NAct q _ _ _ -> kind KType q
                                NClass q _ _ -> kind KType q
                                NProto q _ _ -> kind KProto q
                                _            -> notClassOrProto n
  where kind k []           = k
        kind k q            = KFun [ tvkind v | Quant v _ <- q ] k

isActor                     :: QName -> Env -> Bool
isActor n env               = case findQName n env of
                                NAct q p k te -> True
                                _ -> False

isClass                     :: QName -> Env -> Bool
isClass n env               = case findQName n env of
                                NClass q us te -> True
                                _ -> False

isProto                     :: QName -> Env -> Bool
isProto n env               = case findQName n env of
                                NProto q us te -> True
                                _ -> False

findWitness                 :: Env -> QName -> (QName->Bool) -> Maybe Witness
findWitness env cn f        = listToMaybe [ w | (c,w) <- witnesses env, qmatch env c cn, f $ tcname $ proto w ]

hasWitness                  :: Env -> QName -> QName -> Bool
hasWitness env cn pn        =  not $ null $ findWitness env cn (qmatch env pn)

implProto                   :: Env -> TCon -> QName -> Bool
implProto env p             = qmatch env (tcname p)


-- TCon queries ------------------------------------------------------------------------------------------------------------------

findAttr                    :: Env -> TCon -> Name -> Maybe (Expr->Expr,TSchema,Deco)
findAttr env tc n           = findIn [ (w,te') | (w,u) <- findAncestry env tc, let (_,te') = findCon env u ]
  where findIn ((w,te):tes) = case lookup n te of
                                Just (NSig sc d) -> Just (wexpr w, sc, d)
                                Nothing          -> findIn tes
        findIn []           = Nothing

findAncestry                :: Env -> TCon -> [WTCon]
findAncestry env tc         = ([Nothing],tc) : fst (findCon env tc)

findAncestor                :: Env -> TCon -> QName -> Maybe (Expr->Expr,TCon)
findAncestor env p qn       = listToMaybe [ (wexpr ws, p') | (ws,p') <- findAncestry env p, qmatch env (tcname p') qn ]

hasAncestor                 :: Env -> TCon -> QName -> Bool
hasAncestor env c qn        = maybe False (const True) $ findAncestor env c qn

commonAncestors             :: Env -> TCon -> TCon -> [TCon]
commonAncestors env c1 c2   = filter (\c -> any (qmatch env (tcname c)) ns) $ map snd (findAncestry env c1)
  where ns                  = map (tcname . snd) (findAncestry env c2)

findCon                     :: Env -> TCon -> ([WTCon],TEnv)
findCon env (TC n ts)
  | map tVar tvs == ts      = (us, te)
  | otherwise               = (subst s us, subst s te)
  where (q,us,te)           = findConName n env
        tvs                 = tybound q
        s                   = tvs `zip` ts
      
findConName n env           = case findQName n env of
                                NAct q p k te  -> (q,[],te)
                                NClass q us te -> (q,us,te)
                                NProto q us te -> (q,us,te)
                                NExt n q us te -> (q,us,te)
                                i -> err1 n ("findConName: Class or protocol name expected, got " ++ show i ++ " --- ")

conAttrs                    :: Env -> QName -> [Name]
conAttrs env qn             = dom te
  where (_,_,te)            = findConName qn env

hasAttr                     :: Env -> Name -> QName -> Bool
hasAttr env n qn            = n `elem` conAttrs env qn

findClassByProps            :: Env -> [Name] -> [Type]
findClassByProps env ns     = [ tCon $ TC (NoQ c) (map (const tWild) q) | (c, NClass q us te) <- unfold env $ names env, hasAll te us ]
  where hasAll te us        = not (null ns1) && cObject `elem` map snd us && all (`elem` inherited) ns2
          where (ns1,ns2)   = partition (`elem` dom (propSigs te)) ns
                inherited   = concat [ dom (propSigs te) | (w,u) <- us, let (_,_,te) = findConName (tcname u) env ]

findClassByAttrs            :: Env -> [Name] -> [Type]
findClassByAttrs env ns     = [ tCon $ TC (NoQ c) (map (const tWild) q) | (c, NClass q us te) <- unfold env $ names env, hasAll te us ]
  where hasAll te us        = not (null ns1) && all (`elem` inherited) ns2
          where (ns1,ns2)   = partition (`elem` dom te) ns
                inherited   = concat [ dom te | (w,u) <- us, let (_,_,te) = findConName (tcname u) env ]

findActorByAttrs            :: Env -> [Name] -> [Type]
findActorByAttrs env ns     = [ tCon $ TC (NoQ c) (map (const tWild) q) | (c, NAct q p k te) <- unfold env $ names env, hasAll te ]
  where hasAll te           = all (`elem` dom te) ns

findProtoByAttrs            :: Env -> [Name] -> [TCon]
findProtoByAttrs env ns     = [ TC (NoQ p) (map (const tWild) q) | (p, NProto q us te) <- unfold env $ names env, hasAll te us ]
  where hasAll te us        = not (null ns1) && all (`elem` inherited) ns2
          where (ns1,ns2)   = partition (`elem` dom te) ns
                inherited   = concat [ dom te | (w,u) <- us, let (_,_,te) = findConName (tcname u) env ]


unfold env te               = map exp te
  where exp (n, NAlias qn)  = (n, findQName qn env)
        exp (n, i)          = (n, i)


-- TVar queries ------------------------------------------------------------------------------------------------------------------

findTVBound                 :: Env -> TVar -> Maybe TCon
findTVBound env tv          = case findName (tvname tv) env of
                                NTVar _ mba -> mba
                                _ -> err1 tv "Unknown type variable"

findTVAttr                  :: Env -> TVar -> Name -> Maybe (Expr->Expr,TSchema,Deco)
findTVAttr env tv n         = case findTVBound env tv of
                                Just a -> findAttr env a n
                                Nothing -> Nothing

tvarWit                     :: TVar -> TCon -> Name
tvarWit tv p                = Derived (tvname tv) (nstr $ deriveQ $ tcname p)


-- Well-formed tycon applications -------------------------------------------------------------------------------------------------

wellformed env x            = wf env x

class WellFormed a where
    wf                      :: Env -> a -> Constraints

instance (WellFormed a) => WellFormed (Maybe a) where
    wf env                  = maybe [] (wf env)

instance (WellFormed a) => WellFormed [a] where
    wf env                  = concatMap (wf env)

instance (WellFormed a, WellFormed b) => WellFormed (a,b) where
    wf env (a,b)            = wf env a ++ wf env b

instance WellFormed TCon where
    wf env (TC n ts)        = wf env ts ++ subst s [ constr u (tVar v) | Quant v us <- q, u <- us ]
      where q               = case findQName n env of
                                NAct q p k te  -> q
                                NClass q us te -> q
                                NProto q us te -> q
                                i -> err1 n ("wf: Class or protocol name expected, got " ++ show i)
            s               = tybound q `zip` ts
            constr u t      = if isProto (tcname u) env then Impl (name "_") t u else Cast t (tCon u)
            
instance WellFormed Type where
    wf env (TCon _ tc)      = wf env tc
    wf env (TFun _ x p k t) = wf env x ++ wf env p ++ wf env p ++ wf env k ++ wf env t
    wf env (TTuple _ p k)   = wf env p ++ wf env k
    wf env (TOpt _ t)       = wf env t
    wf env (TRow _ _ _ t r) = wf env t ++ wf env r
    wf env _                = []


instance WellFormed QBind where
    wf env (Quant v us)     = wf env us


-- Method resolution order ------------------------------------------------------------------------------------------------------

mro2                                    :: Env -> [TCon] -> ([WTCon],[WTCon])
mro2 env []                             = ([], [])
mro2 env (u:us)
  | isActor (tcname u) env              = err1 u "Actor subclassing not allowed"
  | isProto (tcname u) env              = ([], mro env (u:us))
  | otherwise                           = (mro env [u], mro env us)

mro1 env us                             = mro env us

mro                                     :: Env -> [TCon] -> [WTCon]
mro env us                              = merge [] $ map lin us' ++ [us']
  where
    us'                                 = case us of [] -> []; u:us -> ([Nothing],u) : [ ([Just (tcname u)],u) | u <- us ]
    
    lin                                 :: WTCon -> [WTCon]
    lin (w,u)                           = (w,u) : [ (w++w',u') | (w',u') <- us' ]
      where (us',_)                     = findCon env u

    merge                               :: [WTCon] -> [[WTCon]] -> [WTCon]
    merge out lists
      | null heads                      = reverse out
      | h:_ <- good                     = merge (h:out) [ if equal hd h then tl else hd:tl | (hd,tl) <- zip heads tails ]
      | otherwise                       = err2 (map snd heads) "Inconsistent resolution order for"
      where (heads,tails)               = unzip [ (hd,tl) | hd:tl <- lists ]
            good                        = [ h | h <- heads, all (absent h) tails]

    equal                               :: WTCon -> WTCon -> Bool
    equal (w1,u1) (w2,u2)
      | headmatch                       = tcargs u1 == tcargs u2 || err2 [u1,u2] "Inconsistent protocol instantiations"
      | otherwise                       = False
      where headmatch                   = qmatch env (tcname u1) (tcname u2)

    absent                              :: WTCon -> [WTCon] -> Bool
    absent (w,h) us                     = tcname h `notElem` map (tcname . snd) us



-- Instantiation -------------------------------------------------------------------------------------------------------------------

instantiate                 :: Env -> TSchema -> TypeM (Constraints, Type)
instantiate env (TSchema _ q t)
                            = do (cs, tvs) <- instQBinds env q
                                 let s = tybound q `zip` tvs
                                 return (cs, subst s t)

instQBinds                  :: Env -> QBinds -> TypeM (Constraints, [Type])
instQBinds env q            = do ts <- newTVars [ tvkind v | Quant v _ <- q ]
                                 cs <- instQuals env q ts
                                 return (cs, ts)

instWitness                 :: Env -> [Type] -> Witness -> TypeM (Constraints,TCon,Expr)        -- witnesses of cs already applied in e!
instWitness env ts wit      = case wit of
                                 WClass q p w ws -> do
                                    cs <- instQuals env q ts
                                    return (cs, subst (tybound q `zip` ts) p, wexpr ws (eCall (eQVar w) $ wvars cs))
                                 WInst p w ws ->
                                    return ([], p, wexpr ws (eQVar w))

instQuals                   :: Env -> QBinds -> [Type] -> TypeM Constraints
instQuals env q ts          = do let s = tybound q `zip` ts
                                 sequence [ constr (tVar v) u | Quant v us <- subst s q, u <- us ]
  where constr t u@(TC n _)
          | isProto n env   = do w <- newWitness; return $ Impl w t u
          | otherwise       = return $ Cast t (tCon u)

wexpr                       :: [Maybe QName] -> Expr -> Expr
wexpr []                    = id
wexpr (Nothing : w)         = wexpr w
wexpr (Just n : w)          = wexpr w . (\e -> eDot e (noq n))

wvars                       :: Constraints -> [Expr]
wvars cs                    = [ eVar v | Impl v _ _ <- cs ]


-- Import handling (local definitions only) ----------------------------------------------

getImps                         :: (FilePath,FilePath) -> Env -> [Import] -> IO Env
getImps ps env []               = return env
getImps ps env (i:is)           = do env' <- impModule ps env i
                                     getImps ps env' is


impModule                       :: (FilePath,FilePath) -> Env -> Import -> IO Env
impModule ps env (Import _ ms)  = imp env ms
  where imp env []              = return env
        imp env (ModuleItem m as : is)
                                = do (env1,te) <- doImp ps env m
                                     let env2 = maybe (defineMod m te env1) (\n->define [(n, NMAlias m)] env1) as
                                     imp (importWits m te env2) is
impModule ps env (FromImport _ (ModRef (0,Just m)) items)
                                = do (env1,te) <- doImp ps env m
                                     return $ importSome items m te $ importWits m te $ env1
impModule ps env (FromImportAll _ (ModRef (0,Just m)))
                                = do (env1,te) <- doImp ps env m
                                     return $ importAll m te $ importWits m te $ env1
impModule _ _ i                 = illegalImport (loc i)


doImp (p,sysp) env m            = case lookup m (modules env) of
                                    Just te -> return (env, te)
                                    Nothing -> do
                                        found <- doesFileExist fpath
                                        if found
                                         then do te <- InterfaceFiles.readFile fpath
                                                 return (defineMod m te (addMod m te env), te)
                                         else do found <- doesFileExist fpath2
                                                 unless found (fileNotFound m)
                                                 te <- InterfaceFiles.readFile fpath2
                                                 return (defineMod m te (addMod m te env), te)
  where fpath                   = joinPath (p : mpath m) ++ ".ty"
        fpath2                  = joinPath (sysp : mpath m) ++ ".ty"
        mpath (ModName ns)      = map nstr ns


importSome                  :: [ImportItem] -> ModName -> TEnv -> Env -> Env
importSome items m te env   = define (map pick items) env
  where 
    te1                     = impNames m te
    pick (ImportItem n mbn) = case lookup n te1 of
                                    Just i  -> (maybe n id mbn, i) 
                                    Nothing -> noItem m n

importAll                   :: ModName -> TEnv -> Env -> Env
importAll m te env          = define (impNames m te) env

impNames                    :: ModName -> TEnv -> TEnv
impNames m te               = mapMaybe imp te
  where 
    imp (n, NAct _ _ _ _)   = Just (n, NAlias (QName m n))
    imp (n, NClass _ _ _)   = Just (n, NAlias (QName m n))
    imp (n, NProto _ _ _)   = Just (n, NAlias (QName m n))
    imp (n, NExt _ _ _ _)   = Nothing
    imp (n, NAlias _)       = Just (n, NAlias (QName m n))
    imp (n, NVar t)         = Just (n, NAlias (QName m n))
    imp (n, NDef t d)       = Just (n, NAlias (QName m n))
    imp _                   = Nothing                               -- cannot happen

importWits                  :: ModName -> TEnv -> Env -> Env
importWits m te env         = foldl addWit env ws
  where ws                  = [ (c, WClass q p (QName m n) ws) | (n, NExt c q ps te') <- te, (ws,p) <- ps ]


-- Type inference monad ------------------------------------------------------------------

type TVarMap                            = Map TVar Type

data TypeState                          = TypeState {
                                                nextint         :: Int,
                                                effectstack     :: [(TFX,Type)],
                                                deferred        :: Constraints,
                                                currsubst       :: TVarMap
                                          }

initTypeState s                         = TypeState { nextint = 1, effectstack = [], deferred = [], currsubst = s }

type TypeM a                            = State TypeState a

runTypeM                                :: TypeM a -> a
runTypeM m                              = evalState m (initTypeState Map.empty)
{-
type TypeM a                            = ExceptT TypeError (State TypeState) a

runTypeM                                :: TypeM a -> a
runTypeM m                              = case evalState (runExceptT m) (initTypeState Map.empty) of
                                            Right x -> x
                                            Left err -> internal ("Unhandled TypeM error: " ++ show err)
-}
newUnique                               :: TypeM Int
newUnique                               = state $ \st -> (nextint st, st{ nextint = nextint st + 1 })

pushFX                                  :: TFX -> Type -> TypeM ()
pushFX fx ret                           = state $ \st -> ((), st{ effectstack = (fx,ret) : effectstack st })

currFX                                  :: TypeM TFX
currFX                                  = state $ \st -> (fst $ head $ effectstack st, st)

currRet                                 :: TypeM Type
currRet                                 = state $ \st -> (snd $ head $ effectstack st, st)

popFX                                   :: TypeM ()
popFX                                   = state $ \st -> ((), st{ effectstack = tail (effectstack st) })

defer                                   :: Constraints -> TypeM ()
defer cs                                = state $ \st -> ((), st{ deferred = cs ++ deferred st })

collectDeferred                         :: TypeM Constraints
collectDeferred                         = state $ \st -> (deferred st, st{ deferred = [] })

substitute                              :: TVar -> Type -> TypeM ()
substitute tv t                         = trace ("  #substitute " ++ prstr tv ++ " ~ " ++ prstr t) $ 
                                          state $ \st -> ((), st{ currsubst = Map.insert tv t (currsubst st)})

getSubstitution                         :: TypeM (Map TVar Type)
getSubstitution                         = state $ \st -> (currsubst st, st)

setSubstitution                         :: Map TVar Type -> TypeM ()
setSubstitution s                       = state $ \st -> ((), st{ currsubst = s })


univar (TV _ (Internal Typevar _ _))    = True
univar (TV _ (Internal Wildvar _ _))    = True
univar _                                = False


pNames                                  = map (Internal TypesPass "p") [0..]
kNames                                  = map (Internal TypesPass "k") [0..]
xNames                                  = map (Internal TypesPass "x") [0..]

newWitness                              = Internal Witness "" <$> newUnique

newTVarOfKind k                         = TVar NoLoc <$> TV k <$> (Internal Typevar (str k) <$> newUnique)
  where str KType                       = ""
        str KFX                         = "x"
        str PRow                        = "p"
        str KRow                        = "k"
        str _                           = ""

newTVars ks                             = mapM newTVarOfKind ks

newTVar                                 = newTVarOfKind KType

subst                                   :: Subst a => Substitution -> a -> a
subst s x0
  | null clash                          = evalState (msubst x0) (initTypeState $ Map.fromList s)
  | otherwise                           = x2
  where x1                              = evalState (msubst x0) (initTypeState $ Map.fromList (s1 ++ clash `zip` map tVar tmp))
        x2                              = evalState (msubst x1) (initTypeState $ Map.fromList (s1 ++ tmp `zip` rng s0))
        (s0,s1)                         = partition ((`elem` clash) . fst) s
        clash                           = dom s `intersect` tyfree (rng s)
        used                            = dom s ++ tyfree (rng s)                             
        tmp                             = take (length clash) $ map (TV KWild . Internal TypesPass "") [1 ..] \\ used

erase x                                 = subst s x
  where s                               = [ (tv, tWild) | tv <- nub (tyfree x) ]

monotypeOf (TSchema _ [] t)             = t
monotypeOf sc                           = err1 sc "Monomorphic type expected"


class Subst t where
    msubst                          :: t -> TypeM t
    tyfree                          :: t -> [TVar]
    tybound                         :: t -> [TVar]
    tybound _                       = []

instance Subst a => Subst (Name,a) where
    msubst (n, t)                   = (,) <$> return n <*> msubst t
    tyfree (n, t)                   = tyfree t
    tybound (n, t)                  = tybound t

instance Subst a => Subst [a] where
    msubst                          = mapM msubst
    tyfree                          = concat . map tyfree
    tybound                         = concat . map tybound

instance Subst a => Subst (Maybe a) where
    msubst                          = maybe (return Nothing) (\x -> Just <$> msubst x)
    tyfree                          = maybe [] tyfree
    tybound                         = maybe [] tybound

instance Subst Constraint where
    msubst (Cast t1 t2)             = Cast <$> msubst t1 <*> msubst t2
    msubst (Sub w t1 t2)            = Sub w <$> msubst t1 <*> msubst t2
    msubst (Impl w t p)             = Impl w <$> msubst t <*> msubst p
    msubst (Sel w t1 n t2)          = Sel w <$> msubst t1 <*> return n <*> msubst t2
    msubst (Mut t1 n t2)            = Mut <$> msubst t1 <*> return n <*> msubst t2

    tyfree (Cast t1 t2)             = tyfree t1 ++ tyfree t2
    tyfree (Sub w t1 t2)            = tyfree t1 ++ tyfree t2
    tyfree (Impl w t p)             = tyfree t ++ tyfree p
    tyfree (Sel w t1 n t2)          = tyfree t1 ++ tyfree t2
    tyfree (Mut t1 n t2)            = tyfree t1 ++ tyfree t2

split_fixed fvs cs                  = partition fixed cs
  where fixed c                     = null (tyfree c \\ fvs)

split_noqual cs                     = partition (not . qual) cs
  where qual (Cast TVar{} TCon{})   = True
        qual Impl{}                 = True
        qual _                      = False

split_ambig vs cs
  | null vs'                        = (amb_cs, safe_cs)
  | otherwise                       = split_ambig (vs'++vs) cs
  where (amb_cs,safe_cs)            = partition ambig cs
        vs'                         = tyfree safe_cs \\ vs
        ambig (Impl w t p)          = any (`notElem` vs) (tyfree t)
        ambig c                     = any (`notElem` vs) (tyfree c)


instance Subst TSchema where
    msubst (TSchema l [] t)         = TSchema l [] <$> msubst t
    msubst (TSchema l q t)          = TSchema l <$> msubst q <*> msubst t
    {-
    msubst sc@(TSchema l q t)       = (msubst' . Map.toList . Map.filterWithKey relevant) <$> getSubstitution
      where relevant k v            = k `elem` vs0
            vs0                     = tyfree sc
            msubst' s               = TSchema l (subst s q') (subst s t')
              where vs              = tybound q
                    newvars         = tyfree (rng s)
                    clashvars       = vs `intersect` newvars
                    avoidvars       = vs0 ++ vs ++ newvars
                    renaming        = tvarSupplyMap clashvars avoidvars
                    q'              = [ Quant (subst renaming v) (subst renaming cs) | Quant v cs <- q ]
                    t'              = subst renaming t
    -}
    tyfree (TSchema _ [] t)         = tyfree t
    tyfree (TSchema _ q t)          = (tyfree q ++ tyfree t) \\ tybound q
    tybound (TSchema _ q t)         = tybound q

testSchemaSubst = do
    putStrLn ("t:  " ++ prstr t)
    putStrLn ("c:  " ++ prstr c)
    putStrLn ("s1: " ++ prstrs s1)
    putStrLn ("s2: " ++ prstrs s2)
    putStrLn ("s3: " ++ prstrs s3)
    putStrLn ("s4: " ++ prstrs s4)
    putStrLn ("s5: " ++ prstrs s5)
    putStrLn ("subst s1 t: " ++ prstr (subst s1 t))
    putStrLn ("subst s2 t: " ++ prstr (subst s2 t))
    putStrLn ("subst s3 t: " ++ prstr (subst s3 t))
    putStrLn ("subst s4 t: " ++ prstr (subst s4 t))
    putStrLn ("subst s5 t: " ++ prstr (subst s5 t))
    putStrLn ("subst s5 c: " ++ prstr (subst s5 c))
  where t   = tSchema [Quant (TV KType (name "A")) [TC (noQ "Eq") []]] c
        c   = (tCon (TC (noQ "apa") [tVar (TV KType (name "A")), 
                                     tVar (TV KType (name "B")),
                                     tVar (TV KType (name "C"))]))
        s1  = [(TV KType (name "B"), tSelf)]
        s2  = [(TV KType (name "A"), tSelf)]
        s3  = [(TV KType (name "B"), tVar (TV KType (name "A")))]
        s4  = [(TV KType (name "B"), tVar (TV KType (name "C"))), (TV KType (name "C"), tSelf)]
        s5  = [(TV KType (name "B"), tVar (TV KType (name "D"))), (TV KType (name "D"), tSelf)]

msubstRenaming                      :: Subst a => a -> TypeM (Substitution,Substitution)
msubstRenaming c                    = do s <- Map.toList . Map.filterWithKey relevant <$> getSubstitution
                                         return $ (dom s `zip` subst (renaming (tyfree (rng s))) (rng s),renaming (tyfree (rng s)))
      where relevant k _            = k `elem` vs0
            vs0                     = tyfree c
            vs                      = tybound c
            renaming newvars        = tvarSupplyMap clashvars avoidvars
              where clashvars       = vs `intersect` newvars
                    avoidvars       = vs0 ++ vs ++ newvars

msubstWith                          :: (Subst a) => Substitution -> a -> TypeM a
msubstWith [] x                     = return x
msubstWith s x                      = do s0 <- getSubstitution
                                         sequence [ substitute tv t | (tv,t) <- s ]
                                         x' <- msubst x
                                         setSubstitution s0
                                         return x'

testMsubstRenaming = do
    putStrLn ("p1: " ++ render (pretty (runTypeM p1)))
    putStrLn ("p2: " ++ render (pretty (runTypeM p2)))
    putStrLn ("p3: " ++ render (pretty (runTypeM p3)))
    putStrLn ("r1: " ++ render (pretty (runTypeM r1)))
    putStrLn ("r2: " ++ render (pretty (runTypeM r2)))
    putStrLn ("r3: " ++ render (pretty (runTypeM r3)))
  where t   = tSchema [Quant (TV KType (name "A")) [TC (noQ "Eq") []]]
                            (tCon (TC (noQ "apa") [tVar (TV KType (name "A")), 
                                                   tVar (TV KType (name "B"))]))
        msubst' sc@(TSchema l q t) = do (s,ren) <- msubstRenaming sc
                                        return $ TSchema l (subst s (subst ren q)) (subst s (subst ren t))
        p1 = do
            substitute (TV KType (name "B")) tSelf
            msubst t
        p2 = do
            substitute (TV KType (name "A")) tSelf
            msubst t
        p3 = do
            substitute (TV KType (name "B")) (tVar (TV KType (name "A")))
            msubst t
        r1 = do
            substitute (TV KType (name "B")) tSelf
            msubst' t
        r2 = do
            substitute (TV KType (name "A")) tSelf
            msubst' t
        r3 = do
            substitute (TV KType (name "B")) (tVar (TV KType (name "A")))
            msubst' t
        
            


instance Subst TVar where
    msubst v                        = do t <- msubst (TVar NoLoc v)
                                         case t of
                                            TVar _ v' -> return v'
                                            _         -> return v
    tyfree v                        = [v]
        
instance Subst TCon where
    msubst (TC n ts)                = TC n <$> msubst ts
    tyfree (TC n ts)                = tyfree ts

instance Subst QBind where
    msubst (Quant v cs)             = Quant <$> msubst v <*> msubst cs
    tyfree (Quant v cs)             = v : tyfree cs
    tybound (Quant v cs)            = [v]

instance Subst Type where
    msubst (TVar l v)               = do s <- getSubstitution
                                         case Map.lookup v s of
                                            Just t ->  msubst t
                                            Nothing -> return (TVar l v)
    msubst (TCon l c)               = TCon l <$> msubst c
    msubst (TFun l fx p k t)        = TFun l <$> msubst fx <*> msubst p <*> msubst k <*> msubst t
    msubst (TTuple l p k)           = TTuple l <$> msubst p <*> msubst k
    msubst (TUnion l as)            = return $ TUnion l as
    msubst (TOpt l t)               = TOpt l <$> msubst t
    msubst (TNone l)                = return $ TNone l
    msubst (TWild l)                = return $ TWild l
    msubst (TNil l s)               = return $ TNil l s
    msubst (TRow l k n t r)         = TRow l k n <$> msubst t <*> msubst r
    msubst (TFX l fx)               = TFX l <$> msubst fx

    tyfree (TVar _ v)               = [v]
    tyfree (TCon _ c)               = tyfree c
    tyfree (TFun _ fx p k t)        = tyfree fx ++ tyfree p ++ tyfree k ++ tyfree t
    tyfree (TTuple _ p k)           = tyfree p ++ tyfree k
    tyfree (TUnion _ as)            = []
    tyfree (TOpt _ t)               = tyfree t
    tyfree (TNone _)                = []
    tyfree (TWild _)                = []
    tyfree (TNil _ _)               = []
    tyfree (TRow _ _ _ t r)         = tyfree t ++ tyfree r
    tyfree (TFX l fx)               = tyfree fx

instance Subst FX where
    msubst (FXMut t)                = FXMut <$> msubst t
    msubst (FXAct t)                = FXAct <$> msubst t
    msubst fx                       = return fx
    
    tyfree (FXMut t)                = tyfree t
    tyfree (FXAct t)                = tyfree t
    tyfree _                        = []  
    
instance Subst PosPar where
    msubst (PosPar n t e p)         = PosPar n <$> msubst t <*> msubst e <*> msubst p
    msubst (PosSTAR n t)            = PosSTAR n <$> msubst t
    msubst PosNIL                   = return PosNIL
    
    tyfree (PosPar n t e p)         = tyfree t ++ tyfree p
    tyfree (PosSTAR n t)            = tyfree t
    tyfree PosNIL                   = []

instance Subst KwdPar where
    msubst (KwdPar n t e p)         = KwdPar n <$> msubst t <*> msubst e <*> msubst p
    msubst (KwdSTAR n t)            = KwdSTAR n <$> msubst t
    msubst KwdNIL                   = return KwdNIL
    
    tyfree (KwdPar n t e p)         = tyfree t ++ tyfree p
    tyfree (KwdSTAR n t)            = tyfree t
    tyfree KwdNIL                   = []

instance Subst Decl where
    msubst (Def l n q p k a ss de fx)   = Def l n <$> msubst q <*> msubst p <*> msubst k <*> msubst a <*> msubst ss <*> return de <*> msubst fx
    msubst (Actor l n q p k ss)         = Actor l n <$> msubst q <*> msubst p <*> msubst k <*> msubst ss
    msubst (Class l n q bs ss)          = Class l n <$> msubst q <*> msubst bs <*> msubst ss
    msubst (Protocol l n q bs ss)       = Protocol l n <$> msubst q <*> msubst bs <*> msubst ss
    msubst (Extension l n q bs ss)      = Extension l n <$> msubst q <*> msubst bs <*> msubst ss
    {-
    msubst d@(Protocol l n q bs ss)     = do (s,ren) <- msubstRenaming d
                                             return $ Protocol l n (subst s (subst ren q)) (subst s (subst ren bs)) (subst s (subst ren ss))
    msubst d@(Class l n q bs ss)        = do (s,ren) <- msubstRenaming d
                                             return $ Class l n (subst s (subst ren q)) (subst s (subst ren bs)) (subst s (subst ren ss))
    msubst d@(Extension l n q bs ss)    = do (s,ren) <- msubstRenaming d
                                             return $ Extension l n (subst s (subst ren q)) (subst s (subst ren bs)) (subst s (subst ren ss))
    msubst d@(Def l n q p k a ss de fx) = do (s,ren) <- msubstRenaming d
                                             return $ Def l n (subst s (subst ren q)) (subst s (subst ren p)) (subst s (subst ren k))
                                                              (subst s (subst ren a)) (subst s (subst ren ss)) de (subst s fx)
    msubst d@(Actor l n q p k ss)       = do (s,ren) <- msubstRenaming d
                                             return $ Actor l n (subst s (subst ren q)) (subst s (subst ren p)) (subst s (subst ren k))
                                                                (subst s (subst ren ss))
    -}
    tybound (Protocol l n q ps b)   = tvSelf : tybound q
    tybound (Class l n q ps b)      = tvSelf : tybound q
    tybound (Extension l n q ps b)  = tvSelf : tybound q
    tybound (Def l n q p k t b d x) = tybound q
    tybound (Actor l n q p k b)     = tybound q
    
    tyfree (Protocol l n q ps b)   = nub (tyfree q ++ tyfree ps ++ tyfree b) \\ (tvSelf : tybound q)
    tyfree (Class l n q ps b)      = nub (tyfree q ++ tyfree ps ++ tyfree b) \\ (tvSelf : tybound q)
    tyfree (Extension l n q ps b)  = nub (tyfree q ++ tyfree ps ++ tyfree b) \\ (tvSelf : tybound q)
    tyfree (Def l n q p k t b d x) = nub (tyfree q ++ tyfree p ++ tyfree k ++ tyfree b ++ tyfree t ++ tyfree x) \\ tybound q
    tyfree (Actor l n q p k b)     = nub (tyfree q ++ tyfree p ++ tyfree k ++ tyfree b) \\ tybound q
    
instance Subst Stmt where
    msubst (Expr l e)               = Expr l <$> msubst e
    msubst (Assign l ps e)          = Assign l <$> msubst ps <*> msubst e
    msubst (MutAssign l t e)        = MutAssign l <$> msubst t <*> msubst e
    msubst (AugAssign l t op e)     = AugAssign l <$> msubst t <*> return op <*> msubst e
    msubst (Assert l e mbe)         = Assert l <$> msubst e <*> msubst mbe
    msubst (Delete l t)             = Delete l <$> msubst t
    msubst (Return l mbe)           = Return l <$> msubst mbe
    msubst (Raise l mbex)           = Raise l <$> msubst mbex
    msubst (If l bs els)            = If l <$> msubst bs <*> msubst els
    msubst (While l e b els)        = While l <$> msubst e <*> msubst b <*> msubst els
    msubst (For l p e b els)        = For l <$> msubst p <*> msubst e <*> msubst b <*> msubst els
    msubst (Try l b hs els fin)     = Try l <$> msubst b <*> msubst hs <*> msubst els <*> msubst fin
    msubst (With l is b)            = With l <$> msubst is <*> msubst b
    msubst (VarAssign l ps e)       = VarAssign l <$> msubst ps <*> msubst e
    msubst (After l e e')           = After l <$> msubst e <*> msubst e'
    msubst (Decl l ds)              = Decl l <$> msubst ds
    msubst (Signature l ns tsc d)   = Signature l ns <$> msubst tsc <*> return d
    msubst s                        = return s

    tyfree (Decl l ds)              = tyfree ds
    tyfree (Signature l ns tsc d)   = tyfree tsc
    tyfree s                        = []

instance Subst Expr where
    msubst (Call l e p k)           = Call l <$> msubst e <*> msubst p <*> msubst k
    msubst (Await l e)              = Await l <$> msubst e
    msubst (Index l e ix)           = Index l <$> msubst e <*> msubst ix
    msubst (Slice l e sl)           = Slice l <$> msubst e <*> msubst sl
    msubst (Cond l e1 cond e2)      = Cond l <$> msubst e1 <*> msubst cond <*> msubst e2
    msubst (BinOp l e1 op e2)       = BinOp l <$> msubst e1 <*> return op <*> msubst e2
    msubst (CompOp l e ops)         = CompOp l <$> msubst e <*> msubst ops
    msubst (UnOp l op e)            = UnOp l op <$> msubst e
    msubst (Dot l e n)              = Dot l <$> msubst e <*> return n
    msubst (DotI l e i tl)          = DotI l <$> msubst e <*> return i <*> return tl
    msubst (Lambda l p k e fx)      = Lambda l <$> msubst p <*> msubst k <*> msubst e <*> msubst fx
    msubst (Yield l e)              = Yield l <$> msubst e
    msubst (YieldFrom l e)          = YieldFrom l <$> msubst e
    msubst (Tuple l p k)            = Tuple l <$> msubst p <*> msubst k
    msubst (List l es)              = List l <$> msubst es
    msubst (ListComp l e c)         = ListComp l <$> msubst e <*> msubst c
    msubst (Dict l as)              = Dict l <$> msubst as
    msubst (DictComp l a c)         = DictComp l <$> msubst a <*> msubst c
    msubst (Set l es)               = Set l <$> msubst es
    msubst (SetComp l e c)          = SetComp l <$> msubst e <*> msubst c
    msubst (Paren l e)              = Paren l <$> msubst e
    msubst e                        = return e

    tyfree e                        = []

instance Subst Exception where
    msubst (Exception e e')         = Exception <$> msubst e <*> msubst e'
    
    tyfree (Exception e e')         = tyfree e ++ tyfree e'

instance Subst Branch where
    msubst (Branch e b)             = Branch <$> msubst e <*> msubst b
    
    tyfree (Branch e b)             = tyfree e ++ tyfree b

instance Subst Pattern where
    msubst (PVar l n t)             = PVar l n <$> msubst t
    msubst (PParen l p)             = PParen l <$> msubst p
    msubst (PTuple l p k)           = PTuple l <$> msubst p <*> msubst k
    msubst (PList l ps p)           = PList l <$> msubst ps <*> msubst p
    
    tyfree (PVar _ n t)             = tyfree t
    tyfree (PParen _ p)             = tyfree p
    tyfree (PTuple _ p k)           = tyfree p ++ tyfree k
    tyfree (PList _ ps p)           = tyfree ps ++ tyfree p

instance Subst PosPat where
    msubst (PosPat p pp)            = PosPat <$> msubst p <*> msubst pp
    msubst (PosPatStar p)           = PosPatStar <$> msubst p
    msubst PosPatNil                = return PosPatNil

    tyfree (PosPat p pp)            = tyfree p ++ tyfree pp
    tyfree (PosPatStar p)           = tyfree p
    tyfree PosPatNil                = []

instance Subst KwdPat where
    msubst (KwdPat n p kp)          = KwdPat n <$> msubst p <*> msubst kp
    msubst (KwdPatStar p)           = KwdPatStar <$> msubst p
    msubst KwdPatNil                = return KwdPatNil

    tyfree (KwdPat n p kp)          = tyfree p ++ tyfree kp
    tyfree (KwdPatStar p)           = tyfree p
    tyfree KwdPatNil                = []

instance Subst Handler where
    msubst (Handler ex b)           = Handler ex <$> msubst b

    tyfree (Handler ex b)           = tyfree b

instance Subst WithItem where
    msubst (WithItem e p)           = WithItem <$> msubst e <*> msubst p
    
    tyfree (WithItem e p)           = tyfree e ++ tyfree p

instance Subst PosArg where
    msubst (PosArg e p)             = PosArg <$> msubst e <*> msubst p
    msubst (PosStar e)              = PosStar <$> msubst e
    msubst PosNil                   = return PosNil

    tyfree (PosArg e p)             = tyfree e ++ tyfree p
    tyfree (PosStar e)              = tyfree e
    tyfree PosNil                   = []

instance Subst KwdArg where
    msubst (KwdArg n e k)           = KwdArg n <$> msubst e <*> msubst k
    msubst (KwdStar e)              = KwdStar <$> msubst e
    msubst KwdNil                   = return KwdNil

    tyfree (KwdArg n e k)           = tyfree e ++ tyfree k
    tyfree (KwdStar e)              = tyfree e
    tyfree KwdNil                   = []

instance Subst Sliz where
    msubst (Sliz l e1 e2 e3)        = Sliz l <$> msubst e1 <*> msubst e2 <*> msubst e3

    tyfree (Sliz _ e1 e2 e3)        = tyfree e1 ++ tyfree e2 ++ tyfree e3

instance Subst OpArg where
    msubst (OpArg op e)             = OpArg op <$> msubst e

    tyfree (OpArg op e)             = tyfree e

instance Subst Elem where
    msubst (Elem e)                 = Elem <$> msubst e
    msubst (Star e)                 = Star <$> msubst e

    tyfree (Elem e)                 = tyfree e
    tyfree (Star e)                 = tyfree e

instance Subst Assoc where
    msubst (Assoc k v)              = Assoc <$> msubst k <*> msubst v
    msubst (StarStar e)             = StarStar <$> msubst e

    tyfree (Assoc k v)              = tyfree k ++ tyfree v
    tyfree (StarStar e)             = tyfree e

instance Subst Comp where
    msubst (CompFor l p e c)        = CompFor l <$> msubst p <*> msubst e <*> msubst c
    msubst (CompIf l e c)           = CompIf l <$> msubst e <*> msubst c
    msubst NoComp                   = return NoComp

    tyfree (CompFor _ p e c)        = tyfree p ++ tyfree e ++ tyfree c
    tyfree (CompIf _ e c)           = tyfree e ++ tyfree c
    tyfree NoComp                   = []

    
-- Error handling ------------------------------------------------------------------------

data CheckerError                   = FileNotFound ModName
                                    | NameNotFound Name
                                    | NameReserved QName
                                    | NameBlocked QName
                                    | NameUnexpected QName
                                    | TypedReassign Pattern
                                    | IllegalRedef Name
                                    | IllegalExtension QName
                                    | MissingSelf Name
                                    | IllegalImport SrcLoc
                                    | DuplicateImport Name
                                    | NoItem ModName Name
                                    | NoModule ModName
                                    | NoClassOrProto QName
                                    | OtherError SrcLoc String
                                    deriving (Show)

data TypeError                      = TypeErrHmm            -- ...
                                    | RigidVariable TVar
                                    | InfiniteType TVar
                                    | ConflictingRow TVar
                                    | KwdNotFound Name
                                    | DecorationMismatch Name TSchema Deco
                                    | EscapingVar [TVar] TSchema
                                    | NoSelStatic Name TCon
                                    | NoSelInstByClass Name TCon
                                    | NoMut Name
                                    | LackSig Name
                                    | LackDef Name
                                    | NoRed Constraint
                                    | NoSolve [Constraint]
                                    | NoUnify Type Type
                                    deriving (Show)

instance Control.Exception.Exception TypeError
instance Control.Exception.Exception CheckerError


instance HasLoc TypeError where
    loc (RigidVariable tv)          = loc tv
    loc (InfiniteType tv)           = loc tv
    loc (ConflictingRow tv)         = loc tv
    loc (KwdNotFound n)             = loc n
    loc (DecorationMismatch n t d)  = loc n
    loc (EscapingVar tvs t)         = loc tvs
    loc (NoSelStatic n u)           = loc n
    loc (NoSelInstByClass n u)      = loc n
    loc (NoMut n)                   = loc n
    loc (LackSig n)                 = loc n
    loc (LackDef n)                 = loc n
    loc (NoRed c)                   = loc c
    loc (NoSolve cs)                = loc cs
    loc (NoUnify t1 t2)             = loc t1

typeError err                       = (loc err,render (expl err))
  where
    expl (RigidVariable tv)         = text "Type" <+> pretty tv <+> text "is rigid"
    expl (InfiniteType tv)          = text "Type" <+> pretty tv <+> text "is infinite"
    expl (ConflictingRow tv)        = text "Row" <+> pretty tv <+> text "has conflicting extensions"
    expl (KwdNotFound n)            = text "Keyword element" <+> quotes (pretty n) <+> text "is not found"
    expl (DecorationMismatch n t d) = text "Decoration for" <+> pretty n <+> text "does not match signature" <+> pretty (n,NSig t d)
    expl (EscapingVar tvs t)        = text "Type annotation" <+> pretty t <+> text "is too general, type variable" <+>
                                      pretty (head tvs) <+> text "escapes"
    expl (NoSelStatic n u)          = text "Static method" <+> pretty n <+> text "cannot be selected from" <+> pretty u <+> text "instance"
    expl (NoSelInstByClass n u)     = text "Instance attribute" <+> pretty n <+> text "cannot be selected from class" <+> pretty u
    expl (NoMut n)                  = text "Non @property attribute" <+> pretty n <+> text "cannot be mutated"
    expl (LackSig n)                = text "Declaration lacks accompanying signature"
    expl (LackDef n)                = text "Signature lacks accompanying definition"
    expl (NoRed c)                  = text "Cannot infer" <+> pretty c
    expl (NoSolve cs)               = text "Cannot solve" <+> commaSep pretty cs
    expl (NoUnify t1 t2)            = text "Cannot unify" <+> pretty t1 <+> text "and" <+> pretty t2


checkerError (FileNotFound n)       = (loc n, "Type interface file not found for " ++ prstr n)
checkerError (NameNotFound n)       = (loc n, "Name " ++ prstr n ++ " is not in scope")
checkerError (NameReserved n)       = (loc n, "Name " ++ prstr n ++ " is reserved but not yet defined")
checkerError (NameBlocked n)        = (loc n, "Name " ++ prstr n ++ " is currently not accessible")
checkerError (NameUnexpected n)     = (loc n, "Unexpected variable name: " ++ prstr n)
checkerError (TypedReassign p)      = (loc p, "Type annotation on reassignment: " ++ prstr p)
checkerError (IllegalRedef n)       = (loc n, "Illegal redefinition of " ++ prstr n)
checkerError (IllegalExtension n)   = (loc n, "Illegal extension of " ++ prstr n)
checkerError (MissingSelf n)        = (loc n, "Missing 'self' parameter in definition of")
checkerError (IllegalImport l)      = (l,     "Relative import not yet supported")
checkerError (DuplicateImport n)    = (loc n, "Duplicate import of name " ++ prstr n)
checkerError (NoModule m)           = (loc m, "Module " ++ prstr m ++ " does not exist")
checkerError (NoItem m n)           = (loc n, "Module " ++ prstr m ++ " does not export " ++ nstr n)
checkerError (NoClassOrProto n)     = (loc n, "Class or protocol name expected, got " ++ prstr n)
checkerError (OtherError l str)     = (l,str)

nameNotFound n                      = Control.Exception.throw $ NameNotFound n
nameReserved n                      = Control.Exception.throw $ NameReserved n
nameBlocked n                       = Control.Exception.throw $ NameBlocked n
nameUnexpected n                    = Control.Exception.throw $ NameUnexpected n
typedReassign p                     = Control.Exception.throw $ TypedReassign p
illegalRedef n                      = Control.Exception.throw $ IllegalRedef n
illegalExtension n                  = Control.Exception.throw $ IllegalExtension n
missingSelf n                       = Control.Exception.throw $ MissingSelf n
fileNotFound n                      = Control.Exception.throw $ FileNotFound n
illegalImport l                     = Control.Exception.throw $ IllegalImport l
duplicateImport n                   = Control.Exception.throw $ DuplicateImport n
noItem m n                          = Control.Exception.throw $ NoItem m n
noModule m                          = Control.Exception.throw $ NoModule m
notClassOrProto n                   = Control.Exception.throw $ NoClassOrProto n
err l s                             = Control.Exception.throw $ OtherError l s

err1 x s                            = err (loc x) (s ++ " " ++ prstr x)
err2 (x:_) s                        = err1 x s

notYetExpr e                        = notYet (loc e) e

rigidVariable tv                    = Control.Exception.throw $ RigidVariable tv
infiniteType tv                     = Control.Exception.throw $ InfiniteType tv
conflictingRow tv                   = Control.Exception.throw $ ConflictingRow tv
kwdNotFound n                       = Control.Exception.throw $ KwdNotFound n
decorationMismatch n t d            = Control.Exception.throw $ DecorationMismatch n t d
escapingVar tvs t                   = Control.Exception.throw $ EscapingVar tvs t
noSelStatic n u                     = Control.Exception.throw $ NoSelStatic n u
noSelInstByClass n u                = Control.Exception.throw $ NoSelInstByClass n u
noMut n                             = Control.Exception.throw $ NoMut n
lackSig ns                          = Control.Exception.throw $ LackSig (head ns)
lackDef ns                          = Control.Exception.throw $ LackDef (head ns)
noRed c                             = Control.Exception.throw $ NoRed c
noSolve cs                          = Control.Exception.throw $ NoSolve cs
noUnify t1 t2                       = Control.Exception.throw $ NoUnify t1 t2

