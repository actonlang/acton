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

{-# LANGUAGE FlexibleInstances, FlexibleContexts, DeriveGeneric #-}
module Acton.TypeEnv where

import Control.Monad
import qualified Control.Exception
import Control.Monad.State.Strict
import Control.Monad.Except
import Data.Char
import Error.Diagnose hiding ((<>), err)
import Prelude hiding ((<>))

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)

import Pretty
import Utils
import Acton.Syntax
import Acton.Builtin
import Acton.Prim
import Acton.Printer
import Acton.Names
import Acton.NameInfo
import Acton.Subst
import Acton.Env
import Acton.LookupStats


data TypeX                      = TypeX {
                                    activeWits  :: [Witness],
                                    closedWits  :: [Witness],
                                    activeWitMap :: WitMap,
                                    closedWitMap :: WitMap,
                                    activeWitTypeMap :: WitMap,
                                    closedWitTypeMap :: WitMap,
                                    posnames    :: [Name],
                                    indecl      :: Bool,
                                    forced      :: Bool,
                                    tyids       :: Map QName Int,
                                    tyidHash    :: HashMap QName Int,
                                    tyinfos     :: IntMap TyInfo,
                                    tycons      :: IntSet,
                                    typrotos    :: IntSet,
                                    tyactors    :: IntSet,
                                    tyconAttrs  :: TyAttrMap,
                                    typrotoAttrs:: TyAttrMap,
                                    tyabstracts :: Map QName [Name]
                                  }

data TyInfo                     = TyInfo {
                                    tywild      :: Type,
                                    tyabove     :: IntSet,
                                    tybelow     :: IntSet,
                                    tyattrs     :: Set Name
                                  }

type Env                        = EnvF TypeX
type WitMap                     = Map QName [Witness]
type TyAttrMap                  = Map Name [Int]

initTypeEnv                     :: Env0 -> Env
initTypeEnv env0                = setX env0 $ foldl' importInfo x0 imps
  where x0                      = TypeX {
                                    activeWits  = [],
                                    closedWits  = primWits,
                                    activeWitMap = Map.empty,
                                    closedWitMap = witMap primWits,
                                    activeWitTypeMap = Map.empty,
                                    closedWitTypeMap = witTypeMap primWits,
                                    posnames    = [],
                                    indecl      = False,
                                    forced      = False,
                                    tyids       = Map.empty,
                                    tyidHash    = HashMap.empty,
                                    tyinfos     = tyinfos0,
                                    tycons      = IntSet.empty,
                                    typrotos    = IntSet.empty,
                                    tyactors    = IntSet.empty,
                                    tyconAttrs  = Map.empty,
                                    typrotoAttrs= Map.empty,
                                    tyabstracts = Map.empty
                                  }
        importInfo x (m,te)     = setupCons env0 f te $ setupWits addClosedWit f te x
          where f               = GName m
        imps | inBuiltin env0   = []
             | otherwise        = [ (m, fromJust $ lookupMod m env0) | m <- mBuiltin : transitiveImports env0 ]


tyinfos0                        = IntMap.fromDistinctAscList pairs
  where pairs                   = [ (tyid t, TyInfo t (iset above) (iset below) Set.empty) | (t,above,below) <- graph ]
        tyid t                  = fromJust $ elemIndex t wtypes
        iset ts                 = IntSet.fromDistinctAscList $ map tyid ts
        wtypes                  = [ t | (t,_,_) <- graph ]
        graph                   = [
                                    (wOpt,      [wOpt],                 [wOpt,tNone]),      -- Watch out for tNone here!
                                    (tNone,     [wOpt,tNone],           [tNone]),
                                    (wFun,      [wOpt, wFun],           [wFun]),
                                    (wTuple,    [wOpt, wTuple],         [wTuple]),
                                    (fxProc,    [fxProc],               [fxProc,fxMut,fxPure,fxAction]),
                                    (fxMut,     [fxProc,fxMut],         [fxMut,fxPure]),
                                    (fxPure,    [fxProc,fxMut,fxPure],  [fxPure]),
                                    (fxAction,  [fxProc,fxAction],      [fxAction])
                                  ]
        wFun                    = tFun tWild tWild tWild tWild
        wTuple                  = tTuple tWild tWild
        wOpt                    = tOpt tWild

instance Show TypeX where
    show _                      = ""

printX env                      = render (nest 4 $ vcat $ map (prinfo x) $ Map.assocs $ tyids x)
  where x                       = envX env


instance Pretty TypeX where
    pretty x                    = text "--- witnesses:"  $+$
                                  vcat (map pretty (witnesses x))

prinfo x (n, tid)               = pretty (noq n) <+> text "=" <+> pretty tid <> colon $+$
                                  nest 4 (text "above" <> colon <+> commaSep pretty (IntSet.toAscList $ tyabove info) $+$
                                          text "below" <> colon <+> commaSep pretty (IntSet.toAscList $ tybelow info) $+$
                                          text "attrs" <> colon <+> commaSep pretty (Set.elems $ tyattrs info))
  where info                    = tyinfos x IntMap.! tid

instance USubst TypeX where
    usubstWith s x              = let we = usubstWith s (activeWits x)
                                  in x{ activeWits = we,
                                        activeWitMap = witMap we,
                                        activeWitTypeMap = witTypeMap we }

instance UFree TypeX where
    ufree x                     = ufree (activeWits x)

witnesses                       :: TypeX -> [Witness]
witnesses x                     = activeWits x ++ closedWits x

witMap                          :: [Witness] -> WitMap
witMap                          = foldr addWit Map.empty

witTypeMap                      :: [Witness] -> WitMap
witTypeMap                      = foldr addWitType Map.empty


nextid x                        = 1 + fst (IntMap.findMax $ tyinfos x)

addconinfo                      :: EnvF x -> (Name -> QName) -> TypeX -> (Name,NameInfo) -> TypeX
addconinfo env f x (n,i)
  | NClass q us te _ <- i       = addcon n q us te addclass x
  | NProto q us te _ <- i       = addcon n q us te addproto x
  | NAct q _ _ te _ <- i        = addcon n q [] (notHidden te) addactor x
  | otherwise                   = x
  where addclass tid info x     = x{ tycons = IntSet.insert tid (tycons x),
                                     tyconAttrs = addTyAttrs tid (tyattrs info) (tyconAttrs x) }
        addproto tid info x     = x{ typrotos = IntSet.insert tid (typrotos x),
                                     typrotoAttrs = addTyAttrs tid (tyattrs info) (typrotoAttrs x) }
        addactor tid info x     = addclass tid info x{ tyactors = IntSet.insert tid (tyactors x) }

        addcon n q us te index x
                                  = index tid info x{ tyids = Map.insert qn tid (tyids x),
                                                      tyidHash = HashMap.insert qn tid (tyidHash x),
                                                      tyinfos = IntMap.insert tid info tyinfos',
                                                      tyabstracts = Map.insert qn abstracts (tyabstracts x) }
          where tid             = nextid x
                qn              = f n
                ui              = [ typeId x (tcname c) | (_,c) <- us ]
                info            = TyInfo {
                                    tywild = tCon $ TC qn [ tWild | _ <- q ],
                                    tyabove = IntSet.fromList ui,
                                    tybelow = IntSet.singleton tid,
                                    tyattrs = foldr Set.union (Set.fromList $ dom te) [ tyattrs $ fromJust $ IntMap.lookup u $ tyinfos x | u <- ui ]
                }
                tyinfos'        = foldr (IntMap.adjust addbelow) (tyinfos x) ui
                addbelow info   = info{ tybelow = IntSet.insert tid (tybelow info) }
                abstracts       = indexAbstractAttrs env us te

indexAbstractAttrs              :: EnvF x -> [WTCon] -> TEnv -> [Name]
indexAbstractAttrs env us te     = recordLookupList "type.indexAbstractAttrs" $
                                   [ n | n <- names, maybe False isAbstract (Map.lookup n visible) ]
  where aenv                    = reverse te ++ concat [ reverse te' | (_,c) <- us, let (_,_,te') = findConName (tcname c) env ]
        names                   = nub $ reverse $ map fst aenv
        visible                 = foldl' add Map.empty aenv
        add m (n,i)             = Map.insertWith (\_ old -> old) n i m
        isAbstract (NSig _ dec _) = dec /= Property
        isAbstract _            = False

addTyAttrs                      :: Int -> Set Name -> TyAttrMap -> TyAttrMap
addTyAttrs tid attrs attrmap     = Set.foldr add attrmap attrs
  where add n                   = Map.insertWith (++) n [tid]

typeById                        :: TypeX -> Int -> Type
typeById x tid                  = tywild (tyinfos x IntMap.! tid)

conById                         :: TypeX -> Int -> Maybe TCon
conById x tid                   = case typeById x tid of
                                    TCon _ c -> Just c
                                    _ -> Nothing

lookupTypeId                    :: TypeX -> QName -> Maybe Int
lookupTypeId x n                = recordLookup "type.lookupTypeId" $
                                  HashMap.lookup n (tyidHash x)

typeId                          :: TypeX -> QName -> Int
typeId x n                      = fromJust $ lookupTypeId x n

tyconDescendants                :: Env -> TCon -> [TCon]
tyconDescendants env tc
  | Just tid <- lookupTypeId x (tcname tc),
    Just info <- IntMap.lookup tid (tyinfos x)
                                = [ c | tid' <- tids tid info, Just c <- [conById x tid'] ]
  | otherwise                   = allDescendants env tc
  where x                       = envX env
        tids tid info           = IntSet.toAscList $
                                  IntSet.delete tid $
                                  IntSet.intersection (tybelow info) (tycons x)

tyconsByAttr                    :: Env -> Name -> [TCon]
tyconsByAttr env n              = [ c | tid <- Map.findWithDefault [] n (tyconAttrs x), Just c <- [conById x tid] ]
  where x                       = envX env

typrotosByAttr                  :: Env -> Name -> [PCon]
typrotosByAttr env n            = [ p | tid <- Map.findWithDefault [] n (typrotoAttrs x), Just p <- [conById x tid] ]
  where x                       = envX env

abstractAttrsX                  :: Env -> QName -> [Name]
abstractAttrsX env n            = recordLookupList "type.abstractAttrs" $
                                  case lookupCached n of
                                    Just attrs -> attrs
                                    Nothing    -> case lookupCached un of
                                                    Just attrs -> attrs
                                                    Nothing    -> case un of
                                                                    GName m n'
                                                                      | Just m == thismod env -> maybe fallback id (lookupCached $ NoQ n')
                                                                    _ -> fallback
  where lookupCached n          = Map.lookup n (tyabstracts $ envX env)
        un                      = unalias env n
        fallback                = abstractAttrs env n



tydefine                        :: TEnv -> Env -> Env
tydefine te env                 = modX env' (setupCons env' f te . setupWits addActiveWit NoQ te)
  where f                       = if inBuiltin env then GName mBuiltin else NoQ
        env'                    = define te env

tydefineClosed                  :: TEnv -> Env -> Env
tydefineClosed te env           = modX env' (setupCons env' f te . setupWits addClosedWit NoQ te)
  where f                       = if inBuiltin env then GName mBuiltin else NoQ
        env'                    = defineClosed te env

setupCons                       :: EnvF x -> (Name -> QName) -> TEnv -> TypeX -> TypeX
setupCons env f te x            = foldl' (addconinfo env f) x te
 
setupWits                       :: (TypeX -> Witness -> TypeX) -> (Name -> QName) -> TEnv -> TypeX -> TypeX
setupWits add f te x            = foldl' add x wits
  where wits                    = [ WClass q (tCon c) p (f n) ws (length opts) | (n, NExt q c ps te' opts _) <- te, (ws,p) <- ps ]

addvarinfo x (tv, c, _)         = x{ tyids = Map.insert qn tid (tyids x),
                                     tyidHash = HashMap.insert qn tid (tyidHash x),
                                     tyinfos = IntMap.insert tid info tyinfos' }
  where tid                     = nextid x
        qn                      = NoQ $ tvname tv
        ci                      = tyinfos x IntMap.! typeId x (tcname c)
        info                    = TyInfo {
                                    tywild = tVar tv,
                                    tyabove = IntSet.insert tid $ tyabove ci,
                                    tybelow = IntSet.singleton tid,
                                    tyattrs = tyattrs ci
                                  }
        tyinfos'                = IntSet.foldr' (IntMap.adjust addbelow) (tyinfos x) (tyabove ci)
        addbelow info           = info{ tybelow = IntSet.insert tid (tybelow info) }

tydefineVars                    :: QBinds -> Env -> Env
tydefineVars q env              = modX env1 (\x -> foldl' addvarinfo x tvs)
  where env1                    = modX env0 (\x -> foldl' addActiveWit x wits)
        env0                    = defineTVars q env
        tvs                     = [ (TV k v, c, us) | (v, NTVar k c us) <- take (length q) (activeNames env0), let tv = TV k v ]
        wits                    = [ WInst [] (tVar tv) p (NoQ $ tvarWit tv u) wchain | (tv, _, us) <- tvs, u <- us, (wchain,p) <- findAncestry env u ]

tydefineInst                    :: TCon -> [WTCon] -> Name -> Env -> Env
tydefineInst c ps w env         = modX env (\x -> foldl' addActiveWit x wits)
  where wits                    = [ WInst [] (tCon c) p (NoQ w) ws | (ws,p) <- ps ]

addActiveWit                    :: TypeX -> Witness -> TypeX
addActiveWit x wit
  | null same                   = x{ activeWits = wit : activeWits x,
                                     activeWitMap = addWit wit (activeWitMap x),
                                     activeWitTypeMap = addWitType wit (activeWitTypeMap x) }
  | otherwise                   = x
  where same                    = [ w | w <- witsByPNameX x (tcname $ proto wit), wtype w == wtype wit ]

addClosedWit                    :: TypeX -> Witness -> TypeX
addClosedWit x wit
  | null same                   = x{ closedWits = wit : closedWits x,
                                     closedWitMap = addWit wit (closedWitMap x),
                                     closedWitTypeMap = addWitType wit (closedWitTypeMap x) }
  | otherwise                   = x
  where same                    = [ w | w <- witsByPNameX x (tcname $ proto wit), wtype w == wtype wit ]

addWit                          :: Witness -> WitMap -> WitMap
addWit w                        = Map.insertWith (++) (tcname $ proto w) [w]

addWitType                      :: Witness -> WitMap -> WitMap
addWitType w m                  = maybe m (\n -> Map.insertWith (++) n [w] m) (wtypeKey $ wtype w)

wtypeKey                        :: Type -> Maybe QName
wtypeKey (TCon _ c)             = Just (tcname c)
wtypeKey (TVar _ v)             = Just (NoQ $ tvname v)
wtypeKey _                      = Nothing

witsByPNameX x pn               = Map.findWithDefault [] pn (activeWitMap x) ++
                                  Map.findWithDefault [] pn (closedWitMap x)

witsByPName                     :: Env -> QName -> [Witness]
witsByPName env pn              = recordLookupList "type.witsByPName" $
                                  witsByPNameX (envX env) pn

witsByTName                     :: Env -> QName -> [Witness]
witsByTName env tn              = recordLookupList "type.witsByTName" $
                                  Map.findWithDefault [] tn (activeWitTypeMap x) ++
                                  Map.findWithDefault [] tn (closedWitTypeMap x)
  where x                       = envX env

limitQuant                      :: TUni -> Env -> Env
limitQuant (UV _ l _) env
  | n <= 0                      = env
  | otherwise                   = modX env1 $ \x -> let we = dropw (activeWits x)
                                                    in x{ activeWits = we,
                                                          activeWitMap = witMap we,
                                                          activeWitTypeMap = witTypeMap we }
  where env1                    = setActiveNames (dropv n (activeNames env)) env{ qlevel = qlevel env - n }
        n                       = qlevel env - l
        vs                      = takev n (activeNames env)
        takev 0 te              = []
        takev n ((v,i):te)
          | NTVar{} <- i        = v : takev (n-1) te
          | otherwise           = takev n te
        dropv 0 te              = te
        dropv n ((v,i):te)
          | NTVar{} <- i        = dropv (n-1) te
          | otherwise           = (v,i) : dropv n te
        dropw (WInst{wtype=TVar _ (TV _ v)} : we)
          | v `elem` vs         = dropw we
        dropw we                = we


posdefine te env                = modX (tydefine te env) $ \x -> x{ posnames = dom te ++ posnames x }

setInDecl env                   = modX env $ \x -> x{ indecl = True }

useForce env                    = modX env $ \x -> x{ forced = True }

inDecl env                      = indecl $ envX env

isForced env                    = forced $ envX env


instance Polarity Env where
    polvars env                 = polvars pte `polcat` invvars ite
      where (pte, ite)          = span ((`elem` pvs) . fst) (activeNames env)
            pvs                 = posnames $ envX env


-- Constraints -------------------------------------------------------------------------------

data Constraint = Cast  {info :: ErrInfo, scope :: Env, type1 :: Type, type2 :: Type}
                | Sub   {info :: ErrInfo, scope :: Env, wit :: Name, type1 :: Type, type2 :: Type}
                | Proto {info :: ErrInfo, scope :: Env, wit :: Name, type1 :: Type, proto1 :: PCon}
                | Sel   {info :: ErrInfo, scope :: Env, wit :: Name, type1 :: Type, name1 :: Name, type2 :: Type}
                | Mut   {info :: ErrInfo, scope :: Env, type1 :: Type, name1 :: Name, type2 :: Type}
                | Seal  {info :: ErrInfo, scope :: Env, type1 :: Type}
                deriving (Show)

type Constraints = [Constraint]

instance HasLoc Constraint where
    loc (Cast info env t1 t2)       = getLoc [loc info, loc t1, loc t2]
    loc (Sub info env _ t1 t2)      = getLoc [loc info, loc t1, loc t2]
    loc (Proto info _ env t1 _)     = getLoc [loc info, loc t1]
    loc (Sel info _ env t1  n1 t2)  = getLoc [loc info, loc t1, loc n1, loc t2]
    loc (Mut info env t1  n1 t2)    = getLoc [loc info, loc t1, loc n1, loc t2]
    loc (Seal info env t1)          = getLoc [loc info, loc t1]

instance Pretty Constraint where
    pretty (Cast _ env t1 t2)       = prettyQuant env <+> pretty t1 <+> text "<" <+> pretty t2
    pretty (Sub _ env w t1 t2)      = prettyQuant env <+> pretty w <+> colon <+> pretty t1 <+> text "<" <+> pretty t2
    pretty (Proto _ env w t u)      = prettyQuant env <+> pretty w <+> colon <+> pretty t <+> parens (pretty u)
    pretty (Sel _ env w t1 n t2)    = prettyQuant env <+> pretty w <+> colon <+> pretty t1 <> text "." <> pretty n <+> text "<" <+> pretty t2
    pretty (Mut _ env t1 n t2)      = prettyQuant env <+> pretty t1 <+> text "." <> pretty n <+> text ">" <+> pretty t2
    pretty (Seal _ env t)           = prettyQuant env <+> text "$Seal" <+> pretty t

prettyQuant env
  | qlevel env > 0                  = brackets (commaSep pretty q) <+> text "=>"
  | otherwise                       = empty
  where q                           = [ QBind (TV k tv) (if c == cValue then ps else c:ps) | (tv, NTVar k c ps) <- activeNames env ]

instance VFree Constraint where
    vfree (Cast info env t1 t2)     = vfree t1 ++ vfree t2
    vfree (Sub info env w t1 t2)    = vfree t1 ++ vfree t2
    vfree (Proto info env w t p)    = vfree t ++ vfree p
    vfree (Sel info env w t1 n t2)  = vfree t1 ++ vfree t2
    vfree (Mut info env t1 n t2)    = vfree t1 ++ vfree t2
    vfree (Seal info env t)         = vfree t

instance UFree Constraint where
    ufree (Cast info env t1 t2)     = ufree t1 ++ ufree t2
    ufree (Sub info env w t1 t2)    = ufree t1 ++ ufree t2
    ufree (Proto info env w t p)    = ufree t ++ ufree p
    ufree (Sel info env w t1 n t2)  = ufree t1 ++ ufree t2
    ufree (Mut info env t1 n t2)    = ufree t1 ++ ufree t2
    ufree (Seal info env t)         = ufree t

instance Tailvars Constraint where
    tailvars (Cast _ env t1 t2)     = tailvars t1 ++ tailvars t2
    tailvars (Sub _ env w t1 t2)    = tailvars t1 ++ tailvars t2
    tailvars (Proto _ env w t p)    = tailvars t ++ tailvars p
    tailvars (Sel _ env w t1 n t2)  = tailvars t1 ++ tailvars t2
    tailvars (Mut _ env t1 n t2)    = tailvars t1 ++ tailvars t2
    tailvars (Seal _ env t)         = tailvars t

instance Vars Constraint where
    freeQ (Cast _ env t1 t2)        = freeQ t1 ++ freeQ t2
    freeQ (Sub _ env w t1 t2)       = freeQ t1 ++ freeQ t2
    freeQ (Proto _ env w t p)       = freeQ t ++ freeQ p
    freeQ (Sel _ env w t1 n t2)     = freeQ t1 ++ freeQ t2
    freeQ (Mut _ env t1 n t2)       = freeQ t1 ++ freeQ t2
    freeQ (Seal _ env t)            = freeQ t

instance UWild Constraint where
    uwild (Cast info env t1 t2)     = Cast info env (uwild t1) (uwild t2)
    uwild (Sub info env w t1 t2)    = Sub info env w (uwild t1) (uwild t2)
    uwild (Proto info env w t p)    = Proto info env w (uwild t) (uwild p)
    uwild (Sel info env w t1 n t2)  = Sel info env w (uwild t1) n (uwild t2)
    uwild (Mut info env t1 n t2)    = Mut info env (uwild t1) n (uwild t2)
    uwild (Seal info env t)         = Seal info env (uwild t)

instance VSubst Constraint where
    vsubst s (Cast i env t1 t2)     = Cast i env (vsubst s t1) (vsubst s t2)
    vsubst s (Sub i env w t1 t2)    = Sub i env w (vsubst s t1) (vsubst s t2)
    vsubst s (Proto i env w t p)    = Proto i env w (vsubst s t) (vsubst s p)
    vsubst s (Sel i env w t1 n t2)  = Sel i env w (vsubst s t1) n (vsubst s t2)
    vsubst s (Mut i env t1 n t2)    = Mut i env (vsubst s t1) n (vsubst s t2)
    vsubst s (Seal i env t)         = Seal i env (vsubst s t)

requantize env cs                   = map requant cs
  where requant (Cast i _ t1 t2)    = Cast i env t1 t2
        requant (Sub i _ w t1 t2)   = Sub i env w t1 t2
        requant (Proto i _ w t p)   = Proto i env w t p
        requant (Sel i _ w t1 n t2) = Sel i env w t1 n t2
        requant (Mut i _ t1 n t2)   = Mut i env t1 n t2
        requant (Seal i _ t)        = Seal i env t


closeDepVars vs cs
  | null vs'                        = nub vs
  | otherwise                       = closeDepVars (vs'++vs) cs
  where vs'                         = concat [ deps c \\ vs | c <- cs, all (`elem` vs) (heads c) ]

        heads (Proto _ w _ t _)     = ufree t
        heads (Cast _ _ t _)        = ufree t
        heads (Sub _ w _ t _)       = ufree t
        heads (Sel _ w _ t n _)     = ufree t
        heads (Mut _ _ t n _)       = ufree t
        heads (Seal _ _ t)          = ufree t

        deps (Proto _ w _ _ p)      = ufree p
        deps (Cast _ _ _ t)         = typarams t
        deps (Sub _ w _ _ t)        = typarams t
        deps (Sel _ w _ _ n t)      = ufree t
        deps (Mut _ _ _ n t)        = ufree t
        deps (Seal _ _ _)           = []

        typarams (TOpt _ t)         = typarams t
        typarams (TCon _ c)         = ufree c
        typarams _                  = []


closePolVars                        :: ([TUni],[TUni]) -> Constraints -> ([TUni],[TUni])
closePolVars pvs cs
  | polnull (pvs' `polminus` pvs)   = pvs'
  | otherwise                       = closePolVars pvs' cs'
  where
    (pvs',cs')                      = boundvs pvs cs

    boundvs pn []                   = (pn, [])
    boundvs (p,n) (Cast _ _ (TUni _ v) (TUni _ u) : cs)
                                    = boundvs (if u `elem` p then v:p else p, if v `elem` n then u:n else n) cs
    boundvs (p,n) (Sub _ _ _ (TUni _ v) (TUni _ u) : cs)
                                    = boundvs (if u `elem` p then v:p else p, if v `elem` n then u:n else n) cs
    boundvs pn (Cast _ _ t (TUni _ v) : cs)
      | v `elem` fst pn             = boundvs (polvars t `polcat` pn) cs
    boundvs pn (Sub _ _ _ t (TUni _ v) : cs)
      | v `elem` fst pn             = boundvs (polvars t `polcat` pn) cs
    boundvs pn (Cast _ _ (TUni _ v) t : cs)
      | v `elem` snd pn             = boundvs (polneg (polvars t) `polcat` pn) cs
    boundvs pn (Sub _ _ _ (TUni _ v) t : cs)
      | v `elem` snd pn             = boundvs (polneg (polvars t) `polcat` pn) cs
    boundvs pn (Proto _ _ _ (TUni _ v) p : cs)
      | v `elem` snd pn             = boundvs (polneg (polvars p) `polcat` pn) cs
    boundvs pn (Sel _ _ _ (TUni _ v) _ t : cs)
      | v `elem` snd pn             = boundvs (polneg (polvars t) `polcat` pn) cs
    boundvs pn (Mut _ _ (TUni _ v) _ t : cs)
      | v `elem` (fst pn ++ snd pn) = boundvs (invvars t `polcat` pn) cs
    boundvs pn (c : cs)             = let (pn',cs') = boundvs pn cs in (pn', c:cs')


headvar (Proto _ w _ (TUni _ u) p)    = u

headvar (Cast _ _ TVar{} (TUni _ u))  = u
headvar (Cast _ _ (TUni _ u) t)       = u
headvar (Cast _ _ t (TUni _ u))       = u     -- ?

headvar (Sub _ w _ TVar{} (TUni _ u)) = u
headvar (Sub _ w _ (TUni _ u) t)      = u
headvar (Sub _ w _ t (TUni _ u))      = u     -- ?

headvar (Sel _ w _ (TUni _ u) n t)    = u

headvar (Mut _ _ (TUni _ u) n t)      = u

headvar (Seal _ _ (TUni _ u))         = u



-- Type inference monad ------------------------------------------------------------------

data TypeState                          = TypeState {
                                                nextint         :: Int,
                                                uniqprefix      :: String, -- Prefix for generated names
                                                effectstack     :: [(TFX,Type)],
                                                deferred        :: Constraints,
                                                unisubst        :: IntMap Type
                                          }

initTypeState p                         = TypeState { nextint = 1, uniqprefix = p, effectstack = [], deferred = [], unisubst = IntMap.empty }

type TypeM a                            = ExceptT TypeError (State TypeState) a

runTypeMState                           :: String -> TypeM a -> Either TypeError (a, TypeState)
runTypeMState p m                       = case runState (runExceptT m) (initTypeState p) of
                                            (Right x, st') -> Right (x, st')
                                            (Left err, _)  -> Left err

runTypeM                                :: TypeM a -> a
runTypeM m                              = case runTypeMState "" m of
                                            Right (x,_) -> x
                                            Left err    -> Control.Exception.throw err

currentState                            :: TypeM TypeState
currentState                            = lift $ state $ \st -> (st, st)

rollbackState                           :: TypeState -> TypeM ()
rollbackState st                        = lift $ state $ \_ -> ((), st)

newUnique                               :: TypeM Int
newUnique                               = lift $ state $ \st -> (nextint st, st{ nextint = nextint st + 1 })

pushFX                                  :: TFX -> Type -> TypeM ()
pushFX fx ret                           = lift $ state $ \st -> ((), st{ effectstack = (fx,ret) : effectstack st })

currFX                                  :: TypeM TFX
currFX                                  = lift $ state $ \st -> (fst $ head $ effectstack st, st)

currRet                                 :: TypeM Type
currRet                                 = lift $ state $ \st -> (snd $ head $ effectstack st, st)

popFX                                   :: TypeM ()
popFX                                   = lift $ state $ \st -> ((), st{ effectstack = tail (effectstack st) })

defer                                   :: Constraints -> TypeM ()
defer cs                                = lift $ state $ \st -> ((), st{ deferred = cs ++ deferred st })

collectDeferred                         :: TypeM Constraints
collectDeferred                         = lift $ state $ \st -> (deferred st, st{ deferred = [] })

usubstitute                             :: TUni -> Type -> TypeM ()
usubstitute uv t                        = lift $
                                          --trace ("  #usubstitute " ++ prstr uv ++ " ~ " ++ prstr t) $
                                          state $ \st -> ((), st{ unisubst = IntMap.insert (uvid uv) t (unisubst st)})

usubstitution                           :: TypeM (IntMap Type)
usubstitution                           = lift $ state $ \st -> (unisubst st, st)

uextend                                 :: [(TUni,Type)] -> TypeM ()
uextend s                               = lift $
                                          state $ \st -> ((), st{ unisubst = IntMap.union (unisubst st) (IntMap.fromList [ (uvid u,t) | (u,t) <- s ])})

-- Name generation ------------------------------------------------------------------------------------------------------------------

newGenerated p                          = do i <- newUnique
                                             st <- currentState
                                             return $ Internal p (tag (uniqprefix st) i) 0
  where tag "" i                        = show i
        tag s i                         = s ++ "_" ++ show i

newWitness                              = newGenerated Witness

newTmp                                  = newGenerated Tempvar

newUnivarOfKind k env                   = TUni NoLoc <$> univar k (qlevel env) <$> newUnique

newUnivarToken n                        = TUni NoLoc $ unitoken n

newUnivars env ks                       = mapM (\k -> newUnivarOfKind k env) ks

newUnivar env                           = newUnivarOfKind KType env


-- unification ----------------------------------------------------------------------------------------------------------------------

tryUnify info t1 t2                         = unify info t1 t2 `catchError` \err -> Control.Exception.throw err

unify                                       :: ErrInfo -> Type -> Type -> TypeM ()
unify info t1 t2                            = do t1' <- usubst t1
                                                 t2' <- usubst t2
                                                 --traceM ("  #unify " ++ prstr t1' ++ " and " ++ prstr t2')
                                                 unify' info t1' t2'

unifyM info ts1 ts2                         = mapM_ (uncurry $ unify info) (ts1 `zip` ts2)


unify' _ (TWild _) t2                       = return ()
unify' _ t1 (TWild _)                       = return ()

unify' info (TCon _ c1) (TCon _ c2)
  | tcname c1 == tcname c2                  = unifyM info (tcargs c1) (tcargs c2)

unify' info (TFun _ fx1 p1 k1 t1) (TFun _ fx2 p2 k2 t2)
                                            = do unify info fx1 fx2
                                                 unify info p2 p1
                                                 unify info k2 k1
                                                 unify info t1 t2

unify' info (TTuple _ p1 k1) (TTuple _ p2 k2)
                                            = do unify info p1 p2
                                                 unify info k1 k2

unify' info (TOpt _ t1) (TOpt _ t2)         = unify info t1 t2
unify' _ (TNone _) (TNone _)                = return ()

unify' _ (TFX _ fx1) (TFX _ fx2)
  | fx1 == fx2                              = return ()

unify' _ (TNil _ k1) (TNil _ k2)
  | k1 == k2                                = return ()
unify' info (TRow _ k1 n1 t1 r1) (TRow _ k2 n2 t2 r2)
  | k1 == k2 && n1 == n2                    = do unify info t1 t2
                                                 unify info r1 r2
unify' info (TStar _ k1 r1) (TStar _ k2 r2)
  | k1 == k2                                = unify info r1 r2

unify' info (TVar _ v1) (TVar _ v2)
  | v1 == v2                                = return ()

unify' info t1@(TUni _ v1) t2@(TUni _ v2)
  | v1 == v2                                = return ()
  | uvlevel v1 < uvlevel v2                 = usubstitute v2 t1
  | otherwise                               = usubstitute v1 t2     -- Retain the var with the smallest ulevel (largest scope)

unify' info (TUni _  v) t2                  = do when (v `elem` ufree t2) (infiniteType v t2)
                                                 usubstitute v t2
unify' info t1 (TUni _  v)                  = do when (v `elem` ufree t1) (infiniteType v t1)
                                                 usubstitute v t1

unify' info t1 t2                           = noUnify info t1 t2


-- Asymmetric matching --------------------------------------------------------------------------------

match vs (TWild _) t                        = Just []
match vs t (TWild _)                        = Just []
match vs (TUni _ _) t                       = Just []
match vs t (TUni _ _)                       = Just []
match vs (TCon _ c1) (TCon _ c2)
  | tcname c1 == tcname c2                  = matches vs (tcargs c1) (tcargs c2)
match vs (TFun _ fx1 p1 k1 t1) (TFun _ fx2 p2 k2 t2)
                                            = matches vs [fx1,p1,k1,t1] [fx2,p2,k2,t2]
match vs (TTuple _ p1 k1) (TTuple _ p2 k2)
                                            = matches vs [p1,k1] [p2,k2]
match vs (TOpt _ t1) (TOpt _ t2)            = match vs t1 t2
match vs (TNone _) (TNone _)                = Just []
match vs (TFX _ fx1) (TFX _ fx2)
  | fx1 == fx2                              = Just []

match vs (TNil _ k1) (TNil _ k2)
  | k1 == k2                                = Just []
match vs (TRow _ k1 n1 t1 r1) (TRow _ k2 n2 t2 r2)
  | k1 == k2 && n1 == n2                    = matches vs [t1,r1] [t2,r2]
match vs (TStar _ k1 r1) (TStar _ k2 r2)
  | k1 == k2                                = match vs r1 r2
match vs (TVar _ tv1) (TVar _ tv2)
  | tv1 == tv2                              = Just []
match vs t1 (TVar _ tv)
  | tv `elem` vs && tv `notElem` vfree t1   = Just [(tv, t1)]
match vs t1 t2                              = Nothing

matches vs [] []                            = Just []
matches vs (t:ts) (t':ts')                  = do s1 <- match vs t t'
                                                 s2 <- matches vs ts ts'
                                                 merge s1 s2
  where merge s1 s2
          | agree                           = Just $ s1 ++ s2
          | otherwise                       = Nothing
          where agree                       = and [ vsubst s1 (tVar v) `wildeq` vsubst s2 (tVar v) | v <- dom s1 `intersect` dom s2 ]
                t `wildeq` t'               = match [] t t' == Just []


-- USubst ---------------------------------------------------------------------------------------------

class USubst t where
    usubst                          :: t -> TypeM t
    usubstWith                      :: IntMap Type -> t -> t

    usubst x                        = do s <- usubstitution
                                         return $ if IntMap.null s then x else usubstWith s x

instance USubst a => USubst (Name,a) where
    usubstWith s (n, t)             = (n, usubstWith s t)

instance (USubst a, USubst b) => USubst (QName,a,b) where
    usubstWith s (n, t, u)          = (n, usubstWith s t, usubstWith s u)

instance USubst a => USubst [a] where
    usubstWith s                    = map (usubstWith s)

instance USubst a => USubst (Maybe a) where
    usubstWith s                    = maybe Nothing (\x -> Just $ usubstWith s x)

instance USubst Constraint where
    usubstWith s (Cast info env t1 t2)    = Cast (usubstWith s info) env (usubstWith s t1) (usubstWith s t2)
    usubstWith s (Sub info env w t1 t2)   = Sub (usubstWith s info) env w (usubstWith s t1) (usubstWith s t2)
    usubstWith s (Proto info env w t p)   = Proto (usubstWith s info) env w (usubstWith s t) (usubstWith s p)
    usubstWith s (Sel info env w t1 n t2) = Sel (usubstWith s info) env w (usubstWith s t1) n (usubstWith s t2)
    usubstWith s (Mut info env t1 n t2)   = Mut (usubstWith s info) env (usubstWith s t1) n (usubstWith s t2)
    usubstWith s (Seal info env t)        = Seal (usubstWith s info) env (usubstWith s t)

instance USubst ErrInfo where
    usubstWith s (DfltInfo l n mbe ts)    = DfltInfo l n (usubstWith s mbe) (usubstWith s ts)
    usubstWith s (DeclInfo l1 l2 n t msg) = DeclInfo l1 l2 n (usubstWith s t) msg
    usubstWith s info                     = info

instance USubst TSchema where
    usubstWith s (TSchema l [] t)         = TSchema l [] (usubstWith s t)
    usubstWith s (TSchema l q t)          = TSchema l (usubstWith s q) (usubstWith s t)

instance USubst TCon where
    usubstWith s (TC n ts)          = TC n (map (usubstWith s) ts)

instance USubst Type where
    usubstWith s (TUni l u)         = case IntMap.lookup (uvid u) s of
                                           Just t  -> usubstWith s t
                                           Nothing -> TUni l u
    usubstWith s (TVar l v)         = TVar l v
    usubstWith s (TCon l c)         = TCon l (usubstWith s c)
    usubstWith s (TFun l fx p k t)  = TFun l (usubstWith s fx) (usubstWith s p) (usubstWith s k) (usubstWith s t)
    usubstWith s (TTuple l p k)     = TTuple l (usubstWith s p) (usubstWith s k)
    usubstWith s (TOpt l t)         = case usubstWith s t of
                                           t'@TOpt{} -> t'
                                           t'        -> TOpt l t'
    usubstWith s (TNone l)          = TNone l
    usubstWith s (TWild l)          = TWild l
    usubstWith s (TNil l k)         = TNil l k
    usubstWith s (TRow l k n t r)   = TRow l k n (usubstWith s t) (usubstWith s r)
    usubstWith s (TStar l k r)      = TStar l k (usubstWith s r)
    usubstWith s (TFX l fx)         = TFX l fx

instance USubst QBind where
    usubstWith s (QBind v cs)       = QBind v (usubstWith s cs)

instance USubst WTCon where
    usubstWith s (wpath, p)         = (wpath, usubstWith s p)

instance USubst PosPar where
    usubstWith s (PosPar n t e p)   = PosPar n (usubstWith s t) (usubstWith s e) (usubstWith s p)
    usubstWith s (PosSTAR n t)      = PosSTAR n (usubstWith s t)
    usubstWith s PosNIL             = PosNIL

instance USubst KwdPar where
    usubstWith s (KwdPar n t e p)   = KwdPar n (usubstWith s t) (usubstWith s e) (usubstWith s p)
    usubstWith s (KwdSTAR n t)      = KwdSTAR n (usubstWith s t)
    usubstWith s KwdNIL             = KwdNIL

instance USubst Decl where
    usubstWith s (Def l n q p k a ss de fx doc)   = Def l n (usubstWith s q) (usubstWith s p) (usubstWith s k) (usubstWith s a) (usubstWith s ss) de (usubstWith s fx) doc
    usubstWith s (Actor l n q p k ss doc)         = Actor l n (usubstWith s q) (usubstWith s p) (usubstWith s k) (usubstWith s ss) doc
    usubstWith s (Class l n q bs ss doc)          = Class l n (usubstWith s q) (usubstWith s bs) (usubstWith s ss) doc
    usubstWith s (Protocol l n q bs ss doc)       = Protocol l n (usubstWith s q) (usubstWith s bs) (usubstWith s ss) doc
    usubstWith s (Extension l q c bs ss doc)      = Extension l (usubstWith s q) (usubstWith s c) (usubstWith s bs) (usubstWith s ss) doc

instance USubst Stmt where
    usubstWith s (Expr l e)               = Expr l (usubstWith s e)
    usubstWith s (Assign l ps e)          = Assign l (usubstWith s ps) (usubstWith s e)
    usubstWith s (MutAssign l t e)        = MutAssign l (usubstWith s t) (usubstWith s e)
    usubstWith s (AugAssign l t op e)     = AugAssign l (usubstWith s t) op (usubstWith s e)
    usubstWith s (Assert l e mbe)         = Assert l (usubstWith s e) (usubstWith s mbe)
    usubstWith s (Delete l t)             = Delete l (usubstWith s t)
    usubstWith s (Return l mbe)           = Return l (usubstWith s mbe)
    usubstWith s (Raise l e)              = Raise l (usubstWith s e)
    usubstWith s (If l bs els)            = If l (usubstWith s bs) (usubstWith s els)
    usubstWith s (While l e b els)        = While l (usubstWith s e) (usubstWith s b) (usubstWith s els)
    usubstWith s (For l p e b els)        = For l (usubstWith s p) (usubstWith s e) (usubstWith s b) (usubstWith s els)
    usubstWith s (Try l b hs els fin)     = Try l (usubstWith s b) (usubstWith s hs) (usubstWith s els) (usubstWith s fin)
    usubstWith s (With l is b)            = With l (usubstWith s is) (usubstWith s b)
    usubstWith s (VarAssign l ps e)       = VarAssign l (usubstWith s ps) (usubstWith s e)
    usubstWith s (After l e e')           = After l (usubstWith s e) (usubstWith s e')
    usubstWith s (Decl l ds)              = Decl l (usubstWith s ds)
    usubstWith s (Signature l ns tsc d)   = Signature l ns (usubstWith s tsc) d
    usubstWith s stmt                     = stmt

instance USubst Expr where
    usubstWith s (Call l e p k)       = Call l (usubstWith s e) (usubstWith s p) (usubstWith s k)
    usubstWith s (TApp l e ts)        = TApp l (usubstWith s e) (usubstWith s ts)
    usubstWith s (Let l ss e)         = Let l (usubstWith s ss) (usubstWith s e)
    usubstWith s (Async l e)          = Async l (usubstWith s e)
    usubstWith s (Await l e)          = Await l (usubstWith s e)
    usubstWith s (Index l e ix)       = Index l (usubstWith s e) (usubstWith s ix)
    usubstWith s (Slice l e sl)       = Slice l (usubstWith s e) (usubstWith s sl)
    usubstWith s (Cond l e1 cond e2)  = Cond l (usubstWith s e1) (usubstWith s cond) (usubstWith s e2)
    usubstWith s (IsInstance l e c)   = IsInstance l (usubstWith s e) c
    usubstWith s (BinOp l e1 op e2)   = BinOp l (usubstWith s e1) op (usubstWith s e2)
    usubstWith s (CompOp l e ops)     = CompOp l (usubstWith s e) (usubstWith s ops)
    usubstWith s (UnOp l op e)        = UnOp l op (usubstWith s e)
    usubstWith s (Dot l e n)          = Dot l (usubstWith s e) n
    usubstWith s (Rest l e n)         = Rest l (usubstWith s e) n
    usubstWith s (DotI l e i)         = DotI l (usubstWith s e) i
    usubstWith s (RestI l e i)        = RestI l (usubstWith s e) i
    usubstWith s (Lambda l p k e fx)  = Lambda l (usubstWith s p) (usubstWith s k) (usubstWith s e) (usubstWith s fx)
    usubstWith s (Yield l e)          = Yield l (usubstWith s e)
    usubstWith s (YieldFrom l e)      = YieldFrom l (usubstWith s e)
    usubstWith s (Tuple l p k)        = Tuple l (usubstWith s p) (usubstWith s k)
    usubstWith s (List l es)          = List l (usubstWith s es)
    usubstWith s (ListComp l e c)     = ListComp l (usubstWith s e) (usubstWith s c)
    usubstWith s (Dict l as)          = Dict l (usubstWith s as)
    usubstWith s (DictComp l a c)     = DictComp l (usubstWith s a) (usubstWith s c)
    usubstWith s (Set l es)           = Set l (usubstWith s es)
    usubstWith s (SetComp l e c)      = SetComp l (usubstWith s e) (usubstWith s c)
    usubstWith s (Paren l e)          = Paren l (usubstWith s e)
    usubstWith s e                    = e

instance USubst Pattern where
    usubstWith s (PWild l t)          = PWild l (usubstWith s t)
    usubstWith s (PVar l n t)         = PVar l n (usubstWith s t)
    usubstWith s (PParen l p)         = PParen l (usubstWith s p)
    usubstWith s (PTuple l p k)       = PTuple l (usubstWith s p) (usubstWith s k)
    usubstWith s (PList l ps p)       = PList l (usubstWith s ps) (usubstWith s p)

instance USubst PosPat where
    usubstWith s (PosPat p pp)        = PosPat (usubstWith s p) (usubstWith s pp)
    usubstWith s (PosPatStar p)       = PosPatStar (usubstWith s p)
    usubstWith s PosPatNil            = PosPatNil

instance USubst KwdPat where
    usubstWith s (KwdPat n p kp)      = KwdPat n (usubstWith s p) (usubstWith s kp)
    usubstWith s (KwdPatStar p)       = KwdPatStar (usubstWith s p)
    usubstWith s KwdPatNil            = KwdPatNil

instance USubst Branch where
    usubstWith s (Branch e b)         = Branch (usubstWith s e) (usubstWith s b)

instance USubst Handler where
    usubstWith s (Handler ex b)       = Handler ex (usubstWith s b)

instance USubst WithItem where
    usubstWith s (WithItem e p)       = WithItem (usubstWith s e) (usubstWith s p)

instance USubst PosArg where
    usubstWith s (PosArg e p)         = PosArg (usubstWith s e) (usubstWith s p)
    usubstWith s (PosStar e)          = PosStar (usubstWith s e)
    usubstWith s PosNil               = PosNil

instance USubst KwdArg where
    usubstWith s (KwdArg n e k)       = KwdArg n (usubstWith s e) (usubstWith s k)
    usubstWith s (KwdStar e)          = KwdStar (usubstWith s e)
    usubstWith s KwdNil               = KwdNil

instance USubst Assoc where
    usubstWith s (Assoc k v)          = Assoc (usubstWith s k) (usubstWith s v)
    usubstWith s (StarStar e)         = StarStar (usubstWith s e)

instance USubst Elem where
    usubstWith s (Elem e)             = Elem (usubstWith s e)
    usubstWith s (Star e)             = Star (usubstWith s e)

instance USubst Comp where
    usubstWith s (CompFor l p e c)    = CompFor l (usubstWith s p) (usubstWith s e) (usubstWith s c)
    usubstWith s (CompIf l e c)       = CompIf l (usubstWith s e) (usubstWith s c)
    usubstWith s NoComp               = NoComp

instance USubst Sliz where
    usubstWith s (Sliz l e1 e2 e3)    = Sliz l (usubstWith s e1) (usubstWith s e2) (usubstWith s e3)

instance USubst OpArg where
    usubstWith s (OpArg op e)         = OpArg op (usubstWith s e)


instance USubst NameInfo where
    usubstWith s (NVar t)             = NVar (usubstWith s t)
    usubstWith s (NSVar t)            = NSVar (usubstWith s t)
    usubstWith s (NDef t d doc)       = NDef (usubstWith s t) d doc
    usubstWith s (NSig t d doc)       = NSig (usubstWith s t) d doc
    usubstWith s (NAct q p k te doc)  = NAct (usubstWith s q) (usubstWith s p) (usubstWith s k) (usubstWith s te) doc
    usubstWith s (NClass q us te doc) = NClass (usubstWith s q) (usubstWith s us) (usubstWith s te) doc
    usubstWith s (NProto q us te doc) = NProto (usubstWith s q) (usubstWith s us) (usubstWith s te) doc
    usubstWith s (NExt q c ps te opts doc) = NExt (usubstWith s q) (usubstWith s c) (usubstWith s ps) (usubstWith s te) opts doc
    usubstWith s (NTVar k c ps)       = NTVar k (usubstWith s c) (usubstWith s ps)
    usubstWith s (NAlias qn)          = NAlias qn
    usubstWith s (NMAlias m)          = NMAlias m
    usubstWith s (NModule ms te doc)  = NModule ms te doc       -- actually usubstWith s te, but te has no free variables (top-level)
    usubstWith s NReserved            = NReserved

instance USubst Witness where
    usubstWith s w@WClass{}           = w                               -- A WClass (i.e., an extension) can't have any free type variables
    usubstWith s w@WInst{}            = w{ wtype  = usubstWith s (wtype w), proto = usubstWith s (proto w) }


instance USubst Env where
    usubstWith s env                  = let ne = usubstWith s (activeNames env)
                                            ex = usubstWith s (envX env)
                                            ae = HashMap.filterWithKey keepQualifiedAttrEnv (attrEnvs env)
                                        in setActiveNames ne env{ envX = ex, attrEnvs = ae }

keepQualifiedAttrEnv                  :: QName -> [(Name,(WPath,NameInfo))] -> Bool
keepQualifiedAttrEnv GName{} _        = True
keepQualifiedAttrEnv _ _              = False

instance UFree Env where
    ufree env                   = ufree (activeNames env) ++ ufree (envX env)


-- Well-formed tycon applications -------------------------------------------------------------------------------------------------

class WellFormed a where
    wf                      :: Env -> a -> Constraints

instance (WellFormed a) => WellFormed (Maybe a) where
    wf env                  = maybe [] (wf env)

instance (WellFormed a) => WellFormed [a] where
    wf env                  = concatMap (wf env)

instance WellFormed TCon where
    wf env (TC n ts)        = wf env ts ++ [ constr (vsubst s u) (vsubst s $ tVar v) | QBind v us <- q, u <- us ]
      where q               = case findQName n env of
                                NAct q p k te _ -> q
                                NClass q us te _ -> q
                                NProto q us te _ -> q
                                NReserved -> nameReserved n
                                i -> err1 n ("wf: Class or protocol name expected, got " ++ show i)
            s               = qbound q `zip` ts
            constr u t      = if isProto env (tcname u)
                              then Proto (noinfo 20) env nWild t u
                              else Cast (noinfo 21) env t (tCon u)

wfProto                     :: Env -> TCon -> TypeM (Constraints, Constraints)
wfProto env (TC n ts)       = do cs <- instQuals env q ts
                                 return (wf env ts, cs)
  where q                   = case findQName n env of
                                NProto q us te _ -> q
                                NReserved -> nameReserved n
                                i -> err1 n ("wfProto: Protocol name expected, got " ++ show i)

instance WellFormed Type where
    wf env (TCon _ tc)      = wf env tc
    wf env (TFun _ x p k t) = wf env x ++ wf env p ++ wf env p ++ wf env k ++ wf env t
    wf env (TTuple _ p k)   = wf env p ++ wf env k
    wf env (TOpt _ t)       = wf env t
    wf env (TRow _ _ _ t r) = wf env t ++ wf env r
    wf env (TStar _ _ r)    = wf env r
    wf env _                = []


instance WellFormed QBind where
    wf env (QBind v us)
      | not $ null ideps    = err2 (head ideps) "Interdependent type variable bounds:"
      | otherwise           = wf env us
      where (_,ps)          = mro2 env us
            ideps           = [ [tcname u, root $ head w] | u <- us, (w,p) <- ps, length w > 1, p == u ]
            root (Left n)   = n
            root (Right n)  = n



-- Instantiation -------------------------------------------------------------------------------------------------------------------

instantiate                 :: Env -> TSchema -> TypeM (Constraints, [Type], Type)
instantiate env (TSchema _ q t)
                            = do (cs, tvs) <- instQBinds env q
                                 let s = qbound q `zip` tvs
                                 return (cs, tvs, vsubst s t)

instQBinds                  :: Env -> QBinds -> TypeM (Constraints, [Type])
instQBinds env q            = do ts <- newUnivars env [ tvkind v | QBind v _ <- q ]
                                 cs <- instQuals env q ts
                                 return (cs, ts)

instWitness                 :: Env -> PCon -> Witness -> TypeM (Constraints,Type,Expr)
instWitness env p0 wit      = case wit of
                                 WClass q t p w ws opts -> do
                                    (cs,tvs) <- instQBinds env q
                                    let s = (tvSelf,t) : qbound q `zip` tvs
                                    unifyM (locinfo p0 22) (tcargs p0) (tcargs $ vsubst s p)
                                    t <- usubst (vsubst s t)
                                    cs <- usubst cs
                                    return (cs, t, wexpr ws (eCall (tApp (eQVar w) tvs) (wvars cs ++ replicate opts eNone)))
                                 WInst q t p w ws -> do
                                    (cs,tvs) <- instQBinds env q
                                    let s = (tvSelf,t) : qbound q `zip` tvs
                                    unifyM (locinfo p0 23) (tcargs p0) (tcargs $ vsubst s p)
                                    t <- usubst (vsubst s t)
                                    return (cs, t, wexpr ws (eQVar w))

instQuals                   :: Env -> QBinds -> [Type] -> TypeM Constraints
instQuals env q ts          = do let s = qbound q `zip` ts
                                 sequence [ constr (vsubst s (tVar v)) (vsubst s u) | QBind v us <- q, u <- us ]
  where constr t u@(TC n _)
          | isProto env n   = do w <- newWitness; return $ Proto (noinfo 24) env w t u
          | otherwise       = return $ Cast (noinfo 25) env t (tCon u)

wvars                       :: Constraints -> [Expr]
wvars cs                    = [ eVar v | Proto _ _ v _ _ <- cs ]


-- Equations -----------------------------------------------------------------------------------------------------------------------

data Equation                           = Eqn Int Name Type Expr

type Equations                          = [Equation]

mkEqn env                               = Eqn (qlevel env)

instance Pretty Equations where
    pretty eqs                          = vcat $ map pretty eqs

instance Pretty Equation where
    pretty (Eqn i n t e)                = pretty n <+> colon <+> pretty t <+> equals <+> pretty e <+> text ("# level " ++ show i)

instance USubst Equation where
    usubstWith s (Eqn i w t e)          = Eqn i w (usubstWith s t) (usubstWith s e)

instance UFree Equation where
    ufree (Eqn i w t e)                 = ufree t ++ ufree e

instance Vars Equation where
    free (Eqn i w t e)                  = free e

    bound (Eqn i w t e)                 = [w]


bindWits eqs
  | null sigws                          = binds
  | otherwise                           = Signature NoLoc sigws (monotype tWild) NoDec : binds
  where sigws                           = [ w | Eqn _ w _ (NotImplemented _) <- eqs ]
        binds                           = [ sAssign (pVar w t) e | Eqn _ w t e <- eqs, w `notElem` sigws ]

scopedWits env0 q cs                    = scoped cs
  where level1                          = qlevel env0 + length q
        scoped (Sub _ env w _ _ : cs)
          | qlevel env == level1        = w : scoped cs
          | qlevel env < level1         = trace ("### Bad constraint level for " ++ prstr w) $ scoped cs
        scoped (Proto _ env w _ _ : cs)
          | qlevel env == level1        = w : scoped cs
          | qlevel env < level1         = trace ("### Bad constraint level for " ++ prstr w) $ scoped cs
        scoped (Sel _ env w _ _ _ : cs)
          | qlevel env == level1        = w : scoped cs
          | qlevel env < level1         = trace ("### Bad constraint level for " ++ prstr w) $ scoped cs
        scoped (_ : cs)                 = scoped cs
        scoped []                       = []

findeqns [] eqns                        = []
findeqns ws eqns                        = findeqns ws' eqns ++ match
  where match                           = [ eq | eq@(Eqn _ w t e) <- eqns, w `elem` ws ]
        ws'                             = filter isWitness $ free match

spliteqns eqns                          = partition isTop eqns
  where isTop (Eqn 0 _ _ _)             = True
        isTop _                         = False


-- Misc. ---------------------------------------------------------------------------------------------------------------------------

proto2type t (TC n ts)                   = tCon $ TC n (t:ts)

wit2row ws                              = \p -> foldr f p ws
  where f (w,t)                         = TRow NoLoc PRow nWild t

wit2arg ws                              = \p -> foldr f p ws
  where f (w,t)                         = PosArg (eVar w)

wit2par ws                              = \p -> foldr f p ws
  where f (w,t)                         = PosPar w (Just t) Nothing

var2arg xs                              = \p -> foldr f p xs
  where f x                             = PosArg (eVar x)

exp2arg es                              = \p -> foldr PosArg p es

protoWitsOf cs                          = [ eVar w | Proto _ _ w t p <- cs ]

qualWPar env q                          = wit2par (qualWits env q)

qualWRow env q                          = wit2row (qualWits env q)

qualWits env q                          = [ (tvarWit tv p, proto2type (tVar tv) p) | QBind tv ps <- q, p <- ps, isProto env (tcname p) ]

witSubst env q cs                       = [ mkEqn env w0 t (eVar w) | ((w,t),w0) <- ws `zip` ws0 ]
  where ws                              = [ (w, proto2type t p) | Proto _ _ w t p <- cs ]
        ws0                             = [ tvarWit tv p | QBind tv ps <- q, p <- ps, isProto env (tcname p) ]

-- Type errors ---------------------------------------------------------------------------------------------------------------------

data TypeError                      = TypeError SrcLoc String
                                    | RigidVariable TVar
                                    | InfiniteType TUni Type
                                    | ConflictingRow TUni
                                    | KwdNotFound ErrInfo Name
                                    | KwdUnexpected ErrInfo Name
                                    | PosElemNotFound ErrInfo String
                                    | IncompatError ErrInfo String
                                    | EscapingVar [TVar] TSchema
                                    | NoSelStatic Name TCon
                                    | NoSelInstByClass Name TCon
                                    | NoMut Name
                                    | LackSig Name
                                    | LackDef Name
                                    | SurplusRow PosRow
                                    | NoRed Constraint
                                    | NoSolve (Maybe Type) [Type] [Constraint]
                                    | NoUnify ErrInfo Type Type
                                    | UninitializedAttribute SrcLoc Name Bool SrcLoc SrcLoc Name (Maybe (Name, SrcLoc)) -- attr loc, attr name, is inferred, init loc, class loc, class name, parent class info
                                    deriving (Show)

data ErrInfo    = DfltInfo {errloc :: SrcLoc, errno :: Int, errexpr :: Maybe Expr, errinsts :: [(QName,TSchema,Type)]}
                | DeclInfo {errloc :: SrcLoc, errloc2 :: SrcLoc, errname :: Name, errschema :: TSchema, errmsg :: String}
                | Simple {errloc ::SrcLoc, errmsg :: String}
                deriving (Show)

noinfo n        = DfltInfo NoLoc n Nothing []

locinfo x n     = DfltInfo (loc x) n Nothing []

locinfo' x n e  = DfltInfo (loc x) n (Just e) []

locinfo2 n e    = DfltInfo (loc e) n (Just e) []


instance Control.Exception.Exception TypeError

instance HasLoc TypeError where
    loc (TypeError l str)           = l
    loc (RigidVariable tv)          = loc tv
    loc (InfiniteType tv t)         = loc t
    loc (ConflictingRow tv)         = loc tv
    loc (KwdNotFound _ n)           = loc n
    loc (KwdUnexpected _ n)         = loc n
    loc (PosElemNotFound info s)    = loc info -- NoLoc     -- TODO: supply position
    loc (EscapingVar tvs t)         = loc tvs
    loc (NoSelStatic n u)           = loc n
    loc (NoSelInstByClass n u)      = loc n
    loc (NoMut n)                   = loc n
    loc (LackSig n)                 = loc n
    loc (LackDef n)                 = loc n
    loc (SurplusRow p)              = NoLoc     -- TODO: supply position
    loc (NoRed c)                   = loc c
    loc (NoSolve _ _ _)             = NoLoc
    loc (NoUnify info t1 t2)        = loc info
    loc (UninitializedAttribute l _ _ _ _ _ _) = l

instance HasLoc ErrInfo where
    loc (Simple l _)                = l
    loc (DfltInfo l _ _ _)          = l
    loc (DeclInfo l _ _ _ _)        = l

instance UFree ErrInfo where
    ufree (DfltInfo l n mbe ts)     = ufree mbe ++ ufree ts
    ufree (DeclInfo l1 l2 n t msg)  = ufree t
    ufree _                         = []
    
instance UWild ErrInfo where
    uwild (DfltInfo l n mbe ts)     = DfltInfo l n mbe (uwild ts)
    uwild (DeclInfo l1 l2 n t msg)  = DeclInfo l1 l2 n (uwild t) msg
    uwild info                      = info


intro t mbe                            = case mbe of
                                             Nothing ->  pretty t
                                             Just e ->   text "The type of the indicated expression" <+> text "(" Pretty.<>
                                                           (if isGen t then text "which we call" else text "inferred to be") <+> pretty t Pretty.<> text ")"
   where isGen (TCon _ (TC (NoQ (Name _ ('t' : ds))) [])) = all isDigit ds
         isGen _ = False

explainRequirement c                = case info c of
                                          Simple l s -> text s
                                          DfltInfo l n mbe ts ->
                                             (if ts /= []
                                              then text (concatMap (\(n,s,t) -> Pretty.print n ++ " has had its polymorphic type "
                                                            ++  Pretty.print s ++ " instantiated to " ++ Pretty.print t) ts++", so ")

                                              else empty) Pretty.<>
                                               (case c of
                                                   Cast _ _ t1 t2 -> intro t1 mbe <+> text "must be a subclass of" <+> pretty t2
                                                   Sub i _ _ t1 t2 -> intro t1 mbe <+> text "must be a subtype of" <+> pretty t2
                                                   Proto _ _ _ t p -> intro t mbe <+> text "must implement" <+> pretty p
                                                   Sel _ _ _ t n t0 -> intro t mbe <+> text "must have an attribute" <+> pretty n <+> text "with type" <+> pretty t0
                                                                          Pretty.<> text "; no such type is known."
                                                   _ -> pretty c <+> text "must hold")
                                          DeclInfo _ _ n sc msg -> text msg   -- $+$ pretty n <+> text "is inferred to have type"<+> pretty sc



useless vs c                           = case c of
                                             Cast _ _ t1 t2 -> f t1 || f t2
                                             Sub _ _ _ t1 t2 -> f t1 || f t2
                                             Proto _ _ _ t p -> f t
                                             Sel _ _ _ t n t0 -> f t || f t0
                                             Mut _ _ t1 n t2 -> True   -- TODO
                                             Seal _ _ _ -> True        -- TODO
     where f (TUni _ v) = notElem v vs
           f _          = False

--typeReport :: TypeError -> Report
typeReport (TypeError l msg) filename src           = Err Nothing msg [(locToPosition l filename src, This msg)] []
typeReport (RigidVariable tv) filename src          = Err Nothing msg [(locToPosition (loc tv) filename src, This msg)] []
                                                      where msg = render (text "Type" <+> pretty tv <+> text "is rigid")
typeReport (InfiniteType tv t) filename src         = Err Nothing msg [(locToPosition (loc t) filename src, This msg)] []
                                                      where msg = render (text "Type" <+> pretty tv <+> text "~" <+> pretty t <+> text "is infinite")
typeReport (ConflictingRow tv) filename src         = Err Nothing msg [(locToPosition (loc tv) filename src, This msg)] []
                                                      where msg = render (text "Type" <+> pretty tv <+> text "has conflicting extensions")
typeReport (KwdNotFound info n) filename src        = Err Nothing "Keyword argument missing" [(locToPosition (loc n) filename src, This msg)] []
                                                      where msg = render (text "Keyword element" <+> quotes (pretty n) <+> text "is not found")
typeReport (KwdUnexpected info n) filename src      = Err Nothing "Unexpected keyword argument" [(locToPosition (loc n) filename src, This msg)] []
                                                      where msg = render (text "Unexpected keyword argument" <+> quotes (pretty n))
typeReport (PosElemNotFound info s) filename src    = Err Nothing s [(locToPosition (loc info) filename src, This s)] []
typeReport (EscapingVar tvs t) filename src         = Err Nothing msg [(locToPosition (loc tvs) filename src, This msg)] []
                                                      where msg = render (text "Type annotation" <+> pretty t <+> text "is too general, type variable" <+>
                                                                  pretty (head tvs) <+> text "escapes")
typeReport (NoSelStatic n u) filename src           = Err Nothing msg [(locToPosition (loc n) filename src, This msg)] []
                                                      where msg = render (text "Static method" <+> pretty n <+> text "cannot be selected from" <+> pretty u <+> text "instance")
typeReport (NoSelInstByClass n u) filename src      = Err Nothing msg [(locToPosition (loc n) filename src, This msg)] []
                                                      where msg = render (text "Instance attribute" <+> pretty n <+> text "cannot be selected from class" <+> pretty u)
typeReport (NoMut n) filename src                   = Err Nothing msg [(locToPosition (loc n) filename src, This msg)] []
                                                      where msg = render (text "Non @property attribute" <+> pretty n <+> text "cannot be mutated")
typeReport (LackSig n) filename src                 = Err Nothing msg [(locToPosition (loc n) filename src, This msg)] []
                                                      where msg = render (text "Declaration lacks accompanying signature")
typeReport (LackDef n) filename src                 = Err Nothing msg [(locToPosition (loc n) filename src, This msg)] []
                                                      where msg = render (text "Signature lacks accompanying definition")
typeReport (NoRed c) filename src
    | DeclInfo l1 l2 n _ _ <- info c = Err
                                         Nothing
                                         "Constraint violation"
                                         [ (locToPosition l1 filename src, This (render (explainRequirement c <+> parens (explainRequirement c{info = dummyInfo}))))
                                         , (locToPosition l2 filename src, Where (Pretty.print n ++ " is defined here"))
                                         ]
                                         []
    | otherwise                      = Err
                                          Nothing
                                          "Constraint violation"
                                          [(locToPosition (loc c) filename src, This (render (explainRequirement c)))]
                                          []

typeReport (NoSolve mbt vs cs) filename src         =
    let header = case length cs of
                    0 -> "Unable to give good error message: please report example"
                    1 -> "Cannot satisfy the following constraint:"
                    _ -> "Cannot satisfy the following simultaneous constraints for the unknown " ++
                         (if length vs == 1
                          then "type " ++ case head vs of
                                          TCon _ tc -> nameStr (noq (tcname tc))
                                          _ -> show (head vs)
                          else "types")
        -- Each constraint gets its own complete error message with source line
        constraint_messages = concatMap (typeError . NoRed) cs
        -- Filter out empty positions and merge their messages into the first real position
        (noLocs, withLocs) = partition ((==NoLoc) . fst) constraint_messages
        withLocsMsgs = case (withLocs, noLocs) of
            ([], []) -> [(NoLoc, "Error: No location information")]
            ([], (l,m):_) -> [(l,m)]
            ((l,m):rest, extras) -> (l, m ++ "\n" ++ concatMap snd extras) : rest
    in Err
        Nothing
        header
        [(locToPosition l filename src, This m) | (l,m) <- withLocsMsgs]
        []
  where
        nameStr (Name _ str) = str

typeReport (NoUnify (Simple l msg) _ _) filename src = Err Nothing "Type unification error" [(locToPosition l filename src, This msg)] []
typeReport (NoUnify info t1 t2) filename src        =
    case (loc t1, loc t2) of
        (l1@Loc{}, l2@Loc{}) -> Err
                                 Nothing
                                 "Type unification error"
                                 [ (locToPosition l1 filename src, This "First type appears here")
                                 , (locToPosition l2 filename src, This "Second type appears here")
                                 ]
                                 []
        _                     -> Err
                                 Nothing
                                 "Type unification error"
                                 [(locToPosition (getLoc[loc info, loc t1, loc t2]) filename src, This msg)]
                                 []
    where msg = render (text "Incompatible types" <+> pretty t1 <+> text "and" <+> pretty t2)

typeReport (IncompatError info msg) filename src    =
    case info of
        DeclInfo l1 l2 n sc msg1    -> Err
                                         Nothing
                                         "Incompatible types"
                                         [ (locToPosition l1 filename src, This msg)
                                         , (locToPosition l2 filename src, Where (Pretty.print n ++ " is defined here"))
                                         ]
                                         []
        _                           -> Err
                                         Nothing
                                         "Incompatible types"
                                         [(locToPosition (loc info) filename src, This msg)]
                                         []
typeReport (SurplusRow p) filename src =
                                    Err Nothing "Too many arguments supplied" [(locToPosition NoLoc filename src, This (prstr (label p)))] []
typeReport (UninitializedAttribute attrLoc attrName isInferred initLoc classLoc className parentInfo) filename src =
                                    Err (Just "Type error") msg
                                        ([ (locToPosition initLoc filename src, This $ "Attribute '" ++ prstr attrName ++ "' is not initialized in __init__")
                                         , (locToPosition (makeLineOnlyLoc classLoc src) filename src, Where $ "In class " ++ prstr className)
                                         ] ++
                                         (case parentInfo of
                                             Just (parentName, parentLoc) -> [(locToPosition (makeLineOnlyLoc parentLoc src) filename src, Where $ "Attribute inherited from " ++ prstr parentName)]
                                             Nothing -> []) ++
                                         [ (locToPosition attrLoc filename src, Where $ "Attribute '" ++ prstr attrName ++ "' " ++ 
                                             if isInferred then "inferred from use" else "is defined here")
                                         ])
                                        []
                                    where msg = "Attribute '" ++ prstr attrName ++ "' is not initialized in " ++ prstr className ++ ".__init__"


typeError                           :: TypeError -> [(SrcLoc, String)]
typeError (TypeError l str)          = [(l, str)]
typeError (RigidVariable tv)         = [(loc tv, render (text "Type" <+> pretty tv <+> text "is rigid"))]
typeError (InfiniteType tv t)        = [(loc tv, render (text "Type" <+> pretty tv <+> text "~" <+> pretty t <+> text "is infinite"))]
typeError (ConflictingRow tv)        = [(loc tv, render (text "Type" <+> pretty tv <+> text "has conflicting extensions"))]
typeError (KwdNotFound _ n)          = [(loc n, render (text "Keyword element" <+> quotes (pretty n) <+> text "is not found"))]
typeError (KwdUnexpected _ n)        = [(loc n, render (text "Keyword element" <+> quotes (pretty n) <+> text "is not expected"))]
typeError (PosElemNotFound info s)   = [(loc info, s)]
typeError (EscapingVar tvs t)        = [(loc tvs, render (text "Type annotation" <+> pretty t <+> text "is too general, type variable" <+>
                                        pretty (head tvs) <+> text "escapes"))]
typeError (NoSelStatic n u)          = [(loc n, render (text "Static method" <+> pretty n <+> text "cannot be selected from" <+> pretty u <+> text "instance"))]
typeError (NoSelInstByClass n u)     = [(loc n, render (text "Instance attribute" <+> pretty n <+> text "cannot be selected from class" <+> pretty u))]
typeError (NoMut n)                  = [(loc n, render (text "Non @property attribute" <+> pretty n <+> text "cannot be mutated"))]
typeError (LackSig n)                = [(loc n, render (text "Declaration lacks accompanying signature"))]
typeError (LackDef n)                = [(loc n, render (text "Signature lacks accompanying definition"))]
typeError (UninitializedAttribute attrLoc attrName isInferred initLoc classLoc className parentInfo) = [(initLoc, "attribute '" ++ prstr attrName ++ "' is not initialized in __init__ of " ++ prstr className)]
typeError (NoRed c)
    | DeclInfo l1 l2 _ _ _ <- info c = [(min l1 l2,""), (max l1 l2,render (explainRequirement c <+> parens (explainRequirement c{info = dummyInfo})))]
--    | DfltInfo l n mbe is <- info c  = [(loc c, render (explainRequirement c <+> parens (text ("errcode " ++ show n))))]
    | otherwise                      = [(loc c, render (explainRequirement c))]
typeError (NoSolve mbt vs cs)        = case length cs of
                                           0 -> [(NoLoc, "Unable to give good error message: please report example")]
                                           1 ->  (NoLoc, "Cannot satisfy the following constraint:\n") : concatMap mkReq cs
                                           _ ->  (NoLoc, "Cannot satisfy the following simultaneous constraints for the unknown "
                                                         ++ (if length vs==1 then "type " else "types ") ++ render(commaList vs)  ++":\n")
                                                : concatMap mkReq cs
         where mkReq                 = typeError . NoRed
typeError (NoUnify info t1 t2)       = case (loc t1, loc t2) of
                                          (l1@Loc{},l2@Loc{}) -> [(l1, ""),(l2,render(text "Incompatible types" <+> pretty t1 <+> text "and" <+> pretty t2))]
                                          _ ->  [(getLoc[loc info, loc t1, loc t2],render(text "Incompatible types" <+> pretty t1 <+> text "and" <+> pretty t2))]
typeError (IncompatError info msg)   = case info of
                                           DeclInfo l1 l2 f sc _ -> [(min l1 l2,""),(max l1 l2,msg)]
                                           _ -> [(loc info, msg)]



-- Error throwing functions:
tyerr x s                           = throwError $ TypeError (loc x) (s ++ " " ++ prstr x)
tyerrs xs s                         = throwError $ TypeError (loc $ head xs) (s ++ " " ++ prstrs xs)
rigidVariable tv                    = throwError $ RigidVariable tv
infiniteType tv t                   = throwError $ InfiniteType tv t
conflictingRow tv                   = throwError $ ConflictingRow tv
kwdNotFound info n                  = throwError $ incompatError info (render(text ("keyword " ++ elemSpec info) <+> quotes (pretty n) <+> text ("is missing" ++ elemSuffix info)))
kwdUnexpected info n                = throwError $ KwdUnexpected info n
escapingVar tvs t                   = throwError $ EscapingVar tvs t
noSelStatic n u                     = throwError $ NoSelStatic n u
noSelInstByClass n u                = throwError $ NoSelInstByClass n u
noMut n                             = throwError $ NoMut n
lackSig ns                          = throwError $ LackSig (head ns)
lackDef ns                          = throwError $ LackDef (head ns)
surplusRow p                        = throwError $ SurplusRow p
noRed c                             = throwError $ NoRed c
noSolve mbt vs cs                   = throwError $ NoSolve mbt vs cs
noUnify info t1 t2                  = throwError $ NoUnify info t1 t2

posElemNotFound b c n               = throwError $ incompatError (info c) ("too " ++ (if b then "few " else "many positional ") ++ elemSpec (info c) ++ elemSuffix (info c))

incompatError info msg             = case info of
                                        DeclInfo l1 l2 f sc msg1 -> IncompatError info (msg ++ Pretty.print f)
                                        _ -> IncompatError info msg

elemSpec DeclInfo{}               = "argument(s)"
elemSpec _                        = "component(s)"

elemSuffix DeclInfo{}             = " in call to "
elemSuffix _                      = " in tuple"

-- elemHint DeclInfo{}               = " Hint: The previous definition may have been implicit, using positional notation."
-- elemHint _                        = ""

dummyInfo                         = noinfo 0


--mkErrorDiagnostic :: String -> String -> Report String -> Diagnostic String
mkErrorDiagnostic filename src report =
  let diag = addFile mempty filename src
  in addReport (addFile diag filename src) report

-- | Convert internal locations to Diagnose positions
locToPosition :: SrcLoc -> String -> String -> Position
locToPosition NoLoc _ _ =
  Position (0,0) (0,0) ""  -- Empty position
locToPosition (Loc start end) filename src =
  -- Convert byte offsets to line/col positions by counting in source
  let startPos = offsetToLineCol start src
      (endLine, endCol) = offsetToLineCol end src
      -- For multi-line spans, adjust end line to match original error format
      finalEndPos = if endLine > fst startPos
                   then (endLine - 1, endCol)
                   else (endLine, endCol)
  in Position startPos finalEndPos filename

-- | Helper to convert byte offset to line/col tuple
offsetToLineCol :: Int -> String -> (Int, Int)
offsetToLineCol offset src =
  let beforeOffset = take offset src
      lines = splitLines beforeOffset
      lineNum = length lines
      colNum = if null lines
               then 1
               else (length (last lines) + 1)
  in (lineNum, colNum)
  where
    splitLines [] = [""]
    splitLines s =
      let (first, rest) = break (=='\n') s
      in first : case rest of
                  [] -> []
                  (_:rest') -> splitLines rest'

-- | Make a location that only spans the first line
-- Many of our locations, like for a class definition, span all the lines of the
-- definition. For printing error messages it's commonly more useful to just
-- point to where the definition starts rather than highlighting the whole.
makeLineOnlyLoc :: SrcLoc -> String -> SrcLoc
makeLineOnlyLoc NoLoc _ = NoLoc
makeLineOnlyLoc (Loc start _) src =
  let endOfLine = findEndOfLine start src
  in Loc start endOfLine
  where
    findEndOfLine pos s =
      let remaining = drop pos s
          lineEnd = takeWhile (/= '\n') remaining
      in pos + length lineEnd
