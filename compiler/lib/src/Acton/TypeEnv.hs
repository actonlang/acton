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
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Char
import Error.Diagnose hiding ((<>), err)
import Prelude hiding ((<>))

import Pretty
import Utils
import Acton.Syntax
import Acton.Builtin
import Acton.Printer
import Acton.Names
import Acton.NameInfo
import Acton.Subst
import Acton.Env


data TypeX                      = TypeX {
                                    posnames   :: [Name],
                                    indecl     :: Bool,
                                    forced     :: Bool
                                  } deriving (Show)

type Env                        = EnvF TypeX

typeX env0                      = setX env0 TypeX{ posnames = [], indecl = False, forced = False }

instance Pretty TypeX where
    pretty _                    = empty

instance USubst TypeX where
    usubst x                    = return x

instance UFree TypeX where
    ufree x                     = []


posdefine te env                = modX (define te env) $ \x -> x{ posnames = dom te ++ posnames x }

setInDecl env                   = modX env $ \x -> x{ indecl = True }

useForce env                    = modX env $ \x -> x{ forced = True }

inDecl env                      = indecl $ envX env

isForced env                    = forced $ envX env


instance Polarity Env where
    polvars env                 = polvars pte `polcat` invvars ite
      where (pte, ite)          = span ((`elem` pvs) . fst) (names env)
            pvs                 = posnames $ envX env


-- Constraints -------------------------------------------------------------------------------

data Constraint = Cast  {info :: ErrInfo, scope :: Env, type1 :: Type, type2 :: Type}
                | Sub   {info :: ErrInfo, scope :: Env, wit :: Name, type1 :: Type, type2 :: Type}
                | Proto {info :: ErrInfo, scope :: Env, wit :: Name, type1 :: Type, proto1 :: PCon}
                | Sel   {info :: ErrInfo, scope :: Env, wit :: Name, type1 :: Type, name1 :: Name, type2 :: Type}
                | Mut   {info :: ErrInfo, scope :: Env, type1 :: Type, name1 :: Name, type2 :: Type}
                | Seal  {info :: ErrInfo, scope :: Env, type1 :: Type}
                | Imply {info :: ErrInfo, wit :: Name, binder :: QBinds, scoped :: Constraints}
                deriving (Show)

type Constraints = [Constraint]

instance HasLoc Constraint where
    loc (Cast info env t1 t2)       = getLoc [loc info, loc t1, loc t2]
    loc (Sub info env _ t1 t2)      = getLoc [loc info, loc t1, loc t2]
    loc (Proto info _ env t1 _)     = getLoc [loc info, loc t1]
    loc (Sel info _ env t1  n1 t2)  = getLoc [loc info, loc t1, loc n1, loc t2]
    loc (Mut info env t1  n1 t2)    = getLoc [loc info, loc t1, loc n1, loc t2]
    loc (Seal info env t1)          = getLoc [loc info, loc t1]
    loc (Imply info _ q cs)         = getLoc [loc info, loc cs]

instance Pretty Constraint where
    pretty (Cast _ env t1 t2)       = prettyQuant env <+> pretty t1 <+> text "<" <+> pretty t2
    pretty (Sub _ env w t1 t2)      = pretty w <+> colon <+> prettyQuant env <+> pretty t1 <+> text "<" <+> pretty t2
    pretty (Proto _ env w t u)      = pretty w <+> colon <+> prettyQuant env <+> pretty t <+> parens (pretty u)
    pretty (Sel _ env w t1 n t2)    = pretty w <+> colon <+> prettyQuant env <+> pretty t1 <> text "." <> pretty n <+> text "<" <+> pretty t2
    pretty (Mut _ env t1 n t2)      = prettyQuant env <+> pretty t1 <+> text "." <> pretty n <+> text ">" <+> pretty t2
    pretty (Seal _ env t)           = prettyQuant env <+> text "$Seal" <+> pretty t
    pretty (Imply _ w q cs)
      | length cs < 4               = pretty w <+> colon <+> pretty q <+> text "=>" <+> braces (commaSep pretty cs)
      | otherwise                   = pretty w <+> colon <+> pretty q <+> text "=>" $+$ nest 4 (vcat $ map pretty cs)

prettyQuant env
  | qlevel env > 0                  = brackets (commaSep pretty q) <+> text "=>"
  | otherwise                       = empty
  where q                           = [ QBind (TV k tv) (if c == cValue then ps else c:ps) | (tv, NTVar k c ps) <- names env ]

instance UFree Constraint where
    ufree (Cast info env t1 t2)     = ufree t1 ++ ufree t2
    ufree (Sub info env w t1 t2)    = ufree t1 ++ ufree t2
    ufree (Proto info env w t p)    = ufree t ++ ufree p
    ufree (Sel info env w t1 n t2)  = ufree t1 ++ ufree t2
    ufree (Mut info env t1 n t2)    = ufree t1 ++ ufree t2
    ufree (Seal info env t)         = ufree t
    ufree (Imply info w q cs)       = ufree cs

instance Tailvars Constraint where
    tailvars (Cast _ env t1 t2)     = tailvars t1 ++ tailvars t2
    tailvars (Sub _ env w t1 t2)    = tailvars t1 ++ tailvars t2
    tailvars (Proto _ env w t p)    = tailvars t ++ tailvars p
    tailvars (Sel _ env w t1 n t2)  = tailvars t1 ++ tailvars t2
    tailvars (Mut _ env t1 n t2)    = tailvars t1 ++ tailvars t2
    tailvars (Seal _ env t)         = tailvars t
    tailvars (Imply _ w q cs)       = tailvars cs

instance Vars Constraint where
    freeQ (Cast _ env t1 t2)        = freeQ t1 ++ freeQ t2
    freeQ (Sub _ env w t1 t2)       = freeQ t1 ++ freeQ t2
    freeQ (Proto _ env w t p)       = freeQ t ++ freeQ p
    freeQ (Sel _ env w t1 n t2)     = freeQ t1 ++ freeQ t2
    freeQ (Mut _ env t1 n t2)       = freeQ t1 ++ freeQ t2
    freeQ (Seal _ env t)            = freeQ t
    freeQ (Imply _ w q cs)          = freeQ cs

instance UWild Constraint where
    uwild (Cast info env t1 t2)     = Cast info env (uwild t1) (uwild t2)
    uwild (Sub info env w t1 t2)    = Sub info env w (uwild t1) (uwild t2)
    uwild (Proto info env w t p)    = Proto info env w (uwild t) (uwild p)
    uwild (Sel info env w t1 n t2)  = Sel info env w (uwild t1) n (uwild t2)
    uwild (Mut info env t1 n t2)    = Mut info env (uwild t1) n (uwild t2)
    uwild (Seal info env t)         = Seal info env (uwild t)
    uwild (Imply info w q cs)       = Imply info w q (uwild cs)


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
        heads (Imply _ w q cs)      = []

        deps (Proto _ w _ _ p)      = ufree p
        deps (Cast _ _ _ t)         = typarams t
        deps (Sub _ w _ _ t)        = typarams t
        deps (Sel _ w _ _ n t)      = ufree t
        deps (Mut _ _ _ n t)        = ufree t
        deps (Seal _ _ _)           = []
        deps (Imply _ w q cs)       = []

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
    boundvsboundvs pn (Mut _ _ (TUni _ v) _ t : cs)
      | v `elem` (fst pn ++ snd pn) = boundvs (invvars t `polcat` pn) cs
    bnds pn (c : cs)                = let (pn',cs') = boundvs pn cs in (pn', c:cs')


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
                                                effectstack     :: [(TFX,Type)],
                                                deferred        :: Constraints,
                                                unisubst        :: Map TUni Type
                                          }

initTypeState s                         = TypeState { nextint = 1, effectstack = [], deferred = [], unisubst = s }

type TypeM a                            = ExceptT TypeError (State TypeState) a

runTypeM                                :: TypeM a -> a
runTypeM m                              = case evalState (runExceptT m) (initTypeState Map.empty) of
                                            Right x  -> x
                                            Left err -> error ("Unhandled TypeM exception: " ++ prstr loc ++ ": " ++ prstr str)
                                              where (loc,str) : _ = typeError err

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
                                          state $ \st -> ((), st{ unisubst = Map.insert uv t (unisubst st)})

usubstitution                           :: TypeM (Map TUni Type)
usubstitution                           = lift $ state $ \st -> (unisubst st, st)


-- Name generation ------------------------------------------------------------------------------------------------------------------

newWitness                              = Internal Witness "" <$> newUnique

newTmp                                  = Internal Tempvar "" <$> newUnique

newUnivarOfKind k                       = TUni NoLoc <$> univar k <$> newUnique

newUnivarToken n                        = TUni NoLoc $ unitoken n

newUnivars ks                           = mapM newUnivarOfKind ks

newUnivar                               = newUnivarOfKind KType


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

unify' info (TVar _ tv1) (TVar _ tv2)
  | tv1 == tv2                              = return ()

unify' info (TUni _ tv1) (TUni _ tv2)
  | tv1 == tv2                              = return ()

unify' info (TUni _ uv) t2                  = do when (uv `elem` ufree t2) (infiniteType uv t2)
                                                 usubstitute uv t2
unify' info t1 (TUni _ uv)                  = do when (uv `elem` ufree t1) (infiniteType uv t1)
                                                 usubstitute uv t1

unify' info t1 t2                           = noUnify info t1 t2


-- USubst ---------------------------------------------------------------------------------------------

class USubst t where
    usubst                          :: t -> TypeM t

instance USubst a => USubst (Name,a) where
    usubst (n, t)                   = (,) <$> return n <*> usubst t

instance (USubst a, USubst b) => USubst (QName,a,b) where
    usubst (n, t, u)                = (,,) <$> return n <*> usubst t <*> usubst u

instance USubst a => USubst [a] where
    usubst                          = mapM usubst

instance USubst a => USubst (Maybe a) where
    usubst                          = maybe (return Nothing) (\x -> Just <$> usubst x)

instance USubst Constraint where
    usubst (Cast info env t1 t2)    = Cast <$> usubst info <*> return env <*> usubst t1 <*> usubst t2
    usubst (Sub info env w t1 t2)   = Sub <$> usubst info <*> return env <*> return w <*> usubst t1 <*> usubst t2
    usubst (Proto info env w t p)   = Proto <$> usubst info <*> return env <*> return w <*> usubst t <*> usubst p
    usubst (Sel info env w t1 n t2) = Sel <$> usubst info <*> return env <*> return w <*> usubst t1 <*> return n <*> usubst t2
    usubst (Mut info env t1 n t2)   = Mut <$> usubst info <*> return env <*> usubst t1 <*> return n <*> usubst t2
    usubst (Seal info env t)        = Seal <$> usubst info <*> return env <*> usubst t
    usubst (Imply info w q cs)      = Imply <$> usubst info <*> return w <*> usubst q <*> usubst cs

instance USubst ErrInfo where
    usubst (DfltInfo l n mbe ts)    = DfltInfo l n <$> usubst mbe <*> usubst ts
    usubst (DeclInfo l1 l2 n t msg) = DeclInfo l1 l2 n <$> usubst t <*> return msg
    usubst info                     = return info

instance USubst TSchema where
    usubst (TSchema l [] t)         = TSchema l [] <$> usubst t
    usubst (TSchema l q t)          = TSchema l <$> usubst q <*> usubst t

instance USubst TVar where
    usubst v                        = do t <- usubst (TVar NoLoc v)
                                         case t of
                                            TVar _ v' -> return v'
                                            _         -> return v

instance USubst TCon where
    usubst (TC n ts)                = TC n <$> usubst ts

instance USubst QBind where
    usubst (QBind v cs)             = QBind <$> usubst v <*> usubst cs

instance USubst WTCon where
    usubst (wpath, p)               = do p <- usubst p; return (wpath, p)

instance USubst Type where
    usubst (TUni l u)               = do s <- usubstitution
                                         case Map.lookup u s of
                                            Just t  -> usubst t
                                            Nothing -> return (TUni l u)
    usubst (TVar l v)               = return $ TVar l v
    usubst (TCon l c)               = TCon l <$> usubst c
    usubst (TFun l fx p k t)        = TFun l <$> usubst fx <*> usubst p <*> usubst k <*> usubst t
    usubst (TTuple l p k)           = TTuple l <$> usubst p <*> usubst k
    usubst (TOpt l t)               = TOpt l <$> usubst t
    usubst (TNone l)                = return $ TNone l
    usubst (TWild l)                = return $ TWild l
    usubst (TNil l s)               = return $ TNil l s
    usubst (TRow l k n t r)         = TRow l k n <$> usubst t <*> usubst r
    usubst (TStar l k r)            = TStar l k <$> usubst r
    usubst (TFX l fx)               = return $ TFX l fx

instance USubst PosPar where
    usubst (PosPar n t e p)         = PosPar n <$> usubst t <*> usubst e <*> usubst p
    usubst (PosSTAR n t)            = PosSTAR n <$> usubst t
    usubst PosNIL                   = return PosNIL

instance USubst KwdPar where
    usubst (KwdPar n t e p)         = KwdPar n <$> usubst t <*> usubst e <*> usubst p
    usubst (KwdSTAR n t)            = KwdSTAR n <$> usubst t
    usubst KwdNIL                   = return KwdNIL

instance USubst Decl where
    usubst (Def l n q p k a ss de fx doc)   = Def l n <$> usubst q <*> usubst p <*> usubst k <*> usubst a <*> usubst ss <*> return de <*> usubst fx <*> return doc
    usubst (Actor l n q p k ss doc)         = Actor l n <$> usubst q <*> usubst p <*> usubst k <*> usubst ss <*> return doc
    usubst (Class l n q bs ss doc)          = Class l n <$> usubst q <*> usubst bs <*> usubst ss <*> return doc
    usubst (Protocol l n q bs ss doc)       = Protocol l n <$> usubst q <*> usubst bs <*> usubst ss <*> return doc
    usubst (Extension l q c bs ss doc)      = Extension l <$> usubst q <*> usubst c <*> usubst bs <*> usubst ss <*> return doc

instance USubst Stmt where
    usubst (Expr l e)               = Expr l <$> usubst e
    usubst (Assign l ps e)          = Assign l <$> usubst ps <*> usubst e
    usubst (MutAssign l t e)        = MutAssign l <$> usubst t <*> usubst e
    usubst (AugAssign l t op e)     = AugAssign l <$> usubst t <*> return op <*> usubst e
    usubst (Assert l e mbe)         = Assert l <$> usubst e <*> usubst mbe
    usubst (Delete l t)             = Delete l <$> usubst t
    usubst (Return l mbe)           = Return l <$> usubst mbe
    usubst (Raise l e)              = Raise l <$> usubst e
    usubst (If l bs els)            = If l <$> usubst bs <*> usubst els
    usubst (While l e b els)        = While l <$> usubst e <*> usubst b <*> usubst els
    usubst (For l p e b els)        = For l <$> usubst p <*> usubst e <*> usubst b <*> usubst els
    usubst (Try l b hs els fin)     = Try l <$> usubst b <*> usubst hs <*> usubst els <*> usubst fin
    usubst (With l is b)            = With l <$> usubst is <*> usubst b
    usubst (VarAssign l ps e)       = VarAssign l <$> usubst ps <*> usubst e
    usubst (After l e e')           = After l <$> usubst e <*> usubst e'
    usubst (Decl l ds)              = Decl l <$> usubst ds
    usubst (Signature l ns tsc d)   = Signature l ns <$> usubst tsc <*> return d
    usubst s                        = return s

instance USubst Expr where
    usubst (Call l e p k)           = Call l <$> usubst e <*> usubst p <*> usubst k
    usubst (TApp l e ts)            = TApp l <$> usubst e <*> usubst ts
    usubst (Async l e)              = Async l <$> usubst e
    usubst (Await l e)              = Await l <$> usubst e
    usubst (Index l e ix)           = Index l <$> usubst e <*> usubst ix
    usubst (Slice l e sl)           = Slice l <$> usubst e <*> usubst sl
    usubst (Cond l e1 cond e2)      = Cond l <$> usubst e1 <*> usubst cond <*> usubst e2
    usubst (IsInstance l e c)       = IsInstance l <$> usubst e <*> return c
    usubst (BinOp l e1 op e2)       = BinOp l <$> usubst e1 <*> return op <*> usubst e2
    usubst (CompOp l e ops)         = CompOp l <$> usubst e <*> usubst ops
    usubst (UnOp l op e)            = UnOp l op <$> usubst e
    usubst (Dot l e n)              = Dot l <$> usubst e <*> return n
    usubst (Rest l e n)             = Rest l <$> usubst e <*> return n
    usubst (DotI l e i)             = DotI l <$> usubst e <*> return i
    usubst (RestI l e i)            = RestI l <$> usubst e <*> return i
    usubst (Lambda l p k e fx)      = Lambda l <$> usubst p <*> usubst k <*> usubst e <*> usubst fx
    usubst (Yield l e)              = Yield l <$> usubst e
    usubst (YieldFrom l e)          = YieldFrom l <$> usubst e
    usubst (Tuple l p k)            = Tuple l <$> usubst p <*> usubst k
    usubst (List l es)              = List l <$> usubst es
    usubst (ListComp l e c)         = ListComp l <$> usubst e <*> usubst c
    usubst (Dict l as)              = Dict l <$> usubst as
    usubst (DictComp l a c)         = DictComp l <$> usubst a <*> usubst c
    usubst (Set l es)               = Set l <$> usubst es
    usubst (SetComp l e c)          = SetComp l <$> usubst e <*> usubst c
    usubst (Paren l e)              = Paren l <$> usubst e
    usubst e                        = return e

instance USubst Pattern where
    usubst (PWild l t)              = PWild l <$> usubst t
    usubst (PVar l n t)             = PVar l n <$> usubst t
    usubst (PParen l p)             = PParen l <$> usubst p
    usubst (PTuple l p k)           = PTuple l <$> usubst p <*> usubst k
    usubst (PList l ps p)           = PList l <$> usubst ps <*> usubst p

instance USubst PosPat where
    usubst (PosPat p pp)            = PosPat <$> usubst p <*> usubst pp
    usubst (PosPatStar p)           = PosPatStar <$> usubst p
    usubst PosPatNil                = return PosPatNil

instance USubst KwdPat where
    usubst (KwdPat n p kp)          = KwdPat n <$> usubst p <*> usubst kp
    usubst (KwdPatStar p)           = KwdPatStar <$> usubst p
    usubst KwdPatNil                = return KwdPatNil

instance USubst Branch where
    usubst (Branch e b)             = Branch <$> usubst e <*> usubst b

instance USubst Handler where
    usubst (Handler ex b)           = Handler ex <$> usubst b

instance USubst WithItem where
    usubst (WithItem e p)           = WithItem <$> usubst e <*> usubst p

instance USubst PosArg where
    usubst (PosArg e p)             = PosArg <$> usubst e <*> usubst p
    usubst (PosStar e)              = PosStar <$> usubst e
    usubst PosNil                   = return PosNil

instance USubst KwdArg where
    usubst (KwdArg n e k)           = KwdArg n <$> usubst e <*> usubst k
    usubst (KwdStar e)              = KwdStar <$> usubst e
    usubst KwdNil                   = return KwdNil

instance USubst Assoc where
    usubst (Assoc k v)              = Assoc <$> usubst k <*> usubst v
    usubst (StarStar e)             = StarStar <$> usubst e

instance USubst Elem where
    usubst (Elem e)                 = Elem <$> usubst e
    usubst (Star e)                 = Star <$> usubst e

instance USubst Comp where
    usubst (CompFor l p e c)        = CompFor l <$> usubst p <*> usubst e <*> usubst c
    usubst (CompIf l e c)           = CompIf l <$> usubst e <*> usubst c
    usubst NoComp                   = return NoComp

instance USubst Sliz where
    usubst (Sliz l e1 e2 e3)        = Sliz l <$> usubst e1 <*> usubst e2 <*> usubst e3

instance USubst OpArg where
    usubst (OpArg op e)             = OpArg op <$> usubst e


instance USubst NameInfo where
    usubst (NVar t)             = NVar <$> usubst t
    usubst (NSVar t)            = NSVar <$> usubst t
    usubst (NDef t d doc)       = NDef <$> usubst t <*> return d <*> return doc
    usubst (NSig t d doc)       = NSig <$> usubst t <*> return d <*> return doc
    usubst (NAct q p k te doc)  = NAct <$> usubst q <*> usubst p <*> usubst k <*> usubst te <*> return doc
    usubst (NClass q us te doc) = NClass <$> usubst q <*> usubst us <*> usubst te <*> return doc
    usubst (NProto q us te doc) = NProto <$> usubst q <*> usubst us <*> usubst te <*> return doc
    usubst (NExt q c ps te opts doc) = NExt <$> usubst q <*> usubst c <*> usubst ps <*> usubst te <*> return opts <*> return doc
    usubst (NTVar k c ps)       = NTVar k <$> usubst c <*> usubst ps
    usubst (NAlias qn)          = NAlias <$> return qn
    usubst (NMAlias m)          = NMAlias <$> return m
    usubst (NModule te doc)     = NModule <$> return te <*> return doc     -- actually usubst te, but te has no free variables (top-level)
    usubst NReserved            = return NReserved

instance USubst Witness where
    usubst w@WClass{}           = return w                      -- A WClass (i.e., an extension) can't have any free type variables
    usubst w@WInst{}            = do t <- usubst (wtype w)
                                     p <- usubst (proto w)
                                     return w{ wtype  = t, proto = p }


instance USubst Env where
    usubst env                  = do ne <- usubst (names env)
                                     we <- usubst (witnesses env)
                                     ex <- usubst (envX env)
                                     return env{ names = ne, witnesses = we, envX = ex }

instance UFree Env where
    ufree env                   = ufree (names env) ++ ufree (witnesses env) ++ ufree (envX env)


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
instQBinds env q            = do ts <- newUnivars [ tvkind v | QBind v _ <- q ]
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
                                        | QEqn Name QBinds Equations

type Equations                          = [Equation]

mkEqn env                               = Eqn (qlevel env)

deepwits eqs                            = bound eqs ++ concat [ bound eq | QEqn _ _ eq <- eqs ]

instance Pretty Equations where
    pretty eqs                          = vcat $ map pretty eqs

instance Pretty Equation where
    pretty (Eqn i n t e)                = pretty n <+> colon <+> pretty t <+> equals <+> pretty e <+> if i>0 then text ("# "++show i) else empty
    pretty (QEqn n q eqs)               = pretty n <+> colon <+> pretty q <+> text "=>" $+$
                                          nest 4 (pretty eqs)

instance USubst Equation where
    usubst (Eqn i w t e)                = Eqn i w <$> usubst t <*> usubst e
    usubst (QEqn n q eqs)               = QEqn n <$> usubst q <*> usubst eqs

instance UFree Equation where
    ufree (Eqn i w t e)                 = ufree t ++ ufree e
    ufree (QEqn n q eqs)                = ufree q ++ ufree eqs

instance Vars Equation where
    free (Eqn i w t e)                  = free e
    free (QEqn n q eqs)                 = free q ++ (free eqs \\ bound q)

    bound (Eqn i w t e)                 = [w]
    bound (QEqn w q eqs)                = [w]


bindWits eqs
  | null sigws                          = binds
  | otherwise                           = Signature NoLoc sigws (monotype tWild) NoDec : binds
  where sigws                           = [ w | Eqn _ w _ (NotImplemented _) <- eqs ]
        binds                           = [ sAssign (pVar w t) e | Eqn _ w t e <- eqs, w `notElem` sigws ]


-- The following two functions generate quantified witness definitions and calls, respectively.
-- See Transform.hs for the invariants that apply to these constructs.
bindTopWits env eqs0                    = map bind eqs0
  where bind (Eqn _ w t e)              = sAssign (pVar w t) e
        bind (QEqn w q eqs)             = sDecl [Def l0 w (stripQual q) (qualWPar env q PosNIL) KwdNIL ann body NoDec fxPure Nothing]
          where ann                     = Just $ tTupleK $ foldr krow (tNil KRow) eqs
                body                    = bindWits eqs ++ [sReturn (eTupleK $ foldr karg KwdNil eqs)]

        krow (Eqn _ w t e) r            = kwdRow w t r
        krow (QEqn w q eqs) r           = r

        karg (Eqn _ w t _) a            = KwdArg w (eVar w) a
        karg (QEqn _ _ _) a             = a

qwitRefs env w0 cs                      = refs cs
  where refs []                         = []
        refs (Sub _ _ w t1 t2 : cs)     = mkEqn env w (tFun0 [t1] t2) (eDot e w) : refs cs
        refs (Proto _ _ w t p : cs)     = mkEqn env w (proto2type t p) (eDot e w) : refs cs
        refs (Sel _ _ w t1 n t2 : cs)   = mkEqn env w (tFun0 [t1] t2) (eDot e w) : refs cs
        refs (c : cs)                   = refs cs
        e                               = eCallP (tApp (eVar w0) (map tVar $ qbound q_tot)) (wit2arg (qualWits env q_tot) PosNil)
        q_tot                           = quantScope0 env

insertOrMerge [] eqs0                   = eqs0
insertOrMerge (eq@Eqn{}:eqs) eqs0       = eq : insertOrMerge eqs eqs0
insertOrMerge ((QEqn _ _ []):eqs) eqs0  = insertOrMerge eqs eqs0
insertOrMerge (qe:eqs) eqs0             = insertOrMerge eqs (ins qe eqs0)
  where ins (QEqn w q eq) (QEqn w' _ eq' : eqs0)
          | w == w'                     = QEqn w q (eq++eq') : eqs0
        ins qe (eq : eqs0)              = eq : ins qe eqs0
        ins qe@(QEqn w _ eq) []         = [qe]


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
                                             Imply _ _ _ _ -> True   -- TODO
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
