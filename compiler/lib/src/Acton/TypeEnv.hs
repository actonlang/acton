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

{-# LANGUAGE FlexibleInstances, DeriveGeneric #-}
module Acton.TypeEnv where

import Control.Monad
import qualified Control.Exception
import Control.Monad.State.Strict
import Control.Monad.Except
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Pretty
import Utils
import Acton.Syntax
import Acton.Env
import Acton.Printer
import Acton.Names
import Acton.Builtin
import Acton.Subst


data TypeX                      = TypeX {
                                    posnames   :: [Name],
                                    indecl     :: Bool,
                                    forced     :: Bool }

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
    usubst (Cast info q t1 t2)      = Cast <$> usubst info <*> usubst q <*> usubst t1 <*> usubst t2
    usubst (Sub info w q t1 t2)     = Sub <$> usubst info <*> return w <*> usubst q <*> usubst t1 <*> usubst t2
    usubst (Proto info w q t p)     = Proto <$> usubst info <*> return w <*> usubst q <*> usubst t <*> usubst p
    usubst (Sel info w q t1 n t2)   = Sel <$> usubst info <*> return w <*> usubst q <*>usubst t1 <*> return n <*> usubst t2
    usubst (Mut info q t1 n t2)     = Mut <$> usubst info <*> usubst q <*> usubst t1 <*> return n <*> usubst t2
    usubst (Seal info q t)          = Seal <$> usubst info <*> usubst q <*> usubst t
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

instance USubst Quant where
    usubst (Quant v cs)             = Quant <$> usubst v <*> usubst cs

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


instance (USubst x) => USubst (EnvF x) where
    usubst env                  = do ne <- usubst (names env)
                                     we <- usubst (witnesses env)
                                     ex <- usubst (envX env)
                                     return env{ names = ne, witnesses = we, envX = ex }

instance (UFree x) => UFree (EnvF x) where
    ufree env                   = ufree (names env) ++ ufree (witnesses env) ++ ufree (envX env)


-- Well-formed tycon applications -------------------------------------------------------------------------------------------------

class WellFormed a where
    wf                      :: EnvF x -> a -> Constraints

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
            constr u t      = if isProto env (tcname u) then Proto (DfltInfo NoLoc 20 Nothing []) nWild [] t u else Cast (DfltInfo NoLoc 21 Nothing []) [] t (tCon u)

wfProto                     :: EnvF x -> TCon -> TypeM (Constraints, Constraints)
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

instantiate                 :: EnvF x -> TSchema -> TypeM (Constraints, [Type], Type)
instantiate env (TSchema _ q t)
                            = do (cs, tvs) <- instQBinds env q
                                 let s = qbound q `zip` tvs
                                 return (cs, tvs, vsubst s t)

instQBinds                  :: EnvF x -> QBinds -> TypeM (Constraints, [Type])
instQBinds env q            = do ts <- newUnivars [ tvkind v | QBind v _ <- q ]
                                 cs <- instQuals env q ts
                                 return (cs, ts)

instWitness                 :: EnvF x -> PCon -> Witness -> TypeM (Constraints,Type,Expr)
instWitness env p0 wit      = case wit of
                                 WClass q t p w ws opts -> do
                                    (cs,tvs) <- instQBinds env q
                                    let s = (tvSelf,t) : qbound q `zip` tvs
                                    unifyM (DfltInfo (loc p0) 22 Nothing []) (tcargs p0) (tcargs $ vsubst s p)
                                    t <- usubst (vsubst s t)
                                    cs <- usubst cs
                                    return (cs, t, wexpr ws (eCall (tApp (eQVar w) tvs) (wvars cs ++ replicate opts eNone)))
                                 WInst q t p w ws -> do
                                    (cs,tvs) <- instQBinds env q
                                    let s = (tvSelf,t) : qbound q `zip` tvs
                                    unifyM (DfltInfo (loc p0) 23 Nothing []) (tcargs p0) (tcargs $ vsubst s p)
                                    t <- usubst (vsubst s t)
                                    return (cs, t, wexpr ws (eQVar w))

instQuals                   :: EnvF x -> QBinds -> [Type] -> TypeM Constraints
instQuals env q ts          = do let s = qbound q `zip` ts
                                 sequence [ constr (vsubst s (tVar v)) (vsubst s u) | QBind v us <- q, u <- us ]
  where constr t u@(TC n _)
          | isProto env n   = do w <- newWitness; return $ Proto (DfltInfo NoLoc 24 Nothing []) w [] t u
          | otherwise       = return $ Cast (DfltInfo NoLoc 25 Nothing []) [] t (tCon u)

wvars                       :: Constraints -> [Expr]
wvars cs                    = [ eVar v | Proto _ v _ _ _ <- cs ]


-- Equations -----------------------------------------------------------------------------------------------------------------------

data Equation                           = Eqn Name Type Expr
                                        | QEqn Name QBinds Equations

type Equations                          = [Equation]

eqnwit (Eqn w _ _)                      = w
eqnwit (QEqn w _ _)                     = w

eqnwits eqs                             = map eqnwit eqs

deepwits eqs                            = eqnwits eqs ++ concat [ eqnwits eq | QEqn _ _ eq <- eqs ]

instance Pretty Equations where
    pretty eqs                          = vcat $ map pretty eqs

instance Pretty Equation where
    pretty (Eqn n t e)                  = pretty n <+> colon <+> pretty t <+> equals <+> pretty e
    pretty (QEqn n q eqs)               = pretty n <+> colon <+> pretty q <+> text "=>" $+$
                                          nest 4 (pretty eqs)

bindWits eqs                            = [ sAssign (pVar w t) e | Eqn w t e <- eqs ]


-- The following two functions generate quantified witness definitions and calls, respectively.
-- See Transform.hs for the invariants that apply to these constructs.
bindTopWits env eqs0                    = map bind eqs0
  where bind (Eqn w t e)                = sAssign (pVar w t) e
        bind (QEqn w q eqs)             = sDecl [Def l0 w (stripQual q) (qualWPar env q PosNIL) KwdNIL ann body NoDec fxPure Nothing]
          where ann                     = Just $ tTupleK $ foldr krow (tNil KRow) eqs
                body                    = bindWits eqs ++ [sReturn (eTupleK $ foldr karg KwdNil eqs)]

        krow (Eqn w t e) r              = kwdRow w t r
        krow (QEqn w q eqs) r           = r

        karg (Eqn w t _) a              = KwdArg w (eVar w) a
        karg (QEqn _ _ _) a             = a

qwitRefs env w0 cs                      = refs cs
  where refs []                         = []
        refs (Sub _ w q t1 t2 : cs)     = Eqn w (tFun0 [t1] t2) (eDot e w) : refs cs
        refs (Proto _ w q t p : cs)     = Eqn w (proto2type t p) (eDot e w) : refs cs
        refs (Sel _ w q t1 n t2 : cs)   = Eqn w (tFun0 [t1] t2) (eDot e w) : refs cs
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

protoWitsOf cs                          = [ eVar w | Proto _ w q t p <- cs ]

qualWPar env q                          = wit2par (qualWits env q)

qualWRow env q                          = wit2row (qualWits env q)

qualWits env q                          = [ (tvarWit tv p, proto2type (tVar tv) p) | QBind tv ps <- q, p <- ps, isProto env (tcname p) ]

witSubst env q cs                       = [ Eqn w0 t (eVar w) | ((w,t),w0) <- ws `zip` ws0 ]
  where ws                              = [ (w, proto2type t p) | Proto _ w q t p <- cs ]
        ws0                             = [ tvarWit tv p | QBind tv ps <- q, p <- ps, isProto env (tcname p) ]

