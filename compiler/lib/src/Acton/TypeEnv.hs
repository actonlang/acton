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

import Pretty
import Utils
import Acton.Syntax
import Acton.Env
import Acton.TypeM
import Acton.Printer
import Acton.Names
import Acton.Builtin
import Acton.Subst
import Acton.Unify

data TypeX                      = TypeX {
                                    posnames   :: [Name],
                                    context    :: EnvCtx,
                                    indecl     :: Bool,
                                    forced     :: Bool }

type Env                        = EnvF TypeX

data EnvCtx                     = CtxTop | CtxDef | CtxAct | CtxClass deriving (Eq,Show)

typeX env0                      = setX env0 TypeX{ posnames = [], context = CtxTop, indecl = False, forced = False }

instance Pretty TypeX where
    pretty _                    = empty

instance USubst TypeX where
    usubst x                    = return x
    ufree x                     = []


posdefine te env                = modX (define te env) $ \x -> x{ posnames = dom te ++ posnames x }

setInDef env                    = modX env $ \x -> x{ context = CtxDef }

setInAct env                    = modX env $ \x -> x{ context = CtxAct }

setInClass env                  = modX env $ \x -> x{ context = CtxClass }

setInDecl env                   = modX env $ \x -> x{ indecl = True }

useForce env                    = modX env $ \x -> x{ forced = True }

onTop env                       = context (envX env) == CtxTop

inDef env                       = context (envX env) == CtxDef

inAct env                       = context (envX env) == CtxAct

inClass env                     = context (envX env) == CtxClass

inDecl env                      = indecl $ envX env

isForced env                    = forced $ envX env


instance Polarity Env where
    polvars env                 = polvars pte `polcat` invvars ite
      where (pte, ite)          = span ((`elem` pvs) . fst) (names env)
            pvs                 = posnames $ envX env


-- Well-formed tycon applications -------------------------------------------------------------------------------------------------

class WellFormed a where
    wf                      :: EnvF x -> a -> Constraints

instance (WellFormed a) => WellFormed (Maybe a) where
    wf env                  = maybe [] (wf env)

instance (WellFormed a) => WellFormed [a] where
    wf env                  = concatMap (wf env)

instance WellFormed TCon where
    wf env (TC n ts)        = wf env ts ++ [ constr (vsubst s u) (vsubst s $ tVar v) | Quant v us <- q, u <- us ]
      where q               = case findQName n env of
                                NAct q p k te _ -> q
                                NClass q us te _ -> q
                                NProto q us te _ -> q
                                NReserved -> nameReserved n
                                i -> err1 n ("wf: Class or protocol name expected, got " ++ show i)
            s               = qbound q `zip` ts
            constr u t      = if isProto env (tcname u) then Impl (DfltInfo NoLoc 20 Nothing []) nWild t u else Cast (DfltInfo NoLoc 21 Nothing []) t (tCon u)

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
    wf env (Quant v us)     = wf env us



-- Instantiation -------------------------------------------------------------------------------------------------------------------

instantiate                 :: EnvF x -> TSchema -> TypeM (Constraints, [Type], Type)
instantiate env (TSchema _ q t)
                            = do (cs, tvs) <- instQBinds env q
                                 let s = qbound q `zip` tvs
                                 return (cs, tvs, vsubst s t)

instQBinds                  :: EnvF x -> QBinds -> TypeM (Constraints, [Type])
instQBinds env q            = do ts <- newTVars [ tvkind v | Quant v _ <- q ]
                                 cs <- instQuals env q ts
                                 return (cs, ts)

instWitness                 :: EnvF x -> PCon -> Witness -> TypeM (Constraints,Type,Expr)
instWitness env p0 wit      = case wit of
                                 WClass q t p w ws -> do
                                    (cs,tvs) <- instQBinds env q
                                    let s = (tvSelf,t) : qbound q `zip` tvs
                                    unifyM (DfltInfo (loc p0) 22 Nothing []) (tcargs p0) (tcargs $ vsubst s p)
                                    t <- usubst (vsubst s t)
                                    cs <- usubst cs
                                    return (cs, t, wexpr ws (eCall (tApp (eQVar w) tvs) $ wvars cs))
                                 WInst q t p w ws -> do
                                    (cs,tvs) <- instQBinds env q
                                    let s = (tvSelf,t) : qbound q `zip` tvs
                                    unifyM (DfltInfo (loc p0) 23 Nothing []) (tcargs p0) (tcargs $ vsubst s p)
                                    t <- usubst (vsubst s t)
                                    return (cs, t, wexpr ws (eQVar w))

instQuals                   :: EnvF x -> QBinds -> [Type] -> TypeM Constraints
instQuals env q ts          = do let s = qbound q `zip` ts
                                 sequence [ constr (vsubst s (tVar v)) (vsubst s u) | Quant v us <- q, u <- us ]
  where constr t u@(TC n _)
          | isProto env n   = do w <- newWitness; return $ Impl (DfltInfo NoLoc 24 Nothing []) w t u
          | otherwise       = return $ Cast (DfltInfo NoLoc 25 Nothing []) t (tCon u)

wvars                       :: Constraints -> [Expr]
wvars cs                    = [ eVar v | Impl _ v _ _ <- cs ]


-- Misc. ---------------------------------------------------------------------------------------------------------------------------

data Equation                           = Eqn Name Type Expr

type Equations                          = [Equation]

instance Pretty Equation where
    pretty (Eqn n t e)                  = pretty n <+> colon <+> pretty t <+> equals <+> pretty e

bindWits eqs                            = [ Assign l0 [PVar l0 w (Just t)] e | Eqn w t e <- eqs ]

impl2type t (TC n ts)                   = tCon $ TC n (t:ts)

wit2row ws                              = \p -> foldr f p ws
  where f (w,t)                         = TRow NoLoc PRow nWild t

wit2arg ws                              = \p -> foldr f p ws
  where f (w,t)                         = PosArg (eVar w)

wit2par ws                              = \p -> foldr f p ws
  where f (w,t)                         = PosPar w (Just t) Nothing

var2arg xs                              = \p -> foldr f p xs
  where f x                             = PosArg (eVar x)

exp2arg es                              = \p -> foldr PosArg p es

witsOf cs                               = [ eVar w | Impl _ w t p <- cs ]

qualWPar env q                          = wit2par (qualWits env q)

qualWRow env q                          = wit2row (qualWits env q)

qualWits env q                          = [ (tvarWit tv p, impl2type (tVar tv) p) | Quant tv ps <- q, p <- ps, isProto env (tcname p) ]

witSubst env q cs                       = [ Eqn w0 t (eVar w) | ((w,t),w0) <- ws `zip` ws0 ]
  where ws                              = [ (w, impl2type t p) | Impl _ w t p <- cs ]
        ws0                             = [ tvarWit tv p | Quant tv ps <- q, p <- ps, isProto env (tcname p) ]

