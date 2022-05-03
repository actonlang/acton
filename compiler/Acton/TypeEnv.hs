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
                                    context    :: EnvCtx,
                                    indecl     :: Bool,
                                    forced     :: Bool,
                                    actdefs    :: [Name],
                                    sealstatus :: [(QName,Bool)] }

type Env                        = EnvF TypeX

data EnvCtx                     = CtxTop | CtxDef | CtxAct | CtxClass deriving (Eq,Show)

typeX env0                      = setX env0 TypeX{ context = CtxTop, indecl = False, forced = False, actdefs = [], sealstatus = [] }

instance Pretty TypeX where
    pretty _                    = empty

instance Subst TypeX where
    msubst x                    = return x
    tyfree x                    = []


setInDef env
  | onTop env                   = modX env $ \x -> x{ context = CtxDef }
  | otherwise                   = env

setInAct env                    = modX env $ \x -> x{ context = CtxAct }

setInClass env                  = modX env $ \x -> x{ context = CtxClass }

setInDecl env                   = modX env $ \x -> x{ indecl = True }

useForce env                    = modX env $ \x -> x{ forced = True }

setActDefs te env
  | inAct env                   = modX env $ \x -> x{ actdefs = [ n | (n,NDef{}) <- te ] ++ actdefs x }
  | otherwise                   = env

setSealStatus ns stats env      = modX env $ \x -> x{ sealstatus = (ns `zip` stats) ++ sealstatus x }

onTop env                       = context (envX env) == CtxTop

inDef env                       = context (envX env) == CtxDef

inAct env                       = context (envX env) == CtxAct

inClass env                     = context (envX env) == CtxClass

inDecl env                      = indecl $ envX env

isForced env                    = forced $ envX env

isActDef n env                  = n `elem` actdefs (envX env)

sealStatus c env                = lookup c $ sealstatus $ envX env


-- Iterate sealed status for type constructors ------------------------------------------------------------------------------------

iterSealStatus te env           = setSealStatus qns (iter (map (const True) ns)) env
  where (ns,is)                 = unzip te
        qns                     = map NoQ ns
        iter s0
          | s0 == s1            = s0
          | otherwise           = iter s1
          where s1              = [ null $ snd $ unsealed env' info | info <- is ]
                env'            = setSealStatus qns s0 env


-- Return the leaf types that may break an actor's seal  --------------------------------------------------------------------------

unsealed env item               = partition isTVar $ f $ leaves item
  where
    f []                        = []
    f (t@(TVar _ tv) : ts)
      | univar tv               = t : f ts                      -- Must be checked again later on when instantiated
    f (t@(TFX _ x) : ts)
      | x `elem` [FXMut,FXProc] = t : f ts                      -- Definitely leaking
    f (t@(TCon _ tc) : ts)
      | castable env t tObject  = t : f ts                      -- Definitely leaking
      | Just False <- stat      = t : f ts                      -- Previously determined to leak
      | Just True <- stat       = f (tcargs tc ++ ts)           -- Not leaking, but conservatively check type arguments too
      | Nothing <- stat         = f (leaves info ++ ts)         -- Sibling type currently being defined, check its full rhs
      where stat                = sealStatus (tcname tc) env
            info                = findQName (tcname tc) env
    f (_ : ts)                  = f ts                          -- Not leaking!


-- In iterSealStatus:
--      Find leaves of NameInfo
--      Don't follow TCon names in env (can't happen, sealStatus always succeeds)
--      Return True/False on TCon/TFX safety
--      Ignore TVar leaves (can't happen, processed decls have no free vars)

-- In reduce (Seal t):
--      Find leaves of t
--      Follow TCon names (when sealStatus fails on sibling a TCon)
--      Fail on leaking TCon/TFX
--      Seal TVar leaves


-- Well-formed tycon applications -------------------------------------------------------------------------------------------------

class WellFormed a where
    wf                      :: EnvF x -> a -> Constraints

instance (WellFormed a) => WellFormed (Maybe a) where
    wf env                  = maybe [] (wf env)

instance (WellFormed a) => WellFormed [a] where
    wf env                  = concatMap (wf env)

instance WellFormed TCon where
    wf env (TC n ts)        = wf env ts ++ subst s [ constr u (tVar v) | Quant v us <- q, u <- us ]
      where q               = case findQName  n env of
                                NAct q p k te  -> q
                                NClass q us te -> q
                                NProto q us te -> q
                                NReserved -> nameReserved n
                                i -> err1 n ("wf: Class or protocol name expected, got " ++ show i)
            s               = qbound q `zip` ts
            constr u t      = if isProto env (tcname u) then Impl (name "_") t u else Cast t (tCon u)

wfProto                     :: EnvF x -> TCon -> TypeM (Constraints, Constraints)
wfProto env (TC n ts)       = do cs <- instQuals env q ts
                                 return (wf env ts, cs)
  where q                   = case findQName n env of
                                NProto q us te -> q
                                NReserved -> nameReserved n
                                i -> err1 n ("wfProto: Protocol name expected, got " ++ show i)
            
instance WellFormed Type where
    wf env (TCon _ tc)      = wf env tc
    wf env (TFun _ x p k t) = wf env x ++ wf env p ++ wf env p ++ wf env k ++ wf env t
    wf env (TTuple _ p k)   = wf env p ++ wf env k
    wf env (TOpt _ t)       = wf env t
    wf env (TRow _ _ _ t r) = wf env t ++ wf env r
    wf env _                = []


instance WellFormed QBind where
    wf env (Quant v us)     = wf env us



-- Instantiation -------------------------------------------------------------------------------------------------------------------

instantiate                 :: EnvF x -> TSchema -> TypeM (Constraints, [Type], Type)
instantiate env (TSchema _ q t)
                            = do (cs, tvs) <- instQBinds env q
                                 let s = qbound q `zip` tvs
                                 return (cs, tvs, subst s t)

instQBinds                  :: EnvF x -> QBinds -> TypeM (Constraints, [Type])
instQBinds env q            = do ts <- newTVars [ tvkind v | Quant v _ <- q ]
                                 cs <- instQuals env q ts
                                 return (cs, ts)

instWitness                 :: EnvF x -> Type -> Witness -> TypeM (Constraints,TCon,Expr)
instWitness env t0 wit      = case wit of
                                 WClass q t1 p w ws -> do
                                    (cs,tvs) <- instQBinds env q
                                    let s = (tvSelf,t0) : qbound q `zip` tvs
                                    unify t0 (subst s t1)
                                    p <- msubst (subst s p)
                                    cs <- msubst cs
                                    return (cs, p, wexpr ws (eCall (tApp (eQVar w) tvs) $ wvars cs))
                                 WInst q t1 p w ws -> do
                                    (cs,tvs) <- instQBinds env q
                                    let s = (tvSelf,t0) : qbound q `zip` tvs
                                    unify t0 (subst s t1)
                                    p <- msubst (subst s p)
                                    return (cs, p, wexpr ws (eQVar w))

instQuals                   :: EnvF x -> QBinds -> [Type] -> TypeM Constraints
instQuals env q ts          = do let s = qbound q `zip` ts
                                 sequence [ constr (subst s (tVar v)) (subst s u) | Quant v us <- q, u <- us ]
  where constr t u@(TC n _)
          | isProto env n   = do w <- newWitness; return $ Impl w t u
          | otherwise       = return $ Cast t (tCon u)

wvars                       :: Constraints -> [Expr]
wvars cs                    = [ eVar v | Impl v _ _ <- cs ]


-- Misc. ---------------------------------------------------------------------------------------------------------------------------

data Equation                           = Eqn Name Type Expr

type Equations                          = [Equation]

bindWits eqs                            = [ Assign l0 [PVar l0 w (Just t)] e | Eqn w t e <- eqs ]

impl2type t (TC n ts)                   = tCon $ TC n (t:ts)

wit2row ws                              = \p -> foldr f p ws
  where f (w,t)                         = TRow NoLoc PRow w t

wit2arg ws                              = \p -> foldr f p ws
  where f (w,t)                         = PosArg (eVar w)

wit2par ws                              = \p -> foldr f p ws
  where f (w,t)                         = PosPar w (Just t) Nothing

var2arg xs                              = \p -> foldr f p xs
  where f x                             = PosArg (eVar x)

exp2arg es                              = \p -> foldr PosArg p es

witsOf cs                               = [ eVar w | Impl w t p <- cs ]

qualWPar env q                          = wit2par (qualWits env q)

qualWRow env q                          = wit2row (qualWits env q)

qualWits env q                          = [ (tvarWit tv p, impl2type (tVar tv) p) | Quant tv ps <- q, p <- ps, isProto env (tcname p) ]

witSubst env q cs                       = [ Eqn w0 t (eVar w) | ((w,t),w0) <- ws `zip` ws0 ]
  where ws                              = [ (w, impl2type t p) | Impl w t p <- cs ]
        ws0                             = [ tvarWit tv p | Quant tv ps <- q, p <- ps, isProto env (tcname p) ]

