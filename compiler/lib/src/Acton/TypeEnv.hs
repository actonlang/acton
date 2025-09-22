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

instance UFree TypeX where
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
            constr u t      = if isProto env (tcname u) then Proto (DfltInfo NoLoc 20 Nothing []) nWild t u else Cast (DfltInfo NoLoc 21 Nothing []) t (tCon u)

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
instQBinds env q            = do ts <- newUnivars [ tvkind v | Quant v _ <- q ]
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
          | isProto env n   = do w <- newWitness; return $ Proto (DfltInfo NoLoc 24 Nothing []) w t u
          | otherwise       = return $ Cast (DfltInfo NoLoc 25 Nothing []) t (tCon u)

wvars                       :: Constraints -> [Expr]
wvars cs                    = [ eVar v | Proto _ v _ _ <- cs ]


-- Equations -----------------------------------------------------------------------------------------------------------------------

data Equation                           = Eqn Name Type Expr
                                        | QEqn Name QBinds Equations

type Equations                          = [Equation]

instance Pretty Equation where
    pretty (Eqn n t e)                  = pretty n <+> colon <+> pretty t <+> equals <+> pretty e
    pretty (QEqn n q eqs)               = pretty n <+> colon <+> pretty q <+> text "=>" $+$
                                          nest 4 (vcat $ map pretty eqs)

bindWits eqs                            = [ Assign l0 [PVar l0 w (Just t)] e | Eqn w t e <- eqs ]



eqfind n (Eqn n' t e : eqs) | n == n'   = e
eqfind n (_ : eqs)                      = eqfind n eqs
eqfind n []                             = error ("INTERNAL: witness " ++ prstr n ++ " lacks a defining equation")

eqdrop n (Eqn n' t e : eqs) | n == n'   = eqs
eqdrop n (eq : eqs)                     = eq : eqdrop n eqs
eqdrop n []                             = []

eqexpand n (QEqn n' q eqs' : eqs)
  | n == n'                             = eqs' ++ eqs
eqexpand n (eq : eqs)                   = eq : eqexpand n eqs
eqexpand n []                           = []


class Inline a where
    inline                              :: Equations -> a -> a

instance Inline a => Inline [a] where
    inline eqs                          = map (inline eqs)

instance Inline a => Inline (Maybe a) where
    inline eqs                          = fmap (inline eqs)

instance Inline Expr where
    inline eqs e@(Var l (NoQ n))
      | isWitness n                     = inline (eqdrop n eqs) (eqfind n eqs)
      | otherwise                       = e
    inline eqs (Call l e p k)           = Call l (inline eqs e) (inline eqs p) (inline eqs k)
    inline eqs (TApp l e ts)            = TApp l (inline eqs e) ts
    inline eqs (Async l e)              = Async l (inline eqs e)
    inline eqs (Await l e)              = Await l (inline eqs e)
    inline eqs (Index l e is)           = Index l (inline eqs e) (inline eqs is)
    inline eqs (Slice l e sl)           = Slice l (inline eqs e) (inline eqs sl)
    inline eqs (Cond l e1 e2 e3)        = Cond l (inline eqs e1) (inline eqs e2) (inline eqs e3)
    inline eqs (IsInstance l e c)       = IsInstance l (inline eqs e) c
    inline eqs (BinOp l e1 op e2)       = BinOp l (inline eqs e1) op (inline eqs e2)
    inline eqs (CompOp l e ops)         = CompOp l (inline eqs e) (inline eqs ops)
    inline eqs (UnOp l op e)            = UnOp l op (inline eqs e)
    inline eqs (Dot l e n)              = Dot l (inline eqs e) n
    inline eqs (Rest l e n)             = Rest l (inline eqs e) n
    inline eqs (DotI l e i)             = DotI l (inline eqs e) i
    inline eqs (RestI l e i)            = RestI l (inline eqs e) i
    inline eqs (Lambda l p k e fx)      = Lambda l (inline eqs p) (inline eqs k) (inline eqs e) fx
    inline eqs (Yield l e)              = Yield l (inline eqs e)
    inline eqs (YieldFrom l e)          = YieldFrom l (inline eqs e)
    inline eqs (Tuple l p k)            = Tuple l (inline eqs p) (inline eqs k)
    inline eqs (List l es)              = List l (inline eqs es)
    inline eqs (ListComp l e c)         = ListComp l (inline eqs e) (inline eqs c)
    inline eqs (Dict l as)              = Dict l (inline eqs as)
    inline eqs (DictComp l a c)         = DictComp l (inline eqs a) (inline eqs c)
    inline eqs (Set l es)               = Set l (inline eqs es)
    inline eqs (SetComp l e c)          = SetComp l (inline eqs e) (inline eqs c)
    inline eqs (Paren l e)              = Paren l (inline eqs e)
    inline eqs e                        = e

instance Inline PosPar where
    inline eqs (PosPar n t e p)         = PosPar n t (inline eqs e) (inline eqs p)
    inline eqs p                        = p

instance Inline KwdPar where
    inline eqs (KwdPar n t e k)         = KwdPar n t (inline eqs e) (inline eqs k)
    inline eqs k                        = k

instance Inline PosArg where
    inline eqs (PosArg e p)             = PosArg (inline eqs e) (inline eqs p)
    inline eqs (PosStar e)              = PosStar (inline eqs e)
    inline eqs PosNil                   = PosNil

instance Inline KwdArg where
    inline eqs (KwdArg n e k)           = KwdArg n (inline eqs e) (inline eqs k)
    inline eqs (KwdStar e)              = KwdStar (inline eqs e)
    inline eqs KwdNil                   = KwdNil

instance Inline OpArg where
    inline eqs (OpArg op e)             = OpArg op (inline eqs e)

instance Inline Comp where
    inline eqs (CompFor l p e c)        = CompFor l p (inline eqs e) (inline eqs c)
    inline eqs (CompIf l e c)           = CompIf l (inline eqs e) (inline eqs c)
    inline eqs NoComp                   = NoComp

instance Inline WithItem where
    inline eqs (WithItem e p)           = WithItem (inline eqs e) p

instance Inline Elem where
    inline eqs (Elem e)                 = Elem (inline eqs e)
    inline eqs (Star e)                 = Star (inline eqs e)

instance Inline Assoc where
    inline eqs (Assoc e1 e2)            = Assoc (inline eqs e1) (inline eqs e2)
    inline eqs (StarStar e)             = StarStar (inline eqs e)

instance Inline Sliz where
    inline eqs (Sliz l e1 e2 e3)        = Sliz l (inline eqs e1) (inline eqs e2) (inline eqs e3)


instance Inline Stmt where
    inline eqs (Assign l ps e)          = Assign l ps (inline eqs e)
    inline eqs (Expr l e)               = Expr l (inline eqs e)
    inline eqs (MutAssign l t e)        = MutAssign l (inline eqs t) (inline eqs e)
    inline eqs (AugAssign l t op e)     = AugAssign l (inline eqs t) op (inline eqs e)
    inline eqs (Assert l e mbe)         = Assert l (inline eqs e) (inline eqs mbe)
    inline eqs (Delete l t)             = Delete l (inline eqs t)
    inline eqs (Return l mbe)           = Return l (inline eqs mbe)
    inline eqs (Raise l e)              = Raise l (inline eqs e)
    inline eqs (If l bs els)            = If l (inline eqs bs) (inline eqs els)
    inline eqs (While l e b els)        = While l (inline eqs e) (inline eqs b) (inline eqs els)
    inline eqs (For l p e b els)        = For l p (inline eqs e) (inline eqs b) (inline eqs els)
    inline eqs (Try l b hs els fin)     = Try l (inline eqs b) (inline eqs hs) (inline eqs els) (inline eqs fin)
    inline eqs (With l is b)            = With l (inline eqs is) (inline eqs b)
    inline eqs (Data l mbp ss)          = Data l mbp (inline eqs ss)
    inline eqs (VarAssign l ps e)       = VarAssign l ps (inline eqs e)
    inline eqs (After l e e')           = After l (inline eqs e) (inline eqs e')
    inline eqs (Decl l ds)              = Decl l (inline eqs ds)
    inline eqs s                        = s

instance Inline Decl where
    inline eqs (Def l n q p k t b d fx doc)
                                        = Def l n q (inline eqs' p) (inline eqs' k) t (inline eqs' b) d fx doc
      where eqs'                        = eqexpand n eqs
    inline eqs (Actor l n q p k b doc)  = Actor l n q (inline eqs' p) (inline eqs' k) (inline eqs' b) doc
      where eqs'                        = eqexpand n eqs
    inline eqs (Class l n q us b doc)   = Class l n q us (inline eqs' b) doc
      where eqs'                        = eqexpand n eqs
    inline eqs (Protocol l n q us b doc) = Protocol l n q us (inline eqs' b) doc
      where eqs'                        = eqexpand n eqs
    inline eqs (Extension l q c us b doc) = Extension l q c us (inline eqs' b) doc
      where eqs'                        = eqexpand n eqs
            n                           = extensionName us c

instance Inline Branch where
    inline eqs (Branch e ss)            = Branch (inline eqs e) (inline eqs ss)

instance Inline Handler where
    inline eqs (Handler ex b)           = Handler ex (inline eqs b)



-- Misc. ---------------------------------------------------------------------------------------------------------------------------

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

protoWitsOf cs                          = [ eVar w | Proto _ w t p <- cs ]

qualWPar env q                          = wit2par (qualWits env q)

qualWRow env q                          = wit2row (qualWits env q)

qualWits env q                          = [ (tvarWit tv p, impl2type (tVar tv) p) | Quant tv ps <- q, p <- ps, isProto env (tcname p) ]

witSubst env q cs                       = [ Eqn w0 t (eVar w) | ((w,t),w0) <- ws `zip` ws0 ]
  where ws                              = [ (w, impl2type t p) | Proto _ w t p <- cs ]
        ws0                             = [ tvarWit tv p | Quant tv ps <- q, p <- ps, isProto env (tcname p) ]

