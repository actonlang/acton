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

{-# LANGUAGE FlexibleInstances #-}
module Acton.LambdaLifter(liftModule) where

import Control.Monad.State.Strict
import Utils
import Acton.Syntax
import Acton.Names
import Acton.Builtin
import Acton.Prim
import Acton.Printer
import Acton.Env
import Acton.QuickType
import Acton.Subst
import Pretty
import Prelude hiding((<>))

liftModule env0 (Module m imp stmts) = return $ (Module m imp stmts', mapModules1 conv env0)
  where stmts' = runL (llSuite (liftEnv env0) stmts)


-- L :: list of all local variables and parameters in scope, initially empty (locals)
-- F :: map of all function names in scope to their (expanded) set of free variables, initially empty (freemap)
-- N :: map of all function names in scope to their unique aliases, initially empty (namemap)

-- Lambda-lifting
-- --------------
-- For each successive statement s in a function body:
--   If s is a declaration group defining functions fs (classes cannot occur in function bodies):
--     Let M be a map of each f in fs to its free variables (funfree0)
--     For each f in fs and each g in M(f) such that M(g) exists: add M(g) to M(f)
--     Iterate until M reaches a fixpoint (funfree)
--     For each f in fs and each g in M(f) such that F(g) exists: add F(g) to M(f)
--     For each f in fs: restrict M(f) to L
--     Extend F with M
--     Extend N by mapping each f in fs to a globally unique name
--     For each def f[q](ws):b in s:
--       Extend L with ws
--       Recursively transform b into b'
--       Add def N(f) [q] (*F(f),ws):b' to the module top level
--     Replace s with 'pass'
--   Otherwise s is a non-def statement binding names vs (possibly empty):
--     Replace any call g@ts(es) in s, where g is in F, by N(g)@ts(*F(g),es)
--     Replace all non-call references in s to any g@ts in F by lambda ws: g@ts(*F(g),ws), where ws are the parameters of g
--     Extend L with vs

-- Closure conversion
-- ------------------
-- For each successive (non-def) statement s in any lambda-lifted statement list:
--   Replace any lambda ws: e in s by Lam(vs),
--     where Lam is a new class name defined as
--        class Lam (function[x,p,(),t]):
--            def __init__(self,vs'): self.vs' = vs'
--            def __call__(self,ws): vs' = self.vs'; return e
--     and x, p, t and vs' are the parameter row, return type and free variables (restricted to L) of the lambda expression
--   Replace any call e(es) in s, where e has a closure type (not a known function or method), by e.__call__(es)
--   Extend L with vs (the variables bound by s)


-- Lift monad ----------------------------------------------------------------------------------------------------------

type LiftM a                    = State LiftState a

type LiftState                  = ([Decl],[Int])        -- lifted defs, name supply

runL                            :: LiftM a -> a
runL m                          = evalState m ([],[1..])

newName                         :: String -> LiftM Name
newName s                       = state (\(totop,uniq:supply) -> (Internal LLiftPass s uniq, (totop,supply)))

liftToTop                       :: [Decl] -> LiftM ()
liftToTop ds                    = state (\(totop,supply) -> ((), (totop++ds,supply)))

liftedToTop                     :: LiftM [Decl]
liftedToTop                     = state (\(totop,supply) -> (totop, ([],supply)))


llCont                          = Internal LLiftPass "cont" 0

llSelf                          = Internal LLiftPass "self" 0


-- Environment ---------------------------------------------------------------------------------------------------------

type LiftEnv                    = EnvF LiftX

data LiftX                      = LiftX {
                                    ctxtX     :: LiftCtxt,
                                    localsX   :: [(Name,Type)],
                                    freemapX  :: [(Name,[Name])],
                                    quantmapX :: [(Name,[TVar])],
                                    namemapX  :: [(Name,Name)]
                                  }
                                  deriving (Eq,Show)

data LiftCtxt                   = OnTop | InDef | InClass deriving (Eq,Show)

instance Pretty (Name, [Name]) where
    pretty (n,ns)               = pretty n <+> braces (commaSep pretty ns)


liftEnv env0                    = setX env0 LiftX{ ctxtX = OnTop, localsX = [], freemapX = [], quantmapX = [], namemapX = [] }

ctxt env                        = ctxtX $ envX env

setCtxt c env                   = modX env $ \x -> x{ ctxtX = c }

locals env                      = localsX $ envX env
freemap env                     = freemapX $ envX env
quantmap env                    = quantmapX $ envX env
namemap env                     = namemapX $ envX env

extLocals e env                 = modX env $ \x -> x{ localsX = vts ++ locals env `exclude` dom vts }
  where vts                     = [ (v, conv t) | (v,NVar t) <- envOf e ]

extFree m env                   = modX env $ \x -> x{ freemapX = m ++ freemap env,
                                                      quantmapX = [ (f, tvarScope env) | f <- dom m ] ++ quantmap env }

extNames m env                  = modX env $ \x -> x{ namemapX = m ++ namemap env }

findFree n env                  = case lookup n (freemap env) of
                                    Just vs -> Just [ (v,t) | v <- vs, Just t <- [lookup v $ locals env] ]
                                    _ -> Nothing


liftedName env n                = case lookup n (namemap env) of
                                    Just n' -> n'
                                    _       -> n

extraArgs env n                 = case findFree n env of
                                    Just vts -> vts
                                    _        -> []


-- Helpers ------------------------------------------------------------------------------------------------------------------

expand funfree0 funfree         = map exp1 funfree
  where exp1 (f,vs)             = (f, nub (concat (vs : catMaybes [ lookup v funfree0 | v <- vs ])))

iterexpand funfree
  | len funfree == len funfree' = funfree
  | otherwise                   = iterexpand funfree'
  where funfree'                = expand funfree funfree
        len                     = map (length . snd)

addParams vts ps                = foldr (\(n,t) p -> PosPar n (Just t) Nothing p) ps vts

addArgs vts p                   = foldr (PosArg . eVar) p (dom vts)


----------------------------------------------------------------------------------------------------------

class Lift e where
    ll                                  :: LiftEnv -> e -> LiftM e

instance Lift a => Lift (Maybe a) where
    ll env                              = traverse (ll env)

instance (Lift a, EnvOf a, Vars a) => Lift [a] where
    ll env []                           = return []
    ll env (a:as)
      | ctxt env == InDef               = (:) <$> ll env a <*> ll (extLocals a env1) as
      | otherwise                       = (:) <$> ll env a <*> ll env1 as
      where env1                        = define (envOf a) env

llSuite env []                          = return []
llSuite env (Decl l ds : ss)
  | ctxt env == InDef                   = do ns <- zip fs <$> mapM (newName . nstr) (bound ds)
                                             let env1 = extNames ns env'
                                             ds1 <- ll env1 ds
                                             liftToTop (vsubst (selfScopeSubst env) ds1)
                                             llSuite env1 ss
  | ctxt env == InClass                 = do ds' <- ll env1 ds
                                             ss' <- llSuite env1 ss
                                             return $ Decl l ds' : ss'
  | ctxt env == OnTop                   = do ds1 <- ll env1 ds
                                             ds2 <- liftedToTop
                                             ss' <- llSuite env1 ss
                                             return $ Decl l (ds2++ds1) : ss'
  where env'                            = extFree funfree $ define (envOf ds) env
        funfree                         = [ (n, vs \\ bound p) | (n,vs) <- funfree1, let p = [ pos d | d <- ds, dname d == n ] ]
        funfree1                        = expand (freemap env) $ iterexpand funfree0
        funfree0                        = [ (dname d, nub $ free d) | d@Def{} <- ds ]
        fs                              = dom funfree0
        env1                            = define (envOf ds) env
llSuite env (s : ss)
  | ctxt env == InDef                   = (:) <$> ll env s <*> llSuite (extLocals s env1) ss
  | ctxt env == InClass                 = (:) <$> ll env s <*> llSuite env1 ss
  | ctxt env == OnTop                   = do s' <- ll env s
                                             ds <- liftedToTop
                                             ss' <- llSuite env1 ss
                                             return $ if null ds then s':ss' else Decl l0 ds : s' : ss'
  where env1                            = define (envOf s) env


instance Lift Stmt where
    ll env (Return l (Just (Call _ e p KwdNil)))
      | closedType env e,
        effect t == fxProc,
        not (isCont t)                  = do e' <- llSub env e                                  -- A CPS-converted proc, but not a $Cont!
                                             p' <- ll env p
                                             return $ Return l $ Just $ Call l (eDot e' attr_exec_) p' KwdNil
      where t                           = typeOf env e
    ll env (Expr l e)                   = Expr l <$> ll env e
    ll env (Assign l pats e)            = Assign l <$> ll env pats <*> ll env e
    ll env (MutAssign l t e)            = MutAssign l <$> ll env t <*> ll env e
    ll env (Return l e)                 = Return l <$> ll env e
    ll env s@(Pass _)                   = pure s
    ll env s@(Break _)                  = pure s
    ll env s@(Continue _)               = pure s
    ll env (If l branches els)          = If l <$> ll env branches <*> llSuite env els
    ll env (While l e b els)            = While l <$> ll env e <*> llSuite env b <*> llSuite env els
    ll env (Signature l ns sc Property) = pure $ Signature l ns (conv sc) Property
    ll env (Signature l ns sc dec)      = pure $ Signature l ns (convTop sc) dec
    ll env s                            = error ("ll unexpected: " ++ prstr s)

instance Lift Decl where
    ll env (Def l n q p KwdNIL a b d fx doc)
                                        = do b' <- llSuite (setCtxt InDef env1) b
                                             return $ Def l n' q' p' KwdNIL (conv a) b' d fx doc
      where env1                        = extLocals p $ define (envOf p) $ defineTVars q env
            q'                          = if ctxt env == InDef then quantScope env ++ q else q
            p'                          = addParams vts (conv p)
            n'                          = liftedName env n
            vts                         = extraArgs env n
    ll env (Class l n q cs b doc)       = do b' <- llSuite (setCtxt InClass env1) b
                                             return $ Class l n q (conv cs) b' doc
      where env1                        = defineTVars (selfQuant (NoQ n) q) env
    ll env d                            = error ("ll unexpected: " ++ prstr d)

instance Lift Branch where
    ll env (Branch e ss)                = Branch <$> ll env e <*> llSuite env ss

freefun env e@(Var l qn@(NoQ n))
  | Just vts <- findFree n env          = Just (tApp (Var l (NoQ $ liftedName env n)) (map tVar tvs), vts)
  where Just tvs                        = lookup n (quantmap env)
freefun env (Var l n)
  | isDefOrClass env n                  = Just (Var l (primSubst n), [])
freefun env (TApp l e@(Var l' qn@(NoQ n)) ts)
  | Just vts <- findFree n env          = Just (TApp l (Var l' (NoQ $ liftedName env n)) (map tVar tvs ++ conv ts), vts)
  where Just tvs                        = lookup n (quantmap env)
freefun env (TApp l (Var l' n) ts)
  | isDefOrClass env n                  = Just (TApp l (Var l' (primSubst n)) (conv ts), [])
freefun env e                           = Nothing

closureConvert env lambda t0 vts0 es    = do n <- newName (nstr $ noq basename)
                                             --traceM ("## closureConvert " ++ prstr lambda ++ "  as  " ++ prstr n)
                                             liftToTop [Class l0 n q bases body Nothing]
                                             return $ eCall (tApp (eVar n) (map tVar $ tvarScope env)) es
  where q                               = quantScope env
        s                               = selfScopeSubst env
        Lambda _ p _ e fx               = vsubst s lambda
        t1                              = vsubst s t0
        cBase                           = conv $ closureCon fx (prowOf p) t1
        basename                        = tcname cBase
        bases                           = map snd $ findAncestry env cBase
        vts                             = conv $ vsubst s vts0
        body                            = props ++ [Decl l0 [initDef], Decl l0 defs]
        props                           = [ Signature l0 [v] (monotype t) Property | (v,t) <- vsubst s vts ]
        initDef                         = Def l0 initKW [] initPars KwdNIL (Just tNone) (initBody++[sReturn eNone]) NoDec fxPure Nothing
        initPars                        = PosPar llSelf (Just tSelf) Nothing $ pospar vts
        initBody                        = mkBody [ MutAssign l0 (eDot (eVar llSelf) v) (eVar v) | (v,t) <- vts ]
        mainDef attr                    = Def l0 attr [] pars KwdNIL (Just $ conv t1) mainBody NoDec fx Nothing
        pars                            = conv $ addSelfPar p
        args                            = pArg p
        methCall to                     = eCallP (eDot (eVar llSelf) to) args
        parsC tc                        = conv $ addSelfPar $ addContPar tc p
        mainBody                        = [ sAssign (pVar v t) (eDot (eVar llSelf) v) | (v,t) <- vts ] ++ [sReturn e]
        callDef                         = mainDef attr_call_
        execDef                         = Def l0 attr_exec_ [] pars KwdNIL (Just t1) [ sReturn (methCall attr_call_) ] NoDec fxProc Nothing
        asynDef                         = mainDef attr_asyn_
        callDefA                        = Def l0 attr_call_ [] (parsC t1) KwdNIL (Just tR) callBodyA NoDec fxProc Nothing
        callBodyA                       = [ sReturn $ eCall (tApp (eQVar primAWAIT) [t1]) [eVar llCont, methCall attr_asyn_] ]
        execDefA                        = delegate attr_exec_ attr_asyn_ tValue
        evalDef                         = mainDef attr_eval_
        callDefF                        = delegate attr_call_ attr_eval_ t1
        execDefF                        = delegate attr_exec_ attr_eval_ tValue
        delegate name to tc             = Def l0 name [] (parsC tc) KwdNIL (Just tR) (delegateBody to tc) NoDec fxProc Nothing
        delegateBody to tc              = [ sReturn $ eCall (tApp (eQVar primRCont) [tValue]) [eVar llCont, methCall to] ]
        defs
          | basename == primCont        = [callDef]
          | basename == primProc        = [callDef, execDef]
          | basename == primAction      = [callDefA, execDefA, asynDef]
          | basename == primMut         = [callDefF, execDefF, evalDef]
          | basename == primPure        = [callDefF, execDefF, evalDef]

addSelfPar p                            = PosPar llSelf (Just tSelf) Nothing p

addContPar t p                          = PosPar llCont (Just $ tCont t) Nothing p

closureCon fx p t
  | isCont (tFun fx p kwdNil t)         = TC primCont [rtype p]
  | fx == fxProc                        = TC primProc [rtail p, contArg (rtype p)]
  | fx == fxAction                      = TC primAction [p,t]
  | fx == fxMut                         = TC primMut [p,t]
  | fx == fxPure                        = TC primPure [p,t]
  | otherwise                           = error ("### BAD closureCon fx: " ++ prstr fx)
  where contArg (TFun _ fx p _ r)       = rtype p
        contArg t                       = error ("### BAD contArg " ++ prstr t)

isCont (TFun _ fx p@TRow{} _ r)         = fx == fxProc && r == tR && rtail p == posNil && not (isCont $ rtype p)
isCont _                                = False


instance Lift Expr where
    ll env e@(Var l (NoQ n))
      | n `elem` dom (locals env)       = pure e
    ll env e
      | Just (e',vts) <- freefun env e  = closureConvert env (Lambda l0 par KwdNIL (call e' vts) fx) t vts (map (eVar . fst) vts )
      where par                         = pPar paramNames p
            call e' vts                 = Call l0 e' (addArgs vts $ pArg par) KwdNil
            TFun _ fx p _ t             = typeOf env e

    ll env (Call l e p KwdNil)
      | Just (e',vts) <- freefun env e  = do p' <- ll env p
                                             return $ Call l e' (addArgs vts p') KwdNil
      | Async _ e' <- e,
        closedType env e'               = do e' <- ll env e'
                                             p' <- ll env p
                                             return $ Call l (eDot e' attr_asyn_) p' KwdNil
      | closedType env e                = do e' <- llSub env e
                                             p' <- ll env p
                                             let t@TFun{effect = fx}   = typeOf env e'
                                                 attr | isCont t        = attr_call_
                                                      | fx == fxProc    = attr_call_
                                                      | fx == fxMut     = attr_eval_
                                                      | fx == fxPure    = attr_eval_
                                                      | fx == fxAction  = attr_asyn_
                                             return $ Call l (eDot e' attr) p' KwdNil
      | otherwise                       = do e' <- llSub env e
                                             p' <- ll env p
                                             return $ Call l e' p' KwdNil

    ll env (Async l e)
      | closedType env e                = do e <- ll env e
                                             let vts = restrict (locals env) (free e)
                                                 call = Call l0 (eDot e attr_asyn_) (pArg par) KwdNil
                                             closureConvert env (Lambda l0 par KwdNIL call fxProc) (tMsg t) vts (map (eVar . fst) vts)
      | otherwise                       = do e <- ll env e
                                             return $ Async l e
      where par                         = pPar paramNames p
            TFun _ fx p _ t             = typeOf env e

    ll env e0@(Lambda l p KwdNIL e fx)  = do e' <- ll env1 e
                                             let vts = restrict (locals env) (free e' \\ bound p)
                                             closureConvert env (Lambda l p KwdNIL e' fx) t vts (map (eVar . fst) vts)
      where env1                        = extLocals p $ define (envOf p) env
            t                           = typeOf env1 e

    ll env (Var l n)                    = pure $ Var l (primSubst n)

    ll env e@Int{}                      = pure e
    ll env e@Float{}                    = pure e
    ll env e@Imaginary{}                = pure e
    ll env e@Bool{}                     = pure e
    ll env e@None{}                     = pure e
    ll env e@NotImplemented{}           = pure e
    ll env e@Ellipsis{}                 = pure e
    ll env e@Strings{}                  = pure e
    ll env e@BStrings{}                 = pure e
    ll env (Cond l e1 e e2)             = Cond l <$> ll env e1 <*> ll env e <*> ll env e2
    ll env (IsInstance l e c)           = IsInstance l <$> ll env e <*> pure c
    ll env (BinOp l e1 Or e2)           = BinOp l <$> ll env e1 <*> pure Or <*> ll env e2
    ll env (BinOp l e1 And e2)          = BinOp l <$> ll env e1 <*> pure And <*> ll env e2
    ll env (UnOp l Not e)               = UnOp l Not <$> ll env e
    ll env (TApp _ (Dot l e n) ts)      = llDot env l e n ts
    ll env (Dot l e n)                  = llDot env l e n []
    ll env (TApp l e ts)                = TApp l <$> ll env e <*> pure (conv ts)
    ll env (DotI l e i)                 = DotI l <$> llSub env e <*> pure i
    ll env (RestI l e i)                = RestI l <$> llSub env e <*> pure i
    ll env (Yield l e)                  = Yield l <$> ll env e
    ll env (YieldFrom l e)              = YieldFrom l <$> ll env e
    ll env (Tuple l es KwdNil)          = Tuple l <$> ll env es <*> pure KwdNil
    ll env (List l es)                  = List l <$> ll env es
    ll env (Dict l as)                  = Dict l <$> ll env as
    ll env (Set l es)                   = Set l <$> ll env es
    ll env e                            = error ("ll unexpected: " ++ prstr e)

llDot env l e n ts
  | closedType env e0                   = Dot l <$> llSub env e <*> pure n
  | Var _ x <- e,
    NClass{} <- findQName x env         = closureConvert env (Lambda l0 par KwdNIL (calldot x n) fx) t [] []
  | otherwise                           = do e' <- llSub env e
                                             x <- newName "obj"
                                             closureConvert env (Lambda l0 par KwdNIL (calldot (NoQ x) n) fx) t [(x,t')] [e']
  where par                             = pPar paramNames p
        TFun _ fx p _ t                 = typeOf env e0
        calldot x n                     = Call l0 (eDot (eQVar x) n) (pArg par) KwdNil
        t'                              = typeOf env e
        e0                              = tApp (Dot l e n) (conv ts)

llSub                                   :: LiftEnv -> Expr -> LiftM Expr
llSub env (Var l n)                     = pure $ Var l (primSubst n)
llSub env (Dot l e n)                   = Dot l <$> llSub env e <*> pure n
llSub env (TApp l e ts)                 = TApp l <$> llSub env e <*> pure (conv ts)
llSub env (Async l e)                   = Async l <$> llSub env e
llSub env e                             = ll env e

primSubst n
  | n == primASYNCc                     = primASYNC
  | n == primAFTERc                     = primAFTER
  | n == primAWAITc                     = primAWAIT
  | n == primPUSH_Cc                    = primPUSH_C
  | n == primPUSHF_Cc                   = primPUSHF_C
  | n == primRContc                     = primRCont
  | n == primSKIPRESc                   = primSKIPRES
  | otherwise                           = n

instance Lift Elem where
    ll env (Elem e)                     = Elem <$> ll env e
    ll env (Star e)                     = Star <$> ll env e

instance Lift Assoc where
    ll env (Assoc k v)                  = Assoc <$> ll env k <*> ll env v
    ll env (StarStar e)                 = StarStar <$> ll env e

instance Lift PosArg where
    ll env (PosArg e p)                 = PosArg <$> ll env e <*> ll env p
    ll env PosNil                       = pure PosNil

instance Lift Pattern where
    ll env (PVar l n t)                 = return (PVar l n (conv t))


-- Convert environment types -----------------------------------------------------------------------------------------

class Conv a where
    conv                                :: a -> a

instance (Conv a) => Conv (Maybe a) where
    conv                                = fmap conv

instance (Conv a) => Conv [a] where
    conv                                = map conv

instance (Conv a) => Conv (Name, a) where
    conv (n, x)                         = (n, conv x)

instance Conv NameInfo where
    conv (NClass q ps te doc)           = NClass (conv q) (conv ps) (conv te) doc
    conv (NSig sc Property doc)         = NSig (conv sc) Property doc
    conv (NSig sc dec doc)              = NSig (convTop sc) dec doc
    conv (NDef sc dec doc)              = NDef (convTop sc) dec doc
    conv (NVar t)                       = NVar (conv t)
    conv (NSVar t)                      = NSVar (conv t)
    conv ni                             = ni

instance Conv QBind where
    conv (QBind tv cs)                  = QBind tv (conv cs)

instance Conv WTCon where
    conv (w,c)                          = (w, conv c)

convTop (TSchema l q t)                 = TSchema l (conv q) (convTop' t)
  where convTop' (TFun l fx p TNil{} t) = TFun l (conv fx) (conv p) kwdNil (conv t)
        convTop' t                      = conv t

instance Conv TSchema where
    conv (TSchema l q t)                = TSchema l (conv q) (conv t)

instance Conv Type where
    conv t0@(TFun l fx p _ t)           = TCon l (conv $ closureCon fx p t)
    conv (TCon l c)                     = TCon l (conv c)
    conv (TTuple l p k)                 = TTuple l (conv p) (conv k)
    conv (TOpt l t)                     = TOpt l (conv t)
    conv (TRow l k n t r)               = TRow l k n (conv t) (conv r)
    conv (TFX l x)                      = TFX l x
    conv t                              = t

instance Conv TCon where
    conv (TC c ts)                      = TC c (conv ts)

instance Conv PosPar where
    conv (PosPar n t Nothing p)         = PosPar n (conv t) Nothing (conv p)
    conv PosNIL                         = PosNIL
