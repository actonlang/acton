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
--        class Lam ($Closure[p,t]):
--            def __init__(self,vs'): self.vs' = vs'
--            def __enter__(self,ws): vs' = self.vs'; return e
--     and p, t and vs' are the parameter row, return type and free variables (restricted to L) of the lambda expression
--   Replace any call e(es) in s, where e has a closure type (not a known function or method), by e.__enter__(es)
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

extLocals e env                 = modX env $ \x -> x{ localsX = vts ++ locals env }
  where vts                     = [ (v, conv t) | (v,NVar t) <- envOf e ]

extFree m env                   = modX env $ \x -> x{ freemapX = m ++ freemap env,
                                                      quantmapX = [ (f, tvarScope env) | f <- dom m ] ++ quantmap env }

extNames m env                  = modX env $ \x -> x{ namemapX = m ++ namemap env }

findFree n env                  = case lookup n (freemap env) of
                                    Just vs -> Just $ restrict (locals env) vs
                                    _ -> Nothing
    

liftedName env n                = case lookup n (namemap env) of
                                    Just n' -> n'
                                    _       -> n

extraArgs env n                 = case findFree n env of
                                    Just vts -> vts
                                    _        -> []

llSelf                          = Internal LLiftPass "self" 0

paramNames                      = map (Internal LLiftPass "x") [1..]


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
                                             liftToTop (subst (selfSubst env) ds1)
                                             llSuite env1 ss
  | ctxt env == InClass                 = do ds' <- ll env1 ds
                                             ss' <- llSuite env1 ss
                                             return $ Decl l ds' : ss'
  | ctxt env == OnTop                   = do ds1 <- ll env1 ds
                                             ds2 <- liftedToTop
                                             ss' <- llSuite env1 ss
                                             return $ Decl l (ds2++ds1) : ss'
  where env'                            = extFree funfree $ define (envOf ds) env
        funfree                         = expand (freemap env) $ iterexpand funfree0
        funfree0                        = [ (dname d, free d) | d@Def{} <- ds ]
        fs                              = dom funfree0
        env1                            = define (envOf ds) env
llSuite env (s : ss)
  | ctxt env == InDef                   = (:) <$> ll env s <*> llSuite (extLocals s env1) ss
  | otherwise                           = (:) <$> ll env s <*> llSuite env1 ss
  where env1                            = define (envOf s) env


instance Lift Stmt where
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
    ll env (Def l n q p KwdNIL a b d fx)
                                        = do b' <- llSuite (setCtxt InDef env1) b
                                             return $ Def l n' (quantScope env ++ q) p' KwdNIL (conv a) b' d fx
      where env1                        = extLocals p $ define (envOf p) $ defineTVars q env
            p'                          = addParams vts (conv p)
            n'                          = liftedName env n
            vts                         = extraArgs env n
    ll env (Class l n q cs b)           = do b' <- llSuite (setCtxt InClass env1) b
                                             return $ Class l n q cs b'
      where env1                        = defineSelf (NoQ n) q $ defineTVars q env
    ll env d                            = error ("ll unexpected: " ++ prstr d)

instance Lift Branch where
    ll env (Branch e ss)                = Branch <$> ll env e <*> llSuite env ss



freefun env (Var l (NoQ n))
  | Just vts <- findFree n env          = Just (tApp (Var l (NoQ $ liftedName env n)) (map tVar tvs), vts)
  where Just tvs                        = lookup n (quantmap env)
freefun env (TApp l (Var l' (NoQ n)) ts)
  | Just vts <- findFree n env          = Just (TApp l (Var l' (NoQ $ liftedName env n)) (map tVar tvs ++ ts), vts)
  where Just tvs                        = lookup n (quantmap env)
freefun env e                           = Nothing

closureConvert env lambda t0 vts0
                                        = do n <- newName "lambda"
                                             liftToTop [Class l0 n q [TC primClos [fx,prowOf p,t]] te]
                                             return $ eCall (tApp (eVar n) (map tVar $ tvarScope env)) [ eVar v | (v,_) <- vts ]
  where q                               = quantScope env
        s                               = selfSubst env
        Lambda _ p _ e fx               = subst s lambda
        t                               = subst s t0
        vts                             = subst s vts0
        te                              = props ++ [Decl l0 [initDef], Decl l0 [enterDef]]
        props                           = [ Signature l0 [v] (monotype t) Property | (v,t) <- subst s vts ]
        initDef                         = Def l0 initKW [] initPars KwdNIL (Just tNone) initBody NoDec fxPure
        initPars                        = PosPar llSelf (Just tSelf) Nothing $ pospar vts
        initBody                        = [ MutAssign l0 (eDot (eVar llSelf) v) (eVar v) | (v,t) <- vts ]
        enterDef                        = Def l0 enterKW [] enterPars KwdNIL (Just t) enterBody NoDec fx
        enterPars                       = PosPar llSelf (Just tSelf) Nothing p
        enterBody                       = [ Assign l0 [PVar l0 v (Just t)] (eDot (eVar llSelf) v) | (v,t) <- vts ] ++ [Return l0 (Just e)]

instance Lift Expr where
    ll env e
      | Just (e',vts) <- freefun env e  = closureConvert env (Lambda l0 par KwdNIL (call e' vts) fx) t vts
      where par                         = pPar paramNames (conv p)
            call e' vts                 = Call l0 e' (addArgs vts $ par2arg par) KwdNil
            TFun _ fx p _ t             = typeOf env e

    ll env (Call l e p KwdNil)
      | Just (e',vts) <- freefun env e  = do p' <- ll env p
                                             return $ Call l e' (addArgs vts p) KwdNil
      | (_,True) <- typeOf2 env e       = do e' <- ll env e
                                             p' <- ll env p
                                             return $ Call l (eDot e' enterKW) p KwdNil
      | otherwise                       = do e' <- ll env e
                                             p' <- ll env p
                                             return $ Call l e' p' KwdNil

    ll env (Lambda l p KwdNIL e fx)     = do e' <- ll env1 e
                                             closureConvert env (Lambda l (conv p) KwdNIL e' fx) t vts
      where env1                        = extLocals p $ define (envOf p) env
            vts                         = restrict (locals env) (free e \\ bound p)
            t                           = typeOf env1 e

    ll env (Var l n)
      | n == primASYNCc                 = return $ Var l primASYNC
      | n == primAFTERc                 = return $ Var l primAFTER
      | n == primAWAITc                 = return $ Var l primAWAIT
      | n == primPUSHc                  = return $ Var l primPUSH
      | n == primRContc                 = return $ Var l primRCont
      | n == primSKIPRESc               = return $ Var l primSKIPRES
      | otherwise                       = return $ Var l n

    ll env e@Int{}                      = pure e
    ll env e@Float{}                    = pure e
    ll env e@Imaginary{}                = pure e
    ll env e@Bool{}                     = pure e
    ll env e@None{}                     = pure e
    ll env e@NotImplemented{}           = pure e
    ll env e@Ellipsis{}                 = pure e
    ll env e@Strings{}                  = pure e
    ll env e@BStrings{}                 = pure e
    ll env (TApp l e ts)                = TApp l <$> ll env e <*> pure (conv ts)
    ll env (Cond l e1 e e2)             = Cond l <$> ll env e1 <*> ll env e <*> ll env e2
    ll env (IsInstance l e c)           = IsInstance l <$> ll env e <*> pure c
    ll env (BinOp l e1 Or e2)           = BinOp l <$> ll env e1 <*> pure Or <*> ll env e2
    ll env (BinOp l e1 And e2)          = BinOp l <$> ll env e1 <*> pure And <*> ll env e2
    ll env (UnOp l Not e)               = UnOp l Not <$> ll env e
    ll env (Dot l e n)                  = Dot l <$> ll env e <*> pure n
    ll env (Rest l e n)                 = Rest l <$> ll env e <*> pure n
    ll env (DotI l e i)                 = DotI l <$> ll env e <*> pure i
    ll env (RestI l e i)                = RestI l <$> ll env e <*> pure i
    ll env (Yield l e)                  = Yield l <$> ll env e
    ll env (YieldFrom l e)              = YieldFrom l <$> ll env e
    ll env (Tuple l es ks)              = Tuple l <$> ll env es <*> ll env ks
    ll env (List l es)                  = List l <$> ll env es
    ll env e                            = error ("ll unexpected: " ++ prstr e)

instance Lift Elem where
    ll env (Elem e)                     = Elem <$> ll env e
    ll env (Star e)                     = Star <$> ll env e

instance Lift PosArg where
    ll env (PosArg e p)                 = PosArg <$> ll env e <*> ll env p
    ll env PosNil                       = pure PosNil

instance Lift KwdArg where
    ll env (KwdArg n e k)               = KwdArg n <$> ll env e <*> ll env k
    ll env KwdNil                       = pure KwdNil

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
    conv (NClass q ps te)               = NClass (conv q) (conv ps) (conv te)
    conv (NSig sc Property)             = NSig (conv sc) Property
    conv (NSig sc dec)                  = NSig (convTop sc) dec
    conv (NDef sc dec)                  = NDef (convTop sc) dec
    conv (NVar t)                       = NVar (conv t)
    conv (NSVar t)                      = NSVar (conv t)
    conv ni                             = ni

instance Conv QBind where
    conv (Quant tv cs)                  = Quant tv (conv cs)

instance Conv WTCon where
    conv (w,c)                          = (w, conv c)

convTop (TSchema l q t)                 = TSchema l (conv q) (convTop' t)
  where convTop' (TFun l fx p TNil{} t) = TFun l (conv fx) (conv p) kwdNil (conv t)
        convTop' t                      = conv t

instance Conv TSchema where
    conv (TSchema l q t)                = TSchema l (conv q) (conv t)

instance Conv Type where
    conv (TFun l fx p TNil{} t)         = TCon l (TC primClos [conv fx, conv p, conv t])
    conv (TCon l c)                     = TCon l (conv c)
    conv (TTuple l p k)                 = TTuple l (conv p) (conv k)
    conv (TOpt l t)                     = TOpt l (conv t)
    conv (TRow l k n t r)               = TRow l k n (conv t) (conv r)
    conv (TFX l x)                      = TFX l (conv x)
    conv t                              = t

instance Conv FX where
    conv (FXAct t)                      = FXMut (conv t)
    conv (FXMut t)                      = FXMut (conv t)
    conv FXAction                       = FXAction
    conv FXPure                         = FXPure

instance Conv TCon where
    conv (TC c ts)                      = TC c (conv ts)

instance Conv PosPar where
    conv (PosPar n t Nothing p)         = PosPar n (conv t) Nothing (conv p)
    conv PosNIL                         = PosNIL