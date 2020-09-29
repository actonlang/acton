module Acton.QuickType where

import Acton.Syntax
import Acton.Names
import Acton.Subst
import Acton.Env
import Acton.Builtin
import Utils

class SchemaOf a where
    schemaOf                        :: Env -> a -> TSchema

class TypeOf a where
    typeOf                          :: Env -> a -> Type

class EnvOf a where
    envOf                           :: a -> TEnv

instance SchemaOf Expr where
    schemaOf env (Var _ n)          = case findQName n env of
                                        NVar t ->
                                            monotype t
                                        NSVar t ->
                                            monotype t
                                        NDef sc _ ->
                                            sc
                                        NClass q _ _ ->
                                            let c = TC n (map tVar $ tybound q)
                                                TSchema _ q' t = findAttr' env c initKW
                                            in tSchema (q++q') $ subst [(tvSelf,tCon c)] t{ restype = tSelf }
                                        NAct q p k _ ->
                                            tSchema q (tFun (fxAct tWild) p k (tCon0 n q))
    schemaOf env (Dot _ e n)        = case typeOf env e of
                                        TCon _ c -> findAttr' env c n
                                        TTuple _ p k -> f n k
      where f n (TRow l k x t r)
              | x == n              = monotype t
              | otherwise           = f n r
    schemaOf env e                  = monotype $ typeOf env e

instance TypeOf Expr where
    typeOf env e@Var{}              = case schemaOf env e of
                                         TSchema _ [] t -> t
    typeOf env e@Dot{}              = case schemaOf env e of
                                         TSchema _ [] t -> t
    typeOf env (Int _ i s)          = tInt
    typeOf env (Float _ f s)        = tFloat
--  typeOf env (Imaginary _ i s)    = undefined
    typeOf env (Bool _ b)           = tBool
    typeOf env (None _)             = tNone
--  typeOf env (NotImplemented _)   = undefined
--  typeOf env (Ellipsis _)         = undefined
    typeOf env (Strings _ ss)       = tStr
    typeOf env (BStrings _ ss)      = tBytes
    typeOf env (Call _ e ps ks)     = case typeOf env e of
                                        TFun _ fx p k t -> if fx == fxAction then tMsg t else t
    typeOf env (TApp _ e ts)        = case schemaOf env e of
                                        TSchema _ q t | length q == length ts ->
                                            subst (tybound q `zip` ts) t
    typeOf env (Await _ e)          = case typeOf env e of
                                        TCon _ (TC c [t]) | qmatch env c qnMsg -> t
    typeOf env (Cond _ e1 e e2)
      | castable env t1 t2          = t2
      | otherwise                   = t1
      where t1                      = typeOf env e1
            t2                      = typeOf env e2
    typeOf env (IsInstance _ e c)   = tBool
    typeOf env (DotI _ e i)         = case typeOf env e of
                                        TTuple _ p k -> f i p
      where f 0 (TRow _ _ _ t p)    = t
            f i (TRow _ _ _ _ p)    = f (i-1) p
    typeOf env (RestI _ e i)        = case typeOf env e of
                                        TTuple _ p k -> TTuple NoLoc (f i p) kwdNil
      where f i (TRow l k x t r)
              | i == 0              = r
              | otherwise           = TRow l k x t (f (i-1) r)
            f i (TNil l k)          = TNil l k
    typeOf env (Rest _ e n)         = case typeOf env e of
                                        TTuple _ p k -> TTuple NoLoc posNil (f n k)
      where f n (TRow l k x t r)
              | x == n              = r
              | otherwise           = TRow l k x t (f n r)
            f n (TNil l k)          = TNil l k
    typeOf env (Lambda _ p k e fx)  = TFun NoLoc fx (prowOf p) (krowOf k) (typeOf env1 e)
      where env1                    = define (envOf k) $ define (envOf p) $ env
--  typeOf env (Yield _ e)          = undefined
--  typeOf env (YieldFrom _ e)      = undefined
    typeOf env (Tuple _ p k)        = TTuple NoLoc (f p) (g k)
      where f (PosArg e p)          = posRow (typeOf env e) (f p)
            f (PosStar e)           = case typeOf env e of TTuple _ p _ -> p
            f PosNil                = posNil
            g (KwdArg n e k)        = kwdRow n (typeOf env e) (g k)
            g (KwdStar e)           = case typeOf env e of TTuple _ _ k -> k
            g KwdNil                = kwdNil
    typeOf env (List _ es)          = tList (maxtype env $ map (typeOf env) es)
    typeOf env (ListComp _ e c)     = tList (typeOf env1 e)
      where env1                    = define (envOf c) env
    typeOf env (Paren _ e)          = typeOf env e

--  The following constructs are translated away during type inference:
--  typeOf env (Index _ e is)       = undefined
--  typeOf env (Slice _ e sl)       = undefined
--  typeOf env (BinOp _ l op r)     = undefined
--  typeOf env (CompOp _ e ops)     = undefined
--  typeOf env (UnOp _ op e)        = undefined
--  typeOf env (Dict _ as)          = undefined
--  typeOf env (DictComp _ a c)     = undefined
--  typeOf env (Set _ es)           = undefined
--  typeOf env (SetComp _ e c)      = undefined


instance TypeOf Elem where
    typeOf env (Elem e)             = typeOf env e
    typeOf env (Star e)             = case typeOf env e of
                                        TCon _ (TC c [t]) | qmatch env c qnList -> t

instance (EnvOf a) => EnvOf [a] where
    envOf                           = concat . map envOf

instance (EnvOf a) => EnvOf (Maybe a) where
    envOf                           = maybe [] envOf

instance EnvOf PosPar where
    envOf (PosPar n (Just t) _ p)   = (n,NVar t) : envOf p
    envOf (PosSTAR n (Just t))      = [(n,NVar t)]
    envOf PosNIL                    = []

instance EnvOf KwdPar where
    envOf (KwdPar n (Just t) _ k)   = (n,NVar t) : envOf k
    envOf (KwdSTAR n (Just t))      = [(n,NVar t)]
    envOf KwdNIL                    = []

instance EnvOf Comp where
    envOf (CompFor _ p e c)         = envOf p ++ envOf c
    envOf (CompIf _ e c)            = envOf c
    envOf NoComp                    = []

instance EnvOf Pattern where
    envOf (PVar _ n (Just t))       = [(n, NVar t)]
    envOf (PVar _ n Nothing)        = []
    envOf (PTuple _ ps ks)          = envOf ps ++ envOf ks
    envOf (PList _ ps p)            = envOf ps ++ envOf p
    envOf (PParen _ p)              = envOf p

instance EnvOf PosPat where
    envOf (PosPat p ps)             = envOf p ++ envOf ps
    envOf (PosPatStar p)            = envOf p
    envOf PosPatNil                 = []

instance EnvOf KwdPat where
    envOf (KwdPat n p ps)           = envOf p ++ envOf ps
    envOf (KwdPatStar p)            = envOf p
    envOf KwdPatNil                 = []

instance EnvOf Stmt where
    envOf (Assign _ ps e)           = envOf ps
    envOf (VarAssign _ ps e)        = envOf ps
    envOf (Decl _ ds)               = envOf ds
    envOf (Signature _ ns sc dec)   = [ (n, NSig sc dec) | n <- ns ]
    envOf (If _ bs els)             = commonEnvOf $ [ ss | Branch _ ss <- bs ] ++ [els]
    envOf (Try _ b hs els fin)      = commonEnvOf $ [ ss | Handler _ ss <- hs ] ++ [b++els]
    envOf (With _ items b)          = exclude (bound items) (envOf b)
    envOf s                         = []

commonEnvOf suites
  | null liveSuites                 = []
  | otherwise                       = restrict (foldr1 intersect $ map bound $ tail liveSuites) (envOf $ head liveSuites)
  where liveSuites                  = filter fallsthru suites

instance EnvOf Decl where
    envOf (Def _ n q p k (Just t) b dec fx)
                                    = [(n, NDef (TSchema NoLoc q $ TFun NoLoc fx (prowOf p) (krowOf k) t) dec)]
    envOf (Class _ n q as ss)       = [(n, NClass q as' (envOf ss))]
      where as'                     = [ ([Nothing],a) | a <- as ]

    envOf (Actor _ n q p k a b)     = [(n, NAct q (prowOf p) (krowOf k) (envOf b))]

--  The following constructs are translated away during type inference:
--  envOf (Protocol _ n q as ss)    = undefined
--  envOf (Extension _ n q as ss)   = undefined

instance EnvOf Except where
    envOf (ExceptAll _)             = []
    envOf (Except _ x)              = []
    envOf (ExceptAs _ x n)          = [(n, NVar $ tCon (TC x []))]

instance EnvOf WithItem where
    envOf (WithItem e p)            = envOf p

instance EnvOf Branch where
    envOf _                         = []

instance EnvOf Handler where
    envOf _                         = []

instance EnvOf Sliz where
    envOf _                         = []

instance EnvOf OpArg where
    envOf _                         = []

instance EnvOf Elem where
    envOf _                         = []

instance EnvOf Assoc where
    envOf _                         = []

