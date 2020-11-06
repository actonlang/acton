module Acton.QuickType where

import Acton.Syntax
import Acton.Names
import Acton.Subst
import Acton.Env
import Acton.Builtin
import Acton.Prim
import Utils

class SchemaOf a where
    schemaOf                        :: EnvF x -> a -> (TSchema, Bool)       -- Snd result indicates that function type means closure

class TypeOf a where
    typeOf                          :: EnvF x -> a -> Type

class EnvOf a where
    envOf                           :: a -> TEnv

instance SchemaOf Expr where
    schemaOf env (Var _ n)          = case findQName n env of
                                        NVar t ->
                                            (monotype t, True)
                                        NSVar t ->
                                            (monotype t, True)
                                        NDef sc dec ->
                                            (sc, funAsClos (Just dec))
                                        NSig sc dec ->
                                            (sc, funAsClos (Just dec))
                                        NClass q _ _ ->
                                            let tc = TC n (map tVar $ tybound q)
                                                (TSchema _ q' t, _) = findAttr' env tc initKW
                                                t' = if restype t == tR then t else t{ restype = tSelf }
                                            in (tSchema (q++q') $ subst [(tvSelf,tCon tc)] t', False)
                                        NAct q p k _ ->
                                            (tSchema q (tFun (fxAct tWild) p k (tCon0 n q)), False)
                                        i -> error ("### schemaOf Var unexpected " ++ prstr (noq n,i))
    schemaOf env (Dot _ (Var _ x) n)
      | NClass q _ _ <- info        = let tc = TC x (map tVar $ tybound q)
                                          Just (_, TSchema _ q' t, mbdec) = findAttr env tc n
                                      in (tSchema (q++q') $ subst [(tvSelf,tCon tc)] (addSelf t mbdec){ restype = tSelf }, funAsClos mbdec)
      where info                    = findQName x env
    schemaOf env e0@(Dot _ e n)     = case typeOf env e of
                                        TCon _ c -> findAttr' env c n
                                        TTuple _ p k -> (f n k, True)
                                        TVar _ v  -> findAttr' env (findTVBound env v) n
                                        t -> error ("### schemaOf Dot unexpected " ++ prstr e0 ++ " : " ++ prstr t)
      where f n (TRow l k x t r)
              | x == n              = monotype t
              | otherwise           = f n r
    schemaOf env e                  = (monotype $ typeOf env e, True)


typeOf2                             :: EnvF x -> Expr -> (Type, Bool)           -- Snd result indicates that function type means closure
typeOf2 env e@Var{}                 = case schemaOf env e of
                                         (TSchema _ [] t, cl) -> (t, cl)
                                         (sc, _) -> error ("###### typeOf " ++ prstr e ++ " is " ++ prstr sc)
typeOf2 env e@Dot{}                 = case schemaOf env e of
                                         (TSchema _ [] t, cl) -> (t, cl)
                                         (sc, _) -> error ("###### typeOf " ++ prstr e ++ " is " ++ prstr sc)
typeOf2 env (TApp _ e ts)           = case schemaOf env e of
                                         (TSchema _ q t, cl) | length q == length ts ->
                                             (subst (tybound q `zip` ts) t, cl)
typeOf2 env e                       = (typeOf env e, True)


instance TypeOf Expr where
    typeOf env e@Var{}              = fst $ typeOf2 env e
    typeOf env e@Dot{}              = fst $ typeOf2 env e
    typeOf env e@TApp{}             = fst $ typeOf2 env e
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
                                        t -> error ("###### typeOf Fun " ++ prstr e ++ " : " ++ prstr t)
    typeOf env (Await _ e)          = case typeOf env e of
                                        TCon _ (TC c [t]) | qmatch env c qnMsg -> t
    typeOf env (BinOp _ l Or r)     = tBool
    typeOf env (BinOp _ l And r)    = tBool
    typeOf env (UnOp _ Not e)       = tBool
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
--  typeOf env (CompOp _ e ops)     = undefined
--  typeOf env (Dict _ as)          = undefined
--  typeOf env (DictComp _ a c)     = undefined
--  typeOf env (Set _ es)           = undefined
--  typeOf env (SetComp _ e c)      = undefined


instance TypeOf Elem where
    typeOf env (Elem e)             = typeOf env e
    typeOf env (Star e)             = case typeOf env e of
                                        TCon _ (TC c [t]) | qmatch env c qnList -> t

instance TypeOf Pattern where
    typeOf env (PVar _ n (Just t))  = t
    typeOf env (PVar _ n Nothing)   = tWild                         -- TODO: prove this never happens
    typeOf env (PTuple _ ps ks)     = tTuple (typeOf env ps) (typeOf env ks)
    typeOf env (PList _ ps p)       = tList (typeOf env $ head ps)
    typeOf env (PParen _ p)         = typeOf env p

instance TypeOf PosPat where
    typeOf env (PosPat p ps)        = posRow (typeOf env p) (typeOf env ps)
    typeOf env (PosPatStar p)       = typeOf env p
    typeOf env PosPatNil            = posNil

instance TypeOf KwdPat where
    typeOf env (KwdPat n p ps)      = kwdRow n (typeOf env p) (typeOf env ps)
    typeOf env (KwdPatStar p)       = typeOf env p
    typeOf env KwdPatNil            = kwdNil

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
    envOf (With _ items b)          = exclude (envOf b) (bound items)
    envOf s                         = []

commonEnvOf suites
  | null liveSuites                 = []
  | otherwise                       = restrict (envOf $ head liveSuites) (foldr1 intersect $ map bound $ tail liveSuites)
  where liveSuites                  = filter fallsthru suites

instance EnvOf Decl where
    envOf (Def _ n q p k (Just t) b dec fx)
                                    = [(n, NDef (TSchema NoLoc q $ TFun NoLoc fx (prowOf p) (krowOf k) t) dec)]
    envOf (Class _ n q as ss)       = [(n, NClass q as' (map dropSelf $ envOf ss))]
      where as'                     = [ ([Nothing],a) | a <- as ]

    envOf (Actor _ n q p k ss)      = [(n, NAct q (prowOf p) (krowOf k) (envOf ss))]

dropSelf (n, NDef (TSchema l q (TFun l' fx (TRow _ _ _ _ p) k t)) dec)
  | dec /= Static                   = (n, NDef (TSchema l q (TFun l' fx p k t)) dec)
dropSelf (n, i)                     = (n, i)

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


----------------------------------------------------------------------------------------------------------------------
-- castable predicate
----------------------------------------------------------------------------------------------------------------------

castable                                    :: EnvF x -> Type -> Type -> Bool
castable env (TWild _) t2                   = True
castable env t1 (TWild _)                   = True

castable env (TCon _ c1) (TCon _ c2)
  | Just (wf,c') <- search                  = tcargs c1 == tcargs c'
  where search                              = findAncestor env c1 (tcname c2)

castable env (TFun _ fx1 p1 k1 t1) (TFun _ fx2 p2 k2 t2)
  | fx1 == fxAction , fx2 /= fxAction       = castable env fx1 fx2 && castable env p2 p1 && castable env k2 k1 && castable env (tMsg t1) t2
  | otherwise                               = castable env fx1 fx2 && castable env p2 p1 && castable env k2 k1 && castable env t1 t2

castable env (TTuple _ p1 k1) (TTuple _ p2 k2)
                                            = castable env p1 p2 && castable env k1 k2

castable env (TUnion _ us1) (TUnion _ us2)
  | all (uniElem env us2) us1               = True
castable env (TUnion _ us1) t2
  | all uniLit us1                          = t2 == tStr
castable env (TCon _ c1) (TUnion _ us2)
  | uniConElem env c1 us2                   = True

castable env (TOpt _ t1) (TOpt _ t2)        = castable env t1 t2
castable env (TNone _) (TOpt _ t)           = True
castable env (TNone _) (TNone _)            = True

castable env (TFX _ fx1) (TFX _ fx2)        = castable' fx1 fx2
  where castable' FXPure FXPure             = True
        castable' FXPure (FXMut _)          = True
        castable' FXPure (FXAct _)          = True
        castable' (FXMut t1) (FXMut t2)     = t1 == t2
        castable' (FXMut t1) (FXAct t2)     = t1 == t2
        castable' (FXAct t1) (FXAct t2)     = t1 == t2
        castable' FXAction FXAction         = True
        castable' FXAction (FXAct _)        = True
        castable' fx1 fx2                   = False

castable env (TNil _ k1) (TNil _ k2)
  | k1 == k2                                = True
castable env (TRow _ k n t1 r1) r2
  | Just (t2,r2') <- findInRow n r2         = t2 /= tWild && castable env t1 t2 && r2' /= tWild && castable env r1 r2'

castable env (TVar _ tv1) (TVar _ tv2)
  | tv1 == tv2                              = True

castable env t1@(TVar _ tv) t2              = castable env (tCon c) t2
  where c                                   = findTVBound env tv

castable env t1 t2@(TVar _ tv)              = False

castable env t1 (TOpt _ t2)                 = castable env t1 t2

castable env t1 t2                          = False


maxtype env (t:ts)                          = maxt t ts
  where maxt top (t:ts)
          | castable env t top              = maxt top ts
          | otherwise                       = maxt t ts
        maxt top []                         = top

