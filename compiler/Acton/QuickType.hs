module Acton.QuickType where

import Acton.Syntax
import Acton.Names
import Acton.Subst
import Acton.Env
import Acton.Builtin
import Acton.Prim
import Utils


class QType a where
    qType                           :: EnvF x -> Checker -> a -> (Type, a)
    qMatch                          :: Checker -> Type -> Type -> a -> a

type Checker                        = Type -> Type -> Expr -> Expr

accept                              :: Checker
accept t t' e                       = e

typecast                            :: Checker
typecast t t' e
  | t == t'                         = e
  | otherwise                       = eCall (tApp (eQVar primCAST) [t,t']) [e]


typeOf env x                        = fst $ qType env accept x

typeInstOf env ts e                 = t
  where (t0, t, e')                 = qInst env accept ts e

schemaOf env e                      = (sc, dec)
  where (sc, dec, e')               = qSchema env accept e

closedType                          :: EnvF x -> Expr -> Bool
closedType env (Var _ n)            = isClosed $ findQName n env
closedType env (Dot _ (Var _ x) n)
  | NClass q _ _ <- findQName x env = case findAttrInfo env (TC x (map tVar $ tybound q)) n of
                                        Just (w,i) -> isClosed i
closedType env (Dot _ e n)          = case typeOf env e of
                                        TCon _ c -> case findAttrInfo env c n of Just (w,i) -> isClosed i
                                        TVar _ v  -> case findAttrInfo env (findTVBound env v) n of Just (w,i) -> isClosed i
                                        TTuple _ p k -> True
closedType env (TApp _ e _)         = closedType env e
closedType env _                    = True

isClosed (NVar _)                   = True
isClosed (NSVar _)                  = True
isClosed (NSig _ Property)          = True
isClosed (NSig sc _)
  | TFun{} <- sctype sc             = False
  | otherwise                       = True      -- 'closed' ~ 'not a function'
isClosed _                          = False



qSchema                             :: EnvF x -> Checker -> Expr -> (TSchema, Maybe Deco, Expr)
qSchema env f e@(Var _ n)           = case findQName n env of
                                        NVar t ->
                                            (monotype t, Nothing, e)
                                        NSVar t ->
                                            (monotype t, Nothing, e)
                                        NDef sc dec ->
                                            (sc, Just dec, e)
                                        NSig sc dec ->
                                            (sc, Just dec, e)
                                        NClass q _ _ ->
                                            let tc = TC n (map tVar $ tybound q)
                                                (TSchema _ q' t, _) = findAttr' env tc initKW
                                                t' = if restype t == tR then t else t{ restype = tSelf }
                                            in (tSchema (q++q') $ subst [(tvSelf,tCon tc)] t', Just NoDec, e)
                                        NAct q p k _ ->
                                            (tSchema q (tFun fxAction p k (tCon0 n q)), Just NoDec, e)
                                        i -> error ("### qSchema Var unexpected " ++ prstr (noq n,i))
qSchema env f e@(Dot _ (Var _ x) n)
  | NClass q _ _ <- info            = let tc = TC x (map tVar $ tybound q)
                                          (TSchema _ q' t, mbdec) = findAttr' env tc n
                                      in (tSchema (q++q') $ subst [(tvSelf,tCon tc)] (addSelf t mbdec), mbdec, e)
  where info                        = findQName x env
qSchema env f e0@(Dot l e n)        = case t of
                                        TCon _ c -> addE $ findAttr' env c n
                                        TTuple _ p k -> addE $ (monotype $ pick n k, Nothing)
                                        TVar _ v  -> addE $ findAttr' env (findTVBound env v) n
                                        t -> error ("### qSchema Dot unexpected " ++ prstr e0 ++ "  ::  " ++ prstr t)
  where (t, e')                     = qType env f e
        addE (sc, dec)              = (sc, dec, Dot l e' n)
        pick n (TRow l k x t r)     = if x == n then t else pick n r
qSchema env f e                     = (monotype t, Nothing, e')
  where (t, e')                     = qType env f e

qInst                               :: EnvF x -> Checker -> [Type] -> Expr -> (Type, Type, Expr)
qInst env f [] (TApp _ e ts)        = qInst env f ts e
qInst env f ts e                    = case qSchema env f e of
                                        (TSchema _ q t, _, e') | length q == length ts -> (t, t', tApp e' ts)
                                           where t' = subst (tybound q `zip` ts) t
                                        (sc, _, _) -> error ("###### qInst [" ++ prstrs ts ++ "] " ++ prstr e ++ " is " ++ prstr sc)

instance QType Expr where
    qType env f e@Var{}             = let (_,t,e') = qInst env f [] e in (t, e')
    qType env f e@Dot{}             = let (_,t,e') = qInst env f [] e in (t, e')
    qType env f (TApp _ e ts)       = let (_,t,e') = qInst env f ts e in (t, e')
    qType env f e@(Int _ _ _)       = (tInt, e)
    qType env f e@(Float _ _ s)     = (tFloat, e)
    qType env f e@(Bool _ _)        = (tBool, e)
    qType env f e@(None _)          = (tNone, e)
    qType env f e@(Strings _ _)     = (tStr, e)
    qType env f e@(BStrings _ _)    = (tBytes, e)
--  qType env f (Imaginary _ i s)   = undefined
--  qType env f (NotImplemented _)  = undefined
--  qType env f (Ellipsis _)        = undefined
    qType env f (Call l e ps ks)
      | TFun{} <- t, TFun{} <- t'   = (restype t', qMatch f (restype t) (restype t') $ Call l e' (qMatch f p (posrow t) ps') (qMatch f k (kwdrow t) ks'))
      | otherwise                   = error ("###### qType Fun " ++ prstr e ++ " : " ++ prstr t)
      where (t, t', e')             = qInst env f [] e
            (p, ps')                = qType env f ps
            (k, ks')                = qType env f ks
    qType env f (Async l e)         = (tMsg t, Async l e')
      where (t, e')                 = qType env f e
    qType env f (Await l e)         = case t of
                                        TCon _ (TC c [t]) | qualEq env c qnMsg -> (t, Await l e')
      where (t, e')                 = qType env f e
    qType env f (BinOp l e1 Or e2)  = (tBool, BinOp l (qMatch f t1 tBool e1) Or (qMatch f t2 tBool e2'))
      where (t1, e1')               = qType env f e1
            (t2, e2')               = qType env f e2
    qType env f (BinOp l e1 And e2) = (tBool, BinOp l (qMatch f t1 tBool e1) And (qMatch f t2 tBool e2'))
      where (t1, e1')               = qType env f e1
            (t2, e2')               = qType env f e2
    qType env f (UnOp l Not e)      = (tBool, UnOp l Not (qMatch f t tBool e'))
      where (t, e')                 = qType env f e
    qType env f (Cond l e1 e e2)    = (t', Cond l (qMatch f t1 t' e1') (qMatch f t tBool e') (qMatch f t2 t' e2'))
      where (t1, e1')               = qType env f e1
            (t, e')                 = qType env f e
            (t2, e2')               = qType env f e2
            t'                      = maxtype env [t1,t2]
    qType env f (IsInstance l e c)  = (tBool, IsInstance l e' c)
      where (t, e')                 = qType env f e
    qType env f (DotI l e i)        = case t of
                                        TTuple _ p _ -> (pick i p, qMatch f tWild (pick i p) $ DotI l e' i)
      where (t, e')                 = qType env f e
            pick i (TRow _ _ _ t' p) = if i == 0 then t' else pick (i-1) p
    qType env f (RestI l e i)       = case t of
                                        TTuple _ p _ -> (TTuple NoLoc (pick i p) kwdNil, RestI l e' i)
      where (t, e')                 = qType env f e
            pick i (TRow l k x t r) = if i == 0 then r else TRow l k x t (pick (i-1) r)
            pick i (TNil l k)       = TNil l k
    qType env f (Rest l e n)        = case t of
                                        TTuple _ p k -> (TTuple NoLoc posNil (pick n k), Rest l e' n)
      where (t, e')                 = qType env f e
            pick n (TRow l k x t r) = if x == n then r else TRow l k x t (pick n r)
            pick n (TNil l k)       = TNil l k
    qType env f (Lambda l p k e fx) = (TFun NoLoc fx (prowOf p) (krowOf k) t, Lambda l p k e' fx)
      where (t, e')                 = qType env1 f e
            env1                    = define (envOf k) $ define (envOf p) env
    qType env f (Tuple l ps ks)     = (TTuple NoLoc p k, Tuple l ps' ks')
      where (p, ps')                = qType env f ps
            (k, ks')                = qType env f ks
    qType env f (List l es)         = (tList (maxtype env ts), List l es')
      where (ts, es')               = unzip $ map (qType env f) es
    qType env f (ListComp l e c)    = (tList t, ListComp l e' c')
      where (_, c')                 = qType env f c
            (t, e')                 = qType env1 f e
            env1                    = define (envOf c) env
    qType env f (Paren l e)         = (t, Paren l e')
      where (t, e')                 = qType env f e

    qMatch f t t' e                 = f t t' e

instance QType Elem where
    qType env f (Elem e)            = (t, Elem e')
      where (t, e')                 = qType env f e

    qMatch f t t' (Elem e)          = Elem (qMatch f t t' e)

instance QType PosArg where
    qType env f (PosArg e p)        = (posRow t r, PosArg e' p')
      where (t, e')                 = qType env f e
            (r, p')                 = qType env f p
    qType env f (PosStar e)         = case t of TTuple _ p _ -> (p, PosStar e')
      where (t, e')                 = qType env f e
    qType env f PosNil              = (posNil, PosNil)

    qMatch f TVar{} r p             = p
    qMatch f r TVar{} p             = p
    qMatch f r r' (PosArg e p)      = PosArg (qMatch f (rtype r) (rtype r') e) (qMatch f (rtail r) (rtail r') p)
    qMatch f _ _ PosNil             = PosNil

instance QType KwdArg where
    qType env f (KwdArg n e k)      = (kwdRow n t r, KwdArg n e' k')
      where (t, e')                 = qType env f e
            (r, k')                 = qType env f k
    qType env f (KwdStar e)         = case t of TTuple _ _ k -> (k, KwdStar e')
      where (t, e')                 = qType env f e
    qType env f KwdNil              = (kwdNil, KwdNil)

    qMatch f TVar{} r k             = k
    qMatch f r TVar{} k             = k
    qMatch f r r' (KwdArg n e k)    = KwdArg n (qMatch f (rtype r) (rtype r') e) (qMatch f (rtail r) (rtail r') k)
    qMatch f _ _ KwdNil             = KwdNil

instance QType Pattern where
    qType env f (PVar l n (Just t)) = (t, PVar l n (Just t))
    qType env f (PVar l n Nothing)  = (typeOf env (eVar n), PVar l n Nothing)
    qType env f (PTuple l ps ks)    = (tTuple (typeOf env ps) (typeOf env ks), PTuple l ps ks)
    qType env f (PList l ps p)      = (tList (typeOf env $ head ps), PList l ps p)
    qType env f (PParen l p)        = (typeOf env p, PParen l p)
    
    qMatch f t t' p                 = p

instance QType PosPat where
    qType env f (PosPat p ps)       = (posRow (typeOf env p) (typeOf env ps), PosPat p ps)
    qType env f (PosPatStar p)      = (typeOf env p, PosPatStar p)
    qType env f PosPatNil           = (posNil, PosPatNil)
    
    qMatch f r r' p                 = p

instance QType KwdPat where
    qType env f (KwdPat n p ps)     = (kwdRow n (typeOf env p) (typeOf env ps), KwdPat n p ps)
    qType env f (KwdPatStar p)      = (typeOf env p, KwdPatStar p)
    qType env f KwdPatNil           = (kwdNil, KwdPatNil)
    
    qMatch f r r' p                 = p

instance QType Comp where
    qType env f (CompFor l p e c)   = (tNone, CompFor l p (qMatch f t (typeOf env p) e') c')
      where (t, e')                 = qType env f e
            (_, c')                 = qType env1 f c
            env1                    = define (envOf p) env
    qType env f (CompIf l e c)      = (tNone, CompIf l (qMatch f t tBool e') c')
      where (t, e')                 = qType env f e
            (_, c')                 = qType env f c
    qType env f NoComp              = (tNone, NoComp)

    qMatch f t t' c                 = c



class EnvOf a where
    envOf                           :: a -> TEnv

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

instance EnvOf NDSliz where
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

castable env (TOpt _ t1) (TOpt _ t2)        = castable env t1 t2
castable env (TNone _) (TOpt _ t)           = True
castable env (TNone _) (TNone _)            = True

castable env (TFX _ fx1) (TFX _ fx2)        = castable' fx1 fx2
  where castable' FXPure   FXPure           = True
        castable' FXPure   FXMut            = True
        castable' FXPure   FXAction         = True
        castable' FXMut    FXMut            = True
        castable' FXMut    FXAction         = True
        castable' FXAction FXAction         = True
        castable' FXAsync  FXAsync          = True
        castable' FXAsync  FXAction         = True
        castable' fx1      fx2              = False

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
maxtype env []                              = tWild


----------------------------------------------------------------------------------------------------------------------
-- extends predicate
----------------------------------------------------------------------------------------------------------------------
{-
extends                                     :: EnvF x -> Type -> QName -> Maybe ([Type],Expr)
extends env (TCon _ c) pn                   = case findWitness env (tcname c) (implProto' env pn) of
                                                Just (WClass q p w ws) -> [ constr (subst s (tVar v)) (subst s u) | Quant v us <- q, u <- us ]
                                                 where s = tybound q `zip` tcargs c
                                                Just (WInst p w ws) -> Just (tcargs p, wexpr ws (eQVar w))
                                                Nothing -> Nothing

extends'                                    :: EnvF x -> Type -> TCon -> Bool
extends env (TCon _ c) p                    = case findWitness env (tcname c) (implProto env p) of
                                                Just (WClass q p w ws) -> and [ extends' env (subst s $ tVar v) (subst s u) | Quant v us <- q, u <- us ]
                                                    where s = tybound q `zip` tcargs c
                                                Just (WInst p w ws) -> True
                                                Nothing -> False
-}