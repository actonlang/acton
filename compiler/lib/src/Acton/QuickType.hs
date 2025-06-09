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

module Acton.QuickType where

import Acton.Syntax
import Acton.Names
import Acton.Subst
import Acton.Env
import Acton.Builtin
import Acton.Prim
import Utils


class QType a where
    qType                           :: EnvF x -> Checker -> a -> (Type, Type, a)
    qMatch                          :: Checker -> Type -> Type -> a -> a

type Checker                        = Type -> Type -> Expr -> Expr

accept                              :: Checker
accept t t' e                       = e

typecast                            :: Checker
typecast t t' e
  | t == t'                         = e
  | otherwise                       = eCAST t t' e


typeOf env x                        = t
  where (t, fx, x')                 = qType env accept x

fxOf env x                          = fx
  where (t, fx, x')                 = qType env accept x

typeInstOf env ts e                 = t
  where (t, fx, e')                 = qInst env accept ts e

schemaOf env e                      = (sc, dec)
  where (sc, fx, dec, e')           = qSchema env accept e

closedType                          :: EnvF x -> Expr -> Bool
closedType env (Var _ n)            = isClosed $ findQName n env
closedType env (Dot _ (Var _ x) n)
  | NClass q _ _ _ <- findQName x env = closedAttr env (TC x (map tVar $ qbound q)) n
closedType env (Dot _ e n)          = case typeOf env e of
                                        TCon _ c -> closedAttr env c n
                                        TVar _ v  -> closedAttr env (findTVBound env v) n
                                        TTuple _ p k -> n `notElem` valueKWs
closedType env (TApp _ e _)         = closedType env e
closedType env (Async _ e)          = closedType env e
closedType env _                    = True


qSchema                             :: EnvF x -> Checker -> Expr -> (TSchema, Type, Maybe Deco, Expr)
qSchema env f e@(Var _ n)           = case findQName n env of
                                        NVar t ->
                                            (monotype t, fxPure, Nothing, e)
                                        NSVar t ->
                                            (monotype t, fxPure, Nothing, e)
                                        NDef sc dec _ ->
                                            (sc, fxPure, Just dec, e)
                                        NSig sc dec _ ->
                                            (sc, fxPure, Just dec, e)
                                        NClass q _ _ _ ->
                                            let tc = TC (unalias env n) (map tVar $ qbound q)
                                                (TSchema _ q' t, _) = findAttr' env tc initKW
                                                t' = if restype t == tR then t else t{ restype = tSelf }
                                            in (tSchema (q++q') $ subst [(tvSelf,tCon tc)] t', fxPure, Just NoDec, e)
                                        NAct q p k _ _ ->
                                            (tSchema q (tFun fxProc p k (tCon0 n q)), fxPure, Just NoDec, e)
                                        i -> error ("### qSchema Var unexpected " ++ prstr (noq n,i))
qSchema env f e@(Dot _ (Var _ x) n)
  | NClass q _ _ _ <- info          = let tc = TC x (map tVar $ qbound q)
                                          (TSchema _ q' t, mbdec) = findAttr' env tc n
                                      in (tSchema (q++q') $ subst [(tvSelf,tCon tc)] (addSelf t mbdec), fxPure, mbdec, e)
  where info                        = findQName x env
qSchema env f e0@(Dot l e n)        = case t of
                                        TCon _ c -> addE e' $ findAttr' env c n
                                        TTuple _ p k
                                          | n `elem` valueKWs -> addE e' $ findAttr' env cValue n
                                          | otherwise -> addE e' $ (monotype $ pick n k, Nothing)
                                        TVar _ v  -> addE e' $ findAttr' env tc n
                                           where tc = findTVBound env v
                                        t -> error ("### qSchema Dot unexpected " ++ prstr e0 ++ "  ::  " ++ prstr t)
  where (t, fx, e')                 = qType env f e
        addE e1 (sc, dec)           = (subst [(tvSelf,t)] sc, fx, dec, Dot l e1 n)
        pick n (TRow l k x t r)     = if x == n then t else pick n r
        pick n (TStar l k r)
          | n == attrKW             = tTupleK r
        pick n (TNil l k)
          | n == attrKW             = tTupleK (tNil KRow)
        pick n _                    = error ("### qSchema Dot " ++ prstr n ++ " on " ++ prstr e0 ++ "  ::  "  ++ prstr t)
qSchema env f e                     = (monotype t, fx, Nothing, e')
  where (t, fx, e')                 = qType env f e

qInst                               :: EnvF x -> Checker -> [Type] -> Expr -> (Type, Type, Expr)
qInst env f [] (TApp _ e ts)        = qInst env f ts e
qInst env f ts e                    = case qSchema env f e of
                                        (TSchema _ q t, fx, _, e') | length q == length ts -> (t', fx, tApp e' ts)
                                           where t' = subst (qbound q `zip` ts) t
                                        (sc, _, _, _) -> error ("###### qInst [" ++ prstrs ts ++ "] " ++ prstr e ++ " is " ++ prstr sc)

instance QType Expr where
    qType env f e@Var{}             = qInst env f [] e
    qType env f e@Dot{}             = qInst env f [] e
    qType env f (TApp _ e ts)       = qInst env f ts e
    qType env f e@(Int _ _ _)       = (tInt, fxPure, e)
    qType env f e@(Float _ _ s)     = (tFloat, fxPure, e)
    qType env f e@(Bool _ _)        = (tBool, fxPure, e)
    qType env f e@(None _)          = (tNone, fxPure, e)
    qType env f e@(Strings _ _)     = (tStr, fxPure, e)
    qType env f e@(BStrings _ _)    = (tBytes, fxPure, e)
--  qType env f (Imaginary _ i s)   = undefined
--  qType env f (NotImplemented _)  = undefined
--  qType env f (Ellipsis _)        = undefined
    qType env f e0@(Call l e ps ks)
      | TFun{} <- t                 = --trace ("## qType Call " ++ prstr e0 ++ ", t = " ++ prstr t) $
                                      (restype t, fx', Call l e' (qMatch f p (posrow t) ps') (qMatch f k (kwdrow t) ks'))
      | otherwise                   = error ("###### qType Fun " ++ prstr e ++ " : " ++ prstr t)
      where (t, fx, e')             = qType env f e
            (p, fxp, ps')           = qType env f ps
            (k, fxk, ks')           = qType env f ks
            fx'                     = upbound env [fx,fxp,fxk,effect t]
    qType env f (Async l e)         = case t of
                                        TFun _ (TFX _ FXAction) p k t' -> (tFun fxProc p k (tMsg t'), fx, Async l e')
      where (t, fx, e')             = qType env f e
    qType env f (Await l e)         = case t of
                                        TCon _ (TC c [t]) | c == qnMsg -> (t, fxProc, Await l e')
      where (t, fx, e')             = qType env f e
    qType env f (BinOp l e1 op e2)  = (t, fxPure, BinOp l (qMatch f t1 t e1') op (qMatch f t2 t e2'))
      where (t1, fx1, e1')          = qType env f e1
            (t2, fx2, e2')          = qType env f e2
            t                       = upbound env [t1,t2]
            fx                      = upbound env [fx1,fx2]
    qType env f (UnOp l Not e)      = (tBool, fx, UnOp l Not (qMatch f t tBool e'))
      where (t, fx, e')             = qType env f e
    qType env f (Cond l e1 e e2)    = (t', fx', Cond l (qMatch f t1 t' e1') (qMatch f t tBool e') (qMatch f t2 t' e2'))
      where (t1, fx1, e1')          = qType env f e1
            (t, fx, e')             = qType env f e
            (t2, fx2, e2')          = qType env f e2
            t'                      = upbound env [t1,t2]
            fx'                     = upbound env [fx1,fx,fx2]
    qType env f (IsInstance l e c)  = (tBool, fx, IsInstance l e' c)
      where (t, fx, e')             = qType env f e
    qType env f (DotI l e i)        = case t of
                                        TTuple _ p _ -> (pick i p, fx, DotI l e' i)
      where (t, fx, e')             = qType env f e
            pick i (TRow _ _ _ t' p) = if i == 0 then t' else pick (i-1) p
    qType env f (RestI l e i)       = case t of
                                        TTuple _ p _ -> (TTuple NoLoc (pick i p) kwdNil, fx, RestI l e' i)
      where (t, fx, e')             = qType env f e
            pick i (TRow l k x t r) = if i == 0 then r else TRow l k x t (pick (i-1) r)
            pick i (TNil l k)       = TNil l k
    qType env f (Rest l e n)        = case t of
                                        TTuple _ p k -> (TTuple NoLoc posNil (pick n k), fx, Rest l e' n)
      where (t, fx, e')             = qType env f e
            pick n (TRow l k x t r) = if x == n then r else TRow l k x t (pick n r)
            pick n (TNil l k)       = TNil l k
    qType env f (Lambda l p k e fx) = (TFun NoLoc fx (prowOf p) (krowOf k) t, fxPure, Lambda l p k e' fx)
      where (t, _, e')              = qType env1 f e
            env1                    = define (envOf k) $ define (envOf p) env
    qType env f (Tuple l ps ks)     = (TTuple NoLoc p k, fx, Tuple l ps' ks')
      where (p, fxp, ps')           = qType env f ps
            (k, fxk, ks')           = qType env f ks
            fx                      = upbound env [fxp,fxk]
    qType env f (List l es)         = (tList (upbound env ts), upbound env fxs, List l es')
      where (ts, fxs, es')          = unzip3 $ map (qType env f) es
    qType env f (ListComp l e c)    = (tList t, upbound env [fxc,fx], ListComp l e' c')
      where (_, fxc, c')            = qType env f c
            (t, fx, e')             = qType env1 f e
            env1                    = define (envOf c) env
    qType env f (SetComp l e c)     = (tSet t, upbound env [fxc,fx], SetComp l e' c')
      where (_, fxc, c')            = qType env f c
            (t, fx, e')             = qType env1 f e
            env1                    = define (envOf c) env
    qType env f (DictComp l a c)    = (tDict tk tv, upbound env [fxc,fx], DictComp l a' c')
      where (_, fxc, c')            = qType env f c
            (TTuple _ tk tv, fx, a') = qType env1 f a
            env1                    = define (envOf c) env
    qType env f (Dict l as)         = (tDict (upbound env ts1) (upbound env ts2), upbound env (fxk++fxv), Dict l (zipWith Assoc ks vs))
      where (ts1, fxk, ks)          = unzip3 $ [ qType env f k | Assoc k v <- as ]
            (ts2, fxv, vs)          = unzip3 $ [ qType env f v | Assoc k v <- as ]
    qType env f (Set l es)          = (tSet (upbound env ts), upbound env fxs, Set l es')
      where (ts, fxs, es')          = unzip3 $ map (qType env f) es
    qType env f (Paren l e)         = (t, fx, Paren l e')
      where (t, fx, e')             = qType env f e
    qType env f (Box t e)           = (t, fx, Box t e)
      where (_, fx, e')             = qType env f e
    qType env f (UnBox t e)         = (t, fx, UnBox t e)
      where (_, fx, e')             = qType env f e
    qType env f e                   = error ("qType, e = " ++ show e)
   
    qMatch f t t' e                 = f t t' e

instance QType Elem where
    qType env f (Elem e)            = (t, fx, Elem e')
      where (t, fx, e')             = qType env f e

    qMatch f t t' (Elem e)          = Elem (qMatch f t t' e)

instance QType Assoc where
    qType env f (Assoc ek ev)       = (tTuple tk tv, upbound env [fxk,fxv], Assoc ek' ev')
      where (tk, fxk, ek')          = qType env f ek
            (tv, fxv, ev')          = qType env f ev

    qMatch f t t' a                 = a

instance QType PosArg where
    qType env f (PosArg e p)        = (posRow t r, upbound env [fx,fxp], PosArg e' p')
      where (t, fx, e')             = qType env f e
            (r, fxp, p')            = qType env f p
    qType env f (PosStar e)         = case t of TTuple _ p _ -> (p, fx, PosStar e')
      where (t, fx, e')             = qType env f e
    qType env f PosNil              = (posNil, fxPure, PosNil)

    qMatch f TVar{} r p             = p
    qMatch f r TVar{} p             = p
    qMatch f r r' (PosArg e p)
      | TRow{} <- r,
        TRow{} <- r'                = PosArg (qMatch f (rtype r) (rtype r') e) (qMatch f (rtail r) (rtail r') p)
      | otherwise                   = error ("#### rtail " ++ prstr r ++ " < " ++ prstr r' ++ " for " ++ prstr (PosArg e p))
    qMatch f _ _ PosNil             = PosNil

instance QType KwdArg where
    qType env f (KwdArg n e k)      = (kwdRow n t r, upbound env [fx,fxk], KwdArg n e' k')
      where (t, fx, e')             = qType env f e
            (r, fxk, k')            = qType env f k
    qType env f (KwdStar e)         = case t of TTuple _ _ k -> (k, fx, KwdStar e')
      where (t, fx, e')             = qType env f e
    qType env f KwdNil              = (kwdNil, fxPure, KwdNil)

    qMatch f TVar{} r k             = k
    qMatch f r TVar{} k             = k
    qMatch f r r' (KwdArg n e k)
      | TRow{} <- r,
        TRow{} <- r'                = KwdArg n (qMatch f (rtype r) (rtype r') e) (qMatch f (rtail r) (rtail r') k)
      | otherwise                   = error ("#### rtail " ++ prstr r ++ " < " ++ prstr r' ++ " for " ++ prstr (KwdArg n e k))
    qMatch f _ _ KwdNil             = KwdNil

instance QType Pattern where
    qType env f (PWild l (Just t))  = (t, fxPure, PWild l (Just t))
    qType env f (PVar l n (Just t)) = (t, fxPure, PVar l n (Just t))
    qType env f (PVar l n Nothing)  = (typeOf env (eVar n), fxPure, PVar l n Nothing)
    qType env f (PTuple l ps ks)    = (tTuple (typeOf env ps) (typeOf env ks), fxPure, PTuple l ps ks)
    qType env f (PList l ps p)      = (tList (typeOf env $ head ps), fxPure, PList l ps p)
    qType env f (PParen l p)        = (typeOf env p, fxPure, PParen l p)
    
    qMatch f t t' p                 = p

instance QType PosPat where
    qType env f (PosPat p ps)       = (posRow (typeOf env p) (typeOf env ps), fxPure, PosPat p ps)
    qType env f (PosPatStar p)      = (typeOf env p, fxPure, PosPatStar p)
    qType env f PosPatNil           = (posNil, fxPure, PosPatNil)
    
    qMatch f r r' p                 = p

instance QType KwdPat where
    qType env f (KwdPat n p ps)     = (kwdRow n (typeOf env p) (typeOf env ps), fxPure, KwdPat n p ps)
    qType env f (KwdPatStar p)      = (typeOf env p, fxPure, KwdPatStar p)
    qType env f KwdPatNil           = (kwdNil, fxPure, KwdPatNil)
    
    qMatch f r r' p                 = p

instance QType Comp where
    qType env f (CompFor l p e c)   = (tNone, upbound env [fx,fxc], CompFor l p (qMatch f t (typeOf env p) e') c')
      where (t, fx, e')             = qType env f e
            (_, fxc, c')            = qType env1 f c
            env1                    = define (envOf p) env
    qType env f (CompIf l e c)      = (tNone, upbound env [fx,fxc], CompIf l e' c')
      where (t, fx, e')             = qType env f e
            (_, fxc, c')            = qType env f c
    qType env f NoComp              = (tNone, fxPure, NoComp)

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
    envOf p                         = error ("### BAD envOf " ++ prstr p)

instance EnvOf KwdPar where
    envOf (KwdPar n (Just t) _ k)   = (n,NVar t) : envOf k
    envOf (KwdSTAR n (Just t))      = [(n,NVar t)]
    envOf KwdNIL                    = []

instance EnvOf Comp where
    envOf (CompFor _ p e c)         = envOf p ++ envOf c
    envOf (CompIf _ e c)            = envOf c
    envOf NoComp                    = []

instance EnvOf Pattern where
    envOf (PWild _ (Just t))        = []
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
    envOf (Signature _ ns sc dec)   = [ (n, NSig sc dec Nothing) | n <- ns ]
    envOf (If _ [Branch e ss] fin)
      | isPUSHF e                   = envOf ss ++ envOf fin
    envOf (If _ bs els)             = commonEnvOf ([ ss | Branch _ ss <- bs ] ++ [els])
    envOf (Try _ b hs els fin)      = commonEnvOf ([ ss | Handler _ ss <- hs ] ++ [b++els]) ++ envOf fin
    envOf (With _ items b)          = envOf b `exclude` bound items
    envOf s                         = []

commonEnvOf suites
  | null liveSuites                 = []
  | otherwise                       = foldl restrict (envOf $ head liveSuites) (map bound $ tail liveSuites)
  where liveSuites                  = filter fallsthru suites

instance EnvOf Decl where
    envOf (Def _ n q p k (Just t) b dec fx)
                                    = [(n, NDef (TSchema NoLoc q $ TFun NoLoc fx (prowOf p) (krowOf k) t) dec Nothing)]
    envOf (Class _ n q as ss)       = [(n, NClass q (leftpath as) (map dropDefSelf $ envOf ss) Nothing)]

    envOf (Actor _ n q p k ss)      = [(n, NAct q (prowOf p) (krowOf k) (map wrap te) Nothing)]
      where te                      = filter (not . isHidden . fst) $ envOf ss `exclude` statevars ss
            wrap (n, NDef sc dec doc) = (n, NDef (wrapFX sc) dec doc)
            wrap (n, i)             = (n, i)
            wrapFX (TSchema l q t)  = TSchema l q (if effect t == fxProc then t{ effect = fxAction } else t)

dropDefSelf (n, NDef (TSchema l q t) dec doc)
                                    = (n, NDef (TSchema l q (dropSelf t dec)) dec doc)
dropDefSelf (n, i)                  = (n, i)

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


upbound env ts                      = case lubfold env ts of Just u -> u

nvarsOf te                          = [ (n,t) | (n, NVar t) <- te ]

updatesOf stmts                     = concatMap upd stmts
  where upd (Assign _ p _)          = concatMap uvars p
        upd (If _ bs els)           = concat [ updatesOf ss | Branch _ ss <- bs ] ++ updatesOf els
        upd (While _ _ ss els)      = updatesOf ss ++ updatesOf els
        upd (Try _ ss hs els fin)   = updatesOf ss ++ concat [ updatesOf ss | Handler _ ss <- hs ] ++ updatesOf els ++ updatesOf fin
        upd _                       = []
        uvars (PVar _ v Nothing)    = [v]
        uvars (PParen _ p)          = uvars p
        uvars (PTuple _ ps ks)      = uvarsP ps ++ uvarsK ks
        uvars (PList _ ps mbp)      = concatMap uvars ps ++ maybe [] uvars mbp
        uvars _                     = []
        uvarsP (PosPat p r)         = uvars p ++ uvarsP r
        uvarsP (PosPatStar p)       = uvars p
        uvarsP (PosPatNil)          = []
        uvarsK (KwdPat _ k r)       = uvars k ++ uvarsK r
        uvarsK (KwdPatStar k)       = uvars k
        uvarsK (KwdPatNil)          = []
