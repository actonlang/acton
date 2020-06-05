{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Acton.Solver where

import Control.Monad
import qualified Data.Map.Strict as Map

import Utils
import Acton.Syntax
import Acton.Builtin
import Acton.Names
import Acton.Prim
import Acton.Env




-- Reduce conservatively and remove entailed constraints
simplify                                    :: Env -> Constraints -> TypeM (Constraints,Equations)
simplify env cs                             = simplify' env [] cs
  where simplify' env eq cs                 = do --traceM ("### simplify: " ++ prstrs cs)
                                                 eq1 <- reduce env eq cs
                                                 cs0 <- collectDeferred
                                                 cs1 <- msubst cs0
                                                 if simple cs1
                                                     -- then return (cs1,eq1)
                                                     then return ([],eq1)
                                                     else simplify' env eq1 cs1
        simple cs                           = True                              -- TODO: add proper test

-- Reduce aggressively or fail
solve                                       :: Env -> Constraints -> TypeM Equations
solve env cs                                = solve' env [] cs
  where solve' env eq cs                    = do --traceM ("### solve: " ++ prstrs cs)
                                                 eq1 <- reduce env eq cs
                                                 cs0 <- collectDeferred
                                                 cs1 <- msubst cs0
                                                 if done cs1
                                                     then return eq1
                                                     else solve' env eq1 cs1
        done cs                             = True                              -- TODO: ensure proper termination...!


----------------------------------------------------------------------------------------------------------------------
-- reduce
----------------------------------------------------------------------------------------------------------------------

type Equation                               = (Name, Type, Expr)
type Equations                              = [Equation]

instance Subst Equation where
    msubst (w, t, e)                        = do t <- msubst t
                                                 e <- msubst e
                                                 return (w, t, e)
    
    tyfree (w, t, e)                        = tyfree t ++ tyfree e


data Polarity                               = Neg | Inv | Pos deriving (Show)

reduce                                      :: Env -> Equations -> Constraints -> TypeM Equations
reduce env eq []                            = return eq
reduce env eq (c:cs)                        = do c' <- msubst c
                                                 eq1 <- reduce' env eq c'
                                                 cs' <- msubst cs
                                                 reduce env eq1 cs'

reduce'                                     :: Env -> Equations -> Constraint -> TypeM Equations
reduce' env eq (Cast t1 t2)                 = do cast' env t1 t2
                                                 return eq

reduce' env eq (Sub w t1 t2)                = sub' env eq w t1 t2

reduce' env eq c@(Impl w t@(TVar _ tv) p)
  | not $ scoped tv env                     = do defer [c]; return eq
  | Just wit <- search                      = do (cs,p',we) <- instWitness env [] wit
                                                 unifyM env (tcargs p) (tcargs p')
                                                 reduce env ((w, impl2type t p, we):eq) cs
  where search                              = findWitness env (NoQ $ tvname tv) (tcname p ==)
  
reduce' env eq c@(Impl w t@(TCon _ tc) p)
  | Just wit <- search                      = do (cs,p',we) <- instWitness env (tcargs tc) wit
                                                 unifyM env (tcargs p) (tcargs p')
                                                 reduce env ((w, impl2type t p, we):eq) cs
  where search                              = findWitness env (tcname tc) (tcname p ==)

reduce' env eq c@(Sel w t1@(TVar _ tv) n t2)
  | not $ scoped tv env                     = do defer [c]; return eq
  | Just (_,sc,dec) <- findTVAttr env tv n  = do (cs,t) <- instantiate env sc
                                                 -- when (tvSelf `elem` contrafree t) (err1 n "Contravariant Self attribute not selectable by instance")
                                                 let e = eLambda [(x0,t1)] (app t (eDot (eVar x0) n) $ witsOf cs)
                                                     cs' = Cast (subst [(tvSelf,t1)] t) t2 : cs
                                                 reduce env ((w, wFun t1 t2, e):eq) cs'
  | Just wit <- search                      = do (cs1,p,we) <- instWitness env [] wit
                                                 let Just (wf,sc,dec) = findAttr env p n
                                                 (cs2,t) <- instantiate env sc
                                                 let e = eLambda [(x0,t1)] (app t (eDot (wf we) n) $ eVar x0 : witsOf cs2) -- witnesses *after* object ref
                                                     cs' = Cast (subst [(tvSelf,t1)] t) t2 : cs1 ++ cs2
                                                 reduce env ((w, wFun t1 t2, e):eq) cs'
  | otherwise                               = err1 n "Attribute not found"
  where search                              = findWitness env (NoQ $ tvname tv) (hasAttr env n)

reduce' env eq (Sel w t1@(TCon _ tc) n t2)
  | Just (wf,sc,dec) <- findAttr env tc n   = do (cs,t) <- instantiate env sc
                                                 -- when (tvSelf `elem` contrafree t) (err1 n "Contravariant Self attribute not selectable by instance")
                                                 let e = eLambda [(x0,t1)] (app t (eDot (eVar x0) n) $ witsOf cs)
                                                     cs' = Cast (subst [(tvSelf,t1)] t) t2 : cs
                                                 reduce env ((w, wFun t1 t2, e):eq) cs'
  | Just wit <- search                      = do (cs1,p,we) <- instWitness env (tcargs tc) wit
                                                 let Just (wf,sc,dec) = findAttr env p n
                                                 (cs2,t) <- instantiate env sc
                                                 let e = eLambda [(x0,t1)] (app t (eDot (wf we) n) $ eVar x0 : witsOf cs2) -- witnesses *after* object ref
                                                     cs' = Cast (subst [(tvSelf,t1)] t) t2 : cs1 ++ cs2
                                                 reduce env ((w, wFun t1 t2, e):eq) cs'
  | otherwise                               = err1 n "Attribute not found"
  where search                              = findWitness env (tcname tc) (hasAttr env n)

reduce' env eq (Sel w t1@(TExist _ p) n t2)
  | Just (wf,sc,dec) <- findAttr env p n    = do (cs,t) <- instantiate env sc
                                                 when (tvSelf `elem` tyfree t) (err1 n "Self attribute not selectable from abstract type")
                                                 let e = eLambda [(x0,t1)] (app t (eDot (eDot (eVar x0) protoKW) n) $ eDot (eVar x0) implKW : witsOf cs)
                                                     cs' = Cast t t2 : cs
                                                 reduce env ((w, wFun t1 t2, e):eq) cs'
  | otherwise                               = err1 n "Attribute not found:"

reduce' env eq (Sel w t1@(TTuple _ p r) n t2)
                                            = do let e = eLambda [(x0,t1)] (eDot (eVar x0) n)
                                                     cs' = [Cast r (kwdRow n t2 tWild)]
                                                 reduce env ((w, wFun t1 t2, e):eq) cs'

reduce' env eq (Sel w t1@(TUnion _ us) n t2)
                                            = do t <- newTVar
                                                 let cs' = [ Cast (mkTCon u) t | u <- us ] ++ [Sel w t n t2]
                                                 reduce env eq cs'
  where mkTCon (ULit _)                     = tStr
        mkTCon (UCon c)                     = tCon (TC c [])

reduce' env eq c@(Mut t1@(TVar _ tv) n t2)
  | not $ scoped tv env                     = do defer [c]; return eq
  | Just (wf,sc,dec) <- findTVAttr env tv n = do when (dec/=Property) (noMut n)
                                                 (cs,t) <- instantiate env sc
                                                 let cs' = Cast t1 tObject : Cast t2 (subst [(tvSelf,t1)] t) : cs
                                                 reduce env eq cs'
  | otherwise                               = err1 n "Attribute not found:"

reduce' env eq (Mut t1@(TCon _ tc) n t2)
  | Just (wf,sc,dec) <- findAttr env tc n   = do when (dec/=Property) (noMut n)
                                                 (cs,t) <- instantiate env sc
                                                 let cs' = Cast t1 tObject : Cast t2 (subst [(tvSelf,t1)] t) : cs
                                                 reduce env eq cs'
  | otherwise                               = err1 n "Attribute not found:"

reduce' env eq c                            = noRed c


----------------------------------------------------------------------------------------------------------------------
-- cast
----------------------------------------------------------------------------------------------------------------------

cast                                        :: Env -> Type -> Type -> TypeM ()
cast env t1 t2                              = do t1' <- msubst t1
                                                 t2' <- msubst t2
                                                 cast' env t1' t2'

{-  
-- part of computing GLBs
cast' env (TVar _ tv) t2
  | not $ scoped tv env                     = do when (tv `elem` tyfree t2) (infiniteType tv)
                                                 substitute tv t2
-- part of computing LUBs
cast' env t1 (TVar _ tv)
  | not $ scoped tv env                     = do when (tv `elem` tyfree t1) (infiniteType tv)
                                                 substitute tv t1
-}
                                             
cast' env (TVar _ tv1) (TVar _ tv2)
  | tv1 == tv2                              = return ()

cast' env t1@(TVar _ tv) t2
  | not $ scoped tv env                     = defer [Cast t1 t2]
  | Just tc <- findTVBound env tv           = cast' env (tCon tc) t2

cast' env t1 t2@(TVar _ tv)
  | not $ scoped tv env                     = defer [Cast t1 t2]

cast' env (TCon _ c1) (TCon _ c2)
  | Just (wf,c') <- search                  = unifyM env (tcargs c1) (tcargs c')        -- TODO: cast/unify based on polarities
  where search                              = findAncestor env c1 (tcname c2)

cast' env (TExist _ p1) (TExist l p2)
  | tcname p1 == tcname p2                  = unifyM env (tcargs p1) (tcargs p2)

--         as declared           as called
cast' env (TFun _ fx1 p1 k1 t1) (TFun _ fx2 p2 k2 t2)
                                            = do cast env fx1 fx2
                                                 cast env p2 p1
                                                 cast env k2 k1
                                                 cast env t1 t2

cast' env (TTuple _ p1 k1) (TTuple _ p2 k2)
                                            = do cast env p1 p2
                                                 cast env k1 k2

cast' env (TUnion _ u1) (TUnion _ u2)
  | all (uniElem u2) u1                     = return ()
cast' env (TUnion _ u1) t2
  | all (uniLit t2) u1                      = return ()
cast' env (TCon _ (TC c1 [])) (TUnion _ u2)
  | uniCon u2 c1                            = return ()

cast' env (TOpt _ t1) (TOpt _ t2)           = cast env t1 t2
cast' env (TNone _) (TOpt _ t)              = return ()
cast' env t1 (TOpt _ t2)                    = cast env t1 t2
cast' env (TNone _) (TNone _)               = return ()

cast' env (TWild _) t2                      = return ()
cast' env t1 (TWild _)                      = return ()

cast' env (TNil _ k1) (TNil _ k2)
  | k1 == k2                                = return ()
cast' env (TRow _ k n t1 r1) r2             = do (t2,r2') <- findElem k (tNil k) n r2 (rowTail r1)
                                                 cast env t1 t2
                                                 cast env r1 r2'

cast' env (TFX _ fx1) (TFX _ fx2)
  | Just unifs <- castFX env fx1 fx2        = unifs

cast' env t1 t2                             = noRed (Cast t1 t2)


castFX env FXPure FXPure                    = Just $ return ()
castFX env FXPure (FXMut _)                 = Just $ return ()
castFX env FXPure (FXAct _)                 = Just $ return ()
castFX env (FXMut t1) (FXMut t2)            = Just $ unify env t1 t2
castFX env (FXMut t1) (FXAct t2)            = Just $ unify env t1 t2
castFX env (FXAct t1) (FXAct t2)            = Just $ unify env t1 t2
castFX env FXAsync FXAsync                  = Just $ return ()
castFX env fx1 fx2                          = Nothing


----------------------------------------------------------------------------------------------------------------------
-- unify
----------------------------------------------------------------------------------------------------------------------

unify                                       :: Env -> Type -> Type -> TypeM ()
unify env t1 t2                             = do t1' <- msubst t1
                                                 t2' <- msubst t2
                                                 unify' env t1' t2'

unifyM env ts1 ts2                          = mapM_ (uncurry $ unify env) (ts1 `zip` ts2)


unify' env (TVar _ tv1) (TVar _ tv2)
  | tv1 == tv2                              = return ()
unify' env (TVar _ tv) t2
  | not $ scoped tv env                     = do when (tv `elem` tyfree t2) (infiniteType tv)
                                                 substitute tv t2
unify' env t1 (TVar _ tv)
  | not $ scoped tv env                     = do when (tv `elem` tyfree t1) (infiniteType tv)
                                                 substitute tv t1

unify' env (TCon _ c1) (TCon _ c2)
  | tcname c1 == tcname c2                  = unifyM env (tcargs c1) (tcargs c2)

unify' env (TExist _ p1) (TExist l p2)
  | tcname p1 == tcname p2                  = unifyM env (tcargs p1) (tcargs p2)

unify' env (TFun _ fx1 p1 k1 t1) (TFun _ fx2 p2 k2 t2)
                                            = do unify env fx1 fx2
                                                 unify env p2 p1
                                                 unify env k2 k1
                                                 unify env t1 t2

unify' env (TTuple _ p1 k1) (TTuple _ p2 k2)
                                            = do unify env p1 p2
                                                 unify env k1 k2

unify' env (TUnion _ u1) (TUnion _ u2)
  | all (uniElem u2) u1,
    all (uniElem u1) u2                     = return ()

unify' env (TOpt _ t1) (TOpt _ t2)          = unify env t1 t2
unify' env (TNone _) (TNone _)              = return ()

unify' env (TWild _) t2                     = return ()
unify' env t1 (TWild _)                     = return ()

unify' env (TNil _ k1) (TNil _ k2)
  | k1 == k2                                = return ()
unify' env (TRow _ k n t1 r1) r2            = do (t2,r2') <- findElem k (tNil k) n r2 (rowTail r1)
                                                 unify env t1 t2
                                                 unify env r1 r2'

unify' env (TFX _ fx1) (TFX _ fx2)
  | Just unifs <- unifyFX env fx1 fx2       = unifs

unify' env t1 t2                            = noUnify t1 t2


unifyFX env FXPure FXPure                   = Just $ return ()
unifyFX env (FXMut t1) (FXMut t2)           = Just $ unify env t1 t2
unifyFX env (FXAct t1) (FXAct t2)           = Just $ unify env t1 t2
unifyFX env FXAsync FXAsync                 = Just $ return ()
unifyFX env fx1 fx2                         = Nothing


----------------------------------------------------------------------------------------------------------------------
-- sub
----------------------------------------------------------------------------------------------------------------------

sub                                         :: Env -> Equations -> Name -> Type -> Type ->TypeM Equations
sub env eq w t1 t2                          = do t1' <- msubst t1
                                                 t2' <- msubst t2
                                                 sub' env eq w t1' t2'

sub'                                        :: Env -> Equations -> Name -> Type -> Type ->TypeM Equations
sub' env eq w (TVar _ tv1) (TVar _ tv2)
  | tv1 == tv2                              = return eq

sub' env eq w t1@(TVar _ tv) t2
  | not $ scoped tv env                     = do defer [Sub w t1 t2]; return eq
  | Just tc <- findTVBound env tv           = return (idwit w t1 t2 : eq)

sub' env eq w t1 t2@(TVar _ tv)
  | not $ scoped tv env                     = do defer [Sub w t1 t2]; return eq

sub' env eq w t1@(TExist _ p1) t2@(TExist l p2)
  | Just (wf,p') <- search                  = do unifyM env (tcargs p1) (tcargs p')
                                                 let e = eLambda [(x0,t1)] (eCall (eQVar primPACK) [wf $ eDot (eVar x0) protoKW, eDot (eVar x0) implKW])
                                                 return ((w, wFun t1 t2, e):eq)
  where search                              = findAncestor env p1 (tcname p2)

--                as declared               as called
--                existing                  expected
sub' env eq w t1@(TFun _ fx1 p1 k1 t1') t2@(TFun _ fx2 p2 k2 t2')                   -- TODO: implement pos/kwd argument shifting
                                            = do wp <- newWitness
                                                 wk <- newWitness
                                                 wt <- newWitness
                                                 let e = eLambda [(x0,t1)] e'
                                                     e' = Lambda l0 (PosSTAR x1 $ Just $ tTuple p2) (KwdSTAR x2 $ Just $ tRecord k2) e0 fxPure
                                                     e0 = eCall (eVar wt) [Call l0 (eVar x0) (PosStar e1) (KwdStar e2)]
                                                     e1 = Call l0 (eVar wp) (PosStar $ eVar x1) KwdNil
                                                     e2 = Call l0 (eVar wk) PosNil (KwdStar $ eVar x2)
                                                     cs = [Cast fx1 fx2, Sub wp p2 p1, Sub wk k2 k1, Sub wt t1' t2']
                                                 reduce env ((w, wFun t1 t2, e):eq) cs

--                existing            expected
sub' env eq w t1@(TTuple _ p1 k1) t2@(TTuple _ p2 k2)                               -- TODO: implement pos/kwd argument shifting
                                            = do wp <- newWitness
                                                 wk <- newWitness
                                                 let e = eLambda [(x0,t1)] (Tuple l0 (PosStar e1) (KwdStar e2))
                                                     e1 = Call l0 (eVar wp) (PosStar $ eCall (eQVar primPosOf) [eVar x0]) KwdNil
                                                     e2 = Call l0 (eVar wk) PosNil (KwdStar $ eCall (eQVar primKwdOf) [eVar x0])
                                                     cs = [Sub wp p1 p2, Sub wk k1 k2]
                                                 reduce env ((w, wFun t1 t2, e):eq) cs

-- Note: a sub-row constraint R1 < R2 is witnessed by a lambda of type 
-- (*(R1))->(*(R2)) or (**(R1))->(**(R2)), depending on the row kind

sub' env eq w r1@(TNil _ k1) r2@(TNil _ k2)
  | k1 == k2                                = return ((w, rowFun k1 r1 r2, eLambda [] (eTuple [])) : eq)
sub' env eq w r1@(TRow _ k n t1 r1') r2     = do (t2,r2') <- findElem k (tNil k) n r2 (rowTail r1)
                                                 wt <- newWitness
                                                 wr <- newWitness
                                                 let e = rowWit k w n t1 r1' wt wr
                                                     cs = [Sub wt t1 t2, Sub wr r1' r2']
                                                 reduce env ((w, rowFun k r1 r2, e):eq) cs

sub' env eq w t1 t2                         = do cast env t1 t2
                                                 return (idwit w t1 t2 : eq)



{-

round : (Real, *int) -> Real
----
w(round)(3.14, None)                                                    w(round)(3.14, None)
w : ((Real,?int)->Real) -> (float,None)->$1
----
w = lambda x0: lambda *x1,**x2: wt(x0(*wp(*x1), **wk(**x2)))            = (lambda x0: lambda *x1,**x2: wt(x0(*wp(*x1), **wk(**x2))))(round)(3.14, None)     | x0=round
wt : (Real) -> $1                                                       = lambda *x1,**x2: wt(round(*wp(*x1), **wk(**x2)))(3.14, None)                      | x1=(3.14,None), x2=()
wp : (float,None) -> (Real,?int)                                        = wt(round(*wp(3.14,None), **wk()))
wk : () -> ()
----
wt = lambda x0: x0                                                      = (lambda x0: x0)(round(*wp((3.14,None)), **wk()))                                  | x0=round(...)
wp = lambda x1,*x2: (w1(x1), *w2(*x2))                                  = round(*(lambda x1,*x2: (w1(x1), *w2(*x2)))((3.14,None)), **(lambda: ())()))       | x1=3.14, x2=(None,)
wk = lambda: ()                                                         = round(*(w1(3.14), *w2(*(None,))), **())
w1 : (float) -> Real                                                    = round(*(w1(3.14), *w2(None)))
w2 : (None) -> (?int,)
----
w1 = lambda x0: PACK(Real$float, x0)                                    = round(*((lambda x0: PACK(Real$float, x0))(3.14), *w2(None)))                      | x0=3.14
w2 = lambda x1,*x2: (w21(x1), *w22(*x2))                                = round(*(PACK(Real$float, 3.14), *(lambda x1,*x2: (w21(x1), *w22(*x2)))(None)))    | x1=None, x2=()
w21 : (None) -> ?int                                                    = round(*(PACK(Real$float, 3.14), *(w21(None), *w22())))
w22 : () -> ()
----
w21 = lambda x0: x0                                                     = round(*(PACK(Real$float, 3.14), *((lambda x0: x0)(None), *w22())))                | x0=None
w22 = lambda: ()                                                        = round(*(PACK(Real$float, 3.14), *(None, *(lambda: ())())))
                                                                        = round(*(PACK(Real$float, 3.14), *(None, *())))
                                                                        = round(*(PACK(Real$float, 3.14), *(None,)))
                                                                        = round(*(PACK(Real$float, 3.14), None))
                                                                        = round(PACK(Real$float, 3.14), None)
-}


----------------------------------------------------------------------------------------------------------------------
-- findElem
----------------------------------------------------------------------------------------------------------------------

findElem k r0 n r tl                        = do r0' <- msubst r0
                                                 r' <- msubst r
                                                 tl' <- msubst tl
                                                 findElem' r0' n r' tl'
  where findElem' r0 n (TRow l k n1 t r2) tl
          | n == n1                         = return (t, revApp r0 r2)
          | otherwise                       = findElem' (TRow l k n1 t r0) n r2 tl
        findElem' r0 n (TNil _ _) tl        = kwdNotFound n
        findElem' r0 n r2@(TVar _ tv) tl
          | r2 == tl                        = conflictingRow tv
          | otherwise                       = do t <- newTVar
                                                 r <- newTVarOfKind k
                                                 substitute tv (tRow k n t r)
                                                 return (t, revApp r0 r)
        revApp (TRow l k n t r1) r2         = revApp r1 (TRow l k n t r2)
        revApp (TNil _ _) r2                = r2


----------------------------------------------------------------------------------------------------------------------
-- Misc.
----------------------------------------------------------------------------------------------------------------------

impl2type t (TC n ts)                   = tCon $ TC n (t:ts)

x0:x1:x2:_                              = xNames

pPar p                                  = f pNames p
  where f ns (TRow _ PRow n t p)
          | n == name "_"               = PosPar (head ns) (Just t) Nothing (f (tail ns) p)
          | otherwise                   = PosPar n (Just t) Nothing (f ns p)
        f ns (TNil _ PRow)              = PosNIL
        f ns t                          = PosSTAR (head ns) (Just t)

kPar k                                  = f kNames k
  where f ns (TRow _ KRow n t p)
          | n == name "_"               = KwdPar (head ns) (Just t) Nothing (f (tail ns) p)
          | otherwise                   = KwdPar n (Just t) Nothing (f ns p)
        f ns (TNil _ KRow)              = KwdNIL
        f ns t                          = KwdSTAR (head ns) (Just t)

wit2arg ws                              = \p -> foldr f p ws
  where f (w,t)                         = PosArg (eVar w)

wit2par ws                              = \p -> foldr f p ws
  where f (w,t)                         = PosPar w (Just t) Nothing

var2arg xs                              = \p -> foldr f p xs
  where f x                             = PosArg (eVar x)

exp2arg es                              = \p -> foldr PosArg p es

witsOf cs                               = [ eVar w | Impl w t p <- cs ]

qualPar env q                           = wit2par (qualWits env q)

qualWits env q                          = [ (tvarWit tv p, impl2type (tVar tv) p) | TBind tv ps <- q, p <- ps, isProto (tcname p) env ]

app tx e []                             = e
app tx e es                             = Lambda NoLoc p' k' (Call NoLoc e (exp2arg es (pArg p')) (kArg k')) fx
  where TFun _ fx p k _                 = tx                    -- If it takes arguments, it must be a function!
        (p',k')                         = (pPar p, kPar k)

app2nd Static tx e es                   = app tx e es
app2nd _ tx e []                        = e
app2nd _ tx e es                        = Lambda NoLoc p' k' (Call NoLoc e (PosArg pSelf (exp2arg es pArgs)) (kArg k')) fx
  where TFun _ fx p k _                 = tx                    -- If it takes arguments, it must be a function!
        (p',k')                         = (pPar p, kPar k)
        PosArg pSelf pArgs              = pArg p'                    

idwit w t1 t2                           = (w, wFun t1 t2, eLambda [(x0,t1)] (eVar x0))

rowFun PRow r1 r2                       = tFun fxPure r1 kwdNil (tTuple r2)
rowFun KRow r1 r2                       = tFun fxPure posNil r1 (tRecord r2)

rowWit PRow w n t r wt wr               = Lambda l0 (PosPar x1 (Just t) Nothing $ PosSTAR x2 (Just $ tTuple r)) KwdNIL eTup fxPure
  where eTup                            = Tuple l0 (PosArg e1 (PosStar (Call l0 (eVar wr) (PosStar $ eVar x2) KwdNil))) KwdNil
        e1                              = eCall (eVar wt) [eVar x1]
rowWit KRow w n t r wt wr               = Lambda l0 PosNIL (KwdPar x1 (Just t) Nothing $ KwdSTAR x2 (Just $ tTuple r)) eRec fxPure
  where eRec                            = Tuple l0 PosNil (KwdArg n e1 (KwdStar (Call l0 (eVar wr) PosNil (KwdStar $ eVar x2))))
        e1                              = eCall (eVar wt) [eVar x1]


wFun t1 t2                              = tFun fxPure (posRow t1 posNil) kwdNil t2