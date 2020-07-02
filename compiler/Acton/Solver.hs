{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Acton.Solver where

import Control.Monad
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Utils
import Acton.Syntax
import Acton.Printer
import Acton.Builtin
import Acton.Names
import Acton.Prim
import Acton.Env




-- Reduce conservatively and remove entailed constraints
simplify                                    :: (Subst a, Pretty a) => Env -> a -> Constraints -> TypeM (Constraints,Equations)
simplify env te cs                          = do cs <- msubst cs
                                                 te <- msubst te
                                                 traceM ("  -simplify: " ++ prstrs cs)
                                                 traceM ("  -for: " ++ prstr te)
                                                 simplify' env te [] cs

simplify'                                   :: (Subst a) => Env -> a -> Equations -> Constraints -> TypeM (Constraints,Equations)
simplify' env te eq []                      = return ([], eq)
simplify' env te eq cs                      = do eq1 <- reduce env eq cs
                                                 cs1 <- msubst =<< collectDeferred
                                                 env1 <- msubst env 
                                                 te1 <- msubst te
                                                 improve env1 te1 eq1 cs1


-- Reduce aggressively or fail:

solve                                       :: (Subst a, Pretty a) => Env -> a -> Constraints -> TypeM (Constraints,Equations)
solve env te cs                             = do cs <- msubst cs
                                                 traceM ("  =solve: " ++ prstrs cs)
                                                 solve' env te [] cs

solve'                                      :: (Subst a, Pretty a) => Env -> a -> Equations -> Constraints -> TypeM (Constraints,Equations)
solve' env te eq []                         = return ([], eq)
solve' env te eq cs                         = do (cs1,eq1) <- simplify' env te eq cs
                                                 approximate env te eq1 cs1

solveAll                                    :: (Subst a, Pretty a) => Env -> a -> Constraints -> TypeM Equations
solveAll env te cs                          = do (cs',eq') <- solve env te cs
                                                 loop eq' cs'
  where loop eq []                          = return eq
        loop eq cs                          = do (cs',eq') <- solve' env te eq cs
                                                 loop (eq'++eq) cs'


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
                                                 traceM ("   reduce " ++ prstr c')
                                                 eq1 <- reduce' env eq c'
                                                 cs' <- msubst cs
                                                 reduce env eq1 cs'

reduce'                                     :: Env -> Equations -> Constraint -> TypeM Equations
reduce' env eq (Cast t1 t2)                 = do cast' env t1 t2
                                                 return eq

reduce' env eq (Sub w t1 t2)                = sub' env eq w t1 t2

reduce' env eq c@(Impl w t@(TVar _ tv) p)
  | univar tv                               = do defer [c]; return eq
  | Just wit <- witSearch                   = do (cs,p',we) <- instWitness env [] wit
                                                 unifyM env (tcargs p) (tcargs p')
                                                 reduce env ((w, impl2type t p, we):eq) cs
  where witSearch                           = findWitness env (NoQ $ tvname tv) (implProto env p)
  
reduce' env eq c@(Impl w t@(TCon _ tc) p)
  | Just wit <- witSearch                   = do (cs,p',we) <- instWitness env (tcargs tc) wit
                                                 unifyM env (tcargs p) (tcargs p')
                                                 reduce env ((w, impl2type t p, we):eq) cs
  where witSearch                           = findWitness env (tcname tc) (implProto env p)

reduce' env eq c@(Sel w (TVar _ tv) n _)
  | univar tv                               = do defer [c]; return eq
  | Just wsc <- attrSearch                  = do (eq',cs) <- solveSelAttr env wsc c
                                                 reduce env (eq'++eq) cs
  | Just wit <- witSearch                   = do (eq',cs) <- solveSelWit env wit c
                                                 reduce env (eq'++eq) cs
  | otherwise                               = err1 n "Attribute not found"
  where attrSearch                          = findTVAttr env tv n
        witSearch                           = findWitness env (NoQ $ tvname tv) (hasAttr env n)

reduce' env eq c@(Sel w (TCon _ tc) n _)
  | Just wsc <- attrSearch                  = do (eq',cs) <- solveSelAttr env wsc c
                                                 reduce env (eq'++eq) cs
  | Just wit <- witSearch                   = do (eq',cs) <- solveSelWit env wit c
                                                 reduce env (eq'++eq) cs
  | otherwise                               = err1 n "Attribute not found"
  where attrSearch                          = findAttr env tc n
        witSearch                           = findWitness env (tcname tc) (hasAttr env n)

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

reduce' env eq c@(Mut (TVar _ tv) n _)
  | univar tv                               = do defer [c]; return eq
  | Just wsc <- attrSearch                  = do cs <- solveMutAttr env wsc c
                                                 reduce env eq cs
  | otherwise                               = err1 n "Attribute not found:"
  where attrSearch                          = findTVAttr env tv n

reduce' env eq c@(Mut (TCon _ tc) n _)
  | Just wsc <- attrSearch                  = do cs <- solveMutAttr env wsc c
                                                 reduce env eq cs
  | otherwise                               = err1 n "Attribute not found:"
  where attrSearch                          = findAttr env tc n

reduce' env eq c                            = noRed c


solveSelAttr env (wf,sc,_) (Sel w t1 n t2)  = do (cs,t) <- instantiate env sc
                                                 -- when (tvSelf `elem` contrafree t) (err1 n "Contravariant Self attribute not selectable by instance")
                                                 let e = eLambda [(x0,t1)] (app t (eDot (wf $ eVar x0) n) $ witsOf cs)
                                                     cs = [Cast (subst [(tvSelf,t1)] t) t2]
                                                 return ([(w, wFun t1 t2, e)], cs)

solveSelWit env wit (Sel w t1 n t2)         = do (cs1,p,we) <- instWitness env [] wit
                                                 let Just (wf,sc,dec) = findAttr env p n
                                                 (cs2,t) <- instantiate env sc
                                                 let e = eLambda [(x0,t1)] (app t (eDot (wf we) n) $ eVar x0 : witsOf cs2) -- witnesses *after* object ref
                                                     cs = Cast (subst [(tvSelf,t1)] t) t2 : cs1 ++ cs2
                                                 return ([(w, wFun t1 t2, e)], cs)

solveMutAttr env (wf,sc,dec) (Mut t1 n t2)  = do when (dec/=Property) (noMut n)
                                                 (cs,t) <- instantiate env sc
                                                 let cs' = Cast t1 tObject : Cast t2 (subst [(tvSelf,t1)] t) : cs
                                                 return cs'

----------------------------------------------------------------------------------------------------------------------
-- cast
----------------------------------------------------------------------------------------------------------------------

cast                                        :: Env -> Type -> Type -> TypeM ()
cast env t1 t2                              = do t1' <- msubst t1
                                                 t2' <- msubst t2
                                                 cast' env t1' t2'

castM env ts1 ts2                           = mapM_ (uncurry $ cast env) (ts1 `zip` ts2)


cast' env (TWild _) t2                      = return ()
cast' env t1 (TWild _)                      = return ()

cast' env (TCon _ c1) (TCon _ c2)
  | Just (wf,c') <- search                  = unifyM env (tcargs c') (tcargs c2)        -- TODO: cast/unify based on polarities
  where search                              = findAncestor env c1 (tcname c2)

cast' env (TFun _ (TFX _ FXAsync) p1 k1 t1) (TFun _ (TFX _ (FXAct _)) p2 k2 t2)
                                            = do cast env p2 p1
                                                 cast env k2 k1
                                                 cast env (tMsg t1) t2

cast' env (TFun _ fx1 p1 k1 t1) (TFun _ fx2 p2 k2 t2)
                                            = do cast env fx1 fx2
                                                 cast env p2 p1
                                                 cast env k2 k1
                                                 cast env t1 t2

cast' env (TTuple _ p1 k1) (TTuple _ p2 k2)
                                            = do cast env p1 p2
                                                 cast env k1 k2

cast' env (TUnion _ us1) (TUnion _ us2)
  | all (uniElem us2) us1                   = return ()
cast' env (TUnion _ us1) t2
  | all uniLit us1                          = unify env tStr t2
cast' env (TCon _ c1) (TUnion _ us2)
  | uniConElem us2 (unalias env c1)         = return ()

cast' env (TOpt _ t1) (TOpt _ t2)           = cast env t1 t2
cast' env (TNone _) (TOpt _ t)              = return ()
cast' env (TNone _) (TNone _)               = return ()

cast' env (TFX _ fx1) (TFX _ fx2)
  | Just unifs <- castFX fx1 fx2            = unifs
  where castFX FXPure FXPure                = Just $ return ()
        castFX FXPure (FXMut _)             = Just $ return ()
        castFX FXPure (FXAct _)             = Just $ return ()
        castFX (FXMut t1) (FXMut t2)        = Just $ unify env t1 t2
        castFX (FXMut t1) (FXAct t2)        = Just $ unify env t1 t2
        castFX (FXAct t1) (FXAct t2)        = Just $ unify env t1 t2
        castFX FXAsync FXAsync              = Just $ return ()
        castFX fx1 fx2                      = Nothing

cast' env (TNil _ k1) (TNil _ k2)
  | k1 == k2                                = return ()
cast' env r1 (TRow _ k n t2 r2)             = do (t1,r1') <- findElem k (tNil k) n r1 (rowTail r2)
                                                 cast env t1 t2
                                                 cast env r1' r2
cast' env (TRow _ k n t1 r1) r2             = do (t2,r2') <- findElem k (tNil k) n r2 (rowTail r1)
                                                 cast env t1 t2
                                                 cast env r1 r2'

cast' env (TVar _ tv) t2@TFun{}
  | univar tv                               = do t1 <- instwild KType $ tFun tWild tWild tWild tWild
                                                 substitute tv t1
                                                 cast env t1 t2
cast' env t1@TFun{} (TVar _ tv)
  | univar tv                               = do t2 <- instwild KType $ tFun tWild tWild tWild tWild
                                                 substitute tv t2
                                                 cast env t1 t2

cast' env (TVar _ tv) t2@TTuple{}
  | univar tv                               = do t1 <- instwild KType $ tTuple tWild tWild
                                                 substitute tv t1
                                                 cast env t1 t2
cast' env t1@TTuple{} (TVar _ tv)
  | univar tv                               = do t2 <- instwild KType $ tTuple tWild tWild
                                                 substitute tv t2
                                                 cast env t1 t2

cast' env (TVar _ tv1) (TVar _ tv2)
  | tv1 == tv2                              = return ()

cast' env t1@(TVar _ tv) t2
  | univar tv                               = defer [Cast t1 t2]
  | Just tc <- findTVBound env tv           = cast' env (tCon tc) t2

cast' env t1 t2@(TVar _ tv)
  | univar tv                               = defer [Cast t1 t2]
  | otherwise                               = noRed (Cast t1 t2)

cast' env t1 (TOpt _ t2)                    = cast env t1 t2                -- Only matches when t1 is NOT a variable

cast' env t1 t2                             = noRed (Cast t1 t2)


----------------------------------------------------------------------------------------------------------------------
-- castP (cast predicate)
----------------------------------------------------------------------------------------------------------------------

castP                                       :: Env -> Type -> Type -> Bool
castP env (TWild _) t2                      = True
castP env t1 (TWild _)                      = True

castP env (TCon _ c1) (TCon _ c2)
  | Just (wf,c') <- search                  = tcargs c1 == tcargs c'
  where search                              = findAncestor env c1 (tcname c2)

castP env (TFun _ (TFX _ FXAsync) p1 k1 t1) (TFun _ (TFX _ (FXAct _)) p2 k2 t2)
                                            = castP env p2 p1 && castP env k2 k1 && castP env (tMsg t1) t2

castP env (TFun _ fx1 p1 k1 t1) (TFun _ fx2 p2 k2 t2)
                                            = castP env fx1 fx2 && castP env p2 p1 && castP env k2 k1 && castP env t1 t2

castP env (TTuple _ p1 k1) (TTuple _ p2 k2) = castP env p1 p2 && castP env k1 k2

castP env (TUnion _ us1) (TUnion _ us2)
  | all (uniElem us2) us1                   = True
castP env (TUnion _ us1) t2
  | all uniLit us1                          = t2 == tStr
castP env (TCon _ c1) (TUnion _ us2)
  | uniConElem us2 (unalias env c1)         = True

castP env (TOpt _ t1) (TOpt _ t2)           = castP env t1 t2
castP env (TNone _) (TOpt _ t)              = True
castP env (TNone _) (TNone _)               = True

castP env (TFX _ fx1) (TFX _ fx2)           = castP' fx1 fx2
  where castP' FXPure FXPure                = True
        castP' FXPure (FXMut _)             = True
        castP' FXPure (FXAct _)             = True
        castP' (FXMut t1) (FXMut t2)        = t1 == t2
        castP' (FXMut t1) (FXAct t2)        = t1 == t2
        castP' (FXAct t1) (FXAct t2)        = t1 == t2
        castP' FXAsync FXAsync              = True
        castP' fx1 fx2                      = False

castP env (TNil _ k1) (TNil _ k2)
  | k1 == k2                                = True
castP env (TRow _ k n t1 r1) r2
  | Just (t2,r2') <- lookupElem n r2        = t2 /= tWild && castP env t1 t2 && r2' /= tWild && castP env r1 r2'

castP env (TVar _ tv1) (TVar _ tv2)
  | tv1 == tv2                              = True

castP env t1@(TVar _ tv) t2
  | univar tv                               = False
  | Just tc <- findTVBound env tv           = castP env (tCon tc) t2

castP env t1 t2@(TVar _ tv)                 = False

castP env t1 (TOpt _ t2)                    = castP env t1 t2

castP env t1 t2                             = False



----------------------------------------------------------------------------------------------------------------------
-- unify
----------------------------------------------------------------------------------------------------------------------

unify                                       :: Env -> Type -> Type -> TypeM ()
unify env t1 t2                             = do t1' <- msubst t1
                                                 t2' <- msubst t2
                                                 --traceM ("  #unify " ++ prstr t1' ++ " and " ++ prstr t2')
                                                 unify' env t1' t2'

unifyM env ts1 ts2                          = mapM_ (uncurry $ unify env) (ts1 `zip` ts2)


unify' env (TWild _) t2                     = return ()
unify' env t1 (TWild _)                     = return ()

unify' env (TCon _ c1) (TCon _ c2)
  | qmatch env (tcname c1) (tcname c2)      = unifyM env (tcargs c1) (tcargs c2)

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

unify' env (TFX _ fx1) (TFX _ fx2)
  | Just unifs <- unifyFX fx1 fx2           = unifs
  where unifyFX FXPure FXPure               = Just $ return ()
        unifyFX (FXMut t1) (FXMut t2)       = Just $ unify env t1 t2
        unifyFX (FXAct t1) (FXAct t2)       = Just $ unify env t1 t2
        unifyFX FXAsync FXAsync             = Just $ return ()
        unifyFX fx1 fx2                     = Nothing

unify' env (TNil _ k1) (TNil _ k2)
  | k1 == k2                                = return ()
unify' env r1 (TRow _ k n t2 r2)            = do (t1,r1') <- findElem k (tNil k) n r1 (rowTail r2)
                                                 unify env t1 t2
                                                 unify env r1' r2

unify' env (TVar _ tv1) (TVar _ tv2)
  | tv1 == tv2                              = return ()

unify' env (TVar _ tv) t2
  | univar tv                               = do when (tv `elem` tyfree t2) (infiniteType tv)
                                                 substitute tv t2
unify' env t1 (TVar _ tv)
  | univar tv                               = do when (tv `elem` tyfree t1) (infiniteType tv)
                                                 substitute tv t1

unify' env t1 t2                            = noUnify t1 t2


----------------------------------------------------------------------------------------------------------------------
-- sub
----------------------------------------------------------------------------------------------------------------------

sub                                         :: Env -> Equations -> Name -> Type -> Type ->TypeM Equations
sub env eq w t1 t2                          = do t1' <- msubst t1
                                                 t2' <- msubst t2
                                                 sub' env eq w t1' t2'

sub'                                        :: Env -> Equations -> Name -> Type -> Type ->TypeM Equations

sub' env eq w t1@(TWild _) t2               = return (idwit w t1 t2 : eq)
sub' env eq w t1 t2@(TWild _)               = return (idwit w t1 t2 : eq)

--                as declared               as called
--                existing                  expected
sub' env eq w t1@(TFun _ fx1 p1 k1 t1') t2@(TFun _ fx2 p2 k2 t2')                   -- TODO: implement pos/kwd argument shifting
                                            = do wp <- newWitness
                                                 wk <- newWitness
                                                 wt <- newWitness
                                                 let e = eLambda [(x0,t1)] e'
                                                     e' = Lambda l0 (PosSTAR x1 $ Just $ tTupleP p2) (KwdSTAR x2 $ Just $ tTupleK k2) e0 fxPure
                                                     e0 = asynwrap fx1 fx2 $ eCall (eVar wt) [Call l0 (eVar x0) (PosStar e1) (KwdStar e2)]
                                                     e1 = Call l0 (eVar wp) (PosStar $ eVar x1) KwdNil
                                                     e2 = Call l0 (eVar wk) PosNil (KwdStar $ eVar x2)
                                                     cs = [asyncast fx1 fx2, Sub wp p2 p1, Sub wk k2 k1, asynsub fx1 fx2 wt t1' t2']
                                                 reduce env ((w, wFun t1 t2, e):eq) cs


--                existing            expected
sub' env eq w t1@(TTuple _ p1 k1) t2@(TTuple _ p2 k2)                               -- TODO: implement pos/kwd argument shifting
                                            = do wp <- newWitness
                                                 wk <- newWitness
                                                 let e = eLambda [(x0,t1)] (Paren l0 $ Tuple l0 (PosStar e1) (KwdStar e2))
                                                     e1 = Call l0 (eVar wp) (PosStar $ eCall (eQVar primPosOf) [eVar x0]) KwdNil
                                                     e2 = Call l0 (eVar wk) PosNil (KwdStar $ eCall (eQVar primKwdOf) [eVar x0])
                                                     cs = [Sub wp p1 p2, Sub wk k1 k2]
                                                 reduce env ((w, wFun t1 t2, e):eq) cs

-- Note: a sub-row constraint R1 < R2 is witnessed by a lambda of type
-- (*(R1))->(*(R2)) or (**(R1))->(**(R2)), depending on the row kind

sub' env eq w r1@(TNil _ k1) r2@(TNil _ k2)
  | k1 == k2                                = return ((w, rowFun k1 r1 r2, eLambda [] (Paren l0 $ eTuple [])) : eq)

--          existing     expected                Match labels in the order of the expected row
sub' env eq w r1     r2@(TRow _ k n t2 r2') = do (t1,r1') <- findElem k (tNil k) n r1 (rowTail r2)
                                                 wt <- newWitness
                                                 wr <- newWitness
                                                 let e = rowWit k w n t1 r1' wt wr
                                                     cs = [Sub wt t1 t2, Sub wr r1' r2']
                                                 reduce env ((w, rowFun k r1 r2, e):eq) cs
sub' env eq w r1@(TRow _ k n t1 r1') r2     = do (t2,r2') <- findElem k (tNil k) n r2 (rowTail r1)
                                                 wt <- newWitness
                                                 wr <- newWitness
                                                 let e = rowWit k w n t2 r2' wt wr
                                                     cs = [Sub wt t1 t2, Sub wr r1' r2']
                                                 reduce env ((w, rowFun k r1 r2, e):eq) cs

sub' env eq w (TVar _ tv) t2@TFun{}
  | univar tv                               = do t1 <- instwild KType $ tFun tWild tWild tWild tWild
                                                 substitute tv t1
                                                 sub env eq w t1 t2
sub' env eq w t1@TFun{} (TVar _ tv)
  | univar tv                               = do t2 <- instwild KType $ tFun tWild tWild tWild tWild
                                                 substitute tv t2
                                                 sub env eq w t1 t2

sub' env eq w (TVar _ tv) t2@TTuple{}
  | univar tv                               = do t1 <- instwild KType $ tTuple tWild tWild
                                                 substitute tv t1
                                                 sub env eq w t1 t2
sub' env eq w t1@TTuple{} (TVar _ tv)
  | univar tv                               = do t2 <- instwild KType $ tTuple tWild tWild
                                                 substitute tv t2
                                                 sub env eq w t1 t2

sub' env eq w t1@(TVar _ tv1) t2@(TVar _ tv2)
  | tv1 == tv2                              = return (idwit w t1 t2 : eq)

sub' env eq w t1@(TVar _ tv) t2
  | univar tv                               = do defer [Sub w t1 t2]; return eq

sub' env eq w t1 t2@(TVar _ tv)
  | univar tv                               = do defer [Sub w t1 t2]; return eq

sub' env eq w t1 t2                         = do cast env t1 t2
                                                 return (idwit w t1 t2 : eq)

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
          | not $ univar tv                 = kwdNotFound n
          | otherwise                       = do t <- newTVar
                                                 r <- newTVarOfKind k
                                                 substitute tv (tRow k n t r)
                                                 return (t, revApp r0 r)
        findElem' e0 n r2 tl                = noUnify r2 (tRow k n tWild tWild)
        revApp (TRow l k n t r1) r2         = revApp r1 (TRow l k n t r2)
        revApp (TNil _ _) r2                = r2


{-

round : (Real, ?int) -> Real
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
w1 : (float) -> Real                                                    = round(*(w1(3.14), *w2(None,)))
w2 : (None,) -> (?int,)
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


------------------------------------------

round : (x:Real, n:?int) -> Real                                        round(n=None,x=3.14)
----
w(round)(n=None, x=3.14)                                                w(round)(n=None,x=3.14)
w : ((x:Real,n:?int)->Real) -> (n:None,x:float)->$1
----
w = lambda x0: lambda **x1: wt(x0(**wr(**x1)))                          = (lambda x0: lambda **x1: wt(x0(**wr(**x1))))(round)(n=None,x=3.14)            | x0=round
wt : (Real) -> $1                                                       = (lambda **x1: wt(round(**wr(**x1))))(n=None,x=3.14)                           | x1=(n=None,x=3.14))
wr : (n:None,x:float) -> (x:Real,n:?int)                                = wt(round(**wr(n=None,x=3.14)))
----
wt = lambda x0: x0                                                      = (lambda x0: x0 )(round(**wr(n=None,x=3.14)))                                  | x0=round(...)
wr = lambda x,**x2: (x=w1(x), **w2(**x2))                               = round(**(lambda x,**x2: (x=w1(x), **w2(**x2)))(n=None,x=3.14))                | x=3.14, x2=(n=None,)
w1 : (float) -> Real                                                    = round(**(x=w1(3.14), **w2(n=None,)))
w2 : (n:None,) -> (n:?int,)
----
w1 = lambda x0: PACK(Real$float, x0)                                    = round(**(x=(lambda x0: PACK(Real$float, x0))(3.14), **w2(n=None,)))             | x0=3.14
w2 = lambda n, **x2: (n=w21(n), **w22(**x2))                            = round(**(x=PACK(Real$float, 3.14), **(lambda n, **x2: (n=w21(n), **w22(**x2)))(n=None,)))
w21 : (None) -> ?int                                                    = round(**(x=PACK(Real$float, 3.14), **(n=w21(None), **w22(**()))))
w22 : () -> ()
----
w21 = lambda x0: x0                                                     = round(**(x=PACK(Real$float, 3.14), **(n=None, **())))
w22 = lambda: ()                                                        = round(**(x=PACK(Real$float, 3.14), **(n=None)))
                                                                        = round(**(x=PACK(Real$float, 3.14), n=None))
                                                                        = round(x=PACK(Real$float, 3.14), n=None)
-}

----------------------------------------------------------------------------------------------------------------------
-- Variable info
----------------------------------------------------------------------------------------------------------------------

data VInfo                                  = VInfo {
                                                varvars     :: [(TVar,TVar)],
                                                embedded    :: [TVar],
                                                ubounds     :: Map TVar [Type], 
                                                lbounds     :: Map TVar [Type], 
                                                pbounds     :: Map TVar [(Name,TCon)],
                                                mutattrs    :: Map TVar [Name],
                                                selattrs    :: Map TVar [Name] }

varvar v1 v2 vi                             = vi{ varvars = (v1,v2) : varvars vi }
embed vs vi                                 = vi{ embedded = vs ++ embedded vi }
ubound v t vi                               = vi{ ubounds = Map.insertWith (++) v [t] (ubounds vi) }
lbound v t vi                               = vi{ lbounds = Map.insertWith (++) v [t] (lbounds vi) }
pbound v w p vi                             = vi{ pbounds = Map.insertWith (++) v [(w,p)] (pbounds vi) }
mutattr v n vi                              = vi{ mutattrs = Map.insertWith (++) v [n] (mutattrs vi) }
selattr v n vi                              = vi{ selattrs = Map.insertWith (++) v [n] (selattrs vi) }

lookup' v m                                 = maybe [] id $ Map.lookup v m

varinfo cs                                  = f cs (VInfo [] [] Map.empty Map.empty Map.empty Map.empty Map.empty)
  where
    f (Cast (TVar _ v1) (TVar _ v2) : cs)
      | v1 == v2                            = f cs
      | otherwise                           = f cs . varvar v1 v2
    f (Cast (TVar _ v) t : cs)              = f cs . ubound v t . embed (tyfree t)
    f (Cast t (TVar _ v) : cs)              = f cs . lbound v t . embed (tyfree t)
    f (Sub _ (TVar _ v1) (TVar _ v2) : cs)  = f cs . varvar v1 v2
    f (Sub _ (TVar _ v) t : cs)             = f cs . ubound v t . embed (tyfree t)
    f (Sub _ t (TVar _ v) : cs)             = f cs . lbound v t . embed (tyfree t)
    f (Impl w (TVar _ v) p : cs)            = f cs . pbound v w p
    f (Mut (TVar _ v) n _ : cs)             = f cs . mutattr v n
    f (Sel _ (TVar _ v) n _ : cs)           = f cs . selattr v n
    f []                                    = Just
    f (_ : cs)                              = \_ -> Nothing

varclose xys                                = clos [] xys
  where clos cl []                          = Right cl
        clos cl ((x,y):xys)
          | (x,y) `elem` cl                 = clos cl xys
          | not $ null common               = Left (x,y:common)
          | otherwise                       = clos ((x,y):cl)  (new_below++new_above++xys)
          where below_x                     = below x cl
                above_y                     = above y cl
                common                      = below_x `intersect` above_y
                new_below                   = [ (w,y) | w <- below_x ]
                new_above                   = [ (x,v) | v <- above_y ]

below x cl                                  = [ v | (v,w) <- cl, w==x ]
above y cl                                  = [ w | (v,w) <- cl, v==y ]
            
gsimp cl obs xys                        = [ (x,y) | (x,y) <- xys,
                                                    x `notElem` obs || y `notElem` obs,
                                                    (above x cl \\ [y]) `eq` above y cl, 
                                                    (below y cl \\ [x]) `eq` below x cl ]
  where a `eq` b                        = all (`elem` b) a && all (`elem` a) b


instwild k (TWild _)                    = newTVarOfKind k
instwild _ (TFun l e p k t)             = TFun l <$> instwild KFX e <*> instwild PRow p <*> instwild KRow k <*> instwild KType t
instwild _ (TTuple l p k)               = TTuple l <$> instwild PRow p <*> instwild KRow k
instwild _ (TOpt l t)                   = TOpt l <$> instwild KType t
instwild _ (TCon l c)                   = TCon l <$> instwildcon c
instwild _ (TRow l k n t r)             = TRow l k n <$> instwild KType t <*> instwild k r
instwild _ (TFX l (FXMut t))            = TFX l <$> FXMut <$> instwild KFX t
instwild _ (TFX l (FXAct t))            = TFX l <$> FXAct <$> instwild KFX t
instwild k t                            = return t

instwildcon c                           = TC (tcname c) <$> mapM (instwild KType) (tcargs c)

----------------------------------------------------------------------------------------------------------------------
-- GLB
----------------------------------------------------------------------------------------------------------------------

mkGLB                                   :: Env -> (TVar,[Type]) -> TypeM (TVar,Type)
mkGLB env (v,ts)                        = do t <- instwild KType $ foldr1 (glb env) ts
                                             traceM ("   glb " ++ prstrs ts ++ " = " ++ prstr t)
                                             return (v, t)

glb env (TWild _) t2                    = t2
glb env t1 (TWild _)                    = t1

glb env TVar{} _                        = tWild        -- (Might occur in recursive calls)
glb env _ TVar{}                        = tWild        -- (Might occur in recursive calls)

glb env (TCon _ c1) (TCon _ c2)
  | qmatch env (tcname c1) (tcname c2)  = tCon c1
  | hasAncestor env c1 (tcname c2)      = tCon c1
  | hasAncestor env c2 (tcname c1)      = tCon c2

glb env (TFun _ e1 p1 k1 t1) (TFun _ e2 p2 k2 t2)                                           -- tWilds instead of glbs enable the special
                                        = tFun tWild (lub env p1 p2) (lub env k1 k2) tWild  -- async rules in sub and cast
glb env (TTuple _ p1 k1) (TTuple _ p2 k2)
                                        = tTuple (glb env p1 p2) (glb env k1 k2)

glb env (TUnion _ us1) (TUnion _ us2)
  | [UCon qn] <- us                     = tCon (TC qn [])
  | not $ null us                       = tUnion us
  where us                              = us1 `intersect` us2
glb env t1@(TUnion _ us) t2@(TCon _ c)
  | uniConElem us (unalias env c)       = t2
  | all uniLit us && eqStr env t2       = t1
glb env t1@(TCon _ c) t2@(TUnion _ us)
  | uniConElem us (unalias env c)       = t1
  | all uniLit us && eqStr env t1       = t2
  
glb env (TOpt _ t1) (TOpt _ t2)         = tOpt (glb env t1 t2)
glb env (TNone _) t2                    = tNone
glb env t1 (TNone _)                    = tNone
glb env (TOpt _ t1) t2                  = glb env t1 t2
glb env t1 (TOpt _ t2)                  = glb env t1 t2

glb env t1@(TFX _ fx1) t2@(TFX _ fx2)   = tTFX (glfx fx1 fx2)
  where glfx FXAsync FXAsync            = FXAsync
        glfx FXAsync (FXAct _)          = FXAsync
        glfx (FXAct _) FXAsync          = FXAsync
        glfx FXAsync fx2                = noGLB t1 t2
        glfx fx1 FXAsync                = noGLB t1 t2
        glfx FXPure _                   = FXPure
        glfx _ FXPure                   = FXPure
        glfx (FXMut t1) _               = FXMut t1
        glfx _ (FXMut t2)               = FXMut t2
        glfx (FXAct t1) (FXAct t2)      = FXAct t1

glb env (TNil _ k1) (TNil _ k2)
  | k1 == k2                            = tNil k1
glb env (TRow _ k n t1 r1) r
  | Just (t2,r2) <- lookupElem n r      = tRow k n (glb env t1 t2) (glb env r1 r2)

glb env t1 t2                           = noGLB t1 t2
    
noGLB t1 t2                             = err1 t1 ("No common subtype: " ++ prstr t2)

eqStr env (TCon _ c)                    = qmatch env (tcname c) qnStr

lookupElem n (TRow l k n' t r)
  | n == n'                             = Just (t,r)
  | otherwise                           = case lookupElem n r of
                                            Nothing -> Nothing
                                            Just (t',r') -> Just (t, TRow l k n' t r')
lookupElem n (TVar _ _)                 = Just (tWild,tWild)
lookupElem n (TNil _ _)                 = Nothing
    


----------------------------------------------------------------------------------------------------------------------
-- LUB
----------------------------------------------------------------------------------------------------------------------

mkLUB env (v,ts)                        = do t <- instwild KType $ foldr1 (lub env) ts
                                             traceM ("   lub " ++ prstrs ts ++ " = " ++ prstr t)
                                             return (v, t)

lub env (TWild _) t2                    = t2
lub env t1 (TWild _)                    = t1

lub env TVar{} _                        = tWild        -- (Might occur in recursive calls)
lub env _ TVar{}                        = tWild        -- (Might occur in recursive calls)

lub env (TCon _ c1) (TCon _ c2)
  | c1 == c2                            = tCon c1
  | uniCon (unalias env c1),
    uniCon (unalias env c2)             = tUnion [UCon (tcname c1), UCon (tcname c2)]
  | hasAncestor env c1 (tcname c2)      = tCon c2
  | hasAncestor env c2 (tcname c1)      = tCon c1
  | not $ null common                   = tCon $ head common
  where common                          = commonAncestors env c1 c2

lub env (TFun _ e1 p1 k1 t1) (TFun _ e2 p2 k2 t2)
                                        = tFun (lub env e1 e2) (glb env p1 p2) (glb env k1 k2) (lub env t1 t2)
lub env (TTuple _ p1 k1) (TTuple _ p2 k2)
                                        = tTuple (lub env p1 p2) (lub env k1 k2)

lub env (TUnion _ us1) (TUnion _ us2)   = tUnion $ us1 `union` us2
lub env t1@(TUnion _ us) t2@(TCon _ c)
  | all uniLit us && t2 == tStr         = t2
  | uniCon (unalias env c)              = tUnion $ us `union` [UCon (tcname c)]
lub env t1@(TCon _ c) t2@(TUnion _ us)
  | all uniLit us && t1 == tStr         = t1
  | uniConElem us (unalias env c)       = tUnion $ us `union` [UCon (tcname c)]
  
lub env (TOpt _ t1) (TOpt _ t2)         = tOpt (lub env t1 t2)
lub env (TNone _) t2                    = tOpt t2
lub env t1 (TNone _)                    = tOpt t1
lub env (TOpt _ t1) t2                  = tOpt $ lub env t1 t2
lub env t1 (TOpt _ t2)                  = tOpt $ lub env t1 t2

lub env t1@(TFX _ fx1) t2@(TFX _ fx2)   = tTFX (lufx fx1 fx2)
  where lufx FXAsync FXAsync            = FXAsync
        lufx FXAsync _                  = FXAct tWild
        lufx _ FXAsync                  = FXAct tWild
        lufx (FXAct t1) _               = FXAct t1
        lufx _ (FXAct t2)               = FXAct t2
        lufx (FXMut t1) _               = FXMut t1
        lufx _ (FXMut t2)               = FXMut t2
        lufx FXPure FXPure              = FXPure

lub env (TNil _ k1) (TNil _ k2)
  | k1 == k2                            = tNil k1
lub env (TRow _ k n t1 r1) r
  | Just (t2,r2) <- lookupElem n r      = tRow k n (lub env t1 t2) (lub env r1 r2)

lub env t1 t2                           = noLUB t1 t2

noLUB t1 t2                             = err1 t1 ("No common supertype: " ++ prstr t2)

----------------------------------------------------------------------------------------------------------------------
-- Improvement
----------------------------------------------------------------------------------------------------------------------

-- Check if the deferred constraint set should be resubmitted
-- Unify all var cycles
-- Perform G-simplification on internal variables
-- Check that there's at least one non-embedded variable
-- For all non-embedded variables: replace mulitple lower/upper con bounds with a LUB/GLB
-- For non-embedded variables with a single lower/upper bound: replace with bound if not negative/positive
-- For non-embedded variables or chains with both a lower and an upper con bound: ensure the bounds are also related
-- For non-embedded variables with multiple protocol constraints: identify equal and subtype-related protocols

improve env te eq []                    = return ([], eq)
improve env te eq cs
  | Nothing <- info                     = do traceM ("  *Resubmit")
                                             simplify' env te eq cs
  | Left (v,vs) <- closure              = do traceM ("  *Unify cycle " ++ prstr v ++ " = " ++ prstrs vs)
                                             sequence [ unify env (tVar v) (tVar v') | v' <- vs ]
                                             simplify' env te eq cs
  | not $ null gsimple                  = do traceM ("  *G-simplify " ++ prstrs [ (v,tVar v') | (v,v') <- gsimple ])
                                             sequence [ unify env (tVar v) (tVar v') | (v,v') <- gsimple ]
                                             simplify' env te eq cs
  | not $ null cyclic                   = err2 cyclic "Cyclic constraint set: "
  | not $ null (multiUBnd++multiLBnd)   = do ub <- mapM (mkGLB env) multiUBnd
                                             lb <- mapM (mkLUB env) multiLBnd
                                             traceM ("  *GLB " ++ prstrs ub)
                                             traceM ("  *LUB " ++ prstrs lb)
                                             let cs' = [ Cast (tVar v) t | (v,t) <- ub ] ++ [ Cast t (tVar v) | (v,t) <- lb ]
                                             simplify' env te eq (cs' ++ map (replace ub lb) cs)
  | not $ null transCast                = do traceM ("  *Transitive cast")
                                             simplify' env te eq (transCast ++ cs)
  | not $ null posLBnd                  = do traceM ("  *S-simplify (dn) " ++ prstrs posLBnd)
                                             sequence [ unify env (tVar v) t | (v,t) <- posLBnd ]
                                             simplify' env te eq cs
  | not $ null negUBnd                  = do traceM ("  *S-simplify (up) " ++ prstrs negUBnd)
                                             sequence [ unify env (tVar v) t | (v,t) <- negUBnd ]
                                             simplify' env te eq cs
  | not $ null optBnd                   = do traceM ("  *Simplify upper opt bound " ++ prstrs optBnd)
                                             sequence [ unify env (tVar v) =<< (tOpt <$> newTVar) | v <- optBnd ]
                                             simplify' env te eq cs
  | not $ null actBnd                   = do traceM ("  *Simplify upper actor bound " ++ prstrs optBnd)
                                             sequence [ unify env (tVar v) t | (v,t) <- actBnd ]
                                             simplify' env te eq cs
  | not $ null redEq                    = do traceM ("  *(Context red) " ++ prstrs [ w | (w,_,_) <- redEq ])
                                             sequence [ unify env t1 t2 | (t1,t2) <- redUni ]
                                             simplify' env te (redEq++eq) (remove [ w | (w,_,_) <- redEq ] cs)
  | not $ null dots                     = do traceM ("  *Implied mutation/selection solutions " ++ prstrs dots)
                                             (eq',cs') <- solveDots env mutC selC selP cs
                                             simplify' env te (eq'++eq) cs'
  | otherwise                           = return (cs, eq)
  where info                            = varinfo cs
        Just vi                         = info
        closure                         = varclose (varvars vi)
        Right vclosed                   = closure
        gsimple                         = gsimp vclosed obsvars (varvars vi)
        cyclic                          = if null freevars then filter (not . null . (intersect $ embedded vi) . tyfree) cs else []
        multiUBnd                       = [ (v,ts) | (v,ts) <- Map.assocs (ubounds vi), v `notElem` embedded vi, length ts > 1 ]
        multiLBnd                       = [ (v,ts) | (v,ts) <- Map.assocs (lbounds vi), v `notElem` embedded vi, length ts > 1 ]
        multiPBnd                       = [ (v,ps) | (v,ps) <- Map.assocs (pbounds vi), length ps > 1 ]
        lowerBnd                        = [ (v,t) | (v,[t]) <- Map.assocs (lbounds vi), v `notElem` embedded vi ]
        upperBnd                        = [ (v,t) | (v,[t]) <- Map.assocs (ubounds vi), v `notElem` embedded vi ]
        transCast                       = [ Cast t t' | (v,t) <- lowerBnd, (v',t') <- upperBnd, v == v' || (v,v') `elem` vclosed, not $ castP env t t' ]
        posLBnd                         = [ (v,u) | (v,t) <- lowerBnd, u:us <- [supImplAll env (lookup' v $ pbounds vi) t], null us || v `notElem` negvars ]
        negUBnd                         = [ (v,t) | (v,t) <- upperBnd, v `notElem` (posvars ++ pvars) ]
        optBnd                          = [ v | (v, TOpt _ _) <- upperBnd ]
        actBnd                          = [ (v,t) | (v, t@(TCon _ c)) <- upperBnd, isActor (tcname c) env ]
        (redEq,redUni)                  = ctxtReduce env vi multiPBnd
        mutC                            = findBoundAttrs env (mutattrs vi) (ubounds vi)
        selC                            = findBoundAttrs env (selattrs vi) (ubounds vi)
        selP                            = findWitAttrs env (selattrs vi) (pbounds vi)
        dots                            = dom mutC ++ dom selC ++ dom selP
        fixedvars                       = tyfree env
        pvars                           = Map.keys (pbounds vi)
        posvars                         = tyfree te                         -- TODO: implement true polarity assignment
        negvars                         = posvars                           -- TODO: implement true polarity assignment
        obsvars                         = posvars ++ negvars ++ fixedvars
        freevars                        = (Map.keys (ubounds vi) ++ Map.keys (lbounds vi)) \\ embedded vi
        
findBoundAttrs env attrs bounds         = [ ((v,n),wsc) | (v,ns) <- Map.assocs attrs, n <- ns, wsc <- bounds' v n ]
  where bounds' v n                     = [ wsc | TCon _ c <- lookup' v bounds, Just wsc <- [findAttr env c n] ]

findWitAttrs env attrs bounds           = [ ((v,n), WInst p (NoQ w) ws) | (v,ns) <- Map.assocs attrs, n <- ns, (w,p,ws) <- bounds' v n ]
  where bounds' v n                     = [ (w,p,ws) | (w,p0) <- lookup' v bounds, (ws,p) <- findAncestry env p0, hasAttr env n (tcname p) ]


-- Approximating:
---------------

-- Unify all var-vars

-- For a variable v with a (single) lower bound t and protocol constraints pi:
    -- Let ts be [ t' | t' <- ancestry t, implAll t' pi ]
    -- If ts is null then fail else unify v with (head ts)

-- For a variable v with a (single) upper bound t, unify v with t

-- Solve all Mut constraints by searching for a unique object sub-class which contains all assigned attributes
-- Fail if none exists

-- Solve all Sel constraints by searching for a unique class or (single) protocol which contains all selected attributes
-- If none exists, solve by a kwd tuple

-- Solve ambiguous Impl constraints according to a predefined defaulting table
-- Fail if no table row matches

approximate                             :: (Subst a, Pretty a) => Env -> a -> Equations -> Constraints -> TypeM (Constraints,Equations)
approximate env te eq []                = return ([], eq)
approximate env te eq cs
  | not $ null vvs                      = trace ("  *varvar-approx " ++ prstrs vvs) $ 
                                          do sequence [ unify env (tVar v) (tVar v') | (v,v') <- vvs ]
                                             simplify' env te eq cs
  | not $ null lowerBnd                 = do traceM ("  *S-approx (dn) " ++ prstrs lowerBnd)
                                             sequence [ unify env (tVar v) t | (v,t) <- lowerBnd ]
                                             simplify' env te eq cs
  | not $ null upperBnd                 = do traceM ("  *S-approx (up) " ++ prstrs upperBnd)
                                             sequence [ unify env (tVar v) t | (v,t) <- upperBnd ]
                                             simplify' env te eq cs
  | not $ null mutC                     = trace ("  *Unique mutation target " ++ prstrs mutC) $ 
                                          do sequence [ unify env (tVar v) =<< instwild KType t | (v,t) <- mutC ]
                                             simplify' env te eq cs
  | not $ null selC                     = trace ("  *Unique selection class " ++ prstrs selC) $
                                          do sequence [ unify env (tVar v) =<< instwild KType t | (v,t) <- selC ]
                                             simplify' env te eq cs
  | not $ null selA                     = trace ("  *Unique selection actor " ++ prstrs selA) $
                                          do sequence [ unify env (tVar v) =<< instwild KType t | (v,t) <- selA ]
                                             simplify' env te eq cs
  | not $ null selP                     = trace ("  *Unique selection protocol " ++ prstrs selP) $
                                          do cs' <- sequence [ Impl <$> newWitness <*> return (tVar v) <*> instwildcon p | (v,p) <- selP ]
                                             simplify' env te eq (cs'++cs)
  | not $ null defaults                 = trace ("  *defaulting " ++ prstrs defaults) $
                                          do sequence [ unify env (tVar v) =<< instwild KType t | (v,t) <- defaults ]
                                             simplify' env te eq cs
  | otherwise                           = --trace ("names env:\n" ++ prstr (unalias env $ names env)) $ 
                                          trace ("no approx ") $ noSolve cs
  where info                            = varinfo cs
        Just vi                         = info
        vvs                             = varvars vi
        pvars                           = Map.keys (pbounds vi)
        lowerBnd                        = [ (v,u) | (v,[t]) <- Map.assocs (lbounds vi), u:_ <- [supImplAll env (lookup' v $ pbounds vi) t] ]
        upperBnd                        = [ (v,t) | (v,[t]) <- Map.assocs (ubounds vi), v `notElem` pvars ]
        mutC                            = [ (v,t) | (v,ns) <- Map.assocs (mutattrs vi), [t] <- [findClassByProps env ns] ]
        selC                            = [ (v,t) | (v,ns) <- Map.assocs (selattrs vi), [t] <- [findClassByAttrs env ns] ]
        selA                            = [ (v,t) | (v,ns) <- Map.assocs (selattrs vi), [t] <- [findActorByAttrs env ns] ]
        selP                            = [ (v,t) | (v,ns) <- Map.assocs (selattrs vi), [t] <- [findProtoByAttrs env ns] ]
        defaults                        = [ (v,t) | (v,ps) <- Map.assocs (pbounds vi), t:_ <- [applyDefaults env ps]]


supImplAll env [] t                     = [t]
supImplAll env ps (TCon _ c)            = [ tCon c | (_,c) <- findAncestry env c, implAll c ]
  where implAll c                       = and [ hasWitness env (tcname c) (tcname p) | (w,p) <- ps ]
supImplAll env ps t                     = []


replace ub lb c@(Cast TVar{} TVar{})    = c
replace ub lb (Cast (TVar _ v) t)
  | Just t' <- lookup v ub              = Cast t' t
replace ub lb (Cast t (TVar _ v))
  | Just t' <- lookup v lb              = Cast t t'
replace ub lb c@(Sub _ TVar{} TVar{})   = c
replace ub lb (Sub w (TVar _ v) t)
  | Just t' <- lookup v ub              = Sub w t' t
replace ub lb (Sub w t (TVar _ v))
  | Just t' <- lookup v lb              = Sub w t t'
replace ub lb c                         = c

solveDots env mutC selC selP cs         = do (eqs,css) <- unzip <$> mapM solveDot cs
                                             return (concat eqs, concat css)
  where solveDot c@(Mut (TVar _ v) n _)
          | Just w <- lookup (v,n) mutC = solveMutAttr env w c >>= \cs -> return ([], cs)
        solveDot c@(Sel _ (TVar _ v) n _)
          | Just w <- lookup (v,n) selC = solveSelAttr env w c
          | Just w <- lookup (v,n) selP = solveSelWit env w c
        solveDot c                      = return ([], [c])

ctxtReduce env vi multiPBnds            = (concat eqs, concat css)
  where (eqs,css)                       = unzip $ map ctxtRed multiPBnds
        ctxtRed (v,wps)                 = imp v [] [] [] wps
        imp v eq uni wps ((w,p):wps')
          | (w',wf,p1,p'):_ <- hits     = trace ("  *" ++ prstr p ++ " covered by " ++ prstr p1) $
                                          imp v ((w, impl2type (tVar v) p, wf (eVar w')) : eq) ((tcargs p `zip` tcargs p') ++ uni) wps wps'
          | otherwise                   = trace ("   (Not covered: " ++ prstr p ++ " in context " ++ prstrs (map snd (wps++wps')) ++ ")") $
                                          imp v eq uni ((w,p):wps) wps'
          where hits                    = [ (w',wf,p0,p') | (w',p0) <- wps++wps', w'/=w, Just (wf,p') <- [findAncestor env p0 (tcname p)] ]
        imp v eq uni wps []               = (eq, uni)
  -- TODO: also check that an mro exists (?)


remove ws []                            = []
remove ws (Impl w t p : cs)
  | w `elem` ws                         = remove ws cs
remove ws (c : cs)                      = c : remove ws cs

applyDefaults env ps                    = foldr1 intersect (map findDefault ps)
  where findDefault (w,p)               = maybe [] id $ Map.lookup (unalias env $ tcname p) defaultmap

defaultmap                              = Map.fromList [ 
                                            (qnSequence, [tList tWild]),
                                            (qnMapping, [tDict tWild tWild]),
                                            (qnSetP, [tSet tWild]),
                                            (qnIndexed, [tList tWild, tStr, tDict tWild tWild]),
                                            (qnSliceable, [tList tWild, tStr]),
                                            (qnPlus, [tInt, tFloat, tStr, tList tWild]),
                                            (qnMinus, [tInt, tFloat, tSet tWild]),
                                            (qnComplex, [tInt, tFloat]),
                                            (qnReal, [tFloat, tInt]),
                                            (qnRational, [tFloat]),
                                            (qnIntegral, [tInt]),
                                            (qnLogical, [tInt, tSet tWild]) ]


----------------------------------------------------------------------------------------------------------------------
-- Misc.
----------------------------------------------------------------------------------------------------------------------

asyncast t1@TFun{} t2@TFun{}
  | fx t2 == fxAsync                    = Cast t1 t2{ fx = tWild }  -- Special function cast for actor interfaces
asyncast fx1 fx2
  | fx2 == fxAsync                      = Cast fx1 tWild            -- Special effect cast for subtyping functions
asyncast t1 t2                          = Cast t1 t2

asynsub fx1 fx2 w t1 t2
  | fx2 == fxAsync                      = Sub w t1 t2
  | fx1 == fxAsync                      = Sub w (tMsg t1) t2        -- Special result constraint for subtyping functions
  | otherwise                           = Sub w t1 t2

asynwrap fx1 fx2 e
  | fx1 == fxAsync                      = e
  | fx2 == fxAsync                      = eCall (eQVar primASYNC) [eLambda [] e]    -- Async term wrapper for subtyping functions
  | otherwise                           = e

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

qualWPar env q                          = wit2par (qualWits env q)

qualWits env q                          = [ (tvarWit tv p, impl2type (tVar tv) p) | Quant tv ps <- q, p <- ps, isProto (tcname p) env ]

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

rowFun PRow r1 r2                       = tFun fxPure r1 kwdNil (tTupleP r2)
rowFun KRow r1 r2                       = tFun fxPure posNil r1 (tTupleK r2)

rowWit PRow w n t r wt wr               = Lambda l0 (PosPar x1 (Just t) Nothing $ PosSTAR x2 (Just $ tTupleP r)) KwdNIL eTup fxPure
  where eTup                            = Paren l0 $ Tuple l0 (PosArg e1 (PosStar (Call l0 (eVar wr) (PosStar $ eVar x2) KwdNil))) KwdNil
        e1                              = eCall (eVar wt) [eVar x1]
rowWit KRow w n t r wt wr               = Lambda l0 PosNIL (KwdPar n (Just t) Nothing $ KwdSTAR x2 (Just $ tTupleK r)) eRec fxPure
  where eRec                            = Paren l0 $ Tuple l0 PosNil (KwdArg n e1 (KwdStar (Call l0 (eVar wr) PosNil (KwdStar $ eVar x2))))
        e1                              = eCall (eVar wt) [eVar n]


wFun t1 t2                              = tFun fxPure (posRow t1 posNil) kwdNil t2
