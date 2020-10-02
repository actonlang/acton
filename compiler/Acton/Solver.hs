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
import Acton.Subst
import Acton.TypeM
import Acton.TypeEnv




-- Reduce conservatively and remove entailed constraints
simplify                                    :: (Polarity a, Pretty a) => Env -> TEnv -> a -> Constraints -> TypeM (Constraints,Equations)
simplify env te tt cs                       = do cs <- msubst cs
                                                 te <- msubst te
                                                 traceM ("  -simplify: " ++ prstrs cs)
                                                 traceM ("  -for: " ++ prstr te)
                                                 simplify' env te tt [] cs

simplify'                                   :: (Polarity a, Pretty a) => Env -> TEnv -> a -> Equations -> Constraints -> TypeM (Constraints,Equations)
simplify' env te tt eq []                   = return ([], eq)
simplify' env te tt eq cs                   = do eq1 <- reduce env eq cs
                                                 cs1 <- msubst =<< collectDeferred
                                                 env1 <- msubst env 
                                                 te1 <- msubst te
                                                 tt1 <- msubst tt
                                                 --traceM ("############## env:\n" ++ prstr [ (n,i) | (n,i@NAct{}) <- names env1 ])
                                                 --traceM ("############## cs:\n" ++ render (nest 4 $ vcat (map pretty cs1)) ++ "\n##############")
                                                 improve env1 te1 tt1 eq1 cs1


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

reduce' env eq c@(Impl w t@(TOpt _ t') p)
  | qmatch env (tcname p) qnIdentity        = return ((w, impl2type t p, eQVar witIdentityOpt):eq)
  | qmatch env (tcname p) qnEq              = do w' <- newWitness
                                                 let e = eCall (eQVar witEqOpt) [eVar w']
                                                 reduce env ((w, impl2type t p, e):eq) [Impl w' t' p]

reduce' env eq c@(Impl w t@(TUnion _ us) p)
  | qmatch env (tcname p) qnEq              = do let e = eQVar $ if all uniLit us then witEqStr else witEqUnion
                                                 return ((w, impl2type t p, e):eq)

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
                                            = do let e = eLambda [(px0,t1)] (eDot (eVar px0) n)
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

reduce' env eq (Seal Nothing (TVar _ v1) (TVar _ v2) t1 t2)
  | v1 == v2                                = reduce env eq [Cast t1 t2]
reduce' env eq (Seal (Just w) fx@(TVar _ v1) (TVar _ v2) t1 t2)
  | v1 == v2                                = do let e = eCall (eVar px0) []
                                                 reduce env ((w, wFun t t2, lambdaFX e):eq) [Cast t1 t2]
  where lambdaFX e                          = Lambda NoLoc (pospar [(px0,t)]) KwdNIL e fx
        t                                   = tFun fx posNil kwdNil t1
reduce' env eq c@(Seal w TVar{} _ _ _)      = do defer [c]; return eq                               -- Defer until both effects
reduce' env eq c@(Seal w _ TVar{} _ _)      = do defer [c]; return eq                               -- are instantiated
reduce' env eq (Seal Nothing fx1 fx2 t1 t2)
  | fx1 == fxAction, fx2 /= fxAction        = do traceM ("  #sealing cast")                         -- Unsealing:
                                                 let cs = [Cast fx1 fx2, Cast (tMsg t1) t2]         --   Relate the effects, result must be a message
                                                 reduce env eq cs
  | otherwise                               = do traceM ("  #no sealing cast")                      -- No sealing needed:
                                                 let cs = [Cast fx1 fx2, Cast t1 t2]                --   Relate the effects and result types
                                                 reduce env eq cs
reduce' env eq (Seal (Just w) fx1 fx2 t1 t2)
  | fx1 /= fxAction, fx2 == fxAction        = do traceM ("  #sealing " ++ prstr w)                  -- Sealing:
                                                 let e = eCall (eQVar primASYNC) [eVar px0]         --   Wrap closure into an async message
                                                     cs = [Cast t1 t2]                              --   Relate the result types, fx1 can be anything
                                                 reduce env ((w, wFun t t2, lambdaFX e):eq) cs
  | fx1 == fxAction, fx2 /= fxAction        = do traceM ("  #unsealing " ++ prstr w)                -- Unsealing:
                                                 let e = eCall (eVar px0) []                        --   Call action closure right away
                                                     cs = [Cast fx1 fx2, Cast (tMsg t1) t2]         --   Relate the effects, result must be a message
                                                 reduce env ((w, wFun t t2, lambdaFX e):eq) cs
  | otherwise                               = do traceM ("  #no sealing " ++ prstr w)               -- No sealing needed:
                                                 let e = eCall (eVar px0) []                        --   Call closure right away
                                                     cs = [Cast fx1 fx2, Cast t1 t2]                --   Relate the effects and result types
                                                 reduce env ((w, wFun t t2, lambdaFX e):eq) cs
  where lambdaFX e                          = Lambda NoLoc (pospar [(px0,t)]) KwdNIL e fx1
        t                                   = tFun fx1 posNil kwdNil t1

reduce' env eq c                            = noRed c


solveSelAttr env (wf,sc,_) (Sel w t1 n t2)  = do (cs1,tvs,t) <- instantiate env sc
                                                 -- when (tvSelf `elem` contrafree t) (err1 n "Contravariant Self attribute not selectable by instance")
                                                 let e = eLambda [(px0,t1)] (app t (tApp (eDot (wf $ eVar px0) n) tvs) $ witsOf cs1)
                                                     cs = Cast (subst [(tvSelf,t1)] t) t2 : cs1
--                                                 traceM ("### solveSelAttr unify " ++ prstr (subst [(tvSelf,t1)] t) ++ " " ++ prstr t2)
--                                                 unify env (subst [(tvSelf,t1)] t) t2
--                                                 return ([(w, wFun t1 t2, e)], cs1)
                                                 return ([(w, wFun t1 t2, e)], cs)

solveSelWit env wit (Sel w t1 n t2)         = do let ts = case t1 of TCon _ c -> tcargs c; _ -> []
                                                 (cs1,p,we) <- instWitness env ts wit
                                                 let Just (wf,sc,dec) = findAttr env p n
                                                 (cs2,tvs,t) <- instantiate env sc
                                                 let e = eLambda [(px0,t1)] (app t (tApp (eDot (wf we) n) tvs) $ witsOf cs2 ++ [eVar px0])
                                                     cs = Cast (subst [(tvSelf,t1)] t) t2 : cs1 ++ cs2
--                                                 traceM ("### solveSelWit unify " ++ prstr (subst [(tvSelf,t1)] t) ++ " " ++ prstr t2)
--                                                 unify env (subst [(tvSelf,t1)] t) t2
--                                                 return ([(w, wFun t1 t2, e)], cs1++cs2)
                                                 return ([(w, wFun t1 t2, e)], cs)

solveMutAttr env (wf,sc,dec) (Mut t1 n t2)  = do when (dec/=Property) (noMut n)
                                                 let TSchema _ [] t = sc
                                                     cs = [Cast t1 tObject, Cast t2 (subst [(tvSelf,t1)] t)]
                                                 return cs

----------------------------------------------------------------------------------------------------------------------
-- cast
----------------------------------------------------------------------------------------------------------------------

cast                                        :: Env -> Type -> Type -> TypeM ()
cast env t1 t2                              = do t1' <- msubst t1
                                                 t2' <- msubst t2
                                                 traceM ("   cast " ++ prstr t1 ++ " < " ++ prstr t2)
                                                 cast' env t1' t2'

castM env ts1 ts2                           = mapM_ (uncurry $ cast env) (ts1 `zip` ts2)


cast' env (TWild _) t2                      = return ()
cast' env t1 (TWild _)                      = return ()

cast' env (TCon _ c1) (TCon _ c2)
  | Just (wf,c') <- search                  = unifyM env (tcargs c') (tcargs c2)        -- TODO: cast/unify based on polarities
  where search                              = findAncestor env c1 (tcname c2)

cast' env (TFun _ fx1 p1 k1 t1) (TFun _ fx2 p2 k2 t2)
                                            = do reduce env [] [Seal Nothing fx1 fx2 t1 t2]
                                                 cast env p2 p1
                                                 cast env k2 k1

cast' env (TTuple _ p1 k1) (TTuple _ p2 k2)
                                            = do cast env p1 p2
                                                 cast env k1 k2

cast' env (TUnion _ us1) (TUnion _ us2)
  | all (uniElem us2) us1                   = return ()
cast' env (TCon _ c1) (TUnion _ us2)
  | uniConElem env us2 c1                   = return ()

cast' env (TOpt _ t1@TOpt{}) t2             = cast env t1 t2
cast' env t1 (TOpt _ t2@TOpt{})             = cast env t1 t2
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
        castFX FXAction FXAction            = Just $ return ()
        castFX FXAction (FXAct _)           = Just $ return ()
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
  | univar tv                               = do t1 <- instwild env KType $ tFun tWild tWild tWild tWild
                                                 substitute tv t1
                                                 cast env t1 t2
cast' env t1@TFun{} (TVar _ tv)
  | univar tv                               = do t2 <- instwild env KType $ tFun tWild tWild tWild tWild
                                                 substitute tv t2
                                                 cast env t1 t2

cast' env (TVar _ tv) t2@TTuple{}
  | univar tv                               = do t1 <- instwild env KType $ tTuple tWild tWild
                                                 substitute tv t1
                                                 cast env t1 t2
cast' env t1@TTuple{} (TVar _ tv)
  | univar tv                               = do t2 <- instwild env KType $ tTuple tWild tWild
                                                 substitute tv t2
                                                 cast env t1 t2

cast' env (TVar _ tv1) (TVar _ tv2)
  | tv1 == tv2                              = return ()

cast' env t1@(TVar _ tv) t2
  | univar tv                               = trace ("   deferring " ++ prstr (Cast t1 t2)) $ defer [Cast t1 t2]
  | Just tc <- findTVBound env tv           = cast' env (tCon tc) t2

cast' env t1 t2@(TVar _ tv)
  | univar tv                               = defer [Cast t1 t2]
  | otherwise                               = noRed (Cast t1 t2)

cast' env (TUnion _ us1) t2
  | all uniLit us1                          = cast env tStr t2              -- Only matches when t2 is NOT a variable

cast' env t1 (TOpt _ t2)                    = cast env t1 t2                -- Only matches when t1 is NOT a variable

cast' env t1 t2                             = noRed (Cast t1 t2)


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

unify' env (TUnion _ us1) (TUnion _ us2)
  | all (uniElem us2) us1,
    all (uniElem us1) us2                   = return ()

unify' env (TOpt _ t1) (TOpt _ t2)          = unify env t1 t2
unify' env (TNone _) (TNone _)              = return ()

unify' env (TFX _ fx1) (TFX _ fx2)
  | Just unifs <- unifyFX fx1 fx2           = unifs
  where unifyFX FXPure FXPure               = Just $ return ()
        unifyFX (FXMut t1) (FXMut t2)       = Just $ unify env t1 t2
        unifyFX (FXAct t1) (FXAct t2)       = Just $ unify env t1 t2
        unifyFX FXAction FXAction           = Just $ return ()
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

sub' env eq w t1@TWild{} t2                 = return (idwit w t1 t2 : eq)
sub' env eq w t1 t2@TWild{}                 = return (idwit w t1 t2 : eq)

--                as declared               as called
--                existing                  expected
sub' env eq w t1@(TFun _ fx1 p1 k1 t1') t2@(TFun _ fx2 p2 k2 t2')                   -- TODO: implement pos/kwd argument shifting
                                            = do wx <- newWitness
                                                 wp <- newWitness
                                                 wk <- newWitness
                                                 wt <- newWitness
                                                 tv <- newTVar
                                                 let e = eLambda [(px0,t1)] e'
                                                     e' = Lambda l0 (PosSTAR px1 $ Just $ tTupleP p2) (KwdSTAR px2 $ Just $ tTupleK k2) e0 fx2
                                                     e0 = eCall (eVar wx) [lambda0 fx1 $ eCall (eVar wt) [Call l0 (eVar px0) (PosStar e1) (KwdStar e2)]]
                                                     e1 = eCall (eVar wp) [eVar px1]
                                                     e2 = eCall (eVar wk) [eVar px2]
                                                     cs = [Seal (Just wx) fx1 fx2 t1' tv, Sub wp p2 p1, Sub wk k2 k1, Sub wt tv t2']

                                                 reduce env ((w, wFun t1 t2, e):eq) cs


--                existing            expected
sub' env eq w t1@(TTuple _ p1 k1) t2@(TTuple _ p2 k2)                               -- TODO: implement pos/kwd argument shifting
                                            = do wp <- newWitness
                                                 wk <- newWitness
                                                 let e = eLambda [(px0,t1)] (Paren l0 $ Tuple l0 (PosStar e1) (KwdStar e2))
                                                     e1 = eCall (eVar wp) [Paren l0 $ Tuple l0 (PosStar $ eVar px0) KwdNil]
                                                     e2 = eCall (eVar wk) [Paren l0 $ Tuple l0 PosNil (KwdStar $ eVar px0)]
                                                     cs = [Sub wp p1 p2, Sub wk k1 k2]
                                                 reduce env ((w, wFun t1 t2, e):eq) cs

-- Note: a sub-row constraint R1 < R2 is witnessed by a lambda of type
-- (*(R1))->(*(R2)) or (**(R1))->(**(R2)), depending on the row kind

sub' env eq w r1@(TNil _ k1) r2@(TNil _ k2)
  | k1 == k2                                = return (idwit w tUnit tUnit : eq)

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
  | univar tv                               = do t1 <- instwild env KType $ tFun tWild tWild tWild tWild
                                                 substitute tv t1
                                                 sub env eq w t1 t2
sub' env eq w t1@TFun{} (TVar _ tv)
  | univar tv                               = do t2 <- instwild env KType $ tFun tWild tWild tWild tWild
                                                 substitute tv t2
                                                 sub env eq w t1 t2


sub' env eq w (TVar _ tv) t2@TTuple{}
  | univar tv                               = do t1 <- instwild env KType $ tTuple tWild tWild
                                                 substitute tv t1
                                                 sub env eq w t1 t2
sub' env eq w t1@TTuple{} (TVar _ tv)
  | univar tv                               = do t2 <- instwild env KType $ tTuple tWild tWild
                                                 substitute tv t2
                                                 sub env eq w t1 t2

sub' env eq w t1@(TVar _ tv1) t2@(TVar _ tv2)
  | tv1 == tv2                              = do return (idwit w t1 t2 : eq)

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
w = lambda x: lambda *x1,**x2: wt(x(*wp(x1), **wk(x2)))                 = (lambda x: lambda *x1,**x2: wt(x(*wp(x1), **wk(x2))))(round)(3.14, None)          | x=round
wt : (Real) -> $1                                                       = lambda *x1,**x2: wt(round(*wp(x1), **wk(x2)))(3.14, None)                         | x1=(3.14,None), x2=()
wp : ((float,None)) -> (Real,?int)                                      = wt(round(*wp((3.14,None)), **wk(())))
wk : (()) -> ()
----
wt = lambda x: x                                                        = (lambda x: x)(round(*wp((3.14,None)), **wk(())))                                  | x=round(...)
wp = lambda x: (w1(x.0), *w2(x.*1))                                     = round(*(lambda x: (w1(x.0), *w2(x.*1)))((3.14,None)), **wk(()))                   | x=(3.14, None)
wk = lambda y: ()                                                       = round(*(w1(3.14), *w2((None,))), **((lambda y: ())()))                            | y=()
w1 : (float) -> Real                                                    = round(*(w1(3.14), *w2((None,))), **())
w2 : ((None,)) -> (?int,)
----
w1 = lambda x: PACK(Real$float, x)                                      = round(*((lambda x: PACK(Real$float, x))(3.14), *w2((None,))))                     | x=3.14
w2 = lambda x: (w21(x.0), *w22(x.*1))                                   = round(*(PACK(Real$float, 3.14), *(lambda x: (w21(x.0), *w22(x.*1)))((None,))))    | x=(None,)
w21 : (None) -> ?int                                                    = round(*(PACK(Real$float, 3.14), *(w21(None), *w22(()))))
w22 : (()) -> ()
----
w21 = lambda x: x                                                       = round(*(PACK(Real$float, 3.14), *((lambda x: x)(None), *w22(()))))                | x=None
w22 = lambda y: ()                                                      = round(*(PACK(Real$float, 3.14), *(None, *(lambda y: ())())))                      | y=()
                                                                        = round(*(PACK(Real$float, 3.14), *(None, *())))
                                                                        = round(*(PACK(Real$float, 3.14), *(None,)))
                                                                        = round(*(PACK(Real$float, 3.14), None))
                                                                        = round(PACK(Real$float, 3.14), None)



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
    f (Impl w (TVar _ v) p : cs)            = f cs . pbound v w p . embed (tyfree p)
    f (Mut (TVar _ v) n t : cs)             = f cs . mutattr v n . embed (tyfree t)
    f (Sel _ (TVar _ v) n t : cs)           = f cs . selattr v n . embed (tyfree t)
    f (Seal w fx1 fx2 t1 t2 : cs)
      | TVar _ v1 <- fx1, TVar _ v2 <- fx2  = f cs . varvar v1 v2 . embed (tyfree [t1,t2])
      | TVar _ v <- fx1                     = f cs . ubound v fx2 . embed (tyfree [t1,t2])
      | TVar _ v <- fx2                     = f cs . lbound v fx1 . embed (tyfree [t1,t2])
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

gsimp cl obs []                         = []
gsimp cl obs ((x,y):xys)
  | not subsumed                        = gsimp cl obs xys
  | x_obs && y_obs                      = gsimp cl obs xys
  | x_obs                               = (x,y) : gsimp cl (y:obs) xys
  | y_obs                               = (x,y) : gsimp cl (x:obs) xys
  | otherwise                           = (x,y) : gsimp cl obs xys
  where subsumed                        = (above x cl \\ [y]) `eq` above y cl && (below y cl \\ [x]) `eq` below x cl
        a `eq` b                        = all (`elem` b) a && all (`elem` a) b
        x_obs                           = x `elem` obs
        y_obs                           = y `elem` obs

instwild env k (TWild _)                = newTVarOfKind k
instwild env _ (TFun l e p k t)         = TFun l <$> instwild env KFX e <*> instwild env PRow p <*> instwild env KRow k <*> instwild env KType t
instwild env _ (TTuple l p k)           = TTuple l <$> instwild env PRow p <*> instwild env KRow k
instwild env _ (TOpt l t)               = TOpt l <$> instwild env KType t
instwild env _ (TCon l c)               = TCon l <$> instwildcon env c
instwild env _ (TRow l k n t r)         = TRow l k n <$> instwild env KType t <*> instwild env k r
instwild env _ (TFX l (FXMut t))        = TFX l <$> FXMut <$> instwild env KType t
instwild env _ (TFX l (FXAct t))        = TFX l <$> FXAct <$> instwild env KType t
instwild env k t                        = return t

instwildcon env c                       = case tconKind (tcname c) env of
                                            KFun ks _ -> TC (tcname c) <$> sequence [ instwild env k t | (k,t) <- ks `zip` tcargs c ]
                                            _ -> return $ TC (tcname c) []

----------------------------------------------------------------------------------------------------------------------
-- GLB
----------------------------------------------------------------------------------------------------------------------

mkGLB                                   :: Env -> (TVar,[Type]) -> TypeM (TVar,Type)
mkGLB env (v,ts)                        = do t <- instwild env KType $ foldr1 (glb env) ts
                                             traceM ("   glb " ++ prstrs ts ++ " = " ++ prstr t)
                                             return (v, t)

glb env (TWild _) t2                    = t2
glb env t1 (TWild _)                    = t1

glb env TVar{} _                        = tWild        -- (Might occur in recursive calls)
glb env _ TVar{}                        = tWild        -- (Might occur in recursive calls)

glb env (TCon _ c1) (TCon _ c2)
  | qmatch env (tcname c1) (tcname c2)  = tCon c1
  | hasAncestor env c1 c2               = tCon c1
  | hasAncestor env c2 c1               = tCon c2

glb env (TFun _ e1 p1 k1 t1) (TFun _ e2 p2 k2 t2)                                           -- tWilds instead of glbs enable the special
                                        = tFun tWild (lub env p1 p2) (lub env k1 k2) tWild  -- async rules in sub and cast
glb env (TTuple _ p1 k1) (TTuple _ p2 k2)
                                        = tTuple (glb env p1 p2) (glb env k1 k2)

glb env (TUnion _ us1) (TUnion _ us2)
  | [UCon qn] <- us                     = tCon (TC qn [])
  | not $ null us                       = tUnion us
  where us                              = us1 `intersect` us2
glb env t1@(TUnion _ us) t2@(TCon _ c)
  | uniConElem env us c                 = t2
  | all uniLit us && isStr env t2       = t1
glb env t1@(TCon _ c) t2@(TUnion _ us)
  | uniConElem env us c                 = t1
  | all uniLit us && isStr env t1       = t2
  
glb env (TOpt _ t1) (TOpt _ t2)         = tOpt (glb env t1 t2)
glb env (TNone _) t2                    = tNone
glb env t1 (TNone _)                    = tNone
glb env (TOpt _ t1) t2                  = glb env t1 t2
glb env t1 (TOpt _ t2)                  = glb env t1 t2

glb env t1@(TFX _ fx1) t2@(TFX _ fx2)   = tTFX (glfx fx1 fx2)
  where glfx FXAction FXAction          = FXAction
        glfx FXAction (FXAct _)         = FXAction
        glfx (FXAct _) FXAction         = FXAction
        glfx FXAction fx2               = noGLB t1 t2
        glfx fx1 FXAction               = noGLB t1 t2
        glfx FXPure _                   = FXPure
        glfx _ FXPure                   = FXPure
        glfx (FXMut t1) _               = FXMut t1
        glfx _ (FXMut t2)               = FXMut t2
        glfx (FXAct t1) (FXAct t2)      = FXAct t1

glb env (TNil _ k1) (TNil _ k2)
  | k1 == k2                            = tNil k1
glb env (TRow _ k n t1 r1) r
  | Just (t2,r2) <- findInRow n r       = tRow k n (glb env t1 t2) (glb env r1 r2)

glb env t1 t2                           = noGLB t1 t2
    
noGLB t1 t2                             = err1 t1 ("No common subtype: " ++ prstr t2)

isStr env (TCon _ c)                    = qmatch env (tcname c) qnStr



----------------------------------------------------------------------------------------------------------------------
-- LUB
----------------------------------------------------------------------------------------------------------------------

mkLUB env (v,ts)                        = do traceM ("   lub " ++ prstrs ts ++ " ...")
                                             t <- instwild env KType $ foldr1 (lub env) ts
                                             traceM ("   lub " ++ prstrs ts ++ " = " ++ prstr t)
                                             return (v, t)

lub env (TWild _) t2                    = t2
lub env t1 (TWild _)                    = t1

lub env TVar{} _                        = tWild        -- (Might occur in recursive calls)
lub env _ TVar{}                        = tWild        -- (Might occur in recursive calls)

lub env (TCon _ c1) (TCon _ c2)
  | qmatch env (tcname c1) (tcname c2)  = tCon c1
  | Just u1 <- uniCon env c1,
    Just u2 <- uniCon env c2            = tUnion [u1, u2]
  | hasAncestor env c1 c2               = tCon c2
  | hasAncestor env c2 c1               = tCon c1
  | not $ null common                   = tCon $ head common
  where common                          = commonAncestors env c1 c2

lub env (TFun _ e1 p1 k1 t1) (TFun _ e2 p2 k2 t2)
                                        = tFun (lub env e1 e2) (glb env p1 p2) (glb env k1 k2) (lub env t1 t2)
lub env (TTuple _ p1 k1) (TTuple _ p2 k2)
                                        = tTuple (lub env p1 p2) (lub env k1 k2)

lub env (TUnion _ us1) (TUnion _ us2)   = tUnion $ us1 `union` us2
lub env t1@(TUnion _ us) t2@(TCon _ c)
  | all uniLit us && t2 == tStr         = t2
  | Just u <- uniCon env c              = tUnion $ us `union` [u]
lub env t1@(TCon _ c) t2@(TUnion _ us)
  | all uniLit us && t1 == tStr         = t1
  | Just u <- uniCon env c              = tUnion $ us `union` [u]
  
lub env (TOpt _ t1) (TOpt _ t2)         = tOpt (lub env t1 t2)
lub env (TNone _) t2@TOpt{}             = t2
lub env t1@TOpt{} (TNone _)             = t1
lub env (TNone _) t2                    = tOpt t2
lub env t1 (TNone _)                    = tOpt t1
lub env (TOpt _ t1) t2                  = tOpt $ lub env t1 t2
lub env t1 (TOpt _ t2)                  = tOpt $ lub env t1 t2

lub env t1@(TFX _ fx1) t2@(TFX _ fx2)   = tTFX (lufx fx1 fx2)
  where lufx FXAction FXAction          = FXAction
        lufx FXAction _                 = FXAct tWild
        lufx _ FXAction                 = FXAct tWild
        lufx (FXAct t1) _               = FXAct t1
        lufx _ (FXAct t2)               = FXAct t2
        lufx (FXMut t1) _               = FXMut t1
        lufx _ (FXMut t2)               = FXMut t2
        lufx FXPure FXPure              = FXPure

lub env (TNil _ k1) (TNil _ k2)
  | k1 == k2                            = tNil k1
lub env (TRow _ k n t1 r1) r
  | Just (t2,r2) <- findInRow n r       = tRow k n (lub env t1 t2) (lub env r1 r2)

lub env t1 t2                           = noLUB t1 t2

noLUB t1 t2                             = err1 t1 ("No common supertype: " ++ prstr t2)

----------------------------------------------------------------------------------------------------------------------
-- Improvement
----------------------------------------------------------------------------------------------------------------------

-- Check if the deferred constraint set should be resubmitted
-- Unify all var cycles
-- Perform G-simplification on internal variables
-- Check that there's at least one non-embedded variable
-- For all non-embedded variables: replace multiple lower/upper con bounds with a LUB/GLB
-- For non-embedded variables with a single lower/upper bound: replace with bound if not negative/positive
-- For non-embedded variables with multiple protocol constraints: identify equal and subtype-related protocols

-- After improvement:
--  headvar is defined
--  Cast/Sub bound is either TVar (upper), TCon, TUnion, TNone (lower), TOpt (upper) or TFX
--  acyclic
--  G-minimal (constrained vars are observable)
--  S-minimal (constrained vars are invariant)
--  just single upper/lower bounds
--  no closed upper/lower bounds
--  no redundant Impl
--  no Sel/Mut covered by Cast/Sub/Impl bounds


improve                                 :: (Polarity a, Pretty a) => Env -> TEnv -> a -> Equations -> Constraints -> TypeM (Constraints,Equations)
improve env te tt eq []                 = return ([], eq)
improve env te tt eq cs
  | Nothing <- info                     = do traceM ("  *Resubmit")
                                             simplify' env te tt eq cs
  | Left (v,vs) <- closure              = do traceM ("  *Unify cycle " ++ prstr v ++ " = " ++ prstrs vs)
                                             sequence [ unify env (tVar v) (tVar v') | v' <- vs ]
                                             simplify' env te tt eq cs
  | not $ null gsimple                  = do traceM ("  *G-simplify " ++ prstrs [ (v,tVar v') | (v,v') <- gsimple ])
                                             traceM ("  *obsvars: " ++ prstrs obsvars)
                                             traceM ("  *varvars: " ++ prstrs (varvars vi))
                                             sequence [ unify env (tVar v) (tVar v') | (v,v') <- gsimple ]
                                             simplify' env te tt eq cs
  | not $ null cyclic                   = err2 cyclic ("Cyclic subtyping:")
  | not $ null openFX                   = do traceM ("  *Close actor FX: " ++ prstrs openFX)
                                             simplify' env te tt eq ([ Cast (tVar tv) fxAction | tv <- openFX ] ++ cs)
  | not $ null (multiUBnd++multiLBnd)   = do ub <- mapM (mkGLB env) multiUBnd
                                             lb <- mapM (mkLUB env) multiLBnd
                                             traceM ("  *GLB " ++ prstrs ub)
                                             traceM ("  *LUB " ++ prstrs lb)
                                             let cs' = [ Cast (tVar v) t | (v,t) <- ub ] ++ [ Cast t (tVar v) | (v,t) <- lb ]
                                             simplify' env te tt eq (cs' ++ map (replace ub lb) cs)
  | not $ null posLBnd                  = do traceM ("  *S-simplify (dn) " ++ prstrs posLBnd)
                                             sequence [ unify env (tVar v) t | (v,t) <- posLBnd ]
                                             simplify' env te tt eq cs
  | not $ null negUBnd                  = do traceM ("  *S-simplify (up) " ++ prstrs negUBnd)
                                             sequence [ unify env (tVar v) t | (v,t) <- negUBnd ]
                                             simplify' env te tt eq cs
  | not $ null closUBnd                 = do traceM ("  *Simplify upper closed bound " ++ prstrs closUBnd)
                                             sequence [ unify env (tVar v) t | (v,t) <- closUBnd ]
                                             simplify' env te tt eq cs
  | not $ null closLBnd                 = do traceM ("  *Simplify lower closed bound " ++ prstrs closLBnd)
                                             sequence [ unify env (tVar v) t | (v,t) <- closLBnd ]
                                             simplify' env te tt eq cs
  | not $ null redEq                    = do traceM ("  *(Context red) " ++ prstrs [ w | (w,_,_) <- redEq ])
                                             sequence [ unify env t1 t2 | (t1,t2) <- redUni ]
                                             simplify' env te tt (redEq++eq) (remove [ w | (w,_,_) <- redEq ] cs)
  | not $ null dots                     = do traceM ("  *Implied mutation/selection solutions " ++ prstrs dots)
                                             (eq',cs') <- solveDots env mutC selC selP cs
                                             simplify' env te tt (eq'++eq) cs'
  | otherwise                           = trace ("  *improvement done") $ return (cs, eq)
  where info                            = varinfo cs
        Just vi                         = info
        closure                         = varclose (varvars vi)
        Right vclosed                   = closure
        (vvsL,vvsU)                     = unzip vclosed
        gsimple                         = gsimp vclosed obsvars (varvars vi)
        openFX                          = [ v | v <- actFX, fxAction `notElem` lookup' v (ubounds vi) ]
        actFX                           = [ v | (n,i@NAct{}) <- te ++ names env, v <- tyfree i, univar v, tvkind v == KFX ]
        multiUBnd                       = [ (v,ts) | (v,ts) <- Map.assocs (ubounds vi), v `notElem` embedded vi, length ts > 1 ]
        multiLBnd                       = [ (v,ts) | (v,ts) <- Map.assocs (lbounds vi), v `notElem` embedded vi, length ts > 1 ]
        multiPBnd                       = [ (v,ps) | (v,ps) <- Map.assocs (pbounds vi), length ps > 1 ]
        lowerBnd                        = [ (v,t) | (v,[t]) <- Map.assocs (lbounds vi), v `notElem` embedded vi ]
        upperBnd                        = [ (v,t) | (v,[t]) <- Map.assocs (ubounds vi), v `notElem` embedded vi ]
        posLBnd                         = [ (v,t) | (v,t) <- lowerBnd, v `notElem` negvars, implAll env (lookup' v $ pbounds vi) t ]
        negUBnd                         = [ (v,t) | (v,t) <- upperBnd, v `notElem` posvars, implAll env (lookup' v $ pbounds vi) t ]
        closLBnd                        = [ (v,t) | (v, [t]) <- Map.assocs (lbounds vi), lClosed env t ]
        closUBnd                        = [ (v,t) | (v, [t]) <- Map.assocs (ubounds vi), uClosed env t ]
        (redEq,redUni)                  = ctxtReduce env vi multiPBnd
        mutC                            = findBoundAttrs env (mutattrs vi) (ubounds vi)
        selC                            = findBoundAttrs env (selattrs vi) (ubounds vi)
        selP                            = findWitAttrs env (selattrs vi) (pbounds vi)
        dots                            = dom mutC ++ dom selC ++ dom selP
        fixedvars                       = tyfree env
        pvars                           = Map.keys (pbounds vi) ++ tyfree (Map.elems (pbounds vi))
        sealL                           = [ v | Seal _ (TVar _ v) _ _ _ <- cs ]
        sealU                           = [ v | Seal _ _ (TVar _ v) _ _ <- cs ]
        (posvars0,negvars0)             = polvars te `polcat` polvars tt
        (posvars,negvars)               = (posvars0++fixedvars++vvsL++sealL, negvars0++fixedvars++vvsU++sealU)
        obsvars                         = posvars0 ++ negvars0 ++ fixedvars ++ pvars ++ embedded vi
        boundvars                       = Map.keys (ubounds vi) ++ Map.keys (lbounds vi)
        boundprot                       = tyfree (Map.elems $ ubounds vi) ++ tyfree (Map.elems $ lbounds vi)
        cyclic                          = if null (boundvars\\boundprot) then [ c | c <- cs, headvar c `elem` boundvars ] else []

uClosed env (TCon _ c)                  = isActor env (tcname c)
uClosed env (TFX _ FXPure)              = True
uClosed env (TNone _)                   = True
uClosed env (TNil _ _)                  = True
uClosed env _                           = False

lClosed env (TUnion _ us)
  | uniMax us                           = True
lClosed env (TOpt _ _)                  = True
lClosed env (TNil _ _)                  = True
lClosed env _                           = False

findBoundAttrs env attrs bounds         = [ ((v,n),wsc) | (v,ns) <- Map.assocs attrs, n <- ns, wsc <- bounds' v n ]
  where bounds' v n                     = [ wsc | TCon _ c <- lookup' v bounds, Just wsc <- [findAttr env c n] ]

findWitAttrs env attrs bounds           = [ ((v,n), WInst p (NoQ w) ws) | (v,ns) <- Map.assocs attrs, n <- ns, (w,p,ws) <- bounds' v n ]
  where bounds' v n                     = [ (w,p,ws) | (w,p0) <- lookup' v bounds, (ws,p) <- findAncestry env p0, n `elem` conAttrs env (tcname p) ]


data Candidate                          = CProto QName
                                        | CCon QName
                                        | CVar TVar
                                        | CNone
                                        | COpt
                                        | CUnion [QName]
                                        | CFun
                                        | CTuple
                                        | CPure
                                        | CMut
                                        | CAct
                                        | CAction
                                        deriving (Eq,Show)

instance Pretty Candidate where
    pretty (CProto n)                   = pretty n
    pretty (CCon n)                     = pretty n
    pretty (CVar v)                     = pretty v
    pretty (CUnion ns)                  = pretty (tUnion (map UCon ns))
    pretty c                            = text (show c)

allAbove env (TCon _ c@(TC n _))
  | n' `elem` uniCons                   = map CCon (n' : allAncestors env n) ++ allAbove env (tUnion [UCon n']) ++ [CNone]
  | otherwise                           = map CCon (n' : allAncestors env n) ++ [CNone]
  where n'                              = unalias env n
allAbove env (TVar _ v) | not(univar v) = case findTVBound env v of Just c -> [CVar v, CCon (tcname c)]; _ -> [CVar v]
allAbove env (TOpt _ t)                 = [COpt]
allAbove env (TNone _)                  = [CNone]
allAbove env (TFun _ _ _ _ _)           = [CFun,CNone]
allAbove env (TTuple _ _ _)             = [CTuple,CNone]
allAbove env (TFX _ FXPure)             = [CPure,CMut,CAct]
allAbove env (TFX _ (FXMut _))          = [CMut,CAct]
allAbove env (TFX _ (FXAct _))          = [CAct]
allAbove env (TFX _ FXAction)           = [CAction,CAct]
allAbove env (TUnion _ us)
  | all uniLit us                       = CCon qnStr : map CUnion (uniAbove [UCon qnStr]) ++ [CNone]
  | otherwise                           = map CUnion (uniAbove us) ++ [CNone]
allAbove env _                          = []


allBelow env (TCon _ (TC n _))          = map CCon (unalias env n : allDescendants env n)
allBelow env (TVar _ v) | not(univar v) = [CVar v]
allBelow env (TOpt _ t)                 = COpt : allBelow env t ++ [CNone]
allBelow env (TNone _)                  = [CNone]
allBelow env (TFun _ _ _ _ _)           = [CFun]
allBelow env (TTuple _ _ _)             = [CTuple]
allBelow env (TFX _ FXPure)             = [CPure]
allBelow env (TFX _ (FXMut _))          = [CMut,CPure]
allBelow env (TFX _ (FXAct _))          = [CAct,CMut,CPure,CAction]
allBelow env (TFX _ FXAction)           = [CAction]
allBelow env (TUnion _ us)              = map CUnion (uniBelow us) ++ [ CCon n | UCon n <- us ]
allBelow env _                          = []

protos env (CCon n)                     = map (tcname . proto) $ allWitnesses env n
protos env (CVar v)                     = map (tcname . proto) $ allWitnesses env (NoQ $ tvname v)
protos env CNone                        = [qnIdentity,qnEq]
protos env COpt                         = [qnIdentity,qnEq]
protos env (CUnion _)                   = [qnEq]
protos env _                            = []

attrs env (CProto n)                    = allAttrs env n
attrs env (CCon n)                      = allAttrs env n
attrs env (CVar v)                      = maybe [] (allAttrs env . tcname) $ findTVBound env v
attrs env (CUnion ns)                   = foldr1 intersect $ map (allAttrs env) ns
attrs env _                             = []

protoattrs env c                        = concat [ allAttrs env n | n <- protos env c ]

type Solution                           = Map TVar [Candidate]

constrain                               :: Env -> Solution -> Constraint -> Solution
constrain env vs (Cast (TVar _ v) (TVar _ v'))
  | univar v && univar v'               = vs
constrain env vs (Cast (TVar _ v) t)
  | univar v                            = Map.adjust (intersect $ allBelow env t) v vs
constrain env vs (Cast t (TVar _ v))    = Map.adjust (intersect $ allAbove env t) v vs
constrain env vs c@(Sub w (TVar _ v) (TVar _ v'))
  | univar v && univar v'               = vs
constrain env vs (Sub w (TVar _ v) t)
  | univar v                            = Map.adjust (intersect $ allBelow env t) v vs
constrain env vs (Sub w t (TVar _ v))   = Map.adjust (intersect $ allAbove env t) v vs
constrain env vs (Impl w (TVar _ v) p)  = Map.adjust (filter (\c -> any (qmatch env $ tcname p) (protos env c))) v vs
constrain env vs (Mut (TVar _ v) n t)   = Map.adjust (filter (\c -> n `elem` attrs env c)) v vs
constrain env vs (Sel w (TVar _ v) n t) = Map.adjust (filter (\c -> n `elem` attrs env c || n `elem` protoattrs env c)) v vs
constrain env vs (Seal w (TVar _ v) (TVar _ v') _ _)
  | univar v && univar v'               = vs
constrain env vs (Seal w (TVar _ v) t _ _)
  | t == fxAction                       = Map.adjust (intersect $ allBelow env t) v vs
  | otherwise                           = Map.adjust (intersect $ allBelow env t) v vs
constrain env vs (Seal w t (TVar _ v) _ _)
  | t == fxAction || w == Nothing       = Map.adjust (intersect $ allAbove env t) v vs
  | otherwise                           = Map.adjust (intersect $ allAbove env t ++ [CAction]) v vs
constrain env vs _                      = vs


candidates env KType                    = map CProto (allProtos env) ++ [CNone,COpt,CFun,CTuple] ++ 
                                          map CCon (allCons env) ++ map CVar (allVars env KType) ++ map CUnion (uniAbove [])
candidates env KFX                      = [CPure,CMut,CAct,CAction] ++ map CVar (allVars env KFX)
candidates env k                        = map CVar (allVars env k)

--
-- Solve all constraints on vs using a closed world assumption
--

solve                                   :: (Polarity a, Pretty a) => Env -> TEnv -> a -> Equations -> [TVar] -> Constraints -> TypeM (Constraints,Equations)
solve env te tt eq [] cs                = return (cs, eq)
solve env te tt eq vs cs                = do traceM ("###trying collapse " ++ prstrs vs1 ++ " (embedded: " ++ prstrs (vs `intersect` embedded) ++ ")")
                                             (cs,eq) <- collapse env eq vs1 cs
                                             vs2 <- (nub . filter univar . tyfree) <$> msubst (map tVar vs1)
                                             solve' env te tt eq vs2 (reverse cs)
  where vs0                             = vs \\ embedded
        vs1 | null vs0                  = vs
            | otherwise                 = vs0
        embedded                        = concat $ [ emb c | c <- cs, headvar c `elem` vs ]
        emb (Cast TVar{} TVar{})        = []
        emb (Cast TVar{} t)             = tyfree t
        emb (Cast t TVar{})             = tyfree t
        emb (Sub _ TVar{} TVar{})       = []
        emb (Sub _ TVar{} t)            = tyfree t
        emb (Sub _ t TVar{})            = tyfree t
        emb (Impl _ t p)                = tyfree p
        emb (Seal _ fx1 fx2 t1 t2)      = emb (Cast fx1 fx2) ++ emb (Cast t1 t2)
        emb (Sel _ TVar{} n t)          = tyfree t
        emb (Mut TVar{} n t)            = tyfree t
        emb _                           = []
        

solve' env te tt eq vs cs               = do traceM ("###solving: " ++ prstrs vs)
                                             sequence [ unify env (tVar v) =<< instwild env (tvkind v) t | (v, Right t) <- solved ]
                                             cs' <- sequence [ Impl <$> newWitness <*> pure (tVar v) <*> instwildcon env p | (v, Left p) <- solved ]
                                             env <- msubst env
                                             te <- msubst te
                                             simplify' env te tt eq (cs'++cs)
  where tvmap0                          = Map.fromList [ (v, candidates env (tvkind v)) | v <- vs ]
        tvmap1                          = foldl (constrain env) tvmap0 cs
        solved                          = [ (v, solution v) | v <- vs ]
        solution v                      = case lookup' v tvmap1 of
                                            [] -> err1 v ("Cannot solve " ++ prstrs cs ++ " for variable")
                                            c:cs -> trace ("#### Candidates for " ++ prstr v ++ ": " ++ prstrs (c:cs)) $ mkres c
        mkres (CProto n)                = Left $ mkcon n
        mkres (CCon n)                  = Right $ tCon $ mkcon n
        mkres (CVar v)                  = Right $ tVar v
        mkres CNone                     = Right $ tNone
        mkres COpt                      = Right $ tOpt tWild
        mkres (CUnion ns)               = Right $ tUnion (map UCon ns)
        mkres CFun                      = Right $ tFun tWild tWild tWild tWild
        mkres CTuple                    = Right $ tTuple tWild tWild
        mkres CPure                     = Right $ fxPure
        mkres CMut                      = Right $ fxMut tWild
        mkres CAct                      = Right $ fxAct tWild
        mkres CAction                   = Right $ fxAction
        mkcon n                         = case tconKind n env of
                                            KFun ks _ -> TC n (replicate (length ks) tWild)
                                            _         -> TC n []

collapse                                :: Env -> Equations -> [TVar] -> Constraints -> TypeM (Constraints, Equations)
collapse env eq vs cs                   = col [] eq cs
  where col cs eq []                    = do cs <- msubst cs
                                             return (cs,eq)
        col cs eq (Cast t1@TVar{} t2@TVar{} : cs')
          | tvar t1 `elem` vs,
            tvar t2 `elem` vs           = unify env t1 t2 >> msubst cs' >>= col cs eq
        col cs eq (Sub w t1@TVar{} t2@TVar{} : cs')
          | tvar t1 `elem` vs,
            tvar t2 `elem` vs           = unify env t1 t2 >> msubst cs' >>= col cs (idwit w t1 t2 : eq)
        col cs eq (Seal w fx1@TVar{} fx2@TVar{} t1 t2 : cs')
          | tvar fx1 `elem` vs,
            tvar fx2 `elem` vs          = trace ("### collapse " ++ prstr w) $ unify env fx1 fx2 >> msubst cs' >>= col (Cast t1 t2 : cs) eq'
          where eq' | Just w' <- w      = (w', wFun t t2, e) : eq
                    | otherwise         = eq
                t                       = tFun fx1 posNil kwdNil t1
                e                       = Lambda NoLoc (pospar [(px0,t)]) KwdNIL (eCall (eVar px0) []) fx2
        col cs eq (c : cs')             = col (c : cs) eq cs'


implAll env [] t                        = True
implAll env ps (TCon _ c)               = and [ hasWitness env (tcname c) (tcname p) | (w,p) <- ps ]
implAll env ps (TOpt _ _)               = all (\(_,p) -> any (qmatch env $ tcname p) [qnIdentity,qnEq]) ps
implAll env ps TUnion{}                 = all (qmatch env qnEq . tcname . snd) ps
implAll env ps t                        = False


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
replace ub lb c@(Seal _ TVar{} TVar{} _ _)
                                        = c
replace ub lb (Seal w (TVar _ v) fx t t')
  | Just fx' <- lookup v ub             = Seal w fx' fx t t'
replace ub lb (Seal w fx (TVar _ v) t t')
  | Just fx' <- lookup v lb             = Seal w fx fx' t t'
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
  where findDefault (w,p)               = lookup' (unalias env $ tcname p) defaultmap

defaultmap                              = Map.fromList [ 
                                            (qnSequence, [tList tWild]),
                                            (qnMapping, [tDict tWild tWild]),
                                            (qnSetP, [tSet tWild]),
                                            (qnIndexed, [tList tWild, tStr, tDict tWild tWild]),
                                            (qnSliceable, [tList tWild, tStr]),
                                            (qnPlus, [tInt, tFloat, tStr, tList tWild]),
                                            (qnMinus, [tInt, tFloat, tSet tWild]),
                                            (qnNumber, [tInt, tFloat]),
                                            (qnReal, [tFloat, tInt]),
                                            (qnRational, [tFloat]),
                                            (qnIntegral, [tInt]),
                                            (qnLogical, [tInt, tSet tWild]) ]


----------------------------------------------------------------------------------------------------------------------
-- Misc.
----------------------------------------------------------------------------------------------------------------------

px0:px1:px2:_                           = xNames

app tx e []                             = e
app tx e es                             = Lambda NoLoc p' k' (Call NoLoc e (exp2arg es (pArg p')) (kArg k')) fx
  where TFun _ fx p k _                 = tx                    -- If it takes arguments, it must be a function!
        (p',k')                         = (pPar pNames p, kPar kNames k)

app2nd Static tx e es                   = app tx e es
app2nd _ tx e []                        = e
app2nd _ tx e es                        = Lambda NoLoc p' k' (Call NoLoc e (PosArg pSelf (exp2arg es pArgs)) (kArg k')) fx
  where TFun _ fx p k _                 = tx                    -- If it takes arguments, it must be a function!
        (p',k')                         = (pPar pNames p, kPar kNames k)
        PosArg pSelf pArgs              = pArg p'                    

idwit w t1 t2                           = (w, wFun t1 t2, eLambda [(px0,t1)] (eVar px0))

rowFun PRow r1 r2                       = tFun fxPure (posRow (tTupleP r1) posNil) kwdNil (tTupleP r2)
rowFun KRow r1 r2                       = tFun fxPure (posRow (tTupleK r1) posNil) kwdNil (tTupleK r2)

rowWit PRow w n t r wt wr               = eLambda [(px0,posRow t r)] eTup
  where eTup                            = Paren l0 $ Tuple l0 (PosArg e1 (PosStar e2)) KwdNil
        e1                              = eCall (eVar wt) [DotI l0 (eVar px0) 0]
        e2                              = eCall (eVar wr) [RestI l0 (eVar px0) 0]
rowWit KRow w n t r wt wr               = eLambda [(px0,kwdRow n t r)] eTup
  where eTup                            = Paren l0 $ Tuple l0 PosNil (KwdArg n e1 (KwdStar e2))
        e1                              = eCall (eVar wt) [Dot l0 (eVar px0) n]
        e2                              = eCall (eVar wr) [Rest l0 (eVar px0) n]

{-
rowWit PRow w n t r wt wr               = Lambda l0 (PosPar px1 (Just t) Nothing $ PosSTAR px2 (Just $ tTupleP r)) KwdNIL eTup fxPure
  where eTup                            = Paren l0 $ Tuple l0 (PosArg e1 (PosStar (Call l0 (eVar wr) (PosStar $ eVar px2) KwdNil))) KwdNil
        e1                              = eCall (eVar wt) [eVar px1]
rowWit KRow w n t r wt wr               = Lambda l0 PosNIL (KwdPar n (Just t) Nothing $ KwdSTAR px2 (Just $ tTupleK r)) eRec fxPure
  where eRec                            = Paren l0 $ Tuple l0 PosNil (KwdArg n e1 (KwdStar (Call l0 (eVar wr) PosNil (KwdStar $ eVar px2))))
        e1                              = eCall (eVar wt) [eVar n]
-}

wFun t1 t2                              = tFun fxPure (posRow t1 posNil) kwdNil t2

lambda0 fx e                            = Lambda NoLoc PosNIL KwdNIL e fx
