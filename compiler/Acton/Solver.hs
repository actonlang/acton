{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Acton.Solver where

import Control.Monad
import qualified Data.Map.Strict as Map

import Utils
import Acton.Syntax
import Acton.Builtin
import Acton.Names
import Acton.Env




-- Reduce conservatively and remove entailed constraints
simplify                                    :: [Constraint] -> TypeM [Constraint]
simplify cs                                 = do --traceM ("### simplify: " ++ prstrs cs)
                                                 reduceAll cs
                                                 cs0 <- collectDeferred
                                                 cs1 <- msubst cs0
                                                 if simple cs1
                                                     then return [] -- cs1
                                                     else simplify cs1
  where simple cs                           = True                              -- TODO: add proper test

-- Reduce aggressively or fail
solve                                       :: [Constraint] -> TypeM ()
solve cs                                    = do --traceM ("### solve: " ++ prstrs cs)
                                                 reduceAll cs
                                                 cs0 <- collectDeferred
                                                 cs1 <- msubst cs0
                                                 if done cs1
                                                     then return ()
                                                     else solve cs1
  where done cs                             = True                              -- TODO: ensure proper termination...!

reduceAll cs                                = mapM_ reduce cs

reduce c                                    = do c' <- msubst c
                                                 --traceM ("### reduce: " ++ prstr c')
                                                 reduce' c'

reduce' (Sub env t1 t2)                     = red' True env t1 t2
reduce' (Equ env t1 t2)                     = red' False env t1 t2
reduce' (SubGen env t1 t2)                  = redGen' True env t1 t2
reduce' (EquGen env t1 t2)                  = redGen' False env t1 t2

reduce' c@(Impl w env (TVar _ tv) u)
  | not $ skolem tv                         = defer [c]
reduce' c@(Impl w env t u)
  | otherwise                   = return ()

reduce' c@(Sel env (TVar _ tv) n t2)
  | not $ skolem tv                         = defer [c]
  | Just u <- findSubBound tv env           = reduce' (Sel env (tCon u) n t2)
reduce' (Sel env t1@(TCon _ tc) n t2)       = do let sc = findAttr env tc n
                                                 when (scdec sc == StaticMethod) (noSelStatic n tc)
                                                 (cs,t) <- instantiate env sc
                                                 let t' = subst [(tvSelf,t1)] t
                                                 reduceAll (Equ env t' t2 : cs)
reduce' (Sel env (TTuple _ p r) n t2)       = reduce (Equ env r (kwdRow n (monotype t2) tWild))
{-
reduce' (Sel env (TExist _ p) n t2)         = do let sc = findAttr env tc n
                                                 when (isInstAttr $ scdec sc) (noSelInstByClass n tc)
                                                 (cs,t) <- instantiate env (addself sc)
                                                 let t' = subst [(tvSelf,tCon tc)] t
                                                 reduceAll (Equ env t' t2 : cs)
  where
    addself (TSchema l q t (ClassAttr _))   = TSchema l q (addself' t) StaticMethod
    addself sc                              = sc
    addself' (TFun l fx p r t)              = TFun l fx (posRow (monotype tSelf) p) r t
    addself' t                              = TFun (loc t) fxNil (posRow (monotype tSelf) posNil) kwdNil t
-}
reduce' (Sel env (TUnion _ [ULit _]) n t2)  = reduce' (Sel env tStr n t2)

reduce' c@(Mut env (TVar _ tv) n t2)
  | not $ skolem tv                         = defer [c]
  | Just u <- findSubBound tv env           = reduce' (Mut env (tCon u) n t2)
reduce' (Mut env t1@(TCon _ tc) n t2)       = do let sc = findAttr env tc n
                                                 when (not $ isInstAttr $ scdec sc) (noMutClass n)
                                                 (cs,t) <- instantiate env sc
                                                 let t' = subst [(tvSelf,t1)] t
                                                 reduceAll (Sub env t1 tObject : Equ env t' t2 : cs)
reduce' c                                   = noRed c



red sub env t1 t2                           = do t1' <- msubst t1
                                                 t2' <- msubst t2
                                                 --traceM ("### red: " ++ prstr (if sub then Sub t1' t2' else Equ t1' t2'))
                                                 red' sub env t1' t2'

red' sub env (TVar _ tv1) (TVar _ tv2)
  | tv1 == tv2                              = return ()

red' True env t1@(TVar _ tv) t2
  | not $ skolem tv                         = defer [Sub env t1 t2]
-- if skolem...

red' True env t1 t2@(TVar _ tv)
  | not $ skolem tv                         = defer [Sub env t1 t2]
-- if skolem...

red' False env (TVar _ tv) t2
  | not $ skolem tv                         = do when (tv `elem` tyfree t2) (infiniteType tv)
                                                 substitute tv t2
red' False env t1 (TVar _ tv)
  | not $ skolem tv                         = do when (tv `elem` tyfree t1) (infiniteType tv)
                                                 substitute tv t1

red' sub env (TCon _ c1) (TCon l c2)
  | tcname c1 == tcname c2                  = mapM_ (uncurry $ red False env) (tcargs c1 `zip` tcargs c2)       -- TODO: use polarities
  | Just (_,t) <- findSubAxiom env c1 n2    = red sub env t (TCon l c2)                                         -- TODO: handle proto
  | otherwise                               = err1 n2 ("Not related: " ++ prstr c1 ++ " and")
  where n2                                  = tcname c2


red' sub env (TExist _ p1) (TExist l p2)
  | tcname p1 == tcname p2                  = mapM_ (uncurry $ red False env) (tcargs p1 `zip` tcargs p2)       -- TODO: use polarities

--           as declared           as called
red' sub env (TFun _ fx1 p1 k1 t1) (TFun _ fx2 p2 k2 t2)
                                            = do red sub env fx1 fx2
                                                 red sub env p2 p1            -- TODO: implement pos/kwd argument shifting
                                                 red sub env k2 k1
                                                 red sub env t1 t2

red' sub env (TTuple _ p1 k1) (TTuple _ p2 k2)
                                            = do red sub env p1 p2
                                                 red sub env k1 k2

red' sub env (TUnion _ u1) (TUnion _ u2)
  | all (uniElem u2) u1                     = return ()
red' True env (TUnion _ u1) t2
  | all (uniLit t2) u1                      = return ()
red' True env (TCon _ (TC qn [])) (TUnion _ u2)
  | uniCon u2 qn                            = return ()

red' sub env (TOpt _ t1) (TOpt _ t2)        = red sub env t1 t2
red' True env (TNone _) (TOpt _ t)          = return ()
red' True env t1 (TOpt _ t2)                = red True env t1 t2
red' sub env (TNone _) (TNone _)            = return ()

red' sub env (TWild _) t2                   = return ()
red' sub env t1 (TWild _)                   = return ()

red' sub env (TNil _ k1) (TNil _ k2)
  | k1 == k2                                = return ()
red' sub env (TRow _ k n t1 r1) r2          = do (t2,r2') <- findElem (tNil k) n r2 (rowTail r1)
                                                 redGen sub env t1 t2
                                                 red sub env r1 r2'
  where findElem r0 n r tl                  = do r0' <- msubst r0
                                                 r' <- msubst r
                                                 tl' <- msubst tl
                                                 findElem' r0' n r' tl'
        findElem' r0 n (TRow l k n1 t r2) tl
          | n == n1                         = return (t, revApp r0 r2)
          | otherwise                       = findElem' (TRow l k n1 t r0) n r2 tl
        findElem' r0 n (TNil _ _) tl        = kwdNotFound n
        findElem' r0 n r2@(TVar _ tv) tl
          | r2 == tl                        = conflictingRow tv
          | otherwise                       = do t <- monotype <$> newTVar
                                                 r <- newTVarOfKind k
                                                 substitute tv (tRow k n t r)
                                                 return (t, revApp r0 r)
        revApp (TRow l k n t r1) r2         = revApp r1 (TRow l k n t r2)
        revApp (TNil _ _) r2                = r2
red' False env t1 t2                        = noRed (Equ env t1 t2)
red' True env t1 t2                         = noRed (Sub env t1 t2)

redGen sub env sc1 sc2                      = do sc1' <- msubst sc1
                                                 sc2' <- msubst sc2
                                                 redGen' sub env sc1' sc2'

redGen' sub env (TSchema _ [] t1 d1) (TSchema _ [] t2 d2)
  | d1 /= d2                                = distinctDecorations d1 d2
  | otherwise                               = red' sub env t1 t2
redGen' sub env sc1 sc2@(TSchema _ q2 t2 d2)
  | scdec sc1 /= d2                         = distinctDecorations (scdec sc1) d2
  | otherwise                               = do (cs,t1) <- instantiate env1 sc1
                                                 red sub env1 t1 t2
                                                 -- all the cs must be true in env + q2
                                                 solve cs
                                                 -- tyvars not free in sc1,sc2 cannot be affected by above reductions
                                                 tvs <- msubstTV $ tyfree [sc1,sc2]
                                                 let esc = intersect (tybound q2) tvs
                                                 when (not $ null esc) (escapingVar esc sc1 sc2)
  where env1                                = defineTVars q2 env


monotypeOf (TSchema _ [] t _)               = t
monotypeOf sc                               = err1 sc "Monomorphic type expected"


