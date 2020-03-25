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
simplify                                    :: Env -> [Constraint] -> TypeM [Constraint]
simplify env cs                             = do --traceM ("### simplify: " ++ prstrs cs)
                                                 reduceAll env cs
                                                 cs0 <- collectDeferred
                                                 cs1 <- msubst cs0
                                                 if simple cs1
                                                     then return [] -- cs1
                                                     else simplify env cs1
  where simple cs                           = True                              -- TODO: add proper test

-- Reduce aggressively or fail
solve                                       :: Env -> [Constraint] -> TypeM ()
solve env cs                                = do --traceM ("### solve: " ++ prstrs cs)
                                                 reduceAll env cs
                                                 cs0 <- collectDeferred
                                                 cs1 <- msubst cs0
                                                 if done cs1
                                                     then return ()
                                                     else solve env cs1
  where done cs                             = True                              -- TODO: ensure proper termination...!

reduceAll                                   :: Env -> [Constraint] -> TypeM ()
reduceAll env cs                            = mapM_ (reduce env) cs

reduce                                      :: Env -> Constraint -> TypeM ()
reduce env c                                = do c' <- msubst c
                                                 --traceM ("### reduce: " ++ prstr c')
                                                 reduce' env c'

reduce'                                     :: Env -> Constraint -> TypeM ()
reduce' env (Sub w t1 t2)                   = sub' env w t1 t2
--reduce' (Qual w tvs cs1 cs2)                  = ...

reduce' env c@(Impl w (TVar _ tv) u)
  | not $ skolem tv                         = defer [c]
reduce' env c@(Impl w t u)
  | otherwise                   = return ()                                      -- TODO: implement, of course

reduce' env c@(Sel (TVar _ tv) n t2)
  | not $ skolem tv                         = defer [c]
  | Just u <- findSubBound tv env           = reduce' env (Sel (tCon u) n t2)
reduce' env (Sel t1@(TCon _ tc) n t2)       = do let (sc,dec) = findAttr env tc n
                                                 when (dec == StaticMethod) (noSelStatic n tc)
                                                 (cs,t) <- instantiate env sc
                                                 let t' = subst [(tvSelf,t1)] t
                                                 reduceAll env (Sub Nothing t' t2 : cs)
reduce' env (Sel (TTuple _ p r) n t2)       = reduce env (Sub Nothing r (kwdRow n (monotype t2) tWild))
{-
reduce' env (Sel (TExist _ p) n t2)         = do let (sc,dec) = findAttr env tc n
                                                 when (isInstAttr dec) (noSelInstByClass n tc)
                                                 (cs,t) <- instantiate env (addself sc dec)
                                                 let t' = subst [(tvSelf,tCon tc)] t
                                                 reduceAll env (Sub Nothing t' t2 : cs)
  where
    addself (TSchema l q t) (ClassAttr _)   = TSchema l q (addself' t)
    addself sc _                            = sc
    addself' (TFun l fx p r t)              = TFun l fx (posRow (monotype tSelf) p) r t
    addself' t                              = TFun (loc t) fxNil (posRow (monotype tSelf) posNil) kwdNil t
-}
reduce' env (Sel (TUnion _ [ULit _]) n t2)  = reduce' env (Sel tStr n t2)

reduce' env c@(Mut (TVar _ tv) n t2)
  | not $ skolem tv                         = defer [c]
  | Just u <- findSubBound tv env           = reduce' env (Mut (tCon u) n t2)
reduce' env (Mut t1@(TCon _ tc) n t2)       = do let (sc,dec) = findAttr env tc n
                                                 when (not $ isInstAttr dec) (noMutClass n)
                                                 (cs,t) <- instantiate env sc
                                                 let t' = subst [(tvSelf,t1)] t
                                                 w <- newWitness
                                                 reduceAll env (Sub (Just w) t1 tObject : Sub Nothing t' t2 : cs)
reduce' env c                               = noRed c



sub                                         :: Env -> Maybe Name -> Type -> Type ->TypeM ()
sub env w t1 t2                             = do t1' <- msubst t1
                                                 t2' <- msubst t2
                                                 sub' env w t1' t2'

sub'                                        :: Env -> Maybe Name -> Type -> Type ->TypeM ()
sub' env w (TVar _ tv1) (TVar _ tv2)
  | tv1 == tv2                              = return ()

sub' env (Just w) t1@(TVar _ tv) t2
  | not $ skolem tv                         = defer [Sub (Just w) t1 t2]
-- if skolem...

sub' env (Just w) t1 t2@(TVar _ tv)
  | not $ skolem tv                         = defer [Sub (Just w) t1 t2]
-- if skolem...

sub' env Nothing (TVar _ tv) t2
  | not $ skolem tv                         = do when (tv `elem` tyfree t2) (infiniteType tv)
                                                 substitute tv t2
sub' env Nothing t1 (TVar _ tv)
  | not $ skolem tv                         = do when (tv `elem` tyfree t1) (infiniteType tv)
                                                 substitute tv t1

sub' env w (TCon _ c1) (TCon l c2)
  | tcname c1 == tcname c2                  = mapM_ (uncurry $ sub env Nothing) (tcargs c1 `zip` tcargs c2)     -- TODO: use polarities
  | Just (_,t) <- findSubAxiom env c1 n2    = sub env w t (TCon l c2)                                           -- TODO: handle proto
  | otherwise                               = err1 n2 ("Not related: " ++ prstr c1 ++ " and")
  where n2                                  = tcname c2


sub' env w (TExist _ p1) (TExist l p2)
  | tcname p1 == tcname p2                  = mapM_ (uncurry $ sub env Nothing) (tcargs p1 `zip` tcargs p2)     -- TODO: use polarities

--           as declared           as called
sub' env w (TFun _ fx1 p1 k1 t1) (TFun _ fx2 p2 k2 t2)
                                            = do sub env w fx1 fx2
                                                 sub env w p2 p1            -- TODO: implement pos/kwd argument shifting
                                                 sub env w k2 k1
                                                 sub env w t1 t2

sub' env w (TTuple _ p1 k1) (TTuple _ p2 k2)
                                            = do sub env w p1 p2
                                                 sub env w k1 k2

sub' env w (TUnion _ u1) (TUnion _ u2)
  | all (uniElem u2) u1                     = return ()
sub' env (Just w) (TUnion _ u1) t2
  | all (uniLit t2) u1                      = return ()
sub' env (Just w) (TCon _ (TC qn [])) (TUnion _ u2)
  | uniCon u2 qn                            = return ()

sub' env w (TOpt _ t1) (TOpt _ t2)          = sub env w t1 t2
sub' env (Just w) (TNone _) (TOpt _ t)      = return ()
sub' env (Just w) t1 (TOpt _ t2)            = sub env (Just w) t1 t2
sub' env w (TNone _) (TNone _)              = return ()

sub' env w (TWild _) t2                     = return ()
sub' env w t1 (TWild _)                     = return ()

sub' env w (TNil _ k1) (TNil _ k2)
  | k1 == k2                                = return ()
sub' env w (TRow _ k n t1 r1) r2            = do (t2,r2') <- findElem (tNil k) n r2 (rowTail r1)
                                                 cs <- matchSchema env w t1 t2
                                                 reduceAll env cs
                                                 sub env w r1 r2'
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
sub' env w t1 t2                            = noRed (Sub w t1 t2)


matchSchema env w (TSchema _ [] t) (TSchema _ [] u)
                                            = return [Sub w t u]
matchSchema env w sc1 sc2                   = do (cs,t) <- instantiate env sc1
                                                 matchInst env w cs t sc2


matchInst env w cs t sc@(TSchema _ q u)     = do cs' <- simplify env1 (Sub w t u : cs)
                                                 fvs <- msubstTV (tyfree sc ++ tyfree env)
                                                 let esc = intersect (tybound q) fvs
                                                 when (not $ null esc) (escapingVar esc sc)
                                                 case partition (canWait fvs) cs' of
                                                     (cs1,[]) -> return cs1
                                                     (cs1,cs2) -> do w' <- newWitness
                                                                     c <- Qual w' <$> msubst q <*> pure cs2
                                                                     return (c:cs1)
  where env1                                = defineTVars q env
        canWait fvs c                       = all (`elem` fvs) (tyfree c)


monotypeOf (TSchema _ [] t)                 = t
monotypeOf sc                               = err1 sc "Monomorphic type expected"


