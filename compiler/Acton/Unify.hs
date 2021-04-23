{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Acton.Unify where

import Control.Monad

import Utils
import Pretty
import Acton.Syntax
import Acton.Subst
import Acton.TypeM


-- unification ----------------------------------------------------------------------------------------------------------------------

unify                                       :: Type -> Type -> TypeM ()
unify t1 t2                                 = do t1' <- msubst t1
                                                 t2' <- msubst t2
                                                 --traceM ("  #unify " ++ prstr t1' ++ " and " ++ prstr t2')
                                                 unify' t1' t2'

unifyM ts1 ts2                              = mapM_ (uncurry unify) (ts1 `zip` ts2)


unify' (TWild _) t2                         = return ()
unify' t1 (TWild _)                         = return ()

unify' (TCon _ c1) (TCon _ c2)
  | tcname c1 == tcname c2                  = unifyM (tcargs c1) (tcargs c2)

unify' (TFun _ fx1 p1 k1 t1) (TFun _ fx2 p2 k2 t2)
                                            = do unify fx1 fx2
                                                 unify p2 p1
                                                 unify k2 k1
                                                 unify t1 t2

unify' (TTuple _ p1 k1) (TTuple _ p2 k2)
                                            = do unify p1 p2
                                                 unify k1 k2

unify' (TOpt _ t1) (TOpt _ t2)              = unify t1 t2
unify' (TNone _) (TNone _)                  = return ()

unify' (TFX _ fx1) (TFX _ fx2)
  | fx1 == fx2                              = return ()

unify' (TNil _ k1) (TNil _ k2)
  | k1 == k2                                = return ()
unify' r1 (TRow _ k n t2 r2)                = do (t1,r1') <- findElem k (tNil k) n r1 (rowTail r2)
                                                 unify t1 t2
                                                 unify r1' r2

unify' (TVar _ tv1) (TVar _ tv2)
  | tv1 == tv2                              = return ()

unify' (TVar _ tv) t2
  | univar tv                               = do when (tv `elem` tyfree t2) (infiniteType tv)
                                                 substitute tv t2
unify' t1 (TVar _ tv)
  | univar tv                               = do when (tv `elem` tyfree t1) (infiniteType tv)
                                                 substitute tv t1

unify' t1 t2                                = noUnify t1 t2



-- matching ----------------------------------------------------------------------------------------------------------------------

match vs (TWild _) t                        = Just []
match vs t (TWild _)                        = Just []
match vs (TCon _ c1) (TCon _ c2)
  | tcname c1 == tcname c2                  = match' (tcargs c1) (tcargs c2)
  where match' (t1:ts1) (t2:ts2)            = do s1 <- match vs t1 t2
                                                 s2 <- match' ts1 ts2
                                                 merge s1 s2
        match' [] []                        = Just []
match vs (TFun _ fx1 p1 k1 t1) (TFun _ fx2 p2 k2 t2)
                                            = do s1 <- match vs fx1 fx2
                                                 s2 <- match vs p1 p2
                                                 s3 <- match vs k1 k2
                                                 s4 <- match vs t1 t2
                                                 s <- merge s1 s2
                                                 s' <- merge s3 s4
                                                 merge s s'
match vs (TTuple _ p1 k1) (TTuple _ p2 k2)
                                            = do s1 <- match vs p1 p2
                                                 s2 <- match vs k1 k2
                                                 merge s1 s2
match vs (TOpt _ t1) (TOpt _ t2)            = match vs t1 t2
match vs (TNone _) (TNone _)                = Just []
match vs (TFX _ fx1) (TFX _ fx2)
  | fx1 == fx2                              = Just []

match vs (TNil _ k1) (TNil _ k2)
  | k1 == k2                                = Just []
match vs r1 (TRow _ k n2 t2 r2)
  | Just (t1,r1') <- findElem r1            = do s1 <- match vs t1 t2
                                                 s2 <- match vs r1' r2
                                                 merge s1 s2
  where findElem (TRow l k n1 t1 r1)
          | n1 == n2                        = Just (t1, r1)
          | otherwise                       = do (t1',r1') <- findElem r1
                                                 Just (t1', TRow l k n1 t1 r1')
        findElem r1                         = Nothing
match vs (TVar _ tv1) (TVar _ tv2)
  | tv1 == tv2                              = Just []
match vs t1 (TVar _ tv)
  | tv `elem` vs && tv `notElem` tyfree t1  = Just [(tv, t1)]
match vs t1 t2                              = Nothing

merge s1 s2
  | agree                                   = Just $ s1 ++ s2
  | otherwise                               = Nothing
  where agree                               = and [ subst s1 (tVar v) == subst s2 (tVar v) | v <- dom s1 `intersect` dom s2 ]


-- findElem ------------------------------------------------------------------------------------------------------------------------

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


