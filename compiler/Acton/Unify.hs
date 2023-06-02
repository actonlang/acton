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
  | tcname c1 == tcname c2                  = matches vs (tcargs c1) (tcargs c2)
match vs (TFun _ fx1 p1 k1 t1) (TFun _ fx2 p2 k2 t2)
                                            = matches vs [fx1,p1,k1,t1] [fx2,p2,k2,t2]
match vs (TTuple _ p1 k1) (TTuple _ p2 k2)
                                            = matches vs [p1,k1] [p2,k2]
match vs (TOpt _ t1) (TOpt _ t2)            = match vs t1 t2
match vs (TNone _) (TNone _)                = Just []
match vs (TFX _ fx1) (TFX _ fx2)
  | fx1 == fx2                              = Just []

match vs (TNil _ k1) (TNil _ k2)
  | k1 == k2                                = Just []
match vs r1 (TRow _ k n2 t2 r2)
  | Just (t1,r1') <- findElem r1            = matches vs [t1,r1'] [t2,r2]
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

matches vs [] []                            = Just []
matches vs (t:ts) (t':ts')                  = do s1 <- match vs t t'
                                                 s2 <- matches vs ts ts'
                                                 merge s1 s2

merge s1 s2
  | agree                                   = Just $ s1 ++ s2
  | otherwise                               = Nothing
  where agree                               = and [ subst s1 (tVar v) `wildeq` subst s2 (tVar v) | v <- dom s1 `intersect` dom s2 ]
        t `wildeq` t'                       = match [] t t' == Just []


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


