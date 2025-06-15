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
import Control.Monad.Except
import qualified Control.Exception
import Utils
import Pretty
import Acton.Syntax
import Acton.Subst
import Acton.TypeM


-- unification ----------------------------------------------------------------------------------------------------------------------

tryUnify info t1 t2                         = unify info t1 t2 `catchError` \err -> Control.Exception.throw err

unify                                       :: ErrInfo -> Type -> Type -> TypeM ()
unify info t1 t2                            = do t1' <- msubst t1
                                                 t2' <- msubst t2
                                                 --traceM ("  #unify " ++ prstr t1' ++ " and " ++ prstr t2')
                                                 unify' info t1' t2'

unifyM info ts1 ts2                         = mapM_ (uncurry $ unify info) (ts1 `zip` ts2)


unify' _ (TWild _) t2                       = return ()
unify' _ t1 (TWild _)                       = return ()

unify' info (TCon _ c1) (TCon _ c2)
  | tcname c1 == tcname c2                  = unifyM info (tcargs c1) (tcargs c2)

unify' info (TFun _ fx1 p1 k1 t1) (TFun _ fx2 p2 k2 t2)
                                            = do unify info fx1 fx2
                                                 unify info p2 p1
                                                 unify info k2 k1
                                                 unify info t1 t2

unify' info (TTuple _ p1 k1) (TTuple _ p2 k2)
                                            = do unify info p1 p2
                                                 unify info k1 k2

unify' info (TOpt _ t1) (TOpt _ t2)         = unify info t1 t2
unify' _ (TNone _) (TNone _)                = return ()

unify' _ (TFX _ fx1) (TFX _ fx2)
  | fx1 == fx2                              = return ()

unify' _ (TNil _ k1) (TNil _ k2)
  | k1 == k2                                = return ()
unify' info (TRow _ k1 n1 t1 r1) (TRow _ k2 n2 t2 r2)
  | k1 == k2 && n1 == n2                    = do unify info t1 t2
                                                 unify info r1 r2
unify' info (TStar _ k1 r1) (TStar _ k2 r2)
  | k1 == k2                                = unify info r1 r2

unify' info (TVar _ tv1) (TVar _ tv2)
  | tv1 == tv2                              = return ()

unify' info (TVar _ tv) t2
  | univar tv                               = do when (tv `elem` tyfree t2) (infiniteType tv)
                                                 setUni tv t2
unify' info t1 (TVar _ tv)
  | univar tv                               = do when (tv `elem` tyfree t1) (infiniteType tv)
                                                 setUni tv t1

unify' info t1 t2                           = noUnify info t1 t2



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
match vs (TRow _ k1 n1 t1 r1) (TRow _ k2 n2 t2 r2)
  | k1 == k2 && n1 == n2                    = matches vs [t1,r1] [t2,r2]
match vs (TStar _ k1 r1) (TStar _ k2 r2)
  | k1 == k2                                = match vs r1 r2
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
