{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Acton.Solver where

import Control.Monad
import qualified Data.Map.Strict as Map

import Utils
import Acton.Syntax
import Acton.Builtin
import Acton.Names
import Acton.TypeM
import Acton.Env




-- Reduce conservatively and remove entailed constraints
simplify                                    :: Env -> [Constraint] -> TypeM [Constraint]
simplify env []                             = do cs0 <- collectDeferred
                                                 cs1 <- msubst cs0
                                                 if simple cs1
                                                     then return cs1
                                                     else simplify env cs1
  where simple cs                           = True                              -- TODO: add proper test
simplify env (c:cs)                         = do c' <- msubst c
                                                 reduce env c'
                                                 simplify env cs

-- Reduce aggressively or fail
solve                                       :: Env -> [Constraint] -> TypeM ()
solve env []                                = do cs0 <- collectDeferred
                                                 cs1 <- msubst cs0
                                                 if cs1 == [] 
                                                     then return ()
                                                     else solve env cs1         -- TODO: termination...!
solve env (c:cs)                            = do c' <- msubst c
                                                 reduce env c'
                                                 solve env cs

reduce env (Sub t1 t2)                      = red' True env t1 t2
reduce env (Equ t1 t2)                      = red' False env t1 t2
reduce env (SubGen t1 t2)                   = redGen True env t1 t2
reduce env (EquGen t1 t2)                   = redGen False env t1 t2

reduce env (Impl t@(TVar _ tv) u)
  | not $ skolem tv                         = defer [Impl t u]
reduce env (Impl t u)
  | entail env (Impl t u)                   = return ()

reduce env (Sel t1@(TVar _ tv) n t2)
  | not $ skolem tv                         = defer [Sel t1 n t2]
reduce env (Sel (TCon _ tc) n t2)           = return ()

reduce env (Mut t1@(TVar _ tv) n t2)
  | not $ skolem tv                         = defer [Mut t1 n t2]
reduce env (Mut (TCon _ tc) n t2)           = return ()
reduce env c                                = noRed c

red sub env t1 t2                           = do -- traceM ("red sub env " ++ prstr (Sub t1 t2))
                                                 red' sub env t1 t2

red' sub env (TVar _ tv1) (TVar _ tv2)
  | tv1 == tv2                              = return ()

red' True env t1@(TVar _ tv) t2
  | not $ skolem tv                         = defer [Sub t1 t2]
red' True env t1 t2@(TVar _ tv)
  | not $ skolem tv                         = defer [Sub t1 t2]

red' True env t1@TVar{} t2
  | entail env (Sub t1 t2)                  = return ()
red' True env t1 t2@TVar{}
  | entail env (Sub t1 t2)                  = return ()

red' False env (TVar _ tv) t2
  | not $ skolem tv                         = do s <- getSubstitution
                                                 case Map.lookup tv s of
                                                   Just t1 -> red' False env t1 t2
                                                   Nothing -> do t2' <- msubst t2
                                                                 when (tv `elem` tyfree t2') (infiniteType tv)
                                                                 substitute tv t2'
red' False env t1 (TVar _ tv)
  | not $ skolem tv                         = do s <- getSubstitution
                                                 case Map.lookup tv s of
                                                   Just t2 -> red' False env t1 t2
                                                   Nothing -> do t1' <- msubst t1
                                                                 when (tv `elem` tyfree t1') (infiniteType tv)
                                                                 substitute tv t1'

red' sub env (TCon l1 c1) (TCon l2 c2)
  | tcname c1 == tcname c2                  = mapM_ (uncurry $ red False env) (tcargs c1 `zip` tcargs c2)       -- TODO: use polarities
  | otherwise                               = do (t1,t2) <- instSubAxiom env (tcname c1) (tcname c2)
                                                 red sub env (TCon l1 c1) t1
                                                 red sub env t2 (TCon l2 c2)
--           as declared           as called
red' sub env (TFun _ fx1 p1 r1 t1) (TFun _ fx2 p2 r2 t2)
                                            = do red sub env fx1 fx2
                                                 red sub env p2 p1            -- TODO: implement pos/kwd argument shifting
                                                 red sub env r2 r1
                                                 red sub env t1 t2

red' sub env (TRecord _ r1) (TRecord _ r2)  = red sub env r1 r2

red' sub env (TTuple _ p1) (TTuple _ p2)    = red sub env p1 p2

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

red' sub env (TNil _) (TNil _)              = return ()
red' sub env (TRow _ n t1 r1) r2            = do (t2,r2') <- findKwd tNil n r2 (rowTail r1)
                                                 redGen sub env t1 t2
                                                 red sub env r1 r2
  where findKwd r0 n r tl                   = do r0' <- msubst r0
                                                 r' <- msubst r
                                                 tl' <- msubst tl
                                                 findKwd' r0' n r' tl'
        findKwd' r0 n (TRow l n1 t r2) tl
          | n == n1                         = return (t, revApp r0 r2)
          | otherwise                       = findKwd' (TRow l n1 t r0) n r2 tl
        findKwd' r0 n (TNil _) tl           = kwdNotFound n
        findKwd' r0 n r2@(TVar _ tv) tl
          | r2 == tl                        = conflictingRow tv
          | otherwise                       = do t <- monotype <$> newTVar
                                                 r <- newTVar
                                                 substitute tv (kwdRow n t r)
                                                 return (t, revApp r0 r)
        revApp (TRow l n t r1) r2           = revApp r1 (TRow l n t r2)
        revApp (TNil _) r2                  = r2
red' False env t1 t2                        = noRed (Equ t1 t2)
red' True env t1 t2                         = noRed (Sub t1 t2)


redGen sub env (TSchema _ [] t1 d1) (TSchema _ [] t2 d2)
  | d1 /= d2                                = distinctDecorations d1 d2
  | otherwise                               = red sub env t1 t2
redGen sub env sc1 sc2@(TSchema _ q2 t2 d2)
  | scdec sc1 /= d2                         = distinctDecorations (scdec sc1) d2
  | otherwise                               = do t1 <- instantiate env1 sc1
                                                 red sub env1 t1 t2
                                                 tvs <- (intersect (tybound q2) . tyfree) <$> msubst [sc1,sc2]
                                                 when (any (`elem` tvs) (tybound q2)) (escapingVar tvs sc1 sc2)
  where env1                                = defineTVars q2 env


