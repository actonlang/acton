{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Acton.Solver where

import Utils
import Acton.Syntax
import Acton.Builtin
import Acton.Names
import Acton.TypeM
import Acton.Env




-- Reduce conservatively and remove entailed constraints
simplify                                :: Env -> [Constraint] -> TypeM [Constraint]
simplify env []                         = do cs0 <- collectDeferred
                                             cs1 <- msubst cs0
                                             if simple cs1
                                                 then return cs1
                                                 else simplify env cs1
  where simple cs                       = True                              -- TODO: add proper test
simplify env (c:cs)                     = do c' <- msubst c
                                             red1 env c'
                                             simplify env cs

-- Reduce aggressively or fail
solve                                   :: Env -> [Constraint] -> TypeM ()
solve env []                            = do cs0 <- collectDeferred
                                             cs1 <- msubst cs0
                                             if cs1 == [] 
                                                 then return ()
                                                 else solve env cs1         -- TODO: termination...!
solve env (c:cs)                        = do c' <- msubst c
                                             red1 env c'
                                             solve env cs

red1 env (Sub t1 t2)                    = sub' env t1 t2
red1 env (Equ t1 t2)                    = sub' env t1 t2
red1 env _                              = return ()                         -- TODO: implement!


sub env t1 t2                               = do -- traceM ("sub env " ++ prstr (Sub t1 t2))
                                                 sub' env t1 t2
sub' env (TVar _ tv1) (TVar _ tv2)
  | tv1 == tv2                              = return ()
{-
sub' env (TVar _ tv) t2                     = do s <- getSubstitution
                                                 case Map.lookup tv s of
                                                   Just t1 -> sub' env t1 t2
                                                   Nothing -> do t2' <- msubst t2
                                                                 when (tv `elem` tyfree t2') (infiniteType tv)
                                                                 substitute tv t2'
sub' env t1 (TVar _ tv)                     = do s <- getSubstitution
                                                 case Map.lookup tv s of
                                                   Just t2 -> sub' env t1 t2
                                                   Nothing -> do t1' <- msubst t1
                                                                 when (tv `elem` tyfree t1') (infiniteType tv)
                                                                 substitute tv t1'
-}
--       as declared           as called
sub' env (TFun _ fx1 p1 r1 t1) (TFun _ fx2 p2 r2 t2)
                                            = do sub env fx1 fx2
                                                 sub env p2 p1            -- TODO: implement pos/kwd argument shifting
                                                 sub env r2 r1
                                                 sub env t1 t2
sub' env (TRecord _ r1) (TRecord _ r2)      = sub env r1 r2
sub' env (TTuple _ p1) (TTuple _ p2)        = sub env p1 p2
sub' env (TUnion _ u1) t2
  | all (uniLit t2) u1                      = return ()
sub' env (TUnion _ u1) (TUnion _ u2)
  | all (uniElem u2) u1                     = return ()
sub' env (TCon _ (TC qn [])) (TUnion _ u2)
  | uniCon u2 qn                            = return ()
sub' env (TNone _) (TNone _)                = return ()
sub' env (TNone _) (TOpt _ t)               = return ()
sub' env t1@TVar{} t2@TOpt{}                = defer [Sub t1 t2]
sub' env (TOpt _ t1) (TOpt _ t2)            = sub env t1 t2
sub' env t1 (TOpt _ t2)                     = sub env t1 t2
sub' env (TSelf _) (TSelf _)                = return ()
sub' env (TWild _) t2                       = return ()
sub' env t1 (TWild _)                       = return ()
sub' env (TNil _) (TNil _)                  = return ()
sub' env (TRow _ n t1 r1) r2                = do (t2,r2') <- findKwd tNil n r2 (rowTail r1)
                                                 subGen env t1 t2
                                                 sub env r1 r2
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
sub' env t1 t2                              = noSub (erase t1) (erase t2)


subGen env (TSchema _ [] t1 d1) (TSchema _ [] t2 d2)
  | d1 == d2                                = sub env t1 t2
-- TODO: implement the rest


