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

type Equations                              = [(Name, Type, Expr)]

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

reduce' env eq c@(Impl w (TVar _ tv) p)
  | not $ scoped tv env                     = do defer [c]; return eq
  | Just wit <- search                      = do (cs,p',e) <- instWitness env [] wit
                                                 unifyM env (tcargs p) (tcargs p')
                                                 reduce env eq cs
  where search                              = findWitness env (NoQ $ tvname tv) (tcname p ==)
  
reduce' env eq c@(Impl w (TCon _ tc) p)
  | Just wit <- search                      = do (cs,p',e) <- instWitness env (tcargs tc) wit
                                                 unifyM env (tcargs p) (tcargs p')
                                                 reduce env eq cs
  where search                              = findWitness env (tcname tc) (tcname p ==)

reduce' env eq c@(Impl w (TExist _ u) p)
  | Just (wf,p') <- search                  = do unifyM env (tcargs p) (tcargs p')
                                                 return eq
  | otherwise                               = trace ("## No success") $ noRed c
  where search                              = findAncestor env u (tcname p)

reduce' env eq c@(Sel w t1@(TVar _ tv) n t2)
  | not $ scoped tv env                     = do defer [c]; return eq
  | Just (wf,sc,dec) <- findVAttr env tv n  = do (cs,t) <- instantiate env sc
                                                 -- when (tvSelf `elem` contrafree t) (err1 n "Contravariant Self attribute not selectable by instance")
                                                 let t' = subst [(tvSelf,t1)] t
                                                 cast env t' t2
                                                 reduce env eq cs
  | Just wit <- search                      = do (cs1,p,e) <- instWitness env [] wit
                                                 let Just (wf,sc,dec) = findAttr env p n
                                                 (cs2,t) <- instantiate env sc                -- TODO: apply wits of cs2, make "self" extra arg
                                                 let t' = subst [(tvSelf,t1)] t
                                                 cast env t' t2
                                                 reduce env eq (cs1++cs2)
  | otherwise                               = err1 n "Attribute not found"
  where search                              = findWitness env (NoQ $ tvname tv) (hasAttr env n)

reduce' env eq (Sel w t1@(TCon _ tc) n t2)
  | Just (wf,sc,dec) <- findAttr env tc n   = do (cs,t) <- instantiate env sc
                                                 -- when (tvSelf `elem` contrafree t) (err1 n "Contravariant Self attribute not selectable by instance")
                                                 let t' = subst [(tvSelf,t1)] t
                                                 cast env t' t2
                                                 reduce env eq cs
  | Just wit <- search                      = do (cs1,p,e) <- instWitness env (tcargs tc) wit
                                                 let Just (wf,sc,dec) = findAttr env p n
                                                 (cs2,t) <- instantiate env sc                -- TODO: apply wits of cs2, make "self" extra arg
                                                 let t' = subst [(tvSelf,t1)] t
                                                 cast env t' t2
                                                 reduce env eq (cs1++cs2)
  | otherwise                               = err1 n "Attribute not found"
  where search                              = findWitness env (tcname tc) (hasAttr env n)

reduce' env eq (Sel w (TExist _ p) n t2)
  | Just (wf,sc,dec) <- findAttr env p n    = do (cs,t) <- instantiate env sc
                                                 when (tvSelf `elem` tyfree t) (err1 n "Self attribute not selectable from abstract type")
                                                 cast env t t2
                                                 reduce env eq cs
  | otherwise                               = err1 n "Attribute not found:"

reduce' env eq (Sel w (TTuple _ p r) n t2)  = do cast env r (kwdRow n t2 tWild)
                                                 return eq

reduce' env eq (Sel w (TUnion _ us) n t2)   = do t <- newTVar
                                                 castM env (map mkTCon us) (repeat t)
                                                 reduce env eq [Sel w t n t2]
  where mkTCon (ULit _)                     = tStr
        mkTCon (UCon c)                     = tCon (TC c [])

reduce' env eq c@(Mut t1@(TVar _ tv) n t2)
  | not $ scoped tv env                     = do defer [c]; return eq
  | Just (wf,sc,dec) <- findVAttr env tv n  = do when (dec/=Property) (noMut n)
                                                 (cs,t) <- instantiate env sc
                                                 let t' = subst [(tvSelf,t1)] t
                                                 cast env t1 tObject
                                                 cast env t2 t'
                                                 reduce env eq cs
  | otherwise                               = err1 n "Attribute not found:"

reduce' env eq (Mut t1@(TCon _ tc) n t2)
  | Just (wf,sc,dec) <- findAttr env tc n   = do when (dec/=Property) (noMut n)
                                                 (cs,t) <- instantiate env sc
                                                 let t' = subst [(tvSelf,t1)] t
                                                 cast env t1 tObject
                                                 cast env t2 t'
                                                 reduce env eq cs
  | otherwise                               = err1 n "Attribute not found:"

reduce' env eq c                            = noRed c


----------------------------------------------------------------------------------------------------------------------
-- cast
----------------------------------------------------------------------------------------------------------------------

cast                                        :: Env -> Type -> Type -> TypeM ()
cast env t1 t2                              = do t1' <- msubst t1
                                                 t2' <- msubst t2
                                                 cast' env t1' t2'

castM env ts1 ts2                           = mapM_ (uncurry $ cast env) (ts1 `zip` ts2)

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
  | t2 == tBoolean                          = return ()
  | Just tc <- findVBound env tv            = cast' env (tCon tc) t2

cast' env t1 t2@(TVar _ tv)
  | not $ scoped tv env                           = defer [Cast t1 t2]

cast' env (TCon _ c1) (TCon _ c2)
  | c2 == cBoolean                          = return ()
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
  | Just tc <- findVBound env tv            = sub' env eq w (tCon tc) t2

sub' env eq w t1 t2@(TVar _ tv)
  | not $ scoped tv env                     = do defer [Sub w t1 t2]; return eq

sub' env eq w (TExist _ p1) (TExist l p2)
  | Just (wf,p') <- search                  = do unifyM env (tcargs p1) (tcargs p'); return eq
  where search                              = findAncestor env p1 (tcname p2)

--           as declared           as called
sub' env eq w (TFun _ fx1 p1 k1 t1) (TFun _ fx2 p2 k2 t2)
                                            = do w1 <- newWitness
                                                 w2 <- newWitness
                                                 w3 <- newWitness
                                                 eq1 <- sub env eq  w1 fx1 fx2
                                                 eq2 <- sub env eq1 w2 p2 p1            -- TODO: implement pos/kwd argument shifting
                                                 eq3 <- sub env eq2 w3 k2 k1
                                                 sub env eq3 w t1 t2

sub' env eq w (TTuple _ p1 k1) (TTuple _ p2 k2)
                                            = do eq1 <- sub env eq w p1 p2
                                                 sub env eq1 w k1 k2

sub' env eq w (TNil _ k1) (TNil _ k2)
  | k1 == k2                                = return eq
sub' env eq w (TRow _ k n t1 r1) r2         = do (t2,r2') <- findElem k (tNil k) n r2 (rowTail r1)
                                                 eq1 <- sub env eq w t1 t2
                                                 sub env eq1 w r1 r2'
sub' env eq w t1 t2                         = do cast env t1 t2
                                                 return eq              -- lambda x:x



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
