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
simplify                                    :: Env -> Constraints -> TypeM Constraints
simplify env cs                             = do --traceM ("### simplify: " ++ prstrs cs)
                                                 reduce env cs
                                                 cs0 <- collectDeferred
                                                 cs1 <- msubst cs0
                                                 if simple cs1
                                                     then return [] -- cs1
                                                     else simplify env cs1
  where simple cs                           = True                              -- TODO: add proper test

-- Reduce aggressively or fail
solve                                       :: Env -> Constraints -> TypeM ()
solve env cs                                = do --traceM ("### solve: " ++ prstrs cs)
                                                 reduce env cs
                                                 cs0 <- collectDeferred
                                                 cs1 <- msubst cs0
                                                 if done cs1
                                                     then return ()
                                                     else solve env cs1
  where done cs                             = True                              -- TODO: ensure proper termination...!


----------------------------------------------------------------------------------------------------------------------
-- reduce
----------------------------------------------------------------------------------------------------------------------

reduce                                      :: Env -> Constraints -> TypeM ()
reduce env []                               = return ()
reduce env (c:cs)                           = do c' <- msubst c
                                                 reduce' env c'
                                                 cs' <- msubst cs
                                                 reduce env cs'

reduce'                                     :: Env -> Constraint -> TypeM ()
reduce' env (Cast t1 t2)                    = cast' env t1 t2
reduce' env (Sub w t1 t2)                   = sub' env w t1 t2

reduce' env c@(Impl w (TVar _ tv) u)
  | not $ skolem tv                         = defer [c]
reduce' env c@(Impl w t u)
  | otherwise                   = return ()                                      -- TODO: implement, of course

--reduce' (Qual w q cs)                       = ...


reduce' env c@(Sel (TVar _ tv) n t2)
  | not $ skolem tv                         = defer [c]
  | Just u <- findSubBound tv env           = reduce' env (Sel (tCon u) n t2)
reduce' env (Sel t1@(TCon _ tc) n t2)       = do let (sc,dec) = findAttr env tc n
                                                 when (dec == Static) (noSelStatic n tc)
                                                 (cs,t) <- instantiate env sc
                                                 let t' = subst [(tvSelf,t1)] t
                                                 reduce env (Cast t' t2 : cs)
reduce' env (Sel (TTuple _ p r) n t2)       = reduce' env (Cast r (kwdRow n t2 tWild))
{-
reduce' env (Sel (TExist _ p) n t2)         = do let (sc,dec) = findAttr env tc n
                                                 when (dec==Property) (noSelInstByClass n tc)
                                                 (cs,t) <- instantiate env (addself sc dec)
                                                 let t' = subst [(tvSelf,tCon tc)] t
                                                 reduce env (Cast t' t2 : cs)
  where
    addself sc Static                       = sc
    addself (TSchema l q t) _               = TSchema l q (addself' t)
    addself' (TFun l fx p r t)              = TFun l fx (posRow (monotype tSelf) p) r t
    addself' t                              = TFun (loc t) fxNil (posRow (monotype tSelf) posNil) kwdNil t
-}
reduce' env (Sel (TUnion _ [ULit _]) n t2)  = reduce' env (Sel tStr n t2)

reduce' env c@(Mut (TVar _ tv) n t2)
  | not $ skolem tv                         = defer [c]
  | Just u <- findSubBound tv env           = reduce' env (Mut (tCon u) n t2)
reduce' env (Mut t1@(TCon _ tc) n t2)       = do let (sc,dec) = findAttr env tc n
                                                 when (dec==Property) (noMutClass n)
                                                 (cs,t) <- instantiate env sc
                                                 let t' = subst [(tvSelf,t1)] t
                                                 reduce env (Cast t1 tObject : Cast t' t2 : cs)
reduce' env c                               = noRed c


----------------------------------------------------------------------------------------------------------------------
-- cast
----------------------------------------------------------------------------------------------------------------------

cast                                        :: Env -> Type -> Type -> TypeM ()
cast env t1 t2                              = do t1' <- msubst t1
                                                 t2' <- msubst t2
                                                 cast' env t1' t2'

{-  
-- part of computing GLBs
cast' env (TVar _ tv) t2
  | not $ skolem tv                         = do when (tv `elem` tyfree t2) (infiniteType tv)
                                                 substitute tv t2
-- part of computing LUBs
cast' env t1 (TVar _ tv)
  | not $ skolem tv                         = do when (tv `elem` tyfree t1) (infiniteType tv)
                                                 substitute tv t1
-}
                                             
cast' env (TVar _ tv1) (TVar _ tv2)
  | tv1 == tv2                              = return ()

cast' env t1@(TVar _ tv) t2
  | not $ skolem tv                         = defer [Cast t1 t2]
-- if skolem...

cast' env t1 t2@(TVar _ tv)
  | not $ skolem tv                         = defer [Cast t1 t2]
-- if skolem...

cast' env (TCon _ c1) (TCon _ c2)
  | tcname c1 == tcname c2                  = mapM_ (uncurry $ cast env) ((tcargs c1 `zip` tcargs c2) ++ (tcargs c2 `zip` tcargs c1))   -- TODO: use polarities
  | not $ null sup                          = mapM_ (uncurry $ cast env) ((head sup `zip` tcargs c2) ++ (tcargs c2 `zip` head sup))     -- TODO: use polarities
  where NClass q as te                      = findQName (tcname c1) env
        s                                   = tybound q `zip` tcargs c1
        sup                                 = [ subst s (tcargs c) | c <- as, tcname c == tcname c2 ]


cast' env (TExist _ p1) (TExist l p2)
  | tcname p1 == tcname p2                  = mapM_ (uncurry $ cast env) ((tcargs p1 `zip` tcargs p2) ++ (tcargs p2 `zip` tcargs p1))   -- TODO: use polarities

--           as declared           as called
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
cast' env (TRow _ k n t1 r1) r2             = do (t2,r2') <- findElem (tNil k) n r2 (rowTail r1)
                                                 cast env t1 t2
                                                 cast env r1 r2'
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
          | otherwise                       = do t <- newTVar
                                                 r <- newTVarOfKind k
                                                 substitute tv (tRow k n t r)
                                                 return (t, revApp r0 r)
        revApp (TRow l k n t r1) r2         = revApp r1 (TRow l k n t r2)
        revApp (TNil _ _) r2                = r2
cast' env t1 t2                             = noRed (Cast t1 t2)



----------------------------------------------------------------------------------------------------------------------
-- sub
----------------------------------------------------------------------------------------------------------------------

sub                                         :: Env -> Name -> Type -> Type ->TypeM ()
sub env w t1 t2                             = do t1' <- msubst t1
                                                 t2' <- msubst t2
                                                 sub' env w t1' t2'

sub'                                        :: Env -> Name -> Type -> Type ->TypeM ()
sub' env w (TVar _ tv1) (TVar _ tv2)
  | tv1 == tv2                              = return ()

sub' env w t1@(TVar _ tv) t2
  | not $ skolem tv                         = defer [Sub w t1 t2]
-- if skolem...

sub' env w t1 t2@(TVar _ tv)
  | not $ skolem tv                         = defer [Sub w t1 t2]
-- if skolem...

sub' env w (TExist _ p1) (TExist l p2)
  | not $ null sup                          = mapM_ (uncurry $ cast env) ((head sup `zip` tcargs p2) ++ (tcargs p2 `zip` head sup))     -- TODO: use polarities
  where NProto q as te                      = findQName (tcname p1) env
        s                                   = tybound q `zip` tcargs p1
        sup                                 = [ subst s (tcargs c) | c <- as, tcname c == tcname p2 ]

--           as declared           as called
sub' env w (TFun _ fx1 p1 k1 t1) (TFun _ fx2 p2 k2 t2)
                                            = do sub env w fx1 fx2
                                                 sub env w p2 p1            -- TODO: implement pos/kwd argument shifting
                                                 sub env w k2 k1
                                                 sub env w t1 t2

sub' env w (TTuple _ p1 k1) (TTuple _ p2 k2)
                                            = do sub env w p1 p2
                                                 sub env w k1 k2

sub' env w (TNil _ k1) (TNil _ k2)
  | k1 == k2                                = return ()
sub' env w (TRow _ k n t1 r1) r2            = do (t2,r2') <- findElem (tNil k) n r2 (rowTail r1)
                                                 sub env w t1 t2
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
          | otherwise                       = do t <- newTVar
                                                 r <- newTVarOfKind k
                                                 substitute tv (tRow k n t r)
                                                 return (t, revApp r0 r)
        revApp (TRow l k n t r1) r2         = revApp r1 (TRow l k n t r2)
        revApp (TNil _ _) r2                = r2
sub' env w t1 t2                            = do cast env t1 t2
                                                 return ()              -- lambda x:x




{-


w0:A(Eq) | A,x:A,y:A |= Eq._eq_ : (A,A)->bool  ~>  w0._eq_      ... |- x : A        ... |- y : A
------------------------------------------------------------------------------------------------
w0:A(Eq) | A,x:A,y:A |= Eq._eq_(x,y) : bool  ~>  w0._eq_(x,y)
---------------------------------------------------------------------------
w0:A(Eq) | A,x:A,y:A |= return Eq._eq_(x,y) : bool  ~>  return w0._eq_(x,y)
--------------------------------------------------------------------------------------------------------------
w2:[A(Ord)]=>A(Eq) | |= def f [A(Ord)] (x:A, y:A) -> bool: return Eq._eq_(x,y) : [A(Ord)]=>(x:A,y:A)->bool  ~>  
                        def f [A] (w1:A(Ord),x:A,y:A)->bool: w0=w2(w1); return w0._eq_(x,y)
----------------------------------------------------------------------------------------------
 | |= def f [A(Ord)] (x:A, y:A) -> bool: return Eq.__eq__(x,y) : [A(Ord)]=>(x:A,y:A)->bool  ~>  
      def w2 [A] (w3:A(Ord))->A(Eq):return w3._Eq_   +
      def f [A] (w1:A(Ord),x:A,y:A)->bool: w0=w2(w1); return w0._eq_(x,y)


ws:cs | N |= E : t ~> E'   ==>   N,Q(cs) |- E : t ~> E'


w:A(Ord)x:A,y:A |- Eq._eq_ : (A,A)->bool  ~>  w._Eq_._eq_      ... |- x : A        ... |- y : A
-----------------------------------------------------------------------------------------------
w:A(Ord),x:A,y:A |- Eq._eq_(x,y) : bool  ~>  w._Eq_._eq_(x,y)
---------------------------------------------------------------------------
w:A(Ord),x:A,y:A |- return Eq._eq_(x,y) : bool  ~>  return w._Eq_._eq_(x,y)
----------------------------------------------------------------------------------------------
|- def f [A(Ord)] (x:A, y:A) -> bool: return Eq._Eq_._eq_(x,y) : [A(Ord)]=>(x:A,y:A)->bool  ~>  
   def f [A] (w:A(Ord),x:A,y:A)->bool: return w._Eq_._eq_(x,y)


-}

