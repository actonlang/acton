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

reduceAll env cs                            = mapM_ (reduce env) cs

reduce env c                                = do c' <- msubst c
                                                 --traceM ("### reduce: " ++ prstr c')
                                                 reduce' env c'

reduce' env (Sub t1 t2)                     = red' True env t1 t2
reduce' env (Equ t1 t2)                     = red' False env t1 t2
reduce' env (SubGen t1 t2)                  = redGen' True env t1 t2
reduce' env (EquGen t1 t2)                  = redGen' False env t1 t2

reduce' env c@(Impl (TVar _ tv) u)
  | not $ skolem tv                         = defer [c]
reduce' env (Impl t u)
  | entail env (Impl t u)                   = return ()

reduce' env c@(Sel (TVar _ tv) n t2)
  | not $ skolem tv                         = defer [c]
  | Just u <- findSubBound tv env           = reduce' env (Sel (tCon u) n t2)
reduce' env (Sel t1@(TCon _ tc) n t2)
  | Just sc <- moduleAttr tc n env          = do (cs,t) <- instantiate env sc
                                                 reduceAll env (Equ t t2 : cs)
  | Just (qn,i) <- protoAttr n env          = do u <- TC qn <$> newTVars i
                                                 let (cs,sc) = findAttr env u n
                                                 when (scdec sc == StaticMethod) (noSelStatic n u)
                                                 (cs',t) <- instantiate env sc
                                                 let t' = subst [(tvSelf,t1)] t
                                                 reduceAll env (Impl t1 u : Equ t' t2 : cs ++ cs')
  | otherwise                               = do let (cs,sc) = findAttr env tc n
                                                 when (scdec sc == StaticMethod) (noSelStatic n tc)
                                                 (cs,t) <- instantiate env sc
                                                 let t' = subst [(tvSelf,t1)] t
                                                 reduceAll env (Equ t' t2 : cs)
reduce' env (Sel (TRecord _ r) n t2)        = reduce env (Equ r (kwdRow n (monotype t2) tWild))

reduce' env (Sel (TAt _ tc) n t2) 
  | Just (qn,i) <- protoAttr n env          = notYet (loc n) "Protocol attribute selection from class"
  | otherwise                               = do let (cs,sc) = findAttr env tc n
                                                 when (isInstAttr $ scdec sc) (noSelInstByClass n tc)
                                                 (cs,t) <- instantiate env (addself sc)
                                                 let t' = subst [(tvSelf,tCon tc)] t
                                                 reduceAll env (Equ t' t2 : cs)
  where
    addself (TSchema l q t (InstMethod _))  = TSchema l q (addself' t) StaticMethod
    addself (TSchema l q t ClassAttr)       = TSchema l q (addself' t) StaticMethod
    addself sc                              = sc
    addself' (TFun l fx p r t)              = TFun l fx (posRow (monotype tSelf) p) r t
    addself' t                              = TFun (loc t) fxNil (posRow (monotype tSelf) posNil) kwdNil t
    

reduce' env (Mut t1@(TVar _ tv) n t2)
  | not $ skolem tv                         = defer [Mut t1 n t2]
  | Just u <- findSubBound tv env           = reduce' env (Mut (tCon u) n t2)
reduce' env (Mut t1@(TCon _ tc) n t2)
  | Just (qn,i) <- protoAttr n env          = noMutProto n
  | otherwise                               = do let (cs,sc) = findAttr env tc n
                                                 when (not $ isInstAttr $ scdec sc) (noMutClass n)
                                                 (cs,t) <- instantiate env sc
                                                 let t' = subst [(tvSelf,t1)] t
                                                 reduceAll env (Sub t1 tObject : Equ t' t2 : cs)
reduce' env c                               = noRed c



red sub env t1 t2                           = do t1' <- msubst t1
                                                 t2' <- msubst t2
                                                 --traceM ("### red: " ++ prstr (if sub then Sub t1' t2' else Equ t1' t2'))
                                                 red' sub env t1' t2'

red' sub env (TVar _ tv1) (TVar _ tv2)
  | tv1 == tv2                              = return ()

red' True env t1@(TVar _ tv) t2
  | not $ skolem tv                         = defer [Sub t1 t2]
  | entail env (Sub t1 t2)                  = return ()
red' True env t1 t2@(TVar _ tv)
  | not $ skolem tv                         = defer [Sub t1 t2]
  | entail env (Sub t1 t2)                  = return ()

red' False env (TVar _ tv) t2
  | not $ skolem tv                         = do when (tv `elem` tyfree t2) (infiniteType tv)
                                                 substitute tv t2
red' False env t1 (TVar _ tv)
  | not $ skolem tv                         = do when (tv `elem` tyfree t1) (infiniteType tv)
                                                 substitute tv t1

red' sub env (TCon _ c1) (TCon l c2)
  | tcname c1 == tcname c2                  = mapM_ (uncurry $ red False env) (tcargs c1 `zip` tcargs c2)       -- TODO: use polarities
  | otherwise                               = do reduceAll env cs
                                                 red sub env t (TCon l c2)
  where (cs,t)                              = findSubAxiom env c1 (tcname c2)

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
                                                 red sub env r1 r2'
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

redGen sub env sc1 sc2                      = do sc1' <- msubst sc1
                                                 sc2' <- msubst sc2
                                                 redGen' sub env sc1' sc2'

redGen' sub env (TSchema _ [] t1 d1) (TSchema _ [] t2 d2)
  | d1 /= d2                                = distinctDecorations d1 d2
  | otherwise                               = red' sub env t1 t2
redGen' sub env sc1 sc2@(TSchema _ q2 t2 d2)
  | scdec sc1 /= d2                         = distinctDecorations (scdec sc1) d2
  | otherwise                               = do (cs,t1) <- instantiate env1 sc1
                                                 mapM_ (reduce env) cs
                                                 red sub env1 t1 t2
                                                 tvs <- (intersect (tybound q2) . tyfree) <$> msubst [sc1,sc2]
                                                 when (any (`elem` tvs) (tybound q2)) (escapingVar tvs sc1 sc2)
  where env1                                = defineTVars q2 env


-- Entailment ----------------------------------------------------------------------------

entail                                  :: Env -> Constraint -> Bool
entail env c                            = True                                              -- TODO: implement this



-- Instantiation -------------------------------------------------------------------------

instantiate                     :: Env -> TSchema -> TypeM (Constraints, Type)
instantiate env (TSchema _ [] t _)
                                = return ([], t)
instantiate env (TSchema _ q t _)
                                = do tvs <- newTVars (length q)
                                     let s = tybound q `zip` tvs
                                     return (constraintsOf env (subst s q), subst s t)


