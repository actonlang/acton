{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Acton.Constraints where

import Debug.Trace
import Data.Maybe (maybeToList, fromJust)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Control.Monad.State.Strict
import Control.Monad.Except
import qualified Control.Exception
import Data.Typeable
import Pretty
import Utils
import Acton.Syntax
import Acton.Names
import Acton.TypeM
import Acton.Env


unify t1 t2                                 = do -- traceM ("unify " ++ prstr (QEqu l0 0 t1 t2))
                                                 unify' t1 t2

unify' (OVar tv1) (OVar tv2)
  | tv1 == tv2                              = return ()
unify' (OVar tv) t2                         = do s <- o_substitution
                                                 case Map.lookup tv s of
                                                   Just t1 -> unify' t1 t2
                                                   Nothing -> do t2' <- mapsubst t2
                                                                 when (tv `elem` oTyvars t2') (throwError $ InfiniteType tv)
                                                                 o_substitute tv t2'
unify' t1 (OVar tv)                         = do s <- o_substitution
                                                 case Map.lookup tv s of
                                                   Just t2 -> unify' t1 t2
                                                   Nothing -> do t1' <- mapsubst t1
                                                                 when (tv `elem` oTyvars t1') (throwError $ InfiniteType tv)
                                                                 o_substitute tv t1'

--       as declared      as called
unify' (OFun a1 r1 t1) (OFun a2 r2 t2)      = do unify a1 a2
                                                 unify r2 r1            -- contra-variant
                                                 unify t1 t2
unify' (ORecord r1) (ORecord r2)            = unify r1 r2
unify' (ODict k1 v1) (ODict k2 v2)          = do unify k1 k2
                                                 unify v1 v2
unify' (OTuple r1) (OTuple r2)              = unify r1 r2
unify' (OList t1) (OList t2)                = unify t1 t2
unify' (OSet t1) (OSet t2)                  = unify t1 t2
unify' (OMsg t1) (OMsg t2)                  = unify t1 t2
unify' OStr OStr                            = return ()
unify' OInt OInt                            = return ()
unify' OFloat OFloat                        = return ()
unify' OBool OBool                          = return ()
unify' ONone ONone                          = return ()
unify' _     ONone                          = return ()         -- temporary, until the opt type...
unify' ONone _                              = return ()         -- temporary, until the opt type...


unify' (OPos t1 r1) r2                      = do (t2,r2') <- findFst r2 (o_rowTail r1)
                                                 unify t1 t2
                                                 unify r1 r2'
  where findFst r tl                        = do r' <- mapsubst r
                                                 tl' <- mapsubst tl
                                                 findFst' r' tl'
        findFst' (OPos t r2) tl             = return (t, r2)
        findFst' (OStar1 t ONil) tl         = do r3 <- newOVar
                                                 unify t (OTuple r3)
                                                 (t',r3') <- findFst r3 tl
                                                 return (t', r3')
        findFst' r2@OStar1{} tl             = internal r1 r2
        findFst' (OKwd n t r2) tl           = return (t, r2)
        findFst' (OStar2 t ONil) tl         = do r3 <- newOVar
                                                 a2 <- newOVar
                                                 unify t (ORecord r3)
                                                 (t',r3') <- findFst r3 tl
                                                 return (t', r3')
        findFst' r2@OStar2{} tl             = internal r1 r2
        findFst' ONil tl                    = throwError EmptyRow
        findFst' r2@(OVar tv) tl
          | r2 == tl                        = throwError (ConflictingRow tv)
          | otherwise                       = do t <- newOVar
                                                 r <- newOVar
                                                 o_substitute tv (OPos t r)
                                                 return (t, r)
        findFst' r tl                       = error ("##### findFst' " ++ prstr r)

unify' (OStar1 t r1) r2                     = do r1' <- mapsubst r1
                                                 case r1' of
                                                    ONil -> do
                                                        r <- newOVar
                                                        unify t (OTuple r)
                                                        unify r r2
                                                    _ -> throwError Defer

unify' r1 (OStar1 t r2)                     = do r2' <- mapsubst r2
                                                 case r2' of
                                                    ONil -> do
                                                        r <- newOVar
                                                        unify (OTuple r) t
                                                        unify r1 r
                                                    _ -> throwError Defer

unify' (OKwd n t1 r1) r2                    = do (t2,r2') <- findKwd ONil n r2 (o_rowTail r1)
                                                 unify t1 t2
                                                 unify (del n r1) (del n r2')
  where findKwd r0 n r tl                   = do r0' <- mapsubst r0
                                                 r'  <- mapsubst r
                                                 tl' <- mapsubst tl
                                                 findKwd' r0' n r' tl'
        findKwd' r0 n (OKwd n1 t r2) tl
          | n == n1                         = return (t, revApp r0 r2)
          | otherwise                       = findKwd' (OKwd n1 t r0) n r2 tl
        findKwd' r0 n (OStar2 t ONil) tl    = do r3 <- newOVar
                                                 a2 <- newOVar
                                                 unify t (ORecord r3)
                                                 (t',r3') <- findKwd ONil n r3 tl
                                                 return (t', revApp r0 r3')
        findKwd' r0 n r2@OStar2{} tl        = internal r1 r2
        findKwd' r0 n (OPos t r2) tl        = findKwd' (OPos t r0) n r2 tl
        findKwd' r0 n (OStar1 t ONil) tl    = do r3 <- newOVar
                                                 unify t (OTuple r3)
                                                 findKwd r0 n r3 tl
        findKwd' r0 n r2@OStar1{} tl        = internal r1 r2
        findKwd' r0 n ONil tl               = throwError (KwdNotFound n)
        findKwd' r0 n r2@(OVar tv) tl
          | r2 == tl                        = throwError (ConflictingRow tv)
          | otherwise                       = do t <- newOVar
                                                 r <- newOVar
                                                 o_substitute tv (OKwd n t r)
                                                 return (t, revApp r0 r)

unify' (OStar2 t r1) r2                     = do r <- newOVar
                                                 unify t (ORecord r)
                                                 r' <- mapsubst r
                                                 r1' <- mapsubst r1
                                                 unify (catRow r' r1') r2

unify' r1 (OStar2 t r2)                     = do r <- newOVar
                                                 unify (ORecord r) t
                                                 r' <- mapsubst r
                                                 r2' <- mapsubst r2
                                                 unify r1 (catRow r' r2')

unify' ONil ONil                            = return ()

unify' (OSchema vs1 cs1 t1) (OSchema vs2 cs2 t2)
  | vs1==vs1 && cs1==cs2 && t1==t2          = return ()

-- Will go away when Rank-N records become nominal
unify' (OSchema vs1 [] t1) t2               = do ts <- mapM (const newOVar) vs1
                                                 let s = vs1 `zip` ts
                                                 unify' (oSubst s t1) t2

unify' (OList t1) (ODict k2 v2)             = do unify t1 k2                  -- Temporary HACK...
                                                 unify OInt v2


unify' t1 t2                                = throwError (NoUnify (erase t1) (erase t2))



catRow (OPos t r1) r2                       = OPos t (catRow r1 r2)
catRow (OKwd n t r1) r2                     = OKwd n t (catRow r1 r2)
catRow (OStar1 t r1) r2                     = OStar1 t (catRow r1 r2)
catRow (OStar2 t r1) r2                     = OStar2 t (catRow r1 r2)
catRow ONil r2                              = r2
catRow r1 ONil                              = r1
catRow r1 r2                                = internal r1 r2

o_rowTail (OPos _ r)                    = o_rowTail r
o_rowTail (OStar1 _ r)                  = o_rowTail r
o_rowTail (OKwd _ _ r)                  = o_rowTail r
o_rowTail (OStar2 _ r)                  = o_rowTail r
o_rowTail r                             = r             -- OVar v or ONil

revApp (OPos t r1) r2                   = revApp r1 (OPos t r2)
revApp (OStar1 t r1) r2                 = revApp r1 (OStar1 t r2)
revApp (OKwd n t r1) r2                 = revApp r1 (OKwd n t r2)
revApp (OStar2 t r1) r2                 = revApp r1 (OStar2 t r2)
revApp ONil r2                          = r2

del n (OKwd m t r)
  | n == m                              = del n r
  | otherwise                           = OKwd m t (del n r)
del n (OStar2 (ORecord r1) r)           = OStar2 (ORecord (del n r1)) (del n r)
del n (OStar2 t r)                      = OStar2 t (del n r)
del n (OPos t r)                        = OPos t (del n r)
del n (OStar1 t r)                      = OStar1 t (del n r)
del n r                                 = r

internal r1 r2                          = error ("Internal: Cannot unify: " ++ prstr r1 ++ " = " ++ prstr r2)


-- Environment unification ---------------------------------------------------------------

unifyTEnv env tenvs []                  = return []
unifyTEnv env tenvs (v:vs)              = case [ ni | Just ni <- map (lookup v) tenvs] of
                                            [] -> unifyTEnv env tenvs vs
                                            [ni] -> ((v,ni):) <$> unifyTEnv env tenvs vs
                                            ni:nis -> do ni' <- unifN ni nis
                                                         ((v,ni'):) <$> unifyTEnv env tenvs vs
  where 
    unifN (NVar (TSchema _ [] t) d) nis = do mapM (unifV t d) nis
                                             return (NVar (tSchema t) d)
    unifN (NSVar t) nis                 = do mapM (unifSV t) nis
                                             return (NSVar t)
    unifN ni nis                        = notYet (loc v) (text "Merging of declarations")

    unifV t d (NVar (TSchema _ [] t') d')
      | d == d'                         = constrain [Equ t t']
      | otherwise                       = err1 v "Inconsistent decorations for"
    unifV t d ni                        = err1 v "Inconsistent bindings for"

    unifSV t (NSVar t')                 = constrain [Equ t t']
    unifSV t ni                         = err1 v "Inconsistent bindings for"


instantiate env (TSchema _ [] t)        = return t
instantiate env (TSchema _ q t)         = do tvs <- newTVars (length q)
                                             let s = tybound q `zip` tvs
                                                 q1 = subst s q
                                                 t1 = subst s t
                                             mapM (q_constrain env) q1
                                             return t1

q_constrain env (TBind tv cs)           = mapM (q_constr tv) cs
  where q_constr tv tc@(TC qn ts)       = case findqname qn env of
                                            NClass{} -> constrain [Sub (tVar tv) (tCon tc)]
                                            NProto{} -> constrain [Impl (tVar tv) tc]



----------------------------------------



newOVar                                 = intOVar <$> o_unique

newOVars n                              = mapM (const newOVar) [1..n]

o_instantiate l t0@(OSchema vs cs t)    = do tvs <- newOVars (length vs)
                                             let s   = vs `zip` tvs
                                                 cs1 = [ oSubst s c{cloc=l} | c <- cs ]
                                                 t1  = oSubst s t
                                             o_constrain cs1
--                                             o_dump [INS l t1]
                                             return t1
o_instantiate l t                       = do -- o_dump [INS l t]
                                             return t


erase x                                 = oSubst s x
  where s                               = [ (tv, wildOVar) | tv <- nub (oTyvars x) ]

---------


-- Reduce conservatively and remove entailed constraints
simplify                                :: [Constraint] -> TypeM [Constraint]
simplify cs                             = return cs

-- Reduce aggressively or fail
solve                                   :: [Constraint] -> TypeM ()
solve cs                                = return ()

o_solveAll cs                               = do -- traceM ("##### SolveAll: ")
                                                 -- traceM (prstr cs)
                                                 -- traceM ("##### Resolving "++ show (length cs))
                                                 -- o_resetsubst True
                                                 o_resolveAll False cs
                                                 s <- o_realsubst
                                                 -- traceM ("##### Result:\n" ++ prstr s)
                                                 return ()

o_reduce cs                                 = do red False cs
                                                 cs0 <- o_deferred
                                                 cs1 <- mapsubst cs0
                                                 if cs0 == cs1 then return cs1 else o_reduce cs1

o_resolve cs                                = do red True cs
                                                 cs0 <- o_deferred
                                                 cs1 <- mapsubst cs0
                                                 if cs1 == [] then return () else o_resolve cs1
                                                 
    
o_resolveAll f cs                           = do red f cs
                                                 cs0 <- o_deferred
                                                 if cs0 == [] then return () else do
                                                     cs1 <- mapsubst cs0
                                                     let useForce = cs1 == cs0
                                                     -- traceM ("##### Reduced: "++show useForce++" "++show (length cs1))
                                                     -- when useForce (traceM (prstr cs0))
                                                     o_resolveAll useForce cs1

red f []                                    = return ()
red f (c : cs)                              = do c' <- mapsubst c
                                                 red1 f c' `catchError` handler c
                                                 red f cs
  where handler c err                       = do c' <- mapsubst c
                                                 case err of
                                                     Defer -> o_defer c'
                                                     _     -> throwError $ NoSolve (erase c') err

red1 f (QEqu _ _ t1 t2)                     = unify' t1 t2

red1 f (QIn _ _ t (OList u))                = unify' t u
red1 f (QIn _ _ t (ODict k v))              = unify' t k
red1 f (QIn _ _ t OStr)                     = unify' t OStr
red1 f (QIn _ _ t (ORecord r))              = do t1 <- newOVar; unify' t (OTuple (OPos OStr (OPos t1 ONil)))    -- ...hack...

red1 f (QDot l _ (ORecord r) n t)           = case lookupRow n r of
                                                Right t0 -> do
                                                    -- o_dump [GEN l t0]
                                                    t1 <- o_instantiate l $ o_openFX t0
                                                    cs1 <- o_constraints
                                                    unify' t1 t
                                                    red f cs1
                                                Left ONil ->
                                                    throwError $ NoSelect n
                                                Left r1  -> do
                                                    r2 <- newOVar
                                                    unify' r1 (OKwd n t r2)
  where lookupRow n (OKwd n' t r)
          | n == n'                         = Right t
          | otherwise                       = lookupRow n r
        lookupRow n (OPos t r)              = lookupRow n r
        lookupRow n r                       = Left r
red1 f (QDot l _ (OList t) n u)
  | nstr n == "append"                      = builtinFun l (OFun ONil (OPos t ONil) ONone) u
  | nstr n == "clear"                       = builtinFun l (OFun ONil ONil ONone) u
  | nstr n == "copy"                        = builtinFun l (OFun ONil ONil (OList t)) u
  | nstr n == "extend"                      = builtinFun l (OFun ONil (OPos (OList t) ONil) ONone) u
  | nstr n == "insert"                      = builtinFun l (OFun ONil (OPos OInt (OPos (OList t) ONil)) ONone) u
  | nstr n == "pop"                         = builtinFun l (OFun ONil (OPos OInt ONil) t) u
  | nstr n == "remove"                      = builtinFun l (OFun ONil (OPos t ONil) ONone) u
  | nstr n == "reverse"                     = builtinFun l (OFun ONil ONil ONone) u
  | nstr n == "sort"                        = builtinFun l (OFun ONil ONil ONone) u
  | otherwise                               = throwError $ NoSelect n
red1 f (QDot l _ (ODict k v) n u)
  | nstr n == "items"                       = builtinFun l (OFun ONil ONil (OList (OTuple (OPos k (OPos v ONil))))) u
  | nstr n == "keys"                        = builtinFun l (OFun ONil ONil (OList k)) u
  | nstr n == "values"                      = builtinFun l (OFun ONil ONil (OList v)) u
  | nstr n == "update"                      = builtinFun l (OFun ONil (OPos (ODict k v) ONil) ONone) u
  | otherwise                               = throwError $ NoSelect n
red1 f (QDot l _ (OSet t) n u)
  | nstr n == "add"                         = builtinFun l (OFun ONil (OPos t ONil) ONone) u
  | nstr n == "clear"                       = builtinFun l (OFun ONil ONil ONone) u
  | nstr n == "pop"                         = builtinFun l (OFun ONil ONil t) u
  | nstr n == "remove"                      = builtinFun l (OFun ONil (OPos t ONil) ONone) u
  | nstr n == "discard"                     = builtinFun l (OFun ONil (OPos t ONil) ONone) u
  | otherwise                               = throwError $ NoSelect n
red1 f (QDot l _ OStr n u)
  | nstr n == "capitalize"                  = builtinFun l (OFun ONil ONil OStr) u
  | nstr n == "casefold"                    = builtinFun l (OFun ONil ONil OStr) u
  | nstr n == "center"                      = builtinFun l (OFun ONil (OPos OInt ONil) OStr) u
  | nstr n == "count"                       = builtinFun l (OFun ONil (OPos OStr ONil) OInt) u
  | nstr n == "encode"                      = builtinFun l (OFun ONil ONil OStr) u
  | nstr n == "endswith"                    = builtinFun l (OFun ONil (OPos OStr ONil) OBool) u
  | nstr n == "expandtabs"                  = builtinFun l (OFun ONil ONil OStr) u
  | nstr n == "format"                      = do r <- newOVar; builtinFun l (OFun ONil r OStr) u
  | nstr n == "format_map"                  = builtinFun l (OFun ONil (OPos (ODict OStr OStr) ONil) OStr) u
  | nstr n == "find"                        = builtinFun l (OFun ONil (OPos OStr ONil) OInt) u
--  | nstr n == "index"                       = builtinFun l (OFun ONil (OPos OStr ONil) OInt) u
  | nstr n == "isalnum"                     = builtinFun l (OFun ONil ONil OBool) u
  | nstr n == "isalpha"                     = builtinFun l (OFun ONil ONil OBool) u
  | nstr n == "isdecimal"                   = builtinFun l (OFun ONil ONil OBool) u
  | nstr n == "isdigit"                     = builtinFun l (OFun ONil ONil OBool) u
  | nstr n == "isidentifier"                = builtinFun l (OFun ONil ONil OBool) u
  | nstr n == "islower"                     = builtinFun l (OFun ONil ONil OBool) u
  | nstr n == "isnumeric"                   = builtinFun l (OFun ONil ONil OBool) u
  | nstr n == "isprintable"                 = builtinFun l (OFun ONil ONil OBool) u
  | nstr n == "isspace"                     = builtinFun l (OFun ONil ONil OBool) u
  | nstr n == "istitle"                     = builtinFun l (OFun ONil ONil OBool) u
  | nstr n == "isupper"                     = builtinFun l (OFun ONil ONil OBool) u
  | nstr n == "join"                        = builtinFun l (OFun ONil (OPos (OList OStr) ONil) OStr) u
  | nstr n == "ljust"                       = builtinFun l (OFun ONil (OPos OInt ONil) OStr) u
  | nstr n == "lower"                       = builtinFun l (OFun ONil ONil OStr) u
  | nstr n == "lstrip"                      = builtinFun l (OFun ONil ONil OStr) u
  | nstr n == "partition"                   = builtinFun l (OFun ONil (OPos OStr ONil) (OTuple (OPos OStr (OPos OStr (OPos OStr ONil))))) u
  | nstr n == "replace"                     = builtinFun l (OFun ONil (OPos OStr (OPos OStr ONil)) OStr) u
  | nstr n == "rfind"                       = builtinFun l (OFun ONil (OPos OStr ONil) OInt) u
  | nstr n == "rindex"                      = builtinFun l (OFun ONil (OPos OStr ONil) OInt) u
  | nstr n == "rjust"                       = builtinFun l (OFun ONil (OPos OInt ONil) OStr) u
  | nstr n == "rpartition"                  = builtinFun l (OFun ONil (OPos OStr ONil) (OTuple (OPos OStr (OPos OStr (OPos OStr ONil))))) u
  | nstr n == "rsplit"                      = do v <- newOVar; builtinFun l (OFun ONil (OPos OStr v) (OList OStr)) u
  | nstr n == "rstrip"                      = builtinFun l (OFun ONil ONil OStr) u
  | nstr n == "split"                       = do v <- newOVar; builtinFun l (OFun ONil (OPos OStr v) (OList OStr)) u
  | nstr n == "splitlines"                  = builtinFun l (OFun ONil ONil (OList OStr)) u
  | nstr n == "startswith"                  = builtinFun l (OFun ONil (OPos OStr ONil) OBool) u
  | nstr n == "strip"                       = builtinFun l (OFun ONil ONil OStr) u
  | nstr n == "swapcase"                    = builtinFun l (OFun ONil ONil OStr) u
  | nstr n == "title"                       = builtinFun l (OFun ONil ONil OStr) u
  | nstr n == "translate"                   = builtinFun l (OFun ONil (OPos (ODict OInt OStr) ONil) OStr) u
  | nstr n == "upper"                       = builtinFun l (OFun ONil ONil OStr) u
  | nstr n == "zfill"                       = builtinFun l (OFun ONil (OPos OInt ONil) OStr) u
  | otherwise                               = throwError $ NoSelect n

red1 f (QIx _ _ (OList t) i u)              = do unify' OInt i; unify t u
red1 f (QIx _ _ (ODict k v) i u)            = do unify' k i; unify v u
red1 f (QIx _ _ OStr i u)                   = do unify' OInt i; unify OStr u

red1 f (QMod _ _ OStr ORecord{})            = return ()
red1 f (QMod _ _ OStr OTuple{})             = return ()
red1 f (QMod _ _ OStr OList{})              = return ()
red1 f (QMod _ _ OStr OSet{})               = return ()
red1 f (QMod _ _ OStr OStr)                 = return ()
red1 f (QMod _ _ OStr OInt)                 = return ()
red1 f (QMod _ _ OStr OFloat)               = return ()
red1 f (QMod _ _ OStr OBool)                = return ()
red1 f (QMod _ _ OInt t)                    = unify' OInt t
red1 f (QMod _ _ OFloat t)                  = unify' OFloat t

red1 f (QPlus _ _ OList{})                  = return ()
red1 f (QPlus _ _ OSet{})                   = return ()
red1 f (QPlus _ _ OStr)                     = return ()
red1 f (QPlus _ _ OInt)                     = return ()
red1 f (QPlus _ _ OFloat)                   = return ()

red1 f (QNum _ _ OInt)                      = return ()
red1 f (QNum _ _ OFloat)                    = return ()

red1 f (QBool _ _ ORecord{})                = return ()
red1 f (QBool _ _ ODict{})                  = return ()
red1 f (QBool _ _ OTuple{})                 = return ()
red1 f (QBool _ _ OList{})                  = return ()
red1 f (QBool _ _ OSet{})                   = return ()
red1 f (QBool _ _ OMsg{})                   = return ()
red1 f (QBool _ _ OStr)                     = return ()
red1 f (QBool _ _ OInt)                     = return ()
red1 f (QBool _ _ OFloat)                   = return ()
red1 f (QBool _ _ OBool)                    = return ()
red1 f (QBool _ _ ONone)                    = return ()

red1 False (QIn _ _ _ OVar{})               = throwError Defer
red1 False (QDot _ _ OVar{} _ _)            = throwError Defer
red1 False (QIx _ _ OVar{} _ _)             = throwError Defer
red1 False (QMod _ _ OVar{} _)              = throwError Defer
red1 False (QPlus _ _ OVar{})               = throwError Defer
red1 False (QNum _ _ OVar{})                = throwError Defer
red1 False (QBool _ _ OVar{})               = throwError Defer
red1 False c                                = throwError $ NoOverload c

red1 True (QIn l _ t u)                     = do v <- newOVar; unify' (ODict t v) u
red1 True c@(QDot _ _ t n u)
  | nstr n `elem` listmeths                 = do v <- newOVar; unify' t (OList v);                          -- temp. fix until backtracking
                                                 c' <- mapsubst c; red1 True c'
  | nstr n `elem` dictmeths                 = do k <- newOVar; v <- newOVar; unify' t (ODict k v);          -- temp. fix until backtracking
                                                 c' <- mapsubst c; red1 True c'
  | nstr n `elem` strmeths                  = do unify' t OStr; c' <- mapsubst c; red1 True c'              -- temp. fix until backtracking
  | otherwise                               = do r <- newOVar; unify' t (ORecord (OKwd n u r))
  where listmeths                           = ["append","clear","copy","extend","insert","pop","remove","reverse","sort"]
        dictmeths                           = ["keys","values","items"]
        strmeths                            = ["capitalize","casefold","center",{-"count",-}"encode","endswith","expandtabs","format",
                                               "format_map","find",{-"index",-}"isalnum","isalpha","isdecimal","isdigit","isidentifier",
                                               "islower","isnumeric","isprintable","isspace","istitle","isupper","join","ljust","lower",
                                               "lstrip","partition","replace","rfind","rindex","rjust","rpartition","rsplit","rstrip",
                                               "split","splitlines","startswith","strip","swapcase",{-"title",-}"translate","upper","zfill"]
red1 True (QIx l _ t OInt u)                = unify' t (OList u)
red1 True (QIx l _ t i u)                   = unify' t (ODict i u)
red1 True (QMod _ _ t u)                    = do unify' t OInt; unify' u OInt
red1 True (QPlus _ _ t)                     = unify' t OInt
red1 True (QNum _ _ t)                      = unify' t OInt
red1 True (QBool _ _ t)                     = unify' t OStr

builtinFun l t u                            = do -- o_dump [GEN l t0]
                                                 t1 <- o_instantiate l $ o_openFX t0
                                                 unify' t1 u
  where t0                                  = OSchema [] [] t


