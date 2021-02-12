{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Acton.Solver where

import Control.Monad
import Control.Monad.Except
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Control.Exception

import Utils
import Pretty
import Acton.Syntax
import Acton.Printer
import Acton.Builtin
import Acton.Names
import Acton.Prim
import Acton.Env
import Acton.Subst
import Acton.TypeM
import Acton.TypeEnv




-- Reduce conservatively and remove entailed constraints
simplify                                    :: (Polarity a, Pretty a) => Env -> TEnv -> a -> Constraints -> TypeM (Constraints,Equations)
simplify env te tt cs                       = do cs <- msubst cs
                                                 te <- msubst te
                                                 --traceM ("  -simplify:\n" ++ render (nest 8 $ vcat $ map pretty cs))
                                                 --traceM ("  -for:\n" ++ render (nest 8 $ vcat $ map pretty te))
                                                 simplify' env te tt [] cs `catchError` \err -> Control.Exception.throw err

simplify'                                   :: (Polarity a, Pretty a) => Env -> TEnv -> a -> Equations -> Constraints -> TypeM (Constraints,Equations)
simplify' env te tt eq []                   = return ([], eq)
simplify' env te tt eq cs                   = do eq1 <- reduce env eq cs
                                                 cs1 <- msubst =<< collectDeferred
                                                 env1 <- msubst env 
                                                 te1 <- msubst te
                                                 tt1 <- msubst tt
                                                 --traceM ("############## env:\n" ++ prstr [ (n,i) | (n,i@NAct{}) <- names env1 ])
                                                 --traceM ("############## cs:\n" ++ render (nest 4 $ vcat (map pretty cs1)) ++ "\n##############")
                                                 improve env1 te1 tt1 eq1 cs1


----------------------------------------------------------------------------------------------------------------------
-- solve
----------------------------------------------------------------------------------------------------------------------

data Rank                                   = RTry { headv :: TVar, alts :: [Type], rev :: Bool }
                                            | RUni { headv :: TVar, alts :: [Type] }
                                            deriving (Show)

instance Eq Rank where
    RTry h1 _ _ == RTry h2 _ _              = h1 == h2
    RUni h1 _   == RUni h2 _                = h1 == h2
    _           == _                        = False

instance Pretty Rank where
    pretty (RTry v ts rev)                  = pretty v <+> braces (commaSep pretty ts) Pretty.<> (if rev then char '\'' else empty)
    pretty (RUni v ts)                      = pretty v <+> char '=' <+> commaSep pretty ts

solve                                       :: (Polarity a, Pretty a) => Env -> (Constraint -> Bool) ->
                                               TEnv -> a -> Equations -> Constraints -> TypeM (Constraints,Equations)
solve env select te tt eq []                = return ([], eq)
solve env select te tt eq cs                = --trace ("\n\n######### solve") $
                                              solve' env select [] te tt eq cs `catchError` \err -> Control.Exception.throw err

solve' env select hist te tt eq cs
  | null solve_cs                           = return (keep_cs, eq)
  | otherwise                               = do st <- currentState
                                                 --traceM ("## keep:\n" ++ render (nest 8 $ vcat $ map pretty keep_cs))
                                                 --traceM ("## solve: " ++ render (nest 8 $ vcat $ map pretty solve_cs))
                                                 --traceM ("## posvs: " ++ prstrs posvs)
                                                 --traceM ("## negvs: " ++ prstrs negvs)
                                                 case head goals of
                                                    RTry v alts False ->
                                                        --trace ("### goal " ++ prstr v ++ ", candidates: " ++ prstrs alts) $
                                                        tryAlts st v alts
                                                    RTry v alts True  ->
                                                        --trace ("### goal " ++ prstr v ++ ", candidates (rev): " ++ prstrs alts) $
                                                        tryAlts st v (alts)
                                                    RUni v alts       ->
                                                        --trace ("### goal " ++ prstr v ++ ", unifying with " ++ prstrs alts) $
                                                        unifyM env (repeat $ tVar v) alts >> proceed hist cs
  where (solve_cs, keep_cs)                 = partition select cs
        goals                               = sortOn deco $ map condense $ group rnks   -- (rnks ++ rnks')
        rnks                                = map (rank env) solve_cs
        rvs                                 = map headv rnks
        rnks'                               = [ rnk | rnk <- map (rank env) keep_cs, headv rnk `elem` rvs ]
        tryAlts st tv []                    = {-trace ("### Out of alternatives for " ++ prstr tv) $ -}noSolve cs
        tryAlts st tv (t:ts)                = tryAlt tv t `catchError` const ({-traceM ("### ROLLBACK " ++ prstr tv) >> -}rollbackState st >> tryAlts st tv ts)
        tryAlt tv (TCon _ c)
          | isProto env (tcname c)          = do p <- instwildcon env c
                                                 w <- newWitness
                                                 --traceM ("  # trying " ++ prstr tv ++ " (" ++ prstr p ++ ")")
                                                 proceed hist (Impl w (tVar tv) p : cs)
        tryAlt tv t                         = do t <- instwild env (tvkind tv) t
                                                 --traceM ("  # trying " ++ prstr tv ++ " = " ++ prstr t)
                                                 substitute tv t
                                                 proceed (t:hist) cs
        proceed hist cs                     = do cs <- msubst cs
                                                 te <- msubst te
                                                 tt <- msubst tt
                                                 (cs,eq) <- simplify' env te tt eq cs
                                                 hist <- msubst hist
                                                 solve' env select hist te tt eq cs
        condense (RTry v as r : rs)         = RTry v (if rev' then subrev ts' else ts') rev'
          where ts                          = foldr intersect as $ map alts rs
                ts'                         = if v `elem` optvs then ts \\ [tOpt tWild] else ts
                rev'                        = (or $ r : map rev rs) || v `elem` posvs
        condense (RUni v as : rs)           = RUni v (foldr union as $ map alts rs)
        optvs                               = optvars cs ++ optvars hist
        embvs                               = embvars cs
        univs                               = univars cs
        (posvs, negvs)                      = polvars te `polcat` polvars tt

        deco (RTry v as r)                  = (0, length $ filter (==v) embvs, length $ filter (==v) univs, length as)
        deco (RUni v as)                    = (1, 0, 0, length as)

        subreverse v ts                     = --trace ("%%% rev rank " ++ prstr v) $
                                              --trace ("  % origin:  " ++ prstrs ts) $
                                              --trace ("  % reverse: " ++ prstrs (reverse ts)) $
                                              --trace ("  % subrev:  " ++ prstrs (subrev ts)) $ 
                                              reverse ts
        subrev []                           = []
        subrev (t:ts)                       = subrev ts1 ++ t : subrev ts2
          where (ts1,ts2)                   = partition (\t' -> castable env t' t) ts

-- subrev [int,Pt,float,CPt,C3Pt]           = [] ++ int : subrev [Pt,float,CPt,C3Pt] 
--                                          = int : subrev [CPt,C3Pt] ++ Pt : subrev [float]
--                                          = int : [C3Pt] ++ CPt ++ subrev [] ++ Pt : [] ++ float : subrev []
--                                          = int : C3Pt : CPt : Pt : float

rank                                        :: Env -> Constraint -> Rank
rank env (Sub _ t1 t2)                      = rank env (Cast t1 t2)
rank env (Cast (TVar _ tv) (TVar _ tv2))
  | univar tv, univar tv2                   = RUni tv [tVar tv2]
rank env (Cast (TVar _ tv) (TOpt _ t2))
  | univar tv                               = RTry tv (allBelow env t2 ++ [tOpt tWild, tNone]) False
rank env (Cast TNone{} (TVar _ tv))
  | univar tv                               = RTry tv [tOpt tWild, tNone] True
rank env (Cast (TVar _ tv) t2)
  | univar tv                               = RTry tv (allBelow env t2) False
rank env (Cast t1 (TVar _ tv))
  | univar tv                               = RTry tv (allAbove env t1) True
rank env (Impl _ (TVar _ tv) p)
  | univar tv                               = RTry tv (allExtProto env p) False
rank env (Sel _ (TVar _ tv) n _)
  | univar tv                               = RTry tv (allConAttr env n ++ allProtoAttr env n ++ allExtProtoAttr env n) False
rank env (Mut (TVar _ tv) n _)
  | univar tv                               = RTry tv (allConAttr env n `intersect` allBelow env tObject) False


class OptVars a where
    optvars                             :: a -> [TVar]

instance (OptVars a) => OptVars [a] where
    optvars                             = concat . map optvars

instance OptVars Constraint where
    optvars (Cast t1 t2)               = optvars [t1, t2]
    optvars (Sub w t1 t2)              = optvars [t1, t2]
    optvars (Impl w t p)               = optvars t ++ optvars p
    optvars (Sel w t1 n t2)            = optvars [t1, t2]
    optvars (Mut t1 n t2)              = optvars [t1, t2]

instance OptVars Type where
    optvars (TOpt _ (TVar _ v))        = [v]
    optvars (TOpt _ t)                 = optvars t
    optvars (TCon _ c)                 = optvars c
    optvars (TFun _ fx p k t)          = optvars [p, k, t]
    optvars (TTuple _ p k)             = optvars [p, k]
    optvars (TRow _ _ _ t r)           = optvars [t, r]
    optvars _                          = []

instance OptVars TCon where
    optvars (TC n ts)                   = optvars ts

embvars cs                              = concat $ map emb cs
  where emb (Cast (TVar _ v) (TVar _ v'))
          | univar v && univar v'       = []
        emb (Cast (TVar _ v) t)
          | univar v                    = tyfree t
        emb (Cast t (TVar _ v))
          | univar v                    = tyfree t
        emb (Sub _ (TVar _ v) (TVar _ v'))
          | univar v && univar v'       = []
        emb (Sub _ (TVar _ v) t)
          | univar v                    = tyfree t
        emb (Sub _ t (TVar _ v))
          | univar v                    = tyfree t
        emb (Impl _ (TVar _ v) p)
          | univar v                    = tyfree p
        emb (Sel _ (TVar _ v) n t)
          | univar v                    = tyfree t
        emb (Mut (TVar _ v) n t)
          | univar v                    = tyfree t
        emb _                           = []

univars cs                              = concat $ map uni cs
  where uni (Cast (TVar _ v) (TVar _ v'))
          | univar v && univar v'       = [v,v']
        uni (Sub _ (TVar _ v) (TVar _ v'))
          | univar v && univar v'       = [v,v']
        uni _                           = []

allAbove env (TCon _ tc)                = tOpt tWild : map tCon tcons
  where n                               = unalias env (tcname tc)
        tcons                           = allAncestors env tc ++ [tc]
allAbove env (TVar _ tv)
  | not $ univar tv                     = [tOpt tWild, tCon tc, tVar tv]
  where tc                              = findTVBound env tv
allAbove env (TOpt _ t)                 = [tOpt tWild]
allAbove env (TNone _)                  = [tOpt tWild, tNone]
allAbove env (TFun _ _ _ _ _)           = [tOpt tWild, tFun tWild tWild tWild tWild]
allAbove env (TTuple _ _ _)             = [tOpt tWild, tTuple tWild tWild]
allAbove env (TRow _ k n _ _)           = [tRow k n tWild tWild]
allAbove env (TNil _ k)                 = [tNil k]
allAbove env (TFX _ FXAction)           = [fxAction]
allAbove env (TFX _ FXMut)              = [fxAction, fxMut]
allAbove env (TFX _ FXPure)             = [fxAction, fxMut, fxPure]

allBelow env (TCon _ tc)                = map tCon $ tc : allDescendants env tc
allBelow env (TVar _ tv)                = [tVar tv]
allBelow env (TOpt _ t)                 = tOpt tWild : allBelow env t ++ [tNone]
allBelow env (TNone _)                  = [tNone]
allBelow env (TFun _ _ _ _ _)           = [tFun tWild tWild tWild tWild]
allBelow env (TTuple _ _ _)             = [tTuple tWild tWild]
allBelow env (TRow _ k n _ _)           = [tRow k n tWild tWild]
allBelow env (TNil _ k)                 = [tNil k]
allBelow env (TFX _ FXAction)           = [fxAction, fxMut, fxPure]
allBelow env (TFX _ FXMut)              = [fxMut, fxPure]
allBelow env (TFX _ FXPure)             = [fxPure]


allExtProto env p                       = [ tVar tv | tv <- tvarScope0 env, wit <- allWitnesses env (NoQ $ tvname tv), implProto env p wit ] ++
                                          [ tCon tc | tc <- allCons env, wit <- allWitnesses env (tcname tc), implProto env p wit ]

allConAttr env n                        = [ tCon tc | tc <- allCons env, n `elem` allAttrs env tc ]

allProtoAttr env n                      = [ tCon p | p <- allProtos env, n `elem` allAttrs env p ]

allExtProtoAttr env n                   = [ tCon tc | tc <- allCons env, any ((n `elem`) . allAttrs env . proto) (allWitnesses env $ tcname tc) ]


----------------------------------------------------------------------------------------------------------------------
-- reduce
----------------------------------------------------------------------------------------------------------------------

type Equation                               = (Name, Type, Expr)
type Equations                              = [Equation]

instance Subst Equation where
    msubst (w, t, e)                        = do t <- msubst t
                                                 e <- msubst e
                                                 return (w, t, e)
    
    tyfree (w, t, e)                        = tyfree t ++ tyfree e


reduce                                      :: Env -> Equations -> Constraints -> TypeM Equations
reduce env eq []                            = return eq
reduce env eq (c:cs)                        = do c' <- msubst c
                                                 --traceM ("   reduce " ++ prstr c')
                                                 eq1 <- reduce' env eq c'
                                                 cs' <- msubst cs
                                                 reduce env eq1 cs'

reduce'                                     :: Env -> Equations -> Constraint -> TypeM Equations
reduce' env eq (Cast t1 t2)                 = do cast' env t1 t2
                                                 return eq

reduce' env eq (Sub w t1 t2)                = sub' env eq w t1 t2

reduce' env eq c@(Impl w t@(TVar _ tv) p)
  | univar tv                               = do defer [c]; return eq
  | Just wit <- witSearch                   = do (cs,p',we) <- instWitness env [] wit
                                                 unifyM env (tcargs p) (tcargs p')
                                                 reduce env ((w, impl2type t p, we):eq) cs
  where witSearch                           = findWitness env (NoQ $ tvname tv) (implProto env p)
  
reduce' env eq c@(Impl w t@(TCon _ tc) p)
  | Just wit <- witSearch                   = do (cs,p',we) <- instWitness env (tcargs tc) wit
                                                 unifyM env (tcargs p) (tcargs p')
                                                 reduce env ((w, impl2type t p, we):eq) cs
  where witSearch                           = findWitness env (tcname tc) (implProto env p)

reduce' env eq c@(Impl w t@(TOpt _ t') p)
  | qualEq env (tcname p) qnIdentity        = do let e = eCall (tApp (eQVar primIdentityOpt) [t']) []
                                                 return ((w, impl2type t p, e):eq)
  | qualEq env (tcname p) qnEq              = do w' <- newWitness
                                                 let e = eCall (tApp (eQVar primEqOpt) [t']) [eVar w']
                                                 reduce env ((w, impl2type t p, e):eq) [Impl w' t' p]

reduce' env eq c@(Impl w t@(TNone _) p)
  | qualEq env (tcname p) qnIdentity        = return ((w, impl2type t p, eQVar primWIdentityNone):eq)
  | qualEq env (tcname p) qnEq              = return ((w, impl2type t p, eQVar primWEqNone):eq)

reduce' env eq c@(Sel w (TVar _ tv) n _)
  | univar tv                               = do defer [c]; return eq
  | Just wsc <- attrSearch                  = do (eq',cs) <- solveSelAttr env wsc c
                                                 reduce env (eq'++eq) cs
  | Just wit <- witSearch                   = do (eq',cs) <- solveSelWit env wit c
                                                 reduce env (eq'++eq) cs
  | otherwise                               = tyerr n "Attribute not found"
  where attrSearch                          = findTVAttr env tv n
        witSearch                           = findWitness env (NoQ $ tvname tv) (hasAttr env n)

reduce' env eq c@(Sel w (TCon _ tc) n _)
  | Just wsc <- attrSearch                  = do (eq',cs) <- solveSelAttr env wsc c
                                                 reduce env (eq'++eq) cs
  | Just wit <- witSearch                   = do (eq',cs) <- solveSelWit env wit c
                                                 reduce env (eq'++eq) cs
  | otherwise                               = tyerr n "Attribute not found"
  where attrSearch                          = findAttr env tc n
        witSearch                           = findWitness env (tcname tc) (hasAttr env n)

reduce' env eq (Sel w t1@(TTuple _ p r) n t2)
                                            = do let e = eLambda [(px0,t1)] (eDot (eVar px0) n)
                                                     cs' = [Cast r (kwdRow n t2 tWild)]
                                                 reduce env ((w, wFun t1 t2, e):eq) cs'

reduce' env eq c@(Mut (TVar _ tv) n _)
  | univar tv                               = do defer [c]; return eq
  | Just wsc <- attrSearch                  = do cs <- solveMutAttr env wsc c
                                                 reduce env eq cs
  | otherwise                               = tyerr n "Attribute not found:"
  where attrSearch                          = findTVAttr env tv n

reduce' env eq c@(Mut (TCon _ tc) n _)
  | Just wsc <- attrSearch                  = do cs <- solveMutAttr env wsc c
                                                 reduce env eq cs
  | otherwise                               = tyerr n "Attribute not found:"
  where attrSearch                          = findAttr env tc n

reduce' env eq c                            = noRed c


solveSelAttr env (wf,sc,d) (Sel w t1 n t2)  = do (cs1,tvs,t) <- instantiate env sc
                                                 when (tvSelf `elem` snd (polvars t)) (tyerr n "Contravariant Self attribute not selectable by instance")
                                                 let e = eLambda [(px0,t1)] (app t (tApp (eDot (wf $ eVar px0) n) tvs) $ witsOf cs1)
                                                     cs = Cast (subst [(tvSelf,t1)] t) t2 : cs1
                                                 return ([(w, wFun t1 t2, e)], cs)

solveSelWit env wit (Sel w t1 n t2)         = do let ts = case t1 of TCon _ c -> tcargs c; _ -> []
                                                 (cs1,p,we) <- instWitness env ts wit
                                                 let Just (wf,sc,d) = findAttr env p n
                                                 (cs2,tvs,t) <- instantiate env sc
                                                 when (tvSelf `elem` snd (polvars t)) (tyerr n "Contravariant Self attribute not selectable by instance")
                                                 let e = eLambda [(px0,t1)] (app t (tApp (eDot (wf we) n) tvs) $ eVar px0 : witsOf cs2)
                                                     cs = Cast (subst [(tvSelf,t1)] t) t2 : cs1 ++ cs2
                                                 return ([(w, wFun t1 t2, e)], cs)

solveMutAttr env (wf,sc,dec) (Mut t1 n t2)  = do when (dec /= Just Property) (noMut n)
                                                 let TSchema _ [] t = sc
                                                     cs = [Cast t2 (subst [(tvSelf,t1)] t)]
                                                 return cs

----------------------------------------------------------------------------------------------------------------------
-- cast
----------------------------------------------------------------------------------------------------------------------

cast                                        :: Env -> Type -> Type -> TypeM ()
cast env t1 t2                              = do t1' <- msubst t1
                                                 t2' <- msubst t2
                                                 --traceM ("   cast " ++ prstr t1 ++ " < " ++ prstr t2)
                                                 cast' env t1' t2'

castM env ts1 ts2                           = mapM_ (uncurry $ cast env) (ts1 `zip` ts2)


cast' env (TWild _) t2                      = return ()
cast' env t1 (TWild _)                      = return ()

cast' env (TCon _ c1) (TCon _ c2)
  | Just (wf,c') <- search                  = unifyM env (tcargs c') (tcargs c2)        -- TODO: cast/unify based on polarities
  where search                              = findAncestor env c1 (tcname c2)

cast' env (TFun _ fx1 p1 k1 t1) (TFun _ fx2 p2 k2 t2)
                                            = do cast env fx1 fx2
                                                 cast env p2 p1
                                                 cast env k2 k1
                                                 cast env t1 t2

cast' env (TTuple _ p1 k1) (TTuple _ p2 k2)
                                            = do cast env p1 p2
                                                 cast env k1 k2

cast' env (TOpt _ t1@TOpt{}) t2             = cast env t1 t2
cast' env t1 (TOpt _ t2@TOpt{})             = cast env t1 t2
cast' env (TOpt _ t1) (TOpt _ t2)           = cast env t1 t2
cast' env (TVar _ tv) t2@TNone{}            = do substitute tv tNone
                                                 cast env tNone t2
cast' env t1@TOpt{} (TVar _ tv)             = do t2 <- instwild env KType $ tOpt tWild
                                                 substitute tv t2
                                                 cast env t1 t2
cast' env t1 (TOpt _ t2)
  | t1 == t2                                = return ()
cast' env (TNone _) (TOpt _ t)              = return ()
cast' env (TNone _) (TNone _)               = return ()

cast' env (TFX _ fx1) (TFX _ fx2)
  | castFX fx1 fx2                          = return ()
  where castFX FXPure   FXPure              = True
        castFX FXPure   FXMut               = True
        castFX FXPure   FXAction            = True
        castFX FXMut    FXMut               = True
        castFX FXMut    FXAction            = True
        castFX FXAction FXAction            = True
        castFX fx1      fx2                 = False

cast' env (TNil _ k1) (TNil _ k2)
  | k1 == k2                                = return ()
cast' env (TVar _ tv) r2@(TNil _ k)         = do substitute tv (tNil k)
                                                 cast env (tNil k) r2
cast' env r1 (TRow _ k n t2 r2)             = do (t1,r1') <- findElem k (tNil k) n r1 (rowTail r2)
                                                 cast env t1 t2
                                                 cast env r1' r2
cast' env (TRow _ k n t1 r1) r2             = do (t2,r2') <- findElem k (tNil k) n r2 (rowTail r1)
                                                 cast env t1 t2
                                                 cast env r1 r2'

cast' env (TVar _ tv) t2@TFun{}
  | univar tv                               = do t1 <- instwild env KType $ tFun tWild tWild tWild tWild
                                                 substitute tv t1
                                                 cast env t1 t2
cast' env t1@TFun{} (TVar _ tv)                                                                             -- Should remove this, rejects tv = TOpt...
  | univar tv                               = do t2 <- instwild env KType $ tFun tWild tWild tWild tWild
                                                 substitute tv t2
                                                 cast env t1 t2

cast' env (TVar _ tv) t2@TTuple{}
  | univar tv                               = do t1 <- instwild env KType $ tTuple tWild tWild
                                                 substitute tv t1
                                                 cast env t1 t2

cast' env (TVar _ tv1) (TVar _ tv2)
  | tv1 == tv2                              = return ()

cast' env t1@(TVar _ tv) t2
  | univar tv                               = defer [Cast t1 t2]

cast' env t1 t2@(TVar _ tv)
  | univar tv                               = defer [Cast t1 t2]

cast' env t1@(TVar _ tv) t2                 = cast' env (tCon tc) t2
  where tc                                  = findTVBound env tv

cast' env t1 t2@(TVar _ tv)                 = noRed (Cast t1 t2)

cast' env t1 (TOpt _ t2)                    = cast env t1 t2                -- Only matches when t1 is NOT a variable

cast' env t1 t2                             = noRed (Cast t1 t2)


----------------------------------------------------------------------------------------------------------------------
-- unify
----------------------------------------------------------------------------------------------------------------------

unify                                       :: Env -> Type -> Type -> TypeM ()
unify env t1 t2                             = do t1' <- msubst t1
                                                 t2' <- msubst t2
                                                 --traceM ("  #unify " ++ prstr t1' ++ " and " ++ prstr t2')
                                                 unify' env t1' t2'

unifyM env ts1 ts2                          = mapM_ (uncurry $ unify env) (ts1 `zip` ts2)


unify' env (TWild _) t2                     = return ()
unify' env t1 (TWild _)                     = return ()

unify' env (TCon _ c1) (TCon _ c2)
  | qualEq env (tcname c1) (tcname c2)      = unifyM env (tcargs c1) (tcargs c2)

unify' env (TFun _ fx1 p1 k1 t1) (TFun _ fx2 p2 k2 t2)
                                            = do unify env fx1 fx2
                                                 unify env p2 p1
                                                 unify env k2 k1
                                                 unify env t1 t2

unify' env (TTuple _ p1 k1) (TTuple _ p2 k2)
                                            = do unify env p1 p2
                                                 unify env k1 k2

unify' env (TOpt _ t1) (TOpt _ t2)          = unify env t1 t2
unify' env (TNone _) (TNone _)              = return ()

unify' env (TFX _ fx1) (TFX _ fx2)
  | fx1 == fx2                              = return ()

unify' env (TNil _ k1) (TNil _ k2)
  | k1 == k2                                = return ()
unify' env r1 (TRow _ k n t2 r2)            = do (t1,r1') <- findElem k (tNil k) n r1 (rowTail r2)
                                                 unify env t1 t2
                                                 unify env r1' r2

unify' env (TVar _ tv1) (TVar _ tv2)
  | tv1 == tv2                              = return ()

unify' env (TVar _ tv) t2
  | univar tv                               = do when (tv `elem` tyfree t2) (infiniteType tv)
                                                 substitute tv t2
unify' env t1 (TVar _ tv)
  | univar tv                               = do when (tv `elem` tyfree t1) (infiniteType tv)
                                                 substitute tv t1

unify' env t1 t2                            = noUnify t1 t2


----------------------------------------------------------------------------------------------------------------------
-- sub
----------------------------------------------------------------------------------------------------------------------

sub                                         :: Env -> Equations -> Name -> Type -> Type ->TypeM Equations
sub env eq w t1 t2                          = do t1' <- msubst t1
                                                 t2' <- msubst t2
                                                 sub' env eq w t1' t2'

sub'                                        :: Env -> Equations -> Name -> Type -> Type ->TypeM Equations

sub' env eq w t1@TWild{} t2                 = return (idwit w t1 t2 : eq)
sub' env eq w t1 t2@TWild{}                 = return (idwit w t1 t2 : eq)

--                as declared               as called
--                existing                  expected
sub' env eq w t1@(TFun _ fx1 p1 k1 t1') t2@(TFun _ fx2 p2 k2 t2')                   -- TODO: implement pos/kwd argument shifting
                                            = do wp <- newWitness
                                                 wk <- newWitness
                                                 wt <- newWitness
                                                 let e = eLambda [(px0,t1)] e'
                                                     e' = Lambda l0 (PosSTAR px1 $ Just $ tTupleP p2) (KwdSTAR px2 $ Just $ tTupleK k2) e0 fx2
                                                     e0 = eCall (eVar wt) [Call l0 (eVar px0) (PosStar e1) (KwdStar e2)]
                                                     e1 = eCall (eVar wp) [eVar px1]
                                                     e2 = eCall (eVar wk) [eVar px2]
                                                     cs = [Cast fx1 fx2, Sub wp p2 p1, Sub wk k2 k1, Sub wt t1' t2']

                                                 reduce env ((w, wFun t1 t2, e):eq) cs

--                existing            expected
sub' env eq w t1@(TTuple _ p1 k1) t2@(TTuple _ p2 k2)                               -- TODO: implement pos/kwd argument shifting
                                            = do wp <- newWitness
                                                 wk <- newWitness
                                                 let e = eLambda [(px0,t1)] (Paren l0 $ Tuple l0 (PosStar e1) (KwdStar e2))
                                                     e1 = eCall (eVar wp) [Paren l0 $ Tuple l0 (PosStar $ eVar px0) KwdNil]
                                                     e2 = eCall (eVar wk) [Paren l0 $ Tuple l0 PosNil (KwdStar $ eVar px0)]
                                                     cs = [Sub wp p1 p2, Sub wk k1 k2]
                                                 reduce env ((w, wFun t1 t2, e):eq) cs

-- Note: a sub-row constraint R1 < R2 is witnessed by a lambda of type
-- (*(R1))->(*(R2)) or (**(R1))->(**(R2)), depending on the row kind

sub' env eq w r1@(TNil _ k1) r2@(TNil _ k2)
  | k1 == k2                                = return (idwit w tUnit tUnit : eq)

--          existing     expected                Match labels in the order of the expected row
sub' env eq w r1     r2@(TRow _ k n t2 r2') = do (t1,r1') <- findElem k (tNil k) n r1 (rowTail r2)
                                                 wt <- newWitness
                                                 wr <- newWitness
                                                 let e = rowWit k w n t1 r1' wt wr
                                                     cs = [Sub wt t1 t2, Sub wr r1' r2']
                                                 reduce env ((w, rowFun k r1 r2, e):eq) cs
sub' env eq w r1@(TRow _ k n t1 r1') r2     = do (t2,r2') <- findElem k (tNil k) n r2 (rowTail r1)
                                                 wt <- newWitness
                                                 wr <- newWitness
                                                 let e = rowWit k w n t2 r2' wt wr
                                                     cs = [Sub wt t1 t2, Sub wr r1' r2']
                                                 reduce env ((w, rowFun k r1 r2, e):eq) cs

sub' env eq w (TVar _ tv) t2@TFun{}
  | univar tv                               = do t1 <- instwild env KType $ tFun tWild tWild tWild tWild
                                                 substitute tv t1
                                                 sub env eq w t1 t2
sub' env eq w t1@TFun{} (TVar _ tv)                                                                             -- Should remove this, rejects tv = TOpt...
  | univar tv                               = do t2 <- instwild env KType $ tFun tWild tWild tWild tWild
                                                 substitute tv t2
                                                 sub env eq w t1 t2


sub' env eq w (TVar _ tv) t2@TTuple{}
  | univar tv                               = do t1 <- instwild env KType $ tTuple tWild tWild
                                                 substitute tv t1
                                                 sub env eq w t1 t2

sub' env eq w t1@(TVar _ tv1) t2@(TVar _ tv2)
  | tv1 == tv2                              = do return (idwit w t1 t2 : eq)
  | univar tv1 && univar tv2                = do defer [Sub w t1 t2]; return eq

sub' env eq w t1 t2                         = do cast env t1 t2
                                                 return (idwit w t1 t2 : eq)

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
          | not $ univar tv                 = kwdNotFound n
          | otherwise                       = do t <- newTVar
                                                 r <- newTVarOfKind k
                                                 substitute tv (tRow k n t r)
                                                 return (t, revApp r0 r)
        findElem' e0 n r2 tl                = noUnify r2 (tRow k n tWild tWild)
        revApp (TRow l k n t r1) r2         = revApp r1 (TRow l k n t r2)
        revApp (TNil _ _) r2                = r2


{-

round : (Real, ?int) -> Real
----
w(round)(3.14, None)                                                    w(round)(3.14, None)
w : ((Real,?int)->Real) -> (float,None)->$1
----
w = lambda x: lambda *x1,**x2: wt(x(*wp(x1), **wk(x2)))                 = (lambda x: lambda *x1,**x2: wt(x(*wp(x1), **wk(x2))))(round)(3.14, None)          | x=round
wt : (Real) -> $1                                                       = lambda *x1,**x2: wt(round(*wp(x1), **wk(x2)))(3.14, None)                         | x1=(3.14,None), x2=()
wp : ((float,None)) -> (Real,?int)                                      = wt(round(*wp((3.14,None)), **wk(())))
wk : (()) -> ()
----
wt = lambda x: x                                                        = (lambda x: x)(round(*wp((3.14,None)), **wk(())))                                  | x=round(...)
wp = lambda x: (w1(x.0), *w2(x.*1))                                     = round(*(lambda x: (w1(x.0), *w2(x.*1)))((3.14,None)), **wk(()))                   | x=(3.14, None)
wk = lambda y: ()                                                       = round(*(w1(3.14), *w2((None,))), **((lambda y: ())()))                            | y=()
w1 : (float) -> Real                                                    = round(*(w1(3.14), *w2((None,))), **())
w2 : ((None,)) -> (?int,)
----
w1 = lambda x: PACK(Real$float, x)                                      = round(*((lambda x: PACK(Real$float, x))(3.14), *w2((None,))))                     | x=3.14
w2 = lambda x: (w21(x.0), *w22(x.*1))                                   = round(*(PACK(Real$float, 3.14), *(lambda x: (w21(x.0), *w22(x.*1)))((None,))))    | x=(None,)
w21 : (None) -> ?int                                                    = round(*(PACK(Real$float, 3.14), *(w21(None), *w22(()))))
w22 : (()) -> ()
----
w21 = lambda x: x                                                       = round(*(PACK(Real$float, 3.14), *((lambda x: x)(None), *w22(()))))                | x=None
w22 = lambda y: ()                                                      = round(*(PACK(Real$float, 3.14), *(None, *(lambda y: ())())))                      | y=()
                                                                        = round(*(PACK(Real$float, 3.14), *(None, *())))
                                                                        = round(*(PACK(Real$float, 3.14), *(None,)))
                                                                        = round(*(PACK(Real$float, 3.14), None))
                                                                        = round(PACK(Real$float, 3.14), None)



round : (Real, ?int) -> Real
----
w(round)(3.14, None)                                                    w(round)(3.14, None)
w : ((Real,?int)->Real) -> (float,None)->$1
----
w = lambda x0: lambda *x1,**x2: wt(x0(*wp(*x1), **wk(**x2)))            = (lambda x0: lambda *x1,**x2: wt(x0(*wp(*x1), **wk(**x2))))(round)(3.14, None)     | x0=round
wt : (Real) -> $1                                                       = lambda *x1,**x2: wt(round(*wp(*x1), **wk(**x2)))(3.14, None)                      | x1=(3.14,None), x2=()
wp : (float,None) -> (Real,?int)                                        = wt(round(*wp(3.14,None), **wk()))
wk : () -> ()
----
wt = lambda x0: x0                                                      = (lambda x0: x0)(round(*wp((3.14,None)), **wk()))                                  | x0=round(...)
wp = lambda x1,*x2: (w1(x1), *w2(*x2))                                  = round(*(lambda x1,*x2: (w1(x1), *w2(*x2)))((3.14,None)), **(lambda: ())()))       | x1=3.14, x2=(None,)
wk = lambda: ()                                                         = round(*(w1(3.14), *w2(*(None,))), **())
w1 : (float) -> Real                                                    = round(*(w1(3.14), *w2(None,)))
w2 : (None,) -> (?int,)
----
w1 = lambda x0: PACK(Real$float, x0)                                    = round(*((lambda x0: PACK(Real$float, x0))(3.14), *w2(None)))                      | x0=3.14
w2 = lambda x1,*x2: (w21(x1), *w22(*x2))                                = round(*(PACK(Real$float, 3.14), *(lambda x1,*x2: (w21(x1), *w22(*x2)))(None)))    | x1=None, x2=()
w21 : (None) -> ?int                                                    = round(*(PACK(Real$float, 3.14), *(w21(None), *w22())))
w22 : () -> ()
----
w21 = lambda x0: x0                                                     = round(*(PACK(Real$float, 3.14), *((lambda x0: x0)(None), *w22())))                | x0=None
w22 = lambda: ()                                                        = round(*(PACK(Real$float, 3.14), *(None, *(lambda: ())())))
                                                                        = round(*(PACK(Real$float, 3.14), *(None, *())))
                                                                        = round(*(PACK(Real$float, 3.14), *(None,)))
                                                                        = round(*(PACK(Real$float, 3.14), None))
                                                                        = round(PACK(Real$float, 3.14), None)


------------------------------------------

round : (x:Real, n:?int) -> Real                                        round(n=None,x=3.14)
----
w(round)(n=None, x=3.14)                                                w(round)(n=None,x=3.14)
w : ((x:Real,n:?int)->Real) -> (n:None,x:float)->$1
----
w = lambda x0: lambda **x1: wt(x0(**wr(**x1)))                          = (lambda x0: lambda **x1: wt(x0(**wr(**x1))))(round)(n=None,x=3.14)            | x0=round
wt : (Real) -> $1                                                       = (lambda **x1: wt(round(**wr(**x1))))(n=None,x=3.14)                           | x1=(n=None,x=3.14))
wr : (n:None,x:float) -> (x:Real,n:?int)                                = wt(round(**wr(n=None,x=3.14)))
----
wt = lambda x0: x0                                                      = (lambda x0: x0 )(round(**wr(n=None,x=3.14)))                                  | x0=round(...)
wr = lambda x,**x2: (x=w1(x), **w2(**x2))                               = round(**(lambda x,**x2: (x=w1(x), **w2(**x2)))(n=None,x=3.14))                | x=3.14, x2=(n=None,)
w1 : (float) -> Real                                                    = round(**(x=w1(3.14), **w2(n=None,)))
w2 : (n:None,) -> (n:?int,)
----
w1 = lambda x0: PACK(Real$float, x0)                                    = round(**(x=(lambda x0: PACK(Real$float, x0))(3.14), **w2(n=None,)))             | x0=3.14
w2 = lambda n, **x2: (n=w21(n), **w22(**x2))                            = round(**(x=PACK(Real$float, 3.14), **(lambda n, **x2: (n=w21(n), **w22(**x2)))(n=None,)))
w21 : (None) -> ?int                                                    = round(**(x=PACK(Real$float, 3.14), **(n=w21(None), **w22(**()))))
w22 : () -> ()
----
w21 = lambda x0: x0                                                     = round(**(x=PACK(Real$float, 3.14), **(n=None, **())))
w22 = lambda: ()                                                        = round(**(x=PACK(Real$float, 3.14), **(n=None)))
                                                                        = round(**(x=PACK(Real$float, 3.14), n=None))
                                                                        = round(x=PACK(Real$float, 3.14), n=None)
-}

----------------------------------------------------------------------------------------------------------------------
-- Variable info
----------------------------------------------------------------------------------------------------------------------

data VInfo                                  = VInfo {
                                                varvars     :: [(TVar,TVar)],
                                                embedded    :: [TVar],
                                                ubounds     :: Map TVar [Type], 
                                                lbounds     :: Map TVar [Type], 
                                                pbounds     :: Map TVar [(Name,TCon)],
                                                mutattrs    :: Map TVar [Name],
                                                selattrs    :: Map TVar [Name] }

varvar v1 v2 vi                             = vi{ varvars = (v1,v2) : varvars vi }
embed vs vi                                 = vi{ embedded = vs ++ embedded vi }
ubound v t vi                               = vi{ ubounds = Map.insertWith (++) v [t] (ubounds vi) }
lbound v t vi                               = vi{ lbounds = Map.insertWith (++) v [t] (lbounds vi) }
pbound v w p vi                             = vi{ pbounds = Map.insertWith (++) v [(w,p)] (pbounds vi) }
mutattr v n vi                              = vi{ mutattrs = Map.insertWith (++) v [n] (mutattrs vi) }
selattr v n vi                              = vi{ selattrs = Map.insertWith (++) v [n] (selattrs vi) }

lookup' v m                                 = maybe [] id $ Map.lookup v m

varinfo cs                                  = f cs (VInfo [] [] Map.empty Map.empty Map.empty Map.empty Map.empty)
  where
    f (Cast (TVar _ v1) (TVar _ v2) : cs)
      | v1 == v2                            = f cs
      | otherwise                           = f cs . varvar v1 v2
    f (Cast (TVar _ v) t : cs)              = f cs . ubound v t . embed (tyfree t)
    f (Cast t (TVar _ v) : cs)              = f cs . lbound v t . embed (tyfree t)
    f (Sub _ (TVar _ v1) (TVar _ v2) : cs)  = f cs . varvar v1 v2
    f (Sub _ (TVar _ v) t : cs)             = f cs . ubound v t . embed (tyfree t)
    f (Sub _ t (TVar _ v) : cs)             = f cs . lbound v t . embed (tyfree t)
    f (Impl w (TVar _ v) p : cs)            = f cs . pbound v w p . embed (tyfree p)
    f (Mut (TVar _ v) n t : cs)             = f cs . mutattr v n . embed (tyfree t)
    f (Sel _ (TVar _ v) n t : cs)           = f cs . selattr v n . embed (tyfree t)
    f []                                    = Just
    f (_ : cs)                              = \_ -> Nothing

varclose xys                                = clos [] xys
  where clos cl []                          = Right cl
        clos cl ((x,y):xys)
          | (x,y) `elem` cl                 = clos cl xys
          | not $ null common               = Left (x,y:common)
          | otherwise                       = clos ((x,y):cl)  (new_below++new_above++xys)
          where below_x                     = below x cl
                above_y                     = above y cl
                common                      = below_x `intersect` above_y
                new_below                   = [ (w,y) | w <- below_x ]
                new_above                   = [ (x,v) | v <- above_y ]

below x cl                                  = [ v | (v,w) <- cl, w==x ]
above y cl                                  = [ w | (v,w) <- cl, v==y ]

gsimp cl obs []                         = []
gsimp cl obs ((x,y):xys)
  | not subsumed                        = gsimp cl obs xys
  | x_obs && y_obs                      = gsimp cl obs xys
  | x_obs                               = (x,y) : gsimp cl (y:obs) xys
  | y_obs                               = (x,y) : gsimp cl (x:obs) xys
  | otherwise                           = (x,y) : gsimp cl obs xys
  where subsumed                        = (above x cl \\ [y]) `eq` above y cl && (below y cl \\ [x]) `eq` below x cl
        a `eq` b                        = all (`elem` b) a && all (`elem` a) b
        x_obs                           = x `elem` obs
        y_obs                           = y `elem` obs

instwild env k (TWild _)                = newTVarOfKind k
instwild env _ (TFun l e p k t)         = TFun l <$> instwild env KFX e <*> instwild env PRow p <*> instwild env KRow k <*> instwild env KType t
instwild env _ (TTuple l p k)           = TTuple l <$> instwild env PRow p <*> instwild env KRow k
instwild env _ (TOpt l t)               = TOpt l <$> instwild env KType t
instwild env _ (TCon l c)               = TCon l <$> instwildcon env c
instwild env _ (TRow l k n t r)         = TRow l k n <$> instwild env KType t <*> instwild env k r
instwild env k t                        = return t

instwildcon env c                       = case tconKind (tcname c) env of
                                            KFun ks _ -> TC (tcname c) <$> sequence [ instwild env k t | (k,t) <- ks `zip` tcargs c ]
                                            _ -> return $ TC (tcname c) []

----------------------------------------------------------------------------------------------------------------------
-- GLB
----------------------------------------------------------------------------------------------------------------------

mkGLB                                   :: Env -> (TVar,[Type]) -> TypeM (TVar,Type)
mkGLB env (v,ts)                        = do t <- instwild env KType $ foldr1 (glb env) ts
                                             --traceM ("   glb " ++ prstrs ts ++ " = " ++ prstr t)
                                             return (v, t)

glb env (TWild _) t2                    = t2
glb env t1 (TWild _)                    = t1

glb env TVar{} _                        = tWild        -- (Might occur in recursive calls)
glb env _ TVar{}                        = tWild        -- (Might occur in recursive calls)

glb env (TCon _ c1) (TCon _ c2)
  | qualEq env (tcname c1) (tcname c2)  = tCon c1
  | hasAncestor env c1 c2               = tCon c1
  | hasAncestor env c2 c1               = tCon c2

glb env (TFun _ e1 p1 k1 t1) (TFun _ e2 p2 k2 t2)                                           -- tWilds instead of glbs enable the special
                                        = tFun tWild (lub env p1 p2) (lub env k1 k2) tWild  -- async rules in sub and cast
glb env (TTuple _ p1 k1) (TTuple _ p2 k2)
                                        = tTuple (glb env p1 p2) (glb env k1 k2)

glb env (TOpt _ t1) (TOpt _ t2)         = tOpt (glb env t1 t2)
glb env (TNone _) t2                    = tNone
glb env t1 (TNone _)                    = tNone
glb env (TOpt _ t1) t2                  = glb env t1 t2
glb env t1 (TOpt _ t2)                  = glb env t1 t2

glb env t1@(TFX _ fx1) t2@(TFX _ fx2)   = tTFX (glfx fx1 fx2)
  where glfx FXPure   _                 = FXPure
        glfx _        FXPure            = FXPure
        glfx FXMut    _                 = FXMut
        glfx _        FXMut             = FXMut
        glfx FXAction FXAction          = FXAction

glb env (TNil _ k1) (TNil _ k2)
  | k1 == k2                            = tNil k1
glb env (TRow _ k n t1 r1) r
  | Just (t2,r2) <- findInRow n r       = tRow k n (glb env t1 t2) (glb env r1 r2)

glb env t1 t2                           = -- noGLB t1 t1
                                          error ("No common subtype: " ++ prstr t1 ++ " and " ++ prstr t2)
    
noGLB t1 t2                             = tyerr t1 ("No common subtype: " ++ prstr t2)

isStr env (TCon _ c)                    = qualEq env (tcname c) qnStr



----------------------------------------------------------------------------------------------------------------------
-- LUB
----------------------------------------------------------------------------------------------------------------------

mkLUB env (v,ts)                        = do --traceM ("   lub " ++ prstrs ts ++ " ...")
                                             t <- instwild env KType $ foldr1 (lub env) ts
                                             --traceM ("   lub " ++ prstrs ts ++ " = " ++ prstr t)
                                             return (v, t)

lub env (TWild _) t2                    = t2
lub env t1 (TWild _)                    = t1

lub env TVar{} _                        = tWild        -- (Might occur in recursive calls)
lub env _ TVar{}                        = tWild        -- (Might occur in recursive calls)

lub env (TCon _ c1) (TCon _ c2)
  | qualEq env (tcname c1) (tcname c2)  = tCon c1
  | hasAncestor env c1 c2               = tCon c2
  | hasAncestor env c2 c1               = tCon c1
  | not $ null common                   = tCon $ head common
  where common                          = commonAncestors env c1 c2

lub env (TFun _ e1 p1 k1 t1) (TFun _ e2 p2 k2 t2)
                                        = tFun (lub env e1 e2) (glb env p1 p2) (glb env k1 k2) (lub env t1 t2)
lub env (TTuple _ p1 k1) (TTuple _ p2 k2)
                                        = tTuple (lub env p1 p2) (lub env k1 k2)

lub env (TOpt _ t1) (TOpt _ t2)         = tOpt (lub env t1 t2)
lub env (TNone _) t2@TOpt{}             = t2
lub env t1@TOpt{} (TNone _)             = t1
lub env (TNone _) t2                    = tOpt t2
lub env t1 (TNone _)                    = tOpt t1
lub env (TOpt _ t1) t2                  = tOpt $ lub env t1 t2
lub env t1 (TOpt _ t2)                  = tOpt $ lub env t1 t2

lub env t1@(TFX _ fx1) t2@(TFX _ fx2)   = tTFX (lufx fx1 fx2)
  where lufx FXAction _                 = FXAction
        lufx _        FXAction          = FXAction
        lufx FXMut    _                 = FXMut
        lufx _        FXMut             = FXMut
        lufx FXPure   FXPure            = FXPure

lub env (TNil _ k1) (TNil _ k2)
  | k1 == k2                            = tNil k1
lub env (TRow _ k n t1 r1) r
  | Just (t2,r2) <- findInRow n r       = tRow k n (lub env t1 t2) (lub env r1 r2)

lub env t1 t2                           = -- noLUB t1 t2
                                          error ("No common supertype: " ++ prstr t1 ++ " and " ++ prstr t2)

noLUB t1 t2                             = tyerr t1 ("No common supertype: " ++ prstr t2)

----------------------------------------------------------------------------------------------------------------------
-- Improvement
----------------------------------------------------------------------------------------------------------------------

-- Check if the deferred constraint set should be resubmitted
-- Unify all var cycles
-- Perform G-simplification on internal variables
-- Check that there's at least one non-embedded variable
-- For all non-embedded variables: replace multiple lower/upper con bounds with a LUB/GLB
-- For non-embedded variables with a single lower/upper bound: replace with bound if not negative/positive
-- For non-embedded variables with multiple protocol constraints: identify equal and subtype-related protocols

-- After improvement:
--  headvar is defined
--  Cast/Sub bound is either TVar (upper), TCon, TNone (lower), TOpt (upper) or TFX
--  acyclic
--  G-minimal (constrained vars are observable)
--  S-minimal (constrained vars are invariant)
--  just single upper/lower bounds
--  no closed upper/lower bounds
--  no redundant Impl
--  no Sel/Mut covered by Cast/Sub/Impl bounds


improve                                 :: (Polarity a, Pretty a) => Env -> TEnv -> a -> Equations -> Constraints -> TypeM (Constraints,Equations)
improve env te tt eq []                 = return ([], eq)
improve env te tt eq cs
  | Nothing <- info                     = do --traceM ("  *Resubmit")
                                             simplify' env te tt eq cs
  | Left (v,vs) <- closure              = do --traceM ("  *Unify cycle " ++ prstr v ++ " = " ++ prstrs vs)
                                             sequence [ unify env (tVar v) (tVar v') | v' <- vs ]
                                             simplify' env te tt eq cs
  | not $ null gsimple                  = do --traceM ("  *G-simplify " ++ prstrs [ (v,tVar v') | (v,v') <- gsimple ])
                                             --traceM ("  *obsvars: " ++ prstrs obsvars)
                                             --traceM ("  *varvars: " ++ prstrs (varvars vi))
                                             sequence [ unify env (tVar v) (tVar v') | (v,v') <- gsimple ]
                                             simplify' env te tt eq cs
  | not $ null cyclic                   = tyerrs cyclic ("Cyclic subtyping:")
  | not $ null (multiUBnd++multiLBnd)   = do ub <- mapM (mkGLB env) multiUBnd
                                             lb <- mapM (mkLUB env) multiLBnd
                                             --traceM ("  *GLB " ++ prstrs ub)
                                             --traceM ("  *LUB " ++ prstrs lb)
                                             let cs' = [ Cast (tVar v) t | (v,t) <- ub ] ++ [ Cast t (tVar v) | (v,t) <- lb ]
                                             simplify' env te tt eq (cs' ++ map (replace ub lb) cs)
  | not $ null posLBnd                  = do --traceM ("  *S-simplify (dn) " ++ prstrs posLBnd)
                                             sequence [ unify env (tVar v) t | (v,t) <- posLBnd ]
                                             simplify' env te tt eq cs
  | not $ null negUBnd                  = do --traceM ("  *S-simplify (up) " ++ prstrs negUBnd)
                                             sequence [ unify env (tVar v) t | (v,t) <- negUBnd ]
                                             simplify' env te tt eq cs
  | not $ null closUBnd                 = do --traceM ("  *Simplify upper closed bound " ++ prstrs closUBnd)
                                             sequence [ unify env (tVar v) t | (v,t) <- closUBnd ]
                                             simplify' env te tt eq cs
  | not $ null closLBnd                 = do --traceM ("  *Simplify lower closed bound " ++ prstrs closLBnd)
                                             sequence [ unify env (tVar v) t | (v,t) <- closLBnd ]
                                             simplify' env te tt eq cs
  | not $ null redEq                    = do --traceM ("  *(Context red) " ++ prstrs [ w | (w,_,_) <- redEq ])
                                             sequence [ unify env t1 t2 | (t1,t2) <- redUni ]
                                             simplify' env te tt (redEq++eq) (remove [ w | (w,_,_) <- redEq ] cs)
  | not $ null dots                     = do --traceM ("  *Implied mutation/selection solutions " ++ prstrs dots)
                                             (eq',cs') <- solveDots env mutC selC selP cs
                                             simplify' env te tt (eq'++eq) cs'
  | otherwise                           = do --traceM ("  *improvement done")
                                             return (cs, eq)
  where info                            = varinfo cs
        Just vi                         = info
        closure                         = varclose (varvars vi)
        Right vclosed                   = closure
        (vvsL,vvsU)                     = unzip vclosed
        gsimple                         = gsimp vclosed obsvars (varvars vi)
        multiUBnd                       = [ (v,ts) | (v,ts) <- Map.assocs (ubounds vi), v `notElem` embedded vi, length ts > 1 ]
        multiLBnd                       = [ (v,ts) | (v,ts) <- Map.assocs (lbounds vi), v `notElem` embedded vi, length ts > 1 ]
        multiPBnd                       = [ (v,ps) | (v,ps) <- Map.assocs (pbounds vi), length ps > 1 ]
        lowerBnd                        = [ (v,t) | (v,[t]) <- Map.assocs (lbounds vi), v `notElem` embedded vi ]
        upperBnd                        = [ (v,t) | (v,[t]) <- Map.assocs (ubounds vi), v `notElem` embedded vi ]
        posLBnd                         = [ (v,t) | (v,t) <- lowerBnd, v `notElem` negvars, implAll env (lookup' v $ pbounds vi) t ]
        negUBnd                         = [ (v,t) | (v,t) <- upperBnd, v `notElem` posvars, implAll env (lookup' v $ pbounds vi) t, noDots env vi v ]
        closLBnd                        = [ (v,t) | (v, [t]) <- Map.assocs (lbounds vi), upClosed env t ]
        closUBnd                        = [ (v,t) | (v, [t]) <- Map.assocs (ubounds vi), dnClosed env t ]
        (redEq,redUni)                  = ctxtReduce env vi multiPBnd
        mutC                            = findBoundAttrs env (mutattrs vi) (ubounds vi)
        selC                            = findBoundAttrs env (selattrs vi) (ubounds vi)
        selP                            = findWitAttrs env (selattrs vi) (pbounds vi)
        dots                            = dom mutC ++ dom selC ++ dom selP
        fixedvars                       = tyfree env
        pvars                           = Map.keys (pbounds vi) ++ tyfree (Map.elems (pbounds vi))
        (posvars0,negvars0)             = polvars te `polcat` polvars tt
        (posvars,negvars)               = (posvars0++fixedvars++vvsL, negvars0++fixedvars++vvsU)
        obsvars                         = posvars0 ++ negvars0 ++ fixedvars ++ pvars ++ embedded vi
        boundvars                       = Map.keys (ubounds vi) ++ Map.keys (lbounds vi)
        boundprot                       = tyfree (Map.elems $ ubounds vi) ++ tyfree (Map.elems $ lbounds vi)
        cyclic                          = if null (boundvars\\boundprot) then [ c | c <- cs, headvar c `elem` boundvars ] else []

dnClosed env (TCon _ c)                 = isActor env (tcname c)
dnClosed env (TFX _ FXPure)             = True
dnClosed env (TNone _)                  = True
dnClosed env (TNil _ _)                 = True
dnClosed env _                          = False

upClosed env (TOpt _ _)                 = True
upClosed env (TNil _ _)                 = True
upClosed env _                          = False

findBoundAttrs env attrs bounds         = [ ((v,n),wsc) | (v,ns) <- Map.assocs attrs, n <- ns, wsc <- bounds' v n ]
  where bounds' v n                     = [ wsc | TCon _ c <- lookup' v bounds, Just wsc <- [findAttr env c n] ]

findWitAttrs env attrs bounds           = [ ((v,n), WInst p (NoQ w) ws) | (v,ns) <- Map.assocs attrs, n <- ns, (w,p,ws) <- bounds' v n ]
  where bounds' v n                     = [ (w,p,ws) | (w,p0) <- lookup' v bounds, (ws,p) <- findAncestry env p0, n `elem` conAttrs env (tcname p) ]


implAll env [] t                        = True
implAll env ps (TCon _ c)               = and [ hasWitness env (tcname c) (tcname p) | (w,p) <- ps ]
implAll env ps (TOpt _ _)               = all (\(_,p) -> any (qualEq env $ tcname p) [qnIdentity,qnEq]) ps
implAll env ps t                        = False

noDots env vi v                         = null (lookup' v $ selattrs vi) && null (lookup' v $ mutattrs vi)


replace ub lb c@(Cast TVar{} TVar{})    = c
replace ub lb (Cast (TVar _ v) t)
  | Just t' <- lookup v ub              = Cast t' t
replace ub lb (Cast t (TVar _ v))
  | Just t' <- lookup v lb              = Cast t t'
replace ub lb c@(Sub _ TVar{} TVar{})   = c
replace ub lb (Sub w (TVar _ v) t)
  | Just t' <- lookup v ub              = Sub w t' t
replace ub lb (Sub w t (TVar _ v))
  | Just t' <- lookup v lb              = Sub w t t'
replace ub lb c                         = c

solveDots env mutC selC selP cs         = do (eqs,css) <- unzip <$> mapM solveDot cs
                                             return (concat eqs, concat css)
  where solveDot c@(Mut (TVar _ v) n _)
          | Just w <- lookup (v,n) mutC = solveMutAttr env w c >>= \cs -> return ([], cs)
        solveDot c@(Sel _ (TVar _ v) n _)
          | Just w <- lookup (v,n) selC = solveSelAttr env w c
          | Just w <- lookup (v,n) selP = solveSelWit env w c
        solveDot c                      = return ([], [c])

ctxtReduce env vi multiPBnds            = (concat eqs, concat css)
  where (eqs,css)                       = unzip $ map ctxtRed multiPBnds
        ctxtRed (v,wps)                 = imp v [] [] [] wps
        imp v eq uni wps ((w,p):wps')
          | (w',wf,p1,p'):_ <- hits     = --trace ("  *" ++ prstr p ++ " covered by " ++ prstr p1) $
                                          imp v ((w, impl2type (tVar v) p, wf (eVar w')) : eq) ((tcargs p `zip` tcargs p') ++ uni) wps wps'
          | otherwise                   = --trace ("   (Not covered: " ++ prstr p ++ " in context " ++ prstrs (map snd (wps++wps')) ++ ")") $
                                          imp v eq uni ((w,p):wps) wps'
          where hits                    = [ (w',wf,p0,p') | (w',p0) <- wps++wps', w'/=w, Just (wf,p') <- [findAncestor env p0 (tcname p)] ]
        imp v eq uni wps []               = (eq, uni)
  -- TODO: also check that an mro exists (?)


remove ws []                            = []
remove ws (Impl w t p : cs)
  | w `elem` ws                         = remove ws cs
remove ws (c : cs)                      = c : remove ws cs


----------------------------------------------------------------------------------------------------------------------
-- Misc.
----------------------------------------------------------------------------------------------------------------------

px0:px1:px2:_                           = xNames

app tx e []                             = e
app tx e es                             = Lambda NoLoc p' k' (Call NoLoc e (exp2arg es (pArg p')) (kArg k')) fx
  where TFun _ fx p k _                 = tx                    -- If it takes arguments, it must be a function!
        (p',k')                         = (pPar pNames p, kPar kNames k)

app2nd (Just Static) tx e es            = app tx e es
app2nd (Just Property) tx e es          = app tx e es
app2nd Nothing tx e es                  = app tx e es
app2nd _ tx e []                        = e
app2nd _ tx e es                        = Lambda NoLoc p' k' (Call NoLoc e (PosArg pSelf (exp2arg es pArgs)) (kArg k')) fx
  where TFun _ fx p k _                 = tx                    -- If it takes arguments, it must be a function!
        (p',k')                         = (pPar pNames p, kPar kNames k)
        PosArg pSelf pArgs              = pArg p'                    

idwit w t1 t2                           = (w, wFun t1 t2, eLambda [(px0,t1)] (eVar px0))

rowFun PRow r1 r2                       = tFun fxPure (posRow (tTupleP r1) posNil) kwdNil (tTupleP r2)
rowFun KRow r1 r2                       = tFun fxPure (posRow (tTupleK r1) posNil) kwdNil (tTupleK r2)

rowWit PRow w n t r wt wr               = eLambda [(px0,posRow t r)] eTup
  where eTup                            = Paren l0 $ Tuple l0 (PosArg e1 (PosStar e2)) KwdNil
        e1                              = eCall (eVar wt) [DotI l0 (eVar px0) 0]
        e2                              = eCall (eVar wr) [RestI l0 (eVar px0) 0]
rowWit KRow w n t r wt wr               = eLambda [(px0,kwdRow n t r)] eTup
  where eTup                            = Paren l0 $ Tuple l0 PosNil (KwdArg n e1 (KwdStar e2))
        e1                              = eCall (eVar wt) [Dot l0 (eVar px0) n]
        e2                              = eCall (eVar wr) [Rest l0 (eVar px0) n]

wFun t1 t2                              = tFun fxPure (posRow t1 posNil) kwdNil t2
