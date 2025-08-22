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
module Acton.Solver where

import Control.Monad
import Control.Monad.Except
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Control.Exception
import Control.DeepSeq

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
import Acton.Unify



-- Reduce conservatively and remove entailed constraints
simplify                                    :: (Polarity a, Pretty a) => Env -> TEnv -> a -> Constraints -> TypeM (Constraints,Equations)
simplify env te tt cs                       = do css <- groupCs env cs
                                                 --traceM ("#### SIMPLIFY " ++ prstrs (map length css))
                                                 --sequence [ traceM ("## long:\n" ++ render (nest 4 $ vcat $ map pretty cs)) | cs <- css, length cs > 500 ]
                                                 simplifyGroups env te tt css

simplifyGroups env te tt []                 = return ([], [])
simplifyGroups env te tt (cs:css)           = do --traceM ("\n\n######### simplifyGroup\n" ++ render (nest 4 $ vcat $ map pretty cs))
                                                 (cs1,eq1) <- simplify' env te tt [] cs `catchError` \err -> Control.Exception.throw err
                                                 env <- usubst env
                                                 (cs2,eq2) <- simplifyGroups env te tt css
                                                 return (cs1++cs2, eq1++eq2)

simplify'                                   :: (Polarity a, Pretty a) => Env -> TEnv -> a -> Equations -> Constraints -> TypeM (Constraints,Equations)
simplify' env te tt eq []                   = return ([], eq)
simplify' env te tt eq cs                   = do eq1 <- reduce env eq cs
                                                 cs1 <- usubst =<< collectDeferred
                                                 --traceM ("## Improving:\n" ++ render (nest 8 $ vcat $ map pretty cs1))
                                                 env1 <- usubst env 
                                                 te1 <- usubst te
                                                 tt1 <- usubst tt
                                                 improve env1 te1 tt1 eq1 cs1

quicksimp env eq []                         = return ([], eq)
quicksimp env eq cs                         = do eq1 <- reduce env eq cs
                                                 cs1 <- usubst =<< collectDeferred
                                                 return (cs1, eq1)

groupCs env cs                              = do st <- currentState
                                                 mapM mark ([1..] `zip` cs)
                                                 m <- foldM group Map.empty cs
                                                 rollbackState st
                                                 return $ Map.elems m
  where mark (n,c)                          = do tvs <- (filter univar . ufree) <$> usubst c
                                                 tvs' <- (filter univar . ufree) <$> usubst (map tVar $ attrfree c)
                                                 sequence [ unify (DfltInfo NoLoc 1 Nothing []) (newUnivarToken n) (tVar tv) | tv <- nub (tvs++tvs') ]
        group m c                           = do tvs <- (filter univar . ufree) <$> usubst c
                                                 let tv = case tvs of [] -> tv0; tv:_ -> tv
                                                 return $ Map.insertWith (++) tv [c] m
        attrfree c@(Sel _ _ _ n _)          = allConAttrUFree env n
        attrfree c@(Mut _ _ n _)            = allConAttrUFree env n
        attrfree _                          = []
        TVar _ tv0                          = newUnivarToken 0


----------------------------------------------------------------------------------------------------------------------
-- solve
----------------------------------------------------------------------------------------------------------------------

data Rank                                   = RRed { cstr :: Constraint }
                                            | RSealed { tgt :: Type }
                                            | RTry { tgt :: Type, alts :: [Type], rev :: Bool }
                                            | RVar { tgt :: Type, alts :: [Type] }
                                            | ROvl { tgt :: Type }
                                            | RSkip
                                            deriving (Show)

instance Eq Rank where
    RRed _      == RRed _                   = True
    RSealed t1  == RSealed t2               = t1 == t2
    RTry t1 _ _ == RTry t2 _ _              = t1 == t2
    RVar t1 _   == RVar t2 _                = t1 == t2
    ROvl t1     == ROvl t2                  = True
    RSkip       == RSkip                    = True
    _           == _                        = False

instance Pretty Rank where
    pretty (RRed c)                         = text "<reduce>" <+> pretty c
    pretty (RSealed t)                      = pretty t <+> text "sealed"
    pretty (RTry t ts rev)                  = pretty t <+> braces (commaSep pretty ts) Pretty.<> (if rev then char '\'' else empty)
    pretty (RVar t ts)                      = pretty t <+> char '~' <+> commaSep pretty ts
    pretty (ROvl t)                         = pretty t <+> text "..."
    pretty RSkip                            = text "<skip>"

solve                                       :: (Polarity a, Pretty a, Show a) => Env -> (Constraint -> Bool) ->
                                               TEnv -> a -> Equations -> Constraints -> TypeM (Constraints,Equations)
solve env select te tt eq cs                = do css <- groupCs env cs
                                                 (cs',eq') <- solveGroups env select te tt css
                                                 eq <- usubst eq
                                                 return (cs', eq'++eq)

solveGroups env select te tt []             = return ([], [])
solveGroups env select te tt (cs:css)       = do --traceM ("\n\n######### solveGroup\n" ++ render (nest 4 $ vcat $ map pretty cs))
                                                 --traceM ("  ### te:\n" ++ render (nest 4 $ vcat $ map pretty te))
                                                 (cs1,eq1) <- solve' env select [] te tt [] cs `catchError` \err -> Control.Exception.throw err
                                                 env <- usubst env
                                                 (cs2,eq2) <- solveGroups env select te tt css
                                                 return (cs1++cs2, eq1++eq2)

solve' env select hist te tt eq cs
  | not $ null vargoals                     = do --traceM (unlines [ "### var goal " ++ prstr t ++ " ~ " ++ prstrs alts | RVar t alts <- vargoals ])
                                                 --traceM ("### var goals: " ++ show (sum [ length alts | RVar t alts <- vargoals ]))
                                                 sequence [ unify (DfltInfo (loc t) 2 Nothing []) t t' | RVar t alts <- vargoals, t' <- alts ]
                                                 proceed hist cs
  | any not keep_evidence                   = noSolve0 env Nothing [] keep_cs
  | null solve_cs || null goals             = return (keep_cs, eq)
  | otherwise                               = do st <- currentState
                                                 --traceM ("## keep:\n" ++ render (nest 8 $ vcat $ map pretty keep_cs))
                                                 --traceM ("## solve:\n" ++ render (nest 8 $ vcat $ map pretty solve_cs))
                                                 --traceM ("## ranks:\n" ++ render (nest 8 $ vcat $ map pretty rnks))
                                                 --traceM ("## optvs: " ++ prstrs optvs)
                                                 --traceM ("## posvs: " ++ prstrs posvs)
                                                 --traceM ("## negvs: " ++ prstrs negvs)
                                                 case head goals of
                                                    RRed c -> do
                                                        --traceM ("### reduce " ++ prstr c)
                                                        proceed hist cs
                                                    RSealed t -> do
                                                        --traceM ("### try goal " ++ prstr t ++ ", candidates: " ++ prstrs [fxAction, fxPure])
                                                        tryAlts st t [fxAction, fxPure]
                                                    RTry t alts r -> do
                                                        --traceM ("### try goal " ++ prstr t ++ ", candidates: " ++ prstrs alts ++ if r then " (rev)" else "")
                                                        tryAlts st t alts
                                                    RVar t alts -> do
                                                        --traceM ("### var goal " ++ prstr t ++ ", unifying with " ++ prstrs alts)
                                                        unifyM (DfltInfo (loc t) 3 Nothing []) alts (repeat t) >> proceed hist cs
                                                    ROvl t -> do
                                                        --traceM ("### ovl goal " ++ prstr t ++ ", defaulting remaining constraints")
                                                        (cs,eq) <- simplify' (useForce env) te tt eq cs
                                                        te <- usubst te
                                                        tt <- usubst tt
                                                        hist <- usubst hist
                                                        solve' env select hist te tt eq cs
                                                    RSkip ->
                                                        return (keep_cs, eq)

  where (solve_cs, keep_cs)                 = partition select cs
        keep_evidence                       = [ hasWitness env t p | Impl _ _ t p <- keep_cs ]
        (vargoals, goals)                   = span isVar $ sortOn deco $ map condense $ group rnks
        group []                            = []
        group (r:rs)                        = (r : rs1) : group rs2
          where (rs1,rs2)                   = partition (==r) rs
        rnks                                = map (rank env) solve_cs
        tryAlts st t@(TVar _ tv) []        = do  --traceM ("### FAIL " ++ prstr tv ++ ":\n" ++ render (nest 4 $ vcat $ map pretty cs))
                                                 let ts = map (\n -> tCon (TC (noQ ('t':show n)) [])) [0..]
                                                     vs = filter (\v -> length (filter (\c -> v `elem` ufree c) cs) > 1) (nub (ufree cs))
                                                     cs' = if length cs == 1 then cs else filter (not . useless vs) cs
                                                     vs' = filter (\v -> length (filter (\c -> v `elem` ufree c) cs') > 1) (nub (ufree cs'))
                                                 sequence [ usubstitute uv t | (uv,t) <- vs' `zip` ts ]
                                                 cs' <- usubst cs'
                                                 noSolve0 env (Just t) (take (length vs') ts) cs'
        tryAlts st _ []                     = noSolve0 env Nothing [] cs
        tryAlts st t0 (t:ts)                = tryAlt t0 t `catchError` const (
                                                    do --traceM ("=== ROLLBACK " ++ prstr t0)
                                                       rollbackState st >> tryAlts st t0 ts)
        tryAlt t0 (TCon _ c)
          | isProto env (tcname c)          = do p <- instwildcon env c
                                                 w <- newWitness
                                                 --traceM ("  # trying " ++ prstr t0 ++ " (" ++ prstr p ++ ")")
                                                 proceed hist (Impl (DfltInfo NoLoc 4 Nothing []) w t0 p : cs)
        tryAlt t0 (TTuple _ _ _)
          | not $ null attrs                = do t <- instwild env KType (tTupleK $ foldr (\n -> kwdRow n tWild) tWild attrs)
                                                 --traceM ("  # trying tuple " ++ prstr t0 ++ " = " ++ prstr t)
                                                 unify (DfltInfo NoLoc 5 Nothing []) t0 t
                                                 proceed (t:hist) cs
          where attrs                       = sortBy (\a b -> compare (nstr a) (nstr b)) $ nub [ n | Sel _ _ t n _ <- solve_cs, t == t0 ]
        tryAlt t0@(TVar _ tv) t
          | tvkind tv == KFX                = do t <- instwild env (kindOf env t0) t
                                                 --traceM ("  # trying " ++ prstr t0 ++ " = " ++ prstr t)
                                                 unify (DfltInfo NoLoc 5 Nothing []) t0 t
                                                 (cs,eq) <- quicksimp env eq cs
                                                 hist <- usubst hist
                                                 solve' env select hist te tt eq cs
        tryAlt t0 t                         = do t <- instwild env (kindOf env t0) t
                                                 --traceM ("  # trying " ++ prstr t0 ++ " = " ++ prstr t)
                                                 unify (DfltInfo NoLoc 5 Nothing []) t0 t
                                                 proceed (t:hist) cs
        proceed hist cs                     = do te <- usubst te
                                                 tt <- usubst tt
                                                 (cs,eq) <- simplify' env te tt eq cs
                                                 hist <- usubst hist
                                                 solve' env select hist te tt eq cs

        condense (RRed c : rs)              = RRed c
        condense (RSealed t : rs)           = RSealed t
        condense (RTry t as r : rs)
          | TVar{} <- t                     = RTry t (if rev' then subrev ts' else ts') rev'
          | otherwise                       = RTry t ts r
          where ts                          = foldr intersect as $ map alts rs
                ts'                         = if tvar t `elem` optvs then ts \\ [tOpt tWild] else ts
                rev'                        = (or $ r : map rev rs) || tvar t `elem` posvs
        condense (RVar t as : rs)           = RVar t (foldr union as $ map alts rs)
        condense (ROvl t : rs)              = ROvl t
        condense (RSkip : rs)               = RSkip
        condense rs                         = error ("### condense " ++ show rs)

        optvs                               = optvars cs ++ optvars hist
        embvs                               = embvars cs
        univs                               = univars cs
        (posvs, negvs)                      = polvars te `polcat` polvars tt

        isVar RVar{}                        = True
        isVar _                             = False

        deco (RRed cs)                      = (0, 0, 0, 0)
        deco (RSealed t)                    = (2, 0, 0, 0)
        deco (RTry (TVar _ v) as r)         = (w, length $ filter (==v) embvs, length as, length $ filter (==v) univs)
          where w | wildTuple `elem` as     =  3
                  | otherwise               =  4
        deco (RTry t as r)                  = (5, 0, length as, 0)
        deco (RVar t as)                    = (6, 0, length as, 0)
        deco (ROvl t)                       = (7, 0, 0, 0)
        deco (RSkip)                        = (8, 0, 0, 0)

        subrev []                           = []
        subrev (t:ts)                       = subrev ts1 ++ t : subrev ts2
          where (ts1,ts2)                   = partition (\t' -> castable env t' t) ts

-- subrev [int,Pt,float,CPt,C3Pt]           = [] ++ int : subrev [Pt,float,CPt,C3Pt]
--                                          = int : subrev [CPt,C3Pt] ++ Pt : subrev [float]
--                                          = int : [C3Pt] ++ CPt ++ subrev [] ++ Pt : [] ++ float : subrev []
--                                          = int : C3Pt : CPt : Pt : float

rank                                        :: Env -> Constraint -> Rank
rank env (Sub info _ t1 t2)                 = rank env (Cast info t1 t2)

rank env (Cast _ t1@(TVar _ v1) t2@(TVar _ v2))
  | univar v1, univar v2                    = RVar t1 [t2]
rank env (Cast _ t1@TVar{} (TOpt _ t2@TVar{}))
  | univar (tvar t1), univar (tvar t2)      = RVar t1 [t2]
rank env (Cast _ t1@TVar{} (TOpt _ t2))
  | univar (tvar t1)                        = RTry t1 ([tOpt tWild, tNone] ++ allBelow env t2) False
rank env (Cast _ TNone{} t2@TVar{})
  | univar (tvar t2)                        = RTry t2 [tOpt tWild, tNone] True
rank env (Cast _ t1@TVar{} t2)
  | univar (tvar t1)                        = RTry t1 (allBelow env t2) False
rank env (Cast _ t1 t2@TVar{})
  | univar (tvar t2)                        = RTry t2 (allAbove env t1) True

rank env c@(Impl _ _ t p)
  | schematic t `elem` ts                   = ROvl t
  | not $ null $ ufree t                   = RTry t ts False
  where ts                                  = allExtProto env t p

rank env (Sel _ _ t@TVar{} n _)             = RTry t (allConAttr env n ++ allProtoAttr env n ++ allExtProtoAttr env n ++ [wildTuple]) False
rank env (Mut _ t@TVar{} n _)               = RTry t (allConAttr env n) False

rank env (Seal _ t@TVar{})
  | tvkind (tvar t) == KFX                  = RSealed t
  | otherwise                               = RSkip

rank env c                                  = RRed c

wildTuple                                   = tTuple tWild tWild

-------------------------------------------------------------------------------------------------------------------------

class OptVars a where
    optvars                             :: a -> [TUni]

instance (OptVars a) => OptVars [a] where
    optvars                             = concat . map optvars

instance OptVars Constraint where
    optvars (Cast _ t1 t2)              = optvars [t1, t2]
    optvars (Sub _ w t1 t2)             = optvars [t1, t2]
    optvars (Impl _ w t p)              = optvars t ++ optvars p
    optvars (Sel _ w t1 n t2)           = optvars [t1, t2]
    optvars (Mut _ t1 n t2)             = optvars [t1, t2]
    optvars (Seal _ t)                  = optvars t

instance OptVars Type where
    optvars (TOpt _ (TVar _ v))         = [v]
    optvars (TOpt _ t)                  = optvars t
    optvars (TCon _ c)                  = optvars c
    optvars (TFun _ fx p k t)           = optvars [p, k, t]
    optvars (TTuple _ p k)              = optvars [p, k]
    optvars (TRow _ _ _ t r)            = optvars [t, r]
    optvars (TStar _ _ r)               = optvars r
    optvars _                           = []

instance OptVars TCon where
    optvars (TC n ts)                   = optvars ts

embvars cs                              = concat $ map emb cs
  where emb (Cast _ (TVar _ v) (TVar _ v'))
          | univar v && univar v'       = []
        emb (Cast _ (TVar _ v) t)
          | univar v                    = ufree t
        emb (Cast _ t (TVar _ v))
          | univar v                    = ufree t
        emb (Sub _ _ (TVar _ v) (TVar _ v'))
          | univar v && univar v'       = []
        emb (Sub _ _ (TVar _ v) t)
          | univar v                    = ufree t
        emb (Sub _ _ t (TVar _ v))
          | univar v                    = ufree t
        emb (Impl _ _ (TVar _ v) p)
          | univar v                    = ufree p
        emb (Impl _ _ (TCon _ c) p)
          | otherwise                   = ufree c ++ ufree p
        emb (Sel _ _ (TVar _ v) n t)
          | univar v                    = ufree t
        emb (Mut _ (TVar _ v) n t)
          | univar v                    = ufree t
        emb _                           = []

univars cs                              = concat $ map uni cs
  where uni (Cast _ (TVar _ v) (TVar _ v'))
          | univar v && univar v'       = [v,v']
        uni (Sub _ _ (TVar _ v) (TVar _ v'))
          | univar v && univar v'       = [v,v']
        uni _                           = []

allAbove env (TCon _ tc)                = tOpt tWild : map tCon tcons
  where n                               = tcname tc
        tcons                           = allAncestors env tc ++ [schematic' tc]
allAbove env (TVar _ tv)
  | not $ univar tv                     = [tOpt tWild, tCon tc, tVar tv]
  where tc                              = schematic' $ findTVBound env tv
allAbove env (TOpt _ t)                 = [tOpt tWild]
allAbove env (TNone _)                  = [tOpt tWild, tNone]
allAbove env (TFun _ _ _ _ _)           = [tOpt tWild, tFun tWild tWild tWild tWild]
allAbove env (TTuple _ _ _)             = [tOpt tWild, tTuple tWild tWild]
--allAbove env (TRow _ k n _ _)           = [tRow k n tWild tWild]
--allAbove env (TStar _ k r)              = [tStar k tWild]
--allAbove env (TNil _ k)                 = [tNil k]
allAbove env (TFX _ FXProc)             = [fxProc]
allAbove env (TFX _ FXMut)              = [fxProc, fxMut]
allAbove env (TFX _ FXPure)             = [fxProc, fxMut, fxPure]
allAbove env (TFX _ FXAction)           = [fxProc, fxAction]

allBelow env (TCon _ tc)                = map tCon $ schematic' tc : allDescendants env tc
allBelow env (TVar _ tv)                = [tVar tv]
allBelow env (TOpt _ t)                 = tOpt tWild : allBelow env t ++ [tNone]
allBelow env (TNone _)                  = [tNone]
allBelow env (TFun _ _ _ _ _)           = [tFun tWild tWild tWild tWild]
allBelow env (TTuple _ _ _)             = [tTuple tWild tWild]
--allBelow env (TRow _ k n _ _)           = [tRow k n tWild tWild]
--allBelow env (TStar _ k r)              = [tStar k tWild]
--allBelow env (TNil _ k)                 = [tNil k]
allBelow env (TFX _ FXProc)             = [fxProc, fxMut, fxPure, fxAction]
allBelow env (TFX _ FXMut)              = [fxMut, fxPure]
allBelow env (TFX _ FXPure)             = [fxPure]
allBelow env (TFX _ FXAction)           = [fxAction]

----------------------------------------------------------------------------------------------------------------------
-- reduce
----------------------------------------------------------------------------------------------------------------------

instance USubst Equation where
    usubst (Eqn w t e)                      = do t <- usubst t
                                                 e <- usubst e
                                                 return (Eqn w t e)
    
instance UFree Equation where
    ufree (Eqn w t e)                       = ufree t ++ ufree e

instance Vars Equation where
    free (Eqn w t e)                     = free e

    bound (Eqn w t e)                    = [w]



reduce                                      :: Env -> Equations -> Constraints -> TypeM Equations
reduce env eq []                            = return eq
reduce env eq (c:cs)                        = do c <- usubst c
                                                 --traceM ("   reduce " ++ prstr c)
                                                 eq1 <- reduce' env eq c
                                                 reduce env eq1 cs

reduce'                                     :: Env -> Equations -> Constraint -> TypeM Equations
reduce' env eq c@(Cast i t1 t2)             = do cast' env i t1 t2
                                                 return eq

reduce' env eq c@(Sub i w t1 t2)            = sub' env i eq w t1 t2

reduce' env eq c@(Impl _ w t@(TVar _ tv) p)
  | univar tv                               = do defer [c]; return eq
  | [wit] <- witSearch                      = do (eq',cs) <- solveImpl env wit w t p
                                                 reduce env (eq'++eq) cs
  | not $ null witSearch                    = do defer [c]; return eq
  | [wit] <- witSearch'                     = do (eq',cs) <- solveImpl env wit w (tCon tc) p
                                                 reduce env (eq'++eq) cs
  where witSearch                           = findWitness env t p
        tc                                  = findTVBound env tv
        witSearch'                          = findWitness env (tCon tc) p

reduce' env eq c@(Impl _ w t@(TCon _ tc) p)
  | tcname p == qnIdentity,
    isActor env (tcname tc)                 = do let e = eCall (eQVar primIdentityActor) []
                                                 return (Eqn w (impl2type t p) e : eq)
  | [wit] <- witSearch                      = do (eq',cs) <- solveImpl env wit w t p
                                                 reduce env (eq'++eq) cs
  | not $ null witSearch                    = do defer [c]; return eq
  where witSearch                           = findWitness env t p

reduce' env eq c@(Impl _ w t@(TFX _ tc) p)
  | [wit] <- witSearch                      = do (eq',cs) <- solveImpl env wit w t p
                                                 reduce env (eq'++eq) cs
  | not $ null witSearch                    = do defer [c]; return eq
  where witSearch                           = findWitness env t p

reduce' env eq c@(Impl info w t@(TOpt _ t') p)
  | tcname p == qnEq                        = do w' <- newWitness
                                                 let e = eCall (tApp (eQVar primEqOpt) [t']) [eVar w']
                                                 reduce env (Eqn w (impl2type t p) e : eq) [Impl info w' t' p]

reduce' env eq c@(Impl _ w t@(TNone _) p)
  | tcname p == qnEq                        = return (Eqn w (impl2type t p) (eQVar primWEqNone) : eq)

reduce' env eq c@(Sel _ w (TVar _ tv) n _)
  | univar tv                               = do defer [c]; return eq
  | Just wsc <- attrSearch                  = do (eq',cs) <- solveSelAttr env wsc c
                                                 reduce env (eq'++eq) cs
  | Just p <- protoSearch                   = do (eq',cs) <- solveSelProto env p c
                                                 reduce env (eq'++eq) cs
  | otherwise                               = tyerr n "Attribute not found"
  where attrSearch                          = findTVAttr env tv n
        protoSearch                         = findProtoByAttr env (NoQ $ tvname tv) n

reduce' env eq c@(Sel _ w (TCon _ tc) n _)
  | Just wsc <- attrSearch                  = do (eq',cs) <- solveSelAttr env wsc c
                                                 reduce env (eq'++eq) cs
  | Just p <- protoSearch                   = do (eq',cs) <- solveSelProto env p c
                                                 reduce env (eq'++eq) cs
  | otherwise                               = tyerr n "Attribute not found"
  where attrSearch                          = findAttr env tc n
        protoSearch                         = findProtoByAttr env (tcname tc) n


reduce' env eq c@(Sel info w t1@(TTuple _ _ r) n t2)
  | TVar _ tv <- r                          = do defer [c]; return eq
  | n `elem` valueKWs                       = do let e = eLambda [(px0,t1)] (eDot (eVar px0) n)
                                                 return (Eqn w (wFun t1 t2) e : eq)
  | otherwise                               = do --traceM ("### Sel " ++ prstr c)
                                                 select r
  where select (TRow _ _ n' t r)
          | n == n'                         = do w' <- newWitness
                                                 let e = eLambda [(px0,t1)] (eDot (eCallVar w' [eVar px0]) n)
                                                 reduce env (Eqn w (wFun t1 t2) e : eq) [Sub info w' t t2]
          | otherwise                       = select r
        select (TStar _ _ r)                = do w' <- newWitness
                                                 let e = eLambda [(px0,t1)] (eCallVar w' [eDot (eVar px0) attrKW])
                                                 reduce env (Eqn w (wFun t1 t2) e : eq) [Sel info w' (tTupleK r) n t2]
        select (TNil _ _)                   = kwdNotFound0 env info n

--  lambda (x:(a:int,b:int,**(c:int))): x.b  ==>  lambda x: (b=x.b, a=x.a, KW=x.KW).b            ==>  lambda x: x.b
--  lambda (x:(a:int,b:int,**(c:int))): x.c  ==>  lambda x: (c=x.KW.x, a=x.a, b=x.b, KW=x.KW).c  ==>  lambda x: x.KW.c

reduce' env eq c@(Mut _ (TVar _ tv) n _)
  | univar tv                               = do defer [c]; return eq
  | Just wsc <- attrSearch                  = do solveMutAttr env wsc c
                                                 return eq
  | otherwise                               = tyerr n "Attribute not found:"
  where attrSearch                          = findTVAttr env tv n

reduce' env eq c@(Mut _ (TCon _ tc) n _)
  | Just wsc <- attrSearch                  = do solveMutAttr env wsc c
                                                 return eq
  | otherwise                               = tyerr n "Attribute not found:"
  where attrSearch                          = findAttr env tc n

reduce' env eq c@(Seal _ t@(TVar _ tv))
  | univar tv                               = do defer [c]; return eq
  | otherwise                               = return eq
reduce' env eq (Seal info t@(TCon _ tc))
--  | castable env t tObject                  = tyerr t "Leaking actor seal:"                       -- when we start prohibit sharing of mutable data
  | otherwise                               = reduce env eq (map (Seal info) $ tcargs tc)
reduce' env eq (Seal _ t@(TFX _ fx))
--  | fx `elem` [FXMut,FXProc]                = tyerr t "Leaking actor seal:"
--  | fx `elem` [FXProc]                      = tyerr t "Leaking actor seal:"
  | otherwise                               = return eq
reduce' env eq (Seal info t)                = reduce env eq (map (Seal info) ts)
  where ts                                  = leaves t

reduce' env eq c                            = noRed0 env c


solveImpl env wit w t p                     = do (cs,t',we) <- instWitness env p wit
                                                 unify (DfltInfo NoLoc 7 Nothing []) t t'
                                                 return ([Eqn w (impl2type t p) we], cs)

solveSelAttr env (wf,sc,d) (Sel info w t1 n t2)
                                            = do (cs,tvs,t) <- instantiate env sc
                                                 when (negself t) (tyerr n "Contravariant Self attribute not selectable by instance")
                                                 w' <- newWitness
                                                 let e = eLambda [(px0,t1)] (eCallVar w' [app t (tApp (eDot (wf $ eVar px0) n) tvs) $ witsOf cs])
                                                     c = Sub (DfltInfo (loc info) 8 Nothing []) w' (vsubst [(tvSelf,t1)] t) t2
                                                 return ([Eqn w (wFun t1 t2) e], c:cs)

--  e1.__setslice__(sl, e2)
--  e1.__setslice__(w_Iterable, sl, e2)
--  w_Sliceable.__setslice__(e1, w_Iterable, sl, e2)

--  w(e1)(sl,e2)                                                        w = lambda x0: lambda p1,p2: w_Sliceable.__setslice__(x0, w1, p1, p2)
--  (lambda p1,p2: w_Sliceable.__setslice__(x0, w1, p1, p2))(sl,e2)
--  w_Sliceable.__setslice__(x0, w1, sl, e2)                            w1 = w_Iterable
--  w_Sliceable.__setslice__(e1, w_Iterable, sl, e2)

solveSelProto env pn c@(Sel info w t1 n t2) = do p <- instwildcon env pn
                                                 w' <- newWitness
                                                 (eq,cs) <- solveSelWit env (p, eVar w') c
                                                 return (eq, Impl info w' t1 p : cs)

solveSelWit env (p,we) c0@(Sel info w t1 n t2)
                                            = do let Just (wf,sc,d) = findAttr env p n
                                                 (cs,tvs,t) <- instantiate env sc
                                                 when (negself t) (tyerr n "Contravariant Self attribute not selectable by instance")
                                                 w' <- newWitness
                                                 let e = eLambda [(px0,t1)] (eCallVar w' [app t (tApp (eDot (wf we) n) tvs) $ eVar px0 : witsOf cs])
                                                     c = Sub (DfltInfo NoLoc 9 Nothing []) w' (vsubst [(tvSelf,t1)] t) t2
                                                 return ([Eqn w (wFun t1 t2) e], c:cs)

solveMutAttr env (wf,sc,dec) c@(Mut info t1 n t2)
                                            = do when (dec /= Just Property) (noMut n)
                                                 let TSchema _ [] t = sc
                                                 cast env (DfltInfo (loc c) 10 Nothing []) t2 (vsubst [(tvSelf,t1)] t)

----------------------------------------------------------------------------------------------------------------------
-- witness lookup
----------------------------------------------------------------------------------------------------------------------

findWitness                 :: Env -> Type -> PCon -> [Witness]
findWitness env t p
  | length all_ws <= 1      = all_ws
  | otherwise               = --trace ("## findWitness " ++ prstr t ++ " (" ++ prstr p ++ "), all_ws: " ++ prstrs all_ws) $
                              case elim [] match_ws of
                                [w] | null uni_ws  -> {-trace (" # best: " ++ prstr w) $ -}[w]
                                w:u | force        -> {-trace (" # best forced: " ++ prstrs (w:u)) $ -}[w]
                                _   | force        -> {-trace (" # first forced: " ++ prstr (head all_ws)) $ -}[head all_ws]
                                    | otherwise    -> {-trace (" # all") $ -}all_ws
  where t_                  = uwild t                   -- matching against uwild t also accepts witnesses that would instantiate t
        p_                  = uwild p                   -- matching against uwild p also accepts witnesses that would instantiate p
        force               = isForced env
        t'                  = if force then t_ else t   -- allow instantiation only when in forced mode
        elimSelf wc         = wc{ proto = vsubst [(tvSelf,wtype wc)] (proto wc) }
        all_ws              = reverse $ filter (matchCoarse t_ p_) $ map elimSelf $ witsByPName env $ tcname p -- all witnesses that could be used
        (match_ws, rest_ws) = partition (matchFine t') all_ws                                                  -- only those that match t exactly
        uni_ws              = filter (unifying (DfltInfo (loc t) 11 Nothing []) t) rest_ws
        elim ws' []         = reverse ws'
        elim ws' (w:ws)
          | covered         = elim ws' ws
          | otherwise       = elim (w:ws') ws
          where covered     = or [ matchWit w' w && not (matchWit w w') | w' <- ws'++ws ]

findProtoByAttr env cn n    = case filter hasAttr $ witsByTName env cn of
                                [] -> Nothing
                                w:_ -> Just $ schematic' $ proto w
  where hasAttr w           = n `elem` conAttrs env (tcname $ proto w)

hasWitness                  :: Env -> Type -> PCon -> Bool
hasWitness env (TVar _ tv) p
  | univar tv               = True
hasWitness env (TCon _ c) p
  | isActor env (tcname c),
    tcname p == qnIdentity  = True
hasWitness env t p          =  not $ null $ findWitness env t p

allExtProto                 :: Env -> Type -> PCon -> [Type]
allExtProto env t p         = reverse [ schematic (wtype w) | w <- witsByPName env (tcname p), matchCoarse t_ p_ w ]
  where t_                  = uwild t                   -- matching against uwild t also accepts witnesses that would instantiate t
        p_                  = uwild p                   -- matching against uwild p also accepts witnesses that would instantiate p

allExtProtoAttr             :: Env -> Name -> [Type]
allExtProtoAttr env n       = [ tCon tc | tc <- allCons env, any ((n `elem`) . allAttrs' env . proto) (witsByTName env $ tcname tc) ]

matchCoarse t p w           = match_p && eqhead t (wtype w)
  where match_p             = matching (tcargs p) (qbound $ binds w) (tcargs (proto w))
        eqhead (TWild _)  _             = True
        eqhead _          (TWild _)     = True
        eqhead (TCon _ c) (TCon _ c')   = tcname c == tcname c'
        eqhead (TFX _ fx) (TFX _ fx')   = fx == fx'
        eqhead (TVar _ v) (TVar _ v')   = v == v'
        eqhead _          _             = False

matchFine t w               = matching [t] (qbound $ binds w) [wtype w]

matchExactly t p w          = matching (t : tcargs p) (qbound $ binds w) (wtype w : tcargs (proto w))

matchWit w w'               = matchExactly (wtype w) (proto w) w'

matching ts vs ts'          = isJust $ matches vs ts ts'    -- there is a substitution s with domain vs such that ts == vsubst s ts'

unifying info t w           = runTypeM $ tryUnify `catchError` const (return False)     -- WATCH OUT: instantiate TVars to TUnis first?
  where tryUnify            = do unify info t (wtype w)
                                 return True


----------------------------------------------------------------------------------------------------------------------
-- cast
----------------------------------------------------------------------------------------------------------------------

cast                                        :: Env -> ErrInfo -> Type -> Type -> TypeM ()
cast env info t1 t2                         = do t1' <- usubst t1
                                                 t2' <- usubst t2
                                                 info' <- usubst info
                                                 --traceM ("   cast " ++ prstr t1' ++ " < " ++ prstr t2')
                                                 cast' env info' t1' t2'

castM env info ts1 ts2                      = mapM_ (uncurry $ cast env info) (ts1 `zip` ts2)


cast' env _ (TWild _) t2                    = return ()
cast' env _ t1 (TWild _)                    = return ()

cast' env info (TCon _ c1) (TCon _ c2)
  | Just (wf,c') <- search                  = if tcname c1 == tcname c2 && tcname c1 `elem` covariant then
                                                  castM env info (tcargs c') (tcargs c2)
                                              else                                              -- TODO: infer polarities in general!
                                                  unifyM info (tcargs c') (tcargs c2)
  where search                              = findAncestor env c1 (tcname c2)

cast' env info f1@(TFun _ fx1 p1 k1 t1) f2@(TFun _ fx2 p2 k2 t2)
                                            = do cast env info fx1 fx2
                                                 k2 <- castPos env info p2 p1 k2 k1
                                                 cast env info k2 k1
                                                 cast env info t1 t2

cast' env info (TTuple _ p1 k1) (TTuple _ p2 k2)
                                            = do k1 <- castPos env info p1 p2 k1 k2
                                                 cast env info k1 k2

cast' env info (TOpt _ t1@TOpt{}) t2        = cast env info t1 t2
cast' env info t1 (TOpt _ t2@TOpt{})        = cast env info t1 t2
cast' env info (TOpt _ t1) (TOpt _ t2)      = cast env info t1 t2
cast' env info (TVar _ tv) t2@TNone{}       = do usubstitute tv tNone
                                                 cast env info tNone t2
cast' env info t1@TOpt{} (TVar _ tv)        = do t2 <- instwild env KType $ tOpt tWild      -- What if tv is in t1???
                                                 usubstitute tv t2
                                                 cast env info t1 t2
cast' env info t1 (TOpt _ t2)
  | t1 == t2                                = return ()
cast' env _ (TNone _) (TOpt _ t)            = return ()
cast' env _ (TNone _) (TNone _)             = return ()

cast' env info t1@(TFX _ fx1) t2@(TFX _ fx2)
  | castFX fx1 fx2                          = return ()
  where castFX FXPure   FXPure              = True
        castFX FXPure   FXMut               = True
        castFX FXPure   FXProc              = True
        castFX FXMut    FXMut               = True
        castFX FXMut    FXProc              = True
        castFX FXProc   FXProc              = True
        castFX FXAction FXAction            = True
        castFX FXAction FXProc              = True
        castFX _        _                   = False

cast' env _ (TNil _ k1) (TNil _ k2)
  | k1 == k2                                = return ()
cast' env info (TVar _ tv) r2@(TNil _ k)
  | tvkind tv == k                          = do usubstitute tv (tNil k)
                                                 cast env info (tNil k) r2
cast' env info r1@(TNil _ k) (TVar _ tv)
  | k == tvkind tv                          = do usubstitute tv (tNil k)
                                                 cast env info r1 (tNil k)
cast' env info (TRow _ k1 n1 t1 r1) (TRow _ k2 n2 t2 r2)
  | k1 == k2 && n1 == n2                    = do cast env info t1 t2
                                                 cast env info r1 r2
cast' env info r1@(TNil _ _) r2@(TRow _ _ n _ _)
                                            = posElemNotFound0 env True (Cast info r1 r2) n
cast' env info r1@(TRow _ _ n _ _) r2@(TNil _ _)
                                            = surplusRow r1
cast' env info (TStar _ k1 r1) (TStar _ k2 r2)
  | k1 == k2                                = cast env info r1 r2
cast' env info (TVar _ tv) r2@(TRow _ k n _ _)
  | tvkind tv == k                          = do r1 <- instwild env k $ tRow k n tWild tWild
                                                 usubstitute tv r1
                                                 cast env info r1 r2
cast' env info r1@(TRow _ k n _ _) (TVar _ tv)
  | k == tvkind tv                          = do r2 <- instwild env k $ tRow k n tWild tWild
                                                 usubstitute tv r2
                                                 cast env info r1 r2
cast' env info (TVar _ tv) r2@(TStar _ k _)
  | tvkind tv == k                          = do r1 <- instwild env k $ tStar k tWild
                                                 usubstitute tv r2
                                                 cast env info r1 r2
cast' env info r1@(TStar _ k _) (TVar _ tv)
  | k == tvkind tv                          = do r2 <- instwild env k $ tStar k tWild
                                                 usubstitute tv r1
                                                 cast env info r1 r2

cast' env info (TVar _ tv) t2@TFun{}
  | univar tv && tvkind tv == KType         = do t1 <- instwild env KType $ tFun tWild tWild tWild tWild
                                                 usubstitute tv t1
                                                 cast env info t1 t2
cast' env info t1@TFun{} (TVar _ tv)                                                                             -- Should remove this, rejects tv = TOpt...
  | univar tv && KType == tvkind tv         = do t2 <- instwild env KType $ tFun tWild tWild tWild tWild
                                                 usubstitute tv t2
                                                 cast env info t1 t2
cast' env info (TVar _ tv) t2@TTuple{}
  | univar tv && tvkind tv == KType         = do t1 <- instwild env KType $ tTuple tWild tWild
                                                 usubstitute tv t1
                                                 cast env info t1 t2

cast' env info (TVar _ tv1) (TVar _ tv2)
  | tv1 == tv2                              = return ()
cast' env info t1@(TVar _ tv) t2
  | univar tv                               = defer [Cast info t1 t2]
cast' env info t1 t2@(TVar _ tv)
  | univar tv                               = defer [Cast info t1 t2]
cast' env info t1@(TVar _ tv) t2            = cast' env info (tCon tc) t2
  where tc                                  = findTVBound env tv

cast' env info t1 t2@(TVar _ tv)            = noRed0 env (Cast info t1 t2)

cast' env info t1 (TOpt _ t2)               = cast env info t1 t2                -- Only matches when t1 is NOT a variable

cast' env info t1 t2                        = noRed0 env (Cast info t1 t2)

simpInfo env info                           = case info of
                                                 DeclInfo l1 l2 n sc msg -> DeclInfo l1 l2 n (simp env sc) msg
                                              --   SelInfo l1 l2 n t msg -> SelInfo l1 l2 n (simp env t) msg
                                                 _ -> info

noRed0 env c                                = do c <- uwild <$> usubst c
                                                 noRed (c{info = simpInfo env (info c)})

noSolve0 env mbt vs cs                      = do mbt <- uwild <$> usubst mbt
                                                 cs <- uwild <$> usubst cs
                                                 noSolve mbt vs $ map (\c -> c{info = simpInfo env (info c)}) cs

posElemNotFound0 env b c n                  = do c <- uwild <$> usubst c
                                                 posElemNotFound b (c{info = simpInfo env (info c)}) n

kwdNotFound0 env info n                     = do i <- uwild <$> usubst info
                                                 kwdNotFound (simpInfo env i) n

castPos env info p1 p2 k1 k2                = (do cast env info p1 p2; return k1) `catchError` handler
  where handler (SurplusRow p)              = do --traceM ("## Shifting row " ++ prstr p ++ " into " ++ prstr k2)
                                                 shift p (labels k2)
        handler ex                          = throwError ex
        shift (TRow l k _ t r) (n:ns)       = TRow l KRow n t <$> shift r ns
        shift TNil{} ns                     = return k1
        shift _ _                           = noRed (Cast info (tTuple p1 k1) (tTuple p2 k2))
        labels (TRow _ _ n _ r)             = n : labels r
        labels _                            = []

----------------------------------------------------------------------------------------------------------------------
-- sub
----------------------------------------------------------------------------------------------------------------------

sub                                         :: Env -> ErrInfo -> Equations -> Name -> Type -> Type ->TypeM Equations
sub env info eq w t1 t2                     = do t1' <- usubst t1
                                                 t2' <- usubst t2
                                                 info' <- usubst info
                                                 sub' env info' eq w t1' t2'

sub'                                        :: Env -> ErrInfo -> Equations -> Name -> Type -> Type ->TypeM Equations

sub' env _ eq w t1@TWild{} t2               = return (idwit env w t1 t2 : eq)
sub' env _ eq w t1 t2@TWild{}               = return (idwit env w t1 t2 : eq)

--                     as declared               as called
--                     existing                  expected
sub' env info eq w t1@(TFun _ fx1 p1 k1 t1') t2@(TFun _ fx2 p2 k2 t2')
  | varTails [p1,p2] || varTails [k1,k2]    = do --traceM ("## Unifying funs: " ++ prstr w ++ ": " ++ prstr t1 ++ " ~ " ++ prstr t2)
                                                 unify info t1 t2
                                                 return (idwit env w t1 t2 : eq)
--  | any isTVar [p1,p2]                      = do --traceM ("## Unifying fun pos " ++ prstr w ++ ": " ++ prstr t1 ++ " < " ++ prstr t2)
--                                                 unify info p1 p2
--                                                 sub env info eq w t1 t2
--  | any isTVar [k1,k2]                      = do --traceM ("## Unifying fun kwd " ++ prstr w ++ ": " ++ prstr t1 ++ " < " ++ prstr t2)
--                                                 unify info k1 k2
--                                                 sub env info eq w t1 t2
  | otherwise                               = do --traceM ("### Aligning fun " ++ prstr w ++ ": " ++ prstr t1 ++ " < " ++ prstr t2)
                                                 (cs1,ap,es) <- subpos env info ((map eVar pNames)!!) 0 p2 p1
                                                 (cs2,ak) <- subkwd0 env info eVar es k2 k1
                                                 t1 <- usubst t1
                                                 t2 <- usubst t2
                                                 w' <- newWitness
                                                 let (TFun _ fx1 p1 k1 t1', TFun _ fx2 p2 k2 t2') = (t1, t2)
                                                     (pp,pk) = (pPar pNames p2, kPar attrKW k2)
                                                     lambda = eLambda [(px0,t1)] $ Lambda l0 pp pk (eCallVar w' [Call l0 (eVar px0) ap ak]) fx1
                                                 reduce env (Eqn w (wFun t1 t2) lambda : eq) (Cast info fx1 fx2 : Sub info w' t1' t2':cs1++cs2)

--                     existing            expected
sub' env info eq w t1@(TTuple _ p1 k1) t2@(TTuple _ p2 k2)
  | varTails [p1,p2] || varTails [k1,k2]    = do --traceM ("### Unifying tuples: " ++ prstr w ++ ": " ++ prstr t1 ++ " ~ " ++ prstr t2)
                                                 unify info t1 t2
                                                 return (idwit env w t1 t2 : eq)
--  | any isTVar [p1,p2]                      = do traceM ("### Unifying tuple pos: " ++ prstr w ++ ": " ++ prstr t1 ++ " < " ++ prstr t2)
--                                                 unify info p1 p2
--                                                 sub env info eq w t1 t2
--  | any isTVar [k1,k2]                      = do traceM ("### Unifying tuple kwd: " ++ prstr w ++ ": " ++ prstr t1 ++ " < " ++ prstr t2)
--                                                 unify info k1 k2
--                                                 sub env info eq w t1 t2
  | otherwise                               = do --traceM ("### Aligning tuple " ++ prstr w ++ ": " ++ prstr t1 ++ " < " ++ prstr t2)
                                                 (cs1,ap,es) <- subpos env info (eDotI (eVar px0) . toInteger) 0 p1 p2
                                                 (cs2,ak) <- subkwd0 env info (eDot (eVar px0)) es k1 k2
                                                 t1 <- usubst t1
                                                 t2 <- usubst t2
                                                 let (TTuple _ p1 k1, TTuple _ p2 k2) = (t1, t2)
                                                     lambda = eLambda [(px0,t1)] (Paren l0 $ Tuple l0 ap ak)
                                                 reduce env (Eqn w (wFun t1 t2) lambda : eq) (cs1++cs2)

sub' env info eq w (TVar _ tv) t2@TFun{}
  | univar tv                               = do t1 <- instwild env KType $ tFun tWild tWild tWild tWild
                                                 usubstitute tv t1
                                                 sub env info eq w t1 t2
sub' env info eq w t1@TFun{} (TVar _ tv)                                                                             -- Should remove this, rejects tv = TOpt...
  | univar tv                               = do t2 <- instwild env KType $ tFun tWild tWild tWild tWild
                                                 usubstitute tv t2
                                                 sub env info eq w t1 t2

sub' env info eq w (TVar _ tv) t2@TTuple{}
  | univar tv                               = do t1 <- instwild env KType $ tTuple tWild tWild
                                                 usubstitute tv t1
                                                 sub env info eq w t1 t2

sub' env info eq w t1@TTuple{} t2@(TVar _ tv)
  | univar tv                               = do defer [Sub info w t1 t2]; return eq        -- Don't let cast solve this by idwit!

sub' env info eq w t1@(TVar _ tv1) t2@(TVar _ tv2)
  | tv1 == tv2                              = return (idwit env w t1 t2 : eq)
  | univar tv1 && univar tv2                = do defer [Sub info w t1 t2]; return eq

sub' env info eq w t1 t2                    = do cast env info t1 t2
                                                 return (idwit env w t1 t2 : eq)


rowTail (TRow _ _ _ _ r)                    = rowTail r
rowTail r                                   = r

varTails                                    = all (isTVar . rowTail)

rowShape (TRow _ k n t r)                   = do t' <- newUnivar
                                                 r' <- rowShape r
                                                 return (tRow k n t' r')
rowShape (TStar _ k r)                      = do r' <- rowShape r
                                                 return (tStar k r')
rowShape r                                  = return r

subpos                                      :: Env -> ErrInfo -> (Int -> Expr) -> Int -> PosRow -> PosRow -> TypeM (Constraints, PosArg, [(Expr,Type)])
subpos env info f i TVar{}         TVar{}   = error "INTERNAL ERROR: subpos"
subpos env info f i (TVar _ tv)     r2
  | tv `elem` ufree r2                     = conflictingRow tv                     -- use rowTail?
  | otherwise                               = do r1 <- rowShape r2
                                                 --traceM (" ## subpos L " ++ prstr tv ++ " ~ " ++ prstr r1)
                                                 usubstitute tv r1
                                                 subpos env info f i r1 r2
subpos env info f i r1             (TVar _ tv)
  | tv `elem` ufree r1                     = conflictingRow tv                     -- use rowTail?
  | otherwise                               = do r2 <- rowShape r1
                                                 --traceM (" ## subpos R " ++ prstr r2 ++ " ~ " ++ prstr tv)
                                                 usubstitute tv r2
                                                 subpos env info f i r1 r2

subpos env info f i (TRow _ _ _ t1 r1) (TRow _ _ _ t2 r2)
                                            = do --traceM (" ## subpos A " ++ prstr t1 ++ " < " ++ prstr t2)
                                                 (cs,as,es) <- subpos env info f (i+1) r1 r2
                                                 w <- newWitness
                                                 return (Sub info w t1 t2 : cs, PosArg (eCallVar w [f i]) as, es)
subpos env info f i (TStar _ _ r1)     (TStar _ _ r2)
                                            = do --traceM (" ## subpos B " ++ prstr (tTupleP r1) ++ " < " ++ prstr (tTupleP r2))
                                                 w <- newWitness
                                                 return ([Sub info w (tTupleP r1) (tTupleP r2)], PosStar (eCallVar w [f i]), [])
subpos env info f i TNil{}             TNil{}
                                            = do --traceM (" ## subpos C ")
                                                 return ([], PosNil, [])

subpos env info f i (TStar _ _ r1)     r2   = do --traceM (" ## subpos D " ++ prstr r1 ++ " ~ " ++ prstr r2)
                                                 subpos env info (eDotI (f i) . toInteger) 0 r1 r2
subpos env info f i r1                (TStar _ _ r2)
                                            = do --traceM (" ## subpos E " ++ prstr r1 ++ " ~ " ++ prstr r2)
                                                 (cs,as,es) <- subpos env info f i r1 r2
                                                 return (cs, PosStar (eTupleP as), es)

subpos env info f i r1@TNil{}          r@(TRow _ _ _ t2 r2)
  | TOpt{} <- t2                            = do --traceM (" ## subpos F Opt ~ " ++ prstr t2)
                                                 (cs,as,es) <- subpos env info f i r1 r2
                                                 return (cs, PosArg eNone as, es)
  | otherwise                               = do --traceM (" ## subpos G Nil ~ " ++ prstr r)
                                                 posElemNotFound0 env True (Cast info r1 r) nWild
subpos env info f i (TRow _ _ _ t1 r1) r2@TNil{}
                                            = do --traceM (" ## subpos H " ++ prstr t1 ++ " = " ++ prstr (f i))
                                                 (cs,as,es) <- subpos env info f (i+1) r1 r2
                                                 return (cs, as, (f i, t1) : es)


-----------------------

subkwd0                                     :: Env -> ErrInfo -> (Name -> Expr) -> [(Expr,Type)] -> KwdRow -> KwdRow -> TypeM (Constraints, KwdArg)
subkwd0 env info f [] r1 r2                 = subkwd env info f [] r1 r2
subkwd0 env info f ((e,t1):es) r1 (TRow _ _ n t2 r2)
                                            = do --traceM (" ## subkwd0 extra pos for " ++ prstr n ++ ": " ++ prstr t1 ++ " < " ++ prstr t2)
                                                 (cs,as) <- subkwd0 env info f es r1 r2
                                                 w <- newWitness
                                                 return (Sub info w t1 t2 : cs, KwdArg n (eCallVar w [e]) as)
  where labels (TRow _ _ n _ r)             = n : labels r
        labels _                            = []
subkwd0 env info f ((e,t1):es) r1 r2        = posElemNotFound0 env False (Cast info r1 r2) nWild

subkwd                                      :: Env -> ErrInfo -> (Name -> Expr) -> [Name] -> KwdRow -> KwdRow -> TypeM (Constraints, KwdArg)
subkwd env info f seen r1 (TVar _ tv)       = do unif f seen r1
                                                 r2 <- usubst (tVar tv)
                                                 subkwd env info f seen r1 r2
  where unif f seen TVar{}                  = error "INTERNAL ERROR: subkwd"
        unif f seen (TRow _ _ n t r)
          | n `elem` seen                   = do --traceM (" ## subkwd (Row) - Var: " ++ prstr (tRow KRow n t r) ++ " [" ++ prstrs seen ++ "] ≈ " ++ prstr tv)
                                                 unif f (seen\\[n]) r
          | tv `elem` ufree r              = conflictingRow tv                     -- use rowTail?
          | otherwise                       = do --traceM (" ## subkwd Row - Var: " ++ prstr (tRow KRow n t r) ++ " [" ++ prstrs seen ++ "] ≈ " ++ prstr tv)
                                                 t2 <- newUnivar
                                                 r2 <- tRow KRow n t2 <$> newUnivarOfKind KRow
                                                 unify info (tVar tv) r2
        unif f seen (TStar _ _ r)
          | tv `elem` ufree r              = conflictingRow tv                     -- use rowTail?
          | otherwise                       = do --traceM (" ## subkwd Star - Var: " ++ prstr (tStar KRow r) ++ " [" ++ prstrs seen ++ "] ≈ " ++ prstr tv)
                                                 r2 <- tStar KRow <$> newUnivarOfKind KRow
                                                 unify info (tVar tv) r2
        unif f seen TNil{}                  = do --traceM (" ## subkwd Nil - Var: " ++ prstr (tNil KRow) ++ " [" ++ prstrs seen ++ "] ≈ " ++ prstr tv)
                                                 r2 <- pure $ tNil KRow
                                                 unify info (tVar tv) r2

subkwd env info f seen r1 (TRow _ _ n2 t2 r2)
                                            = do (cs1,e) <- pick f seen r1
                                                 r1 <- usubst r1
                                                 r2 <- usubst r2
                                                 (cs2,as) <- subkwd env info f (n2:seen) r1 r2
                                                 return (cs1++cs2, KwdArg n2 e as)
  where pick f seen (TVar _ tv)
          | tv `elem` ufree r2             = conflictingRow tv                     -- use rowTail?
          | otherwise                       = do --traceM (" ## subkwd Var - Row: " ++ prstr (tVar tv) ++ " [" ++ prstrs seen ++ "] ≈ " ++ prstr (tRow KRow n2 t2 r2))
                                                 r1 <- tRow KRow n2 t2 <$> newUnivarOfKind KRow
                                                 unify info (tVar tv) r1
                                                 pick f seen r1
        pick f seen (TRow _ _ n t r)
          | n `elem` seen                   = do --traceM (" ## subkwd (Row) - Row: " ++ prstr (tRow KRow n t r) ++ " [" ++ prstrs seen ++ "] ≈ " ++ prstr (tRow KRow n2 t2 r2))
                                                 pick f (seen\\[n]) r
          | n /= n2                         = pick f seen r
          | otherwise                       = do --traceM (" ## subkwd Row! - Row: " ++ prstr (tRow KRow n t r) ++ " [" ++ prstrs seen ++ "] ≈ " ++ prstr (tRow KRow n2 t2 r2))
                                                 w <- newWitness
                                                 return ([Sub info w t t2], eCallVar w [f n])
        pick f seen (TStar _ _ r)           = do --traceM (" ## subkwd Star - Row: " ++ prstr (tStar KRow r) ++ " [" ++ prstrs seen ++ "] ≈ " ++ prstr (tRow KRow n2 t2 r2))
                                                 pick (eDot (f attrKW)) seen r
        pick f seen (TNil _ _)
          | TOpt{} <- t2                    = do --traceM (" ## subkwd None - Row: " ++ prstr (tNil KRow) ++ " [" ++ prstrs seen ++ "] ≈ " ++ prstr (tRow KRow n2 t2 r2))
                                                 return ([], eNone)
          | otherwise                       = kwdNotFound0 env info n2

subkwd env info f seen r1 (TStar _ _ r2)    = do (cs,e) <- match f seen r1
                                                 return (cs, KwdStar e)
  where match f seen (TVar _ tv)
          | tv `elem` ufree r2             = conflictingRow tv                     -- use rowTail?
          | otherwise                       = do --traceM (" ## subkwd Var - Star: " ++ prstr (tVar tv) ++ " [" ++ prstrs seen ++ "] ≈ " ++ prstr (tStar KRow r2))
                                                 r1 <- tStar KRow <$> newUnivarOfKind KRow
                                                 unify info (tVar tv) r1
                                                 match f seen r1
        match f seen r1@(TRow _ _ n t r)
          | n `elem` seen                   = do --traceM (" ## subkwd (Row) - Star: " ++ prstr (tRow KRow n t r) ++ " [" ++ prstrs seen ++ "] ≈ " ++ prstr (tStar KRow r2))
                                                 match f (seen\\[n]) r
          | otherwise                       = do --traceM (" ## subkwd Row - Star: " ++ prstr (tRow KRow n t r) ++ " [" ++ prstrs seen ++ "] ≈ " ++ prstr (tStar KRow r2))
                                                 (cs,as) <- subkwd env info f seen r1 r2
                                                 return (cs, eTupleK as)
        match f seen r1@(TStar _ _ r)
          | TVar{} <- r, TVar{} <- r2       = do --traceM (" ## subkwd StarVar - StarVar: " ++ prstr (tStar KRow r) ++ " [" ++ prstrs seen ++ "] ≈ " ++ prstr (tStar KRow r2))
                                                 unify info r r2
                                                 return ([], f attrKW)
          | TVar{} <- r                     = do --traceM (" ## subkwd StarVar - Star: " ++ prstr (tStar KRow r) ++ " [" ++ prstrs seen ++ "] ≈ " ++ prstr (tStar KRow r2))
                                                 (cs,as) <- subkwd env info f seen r1 r2
                                                 return (cs, eTupleK as)
          | otherwise                       = do --traceM (" ## subkwd Star - Star: " ++ prstr (tStar KRow r) ++ " [" ++ prstrs seen ++ "] ≈ " ++ prstr (tStar KRow r2))
                                                 match (eDot (f attrKW)) seen r
        match f seen r1@TNil{}              = do --traceM (" ## subkwd Nil - Star: " ++ prstr (tNil KRow) ++ " [" ++ prstrs seen ++ "] ≈ " ++ prstr (tStar KRow r2))
                                                 (cs,as) <- subkwd env info f seen r1 r2
                                                 return (cs, eTupleK as)

subkwd env info f seen r1 TNil{}                = term f seen r1
  where term f seen (TVar _ tv)             = do --traceM (" ## subkwd Var - Nil: " ++ prstr (tVar tv) ++ " [" ++ prstrs seen ++ "] ≈ " ++ prstr (tNil KRow))
                                                 r1 <- pure $ tNil KRow
                                                 unify info (tVar tv) r1
                                                 term f seen (tNil KRow)
        term f seen (TRow _ _ n t r)
          | n `elem` seen                   = do --traceM (" ## subkwd (Row) - Nil: " ++ prstr (tRow KRow n t r) ++ " [" ++ prstrs seen ++ "] ≈ " ++ prstr (tNil KRow))
                                                 term f (seen\\[n]) r
          | otherwise                       = do --traceM (" ## subkwd Row - Nil: " ++ prstr (tRow KRow n t r) ++ " [" ++ prstrs seen ++ "] ≈ " ++ prstr (tNil KRow))
                                                 kwdUnexpected info n
        term f seen (TStar _ _ r)           = do --traceM (" ## subkwd Star - Nil: " ++ prstr (tStar KRow r) ++ " [" ++ prstrs seen ++ "] ≈ " ++ prstr (tNil KRow))
                                                 term f seen r
        term f seen (TNil _ _)              = do --traceM (" ## subkwd Nil - Nil: " ++ prstr (tNil KRow) ++ " [" ++ prstrs seen ++ "] ≈ " ++ prstr (tNil KRow))
                                                 return ([], KwdNil)


{-

---- OK:

x.          c,a             a,c                     a = x.a                                 Row!            Row
x.          c,a             c           a           c = x.a                                 Row!            Row
x.          c,a             .           ac                                                  (Row)           Nil
x.          .               .                       .                                       Nil             Nil

---- OK:

x.          c,a*(b*A)       a,b,c,e                 a = (x.)a, ...                          Row!            Row
x.          c,a*(b*A)       b,c,e       a                                                   Star            Row
x.KW.           b*A         b,c,e       a           b = (x.KW.)b                                Row!        Row
x.          c,a*(b*A)       c,e         ab          c = (x.)c, ...                          Row!            Row
x.          c,a*(b*A)       e           abc                                                 Star            Row
x.KW.           b*A         e           abc                                                     StarVar     Row
x.KW.KW.            A       e           abc                             A ~ e,B                 Var         Row
x.KW.KW.            e,B     e           abc         e = (x.KW.KW.)e, ...                        Row!        Row
x.          c,a*(b*(e,B))   .           abce                                                (Row)           Nil
x.          *(b*(e,B))      .           be                                                  Star            Nil
x.KW.           b*(e,B)     .           be                                                      (Row)       Nil
x.KW.           *(e,B)      .           e                                                       Star        Nil
x.KW.KW             e,B     .           e                                                       (Row)       Nil
x.KW.KW             B       .                                           B ~ .                   Var         Nil
                    .       .                                                                   Nil         Nil

x = (c = 1, a = 2, KW = (b = 4, KW = (e = 5)))                          (a = x.a, b = x.KW.b, c = x.c, e = x.KW.KW.e)

---- OK:

x.          c*(b*A)         c*X                     c = x.c                                 Row!            Row
x.          c*(b*A)         *X          c                                                   (Row)           Star
x.          *(b*A)          *X                                                              Star            Star
x.KW.           b*A         *X                      KW = (...)                                  Row         Star
x.KW.           b*A             X                                       X ~ b,Y                 Row             Var
x.KW.           b*A             b,Y                     b = x.KW.b                              Row!            Row
x.KW.           b*A             Y       b                                                       (Row)           Var
x.KW.           *A              Y                                       Y ~ *Z                  StarVar         Var
x.KW.           *A              *Z                      KW = x.KW.KW    A ~ Z                   StarVar         StarVar

x = (c = 1, KW = (b = 4, KW = y))                                       (c = x.c, KW = (b = x.KW.b, KW = x.KW.KW))

---- OK:

x.          c,a*(b*A)       a,b,c*X                 a = (x.)a, ...                          Row!            Row
x.          c,a*(b*A)       b,c*X       a                                                   Star            Row
x.KW.           b*A         b,c*X       a           b = (x.KW.)b, ...                           Row!        Row
x.          c,a*(b*A)       c*X         ab          c = (x.)c, ...                          Row!            Row
x.          c,a*(b*A)       *X          abc                                                 (Row)           Star
x.          *(b*A)          *X          b                                                   Star            Star
x.KW.           b*A         *X          b                                                       (Row)       Star
x.KW.           *A          *X                      KW = (x.KW.)KW      A ~ X                   StarVar     StarVar

x = (c = 1, a = 2, KW = (b = 4, KW = y))                                (a = x.a, b = x.KW.b, c = x.c, KW = x.KW.KW)

---- OK:

x.          c,a*(b*A)       a,b,c,e*X               a = (x.)a, ...                          Row!            Row
x.          c,a*(b*A)       b,c,e*X     a                                                   Star            Row
x.KW.           b*A         b,c,e*X                 b = (x.KW.)b, ...                           Row!        Row
x.          c,a*(b*A)       c,e*X       ab          c = (x.)c, ...                          Row!            Row
x.          c,a*(b*A)       e*X         abc                                                 Star            Row
x.KW.           b*A         e*X         b                                                       Star        Row
x.KW.KW.            A       e*X         b                               A ~ e,B                     Var     Row
x.KW.KW.            e,B     e*X         b           e = (x.KW.KW.)e                                 Row!    Row
x.          c,a*(b*(e,B))   *X          abce                                                (Row)           Star
x.          *(b*(e,B))      *X          be                                                  Star            Star
x.KW.           b*(e,B)     *X          be                                                      (Row)       Star
x.KW.           *(e,B)      *X          e                                                       Star        Star
x.KW.KW.            e,B     *X          e                                                           (Row)   Star
x.KW.KW.            B       *X                                          B ~ *X                      Var     Star
x.KW.KW.            *X      *X                      KW = (x.KW.KW.)KW                               StarVar StarVar

x = (c = 1, a = 2, KW = (b = 4, KW = (e = 5, KW = y)))                  (a = x.a, b = x.KW.b, c = x.c, e = x.KW.KW.e, KW = x.KW.KW.KW)

---- OK:

x.          c,a*A           a,b,c,e*X               a = (x.)a, ...                          Row!        Row
x.          c,a*A           b,c,e*X     a                                                   StarVar     Row
x.KW.           A           b,c,e*X                                     A ~ b,B                 Var     Row
x.KW.           b,B         b,c,e*X                 b = (x.KW.)b, ...                           Row!    Row
x.          c,a*(b,B)       c,e*X       ab          c = (x.)c, ...                          Row!        Row
x.          c,a*(b,B)       e*X         abc                                                 Star        Row
x.KW.           b,B         e*X         b                               B ~ e,C                 Var     Row
x.KW.           b,e,C       e*X         b           e = (x.KW.)e, ...                           Row!    Row
x.          c,a*(b,e,C)     *X          abce                                                (Row)       Star
x.          *(b,e,C)        *X          be                                                  Star        Star
x.KW.           b,e,C       *X          be                                                      (Row)   Star
x.KW.           C           *X                                          C ~ *X                  Var     Star
x.KW.           *X          *X                      KW = (x.KW.)KW                              StarVar StarVar

x = (c = 1, a = 2, KW = (b = 4, e = 5, KW = y))                         (a = x.a, b = x.KW.b, c = x.c, e = x.KW.e, KW = x.KW.KW)

---- OK:

x.          c,a,d*A         a,b,c,e*X               a = (x.)a, ...                          Row!        Row
x.          c,a,d*A         b,c,e*X     a                                                   StarVar     Row
x.KW.           A           b,c,e*X     a                               A ~ b,B                 Var     Row
x.KW.           b,B         b,c,e*X     a           b = (x.KW.)b, ...                           Row!    Row
x.          c,a,d*(b,B)     c,e*X       ab          c = x.c, ...                            Row!        Row
x.          c,a,d*(b,B)     e*X         abc                                                 Star        Row
x.KW.           b,B         e*X         b                               B ~ e,C                 Var     Row
x.KW.           b,e,C       e*X         b           e = x.KW.e, ...                             Row!    Row
x.          c,a,d*(b,e,C)   *X          abce                                                (Row)       Star
x.          d*(b,e,C)       *X          be          KW = (...)                              Row         Star
x.          d*(b,e,C)           X       be                              X ~ d,Y             Row             Var
x.          d*(b,e,C)           d,Y     be              d = (x.)d, ...                      Row!            Row
x.          d*(b,e,C)           Y       bed                                                 (Row)           Var
x.          *(b,e,C)            Y       be                              Y ~ *Z              Star            Var
x.          *(b,e,C)            *Z      be                                                  Star            Star
x.KW.           b,e,C           *Z      be                                                      (Row)       Star
x.KW.           C               *Z                                      C ~ *Z                  Var         Star
x.KW.           *Z              *Z                      KW = (x.KW.)KW                          StarVar     StarVar

x = (c = 1, a = 2, d = 3, KW = (b = 4, e = 5, KW = y))                  (a = x.a, b = x.KW.b, c = x.c, e = x.KW.e, KW = (d = x.d, KW = x.KW.KW))


---- OK:

x.          c,a*A           a,c*X                   a = (x.)a, ...                          Row!        Row
x.          c,a*A           c*X         a           c = (x.)c, ...                          Row!        Row
x.          c,a*A           *X          ac                                                  (Row)       Star
x.          *A              *X                      KW = (x.)KW         A ~ X               StarVar     StarVar

x = (c = 1, a = 2, KW = y)                                              (a = x.a, c = x.c, KW = x.KW)

---- OK:

x.          c,a,d*A         a,c*X                   a = (x.)a, ...                          Row!        Row
x.          c,a,d*A         c*X         a           c = (x.)c, ...                          Row!        Row
x.          c,a,d*A         *X          ac                                                  (Row)       Star
x.          d*A             *X                      KW = (...)                              Row         Star
x.          d*A                 X                                       X ~ d,Y             Row             Var
x.          d*A                 d,Y                     d = (x.)d, ...                      Row!            Row
x.          d*A                 Y       d                                                   (Row)           Var
x.          *A                  Y                                       Y ~ *A              Star            Var
x.          *A                  *A                      KW = (x.)KW                         StarVar         StarVar

x = (c = 1, a = 2, d = 3, KW = y)                                       (a = x.a, c = x.c, KW = (d = x.d, KW = x.KW))

---- OK:

x.          c,a,d*A         a,c*(d*X)               a = (x.)a, ...                          Row!        Row
x.          c,a,d*A         c*(d*X)     a           c = (x.)c, ...                          Row!        Row
x.          c,a,d*A         *(d*X)      ac                                                  (Row)       Star
x.          d*A             *(d*X)                  KW = (...)                              Row         Star
x.          d*A                 d*X                     d = x.d, ...                        Row!            Row
x.          d*A                 *X      d                                                   (Row)           Star
x.          *A                  *X                      KW = (x.)KW     A ~ X               StarVar         StarVar

x = (c = 1, a = 2, d = 3, KW = y)                                       (a = x.a, c = x.c, KW = (d = x.d, KW = x.KW))

---- OK:

x.          c,a*A           a,c*(d*X)               a = (x.)a                               Row!        Row
x.          c,a*A           c*(d*X)     a           c = (x.)c                               Row!        Row
x.          c,a*A           *(d*X)      ac                                                  (Row)       Star
x.          *A              *(d*X)                  KW = (...)                              StarVar     Star
x.          *A                  d*X                                                         Star            Row
x.KW.           A               d*X                                     A ~ d,B             Var             Row
x.KW.           d,B             d*X                     d = x.KW.d                          Row!            Row
x.          *(d,B)              d*X     d                                                   Star            Star
x.KW.           d,B             *X      d                                                   (Row)           Star
x.KW.           B               *X                                      B ~ *X              Var             Star
x.KW.           *X              *X                      KW = x.KW.KW                        StarVar         StarVar

x = (c = 1, a = 2, KW = (d = 3, KW = y))                                (a = x.a, c = x.c, KW = (d = x.KW.d, KW = x.KW.KW))

---- OK:

x.          c,a,d           a*X                     a = x.a                                 Row!        Row
x.          c,a,d           *X          a                                                   (Row)       Star
x.          c,d             *X                      KW = (...)                              Row         Star
x.          c,d                 X                                       X ~ c,Y             Row             Var
x.          c,d                 c,Y                     c = x.c                             Row!            Row
x.          c,d                 Y       c                               Y ~ d,Z             Row             Var
x.          c,d                 d,Z     c               d = x.d                             Row!            Row
x.          c,d                 Z       cd                                                  (Row)           Var
x.          .                   Z                                       Z ~ .               Nil             Var

x = (c = 1, a = 2, d = 3)                                               (a = x.a, KW = (c = x.c, d = x.d))

---- OK:

x.          c,a,d*A         a*X                     a = x.a                                 Row!        Row
x.          c,a,d*A         *X          a                                                   (Row)       Star
x.          c,d*A           *X                      KW = (...)                              Row         Star
x.          c,d*A               X                                       X ~ c,Y             (Row)           Var
x.          c,d*A               c,Y                     c = x.c                             Row!            Row
x.          c,d*A               Y       c                               Y ~ d,Z             (Row)           Var
x.          c,d*A               d,Z     c               d = x.d                             Row!            Row
x.          c,d*A               Z       cd                                                  (Row)           Var
x.          *A                  Z                                       Z ~ *A              Star            Var
x.          *A                  *A                      KW = x.KW                           StarVar         StarVar

x = c = 1, a = 2, d = 3, KW = y)                                        (a = x.a, KW = (c = x.c, d = x.d, KW = x.KW))

-}



{-
subpos 0 (A,B,*R) (A,B,C,D)                     = Arg x.0 $ subpos 1 (B,*R) (B,C,D)                                     f = x.
                                                = Arg x.0 $ Arg x.1 $ subpos 2 (*R) (C,D)
                                                = Arg x.0 $ Arg x.1 $ subpos 0 R (C,D)                                  f = x.2.   R ~ (C,D)
                                                = Arg x.0 $ Arg x.1 $ subpos 0 (C,D) (C,D)
                                                = Arg x.0 $ Arg x.1 $ Arg x.2.0 $ subpos 1 (D) (D)
                                                = Arg x.0 $ Arg x.1 $ Arg x.2.0 $ Arg x.2.1 $ subpos 2 () ()
                                                = Arg x.0 $ Arg x.1 $ Arg x.2.0 $ Arg x.2.1 $ Nil

subpos 0 (A,B,C,D) (A,B,*S)                     = Arg x.0 $ subpos 1 (B,C,D) (B,*S)                                     f = x.
                                                = Arg x.0 $ Arg x.1 $ subpos 2 (C,D) (*S)
                                                = Arg x.0 $ Arg x.1 $ Arg (subpos 2 (C,D) S) Nil
                                                = Arg x.0 $ Arg x.1 $ Arg (subpos 2 (C,D) (C,D)) Nil
                                                = Arg x.0 $ Arg x.1 $ Arg (Arg x.2 $ subpos 3 (D) (D)) Nil
                                                = Arg x.0 $ Arg x.1 $ Arg (Arg x.2 $ Arg x.3 $ subpos 4 () ()) Nil
                                                = Arg x.0 $ Arg x.1 $ Arg (Arg x.2 $ Arg x.3 $ Nil) Nil

subpos 0 (A,*R) (A,B,*S)                        = Arg x.0 $ subpos 1 (*R) (B,*S)                                        f = x.
                                                = Arg x.0 $ subpos 0 R (B,*S)                                           f = x.1.    R ~ (B,*S)
                                                = Arg x.0 $ subpos 0 (B,*S) (B,*S)
                                                = Arg x.0 $ Arg x.1.0 $ subpos 1 (*S) (*S)
                                                = Arg x.0 $ Arg x.1.0 $ Arg w(x.1.1) Nil

-}


----------------------------------------------------------------------------------------------------------------------
-- Variable info
----------------------------------------------------------------------------------------------------------------------

data VInfo                                  = VInfo {
                                                varvars     :: [(TUni,TUni)],
                                                embedded    :: [TUni],
                                                sealed      :: [TUni],
                                                ubounds     :: Map TUni [Type],
                                                lbounds     :: Map TUni [Type],
                                                pbounds     :: Map TUni [(Name,PCon)],
                                                mutattrs    :: Map TUni [Name],
                                                selattrs    :: Map TUni [Name] }

varvar v1 v2 vi                             = vi{ varvars = (v1,v2) : varvars vi }
embed vs vi                                 = vi{ embedded = vs ++ embedded vi }
seal v vi                                   = vi{ sealed = v : sealed vi }
ubound v t vi                               = vi{ ubounds = Map.insertWith (++) v [t] (ubounds vi) }
lbound v t vi                               = vi{ lbounds = Map.insertWith (++) v [t] (lbounds vi) }
pbound v w p vi                             = vi{ pbounds = Map.insertWith (++) v [(w,p)] (pbounds vi) }
mutattr v n vi                              = vi{ mutattrs = Map.insertWith (++) v [n] (mutattrs vi) }
selattr v n vi                              = vi{ selattrs = Map.insertWith (++) v [n] (selattrs vi) }

lookup' v m                                 = maybe [] id $ Map.lookup v m

varinfo cs                                  = f cs (VInfo [] [] [] Map.empty Map.empty Map.empty Map.empty Map.empty)
  where
    f (Cast _ (TVar _ v1) (TVar _ v2) : cs)
      | v1 == v2                            = f cs
      | univar v1, univar v2                = f cs . varvar v1 v2
    f (Cast _ (TVar _ v) t : cs)
      | univar v                            = f cs . ubound v t . embed (ufree t)
    f (Cast _ t (TVar _ v) : cs)
      | univar v                            = f cs . lbound v t . embed (ufree t)
    f (Sub _ _ (TVar _ v1) (TVar _ v2) : cs)
      | v1 == v2                            = f cs
      | univar v1, univar v2                = f cs . varvar v1 v2
    f (Sub _ _ (TVar _ v) t : cs)
      | univar v                            = f cs . ubound v t . embed (ufree t)
    f (Sub _ _ t (TVar _ v) : cs)
      | univar v                            = f cs . lbound v t . embed (ufree t)
    f (Impl _ w (TVar _ v) p : cs)
      | univar v                            = f cs . pbound v w p . embed (ufree p)
    f (Impl _ w t p : cs)
      | not $ null vs                       = f cs . embed (vs ++ ufree p)
      where vs                              = filter univar $ ufree t
    f (Mut _ (TVar _ v) n t : cs)
      | univar v                            = f cs . mutattr v n . embed (ufree t)
    f (Sel _ _ (TVar _ v) n t : cs)
      | univar v                            = f cs . selattr v n . embed (ufree t)
    f (Seal _ (TVar _ v) : cs)
      | univar v                            = f cs . seal v
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

gsimp vi cl obs []                      = []
gsimp vi cl obs ((x,y):xys)
  | not subsumed                        = gsimp vi cl obs xys
  | x_obs && y_obs                      = gsimp vi cl obs xys
  | x_obs                               = (x,y) : gsimp vi cl (y:obs) xys
  | y_obs                               = (x,y) : gsimp vi cl (x:obs) xys
  | otherwise                           = (x,y) : gsimp vi cl obs xys
  where subsumed                        = (above x cl \\ [y]) `eq` above y cl && (below y cl \\ [x]) `eq` below x cl &&
                                          lookup' x (ubounds vi) `subset` lookup' y (ubounds vi) && lookup' y (lbounds vi) `subset` lookup' x (lbounds vi) &&
                                          rng (lookup' x (pbounds vi)) `eq` rng (lookup' y (pbounds vi))
        a `eq` b                        = a `subset` b && b `subset` a
        a `subset` b                    = all (`elem` b) a
        x_obs                           = x `elem` obs
        y_obs                           = y `elem` obs

instwild env k (TWild _)                = newUnivarOfKind k
instwild env _ (TFun l e p k t)         = TFun l <$> instwild env KFX e <*> instwild env PRow p <*> instwild env KRow k <*> instwild env KType t
instwild env _ (TTuple l p k)           = TTuple l <$> instwild env PRow p <*> instwild env KRow k
instwild env _ (TOpt l t)               = TOpt l <$> instwild env KType t
instwild env _ (TCon l c)               = TCon l <$> instwildcon env c
instwild env _ (TRow l k n t r)         = TRow l k n <$> instwild env KType t <*> instwild env k r
instwild env _ (TStar l k r)            = TStar l k <$> instwild env k r
instwild env k t                        = return t

instwildcon env c                       = case tconKind (tcname c) env of
                                            KFun ks _ -> TC (tcname c) <$> sequence [ instwild env k t | (k,t) <- ks `zip` tcargs c ]
                                            _ -> return $ TC (tcname c) []


mkGLB env (v,ts)
  | Just t <- glbfold env ts'           = do t <- instwild env KType t
                                             --traceM ("   glb " ++ prstrs ts ++ " = " ++ prstr t)
                                             return (v, t)
  | otherwise                           = tyerrs ts ("No common subtype:")
  where ts'                             = map schematic ts


mkLUB env (v,ts)
  | Just t <- lubfold env ts'           = do t <- instwild env KType t
                                             --traceM ("   lub " ++ prstrs ts ++ " = " ++ prstr t)
                                             return (v, t)
  | otherwise                           = tyerrs ts ("No common supertype:")
  where ts'                             = map schematic ts


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
  | Nothing <- info                     = do --traceM ("  *Resubmit " ++ show (length cs))
                                             simplify' env te tt eq cs
  | Left (v,vs) <- closure              = do --traceM ("  *Unify cycle " ++ prstr v ++ " = " ++ prstrs vs)
                                             sequence [ unify (DfltInfo NoLoc 12 Nothing []) (tVar v) (tVar v') | v' <- vs ]
                                             simplify' env te tt eq cs
  | not $ null gsimple                  = do --traceM ("  *G-simplify " ++ prstrs [ (v,tVar v') | (v,v') <- gsimple ])
                                             --traceM ("  *obsvars: " ++ prstrs obsvars)
                                             --traceM ("  *varvars: " ++ prstrs (varvars vi))
                                             sequence [ unify (DfltInfo NoLoc 13 Nothing []) (tVar v) (tVar v') | (v,v') <- gsimple ]
                                             simplify' env te tt eq cs
  | not $ null cyclic                   = tyerrs cyclic ("Cyclic subtyping:")
  | not $ null (multiUBnd++multiLBnd)   = do ub <- mapM (mkGLB env) multiUBnd   -- GLB of the upper bounds
                                             lb <- mapM (mkLUB env) multiLBnd   -- LUB of the lower bounds
                                             --traceM ("  *GLB " ++ prstrs ub)
                                             --traceM ("  *LUB " ++ prstrs lb)
                                             let cs' = [ Cast (DfltInfo NoLoc 14 Nothing []) (tVar v) t | (v,t) <- ub ] ++ [ Cast (DfltInfo NoLoc 110 Nothing []) t (tVar v) | (v,t) <- lb ]
                                             simplify' env te tt eq (cs' ++ map (replace ub lb) cs)
  | not $ null posLBnd                  = do --traceM ("  *S-simplify (dn) " ++ prstrs posLBnd)
                                             --traceM ("   posnames "  ++ prstrs (posnames $ envX env))
                                             sequence [ unify (DfltInfo NoLoc 15 Nothing []) (tVar v) t | (v,t) <- posLBnd ]
                                             simplify' env te tt eq cs
  | not $ null negUBnd                  = do --traceM ("  *S-simplify (up) " ++ prstrs negUBnd)
                                             --traceM ("   posnames "  ++ prstrs (posnames $ envX env))
                                             sequence [ unify (DfltInfo NoLoc 16 Nothing []) (tVar v) t | (v,t) <- negUBnd ]
                                             simplify' env te tt eq cs
  | not $ null closUBnd                 = do --traceM ("  *Simplify upper closed bound " ++ prstrs closUBnd)
                                             sequence [ unify (DfltInfo NoLoc 17 Nothing []) (tVar v) t | (v,t) <- closUBnd ]
                                             simplify' env te tt eq cs
  | not $ null closLBnd                 = do --traceM ("  *Simplify lower closed bound " ++ prstrs closLBnd)
                                             sequence [ unify (DfltInfo NoLoc 18 Nothing []) (tVar v) t | (v,t) <- closLBnd ]
                                             simplify' env te tt eq cs
  | not $ null redEq                    = do --traceM ("  *(Context red) " ++ prstrs [ w | Eqn w _ _ <- redEq ])
                                             sequence [ unify (DfltInfo NoLoc 19 Nothing []) t1 t2 | (t1,t2) <- redUni ]
                                             simplify' env te tt (redEq++eq) (remove [ w | Eqn w _ _ <- redEq ] cs)
  | not $ null dots                     = do --traceM ("  *Implied mutation/selection solutions " ++ prstrs dots)
                                             (eq',cs') <- solveDots env mutC selC selP cs
                                             simplify' env te tt (eq'++eq) cs'
  | not $ null redSeal                  = do --traceM ("  *removing redundant Seal constraints on: " ++ prstrs redSeal)
                                             return (cs \\ map (Seal (DfltInfo NoLoc 110 Nothing []) . tVar) redSeal, eq)
  | otherwise                           = do --traceM ("  *improvement done " ++ show (length cs))
                                             return (cs, eq)
  where info                            = varinfo cs
        Just vi                         = info
        closure                         = varclose (varvars vi)
        Right vclosed                   = closure
        (vvsL,vvsU)                     = unzip vclosed
        gsimple                         = gsimp vi vclosed obsvars (varvars vi)
        multiUBnd                       = [ (v,us) | (v,ts) <- Map.assocs (ubounds vi), v `notElem` embedded vi, let us = unOpt ts, length us > 1, noLOpt v vi ]
        multiLBnd                       = [ (v,ts) | (v,ts) <- Map.assocs (lbounds vi), v `notElem` embedded vi, length ts > 1 ]
        multiPBnd                       = [ (v,ps) | (v,ps) <- Map.assocs (pbounds vi), length ps > 1 ]
        lowerBnd                        = [ (v,t) | (v,[t]) <- Map.assocs (lbounds vi), v `notElem` embedded vi ]
        upperBnd                        = [ (v,t) | (v,[t]) <- Map.assocs (ubounds vi), v `notElem` embedded vi ]
        posLBnd                         = [ (v,t) | (v,t) <- lowerBnd, v `notElem` negvars, implAll env (lookup' v $ pbounds vi) t ]
        negUBnd                         = [ (v,t) | (v,t) <- upperBnd, v `notElem` posvars, implAll env (lookup' v $ pbounds vi) t, noDots env vi v ]
        closLBnd                        = [ (v,t) | (v, [t]) <- Map.assocs (lbounds vi), upClosed env t, implAll env (lookup' v $ pbounds vi) t ]
        closUBnd                        = [ (v,t) | (v, [t]) <- Map.assocs (ubounds vi), dnClosed env t, implAll env (lookup' v $ pbounds vi) t, noDots env vi v ]
        (redEq,redUni)                  = ctxtReduce env vi multiPBnd
        mutC                            = findBoundAttrs env (mutattrs vi) (ubounds vi)
        selC                            = findBoundAttrs env (selattrs vi) (ubounds vi)
        selP                            = findWitAttrs env (selattrs vi) (pbounds vi)
        dots                            = dom mutC ++ dom selC ++ dom selP
        pvars                           = Map.keys (pbounds vi) ++ ufree (Map.elems (pbounds vi))
        dotvars                         = Map.keys (selattrs vi) ++ Map.keys (mutattrs vi)
        (posvars0,negvars0)             = polvars te `polcat` polvars tt `polcat` polvars env
        (posvars,negvars)               = (posvars0++vvsL, negvars0++vvsU)
        obsvars                         = posvars0 ++ negvars0 ++ pvars ++ dotvars ++ embedded vi ++ sealed vi
        boundvars                       = Map.keys (ubounds vi) ++ Map.keys (lbounds vi)
        boundprot                       = ufree (Map.elems $ ubounds vi) ++ ufree (Map.elems $ lbounds vi)
        cyclic                          = if null (boundvars\\boundprot) then [ c | c <- cs, headvar c `elem` boundvars ] else []
        redSeal                         = sealed vi \\ (posvars ++ negvars ++ embedded vi ++ Map.keys (ubounds vi) ++ Map.keys (lbounds vi)
                                          ++ Map.keys (pbounds vi) ++ Map.keys (mutattrs vi) ++ Map.keys (selattrs vi))

dnClosed env (TCon _ c)                 = isActor env (tcname c)
dnClosed env (TFX _ FXPure)             = True
dnClosed env (TFX _ FXAction)           = True
dnClosed env (TNone _)                  = True
dnClosed env (TNil _ _)                 = True
dnClosed env _                          = False

upClosed env (TFX _ FXProc)             = True
upClosed env (TOpt _ _)                 = True
upClosed env (TNil _ _)                 = True
upClosed env _                          = False

findBoundAttrs env attrs bounds         = [ ((v,n),wsc) | (v,ns) <- Map.assocs attrs, n <- ns, wsc <- bounds' v n ]
  where bounds' v n                     = [ wsc | TCon _ c <- lookup' v bounds, Just wsc <- [findAttr env c n] ]

findWitAttrs env attrs bounds           = [ ((v,n), (p, wexpr ws $ eVar w)) | (v,ns) <- Map.assocs attrs, n <- ns, (w,p,ws) <- bounds' v n ]
  where bounds' v n                     = [ (w,p,ws) | (w,p0) <- lookup' v bounds, (ws,p) <- findAncestry env p0, n `elem` conAttrs env (tcname p) ]


implAll env [] t                        = True
implAll env ps t@TCon{}                 = and [ hasWitness env t p | (w,p) <- ps ]
implAll env ps t@TFX{}                  = and [ hasWitness env t p | (w,p) <- ps ]
implAll env ps t@TOpt{}                 = all ((`elem` [qnIdentity,qnEq]) . tcname . snd) ps
implAll env ps t                        = False

noDots env vi v                         = null (lookup' v $ selattrs vi) && null (lookup' v $ mutattrs vi)

noLOpt v vi                             = not $ any optCon $ lookup' v (lbounds vi)
  where optCon TNone{}                  = True
        optCon TOpt{}                   = True
        optCon _                        = False

unOpt []                                = []
unOpt (TOpt _ TVar{} : ts)              = unOpt ts
unOpt (TOpt _ t : ts)                   = t : unOpt ts
unOpt (t : ts)                          = t : unOpt ts

replace ub lb c@(Cast _ (TVar _ v1) (TVar _ v2))
  | univar v1, univar v2                = c
replace ub lb c@(Cast _ (TVar _ v1) (TOpt _ (TVar _ v2)))
  | univar v1, univar v2                = c
replace ub lb (Cast info (TVar _ v) t)
  | Just t' <- lookup v ub              = Cast info t' t
replace ub lb (Cast info t (TVar _ v))
  | Just t' <- lookup v lb              = Cast info t t'
replace ub lb c@(Sub _ _ (TVar _ v1) (TVar _ v2))
  | univar v1, univar v2                = c
replace ub lb c@(Sub _ _ (TVar _ v1) (TOpt _ (TVar _ v2)))
  | univar v1, univar v2                = c
replace ub lb (Sub info w (TVar _ v) t)
  | Just t' <- lookup v ub              = Sub info w t' t
replace ub lb (Sub info w t (TVar _ v))
  | Just t' <- lookup v lb              = Sub info w t t'
replace ub lb c                         = c

solveDots env mutC selC selP cs         = do (eqs,css) <- unzip <$> mapM solveDot cs
                                             return (concat eqs, concat css)
  where solveDot c@(Mut _ (TVar _ v) n _)
          | Just w <- lookup (v,n) mutC = solveMutAttr env w c >> return ([], [])
        solveDot c@(Sel _ _ (TVar _ v) n _)
          | Just w <- lookup (v,n) selC = solveSelAttr env w c
          | Just w <- lookup (v,n) selP = solveSelWit env w c
        solveDot c                      = return ([], [c])

ctxtReduce env vi multiPBnds            = (concat eqs, concat css)
  where (eqs,css)                       = unzip $ map ctxtRed multiPBnds
        ctxtRed (v,wps)                 = imp v [] [] [] wps
        imp v eq uni wps ((w,p):wps')
          | (w',wf,p1,p'):_ <- hits     = --trace ("  *" ++ prstr p ++ " covered by " ++ prstr p1) $
                                          imp v (Eqn w (impl2type (tVar v) p) (wf (eVar w')) : eq) ((tcargs p `zip` tcargs p') ++ uni) wps wps'
          | otherwise                   = --trace ("   (Not covered: " ++ prstr p ++ " in context " ++ prstrs (map snd (wps++wps')) ++ ")") $
                                          imp v eq uni ((w,p):wps) wps'
          where hits                    = [ (w',wf,p0,vsubst s p') | (w',p0) <- wps++wps', w'/=w, Just (wf,p') <- [findAncestor env p0 (tcname p)] ]
                s                       = [(tvSelf,tVar v)]
        imp v eq uni wps []             = (eq, uni)
  -- TODO: also check that an mro exists (?)


remove ws []                            = []
remove ws (Impl _ w t p : cs)
  | w `elem` ws                         = remove ws cs
remove ws (c : cs)                      = c : remove ws cs


----------------------------------------------------------------------------------------------------------------------
-- Misc.
----------------------------------------------------------------------------------------------------------------------

px0:px1:px2:_                           = xNames

app tx e []                             = e
app (TFun _ fx p k _) e es              = Lambda NoLoc p' k' (Call NoLoc e (exp2arg es (pArg p')) (kArg k')) fx
  where (p',k')                         = (pPar pNames p, kPar attrKW k)
app tx e es                             = Call NoLoc e (exp2arg es PosNil) KwdNil

app2nd (Just Static) tx e es            = app tx e es
app2nd (Just Property) tx e es          = app tx e es
app2nd Nothing tx e es                  = app tx e es
app2nd _ tx e []                        = e
app2nd _ tx e es                        = Lambda NoLoc p' k' (Call NoLoc e (PosArg pSelf (exp2arg es pArgs)) (kArg k')) fx
  where TFun _ fx p k _                 = tx                    -- If it already takes a first argument, it must be a function!
        (p',k')                         = (pPar pNames p, kPar attrKW k)
        PosArg pSelf pArgs              = pArg p'

idwit env w t1 t2                       = Eqn w (wFun t1 t2) (eLambda [(px0,t1)] (eVar px0))

rowFun PRow r1 r2                       = tFun fxPure (posRow (tTupleP r1) posNil) kwdNil (tTupleP r2)
rowFun KRow r1 r2                       = tFun fxPure (posRow (tTupleK r1) posNil) kwdNil (tTupleK r2)

rowWit PRow n t r wt wr                 = eLambda [(px0,tTupleP $ posRow t r)] eTup
  where eTup                            = Paren l0 $ Tuple l0 (PosArg e1 (PosStar e2)) KwdNil
        e1                              = eCall (eVar wt) [DotI l0 (eVar px0) 0]
        e2                              = eCall (eVar wr) [RestI l0 (eVar px0) 0]
rowWit KRow n t r wt wr                 = eLambda [(px0,tTupleK $ kwdRow n t r)] eTup
  where eTup                            = Paren l0 $ Tuple l0 PosNil (KwdArg n e1 (KwdStar e2))
        e1                              = eCall (eVar wt) [Dot l0 (eVar px0) n]
        e2                              = eCall (eVar wr) [Rest l0 (eVar px0) n]

wFun t1 t2                              = tFun fxPure (posRow t1 posNil) kwdNil t2
