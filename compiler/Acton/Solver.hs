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
simplify env te tt cs                       = do cs <- msubst cs
                                                 te <- msubst te
                                                 --traceM ("  -simplify:\n" ++ render (nest 8 $ vcat $ map pretty cs))
                                                 --traceM ("  -for:\n" ++ render (nest 8 $ vcat $ map pretty te))
                                                 simplifyGroups env te tt (groupCs env cs)

simplifyGroups env te tt []                 = return ([], [])
simplifyGroups env te tt (cs:css)           = do --traceM ("\n\n######### simplifyGroup " ++ prstrs cs)
                                                 (cs1,eq1) <- simplify' env te tt [] cs `catchError` \err -> Control.Exception.throw err
                                                 env <- msubst env
                                                 (cs2,eq2) <- simplifyGroups env te tt css
                                                 return (cs1++cs2, eq1++eq2)

simplify'                                   :: (Polarity a, Pretty a) => Env -> TEnv -> a -> Equations -> Constraints -> TypeM (Constraints,Equations)
simplify' env te tt eq []                   = return ([], eq)
simplify' env te tt eq cs                   = do eq1 <- reduce env eq cs
                                                 cs1 <- msubst =<< collectDeferred
                                                 --traceM ("## Improving:\n" ++ render (nest 8 $ vcat $ map pretty cs1))
                                                 env1 <- msubst env 
                                                 te1 <- msubst te
                                                 tt1 <- msubst tt
                                                 improve env1 te1 tt1 eq1 cs1

tryred env eq []                            = return ([], eq)
tryred env eq cs                            = do eq1 <- reduce env eq cs
                                                 cs1 <- msubst =<< collectDeferred
                                                 return (cs1, eq1)

groupCs env []                              = []
groupCs env (c:cs)                          = close (tyfree c ++ attrfree [c]) [c] cs
  where close tvs cs0 cs
          | null cs1                        = cs0 : groupCs env cs2
          | otherwise                       = close (tvs++tyfree cs1++attrfree cs1) (cs0++cs1) cs2
          where (cs1,cs2)                   = partition (not . null . intersect tvs . tyfree) cs
        attrs cs                            = [ n | Sel _ t n _ <- cs ] ++ [ n | Mut t n _ <- cs ]
        attrfree cs                         = [ tv | n <- attrs cs, tv <- allConAttrFree env n ]


----------------------------------------------------------------------------------------------------------------------
-- solve
----------------------------------------------------------------------------------------------------------------------

data Rank                                   = RRed { cstr :: Constraint }
                                            | RUni { tgt :: Type, alts :: [Type] }
                                            | RSealed { tgt :: Type }
                                            | RTry { tgt :: Type, alts :: [Type], rev :: Bool }
                                            | RVar { tgt :: Type, alts :: [Type] }
                                            | ROvl { tgt :: Type }
                                            | RSkip
                                            deriving (Show)

instance Eq Rank where
    RRed _      == RRed _                   = True
    RUni t1 _   == RUni t2 _                = t1 == t2
    RSealed t1  == RSealed t2               = t1 == t2
    RTry t1 _ _ == RTry t2 _ _              = t1 == t2
    RVar t1 _   == RVar t2 _                = t1 == t2
    ROvl t1     == ROvl t2                  = True
    RSkip       == RSkip                    = True
    _           == _                        = False

instance Pretty Rank where
    pretty (RRed c)                         = text "<reduce>" <+> pretty c
    pretty (RUni t ts)                      = pretty t <+> char '=' <+> commaSep pretty ts
    pretty (RSealed t)                      = pretty t <+> text "sealed"
    pretty (RTry t ts rev)                  = pretty t <+> braces (commaSep pretty ts) Pretty.<> (if rev then char '\'' else empty)
    pretty (RVar t ts)                      = pretty t <+> char '~' <+> commaSep pretty ts
    pretty (ROvl t)                         = pretty t <+> text "..."
    pretty RSkip                            = text "<skip>"

solve                                       :: (Polarity a, Pretty a) => Env -> (Constraint -> Bool) ->
                                               TEnv -> a -> Equations -> Constraints -> TypeM (Constraints,Equations)
solve env select te tt eq cs                = do (cs',eq') <- solveGroups env select te tt (groupCs env cs)
                                                 eq <- msubst eq
                                                 return (cs', eq'++eq)

solveGroups env select te tt []             = return ([], [])
solveGroups env select te tt (cs:css)       = do traceM ("\n\n######### solveGroup\n" ++ render (nest 4 $ vcat $ map pretty cs))
                                                 (cs1,eq1) <- solve' env select [] te tt [] cs `catchError` \err -> Control.Exception.throw err
                                                 env <- msubst env
                                                 (cs2,eq2) <- solveGroups env select te tt css
                                                 return (cs1++cs2, eq1++eq2)

solve' env select hist te tt eq cs
  | null solve_cs || null goals             = return (keep_cs, eq)
  | otherwise                               = do st <- currentState
                                                 traceM ("## keep:\n" ++ render (nest 8 $ vcat $ map pretty keep_cs))
                                                 traceM ("## solve:\n" ++ render (nest 8 $ vcat $ map pretty solve_cs))
                                                 traceM ("## ranks:\n" ++ render (nest 8 $ vcat $ map pretty rnks))
                                                 traceM ("## keep2:\n" ++ render (nest 8 $ vcat $ map pretty keep2))
                                                 traceM ("## solve2:\n" ++ render (nest 8 $ vcat $ map pretty solve2))
                                                 --traceM ("## optvs: " ++ prstrs optvs)
                                                 --traceM ("## posvs: " ++ prstrs posvs)
                                                 --traceM ("## negvs: " ++ prstrs negvs)
                                                 case head goals of
                                                    RRed c -> do
                                                        traceM ("### reduce " ++ prstr c)
                                                        proceed hist cs
                                                    RUni t alts -> do
                                                        traceM ("### uni goal " ++ prstr t ++ ", unifying with " ++ prstrs alts)
                                                        unifyM alts (repeat t) >> proceed hist cs
                                                    RSealed t ->
                                                        tryAlts st t [fxAction, fxPure]
                                                    RTry t alts r -> do
                                                        traceM ("### try goal " ++ prstr t ++ ", candidates: " ++ prstrs alts ++ if r then " (rev)" else "")
                                                        tryAlts st t alts
                                                    RVar t alts -> do
                                                        traceM ("### var goal " ++ prstr t ++ ", unifying with " ++ prstrs alts)
                                                        unifyM alts (repeat t) >> proceed hist cs
                                                    ROvl t -> do
                                                        traceM ("### ovl goal " ++ prstr t ++ ", defaulting remaining constraints")
                                                        (cs,eq) <- tryred (useForce env) eq cs
                                                        te <- msubst te
                                                        tt <- msubst tt
                                                        hist <- msubst hist
                                                        solve' env select hist te tt eq cs
                                                    RSkip ->
                                                        return (keep_cs, eq)

  where (solve1, keep1)                     = partition select cs
        heads                               = [ v | Just v <- map headvar solve1 ]
        (solve2, keep2)                     = partition (maybe False (`elem` heads) . headvar) keep1
        (solve_cs, keep_cs)                 = (solve1++solve2, keep2)
        goals                               = sortOn deco $ map condense $ group rnks
        group []                            = []
        group (r:rs)                        = (r : rs1) : group rs2
          where (rs1,rs2)                   = partition (==r) rs
        rnks                                = map (rank env) solve_cs
        tryAlts st t0 []                    = trace ("### FAIL " ++ prstr t0) $
                                              noSolve cs
        tryAlts st t0 (t:ts)                = tryAlt t0 t `catchError` const ({-traceM ("### ROLLBACK " ++ prstr t0) >> -}rollbackState st >> tryAlts st t0 ts)
        tryAlt t0 (TCon _ c)
          | isProto env (tcname c)          = do p <- instwildcon env c
                                                 w <- newWitness
                                                 traceM ("  # trying " ++ prstr t0 ++ " (" ++ prstr p ++ ")")
                                                 proceed hist (Impl w t0 p : cs)
        tryAlt t0 t                         = do t <- instwild env (kindOf env t0) t
                                                 traceM ("  # trying " ++ prstr t0 ++ " = " ++ prstr t)
                                                 unify t0 t
                                                 proceed (t:hist) cs
        proceed hist cs                     = do (cs,eq) <- tryred env eq cs
                                                 te <- msubst te
                                                 tt <- msubst tt
                                                 hist <- msubst hist
                                                 solve' env select hist te tt eq cs

        condense (RRed c : rs)              = RRed c
        condense (RUni t as : rs)           = RUni t (foldr union as $ map alts rs)
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

        deco (RRed cs)                      = (0, 0, 0, 0)
        deco (RUni t as)                    = (1, 0, length as, 0)
        deco (RSealed t)                    = (2, 0, 0, 0)
        deco (RTry (TVar _ v) as r)         = (3, length $ filter (==v) embvs, length as, length $ filter (==v) univs)
        deco (RTry t as r)                  = (4, 0, length as, 0)
        deco (RVar t as)                    = (5, 0, length as, 0)
        deco (ROvl t)                       = (6, 0, 0, 0)
        deco (RSkip)                        = (7, 0, 0, 0)

        subrev []                           = []
        subrev (t:ts)                       = subrev ts1 ++ t : subrev ts2
          where (ts1,ts2)                   = partition (\t' -> castable env t' t) ts

-- subrev [int,Pt,float,CPt,C3Pt]           = [] ++ int : subrev [Pt,float,CPt,C3Pt] 
--                                          = int : subrev [CPt,C3Pt] ++ Pt : subrev [float]
--                                          = int : [C3Pt] ++ CPt ++ subrev [] ++ Pt : [] ++ float : subrev []
--                                          = int : C3Pt : CPt : Pt : float

rank                                        :: Env -> Constraint -> Rank
rank env (Sub _ t1 t2)                      = rank env (Cast t1 t2)

rank env (Cast t1@TVar{} t2@TVar{})
  | univar (tvar t1), univar (tvar t2)      = RUni t1 [t2]
rank env (Cast t1@TVar{} (TOpt _ t2@TVar{}))
  | univar (tvar t1), univar (tvar t2)      = RVar t1 [t2]
rank env (Cast t1@TVar{} (TOpt _ t2))
  | univar (tvar t1)                        = RTry t1 (allBelow env t2 ++ [tOpt tWild, tNone]) False
rank env (Cast TNone{} t2@TVar{})
  | univar (tvar t2)                        = RTry t2 [tOpt tWild, tNone] True
rank env (Cast t1@TVar{} t2)
  | univar (tvar t1)                        = RTry t1 (allBelow env t2) False
rank env (Cast t1 t2@TVar{})
  | univar (tvar t2)                        = RTry t2 (allAbove env t1) True

rank env c@(Impl _ t p)
  | schematic t `elem` ts                   = ROvl t
  | otherwise                               = RTry t ts False
  where ts                                  = allExtProto env t p

rank env (Sel _ t n _)                      = RTry t (allConAttr env n ++ allProtoAttr env n ++ allExtProtoAttr env n) False
rank env (Mut t n _)                        = RTry t (allConAttr env n) False

rank env (Seal t@TVar{})
  | tvkind (tvar t) == KFX                  = RSealed t
  | otherwise                               = RSkip

rank env c                                  = RRed c


----------------------------------------------------------------------------------------------------------------------
-- New variable info
----------------------------------------------------------------------------------------------------------------------

data VarInfo                                = VarInfo {
                                                ubnd        :: [Type],
                                                lbnd        :: [Type],
                                                uvar        :: [TVar],
                                                lvar        :: [TVar],
                                                pbnd        :: [PCon],
                                                sels        :: [Name],
                                                muts        :: [Name] }

varinf                                      :: Constraints -> Map TVar VarInfo
varinf                                      = foldl f Map.empty
  where
    f m (Cast (TVar _ v1) (TVar _ v2))
      | v1 == v2                            = m
      | otherwise                           = update m v1 (\vi -> vi { uvar = v2 : uvar vi })
    f m (Cast (TVar _ v) t)                 = update m v (\vi -> vi { ubnd = t : ubnd vi })
    f m (Cast t (TVar _ v))                 = update m v (\vi -> vi { lbnd = t : lbnd vi })
    f m (Sub _ t1 t2)                       = f m (Cast t1 t2)
    f m (Impl w (TVar _ v) p)               = update m v (\vi -> vi { pbnd = p : pbnd vi })
    f m (Impl w t p)
      | not $ null vs                       = undefined
      where vs                              = tyfree t
    f m (Sel _ (TVar _ v) n t)              = update m v (\vi -> vi { sels = n : sels vi })
    f m (Mut (TVar _ v) n t)                = update m v (\vi -> vi { muts = n : muts vi })
    f m (Seal (TVar _ v))                   = undefined
    f m _                                   = undefined

    update                                  :: Map TVar VarInfo -> TVar -> (VarInfo -> VarInfo) -> Map TVar VarInfo
    update m v f                            = Map.alter upd v m
      where upd (Just i)                    = Just (f i)
            upd Nothing                     = Just (f $ VarInfo [] [] [] [] [] [] [])

-------------------------------------------------------------------------------------------------------------------------

class OptVars a where
    optvars                             :: a -> [TVar]

instance (OptVars a) => OptVars [a] where
    optvars                             = concat . map optvars

instance OptVars Constraint where
    optvars (Cast t1 t2)                = optvars [t1, t2]
    optvars (Sub w t1 t2)               = optvars [t1, t2]
    optvars (Impl w t p)                = optvars t ++ optvars p
    optvars (Sel w t1 n t2)             = optvars [t1, t2]
    optvars (Mut t1 n t2)               = optvars [t1, t2]
    optvars (Seal t)                    = optvars t

instance OptVars Type where
    optvars (TOpt _ (TVar _ v))         = [v]
    optvars (TOpt _ t)                  = optvars t
    optvars (TCon _ c)                  = optvars c
    optvars (TFun _ fx p k t)           = optvars [p, k, t]
    optvars (TTuple _ p k)              = optvars [p, k]
    optvars (TRow _ _ _ t r)            = optvars [t, r]
    optvars _                           = []

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
        emb (Impl _ (TCon _ c) p)
          | otherwise                   = tyfree c ++ tyfree p
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
  where n                               = tcname tc
        tcons                           = allAncestors env tc ++ [schematic' tc]
allAbove env (TVar _ tv)
  | not $ univar tv                     = [tOpt tWild, tCon tc, tVar tv]
  where tc                              = findTVBound env tv
allAbove env (TOpt _ t)                 = [tOpt tWild]
allAbove env (TNone _)                  = [tOpt tWild, tNone]
allAbove env (TFun _ _ _ _ _)           = [tOpt tWild, tFun tWild tWild tWild tWild]
allAbove env (TTuple _ _ _)             = [tOpt tWild, tTuple tWild tWild]
allAbove env (TRow _ k n _ _)           = [tRow k n tWild tWild]
allAbove env (TNil _ k)                 = [tNil k]
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
allBelow env (TRow _ k n _ _)           = [tRow k n tWild tWild]
allBelow env (TNil _ k)                 = [tNil k]
allBelow env (TFX _ FXProc)             = [fxProc, fxMut, fxPure, fxAction]
allBelow env (TFX _ FXMut)              = [fxMut, fxPure]
allBelow env (TFX _ FXPure)             = [fxPure]
allBelow env (TFX _ FXAction)           = [fxAction]

----------------------------------------------------------------------------------------------------------------------
-- reduce
----------------------------------------------------------------------------------------------------------------------

instance Subst Equation where
    msubst (Eqn w t e)                      = do t <- msubst t
                                                 e <- msubst e
                                                 return (Eqn w t e)
    
    tyfree (Eqn w t e)                      = tyfree t ++ tyfree e

instance Vars Equation where
    free (Eqn w t e)                     = free e

    bound (Eqn w t e)                    = [w]



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
  | Just wit <- witSearch                   = do (eq',cs) <- solveImpl env wit w t p
                                                 reduce env (eq'++eq) cs
  where witSearch                           = findWitness env t p
  
reduce' env eq c@(Impl w t@(TCon _ tc) p)
  | Just wit <- witSearch                   = do (eq',cs) <- solveImpl env wit w t p
                                                 reduce env (eq'++eq) cs
  | not $ null $ filter univar $ tyfree t   = do defer [c]; return eq
  where witSearch                           = findWitness env t p

reduce' env eq c@(Impl w t@(TFX _ tc) p)
  | Just wit <- witSearch                   = do (eq',cs) <- solveImpl env wit w t p
                                                 reduce env (eq'++eq) cs
  | not $ null $ filter univar $ tyfree t   = do defer [c]; return eq
  where witSearch                           = findWitness env t p

reduce' env eq c@(Impl w t@(TOpt _ t') p)
  | tcname p == qnIdentity                  = do let e = eCall (tApp (eQVar primIdentityOpt) [t']) []
                                                 return (Eqn w (impl2type t p) e : eq)
  | tcname p == qnEq                        = do w' <- newWitness
                                                 let e = eCall (tApp (eQVar primEqOpt) [t']) [eVar w']
                                                 reduce env (Eqn w (impl2type t p) e : eq) [Impl w' t' p]

reduce' env eq c@(Impl w t@(TNone _) p)
  | tcname p == qnIdentity                  = return (Eqn w (impl2type t p) (eQVar primWIdentityNone) : eq)
  | tcname p == qnEq                        = return (Eqn w (impl2type t p) (eQVar primWEqNone) : eq)

reduce' env eq c@(Sel w (TVar _ tv) n _)
  | univar tv                               = do defer [c]; return eq
  | Just wsc <- attrSearch                  = do (eq',cs) <- solveSelAttr env wsc c
                                                 reduce env (eq'++eq) cs
  | Just p <- protoSearch                   = do (eq',cs) <- solveSelProto env p c
                                                 reduce env (eq'++eq) cs
  | otherwise                               = tyerr n "Attribute not found"
  where attrSearch                          = findTVAttr env tv n
        protoSearch                         = findProtoByAttr env (NoQ $ tvname tv) n

reduce' env eq c@(Sel w (TCon _ tc) n _)
  | Just wsc <- attrSearch                  = do (eq',cs) <- solveSelAttr env wsc c
                                                 reduce env (eq'++eq) cs
  | Just p <- protoSearch                   = do (eq',cs) <- solveSelProto env p c
                                                 reduce env (eq'++eq) cs
  | otherwise                               = tyerr n "Attribute not found"
  where attrSearch                          = findAttr env tc n
        protoSearch                         = findProtoByAttr env (tcname tc) n

reduce' env eq (Sel w t1@(TTuple _ p r) n t2)
                                            = do let e = eLambda [(px0,t1)] (eDot (eVar px0) n)
                                                 unify r (kwdRow n t2 tWild)
                                                 return (Eqn w (wFun t1 t2) e : eq)

reduce' env eq c@(Mut (TVar _ tv) n _)
  | univar tv                               = do defer [c]; return eq
  | Just wsc <- attrSearch                  = do solveMutAttr env wsc c
                                                 return eq
  | otherwise                               = tyerr n "Attribute not found:"
  where attrSearch                          = findTVAttr env tv n

reduce' env eq c@(Mut (TCon _ tc) n _)
  | Just wsc <- attrSearch                  = do solveMutAttr env wsc c
                                                 return eq
  | otherwise                               = tyerr n "Attribute not found:"
  where attrSearch                          = findAttr env tc n

reduce' env eq (Seal t@(TVar _ tv))
  | univar tv                               = do defer [Seal t]; return eq
  | otherwise                               = return eq
reduce' env eq (Seal t@(TCon _ tc))
--  | castable env t tObject                  = tyerr t "Leaking actor seal:"                       -- when we start prohibit sharing of mutable data
  | otherwise                               = reduce env eq (map Seal $ tcargs tc)
reduce' env eq (Seal t@(TFX _ fx))
  | fx `elem` [FXMut,FXProc]                = tyerr t "Leaking actor seal:"
  | otherwise                               = return eq
reduce' env eq (Seal t)                     = reduce env eq (map Seal ts)
  where ts                                  = leaves t

reduce' env eq c                            = noRed c


solveImpl env wit w t p                     = do (cs,p',we) <- instWitness env t wit
                                                 unifyM (tcargs p) (tcargs p')
                                                 return ([Eqn w (impl2type t p) we], cs)

solveSelAttr env (wf,sc,d) (Sel w t1 n t2)  = do (cs,tvs,t) <- instantiate env sc
                                                 when (tvSelf `elem` snd (polvars t)) (tyerr n "Contravariant Self attribute not selectable by instance")
                                                 let e = eLambda [(px0,t1)] (app t (tApp (eDot (wf $ eVar px0) n) tvs) $ witsOf cs)
                                                 unify (subst [(tvSelf,t1)] t) t2
                                                 return ([Eqn w (wFun t1 t2) e], cs)

--  e1.__setslice__(sl, e2)
--  e1.__setslice__(w_Iterable, sl, e2)
--  w_Sliceable.__setslice__(e1, w_Iterable, sl, e2)

--  w(e1)(sl,e2)                                                        w = lambda x0: lambda p1,p2: w_Sliceable.__setslice__(x0, w1, p1, p2)
--  (lambda p1,p2: w_Sliceable.__setslice__(x0, w1, p1, p2))(sl,e2)
--  w_Sliceable.__setslice__(x0, w1, sl, e2)                            w1 = w_Iterable
--  w_Sliceable.__setslice__(e1, w_Iterable, sl, e2)

solveSelProto env pn c@(Sel w t1 n t2)      = do p <- instwildcon env pn
                                                 w' <- newWitness
                                                 (eq,cs) <- solveSelWit env (p, eVar w') c
                                                 return (eq, Impl w' t1 p : cs)

solveSelWit env (p,we) (Sel w t1 n t2)      = do let Just (wf,sc,d) = findAttr env p n
                                                 (cs,tvs,t) <- instantiate env sc
                                                 when (tvSelf `elem` snd (polvars t)) (tyerr n "Contravariant Self attribute not selectable by instance")
                                                 let e = eLambda [(px0,t1)] (app t (tApp (eDot (wf we) n) tvs) $ eVar px0 : witsOf cs)
                                                 unify (subst [(tvSelf,t1)] t) t2
                                                 return ([Eqn w (wFun t1 t2) e], cs)

solveMutAttr env (wf,sc,dec) (Mut t1 n t2)  = do when (dec /= Just Property) (noMut n)
                                                 let TSchema _ [] t = sc
                                                 unify t2 (subst [(tvSelf,t1)] t)

----------------------------------------------------------------------------------------------------------------------
-- witness lookup
----------------------------------------------------------------------------------------------------------------------

findWitness                 :: Env -> Type -> PCon -> Maybe Witness
findWitness env t p         = case elim [] match_ws of
                                [w] | null uni_ws  -> Just w
                                w:_ | isForced env -> Just w
                                _ -> Nothing
  where (match_ws, ws1)     = partition (matching t) (witsByPName env $ tcname p)
        uni_ws              = filter (unifying t) ws1
        elim ws' []         = reverse ws'
        elim ws' (w:ws)
          | covered         = elim ws' ws
          | otherwise       = elim (w:ws') ws
          where covered     = or [ matching (wtype w') w && not (matching (wtype w) w') | w' <- ws'++ws ]

findProtoByAttr env cn n    = case filter hasAttr $ witsByTName env cn of
                                [] -> Nothing
                                w:_ -> Just $ schematic' $ proto w
  where hasAttr w           = n `elem` conAttrs env (tcname $ proto w)

hasWitness                  :: Env -> Type -> PCon -> Bool
hasWitness env t p          =  isJust $ findWitness env t p

allExtProto                 :: Env -> Type -> PCon -> [Type]
allExtProto env t p         = reverse [ schematic (wtype w) | w <- witsByPName env (tcname p), matching t0 w {- && schematic (wtype w) /= t0 -} ]
  where t0                  = wild t                    -- matching against wild t also accepts witnesses that would instantiate t

allExtProtoAttr             :: Env -> Name -> [Type]
allExtProtoAttr env n       = [ tCon tc | tc <- allCons env, any ((n `elem`) . allAttrs' env . proto) (witsByTName env $ tcname tc) ]


matching t w                = matching' t (qbound $ binds w) (wtype w)

matching' t vs t'           = isJust $ match vs t t'    -- there is a substitution s with domain vs such that t == subst s t'

unifying t w                = runTypeM $ tryUnify `catchError` const (return False)
  where tryUnify            = do unify t (wtype w)
                                 return True


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
  | Just (wf,c') <- search                  = unifyM (tcargs c') (tcargs c2)        -- TODO: cast/unify based on polarities
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
        castFX FXPure   FXProc              = True
        castFX FXMut    FXMut               = True
        castFX FXMut    FXProc              = True
        castFX FXProc   FXProc              = True
        castFX FXAction FXAction            = True
        castFX FXAction FXProc              = True
        castFX _        _                   = False

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
-- sub
----------------------------------------------------------------------------------------------------------------------

sub                                         :: Env -> Equations -> Name -> Type -> Type ->TypeM Equations
sub env eq w t1 t2                          = do t1' <- msubst t1
                                                 t2' <- msubst t2
                                                 sub' env eq w t1' t2'

sub'                                        :: Env -> Equations -> Name -> Type -> Type ->TypeM Equations

sub' env eq w t1@TWild{} t2                 = return (idwit env w t1 t2 : eq)
sub' env eq w t1 t2@TWild{}                 = return (idwit env w t1 t2 : eq)

--                as declared               as called
--                existing                  expected
sub' env eq w t1@(TFun _ fx1 p1 k1 t1') t2@(TFun _ fx2 p2 k2 t2')                   -- TODO: implement pos/kwd argument shifting
                                            = do wp <- newWitness
                                                 wk <- newWitness
                                                 wt <- newWitness
                                                 let e = eLambda [(px0,t1)] e'
                                                     e' = Lambda l0 (PosSTAR px1 $ Just $ tTupleP p2) (KwdSTAR px2 $ Just $ tTupleK k2) e0 fx1
                                                     e0 = eCall (eVar wt) [Call l0 (eVar px0) (PosStar e1) (KwdStar e2)]
                                                     e1 = eCall (eVar wp) [eVar px1]
                                                     e2 = eCall (eVar wk) [eVar px2]
                                                     cs = [Cast fx1 fx2, Sub wp p2 p1, Sub wk k2 k1, Sub wt t1' t2']

                                                 reduce env (Eqn w (wFun t1 t2) e : eq) cs

--                existing            expected
sub' env eq w t1@(TTuple _ p1 k1) t2@(TTuple _ p2 k2)                               -- TODO: implement pos/kwd argument shifting
                                            = do wp <- newWitness
                                                 wk <- newWitness
                                                 let e = eLambda [(px0,t1)] (Paren l0 $ Tuple l0 (PosStar e1) (KwdStar e2))
                                                     e1 = eCall (eVar wp) [Paren l0 $ Tuple l0 (PosStar $ eVar px0) KwdNil]
                                                     e2 = eCall (eVar wk) [Paren l0 $ Tuple l0 PosNil (KwdStar $ eVar px0)]
                                                     cs = [Sub wp p1 p2, Sub wk k1 k2]
                                                 reduce env (Eqn w (wFun t1 t2) e : eq) cs

-- Note: a sub-row constraint R1 < R2 is witnessed by a lambda of type
-- (*(R1))->(*(R2)) or (**(R1))->(**(R2)), depending on the row kind

sub' env eq w r1@(TNil _ k1) r2@(TNil _ k2)
  | k1 == k2                                = return (idwit env w tUnit tUnit : eq)

--          existing     expected                Match labels in the order of the expected row
sub' env eq w r1     r2@(TRow _ k n t2 r2') = do (t1,r1') <- findElem k (tNil k) n r1 (rowTail r2)
                                                 wt <- newWitness
                                                 wr <- newWitness
                                                 let e = rowWit k w n t1 r1' wt wr
                                                     cs = [Sub wt t1 t2, Sub wr r1' r2']
                                                 reduce env (Eqn w (rowFun k r1 r2) e : eq) cs
sub' env eq w r1@(TRow _ k n t1 r1') r2     = do (t2,r2') <- findElem k (tNil k) n r2 (rowTail r1)
                                                 wt <- newWitness
                                                 wr <- newWitness
                                                 let e = rowWit k w n t2 r2' wt wr
                                                     cs = [Sub wt t1 t2, Sub wr r1' r2']
                                                 reduce env (Eqn w (rowFun k r1 r2) e : eq) cs

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
  | tv1 == tv2                              = return (idwit env w t1 t2 : eq)
  | univar tv1 && univar tv2                = do defer [Sub w t1 t2]; return eq

sub' env eq w t1 t2                         = do cast env t1 t2
                                                 return (idwit env w t1 t2 : eq)


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
                                                sealed      :: [TVar],
                                                ubounds     :: Map TVar [Type], 
                                                lbounds     :: Map TVar [Type], 
                                                pbounds     :: Map TVar [(Name,PCon)],
                                                mutattrs    :: Map TVar [Name],
                                                selattrs    :: Map TVar [Name] }

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
    f (Cast (TVar _ v1) (TVar _ v2) : cs)
      | v1 == v2                            = f cs
      | otherwise                           = f cs . varvar v1 v2
    f (Cast (TVar _ v) t : cs)              = f cs . ubound v t . embed (tyfree t)
    f (Cast t (TVar _ v) : cs)              = f cs . lbound v t . embed (tyfree t)
    f (Sub _ (TVar _ v1) (TVar _ v2) : cs)  = f cs . varvar v1 v2
    f (Sub _ (TVar _ v) t : cs)             = f cs . ubound v t . embed (tyfree t)
    f (Sub _ t (TVar _ v) : cs)             = f cs . lbound v t . embed (tyfree t)
    f (Impl w (TVar _ v) p : cs)            = f cs . pbound v w p . embed (tyfree p)
    f (Impl w t p : cs)
      | not $ null vs                       = f cs . embed (vs ++ tyfree p)
      where vs                              = tyfree t
    f (Mut (TVar _ v) n t : cs)             = f cs . mutattr v n . embed (tyfree t)
    f (Sel _ (TVar _ v) n t : cs)           = f cs . selattr v n . embed (tyfree t)
    f (Seal (TVar _ v) : cs)                = f cs . seal v
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


mkGLB env (v,ts)
  | Just t <- glbfold env ts            = do t <- instwild env KType t
                                             --traceM ("   glb " ++ prstrs ts ++ " = " ++ prstr t)
                                             return (v, t)
  | otherwise                           = tyerrs ts ("No common subtype:")


mkLUB env (v,ts)
  | Just t <- lubfold env ts            = do t <- instwild env KType t
                                             --traceM ("   lub " ++ prstrs ts ++ " = " ++ prstr t)
                                             return (v, t)
  | otherwise                           = tyerrs ts ("No common supertype:")


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
                                             sequence [ unify (tVar v) (tVar v') | v' <- vs ]
                                             simplify' env te tt eq cs
  | not $ null gsimple                  = do --traceM ("  *G-simplify " ++ prstrs [ (v,tVar v') | (v,v') <- gsimple ])
                                             --traceM ("  *obsvars: " ++ prstrs obsvars)
                                             --traceM ("  *varvars: " ++ prstrs (varvars vi))
                                             sequence [ unify (tVar v) (tVar v') | (v,v') <- gsimple ]
                                             simplify' env te tt eq cs
  | not $ null cyclic                   = tyerrs cyclic ("Cyclic subtyping:")
  | not $ null (multiUBnd++multiLBnd)   = do ub <- mapM (mkGLB env) multiUBnd   -- GLB of the upper bounds
                                             lb <- mapM (mkLUB env) multiLBnd   -- LUB of the lower bounds
                                             --traceM ("  *GLB " ++ prstrs ub)
                                             --traceM ("  *LUB " ++ prstrs lb)
                                             let cs' = [ Cast (tVar v) t | (v,t) <- ub ] ++ [ Cast t (tVar v) | (v,t) <- lb ]
                                             simplify' env te tt eq (cs' ++ map (replace ub lb) cs)
  | not $ null posLBnd                  = do --traceM ("  *S-simplify (dn) " ++ prstrs posLBnd)
                                             sequence [ unify (tVar v) t | (v,t) <- posLBnd ]
                                             simplify' env te tt eq cs
  | not $ null negUBnd                  = do --traceM ("  *S-simplify (up) " ++ prstrs negUBnd)
                                             sequence [ unify (tVar v) t | (v,t) <- negUBnd ]
                                             simplify' env te tt eq cs
  | not $ null closUBnd                 = do --traceM ("  *Simplify upper closed bound " ++ prstrs closUBnd)
                                             sequence [ unify (tVar v) t | (v,t) <- closUBnd ]
                                             simplify' env te tt eq cs
  | not $ null closLBnd                 = do --traceM ("  *Simplify lower closed bound " ++ prstrs closLBnd)
                                             sequence [ unify (tVar v) t | (v,t) <- closLBnd ]
                                             simplify' env te tt eq cs
  | not $ null redEq                    = do --traceM ("  *(Context red) " ++ prstrs [ w | Eqn w _ _ <- redEq ])
                                             sequence [ unify t1 t2 | (t1,t2) <- redUni ]
                                             simplify' env te tt (redEq++eq) (remove [ w | Eqn w _ _ <- redEq ] cs)
  | not $ null dots                     = do --traceM ("  *Implied mutation/selection solutions " ++ prstrs dots)
                                             (eq',cs') <- solveDots env mutC selC selP cs
                                             simplify' env te tt (eq'++eq) cs'
  | not $ null redSeal                  = do --traceM ("  *removing redundant Seal constraints on: " ++ prstrs redSeal)
                                             return (cs \\ map (Seal . tVar) redSeal, eq)
  | otherwise                           = do --traceM ("  *improvement done " ++ show (length cs))
                                             return (cs, eq)
  where info                            = varinfo cs
        Just vi                         = info
        closure                         = varclose (varvars vi)
        Right vclosed                   = closure
        (vvsL,vvsU)                     = unzip vclosed
        gsimple                         = gsimp vi vclosed obsvars (varvars vi)
        multiUBnd                       = [ (v,us) | (v,ts) <- Map.assocs (ubounds vi), v `notElem` embedded vi, let us = noOpt ts, length us > 1 ]
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
        obsvars                         = posvars0 ++ negvars0 ++ fixedvars ++ pvars ++ embedded vi ++ sealed vi
        boundvars                       = Map.keys (ubounds vi) ++ Map.keys (lbounds vi)
        boundprot                       = tyfree (Map.elems $ ubounds vi) ++ tyfree (Map.elems $ lbounds vi)
        cyclic                          = if null (boundvars\\boundprot) then [ c | c <- cs, fromJust (headvar c) `elem` boundvars ] else []
        redSeal                         = sealed vi \\ (posvars ++ negvars ++ embedded vi ++ Map.keys (ubounds vi) ++ Map.keys (lbounds vi)
                                          ++ Map.keys (pbounds vi) ++ Map.keys (mutattrs vi) ++ Map.keys (selattrs vi))

noOpt ts                                = filter chk ts
  where chk TOpt{}                      = False
        chk _                           = True

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
implAll env ps (TCon _ c)               = and [ hasWitness env (tCon c) p | (w,p) <- ps ]
implAll env ps (TOpt _ _)               = all ((`elem` [qnIdentity,qnEq]) . tcname . snd) ps
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
          | Just w <- lookup (v,n) mutC = solveMutAttr env w c >> return ([], [])
        solveDot c@(Sel _ (TVar _ v) n _)
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
          where hits                    = [ (w',wf,p0,subst s p') | (w',p0) <- wps++wps', w'/=w, Just (wf,p') <- [findAncestor env p0 (tcname p)] ]
                s                       = [(tvSelf,tVar v)]
        imp v eq uni wps []             = (eq, uni)
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

idwit env w t1 t2                       = Eqn w (wFun t1 t2) (eLambda [(px0,t1)] (eVar px0))

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
