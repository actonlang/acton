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
import Acton.NameInfo
import Acton.Env
import Acton.Subst
import Acton.TypeEnv


-- Reduce conservatively and remove entailed constraints
simplifyNew                                 :: Env -> Constraints -> TypeM (Constraints,Equations)
simplifyNew env cs                          = do css <- groupCs env cs
                                                 --traceM ("#### SIMPLIFY NEW" ++ prstrs (map length css))
                                                 --sequence [ traceM ("## long:\n" ++ render (nest 4 $ vcat $ map pretty cs)) | cs <- css, length cs > 500 ]
                                                 combine <$> simplifyGroupsNew env css

simplifyGroupsNew env []                    = return ([], [])
simplifyGroupsNew env (cs:css)              = do --traceM ("\n\n######### simplifyNewGroup\n" ++ render (nest 4 $ vcat $ map pretty cs))
                                                 eq1 <- reduce env [] cs `catchError` \err -> Control.Exception.throw err
                                                 cs1 <- usubst =<< collectDeferred
                                                 (cs2,eq2) <- simplifyGroupsNew env css
                                                 return (cs1++cs2, eq1++eq2)

-- Reduce conservatively and remove entailed constraints
simplify                                    :: Env -> TEnv -> Type -> Constraints -> TypeM (Constraints,Equations)
simplify env te tt cs                       = do css <- groupCs env cs
                                                 te <- usubst te
                                                 tt <- usubst tt
                                                 --traceM ("#### SIMPLIFY " ++ prstrs (map length css))
                                                 --sequence [ traceM ("## long:\n" ++ render (nest 4 $ vcat $ map pretty cs)) | cs <- css, length cs > 500 ]
                                                 combine <$> simplifyGroups env te tt css

simplifyGroups env te tt []                 = return ([], [])
simplifyGroups env te tt (cs:css)           = do --traceM ("\n\n######### simplifyGroup\n" ++ render (nest 4 $ vcat $ map pretty cs))
                                                 (cs1,eq1) <- simplify' env te tt [] cs `catchError` \err -> Control.Exception.throw err
                                                 (cs2,eq2) <- simplifyGroups env te tt css
                                                 return (cs1++cs2, eq1++eq2)

simplify'                                   :: Env -> TEnv -> Type -> Equations -> Constraints -> TypeM (Constraints,Equations)
simplify' env te tt eq []                   = return ([], eq)
simplify' env te tt eq cs                   = do eq <- reduce env eq cs
                                                 cs <- usubst =<< collectDeferred
                                                 --traceM ("## Improving " ++ show (length cs))
                                                 --traceM ("## Improving:\n" ++ render (nest 8 $ vcat $ map pretty cs))
                                                 env <- usubst env      -- Remove....
                                                 te <- usubst te
                                                 tt <- usubst tt
                                                 improve env te tt eq cs

quicksimp env eq []                         = return ([], eq)
quicksimp env eq cs                         = do eq1 <- reduce env eq cs
                                                 cs1 <- usubst =<< collectDeferred
                                                 return (cs1, eq1)

groupCs env cs                              = do st <- currentState
                                                 mark 1 cs
                                                 m <- foldM group Map.empty cs
                                                 rollbackState st
                                                 let css = Map.elems m
                                                     i = length [ c | c@Imply{} <- cs ]
                                                     n = length (concat css)
                                                 --traceM ("#### Grouped " ++ show n ++ " (" ++ show i ++ ")" ++ " constraints into " ++ show (map length css) ++ " groups")
                                                 return css
  where mark n []                           = return n
        mark n (Imply _ _ _ cs' : cs)       = do n' <- mark n cs'
                                                 mark n' cs
        mark n (c : cs)                     = do tvs <- ufree <$> usubst c
                                                 tvs' <- ufree <$> usubst (map tUni $ attrfree c)
                                                 sequence [ unify (noinfo 1) (newUnivarToken n) (tUni tv) | tv <- nub (tvs++tvs') ]
                                                 mark (n+1) cs
        group m (Imply i w q cs)            = do m' <- foldM group Map.empty cs
                                                 return $ Map.foldrWithKey (\tv cs' -> Map.insertWith (++) tv [Imply i w q cs']) m m'
        group m c                           = do tvs <- ufree <$> usubst c
                                                 let tv = case tvs of [] -> tv0; tv:_ -> tv
                                                 return $ Map.insertWith (++) tv [c] m
        attrfree c@(Sel _ _ _ _ n _)        = allConAttrUFree env n
        attrfree c@(Mut _ _ _ n _)          = allConAttrUFree env n
        attrfree _                          = []
        TUni _ tv0                          = newUnivarToken 0


combine (cs, eqs)                           = (comb [] cs, insertOrMerge eqs [])
  where comb cs1 []                         = cs1
        comb cs1 (c@Imply{} : cs)           = comb (join c cs1) cs
        comb cs1 (c : cs)                   = c : comb cs1 cs
        join c []                           = [c]
        join c@Imply{wit=w} (c'@Imply{wit=w'} : cs)
          | w == w'                         = c{ scoped = scoped c ++ scoped c' } : cs
          | otherwise                       = c' : join c cs


----------------------------------------------------------------------------------------------------------------------
-- solve
----------------------------------------------------------------------------------------------------------------------
-- ###################################################################################################################

data Newrank                                = R_red
                                            | R_pos TUni [Type]
                                            | R_low TUni [Type]
                                            | R_neg TUni [Type]
                                            | R_amb TUni [Type]
                                            | R_var TUni TUni
                                            | R_ret
                                            deriving (Show)

weight R_red{}                              = 0
weight R_pos{}                              = 1
weight R_low{}                              = 2
weight R_neg{}                              = 3
weight R_amb{}                              = 4
weight R_var{}                              = 5
weight R_ret{}                              = 6

instance Eq Newrank where
    a == b                                  = weight a == weight b

instance Ord Newrank where
    a <= b                                  = weight a <= weight b


newrank pol (Sub info env _ t1 t2)          = newrank pol (Cast info env t1 t2)
newrank pol (Cast _ env (TUni _ v) (TUni _ v'))
                                            = R_var v v'
newrank pol (Cast _ env t (TUni _ v))
  | neg && not pos                          = R_pos v alts
  | otherwise                               = R_low v alts
  where (pos, neg)                          = (v `elem` fst pol, v `elem` snd pol)
        alts                                = allAbove env t
newrank pol (Cast _ env (TUni _ v) t)
  | neg && pos                              = R_ret
  | neg                                     = R_neg v alts
  | otherwise                               = R_amb v alts
  where (pos, neg)                          = (v `elem` fst pol, v `elem` snd pol)
        alts                                = allBelow env t
newrank pol (Proto _ env _ (TUni _ v) p)                                                   -- Proto behaves as an upper typ bound
  | neg && pos                              = R_ret
  | neg                                     = R_neg v alts
  | otherwise                               = R_amb v alts
  where (pos, neg)                          = (v `elem` fst pol, v `elem` snd pol)
        alts                                = allExtProto env p
newrank pol (Sel _ env _ (TUni _ v) n _)                                                   -- Sel behaves as an upper
  | neg && pos                              = R_ret
  | neg                                     = R_neg v alts
  | otherwise                               = R_amb v alts
  where (pos, neg)                          = (v `elem` fst pol, v `elem` snd pol)
        alts                                = allConAttr env n ++ allProtoAttr env n ++ allExtProtoAttr env n ++ [wildTuple]
newrank pol (Mut _ env (TUni _ v) n _)      = R_amb v alts
  where alts                                = allConAttr env n
newrank pol (Seal _ env (TUni _ v))
  | uvkind v == KFX                         = R_amb v [fxAction, fxPure]
newrank pol c                               = R_red


info0 = noinfo 0

newsolve env te eq cs = do
    te <- usubst te
    cs <- usubst cs
    newsolve' env te eq cs

newsolve' env te eq [] =                                        -- done
    return ([], eq)
newsolve' env te eq cs = do
    let pol = closePolVars (polvars te) cs
    st <- currentState
    case head $ sort $ map (newrank pol) cs of
        R_red -> do                                             -- reducible
            eq <- reduce env eq cs
            cs <- collectDeferred
            newsolve env te eq cs
        R_pos v ts ->                                           -- positive lower con
            newtry env st te eq cs v ts
        R_low v ts ->                                           -- general lower con
            newtry env st te eq cs v ts
        R_neg v ts ->                                           -- negative upper con
            newtry env st te eq cs v ts
        R_amb v ts ->                                           -- must solve upper con
            newtry env st te eq cs v ts
        R_var v v' -> do                                        -- var-var
            unify info0 (tUni v) (tUni v')
            newsolve env te eq cs
        R_ret ->                                                -- general upper con
            --coalesce env cs
            return (cs, eq)

newtry env st te eq cs v [] =
    noSolve0 env (Just $ tUni v) [] cs
newtry env st te eq cs v (t:ts) =
    (unify info0 (tUni v) t >> newsolve env te eq cs)
    `catchError`
    const (rollbackState st >> newtry env st te eq cs v ts)

-- ###################################################################################################################

data Rank                                   = RRed { cstr :: Constraint }
                                            | RSealed { tgt :: TUni }
                                            | RTry { tgt :: TUni, alts :: [Type], rev :: Bool }
                                            | RVar { tgt :: TUni, alts :: [Type] }
                                            | RImp QBinds [Rank]
                                            | RSkip
                                            deriving (Show)

instance Eq Rank where
    RRed _      == RRed _                   = True
    RSealed v1  == RSealed v2               = v1 == v2
    RTry v1 _ _ == RTry v2 _ _              = v1 == v2
    RVar v1 _   == RVar v2 _                = v1 == v2
    RSkip       == RSkip                    = True
    RImp _ _    == RImp _ _                 = False
    _           == _                        = False

instance Pretty Rank where
    pretty (RRed c)                         = text "<reduce>" <+> pretty c
    pretty (RSealed v)                      = pretty v <+> text "sealed"
    pretty (RTry v ts rev)                  = pretty v <+> braces (commaSep pretty ts) Pretty.<> (if rev then char '\'' else empty)
    pretty (RVar v ts)                      = pretty v <+> char '~' <+> commaSep pretty ts
    pretty (RImp q rs)                      = prettyQual q <+> braces (commaSep pretty rs)
    pretty RSkip                            = text "<skip>"

solve                                       :: Env -> (Constraint -> Bool) ->
                                               TEnv -> Type -> Equations -> Constraints -> TypeM (Constraints,Equations)
solve env select te tt eq cs                = do css <- groupCs env cs
                                                 te <- usubst te
                                                 tt <- usubst tt
                                                 (cs',eq') <- solveGroups env select te tt eq css
                                                 return $ combine (cs', eq')

solveGroups env select te tt eq []          = return ([], eq)
solveGroups env select te tt eq (cs:css)    = do --traceM ("\n\n######### solveGroup\n" ++ render (nest 4 $ vcat $ map pretty cs))
                                                 --traceM ("  ### te:\n" ++ render (nest 4 $ vcat $ map pretty te))
                                                 (cs1,eq1) <- solve' env select [] te tt eq cs `catchError` \err -> Control.Exception.throw err
                                                 (cs2,eq2) <- solveGroups env select te tt eq1 css
                                                 return (cs1++cs2, eq2)

solve' env select hist te tt eq cs
  | not $ null vargoals                     = do --traceM (unlines [ "### var goal " ++ prstr t ++ " ~ " ++ prstrs alts | RVar t alts <- vargoals ])
                                                 --traceM ("### var goals: " ++ show (sum [ length alts | RVar t alts <- vargoals ]))
                                                 sequence [ unify (noinfo 2) (tUni v) t | RVar v alts <- vargoals, t <- alts ]
                                                 proceed hist eq cs
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
                                                        proceed hist eq cs
                                                    RSealed v -> do
                                                        --traceM ("### try goal " ++ prstr v ++ ", candidates: " ++ prstrs [fxAction, fxPure])
                                                        tryAlts st v [fxAction, fxPure]
                                                    RTry v alts r -> do
                                                        --traceM ("### try goal " ++ prstr v ++ ", candidates: " ++ prstrs alts ++ if r then " (rev)" else "")
                                                        tryAlts st v alts
                                                    RVar v alts -> do
                                                        --traceM ("### var goal " ++ prstr v ++ ", unifying with " ++ prstrs alts)
                                                        unifyM (noinfo 3) alts (repeat $ tUni v) >> proceed hist eq cs
                                                    RSkip ->
                                                        return (keep_cs, eq)

  where (solve_cs, keep_cs)                 = partition select cs
        keep_evidence                       = [ hasWitness env t p | Proto _ env _ t p <- keep_cs ]

        (vargoals, goals)                   = span isVar $ sortOn deco $ flatten $ condense env rnks

        flatten []                          = []
        flatten (RImp _ rs' : rs)           = flatten (rs' ++ rs)
        flatten (r : rs)                    = r : flatten rs

        rnks                                = map (rank env) solve_cs

        tryAlts st tv []                    = do --traceM ("### FAIL " ++ prstr tv ++ ":\n" ++ render (nest 4 $ vcat $ map pretty cs))
                                                 cs <- return $ concat $ map (\c -> case c of Imply _ _ q cs -> cs; _ -> [c]) cs
                                                 let ts = map (\n -> tCon (TC (noQ ('t':show n)) [])) [0..]
                                                     vs = filter (\v -> length (filter (\c -> v `elem` ufree c) cs) > 1) (nub (ufree cs))
                                                     cs' = if length cs == 1 then cs else filter (not . useless vs) cs
                                                     vs' = filter (\v -> length (filter (\c -> v `elem` ufree c) cs') > 1) (nub (ufree cs'))
                                                 sequence [ usubstitute uv t | (uv,t) <- vs' `zip` ts ]
                                                 cs' <- usubst cs'
                                                 noSolve0 env (Just $ tUni tv) (take (length vs') ts) cs'
        tryAlts st tv (t:ts)                = tryAlt tv t `catchError` const (
                                                    do --traceM ("=== ROLLBACK " ++ prstr tv)
                                                       rollbackState st >> tryAlts st tv ts)
        tryAlt v (TCon _ c)
          | isProto env (tcname c)          = do p <- instwildcon env c
                                                 w <- newWitness
                                                 --traceM ("  # trying " ++ prstr v ++ " (" ++ prstr p ++ ")")
                                                 proceed hist eq (Proto (noinfo 4) env w (tUni v) p : cs)
        tryAlt v (TTuple _ _ _)
          | not $ null attrs                = do t <- instwild env KType (tTupleK $ foldr (\n -> kwdRow n tWild) tWild attrs)
                                                 --traceM ("  # trying tuple " ++ prstr v ++ " = " ++ prstr t)
                                                 unify (noinfo 5) (tUni v) t
                                                 proceed (t:hist) eq cs
          where selsOf cs                   = sortBy (\a b -> compare (nstr a) (nstr b)) $ nub [ n | Sel _ _ _ (TUni _ v') n _ <- cs, v' == v ]
                attrs                       = nub $ selsOf solve_cs ++ concat [ selsOf cs | Imply _ _ _ cs <- solve_cs ]
        tryAlt v t
          | uvkind v == KFX                 = do t <- instwild env (uvkind v) t
                                                 --traceM ("  # TRYING " ++ prstr v ++ " = " ++ prstr t)
                                                 unify (noinfo 5) (tUni v) t
                                                 (cs,eq) <- quicksimp env eq cs
                                                 hist <- usubst hist
                                                 solve' env select hist te tt eq cs
        tryAlt v t                          = do t <- instwild env (uvkind v) t
                                                 --traceM ("  # trying " ++ prstr v ++ " = " ++ prstr t)
                                                 unify (noinfo 5) (tUni v) t
                                                 proceed (t:hist) eq cs
        proceed hist eq cs                  = do te <- usubst te
                                                 tt <- usubst tt
                                                 (cs,eq) <- simplify' env te tt eq cs
                                                 hist <- usubst hist
                                                 solve' env select hist te tt eq cs

        condense env rs                     = map cond (group rs)
          where cond (RRed c : rs)          = RRed c
                cond (RSealed v : rs)       = RSealed v
                cond (RTry v as r : rs)     = RTry v (if rev' then subrev ts' else ts') rev'
                  where ts                  = foldr intersect as $ map alts rs
                        ts'                 = if v `elem` optvs then ts \\ [tOpt tWild] else ts
--                        rev'                = (or $ r : map rev rs) || v `elem` posvs
                        rev'                = (and $ r : map rev rs) || v `elem` posvs
                cond (RVar v as : rs)       = RVar v (foldr union as $ map alts rs)
                cond [RImp q rs]            = RImp q (condense env1 rs)
                  where env1                = defineTVars q env
                cond (RSkip : rs)           = RSkip
                cond rs                     = error ("### condense " ++ show rs)

                subrev []                   = []
                subrev (t:ts)               = subrev ts1 ++ t : subrev ts2
                  where (ts1,ts2)           = partition (\t' -> castable env t' t) ts

                group []                    = []
                group (r:rs)                = (r : rs1) : group rs2
                  where (rs1,rs2)           = partition (==r) rs

        optvs                               = optvars cs ++ optvars hist
        embvs                               = embvars cs
        univs                               = univars cs
        (posvs, negvs)                      = polvars te `polcat` polvars tt

        isVar RVar{}                        = True
        isVar _                             = False

        deco (RRed cs)                      = (0, 0, 0, 0)
        deco (RSealed v)                    = (2, 0, 0, 0)
        deco (RTry v as r)                  = (w, length $ filter (==v) embvs, length as, length $ filter (==v) univs)
          where w | uvkind v /= KFX         =  3    -- types and rows, normal search
                  | otherwise               =  4    -- effects, never qualified, last to be searched
        deco (RVar v as)                    = (5, 0, length as, 0)
        deco (RSkip)                        = (6, 0, 0, 0)


-- subrev [int,Pt,float,CPt,C3Pt]           = [] ++ int : subrev [Pt,float,CPt,C3Pt]
--                                          = int : subrev [CPt,C3Pt] ++ Pt : subrev [float]
--                                          = int : [C3Pt] ++ CPt ++ subrev [] ++ Pt : [] ++ float : subrev []
--                                          = int : C3Pt : CPt : Pt : float

rank                                        :: Env -> Constraint -> Rank
rank _ (Sub info env _ t1 t2)               = rank env (Cast info env t1 t2)

rank _ (Cast _ env (TUni _ v) t2@TUni{})    = RVar v [t2]
rank _ (Cast _ env (TUni _ v) (TOpt _ t2@TUni{}))
                                            = RVar v [t2]
rank _ (Cast _ env (TUni _ v) (TOpt _ t2))  = RTry v ([tOpt tWild, tNone] ++ allBelow env t2) False
rank _ (Cast _ env TNone{} (TUni _ v))      = RTry v [tOpt tWild, tNone] True
rank _ (Cast _ env (TUni _ v) t2)           = RTry v (allBelow env t2) False
rank _ (Cast _ env t1 (TUni _ v))           = RTry v (allAbove env t1) True

rank _ (Proto _ env _ (TUni _ v) p)         = RTry v ts False
  where ts                                  = allExtProto env p

rank _ (Sel _ env _ (TUni _ v) n _)         = RTry v (allConAttr env n ++ allProtoAttr env n ++ allExtProtoAttr env n ++ [wildTuple]) False
rank _ (Mut _ env (TUni _ v) n _)           = RTry v (allConAttr env n) False

rank _ (Seal _ env (TUni _ v))
  | uvkind v == KFX                         = RSealed v
  | otherwise                               = RSkip

rank env0 (Imply _ _ q cs)                  = RImp q (map (rank env1) cs)
  where env1                                = defineTVars q env0
rank _ c                                    = RRed c

wildTuple                                   = tTuple tWild tWild

splitImply cs                               = partition isImply cs
  where isImply Imply{}                     = True
        isImply _                           = False


-------------------------------------------------------------------------------------------------------------------------

class OptVars a where
    optvars                             :: a -> [TUni]

instance (OptVars a) => OptVars [a] where
    optvars                             = concat . map optvars

instance OptVars Constraint where
    optvars (Cast _ _ t1 t2)            = optvars [t1, t2]
    optvars (Sub _ w _ t1 t2)           = optvars [t1, t2]
    optvars (Proto _ w _ t p)           = optvars t ++ optvars p
    optvars (Sel _ w _ t1 n t2)         = optvars [t1, t2]
    optvars (Mut _ _ t1 n t2)           = optvars [t1, t2]
    optvars (Seal _ _ t)                = optvars t
    optvars (Imply _ _ q cs)            = optvars cs

instance OptVars Type where
    optvars (TOpt _ (TUni _ v))         = [v]
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
  where emb (Cast _ _ (TUni _ v) (TUni _ v'))
                                        = []
        emb (Cast _ _ (TUni _ v) t)     = ufree t
        emb (Cast _ _ t (TUni _ v))     = ufree t
        emb (Sub _ _ _ (TUni _ v) (TUni _ v'))
                                        = []
        emb (Sub _ _ _ (TUni _ v) t)    = ufree t
        emb (Sub _ _ _ t (TUni _ v))    = ufree t
        emb (Proto _ _ _ (TUni _ v) p)  = ufree p
        emb (Proto _ _ _ (TCon _ c) p)  = ufree c ++ ufree p
        emb (Sel _ _ _ (TUni _ v) n t)  = ufree t
        emb (Mut _ _ (TUni _ v) n t)    = ufree t
        emb _                           = []

univars cs                              = concat $ map uni cs
  where uni (Cast _ _ (TUni _ v) (TUni _ v'))
                                        = [v,v']
        uni (Sub _ _ _ (TUni _ v) (TUni _ v'))
                                        = [v,v']
        uni _                           = []

allAbove env (TCon _ tc)                = tOpt tWild : map tCon tcons
  where n                               = tcname tc
        tcons                           = allAncestors env tc ++ [schematic' tc]
allAbove env (TVar _ tv)                = [tOpt tWild, tCon tc, tVar tv]
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
    usubst (Eqn w t e)                      = Eqn w <$> usubst t <*> usubst e
    usubst (QEqn n q eqs)                   = QEqn n <$> usubst q <*> usubst eqs
    
instance UFree Equation where
    ufree (Eqn w t e)                       = ufree t ++ ufree e
    ufree (QEqn n q eqs)                    = ufree q ++ ufree eqs

instance Vars Equation where
    free (Eqn w t e)                        = free e
    free (QEqn n q eqs)                     = free q ++ (free eqs \\ bound q)

    bound (Eqn w t e)                       = [w]
    bound (QEqn w q eqs)                    = [w]



reduce                                      :: Env -> Equations -> Constraints -> TypeM Equations
reduce env_ eq []                           = return eq
reduce env_ eq (c:cs)                       = do c <- usubst c
                                                 --traceM ("   reduce " ++ prstr c)
                                                 eq1 <- reduce' env_ eq c
                                                 reduce env_ eq1 cs

reduce'                                     :: Env -> Equations -> Constraint -> TypeM Equations
reduce' env_ eq c@(Imply i w q cs)          = do cs0 <- collectDeferred
                                                 --traceM ("### reduce implication " ++ prstr w ++ ": " ++ prstr q ++ " =>\n" ++ render (nest 8 $ vcat $ map pretty cs))
                                                 eq' <- reduce env1 [] cs
                                                 cs' <- usubst =<< collectDeferred
                                                 when (not $ null cs') $ defer [Imply i w q cs']
                                                 defer cs0
                                                 return $ insertOrMerge [QEqn w q eq'] eq
  where env1                                = defineTVars q env_

reduce' _ eq c@(Cast i env t1 t2)           = do cast' env i t1 t2
                                                 return eq

reduce' _ eq c@(Sub i env w t1 t2)          = sub' env i eq w t1 t2

reduce' env_ eq c@(Proto _ env w TUni{} p)  = do defer [c]; return eq

reduce' env_ eq c@(Proto _ env w t@(TVar _ tv) p)
  | [wit] <- witSearch                      = do (eq',cs) <- solveProto env_ wit w t p
                                                 reduce env_ (eq'++eq) cs
  | [wit] <- witSearch'                     = do (eq',cs) <- solveProto env_ wit w (tCon tc) p
                                                 reduce env_ (eq'++eq) cs
  where witSearch                           = findWitness env_ t p
        tc                                  = findTVBound env_ tv
        witSearch'                          = findWitness env_ (tCon tc) p

reduce' env_ eq c@(Proto _ env w t@(TCon _ tc) p)
  | tcname p == qnIdentity,
    isActor env_ (tcname tc)                = do let e = eCall (eQVar primIdentityActor) []
                                                 return (Eqn w (proto2type t p) e : eq)
  | [wit] <- witSearch                      = do (eq',cs) <- solveProto env_ wit w t p
                                                 reduce env_ (eq'++eq) cs
  where witSearch                           = findWitness env_ t p

reduce' env_ eq c@(Proto _ env w t@(TFX _ tc) p)
  | [wit] <- witSearch                      = do (eq',cs) <- solveProto env_ wit w t p
                                                 reduce env_ (eq'++eq) cs
  where witSearch                           = findWitness env_ t p

reduce' env_ eq c@(Proto info env w t@(TOpt _ t') p)
  | tcname p == qnEq                        = do w' <- newWitness
                                                 let e = eCall (tApp (eQVar primEqOpt) [t']) [eVar w']
                                                 reduce env_ (Eqn w (proto2type t p) e : eq) [Proto info env w' t' p]

reduce' env_ eq c@(Proto _ env w t@(TNone _) p)
  | tcname p == qnEq                        = return (Eqn w (proto2type t p) (eQVar primWEqNone) : eq)

reduce' env_ eq c@(Sel _ env w TUni{} n _)  = do defer [c]; return eq

reduce' env_ eq c@(Sel _ env w (TVar _ tv) n _)
  | Just wsc <- attrSearch                  = do (eq',cs) <- solveSelAttr env_ wsc c
                                                 reduce env_ (eq'++eq) cs
  | Just p <- protoSearch                   = do (eq',cs) <- solveSelProto env_ p c
                                                 reduce env_ (eq'++eq) cs
  | otherwise                               = tyerr n "Attribute not found"
  where attrSearch                          = findTVAttr env_ tv n
        protoSearch                         = findProtoByAttr env_ (NoQ $ tvname tv) n

reduce' env_ eq c@(Sel _ env w (TCon _ tc) n _)
  | Just wsc <- attrSearch                  = do (eq',cs) <- solveSelAttr env_ wsc c
                                                 reduce env_ (eq'++eq) cs
  | Just p <- protoSearch                   = do (eq',cs) <- solveSelProto env_ p c
                                                 reduce env_ (eq'++eq) cs
  | otherwise                               = tyerr n "Attribute not found"
  where attrSearch                          = findAttr env_ tc n
        protoSearch                         = findProtoByAttr env_ (tcname tc) n


reduce' env_ eq c@(Sel info env w t1@(TTuple _ _ TUni{}) n t2)
                                            = do defer [c]; return eq

reduce' env_ eq c@(Sel info env w t1@(TTuple _ _ r) n t2)
  | n `elem` valueKWs                       = do let e = eLambda [(px0,t1)] (eDot (eVar px0) n)
                                                 return (Eqn w (wFun t1 t2) e : eq)
  | otherwise                               = do --traceM ("### Sel " ++ prstr c)
                                                 select r
  where select (TRow _ _ n' t r)
          | n == n'                         = do w' <- newWitness
                                                 let e = eLambda [(px0,t1)] (eDot (eCallVar w' [eVar px0]) n)
                                                 reduce env_ (Eqn w (wFun t1 t2) e : eq) [Sub info env w' t t2]
          | otherwise                       = select r
        select (TStar _ _ r)                = do w' <- newWitness
                                                 let e = eLambda [(px0,t1)] (eCallVar w' [eDot (eVar px0) attrKW])
                                                 reduce env_ (Eqn w (wFun t1 t2) e : eq) [Sel info env w' (tTupleK r) n t2]
        select (TNil _ _)                   = kwdNotFound0 env_ info n

--  lambda (x:(a:int,b:int,**(c:int))): x.b  ==>  lambda x: (b=x.b, a=x.a, KW=x.KW).b            ==>  lambda x: x.b
--  lambda (x:(a:int,b:int,**(c:int))): x.c  ==>  lambda x: (c=x.KW.x, a=x.a, b=x.b, KW=x.KW).c  ==>  lambda x: x.KW.c

reduce' env_ eq c@(Mut _ env TUni{} n _)    = do defer [c]; return eq

reduce' env_ eq c@(Mut _ env (TVar _ tv) n _)
  | Just wsc <- attrSearch                  = do solveMutAttr env_ wsc c
                                                 return eq
  | otherwise                               = tyerr n "Attribute not found:"
  where attrSearch                          = findTVAttr env_ tv n

reduce' env_ eq c@(Mut _ env (TCon _ tc) n _)
  | Just wsc <- attrSearch                  = do solveMutAttr env_ wsc c
                                                 return eq
  | otherwise                               = tyerr n "Attribute not found:"
  where attrSearch                          = findAttr env_ tc n

reduce' env_ eq c@(Seal _ env TUni{})         = do defer [c]; return eq

reduce' env_ eq c@(Seal _ env t@(TVar _ tv))  = return eq
reduce' env_ eq (Seal info env t@(TCon _ tc))
--  | castable env_ t tObject                 = tyerr t "Leaking actor seal:"                       -- when we start prohibit sharing of mutable data
  | otherwise                               = reduce env_ eq (map (Seal info env) $ tcargs tc)
reduce' env_ eq (Seal _ env t@(TFX _ fx))
--  | fx `elem` [FXMut,FXProc]                = tyerr t "Leaking actor seal:"
--  | fx `elem` [FXProc]                      = tyerr t "Leaking actor seal:"
  | otherwise                               = return eq
reduce' env_ eq (Seal info env t)           = reduce env_ eq (map (Seal info env) ts)
  where ts                                  = leaves t

reduce' env_ eq c                           = noRed0 env_ c


solveProto env wit w t p                    = do (cs,t',we) <- instWitness env p wit
                                                 unify (noinfo 7) t t'
                                                 return ([Eqn w (proto2type t p) we], cs)

solveSelAttr env_ (wf,sc,d) (Sel info env w t1 n t2)
                                            = do (cs,tvs,t) <- instantiate env_ sc
                                                 when (negself t) (tyerr n "Contravariant Self attribute not selectable by instance")
                                                 w' <- newWitness
                                                 let e = eLambda [(px0,t1)] (eCallVar w' [app t (tApp (eDot (wf $ eVar px0) n) tvs) $ protoWitsOf cs])
                                                     c = Sub (locinfo info 8) env w' (vsubst [(tvSelf,t1)] t) t2
                                                 return ([Eqn w (wFun t1 t2) e], c:cs)

--  e1.__setslice__(sl, e2)
--  e1.__setslice__(w_Iterable, sl, e2)
--  w_Sliceable.__setslice__(e1, w_Iterable, sl, e2)

--  w(e1)(sl,e2)                                                        w = lambda x0: lambda p1,p2: w_Sliceable.__setslice__(x0, w1, p1, p2)
--  (lambda p1,p2: w_Sliceable.__setslice__(x0, w1, p1, p2))(sl,e2)
--  w_Sliceable.__setslice__(x0, w1, sl, e2)                            w1 = w_Iterable
--  w_Sliceable.__setslice__(e1, w_Iterable, sl, e2)

solveSelProto env_ pn c@(Sel info env w t1 n t2)
                                            = do p <- instwildcon env_ pn
                                                 w' <- newWitness
                                                 (eq,cs) <- solveSelWit env_ (p, eVar w') c
                                                 return (eq, Proto info env w' t1 p : cs)

solveSelWit env_ (p,we) c0@(Sel info env w t1 n t2)
                                            = do let Just (wf,sc,d) = findAttr env_ p n
                                                 (cs,tvs,t) <- instantiate env_ sc
                                                 when (negself t) (tyerr n "Contravariant Self attribute not selectable by instance")
                                                 w' <- newWitness
                                                 let e = eLambda [(px0,t1)] (eCallVar w' [app t (tApp (eDot (wf we) n) tvs) $ eVar px0 : protoWitsOf cs])
                                                     c = Sub (noinfo 9) env w' (vsubst [(tvSelf,t1)] t) t2
                                                 return ([Eqn w (wFun t1 t2) e], c:cs)

solveMutAttr env_ (wf,sc,dec) c@(Mut info env t1 n t2)
                                            = do when (dec /= Just Property) (noMut n)
                                                 let TSchema _ [] t = sc
                                                 cast env_ (locinfo c 10) t2 (vsubst [(tvSelf,t1)] t)

----------------------------------------------------------------------------------------------------------------------
-- witness lookup
----------------------------------------------------------------------------------------------------------------------

findWitness                 :: Env -> Type -> PCon -> [Witness]
findWitness env t p         = reverse $ filter (eqhead t . wtype) $ witsByPName env $ tcname p
  where eqhead (TCon _ c) (TCon _ c')   = tcname c == tcname c'
        eqhead (TFX _ fx) (TFX _ fx')   = fx == fx'
        eqhead (TVar _ v) (TVar _ v')   = v == v'
        eqhead _          _             = False

findProtoByAttr env cn n    = case filter hasAttr $ witsByTName env cn of
                                [] -> Nothing
                                w:_ -> Just $ schematic' $ proto w
  where hasAttr w           = n `elem` conAttrs env (tcname $ proto w)

hasWitness                  :: Env -> Type -> PCon -> Bool
hasWitness env TUni{} p     = True
hasWitness env (TCon _ c) p
  | isActor env (tcname c),
    tcname p == qnIdentity  = True
hasWitness env t p          =  not $ null $ findWitness env t p

allExtProto                 :: Env -> PCon -> [Type]
allExtProto env p
  | p == pIdentity          = ts ++ [ schematic $ tCon tc | tc <- allCons env, isActor env (tcname tc) ]
  | otherwise               = ts
  where ts                  = reverse [ schematic (wtype w) | w <- witsByPName env (tcname p) ]

allExtProtoAttr             :: Env -> Name -> [Type]
allExtProtoAttr env n       = [ tCon tc | tc <- allCons env, any ((n `elem`) . allAttrs' env . proto) (witsByTName env $ tcname tc) ]


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

--                 as declared               as called
--                 existing                  expected
cast' env info t1@(TFun _ fx1 p1 k1 t1') t2@(TFun _ fx2 p2 k2 t2')
  | varTails [p1,p2] || varTails [k1,k2]    = do --traceM ("## Unifying funs: " ++ prstr t1 ++ " ~ " ++ prstr t2)
                                                 unify info t1 t2
                                                 return ()
  | otherwise                               = do --traceM ("### Aligning fun " ++ prstr t1 ++ " < " ++ prstr t2)
                                                 (cs1,ts) <- castpos env info p2 p1
                                                 cs2 <- castkwd0 env info ts k2 k1
                                                 t1 <- usubst t1
                                                 t2 <- usubst t2
                                                 let (TFun _ fx1 p1 k1 t1', TFun _ fx2 p2 k2 t2') = (t1, t2)
                                                 reduce env [] (Cast info env fx1 fx2 : Cast info env t1' t2' : cs1 ++ cs2)
                                                 return ()

cast' env info t1@(TTuple _ p1 k1) t2@(TTuple _ p2 k2)
  | varTails [p1,p2] || varTails [k1,k2]    = do --traceM ("### Unifying tuples: " ++ prstr t1 ++ " ~ " ++ prstr t2)
                                                 unify info t1 t2
                                                 return ()
  | otherwise                               = do --traceM ("### Aligning tuple " ++ prstr t1 ++ " < " ++ prstr t2)
                                                 (cs1,ts) <- castpos env info p1 p2
                                                 cs2 <- castkwd0 env info ts k1 k2
                                                 t1 <- usubst t1
                                                 t2 <- usubst t2
                                                 let (TTuple _ p1 k1, TTuple _ p2 k2) = (t1, t2)
                                                 reduce env [] (cs1 ++ cs2)
                                                 return ()

cast' env info (TOpt _ t1@TOpt{}) t2        = cast env info t1 t2
cast' env info t1 (TOpt _ t2@TOpt{})        = cast env info t1 t2
cast' env info (TOpt _ t1) (TOpt _ t2)      = cast env info t1 t2
cast' env info (TUni _ tv) t2@TNone{}       = do usubstitute tv tNone
                                                 cast env info tNone t2
cast' env info t1@TOpt{} (TUni _ tv)        = do t2 <- instwild env KType $ tOpt tWild      -- What if tv is in t1???
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

cast' env info (TUni _ tv) t2@TFun{}
  | uvkind tv == KType                      = do t1 <- instwild env KType $ tFun tWild tWild tWild tWild
                                                 usubstitute tv t1
                                                 cast env info t1 t2
cast' env info t1@TFun{} (TUni _ tv)                                                                             -- Should remove this, rejects tv = TOpt...
  | KType == uvkind tv                      = do t2 <- instwild env KType $ tFun tWild tWild tWild tWild
                                                 usubstitute tv t2
                                                 cast env info t1 t2
cast' env info (TUni _ tv) t2@TTuple{}
  | uvkind tv == KType                      = do t1 <- instwild env KType $ tTuple tWild tWild
                                                 usubstitute tv t1
                                                 cast env info t1 t2

cast' env info (TVar _ tv1) (TVar _ tv2)
  | tv1 == tv2                              = return ()

cast' env info (TUni _ tv1) (TUni _ tv2)
  | tv1 == tv2                              = return ()

cast' env info t1@(TUni _ tv) t2            = defer [Cast info env t1 t2]
cast' env info t1 t2@(TUni _ tv)            = defer [Cast info env t1 t2]

cast' env info t1@(TVar _ tv) t2            = cast' env info (tCon tc) t2
  where tc                                  = findTVBound env tv

cast' env info t1 (TOpt _ t2)               = cast env info t1 t2                -- Only matches when t1 is NOT a univar

cast' env info t1 t2                        = noRed0 env (Cast info env t1 t2)


castpos                                     :: Env -> ErrInfo -> PosRow -> PosRow -> TypeM (Constraints, [Type])
castpos env info TUni{}         TUni{}      = error "INTERNAL ERROR: castpos"
castpos env info (TUni _ tv)     r2
  | tv `elem` ufree r2                      = conflictingRow tv                     -- use rowTail?
  | otherwise                               = do r1 <- rowShape r2
                                                 --traceM (" ## castpos L " ++ prstr tv ++ " ~ " ++ prstr r1)
                                                 usubstitute tv r1
                                                 castpos env info r1 r2
castpos env info r1             (TUni _ tv)
  | tv `elem` ufree r1                      = conflictingRow tv                     -- use rowTail?
  | otherwise                               = do r2 <- rowShape r1
                                                 --traceM (" ## castpos R " ++ prstr r2 ++ " ~ " ++ prstr tv)
                                                 usubstitute tv r2
                                                 castpos env info r1 r2

castpos env info (TRow _ _ _ t1 r1) (TRow _ _ _ t2 r2)
                                            = do --traceM (" ## castpos A " ++ prstr t1 ++ " < " ++ prstr t2)
                                                 (cs,ts) <- castpos env info r1 r2
                                                 return (Cast info env t1 t2 : cs, ts)
castpos env info (TStar _ _ r1)     (TStar _ _ r2)
                                            = do --traceM (" ## castpos B " ++ prstr (tTupleP r1) ++ " < " ++ prstr (tTupleP r2))
                                                 return ([Cast info env (tTupleP r1) (tTupleP r2)], [])
castpos env info TNil{}             TNil{}
                                            = do --traceM (" ## castpos C ")
                                                 return ([], [])

castpos env info (TStar _ _ r1)     r2      = do --traceM (" ## castpos D " ++ prstr r1 ++ " ~ " ++ prstr r2)
                                                 castpos env info r1 r2
castpos env info r1                 (TStar _ _ r2)
                                            = do --traceM (" ## castpos E " ++ prstr r1 ++ " ~ " ++ prstr r2)
                                                 castpos env info r1 r2

castpos env info r1@TNil{}          r@(TRow _ _ _ t2 r2)
  | TOpt{} <- t2                            = do --traceM (" ## castpos F Opt ~ " ++ prstr t2)
                                                 castpos env info r1 r2
  | otherwise                               = do --traceM (" ## castpos G Nil ~ " ++ prstr r)
                                                 posElemNotFound0 env True (Cast info env r1 r) nWild
castpos env info (TRow _ _ _ t1 r1) r2@TNil{}
                                            = do --traceM (" ## castpos H " ++ prstr t1)
                                                 (cs,ts) <- castpos env info r1 r2
                                                 return (cs, t1 : ts)


castkwd0                                    :: Env -> ErrInfo -> [Type] -> KwdRow -> KwdRow -> TypeM Constraints
castkwd0 env info [] r1 r2                  = castkwd env info r1 r2
castkwd0 env info (t1:ts) r1 (TRow _ _ n t2 r2)
                                            = do --traceM (" ## castkwd0 extra pos for " ++ prstr n ++ ": " ++ prstr t1 ++ " < " ++ prstr t2)
                                                 cs <- castkwd0 env info ts r1 r2
                                                 return (Cast info env t1 t2 : cs)
castkwd0 env info ts r1 r2                  = posElemNotFound0 env False (Cast info env r1 r2) nWild

castkwd                                     :: Env -> ErrInfo -> KwdRow -> KwdRow -> TypeM Constraints
castkwd env info r1 (TUni _ tv)             = do unif r1
                                                 r2 <- usubst (tUni tv)
                                                 castkwd env info r1 r2
  where unif TUni{}                         = error "INTERNAL ERROR: castkwd"
        unif (TRow _ _ n t r)
          | tv `elem` ufree r               = conflictingRow tv                     -- use rowTail?
          | otherwise                       = do --traceM (" ## castkwd Row - Var: " ++ prstr (tRow KRow n t r) ++ " = " ++ prstr tv)
                                                 t2 <- newUnivar
                                                 r2 <- tRow KRow n t2 <$> newUnivarOfKind KRow
                                                 unify info (tUni tv) r2
        unif (TStar _ _ r)
          | tv `elem` ufree r               = conflictingRow tv                     -- use rowTail?
          | otherwise                       = do --traceM (" ## castkwd Star - Var: " ++ prstr (tStar KRow r) ++ " = " ++ prstr tv)
                                                 r2 <- tStar KRow <$> newUnivarOfKind KRow
                                                 unify info (tUni tv) r2
        unif TNil{}                         = do --traceM (" ## castkwd Nil - Var: " ++ prstr (tNil KRow) ++ " = " ++ prstr tv)
                                                 r2 <- pure $ tNil KRow
                                                 unify info (tUni tv) r2

castkwd env info r1 (TRow _ _ n2 t2 r2)     = do (t1,r1') <- pick r1
                                                 r2 <- usubst r2
                                                 cs <- castkwd env info r1' r2
                                                 return (Cast info env t1 t2 : cs)
  where pick (TUni _ tv)
          | tv `elem` ufree r2              = conflictingRow tv                     -- use rowTail?
          | otherwise                       = do --traceM (" ## castkwd Var - Row: " ++ prstr (tUni tv) ++ " = " ++ prstr (tRow KRow n2 t2 r2))
                                                 r1 <- tRow KRow n2 t2 <$> newUnivarOfKind KRow
                                                 unify info (tUni tv) r1
                                                 pick r1
        pick (TRow _ _ n t r)
          | n == n2                         = do --traceM (" ## castkwd Row - Row: " ++ prstr (tRow KRow n t r) ++ " = " ++ prstr (tRow KRow n2 t2 r2))
                                                 return (t, r)
          | otherwise                       = do --traceM (" ## castkwd Row - Row: " ++ prstr (tRow KRow n t r) ++ " ≠ " ++ prstr (tRow KRow n2 t2 r2))
                                                 kwdNotFound0 env info n2
        pick (TStar _ _ r)                  = do --traceM (" ## castkwd Star - Row: " ++ prstr (tStar KRow r) ++ " ≠ " ++ prstr (tRow KRow n2 t2 r2))
                                                 kwdNotFound0 env info n2
        pick TNil{}                         = do --traceM (" ## castkwd None - Row: " ++ prstr (tNil KRow) ++ " ≠ " ++ prstr (tRow KRow n2 t2 r2))
                                                 kwdNotFound0 env info n2

castkwd env info r1 (TStar _ _ r2)          = match r1
  where match (TUni _ tv)
          | tv `elem` ufree r2              = conflictingRow tv                     -- use rowTail?
          | otherwise                       = do --traceM (" ## castkwd Var - Star: " ++ prstr (tUni tv) ++ " = " ++ prstr (tStar KRow r2))
                                                 r1 <- tStar KRow <$> newUnivarOfKind KRow
                                                 unify info (tUni tv) r1
                                                 match r1
        match (TRow _ _ n t r)              = do --traceM (" ## castkwd Row - Star: " ++ prstr (tRow KRow n t r) ++ " ≠ " ++ prstr (tStar KRow r2))
                                                 kwdUnexpected info n
        match r1@(TStar _ _ r)
          | TUni _ v <- r, TUni _ v2 <- r2  = do --traceM (" ## castkwd StarVar - StarVar: " ++ prstr (tStar KRow r) ++ " = " ++ prstr (tStar KRow r2))
                                                 unify info r r2
                                                 return []
          | TUni _ v <- r                   = do --traceM (" ## castkwd StarVar - Star: " ++ prstr (tStar KRow r) ++ " = " ++ prstr (tStar KRow r2))
                                                 castkwd env info r1 r2
          | otherwise                       = do --traceM (" ## castkwd Star - Star: " ++ prstr (tStar KRow r) ++ " = " ++ prstr (tStar KRow r2))
                                                 castkwd env info r r2
        match r1@TNil{}                     = do --traceM (" ## castkwd Nil - Star: " ++ prstr (tNil KRow) ++ " ≠ " ++ prstr (tStar KRow r2))
                                                 noRed0 env (Cast info env r1 r2)

castkwd env info r1 r2@TNil{}               = term r1
  where term (TUni _ tv)                    = do --traceM (" ## castkwd Var - Nil: " ++ prstr (tUni tv) ++ " = " ++ prstr (tNil KRow))
                                                 r1 <- pure $ tNil KRow
                                                 unify info (tUni tv) r1
                                                 term (tNil KRow)
        term (TNil _ _)                     = do --traceM (" ## castkwd Nil - Nil: " ++ prstr (tNil KRow) ++ " = " ++ prstr (tNil KRow))
                                                 return []
        term (TRow _ _ n t r)               = do --traceM (" ## castkwd Row - Nil: " ++ prstr (tRow KRow n t r) ++ " ≠ " ++ prstr (tNil KRow))
                                                 kwdUnexpected info n
        term (TStar _ _ r)                  = do --traceM (" ## castkwd Star - Nil: " ++ prstr (tStar KRow r) ++ " ≠ " ++ prstr (tNil KRow))
                                                 noRed0 env (Cast info env r1 r2)



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
  | otherwise                               = do --traceM ("### Aligning fun " ++ prstr w ++ ": " ++ prstr t1 ++ " < " ++ prstr t2)
                                                 (cs1,ap,es) <- subpos env info ((map eVar pNames)!!) 0 p2 p1
                                                 (cs2,ak) <- subkwd0 env info eVar es k2 k1
                                                 t1 <- usubst t1
                                                 t2 <- usubst t2
                                                 w' <- newWitness
                                                 let (TFun _ fx1 p1 k1 t1', TFun _ fx2 p2 k2 t2') = (t1, t2)
                                                     (pp,pk) = (pPar pNames p2, kPar attrKW k2)
                                                     lambda = eLambda [(px0,t1)] $ Lambda l0 pp pk (eCallVar w' [Call l0 (eVar px0) ap ak]) fx1
                                                 reduce env (Eqn w (wFun t1 t2) lambda : eq) (Cast info env fx1 fx2 : Sub info env w' t1' t2':cs1++cs2)

--                     existing            expected
sub' env info eq w t1@(TTuple _ p1 k1) t2@(TTuple _ p2 k2)
  | varTails [p1,p2] || varTails [k1,k2]    = do --traceM ("### Unifying tuples: " ++ prstr w ++ ": " ++ prstr t1 ++ " ~ " ++ prstr t2)
                                                 unify info t1 t2
                                                 return (idwit env w t1 t2 : eq)
  | otherwise                               = do --traceM ("### Aligning tuple " ++ prstr w ++ ": " ++ prstr t1 ++ " < " ++ prstr t2)
                                                 (cs1,ap,es) <- subpos env info (eDotI (eVar px0) . toInteger) 0 p1 p2
                                                 (cs2,ak) <- subkwd0 env info (eDot (eVar px0)) es k1 k2
                                                 t1 <- usubst t1
                                                 t2 <- usubst t2
                                                 let (TTuple _ p1 k1, TTuple _ p2 k2) = (t1, t2)
                                                     lambda = eLambda [(px0,t1)] (Paren l0 $ Tuple l0 ap ak)
                                                 reduce env (Eqn w (wFun t1 t2) lambda : eq) (cs1++cs2)

sub' env info eq w (TUni _ tv) t2@TFun{}    = do t1 <- instwild env KType $ tFun tWild tWild tWild tWild
                                                 usubstitute tv t1
                                                 sub env info eq w t1 t2
sub' env info eq w t1@TFun{} (TUni _ tv)    = do t2 <- instwild env KType $ tFun tWild tWild tWild tWild
                                                 usubstitute tv t2
                                                 sub env info eq w t1 t2

sub' env info eq w (TUni _ tv) t2@TTuple{}  = do t1 <- instwild env KType $ tTuple tWild tWild
                                                 usubstitute tv t1
                                                 sub env info eq w t1 t2

sub' env info eq w t1@TTuple{} t2@(TUni _ tv)
                                            = do defer [Sub info env w t1 t2]; return eq        -- Don't let cast solve this by idwit!

sub' env info eq w t1@(TVar _ tv1) t2@(TVar _ tv2)
  | tv1 == tv2                              = return (idwit env w t1 t2 : eq)

sub' env info eq w t1@(TUni _ tv1) t2@(TUni _ tv2)
  | tv1 == tv2                              = return (idwit env w t1 t2 : eq)

sub' env info eq w t1@(TUni _ tv1) t2@(TUni _ tv2)
                                            = do defer [Sub info env w t1 t2]; return eq

sub' env info eq w t1 t2                    = do cast env info t1 t2
                                                 return (idwit env w t1 t2 : eq)


rowTail (TRow _ _ _ _ r)                    = rowTail r
rowTail r                                   = r

varTails                                    = all (isUnivar . rowTail)

rowShape (TRow _ k n t r)                   = do t' <- newUnivar
                                                 r' <- rowShape r
                                                 return (tRow k n t' r')
rowShape (TStar _ k r)                      = do r' <- rowShape r
                                                 return (tStar k r')
rowShape r                                  = return r

subpos                                      :: Env -> ErrInfo -> (Int -> Expr) -> Int -> PosRow -> PosRow -> TypeM (Constraints, PosArg, [(Expr,Type)])
subpos env info f i TUni{}         TUni{}   = error "INTERNAL ERROR: subpos"
subpos env info f i (TUni _ tv)     r2
  | tv `elem` ufree r2                      = conflictingRow tv                     -- use rowTail?
  | otherwise                               = do r1 <- rowShape r2
                                                 --traceM (" ## subpos L " ++ prstr tv ++ " ~ " ++ prstr r1)
                                                 usubstitute tv r1
                                                 subpos env info f i r1 r2
subpos env info f i r1             (TUni _ tv)
  | tv `elem` ufree r1                      = conflictingRow tv                     -- use rowTail?
  | otherwise                               = do r2 <- rowShape r1
                                                 --traceM (" ## subpos R " ++ prstr r2 ++ " ~ " ++ prstr tv)
                                                 usubstitute tv r2
                                                 subpos env info f i r1 r2

subpos env info f i (TRow _ _ _ t1 r1) (TRow _ _ _ t2 r2)
                                            = do --traceM (" ## subpos A " ++ prstr t1 ++ " < " ++ prstr t2)
                                                 (cs,as,es) <- subpos env info f (i+1) r1 r2
                                                 w <- newWitness
                                                 return (Sub info env w t1 t2 : cs, PosArg (eCallVar w [f i]) as, es)
subpos env info f i (TStar _ _ r1)     (TStar _ _ r2)
                                            = do --traceM (" ## subpos B " ++ prstr (tTupleP r1) ++ " < " ++ prstr (tTupleP r2))
                                                 w <- newWitness
                                                 return ([Sub info env w (tTupleP r1) (tTupleP r2)], PosStar (eCallVar w [f i]), [])
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
                                                 posElemNotFound0 env True (Cast info env r1 r) nWild
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
                                                 return (Sub info env w t1 t2 : cs, KwdArg n (eCallVar w [e]) as)
subkwd0 env info f ((e,t1):es) r1 r2        = posElemNotFound0 env False (Cast info env r1 r2) nWild

subkwd                                      :: Env -> ErrInfo -> (Name -> Expr) -> [Name] -> KwdRow -> KwdRow -> TypeM (Constraints, KwdArg)
subkwd env info f seen r1 (TUni _ tv)       = do unif f seen r1
                                                 r2 <- usubst (tUni tv)
                                                 subkwd env info f seen r1 r2
  where unif f seen TUni{}                  = error "INTERNAL ERROR: subkwd"
        unif f seen (TRow _ _ n t r)
          | n `elem` seen                   = do --traceM (" ## subkwd (Row) - Var: " ++ prstr (tRow KRow n t r) ++ " [" ++ prstrs seen ++ "] ≈ " ++ prstr tv)
                                                 unif f (seen\\[n]) r
          | tv `elem` ufree r               = conflictingRow tv                     -- use rowTail?
          | otherwise                       = do --traceM (" ## subkwd Row - Var: " ++ prstr (tRow KRow n t r) ++ " [" ++ prstrs seen ++ "] ≈ " ++ prstr tv)
                                                 t2 <- newUnivar
                                                 r2 <- tRow KRow n t2 <$> newUnivarOfKind KRow
                                                 unify info (tUni tv) r2
        unif f seen (TStar _ _ r)
          | tv `elem` ufree r               = conflictingRow tv                     -- use rowTail?
          | otherwise                       = do --traceM (" ## subkwd Star - Var: " ++ prstr (tStar KRow r) ++ " [" ++ prstrs seen ++ "] ≈ " ++ prstr tv)
                                                 r2 <- tStar KRow <$> newUnivarOfKind KRow
                                                 unify info (tUni tv) r2
        unif f seen TNil{}                  = do --traceM (" ## subkwd Nil - Var: " ++ prstr (tNil KRow) ++ " [" ++ prstrs seen ++ "] ≈ " ++ prstr tv)
                                                 r2 <- pure $ tNil KRow
                                                 unify info (tUni tv) r2

subkwd env info f seen r1 (TRow _ _ n2 t2 r2)
                                            = do (cs1,e) <- pick f seen r1
                                                 r1 <- usubst r1
                                                 r2 <- usubst r2
                                                 (cs2,as) <- subkwd env info f (n2:seen) r1 r2
                                                 return (cs1++cs2, KwdArg n2 e as)
  where pick f seen (TUni _ tv)
          | tv `elem` ufree r2              = conflictingRow tv                     -- use rowTail?
          | otherwise                       = do --traceM (" ## subkwd Var - Row: " ++ prstr (tVar tv) ++ " [" ++ prstrs seen ++ "] ≈ " ++ prstr (tRow KRow n2 t2 r2))
                                                 r1 <- tRow KRow n2 t2 <$> newUnivarOfKind KRow
                                                 unify info (tUni tv) r1
                                                 pick f seen r1
        pick f seen (TRow _ _ n t r)
          | n `elem` seen                   = do --traceM (" ## subkwd (Row) - Row: " ++ prstr (tRow KRow n t r) ++ " [" ++ prstrs seen ++ "] ≈ " ++ prstr (tRow KRow n2 t2 r2))
                                                 pick f (seen\\[n]) r
          | n /= n2                         = pick f seen r
          | otherwise                       = do --traceM (" ## subkwd Row! - Row: " ++ prstr (tRow KRow n t r) ++ " [" ++ prstrs seen ++ "] ≈ " ++ prstr (tRow KRow n2 t2 r2))
                                                 w <- newWitness
                                                 return ([Sub info env w t t2], eCallVar w [f n])
        pick f seen (TStar _ _ r)           = do --traceM (" ## subkwd Star - Row: " ++ prstr (tStar KRow r) ++ " [" ++ prstrs seen ++ "] ≈ " ++ prstr (tRow KRow n2 t2 r2))
                                                 pick (eDot (f attrKW)) seen r
        pick f seen (TNil _ _)
          | TOpt{} <- t2                    = do --traceM (" ## subkwd None - Row: " ++ prstr (tNil KRow) ++ " [" ++ prstrs seen ++ "] ≈ " ++ prstr (tRow KRow n2 t2 r2))
                                                 return ([], eNone)
          | otherwise                       = kwdNotFound0 env info n2

subkwd env info f seen r1 (TStar _ _ r2)    = do (cs,e) <- match f seen r1
                                                 return (cs, KwdStar e)
  where match f seen (TUni _ tv)
          | tv `elem` ufree r2              = conflictingRow tv                     -- use rowTail?
          | otherwise                       = do --traceM (" ## subkwd Var - Star: " ++ prstr (tVar tv) ++ " [" ++ prstrs seen ++ "] ≈ " ++ prstr (tStar KRow r2))
                                                 r1 <- tStar KRow <$> newUnivarOfKind KRow
                                                 unify info (tUni tv) r1
                                                 match f seen r1
        match f seen r1@(TRow _ _ n t r)
          | n `elem` seen                   = do --traceM (" ## subkwd (Row) - Star: " ++ prstr (tRow KRow n t r) ++ " [" ++ prstrs seen ++ "] ≈ " ++ prstr (tStar KRow r2))
                                                 match f (seen\\[n]) r
          | otherwise                       = do --traceM (" ## subkwd Row - Star: " ++ prstr (tRow KRow n t r) ++ " [" ++ prstrs seen ++ "] ≈ " ++ prstr (tStar KRow r2))
                                                 (cs,as) <- subkwd env info f seen r1 r2
                                                 return (cs, eTupleK as)
        match f seen r1@(TStar _ _ r)
          | TUni _ v <- r, TUni _ v2 <- r2  = do --traceM (" ## subkwd StarVar - StarVar: " ++ prstr (tStar KRow r) ++ " [" ++ prstrs seen ++ "] ≈ " ++ prstr (tStar KRow r2))
                                                 unify info r r2
                                                 return ([], f attrKW)
          | TUni _ v <- r                   = do --traceM (" ## subkwd StarVar - Star: " ++ prstr (tStar KRow r) ++ " [" ++ prstrs seen ++ "] ≈ " ++ prstr (tStar KRow r2))
                                                 (cs,as) <- subkwd env info f seen r1 r2
                                                 return (cs, eTupleK as)
          | otherwise                       = do --traceM (" ## subkwd Star - Star: " ++ prstr (tStar KRow r) ++ " [" ++ prstrs seen ++ "] ≈ " ++ prstr (tStar KRow r2))
                                                 match (eDot (f attrKW)) seen r
        match f seen r1@TNil{}              = do --traceM (" ## subkwd Nil - Star: " ++ prstr (tNil KRow) ++ " [" ++ prstrs seen ++ "] ≈ " ++ prstr (tStar KRow r2))
                                                 (cs,as) <- subkwd env info f seen r1 r2
                                                 return (cs, eTupleK as)

subkwd env info f seen r1 TNil{}            = term f seen r1
  where term f seen (TUni _ tv)             = do --traceM (" ## subkwd Var - Nil: " ++ prstr (tVar tv) ++ " [" ++ prstrs seen ++ "] ≈ " ++ prstr (tNil KRow))
                                                 r1 <- pure $ tNil KRow
                                                 unify info (tUni tv) r1
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
    f (Cast _ _ (TUni _ v1) (TUni _ v2) : cs)
      | v1 == v2                            = f cs
      | otherwise                           = f cs . varvar v1 v2
    f (Cast _ _ (TUni _ v) t : cs)          = f cs . ubound v t . embed (ufree t)
    f (Cast _ _ t (TUni _ v) : cs)          = f cs . lbound v t . embed (ufree t)
    f (Sub _ _ _ (TUni _ v1) (TUni _ v2) : cs)
      | v1 == v2                            = f cs
      | otherwise                           = f cs . varvar v1 v2
    f (Sub _ _ _ (TUni _ v) t : cs)         = f cs . ubound v t . embed (ufree t)
    f (Sub _ _ _ t (TUni _ v) : cs)         = f cs . lbound v t . embed (ufree t)
    f (Proto _ _ w (TUni _ v) p : cs)       = f cs . pbound v w p . embed (ufree p)
    f (Proto _ _ w t p : cs)
      | not $ null vs                       = f cs . embed (vs ++ ufree p)
      where vs                              = nub $ ufree t
    f (Mut _ _ (TUni _ v) n t : cs)         = f cs . mutattr v n . embed (ufree t)
    f (Sel _ _ _ (TUni _ v) n t : cs)       = f cs . selattr v n . embed (ufree t)
    f (Sel _ _ _ (TTuple _ _ TUni{}) _ _ : cs)
                                            = f cs
    f (Seal _ _ (TUni _ v) : cs)            = f cs . seal v
    f (Imply _ _ _ cs' : cs)                = f (cs'++cs)
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
--  Cast/Sub bound is either TUni (upper), TCon, TNone (lower), TOpt (upper) or TFX
--  acyclic
--  G-minimal (constrained vars are observable)
--  S-minimal (constrained vars are invariant)
--  just single upper/lower bounds
--  no closed upper/lower bounds
--  no redundant Proto
--  no Sel/Mut covered by Cast/Sub/Proto bounds


instance Pretty (TUni, [Type]) where
    pretty (uv, ts)                     = pretty uv <+> text "<" <+> pretty ts

instance Pretty [Type] where
    pretty ts                           = brackets (commaSep pretty ts)

instance Pretty (TUni, Type) where
    pretty (uv, t)                      = pretty uv <+> text "~" <+> pretty t

improve                                 :: Env -> TEnv -> Type -> Equations -> Constraints -> TypeM (Constraints,Equations)
improve env te tt eq []                 = return ([], eq)
improve env te tt eq cs
  | Nothing <- info                     = do --traceM ("  *Resubmit " ++ show (length cs))
                                             simplify' env te tt eq cs
  | Left (v,vs) <- closure              = do --traceM ("  *Unify cycle " ++ prstr v ++ " = " ++ prstrs vs)
                                             sequence [ unify (noinfo 12) (tUni v) (tUni v') | v' <- vs ]
                                             simplify' env te tt eq cs
  | not $ null gsimple                  = do --traceM ("  *G-simplify " ++ prstrs [ (v,tUni v') | (v,v') <- gsimple ])
                                             --traceM ("  *obsvars: " ++ prstrs obsvars)
                                             --traceM ("  *varvars: " ++ prstrs (varvars vi))
                                             sequence [ unify (noinfo 13) (tUni v) (tUni v') | (v,v') <- gsimple ]
                                             simplify' env te tt eq cs
  | not $ null cyclic                   = tyerrs cyclic ("Cyclic subtyping:")
  | not $ null (multiUBnd++multiLBnd)   = do ub <- mapM (mkGLB env) multiUBnd   -- GLB of the upper bounds
                                             lb <- mapM (mkLUB env) multiLBnd   -- LUB of the lower bounds
                                             --traceM ("  *GLB " ++ prstrs ub)
                                             --traceM ("  *LUB " ++ prstrs lb)
                                             simplify' env te tt eq (replace ub lb cs)
  | not $ null posLBnd                  = do --traceM ("  *S-simplify (dn) " ++ prstrs posLBnd)
                                             --traceM ("   posnames "  ++ prstrs (posnames $ envX env))
                                             sequence [ unify (noinfo 15) (tUni v) t | (v,t) <- posLBnd ]
                                             simplify' env te tt eq cs
  | not $ null negUBnd                  = do --traceM ("  *S-simplify (up) " ++ prstrs negUBnd)
                                             --traceM ("   posnames "  ++ prstrs (posnames $ envX env))
                                             sequence [ unify (noinfo 16) (tUni v) t | (v,t) <- negUBnd ]
                                             simplify' env te tt eq cs
  | not $ null closUBnd                 = do --traceM ("  *Simplify upper closed bound " ++ prstrs closUBnd)
                                             sequence [ unify (noinfo 17) (tUni v) t | (v,t) <- closUBnd ]
                                             simplify' env te tt eq cs
  | not $ null closLBnd                 = do --traceM ("  *Simplify lower closed bound " ++ prstrs closLBnd)
                                             sequence [ unify (noinfo 18) (tUni v) t | (v,t) <- closLBnd ]
                                             simplify' env te tt eq cs
  | not $ null redEq                    = do --traceM ("  *(Context red) " ++ prstrs (deepwits redEq))
                                             sequence [ unify (noinfo 19) t1 t2 | (t1,t2) <- redUni ]
                                             simplify' env te tt (insertOrMerge redEq eq) (remove (deepwits redEq) cs)
  | not $ null dots                     = do --traceM ("  *Implied mutation/selection solutions " ++ prstrs dots)
                                             (eq',cs') <- solveDots env mutC selC selP cs
                                             simplify' env te tt (eq'++eq) cs'
  | not $ null redSeal                  = do --traceM ("  *removing redundant Seal constraints on: " ++ prstrs redSeal)
                                             return (filterOut redSeal cs, eq)
  | otherwise                           = do --traceM ("  *improvement done " ++ show (length cs))
                                             return (cs, eq)
  where info                            = varinfo cs
        Just vi                         = info
        closure                         = varclose (varvars vi)
        Right vclosed                   = closure
        (vvsL,vvsU)                     = unzip vclosed
        gsimple                         = gsimp vi vclosed obsvars (varvars vi)
        multiUBnd                       = [ (v,ts) | (v,ts) <- multiUBounds cs, noEmbed v, noLOpt v ]
        multiLBnd                       = [ (v,ts) | (v,ts) <- multiLBounds cs, noEmbed v ]
        lowerBnd                        = [ (v,t) | (v,[t]) <- Map.assocs (lbounds vi), noEmbed v ]
        upperBnd                        = [ (v,t) | (v,[t]) <- Map.assocs (ubounds vi), noEmbed v ]
        posLBnd                         = [ (v,t) | (v,t) <- lowerBnd, v `notElem` negvars, implAll env (lookup' v $ pbounds vi) t ]
        negUBnd                         = [ (v,t) | (v,t) <- upperBnd, v `notElem` posvars, implAll env (lookup' v $ pbounds vi) t, noDots env vi v ]
        closLBnd                        = [ (v,t) | (v, [t]) <- Map.assocs (lbounds vi), upClosed env t, implAll env (lookup' v $ pbounds vi) t ]
        closUBnd                        = [ (v,t) | (v, [t]) <- Map.assocs (ubounds vi), dnClosed env t, implAll env (lookup' v $ pbounds vi) t, noDots env vi v ]
        (redEq,redUni)                  = ctxtReduce env cs
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
        noEmbed v                       = v `notElem` embedded vi
        noLOpt v                        = not $ any optCon $ lookup' v (lbounds vi)
          where optCon TNone{}                  = True
                optCon TOpt{}                   = True
                optCon _                        = False

multiUBounds cs                         = Map.assocs $ Map.filter ((>1) . length) $ foldr (Map.unionWith app1) Map.empty maps
  where
    maps                                = bnds cs : imps cs

    bnds []                             = Map.empty
    bnds (Cast _ _ TUni{} TUni{} : cs)  = bnds cs
    bnds (Cast _ _ (TUni _ v) t : cs)   = unOpt v t cs
    bnds (Sub _ _ _ TUni{} TUni{} : cs) = bnds cs
    bnds (Sub _ _ _ (TUni _ v) t : cs)  = unOpt v t cs
    bnds (_ : cs)                       = bnds cs

    imps []                             = []
    imps (Imply _ _ _ cs' : cs)         = bnds cs' : imps cs
    imps (_ : cs)                       = imps cs

    app1 [x] [y] | x == y               = [x]
    app1 xs ys                          = xs ++ ys

    unOpt v (TOpt _ TUni{}) cs          = bnds cs
    unOpt v (TOpt _ t) cs               = Map.insertWith (++) v [t] $ bnds cs
    unOpt v t cs                        = Map.insertWith (++) v [t] $ bnds cs

multiLBounds cs                         = Map.assocs $ Map.filter ((>1) . length) $ foldr (Map.unionWith app1) Map.empty maps
  where
    maps                                = bnds cs : imps cs

    bnds []                             = Map.empty
    bnds (Cast _ _ TUni{} TUni{} : cs)  = bnds cs
    bnds (Cast _ _ t (TUni _ v) : cs)   = Map.insertWith (++) v [t] $ bnds cs
    bnds (Sub _ _ _ TUni{} TUni{} : cs) = bnds cs
    bnds (Sub _ _ _ t (TUni _ v) : cs)  = Map.insertWith (++) v [t] $ bnds cs
    bnds (_ : cs)                       = bnds cs

    imps []                             = []
    imps (Imply _ _ _ cs' : cs)         = bnds cs' : imps cs
    imps (_ : cs)                       = imps cs

    app1 [x] [y] | x == y               = [x]
    app1 xs ys                          = xs ++ ys


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

replace ub lb cs                        = ubs ++ lbs ++ cs'
  where
    (vss,cs')                           = unzip $ map repl cs
    vs                                  = nubBy (\a b -> fst a == fst b) (concat vss)
    ubs                                 = [ Cast info env (tUni v) t | (v,env) <- vs, Just t <- [lookup v ub] ]
    lbs                                 = [ Cast info env t (tUni v) | (v,env) <- vs, Just t <- [lookup v lb] ]
    info                                = noinfo 14

    repl c@(Cast _ _ TUni{} TUni{})           = ([], c)
    repl c@(Cast _ _ TUni{} (TOpt _ TUni{}))  = ([], c)
    repl (Cast info env (TUni _ v) t)
      | Just t' <- lookup v ub                = ([(v,env)], Cast info env t' t)
    repl (Cast info env t (TUni _ v))
      | Just t' <- lookup v lb                = ([(v,env)], Cast info env t t')
    repl c@(Sub _ _ _ TUni{} TUni{})          = ([], c)
    repl c@(Sub _ _ _ TUni{} (TOpt _ TUni{})) = ([], c)
    repl (Sub info env w (TUni _ v) t)
      | Just t' <- lookup v ub                = ([(v,env)], Sub info env w t' t)
    repl (Sub info env w t (TUni _ v))
      | Just t' <- lookup v lb                = ([(v,env)], Sub info env w t t')
    repl (Imply info w q cs)                  = ([], Imply info w q $ replace ub lb cs)
    repl c                                    = ([], c)


solveDots env_ mutC selC selP cs        = do (eqs,css) <- unzip <$> mapM solveDot cs
                                             return (insertOrMerge (concat eqs) [], concat css)
  where solveDot c@(Mut _ env (TUni _ v) n _)
          | Just w <- lookup (v,n) mutC = solveMutAttr env_ w c >> return ([], [])
        solveDot c@(Sel _ _ env (TUni _ v) n _)
          | Just w <- lookup (v,n) selC = solveSelAttr env_ w c
          | Just w <- lookup (v,n) selP = solveSelWit env_ w c
        solveDot (Imply i w q cs)       = do (eq,cs) <- solveDots (defineTVars q env_) mutC selC selP cs
                                             return (if null eq then [] else [QEqn w q eq], if null cs then [] else [Imply i w q cs])
        solveDot c                      = return ([], [c])

instance Pretty (TUni,Name) where
    pretty (v,n)                        = pretty v Pretty.<> text "." Pretty.<> pretty n

ctxtReduce                              :: Env -> Constraints -> (Equations, [(Type,Type)])
ctxtReduce env cs                       = (eq0, uni0) -- foldr combine (eq0,uni0) qreds
  where (eq0,uni0)                      = ctxtRed env (multiPBounds cs)
        qreds                           = [ qualeq w q $ ctxtRed env (multiPBounds cs') | Imply i w q cs' <- cs ]
        qualeq w q (eq,uni)             = (if null eq then [] else [QEqn w q eq], uni)
        combine (qe,uni) (eq0,ini0)     = (insertOrMerge qe eq0, uni++uni0)

instance Pretty (TUni, [(Name,PCon)]) where
    pretty (tv, wps)                    = pretty tv <+> parens (commaSep pretty wps)

instance Pretty (Name,PCon) where
    pretty (w,p)                        = pretty w <+> colon <+> pretty p

multiPBounds cs                         = Map.assocs $ f cs Map.empty
  where
    f []                                = Map.filter ((>1) . length)
    f (Proto _ _ w (TUni _ v) p : cs)   = f cs . Map.insertWith (++) v [(w,p)]
    f (_ : cs)                          = f cs

ctxtRed                                 :: Env -> [(TUni, [(Name, PCon)])] -> (Equations, [(Type,Type)])
ctxtRed env multiPBnds                  = (concat eqs, concat unis)
  where (eqs,unis)                      = unzip $ map ctxtRed multiPBnds
        ctxtRed (v,wps)                 = imp v [] [] [] wps
        imp v eq uni wps ((w,p):wps')
          | (e,p'):_ <- hits            = --trace ("  *" ++ prstr w ++ " covered by " ++ prstr e) $
                                          imp v (Eqn w (proto2type (tUni v) p) e : eq) ((tcargs p `zip` tcargs p') ++ uni) wps wps'
          | otherwise                   = --trace ("   (Not covered: " ++ prstr p ++ " in context " ++ prstrs (map fst (wps++wps')) ++ ")") $
                                          imp v eq uni ((w,p):wps) wps'
          where hits                    = [ (wf $ eVar w', vsubst s p') | (w',p0) <- wps++wps', w'/=w, Just (wf,p') <- [findAncestor env p0 (tcname p)] ]
                s                       = [(tvSelf,tUni v)]
        imp v eq uni wps []             = (reverse eq, uni)
  -- TODO: also check that an mro exists (?)


remove ws []                            = []
remove ws (Proto _ _ w t p : cs)
  | w `elem` ws                         = remove ws cs
remove ws (Imply i w q cs0 : cs)
  | null cs1                            = remove ws cs
  | otherwise                           = Imply i w q cs1 : remove ws cs
  where cs1                             = remove ws cs0
remove ws (c : cs)                      = c : remove ws cs

filterOut vs cs                         = filter preserve cs
  where preserve (Seal _ _ (TUni _ v))  = v `notElem` vs
        preserve _                      = True


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
