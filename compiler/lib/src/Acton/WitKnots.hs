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
module Acton.WitKnots(tieWitKnots) where

import Control.Monad
import Data.List (nub, intersect,intersperse, isPrefixOf, partition)
import Data.Maybe (isJust,mapMaybe)
import Pretty
import Debug.Trace
import Utils
import Acton.Syntax
import Acton.Names
import Acton.Builtin
import Acton.Prim
import Acton.Env
import Acton.Subst
import Acton.Unify
import Acton.Transform
import Acton.TypeM
import Acton.TypeEnv
import Prelude hiding ((<>))


tieWitKnots te (Decl l ds : ss) = return (te', Decl l ds' : ss)
  where (te',ds')               = tieDeclKnots te ds
tieWitKnots te (With _ [] b : ss)
                                = tieWitKnots te (b ++ ss)
tieWitKnots te (s : ss)         = do (te', ss') <- tieWitKnots te ss
                                     return (te', s:ss')
tieWitKnots te []               = return (te, [])


tieDeclKnots te ds
  | null ws                     = (te, ds)
  | null wdeps                  = (te, ds)
  | null cycles                 = (te, ds)
  | null cycledeps              = (te, ds)
  | not $ null insts            = error ("INTERNAL: #### Witness cycles with instantiations, not yet handled:\n" ++ 
                                         render (nest 4 $ vcat $ map pretty insts))
  | otherwise                   = do --traceM ("#### Local witness deps:\n" ++ render (nest 4 $ vcat $ map pretty wdeps))
                                     --traceM ("#### Chains:\n" ++ render (nest 4 $ vcat $ map pretty wchains))
                                     --traceM ("#### Cycles:\n" ++ render (nest 4 $ vcat $ map pretty cycles))
                                     --traceM ("#### Cycle dependencies:\n" ++ render (nest 4 $ vcat $ map pretty cycledeps ))
                                     let ds' = map (tieknots cycledeps) ds
                                         te' = map (addopts cycledeps) te
                                     (te', ds')
  where ws                      = [ n | (n,NExt{}) <- te ]
        wexts                   = [ d | d <- ds, dname d `elem` ws ]
        wdeps                   = map (witDeps ws) wexts
        wchains                 = [ ch | dep <- wdeps, ch <- witChains wdeps (absToApp dep) ]
        (cycles, insts)         = collect [] [] wchains
        cycledeps               = analyze ws cycles





data WAbs                       = WAbs [TVar] [Name] [WApp] deriving Eq
data WApp                       = WApp Name [Type] [Expr] deriving Eq
type WChain                     = [WApp]
type Cut                        = (Name, [Name])

instance Pretty (Name, WAbs) where
    pretty (w, WAbs tvs ps as)  = pretty w <> pretty_q <> parens (commaSep pretty ps) <+> text "-->" <+> commaSep pretty as
      where pretty_q            = if null tvs then empty else text "@" <> brackets (commaSep pretty tvs)

instance Pretty WApp where
    pretty (WApp n ts es)       = pretty n <> pretty_ts <> parens (commaSep pretty es)
      where pretty_ts           = if null ts then empty else text "@" <> brackets (commaSep pretty ts)

instance Pretty WChain where
    pretty chain                = pr chain
      where pr [wa]             = pretty wa
            pr (wa : was)       = pretty wa <+> text " - - > " <+> pr was

instance Pretty Cut where
    pretty (hd, deps)           = pretty hd  <+> text "<--" <+> commaSep pretty deps


initMeth d                      = head [ m | Decl _ ms <- dbody d, m <- ms, dname m == initKW ]

witDeps ws d                    = (w, WAbs tvs ps as)
  where w                       = dname d
        as                      = nub $ [ wapp | Assign _ [PVar _ _ (Just t)] e <- dbody d, Just wapp <- [witCall ws t e] ]
        ps                      = tail $ bound $ pos $ initMeth d
        tvs                     = qbound $ qbinds d

witCall ws t e@Call{}
  | Var _ (NoQ n) <- fun e,
    n `elem` ws                 = Just (WApp n [] $ arglist (pargs e))
  | TApp _ f tvs <- fun e,
    Var _ (NoQ n) <- f,
    n `elem` ws                 = Just (WApp n tvs $ arglist (pargs e))
  where arglist (PosArg e p)    = e : arglist p
        arglist _               = []
witCall ws t _                  = Nothing

absToApp (w, (WAbs tvs ps _))   = WApp w (map tVar tvs) (map eVar ps)
 
witChains deps a@(WApp w ts es)
  | null deps0                  = [[a]]
  | otherwise                   = [ a : as' | a' <- as, as' <- witChains deps1 (apply a') ]
  where (deps0, deps1)          = partition ((==w) . fst) deps
        WAbs vs ps as           = snd $ head deps0
        s0                      = vs `zip` ts
        s1                      = ps `zip` es
        apply (WApp n ts es)    = WApp n (vsubst s0 ts) (termsubst s1 es)

collect cycles insts []         = (reverse cycles, reverse insts)
collect cycles insts (chain : chains)
  | length chain <= 1           = collect cycles insts chains
  | a_0 == a_n                  = collect (chain:cycles) insts chains         -- New unique cycle
  | appname a_0 == appname a_n  = collect cycles (chain:insts) chains         -- Instantiated cycle
  | otherwise                   = collect cycles insts chains
  where a_0                     = head chain
        a_n                     = last chain

appname (WApp n _ _)            = n

analyze ws chains               = [ (n, chainOrigins n) | n <- ws ]
  where bare                    = map (map appname) chains
        chainLinks n            = nub [ ap | hd:tl <- bare, hd == n, ap <- init tl ]    -- Intuitive...
        chainOrigins n          = nub [ hd | hd:tl <- bare, hd /= n, n `elem` tl ]      -- Works better as long as we're ignoring cyclic instantiations


-- Now, for each declaration dec in ds:
--      Let me = dname(dec)
--      Let mydeps = depsof(me)
--      For all d in mydeps:
--          append W_d : ?d[Xs] to the parameters of __init__, where Xs are dec's generics
--      For each top level assignment v : t = d@[ts](es) in dec:
--          If d is me and me is in cyclic:
--              Replace rhs with  W_self
--          Else:
--              For each x in depsof(d), append to the rhs arguments:
--                  W_self  if x is me
--                  W_x     if x is in mydeps
--                  None    otherwise
--              if d is in mydeps:
--                  Prefix the rhs with  W_d if W_d is not None else ...

depsof w cycledeps              = case lookup w cycledeps of
                                    Just deps -> deps
                                    _ -> []


addopts cycledeps (n, NExt q c us te _ doc)
                                = trace ("#### Extending " ++ prstr n ++ " with opts " ++ prstrs opts) $ 
                                  (n, NExt q c us te opts doc)
  where opts                    = depsof n cycledeps
addopts cycledeps ni            = ni


tieknots cycledeps dec          = dec{ dbody = map tie (dbody dec) }
  where
    me                          = dname dec
    mydeps                      = depsof me cycledeps
    generics                    = qbound (qbinds dec)
    cyclic                      = (`elem` (dom cycledeps))

    tie (Decl l ds)             = Decl l (map tieI ds)
    tie s@Assign{}              = s{ expr = tieE (expr s) }
    tie s                       = s

    tieI d@Def{}
      | dname d == initKW       = d{ pos = ap (pos d) }
      where ap (PosPar n t e p) = PosPar n t e (ap p)
            ap (PosNIL)         = foldr depParam PosNIL mydeps
    tieI d                      = d

    tieE e@Call{fun=Var _ (NoQ n)}
                                = tieCall n e
    tieE e@Call{fun=TApp _ (Var _ (NoQ n)) _}
                                = tieCall n e
    tieE e                      = e
    
    tieCall n e@Call{}
      | n == me && cyclic me    = eVar selfKW'
      | n `elem` mydeps         = depKnot n e'
      | otherwise               = e'
      where e'                  = e{ pargs = ap (pargs e)}
            ap (PosArg e p)     = PosArg e (ap p)
            ap (PosNil)         = foldr extra PosNil (depsof n cycledeps)
            extra x
              | x == me         = PosArg (eVar selfKW')
              | x `elem` mydeps = PosArg (eVar $ depVar x)
              | otherwise       = PosArg eNone
    tieCall n e                 = e

    depVar n                    = witAttr (NoQ n)

    depType n                   = tCon (TC (NoQ n) (map tVar generics))

    depParam n                  = PosPar n' (Just $ tOpt t) Nothing
      where t                   = depType n
            n'                  = depVar n

    depKnot n                   = eCond (eCAST (tOpt t) t $ eVar n') (eCall (tApp (eQVar primISNOTNONE) [t]) [eVar n'])
      where t                   = depType n
            n'                  = depVar n