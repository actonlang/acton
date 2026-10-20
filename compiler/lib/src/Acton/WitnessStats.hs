-- Copyright (C) 2026 Acton contributors
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


{-# LANGUAGE OverloadedStrings #-}
-- Witness diagnostics are opt-in: none of this traversal runs during normal builds.
module Acton.WitnessStats (witnessStats) where

import Data.Aeson (Value, object, (.=))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import Acton.WitnessKey (expressionHash)
import Acton.Syntax hiding (expr, comp)
import Acton.Names (isWitness)
import Acton.Env (Env0, lookupName, lookupModuleInfo, moduleLookupName, thismod)
import Acton.NameInfo (NameInfo(..))
import Acton.Prim (tupleWits)
import Acton.CodeGen (staticWitnessName)
import Utils (prstr)

-- Counts describe syntax, not execution frequency. Defaults in type/parameter
-- metadata are deliberately excluded: only their expansions at call sites run.
-- Keep the typed environment even when inspecting later, class-converted trees,
-- so imported and local extension constructors retain their identity.
witnessStats :: Env0 -> String -> Module -> Value
witnessStats env stage m = object
    [ "kind" .= ("witness-stats" :: String), "version" .= (1 :: Int)
    , "module" .= prstr (modname m), "stage" .= stage
    , "bindings" .= length bs
    , "alias_bindings" .= length [() | (_, e) <- bs, alias e]
    , "lambda_bindings" .= length [() | (_, Lambda{}) <- bs]
    , "static_bindings" .= length [() | (_, e) <- bs, static e]
    , "constructor_expressions" .= length calls
    , "nonstatic_constructor_expressions" .= length dynamic
    , "nonstatic_constructor_expressions_in_loops" .= length [() | (d,_) <- dynamic, d > 0]
    , "distinct_constructor_expressions" .= uniqueCount [e | (_,(_,e)) <- calls]
    , "constructors" .= objectCounts calls
    , "nonstatic_constructors" .= M.fromListWith (+) [(prstr n, 1 :: Int) | (_,n) <- dynamic]
    ]
  where
    (bs, es) = suite 0 (mbody m)
    -- Witness RHS recipes also identify primitive/generated witness classes
    -- that are not NExts in the original environment.
    known = S.fromList ([n | (_,e) <- bs, Just n <- [recipeHead e]] ++ map snd tupleWits)
    recipeHead (Dot _ e _) = recipeHead e
    recipeHead (Paren _ e) = recipeHead e
    recipeHead (Call _ f _ _) = headName f
    recipeHead _ = Nothing
    lookupQ (NoQ n) = lookupName n env
    lookupQ (GName mn n) | Just mn == thismod env = lookupName n env
    lookupQ q = lookupModuleInfo (mname q) env >>= (\mi -> moduleLookupName mi (noq q))
    witness n = S.member n known || case lookupQ n of
        Just NExt{} -> True
        _ -> False
    headName (TApp _ e _) = headName e
    headName (Var _ n) = Just n
    headName (Paren _ e) = headName e
    headName _ = Nothing
    calls = [(d, (n,e)) | (d,e@Call{}) <- es, Just n <- [headName (fun e)], witness n]
    dynamic = [(d, n) | (d,(n,e)) <- calls, not (static e)]
    static e = case staticWitnessName e of (Just _,_) -> True; _ -> False
    alias Var{} = True
    alias (TApp _ e _) = alias e
    alias (Dot _ e _) = alias e
    alias (Paren _ e) = alias e
    alias _ = False
    objectCounts xs = M.fromListWith (+) [(prstr n, 1 :: Int) | (_, (n,_)) <- xs]

-- A small explicit walk keeps declaration metadata and evaluated expressions
-- separate. A declaration starts a new execution context for loop counts.
type Sites = ([(Maybe Type, Expr)], [(Int, Expr)])

suite :: Int -> Suite -> Sites
suite d = foldMap (stmt d)

stmt :: Int -> Stmt -> Sites
stmt d s = case s of
    Assign _ ps e -> ([(t,e) | PVar _ n t <- ps, isWitness n], []) <> foldMap (patternSites d) ps <> expr d e
    Expr _ e -> expr d e
    MutAssign _ a b -> expr d a <> expr d b
    AugAssign _ a _ b -> expr d a <> expr d b
    Assert _ e me -> expr d e <> foldMap (expr d) me
    Delete _ e -> expr d e
    Return _ me -> foldMap (expr d) me
    Raise _ e -> expr d e
    If _ bs els -> foldMap (\(Branch e b) -> expr d e <> suite d b) bs <> suite d els
    While _ e b els -> expr (d+1) e <> suite (d+1) b <> suite d els
    For _ p e b els -> expr d e <> patternSites (d+1) p <> suite (d+1) b <> suite d els
    Try _ b hs els fin -> suite d b <> foldMap (\(Handler _ b) -> suite d b) hs <> suite d els <> suite d fin
    With _ is b -> foldMap (\(WithItem e p) -> expr d e <> foldMap (patternSites d) p) is <> suite d b
    Data _ p b -> foldMap (patternSites d) p <> suite d b
    VarAssign _ ps e -> foldMap (patternSites d) ps <> expr d e
    After _ _ a b -> expr d a <> expr d b
    Decl _ ds -> foldMap decl ds
    _ -> mempty
  where
    decl Typedef{} = mempty
    decl x = suite 0 (dbody x)

expr :: Int -> Expr -> Sites
expr d e = ([], [(d,e)]) <> case e of
    Call _ f p k -> expr d f <> pos p <> kwd k
    Let _ b x -> suite d b <> expr d x
    TApp _ x _ -> expr d x
    Async _ x -> expr d x
    Await _ x -> expr d x
    Index _ x y -> expr d x <> expr d y
    Slice _ x (Sliz _ a b c) -> expr d x <> foldMap (foldMap (expr d)) [a,b,c]
    Cond _ a b c -> foldMap (expr d) [a,b,c]
    IsInstance _ x _ -> expr d x
    BinOp _ x _ y -> expr d x <> expr d y
    CompOp _ x ops -> expr d x <> foldMap (\(OpArg _ y) -> expr d y) ops
    UnOp _ _ x -> expr d x
    Dot _ x _ -> expr d x
    Rest _ x _ -> expr d x
    DotI _ x _ -> expr d x
    RestI _ x _ -> expr d x
    Opt _ x _ -> expr d x
    OptChain _ x -> expr d x
    Lambda _ _ _ x _ -> expr 0 x
    Yield _ x -> foldMap (expr d) x
    YieldFrom _ x -> expr d x
    Tuple _ p k -> pos p <> kwd k
    List _ xs -> foldMap elem xs
    ListComp _ x c -> elemAt (d + compDepth c) x <> compAt d c
    Dict _ xs -> foldMap assoc xs
    DictComp _ x c -> assocAt (d + compDepth c) x <> compAt d c
    Set _ xs -> foldMap elem xs
    SetComp _ x c -> elemAt (d + compDepth c) x <> compAt d c
    GeneratorExpr _ x c -> elemAt (d + compDepth c) x <> compAt d c
    Paren _ x -> expr d x
    Box _ x -> expr d x
    UnBox _ x -> expr d x
    _ -> mempty
  where
    pos (PosArg x p) = expr d x <> pos p
    pos (PosStar x) = expr d x
    pos PosNil = mempty
    kwd (KwdArg _ x k) = expr d x <> kwd k
    kwd (KwdStar x) = expr d x
    kwd KwdNil = mempty
    elem = elemAt d
    elemAt depth (Elem x) = expr depth x
    elemAt depth (Star x) = expr depth x
    assoc = assocAt d
    assocAt depth (Assoc x y) = expr depth x <> expr depth y
    assocAt depth (StarStar x) = expr depth x
    compAt depth (CompFor _ p x c) = expr depth x <> patternSites (depth+1) p <> compAt (depth+1) c
    compAt depth (CompIf _ x c) = expr depth x <> compAt depth c
    compAt _ NoComp = mempty
    compDepth (CompFor _ _ _ c) = 1 + compDepth c
    compDepth (CompIf _ _ c) = compDepth c
    compDepth NoComp = 0

-- Fingerprints keep the diagnostic linear on the usual constructor recipes;
-- exact equality protects the distinct count against hash collisions.
uniqueCount :: [Expr] -> Int
uniqueCount es = sum (map length (IM.elems (foldl' add IM.empty es)))
  where
    add buckets e = IM.alter (Just . insert e . maybe [] id) (expressionHash e) buckets
    insert e xs | e `elem` xs = xs
                | otherwise = e:xs

patternSites :: Int -> Pattern -> Sites
patternSites d p = case p of
    PData _ _ es -> foldMap (expr d) es
    PParen _ p -> patternSites d p
    PTuple _ p k -> pos p <> kwd k
    PList _ ps tail -> foldMap (patternSites d) ps <> foldMap (patternSites d) tail
    _ -> mempty
  where
    pos (PosPat p ps) = patternSites d p <> pos ps
    pos (PosPatStar p) = patternSites d p
    pos PosPatNil = mempty
    kwd (KwdPat _ p ps) = patternSites d p <> kwd ps
    kwd (KwdPatStar p) = patternSites d p
    kwd KwdPatNil = mempty
