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


-- Structural fingerprints for the local witness index. A fingerprint only
-- selects a bucket: callers must still compare the complete Type and Expr.
module Acton.WitnessKey (witnessHash, expressionHash) where

import Data.Hashable (hash)
import Data.Maybe (isJust)
import Acton.Syntax

witnessHash :: Type -> Expr -> Int
witnessHash t e = hash (typeHash t, expressionHash e)

-- Follow Syntax's equality, rather than Show/pretty-printing: locations are
-- ignored, numeric lexemes matter, and defaults contribute only their presence.
-- Unsupported expression forms share a bucket. That preserves the old exact
-- comparison as a conservative fallback without imposing a new Eq instance on
-- the AST (Box/UnBox, for example, deliberately have no structural equality).
expressionHash :: Expr -> Int
expressionHash e = case e of
    Var _ n -> hash (0 :: Int, n)
    Int _ _ s -> hash (1 :: Int, s)
    Float _ _ s -> hash (2 :: Int, s)
    Imaginary _ _ s -> hash (3 :: Int, s)
    Bool _ b -> hash (4 :: Int, b)
    None{} -> 5
    NotImplemented{} -> 6
    Ellipsis{} -> 7
    Strings _ s -> hash (8 :: Int, s)
    BStrings _ s -> hash (9 :: Int, s)
    Call _ f p k -> node 10 [expressionHash f, pos p, kwd k]
    TApp _ f ts -> node 11 (expressionHash f : map typeHash ts)
    Dot _ x n -> node 12 [expressionHash x, hash n]
    DotI _ x i -> node 13 [expressionHash x, hash i]
    Cond _ a b c -> node 14 (map expressionHash [a,b,c])
    Tuple _ p k -> node 15 [pos p, kwd k]
    Paren _ x -> expressionHash x
    _ -> -1
  where
    pos (PosArg x p) = node 0 [expressionHash x, pos p]
    pos (PosStar x) = node 1 [expressionHash x]
    pos PosNil = 2
    kwd (KwdArg n x k) = node 0 [hash n, expressionHash x, kwd k]
    kwd (KwdStar x) = node 1 [expressionHash x]
    kwd KwdNil = 2

node :: Int -> [Int] -> Int
node tag xs = hash (tag, xs)

typeHash :: Type -> Int
typeHash t = case t of
    TUni _ u -> node 0 [uvid u]
    TVar _ (TV _ n) -> node 1 [hash n]
    TCon _ (TC n ts) -> node 2 (hash n : map typeHash ts)
    TFun _ fx p k r -> node 3 (map typeHash [fx,p,k,r])
    TTuple _ p k -> node 4 (map typeHash [p,k])
    TOpt _ x -> node 5 [typeHash x]
    TNone{} -> 6
    TWild{} -> 7
    TNil _ k -> node 8 [hash (show k)]
    TRow _ k n x d r -> node 9 [hash (show k), hash n, typeHash x, hash (isJust d), typeHash r]
    TStar _ k r -> node 10 [hash (show k), typeHash r]
    TFX _ fx -> node 11 [hash (show fx)]
    TUnboxed _ x -> node 12 [typeHash x]
