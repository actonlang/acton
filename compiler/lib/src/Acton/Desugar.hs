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

module Acton.Desugar (desugarOptDot) where

import Acton.Syntax
import Utils (SrcLoc)

-- | Desugar a single optional-chain access like @a?.b@ into a
--   null‑short‑circuiting lambda application:
--
--   1. Bind the base expression exactly once into a fresh temp.
--      This preserves evaluation order and avoids duplicating side effects.
--   2. Build a conditional that returns @tmp.b@ when @tmp is not None@,
--      otherwise returns @None@.
--   3. Wrap the conditional in a lambda and apply it to the base expression.
--
--   Conceptually:
--
--     a?.b
--       ==> (lambda tmp: tmp.b if tmp is not None else None)(a)
--
--   This keeps the rewrite local to the parser so later passes never see
--   an OptDot node, while still producing a normal expression tree that the
--   type checker can handle.
desugarOptDot :: SrcLoc -> Expr -> Name -> Expr
desugarOptDot l base attr =
  let tmp = Internal Tempvar "opt" 0
      tmpVar = Var l (NoQ tmp)
      noneExpr = None l
      testExpr = CompOp l tmpVar [OpArg IsNot noneExpr]
      thenExpr = Dot l tmpVar attr
      condExpr = Cond l thenExpr testExpr noneExpr
      lambdaExpr = Lambda l (PosPar tmp Nothing Nothing PosNIL) KwdNIL condExpr tWild
  in Call l lambdaExpr (PosArg base PosNil) KwdNil
