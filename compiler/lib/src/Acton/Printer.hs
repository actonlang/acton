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

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, DeriveGeneric #-}
module Acton.Printer (module Acton.Printer, module Pretty) where

import Utils
import Pretty
import Acton.Syntax
import Acton.Builtin
import Acton.Prim
import Prelude hiding ((<>))



instance Pretty Module where
    pretty (Module qn imps stmts)   = prHead qn $+$ vpretty imps $+$ blank $+$ vpretty stmts

prHead qn                           = empty
--prHead qn                           = text "module" <+> pretty qn <> colon $+$ blank

instance Pretty Import where
    pretty (Import _ ms)            = text "import" <+> commaSep pretty ms
    pretty (FromImport _ n ns)      = text "from" <+> pretty n <+> text "import" <+> commaSep pretty ns
    pretty (FromImportAll _ n)      = text "from" <+> pretty n <+> text "import" <+> text "*"

prettySuite ss                      = nest 4 $ vcat $ map pretty ss

-- Pretty print a suite with optional docstring at the beginning
prettyDocSuite :: Maybe String -> Suite -> Doc
prettyDocSuite Nothing ss           = prettySuite ss
prettyDocSuite (Just doc) ss        = nest 4 $ vcat $ text "\"\"\"" <> text (escapeDocstring doc) <> text "\"\"\"" : map pretty ss

-- Escape special characters in docstrings for pretty printing
escapeDocstring :: String -> String
escapeDocstring []                  = []
escapeDocstring ('\\':xs)           = '\\' : '\\' : escapeDocstring xs
escapeDocstring ('"':xs)            = '\\' : '"' : escapeDocstring xs
escapeDocstring (x:xs)              = x : escapeDocstring xs

instance Pretty Stmt where
    pretty (Expr _ e)               = pretty e
    pretty (Assign _ ps e)          = hsep . punctuate (space <> equals) $ map pretty ps ++ [pretty e]
    pretty (MutAssign _ t e)        = pretty t <+> equals <+> pretty e
    pretty (AugAssign _ t o e)      = pretty t <+> pretty o <+> pretty e
    pretty (Assert _ e mbe)         = text "assert" <+> pretty e <> nonEmpty (comma <+>) pretty mbe
    pretty (Pass _)                 = text "pass"
    pretty (Delete _ t)             = text "del" <+> pretty t
    pretty (Return _ e)             = text "return" <+> pretty e
    pretty (Raise _ e)              = text "raise" <+> pretty e
    pretty (Break _)                = text "break"
    pretty (Continue _)             = text "continue"
    pretty (If _ (b:bs) b2)         = prettyBranch "if" b $+$ vmap (prettyBranch "elif") bs $+$ prettyEnd "else" b2
    pretty (While _ e b b2)         = text "while" <+> pretty e <> colon $+$ prettySuite b $+$ prettyEnd "else" b2
    pretty (For _ p e b b2)         = text "for" <+> pretty p <+> text "in" <+> pretty e <> colon $+$
                                      prettySuite b $+$ prettyEnd "else" b2
    pretty (Try _ b hs b2 b3)       = text "try" <> colon $+$ prettySuite b $+$ vmap pretty hs $+$
                                      prettyEnd "else" b2 $+$ prettyEnd "finally" b3
    pretty (With _ items b)         = text "with" <+> commaSep pretty items <> colon $+$ prettySuite b
    pretty (Data _ (Just e) b)      = pretty e <> colon $+$ prettySuite b
    pretty (Data _ Nothing b)       = text "return" <> colon $+$ prettySuite b
    pretty (VarAssign _ ps e)       = text "var" <+> (hsep . punctuate (space <> equals) $ map pretty ps ++ [pretty e])
    pretty (After _ e e')           = text "after" <+> pretty e <> colon <+> pretty e'
    pretty (Decl _ [d])             = pretty d
    pretty (Decl _ ds)              = text "# recursive group:" $+$ (vcat $ map pretty ds) $+$ text "# (recursive group)"
    pretty (Signature _ vs sc d)    = prettyDec d $ commaList vs <+> colon <+> pretty sc

instance Pretty Decl where
    pretty (Def _ n q p k a b d x doc)
                                    = (prettyDecFX d x $ text "def" <+> pretty n <> nonEmpty brackets commaList q <+>
                                      parens (pretty (p,k)) <> nonEmpty (text " -> " <>) pretty a <> colon) $+$ prettyDocSuite doc b
    pretty (Actor _ n q p k b doc)  = text "actor" <+> pretty n <> nonEmpty brackets commaList q <+>
                                      parens (pretty (p,k)) <> colon $+$ prettyDocSuite doc b
    pretty (Class _ n q a b doc)    = text "class" <+> pretty n <> nonEmpty brackets commaList q <+>
                                      nonEmpty parens commaList a <> colon $+$ prettyDocSuite doc b
    pretty (Protocol _ n q a b doc) = text "protocol" <+> pretty n <> nonEmpty brackets commaList q <+>
                                      nonEmpty parens commaList a <> colon $+$ prettyDocSuite doc b
    pretty (Extension _ q c a b doc)
      | tvs == tcargs c             = text "extension" <+> pretty (tcname c) <> nonEmpty brackets commaList q <+>
                                      nonEmpty parens commaList a <> colon $+$ prettyDocSuite doc b
      | otherwise                   = text "extension" <+> prettyQual q <+> pretty c <+>
                                      nonEmpty parens commaList a <> colon $+$ prettyDocSuite doc b
      where tvs                     = map tVar $ qbound q

prettyDecFX d fx                    = prettyDec d . (prettyFXnoWild fx <+>)

prettyFXnoWild (TWild _)            = empty
prettyFXnoWild fx                   = pretty fx

prettyBranch kw (Branch e b)        = text kw <+> pretty e <> colon $+$ prettySuite b

prettyEnd kw []                     = empty
prettyEnd kw b                      = text kw <> colon $+$ prettySuite b

prettyOpt sep Nothing               = empty
prettyOpt sep (Just a)              = sep <+> pretty a


instance Pretty PosPar where
    pretty (PosPar n t e PosNIL)    = pretty n <+> prettyOpt colon t <+> prettyOpt equals e
    pretty (PosPar n t e p)         = pretty n <+> prettyOpt colon t <+> prettyOpt equals e <> comma <+> pretty p
    pretty (PosSTAR n t)            = text "*" <> pretty n <+> prettyOpt colon t
    pretty PosNIL                   = empty

instance Pretty KwdPar where
    pretty (KwdPar n t e KwdNIL)    = pretty n <+> prettyOpt colon t <+> prettyOpt equals e
    pretty (KwdPar n t e k)         = pretty n <+> prettyOpt colon t <+> prettyOpt equals e <> comma <+> pretty k
    pretty (KwdSTAR n t)            = text "**" <> pretty n <+> prettyOpt colon t
    pretty KwdNIL                   = empty

instance Pretty (PosPar,KwdPar) where
    pretty (PosNIL, ks)             = pretty ks
    pretty (ps, KwdNIL)             = pretty ps
    pretty (ps, ks)                 = pretty ps <> comma <+> pretty ks

instance Pretty PosArg where
    pretty (PosArg e PosNil)        = pretty e
    pretty (PosArg e p)             = pretty e <> comma <+> pretty p
    pretty (PosStar e)
      | atomic e                    = text "*" <> pretty e
      | otherwise                   = text "*" <> parens (pretty e)
    pretty PosNil                   = empty

instance Pretty KwdArg where
    pretty (KwdArg n e KwdNil)      = pretty n <+> equals <+> pretty e
    pretty (KwdArg n e k)           = pretty n <+> equals <+> pretty e <> comma <+> pretty k
    pretty (KwdStar e)
      | atomic e                    = text "**" <> pretty e
      | otherwise                   = text "**" <> parens (pretty e)
    pretty KwdNil                   = empty

instance Pretty (PosArg,KwdArg) where
    pretty (PosNil, ks)             = pretty ks
    pretty (ps, KwdNil)             = pretty ps
    pretty (ps, ks)                 = pretty ps  <> comma <+> pretty ks

atomic Await{}                      = False
atomic Cond{}                       = False
atomic BinOp{}                      = False
atomic CompOp{}                     = False
atomic UnOp{}                       = False
atomic Lambda{}                     = False
atomic Yield{}                      = False
atomic YieldFrom{}                  = False
atomic _                            = True

instance Pretty Expr where
    pretty (Var _ n)                = pretty n
    pretty (Int _ _ str)            = text str
    pretty (Float _ _ str)          = text str
    pretty (Imaginary _ _ str)      = text str
    pretty (Bool _ v)               = pretty v
    pretty (None _)                 = text "None"
    pretty (NotImplemented _)       = text "NotImplemented"
    pretty (Ellipsis _)             = text "..."
    pretty (Strings _ ss)           = hsep (map (pretty . show) ss)
    pretty (BStrings _ ss)          = hsep (map (\s -> text " b" <> pretty s) ss)
    pretty (Call _ e ps ks)
        | atomic e                  = pretty e <> parens (pretty (ps,ks))
        | otherwise                 = parens (pretty e) <> parens (pretty (ps,ks))
    pretty (TApp _ e ts)            = pretty e <> text "@" <> brackets (commaSep pretty ts)
    pretty (Async _ e)              = parens (text "async" <+> pretty e)
    pretty (Await _ e)              = text "await" <+> pretty e
    pretty (Index _ e ix)           = pretty e <> brackets (pretty ix)
    pretty (Slice _ e sl)           = pretty e <> brackets (pretty sl)
    pretty (IsInstance _ e c)       = text "isinstance" <> parens (pretty e <> comma <+> pretty c)
    pretty (Dot _ e n)              = pretty e <> dot <> pretty n
    pretty (Rest _ e n)             = pretty e <> dot <> text "~" <> pretty n
    pretty (DotI _ e i)             = pretty e <> dot <> pretty i
    pretty (RestI _ e i)            = pretty e <> dot <> text "~" <> pretty i
    pretty (Lambda _ ps ks e fx)    = prettyFXnoWild fx <+> text "lambda" <+> prettyLambdaPar ps ks <> colon <+> pretty e
    pretty (Yield _ e)              = text "yield" <+> pretty e
    pretty (YieldFrom _ e)          = text "yield" <+> text "from" <+> pretty e
    pretty (Tuple _ ps KwdNil)
        | singlePosArg ps           = parens (pretty ps <> comma)
    pretty (Tuple _ ps ks)          = parens (pretty (ps,ks))
    pretty (List _ es)              = brackets (commaList es)
    pretty (ListComp _ e co)        = brackets (pretty e <+> pretty co)
    pretty (Dict _ es)              = braces (commaList es)
    pretty (DictComp _ e co)        = braces (pretty e <+> pretty co)
    pretty (Set _ [])               = text "set" <> parens empty
    pretty (Set _ es)               = braces (commaList es)
    pretty (SetComp _ e co)         = braces (pretty e <+> pretty co)
    pretty (Paren _ e@Tuple{})      = pretty e
    pretty (Paren _ e)              = parens (pretty e)
    pretty (Box t e)                = parens (text "BOX" <+> pretty t <+> pretty e)
    pretty (UnBox t e)              = parens (text "UNBOX" <+> pretty t <+> pretty e)
    pretty e                        = prettyPrec 0 e  -- BinOp, CompOp, UnOp and Cond

{-
We assign precedences to operator expressions according to their main operator as follows.
The Python language reference does not assign numerical precedences, but the precedence order
implied by the syntax rules is consistent with the values below, with one exception:
Quote from section 6.5 in The Python Language Reference (v 3.8.6):
    "The power operator binds more tightly than unary operators on its left;
     it binds less tightly than unary operators on its right."
Printing here does not minimize the use of parentheses; unary operator expressions are
put in parenthesis (for clarity) in all operator contexts, also where the parser does not need them.

12 **
11 (unary) + - ~
10 * / // / @
 9 + -
 8 << >>
 7 &
 6 ^
 5 |
 4 < > <= >= == !=, is, is not, in, not in
 3 not
 2 and
 1 or
 0 _ if _ else _
-}

prettyPrec n e@(BinOp _ e1 op e2)   = parensIf (n > prc) ps
     where prc                      = fromJust (lookup op bps)
           bps                      = [(Or,1),(And,2),(Plus,9),(Minus,9),(Mult,10),(Pow,12),(Div,10),(Mod,10),
                                       (EuDiv,10),(BOr,5),(BXor,6),(BAnd,7),(ShiftL,8),(ShiftR,8),(MMult,10)]
           ps | op == Pow           = prettyPrec (prc+1) e1 <+> pretty op <+> prettyPrec prc e2
              | otherwise           = prettyPrec prc e1 <+> pretty op <+> prettyPrec (prc+1) e2
prettyPrec n (CompOp _ e ops)       = parensIf (n > 4) $ pretty e <+> hsep (map pretty ops)
prettyPrec n e1@(UnOp _ op e)       = parensIf (n > 0) $ pretty op <> prettyPrec prc e
   where prc                        = if op == Not then 3 else 11
prettyPrec n (Cond _ e1 e e2)       = parensIf (n > 1) $ pretty e1 <+> text "if" <+> pretty e <+> text "else" <+> pretty e2
prettyPrec _ e                      = pretty e

prettyLambdaPar ps ks
  | annotP ps || annotK ks          = parens (pretty (ps,ks))
  | otherwise                       = pretty (ps,ks)
  where annotP (PosPar _ mba _ p)   = mba /= Nothing || annotP p
        annotP (PosSTAR _ mba)      = mba /= Nothing
        annotP PosNIL               = False
        annotK (KwdPar _ mba _ k)   = mba /= Nothing || annotK k
        annotK (KwdSTAR _ mba)      = mba /= Nothing
        annotK KwdNIL               = False

instance Pretty OpArg where
    pretty (OpArg op e)             = pretty op <+> pretty e


prettyTuple []                      = text "()"
prettyTuple [e]                     = pretty e <> char ','
prettyTuple es                      = commaCat es

instance Pretty Name where
    pretty nm
      | nm == nSelf                 = text "Self"
      | isIdent str                 = text str
      | otherwise                   = quotes (text str)
      where str                     = nstr nm

instance Pretty ModName where
    pretty (ModName ns)             = dotCat pretty ns

instance Pretty QName where
--    pretty (QName m n)              = char '~' <> pretty m <> dot <> pretty n
    pretty (QName m n)              = pretty m <> dot <> pretty n
--    pretty (NoQ n)                  = char '~' <> pretty n
    pretty (NoQ n)                  = pretty n
    pretty (GName m n)
      | m == mPrim                  = text ("$" ++ rawstr n)
--      | m == mBuiltin               = text ("B_" ++ nstr n)
      | otherwise                   = pretty m <> dot <> pretty n

instance Pretty ModRef where
    pretty (ModRef (i,n))           = hcat (replicate i dot) <> pretty n

instance Pretty Handler where
    pretty (Handler ex b)           = pretty ex <> colon $+$ prettySuite b

instance Pretty Except where
    pretty (ExceptAll _)            = text "except"
    pretty (Except _ x)             = text "except" <+> pretty x
    pretty (ExceptAs _ x n)         = text "except" <+> pretty x <+> text "as" <+> pretty n

prettyAnn Nothing                   = empty
prettyAnn (Just a)                  = colon <+> pretty a

instance Pretty Elem where
    pretty (Elem e)                 = pretty e
    pretty (Star e)                 = text "*" <> pretty e

instance Pretty Assoc where
    pretty (Assoc k v)              = pretty k <> colon <+> pretty v
    pretty (StarStar e)             = text "**" <> pretty e

instance Pretty WithItem where
    pretty (WithItem e p)           = pretty e <+> nonEmpty (text "as" <+>) pretty p

instance Pretty ModuleItem where
    pretty (ModuleItem n1 n2)       = pretty n1 <+> nonEmpty (text "as" <+>) pretty n2

instance Pretty ImportItem where
    pretty (ImportItem n1 n2)       = pretty n1 <+> nonEmpty (text "as" <+>) pretty n2

instance Pretty Sliz where
    pretty (Sliz _ a b c)           = pretty a <> colon <> pretty b <> nonEmpty (colon <>) pretty c

instance Pretty Comp where
    pretty (CompFor _ p e c)        = text "for" <+> pretty p <+> text "in" <+> pretty e <+> pretty c
    pretty (CompIf _ e c)           = text "if" <+> pretty e <+> pretty c
    pretty NoComp                   = empty


instance Pretty PosPat where
    pretty (PosPat p PosPatNil)     = pretty p
    pretty (PosPat p ps)            = pretty p <> comma <+> pretty ps
    pretty (PosPatStar p)           = text "*" <> pretty p
    pretty PosPatNil                = empty

instance Pretty KwdPat where
    pretty (KwdPat n p KwdPatNil)   = pretty n <+> equals <+> pretty p
    pretty (KwdPat n p ps)          = pretty n <+> equals <+> pretty p <> comma <+> pretty ps
    pretty (KwdPatStar p)           = text "**" <> pretty p
    pretty KwdPatNil                = empty

instance Pretty (PosPat,KwdPat) where
    pretty (PosPatNil, ks)          = pretty ks
    pretty (ps, KwdPatNil)          = pretty ps
    pretty (ps, ks)                 = pretty ps <> comma <+> pretty ks

instance Pretty Pattern where
    pretty (PWild _ a)              = text "_" <> prettyAnn a
    pretty (PVar _ n a)             = pretty n <> prettyAnn a
    pretty (PTuple _ ps KwdPatNil)
      | singlePosPat ps             = pretty ps <> comma
    pretty (PTuple _ ps ks)         = pretty (ps, ks)
    pretty (PList _ ps p)           = brackets (prettyPats ps p)
    pretty (PParen _ p)             = parens (pretty p)
    pretty (PData _ n ixs)          = pretty n <> hcat (map (brackets . pretty) ixs)

prettyPats [] Nothing               = empty
prettyPats ps Nothing               = commaSep pretty ps
prettyPats [] (Just p)              = text "*" <> pretty p
prettyPats ps (Just p)              = commaSep pretty ps <> comma <+> text "*" <> pretty p

prettyDec d                         = (pretty d $+$)

instance Pretty Deco where
    pretty NoDec                    = empty
    pretty Static                   = text "@static"
    pretty Property                 = text "@property"

instance Pretty Unary where
    pretty Not                      = text "not "
    pretty BNot                     = text "~"
    pretty UMinus                   = text "-"
    pretty UPlus                    = text "+"

instance Pretty Binary where
    pretty Or                       = text "or"
    pretty And                      = text "and"
    pretty BOr                      = text "|"
    pretty BXor                     = text "^"
    pretty BAnd                     = text "&"
    pretty ShiftL                   = text "<<"
    pretty ShiftR                   = text ">>"
    pretty Plus                     = text "+"
    pretty Minus                    = text "-"
    pretty Mult                     = text "*"
    pretty MMult                    = text "@"
    pretty Div                      = text "/"
    pretty Mod                      = text "%"
    pretty EuDiv                    = text "//"
    pretty Pow                      = text "**"

instance Pretty Comparison where
    pretty Lt                       = text "<"
    pretty Gt                       = text ">"
    pretty Eq                       = text "=="
    pretty GE                       = text ">="
    pretty LE                       = text "<="
    pretty NEq                      = text "!="
    pretty In                       = text "in"
    pretty NotIn                    = text "not in"
    pretty Is                       = text "is"
    pretty IsNot                    = text "is not"


instance Pretty Aug where
    pretty PlusA                    = text "+="
    pretty MinusA                   = text "-="
    pretty MultA                    = text "*="
    pretty MMultA                   = text "@="
    pretty DivA                     = text "/="
    pretty ModA                     = text "%="
    pretty PowA                     = text "**="
    pretty BAndA                    = text "&="
    pretty BOrA                     = text "|="
    pretty BXorA                    = text "^="
    pretty ShiftLA                  = text "<<="
    pretty ShiftRA                  = text ">>="
    pretty EuDivA                   = text "//="

instance Pretty TSchema where
    pretty (TSchema _ q t)          = prettyQual q <+> pretty t

prettyQual []                       = empty
prettyQual q                        = pretty q <+> text "=>"

instance Pretty TVar where
    pretty (TV k n)                 = pretty n -- <> parens (colon <> pretty k)

instance Pretty TUni where
    pretty (UV k i)
      | i < 0                       = text "T_" <> pretty (-i) <> text "w"
      | otherwise                   = text "T_" <> pretty i <> prk k
      where prk PRow                = text "p"
            prk KRow                = text "k"
            prk KFX                 = text "x"
            prk _                   = empty

instance Pretty TCon where
    pretty (TC n [])                = pretty n
--    pretty (TC n [t])
--      | n == qnSequence             = brackets (pretty t)
--      | n == qnSetP                 = braces (pretty t)
--    pretty (TC n [kt,vt])
--      | n == qnMapping              = braces (pretty kt <> colon <+> pretty vt)
    pretty (TC n ts)                = pretty n <> brackets (commaList ts)

instance Pretty QBinds where
    pretty q                        = brackets (commaList q)

instance Pretty QBind where
    pretty (QBind v [])             = pretty v
    pretty (QBind v cs)             = pretty v <> parens (commaList cs)

prettyPosRow (TRow _ PRow _ t (TNil _ PRow))
                                    = pretty t
prettyPosRow (TRow _ PRow _ t p)    = pretty t <> comma <+> prettyPosRow p
prettyPosRow (TStar _ PRow r)
  | TVar _ v <- r                   = text "*" <> pretty v
  | TUni _ u <- r                   = text "*" <> pretty u
  | TWild _ <- r                    = text "*"
  | otherwise                       = text "*" <> parens (prettyPosRow r)   -- Print row as a tuple
prettyPosRow (TVar _ v)             = text "+" <> pretty v
prettyPosRow (TUni _ u)             = text "+" <> pretty u
prettyPosRow (TWild _)              = text "+"
prettyPosRow (TNil _ PRow)          = empty
prettyPosRow t                      = text "??" <>  pretty t

prettyKwdRow (TRow _ KRow n t (TNil _ KRow))
                                    = pretty n <> colon <+> pretty t
prettyKwdRow (TRow _ KRow n t k)    = pretty n <> colon <+> pretty t <> comma <+> prettyKwdRow k
prettyKwdRow (TStar _ KRow r)
  | TVar _ v <- r                   = text "**" <> pretty v
  | TUni _ u <- r                   = text "**" <> pretty u
  | TWild _ <- r                    = text "**"
  | otherwise                       = text "**" <> parens (prettyKwdRow r)  -- Print row as a tuple
prettyKwdRow (TVar _ v)             = text "++" <> pretty v
prettyKwdRow (TUni _ u)             = text "++" <> pretty u
prettyKwdRow (TWild _)              = text "++"
prettyKwdRow (TNil _ KRow)          = empty
prettyKwdRow t                      = text "??" <>  pretty t

prettyFunRow (TNil _ PRow) k        = prettyKwdRow k
prettyFunRow p (TNil _ KRow)        = prettyPosRow p
prettyFunRow p k                    = prettyPosRow p <> comma <+> prettyKwdRow k

instance Pretty Type where
    pretty (TVar _ v)               = pretty v
    pretty (TUni _ u)               = pretty u
    pretty (TCon  _ c)              = pretty c
    pretty (TFun _ fx p k t)        = prettyFXnoPure fx <> parens (prettyFunRow p k) <+> text "->" <+> pretty t
      where spaceSep f              = hsep . punctuate space . map f
    pretty (TTuple _ p k)
      | TVar{} <- p, TNil{} <- k    = pretty p                              -- Print top row variable as a tyvar
      | TNil{} <- p, TVar{} <- k    = pretty k                              -- Print top row variable as a tyvar
      | TUni{} <- p, TNil{} <- k    = pretty p                              -- Print top row variable as a univar
      | TNil{} <- p, TUni{} <- k    = pretty k                              -- Print top row variable as a univar
      | TRow _ _ _ t TNil{} <- p,
        TNil{} <- k                 = parens (pretty t <> comma)
      | otherwise                   = parens (prettyFunRow p k)
    pretty (TOpt _ t)               = text "?" <> pretty t
    pretty (TNone _)                = text "None"
    pretty (TWild _)                = text "_"
    pretty (TRow _ PRow _ t TNil{}) = parens $ pretty t <> comma
    pretty r@TRow{rkind=PRow}       = parens $ prettyPosRow r
    pretty r@TRow{rkind=KRow}       = parens $ prettyKwdRow r
    pretty r@TStar{rkind=PRow}      = parens $ prettyPosRow r
    pretty r@TStar{rkind=KRow}      = parens $ prettyKwdRow r
    pretty r@TNil{rkind=PRow}       = parens empty
    pretty r@TNil{rkind=KRow}       = parens empty
    pretty (TFX _ fx)               = pretty fx

prettyFXnoPure (TFX _ FXPure)       = empty
prettyFXnoPure t                    = pretty t

instance Pretty FX where
    pretty FXProc                   = text "proc"
    pretty FXMut                    = text "mut"
    pretty FXPure                   = text "pure"
    pretty FXAction                 = text "action"

instance Pretty Kind where
    pretty KType                    = text "type"
    pretty KProto                   = text "protocol"
    pretty KFX                      = text "effect"
    pretty PRow                     = text "positional row"
    pretty KRow                     = text "keyword row"
    pretty (KFun ks k)              = brackets (commaSep pretty ks) <+> text "=>" <+> pretty k
    pretty (KUni i)                 = text "K_" <> pretty i
    pretty KWild                    = text "_"

instance Pretty Constraint where
    pretty (Cast _ q t1 t2)         = prettyQuant q <+> pretty t1 <+> text "<" <+> pretty t2
    pretty (Sub _ w q t1 t2)        = pretty w <+> colon <+> prettyQuant q <+> pretty t1 <+> text "<" <+> pretty t2
    pretty (Proto _ w q t u)        = pretty w <+> colon <+> prettyQuant q <+> pretty t <+> parens (pretty u)
    pretty (Sel _ w q t1 n t2)      = pretty w <+> colon <+> prettyQuant q <+> pretty t1 <> text "." <> pretty n <+> text "<" <+> pretty t2
    pretty (Mut _ q t1 n t2)        = prettyQuant q <+> pretty t1 <+> text "." <> pretty n <+> text ">" <+> pretty t2
    pretty (Seal _ q t)             = prettyQuant q <+> text "$Seal" <+> pretty t
    pretty (Imply _ w q cs)
      | length cs < 4               = pretty w <+> colon <+> pretty q <+> text "=>" <+> braces (commaSep pretty cs)
      | otherwise                   = pretty w <+> colon <+> pretty q <+> text "=>" $+$ nest 4 (vcat $ map pretty cs)

prettyQuant []                      = empty
prettyQuant qs                      = brackets (commaSep pretty qs) <+> text "=>"

instance Pretty Quant where
    pretty (Quant tv wps)           = pretty tv <> parens (commaSep pretty wps)

instance Pretty (WPath,TCon) where
    pretty (wpath, p)               = pretty p


instance Pretty (TVar,TVar) where           -- CHANGE
--instance Pretty (TUni,TUni) where
    pretty (tv,tv')                 = pretty tv <+> text "~" <+> pretty tv'

instance Pretty (TVar,Type) where           -- CHANGE
--instance Pretty (TUni,Type) where
    pretty (tv,t)                   = pretty tv <+> text "~" <+> pretty t

--instance Pretty (TVar,TCon) where         -- CHANGE
--instance Pretty (TUni,TCon) where
--    pretty (tv,tc)                  = pretty tv <+> text "~" <+> pretty tc

instance Pretty (TVar,Either TCon Type) where   -- CHANGE
--instance Pretty (TUni,Either TCon Type) where
    pretty (tv, Left p)             = pretty tv <+> text "~" <+> text "protocol" <+> pretty p
    pretty (tv, Right t)            = pretty (tv,t)

instance Pretty (TVar,Name) where           -- CHANGE
--instance Pretty (TUni,Name) where
    pretty (tv,n)                   = pretty tv <> text "." <> pretty n

instance Pretty Substitution where
    pretty s                        = commaSep pretty s

instance Pretty (Name,Type) where
    pretty (n,t)                    = pretty n <> text ":" <+> pretty t
