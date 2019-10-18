{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, DeriveGeneric #-}
module Acton.Printer where

import Utils
import Pretty
import Acton.Syntax
import Acton.Builtin



instance Pretty Module where
    pretty (Module qn imps stmts)   = prHead qn $+$ vpretty imps $+$ blank $+$ vpretty stmts

prHead qn                           = empty
--prHead qn                           = text "module" <+> pretty qn <> colon $+$ blank

instance Pretty Import where
    pretty (Import _ ms)            = text "import" <+> commaSep pretty ms
    pretty (FromImport _ n ns)      = text "from" <+> pretty n <+> text "import" <+> commaSep pretty ns
    pretty (FromImportAll _ n)      = text "from" <+> pretty n <+> text "import" <+> text "*"

prettySuite ss                      = nest 4 $ vcat $ map pretty ss

instance Pretty Stmt where
    pretty (Expr _ e)               = pretty e
    pretty (Assign _ ps e)          = hsep . punctuate (space <> equals) $ map pretty ps ++ [pretty e]
    pretty (AugAssign _ p o e)      = pretty p <+> pretty o <+> pretty e
    pretty (Assert _ es)            = text "assert" <+> commaList es
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
    pretty (Decl _ ds)              = vcat $ map pretty ds

instance Pretty Decl where
    pretty (Def _ n q ps a b md)    = prettyMod md $ text "def" <+> pretty n <+> nonEmpty brackets commaList q <+> parens (pretty ps) <>
                                      nonEmpty (text " -> " <>) pretty a <> colon $+$ prettySuite b
    pretty (Actor _ n q ps a b)     = text "actor" <+> pretty n <+> nonEmpty brackets commaList q <+> parens (pretty ps) <>
                                      nonEmpty (text " -> " <>) pretty a <> colon $+$ prettySuite b
    pretty (Class _ n q a b)        = text "class" <+> pretty n <+> nonEmpty brackets commaList q <+>
                                      nonEmpty parens commaList a <> colon $+$ prettySuite b
    pretty (Protocol _ n q a b)     = text "protocol" <+> pretty n <+> nonEmpty brackets commaList q <+>
                                      nonEmpty parens commaList a <> colon $+$ prettySuite b
    pretty (Extension _ n q a b)    = text "extension" <+> pretty n <+> nonEmpty brackets commaList q <+>
                                      nonEmpty parens commaList a <> colon $+$ prettySuite b
    pretty (Signature _ vs t dec)   = pretty dec $+$ commaList vs <+> text ":" <+> pretty t

instance Pretty Decoration where
    pretty ClassAttr                = text "@classattr"
    pretty InstAttr                 = text "@instattr"
    pretty StaticMethod             = text "@staticmethod"
    pretty ClassMethod              = text "@classmethod"
    pretty InstMethod               = text "@instmethod"
    pretty NoDecoration             = empty

prettyBranch kw (Branch e b)        = text kw <+> pretty e <> colon $+$ prettySuite b

prettyEnd kw []                     = empty
prettyEnd kw b                      = text kw <> colon $+$ prettySuite b

prettyArgs []                       = text ")"
prettyArgs [kwd]                    = prettyArg kwd <> text ")"
prettyArgs (kwd:kwds)               = prettyArg kwd <> comma $$ prettyArgs kwds
    
prettyArg (KwArg kw (Call _ e es))
  | length kwds == 0                = f <> parens (commaList es)
  | length pos == 0                 = f <> text "(" $$ nest 4 (prettyArgs kwds)
  | otherwise                       = f <> text "(" <> commaList pos <> comma $$ nest 4 (prettyArgs kwds)
  where (pos,kwds)                  = partition isPosArg es
        f                           = pretty kw <+> equals <+> pretty e
prettyArg arg                       = pretty arg

instance Pretty Expr where
    pretty (Var _ n)                = pretty n
    pretty (Int _ _ str)            = text str
    pretty (Float _ _ str)          = text str
    pretty (Imaginary _ _ str)      = text str
    pretty (Bool _ v)               = pretty v
    pretty (None _)                 = text "None"
    pretty (NotImplemented _)       = text "NotImplemented"
    pretty (Ellipsis _)             = text "..."
    pretty (Strings _ ss)           = hcat (map pretty ss)
    pretty (BStrings _ ss)          = hcat (map pretty ss)
    pretty (UStrings _ ss)          = hcat (map pretty ss)
    pretty (Call _ e es)
      | length kwds < 2             = pretty e <> parens (commaList es)
      | length pos == 0             = pretty e <> text "(" $$ nest 4 (prettyArgs kwds)
      | otherwise                   = pretty e <> text "(" <> commaList pos <> comma $$ nest 4 (prettyArgs kwds)
      where (pos,kwds)              = partition isPosArg es
    pretty (Await _ e)              = text "await" <+> pretty e
    pretty (Ix _ e ix)              = pretty e <> brackets (commaList ix)
    pretty (Cond _ e1 e e2)         = pretty e1 <+> text "if" <+> pretty e <+> text "else" <+> pretty e2
    pretty (BinOp _ e1 o e2)        = pretty e1 <+> pretty o <+> pretty e2
    pretty (CompOp _ e ops)         = pretty e <+> hsep (map pretty ops)
    pretty (UnOp _ o e)             = pretty o <> pretty e
    pretty (Dot _ e n)              = pretty e <> dot <> pretty n
    pretty (DotI _ e i)             = pretty e <> dot <> pretty i
    pretty (Lambda _ ps e)          = text "lambda" <+> pretty ps <> colon <+> pretty e
    pretty (Tuple _ es)             = prettyTuple es
    pretty (Yield _ e)              = text "yield" <+> pretty e
    pretty (YieldFrom _ e)          = text "yield" <+> text "from" <+> pretty e
    pretty (Generator _ e co)       = pretty e <+> pretty co
    pretty (List _ es)              = brackets (commaList es)
    pretty (ListComp _ e co)        = brackets (pretty e <+> pretty co)
    pretty (Dict _ es)              = braces (commaList es)
    pretty (DictComp _ e co)        = braces (pretty e <+> pretty co)
    pretty (Set _ [])               = text "set" <> parens empty
    pretty (Set _ es)               = braces (commaList es)
    pretty (SetComp _ e co)         = braces (pretty e <+> pretty co)
    pretty (Record _ [])            = text "record" <> parens empty
    pretty (Record _ fs)            = parens (commaList fs)
    pretty (RecordComp _ n e co)    = parens (pretty n <+> equals <+> pretty e <+> pretty co)
    pretty (Paren _ e)              = parens (pretty e)

instance Pretty OpArg where
    pretty (OpArg op e)             = pretty op <+> pretty e


prettyTuple []                      = text "()"
prettyTuple [e]                     = pretty e <> char ','
prettyTuple es                      = commaCat es

instance Pretty Name where
    pretty (Name _ str)
      | isIdent str                 = text str
      | otherwise                   = quotes (text str)
    pretty n                        = text (show n)

instance Pretty QName where
    pretty (QName n ns)             = dotCat pretty (n:ns)

instance Pretty ModRef where
    pretty (ModRef (i,n))           = hcat (replicate i dot) <> pretty n
    
instance Pretty a => Pretty (Op a) where
    pretty (Op _ a)                 = pretty a

instance Pretty Exception where
    pretty (Exception e1 e2)        = pretty e1 <+> nonEmpty (text "from" <+>) pretty e2

instance Pretty Handler where
    pretty (Handler ex b)           = pretty ex <> colon $+$ prettySuite b
    
instance Pretty Except where
    pretty (ExceptAll _)            = text "except"
    pretty (Except _ x)             = text "except" <+> pretty x
    pretty (ExceptAs _ x n)         = text "except" <+> pretty x <+> text "as" <+> pretty n

instance Pretty Params where
    pretty (Params a b c d)         = hsep $ punctuate comma params
      where ps                      = map pretty a
            kw                      = map pretty c
            psE0                    = nonEmpty (text "*" <>) pretty b
            psE                     = if isEmpty psE0 && kw /= [] then text "*" else psE0
            kwE                     = nonEmpty (text "**" <>) pretty d
            params                  = filter (not . isEmpty) (ps ++ [psE] ++ kw ++ [kwE])

instance Pretty Param where
    pretty (Param n ann Nothing)    = pretty n <> prettyAnn ann
    pretty (Param n ann (Just e))   = pretty n <> prettyAnn ann <+> equals <+> pretty e

instance Pretty StarPar where
    pretty (StarPar _ n ann)        = pretty n <> prettyAnn ann
    pretty NoStar                   = empty

prettyAnn Nothing                   = empty
prettyAnn (Just a)                  = colon <+> pretty a

instance Pretty e => Pretty (Elem e) where
    pretty (Elem e)                 = pretty e
    pretty (Star e)                 = text "*" <> pretty e

instance Pretty Assoc where
    pretty (Assoc k v)              = pretty k <> colon <+> pretty v
    pretty (StarStarAssoc e)        = text "**" <> pretty e

instance Pretty Field where
    pretty (Field n e)              = pretty n <+> equals <+> pretty e
    pretty (StarStarField e)        = text "**" <> pretty e

instance Pretty WithItem where
    pretty (WithItem e p)           = pretty e <+> nonEmpty (text "as" <+>) pretty p

instance Pretty ModuleItem where
    pretty (ModuleItem n1 n2)       = pretty n1 <+> nonEmpty (text "as" <+>) pretty n2

instance Pretty ImportItem where
    pretty (ImportItem n1 n2)       = pretty n1 <+> nonEmpty (text "as" <+>) pretty n2

instance Pretty Arg where
    pretty (Arg e)                  = pretty e
    pretty (KwArg n e)              = pretty n <+> equals <+> pretty e
    pretty (StarArg e)              = text "*" <> pretty e
    pretty (StarStarArg e)          = text "**" <> pretty e

instance Pretty Index where
    pretty (Index _ e)              = pretty e
    pretty (Slice _ a b c)          = pretty a <> colon <> pretty b <> prettySlice c

prettySlice (Nothing)               = empty
prettySlice (Just (Nothing))        = colon
prettySlice (Just (Just b))         = colon <> pretty b

instance Pretty Comp where
    pretty (CompFor _ p e c)        = text "for" <+> pretty p <+> text "in" <+> pretty e <+> pretty c
    pretty (CompIf _ e c)           = text "if" <+> pretty e <+> pretty c
    pretty NoComp                   = empty


instance Pretty Pattern where
    pretty (PVar _ n a)             = pretty n <> prettyAnn a
    pretty (PTuple _ ps)            = prettyPEs ps
    pretty (PList _ ps)             = prettyPEs ps
    pretty (PIx _ e ix)             = pretty e <> brackets (commaList ix)
    pretty (PDot _ e n)             = pretty e <> dot <> pretty n
    pretty (PParen _ p)             = parens (pretty p)
    pretty (PData _ n ixs)          = pretty n <> hcat (map (brackets . pretty) ixs)

prettyPEs []                        = text "()"
prettyPEs [p]                       = pretty p <> char ','
prettyPEs ps                        = commaCat ps

prettyMod (Sync True)              = (text "sync" <+>)
prettyMod (Sync False)             = id -- (text "(sync)" <+>)
prettyMod Async                    = (text "async" <+>)
prettyMod NoMod                    = id
prettyMod StaticMeth               = (text "@staticmethod" $+$)
prettyMod ClassMeth                = (text "@classmethod" $+$)
prettyMod InstMeth                 = (text "@instmethod" $+$)


-------------------------------------------------------------------------------------------------

instance Pretty InfoTag where
    pretty (GEN l t)    = text "GEN" <+> parens (pretty l) <> colon <+> pretty t
    pretty (INS l t)    = text "INS" <+> parens (pretty l) <> colon <+> pretty t


instance Pretty OType where
    pretty (OVar tv)                = pretty tv
    
    pretty (OFun ONil row t)        = parens (prettyRow row) <> text "->" <> pretty t
    pretty (OFun fx row t)          = prettyEffect fx <> parens (prettyRow row) <> text "->" <> pretty t
    pretty (ORecord row)            = parens (prettyRow row)
    pretty (ODict k t)              = text "dict" <> parens (pretty k <> comma <> pretty t)
    pretty (OTuple row)             = parens $ prettyTupleRow row
    pretty (OList t)                = brackets $ pretty t
    pretty (OSet t)                 = braces $ pretty t
    pretty (OMsg t)                 = text "msg" <> parens (pretty t)
    pretty OStr                     = text "str"
    pretty OInt                     = text "int"
    pretty OFloat                   = text "float"
    pretty OBool                    = text "bool"
    pretty ONone                    = text "None"

    pretty (OSchema tvs [] t)       = pretty t
    pretty (OSchema tvs cs t)       = pretty t <+> text "\\\\" <+> commaCat cs

    pretty r | isRow r              = text "<" <> prettyRow r <> text ">"

prettyEffect (OKwd n _ fx)          = text (nstr n) <+> prettyEffect fx
prettyEffect ONil                   = empty
prettyEffect t                      = pretty t

prettyTupleRow r@(OPos t ONil)      = prettyRow r <> comma
prettyTupleRow r                    = prettyRow r

prettyRow (OPos t r)                = pretty t <> prettyTail r
prettyRow (OStar1 t r)              = text "*" <> pretty t <> prettyTail r
prettyRow (OKwd n t r)              = pretty n <> colon <+> pretty t <> prettyTail r
prettyRow (OStar2 t r)              = text "**" <> pretty t <> prettyTail r
prettyRow ONil                      = empty
prettyRow (OVar tv)                 = pretty tv
prettyRow t                         = text "?" <> pretty t

prettyTail ONil                     = empty
prettyTail (OVar tv)                = comma <> pretty tv
prettyTail row                      = comma <> prettyRow row


instance Pretty [Qonstraint] where
    pretty cs                       = vcat (map pretty cs)

instance Pretty Qonstraint where
    pretty (QEqu _ _ t1 t2)         = pretty t1 <+> equals <+> pretty t2
    pretty (QIn _ _ t1 t2)          = pretty t1 <+> text "in" <+> pretty t2
    pretty (QDot _ _ t1 n t2)       = pretty t1 <> dot <> pretty n <+> equals <+> pretty t2
    pretty (QIx _ _ t1 t2 t3)       = pretty t1 <> brackets (pretty t2) <+> equals <+> pretty t3
    pretty (QMod _ _ t1 t2)         = text "Mod" <> parens (pretty t1 <> comma <> pretty t2)
    pretty (QPlus _ _ t1)           = text "Plus" <> parens (pretty t1)
    pretty (QNum _ _ t1)            = text "Num" <> parens (pretty t1)
    pretty (QBool _ _ t1)           = text "Bool" <> parens (pretty t1)


instance Pretty (Name, OType) where
    pretty (n, t)                   = pretty n <> colon <+> pretty t

instance Pretty (OVar, OType) where
    pretty (tv, t)                  = pretty tv <+> equals <+> pretty t

instance Pretty OVar where
    pretty []                       = text "_"
    pretty [i]                      = text (stringSupply !! i)
      where stringSupply            = [ c:tl | tl <- "" : map show [1..], c <- "ZABCDEFGHIJKLMNOPQRSTUWXY"  ]
    pretty (1:nums)                 = text "V" <> hcat (punctuate (text "_") (map pretty nums))


---------------------------------------------------------------------------------------------------------------------------

instance Pretty OSubstitution where
    pretty s                        = vcat (map pr s)
      where pr (tv,t)               = pretty tv <+> equals <+> pretty t


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
    pretty (TSchema _ [] t)         = pretty t
    pretty (TSchema _ q t)
      | length vs0 == length q      = pretty t
      | otherwise                   = brackets (commaList q) <+> text "=>" <+> pretty t
      where vs0                     = [ v | TBind v [] <- q ]

instance Pretty TVar where
    pretty (TV n)                   = pretty n

instance Pretty TCon where
    pretty (TC n [])                = pretty n
    pretty (TC n [t])
      | n == qnSequence             = brackets (pretty t)
      | n == qnSet                  = braces (pretty t)
    pretty (TC n [kt,vt])
      | n == qnMapping              = braces (pretty kt <> colon <+> pretty vt)
    pretty (TC n ts)                = pretty n <> brackets (commaList ts)
    
instance Pretty TBind where
    pretty (TBind v [])             = pretty v
    pretty (TBind v cs)             = pretty v <> parens (commaList cs)
    
instance Pretty UType where
    pretty (UCon n)                 = pretty n
    pretty (ULit str)               = text ('\'' : str ++"'")

instance Pretty FXRow where
    pretty (FXsync r)               = text "sync" <+> pretty r
    pretty (FXasync r)              = text "async" <+> pretty r
    pretty (FXact r)                = text "act" <+> pretty r
    pretty (FXmut r)                = text "mut" <+> pretty r
    pretty (FXret t r)              = text "ret" <> parens (pretty t) <+> pretty r
    pretty (FXVar tv)               = pretty tv
    pretty FXNil                    = empty

instance Pretty PosRow where
    pretty (PosRow t PosNil)        = pretty t
    pretty (PosRow t p)             = pretty t <> comma <+> pretty p
    pretty (PosVar mbn)             = text "*" <> maybe empty pretty mbn
    pretty PosNil                   = empty
    
instance Pretty KwdRow where
    pretty (KwdRow n t KwdNil)      = pretty n <> colon <+> pretty t
    pretty (KwdRow n t k)           = pretty n <> colon <+> pretty t <> comma <+> pretty k
    pretty (KwdVar mbn)             = text "**" <> maybe empty pretty mbn
    pretty KwdNil                   = empty
    
instance Pretty (PosRow, KwdRow) where
    pretty (PosNil, k)              = pretty k
    pretty (p, KwdNil)              = pretty p
    pretty (p, k)                   = pretty p <> comma <+> pretty k

instance Pretty Type where
    pretty (TSelf _)                = text "Self"
    pretty (TVar _ v)               = pretty v
    pretty (TCon  _ c)              = pretty c
    pretty (TAt  _ c)               = text "@" <> pretty c
    pretty (TFun _ e p k t)         = pretty e <+> parens (pretty (p,k)) <+> text "->" <+> pretty t
      where spaceSep f              = hsep . punctuate space . map f      
    pretty (TTuple _ pos)           = parens (pretty pos)
    pretty (TRecord _ kw)           = parens (pretty kw)
    pretty (TOpt _ t)               = text "?" <> pretty t
    pretty (TUnion _ as)            = parens (vbarSep pretty as)
      where vbarSep f               = hsep . punctuate (space <> char '|') . map f
    pretty (TNone _)                = text "None"

instance Pretty Substitution where
    pretty s                        = vcat (map pr s)
      where pr (tv,t)               = pretty tv <+> text "->" <+> pretty t

