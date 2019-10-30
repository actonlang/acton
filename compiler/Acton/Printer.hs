{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, DeriveGeneric #-}
module Acton.Printer where

import Utils
import Pretty
import Acton.Syntax
import Acton.Builtin
-- import Prelude hiding ((<>))



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
    pretty (Def _ n q ps ks a b md) = prettyMod md $ text "def" <+> pretty n <+> nonEmpty brackets commaList q <+> parens (pretty (ps,ks)) <>
                                      nonEmpty (text " -> " <>) pretty a <> colon $+$ prettySuite b
    pretty (Actor _ n q ps ks a b)  = text "actor" <+> pretty n <+> nonEmpty brackets commaList q <+> parens (pretty (ps,ks)) <>
                                      nonEmpty (text " -> " <>) pretty a <> colon $+$ prettySuite b
    pretty (Class _ n q a b)        = text "class" <+> pretty n <+> nonEmpty brackets commaList q <+>
                                      nonEmpty parens commaList a <> colon $+$ prettySuite b
    pretty (Protocol _ n q a b)     = text "protocol" <+> pretty n <+> nonEmpty brackets commaList q <+>
                                      nonEmpty parens commaList a <> colon $+$ prettySuite b
    pretty (Extension _ n q a b)    = text "extension" <+> pretty n <+> nonEmpty brackets commaList q <+>
                                      nonEmpty parens commaList a <> colon $+$ prettySuite b
    pretty (Signature _ vs sc)      = prettySig vs sc

prettySig vs (TSchema _ [] t dec)   = pretty dec $+$ commaList vs <+> text ":" <+> pretty t
prettySig vs (TSchema _ q t dec)    = pretty dec $+$ commaList vs <+> text ":" <+> pretty q <+> text "=>" <+> pretty t

instance Pretty Decoration where
    pretty ClassAttr                = text "@classattr"
    pretty InstAttr                 = text "@instattr"
    pretty StaticMethod             = text "@staticmethod"
    pretty ClassMethod              = text "@classmethod"
    pretty InstMethod               = text "@instmethod"
    pretty NoDec                    = empty

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
    pretty (PosStar e)              = text "*" <> pretty e
    pretty PosNil                   = empty

instance Pretty KwdArg where
    pretty (KwdArg n e KwdNil)      = pretty n <+> equals <+> pretty e
    pretty (KwdArg n e k)           = pretty n <+> equals <+> pretty e <> comma <+> pretty k
    pretty (KwdStar e)              = text "**" <> pretty e
    pretty KwdNil                   = empty

instance Pretty (PosArg,KwdArg) where
    pretty (PosNil, ks)             = pretty ks
    pretty (ps, KwdNil)             = pretty ps
    pretty (ps, ks)                 = pretty ps <> comma <+> pretty ks

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
    pretty (Call _ e ps ks)         = pretty e <> parens (pretty (ps,ks))
    pretty (Await _ e)              = text "await" <+> pretty e
    pretty (Index _ e ix)           = pretty e <> brackets (commaList ix)
    pretty (Slice _ e sl)           = pretty e <> brackets (commaList sl)
    pretty (Cond _ e1 e e2)         = pretty e1 <+> text "if" <+> pretty e <+> text "else" <+> pretty e2
    pretty (BinOp _ e1 o e2)        = pretty e1 <+> pretty o <+> pretty e2
    pretty (CompOp _ e ops)         = pretty e <+> hsep (map pretty ops)
    pretty (UnOp _ o e)             = pretty o <> pretty e
    pretty (Dot _ e n)              = pretty e <> dot <> pretty n
    pretty (DotI _ e i)             = pretty e <> dot <> pretty i
    pretty (Lambda _ ps ks e)       = text "lambda" <+> pretty (ps,ks) <> colon <+> pretty e
    pretty (Yield _ e)              = text "yield" <+> pretty e
    pretty (YieldFrom _ e)          = text "yield" <+> text "from" <+> pretty e
    pretty (Tuple _ pargs)          = pretty pargs
    pretty (TupleComp _ e co)       = pretty e <+> pretty co
    pretty (Record _ KwdNil)        = text "record" <> parens empty
    pretty (Record _ kargs)         = parens (pretty kargs)
    pretty (RecordComp _ n e co)    = parens (pretty n <+> equals <+> pretty e <+> pretty co)
    pretty (List _ es)              = brackets (commaList es)
    pretty (ListComp _ e co)        = brackets (pretty e <+> pretty co)
    pretty (Dict _ es)              = braces (commaList es)
    pretty (DictComp _ e co)        = braces (pretty e <+> pretty co)
    pretty (Set _ [])               = text "set" <> parens empty
    pretty (Set _ es)               = braces (commaList es)
    pretty (SetComp _ e co)         = braces (pretty e <+> pretty co)
    pretty (Paren _ e)              = parens (pretty e)

instance Pretty OpArg where
    pretty (OpArg op e)             = pretty op <+> pretty e


prettyTuple []                      = text "()"
prettyTuple [e]                     = pretty e <> char ','
prettyTuple es                      = commaCat es

instance Pretty Name where
    pretty nm
      | isIdent str                 = text str
      | otherwise                   = quotes (text str)
      where str                     = nstr nm

instance Pretty ModName where
    pretty (ModName ns)             = dotCat pretty ns

instance Pretty QName where
    pretty (QName m n)              = pretty m <> dot <> pretty n
    pretty (NoQual n)               = pretty n

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

instance Pretty Slice where
    pretty (Sliz _ a b c)           = pretty a <> colon <> pretty b <> prettySlice c

prettySlice (Nothing)               = empty
prettySlice (Just (Nothing))        = colon
prettySlice (Just (Just b))         = colon <> pretty b

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

instance Pretty Pattern where
    pretty (PVar _ n a)             = pretty n <> prettyAnn a
    pretty (PTuple _ ps)            = pretty ps
--    pretty (PRecord _ ps)           = parens (pretty ps)
    pretty (PList _ ps p)           = brackets (prettyPats ps p)
    pretty (PIndex _ e ix)          = pretty e <> brackets (commaList ix)
    pretty (PSlice _ e sl)          = pretty e <> brackets (commaList sl)
    pretty (PDot _ e n)             = pretty e <> dot <> pretty n
    pretty (PParen _ p)             = parens (pretty p)
    pretty (PData _ n ixs)          = pretty n <> hcat (map (brackets . pretty) ixs)

prettyPats [] Nothing               = empty
prettyPats ps Nothing               = commaSep pretty ps
prettyPats [] (Just p)              = text "*" <> pretty p
prettyPats ps (Just p)              = commaSep pretty ps <> comma <+> text "*" <> pretty p

prettyMod (Sync True)               = (text "sync" <+>)
prettyMod (Sync False)              = id -- (text "(sync)" <+>)
prettyMod Async                     = (text "async" <+>)
prettyMod NoMod                     = id
prettyMod StaticMeth                = (text "@staticmethod" $+$)
prettyMod ClassMeth                 = (text "@classmethod" $+$)
prettyMod (InstMeth True)           = (text "@instmethod" $+$)
prettyMod (InstMeth False)          = id -- (text "(@instmethod)" $+$)


instance Pretty SrcInfoTag where
    pretty (GEN l t)                = text "GEN" <+> parens (pretty l) <> colon <+> pretty t
    pretty (INS l t)                = text "INS" <+> parens (pretty l) <> colon <+> pretty t


-------------------------------------------------------------------------------------------------

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
    pretty (TSchema _ [] t NoDec)   = pretty t
    pretty (TSchema _ q t NoDec)    = pretty q <+> text "=>" <+> pretty t

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

instance Pretty [TBind] where
    pretty q                        = brackets (commaList q)

instance Pretty TBind where
    pretty (TBind v [])             = pretty v
    pretty (TBind v cs)             = pretty v <> parens (commaList cs)
    
instance Pretty UType where
    pretty (UCon n)                 = pretty n
    pretty (ULit str)               = text ('\'' : str ++"'")

prettyFXRow (TRow _ n t r)
  | n == syncKW                     = text "sync" <+> prettyFXRow r
  | n == asyncKW                    = text "async" <+> prettyFXRow r
  | n == actKW                      = text "act" <+> prettyFXRow r
  | n == mutKW                      = text "mut" <+> prettyFXRow r
  | n == retKW                      = text "ret" <> parens (pretty t) <+> prettyFXRow r
prettyFXRow (TVar _ tv)             = pretty tv
prettyFXRow (TNil _)                = empty

prettyPosRow (TRow _ _ t (TNil _))  = pretty t
prettyPosRow (TRow _ _ t p)         = pretty t <> comma <+> prettyPosRow p
prettyPosRow (TVar _ v)             = text "*" <> pretty v
prettyPosRow (TWild _)              = text "*"
prettyPosRow (TNil _)               = empty
    
prettyKwdRow (TRow _ n t (TNil _))  = pretty n <> colon <+> pretty t
prettyKwdRow (TRow _ n t k)         = pretty n <> colon <+> pretty t <> comma <+> prettyKwdRow k
prettyKwdRow (TVar _ v)             = text "**" <> pretty v
prettyKwdRow (TWild _)              = text "**"
prettyKwdRow (TNil _)               = empty
    
prettyFunRow (TNil _) k             = prettyKwdRow k
prettyFunRow p (TNil _)             = prettyPosRow p
prettyFunRow p k                    = prettyPosRow p <> comma <+> prettyKwdRow k

instance Pretty Type where
    pretty (TVar _ v)               = pretty v
    pretty (TCon  _ c)              = pretty c
    pretty (TAt  _ c)               = text "@" <> pretty c
    pretty (TFun _ e p k t)         = prettyFXRow e <+> parens (prettyFunRow p k) <+> text "->" <+> pretty t
      where spaceSep f              = hsep . punctuate space . map f      
    pretty (TTuple _ pos)           = parens (prettyPosRow pos)
    pretty (TRecord _ kw)           = parens (prettyKwdRow kw)
    pretty (TOpt _ t)               = text "?" <> pretty t
    pretty (TUnion _ as)            = parens (vbarSep pretty as)
      where vbarSep f               = hsep . punctuate (space <> char '|') . map f
    pretty (TSelf _)                = text "Self"
    pretty (TNone _)                = text "None"
    pretty (TWild _)                = text "_"
    pretty row                      = prettyKwdRow row

instance Pretty Substitution where
    pretty s                        = vcat (map pr s)
      where pr (tv,t)               = pretty tv <+> text "->" <+> pretty t

