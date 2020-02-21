module Acton.CPretty where

import Pretty
import Acton.Syntax
import Data.List
import Data.Maybe
import Acton.Printer

class CPretty a where
  cpretty :: a -> Doc

instance CPretty Module where
   cpretty (Module  _ _ ss)             =  text "#include \"common.h\"" $+$ blank $+$
                                           vcat (map (structdecls . cpretty . name) ["$list","$dict","$set","$tup1_t","$tup2_t","$tup3_t"] ++
                                                concat (concatMap structs ss) ++
                                                map cpretty ss
                                               )


structs (Decl  _ ds)                    = map strs ds
  where strs (Class _ nm@(Name _ _) qs bs ss) 
                                        = [structdecls cnm, structdecls (cnm <> text "$__class__"), structdecls (cnm <> text "$opaque")]
           where cnm                    = cpretty nm
        strs (Class _ nm qs bs ss)      -- Internal class names are for classes formed from extensions
                                        = [structdecls cnm, structdecls (cnm <> text "$__class__")]
           where cnm                    = cpretty nm
                    
instance CPretty Stmt where
  cpretty (Decl _ ds)                   = vcat (map cpretty ds)
                                          
instance CPretty Decl where
   cpretty (Class _ nm@(Name _ ns) qs bs ss)
                                        = vcat (map ($+$ blank) [
                                              text "//" <+> text ns <+> text (replicate (76 - length ns) '/'),
                                              witness_struct cnm is,
                                              class_struct cnm ms,
                                              opaque_struct cnm ms
                                           ])
     where (ms,is)                      = partition isMeth ss
           cnm                          = cpretty nm
   cpretty (Class _ nm qs bs ss)        = vcat (map ($+$ blank) [
                                              text "//" <+> cnm <+> text " implementation //////////////////////////////",
                                              class_struct cnm ss,
                                              witness_struct cnm is
                                           ])
     where (ms,is)                      = partition isMeth ss
           cnm                          = cpretty nm
     
   cpretty (Signature _ ns (TSchema _ _ (TFun _ f p _ r) _))
                                        = vcat (map (\n -> resultTuple r <+> parens (text "*"<> cpretty n)<>parens (cprettyPosRow p) <>semi) ns)
   cpretty (Signature _ ns tsc)         =  vcat (map (\n ->cpretty tsc<+> cpretty n<>semi) ns)
   cpretty d                            = error ("cpretty: unexpected Decl: "++show d)

substdollar []                          = []
substdollar ('_':cs)                    = '$':substdollar cs
substdollar (c:cs)                      = c:substdollar cs

isMeth (Decl _ (s:_))                   = isFunType (sctype (dtyp s))
   where isFunType (TFun {})            = True
         isFunType _                    = False
 
instance CPretty TBind where
   cpretty  (TBind tv bs) = vcat (map (\b -> cpretty b<>pretty tv) bs)

instance CPretty TCon where
   cpretty (TC (NoQual (Name _ "EXISTS")) [_,TCon _ (TC qn _)])
                                    = cpretty qn<>text "$opaque"
   cpretty (TC qn _)                = cpretty qn

instance CPretty TSchema where
    cpretty (TSchema _ _ t _)       = cpretty t

instance CPretty Name where
    cpretty (Name _ "int")          = text "$int"
    cpretty (Name _ "float")        = text "$float"
    cpretty (Name _ "bool")         = text "$bool"
    cpretty (Name _ "int")          = text "$int"
    cpretty (Name _ "list")         = text "$list"
    cpretty (Name _ "dict")         = text "$dict"
    cpretty (Name _ "set")          = text "$set"
    cpretty (Name _ str)            = text str
    cpretty (Name _ str)            = text str
    cpretty (Name _ str)            = text str
    cpretty (Name _ str)            = text str
    cpretty (Internal str _ _)      = text (substdollar str)

instance CPretty QName where
    cpretty (QName _ n)             = error "cpretty for qualified name" 
    cpretty (NoQual n)              = cpretty n
    
instance CPretty Type where
    cpretty (TVar _ v)              = text "$WORD"
    cpretty (TCon  _ c)             = cpretty c
 --   pretty (TAt  _ c)               = text "@" <> pretty c
    cpretty (TFun _ e p _ t)        =  parens (cprettyPosRow p) <+> text "->" <+> pretty t
      where spaceSep f              = hsep . punctuate space . map f      
    cpretty (TTuple _ pos)          = parens (cprettyPosRow pos)
    cpretty (TRecord _ kw)          = parens (cprettyPosRow kw)
    cpretty (TUnion _ as)           = parens (vbarSep pretty as)
      where vbarSep f               = hsep . punctuate (space <> char '|') . map f
    cpretty (TOpt _ t)              = text "?" <> pretty t
    cpretty (TNone _)               = text "None"
    cpretty (TWild _)               = text "_"
    cpretty row                     = prettyKwdRow row

{-
prettyFXRow (TRow _ n t r)
  | n == rAct                       = text "act" <+> prettyFXRow r
  | n == rMut                       = text "mut" <> brackets (pretty t) <+> prettyFXRow r
  | n == rRet                       = text "ret" <> brackets (pretty t) <+> prettyFXRow r
prettyFXRow (TVar _ tv)             = pretty tv
prettyFXRow (TWild _)               = text "_"
prettyFXRow (TNil _)                = empty
-}
cprettyPosRow (TRow _ _ t (TNil _)) = cpretty t
cprettyPosRow (TRow _ _ t p)        = cpretty t <> comma <+> cprettyPosRow p
--cprettyPosRow (TVar _ v)             = text "*" <> pretty v
--cprettyPosRow (TWild _)              = text "*"
cprettyPosRow (TNil _)              = empty
    
--prettyKwdRow (TRow _ n t (TNil _))  = pretty n <> colon <+> pretty t
--prettyKwdRow (TRow _ n t k)         = pretty n <> colon <+> pretty t <> comma <+> prettyKwdRow k
--prettyKwdRow (TVar _ v)             = text "**" <> pretty v
--prettyKwdRow (TWild _)              = text "**"
--prettyKwdRow (TNil _)               = empty
    
--prettyFunRow (TNil _) k             = prettyKwdRow k
--prettyFunRow p (TNil _)             = prettyPosRow p
--prettyFunRow p k                    = prettyPosRow p <> comma <+> prettyKwdRow k


structdecls nm                          = text "struct" <+> nm <> semi $+$
                                          text "typedef struct" <+> nm <+> text "*" <> nm <> semi $+$
                                          blank

witness_struct nm is                    = text "struct" <+> nm <+> text "{" $+$
                                          (nest 4 $ vcat ([text "char *GCINFO;",
                                                           nm <> text "$__class__" <+>text" __class__"<>semi] ++
                                                           [vcat (map cpretty  is)])) $+$
                                           text "};"

class_struct  nm ms                     = text "struct" <+> nm<>text "$__class__" <+> text "{" $+$
                                          (nest 4 $ text "char *GCINFO;" $+$ vcat (map cpretty ms)) $+$
                                          text "};"

opaque_struct  nm ms                     = text "struct" <+> nm<>text "$opaque" <+> text "{" $+$
                                          (nest 4 $ text "char *GCINFO;" $+$
                                          vcat [nm<>text "$__class__" <+> text "__proto__"<>semi,
                                                text "$WORD __impl__"<>semi]) $+$
                                          text "};" $+$ blank $+$
                                          nm<>text "$opaque" <+> nm<>text "$__pack__"<>parens (nm <+>text "__proto__, $WORD __impl__") <> semi $+$
                                          blank
                                          

resultTuple (TTuple _ r)                = tup 0 r
   where tup n (TNil _)                 = text ("$tup"++show n++"_t")
         tup n (TRow _ _ (TSchema _ _ (TVar{}) _) r)
                                        = tup (n+1) r
         tup _ r                        = error ("cPrettyPosRow: unhandled tuple; row is "++render(pretty r))
resultTuple t                           = cpretty t
