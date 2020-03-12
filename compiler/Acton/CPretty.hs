module Acton.CPretty where

import Pretty
import Acton.Syntax
import Data.List
import Data.Maybe
import Acton.Printer
import Utils
import Prelude hiding ((<>))

class CPretty a where
  cpretty :: a -> Doc

instance CPretty Module where
   cpretty (Module  _ _ ss)             =  text "#pragma once" $+$ blank $+$
                                           text "#include \"common.h\"" $+$ blank $+$
                                           vcat (map (structdecls . cpretty . name) ["$list","$dict","$set","$str","$int","$float","$complx","$bool"] ++
                                                concat (concatMap structs ss) ++
                                                map cpretty ss
                                               )

structs (Decl  _ ds)                    = map strs ds
  where strs (Class _ nm qs bs ss) 
                                        = [structdecls cnm, structdecls (cnm <> text "$__class__")] ++
                                          case nm of
                                             Name{} -> [structdecls (cnm <> text "$opaque")]
                                             Internal{} -> []
           where cnm                    = cpretty nm
                     
instance CPretty Stmt where
    cpretty (Decl _ ds)                 = vcat (map cpretty ds)
    cpretty (Signature _ ns (TSchema _ _ (TFun _ f p _ r) _))
                                        = vcat (map (\n -> resultTuple r <+> parens (text "*"<> cpretty n)<>parens (cprettyPosRow p) <>semi) ns)
    cpretty (Signature _ ns tsc)        = vcat (map (\n ->cpretty tsc<+> cpretty n<>semi) ns)
                                          
instance CPretty Decl where
   cpretty (Class _ nm qs bs ss)
                                        = vcat (map ($+$ blank) [
                                              text "//" <+> cnm <+> text (replicate 60 '/'),
                                              witness_struct cnm is,
                                              class_struct nm ms,
                                              case nm of
                                                  Name{} -> opaque_struct cnm ms
                                                  Internal{} -> fun_prototypes nm ms
                                           ])
     where (ms,is)                      = partition isMeth ss
           cnm                          = cpretty nm
      
   cpretty d                            = error ("cpretty: unexpected Decl: "++show d)

substdollar []                          = []
substdollar ('_':cs)                    = '$':substdollar cs
substdollar (c:cs)                      = c:substdollar cs

--isMeth (Decl _ (s:_))                   = isFunType (sctype (dtyp s))
isMeth s@Signature{}                    = isFunType (sctype (typ s))
   where isFunType (TFun {})            = True
         isFunType _                    = False
isMeth s                                = False
 
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
    cpretty (Name _ "complx")       = text "$complx"
    cpretty (Name _ "bool")         = text "$bool"
    cpretty (Name _ "list")         = text "$list"
    cpretty (Name _ "dict")         = text "$dict"
    cpretty (Name _ "set")          = text "$set"
    cpretty (Name _ "str")          = text "$str"
    cpretty (Name _ ns)             = text ns
    cpretty (Internal str _ _)      = text (substdollar str)

instance CPretty QName where
    cpretty (QName _ n)             = error "cpretty for qualified name" 
    cpretty (NoQual n)              = cpretty n
    
instance CPretty Type where
    cpretty (TVar _ v)              = text "$WORD"
    cpretty (TCon  _ c)             = cpretty c
    cpretty (TFun _ e p _ t)        =  parens (cprettyPosRow p) <+> text "->" <+> pretty t
      where spaceSep f              = hsep . punctuate space . map f      
    cpretty (TTuple _ p _)          = parens (cprettyPosRow p)
    cpretty (TUnion _ as)           = parens (vbarSep pretty as)
      where vbarSep f               = hsep . punctuate (space <> char '|') . map f
    cpretty (TOpt _ t)              = text "?" <> pretty t
    cpretty (TNone _)               = text "None"
    cpretty (TWild _)               = text "_"
    cpretty row                     = prettyKwdRow row

cprettyPosRow (TRow _ _ _ t (TNil _ _)) = cpretty t
cprettyPosRow (TRow _ _ _ t p)      = cpretty t <> comma <+> cprettyPosRow p
cprettyPosRow (TNil _ _)            = empty

structdecls cnm                         = text "struct" <+> cnm <> semi $+$
                                          text "typedef struct" <+> cnm <+> text "*" <> cnm <> semi $+$
                                          blank

witness_struct cnm is                   = text "struct" <+> cnm <+> text "{" $+$
                                          (nest 4 $ vcat ([text "char *GCINFO;",
                                                           cnm <> text "$__class__" <+>text" __class__"<>semi] ++
                                                           [vcat (map cpretty  is)])) $+$
                                           text "};"

class_struct nm ms                      = text "struct" <+> cpretty nm<>text "$__class__" <+> text "{" $+$
                                          (nest 4 $ text "char *GCINFO;" $+$ vcat (map (cpretty . addparSig nm) ms)) $+$
                                          text "};"
  where addparSig nm sig@(Signature _ _ (TSchema _ _ _ StaticMethod))
                                        = sig
        addparSig nm (Signature l ns (TSchema l2 qs (TFun l3 f p k r) d))
                                        =  Signature l ns (TSchema l2 qs (TFun l3 f (addFstElem nm p)  k r) d)

addFstElem nm p                         = TRow NoLoc PRow (name "???") (monotype (tCon (TC (noQual (substdollar(nstr nm))) []))) p

opaque_struct  cnm ms                   = text "struct" <+> cnm<>text "$opaque" <+> text "{" $+$
                                          (nest 4 $ text "char *GCINFO;" $+$
                                          vcat [cnm <+> text "__proto__"<>semi,
                                                text "$WORD __impl__"<>semi]) $+$
                                          text "};" $+$ blank $+$
                                          cnm<>text "$opaque" <+> cnm<>text "$__pack__"<>parens (cnm <+>text "__proto__, $WORD __impl__") <> semi $+$
                                          blank

fun_prototypes nm ss                    = vcat (map proto ss)
  where  --proto (Signature _ ns (TSchema _ _ (TFun _ f p _ r) StaticMethod))
         --                               = vcat (map (\n -> resultTuple r <+> cpretty nm<>text "$"<>cpretty n<+>parens (cprettyPosRow p) <>semi) ns)
         proto (Signature _ ns (TSchema _ _ (TFun _ f p _ r) _))
                                        = vcat (map (\n -> resultTuple r <+> cpretty nm<>text "$"<>cpretty n<+>parens (cprettyPosRow (addFstElem nm p)) <>semi) ns)


resultTuple (TTuple _ r _)              = tup 0 r
   where tup n (TNil _ _)               = text ("$tup"++show n++"_t")
         tup n (TRow _ _ _ _ r)         = tup (n+1) r
         tup _ r                        = error ("cPrettyPosRow: unhandled tuple; row is "++render(pretty r))
resultTuple t                           = cpretty t
