{-# LANGUAGE FlexibleInstances #-}
module Tests.CPretty where

import Pretty
import Acton.Syntax
import Data.List
import Data.Maybe
import Acton.Printer
import Utils
import Prelude hiding ((<>))
import Acton.Env


cprettyEnv ps ss                           =  text "#pragma once" $+$ blank $+$
                                           text "#include \"common.h\"" $+$ blank $+$
                                           vcat (map (structdecls . cpretty ps . name) ["$list","$dict","$set","$str","$int","$float","$complx","$bool","Iterator","Slice"] ++
                                                concatMap (structs ps) ss ++
                                                map (cpretty ps) ss
                                               )

class CPretty a where
   cpretty :: [Name] -> a -> Doc

instance CPretty a => CPretty [a] where
   cpretty ps ss = vcat (map (cpretty ps) ss)
   

structs ps (nm,NClass qs bs ss)         = [structdecls cnm, structdecls (cnm <> text "$__class__")] ++
                                          case nm of
                                             Name{} -> if nm `elem` ps then [structdecls (cnm <> text "$opaque")] else []
                                             Internal{} -> []
           where cnm                    = cpretty ps nm
structs _ _                             = []

instance CPretty (Name,NameInfo) where
    cpretty ps (nm, NSig (TSchema _ _ (TFun _ f p _ r)) _) 
                                        = resultTuple ps r <+> parens (text "*"<> cpretty ps nm)<>parens (cprettyPosRow ps p) <>semi

    cpretty ps (nm, NSig tsc _)         = cpretty ps tsc<+> cpretty ps nm<>semi
                                           
    cpretty ps (nm, NClass qs bs ss)    = vcat (map ($+$ blank) [
                                              text "//" <+> cnm <+> text (replicate 60 '/'),
                                              witness_struct ps cnm is,
                                              class_struct ps nm ms,
                                              case nm of
                                                  Name{} -> if nm `elem` ps  then opaque_struct cnm ms else empty
                                                  Internal{} -> fun_prototypes ps nm ms
                                           ])
     where (ms,is)                      = partition isMeth ss
           cnm                          = cpretty ps nm
    cpretty ps (nm, NVar tsc)           = cpretty ps tsc<+> cpretty ps nm<>semi      
    cpretty _ d                         = error ("cpretty: unexpected NameInfo: "++show d)

substdollar []                          = []
substdollar ('_':cs)                    = '$':substdollar cs
substdollar (c:cs)                      = c:substdollar cs

isMeth (_,NSig tsc _)                   = isFunType (sctype tsc)
   where isFunType (TFun {})            = True
         isFunType _                    = False
isMeth s                                = False
 
instance CPretty TBind where
   cpretty  ps (TBind tv bs)        = vcat (map (\b -> cpretty ps b<>pretty tv) bs)

instance CPretty TCon where
   cpretty ps (TC qn _)             = cpretty ps qn

instance CPretty TSchema where
    cpretty ps (TSchema _ _ t)      = cpretty ps t

instance CPretty Name where
    cpretty _ (Name _ "int")          = text "$int"
    cpretty _ (Name _ "float")        = text "$float"
    cpretty _ (Name _ "complex")      = text "$complx"
--    cpretty _ (Name _ "__complex__")  = text "__complx__"
    cpretty _ (Name _ "Complex")      = text "Complx"
    cpretty _ (Name _ "_Complex")      = text "_Complx"
    cpretty _ (Name _ "bool")         = text "$bool"
    cpretty _ (Name _ "list")         = text "$list"
    cpretty _ (Name _ "dict")         = text "$dict"
    cpretty _ (Name _ "set")          = text "$set"
    cpretty _ (Name _ "str")          = text "$str"
    cpretty _ (Name _ ns)             = text ns
    cpretty _ (Internal str _ _)      = text (substdollar str)

instance CPretty QName where
    cpretty ps (QName _ n)             = cpretty ps n  -- ******************** !!!!
    cpretty ps (NoQual n)              = cpretty ps n
    
instance CPretty Type where
    cpretty _ (TVar _ v)            = text "$WORD"
    cpretty ps (TCon  _ c)          = cpretty ps c
    cpretty ps (TFun _ e p _ t)     = cpretty ps t <> text "(*)" <> parens (cprettyPosRow ps p)
    cpretty ps (TExist _ tc)        = cpretty ps tc<>text "$opaque"
    cpretty ps (TTuple _ p _)       = parens (cprettyPosRow ps p)
    cpretty ps (TUnion _ as)        = parens (vbarSep pretty as)
      where vbarSep f               = hsep . punctuate (space <> char '|') . map f
    cpretty ps (TOpt _ t)           = cpretty ps t
    cpretty _ (TNone _)             = text "None"
    cpretty _ (TWild _)             = text "_"
    cpretty _ row                   = prettyKwdRow row

cprettyPosRow ps (TRow _ _ _ t (TNil _ _)) = cpretty ps t
cprettyPosRow ps (TRow _ _ _ t p)    = cpretty ps t <> comma <+> cprettyPosRow ps p
cprettyPosRow _(TNil _ _)            = empty

structdecls cnm                         = text "struct" <+> cnm <> semi $+$
                                          text "typedef struct" <+> cnm <+> text "*" <> cnm <> semi $+$
                                          blank

witness_struct ps cnm is                = text "struct" <+> cnm <+> text "{" $+$
                                          (nest 4 $ vcat ([text "char *GCINFO;",
                                                           cnm <> text "$__class__" <+>text" __class__"<>semi] ++
                                                           [vcat (map (cpretty ps)  is)])) $+$
                                           text "};"

class_struct                            :: [Name] -> Name -> [(Name,NameInfo)] -> Doc
class_struct ps nm ms                   = text "struct" <+> cpretty ps nm<>text "$__class__" <+> text "{" $+$
                                          (nest 4 $ text "char *GCINFO;" $+$ vcat (map (cpretty ps . addparSig nm) ms)) $+$
                                          text "};"
  where addparSig nm (n,sig@(NSig (TSchema _ _ _) StaticMethod))
                                        = (n,sig)
        addparSig nm (n,NSig (TSchema l2 qs (TFun l3 f p k r)) d)
                                        =  (n,NSig (TSchema l2 qs (TFun l3 f (addFstElem nm p)  k r)) d)

addFstElem nm p                         = TRow NoLoc PRow (name "???") (monotype (tCon (TC (noQual (substdollar(nstr nm))) []))) p

opaque_struct  cnm ms                   = text "struct" <+> cnm<>text "$opaque" <+> text "{" $+$
                                          (nest 4 $ text "char *GCINFO;" $+$
                                          vcat [cnm <+> text "__proto__"<>semi,
                                                text "$WORD __impl__"<>semi]) $+$
                                          text "};" $+$ blank $+$
                                          cnm<>text "$opaque" <+> cnm<>text "$__pack__"<>parens (cnm <+>text "__proto__, $WORD __impl__") <> semi $+$
                                          blank

fun_prototypes ps nm ss                  = vcat (map proto ss)
  where  --proto (Signature _ ns (TSchema _ _ (TFun _ f p _ r)) StaticMethod)
         --                               = vcat (map (\n -> resultTuple r <+> cpretty nm<>text "$"<>cpretty n<+>parens (cprettyPosRow p) <>semi) ns)
         proto (ns,NSig (TSchema _ _ (TFun _ f p _ r)) _)
                                        = resultTuple ps r <+> cpretty ps nm<>text "$"<>cpretty ps ns<+>parens (cprettyPosRow ps (addFstElem nm p)) <>semi


resultTuple ps (TTuple _ r _)           = tup 0 r
   where tup n (TNil _ _)               = text ("$tup"++show n++"_t")
         tup n (TRow _ _ _ _ r)         = tup (n+1) r
         tup _ r                        = error ("cPrettyPosRow: unhandled tuple; row is "++render(pretty r))
resultTuple ps t                        = cpretty ps t
