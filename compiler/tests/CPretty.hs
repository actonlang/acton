module Tests.CPretty where

import Pretty
import Acton.Syntax
import Data.List
import Data.Maybe
import Acton.Printer
import Utils
import Prelude hiding ((<>))

cprint m@Module{} = cpretty (CEnv True) m

data CEnv = CEnv {isglobal :: Bool}

class CPretty a where
  cpretty :: CEnv -> a -> Doc

instance CPretty a => CPretty [a] where
   cpretty env ss                       = vcat (map (cpretty env) ss)
   
instance CPretty Module where
   cpretty env (Module  _ _ ss)         =  text "#pragma once" $+$ blank $+$
                                           text "#include \"common.h\"" $+$ blank $+$
                                           vcat (map (structdecls . cpretty env . name) ["$list","$dict","$set","$str",
                                                "$int","$float","$complex","$bool","Iterator","Slice"] ++
                                                concat (concatMap (structs env) ss) ++
                                                map (cpretty env) ss
                                               )

structs env (Decl  _ ds)                = map strs ds
  where strs (Class _ nm qs bs ss) 
                                        = [structdecls cnm, structdecls (cnm <> text "$__class__")] ++
                                          case nm of
                                             Name{} -> [structdecls (cnm <> text "$opaque")]
                                             Internal{} -> []
           where cnm                    = cpretty env nm
        strs _                          = []
structs _ _                             = []

                     
instance CPretty Stmt where
   cpretty env (Decl _ ds)              = vcat (map (cpretty env) ds)
   cpretty env (Signature _ ns (TSchema _ _ (TFun _ f p _ r)) _)
                                        = if (isglobal env)
                                          then vcat (map (\n -> resultTuple env r <+> cpretty env n<>parens (cprettyPosRow env p) <>semi) ns)
                                          else vcat (map (\n -> resultTuple env r <+> parens (text "*"<> cpretty env n)<>parens (cprettyPosRow env p) <>semi) ns)
   cpretty env (Signature _ ns tsc  _)  = vcat (map (\n ->cpretty env tsc<+> cpretty env n<>semi) ns)
   cpretty env stmt                     = empty --error ("cpretty; unexpected Stmt: "++ show stmt)
   
instance CPretty Decl where
   cpretty env (Class _ nm qs bs ss)    = vcat (map ($+$ blank) [
                                              text "//" <+> cnm <+> text (replicate 60 '/'),
                                              witness_struct env cnm is,
                                              class_struct env{isglobal=False} nm ms,
                                              case nm of
                                                  Name{} -> opaque_struct cnm ms
                                                  Internal{} -> fun_prototypes env nm ms
                                           ])
     where (ms,is)                      = partition isMeth ss
           cnm                          = cpretty env nm
      
   cpretty env d                        = error ("cpretty: unexpected Decl: "++show d)

substdollar []                          = []
substdollar ('_':cs)                    = '$':substdollar cs
substdollar (c:cs)                      = c:substdollar cs

isMeth (Signature _ _ sc _)             = isFunType (sctype sc)
   where isFunType (TFun {})            = True
         isFunType _                    = False
isMeth _                                = False

 
instance CPretty TBind where
   cpretty env  (TBind tv bs) = vcat (map (\b -> cpretty env b<>pretty tv) bs)

instance CPretty TCon where
   cpretty env (TC qn _)                = cpretty env qn

instance CPretty TSchema where
    cpretty env (TSchema _ _ t)         = cpretty env t

instance CPretty Name where
    cpretty env (Name _ "int")          = text "$int"
    cpretty env (Name _ "float")        = text "$float"
    cpretty env (Name _ "complex")      = text "$complex"
    cpretty env (Name _ "bool")         = text "$bool"
    cpretty env (Name _ "list")         = text "$list"
    cpretty env (Name _ "dict")         = text "$dict"
    cpretty env (Name _ "set")          = text "$set"
    cpretty env (Name _ "str")          = text "$str"
    cpretty env (Name _ ns)             = text ns
    cpretty env (Internal str _ _)      = text (substdollar str)

instance CPretty QName where
    cpretty env (QName _ n)             = error "cpretty for qualified name" 
    cpretty env (NoQual n)              = cpretty env n
    
instance CPretty Type where
    cpretty env (TVar _ v)              = text "$WORD"
    cpretty env (TCon  _ c)             = cpretty env c
    cpretty env (TFun _ e p _ t)        = cpretty env t <+> parens (cprettyPosRow env p)
    cpretty env (TTuple _ p _)          = parens (cprettyPosRow env p)
    cpretty env (TUnion _ as)           = parens (vbarSep pretty as)
      where vbarSep f                   = hsep . punctuate (space <> char '|') . map f
    cpretty env (TOpt _ t)              = cpretty env t
    cpretty env (TExist _ p)            = cpretty env p <> text "$opaque"
    cpretty env (TNone _)               = text "None"
    cpretty env (TWild _)               = text "_"
    cpretty env row                     = prettyKwdRow row

cprettyPosRow env (TRow _ _ _ t (TNil _ _))
                                        = cpretty env t
cprettyPosRow env (TRow _ _ _ t p)      = cpretty env t <> comma <+> cprettyPosRow env p
cprettyPosRow env (TNil _ _)            = empty

structdecls cnm                         = text "struct" <+> cnm <> semi $+$
                                          text "typedef struct" <+> cnm <+> text "*" <> cnm <> semi $+$
                                          blank

witness_struct env cnm is               = text "struct" <+> cnm <+> text "{" $+$
                                          (nest 4 $ vcat ([text "char *GCINFO;",
                                                           cnm <> text "$__class__" <+>text" __class__"<>semi] ++
                                                           [vcat (map (cpretty env) is)])) $+$
                                           text "};"

class_struct env nm ms                  = text "struct" <+> cpretty env nm<>text "$__class__" <+> text "{" $+$
                                          (nest 4 $ text "char *GCINFO;" $+$ vcat (map (cpretty env . addparSig nm) ms)) $+$
                                          text "};"
  where  addparSig nm (Signature l ns (TSchema l2 qs (TFun l3 f p k r)) d)
                                        =  Signature l ns (TSchema l2 qs (TFun l3 f (addFstElem nm p)  k r)) d

addFstElem nm p                         = TRow NoLoc KType (name "???") (monotype (tCon (TC (noQual (substdollar(nstr nm))) []))) p

opaque_struct  cnm ms                   = text "struct" <+> cnm<>text "$opaque" <+> text "{" $+$
                                          (nest 4 $ text "char *GCINFO;" $+$
                                          vcat [cnm <+> text "__proto__"<>semi,
                                                text "$WORD __impl__"<>semi]) $+$
                                          text "};" $+$ blank $+$
                                          cnm<>text "$opaque" <+> cnm<>text "$__pack__"<>parens (cnm <+>text "__proto__, $WORD __impl__") <> semi $+$
                                          blank

fun_prototypes env nm ss                = vcat (map proto ss)
  where proto (Signature _ ns (TSchema _ _ (TFun _ f p _ r)) _)
                                        = vcat (map (\n -> resultTuple env r <+> cpretty env nm<>text "$"<>cpretty env n<+>parens (cprettyPosRow env (addFstElem nm p)) <>semi) ns)


resultTuple env (TTuple  _ r _)          = tup 0 r
   where tup n (TNil _ _)                = text ("$tup"++show n++"_t")
         tup n (TRow _ _ _ (TSchema _ _ _) r)
                                        = tup (n+1) r
         tup _ r                        = error ("cPrettyPosRow: unhandled tuple; row is "++render(pretty r))
resultTuple env t                       = cpretty env t
