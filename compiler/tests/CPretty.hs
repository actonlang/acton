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
                                           vcat (map (structdecls . cpretty env . name) ["list","dict","set","str",
                                                "int","float","complex","bool","Iterator","Slice"] ++
                                                concat (concatMap (structs env) ss) ++
                                                map (cpretty env) ss
                                               )

structs env (Decl  _ ds)                = map strs ds
  where strs (Class _ nm qs bs ss) 
                                        = [structdecls cnm, structdecls (cnm <> text "$class")] ++
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
                                          else vcat (map (\n -> resultTuple env{isglobal=True} r <+> parens (text "*"<> cpretty env n)<>parens (cprettyPosRow env{isglobal=True} p) <>semi) ns)
   cpretty env (Signature _ ns tsc  _)  = vcat (map (\n ->cpretty env{isglobal=True} tsc<+> cpretty env n<>semi) ns)
   cpretty env stmt                     = empty 
   
instance CPretty Decl where
   cpretty env (Class _ nm qs bs ss)    = vcat (map ($+$ blank) [
                                              text "//" <+> cnm <+> text (replicate 60 '/'),
                                              witness_struct env{isglobal=False} cnm is,
                                              class_struct env{isglobal=False} nm ms,
                                              case nm of
                                                  Name{} -> opaque_struct cnm ms
                                                  Internal{} -> fun_prototypes env nm ms
                                           ])
     where (ms,is)                      = partition isMeth ss
           cnm                          = cpretty env nm
      
   cpretty env d                        = error ("cpretty: unexpected Decl: "++show d)

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
  cpretty env n@Internal{}              = text (nstr n)
  cpretty env n
      | nstr n=="_Complex"              = text "_Complx"
      | nstr n=="__complex__"           = text "__complx__"
      | isglobal env                    = text ('$' : nstr n)
      | otherwise                       = text (nstr n)
 

instance CPretty ModName where
    cpretty env (ModName ns)            = hsep $ punctuate (text "$$") (map (cpretty env) ns)
    
instance CPretty QName where
    cpretty env (QName mn n)            = cpretty env mn<>text "$$"<>cpretty env n
    cpretty env (NoQ n)                 = cpretty env n
    
instance CPretty Type where
    cpretty env (TVar _ v)              = text "$WORD"
    cpretty env (TCon  _ c)             = cpretty env c
    cpretty env (TFun _ e p _ t)        = cpretty env t <+> parens (cprettyPosRow env p)
    cpretty env (TTuple _ p _)          = parens (cprettyPosRow env p)
    cpretty env (TUnion _ as)           = parens (vbarSep pretty as)
      where vbarSep f                   = hsep . punctuate (space <> char '|') . map f
    cpretty env (TOpt _ t)              = cpretty env t
    cpretty env (TExist _ p)            = cpretty env p <> text "$opaque"
    cpretty env (TNone _)               = text "void"
    cpretty env (TWild _)               = text "_"
    cpretty env row                     = prettyKwdRow row

cprettyPosRow env (TRow _ _ _ t TNil{}) = cpretty env t
cprettyPosRow env (TRow _ _ _ t p)      = cpretty env t <> comma <+> cprettyPosRow env p
cprettyPosRow env TNil{}                = empty

structdecls cnm                         = text "struct" <+> cnm <> semi $+$
                                          text "typedef struct" <+> cnm <+> text "*" <> cnm <> semi $+$
                                          blank

witness_struct env cnm is               = text "struct" <+> cnm <+> text "{" $+$
                                          (nest 4 $ vcat ([--text "char *$GCINFO;",
                                                           cnm <> text "$class" <+>text"$class"<>semi] ++
                                                           [vcat (map (cpretty env) is)])) $+$
                                           text "};"

class_struct                            ::  CEnv -> Name -> [Stmt] -> Doc
class_struct env nm ms                  = text "struct" <+> cpretty env{isglobal=True} nm<>text "$class" <+> text "{" $+$
                                          (nest 4 $ text "char *$GCINFO;" $+$ vcat (map (cpretty env . addparSig nm) ms)) $+$
                                          text "};"
  where addparSig nm (Signature l ns (TSchema l2 qs (TFun l3 f p k r)) d)
                                        =  Signature l ns (TSchema l2 qs (TFun l3 f (addFstElem nm p)  k r)) d

addFstElem nm p                         = posRow (tCon (TC (NoQ nm) [])) p

opaque_struct  cnm ms                   = text "struct" <+> cnm<>text "$opaque" <+> text "{" $+$
                                          (nest 4 $ text "char *$GCINFO;" $+$
                                          vcat [cnm <+> text "proto"<>semi,
                                                text "$WORD impl"<>semi]) $+$
                                          text "};" $+$ blank $+$
                                          cnm<>text "$opaque" <+> cnm<>text "$pack"<>parens (cnm <+>text "proto, $WORD impl") <> semi $+$
                                          blank

fun_prototypes env nm ss                = vcat (map proto ss)
  where proto (Signature _ ns (TSchema _ _ (TFun _ f p _ r)) _)
                                        = vcat (map (\n -> resultTuple env r <+> cpretty env nm<>cpretty env n<+>parens (cprettyPosRow env (addFstElem nm p)) <>semi) ns)


resultTuple env (TTuple  _ r _)          = tup 0 r
   where tup n (TNil _ _)                = text ("$tup"++show n++"_t")
         tup n (TRow _ _ _ _ r)          = tup (n+1) r
         tup _ r                         = error ("cPrettyPosRow: unhandled tuple; row is "++render(pretty r))
resultTuple env t                        = cpretty env t
