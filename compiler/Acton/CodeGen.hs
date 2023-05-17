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
module Acton.CodeGen where

import qualified Data.Set
import qualified Data.List
import qualified Acton.Env
import Utils
import Pretty
import Acton.Syntax
import Acton.Names
import Acton.Builtin
import Acton.Printer
import Acton.Prim
import Acton.Env
import Acton.QuickType
import Acton.Subst
import Prelude hiding ((<>))
import System.FilePath.Posix
import Numeric

generate                            :: Acton.Env.Env0 -> FilePath -> Module -> IO (String,String,String)
generate env srcbase m              = do return (n, h,c)
  where n                           = concat (Data.List.intersperse "." (modPath (modname m))) --render $ quotes $ gen env0 (modname m)
        h                           = render $ hModule env0 m
        c                           = render $ cModule env0 srcbase m
        env0                        = genEnv $ setMod (modname m) env 

genRoot                            :: Acton.Env.Env0 -> QName -> IO String
genRoot env0 qn@(GName m n)         = do return $ render (cInclude $+$ cInit $+$ cRoot)
  where env                         = genEnv $ setMod m env0
        cInclude                    = include env "types" m
        cInit                       = (text "void" <+> gen env primROOTINIT <+> parens empty <+> char '{') $+$
                                       nest 4 (gen env (GName m initKW) <> parens empty <> semi) $+$
                                       char '}'
        cRoot                       = (gen env tActor <+> gen env primROOT <+> parens empty <+> char '{') $+$
                                       nest 4 (text "return" <+> parens (gen env tActor) <> gen env primNEWACTOR <> parens (gen env qn) <> semi) $+$
                                       char '}'


endsRight [Right _]                 = True
endsRight(x : xs)                   = endsRight xs
endsRight []                        = False

myPretty (GName m n)
      | m == mBuiltin               = text ("B_" ++ nstr n)
      | otherwise                   = pretty m <> dot <> pretty n
      
myPretty (NoQ w@(Internal _ _ _))   = pretty w

staticStubs env                     = map f wns
    where wns                       = map h (filter g $ witnesses env)
          g w@(WClass{})            = length (wsteps w) == 1 || endsRight (wsteps w)
          g _                       = False
          f w                       = myPretty w <+> myPretty (witName w) <+> equals <+> braces(char '&' <> myPretty (instName w)) <> semi
          h w                       = if nm1 == nm2 then wname w else gBuiltin (Derived (Derived nm1 nm2) nm3)
             where nm1              = noq(tcname(proto w))
                   Derived nm2 nm3  = noq (wname w)

staticImpls env                     = map f wns ++ map k wns
    where wns                       = map h (filter g $ witnesses env)
          g w@(WClass{})            = length (wsteps w) == 1 || endsRight (wsteps w) && binds w == []
          g _                       = False
          f w                       = text "struct" <+> myPretty w <+> myPretty (instName w) <> semi
          k w                       = text "struct" <+> myPretty w <+> myPretty (instName w) <+> equals <+> braces(char '&' <> myPretty (methName w)) <> semi
          h w                       = if nm1 == nm2 then wname w else gBuiltin (Derived (Derived nm1 nm2) nm3)
             where nm1              = noq(tcname(proto w))
                   Derived nm2 nm3  = noq (wname w)

instName (GName m n)                = GName m (Derived n (globalName "instance"))
methName (GName m n)                = GName m (Derived n (globalName "methods"))

derivedHead (Derived d@(Derived{}) _) = derivedHead d
derivedHead (Derived n _)           = n

staticWitnessName (Dot _ c@(Call _ _ _ KwdNil) a) = (nm, NoQ a:as)
   where (nm,as)                    = staticWitnessName c
staticWitnessName (Call _ (Var _ v@(GName m n)) PosNil KwdNil)
    | m == mBuiltin                 = (Just v, [])
staticWitnessName (Call _ (TApp _ (Var _ (GName m n@(Derived n1 n2))) [TCon _ (TC gn1 []), _]) _ KwdNil)
   | m == mBuiltin && n1 == nMapping && n2 == nDict && gn1 == qnInt
                                    = (Just (gBuiltin (Derived n nInt)),[])
staticWitnessName (Call _ (TApp _ (Var _ (GName m n@(Derived n1 n2))) [TCon _ (TC gn1 []), _]) _ KwdNil)
   | m == mBuiltin && n1 == nMapping && n2 == nDict && gn1 == qnStr
                                    = (Just (gBuiltin (Derived n nStr)),[])
staticWitnessName (Call _ (TApp _ (Var _ v@(GName m n)) _) PosNil KwdNil)
    | m == mBuiltin                 = (Just v, [])
staticWitnessName _                 = (Nothing, [])
 
-- Environment --------------------------------------------------------------------------------------

genEnv env0                         = setX env0 GenX{ globalX = [], localX = [], retX = tNone }

type GenEnv                         = EnvF GenX

data GenX                           = GenX { globalX :: [Name], localX :: [Name], retX :: Type }

gdefine te env                      = modX env1 $ \x -> x{ globalX = dom te ++ globalX x }
  where env1                        = define te env

ldefine te env                      = modX env1 $ \x -> x{ localX = dom te ++ localX x }
  where env1                        = define te env

setRet t env                        = modX env $ \x -> x{ retX = t }

global env                          = globalX (envX env) \\ localX (envX env)

defined env                         = globalX (envX env) ++ localX (envX env)

ret env                             = retX $ envX env


-- Helpers ------------------------------------------------------------------------------------------

include                             :: GenEnv -> String -> ModName -> Doc
include env dir m                   = text "#include" <+> doubleQuotes (text (joinPath $ dir : modPath m) <> text ".h")

modNames (Import _ ms : is)         = [ m | ModuleItem m _ <- ms ] ++ modNames is
modNames (FromImport _ (ModRef (0,Just m)) _ : is)
                                    = m : modNames is
modNames (FromImportAll _ (ModRef (0,Just m)) : is)
                                    = m : modNames is
modNames []                         = []


-- Header -------------------------------------------------------------------------------------------

hModule env (Module m imps stmts)   = text "#pragma" <+> text "once" $+$
                                      (if inBuiltin env
                                       then empty
                                       else include env "builtin" (modName ["builtin"]) $+$
                                            include env "builtin" (modName ["env"]) $+$
                                            include env "rts" (modName ["rts"])) $+$
                                      text "#include" <+> doubleQuotes (text "gc/gc_typed.h") $+$
                                      vcat (map (include env "types") $ modNames imps) $+$
                                      hSuite env stmts $+$
                                      text "void" <+> genTopName env initKW <+> parens empty <> semi 


hSuite env []                       = empty
hSuite env (s:ss)                   = hStmt env s $+$ hSuite (gdefine (envOf s) env) ss

hStmt env (Decl _ ds)               = vmap (declstub env1) ds $+$
                                      vmap (typedef env1) ds $+$
                                      vmap (decl env1) ds $+$
                                      vmap (methstub env1) ds
  where env1                        = gdefine (envOf ds) env
hStmt env s                         = vcat [ text "extern" <+> gen env t <+> genTopName env n <> semi | (n,NVar t) <- envOf s]

declstub env (Class _ n q a b)      = text "struct" <+> genTopName env n <> semi
declstub env Def{}                  = empty

typedef env (Class _ n q a b)       = text "typedef" <+> text "struct" <+> genTopName env n <+> char '*' <> genTopName env n <> semi
typedef env Def{}                   = empty

decl env (Class _ n q a b)          = (text "struct" <+> classname env n <+> char '{') $+$
                                      nest 4 (vcat $ stdprefix env ++ initdef : serialize env tc : deserialize env tc : meths) $+$
                                      char '}' <> semi $+$
                                      inst_struct
  where tc                          = TC (NoQ n) [ tVar v | Quant v _ <- q ]
        initdef : meths             = fields env tc
        properties                  = [ varsig env n (sctype sc) <> semi | (n, NSig sc Property) <- fullAttrEnv env tc ]
        inst_struct | initNotImpl   = empty
                    | otherwise     = (text "struct" <+> genTopName env n <+> char '{') $+$ 
                                      nest 4 (classlink env n $+$ vcat properties) $+$
                                      char '}' <> semi $+$
                                      text "extern" <+> gcbm env n
        initNotImpl                 = any hasNotImpl [ b' | Decl _ ds <- b, Def{dname=n',dbody=b'} <- ds, n' == initKW ]
decl env (Def _ n q p _ a _ _ fx)   = gen env (exposeMsg fx $ fromJust a) <+> genTopName env n <+> parens (params env $ prowOf p) <> semi

methstub env (Class _ n q a b)      = text "extern" <+> text "struct" <+> classname env n <+> methodtable env n <> semi $+$
                                      constub env t n r b
  where TFun _ _ r _ t              = sctype $ fst $ schemaOf env (eVar n)
methstub env Def{}                  = empty

constub env t n r b
  | null ns || hasNotImpl b         = gen env t <+> newcon env n <> parens (params env r) <> semi
  | otherwise                       = empty
  where ns                          = abstractAttrs env (NoQ n)

fields env c                        = map field (subst [(tvSelf,tCon c)] te)
  where te                          = fullAttrEnv env c
        field (n, NDef sc Static)   = funsig env n (sctype sc) <> semi
        field (n, NDef sc NoDec)    = methsig env c n (sctype sc) <> semi
        field (n, NVar t)           = varsig env n t <> semi
        field (n, NSig sc Static)   = funsig env n (sctype sc) <> semi
        field (n, NSig sc NoDec)    = methsig env c n (sctype sc) <> semi
        field (n, NSig sc Property) = empty

funsig env n (TFun _ _ r _ t)       = gen env t <+> parens (char '*' <> gen env n) <+> parens (params env r)
funsig env n t                      = varsig env n t

methsig env c n (TFun _ fx r _ t)   = gen env (exposeMsg fx t) <+> parens (char '*' <> gen env n) <+> parens (params env $ posRow (tCon c) r)
methsig env c n t                   = varsig env n t

params env (TNil _ _)               = empty
params env (TRow _ _ _ t r@TRow{})  = gen env t <> comma <+> params env r
params env (TRow _ _ _ t TNil{})    = gen env t
params env (TRow _ _ _ t TVar{})    = gen env t                                         -- Ignore param tails for now...
params env t                        = error ("codegen unexpected row: " ++ prstr t)

exposeMsg fx t                      = if fx == fxAction then tMsg t else t

exposeMsg' t@TFun{}                 = t{ restype = exposeMsg (effect t) (restype t) }
exposeMsg' t                        = t

varsig env n t                      = gen env t <+> gen env n

stdprefix env                       = [gcdescr env, class_name env, classid env, superlink env]

gcdescr env                         = text "GC_descr" <+> gen env gcdescrKW <> semi
gcbm env n                          = text "GC_word" <+> genTopName env n <> text "D_gcbm" <> brackets (text "GC_BITMAP_SIZE" <> parens (text "struct" <+> genTopName env n)) <> semi
class_name env                      = text "char" <+> text "*" <> gen env classnameKW <> semi

classid env                         = text "int" <+> gen env classidKW <> semi

superlink env                       = gen env tSuperclass <+> gen env superclassKW <> semi
  where tSuperclass                 = tCon $ TC qnSuperClass []

qnSuperClass                        = GName mPrim (Derived (name "Super") suffixClass)

serialize env c                     = text "void" <+> parens (char '*' <> gen env serializeKW) <+> parens (gen env c <> comma <+> gen env tSerialstate) <> semi

deserialize env c                   = gen env (tCon c) <+> parens (char '*' <> gen env deserializeKW) <+> parens (gen env c <> comma <+> gen env tSerialstate) <> semi

classlink env n                     = text "struct" <+> classname env n <+> text "*" <> gen env classKW <> semi

classname env n                     = genTopName env (Derived n suffixClass)

methodtable env n                   = gen env (tableName $ gname env n)

staticwitness env n                 = gen env (witName n)

methodtable' env (NoQ n)            = methodtable env n
methodtable' env n                  = gen env $ tableName n

tableName (GName m n)               = GName m (Derived n suffixMethods)
witName (GName m n)                 = GName m (Derived n suffixWitness)
witName n                           = error ("witName " ++ show n)

newcon env n                        = gen env (conName $ gname env n)

newcon' env (NoQ n)                 = newcon env n
newcon' env n                       = gen env $ conName $ unalias env n

conName (GName m n)                 = GName m (Derived n suffixNew)

serializeSup env c                  = methodtable' env c <> dot <> gen env serializeKW
deserializeSup env c                = methodtable' env c <> dot <> gen env deserializeKW

classKW                             = primKW "class"
gcdescrKW                           = primKW "GCdescr"
classnameKW                         = primKW "name"
classidKW                           = primKW "class_id"
superclassKW                        = primKW "superclass"
componentsKW                        = name "components"
serializeKW                         = name "__serialize__"
deserializeKW                       = name "__deserialize__"

primAND                             = gPrim "AND"
primOR                              = gPrim "OR"
primNOT                             = gPrim "NOT"
primROOT                            = gPrim "ROOT"
primROOTINIT                        = gPrim "ROOTINIT"
primRegister                        = gPrim "register"

primToInt                           = name "to$int"
primToInt2                          = name "to$int2"
primToFloat                         = name "to$float"
primToStr                           = name "to$str"
primToBytearray                     = name "to$bytearray"
primToBytes                         = name "to$bytes"

tmpV                                = primKW "tmp"

tSerialstate                        = tCon $ TC (GName mPrim (name "Serial$state")) []
primStepSerialize                   = gPrim "step_serialize"
primStepDeserialize                 = gPrim "step_deserialize"
primDNEW                            = gPrim "DNEW"
primNEWTUPLE                        = gPrim "NEWTUPLE"
primNEWTUPLE0                       = gPrim "NEWTUPLE0"


-- Implementation -----------------------------------------------------------------------------------

cModule env srcbase (Module m imps stmts)
                                    = include env (if inBuiltin env then "" else "types") m $+$
                                      (if inBuiltin env then text "#include \"../rts/rts.h\"" else empty) $+$
                                      ext_include $+$
                                      declModule env stmts $+$
                                      text "int" <+> genTopName env initFlag <+> equals <+> text "0" <> semi $+$
                                      (text "void" <+> genTopName env initKW <+> parens empty <+> char '{') $+$
                                      nest 4 (text "if" <+> parens (genTopName env initFlag) <+> text "return" <> semi $+$
                                              genTopName env initFlag <+> equals <+> text "1" <> semi $+$
                                              ext_init $+$
                                              initImports $+$
                                              initModule env stmts) $+$
                                      char '}'
  where initImports                 = vcat [ gen env (GName m initKW) <> parens empty <> semi | m <- modNames imps ]
        external                    = hasNotImpl stmts && not (inBuiltin env)
        ext_include                 = if hasNotImpl stmts then text "#include" <+> doubleQuotes (text srcbase <> text ".ext.c") else empty
        ext_init                    = if hasNotImpl stmts then genTopName env (name "__ext_init__") <+> parens empty <> semi else empty


declModule env []                   = empty
declModule env (Decl _ ds : ss)     = vcat [ declDecl env1 d | d <- ds ] $+$
                                      declModule env1 ss
  where env1                        = gdefine (envOf ds) env
        te                          = envOf ds
declModule env (Signature{} : ss)   = declModule env ss
declModule env (s : ss)             = vcat [ gen env t <+> genTopName env n <> semi | (n,NVar t) <- te ] $+$
                                      declModule env1 ss
  where te                          = envOf s `exclude` defined env
        env1                        = gdefine te env


declDecl env (Def _ n q p KwdNIL (Just t) b d fx)
  | hasNotImpl b                    = gen env t <+> genTopName env n <+> parens (gen env p) <> semi
  | otherwise                       = (gen env t1 <+> genTopName env n <+> parens (gen env p) <+> char '{') $+$
                                      nest 4 (genSuite env1 b) $+$
                                      char '}'
  where env1                        = setRet t1 $ ldefine (envOf p) $ defineTVars q env
        t1                          = exposeMsg fx t

declDecl env (Class _ n q as b)
    | cDefinedClass                 = vcat [ declDecl env1 d{ dname = methodname n (dname d) } | Decl _ ds <- b', d@Def{} <- ds ] $+$
                                      text "struct" <+> classname env n <+> methodtable env n <> semi
    | otherwise                     = vcat [ declDecl env1 d{ dname = methodname n (dname d) } | Decl _ ds <- b', d@Def{} <- ds ] $+$
                                      declSerialize env1 n c props sup_c $+$
                                      declDeserialize env1 n c props sup_c $+$
                                      declCon env1 n q b $+$
                                      text "struct" <+> classname env n <+> methodtable env n <> semi $+$
                                      gcbm env n
  where b'                          = subst [(tvSelf, tCon c)] b
        c                           = TC (NoQ n) (map tVar $ qbound q)
        env1                        = defineTVars q env
        props                       = [ n | (n, NSig sc Property) <- fullAttrEnv env c ]
        sup_c                       = filter ((`elem` special_repr) . tcname) as
        special_repr                = [primActor] -- To be extended...
        cDefinedClass               = inBuiltin env && any hasNotImpl [b' | Decl _ ds <- b, Def{dname=n',dbody=b'} <- ds, n' == initKW ]

declSerialize env n c props sup_c   = (text "void" <+> genTopName env (methodname n serializeKW) <+> parens (gen env pars) <+> char '{') $+$
                                      nest 4 (super_step $+$ vcat [ step i | i <- props \\ super_attrs ]) $+$
                                      char '}'
  where pars                        = PosPar self (Just $ tCon c) Nothing $ PosPar st (Just tSerialstate) Nothing PosNIL
        st                          = name "state"
        self                        = name "self"
        super_step | [c] <- sup_c   = serializeSup env (tcname c) <> parens (parens (gen env $ tcname c) <> gen env self <> comma <+> gen env st) <> semi
                   | otherwise      = empty
        super_attrs                 = [ i | c <- sup_c, i <- conAttrs env (tcname c) ]
        step i                      = gen env primStepSerialize <> parens (gen env self <> text "->" <> gen env i <> comma <+> gen env st) <> semi

declDeserialize env n c props sup_c = (gen env (tCon c) <+> genTopName env (methodname n deserializeKW) <+> parens (gen env pars) <+> char '{') $+$
                                      nest 4 (optcreate $+$ super_step $+$ vcat [ step i | i <- props \\ super_attrs ] $+$ ret) $+$
                                      char '}'
  where pars                        = PosPar self (Just $ tCon c) Nothing $ PosPar st (Just tSerialstate) Nothing PosNIL
        st                          = name "state"
        self                        = name "self"
        env1                        = ldefine [(st, NVar tSerialstate)] env
        optcreate                   = (text "if" <+> parens (text "!" <> gen env self) <+> char '{') $+$
                                      nest 4 ((text "if" <+> parens (text "!" <> gen env st) <+> char '{') $+$
                                              nest 4 (alloc $+$ text "return" <+> gen env self <> semi) $+$
                                              char '}' $+$
                                              create) $+$
                                      char '}'
        create                      = gen env self <+> text "=" <+> gen env primDNEW <> parens (genTopName env n <> comma <+> gen env st) <> semi
        alloc                       = gen env self <+> equals <+> typed_malloc env (gname env n) (methodtable env1 n) <> semi $+$
                                      gen env self <> text "->" <> gen env1 classKW <+> equals <+> char '&' <> methodtable env1 n <> semi
        super_step | [c] <- sup_c   = deserializeSup env (tcname c) <> parens (parens (gen env $ tcname c) <> gen env self <> comma <+> gen env st) <> semi
                   | otherwise      = empty
        super_attrs                 = [ i | c <- sup_c, i <- conAttrs env (tcname c) ]
        step i                      = gen env self <> text "->" <> gen env i <+> text "=" <+> gen env primStepDeserialize <> parens (gen env st) <> semi
        ret                         = text "return" <+> gen env self <> semi


initModule env []                   = empty
initModule env (Decl _ ds : ss)     = vcat [ char '{' $+$ nest 4 (initClassBase env1 n q as hC $+$ initClass env n b hC) $+$ char '}' | Class _ n q as b <- ds, let hC = hasCDef b ] $+$
                                      initModule env1 ss
  where env1                        = gdefine (envOf ds) env
        hasCDef b                   = inBuiltin env && any hasNotImpl [b' | Decl _ ds <- b, Def{dname=n',dbody=b'} <- ds, n' == initKW ]
        
initModule env (Signature{} : ss)   = initModule env ss
initModule env (s : ss)             = genStmt env s $+$
                                      vcat [ genTopName env n <+> equals <+> gen env n <> semi | (n,_) <- te ] $+$
                                      initModule env1 ss
  where te                          = envOf s `exclude` defined env
        env1                        = gdefine te env

-- TODO: complete GC_set_bit
initClassBase env c q as hasCDef    = text "//" <+> genTopName env c <> text "D_gcbm" <+> text "is already initalized to 0 as bdwgc malloc is actually calloc" $+$
                                      vcat [ text "GC_set_bit" <> parens (genTopName env c <> text "D_gcbm" <> comma <+> text "GC_WORD_OFFSET" <> parens (text "struct" <+> genTopName env c <> comma <+> gen env n))<> semi | (n, NSig sc Property) <- fullAttrEnv env tc ] $+$
                                      methodtable env c <> dot <> gen env gcdescrKW <+> equals <+> text "GC_make_descriptor" <> parens (genTopName env c <> text "D_gcbm" <> comma <+> text "GC_WORD_LEN" <> parens (text "struct" <+> genTopName env c)) <> semi $+$
                                      methodtable env c <> dot <> gen env classnameKW <+> equals <+> doubleQuotes (genTopName env c) <> semi $+$
                                      methodtable env c <> dot <> gen env superclassKW <+> equals <+> super <> semi $+$
                                      vcat [ inherit c' n | (c',n) <- inheritedAttrs env (NoQ c) ] $+$ text "\n\n"
  where super                       = if null as then text "NULL" else parens (gen env qnSuperClass) <> text "&" <> methodtable' env (tcname $ head as)
        selfsubst                   = subst [(tvSelf, tCon tc)]
        tc                          = TC (NoQ c) [ tVar v | Quant v _ <- q ]
        inherit c' n
          | hasCDef                 = methodtable env c <> dot <> gen env n <+> equals <+> genTopName env (methodname c n) <> semi
          | otherwise               = methodtable env c <> dot <> gen env n <+> equals <+> cast (fromJust $ lookup n te) <> methodtable' env c' <> dot <> gen env n <> semi
        cast (NSig sc dec)          = parens (gen env (selfsubst $ addSelf (sctype sc) (Just dec)))
        cast (NDef sc dec)          = parens (gen env (selfsubst $ addSelf (sctype sc) (Just dec)))
        cast (NVar t)               = parens (gen env $ selfsubst t)
        te                          = fullAttrEnv env $ TC (NoQ c) [ tVar v | Quant v _ <- q ]

initClass env c [] hasCDef          = vcat [ methodtable env c <> dot <> gen env n <+> equals <+> genTopName env (methodname c n) <> semi | n <- [serializeKW,deserializeKW] ] $+$
                                      if hasCDef then empty else gen env primRegister <> parens (char '&' <> methodtable env c) <> semi
initClass env c (Decl _ ds : ss) b  = vcat [ methodtable env c <> dot <> gen env n <+> equals <+> genTopName env (methodname c n) <> semi | Def{dname=n} <- ds ] $+$
                                      initClass env1 c ss b
  where env1                        = gdefine (envOf ds) env
initClass env c (Signature{} : ss) b = initClass env c ss b
initClass env c (s : ss) b
  | isNotImpl s                     = initClass env c ss b
  | otherwise                       = genStmt env s $+$
                                      vcat [ genTopName env c <> dot <> gen env n <+> equals <+> gen env n <> semi | (n,_) <- te ] $+$
                                      initClass env1 c ss b
  where te                          = envOf s `exclude` defined env
        env1                        = ldefine te env


initFlag                            = name "done$"

methodname c n                      = Derived c n



class Gen a where
    gen                             :: GenEnv -> a -> Doc


instance (Gen a) => Gen (Maybe a) where
    gen env x                       = maybe empty (gen env) x


instance Gen ModName where
    gen env (ModName ns)            = hcat $ punctuate (text "Q_") $ map (gen env) ns

instance Gen QName where
    gen env (GName m n)
      | m == mPrim                  = char '$' <> text (nstr n)
      | m == mBuiltin               = text "B_" <> text (nstr n)
      | otherwise                   = gen env m <> text "Q_" <> text (mkCident $ nstr n)
    gen env (NoQ n)                 = gen env n
    gen env n@QName{}               = gen env (unalias env n)

instance Gen Name where
    gen env nm                      = text $ unCkeyword $ mkCident $ nstr nm

genTopName env n                    = gen env (gname env n)

genQName env (NoQ n)
  | n `elem` global env             = genTopName env n
  | isAlias n env                   = genTopName env n
genQName env n                      = gen env n

gname env n                         = unalias env (NoQ n)

mkCident "complex"                  = "complx"
mkCident "__complex__"              = "__complx__"
mkCident "complx"                   = "A_complex"
mkCident "__complx__"               = "A___complex__"

mkCident (c:s)
  | isAlpha c                       = c : esc s
  | otherwise                       = hex c ++ esc s
  where isAlpha c                   = c `elem` ['a'..'z'] || c `elem` ['A'..'Z'] || c `elem` ['_','$']
        isAlphaNum c                = isAlpha c || c `elem` ['0'..'9']
        esc (c:s) | isAlphaNum c    = c : esc s
                  | otherwise       = hex c ++ esc s
        esc ""                      = ""
        hex c                       = "X_" ++ showHex (fromEnum c) "_"

unCkeyword str
  | str `Data.Set.member` rws       = preEscape str
  | otherwise                       = str
  where rws                         = Data.Set.fromDistinctAscList [
                                        "auto",     "break",    "case",     "char",     "const",    "continue",
                                        "default",  "do",       "double",   "else",     "enum",     "extern",
                                        "float",    "for",      "goto",     "if",       "int",      "long",
                                        "register", "return",   "short",    "signed",   "sizeof",   "static",
                                        "struct",   "switch",   "typedef",  "union",    "unsigned", "void",
                                        "volatile", "while"
                                      ]



preEscape str                       = "A_" ++ str

word                                = text "$WORD"

genSuite env []                     = empty
genSuite env (s:ss)                 = genStmt env s $+$ genSuite (ldefine (envOf s) env) ss
  where te                          = envOf s `exclude` defined env
        env1                        = ldefine te env

genStmt env (Decl _ ds)             = empty
genStmt env (Assign _ [PVar _ n (Just t)] e)
  | n `notElem` defined env         = gen env t <+> gen env n <+> equals <+> rhs <> semi
  where isWitness (Internal Witness _ _)
                                    = True
        isWitness _                 = False
        rhs                         = if isWitness n 
                                      then case staticWitnessName e of
                                           (Just (nm),as) -> foldr (\x y -> y <>text "->" <> myPretty (x)) (parens(myPretty (tcname(tcon t))) <> myPretty (witName nm)) as 
                                           _  ->  genExp env t e
                                      else genExp env t e
genStmt env s                       = vcat [ gen env t <+> gen env n <> semi | (n,NVar t) <- te ] $+$
                                      gen env s
  where te                          = envOf s `exclude` defined env

instance Gen Stmt where
    gen env (Expr _ Strings{})      = semi
    gen env (Expr _ e)              = genExp' env e <> semi
    gen env (Assign _ [p] e)        = gen env p <+> equals <+> genExp env t e <> semi
      where t                       = typeOf env p
    gen env (MutAssign _ tg e)      = genTarget env tg <+> equals <+> genExp env t e <> semi
      where t                       = targetType env tg
    gen env (Pass _)                = empty
    gen env (Return _ Nothing)      = text "return" <+> gen env eNone <> semi
    gen env (Return _ (Just e))     = text "return" <+> genExp env (ret env) e <> semi
    gen env (Break _)               = text "break" <> semi
    gen env (Continue _)            = text "continue" <> semi
    gen env (If _ (b:bs) b2)        = genBranch env "if" b $+$ vmap (genBranch env "else if") bs $+$ genElse env b2
    gen env (While _ e b [])        = (text "while" <+> parens (genBool env e <> text "->val") <+> char '{') $+$ nest 4 (genSuite env b) $+$ char '}'
    gen env _                       = empty


genBranch env kw (Branch e b)       = (text kw <+> parens (genBranchExp env e) <+> char '{') $+$ nest 4 (genSuite env b) $+$ char '}'


genBranchExp env (IsInstance _ x y) = text "$ISINSTANCE0" <> parens(gen env x <>comma <+> genQName env y)
genBranchExp env e@(Call _ (Dot _ (Var _ w) op) (PosArg x (PosArg y PosNil)) KwdNil)
                                    = case findQName w env of
                                        NVar (TCon _ (TC p [TCon _ (TC t [])]))
                                          | p==qnOrd && t==qnInt ->
                                             text "ORD_INT" <> text (nstr op) <> parens(gen env x <>comma <+> gen env y) 
                                        _ -> genBool env e <> text "->val" 
genBranchExp env e                  = genBool env e <> text "->val"

genElse env []                      = empty
genElse env b                       = (text "else" <+> char '{') $+$ nest 4 (genSuite env b) $+$ char '}'

instance Gen PosPar where
    gen env (PosPar n t _ PosNIL)   = gen env t <+> gen env n
    gen env (PosPar n t _ p)        = gen env t <+> gen env n <> comma <+> gen env p
    gen env PosNIL                  = empty

instance Gen PosArg where
    gen env (PosArg e PosNil)       = gen env e
    gen env (PosArg e p)            = gen env e <> comma <+> gen env p
    gen env PosNil                  = empty


formatLit (Strings l ss)            = Strings l [format $ concat ss]
  where format []                   = []
        format ('%':s)              = '%' : flags s
        format (c:s)                = c : format s
        flags (f:s)
          | f `elem` "#0- +"        = f : flags s
        flags s                     = width s
        width ('*':s)               = '*' : dot s
        width (n:s)
          | n `elem` "123456789"    = let (n',s') = span (`elem` "0123456789") s in n : n' ++ dot s'
        width s                     = dot s
        dot ('.':s)                 = '.' : prec s
        dot s                       = len s
        prec ('*':s)                = '*' : len s
        prec (n:s)
          | n `elem` "0123456789"   = let (n',s') = span (`elem` "0123456789") s in n : n' ++ len s'
        prec s                      = len s
        len (l:s)
          | l `elem` "hlL"          = 'l' : conv s
        len s                       = conv0 s
        conv0 (t:s)
          | t `elem` "diouxXc"      = 'l' : conv (t:s)
        conv0 s                     = conv s
        conv (t:s)                  = t : format s

castLit env (Strings l ss) p        = format (concat ss) p
  where format [] p                 = empty
        format ('%':s) p            = flags s p
        format (c:s) p              = format s p
        flags (f:s) p
          | f `elem` "#0- +"        = flags s p
        flags s p                   = width s p
        width ('*':s) (PosArg e p)  = comma <+> parens (text "int") <> expr <> dot s p
          where expr                = text "from$int" <> parens (gen env e) 
        width (n:s) p
          | n `elem` "123456789"    = let (n',s') = span (`elem` "0123456789") s in dot s' p
        width s p                   = dot s p
        dot ('.':s) p               = prec s p
        dot s p                     = len s p
        prec ('*':s) (PosArg e p)   = comma <+> parens (text "int") <> expr <> len s p
          where expr                = text "from$int" <> parens (gen env e)  --parens (parens (gen env tInt) <> gen env e) <> text "->val"
        prec (n:s) p
          | n `elem` "0123456789"   = let (n',s') = span (`elem` "0123456789") s in len s' p
        prec s p                    = len s p
        len (l:s) p
          | l `elem` "hlL"          = conv s p
        len s p                     = conv s p
        conv (t:s) (PosArg e p)
          | t `elem` "diouxXc"      = comma <+> expr <> format s p
          where expr                = text "from$int" <> parens (gen env e) --parens (parens (gen env tInt) <> gen env e) <> text "->val"
        conv (t:s) (PosArg e p)
          | t `elem` "eEfFgG"       = comma <+> expr <> format s p
          where expr                = parens (parens (gen env tFloat) <> gen env e) <> text "->val"
        conv (t:s) (PosArg e p)
          | t `elem` "rsa"          = comma <+> expr <> format s p
          where expr                = parens (parens (gen env tStr) <> gen env e) <> text "->str"
        conv ('%':s) p              = format s p

genCall env [] (TApp _ e ts) p      = genCall env ts e p
genCall env [_,t] (Var _ n) (PosArg e PosNil)
  | n == primCAST                   = parens (parens (gen env t) <> gen env e)
genCall env [row] (Var _ n) p
  | qn == qnPrint                   = if i>0
                                      then gen env qn <> parens (gen env primNEWTUPLE <> parens (pretty i <> comma <> gen env p))
                                      else gen env qn <> parens (gen env primNEWTUPLE0)
  where i                           = nargs p
        qn                          = unalias env n
genCall env [row] (Var _ n) (PosArg s@Strings{} (PosArg tup PosNil))
  | n == primFORMAT                 = gen env n <> parens (genStr env (formatLit s) <> castLit env s (flatten tup))
  where unbox (TNil _ _) p          = empty
        unbox (TRow _  _ _ t r) (PosArg e p)
          | t == tStr               = comma <+> expr <> text "->str" <> unbox r p
          | otherwise               = comma <+> expr <> text "->val" <> unbox r p
          where expr                = parens (parens (gen env t) <> gen env e)
        flatten (Tuple _ p KwdNil)  = p
        flatten e                   = foldr PosArg PosNil $ map (DotI l0 e) [0..]
genCall env [t] (Var _ n) PosNil
  | n == primNEWACTOR               = gen env n <> parens (gen env t)
genCall env ts e@(Var _ n) p
  | NClass{} <- info                = genNew env n p
  | NDef{} <- info                  = (instCast env ts e $ gen env e) <> parens (gen env p)
  where info                        = findQName n env
genCall env ts (Async _ e) p        = genCall env ts e p
genCall env ts e0@(Dot _ e n) p     = genDotCall env ts (snd $ schemaOf env e0) e n p

instCast env [] e                   = id
instCast env ts (Var _ x)
  | GName m _ <- x, m == mPrim      = id
instCast env ts e                   = parens . (parens (gen env t) <>)
  where t                           = typeInstOf env ts e

targetType env (Dot _ e n)          = sctype sc
  where t0                          = typeOf env e
        (_,c0)                      = case t0 of
                                         TCon _ tc -> splitTC env tc
                                         TVar _ tv -> splitTC env (findTVBound env tv)
        (sc, dec)                   = findAttr' env c0 n
targetType env e                    = typeOf env e                  -- Must be a Var with a monomorphic type since it is assignable

dotCast env ent ts (Var _ x) n
  | GName m _ <- x, m == mPrim      = id
dotCast env ent ts e n
  | gen_t == gen env t1             = id
  | otherwise                       = parens . (parens gen_t <>)
  where t0                          = typeOf env e
        (argsubst, c0)              = case t0 of
                                         TCon _ tc -> splitTC env tc
                                         TVar _ tv -> splitTC env (findTVBound env tv)
        (sc, dec)                   = findAttr' env c0 n
        t                           = subst fullsubst $ if ent then addSelf t1 dec else t1
        t1                          = exposeMsg' (sctype sc)
        fullsubst                   = (tvSelf,t0) : (qbound (scbind sc) `zip` ts) ++ argsubst
        gen_t                       = gen env t

classCast env ts x q n              = parens . (parens (gen env t) <>)
  where (ts0,ts1)                   = splitAt (length q) ts
        tc                          = TC x ts0
        (sc, dec)                   = findAttr' env tc n
        t                           = subst fullsubst $ addSelf (sctype sc) dec
        fullsubst                   = (tvSelf,tCon tc) : (qbound (scbind sc) `zip` ts1)

genNew env n p                      = newcon' env n <> parens (gen env p)

declCon env n q b
  | null abstr || hasNotImpl b      = (gen env tRes <+> newcon env n <> parens (gen env pars) <+> char '{') $+$
                                      nest 4 (gen env tObj <+> gen env tmpV <+> equals <+> typed_malloc env (gname env n) (methodtable env1 n) <> semi $+$
                                              gen env tmpV <> text "->" <> gen env1 classKW <+> equals <+> char '&' <> methodtable env1 n <> semi $+$
                                              initcall env1) $+$
                                      char '}'
  | otherwise                       = empty
  where TFun _ fx r _ t             = sctype $ fst $ schemaOf env (eVar n)
        tObj                        = tCon $ TC (NoQ n) (map tVar $ qbound q)
        tRes                        = if t == tR then tR else tObj
        pars                        = pPar paramNames r
        args                        = pArg pars
        initcall env | t == tR      = text "return" <+> methodtable env n <> dot <> gen env initKW <> parens (gen env tmpV <> comma <+> gen env args) <> semi
                     | otherwise    = methodtable env n <> dot <> gen env initKW <> parens (gen env tmpV <> comma' (gen env args)) <> semi $+$
                                      text "return" <+> gen env tmpV <> semi
        retobj (PosArg e PosNil)    = PosArg (eCall (tApp (eQVar primCONSTCONT) [tObj]) [e, eVar tmpV]) PosNil
        retobj (PosArg e p)         = PosArg e (retobj p) 
        env1                        = ldefine ((tmpV, NVar tObj) : envOf pars) env
        abstr                       = abstractAttrs env (NoQ n)

typed_malloc env n mt               = text "GC_MALLOC_EXPLICITLY_TYPED" <> parens (text "sizeof" <> parens (text "struct" <+> gen env n) <> comma <+> mt <> dot <> gen env gcdescrKW)
malloc env n                        = text "malloc" <> parens (text "sizeof" <> parens (text "struct" <+> gen env n))

comma' x                            = if isEmpty x then empty else comma <+> x

genDotCall env ts dec e@(Var _ x) n p
  | NClass q _ _ <- info,
    Just _ <- dec                   = classCast env ts x q n (methodtable' env x <> text "." <> gen env n) <> parens (gen env p)
  where info                        = findQName x env
genDotCall env ts dec e n p
  | Just NoDec <- dec               = genEnter env ts e n p
  | Just Static <- dec              = dotCast env False ts e n (gen env e <> text "->" <> gen env classKW <> text "->" <> gen env n) <> parens (gen env p)


genDot env ts e@(Var _ x) n
  | NClass q _ _ <- findQName x env = classCast env ts x q n $ methodtable' env x <> text "." <> gen env n
genDot env ts e n                   = dotCast env False ts e n $ gen env e <> text "->" <> gen env n
-- NOTE: all method references are eta-expanded by the lambda-lifter at this point, so n cannot be a method (i.e., require methodtable lookup) here

genTarget env (Dot _ e n)           = gen env e <> text "->" <> gen env n
genTarget env e                     = gen env e


genEnter env ts e n p
  | costly e                        = parens (lbrace <+> (gen env t <+> gen env tmpV <+> equals <+> gen env e <> semi $+$
                                                          genEnter env1 ts (eVar tmpV) n p <> semi) <+> rbrace)
  where costly Var{}                = False
        costly (Dot _ e n)          = costly e
        costly (DotI _ e i)         = costly e
        costly e                    = True
        t                           = typeOf env e
        env1                        = ldefine [(tmpV,NVar t)] env
genEnter env ts e n p               = dotCast env True ts e n (gen env e <> text "->" <> gen env classKW <> text "->" <> gen env n) <> parens (gen env e <> comma' (gen env p))

genInst env ts e@Var{}              = instCast env ts e $ gen env e
genInst env ts (Dot _ e n)          = genDot env ts e n

adjust t t' e
  | t == t'                         = e
adjust (TOpt _ t) t' e              = adjust t t' e
adjust t (TOpt _ t') e              = adjust t t' e
adjust TNone{} t' e                 = e
adjust t t'@TVar{} e                = e
adjust (TCon _ c) (TCon _ c') e
  | tcname c == tcname c'           = e
adjust t t' e                       = typecast t t' e

genExp env t' e                     = gen env (adjust t t' e')
  where (t, e')                     = qType env adjust e
 
genExp' env e                       = gen env e'
  where (t, e')                     = qType env adjust e

instance Gen Expr where
    gen env (Var _ n)
      | NClass{} <- findQName n env = newcon' env n
      | otherwise                   = genQName env n
    gen env (Int _ i str)
        |i <= 9223372036854775807   = gen env primToInt <> parens (text str) -- literal is 2^63-1
         | otherwise                = gen env primToInt2 <> parens (doubleQuotes $ text (show i))
    gen env (Float _ _ str)         = gen env primToFloat <> parens (text str)
    gen env (Bool _ True)           = gen env qnTrue
    gen env (Bool _ False)          = gen env qnFalse
    gen env (None _)                = gen env qnNone
    gen env e@Strings{}             = gen env primToStr <> parens(hsep (map pretty (sval e)))
    gen env e@BStrings{}            = gen env primToBytes <> parens(hsep (map pretty (sval e)))
    gen env (Call _ e@(Dot _ (Var _ w) fa) p@(PosArg (Call _ (TApp _ _ ts) (PosArg i@(Int _ n s) PosNil) KwdNil) PosNil) KwdNil)
         | fa == fromatomKW         = case wInfo of
                                         NVar (TCon _ (TC _ [TCon _ (TC tn _)]))
                                            | tn == qnInt -> gen env i
                                            | tn == qnI64 -> text "toB_i64" <> parens (text s) 
                                         _                -> genCall env [] e p
       where wInfo = findQName w env
    gen env (Call l  e@(TApp _ (Dot _ (Var _ w) fi) _) p@(PosArg _ (PosArg (List _ es) PosNil)) KwdNil)
      | fi == fromiterKW 
                                     = case findQName w env of
                                          NVar (TCon _ (TC gn1 (TCon _ (TC gn2 _):( TCon _ (TC gn3 _):_))))
                                             | gn2==qnDict && elem gn3 hs -> text "B_mk_dict" <> parens (pretty (length es) <> comma <+> hashwit gn3 <> hsep [comma <+> gen env e | e <- es])
                                             | gn2==qnSetT && elem gn3 hs -> text "B_mk_set" <> parens (pretty (length es) <> comma <+> hashwit gn3 <> hsep [comma <+> gen env e | e <- es])
                                          _ -> genCall env [] e p
      where hs                       = [qnInt, qnStr, qnFloat, qnI64, qnI32, qnI16, qnU64, qnU32, qnU16, qnBytes]
            hashwit gn               = gen env (witName (gBuiltin (Derived nHashable (noq gn))))
    gen env (Call l  e@(TApp _ (Dot _ (Dot _ (Var _ w) _) fi) _) p@(PosArg _ (PosArg (List _ es) PosNil)) KwdNil)
      | fi == fromiterKW            = case findQName w env of
                                          NVar (TCon _ (TC gn1 (TCon _ (TC gn2 _):ts)))
                                             | gn2==qnList -> text "B_mk_list" <> parens (pretty (length es) <> hsep [comma <+> gen env e | e <- es])
                                          _ -> genCall env [] e p
    gen env (Call _ e p _)          = genCall env [] e p
    gen env (Async _ e)             = gen env e
    gen env (TApp _ e ts)           = genInst env ts e
    gen env (IsInstance _ e c)      = gen env primISINSTANCE <> parens (gen env e <> comma <+> genQName env c)
    gen env (Dot _ e n)             = genDot env [] e n
    gen env (DotI _ e i)            = gen env e <> text "->" <> gen env componentsKW <> brackets (pretty i)
    gen env (RestI _ e i)           = gen env eNone <> semi <+> text "// CodeGen for tuple tail not implemented"
    gen env (Tuple _ p KwdNil)      
       | n == 0                    = gen env primNEWTUPLE0
       | otherwise                 = gen env primNEWTUPLE <> parens (text (show n) <> comma' (gen env p))
       where n                     = nargs p
    gen env (List _ es)
      | null es                     = newcon' env n <> parens (text "NULL" <> comma <+> text "NULL")
      | otherwise                   = parens (lbrace <+> (
                                         vcat (gen env n <+> tmp <+> equals <+> newcon' env n <> parens (text "NULL" <> comma <+> text "NULL") <> semi :
                                                [append <> parens (pars e) <> semi | e <- es ]) $+$
                                        tmp <> semi) <+> rbrace)
      where n                       = qnList
            tmp                     = gen env tmpV
            w                       = gen env witSequenceList
            append                  = w <> text "->" <> gen env classKW <> text "->" <> gen env appendKW
            pars e                  = w <> comma <+> tmp <> comma <+> gen env e
        -- brackets (commaSep (gen env) es)
    gen env (BinOp _ e1 And e2)     = gen env primAND <> parens (gen env t <> comma <+> gen env e1 <> comma <+> gen env e2)
      where t                       = typeOf env e1
    gen env (BinOp _ e1 Or e2)      = gen env primOR <> parens (gen env t <> comma <+> gen env e1 <> comma <+> gen env e2)
      where t                       = typeOf env e1
    gen env (UnOp _ Not e)          = gen env primNOT <> parens (gen env t <> comma <+> genBool env e)
      where t                       = typeOf env e
    gen env (Cond _ e1 e e2)        = parens (parens (genBool env e) <> text "->val" <+> text "?" <+> gen env e1 <+> text ":" <+> gen env e2)

genStr env s                        = text $ head $ sval s

genBool env e                       = genExp env tBool e
  where t                           = typeOf env e

nargs                               :: PosArg -> Int
nargs PosNil                        = 0
nargs (PosArg _ p)                  = 1 + nargs p

instance Gen Elem where
    gen env (Elem e)                = gen env e

instance Gen Pattern where
    gen env (PVar _ n _)            = gen env n

instance Gen TSchema where
    gen env (TSchema _ _ t)         = gen env t

instance Gen TVar where
    gen env (TV k n)                = word

instance Gen TCon where
    gen env (TC n ts)               = gen env (unalias env n)
    
instance Gen Type where
    gen env (TVar _ v)              = gen env v
    gen env (TCon  _ c)             = gen env c
    gen env (TFun _ _ p _ t)        = gen env t <+> parens (char '*') <+> parens (gen env p)
    gen env (TTuple _ pos _)        = gen env qnTuple
    gen env (TOpt _ t)              = gen env t
    gen env (TNone _)               = gen env qnNoneType
    gen env (TWild _)               = word
    gen env (TRow _ _ _ t TNil{})   = gen env t
    gen env (TRow _ _ _ t r)        = gen env t <> comma <+> gen env r
    gen env (TNil _ _)              = empty

