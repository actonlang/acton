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

generate                            :: Acton.Env.Env0 -> Module -> IO (String,String,String)
generate env m                      = do return (n, h,c)
  where n                           = render $ quotes $ gen env0 (modname m)
        h                           = render $ hModule env0 m
        c                           = render $ cModule env0 m
        env0                        = genEnv $ setMod (modname m) env

genRoot                             :: Acton.Env.Env0 -> QName -> Type -> IO String
genRoot env0 qn@(GName m n) t       = do return $ render (cInclude $+$ cInit $+$ cRoot)
  where env                         = genEnv $ setMod m env0
        env1                        = ldefine (envOf pars) env
        pars                        = pPar paramNames' r
        r                           = posRow t $ posRow (tCont tWild tWild) posNil
        cInclude                    = include env "modules" m
        cInit                       = (text "void" <+> gen env primROOTINIT <+> parens empty <+> char '{') $+$
                                       nest 4 (gen env1 (GName m initKW) <> parens empty <> semi) $+$
                                       char '}'
        cRoot                       = (gen env tR <+> gen env primROOT <+> parens (gen env pars) <+> char '{') $+$
                                       nest 4 (text "return" <+> gen env1 qn <> parens (gen env1 $ pArg pars) <> semi) $+$
                                       char '}'

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
                                      include env "builtin" (modName ["builtin"]) $+$
                                      include env "builtin" (modName ["minienv"]) $+$
                                      include env "rts" (modName ["rts"]) $+$
                                      vcat (map (include env "modules") $ modNames imps) $+$
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
                                      (text "struct" <+> genTopName env n <+> char '{') $+$ 
                                      nest 4 (classlink env n $+$ vcat properties) $+$
                                      char '}' <> semi
  where tc                          = TC (NoQ n) [ tVar v | Quant v _ <- q ]
        initdef : meths             = fields env tc
        properties                  = [ varsig env n (sctype sc) <> semi | (n, NSig sc Property) <- fullAttrEnv env tc ]
decl env (Def _ n q p _ a b _ fx)   = gen env (fromJust a) <+> genTopName env n <+> parens (params env $ prowOf p) <> semi

methstub env (Class _ n q a b)      = text "extern" <+> text "struct" <+> classname env n <+> methodtable env n <> semi $+$
                                      gen env t <+> newcon env n <> parens (params env r) <> semi
  where TFun _ _ r _ t              = sctype $ fst $ schemaOf env (eVar n)
methstub env Def{}                  = empty

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

methsig env c n (TFun _ _ r _ t)    = gen env t <+> parens (char '*' <> gen env n) <+> parens (params env $ posRow (tCon c) r)
methsig env c n t                   = varsig env n t

params env (TNil _ _)               = empty
params env (TRow _ _ _ t r@TRow{})  = gen env t <> comma <+> params env r
params env (TRow _ _ _ t TNil{})    = gen env t
params env (TRow _ _ _ t TVar{})    = gen env t                                         -- Ignore param tails for now...
params env t                        = error ("codegen unexpected row: " ++ prstr t)

varsig env n t                      = gen env t <+> gen env n

stdprefix env                       = [gcinfo env, classid env, superlink env]

gcinfo env                          = text "char" <+> text "*" <> gen env gcinfoKW <> semi

classid env                         = text "int" <+> gen env classidKW <> semi

superlink env                       = gen env tSuperclass <+> gen env superclassKW <> semi
  where tSuperclass                 = tCon $ TC qnSuperClass []

qnSuperClass                        = GName mPrim (Derived (name "Super") (name "class"))

serialize env c                     = text "void" <+> parens (char '*' <> gen env serializeKW) <+> parens (gen env c <> comma <+> gen env tSerialstate) <> semi

deserialize env c                   = gen env (tCon c) <+> parens (char '*' <> gen env deserializeKW) <+> parens (gen env c <> comma <+> gen env tSerialstate) <> semi

classlink env n                     = text "struct" <+> classname env n <+> text "*" <> gen env classKW <> semi

classname env n                     = genTopName env (Derived n $ name "class")

methodtable env n                   = gen env (tableName $ gname env n)

methodtable' env (NoQ n)            = methodtable env n
methodtable' env n                  = gen env $ tableName n

tableName (GName m n)               = GName m (Derived n $ name "methods")


newcon env n                        = gen env (conName $ gname env n)

newcon' env (NoQ n)                 = newcon env n
newcon' env n                       = gen env $ conName n

conName (GName m n)                 = GName m (Derived n $ name "new")

serializeSup env c                  = methodtable' env c <> dot <> gen env serializeKW
deserializeSup env c                = methodtable' env c <> dot <> gen env deserializeKW


classKW                             = primKW "class"
gcinfoKW                            = primKW "GCINFO"
classidKW                           = primKW "class_id"
superclassKW                        = primKW "superclass"
componentsKW                        = name "components"
serializeKW                         = name "__serialize__"
deserializeKW                       = name "__deserialize__"

primTuple                           = gPrim "tuple"
primNoneType                        = gPrim "NoneType"
primNone                            = gPrim "None"
primTrue                            = gPrim "True"
primFalse                           = gPrim "False"

primAND                             = gPrim "AND"
primOR                              = gPrim "OR"
primNOT                             = gPrim "NOT"
primROOT                            = gPrim "ROOT"
primROOTINIT                        = gPrim "ROOTINIT"
primRegister                        = gPrim "register"

primToInt                           = name "to$int"
primToFloat                         = name "to$float"
primToStr                           = name "to$str"
primToBytearray                     = name "to$bytearray"

tmpV                                = primKW "tmp"

tSerialstate                        = tCon $ TC (GName mPrim (Derived (name "Serial") (name "state"))) []
primStepSerialize                   = gPrim "step_serialize"
primStepDeserialize                 = gPrim "step_deserialize"
primDNEW                            = gPrim "DNEW"
primNEWTUPLE                        = gPrim "NEWTUPLE"


-- Implementation -----------------------------------------------------------------------------------

cModule env (Module m imps stmts)   = include env "modules" m $+$
                                      declModule env stmts $+$
                                      text "int" <+> genTopName env initFlag <+> equals <+> text "0" <> semi $+$
                                      (text "void" <+> genTopName env initKW <+> parens empty <+> char '{') $+$
                                      nest 4 (text "if" <+> parens (genTopName env initFlag) <+> text "return" <> semi $+$
                                              genTopName env initFlag <+> equals <+> text "1" <> semi $+$
                                              initImports $+$
                                              initModule env stmts) $+$
                                      char '}'
  where initImports                 = vcat [ gen env (GName m initKW) <> parens empty <> semi | m <- modNames imps ]


declModule env []                   = empty
declModule env (Decl _ ds : ss)     = vcat [ declDecl env1 d | d <- ds ] $+$
                                      declModule env1 ss
  where env1                        = gdefine (envOf ds) env
        te                          = envOf ds
declModule env (s : ss)             = vcat [ gen env t <+> genTopName env n <> semi | (n,NVar t) <- te ] $+$
                                      declModule env1 ss
  where te                          = envOf s `exclude` defined env
        env1                        = gdefine te env

declDecl env (Def _ n q p KwdNIL (Just t) b d m)
                                    = (gen env t <+> genTopName env n <+> parens (gen env p) <+> char '{') $+$
                                      nest 4 (genSuite env1 b $+$ ret) $+$
                                      char '}'
  where env1                        = setRet t $ ldefine (envOf p) $ defineTVars q env
        ret | fallsthru b           = text "return" <+> gen env primNone <> semi
            | otherwise             = empty

declDecl env (Class _ n q as b)     = vcat [ declDecl env1 d{ dname = methodname n (dname d) } | Decl _ ds <- b', d@Def{} <- ds ] $+$
                                      declSerialize env1 n c props sup_c $+$
                                      declDeserialize env1 n c props sup_c $+$
                                      declCon env1 n q $+$
                                      text "struct" <+> classname env n <+> methodtable env n <> semi
  where b'                          = subst [(tvSelf, tCon c)] b
        c                           = TC (NoQ n) (map tVar $ qbound q)
        env1                        = defineTVars q env
        props                       = [ n | (n, NSig sc Property) <- fullAttrEnv env c ]
        sup_c                       = filter ((`elem` special_repr) . tcname) as
        special_repr                = [primActor]                                               -- To be extended...

declSerialize env n c props sup_c   = (text "void" <+> genTopName env (methodname n serializeKW) <+> parens (gen env pars) <+> char '{') $+$
                                      nest 4 (super_step $+$ vcat [ step i | i <- props \\ super_props ]) $+$
                                      char '}'
  where pars                        = PosPar self (Just $ tCon c) Nothing $ PosPar st (Just tSerialstate) Nothing PosNIL
        st                          = name "state"
        self                        = name "self"
        super_step | [c] <- sup_c   = serializeSup env (tcname c) <> parens (parens (gen env $ tcname c) <> gen env self <> comma <+> gen env st) <> semi
                   | otherwise      = empty
        super_props                 = [ i | c <- sup_c, (i,_) <- attrEnv env c ]
        step i                      = gen env primStepSerialize <> parens (gen env self <> text "->" <> gen env i <> comma <+> gen env st) <> semi

declDeserialize env n c props sup_c = (gen env (tCon c) <+> genTopName env (methodname n deserializeKW) <+> parens (gen env pars) <+> char '{') $+$
                                      nest 4 (optcreate $+$ super_step $+$ vcat [ step i | i <- props \\ super_props ] $+$ ret) $+$
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
        alloc                       = gen env self <+> equals <+> malloc env (gname env n) <> semi $+$
                                      gen env self <> text "->" <> gen env1 classKW <+> equals <+> char '&' <> methodtable env1 n <> semi
        super_step | [c] <- sup_c   = deserializeSup env (tcname c) <> parens (parens (gen env $ tcname c) <> gen env self <> comma <+> gen env st) <> semi
                   | otherwise      = empty
        super_props                 = [ i | c <- sup_c, (i,_) <- attrEnv env c ]
        step i                      = gen env self <> text "->" <> gen env i <+> text "=" <+> gen env primStepDeserialize <> parens (gen env st) <> semi
        ret                         = text "return" <+> gen env self <> semi


initModule env []                   = empty
initModule env (Decl _ ds : ss)     = vcat [ char '{' $+$ nest 4 (initClassBase env1 n q as $+$ initClass env n b) $+$ char '}' | Class _ n q as b <- ds ] $+$
                                      initModule env1 ss
  where env1                        = gdefine (envOf ds) env
initModule env (Signature{} : ss)   = initModule env ss
initModule env (s : ss)             = genStmt env s $+$
                                      vcat [ genTopName env n <+> equals <+> gen env n <> semi | (n,_) <- te ] $+$
                                      initModule env1 ss
  where te                          = envOf s `exclude` defined env
        env1                        = gdefine te env


initClassBase env c q as            = methodtable env c <> dot <> gen env gcinfoKW <+> equals <+> doubleQuotes (genTopName env c) <> semi $+$
                                      methodtable env c <> dot <> gen env superclassKW <+> equals <+> super <> semi $+$
                                      vcat [ inherit c' n | (c',ns) <- inheritedAttrs env (NoQ c), n <- ns ]
  where super                       = if null as then text "NULL" else parens (gen env qnSuperClass) <> text "&" <> methodtable' env (tcname $ head as)
        selfsubst                   = subst [(tvSelf, tCon $ TC (NoQ c) [])]
        inherit c' n                = methodtable env c <> dot <> gen env n <+> equals <+> cast (fromJust $ lookup n te) <> methodtable' env c' <> dot <> gen env n <> semi
        cast (NSig sc dec)          = parens (gen env (selfsubst $ addSelf (sctype sc) (Just dec)))
        cast (NDef sc dec)          = parens (gen env (selfsubst $ addSelf (sctype sc) (Just dec)))
        cast (NVar t)               = parens (gen env $ selfsubst t)
        te                          = fullAttrEnv env $ TC (NoQ c) [ tVar v | Quant v _ <- q ]

initClass env c []                  = vcat [ methodtable env c <> dot <> gen env n <+> equals <+> genTopName env (methodname c n) <> semi | n <- [serializeKW,deserializeKW] ] $+$
                                      gen env primRegister <> parens (char '&' <> methodtable env c) <> semi
initClass env c (Decl _ ds : ss)    = vcat [ methodtable env c <> dot <> gen env n <+> equals <+> genTopName env (methodname c n) <> semi | Def{dname=n} <- ds ] $+$
                                      initClass env1 c ss
  where env1                        = gdefine (envOf ds) env
initClass env c (Signature{} : ss)  = initClass env c ss
initClass env c (s : ss)            = genStmt env s $+$
                                      vcat [ genTopName env c <> dot <> gen env n <+> equals <+> gen env n <> semi | (n,_) <- te ] $+$
                                      initClass env1 c ss
  where te                          = envOf s `exclude` defined env
        env1                        = ldefine te env


initFlag                            = name "done$"

methodname c n                      = Derived c n



class Gen a where
    gen                             :: GenEnv -> a -> Doc


instance (Gen a) => Gen (Maybe a) where
    gen env x                       = maybe empty (gen env) x


instance Gen ModName where
    gen env (ModName ns)            = hcat $ punctuate (char '$') $ map (gen env) ns

instance Gen QName where
    gen env (GName m n)
      | m == mPrim                  = char '$' <> text (nstr n)
      | m == mBuiltin               = char '$' <> text (nstr n)
      | otherwise                   = gen env m <> text "$$" <> text (mkCident $ nstr n)
    gen env (NoQ n)                 = gen env n
    gen env n@QName{}               = gen env (unalias env n)

instance Gen Name where
    gen env nm                      = text $ unCkeyword $ mkCident $ nstr nm


mkCident "complex"                  = "complx"
mkCident "__complex__"              = "__complx__"
mkCident "complx"                   = "complex$"
mkCident "__complx__"               = "__complex$__"

mkCident str
  | isCident str                    = str
  | otherwise                       = preEscape $ concat $ map esc str
  where isCident s@(c:cs)           = isAlpha c && all isAlphaNum cs
        isAlpha c                   = c `elem` ['a'..'z'] || c `elem` ['A'..'Z'] || c `elem` ['_','$']
        isAlphaNum c                = isAlpha c || c `elem` ['0'..'9']
        esc c | isAlphaNum c        = [c]
              | otherwise           = '_' : show (fromEnum c) ++ "_"

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

preEscape str                       = "_$" ++ str

genTopName env n                    = gen env (gname env n)

gname env n                         = unalias env (NoQ n)

word                                = text "$WORD"

genSuite env []                     = empty
genSuite env (s:ss)                 = genStmt env s $+$ genSuite (ldefine (envOf s) env) ss
  where te                          = envOf s `exclude` defined env
        env1                        = ldefine te env

genStmt env (Decl _ ds)             = empty
genStmt env (Assign _ [PVar _ n (Just t)] e)
  | n `notElem` defined env         = gen env t <+> gen env n <+> equals <+> genExp env t e <> semi
genStmt env s                       = vcat [ gen env t <+> gen env n <> semi | (n,NVar t) <- te ] $+$
                                      gen env s
  where te                          = envOf s `exclude` defined env

instance Gen Stmt where
    gen env (Expr _ Strings{})      = semi
    gen env (Expr _ e)              = genExp' env e <> semi
    gen env (Assign _ [p] e)        = gen env p <+> equals <+> genExp env t e <> semi
      where t                       = typeOf env p
    gen env (MutAssign _ tg e)      = gen env tg <+> equals <+> genExp env t e <> semi
      where t                       = typeOf env tg
    gen env (Pass _)                = empty
    gen env (Return _ Nothing)      = text "return" <+> gen env eNone <> semi
    gen env (Return _ (Just e))     = text "return" <+> genExp env (ret env) e <> semi
    gen env (Break _)               = text "break" <> semi
    gen env (Continue _)            = text "continue" <> semi
    gen env (If _ (b:bs) b2)        = genBranch env "if" b $+$ vmap (genBranch env "else if") bs $+$ genElse env b2
    gen env (While _ e b [])        = (text "while" <+> parens (genBool env e <> text "->val") <+> char '{') $+$ nest 4 (genSuite env b) $+$ char '}'
    gen env (Signature _ ns sc _)
      | TFun{} <- sctype sc         = vcat [ funsig env n (sctype sc) <> semi | n <- ns ]
      | otherwise                   = vcat [ varsig env n (sctype sc) <> semi | n <- ns ]
    gen env _                       = empty

genBranch env kw (Branch e b)       = (text kw <+> parens (genBool env e <> text "->val") <+> char '{') $+$ nest 4 (genSuite env b) $+$ char '}'

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
          where expr                = parens (parens (gen env tInt) <> gen env e) <> text "->val"
        width (n:s) p
          | n `elem` "123456789"    = let (n',s') = span (`elem` "0123456789") s in dot s' p
        width s p                   = dot s p
        dot ('.':s) p               = prec s p
        dot s p                     = len s p
        prec ('*':s) (PosArg e p)   = comma <+> parens (text "int") <> expr <> len s p
          where expr                = parens (parens (gen env tInt) <> gen env e) <> text "->val"
        prec (n:s) p
          | n `elem` "0123456789"   = let (n',s') = span (`elem` "0123456789") s in len s' p
        prec s p                    = len s p
        len (l:s) p
          | l `elem` "hlL"          = conv s p
        len s p                     = conv s p
        conv (t:s) (PosArg e p)
          | t `elem` "diouxXc"      = comma <+> expr <> format s p
          where expr                = parens (parens (gen env tInt) <> gen env e) <> text "->val"
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
  | qn == qnPrint                   = gen env qn <> parens (pretty i <> if i > 0 then comma <+> gen env p else empty)
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
genCall env ts e0@(Dot _ e n) p     = genDotCall env ts (snd $ schemaOf env e0) e n p
genCall env ts e p                  = genEnter env ts e callKW p

instCast env [] e                   = id
instCast env ts (Var _ x)
  | GName m _ <- x, m == mPrim      = id
instCast env ts e                   = parens . (parens (gen env t) <>)
  where t                           = typeInstOf env ts e

dotCast env ent ts (Var _ x) n
  | GName m _ <- x, m == mPrim      = id
dotCast env ent ts e n
  | null ts && null argsubst        = id
  | otherwise                       = -- trace ("## dotCast " ++ prstr e ++ " : " ++ prstr t0 ++ ", ." ++ prstr n ++ ": " ++ prstr sc ++ ", t: " ++ prstr t) $
                                      parens . (parens (gen env t) <>)
  where t0                          = typeOf env e
        (argsubst, c0)              = case t0 of
                                         TCon _ tc -> splitTC env tc
                                         TVar _ tv -> splitTC env (findTVBound env tv)
        (sc, dec)                   = findAttr' env c0 n
        t                           = subst fullsubst $ if ent then addSelf (sctype sc) dec else sctype sc
        fullsubst                   = (tvSelf,t0) : (qbound (scbind sc) `zip` ts) ++ argsubst


genNew env n p                      = newcon' env n <> parens (gen env p)

declCon env n q                     = (gen env tRes <+> newcon env n <> parens (gen env pars) <+> char '{') $+$
                                      nest 4 (gen env tObj <+> gen env tmpV <+> equals <+> malloc env (gname env n) <> semi $+$
                                              gen env tmpV <> text "->" <> gen env1 classKW <+> equals <+> char '&' <> methodtable env1 n <> semi $+$
                                              initcall env1) $+$
                                      char '}'
  where TFun _ fx r _ t             = sctype $ fst $ schemaOf env (eVar n)
        tObj                        = tCon $ TC (NoQ n) (map tVar $ qbound q)
        tRes                        = if t == tR then tR else tObj
        pars                        = pPar paramNames' r
        args                        = pArg pars
        initcall env | t == tR      = text "return" <+> methodtable env n <> dot <> gen env initKW <> parens (gen env tmpV <> comma <+> gen env (retobj args)) <> semi
                     | otherwise    = methodtable env n <> dot <> gen env initKW <> parens (gen env tmpV <> comma' (gen env args)) <> semi $+$
                                      text "return" <+> gen env tmpV <> semi
        retobj (PosArg e PosNil)    = PosArg (eCall (tApp (eQVar primCONSTCONT) [fx,tObj]) [eVar tmpV, e]) PosNil
        retobj (PosArg e p)         = PosArg e (retobj p)
        env1                        = ldefine ((tmpV, NVar tObj) : envOf pars) env

malloc env n                        = text "malloc" <> parens (text "sizeof" <> parens (text "struct" <+> gen env n))

comma' x                            = if isEmpty x then empty else comma <+> x

genDotCall env ts dec e@(Var _ x) n p
  | NClass{} <- info, Just _ <- dec = dotCast env False ts e n (methodtable' env x <> text "." <> gen env n) <> parens (gen env p)
  | NClass{} <- info                = genEnter env ts (eDot e n) callKW p       -- In case n is a closure...
  where info                        = findQName x env
genDotCall env ts dec e n p
  | Just NoDec <- dec               = genEnter env ts e n p
  | Just Static <- dec              = dotCast env False ts e n (gen env e <> text "->" <> gen env classKW <> text "->") <> gen env n <> parens (gen env p)
  | otherwise                       = genEnter env ts (eDot e n) callKW p


genDot env ts e@(Var _ x) n
  | NClass{} <- findQName x env     = dotCast env False ts e n $ methodtable' env x <> text "." <> gen env n
genDot env ts e n                   = dotCast env False ts e n $ gen env e <> text "->" <> gen env n
-- NOTE: all method references are eta-expanded by the lambda-lifter at this point, so n cannot be a method (i.e., require methodtable lookup) here


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
--adjust TFun{} _ e                   = e
--adjust _ TFun{} e                   = e
adjust t t' e                       = typecast t t' e

genExp env t' e                     = gen env (adjust t t' e')
  where (t, e')                     = qType env adjust e

genExp' env e                       = gen env e'
  where (t, e')                     = qType env adjust e

instance Gen Expr where
    gen env (Var _ (NoQ n))
      | n `elem` global env         = genTopName env n
      | isAlias n env               = genTopName env n
    gen env (Var _ n)
      | NClass{} <- findQName n env = newcon' env n
      | otherwise                   = gen env n
    gen env (Int _ _ str)           = gen env primToInt <> parens (text str)
    gen env (Float _ _ str)         = gen env primToFloat <> parens (text str)
    gen env (Bool _ True)           = gen env primTrue
    gen env (Bool _ False)          = gen env primFalse
    gen env (None _)                = gen env primNone
    gen env e@Strings{}             = gen env primToStr <> parens (genStr env e)
    gen env e@BStrings{}            = gen env primToBytearray <> parens (genStr env e)
    gen env (Call _ e p _)          = genCall env [] e p
    gen env (TApp _ e ts)           = genInst env ts e
    gen env (IsInstance _ e c)      = gen env primISINSTANCE <> parens (gen env e <> comma <+> gen env c)
    gen env (Dot _ e n)             = genDot env [] e n
    gen env (DotI _ e i)            = gen env e <> text "->" <> gen env componentsKW <> brackets (pretty i)
    gen env (RestI _ e i)           = gen env eNone <> semi <+> text "// CodeGen for tuple tail not implemented"
    gen env (Tuple _ p KwdNil)      = gen env primNEWTUPLE <> parens (text (show $ nargs p) <> comma' (gen env p))
    gen env (List _ es)
      | null es                     = newcon' env n <> parens (text "NULL" <> comma <+> text "NULL")
      | otherwise                   = parens (lbrace <+> (
                                        gen env n <+> tmp <+> equals <+> newcon' env n <> parens (text "NULL" <> comma <+> text "NULL") <> semi $+$
                                        vcat [ append <> parens (pars e) <> semi | e <- es ] $+$
                                        tmp <> semi) <+> rbrace)
      where n                       = qnList
            tmp                     = gen env tmpV
            w                       = gen env primWSequenceList
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

genStr env s                        = doubleQuotes $ text $ tail $ init $ concat $ sval s

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
    gen env (TTuple _ pos _)        = gen env primTuple
    gen env (TOpt _ t)              = gen env t
    gen env (TNone _)               = gen env primNoneType
    gen env (TWild _)               = word
    gen env (TRow _ _ _ t TNil{})   = gen env t
    gen env (TRow _ _ _ t r)        = gen env t <> comma <+> gen env r
    gen env (TNil _ _)              = empty

