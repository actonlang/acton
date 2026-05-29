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
import qualified Data.HashSet as HashSet
import qualified Data.List
import qualified Acton.Env
import Utils
import Pretty
import Acton.Syntax
import Acton.Names
import Acton.Builtin
import Acton.Printer
import Acton.Prim
import Acton.NameInfo
import Acton.Env
import Acton.QuickType
import Acton.Subst
import qualified Acton.Boxing as B
import Control.Monad.State.Lazy
import Prelude hiding ((<>))
import System.FilePath.Posix
import Numeric
-- For fast SrcLoc offset->line lookup when emitting #line
import qualified Data.IntMap.Strict as IM

generate                            :: Acton.Env.Env0 -> FilePath -> String -> Bool -> Module -> String -> IO (String,String,String)
generate env srcbase srcText emitLines m hash = do return (n, h, c)

  where n                           = concat (Data.List.intersperse "." (modPath (modname m))) --render $ quotes $ gen env0 (modname m)
        hashComment                 = text "/* Acton impl hash:" <+> text hash <+> text "*/"
        h                           = render $ hashComment $+$ hModule env0 m
        c                           = render $ hashComment $+$ cModule env0 srcbase srcText emitLines m
        env0                        = genEnv $ setMod (modname m) env

genRoot                            :: Acton.Env.Env0 -> QName -> IO String
genRoot env0 qn@(GName m n)         = do return $ render (cInclude $+$ cIncludeMods $+$ cInit $+$ cRoot)
  where env                         = genEnv $ setMod m env0
        cInclude                    = text "#include \"rts/common.h\""
        cIncludeMods                = include env "out/types" m
        cInit                       = (text "void" <+> gen env primROOTINIT <+> parens empty <+> char '{') $+$
                                       nest 4 (gen env (GName m initKW) <> parens empty <> semi) $+$
                                       char '}'
        cRoot                       = (gen env tActor <+> gen env primROOT <+> parens empty <+> char '{') $+$
                                       nest 4 (text "return" <+> parens (gen env tActor) <> gen env primNEWACTOR <> parens (gen env qn) <> semi) $+$
                                       char '}'




myPretty (GName m n)
      | m == mBuiltin               = text ("B_" ++ nstr n)
      | otherwise                   = pretty m <> dot <> pretty n
myPretty (NoQ w@(Internal _ _ _))   = pretty w

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

genEnv env0                         = setX env0 GenX{ globalX = HashSet.empty,
                                                      localX = HashSet.empty,
                                                      retX = tNone, volVarsX = [], lineEmitX = Nothing }

type GenEnv                         = EnvF GenX

data GenX                           = GenX { globalX :: HashSet.HashSet Name
                                           , localX :: HashSet.HashSet Name
                                           , retX :: Type
                                           , volVarsX :: [Name]
                                           , lineEmitX :: Maybe (SrcLoc -> Doc)
                                           }

gdefine te env                      = modX env1 $ \x -> x{ globalX = foldr HashSet.insert (globalX x) (dom te) }
  where env1                        = define te env

ldefine te env                      = modX env1 $ \x -> x{ localX = foldr HashSet.insert (localX x) (dom te) }
  where env1                        = define te env

classdefine stmts env               = gdefine [ (n,i) | (n,i@NClass{}) <- envOf stmts ] env

setRet t env                        = modX env $ \x -> x{ retX = t }

isGlobal env n                      = n `HashSet.member` globalX x && not (n `HashSet.member` localX x)
  where x                           = envX env

isDefined env n                     = n `HashSet.member` globalX x || n `HashSet.member` localX x
  where x                           = envX env

excludeDefined env te               = filter (not . isDefined env . fst) te

filterDefined env ns                = filter (isDefined env) ns

ret env                             = retX $ envX env

setVolVars as env                   = modX env $ \x -> x{ volVarsX = as }

isVolVar a env                      = a `elem` volVarsX (envX env)

localDefined env                    = localX (envX env)

-- Line emission helpers
setLineEmit :: (SrcLoc -> Doc) -> GenEnv -> GenEnv
setLineEmit f env                   = modX env $ \x -> x{ lineEmitX = Just f }

getLineEmit :: GenEnv -> (SrcLoc -> Doc)
getLineEmit env                     = case lineEmitX (envX env) of
                                        Just f  -> f
                                        Nothing -> const empty


-- Helpers ------------------------------------------------------------------------------------------

include                             :: GenEnv -> String -> ModName -> Doc
include env dir m                   = text "#include" <+> doubleQuotes (text (joinPath $ dir : modPath m) <> text ".h")

modNames (Import _ ms : is)         = [ m | ModuleItem m _ <- ms ] ++ modNames is
modNames (FromImport _ (ModRef (0,Just m)) _ : is)
                                    = m : modNames is
modNames (FromImportAll _ (ModRef (0,Just m)) : is)
                                    = m : modNames is
modNames []                         = []

tnm env (Derived _ (Derived _ n))           
    | B.isUnboxable (tCon0 (gBuiltin n) []) = gen env (tCon0 (gBuiltin n) [])
tnm env (Derived _ n)           
    | B.isUnboxable (tCon0 (gBuiltin n) []) = gen env (tCon0 (gBuiltin n) [])
tnm _  _                            = word
        

settype env True t
    | B.isUnboxable t        = text (unboxed_c_type t)
settype env b t              = gen env t   

-- Header -------------------------------------------------------------------------------------------

hModule env (Module m imps _ stmts) = text "#pragma" <+> text "once" $+$
                                      (if inBuiltin env
                                       then empty
                                       else text "#include \"builtin/builtin.h\"" $+$ -- TODO: can we include out/types/__builtin__.h instead?
                                            include env "rts" (modName ["rts"])) $+$
                                      vcat (map (include env "out/types") $ modNames imps) $+$
                                      hSuite 1 env1 stmts $+$
                                      hSuite 2 env1 stmts $+$
                                      text "void" <+> genTopName env initKW <+> parens empty <> semi
  where env1                        = classdefine stmts env


hSuite phase env []                 = empty
hSuite phase env (s:ss)             = hStmt phase env s $+$ hSuite phase (gdefine (envOf s) env) ss

hStmt 1 env (Decl _ ds)             = vmap (declstub env1) ds $+$
                                      vmap (typedef env1) ds $+$
                                      vmap (decl env1) ds $+$
                                      vmap (methstub env1) ds
  where env1                        = gdefine (envOf ds) env
hStmt 2 env s                       = vcat [ text "extern" <+> genTypeDecl env n t <+> genTopName env n <> semi | (n,NVar t) <- envOf s]
hStmt _ env s                       = empty

declstub env (Class _ n q a b ddoc) = text "struct" <+> genTopName env n <> semi
declstub env Def{}                  = empty

typedef env (Class _ n q a b ddoc)  = text "typedef" <+> text "struct" <+> genTopName env n <+> char '*' <> genTopName env n <> semi
typedef env Def{}                   = empty

decl env (Class _ n q a b ddoc)     = (text "struct" <+> classname env n <+> char '{') $+$
                                      nest 4 (vcat $ stdprefix env ++ initdef : serialize env tc : deserialize env tc : meths) $+$
                                      char '}' <> semi $+$
                                      inst_struct
  where tc                          = TC (NoQ n) [ tVar v | QBind v _ <- q ]
        env1                        = setGtypes env (findAttrSchemas env1 (NoQ n))
        initdef : meths             = fields (setInClass env1) tc
        properties                  = [ varsig env n (sctype sc) <> semi | (n, NSig sc Property _) <- fullAttrEnv env tc ]
        inst_struct | initNotImpl   = empty
                    | otherwise     = (text "struct" <+> genTopName env n <+> char '{') $+$
                                      nest 4 (classlink env n $+$ vcat properties) $+$
                                      char '}' <> semi
        initNotImpl                 = any hasNotImpl [ b' | Decl _ ds <- b, Def{dname=n',dbody=b'} <- ds, n' == initKW ]
decl env (Def _ n q p _ (Just t) _ _ fx ddoc)
                                    = genTypeDecl env n (exposeMsg fx t) <+> genTopName env n <+> parens (uparams env $ prowOf p) <> semi
methstub env (Class _ n q a b ddoc) = text "extern" <+> text "struct" <+> classname env n <+> methodtable env n <> semi $+$
                                      constub env t n r b
  where TFun _ _ r _ t              = sctype $ fst $ schemaOf env (eVar n)
methstub env Def{}                  = empty

constub env t n r b
  | null ns || hasNotImpl b         = utype env t <+> newcon env n <> parens (uparams env r) <> semi
  | otherwise                       = empty
  where ns                          = abstractAttrs env (NoQ n)

fields env c                        = map field (vsubst [(tvSelf,tCon c)] te)
  where te                          = fullAttrEnv env c
        field (n, NDef sc Static _) = funsig env n (sctype sc) <> semi
        field (n, NDef sc NoDec _)  
          | n == initKW             = methsig env c n (sctype sc) <> semi
          | otherwise               = methsig2 env c (Just n) (B.rtypeOf env c n) <> semi
        field (n, NVar t)           = varsig env n t <> semi
        field (n, NSig sc Static _) = funsig env n (sctype sc) <> semi
        field (n, NSig sc NoDec _)  = methsig2 env c (Just n) (B.rtypeOf env c n) <> semi
        field (n, NSig sc Property _)
                                    = empty

funsig env n (TFun _ _ r _ t)       = utype env t <+> parens (char '*' <> gen env n) <+> parens (uparams env r)
funsig env n t                      = varsig env n t

funsig2 :: GenEnv -> Maybe Name -> Type -> Doc
funsig2 env mbn (TFun _ fx p _ t)   = utype env (exposeMsg fx t) <+> parens (char '*' <> maybe empty (gen env) mbn) <+> parens (uparams env p)

methsig env c n (TFun _ fx r _ t)   = utype env (exposeMsg fx t) <+> parens (char '*' <> gen env n) <+> parens (uparams env $ posRow (tCon c) r)
methsig env c n t                   = varsig env n t

methsig2 :: GenEnv -> TCon -> Maybe Name -> Type -> Doc
methsig2 env c mbn (TFun _ fx p _ t)
                                    = (if fx==fxAction then text "B_Msg" else gen env t) <+> parens (char '*' <> maybe empty (gen env) mbn) <+> parens (gen env (posRow (tCon c) p))         

{-
params env (TNil _ _)               = empty
params env (TRow _ _ _ t r@TRow{})  = gen env t <> comma <+> params env r
params env (TRow _ _ _ t TNil{})    = gen env t
params env (TRow _ _ _ t TVar{})    = gen env t                                         -- Ignore param tails for now...
params env t                        = error ("codegen unexpected row: " ++ prstr t)
-}

uparams env (TNil _ _)               = empty
uparams env (TRow _ _ _ t r@TRow{})  = utype env t <> comma <+> uparams env r
uparams env (TRow _ _ _ t TNil{})    = utype env t
uparams env (TRow _ _ _ t TVar{})    = utype env t                                         -- Ignore param tails for now...
uparams env t@TVar{}                 = gen env t
uparams env t                        = error ("codegen unexpected row: " ++ prstr t)

utype env t
 | B.isUnboxable t                  = text (unboxed_c_type t)
 | otherwise                        = gen env t

exposeMsg fx t                      = if fx == fxAction then tMsg t else t

exposeMsg' t@TFun{}                 = t{ restype = exposeMsg (effect t) (restype t) }
exposeMsg' t                        = t

varsig env n t                      = utype env t <+> gen env n

stdprefix env                       = [gcinfo env, classid env, superlink env]

gcinfo env                          = text "char" <+> text "*" <> gen env gcinfoKW <> semi

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
methodtable' env n                  = gen env $ tableName (unalias env n)

tableName (GName m n)               = GName m (Derived n suffixMethods)
tableName n                         = error ("#### tableName " ++ show n)

witName (GName m n)                 = GName m (Derived n suffixWitness)
witName n                           = error ("#### witName " ++ show n)

newcon env n                        = gen env (conName $ gname env n)

newcon' env (NoQ n)                 = newcon env n
newcon' env n                       = gen env $ conName $ unalias env n

conName (GName m n)                 = GName m (Derived n suffixNew)

serializeSup env c                  = methodtable' env c <> dot <> gen env serializeKW
deserializeSup env c                = methodtable' env c <> dot <> gen env deserializeKW

classKW                             = primKW "class"
gcinfoKW                            = primKW "GCINFO"
classidKW                           = primKW "class_id"
superclassKW                        = primKW "superclass"
componentsKW                        = name "components"

primAND                             = gPrim "AND"
primOR                              = gPrim "OR"
primNOT                             = gPrim "NOT"
primROOT                            = gPrim "ROOT"
primROOTINIT                        = gPrim "ROOTINIT"
primRegister                        = gPrim "register"

primToInt                           = name "toB_int"
primToU64                           = name "toB_u64"
primToBigInt                        = name "toB_bigint"
primToBigInt2                       = name "toB_bigint2"
primToFloat                         = name "to$float"
primToStr                           = name "to$str"
primToBytearray                     = name "to$bytearray"
primToBytes                         = Derived (name "to$bytes") (name "len")

tmpV                                = primKW "tmp"

tSerialstate                        = tCon $ TC (GName mPrim (name "Serial$state")) []
primStepSerialize                   = gPrim "step_serialize"
primStepDeserialize                 = gPrim "step_deserialize"
primValSerialize                    = gPrim "val_serialize"
primValDeserialize                  = gPrim "val_deserialize"
primDNEW                            = gPrim "DNEW"
primNEWTUPLE                        = gPrim "NEWTUPLE"
primNEWTUPLE0                       = gPrim "NEWTUPLE0"


-- Implementation -----------------------------------------------------------------------------------

cModule env srcbase srcText emitLines (Module m imps _ stmts)
                                    = (if inBuiltin env then text "#include \"builtin/builtin.c\"" else empty) $+$
                                      text "#include \"rts/common.h\"" $+$
                                      include env (if inBuiltin env then "" else "out/types") m $+$
                                      ext_include $+$
                                      declModule envWithLine stmts $+$
                                      text "int" <+> genTopName env initFlag <+> equals <+> text "0" <> semi $+$
                                      (text "void" <+> genTopName env initKW <+> parens empty <+> char '{') $+$
                                      nest 4 (text "if" <+> parens (genTopName env initFlag) <+> text "return" <> semi $+$
                                              genTopName env initFlag <+> equals <+> text "1" <> semi $+$
                                              ext_init $+$
                                              initImports $+$
                                              initTables env1 stmts $+$
                                              initGlobals env1 stmts) $+$
                                      char '}'
  where initImports                 = vcat [ gen env (GName m initKW) <> parens empty <> semi | m <- modNames imps ]
        external                    = notImpl && not (inBuiltin env)
        ext_include                 = if notImpl then text "#include" <+> doubleQuotes (text srcbase <> text ".ext.c") else empty
        ext_init                    = if notImpl then genTopName env (name "__ext_init__") <+> parens empty <> semi else empty
        notImpl                     = hasNotImpl stmts
        env1                        = classdefine stmts env
        -- Emit a C #line directive for a given Acton SrcLoc if available
        actFile                     = srcbase ++ ".act"
        -- Our AST source locations (SrcLoc) carry byte offsets (start,end), not (row,col).
        -- C's #line requires a 1-based line number, so we convert offsets -> line numbers.
        -- We precompute line start offsets and use an IntMap + lookupLE for O(log n) mapping.
        lineStarts :: [Int]
        lineStarts                  = 0 : [ i+1 | (i,c) <- zip ([0..] :: [Int]) srcText, c == '\n' ]
        lineMap    :: IM.IntMap Int
        lineMap                     = IM.fromList (zip lineStarts ([1..] :: [Int]))
        offsetToLine :: Int -> Int
        offsetToLine off            = case IM.lookupLE off lineMap of
                                         Just (_, ln) -> ln
                                         Nothing      -> 1
        emitLine NoLoc              = empty
        emitLine (Loc startOffset _) =
            text "#line" <+> pretty (offsetToLine startOffset) <+> doubleQuotes (text actFile)
        envWithLine                 = if emitLines then setLineEmit emitLine env1 else env1


declModule env []                   = empty
declModule env (Decl _ ds : ss)     = vcat [ declDecl env1 d | d <- ds ] $+$
                                      declModule env1 ss
  where env1                        = gdefine (envOf ds) env
declModule env (Signature{} : ss)   = declModule env ss
declModule env (s : ss)             = vcat [ genTypeDecl env n t <+> genTopName env n <> semi | (n,NVar t) <- te ] $+$
                                      declModule env1 ss
  where te                          = excludeDefined env (envOf s)
        env1                        = gdefine te env


genPosPar env n d t p
    | not (contextIs env CtxClass) || d == Static
                                    = match p (posrow t)
    | isInit n                      = gen env p
    | p1 == PosNIL                  = genTypeDecl env x (fromJust y) <+> gen env x
    | otherwise                     = genTypeDecl env x (fromJust y) <+> gen env x <> comma <+> match p1 (posrow t)
    where PosPar x y z p1           = p
          match (PosPar n (Just t) Nothing PosNIL) (TRow _ _ _ t' _)
                                    = settype env (rawParam t' t) t <+> gen env n
          match (PosPar n (Just t) Nothing r) (TRow _ _ _ t' tl)
                                    = settype env (rawParam t' t) t <+> gen env n <> comma <+> match r tl
          match PosNIL (TNil _ _)   = empty
          match p TVar{}            = gen env p
          match p t                 = error ("Internal error CodeGen.genPosPar: n = "++show n++", p = "++show p++", t ="++show t)
          rawParam _ TUnboxed{}     = True
          rawParam _ _              = False
          isInit (Derived _ n)      = n == initKW 
          isInit n                  = n == initKW
        
declDecl env (Def dloc n q p KwdNIL (Just t) b d fx ddoc)
  | hasNotImpl b                    = t3 <+> genTopName env n <+> parens (genPosPar env n d t1 p) <> semi $+$
                                      text "/*" $+$ 
                                      decl $+$
                                      text "*/"
  | otherwise                       = decl
  where methnm n@(Derived c n0)
          |n0 == suffixNewact        = n
          |otherwise                = n0
        methnm n0                   = n0
        t1                          = case methodType n of
                                         Just t' -> t'
                                         Nothing -> error "Internal error: CodeGen.declDecl"
        methodType n@(Derived c n0)
          | contextIs env CtxClass,
            NClass q _ _ _ <- findQName (NoQ c) env
                                    = Just $ B.rtypeOf env (TC (NoQ c) (map tVar $ qbound q)) n0
        methodType n                = B.generalType env (methnm n)
        (ss',vs)                    = genSuite env1 b
        decl                        = emit dloc $+$
                                      t3 <+> genTopName env n <+> parens (genPosPar (setVolVars vs env) n d t1 p) <+> char '{' $+$
                                      nest 4 ss' $+$ 
                                      char '}'
        env1                        = setRet t2 $ ldefine (envOf p) $ defineTVars q env
        t2                          = exposeMsg fx t
        t3                          = (if isVolVar n env then text "volatile" else empty) <+> settype env (rawReturn (restype t1)) t2
        rawReturn TUnboxed{}        = True
        rawReturn _                 = False
        emit                        = getLineEmit env

declDecl env (Class _ n q as b ddoc)
    | cDefinedClass                 = vcat [ declDecl env2 d{ dname = methodname n (dname d) } | Decl _ ds <- b', d@Def{} <- ds ] $+$
                                      text "struct" <+> classname env n <+> methodtable env n <> semi
    | otherwise                     = vcat [ declDecl env2 d{ dname = methodname n (dname d) } | Decl _ ds <- b', d@Def{} <- ds ] $+$
                                      declSerialize env1 n c props sup_c $+$
                                      declDeserialize env1 n c props sup_c $+$
                                      declCleanup env1 n sup_c $+$
                                      declCon env1 n q b $+$
                                      text "struct" <+> classname env n <+> methodtable env n <> semi
  where b'                          = vsubst [(tvSelf, tCon c)] b
        c                           = TC (NoQ n) (map tVar $ qbound q)
        env1                        = setInClass (defineTVars q env)
        env2                        = setGtypes env1 (findAttrSchemas env1 (NoQ n))
        props                       = [ (n,sctype sc) | (n, NSig sc Property _) <- fullAttrEnv env c ]
        sup_c                       = filter ((`elem` special_repr) . tcname) as
        special_repr                = [primActor] -- To be extended...
        cDefinedClass               = inBuiltin env && any hasNotImpl [b' | Decl _ ds <- b, Def{dname=n',dbody=b'} <- ds, n' == initKW ]

declCleanup env n sup_c
  -- TODO: only match if this is an actor, or even better if this actor has a __cleanup__ method defined (not empty!?)
  | not (null sup_c)                = -- Only for actors
                                      text "void" <+> genTopName env (methodname n attr_finalizer) <+> parens (text "void *obj, void *cdata") <+> char '{' $+$
                                      -- t_cleanupQ_Foo self = (t_cleanupQ_Foo)obj;
                                      -- self->$class->__cleanup__(self);
                                      nest 4 ((genTopName env n) <+> gen env self <+> equals <+> parens (genTopName env n) <> text "obj" <> semi $+$
                                              gen env self <> text "->" <> gen env classKW <> text "->" <> gen env cleanupKW <> parens (gen env self) <> semi) $+$
                                      char '}'
  | otherwise                       = empty
  where self                        = name "self"

declSerialize env n c props sup_c   = (text "void" <+> genTopName env (methodname n serializeKW) <+> parens (gen env pars) <+> char '{') $+$
                                      nest 4 (super_step $+$ vcat [ step i | i <- props, fst i `notElem`super_attrs ]) $+$
                                      char '}'
  where pars                        = PosPar self (Just $ tCon c) Nothing $ PosPar st (Just tSerialstate) Nothing PosNIL
        st                          = name "state"
        self                        = name "self"
        super_step | [c] <- sup_c   = serializeSup env (tcname c) <> parens (parens (gen env $ tcname c) <> gen env self <> comma <+> gen env st) <> semi
                   | otherwise      = empty
        super_attrs                 = [ i | c <- sup_c, i <- conAttrs env (tcname c) ]
        step (i,t)
          | B.isUnboxable t         = gen env primValSerialize <> parens (text (class_id t) <> comma <+> text "&" <> gen env self <> text "->" <> gen env i <> comma <+> gen env st) <> semi
          | otherwise               = gen env primStepSerialize <> parens (gen env self <> text "->" <> gen env i <> comma <+> gen env st) <> semi 

declDeserialize env n c props sup_c = (gen env (tCon c) <+> genTopName env (methodname n deserializeKW) <+> parens (gen env pars) <+> char '{') $+$
                                      nest 4 (word <+> gen env tmpV <> semi $+$ optcreate $+$ super_step $+$ vcat [ step i | i <- props, fst i `notElem` super_attrs ] $+$ ret) $+$
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
        alloc                       = gen env self <+> equals <+> acton_malloc env (gname env n) <> semi $+$
                                      gen env self <> text "->" <> gen env1 classKW <+> equals <+> char '&' <> methodtable env1 n <> semi
        super_step | [c] <- sup_c   = deserializeSup env (tcname c) <> parens (parens (gen env $ tcname c) <> gen env self <> comma <+> gen env st) <> semi
                   | otherwise      = empty
        super_attrs                 = [ i | c <- sup_c, i <- conAttrs env (tcname c) ]
        step (i,t)
           | B.isUnboxable t        = gen env tmpV <+> text "=" <+> gen env primValDeserialize <> parens (gen env st) <> semi $+$
                                      text "memcpy" <> parens (text "&"<>gen env tmpV <> comma <+> text "&" <> gen env self <> text "->" <> gen env i <> comma <+> text "sizeof" <> parens(word)) <> semi
           | otherwise              = gen env self <> text "->" <> gen env i <+> text "=" <+> gen env primStepDeserialize <> parens (gen env st) <> semi
        ret                         = text "return" <+> gen env self <> semi


initTables env []                   = empty
initTables env (Decl _ ds : ss)     = vcat [ char '{' $+$ nest 4 (initClassBase env1 n q as hC $+$ initClass env1 n b hC) $+$ char '}' | Class _ n q as b ddoc <- ds, let hC = hasCDef b ] $+$
                                      initTables env1 ss
  where env1                        = gdefine (envOf ds) env
        hasCDef b                   = inBuiltin env && any hasNotImpl [b' | Decl _ ds <- b, Def{dname=n',dbody=b'} <- ds, n' == initKW ]

initTables env (s : ss)             = initTables env ss


initGlobals env []                  = empty
initGlobals env (Decl _ ds : ss)    = initGlobals env1 ss
  where env1                        = gdefine (envOf ds) env
initGlobals env (Signature{} : ss)  = initGlobals env ss
initGlobals env (s : ss)            = genStmt1 env s $+$
                                      vcat [ genTopName env n <+> equals <+> gen env n <> semi | (n,_) <- te ] $+$
                                      initGlobals env1 ss
  where te                          = excludeDefined env (envOf s)
        env1                        = gdefine te env

initClassBase env c q as hasCDef    = methodtable env c <> dot <> gen env gcinfoKW <+> equals <+> doubleQuotes (genTopName env c) <> semi $+$
                                      methodtable env c <> dot <> gen env superclassKW <+> equals <+> super <> semi $+$
                                      vcat [ inherit c' n | (c',n) <- inheritedAttrs env (NoQ c) ]
  where tc                          =  TC (NoQ c) [ tVar v | QBind v _ <- q ]
        super                       = if null as then text "NULL" else parens (gen env qnSuperClass) <> text "&" <> methodtable' env (tcname $ head as)
        selfsubst                   = selfSubst (NoQ c) q
        inherit c' n
          | hasCDef                 = methodtable env c <> dot <> gen env n <+> equals <+> genTopName env (methodname c n) <> semi
          | otherwise               = methodtable env c <> dot <> gen env n <+> equals <+> cast tc n (fromJust $ lookup n te) (fromJust $ lookup n te') <> methodtable' env c' <> dot <> gen env n <> semi
        cast tc n (NSig sc dec _) (NDef sc' _ _)
          | dec == Static           = parens (funsig2 env Nothing mt)
          | otherwise               = parens (methsig2 env tc Nothing mt)
          where mt                  = B.matchTypes (selfsubst $ sctype sc) (selfsubst $ sctype sc')
        cast tc n (NDef sc dec _) (NDef sc' _ _)
          | dec == Static           = parens (funsig2 env Nothing mt)
          | otherwise               = parens (methsig2 env tc Nothing mt)
          where mt                  = B.matchTypes (selfsubst $ sctype sc) (selfsubst $ sctype sc')
        cast tc n (NSig sc dec _) (NSig sc' _ _)
          | dec == Static           = parens (funsig2 env Nothing mt)
          | otherwise               = parens (methsig2 env tc Nothing mt)
          where mt                  = B.matchTypes (selfsubst $ sctype sc) (selfsubst $ sctype sc')
        cast tc n (NDef sc dec _) (NSig sc' _ _)
          | dec == Static           = parens (funsig2 env Nothing mt)
          | otherwise               = parens (methsig2 env tc Nothing mt)
          where mt                  = B.matchTypes (selfsubst $ sctype sc) (selfsubst $ sctype sc')
        cast _ _ (NVar t) _         = parens (gen env $ selfsubst t)
        te                          = fullAttrEnv env tc
        te'                         = findAttrSchemas env (NoQ c)

initClass env c [] hasCDef          = vcat [ methodtable env c <> dot <> gen env n <+> equals <+> genTopName env (methodname c n) <> semi | n <- [serializeKW,deserializeKW] ] $+$
                                      if hasCDef then empty else gen env primRegister <> parens (char '&' <> methodtable env c) <> semi
initClass env c (Decl _ ds : ss) b  = vcat [ methodtable env c <> dot <> gen env n <+> equals <+> genTopName env (methodname c n) <> semi | Def{dname=n} <- ds ] $+$
                                      initClass env1 c ss b
  where env1                        = gdefine (envOf ds) env
initClass env c (Signature{} : ss) b = initClass env c ss b
initClass env c (s : ss) b
  | isNotImpl s                     = initClass env c ss b
  | otherwise                       = genStmt1 env s $+$
                                      vcat [ genTopName env c <> dot <> gen env n <+> equals <+> gen env n <> semi | (n,_) <- te ] $+$
                                      initClass env1 c ss b
  where te                          = excludeDefined env (envOf s)
        env1                        = ldefine te env


initFlag                            = name "done$"

methodname c n                      = Derived c n

class Gen a where
    gen                             :: GenEnv -> a -> Doc
    genV                            :: GenEnv -> a -> (Doc,[Name])

    genV env x                      = (gen env x,[])
    gen env x                       = fst(genV env x)

instance (Gen a) => Gen (Maybe a) where
    gen env x                       = maybe empty (gen env) x
    genV env x                      = maybe (empty,[]) (genV env) x

instance Gen ModName where
    gen env (ModName ns)            = hcat $ punctuate (text "Q_") $ map (gen env) ns

instance Gen QName where
    gen env (GName m n)
      | m == mPrim                  = char '$' <> text (rawstr n)
      | m == mBuiltin               = text "B_" <> text (nstr n)
      | otherwise                   = gen env m <> text "Q_" <> text (mkCident $ nstr n)
    gen env (NoQ n)                 = gen env n
    gen env n@QName{}
--      | n `elem` B.mathfuns         = text (nstr (noq n))
      | otherwise                   = gen env (unalias env n)

instance Gen Name where
    gen env nm                      = text $ unCkeyword $ mkCident $ nstr nm

genTopName env n                    = gen env (gname env n)

genQName env (NoQ n)
  | isGlobal env n                  = genTopName env n
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
                                        "alignas",
                                        "alignof",
                                        "auto",
                                        "bool",
                                        "break",
                                        "case",
                                        "char",
                                        "const",
                                        "constexpr",
                                        "continue",
                                        "default",
                                        "do",
                                        "double",
                                        "else",
                                        "enum",
                                        "extern",
                                        "false",
                                        "float",
                                        "for",
                                        "goto",
                                        "if",
                                        "inline",
                                        "int",
                                        "long",
                                        "nullptr",
                                        "register",
                                        "restrict",
                                        "return",
                                        "short",
                                        "signed",
                                        "sizeof",
                                        "static",
                                        "static_assert",
                                        "struct",
                                        "switch",
                                        "thread_local",
                                        "true",
                                        "typedef",
                                        "typeof",
                                        "typeof_unequal",
                                        "union",
                                        "unsigned",
                                        "void",
                                        "volatile",
                                        "while",
                                        "_Alignas",
                                        "_Alignof",
                                        "_Atomic",
                                        "_BitInt",
                                        "_Bool",
                                        "_Complex",
                                        "_Decimal128",
                                        "_Decimal32",
                                        "_Decimal64",
                                        "_Generic",
                                        "_Imaginary",
                                        "_Noreturn",
                                        "_Static_assert",
                                        "_Thread_local"
                                      ]



preEscape str                       = "A_" ++ str

word                                = text "$WORD"

genSuite :: GenEnv -> Suite -> (Doc,[Name])
genSuite env []                     = (empty,[])
genSuite env (s:ss)                 = ((emit (sloc s) $+$ c) $+$ cs, vs' ++ filterDefined env vs)
    where (cs,vs)                   = genSuite (ldefine (envOf s) env) ss
          (c,vs')                   = genStmt (setVolVars vs env) s
          emit                      = getLineEmit env

genTypeDecl env n t                 =  (if isVolVar n env then text "volatile" else empty) <+> utype env t

genStmt env (Decl _ ds)             = (empty, [])
genStmt env (Assign _ [PVar _ n (Just t)] e)
  | not (isDefined env n)           = (genTypeDecl env n t <+> gen env n <+> equals <+> rhs <> semi, [])
  where rhs                         = if isWitness n
                                      then case staticWitnessName e of
                                           (Just nm,as) ->
                                               foldr (\x y -> y <> text "->" <> myPretty x) (parens (myPretty ( tcname (tcon t))) <> myPretty (witName nm)) as
                                           _  -> genExp env t e
                                      else genExp env t e
genStmt env s                       = (vcat [ genTypeDecl env n t <+> gen env n <> semi | (n,NVar t) <- te ] $+$ s', vs)
  where te                          = excludeDefined env (envOf s)
        env1                        = ldefine te env
        (s', vs)                    = genV env1 s

genStmt1 env s                      = fst $ genStmt env s

instance Gen Stmt where
    genV env s | isNotImpl s        = (text "//" <+> text "NotImplemented", [])
    genV env (Expr _ Strings{})     = (semi, [])
    genV env (Expr _ e)
      | isRAISE e                   = (genExp' env e <> semi $+$ text "__builtin_unreachable();" , [])
    genV env (Expr _ e)             = (genExp' env e <> semi, [])
    genV env (Assign _ [p] e)
        | isUnboxedRep t            = (gen env p <+> equals <+> gen env e <> semi, [])
        | otherwise                 = (gen env p <+> equals <+> genExp env t e <> semi, [])

      where t                       = typeOf env p
    genV env (AugAssign _ tg op e)
      | boxedRepType (targetType env tg) == tU1,
        Just d <- genU1RawAugOp env op tg e
                                    = (genTarget env tg <+> equals <+> d <> semi, [])
    genV env (AugAssign _ tg op e)  = (genTarget env tg <+> augPretty op <+> genExp env t e <> semi, [])
      where t                       = targetType env tg
    genV env (MutAssign _ tg e)     = (genTarget env tg <+> equals <+> genExp env t e <> semi, [])
      where t                       = targetType env tg
    genV env (Pass _)               = (empty, [])
    genV env (Return _ Nothing)     = (text "return" <+> gen env eNone <> semi, [])
    genV env (Return _ (Just e))    = (text "return" <+> genExp env (ret env) e <> semi, [])
    genV env (Break _)              = (text "break" <> semi, [])
    genV env (Continue _)           = (text "continue" <> semi, [])
    genV _ (If _ (Branch (Bool _ True) [Pass _] : _) _)
                                    = (empty, [])
    genV env (If  _ [b@(Branch e ss)] fin)
      | isPUSH e                    = (b' $+$ fin', v1 ++ v2 ++ volatiles)
      where (b',v1)                 = genBranch env "if" b
            (fin',v2)               = genElse env fin
            volatiles               = filterDefined env (bound ss)
    genV env (If _ (b:bs) b2)       = (b' $+$ vcat bs' $+$ b2', v1 ++ concat v2 ++ v3)
       where (b',v1)                = genBranch env "if" b
             (bs',v2)               = unzip (map (genBranch env "else if") bs)
             (b2',v3)               = genElse env b2
    genV env (While _ e b [])       = genBranch env "while" (Branch e b)
    genV env _                      = (empty, [])

genBranch env kw (Branch e b)       = ((text kw <+> parens(genBool env (B.unbox t e)) <+> char '{') $+$ nest 4 b' $+$ char '}', vs)
   where (b',vs)                    = genSuite env b
         t                          = typeOf env e

genElse env []                      = (empty, [])
genElse env b                       = ((text "else" <+> char '{') $+$ nest 4 b' $+$ char '}', vs)
   where (b',vs)                    = genSuite env b

instance Gen PosPar where
    gen env (PosPar n (Just t) _ PosNIL)
                                    = genTypeDecl env n t <+> gen env n
    gen env (PosPar n (Just t) _ p) = genTypeDecl env n t <+> gen env n <> comma <+> gen env p
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
          where expr                = text "fromB_int" <> parens (gen env e)
        width (n:s) p
          | n `elem` "123456789"    = let (n',s') = span (`elem` "0123456789") s in dot s' p
        width s p                   = dot s p
        dot ('.':s) p               = prec s p
        dot s p                     = len s p
        prec ('*':s) (PosArg e p)   = comma <+> parens (text "int") <> expr <> len s p
          where expr                = text "fromB_int" <> parens (gen env e)  --parens (parens (gen env tInt) <> gen env e) <> text "->val"
        prec (n:s) p
          | n `elem` "0123456789"   = let (n',s') = span (`elem` "0123456789") s in len s' p
        prec s p                    = len s p
        len (l:s) p
          | l `elem` "hlL"          = conv s p
        len s p                     = conv s p
        conv (t:s) (PosArg e p)
          | t `elem` "diouxXc"      = comma <+> expr <> format s p
          where expr                = text "fromB_int" <> parens (gen env e) --parens (parens (gen env tInt) <> gen env e) <> text "->val"
        conv (t:s) (PosArg e p)
          | t `elem` "eEfFgG"       = comma <+> expr <> format s p
          where expr                = parens (parens (gen env tFloat) <> gen env e) <> text "->val"
        conv (t:s) (PosArg e p)
          | t `elem` "rsa"          = comma <+> expr <> format s p
          where expr                = parens (parens (gen env tStr) <> gen env e) <> text "->str"
        conv ('%':s) p              = format s p

-- Helpers for choosing the C representation at call boundaries.
--
-- Boxing decides where Acton values are represented raw in the IR, but CodeGen
-- still has to match the C ABI of the callee.  The predicates below describe
-- the representation generated by an expression, not just its Acton type.

-- Render normal call arguments against the formal positional row.  Only
-- parameters whose formal type is TUnboxed are forced to raw C values.
genCallPosArgs env (TRow _ _ _ t r) (PosArg e PosNil)
                                    = genCallArg env t e
genCallPosArgs env (TRow _ _ _ t r) (PosArg e p)
                                    = genCallArg env t e <> comma <+> genCallPosArgs env r p
genCallPosArgs env _ p              = gen env p

-- Render constructor arguments.  Constructor C signatures use raw values for
-- unboxable Acton types even when the source-level parameter type is boxed.
genUCallPosArgs env (TRow _ _ _ t r) (PosArg e PosNil)
                                    = genUCallArg env t e
genUCallPosArgs env (TRow _ _ _ t r) (PosArg e p)
                                    = genUCallArg env t e <> comma <+> genUCallPosArgs env r p
genUCallPosArgs env _ p             = gen env p

-- Render one normal call argument.  A TUnboxed formal parameter must receive a
-- raw C value; if the actual expression is not already raw, emit an UnBox.
genCallArg env TUnboxed{} e@UnBox{} = gen env e
genCallArg env (TUnboxed _ t) (Box _ e)
  | boxedExpr env e                 = gen env (UnBox t e)
  | otherwise                       = gen env e
genCallArg env (TUnboxed _ t) e
  | rawExpr env e                   = gen env e
  | otherwise                       = gen env (B.unbox t e)
genCallArg env _ e                  = gen env e

-- Render one constructor argument in the representation used by generated
-- constructors: raw for unboxable values, normal boxed generation otherwise.
genUCallArg env t e
  | B.isUnboxable t'                = genRawExpr env e
  | otherwise                       = gen env e
  where t'                          = boxedRepType t

-- Generate an expression as raw C when its boxed representation is unboxable.
-- Already-raw expressions are left alone; boxed values are unboxed here.
genRawExpr env e
  | rawExpr env e                   = gen env e
  | B.isUnboxable t                 = gen env (B.unbox t e)
  | otherwise                       = gen env e
  where t                           = boxedRepType (typeOf env e)

-- Generate a raw C expression for a known target representation.  This is
-- needed when the expression's own source type is boxed or optional, but the
-- surrounding context has established that the C value must be raw.
genRawExprAs env t e
  | rawExpr env e                   = gen env e
genRawExprAs env t (Box _ e)
  | boxedExpr env e                 = gen env (B.unbox t' e)
  | otherwise                       = genRawExprAs env t' e
  where t'                          = boxedRepType t
genRawExprAs env t e                = gen env (B.unbox t' e)
  where t'                          = boxedRepType t

-- True when gen will already produce the raw C representation for this
-- expression.  Operations are raw only when their operands/branches are raw.
rawExpr env UnBox{}                 = True
rawExpr env (Var _ n)               = unboxedVar env n
rawExpr env (Dot _ e n)             = unboxedField env e n
rawExpr env (Paren _ e)             = rawExpr env e
rawExpr env c@(Call _ f _ KwdNil)
  | rawClassConstructor env c f     = True
  | callableReturnsRaw env f        = True
rawExpr env (BinOp _ e1 op e2)
  | op `notElem` [And, Or]          = rawExpr env e1 && rawExpr env e2
rawExpr env (UnOp _ op e)
  | op /= Not                       = rawExpr env e
rawExpr env (Cond _ e1 _ e2)        = rawExpr env e1 && rawExpr env e2
rawExpr env _                       = False

-- True when gen is known to produce a boxed Acton value.  This is used to
-- avoid double boxing and to decide whether an explicit UnBox is meaningful.
boxedExpr env (Var _ n)             = not $ unboxedVar env n
boxedExpr env (Dot _ e n)           = not $ unboxedField env e n
boxedExpr env DotI{}                = True
boxedExpr env (Paren _ e)           = boxedExpr env e
boxedExpr env Box{}                 = True
boxedExpr env (Call _ (TApp _ (Var _ n) _) _ KwdNil)
  | n == primCAST                   = True
boxedExpr env c@(Call _ f _ KwdNil)
  | rawClassConstructor env c f     = False
boxedExpr env (Call _ f _ KwdNil)   = callReturnsBoxed env f
boxedExpr env _                     = False

-- Some builtin class constructors, such as int/u64/i16, are written as class
-- calls in Acton but return the raw C value for their unboxable result type.
rawClassConstructor env c f         = callIsClass env f && B.isUnboxable (boxedRepType (typeOf env c))

-- Class calls normally return boxed objects.  Non-class calls are classified by
-- their callable return type.
callReturnsBoxed env f@(TApp _ f0 _)
  | callIsClass env f0              = True
  | otherwise                       = callableReturnsBoxed env f
callReturnsBoxed env f@(Var _ n)
  | NClass{} <- findQName n env     = True
  | otherwise                       = callableReturnsBoxed env f
callReturnsBoxed env f              = callableReturnsBoxed env f

-- Identify class constructor calls, ignoring type application wrappers.
callIsClass env (TApp _ f _)        = callIsClass env f
callIsClass env (Var _ n)           = case findQName n env of
                                        NClass{} -> True
                                        _        -> False
callIsClass env _                   = False

-- A normal callable returning an unboxable Acton type returns the boxed object
-- unless its type has already been changed to TUnboxed.
callableReturnsBoxed env f          = case B.rtypeOfFun env f of
                                        TFun _ _ _ _ t -> B.isUnboxable t
                                        _              -> False

-- A callable whose return type is explicitly TUnboxed returns raw C.
callableReturnsRaw env f            = case B.rtypeOfFun env f of
                                        TFun _ _ _ _ TUnboxed{} -> True
                                        _                       -> False

-- Compute the C-facing callable type used for argument rendering.  Public
-- polymorphic callables are matched against a wildcard instantiation so
-- polymorphic positions keep their boxed ABI; internal/generated functions and
-- selected C primitives use the fully instantiated type directly.
genCallableType env ts e@(Var _ n)
  | boxedCPrim n                    = typeInstOf env ts e
  | isInternalQName n               = B.matchTypes t t
  | otherwise                       = B.matchTypes t t0
  where t                           = typeInstOf env ts e
        t0                          = typeInstOf env (map (const tWild) ts) e
genCallableType env ts e            = typeInstOf env ts e

boxedCPrim n                        = n `elem` [primAFTER, primAFTERc, primAFTERf]

isInternalQName (NoQ n)             = isInternal n
isInternalQName (GName _ n)         = isInternal n
isInternalQName (QName _ n)         = isInternal n

genCall env [] (TApp _ e ts) p      = genCall env ts e p
genCall env [_,t] (Var _ n) (PosArg e PosNil)
  | n == primCAST                   = parens (parens (gen env t) <> gen env e)
genCall env [row] (Var _ n) (PosArg s@Strings{} (PosArg tup PosNil))
  | n == primFORMAT                 = gen env n <> parens (genStr env (formatLit s) <> castLit env s (flatten tup))
  where -- unbox (TNil _ _) p          = empty
        -- unbox (TRow _  _ _ t r) (PosArg e p)
        --  | t == tStr               = comma <+> expr <> text "->str" <> unbox r p
        --  | otherwise               = comma <+> expr <> text "->val" <> unbox r p
        --  where expr                = parens (parens (gen env t) <> gen env e)
        flatten (Tuple _ p KwdNil)  = p
        flatten e                   = foldr PosArg PosNil $ map (DotI l0 e) [0..]
genCall env [t] (Var _ n) PosNil
  | n == primNEWACTOR               = gen env n <> parens (gen env t)
-- Only install GCfinalizer if one is defined, i.e. we don't have the default
-- $ActorD___cleanup__
-- TODO: would be even better to determine this in the compiler and not emit
-- this line rather than inspect the method table at run time
genCall env [TCon _ tc] (Var _ n) p
  | n == primInstallFinalizer       = text "if" <+> parens (text "(void*)" <> gen env p <> text "->" <> gen env classKW <> text "->" <> gen env cleanupKW <+> text "!= (void*)$ActorD___cleanup__") <+> gen env n <> parens (gen env p <> comma <+> genTopName env (methodname (noq $ tcname tc) attr_finalizer))
genCall env ts e@(Var _ n) p
  | NClass{} <- info                = genNew env n p
  | NDef{} <- info                  = (instCast env ts e $ gen env e) <> parens (genCallPosArgs env r p)
  where info                        = findQName n env
        TFun _ _ r _ _              = genCallableType env ts e
genCall env ts (Async _ e) p        = genCall env ts e p
genCall env ts e0@(Dot _ e n) p     = genDotCall env ts (snd $ schemaOf env e0) e n p
genCall env ts e p                  = gen env e <> parens (gen env p)

instCast env [] e                   = id
instCast env ts e@(Var _ x)
  | GName m _ <- x, m == mPrim      = id
instCast env ts e                   = parens . (parens (gen env t) <>)
  where t                           = B.matchTypes (typeInstOf env ts e) (typeInstOf env (map (\_ -> tWild) ts) e)  -- to cast to the general type

targetType env (Dot _ e n)          = sctype sc
  where t0                          = typeOf env e
        (_,c0)                      = case t0 of
                                         TCon _ tc -> splitTC env tc
                                         TVar _ tv -> splitTC env (findTVBound env tv)
        (sc, dec)                   = findAttr' env c0 n
targetType env e                    = typeOf env e                  -- Must be a Var with a monomorphic type since it is assignable

isUnboxedRep :: Type -> Bool
isUnboxedRep TUnboxed{}             = True
isUnboxedRep t                      = B.isUnboxable t

boxedRepType :: Type -> Type
boxedRepType (TUnboxed _ t)         = t
boxedRepType t                      = t

unboxedField :: GenEnv -> Expr -> Name -> Bool
unboxedField env e n                = maybe False (isUnboxedRep . sctype . fst) (fieldAttr env e n)

unboxedVar :: GenEnv -> QName -> Bool
unboxedVar env n                    = case findQName n env of
                                        NVar t  -> isRawVar t
                                        NSVar t -> isRawVar t
                                        _       -> False
  where isRawVar TUnboxed{}         = True
        isRawVar t                  = isGlobalVar n && B.isUnboxable t
        isGlobalVar (NoQ n)         = n `elem` global env
        isGlobalVar GName{}         = True
        isGlobalVar QName{}         = True

fieldAttr :: GenEnv -> Expr -> Name -> Maybe (TSchema, Maybe Deco)
fieldAttr env e n                   = case typeOf env e of
                                        TCon _ tc -> Just $ findAttr' env (snd $ splitTC env tc) n
                                        TVar _ tv -> Just $ findAttr' env (snd $ splitTC env (findTVBound env tv)) n
                                        _         -> Nothing

dotCast env ent ts (Var _ x) n
  | GName m _ <- x, m == mPrim      = id
dotCast env ent ts e n
  | gen_t == gen env t1,
    not (needsPrimCallableCast t0 n) = id
  | otherwise                       = parens . (parens gen_t <>)
  where t0                          = typeOf env e
        (argsubst, c0, rtc)         = case t0 of
                                         TCon _ tc -> let (s,c) = splitTC env tc in (s, c, tc)
                                         TVar _ tv -> let tc = findTVBound env tv
                                                          (s,c) = splitTC env tc
                                                      in (s, c, tc)
                                         TTuple{}  -> ([], cValue, cValue)
        (sc, dec)                   = findAttr' env c0 n
        t                           = vsubst fullsubst $ if ent then addSelf t1 dec else t1
        t1                          = exposeMsg' (sctype sc)
        t' sc'                      = vsubst fullsubst $ if ent then addSelf (t1' sc') dec else t1' sc'
        t1' sc'                     = exposeMsg' (sctype sc')
        fullsubst                   = (tvSelf,t0) : (qbound (scbind sc) `zip` ts) ++ argsubst
        te                          = findAttrSchemas env (tcname c0)
        gen_t
          | null ts                 = gen env $ if ent then addSelf rt dec else rt
          | otherwise               = case lookup n te of
                                        Just (NDef sc' _ _) -> gen env (B.matchTypes t (sctype sc'))
                                        Just (NSig sc' _ _) -> gen env (B.matchTypes t (t' sc'))
                                        Just (NVar t) -> gen env t
                                        ni  -> error ("Internal error in CodeGen.dotCast: looking for NameInfo for " ++ show n ++ ", found "++ show ni)
        rt                          = exposeMsg' (B.rtypeOf env rtc n)

needsPrimCallableCast (TCon _ (TC q _)) n
  | q == primCont                   = n == attr_call_
  | q == primProc                   = n `elem` [attr_call_, attr_exec_]
  | q == primAction                 = n `elem` [attr_call_, attr_exec_, attr_asyn_]
  | q == primMut                    = n `elem` [attr_call_, attr_exec_, attr_eval_]
  | q == primPure                   = n `elem` [attr_call_, attr_exec_, attr_eval_]
needsPrimCallableCast _ _           = False
                                        
classCast env ts x q n              = parens . (parens (gen env t) <>)
  where (ts0,ts1)                   = splitAt (length q) ts
        tc                          = TC x ts0
        (sc, dec)                   = findAttr' env tc n
        t                           = vsubst fullsubst $ addSelf (sctype sc) dec
        fullsubst                   = (tvSelf,tCon tc) : (qbound (scbind sc) `zip` ts1)

genNew env n p                      = newcon' env n <> parens (genUCallPosArgs env r p)
  where TFun _ _ r _ _              = sctype $ fst $ schemaOf env (Var NoLoc n)

declCon env n q b
  | null abstr || hasNotImpl b      = (gen env tRes <+> newcon env n <> parens (gen env pars) <+> char '{') $+$
                                      nest 4 (gen env tObj <+> gen env tmpV <+> equals <+> acton_malloc env (gname env n) <> semi $+$
                                              gen env tmpV <> text "->" <> gen env1 classKW <+> equals <+> char '&' <> methodtable env1 n <> semi $+$
                                              altcall $+$
                                              initcall) $+$
                                      char '}'
  | otherwise                       = empty
  where TFun _ fx r _ t             = sctype $ fst $ schemaOf env (eVar n)
        tc                          = TC (NoQ n) (map tVar $ qbound q)
        tObj                        = tCon tc
        tRes                        = if t == tR then tR else tObj
        pars                        = pPar paramNames r
        args                        = pArg pars
        altcall
          | Just (_,sc,_) <- alt    = let i = arity $ posrow $ sctype sc
                                          args' = pArg (chop i pars)
                                      in methodtable env n <> dot <> gen env altInit <> parens (gen env1 tmpV <> comma' (gen env1 args')) <> semi
          | otherwise               = empty
          where alt                 = findAttr env tc altInit
        initcall
          | t == tR                 = text "return" <+> methodtable env n <> dot <> gen env initKW <> parens (gen env1 tmpV <> comma <+> gen env1 (retobj args)) <> semi
          | otherwise               = methodtable env n <> dot <> gen env initKW <> parens (gen env1 tmpV <> comma' (gen env1 args)) <> semi $+$
                                      text "return" <+> gen env tmpV <> semi
        retobj (PosArg e p)         = PosArg (eCall (tApp (eQVar primCONSTCONT) [tObj]) [eVar tmpV, e]) p
        env1                        = ldefine ((tmpV, NVar tObj) : envOf pars) env
        abstr                       = abstractAttrs env (NoQ n)

acton_malloc env n                  = text "acton_malloc" <> parens (text "sizeof" <> parens (text "struct" <+> gen env n))

comma' x                            = if isEmpty x then empty else comma <+> x

genDotCall env ts dec e@(Var _ x) n p
  | NClass q _ _ _ <- info,
    Just _ <- dec                   = classCast env ts x q n (methodtable' env x <> text "." <> gen env n) <> parens (gen env p)
  where info                        = findQName x env
genDotCall env ts dec e n p
  | Just NoDec <- dec               = genEnter env ts e n p
  | Just Static <- dec              = dotCast env False ts e n (gen env e <> text "->" <> gen env classKW <> text "->" <> gen env n) <> parens (gen env p)


genDot env ts e@(Var _ x) n
  | NClass q _ _ _ <- findQName x env = classCast env ts x q n $ methodtable' env x <> text "." <> gen env n
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
genEnter env ts e n p               = dotCast env True ts e n (gen env e <> text "->" <> gen env classKW <> text "->" <> gen env n) <> parens (gen env e <> comma' (genCallPosArgs env r p))
  where TFun _ _ r _ _              = dotCallRType env ts e n

dotCallRType env [] e n             = B.rtypeOfFun env (Dot NoLoc e n)
dotCallRType env ts e n             = B.rtypeOfFun env (TApp NoLoc (Dot NoLoc e n) ts)

genInst env ts e@Var{}              = instCast env ts e $ gen env e
genInst env ts (Dot _ e n)          = genDot env ts e n

adjust t t' e
  | t == t'                         = e
adjust (TUnboxed _ t) t' e          = adjust t t' e
adjust t (TUnboxed _ t') e          = adjust t t' e
adjust (TOpt _ t) t' e              = adjust t t' e
adjust t (TOpt _ t') e              = adjust t t' e
adjust TNone{} t' e                 = e
adjust t t'@TVar{} e                = e
adjust (TCon _ c) (TCon _ c') e
  | tcname c == tcname c'           = e
adjust t t' e
   | B.isUnboxable t'               = e
   | otherwise                      = typecast t t' e

genExp env t' e                     = gen env (adjust t t' e')
  where (t, fx, e')                 = qType env adjust e

genExp' env e                       = gen env e'
  where (t, fx, e')                 = qType env adjust e

genBoxed env t e                    = text ("toB_"++render(pretty (noq (tcname(tcon (boxedRepType t)))))) <> parens e

instance Gen Expr where
    gen env (Var _ n)
      | NClass{} <- findQName n env = newcon' env n
      | otherwise                   = genQName env n
    gen env (Int _ i str)
        | i < 0                     = gen env primToBigInt2 <> parens (doubleQuotes $ text str)       -- negative → string
        | i <= 9223372036854775807  = gen env primToBigInt  <> parens (text (str++"UL"))             -- fits i64 → toB_bigint
        | i <= 18446744073709551615 = gen env primToU64     <> parens (text (str++"UL"))             -- fits u64  → toB_u64
        | otherwise                 = gen env primToBigInt2 <> parens (doubleQuotes $ text str)       -- large → string
    gen env (Float _ _ str)         = gen env primToFloat <> parens (text str)
    gen env (Bool _ True)           = gen env qnTrue
    gen env (Bool _ False)          = gen env qnFalse
    gen env (None _)                = gen env qnNone
    gen env e@Strings{}             = gen env primToStr <> parens(hsep (map pretty (sval e)))
    gen env e@BStrings{}            = gen env primToBytes <> parens( hsep (map pretty es) <> comma <+>text(show(length(read(concat es) :: String))))
      where es                      = sval e
    gen env (Call l  (TApp _ e@(Var _ mk) _) p@(PosArg w (PosArg (Set _ es) PosNil)) KwdNil)
      | mk == primMkSet             = text "B_mk_set" <> parens (pretty (length es) <> comma <+> gen env w <> hsep [comma <+> gen env e | e <- es])
    gen env (Call l  (TApp _ e@(Var _ mk) _) p@(PosArg w (PosArg (Dict _ es) PosNil)) KwdNil)
      | mk == primMkDict            = text "B_mk_dict" <> parens (pretty (length es) <> comma <+> gen env w <>  hsep [comma <+> gen env e | e <- es])
    gen env c@(Call _ e p _)        = genCall env [] e p
    gen env (Async _ e)             = gen env e
    gen env (TApp _ e ts)           = genInst env ts e
    gen env (Let _ ss e)            = text "({" <+> fst(genSuite env ss) $+$ gen (ldefine (envOf ss) env) e <> text ";})"
    gen env (IsInstance _ e c)      = gen env primISINSTANCE <> parens (gen env e <> comma <+> genQName env c)
    gen env (Dot _ e n)             = genDot env [] e n
    gen env e0@(DotI _ e i)         = parens $ parens (gen env t) <> gen env e <> text "->" <> gen env componentsKW <> brackets (pretty i)
      where t                       = boxedRepType (typeOf env e0)
    gen env (RestI _ e i)           = gen env eNone <> semi <+> text "// CodeGen for tuple tail not implemented"
    gen env (Tuple _ p KwdNil)
       | n == 0                     = gen env primNEWTUPLE0
       | otherwise                  = gen env primNEWTUPLE <> parens (text (show n) <> comma' (gen env p))
       where n                      = nargs p
    gen env (List _ es)             = text "B_mk_list" <> parens (pretty (length es) <> hsep [comma <+> gen env e | e <- es])
    gen env (BinOp _ e1 And e2)     = gen env primAND <> parens (gen env t <> comma <+> gen env e1 <> comma <+> gen env e2)
      where t                       = typeOf env e1
    gen env (BinOp _ e1 Or e2)      = gen env primOR <> parens (gen env t <> comma <+> gen env e1 <> comma <+> gen env e2)
      where t                       = typeOf env e1
    gen env (BinOp _ e1 op e2)
            | boxedRepType t == tU1,
              Just d <- genU1RawBinOp env op e1 e2
                                    = d
            | op == Div && t /= tFloat
             || op `elem` [Pow, Mod, EuDiv]     -- Pow since there is no C operator, the others since they need to check for division by zero
                                    = gencFunCall env (tstr ++ '_' : opstr op) [e1, e2]
            | B.isUnboxable (boxedRepType t)
                                    = castRawResult t (genRawExpr env e1 <+> binPretty op <+> genRawExpr env e2)
            | otherwise             = gen env e1 <+> binPretty op <+> gen env e2
      where t                       = typeOf env e1
            tstr                    = nstr (noq (tcname (tcon (boxedRepType t))))
            opstr Pow               = "pow"
            opstr Div               = "DIV"
            opstr Mod               = "MOD"
            opstr EuDiv             = "FLOORDIV"
    gen env (CompOp _ e [a])        = gen env e <+> gen env a
    gen env (UnOp _ Not e)          = gen env primNOT <> parens (gen env t <> comma <+> gen env e)
      where t                       = typeOf env e
    gen env (UnOp _ op e)
      | t == tU1
                                    = genU1RawUnOp env op e
      | B.isUnboxable t             = castRawResult t (genRawUnOp env op e)
      where t                       = boxedRepType (typeOf env e)
    gen env (UnOp _ op e)           = parens (pretty op <+> gen env e)
    gen env (Cond _ e1 e e2)        = parens (parens (gen env (B.unbox tBool e)) <+> text "?" <+> gen env e1 <+> text ":" <+> gen env e2)
    gen env (Paren _ e)             = parens (gen env e)
    gen env (Box t i@Int{})
      | B.isUnboxable t'            = genBoxed env t' (gen env (UnBox t' i))
      where t'                      = boxedRepType t
    gen env (Box t f@Float{})
      | B.isUnboxable t'            = genBoxed env t' (gen env (UnBox t' f))
      where t'                      = boxedRepType t
    gen env (Box t e)
      | boxedExpr env e             = gen env e
      | otherwise                   = genBoxed env t (gen env e)
    gen env (UnBox _ e@(Call _ (Var _ f) p KwdNil))
        | f == primISNOTNONE        = genCall env [] (Var NoLoc primISNOTNONE0) p
        | f == primISNONE           = genCall env [] (Var NoLoc primISNONE0) p
        | f `elem` [primPUSH,primPUSHF]
                                    = gen env f <> parens(empty)
  --      | f `elem` B.mathfuns       = genCall env [] e p
        | tCon (TC (gBuiltin (noq f)) []) `elem` B.integralTypes   -- f is the constructor for an integer type, so check if argument e is a literal
                                    = genUnboxedInt env (posargs p) e
    gen env (UnBox t e@(Call _ (Dot _ (Var _ w) op) (PosArg x (PosArg y PosNil)) KwdNil))  -- use macro for int (in)equality tests
                                    = case findQName w env of
                                        NVar (TCon _ (TC p [TCon _ (TC t [])]))
                                          | (p==qnOrd || p==qnEq) &&  t == qnBigint ->
                                             text "ORD_" <> tname <> text (nstr op) <> parens(parens (parens tname <> gen env x) <> comma <+> parens (parens tname <> gen env y))
                                        _ ->  parens (parens (gen env t) <> gen env e) <> text "->val"
      where tname                   = genQName env qnBigint 

    gen env (UnBox _ (IsInstance _ e c))
                                    = gen env primISINSTANCE0 <> parens(gen env e <> comma <+> genQName env c)
    gen env (UnBox t (Int _ n s))   = text (s++ suffix t)
       where suffix t
               | t == tInt          = "LL"
               | t == tU64          = "UL"
               | otherwise          = ""
             
    gen env (UnBox _ (Float _ x s)) = text s
    gen env (UnBox _ (Bool _ b))    = if b then text "true" else text "false"
    gen env (UnBox t (Cond _ e1 e e2))
                                    = parens (parens (gen env (B.unbox tBool e)) <+> text "?" <+> genRawExprAs env t e1 <+> text ":" <+> genRawExprAs env t e2)
    gen env (UnBox _ v@(Var _ n))
      | unboxedVar env n             = gen env v
    gen env (UnBox _ d@(Dot _ e n))
      | unboxedField env e n         = gen env d
    gen env (UnBox t e)             = parens (parens (gen env t) <> gen env e) <> text "->val"
--    gen env (UnBox t e)             = parens (gen env e) <> text "->val"
    gen env e                       = error ("CodeGen.gen for Expr: e = " ++ show e)

gencFunCall env nm []               = text nm <> parens empty
gencFunCall env nm (x : xs)         = text nm <> parens (genRawExpr env x <> hsep [ comma <+> genRawExpr env x | x <- xs ])

genU1RawBinOp env Plus e1 e2        = Just $ genU1Norm (genU1Raw2 env "+" e1 e2)
genU1RawBinOp env Minus e1 e2       = Just $ genU1Norm (genU1Raw2 env "-" e1 e2)
genU1RawBinOp env Mult e1 e2        = Just $ genU1Norm (genU1Raw2 env "*" e1 e2)
genU1RawBinOp env BOr e1 e2         = Just $ genU1Norm (genU1Raw2 env "|" e1 e2)
genU1RawBinOp env BXor e1 e2        = Just $ genU1Norm (genU1Raw2 env "^" e1 e2)
genU1RawBinOp env BAnd e1 e2        = Just $ genU1Norm (genU1Raw2 env "&" e1 e2)
genU1RawBinOp env ShiftL e1 e2      = Just $ genU1Norm (genU1Raw2 env "<<" e1 e2)
genU1RawBinOp env ShiftR e1 e2      = Just $ genU1Norm (genU1Raw2 env ">>" e1 e2)
genU1RawBinOp _ _ _ _               = Nothing

genU1Raw2 env op e1 e2              = genRawExpr env e1 <+> text op <+> genRawExpr env e2

genRawUnOp env UPlus e              = genRawExpr env e
genRawUnOp env op e                 = parens (pretty op <+> genRawExpr env e)

genU1RawUnOp env UPlus e            = genRawExpr env e
genU1RawUnOp env UMinus e           = genU1Norm (text "-" <> parens (genRawExpr env e))
genU1RawUnOp env BNot e             = genU1Norm (text "~" <> parens (genRawExpr env e))
genU1RawUnOp env op e               = parens (pretty op <+> gen env e)

genU1RawAugOp env PlusA tg e        = Just $ genU1Norm (genU1RawAug2 env "+" tg e)
genU1RawAugOp env MinusA tg e       = Just $ genU1Norm (genU1RawAug2 env "-" tg e)
genU1RawAugOp env MultA tg e        = Just $ genU1Norm (genU1RawAug2 env "*" tg e)
genU1RawAugOp env PowA tg e         = Just $ text "u1_pow" <> parens (genTarget env tg <> comma <+> genRawExpr env e)
genU1RawAugOp env ModA tg e         = Just $ text "u1_MOD" <> parens (genTarget env tg <> comma <+> genRawExpr env e)
genU1RawAugOp env EuDivA tg e       = Just $ text "u1_FLOORDIV" <> parens (genTarget env tg <> comma <+> genRawExpr env e)
genU1RawAugOp env BOrA tg e         = Just $ genU1Norm (genU1RawAug2 env "|" tg e)
genU1RawAugOp env BXorA tg e        = Just $ genU1Norm (genU1RawAug2 env "^" tg e)
genU1RawAugOp env BAndA tg e        = Just $ genU1Norm (genU1RawAug2 env "&" tg e)
genU1RawAugOp env ShiftLA tg e      = Just $ genU1Norm (genU1RawAug2 env "<<" tg e)
genU1RawAugOp env ShiftRA tg e      = Just $ genU1Norm (genU1RawAug2 env ">>" tg e)
genU1RawAugOp _ _ _ _               = Nothing

genU1RawAug2 env op tg e            = genTarget env tg <+> text op <+> genRawExpr env e

genU1Norm d                         = parens (parens d <+> text "& 1")

castRawResult t d                   = parens (parens (text (unboxed_c_type (boxedRepType t))) <> parens d)

genUnboxedInt env [Int _ n s, None _] _
                                    = text s
genUnboxedInt env _ c               = parens (gen env c) <> text "->val"

instance Gen OpArg where
    gen env (OpArg  op e)           = compPretty op <+> gen env e

-- compPretty Is                       = text "=="
-- compPretty IsNot                    = text "!="
compPretty op                       = pretty op

binPretty And                       = text "&&"
binPretty Or                        = text "||"
binPretty EuDiv                     = text "/"
binPretty op                        = pretty op

augPretty EuDivA                    = text "/="
augPretty op                        = pretty op

genStr env s                        = text $ head $ sval s

genBool env (Paren _ e)             = genBool env e
genBool env e                       = genExp env tBool e
  where t                           = typeOf env e

nargs                               :: PosArg -> Int
nargs PosNil                        = 0
nargs (PosArg _ p)                  = 1 + nargs p

instance Gen Elem where
    gen env (Elem e)                = gen env e

instance Gen Assoc where
    gen env (Assoc e1 e2)           = gen env primNEWTUPLE <> parens (text "2" <> comma <+> gen env e1 <> comma <+> gen env e2)

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
    gen env (TFun _ _ p _ t)        = gen env t <+> parens (text "*") <+> parens (gen env p)
    gen env (TTuple _ pos _)        = gen env qnTuple
    gen env (TOpt _ t)              = gen env t
    gen env (TNone _)               = gen env qnNoneType
    gen env (TWild _)               = word
    gen env (TRow _ _ _ t TNil{})   = gen env t
    gen env (TRow _ _ _ t r)        = gen env t <> comma <+> gen env r
    gen env (TNil _ _)              = empty
    gen env (TUnboxed _ t)          = text (unboxed_c_type t)

unboxed_c_type t
    | t == tInt = "int64_t"
    | t == tU64 = "uint64_t"
    | t == tI32 = "int32_t"
    | t == tU32 = "uint32_t"
    | t == tI16 = "int16_t"
    | t == tU16 = "uint16_t"
    | t == tI8  = "int8_t"
    | t == tU8  = "uint8_t"
    | t == tU1  = "uint8_t"  -- ????
    | t == tFloat = "double"
--    | t == tBool  = "bool"
    | otherwise = error ("Internal error: trying to find unboxed type for " ++ show t)

class_id t
    | t == tInt = "I64_ID"
    | t == tU64 = "U64_ID"
    | t == tI32 = "I32_ID"
    | t == tU32 = "U32_ID"
    | t == tI16 = "I16_ID"
    | t == tU16 = "U16_ID"
    | t == tI8  = "I8_ID"
    | t == tU8  = "U8_ID"
    | t == tU1  = "U1_ID"
    | t == tFloat = "FLOAT_ID"
    | otherwise = error ("Internal error: trying to find class id for " ++ show t)
