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

generate                            :: Acton.Env.Env0 -> Module -> IO (String,String)
generate env m                      = do return (h,c)
  where h                           = render $ hModule env0 m
        c                           = render $ cModule env0 m
        env0                        = genEnv $ setMod (modname m) env


-- Environment --------------------------------------------------------------------------------------

genEnv env0                         = setX env0 GenX{ globalX = [], localX = [] }

type GenEnv                         = EnvF GenX

data GenX                           = GenX { globalX :: [Name], localX :: [Name] }

gdefine te env                      = modX env1 $ \x -> x{ globalX = dom te ++ globalX x }
  where env1                        = define te env

ldefine te env                      = modX env1 $ \x -> x{ localX = dom te ++ localX x }
  where env1                        = define te env

global env                          = globalX (envX env) \\ localX (envX env)

defined env                         = globalX (envX env) ++ localX (envX env)


-- Helpers ------------------------------------------------------------------------------------------

include env m                       = text "#include" <+> doubleQuotes (gen env m <> text ".h")

modNames (Import _ ms : is)         = [ m | ModuleItem m _ <- ms ] ++ modNames is
modNames (FromImport _ (ModRef (0,Just m)) _ : is)
                                    = m : modNames is
modNames (FromImportAll _ (ModRef (0,Just m)) : is)
                                    = m : modNames is
modNames []                         = []


-- Header -------------------------------------------------------------------------------------------

hModule env (Module m imps stmts)   = text "#ifndef" <+> gen env m $+$
                                      text "#define" <+> gen env m $+$
                                      vcat (map (include env) $ modNames imps) $+$
                                      hSuite env stmts $+$
                                      text "#endif"

hSuite env []                       = empty
hSuite env (s:ss)                   = hStmt env s $+$ hSuite (gdefine (envOf s) env) ss

hStmt env (Decl _ ds)               = vmap (stub env1) ds $+$
                                      vmap (typedef env1) ds $+$
                                      vmap (decl env1) ds $+$
                                      vmap (methstub env1) ds
  where env1                        = gdefine (envOf ds) env
hStmt env s                         = vcat [ text "extern" <+> gen env t <+> genTopName env n <> semi | (n,NVar t) <- envOf s]

stub env (Class _ n q a b)          = text "struct" <+> genTopName env n <> semi
stub env Def{}                      = empty

typedef env (Class _ n q a b)       = text "typedef" <+> text "struct" <+> genTopName env n <+> char '*' <> genTopName env n <> semi
typedef env Def{}                   = empty

decl env (Class _ n q a b)          = (text "struct" <+> classname env n <+> char '{') $+$ 
                                      nest 4 (vcat $ stdprefix env ++ initdef : serialize env tc : deserialize env tc : meths) $+$
                                      char '}' <> semi $+$
                                      (text "struct" <+> genTopName env n <+> char '{') $+$ 
                                      nest 4 (classlink env n $+$ properties env tc) $+$ 
                                      char '}' <> semi
  where tc                          = TC (NoQ n) [ tVar v | Quant v _ <- q ]
        initdef : meths             = fields env tc
decl env (Def _ n q p _ a b _ fx)   = gen env (fromJust a) <+> genTopName env n <+> parens (params env $ prowOf p) <> semi

methstub env (Class _ n q a b)      = text "extern" <+> text "struct" <+> classname env n <+> methodtable env n <> semi
methstub env Def{}                  = empty

fields env c                        = map field te
  where te                          = fullAttrEnv env c
        field (n, NDef sc Static)   = funsig env n (sctype sc) <> semi
        field (n, NDef sc NoDec)    = methsig env c n (sctype sc) <> semi
        field (n, NVar t)           = varsig env n t <> semi
        field (n, NSig sc Static)   = funsig env n (sctype sc) <> semi <+> text "// abstract!!!"
        field (n, NSig sc NoDec)    = methsig env c n (sctype sc) <> semi <+> text "// abstract!!!"
        field (n, NSig sc Property) = empty

funsig env n (TFun _ _ r _ t)       = gen env t <+> parens (char '*' <> gen env n) <+> parens (params env r)

methsig env c n (TFun _ _ r _ t)    = gen env t <+> parens (char '*' <> gen env n) <+> parens (params env $ posRow (tCon c) r)

params env (TNil _ _)               = empty
params env (TRow _ _ _ t TNil{})    = gen env t
params env (TRow _ _ _ t r)         = gen env t <> comma <+> params env r
params env t                        = error ("codegen unexpected row: " ++ prstr t)

varsig env n t                      = gen env t <+> gen env n

properties env c                    = vmap prop te
  where te                          = fullAttrEnv env c
        prop (n, NSig sc Property)  = varsig env n (sctype sc) <> semi
        prop _                      = empty

stdprefix env                       = [gcinfo env, classid env, superlink env]

gcinfo env                          = text "char" <+> text "*" <> gen env gcinfoKW <> semi

classid env                         = gen env tInt <+> gen env classidKW <> semi

superlink env                       = gen env tSuperclass <+> gen env superclassKW <> semi
  where tSuperclass                 = tCon $ TC qnSuperClass []

qnSuperClass                        = GName mPrim (Derived (name "Super") (name "class"))

serialize env c                     = methsig env c (name "__serialize__") (TFun l0 fxPure serialstate kwdNil tNone) <> semi

deserialize env c                   = funsig env (name "__deserialize__") (TFun l0 fxPure serialstate kwdNil (tCon c)) <> semi

serialstate                         = posRow tSerialstate posNil
  where tSerialstate                = tCon $ TC (GName mPrim (Derived (name "Serial") (name "state"))) []

classlink env n                     = text "struct" <+> classname env n <+> text "*" <> gen env classKW <> semi

classname env n                     = genTopName env (Derived n $ name "class")

methodtable env n                   = genTopName env (Derived n $ name "methods")

methodtable' env (NoQ n)            = methodtable env n
methodtable' env (GName m n)        = gen env $ GName m (Derived n $ name "methods")


classKW                             = primKW "class"
gcinfoKW                            = primKW "GCINFO"
classidKW                           = primKW "class_id"
superclassKW                        = primKW "superclass"
appKW                               = primKW "APP"
entKW                               = primKW "ENT"
newKW                               = primKW "NEW"
newccKW                             = primKW "NEWCC"
registerKW                          = primKW "register"
noneKW                              = primKW "None"
nonetypeKW                          = primKW "NoneType"

-- Implementation -----------------------------------------------------------------------------------

cModule env (Module m imps stmts)   = include env m $+$
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

declDecl env (Def _ n q p KwdNIL a b d m)
                                    = (gen env a <+> genTopName env n <+> parens (gen env p) <+> char '{') $+$
                                      nest 4 (genSuite env1 b $+$ ret) $+$
                                      char '}'
  where env1                        = ldefine (envOf p) $ defineTVars q env
        ret | fallsthru b           = text "return" <+> gen env noneKW <> semi
            | otherwise             = empty
declDecl env (Class _ n q as b)     = vcat [ declDecl env1 d{ dname = methodname n (dname d) } | Decl _ ds <- b', d@Def{} <- ds ] $+$
                                      text "struct" <+> classname env n <+> methodtable env n <> semi
  where b'                          = subst [(tvSelf, tCon $ TC (NoQ n) (map tVar $ tybound q))] b
        env1                        = defineTVars q env



initModule env []                   = empty
initModule env (Decl _ ds : ss)     = vcat [ char '{' $+$ nest 4 (initClassBase env1 n as $+$ initClass env n b) $+$ char '}' | Class _ n q as b <- ds ] $+$
                                      initModule env1 ss
  where env1                        = gdefine (envOf ds) env
initModule env (Signature{} : ss)   = initModule env ss
initModule env (s : ss)             = genStmt env s $+$
                                      vcat [ genTopName env n <+> equals <+> gen env n <> semi | (n,_) <- te ] $+$
                                      initModule env1 ss
  where te                          = envOf s `exclude` defined env
        env1                        = gdefine te env


initClassBase env c as              = methodtable env c <> dot <> gen env gcinfoKW <+> equals <+> doubleQuotes (genTopName env c) <> semi $+$
                                      methodtable env c <> dot <> gen env superclassKW <+> equals <+> super <> semi $+$
                                      vcat [ inherit c' n i | (c',te) <- inheritedAttrs env (NoQ c), (n,i) <- te ]
  where super                       = if null as then text "NULL" else parens (gen env qnSuperClass) <> text "&" <> methodtable' env (tcname $ head as)
        selfsubst                   = subst [(tvSelf, tCon $ TC (NoQ c) [])]
        inherit c' n i              = methodtable env c <> dot <> gen env n <+> equals <+> cast i <> methodtable' env c' <> dot <> gen env n <> semi
          where cast (NVar t)       = parens (gen env $ selfsubst t)
                cast (NDef sc dec)  = parens (gen env (selfsubst $ addSelf (sctype sc) (Just dec)))

initClass env c []                  = gen env registerKW <> parens (char '&' <> methodtable env c) <> semi
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
    gen env (QName m n)             = error ("Unexpected QName in CodeGen: " ++ prstr (QName m n))

instance Gen Name where
    gen env nm                      = text $ unCkeyword $ mkCident $ nstr nm

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
                                        "auto",     "break",    "case",     "char",     "continue", "default",
                                        "default",  "do",       "double",   "else",     "enum",     "extern",
                                        "float",    "for",      "goto",     "if",       "int",      "long",
                                        "register", "return",   "short",    "signed",   "sizeof",   "static",
                                        "struct",   "switch",   "typedef",  "union",    "unsigned", "void",
                                        "volatile", "while"
                                      ]

preEscape str                       = "_$" ++ str


genTopName env n                    = gen env (gname env n)

word                                = text "$WORD"

genSuite env []                     = empty
genSuite env (s:ss)                 = genStmt env s $+$ genSuite (ldefine (envOf s) env) ss
  where te                          = envOf s `exclude` defined env
        env1                        = ldefine te env

genStmt env (Decl _ ds)             = empty
genStmt env (Assign _ [PVar _ n (Just t)] e)
  | n `notElem` defined env         = gen env t <+> gen env n <+> equals <+> gen env e <> semi
genStmt env s                       = vcat [ gen env t <+> gen env n <> semi | (n,NVar t) <- te ] $+$
                                      gen env s
  where te                          = envOf s `exclude` defined env

instance Gen Stmt where
    gen env (Expr _ e)              = gen env e <> semi
    gen env (Assign _ [p] e)        = gen env p <+> equals <+> gen env e <> semi
    gen env (MutAssign _ t e)       = gen env t <+> equals <+> gen env e <> semi
    gen env (Pass _)                = empty
    gen env (Return _ Nothing)      = text "return" <+> gen env eNone <> semi
    gen env (Return _ (Just e))     = text "return" <+> gen env e <> semi
    gen env (Break _)               = text "break" <> semi
    gen env (Continue _)            = text "continue" <> semi
    gen env (If _ (b:bs) b2)        = genBranch env "if" b $+$ vmap (genBranch env "else if") bs $+$ genElse env b2
    gen env (While _ e b [])        = (text "while" <+> parens (gen env e) <+> char '{') $+$ nest 4 (genSuite env b) $+$ char '}'
    gen env _                       = empty

genBranch env kw (Branch e b)       = (text kw <+> parens (gen env e) <+> char '{') $+$ nest 4 (genSuite env b) $+$ char '}'

genElse env []                      = empty
genElse env b                       = (text "else" <+> char '{') $+$ nest 4 (genSuite env b) $+$ char '}'

instance Gen PosPar where
    gen env (PosPar n t e PosNIL)   = gen env t <+> gen env n
    gen env (PosPar n t e p)        = gen env t <+> gen env n <> comma <+> gen env p
    gen env PosNIL                  = empty

instance Gen PosArg where
    gen env (PosArg e PosNil)       = gen env e
    gen env (PosArg e p)            = gen env e <> comma <+> gen env p
    gen env PosNil                  = empty

instance Gen KwdArg where
    gen env (KwdArg n e KwdNil)     = gen env n <+> equals <+> gen env e
    gen env (KwdArg n e k)          = gen env n <+> equals <+> gen env e <> comma <+> gen env k
    gen env KwdNil                  = empty

genCall env t0 (TApp _ (Var _ n) [_,t]) (PosArg e PosNil)
  | n == primCAST                   = parens (gen env t) <> gen env e
genCall env t0 (TApp _ e _) p       = genCall env t0 e p
genCall env t0 e@(Var _ n) p
  | NDef{} <- info                  = gen env e <> parens (gen env p)
  | NClass{} <- info                = gen env new <> parens (gen env $ PosArg e p')
  where info                        = findQName n env
        (new,p')                    = if t0 == tR then (newccKW, rotate p) else (newKW, p)
genCall env t0 e0@(Dot _ e n) p     = genDotCall env (snd $ schemaOf env e0) e n p
genCall env t0 e p                  = apply env (typeOf env e) e callKW p

genDotCall env dec (TApp _ e _) n p = genDotCall env dec e n p
genDotCall env dec e@(Var _ x) n p
  | NClass{} <- info, Just _ <- dec = gen env e <> text "." <> gen env n <> parens (gen env p)
  | NClass{} <- info                = apply env (typeOf env e) (eDot e n) callKW p
  where info                        = findQName x env
genDotCall env dec e n p
  | Just NoDec <- dec               = apply env (typeOf env e) e n p
  | Just Static <- dec              = gen env e <> text "->" <> gen env classKW <> text "->" <> gen env n <> parens (gen env p)
genDotCall env dec e n p            = apply env (typeOf env e) (eDot e n) callKW p


genDot env (TApp _ e _) n           = genDot env e n
genDot env e@(Var _ x) n
  | NClass{} <- findQName x env     = gen env e <> text "." <> gen env n
genDot env e n                      = gen env e <> text "->" <> gen env n


apply env t e n p                   = gen env appKW <> parens (gen env t <> comma <+> gen env e <> comma <+> 
                                                               gen env n <> comma <+> gen env p)

instance Gen Expr where
    gen env (Var _ (NoQ n))
      | n `elem` global env         = genTopName env n
    gen env (Var _ n)               = gen env n
    gen env (Int _ _ str)           = text str
    gen env (Float _ _ str)         = text str
--    gen env (Imaginary _ _ str)     = text str
    gen env (Bool _ True)           = text "1"
    gen env (Bool _ False)          = text "0"
    gen env (None _)                = gen env noneKW
--    gen env (NotImplemented _)      = text "NotImplemented"
--    gen env (Ellipsis _)            = text "..."
    gen env (Strings _ [s])         = doubleQuotes $ text $ tail $ init s
    gen env (BStrings _ [s])        = doubleQuotes $ text $ tail $ init s
    gen env e0@(Call _ e p KwdNil)  = genCall env (typeOf env e0) e p
    gen env (TApp _ e ts)           = gen env e
    gen env (IsInstance _ e c)      = gen env primISINSTANCE <> parens (gen env e <> comma <+> gen env (globalize env c))
    gen env (Dot _ e n)             = genDot env e n
    gen env (Rest _ e n)            = text "CodeGen for tuple tail not implemented"
    gen env (DotI _ e i)            = gen env e <> brackets (pretty i)
    gen env (RestI _ e i)           = text "CodeGen for tuple tail not implemented"
--    gen env (Yield _ e)             = 
--    gen env (YieldFrom _ e)         = 
    gen env (Tuple _ pargs kargs)   = parens (gen env pargs <+> gen env kargs)
    gen env (List _ es)             = brackets (commaSep (gen env) es)
    gen env e                       = genPrec env 0 e -- BinOp, UnOp  and Cond

rotate p                            = rot [] p
 where rot es (PosArg e PosNil)     = PosArg e $ foldl (flip PosArg) PosNil es
       rot es (PosArg e p)          = rot (e:es) p
       rot es p                     = foldl (flip PosArg) p es

{-
We assign precedences and associativity to remaining operators as follows

   Not   4  ---
   And   3  left
   Or    2  left
   ?:    1  right

Note that the expression between ? and : in the ternary conditional operator is parsed as if it was parenthesized, 
so we never print parentheses around it. The remaining binary operator _ ?: _ has lower precedence than the other 
boolean operators and associates to the right.

We never need to put unary negated expressions in parentheses, since all higher precedence operators have been 
eliminated in previous passes.
-}

genPrec env _ (UnOp _ Not e)            = text "!" <> genPrec env 4 e
genPrec env n e@(BinOp _ e1 And e2)     = parensIf (n > 3) (genPrec env 3 e1 <+> text "&&" <+> genPrec env 4 e2)
genPrec env n e@(BinOp _ e1 Or e2)      = parensIf (n > 2) (genPrec env 2 e1 <+> text "||" <+> genPrec env 3 e2)
genPrec env n (Cond _ e1 e e2)          = parensIf (n > 1) (genPrec env 2 e <+> text "?" <+> gen env e1 <+> text ":" <+> genPrec env 1 e2)
genPrec env _ e                         = gen env e

instance Gen OpArg where
    gen env (OpArg op e)            = gen env op <+> gen env e

instance Gen Bool where
    gen env b                       = text (show b)

instance Gen Integer where
    gen env i                       = text (show i)

instance Gen String where
    gen env s                       = text s

instance Gen Elem where
    gen env (Elem e)                = gen env e

instance Gen Pattern where
    gen env (PVar _ n _)            = gen env n

instance Gen Unary where
    gen env Not                     = text "not "
    gen env BNot                    = text "~"
    gen env UMinus                  = text "-"
    gen env UPlus                   = text "+"

instance Gen Binary where
    gen env Or                      = text "or"
    gen env And                     = text "and"
    gen env BOr                     = text "|"
    gen env BXor                    = text "^"
    gen env BAnd                    = text "&"
    gen env ShiftL                  = text "<<"
    gen env ShiftR                  = text ">>"
    gen env Plus                    = text "+"
    gen env Minus                   = text "-"
    gen env Mult                    = text "*"
    gen env MMult                   = text "@"
    gen env Div                     = text "/"
    gen env Mod                     = text "%"
    gen env EuDiv                   = text "//"
    gen env Pow                     = text "**"

instance Gen Comparison where
    gen env Lt                      = text "<"
    gen env Gt                      = text ">"
    gen env Eq                      = text "=="
    gen env GE                      = text ">="
    gen env LE                      = text "<="
    gen env NEq                     = text "!="
    gen env In                      = text "in"
    gen env NotIn                   = text "not in"
    gen env Is                      = text "is"
    gen env IsNot                   = text "is not"


instance Gen Aug where
    gen env PlusA                   = text "+="
    gen env MinusA                  = text "-="
    gen env MultA                   = text "*="
    gen env MMultA                  = text "@="
    gen env DivA                    = text "/="
    gen env ModA                    = text "%="
    gen env PowA                    = text "**="
    gen env BAndA                   = text "&="
    gen env BOrA                    = text "|="
    gen env BXorA                   = text "^="
    gen env ShiftLA                 = text "<<="
    gen env ShiftRA                 = text ">>="
    gen env EuDivA                  = text "//="

instance Gen TSchema where
    gen env (TSchema _ _ t)         = gen env t

instance Gen TVar where
    gen env (TV k n)                = word

instance Gen TCon where
    gen env (TC n ts)               = gen env (globalize env n)
    
instance Gen Type where
    gen env (TVar _ v)              = gen env v
    gen env (TCon  _ c)             = gen env c
    gen env (TFun _ _ p _ t)        = gen env t <+> parens (char '*') <+> parens (gen env p)
    gen env (TTuple _ pos _)        = parens (gen env pos)                                      -- TODO: replace!
    gen env (TUnion _ as)           = word
    gen env (TOpt _ t)              = gen env t
    gen env (TNone _)               = text "void*"
    gen env (TWild _)               = word
    gen env (TRow _ _ _ t TNil{})   = gen env t
    gen env (TRow _ _ _ t r)        = gen env t <> comma <+> gen env r
    gen env (TNil _ _)              = empty

