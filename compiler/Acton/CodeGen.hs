{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, DeriveGeneric #-}
module Acton.CodeGen where

import qualified Data.Set
import qualified Acton.Env
import Utils
import Pretty
import Acton.Syntax
import Acton.Builtin
import Acton.Printer
import Acton.Prim
import Acton.Env
import Acton.QuickType
import Prelude hiding ((<>))

generate                            :: Acton.Env.Env0 -> Module -> IO (String,String)
generate env m                      = do return (h,c)
  where h                           = render $ hModule env0 m
        c                           = render $ cModule env0 m
        env0                        = genEnv env (modname m)


-- Environment --------------------------------------------------------------------------------------

genEnv env0 m                       = setX env0 GenX{ globalX = [] }

type GenEnv                         = EnvF GenX

data GenX                           = GenX { globalX :: [Name] }

global env                          = globalX $ envX env

gdefine te env                      = modX env1 $ \x -> x{ globalX = dom te ++ global env }
  where env1                        = define te env

ldefine te env                      = modX env1 $ \x -> x{ globalX = global env \\ dom te }
  where env1                        = define te env


-- Header -------------------------------------------------------------------------------------------

hModule env (Module qn imps stmts)  = vcat (map (gen env) imps) $+$
                                      hSuite env stmts

hSuite env []                       = empty
hSuite env (s:ss)                   = hStmt env s $+$ hSuite (gdefine (envOf s) env) ss

hStmt env (Assign _ [PVar _ n (Just t)] _)
                                    = text "extern" <+> gen env t <+> genTopName env n <> semi
hStmt env (Decl _ ds)               = vmap (stub env1) ds $+$
                                      vmap (typedef env1) ds $+$
                                      vmap (decl env1) ds $+$
                                      vmap (methstub env1) ds
  where env1                        = gdefine (envOf ds) env
hStmt env _                         = empty

stub env (Class _ n q a b)          = text "struct" <+> genTopName env n <> semi
stub env Def{}                      = empty

typedef env (Class _ n q a b)       = text "typedef" <+> text "struct" <+> genTopName env n <+> char '*' <> genTopName env n <> semi
typedef env Def{}                   = empty

decl env (Class _ n q a b)          = (text "struct" <+> classname env n <+> char '{') $+$ 
                                      nest 4 (fields env tc) $+$ 
                                      char '}' <> semi $+$
                                      (text "struct" <+> genTopName env n <+> char '{') $+$ 
                                      nest 4 (properties env tc) $+$ 
                                      char '}' <> semi
  where tc                          = TC (NoQ n) [ tVar v | Quant v _ <- q ]
decl env (Def _ n q p _ a b _ fx)   = funsig env (gname env n) (TFun l0 fx (prowOf p) kwdNil (fromJust a))

methstub env (Class _ n q a b)      = text "extern" <+> text "struct" <+> classname env n <+> methodsname env n <> semi
methstub env Def{}                  = empty

fields env c                        = vmap field te
  where te                          = fullAttrEnv env c
        field (n, NDef sc Static)   = funsig env n (sctype sc) <> semi
        field (n, NDef sc NoDec)    = methsig env c n (sctype sc) <> semi
        field (n, NVar t)           = varsig env n t <> semi
        field (n, NSig sc Property) = empty
        field (n, NSig sc _)        = parens (text "signature" <+> gen env n)

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

classname env n                     = genTopName env n <> text "$class"

methodsname env n                   = genTopName env n <> text "$methods"


-- Implementation -----------------------------------------------------------------------------------

cModule env (Module qn imps stmts)  = text "#include" <+> doubleQuotes (gen env qn <> text ".h") $+$
                                      cSuite env stmts

cSuite env []                       = empty
cSuite env (s:ss)                   = cStmt env s $+$ cSuite (gdefine (envOf s) env) ss

cStmt env (Assign _ [PVar _ n (Just t)] _)
                                    = gen env t <+> genTopName env n <> semi
cStmt env (Decl _ ds)               = vcat $ map (gen env1) ds
  where env1                        = gdefine (envOf ds) env
cStmt env _                         = empty


class Gen a where
    gen                             :: GenEnv -> a -> Doc


instance (Gen a) => Gen (Maybe a) where
    gen env x                       = maybe empty (gen env) x


instance Gen Import where
    gen env (Import _ ms)           = vcat [ text "#include" <+> doubleQuotes (gen env m <> text ".h") | m <- ms ]

instance Gen ModuleItem where
    gen env (ModuleItem m Nothing)  = gen env m

instance Gen ModName where
    gen env (ModName ns)            = hcat $ punctuate (char '$') $ map (gen env) ns

instance Gen QName where
    gen env (GName m n)
      | m == mPrim                  = char '$' <> text (nstr n)
      | m == mBuiltin               = char '$' <> text (nstr n)
      | otherwise                   = gen env m <> text "$$" <> text (mkCident $ nstr n)
    gen env (NoQ n)
      | n `elem` global env         = gen env (gname env n)
      | otherwise                   = gen env n
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

genSuite env ss                     = nest 4 $ genS env ss
  where genS env []                 = empty
        genS env (s:ss)             = gen env s $+$ genS (ldefine (envOf s) env) ss

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
    gen env (While _ e b [])        = (text "while" <+> parens (gen env e) <+> char '{') $+$ genSuite env b $+$ char '}'
    gen env _                       = empty

genBranch env kw (Branch e b)       = (text kw <+> parens (gen env e) <+> char '{') $+$ genSuite env b $+$ char '}'

genElse env []                      = empty
genElse env b                       = (text "else" <+> char '{') $+$ genSuite env b $+$ char '}'

instance Gen Decl where
    gen env (Def _ n q p KwdNIL a b d m)
                                    = (gen env a <+> genTopName env n <+> parens (gen env p) <+> char '{')
                                      $+$ genSuite env1 b $+$ char '}'
      where env1                    = ldefine (envOf p) $ defineTVars q env
    gen env (Class _ n q a b)       = (text "struct" <+> genTopName env n <+> nonEmpty parens commaList a <+> char '{') $+$
                                      genSuite env b $+$ char '}'
      where env1                    = defineSelf (NoQ n) q $ defineTVars q env



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

instance Gen Expr where
    gen env (Var _ n)               = gen env n
    gen env (Int _ _ str)           = text str
    gen env (Float _ _ str)         = text str
    gen env (Imaginary _ _ str)     = text str
    gen env (Bool _ v)              = gen env v
    gen env (None _)                = text "None"
    gen env (NotImplemented _)      = text "NotImplemented"
    gen env (Ellipsis _)            = text "..."
    gen env (Strings _ [s])         = text s
    gen env (BStrings _ [s])        = text s
    gen env (Call _ e ps _)         = gen env e <> parens (gen env ps)
    gen env (TApp _ e ts)           = gen env e
--    gen env (Cond _ e1 e e2)        = gen env e <+> text "?" <+> gen env e1 <+> text ":" <+> gen env e2
    gen env (IsInstance _ e c)      = gen env primISINSTANCE <> parens (gen env e <> comma <+> gen env c)
--    gen env (BinOp _ e1 Or e2)      = gen env e1 <+> text "||" <+> gen env e2
--    gen env (BinOp _ e1 And e2)     = gen env e1 <+> text "&&" <+> gen env e2
--    gen env (UnOp _ Not e)          = text "!" <> gen env e
    gen env (Dot _ e n)             = gen env e <> text "->" <> gen env n
    gen env (Rest _ e n)            = text "CodeGen for tuple tail not implemented" --gen env e <> brackets (pretty i)
    gen env (DotI _ e i)            = gen env e <> brackets (pretty i)
    gen env (RestI _ e i)           = text "CodeGen for tuple tail not implemented" --gen env e <> brackets (pretty i)
    gen env (Yield _ e)             = text "yield" <+> gen env e
    gen env (YieldFrom _ e)         = text "yield" <+> text "from" <+> gen env e
    gen env (Tuple _ pargs kargs)   = parens (gen env pargs <+> gen env kargs)
    gen env (List _ es)             = brackets (commaList es)
    gen env (Paren _ e)             = gen env e
    gen env e                       = genPrec env 0 e -- BinOp, UnOp  and Cond

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
genPrec env n (Cond _ e1 e e2)          = parensIf (n > 1) (genPrec env 2 e1 <+> text "?" <+> gen env e <+> text ":" <+> genPrec env 1 e2)
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
    gen env (PVar _ n Nothing)      = gen env n
    gen env (PVar _ n (Just t))     = gen env t <+> gen env n

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
    gen env (TC n ts)               = gen env n
    
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

