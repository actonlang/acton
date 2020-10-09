{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, DeriveGeneric #-}
module Acton.CodeGen where

import qualified Data.Set
import qualified Data.Hashable
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

generate                            :: Acton.Env.Env0 -> Module -> IO String
generate env m                      = return $ render $ gen (genEnv env (modname m)) m

class Gen a where
    gen                             :: GenEnv -> a -> Doc

type GenEnv                         = EnvF GenX

data GenX                           = GenX { thismoduleX :: ModName }

thismodule env                      = thismoduleX $ envX env

genEnv env0 m                       = setX env0 GenX{ thismoduleX = m }


instance (Gen a) => Gen (Maybe a) where
    gen env x                       = maybe empty (gen env) x

instance Gen Module where
    gen env (Module qn imps stmts)  = vcat (map (gen env) imps) $+$ blank $+$ vcat (map (gen env) stmts)

instance Gen Import where
    gen env (Import _ ms)           = vcat [ text "#include" <+> doubleQuotes (gen env m <> text ".h") | m <- ms ]

instance Gen ModuleItem where
    gen env (ModuleItem m Nothing)  = gen env m

instance Gen ModName where
    gen env (ModName ns)            = hcat $ punctuate (char '$') $ map (gen env) ns

instance Gen QName where
    gen env (QName m n)
      | m == mPrim                  = char '$' <> text (nstr n)
      | m == mBuiltin               = char '$' <> text (nstr n)
      | otherwise                   = gen env m <> text "$$" <> gen env n
    gen env (NoQ n)                 = gen env n

instance Gen Name where
    gen env nm
      | isCident str                = text str
      | otherwise                   = text "_$" <> text str'
      where str                     = nstr nm
            str'                    = show (Data.Hashable.hash str) ++ filter isAlpha str
            isCident s@(c:cs)       = isAlpha c && all isAlphaNum cs && not (isCkeyword s)
            isAlpha c               = c `elem` ['a'..'z'] || c `elem` ['A'..'Z'] || c `elem` ['_','$']
            isAlphaNum c            = isAlpha c || c `elem` ['0'..'9']
            isCkeyword x            = x `Data.Set.member` rws
            rws                     = Data.Set.fromDistinctAscList [
                                        "auto",     "break",    "case",     "char",     "continue", "default",
                                        "default",  "do",       "double",   "else",     "enum",     "extern",
                                        "float",    "for",      "goto",     "if",       "int",      "long",
                                        "register", "return",   "short",    "signed",   "sizeof",   "static",
                                        "struct",   "switch",   "typedef",  "union",    "unsigned", "void",
                                        "volatile", "while"
                                      ]

genQName env n                      = gen env (thismodule env) <> char '$' <> gen env n

word                                = text "$WORD"
        
genSuite env ss                     = nest 4 $ genS env ss
  where genS env []                 = empty
        genS env (s:ss)             = gen env s $+$ genS (define (envOf s) env) ss

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
    gen env (Decl _ ds)             = vcat $ map (gen env1) ds
      where env1                    = define (envOf ds) env
    gen env (Signature _ vs sc d)   = empty

genBranch env kw (Branch e b)       = (text kw <+> parens (gen env e) <+> char '{') $+$ genSuite env b $+$ char '}'

genElse env []                      = empty
genElse env b                       = (text "else" <+> char '{') $+$ genSuite env b $+$ char '}'

instance Gen Decl where
    gen env (Def _ n q p KwdNIL a b d m)
                                    = (gen env a <+> genQName env n <+> parens (gen env p) <+> char '{')
                                      $+$ genSuite env1 b $+$ char '}'
      where env1                    = define (envOf p) $ defineTVars q env
    gen env (Class _ n q a b)       = (text "struct" <+> genQName env n <+> nonEmpty parens commaList a <+> char '{') $+$ 
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
    gen env (TC n _)                = gen env n

instance Gen UType where
    gen env (UCon n)                = gen env n
    gen env (ULit str)              = text str

genRow env (TRow _ _ _ t (TNil _ _))    = gen env t
genRow env (TRow _ _ _ t p)         = gen env t <> comma <+> genRow env p
genRow env (TNil _ _)               = empty
    
instance Gen Type where
    gen env (TVar _ v)              = gen env v
    gen env (TCon  _ c)             = gen env c
    gen env (TFun _ _ p _ t)        = parens (genRow env p) <+> text "->" <+> gen env t
    gen env (TTuple _ pos _)        = parens (genRow env pos)
    gen env (TUnion _ as)           = parens (vbarSep (gen env) as)
      where vbarSep f               = hsep . punctuate (space <> char '|') . map f
    gen env (TOpt _ t)              = gen env t
    gen env (TNone _)               = text "0"
    gen env (TWild _)               = word
    gen env row                     = genRow env row

