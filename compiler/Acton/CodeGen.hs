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

generate                            :: Acton.Env.Env -> Module -> IO String
generate env m                      = return $ render $ gen (genEnv env) m

class Gen a where
    gen                             :: Env -> a -> Doc

data Env                            = Env { thismodule :: ModName }

genEnv env                          = Env { thismodule = Acton.Env.defaultmod env }


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
      | m == mPrim                  = genPrimName n
      | otherwise                   = gen env m <> char '$' <> gen env n
    gen env (NoQual n)              = gen env n

instance Gen Name where
    gen env nm
      | isCident str                = text str
      | otherwise                   = text "___" <> text str' <> text "___"
      where str                     = nstr nm
            str'                    = show (Data.Hashable.hash str) ++ filter isAlpha str
            isCident s@(c:cs)       = isAlpha c && all isAlphaNum cs && not (isCkeyword s)
            isAlpha c               = c `elem` ['a'..'z'] || c `elem` ['A'..'Z'] || c == '_'
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

word                                = text "WORD"

genSuite env ss                     = nest 4 $ vcat $ map (gen env) ss

instance Gen Stmt where
    gen env (Expr _ e)              = gen env e <> semi
    gen env (Assign _ [p] e)        = gen env p <+> equals <+> gen env e <> semi
    gen env (Update _ [t] e)        = gen env t <+> equals <+> gen env e <> semi
    gen env (IUpdate _ t op e)      = gen env t <+> gen env op <+> gen env e <> semi                    -- TODO: remove
    gen env (Pass _)                = empty
    gen env (Return _ Nothing)      = text "return" <+> gen env eNone <> semi
    gen env (Return _ (Just e))     = text "return" <+> gen env e <> semi
    gen env (Raise _ e)             = text "raise" <+> gen env e                                        -- TODO: remove
    gen env (Break _)               = text "break" <> semi
    gen env (Continue _)            = text "continue" <> semi
    gen env (If _ (b:bs) b2)        = genBranch env "if" b $+$ vmap (genBranch env "else if") bs $+$ genElse env b2
    gen env (While _ e b [])        = (text "while" <+> parens (gen env e) <+> char '{') $+$ genSuite env b $+$ char '}'
    gen env (Try _ b hs _ _)        = text "try" <> colon $+$ genSuite env b $+$ vmap (gen env) hs      -- TODO: remove
    gen env (Decl _ ds)             = vcat $ map (gen env) ds

genBranch env kw (Branch e b)       = (text kw <+> parens (gen env e) <+> char '{') $+$ genSuite env b $+$ char '}'

genElse env []                      = empty
genElse env b                       = (text "else" <+> char '{') $+$ genSuite env b $+$ char '}'

instance Gen Decl where
    gen env (Def _ n q ps ks a b m) = (gen env a <+> genQName env n <+> parens (gen env ps) <+> char '{')
                                      $+$ genSuite env b $+$ char '}'
    gen env (Class _ n q a b)       = text "class" <+> gen env n <+> nonEmpty brackets commaList q <+>
                                      nonEmpty parens commaList a <> colon $+$ genSuite env b
    gen env (Signature _ vs sc)     = empty



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
    gen env (Cond _ e1 e e2)        = gen env e1 <+> text "if" <+> gen env e <+> text "else" <+> gen env e2
    gen env (BinOp _ e1 o e2)       = gen env e1 <+> gen env o <+> gen env e2               -- TODO: remove
    gen env (CompOp _ e ops)        = gen env e <+> hsep (map (gen env) ops)                -- TODO: remove
    gen env (UnOp _ o e)            = gen env o <> gen env e                                -- TODO: remove
    gen env (Dot _ e n)             = gen env e <> text "->" <> gen env n
    gen env (DotI _ e i False)      = gen env e <> brackets (pretty i)
    gen env (DotI _ e i True)       = text "CodeGen for tuple tail not implemented" --gen env e <> brackets (pretty i)
    gen env (Yield _ e)             = text "yield" <+> gen env e
    gen env (YieldFrom _ e)         = text "yield" <+> text "from" <+> gen env e
    gen env (Tuple _ pargs)         = gen env pargs
    gen env (Record _ KwdNil)       = text "record" <> parens empty
    gen env (Record _ kargs)        = parens (gen env kargs)
    gen env (List _ es)             = brackets (commaList es)
    gen env (Dict _ es)             = braces (commaList es)
    gen env (Set _ [])              = text "set" <> parens empty
    gen env (Set _ es)              = braces (commaList es)
    gen env (Paren _ e)             = gen env e

instance Gen OpArg where
    gen env (OpArg op e)            = gen env op <+> gen env e

instance Gen Bool where
    gen env b                       = text (show b)

instance Gen Integer where
    gen env i                       = text (show i)

instance Gen String where
    gen env s                       = text s

instance Gen a => Gen (Op a) where
    gen env (Op _ a)                = gen env a

instance Gen Exception where
    gen env (Exception e1 e2)       = gen env e1 <+> nonEmpty (text "from" <+>) (gen env) e2

instance Gen Handler where
    gen env (Handler ex b)          = gen env ex <> colon $+$ genSuite env b
    
instance Gen Except where
    gen env (ExceptAll _)           = text "except"
    gen env (Except _ x)            = text "except" <+> gen env x
    gen env (ExceptAs _ x n)        = text "except" <+> gen env x <+> text "as" <+> gen env n

genAnn env Nothing                  = empty
genAnn env (Just a)                 = colon <+> gen env a

instance Gen Elem where
    gen env (Elem e)                = gen env e
    gen env (Star e)                = text "*" <> gen env e

instance Gen Assoc where
    gen env (Assoc k v)             = gen env k <> colon <+> gen env v
    gen env (StarStar e)            = text "**" <> gen env e

instance Gen WithItem where
    gen env (WithItem e p)          = gen env e <+> nonEmpty (text "as" <+>) (gen env) p

instance Gen Slice where
    gen env (Sliz _ a b c)          = gen env a <> colon <> gen env b <> gen env c

instance Gen Comp where
    gen env (CompFor _ p e c)       = text "for" <+> gen env p <+> text "in" <+> gen env e <+> gen env c
    gen env (CompIf _ e c)          = text "if" <+> gen env e <+> gen env c
    gen env NoComp                  = empty


instance Gen PosPat where
    gen env (PosPat p PosPatNil)    = gen env p
    gen env (PosPat p ps)           = gen env p <> comma <+> gen env ps
    gen env (PosPatStar p)          = text "*" <> gen env p
    gen env PosPatNil               = empty

instance Gen Pattern where
    gen env (PVar _ n Nothing)      = gen env n
    gen env (PVar _ n (Just t))     = gen env t <+> gen env n
    gen env (PParen _ p)            = gen env p

instance Gen Target where
    gen env (TaVar _ n)             = gen env n
    gen env (TIndex _ e ix)         = gen env e <> brackets (commaList ix)
    gen env (TSlice _ e sl)         = gen env e <> brackets (commaList sl)
    gen env (TDot _ e n)            = gen env e <> text "->" <> gen env n
    gen env (TDotI _ e i tl)        = undefined
    gen env (TParen _ p)            = gen env p

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
    gen env (TSchema _ _ t _)       = gen env t

instance Gen TVar where
    gen env (TV k n)                = word

instance Gen TCon where
    gen env (TC n _)                = gen env n

instance Gen UType where
    gen env (UCon n)                = gen env n
    gen env (ULit str)              = text str

genRow env (TRow _ _ t (TNil _))    = gen env t
genRow env (TRow _ _ t p)           = gen env t <> comma <+> genRow env p
genRow env (TNil _)                 = empty
    
instance Gen Type where
    gen env (TVar _ v)              = gen env v
    gen env (TCon  _ c)             = gen env c
    gen env (TAt  _ c)              = text "@" <> gen env c
    gen env (TFun _ _ p _ t)        = parens (genRow env p) <+> text "->" <+> gen env t
    gen env (TTuple _ pos)          = parens (genRow env pos)
    gen env (TRecord _ kw)          = parens (genRow env kw)
    gen env (TUnion _ as)           = parens (vbarSep (gen env) as)
      where vbarSep f               = hsep . punctuate (space <> char '|') . map f
    gen env (TOpt _ t)              = gen env t
    gen env (TNone _)               = text "0"
    gen env (TWild _)               = word
    gen env row                     = genRow env row

