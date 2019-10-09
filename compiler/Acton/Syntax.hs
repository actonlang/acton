{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, DeriveGeneric #-}
module Acton.Syntax where

import Pretty
import Utils
import qualified Data.Binary
import qualified Data.Set
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import GHC.Generics (Generic)
import Prelude hiding((<>))


version :: [Int]
version = [0,1]

data Module     = Module        QName [Import] [Stmt] deriving (Eq,Show)

data Import     = Import        { iloc::SrcLoc, modules::[ModuleItem] }
                | FromImport    { iloc::SrcLoc, modul::ModRef, items::[ImportItem] }
                | FromImportAll { iloc::SrcLoc, modul::ModRef }
                deriving (Show)

type Suite      = [Stmt]

data Stmt       = Expr          { sloc::SrcLoc, expr::Expr }
                | Assign        { sloc::SrcLoc, patterns::[Pattern], expr::Expr }
                | AugAssign     { sloc::SrcLoc, pattern::Pattern, aop::Op Aug, expr::Expr }
                | Assert        { sloc::SrcLoc, exprs::[Expr] }
                | TypeSig       { sloc::SrcLoc, vars :: [Name], typ :: CType}
                | Pass          { sloc::SrcLoc }
                | Delete        { sloc::SrcLoc, pattern::Pattern }
                | Return        { sloc::SrcLoc, optExpr::Maybe Expr }
                | Raise         { sloc::SrcLoc, except::Maybe Exception }
                | Break         { sloc::SrcLoc }
                | Continue      { sloc::SrcLoc }
                | If            { sloc::SrcLoc, branches::[Branch], els::Suite }
                | While         { sloc::SrcLoc, expr::Expr, body::Suite, els::Suite }
                | For           { sloc::SrcLoc, pattern::Pattern, expr::Expr, body::Suite, els::Suite }
                | Try           { sloc::SrcLoc, body::Suite, handlers::[Handler], els::Suite, finally::Suite }
                | With          { sloc::SrcLoc, context::[WithItem], body::Suite }
                | Data          { sloc::SrcLoc, mbpat::Maybe Pattern, dsuite::Suite }
                | Extends       { sloc::SrcLoc, qname::QName, args::[Arg] }
                | VarAssign     { sloc::SrcLoc, patterns::[Pattern], expr::Expr }
                | Decl          { sloc::SrcLoc, decls::[Decl] }
                deriving (Show)

data Decl       = Def           { dloc::SrcLoc, dname:: Name, pars::Params Param, ann::(Maybe CType), dbody::Suite, modif::Modif }
                | Actor         { dloc::SrcLoc, dname:: Name, pars::Params Param, ann::(Maybe CType), dbody::Suite }
                | Class         { dloc::SrcLoc, dname::Name, dargs::[Arg], dbody::Suite }
                | Decorator     { dloc::SrcLoc, dqname::QName, dargs::[Arg], decl::Decl }
                deriving (Show)

data Expr       = Var           { eloc::SrcLoc, var::Name }
                | Int           { eloc::SrcLoc, ival::Integer, lexeme::String }
                | Float         { eloc::SrcLoc, dval::Double, lexeme::String }
                | Imaginary     { eloc::SrcLoc, dval::Double, lexeme::String }
                | Bool          { eloc::SrcLoc, bval::Bool }
                | None          { eloc::SrcLoc }
                | NotImplemented{ eloc::SrcLoc }
                | Ellipsis      { eloc::SrcLoc }
                | Strings       { eloc::SrcLoc, sval::[String] }
                | BStrings      { eloc::SrcLoc, sval::[String] }
                | UStrings      { eloc::SrcLoc, sval::[String] }
                | Call          { eloc::SrcLoc, function::Expr, arguments::[Arg] }
                | Await         { eloc::SrcLoc, exp1::Expr }
                | Ix            { eloc::SrcLoc, exp1::Expr, index::[Index] }
                | Cond          { eloc::SrcLoc, exp1::Expr, cond::Expr, exp2::Expr }
                | BinOp         { eloc::SrcLoc, exp1::Expr, bop::Op Binary, exp2::Expr }
                | CompOp        { eloc::SrcLoc, exp1::Expr, ops::[OpArg] }
                | UnOp          { eloc::SrcLoc, uop::Op Unary, exp1::Expr }
                | Dot           { eloc::SrcLoc, exp1::Expr, attr::Name }
                | DotI          { eloc::SrcLoc, exp1::Expr, ival::Integer }
                | Lambda        { eloc::SrcLoc, params::Params Param, exp1::Expr }
                | Yield         { eloc::SrcLoc, yexp1::Maybe Expr }
                | YieldFrom     { eloc::SrcLoc, yfrom::Expr }
                | Tuple         { eloc::SrcLoc, elems::[Elem Expr] }
                | Generator     { eloc::SrcLoc, elem1::Elem Expr, comp::Comp }
                | List          { eloc::SrcLoc, elems::[Elem Expr] }
                | ListComp      { eloc::SrcLoc, elem1::Elem Expr, comp::Comp }
                | Dict          { eloc::SrcLoc, assocs::[Assoc] }
                | DictComp      { eloc::SrcLoc, assoc1::Assoc, comp::Comp }
                | Set           { eloc::SrcLoc, elems::[Elem Expr] }
                | SetComp       { eloc::SrcLoc, elem1::Elem Expr, comp::Comp }
                | Struct        { eloc::SrcLoc, fields::[Field] }
                | StructComp    { eloc::SrcLoc, var::Name, exp1::Expr, comp::Comp }
                | Paren         { eloc::SrcLoc, exp1::Expr }
                deriving (Show)

data Pattern    = PVar          { ploc::SrcLoc, pn::Name, pann::Maybe CType }
                | PIx           { ploc::SrcLoc, pexp::Expr, pix::[Index] }
                | PDot          { ploc::SrcLoc, pexp::Expr, pn::Name }
                | PParen        { ploc::SrcLoc, pat::Pattern }
                | PTuple        { ploc::SrcLoc, pelems::[Elem Pattern] }
                | PList         { ploc::SrcLoc, pelems::[Elem Pattern] }
                | PData         { ploc::SrcLoc, pn::Name, pixs::[Expr] }
                deriving (Show)
                
data Name       = Name SrcLoc String | Internal Int String deriving (Generic)

nloc (Name l _) = l
nloc Internal{} = NoLoc

nstr (Name _ s) = s
nstr (Internal i s) = show i ++ "____" ++ s

mkqname str     = QName (Name NoLoc str) []

data QName      = QName Name [Name] deriving (Show,Eq)
data ModuleItem = ModuleItem QName (Maybe Name) deriving (Show,Eq)
data ModRef     = ModRef (Int, Maybe QName) deriving (Show,Eq)
data ImportItem = ImportItem Name (Maybe Name) deriving (Show,Eq)
data Op a       = Op SrcLoc a deriving (Show)
data Exception  = Exception Expr (Maybe Expr) deriving (Show,Eq)
data Branch     = Branch Expr Suite deriving (Show,Eq)
data Handler    = Handler Except Suite deriving (Show,Eq)
data Except     = ExceptAll SrcLoc | Except SrcLoc Expr | ExceptAs SrcLoc Expr Name deriving (Show)
data Params a   = Params { positional::[a], posExt::StarPar, kwOnly::[a], kwExt::StarPar} deriving (Show,Eq)
data Param      = Param Name (Maybe CType) (Maybe Expr) deriving (Show,Eq)
data StarPar    = StarPar SrcLoc Name (Maybe CType) | NoStar deriving (Show)

data Elem e     = Elem e | Star e deriving (Show,Eq)
data Assoc      = Assoc Expr Expr | StarStarAssoc Expr deriving (Show,Eq)
data Field      = Field Name Expr | StarStarField Expr deriving (Show,Eq)
data Arg        = Arg Expr | KwArg Name Expr | StarArg Expr | StarStarArg Expr deriving (Show,Eq)

data OpArg      = OpArg (Op Comparison) Expr deriving (Eq,Show)
data Index      = Index SrcLoc Expr | Slice SrcLoc (Maybe Expr) (Maybe Expr) (Maybe (Maybe Expr)) deriving (Show)
data Comp       = CompFor SrcLoc Pattern Expr Comp | CompIf SrcLoc Expr Comp | NoComp deriving (Show)
data WithItem   = WithItem Expr (Maybe Pattern) deriving (Show,Eq)

data Unary      = Not|BNot|UMinus|UPlus deriving (Show,Eq)
data Binary     = Or|And|BOr|BXor|BAnd|ShiftL|ShiftR|Plus|Minus|Mult|MMult|Div|Mod|EuDiv|Pow deriving (Show,Read,Eq,Generic)
data Comparison = Lt|Gt|Eq|GE|LE|LtGt|NEq|In|NotIn|Is|IsNot deriving (Show,Eq)
data Aug        = PlusA|MinusA|MultA|MMultA|DivA|ModA|PowA|BAndA|BOrA|BXorA|ShiftLA|ShiftRA|EuDivA deriving (Show,Eq)

data Annot      = Annot Expr
                | Type SrcLoc Type
                 deriving (Show,Eq)

data Modif      = Sync Bool | Async | NoMod deriving (Show,Eq)

data Type       = TVar      TVar
                -- Types
                | TFun      Effect Row Type
                | TStruct   Row
                | TTuple    Row
                | TDict     Type Type
                | TList     Type
                | TSet      Type
                | TMsg      Type
                | TStr
                | TInt
                | TFloat
                | TBool
                | TNone
                -- Rows
                | RPos      Type Row
                | RStar1    Type Row
                | RKwd      Name Type Row
                | RStar2    Type Row
                | RNil
                -- Polymorphism
                | TSchema   [TVar] [Constraint] Type
                deriving (Eq,Show,Read,Generic)

data Constraint = CEqu      { cloc::SrcLoc, vn::Int, t1::Type, t2::Type }
                | CIn       { cloc::SrcLoc, vn::Int, t1::Type, t2::Type }
                | CDot      { cloc::SrcLoc, vn::Int, t1::Type, cname::Name, t2::Type }
                | CIx       { cloc::SrcLoc, vn::Int, t1::Type, t2::Type, t3::Type }
                | CMod      { cloc::SrcLoc, vn::Int, t1::Type, t2::Type }
                | CPlus     { cloc::SrcLoc, vn::Int, t1::Type }
                | CNum      { cloc::SrcLoc, vn::Int, t1::Type }
                | CBool     { cloc::SrcLoc, vn::Int, t1::Type }
                deriving (Eq,Show,Read,Generic)

type TVar           = [Int]

type Row            = Type

type Effect         = Row

type Scheme         = Type

type Substitution   = [(TVar,Type)]

data CVar       = CVar Name deriving (Eq,Show,Read) -- the Name is an uppercase letter, optionally followed by digits.

data UType      = UInt | UFloat | UBool | UStr | UStrCon String deriving (Eq,Show,Read)

type CEffect    = [Name]

data TPar       = PosPar CType | KwPar Name CType deriving  (Eq,Show)

data CType      = CTVar     { tloc :: SrcLoc, cvar :: CVar }
                | CTFun     { tloc :: SrcLoc, ceffect :: CEffect, tpars :: Params TPar, restype :: CType }
                | CTTuple   { tloc :: SrcLoc, postypes :: [CType], star1types :: Maybe (Maybe CVar) }
                | CTStruct  { tloc :: SrcLoc, kwdtypes :: [(Name,CType)], star2types :: Maybe (Maybe CVar) }
                | CTFList   { tloc :: SrcLoc, elemtype :: CType }
                | CTFSet    { tloc :: SrcLoc, elemtype :: CType }
                | CTFDict   { tloc :: SrcLoc, keytype :: CType, valtype :: CType }
                | CTOpt     { tloc :: SrcLoc, opttype :: CType }
                | CTUnion   { tloc :: SrcLoc, alts :: [UType], uext :: Maybe CVar }
                | CTClass   { tloc :: SrcLoc, classname :: Name, targs :: [CType] }  -- dict,set,list here or as separate constructors?
                | CTStr     { tloc :: SrcLoc }
                | CTInt     { tloc :: SrcLoc }
                | CTFloat   { tloc :: SrcLoc }
                | CTBool    { tloc :: SrcLoc }
                | CTNone    { tloc :: SrcLoc }
                | CTQual    { tloc :: SrcLoc, cconstraints :: [CType], qtype :: CType } -- type for cconstraints to be changed
                deriving (Eq,Show,Generic)

instance Data.Binary.Binary Type
instance Data.Binary.Binary Name
instance Data.Binary.Binary Constraint
instance Data.Binary.Binary Binary


-- SrcInfo ------------------

type SrcInfo        = [InfoTag]

data InfoTag        = GEN   SrcLoc Type
                    | INS   SrcLoc Type
                    deriving (Eq,Show)

lookupGEN l info    = listToMaybe [ t | GEN l' t <- info, l' == l ]

lookupINS l info    = listToMaybe [ t | INS l' t <- info, l' == l ]

instance Pretty InfoTag where
    pretty (GEN l t)    = text "GEN" <+> parens (pretty l) <> colon <+> pretty t
    pretty (INS l t)    = text "INS" <+> parens (pretty l) <> colon <+> pretty t


-- Locations ----------------

instance HasLoc Import where
    loc                 = iloc

instance HasLoc Stmt where
    loc                 = sloc

instance HasLoc Decl where
    loc                 = dloc

instance HasLoc Expr where
    loc                 = eloc

instance HasLoc Name where
    loc                 = nloc

instance HasLoc QName where
    loc (QName n ns)    = loc (n:ns)
    
instance HasLoc e => HasLoc (Elem e) where
    loc (Elem e)        = loc e
    loc (Star e)        = loc e

instance HasLoc Assoc where
    loc (Assoc k v)     = loc k `upto` loc v
    loc (StarStarAssoc e) = loc e

instance HasLoc Pattern where
    loc                 = ploc


-- Eq -------------------------

instance Eq Import where
    x@Import{}          ==  y@Import{}          = modules x == modules y
    x@FromImport{}      ==  y@FromImport{}      = modul x == modul y && items x == items y
    x@FromImportAll{}   ==  y@FromImportAll{}   = modul x == modul y
    _                   ==  _                   = False

instance Eq Stmt where
    x@Expr{}            ==  y@Expr{}            = expr x == expr y
    x@Assign{}          ==  y@Assign{}          = patterns x == patterns y && expr x == expr y
    x@AugAssign{}       ==  y@AugAssign{}       = pattern x == pattern y && aop x == aop y && expr x == expr y
    x@Assert{}          ==  y@Assert{}          = exprs x == exprs y
    x@Pass{}            ==  y@Pass{}            = True
    x@Delete{}          ==  y@Delete{}          = pattern x == pattern y
    x@Return{}          ==  y@Return{}          = optExpr x == optExpr y
    x@Raise{}           ==  y@Raise{}           = except x == except y
    x@Break{}           ==  y@Break{}           = True
    x@Continue{}        ==  y@Continue{}        = True
    x@If{}              ==  y@If{}              = branches x == branches y && els x == els y
    x@While{}           ==  y@While{}           = expr x == expr y && body x == body y && els x == els y
    x@For{}             ==  y@For{}             = pattern x == pattern y && expr x == expr y && body x == body y
                                                  && els x == els y
    x@Try{}             ==  y@Try{}             = body x == body y && handlers x == handlers y && els x == els y
                                                  && finally x == finally y
    x@With{}            ==  y@With{}            = context x == context y && body x == body y
    x@Data{}            ==  y@Data{}            = mbpat x == mbpat y && dsuite x == dsuite y
    x@Extends{}         ==  y@Extends{}         = qname x == qname y && args x == args y
    x@VarAssign{}       ==  y@VarAssign{}       = patterns x == patterns y && expr x == expr y
    x@Decl{}            ==  y@Decl{}            = decls x == decls y
    _                   ==  _                   = False

instance Eq Decl where
    Def _ n1 p1 a1 b1 m1    ==  Def _ n2 p2 a2 b2 m2    = n1 == n2 && p1 == p2 && a1 == a2 && b1 == b2 && m1 == m2
    Actor _ n1 p1 a1 b1     ==  Actor _ n2 p2 a2 b2     = n1 == n2 && p1 == p2 && a1 == a2 && b1 == b2
    Class _ n1 a1 b1        ==  Class _ n2 a2 b2        = n1 == n2 && a1 == a2 && b1 == b2
    Decorator _ n1 a1 d1    ==  Decorator _ n2 a2 d2    = n1 == n2 && a1 == a2 && d1 == d2

instance Eq Expr where
    x@Var{}             ==  y@Var{}             = var x == var y
    x@Int{}             ==  y@Int{}             = lexeme x == lexeme y
    x@Float{}           ==  y@Float{}           = lexeme x == lexeme y
    x@Imaginary{}       ==  y@Imaginary{}       = lexeme x == lexeme y
    x@Bool{}            ==  y@Bool{}            = bval x == bval y
    x@None{}            ==  y@None{}            = True
    x@NotImplemented{}  ==  y@NotImplemented{}  = True
    x@Ellipsis{}        ==  y@Ellipsis{}        = True
    x@Strings{}         ==  y@Strings{}         = sval x == sval y
    x@BStrings{}        ==  y@BStrings{}        = sval x == sval y
    x@UStrings{}        ==  y@UStrings{}        = sval x == sval y
    x@Call{}            ==  y@Call{}            = function x == function y && arguments x == arguments y
    x@Await{}           ==  y@Await{}           = exp1 x == exp1 y
    x@Ix{}              ==  y@Ix{}              = exp1 x == exp1 y && index x == index y
    x@Cond{}            ==  y@Cond{}            = exp1 x == exp1 y && cond x == cond y && exp2 x == exp2 y
    x@BinOp{}           ==  y@BinOp{}           = exp1 x == exp1 y && bop x == bop y && exp2 x == exp2 y
    x@CompOp{}          ==  y@CompOp{}          = exp1 x == exp1 y && ops x == ops y
    x@UnOp{}            ==  y@UnOp{}            = uop x == uop y && exp1 x == exp1 y
    x@Dot{}             ==  y@Dot{}             = exp1 x == exp1 y && attr x == attr y
    x@DotI{}            ==  y@DotI{}            = exp1 x == exp1 y && ival x == ival y
    x@Lambda{}          ==  y@Lambda{}          = params x == params y && exp1 x == exp1 y
    x@Tuple{}           ==  y@Tuple{}           = elems x == elems y
    x@Yield{}           ==  y@Yield{}           = yexp1 x == yexp1 y
    x@YieldFrom{}       ==  y@YieldFrom{}       = yfrom x == yfrom y
    x@Generator{}       ==  y@Generator{}       = elem1 x == elem1 y && comp x == comp y
    x@List{}            ==  y@List{}            = elems x == elems y
    x@ListComp{}        ==  y@ListComp{}        = elem1 x == elem1 y && comp x == comp y
    x@Dict{}            ==  y@Dict{}            = assocs x == assocs y
    x@DictComp{}        ==  y@DictComp{}        = assoc1 x == assoc1 y && comp x == comp y
    x@Set{}             ==  y@Set{}             = elems x == elems y
    x@SetComp{}         ==  y@SetComp{}         = elem1 x == elem1 y && comp x == comp y
    x@Struct{}          ==  y@Struct{}          = fields x == fields y
    x@StructComp{}      ==  y@StructComp{}      = var x == var y && exp1 x == exp1 y && comp x == comp y
    x@Paren{}           ==  y                   = exp1 x == y
    x                   ==  y@Paren{}           = x == exp1 y
    _                   ==  _                   = False

instance Eq Name where
    Name _ s1           == Name _ s2            = s1 == s2
    Internal i1 s1      == Internal i2 s2       = i1 == i2 && s1 == s2
    _                   == _                    = False

instance Eq a => Eq (Op a) where
    Op _ x              ==  Op _ y              = x == y

instance Eq Except where
    ExceptAll _         ==  ExceptAll _         = True
    Except _ e1         ==  Except _ e2         = e1 == e2
    ExceptAs _ e1 n1    ==  ExceptAs _ e2 n2    = e1 == e2 && n1 == n2
    _                   ==  _                   = False

instance Eq StarPar where
    StarPar _ n1 ann1   ==  StarPar _ n2 ann2   = n1 == n2 && ann1 == ann2
    NoStar              ==  NoStar              = True
    _                   ==  _                   = False

instance Eq Index where
    Index _ e1          ==  Index _ e2          = e1 == e2
    Slice _ a1 b1 c1    ==  Slice _ a2 b2 c2    = a1 == a2 && b1 == b2 && c1 == c2
    _                   ==  _                   = False
    
instance Eq Comp where
    CompFor _ p1 e1 c1  ==  CompFor _ p2 e2 c2  = p1 == p2 && e1 == e2 && c1 == c2
    CompIf _ e1 c1      ==  CompIf _ e2 c2      = e1 == e2 && c1 == c2
    NoComp              ==  NoComp              = True
    _                   ==  _                   = False

instance Eq Pattern where
    PVar _ n1 a1        == PVar _ n2 a2         = n1 == n2 && a1 == a2
    PTuple _ ps1        == PTuple _ ps2         = ps1 == ps2
    PList _ ps1         == PList _ ps2          = ps1 == ps2
    PIx _ e1 ix1        == PIx _ e2 ix2         = e1 == e2 && ix1 == ix2
    PDot _ e1 n1        == PDot _ e2 n2         = e1 == e2 && n1 == n2
    PData _ n1 ix1      == PData _ n2 ix2       = n1 == n2 && ix1 == ix2
    PParen _ p1         == p2                   = p1 == p2
    p1                  == PParen _ p2          = p1 == p2
    _                   == _                    = False


-- Show & Read ----------------

instance Show Name where
    show n              = show (nstr n)

instance Read Name where
    readsPrec p str     = [ (Name NoLoc s, str') | (s,str') <- readsPrec p str ]


-- Helpers ------------------

importsOf (Module _ imps _)         = impsOf imps
  where 
    impsOf []                       = []
    impsOf (Import _ mis : i)       = map mName mis ++ impsOf i
    impsOf (FromImport _ mr _ : i)  = mRef mr : impsOf i
    impsOf (FromImportAll _ mr : i) = mRef mr : impsOf i

    mName (ModuleItem qn _)         = qn

    mRef (ModRef (0,Just qn))       = qn
    mRef _                          = error "dot prefix in name of import modules not supported"

unop op e                           = UnOp l0 (Op l0 op) e
binop e1 op e2                      = BinOp l0 e1 (Op l0 op) e2
cmp e1 op e2                        = CompOp l0 e1 [OpArg (Op l0 op) e2]
tuple es                            = Tuple l0 (map Elem es)

mkStringLit s                       = Strings l0 ['\'' : s ++ "\'"]

qname2expr (QName n ns)             = foldl (\e m -> Dot (nloc n `upto` nloc m) e m) (Var (nloc n) n) ns

asyncFX                             = RKwd asyncKW TNone
syncFX                              = RKwd syncKW TNone
actFX                               = RKwd actKW TNone
mutFX                               = RKwd mutKW TNone

asyncKW                             = Name NoLoc "async"
syncKW                              = Name NoLoc "sync"
actKW                               = Name NoLoc "actor"
mutKW                               = Name NoLoc "mut"

elemcore (Elem e)                   = e
elemcore (Star e)                   = e

fieldcore (Field _ e)               = e
fieldcore (StarStarField e)         = e

paramcores (Params ps _ kw _)       = [ e | Param _ _ (Just e) <- ps ++ kw ]

argcore (Arg e)                     = e
argcore (StarArg e)                 = e
argcore (KwArg _ e)                 = e
argcore (StarStarArg e)             = e

slices ix                           = [ s | s@Slice{} <- ix ]

isPosArg (Arg _)                    = True
isPosArg (StarArg _)                = True
isPosArg _                          = False

isIdent s@(c:cs)                    = isAlpha c && all isAlphaNum cs && not (isKeyword s)
  where isAlpha c                   = c `elem` ['a'..'z'] || c `elem` ['A'..'Z'] || c == '_'
        isAlphaNum c                = isAlpha c || c `elem` ['0'..'9']

isKeyword x                         = x `Data.Set.member` rws
  where rws                         = Data.Set.fromDistinctAscList [
                                        "False","None","NotImplemented","True","actor","and","as","assert",
                                        "async","await","break","class","continue","def","del","elif","else",
                                        "except","extends","finally","for","from","global","if","import","in",
                                        "is","lambda","nonlocal","not","or","pass","raise","return","sync",
                                        "try","var","while","with","yield"
                                      ]

istemp (Name _ str)                 = length (takeWhile (=='_') str) == 1

notemp                              = not . istemp

primitive t                         = t `elem` [TStr, TInt, TFloat, TBool, TNone]

pat2exp (PVar l n _)                = Var l n
pat2exp (PTuple l ps)               = Tuple l (map pat2elem ps)
pat2exp (PList l ps)                = List l (map pat2elem ps)
pat2exp (PIx l e ix)                = Ix l e ix
pat2exp (PDot l e n)                = Dot l e n
pat2exp (PParen l p)                = Paren l (pat2exp p)
pat2exp (PData l n ixs)             = foldl g (Var (loc n) n) ixs
  where g e ix                      = Ix (loc e `upto` loc ix) e [Index (loc ix) ix]

pat2elem (Elem p)                   = Elem (pat2exp p)
pat2elem (Star p)                   = Star (pat2exp p)

-- Printing -----------------

instance Pretty Module where
    pretty (Module qn imps stmts)   = prHead qn $+$ vpretty imps $+$ blank $+$ vpretty stmts

prHead qn                           = empty
--prHead qn                           = text "module" <+> pretty qn <> colon $+$ blank

instance Pretty Import where
    pretty (Import _ ms)            = text "import" <+> commaSep pretty ms
    pretty (FromImport _ n ns)      = text "from" <+> pretty n <+> text "import" <+> commaSep pretty ns
    pretty (FromImportAll _ n)      = text "from" <+> pretty n <+> text "import" <+> text "*"

prettySuite ss                      = nest 4 $ vcat $ map pretty ss

instance Pretty Stmt where
    pretty (Expr _ e)               = pretty e
    pretty (Assign _ ps e)          = hsep . punctuate (space <> equals) $ map pretty ps ++ [pretty e]
    pretty (AugAssign _ p o e)      = pretty p <+> pretty o <+> pretty e
    pretty (Assert _ es)            = text "assert" <+> commaList es
    pretty (Pass _)                 = text "pass"
    pretty (Delete _ t)             = text "del" <+> pretty t
    pretty (Return _ e)             = text "return" <+> pretty e
    pretty (Raise _ e)              = text "raise" <+> pretty e
    pretty (Break _)                = text "break"
    pretty (Continue _)             = text "continue"
    pretty (TypeSig _ vs t)         = commaList vs <+> text":" <+> pretty t
    pretty (If _ (b:bs) b2)         = prettyBranch "if" b $+$ vmap (prettyBranch "elif") bs $+$ prettyEnd "else" b2
    pretty (While _ e b b2)         = text "while" <+> pretty e <> colon $+$ prettySuite b $+$ prettyEnd "else" b2
    pretty (For _ p e b b2)         = text "for" <+> pretty p <+> text "in" <+> pretty e <> colon $+$ 
                                      prettySuite b $+$ prettyEnd "else" b2
    pretty (Try _ b hs b2 b3)       = text "try" <> colon $+$ prettySuite b $+$ vmap pretty hs $+$
                                      prettyEnd "else" b2 $+$ prettyEnd "finally" b3
    pretty (With _ items b)         = text "with" <+> commaSep pretty items <> colon $+$ prettySuite b
    pretty (Data _ (Just e) b)      = pretty e <> colon $+$ prettySuite b
    pretty (Data _ Nothing b)       = text "return" <> colon $+$ prettySuite b
    pretty (Extends _ n as)         = text "extends" <+> pretty n <> nonEmpty parens commaList as
    pretty (VarAssign _ ps e)       = text "var" <+> (hsep . punctuate (space <> equals) $ map pretty ps ++ [pretty e])
    pretty (Decl _ ds)              = vcat $ map pretty ds

instance Pretty Decl where
    pretty (Def _ n p a b md)       = pretty md <+> text "def" <+> pretty n <> parens (pretty p) <>
                                      nonEmpty (text "->" <>) pretty a <> colon $+$ prettySuite b
    pretty (Actor _ n p a b)        = text "actor" <+> pretty n <> parens (pretty p) <>
                                      nonEmpty (text "->" <>) pretty a <> colon $+$ prettySuite b
    pretty (Class _ n as b)         = text "class" <+> pretty n <> nonEmpty parens commaList as <> colon $+$ prettySuite b
    pretty (Decorator _ n as s)     = text "@" <> pretty n <> parens (commaList as) $+$ pretty s

prettyBranch kw (Branch e b)        = text kw <+> pretty e <> colon $+$ prettySuite b

prettyEnd kw []                     = empty
prettyEnd kw b                      = text kw <> colon $+$ prettySuite b

prettyArgs []                       = text ")"
prettyArgs [kwd]                    = prettyArg kwd <> text ")"
prettyArgs (kwd:kwds)               = prettyArg kwd <> comma $$ prettyArgs kwds
    
prettyArg (KwArg kw (Call _ e es))
  | length kwds == 0                = f <> parens (commaList es)
  | length pos == 0                 = f <> text "(" $$ nest 4 (prettyArgs kwds)
  | otherwise                       = f <> text "(" <> commaList pos <> comma $$ nest 4 (prettyArgs kwds)
  where (pos,kwds)                  = partition isPosArg es
        f                           = pretty kw <+> equals <+> pretty e
prettyArg arg                       = pretty arg

instance Pretty Expr where
    pretty (Var _ n)                = pretty n
    pretty (Int _ _ str)            = text str
    pretty (Float _ _ str)          = text str
    pretty (Imaginary _ _ str)      = text str
    pretty (Bool _ v)               = pretty v
    pretty (None _)                 = text "None"
    pretty (NotImplemented _)       = text "NotImplemented"
    pretty (Ellipsis _)             = text "..."
    pretty (Strings _ ss)           = hcat (map pretty ss)
    pretty (BStrings _ ss)          = hcat (map pretty ss)
    pretty (UStrings _ ss)          = hcat (map pretty ss)
    pretty (Call _ e es)
      | length kwds < 2             = pretty e <> parens (commaList es)
      | length pos == 0             = pretty e <> text "(" $$ nest 4 (prettyArgs kwds)
      | otherwise                   = pretty e <> text "(" <> commaList pos <> comma $$ nest 4 (prettyArgs kwds)
      where (pos,kwds)              = partition isPosArg es
    pretty (Await _ e)              = text "await" <+> pretty e
    pretty (Ix _ e ix)              = pretty e <> brackets (commaList ix)
    pretty (Cond _ e1 e e2)         = pretty e1 <+> text "if" <+> pretty e <+> text "else" <+> pretty e2
    pretty (BinOp _ e1 o e2)        = pretty e1 <+> pretty o <+> pretty e2
    pretty (CompOp _ e ops)         = pretty e <+> hsep (map pretty ops)
    pretty (UnOp _ o e)             = pretty o <> pretty e
    pretty (Dot _ e n)              = pretty e <> dot <> pretty n
    pretty (DotI _ e i)             = pretty e <> dot <> pretty i
    pretty (Lambda _ p e)           = text "lambda" <+> pretty p <> colon <+> pretty e
    pretty (Tuple _ es)             = prettyTuple es
    pretty (Yield _ e)              = text "yield" <+> pretty e
    pretty (YieldFrom _ e)          = text "yield" <+> text "from" <+> pretty e
    pretty (Generator _ e co)       = pretty e <+> pretty co
    pretty (List _ es)              = brackets (commaList es)
    pretty (ListComp _ e co)        = brackets (pretty e <+> pretty co)
    pretty (Dict _ es)              = braces (commaList es)
    pretty (DictComp _ e co)        = braces (pretty e <+> pretty co)
    pretty (Set _ [])               = text "set" <> parens empty
    pretty (Set _ es)               = braces (commaList es)
    pretty (SetComp _ e co)         = braces (pretty e <+> pretty co)
    pretty (Struct _ [])            = text "struct" <> parens empty
    pretty (Struct _ fs)            = parens (commaList fs)
    pretty (StructComp _ n e co)    = parens (pretty n <+> equals <+> pretty e <+> pretty co)
    pretty (Paren _ e)              = parens (pretty e)

instance Pretty OpArg where
    pretty (OpArg op e)             = pretty op <+> pretty e


prettyTuple []                      = text "()"
prettyTuple [e]                     = pretty e <> char ','
prettyTuple es                      = commaCat es

instance Pretty Name where
    pretty (Name _ str)
      | isIdent str                 = text str
      | otherwise                   = quotes (text str)
    pretty n                        = text (show n)

instance Pretty QName where
    pretty (QName n ns)             = dotCat pretty (n:ns)

instance Pretty ModRef where
    pretty (ModRef (i,n))           = hcat (replicate i dot) <> pretty n
    
instance Pretty a => Pretty (Op a) where
    pretty (Op _ a)                 = pretty a

instance Pretty Exception where
    pretty (Exception e1 e2)        = pretty e1 <+> nonEmpty (text "from" <+>) pretty e2

instance Pretty Handler where
    pretty (Handler ex b)           = pretty ex <> colon $+$ prettySuite b
    
instance Pretty Except where
    pretty (ExceptAll _)            = text "except"
    pretty (Except _ e)             = text "except" <+> pretty e
    pretty (ExceptAs _ e n)         = text "except" <+> pretty e <+> text "as" <+> pretty n

instance Pretty a => Pretty (Params a) where
    pretty p@Params {}              = hsep $ punctuate comma params
      where ps                      = map pretty (positional p)
            kw                      = map pretty (kwOnly p)
            psE0                    = nonEmpty (text "*" <>) pretty (posExt p)
            psE                     = if isEmpty psE0 && kw /= [] then text "*" else psE0
            kwE                     = nonEmpty (text "**" <>) pretty (kwExt p)
            params                  = filter (not . isEmpty) (ps ++ [psE] ++ kw ++ [kwE])

instance Pretty Param where
    pretty (Param n ann Nothing)    = pretty n <> prettyAnn ann
    pretty (Param n ann (Just e))   = pretty n <> prettyAnn ann <+> equals <+> pretty e

instance Pretty StarPar where
    pretty (StarPar _ n ann)        = pretty n <> prettyAnn ann
    pretty NoStar                   = empty

prettyAnn Nothing                   = empty
prettyAnn (Just a)                  = colon <+> pretty a

instance Pretty e => Pretty (Elem e) where
    pretty (Elem e)                 = pretty e
    pretty (Star e)                 = text "*" <> pretty e

instance Pretty Assoc where
    pretty (Assoc k v)              = pretty k <> colon <+> pretty v
    pretty (StarStarAssoc e)        = text "**" <> pretty e

instance Pretty Field where
    pretty (Field n e)              = pretty n <+> equals <+> pretty e
    pretty (StarStarField e)        = text "**" <> pretty e

instance Pretty WithItem where
    pretty (WithItem e p)           = pretty e <+> nonEmpty (text "as" <+>) pretty p

instance Pretty ModuleItem where
    pretty (ModuleItem n1 n2)       = pretty n1 <+> nonEmpty (text "as" <+>) pretty n2

instance Pretty ImportItem where
    pretty (ImportItem n1 n2)       = pretty n1 <+> nonEmpty (text "as" <+>) pretty n2

instance Pretty Arg where
    pretty (Arg e)                  = pretty e
    pretty (KwArg n e)              = pretty n <+> equals <+> pretty e
    pretty (StarArg e)              = text "*" <> pretty e
    pretty (StarStarArg e)          = text "**" <> pretty e

instance Pretty Index where
    pretty (Index _ e)              = pretty e
    pretty (Slice _ a b c)          = pretty a <> colon <> pretty b <> prettySlice c

prettySlice (Nothing)               = empty
prettySlice (Just (Nothing))        = colon
prettySlice (Just (Just b))         = colon <> pretty b

instance Pretty Comp where
    pretty (CompFor _ p e c)        = text "for" <+> pretty p <+> text "in" <+> pretty e <+> pretty c
    pretty (CompIf _ e c)           = text "if" <+> pretty e <+> pretty c
    pretty NoComp                   = empty


instance Pretty Annot where
    pretty (Annot e)                = pretty e
    pretty (Type _ t)               = pretty t

instance Pretty Pattern where
    pretty (PVar _ n a)             = pretty n <> prettyAnn a
    pretty (PTuple _ ps)            = prettyPEs ps
    pretty (PList _ ps)             = prettyPEs ps
    pretty (PIx _ e ix)             = pretty e <> brackets (commaList ix)
    pretty (PDot _ e n)             = pretty e <> dot <> pretty n
    pretty (PParen _ p)             = parens (pretty p)
    pretty (PData _ n ixs)          = pretty n <> hcat (map (brackets . pretty) ixs)

prettyPEs []                        = text "()"
prettyPEs [p]                       = pretty p <> char ','
prettyPEs ps                        = commaCat ps

instance Pretty Modif where
    pretty (Sync True)              = text "sync"
    pretty (Sync False)             = empty -- text "(sync)"
    pretty Async                    = text "async"
    pretty NoMod                    = empty

instance Pretty Type where
    pretty (TVar tv)                = pretty tv
    
    pretty (TFun RNil row t)        = parens (prettyRow row) <> text "->" <> pretty t
    pretty (TFun fx row t)          = prettyEffect fx <> parens (prettyRow row) <> text "->" <> pretty t
    pretty (TStruct row)            = parens (prettyRow row)
    pretty (TDict k t)              = text "dict" <> parens (pretty k <> comma <> pretty t)
    pretty (TTuple row)             = parens $ prettyTupleRow row
    pretty (TList t)                = brackets $ pretty t
    pretty (TSet t)                 = braces $ pretty t
    pretty (TMsg t)                 = text "msg" <> parens (pretty t)
    pretty TStr                     = text "str"
    pretty TInt                     = text "int"
    pretty TFloat                   = text "float"
    pretty TBool                    = text "bool"
    pretty TNone                    = text "None"

    pretty (TSchema tvs [] t)       = pretty t
    pretty (TSchema tvs cs t)       = pretty t <+> text "\\\\" <+> commaCat cs

    pretty r | isRow r              = text "<" <> prettyRow r <> text ">"

monotype (TSchema _ _ _)            = False
monotype _                          = True

polytype                            = not . monotype

isRow RPos{}                        = True
isRow RStar1{}                      = True
isRow RKwd{}                        = True
isRow RStar2{}                      = True
isRow RNil                          = True
isRow _                             = False

prettyEffect (RKwd n _ fx)          = text (nstr n) <+> prettyEffect fx
prettyEffect RNil                   = empty
prettyEffect t                      = pretty t

prettyTupleRow r@(RPos t RNil)      = prettyRow r <> comma
prettyTupleRow r                    = prettyRow r

prettyRow (RPos t r)                = pretty t <> prettyTail r
prettyRow (RStar1 t r)              = text "*" <> pretty t <> prettyTail r
prettyRow (RKwd n t r)              = pretty n <> colon <+> pretty t <> prettyTail r
prettyRow (RStar2 t r)              = text "**" <> pretty t <> prettyTail r
prettyRow RNil                      = empty
prettyRow (TVar tv)                 = pretty tv
prettyRow t                         = text "?" <> pretty t

prettyTail RNil                     = empty
prettyTail (TVar tv)                = comma <> pretty tv
prettyTail row                      = comma <> prettyRow row


instance Pretty [Constraint] where
    pretty cs                       = vcat (map pretty cs)

instance Pretty Constraint where
    pretty (CEqu _ _ t1 t2)         = pretty t1 <+> equals <+> pretty t2
    pretty (CIn _ _ t1 t2)          = pretty t1 <+> text "in" <+> pretty t2
    pretty (CDot _ _ t1 n t2)       = pretty t1 <> dot <> pretty n <+> equals <+> pretty t2
    pretty (CIx _ _ t1 t2 t3)       = pretty t1 <> brackets (pretty t2) <+> equals <+> pretty t3
    pretty (CMod _ _ t1 t2)         = text "Mod" <> parens (pretty t1 <> comma <> pretty t2)
    pretty (CPlus _ _ t1)           = text "Plus" <> parens (pretty t1)
    pretty (CNum _ _ t1)            = text "Num" <> parens (pretty t1)
    pretty (CBool _ _ t1)           = text "Bool" <> parens (pretty t1)


instance Pretty (Name, Type) where
    pretty (n, t)                   = pretty n <> colon <+> pretty t

instance Pretty (TVar, Type) where
    pretty (tv, t)                  = pretty tv <+> equals <+> pretty t

instance Pretty TVar where
    pretty []                       = text "_"
    pretty [i]                      = text (stringSupply !! i)
      where stringSupply            = [ c:tl | tl <- "" : map show [1..], c <- "ZABCDEFGHIJKLMNOPQRSTUWXY"  ]
    pretty (1:nums)                 = text "V" <> hcat (punctuate (text "_") (map pretty nums))


wildTVar                            = TVar []

schemaTVars                         = map schemaTVar [1..]

schemaTVar i                        = TVar [i]

intTVar i                           = TVar [1, i]
    
unTVar tvs                          = [ v | TVar v <- tvs ]

schemaVars                          = unTVar schemaTVars


instance Pretty Substitution where
    pretty s                        = vcat (map pr s)
      where pr (tv,t)               = pretty tv <+> equals <+> pretty t


instance Pretty Unary where
    pretty Not                      = text "not "
    pretty BNot                     = text "~"
    pretty UMinus                   = text "-"
    pretty UPlus                    = text "+"

instance Pretty Binary where
    pretty Or                       = text "or"
    pretty And                      = text "and"
    pretty BOr                      = text "|"
    pretty BXor                     = text "^"
    pretty BAnd                     = text "&"
    pretty ShiftL                   = text "<<"
    pretty ShiftR                   = text ">>"
    pretty Plus                     = text "+"
    pretty Minus                    = text "-"
    pretty Mult                     = text "*"
    pretty MMult                    = text "@"
    pretty Div                      = text "/"
    pretty Mod                      = text "%"
    pretty EuDiv                    = text "//"
    pretty Pow                      = text "**"

instance Pretty Comparison where
    pretty Lt                       = text "<"
    pretty Gt                       = text ">"
    pretty Eq                       = text "=="
    pretty GE                       = text ">="
    pretty LE                       = text "<="
    pretty NEq                      = text "!="
    pretty In                       = text "in"
    pretty NotIn                    = text "not in"
    pretty Is                       = text "is"
    pretty IsNot                    = text "is not"


instance Pretty Aug where
    pretty PlusA                    = text "+="
    pretty MinusA                   = text "-="
    pretty MultA                    = text "*="
    pretty MMultA                   = text "@="
    pretty DivA                     = text "/="
    pretty ModA                     = text "%="
    pretty PowA                     = text "**="
    pretty BAndA                    = text "&="
    pretty BOrA                     = text "|="
    pretty BXorA                    = text "^="
    pretty ShiftLA                  = text "<<="
    pretty ShiftRA                  = text ">>="
    pretty EuDivA                   = text "//="

instance Pretty CVar where
    pretty (CVar n)                 = pretty n
    
instance Pretty UType where
    pretty UInt                     = text "int"
    pretty UFloat                   = text "float"
    pretty UBool                    = text "bool"
    pretty UStr                     = text "str"
    pretty (UStrCon str)            = text ('\'' : str ++"'")

instance Pretty TPar where
    pretty (PosPar t)               = pretty t
    pretty (KwPar n t)              = pretty n <+> text ":" <+> pretty t

instance Pretty CType where
    pretty (CTVar _ v)              = pretty v
    pretty (CTFun _ es ps t)        = spaceSep pretty es <+> parens (pretty ps) <+> text "->" <+> pretty t
      where spaceSep f               = hsep . punctuate space . map f      
    pretty (CTTuple _ ps mbmbv)     = parens (commaList ps <+> maybe empty (\mbv -> text "*" <+> maybe empty pretty mbv) mbmbv)
    pretty (CTStruct _ ps mbmbv)    = parens (commaSep (\(x,t) -> pretty x <+> text ":" <+> pretty t) ps
                                         <+> maybe empty (\mbv -> text "**" <+> maybe empty pretty mbv) mbmbv)
    pretty (CTFList _ t)            = brackets (pretty t)
    pretty (CTFSet _ t)             = braces (pretty t)
    pretty (CTFDict _ kt vt)        = braces (commaList [kt,vt])
    pretty (CTOpt _ t)              = text "?" <> pretty t
    pretty (CTUnion _ as mbv)       = parens (vbarSep pretty as <+> maybe empty (\v -> text "|" <+> pretty v) mbv)
      where vbarSep f               = hsep . punctuate (space <> char '|') . map f
    pretty (CTClass  _ nm ts)       = pretty nm <> brackets (commaList ts)
    pretty (CTStr _)                = text "str"
    pretty (CTInt _)                = text "int"
    pretty (CTFloat _)              = text "float"
    pretty (CTBool _)               = text "bool"
    pretty (CTNone _)               = text "None"
    pretty (CTQual _ cs t)          = parens (commaList cs) <+> text "=>" <+> pretty t


    
