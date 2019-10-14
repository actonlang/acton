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
                | TypeSig       { sloc::SrcLoc, vars :: [Name], typ :: TSchema}
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
                | VarAssign     { sloc::SrcLoc, patterns::[Pattern], expr::Expr }
                | Decl          { sloc::SrcLoc, decls::[Decl] }
                deriving (Show)

data Decl       = Def           { dloc::SrcLoc, dname:: Name, qual::[TBind], params::Params, ann::(Maybe Type), dbody::Suite, modif::Modif }
                | Actor         { dloc::SrcLoc, dname:: Name, qual::[TBind], params::Params, ann::(Maybe Type), dbody::Suite }
                | Class         { dloc::SrcLoc, dname:: Name, qual::[TBind], bounds::[TCon], dbody::Suite }
                | Protocol      { dloc::SrcLoc, dname:: Name, qual::[TBind], bounds::[TCon], dbody::Suite }
                | Extension     { dloc::SrcLoc, dqname::QName, qual::[TBind], bounds::[TCon], dbody::Suite }
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
                | Lambda        { eloc::SrcLoc, pars::Params, exp1::Expr }
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
                | Record        { eloc::SrcLoc, fields::[Field] }
                | RecordComp    { eloc::SrcLoc, var::Name, exp1::Expr, comp::Comp }
                | Paren         { eloc::SrcLoc, exp1::Expr }
                deriving (Show)

data Pattern    = PVar          { ploc::SrcLoc, pn::Name, pann::Maybe Type }
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

data QName      = QName Name [Name] deriving (Show,Eq)
data ModuleItem = ModuleItem QName (Maybe Name) deriving (Show,Eq)
data ModRef     = ModRef (Int, Maybe QName) deriving (Show,Eq)
data ImportItem = ImportItem Name (Maybe Name) deriving (Show,Eq)
data Op a       = Op SrcLoc a deriving (Show)
data Exception  = Exception Expr (Maybe Expr) deriving (Show,Eq)
data Branch     = Branch Expr Suite deriving (Show,Eq)
data Handler    = Handler Except Suite deriving (Show,Eq)
data Except     = ExceptAll SrcLoc | Except SrcLoc Expr | ExceptAs SrcLoc Expr Name deriving (Show)
data Params     = Params [Param] StarPar [Param] StarPar deriving (Show,Eq)
data Param      = Param Name (Maybe TSchema) (Maybe Expr) deriving (Show,Eq)
data StarPar    = StarPar SrcLoc Name (Maybe Type) | NoStar deriving (Show)

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

data Modif      = Sync Bool | Async | NoMod deriving (Show,Eq)

data OType      = OVar      OVar
                -- Types
                | OFun      OEffect ORow OType
                | ORecord   ORow
                | OTuple    ORow
                | ODict     OType OType
                | OList     OType
                | OSet      OType
                | OMsg      OType
                | OStr
                | OInt
                | OFloat
                | OBool
                | ONone
                -- Rows
                | OPos      OType ORow
                | OStar1    OType ORow
                | OKwd      Name OType ORow
                | OStar2    OType ORow
                | ONil
                -- Polymorphism
                | OSchema   [OVar] [Qonstraint] OType
                deriving (Eq,Show,Read,Generic)

data Qonstraint = QEqu      { cloc::SrcLoc, vn::Int, t1::OType, t2::OType }
                | QIn       { cloc::SrcLoc, vn::Int, t1::OType, t2::OType }
                | QDot      { cloc::SrcLoc, vn::Int, t1::OType, cname::Name, t2::OType }
                | QIx       { cloc::SrcLoc, vn::Int, t1::OType, t2::OType, t3::OType }
                | QMod      { cloc::SrcLoc, vn::Int, t1::OType, t2::OType }
                | QPlus     { cloc::SrcLoc, vn::Int, t1::OType }
                | QNum      { cloc::SrcLoc, vn::Int, t1::OType }
                | QBool     { cloc::SrcLoc, vn::Int, t1::OType }
                deriving (Eq,Show,Read,Generic)

type OVar       = [Int]

type ORow       = OType

type OEffect    = ORow

type OSubst     = [(OVar,OType)]

----

data TSchema    = TSchema SrcLoc [TBind] Type deriving (Show)

data TVar       = TV Name deriving (Eq,Show) -- the Name is an uppercase letter, optionally followed by digits.

data TCon       = TC QName [Type] deriving (Eq,Show)

data UType      = UInt | UFloat | UBool | UStr | UStrCon String deriving (Eq,Show)

type EfxRow     = [Name] -- for now

data PosRow     = PosRow TSchema PosRow | PosVar (Maybe TVar) | PosNil deriving (Eq,Show)

data KwdRow     = KwdRow Name TSchema KwdRow | KwdVar (Maybe TVar) | KwdNil deriving (Show)

data TBind      = TBind TVar [TCon] deriving (Eq,Show)

data Type       = TSelf     { tloc :: SrcLoc }
                | TVar      { tloc :: SrcLoc, cvar :: TVar }
                | TCon      { tloc :: SrcLoc, ccon :: TCon }
                | TAt       { tloc :: SrcLoc, ccon :: TCon }
                | TFun      { tloc :: SrcLoc, ceffect :: EfxRow, posrow :: PosRow, kwdrow :: KwdRow, restype :: Type }
                | TTuple    { tloc :: SrcLoc, posrow :: PosRow }
                | TRecord   { tloc :: SrcLoc, kwdrow :: KwdRow }
                | PSeq      { tloc :: SrcLoc, elemtype :: Type }
                | PSet      { tloc :: SrcLoc, elemtype :: Type }
                | PMap      { tloc :: SrcLoc, keytype :: Type, valtype :: Type }
                | TOpt      { tloc :: SrcLoc, opttype :: Type }
                | TUnion    { tloc :: SrcLoc, alts :: [UType] }
                | TStr      { tloc :: SrcLoc }
                | TInt      { tloc :: SrcLoc }
                | TFloat    { tloc :: SrcLoc }
                | TBool     { tloc :: SrcLoc }
                | TNone     { tloc :: SrcLoc }
                deriving (Show,Generic)

instance Data.Binary.Binary OType
instance Data.Binary.Binary Name
instance Data.Binary.Binary Qonstraint
instance Data.Binary.Binary Binary

tvarSupply      = [ name (c:tl) | tl <- "" : map show [1..], c <- "ABCDEFGHIJKLMNOPQRSTUWXY"  ]


-- SrcInfo ------------------

type SrcInfo        = [InfoTag]

data InfoTag        = GEN   SrcLoc OType
                    | INS   SrcLoc OType
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

instance HasLoc TSchema where
    loc (TSchema l _ _) = l

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
    x@VarAssign{}       ==  y@VarAssign{}       = patterns x == patterns y && expr x == expr y
    x@Decl{}            ==  y@Decl{}            = decls x == decls y
    _                   ==  _                   = False

instance Eq Decl where
    Def _ n1 q1 p1 a1 b1 m1 ==  Def _ n2 q2 p2 a2 b2 m2 = n1 == n2 && q1 == q2 && p1 == p2 && a1 == a2 && b1 == b2 && m1 == m2
    Actor _ n1 q1 p1 a1 b1  ==  Actor _ n2 q2 p2 a2 b2  = n1 == n2 && q1 == q2 && p1 == p2 && a1 == a2 && b1 == b2
    Class _ n1 q1 a1 b1     ==  Class _ n2 q2 a2 b2     = n1 == n2 && q1 == q2 && a1 == a2 && b1 == b2
    Protocol _ n1 q1 a1 b1  ==  Protocol _ n2 q2 a2 b2  = n1 == n2 && q1 == q2 && a1 == a2 && b1 == b2
    Extension _ n1 q1 a1 b1 ==  Extension _ n2 q2 a2 b2 = n1 == n2 && q1 == q2 && a1 == a2 && b1 == b2
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
    x@Lambda{}          ==  y@Lambda{}          = pars x == pars y && exp1 x == exp1 y
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
    x@Record{}          ==  y@Record{}          = fields x == fields y
    x@RecordComp{}      ==  y@RecordComp{}      = var x == var y && exp1 x == exp1 y && comp x == comp y
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

instance Eq TSchema where
    TSchema _ q1 t1     == TSchema _ q2 t2      = q1 == q2 && t1 == t2

instance Eq KwdRow where
    r1                  == r2                   = walk r2 r1 && walk r1 r2
      where walk (KwdRow n t r) r0              = has n t r0 && walk r r0
            walk r r0                           = ends r r
            has n t (KwdRow n' t' r')           = n == n' && t == t' || has n t r'
            has n t _                           = False
            ends r (KwdRow _ _ r')              = ends r r'
            ends (KwdVar v) (KwdVar v')         = v == v'
            ends KwdNil KwdNil                  = True
            ends _ _                            = False

instance Eq Type where
    TSelf _             == TSelf _              = True
    TVar _ v1           == TVar _ v2            = v1 == v2
    TCon _ c1           == TCon _ c2            = c1 == c2
    TAt _ c1            == TAt _ c2             = c1 == c2
    TFun _ e1 p1 r1 t1  == TFun _ e2 p2 r2 t2   = e1 == e2 && p1 == p2 && r1 == r2 && t1 == t2
    TTuple _ p1         == TTuple _ p2          = p1 == p2
    TRecord _ r1        == TRecord _ r2         = r1 == r2
    PSeq _ t1           == PSeq _ t2            = t1 == t2
    PSet _ t1           == PSet _ t2            = t1 == t2
    PMap _ k1 t1        == PMap _ k2 t2         = k1 == k2 && t1 == t2
    TOpt _ t1           == TOpt _ t2            = t1 == t2
    TUnion _ u1         == TUnion _ u2          = all (`elem` u2) u1 && all (`elem` u1) u2
    TStr _              == TStr _               = True
    TInt _              == TInt _               = True
    TFloat _            == TFloat _             = True
    TBool _             == TBool _              = True
    TNone _             == TNone _              = True
    _                   == _                    = False


-- Show & Read ----------------

instance Show Name where
    show n              = show (nstr n)

instance Read Name where
    readsPrec p str     = [ (Name NoLoc s, str') | (s,str') <- readsPrec p str ]


-- Helpers ------------------

name                                = Name NoLoc

qName (str:strs)                    = QName (name str) (map name strs)

noQual n                            = QName n []

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

asyncFX                             = OKwd asyncKW ONone
syncFX                              = OKwd syncKW ONone
actFX                               = OKwd actKW ONone
mutFX                               = OKwd mutKW ONone

asyncKW                             = Name NoLoc "async"
syncKW                              = Name NoLoc "sync"
actKW                               = Name NoLoc "actor"
mutKW                               = Name NoLoc "mut"

elemcore (Elem e)                   = e
elemcore (Star e)                   = e

fieldcore (Field _ e)               = e
fieldcore (StarStarField e)         = e

paramcores (Params ps _ ks _)       = [ e | Param _ _ (Just e) <- ps ++ ks ]

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
                                        "except","finally","for","from","global","if","import","in",
                                        "is","lambda","nonlocal","not","or","pass","raise","return","sync",
                                        "try","var","while","with","yield","Self"
                                      ]

istemp (Name _ str)                 = length (takeWhile (=='_') str) == 1

notemp                              = not . istemp

primitive t                         = t `elem` [OStr, OInt, OFloat, OBool, ONone]

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
    pretty (VarAssign _ ps e)       = text "var" <+> (hsep . punctuate (space <> equals) $ map pretty ps ++ [pretty e])
    pretty (Decl _ ds)              = vcat $ map pretty ds

instance Pretty Decl where
    pretty (Def _ n q ps a b md)    = pretty md <+> text "def" <+> pretty n <+> nonEmpty brackets commaList q <+> parens (pretty ps) <>
                                      nonEmpty (text " -> " <>) pretty a <> colon $+$ prettySuite b
    pretty (Actor _ n q ps a b)     = text "actor" <+> pretty n <+> nonEmpty brackets commaList q <+> parens (pretty ps) <>
                                      nonEmpty (text " -> " <>) pretty a <> colon $+$ prettySuite b
    pretty (Class _ n q a b)        = text "class" <+> pretty n <+> nonEmpty brackets commaList q <+>
                                      nonEmpty parens commaList a <> colon $+$ prettySuite b
    pretty (Protocol _ n q a b)     = text "protocol" <+> pretty n <+> nonEmpty brackets commaList q <+>
                                      nonEmpty parens commaList a <> colon $+$ prettySuite b
    pretty (Extension _ n q a b)    = text "extension" <+> pretty n <+> nonEmpty brackets commaList q <+>
                                      nonEmpty parens commaList a <> colon $+$ prettySuite b
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
    pretty (Lambda _ ps e)          = text "lambda" <+> pretty ps <> colon <+> pretty e
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
    pretty (Record _ [])            = text "record" <> parens empty
    pretty (Record _ fs)            = parens (commaList fs)
    pretty (RecordComp _ n e co)    = parens (pretty n <+> equals <+> pretty e <+> pretty co)
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

instance Pretty Params where
    pretty (Params a b c d)         = hsep $ punctuate comma params
      where ps                      = map pretty a
            kw                      = map pretty c
            psE0                    = nonEmpty (text "*" <>) pretty b
            psE                     = if isEmpty psE0 && kw /= [] then text "*" else psE0
            kwE                     = nonEmpty (text "**" <>) pretty d
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

instance Pretty OType where
    pretty (OVar tv)                = pretty tv
    
    pretty (OFun ONil row t)        = parens (prettyRow row) <> text "->" <> pretty t
    pretty (OFun fx row t)          = prettyEffect fx <> parens (prettyRow row) <> text "->" <> pretty t
    pretty (ORecord row)            = parens (prettyRow row)
    pretty (ODict k t)              = text "dict" <> parens (pretty k <> comma <> pretty t)
    pretty (OTuple row)             = parens $ prettyTupleRow row
    pretty (OList t)                = brackets $ pretty t
    pretty (OSet t)                 = braces $ pretty t
    pretty (OMsg t)                 = text "msg" <> parens (pretty t)
    pretty OStr                     = text "str"
    pretty OInt                     = text "int"
    pretty OFloat                   = text "float"
    pretty OBool                    = text "bool"
    pretty ONone                    = text "None"

    pretty (OSchema tvs [] t)       = pretty t
    pretty (OSchema tvs cs t)       = pretty t <+> text "\\\\" <+> commaCat cs

    pretty r | isRow r              = text "<" <> prettyRow r <> text ">"

monotype (OSchema _ _ _)            = False
monotype _                          = True

polytype                            = not . monotype

isRow OPos{}                        = True
isRow OStar1{}                      = True
isRow OKwd{}                        = True
isRow OStar2{}                      = True
isRow ONil                          = True
isRow _                             = False

prettyEffect (OKwd n _ fx)          = text (nstr n) <+> prettyEffect fx
prettyEffect ONil                   = empty
prettyEffect t                      = pretty t

prettyTupleRow r@(OPos t ONil)      = prettyRow r <> comma
prettyTupleRow r                    = prettyRow r

prettyRow (OPos t r)                = pretty t <> prettyTail r
prettyRow (OStar1 t r)              = text "*" <> pretty t <> prettyTail r
prettyRow (OKwd n t r)              = pretty n <> colon <+> pretty t <> prettyTail r
prettyRow (OStar2 t r)              = text "**" <> pretty t <> prettyTail r
prettyRow ONil                      = empty
prettyRow (OVar tv)                 = pretty tv
prettyRow t                         = text "?" <> pretty t

prettyTail ONil                     = empty
prettyTail (OVar tv)                = comma <> pretty tv
prettyTail row                      = comma <> prettyRow row


instance Pretty [Qonstraint] where
    pretty cs                       = vcat (map pretty cs)

instance Pretty Qonstraint where
    pretty (QEqu _ _ t1 t2)         = pretty t1 <+> equals <+> pretty t2
    pretty (QIn _ _ t1 t2)          = pretty t1 <+> text "in" <+> pretty t2
    pretty (QDot _ _ t1 n t2)       = pretty t1 <> dot <> pretty n <+> equals <+> pretty t2
    pretty (QIx _ _ t1 t2 t3)       = pretty t1 <> brackets (pretty t2) <+> equals <+> pretty t3
    pretty (QMod _ _ t1 t2)         = text "Mod" <> parens (pretty t1 <> comma <> pretty t2)
    pretty (QPlus _ _ t1)           = text "Plus" <> parens (pretty t1)
    pretty (QNum _ _ t1)            = text "Num" <> parens (pretty t1)
    pretty (QBool _ _ t1)           = text "Bool" <> parens (pretty t1)


instance Pretty (Name, OType) where
    pretty (n, t)                   = pretty n <> colon <+> pretty t

instance Pretty (OVar, OType) where
    pretty (tv, t)                  = pretty tv <+> equals <+> pretty t

instance Pretty OVar where
    pretty []                       = text "_"
    pretty [i]                      = text (stringSupply !! i)
      where stringSupply            = [ c:tl | tl <- "" : map show [1..], c <- "ZABCDEFGHIJKLMNOPQRSTUWXY"  ]
    pretty (1:nums)                 = text "V" <> hcat (punctuate (text "_") (map pretty nums))


wildOVar                            = OVar []

schemaOVars                         = map schemaOVar [1..]

schemaOVar i                        = OVar [i]

intOVar i                           = OVar [1, i]
    
unOVar tvs                          = [ v | OVar v <- tvs ]

schemaVars                          = unOVar schemaOVars


instance Pretty OSubst where
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

instance Pretty TSchema where
    pretty (TSchema _ [] t)         = pretty t
    pretty (TSchema _ q t)          = brackets (commaList q) <+> text "=>" <+> pretty t

instance Pretty TVar where
    pretty (TV n)                   = pretty n

instance Pretty TCon where
    pretty (TC n [])                = pretty n
    pretty (TC n ts)                = pretty n <> brackets (commaList ts)
    
instance Pretty TBind where
    pretty (TBind v [])             = pretty v
    pretty (TBind v cs)             = pretty v <> parens (commaList cs)
    
instance Pretty UType where
    pretty UInt                     = text "int"
    pretty UFloat                   = text "float"
    pretty UBool                    = text "bool"
    pretty UStr                     = text "str"
    pretty (UStrCon str)            = text ('\'' : str ++"'")

instance Pretty PosRow where
    pretty (PosRow t PosNil)        = pretty t
    pretty (PosRow t p)             = pretty t <> comma <+> pretty p
    pretty (PosVar mbn)             = text "*" <> maybe empty pretty mbn
    pretty PosNil                   = empty
    
instance Pretty KwdRow where
    pretty (KwdRow n t KwdNil)      = pretty n <> colon <+> pretty t
    pretty (KwdRow n t k)           = pretty n <> colon <+> pretty t <> comma <+> pretty k
    pretty (KwdVar mbn)             = text "**" <> maybe empty pretty mbn
    pretty KwdNil                   = empty
    
instance Pretty (PosRow, KwdRow) where
    pretty (PosNil, k)              = pretty k
    pretty (p, KwdNil)              = pretty p
    pretty (p, k)                   = pretty p <> comma <+> pretty k

instance Pretty Type where
    pretty (TSelf _)                = text "Self"
    pretty (TVar _ v)               = pretty v
    pretty (TCon  _ c)              = pretty c
    pretty (TAt  _ c)               = text "@" <> pretty c
    pretty (TFun _ es p k t)        = spaceSep pretty es <+> parens (pretty (p,k)) <+> text "->" <+> pretty t
      where spaceSep f              = hsep . punctuate space . map f      
    pretty (TTuple _ pos)           = parens (pretty pos)
    pretty (TRecord _ kw)           = parens (pretty kw)
    pretty (PSeq _ t)               = brackets (pretty t)
    pretty (PSet _ t)               = braces (pretty t)
    pretty (PMap _ kt vt)           = braces (pretty kt <> colon <+> pretty vt)
    pretty (TOpt _ t)               = text "?" <> pretty t
    pretty (TUnion _ as)            = parens (vbarSep pretty as)
      where vbarSep f               = hsep . punctuate (space <> char '|') . map f
    pretty (TStr _)                 = text "str"
    pretty (TInt _)                 = text "int"
    pretty (TFloat _)               = text "float"
    pretty (TBool _)                = text "bool"
    pretty (TNone _)                = text "None"


    
