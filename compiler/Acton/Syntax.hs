{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, DeriveGeneric #-}
module Acton.Syntax where

import Utils
import qualified Data.Binary
import qualified Data.Set
import GHC.Generics (Generic)
import Prelude hiding((<>))


version :: [Int]
version = [0,1]

data Module     = Module        QName [Import] [Stmt] deriving (Eq,Show)

data Import     = Import        { iloc::SrcLoc, moduls::[ModuleItem] }
                | FromImport    { iloc::SrcLoc, modul::ModRef, items::[ImportItem] }
                | FromImportAll { iloc::SrcLoc, modul::ModRef }
                deriving (Show)

type Suite      = [Stmt]

data Stmt       = Expr          { sloc::SrcLoc, expr::Expr }
                | Assign        { sloc::SrcLoc, patterns::[Pattern], expr::Expr }
                | AugAssign     { sloc::SrcLoc, pattern::Pattern, aop::Op Aug, expr::Expr }
                | Assert        { sloc::SrcLoc, exprs::[Expr] }
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
                | Signature     { dloc::SrcLoc, dvars :: [Name], dtyp :: TSchema, dec::Decoration }
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
                
data Name       = Name SrcLoc String | Internal String Int deriving (Generic)

nloc (Name l _) = l
nloc Internal{} = NoLoc

nstr (Name _ s) = shift s
  where shift []            = []
        shift str
          | n == 0          = head str : shift (tail str)
          | n <= 2          = replicate n '_' ++ shift rest
          | otherwise       = replicate (n+1) '_' ++ shift rest
          where (xs,rest)   = span (=='_') str
                n           = length xs
nstr (Internal s i) = s ++ "___" ++ show i

data QName      = QName Name [Name] deriving (Show,Eq,Generic)
data ModuleItem = ModuleItem QName (Maybe Name) deriving (Show,Eq)
data ModRef     = ModRef (Int, Maybe QName) deriving (Show,Eq)
data ImportItem = ImportItem Name (Maybe Name) deriving (Show,Eq)
data Op a       = Op SrcLoc a deriving (Show)
data Exception  = Exception Expr (Maybe Expr) deriving (Show,Eq)
data Branch     = Branch Expr Suite deriving (Show,Eq)
data Handler    = Handler Except Suite deriving (Show,Eq)
data Except     = ExceptAll SrcLoc | Except SrcLoc Name | ExceptAs SrcLoc Name Name deriving (Show)
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

data Modif      = Sync Bool | Async | NoMod | StaticMeth | ClassMeth | InstMeth deriving (Show,Eq)
data Decoration = ClassAttr | InstAttr | StaticMethod | ClassMethod | InstMethod | NoDecoration deriving (Eq,Show,Generic)
    

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

type OSubstitution = [(OVar,OType)]

wildOVar        = OVar []

schemaOVars     = map schemaOVar [1..]

schemaOVar i    = OVar [i]

intOVar i       = OVar [1, i]
    
unOVar tvs      = [ v | OVar v <- tvs ]

schemaVars      = unOVar schemaOVars

monotype (OSchema _ _ _)    = False
monotype _      = True

polytype        = not . monotype

isRow OPos{}    = True
isRow OStar1{}  = True
isRow OKwd{}    = True
isRow OStar2{}  = True
isRow ONil      = True
isRow _         = False


---------------------------------------------------------------------------

data TSchema    = TSchema SrcLoc [TBind] Type deriving (Show,Generic)

data TVar       = TV Name deriving (Eq,Ord,Show,Generic) -- the Name is an uppercase letter, optionally followed by digits.

data TCon       = TC QName [Type] deriving (Eq,Show,Generic)

data UType      = UCon QName | ULit String deriving (Eq,Show,Generic)

type EfxRow     = [Name] -- for now

data PosRow     = PosRow TSchema PosRow | PosVar (Maybe TVar) | PosNil deriving (Eq,Show,Generic)

data KwdRow     = KwdRow Name TSchema KwdRow | KwdVar (Maybe TVar) | KwdNil deriving (Show,Generic)

data TBind      = TBind TVar [TCon] deriving (Eq,Show,Generic)

data Type       = TSelf     { tloc :: SrcLoc }
                | TVar      { tloc :: SrcLoc, cvar :: TVar }
                | TCon      { tloc :: SrcLoc, ccon :: TCon }
                | TAt       { tloc :: SrcLoc, ccon :: TCon }
                | TFun      { tloc :: SrcLoc, ceffect :: EfxRow, posrow :: PosRow, kwdrow :: KwdRow, restype :: Type }
                | TTuple    { tloc :: SrcLoc, posrow :: PosRow }
                | TRecord   { tloc :: SrcLoc, kwdrow :: KwdRow }
                | TOpt      { tloc :: SrcLoc, opttype :: Type }
                | TUnion    { tloc :: SrcLoc, alts :: [UType] }
                | TNone     { tloc :: SrcLoc }
                deriving (Show,Generic)

type Substitution = [(TVar,Type)]

instance Data.Binary.Binary OType
instance Data.Binary.Binary Qonstraint

instance Data.Binary.Binary Name
instance Data.Binary.Binary QName
instance Data.Binary.Binary Decoration
instance Data.Binary.Binary TSchema
instance Data.Binary.Binary TVar
instance Data.Binary.Binary TCon
instance Data.Binary.Binary UType
instance Data.Binary.Binary PosRow
instance Data.Binary.Binary KwdRow
instance Data.Binary.Binary TBind
instance Data.Binary.Binary Type

tvarSupply      = [ TV $ Name NoLoc (c:tl) | tl <- "" : map show [1..], c <- "ABCDEFGHIJKLMNOPQRSTUWXY"  ]


-- SrcInfo ------------------

type SrcInfo        = [InfoTag]

data InfoTag        = GEN   SrcLoc OType
                    | INS   SrcLoc OType
                    deriving (Eq,Show)

lookupGEN l info    = listToMaybe [ t | GEN l' t <- info, l' == l ]

lookupINS l info    = listToMaybe [ t | INS l' t <- info, l' == l ]


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
    x@Import{}          ==  y@Import{}          = moduls x == moduls y
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
    Signature _ ns1 t1 d1   ==  Signature _ ns2 t2 d2   = ns1 == ns2 && t1 == t2 && d1 == d2
    _                       == _                        = False

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
    Internal s1 i1      == Internal s2 i2       = s1 == s2 && i1 == i2
    _                   == _                    = False

instance Ord Name where
    Name _ s1           <= Name _ s2            = s1 <= s2
    Internal s1 i1      <= Internal s2 i2       = (s1,i1) <= (s2,i2)
    Name _ _            <= Internal _ _         = True
    _                   <= _                    = False

instance Eq a => Eq (Op a) where
    Op _ x              ==  Op _ y              = x == y

instance Eq Except where
    ExceptAll _         ==  ExceptAll _         = True
    Except _ x1         ==  Except _ x2         = x1 == x2
    ExceptAs _ x1 n1    ==  ExceptAs _ x2 n2    = x1 == x2 && n1 == n2
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
    TOpt _ t1           == TOpt _ t2            = t1 == t2
    TUnion _ u1         == TUnion _ u2          = all (`elem` u2) u1 && all (`elem` u1) u2
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
                                        "False","None","NotImplemented","Self","True","actor","and","as","assert",
                                        "async","await","break","class","continue","def","del","elif","else",
                                        "except","finally","for","from","global","if","import","in",
                                        "is","lambda","nonlocal","not","or","pass","raise","return","sync",
                                        "try","var","while","with","yield"
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

