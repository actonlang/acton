{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, DeriveGeneric #-}
module Acton.Syntax where

import Utils
import qualified Data.Binary
import qualified Data.Set
import GHC.Generics (Generic)
import Prelude hiding((<>))


version :: [Int]
version = [0,1]

data Module     = Module        ModName [Import] Suite deriving (Eq,Show)

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

data Decl       = Def           { dloc::SrcLoc, dname:: Name, qual::[TBind], pos::PosPar, kwd::KwdPar, ann::(Maybe Type), dbody::Suite, modif::Modif }
                | Actor         { dloc::SrcLoc, dname:: Name, qual::[TBind], pos::PosPar, kwd::KwdPar, ann::(Maybe Type), dbody::Suite }
                | Class         { dloc::SrcLoc, dname:: Name, qual::[TBind], bounds::[TCon], dbody::Suite }
                | Protocol      { dloc::SrcLoc, dname:: Name, qual::[TBind], bounds::[TCon], dbody::Suite }
                | Extension     { dloc::SrcLoc, dqname::QName, qual::[TBind], bounds::[TCon], dbody::Suite }
                | Signature     { dloc::SrcLoc, dvars :: [Name], dtyp :: TSchema }
                deriving (Show)

data Expr       = Var           { eloc::SrcLoc, var::QName }
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
                | Call          { eloc::SrcLoc, function::Expr, pargs::PosArg, kargs::KwdArg }
                | Await         { eloc::SrcLoc, exp1::Expr }
                | Index         { eloc::SrcLoc, exp1::Expr, index::[Expr] }
                | Slice         { eloc::SrcLoc, exp1::Expr, slice::[Slice] }
                | Cond          { eloc::SrcLoc, exp1::Expr, cond::Expr, exp2::Expr }
                | BinOp         { eloc::SrcLoc, exp1::Expr, bop::Op Binary, exp2::Expr }
                | CompOp        { eloc::SrcLoc, exp1::Expr, ops::[OpArg] }
                | UnOp          { eloc::SrcLoc, uop::Op Unary, exp1::Expr }
                | Dot           { eloc::SrcLoc, exp1::Expr, attr::Name }
                | DotI          { eloc::SrcLoc, exp1::Expr, ival::Integer }
                | Lambda        { eloc::SrcLoc, ppar::PosPar, kpar::KwdPar, exp1::Expr }
                | Yield         { eloc::SrcLoc, yexp1::Maybe Expr }
                | YieldFrom     { eloc::SrcLoc, yfrom::Expr }
                | Tuple         { eloc::SrcLoc, parg::PosArg }
                | TupleComp     { eloc::SrcLoc, exp1::Expr, comp::Comp }
                | Record        { eloc::SrcLoc, kargs::KwdArg }
                | RecordComp    { eloc::SrcLoc, cvar::Name, exp1::Expr, comp::Comp }
                | List          { eloc::SrcLoc, elems::[Elem] }
                | ListComp      { eloc::SrcLoc, elem1::Elem, comp::Comp }
                | Dict          { eloc::SrcLoc, assocs::[Assoc] }
                | DictComp      { eloc::SrcLoc, assoc1::Assoc, comp::Comp }
                | Set           { eloc::SrcLoc, elems::[Elem] }
                | SetComp       { eloc::SrcLoc, elem1::Elem, comp::Comp }
                | Paren         { eloc::SrcLoc, exp1::Expr }
                deriving (Show)

data Pattern    = PVar          { ploc::SrcLoc, pn::Name, pann::Maybe Type }
                | PIndex        { ploc::SrcLoc, pexp::Expr, pindex::[Expr] }
                | PSlice        { ploc::SrcLoc, pexp::Expr, pslice::[Slice] }
                | PDot          { ploc::SrcLoc, pexp::Expr, pn::Name }
                | PParen        { ploc::SrcLoc, pat::Pattern }
--                | PRecord       { ploc::SrcLoc, kpat::KwdPat }
                | PTuple        { ploc::SrcLoc, ppat::PosPat }
                | PList         { ploc::SrcLoc, pats::[Pattern], ptail::Maybe Pattern }
                | PData         { ploc::SrcLoc, pn::Name, pixs::[Expr] }
                deriving (Show)
                
data Pass       = ParsePass | TypesPass | NormPass | CPSPass | LLiftPass | CGenPass deriving (Eq,Ord,Show,Read,Generic)

data Name       = Name SrcLoc String | Internal String Int Pass deriving (Generic)

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
nstr (Internal s i p)       = s ++ "___" ++ show i ++ suffix p
  where suffix ParsePass    = "p"
        suffix TypesPass    = ""
        suffix NormPass     = "n"
        suffix CPSPass      = "c"
        suffix LLiftPass    = "l"
        suffix CGenPass     = "g"

name            = Name NoLoc

data ModName    = ModName [Name] deriving (Show,Read,Eq,Generic)

modName ss      = ModName (map name ss)

data QName      = QName { mname::ModName, item::Name } | NoQual { item::Name } deriving (Show,Read,Eq,Generic)

qName ss s      = QName (modName ss) (name s)

noQual s        = NoQual (name s)

data ModuleItem = ModuleItem ModName (Maybe Name) deriving (Show,Eq)
data ModRef     = ModRef (Int, Maybe ModName) deriving (Show,Eq)
data ImportItem = ImportItem Name (Maybe Name) deriving (Show,Eq)
data Op a       = Op SrcLoc a deriving (Show)
data Exception  = Exception Expr (Maybe Expr) deriving (Show,Eq)
data Branch     = Branch Expr Suite deriving (Show,Eq)
data Handler    = Handler Except Suite deriving (Show,Eq)
data Except     = ExceptAll SrcLoc | Except SrcLoc QName | ExceptAs SrcLoc QName Name deriving (Show)

data Elem       = Elem Expr | Star Expr deriving (Show,Eq)
data Assoc      = Assoc Expr Expr | StarStar Expr deriving (Show,Eq)
-- data Field      = Field Name Expr | StarStarField Expr deriving (Show,Eq)

data PosPar     = PosPar Name (Maybe TSchema) (Maybe Expr) PosPar | PosSTAR Name (Maybe Type) | PosNIL deriving (Show,Eq)
data KwdPar     = KwdPar Name (Maybe TSchema) (Maybe Expr) KwdPar | KwdSTAR Name (Maybe Type) | KwdNIL deriving (Show,Eq)

data PosArg     = PosArg Expr PosArg | PosStar Expr | PosNil deriving (Show,Eq)
data KwdArg     = KwdArg Name Expr KwdArg | KwdStar Expr | KwdNil deriving (Show,Eq)

data PosPat     = PosPat Pattern PosPat | PosPatStar Pattern | PosPatNil deriving (Show,Eq)
data KwdPat     = KwdPat Name Pattern KwdPat | KwdPatStar Pattern | KwdPatNil deriving (Show,Eq)

data OpArg      = OpArg (Op Comparison) Expr deriving (Eq,Show)
data Slice      = Sliz SrcLoc (Maybe Expr) (Maybe Expr) (Maybe (Maybe Expr)) deriving (Show)
data Comp       = CompFor SrcLoc Pattern Expr Comp | CompIf SrcLoc Expr Comp | NoComp deriving (Show)
data WithItem   = WithItem Expr (Maybe Pattern) deriving (Show,Eq)

data Unary      = Not|UPlus|UMinus|BNot deriving (Show,Eq)
data Binary     = Or|And|Plus|Minus|Mult|Pow|Div|Mod|EuDiv|BOr|BXor|BAnd|ShiftL|ShiftR|MMult deriving (Show,Read,Eq,Generic)
data Aug        = PlusA|MinusA|MultA|PowA|DivA|ModA|EuDivA|BOrA|BXorA|BAndA|ShiftLA|ShiftRA|MMultA deriving (Show,Eq)
data Comparison = Eq|NEq|LtGt|Lt|Gt|GE|LE|In|NotIn|Is|IsNot deriving (Show,Eq)

data Modif      = NoMod | Sync Bool | Async | StaticMeth | ClassMeth | InstMeth Bool deriving (Show,Eq)
data Decoration = NoDec | InstAttr Bool | ClassAttr | StaticMethod | ClassMethod | InstMethod Bool deriving (Eq,Show,Read,Generic)
    
data TSchema    = TSchema { scloc::SrcLoc, scbind::[TBind], sctype::Type, scdec::Decoration } deriving (Show,Read,Generic)

data TVar       = TV { tvname::Name } deriving (Eq,Ord,Show,Read,Generic) -- the Name is an uppercase letter, optionally followed by digits.

data TCon       = TC { tcname::QName, tcargs::[Type] } deriving (Eq,Show,Read,Generic)

data UType      = UCon QName | ULit String deriving (Eq,Show,Read,Generic)

data TBind      = TBind TVar [TCon] deriving (Eq,Show,Read,Generic)

data Type       = TVar      { tloc::SrcLoc, tvar::TVar }
                | TCon      { tloc::SrcLoc, tcon::TCon }
                | TAt       { tloc::SrcLoc, tcon::TCon }
                | TFun      { tloc::SrcLoc, fxrow::FXRow, posrow::PosRow, kwdrow::KwdRow, restype::Type }
                | TTuple    { tloc::SrcLoc, posrow::PosRow }
                | TRecord   { tloc::SrcLoc, kwdrow::KwdRow }
                | TUnion    { tloc::SrcLoc, alts::[UType] }
                | TOpt      { tloc::SrcLoc, opttype::Type }
                | TNone     { tloc::SrcLoc }
                | TWild     { tloc::SrcLoc }
                | TNil      { tloc::SrcLoc }
                | TRow      { tloc::SrcLoc, label::Name, rtype::TSchema, rtail::TRow }
                deriving (Show,Read,Generic)

type FXRow      = Type
type PosRow     = Type
type KwdRow     = Type
type TRow       = Type

skolem (TV n)   = case n of
                    Name{}     -> True
                    Internal{} -> False

monotype t      = TSchema NoLoc [] t NoDec
monotype' t d   = TSchema NoLoc [] t d
tSchema q t     = TSchema NoLoc q t NoDec
tSchema' q t d  = TSchema NoLoc q t d

tVar v          = TVar NoLoc v
tCon c          = TCon NoLoc c
tAt c           = TAt NoLoc c
tFun fx p r t   = TFun NoLoc fx p r t
tTuple p        = TTuple NoLoc p
tRecord r       = TRecord NoLoc r
tUnion ts       = TUnion NoLoc ts
tOpt t          = TOpt NoLoc t
tNone           = TNone NoLoc
tWild           = TWild NoLoc
tNil            = TNil NoLoc

tSelf           = TVar NoLoc tvSelf
tvSelf          = TV nSelf
nSelf           = Name NoLoc "Self"

rPos n          = Name NoLoc (show n)
rSync           = Name NoLoc "sync"
rAsync          = Name NoLoc "async"
rAct            = Name NoLoc "actor"
rMut            = Name NoLoc "mut"
rRet            = Name NoLoc "ret"

fxSync          = TRow NoLoc rSync (monotype tNone)
fxAsync         = TRow NoLoc rAsync (monotype tNone)
fxAct           = TRow NoLoc rAct (monotype tNone)
fxMut           = TRow NoLoc rMut (monotype tNone)
fxRet t         = TRow NoLoc rRet (monotype t)
fxVar v         = TVar NoLoc v
fxNil           = TNil NoLoc

posRow sc r     = TRow NoLoc (rPos n) sc r
  where n       = rowDepth r + 1
posVar mbv      = maybe tWild tVar mbv
posNil          = tNil

kwdRow n sc     = TRow NoLoc n sc
kwdVar mbv      = maybe tWild tVar mbv
kwdNil          = tNil

rowDepth (TRow _ _ _ r) = rowDepth r + 1
rowDepth _              = 0

rowTail (TRow _ _ _ r)  = rowTail r
rowTail r               = r


tvarSupply      = [ TV $ Name NoLoc (c:tl) | tl <- "" : map show [1..], c <- "ABCDEFGHIJKLMNOPQRSTUWXY"  ]


type Substitution = [(TVar,Type)]


instance Data.Binary.Binary Pass
instance Data.Binary.Binary Name
instance Data.Binary.Binary ModName
instance Data.Binary.Binary QName
instance Data.Binary.Binary Decoration
instance Data.Binary.Binary TSchema
instance Data.Binary.Binary TVar
instance Data.Binary.Binary TCon
instance Data.Binary.Binary UType
instance Data.Binary.Binary TBind
instance Data.Binary.Binary Type


-- SrcInfo ------------------

type SrcInfo            = [SrcInfoTag]

data SrcInfoTag         = GEN   SrcLoc TSchema
                        | INS   SrcLoc Type
                        deriving (Eq,Show)

lookupGEN l info        = listToMaybe [ t | GEN l' t <- info, l' == l ]

lookupINS l info        = listToMaybe [ t | INS l' t <- info, l' == l ]


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

instance HasLoc ModName where
    loc (ModName ns)    = loc ns
    
instance HasLoc QName where
    loc (QName m n)     = loc m `upto` loc n
    loc (NoQual n)      = loc n
    
instance HasLoc Elem where
    loc (Elem e)        = loc e
    loc (Star e)        = loc e

instance HasLoc Assoc where
    loc (Assoc k v)     = loc k `upto` loc v
    loc (StarStar e)    = loc e

instance HasLoc Pattern where
    loc                 = ploc

instance HasLoc TSchema where
    loc (TSchema l _ _ _) = l

instance HasLoc TVar where
    loc (TV v)          = loc v

instance HasLoc TCon where
    loc (TC c ts)       = loc c `upto` loc ts

instance HasLoc Type where
    loc                 = tloc

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
    Def _ n1 q1 p1 k1 a1 b1 m1  ==  Def _ n2 q2 p2 k2 a2 b2 m2  = n1 == n2 && q1 == q2 && p1 == p2 && k1 == k2 && a1 == a2 && b1 == b2 && m1 == m2
    Actor _ n1 q1 p1 k1 a1 b1   ==  Actor _ n2 q2 p2 k2 a2 b2   = n1 == n2 && q1 == q2 && p1 == p2 && k1 == k2 && a1 == a2 && b1 == b2
    Class _ n1 q1 a1 b1         ==  Class _ n2 q2 a2 b2         = n1 == n2 && q1 == q2 && a1 == a2 && b1 == b2
    Protocol _ n1 q1 a1 b1      ==  Protocol _ n2 q2 a2 b2      = n1 == n2 && q1 == q2 && a1 == a2 && b1 == b2
    Extension _ n1 q1 a1 b1     ==  Extension _ n2 q2 a2 b2     = n1 == n2 && q1 == q2 && a1 == a2 && b1 == b2
    Signature _ ns1 t1          ==  Signature _ ns2 t2          = ns1 == ns2 && t1 == t2
    _                           == _                            = False

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
    x@Call{}            ==  y@Call{}            = function x == function y && pargs x == pargs y && kargs x == kargs y
    x@Await{}           ==  y@Await{}           = exp1 x == exp1 y
    x@Index{}           ==  y@Index{}           = exp1 x == exp1 y && index x == index y
    x@Slice{}           ==  y@Slice{}           = exp1 x == exp1 y && slice x == slice y
    x@Cond{}            ==  y@Cond{}            = exp1 x == exp1 y && cond x == cond y && exp2 x == exp2 y
    x@BinOp{}           ==  y@BinOp{}           = exp1 x == exp1 y && bop x == bop y && exp2 x == exp2 y
    x@CompOp{}          ==  y@CompOp{}          = exp1 x == exp1 y && ops x == ops y
    x@UnOp{}            ==  y@UnOp{}            = uop x == uop y && exp1 x == exp1 y
    x@Dot{}             ==  y@Dot{}             = exp1 x == exp1 y && attr x == attr y
    x@DotI{}            ==  y@DotI{}            = exp1 x == exp1 y && ival x == ival y
    x@Lambda{}          ==  y@Lambda{}          = ppar x == ppar y && kpar x == kpar y && exp1 x == exp1 y
    x@Yield{}           ==  y@Yield{}           = yexp1 x == yexp1 y
    x@YieldFrom{}       ==  y@YieldFrom{}       = yfrom x == yfrom y
    x@Tuple{}           ==  y@Tuple{}           = pargs x == pargs y
    x@TupleComp{}       ==  y@TupleComp{}       = exp1 x == exp1 y && comp x == comp y
    x@Record{}          ==  y@Record{}          = kargs x == kargs y
    x@RecordComp{}      ==  y@RecordComp{}      = var x == var y && exp1 x == exp1 y && comp x == comp y
    x@List{}            ==  y@List{}            = elems x == elems y
    x@ListComp{}        ==  y@ListComp{}        = elem1 x == elem1 y && comp x == comp y
    x@Dict{}            ==  y@Dict{}            = assocs x == assocs y
    x@DictComp{}        ==  y@DictComp{}        = assoc1 x == assoc1 y && comp x == comp y
    x@Set{}             ==  y@Set{}             = elems x == elems y
    x@SetComp{}         ==  y@SetComp{}         = elem1 x == elem1 y && comp x == comp y
    x@Paren{}           ==  y                   = exp1 x == y
    x                   ==  y@Paren{}           = x == exp1 y
    _                   ==  _                   = False

instance Eq Name where
    Name _ s1           == Name _ s2            = s1 == s2
    Internal s1 i1 p1   == Internal s2 i2 p2    = s1 == s2 && i1 == i2 && p1 == p2
    _                   == _                    = False

instance Ord Name where
    Name _ s1           <= Name _ s2            = s1 <= s2
    Internal s1 i1 p1   <= Internal s2 i2 p2    = (s1,i1,p1) <= (s2,i2,p2)
    Name{}              <= Internal{}           = True
    _                   <= _                    = False

instance Eq a => Eq (Op a) where
    Op _ x              ==  Op _ y              = x == y

instance Eq Except where
    ExceptAll _         ==  ExceptAll _         = True
    Except _ x1         ==  Except _ x2         = x1 == x2
    ExceptAs _ x1 n1    ==  ExceptAs _ x2 n2    = x1 == x2 && n1 == n2
    _                   ==  _                   = False

instance Eq Slice where
    Sliz _ a1 b1 c1     ==  Sliz _ a2 b2 c2     = a1 == a2 && b1 == b2 && c1 == c2
    
instance Eq Comp where
    CompFor _ p1 e1 c1  ==  CompFor _ p2 e2 c2  = p1 == p2 && e1 == e2 && c1 == c2
    CompIf _ e1 c1      ==  CompIf _ e2 c2      = e1 == e2 && c1 == c2
    NoComp              ==  NoComp              = True
    _                   ==  _                   = False

instance Eq Pattern where
    PVar _ n1 a1        == PVar _ n2 a2         = n1 == n2 && a1 == a2
--    PRecord _ ps1       == PRecord _ ps2        = ps1 == ps2
    PTuple _ ps1        == PTuple _ ps2         = ps1 == ps2
    PList _ ps1 p1      == PList _ ps2 p2       = ps1 == ps2 && p1 == p2
    PIndex _ e1 ix1     == PIndex _ e2 ix2      = e1 == e2 && ix1 == ix2
    PSlice _ e1 sl1     == PSlice _ e2 sl2      = e1 == e2 && sl1 == sl2
    PDot _ e1 n1        == PDot _ e2 n2         = e1 == e2 && n1 == n2
    PData _ n1 ix1      == PData _ n2 ix2       = n1 == n2 && ix1 == ix2
    PParen _ p1         == p2                   = p1 == p2
    p1                  == PParen _ p2          = p1 == p2
    _                   == _                    = False

instance Eq TSchema where
    TSchema _ q1 t1 d1  == TSchema _ q2 t2 d2   = q1 == q2 && t1 == t2 && d1 == d2

instance Eq Type where
    TVar _ v1           == TVar _ v2            = v1 == v2
    TCon _ c1           == TCon _ c2            = c1 == c2
    TAt _ c1            == TAt _ c2             = c1 == c2
    TFun _ e1 p1 r1 t1  == TFun _ e2 p2 r2 t2   = e1 == e2 && p1 == p2 && r1 == r2 && t1 == t2
    TTuple _ p1         == TTuple _ p2          = p1 == p2
    TRecord _ r1        == TRecord _ r2         = r1 == r2
    TUnion _ u1         == TUnion _ u2          = all (`elem` u2) u1 && all (`elem` u1) u2
    TOpt _ t1           == TOpt _ t2            = t1 == t2
    TNone _             == TNone _              = True
    TWild _             == TWild _              = True
    TNil _              == TNil _               = True
    TRow _ n1 t1 r1     == TRow _ n2 t2 r2      = n1 == n2 && t1 == t2 && r1 == r2
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
-- tuple es                            = Tuple l0 (map Elem es)

mkStringLit s                       = Strings l0 ['\'' : s ++ "\'"]

asyncKW                             = Name NoLoc "async"
syncKW                              = Name NoLoc "sync"
actKW                               = Name NoLoc "actor"
mutKW                               = Name NoLoc "mut"
retKW                               = Name NoLoc "ret"

isInstAttr (InstAttr _)             = True
isInstAttr _                        = False

isInstMeth (InstMeth _)             = True
isInstMeth _                        = False

isSync (Sync _)                     = True
isSync _                            = False

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

posParLen PosNIL                    = 0
posParLen (PosSTAR _ _)             = 0
posParLen (PosPar _ _ _ r)          = 1 + posParLen r

posArgLen PosNil                    = 0
posArgLen (PosStar _)               = 0
posArgLen (PosArg _ r)              = 1 + posArgLen r

posPatLen PosPatNil                 = 0
posPatLen (PosPatStar _)            = 0
posPatLen (PosPat _ r)              = 1 + posPatLen r

posRowLen                           = rowDepth

posParHead (PosPar a b c _)         = (a,b,c)
posArgHead (PosArg a _)             = a
posPatHead (PosPat a _)             = a
posRowHead (TRow _ _ a _)           = a
 