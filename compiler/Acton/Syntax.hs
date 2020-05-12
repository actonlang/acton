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
                | Update        { sloc::SrcLoc, targets::[Target], expr::Expr }
                | IUpdate       { sloc::SrcLoc, target::Target, aop::Op Aug, expr::Expr }
                | Assert        { sloc::SrcLoc, expr::Expr, optExpr::Maybe Expr }
                | Pass          { sloc::SrcLoc }
                | Delete        { sloc::SrcLoc, target::Target}
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
                | After         { sloc::SrcLoc, expr::Expr, expr2::Expr }
                | Signature     { sloc::SrcLoc, vars::[Name], typ::TSchema, dec::Decoration }
                | Decl          { sloc::SrcLoc, decls::[Decl] }
                deriving (Show)

data Decl       = Def           { dloc::SrcLoc, dname:: Name, qual::Qual, pos::PosPar, kwd::KwdPar, ann::(Maybe Type), dbody::Suite, deco::Decoration }
                | Actor         { dloc::SrcLoc, dname:: Name, qual::Qual, pos::PosPar, kwd::KwdPar, ann::(Maybe Type), dbody::Suite }
                | Class         { dloc::SrcLoc, dname:: Name, qual::Qual, bounds::[TCon], dbody::Suite }
                | Protocol      { dloc::SrcLoc, dname:: Name, qual::Qual, bounds::[TCon], dbody::Suite }
                | Extension     { dloc::SrcLoc, dqname::QName, qual::Qual, bounds::[TCon], dbody::Suite }
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
                | Call          { eloc::SrcLoc, function::Expr, pargs::PosArg, kargs::KwdArg }
                | Await         { eloc::SrcLoc, exp1::Expr }
                | Index         { eloc::SrcLoc, exp1::Expr, index::[Expr] }
                | Slice         { eloc::SrcLoc, exp1::Expr, slice::[Sliz] }
                | Cond          { eloc::SrcLoc, exp1::Expr, cond::Expr, exp2::Expr }
                | BinOp         { eloc::SrcLoc, exp1::Expr, bop::Op Binary, exp2::Expr }
                | CompOp        { eloc::SrcLoc, exp1::Expr, ops::[OpArg] }
                | UnOp          { eloc::SrcLoc, uop::Op Unary, exp1::Expr }
                | Dot           { eloc::SrcLoc, exp1::Expr, attr::Name }
                | DotI          { eloc::SrcLoc, exp1::Expr, ival::Integer, tl :: Bool }
                | Lambda        { eloc::SrcLoc, ppar::PosPar, kpar::KwdPar, exp1::Expr }
                | Yield         { eloc::SrcLoc, yexp1::Maybe Expr }
                | YieldFrom     { eloc::SrcLoc, yfrom::Expr }
                | Tuple         { eloc::SrcLoc, pargs::PosArg, kargs::KwdArg }
                | List          { eloc::SrcLoc, elems::[Elem] }
                | ListComp      { eloc::SrcLoc, elem1::Elem, comp::Comp }
                | Dict          { eloc::SrcLoc, assocs::[Assoc] }
                | DictComp      { eloc::SrcLoc, assoc1::Assoc, comp::Comp }
                | Set           { eloc::SrcLoc, elems::[Elem] }
                | SetComp       { eloc::SrcLoc, elem1::Elem, comp::Comp }
                | Paren         { eloc::SrcLoc, exp1::Expr }
                deriving (Show)

data Pattern    = PVar          { ploc::SrcLoc, pn::Name, pann::Maybe Type }
                | PParen        { ploc::SrcLoc, pat::Pattern }
                | PTuple        { ploc::SrcLoc, ppat::PosPat, kpat::KwdPat}
                | PList         { ploc::SrcLoc, pats::[Pattern], ptail::Maybe Pattern }
                | PData         { ploc::SrcLoc, pn::Name, pixs::[Expr] }
                deriving (Show)

data Target     = TaVar         { taloc::SrcLoc, tn::Name}
                | TaIndex       { taloc::SrcLoc, texp::Expr, tindex::[Expr] }
                | TaSlice       { taloc::SrcLoc, texp::Expr, tslice::[Sliz] }
                | TaDot         { taloc::SrcLoc, texp::Expr, tn::Name }
                | TaParen       { taloc::SrcLoc, targ::Target }
                | TaTuple       { taloc::SrcLoc, targs::[Target]}

                deriving (Show)

data Pass       = ParsePass | KindPass | TypesPass | NormPass | CPSPass | DeactPass | LLiftPass | CPass | NoPass
                deriving (Eq,Ord,Show,Read,Generic)

data Name       = Name SrcLoc String | Derived Name String | Internal String Int Pass deriving (Generic)

nloc (Name l _) = l
nloc _          = NoLoc

nstr (Name _ s)             = s
nstr (Derived n s)          = nstr n ++ "$" ++ s
nstr (Internal s i p)       = s ++ "$" ++ show i ++ suffix p
  where suffix ParsePass    = "p"
        suffix KindPass     = "k"
        suffix TypesPass    = "t"
        suffix NormPass     = "n"
        suffix CPSPass      = "c"
        suffix DeactPass    = "d"
        suffix LLiftPass    = "l"
        suffix CPass        = "C"
        suffix NoPass       = ""

name            = Name NoLoc

data ModName    = ModName [Name] deriving (Show,Read,Eq,Generic)

modName ss      = ModName (map name ss)

data QName      = QName { mname::ModName, noq::Name } | NoQ { noq::Name } deriving (Show,Read,Eq,Generic)

qName ss s      = QName (modName ss) (name s)

noQ s           = NoQ (name s)

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

data PosPar     = PosPar Name (Maybe Type) (Maybe Expr) PosPar | PosSTAR Name (Maybe Type) | PosNIL deriving (Show,Eq)
data KwdPar     = KwdPar Name (Maybe Type) (Maybe Expr) KwdPar | KwdSTAR Name (Maybe Type) | KwdNIL deriving (Show,Eq)

data PosArg     = PosArg Expr PosArg | PosStar Expr | PosNil deriving (Show,Eq)
data KwdArg     = KwdArg Name Expr KwdArg | KwdStar Expr | KwdNil deriving (Show,Eq)

data PosPat     = PosPat Pattern PosPat | PosPatStar Pattern | PosPatNil deriving (Show,Eq)
data KwdPat     = KwdPat Name Pattern KwdPat | KwdPatStar Pattern | KwdPatNil deriving (Show,Eq)

data OpArg      = OpArg (Op Comparison) Expr deriving (Eq,Show)
data Sliz       = Sliz SrcLoc (Maybe Expr) (Maybe Expr) (Maybe Expr) deriving (Show)
data Comp       = CompFor SrcLoc Pattern Expr Comp | CompIf SrcLoc Expr Comp | NoComp deriving (Show)
data WithItem   = WithItem Expr (Maybe Pattern) deriving (Show,Eq)

data Unary      = Not|UPlus|UMinus|BNot deriving (Show,Eq)
data Binary     = Or|And|Plus|Minus|Mult|Pow|Div|Mod|EuDiv|BOr|BXor|BAnd|ShiftL|ShiftR|MMult deriving (Show,Read,Eq,Generic)
data Aug        = PlusA|MinusA|MultA|PowA|DivA|ModA|EuDivA|BOrA|BXorA|BAndA|ShiftLA|ShiftRA|MMultA deriving (Show,Eq)
data Comparison = Eq|NEq|LtGt|Lt|Gt|GE|LE|In|NotIn|Is|IsNot deriving (Show,Eq)

data Decoration = NoDec | Property | Static deriving (Eq,Show,Read,Generic)
    
data Kind       = KType | KProto | KFX | PRow | KRow | KFun [Kind] Kind | KVar Name | KWild deriving (Eq,Ord,Show,Read,Generic)

data TSchema    = TSchema { scloc::SrcLoc, scbind::Qual, sctype::Type } deriving (Show,Read,Generic)

data TVar       = TV { tvkind::Kind, tvname::Name } deriving (Ord,Show,Read,Generic) -- the Name is an uppercase letter, optionally followed by digits.

data TCon       = TC { tcname::QName, tcargs::[Type] } deriving (Eq,Show,Read,Generic)

data UType      = UCon QName | ULit String deriving (Eq,Show,Read,Generic)

data TBind      = TBind TVar [TCon] deriving (Eq,Show,Read,Generic)

data FX         = FXPure | FXMut Type | FXAct Type | FXAsync | FXActor deriving (Eq,Show,Read,Generic)

type Qual       = [TBind]

data Type       = TVar      { tloc::SrcLoc, tvar::TVar }
                | TCon      { tloc::SrcLoc, tcon::TCon }
                | TExist    { tloc::SrcLoc, tcon::TCon }
                | TFun      { tloc::SrcLoc, fx::TFX, posrow::PosRow, kwdrow::KwdRow, restype::Type }
                | TTuple    { tloc::SrcLoc, posrow::PosRow, kwdrow::KwdRow }
                | TUnion    { tloc::SrcLoc, alts::[UType] }
                | TOpt      { tloc::SrcLoc, opttype::Type }
                | TNone     { tloc::SrcLoc }
                | TWild     { tloc::SrcLoc }
                | TNil      { tloc::SrcLoc, rkind::Kind }
                | TRow      { tloc::SrcLoc, rkind::Kind, label::Name, rtype::Type, rtail::TRow }
                | TFX       { tloc::SrcLoc, tfx::FX }
                deriving (Show,Read,Generic)

type TFX        = Type
type PosRow     = Type
type KwdRow     = Type
type TRow       = Type

data Constraint = Cast  Type Type
                | Sub   Name Type Type
                | Impl  Name Type TCon
                | Sel   Name Type Name Type
                | Mut   Type Name Type
                deriving (Show,Read,Generic)

type Constraints = [Constraint]

skolem (TV k n) = case n of
                    Name{} -> True
                    _      -> False

dDef n p b      = Def NoLoc n [] p KwdNIL Nothing b NoDec

sDef n p b      = sDecl [dDef n p b]
sReturn e       = Return NoLoc (Just e)
sAssign ps e    = Assign NoLoc ps e
sUpdate ts e    = Update NoLoc ts e
sIUpdate t o e  = IUpdate NoLoc t o e
sRaise e        = Raise NoLoc (Just (Exception e Nothing))
sExpr e         = Expr NoLoc e
sDecl ds        = Decl NoLoc ds
sTry b hs els f = Try NoLoc b hs els f
sIf bs els      = If NoLoc bs els
sIf1 e b els    = sIf [Branch e b] els

handler qn b    = Handler (Except NoLoc qn) b

eCall e es      = Call NoLoc e (foldr PosArg PosNil es) KwdNil
eCallVar c es   = eCall (eVar c) es
eCallV c es     = eCall (Var NoLoc c) es
eQVar n         = Var NoLoc n
eVar n          = Var NoLoc (NoQ n)
eDot e n        = Dot NoLoc e n
eNone           = None NoLoc
eInt n          = Int NoLoc n (show n)
eBool b         = Bool NoLoc b
eBinOp e o e'   = BinOp NoLoc e (Op NoLoc o) e'
eLambda ns e    = Lambda NoLoc (pospar ns) KwdNIL e

pospar xs       = foldr (\n p -> PosPar n Nothing Nothing p) PosNIL xs

pVar n t        = PVar NoLoc n t

taVar n         = TaVar NoLoc n
taIndex e ix    = TaIndex NoLoc e [ix]

monotype t      = TSchema NoLoc [] t
tSchema q t     = TSchema NoLoc q t

tBind v         = TBind v []

tVar v          = TVar NoLoc v
tCon c          = TCon NoLoc c
tExist p        = TExist NoLoc p
tFun fx p k t   = TFun NoLoc fx p k t
tTuple p        = TTuple NoLoc p kwdNil
tRecord k       = TTuple NoLoc posNil k
tUnion ts       = TUnion NoLoc ts
tOpt t          = TOpt NoLoc t
tNone           = TNone NoLoc
tWild           = TWild NoLoc
tNil k          = TNil NoLoc k
tRow k          = TRow NoLoc k

tFun0 ps t      = tFun fxPure (foldr posRow posNil ps) kwdNil t

tSelf           = TVar NoLoc tvSelf
tvSelf          = TV KType nSelf
nSelf           = Name NoLoc "Self"

fxActor         = TFX NoLoc FXActor
fxAsync         = TFX NoLoc FXAsync
fxAct t         = TFX NoLoc (FXAct t)
fxMut t         = TFX NoLoc (FXMut t)
fxPure          = TFX NoLoc FXPure

posRow t r      = TRow NoLoc PRow (name "_") t r
posVar mbv      = maybe tWild tVar mbv
posNil          = tNil PRow

kwdRow n t r    = TRow NoLoc KRow n t r
kwdVar mbv      = maybe tWild tVar mbv
kwdNil          = tNil KRow

rowTail (TRow _ _ _ _ r)
                = rowTail r
rowTail r       = r


tvarSupply      = [ TV KType $ name (c:tl) | tl <- "" : map show [1..], c <- "ABCDEFGHIJKLMNOTUVW" ]

fxSupply        = [ TV KFX $ name (c:tl) | tl <- "" : map show [1..], c <- "XY" ]

prowSupply      = [ TV PRow $ name (c:tl) | tl <- "" : map show [1..], c <- "PQ" ]
krowSupply      = [ TV KRow $ name (c:tl) | tl <- "" : map show [1..], c <- "RS" ]


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
instance Data.Binary.Binary Kind
instance Data.Binary.Binary FX
instance Data.Binary.Binary Constraint


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
    loc (NoQ n)         = loc n
    
instance HasLoc Elem where
    loc (Elem e)        = loc e
    loc (Star e)        = loc e

instance HasLoc Assoc where
    loc (Assoc k v)     = loc k `upto` loc v
    loc (StarStar e)    = loc e

instance HasLoc Pattern where
    loc                 = ploc

instance HasLoc TSchema where
    loc (TSchema l _ _) = l

instance HasLoc TVar where
    loc (TV _ v)        = loc v

instance HasLoc TCon where
    loc (TC c ts)       = loc c `upto` loc ts

instance HasLoc Type where
    loc                 = tloc

instance HasLoc Constraint where
    loc (Cast _ t)      = loc t
    loc (Sub  _ _ t)    = loc t
    loc (Impl _ _ p)    = loc p
    loc (Sel _ _ n _)   = loc n
    loc (Mut _ n _)     = loc n


-- Eq -------------------------

instance Eq Import where
    x@Import{}          ==  y@Import{}          = moduls x == moduls y
    x@FromImport{}      ==  y@FromImport{}      = modul x == modul y && items x == items y
    x@FromImportAll{}   ==  y@FromImportAll{}   = modul x == modul y
    _                   ==  _                   = False

instance Eq Stmt where
    x@Expr{}            ==  y@Expr{}            = expr x == expr y
    x@Assign{}          ==  y@Assign{}          = patterns x == patterns y && expr x == expr y
    x@Update{}          ==  y@Update{}          = targets x == targets y && expr x == expr y
    x@IUpdate{}         ==  y@IUpdate{}         = target x == target y && aop x == aop y && expr x == expr y
    x@Assert{}          ==  y@Assert{}          = expr x == expr y && optExpr x == optExpr y
    x@Pass{}            ==  y@Pass{}            = True
    x@Delete{}          ==  y@Delete{}          = target x == target y
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
    x@After{}           ==  y@After{}           = expr x == expr y && expr2 x == expr2 y
    x@Decl{}            ==  y@Decl{}            = decls x == decls y
    x@Signature{}       ==  y@Signature{}       = vars x == vars y && typ x == typ y && dec x == dec y
    _                   ==  _                   = False

instance Eq Decl where
    Def _ n1 q1 p1 k1 a1 b1 m1  ==  Def _ n2 q2 p2 k2 a2 b2 m2  = n1 == n2 && q1 == q2 && p1 == p2 && k1 == k2 && a1 == a2 && b1 == b2 && m1 == m2
    Actor _ n1 q1 p1 k1 a1 b1   ==  Actor _ n2 q2 p2 k2 a2 b2   = n1 == n2 && q1 == q2 && p1 == p2 && k1 == k2 && a1 == a2 && b1 == b2
    Class _ n1 q1 a1 b1         ==  Class _ n2 q2 a2 b2         = n1 == n2 && q1 == q2 && a1 == a2 && b1 == b2
    Protocol _ n1 q1 a1 b1      ==  Protocol _ n2 q2 a2 b2      = n1 == n2 && q1 == q2 && a1 == a2 && b1 == b2
    Extension _ n1 q1 a1 b1     ==  Extension _ n2 q2 a2 b2     = n1 == n2 && q1 == q2 && a1 == a2 && b1 == b2
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
    x@Tuple{}           ==  y@Tuple{}           = pargs x == pargs y && kargs x == kargs y
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
    Derived n1 s1       == Derived n2 s2        = n1 == n2 && s1 == s2
    Internal s1 i1 p1   == Internal s2 i2 p2    = s1 == s2 && i1 == i2 && p1 == p2
    _                   == _                    = False

instance Ord Name where
    Name _ s1           <= Name _ s2            = s1 <= s2
    Derived n1 s1       <= Derived n2 s2        = (n1,s1) <= (n2,s2)
    Internal s1 i1 p1   <= Internal s2 i2 p2    = (s1,i1,p1) <= (s2,i2,p2)
    Name{}              <= Derived{}            = True
    Name{}              <= Internal{}           = True
    Derived{}           <= Internal{}           = True
    _                   <= _                    = False

instance Eq a => Eq (Op a) where
    Op _ x              ==  Op _ y              = x == y

instance Eq Except where
    ExceptAll _         ==  ExceptAll _         = True
    Except _ x1         ==  Except _ x2         = x1 == x2
    ExceptAs _ x1 n1    ==  ExceptAs _ x2 n2    = x1 == x2 && n1 == n2
    _                   ==  _                   = False

instance Eq Sliz where
    Sliz _ a1 b1 c1     ==  Sliz _ a2 b2 c2     = a1 == a2 && b1 == b2 && c1 == c2
    
instance Eq Comp where
    CompFor _ p1 e1 c1  ==  CompFor _ p2 e2 c2  = p1 == p2 && e1 == e2 && c1 == c2
    CompIf _ e1 c1      ==  CompIf _ e2 c2      = e1 == e2 && c1 == c2
    NoComp              ==  NoComp              = True
    _                   ==  _                   = False

instance Eq Pattern where
    PVar _ n1 a1        == PVar _ n2 a2         = n1 == n2 && a1 == a2
    PTuple _ p1 k1      == PTuple _ p2 k2       = p1 == p2 && k1 == k2
    PList _ ps1 p1      == PList _ ps2 p2       = ps1 == ps2 && p1 == p2
    PData _ n1 ix1      == PData _ n2 ix2       = n1 == n2 && ix1 == ix2
    PParen _ p1         == p2                   = p1 == p2
    p1                  == PParen _ p2          = p1 == p2
    _                   == _                    = False
instance Eq Target where
    TaVar _ n1          == TaVar _ n2           = n1 == n2
    TaTuple _ ts1       == TaTuple _ ts2        = ts1 == ts2
    TaIndex _ e1 ix1    == TaIndex _ e2 ix2     = e1 == e2 && ix1 == ix2
    TaSlice _ e1 sl1    == TaSlice _ e2 sl2     = e1 == e2 && sl1 == sl2
    TaDot _ e1 n1       == TaDot _ e2 n2        = e1 == e2 && n1 == n2
    TaParen _ t1        == t2                   = t1 == t2
    t1                  == TaParen _ t2         = t1 == t2
    _                   == _                    = False


instance Eq TSchema where
    TSchema _ q1 t1     == TSchema _ q2 t2      = q1 == q2 && t1 == t2

instance Eq TVar where
    TV k1 v1            == TV k2 v2             = v1 == v2

instance Eq Type where
    TVar _ v1           == TVar _ v2            = v1 == v2
    TCon _ c1           == TCon _ c2            = c1 == c2
    TExist _ p1         == TExist _ p2          = p1 == p2
    TFun _ e1 p1 r1 t1  == TFun _ e2 p2 r2 t2   = e1 == e2 && p1 == p2 && r1 == r2 && t1 == t2
    TTuple _ p1 r1      == TTuple _ p2 r2       = p1 == p2 && r1 == r2
    TUnion _ u1         == TUnion _ u2          = all (`elem` u2) u1 && all (`elem` u1) u2
    TOpt _ t1           == TOpt _ t2            = t1 == t2
    TNone _             == TNone _              = True
    TWild _             == TWild _              = True
    TNil _ s1           == TNil _ s2            = s1 == s2
    TRow _ s1 n1 t1 r1  == TRow _ s2 n2 t2 r2   = s1 == s2 && n1 == n2 && t1 == t2 && r1 == r2
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

isIdent s@(c:cs)                    = isAlpha c && all isAlphaNum cs && not (isKeyword s)
  where isAlpha c                   = c `elem` ['a'..'z'] || c `elem` ['A'..'Z'] || c == '_'
        isAlphaNum c                = isAlpha c || c `elem` ['0'..'9']

isKeyword x                         = x `Data.Set.member` rws
  where rws                         = Data.Set.fromDistinctAscList [
                                        "False","None","NotImplemented","Self","True","actor","after","and","as",
                                        "assert","await","break","class","continue","def","del","elif","else",
                                        "except","extension","finally","for","from","if","import","in",
                                        "is","lambda","not","or","protocol","pass","raise","return",
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

posParHead (PosPar a b c _)         = (a,b,c)
posArgHead (PosArg a _)             = a
posPatHead (PosPat a _)             = a
posRowHead (TRow _ PRow _ a _)      = a
 