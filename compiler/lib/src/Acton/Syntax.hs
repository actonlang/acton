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

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, DeriveGeneric, DeriveAnyClass #-}
module Acton.Syntax where

import Utils
import qualified Data.Binary
import qualified Data.Set
import qualified Data.HashMap.Strict as M
import qualified Data.Hashable
import Data.Char
import GHC.Generics (Generic)
import Control.DeepSeq
import Prelude hiding((<>))

version :: [Int]
version = [0,8]

data Module     = Module        { modname::ModName, imps::[Import], mbody::Suite } deriving (Eq,Show,Generic,NFData)

data Import     = Import        { iloc::SrcLoc, moduls::[ModuleItem] }
                | FromImport    { iloc::SrcLoc, modul::ModRef, items::[ImportItem] }
                | FromImportAll { iloc::SrcLoc, modul::ModRef }
                deriving (Show,Read,NFData,Generic)

type Suite      = [Stmt]

data Stmt       = Expr          { sloc::SrcLoc, expr::Expr }
                | Assign        { sloc::SrcLoc, patterns::[Pattern], expr::Expr }
                | MutAssign     { sloc::SrcLoc, target::Target, expr::Expr }
                | AugAssign     { sloc::SrcLoc, target::Target, aop::Aug, expr::Expr }
                | Assert        { sloc::SrcLoc, expr::Expr, optExpr::Maybe Expr }
                | Pass          { sloc::SrcLoc }
                | Delete        { sloc::SrcLoc, target::Target }
                | Return        { sloc::SrcLoc, optExpr::Maybe Expr }
                | Raise         { sloc::SrcLoc, expr::Expr }
                | Break         { sloc::SrcLoc }
                | Continue      { sloc::SrcLoc }
                | If            { sloc::SrcLoc, branches::[Branch], els::Suite }
                | While         { sloc::SrcLoc, expr::Expr, body::Suite, els::Suite }
                | For           { sloc::SrcLoc, pattern::Pattern, expr::Expr, body::Suite, els::Suite }
                | Try           { sloc::SrcLoc, body::Suite, handlers::[Handler], els::Suite, finally::Suite }
                | With          { sloc::SrcLoc, witems::[WithItem], body::Suite }
                | Data          { sloc::SrcLoc, mbpat::Maybe Pattern, dsuite::Suite }
                | VarAssign     { sloc::SrcLoc, patterns::[Pattern], expr::Expr }
                | After         { sloc::SrcLoc, expr::Expr, expr2::Expr }
                | Signature     { sloc::SrcLoc, vars::[Name], typ::TSchema, dec::Deco }
                | Decl          { sloc::SrcLoc, decls::[Decl] }
                deriving (Show,Read,NFData,Generic)

data Decl       = Def           { dloc::SrcLoc, dname:: Name, qbinds::QBinds, pos::PosPar, kwd::KwdPar, ann::Maybe Type, dbody::Suite, deco::Deco, dfx::TFX, ddoc::Maybe String }
                | Actor         { dloc::SrcLoc, dname:: Name, qbinds::QBinds, pos::PosPar, kwd::KwdPar, dbody::Suite, ddoc::Maybe String }
                | Class         { dloc::SrcLoc, dname:: Name, qbinds::QBinds, bounds::[TCon], dbody::Suite, ddoc::Maybe String }
                | Protocol      { dloc::SrcLoc, dname:: Name, qbinds::QBinds, bounds::[PCon], dbody::Suite, ddoc::Maybe String }
--                | Extension     { dloc::SrcLoc, dqname::QName, qbinds::QBinds, bounds::[PCon], dbody::Suite }
                | Extension     { dloc::SrcLoc, qbinds::QBinds, tycon::TCon, bounds::[PCon], dbody::Suite, ddoc::Maybe String }
                deriving (Show,Read,NFData,Generic)

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
                | Call          { eloc::SrcLoc, fun::Expr, pargs::PosArg, kargs::KwdArg }
                | TApp          { eloc::SrcLoc, fun::Expr, targs::[Type] }
                | Async         { eloc::SrcLoc, exp1::Expr }
                | Await         { eloc::SrcLoc, exp1::Expr }
                | Index         { eloc::SrcLoc, exp1::Expr, index::Expr }
                | Slice         { eloc::SrcLoc, exp1::Expr, slice::Sliz }
                | Cond          { eloc::SrcLoc, exp1::Expr, cond::Expr, exp2::Expr }
                | IsInstance    { eloc::SrcLoc, exp1::Expr, classref::QName }
                | BinOp         { eloc::SrcLoc, exp1::Expr, bop::Binary, exp2::Expr }
                | CompOp        { eloc::SrcLoc, exp1::Expr, ops::[OpArg] }
                | UnOp          { eloc::SrcLoc, uop::Unary, exp1::Expr }
                | Dot           { eloc::SrcLoc, exp1::Expr, attr::Name }
                | Rest          { eloc::SrcLoc, exp1::Expr, attr::Name }
                | DotI          { eloc::SrcLoc, exp1::Expr, ival::Integer }
                | RestI         { eloc::SrcLoc, exp1::Expr, ival::Integer }
                | Lambda        { eloc::SrcLoc, ppar::PosPar, kpar::KwdPar, exp1::Expr, efx::TFX }
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
                | Box           { tp :: Type, exp1 :: Expr }
                | UnBox         { tp :: Type, exp1 :: Expr }
                deriving (Show,Read,NFData,Generic)

data Pattern    = PWild         { ploc::SrcLoc, pann::Maybe Type }
                | PVar          { ploc::SrcLoc, pn::Name, pann::Maybe Type }
                | PParen        { ploc::SrcLoc, pat::Pattern }
                | PTuple        { ploc::SrcLoc, ppat::PosPat, kpat::KwdPat}
                | PList         { ploc::SrcLoc, pats::[Pattern], ptail::Maybe Pattern }
                | PData         { ploc::SrcLoc, pn::Name, pixs::[Expr] }
                deriving (Show,Read,NFData,Generic)

type Target     = Expr

data Prefix     = Globvar | Xistvar | Tempvar | Witness | NormPass | CPSPass | LLiftPass | BoxPass
                deriving (Eq,Ord,Show,Read,Generic,NFData)

data Name       = Name SrcLoc String | Derived Name Name | Internal Prefix String Int deriving (Generic,Show,NFData)

nloc (Name l _) = l
nloc _          = NoLoc

nstr (Name _ s)             = esc s
  where esc (c:'_':s)
          | isUpper c       = c : {- 'X' : -} '_' : esc s
        esc (c:s)           = c : esc s
        esc ""              = ""
nstr (Derived n s)
  | Internal{} <- s         = nstr n ++ nstr s
  | otherwise               = nstr n ++ "D_" ++ nstr s
nstr (Internal p s i)       = prefix p ++ "_" ++ unique i ++ s
  where prefix Globvar      = "G"
        prefix Xistvar      = "E"
        prefix Tempvar      = "V"
        prefix Witness      = "W"
        prefix NormPass     = "N"
        prefix CPSPass      = "C"
        prefix LLiftPass    = "L"
        prefix BoxPass      = "U"
        unique 0            = ""
        unique i            = show i

rawstr (Name _ s)           = s
rawstr n                    = nstr n

name            = Name NoLoc

nWild           = name "_"

globalName s    = Internal Globvar s 0

globalNames s   = map (Internal Globvar s) [1..]


data ModName    = ModName [Name] deriving (Show,Read,Eq,Generic,NFData)

modName ss      = ModName (map name ss)

modPath (ModName ns) = map nstr ns

modCat (ModName ns) n = ModName (ns++[n])

instance Ord ModName where
    compare a b = compare (modPath a) (modPath b)

data QName      = QName { mname::ModName, noq::Name } | NoQ { noq::Name } | GName { mname::ModName, noq::Name } deriving (Show,Read,Eq,Ord,Generic,NFData)

qName ss s      = QName (modName ss) (name s)

noQ s           = NoQ (name s)


data ModuleItem = ModuleItem ModName (Maybe Name) deriving (Show,Eq,Read,NFData,Generic)
data ModRef     = ModRef (Int, Maybe ModName) deriving (Show,Eq,Read,NFData,Generic)
data ImportItem = ImportItem Name (Maybe Name) deriving (Show,Eq,Read,NFData,Generic)
data Branch     = Branch Expr Suite deriving (Show,Eq,Read,NFData,Generic)
data Handler    = Handler Except Suite deriving (Show,Eq,Read,NFData,Generic)
data Except     = ExceptAll SrcLoc | Except SrcLoc QName | ExceptAs SrcLoc QName Name deriving (Show,Read,NFData,Generic)

data Elem       = Elem Expr | Star Expr deriving (Show,Eq,Read,NFData,Generic)
data Assoc      = Assoc Expr Expr | StarStar Expr deriving (Show,Eq,Read,NFData,Generic)

data PosPar     = PosPar Name (Maybe Type) (Maybe Expr) PosPar | PosSTAR Name (Maybe Type) | PosNIL deriving (Show,Eq,Read,NFData,Generic)
data KwdPar     = KwdPar Name (Maybe Type) (Maybe Expr) KwdPar | KwdSTAR Name (Maybe Type) | KwdNIL deriving (Show,Eq,Read,NFData,Generic)

data PosArg     = PosArg Expr PosArg | PosStar Expr | PosNil deriving (Show,NFData,Eq,Read,Generic)
data KwdArg     = KwdArg Name Expr KwdArg | KwdStar Expr | KwdNil deriving (Show,Eq,Read,NFData,Generic)

data PosPat     = PosPat Pattern PosPat | PosPatStar Pattern | PosPatNil deriving (Show,Eq,Read,NFData,Generic)
data KwdPat     = KwdPat Name Pattern KwdPat | KwdPatStar Pattern | KwdPatNil deriving (Show,Eq,Read,NFData,Generic)

data OpArg      = OpArg Comparison Expr deriving (Eq,Show,Read,NFData,Generic)
data Sliz       = Sliz SrcLoc (Maybe Expr) (Maybe Expr) (Maybe Expr) deriving (Show,Read,NFData,Generic)
data Comp       = CompFor SrcLoc Pattern Expr Comp | CompIf SrcLoc Expr Comp | NoComp deriving (Show,Read,NFData,Generic)
data WithItem   = WithItem Expr (Maybe Pattern) deriving (Show,Eq,Read,NFData,Generic)


data Unary      = Not|UPlus|UMinus|BNot deriving (Show,Eq,Read,NFData,Generic)
data Binary     = Or|And|Plus|Minus|Mult|Pow|Div|Mod|EuDiv|BOr|BXor|BAnd|ShiftL|ShiftR|MMult deriving (Show,Read,Eq,NFData,Generic)
data Aug        = PlusA|MinusA|MultA|PowA|DivA|ModA|EuDivA|BOrA|BXorA|BAndA|ShiftLA|ShiftRA|MMultA deriving (Show,Eq,Read,NFData,Generic)
data Comparison = Eq|NEq|LtGt|Lt|Gt|GE|LE|In|NotIn|Is|IsNot deriving (Show,Eq,Read,NFData,Generic)

data Deco       = NoDec | Property | Static deriving (Eq,Show,Read,Generic,NFData)

data Kind       = KType | KProto | KFX | PRow | KRow | KFun [Kind] Kind | KUni Int | KWild deriving (Eq,Ord,Show,Read,Generic,NFData)

data TSchema    = TSchema { scloc::SrcLoc, scbind::QBinds, sctype::Type } deriving (Show,Read,Generic,NFData)

data TVar       = TV { tvkind::Kind, tvname::Name } -- the Name is an uppercase letter, optionally followed by digits.
                deriving (Show,Read,Generic,NFData)

data TUni       = UV { uvkind::Kind, uvid::Int }
                deriving (Show,Read,Generic,NFData)

univar k i      = UV k i
unitoken i      = UV KWild (-(i*2))
uniwild k i     = UV k (-(i*2+1))

data TCon       = TC { tcname::QName, tcargs::[Type] } deriving (Eq,Show,Read,Generic,NFData)

data FX         = FXPure | FXMut | FXProc | FXAction deriving (Eq,Show,Read,Generic,NFData)

data QBind      = QBind TVar [TCon] deriving (Eq,Show,Read,Generic,NFData)

type QBinds     = [QBind]

type PCon       = TCon
type CCon       = TCon

type KUni       = Int

data Type       = TUni      { tloc::SrcLoc, uvar::TUni }
                | TVar      { tloc::SrcLoc, tvar::TVar }
                | TCon      { tloc::SrcLoc, tcon::TCon }
                | TFun      { tloc::SrcLoc, effect::TFX, posrow::PosRow, kwdrow::KwdRow, restype::Type }
                | TTuple    { tloc::SrcLoc, posrow::PosRow, kwdrow::KwdRow }
                | TOpt      { tloc::SrcLoc, opttype::Type }
                | TNone     { tloc::SrcLoc }
                | TWild     { tloc::SrcLoc }
                | TNil      { tloc::SrcLoc, rkind::Kind }
                | TRow      { tloc::SrcLoc, rkind::Kind, label::Name, rtype::Type, rtail::TRow }
                | TStar     { tloc::SrcLoc, rkind::Kind, rtail::TRow }
                | TFX       { tloc::SrcLoc, tfx::FX }
                deriving (Show,Read,Generic,NFData)

type TFX        = Type
type PosRow     = Type
type KwdRow     = Type
type TRow       = Type

data Constraint = Cast  {info :: ErrInfo, qual :: QBinds, type1 :: Type, type2 :: Type}
                | Sub   {info :: ErrInfo, wit :: Name, qual :: QBinds, type1 :: Type, type2 :: Type}
                | Proto {info :: ErrInfo, wit :: Name, qual :: QBinds, type1 :: Type, proto1 :: PCon}
                | Sel   {info :: ErrInfo, wit :: Name, qual :: QBinds, type1 :: Type, name1 :: Name, type2 :: Type}
                | Mut   {info :: ErrInfo, qual :: QBinds, type1 :: Type, name1 :: Name, type2 :: Type}
                | Seal  {info :: ErrInfo, qual :: QBinds, type1 :: Type}
                | Imply {info :: ErrInfo, wit :: Name, binder :: QBinds, scoped :: Constraints}
                deriving (Eq,Show,Read,Generic,NFData)

type Constraints = [Constraint]

data ErrInfo    = DfltInfo {errloc :: SrcLoc, errno :: Int, errexpr :: Maybe Expr, errinsts :: [(QName,TSchema,Type)]}
                | DeclInfo {errloc :: SrcLoc, errloc2 :: SrcLoc, errname :: Name, errschema :: TSchema, errmsg :: String}
                | Simple {errloc ::SrcLoc, errmsg :: String}
                deriving (Eq,Show,Read,Generic,NFData)

type WPath      = [Either QName QName]

type WTCon      = (WPath,PCon)

leftpath tcs    = [ (map Left ns, tc) | (ns,tc) <- nss `zip` tcs ]
  where nss     = tail $ inits $ map tcname tcs

mkBody []       = [Pass NoLoc]
mkBody b        = b


sDef n p t b fx = sDecl [Def NoLoc n [] p KwdNIL (Just t) b NoDec fx Nothing]
sReturn e       = Return NoLoc (Just e)
sAssign p e     = Assign NoLoc [p] e
sMutAssign t e  = MutAssign NoLoc t e
sRaise e        = Raise NoLoc e
sExpr e         = Expr NoLoc e
sDecl ds        = Decl NoLoc ds
sIf bs els      = If NoLoc bs els
sIf1 e b els    = sIf [Branch e b] els
sNotImpl        = Expr NoLoc eNotImpl
sPass           = Pass NoLoc
sBreak          = Break NoLoc
sContinue       = Continue NoLoc

handler qn b    = Handler (Except NoLoc qn) b

tApp e []       = e
tApp e ts       = TApp NoLoc e ts

eCall e es      = Call NoLoc e (posarg es) KwdNil
eCallVar c es   = eCall (eVar c) es
eCallV c es     = eCall (Var NoLoc c) es
eCallP e as     = Call NoLoc e as KwdNil
eTuple es       = Tuple NoLoc (posarg es) KwdNil
eTupleP args    = Tuple NoLoc args KwdNil
eTupleK args    = Tuple NoLoc PosNil args
eQVar n         = Var NoLoc n
eVar n          = Var NoLoc (NoQ n)
eDot e n        = Dot NoLoc e n
eDotI e i       = DotI NoLoc e i
eNone           = None NoLoc
eCond e b e'    = Cond NoLoc e b e'
eInt n          = Int NoLoc n (show n)
eBool b         = Bool NoLoc b
eBinOp e o e'   = BinOp NoLoc e o e'
eLambda nts e   = Lambda NoLoc (pospar nts) KwdNIL e fxPure
eLambda' nts e  = Lambda NoLoc (pospar nts) KwdNIL e fxProc
eAsync e        = Async NoLoc e
eAwait e        = Await NoLoc e
eNotImpl        = NotImplemented NoLoc
eIsInstance x n = IsInstance NoLoc (eVar x) n

pospar nts      = foldr (\(n,t) p -> PosPar n (Just t) Nothing p) PosNIL nts
pospar' ns      = foldr (\n p -> PosPar n Nothing Nothing p) PosNIL ns

pospars' (PosPar n _ _ p)
                = n : pospars' p
pospars' PosNIL = []

posarg es       = foldr PosArg PosNil es

posargs (PosArg e p)
                = e : posargs p
posargs (PosStar e)
                = [e]
posargs PosNil  = []

selfPar Def{pos=PosPar x _ _ _} = Just x
selfPar Def{kwd=KwdPar x _ _ _} = Just x
selfPar _                       = Nothing

pVar n t        = PVar NoLoc n (Just t)
pVar' n         = PVar NoLoc n Nothing

monotype t      = TSchema NoLoc [] t
tSchema q t     = TSchema NoLoc q t

quant v         = QBind v []
qbound q        = [ tv | QBind tv _ <- q ]

tVar v          = TVar NoLoc v
tUni v          = TUni NoLoc v
tCon c          = TCon NoLoc c
tFun fx p k t   = TFun NoLoc fx p k t
tTuple p k      = TTuple NoLoc p k
tTupleP p       = TTuple NoLoc p kwdNil
tTupleK k       = TTuple NoLoc posNil k
tUnit           = tTuple posNil kwdNil
tOpt t@TOpt{}   = t
tOpt t          = TOpt NoLoc t
tNone           = TNone NoLoc
tWild           = TWild NoLoc
tNil k          = TNil NoLoc k
tRow k          = TRow NoLoc k
tStar k         = TStar NoLoc k
tTFX fx         = TFX NoLoc fx

tCon0 n q       = tCon $ TC n [ tVar tv | QBind tv _ <- q ]

tFun0 ps t      = tFun fxPure (foldr posRow posNil ps) kwdNil t

tSelf           = TVar NoLoc tvSelf
tvSelf          = TV KType nSelf
nSelf           = Name NoLoc "Self"

fxSelf          = TV KFX nSelf

fxPure          = tTFX FXPure
fxMut           = tTFX FXMut
fxProc          = tTFX FXProc
fxAction        = tTFX FXAction
fxWild          = tWild

fxFun fx1 fx2   = tFun fxPure (posRow (tF0 fx1) posNil) kwdNil (tF0 fx2)
  where tF0 fx  = tFun fx posNil kwdNil tNone

posNil          = tNil PRow
posRow          = tRow PRow nWild
posStar         = tStar PRow
posStar'        = posStar . maybe tWild tVar

kwdNil          = tNil KRow
kwdRow          = tRow KRow
kwdStar         = tStar KRow
kwdStar'        = kwdStar . maybe tWild tVar

prowOf (PosPar n a d p) = posRow (dflt d $ case a of Just t -> t; _ -> tWild) (prowOf p)
  where dflt Nothing    = id
        dflt (Just e)   = tOpt
prowOf (PosSTAR n a)    = posStar (case a of Just (TTuple _ r _) -> r; _ -> tWild)
--prowOf (PosSTAR n a)    = (case a of Just (TTuple _ r _) -> r; _ -> tWild)
prowOf PosNIL           = posNil

krowOf (KwdPar n a d k) = kwdRow n (dflt d $ case a of Just t -> t; _ -> tWild) (krowOf k)
  where dflt Nothing    = id
        dflt (Just e)   = tOpt
krowOf (KwdSTAR n a)    = kwdStar (case a of Just (TTuple _ _ r) -> r; _ -> tWild)
--krowOf (KwdSTAR n a)    = (case a of Just (TTuple _ _ r) -> r; _ -> tWild)
krowOf KwdNIL           = kwdNil

pArg (PosPar n a _ p)   = PosArg (eVar n) (pArg p)
pArg (PosSTAR n a)      = PosStar (eVar n)
pArg PosNIL             = PosNil

kArg (KwdPar n a _ p)   = KwdArg n (eVar n) (kArg p)
kArg (KwdSTAR n a)      = KwdStar (eVar n)
kArg KwdNIL             = KwdNil

chop 0 _                = PosNIL
chop i (PosPar n a d p) = PosPar n a d (chop (i-1) p)
chop _ p                = p

arity (TRow _ _ _ _ r)  = 1 + arity r
arity _                 = 0

pPar ns (TRow _ PRow n t p)
                        = PosPar (head ns) (Just t) Nothing (pPar (tail ns) p)
pPar ns (TNil _ PRow)   = PosNIL
pPar ns (TStar _ PRow r)= PosSTAR (head ns) (Just $ tTupleP r)

kPar kw (TRow _ KRow n t r)
                        = KwdPar n (Just t) Nothing (kPar kw r)
kPar kw (TNil _ KRow)   = KwdNIL
kPar kw (TStar _ KRow r)= KwdSTAR kw (Just $ tTupleK r)

tRowLoc t@TRow{}        = getLoc [tloc t, loc (rtype t)]

tvarSupply              = [ TV KType $ name (c:tl) | tl <- "" : map show [1..], c <- "ABCDEFGHIJKLMNOPQRSTUVW" ]

fxSupply                = [ TV KType $ name (c:tl) | tl <- "" : map show [1..], c <- "XYZ" ]

tvarSupplyMap vs avoid  = map setk (vs `zip` (tvarSupply \\ avoid))
  where setk (v,v')     = (v, tVar $ v'{ tvkind = tvkind v })


type Substitution       = [(TVar,Type)]

type TEnv               = [(Name, NameInfo)]

data NameInfo           = NVar      Type
                        | NSVar     Type
                        | NDef      TSchema Deco (Maybe String)
                        | NSig      TSchema Deco (Maybe String)
                        | NAct      QBinds PosRow KwdRow TEnv (Maybe String)
                        | NClass    QBinds [WTCon] TEnv (Maybe String)
                        | NProto    QBinds [WTCon] TEnv (Maybe String)
                        | NExt      QBinds TCon [WTCon] TEnv [Name] (Maybe String)
                        | NTVar     Kind CCon [PCon]
                        | NAlias    QName
                        | NMAlias   ModName
                        | NModule   TEnv (Maybe String)
                        | NReserved
                        deriving (Eq,Show,Read,Generic)

type HTEnv            =  M.HashMap Name HNameInfo

data HNameInfo          = HNVar      Type
                        | HNSVar     Type
                        | HNDef      TSchema Deco (Maybe String)
                        | HNSig      TSchema Deco (Maybe String)
                        | HNAct      QBinds PosRow KwdRow TEnv (Maybe String)
                        | HNClass    QBinds [WTCon] TEnv (Maybe String)
                        | HNProto    QBinds [WTCon] TEnv (Maybe String)
                        | HNExt      QBinds TCon [WTCon] TEnv [Name] (Maybe String)
                        | HNTVar     Kind CCon [PCon]
                        | HNAlias    QName
                        | HNMAlias   ModName
                        | HNModule   HTEnv (Maybe String)
                        | HNReserved
                        deriving (Eq, Show, Read, Generic)


instance Data.Hashable.Hashable Name where
    hashWithSalt s (Name _ nstr)    = Data.Hashable.hashWithSalt s nstr
    hashWithSalt s (Derived  n1 n2) = Data.Hashable.hashWithSalt s (n1,n2)
    hashWithSalt s (Internal pre str n) = Data.Hashable.hashWithSalt s (show pre,str,n)

--data TEnvs              = TEnvs { nmod::NameInfo, tchecked::NameInfo, normalized::NameInfo, deactorized:: NameInfo, cpsconverted:: NameInfo, llifted:: NameInfo }
--                        deriving (Eq,Show,Read,Generic)

convNameInfo2HNameInfo               :: NameInfo -> HNameInfo
convNameInfo2HNameInfo (NModule te mdoc)      = HNModule (convTEnv2HTEnv te) mdoc
convNameInfo2HNameInfo (NVar t)               = HNVar t
convNameInfo2HNameInfo (NSVar t)              = HNSVar t
convNameInfo2HNameInfo (NDef sc dec mdoc)     = HNDef sc dec mdoc
convNameInfo2HNameInfo (NSig sc dec mdoc)     = HNSig sc dec mdoc
convNameInfo2HNameInfo (NAct q p k te mdoc)   = HNAct q p k te mdoc
convNameInfo2HNameInfo (NClass q ws te mdoc)  = HNClass q ws te mdoc
convNameInfo2HNameInfo (NProto q ws te mdoc)  = HNProto q ws te mdoc
convNameInfo2HNameInfo (NExt q tc ws te ns mdoc) = HNExt q tc ws te ns mdoc
convNameInfo2HNameInfo (NTVar k c ps)         = HNTVar k c ps
convNameInfo2HNameInfo (NAlias qn)            = HNAlias qn
convNameInfo2HNameInfo (NMAlias mn)           = HNMAlias mn
convNameInfo2HNameInfo (NReserved)            = HNReserved

convHNameInfo2NameInfo               :: HNameInfo -> NameInfo
convHNameInfo2NameInfo (HNModule te mdoc)      = NModule (convHTEnv2TEnv te) mdoc
convHNameInfo2NameInfo (HNVar t)               = NVar t
convHNameInfo2NameInfo (HNSVar t)              = NSVar t
convHNameInfo2NameInfo (HNDef sc dec mdoc)     = NDef sc dec mdoc
convHNameInfo2NameInfo (HNSig sc dec mdoc)     = NSig sc dec mdoc
convHNameInfo2NameInfo (HNAct q p k te mdoc)   = NAct q p k te mdoc
convHNameInfo2NameInfo (HNClass q ws te mdoc)  = NClass q ws te mdoc
convHNameInfo2NameInfo (HNProto q ws te mdoc)  = NProto q ws te mdoc
convHNameInfo2NameInfo (HNExt q tc ws te ns mdoc) = NExt q tc ws te ns mdoc
convHNameInfo2NameInfo (HNTVar k c ps)         = NTVar k c ps
convHNameInfo2NameInfo (HNAlias qn)            = NAlias qn
convHNameInfo2NameInfo (HNMAlias mn)           = NMAlias mn
convHNameInfo2NameInfo (HNReserved)            = NReserved

convTEnv2HTEnv                       :: TEnv -> HTEnv
convTEnv2HTEnv te                     = M.fromList (map convPair te)
  where
     convPair (n, ni)      = (n, convNameInfo2HNameInfo ni)

convHTEnv2TEnv                       :: HTEnv -> TEnv
convHTEnv2TEnv te                     = map convPair (M.toList te)
  where
     convPair (n, hni)      = (n, convHNameInfo2NameInfo hni)



-- | Strip all docstrings from NameInfo (and nested environments).
-- This is used when computing a public-interface hash so that
-- documentation-only edits do not cause dependents to rebuild.
stripDocsNI :: NameInfo -> NameInfo
stripDocsNI ni = case ni of
  NModule te _        -> NModule (map stripBind te) Nothing
  NAct q p k te _     -> NAct q p k (map stripBind te) Nothing
  NClass q cs te _    -> NClass q cs (map stripBind te) Nothing
  NProto q ps te _    -> NProto q ps (map stripBind te) Nothing
  NExt q c ps te o _  -> NExt q c ps (map stripBind te) o Nothing
  NDef sc dec _       -> NDef sc dec Nothing
  NSig sc dec _       -> NSig sc dec Nothing
  other               -> other
  where
    stripBind (n, info) = (n, stripDocsNI info)

data Witness            = WClass    { binds::QBinds, wtype::Type, proto::PCon, wname::QName, wsteps::WPath, wopts::Int }
                        | WInst     { binds::QBinds, wtype::Type, proto::PCon, wname::QName, wsteps::WPath }
                        deriving (Show)

typeDecl (_,NDef{})     = False
typeDecl _              = True

-- Finding type leaves -----

class Leaves a where
    leaves                  :: a -> [Type]

instance (Leaves a) => Leaves [a] where
    leaves                  = concatMap leaves

instance (Leaves a) => Leaves (Name,a) where
    leaves (n,x)            = leaves x

instance Leaves NameInfo where
    leaves (NClass q cs te _) = leaves q ++ leaves cs ++ leaves te
    leaves (NProto q ps te _) = leaves q ++ leaves ps ++ leaves te
    leaves (NAct q p k te _)  = leaves q ++ leaves [p,k] ++ leaves te
    leaves (NExt q c ps te _ _) = leaves q ++ leaves c ++ leaves ps ++ leaves te
    leaves (NDef sc dec _)    = leaves sc
    leaves _                  = []

instance Leaves QBind where
    leaves (QBind tv ps)    = tVar tv : leaves ps

instance Leaves (WPath,PCon) where
    leaves (wp,p)           = leaves p

instance Leaves TSchema where
    leaves (TSchema _ q t)  = leaves q ++ leaves t

instance Leaves Type where
    leaves t@TCon{}         = [t]
    leaves t@TVar{}         = [t]
    leaves t@TUni{}         = [t]
    leaves t@TFX{}          = [t]
    leaves (TFun _ x p k t) = leaves [x,p,k,t]
    leaves (TTuple _ p k)   = leaves [p,k]
    leaves (TOpt _ t)       = leaves t
    leaves (TRow _ _ _ t r) = leaves [t,r]
    leaves (TStar _ _ r)    = leaves r
    leaves _                = []

instance Leaves TCon where
    leaves tc               = [tCon tc]

instance Leaves TVar where
    leaves tv               = [tVar tv]


instance Data.Binary.Binary Prefix
instance Data.Binary.Binary Name
instance Data.Binary.Binary ModName
instance Data.Binary.Binary QName
instance Data.Binary.Binary Deco
instance Data.Binary.Binary TSchema
instance Data.Binary.Binary TVar
instance Data.Binary.Binary TUni
instance Data.Binary.Binary TCon
instance Data.Binary.Binary QBind
instance Data.Binary.Binary Type
instance Data.Binary.Binary Kind
instance Data.Binary.Binary FX
instance Data.Binary.Binary ErrInfo
instance Data.Binary.Binary Constraint
instance Data.Binary.Binary NameInfo
instance Data.Binary.Binary Expr
instance Data.Binary.Binary Stmt
instance Data.Binary.Binary Decl
instance Data.Binary.Binary Branch
instance Data.Binary.Binary Handler
instance Data.Binary.Binary Except
instance Data.Binary.Binary Elem
instance Data.Binary.Binary Assoc
instance Data.Binary.Binary Pattern
instance Data.Binary.Binary PosPar
instance Data.Binary.Binary KwdPar
instance Data.Binary.Binary PosArg
instance Data.Binary.Binary KwdArg
instance Data.Binary.Binary PosPat
instance Data.Binary.Binary KwdPat
instance Data.Binary.Binary OpArg
instance Data.Binary.Binary Sliz
instance Data.Binary.Binary Comp
instance Data.Binary.Binary WithItem
instance Data.Binary.Binary Unary
instance Data.Binary.Binary Binary
instance Data.Binary.Binary Aug
instance Data.Binary.Binary Comparison
instance Data.Binary.Binary Module
instance Data.Binary.Binary Import
instance Data.Binary.Binary ModuleItem
instance Data.Binary.Binary ImportItem
instance Data.Binary.Binary ModRef


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
    loc (GName m n)     = loc m `upto` loc n

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

instance HasLoc TUni where
    loc (UV _ v)        = NoLoc

instance HasLoc TCon where
    loc (TC c ts)       = loc c `upto` loc ts

instance HasLoc Type where
    loc                 = tloc

instance HasLoc Constraint where
      loc (Cast info q t1 t2) = getLoc [loc info, loc t1, loc t2]
      loc (Sub info _ q t1 t2) = getLoc [loc info, loc t1, loc t2]
      loc (Proto info _ q t1 _) = getLoc [loc info, loc t1]
      loc (Sel info _ q t1  n1 t2) = getLoc [loc info, loc t1, loc n1, loc t2]
      loc (Mut info q t1  n1 t2) = getLoc [loc info, loc t1, loc n1, loc t2]
      loc (Seal info q t1) = getLoc [loc info, loc t1]
      loc (Imply info _ q cs) =  getLoc [loc info, loc cs]

instance HasLoc ErrInfo where
      loc (Simple l _)   = l
      loc (DfltInfo l _ _ _) = l
      loc (DeclInfo l _ _ _ _) = l

instance HasLoc PosArg where
      loc (PosArg e p) = loc e  `upto` loc p
      loc (PosStar e)  = loc e
      loc PosNil       = NoLoc

instance HasLoc KwdArg where
      loc (KwdArg n e k) = loc n `upto` loc k
      loc (KwdStar e)    = loc e
      loc KwdNil         = NoLoc

-- Eq -------------------------

instance Eq Import where
    x@Import{}          ==  y@Import{}          = moduls x == moduls y
    x@FromImport{}      ==  y@FromImport{}      = modul x == modul y && items x == items y
    x@FromImportAll{}   ==  y@FromImportAll{}   = modul x == modul y
    _                   ==  _                   = False

instance Eq Stmt where
    x@Expr{}            ==  y@Expr{}            = expr x == expr y
    x@Assign{}          ==  y@Assign{}          = patterns x == patterns y && expr x == expr y
    x@MutAssign{}       ==  y@MutAssign{}       = target x == target y && expr x == expr y
    x@AugAssign{}       ==  y@AugAssign{}       = target x == target y && aop x == aop y && expr x == expr y
    x@Assert{}          ==  y@Assert{}          = expr x == expr y && optExpr x == optExpr y
    x@Pass{}            ==  y@Pass{}            = True
    x@Delete{}          ==  y@Delete{}          = target x == target y
    x@Return{}          ==  y@Return{}          = optExpr x == optExpr y
    x@Raise{}           ==  y@Raise{}           = expr x == expr y
    x@Break{}           ==  y@Break{}           = True
    x@Continue{}        ==  y@Continue{}        = True
    x@If{}              ==  y@If{}              = branches x == branches y && els x == els y
    x@While{}           ==  y@While{}           = expr x == expr y && body x == body y && els x == els y
    x@For{}             ==  y@For{}             = pattern x == pattern y && expr x == expr y && body x == body y
                                                  && els x == els y
    x@Try{}             ==  y@Try{}             = body x == body y && handlers x == handlers y && els x == els y
                                                  && finally x == finally y
    x@With{}            ==  y@With{}            = witems x == witems y && body x == body y
    x@Data{}            ==  y@Data{}            = mbpat x == mbpat y && dsuite x == dsuite y
    x@VarAssign{}       ==  y@VarAssign{}       = patterns x == patterns y && expr x == expr y
    x@After{}           ==  y@After{}           = expr x == expr y && expr2 x == expr2 y
    x@Decl{}            ==  y@Decl{}            = decls x == decls y
    x@Signature{}       ==  y@Signature{}       = vars x == vars y && typ x == typ y && dec x == dec y
    _                   ==  _                   = False

instance Eq Decl where
    Def _ n1 q1 p1 k1 a1 b1 m1 d1 doc1   ==  Def _ n2 q2 p2 k2 a2 b2 m2 d2 doc2
                                                                    = n1==n2 && q1==q2 && p1==p2 && k1==k2 && a1==a2 && b1==b2 && d1==d2 && m1==m2 && doc1==doc2
    Actor _ n1 q1 p1 k1 b1 doc1          ==  Actor _ n2 q2 p2 k2 b2 doc2      = n1 == n2 && q1 == q2 && p1 == p2 && k1 == k2 && b1 == b2 && doc1 == doc2
    Class _ n1 q1 a1 b1 doc1             ==  Class _ n2 q2 a2 b2 doc2         = n1 == n2 && q1 == q2 && a1 == a2 && b1 == b2 && doc1 == doc2
    Protocol _ n1 q1 a1 b1 doc1          ==  Protocol _ n2 q2 a2 b2 doc2      = n1 == n2 && q1 == q2 && a1 == a2 && b1 == b2 && doc1 == doc2
    Extension _ q1 c1 a1 b1 doc1         ==  Extension _ q2 c2 a2 b2 doc2     = q1 == q2 && c1 == c2 && a1 == a2 && b1 == b2 && doc1 == doc2
    _                               == _                            = False

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
    x@Call{}            ==  y@Call{}            = fun x == fun y && pargs x == pargs y && kargs x == kargs y
    x@TApp{}            ==  y@TApp{}            = fun x == fun y && targs x == targs y
    x@Async{}           ==  y@Async{}           = exp1 x == exp1 y
    x@Await{}           ==  y@Await{}           = exp1 x == exp1 y
    x@Index{}           ==  y@Index{}           = exp1 x == exp1 y && index x == index y
    x@Slice{}           ==  y@Slice{}           = exp1 x == exp1 y && slice x == slice y
    x@Cond{}            ==  y@Cond{}            = exp1 x == exp1 y && cond x == cond y && exp2 x == exp2 y
    x@IsInstance{}      ==  y@IsInstance{}      = exp1 x == exp1 y && classref x == classref y
    x@BinOp{}           ==  y@BinOp{}           = exp1 x == exp1 y && bop x == bop y && exp2 x == exp2 y
    x@CompOp{}          ==  y@CompOp{}          = exp1 x == exp1 y && ops x == ops y
    x@UnOp{}            ==  y@UnOp{}            = uop x == uop y && exp1 x == exp1 y
    x@Dot{}             ==  y@Dot{}             = exp1 x == exp1 y && attr x == attr y
    x@Rest{}            ==  y@Rest{}            = exp1 x == exp1 y && attr x == attr y
    x@DotI{}            ==  y@DotI{}            = exp1 x == exp1 y && ival x == ival y
    x@RestI{}           ==  y@RestI{}           = exp1 x == exp1 y && ival x == ival y
    x@Lambda{}          ==  y@Lambda{}          = ppar x == ppar y && kpar x == kpar y && exp1 x == exp1 y && efx x == efx y
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
    Internal p1 s1 i1   == Internal p2 s2 i2    = p1 == p2 && s1 == s2 && i1 == i2
    _                   == _                    = False

instance Ord Name where
    Name _ s1           <= Name _ s2            = s1 <= s2
    Derived n1 s1       <= Derived n2 s2        = (n1,s1) <= (n2,s2)
    Internal p1 s1 i1   <= Internal p2 s2 i2    = (p1,s1,i1) <= (p2,s2,i2)
    Name{}              <= Derived{}            = True
    Name{}              <= Internal{}           = True
    Derived{}           <= Internal{}           = True
    _                   <= _                    = False

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
    PWild _ a1          == PWild _ a2           = a1 == a2
    PVar _ n1 a1        == PVar _ n2 a2         = n1 == n2 && a1 == a2
    PTuple _ p1 k1      == PTuple _ p2 k2       = p1 == p2 && k1 == k2
    PList _ ps1 p1      == PList _ ps2 p2       = ps1 == ps2 && p1 == p2
    PData _ n1 ix1      == PData _ n2 ix2       = n1 == n2 && ix1 == ix2
    PParen _ p1         == p2                   = p1 == p2
    p1                  == PParen _ p2          = p1 == p2
    _                   == _                    = False

instance Eq TSchema where
    TSchema _ q1 t1     == TSchema _ q2 t2      = q1 == q2 && t1 == t2

instance Eq TVar where
    TV k1 v1            == TV k2 v2             = v1 == v2

instance Eq TUni where
    UV k1 i1            == UV k2 i2             = i1 == i2

instance Ord TVar where
    TV _ v1             <= TV _ v2              = v1 <= v2

instance Ord TUni where
    UV _ i1             <= UV _ i2              = i1 <= i2

instance Eq Type where
    TUni _ u1           == TUni _ u2            = u1 == u2
    TVar _ v1           == TVar _ v2            = v1 == v2
    TCon _ c1           == TCon _ c2            = c1 == c2
    TFun _ e1 p1 r1 t1  == TFun _ e2 p2 r2 t2   = e1 == e2 && p1 == p2 && r1 == r2 && t1 == t2
    TTuple _ p1 r1      == TTuple _ p2 r2       = p1 == p2 && r1 == r2
    TOpt _ t1           == TOpt _ t2            = t1 == t2
    TNone _             == TNone _              = True
    TWild _             == TWild _              = True
    TNil _ k1           == TNil _ k2            = k1 == k2
    TRow _ k1 n1 t1 r1  == TRow _ k2 n2 t2 r2   = k1 == k2 && n1 == n2 && t1 == t2 && r1 == r2
    TStar _ k1 r1       == TStar _ k2 r2        = k1 == k2 && r1 == r2
    TFX _ fx1           == TFX _ fx2            = fx1 == fx2
    _                   == _                    = False

-- Show & Read ----------------

--instance Show Name where
--    show n              = show (nstr n)

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

unop op e                           = UnOp l0 op e
binop e1 op e2                      = BinOp l0 e1 op e2
cmp e1 op e2                        = CompOp l0 e1 [OpArg op e2]

mkStringLit s                       = Strings l0 ['\'' : s ++ "\'"]

isIdent s@(c:cs)                    = isAlpha c && all isAlphaNum cs && not (isKeyword s)
  where isAlpha c                   = c `elem` ['a'..'z'] || c `elem` ['A'..'Z'] || c == '_'
        isAlphaNum c                = isAlpha c || c `elem` ['0'..'9']

isKeyword x                         = x `Data.Set.member` rws
  where rws                         = Data.Set.fromDistinctAscList [
                                        "False","None","NotImplemented","Self","True","action","actor","after","and","as",
                                        "assert","async","await","break","class","continue","def","del","elif","else",
                                        "except","extension","finally","for","from","if","import","in","is","isinstance",
                                        "lambda","mut","not","or","pass","proc","protocol","pure","raise","return","try",
                                        "var","while","with","yield","_"
                                      ]

isSig Signature{}                   = True
isSig _                             = False

isDecl Decl{}                       = True
isDecl _                            = False

isQVar (Var _ n)                    = Just n
isQVar (TApp _ e _)                 = isQVar e
isQVar e                            = Nothing

isVar e                             = case isQVar e of Just (NoQ n) -> Just n; _ -> Nothing

hasNotImpl ss                       = any isNotImpl ss

-- Check for __cleanup__ method on actor
hasCleanup ss                       = any isCleanup ss

isCleanup (Def _ n _ _ _ _ _ _ _ _) = n == Name NoLoc "__cleanup__"

isNotImpl (Expr _ e)                = e == eNotImpl
isNotImpl (Assign _ _ e)            = e == eNotImpl
isNotImpl (Decl _ ds)               = any (hasNotImpl . dbody) ds
isNotImpl _                         = False

notImplBody b                       = not $ null $ notImpls b

notImpls b                          = [ e | Expr _ e <- b, e == eNotImpl ]

isUnivar TUni{}                     = True
isUnivar _                          = False

singlePosArg (PosArg _ PosNil)      = True
singlePosArg _                      = False

singlePosPat (PosPat _ PosPatNil)   = True
singlePosPat _                      = False

posParHead (PosPar a b c _)         = (a,b,c)
posArgHead (PosArg a _)             = a
