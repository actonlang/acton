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

module Acton.Relabel where

import Acton.Syntax
import Utils
import Control.Monad.State.Lazy

relab t = evalState (relabel t) 0

-- Relabelling monad
type RM a = State Int a

newLoc :: RM SrcLoc
newLoc = do o <- get
            put (o+2)
            return (Loc o (o+1))

class Relabel a where
    relabel :: a -> RM a

instance Relabel a => Relabel [a] where
    relabel [] = return []
    relabel (a:as) = (:) <$> relabel a <*> relabel as

instance Relabel a => Relabel (Maybe a) where
    relabel Nothing = return Nothing
    relabel (Just a) = Just <$> relabel a

instance Relabel Module where
    relabel (Module qn imps ss) = Module <$> relabel qn <*> relabel imps <*> relabel ss

instance Relabel Import where
    relabel (Import _ ms) = Import <$> newLoc <*> relabel ms
    relabel (FromImport _ m ns) = FromImport <$> newLoc <*> relabel m <*> relabel ns
    relabel (FromImportAll _ m) = FromImportAll <$> newLoc <*> relabel m

instance Relabel Stmt where
    relabel (Expr _ e) = Expr <$> newLoc <*> relabel e
    relabel (Assign _ ps e) = Assign <$> newLoc <*> relabel ps <*> relabel e
    relabel (MutAssign _ t e) = MutAssign <$> newLoc <*> relabel t <*> relabel e
    relabel (AugAssign _ t op e) = AugAssign <$> newLoc <*> relabel t <*> pure op <*> relabel e
    relabel (Assert _ e mbe) = Assert <$> newLoc <*> relabel e <*> relabel mbe
    relabel (Pass _) = Pass <$> newLoc
    relabel (Delete _ t) = Delete <$> newLoc <*> relabel t
    relabel (Return _ mbe) = Return <$> newLoc <*> relabel mbe
    relabel (Raise _ e) = Raise <$> newLoc <*> relabel e
    relabel (Break _) = Break <$> newLoc
    relabel (Continue _) = Continue <$> newLoc
    relabel (If _ bs els) = If <$> newLoc <*> relabel bs <*> relabel els
    relabel (While _ e b els) = While <$> newLoc <*> relabel e <*> relabel b <*> relabel els
    relabel (For _ p e b els) = For <$> newLoc <*> relabel p <*> relabel e <*> relabel b <*> relabel els
    relabel (Try _ b hs els fin) = Try <$> newLoc <*> relabel b <*> relabel hs <*> relabel els <*> relabel fin
    relabel (With _ is b) = With <$> newLoc <*> relabel is <*> relabel b
    relabel (Data _ mbt ss) = Data <$> newLoc <*> relabel mbt <*> relabel ss
    relabel (VarAssign _ ps e) = VarAssign <$> newLoc <*> relabel ps <*> relabel e
    relabel (After _ e e') = After <$> newLoc <*> relabel e <*> relabel e'
    relabel (Decl _ ds) = Decl <$> newLoc <*> relabel ds
    relabel (Signature _ ns t d) = Signature <$> newLoc <*> relabel ns <*> relabel t <*> return d

instance Relabel Decl where
    relabel (Def _ n q ps ks ann ss dec fx) = Def <$> newLoc <*> relabel n <*> relabel q <*> relabel ps <*> relabel ks <*> 
                                            relabel ann <*> relabel ss <*> return dec <*> return fx
    relabel (Actor _ n q ps ks b) = Actor <$> newLoc <*> relabel n <*> relabel q <*> relabel ps <*> relabel ks <*> relabel b
    relabel (Class _ n q as ss) = Class <$> newLoc <*> relabel n <*> relabel q <*> relabel as <*> relabel ss
    relabel (Protocol _ n q as ss) = Protocol <$> newLoc <*> relabel n <*> relabel q <*> relabel as <*> relabel ss
    relabel (Extension _ q c as ss) = Extension <$> newLoc <*> relabel q <*> relabel c <*> relabel as <*> relabel ss

instance Relabel Expr where
    relabel (Var _ nm) = Var <$> newLoc <*> relabel nm
    relabel (Int _ i s) = Int <$> newLoc <*> return i <*> return s
    relabel (Float _ f s) = Float <$> newLoc <*> return f <*> return s
    relabel (Imaginary _ i s) = Imaginary <$> newLoc <*> return i <*> return s
    relabel (Bool _ b) = Bool <$> newLoc <*> return b
    relabel (None _) = None <$> newLoc
    relabel (NotImplemented _) = NotImplemented <$> newLoc
    relabel (Ellipsis _) = Ellipsis <$> newLoc
    relabel (Strings _ ss) = Strings <$> newLoc <*> return ss
    relabel (BStrings _ ss) = BStrings <$> newLoc <*> return ss
    relabel (Call _ e ps ks) = Call <$> newLoc <*> relabel e <*> relabel ps <*> relabel ks
    relabel (TApp _ e ts) = TApp <$> newLoc <*> relabel e <*> relabel ts
    relabel (Async _ e) = Async <$> newLoc <*> relabel e
    relabel (Await _ e) = Await <$> newLoc <*> relabel e
    relabel (Index _ e is) = Index <$> newLoc <*> relabel e <*> relabel is
    relabel (Slice _ e sl) = Slice <$> newLoc <*> relabel e <*> relabel sl
    relabel (NDSlice _ e sl) = NDSlice <$> newLoc <*> relabel e <*> relabel sl
    relabel (Cond _ e1 e2 e3) = Cond <$> newLoc <*> relabel e1 <*> relabel e2 <*> relabel e3
    relabel (IsInstance _ e c) = IsInstance <$> newLoc <*> relabel e <*> relabel c
    relabel (BinOp _ l op r) = BinOp <$> newLoc <*> relabel l <*> pure op <*> relabel r
    relabel (CompOp _ e ops) = CompOp <$> newLoc <*> relabel e <*> relabel ops
    relabel (UnOp _ op e) = UnOp <$> newLoc <*> pure op <*> relabel e 
    relabel (Dot _ e nm) = Dot <$> newLoc <*> relabel e <*> relabel nm
    relabel (Rest _ e nm) = Rest <$> newLoc <*> relabel e <*> relabel nm
    relabel (DotI _ e i) = DotI <$> newLoc <*> relabel e <*> return i
    relabel (RestI _ e i) = RestI <$> newLoc <*> relabel e <*> return i
    relabel (Lambda _ ps ks e fx) = Lambda <$> newLoc <*> relabel ps <*> relabel ks <*> relabel e <*> relabel fx
    relabel (Yield _ e) = Yield <$> newLoc <*> relabel e
    relabel (YieldFrom _ e) = YieldFrom <$> newLoc <*> relabel e
    relabel (Tuple _ ps ks) = Tuple <$> newLoc <*> relabel ps <*> relabel ks
    relabel (List _ es) = List <$> newLoc <*> relabel es
    relabel (ListComp _ e c) = ListComp <$> newLoc <*> relabel e <*> relabel c
    relabel (Dict _ as) = Dict <$> newLoc <*> relabel as
    relabel (DictComp _ a c) = DictComp <$> newLoc <*> relabel a <*> relabel c
    relabel (Set _ es) = Set <$> newLoc <*> relabel es
    relabel (SetComp _ e c) = SetComp <$> newLoc <*> relabel e <*> relabel c
    relabel (Paren _ e) = Paren <$> newLoc <*> relabel e

instance Relabel Pattern where
    relabel (PWild _ a) = PWild <$> newLoc <*> relabel a
    relabel (PVar _ n a) = PVar <$> newLoc <*> relabel n <*> relabel a
    relabel (PTuple _ ps ks) = PTuple <$> newLoc <*> relabel ps <*> relabel ks
    relabel (PList _ ps p) = PList <$> newLoc <*> relabel ps <*> relabel p
    relabel (PParen _ p) = PParen <$> newLoc <*> relabel p

instance Relabel Name where
  relabel (Name _ s) = Name <$> newLoc <*> return s
  relabel n = return n

instance Relabel ModName where
  relabel (ModName ns) = ModName <$> relabel ns

instance Relabel QName where
  relabel (QName m n) = QName <$> relabel m <*> relabel n
  relabel (NoQ n) = NoQ <$> relabel n
  relabel (GName m n) = GName <$> relabel m <*> relabel n

instance Relabel ModRef where
  relabel (ModRef (n,mbqn)) = (\m -> ModRef (n,m)) <$> relabel mbqn

instance Relabel ModuleItem where
  relabel (ModuleItem qn mbn) = ModuleItem <$> relabel qn <*> relabel mbn

instance Relabel ImportItem where
  relabel (ImportItem nm mbn) = ImportItem <$> relabel nm <*> relabel mbn

instance Relabel Branch where
    relabel (Branch e ss) = Branch <$> relabel e <*> relabel ss

instance Relabel Handler where
    relabel (Handler ex b) = Handler <$> relabel ex <*> relabel b

instance Relabel Except where
    relabel (ExceptAll _) = ExceptAll <$> newLoc
    relabel (Except _ x) = Except <$> newLoc <*> relabel x
    relabel (ExceptAs _ x n) = ExceptAs <$> newLoc <*> relabel x <*> relabel n

instance Relabel PosPar where
    relabel (PosPar n t e p) = PosPar <$> relabel n <*> relabel t <*> relabel e <*> relabel p
    relabel (PosSTAR n t) = PosSTAR <$> relabel n <*> relabel t
    relabel PosNIL = return PosNIL
    
instance Relabel KwdPar where
    relabel (KwdPar n t e k) = KwdPar <$> relabel n <*> relabel t <*> relabel e <*> relabel k
    relabel (KwdSTAR n t) = KwdSTAR <$> relabel n <*> relabel t
    relabel KwdNIL = return KwdNIL
    
instance Relabel PosArg where
    relabel (PosArg e p) = PosArg <$> relabel e <*> relabel p
    relabel (PosStar e) = PosStar <$> relabel e
    relabel PosNil = return PosNil
    
instance Relabel KwdArg where
    relabel (KwdArg n e k) = KwdArg <$> relabel n <*> relabel e <*> relabel k
    relabel (KwdStar e) = KwdStar <$> relabel e
    relabel KwdNil = return KwdNil
    
instance Relabel PosPat where
    relabel (PosPat p ps) = PosPat <$> relabel p <*> relabel ps
    relabel (PosPatStar p) = PosPatStar <$> relabel p
    relabel PosPatNil = return PosPatNil
    
instance Relabel KwdPat where
    relabel (KwdPat n p ps) = KwdPat <$> relabel n <*> relabel p <*> relabel ps
    relabel (KwdPatStar p) = KwdPatStar <$> relabel p
    relabel KwdPatNil = return KwdPatNil
    
instance Relabel OpArg where
    relabel (OpArg op e) = OpArg op <$> relabel e

instance Relabel Comp where
    relabel (CompFor _ p e c) = CompFor <$> newLoc <*> relabel p <*> relabel e <*> relabel c
    relabel (CompIf _ e c) = CompIf <$> newLoc <*> relabel e <*> relabel c
    relabel NoComp = return NoComp

instance Relabel WithItem where
    relabel (WithItem e p) = WithItem <$> relabel e <*> relabel p

instance Relabel Elem where
  relabel (Elem e) = Elem <$> relabel e
  relabel (Star e) = Star <$> relabel e

instance Relabel Assoc where
  relabel (Assoc e1 e2) = Assoc <$> relabel e1 <*> relabel e2
  relabel (StarStar e) = StarStar <$> relabel e
  
instance Relabel Sliz where
  relabel (Sliz _ e1 e2 e3) = Sliz <$> newLoc <*> relabel e1 <*> relabel e2 <*> relabel e3

instance Relabel NDSliz where
  relabel (NDExpr e) = NDExpr <$> relabel e
  relabel (NDSliz s) = NDSliz <$> relabel s

instance Relabel TSchema where
    relabel (TSchema _ q t) = TSchema <$> newLoc <*> relabel q <*> relabel t

instance Relabel TVar where
    relabel (TV k n) = TV k <$> relabel n

instance Relabel TCon where
    relabel (TC n ts) = TC <$> relabel n <*> relabel ts

instance Relabel QBind where
    relabel (Quant v cs) = Quant <$> relabel v <*> relabel cs

instance Relabel Type where
    relabel (TVar _ v) = TVar <$> newLoc <*> relabel v
    relabel (TFun _ es p k t) = TFun <$> newLoc <*> relabel es <*> relabel p <*> relabel k <*> relabel t
    relabel (TTuple _ p k) = TTuple <$> newLoc <*> relabel p <*> relabel k
    relabel (TOpt _ t) = TOpt <$> newLoc <*> relabel t
    relabel (TCon  _ c) = TCon <$> newLoc <*> relabel c
    relabel (TNone _) = TNone <$> newLoc
    relabel (TWild _) = TWild <$> newLoc
    relabel (TNil _ k) = TNil <$> newLoc <*> return k
    relabel (TRow _ k n t r) = TRow <$> newLoc <*> return k <*> relabel n <*> relabel t <*> relabel r
    relabel (TStar l k r) = TStar <$> newLoc <*> return k <*> relabel r
    relabel (TFX _ fx) = TFX <$> newLoc <*> pure fx

instance Relabel Constraint where
    relabel (Cast info t1 t2) = Cast info <$> relabel t1 <*> relabel t2
    relabel (Sub info w t1 t2) = Sub info <$> relabel w <*> relabel t1 <*> relabel t1
    relabel (Impl info w t p) = Impl info <$> relabel w <*> relabel t <*> relabel p
    relabel (Sel info w t1 n t2) = Sel info w <$> relabel t1 <*> relabel n <*> relabel t2
    relabel (Mut info t1 n t2) = Mut info <$> relabel t1 <*> relabel n <*> relabel t2
    relabel (Seal info t) = Seal info <$> relabel t
