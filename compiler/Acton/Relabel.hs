module Acton.Relabel where

import Acton.Syntax
import Utils
import Control.Monad.State.Lazy

relab t = evalState (relabel t) 0

-- Relabelling monad
type RM a = State Int a

newLoc :: RM SrcLoc
newLoc = do
              o <- get
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
    relabel (Assign _ ts e) = Assign <$> newLoc <*> relabel ts <*> relabel e
    relabel (AugAssign _ p op e) = AugAssign <$> newLoc <*> relabel p <*> relabel op <*> relabel e
    relabel (Assert _ e mbe) = Assert <$> newLoc <*> relabel e <*> relabel mbe
    relabel (Pass _) = Pass <$> newLoc
    relabel (Delete _ p) = Delete <$> newLoc <*> relabel p
    relabel (Return _ mbe) = Return <$> newLoc <*> relabel mbe
    relabel (Raise _ mbex) = Raise <$> newLoc <*> relabel mbex
    relabel (Break _) = Break <$> newLoc
    relabel (Continue _) = Continue <$> newLoc
    relabel (If _ bs els) = If <$> newLoc <*> relabel bs <*> relabel els
    relabel (While _ e b els) = While <$> newLoc <*> relabel e <*> relabel b <*> relabel els
    relabel (For _ p e b els) = For <$> newLoc <*> relabel p <*> relabel e <*> relabel b <*> relabel els
    relabel (Try _ b hs els fin) = Try <$> newLoc <*> relabel b <*> relabel hs <*> relabel els <*> relabel fin
    relabel (With _ is b) = With <$> newLoc <*> relabel is <*> relabel b
    relabel (Data _ mbt ss) = Data <$> newLoc <*> relabel mbt <*> relabel ss
    relabel (VarAssign _ ps e) = VarAssign <$> newLoc <*> relabel ps <*> relabel e
    relabel (Decl _ ds) = Decl <$> newLoc <*> relabel ds

instance Relabel Decl where
    relabel (Def _ n q ps ks ann ss md) = Def <$> newLoc <*> relabel n <*> relabel q <*> relabel ps <*> relabel ks <*> relabel ann <*> relabel ss <*> return md
    relabel (Actor _ n q ps ks ann b) = Actor <$> newLoc <*> relabel n <*> relabel q <*> relabel ps <*> relabel ks <*> relabel ann <*> relabel b
    relabel (Class _ n q as ss) = Class <$> newLoc <*> relabel n <*> relabel q <*> relabel as <*> relabel ss
    relabel (Protocol _ n q as ss) = Protocol <$> newLoc <*> relabel n <*> relabel q <*> relabel as <*> relabel ss
    relabel (Extension _ n q as ss) = Extension <$> newLoc <*> relabel n <*> relabel q <*> relabel as <*> relabel ss
    relabel (Signature _ ns t) = Signature <$> newLoc <*> relabel ns <*> relabel t

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
    relabel (Index _ e is) = Index <$> newLoc <*> relabel e <*> relabel is
    relabel (Slice _ e sl) = Slice <$> newLoc <*> relabel e <*> relabel sl
    relabel (Cond _ e1 e2 e3) = Cond <$> newLoc <*> relabel e1 <*> relabel e2 <*> relabel e3
    relabel (BinOp _ l op r) = BinOp <$> newLoc <*> relabel l <*> relabel op <*> relabel r
    relabel (CompOp _ e ops) = CompOp <$> newLoc <*> relabel e <*> relabel ops
    relabel (UnOp _ op e) = UnOp <$> newLoc <*> relabel op <*> relabel e 
    relabel (Dot _ e nm) = Dot <$> newLoc <*> relabel e <*> relabel nm
    relabel (DotI _ e i) = DotI <$> newLoc <*> relabel e <*> return i
    relabel (Lambda _ ps ks e) = Lambda <$> newLoc <*> relabel ps <*> relabel ks <*> relabel e
    relabel (Yield _ e) = Yield <$> newLoc <*> relabel e
    relabel (YieldFrom _ e) = YieldFrom <$> newLoc <*> relabel e
    relabel (Tuple _ es) = Tuple <$> newLoc <*> relabel es
    relabel (TupleComp _ e c) = TupleComp <$> newLoc <*> relabel e <*> relabel c
    relabel (Record _ fs) = Record <$> newLoc <*> relabel fs
    relabel (RecordComp _ n e c) = RecordComp <$> newLoc <*> relabel n <*> relabel e <*> relabel c
    relabel (List _ es) = List <$> newLoc <*> relabel es
    relabel (ListComp _ e c) = ListComp <$> newLoc <*> relabel e <*> relabel c
    relabel (Dict _ as) = Dict <$> newLoc <*> relabel as
    relabel (DictComp _ a c) = DictComp <$> newLoc <*> relabel a <*> relabel c
    relabel (Set _ es) = Set <$> newLoc <*> relabel es
    relabel (SetComp _ e c) = SetComp <$> newLoc <*> relabel e <*> relabel c
    relabel (Paren _ e) = Paren <$> newLoc <*> relabel e

instance Relabel Pattern where
    relabel (PVar _ n a) = PVar <$> newLoc <*> relabel n <*> relabel a
--    relabel (PRecord _ ps) = PRecord <$> newLoc <*> relabel ps
    relabel (PTuple _ ps) = PTuple <$> newLoc <*> relabel ps
    relabel (PList _ ps p) = PList <$> newLoc <*> relabel ps <*> relabel p
    relabel (PIndex _ e ix) = PIndex <$> newLoc <*> relabel e <*> relabel ix
    relabel (PSlice _ e sl) = PSlice <$> newLoc <*> relabel e <*> relabel sl
    relabel (PDot _ e n) = PDot <$> newLoc <*> relabel e <*> relabel n
    relabel (PParen _ p) = PParen <$> newLoc <*> relabel p

instance Relabel Exception where
  relabel (Exception e mbe) = Exception <$> relabel e <*> relabel mbe

instance Relabel Name where
  relabel (Name _ s) = Name <$> newLoc <*> return s

instance Relabel ModName where
  relabel (ModName ns) = ModName <$> relabel ns

instance Relabel QName where
  relabel (QName m n) = QName <$> relabel m <*> relabel n
  relabel (NoQual n) = NoQual <$> relabel n

instance Relabel ModRef where
  relabel (ModRef (n,mbqn)) = (\m -> ModRef (n,m)) <$> relabel mbqn

instance Relabel ModuleItem where
  relabel (ModuleItem qn mbn) = ModuleItem <$> relabel qn <*> relabel mbn

instance Relabel ImportItem where
  relabel (ImportItem nm mbn) = ImportItem <$> relabel nm <*> relabel mbn

instance Relabel (Op a) where
  relabel (Op _ a) = Op <$> newLoc <*> return a

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
    relabel (OpArg op e) = OpArg <$> relabel op <*> relabel e

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
  
instance Relabel Slice where
  relabel (Sliz _ e1 e2 e3) = Sliz <$> newLoc <*> relabel e1 <*> relabel e2 <*> relabel e3

instance Relabel TSchema where
    relabel (TSchema _ q t d) = TSchema <$> newLoc <*> relabel q <*> relabel t <*> return d

instance Relabel TVar where
    relabel (TV n) = TV <$> relabel n

instance Relabel TCon where
    relabel (TC n ts) = TC <$> relabel n <*> relabel ts

instance Relabel TBind where
    relabel (TBind v cs) = TBind <$> relabel v <*> relabel cs

instance Relabel Type where
    relabel (TVar _ v) = TVar <$> newLoc <*> relabel v
    relabel (TFun _ es p k t) = TFun <$> newLoc <*> relabel es <*> relabel p <*> relabel k <*> relabel t
    relabel (TTuple _ p) = TTuple <$> newLoc <*> relabel p
    relabel (TRecord _ k) = TRecord <$> newLoc <*> relabel k
    relabel (TOpt _ t) = TOpt <$> newLoc <*> relabel t
    relabel (TUnion _ as) = TUnion <$> newLoc <*> return as
    relabel (TCon  _ c) = TCon <$> newLoc <*> relabel c
    relabel (TAt  _ c) = TAt <$> newLoc <*> relabel c
    relabel (TNone _) = TNone <$> newLoc
    relabel (TWild _) = TWild <$> newLoc
    relabel (TNil _) = TNil <$> newLoc
    relabel (TRow _ n t r) = TRow <$> newLoc <*> relabel n <*> relabel t <*> relabel r

