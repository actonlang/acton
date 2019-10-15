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
    relabel (TypeSig _ ns t) = TypeSig <$> newLoc <*> relabel ns <*> relabel t
    relabel (Assert _ es) = Assert <$> newLoc <*> relabel es
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
    relabel (Def _ n q ps mba ss md) = Def <$> newLoc <*> relabel n <*> relabel q <*> relabel ps <*> relabel mba <*> relabel ss <*> return md
    relabel (Actor _ n q ps ann b) = Actor <$> newLoc <*> relabel n <*> relabel q <*> relabel ps <*> relabel ann <*> relabel b
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
    relabel (UStrings _ ss) = UStrings <$> newLoc <*> return ss
    relabel (Call _ e es) = Call <$> newLoc <*> relabel e <*> relabel es
    relabel (Ix _ e is) = Ix <$> newLoc <*> relabel e <*> relabel is
    relabel (Cond _ e1 e2 e3) = Cond <$> newLoc <*> relabel e1 <*> relabel e2 <*> relabel e3
    relabel (BinOp _ l op r) = BinOp <$> newLoc <*> relabel l <*> relabel op <*> relabel r
    relabel (CompOp _ e ops) = CompOp <$> newLoc <*> relabel e <*> relabel ops
    relabel (UnOp _ op e) = UnOp <$> newLoc <*> relabel op <*> relabel e 
    relabel (Dot _ e nm) = Dot <$> newLoc <*> relabel e <*> relabel nm
    relabel (DotI _ e i) = DotI <$> newLoc <*> relabel e <*> return i
    relabel (Lambda _ ps e) = Lambda <$> newLoc <*> relabel ps <*> relabel e
    relabel (Yield _ e) = Yield <$> newLoc <*> relabel e
    relabel (YieldFrom _ e) = YieldFrom <$> newLoc <*> relabel e
    relabel (Tuple _ es) = Tuple <$> newLoc <*> relabel es
    relabel (Generator _ e c) = Generator <$> newLoc <*> relabel e <*> relabel c
    relabel (List _ es) = List <$> newLoc <*> relabel es
    relabel (ListComp _ e c) = ListComp <$> newLoc <*> relabel e <*> relabel c
    relabel (Dict _ as) = Dict <$> newLoc <*> relabel as
    relabel (DictComp _ a c) = DictComp <$> newLoc <*> relabel a <*> relabel c
    relabel (Set _ es) = Set <$> newLoc <*> relabel es
    relabel (SetComp _ e c) = SetComp <$> newLoc <*> relabel e <*> relabel c
    relabel (Record _ fs) = Record <$> newLoc <*> relabel fs
    relabel (RecordComp _ n e c) = RecordComp <$> newLoc <*> relabel n <*> relabel e <*> relabel c
    relabel (Paren _ e) = Paren <$> newLoc <*> relabel e

instance Relabel Pattern where
    relabel (PVar _ n a) = PVar <$> newLoc <*> relabel n <*> relabel a
    relabel (PTuple _ ps) = PTuple <$> newLoc <*> relabel ps
    relabel (PList _ ps) = PList <$> newLoc <*> relabel ps
    relabel (PIx _ e ix) = PIx <$> newLoc <*> relabel e <*> relabel ix
    relabel (PDot _ e n) = PDot <$> newLoc <*> relabel e <*> relabel n
    relabel (PParen _ p) = PParen <$> newLoc <*> relabel p

instance Relabel Exception where
  relabel (Exception e mbe) = Exception <$> relabel e <*> relabel mbe

instance Relabel Name where
  relabel (Name _ s) = Name <$> newLoc <*> return s

instance Relabel QName where
  relabel (QName nm nms) = QName <$> relabel nm <*> relabel nms

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
    relabel (Except _ e) = Except <$> newLoc <*> relabel e
    relabel (ExceptAs _ e n) = ExceptAs <$> newLoc <*> relabel e <*> relabel n

instance Relabel Param where
    relabel (Param nm mba mbe) = Param <$> relabel nm <*> relabel mba <*> relabel mbe

instance Relabel Params where
    relabel (Params p s1 k s2) = Params <$> relabel p <*> relabel s1 <*> relabel k <*> relabel s2

instance Relabel StarPar where
    relabel (StarPar _ n mbt) = StarPar <$> newLoc <*> relabel n <*> relabel mbt
    relabel NoStar = return NoStar
    
instance Relabel Arg where
    relabel (Arg e) = Arg <$> relabel e
    relabel (KwArg nm e) = KwArg <$> relabel nm <*> relabel e
    relabel (StarArg e) = StarArg <$> relabel e
    relabel (StarStarArg e) = StarStarArg <$> relabel e

instance Relabel OpArg where
    relabel (OpArg op e) = OpArg <$> relabel op <*> relabel e

instance Relabel Comp where
    relabel (CompFor _ p e c) = CompFor <$> newLoc <*> relabel p <*> relabel e <*> relabel c
    relabel (CompIf _ e c) = CompIf <$> newLoc <*> relabel e <*> relabel c
    relabel NoComp = return NoComp

instance Relabel WithItem where
    relabel (WithItem e p) = WithItem <$> relabel e <*> relabel p

instance Relabel e => Relabel (Elem e) where
  relabel (Elem e) = Elem <$> relabel e
  relabel (Star e) = Star <$> relabel e

instance Relabel Assoc where
  relabel (Assoc e1 e2) = Assoc <$> relabel e1 <*> relabel e2
  relabel (StarStarAssoc e) = StarStarAssoc <$> relabel e
  
instance Relabel Index where
  relabel (Index _ e) = Index <$> newLoc <*> relabel e

instance Relabel Field where
  relabel (Field nm e) = Field <$> relabel nm <*> relabel e
  relabel (StarStarField e) = StarStarField <$> relabel e

instance Relabel OType where
  relabel = undefined

instance Relabel TSchema where
    relabel (TSchema _ q t) = TSchema <$> newLoc <*> relabel q <*> relabel t

instance Relabel TVar where
    relabel (TV n) = TV <$> relabel n

instance Relabel TCon where
    relabel (TC n ts) = TC <$> relabel n <*> relabel ts

instance Relabel TBind where
    relabel (TBind v cs) = TBind <$> relabel v <*> relabel cs

instance Relabel PosRow where
    relabel (PosRow t p) = PosRow <$> relabel t <*> relabel p
    relabel (PosVar v) = PosVar <$> relabel v
    relabel PosNil = return PosNil

instance Relabel KwdRow where
    relabel (KwdRow n t k) = KwdRow <$> relabel n <*> relabel t <*> relabel k
    relabel (KwdVar v) = KwdVar <$> relabel v
    relabel KwdNil = return KwdNil

instance Relabel Type where
    relabel (TSelf _) = TSelf <$> newLoc
    relabel (TVar _ v) = TVar <$> newLoc <*> relabel v
    relabel (TFun _ es p k t) = TFun <$> newLoc <*> relabel es <*> relabel p <*> relabel k <*> relabel t
    relabel (TTuple _ p) = TTuple <$> newLoc <*> relabel p
    relabel (TRecord _ k) = TRecord <$> newLoc <*> relabel k
    relabel (TOpt _ t) = TOpt <$> newLoc <*> relabel t
    relabel (TUnion _ as) = TUnion <$> newLoc <*> return as
    relabel (TCon  _ c) = TCon <$> newLoc <*> relabel c
    relabel (TAt  _ c) = TAt <$> newLoc <*> relabel c
    relabel (TNone _) = TNone <$> newLoc
