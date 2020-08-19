module Acton.QuickType where

import Acton.Syntax
import Acton.Names
import Acton.Subst
import Acton.Env

class TypeOf a where
    typeof              :: Env -> a -> Type
{-
instance TypeOf Expr where
    typeof env (Var _ nm)           = Var <$> typeof env nm
    typeof env (Int _ i s)          = Int <$> return i <*> return s
    typeof env (Float _ f s)        = Float <$> return f <*> return s
    typeof env (Imaginary _ i s)    = Imaginary <$> return i <*> return s
    typeof env (Bool _ b)           = Bool <$> return b
    typeof env (None _)             = None <$> newLoc
    typeof env (NotImplemented _)   = NotImplemented <$> newLoc
    typeof env (Ellipsis _)         = Ellipsis <$> newLoc
    typeof env (Strings _ ss)       = Strings <$> return ss
    typeof env (BStrings _ ss)      = BStrings <$> return ss
    typeof env (Call _ e ts ps ks)  = Call <$> typeof env e <*> typeof env ts <*> typeof env ps <*> typeof env ks
    typeof env (Await _ e)          = Await <$> typeof env e
    typeof env (Index _ e is)       = Index <$> typeof env e <*> typeof env is
    typeof env (Slice _ e sl)       = Slice <$> typeof env e <*> typeof env sl
    typeof env (Cond _ e1 e2 e3)    = Cond <$> typeof env e1 <*> typeof env e2 <*> typeof env e3
    typeof env (BinOp _ l op r)     = BinOp <$> typeof env l <*> pure op <*> typeof env r
    typeof env (CompOp _ e ops)     = CompOp <$> typeof env e <*> typeof env ops
    typeof env (UnOp _ op e)        = UnOp <$> pure op <*> typeof env e
    typeof env (Dot _ e nm)         = Dot <$> typeof env e <*> typeof env nm
    typeof env (DotI _ e i t)       = DotI <$> typeof env e <*> return i <*> return t
    typeof env (Lambda _ p k e fx)  = Lambda <$> typeof env ps <*> typeof env ks <*> typeof env e <*> typeof env fx
    typeof env (Yield _ e)          = Yield <$> typeof env e
    typeof env (YieldFrom _ e)      = YieldFrom <$> typeof env e
    typeof env (Tuple _ ps ks)      = Tuple <$> typeof env ps <*> typeof env ks
    typeof env (List _ es)          = List <$> typeof env es
    typeof env (ListComp _ e c)     = ListComp <$> typeof env e <*> typeof env c
    typeof env (Dict _ as)          = Dict <$> typeof env as
    typeof env (DictComp _ a c)     = DictComp <$> typeof env a <*> typeof env c
    typeof env (Set _ es)           = Set <$> typeof env es
    typeof env (SetComp _ e c)      = SetComp <$> typeof env e <*> typeof env c
    typeof env (Paren _ e)          = Paren <$> typeof env e

instance TypeOf Pattern where
    typeof env (PVar _ n a)         = PVar <$> typeof env n <*> typeof env a
    typeof env (PTuple _ ps ks)     = PTuple <$> typeof env ps <*> typeof env ks
    typeof env (PList _ ps p)       = PList <$> typeof env ps <*> typeof env p
    typeof env (PParen _ p)         = PParen <$> typeof env p

instance TypeOf Exception where
    typeof env (Exception e mbe)    = Exception <$> typeof env e <*> typeof env mbe

instance TypeOf Name where
    typeof env (Name _ s)           = Name <$> return s
    typeof env n                    = return n

instance TypeOf ModName where
    typeof env (ModName ns)         = ModName <$> typeof env ns

instance TypeOf QName where
    typeof env (QName m n)          = QName <$> typeof env m <*> typeof env n
    typeof env (NoQ n)              = NoQ <$> typeof env n

instance TypeOf ModRef where
    typeof env (ModRef (n,mbqn))    = (\m -> ModRef (n,m)) <$> typeof env mbqn

instance TypeOf ModuleItem where
    typeof env (ModuleItem qn mbn)  = ModuleItem <$> typeof env qn <*> typeof env mbn

instance TypeOf ImportItem where
    typeof env (ImportItem nm mbn)  = ImportItem <$> typeof env nm <*> typeof env mbn

instance TypeOf Branch where
    typeof env (Branch e ss)        = Branch <$> typeof env e <*> typeof env ss

instance TypeOf Handler where
    typeof env (Handler ex b)       = Handler <$> typeof env ex <*> typeof env b

instance TypeOf Except where
    typeof env (ExceptAll _)        = ExceptAll <$> newLoc
    typeof env (Except _ x)         = Except <$> typeof env x
    typeof env (ExceptAs _ x n)     = ExceptAs <$> typeof env x <*> typeof env n

instance TypeOf PosPar where
    typeof env (PosPar n t e p)     = PosPar <$> typeof env n <*> typeof env t <*> typeof env e <*> typeof env p
    typeof env (PosSTAR n t)        = PosSTAR <$> typeof env n <*> typeof env t
    typeof env PosNIL               = return PosNIL

instance TypeOf KwdPar where
    typeof env (KwdPar n t e k)     = KwdPar <$> typeof env n <*> typeof env t <*> typeof env e <*> typeof env k
    typeof env (KwdSTAR n t)        = KwdSTAR <$> typeof env n <*> typeof env t
    typeof env KwdNIL               = return KwdNIL

instance TypeOf PosArg where
    typeof env (PosArg e p)         = PosArg <$> typeof env e <*> typeof env p
    typeof env (PosStar e)          = PosStar <$> typeof env e
    typeof env PosNil               = return PosNil

instance TypeOf KwdArg where
    typeof env (KwdArg n e k)       = KwdArg <$> typeof env n <*> typeof env e <*> typeof env k
    typeof env (KwdStar e)          = KwdStar <$> typeof env e
    typeof env KwdNil               = return KwdNil

instance TypeOf PosPat where
    typeof env (PosPat p ps)        = PosPat <$> typeof env p <*> typeof env ps
    typeof env (PosPatStar p)       = PosPatStar <$> typeof env p
    typeof env PosPatNil            = return PosPatNil

instance TypeOf KwdPat where
    typeof env (KwdPat n p ps)      = KwdPat <$> typeof env n <*> typeof env p <*> typeof env ps
    typeof env (KwdPatStar p)       = KwdPatStar <$> typeof env p
    typeof env KwdPatNil            = return KwdPatNil

instance TypeOf OpArg where
    typeof env (OpArg op e)         = OpArg op <$> typeof env e

instance TypeOf Comp where
    typeof env (CompFor _ p e c)    = CompFor <$> typeof env p <*> typeof env e <*> typeof env c
    typeof env (CompIf _ e c)       = CompIf <$> typeof env e <*> typeof env c
    typeof env NoComp               = return NoComp

instance TypeOf WithItem where
    typeof env (WithItem e p)       = WithItem <$> typeof env e <*> typeof env p

instance TypeOf Elem where
    typeof env (Elem e)             = Elem <$> typeof env e
    typeof env (Star e)             = Star <$> typeof env e

instance TypeOf Assoc where
    typeof env (Assoc e1 e2)        = Assoc <$> typeof env e1 <*> typeof env e2
    typeof env (StarStar e)         = StarStar <$> typeof env e

instance TypeOf Sliz where
    typeof env (Sliz _ e1 e2 e3)    = Sliz <$> typeof env e1 <*> typeof env e2 <*> typeof env e3

-}