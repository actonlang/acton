{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Acton.Transform where

import Utils
import Acton.Syntax


class Transform a where
    trans                           :: TransEnv -> a -> a

data TransEnv                       = TransEnv

instance (Transform a) => Transform [a] where
    trans env                       = map (trans env)

instance (Transform a) => Transform (Maybe a) where
    trans env                       = fmap (trans env)

instance Transform Stmt where
    trans env (Expr l e)            = Expr l (trans env e)
    trans env (Assign l ts e)       = Assign l (trans env ts) (trans env e)
    trans env (Update l ts e)       = Update l (trans env ts) (trans env e)
    trans env (IUpdate l t op e)    = IUpdate l (trans env t) op (trans env e)
    trans env (Assert l e mbe)      = Assert l (trans env e) (trans env mbe)
    trans env (Pass l)              = Pass l
    trans env (Delete l t)          = Delete l (trans env t)
    trans env (Return l mbe)        = Return l (trans env mbe)
    trans env (Raise l mbex)        = Raise l (trans env mbex)
    trans env (Break l)             = Break l
    trans env (Continue l)          = Continue l
    trans env (If l bs els)         = If l (trans env bs) (trans env els)
    trans env (While l e b els)     = While l (trans env e) (trans env b) (trans env els)
    trans env (For l p e b els)     = For l (trans env p) (trans env e) (trans env b) (trans env els)
    trans env (Try l b hs els fin)  = Try l (trans env b) (trans env hs) (trans env els) (trans env fin)
    trans env (With l is b)         = With l (trans env is) (trans env b)
    trans env (Data l mbt ss)       = Data l (trans env mbt) (trans env ss)
    trans env (VarAssign l ps e)    = VarAssign l (trans env ps) (trans env e)
    trans env (After l e e')        = After l (trans env e) (trans env e')
    trans env (Decl l ds)           = Decl l (trans env ds)
    trans env (Signature l ns t d)  = Signature l ns (trans env t) d

instance Transform Decl where
    trans env (Def l n q p k t b d fx)  = Def l n (trans env q) (trans env p) (trans env k) (trans env t) (trans env b) d fx
    trans env (Actor l n q p k b)       = Actor l n (trans env q) (trans env p) (trans env k) (trans env b)
    trans env (Class l n q us b)        = Class l n (trans env q) (trans env us) (trans env b)
    trans env (Protocol l n q us b)     = Protocol l n (trans env q) (trans env us) (trans env b)
    trans env (Extension l n q us b)    = Extension l n (trans env q) (trans env us) (trans env b)

instance Transform Expr where
--    trans env (Call l (Var l' n) p k)
--      | Just w <- expansions env n  = Call l (Var l' n) (w $ trans env p) (trans env k)   
--    trans en (Var l n)
--      | Just w <- expansions env n  = 
    
    
    trans env (Call l e p k)        = Call l (trans env e) (trans env p) (trans env k)
    trans env (Await l e)           = Await l (trans env e)
    trans env (Index l e is)        = Index l (trans env e) (trans env is)
    trans env (Slice l e sl)        = Slice l (trans env e) (trans env sl)
    trans env (Cond l e1 e2 e3)     = Cond l (trans env e1) (trans env e2) (trans env e3)
    trans env (BinOp l e1 op e2)    = BinOp l (trans env e1) op (trans env e2)
    trans env (CompOp l e ops)      = CompOp l (trans env e) (trans env ops)
    trans env (UnOp l op e)         = UnOp l op (trans env e)
    trans env (Dot l e n)           = Dot l (trans env e) n
    trans env (DotI l e i tl)       = DotI l (trans env e) i tl
    trans env (Lambda l ps ks e fx) = Lambda l (trans env ps) (trans env ks) (trans env e) (trans env fx)
    trans env (Yield l e)           = Yield l (trans env e)
    trans env (YieldFrom l e)       = YieldFrom l (trans env e)
    trans env (Tuple l ps ks)       = Tuple l (trans env ps) (trans env ks)
    trans env (List l es)           = List l (trans env es)
    trans env (ListComp l e c)      = ListComp l (trans env e) (trans env c)
    trans env (Dict l as)           = Dict l (trans env as)
    trans env (DictComp l a c)      = DictComp l (trans env a) (trans env c)
    trans env (Set l es)            = Set l (trans env es)
    trans env (SetComp l e c)       = SetComp l (trans env e) (trans env c)
    trans env (Paren l e)           = Paren l (trans env e)
    trans env e                     = e

instance Transform Pattern where
    trans env (PVar l n a)          = PVar l n (trans env a)
    trans env (PTuple l ps ks)      = PTuple l (trans env ps) (trans env ks)
    trans env (PList l ps p)        = PList l (trans env ps) (trans env p)
    trans env (PParen l p)          = PParen l (trans env p)

instance Transform Target where
    trans env (TaVar l n)           = TaVar l n
    trans env (TaTuple l ps)        = TaTuple l (trans env ps)
    trans env (TaIndex l e ix)      = TaIndex l (trans env e) (trans env ix)
    trans env (TaSlice l e sl)      = TaSlice l (trans env e) (trans env sl)
    trans env (TaDot l e n)         = TaDot l (trans env e) n
    trans env (TaParen l t)         = TaParen l (trans env t)

instance Transform Exception where
  trans env (Exception e mbe)       = Exception (trans env e) (trans env mbe)

instance Transform Branch where
    trans env (Branch e ss)         = Branch (trans env e) (trans env ss)

instance Transform Handler where
    trans env (Handler ex b)        = Handler ex (trans env b)

instance Transform PosPar where
    trans env (PosPar n t e p)      = PosPar n (trans env t) (trans env e) (trans env p)
    trans env (PosSTAR n t)         = PosSTAR n (trans env t)
    trans env PosNIL                = PosNIL
    
instance Transform KwdPar where
    trans env (KwdPar n t e k)      = KwdPar n (trans env t) (trans env e) (trans env k)
    trans env (KwdSTAR n t)         = KwdSTAR n (trans env t)
    trans env KwdNIL                = KwdNIL
    
instance Transform PosArg where
    trans env (PosArg e p)          = PosArg (trans env e) (trans env p)
    trans env (PosStar e)           = PosStar (trans env e)
    trans env PosNil                = PosNil
    
instance Transform KwdArg where
    trans env (KwdArg n e k)        = KwdArg n (trans env e) (trans env k)
    trans env (KwdStar e)           = KwdStar (trans env e)
    trans env KwdNil                = KwdNil
    
instance Transform PosPat where
    trans env (PosPat p ps)         = PosPat (trans env p) (trans env ps)
    trans env (PosPatStar p)        = PosPatStar (trans env p)
    trans env PosPatNil             = PosPatNil
    
instance Transform KwdPat where
    trans env (KwdPat n p ps)       = KwdPat n (trans env p) (trans env ps)
    trans env (KwdPatStar p)        = KwdPatStar (trans env p)
    trans env KwdPatNil             = KwdPatNil
    
instance Transform OpArg where
    trans env (OpArg op e)          = OpArg op (trans env e)

instance Transform Comp where
    trans env (CompFor l p e c)     = CompFor l (trans env p) (trans env e) (trans env c)
    trans env (CompIf l e c)        = CompIf l (trans env e) (trans env c)
    trans env NoComp                = NoComp

instance Transform WithItem where
    trans env (WithItem e p)        = WithItem (trans env e) (trans env p)

instance Transform Elem where
  trans env (Elem e)                = Elem (trans env e)
  trans env (Star e)                = Star (trans env e)

instance Transform Assoc where
  trans env (Assoc e1 e2)           = Assoc (trans env e1) (trans env e2)
  trans env (StarStar e)            = StarStar (trans env e)
  
instance Transform Sliz where
  trans env (Sliz l e1 e2 e3)       = Sliz l (trans env e1) (trans env e2) (trans env e3)



instance Transform TSchema where
    trans env (TSchema l q t)       = TSchema l (trans env q) (trans env t)

instance Transform TVar where
    trans env (TV k n)              = TV k n

instance Transform TCon where
    trans env (TC n ts)             = TC n (trans env ts)

instance Transform TBind where
    trans env (TBind v cs)          = TBind (trans env v) (trans env cs)

instance Transform Type where
    trans env (TVar l v)            = TVar l (trans env v)
    trans env (TFun l es p k t)     = TFun l (trans env es) (trans env p) (trans env k) (trans env t)
    trans env (TTuple l p k)        = TTuple l (trans env p) (trans env k)
    trans env (TOpt l t)            = TOpt l (trans env t)
    trans env (TUnion l as)         = TUnion l as
    trans env (TCon  l c)           = TCon l (trans env c)
    trans env (TExist  l p)         = TExist l (trans env p)
    trans env (TNone l)             = TNone l
    trans env (TWild l)             = TWild l
    trans env (TNil l k)            = TNil l k
    trans env (TRow l k n t r)      = TRow l k n (trans env t) (trans env r)
    trans env (TFX l fx)            = TFX l (trans env fx)

instance Transform FX where
    trans env (FXAsync)             = FXAsync
    trans env (FXAct t)             = FXAct (trans env t)
    trans env (FXMut t)             = FXMut (trans env t)
    trans env (FXPure)              = FXPure

