{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Acton.Transform where

import Utils
import Acton.Syntax
import Acton.Names


transform fun x                         = trans TransEnv{ trfun = fun, trscope = [] } x

class Transform a where
    trans                               :: TransEnv -> a -> a

data TransEnv                           = TransEnv { trfun :: [Name] -> Expr -> Maybe Expr, trscope :: [Name] }

extscope ns env                         = env{ trscope = ns ++ trscope env }


instance (Transform a) => Transform [a] where
    trans env                           = map (trans env)

instance (Transform a) => Transform (Maybe a) where
    trans env                           = fmap (trans env)

instance Transform Stmt where
    trans env (Expr l e)                = Expr l (trans env e)
    trans env (Assign l ps e)           = Assign l ps (trans env e)
    trans env (Update l ts e)           = Update l (trans env ts) (trans env e)
    trans env (IUpdate l t op e)        = IUpdate l (trans env t) op (trans env e)
    trans env (Assert l e mbe)          = Assert l (trans env e) (trans env mbe)
    trans env (Delete l t)              = Delete l (trans env t)
    trans env (Return l mbe)            = Return l (trans env mbe)
    trans env (Raise l mbex)            = Raise l (trans env mbex)
    trans env (If l bs els)             = If l (trans env bs) (trans env els)
    trans env (While l e b els)         = While l (trans env e) (trans env b) (trans env els)
    trans env (For l p e b els)         = For l p (trans env e) (trans env1 b) (trans env els)
      where env1                        = extscope (bound p) env
    trans env (Try l b hs els fin)      = Try l (trans env b) (trans env hs) (trans env els) (trans env fin)
    trans env (With l is b)             = With l (trans env1 is) (trans env1 b)
      where env1                        = extscope (bound is) env
    trans env (Data l mbp ss)           = Data l mbp (trans env ss)
    trans env (VarAssign l ps e)        = VarAssign l ps (trans env e)
    trans env (After l e e')            = After l (trans env e) (trans env e')
    trans env (Decl l ds)               = Decl l (trans env ds)
    trans env s                         = s

instance Transform Decl where
    trans env (Def l n q p k t b d fx)  = Def l n q (trans env1 p) (trans env1 k) t (trans env1 b) d fx
      where env1                        = extscope (bound q ++ bound p ++ bound k) env
    trans env (Actor l n q p k b)       = Actor l n q (trans env1 p) (trans env1 k) (trans env1 b)
      where env1                        = extscope (bound q ++ bound p ++ bound k) env
    trans env (Class l n q us b)        = Class l n q us (trans env1 b)
      where env1                        = extscope (bound q) env
    trans env (Protocol l n q us b)     = Protocol l n q us (trans env1 b)
      where env1                        = extscope (bound q) env
    trans env (Extension l n q us b)    = Extension l n q us (trans env1 b)
      where env1                        = extscope (bound q) env

instance Transform Expr where
    trans env e                         = case trfun env (trscope env) e of
                                            Just e' -> trans' env e'
                                            Nothing -> trans' env e

trans' env (Call l e p k)               = Call l (trans env e) (trans env p) (trans env k)
trans' env (Await l e)                  = Await l (trans env e)
trans' env (Index l e is)               = Index l (trans env e) (trans env is)
trans' env (Slice l e sl)               = Slice l (trans env e) (trans env sl)
trans' env (Cond l e1 e2 e3)            = Cond l (trans env e1) (trans env e2) (trans env e3)
trans' env (BinOp l e1 op e2)           = BinOp l (trans env e1) op (trans env e2)
trans' env (CompOp l e ops)             = CompOp l (trans env e) (trans env ops)
trans' env (UnOp l op e)                = UnOp l op (trans env e)
trans' env (Dot l e n)                  = Dot l (trans env e) n
trans' env (DotI l e i tl)              = DotI l (trans env e) i tl
trans' env (Lambda l p k e fx)          = Lambda l (trans env1 p) (trans env1 k) (trans env1 e) fx
  where env1                            = extscope (bound p ++ bound k) env
trans' env (Yield l e)                  = Yield l (trans env e)
trans' env (YieldFrom l e)              = YieldFrom l (trans env e)
trans' env (Tuple l ps ks)              = Tuple l (trans env ps) (trans env ks)
trans' env (List l es)                  = List l (trans env es)
trans' env (ListComp l e c)             = ListComp l (trans env1 e) (trans env1 c)
  where env1                            = extscope (bound c) env
trans' env (Dict l as)                  = Dict l (trans env as)
trans' env (DictComp l a c)             = DictComp l (trans env1 a) (trans env1 c)
  where env1                            = extscope (bound c) env
trans' env (Set l es)                   = Set l (trans env es)
trans' env (SetComp l e c)              = SetComp l (trans env1 e) (trans env1 c)
  where env1                            = extscope (bound c) env
trans' env (Paren l e)                  = Paren l (trans env e)
trans' env e                            = e

instance Transform Target where
    trans env (TaVar l n)               = TaVar l n
    trans env (TaTuple l ps)            = TaTuple l (trans env ps)
    trans env (TaIndex l e ix)          = TaIndex l (trans env e) (trans env ix)
    trans env (TaSlice l e sl)          = TaSlice l (trans env e) (trans env sl)
    trans env (TaDot l e n)             = TaDot l (trans env e) n
    trans env (TaParen l t)             = TaParen l (trans env t)

instance Transform Exception where
    trans env (Exception e mbe)         = Exception (trans env e) (trans env mbe)

instance Transform Branch where
    trans env (Branch e ss)             = Branch (trans env e) (trans env ss)

instance Transform Handler where
    trans env (Handler ex b)            = Handler ex (trans env1 b)
      where env1                        = extscope (bound ex) env

instance Transform PosPar where
    trans env (PosPar n t e p)          = PosPar n t (trans env e) (trans env p)
    trans env p                         = p
    
instance Transform KwdPar where
    trans env (KwdPar n t e k)          = KwdPar n t (trans env e) (trans env k)
    trans env k                         = k
    
instance Transform PosArg where
    trans env (PosArg e p)              = PosArg (trans env e) (trans env p)
    trans env (PosStar e)               = PosStar (trans env e)
    trans env PosNil                    = PosNil
    
instance Transform KwdArg where
    trans env (KwdArg n e k)            = KwdArg n (trans env e) (trans env k)
    trans env (KwdStar e)               = KwdStar (trans env e)
    trans env KwdNil                    = KwdNil
    
instance Transform OpArg where
    trans env (OpArg op e)              = OpArg op (trans env e)

instance Transform Comp where
    trans env (CompFor l p e c)         = CompFor l p (trans env e) (trans env c)
    trans env (CompIf l e c)            = CompIf l (trans env e) (trans env c)
    trans env NoComp                    = NoComp

instance Transform WithItem where
    trans env (WithItem e p)            = WithItem (trans env e) p

instance Transform Elem where
    trans env (Elem e)                  = Elem (trans env e)
    trans env (Star e)                  = Star (trans env e)

instance Transform Assoc where
    trans env (Assoc e1 e2)             = Assoc (trans env e1) (trans env e2)
    trans env (StarStar e)              = StarStar (trans env e)
  
instance Transform Sliz where
    trans env (Sliz l e1 e2 e3)         = Sliz l (trans env e1) (trans env e2) (trans env e3)
