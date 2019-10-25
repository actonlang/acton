{-# LANGUAGE FlexibleInstances #-}
module Acton.Names where

import Utils
import Acton.Syntax
import Debug.Trace


self                                = Name NoLoc "self"

declnames (Extension{} : ds)        = declnames ds
declnames (Signature _ ns _ _: ds)  = ns ++ declnames ds
declnames (d : ds)                  = dname d : declnames ds
declnames []                        = []

splitDeclGroup []                   = []
splitDeclGroup (d:ds)               = split (free d) [d] ds
  where split vs ds0 []             = [reverse ds0]
        split vs ds0 (d:ds)
          | any (`elem` ws) vs      = split (free d++vs) (d:ds0) ds
          | otherwise               = reverse ds0 : split (free d) [d] ds
          where ws                  = declnames (d:ds)


-- Control flow --------------------

class Fallsthru a where
    fallsthru                       :: a -> Bool        -- Statement doesn't break control flow
    
instance Fallsthru a => Fallsthru [a] where
    fallsthru                       = all fallsthru
    
instance Fallsthru Stmt where
    fallsthru Return{}              = False
    fallsthru Raise{}               = False
    fallsthru Break{}               = False
    fallsthru Continue{}            = False
    fallsthru (While _ e b els)     = fallsthru b || fallsthru els
    fallsthru (For _ p e b els)     = fallsthru b || fallsthru els
    fallsthru (With _ items b)      = fallsthru b
    fallsthru (Try _ b hs els fin)  = (fallsthru (b++els) || any fallsthru hs) && fallsthru fin
    fallsthru (If _ bs els)         = any fallsthru bs || fallsthru els
    fallsthru _                     = True
    
instance Fallsthru Branch where
    fallsthru (Branch e ss)         = fallsthru ss

instance Fallsthru Handler where
    fallsthru (Handler ex ss)       = fallsthru ss


-- Data variables ------------------

class DataVars a where
    datavars                        :: Int -> a -> [Name]      -- Variables in any lhs data pattern, with minimum arity

instance DataVars a => DataVars [a] where
    datavars n                      = concatMap (datavars n)

instance DataVars Stmt where
    datavars n (Assign _ ps _)      = datavars n ps
    datavars n (Data _ p b)         = maybe [] (datavars n) p
    datavars n (While _ e b els)    = datavars n b ++ datavars n els
    datavars n (For _ p e b els)    = datavars n b ++ datavars n els
    datavars n (If _ bs els)        = concatMap (datavars n) bs ++ datavars n els
    datavars n _                    = []

instance DataVars Branch where
    datavars n (Branch e ss)        = datavars n ss

instance DataVars Pattern where
    datavars n (PTuple _ ps p)      = bound ps ++ bound p
    datavars n (PList _ ps p)       = bound ps ++ bound p
    datavars n (PParen _ p)         = bound p
    datavars n (PData _ v ixs)
      | length ixs >= n             = [v]
    datavars n _                    = []


-- State variables -----------------

statedefs b                         = concat [ bound ps | VarAssign _ ps _ <- b ]


-- Free and bound names ------------

class Vars a where
    free                            :: a -> [Name]
    bound                           :: a -> [Name]

    free x                          = []
    bound x                         = []

instance Vars a => Vars [a] where
    free                            = concatMap free
    bound                           = concatMap bound

instance Vars a => Vars (Maybe a) where
    free                            = maybe [] free
    bound                           = maybe [] bound

instance Vars Stmt where
    free (Expr _ e)                 = free e
    free (Assign _ ps e)            = free ps ++ free e
    free (AugAssign _ p op e)       = free p ++ bound p ++ free e
    free (Assert _ es)              = free es
    free (Pass _)                   = []
    free (Delete _ p)               = free p ++ bound p
    free (Return _ e)               = free e
    free (Raise _ e)                = free e
    free (Break _)                  = []
    free (Continue _)               = []
    free (If _ branches els)        = free branches ++ free els
    free (While _ e b els)          = free e ++ free b ++ free els
    free (For _ p e b els)          = free p ++ free e ++ (free b \\ bound p) ++ free els
    free (Try _ b hs els fin)       = free b ++ free hs ++ free els ++ free fin
    free (With _ items b)           = free items ++ (free b \\ bound items)
    free (Data _ p b)               = free p ++ free b
    free (VarAssign _ ps e)         = free ps ++ free e
    free (Decl _ ds)                = free ds

    bound (Assign _ ps _)           = bound ps
    bound (VarAssign _ ps e)        = bound ps
    bound (Data _ p b)              = bound p ++ (filter istemp $ bound b)
    bound (While _ e b els)         = bound b ++ bound els
    bound (For _ p e b els)         = bound b ++ bound els ++ bound p
    bound (With _ items b)          = bound b ++ bound items
    bound (Try _ b hs els fin)      = bound b ++ concatMap bound hs ++ bound els ++ bound fin
    bound (If _ bs els)             = concatMap bound bs ++ bound els
    bound (Delete _ p)              = bound p
    bound (Decl _ ds)               = bound ds
    bound _                         = []

instance Vars Decl where
    free (Def _ n q ps ks ann b md) = (free ps ++ free ks ++ free b) \\ (n : bound ps ++ bound ks ++ bound b)
    free (Actor _ n q ps ks ann b)  = (free ps ++ free ks ++ free b) \\ (n : self : bound ps ++ bound ks ++ bound b)
    free (Class _ n q cs b)         = (free cs ++ free b) \\ (n : bound b)
    free (Protocol _ n q cs b)      = (free cs ++ free b) \\ (n : bound b)
    free (Extension _ n q cs b)     = (free n ++ free cs ++ free b) \\ bound b
    free (Signature _ ns t dec)     = free t

    bound (Def _ n _ _ _ _ _ _)     = [n]
    bound (Actor _ n _ _ _ _ _)     = [n]
    bound (Class _ n _ _ _)         = [n]
    bound (Protocol _ n _ _ _)      = [n]
    bound (Extension _ n _ _ _)     = []
    bound (Signature _ ns t dec)    = []

instance Vars Branch where
    free (Branch e ss)              = free e ++ free ss
    bound (Branch e ss)             = bound ss
    
instance Vars Handler where
    free (Handler ex ss)            = free ex ++ (free ss \\ bound ex)
    bound (Handler ex ss)           = bound ss ++ bound ex

instance Vars Expr where
    free (Var _ n)                  = [n]
    free (Int _ _ str)              = []
    free (Float _ _ str)            = []
    free (Imaginary _ _ str)        = []
    free (Bool _ v)                 = []
    free (None _)                   = []
    free (NotImplemented _)         = []
    free (Ellipsis _)               = []
    free (Strings _ ss)             = []
    free (BStrings _ ss)            = []
    free (UStrings _ ss)            = []
    free (Call _ e ps ks)           = free e ++ free ps ++ free ks
    free (Await _ e)                = free e
    free (Index _ e ix)             = free e ++ free ix
    free (Slice _ e sl)             = free e ++ free sl
    free (Cond _ e1 e e2)           = free [e1,e,e2]
    free (BinOp _ e1 o e2)          = free [e1,e2]
    free (CompOp _ e ops)           = free e ++ free ops
    free (UnOp _ o e)               = free e
    free (Dot _ e n)                = free e
    free (DotI _ e i)               = free e
    free (Lambda _ ps ks e)         = free ps ++ free ks ++ (free e \\ (bound ps ++ bound ks))
    free (Yield _ e)                = free e
    free (YieldFrom _ e)            = free e
    free (Tuple _ es)               = free es
    free (TupleComp _ e co)         = (free e \\ bound co) ++ free co
    free (Record _ fs)              = free fs
    free (RecordComp _ n e co)      = ((n : free e) \\ bound co) ++ free co
    free (List _ es)                = free es
    free (ListComp _ e co)          = (free e \\ bound co) ++ free co
    free (Dict _ es)                = free es
    free (DictComp _ e co)          = (free e \\ bound co) ++ free co
    free (Set _ es)                 = free es
    free (SetComp _ e co)           = (free e \\ bound co) ++ free co
    free (Paren _ e)                = free e

instance Vars Name where
    free n                          = [n]

instance Vars ModName where
    free (ModName (n:ns))           = [n]

instance Vars QName where
    free (QName m n)                = free m
    free (NoQual n)                 = free n

instance Vars Exception where
    free (Exception e1 e2)          = free e1 ++ free e2

instance Vars Except where
    free (ExceptAll _)              = []
    free (Except _ x)               = free x
    free (ExceptAs _ x n)           = free x

    bound (ExceptAll _)             = []
    bound (Except _ x)              = []
    bound (ExceptAs _ x n)          = [n]

instance Vars PosPar where
    free (PosPar n t e p)           = free e ++ free p
    free (PosSTAR n t)              = []
    free PosNIL                     = []
    
    bound (PosPar n t e p)          = n : bound p
    bound (PosSTAR n t)             = [n]
    bound PosNIL                    = []

instance Vars KwdPar where
    free (KwdPar n t e k)           = free e ++ free k
    free (KwdSTAR n t)              = []
    free KwdNIL                     = []
    
    bound (KwdPar n t e k)          = n : bound k
    bound (KwdSTAR n t)             = [n]
    bound KwdNIL                    = []

instance Vars (PosPar,KwdPar) where
    free (ppar,kpar)                = free ppar ++ free kpar
    
    bound (ppar,kpar)               = bound ppar ++ bound kpar

instance Vars Elem where
    free (Elem e)                   = free e
    free (Star e)                   = free e

    bound (Elem p)                  = bound p
    bound (Star p)                  = bound p
    
instance Vars Assoc where
    free (Assoc k v)                = free k ++ free v
    free (StarStar e)               = free e

instance Vars Field where
    free (Field n e)                = free e
    free (StarStarField e)          = free e
    
    bound (Field n e)               = [n]
    bound (StarStarField e)         = []

instance Vars WithItem where
    free (WithItem e p)             = free e ++ free p

    bound (WithItem e p)            = bound p
    
instance Vars PosArg where
    free (PosArg e p)               = free e ++ free p
    free (PosStar e)                = free e
    free PosNil                     = []
    
instance Vars KwdArg where
    free (KwdArg n e k)             = free e ++ free k
    free (KwdStar e)                = free e
    free KwdNil                     = []
    
instance Vars OpArg where
    free (OpArg o e)                = free e

instance Vars Slice where
    free (Sliz _ e1 e2 e3)          = free e1 ++ free e2 ++ free e3

instance Vars Comp where
    free (CompFor _ pat e c)        = (free e ++ free c) \\ bound pat
    free (CompIf _ e c)             = free e ++ free c
    free NoComp                     = []

    bound (CompFor _ pat e c)       = bound pat ++ bound c
    bound (CompIf _ e c)            = bound c
    bound NoComp                    = []

instance Vars PosPat where
    free (PosPat p ps)              = free p ++ free ps
    free (PosPatStar p)             = free p
    free PosPatNil                  = []

    bound (PosPat p ps)             = bound p ++ bound ps
    bound (PosPatStar p)            = bound p
    bound PosPatNil                 = []
    
instance Vars KwdPat where
    free (KwdPat n p ps)            = free p ++ free ps
    free (KwdPatStar p)             = free p
    free KwdPatNil                  = []

    bound (KwdPat n p ps)           = bound p ++ bound ps
    bound (KwdPatStar p)            = bound p
    bound KwdPatNil                 = []
    
instance Vars Pattern where
    free (PVar _ n a)               = []
    free (PIndex _ e ix)            = free e ++ free ix
    free (PSlice _ e sl)            = free e ++ free sl
    free (PDot _ e n)               = free e
--    free (PRecord _ ps)             = free ps
--    free (PTuple _ ps)              = free ps
    free (PTuple _ ps p)            = free ps ++ free p
    free (PList _ ps p)             = free ps ++ free p
    free (PParen _ p)               = free p
    free (PData _ n ixs)            = free ixs

    bound (PVar _ n a)              = [n]
    bound (PIndex _ e ix)           = []
    bound (PSlice _ e sl)           = []
    bound (PDot _ e n)              = []
--    bound (PRecord _ ps)            = bound ps
--    bound (PTuple _ ps)             = bound ps
    bound (PTuple _ ps p)           = bound ps ++ bound p
    bound (PList _ ps p)            = bound ps ++ bound p
    bound (PParen _ p)              = bound p
    bound (PData _ n ixs)           = [n]

instance Vars ModuleItem where
    bound (ModuleItem qn Nothing)   = free qn
    bound (ModuleItem qn (Just n))  = free n

instance Vars ImportItem where
    free (ImportItem n1 as)         = []
    
    bound (ImportItem n Nothing)    = free n
    bound (ImportItem n (Just as))  = free as

instance Vars ModRef where 
    bound (ModRef (0, n))           = free n
    bound _                         = []

instance Vars TSchema where
    free (TSchema _ q t)            = free q ++ free t

instance Vars TVar where
    free (TV v)                     = []

instance Vars TCon where
    free (TC n ts)                  = free n ++ free ts

instance Vars TBind where
    free (TBind v cs)               = free cs

instance Vars Type where
    free (TVar _ v)                 = free v
    free (TFun _ es p k t)          = free es ++ free p ++ free k ++ free t
    free (TTuple _ p)               = free p
    free (TRecord _ k)              = free k
    free (TOpt _ t)                 = free t
    free (TCon  _ c)                = free c
    free (TAt  _ c)                 = free c
    free (TRow _ _ t r)             = free t ++ free r
    free _                          = []


-----------------

-- Names free in embedded lambda
-- Called during translation to ensure that lambdas contain no state variables
-- Will become defunct once lambda-lifting works directly on Acton code

lambdafree s                        = undefined
