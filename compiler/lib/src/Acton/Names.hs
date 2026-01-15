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

{-# LANGUAGE FlexibleInstances #-}
module Acton.Names where

import Utils
import Acton.Syntax
import Acton.Builtin
import Debug.Trace


isWitness (Internal Witness _ _)    = True
isWitness _                         = False

isInternal (Internal _ _ _)         = True
isInternal _                        = False

isUnboxed (Internal BoxPass _ _)    = True
isUnboxed _                         = False


self                                = Name NoLoc "self"

localName n                         = Derived n suffixLocal
newactName n                        = Derived n suffixNewact

selfKW'                             = Internal Witness "self" 0
thisKW'                             = Internal Witness "this" 0

g_self                              = globalName "self"

g_act                               = globalName "act"
g_skip                              = globalName "skip"

altInit                             = globalName "init"

suffixLocal                         = globalName "local"
suffixNewact                        = globalName "newact"
suffixClass                         = globalName "class"
suffixMethods                       = globalName "methods"
suffixNew                           = globalName "new"
suffixWitness                       = globalName "witness"

paramNames                          = globalNames ""
pNames                              = globalNames "p"
xNames                              = globalNames "x"
yNames                              = globalNames "y"
tmpNames                            = globalNames "tmp"

attrKW                              = globalName "kw"


deriveQ (NoQ n)                     = n
deriveQ (QName (ModName m) n)       = deriveMod n m
deriveQ (GName m n)
  | m == mBuiltin                   = n
deriveQ (GName (ModName m) n)       = deriveMod n m

deriveMod n0 []                     = n0
deriveMod n0 (n:m)                  = deriveMod (Derived n0 n) m

deriveT (TVar _ v)                  = tvname v
deriveT (TCon _ c)                  = deriveQ (tcname c)

witAttr qn                          = Internal Witness (nstr $ deriveQ qn) 0

extensionName [] c                  = Derived (globalName "ext") (deriveQ $ tcname c)
extensionName (p:_) c
  | length ts == length vs          = n0
  | otherwise                       = foldl Derived n0 (map deriveT ts)
  where ts                          = tcargs c
        vs                          = [ v | TVar _ v <- ts ]
        n0                          = Derived (deriveQ $ tcname p) (deriveQ $ tcname c)


-- Mutually recursive groups -------

declnames (Extension{} : ds)        = declnames ds
declnames (d : ds)                  = dname d : declnames ds
declnames []                        = []

dname' (Extension _ _ c us _ _)     = extensionName us c
dname' d                            = dname d

splitDeclGroup []                   = []
splitDeclGroup (d:ds)               = join $ split (free d) [d] ds
  where split vs ds0 []             = [reverse ds0]
        split vs ds0 (d:ds)
          | any (`elem` ws) vs      = split (free d++vs) (d:ds0) ds
          | otherwise               = reverse ds0 : split (free d) [d] ds
          where ws                  = declnames (d:ds)
        join []                     = []
        join dss
          | not $ null dss1         = concat dss1 : join dss2
          where (dss1,dss2)         = span (all tydecl) dss
                tydecl Def{}        = False
                tydecl _            = True
        join (ds:dss)               = ds : join dss


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
    datavars n (PTuple _ ps ks)     = bound ps ++ bound ks
    datavars n (PList _ ps p)       = bound ps ++ bound p
    datavars n (PParen _ p)         = bound p
    datavars n (PData _ v ixs)
      | length ixs >= n             = [v]
    datavars n _                    = []


-- Special attributes variables ----

methods b                           = [ n | Decl _ ds <- b, Def{dname=n} <- ds ]

statevars b                         = concat [ bound ps | VarAssign _ ps _ <- b ]


isHidden n@(Name _ str)             = length (takeWhile (=='_') str) == 1 || n == resumeKW || n == cleanupKW
isHidden _                          = True

notHidden                           = filter (not . isHidden)

isPrivateName                       :: Name -> Bool
isPrivateName n                     = case nstr n of
                                        ('_':_) -> True
                                        _       -> False

isPublicName                        :: Name -> Bool
isPublicName                        = not . isPrivateName


-- Free and bound names ------------

class Vars a where
    free                            :: a -> [Name]
    freeQ                           :: a -> [QName]
    bound                           :: a -> [Name]

    free x                          = free $ freeQ x
    freeQ x                         = []
    bound x                         = []

qns `diffQ` ns                      = filter f qns
  where f (NoQ n)                   = n `notElem` ns
        f _                         = True

instance Vars a => Vars [a] where
    free                            = concatMap free
    freeQ                           = concatMap freeQ
    bound                           = concatMap bound

instance Vars a => Vars (Maybe a) where
    free                            = maybe [] free
    freeQ                           = maybe [] freeQ
    bound                           = maybe [] bound

instance Vars Stmt where
    freeQ (Expr _ e)                = freeQ e
    freeQ (Assign _ ps e)           = freeQ ps ++ freeQ e
    freeQ (MutAssign _ t e)         = freeQ t ++ freeQ e
    freeQ (AugAssign _ t op e)      = freeQ t ++ freeQ e
    freeQ (Assert _ e mbe)          = freeQ e ++ freeQ mbe
    freeQ (Pass _)                  = []
    freeQ (Delete _ t)              = freeQ t
    freeQ (Return _ e)              = freeQ e
    freeQ (Raise _ e)               = freeQ e
    freeQ (Break _)                 = []
    freeQ (Continue _)              = []
    freeQ (If _ branches els)       = freeQ branches ++ freeQ els
    freeQ (While _ e b els)         = freeQ e ++ freeQ b ++ freeQ els
    freeQ (For _ p e b els)         = freeQ p ++ freeQ e ++ (freeQ b `diffQ` bound p) ++ freeQ els
    freeQ (Try _ b hs els fin)      = freeQ b ++ freeQ hs ++ freeQ els ++ freeQ fin
    freeQ (With _ items b)          = freeQ items ++ (freeQ b `diffQ` bound items)
    freeQ (Data _ p b)              = freeQ p ++ freeQ b
    freeQ (VarAssign _ ps e)        = freeQ ps ++ freeQ e
    freeQ (After _ e e')            = freeQ e ++ freeQ e'
    freeQ (Decl _ ds)               = freeQ ds
    freeQ (Signature _ ns t d)      = freeQ t

    bound (Assign _ ps _)           = bound ps
    bound (VarAssign _ ps e)        = bound ps
    bound (AugAssign _ t _ e)       = free t
    bound (Decl _ ds)               = bound ds
    bound (Signature _ ns t d)      = ns
    bound (If _ bs els)             = bound bs ++ bound els
    bound (While _ _ b els)         = bound b ++ bound els
    bound (With _ items b)          = bound b
    bound _                         = []


assigned stmts                      = concatMap assig stmts
  where assig (While _ e b els)     = assigned b ++ assigned els
        assig (For _ p e b els)     = assigned b ++ assigned els ++ bound p
        assig (With _ items b)      = assigned b ++ bound items
        assig (Try _ b hs els fin)  = assigned b ++ concat [ bound ex ++ assigned b | Handler ex b <- hs ] ++ assigned els ++ assigned fin
        assig (If _ bs els)         = concat [ assigned b | Branch _ b <- bs ] ++ assigned els
        assig (Assign _ ps _)       = bound ps
        assig s                     = bound s


instance Vars Decl where
    freeQ (Def _ n q ps ks t b d fx _)
                                    = (freeQ ps ++ freeQ ks ++ freeQ b ++ freeQ fx) `diffQ` (n : bound q ++ bound ps ++ bound ks ++ assigned b)
    freeQ (Actor _ n q ps ks b _)   = (freeQ ps ++ freeQ ks ++ freeQ b) `diffQ` (n : self : bound q ++ bound ps ++ bound ks ++ assigned b)
    freeQ (Class _ n q cs b _)      = (freeQ cs ++ freeQ b) `diffQ` (n : bound q ++ assigned b)
    freeQ (Protocol _ n q ps b _)   = (freeQ ps ++ freeQ b) `diffQ` (n : bound q ++ assigned b)
    freeQ (Extension _ q c ps b _)  = (freeQ c ++ freeQ ps ++ freeQ b) `diffQ` (bound q ++ assigned b)

    bound (Def _ n _ _ _ _ _ _ _ _) = [n]
    bound (Actor _ n _ _ _ _ _)     = [n]
    bound (Class _ n _ _ _ _)       = [n]
    bound (Protocol _ n _ _ _ _)    = [n]
    bound (Extension _ _ _ _ _ _)   = []

instance Vars Branch where
    freeQ (Branch e ss)             = freeQ e ++ freeQ ss
    bound (Branch e ss)             = bound ss

instance Vars Handler where
    freeQ (Handler ex ss)           = freeQ ex ++ (freeQ ss `diffQ` bound ex)
    bound (Handler ex ss)           = bound ss ++ bound ex

instance Vars Expr where
    freeQ (Var _ n)                 = [n]
    freeQ (Int _ _ str)             = []
    freeQ (Float _ _ str)           = []
    freeQ (Imaginary _ _ str)       = []
    freeQ (Bool _ v)                = []
    freeQ (None _)                  = []
    freeQ (NotImplemented _)        = []
    freeQ (Ellipsis _)              = []
    freeQ (Strings _ ss)            = []
    freeQ (BStrings _ ss)           = []
    freeQ (Call _ e ps ks)          = freeQ e ++ freeQ ps ++ freeQ ks
    freeQ (TApp _ e ts)             = freeQ e ++ freeQ ts
    freeQ (Async _ e)               = freeQ e
    freeQ (Await _ e)               = freeQ e
    freeQ (Index _ e ix)            = freeQ e ++ freeQ ix
    freeQ (Slice _ e sl)            = freeQ e ++ freeQ sl
    freeQ (Cond _ e1 e e2)          = freeQ [e1,e,e2]
    freeQ (IsInstance _ e c)        = freeQ e ++ freeQ c
    freeQ (BinOp _ e1 o e2)         = freeQ [e1,e2]
    freeQ (CompOp _ e ops)          = freeQ e ++ freeQ ops
    freeQ (UnOp _ o e)              = freeQ e
    freeQ (Dot _ e n)               = freeQ e
    freeQ (Rest _ e n)              = freeQ e
    freeQ (DotI _ e i)              = freeQ e
    freeQ (RestI _ e i)             = freeQ e
    freeQ (Lambda _ ps ks e fx)     = freeQ ps ++ freeQ ks ++ (freeQ e `diffQ` (bound ps ++ bound ks))
    freeQ (Yield _ e)               = freeQ e
    freeQ (YieldFrom _ e)           = freeQ e
    freeQ (Tuple _ ps ks)           = freeQ ps ++ freeQ ks
    freeQ (List _ es)               = freeQ es
    freeQ (ListComp _ e co)         = (freeQ e `diffQ` bound co) ++ freeQ co
    freeQ (Dict _ es)               = freeQ es
    freeQ (DictComp _ e co)         = (freeQ e `diffQ` bound co) ++ freeQ co
    freeQ (Set _ es)                = freeQ es
    freeQ (SetComp _ e co)          = (freeQ e `diffQ` bound co) ++ freeQ co
    freeQ (Paren _ e)               = freeQ e
    freeQ (UnBox t e)               = freeQ e
    freeQ (Box t e)                 = freeQ e

instance Vars Name where
    free n                          = [n]

instance Vars ModName where
    free (ModName (n:ns))           = [n]

instance Vars QName where
    free (QName m n)                = free m
    free (NoQ n)                    = free n
    free (GName m n)                = free m
    
    freeQ n                         = [n]

instance Vars Except where
    freeQ (ExceptAll _)             = []
    freeQ (Except _ x)              = freeQ x
    freeQ (ExceptAs _ x n)          = freeQ x

    bound (ExceptAll _)             = []
    bound (Except _ x)              = []
    bound (ExceptAs _ x n)          = [n]

instance Vars PosPar where
    freeQ (PosPar n t e p)          = freeQ t ++ freeQ e ++ freeQ p
    freeQ (PosSTAR n t)             = freeQ t
    freeQ PosNIL                    = []

    bound (PosPar n t e p)          = n : bound p
    bound (PosSTAR n t)             = [n]
    bound PosNIL                    = []

instance Vars KwdPar where
    freeQ (KwdPar n t e k)          = freeQ t ++ freeQ e ++ freeQ k
    freeQ (KwdSTAR n t)             = freeQ t
    freeQ KwdNIL                    = []

    bound (KwdPar n t e k)          = n : bound k
    bound (KwdSTAR n t)             = [n]
    bound KwdNIL                    = []

instance Vars (PosPar,KwdPar) where
    freeQ (ppar,kpar)               = freeQ ppar ++ freeQ kpar

    bound (ppar,kpar)               = bound ppar ++ bound kpar

instance Vars Elem where
    freeQ (Elem e)                  = freeQ e
    freeQ (Star e)                  = freeQ e

    bound (Elem p)                  = bound p
    bound (Star p)                  = bound p

instance Vars Assoc where
    freeQ (Assoc k v)               = freeQ k ++ freeQ v
    freeQ (StarStar e)              = freeQ e

instance Vars WithItem where
    freeQ (WithItem e p)            = freeQ e ++ freeQ p

    bound (WithItem e p)            = bound p

instance Vars PosArg where
    freeQ (PosArg e p)              = freeQ e ++ freeQ p
    freeQ (PosStar e)               = freeQ e
    freeQ PosNil                    = []

instance Vars KwdArg where
    freeQ (KwdArg n e k)            = freeQ e ++ freeQ k
    freeQ (KwdStar e)               = freeQ e
    freeQ KwdNil                    = []

instance Vars OpArg where
    freeQ (OpArg o e)               = freeQ e

instance Vars Sliz where
    freeQ (Sliz _ e1 e2 e3)         = freeQ e1 ++ freeQ e2 ++ freeQ e3

instance Vars Comp where
    freeQ (CompFor _ pat e c)       = (freeQ e ++ freeQ c) `diffQ` bound pat
    freeQ (CompIf _ e c)            = freeQ e ++ freeQ c
    freeQ NoComp                    = []

    bound (CompFor _ pat e c)       = bound pat ++ bound c
    bound (CompIf _ e c)            = bound c
    bound NoComp                    = []

instance Vars PosPat where
    freeQ (PosPat p ps)             = freeQ p ++ freeQ ps
    freeQ (PosPatStar p)            = freeQ p
    freeQ PosPatNil                 = []

    bound (PosPat p ps)             = bound p ++ bound ps
    bound (PosPatStar p)            = bound p
    bound PosPatNil                 = []

instance Vars KwdPat where
    freeQ (KwdPat n p ps)           = freeQ p ++ freeQ ps
    freeQ (KwdPatStar p)            = freeQ p
    freeQ KwdPatNil                 = []

    bound (KwdPat n p ps)           = bound p ++ bound ps
    bound (KwdPatStar p)            = bound p
    bound KwdPatNil                 = []

instance Vars Pattern where
    freeQ (PWild _ _)               = []
    freeQ (PVar _ n a)              = []
    freeQ (PTuple _ ps ks)          = freeQ ps ++ freeQ ks
    freeQ (PList _ ps p)            = freeQ ps ++ freeQ p
    freeQ (PParen _ p)              = freeQ p
    freeQ (PData _ n ixs)           = freeQ ixs

    bound (PWild _ _)               = []
    bound (PVar _ n _)              = [n]
    bound (PTuple _ ps ks)          = bound ps ++ bound ks
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
    freeQ (TSchema _ q t)           = freeQ q ++ freeQ t

instance Vars TVar where
    freeQ (TV k v)                  = []

instance Vars TUni where
    freeQ (UV k v)                  = []

instance Vars TCon where
    freeQ (TC n ts)                 = freeQ n ++ freeQ ts

instance Vars QBind where
    freeQ (QBind v cs)              = freeQ cs

instance Vars Type where
    freeQ (TVar _ v)                = freeQ v
    freeQ (TUni _ u)                = freeQ u
    freeQ (TFun _ es p k t)         = freeQ es ++ freeQ p ++ freeQ k ++ freeQ t
    freeQ (TTuple _ p k)            = freeQ p ++ freeQ k
    freeQ (TOpt _ t)                = freeQ t
    freeQ (TCon  _ c)               = freeQ c
    freeQ (TRow _ _ _ t r)          = freeQ t ++ freeQ r
    freeQ (TStar _ _ r)             = freeQ r
    freeQ _                         = []
