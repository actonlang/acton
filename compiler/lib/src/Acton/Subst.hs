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
module Acton.Subst where

import Control.Monad.State.Strict
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Acton.Syntax
import Acton.Names
import Acton.Builtin
import Acton.Printer
import Acton.TypeM
import Utils


addSelf                                 :: Type -> Maybe Deco -> Type
addSelf (TFun l x p k t) (Just NoDec)   = TFun l x (posRow tSelf p) k t
addSelf t _                             = t

dropSelf                                :: Type -> Deco -> Type
dropSelf (TFun l x p k t) NoDec
  | TRow _ _ _ _ p' <- p                = TFun l x p' k t
  | TRow _ _ _ _ k' <- k                = TFun l x p k' t
dropSelf t _                            = t

selfType                                :: PosPar -> KwdPar -> Deco -> Type
selfType p k NoDec
  | TRow _ _ _ t _ <- prowOf p          = t
  | TRow _ _ _ t _ <- krowOf k          = t
selfType _ _ _                          = tSelf

qualbound q                         = [ v | QBind v ps <- q, not $ null ps ]


-- VFree ----------------------------------------------------------------------------------------------

class VFree a where
    vfree                           :: a -> [TVar]

instance VFree a => VFree [a] where
    vfree                           = concatMap vfree

instance VFree a => VFree (Maybe a) where
    vfree                           = maybe [] vfree

instance VFree a => VFree (Name,a) where
    vfree (n, t)                    = vfree t

instance VFree Constraint where
    vfree (Cast info q t1 t2)       = (vfree q ++ vfree t1 ++ vfree t2) \\ qscope q
    vfree (Sub info w q t1 t2)      = (vfree q ++ vfree t1 ++ vfree t2) \\ qscope q
    vfree (Proto info w q t p)      = (vfree q ++ vfree t ++ vfree p) \\ qscope q
    vfree (Sel info w q t1 n t2)    = (vfree q ++ vfree t1 ++ vfree t2) \\ qscope q
    vfree (Mut info q t1 n t2)      = (vfree q ++ vfree t1 ++ vfree t2) \\ qscope q
    vfree (Seal info q t)           = (vfree q ++ vfree t) \\ qscope q
    vfree (Imply info w q cs)       = (vfree q ++ vfree cs) \\ qbound q

instance VFree Type where
    vfree (TVar _ v)                = [v]
    vfree (TCon _ c)                = vfree c
    vfree (TFun _ fx p k t)         = vfree fx ++ vfree p ++ vfree k ++ vfree t
    vfree (TTuple _ p k)            = vfree p ++ vfree k
    vfree (TOpt _ t)                = vfree t
    vfree (TRow _ k n t r)          = vfree t ++ vfree r
    vfree (TStar _ k r)             = vfree r
    vfree _                         = []

instance VFree TCon where
    vfree (TC n ts)                 = vfree ts

instance VFree QBind where
    vfree (QBind v cs)              = vfree cs

instance VFree Quant where
    vfree (Quant v ps)              = vfree ps

instance VFree WTCon where
    vfree (wpath, p)                = vfree p

instance VFree TSchema where
    vfree (TSchema _ q t)           = (vfree q ++ vfree t) \\ qbound q

instance VFree PosPar where
    vfree (PosPar n t e p)          = vfree t ++ vfree p
    vfree (PosSTAR n t)             = vfree t
    vfree PosNIL                    = []

instance VFree KwdPar where
    vfree (KwdPar n t e k)          = vfree t ++ vfree k
    vfree (KwdSTAR n t)             = vfree t
    vfree KwdNIL                    = []

instance VFree Decl where
    vfree (Def _ n q p k a ss de fx doc) = vfree q ++ vfree p ++ vfree k ++ vfree a ++ vfree ss ++ vfree fx
    vfree (Actor _ n q p k ss doc)       = vfree q ++ vfree p ++ vfree k ++ vfree ss
    vfree (Class _ n q bs ss doc)        = vfree q ++ vfree bs ++ vfree ss
    vfree (Protocol _ n q bs ss doc)     = vfree q ++ vfree bs ++ vfree ss
    vfree (Extension _ q c bs ss doc)    = vfree q ++ vfree c ++ vfree bs ++ vfree ss

instance VFree Stmt where
    vfree (Expr _ e)               = vfree e
    vfree (Assign _ ps e)          = vfree ps ++ vfree e
    vfree (MutAssign _ t e)        = vfree t ++ vfree e
    vfree (AugAssign _ t op e)     = vfree t ++ vfree e
    vfree (Assert _ e mbe)         = vfree e ++ vfree mbe
    vfree (Delete _ t)             = vfree t
    vfree (Return _ mbe)           = vfree mbe
    vfree (Raise _ e)              = vfree e
    vfree (If _ bs els)            = vfree bs ++ vfree els
    vfree (While _ e b els)        = vfree e ++ vfree b ++ vfree els
    vfree (For _ p e b els)        = vfree p ++ vfree e ++ vfree b ++ vfree els
    vfree (Try _ b hs els fin)     = vfree b ++ vfree hs ++ vfree els ++ vfree fin
    vfree (With _ is b)            = vfree is ++ vfree b
    vfree (VarAssign _ ps e)       = vfree ps ++ vfree e
    vfree (After _ e e')           = vfree e ++ vfree e'
    vfree (Decl _ ds)              = vfree ds
    vfree (Signature _ ns tsc d)   = vfree tsc
    vfree s                        = []

instance VFree Branch where
    vfree (Branch e b)              = vfree e ++ vfree b

instance VFree Handler where
    vfree (Handler ex b)            = vfree b

instance VFree WithItem where
    vfree (WithItem e p)            = vfree e ++ vfree p

instance VFree Expr where
    vfree (Call l e p k)            = vfree e ++ vfree p ++ vfree k
    vfree (TApp l e ts)             = vfree e ++ vfree ts
    vfree (Async l e)               = vfree e
    vfree (Await l e)               = vfree e
    vfree (Index l e ix)            = vfree e ++ vfree ix
    vfree (Slice l e sl)            = vfree e ++ vfree sl
    vfree (Cond l e1 cond e2)       = vfree e1 ++ vfree cond ++ vfree e2
    vfree (IsInstance l e c)        = vfree e
    vfree (BinOp l e1 op e2)        = vfree e1 ++ vfree e2
    vfree (CompOp l e ops)          = vfree e ++ vfree ops
    vfree (UnOp l op e)             = vfree e
    vfree (Dot l e n)               = vfree e
    vfree (Rest l e n)              = vfree e
    vfree (DotI l e i)              = vfree e
    vfree (RestI l e i)             = vfree e
    vfree (Lambda l p k e fx)       = vfree p ++ vfree k ++ vfree e ++ vfree fx
    vfree (Yield l e)               = vfree e
    vfree (YieldFrom l e)           = vfree e
    vfree (Tuple l p k)             = vfree p ++ vfree k
    vfree (List l es)               = vfree es
    vfree (ListComp l e c)          = vfree e ++ vfree c
    vfree (Dict l as)               = vfree as
    vfree (DictComp l a c)          = vfree a ++ vfree c
    vfree (Set l es)                = vfree es
    vfree (SetComp l e c)           = vfree e ++ vfree c
    vfree (Paren l e)               = vfree e
    vfree e                         = []

instance VFree Pattern where
    vfree (PWild _ t)               = vfree t
    vfree (PVar _ n t)              = vfree t
    vfree (PParen _ p)              = vfree p
    vfree (PTuple _ p k)            = vfree p ++ vfree k
    vfree (PList _ ps p)            = vfree ps ++ vfree p

instance VFree PosPat where
    vfree (PosPat p pp)             = vfree p ++ vfree pp
    vfree (PosPatStar p)            = vfree p
    vfree PosPatNil                 = []

instance VFree KwdPat where
    vfree (KwdPat n p kp)           = vfree p ++ vfree kp
    vfree (KwdPatStar p)            = vfree p
    vfree KwdPatNil                 = []

instance VFree PosArg where
    vfree (PosArg e p)              = vfree e ++ vfree p
    vfree (PosStar e)               = vfree e
    vfree PosNil                    = []

instance VFree KwdArg where
    vfree (KwdArg n e k)            = vfree e ++ vfree k
    vfree (KwdStar e)               = vfree e
    vfree KwdNil                    = []

instance VFree Elem where
    vfree (Elem e)                  = vfree e
    vfree (Star e)                  = vfree e

instance VFree Assoc where
    vfree (Assoc k v)               = vfree k ++ vfree v
    vfree (StarStar e)              = vfree e

instance VFree Comp where
    vfree (CompFor _ p e c)         = vfree p ++ vfree e ++ vfree c
    vfree (CompIf _ e c)            = vfree e ++ vfree c
    vfree NoComp                    = []

instance VFree Sliz where
    vfree (Sliz _ e1 e2 e3)         = vfree e1 ++ vfree e2 ++ vfree e3

instance VFree OpArg where
    vfree (OpArg op e)              = vfree e


-- VSubst ---------------------------------------------------------------------------------------------

class VSubst a where
    vsubst                          :: Substitution -> a -> a

instance VSubst a => VSubst [a] where
    vsubst s                        = map $ vsubst s

instance VSubst a => VSubst (Maybe a) where
    vsubst s                        = fmap $ vsubst s

instance VSubst a => VSubst (Name,a) where
    vsubst s (n, t)                 = (n, vsubst s t)

instance VSubst Type where
    vsubst s (TVar l v)             = case lookup v s of
                                        Just t ->  t
                                        Nothing -> TVar l v
    vsubst s (TUni l u)             = TUni l u
    vsubst s (TCon l c)             = TCon l (vsubst s c)
    vsubst s (TFun l fx p k t)      = TFun l (vsubst s fx) (vsubst s p) (vsubst s k) (vsubst s t)
    vsubst s (TTuple l p k)         = TTuple l (vsubst s p) (vsubst s k)
    vsubst s (TOpt l t)             = TOpt l (vsubst s t)
    vsubst s (TRow l k n t r)       = TRow l k n (vsubst s t) (vsubst s r)
    vsubst s (TStar l k r)          = TStar l k (vsubst s r)
    vsubst s (TNone l)              = TNone l
    vsubst s (TWild l)              = TWild l
    vsubst s (TNil l k)             = TNil l k
    vsubst s (TFX l fx)             = TFX l fx

instance VSubst TCon where
    vsubst s (TC n ts)              = TC n (vsubst s ts)

instance VSubst TVar where
    vsubst s v                      = case lookup v s of
                                         Just (TVar _ v') -> v'
                                         _ -> v

instance VSubst TSchema where
    vsubst s (TSchema l q t)        = TSchema l (vsubst s' q) (vsubst s' t)
      where s'                      = quantsubst s q (vfree t)

quantsubst s [] vs                  = s
quantsubst s q vs                   = alpha ++ pruned
  where pruned                      = s `exclude` qbound q
        new                         = nub $ vfree (rng pruned)
        clash                       = new `intersect` qbound q
        alpha                       = clash `zip` map tVar (tvarSupply \\ avoid)
        avoid                       = new ++ vfree q ++ vs

instance VSubst QBind where
    vsubst s (QBind v ts)           = QBind (vsubst s v) (vsubst s ts)

instance VSubst PosPar where
    vsubst s (PosPar n t e p)       = PosPar n (vsubst s t) e (vsubst s p)
    vsubst s (PosSTAR n t)          = PosSTAR n (vsubst s t)
    vsubst s PosNIL                 = PosNIL

instance VSubst KwdPar where
    vsubst s (KwdPar n t e k)       = KwdPar n (vsubst s t) e (vsubst s k)
    vsubst s (KwdSTAR n t)          = KwdSTAR n (vsubst s t)
    vsubst s KwdNIL                 = KwdNIL

instance VSubst Decl where
    vsubst s (Def l n q p k a ss de fx doc) = Def l n (vsubst r q) (vsubst r p) (vsubst r k) (vsubst r a) (vsubst r ss) de (vsubst r fx) doc
      where r                               = quantsubst s q (vfree p ++ vfree k ++ vfree a ++ vfree ss ++ vfree fx)
    vsubst s (Actor l n q p k ss doc)       = Actor l n (vsubst r q) (vsubst r p) (vsubst r k) (vsubst r ss) doc
      where r                               = quantsubst s q (vfree p ++ vfree k ++ vfree ss)
    vsubst s (Class l n q bs ss doc)        = Class l n (vsubst s q) (vsubst s bs) (vsubst s ss) doc
      where r                               = quantsubst s q (vfree bs ++ vfree ss)
    vsubst s (Protocol l n q bs ss doc)     = Protocol l n (vsubst s q) (vsubst s bs) (vsubst s ss) doc
      where r                               = quantsubst s q (vfree bs ++ vfree ss)
    vsubst s (Extension l q c bs ss doc)    = Extension l (vsubst s q) (vsubst s c) (vsubst s bs) (vsubst s ss) doc
      where r                               = quantsubst s q (vfree c ++ vfree bs ++ vfree ss)

instance VSubst Stmt where
    vsubst s (Expr l e)             = Expr l (vsubst s e)
    vsubst s (Assign l ps e)        = Assign l (vsubst s ps) (vsubst s e)
    vsubst s (MutAssign l t e)      = MutAssign l (vsubst s t) (vsubst s e)
    vsubst s (AugAssign l t op e)   = AugAssign l (vsubst s t) op (vsubst s e)
    vsubst s (Assert l e mbe)       = Assert l (vsubst s e) (vsubst s mbe)
    vsubst s (Delete l t)           = Delete l (vsubst s t)
    vsubst s (Return l mbe)         = Return l (vsubst s mbe)
    vsubst s (Raise l e)            = Raise l (vsubst s e)
    vsubst s (If l bs els)          = If l (vsubst s bs) (vsubst s els)
    vsubst s (While l e b els)      = While l (vsubst s e) (vsubst s b) (vsubst s els)
    vsubst s (For l p e b els)      = For l (vsubst s p) (vsubst s e) (vsubst s b) (vsubst s els)
    vsubst s (Try l b hs els fin)   = Try l (vsubst s b) (vsubst s hs) (vsubst s els) (vsubst s fin)
    vsubst s (With l is b)          = With l (vsubst s is) (vsubst s b)
    vsubst s (VarAssign l ps e)     = VarAssign l (vsubst s ps) (vsubst s e)
    vsubst s (After l e e')         = After l (vsubst s e) (vsubst s e')
    vsubst s (Decl l ds)            = Decl l (vsubst s ds)
    vsubst s (Signature l ns tsc d) = Signature l ns (vsubst s tsc) d
    vsubst s stmt                   = stmt

instance VSubst Expr where
    vsubst s (Call l e p k)         = Call l (vsubst s e) (vsubst s p) (vsubst s k)
    vsubst s (TApp l e ts)          = TApp l (vsubst s e) (vsubst s ts)
    vsubst s (Async l e)            = Async l (vsubst s e)
    vsubst s (Await l e)            = Await l (vsubst s e)
    vsubst s (Index l e ix)         = Index l (vsubst s e) (vsubst s ix)
    vsubst s (Slice l e sl)         = Slice l (vsubst s e) (vsubst s sl)
    vsubst s (Cond l e1 cond e2)    = Cond l (vsubst s e1) (vsubst s cond) (vsubst s e2)
    vsubst s (IsInstance l e c)     = IsInstance l (vsubst s e) c
    vsubst s (BinOp l e1 op e2)     = BinOp l (vsubst s e1) op (vsubst s e2)
    vsubst s (CompOp l e ops)       = CompOp l (vsubst s e) (vsubst s ops)
    vsubst s (UnOp l op e)          = UnOp l op (vsubst s e)
    vsubst s (Dot l e n)            = Dot l (vsubst s e) n
    vsubst s (Rest l e n)           = Rest l (vsubst s e) n
    vsubst s (DotI l e i)           = DotI l (vsubst s e) i
    vsubst s (RestI l e i)          = RestI l (vsubst s e) i
    vsubst s (Lambda l p k e fx)    = Lambda l (vsubst s p) (vsubst s k) (vsubst s e) (vsubst s fx)
    vsubst s (Yield l e)            = Yield l (vsubst s e)
    vsubst s (YieldFrom l e)        = YieldFrom l (vsubst s e)
    vsubst s (Tuple l p k)          = Tuple l (vsubst s p) (vsubst s k)
    vsubst s (List l es)            = List l (vsubst s es)
    vsubst s (ListComp l e c)       = ListComp l (vsubst s e) (vsubst s c)
    vsubst s (Dict l as)            = Dict l (vsubst s as)
    vsubst s (DictComp l a c)       = DictComp l (vsubst s a) (vsubst s c)
    vsubst s (Set l es)             = Set l (vsubst s es)
    vsubst s (SetComp l e c)        = SetComp l (vsubst s e) (vsubst s c)
    vsubst s (Paren l e)            = Paren l (vsubst s e)
    vsubst s e                      = e

instance VSubst Branch where
    vsubst s (Branch e b)           = Branch (vsubst s e) (vsubst s b)

instance VSubst Pattern where
    vsubst s (PWild l t)            = PWild l (vsubst s t)
    vsubst s (PVar l n t)           = PVar l n (vsubst s t)
    vsubst s (PParen l p)           = PParen l (vsubst s p)
    vsubst s (PTuple l p k)         = PTuple l (vsubst s p) (vsubst s k)
    vsubst s (PList l ps p)         = PList l (vsubst s ps) (vsubst s p)
    
instance VSubst PosPat where
    vsubst s (PosPat p pp)          = PosPat (vsubst s p) (vsubst s pp)
    vsubst s (PosPatStar p)         = PosPatStar (vsubst s p)
    vsubst s PosPatNil              = PosPatNil

instance VSubst KwdPat where
    vsubst s (KwdPat n p kp)        = KwdPat n (vsubst s p) (vsubst s kp)
    vsubst s (KwdPatStar p)         = KwdPatStar (vsubst s p)
    vsubst s KwdPatNil              = KwdPatNil

instance VSubst Handler where
    vsubst s (Handler ex b)         = Handler ex (vsubst s b)

instance VSubst WithItem where
    vsubst s (WithItem e p)         = WithItem (vsubst s e) (vsubst s p)
    
instance VSubst PosArg where
    vsubst s (PosArg e p)           = PosArg (vsubst s e) (vsubst s p)
    vsubst s (PosStar e)            = PosStar (vsubst s e)
    vsubst s PosNil                 = PosNil

instance VSubst KwdArg where
    vsubst s (KwdArg n e k)         = KwdArg n (vsubst s e) (vsubst s k)
    vsubst s (KwdStar e)            = KwdStar (vsubst s e)
    vsubst s KwdNil                 = KwdNil

instance VSubst Assoc where
    vsubst s (Assoc k v)            = Assoc (vsubst s k) (vsubst s v)
    vsubst s (StarStar e)           = StarStar (vsubst s e)

instance VSubst Elem where
    vsubst s (Elem e)               = Elem (vsubst s e)
    vsubst s (Star e)               = Star (vsubst s e)

instance VSubst Comp where
    vsubst s (CompFor l p e c)      = CompFor l (vsubst s p) (vsubst s e) (vsubst s c)
    vsubst s (CompIf l e c)         = CompIf l (vsubst s e) (vsubst s c)
    vsubst s NoComp                 = NoComp

instance VSubst Sliz where
    vsubst s (Sliz l e1 e2 e3)      = Sliz l (vsubst s e1) (vsubst s e2) (vsubst s e3)

instance VSubst OpArg where
    vsubst s (OpArg op e)           = OpArg op (vsubst s e)

instance VSubst WTCon where
    vsubst s (w,u)              = (w, vsubst s u)


-- UFree ----------------------------------------------------------------------------------------------

class UFree t where
    ufree                           :: t -> [TUni]

instance UFree a => UFree (Name,a) where
    ufree (n, t)                    = ufree t

instance (UFree a, UFree b) => UFree (QName,a,b) where
    ufree (n, t, u)                 = ufree t ++ ufree u

instance UFree a => UFree [a] where
    ufree                           = concat . map ufree

instance UFree a => UFree (Maybe a) where
    ufree                           = maybe [] ufree

instance UFree Constraint where
    ufree (Cast info q t1 t2)       = ufree info ++ ufree q ++ ufree t1 ++ ufree t2
    ufree (Sub info w q t1 t2)      = ufree info ++ ufree q ++ ufree t1 ++ ufree t2
    ufree (Proto info w q t p)      = ufree info ++ ufree q ++ ufree t ++ ufree p
    ufree (Sel info w q t1 n t2)    = ufree info ++ ufree q ++ ufree t1 ++ ufree t2
    ufree (Mut info q t1 n t2)      = ufree info ++ ufree q ++ ufree t1 ++ ufree t2
    ufree (Seal info q t)           = ufree info ++ ufree q ++ ufree t
    ufree (Imply info w q cs)       = ufree info ++ ufree q ++ ufree cs

instance UFree ErrInfo where
    ufree (DfltInfo l n mbe ts)     = ufree mbe ++ ufree ts
    ufree (DeclInfo l1 l2 n t msg)  = ufree t
    ufree _                         = []
    
instance UFree TSchema where
    ufree (TSchema _ q t)           = ufree q ++ ufree t

instance UFree TVar where
    ufree v                         = []
        
instance UFree TCon where
    ufree (TC n ts)                 = ufree ts

instance UFree QBind where
    ufree (QBind v cs)              = ufree cs

instance UFree Quant where
    ufree (Quant v cs)              = ufree cs

instance UFree WTCon where
    ufree (wpath, p)                = ufree p

instance UFree Type where
    ufree (TUni _ u)                = [u]
    ufree (TVar _ v)                = []
    ufree (TCon _ c)                = ufree c
    ufree (TFun _ fx p k t)         = ufree fx ++ ufree p ++ ufree k ++ ufree t
    ufree (TTuple _ p k)            = ufree p ++ ufree k
    ufree (TOpt _ t)                = ufree t
    ufree (TNone _)                 = []
    ufree (TWild _)                 = []
    ufree (TNil _ _)                = []
    ufree (TRow _ _ _ t r)          = ufree t ++ ufree r
    ufree (TStar _ _ r)             = ufree r
    ufree (TFX l fx)                = []

instance UFree PosPar where
    ufree (PosPar n t e p)          = ufree t ++ ufree p
    ufree (PosSTAR n t)             = ufree t
    ufree PosNIL                    = []

instance UFree KwdPar where
    ufree (KwdPar n t e p)          = ufree t ++ ufree p
    ufree (KwdSTAR n t)             = ufree t
    ufree KwdNIL                    = []

instance UFree Decl where
    ufree (Def _ n q p k a ss de fx _)  = ufree q ++ ufree p ++ ufree k ++ ufree a ++ ufree ss ++ ufree fx
    ufree (Actor _ n q p k ss _)        = ufree q ++ ufree p ++ ufree k ++ ufree ss
    ufree (Class _ n q bs ss _)         = ufree q ++ ufree bs ++ ufree ss
    ufree (Protocol _ n q bs ss _)      = ufree q ++ ufree bs ++ ufree ss
    ufree (Extension _ q c bs ss _)     = ufree q ++ ufree c ++ ufree bs ++ ufree ss

instance UFree Stmt where
    ufree (Expr _ e)                = ufree e
    ufree (Assign _ ps e)           = ufree ps ++ ufree e
    ufree (MutAssign _ t e)         = ufree t ++ ufree e
    ufree (AugAssign _ t op e)      = ufree t ++ ufree e
    ufree (Assert _ e mbe)          = ufree e ++ ufree mbe
    ufree (Delete _ t)              = ufree t
    ufree (Return _ mbe)            = ufree mbe
    ufree (Raise _ e)               = ufree e
    ufree (If _ bs els)             = ufree bs ++ ufree els
    ufree (While _ e b els)         = ufree e ++ ufree b ++ ufree els
    ufree (For _ p e b els)         = ufree p ++ ufree e ++ ufree b ++ ufree els
    ufree (Try _ b hs els fin)      = ufree b ++ ufree hs ++ ufree els ++ ufree fin
    ufree (With _ is b)             = ufree is ++ ufree b
    ufree (VarAssign _ ps e)        = ufree ps ++ ufree e
    ufree (After _ e e')            = ufree e ++ ufree e'
    ufree (Decl _ ds)               = ufree ds
    ufree (Signature _ ns tsc d)    = ufree tsc
    ufree s                         = []

instance UFree Expr where
    ufree (Call l e p k)            = ufree e ++ ufree p ++ ufree k
    ufree (TApp l e ts)             = ufree e ++ ufree ts
    ufree (Async l e)               = ufree e
    ufree (Await l e)               = ufree e
    ufree (Index l e ix)            = ufree e ++ ufree ix
    ufree (Slice l e sl)            = ufree e ++ ufree sl
    ufree (Cond l e1 cond e2)       = ufree e1 ++ ufree cond ++ ufree e2
    ufree (IsInstance l e c)        = ufree e
    ufree (BinOp l e1 op e2)        = ufree e1 ++ ufree e2
    ufree (CompOp l e ops)          = ufree e ++ ufree ops
    ufree (UnOp l op e)             = ufree e
    ufree (Dot l e n)               = ufree e
    ufree (Rest l e n)              = ufree e
    ufree (DotI l e i)              = ufree e
    ufree (RestI l e i)             = ufree e
    ufree (Lambda l p k e fx)       = ufree p ++ ufree k ++ ufree e ++ ufree fx
    ufree (Yield l e)               = ufree e
    ufree (YieldFrom l e)           = ufree e
    ufree (Tuple l p k)             = ufree p ++ ufree k
    ufree (List l es)               = ufree es
    ufree (ListComp l e c)          = ufree e ++ ufree c
    ufree (Dict l as)               = ufree as
    ufree (DictComp l a c)          = ufree a ++ ufree c
    ufree (Set l es)                = ufree es
    ufree (SetComp l e c)           = ufree e ++ ufree c
    ufree (Paren l e)               = ufree e
    ufree e                         = []

instance UFree Pattern where
    ufree (PWild _ t)               = ufree t
    ufree (PVar _ n t)              = ufree t
    ufree (PParen _ p)              = ufree p
    ufree (PTuple _ p k)            = ufree p ++ ufree k
    ufree (PList _ ps p)            = ufree ps ++ ufree p

instance UFree PosPat where
    ufree (PosPat p pp)             = ufree p ++ ufree pp
    ufree (PosPatStar p)            = ufree p
    ufree PosPatNil                 = []

instance UFree KwdPat where
    ufree (KwdPat n p kp)           = ufree p ++ ufree kp
    ufree (KwdPatStar p)            = ufree p
    ufree KwdPatNil                 = []

instance UFree Branch where
    ufree (Branch e b)              = ufree e ++ ufree b

instance UFree Handler where
    ufree (Handler ex b)            = ufree b

instance UFree WithItem where
    ufree (WithItem e p)            = ufree e ++ ufree p

instance UFree PosArg where
    ufree (PosArg e p)              = ufree e ++ ufree p
    ufree (PosStar e)               = ufree e
    ufree PosNil                    = []

instance UFree KwdArg where
    ufree (KwdArg n e k)            = ufree e ++ ufree k
    ufree (KwdStar e)               = ufree e
    ufree KwdNil                    = []

instance UFree Elem where
    ufree (Elem e)                  = ufree e
    ufree (Star e)                  = ufree e

instance UFree Assoc where
    ufree (Assoc k v)               = ufree k ++ ufree v
    ufree (StarStar e)              = ufree e

instance UFree Comp where
    ufree (CompFor _ p e c)         = ufree p ++ ufree e ++ ufree c
    ufree (CompIf _ e c)            = ufree e ++ ufree c
    ufree NoComp                    = []

instance UFree Sliz where
    ufree (Sliz _ e1 e2 e3)         = ufree e1 ++ ufree e2 ++ ufree e3

instance UFree OpArg where
    ufree (OpArg op e)              = ufree e


-- USubst ---------------------------------------------------------------------------------------------

class USubst t where
    usubst                          :: t -> TypeM t

instance USubst a => USubst (Name,a) where
    usubst (n, t)                   = (,) <$> return n <*> usubst t

instance (USubst a, USubst b) => USubst (QName,a,b) where
    usubst (n, t, u)                = (,,) <$> return n <*> usubst t <*> usubst u

instance USubst a => USubst [a] where
    usubst                          = mapM usubst

instance USubst a => USubst (Maybe a) where
    usubst                          = maybe (return Nothing) (\x -> Just <$> usubst x)

instance USubst Constraint where
    usubst (Cast info q t1 t2)      = Cast <$> usubst info <*> usubst q <*> usubst t1 <*> usubst t2
    usubst (Sub info w q t1 t2)     = Sub <$> usubst info <*> return w <*> usubst q <*> usubst t1 <*> usubst t2
    usubst (Proto info w q t p)     = Proto <$> usubst info <*> return w <*> usubst q <*> usubst t <*> usubst p
    usubst (Sel info w q t1 n t2)   = Sel <$> usubst info <*> return w <*> usubst q <*>usubst t1 <*> return n <*> usubst t2
    usubst (Mut info q t1 n t2)     = Mut <$> usubst info <*> usubst q <*> usubst t1 <*> return n <*> usubst t2
    usubst (Seal info q t)          = Seal <$> usubst info <*> usubst q <*> usubst t
    usubst (Imply info w q cs)      = Imply <$> usubst info <*> return w <*> usubst q <*> usubst cs

instance USubst ErrInfo where
    usubst (DfltInfo l n mbe ts)    = DfltInfo l n <$> usubst mbe <*> usubst ts
    usubst (DeclInfo l1 l2 n t msg) = DeclInfo l1 l2 n <$> usubst t <*> return msg
    usubst info                     = return info
    
instance USubst TSchema where
    usubst (TSchema l [] t)         = TSchema l [] <$> usubst t
    usubst (TSchema l q t)          = TSchema l <$> usubst q <*> usubst t

instance USubst TVar where
    usubst v                        = do t <- usubst (TVar NoLoc v)
                                         case t of
                                            TVar _ v' -> return v'
                                            _         -> return v

instance USubst TCon where
    usubst (TC n ts)                = TC n <$> usubst ts

instance USubst QBind where
    usubst (QBind v cs)             = QBind <$> usubst v <*> usubst cs

instance USubst Quant where
    usubst (Quant v cs)             = Quant <$> usubst v <*> usubst cs

instance USubst WTCon where
    usubst (wpath, p)               = do p <- usubst p; return (wpath, p)

instance USubst Type where
    usubst (TUni l u)               = do s <- usubstitution
                                         case Map.lookup u s of
                                            Just t  -> usubst t
                                            Nothing -> return (TUni l u)
    usubst (TVar l v)               = return $ TVar l v
    usubst (TCon l c)               = TCon l <$> usubst c
    usubst (TFun l fx p k t)        = TFun l <$> usubst fx <*> usubst p <*> usubst k <*> usubst t
    usubst (TTuple l p k)           = TTuple l <$> usubst p <*> usubst k
    usubst (TOpt l t)               = TOpt l <$> usubst t
    usubst (TNone l)                = return $ TNone l
    usubst (TWild l)                = return $ TWild l
    usubst (TNil l s)               = return $ TNil l s
    usubst (TRow l k n t r)         = TRow l k n <$> usubst t <*> usubst r
    usubst (TStar l k r)            = TStar l k <$> usubst r
    usubst (TFX l fx)               = return $ TFX l fx

instance USubst PosPar where
    usubst (PosPar n t e p)         = PosPar n <$> usubst t <*> usubst e <*> usubst p
    usubst (PosSTAR n t)            = PosSTAR n <$> usubst t
    usubst PosNIL                   = return PosNIL

instance USubst KwdPar where
    usubst (KwdPar n t e p)         = KwdPar n <$> usubst t <*> usubst e <*> usubst p
    usubst (KwdSTAR n t)            = KwdSTAR n <$> usubst t
    usubst KwdNIL                   = return KwdNIL

instance USubst Decl where
    usubst (Def l n q p k a ss de fx doc)   = Def l n <$> usubst q <*> usubst p <*> usubst k <*> usubst a <*> usubst ss <*> return de <*> usubst fx <*> return doc
    usubst (Actor l n q p k ss doc)         = Actor l n <$> usubst q <*> usubst p <*> usubst k <*> usubst ss <*> return doc
    usubst (Class l n q bs ss doc)          = Class l n <$> usubst q <*> usubst bs <*> usubst ss <*> return doc
    usubst (Protocol l n q bs ss doc)       = Protocol l n <$> usubst q <*> usubst bs <*> usubst ss <*> return doc
    usubst (Extension l q c bs ss doc)      = Extension l <$> usubst q <*> usubst c <*> usubst bs <*> usubst ss <*> return doc

instance USubst Stmt where
    usubst (Expr l e)               = Expr l <$> usubst e
    usubst (Assign l ps e)          = Assign l <$> usubst ps <*> usubst e
    usubst (MutAssign l t e)        = MutAssign l <$> usubst t <*> usubst e
    usubst (AugAssign l t op e)     = AugAssign l <$> usubst t <*> return op <*> usubst e
    usubst (Assert l e mbe)         = Assert l <$> usubst e <*> usubst mbe
    usubst (Delete l t)             = Delete l <$> usubst t
    usubst (Return l mbe)           = Return l <$> usubst mbe
    usubst (Raise l e)              = Raise l <$> usubst e
    usubst (If l bs els)            = If l <$> usubst bs <*> usubst els
    usubst (While l e b els)        = While l <$> usubst e <*> usubst b <*> usubst els
    usubst (For l p e b els)        = For l <$> usubst p <*> usubst e <*> usubst b <*> usubst els
    usubst (Try l b hs els fin)     = Try l <$> usubst b <*> usubst hs <*> usubst els <*> usubst fin
    usubst (With l is b)            = With l <$> usubst is <*> usubst b
    usubst (VarAssign l ps e)       = VarAssign l <$> usubst ps <*> usubst e
    usubst (After l e e')           = After l <$> usubst e <*> usubst e'
    usubst (Decl l ds)              = Decl l <$> usubst ds
    usubst (Signature l ns tsc d)   = Signature l ns <$> usubst tsc <*> return d
    usubst s                        = return s

instance USubst Expr where
    usubst (Call l e p k)           = Call l <$> usubst e <*> usubst p <*> usubst k
    usubst (TApp l e ts)            = TApp l <$> usubst e <*> usubst ts
    usubst (Async l e)              = Async l <$> usubst e
    usubst (Await l e)              = Await l <$> usubst e
    usubst (Index l e ix)           = Index l <$> usubst e <*> usubst ix
    usubst (Slice l e sl)           = Slice l <$> usubst e <*> usubst sl
    usubst (Cond l e1 cond e2)      = Cond l <$> usubst e1 <*> usubst cond <*> usubst e2
    usubst (IsInstance l e c)       = IsInstance l <$> usubst e <*> return c
    usubst (BinOp l e1 op e2)       = BinOp l <$> usubst e1 <*> return op <*> usubst e2
    usubst (CompOp l e ops)         = CompOp l <$> usubst e <*> usubst ops
    usubst (UnOp l op e)            = UnOp l op <$> usubst e
    usubst (Dot l e n)              = Dot l <$> usubst e <*> return n
    usubst (Rest l e n)             = Rest l <$> usubst e <*> return n
    usubst (DotI l e i)             = DotI l <$> usubst e <*> return i
    usubst (RestI l e i)            = RestI l <$> usubst e <*> return i
    usubst (Lambda l p k e fx)      = Lambda l <$> usubst p <*> usubst k <*> usubst e <*> usubst fx
    usubst (Yield l e)              = Yield l <$> usubst e
    usubst (YieldFrom l e)          = YieldFrom l <$> usubst e
    usubst (Tuple l p k)            = Tuple l <$> usubst p <*> usubst k
    usubst (List l es)              = List l <$> usubst es
    usubst (ListComp l e c)         = ListComp l <$> usubst e <*> usubst c
    usubst (Dict l as)              = Dict l <$> usubst as
    usubst (DictComp l a c)         = DictComp l <$> usubst a <*> usubst c
    usubst (Set l es)               = Set l <$> usubst es
    usubst (SetComp l e c)          = SetComp l <$> usubst e <*> usubst c
    usubst (Paren l e)              = Paren l <$> usubst e
    usubst e                        = return e

instance USubst Pattern where
    usubst (PWild l t)              = PWild l <$> usubst t
    usubst (PVar l n t)             = PVar l n <$> usubst t
    usubst (PParen l p)             = PParen l <$> usubst p
    usubst (PTuple l p k)           = PTuple l <$> usubst p <*> usubst k
    usubst (PList l ps p)           = PList l <$> usubst ps <*> usubst p
    
instance USubst PosPat where
    usubst (PosPat p pp)            = PosPat <$> usubst p <*> usubst pp
    usubst (PosPatStar p)           = PosPatStar <$> usubst p
    usubst PosPatNil                = return PosPatNil

instance USubst KwdPat where
    usubst (KwdPat n p kp)          = KwdPat n <$> usubst p <*> usubst kp
    usubst (KwdPatStar p)           = KwdPatStar <$> usubst p
    usubst KwdPatNil                = return KwdPatNil

instance USubst Branch where
    usubst (Branch e b)             = Branch <$> usubst e <*> usubst b

instance USubst Handler where
    usubst (Handler ex b)           = Handler ex <$> usubst b

instance USubst WithItem where
    usubst (WithItem e p)           = WithItem <$> usubst e <*> usubst p
    
instance USubst PosArg where
    usubst (PosArg e p)             = PosArg <$> usubst e <*> usubst p
    usubst (PosStar e)              = PosStar <$> usubst e
    usubst PosNil                   = return PosNil

instance USubst KwdArg where
    usubst (KwdArg n e k)           = KwdArg n <$> usubst e <*> usubst k
    usubst (KwdStar e)              = KwdStar <$> usubst e
    usubst KwdNil                   = return KwdNil

instance USubst Assoc where
    usubst (Assoc k v)              = Assoc <$> usubst k <*> usubst v
    usubst (StarStar e)             = StarStar <$> usubst e

instance USubst Elem where
    usubst (Elem e)                 = Elem <$> usubst e
    usubst (Star e)                 = Star <$> usubst e

instance USubst Comp where
    usubst (CompFor l p e c)        = CompFor l <$> usubst p <*> usubst e <*> usubst c
    usubst (CompIf l e c)           = CompIf l <$> usubst e <*> usubst c
    usubst NoComp                   = return NoComp

instance USubst Sliz where
    usubst (Sliz l e1 e2 e3)        = Sliz l <$> usubst e1 <*> usubst e2 <*> usubst e3

instance USubst OpArg where
    usubst (OpArg op e)             = OpArg op <$> usubst e


-- Polarity -------------------------------------------------------------------------------------------

class (USubst a, UFree a) => Polarity a where
    polvars                         :: a -> ([TUni],[TUni])

(p,n) `polcat` (p',n')              = (p++p', n++n')

polnil                              = ([], [])

polnull (p,n)                       = null p && null n

polneg (p,n)                        = (n,p)

(p,n) `polminus` (p',n')            = (p\\p', n\\n')

instance Polarity Type where
    polvars (TUni _ u)              = ([u],[])
    polvars (TVar _ v)              = polnil
    polvars (TCon _ c)              = polvars c
    polvars (TFun _ fx p k t)       = polvars fx `polcat` polvars t `polcat` polneg (polvars p `polcat` polvars k)
    polvars (TTuple _ p k)          = polvars p `polcat` polvars k
    polvars (TOpt _ t)              = polvars t
    polvars (TNone _)               = polnil
    polvars (TWild _)               = polnil
    polvars (TNil _ _)              = polnil
    polvars (TRow _ _ _ t r)        = polvars t `polcat` polvars r
    polvars (TStar _ _ r)           = polvars r
    polvars (TFX l fx)              = polnil

invvars x                           = (vs, vs)
  where vs                          = ufree x

covariant                           = [qnSetT,qnDict,qnList]                -- Ignore mutation for now!

instance Polarity TCon where
    polvars (TC c ts)
      | c `elem` covariant          = (vs,[])
      | otherwise                   = (vs,vs)
      where vs                      = ufree ts

instance Polarity QBind where
    polvars (QBind v cs)            = (vs,vs) where vs = ufree cs

instance Polarity TSchema where
    polvars (TSchema _ q t)         = polvars q `polcat` polvars t

instance (Polarity a) => Polarity (Maybe a) where
    polvars                         = maybe polnil polvars

instance (Polarity a) => Polarity [a] where
    polvars                         = foldr polcat polnil . map polvars


closePolVars                            :: ([TUni],[TUni]) -> Constraints -> ([TUni],[TUni])
closePolVars pvs cs
  | polnull (pvs' `polminus` pvs)       = pvs'
  | otherwise                           = closePolVars pvs' cs'
  where
    (pvs',cs')                          = boundvs pvs cs

    boundvs pn []                        = (pn, [])
    boundvs pn (Cast _ _ t (TUni _ v) : cs)
      | v `elem` fst pn                 = boundvs (polvars t `polcat` pn) cs
    boundvs pn (Sub _ _ _ t (TUni _ v) : cs)
      | v `elem` fst pn                 = boundvs (polvars t `polcat` pn) cs
    boundvs pn (Cast _ _ (TUni _ v) t : cs)
      | v `elem` snd pn                 = boundvs (polneg (polvars t) `polcat` pn) cs
    boundvs pn (Sub _ _ _ (TUni _ v) t : cs)
      | v `elem` snd pn                 = boundvs (polneg (polvars t) `polcat` pn) cs
    boundvs pn (Proto _ _ _ (TUni _ v) p : cs)
      | v `elem` snd pn                 = boundvs (polneg (polvars p) `polcat` pn) cs
    boundvs pn (Sel _ _ _ (TUni _ v) _ t : cs)
      | v `elem` snd pn                 = boundvs (polneg (polvars t) `polcat` pn) cs
    boundvsboundvs pn (Mut _ _ (TUni _ v) _ t : cs)
      | v `elem` (fst pn ++ snd pn)     = boundvs (invvars t `polcat` pn) cs
    bnds pn (c : cs)                    = let (pn',cs') = boundvs pn cs in (pn', c:cs')


-- Does Self occur positively?
posself (TVar _ v)                  = v == tvSelf
posself (TCon _ c)                  = any posself (tcargs c)
posself (TFun _ fx p k t)           = any posself [fx,t] || any negself [p,k]
posself (TTuple _ p k)              = any posself [p,k]
posself (TOpt _ t)                  = posself t
posself (TRow _ _ _ t r)            = any posself [t,r]
posself (TStar _ _ r)               = posself r
posself _                           = False

-- Does Self occur negatively?
negself (TCon _ c)                  = any negself (tcargs c)
negself (TFun _ fx p k t)           = any negself [fx,t] || any posself [p,k]
negself (TTuple _ p k)              = any negself [p,k]
negself (TOpt _ t)                  = negself t
negself (TRow _ _ _ t r)            = any negself [t,r]
negself (TStar _ _ r)               = negself r
negself _                           = False


-- Tailvars -------------------------------------------------------------------------------------------

class (UFree a) => Tailvars a where
    -- Find free univars of kind PRow or KRow (for the purpose of defaulting them to TNil)
    tailvars                        :: a -> [TUni]

instance Tailvars Type where
    tailvars (TCon _ c)             = tailvars c
    tailvars (TFun _ fx p k t)      = tailvars fx ++ tailvars' p ++ tailvars' k ++ tailvars t
    tailvars (TTuple _ p k)
      | TUni{} <- p, TNil{} <- k    = []    -- Exclude open * tuples
      | TNil{} <- p, TUni{} <- k    = []    -- Exclude open ** tuples
      | otherwise                   = tailvars' p ++ tailvars' k
    tailvars (TOpt _ t)             = tailvars t
    tailvars _                      = []

tailvars' (TRow _ _ _ t r)          = tailvars t ++ tailvars' r
tailvars' (TStar _ _ r)             = tailvars r
tailvars' (TNil _ _)                = []
tailvars' (TUni _ u)                = [u]

instance Tailvars TCon where
    tailvars (TC c ts)              = tailvars ts

instance Tailvars QBind where
    tailvars (QBind v cs)           = tailvars cs

instance Tailvars Quant where
    tailvars (Quant v cs)           = tailvars cs

instance Tailvars WTCon where
    tailvars (wpath, p)             = tailvars p

instance Tailvars TSchema where
    tailvars (TSchema _ q t)        = tailvars q ++ tailvars t

instance (Tailvars a) => Tailvars (Maybe a) where
    tailvars                        = maybe [] tailvars

instance (Tailvars a) => Tailvars [a] where
    tailvars                        = concat . map tailvars

instance Tailvars Constraint where
    tailvars (Cast _ q t1 t2)       = tailvars q ++ tailvars t1 ++ tailvars t2
    tailvars (Sub _ w q t1 t2)      = tailvars q ++ tailvars t1 ++ tailvars t2
    tailvars (Proto _ w q t p)      = tailvars q ++ tailvars t ++ tailvars p
    tailvars (Sel _ w q t1 n t2)    = tailvars q ++ tailvars t1 ++ tailvars t2
    tailvars (Mut _ q t1 n t2)      = tailvars q ++ tailvars t1 ++ tailvars t2
    tailvars (Seal _ q t)           = tailvars q ++ tailvars t
    tailvars (Imply _ w q cs)       = tailvars q ++ tailvars cs


-- Misc. ---------------------------------------------------------------------------------------------


closeDepVars vs cs
  | null vs'                        = nub vs
  | otherwise                       = closeDepVars (vs'++vs) cs
  where vs'                         = concat [ deps c \\ vs | c <- cs, all (`elem` vs) (heads c) ]

        heads (Proto _ w q t _)     = ufree t
        heads (Cast _ q t _)        = ufree t
        heads (Sub _ w q t _)       = ufree t
        heads (Sel _ w q t n _)     = ufree t
        heads (Mut _ q t n _)       = ufree t
        heads (Seal _ q t)          = ufree t
        heads (Imply _ w q cs)      = []

        deps (Proto _ w q _ p)      = ufree p
        deps (Cast _ q _ t)         = typarams t
        deps (Sub _ w q _ t)        = typarams t
        deps (Sel _ w q _ n t)      = ufree t
        deps (Mut _ q _ n t)        = ufree t
        deps (Seal _ q _)           = []
        deps (Imply _ w q cs)       = []

        typarams (TOpt _ t)         = typarams t
        typarams (TCon _ c)         = ufree c
        typarams _                  = []


closeDepVarsQ vs q
  | null vs'                        = nub vs
  | otherwise                       = closeDepVarsQ (vs'++vs) q
  where vs'                         = concat [ vfree us \\ vs | QBind v us <- q, v `elem` vs ]

schematic (TCon _ tc)               = tCon (schematic' tc)
schematic (TFun _ _ _ _ _)          = tFun tWild tWild tWild tWild
schematic (TTuple _ _ _)            = tTuple tWild tWild
schematic (TOpt _ _)                = tOpt tWild
schematic (TRow _ k n _ r)          = tRow k n tWild (schematic r)
schematic (TStar _ k _)             = tStar k tWild
schematic t                         = t

schematic' (TC n ts)                = TC n [ tWild | _ <- ts ]

wildargs i                          = [ tWild | _ <- nbinds i ]
  where
    nbinds (NAct q _ _ _ _)         = q
    nbinds (NClass q _ _ _)         = q
    nbinds (NProto q _ _ _)         = q
    nbinds (NExt q _ _ _ _ _)       = q


class UWild a where
    uwild                           :: a -> a

instance UWild a => UWild [a] where
    uwild                           = map uwild

instance UWild a => UWild (Maybe a) where
    uwild                           = fmap uwild

instance UWild TCon where
    uwild (TC n ts)                 = TC n (uwild ts)

instance UWild Type where
    uwild (TUni l u)                = tWild
    uwild (TCon l c)                = TCon l (uwild c)
    uwild (TFun l fx p k t)         = TFun l fx (uwild p) (uwild k) (uwild t)
    uwild (TTuple l p k)            = TTuple l (uwild p) (uwild k)
    uwild (TOpt l t)                = TOpt l (uwild t)
    uwild (TRow l k n t r)          = TRow l k n (uwild t) (uwild r)
    uwild (TStar l k r)             = TStar l k (uwild r)
    uwild t                         = t

instance UWild TSchema where
    uwild (TSchema l q t)           = TSchema l (uwild q) (uwild t)

instance UWild QBind where
    uwild (QBind v cs)              = QBind v (uwild cs)

instance UWild Quant where
    uwild (Quant v cs)              = Quant v (uwild cs)

instance UWild WTCon where
    uwild (wpath, p)                = (wpath, uwild p)

instance UWild Constraint where
    uwild (Cast info q t1 t2)       = Cast (uwild info) (uwild q) (uwild t1) (uwild t2)
    uwild (Sub info w q t1 t2)      = Sub (uwild info) w (uwild q) (uwild t1) (uwild t2)
    uwild (Proto info w q t p)      = Proto (uwild info) w (uwild q) (uwild t) (uwild p)
    uwild (Sel info w q t1 n t2)    = Sel (uwild info) w (uwild q) (uwild t1) n (uwild t2)
    uwild (Mut info q t1 n t2)      = Mut (uwild info) (uwild q) (uwild t1) n (uwild t2)
    uwild (Seal info q t)           = Seal (uwild info) (uwild q) (uwild t)
    uwild (Imply info w q cs)       = Imply (uwild info) w (uwild q) (uwild cs)

instance UWild ErrInfo where
    uwild (DfltInfo l n mbe ts)     = DfltInfo l n mbe (uwild ts)
    uwild (DeclInfo l1 l2 n t msg)  = DeclInfo l1 l2 n (uwild t) msg
    uwild info                      = info

instance (UWild a, UWild b) => UWild (QName,a,b) where
    uwild (n, t, u)                 = (n, uwild t, uwild u)
