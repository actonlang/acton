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

closeDepVars vs cs
  | null vs'                        = nub vs
  | otherwise                       = closeDepVars (vs'++vs) cs
  where vs'                         = concat [ deps c \\ vs | c <- cs, all (`elem` vs) (heads c) ]

        heads (Impl _ w t _)        = ufree t
        heads (Cast _ t _)          = ufree t
        heads (Sub _ w t _)         = ufree t
        heads (Sel _ w t n _)       = ufree t
        heads (Mut _ t n _)         = ufree t
        heads (Seal _ t)            = ufree t

        deps (Impl _ w _ p)         = ufree p
        deps (Cast _ _ t)           = typarams t
        deps (Sub _ w _ t)          = typarams t
        deps (Sel _ w _ n t)        = ufree t
        deps (Mut _ _ n t)          = ufree t
        deps (Seal _ _)             = []

        typarams (TOpt _ t)         = typarams t
        typarams (TCon _ c)         = ufree c
        typarams _                  = []


closeDepVarsQ vs q
  | null vs'                        = nub vs
  | otherwise                       = closeDepVarsQ (vs'++vs) q
  where vs'                         = concat [ ufree us \\ vs | Quant v us <- q, v `elem` vs ]

qualbound q                         = [ v | Quant v ps <- q, not $ null ps ]


subst                               :: Subst a => Substitution -> a -> a
subst s x0
  | null clash                      = runTypeM' s (usubst x0)
  | otherwise                       = x2
  where x1                          = runTypeM' s0 (usubst x0)
        x2                          = runTypeM' s1 (usubst x1)
        s0                          = [ (v, subst (clash `zip` map tVar tmp) t) | (v,t) <- s ]
        s1                          = tmp `zip` map tVar clash
        clash                       = dom s `intersect` ufree (rng s)
        used                        = dom s ++ ufree (rng s)                             
        tmp                         = take (length clash) $ map (TV KWild) tmpNames \\ used


class Subst t where
    usubst                          :: t -> TypeM t
    ufree                           :: t -> [TVar]
    tybound                         :: t -> [TVar]
    tybound _                       = []

instance Subst a => Subst (Name,a) where
    usubst (n, t)                   = (,) <$> return n <*> usubst t
    ufree (n, t)                    = ufree t
    tybound (n, t)                  = tybound t

instance (Subst a, Subst b) => Subst (QName,a,b) where
    usubst (n, t, u)                = (,,) <$> return n <*> usubst t <*> usubst u
    ufree (n, t, u)                 = ufree t ++ ufree u
    tybound (n, t, u)               = tybound t ++ tybound u

instance Subst a => Subst [a] where
    usubst                          = mapM usubst
    ufree                           = concat . map ufree
    tybound                         = concat . map tybound

instance Subst a => Subst (Maybe a) where
    usubst                          = maybe (return Nothing) (\x -> Just <$> usubst x)
    ufree                           = maybe [] ufree
    tybound                         = maybe [] tybound

instance Subst Constraint where
    usubst (Cast info t1 t2)        = Cast <$> usubst info <*> usubst t1 <*> usubst t2
    usubst (Sub info w t1 t2)       = Sub <$> usubst info <*> return w <*> usubst t1 <*> usubst t2
    usubst (Impl info w t p)        = Impl <$> usubst info <*> return w <*>usubst t <*> usubst p
    usubst (Sel info w t1 n t2)     = Sel <$> usubst info <*> return w <*>usubst t1 <*> return n <*> usubst t2
    usubst (Mut info t1 n t2)       = Mut <$> usubst info <*> usubst t1 <*> return n <*> usubst t2
    usubst (Seal info t)            = Seal <$> usubst info <*> usubst t

    ufree (Cast info t1 t2)         = ufree info ++ ufree t1 ++ ufree t2
    ufree (Sub info w t1 t2)        = ufree info ++ ufree t1 ++ ufree t2
    ufree (Impl info w t p)         = ufree info ++ ufree t ++ ufree p
    ufree (Sel info w t1 n t2)      = ufree info ++ ufree t1 ++ ufree t2
    ufree (Mut info t1 n t2)        = ufree info ++ ufree t1 ++ ufree t2
    ufree (Seal info t)             = ufree info ++ ufree t


instance Subst ErrInfo where
    usubst (DfltInfo l n mbe ts)    = DfltInfo l n <$> usubst mbe <*> usubst ts
    usubst (DeclInfo l1 l2 n t msg) = DeclInfo l1 l2 n <$> usubst t <*> return msg
    usubst info                     = return info
    
    ufree (DfltInfo l n mbe ts)     = ufree mbe ++ ufree ts
    ufree (DeclInfo l1 l2 n t msg)  = ufree t
    ufree _                         = []
    
instance Subst TSchema where
    usubst (TSchema l [] t)         = TSchema l [] <$> usubst t
    usubst (TSchema l q t)          = TSchema l <$> usubst q <*> usubst t

    ufree (TSchema _ [] t)          = ufree t
    ufree (TSchema _ q t)           = (ufree q ++ ufree t) \\ tybound q

    tybound (TSchema _ q t)         = tybound q


schematic (TCon _ tc)               = tCon (schematic' tc)
schematic (TFun _ _ _ _ _)          = tFun tWild tWild tWild tWild
schematic (TTuple _ _ _)            = tTuple tWild tWild
schematic (TOpt _ _)                = tOpt tWild
schematic (TRow _ k n _ r)          = tRow k n tWild (schematic r)
schematic (TStar _ k _)             = tStar k tWild
schematic t                         = t

schematic' (TC n ts)                = TC n [ tWild | _ <- ts ]

wild t                              = subst [ (v,tWild) | v <- nub (ufree t), univar v ] t

wildify                             :: (Subst a) => a -> TypeM a
wildify a                           = return (wild a)

wildargs i                          = [ tWild | _ <- nbinds i ]
  where
    nbinds (NAct q _ _ _ _)         = q
    nbinds (NClass q _ _ _)         = q
    nbinds (NProto q _ _ _)         = q
    nbinds (NExt q _ _ _ _)         = q
            


instance Subst TVar where
    usubst v                        = do t <- usubst (TVar NoLoc v)
                                         case t of
                                            TVar _ v' -> return v'
                                            _         -> return v

    ufree v                         = [v]
        
instance Subst TCon where
    usubst (TC n ts)                = TC n <$> usubst ts
    ufree (TC n ts)                 = ufree ts

instance Subst QBind where
    usubst (Quant v cs)             = Quant <$> usubst v <*> usubst cs
    ufree (Quant v cs)              = v : ufree cs
    tybound (Quant v cs)            = [v]

instance Subst Type where
    usubst (TVar l v)               = do s <- usubstitution
                                         case Map.lookup v s of
                                            Just t ->  usubst t
                                            Nothing -> return (TVar l v)
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

    ufree (TVar _ v)                = [v]
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


instance Subst PosPar where
    usubst (PosPar n t e p)         = PosPar n <$> usubst t <*> usubst e <*> usubst p
    usubst (PosSTAR n t)            = PosSTAR n <$> usubst t
    usubst PosNIL                   = return PosNIL
    
    ufree (PosPar n t e p)          = ufree t ++ ufree p
    ufree (PosSTAR n t)             = ufree t
    ufree PosNIL                    = []

instance Subst KwdPar where
    usubst (KwdPar n t e p)         = KwdPar n <$> usubst t <*> usubst e <*> usubst p
    usubst (KwdSTAR n t)            = KwdSTAR n <$> usubst t
    usubst KwdNIL                   = return KwdNIL
    
    ufree (KwdPar n t e p)          = ufree t ++ ufree p
    ufree (KwdSTAR n t)             = ufree t
    ufree KwdNIL                    = []

instance Subst Decl where
    usubst (Def l n q p k a ss de fx doc)   = Def l n <$> usubst q <*> usubst p <*> usubst k <*> usubst a <*> usubst ss <*> return de <*> usubst fx <*> return doc
    usubst (Actor l n q p k ss doc)         = Actor l n <$> usubst q <*> usubst p <*> usubst k <*> usubst ss <*> return doc
    usubst (Class l n q bs ss doc)          = Class l n <$> usubst q <*> usubst bs <*> usubst ss <*> return doc
    usubst (Protocol l n q bs ss doc)       = Protocol l n <$> usubst q <*> usubst bs <*> usubst ss <*> return doc
    usubst (Extension l q c bs ss doc)      = Extension l <$> usubst q <*> usubst c <*> usubst bs <*> usubst ss <*> return doc

    tybound (Protocol l n q ps b _)   = tvSelf : tybound q
    tybound (Class l n q ps b _)      = tvSelf : tybound q
    tybound (Extension l q c ps b _)  = tvSelf : tybound q
    tybound (Def l n q p k t b d x _) = tybound q
    tybound (Actor l n q p k b _)     = tybound q
    
    ufree (Protocol l n q ps b _)     = nub (ufree q ++ ufree ps ++ ufree b) \\ (tvSelf : tybound q)
    ufree (Class l n q ps b _)        = nub (ufree q ++ ufree ps ++ ufree b) \\ (tvSelf : tybound q)
    ufree (Extension l q c ps b _)    = nub (ufree q ++ ufree c ++ ufree ps ++ ufree b) \\ (tvSelf : tybound q)
    ufree (Def l n q p k t b d x _)   = nub (ufree q ++ ufree p ++ ufree k ++ ufree b ++ ufree t ++ ufree x) \\ tybound q
    ufree (Actor l n q p k b _)       = nub (ufree q ++ ufree p ++ ufree k ++ ufree b) \\ (tybound q)
    
instance Subst Stmt where
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

    ufree (Expr l e)                = ufree e
    ufree (Assign l ps e)           = ufree ps ++ ufree e
    ufree (MutAssign l t e)         = ufree t ++ ufree e
    ufree (AugAssign l t op e)      = ufree t ++ ufree e
    ufree (Assert l e mbe)          = ufree mbe
    ufree (Delete l t)              = ufree t
    ufree (Return l mbe)            = ufree mbe
    ufree (Raise l e)               = ufree e
    ufree (If l bs els)             = ufree bs ++ ufree els
    ufree (While l e b els)         = ufree e ++ ufree b ++ ufree els
    ufree (For l p e b els)         = ufree p ++ ufree e ++ ufree b ++ ufree els
    ufree (Try l b hs els fin)      = ufree b ++ ufree hs ++ ufree els ++ ufree fin
    ufree (With l is b)             = ufree is ++ ufree b
    ufree (VarAssign l ps e)        = ufree ps ++ ufree e
    ufree (After l e e')            = ufree e ++ ufree e'
    ufree (Decl l ds)               = ufree ds
    ufree (Signature l ns tsc d)    = ufree tsc
    ufree s                         = []

instance Subst Expr where
    usubst (Call l e p k)           = Call l <$> usubst e <*> usubst p <*> usubst k
    usubst (TApp l e ts)            = TApp l <$> usubst e <*> usubst ts
    usubst (Async l e)              = Async l <$> usubst e
    usubst (Await l e)              = Await l <$> usubst e
    usubst (Index l e ix)           = Index l <$> usubst e <*> usubst ix
    usubst (Slice l e sl)           = Slice l <$> usubst e <*> usubst sl
    usubst (NDSlice l e sl)         = NDSlice l <$> usubst e <*> usubst sl
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

    ufree (Call l e p k)            = ufree e ++ ufree p ++ ufree k
    ufree (TApp l e ts)             = ufree e ++ ufree ts
    ufree (Async l e)               = ufree e
    ufree (Await l e)               = ufree e
    ufree (Index l e ix)            = ufree e ++ ufree ix
    ufree (Slice l e sl)            = ufree e ++ ufree sl
    ufree (NDSlice l e sl)          = ufree e ++ ufree sl
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

instance Subst Branch where
    usubst (Branch e b)             = Branch <$> usubst e <*> usubst b
    
    ufree (Branch e b)              = ufree e ++ ufree b

instance Subst Pattern where
    usubst (PWild l t)              = PWild l <$> usubst t
    usubst (PVar l n t)             = PVar l n <$> usubst t
    usubst (PParen l p)             = PParen l <$> usubst p
    usubst (PTuple l p k)           = PTuple l <$> usubst p <*> usubst k
    usubst (PList l ps p)           = PList l <$> usubst ps <*> usubst p
    
    ufree (PWild _ t)               = ufree t
    ufree (PVar _ n t)              = ufree t
    ufree (PParen _ p)              = ufree p
    ufree (PTuple _ p k)            = ufree p ++ ufree k
    ufree (PList _ ps p)            = ufree ps ++ ufree p

instance Subst PosPat where
    usubst (PosPat p pp)            = PosPat <$> usubst p <*> usubst pp
    usubst (PosPatStar p)           = PosPatStar <$> usubst p
    usubst PosPatNil                = return PosPatNil

    ufree (PosPat p pp)             = ufree p ++ ufree pp
    ufree (PosPatStar p)            = ufree p
    ufree PosPatNil                 = []

instance Subst KwdPat where
    usubst (KwdPat n p kp)          = KwdPat n <$> usubst p <*> usubst kp
    usubst (KwdPatStar p)           = KwdPatStar <$> usubst p
    usubst KwdPatNil                = return KwdPatNil

    ufree (KwdPat n p kp)           = ufree p ++ ufree kp
    ufree (KwdPatStar p)            = ufree p
    ufree KwdPatNil                 = []

instance Subst Handler where
    usubst (Handler ex b)           = Handler ex <$> usubst b

    ufree (Handler ex b)            = ufree b

instance Subst WithItem where
    usubst (WithItem e p)           = WithItem <$> usubst e <*> usubst p
    
    ufree (WithItem e p)            = ufree e ++ ufree p

instance Subst PosArg where
    usubst (PosArg e p)             = PosArg <$> usubst e <*> usubst p
    usubst (PosStar e)              = PosStar <$> usubst e
    usubst PosNil                   = return PosNil

    ufree (PosArg e p)              = ufree e ++ ufree p
    ufree (PosStar e)               = ufree e
    ufree PosNil                    = []

instance Subst KwdArg where
    usubst (KwdArg n e k)           = KwdArg n <$> usubst e <*> usubst k
    usubst (KwdStar e)              = KwdStar <$> usubst e
    usubst KwdNil                   = return KwdNil

    ufree (KwdArg n e k)            = ufree e ++ ufree k
    ufree (KwdStar e)               = ufree e
    ufree KwdNil                    = []

instance Subst Sliz where
    usubst (Sliz l e1 e2 e3)        = Sliz l <$> usubst e1 <*> usubst e2 <*> usubst e3

    ufree (Sliz _ e1 e2 e3)         = ufree e1 ++ ufree e2 ++ ufree e3

instance Subst NDSliz where
    usubst (NDExpr e)               = NDExpr <$> usubst e
    usubst (NDSliz s)               = NDSliz <$> usubst s

    ufree (NDExpr e)                = ufree e
    ufree (NDSliz s)                = ufree s
    

instance Subst OpArg where
    usubst (OpArg op e)             = OpArg op <$> usubst e

    ufree (OpArg op e)              = ufree e

instance Subst Elem where
    usubst (Elem e)                 = Elem <$> usubst e
    usubst (Star e)                 = Star <$> usubst e

    ufree (Elem e)                  = ufree e
    ufree (Star e)                  = ufree e

instance Subst Assoc where
    usubst (Assoc k v)              = Assoc <$> usubst k <*> usubst v
    usubst (StarStar e)             = StarStar <$> usubst e

    ufree (Assoc k v)               = ufree k ++ ufree v
    ufree (StarStar e)              = ufree e

instance Subst Comp where
    usubst (CompFor l p e c)        = CompFor l <$> usubst p <*> usubst e <*> usubst c
    usubst (CompIf l e c)           = CompIf l <$> usubst e <*> usubst c
    usubst NoComp                   = return NoComp

    ufree (CompFor _ p e c)         = ufree p ++ ufree e ++ ufree c
    ufree (CompIf _ e c)            = ufree e ++ ufree c
    ufree NoComp                    = []

class (Subst a) => Polarity a where
    polvars                         :: a -> ([TVar],[TVar])

(p,n) `polcat` (p',n')              = (p++p', n++n')

polneg (p,n)                        = (n,p)

(p,n) `polminus` vs                 = (p\\vs, n\\vs)

instance Polarity Type where
    polvars (TVar _ v)              = ([v],[])
    polvars (TCon _ c)              = polvars c
    polvars (TFun _ fx p k t)       = polvars fx `polcat` polvars t `polcat` polneg (polvars p `polcat` polvars k)
    polvars (TTuple _ p k)          = polvars p `polcat` polvars k
    polvars (TOpt _ t)              = polvars t
    polvars (TNone _)               = ([],[])
    polvars (TWild _)               = ([],[])
    polvars (TNil _ _)              = ([],[])
    polvars (TRow _ _ _ t r)        = polvars t `polcat` polvars r
    polvars (TStar _ _ r)           = polvars r
    polvars (TFX l fx)              = ([],[])

invvars x                           = (vs, vs)
  where vs                          = ufree x

covariant                           = [qnSetT,qnDict,qnList]                -- Ignore mutation for now!

instance Polarity TCon where
    polvars (TC c ts)
      | c `elem` covariant          = (vs,[])
      | otherwise                   = (vs,vs)
      where vs                      = ufree ts

instance Polarity QBind where
    polvars (Quant v cs)            = (vs,vs) where vs = v : ufree cs

instance Polarity TSchema where
    polvars (TSchema _ q t)         = (polvars q `polcat` polvars t) `polminus` tybound q

instance (Polarity a) => Polarity (Maybe a) where
    polvars                         = maybe ([],[]) polvars

instance (Polarity a) => Polarity [a] where
    polvars                         = foldr polcat ([],[]) . map polvars


class (Subst a) => Tailvars a where
    tailvars                        :: a -> [TVar]

instance Tailvars Type where
    tailvars (TCon _ c)             = tailvars c
    tailvars (TFun _ fx p k t)      = tailvars fx ++ tailvars' p ++ tailvars' k ++ tailvars t
    tailvars (TTuple _ p k)
      | TVar{} <- p, TNil{} <- k    = []
      | TNil{} <- p, TVar{} <- k    = []
      | otherwise                   = tailvars' p ++ tailvars' k
    tailvars (TOpt _ t)             = tailvars t
    tailvars _                      = []

tailvars' (TRow _ _ _ t r)          = tailvars t ++ tailvars' r
tailvars' (TStar _ _ r)             = tailvars r
tailvars' (TNil _ _)                = []
tailvars' (TVar _ v)                = [v]

instance Tailvars TCon where
    tailvars (TC c ts)              = tailvars ts

instance Tailvars QBind where
    tailvars (Quant v cs)           = tailvars cs

instance Tailvars TSchema where
    tailvars (TSchema _ q t)        = (tailvars q ++ tailvars t) \\ tybound q

instance (Tailvars a) => Tailvars (Maybe a) where
    tailvars                        = maybe [] tailvars

instance (Tailvars a) => Tailvars [a] where
    tailvars                        = concat . map tailvars

instance Tailvars Constraint where
    tailvars (Cast _ t1 t2)         = tailvars t1 ++ tailvars t2
    tailvars (Sub _ w t1 t2)        = tailvars t1 ++ tailvars t2
    tailvars (Impl _ w t p)         = tailvars t ++ tailvars p
    tailvars (Sel _ w t1 n t2)      = tailvars t1 ++ tailvars t2
    tailvars (Mut _ t1 n t2)        = tailvars t1 ++ tailvars t2
    tailvars (Seal _ t)             = tailvars t
