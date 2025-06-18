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

        heads (Impl _ w t _)        = tyfree t
        heads (Cast _ t _)          = tyfree t
        heads (Sub _ w t _)         = tyfree t
        heads (Sel _ w t n _)       = tyfree t
        heads (Mut _ t n _)         = tyfree t
        heads (Seal _ t)            = tyfree t

        deps (Impl _ w _ p)         = tyfree p
        deps (Cast _ _ t)           = typarams t
        deps (Sub _ w _ t)          = typarams t
        deps (Sel _ w _ n t)        = tyfree t
        deps (Mut _ _ n t)          = tyfree t
        deps (Seal _ _)             = []

        typarams (TOpt _ t)         = typarams t
        typarams (TCon _ c)         = tyfree c
        typarams _                  = []


closeDepVarsQ vs q
  | null vs'                        = nub vs
  | otherwise                       = closeDepVarsQ (vs'++vs) q
  where vs'                         = concat [ tyfree us \\ vs | Quant v us <- q, v `elem` vs ]

qualbound q                         = [ v | Quant v ps <- q, not $ null ps ]


subst                               :: Subst a => Substitution -> a -> a
subst s x0
  | null clash                      = runTypeM' s (msubst x0)
  | otherwise                       = x2
  where x1                          = runTypeM' s0 (msubst x0)
        x2                          = runTypeM' s1 (msubst x1)
        s0                          = [ (v, subst (clash `zip` map tVar tmp) t) | (v,t) <- s ]
        s1                          = tmp `zip` map tVar clash
        clash                       = dom s `intersect` tyfree (rng s)
        used                        = dom s ++ tyfree (rng s)
        tmp                         = take (length clash) $ map (TV KWild) tmpNames \\ used

substIteratively                    :: Subst a => Substitution -> a -> a
substIteratively s x                = runTypeM' s (msubst x)

erase x                             = subst s x
  where s                           = [ (tv, tWild) | tv <- nub (tyfree x) ]

class Subst t where
    msubst                          :: t -> TypeM t
    tyfree                          :: t -> [TVar]
    tybound                         :: t -> [TVar]
    tybound _                       = []

instance Subst a => Subst (Name,a) where
    msubst (n, t)                   = (,) <$> return n <*> msubst t
    tyfree (n, t)                   = tyfree t
    tybound (n, t)                  = tybound t

instance (Subst a, Subst b) => Subst (QName,a,b) where
    msubst (n, t, u)                = (,,) <$> return n <*> msubst t <*> msubst u
    tyfree (n, t, u)                = tyfree t ++ tyfree u
    tybound (n, t, u)               = tybound t ++ tybound u

instance Subst a => Subst [a] where
    msubst                          = mapM msubst
    tyfree                          = concat . map tyfree
    tybound                         = concat . map tybound

instance Subst a => Subst (Maybe a) where
    msubst                          = maybe (return Nothing) (\x -> Just <$> msubst x)
    tyfree                          = maybe [] tyfree
    tybound                         = maybe [] tybound

instance Subst Constraint where
    msubst (Cast info t1 t2)        = Cast <$> msubst info <*> msubst t1 <*> msubst t2
    msubst (Sub info w t1 t2)       = Sub <$> msubst info <*> return w <*> msubst t1 <*> msubst t2
    msubst (Impl info w t p)        = Impl <$> msubst info <*> return w <*>msubst t <*> msubst p
    msubst (Sel info w t1 n t2)     = Sel <$> msubst info <*> return w <*>msubst t1 <*> return n <*> msubst t2
    msubst (Mut info t1 n t2)       = Mut <$> msubst info <*> msubst t1 <*> return n <*> msubst t2
    msubst (Seal info t)            = Seal <$> msubst info <*> msubst t

    tyfree (Cast info t1 t2)        = tyfree info ++ tyfree t1 ++ tyfree t2
    tyfree (Sub info w t1 t2)       = tyfree info ++ tyfree t1 ++ tyfree t2
    tyfree (Impl info w t p)        = tyfree info ++ tyfree t ++ tyfree p
    tyfree (Sel info w t1 n t2)     = tyfree info ++ tyfree t1 ++ tyfree t2
    tyfree (Mut info t1 n t2)       = tyfree info ++ tyfree t1 ++ tyfree t2
    tyfree (Seal info t)            = tyfree info ++ tyfree t


instance Subst ErrInfo where
    msubst (DfltInfo l n mbe ts)    = DfltInfo l n <$> msubst mbe <*> msubst ts
    msubst (DeclInfo l1 l2 n t msg) = DeclInfo l1 l2 n <$> msubst t <*> return msg
    msubst info                     = return info

    tyfree (DfltInfo l n mbe ts)    = tyfree mbe ++ tyfree ts
    tyfree (DeclInfo l1 l2 n t msg) = tyfree t
    tyfree _                        = []

instance Subst TSchema where
    msubst (TSchema l [] t)         = TSchema l [] <$> msubst t
    msubst (TSchema l q t)          = TSchema l <$> msubst q <*> msubst t
    {-
    msubst sc@(TSchema l q t)       = (msubst' . Map.toList . Map.filterWithKey relevant) <$> getSubstitution
      where relevant k v            = k `elem` vs0
            vs0                     = tyfree sc
            msubst' s               = TSchema l (subst s q') (subst s t')
              where vs              = tybound q
                    newvars         = tyfree (rng s)
                    clashvars       = vs `intersect` newvars
                    avoidvars       = vs0 ++ vs ++ newvars
                    renaming        = tvarSupplyMap clashvars avoidvars
                    q'              = [ Quant (subst renaming v) (subst renaming cs) | Quant v cs <- q ]
                    t'              = subst renaming t
    -}
    tyfree (TSchema _ [] t)         = tyfree t
    tyfree (TSchema _ q t)          = (tyfree q ++ tyfree t) \\ tybound q
    tybound (TSchema _ q t)         = tybound q

testSchemaSubst = do
    putStrLn ("t:  " ++ prstr t)
    putStrLn ("c:  " ++ prstr c)
    putStrLn ("s1: " ++ prstrs s1)
    putStrLn ("s2: " ++ prstrs s2)
    putStrLn ("s3: " ++ prstrs s3)
    putStrLn ("s4: " ++ prstrs s4)
    putStrLn ("s5: " ++ prstrs s5)
    putStrLn ("subst s1 t: " ++ prstr (subst s1 t))
    putStrLn ("subst s2 t: " ++ prstr (subst s2 t))
    putStrLn ("subst s3 t: " ++ prstr (subst s3 t))
    putStrLn ("subst s4 t: " ++ prstr (subst s4 t))
    putStrLn ("subst s5 t: " ++ prstr (subst s5 t))
    putStrLn ("subst s5 c: " ++ prstr (subst s5 c))
  where t   = tSchema [Quant (TV KType (name "A")) [TC (noQ "Eq") []]] c
        c   = (tCon (TC (noQ "apa") [tVar (TV KType (name "A")),
                                     tVar (TV KType (name "B")),
                                     tVar (TV KType (name "C"))]))
        s1  = [(TV KType (name "B"), tSelf)]
        s2  = [(TV KType (name "A"), tSelf)]
        s3  = [(TV KType (name "B"), tVar (TV KType (name "A")))]
        s4  = [(TV KType (name "B"), tVar (TV KType (name "C"))), (TV KType (name "C"), tSelf)]
        s5  = [(TV KType (name "B"), tVar (TV KType (name "D"))), (TV KType (name "D"), tSelf)]

msubstRenaming                      :: Subst a => a -> TypeM (Substitution,Substitution)
msubstRenaming c                    = do s <- Map.toList . Map.filterWithKey relevant <$> getSubstitution
                                         return $ (dom s `zip` subst (renaming (tyfree (rng s))) (rng s),renaming (tyfree (rng s)))
      where relevant k _            = k `elem` vs0
            vs0                     = tyfree c
            vs                      = tybound c
            renaming newvars        = tvarSupplyMap clashvars avoidvars
              where clashvars       = vs `intersect` newvars
                    avoidvars       = vs0 ++ vs ++ newvars

msubstWith                          :: (Subst a) => Substitution -> a -> TypeM a
msubstWith [] x                     = return x
msubstWith s x                      = do s0 <- getSubstitution
                                         sequence [ substitute tv t | (tv,t) <- s ]
                                         x' <- msubst x
                                         setSubstitution s0
                                         return x'

wildify                             :: (Subst a) => a -> TypeM a
wildify a                           = msubstWith (zip (tyfree a \\ (tvSelf : tybound a)) (repeat tWild)) a

testMsubstRenaming = do
    putStrLn ("p1: " ++ render (pretty (runTypeM p1)))
    putStrLn ("p2: " ++ render (pretty (runTypeM p2)))
    putStrLn ("p3: " ++ render (pretty (runTypeM p3)))
    putStrLn ("r1: " ++ render (pretty (runTypeM r1)))
    putStrLn ("r2: " ++ render (pretty (runTypeM r2)))
    putStrLn ("r3: " ++ render (pretty (runTypeM r3)))
  where t   = tSchema [Quant (TV KType (name "A")) [TC (noQ "Eq") []]]
                            (tCon (TC (noQ "apa") [tVar (TV KType (name "A")),
                                                   tVar (TV KType (name "B"))]))
        msubst' sc@(TSchema l q t) = do (s,ren) <- msubstRenaming sc
                                        return $ TSchema l (subst s (subst ren q)) (subst s (subst ren t))
        p1 = do
            substitute (TV KType (name "B")) tSelf
            msubst t
        p2 = do
            substitute (TV KType (name "A")) tSelf
            msubst t
        p3 = do
            substitute (TV KType (name "B")) (tVar (TV KType (name "A")))
            msubst t
        r1 = do
            substitute (TV KType (name "B")) tSelf
            msubst' t
        r2 = do
            substitute (TV KType (name "A")) tSelf
            msubst' t
        r3 = do
            substitute (TV KType (name "B")) (tVar (TV KType (name "A")))
            msubst' t




instance Subst TVar where
    msubst v                        = do t <- msubst (TVar NoLoc v)
                                         case t of
                                            TVar _ v' -> return v'
                                            _         -> return v
    tyfree v                        = [v]

instance Subst TCon where
    msubst (TC n ts)                = TC n <$> msubst ts
    tyfree (TC n ts)                = tyfree ts

instance Subst QBind where
    msubst (Quant v cs)             = Quant <$> msubst v <*> msubst cs
    tyfree (Quant v cs)             = v : tyfree cs
    tybound (Quant v cs)            = [v]

instance Subst Type where
    msubst (TVar l v)               = do s <- getSubstitution
                                         case Map.lookup v s of
                                            Just t ->  msubst t
                                            Nothing -> return (TVar l v)
    msubst (TCon l c)               = TCon l <$> msubst c
    msubst (TFun l fx p k t)        = TFun l <$> msubst fx <*> msubst p <*> msubst k <*> msubst t
    msubst (TTuple l p k)           = TTuple l <$> msubst p <*> msubst k
    msubst (TOpt l t)               = TOpt l <$> msubst t
    msubst (TNone l)                = return $ TNone l
    msubst (TWild l)                = return $ TWild l
    msubst (TNil l s)               = return $ TNil l s
    msubst (TRow l k n t r)         = TRow l k n <$> msubst t <*> msubst r
    msubst (TStar l k r)            = TStar l k <$> msubst r
    msubst (TFX l fx)               = return $ TFX l fx

    tyfree (TVar _ v)               = [v]
    tyfree (TCon _ c)               = tyfree c
    tyfree (TFun _ fx p k t)        = tyfree fx ++ tyfree p ++ tyfree k ++ tyfree t
    tyfree (TTuple _ p k)           = tyfree p ++ tyfree k
    tyfree (TOpt _ t)               = tyfree t
    tyfree (TNone _)                = []
    tyfree (TWild _)                = []
    tyfree (TNil _ _)               = []
    tyfree (TRow _ _ _ t r)         = tyfree t ++ tyfree r
    tyfree (TStar _ _ r)            = tyfree r
    tyfree (TFX l fx)               = []


instance Subst PosPar where
    msubst (PosPar n t e p)         = PosPar n <$> msubst t <*> msubst e <*> msubst p
    msubst (PosSTAR n t)            = PosSTAR n <$> msubst t
    msubst PosNIL                   = return PosNIL

    tyfree (PosPar n t e p)         = tyfree t ++ tyfree p
    tyfree (PosSTAR n t)            = tyfree t
    tyfree PosNIL                   = []

instance Subst KwdPar where
    msubst (KwdPar n t e p)         = KwdPar n <$> msubst t <*> msubst e <*> msubst p
    msubst (KwdSTAR n t)            = KwdSTAR n <$> msubst t
    msubst KwdNIL                   = return KwdNIL

    tyfree (KwdPar n t e p)         = tyfree t ++ tyfree p
    tyfree (KwdSTAR n t)            = tyfree t
    tyfree KwdNIL                   = []

instance Subst Decl where
    msubst (Def l n q p k a ss de fx doc)   = Def l n <$> msubst q <*> msubst p <*> msubst k <*> msubst a <*> msubst ss <*> return de <*> msubst fx <*> return doc
    msubst (Actor l n q p k ss doc)         = Actor l n <$> msubst q <*> msubst p <*> msubst k <*> msubst ss <*> return doc
    msubst (Class l n q bs ss doc)          = Class l n <$> msubst q <*> msubst bs <*> msubst ss <*> return doc
    msubst (Protocol l n q bs ss doc)       = Protocol l n <$> msubst q <*> msubst bs <*> msubst ss <*> return doc
    msubst (Extension l q c bs ss doc)      = Extension l <$> msubst q <*> msubst c <*> msubst bs <*> msubst ss <*> return doc
    {-
    msubst d@(Protocol l n q bs ss)     = do (s,ren) <- msubstRenaming d
                                             return $ Protocol l n (subst s (subst ren q)) (subst s (subst ren bs)) (subst s (subst ren ss))
    msubst d@(Class l n q bs ss)        = do (s,ren) <- msubstRenaming d
                                             return $ Class l n (subst s (subst ren q)) (subst s (subst ren bs)) (subst s (subst ren ss))
    msubst d@(Extension l q c bs ss)    = do (s,ren) <- msubstRenaming d
                                             return $ Extension l (subst s (subst ren q)) (subst s (subst ren c)) (subst s (subst ren bs)) (subst s (subst ren ss))
    msubst d@(Def l n q p k a ss de fx) = do (s,ren) <- msubstRenaming d
                                             return $ Def l n (subst s (subst ren q)) (subst s (subst ren p)) (subst s (subst ren k))
                                                              (subst s (subst ren a)) (subst s (subst ren ss)) de (subst s fx)
    msubst d@(Actor l n q p k ss)       = do (s,ren) <- msubstRenaming d
                                             return $ Actor l n (subst s (subst ren q)) (subst s (subst ren p)) (subst s (subst ren k))
                                                                (subst s (subst ren ss))
    -}
    tybound (Protocol l n q ps b _)   = tvSelf : tybound q
    tybound (Class l n q ps b _)      = tvSelf : tybound q
    tybound (Extension l q c ps b _)  = tvSelf : tybound q
    tybound (Def l n q p k t b d x _) = tybound q
    tybound (Actor l n q p k b _)     = tybound q

    tyfree (Protocol l n q ps b _)   = nub (tyfree q ++ tyfree ps ++ tyfree b) \\ (tvSelf : tybound q)
    tyfree (Class l n q ps b _)      = nub (tyfree q ++ tyfree ps ++ tyfree b) \\ (tvSelf : tybound q)
    tyfree (Extension l q c ps b _)  = nub (tyfree q ++ tyfree c ++ tyfree ps ++ tyfree b) \\ (tvSelf : tybound q)
    tyfree (Def l n q p k t b d x _) = nub (tyfree q ++ tyfree p ++ tyfree k ++ tyfree b ++ tyfree t ++ tyfree x) \\ tybound q
    tyfree (Actor l n q p k b _)     = nub (tyfree q ++ tyfree p ++ tyfree k ++ tyfree b) \\ (tybound q)

instance Subst Stmt where
    msubst (Expr l e)               = Expr l <$> msubst e
    msubst (Assign l ps e)          = Assign l <$> msubst ps <*> msubst e
    msubst (MutAssign l t e)        = MutAssign l <$> msubst t <*> msubst e
    msubst (AugAssign l t op e)     = AugAssign l <$> msubst t <*> return op <*> msubst e
    msubst (Assert l e mbe)         = Assert l <$> msubst e <*> msubst mbe
    msubst (Delete l t)             = Delete l <$> msubst t
    msubst (Return l mbe)           = Return l <$> msubst mbe
    msubst (Raise l e)              = Raise l <$> msubst e
    msubst (If l bs els)            = If l <$> msubst bs <*> msubst els
    msubst (While l e b els)        = While l <$> msubst e <*> msubst b <*> msubst els
    msubst (For l p e b els)        = For l <$> msubst p <*> msubst e <*> msubst b <*> msubst els
    msubst (Try l b hs els fin)     = Try l <$> msubst b <*> msubst hs <*> msubst els <*> msubst fin
    msubst (With l is b)            = With l <$> msubst is <*> msubst b
    msubst (VarAssign l ps e)       = VarAssign l <$> msubst ps <*> msubst e
    msubst (After l e e')           = After l <$> msubst e <*> msubst e'
    msubst (Decl l ds)              = Decl l <$> msubst ds
    msubst (Signature l ns tsc d)   = Signature l ns <$> msubst tsc <*> return d
    msubst s                        = return s

    tyfree (Expr l e)               = tyfree e
    tyfree (Assign l ps e)          = tyfree ps ++ tyfree e
    tyfree (MutAssign l t e)        = tyfree t ++ tyfree e
    tyfree (AugAssign l t op e)     = tyfree t ++ tyfree e
    tyfree (Assert l e mbe)         = tyfree mbe
    tyfree (Delete l t)             = tyfree t
    tyfree (Return l mbe)           = tyfree mbe
    tyfree (Raise l e)              = tyfree e
    tyfree (If l bs els)            = tyfree bs ++ tyfree els
    tyfree (While l e b els)        = tyfree e ++ tyfree b ++ tyfree els
    tyfree (For l p e b els)        = tyfree p ++ tyfree e ++ tyfree b ++ tyfree els
    tyfree (Try l b hs els fin)     = tyfree b ++ tyfree hs ++ tyfree els ++ tyfree fin
    tyfree (With l is b)            = tyfree is ++ tyfree b
    tyfree (VarAssign l ps e)       = tyfree ps ++ tyfree e
    tyfree (After l e e')           = tyfree e ++ tyfree e'
    tyfree (Decl l ds)              = tyfree ds
    tyfree (Signature l ns tsc d)   = tyfree tsc
    tyfree s                        = []

instance Subst Expr where
    msubst (Call l e p k)           = Call l <$> msubst e <*> msubst p <*> msubst k
    msubst (TApp l e ts)            = TApp l <$> msubst e <*> msubst ts
    msubst (Async l e)              = Async l <$> msubst e
    msubst (Await l e)              = Await l <$> msubst e
    msubst (Index l e ix)           = Index l <$> msubst e <*> msubst ix
    msubst (Slice l e sl)           = Slice l <$> msubst e <*> msubst sl
    msubst (NDSlice l e sl)         = NDSlice l <$> msubst e <*> msubst sl
    msubst (Cond l e1 cond e2)      = Cond l <$> msubst e1 <*> msubst cond <*> msubst e2
    msubst (IsInstance l e c)       = IsInstance l <$> msubst e <*> return c
    msubst (BinOp l e1 op e2)       = BinOp l <$> msubst e1 <*> return op <*> msubst e2
    msubst (CompOp l e ops)         = CompOp l <$> msubst e <*> msubst ops
    msubst (UnOp l op e)            = UnOp l op <$> msubst e
    msubst (Dot l e n)              = Dot l <$> msubst e <*> return n
    msubst (Rest l e n)             = Rest l <$> msubst e <*> return n
    msubst (DotI l e i)             = DotI l <$> msubst e <*> return i
    msubst (RestI l e i)            = RestI l <$> msubst e <*> return i
    msubst (Lambda l p k e fx)      = Lambda l <$> msubst p <*> msubst k <*> msubst e <*> msubst fx
    msubst (Yield l e)              = Yield l <$> msubst e
    msubst (YieldFrom l e)          = YieldFrom l <$> msubst e
    msubst (Tuple l p k)            = Tuple l <$> msubst p <*> msubst k
    msubst (List l es)              = List l <$> msubst es
    msubst (ListComp l e c)         = ListComp l <$> msubst e <*> msubst c
    msubst (Dict l as)              = Dict l <$> msubst as
    msubst (DictComp l a c)         = DictComp l <$> msubst a <*> msubst c
    msubst (Set l es)               = Set l <$> msubst es
    msubst (SetComp l e c)          = SetComp l <$> msubst e <*> msubst c
    msubst (Paren l e)              = Paren l <$> msubst e
    msubst e                        = return e

    tyfree (Call l e p k)           = tyfree e ++ tyfree p ++ tyfree k
    tyfree (TApp l e ts)            = tyfree e ++ tyfree ts
    tyfree (Async l e)              = tyfree e
    tyfree (Await l e)              = tyfree e
    tyfree (Index l e ix)           = tyfree e ++ tyfree ix
    tyfree (Slice l e sl)           = tyfree e ++ tyfree sl
    tyfree (NDSlice l e sl)         = tyfree e ++ tyfree sl
    tyfree (Cond l e1 cond e2)      = tyfree e1 ++ tyfree cond ++ tyfree e2
    tyfree (IsInstance l e c)       = tyfree e
    tyfree (BinOp l e1 op e2)       = tyfree e1 ++ tyfree e2
    tyfree (CompOp l e ops)         = tyfree e ++ tyfree ops
    tyfree (UnOp l op e)            = tyfree e
    tyfree (Dot l e n)              = tyfree e
    tyfree (Rest l e n)             = tyfree e
    tyfree (DotI l e i)             = tyfree e
    tyfree (RestI l e i)            = tyfree e
    tyfree (Lambda l p k e fx)      = tyfree p ++ tyfree k ++ tyfree e ++ tyfree fx
    tyfree (Yield l e)              = tyfree e
    tyfree (YieldFrom l e)          = tyfree e
    tyfree (Tuple l p k)            = tyfree p ++ tyfree k
    tyfree (List l es)              = tyfree es
    tyfree (ListComp l e c)         = tyfree e ++ tyfree c
    tyfree (Dict l as)              = tyfree as
    tyfree (DictComp l a c)         = tyfree a ++ tyfree c
    tyfree (Set l es)               = tyfree es
    tyfree (SetComp l e c)          = tyfree e ++ tyfree c
    tyfree (Paren l e)              = tyfree e
    tyfree e                        = []

instance Subst Branch where
    msubst (Branch e b)             = Branch <$> msubst e <*> msubst b

    tyfree (Branch e b)             = tyfree e ++ tyfree b

instance Subst Pattern where
    msubst (PWild l t)              = PWild l <$> msubst t
    msubst (PVar l n t)             = PVar l n <$> msubst t
    msubst (PParen l p)             = PParen l <$> msubst p
    msubst (PTuple l p k)           = PTuple l <$> msubst p <*> msubst k
    msubst (PList l ps p)           = PList l <$> msubst ps <*> msubst p

    tyfree (PWild _ t)              = tyfree t
    tyfree (PVar _ n t)             = tyfree t
    tyfree (PParen _ p)             = tyfree p
    tyfree (PTuple _ p k)           = tyfree p ++ tyfree k
    tyfree (PList _ ps p)           = tyfree ps ++ tyfree p

instance Subst PosPat where
    msubst (PosPat p pp)            = PosPat <$> msubst p <*> msubst pp
    msubst (PosPatStar p)           = PosPatStar <$> msubst p
    msubst PosPatNil                = return PosPatNil

    tyfree (PosPat p pp)            = tyfree p ++ tyfree pp
    tyfree (PosPatStar p)           = tyfree p
    tyfree PosPatNil                = []

instance Subst KwdPat where
    msubst (KwdPat n p kp)          = KwdPat n <$> msubst p <*> msubst kp
    msubst (KwdPatStar p)           = KwdPatStar <$> msubst p
    msubst KwdPatNil                = return KwdPatNil

    tyfree (KwdPat n p kp)          = tyfree p ++ tyfree kp
    tyfree (KwdPatStar p)           = tyfree p
    tyfree KwdPatNil                = []

instance Subst Handler where
    msubst (Handler ex b)           = Handler ex <$> msubst b

    tyfree (Handler ex b)           = tyfree b

instance Subst WithItem where
    msubst (WithItem e p)           = WithItem <$> msubst e <*> msubst p

    tyfree (WithItem e p)           = tyfree e ++ tyfree p

instance Subst PosArg where
    msubst (PosArg e p)             = PosArg <$> msubst e <*> msubst p
    msubst (PosStar e)              = PosStar <$> msubst e
    msubst PosNil                   = return PosNil

    tyfree (PosArg e p)             = tyfree e ++ tyfree p
    tyfree (PosStar e)              = tyfree e
    tyfree PosNil                   = []

instance Subst KwdArg where
    msubst (KwdArg n e k)           = KwdArg n <$> msubst e <*> msubst k
    msubst (KwdStar e)              = KwdStar <$> msubst e
    msubst KwdNil                   = return KwdNil

    tyfree (KwdArg n e k)           = tyfree e ++ tyfree k
    tyfree (KwdStar e)              = tyfree e
    tyfree KwdNil                   = []

instance Subst Sliz where
    msubst (Sliz l e1 e2 e3)        = Sliz l <$> msubst e1 <*> msubst e2 <*> msubst e3

    tyfree (Sliz _ e1 e2 e3)        = tyfree e1 ++ tyfree e2 ++ tyfree e3

instance Subst NDSliz where
    msubst (NDExpr e)               = NDExpr <$> msubst e
    msubst (NDSliz s)               = NDSliz <$> msubst s

    tyfree (NDExpr e)               = tyfree e
    tyfree (NDSliz s)               = tyfree s



instance Subst OpArg where
    msubst (OpArg op e)             = OpArg op <$> msubst e

    tyfree (OpArg op e)             = tyfree e

instance Subst Elem where
    msubst (Elem e)                 = Elem <$> msubst e
    msubst (Star e)                 = Star <$> msubst e

    tyfree (Elem e)                 = tyfree e
    tyfree (Star e)                 = tyfree e

instance Subst Assoc where
    msubst (Assoc k v)              = Assoc <$> msubst k <*> msubst v
    msubst (StarStar e)             = StarStar <$> msubst e

    tyfree (Assoc k v)              = tyfree k ++ tyfree v
    tyfree (StarStar e)             = tyfree e

instance Subst Comp where
    msubst (CompFor l p e c)        = CompFor l <$> msubst p <*> msubst e <*> msubst c
    msubst (CompIf l e c)           = CompIf l <$> msubst e <*> msubst c
    msubst NoComp                   = return NoComp

    tyfree (CompFor _ p e c)        = tyfree p ++ tyfree e ++ tyfree c
    tyfree (CompIf _ e c)           = tyfree e ++ tyfree c
    tyfree NoComp                   = []

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
  where vs                          = tyfree x

covariant                           = [qnSetT,qnDict,qnList]                -- Ignore mutation for now!

instance Polarity TCon where
    polvars (TC c ts)
      | c `elem` covariant          = (vs,[])
      | otherwise                   = (vs,vs)
      where vs                      = tyfree ts

instance Polarity QBind where
    polvars (Quant v cs)            = (vs,vs) where vs = v : tyfree cs

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
