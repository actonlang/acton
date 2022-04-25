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

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Acton.Transform where

import Utils
import Acton.Syntax
import Acton.Names
import Acton.Prim
import Acton.Printer


termred                                 :: Suite -> Suite
termred b                               = -- trace ("### termred:\n" ++ render (nest 4 $ vcat $ map pretty b)) $
                                          wtrans env0 b

termsubst                               :: (Transform a) => [(Name,Expr)] -> a -> a
termsubst [] x                          = x
termsubst s x                           = trans (extsubst s env0) x

class Transform a where
    trans                               :: TransEnv -> a -> a

data TransEnv                           = TransEnv { trsubst :: [(Name,Maybe Expr)], witscope :: [(Name,Type,Expr)] }

env0                                    = TransEnv{ trsubst = [], witscope = [] }

blockscope ns env                       = env{ trsubst = (ns `zip` repeat Nothing) ++ trsubst env }

extsubst ns env                         = env{ trsubst = [ (n,Just e) | (n,e) <- ns ] ++ trsubst env }

extscope n t e env                      = env{ witscope = (n,t,e) : witscope env }

trfind n env                            = case lookup n (trsubst env) of
                                            Just (Just e) -> Just e
                                            _ -> Nothing

instance Pretty (Name,Expr) where
    pretty (n,e)                        = pretty n <+> text "~" <+> pretty e

wtrans env (Assign l p@[PVar _ n (Just t)] e : ss)
  | Lambda{} <- e                       = wtrans (extsubst [(n,e1)] env) ss
  | Var{} <- e                          = wtrans (extsubst [(n,e1)] env) ss
  | Dot _ Var{} _ <- e                  = wtrans (extsubst [(n,e1)] env) ss
  | not $ null hits                     = wtrans (extsubst [(n,head hits)] env) ss
  | Internal Witness _ _ <- n           = Assign l p e1 : wtrans (extscope n t e1 env) ss
  where hits                            = [ eVar n' | (n',t',e') <- witscope env, e' == e1, t' == t ]
        e1                              = trans env e
wtrans env ss                           = trans env ss
                                

instance (Transform a) => Transform [a] where
    trans env                           = map (trans env)

instance (Transform a) => Transform (Maybe a) where
    trans env                           = fmap (trans env)

instance Transform Stmt where
    trans env (Assign l ps e)           = Assign l ps (trans env e)
    trans env (Expr l e)                = Expr l (trans env e)
    trans env (MutAssign l t e)         = MutAssign l (trans env t) (trans env e)
    trans env (AugAssign l t op e)      = AugAssign l (trans env t) op (trans env e)
    trans env (Assert l e mbe)          = Assert l (trans env e) (trans env mbe)
    trans env (Delete l t)              = Delete l (trans env t)
    trans env (Return l mbe)            = Return l (trans env mbe)
    trans env (Raise l mbex)            = Raise l (trans env mbex)
    trans env (If l bs els)             = If l (trans env bs) (trans env els)
    trans env (While l e b els)         = While l (trans env e) (trans env b) (trans env els)
    trans env (For l p e b els)         = For l p (trans env e) (trans env1 b) (trans env els)
      where env1                        = blockscope (bound p) env
    trans env (Try l b hs els fin)      = Try l (trans env b) (trans env hs) (trans env els) (trans env fin)
    trans env (With l is b)             = With l (trans env1 is) (wtrans env1 b)
      where env1                        = blockscope (bound is) env
    trans env (Data l mbp ss)           = Data l mbp (trans env ss)
    trans env (VarAssign l ps e)        = VarAssign l ps (trans env e)
    trans env (After l e e')            = After l (trans env e) (trans env e')
    trans env (Decl l ds)               = Decl l (trans env ds)
    trans env s                         = s

instance Transform Decl where
    trans env (Def l n q p k t b d fx)  = Def l n q (trans env1 p) (trans env1 k) t (wtrans env1 b) d fx
      where env1                        = blockscope (bound q ++ bound p ++ bound k) env
    trans env (Actor l n q p k b)       = Actor l n q (trans env1 p) (trans env1 k) (wtrans env1 b)
      where env1                        = blockscope (bound q ++ bound p ++ bound k) env
    trans env (Class l n q us b)        = Class l n q us (wtrans env1 b)
      where env1                        = blockscope (bound q) env
    trans env (Protocol l n q us b)     = Protocol l n q us (wtrans env1 b)
      where env1                        = blockscope (bound q) env
    trans env (Extension l n q us b)    = Extension l n q us (wtrans env1 b)
      where env1                        = blockscope (bound q) env

instance Transform Expr where
    trans env (Var l (NoQ n))
      | Just e <- trfind n env          = trans (blockscope [n] env) e

    trans env e0@(Call l e p k)
      | Lambda{} <- e',
        Just s1 <- pzip (ppar e') p',
        Just s2 <-  kzip (kpar e') k'   = termsubst (s1++s2) (exp1 e')
      | otherwise                       = Call l e' p' k'
      where e'                          = trans env e
            p'                          = trans env p
            k'                          = trans env k
    trans env (TApp l e ts)             = TApp l (trans env e) ts
    trans env (Async l e)               = Async l (trans env e)
    trans env (Await l e)               = Await l (trans env e)
    trans env (Index l e is)            = Index l (trans env e) (trans env is)
    trans env (Slice l e sl)            = Slice l (trans env e) (trans env sl)
    trans env (NDSlice l e sl)          = NDSlice l (trans env e) (trans env sl)
    trans env (Cond l e1 e2 e3)         = Cond l (trans env e1) (trans env e2) (trans env e3)
    trans env (IsInstance l e c)        = IsInstance l (trans env e) c
    trans env (BinOp l e1 op e2)        = BinOp l (trans env e1) op (trans env e2)
    trans env (CompOp l e ops)          = CompOp l (trans env e) (trans env ops)
    trans env (UnOp l op e)             = UnOp l op (trans env e)
    trans env (Dot l e n)
      | Tuple{} <- e'                   = trans env (kwditem n $ kargs e')
      | otherwise                       = Dot l e' n
      where e'                          = trans env e
    trans env (Rest l e n)
      | Tuple{} <- e'                   = Tuple NoLoc PosNil (kwdrest n $ kargs e')
      | otherwise                       = Rest l e' n
      where e'                          = trans env e
    trans env (DotI l e i)
      | Tuple{} <- e'                   = trans env (positem i $ pargs e')
      | otherwise                       = DotI l e' i
      where e'                          = trans env e
    trans env (RestI l e i)
      | Tuple{} <- e'                   = Tuple NoLoc (posrest i $ pargs e') KwdNil
      | otherwise                       = RestI l e' i
      where e'                          = trans env e
    trans env e0@(Lambda l p k e fx)
      | null clash                      = eta $ Lambda l (trans env1 p) (trans env1 k) (trans env1 e) fx
      | otherwise                       = eta $ Lambda l (trans env1 $ prename s p) (trans env1 $ krename s k) (trans env1 $ erename s e) fx
      where env1                        = extsubst (psubst p ++ ksubst k) env
            fvs                         = free e
            bvs                         = bound p ++ bound k
            clash                       = bvs `intersect` free (rng $ restrict (trsubst env) fvs)
            s                           = clash `zip` (yNames \\ (fvs++bvs))
            e1                          = Lambda l (prename s p) (krename s k) (erename s e) fx
    trans env (Yield l e)               = Yield l (trans env e)
    trans env (YieldFrom l e)           = YieldFrom l (trans env e)
    trans env e@(Tuple l p k)
      | PosStar x <- p',
        KwdStar y <- k',
        x == y                          = x
      | PosArg (DotI _ x 0) r <- p',
        PosStar (RestI _ y 0) <- r,
        KwdNil <- k',
        x == y                          = x
      | KwdArg n (Dot _ x a) r <- k',
        KwdStar (Rest _ y b) <- r,
        PosNil <- p',
        n == a, n == b, x == y          = x
      | otherwise                       = Tuple l p' k'
      where p'                          = trans env p
            k'                          = trans env k
            e'                          = Tuple l p' k'
    trans env (List l es)               = List l (trans env es)
    trans env (ListComp l e c)          = ListComp l (trans env1 e) (trans env1 c)
      where env1                        = blockscope (bound c) env
    trans env (Dict l as)               = Dict l (trans env as)
    trans env (DictComp l a c)          = DictComp l (trans env1 a) (trans env1 c)
      where env1                        = blockscope (bound c) env
    trans env (Set l es)                = Set l (trans env es)
    trans env (SetComp l e c)           = SetComp l (trans env1 e) (trans env1 c)
      where env1                        = blockscope (bound c) env
--    trans env (Paren l e)               = Paren l (trans env e)
    trans env (Paren l e)               = trans env e
    trans env e                         = e

eta (Lambda _ p k (Call _ e p' k') fx)
  | eq1 p p' && eq2 k k'                = e
  where
    eq1 (PosPar n _ _ p) (PosArg e p')  = eVar n == e && eq1 p p'
    eq1 (PosSTAR n _) (PosStar e)       = eVar n == e
    eq1 PosNIL PosNil                   = True
    eq1 _ _                             = False
    eq2 (KwdPar n _ _ k) (KwdArg _ e k')= eVar n == e && eq2 k k'                       -- Requires perfectly sorted args, which the type-checker produces
    eq2 (KwdSTAR n _) (KwdStar e)       = eVar n == e
    eq2 KwdNIL KwdNil                   = True
    eq2 _ _                             = False
eta e                                   = e

pzip (PosPar n _ _ p) (PosArg e a)      = do p' <- pzip p a; return $ (n, e) : p'
pzip (PosSTAR n _) a                    = Just [(n, Tuple NoLoc a KwdNil)]
pzip PosNIL _                           = Just []
pzip _ _                                = Nothing

kzip (KwdPar n _ _ k) (KwdArg _ e a)    = do k' <- kzip k a; return $ (n, e) : k'       -- Requires perfectly sorted args, which the type-checker produces
kzip (KwdSTAR n _) a                    = Just [(n, Tuple NoLoc PosNil a)]
kzip KwdNIL _                           = Just []
kzip _ _                                = Nothing

positem 0 (PosArg e _)                  = e
positem i (PosArg _ p)                  = positem (i-1) p

kwditem n (KwdArg n' e _) | n == n'     = e
kwditem n (KwdArg _ _ k)                = kwditem n k

posrest 0 (PosArg _ p)                  = p
posrest i (PosArg e p)                  = PosArg e (posrest (i-1) p)
--posrest i p                             = PosStar $ RestI l0 (Tuple l0 p KwdNil) i

kwdrest n (KwdArg n' e k) | n == n'     = k
kwdrest n (KwdArg n' e k)               = KwdArg n' e (kwdrest n k)


prename s (PosPar n t e p)              = PosPar (rename s n) t e (prename s p)
prename s (PosSTAR n t)                 = PosSTAR (rename s n) t
prename s p                             = p

krename s (KwdPar n t e k)              = KwdPar (rename s n) t e (krename s k)
krename s (KwdSTAR n t)                 = KwdSTAR (rename s n) t
krename s k                             = k

rename s n                              = case lookup n s of
                                            Just n' -> n'
                                            _ -> n

erename s e                             = termsubst [ (n, eVar n') | (n,n') <- s ] e

yNames                                  = paramNames "y"
pNames                                  = paramNames "p"
kNames                                  = paramNames "k"

instance Transform Exception where
    trans env (Exception e mbe)         = Exception (trans env e) (trans env mbe)

instance Transform Branch where
    trans env (Branch e ss)             = Branch (trans env e) (trans env ss)

instance Transform Handler where
    trans env (Handler ex b)            = Handler ex (trans env1 b)
      where env1                        = blockscope (bound ex) env

instance Transform PosPar where
    trans env (PosPar n t e p)          = PosPar n t (trans env e) (trans env p)
    trans env (PosSTAR n (Just t))
      | Internal{} <- n,
        TTuple _ p _ <- t               = pPar pNames p
    trans env p                         = p

psubst (PosPar _ _ _ p)                 = psubst p
psubst (PosSTAR n (Just t))
  | Internal{} <- n,
    TTuple _ p _ <- t                   = [(n,Tuple l0 (pArg $ pPar pNames p) KwdNil)]
psubst _                                = []
    
instance Transform KwdPar where
    trans env (KwdPar n t e k)          = KwdPar n t (trans env e) (trans env k)
    trans env (KwdSTAR n (Just t))
      | Internal{} <- n,
        TTuple _ _ k <- t               = kPar kNames k
    trans env k                         = k

ksubst (KwdPar _ _ _ k)                 = ksubst k
ksubst (KwdSTAR n (Just t))
  | Internal{} <- n,
    TTuple _ _ k <- t                   = [(n,Tuple l0 PosNil (kArg $ kPar kNames k))]
ksubst _                                = []

instance Transform PosArg where
    trans env (PosArg e p)              = PosArg (trans env e) (trans env p)
    trans env (PosStar e)
      | Tuple _ p _ <- e'               = p
      | Paren _ (Tuple _ p _) <- e'     = p
      where e'                          = trans env e
    trans env (PosStar e)               = PosStar (trans env e)
    trans env PosNil                    = PosNil
    
instance Transform KwdArg where
    trans env (KwdArg n e k)            = KwdArg n (trans env e) (trans env k)
    trans env (KwdStar e)
      | Tuple _ _ k <- e'               = k
      | Paren _ (Tuple _ _ k) <- e'     = k
      where e'                          = trans env e
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

instance Transform NDSliz where
    trans env (NDExpr e)                = NDExpr (trans env e) 
    trans env (NDSliz s)                = NDSliz (trans env s)
