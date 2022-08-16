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
module Acton.Deactorizer where

import Acton.Syntax
import Acton.Names
import Acton.Subst
import Acton.Prim
import Acton.Builtin
import Acton.QuickType
import Acton.Env
import Acton.Transform
import Utils
import Pretty
import Control.Monad.State.Strict

deactorize                          :: Env0 -> Module -> IO (Module, Env0)
deactorize env0 (Module m imps b)   = return (Module m imps (runDeactM $ deactTop env b), mapModules conv env1)
  where env                         = deactEnv env1
        env1                        = mapModules1 sealActors env0

deactTop env []                     = return []
deactTop env (s : ss)               = do s' <- deact env s
                                         ss' <- deactTop env1 ss
                                         return (s' : ss')
  where env1                        = extend (map sealActors $ envOf s) env


-- Deactorizing monad
type DeactM a                       = State DeactState a

type DeactState                     = [Int]

runDeactM                           :: DeactM a -> a
runDeactM m                         = evalState m [1..]

type DeactEnv                       = EnvF DeactX

data DeactX                         = DeactX { actionsX :: [Name], stvarsX :: [Name], localsX :: [Name], sampledX :: [Name], retX :: Maybe Type }

deactEnv                            :: Env0 -> DeactEnv
deactEnv env0                       = setX env0 DeactX{ actionsX = [], stvarsX = [], localsX = [], sampledX = [], retX = Nothing }

extend                              :: TEnv -> DeactEnv -> DeactEnv
extend te env                       = define te env

extendAndShadow                     :: TEnv -> DeactEnv -> DeactEnv
extendAndShadow te env              = modX (define te env) $ \x -> x{ actionsX = actions env \\ ns, localsX = locals env \\ ns }
  where ns                          = dom te

newName                             :: String -> DeactM Name
newName s                           = state (\(uniq:supply) -> (Internal DeactPass s uniq, supply))



actions env                         = actionsX $ envX env

stvars env                          = stvarsX $ envX env

locals env                          = localsX $ envX env

sampled env                         = sampledX $ envX env

ret env                             = fromJust $ retX $ envX env

setActor acts stvars locals env     = modX env $ \x -> x{ actionsX = acts, stvarsX = stvars, localsX = locals, sampledX = [] }

setSampled ns env                   = modX env $ \x -> x{ sampledX = ns ++ sampled env }

clearSampled env                    = modX env $ \x -> x{ sampledX = [] }

setRet t env                        = modX env $ \x -> x{ retX = Just t }


-- Actor interface sealing --------------------------------------------------------------------------------

sealActors (n, NAct q p k te)       = (n, NAct (seal q) (seal p) (seal k) (seal te))
sealActors ni                       = ni

class Seal a where
    seal                            :: a -> a

instance (Seal a) => Seal [a] where
    seal                            = map seal

instance Seal (Name, NameInfo) where
    seal (n, i)                     = (n, seal i)

instance Seal NameInfo where
    seal (NSig sc dec)              = NSig (seal sc) dec
    seal (NDef sc dec)              = NDef (seal sc) dec
    seal (NVar t)                   = NVar (seal t)
    seal i                          = i

instance Seal QBind where
    seal (Quant c ps)               = Quant c (seal ps)

instance Seal TSchema where
    seal (TSchema l q t)            = TSchema l (seal q) (seal t)

instance Seal Type where
    seal (TVar l v)                 = TVar l v
    seal (TCon l c)                 = TCon l $ seal c
    seal (TFun l fx p k t)
      | fx == fxProc                = TFun l (seal fx) (seal p) (seal k) (tMsg (seal t))
      | otherwise                   = TFun l (seal fx) (seal p) (seal k) (seal t)
    seal (TTuple l p k)             = TTuple l (seal p) (seal k)
    seal (TOpt l t)                 = TOpt l (seal t)
    seal (TNone l)                  = TNone l
    seal (TWild l)                  = TWild l
    seal (TNil l k)                 = TNil l k
    seal (TRow l k n t r)           = TRow l k n (seal t) (seal r)
    seal (TFX l fx)                 = TFX l (seal fx)

instance Seal TCon where
    seal (TC c ts)                  = TC c (seal ts)

instance Seal FX where
    seal FXMut                      = FXMut
    seal FXPure                     = FXPure
    seal FXProc                     = FXAction         -- the sealing essence!
    seal FXAction                   = FXAction

instance Seal PosPar where
    seal (PosPar n (Just t) _ p)    = PosPar n (Just $ seal t) Nothing (seal p)
    seal (PosSTAR n (Just t))       = PosSTAR n (Just $ seal t)
    seal PosNIL                     = PosNIL


-- Deactorize actor declarations -----------------------------------------------------------------------

class Deact a where
    deact                           :: DeactEnv -> a -> DeactM a

instance Deact a => Deact (Maybe a) where
    deact env                       = traverse (deact env)

instance Deact a => Deact [a] where
    deact env                       = traverse (deact env)

deactSuite env []                   = return []
deactSuite env (s : ss)             = do s' <- deact (setSampled ns env) s
                                         ss' <- deactSuite env1 ss
                                         return (samples ++ s' : ss')
  where env1                        = extend (map sealActors $ envOf s) env
        ns                          = nub $ stvars env `intersect` lamfree s
        samples                     = [ sAssign (pVar n $ typeOf env (eVar n)) (eDot (eVar selfKW) n) | n <- ns ]


instance Deact Stmt where
    deact env s@(Expr _ (NotImplemented _))
                                    = return s
    deact env (Expr l e)            = Expr l <$> deactExpTop env e
    deact env (Assign l [p@(PVar _ n _)] e)
      | n `elem` locals env         = MutAssign l (selfRef n) <$> deactExp env t e
      | otherwise                   = Assign l [p] <$> deactExp env t e
      where t                       = typeOf env p
    deact env (MutAssign l tg e)    = MutAssign l <$> deactExp env tWild tg <*> deactExp env t e
      where t                       = typeOf env tg
    deact env (Pass l)              = return $ Pass l
    deact env (Return l Nothing)    = return $ Return l Nothing
    deact env (Return l (Just e))   = Return l . Just <$> deactExp env (ret env) e
    deact env (Break l)             = return $ Break l
    deact env (Continue l)          = return $ Continue l
    deact env (If l bs els)         = If l <$> deact env bs <*> deactSuite env1 els
      where env1                    = clearSampled env
    deact env (While l e b els)     = While l <$> deactExp env tWild e <*> deactSuite env1 b <*> deactSuite env1 els
      where env1                    = clearSampled env
    deact env (Try l b hs els fin)  = Try l <$> deactSuite env b <*> deact env hs <*> deactSuite env els <*> deactSuite env fin
    deact env (VarAssign l [p@(PVar _ n _)] e)
                                    = MutAssign l (selfRef n) <$> deactExp env t e
      where t                       = typeOf env p
    deact env (After l e1 e2)       = do delta <- deactExp env tFloat e1
                                         lambda <- deactExp env t $ Lambda l0 PosNIL KwdNIL e2 fxProc
                                         return $ Expr l $ Call l0 (tApp (eQVar primAFTERf) [t2]) (PosArg delta $ PosArg lambda PosNil) KwdNil
      where t2                      = typeOf env e2
            t                       = tFun fxProc posNil kwdNil t2
    deact env (Decl l ds)           = do ds1 <- deact env1 ds
                                         return $ Decl l $ ds1 ++ [ newact env1 n q p | Actor _ n q p _ _ <- ds, not $ abstractActor env1 (NoQ n) ]
      where env1                    = extend (map sealActors $ envOf ds) env
    deact env (Signature l ns t d)  = return $ Signature l ns t d
    deact env s                     = error ("deact unexpected stmt: " ++ prstr s)

instance Deact Decl where
    deact env (Actor l n q params KwdNIL body)
                                    = do --traceM ("### deact actor " ++ prstr n)
                                         inits' <- deactSuite env2 inits
                                         decls' <- mapM deactMeths decls
                                         let _init_ = Def l0 initKW [] (addSelfPar params') KwdNIL (Just tNone) (mkBody $ copies++inits') NoDec fxProc
                                         return $ Class l n q [TC primActor [], cValue] (propsigs ++ [Decl l0 [_init_]] ++ decls' ++ wrapped)
      where env1                    = setActor actions stvars (locals++meths) $ extend (envOf params') $ define [(selfKW, NVar tSelf)] $ defineTVars q env
            env2                    = define (envOf decls ++ envOf inits) env1

            params'                 = seal params
            (decls,ss)              = partition isDecl body
            meths                   = bound decls
            inits                   = filter (not . isSig) ss
            stvars                  = statevars body
            fvs                     = free decls
            locals                  = nub $ (bound params' `intersect` fvs) ++ [ n | n <- dom $ envOf inits, not (isHidden n) || n `elem` fvs ]
            wrapped                 = [ wrapMeth def | Decl _ ds <- decls, def <- ds, dname def `elem` actions ]
            actions                 = [ n | Signature _ ns (TSchema _ _ (TFun _ fx _ _ _)) _ <- body, fx == fxProc, n <- ns ]

            propsigs                = [ Signature l0 [n] (monotype t) Property | (n,t) <- concat $ props' params' : map props inits ]

            props (VarAssign _ p _) = [ (n, t) | PVar _ n (Just t) <- p ]
            props (Assign _ p _)    = [ (n, t) | PVar _ n (Just t) <- p, n `elem` locals ]
            props (If _ bs els)     = restrict (concat $ map props els) (foldr1 intersect $ map bound bs)
            props _                 = []

            props' (PosPar n a _ p)
              | n `elem` locals     = (n, fromJust a) : props' p
              | otherwise           = props' p
            props' (PosSTAR n a)
              | n `elem` locals     = [(n, fromJust a)]
            props' _                = []

            copies                  = [ MutAssign l0 (selfRef n) (Var l0 (NoQ n)) | n <- bound params, n `elem` locals ]

            deactMeths (Decl l ds)  = Decl l <$> mapM deactMeth ds

            deactMeth (Def l n q p KwdNIL (Just t) b d fx)
                                    = do b' <- deactSuite env' b
                                         return $ Def l n' q' (addSelfPar p') KwdNIL (Just t') b' d fx
              where env'            = extendAndShadow (envOf p) $ setRet t' $ defineTVars q' env2
                    n'              = if n `elem` actions then localName n else n
                    p'              = seal p
                    t'              = seal t
                    q'              = seal q

            wrapMeth (Def l n q p KwdNIL (Just t) _ d fx)
                                    = Decl l0 [Def l0 n q (addSelfPar p') KwdNIL (Just $ tMsg t') [Return l0 (Just $ async)] d fxAction]
              where n'              = localName n
                    p'              = seal p
                    t'              = seal t
                    q'              = seal q
                    async           = Call l0 (tApp (eQVar primASYNCf) [t']) (PosArg self (PosArg clos PosNil)) KwdNil
                    self            = Var l0 (NoQ selfKW)
                    clos            = Lambda l0 PosNIL KwdNIL (Call l0 (tApp (selfRef n') ts) (pArg p') KwdNil) fxProc
                    ts              = map tVar (qbound q')


    deact env (Def l n q p KwdNIL (Just t) b d fx)
                                    = do --traceM ("### deact def " ++ prstr n)
                                         b <- deactSuite env1 b
                                         return $ Def l n q p KwdNIL (Just t) b d fx
      where env1                    = extendAndShadow (envOf p) $ setRet t $ defineTVars q env
    deact env (Class l n q u b)     = --trace ("### deact class " ++ prstr n) $
                                      Class l n q u <$> deactSuite env1 b
      where env1                    = defineSelf (NoQ n) q $ defineTVars q env
    deact env d                     = error ("deact unexpected decl: " ++ prstr d)


newact env n q p                    = Def l0 (newactName n) q p KwdNIL (Just t) [newassign, waitinit, sReturn x] NoDec fxProc
  where p'                          = seal p
        t                           = tCon $ TC (NoQ n) (map tVar $ qbound q)
        x                           = eVar tmpName
        newassign                   = sAssign (pVar tmpName t) (eCall (tApp (eQVar primNEWACTOR) [t]) [])
        waitinit                    = sExpr $ eCall (tApp (eQVar primAWAITf) [tNone]) [asyncmsg]
        asyncmsg                    = eCall (tApp (eQVar primASYNCf) [tNone]) [x, closure]
        closure                     = Lambda l0 PosNIL KwdNIL initcall fxProc
        initcall                    = Call l0 (eDot x initKW) (pArg p') KwdNil

newactQName (QName m n)             = QName m (newactName n)
newactQName (NoQ n)                 = NoQ (newactName n)
newactQName (GName m n)             = GName m (newactName n)

newactName n                        = Derived n (name "newact")

tmpName                             = Internal DeactPass "tmp" 0

addSelfPar p                        = PosPar selfKW (Just tSelf) Nothing p

selfRef n                           = Dot l0 (Var l0 (NoQ selfKW)) n


-- $ASYNCf : [A] => action($Actor, proc()->A) -> Msg[A]
-- $AFTERf : [A] => proc(int,      proc()->A) -> Msg[A]
-- $AWAITf : [A] => proc(Msg[A])              -> A


instance Deact Branch where
    deact env (Branch e ss)         = Branch <$> deactExp env tWild e <*> deactSuite env1 ss
      where env1                    = clearSampled env

instance Deact Handler where
    deact env (Handler ex b)        = Handler ex <$> deactSuite env1 b
      where env1                    = extendAndShadow (envOf ex) env


fxfree (TVar _ v)                   = []
fxfree (TCon _ c)                   = concat $ map fxfree $ tcargs c
fxfree (TFun _ fx p k t)            = fxfree fx ++ fxfree p ++ fxfree k ++ fxfree t
fxfree (TTuple _ p k)               = fxfree p ++ fxfree k
fxfree (TOpt _ t)                   = fxfree t
fxfree (TNone _)                    = []
fxfree (TWild _)                    = []
fxfree (TNil _ _)                   = []
fxfree (TRow _ _ _ t r)             = fxfree t ++ fxfree r
fxfree (TFX l fx)                   = [fx]


adapt env t0@(TFun _ fx@(TFX _ x) p _ t) t1@(TFun _ fx'@(TFX _ x') p' _ t') e
  | (FXProc,   FXProc)   <- (x,x')  = eta
  | (_,        FXProc)   <- (x,x')  = etaF
  | (FXAction, FXAction) <- (x,x')  = eta
  | (FXProc,   FXAction) <- (x,x')  = async clos
  | (_,        FXAction) <- (x,x')  = async closF
  | (_,        _)        <- (x,x')  = eta
  where pars                        = pPar paramNames' p'
        args                        = qMatch (adapt env) p' p (pArg pars)
        e0                          = Call l0 e args KwdNil
        e1                          = qMatch (adapt env) t t' e0
        etaF                        = Lambda l0 pars KwdNIL e1 fx'
        eta                         = if args == pArg pars && e1 == e0 then e else etaF
        closF                       = Lambda l0 PosNIL KwdNIL e1 fxProc
        clos                        = if args == PosNil && e1 == e0 then e else closF
        async cl                    = Lambda l0 pars KwdNIL (Call l0 (tApp (eQVar primASYNCf) [unmsg t']) (PosArg (eVar selfKW) $ PosArg cl PosNil) KwdNil) fx'
        unmsg (TCon _ (TC n [t]))
          | unalias env n == qnMsg  = t
adapt env t0 t1 e
  | all (`elem`[FXPure,FXMut]) fxs  = e
  | all (`elem`[FXProc]) fxs        = e
  | TCon _ tc <- t0                 = notYet (loc e) "Methods embedded in data structures"
  where fxs                         = fxfree t0 ++ fxfree t1


deactExpTop env e                   = deact env $ qMatch (adapt env) t' tWild e'
  where (t',e')                     = qType env (adapt env) e

deactExp env t (Call l e p KwdNil)
  | TFX _ FXAction <- fx            = do e1 <- deact env $ qMatch (adapt env) t' t $ Call l e' (qMatch (adapt env) r r' p) KwdNil
                                         return $ Call l (tApp (eQVar primAWAITf) [t]) (PosArg e1 PosNil) KwdNil
  where (TFun _ fx r' _ t', e')     = qType env (adapt env) e
        (r,p')                      = qType env (adapt env) p
deactExp env t e                    = deact env $ qMatch (adapt env) t' t e'
  where (t',e')                     = qType env (adapt env) e


instance Deact Expr where
    deact env (Var l (NoQ n))
      | n `elem` sampled env        = return $ Var l (NoQ n)
      | n `elem` actions env        = return $ Dot l (Var l (NoQ selfKW)) (localName n)
      | n `elem` locals env         = return $ Dot l (Var l (NoQ selfKW)) n
    deact env (Var l n)
      | isActor env n               = return $ Var l $ newactQName n
      | otherwise                   = return $ Var l n
    deact env (Async l e)           = deact env e
    deact env (Await l e)           = do e' <- deact env e
                                         return $ Call l (tApp (eQVar primAWAITf) ts) (PosArg e' PosNil) KwdNil
      where TCon _ msg              = typeOf env e
            ts                      = tcargs msg
    deact env (Int l i s)           = return $ Int l i s
    deact env (Float l f s)         = return $ Float l f s
    deact env (Imaginary l i s)     = return $ Imaginary l i s
    deact env (Bool l b)            = return $ Bool l b
    deact env (None l)              = return $ None l
    deact env (NotImplemented l)    = return $ NotImplemented l
    deact env (Ellipsis l)          = return $ Ellipsis l
    deact env (Strings l s)         = return $ Strings l s
    deact env (BStrings l s)        = return $ BStrings l s
    deact env (Call l e ps KwdNil)  = Call l <$> deact env e <*> deact env ps <*> pure KwdNil
    deact env (TApp l e ts)         = TApp l <$> deact env e <*> pure ts
    deact env (Cond l e1 e e2)      = Cond l <$> deact env e1 <*> deact env e <*> deact env e2
    deact env (IsInstance l e c)    = IsInstance l <$> deact env e <*> return c
    deact env (BinOp l e1 Or e2)    = BinOp l <$> deact env e1 <*> pure Or <*> deact env e2
    deact env (BinOp l e1 And e2)   = BinOp l <$> deact env e1 <*> pure And <*> deact env e2
    deact env (UnOp l Not e)        = UnOp l Not <$> deact env e
    deact env (Dot l e nm)          = Dot l <$> deact env e <*> return nm
    deact env (DotI l e i)          = DotI l <$> deact env e <*> return i
    deact env (RestI l e i)         = RestI l <$> deact env e <*> return i
    deact env (Lambda l p KwdNIL e fx)
                                    = Lambda l p KwdNIL <$> deact env1 e <*> return fx
      where env1                    = extendAndShadow (envOf p) env
    deact env (Yield l e)           = Yield l <$> deact env e
    deact env (YieldFrom l e)       = YieldFrom l <$> deact env e
    deact env (Tuple l es KwdNil)   = Tuple l <$> deact env es <*> pure KwdNil
    deact env (List l es)           = List l <$> deact env es
    deact env e                     = error ("deact unexpected expr: " ++ prstr e)

instance Deact PosArg where
    deact env (PosArg e p)          = PosArg <$> deact env e <*> deact env p
    deact env PosNil                = return PosNil

instance Deact Elem where
    deact env (Elem e)              = Elem <$> deact env e
    deact env (Star e)              = Star <$> deact env e


-- Variables free in a lambda -----------------------------------------------------------------------------------

class LambdaFree a where
    lamfree                         :: a -> [Name]

instance (LambdaFree a) => LambdaFree [a] where
    lamfree                         = concat . map lamfree

instance (LambdaFree a) => LambdaFree (Maybe a) where
    lamfree                         = maybe [] lamfree

instance LambdaFree Stmt where
    lamfree (Expr _ e)              = lamfree e
    lamfree (Assign _ p e)          = lamfree e
    lamfree (MutAssign _ t e)       = lamfree t ++ lamfree e
    lamfree (Return _ e)            = maybe [] lamfree e
    lamfree (If _ bs els)           = concat [ lamfree e | Branch e ss <- bs ]
    lamfree (While _ e b els)       = lamfree e
    lamfree (VarAssign _ p e)       = lamfree e
    lamfree (After l e1 e2)         = lamfree e1 ++ free e2         -- deact will turn e2 into a lambda
    lamfree _                       = []

instance LambdaFree Expr where
    lamfree (Await _ e)             = lamfree e
    lamfree (Call _ e ps KwdNil)    = lamfree e ++ lamfree ps
    lamfree (TApp _ e ts)           = lamfree e
    lamfree (Cond _ e1 e e2)        = lamfree e1 ++ lamfree e ++ lamfree e2
    lamfree (IsInstance _ e c)      = lamfree e
    lamfree (BinOp _ e1 Or e2)      = lamfree e1 ++ lamfree e2
    lamfree (BinOp _ e1 And e2)     = lamfree e1 ++ lamfree e2
    lamfree (UnOp _ Not e)          = lamfree e
    lamfree (Dot _ e n)             = lamfree e
    lamfree (DotI _ e i)            = lamfree e
    lamfree (RestI _ e i)           = lamfree e
    lamfree (Yield _ e)             = lamfree e
    lamfree (YieldFrom _ e)         = lamfree e
    lamfree (Tuple _ p k)           = lamfree p ++ lamfree k
    lamfree (List _ es)             = lamfree es
    lamfree e@Lambda{}              = free e                        -- Free in lambda!
    lamfree _                       = []

instance LambdaFree PosArg where
    lamfree (PosArg e p)            = lamfree e ++ lamfree p
    lamfree PosNil                  = []

instance LambdaFree KwdArg where
    lamfree (KwdArg n e k)          = lamfree e ++ lamfree k
    lamfree KwdNil                  = []

instance LambdaFree Elem where
    lamfree (Elem e)                = lamfree e


-- Convert environments -----------------------------------------------------------------------------------------

conv env m (n, NAct q p k te')      = (n, NClass q (leftpath [TC primActor [], cValue]) (convActorEnv q p k te')) :
                                      (newactName n, NDef (tSchema q (tFun fxProc p k t)) NoDec) :
                                      []
  where convActorEnv q0 p k te'     = (initKW, NDef t0 NoDec) : te'
          where t0                  = tSchema q0 (tFun fxProc p k tNone)
        t                           = tCon $ TC (GName m n) (map tVar $ qbound q)
conv env m ni                       = [ni]
