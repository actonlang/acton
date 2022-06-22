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
module Acton.Deactorizer2 where

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
deactorize env0 (Module m imps b)   = return (Module m imps (runDeactM $ deactSuite env b), mapModules conv env0)
  where env                         = deactEnv env0


-- Deactorizing monad
type DeactM a                       = State DeactState a

type DeactState                     = [Int]

runDeactM                           :: DeactM a -> a
runDeactM m                         = evalState m [1..]

type DeactEnv                       = EnvF DeactX

data DeactX                         = DeactX { exportsX :: [Name], stvarsX :: [Name], localsX :: [Name], sampledX :: [Name] }

deactEnv                            :: Env0 -> DeactEnv
deactEnv env0                       = setX env0 DeactX{ exportsX = [], stvarsX = [], localsX = [], sampledX = [] }

defineAndShadow                     :: TEnv -> DeactEnv -> DeactEnv
defineAndShadow te env              = modX (define te env) $ \x -> x{ exportsX = exports env \\ ns, localsX = locals env \\ ns }
  where ns                          = dom te

newName                             :: String -> DeactM Name
newName s                           = state (\(uniq:supply) -> (Internal DeactPass s uniq, supply))


exports env                         = exportsX $ envX env

stvars env                          = stvarsX $ envX env

locals env                          = localsX $ envX env

sampled env                         = sampledX $ envX env

setActor exports stvars locals env  = modX env $ \x -> x{ exportsX = exports, stvarsX = stvars, localsX = locals, sampledX = [] }

setSampled ns env                   = modX env $ \x -> x{ sampledX = ns ++ sampled env }

clearSampled env                    = modX env $ \x -> x{ sampledX = [] }


-- Deactorize actor declarations -----------------------------------------------------------------------

class Deact a where
    deact                           :: DeactEnv -> a -> DeactM a

instance Deact a => Deact (Maybe a) where
    deact env                       = traverse (deact env)

instance Deact a => Deact [a] where
    deact env                       = traverse (deact env)


deactBody env b
  | isNotImpl b                     = return b
  | otherwise                       = deactSuite env b

deactSuite env []                   = return []
deactSuite env (s : ss)             = do s' <- deact (setSampled ns env) s
                                         ss' <- deactSuite env1 ss
                                         return (samples ++ s' : ss')
  where env1                        = define (envOf s) env
        ns                          = nub $ stvars env `intersect` lamfree s
        samples                     = [ sAssign (pVar n $ typeOf env (eVar n)) (eDot (eVar selfKW) n) | n <- ns ]


instance Deact Stmt where
    deact env (Expr l e)            = Expr l <$> deact env e
    deact env (Assign l [p@(PVar _ n _)] e)
      | n `elem` locals env         = MutAssign l (selfRef n) <$> deact env e
      | otherwise                   = Assign l [p] <$> deact env e
      where t                       = typeOf env p
    deact env (MutAssign l tg e)    = MutAssign l <$> deact env tg <*> deact env e
      where t                       = typeOf env tg
    deact env (Pass l)              = return $ Pass l
    deact env (Return l Nothing)    = return $ Return l Nothing
    deact env (Return l (Just e))   = Return l . Just <$> deact env e
    deact env (Break l)             = return $ Break l
    deact env (Continue l)          = return $ Continue l
    deact env (If l bs els)         = If l <$> deact env bs <*> deactSuite env1 els
      where env1                    = clearSampled env
    deact env (While l e b els)     = While l <$> deact env e <*> deactSuite env1 b <*> deactSuite env1 els
      where env1                    = clearSampled env
    deact env (Try l b hs els fin)  = Try l <$> deactSuite env b <*> deact env hs <*> deactSuite env els <*> deactSuite env fin
    deact env (VarAssign l [p@(PVar _ n _)] e)
                                    = MutAssign l (selfRef n) <$> deact env e
      where t                       = typeOf env p
    deact env (After l e1 e2)       = do delta <- deact env e1
                                         lambda <- deact env $ Lambda l0 PosNIL KwdNIL e2 fxProc
                                         return $ Expr l $ Call l0 (tApp (eQVar primAFTERf) [t2]) (PosArg delta $ PosArg lambda PosNil) KwdNil
      where t2                      = typeOf env e2
            t                       = tFun fxProc posNil kwdNil t2
    deact env (Decl l ds)           = do ds1 <- deact env1 ds
                                         return $ Decl l $ ds1 ++ [ newact env1 n q p | Actor _ n q p _ _ <- ds, not $ abstractActor env1 (NoQ n) ]
      where env1                    = define (envOf ds) env
    deact env (Signature l ns t d)  = return $ Signature l ns t d
    deact env s                     = error ("deact unexpected stmt: " ++ prstr s)

instance Deact Decl where
    deact env (Actor l n q params KwdNIL body)
                                    = do inits' <- deactSuite (define (envOf decls) env1) inits
                                         decls' <- deactSuite (define (envOf inits) env1) decls
                                         let _init_ = Def l0 initKW [] (addSelfPar params) KwdNIL (Just tNone) (mkBody $ copies++inits') NoDec fxProc
                                         return $ Class l n q [TC primActor [], cValue] (propsigs ++ [Decl l0 [_init_]] ++ decls')
      where env1                    = setActor exports stvars (locals++meths) $ define (envOf params) $ define [(selfKW, NVar tSelf)] $ defineTVars q env
            env2                    = define (envOf decls ++ envOf inits) env1

            (decls,ss)              = partition isDecl body
            inits                   = filter (not . isSig) ss
            stvars                  = statevars body
            meths                   = bound decls
            fvs                     = free decls
            locals                  = nub $ (bound params `intersect` fvs) ++ [ n | n <- dom $ envOf inits, not (isHidden n) || n `elem` fvs ]
            exports                 = [ n | Decl _ ds <- decls, Def{dname=n, dfx=fx} <- ds, fx == fxProc ]

            propsigs                = [ Signature l0 [n] (monotype t) Property | (n,t) <- concat $ props' params : map props inits ]

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

    deact env (Def l n q p KwdNIL (Just t) b d fx)
                                    = do b <- deactBody env1 b
                                         return $ Def l n q p KwdNIL (Just t) b d fx
      where env1                    = defineAndShadow (envOf p) $ defineTVars q env

    deact env (Class l n q u b)     = Class l n q u <$> deactSuite env1 b
      where env1                    = defineSelf (NoQ n) q $ defineTVars q env

    deact env d                     = error ("deact unexpected decl: " ++ prstr d)


newact env n q p                    = Def l0 (newactName n) q p KwdNIL (Just t) [newassign, waitinit, sReturn x] NoDec fxProc
  where t                           = tCon $ TC (NoQ n) (map tVar $ qbound q)
        x                           = eVar tmpName
        newassign                   = sAssign (pVar tmpName t) (eCall (tApp (eQVar primNEWACTOR) [t]) [])
        waitinit                    = sExpr $ eCall (tApp (eQVar primAWAITf) [tNone]) [asyncmsg]
        asyncmsg                    = eCall (tApp (eQVar primASYNCf) [tNone]) [x, closure]
        closure                     = Lambda l0 PosNIL KwdNIL initcall fxProc
        initcall                    = Call l0 (eDot x initKW) (pArg p) KwdNil

newactQName (QName m n)             = QName m (newactName n)
newactQName (NoQ n)                 = NoQ (newactName n)
newactQName (GName m n)             = GName m (newactName n)

newactName n                        = Derived n (name "newact")

tmpName                             = Internal DeactPass "tmp" 0

addSelfPar p                        = PosPar selfKW (Just tSelf) Nothing p

selfRef n                           = Dot l0 (Var l0 (NoQ selfKW)) n


-- $ASYNCf : [A] => mut($Actor, proc()->A) -> Msg[A]
-- $AFTERf : [A] => proc(int,      proc()->A) -> Msg[A]
-- $AWAITf : [A] => proc(Msg[A])              -> A


instance Deact Branch where
    deact env (Branch e ss)         = Branch <$> deact env e <*> deactSuite env1 ss
      where env1                    = clearSampled env

instance Deact Handler where
    deact env (Handler ex b)        = Handler ex <$> deactSuite env1 b
      where env1                    = defineAndShadow (envOf ex) env

isProcMeth env e
  | Just n <- isQVar e,
    NDef sc _ <- findQName n env,
    TFun{effect=fx} <- sctype sc    = fx == fxProc
isProcMeth env _                    = False

isExportMeth env e
  | Just n <- isVar e               = n `elem` exports env
isExportMeth env _                  = False

instance Deact Expr where
    deact env (Var l (NoQ n))
      | n `elem` sampled env        = return $ Var l (NoQ n)
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
    deact env (Call l (TApp _ (Var _ n) ts) (PosArg e PosNil) KwdNil)
      | n == primEXEC,
        isProcMeth env e            = deact env e
    deact env (Call l (TApp _ (Var _ n) ts) (PosArg s (PosArg e PosNil)) KwdNil)
      | n == primSEAL,
        isExportMeth env e          = deact env e
-----------------------------------------------------------------
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
      where env1                    = defineAndShadow (envOf p) env
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


-- Convert types and environments -----------------------------------------------------------------------

conv env m (n, NAct q p k te')      = (n, NClass q (leftpath [TC primActor [], cValue]) (convActorEnv q p k te')) :
                                      (newactName n, NDef (tSchema q (tFun fxAction p k t)) NoDec) :
                                      []
  where convActorEnv q0 p k te'     = (initKW, NDef t0 NoDec) : te'
          where t0                  = tSchema q0 (tFun fxProc p k tNone)
        t                           = tCon $ TC (GName m n) (map tVar $ qbound q)
conv env m ni                       = [ni]