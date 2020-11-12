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
deactorize env0 m                   = return (runDeactM (deact env m), mapModules1 conv env0)
  where env                         = deactEnv env0

-- Deactorizing monad
type DeactM a                       = State DeactState a

type DeactState                     = [Int]

runDeactM                           :: DeactM a -> a
runDeactM m                         = evalState m [1..]

type DeactEnv                       = EnvF DeactX

data DeactX                         = DeactX { actionsX :: [Name], stvarsX :: [Name], localsX :: [Name], sampledX :: [Name], actidX :: Maybe Type }

deactEnv                            :: Env0 -> DeactEnv
deactEnv env0                       = setX env0 DeactX{ actionsX = [], stvarsX = [], localsX = [], sampledX = [], actidX = Nothing }

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

actid env                           = actidX $ envX env

actorId env                         = fromJust (actid env)


setActor id acts stvars locals env  = modX env $ \x -> x{ actionsX = acts, stvarsX = stvars, localsX = locals, sampledX = [], actidX = Just id }

setSampled ns env                   = modX env $ \x -> x{ sampledX = ns ++ sampled env }

clearSampled env                    = modX env $ \x -> x{ sampledX = [] }

setId (TFX _ (FXAct id)) env        = modX env $ \x -> x{ actidX = Just id }
setId _ env                         = env


-- Deactorize actor declarations -----------------------------------------------------------------------

class Deact a where
    deact                           :: DeactEnv -> a -> DeactM a

instance Deact a => Deact (Maybe a) where
    deact env                       = traverse (deact env)

instance Deact a => Deact [a] where
    deact env                       = traverse (deact env)

instance Deact Module where
    deact env (Module m imps ss)    = Module m imps <$> deactSuite env ss


deactSuite env []                   = return []
deactSuite env (s : ss)             = do s' <- deact (setSampled ns env) s
                                         ss' <- deactSuite env1 ss
                                         return (samples ++ s' : ss')
  where env1                        = extend (envOf s) env
        ns                          = nub $ stvars env `intersect` lamfree s
        samples                     = [ sAssign (pVar n $ typeOf env (eVar n)) (eDot (eVar selfKW) n) | n <- ns ]


instance Deact Stmt where
    deact env (Expr l e)            = Expr l <$> deact env e
    deact env (Assign l [PVar _ n _] e)
      | n `elem` stvars env         = MutAssign l (selfRef n) <$> deact env e
      | n `elem` locals env         = MutAssign l (selfRef n) <$> deact env e
    deact env (Assign l ps e)       = Assign l ps <$> deact env e
    deact env (MutAssign l t e)     = MutAssign l <$> deact env t <*> deact env e
    deact env (Pass l)              = return $ Pass l
    deact env (Return l mbe)        = Return l <$> deact env mbe
    deact env (Break l)             = return $ Break l
    deact env (Continue l)          = return $ Continue l
    deact env (If l bs els)         = If l <$> deact env bs <*> deactSuite env1 els
      where env1                    = clearSampled env
    deact env (While l e b els)     = While l <$> deact env e <*> deactSuite env1 b <*> deactSuite env1 els
      where env1                    = clearSampled env
    deact env (Try l b hs els fin)  = Try l <$> deactSuite env b <*> deact env hs <*> deactSuite env els <*> deactSuite env fin
    deact env (VarAssign l [PVar _ n _] e)
                                    = MutAssign l (selfRef n) <$> deact env e
    deact env (After l e1 e2)       = do delta <- deact env e1
                                         lambda <- deact env $ Lambda l0 PosNIL KwdNIL e2 (fxAct $ actorId env)
                                         return $ Expr l $ Call l0 (tApp (eQVar primAFTERf) ts) (PosArg delta $ PosArg lambda PosNil) KwdNil
      where ts                      = [actorId env, typeOf env e2]
    deact env (Decl l ds)           = Decl l <$> deact env1 ds
      where env1                    = extend (envOf ds) env
    deact env (Signature l ns t d)  = return $ Signature l ns t d
    deact env s                     = error ("deact unexpected: " ++ prstr s)

instance Deact Decl where
    deact env (Actor l n q p KwdNIL b)
                                    = do inits <- deactSuite env1 inits
                                         decls <- mapM deactMeths decls
                                         let _init_ = Def l0 initKW [] (addSelfPar p) KwdNIL (Just tNone) (mkBody $ copies++inits) NoDec (fxAct tSelf)
                                         return $ Class l n q [TC primActor [], cStruct] (propsigs ++ [Decl l0 [_init_]] ++ decls ++ wrapped)
      where env1                    = setActor tSelf actions stvars locals $ extend (envOf p) $ defineTVars q env
            env2                    = define (envOf decls ++ envOf inits) env1

            (decls,ss)              = partition isDecl b
            meths                   = bound decls
            inits                   = filter (not . isSig) ss
            stvars                  = statevars b
            fvs                     = free decls
            locals                  = nub $ (bound p `intersect` fvs) ++ [ n | n <- dom $ envOf b, not (isHidden n) || n `elem` fvs ]
            wrapped                 = [ wrapMeth def | Decl _ ds <- decls, def <- ds, dname def `elem` actions ]
            actions                 = [ n | Signature _ ns (TSchema _ _ (TFun _ fx _ _ _)) _ <- b, fx == fxAction, n <- ns ]

            propsigs                = [ Signature l0 [n] (monotype t) Property | (n,t) <- concat $ props' p : map props inits ]

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

            copies                  = [ MutAssign l0 (selfRef n) (Var l0 (NoQ n)) | n <- bound p, n `elem` locals ]

            deactMeths (Decl l ds)  = Decl l <$> mapM deactMeth ds

            deactMeth (Def l n q p KwdNIL t b d fx)
                                    = do b <- deactSuite env' b
                                         return $ Def l n' q (addSelfPar p) KwdNIL t b d fx
              where env'            = extendAndShadow (envOf p) env2
                    n'              = if n `elem` actions then localName n else n

            wrapMeth (Def l n q p KwdNIL (Just t) b d fx)
                                    = Decl l0 [Def l0 n q (addSelfPar p) KwdNIL (Just $ tMsg t) [Return l0 (Just $ async)] d fxAction]
              where n'              = localName n
                    async           = Call l0 (tApp (eQVar primASYNCf) ts') (PosArg self (PosArg clos PosNil)) KwdNil
                    self            = Var l0 (NoQ selfKW)
                    clos            = Lambda l0 PosNIL KwdNIL (Call l0 (tApp (selfRef n') ts) (par2arg p) KwdNil) fx
                    ts              = map tVar (tybound q)
                    ts'             = [tSelf, t]

    deact env (Def l n q p KwdNIL t b d fx)
                                    = do b <- deactSuite env1 b
                                         return $ Def l n q p KwdNIL t b d fx
      where env1                    = extendAndShadow (envOf p) $ defineTVars q $ setId fx env
    deact env (Class l n q u b)     = Class l n q u <$> deactSuite env1 b
      where env1                    = defineSelf (NoQ n) q $ defineTVars q env
    deact env d                     = error ("deact unexpected: " ++ prstr d)

localName n                         = Derived n (name "local")

addSelfPar p                        = PosPar selfKW (Just tSelf) Nothing p

selfRef n                           = Dot l0 (Var l0 (NoQ selfKW)) n


-- $ASYNCf : [S,A] => act[S]($Actor, act[S]()->A) -> Msg[A]
-- $AFTERf : [S,A] => act[S](int,    act[S]()->A) -> Msg[A]
-- $AWAITf : [S,A] => act[S](Msg[A])              -> A


instance Deact Expr where
    deact env (Var l (NoQ n))
      | n `elem` actions env        = return $ Dot l (Var l (NoQ selfKW)) (localName n)
      | n `elem` sampled env        = return $ Var l (NoQ n)
      | n `elem` stvars env         = return $ Dot l (Var l (NoQ selfKW)) n
      | n `elem` locals env         = return $ Dot l (Var l (NoQ selfKW)) n
    deact env (Var l n)             = return $ Var l n
    deact env (Await l e)           = do e' <- deact env e
                                         return $ Call l (tApp (eQVar primAWAITf) ts) (PosArg e' PosNil) KwdNil
      where TCon _ msg              = typeOf env e
            ts                      = actorId env : tcargs msg
    deact env (Int l i s)           = return $ Int l i s
    deact env (Float l f s)         = return $ Float l f s
    deact env (Imaginary l i s)     = return $ Imaginary l i s
    deact env (Bool l b)            = return $ Bool l b
    deact env (None l)              = return $ None l
    deact env (NotImplemented l)    = return $ NotImplemented l
    deact env (Ellipsis l)          = return $ Ellipsis l
    deact env (Strings l s)         = return $ Strings l s
    deact env (BStrings l s)        = return $ BStrings l s
    deact env (Call l e ps KwdNil)
      | TApp _ (Var _ n) ts <- e,
        n == primASYNCw             = Call l (tApp (eQVar primASYNCf) ts) <$> (PosArg (eVar selfKW) <$> deact env ps) <*> pure KwdNil
      | otherwise                   = Call l <$> deact env e <*> deact env ps <*> pure KwdNil
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
    deact env e                     = error ("deact unexpected: " ++ prstr e)

instance Deact Branch where
    deact env (Branch e ss)         = Branch <$> deact env e <*> deactSuite env1 ss
      where env1                    = clearSampled env

instance Deact Handler where
    deact env (Handler ex b)        = Handler ex <$> deactSuite env1 b
      where env1                    = extendAndShadow (envOf ex) env

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

conv (n, NAct q p k te')            = (n, NClass q [([Nothing],TC primActor [])] (convActorEnv q p k te'))
  where convActorEnv q0 p k te'     = (initKW, NDef t0 NoDec) : [ (n, convI i) | (n,i) <- te' ]
          where t0                  = tSchema q0 (TFun NoLoc fx0 p k tNone)

        convI (NSig sc dec)         = NSig (convS sc) dec
        convI (NDef sc dec)         = NDef (convS sc) dec
        convI i                     = i

        convS (TSchema l q t)       = TSchema l q (convT t)

        convT (TFun l fx p k t)
          | fx == fxAction          = TFun l fx0 p k (tMsg t)
        convT t                     = t

        fx0                         = fxAct tSelf
conv ni                             = ni

