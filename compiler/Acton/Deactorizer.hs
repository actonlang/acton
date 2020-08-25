module Acton.Deactorizer where

import Acton.Syntax
import Acton.Names
import Acton.Subst
import Acton.Prim
import Acton.Builtin
import Acton.QuickType
import Acton.Env
import Utils
import Control.Monad.State.Lazy

deactorize                          :: Env -> Module -> IO Module
deactorize mainenv m                = return $ evalState (deact env m) []
  where env                         = deactEnv mainenv

-- Deactorizing monad
type DeactM a                       = State [Stmt] a

store                               :: [Stmt] -> DeactM ()
store ss                            = state (\stmts -> ((), reverse ss ++ stmts))

swapStore                           :: [Stmt] -> DeactM [Stmt]
swapStore ss                        = state (\stmts -> (stmts, ss))

withStore                           :: DeactM a -> DeactM (a,[Stmt])
withStore m                         = do ss0 <- swapStore []
                                         r <- m
                                         ss1 <- swapStore ss0
                                         return (r, ss1)

data DeactEnv                       = DeactEnv { mainenv :: Env, actions :: [Name], locals :: [Name] }

deactEnv mainenv                    = DeactEnv { mainenv = mainenv, actions = [], locals = [] }

extend te env                       = env{ mainenv = define te (mainenv env),
                                           actions = actions env \\ ns,
                                           locals = locals env \\ ns }
  where ns                          = dom te

extendTVars q env                   = env{ mainenv = defineTVars q (mainenv env) }

setActor actions locals env         = env{ actions = actions, locals = locals }


class Deact a where
    deact                           :: DeactEnv -> a -> DeactM a

instance (Deact a, EnvOf a) => Deact [a] where
    deact env []                    = return []
    deact env (a:as)                = (:) <$> deact env a <*> deact env1 as
      where env1                    = extend (envOf a) env

instance Deact a => Deact (Maybe a) where
    deact env Nothing               = return Nothing
    deact env (Just a)              = Just <$> deact env a

instance Deact Module where
    deact env (Module qn imps ss)   = Module qn imps <$> deact env ss

instance Deact Stmt where
    deact env (Expr l e)            = Expr l <$> deact env e
    deact env (Assign l [PVar _ n _] e)
      | n `elem` locals env         = MutAssign l (selfRef n) <$> deact env e
    deact env (Assign l ps e)       = Assign l ps <$> deact env e
    deact env (MutAssign l t e)     = MutAssign l <$> deact env t <*> deact env e
    deact env (AugAssign l t op e)  = AugAssign l <$> deact env t <*> return op <*> deact env e
    deact env (Pass l)              = return $ Pass l
    deact env (Delete l t)          = Delete l <$> deact env t
    deact env (Return l mbe)        = Return l <$> deact env mbe
    deact env (Raise l mbex)        = Raise l <$> deact env mbex
    deact env (Break l)             = return $ Break l
    deact env (Continue l)          = return $ Continue l
    deact env (If l bs els)         = If l <$> deact env bs <*> deact env els
    deact env (While l e b els)     = While l <$> deact env e <*> deact env b <*> deact env els
    deact env (For l p e b els)     = For l p <$> deact env e <*> deact env1 b <*> deact env els
      where env1                    = extend (envOf p) env
    deact env (Try l b hs els fin)  = Try l <$> deact env b <*> deact env hs <*> deact env els <*> deact env fin
    deact env (With l is b)         = With l <$> deact env is <*> deact env1 b
      where env1                    = extend (envOf is) env
    deact env (Data l mbp ss)       = Data l mbp <$> deact env ss
    deact env (VarAssign l [PVar _ n _] e)
                                    = MutAssign l (selfRef n) <$> deact env e
    deact env (After l e1 e2)       = do e1' <- deact env e1
                                         e2' <- deact env e2
                                         let lambda = Lambda l0 PosNIL KwdNIL e2' (fxAct tWild)
                                         return $ Expr l $ Call l0 (eQVar primAFTER) (PosArg e1' $ PosArg lambda PosNil) KwdNil  -- TODO: supply type args
    deact env (Decl l ds)           = Decl l <$> deact env1 ds
      where env1                    = extend (envOf ds) env
    deact env (Signature l ns t d)  = return $ Signature l ns t d

instance Deact Decl where
    deact env (Actor l n q p k b)   = do inits <- deact env1 inits
                                         decls <- mapM deactMeths decls
                                         let _init_ = Def l0 initKW [] (addSelf p) k Nothing (if null inits then [Pass l0] else inits) NoDec (fxAct tWild)
                                         return $ Class l n q [TC primActor []] (properties ++ [Decl l0 [_init_]] ++ decls ++ wrapped)
      where env1                    = setActor actions locals $ extend (envOf p ++ envOf k) $ extendTVars q env

            (decls,ss)              = partition isDecl b
            meths                   = bound decls
            inits                   = filter (not . isSig) ss
            stvars                  = statedefs b
            params                  = bound (p,k)
            locals                  = nub $ params ++ bound b ++ stvars
            wrapped                 = [ wrapMeth def | Decl _ ds <- decls, def <- ds, dname def `elem` actions ]
            actions                 = [ n | Signature _ ns (TSchema _ _ (TFun _ fx _ _ _)) _ <- b, fx == fxAction, n <- ns ]

            properties              = [ Signature l0 [n] (monotype t) Property | (n,t) <- concat $ props' p : map props inits ]

            props (VarAssign _ p _) = [ (n, fromJust a) | PVar _ n a <- p ]
            props (Assign _ p _)    = [ (n, fromJust a) | PVar _ n a <- p ]
            props (If _ bs els)     = restrict (foldr1 intersect $ map bound bs) (concat $ map props els)
            props _                 = []

            props' (PosPar n a _ p) = (n, fromJust a) : props' p
            props' (PosSTAR n a)    = [(n, fromJust a)]
            props' PosNIL           = []

            copies                  = [ MutAssign l0 (selfRef n) (Var l0 (NoQ n)) | n <- params ]

            deactMeths (Decl l ds)  = Decl l <$> mapM deactMeth ds
            deactMeths s            = deact env1 s

            deactMeth (Def l n q p k t b d fx)
                                    = do b <- deact env2 b
                                         return $ Def l n' q (addSelf p) k t b d fx
              where env2            = extend (envOf p ++ envOf k) env1
                    n'              = if n `elem` actions then localName n else n

            wrapMeth (Def l n q p k (Just t) b d fx)
                                    = Decl l0 [Def l0 n q (addSelf p) k (Just t) [Return l0 (Just $ async)] d fx]
              where n'              = localName n
                    async           = Call l0 (Var l0 primASYNC) (PosArg self (PosArg clos PosNil)) KwdNil       -- TODO: supply type args
                    self            = Var l0 (NoQ selfKW)
                    clos            = Lambda l0 PosNIL KwdNIL (Call l0 (tApp (selfRef n') ts) (parToArg p) KwdNil) fx
                    ts              = map tVar (tybound q)

    deact env (Def l n q p k t b d fx)
                                    = do b <- deact env b
                                         return $ Def l n q p k t b d fx
      where env1                    = extend (envOf p ++ envOf k) $ extendTVars q env
    deact env (Class l n q u b)     = Class l n q u <$> deact env b
      where env1                    = extendTVars q env
    deact env (Protocol l n q u b)  = Protocol l n q u <$> deact env b
      where env1                    = extendTVars q env
    deact env (Extension l n q u b) = Extension l n q u <$> deact env b
      where env1                    = extendTVars q env

localName n                         = Derived n "local"

addSelf p                           = PosPar selfKW (Just tSelf) Nothing p

selfRef n                           = Dot l0 (Var l0 (NoQ selfKW)) n

parToArg PosNIL                     = PosNil
parToArg (PosPar n _ _ p)           = PosArg (Var l0 (NoQ n)) (parToArg p)


-- $ASYNC : [S,A] => act[S]($Actor, act[S]()->A) -> A
-- $AFTER : [S,A] => act[S](int, act[S]()->A) -> None
-- $AWAIT : [S,A] => act[S](Msg[A]) -> A


instance Deact Expr where
    deact env (Var l (NoQ n))
      | n `elem` actions env        = return $ Dot l (Var l (NoQ selfKW)) (localName n)
      | n `elem` locals env         = return $ Dot l (Var l (NoQ selfKW)) n
    deact env (Var l n)             = return $ Var l n
    deact env (Await l e)           = do e' <- deact env e
                                         return $ Call l (eQVar primAWAIT) (PosArg e' PosNil) KwdNil     -- TODO: supply the type args
    deact env (Int l i s)           = return $ Int l i s
    deact env (Float l f s)         = return $ Float l f s
    deact env (Imaginary l i s)     = return $ Imaginary l i s
    deact env (Bool l b)            = return $ Bool l b
    deact env (None l)              = return $ None l
    deact env (NotImplemented l)    = return $ NotImplemented l
    deact env (Ellipsis l)          = return $ Ellipsis l
    deact env (Strings l s)         = return $ Strings l s
    deact env (BStrings l s)        = return $ BStrings l s
    deact env (Call l e ps _)       = Call l <$> deact env e <*> deact env ps <*> pure KwdNil
    deact env (TApp l e ts)         = TApp l <$> deact env e <*> pure ts
    deact env (Index l e is)        = Index l <$> deact env e <*> deact env is
    deact env (Slice l e sl)        = Slice l <$> deact env e <*> deact env sl
    deact env (Cond l e1 e2 e3)     = Cond l <$> deact env e1 <*> deact env e2 <*> deact env e3
    deact env (IsInstance l e c)    = IsInstance l <$> deact env e <*> return c
    deact env (BinOp l e1 op e2)    = BinOp l <$> deact env e1 <*> return op <*> deact env e2
    deact env (CompOp l e ops)      = CompOp l <$> deact env e <*> deact env ops
    deact env (UnOp l op e)         = UnOp l op <$> deact env e 
    deact env (Dot l e nm)          = Dot l <$> deact env e <*> return nm
    deact env (DotI l e i t)        = DotI l <$> deact env e <*> return i <*> return t
    deact env (Lambda l p k e fx)   = Lambda l p k <$> deact env1 e <*> return fx
      where env1                    = extend (envOf p ++ envOf k) env
    deact env (Yield l e)           = Yield l <$> deact env e
    deact env (YieldFrom l e)       = YieldFrom l <$> deact env e
    deact env (Tuple l es ks)       = Tuple l <$> deact env es <*> deact env ks
    deact env (List l es)           = List l <$> deact env es
    deact env (ListComp l e c)      = ListComp l <$> deact env1 e <*> deact env c
      where env1                    = extend (envOf c) env
    deact env (Dict l as)           = Dict l <$> deact env as
    deact env (DictComp l a c)      = DictComp l <$> deact env1 a <*> deact env c
      where env1                    = extend (envOf c) env
    deact env (Set l es)            = Set l <$> deact env es
    deact env (SetComp l e c)       = SetComp l <$> deact env1 e <*> deact env c
      where env1                    = extend (envOf c) env
    deact env (Paren l e)           = Paren l <$> deact env e

instance Deact Exception where
    deact env (Exception e mbe)     = Exception <$> deact env e <*> deact env mbe

instance Deact Branch where
    deact env (Branch e ss)         = Branch <$> deact env e <*> deact env ss

instance Deact Handler where
    deact env (Handler ex b)        = Handler ex <$> deact env1 b
      where env1                    = extend (envOf ex) env

instance Deact PosArg where
    deact env (PosArg e p)          = PosArg <$> deact env e <*> deact env p
    deact env PosNil                = return PosNil

instance Deact KwdArg where
    deact env (KwdArg n e k)        = KwdArg n <$> deact env e <*> deact env k
    deact env KwdNil                = return KwdNil

instance Deact OpArg where
    deact env (OpArg op e)          = OpArg op <$> deact env e

instance Deact Comp where
    deact env (CompFor l p e c)     = CompFor l p <$> deact env1 e <*> deact env1 c
      where env1                    = extend (envOf p) env
    deact env (CompIf l e c)        = CompIf l <$> deact env e <*> deact env c
    deact env NoComp                = return NoComp

instance Deact WithItem where
    deact env (WithItem e p)        = WithItem <$> deact env e <*> pure p

instance Deact Elem where
    deact env (Elem e)              = Elem <$> deact env e
    deact env (Star e)              = Star <$> deact env e

instance Deact Assoc where
    deact env (Assoc e1 e2)         = Assoc <$> deact env e1 <*> deact env e2
    deact env (StarStar e)          = StarStar <$> deact env e
  
instance Deact Sliz where
    deact env (Sliz l e1 e2 e3)     = Sliz l <$> deact env e1 <*> deact env e2 <*> deact env e3
