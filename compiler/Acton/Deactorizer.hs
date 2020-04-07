module Acton.Deactorizer where

import Acton.Syntax
import Acton.Names
import Acton.Env hiding (newName)
import Acton.Prim
import Acton.Builtin
import Utils
import Control.Monad.State.Lazy

deactorize                          :: Env -> Module -> IO Module
deactorize env0 m                   = return $ evalState (deact env m) ([1..], [])
  where env                         = deactEnv env0

-- Deactorizing monad
type DeactM a                       = State ([Int],[Stmt]) a

newName                             :: String -> DeactM Name
newName s                           = state (\(uniq:supply, stmts) -> (Internal s uniq DeactPass, (supply, stmts)))

store                               :: [Stmt] -> DeactM ()
store ss                            = state (\(supply, stmts) -> ((), (supply, reverse ss ++ stmts)))

swapStore                           :: [Stmt] -> DeactM [Stmt]
swapStore ss                        = state (\(supply, stmts) -> (stmts, (supply, ss)))

withStore                           :: DeactM a -> DeactM (a,[Stmt])
withStore m                         = do ss0 <- swapStore []
                                         r <- m
                                         ss1 <- swapStore ss0
                                         return (r, ss1)

data DeactEnv                       = DeactEnv { actor :: Maybe Name, locals :: [Name] }

deactEnv env                        = DeactEnv { actor = Nothing, locals = [] }

realActor env                       = fromJust $ actor env

selfRef n env                       = n `elem` locals env

hideLocals ns env                   = env{ locals = locals env \\ ns }

class Deact a where
    deact                           :: DeactEnv -> a -> DeactM a

instance Deact a => Deact [a] where
    deact env []                    = return []
    deact env (a:as)                = (:) <$> deact env a <*> deact env as

instance Deact a => Deact (Maybe a) where
    deact env Nothing               = return Nothing
    deact env (Just a)              = Just <$> deact env a

instance Deact Module where
    deact env (Module qn imps ss)   = Module qn imps <$> deact env ss

instance Deact Stmt where
    deact env (Expr l e)            = Expr l <$> deact env e
    deact env (Assign l ps e)       = Assign l <$> deact env ps <*> deact env e
    deact env (Update l ts e)       = Update l <$> deact env ts <*> deact env e
    deact env (IUpdate l t op e)    = IUpdate l <$> deact env t <*> return op <*> deact env e
    deact env (Pass l)              = return $ Pass l
    deact env (Delete l t)          = Delete l <$> deact env t
    deact env (Return l mbe)        = Return l <$> deact env mbe
    deact env (Raise l mbex)        = Raise l <$> deact env mbex
    deact env (Break l)             = return $ Break l
    deact env (Continue l)          = return $ Continue l
    deact env (If l bs els)         = If l <$> deact env bs <*> deact env els
    deact env (While l e b els)     = While l <$> deact env e <*> deact env b <*> deact env els
    deact env (For l p e b els)     = For l <$> deact env p <*> deact env e <*> deact env b <*> deact env els
    deact env (Try l b hs els fin)  = Try l <$> deact env b <*> deact env hs <*> deact env els <*> deact env fin
    deact env (With l is b)         = With l <$> deact env is <*> deact env b
    deact env (Data l mbt ss)       = Data l <$> deact env mbt <*> deact env ss
    deact env (VarAssign l ps e)    = do let [PVar _ n (Just t)] = ps
                                         store [Signature l0 [n] (monotype t) Property]
                                         Assign l <$> deact env ps <*> deact env e
    deact env (After l e1 e2)       = do e1' <- deact env e1
                                         e2' <- deact env e2
                                         let lambda = Lambda l0 PosNIL KwdNIL e2'
                                         return $ Expr l $ Call l0 (eQVar primAFTER) (PosArg e1' $ PosArg lambda PosNil) KwdNil
    deact env (Decl l ds)           = Decl l <$> deactD env ds
    deact env (Signature l ns t d)  = return $ Signature l ns t d

deactD env []                       = return []
deactD env (d@Actor{} : ds)         = (++) <$> deactA env d <*> deactD env ds
deactD env (d : ds)                 = (:) <$> deact env d <*> deactD env ds

deactA env (Actor l n q p k t b)    = do n' <- newName (nstr n)
                                         (bint,sext) <- withStore (deact (env1 n') b)
                                         let (ssigs,sext') = partition isSig sext
                                             _init_ = Def l0 initKW [] (addSelf p) k t (create:copies) NoDec
                                             create = Update l0 [selfPat selfKW] (Call l0 (Var l0 (NoQual n')) (parToArg p) KwdNil)
                                             copies = [ Update l0 [selfPat n] (Dot l0 (Dot l0 (Var l0 (NoQual selfKW)) selfKW) n) | n <- consts ]
                                             consts = bound b \\ statedefs b \\ bound ds
                                             _init' = Def l0 initKW [] (PosPar selfKW Nothing Nothing p) k t ss NoDec
                                             (ds,ss) = partition isDecl bint
                                             extern = Class l n q [] (reverse sext' ++ [Decl l0 [_init_]])
                                             intern = Class l n' q [] (ssigs ++ [Decl l0 [_init']] ++ ds)
                                         return [intern, extern]
  where env1 n'                     = env{ locals = nub $ bound (p,k) ++ bound b ++ statedefs b, actor = Just n' }
        selfPat n                   = TaDot l0 (Var l0 (NoQual selfKW)) n
        isSig Signature{}           = True
        isSig _                     = False
        isDecl Decl{}               = True
        isDecl _                    = False

addSelf p                           = PosPar selfKW Nothing Nothing p

parToArg PosNIL                     = PosNil
parToArg (PosPar n _ _ p)           = PosArg (Var l0 (NoQual n)) (parToArg p)

asyncCall env n p                   = Call l0 (Var l0 primASYNC) (PosArg selfSelf (PosArg clos PosNil)) KwdNil
  where selfSelf                    = Dot l0 (Var l0 (NoQual selfKW)) selfKW
        meth                        = Dot l0 selfSelf n
        clos                        = Lambda l0 PosNIL KwdNIL $ Call l0 meth (parToArg p) KwdNil

awaitCall env n p                   = Call l0 (Var l0 primAWAIT) (PosArg (asyncCall env n p) PosNil) KwdNil

instance Deact Decl where
    deact env (Def l n q p k t b m) = Def l n q p k t <$> deact env1 b <*> return m
      where env1                    = hideLocals (bound (p,k) ++ bound b) env
    deact env (Class l n q u b)     = Class l n q u <$> deact env b
    deact env (Protocol l n q u b)  = Protocol l n q u <$> deact env b
    deact env (Extension l n q u b) = Extension l n q u <$> deact env b

instance Deact Expr where
    deact env (Var l (NoQual n))
      | selfRef n env               = return $ Dot l (Var l (NoQual selfKW)) n
    deact env (Var l n)             = return $ Var l n
    deact env (Await l e)           = do e' <- deact env e
                                         return $ Call l (eQVar primAWAIT) (PosArg e' PosNil) KwdNil
    deact env (Int l i s)           = return $ Int l i s
    deact env (Float l f s)         = return $ Float l f s
    deact env (Imaginary l i s)     = return $ Imaginary l i s
    deact env (Bool l b)            = return $ Bool l b
    deact env (None l)              = return $ None l
    deact env (NotImplemented l)    = return $ NotImplemented l
    deact env (Ellipsis l)          = return $ Ellipsis l
    deact env (Strings l s)         = return $ Strings l s
    deact env (BStrings l s)        = return $ BStrings l s
    deact env (Call l e ps _)       = Call l <$> deact env e <*> deact env ps <*> return KwdNil
    deact env (Index l e is)        = Index l <$> deact env e <*> deact env is
    deact env (Slice l e sl)        = Slice l <$> deact env e <*> deact env sl
    deact env (Cond l e1 e2 e3)     = Cond l <$> deact env e1 <*> deact env e2 <*> deact env e3
    deact env (BinOp l e1 op e2)    = BinOp l <$> deact env e1 <*> return op <*> deact env e2
    deact env (CompOp l e ops)      = CompOp l <$> deact env e <*> deact env ops
    deact env (UnOp l op e)         = UnOp l op <$> deact env e 
    deact env (Dot l e nm)          = Dot l <$> deact env e <*> return nm
    deact env (DotI l e i t)        = DotI l <$> deact env e <*> return i <*> return t
    deact env (Lambda l ps ks e)    = Lambda l ps ks <$> deact env e
    deact env (Yield l e)           = Yield l <$> deact env e
    deact env (YieldFrom l e)       = YieldFrom l <$> deact env e
    deact env (Tuple l es ks)       = Tuple l <$> deact env es <*> deact env ks
    deact env (List l es)           = List l <$> deact env es
    deact env (ListComp l e c)      = ListComp l <$> deact env e <*> deact env c
    deact env (Dict l as)           = Dict l <$> deact env as
    deact env (DictComp l a c)      = DictComp l <$> deact env a <*> deact env c
    deact env (Set l es)            = Set l <$> deact env es
    deact env (SetComp l e c)       = SetComp l <$> deact env e <*> deact env c
    deact env (Paren l e)           = Paren l <$> deact env e

instance Deact Pattern where
    deact env (PVar l n a)          = return $ PVar l n a

instance Deact Target where
    deact env (TaVar l n)
      | selfRef n env               = return $ TaDot l (Var l (NoQual selfKW)) n
    deact env (TaVar l n)           = return $ TaVar l n
    deact env (TaIndex l e ix)      = TaIndex l <$> deact env e <*> deact env ix
    deact env (TaSlice l e sl)      = TaSlice l <$> deact env e <*> deact env sl
    deact env (TaDot l e n)         = TaDot l <$> deact env e <*> return n

instance Deact Exception where
    deact env (Exception e mbe)     = Exception <$> deact env e <*> deact env mbe

instance Deact Branch where
    deact env (Branch e ss)         = Branch <$> deact env e <*> deact env ss

instance Deact Handler where
    deact env (Handler ex b)        = Handler ex <$> deact env b

instance Deact PosArg where
    deact env (PosArg e p)          = PosArg <$> deact env e <*> deact env p
    deact env PosNil                = return PosNil

instance Deact KwdArg where
    deact env (KwdArg n e k)        = KwdArg n <$> deact env e <*> deact env k
    deact env KwdNil                = return KwdNil

instance Deact OpArg where
    deact env (OpArg op e)          = OpArg op <$> deact env e

instance Deact Comp where
    deact env (CompFor l p e c)     = CompFor l <$> deact env p <*> deact env e <*> deact env c
    deact env (CompIf l e c)        = CompIf l <$> deact env e <*> deact env c
    deact env NoComp                = return NoComp

instance Deact WithItem where
    deact env (WithItem e p)        = WithItem <$> deact env e <*> deact env p

instance Deact Elem where
    deact env (Elem e)              = Elem <$> deact env e
    deact env (Star e)              = Star <$> deact env e

instance Deact Assoc where
    deact env (Assoc e1 e2)         = Assoc <$> deact env e1 <*> deact env e2
    deact env (StarStar e)          = StarStar <$> deact env e
  
instance Deact Sliz where
    deact env (Sliz l e1 e2 e3)     = Sliz l <$> deact env e1 <*> deact env e2 <*> deact env e3
