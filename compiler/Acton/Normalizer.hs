module Acton.Normalizer where

import Acton.Syntax
import Acton.Names
import Acton.Env hiding (newName)
import Acton.Prim
import Acton.Builtin
import Utils
import Control.Monad.State.Lazy
import Debug.Trace

normalize                           :: (TEnv,Env) -> Module -> IO Module
normalize (te,env0) m               = return $ evalState (norm env m) 0
  where env                         = normEnv (te,env0)

--  Normalization:
--  - All imported or built-in names are qualified by module, including those imported by 'from _ import'
--  - All module aliases are replaced by their original module name
--  X All parameters are positional
--  X Parameter defaults are moved inside function definitions
--  - Comprehensions are translated into loops
--  X String literals are concatenated and delimited by double quotes
--  - Tuple (and list) patterns are replaced by a var pattern followed by explicit element assignments
--  - With statemenmts are replaced by enter/exit prim calls + exception handling
--  X The assert statement is replaced by a prim call ASSERT
--  X The raise statement is replaced by one of prim calls RAISE, RAISEFROM or RERAISE
--  - The delete statement is replaced by (a sequence of) __delitem__ calls (for PIndex) or None assignments
--  X Return without argument is replaced by return None
--  - Incremental assignments are replaced by the corresponding __iop__ calls
--  - The else branch of a while loop is replaced by an explicit if statement enclosing the loop


-- Normalizing monad
type NormM a                        = State Int a

newName                             :: String -> NormM Name
newName s                           = do n <- get
                                         put (n+1)
                                         return $ Internal s n NormPass

                                    -- builtin names are last in global; local names are first in local
data NormEnv                        = NormEnv {global :: TEnv, local :: [Name], currentmod :: ModName} deriving Show

normEnv (te,env)                    = NormEnv (te ++ names env) [] (defaultmod env)

extLocal vs env                     = env{ local = vs ++ local env }

normPat                             :: NormEnv -> Pattern -> NormM (Pattern,Suite)
normPat _ p@(PVar _ _ _)            = return (p,[])
normPat env (PParen _ p)            = normPat env p
normPat env (PTuple _ pp kp)        = do v <- newName "tup"  -- *************** nothing done with kp!!!
                                         return (pVar v Nothing,normPP v 0 pp)
  where normPP v n (PosPat p pp)    = s : normPP v (n+1) pp
          where s                   = Assign NoLoc [p] (DotI NoLoc (eQVar (QName (currentmod env) v)) n False)
        normPP v n (PosPatStar p)   = [Assign NoLoc [p] (DotI NoLoc (eQVar (QName (currentmod env) v)) n True)]
        normPP _ _ PosPatNil        = []
normPat env (PList _ ps pt)         = do v <- newName "lst"
                                         return (pVar v Nothing, normList v 0 ps pt)
  where normList v n (p:ps) pt      = s : normList v (n+1) ps pt
          where s                   = Assign NoLoc [p] (eCall (eDot (eVar (name "Indexed__??")) getitemKW)
                                        [eQVar (QName (currentmod env) v), Int NoLoc n (show n)])
        normList v n [] (Just p)    = [Assign NoLoc [p] (eCall (eDot (eVar (name "Sliceable__??")) getsliceKW)
                                        [eQVar (QName (currentmod env) v), Int NoLoc n (show n), None NoLoc, None NoLoc])]
        normList v n [] Nothing     = [] 

normPat' env Nothing                = return (Nothing, [])
normPat' env (Just p)               = do (p',ss) <- normPat env p
                                         return (Just p', ss)

normItems env []                    = return ([], [])
normItems env (WithItem e p : is)   = do e' <- norm env e
                                         (p',ss) <- normPat' env p
                                         (is',ss') <- normItems (extLocal (bound p) env) is
                                         return (WithItem e' p' : is', ss++ss')


class Norm a where
    norm                            :: NormEnv -> a -> NormM a
    norm'                           :: NormEnv -> a -> NormM [a]
    norm' env x                     = (:[]) <$> norm env x

instance Norm a => Norm [a] where
    norm env xs                     = concat <$> mapM (norm' env) xs

instance Norm a => Norm (Maybe a) where
    norm env Nothing                = return Nothing
    norm env (Just a)               = Just <$> norm env a

instance Norm Module where
    norm env (Module qn imps ss)    = Module <$> norm env qn <*> norm env imps <*> norm env ss

instance Norm Import where
    norm env (Import l ms)          = Import l <$> norm env ms
    norm env (FromImport l m ns)    = FromImport l <$> norm env m <*> norm env ns
    norm env (FromImportAll l m)    = FromImportAll l <$> norm env m

instance Norm Stmt where
    norm env (Expr l e)             = Expr l <$> norm env e
    norm env (Update l ts e)        = Update l <$> norm env ts <*> norm env e
    norm env (IUpdate l t op e)     = IUpdate l <$> norm env t <*> norm env op <*> norm env e
    norm env (Assert l e mbe)       = do e' <- norm env e
                                         mbe' <- norm env mbe
                                         return $ Expr l $ eCall (eQVar primASSERT) [e', maybe eNone id mbe']
    norm env (Pass l)               = return $ Pass l
    norm env (Delete l p)           = Delete l <$> norm env p
    norm env (Return l Nothing)     = return $ Return l $ Just $ None l0
    norm env (Return l (Just e))    = do e' <- norm env e
                                         return $ Return l $ Just e'
    norm env (Raise l mbex)         = do mbex' <- norm env mbex
                                         case mbex' of
                                            Nothing ->
                                               return $ Expr l $ eCall (eQVar primRERAISE) []
                                            Just (Exception e Nothing) ->
                                               return $ Expr l $ eCall (eQVar primRAISE) [e]
                                            Just (Exception e (Just e')) -> 
                                               return $ Expr l $ eCall (eQVar primRAISEFROM) [e,e']
    norm env (Break l)              = return $ Break l
    norm env (Continue l)           = return $ Continue l
    norm env (If l bs els)          = If l <$> norm env bs <*> norm env els
    norm env (While l e b els)      = While l <$> norm env e <*> norm env b <*> norm env els
    norm env (Try l b hs els fin)   = Try l <$> norm env b <*> norm env hs <*> norm env els <*> norm env fin
    norm env (For l p e b els)      = do (v,ss) <- normPat env p
                                         e' <- norm env e
                                         b' <- norm env1 (ss ++ b)
                                         els' <- norm env els
                                         return $ For l v e' b' els'
      where env1                    = extLocal (bound p) env
    norm env (With l is b)          = do (is',ss) <- normItems env is
                                         b' <- norm env1 (ss ++ b)
                                         return $ With l is' b'
      where env1                    = extLocal (bound is) env
    norm env (Data l mbt ss)        = Data l <$> norm env mbt <*> norm env ss
    norm env (VarAssign l ps e)     = VarAssign l <$> norm env ps <*> norm env e
    norm env (After l e n ps ks)    = After l <$> norm env e <*> return n <*> norm env ps <*> norm env ks
    norm env (Decl l ds)            = Decl l <$> norm env ds

--    norm' env (Delete l p)          =

    norm' env (Assign l ts e)       = do e' <- norm env e
                                         ps <- mapM (normPat env) ts
                                         let (vs,sss) = unzip ps
                                         ss' <- norm env (concat sss)
                                         return $ Assign l vs e' : ss'
    norm' env s                     = do s' <- norm env s
                                         return [s']
                                         
                                         
                                       
instance Norm Decl where
    norm env (Def l n q p k t b m)  = do p' <- joinPar <$> norm env p <*> norm (extLocal (bound p) env) k
                                         b' <- norm env1 b
                                         return $ Def l n q (noDefaults p') KwdNIL t (defaults p' ++ b') m
      where env1                    = extLocal (bound b ++ bound p ++ bound k) env
    norm env (Actor l n q p k t b)  = do p' <- joinPar <$> norm env p <*> norm (extLocal (bound p) env) k
                                         b' <- norm env1 b
                                         return $ Actor l n q (noDefaults p') KwdNIL t (defaults p' ++ b')
      where env1                    = extLocal (bound b ++ bound p ++ bound k) env
    norm env (Class l n q as b)     = Class l n q as <$> norm env b
    norm env (Protocol l n q as b)  = Protocol l n q as <$> norm env b
    norm env (Extension l n q as b) = Extension l n q as <$> norm env b
    norm env (Signature l ns t)     = return $ Signature l ns t

catStrings ['"':s]                  = '"' : s
catStrings ss                       = '"' : (escape '"' (concatMap stripQuotes ss)) ++ ['"']
  where escape c []                 = []
        escape c ('\\':x:xs)        = '\\' : x : escape c xs
        escape c (x:xs)
          | x == c                  = '\\' : x : escape c xs
          | otherwise               = x : escape c xs
        stripQuotes s               = init $ tail s

instance Norm Expr where
    norm env (Var l nm)             = Var l <$> norm env nm
    norm env (Int l i s)            = Int l <$> return i <*> return s
    norm env (Float l f s)          = Float l <$> return f <*> return s
    norm env (Imaginary l i s)      = Imaginary l <$> return i <*> return s
    norm env (Bool l b)             = Bool l <$> return b
    norm env (None l)               = return $ None l
    norm env (NotImplemented l)     = return $ NotImplemented l
    norm env (Ellipsis l)           = return $ Ellipsis l
    norm env (Strings l ss)         = return $ Strings l [catStrings ss]
    norm env (BStrings l ss)        = return $ BStrings l [catStrings ss]
    norm env (Call l e ps ks)       = Call l <$> norm env e <*> norm env ps <*> norm env ks
    norm env (Cond l e1 e2 e3)      = Cond l <$> norm env e1 <*> norm env e2 <*> norm env e3
    norm env (BinOp l e1 op e2)     = BinOp l <$> norm env e1 <*> norm env op <*> norm env e2   -- only Or,And
    norm env (UnOp l op e)          = UnOp l <$> norm env op <*> norm env e                     -- only Not
    norm env (Dot l e nm)           = Dot l <$> norm env e <*> norm env nm
    norm env (DotI l e i t)         = DotI l <$> norm env e <*> return i <*> return t
    norm env (Lambda l ps ks e)     = Lambda l <$> norm env ps <*> norm (extLocal (bound ps) env) ks <*> norm env1 e
      where env1                    = extLocal (bound ps ++ bound ks) env
    norm env (Yield l e)            = Yield l <$> norm env e
    norm env (YieldFrom l e)        = YieldFrom l <$> norm env e
    norm env (Tuple l es ks)        = Tuple l <$> norm env es <*> norm env ks
    norm env (List l es)            = List l <$> norm env es
    norm env (ListComp l e c)       = ListComp l <$> norm env1 e <*> norm env c
      where env1                    = extLocal (bound c) env
    norm env (Dict l as)            = Dict l <$> norm env as
    norm env (DictComp l a c)       = DictComp l <$> norm env1 a <*> norm env c
      where env1                    = extLocal (bound c) env
    norm env (Set l es)             = Set l <$> norm env es
    norm env (SetComp l e c)        = SetComp l <$> norm env1 e <*> norm env c
      where env1                    = extLocal (bound c) env
    norm env (Paren l e)            = Paren l <$> norm env e
    norm env e                      = error ("trying to normalize " ++ show e)

instance Norm Pattern where
    norm env (PVar l n a)           = return $ PVar l n a
    norm env (PTuple l ps ks)       = PTuple l <$> norm env ps <*> norm env ks
    norm env (PList l ps p)         = PList l <$> norm env ps <*> norm env p
    norm env (PParen l p)           = PParen l <$> norm env p

instance Norm Target where
    norm env (TaVar l n)            = return $ TaVar l n
    norm env (TaIndex l e ix)       = TaIndex l <$> norm env e <*> norm env ix
    norm env (TaSlice l e sl)       = TaSlice l <$> norm env e <*> norm env sl
    norm env (TaDot l e n)          = TaDot l <$> norm env e <*> norm env n
    norm env (TaDotI l e i tl)      = TaDotI l <$> norm env e <*> return i <*> return tl
    norm env (TaTuple l ps)         = TaTuple l <$> norm env ps
    norm env (TaParen l p)          = TaParen l <$> norm env p

instance Norm Exception where
    norm env (Exception e mbe)      = Exception <$> norm env e <*> norm env mbe

instance Norm Name where
    norm env (Name l s)             = return $ Name l s
    norm env (Internal s i p)       = return $ Internal s i p

instance Norm ModName where
    norm env m@(ModName [n])        = case lookupM n (global env) of
                                          Just (NMAlias m') -> return m'
                                          _ -> return m
    norm env (ModName ns)           = ModName <$> norm env ns

instance Norm QName where
    norm env (QName m n)            = QName <$> norm env m <*> norm env n
    norm env (NoQual n)             = case elem n (local env) of
                                         True-> return $ NoQual n
                                         False ->  case lookup n (global env) of
                                                      Just (NAlias qn) -> return qn
                                                      Just _  -> return $ QName (currentmod env) n
                                                      Nothing -> return $ NoQual n

instance Norm ModRef where
    norm env (ModRef (n,mbqn))      = (\m -> ModRef (n,m)) <$> norm env mbqn

instance Norm ModuleItem where
    norm env (ModuleItem qn mbn)    = ModuleItem <$> norm env qn <*> norm env mbn

instance Norm ImportItem where
    norm env (ImportItem nm mbn)    = ImportItem <$> norm env nm <*> norm env mbn

instance Norm (Op a) where
    norm env (Op l a)               = Op l <$> return a

instance Norm Branch where
    norm env (Branch e ss)          = Branch <$> norm env e <*> norm env ss

instance Norm Handler where
    norm env (Handler ex b)         = Handler <$> norm env ex <*> norm env1 b
      where env1                    = extLocal (bound ex) env

instance Norm Except where
    norm env (ExceptAll l)          = return $ ExceptAll l
    norm env (Except l x)           = Except l <$> norm env x
    norm env (ExceptAs l x n)       = ExceptAs l <$> norm env x <*> norm env n

instance Norm PosPar where
    norm env (PosPar n t e p)       = PosPar n t <$> norm env e <*> norm (extLocal [n] env) p
    norm env (PosSTAR n t)          = return $ PosSTAR n t
    norm env PosNIL                 = return PosNIL
    
instance Norm KwdPar where
    norm env (KwdPar n t e k)       = KwdPar n t <$> norm env e <*> norm (extLocal [n] env) k
    norm env (KwdSTAR n t)          = return $ KwdSTAR n t
    norm env KwdNIL                 = return KwdNIL

joinPar (PosPar n t e p) k          = PosPar n t e (joinPar p k)
joinPar (PosSTAR n t) k             = PosPar n (fmap monotype t) Nothing (kwdToPos k)
joinPar PosNIL k                    = kwdToPos k

kwdToPos (KwdPar n t e k)           = PosPar n t e (kwdToPos k)
kwdToPos (KwdSTAR n t)              = PosPar n (fmap monotype t) Nothing PosNIL
kwdToPos KwdNIL                     = PosNIL

defaults (PosPar n t (Just e) p)    = s : defaults p
  where s                           = sIf1 test [set] []
        test                        = eCall (eQVar primIsNone) [eVar n]
        set                         = sAssign [pVar n Nothing] e
defaults (PosPar n t Nothing p)     = defaults p
defaults _                          = []

noDefaults (PosPar n t _ p)         = PosPar n t Nothing (noDefaults p)
noDefaults p                        = p

instance Norm PosArg where
    norm env (PosArg e p)           = PosArg <$> norm env e <*> norm env p
    norm env (PosStar e)            = PosStar <$> norm env e
    norm env PosNil                 = return PosNil
    
instance Norm KwdArg where
    norm env (KwdArg n e k)         = KwdArg n <$> norm env e <*> norm env k
    norm env (KwdStar e)            = KwdStar <$> norm env e
    norm env KwdNil                 = return KwdNil
    
instance Norm PosPat where
    norm env (PosPat p ps)          = PosPat <$> norm env p <*> norm env ps
    norm env (PosPatStar p)         = PosPatStar <$> norm env p
    norm env PosPatNil              = return PosPatNil
    
instance Norm KwdPat where
    norm env (KwdPat n p ps)        = KwdPat n <$> norm env p <*> norm env ps
    norm env (KwdPatStar p)         = KwdPatStar <$> norm env p
    norm env KwdPatNil              = return KwdPatNil
    
instance Norm OpArg where
    norm env (OpArg op e)           = OpArg <$> norm env op <*> norm env e

instance Norm Comp where
    norm env (CompFor l p e c)      = CompFor l <$> norm env p <*> norm env e <*> norm (extLocal (bound p) env) c
    norm env (CompIf l e c)         = CompIf l <$> norm env e <*> norm env c
    norm env NoComp                 = return NoComp

instance Norm WithItem where
    norm env (WithItem e p)         = WithItem <$> norm env e <*> norm env p

instance Norm Elem where
    norm env (Elem e)               = Elem <$> norm env e
    norm env (Star e)               = Star <$> norm env e

instance Norm Assoc where
    norm env (Assoc e1 e2)          = Assoc <$> norm env e1 <*> norm env e2
    norm env (StarStar e)           = StarStar <$> norm env e
  
instance Norm Sliz where
    norm env (Sliz l e1 e2 e3)      = Sliz l <$> norm env e1 <*> norm env e2 <*> norm env e3
{-
instance Norm TSchema where
    norm env (TSchema l q t d)      = TSchema l <$> norm env q <*> norm env t <*> return d

instance Norm TVar where
    norm env (TV n)                 = TV <$> norm env n

instance Norm TCon where
    norm env (TC n ts)              = TC n <$> norm env ts

instance Norm TBind where
    norm env (TBind v cs)           = TBind <$> norm env v <*> norm env cs

instance Norm Type where
    norm env (TVar l v)             = TVar l <$> norm env v
    norm env (TFun l es p k t)      = TFun l <$> norm env es <*> norm env p <*> norm env k <*> norm env t
    norm env (TTuple l p k)         = TTuple l <$> norm env p <*> norm env k
    norm env (TOpt l t)             = TOpt l <$> norm env t
    norm env (TUnion l as)          = TUnion l <$> return as
    norm env (TCon  l c)            = TCon l <$> norm env c
    norm env (TNone l)              = return $ TNone l
    norm env (TWild l)              = return $ TWild l
    norm env (TNil l)               = return $ TNil l
    norm env (TRow l n t r)         = TRow l n <$> norm env t <*> norm env r
-}