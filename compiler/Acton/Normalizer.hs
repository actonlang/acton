module Acton.Normalizer where

import Acton.Syntax
import Acton.Names
import Acton.Env hiding (newName)
import Acton.Prim
import Acton.Builtin
import Utils
import Control.Monad.State.Lazy
import Debug.Trace

normalize                           :: (TEnv,Env0) -> Module -> IO Module
normalize (te,env0) m               = return $ evalState (norm env m) 0
  where env                         = normEnv (te,env0)

--  Normalization:
--  X All module aliases are replaced by their original module name
--  X All parameters are positional
--  X Parameter defaults are moved inside function definitions
--  - Comprehensions are translated into loops
--  X String literals are concatenated and delimited by double quotes
--  X Tuple (and list) patterns are replaced by a var pattern followed by explicit element assignments
--  - With statemenmts are replaced by enter/exit prim calls + exception handling
--  X The assert statement is replaced by a prim call ASSERT
--  X The raise statement is replaced by one of prim calls RAISE, RAISEFROM or RERAISE
--  X Return without argument is replaced by return None
--  - The else branch of a while loop is replaced by an explicit if statement enclosing the loop


-- Normalizing monad
type NormM a                        = State Int a

newName                             :: String -> NormM Name
newName s                           = do n <- get
                                         put (n+1)
                                         return $ Internal NormPass s n

                                    -- builtin names are last in global; local names are first in local
data NormEnv                        = NormEnv { global :: TEnv, local :: [Name] } deriving Show

normEnv (te,env)                    = NormEnv (te ++ names env) []

extLocal vs env                     = env{ local = vs ++ local env }

normPat                             :: NormEnv -> Pattern -> NormM (Pattern,Suite)
normPat _ p@(PVar _ _ _)            = return (p,[])
normPat env (PParen _ p)            = normPat env p
normPat env (PTuple _ pp kp)        = do v <- newName "tup"
                                         ss <- norm env $ normPP v 0 pp ++ normKP v [] kp
                                         return (pVar v Nothing, ss)                                    -- TODO: provide a type for v
  where normPP v n (PosPat p pp)    = Assign NoLoc [p] (DotI NoLoc (eVar v) n) : normPP v (n+1) pp
        normPP v n (PosPatStar p)   = [Assign NoLoc [p] (foldl (RestI NoLoc) (eVar v) [0..n-1])]
        normPP _ _ PosPatNil        = []
        normKP v ns (KwdPat n p kp) = Assign NoLoc [p] (Dot NoLoc (eVar v) n) : normKP v (n:ns) kp
        normKP v ns (KwdPatStar p)  = [Assign NoLoc [p] (foldl (Rest NoLoc) (eVar v) (reverse ns))]
        normKP _ _ KwdPatNil        = []
normPat env (PList _ ps pt)         = do v <- newName "lst"
                                         ss <- norm env $ normList v 0 ps pt
                                         return (pVar v Nothing, ss)                                    -- TODO: provide a type for v
  where normList v n (p:ps) pt      = s : normList v (n+1) ps pt
          where s                   = Assign NoLoc [p] (eCall (eDot (eQVar qnIndexed) getitemKW)
                                        [eVar v, Int NoLoc n (show n)])
        normList v n [] (Just p)    = [Assign NoLoc [p] (eCall (eDot (eQVar qnSliceable) getsliceKW)
                                        [eVar v, Int NoLoc n (show n), None NoLoc, None NoLoc])]
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
    norm env (Module m imps ss)     = Module m <$> norm env imps <*> norm env ss

instance Norm Import where
    norm env (Import l ms)          = Import l <$> norm env ms
    norm env (FromImport l m ns)    = FromImport l <$> norm env m <*> norm env ns
    norm env (FromImportAll l m)    = FromImportAll l <$> norm env m

instance Norm Stmt where
    norm env (Expr l e)             = Expr l <$> norm env e
    norm env (MutAssign l t e)      = MutAssign l <$> norm env t <*> norm env e
    norm env (Assert l e mbe)       = do e' <- norm env e
                                         mbe' <- norm env mbe
                                         return $ Expr l $ eCall (eQVar primASSERT) [e', maybe eNone id mbe']
    norm env (Pass l)               = return $ Pass l
    norm env (Delete l t)           = Delete l <$> norm env t
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
    norm env (After l e e')         = After l <$> norm env e <*> norm env e'
    norm env (Decl l ds)            = Decl l <$> norm env ds
    norm env (Signature l ns t d)   = return $ Signature l ns t d
    norm env s                      = error ("norm unexpected: " ++ prstr s)    

    norm' env (Assign l ts e)       = do e' <- norm env e
                                         ps <- mapM (normPat env) ts
                                         let (vs,sss) = unzip ps
                                         ss' <- norm env (concat sss)
                                         return $ Assign l vs e' : ss'
    norm' env s                     = do s' <- norm env s
                                         return [s']


instance Norm Decl where
    norm env (Def l n q p k t b d x)= do p' <- joinPar <$> norm env p <*> norm (extLocal (bound p) env) k
                                         b' <- norm env1 b
                                         return $ Def l n q (noDefaults p') KwdNIL t (defaults p' ++ b') d x
      where env1                    = extLocal (bound b ++ bound p ++ bound k) env
    norm env (Actor l n q p k b)    = do p' <- joinPar <$> norm env p <*> norm (extLocal (bound p) env) k
                                         b' <- norm env1 b
                                         return $ Actor l n q (noDefaults p') KwdNIL (defaults p' ++ b')
      where env1                    = extLocal (bound b ++ bound p ++ bound k) env
    norm env (Class l n q as b)     = Class l n q as <$> norm env b
    norm env d                      = error ("norm unexpected: " ++ prstr d)



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
    norm env (Call l e ps ks)       = Call l <$> norm env e <*> norm env (joinArg ps ks) <*> pure KwdNil
    norm env (TApp l e ts)          = TApp l <$> norm env e <*> pure ts
    norm env (Await l e)            = Await l <$> norm env e
    norm env (Cond l e1 e2 e3)      = Cond l <$> norm env e1 <*> norm env e2 <*> norm env e3
    norm env (IsInstance l e c)     = IsInstance l <$> norm env e <*> pure c
    norm env (BinOp l e1 Or e2)     = BinOp l <$> norm env e1 <*> pure Or <*> norm env e2
    norm env (BinOp l e1 And e2)    = BinOp l <$> norm env e1 <*> pure And <*> norm env e2
    norm env (UnOp l Not e)         = UnOp l Not <$> norm env e
    norm env (Dot l e nm)           = Dot l <$> norm env e <*> norm env nm
    norm env (Rest l e nm)          = Rest l <$> norm env e <*> norm env nm
    norm env (DotI l e i)           = DotI l <$> norm env e <*> return i
    norm env (RestI l e i)          = RestI l <$> norm env e <*> return i
    norm env (Lambda l p k e fx)    = do p' <- joinPar <$> norm env p <*> norm (extLocal (bound p) env) k
                                         Lambda l (noDefaults p') KwdNIL <$> norm env1 e <*> return fx      -- TODO: replace defaulted params with Conds
      where env1                    = extLocal (bound p ++ bound k) env
    norm env (Yield l e)            = Yield l <$> norm env e
    norm env (YieldFrom l e)        = YieldFrom l <$> norm env e
    norm env (Tuple l es ks)        = Tuple l <$> norm env es <*> norm env ks
    norm env (List l es)            = List l <$> norm env es
    norm env (ListComp l e c)       = ListComp l <$> norm env1 e <*> norm env c
      where env1                    = extLocal (bound c) env
    norm env (Paren l e)            = Paren l <$> norm env e
    norm env e                      = error ("norm unexpected: " ++ prstr e)

instance Norm Pattern where
    norm env (PVar l n a)           = return $ PVar l n a
    norm env (PTuple l ps ks)       = PTuple l <$> norm env ps <*> norm env ks
    norm env (PList l ps p)         = PList l <$> norm env ps <*> norm env p
    norm env (PParen l p)           = PParen l <$> norm env p

instance Norm Exception where
    norm env (Exception e mbe)      = Exception <$> norm env e <*> norm env mbe

instance Norm Name where
    norm env n                      = return n

instance Norm ModName where
    norm env m@(ModName [n])        = case lookup n (global env) of
                                          Just (NMAlias m') -> return m'
                                          _ -> return m
    norm env (ModName ns)           = ModName <$> norm env ns

instance Norm QName where
    norm env (QName m n)            = QName <$> norm env m <*> norm env n
    norm env (NoQ n)                = NoQ <$> norm env n

instance Norm ModRef where
    norm env (ModRef (n,mbqn))      = (\m -> ModRef (n,m)) <$> norm env mbqn

instance Norm ModuleItem where
    norm env (ModuleItem qn mbn)    = ModuleItem <$> norm env qn <*> norm env mbn

instance Norm ImportItem where
    norm env (ImportItem nm mbn)    = ImportItem <$> norm env nm <*> norm env mbn

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
joinPar (PosSTAR n t) k             = PosPar n t Nothing (kwdToPosPar k)
joinPar PosNIL k                    = kwdToPosPar k

kwdToPosPar (KwdPar n t e k)        = PosPar n t e (kwdToPosPar k)
kwdToPosPar (KwdSTAR n t)           = PosPar n t Nothing PosNIL
kwdToPosPar KwdNIL                  = PosNIL

joinArg (PosArg e p) k              = PosArg e (joinArg p k)
joinArg (PosStar e) k               = PosArg e (kwdToPosArg k)
joinArg PosNil k                    = kwdToPosArg k

kwdToPosArg (KwdArg n e k)          = PosArg e (kwdToPosArg k)
kwdToPosArg (KwdStar e)             = PosArg e PosNil
kwdToPosArg KwdNil                  = PosNil

defaults (PosPar n t (Just e) p)    = s : defaults p
  where s                           = sIf1 test [set] []
        test                        = eCall (eDot (eQVar witIdentityOpt) isnotKW) [eVar n,eNone]
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
    norm env (OpArg op e)           = OpArg op <$> norm env e

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

instance Norm QBind where
    norm env (Quant v cs)           = Quant <$> norm env v <*> norm env cs

instance Norm Constraint where
    norm env (Cast t t')            = Cast <$> norm env t <*> norm env t'
    norm env (Sub w t t')           = Sub w <$> norm env t <*> norm env t'
    norm env (Impl w t p)           = Impl w <$> norm env t <*> norm env p
    norm env (Sel w t n t')         = Sel w <$> norm env t <*> return n <*> norm env t'
    norm env (Mut t n t')           = Mut <$> norm env t <*> return n <*> norm env t'

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