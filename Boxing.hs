module Acton.Boxing where

import Acton.Syntax
import Acton.Names
import Acton.Env
import Acton.QuickType
import Acton.Prim
import Acton.Builtin
import Pretty
import Utils
import Control.Monad.State.Lazy
import Debug.Trace

doBoxing                           :: Acton.Env.Env0 -> Module -> IO Module
doBoxing env m                     = return m{mbody = ss}
   where ss                        = runBoxM (boxing (boxEnv env) (mbody m))

type BoxM a                        = State Int a 

newName                             :: String -> BoxM Name
newName s                          = do n <- get
                                        put (n+1)
                                        return $ Internal BoxPass s n

runBoxM m                          = evalState m 0


type BoxEnv                        = EnvF BoxX

data BoxX                          = BoxX{ unboxdefsX :: [Name] }

boxEnv                             :: Env0 -> BoxEnv
boxEnv env0                        = setX env0 BoxX{ unboxdefsX = [] }

hasUnboxedDef n env                = n `elem` unboxdefsX (envX env)

setUnboxedDef n env                = modX env $ \x -> x{ unboxdefsX = n : unboxdefsX x }

unbox _ (Box _ e)                  = e
unbox t e                          = UnBox t e

integralTypes                      = [qnInt, qnI64, qnI32, qnI16, qnI64, qnU32, qnU16]
numericTypes                       =  integralTypes ++ [qnComplex, qnFloat]
unboxableTypes                     = tail numericTypes

class Boxing a where
    boxing :: BoxEnv -> a -> BoxM a

instance (Boxing a, EnvOf a) => Boxing [a] where
    boxing env []                   = return []
    boxing env (x : xs)             = do x'  <- boxing env x
                                         xs' <- boxing env1 xs
                                         return (x' : xs')
      where te                      = envOf x
            env1                    = define te env
    
isWitness (NoQ (Internal Witness _ _)) = True
isWitness _  = False

instance Boxing a => Boxing (Maybe a) where
    boxing env (Just x)           = Just <$> boxing env x
    boxing env Nothing            = return Nothing

boxingFromAtom w tNames [i@Int{}] 
   | t == qnInt                   = return i
   | t `elem` numericTypes        = return $ Box (last tNames) (unbox t i)
   where t = head tNames
boxingFromAtom w tNames [x@Float{}]= return $ Box (last tNames) (unbox (head tNames) x)
boxingFromAtom w t es              = return $ Call NoLoc (eDot (eQVar w) fromatomKW) (posarg es) KwdNil

boxingBinop                       :: QName -> Name -> [Expr] -> [QName] -> BoxM Expr
boxingBinop w attr es@[x1, x2] tNames
   | t `elem` unboxableTypes      = return $ Box (last tNames) $ Paren NoLoc (BinOp NoLoc (unbox t x1) op (unbox t x2)) 
   where t = head tNames
         op = bin2Binary attr
boxingBinop w attr es _           = return $ Call NoLoc (eDot (eQVar w) attr) (posarg es) KwdNil

boxingCompop w attr es@[x1, x2] tNames
   | head tNames `elem` unboxableTypes
                                  = return $ Box qnBool $ Paren NoLoc (CompOp NoLoc (unbox (head tNames) x1) [OpArg op (unbox (head tNames) x2)])
   where op = cmp2Comparison attr
boxingCompop w attr es _          = return $ Call NoLoc (eDot (eQVar w) attr) (posarg es) KwdNil

boxingWitness                      :: BoxEnv -> QName -> Name -> PosArg -> BoxM Expr
boxingWitness env w attr p        = case findQName w env of
                                        NVar (TCon _ (TC _ ts))
                                           | any (not . vFree) ts    -> return (Call NoLoc (eDot (eQVar w) attr) p KwdNil)
                                           | attr == fromatomKW      -> boxingFromAtom w (tNames ts) es
                                           | attr `elem` binopKWs    -> boxingBinop w attr es (tNames ts)
                                           | attr `elem` compareKWs  -> boxingCompop w attr es (tNames ts)
                                        _                            -> return (Call NoLoc (eDot (eQVar w) attr) p KwdNil)
   where tNames ts                = map (tcname . tcon) ts
         es                       = posargs p
         vFree (TCon _ (TC _ _))  = True
         vFree _                  = False

prims = [primISINSTANCE, primISNOTNONE, primISNONE]

qMath str = QName (ModName [name "math"]) (name str)

mathfuns = map qMath ["sqrt", "sin", "cos"]

instance Boxing Expr where
    boxing env (Var l v)            = return $ Var l v
    boxing env (Call _ (Dot _ (Var _ w) attr) p KwdNil)
      | isWitness w                 = do p' <- boxing env p
                                         boxingWitness env w attr p'
    boxing env (Call l e@(TApp _ (Var _ f) ts) p KwdNil)
      | f `elem` prims              = do p' <- boxing env p
                                         return $ Box qnBool $ unbox qnBool (Call l e p' KwdNil)
      | otherwise                   = Call l e <$> boxing env p <*> return KwdNil
    boxing env (Call l v@(Var _ f) p KwdNil)
      | f `elem`prims               = do p' <- boxing env p
                                         return $ Box qnBool $ unbox qnBool $ Call l v p' KwdNil
      | f `elem` mathfuns           = do e' <- boxing env e
                                         return $ Box qnFloat $ eCall v [unbox qnFloat e']
      | otherwise                   = Call l <$> boxing env v <*> boxing env p <*> return KwdNil
       where [e]                    = posargs p
    boxing env (Call l f p KwdNil)  = Call l <$> boxing env f <*> boxing env p <*> return KwdNil
    boxing env (TApp l f ts)        = TApp l <$> boxing env f <*> return ts
    boxing env (Async l e)          = Async l <$> boxing env e
    boxing env (Await l e)          = Await l <$> boxing env e
    boxing env (Index l e1 e2)      = Index l <$> boxing env e1 <*> boxing env e2
    boxing env (Cond l e1 e2 e3)    = Cond l <$> boxing env e1 <*> boxing env e2 <*> boxing env e3
    {-
       | t `elem` unboxableTypes    = do e1' <- boxing env e1
                                         e2' <- boxing env e2
                                         e3' <- boxing env e3
                                         return $ Box t $ Cond l (unbox t e1') (unbox qnBool e2') (unbox t e3')
       | otherwise                  = Cond l <$> boxing env e1 <*> boxing env e2 <*> boxing env e3
         where t                    = tcname (tcon (typeOf env e1))
    -}
    boxing env (IsInstance l e qn)  = IsInstance l <$> boxing env e <*> return qn
    boxing env (BinOp l e1 op e2)   = do e1' <- boxing env e1
                                         e2' <- boxing env e2
                                         return $ Box qnBool $ Paren NoLoc (BinOp l (unbox t e1') op (unbox t e2'))
         where t                    = tcname (tcon (typeOf env e2))
    boxing env (CompOp l e os)      = CompOp l <$> boxing env e <*> boxing env os
    boxing env (UnOp l uop e)       = UnOp l uop <$> boxing env e
    boxing env (Dot l e n)          = Dot l <$> boxing env e <*> return n
    boxing env (Rest l e n)         = Rest l <$> boxing env e <*> return n
    boxing env (DotI l e i)         = DotI l <$> boxing env e <*> return i
    boxing env (RestI l e i)        = RestI l <$> boxing env e <*> return i
    boxing env (Lambda l p k f fx)  = Lambda l <$> boxing env p <*> boxing env k <*> boxing env f <*> return fx
    boxing env (Yield l mbe)        = Yield l <$> boxing env mbe
    boxing env (YieldFrom l e)      = YieldFrom l <$> boxing env e
    boxing env (Tuple l p k)        = Tuple l <$> boxing env p <*> boxing env k
    boxing env (List l es)          = List l <$> boxing env es
    boxing env (Set l es)           = Set l <$> boxing env es
    boxing env (Dict l as)          = Dict l <$> boxing env as
    boxing env (Paren l e)          = Paren l <$> boxing env e
    boxing env (Box _ (UnBox _ e))  = return e
--    boxing env (UnBox _ e)          = error ("boxing applied to Unbox e where e is " ++ show e)
    boxing env e                    = return e

instance Boxing OpArg where
    boxing env (OpArg op e)         = OpArg op <$> boxing env e

instance Boxing Stmt where
    boxing env (Expr l e)           = Expr l <$> boxing env e
    boxing env (Assign l ps e)      = Assign l <$> boxing env ps <*> boxing env e
    boxing env (MutAssign l t e)    = MutAssign l <$> boxing env t <*> boxing env e
    boxing env (AugAssign l t aop e)= AugAssign l <$> boxing env t <*> return aop <*> boxing env e
    boxing env (Assert l e mbe)     = Assert l <$> boxing env e <*> boxing env mbe
    boxing env (Pass l)             = return $ Pass l
    boxing env (Delete l t)         = Delete l <$> boxing env t
    boxing env (Return l mbe)       = Return l <$> boxing env mbe
    boxing env (Raise l e)          = Raise l <$> boxing env e
    boxing env (Break l)            = return $ Break l
    boxing env (Continue l)         = return $ Break l
    boxing env (If l bs ss)         = If l <$> boxing env bs <*> boxing env ss
    boxing env (While l e ss els)   = While l <$> boxing env e <*> boxing env ss <*>  boxing env els
    boxing env (For l p e ss els)   = For l <$> boxing env p <*> boxing env e <*> boxing env ss <*>  boxing env els
    boxing env (Try l ss hs els fs) = Try l <$> boxing env ss <*> boxing env hs <*> boxing env els <*>  boxing env fs
    boxing env (With l ws ss)       = With l <$> boxing env ws <*> boxing env ss
    boxing env (Data l mbp ds)      = Data l <$> boxing env mbp <*> boxing env ds
    boxing env (VarAssign l ps e)   = VarAssign l <$> boxing env ps <*> boxing env e
    boxing env (After l e1 e2)      = After l <$> boxing env e1 <*> boxing env e2
    boxing env (Signature l ns sc d)= Signature l <$> return ns <*> return sc <*> return d
    boxing env g@(Decl l ds)        = Decl l <$> boxing env1 ds
      where te                      = envOf g
            env1                    = define te env

instance Boxing Decl where
    boxing env (Class l n q cs ss)  = Class l n q cs <$> boxing env ss
    boxing env (Def l n q p KwdNIL t ss dec fx)
                                    = Def l n q p KwdNIL t <$> boxing env1 ss <*> return dec <*> return fx
      where te                      = envOf p
            env1                    = define te env

instance Boxing Pattern where
    boxing env (PWild l a)         = return $ PWild l a
    boxing env (PVar l n a)        = return $ PVar l n a
    boxing env (PTuple l ps ks)    = PTuple l <$> boxing env ps <*> boxing env ks
    boxing env (PList l ps p)      = PList l <$> boxing env ps <*> boxing env p  
    boxing env (PParen l p)        = boxing env p

instance Boxing Branch where
    boxing env (Branch e ss)       = Branch <$> boxing env e <*> boxing env ss

instance Boxing Handler where
    boxing env (Handler ex b)      = Handler ex <$> boxing env b

instance Boxing PosPar where
    boxing env (PosPar n t e p)    = PosPar n t <$> boxing env e <*> boxing env p
    boxing env (PosSTAR n t)       = return $ PosSTAR n t
    boxing env PosNIL              = return PosNIL
    
instance Boxing KwdPar where
    boxing env (KwdPar n t e k)    = KwdPar n t <$> boxing env e <*> boxing env k
    boxing env (KwdSTAR n t)       = return $ KwdSTAR n t
    boxing env KwdNIL              = return KwdNIL
                                  
instance Boxing PosPat where
    boxing env (PosPat p ps)       = PosPat <$> boxing env p <*> boxing env ps
    boxing env (PosPatStar p)      = PosPatStar <$> boxing env p
    boxing env PosPatNil           = return PosPatNil
    
instance Boxing KwdPat where
    boxing env (KwdPat n p ps)     = KwdPat n <$> boxing env p <*> boxing env ps
    boxing env (KwdPatStar p)      = KwdPatStar <$> boxing env p
    boxing env KwdPatNil           = return KwdPatNil

instance Boxing PosArg where
    boxing env (PosArg e p)        = PosArg <$> boxing env e <*> boxing env p
    boxing env (PosStar e)         = PosStar <$> boxing env e
    boxing env PosNil              = return PosNil
    
instance Boxing KwdArg where
    boxing env (KwdArg n e k)      = KwdArg n <$> boxing env e <*> boxing env k
    boxing env (KwdStar e)         = KwdStar <$> boxing env e
    boxing env KwdNil              = return KwdNil

instance Boxing Elem where
    boxing env (Elem e)            = Elem <$> boxing env e
    boxing env (Star e)            = Star <$> boxing env e   

instance Boxing Assoc where
    boxing env (Assoc k v)         = Assoc <$> boxing env k <*> boxing env v
    boxing env (StarStar e)        = StarStar <$> boxing env e 

instance Boxing WithItem where
    boxing env (WithItem e mbp)    = WithItem <$> boxing env e <*> boxing env mbp
    

bin2Binary kw
   | kw == addKW                   = Plus
   | kw == subKW                   = Minus
   | kw == mulKW                   = Mult
   | kw == powKW                   = Pow
   | kw == truedivKW               = Div
   | kw == modKW                   = Mod
   | kw == floordivKW              = EuDiv
   | kw == lshiftKW                = ShiftL
   | kw == rshiftKW                = ShiftR
   | kw == orKW                    = BOr
   | kw == xorKW                   = BXor
   | kw == andKW                   = BAnd

cmp2Comparison kw
   | kw == eqKW                    = Eq
   | kw == neKW                    = NEq
   | kw == ltKW                    = Lt
   | kw == leKW                    = LE
   | kw == gtKW                    = Gt
   | kw == geKW                    = GE
   | kw == isKW                    = Is
   | kw == isnotKW                 = IsNot
