{-# LANGUAGE FlexibleInstances #-}
module Acton.Boxing where

import Acton.Syntax
import Acton.Names
import Acton.Env
import Acton.QuickType
import Acton.Prim
import Acton.Builtin
import Pretty
import Utils
import Debug.Trace
import Control.Monad.State.Strict
import Control.Monad.Except

doBoxing                           :: Acton.Env.Env0 -> Module -> IO Module
doBoxing env m                     = return m{mbody = ss}
   where (_,ss)                    = runBoxM (boxing (boxEnv env) (mbody m))
                                     
-- Boxing monad; state not yet used, but prepared for introducing unboxed variables ------------------------------

type BoxM a                        = State Int a 

newName                             :: String -> BoxM Name
newName s                          = do n <- get
                                        put (n+1)
                                        return $ Internal BoxPass s n

runBoxM ss                         = evalState ss 0


type BoxEnv                        = EnvF () 

boxEnv                             :: Env0 -> BoxEnv
boxEnv env0                        = setX env0 ()

-- Auxiliaries ----------------------------------------------------

integralTypes                      = [qnInt, qnI64, qnI32, qnI16, qnI64, qnU32, qnU16]
numericTypes                       = integralTypes ++ [qnFloat]   -- qnComplex omitted for now
unboxableTypes                     = tail numericTypes


class UnboxClass a where
    unbox :: QName -> a -> a

instance UnboxClass Expr where
   unbox _ (Box _ e)               = e
   unbox t e                       = UnBox t e

instance UnboxClass PosArg where
   unbox t (PosArg e p)            = PosArg (unbox t e) (unbox t p)
   unbox t (PosStar e)             = PosStar (unbox t e)
   unbox t PosNil                  = PosNil
    
-- Walking the syntax tree to place Box/UnBox annotations and sometimes restructure code to mimic C code on unboxed values ---------------

class Boxing a where
    boxing :: BoxEnv -> a -> BoxM ([Name],a)

instance {-# OVERLAPS #-} Boxing ([Stmt]) where
    boxing env []                   = return ([],[])
    boxing env (x@(Assign _ [PVar _ n _] _) : xs)
       | isWitness (NoQ n)            = do (ws1,x') <- boxing env x
                                           (ws2,xs') <- boxing env1 xs
                                           return $ if n `elem` ws2 then (ws1++ws2,x':xs') else (ws2,xs')
      where te                       = envOf x
            env1                     = define te env
    boxing env (x : xs)              = do (ws1,x') <- boxing env x
                                          (ws2,xs') <- boxing env1 xs
                                          return (ws1++ws2, x':xs')   
      where te                       = envOf x
            env1                     = define te env


instance (Boxing a) => Boxing ([a]) where
    boxing env []                   = return ([],[])
    boxing env (x : xs)             = do (ws1,x1)  <- boxing env x
                                         (ws2,xs2) <- boxing env xs
                                         return (ws1++ws2, x1:xs2)

    
isWitness (NoQ (Internal Witness _ _)) = True
isWitness _  = False

instance Boxing a => Boxing (Maybe a) where
    boxing env (Just x)           = do (ws1, x1) <- boxing env x
                                       return (ws1, Just x1)
    boxing env Nothing            = return ([], Nothing)

boxingFromAtom w tNames [i@Int{}] ws
   | t == qnInt                   = return (ws, i)
   | t `elem` numericTypes        = return (ws, Box (last tNames) (unbox t i))
   where t = head tNames
boxingFromAtom w tNames [x@Float{}] ws
   | t `elem` numericTypes         = return (ws, Box (last tNames) (unbox (head tNames) x))
   where t = head tNames              -- need the guard to avoid ndarray...
boxingFromAtom w t es ws           = return (noq w : ws, Call NoLoc (eDot (eQVar w) fromatomKW) (posarg es) KwdNil)

boxingBinop                       :: QName -> Name -> [Expr] -> [QName] -> [Name] -> BoxM ([Name], Expr)
boxingBinop w attr es@[x1, x2] tNames ws
   | t `elem` unboxableTypes       = return (ws, Box (last tNames) $ Paren NoLoc (BinOp NoLoc (unbox t x1) op (unbox t x2)))
   where t = head tNames
         op = bin2Binary attr
boxingBinop w attr es _ ws         = return (noq w : ws, Call NoLoc (eDot (eQVar w) attr) (posarg es) KwdNil)

boxingCompop w attr es@[x1, x2] tNames ws
   | head tNames `elem` unboxableTypes
                                  = return (ws, Box qnBool $ Paren NoLoc (CompOp NoLoc (unbox (head tNames) x1) [OpArg op (unbox (head tNames) x2)]))
   where op = cmp2Comparison attr
boxingCompop w attr es _ ws       = return (noq w : ws, Call NoLoc (eDot (eQVar w) attr) (posarg es) KwdNil)

boxingWitness                      :: BoxEnv -> QName -> Name -> [Name] ->PosArg -> BoxM ([Name],Expr)
boxingWitness env w attr ws p        = case findQName w env of
                                        NVar (TCon _ (TC pr ts))
                                           | any (not . vFree) ts    -> return (noq w : ws, Call NoLoc (eDot (eQVar w) attr) p KwdNil)
                                           | attr == fromatomKW      -> boxingFromAtom w (tNames ts) es ws
                                           | attr `elem` binopKWs    -> boxingBinop w attr es (tNames ts) ws
                                           | attr `elem` compareKWs  -> boxingCompop w attr es (tNames ts) ws
                                        _                            -> return (noq w : ws, Call NoLoc (eDot (eQVar w) attr) p KwdNil)
   where tNames ts                = map (tcname . tcon) ts
         es                       = posargs p
         vFree (TCon _ (TC _ _))  = True
         vFree _                  = False

prims = [primISINSTANCE, primISNOTNONE, primISNONE]

qMath str = QName (ModName [name "math"]) (name str)
 
mathfuns = map qMath ["sqrt", "exp", "log", "sin", "cos", "tan", "asin", "acos", "atan",  "sinh", "cosh", "tanh",  "asinh", "acosh", "atanh"]

instance Boxing Expr where
    boxing env (Var l v@(NoQ n))
       | isWitness v                = return ([n],Var l v)
    boxing env (Var l v)            = return ([],Var l v)
    boxing env (Call _ (Dot _ (Var _ w) attr) p KwdNil)
      | isWitness w                 = do (ws1,p1) <- boxing env p
                                         (ws2,e1) <- boxingWitness env w attr ws1 p1
                                         return (ws1++ws2,e1)
    boxing env (Call l e@(TApp _ (Var _ f) ts) p KwdNil)
      | f `elem` prims              = do (ws1,p1) <- boxing env p
                                         return (ws1,Box qnBool $ unbox qnBool (eCallP e p1))
      | otherwise                   = do (ws1,p1) <- boxing env p
                                         return (ws1, eCallP e p1)
    boxing env (Call l e@(Var _ f) p KwdNil)
      | f `elem`prims               = do (ws1,p1) <- boxing env p
                                         return (ws1,Box qnBool $ unbox qnBool (eCallP e p1))
      | f `elem` mathfuns           = do (ws1,p1)  <- boxing env p
                                         return (ws1,Box qnFloat $ eCallP e (unbox qnFloat p1))
      | otherwise                   = do (ws1,p1) <- boxing env p
                                         return (ws1, eCallP e p1)
    boxing env (Call l f p KwdNil)  = do (ws1,f1) <- boxing env f
                                         (ws2,p1) <- boxing env p
                                         return (ws1++ws2, eCallP f1 p1)
    boxing env (TApp l f ts)        = do (ws1,f1) <- boxing env f
                                         return (ws1, TApp l f1 ts)
    boxing env (Async l e)          = do (ws1,e1) <- boxing env e
                                         return (ws1, Async l e1)
    boxing env (Await l e)          = do (ws1,e1) <- boxing env e
                                         return (ws1, Await l e1)
    boxing env (Index l e1 e2)      = do (ws1,e1') <- boxing env e1
                                         (ws2,e2') <- boxing env e2
                                         return (ws1++ws2, Index l e1' e2')
    boxing env (Cond l e1 e2 e3)    = do (ws1,e1') <- boxing env e1
                                         (ws2,e2') <- boxing env e2
                                         (ws3,e3') <- boxing env e3
                                         return (ws1++ws2++ws3, Cond l e1' e2' e3')
    boxing env (IsInstance l e qn)  = do (ws1,e1) <- boxing env e
                                         return (ws1, IsInstance l e1 qn)
    boxing env (BinOp l e1 op e2)   = do (ws1,e1') <- boxing env e1
                                         (ws2,e2') <- boxing env e2
                                         return (ws1++ws2, Box qnBool $ Paren NoLoc (BinOp l (unbox t e1') op (unbox t e2')))
         where t                    = tcname (tcon (typeOf env e2))
    boxing env (CompOp l e os)      = do (ws1,e1) <- boxing env e
                                         (ws2,e2) <- boxing env os
                                         return (ws1++ws2, CompOp l e1 e2)
    boxing env (UnOp l uop e)       = do (ws1,e1) <- boxing env e
                                         return (ws1, UnOp l uop e1)
    boxing env (Dot l e n)          = do (ws1,e1) <- boxing env e
                                         return (ws1, Dot l e1 n)
    boxing env (DotI l e i)         = do (ws1,e1) <- boxing env e
                                         return (ws1, DotI l e1 i)
    boxing env (Rest l e n)         = do (ws1,e1) <- boxing env e
                                         return (ws1, Rest l e1 n)
    boxing env (RestI l e i)        = do (ws1,e1) <- boxing env e
                                         return (ws1, RestI l e1 i)
--    boxing env (Lambda l p k f fx)  = Lambda l <$> boxing env p <*> boxing env k <*> boxing env f <*> return fx
--    boxing env (Yield l mbe)        = Yield l <$> boxing env mbe
--    boxing env (YieldFrom l e)      = YieldFrom l <$> boxing env e
    boxing env (Tuple l p k)        = do (ws1,p1) <- boxing env p
                                         return (ws1,Tuple l p1 k)
    boxing env (List l es)          = do (ws1,es1) <- boxing env es
                                         return (ws1, List l es1)
    boxing env (Set l es)           = do (ws1,es1) <- boxing env es
                                         return (ws1, Set l es1)
    boxing env (Dict l es)          = do (ws1,es1) <- boxing env es
                                         return (ws1, Dict l es1)
    boxing env (Paren l e)          = do (ws1,e1) <- boxing env e
                                         return (ws1, Paren l e1)
    boxing env (Box _ (UnBox _ e))  = do (ws1,e1) <- boxing env e
                                         return (ws1, e1)
    boxing env e                    = return ([],e)

instance Boxing OpArg where
    boxing env (OpArg op e)         = do (ws1,e1) <- boxing env e
                                         return (ws1, OpArg op e1)

instance Boxing Stmt where
    boxing env (Expr l e)           = do (ws1,e1) <- boxing env e
                                         return (ws1, Expr l e1)
    boxing env (Assign l ps e)      = do (ws1,e1) <- boxing env e
                                         return (ws1, Assign l ps e1)
    boxing env (MutAssign l t e)    = do (ws1,e1) <- boxing env e
                                         return (ws1, MutAssign l t e1)
    boxing env (AugAssign l t aop e)= do (ws1,e1) <- boxing env e
                                         return (ws1, AugAssign l t aop e1)
    boxing env (Assert l e mbe)     = do (ws1,e1) <- boxing env e
                                         (ws2,mbe1) <- boxing env mbe
                                         return (ws1++ws2, Assert l e1 mbe1)
    boxing env (Pass l)             = return ([], Pass l)
    boxing env (Delete l t)         = return ([], Delete l t)
    boxing env (Return l mbe)       = do (ws1,mbe1) <- boxing env mbe
                                         return (ws1,Return l mbe1)
    boxing env (Raise l e)          = do (ws1,e1) <- boxing env e
                                         return (ws1,Raise l e1)
    boxing env (Break l)            = return ([], Break l)
    boxing env (Continue l)         = return ([], Continue l)
    boxing env (If l bs ss)         = do (ws1,bs1) <- boxing env bs
                                         (ws2,ss1) <- boxing env ss
                                         return (ws1++ws2,If l bs1 ss1)
    boxing env (While l e ss els)   = do (ws1,e1) <- boxing env e
                                         (ws2,ss1) <- boxing env ss
                                         (ws3,els1) <- boxing env els
                                         return (ws1++ws2++ws3, While l e1 ss1 els1)
    boxing env (Try l ss hs els fs) = do (ws1,ss1) <- boxing env ss
                                         (ws2,hs1) <- boxing env hs
                                         (ws3,els1) <- boxing env els
                                         (ws4,fs1) <- boxing env fs
                                         return (ws1++ws2++ws3++ws4, Try l ss1 hs1 els1 fs1)
    boxing env (With l ws ss)       = do (ws1,ws') <- boxing env ws
                                         (ws2,ss1) <- boxing env ss
                                         return (ws1++ws2, With l ws' ss1)
    boxing env (VarAssign l ps e)   = do (ws1,e1) <- boxing env e
                                         return (ws1, VarAssign l ps e)
    boxing env (After l e1 e2)      = do (ws1,e1') <- boxing env e1
                                         (ws2,e2') <- boxing env e2
                                         return (ws1++ws2, After l e1' e2')
    boxing env s@(Signature l ns sc d) = return ([], s)
    boxing env g@(Decl l ds)        = do (ws1, ds1) <- boxing env1 ds
                                         return (ws1, Decl l ds1)
      where te                      = envOf g
            env1                    = define te env

instance Boxing Decl where
    boxing env (Class l n q cs ss)  = do (ws1, ss1) <- boxing env ss
                                         return (ws1, Class l n q cs ss1)
    boxing env (Def l n q p KwdNIL t ss dec fx)
                                    = do (ws1,p1) <- boxing env p
                                         (ws2,ss1) <- boxing env1 ss
                                         return (ws1++ws2,Def l n q p1 KwdNIL t ss1 dec fx)
      where te                      = envOf p
            env1                    = define te env
 
instance Boxing Branch where
    boxing env (Branch e ss)       = do (ws1,e1) <- boxing env e
                                        (ws2,ss1) <- boxing env ss
                                        return (ws1++ws2, Branch e1 ss1)

instance Boxing Handler where
    boxing env (Handler ex b)      = do (ws1, b1) <- boxing env b
                                        return (ws1, Handler ex b1)

instance Boxing PosPar where
    boxing env (PosPar n t e p)    = do (ws1, e1) <- boxing env e
                                        return (ws1, PosPar n t e1 p)
    boxing env (PosSTAR n t)       = return ([],PosSTAR n t)
    boxing env PosNIL              = return ([],PosNIL)

 
instance Boxing PosArg where
    boxing env (PosArg e p)        = do (ws1,e1) <- boxing env e
                                        (ws2,p1) <- boxing env p
                                        return (ws1++ws2, PosArg e1 p1)
    boxing env (PosStar e)         = do (ws1,e1) <- boxing env e
                                        return (ws1, PosStar e1)
    boxing env PosNil              = return ([],PosNil)
    
instance Boxing Elem where
    boxing env (Elem e)            = do (ws1,e1) <- boxing env e
                                        return (ws1, Elem e1)
    boxing env (Star e)            = do (ws1,e1) <- boxing env e
                                        return (ws1, Star e1)

instance Boxing Assoc where
    boxing env (Assoc k v)         = do (ws1,k1) <- boxing env k
                                        (ws2,v1) <- boxing env v
                                        return (ws1++ws2, Assoc k1 v1)
    boxing env (StarStar e)        = do (ws1,e1) <- boxing env e
                                        return (ws1, StarStar e1)
instance Boxing WithItem where
    boxing env (WithItem e mbp)    = do (ws1,e1) <- boxing env e
                                        return (ws1, WithItem e1 mbp)
    

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
