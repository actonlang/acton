{-# LANGUAGE FlexibleInstances #-}
module Acton.Boxing where

import Acton.Syntax
import Acton.Names
import Acton.Env
import Acton.QuickType
import Acton.Prim
import Acton.Builtin
import Acton.Subst
import Pretty
import Utils
import Debug.Trace
import Control.Monad.State.Strict
import Control.Monad.Except

doBoxing                           :: Acton.Env.Env0 -> Module -> IO Module
doBoxing env m                     = return m{mbody = ss}
   where (_,ss)                    = runBoxM (boxing (boxEnv env) (mbody m))

-- Boxing monad  ---------------------------------------------------------------------------------------------------

type BoxM a                        = State Int a

newName                            :: String -> BoxM Name
newName s                          = do n <- get
                                        put (n+1)
                                        return $ Internal BoxPass s n

newNames (n : ns)                  = do un <- newName (nstr n)
                                        ps <- newNames ns
                                        return ((n,un) : ps)
newNames []                        = return []

runBoxM ss                         = evalState ss 0

data BoxX                          = BoxX { unboxedVarsX :: [(Name,Name)], isTopLevelX :: Bool, delayedUnboxX :: Bool, inClassX :: Bool }

type BoxEnv                        = EnvF BoxX

boxEnv                             :: Env0 -> BoxEnv
boxEnv env0                        = setX env0 (BoxX [] True False False)

addUnboxedVars                     :: [(Name,Name)] -> BoxEnv -> BoxEnv
addUnboxedVars ps env               = modX env $ \x -> x{unboxedVarsX = ps ++ unboxedVarsX x}

unboxedVars                        :: BoxEnv -> [(Name,Name)]
unboxedVars env                    = unboxedVarsX $ envX env

setTopLevel b env                  = modX env $ \x -> x{isTopLevelX = b}

isTopLevel env                     = isTopLevelX $ envX env

setDelayedUnbox b env              = modX env $ \x -> x{delayedUnboxX = b}

isDelayedUnbox env                 = delayedUnboxX $ envX env


-- Auxiliaries ---------------------------------------------------------------------------------------------------

integralTypes                      = [tInt, tI64, tI32, tI16, tU64, tU32, tU16]
numericTypes                       = integralTypes ++ [tFloat]
unboxableTypes                     = tail numericTypes


isWitness (Internal Witness _ _)   = True
isWitness _                        = False

isInternal (Internal _ _ _)        = True
isInternal _                       = False

isUnboxed (Internal BoxPass _ _)    = True
isUnboxed _                         = False

isUnboxable t                      = t `elem` unboxableTypes

prims = [primISINSTANCE, primISNOTNONE, primISNONE]

unboxedPrim p
  | p == primISINSTANCE            = primISINSTANCE0
  | p == primISNOTNONE             = primISNOTNONE0
  | p == primISNONE                = primISNONE0

qMath str = QName (ModName [name "math"]) (name str)

mathfuns = map qMath ["sqrt", "exp", "log", "sin", "cos", "tan", "asin", "acos", "atan",  "sinh", "cosh", "tanh",  "asinh", "acosh", "atanh"]

-- class UnBoxClass ---------------------------------------------------------------------------------------------------

class UnboxClass a where
    unbox :: Type -> a -> a

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
    boxing env []                     = return ([],[])
    boxing env (x@(Assign _ [PVar _ n _] _) : xs)
       | isWitness n                  = do (ws1,x') <- boxing env x
                                           (ws2,xs') <- boxing env1 xs
                                           return $ if n `elem` ws2 then (ws1++ws2,x':xs') else (ws2,xs')
      where te                        = envOf x
            env1                      = define te env
    boxing env (x@(Assign _ [p@(PVar _ n (Just t))] _) : xs)
       | isUnboxable t               = do (ws1,x') <- boxing env x
                                          un <- newName (nstr n)
                                          (ws2,xs') <- boxing (addUnboxedVars [(n,un)] (define te env)) xs
                                          let s = sAssign (pVar un t) (unbox t (expr x'))
                                              ss = if isTopLevel env then [sAssign p (Box t (eVar un))] else []
                                          return (ws1++ws2, s : ss ++ xs')
      where te                       = envOf x

    boxing env (x : xs)              = do ps <- if (inClass env) then return [] else newNames [n | (n,NDef (TSchema _ [] (TFun _ _ p _ t)) _ _) <- te, isUnboxable t ||  hasUnboxableType p]
                                          (ws1,x') <- boxing (addUnboxedVars ps env) x
                                          (ws2,xs') <- boxing (addUnboxedVars ps env1) xs
                                          return (ws1++ws2, x' : xs')
      where te                       = envOf x
            env1                     = define te env
            hasUnboxableType (TRow _ _ _ t r)
                                     = isUnboxable t || hasUnboxableType r
            hasUnboxableType _       = False


instance (Boxing a) => Boxing ([a]) where
    boxing env []                   = return ([],[])
    boxing env (x : xs)             = do (ws1,x1)  <- boxing env x
                                         (ws2,xs2) <- boxing env xs
                                         return (ws1++ws2, x1:xs2)

instance Boxing a => Boxing (Maybe a) where
    boxing env (Just x)           = do (ws1, x1) <- boxing env x
                                       return (ws1, Just x1)
    boxing env Nothing            = return ([], Nothing)


instance Boxing Expr where
    boxing env e@(Var l (NoQ n))
       | isWitness n                = return ([n], e)
       | otherwise                  = case lookup n ps of
                                          Just un -> return ([], Box (typeOf env e) (eVar un))
                                          Nothing -> return ([], e)
       where ps                     = unboxedVars env
    boxing env v@Var{}              = return ([], v)
    boxing env (Call _ (Dot _ (Var _ w@(NoQ n)) attr) p KwdNil)
      | isWitness n                 = do (ws1,p1) <- boxing env p
                                         (ws2,e1) <- boxingWitness env w attr ws1 p1
                                         return (ws1++ws2,e1)
     where
      boxingWitness                 :: BoxEnv -> QName -> Name -> [Name] ->PosArg -> BoxM ([Name],Expr)
      boxingWitness env w attr ws p = case findQName w env of
                                        NVar (TCon _ (TC _ ts))
                                           | any (not . vFree) ts    -> return ([n], eCallP (eDot (eQVar w) attr) p)
                                           | attr == fromatomKW      -> boxingFromAtom w ts es
                                           | attr `elem` binopKWs    -> boxingBinop w attr es ts
                                           | attr `elem` eqordKWs    -> boxingCompop w attr es ts
                                        _                            -> return ([n], eCallP (eDot (eQVar w) attr) p)
       where es                     = posargs p
             vFree (TCon _ (TC _ _))= True
             vFree _                = False
      boxingFromAtom w ts [i@Int{}]
        | t == tInt                 = return ([], i)
        | t `elem` numericTypes     = return ([], Box (last ts) (unbox t i))
        where t = head ts
      boxingFromAtom w ts [x@Float{}]
                                    = return ([], Box (last ts) (unbox (head ts) x))
      boxingFromAtom w t es         = return ([n], Call NoLoc (eDot (eQVar w) fromatomKW) (posarg es) KwdNil)
      boxingBinop w attr es@[x1, x2] ts
        | isUnboxable t            =  return ([], Box (last ts) $ Paren NoLoc $ BinOp NoLoc (unbox t x1) op (unbox t x2))
        where t                     = head ts
              op                    = bin2Binary attr
      boxingBinop w attr es _       = return ([n], eCall(eDot (eQVar w) attr) es)

      boxingCompop w attr es@[x1, x2] ts
        | isUnboxable t             = return ([], Box tBool $ Paren NoLoc $ CompOp NoLoc (unbox t x1) [OpArg op (unbox t x2)])
        where t = head ts
              op = cmp2Comparison attr
      boxingCompop w attr es _      = return ([n], eCall (eDot (eQVar w) attr) es)
    boxing env (Call l e@(TApp _ (Var _ f) ts) p KwdNil)
      | f `elem` prims              = do (ws1,p1) <- boxing env p
                                         return (ws1,Box tBool $ eCallP e' p1)
      | otherwise                   = do (ws1,p1) <- boxing env p
                                         return (ws1, eCallP e p1)
       where e'                     = tApp (eQVar (unboxedPrim f)) ts
    boxing env c@(Call l e@(Var _ (NoQ n)) p KwdNil)
      | isUnboxable t               = do (ws1,p1) <- boxing env p
                                         case lookup n (unboxedVars env) of
                                            Just un -> return (ws1, Box t (eCallP (eVar un) (ub env p1)))
                                            Nothing -> return (ws1, eCallP e p1)
       where t                      = typeOf env c
             ub env (PosArg e p)
               | isUnboxable t      = PosArg (unbox t e) (ub env p)
               | otherwise          = PosArg e (ub env p)
              where t = typeOf env e
             ub env (PosStar e)
               | isUnboxable t      = PosStar (unbox t e)
               | otherwise          = PosStar e
              where t = typeOf env e
             ub env PosNil          = PosNil
    boxing env (Call l e@(Var _ f) p KwdNil)
      | f `elem`prims               = do (ws1,p1) <- boxing env p
                                         return (ws1,Box tBool $ eCallP e' p1)
      | f `elem` mathfuns           = do (ws1,p1)  <- boxing env p
                                         return (ws1,Box tFloat $ eCallP e (unbox tFloat p1))
      | otherwise                   = do (ws1,p1) <- boxing env p
                                         return (ws1, eCallP e p1)
       where e'                     = eQVar (unboxedPrim f)
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
    boxing env e@(BinOp l e1 op e2) = do (ws1,e1') <- boxing env e1   -- op is And or Or
                                         (ws2,e2') <- boxing env e2
                                         return (ws1++ws2, BinOp l e1' op e2')
         where t                    = typeOf env e
    boxing env (CompOp l e os)      = do (ws1,e1) <- boxing env e
                                         (ws2,e2) <- boxing env os
                                         return (ws1++ws2, CompOp l e1 e2)
    boxing env (UnOp l uop e)       = do (ws1,e') <- boxing env e      --uop is Not
                                         return (ws1, UnOp l uop e')
         where t                    = typeOf env e
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
    boxing env (Box t e)  =           do (ws1,e1) <- boxing env e
                                         case e1 of
                                            UnBox _ e' -> return (ws1,e')
                                            _ -> return (ws1,Box t e1)
                                         return (ws1, e1)
--    boxing env (Box _ (UnBox _ e))  = do (ws1,e1) <- boxing env e
--                                         return (ws1, e1)
    boxing env e                    = return ([],e)

instance Boxing OpArg where
    boxing env (OpArg op e)         = do (ws1,e1) <- boxing env e
                                         return (ws1, OpArg op e1)

instance Boxing Stmt where
    boxing env (Expr l e)           = do (ws1,e1) <- boxing env e
                                         return (ws1, Expr l e1)
    boxing env (Assign l [pt@(PVar _ x Nothing)]  e@(Call _ (Dot _ (Var _ w@(NoQ n)) attr) p KwdNil))
      | isWitness n
                                    = do (ws1,p1) <- boxing env p
                                         (ws2,pt2) <- boxing env pt
                                         (ws3,s3) <- boxingWitness env pt2 w attr p1
                                         return (ws1++ws2++ws3,s3)
      where
         boxingWitness env pt w attr p = case findQName w env of
                                            NVar (TCon _ (TC _ ts))
                                              | any (not . vFree) ts     -> return ([n], Assign l [pt] (eCallP (eDot (eQVar w) attr) p))
                                              | attr `elem` incrBinopKWs -> boxingincrBinop pt w attr es ts
                                            _                            -> do (ws,e') <- boxing env e
                                                                               return (ws, Assign l [pt] (if isUnboxed (pn pt) then unbox t e' else e'))
          where es                 = posargs p
                vFree (TCon _ (TC _ _))= True
                vFree _            = False
                t                  = typeOf env e
         boxingincrBinop pt w attr es@[x1,x2] ts
          | isUnboxable t          = return ([], AugAssign NoLoc (unbox t x1) op (unbox t x2))
             where t               = head ts
                   op              = bin2Aug attr
         boxingincrBinop pt w attr es _
                                   = return ([n],  Assign l [pt] (eCall (eDot (eQVar w) attr) es))
    boxing env (Assign l [pt@PVar{}] e)
                                   = do (ws1,pt1) <- boxing env pt
                                        (ws2,e2) <- boxing env e
                                        return (ws1++ws2, Assign l [pt1] (if isUnboxed (pn pt1) then unbox t e2 else e2))
             where t               = typeOf env e
    boxing env (Assign l ps e)     = do (ws1,ps1) <- boxing env ps
                                        (ws2,e2) <- boxing env e
                                        return (ws1++ws2, Assign l ps1 e2)
    boxing env (MutAssign l tg@Dot{}  e@(Call _ (Dot _ (Var _ w@(NoQ n)) attr) p KwdNil))
      | isWitness n                 = do (ws1,p1) <- boxing env p
                                         (ws2,s2) <- boxingWitness env w attr p1
                                         return (ws1++ws2,s2)
      where
         t                          = typeOf env tg
         boxingWitness env w attr p = case findQName w env of
                                            NVar (TCon _ (TC _ ts))
                                              | any (not . vFree) ts     -> return ([n], MutAssign l tg (eCallP (eDot (eQVar w) attr) p))
                                              | attr `elem` incrBinopKWs -> boxingincrBinop w attr es ts
                                            _                            -> do (ws,e') <- boxing env e
                                                                               return (ws, if isUnboxable t
                                                                                           then MutAssign l tg e'
                                                                                           else MutAssign l tg e')
          where es                 = posargs p
                vFree (TCon _ (TC _ _))= True
                vFree _            = False
         boxingincrBinop w attr es@[x1,x2] ts
          | isUnboxable t           = return ([], AugAssign NoLoc (unbox t x1) op (unbox t x2))
             where t                = head ts
                   op               = bin2Aug attr
         boxingincrBinop w attr es _ = return ([n],  MutAssign l tg (eCall (eDot (eQVar w) attr) es))
    boxing env (MutAssign l t e)    = do (ws1,e1) <- boxing env e
                                         return (ws1, MutAssign l t e1)
    boxing env (AugAssign l t aop e)= do (ws1,e1) <- boxing env e
                                         return (ws1, AugAssign l t aop e1)
    boxing env (Assert l e mbe)     = do (ws1,e1) <- boxing env e
                                         (ws2,mbe1) <- boxing env mbe
                                         return (ws1++ws2, Assert l e1 mbe1)
    boxing env (Pass l)             = return ([], Pass l)
    boxing env (Delete l t)         = return ([], Delete l t)
    boxing env (Return l (Just e))  = do (ws1,e1) <- boxing env e
                                         if (isUnboxable t && isDelayedUnbox env)
                                          then return (ws1, Return l (Just (unbox t e1)))
                                          else return (ws1, Return l (Just e1))
        where t                     = typeOf env e
    boxing env (Return l Nothing)   = return ([], Return l Nothing)
    boxing env (Raise l e)          = do (ws1,e1) <- boxing env e
                                         return (ws1, Raise l e1)
    boxing env (Break l)            = return ([], Break l)
    boxing env (Continue l)         = return ([], Continue l)
    boxing env (If l bs ss)         = do (ws1,bs1) <- boxing env1 bs
                                         (ws2,ss1) <- boxing env1 ss
                                         return (ws1++ws2,If l bs1 ss1)
       where env1 = setTopLevel False env
    boxing env (While l e ss els)   = do (ws1,e1) <- boxing env e
                                         (ws2,ss1) <- boxing env1 ss
                                         (ws3,els1) <- boxing env1 els
                                         return (ws1++ws2++ws3, While l e1 ss1 els1)
       where env1 = setTopLevel False env
    boxing env (Try l ss hs els fs) = do (ws1,ss1) <- boxing env1 ss
                                         (ws2,hs1) <- boxing env1 hs
                                         (ws3,els1) <- boxing env1 els
                                         (ws4,fs1) <- boxing env1 fs
                                         return (ws1++ws2++ws3++ws4, Try l ss1 hs1 els1 fs1)
       where env1 = setTopLevel False env
    boxing env (With l ws ss)       = do (ws1,ws') <- boxing env ws
                                         (ws2,ss1) <- boxing (setTopLevel False env) ss
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
            env1                    = setTopLevel False $ define te env

instance {-# OVERLAPS #-} Boxing [Decl] where
    boxing env (c@Class{} : ds)     = do (ws1,c1) <- boxing (setInClass env) c
                                         (ws2,ds2) <- boxing env ds
                                         return (ws1++ws2,c1:ds2)
    boxing env (d@Def{} : ds)
      | hasNotImpl (dbody d)        = do (ws,ds1) <- boxing env ds
                                         return (ws, d : ds1)
      | otherwise                   = case lookup (dname d) (unboxedVars env) of
                                        Just un -> do
                                           (ws1,d1) <- boxing (setDelayedUnbox True env) d{dname = un}
                                           let ds1 =  [mkWrapper d un]
                                           (ws2,ds2) <- boxing env ds
                                           return (ws1++ws2,d1 : ds1 ++ ds2)
                                        _ -> do
                                           (ws1,d1) <- boxing env d
                                           (ws2,ds2) <- boxing env ds
                                           return (ws1++ws2,d1:ds2)
       where mkWrapper (Def l n q p _ (Just t) ss dec fx ddoc) un
                | isUnboxable t     = Def l n q p KwdNIL (Just t) [Return NoLoc (Just (Box t (eCallP (eVar un) (ub p))))] dec fx ddoc
                | otherwise         = Def l n q p KwdNIL (Just t) [Return NoLoc (Just (eCallP (eVar un) (ub p)))] dec fx ddoc
             ub (PosPar n (Just t) _ p)
               | isUnboxable t      = PosArg (unbox t (eVar n)) (ub p)
               | otherwise          = PosArg (eVar n) (ub p)
             ub (PosSTAR n _)       = PosStar (eVar n)
             ub PosNIL              = PosNil
    boxing env []                   = return ([],[])

instance Boxing Decl where
    boxing env (Class l n q cs ss ddoc)
                                    = do (ws1, ss1) <- boxing env1 ss'
                                         return (ws1, Class l n q cs ss1 ddoc)
        where ss'                   = vsubst [(tvSelf, tCon c)] ss
              c                     = TC (NoQ n) (map tVar $ qbound q)
              env1                  = defineTVars q env
    boxing env (Def l n q p KwdNIL t ss dec fx ddoc)
                                    = do ps <- if (inClass env || not (isUnboxed n)) then return [] else newNames [n | (n,NVar t) <- te, isUnboxable t]
                                         let env2 = addUnboxedVars ps $ env1
                                         (ws1,p1) <- boxing env2 p
                                         (ws2,ss1) <- boxing env2 ss
                                         return (ws1++ws2,Def l n q p1 KwdNIL t ss1 dec fx ddoc)
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
                                        (ws2, p2) <- boxing env p
                                        case lookup n (unboxedVars env) of
                                           Just un -> return (ws1++ws2, PosPar un t e1 p2)
                                           _ -> return (ws1++ws2, PosPar n t e1 p2)
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

instance Boxing Pattern where
    boxing env v@(PVar l n mbt)    = case lookup n ps of
                                            Just un -> return ([], PVar l un mbt)
                                            Nothing -> return ([], v)
        where ps                   = unboxedVars env
    boxing env (PParen l p)        = do (ws,p') <- boxing env p
                                        return (ws, PParen l p')
    boxing env (PList l ps mbp)    = do (ws1, ps1) <- boxing env ps
                                        (ws2, mbp2) <- boxing env mbp
                                        return (ws1++ws2, PList l ps1 mbp2)



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


bin2Aug kw
   | kw == iaddKW                  = PlusA
   | kw == isubKW                  = MinusA
   | kw == imulKW                  = MultA
   | kw == ipowKW                  = PowA
   | kw == itruedivKW              = DivA
   | kw == imodKW                  = ModA
   | kw == ifloordivKW             = EuDivA
   | kw == ilshiftKW               = ShiftLA
   | kw == irshiftKW               = ShiftRA
   | kw == iorKW                   = BOrA
   | kw == ixorKW                  = BXorA
   | kw == iandKW                  = BAndA

