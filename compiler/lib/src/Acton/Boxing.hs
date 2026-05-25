{-# LANGUAGE FlexibleInstances #-}
module Acton.Boxing where

import Acton.Syntax
import Acton.Names
import Acton.NameInfo
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
import qualified Data.HashMap.Strict as M

doBoxing                           :: Acton.Env.Env0 -> Module -> IO Module
doBoxing env m                     = return m{mbody = ss}   
   where (_,ss)                    = runBoxM (boxing (boxEnv env) (mbody m))

-- Boxing monad  ---------------------------------------------------------------------------------------------------

type BoxM a                        = State Int a

--newName                            :: String -> BoxM Name
--newName s                          = do n <- get
--                                        put (n+1)
--                                        return $ Internal BoxPass s n
--
--newNames (n : ns)                  = do un <- newName (nstr n)
--                                        ps <- newNames ns
--                                        return ((n,un) : ps)
--newNames []                        = return []

runBoxM ss                         = evalState ss 0

type BoxEnv                        = EnvF ()


-- data BoxX                          = BoxX { unboxedVarsX :: [(Name,Name)], isTopLevelX :: Bool, delayedUnboxX :: Bool }


boxEnv                             :: Env0 -> BoxEnv
boxEnv env0                        = setX env0 ()

{-
setInClass mtc env                  = modX env $ \x -> x{ inClassX = mtc }

getInClass env                      = inClassX $ envX env

addUnboxedVars                     :: [(Name,Name)] -> BoxEnv -> BoxEnv
addUnboxedVars ps env               = modX env $ \x -> x{unboxedVarsX = foldr (uncurry M.insert) (unboxedVarsX x) ps}

unboxedVars                        :: BoxEnv -> M.HashMap Name Name
unboxedVars env                    = unboxedVarsX $ envX env

setTopLevel b env                  = modX env $ \x -> x{isTopLevelX = b}

isTopLevel env                     = isTopLevelX $ envX env

setDelayedUnbox b env              = modX env $ \x -> x{delayedUnboxX = b}

isDelayedUnbox env                 = delayedUnboxX $ envX env
-}

-- Unboxing helpers -------------------------------------

-- returns the uninstantiated type of method n in class c, i.e. the type of the corresponding method in oldest superclass.
generalTypeC                       :: EnvF x -> QName -> Name -> Type
generalTypeC env c n                =  case fromJust (lookup n (findAttrSchemas env c)) of
                                           NDef sc _ _ -> sctype sc
                                           NSig sc _ _ -> sctype sc
                                           ni -> error ("Internal error in CodeGen.generalType: found NameInfo "++show ni++ " for "++show n)

-- generalType env n returns the same info as generalType if n is a method of a class, but expects the TEnv of the class to be accessed via the env;
-- for use while traversing the class.
-- If n is a toplevel function it returns just the type in the NameInfo from the env.
-- n may be a method for which findAttrSchemas has no info, e.g. __init__, __str__, etc, in which case Nothing is returned.
generalType                        :: EnvF x -> Name -> Maybe Type
generalType env n                  = case getNI n of
                                           Just (NDef sc _ _) -> Just $ sctype sc
                                           Just (NSig sc _ _) -> Just $ sctype sc
                                           ni -> Nothing 
    where getNI n                  = case contextIs env CtxClass of
                                           True -> lookup n (gtypes env)
                                           False -> Just (findQName (NoQ n) env)
 
-- matchTypes expects to be called with both arguments a function type; the specific one returned by typeOf for a name f and
-- the general one returned by findAttrSchemas (if applicable) for the same name.
-- matchTypes t t' tags as TUnboxed those type components in t which can be unboxed taking the uninstantiated type t'
-- (type of the oldest corresponding method in a superclass) into account. 
matchTypes t@TCon{} t'@TCon{}
   | t == t'                        = if isUnboxable t then TUnboxed NoLoc t else t
matchTypes (TFun _ fx p _ r) (TFun _ fx' p' _ r')
                                    = tFun fx (matchTypes p p') kwdNil (matchTypes r r')
matchTypes (TRow _ _ _ t r) (TRow _ _ _ t' r')
                                    = posRow (matchTypes t t') (matchTypes r r')
matchTypes TNil{} TNil{}            = posNil                                    
matchTypes t _                      = t    -- ignore possibilities for unboxing 


-- returns the representation type of method f selected from c with TUnbox annotations where appropriate
rtypeOf env c f                     =  matchTypes (typeOf env (eVar f)) (generalTypeC env c f)
 
-- Auxiliaries ---------------------------------------------------------------------------------------------------

-- unboxing

integralTypes                      = [tBigint, tInt, tI32, tI16, tI8, tU64, tU32, tU16, tU8, tU1]
numericTypes                       = integralTypes ++ [tFloat]
unboxableTypes                     = tail numericTypes

isUnboxable t                      = t `elem` unboxableTypes


prims = [primISINSTANCE, primISNOTNONE, primISNONE]

unboxedPrim p
  | p == primISINSTANCE            = primISINSTANCE0
  | p == primISNOTNONE             = primISNOTNONE0
  | p == primISNONE                = primISNONE0

{-
qMath str = QName (ModName [name "math"]) (name str)

mathfuns = map qMath ["sqrt", "exp", "log", "sin", "cos", "tan", "asin", "acos", "atan",  "sinh", "cosh", "tanh",  "asinh", "acosh", "atanh"]

-- class UnBoxClass ---------------------------------------------------------------------------------------------------

class UnboxClass a where
    unbox :: Type -> a -> a

instance UnboxClass Expr where
-}
unbox _ (Box _ e)               = e
unbox t e                       = UnBox t e

tryUnbox, tryBox :: Type -> Expr -> Expr
tryUnbox (TUnboxed _ t) e       = unbox t e
tryUnbox _ e                    = e
tryBox t e                      = if isUnboxable t then Box t e else e

fixargs                         :: BoxEnv -> PosArg -> Type -> PosArg
fixargs env (PosArg e p) r      = PosArg (tryUnbox t e) (fixargs env p (rtail r))
   where t                      = rtype r
-- fixargs env (PosStar e) r       = PosStar (tryUnbox t e) 
--    where t                      = rtype r
fixargs env PosNil _            = PosNil

uType env w n                       = sctype sc
   where NVar (TCon _ tc)           = findQName w env
         (sc,_)                     = findAttr' env tc n

{-
instance UnboxClass PosArg where
   unbox t (PosArg e p)            = PosArg (unbox t e) (unbox t p)
   unbox t (PosStar e)             = PosStar (unbox t e)
   unbox t PosNil                  = PosNil
-}
-- Walking the syntax tree to place Box/UnBox annotations and sometimes restructure code to mimic C code on unboxed values ---------------
-- Return list of used witnesses and restructured code

class Boxing a where
    boxing :: BoxEnv -> a -> BoxM ([Name],a)

instance Boxing a => Boxing (Maybe a) where
    boxing env (Just x)           = do (ws1, x1) <- boxing env x
                                       return (ws1, Just x1)
    boxing env Nothing            = return ([], Nothing)

instance (Boxing a) => Boxing ([a]) where
    boxing env []                   = return ([],[])
    boxing env (x : xs)             = do (ws1,x1)  <- boxing env x
                                         (ws2,xs2) <- boxing env xs
                                         return (ws1++ws2, x1:xs2)

instance {-# OVERLAPS #-} Boxing ([Stmt]) where
    boxing env []                     = return ([],[])
    boxing env (x@(Assign _ [PVar _ w _] _) : xs)
    --  if witness n is not used in xs, delete statement x defining n
       | isWitness w                  = do (ws1,x') <- boxing env x      
                                           (ws2,xs') <- boxing env1 xs
                                           return $ if w `elem` ws2 then (ws1++ws2,x':xs') else (ws2,xs')
      where te                        = envOf x
            env1                      = define te env

    boxing env (x : xs)              = do (ws1,x') <- boxing env x
                                          
                                          (ws2,xs') <- (if iterKW `elem` dom te then trace ("Added __iter__") else id) $ boxing (define te env) xs
                                          return (ws1++ws2, x' : xs')
      where te                       = envOf x

 
-- After boxing, each Expr of unboxable type is boxed. Operands in BinOp/CompOp/UnOp expressions
-- are first boxed as part of their own boxing, then unboxed when used to form the BinOp/CompOp/UnOp term
instance Boxing Expr where
    boxing env e@(Var _ (NoQ n))
       | isWitness n                = return ([n], e)
       | isUnboxable t              = return ([], Box t e)
       where t                      = typeOf env e
--    boxing env v@Var{}
--       | isUnboxable t              = return ([], Box t v)               Needed for math.pi
--       | otherwise                  = return ([], v)
--       where t                      = typeOf env v                       ###### qInst [] __builtin__.Eq is [G_self] => () -> __builtin__.Eq[G_self]
    boxing env (Call _ (Dot _ e@(Var _ w@(NoQ n)) attr) p KwdNil)
      | isWitness n                 = do (ws1,p1) <- boxing env p
                                         (ws2,e1) <- boxingWitness env w attr ws1 p1 pr rest
                                         return (ws1++ws2,e1)
     where
      TFun _ _ pr _ rest            = uType env w attr
      boxingWitness                 :: BoxEnv -> QName -> Name -> [Name] -> PosArg -> Type -> Type ->BoxM ([Name],Expr)
      boxingWitness env w attr ws p pr rest = case findQName w env of
                                        NVar (TCon _ (TC _ ts))
                                  --         | any (not . vFree) ts    -> return ([n], eCallP (eDot (eQVar w) attr) p)
                                           | attr == fromatomKW      -> boxingFromAtom w ts es
                                           | attr `elem` binopKWs    -> boxingBinop w attr es ts pr rest  -- rest = "result type", not any form of reminder
                                           | attr `elem` unopKWs     -> boxingUnop w attr es ts pr rest
                                           | attr `elem` eqordKWs    -> boxingCompop w attr es ts pr rest
                                        _                            -> do let c = eCallP (eDot (eQVar w) attr) (fixargs env p pr)
                                                                           return ([n], tryBox rest c)
       where es                     = posargs p
--             vFree (TCon _ (TC _ _))= True
--             vFree _                = False
      boxingFromAtom w ts [i@Int{}]
        | t == tBigint               = return ([], i)
        | t `elem` numericTypes     = return ([], Box (last ts) (unbox t i))
        where t = head ts
      boxingFromAtom w ts [x@Float{}]
                                    = return ([], Box (last ts) (unbox (head ts) x))
      boxingFromAtom w ts es        = return ([n], eCall (eDot (eQVar w) fromatomKW) es)
      boxingBinop w attr es@[x1, x2] ts _ _
        | isUnboxable t            =  return ([], Box (last ts) $ Paren NoLoc $ BinOp NoLoc (unbox t x1) op (unbox t x2))
        where t                     = head ts
              op                    = bin2Binary attr
      boxingBinop w attr es _ pr rest= return ([n], tryBox rest $ eCallP (eDot (eQVar w) attr) (fixargs env (posarg es) pr))
      boxingUnop w attr es@[x1] ts _ _
        | isUnboxable t             =  return ([], Box (last ts) $ Paren NoLoc $ UnOp NoLoc op (unbox t x1))
        where t                     = head ts
              op                    = un2Unary attr
      boxingUnop w attr es _ pr rest= return ([n], tryBox rest $ eCallP (eDot (eQVar w) attr) (fixargs env (posarg es) pr))
      boxingCompop w attr es@[x1, x2] ts _ _
        | isUnboxable t             = return ([], Box tBool $ Paren NoLoc $ CompOp NoLoc (unbox t x1) [OpArg op (unbox t x2)])
        where t = head ts
              op = cmp2Comparison attr
      boxingCompop w attr es _ pr rest= return ([n], tryBox rest $ eCallP (eDot (eQVar w) attr) (fixargs env (posarg es) pr))
    boxing env (Call l e@(TApp _ (Var _ f) ts) p KwdNil)
      | f `elem` prims              = do (ws1,p1) <- boxing env p
                                         return (ws1, Box tBool $ eCallP e' (fixargs env p1 r))
      | otherwise                   = do (ws1,p1) <- boxing env p
                                         return (ws1, tryBox t $ eCallP e (fixargs env p1 r))
       where e'                     = tApp (eQVar (unboxedPrim f)) ts
             TFun _ _ r _ t         = typeOf env e
    boxing env (Call l f p KwdNil)  = do (ws1,f1) <- boxing env f
                                         (ws2,p1) <- boxing env p
                                         let c = eCallP f1 (fixargs env p1 r)
                                         return (ws1++ws2, tryBox t c)
        where  TFun _ _ r _ t       = typeOf env f
    boxing env (TApp l f ts)        = do (ws1,f1) <- boxing env f
                                         return (ws1, TApp l f1 ts)
    boxing env (Let l ss e)         = do (ws1, ss') <- boxing env ss
                                         (ws2, e') <- boxing env e
                                         return (ws1++ws2, Let l ss' e')
    boxing env (Async l e)          = do (ws1,e1) <- boxing env e
                                         return (ws1, Async l e1)
    boxing env (Await l e)          = do (ws1,e1) <- boxing env e
                                         return (ws1, Await l e1)
    boxing env (Cond l e1 e2 e3)    = do (ws1,e1') <- boxing env e1
                                         (ws2,e2') <- boxing env e2
                                         (ws3,e3') <- boxing env e3
                                         case isUnboxable t of
                                             True -> return (ws1++ws2++ws3, Box t $ Cond l (unbox t e1') e2' (unbox t e3'))
                                             False -> return (ws1++ws2++ws3, Cond l e1' e2' e3')
        where t = typeOf env e1
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
    boxing env d@(Dot l e n)        = do (ws1,e1) <- boxing env e
                                         return (ws1, Dot l e1 n)
    boxing env (DotI l e i)         = do (ws1,e1) <- boxing env e
                                         return (ws1, DotI l e1 i)
    boxing env (Rest l e n)         = do (ws1,e1) <- boxing env e
                                         return (ws1, Rest l e1 n)
    boxing env (RestI l e i)        = do (ws1,e1) <- boxing env e
                                         return (ws1, RestI l e1 i)
    boxing env (Tuple l p k)        = do (ws1,p1) <- boxing env p
                                         return (ws1,Tuple l p1 k)
    boxing env (List l es)          = do (ws1,es1) <- boxing env es
                                         return (ws1, List l es1)
    boxing env (Set l es)           = do (ws1,es1) <- boxing env es
                                         return (ws1, Set l es1)
    boxing env (Dict l es)          = do (ws1,es1) <- boxing env es
                                         return (ws1, Dict l es1)
    boxing env (Box t e)  =           do (ws1,e1) <- boxing env e
                                         case e1 of
                                            UnBox _ e' -> return (ws1,e')
                                            _ -> return (ws1,Box t e1)
                                         return (ws1, e1)
    boxing env e                    = return ([],e)

instance Boxing OpArg where
    boxing env (OpArg op e)         = do (ws1,e1) <- boxing env e
                                         return (ws1, OpArg op e1)

instance Boxing Stmt where
    boxing env (Expr l e)           = do (ws1,e1) <- boxing env e
                                         return (ws1, Expr l e1)
{-
   boxing env (Assign l [(PVar _ x Nothing)]  e@(Call _ (Dot _ (Var _ w@(NoQ n)) attr) p KwdNil))
      | isWitness n
                                    = do (ws1,p1) <- boxing env p
                                         (ws2,pt2) <- boxing env pt
                                         (ws3,s3) <- boxingWitness env pt2 w attr p1
                                         return (ws1++ws2++ws3,s3)
      where
         boxingWitness env pt w attr p = case findQName w env of
                                            NVar (TCon _ (TC _ ts))
                                         --     | any (not . vFree) ts     -> return ([n], Assign l [pt] (eCallP (eDot (eQVar w) attr) p))
                                              | attr `elem` augopKWs -> boxingincrBinop pt w attr es ts
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
-}
    boxing env (Assign l [pt@(PVar _ _ Nothing)] e@(Call _ (Dot _ (Var _ (NoQ w)) attr) p KwdNil))  -- (re)introduce augmented arithmetic operator for unboxable types; Assign case
      | isWitness w && attr `elem` augopKWs && isUnboxable t
                                    = do (ws, p') <- boxing env p
                                         let [x1,x2] = posargs p'
                                         return (ws, AugAssign l (unbox t x1) op (unbox t x2))
          where t                      = typeOf env e
                op                     = bin2Aug attr

    boxing env (x@(Assign l [p@(PVar _ n (Just t))] e))
       | isUnboxable t               = do (ws1,p') <- boxing env p
                                          (ws2, e') <- boxing env e
                                          return (ws1++ws2, Assign l [p'] (unbox t e'))
    boxing env (Assign l [pt@PVar{}] e)
                                   = do (ws1,pt1) <- boxing env pt
                                        (ws2,e2) <- boxing env e
                                        return (ws1++ws2,Assign l [pt1] (if isUnboxable t then unbox t e2 else e2))
             where t               = typeOf env e


{-  This does not work. It may cause mutable updates to integer-typed varibles.                                   

    boxing env (MutAssign l tg@Dot{}  e@(Call _ (Dot _ (Var _ w@(NoQ n)) attr) p KwdNil))
      | isWitness n                 = do (ws0,tg1) <- boxing env tg
                                         (ws1,p1) <- boxing env p
                                         (ws2,s2) <- boxingWitness env tg1 w attr p1
                                         return (ws1++ws2,s2)
      where
         t                          = typeOf env tg
         boxingWitness env tg w attr p = case findQName w env of
                                            NVar (TCon _ (TC _ ts))
                                              | any (not . vFree) ts     -> return ([n], MutAssign l tg (eCallP (eDot (eQVar w) attr) p))
                                              | attr `elem` incrBinopKWs -> boxingincrBinop tg w attr es ts
                                            _                            -> do (ws,e') <- boxing env e
                                                                               return (ws, if isUnboxable t
                                                                                           then MutAssign l tg e'
                                                                                           else MutAssign l tg e')
          where es                 = posargs p
                vFree (TCon _ (TC _ _))= True
                vFree _            = False
         boxingincrBinop tg w attr es@[x1,x2] ts
          | isUnboxable t           = return ([], AugAssign NoLoc (unbox t x1) op (unbox t x2))
             where t                = head ts
                   op               = bin2Aug attr
         boxingincrBinop w attr es _ = return ([n],  MutAssign l tg (eCall (eDot (eQVar w) attr) es))
-}
    boxing env (MutAssign l _  e@(Call _ (Dot _ (Var _ (NoQ w)) attr) p KwdNil))  -- (re)introduce augmented arithmetic operator for unboxable types; MutAssign case
      | isWitness w && attr `elem` augopKWs  && isUnboxable t
                                    = do let [x1,x2] = posargs p
                                         (ws, x2') <- boxing env x2
                                         return (ws, AugAssign l x1 op (unbox t x2'))
          where t                   = typeOf env e
                op                  = bin2Aug attr
    boxing env (MutAssign l tg e)   = do (ws0,tg1) <- boxing env tg
                                         (ws1,e1) <- boxing env e
                                         return (ws0++ws1, MutAssign l tg1 (if isUnboxable t then unbox t e1 else e1))
          where t                   = typeOf env e
    boxing env (Pass l)             = return ([], Pass l)
    boxing env (Delete l t)         = return ([], Delete l t)
    boxing env (Return l (Just e))  = do (ws1,e1) <- boxing env e
                                         if isUnboxable t
                                          then return (ws1, Return l (Just (unbox t e1)))
                                          else return (ws1, Return l (Just e1))
        where t                     = typeOf env e
    boxing env (Return l Nothing)   = return ([], Return l Nothing)
    boxing env (Break l)            = return ([], Break l)
    boxing env (Continue l)         = return ([], Continue l)
    boxing env (If l bs ss)         = do (ws1,bs1) <- boxing env bs
                                         (ws2,ss1) <- boxing env ss
                                         return (ws1++ws2,If l bs1 ss1)
    boxing env (While l e ss els)   = do (ws1,e1) <- boxing env e
                                         (ws2,ss1) <- boxing env ss
                                         (ws3,els1) <- boxing env els
                                         return (ws1++ws2++ws3, While l e1 ss1 els1)
    boxing env s@(Signature l ns sc d) = return ([], s)
    boxing env g@(Decl l ds)        = do (ws1, ds1) <- boxing env1 ds
                                         return (ws1, Decl l ds1)
      where te                      = envOf g
            env1                    = define te env
    boxing env s                    = error ("Unhandled boxing " ++show s)

--instance {-# OVERLAPS #-} Boxing [Decl] where
--    boxing env (c@Class{} : ds)     = do (ws1,c1) <- boxing (setInClass env) c
--                                         (ws2,ds2) <- boxing env ds
--                                         return (ws1++ws2,c1:ds2)
--    boxing env (d@Def{} : ds)
   --   | hasNotImpl (dbody d)        = do (ws,ds1) <- boxing env ds
   --                                      return (ws, d : ds1)
   --   | otherwise                   = case lookup (dname d) (unboxedVars env) of
--                                      = do (ws1,d') <- boxing env d
--                                           (ws2,ds') <- boxing env ds
--                                           return (ws1++ws2,d':ds')
--    boxing env []                   = return ([],[])

instance Boxing Decl where
    boxing env (Class l n q cs ss ddoc)
                                    = do (ws1, ss1) <- boxing env2 ss'
                                         return (ws1, Class l n q cs ss1 ddoc)
        where ss'                   = vsubst [(tvSelf, tCon c)] ss
              c                     = TC (NoQ n) (map tVar $ qbound q)
              env1                  = defineTVars q env
              env2                  = setGtypes env1 (findAttrSchemas env1 (NoQ n))
    boxing env (Def l n q p KwdNIL t ss dec fx ddoc)
                                    = do (ws1,p') <- boxing env p
                                         (ws2,ss') <- boxing env1 ss
                                         return (ws1++ws2, Def l n q p' KwdNIL t ss' dec fx ddoc)
      where te                      = envOf p
            env1                    = define te env
  
instance Boxing Branch where
    boxing env (Branch e ss)       = do (ws1,e') <- boxing env e
                                        (ws2,ss') <- boxing env ss
                                        return (ws1++ws2, Branch e' ss')

instance Boxing PosPar where
    boxing env (PosPar n (Just t) e p)
                                   = do (ws1, e1) <- boxing env e
                                        (ws2, p2) <- boxing env p
                                        return (ws1++ws2, PosPar n (Just t) (if isUnboxable t then maybe Nothing (Just . unbox t) e1 else e1)  p2)
    boxing env (PosSTAR n t)       = return ([],PosSTAR n t)
    boxing env PosNIL              = return ([],PosNIL)


instance Boxing PosArg where
    boxing env (PosArg e p)        = do (ws1,e1) <- boxing env e
                                        (ws2,p1) <- boxing env p
                                        return (ws1++ws2, PosArg e1 p1)
      where t                      = typeOf env e
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

instance Boxing Pattern where
    boxing env v@(PVar l n mbt)    = return ([],v)
    
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

un2Unary kw
   | kw == posKW                   = UPlus
   | kw == negKW                   = UMinus
   | kw == invertKW                = BNot

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
