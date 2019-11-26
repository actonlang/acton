{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Acton.Kinds(check) where

import qualified Control.Exception
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Control.Monad.State.Strict
import Control.Monad
import Pretty
import Utils
import Acton.Syntax
import Acton.Names
import Acton.Builtin
import Acton.Env (Env, TEnv, define, nEmpty, nCombine, nProto, nClass, nTVars, definedName, definedQName, tyfree)
import Acton.Solver


check                               :: Acton.Env.Env -> Module -> IO Module
check env (Module l imps ss)        = return (Module l imps ss1)
  where ss1                         = runKindM (kchkTop env ss)


type KVar                           = Name

data KindState                      = KindState {
                                        nextint     :: Int,
                                        currsubst   :: Map KVar Kind
                                      }

type KindM a                        = State KindState a

runKindM                            :: KindM a -> a
runKindM m                          = evalState m $ KindState { nextint = 1, currsubst = Map.empty }

newUnique                           :: KindM Int
newUnique                           = state $ \st -> (nextint st, st{ nextint = nextint st + 1 })

substitute                          :: KVar -> Kind -> KindM ()
substitute kv k                     = state $ \st -> ((), st{ currsubst = Map.insert kv k (currsubst st)})

getSubstitution                     :: KindM (Map KVar Kind)
getSubstitution                     = state $ \st -> (currsubst st, st)


newTVar                             = do k <- newKVar
                                         TVar NoLoc <$> TV k <$> (Internal "W" <$> newUnique <*> return KindPass)

newKVar                             = KVar <$> (Internal "K" <$> newUnique <*> return KindPass)


---------------------------------------------------------------------------------------------------------------------

kchkTop env ss                      = do ss1 <- kchk env ss
                                         ksubst ss1

class KCheck a where
    kchk                            :: Env -> a -> KindM a
    tenv                            :: a -> TEnv
    tenv _                          = nEmpty

instance KCheck a => KCheck [a] where
    kchk env []                     = return []
    kchk env (x:xs)                 = do x' <- kchk env x
                                         xs' <- kchk (define (tenv x') env) xs
                                         return (x':xs')

instance KCheck a => KCheck (Maybe a) where
    kchk env Nothing                = return Nothing
    kchk env (Just a)               = Just <$> kchk env a

instance KCheck Stmt where
    kchk env (Expr l e)             = Expr l <$> kchk env e
    kchk env (Assign l ts e)        = Assign l <$> kchk env ts <*> kchk env e
    kchk env (AugAssign l p op e)   = AugAssign l <$> kchk env p <*> return op <*> kchk env e
    kchk env (Assert l e mbe)       = Assert l <$> kchk env e <*> kchk env mbe
    kchk env (Pass l)               = return $ Pass l
    kchk env (Delete l p)           = Delete l <$> kchk env p
    kchk env (Return l mbe)         = Return l <$> kchk env mbe
    kchk env (Raise l mbex)         = Raise l <$> kchk env mbex
    kchk env (Break l)              = return $ Break l
    kchk env (Continue l)           = return $ Continue l
    kchk env (If l bs els)          = If l <$> kchk env bs <*> kchk env els
    kchk env (While l e b els)      = While l <$> kchk env e <*> kchk env b <*> kchk env els
    kchk env (For l p e b els)      = For l <$> kchk env p <*> kchk env e <*> kchk env b <*> kchk env els
    kchk env (Try l b hs els fin)   = Try l <$> kchk env b <*> kchk env hs <*> kchk env els <*> kchk env fin
    kchk env (With l is b)          = With l <$> kchk env is <*> kchk env b
    kchk env (Data l mbt ss)        = Data l <$> kchk env mbt <*> kchk env ss
    kchk env (VarAssign l ps e)     = VarAssign l <$> kchk env ps <*> kchk env e
    kchk env (Decl l ds)            = Decl l <$> kchk env ds
    
    tenv (Decl _ ds)                = foldl nCombine nEmpty (map tenv ds)
    tenv _                          = nEmpty

instance KCheck Decl where
    kchk env (Def l n [] p k t b m) = Def l n [] <$> kchk env1 p <*> kchk env1 k <*> kchk env1 t <*> kchk env1 b <*> return m
      where env1                    = define (nTVars [ TBind v [] | v <- nub $ tyfree p ++ tyfree k ++ tyfree t ]) env
    kchk env (Def l n q p k t b m)  = do q' <- kchk env q
                                         let env1 = define (tenv q') env
                                         Def l n q' <$> kchk env1 p <*> kchk env1 k <*> kchk env1 t <*> kchk env1 b <*> return m
    kchk env (Actor l n [] p k t b) = Actor l n [] <$> kchk env p <*> kchk env k <*> kchk env t <*> kchk env b
      where env1                    = define (nTVars [ TBind v [] | v <- nub $ tyfree p ++ tyfree k ++ tyfree t ]) env
    kchk env (Actor l n q p k t b)  = do q' <- kchk env q
                                         let env1 = define (tenv q') env
                                         Actor l n q' <$> kchk env p <*> kchk env k <*> kchk env t <*> kchk env b
    kchk env (Class l n q us b)     = Class l n <$> kchk env q <*> kchk env us <*> kchk env b                                               -- TODO
    kchk env (Protocol l n q us b)  = Protocol l n <$> kchk env q <*> kchk env us <*> kchk env b                                            -- TODO
    kchk env (Extension l n q us b) = Extension l n <$> kchk env q <*> kchk env us <*> kchk env b                                           -- TODO
    kchk env (Signature l ns t)     = Signature l ns <$> kchk env t                                                                         -- TODO
    
    tenv (Class _ n q _ _)          = nClass n q [] []
    tenv (Protocol _ n q _ _)       = nProto n q [] []
    tenv _                          = nEmpty

instance KCheck Expr where
    kchk env (Var l n)              = return $ Var l n
    kchk env (Int l i s)            = return $ Int l i s
    kchk env (Float l f s)          = return $ Float l f s
    kchk env (Imaginary l i s)      = return $ Imaginary l i s
    kchk env (Bool l b)             = return $ Bool l b
    kchk env (None l)               = return $ None l
    kchk env (NotImplemented l)     = return $ NotImplemented l
    kchk env (Ellipsis l)           = return $ Ellipsis l
    kchk env (Strings l ss)         = return $ Strings l ss
    kchk env (BStrings l ss)        = return $ BStrings l ss
    kchk env (Call l e ps ks)       = Call l <$> kchk env e <*> kchk env ps <*> kchk env ks
    kchk env (Index l e is)         = Index l <$> kchk env e <*> kchk env is
    kchk env (Slice l e sl)         = Slice l <$> kchk env e <*> kchk env sl
    kchk env (Cond l e1 e2 e3)      = Cond l <$> kchk env e1 <*> kchk env e2 <*> kchk env e3
    kchk env (BinOp l e1 op e2)     = BinOp l <$> kchk env e1 <*> return op <*> kchk env e2
    kchk env (CompOp l e ops)       = CompOp l <$> kchk env e <*> kchk env ops
    kchk env (UnOp l op e)          = UnOp l op <$> kchk env e 
    kchk env (Dot l e n)            = Dot l <$> kchk env e <*> return n
    kchk env (DotI l e i)           = DotI l <$> kchk env e <*> return i
    kchk env (Lambda l ps ks e)     = Lambda l <$> kchk env ps <*> kchk env ks <*> kchk env e
    kchk env (Yield l e)            = Yield l <$> kchk env e
    kchk env (YieldFrom l e)        = YieldFrom l <$> kchk env e
    kchk env (Tuple l es)           = Tuple l <$> kchk env es
    kchk env (TupleComp l e c)      = TupleComp l <$> kchk env e <*> kchk env c
    kchk env (Record l fs)          = Record l <$> kchk env fs
    kchk env (RecordComp l n e c)   = RecordComp l n <$> kchk env e <*> kchk env c
    kchk env (List l es)            = List l <$> kchk env es
    kchk env (ListComp l e c)       = ListComp l <$> kchk env e <*> kchk env c
    kchk env (Dict l as)            = Dict l <$> kchk env as
    kchk env (DictComp l a c)       = DictComp l <$> kchk env a <*> kchk env c
    kchk env (Set l es)             = Set l <$> kchk env es
    kchk env (SetComp l e c)        = SetComp l <$> kchk env e <*> kchk env c
    kchk env (Paren l e)            = Paren l <$> kchk env e

instance KCheck Pattern where
    kchk env (PVar l n t)           = do t' <- kchk env t
                                         kexpect KType t'
                                         return $ PVar l n t'
    kchk env (PTuple l ps)          = PTuple l <$> kchk env ps
    kchk env (PList l ps p)         = PList l <$> kchk env ps <*> kchk env p
    kchk env (PIndex l e ix)        = PIndex l <$> kchk env e <*> kchk env ix
    kchk env (PSlice l e sl)        = PSlice l <$> kchk env e <*> kchk env sl
    kchk env (PDot l e n)           = PDot l <$> kchk env e <*> return n
    kchk env (PParen l p)           = PParen l <$> kchk env p

instance KCheck Exception where
    kchk env (Exception e mbe)      = Exception <$> kchk env e <*> kchk env mbe

instance KCheck Branch where
    kchk env (Branch e ss)          = Branch <$> kchk env e <*> kchk env ss

instance KCheck Handler where
    kchk env (Handler ex b)         = Handler ex <$> kchk env b

instance KCheck PosPar where
    kchk env (PosPar n t e p)       = do t' <- kchk env t
                                         kexpect KType t'
                                         PosPar n t' <$> kchk env e <*> kchk env p
    kchk env (PosSTAR n t)          = do t' <- kchk env t
                                         kexpect KRow t'
                                         return $ PosSTAR n t'
    kchk env PosNIL                 = return PosNIL
    
instance KCheck KwdPar where
    kchk env (KwdPar n t e k)       = do t' <- kchk env t
                                         kexpect KType t'
                                         KwdPar n t' <$> kchk env e <*> kchk env k
    kchk env (KwdSTAR n t)          = do t' <- kchk env t
                                         kexpect KRow t'
                                         return $ KwdSTAR n t'
    kchk env KwdNIL                 = return KwdNIL
    
instance KCheck PosArg where
    kchk env (PosArg e p)           = PosArg <$> kchk env e <*> kchk env p
    kchk env (PosStar e)            = PosStar <$> kchk env e
    kchk env PosNil                 = return PosNil
    
instance KCheck KwdArg where
    kchk env (KwdArg n e k)         = KwdArg n <$> kchk env e <*> kchk env k
    kchk env (KwdStar e)            = KwdStar <$> kchk env e
    kchk env KwdNil                 = return KwdNil
    
instance KCheck PosPat where
    kchk env (PosPat p ps)          = PosPat <$> kchk env p <*> kchk env ps
    kchk env (PosPatStar p)         = PosPatStar <$> kchk env p
    kchk env PosPatNil              = return PosPatNil
    
instance KCheck KwdPat where
    kchk env (KwdPat n p ps)        = KwdPat n <$> kchk env p <*> kchk env ps
    kchk env (KwdPatStar p)         = KwdPatStar <$> kchk env p
    kchk env KwdPatNil              = return KwdPatNil
    
instance KCheck OpArg where
    kchk env (OpArg op e)           = OpArg op <$> kchk env e

instance KCheck Comp where
    kchk env (CompFor l p e c)      = CompFor l <$> kchk env p <*> kchk env e <*> kchk env c
    kchk env (CompIf l e c)         = CompIf l <$> kchk env e <*> kchk env c
    kchk env NoComp                 = return NoComp

instance KCheck WithItem where
    kchk env (WithItem e p)         = WithItem <$> kchk env e <*> kchk env p

instance KCheck Elem where
    kchk env (Elem e)               = Elem <$> kchk env e
    kchk env (Star e)               = Star <$> kchk env e

instance KCheck Assoc where
    kchk env (Assoc e1 e2)          = Assoc <$> kchk env e1 <*> kchk env e2
    kchk env (StarStar e)           = StarStar <$> kchk env e
  
instance KCheck Slice where
    kchk env (Sliz l e1 e2 e3)      = Sliz l <$> kchk env e1 <*> kchk env e2 <*> kchk env e3


instance KCheck TSchema where
    kchk env (TSchema l q t d)      = do q' <- kchk env q
                                         let env1 = define (tenv q') env
                                         TSchema l q' <$> kchk env1 t <*> return d

instance KCheck TVar where
    kchk env (TV k n)               = return $ TV k n

instance KCheck TCon where
    kchk env (TC n ts)              = TC <$> return n <*> kchk env ts

instance KCheck TBind where
    kchk env (TBind v cs)           = TBind <$> kchk env v <*> kchk env cs
    
    tenv q                          = nTVars [q]

instance KCheck Type where
    kchk env (TVar l v)             = TVar l <$> kchk env v
    kchk env (TFun l es p k t)      = TFun l <$> kchk env es <*> kchk env p <*> kchk env k <*> kchk env t
    kchk env (TTuple l p)           = TTuple l <$> kchk env p
    kchk env (TRecord l k)          = TRecord l <$> kchk env k
    kchk env (TOpt l t)             = TOpt l <$> kchk env t
    kchk env (TUnion l as)          = TUnion l <$> return as
    kchk env (TCon l c)             = TCon l <$> kchk env c
    kchk env (TAt l c)              = TAt l <$> kchk env c
    kchk env (TNone l)              = return $ TNone l
    kchk env (TWild l)              = newTVar
    kchk env (TNil l)               = return $ TNil l
    kchk env (TRow l n t r)         = TRow l n <$> kchk env t <*> kchk env r


class KInfer t where
    kinfer                          :: t -> KindM Kind

instance (KInfer t) => KInfer (Maybe t) where
    kinfer Nothing                  = newKVar
    kinfer (Just t)                 = kinfer t

instance KInfer TVar where
    kinfer (TV k n)                 = return k

instance KInfer TCon where
    kinfer (TC c [])                = return $ undefined
    kinfer (TC n ts)                = do let kn = undefined
                                         ks <- mapM kinfer ts
                                         k <- newKVar
                                         kunify kn (KFun ks k)
                                         return k

instance KInfer TSchema where
    kinfer (TSchema _ q t _)        = do ks <- mapM kinfer q
                                         kexpect KType t
                                         return KType

instance KInfer TBind where
    kinfer (TBind tv us)            = do mapM_ (kexpect KType) us
                                         kinfer tv

instance KInfer Type where
    kinfer (TVar _ v)               = kinfer v
    kinfer (TCon _ c)               = do kexpect KType c
                                         return KType
    kinfer (TAt _ c)                = do kexpect KType c
                                         return KType
    kinfer (TFun _ fx p k t)        = do kexpect KRow fx
                                         kexpect KRow p
                                         kexpect KRow k
                                         kexpect KType t
                                         return KType
    kinfer (TTuple _ p)             = do kexpect KRow p
                                         return KType
    kinfer (TRecord _ k)            = do kexpect KRow k
                                         return KType
    kinfer (TUnion _ as)            = return KType
    kinfer (TOpt _ t)               = do kexpect KType t
                                         return KType
    kinfer (TNone _)                = return KType
    kinfer (TNil _)                 = return KRow
    kinfer (TRow _ n t r)           = do kexpect KType t
                                         kinfer r

kexpect k t                         = do k' <- kinfer t
                                         kunify k' k

kunify (KVar v1) (KVar v2)
  | v1 == v2                        = return ()
kunify (KVar v) k2                  = do when (v `elem` kfree k2) (infiniteKind v k2)
                                         substitute v k2
kunify k1 (KVar v)                  = do when (v `elem` kfree k1) (infiniteKind v k1)
                                         substitute v k1
kunify (KFun ks1 k1) (KFun ks2 k2)
  | length ks1 == length ks2        = do mapM_ (uncurry kunify) (ks1 `zip` ks2)
                                         kunify k1 k2
kunify k1 k2
  | k1 == k2                        = return ()
  | otherwise                       = noUnify k1 k2


kfree (KVar v)                      = []
kfree (KFun ks k)                   = concatMap kfree (k:ks)
kfree _                             = []


-- Kind substitution -----------------------------------------------------------------------------------------

class KSubst s where
    ksubst                          :: s -> KindM s

instance KSubst a => KSubst [a] where
    ksubst                          = mapM ksubst

instance KSubst a => KSubst (Maybe a) where
    ksubst                          = maybe (return Nothing) (\x -> Just <$> ksubst x)

instance KSubst Kind where
    ksubst (KVar v)                 = do s <- getSubstitution
                                         case Map.lookup v s of
                                            Just k  -> ksubst k
                                            Nothing -> return (KVar v)
    ksubst (KFun ks k)              = KFun <$> mapM ksubst ks <*> ksubst k
    ksubst k                        = return k
        
instance KSubst TSchema where
    ksubst (TSchema l q t dec)      = TSchema l <$> ksubst q <*> ksubst t <*> return dec

instance KSubst TVar where
    ksubst (TV k n)                 = TV <$> ksubst k <*> return n
    
instance KSubst TCon where
    ksubst (TC n ts)                = TC n <$> ksubst ts

instance KSubst TBind where
    ksubst (TBind v cs)             = TBind <$> ksubst v <*> ksubst cs

instance KSubst Type where
    ksubst (TVar l v)               = TVar l <$> ksubst v
    ksubst (TCon l c)               = TCon l <$> ksubst c
    ksubst (TAt l c)                = TAt l <$> ksubst c
    ksubst (TFun l fx p k t)        = TFun l <$> ksubst fx <*> ksubst p <*> ksubst k<*> ksubst t
    ksubst (TTuple l p)             = TTuple l <$> ksubst p
    ksubst (TRecord l k)            = TRecord l <$> ksubst k
    ksubst (TUnion l as)            = return $ TUnion l as
    ksubst (TOpt l t)               = TOpt l <$> ksubst t
    ksubst (TNone l)                = return $ TNone l
    ksubst (TWild l)                = return $ TWild l
    ksubst (TNil l)                 = return $ TNil l
    ksubst (TRow l n t r)           = TRow l n <$> ksubst t <*> ksubst r

instance KSubst Stmt where
    ksubst (Expr l e)               = Expr l <$> ksubst e
    ksubst (Assign l ts e)          = Assign l <$> ksubst ts <*> ksubst e
    ksubst (AugAssign l p op e)     = AugAssign l <$> ksubst p <*> return op <*> ksubst e
    ksubst (Assert l e mbe)         = Assert l <$> ksubst e <*> ksubst mbe
    ksubst (Pass l)                 = return $ Pass l
    ksubst (Delete l p)             = Delete l <$> ksubst p
    ksubst (Return l mbe)           = Return l <$> ksubst mbe
    ksubst (Raise l mbex)           = Raise l <$> ksubst mbex
    ksubst (Break l)                = return $ Break l
    ksubst (Continue l)             = return $ Continue l
    ksubst (If l bs els)            = If l <$> ksubst bs <*> ksubst els
    ksubst (While l e b els)        = While l <$> ksubst e <*> ksubst b <*> ksubst els
    ksubst (For l p e b els)        = For l <$> ksubst p <*> ksubst e <*> ksubst b <*> ksubst els
    ksubst (Try l b hs els fin)     = Try l <$> ksubst b <*> ksubst hs <*> ksubst els <*> ksubst fin
    ksubst (With l is b)            = With l <$> ksubst is <*> ksubst b
    ksubst (Data l mbt ss)          = Data l <$> ksubst mbt <*> ksubst ss
    ksubst (VarAssign l ps e)       = VarAssign l <$> ksubst ps <*> ksubst e
    ksubst (Decl l ds)              = Decl l <$> ksubst ds

instance KSubst Decl where
    ksubst (Def l n q p k ann b m)  = Def l n <$> ksubst q <*> ksubst p <*> ksubst k <*> ksubst ann <*> ksubst b <*> return m
    ksubst (Actor l n q p k ann b)  = Actor l n <$> ksubst q <*> ksubst p <*> ksubst k <*> ksubst ann <*> ksubst b
    ksubst (Class l n q as b)       = Class l n <$> ksubst q <*> ksubst as <*> ksubst b
    ksubst (Protocol l n q as b)    = Protocol l n <$> ksubst q <*> ksubst as <*> ksubst b
    ksubst (Extension l n q as b)   = Extension l n <$> ksubst q <*> ksubst as <*> ksubst b
    ksubst (Signature l ns t)       = Signature l ns <$> ksubst t

instance KSubst Expr where
    ksubst (Var l n)                = return $ Var l n
    ksubst (Int l i s)              = return $ Int l i s
    ksubst (Float l f s)            = return $ Float l f s
    ksubst (Imaginary l i s)        = return $ Imaginary l i s
    ksubst (Bool l b)               = return $ Bool l b
    ksubst (None l)                 = return $ None l
    ksubst (NotImplemented l)       = return $ NotImplemented l
    ksubst (Ellipsis l)             = return $ Ellipsis l
    ksubst (Strings l ss)           = return $ Strings l ss
    ksubst (BStrings l ss)          = return $ BStrings l ss
    ksubst (Call l e ps ks)         = Call l <$> ksubst e <*> ksubst ps <*> ksubst ks
    ksubst (Index l e is)           = Index l <$> ksubst e <*> ksubst is
    ksubst (Slice l e sl)           = Slice l <$> ksubst e <*> ksubst sl
    ksubst (Cond l e1 e2 e3)        = Cond l <$> ksubst e1 <*> ksubst e2 <*> ksubst e3
    ksubst (BinOp l e1 op e2)       = BinOp l <$> ksubst e1 <*> return op <*> ksubst e2
    ksubst (CompOp l e ops)         = CompOp l <$> ksubst e <*> ksubst ops
    ksubst (UnOp l op e)            = UnOp l op <$> ksubst e 
    ksubst (Dot l e n)              = Dot l <$> ksubst e <*> return n
    ksubst (DotI l e i)             = DotI l <$> ksubst e <*> return i
    ksubst (Lambda l ps ks e)       = Lambda l <$> ksubst ps <*> ksubst ks <*> ksubst e
    ksubst (Yield l e)              = Yield l <$> ksubst e
    ksubst (YieldFrom l e)          = YieldFrom l <$> ksubst e
    ksubst (Tuple l es)             = Tuple l <$> ksubst es
    ksubst (TupleComp l e c)        = TupleComp l <$> ksubst e <*> ksubst c
    ksubst (Record l fs)            = Record l <$> ksubst fs
    ksubst (RecordComp l n e c)     = RecordComp l n <$> ksubst e <*> ksubst c
    ksubst (List l es)              = List l <$> ksubst es
    ksubst (ListComp l e c)         = ListComp l <$> ksubst e <*> ksubst c
    ksubst (Dict l as)              = Dict l <$> ksubst as
    ksubst (DictComp l a c)         = DictComp l <$> ksubst a <*> ksubst c
    ksubst (Set l es)               = Set l <$> ksubst es
    ksubst (SetComp l e c)          = SetComp l <$> ksubst e <*> ksubst c
    ksubst (Paren l e)              = Paren l <$> ksubst e

instance KSubst Pattern where
    ksubst (PVar l n a)             = PVar l n <$> ksubst a
--    ksubst (PRecord l ps)           = PRecord l <$> ksubst ps
    ksubst (PTuple l ps)            = PTuple l <$> ksubst ps
    ksubst (PList l ps p)           = PList l <$> ksubst ps <*> ksubst p
    ksubst (PIndex l e ix)          = PIndex l <$> ksubst e <*> ksubst ix
    ksubst (PSlice l e sl)          = PSlice l <$> ksubst e <*> ksubst sl
    ksubst (PDot l e n)             = PDot l <$> ksubst e <*> return n
    ksubst (PParen l p)             = PParen l <$> ksubst p

instance KSubst Exception where
    ksubst (Exception e mbe)        = Exception <$> ksubst e <*> ksubst mbe

instance KSubst Branch where
    ksubst (Branch e ss)            = Branch <$> ksubst e <*> ksubst ss

instance KSubst Handler where
    ksubst (Handler ex b)           = Handler ex <$> ksubst b

instance KSubst PosPar where
    ksubst (PosPar n t e p)         = PosPar n <$> ksubst t <*> ksubst e <*> ksubst p
    ksubst (PosSTAR n t)            = PosSTAR n <$> ksubst t
    ksubst PosNIL                   = return PosNIL
    
instance KSubst KwdPar where
    ksubst (KwdPar n t e k)         = KwdPar n <$> ksubst t <*> ksubst e <*> ksubst k
    ksubst (KwdSTAR n t)            = KwdSTAR n <$> ksubst t
    ksubst KwdNIL                   = return KwdNIL
    
instance KSubst PosArg where
    ksubst (PosArg e p)             = PosArg <$> ksubst e <*> ksubst p
    ksubst (PosStar e)              = PosStar <$> ksubst e
    ksubst PosNil                   = return PosNil
    
instance KSubst KwdArg where
    ksubst (KwdArg n e k)           = KwdArg n <$> ksubst e <*> ksubst k
    ksubst (KwdStar e)              = KwdStar <$> ksubst e
    ksubst KwdNil                   = return KwdNil
    
instance KSubst PosPat where
    ksubst (PosPat p ps)            = PosPat <$> ksubst p <*> ksubst ps
    ksubst (PosPatStar p)           = PosPatStar <$> ksubst p
    ksubst PosPatNil                = return PosPatNil
    
instance KSubst KwdPat where
    ksubst (KwdPat n p ps)          = KwdPat n <$> ksubst p <*> ksubst ps
    ksubst (KwdPatStar p)           = KwdPatStar <$> ksubst p
    ksubst KwdPatNil                = return KwdPatNil
    
instance KSubst OpArg where
    ksubst (OpArg op e)             = OpArg op <$> ksubst e

instance KSubst Comp where
    ksubst (CompFor l p e c)        = CompFor l <$> ksubst p <*> ksubst e <*> ksubst c
    ksubst (CompIf l e c)           = CompIf l <$> ksubst e <*> ksubst c
    ksubst NoComp                   = return NoComp

instance KSubst WithItem where
    ksubst (WithItem e p)           = WithItem <$> ksubst e <*> ksubst p

instance KSubst Elem where
    ksubst (Elem e)                 = Elem <$> ksubst e
    ksubst (Star e)                 = Star <$> ksubst e

instance KSubst Assoc where
    ksubst (Assoc e1 e2)            = Assoc <$> ksubst e1 <*> ksubst e2
    ksubst (StarStar e)             = StarStar <$> ksubst e
  
instance KSubst Slice where
    ksubst (Sliz l e1 e2 e3)        = Sliz l <$> ksubst e1 <*> ksubst e2 <*> ksubst e3


--------------------------------------------------------------------------------------------------------------

data KindError                      = KindError SrcLoc Kind Kind
                                    | InfiniteKind SrcLoc KVar Kind
                                    deriving (Show)

instance Control.Exception.Exception KindError

noUnify k1 k2                       = Control.Exception.throw $ KindError NoLoc k1 k2
infiniteKind v k                    = Control.Exception.throw $ InfiniteKind NoLoc v k

kindError (KindError l k1 k2)       = (l, " Incompatible kinds: " ++ prstr k1 ++ " and " ++ prstr k2)
kindError (InfiniteKind l v k)      = (l, " Infinite kind inferred: " ++ prstr  v ++ " = " ++ prstr k)
