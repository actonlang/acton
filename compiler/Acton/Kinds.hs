{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Acton.Kinds(check, kindError) where

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
import Acton.Env (tybound, tyfree)
import qualified Acton.Env
import Acton.Solver


check                               :: Acton.Env.Env -> Module -> IO Module
check ienv (Module l imps ss)       = return (Module l imps ss1)
  where env                         = kenv0 ienv
        ss1                         = runKindM (kchkTop env ss)


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


newName s                           = Internal s <$> newUnique <*> return KindPass

newKVar                             = KVar <$> newName "K"


data KEnv                           = KEnv { impenv :: Acton.Env.Env, tcons :: Kinds, tvars :: [TVar], wildOK :: Bool }

type Kinds                          = [(Name,Kind)]

kenv0 ienv                          = KEnv { impenv = ienv, tcons = [], tvars = [], wildOK = True }

extcons ke env                      = env { tcons = ke ++ tcons env }

extvars vs env                      = env { tvars = nub vs ++ tvars env }

noWild env                          = env { wildOK = False }

tconKind (NoQual n) env             = case lookup n (tcons env) of
                                        Just k  -> k
                                        Nothing -> Acton.Env.tconKind (NoQual n) (impenv env)
tconKind qn env                     = Acton.Env.tconKind qn (impenv env)


instance Pretty (Name,Kind) where
    pretty (n,k)                    = pretty n <+> colon <+> pretty k

---------------------------------------------------------------------------------------------------------------------

kchkTop env ss                      = do ss <- kchkSuite env ss
                                         ksubst ss

class KCheck a where
    kchk                            :: KEnv -> a -> KindM a

instance KCheck a => KCheck [a] where
    kchk env                        = mapM (kchk env)

instance KCheck a => KCheck (Maybe a) where
    kchk env                        = traverse (kchk env)

kchkSuite env []                    = return []
kchkSuite env (Decl l ds : ss)      = do ds <- mapM instd ds
                                         let env1 = extcons (concatMap kinds ds) env
                                         ds <- kchk env1 ds
                                         ss <- kchkSuite env1 ss
                                         return (Decl l ds : ss)
  where instd d@Class{}             = do q <- mapM instb (qual d); return d { qual = q }
        instd d@Protocol{}          = do q <- mapM instb (qual d); return d { qual = q }
        instd d                     = return d
        instb (TBind v us)          = TBind <$> instv v <*> return us
        instv (TV KWild n)          = TV <$> newKVar <*> return n
        instv v                     = return v
        kinds (Class _ n q _ _)     = [(n,kind KType q)]
        kinds (Protocol _ n q _ _)  = [(n,kind KProto q)]
        kinds _                     = []
        kind k []                   = k
        kind k q                    = KFun [ tvkind v | TBind v _ <- q ] k
kchkSuite env (s : ss)              = do s <- kchk env s; ss <- kchkSuite env ss; return (s:ss)

instance KCheck Stmt where
    kchk env (Expr l e)             = Expr l <$> kchk env e
    kchk env (Assign l ts e)        = Assign l <$> kchk env ts <*> kchk env e
    kchk env (Update l ts e)        = Update l <$> kchk env ts <*> kchk env e
    kchk env (IUpdate l t op e)     = IUpdate l <$> kchk env t <*> return op <*> kchk env e
    kchk env (Assert l e mbe)       = Assert l <$> kchk env e <*> kchk env mbe
    kchk env (Pass l)               = return $ Pass l
    kchk env (Delete l p)           = Delete l <$> kchk env p
    kchk env (Return l mbe)         = Return l <$> kchk env mbe
    kchk env (Raise l mbex)         = Raise l <$> kchk env mbex
    kchk env (Break l)              = return $ Break l
    kchk env (Continue l)           = return $ Continue l
    kchk env (If l bs els)          = If l <$> kchk env bs <*> kchkSuite env els
    kchk env (While l e b els)      = While l <$> kchk env e <*> kchkSuite env b <*> kchkSuite env els
    kchk env (For l p e b els)      = For l <$> kchk env p <*> kchk env e <*> kchkSuite env b <*> kchkSuite env els
    kchk env (Try l b hs els fin)   = Try l <$> kchkSuite env b <*> kchk env hs <*> kchkSuite env els <*> kchkSuite env fin
    kchk env (With l is b)          = With l <$> kchk env is <*> kchkSuite env b
    kchk env (Data l mbt ss)        = Data l <$> kchk env mbt <*> kchkSuite env ss
    kchk env (VarAssign l ps e)     = VarAssign l <$> kchk env ps <*> kchk env e
    kchk env (After l e e')         = After l <$> kchk env e <*> kchk env e'
    kchk env (Decl l ds)            = Decl l <$> kchk env ds
    kchk env (Signature l ns t d)   = Signature l ns <$> kchk env t <*> return d

instance KCheck Decl where
    kchk env (Def l n q p k t b m)  = Def l n <$> kchkQual env q <*> kchk env1 p <*> kchk env1 k <*> kexp KType env1 t <*> kchkSuite env1 b <*> return m
      where env1 | null q           = extvars ((tyfree p ++ tyfree k ++ tyfree t) \\ tvars env) env
                 | otherwise        = extvars (tybound q) env
    kchk env (Actor l n q p k t b)  = Actor l n <$> kchkQual env q <*> kchk env1 p <*> kchk env1 k <*> kexp KType env1 t <*> kchkSuite env1 b
      where env1 | null q           = extvars ((tyfree p ++ tyfree k ++ tyfree t) \\ tvars env) env
                 | otherwise        = extvars (tybound q) env
    kchk env (Class l n q us b)     = Class l n <$> kchkQual env q <*> kchkBounds env1 us <*> kchkSuite env1 b
      where env1                    = extvars (tybound q) env
    kchk env (Protocol l n q us b)  = Protocol l n <$> kchkQual env q <*> kchkPBounds env1 us <*> kchkSuite env1 b
      where env1                    = extvars (tybound q) env
    kchk env (Extension l n q us b) = Extension l n <$> kchkQual env q <*> kchkPBounds env1 us <*> kchkSuite env1 b
      where env1                    = extvars (tybound q) env

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
    kchk env (DotI l e i tl)        = DotI l <$> kchk env e <*> return i <*> return tl
    kchk env (Lambda l ps ks e)     = Lambda l <$> kchk env ps <*> kchk env ks <*> kchk env e
    kchk env (Yield l e)            = Yield l <$> kchk env e
    kchk env (YieldFrom l e)        = YieldFrom l <$> kchk env e
    kchk env (Tuple l es ks)        = Tuple l <$> kchk env es <*> kchk env ks
    kchk env (List l es)            = List l <$> kchk env es
    kchk env (ListComp l e c)       = ListComp l <$> kchk env e <*> kchk env c
    kchk env (Dict l as)            = Dict l <$> kchk env as
    kchk env (DictComp l a c)       = DictComp l <$> kchk env a <*> kchk env c
    kchk env (Set l es)             = Set l <$> kchk env es
    kchk env (SetComp l e c)        = SetComp l <$> kchk env e <*> kchk env c
    kchk env (Paren l e)            = Paren l <$> kchk env e

instance KCheck Pattern where
    kchk env (PVar l n t)           = PVar l n <$> kexp KType env t
    kchk env (PTuple l ps ks)       = PTuple l <$> kchk env ps <*> kchk env ks
    kchk env (PList l ps p)         = PList l <$> kchk env ps <*> kchk env p
    kchk env (PParen l p)           = PParen l <$> kchk env p

instance KCheck Target where
    kchk env (TaVar l n)            = return $ TaVar l n
    kchk env (TaTuple l ps)         = TaTuple l <$> kchk env ps
    kchk env (TaIndex l e ix)       = TaIndex l <$> kchk env e <*> kchk env ix
    kchk env (TaSlice l e sl)       = TaSlice l <$> kchk env e <*> kchk env sl
    kchk env (TaDot l e n)          = TaDot l <$> kchk env e <*> return n
    kchk env (TaDotI l e i tl)      = TaDotI l <$> kchk env e <*> return i <*> return tl
    kchk env (TaParen l p)          = TaParen l <$> kchk env p

instance KCheck Exception where
    kchk env (Exception e mbe)      = Exception <$> kchk env e <*> kchk env mbe

instance KCheck Branch where
    kchk env (Branch e ss)          = Branch <$> kchk env e <*> kchkSuite env ss

instance KCheck Handler where
    kchk env (Handler ex b)         = Handler <$> kchk env ex <*> kchkSuite env b

instance KCheck Except where
    kchk env (ExceptAll l)          = return $ ExceptAll l
    kchk env (Except l x)           = do kexp KType env (TC x []); return $ Except l x
    kchk env (ExceptAs l x n)       = do kexp KType env (TC x []); return $ ExceptAs l x n

instance KCheck PosPar where
    kchk env (PosPar n t e p)       = PosPar n <$> kchk env t <*> kchk env e <*> kchk env p
    kchk env (PosSTAR n t)          = PosSTAR n <$> kexp KType env t
    kchk env PosNIL                 = return PosNIL
    
instance KCheck KwdPar where
    kchk env (KwdPar n t e k)       = KwdPar n <$> kchk env t <*> kchk env e <*> kchk env k
    kchk env (KwdSTAR n t)          = KwdSTAR n <$> kexp KType env t
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
  
instance KCheck Sliz where
    kchk env (Sliz l e1 e2 e3)      = Sliz l <$> kchk env e1 <*> kchk env e2 <*> kchk env e3

instance KCheck TSchema where
    kchk env (TSchema l q t)
      | null ambig                  = TSchema l <$> kchkQual env q <*> kexp KType env1 t
      | otherwise                   = Acton.Env.err2 ambig "Ambiguous type variable in schema:"
      where env1 | null q           = extvars (tyfree t \\ tvars env) env
                 | otherwise        = extvars (tybound q) env
            ambig                   = tybound q \\ tyfree t

kchkQual env []                     = return []
kchkQual env (TBind v us : q)
  | v `elem` tvars env              = Acton.Env.err1 v "Type variable already in scope:"
  | otherwise                       = do (_k,v) <- kinfer env v
                                         us <- kchkBounds env us
                                         q <- kchkQual (extvars [v] env) q
                                         return $ TBind v us : q

kchkBounds env []                   = return []
kchkBounds env (u:us)               = do (k,u) <- kinfer env u
                                         case k of
                                            KProto -> (:) u <$> kchkPBounds env us
                                            _ -> do kunify (loc u) k KType; (:) u <$> kchkPBounds env us
    
kchkPBounds env us                  = mapM (kexp KProto $ noWild env) us



class KInfer t where
    kinfer                          :: KEnv -> t -> KindM (Kind,t)

instance (KInfer t) => KInfer (Maybe t) where
    kinfer env Nothing              = do k <- newKVar; return (k, Nothing)
    kinfer env (Just t)             = do (k,t) <- kinfer env t; return (k, Just t)

instance KInfer TVar where
    kinfer env (TV k n)             = case k of
                                        KWild -> do k' <- newKVar; return (k', TV k' n)
                                        _     -> return (k, TV k n)

instance KInfer TCon where
    kinfer env (TC n [])            = return (tconKind n env, TC n [])
    kinfer env (TC n ts)            = do let kn = tconKind n env
                                         (ks,ts) <- fmap unzip $ mapM (kinfer env) ts
                                         k <- newKVar
                                         kunify (loc n) kn (KFun ks k)
                                         k <- ksubst k
                                         return (k, TC n ts)

instance KInfer Type where
    kinfer env (TWild l)
      | wildOK env                  = do k <- newKVar
                                         n <- newName "W"
                                         return (k, TVar l (TV k n))
      | otherwise                   = Acton.Env.err1 l "Illegal type wildcard"
    kinfer env (TVar l v)
      | not (skolem v) || v `elem` tvars env
                                    = do (k,v) <- kinfer env v
                                         return (k, TVar l v)
      | otherwise                   = Acton.Env.err1 v "Unbound type variable:"
    kinfer env (TCon l c)           = do (k,c) <- kinfer env c
                                         case k of
                                            KProto -> return (KType, TExist l c)
                                            _ -> do kunify l k KType; return (KType, TCon l c)
    kinfer env (TExist l p)         = do p <- kexp KProto env p
                                         return (KType, TExist l p)
    kinfer env (TFun l fx p k t)    = do fx <- kexp XRow env fx
                                         p <- kexp PRow env p
                                         k <- kexp KRow env k
                                         t <- kexp KType env t
                                         return (KType, TFun l fx p k t)
    kinfer env (TTuple l p k)       = do p <- kexp PRow env p
                                         k <- kexp KRow env k
                                         return (KType, TTuple l p k)
    kinfer env (TUnion l as)        = return (KType, TUnion l as)
    kinfer env (TOpt l t)           = do t <- kexp KType env t
                                         return (KType, TOpt l t)
    kinfer env (TNone l)            = return (KType, TNone l)
    kinfer env (TNil l k)           = return (k, TNil l k)
    kinfer env (TRow l k n t r)     = do t <- kchk env t
                                         r <- kexp k env r
                                         return (k, TRow l k n t r)

kexp k env t                        = do (k',t) <- kinfer env t
                                         kunify (loc t) k' k
                                         return t

kunify l k1 k2                      = do k1 <- ksubst k1; k2 <- ksubst k2; kunify' l k1 k2

kunify' l (KVar v1) (KVar v2)
  | v1 == v2                        = return ()
kunify' l (KVar v) k2               = do when (v `elem` kfree k2) (infiniteKind l v k2)
                                         substitute v k2
kunify' l k1 (KVar v)               = do when (v `elem` kfree k1) (infiniteKind l v k1)
                                         substitute v k1
kunify' l (KFun ks1 k1) (KFun ks2 k2)
  | length ks1 == length ks2        = do mapM_ (uncurry $ kunify l) (ks1 `zip` ks2)
                                         kunify l k1 k2
kunify' l k1 k2
  | k1 == k2                        = return ()
  | otherwise                       = noUnify l k1 k2


kfree (KVar v)                      = []
kfree (KFun ks k)                   = concatMap kfree (k:ks)
kfree _                             = []


-- Instantiate wildcards / apply kind substitution -------------------------------------------------------------------------

class KSubst s where
    ksubst                          :: s -> KindM s

instance KSubst a => KSubst [a] where
    ksubst                          = mapM (ksubst)

instance KSubst a => KSubst (Maybe a) where
    ksubst                          = maybe (return Nothing) (\x -> Just <$> ksubst x)

instance KSubst Kind where
    ksubst KWild                    = return KWild
    ksubst (KVar v)                 = do s <- getSubstitution
                                         case Map.lookup v s of
                                            Just k  -> ksubst k
                                            Nothing -> return (KVar v)
    ksubst (KFun ks k)              = KFun <$> mapM (ksubst) ks <*> ksubst k
    ksubst k                        = return k
        
instance KSubst TSchema where
    ksubst (TSchema l q t)          = TSchema l <$> ksubst q <*> ksubst t

instance KSubst TVar where
    ksubst (TV k n)                 = TV <$> ksubst k <*> return n
    
instance KSubst TCon where
    ksubst (TC n ts)                = TC n <$> ksubst ts

instance KSubst TBind where
    ksubst (TBind v cs)             = TBind <$> ksubst v <*> ksubst cs

instance KSubst Type where
    ksubst (TVar l v)               = TVar l <$> ksubst v
    ksubst (TCon l c)               = TCon l <$> ksubst c
    ksubst (TExist l p)             = TExist l <$> ksubst p
    ksubst (TFun l fx p k t)        = TFun l <$> ksubst fx <*> ksubst p <*> ksubst k<*> ksubst t
    ksubst (TTuple l p k)           = TTuple l <$> ksubst p <*> ksubst k
    ksubst (TUnion l as)            = return $ TUnion l as
    ksubst (TOpt l t)               = TOpt l <$> ksubst t
    ksubst (TNone l)                = return $ TNone l
    ksubst (TNil l s)               = return $ TNil l s
    ksubst (TRow l k n t r)         = TRow l k n <$> ksubst t <*> ksubst r

instance KSubst Stmt where
    ksubst (Expr l e)               = Expr l <$> ksubst e
    ksubst (Assign l ts e)          = Assign l <$> ksubst ts <*> ksubst e
    ksubst (Update l ts e)          = Update l <$> ksubst ts <*> ksubst e
    ksubst (IUpdate l t op e)       = IUpdate l <$> ksubst t <*> return op <*> ksubst e
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
    ksubst (After l e e')           = After l <$> ksubst e <*> ksubst e'
    ksubst (Decl l ds)              = Decl l <$> ksubst ds
    ksubst (Signature l ns t d)     = Signature l ns <$> ksubst t <*> return d

instance KSubst Decl where
    ksubst (Def l n q p k ann b m)  = Def l n <$> ksubst q <*> ksubst p <*> ksubst k <*> ksubst ann <*> ksubst b <*> return m
    ksubst (Actor l n q p k ann b)  = Actor l n <$> ksubst q <*> ksubst p <*> ksubst k <*> ksubst ann <*> ksubst b
    ksubst (Class l n q as b)       = Class l n <$> ksubst q <*> ksubst as <*> ksubst b
    ksubst (Protocol l n q as b)    = Protocol l n <$> ksubst q <*> ksubst as <*> ksubst b
    ksubst (Extension l n q as b)   = Extension l n <$> ksubst q <*> ksubst as <*> ksubst b

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
    ksubst (DotI l e i t)           = DotI l <$> ksubst e <*> return i <*> return t
    ksubst (Lambda l ps ks e)       = Lambda l <$> ksubst ps <*> ksubst ks <*> ksubst e
    ksubst (Yield l e)              = Yield l <$> ksubst e
    ksubst (YieldFrom l e)          = YieldFrom l <$> ksubst e
    ksubst (Tuple l es ks)          = Tuple l <$> ksubst es <*> ksubst ks
    ksubst (List l es)              = List l <$> ksubst es
    ksubst (ListComp l e c)         = ListComp l <$> ksubst e <*> ksubst c
    ksubst (Dict l as)              = Dict l <$> ksubst as
    ksubst (DictComp l a c)         = DictComp l <$> ksubst a <*> ksubst c
    ksubst (Set l es)               = Set l <$> ksubst es
    ksubst (SetComp l e c)          = SetComp l <$> ksubst e <*> ksubst c
    ksubst (Paren l e)              = Paren l <$> ksubst e

instance KSubst Pattern where
    ksubst (PVar l n a)             = PVar l n <$> ksubst a
    ksubst (PTuple l ps ks)         = PTuple l <$> ksubst ps <*> ksubst ks
    ksubst (PList l ps p)           = PList l <$> ksubst ps <*> ksubst p
    ksubst (PParen l p)             = PParen l <$> ksubst p

instance KSubst Target where
    ksubst (TaVar l n)              = return $ TaVar l n
    ksubst (TaTuple l ts)           = TaTuple l <$> ksubst ts
    ksubst (TaIndex l e ix)         = TaIndex l <$> ksubst e <*> ksubst ix
    ksubst (TaSlice l e sl)         = TaSlice l <$> ksubst e <*> ksubst sl
    ksubst (TaDot l e n)            = TaDot l <$> ksubst e <*> return n
    ksubst (TaDotI l e i tl)        = TaDotI l <$> ksubst e <*> return i <*> return tl
    ksubst (TaParen l p)            = TaParen l <$> ksubst p

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
  
instance KSubst Sliz where
    ksubst (Sliz l e1 e2 e3)        = Sliz l <$> ksubst e1 <*> ksubst e2 <*> ksubst e3


--------------------------------------------------------------------------------------------------------------

data KindError                      = KindError SrcLoc Kind Kind
                                    | InfiniteKind SrcLoc KVar Kind
                                    deriving (Show)

instance Control.Exception.Exception KindError

noUnify l k1 k2                     = Control.Exception.throw $ KindError l k1 k2
infiniteKind l v k                  = Control.Exception.throw $ InfiniteKind l v k

kindError (KindError l k1 k2)       = (l, " Expected a " ++ prstr k2 ++ ", actual kind is " ++ prstr k1)
kindError (InfiniteKind l v k)      = (l, " Infinite kind inferred: " ++ prstr  v ++ " = " ++ prstr k)
