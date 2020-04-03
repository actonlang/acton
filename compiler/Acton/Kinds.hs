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


data KEnv                           = KEnv { impenv :: Acton.Env.Env, tcons :: Kinds, tvars :: [TVar], wildOK :: Bool, wildSigOK :: Bool }

type Kinds                          = [(Name,Kind)]

kenv0 ienv                          = KEnv { impenv = ienv, tcons = [], tvars = [], wildOK = True, wildSigOK = True }

extcons ke env                      = env { tcons = ke ++ tcons env }

extvars vs env                      = env { tvars = nub vs ++ tvars env }

noWild env                          = env { wildOK = False }

noWildSig env                       = env { wildSigOK = False }

tconKind (NoQual n) env             = case lookup n (tcons env) of
                                        Just k  -> k
                                        Nothing -> Acton.Env.tconKind (NoQual n) (impenv env)
tconKind qn env                     = Acton.Env.tconKind qn (impenv env)


instance Pretty (Name,Kind) where
    pretty (n,k)                    = pretty n <+> colon <+> pretty k

---------------------------------------------------------------------------------------------------------------------

kchkTop env ss                      = do ss <- kchkSuite env ss
                                         ksubst True ss

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
    kchk env (Signature l ns t d)
      | wildSigOK env               = Signature l ns <$> kchk env t <*> return d
      | otherwise                   = Signature l ns <$> kchk (noWild env) t <*> return d

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
      where env1                    = extvars (tybound q) $ noWildSig env
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
    kchk env (PosPar n t e p)       = PosPar n <$> kexp KType env t <*> kchk env e <*> kchk env p
    kchk env (PosSTAR n t)          = PosSTAR n <$> kexp KType env t
    kchk env PosNIL                 = return PosNIL
    
instance KCheck KwdPar where
    kchk env (KwdPar n t e k)       = KwdPar n <$> kexp KType env t <*> kchk env e <*> kchk env k
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
  | otherwise                       = do (_k,v) <- kinfer env1 v
                                         us <- kchkBounds env1 us
                                         q <- kchkQual (extvars [v] env1) q
                                         return $ TBind v us : q
  where env1                        = noWild env

kchkBounds env []                   = return []
kchkBounds env (u:us)               = do (k,u) <- kinfer (noWild env) u
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
                                         k <- ksubst False k
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
    kinfer env (TFun l fx p k t)    = do fx <- kexp KFX env fx
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
    kinfer env (TRow l k n t r)     = do t <- kexp KType env t
                                         r <- kexp k env r
                                         return (k, TRow l k n t r)
    kinfer env (TFX l fx)           = do fx <- kchk env fx
                                         return (KFX, TFX l fx)

instance KCheck FX where
    kchk env (FXActor)              = return FXActor
    kchk env (FXAsync)              = return FXAsync
    kchk env (FXAct t)              = FXAct <$> kexp KType env t
    kchk env (FXMut t)              = FXMut <$> kexp KType env t
    kchk env (FXPure)               = return FXPure


kexp k env t                        = do (k',t) <- kinfer env t
                                         kunify (loc t) k' k
                                         return t

kunify l k1 k2                      = do k1 <- ksubst False k1; k2 <- ksubst False k2; kunify' l k1 k2

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
    ksubst                          :: Bool -> s -> KindM s

instance KSubst a => KSubst [a] where
    ksubst g                        = mapM (ksubst g)

instance KSubst a => KSubst (Maybe a) where
    ksubst g                        = maybe (return Nothing) (\x -> Just <$> ksubst g x)

instance KSubst Kind where
    ksubst g KWild                  = return KWild
    ksubst g (KVar v)               = do s <- getSubstitution
                                         case Map.lookup v s of
                                            Just k  -> ksubst g k
                                            Nothing -> return (if g then KType else KVar v)
    ksubst g (KFun ks k)            = KFun <$> mapM (ksubst g) ks <*> ksubst g k
    ksubst g k                      = return k
        
instance KSubst TSchema where
    ksubst g (TSchema l q t)        = TSchema l <$> ksubst g q <*> ksubst g t

instance KSubst TVar where
    ksubst g (TV k n)               = TV <$> ksubst g k <*> return n
    
instance KSubst TCon where
    ksubst g (TC n ts)              = TC n <$> ksubst g ts

instance KSubst TBind where
    ksubst g (TBind v cs)           = TBind <$> ksubst g v <*> ksubst g cs

instance KSubst Type where
    ksubst g (TVar l v)             = TVar l <$> ksubst g v
    ksubst g (TCon l c)             = TCon l <$> ksubst g c
    ksubst g (TExist l p)           = TExist l <$> ksubst g p
    ksubst g (TFun l fx p k t)      = TFun l <$> ksubst g fx <*> ksubst g p <*> ksubst g k<*> ksubst g t
    ksubst g (TTuple l p k)         = TTuple l <$> ksubst g p <*> ksubst g k
    ksubst g (TUnion l as)          = return $ TUnion l as
    ksubst g (TOpt l t)             = TOpt l <$> ksubst g t
    ksubst g (TNone l)              = return $ TNone l
    ksubst g (TNil l s)             = return $ TNil l s
    ksubst g (TRow l k n t r)       = TRow l k n <$> ksubst g t <*> ksubst g r

instance KSubst Stmt where
    ksubst g (Expr l e)             = Expr l <$> ksubst g e
    ksubst g (Assign l ts e)        = Assign l <$> ksubst g ts <*> ksubst g e
    ksubst g (Update l ts e)        = Update l <$> ksubst g ts <*> ksubst g e
    ksubst g (IUpdate l t op e)     = IUpdate l <$> ksubst g t <*> return op <*> ksubst g e
    ksubst g (Assert l e mbe)       = Assert l <$> ksubst g e <*> ksubst g mbe
    ksubst g (Pass l)               = return $ Pass l
    ksubst g (Delete l p)           = Delete l <$> ksubst g p
    ksubst g (Return l mbe)         = Return l <$> ksubst g mbe
    ksubst g (Raise l mbex)         = Raise l <$> ksubst g mbex
    ksubst g (Break l)              = return $ Break l
    ksubst g (Continue l)           = return $ Continue l
    ksubst g (If l bs els)          = If l <$> ksubst g bs <*> ksubst g els
    ksubst g (While l e b els)      = While l <$> ksubst g e <*> ksubst g b <*> ksubst g els
    ksubst g (For l p e b els)      = For l <$> ksubst g p <*> ksubst g e <*> ksubst g b <*> ksubst g els
    ksubst g (Try l b hs els fin)   = Try l <$> ksubst g b <*> ksubst g hs <*> ksubst g els <*> ksubst g fin
    ksubst g (With l is b)          = With l <$> ksubst g is <*> ksubst g b
    ksubst g (Data l mbt ss)        = Data l <$> ksubst g mbt <*> ksubst g ss
    ksubst g (VarAssign l ps e)     = VarAssign l <$> ksubst g ps <*> ksubst g e
    ksubst g (After l e e')         = After l <$> ksubst g e <*> ksubst g e'
    ksubst g (Decl l ds)            = Decl l <$> ksubst g ds
    ksubst g (Signature l ns t d)   = Signature l ns <$> ksubst g t <*> return d

instance KSubst Decl where
    ksubst g (Def l n q p k ann b m)    = Def l n <$> ksubst g q <*> ksubst g p <*> ksubst g k <*> ksubst g ann <*> ksubst g b <*> return m
    ksubst g (Actor l n q p k ann b)    = Actor l n <$> ksubst g q <*> ksubst g p <*> ksubst g k <*> ksubst g ann <*> ksubst g b
    ksubst g (Class l n q as b)         = Class l n <$> ksubst g q <*> ksubst g as <*> ksubst g b
    ksubst g (Protocol l n q as b)      = Protocol l n <$> ksubst g q <*> ksubst g as <*> ksubst g b
    ksubst g (Extension l n q as b)     = Extension l n <$> ksubst g q <*> ksubst g as <*> ksubst g b

instance KSubst Expr where
    ksubst g (Var l n)              = return $ Var l n
    ksubst g (Int l i s)            = return $ Int l i s
    ksubst g (Float l f s)          = return $ Float l f s
    ksubst g (Imaginary l i s)      = return $ Imaginary l i s
    ksubst g (Bool l b)             = return $ Bool l b
    ksubst g (None l)               = return $ None l
    ksubst g (NotImplemented l)     = return $ NotImplemented l
    ksubst g (Ellipsis l)           = return $ Ellipsis l
    ksubst g (Strings l ss)         = return $ Strings l ss
    ksubst g (BStrings l ss)        = return $ BStrings l ss
    ksubst g (Call l e ps ks)       = Call l <$> ksubst g e <*> ksubst g ps <*> ksubst g ks
    ksubst g (Index l e is)         = Index l <$> ksubst g e <*> ksubst g is
    ksubst g (Slice l e sl)         = Slice l <$> ksubst g e <*> ksubst g sl
    ksubst g (Cond l e1 e2 e3)      = Cond l <$> ksubst g e1 <*> ksubst g e2 <*> ksubst g e3
    ksubst g (BinOp l e1 op e2)     = BinOp l <$> ksubst g e1 <*> return op <*> ksubst g e2
    ksubst g (CompOp l e ops)       = CompOp l <$> ksubst g e <*> ksubst g ops
    ksubst g (UnOp l op e)          = UnOp l op <$> ksubst g e 
    ksubst g (Dot l e n)            = Dot l <$> ksubst g e <*> return n
    ksubst g (DotI l e i t)         = DotI l <$> ksubst g e <*> return i <*> return t
    ksubst g (Lambda l ps ks e)     = Lambda l <$> ksubst g ps <*> ksubst g ks <*> ksubst g e
    ksubst g (Yield l e)            = Yield l <$> ksubst g e
    ksubst g (YieldFrom l e)        = YieldFrom l <$> ksubst g e
    ksubst g (Tuple l es ks)        = Tuple l <$> ksubst g es <*> ksubst g ks
    ksubst g (List l es)            = List l <$> ksubst g es
    ksubst g (ListComp l e c)       = ListComp l <$> ksubst g e <*> ksubst g c
    ksubst g (Dict l as)            = Dict l <$> ksubst g as
    ksubst g (DictComp l a c)       = DictComp l <$> ksubst g a <*> ksubst g c
    ksubst g (Set l es)             = Set l <$> ksubst g es
    ksubst g (SetComp l e c)        = SetComp l <$> ksubst g e <*> ksubst g c
    ksubst g (Paren l e)            = Paren l <$> ksubst g e

instance KSubst Pattern where
    ksubst g (PVar l n a)           = PVar l n <$> ksubst g a
    ksubst g (PTuple l ps ks)       = PTuple l <$> ksubst g ps <*> ksubst g ks
    ksubst g (PList l ps p)         = PList l <$> ksubst g ps <*> ksubst g p
    ksubst g (PParen l p)           = PParen l <$> ksubst g p

instance KSubst Target where
    ksubst g (TaVar l n)            = return $ TaVar l n
    ksubst g (TaTuple l ts)         = TaTuple l <$> ksubst g ts
    ksubst g (TaIndex l e ix)       = TaIndex l <$> ksubst g e <*> ksubst g ix
    ksubst g (TaSlice l e sl)       = TaSlice l <$> ksubst g e <*> ksubst g sl
    ksubst g (TaDot l e n)          = TaDot l <$> ksubst g e <*> return n
    ksubst g (TaDotI l e i tl)      = TaDotI l <$> ksubst g e <*> return i <*> return tl
    ksubst g (TaParen l p)          = TaParen l <$> ksubst g p

instance KSubst Exception where
    ksubst g (Exception e mbe)      = Exception <$> ksubst g e <*> ksubst g mbe

instance KSubst Branch where
    ksubst g (Branch e ss)          = Branch <$> ksubst g e <*> ksubst g ss

instance KSubst Handler where
    ksubst g (Handler ex b)         = Handler ex <$> ksubst g b

instance KSubst PosPar where
    ksubst g (PosPar n t e p)       = PosPar n <$> ksubst g t <*> ksubst g e <*> ksubst g p
    ksubst g (PosSTAR n t)          = PosSTAR n <$> ksubst g t
    ksubst g PosNIL                 = return PosNIL
    
instance KSubst KwdPar where
    ksubst g (KwdPar n t e k)       = KwdPar n <$> ksubst g t <*> ksubst g e <*> ksubst g k
    ksubst g (KwdSTAR n t)          = KwdSTAR n <$> ksubst g t
    ksubst g KwdNIL                 = return KwdNIL
    
instance KSubst PosArg where
    ksubst g (PosArg e p)           = PosArg <$> ksubst g e <*> ksubst g p
    ksubst g (PosStar e)            = PosStar <$> ksubst g e
    ksubst g PosNil                 = return PosNil
    
instance KSubst KwdArg where
    ksubst g (KwdArg n e k)         = KwdArg n <$> ksubst g e <*> ksubst g k
    ksubst g (KwdStar e)            = KwdStar <$> ksubst g e
    ksubst g KwdNil                 = return KwdNil
    
instance KSubst PosPat where
    ksubst g (PosPat p ps)          = PosPat <$> ksubst g p <*> ksubst g ps
    ksubst g (PosPatStar p)         = PosPatStar <$> ksubst g p
    ksubst g PosPatNil              = return PosPatNil
    
instance KSubst KwdPat where
    ksubst g (KwdPat n p ps)        = KwdPat n <$> ksubst g p <*> ksubst g ps
    ksubst g (KwdPatStar p)         = KwdPatStar <$> ksubst g p
    ksubst g KwdPatNil              = return KwdPatNil
    
instance KSubst OpArg where
    ksubst g (OpArg op e)           = OpArg op <$> ksubst g e

instance KSubst Comp where
    ksubst g (CompFor l p e c)      = CompFor l <$> ksubst g p <*> ksubst g e <*> ksubst g c
    ksubst g (CompIf l e c)         = CompIf l <$> ksubst g e <*> ksubst g c
    ksubst g NoComp                 = return NoComp

instance KSubst WithItem where
    ksubst g (WithItem e p)         = WithItem <$> ksubst g e <*> ksubst g p

instance KSubst Elem where
    ksubst g (Elem e)               = Elem <$> ksubst g e
    ksubst g (Star e)               = Star <$> ksubst g e

instance KSubst Assoc where
    ksubst g (Assoc e1 e2)          = Assoc <$> ksubst g e1 <*> ksubst g e2
    ksubst g (StarStar e)           = StarStar <$> ksubst g e
  
instance KSubst Sliz where
    ksubst g (Sliz l e1 e2 e3)      = Sliz l <$> ksubst g e1 <*> ksubst g e2 <*> ksubst g e3


--------------------------------------------------------------------------------------------------------------

data KindError                      = KindError SrcLoc Kind Kind
                                    | InfiniteKind SrcLoc KVar Kind
                                    deriving (Show)

instance Control.Exception.Exception KindError

noUnify l k1 k2                     = Control.Exception.throw $ KindError l k1 k2
infiniteKind l v k                  = Control.Exception.throw $ InfiniteKind l v k

kindError (KindError l k1 k2)       = (l, " Expected a " ++ prstr k2 ++ ", actual kind is " ++ prstr k1)
kindError (InfiniteKind l v k)      = (l, " Infinite kind inferred: " ++ prstr  v ++ " = " ++ prstr k)
