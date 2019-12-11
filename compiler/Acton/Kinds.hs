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


data KEnv                           = KEnv { impenv :: Acton.Env.Env, tcons :: Kinds, tvars :: [TVar] }

type Kinds                          = [(Name,Kind)]

kenv0 ienv                          = KEnv { impenv = ienv, tcons = [], tvars = [] }

extcons ke env                      = env{ tcons = ke ++ tcons env }

extvars vs env                      = env{ tvars = nub vs ++ tvars env }

tconKind (NoQual n) env             = case lookup n (tcons env) of
                                        Just k  -> k
                                        Nothing -> Acton.Env.tconKind (NoQual n) (impenv env)
tconKind qn env                     = Acton.Env.tconKind qn (impenv env)


instance Pretty (Name,Kind) where
    pretty (n,k)                    = pretty n <+> text ":" <+> pretty k

---------------------------------------------------------------------------------------------------------------------

kchkTop env ss                      = do ss' <- instwild ss
                                         kchkSuite env ss'
                                         ksubst ss'
                                         return ss

class KCheck a where
    kchk                            :: KEnv -> a -> KindM ()

instance KCheck a => KCheck [a] where
    kchk env                        = mapM_ (kchk env)

instance KCheck a => KCheck (Maybe a) where
    kchk env Nothing                = return ()
    kchk env (Just a)               = kchk env a

kchkSuite env []                    = return ()
kchkSuite env (Decl _ ds : ss)      = kchk env1 ds >> kchkSuite env1 ss
  where env1                        = extcons (concatMap kinds ds) env
        kinds (Class _ n q _ _)     = [(n,kind q)]
        kinds (Protocol _ n q _ _)  = [(n,kind q)]
        kinds _                     = []
        kind []                     = KType
        kind q                      = KFun [ tvkind v | TBind v _ <- q ] KType
kchkSuite env (s : ss)              = kchk env s >> kchkSuite env ss

instance KCheck Stmt where
    kchk env (Expr l e)             = kchk env e
    kchk env (Assign l ts e)        = kchk env ts >> kchk env e
    kchk env (Update l ts e)        = kchk env ts >> kchk env e
    kchk env (IUpdate l t op e)     = kchk env t >> kchk env e
    kchk env (Assert l e mbe)       = kchk env e >> kchk env mbe
    kchk env (Pass l)               = return ()
    kchk env (Delete l p)           = kchk env p
    kchk env (Return l mbe)         = kchk env mbe
    kchk env (Raise l mbex)         = kchk env mbex
    kchk env (Break l)              = return $ ()
    kchk env (Continue l)           = return $ ()
    kchk env (If l bs els)          = kchk env bs >> kchkSuite env els
    kchk env (While l e b els)      = kchk env e >> kchkSuite env b >> kchkSuite env els
    kchk env (For l p e b els)      = kchk env p >> kchk env e >> kchkSuite env b >> kchkSuite env els
    kchk env (Try l b hs els fin)   = kchkSuite env b >> kchk env hs >> kchkSuite env els >> kchkSuite env fin
    kchk env (With l is b)          = kchk env is >> kchkSuite env b
    kchk env (Data l mbt ss)        = kchk env mbt >> kchkSuite env ss
    kchk env (VarAssign l ps e)     = kchk env ps >> kchk env e
    kchk env (After l e n ps ks)    = kchk env e >> kchk env ps >> kchk env ks
    kchk env (Decl l ds)            = kchk env ds

instance KCheck Decl where
    kchk env (Def l n q p k t b m)  = kchkQual env q >> kchk env1 p >> kchk env1 k >> kchk env1 t >> kchkSuite env1 b
      where env1 | null q           = extvars ((tyfree p ++ tyfree k ++ tyfree t) \\ tvars env) env
                 | otherwise        = extvars (tybound q) env
    kchk env (Actor l n q p k t b)  = kchkQual env q >> kchk env1 p >> kchk env1 k >> kchk env1 t >> kchkSuite env1 b
      where env1 | null q           = extvars ((tyfree p ++ tyfree k ++ tyfree t) \\ tvars env) env
                 | otherwise        = extvars (tybound q) env
    kchk env (Class l n q us b)     = kchkQual env q >> kchk env1 us >> kchkSuite env1 b
      where env1                    = extvars (tybound q) env
    kchk env (Protocol l n q us b)  = kchkQual env q >> kchk env1 us >> kchkSuite env1 b
      where env1                    = extvars (tybound q) env
    kchk env (Extension l n q us b) = kchkQual env q >> kchk env1 us >> kchkSuite env1 b
      where env1                    = extvars (tybound q) env
    kchk env (Signature l ns t)     = kchk env t

instance KCheck Expr where
    kchk env (Var l n)              = return ()
    kchk env (Int l i s)            = return ()
    kchk env (Float l f s)          = return ()
    kchk env (Imaginary l i s)      = return ()
    kchk env (Bool l b)             = return ()
    kchk env (None l)               = return ()
    kchk env (NotImplemented l)     = return ()
    kchk env (Ellipsis l)           = return ()
    kchk env (Strings l ss)         = return ()
    kchk env (BStrings l ss)        = return ()
    kchk env (Call l e ps ks)       = kchk env e >> kchk env ps >> kchk env ks
    kchk env (Index l e is)         = kchk env e >> kchk env is
    kchk env (Slice l e sl)         = kchk env e >> kchk env sl
    kchk env (Cond l e1 e2 e3)      = kchk env e1 >> kchk env e2 >> kchk env e3
    kchk env (BinOp l e1 op e2)     = kchk env e1 >> kchk env e2
    kchk env (CompOp l e ops)       = kchk env e >> kchk env ops
    kchk env (UnOp l op e)          = kchk env e 
    kchk env (Dot l e n)            = kchk env e
    kchk env (DotI l e i t)         = kchk env e
    kchk env (Lambda l ps ks e)     = kchk env ps >> kchk env ks >> kchk env e
    kchk env (Yield l e)            = kchk env e
    kchk env (YieldFrom l e)        = kchk env e
    kchk env (Tuple l es)           = kchk env es
    kchk env (TupleComp l e c)      = kchk env e >> kchk env c
    kchk env (Record l fs)          = kchk env fs
    kchk env (RecordComp l n e c)   = kchk env e >> kchk env c
    kchk env (List l es)            = kchk env es
    kchk env (ListComp l e c)       = kchk env e >> kchk env c
    kchk env (Dict l as)            = kchk env as
    kchk env (DictComp l a c)       = kchk env a >> kchk env c
    kchk env (Set l es)             = kchk env es
    kchk env (SetComp l e c)        = kchk env e >> kchk env c
    kchk env (Paren l e)            = kchk env e

instance KCheck Pattern where
    kchk env (PVar l n t)           = kexpect env KType t
    kchk env (PTuple l ps)          = kchk env ps
    kchk env (PList l ps p)         = kchk env ps >> kchk env p
    kchk env (PParen l p)           = kchk env p

instance KCheck Target where
    kchk env (TaVar l n)            = return ()
    kchk env (TaTuple l ps)         = kchk env ps
    kchk env (TIndex l e ix)        = kchk env e >> kchk env ix
    kchk env (TSlice l e sl)        = kchk env e >> kchk env sl
    kchk env (TDot l e n)           = kchk env e
    kchk env (TParen l p)           = kchk env p

instance KCheck Exception where
    kchk env (Exception e mbe)      = kchk env e >> kchk env mbe

instance KCheck Branch where
    kchk env (Branch e ss)          = kchk env e >> kchkSuite env ss

instance KCheck Handler where
    kchk env (Handler ex b)         = kchk env ex >> kchkSuite env b

instance KCheck Except where
    kchk env (ExceptAll _)          = return ()
    kchk env (Except _ x)           = case tconKind x env of
                                        KType -> return ()
                                        KFun _ KType -> return ()
    kchk env (ExceptAs l x n)       = kchk env (Except l x)

instance KCheck PosPar where
    kchk env (PosPar n t e p)       = kchk env t >> kchk env e >> kchk env p
    kchk env (PosSTAR n t)          = kexpect env KRow t
    kchk env PosNIL                 = return ()
    
instance KCheck KwdPar where
    kchk env (KwdPar n t e k)       = kchk env t >> kchk env e >> kchk env k
    kchk env (KwdSTAR n t)          = kexpect env KRow t
    kchk env KwdNIL                 = return ()
    
instance KCheck PosArg where
    kchk env (PosArg e p)           = kchk env e >> kchk env p
    kchk env (PosStar e)            = kchk env e
    kchk env PosNil                 = return ()
    
instance KCheck KwdArg where
    kchk env (KwdArg n e k)         = kchk env e >> kchk env k
    kchk env (KwdStar e)            = kchk env e
    kchk env KwdNil                 = return ()
    
instance KCheck PosPat where
    kchk env (PosPat p ps)          = kchk env p >> kchk env ps
    kchk env (PosPatStar p)         = kchk env p
    kchk env PosPatNil              = return ()
    
instance KCheck KwdPat where
    kchk env (KwdPat n p ps)        = kchk env p >> kchk env ps
    kchk env (KwdPatStar p)         = kchk env p
    kchk env KwdPatNil              = return ()
    
instance KCheck OpArg where
    kchk env (OpArg op e)           = kchk env e

instance KCheck Comp where
    kchk env (CompFor l p e c)      = kchk env p >> kchk env e >> kchk env c
    kchk env (CompIf l e c)         = kchk env e >> kchk env c
    kchk env NoComp                 = return ()

instance KCheck WithItem where
    kchk env (WithItem e p)         = kchk env e >> kchk env p

instance KCheck Elem where
    kchk env (Elem e)               = kchk env e
    kchk env (Star e)               = kchk env e

instance KCheck Assoc where
    kchk env (Assoc e1 e2)          = kchk env e1 >> kchk env e2
    kchk env (StarStar e)           = kchk env e
  
instance KCheck Slice where
    kchk env (Sliz l e1 e2 e3)      = kchk env e1 >> kchk env e2 >> kchk env e3

instance KCheck TSchema where
    kchk env (TSchema l q t d)      = kchkQual env q >> kexpect env1 KType t
      where env1 | null q           = extvars (tyfree t \\ tvars env) env
                 | otherwise        = extvars (tybound q) env

kchkQual env []                     = return ()
kchkQual env (TBind v us : q)
  | v `elem` tvars env              = Acton.Env.err1 v "Type variable already in scope:"
  | otherwise                       = mapM_ (kexpect env KType) us >> kchkQual (extvars [v] env) q

instance KCheck TCon where
    kchk env c                      = kexpect env KType c

instance KCheck Type where
    kchk env t                      = kexpect env KType t



class KInfer t where
    kinfer                          :: KEnv -> t -> KindM Kind

instance (KInfer t) => KInfer (Maybe t) where
    kinfer env Nothing              = newKVar
    kinfer env (Just t)             = kinfer env t

instance KInfer TVar where
    kinfer env v
      | not $ skolem v              = return (tvkind v)
      | v `elem` tvars env          = return (tvkind v)
      | otherwise                   = Acton.Env.err1 v "Unbound type variable:"

instance KInfer TCon where
    kinfer env (TC n [])            = return $ tconKind n env
    kinfer env (TC n ts)            = do let kn = tconKind n env
                                         ks <- mapM (kinfer env) ts
                                         k <- newKVar
                                         kunify kn (KFun ks k)
                                         return k

--instance KInfer TSchema where
--    kinfer env sc                   = do kexpect env KType sc           -- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
--                                         return KType

instance KInfer TBind where
    kinfer env (TBind tv us)        = do mapM_ (kexpect env KType) us
                                         kinfer env tv

instance KInfer Type where
    kinfer env (TVar _ v)           = kinfer env v
    kinfer env (TCon _ c)           = do kexpect env KType c
                                         return KType
    kinfer env (TAt _ c)            = do kexpect env KType c
                                         return KType
    kinfer env (TFun _ fx p k t)    = do kexpect env KRow fx
                                         kexpect env KRow p
                                         kexpect env KRow k
                                         kexpect env KType t
                                         return KType
    kinfer env (TTuple _ p)         = do kexpect env KRow p
                                         return KType
    kinfer env (TRecord _ k)        = do kexpect env KRow k
                                         return KType
    kinfer env (TUnion _ as)        = return KType
    kinfer env (TOpt _ t)           = do kexpect env KType t
                                         return KType
    kinfer env (TNone _)            = return KType
    kinfer env (TNil _)             = return KRow
    kinfer env (TRow _ n t r)       = do kchk env t
                                         kexpect env KRow r
                                         return KRow

kexpect env k t                     = do k' <- kinfer env t
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


-- Instantiate wildcards / apply kind substitution -------------------------------------------------------------------------

instwild x                          = kwalk True x

ksubst x                            = kwalk False x

class KWalk s where
    kwalk                           :: Bool -> s -> KindM s

instance KWalk a => KWalk [a] where
    kwalk w                         = mapM (kwalk w)

instance KWalk a => KWalk (Maybe a) where
    kwalk w                         = maybe (return Nothing) (\x -> Just <$> kwalk w x)

instance KWalk Kind where
    kwalk w KWild                   = newKVar
    kwalk w (KVar v)                = do s <- getSubstitution
                                         case Map.lookup v s of
                                            Just k  -> kwalk w k
                                            Nothing -> return (KVar v)
    kwalk w (KFun ks k)             = KFun <$> mapM (kwalk w) ks <*> kwalk w k
    kwalk w k                       = return k
        
instance KWalk TSchema where
    kwalk w (TSchema l q t dec)     = TSchema l <$> kwalk False q <*> kwalk w t <*> return dec

instance KWalk TVar where
    kwalk w (TV k n)                = TV <$> kwalk w k <*> return n
    
instance KWalk TCon where
    kwalk w (TC n ts)               = TC n <$> kwalk w ts

instance KWalk TBind where
    kwalk w (TBind v cs)            = TBind <$> kwalk w v <*> kwalk w cs

instance KWalk Type where
    kwalk True (TWild l)            = do k <- newKVar
                                         TVar NoLoc <$> TV k <$> newName "W"
    kwalk False (TWild l)           = Acton.Env.err1 l "Illegal wildcard type"
    kwalk w (TVar l v)              = TVar l <$> kwalk w v
    kwalk w (TCon l c)              = TCon l <$> kwalk w c
    kwalk w (TAt l c)               = TAt l <$> kwalk w c
    kwalk w (TFun l fx p k t)       = TFun l <$> kwalk w fx <*> kwalk w p <*> kwalk w k<*> kwalk w t
    kwalk w (TTuple l p)            = TTuple l <$> kwalk w p
    kwalk w (TRecord l k)           = TRecord l <$> kwalk w k
    kwalk w (TUnion l as)           = return $ TUnion l as
    kwalk w (TOpt l t)              = TOpt l <$> kwalk w t
    kwalk w (TNone l)               = return $ TNone l
    kwalk w (TNil l)                = return $ TNil l
    kwalk w (TRow l n t r)          = TRow l n <$> kwalk w t <*> kwalk w r

instance KWalk Stmt where
    kwalk w (Expr l e)              = Expr l <$> kwalk w e
    kwalk w (Assign l ts e)         = Assign l <$> kwalk w ts <*> kwalk w e
    kwalk w (Update l ts e)         = Update l <$> kwalk w ts <*> kwalk w e
    kwalk w (IUpdate l t op e)      = IUpdate l <$> kwalk w t <*> return op <*> kwalk w e
    kwalk w (Assert l e mbe)        = Assert l <$> kwalk w e <*> kwalk w mbe
    kwalk w (Pass l)                = return $ Pass l
    kwalk w (Delete l p)            = Delete l <$> kwalk w p
    kwalk w (Return l mbe)          = Return l <$> kwalk w mbe
    kwalk w (Raise l mbex)          = Raise l <$> kwalk w mbex
    kwalk w (Break l)               = return $ Break l
    kwalk w (Continue l)            = return $ Continue l
    kwalk w (If l bs els)           = If l <$> kwalk w bs <*> kwalk w els
    kwalk w (While l e b els)       = While l <$> kwalk w e <*> kwalk w b <*> kwalk w els
    kwalk w (For l p e b els)       = For l <$> kwalk w p <*> kwalk w e <*> kwalk w b <*> kwalk w els
    kwalk w (Try l b hs els fin)    = Try l <$> kwalk w b <*> kwalk w hs <*> kwalk w els <*> kwalk w fin
    kwalk w (With l is b)           = With l <$> kwalk w is <*> kwalk w b
    kwalk w (Data l mbt ss)         = Data l <$> kwalk w mbt <*> kwalk w ss
    kwalk w (VarAssign l ps e)      = VarAssign l <$> kwalk w ps <*> kwalk w e
    kwalk w (After l e n ps ks)     = After l <$> kwalk w e <*> return n <*> kwalk w ps <*> kwalk w ks
    kwalk w (Decl l ds)             = Decl l <$> kwalk w ds

instance KWalk Decl where
    kwalk w (Def l n q p k ann b m) = Def l n <$> kwalk False q <*> kwalk w p <*> kwalk w k <*> kwalk w ann <*> kwalk w b <*> return m
    kwalk w (Actor l n q p k ann b) = Actor l n <$> kwalk False q <*> kwalk w p <*> kwalk w k <*> kwalk w ann <*> kwalk w b
    kwalk w (Class l n q as b)      = Class l n <$> kwalk False q <*> kwalk False as <*> kwalk w b
    kwalk w (Protocol l n q as b)   = Protocol l n <$> kwalk False q <*> kwalk False as <*> kwalk w b
    kwalk w (Extension l n q as b)  = Extension l n <$> kwalk False q <*> kwalk False as <*> kwalk w b
    kwalk w (Signature l ns t)      = Signature l ns <$> kwalk w t

instance KWalk Expr where
    kwalk w (Var l n)               = return $ Var l n
    kwalk w (Int l i s)             = return $ Int l i s
    kwalk w (Float l f s)           = return $ Float l f s
    kwalk w (Imaginary l i s)       = return $ Imaginary l i s
    kwalk w (Bool l b)              = return $ Bool l b
    kwalk w (None l)                = return $ None l
    kwalk w (NotImplemented l)      = return $ NotImplemented l
    kwalk w (Ellipsis l)            = return $ Ellipsis l
    kwalk w (Strings l ss)          = return $ Strings l ss
    kwalk w (BStrings l ss)         = return $ BStrings l ss
    kwalk w (Call l e ps ks)        = Call l <$> kwalk w e <*> kwalk w ps <*> kwalk w ks
    kwalk w (Index l e is)          = Index l <$> kwalk w e <*> kwalk w is
    kwalk w (Slice l e sl)          = Slice l <$> kwalk w e <*> kwalk w sl
    kwalk w (Cond l e1 e2 e3)       = Cond l <$> kwalk w e1 <*> kwalk w e2 <*> kwalk w e3
    kwalk w (BinOp l e1 op e2)      = BinOp l <$> kwalk w e1 <*> return op <*> kwalk w e2
    kwalk w (CompOp l e ops)        = CompOp l <$> kwalk w e <*> kwalk w ops
    kwalk w (UnOp l op e)           = UnOp l op <$> kwalk w e 
    kwalk w (Dot l e n)             = Dot l <$> kwalk w e <*> return n
    kwalk w (DotI l e i t)          = DotI l <$> kwalk w e <*> return i <*> return t
    kwalk w (Lambda l ps ks e)      = Lambda l <$> kwalk w ps <*> kwalk w ks <*> kwalk w e
    kwalk w (Yield l e)             = Yield l <$> kwalk w e
    kwalk w (YieldFrom l e)         = YieldFrom l <$> kwalk w e
    kwalk w (Tuple l es)            = Tuple l <$> kwalk w es
    kwalk w (TupleComp l e c)       = TupleComp l <$> kwalk w e <*> kwalk w c
    kwalk w (Record l fs)           = Record l <$> kwalk w fs
    kwalk w (RecordComp l n e c)    = RecordComp l n <$> kwalk w e <*> kwalk w c
    kwalk w (List l es)             = List l <$> kwalk w es
    kwalk w (ListComp l e c)        = ListComp l <$> kwalk w e <*> kwalk w c
    kwalk w (Dict l as)             = Dict l <$> kwalk w as
    kwalk w (DictComp l a c)        = DictComp l <$> kwalk w a <*> kwalk w c
    kwalk w (Set l es)              = Set l <$> kwalk w es
    kwalk w (SetComp l e c)         = SetComp l <$> kwalk w e <*> kwalk w c
    kwalk w (Paren l e)             = Paren l <$> kwalk w e

instance KWalk Pattern where
    kwalk w (PVar l n a)            = PVar l n <$> kwalk w a
--    kwalk w (PRecord l ps)          = PRecord l <$> kwalk w ps
    kwalk w (PTuple l ps)           = PTuple l <$> kwalk w ps
    kwalk w (PList l ps p)          = PList l <$> kwalk w ps <*> kwalk w p
    kwalk w (PParen l p)            = PParen l <$> kwalk w p

instance KWalk Target where
    kwalk w (TaVar l n)             = return $ TaVar l n
    kwalk w (TaTuple l ts)          = TaTuple l <$> kwalk w ts
    kwalk w (TIndex l e ix)         = TIndex l <$> kwalk w e <*> kwalk w ix
    kwalk w (TSlice l e sl)         = TSlice l <$> kwalk w e <*> kwalk w sl
    kwalk w (TDot l e n)            = TDot l <$> kwalk w e <*> return n
    kwalk w (TParen l p)            = TParen l <$> kwalk w p

instance KWalk Exception where
    kwalk w (Exception e mbe)       = Exception <$> kwalk w e <*> kwalk w mbe

instance KWalk Branch where
    kwalk w (Branch e ss)           = Branch <$> kwalk w e <*> kwalk w ss

instance KWalk Handler where
    kwalk w (Handler ex b)          = Handler ex <$> kwalk w b

instance KWalk PosPar where
    kwalk w (PosPar n t e p)        = PosPar n <$> kwalk w t <*> kwalk w e <*> kwalk w p
    kwalk w (PosSTAR n t)           = PosSTAR n <$> kwalk w t
    kwalk w PosNIL                  = return PosNIL
    
instance KWalk KwdPar where
    kwalk w (KwdPar n t e k)        = KwdPar n <$> kwalk w t <*> kwalk w e <*> kwalk w k
    kwalk w (KwdSTAR n t)           = KwdSTAR n <$> kwalk w t
    kwalk w KwdNIL                  = return KwdNIL
    
instance KWalk PosArg where
    kwalk w (PosArg e p)            = PosArg <$> kwalk w e <*> kwalk w p
    kwalk w (PosStar e)             = PosStar <$> kwalk w e
    kwalk w PosNil                  = return PosNil
    
instance KWalk KwdArg where
    kwalk w (KwdArg n e k)          = KwdArg n <$> kwalk w e <*> kwalk w k
    kwalk w (KwdStar e)             = KwdStar <$> kwalk w e
    kwalk w KwdNil                  = return KwdNil
    
instance KWalk PosPat where
    kwalk w (PosPat p ps)           = PosPat <$> kwalk w p <*> kwalk w ps
    kwalk w (PosPatStar p)          = PosPatStar <$> kwalk w p
    kwalk w PosPatNil               = return PosPatNil
    
instance KWalk KwdPat where
    kwalk w (KwdPat n p ps)         = KwdPat n <$> kwalk w p <*> kwalk w ps
    kwalk w (KwdPatStar p)          = KwdPatStar <$> kwalk w p
    kwalk w KwdPatNil               = return KwdPatNil
    
instance KWalk OpArg where
    kwalk w (OpArg op e)            = OpArg op <$> kwalk w e

instance KWalk Comp where
    kwalk w (CompFor l p e c)       = CompFor l <$> kwalk w p <*> kwalk w e <*> kwalk w c
    kwalk w (CompIf l e c)          = CompIf l <$> kwalk w e <*> kwalk w c
    kwalk w NoComp                  = return NoComp

instance KWalk WithItem where
    kwalk w (WithItem e p)          = WithItem <$> kwalk w e <*> kwalk w p

instance KWalk Elem where
    kwalk w (Elem e)                = Elem <$> kwalk w e
    kwalk w (Star e)                = Star <$> kwalk w e

instance KWalk Assoc where
    kwalk w (Assoc e1 e2)           = Assoc <$> kwalk w e1 <*> kwalk w e2
    kwalk w (StarStar e)            = StarStar <$> kwalk w e
  
instance KWalk Slice where
    kwalk w (Sliz l e1 e2 e3)       = Sliz l <$> kwalk w e1 <*> kwalk w e2 <*> kwalk w e3


--------------------------------------------------------------------------------------------------------------

data KindError                      = KindError SrcLoc Kind Kind
                                    | InfiniteKind SrcLoc KVar Kind
                                    deriving (Show)

instance Control.Exception.Exception KindError

noUnify k1 k2                       = Control.Exception.throw $ KindError NoLoc k1 k2
infiniteKind v k                    = Control.Exception.throw $ InfiniteKind NoLoc v k

kindError (KindError l k1 k2)       = (l, " Incompatible kinds: " ++ prstr k1 ++ " and " ++ prstr k2)
kindError (InfiniteKind l v k)      = (l, " Infinite kind inferred: " ++ prstr  v ++ " = " ++ prstr k)
