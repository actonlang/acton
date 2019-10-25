{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Acton.TypeM where

import Debug.Trace
import Data.Maybe (maybeToList, fromJust)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Control.Monad.State.Strict
import Control.Monad.Except
import qualified Control.Exception
import Data.Typeable
import Pretty
import Utils
import Acton.Syntax
import Acton.Builtin
import Acton.Printer
import Acton.Names



data Constraint                         = Equ       Type Type
                                        | Sub       Type Type
                                        | Impl      Type TCon
                                        | Sel       Type Name Type
                                        -- ...
                                        deriving (Eq,Show)

instance Pretty Constraint where
    pretty (Equ t1 t2)                  = pretty t1 <+> text "  =  " <+> pretty t2
    pretty (Sub t1 t2)                  = pretty t1 <+> text "  <  " <+> pretty t2
    pretty (Impl t u)                   = pretty t <+> text "  impl  " <+> pretty u
    pretty (Sel t1 n t2)                = pretty t1 <+> text "  ." <> pretty n <> text "  " <+> pretty t2
    
type Constraints                        = [Constraint]

data TypeErr                            = TypeErr
                                        -- ...
                                        deriving (Eq,Show,Typeable)

type TVarMap                            = Map TVar Type

data TypeState                          = TypeState {
                                                nextint         :: Int,
                                                constraints     :: Constraints,
                                                effectstack     :: [FXRow],
                                                deferred        :: Constraints,
                                                currsubst       :: TVarMap,
                                                dumped          :: SrcInfo
                                          }

initTypeState s                         = TypeState { nextint = 1, constraints = [], effectstack = [], deferred = [], currsubst = s, dumped = [] }

type TypeM a                            = ExceptT TypeErr (State TypeState) a

runTypeM                                :: TypeM a -> a
runTypeM m                              = case evalState (runExceptT m) (initTypeState Map.empty) of
                                            Right x -> x
                                            Left err -> error ("(internal) Unhandled TypeM error: " ++ show err)

newUnique                               :: TypeM Int
newUnique                               = lift $ state $ \st -> (nextint st, st{ nextint = nextint st + 1 })

constrain                               :: Constraints -> TypeM ()
constrain cs                            = lift $ state $ \st -> ((), st{ constraints = cs ++ constraints st })

collectConstraints                      :: TypeM Constraints
collectConstraints                      = lift $ state $ \st -> (constraints st, st{ constraints = [] })

pushFX                                  :: FXRow -> TypeM ()
pushFX fx                               = lift $ state $ \st -> ((), st{ effectstack = fx : effectstack st })

currFX                                  :: TypeM FXRow
currFX                                  = lift $ state $ \st -> (head (effectstack st), st)

equFX                                   :: FXRow -> TypeM ()
equFX fx                                = do fx0 <- currFX
                                             constrain [Equ fx fx0]

subFX                                   :: FXRow -> TypeM ()
subFX fx                                = do fx0 <- currFX
                                             constrain [Sub fx fx0]

popFX                                   :: TypeM ()
popFX                                   = lift $ state $ \st -> ((), st{ effectstack = tail (effectstack st) })

defer                                   :: Constraints -> TypeM ()
defer cs                                = lift $ state $ \st -> ((), st{ deferred = cs ++ deferred st })

collectDeferred                         :: TypeM Constraints
collectDeferred                         = lift $ state $ \st -> (deferred st, st)

substitute                              :: TVar -> Type -> TypeM ()
substitute tv@(TV (Internal _ _)) t     = lift $ state $ \st -> ((), st{ currsubst = Map.insert tv t (currsubst st)})

getSubstitution                         :: TypeM (Map TVar Type)
getSubstitution                         = lift $ state $ \st -> (currsubst st, st)

dump                                    :: SrcInfo -> TypeM ()
dump inf                                = lift $ state $ \st -> ((), st{ dumped = inf ++ dumped st })

getDump                                 :: TypeM SrcInfo
getDump                                 = lift $ state $ \st -> (dumped st, st)


newTVar                                 = TVar NoLoc <$> TV <$> Internal "V" <$> newUnique

newTVars n                              = mapM (const newTVar) [1..n]

subst                                   :: Subst a => Substitution -> a -> a
subst s x                               = case evalState (runExceptT (msubst x)) (initTypeState $ Map.fromList s) of
                                            Right y  -> y

class Subst t where
    msubst                          :: t -> TypeM t
    tyfree                          :: t -> [TVar]
    tybound                         :: t -> [TVar]
    tybound _                       = []

instance Subst a => Subst (Name,a) where
    msubst (n, t)                   = (,) <$> return n <*> msubst t
    tyfree (n, t)                   = tyfree t
    tybound (n, t)                  = tybound t

instance Subst a => Subst [a] where
    msubst                          = mapM msubst
    tyfree                          = concat . map tyfree
    tybound                         = concat . map tybound

instance Subst a => Subst (Maybe a) where
    msubst                          = maybe (return Nothing) (\x -> Just <$> msubst x)
    tyfree                          = maybe [] tyfree
    tybound                         = maybe [] tybound

instance Subst Constraint where
    msubst (Equ t1 t2)              = Equ <$> msubst t1 <*> msubst t2
    msubst (Sub t1 t2)              = Sub <$> msubst t1 <*> msubst t1
    msubst (Impl t c)               = Impl <$> msubst t <*> msubst c
    msubst (Sel t1 n t2)            = Sel <$> msubst t1 <*> return n <*> msubst t2
    tyfree (Equ t1 t2)              = tyfree t1 ++ tyfree t2
    tyfree (Sub t1 t2)              = tyfree t1 ++ tyfree t2
    tyfree (Impl t c)               = tyfree t ++ tyfree c
    tyfree (Sel t1 n t2)            = tyfree t1 ++ tyfree t2

instance Subst TSchema where
    msubst sc@(TSchema l q t)       = (msubst' . Map.toList . Map.filterWithKey relevant) <$> getSubstitution
      where relevant k v            = k `elem` vs0
            vs0                     = tyfree sc
            msubst' s               = TSchema l (subst s q') (subst s t')
              where vs              = tybound q
                    newvars         = tyfree (rng s)
                    clashvars       = vs `intersect` newvars
                    avoidvars       = vs0 ++ vs ++ newvars
                    freshvars       = tvarSupply \\ avoidvars
                    renaming_s      = clashvars `zip` map (TVar NoLoc) freshvars
                    q'              = [ TBind (subst renaming_s v) (subst renaming_s cs) | TBind v cs <- q ]
                    t'              = subst renaming_s t

    tyfree (TSchema _ q t)          = (tyfree q ++ tyfree t) \\ tybound q
    tybound (TSchema _ q t)         = tybound q

testSchemaSubst = do
    putStrLn ("t:  " ++ render (pretty t))
    putStrLn ("s1: " ++ render (pretty s1))
    putStrLn ("s2: " ++ render (pretty s2))
    putStrLn ("s3: " ++ render (pretty s3))
    putStrLn ("subst s1 t: " ++ render (pretty (subst s1 t)))
    putStrLn ("subst s2 t: " ++ render (pretty (subst s2 t)))
    putStrLn ("subst s3 t: " ++ render (pretty (subst s3 t)))
  where t   = TSchema NoLoc [TBind (TV (name "A")) [TC (noQual "Eq") []]] 
                            (tCon (TC (noQual "apa") [tVar (TV (name "A")), 
                                                      tVar (TV (name "B"))]))
        s1  = [(TV (name "B"), tSelf)]
        s2  = [(TV (name "A"), tSelf)]
        s3  = [(TV (name "B"), tVar (TV (name "A")))]

instance Subst TVar where
    msubst v                        = do t <- msubst (TVar NoLoc v)
                                         case t of
                                            TVar _ v' -> return v'
                                            _         -> return v
    tyfree v                        = [v]
        
instance Subst TCon where
    msubst (TC n ts)                = TC n <$> msubst ts
    tyfree (TC n ts)                = tyfree ts

instance Subst TBind where
    msubst (TBind v cs)             = TBind <$> msubst v <*> msubst cs
    tyfree (TBind v cs)             = tyfree cs
    tybound (TBind v cs)            = [v]

instance Subst Type where
    msubst (TVar l v)               = do s <- getSubstitution
                                         case Map.lookup v s of
                                            Just t  -> msubst t
                                            Nothing -> return (TVar l v)
    msubst (TCon l c)               = TCon l <$> msubst c
    msubst (TAt l c)                = TAt l <$> msubst c
    msubst (TFun l fx p k t)        = TFun l <$> msubst fx <*> msubst p <*> msubst k<*> msubst t
    msubst (TTuple l p)             = TTuple l <$> msubst p
    msubst (TRecord l k)            = TRecord l <$> msubst k
    msubst (TOpt l t)               = TOpt l <$> msubst t
    msubst (TUnion l as)            = return $ TUnion l as
    msubst (TNone l)                = return $ TNone l
    msubst (TSelf l)                = return $ TSelf l
    msubst (TWild l)                = return $ TWild l
    msubst (TNil l)                 = return $ TNil l
    msubst (TRow l n t r)           = TRow l n <$> msubst t <*> msubst r

    tyfree (TVar _ v)               = [v]
    tyfree (TCon _ c)               = tyfree c
    tyfree (TAt _ c)                = tyfree c
    tyfree (TFun _ fx p k t)        = tyfree p ++ tyfree k ++ tyfree t
    tyfree (TTuple _ p)             = tyfree p
    tyfree (TRecord _ k)            = tyfree k
    tyfree (TOpt _ t)               = tyfree t
    tyfree (TUnion _ as)            = []
    tyfree (TNone _)                = []
    tyfree (TSelf _)                = []
    tyfree (TWild _)                = []
    tyfree (TNil _)                 = []
    tyfree (TRow _ _ t r)           = tyfree t ++ tyfree r


----------------------------------------

type OVarMap                            = Map OVar OType

type Deferred                           = [Qonstraint]

type EffectStack                        = [OType]

type Unique                             = Int

type SolveState                         = (Unique, [Qonstraint], EffectStack, Deferred, SrcInfo, OVarMap)

type OTypeM a                           = ExceptT OSolveErr (State SolveState) a

o_runTypeM                              :: OTypeM a -> a
o_runTypeM m                            = case evalState (runExceptT m) (1,[],[],[],[],Map.empty) of
                                            Right x  -> x
                                            Left err -> error "Unhandled o_constraint-solver exception"

o_unique                                :: OTypeM Int
o_unique                                = lift $ state $ \(u,c,f,d,i,s) -> (u, (u+1,c,f,d,i,s))

o_constrain cs                          = lift $ state $ \(u,c,f,d,i,s) -> ((), (u,cs++c,f,d,i,s))

o_pushFX t                              = lift $ state $ \(u,c,f,d,i,s) -> ((), (u,c,t:f,d,i,s))

o_currentFX                             :: OTypeM OType
o_currentFX                             = lift $ state $ \(u,c,t:f,d,i,s) -> (t, (u,c,t:f,d,i,s))

o_effect l fx                           = do fx0 <- o_currentFX
                                             o_constrain [QEqu l 36 fx fx0]

o_defer cs                              = lift $ state $ \(u,c,f,d,i,s) -> ((), (u,c,f,cs:d,i,s))

o_substitute tv t                       = lift $ state $ \(u,c,f,d,i,s) -> ((), (u,c,f,d,i,Map.insert tv t s))

o_dump info1                            = lift $ state $ \(u,c,f,d,i,s) -> ((), (u,c,f,d,reverse info1 ++ i,s))

o_constraints                           :: OTypeM [Qonstraint]
o_constraints                           = (lift $ state $ \(u,c,f,d,i,s) -> (c, (u,[],f,d,i,s))) >>= mapsubst

o_popFX                                 :: OTypeM ()
o_popFX                                 = (lift $ state $ \(u,c,t:f,d,i,s) -> ((), (u,c,f,d,i,s)))

o_deferred                              :: OTypeM Deferred
o_deferred                              = lift $ state $ \(u,c,f,d,i,s) -> (d, (u,c,f,[],i,s))

o_substitution                          = lift $ state $ \(u,c,f,d,i,s) -> (s, (u,c,f,d,i,s))

o_resetsubst False                      = return ()
o_resetsubst True                       = do (u,c,f,d,i,s) <- lift get
                                             c <- mapsubst c
                                             f <- mapsubst f
--                                             i <- mapsubst i
                                             lift $ put (u,c,f,d,i,Map.empty)

o_realsubst                             = do s <- o_substitution
                                             mapsubst (Map.toList s)


oSubst                                  :: MapSubst a => OSubstitution -> a -> a
oSubst s x                              = case evalState (runExceptT (mapsubst x)) (1,[],[],[],[],Map.fromList s) of
                                            Right y  -> y


class MapSubst a where
    mapsubst                            :: a -> OTypeM a
    oTyvars                             :: a -> [OVar]

instance MapSubst a => MapSubst [a] where
    mapsubst                            = mapM mapsubst
    oTyvars                             = concat . map oTyvars

instance MapSubst a => MapSubst (Maybe a) where
    mapsubst                            = mapM mapsubst
    oTyvars                             = maybe [] oTyvars

instance MapSubst a => MapSubst (Name,a) where
    mapsubst (v, t)                     = mapsubst t >>= \t' -> return (v,t')
    oTyvars (n, t)                      = oTyvars t
    
instance MapSubst (OVar,OType) where
    mapsubst (tv, t)                    = mapsubst t >>= \t' -> return (tv,t')
    oTyvars (tv, t)                     = oTyvars t

instance MapSubst OType where
    mapsubst (OVar l)                   = do s <- o_substitution
                                             case Map.lookup l s of
                                                 Just t  -> mapsubst t
                                                 Nothing -> return (OVar l)
    mapsubst (OFun act row t)           = OFun <$> mapsubst act <*> mapsubst row <*> mapsubst t
    mapsubst (ORecord row)              = ORecord <$> mapsubst row
    mapsubst (ODict t1 t2)              = ODict <$> mapsubst t1 <*> mapsubst t2
    mapsubst (OTuple pos)               = OTuple <$> mapsubst pos
    mapsubst (OList t)                  = OList <$> mapsubst t
    mapsubst (OSet t)                   = OSet <$> mapsubst t
    mapsubst (OPos t r)                 = OPos <$> mapsubst t <*> mapsubst r
    mapsubst (OStar1 t r)               = OStar1 <$> mapsubst t <*> mapsubst r
    mapsubst (OKwd n t r)               = OKwd n <$> mapsubst t <*> mapsubst r
    mapsubst (OStar2 t r)               = OStar2 <$> mapsubst t <*> mapsubst r
    mapsubst (OSchema vs cs t)          = OSchema vs <$> mapsubst cs <*> mapsubst t     -- vs are always disjoint from dom(subst)
    mapsubst t                          = return t

    oTyvars (OVar v)                    = [v]
    oTyvars (OFun act row t)            = oTyvars act ++ oTyvars row ++ oTyvars t
    oTyvars (ORecord row)               = oTyvars row
    oTyvars (ODict t1 t2)               = oTyvars t1 ++ oTyvars t2
    oTyvars (OTuple row)                = oTyvars row
    oTyvars (OList t)                   = oTyvars t
    oTyvars (OSet t)                    = oTyvars t
    oTyvars (OMsg t)                    = oTyvars t
    oTyvars (OPos t r)                  = oTyvars t ++ oTyvars r
    oTyvars (OStar1 t r)                = oTyvars t ++ oTyvars r
    oTyvars (OKwd n t r)                = oTyvars t ++ oTyvars r
    oTyvars (OStar2 t r)                = oTyvars t ++ oTyvars r
    oTyvars (OSchema vs cs t)           = (oTyvars t ++ oTyvars cs) \\ vs
    oTyvars tcon                        = []

instance MapSubst Qonstraint where
    mapsubst (QEqu l v t1 t2)           = QEqu l v <$> mapsubst t1 <*> mapsubst t2
    mapsubst (QIn l v t1 t2)            = QIn l v <$> mapsubst t1 <*> mapsubst t2
    mapsubst (QDot l v t1 n t2)         = QDot l v <$> mapsubst t1 <*> return n <*> mapsubst t2
    mapsubst (QIx l v t1 t2 t3)         = QIx l v <$> mapsubst t1 <*> mapsubst t2 <*> mapsubst t3
    mapsubst (QMod l v t1 t2)           = QMod l v <$> mapsubst t1 <*> mapsubst t2
    mapsubst (QPlus l v t1)             = QPlus l v <$> mapsubst t1
    mapsubst (QNum l v t1)              = QNum l v <$> mapsubst t1
    mapsubst (QBool l v t1)             = QBool l v <$> mapsubst t1

    oTyvars (QEqu _ _ t1 t2)            = oTyvars t1 ++ oTyvars t2
    oTyvars (QIn _ _ t1 t2)             = oTyvars t1 ++ oTyvars t2
    oTyvars (QDot _ _ t1 n t2)          = oTyvars t1 ++ oTyvars t2
    oTyvars (QIx _ _ t1 t2 t3)          = oTyvars t1 ++ oTyvars t2 ++ oTyvars t3
    oTyvars (QMod _ _ t1 t2)            = oTyvars t1 ++ oTyvars t2
    oTyvars (QPlus _ _ t1)              = oTyvars t1
    oTyvars (QNum _ _ t1)               = oTyvars t1
    oTyvars (QBool _ _ t1)              = oTyvars t1

data OSolveErr                          = Defer
                                        | EmptyRow
                                        | KwdNotFound Name
                                        | ConflictingRow OVar
                                        | InfiniteType OVar
                                        | NoUnify OType OType
                                        | NoSelect Name
                                        | NoOverload Qonstraint
                                        | NoSolve Qonstraint OSolveErr
                                        | NotYet SrcLoc Doc
                                        deriving (Eq,Show,Typeable)

notYetExpr e                            = notYet (loc e) (pretty e)

notYet loc doc                          = Control.Exception.throw $ NotYet loc doc

instance MapSubst OSolveErr where
    mapsubst (ConflictingRow tv)        = do t <- mapsubst (OVar tv)
                                             case t of
                                                 OVar tv' -> return (ConflictingRow tv')
                                                 _        -> return (ConflictingRow tv)
    mapsubst (InfiniteType tv)          = do t <- mapsubst (OVar tv)
                                             case t of
                                                 OVar tv' -> return (InfiniteType tv')
                                                 _        -> return (InfiniteType tv)
    mapsubst (NoUnify t1 t2)            = NoUnify <$> mapsubst t1 <*> mapsubst t2
    mapsubst (NoSolve c err)            = NoSolve <$> mapsubst c <*> mapsubst err
    mapsubst err                        = return err

    oTyvars (ConflictingRow tv)         = [tv]
    oTyvars (InfiniteType tv)           = [tv]
    oTyvars (NoUnify t1 t2)             = (oTyvars t1 ++ oTyvars t2) \\ [[]]
    oTyvars (NoSolve c err)             = (oTyvars c ++ oTyvars err) \\ [[]]
    oTyvars _                           = []

instance Control.Exception.Exception OSolveErr

solveError err                          = (loc err,render (expl err))
  where expl EmptyRow                   = text "Positional elements are missing"
        expl (KwdNotFound n)            = text "Keyword element" <+> quotes (pretty n) <+> text "is not found"
        expl (ConflictingRow tv)        = text "Row" <+> pretty tv <+> text "has conflicting extensions"
        expl (InfiniteType tv)          = text "Type" <+> pretty tv <+> text "is infinite"
        expl (NoSelect n)               = text "Attribute" <+> pretty n <+> text "is not found"
        expl (NoOverload c)             = explain c
        expl (NoUnify t1 t2)
          | isR t1 || isR t2            = text "Actual row" <+> pretty t1 <+> 
                                          text "and expected row" <+> pretty t2 <+> 
                                          text "do not match"
          | otherwise                   = text "Actual type" <+> pretty t1 <+> 
                                          text "and expected type" <+> pretty t2 <+> 
                                          text "do not unify"
          where isR ONil                = True
                isR (OPos _ _)          = True
                isR (OKwd _ _ _)        = True
                isR (OStar1 _ _)        = True
                isR (OStar2 _ _)        = True
                isR _                   = False
        expl (NoSolve c err)            = parens (text "version" <+> int (vn c)) $$ 
                                          explain c $$ 
                                          nonEmpty (text "because:" <+>) expl err
        expl (NotYet _ doc)             = text "Not yet supported by type inference:" <+> doc
        loc (NoSolve c _)               = cloc c
        loc (NoOverload c)              = cloc c
        loc (NotYet l _)                = l
        loc _                           = l0

explain (QEqu _ v t1 t2)                = text txt1 <+> pretty t1 $$
                                          text txt2 <+> pretty t2 $$
                                          if null txt3 then empty else text txt3
  where (txt1,txt2,txt3)                = equTxt v
explain (QIn _ v t1 t2)                 = text "Type" <+> pretty t2 <+> 
                                          text "does not contain values of type" <+> pretty t1
explain (QDot _ v t1 n t2)              = text "Cannot select attribute" <+> quotes (pretty n) <+> 
                                          text "of type" <+> pretty t2 <+>
                                          text "from value of type" <+> pretty t1
explain (QIx _ v t1 t2 t3)              = text "Cannot select an item of type" <+> pretty t3 <+> 
                                          text "with an index of type" <+> pretty t2 <+>
                                          text "from value of type" <+> pretty t1
explain (QNum _ v t1)                   = text "Type" <+> pretty t1 <+> 
                                          text "is not numeric"
explain (QPlus _ v t1)                  = text "Type" <+> pretty t1 <+> 
                                          text "does not support the '+' operator"
explain (QBool _ v t1)                  = text "Type" <+> pretty t1 <+> 
                                          text "does not convert to a truth value"

equTxt 36   = ("Top-level effect", "does not match the expected empty effect", "")                          -- scanTop
equTxt 0    = ("Instance type", "does not match", "")                                                       -- Var inst dump

equTxt 1    = ("Target type", "does not match right-hand side expression type", "")                         -- Assign
equTxt 2    = ("Target type", "does not match right-hand side expression type", "")                         -- AugAssign
equTxt 4    = ("Type", "does not match expected return type", "")                                           -- Return

equTxt 5    = ("Function type", "does not match the expected type", "of the defined function identifier")   -- Def n

equTxt 6    = ("The type of 'self'", "does not match the expected instance type", "")                       -- Class self
equTxt 7    = ("The type of '__init__'", "does not match the expected function type", "")                   -- Class __init__
equTxt 8    = ("Class type", "does not match the expected type", "of the defined class identifier")         -- Class n
equTxt 9    = ("Method type", "does not match the expected type", "")                                       -- Class Def n

equTxt 10   = ("The type of 'self'", "does not match the expected instance type", "")                       -- Actor self
equTxt 11   = ("Actor type", "does not match the expected type", "of the defined actor identifier")         -- Actor n

equTxt 13   = ("Data tree type", "does not match the type", "that is expected of the target")               -- Data
equTxt 14   = ("Decorator type", "does not match the type", "determined by the decorated item")             -- Decorator
equTxt 17   = ("Var target type", "does not match right-hand side expression type", "")                     -- VarAssign

equTxt 12   = ("Method type", "does not match the expected type", "")                                       -- Actor Def n

equTxt 15   = ("Exception type", "does not match the expected type", "of its alias")                        -- ExceptAs
equTxt 16   = ("Item type", "does not match the expected type", "of its alias")                             -- WithItem
equTxt 30   = ("Slice element type", "does not match the expected type", "")                                -- Slice

equTxt 18   = ("Called function's type", "does not match the expected type", "")                            -- Call
equTxt 37   = ("Awaited message's type", "does not match the expected type", "")                            -- Await
equTxt 38   = ("Indexed expression's type", "does not match the expected type", "")                         -- Ix Index
equTxt 39   = ("Sliced expression's type", "does not match the expected type", "")                          -- Ix Slice

equTxt 19   = ("The 'true' branch type", "does not match the expected type", "of the conditional expression")   -- Cond true
equTxt 20   = ("The 'false' branch type", "does not match the expected type", "of the conditional expression")  -- Cond false
equTxt 21   = ("The parameter type", "does not match the expected type", "of the unary operator")           -- UnOp
equTxt 22   = ("Referenced expression type", "does not match the expected tuple type", "")                  -- DotI
equTxt 29   = ("Lambda type", "does not match", "")                                                         -- Lambda inst dump

equTxt 23   = ("List element type", "does not match the expected type", "")                                 -- List
equTxt 24   = ("Unpacking expression type", "does not match the expected list type", "")                    -- List
equTxt 25   = ("Dictionary element type", "does not match the expected type", "")                           -- Dict
equTxt 26   = ("Set element type", "does not match the expected type", "")                                  -- Set
equTxt 27   = ("Unpacking expression type", "does not match the expected set type", "")                     -- Set

equTxt 35   = ("Parenthesized expression type", "does not match", "")                                       -- Paren

equTxt 28   = ("Type of default value", "does not match the expected type", "of the function parameter")    -- Param
equTxt 33   = ("Parameter type", "does not match the type", "implied by the * operator")                    -- StarPar
equTxt 34   = ("Parameter type", "does not match the type", "implied by the ** operator")                   -- StarPar

equTxt 32   = ("Superclass type", "does not match the expected callable type", "")                          -- Arg
equTxt 31   = ("Left comparison operand type", "does not match type", "of the right operand")               -- OpArg

equTxt 101  = ("Union effect", "does not match", "(impossible)")                                            -- Def union fx
equTxt 102  = ("Union effect", "does not match", "(impossible)")                                            -- Class union fx
equTxt 103  = ("Union effect", "does not match", "(impossible)")                                            -- Actor union fx
equTxt 104  = ("Union effect", "does not match", "(impossible)")                                            -- Actor Def union fx
equTxt 105  = ("Union effect", "does not match", "(impossible)")                                            -- Lambda union fx

equTxt 201  = ("Generic type", "does not match", "(impossible)")                                            -- Var gen dump
equTxt 202  = ("Generic record type", "does not match", "(impossible)")                                     -- QDot Record gen dump

equTxt n    = ("Types", "and", "do not unify (unknown tag version " ++ show n ++ ")")


o_openFX (OSchema vs cs (OFun fx r t))
  | Just fx1 <- open fx             = OSchema (v:vs) cs (OFun fx1 r t)
  where open (OKwd n t fx)          = OKwd n t <$> open fx
        open (OPos t fx)            = OPos t <$> open fx
        open ONil                   = Just (OVar v)
        open _                      = Nothing
        v:_                         = schemaVars \\ vs
o_openFX t                          = t

o_closeFX (OSchema vs cs (OFun fx r t))
  | [v] <- soletail fx \\ vs1,
    v `elem` vs                     = OSchema (vs\\[v]) cs (OFun (oSubst [(v,ONil)] fx) r t)
  where vs1                         = oTyvars t ++ oTyvars r ++ oTyvars cs
        soletail (OKwd _ t fx)      = soletail fx \\ oTyvars t
        soletail (OPos t fx)        = soletail fx \\ oTyvars t
        soletail (OVar v)           = [v]
        soletail _                  = []
o_closeFX t                         = t

openFX (TSchema l q (TFun l' fx p r t))
  | Just fx1 <- open fx             = TSchema l (TBind v [] : q) (TFun l' fx1 p r t)
  where open (TRow l n t fx)        = TRow l n t <$> open fx
        open (TNil l)               = Just (TVar l v)
        open (TVar _ _)             = Nothing
        v                           = head (tvarSupply \\ tybound q)
openFX t                            = t

closeFX (TSchema l q f@(TFun l' fx p r t))
  | TVar _ v <- rowTail fx, sole v  = TSchema l (filter ((v`notElem`) . tybound) q) (TFun l' (subst [(v,tNil)] fx) p r t)
  where sole v                      = v `elem` tybound q && length (filter (==v) (tyfree q ++ tyfree f)) == 1
closeFX t                           = t


