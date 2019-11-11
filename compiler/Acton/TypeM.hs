{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Acton.TypeM where

import Debug.Trace
import Data.Maybe (maybeToList, fromJust)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Control.Monad.State.Strict
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
                                        | EquGen    TSchema TSchema
                                        | SubGen    TSchema TSchema
                                        | Impl      Type TCon
                                        | Sel       Type Name Type
                                        | Mut       Type Name Type
                                        -- ...
                                        deriving (Eq,Show)

instance HasLoc Constraint where                 -- TODO: refine
    loc (Equ t _)                       = loc t
    loc (Sub t _)                       = loc t
    loc (EquGen sc _)                   = loc sc
    loc (SubGen sc _)                   = loc sc
    loc (Impl t _)                      = loc t
    loc (Sel t _ _)                     = loc t
    loc (Mut t _ _)                     = loc t

instance Pretty Constraint where
    pretty (Equ t1 t2)                  = pretty t1 <+> text "  =  " <+> pretty t2
    pretty (Sub t1 t2)                  = pretty t1 <+> text "  <  " <+> pretty t2
    pretty (EquGen sc1 sc2)             = pretty sc1 <+> text "  =  " <+> pretty sc2
    pretty (SubGen sc1 sc2)             = pretty sc1 <+> text "  <  " <+> pretty sc2
    pretty (Impl t u)                   = pretty t <+> text "  impl  " <+> pretty u
    pretty (Sel t1 n t2)                = pretty t1 <+> text " ." <> pretty n <> text "  =  " <+> pretty t2
    pretty (Mut t1 n t2)                = pretty t1 <+> text " ." <> pretty n <> text "  :=  " <+> pretty t2
    
type Constraints                        = [Constraint]

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

type TypeM a                            = State TypeState a

runTypeM                                :: TypeM a -> a
runTypeM m                              = evalState m (initTypeState Map.empty)
{-
type TypeM a                            = ExceptT TypeError (State TypeState) a

runTypeM                                :: TypeM a -> a
runTypeM m                              = case evalState (runExceptT m) (initTypeState Map.empty) of
                                            Right x -> x
                                            Left err -> internal ("Unhandled TypeM error: " ++ show err)
-}
newUnique                               :: TypeM Int
newUnique                               = state $ \st -> (nextint st, st{ nextint = nextint st + 1 })

constrain                               :: Constraints -> TypeM ()
constrain cs                            = state $ \st -> ((), st{ constraints = cs ++ constraints st })

collectConstraints                      :: TypeM Constraints
collectConstraints                      = state $ \st -> (constraints st, st{ constraints = [] })

pushFX                                  :: FXRow -> TypeM ()
pushFX fx                               = state $ \st -> ((), st{ effectstack = fx : effectstack st })

currFX                                  :: TypeM FXRow
currFX                                  = state $ \st -> (head (effectstack st), st)

equFX                                   :: FXRow -> TypeM ()
equFX fx                                = do fx0 <- currFX
                                             constrain [Equ fx fx0]

subFX                                   :: FXRow -> TypeM ()
subFX fx                                = do fx0 <- currFX
                                             constrain [Sub fx fx0]

popFX                                   :: TypeM ()
popFX                                   = state $ \st -> ((), st{ effectstack = tail (effectstack st) })

defer                                   :: Constraints -> TypeM ()
defer cs                                = state $ \st -> ((), st{ deferred = cs ++ deferred st })

collectDeferred                         :: TypeM Constraints
collectDeferred                         = state $ \st -> (deferred st, st)

substitute                              :: TVar -> Type -> TypeM ()
substitute tv@(TV Internal{}) t         = state $ \st -> ((), st{ currsubst = Map.insert tv t (currsubst st)})

getSubstitution                         :: TypeM (Map TVar Type)
getSubstitution                         = state $ \st -> (currsubst st, st)

dump                                    :: SrcInfo -> TypeM ()
dump inf                                = state $ \st -> ((), st{ dumped = inf ++ dumped st })

getDump                                 :: TypeM SrcInfo
getDump                                 = state $ \st -> (dumped st, st)


newName n                               = Internal (nstr n) <$> newUnique <*> return TypesPass

newTVar                                 = TVar NoLoc <$> TV <$> (Internal "V" <$> newUnique <*> return TypesPass)

newTVars n                              = mapM (const newTVar) [1..n]

subst                                   :: Subst a => Substitution -> a -> a
subst s x                               = evalState (msubst x) (initTypeState $ Map.fromList s)
--subst s x                               = case evalState (runExceptT (msubst x)) (initTypeState $ Map.fromList s) of
--                                            Right y  -> y

erase x                                 = subst s x
  where s                               = [ (tv, tWild) | tv <- nub (tyfree x) ]


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
    msubst (EquGen t1 t2)           = EquGen <$> msubst t1 <*> msubst t1
    msubst (SubGen t1 t2)           = SubGen <$> msubst t1 <*> msubst t1
    msubst (Impl t c)               = Impl <$> msubst t <*> msubst c
    msubst (Sel t1 n t2)            = Sel <$> msubst t1 <*> return n <*> msubst t2
    msubst (Mut t1 n t2)            = Mut <$> msubst t1 <*> return n <*> msubst t2
    tyfree (Equ t1 t2)              = tyfree t1 ++ tyfree t2
    tyfree (Sub t1 t2)              = tyfree t1 ++ tyfree t2
    tyfree (EquGen t1 t2)           = tyfree t1 ++ tyfree t2
    tyfree (SubGen t1 t2)           = tyfree t1 ++ tyfree t2
    tyfree (Impl t c)               = tyfree t ++ tyfree c
    tyfree (Sel t1 n t2)            = tyfree t1 ++ tyfree t2
    tyfree (Mut t1 n t2)            = tyfree t1 ++ tyfree t2

instance Subst TSchema where
    msubst sc@(TSchema l q t dec)   = (msubst' . Map.toList . Map.filterWithKey relevant) <$> getSubstitution
      where relevant k v            = k `elem` vs0
            vs0                     = tyfree sc
            msubst' s               = TSchema l (subst s q') (subst s t') dec
              where vs              = tybound q
                    newvars         = tyfree (rng s)
                    clashvars       = vs `intersect` newvars
                    avoidvars       = vs0 ++ vs ++ newvars
                    freshvars       = tvarSupply \\ avoidvars
                    renaming_s      = clashvars `zip` map (TVar NoLoc) freshvars
                    q'              = [ TBind (subst renaming_s v) (subst renaming_s cs) | TBind v cs <- q ]
                    t'              = subst renaming_s t

    tyfree (TSchema _ q t dec)      = (tyfree q ++ tyfree t) \\ tybound q
    tybound (TSchema _ q t dec)     = tybound q

testSchemaSubst = do
    putStrLn ("t:  " ++ render (pretty t))
    putStrLn ("s1: " ++ render (pretty s1))
    putStrLn ("s2: " ++ render (pretty s2))
    putStrLn ("s3: " ++ render (pretty s3))
    putStrLn ("subst s1 t: " ++ render (pretty (subst s1 t)))
    putStrLn ("subst s2 t: " ++ render (pretty (subst s2 t)))
    putStrLn ("subst s3 t: " ++ render (pretty (subst s3 t)))
  where t   = tSchema [TBind (TV (name "A")) [TC (noQual "Eq") []]]
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
    msubst (TUnion l as)            = return $ TUnion l as
    msubst (TOpt l t)               = TOpt l <$> msubst t
    msubst (TNone l)                = return $ TNone l
    msubst (TWild l)                = return $ TWild l
    msubst (TNil l)                 = return $ TNil l
    msubst (TRow l n t r)           = TRow l n <$> msubst t <*> msubst r

    tyfree (TVar _ v)               = [v]
    tyfree (TCon _ c)               = tyfree c
    tyfree (TAt _ c)                = tyfree c
    tyfree (TFun _ fx p k t)        = tyfree fx ++ tyfree p ++ tyfree k ++ tyfree t
    tyfree (TTuple _ p)             = tyfree p
    tyfree (TRecord _ k)            = tyfree k
    tyfree (TUnion _ as)            = []
    tyfree (TOpt _ t)               = tyfree t
    tyfree (TNone _)                = []
    tyfree (TWild _)                = []
    tyfree (TNil _)                 = []
    tyfree (TRow _ _ t r)           = tyfree t ++ tyfree r


----------------------------------------------------------------------------------

data TypeError                      = TypeErrHmm            -- ...
                                    | RigidVariable TVar
                                    | InfiniteType TVar
                                    | ConflictingRow TVar
                                    | KwdNotFound Name
                                    | DistinctDecorations Decoration Decoration
                                    | EscapingVar [TVar] TSchema TSchema
                                    | NoSelStatic Name TCon
                                    | NoSelInstByClass Name TCon
                                    | NoMutProto Name
                                    | NoMutClass Name
                                    | LackSig Name
                                    | LackDef Name
                                    | NoRed Constraint
                                    deriving (Eq,Show,Typeable)

instance Control.Exception.Exception TypeError

instance HasLoc TypeError where
    loc (RigidVariable tv)          = loc tv
    loc (InfiniteType tv)           = loc tv
    loc (ConflictingRow tv)         = loc tv
    loc (KwdNotFound n)             = loc n
    loc (DistinctDecorations _ _)   = NoLoc
    loc (EscapingVar tvs t1 t2)     = loc tvs
    loc (NoSelStatic n u)           = loc n
    loc (NoSelInstByClass n u)      = loc n
    loc (NoMutProto n)              = loc n
    loc (NoMutClass n)              = loc n
    loc (LackSig n)                 = loc n
    loc (LackDef n)                 = loc n
    loc (NoRed c)                   = loc c

notYetExpr e                        = notYet (loc e) e

rigidVariable tv                    = Control.Exception.throw $ RigidVariable tv
infiniteType tv                     = Control.Exception.throw $ InfiniteType tv
conflictingRow tv                   = Control.Exception.throw $ ConflictingRow tv
kwdNotFound n                       = Control.Exception.throw $ KwdNotFound n
distinctDecorations d1 d2           = Control.Exception.throw $ DistinctDecorations d1 d2
escapingVar tvs t1 t2               = Control.Exception.throw $ EscapingVar tvs t1 t2
noSelStatic n u                     = Control.Exception.throw $ NoSelStatic n u
noSelInstByClass n u                = Control.Exception.throw $ NoSelInstByClass n u
noMutProto n                        = Control.Exception.throw $ NoMutProto n
noMutClass n                        = Control.Exception.throw $ NoMutClass n
lackSig ns                          = Control.Exception.throw $ LackSig (head ns)
lackDef ns                          = Control.Exception.throw $ LackDef (head ns)
noRed c                             = Control.Exception.throw $ NoRed c


typeError err                       = (loc err,render (expl err))
  where
    expl (RigidVariable tv)         = text "Type" <+> pretty tv <+> text "is rigid"
    expl (InfiniteType tv)          = text "Type" <+> pretty tv <+> text "is infinite"
    expl (ConflictingRow tv)        = text "Row" <+> pretty tv <+> text "has conflicting extensions"
    expl (KwdNotFound n)            = text "Keyword element" <+> quotes (pretty n) <+> text "is not found"
    expl (DistinctDecorations d d') = text "Decorations" <+> pretty d <+> text "and" <+> text "do not match"
    expl (EscapingVar tvs t1 t2)    = text "Cannot infer" <+> pretty (SubGen t1 t2) <+> text "because type variable" <+>
                                      pretty (head tvs) <+> text "escapes"
    expl (NoSelStatic n u)          = text "Static method" <+> pretty n <+> text "cannot be selected from" <+> pretty u <+> text "instance"
    expl (NoSelInstByClass n u)     = text "Instance attribute" <+> pretty n <+> text "cannot be selected from class" <+> pretty u
    expl (NoMutProto n)             = text "Protocol attribute" <+> pretty n <+> text "cannot be mutated"
    expl (NoMutClass n)             = text "Class attribute" <+> pretty n <+> text "cannot be mutated"
    expl (LackSig n)                = text "Declaration lacks accompanying signature"
    expl (LackDef n)                = text "Signature lacks accompanying definition"
    expl (NoRed c)                  = text "Cannot infer" <+> pretty c

