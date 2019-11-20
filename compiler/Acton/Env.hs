{-# LANGUAGE FlexibleInstances, DeriveGeneric #-}
module Acton.Env where

import qualified Control.Exception
import Debug.Trace
import qualified Data.Binary
import GHC.Generics (Generic)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Control.Monad.State.Strict
import Data.Typeable
import Data.Traversable
import System.FilePath.Posix (joinPath)
import System.Directory (doesFileExist)
import Control.Monad

import Acton.Syntax
import Acton.Builtin
import Acton.Printer
import Acton.Names
import Utils
import Pretty
import InterfaceFiles
-- import Prelude hiding ((<>))




mkEnv                       :: (FilePath,FilePath) -> Env -> Module -> IO Env
mkEnv paths env modul       = getImps paths (setDefaultMod m env) imps
  where Module m imps _     = modul


type Schemas                = [(Name, TSchema)]

type TEnv                   = [(Name, NameInfo)]

data Env                    = Env { names :: TEnv, modules :: [(ModName,TEnv)], defaultmod :: ModName, nocheck :: Bool }

data NameInfo               = NVar      TSchema
                            | NSVar     TSchema
                            | NSig      TSchema
                            | NClass    [TBind] [TCon] TEnv
                            | NProto    [TBind] [TCon] TEnv
                            | NExt      QName [TBind] [TCon]
                            | NTVar     [TCon]
                            | NAlias    QName
                            | NMAlias   ModName
                            | NModule   TEnv
                            | NReserved
                            | NBlocked
                            deriving (Eq,Show,Read,Generic)

nVar                        :: Name -> Type -> TEnv
nVar n t                    = [(n, NVar (monotype t))]

nVar'                       :: Name -> TSchema -> TEnv
nVar' n sc                  = [(n, NVar sc)]

nState                      :: TEnv -> TEnv
nState te                   = [ (n, NSVar t) | (n, NVar t) <- te ]

nSig                        :: [Name] -> TSchema -> TEnv
nSig xs t                   = [ (x, NSig t) | x <- xs ]

nClass                      :: Name -> [TBind] -> [TCon] -> TEnv -> TEnv
nClass n q us te            = [(n, NClass q us te)]

nProto                      :: ModName -> Name -> [TBind] -> [TCon] -> TEnv -> TEnv
nProto m n q us te          = (n, NProto q us te) : te'
  where
    te'                     = [ (n', NSig (f sc)) | (n',NSig sc) <- te, scdec sc == StaticMethod ]
    f (TSchema l q' t d)    = TSchema l (q ++ (TBind tvSelf [TC (NoQual n) $ map (tVar . TV) $ bound q]) : q') t d

nExt                        :: Name -> QName -> [TBind] -> [TCon] -> TEnv
nExt w n q us               = [(w, NExt n q us)]

nVars                       :: TEnv -> Schemas
nVars te                    = [ (n,sc) | (n, NVar sc) <- te ]

nSigs                       :: TEnv -> Schemas
nSigs te                    = [ (n,sc) | (n, NSig sc) <- te ]

nCombine te1 te2            = te1 ++ te2

mapVars                     :: (TSchema -> TSchema) -> TEnv -> TEnv
mapVars f te                = map g te
  where 
    g (n, NVar sc)          = (n, NVar (f sc))
    g (n, NClass q u te)    = (n, NClass q u (map g te))
--    g (n, NProto q u te)    = (n, NProto q u (map g te))
    g (n, i)                = (n, i)


instance Data.Binary.Binary NameInfo

instance Pretty TEnv where
    pretty tenv                 = vcat (map pretty tenv)

instance Pretty Env where
    pretty env                  = vcat (map pretty (names env))

instance Pretty (Name,NameInfo) where
    pretty (n, NVar t)          = prettySig [n] t
    pretty (n, NSVar t)         = text "var" <+> pretty n <+> colon <+> pretty t
    pretty (n, NClass q us [])  = text "class" <+> pretty n <+> nonEmpty brackets commaList q <+>
                                  nonEmpty parens commaList us
    pretty (n, NClass q us te)  = text "class" <+> pretty n <+> nonEmpty brackets commaList q <+>
                                  nonEmpty parens commaList us <> colon $+$ (nest 4 $ pretty te)
    pretty (n, NProto q us [])  = text "protocol" <+> pretty n <+> nonEmpty brackets commaList q <+>
                                  nonEmpty parens commaList us
    pretty (n, NProto q us te)  = text "protocol" <+> pretty n <+> nonEmpty brackets commaList q <+>
                                  nonEmpty parens commaList us <> colon $+$ (nest 4 $ pretty te)
    pretty (w, NExt n q us)     = pretty w  <+> colon <+> text "extension" <+> pretty n <+>
                                  nonEmpty brackets commaList q <+> nonEmpty parens commaList us
    pretty (n, NTVar us)        = pretty n <> parens (commaList us)
    pretty (n, NAlias qn)       = text "alias" <+> pretty n <+> equals <+> pretty qn
    pretty (n, NMAlias m)       = text "module" <+> pretty n <+> equals <+> pretty m
    pretty (n, NModule te)      = text "module" <+> pretty n <> colon $+$ nest 4 (pretty te)
    pretty (n, NSig t)          = pretty n <+>  colon <+> pretty t
    pretty (n, NReserved)       = pretty n <+> text "(reserved)"
    pretty (n, NBlocked)        = pretty n <+> text "(blocked)"

instance Subst Env where
    msubst env                  = do ne <- msubst (names env)
                                     return env{ names = ne }
    tyfree env                  = tyfree (names env)

instance Subst NameInfo where
    msubst (NVar t)             = NVar <$> msubst t
    msubst (NSVar t)            = NSVar <$> msubst t
    msubst (NSig t)             = NSig <$> msubst t
    msubst (NClass q us te)     = NClass <$> msubst q <*> msubst us <*> msubst te
    msubst (NProto q us te)     = NProto <$> msubst q <*> msubst us <*> msubst te
    msubst (NExt n q us)        = NExt n <$> msubst q <*> msubst us
    msubst (NTVar us)           = NTVar <$> msubst us
    msubst (NAlias qn)          = NAlias <$> return qn
    msubst (NMAlias m)          = NMAlias <$> return m
    msubst (NModule te)         = NModule <$> return te     -- actually msubst te, but te has no free variables (top-level)
    msubst NReserved            = return NReserved
    msubst NBlocked             = return NBlocked

    tyfree (NVar t)             = tyfree t
    tyfree (NSVar t)            = tyfree t
    tyfree (NSig t)             = tyfree t
    tyfree (NClass q us te)     = (tyfree q ++ tyfree us ++ tyfree te) \\ tybound q
    tyfree (NProto q us te)     = (tyfree q ++ tyfree us ++ tyfree te) \\ tybound q
    tyfree (NExt n q us)        = (tyfree q ++ tyfree us) \\ tybound q
    tyfree (NTVar us)           = tyfree us
    tyfree (NAlias qn)          = []
    tyfree (NMAlias qn)         = []
    tyfree (NModule te)         = []        -- actually tyfree te, but a module has no free variables on the top level
    tyfree NReserved            = []
    tyfree NBlocked             = []

msubstTV tvs                    = fmap tyfree $ mapM msubst $ map tVar tvs

instance Subst SrcInfoTag where
    msubst (GEN l t)                = GEN l <$> msubst t
    msubst (INS l t)                = INS l <$> msubst t

    tyfree (GEN _ t)                = tyfree t
    tyfree (INS _ t)                = tyfree t


-------------------------------------------------------------------------------------------------------------------

class Unalias a where
    unalias                         :: Env -> a -> a
    unalias env                     = id

instance (Unalias a) => Unalias [a] where
    unalias env                     = map (unalias env)

instance (Unalias a) => Unalias (Maybe a) where
    unalias env                     = fmap (unalias env)

instance Unalias ModName where
    unalias env (ModName ns)        = norm (names env) [] ns
      where
        norm te pre []              = ModName (reverse pre)
        norm te pre (n:ns)          = case lookup n te of
                                        Just (NModule te') -> norm te' (n:pre) ns
                                        Just (NMAlias m) -> norm (findMod m env) [] ns

instance Unalias QName where
    unalias env (QName m n)         = case lookup m' (modules env) of
                                        Just te -> case lookup n te of
                                                      Just (NAlias qn) -> qn
                                                      Just _ -> QName m' n
      where m'                      = unalias env m
    unalias env (NoQual n)          = case lookup n (names env) of
                                        Just (NAlias qn) -> qn
                                        Just _ -> QName (defaultmod env) n
                                    
instance Unalias TSchema where
    unalias env (TSchema l q t d)   = TSchema l (unalias env q) (unalias env t) d

instance Unalias TCon where
    unalias env (TC qn ts)          = TC (unalias env qn) (unalias env ts)

instance Unalias TBind where
    unalias env (TBind tv cs)       = TBind tv (unalias env cs)

instance Unalias Type where
    unalias env (TCon l c)          = TCon l (unalias env c)
    unalias env (TAt l c)           = TAt l (unalias env c)
    unalias env (TFun l e p r t)    = TFun l (unalias env e) (unalias env p) (unalias env r) (unalias env t)
    unalias env (TTuple l p)        = TTuple l (unalias env p)
    unalias env (TRecord l r)       = TTuple l (unalias env r)
    unalias env (TOpt l t)          = TOpt l (unalias env t)
    unalias env (TRow l n t r)      = TRow l n (unalias env t) (unalias env r)
    unalias env t                   = t

instance Unalias NameInfo where
    unalias env (NVar t)            = NVar (unalias env t)
    unalias env (NSVar t)           = NSVar (unalias env t)
    unalias env (NSig t)            = NSig (unalias env t)
    unalias env (NClass q us te)    = NClass (unalias env q) (unalias env us) (unalias env te)
    unalias env (NProto q us te)    = NProto (unalias env q) (unalias env us) (unalias env te)
    unalias env (NExt n q us)       = NExt n (unalias env q) (unalias env us)
    unalias env (NTVar us)          = NTVar (unalias env us)
    unalias env (NAlias qn)         = NAlias (unalias env qn)
    unalias env (NModule te)        = NModule (unalias env te)
    unalias env NReserved           = NReserved
    unalias env NBlocked            = NBlocked

instance Unalias (Name,NameInfo) where
    unalias env (n,i)               = (n, unalias env i)
    
-------------------------------------------------------------------------------------------------------------------

envBuiltin                  = [ (nSequence,         NProto [a] [] []),
                                (nMapping,          NProto [a,b] [] []),
                                (nSet,              NProto [a] [] []),
                                (nInt,              NClass [] [] []),
                                (nFloat,            NClass [] [] []),
                                (nBool,             NClass [] [] []),
                                (nStr,              NClass [] [] [
                                                        (name "join",   NVar (monotype $ tFun0 [tSeq tStr] tStr)),
                                                        (name "strip",  NVar (monotype $ tFun0 [] tStr))
                                                    ]),
                                (nRef,              NClass [] [] []),
                                (nMsg,              NClass [] [] []),
                                (nException,        NClass [] [] []),
                                (nBoolean,          NProto [] [] []),
                                (nIndexed,          NProto [a,b] [] []),
                                (nSliceable,        NProto [] [] []),
                                (nPlus,             NProto [] [] []),
                                (nMinus,            NProto [] [] []),
                                (nNumber,           NProto [] [] []),
                                (nReal,             NProto [] [] []),
                                (nIntegral,         NProto [] [] []),
                                (nLogical,          NProto [] [] []),
                                (nMatrix,           NProto [] [] []),
                                (nEq,               NProto [] [] []),
                                (nOrd,              NProto [] [] []),
                                (nIdentity,         NProto [] [] []),
                                (nCollection,       NProto [a] [] []),
                                (nContextManager,   NProto [] [] []),
                                (nObject,           NClass [] [] []),
                                (nStopIteration,    NClass [] [] []),
                                (nValueError,       NClass [] [] []),
                                (nShow,             NProto [] [] []),
                                (nLen,              NVar (monotype $ tFun0 [pCollection tWild] tInt)),
                                (nPrint,            NVar (tSchema [bounded cShow a] $ tFun fxNil ta kwdNil tNone)),
                                (nPostpone,         NVar (monotype $ tFun0 [tInt, tAsync [] tNone] tNone))
                              ]
  where 
    a:b:c:_                 = [ TBind v [] | v <- tvarSupply ]
    bounded u (TBind v us)  = TBind v (u:us)
    ta:tb:tc:_              = [ TVar NoLoc v | v <- tvarSupply ]

envActorSelf                = [ (selfKW, NVar (monotype tRef)) ]

--------------------------------------------------------------------------------------------------------------------

prune xs                    = filter ((`notElem` xs) . fst)

initEnv                     :: Env
initEnv                     = define autoImp $ defineMod mBuiltin $ addMod mBuiltin envBuiltin env0
  where autoImp             = importAll mBuiltin envBuiltin
        env0                = Env{ names = [], modules = [], defaultmod = mBuiltin, nocheck = False }

setDefaultMod               :: ModName -> Env -> Env
setDefaultMod m env         = env{ defaultmod = m }

setNoCheck                  :: Env -> Env
setNoCheck env              = env{ nocheck = True }

noCheck                     :: Env -> Bool
noCheck env                 = nocheck env

stateScope                  :: Env -> [Name]
stateScope env              = [ z | (z, NSVar _) <- names env ]

reserve                     :: [Name] -> Env -> Env
reserve xs env              = env{ names = [ (x, NReserved) | x <- nub xs ] ++ names env }

block                       :: [Name] -> Env -> Env
block xs env                = env{ names = [ (x, NBlocked) | x <- nub xs ] ++ names env }

define                      :: TEnv -> Env -> Env
define te env               = env{ names = reverse te ++ prune (dom te) (names env) }

defineMod                   :: ModName -> Env -> Env
defineMod (ModName ns) env  = define [(head ns, defmod (tail ns) $ te1)] env
  where te1                 = case lookup (head ns) (names env) of Just (NModule te1) -> te1; _ -> []
        defmod [] te        = NModule $ findMod (ModName ns) env
        defmod (n:ns) te    = NModule $ (n, defmod ns te2) : prune [n] te
          where te2         = case lookup n te of Just (NModule te2) -> te2; _ -> []

defineTVars                 :: [TBind] -> Env -> Env
defineTVars q env           = env{ names = [ (n, NTVar us) | TBind (TV n) us <- q ] ++ names env }

tvarScope                   :: Env -> [TVar]
tvarScope env               = [ TV n | (n, NTVar _) <- names env ]

defineSelf                  :: Name -> [TBind] -> Env -> Env
defineSelf n q env          = defineSelf' (NoQual n) q env

defineSelf'                 :: QName -> [TBind] -> Env -> Env
defineSelf' qn q env        = env{ names = (nSelf, NTVar [tc]) : names env }
  where tc                  = TC qn [ tVar tv | TBind tv _ <- q ]

blocked                     :: Env -> Name -> Bool
blocked env n               = lookup n (names env) == Just NBlocked

reserved                    :: Name -> Env -> Bool
reserved n env              = case lookup n (names env) of
                                Just NReserved -> True
                                _ -> False

reservedOrSig               :: Name -> Env -> Maybe (Maybe TSchema)
reservedOrSig n env         = case lookup n (names env) of
                                Just NReserved -> Just Nothing
                                Just (NSig t)  -> Just (Just t)
                                _              -> Nothing

findSig                     :: Name -> Env -> Maybe TSchema
findSig n env               = case lookup n (names env) of
                                Just (NSig t)  -> Just t
                                _              -> Nothing

findName                    :: Name -> Env -> NameInfo
findName n env              = case lookup n (names env) of
                                Just (NAlias qn) -> findQName qn env
                                Just NReserved -> nameReserved n
                                Just (NSig t) -> nameReserved n
                                Just NBlocked -> nameBlocked n
                                Just info -> info
                                Nothing -> nameNotFound n

findQName                   :: QName -> Env -> NameInfo
findQName (QName m n) env   = case lookup n (findMod (unalias env m) env) of
                                Just (NAlias qn) -> findQName qn env
                                Just i -> i
                                _ -> noItem m n
findQName (NoQual n) env    = findName n env

findSelf                    :: Env -> TCon
findSelf env                = case findName nSelf env of
                                NTVar (u:us) -> u

findMod                     :: ModName -> Env -> TEnv
findMod m env               = case lookup m (modules env) of
                                Just te -> te

isMod                       :: Env -> [Name] -> Bool
isMod env ns                = case lookup (ModName ns) (modules env) of
                                Just te -> True
                                _       -> False

addMod                      :: ModName -> TEnv -> Env -> Env
addMod m te env             = env{ modules = (m,te) : modules env }


dropNames                   :: Env -> Env
dropNames env               = env{ names = names initEnv }

isProto                     :: Env -> QName -> Bool
isProto env n               = case findQName n env of
                                NProto q us te -> True
                                _ -> False

findClass                   :: QName -> Env -> ([TBind],[TCon],TEnv)
findClass n env             = case findQName n env of
                                NClass q us te -> (q,us,te)
                                _ -> err1 n "Class name expected, got"

findProto                   :: QName -> Env -> ([TBind],[TCon],TEnv)
findProto n env             = case findQName n env of
                                NProto q us te -> (q,us,te)
                                _ -> err1 n "Protocol name expected, got"

findSubBound                :: TVar -> Env -> Maybe TCon
findSubBound tv env         = case findName (tvname tv) env of
                                NTVar (u:us) | not $ isProto env (tcname u) -> Just u
                                _ -> Nothing

findImplBound               :: TVar -> Env -> [TCon]
findImplBound tv env        = case findName (tvname tv) env of
                                NTVar (u:us) | isProto env (tcname u) -> u:us
                                             | otherwise -> us
                                _ -> []

findVarType                 :: Name -> Env -> TSchema
findVarType n env           = findVarType' (NoQual n) env

findVarType'                :: QName -> Env -> TSchema
findVarType' n env          = case findQName n env of
                                NVar t         -> t
                                NSVar t        -> t
                                NSig t         -> t
                                NClass q _ _   -> tSchema q (tAt $ TC n $ map tVar $ tybound q)
                                NProto q _ _   -> tSchema q (tAt $ TC n $ map tVar $ tybound q)
                                _              -> err1 n "Unexpected name..."

findSubAxiom                :: Env -> TCon -> QName -> (Constraints, Type)
findSubAxiom env c n
  | null hits               = err1 n ("Not related: " ++ prstr c ++ " and")
  | otherwise               = (cs, tCon $ head hits)
  where (cs,us,_)           = findCon env c
        hits                = [ u | u <- us, tcname u == n ]

findAttr                    :: Env -> TCon -> Name -> (Constraints, TSchema)
findAttr env u n            = (cs, findIn (te ++ concat tes))
  where (cs,us,te)          = findCon env u
        tes                 = [ te' | u' <- us, let (_,_,te') = findCon env u' ]
        findIn te1          = case lookup n te1 of
                                Just (NVar t)         -> t
                                Just (NSVar t)        -> t
                                Just (NSig t)         -> t
                                Just (NClass q _ _)   -> tSchema q (tAt $ TC (NoQual n) $ map tVar $ tybound q)
                                Just (NProto q _ _)   -> tSchema q (tAt $ TC (NoQual n) $ map tVar $ tybound q)
                                Nothing               -> err1 n "Attribute not found:"

findCon                     :: Env -> TCon -> (Constraints, [TCon], TEnv)
findCon env u               = (constraintsOf env (subst s q), subst s us, subst s te)
  where (_, q, us, te)      = case findQName (tcname u) env of
                                NClass q us te -> (False,q,us,te)
                                NProto q us te -> (True,q,us,te)
                                _ -> err1 (tcname u) "Class or protocol name expected, got"
        s                   = tybound q `zip` tcargs u

findWitness                 :: Env -> QName -> [TBind] -> [TCon] -> Maybe Name
findWitness env n q us      = case [ w | (w, NExt n' q' us') <- names env, (n,q,us) == (n',q',us') ] of
                                [w] -> Just w
                                _   -> Nothing

constraintsOf               :: Env -> [TBind] -> Constraints
constraintsOf env q         = [ constr t u | TBind v us <- q, let t = tVar v, u <- us ]
  where constr t u@(TC n _)
          | isProto env n   = Impl env t u
          | otherwise       = Sub env t (tCon u)


-- Environment unification ---------------------------------------------------------------

unifyTEnv                               :: Env -> [TEnv] -> [Name] -> TypeM ()
unifyTEnv env tenvs []                  = return ()
unifyTEnv env tenvs (v:vs)              = case [ ni | Just ni <- map (lookup v) tenvs] of
                                            ni:nis -> mapM (unif ni) nis >> unifyTEnv env tenvs vs
  where
    unif (NVar t) (NVar t')             = unifT t t'
    unif (NSVar t) (NSVar t')           = unifT t t'
    unif (NSig t) (NSig t')             = unifT t t'
    unif (NClass q us te) (NClass q' us' te') 
                                        = unifC q us te q' us' te'
    unif (NProto q us te) (NProto q' us' te') 
                                        = unifC q us te q' us' te'
    unif (NExt _ q us) (NExt _ q' us')  = unifC q us [] q' us' []
    unif _ _                            = err1 v "Inconsistent bindings for"

    unifT (TSchema _ [] t d) (TSchema _ [] t' d')
      | d == d'                         = constrain [Equ env t t']
    unifT t t'                          = err1 v "Cannot merge bindings of polymorphic type"
    
    unifC q us te q' us' te'
      | q /= q' || us /= us'            = err1 v "Inconsistent declaration heads for"
      | not $ null diff                 = err1 v "Inconsistent attribute sets for"
      | otherwise                       = unifyTEnv env [te,te'] vs
      where diff                        = (vs \\ vs') ++ (vs' \\ vs)
            (vs, vs')                   = (dom te, dom te')


-- Import handling -----------------------------------------------------------------------

getImps                         :: (FilePath,FilePath) -> Env -> [Import] -> IO Env
getImps ps env []               = return env
getImps ps env (i:is)           = do env' <- impModule ps env i
                                     getImps ps env' is


impModule                       :: (FilePath,FilePath) -> Env -> Import -> IO Env
impModule ps env (Import _ ms)  = imp env ms
  where imp env []              = return env
        imp env (ModuleItem m as : is)
                                = do (env1,te) <- doImp ps env m
                                     let env2 = maybe (defineMod m env1) (\n->define [(n, NMAlias m)] env1) as
                                     imp env2 is
impModule ps env (FromImport _ (ModRef (0,Just m)) items)
                                = do (env1,te) <- doImp ps env m
                                     return $ define (importSome items m te) env1
impModule ps env (FromImportAll _ (ModRef (0,Just m)))
                                = do (env1,te) <- doImp ps env m
                                     return $ define (importAll m te) env1
impModule _ _ i                 = illegalImport (loc i)


doImp (p,sysp) env m            = case lookup m (modules env) of
                                    Just te -> return (env, te)
                                    Nothing -> do
                                        found <- doesFileExist fpath
                                        if found
                                         then do te <- InterfaceFiles.readFile fpath
                                                 return (addMod m te env, te)
                                         else do found <- doesFileExist fpath2
                                                 unless found (fileNotFound m)
                                                 te <- InterfaceFiles.readFile fpath
                                                 return (addMod m te env, te)
  where fpath                   = joinPath (p : mpath m) ++ ".ty"
        fpath2                  = joinPath (sysp : mpath m) ++ ".ty"
        mpath (ModName ns)      = map nstr ns


importSome                  :: [ImportItem] -> ModName -> TEnv -> TEnv
importSome items m te       = map pick items
  where 
    te1                     = importAll m te
    pick (ImportItem n mbn) = case lookup n te1 of
                                    Just i  -> (maybe n id mbn, i) 
                                    Nothing -> noItem m n

importAll                   :: ModName -> TEnv -> TEnv
importAll m te              = mapMaybe imp te
  where 
    imp (n, NProto _ _ _)   = Just (n, NAlias (QName m n))
    imp (n, NClass _ _ _)   = Just (n, NAlias (QName m n))
    imp (n, NExt _ _ _)     = Nothing
    imp (n, NAlias _)       = Just (n, NAlias (QName m n))
    imp (n, NVar t)         = Just (n, NVar t)
    imp _                   = Nothing                               -- cannot happen


-- Type inference monad ------------------------------------------------------------------


data Constraint                         = Equ       Env Type Type
                                        | Sub       Env Type Type
                                        | EquGen    Env TSchema TSchema
                                        | SubGen    Env TSchema TSchema
                                        | Impl      Env Type TCon
                                        | Sel       Env Type Name Type
                                        | Mut       Env Type Name Type

instance HasLoc Constraint where                 -- TODO: refine
    loc (Equ _ t _)                     = loc t
    loc (Sub _ t _)                     = loc t
    loc (EquGen _ sc _)                 = loc sc
    loc (SubGen _ sc _)                 = loc sc
    loc (Impl _ t _)                    = loc t
    loc (Sel _ t _ _)                   = loc t
    loc (Mut _ t _ _)                   = loc t

instance Pretty Constraint where
    pretty (Equ _ t1 t2)                = pretty t1 <+> text "  =  " <+> pretty t2
    pretty (Sub _ t1 t2)                = pretty t1 <+> text "  <  " <+> pretty t2
    pretty (EquGen _ sc1 sc2)           = pretty sc1 <+> text "  =  " <+> pretty sc2
    pretty (SubGen _ sc1 sc2)           = pretty sc1 <+> text "  <  " <+> pretty sc2
    pretty (Impl _ t u)                 = pretty t <+> text "  impl  " <+> pretty u
    pretty (Sel _ t1 n t2)              = pretty t1 <+> text " ." <> pretty n <> text "  =  " <+> pretty t2
    pretty (Mut _ t1 n t2)              = pretty t1 <+> text " ." <> pretty n <> text "  :=  " <+> pretty t2

instance Show Constraint where
    show                                = render . pretty

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

equFX                                   :: Env -> FXRow -> TypeM ()
equFX env fx                            = do fx0 <- currFX
                                             constrain [Equ env fx fx0]

subFX                                   :: Env -> FXRow -> TypeM ()
subFX env fx                            = do fx0 <- currFX
                                             constrain [Sub env fx fx0]

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

newTVar                                 = TVar NoLoc <$> TV <$> (Internal "V" <$> newUnique <*> return GenPass)

newTVars n                              = mapM (const newTVar) [1..n]

subst                                   :: Subst a => Substitution -> a -> a
subst s x                               = evalState (msubst x) (initTypeState $ Map.fromList s)

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
    msubst (Equ env t1 t2)          = Equ <$> msubst env <*> msubst t1 <*> msubst t2
    msubst (Sub env t1 t2)          = Sub <$> msubst env <*> msubst t1 <*> msubst t1
    msubst (EquGen env t1 t2)       = EquGen <$> msubst env <*> msubst t1 <*> msubst t1
    msubst (SubGen env t1 t2)       = SubGen <$> msubst env <*> msubst t1 <*> msubst t1
    msubst (Impl env t c)           = Impl <$> msubst env <*> msubst t <*> msubst c
    msubst (Sel env t1 n t2)        = Sel <$> msubst env <*> msubst t1 <*> return n <*> msubst t2
    msubst (Mut env t1 n t2)        = Mut <$> msubst env <*> msubst t1 <*> return n <*> msubst t2
    tyfree (Equ _ t1 t2)            = tyfree t1 ++ tyfree t2
    tyfree (Sub _ t1 t2)            = tyfree t1 ++ tyfree t2
    tyfree (EquGen _ t1 t2)         = tyfree t1 ++ tyfree t2
    tyfree (SubGen _ t1 t2)         = tyfree t1 ++ tyfree t2
    tyfree (Impl _ t c)             = tyfree t ++ tyfree c
    tyfree (Sel _ t1 n t2)          = tyfree t1 ++ tyfree t2
    tyfree (Mut _ t1 n t2)          = tyfree t1 ++ tyfree t2

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



-- Error handling ------------------------------------------------------------------------

data CheckerError                   = FileNotFound ModName
                                    | NameNotFound Name
                                    | NameReserved Name
                                    | NameBlocked Name
                                    | IllegalRedef Name
                                    | MissingSelf Name
                                    | IllegalImport SrcLoc
                                    | DuplicateImport Name
                                    | NoItem ModName Name
                                    | OtherError SrcLoc String
                                    deriving (Show)

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
                                    deriving (Show)

instance Control.Exception.Exception TypeError
instance Control.Exception.Exception CheckerError


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

typeError err                       = (loc err,render (expl err))
  where
    expl (RigidVariable tv)         = text "Type" <+> pretty tv <+> text "is rigid"
    expl (InfiniteType tv)          = text "Type" <+> pretty tv <+> text "is infinite"
    expl (ConflictingRow tv)        = text "Row" <+> pretty tv <+> text "has conflicting extensions"
    expl (KwdNotFound n)            = text "Keyword element" <+> quotes (pretty n) <+> text "is not found"
    expl (DistinctDecorations d d') = text "Decorations" <+> pretty d <+> text "and" <+> text "do not match"
    expl (EscapingVar tvs t1 t2)    = text "Cannot instantiate" <+> pretty t1 <+> text "to" <+> pretty t2 <+> 
                                      text "because type variable" <+> pretty (head tvs) <+> text "escapes"
    expl (NoSelStatic n u)          = text "Static method" <+> pretty n <+> text "cannot be selected from" <+> pretty u <+> text "instance"
    expl (NoSelInstByClass n u)     = text "Instance attribute" <+> pretty n <+> text "cannot be selected from class" <+> pretty u
    expl (NoMutProto n)             = text "Protocol attribute" <+> pretty n <+> text "cannot be mutated"
    expl (NoMutClass n)             = text "Class attribute" <+> pretty n <+> text "cannot be mutated"
    expl (LackSig n)                = text "Declaration lacks accompanying signature"
    expl (LackDef n)                = text "Signature lacks accompanying definition"
    expl (NoRed c)                  = text "Cannot infer" <+> pretty c


checkerError (FileNotFound n)       = (loc n, " Type interface file not found for " ++ prstr n)
checkerError (NameNotFound n)       = (loc n, " Name " ++ prstr n ++ " is not in scope")
checkerError (NameReserved n)       = (loc n, " Name " ++ prstr n ++ " is reserved but not yet defined")
checkerError (NameBlocked n)        = (loc n, " Name " ++ prstr n ++ " is currently not accessible")
checkerError (IllegalRedef n)       = (loc n, " Illegal redefinition of " ++ prstr n)
checkerError (MissingSelf n)        = (loc n, " Missing 'self' parameter in definition of")
checkerError (IllegalImport l)      = (l,     " Relative import not yet supported")
checkerError (DuplicateImport n)    = (loc n, " Duplicate import of name " ++ prstr n)
checkerError (NoItem m n)           = (loc n, " Module " ++ prstr m ++ " does not export " ++ nstr n)
checkerError (OtherError l str)     = (l,str)

nameNotFound n                      = Control.Exception.throw $ NameNotFound n
nameReserved n                      = Control.Exception.throw $ NameReserved n
nameBlocked n                       = Control.Exception.throw $ NameBlocked n
illegalRedef n                      = Control.Exception.throw $ IllegalRedef n
missingSelf n                       = Control.Exception.throw $ MissingSelf n
fileNotFound n                      = Control.Exception.throw $ FileNotFound n
illegalImport l                     = Control.Exception.throw $ IllegalImport l
duplicateImport n                   = Control.Exception.throw $ DuplicateImport n
noItem m n                          = Control.Exception.throw $ NoItem m n
err l s                             = Control.Exception.throw $ OtherError l s

err1 x s                            = err (loc x) (s ++ " " ++ prstr x)
err2 (x:_) s                        = err1 x s

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

