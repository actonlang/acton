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
import Prelude hiding ((<>))




mkEnv                       :: (FilePath,FilePath) -> Env -> Module -> IO Env           -- ActonCompiler...
mkEnv paths env modul       = getImps paths (setDefaultMod m env) imps
  where Module m imps _     = modul


type Schemas                = [(Name, TSchema)]

type TEnv                   = [(Name, NameInfo)]

data Env                    = Env { names :: TEnv, modules :: [(ModName,TEnv)], defaultmod :: ModName, nocheck :: Bool } deriving Show

data NameInfo               = NVar      TSchema
                            | NSVar     TSchema
                            | NSig      TSchema
                            | NClass    [TBind] [TCon] TEnv
                            | NProto    [TBind] [TCon] TEnv
                            | NExt      QName [TBind] [TCon]
                            | NTVar     Kind [TCon]
                            | NAlias    QName
                            | NMAlias   ModName
                            | NModule   TEnv
                            | NReserved
                            | NBlocked
                            deriving (Eq,Show,Read,Generic)

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
    pretty (n, NTVar k us)      = pretty n <> parens (commaList us)
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
    msubst (NTVar k us)         = NTVar k <$> msubst us
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
    tyfree (NTVar k us)         = tyfree us
    tyfree (NAlias qn)          = []
    tyfree (NMAlias qn)         = []
    tyfree (NModule te)         = []        -- actually tyfree te, but a module has no free variables on the top level
    tyfree NReserved            = []
    tyfree NBlocked             = []

msubstTV tvs                    = fmap tyfree $ mapM msubst $ map tVar tvs                  -- splitGen, genTEnv, Solver.redGen

instance Subst SrcInfoTag where
    msubst (GEN l t)                = GEN l <$> msubst t
    msubst (INS l t)                = INS l <$> msubst t

    tyfree (GEN _ t)                = tyfree t
    tyfree (INS _ t)                = tyfree t


-------------------------------------------------------------------------------------------------------------------

class Unalias a where
    unalias                         :: Env -> a -> a                                        -- reconstruct, Env.findQName
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
                                        Just (NMAlias m) -> m
                                        _ -> error ("### unalias " ++ show (ModName ns))

instance Unalias QName where
    unalias env (QName m n)         = case lookup m' (modules env) of
                                        Just te -> case lookup n te of
                                                      Just (NAlias qn) -> qn
                                                      Just _ -> QName m' n
      where m'                      = unalias env m
    unalias env (NoQual n)          = case lookup n (names env) of
                                        Just (NAlias qn) -> qn
                                        Just _ -> QName (defaultmod env) n
                                        _ -> error ("### unalias " ++ prstr n)
                                    
instance Unalias TSchema where
    unalias env (TSchema l q t d)   = TSchema l (unalias env q) (unalias env t) d

instance Unalias TCon where
    unalias env (TC qn ts)          = TC (unalias env qn) (unalias env ts)

instance Unalias TBind where
    unalias env (TBind tv cs)       = TBind tv (unalias env cs)

instance Unalias Type where
    unalias env (TCon l c)          = TCon l (unalias env c)
    unalias env (TExist l p)        = TExist l (unalias env p)
    unalias env (TFun l e p r t)    = TFun l (unalias env e) (unalias env p) (unalias env r) (unalias env t)
    unalias env (TTuple l p k)      = TTuple l (unalias env p) (unalias env k)
    unalias env (TOpt l t)          = TOpt l (unalias env t)
    unalias env (TRow l k n t r)    = TRow l k n (unalias env t) (unalias env r)
    unalias env t                   = t

instance Unalias NameInfo where
    unalias env (NVar t)            = NVar (unalias env t)
    unalias env (NSVar t)           = NSVar (unalias env t)
    unalias env (NSig t)            = NSig (unalias env t)
    unalias env (NClass q us te)    = NClass (unalias env q) (unalias env us) (unalias env te)
    unalias env (NProto q us te)    = NProto (unalias env q) (unalias env us) (unalias env te)
    unalias env (NExt n q us)       = NExt n (unalias env q) (unalias env us)
    unalias env (NTVar k us)        = NTVar k (unalias env us)
    unalias env (NAlias qn)         = NAlias (unalias env qn)
    unalias env (NModule te)        = NModule (unalias env te)
    unalias env NReserved           = NReserved
    unalias env NBlocked            = NBlocked

instance Unalias (Name,NameInfo) where
    unalias env (n,i)               = (n, unalias env i)
    
-- TEnv construction helpers and queries ---------------------------------------------------------------------------------------
    
nTVars                      :: [TBind] -> TEnv                              -- infEnv (ExceptAs), Env.defineTVars, Env.defineSelf'
nTVars q                    = [ (n, NTVar k us) | TBind (TV k n) us <- q ]

nVars                       :: TEnv -> Schemas                              -- dump INS/GEN, gen1, infEnv Data, env2row (Actor), checkBindings (inherited,undefs)
nVars te                    = [ (n,sc) | (n, NVar sc) <- te ]

nSigs                       :: TEnv -> Schemas                              -- checkBindings (inherited,refinements,unsigs,allsigs)
nSigs te                    = [ (n,sc) | (n, NSig sc) <- te ]
                                

-- Env construction and modification -------------------------------------------------------------------------------------------

initEnv                     :: Env                                          -- ActonCompiler..., Env.dropNames
initEnv                     = define autoImp env0
  where autoImp             = importAll mBuiltin envBuiltin
        env0                = Env{ names = [(nBuiltin,NModule envBuiltin)], modules = [(mBuiltin,envBuiltin)], defaultmod = mBuiltin, nocheck = False }

setDefaultMod               :: ModName -> Env -> Env                        -- Env.mkEnv
setDefaultMod m env         = env{ defaultmod = m }

setNoCheck                  :: Env -> Env                                   -- infEnv (Decl)
setNoCheck env              = env{ nocheck = True }

addMod                      :: ModName -> TEnv -> Env -> Env                -- Env.doImp, ActonCompiler.doTask
addMod m te env             = env{ modules = (m,te) : modules env }

dropNames                   :: Env -> Env                                   -- ActonCompiler.runRestPasses
dropNames env               = env{ names = names initEnv }


reserve                     :: [Name] -> Env -> Env                         -- infEnv (class,proto,ext), check (actor,def,class,proto,ext)
reserve xs env              = env{ names = [ (x, NReserved) | x <- nub xs ] ++ names env }  -- infer (lambda), infEnv (CompFor), reconstruct

block                       :: [Name] -> Env -> Env                         -- infEnv (class,proto,ext), check (actor,def,class,proto,ext)
block xs env                = env{ names = [ (x, NBlocked) | x <- nub xs ] ++ names env }

define                      :: TEnv -> Env -> Env                           -- all local scopes...
define te env               = env{ names = reverse te ++ prune (dom te) (names env) }

defineTVars                 :: [TBind] -> Env -> Env                        -- infEnv (class,proto,ext), check (actor,def,class,proto,ext), Solver.redGen'
defineTVars q env           = env{ names = nTVars q ++ names env }

defineSelf                  :: QName -> [TBind] -> Env -> Env               -- infEnv (class,proto,ext), check (class,proto,ext)
defineSelf qn q env         = define (nTVars [TBind tvSelf [tc]]) env
  where tc                  = TC qn [ tVar tv | TBind tv _ <- q ]


defineMod                   :: ModName -> TEnv -> Env -> Env                -- Env.impModule, Env.doImp
defineMod m te env          = define [(n, defmod ns $ te1)] env
  where ModName (n:ns)      = m
        te1                 = case lookup n (names env) of Just (NModule te1) -> te1; _ -> []
        defmod [] te1       = NModule $ te
        defmod (n:ns) te1   = NModule $ (n, defmod ns te2) : prune [n] te1
          where te2         = case lookup n te1 of Just (NModule te2) -> te2; _ -> []


-- General Env queries -----------------------------------------------------------------------------------------------------------

noCheck                     :: Env -> Bool                                  -- infEnv (Decl)
noCheck env                 = nocheck env

stateScope                  :: Env -> [Name]                                -- infEnv (class,proto,ext), check (actor,def,class,proto,ext)
stateScope env              = [ z | (z, NSVar _) <- names env ]

tvarScope                   :: Env -> [TVar]                                -- extractSchema
tvarScope env               = [ TV k n | (n, NTVar k _) <- names env ]


-- Name queries -------------------------------------------------------------------------------------------------------------------

reserved                    :: Name -> Env -> Bool                          -- infEnv (signature,class,proto)
reserved n env              = case lookup n (names env) of
                                Just NReserved -> True
                                _              -> False

reservedOrSig               :: Name -> Env -> Bool                          -- infEnv (actor,def,PVar)
reservedOrSig n env         = case lookup n (names env) of
                                Just NReserved -> True
                                Just (NSig t)  -> True
                                _              -> False

findName                    :: Name -> Env -> NameInfo                      -- Env (findQName,findSubBound,findImplBound)
findName n env              = case lookup n (names env) of
                                Just (NAlias qn) -> findQName qn env
                                Just NReserved -> nameReserved n
                                Just (NSig t) -> nameReserved n
                                Just NBlocked -> nameBlocked n
                                Just info -> info
                                Nothing -> nameNotFound n

findQName                   :: QName -> Env -> NameInfo                     -- Env (findName,tconKind,isProto,findClass,findProto,findVarType,findCon0)
findQName (QName m n) env   = case lookup n (fromJust $ maybeFindMod (unalias env m) env) of
                                Just (NAlias qn) -> findQName qn env
                                Just i -> i
                                _ -> noItem m n
findQName (NoQual n) env    = findName n env


maybeFindMod                :: ModName -> Env -> Maybe TEnv                 -- Env (findQName,isMod)
maybeFindMod (ModName ns) env = f ns (names env)
  where f [] te             = Just te
        f (n:ns) te         = case lookup n te of
                                Just (NModule te') -> f ns te'
                                Just (NMAlias m) -> maybeFindMod m env
                                Nothing -> Nothing                         

isMod                       :: Env -> [Name] -> Bool
isMod env ns                = maybe False (const True) (maybeFindMod (ModName ns) env)      -- isModule


findVarType                 :: QName -> Env -> TSchema                                      -- infer (Var,TaVar), infEnv (after), checkAssump, infEnvT (PVar)
findVarType n env           = case findQName n env of
                                NVar t         -> t
                                NSVar t        -> t
                                NSig t         -> t
                                _              -> internal (loc n) ("Unexpected variable name: " ++ prstr n)


tconKind                    :: QName -> Env -> Kind                                         -- Kinds.tconKind
tconKind n env              = case findQName n env of
                                NClass q _ _ -> kind KType q
                                NProto q _ _ -> kind KProto q
                                _            -> notClassOrProto n
  where kind k []           = k
        kind k q            = KFun [ tvkind v | TBind v _ <- q ] k
                                
isProto                     :: QName -> Env -> Bool                                         -- Env (findSubBound,findImplBound,constraintsOf),
isProto n env               = case findQName n env of                                       -- infEnv (ext), class/protoBases
                                NProto q us te -> True
                                _ -> False

locateWitness               :: Env -> QName -> [TBind] -> [TCon] -> Name                    -- check (ext)
locateWitness env n q us    = case [ w | (w, NExt n' q' us') <- names env, (n,q,us) == (n',q',us') ] of
                                [w] -> w


-- TCon queries ------------------------------------------------------------------------------------------------------------------

findSubAxiom                :: Env -> TCon -> QName -> Maybe (Bool,Type)                    -- Solver.red (TCon/TCon), infException
findSubAxiom env c n
  | null hits               = Nothing 
  | otherwise               = Just (proto, tCon $ head hits)
  where (proto,cs,us,_)     = findCon env c
        hits                = [ u | u <- us, tcname u == n ]

findAttr                    :: Env -> TCon -> Name -> (Constraints, TSchema)                -- Solver.reduce (Sel/TCon,Sel/TExists,Mut/TCon)
findAttr env u n            = (cs, findIn (te ++ concat tes))
  where (_,cs,us,te)        = findCon env u
        tes                 = [ te' | u' <- us, let (_,_,_,te') = findCon env u' ]
        findIn te1          = case lookup n te1 of
                                Just (NVar t)         -> t
                                Just (NSVar t)        -> t
                                Just (NSig t)         -> t
                                Nothing               -> err1 n "Attribute not found:"

findCon                     :: Env -> TCon -> (Bool,Constraints,[TCon],TEnv)                -- mro, checkBindings, Env.findSubAxiom, Env.findAttr
findCon env (TC n ts)
  | map tVar tvs == ts      = (proto, constraintsOf env q, us, te)
  | otherwise               = (proto, constraintsOf env (subst s q), subst s us, subst s te)
  where (proto,q,us,te)     = case findQName n env of
                                NClass q us te -> (False,q,us,te)
                                NProto q us te -> (True,q,us,te)
                                _ -> err1 n "Class or protocol name expected, got"
        tvs                 = tybound q
        s                   = tvs `zip` ts


-- TVar queries ------------------------------------------------------------------------------------------------------------------

findSubBound                :: TVar -> Env -> Maybe TCon                                    -- Solver.reduce (Sel,Mut)
findSubBound tv env         = case findName (tvname tv) env of
                                NTVar _ (u:us) | not $ isProto (tcname u) env -> Just u
                                _ -> Nothing

findImplBound               :: TVar -> Env -> [TCon]                                        -- Solver.reduce ...
findImplBound tv env        = case findName (tvname tv) env of
                                NTVar _ (u:us) | isProto (tcname u) env -> u:us
                                               | otherwise -> us
                                _ -> []

-- Instantiation -------------------------------------------------------------------------------------------------------------------

instantiate                 :: Env -> TSchema -> TypeM (Constraints, Type)                  -- Solver.reduce (Sel/TCon,Sel/TExists,Mut/TCon)
instantiate env (TSchema _ [] t _)
                            = return ([], t)
instantiate env (TSchema _ q t _)
                            = do tvs <- newTVars [ tvkind v | TBind v _ <- q ]
                                 let s = tybound q `zip` tvs
                                 return (constraintsOf env (subst s q), subst s t)

constraintsOf               :: Env -> [TBind] -> Constraints                                -- Env (findCon,instantiate)
constraintsOf env q         = [ constr t u | TBind v us <- q, let t = tVar v, u <- us ]
  where constr t u@(TC n _)
          | isProto n env   = Impl env t u
          | otherwise       = Sub env t (tCon u)



-- Environment unification ----------------------------------------------------------------------------------------------------------

unifyTEnv                               :: Env -> [TEnv] -> [Name] -> TypeM ()              -- commonTEnv
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


-- Builtin env -----------------------------------------------------------------------------------------------------------------

envBuiltin                  = [ (nSequence,         NProto [a] [] []),                          -- Env (initEnv)
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
                                (nMsg,              NClass [a] [] []),
                                (nException,        NClass [] [] []),
                                (nBoolean,          NProto [] [] []),
                                (nIndexed,          NProto [a,b] [] []),
                                (nSliceable,        NProto [] [] []),
                                (nHashable,         NProto [] [] []),
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
                                (nStopIteration,    NClass [] [cException] []),
                                (nValueError,       NClass [] [cException] []),
                                (nShow,             NProto [] [] []),
                                (nLen,              NVar (tSchema [a] $ tFun0 [pCollection ta] tInt)),
                                (nPrint,            NVar (tSchema [bounded cShow r] $ tFun fxNil tr kwdNil tNone)),
                                (nDict,             NClass [a,b] [] []),
                                (nList,             NClass [a] [] []),
                                (nSet',             NClass [a] [] [])
                              ]
  where 
    a:b:c:_                 = map tBind tvarSupply
    ta:tb:tc:_              = map tVar tvarSupply
    r:_                     = map tBind prowSupply
    tr:_                    = map tVar prowSupply
    bounded u (TBind v us)  = TBind v (u:us)


-- Import handling (local definitions only) ----------------------------------------------

getImps                         :: (FilePath,FilePath) -> Env -> [Import] -> IO Env
getImps ps env []               = return env
getImps ps env (i:is)           = do env' <- impModule ps env i
                                     getImps ps env' is


impModule                       :: (FilePath,FilePath) -> Env -> Import -> IO Env
impModule ps env (Import _ ms)  = imp env ms
  where imp env []              = return env
        imp env (ModuleItem m as : is)
                                = do (env1,te) <- doImp ps env m
                                     let env2 = maybe (defineMod m te env1) (\n->define [(n, NMAlias m)] env1) as
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
                                                 return (defineMod m te (addMod m te env), te)
                                         else do found <- doesFileExist fpath2
                                                 unless found (fileNotFound m)
                                                 te <- InterfaceFiles.readFile fpath2
                                                 return (defineMod m te (addMod m te env), te)
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
    imp (n, NVar t)         = Just (n, NAlias (QName m n))
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
substitute tv@(TV _ Internal{}) t       = state $ \st -> ((), st{ currsubst = Map.insert tv t (currsubst st)})

getSubstitution                         :: TypeM (Map TVar Type)
getSubstitution                         = state $ \st -> (currsubst st, st)

dump                                    :: SrcInfo -> TypeM ()
dump inf                                = state $ \st -> ((), st{ dumped = inf ++ dumped st })

getDump                                 :: TypeM SrcInfo
getDump                                 = state $ \st -> (dumped st, st)


newName s                               = Internal s <$> newUnique <*> return TypesPass

newTVarOfKind k                         = TVar NoLoc <$> TV k <$> (Internal (str k) <$> newUnique <*> return GenPass)
  where str KType                       = "V"
        str XRow                        = "X"
        str PRow                        = "P"
        str KRow                        = "R"
        str _                           = "C"

newTVars ks                             = mapM newTVarOfKind ks

newTVar                                 = newTVarOfKind KType

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

msubstRenaming                      :: Subst a => a -> TypeM (Substitution,Substitution)
msubstRenaming c                    = do s <- Map.toList . Map.filterWithKey relevant <$> getSubstitution
                                         return $ (dom s `zip` subst (renaming_s  (tyfree (rng s))) (rng s),renaming_s (tyfree (rng s)))
      where relevant k _            = k `elem` vs0
            vs0                     = tyfree c
            vs                      = tybound c
            renaming_s newvars      = clashvars `zip` map tVar freshvars
              where clashvars       = vs `intersect` newvars
                    avoidvars       = vs0 ++ vs ++ newvars
                    freshvars       = tvarSupply \\ avoidvars

testSchemaSubst = do
    putStrLn ("t:  " ++ render (pretty t))
    putStrLn ("s1: " ++ render (pretty s1))
    putStrLn ("s2: " ++ render (pretty s2))
    putStrLn ("s3: " ++ render (pretty s3))
    putStrLn ("subst s1 t: " ++ render (pretty (subst s1 t)))
    putStrLn ("subst s2 t: " ++ render (pretty (subst s2 t)))
    putStrLn ("subst s3 t: " ++ render (pretty (subst s3 t)))
  where t   = tSchema [TBind (TV KType (name "A")) [TC (noQual "Eq") []]]
                            (tCon (TC (noQual "apa") [tVar (TV KType (name "A")), 
                                                      tVar (TV KType (name "B"))]))
        s1  = [(TV KType (name "B"), tSelf)]
        s2  = [(TV KType (name "A"), tSelf)]
        s3  = [(TV KType (name "B"), tVar (TV KType (name "A")))]

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
                                            Just t ->  msubst t
                                            Nothing -> return (TVar l v)
    msubst (TCon l c)               = TCon l <$> msubst c
    msubst (TExist l p)             = TExist l <$> msubst p
    msubst (TFun l fx p k t)        = TFun l <$> msubst fx <*> msubst p <*> msubst k<*> msubst t
    msubst (TTuple l p k)           = TTuple l <$> msubst p <*> msubst k
    msubst (TUnion l as)            = return $ TUnion l as
    msubst (TOpt l t)               = TOpt l <$> msubst t
    msubst (TNone l)                = return $ TNone l
    msubst (TWild l)                = return $ TWild l
    msubst (TNil l s)               = return $ TNil l s
    msubst (TRow l k n t r)         = TRow l k n <$> msubst t <*> msubst r

    tyfree (TVar _ v)               = [v]
    tyfree (TCon _ c)               = tyfree c
    tyfree (TExist _ p)             = tyfree p
    tyfree (TFun _ fx p k t)        = tyfree fx ++ tyfree p ++ tyfree k ++ tyfree t
    tyfree (TTuple _ p k)           = tyfree p ++ tyfree k
    tyfree (TUnion _ as)            = []
    tyfree (TOpt _ t)               = tyfree t
    tyfree (TNone _)                = []
    tyfree (TWild _)                = []
    tyfree (TNil _ _)               = []
    tyfree (TRow _ _ _ t r)         = tyfree t ++ tyfree r
 
instance Subst PosPar where
    msubst (PosPar n t e p)         = PosPar n <$> msubst t <*> msubst e <*> msubst p
    msubst (PosSTAR n t)            = PosSTAR n <$> msubst t
    msubst PosNIL                   = return PosNIL
    
    tyfree (PosPar n t e p)         = tyfree t ++ tyfree p
    tyfree (PosSTAR n t)            = tyfree t
    tyfree PosNIL                   = []

instance Subst KwdPar where
    msubst (KwdPar n t e p)         = KwdPar n <$> msubst t <*> msubst e <*> msubst p
    msubst (KwdSTAR n t)            = KwdSTAR n <$> msubst t
    msubst KwdNIL                   = return KwdNIL
    
    tyfree (KwdPar n t e p)         = tyfree t ++ tyfree p
    tyfree (KwdSTAR n t)            = tyfree t
    tyfree KwdNIL                   = []

instance Subst Expr where
    msubst e                        = return e
    tyfree e                        = []

instance Subst Decl where
    msubst p@(Protocol l n qs bs ss)= do (s,ren) <- msubstRenaming p
                                         return $ Protocol l n (subst s (subst ren qs)) (subst s (subst ren bs)) (subst s (subst ren ss))
    msubst c@(Class l n qs bs ss)   = do (s,ren) <- msubstRenaming c
                                         return $ Class l n (subst s (subst ren qs)) (subst s (subst ren bs)) (subst s (subst ren ss))

    tybound (Protocol l n qs bs ss) = tybound qs
    tybound (Class l n qs bs ss)    = tybound qs

    tyfree (Protocol l n qs bs ss)  = nub (tyfree qs ++ tyfree bs ++ tyfree ss) \\ tybound qs
    tyfree (Class l n qs bs ss)     = nub (tyfree qs ++ tyfree bs ++ tyfree ss) \\ tybound qs
    
instance Subst Stmt where
    msubst (Decl l ds)              = Decl l <$> msubst ds
    msubst (Signature l ns tsc)     = Signature l ns <$> msubst tsc
    msubst s                        = return s

    tyfree (Decl l ds)              = tyfree ds
    tyfree (Signature l ns tsc)     = tyfree tsc
    tyfree s                        = []


    
-- Error handling ------------------------------------------------------------------------

data CheckerError                   = FileNotFound ModName
                                    | NameNotFound Name
                                    | NameReserved Name
                                    | NameBlocked Name
                                    | TypedReassign Pattern
                                    | IllegalRedef Name
                                    | MissingSelf Name
                                    | IllegalImport SrcLoc
                                    | DuplicateImport Name
                                    | NoItem ModName Name
                                    | NoClassOrProto QName
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
checkerError (TypedReassign p)      = (loc p, " Type annotation on reassignment: " ++ prstr p)
checkerError (IllegalRedef n)       = (loc n, " Illegal redefinition of " ++ prstr n)
checkerError (MissingSelf n)        = (loc n, " Missing 'self' parameter in definition of")
checkerError (IllegalImport l)      = (l,     " Relative import not yet supported")
checkerError (DuplicateImport n)    = (loc n, " Duplicate import of name " ++ prstr n)
checkerError (NoItem m n)           = (loc n, " Module " ++ prstr m ++ " does not export " ++ nstr n)
checkerError (NoClassOrProto n)     = (loc n, " Class or protocol name expected, got " ++ prstr n)
checkerError (OtherError l str)     = (l,str)

nameNotFound n                      = Control.Exception.throw $ NameNotFound n
nameReserved n                      = Control.Exception.throw $ NameReserved n
nameBlocked n                       = Control.Exception.throw $ NameBlocked n
typedReassign p                     = Control.Exception.throw $ TypedReassign p
illegalRedef n                      = Control.Exception.throw $ IllegalRedef n
missingSelf n                       = Control.Exception.throw $ MissingSelf n
fileNotFound n                      = Control.Exception.throw $ FileNotFound n
illegalImport l                     = Control.Exception.throw $ IllegalImport l
duplicateImport n                   = Control.Exception.throw $ DuplicateImport n
noItem m n                          = Control.Exception.throw $ NoItem m n
notClassOrProto n                   = Control.Exception.throw $ NoClassOrProto n
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

