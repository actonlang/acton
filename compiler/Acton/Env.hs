{-# LANGUAGE FlexibleInstances, DeriveGeneric #-}
module Acton.Env where

import qualified Control.Exception
import Debug.Trace
import qualified Data.Binary
import GHC.Generics (Generic)
import Data.Typeable
import Data.Traversable
import System.FilePath.Posix (joinPath)
import System.Directory (doesFileExist)
import Control.Monad

import Acton.Syntax
import Acton.Builtin
import Acton.Printer
import Acton.Names
import Acton.TypeM
import Utils
import Pretty
import InterfaceFiles
-- import Prelude hiding ((<>))




mkEnv                       :: (FilePath,FilePath) -> Env -> Module -> IO Env
mkEnv paths env modul       = getImps paths env imps
  where Module _ imps _     = modul


type TEnv                   = [(Name, NameInfo)]

data Env                    = Env { names :: TEnv, modules :: [(ModName,TEnv)], defaultmod :: ModName, selfbound :: Maybe TCon }

data NameInfo               = NVar      TSchema
                            | NSVar     TSchema
                            | NSig      TSchema
                            | NClass    [TBind] [TCon] TEnv
                            | NProto    [TBind] [TCon] TEnv
                            | NExt      [TBind] [TCon] TEnv           -- no support for qualified NExt names yet...
                            | NTVar     [TCon]
                            | NPAttr    QName         -- protocol attribute (global), arg points to owning protocol
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

nProto                      :: Name -> [TBind] -> [TCon] -> TEnv -> TEnv
nProto n q us te            = [(n, NProto q us te)]


nVars                       :: TEnv -> [(Name, TSchema)]
nVars te                    = [ (n,sc) | (n, NVar sc) <- te ]

mapVars                     :: (TSchema -> TSchema) -> TEnv -> TEnv
mapVars f te                = map g te
  where g (n, NVar sc)      = (n, NVar (f sc))
        g (n, i)            = (n, i)


instance Data.Binary.Binary NameInfo

instance Pretty TEnv where
    pretty tenv                 = vcat (map pretty tenv)

instance Pretty Env where
    pretty env                  = vcat (map pretty (names env))

instance Pretty (Name,NameInfo) where
    pretty (n, NVar t)          = prettySig [n] t
    pretty (n, NSVar t)         = text "var" <+> pretty n <+> colon <+> pretty t
    pretty (n, NClass q us te)  = text "class" <+> pretty n <+> nonEmpty brackets commaList q <+>
                                  nonEmpty parens commaList us <> colon $+$ (nest 4 $ pretty te)
    pretty (n, NProto q us te)  = text "protocol" <+> pretty n <+> nonEmpty brackets commaList q <+>
                                  nonEmpty parens commaList us <> colon $+$ (nest 4 $ pretty te)
    pretty (n, NExt q us te)    = text "extension" <+> pretty n <+> nonEmpty brackets commaList q <+>
                                  nonEmpty parens commaList us <> colon $+$ (nest 4 $ pretty te)
    pretty (n, NTVar us)        = pretty n <> parens (commaList us)
    pretty (n, NPAttr qn)       = dot <> pretty n <+> equals <+> pretty qn <> dot <> pretty n
    pretty (n, NAlias qn)       = text "alias" <+> pretty n <+> equals <+> pretty qn
    pretty (n, NMAlias m)       = text "module" <+> pretty n <+> equals <+> pretty m
    pretty (n, NModule te)      = text "module" <+> pretty n <> colon $+$ nest 4 (pretty te)
    pretty (n, NSig t)          = pretty n <+> text "(reserved)" <+> colon <+> pretty t
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
    msubst (NExt q us te)       = NExt <$> msubst q <*> msubst us <*> msubst te
    msubst (NTVar us)           = NTVar <$> msubst us
    msubst (NPAttr qn)          = NPAttr <$> return qn
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
    tyfree (NExt q us te)       = (tyfree q ++ tyfree us ++ tyfree te) \\ tybound q
    tyfree (NTVar us)           = tyfree us
    tyfree (NPAttr qn)          = []
    tyfree (NAlias qn)          = []
    tyfree (NMAlias qn)         = []
    tyfree (NModule te)         = []        -- actually tyfree te, but a module has no free variables on the top level
    tyfree NReserved            = []
    tyfree NBlocked             = []

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
                                        _ -> err1 (ModName (reverse (n:pre))) "Not a module prefix:"

instance Unalias QName where
    unalias env (QName m n)         = case lookup m' (modules env) of
                                         Just te -> case lookup n te of
                                                      Just (NAlias qn) -> qn
                                                      Just _ -> QName m' n
                                                      _ -> noItem m n
      where m'                      = unalias env m
    unalias env (NoQual n)          = QName (defaultmod env) n
                                    
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
    unalias env (NExt q us te)      = NExt (unalias env q) (unalias env us) (unalias env te)
    unalias env (NTVar us)          = NTVar (unalias env us)
    unalias env (NPAttr qn)         = NPAttr (unalias env qn)
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
                                (nStr,              NClass [] [] []),
                                (nRef,              NClass [] [] []),
                                (nMsg,              NClass [a] [] []),
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
                                (nContextManager,   NProto [] [] [])
                              ]
  where 
    a:b:c:_                 = [ TBind v [] | v <- tvarSupply ]
    bounded u (TBind v us)  = TBind v (u:us)
    ta:tb:tc:_              = [ TVar NoLoc v | v <- tvarSupply ]

envActorSelf                = [ (nSelf,     NVar (monotype tRef)) ]

--------------------------------------------------------------------------------------------------------------------

prune xs                    = filter ((`notElem` xs) . fst)

initEnv                     :: Env
initEnv                     = define autoImp $ defineMod mBuiltin $ addMod mBuiltin envBuiltin env0
  where autoImp             = importAll mBuiltin envBuiltin
        env0                = Env{ names = [], modules = [], defaultmod = mBuiltin, selfbound = Nothing }

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
defineTVars q env           = env{ names = [ (n, NTVar us) | TBind (TV n) us <- q ] }

defineSelf                  :: Name -> [TBind] -> Env -> Env
defineSelf n q env          = env{ selfbound = Just tc }
  where tc                  = TC (NoQual n) [ tVar tv | TBind tv _ <- q ]

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

findName                    :: Name -> Env -> NameInfo
findName n env              = case lookup n (names env) of
                                Just (NAlias qn) -> findQName qn env
                                Just NReserved -> nameReserved n
                                Just (NSig t) -> nameReserved n
                                Just NBlocked -> nameBlocked n
                                Just info -> info
                                Nothing -> trace ("### names:\n" ++ render (vcat $ map pretty (names env))) $ nameNotFound n

findQName                   :: QName -> Env -> NameInfo
findQName (QName m n) env   = case lookup n (findMod (unalias env m) env) of
                                Just (NAlias qn) -> findQName qn env
                                Just i -> i
                                _ -> noItem m n
findQName (NoQual n) env    = findName n env

findSelf env                = case selfbound env of
                                Just u -> u
                                Nothing -> error "(internal) Self not in scope"

assignableType              :: Name -> Env -> Type
assignableType n env        = case findType n env of
                                TSchema _ [] t _ -> t
                                -- TODO: report error.......


findMod                     :: ModName -> Env -> TEnv
findMod m env               = case lookup m (modules env) of
                                Just te -> te

addMod                      :: ModName -> TEnv -> Env -> Env
addMod m te env             = env{ modules = (m,te) : modules env }


dropNames                   :: Env -> Env
dropNames env               = env{ names = names initEnv }

findClassOrProto            :: QName -> Env -> (Bool,[TBind],[TCon],TEnv)
findClassOrProto n env      = case findQName n env of
                                NClass q us te -> (False,q,us,te)
                                NProto q us te -> (True,q,us,te)
                                _ -> err1 n "Class or protocol name expected, got"

findClass                   :: QName -> Env -> ([TBind],[TCon],TEnv)
findClass n env             = case findQName n env of
                                NClass q us te -> (q,us,te)
                                _ -> err1 n "Class name expected, got"

findProto                   :: QName -> Env -> ([TBind],[TCon],TEnv)
findProto n env             = case findQName n env of
                                NProto q us te -> (q,us,te)
                                _ -> err1 n "Protocol name expected, got"

findType                    :: Name -> Env -> TSchema
findType n env              = case findName n env of
                                NVar t       -> t
                                NSVar t      -> t
                                NClass q _ _ -> tSchema q (tAt $ TC (NoQual n) $ map tVar $ tybound q)
                                _            -> err1 n "Unexpected name..."

newTEnv vs                  = do ts <- newTVars (length vs)
                                 return $ vs `zip` [ NVar (monotype t) | t <- ts ]


-- Instantiation -------------------------------------------------------------------------

instantiate env (TSchema _ [] t _)
                                = instwild t
instantiate env (TSchema _ q t _)
                                = do tvs <- newTVars (length q)
                                     let s = tybound q `zip` tvs
                                     constrain $ constraintsOf (subst s q) env
                                     instwild (subst s t)

constraintsOf q env             = [ constraint env t u | TBind v us <- q, let t = tVar v, u <- us ]

constraint env t u@(TC n _)     = case findQName n env of
                                    NClass{} -> Sub t (tCon u)
                                    NProto{} -> Impl t u

class Wild a where
    instwild                    :: a -> TypeM a
    instwild a                  = return a
    wildloc                     :: a -> [SrcLoc]
    wildloc a                   = []

instance (Wild a) => Wild [a] where
    instwild                    = mapM instwild
    wildloc                     = concatMap wildloc

instance Wild TSchema where
    instwild (TSchema l q t d)
      | nowild q                = TSchema l q <$> instwild t <*> return d
    wildloc (TSchema _ q t d)
      | nowild q                = wildloc t

instance Wild TBind where
    instwild (TBind tv us)      = TBind tv <$> instwild us
    wildloc (TBind _ us)        = wildloc us

instance Wild TCon where
    instwild (TC c ts)          = TC c <$> instwild ts
    wildloc (TC _ ts)           = wildloc ts

instance Wild Type where
    instwild (TWild _)          = newTVar
    instwild (TCon l tc)        = TCon l <$> instwild tc
    instwild (TAt l tc)         = TAt l <$> instwild tc
    instwild (TFun l e p k t)   = TFun l <$> instwild e <*> instwild p <*> instwild k <*> instwild t
    instwild (TTuple l p)       = TTuple l <$> instwild p
    instwild (TRecord l k)      = TRecord l <$> instwild k
    instwild (TOpt l t)         = TOpt l <$> instwild t
    instwild (TRow l n t r)     = TRow l n <$> instwild t <*> instwild r
    instwild t                  = return t
    
    wildloc (TWild l)           = [l]
    wildloc (TCon _ tc)         = wildloc tc
    wildloc (TAt _ tc)          = wildloc tc
    wildloc (TFun _ e p k t)    = wildloc e ++ wildloc p ++ wildloc k ++ wildloc t
    wildloc (TTuple _ p)        = wildloc p
    wildloc (TRecord _ k)       = wildloc k
    wildloc (TOpt _ t)          = wildloc t
    wildloc (TRow _ _ t r)      = wildloc t ++ wildloc r
    wildloc t                   = []

nowild x
  | null ls                     = True
  | otherwise                   = err1 (head ls) "Illegal wildcard type"
  where ls                      = wildloc x


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
    unif (NExt q us te) (NExt q' us' te') 
                                        = unifC q us te q' us' te'
    unif _ _                            = err1 v "Inconsistent bindings for"

    unifT (TSchema _ [] t d) (TSchema _ [] t' d')
      | d == d'                         = constrain [Equ t t']
    unifT t t'                          = constrain [EquGen t t']
    
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
    imp (n, NExt _ _ _)     = Nothing                               -- <<<<<<<<<<<<<<<<<<<<<<<< to be returned to!
    imp (n, NAlias _)       = Just (n, NAlias (QName m n))
    imp (n, NVar t)         = Just (n, NVar t)
    imp _                   = Nothing                               -- cannot happen


-- Error handling ------------------------------------------------------------------------

data CheckerError                       = FileNotFound ModName
                                        | NameNotFound Name
                                        | NameReserved Name
                                        | NameBlocked Name
                                        | IllegalRedef Name
                                        | IllegalImport SrcLoc
                                        | DuplicateImport Name
                                        | NoItem ModName Name
                                        | OtherError SrcLoc String
                                        deriving (Show)

instance Control.Exception.Exception CheckerError

checkerError (FileNotFound n)           = (loc n, " Type interface file not found for " ++ prstr n)
checkerError (NameNotFound n)           = (loc n, " Name " ++ prstr n ++ " is not in scope")
checkerError (NameReserved n)           = (loc n, " Name " ++ prstr n ++ " is reserved but not yet defined")
checkerError (NameBlocked n)            = (loc n, " Name " ++ prstr n ++ " is currently not accessible")
checkerError (IllegalRedef n)           = (loc n, " Illegal redefinition of " ++ prstr n)
checkerError (IllegalImport l)          = (l,     " Relative import not yet supported")
checkerError (DuplicateImport n)        = (loc n, " Duplicate import of name " ++ prstr n)
checkerError (NoItem m n)               = (loc n, " Module " ++ prstr m ++ " does not export " ++ nstr n)
checkerError (OtherError l str)         = (l,str)

nameNotFound n                          = Control.Exception.throw $ NameNotFound n

nameReserved n                          = Control.Exception.throw $ NameReserved n

nameBlocked n                           = Control.Exception.throw $ NameBlocked n

illegalRedef n                          = Control.Exception.throw $ IllegalRedef n

fileNotFound n                          = Control.Exception.throw $ FileNotFound n

illegalImport l                         = Control.Exception.throw $ IllegalImport l

duplicateImport n                       = Control.Exception.throw $ DuplicateImport n

noItem m n                              = Control.Exception.throw $ NoItem m n

err l s                                 = Control.Exception.throw $ OtherError l s

err1 x s                                = err (loc x) (s ++ " " ++ prstr x)

err2 (x:_) s                            = err1 x s

