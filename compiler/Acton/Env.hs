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

data Env                    = Env { names :: [(Name, Maybe NameInfo)], modules :: [(ModName,TEnv)], defaultmod :: ModName }

data NameInfo               = NVar    TSchema Decoration
                            | NSVar   Type
                            | NClass  [TBind] [TCon] TEnv
                            | NProto  [TBind] [TCon] TEnv
                            | NExt    [TBind] [TCon] TEnv           -- no support for qualified NExt names yet...
                            | NTVar   [TCon]
                            | NPAttr  QName         -- protocol attribute (global), arg points to owning protocol
                            | NAlias  QName
                            | NMAlias ModName
                            | NModule TEnv
                            deriving (Eq,Show,Read,Generic)

nVar t                      = NVar (tSchema t) NoDecoration

instance Data.Binary.Binary NameInfo

instance Pretty TEnv where
    pretty tenv                 = vcat (map pretty tenv)

instance Pretty Env where
    pretty env                  = vcat (map pretty (names env))

instance Pretty (Name, Maybe NameInfo) where
    pretty (n, Nothing)         = pretty n <+> text "(reserved)"
    pretty (n, Just i)          = pretty (n, i)

instance Pretty (Name,NameInfo) where
    pretty (n, NVar t dec)      = pretty dec $+$ pretty n <+> colon <+> pretty t
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

instance Subst Env where
    msubst env                  = do ne <- msubst (names env)
                                     return env{ names = ne }
    tyfree env                  = tyfree (names env)

instance Subst NameInfo where
    msubst (NVar t dec)         = NVar <$> msubst t <*> return dec
    msubst (NSVar t)            = NSVar <$> msubst t
    msubst (NClass q us te)     = NClass <$> msubst q <*> msubst us <*> msubst te
    msubst (NProto q us te)     = NProto <$> msubst q <*> msubst us <*> msubst te
    msubst (NExt q us te)       = NExt <$> msubst q <*> msubst us <*> msubst te
    msubst (NTVar us)           = NTVar <$> msubst us
    msubst (NPAttr qn)          = NPAttr <$> return qn
    msubst (NAlias qn)          = NAlias <$> return qn
    msubst (NMAlias m)          = NMAlias <$> return m
    msubst (NModule te)         = NModule <$> return te     -- actually msubst te, but te has no free variables (top-level)

    tyfree (NVar t dec)         = tyfree t
    tyfree (NSVar t)            = tyfree t
    tyfree (NClass q us te)     = (tyfree q ++ tyfree us ++ tyfree te) \\ tybound q
    tyfree (NProto q us te)     = (tyfree q ++ tyfree us ++ tyfree te) \\ tybound q
    tyfree (NExt q us te)       = (tyfree q ++ tyfree us ++ tyfree te) \\ tybound q
    tyfree (NTVar us)           = tyfree us
    tyfree (NPAttr qn)          = []
    tyfree (NAlias qn)          = []
    tyfree (NMAlias qn)         = []
    tyfree (NModule te)         = []        -- actually tyfree te, but te has no free variables (top-level)

instance Subst SrcInfoTag where
    msubst (GEN l t)                = GEN l <$> msubst t
    msubst (INS l t)                = INS l <$> msubst t

    tyfree (GEN _ t)                = tyfree t
    tyfree (INS _ t)                = tyfree t


monotype (NVar _ StaticMethod)  = False
monotype (NVar _ ClassMethod)   = False
monotype (NVar _ InstMethod)    = False
monotype (NClass _ _ _)         = False
monotype (NProto _ _ _)         = False
monotype _                      = True

polytype                        = not . monotype

-------------------------------------------------------------------------------------------------------------------

class Unalias a where
    unalias                         :: Env -> a -> a
    unalias env                     = id

instance (Unalias a) => Unalias [a] where
    unalias env                     = map (unalias env)

instance (Unalias a) => Unalias (Maybe a) where
    unalias env                     = fmap (unalias env)

instance Unalias ModName where
    unalias env (ModName ns)        = norm [ (n,i) | (n,Just i) <- names env ] [] ns
      where
        norm te pre []              = ModName (reverse pre)
        norm te pre (n:ns)          = case lookup n te of
                                        Just (NModule te') -> norm te' (n:pre) ns
                                        Just (NMAlias m) -> norm (findmod m env) [] ns
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
    unalias env (TSchema l q t)     = TSchema l (unalias env q) (unalias env t)

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
    unalias env (NVar t d)          = NVar (unalias env t) d
    unalias env (NSVar t)           = NSVar (unalias env t)
    unalias env (NClass q us te)    = NClass (unalias env q) (unalias env us) (unalias env te)
    unalias env (NProto q us te)    = NProto (unalias env q) (unalias env us) (unalias env te)
    unalias env (NExt q us te)      = NExt (unalias env q) (unalias env us) (unalias env te)
    unalias env (NTVar us)          = NTVar (unalias env us)
    unalias env (NPAttr qn)         = NPAttr (unalias env qn)
    unalias env (NAlias qn)         = NAlias (unalias env qn)
    unalias env (NModule te)        = NModule (unalias env te)

instance Unalias (Name,NameInfo) where
    unalias env (n,i)               = (n, unalias env i)
    
-------------------------------------------------------------------------------------------------------------------

envBuiltin                  = [ (nSequence, NProto [a] [] []),
                                (nMapping,  NProto [a,b] [] []),
                                (nSet,      NProto [a] [] []),
                                (nInt,      NClass [] [] []),
                                (nFloat,    NClass [] [] []),
                                (nBool,     NClass [] [] []),
                                (nStr,      NClass [] [] []) ]
  where a:b:c:_             = [ TBind v [] | v <- tvarSupply ]
        ta:tb:tc:_          = [ TVar NoLoc v | v <- tvarSupply ]

envActorSelf                = [ (nSelf,     NVar (tSchema tRef) NoDecoration) ]

--------------------------------------------------------------------------------------------------------------------

prune xs                    = filter ((`notElem` xs) . fst)

initEnv                     :: Env
initEnv                     = define autoImp $ definemod mBuiltin $ addmod mBuiltin envBuiltin env0
  where autoImp             = importAll mBuiltin envBuiltin
        env0                = Env{ names = [], modules = [], defaultmod = mBuiltin }

statescope                  :: Env -> [Name]
statescope env              = [ z | (z, Just (NSVar _)) <- names env ]

block                       :: [Name] -> Env -> Env
block xs env                = env{ names = [ (x, Nothing) | x <- nub xs ] ++ names env }

define                      :: TEnv -> Env -> Env
define te env               = env{ names = [ (n, Just i) | (n,i) <- reverse te ] ++ prune (dom te) (names env) }

definemod                   :: ModName -> Env -> Env
definemod (ModName ns) env  = define [(head ns, defmod (tail ns) $ te1)] env
  where te1                 = case lookup (head ns) (names env) of Just (Just (NModule te1)) -> te1; _ -> []
        defmod [] te        = NModule $ findmod (ModName ns) env
        defmod (n:ns) te    = NModule $ (n, defmod ns te2) : prune [n] te
          where te2         = case lookup n te of Just (NModule te2) -> te2; _ -> []

blocked                     :: Env -> Name -> Bool
blocked env n               = lookup n (names env) == Just Nothing

findname                    :: Name -> Env -> NameInfo
findname n env              = case lookup n (names env) of
                                Just (Just (NAlias qn)) -> findqname qn env
                                Just (Just i) -> i
                                Just Nothing  -> nameReserved n
                                Nothing       -> nameNotFound n

findqname                   :: QName -> Env -> NameInfo
findqname (QName m n) env   = case lookup n (findmod (unalias env m) env) of
                                Just (NAlias qn) -> findqname qn env
                                Just i -> i
                                _ -> noItem m n
findqname (NoQual n) env    = findname n env


findmod                     :: ModName -> Env -> TEnv
findmod m env               = case lookup m (modules env) of
                                Just te -> te

addmod                      :: ModName -> TEnv -> Env -> Env
addmod m te env             = env{ modules = (m,te) : modules env }


dropnames                   :: Env -> Env
dropnames env               = env{ names = names initEnv }

newTEnv vs                  = do ts <- newTVars (length vs)
                                 return $ vs `zip` [ NVar (tSchema t) NoDecoration | t <- ts ]


-- Import handling

getImps                         :: (FilePath,FilePath) -> Env -> [Import] -> IO Env
getImps ps env []               = return env
getImps ps env (i:is)           = do env' <- impModule ps env i
                                     getImps ps env' is


impModule                       :: (FilePath,FilePath) -> Env -> Import -> IO Env
impModule ps env (Import _ ms)  = imp env ms
  where imp env []              = return env
        imp env (ModuleItem m as : is)
                                = do (env1,te) <- doImp ps env m
                                     let env2 = maybe (definemod m env1) (\n->define [(n, NMAlias m)] env1) as
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
                                                 return (addmod m te env, te)
                                         else do found <- doesFileExist fpath2
                                                 unless found (fileNotFound m)
                                                 te <- InterfaceFiles.readFile fpath
                                                 return (addmod m te env, te)
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
    imp (n, NVar t dec)     = Just (n, NVar t dec)
    imp _                   = Nothing                               -- cannot happen


-- Old builtins --------------------------------------------------------------------------

old_builtins                = [
                                (nm "record", schema [a] []
                                            (OFun ONil a (ORecord a))),
                                (nm "merge", schema [a,b,c] []
                                            (OFun ONil (OPos a (OPos b ONil)) c)),     -- very liberal type, for now!
                                (nm "sorted", schema [a,b] [QIn l0 0 a b]               -- contract requirement (containment)
                                            (OFun ONil (OPos b ONil) b)),
                                (nm "filter", schema [a, b] [QIn l0 0 a b]              -- contract requirement (containment)
                                            (OFun ONil (OPos (OFun ONil (OPos a ONil) OBool) (OPos b ONil)) b)),
                                (nm "id", schema [a] []
                                            (OFun ONil (OPos a ONil) OInt)),
                                (nm "len", schema [a,b] [QIn l0 0 a b]                  -- contract requirement (containment)
                                            (OFun ONil (OPos b ONil) OInt)),
                                (nm "dict", schema [a,b] []
                                            (OFun ONil (OPos (ODict a b) ONil) (ODict a b))),
                                (nm "sorteddict", schema [a,b] []
                                            (OFun ONil (OPos (ODict a b) ONil) (ODict a b))),
                                (nm "defaultdict", schema [a,b] []
                                            (OFun ONil (OPos (OFun ONil ONil b) ONil) (ODict a b))),
                                (nm "list", schema [a] []                               -- contract requirement (containment)
                                            (OFun ONil (OPos (OList a) ONil) (OList a))),
                                (nm "set", schema [a] []                                -- contract requirement (containment)
                                            (OFun ONil (OPos (OList a) ONil) (OSet a))),
                                (nm "sortedset", schema [a] []                                -- contract requirement (containment)
                                            (OFun ONil (OPos (OList a) ONil) (OSet a))),
                                (nm "sum", schema [] []
                                            (OFun ONil (OPos (OList OInt) ONil) OInt)),
                                (nm "enumerate", schema [a] []
                                            (OFun ONil (OPos (OList a) ONil) (OList (OTuple (OPos OInt (OPos a ONil)))))),
                                (nm "reversed", schema [a] []
                                            (OFun ONil (OPos (OList a) ONil) (OList a))),
                                (nm "min", schema [a] []
                                            (OFun ONil (OPos (OList a) ONil) a)),
                                (nm "max", schema [a] []
                                            (OFun ONil (OPos (OList a) ONil) a)),
                                (nm "iter", schema [a,b] [QIn l0 0 a b]
                                            (OFun ONil (OPos b ONil) (OList a))),
                                (nm "next", schema [a] []
                                            (OFun ONil (OPos (OList a) ONil) a)),
                                (nm "IPv4Address", schema [] []
                                            (OFun ONil (OPos OStr ONil) OInt)),
                                (nm "IPv4Network", schema [] []
                                            (OFun ONil (OPos OStr ONil) OInt)),
                                (nm "IPv6Address", schema [] []
                                            (OFun ONil (OPos OStr ONil) OInt)),
                                (nm "IPv6Network", schema [] []
                                            (OFun ONil (OPos OStr ONil) OInt)),
                                (nm "Decimal", schema [a,b] []
                                            (OFun ONil (OPos a ONil) b)),
                                (nm "abs", schema [] []
                                            (OFun ONil (OPos OInt ONil) OInt)),
                                (nm "all",  schema [a] []
                                            (OFun ONil a OBool)),
                                (nm "any",  schema [a] []
                                            (OFun ONil a OBool)),
                                (nm "int",  schema [a] []
                                            (OFun ONil a OInt)),
                                (nm "str", schema [a] []
                                            (OFun ONil a OStr)),
                                (nm "print", schema [a] []
                                            (OFun ONil a ONone)),
                                (nm "postpone", schema [a,b] []
                                            (OFun (syncFX ONil) (OPos OInt (OPos (OFun (asyncFX ONil) a (OMsg a)) a)) ONone)),
                                (nm "weakref", schema [a] []
                                            (OFun ONil (OPos a ONil) OInt)),
                                (nm "weakref_subscribe", schema [a, b] []
                                            (OFun ONil (OPos a (OPos b ONil)) ONone)),
                                (nm "weakref_unsubscribe", schema [a] []
                                            (OFun ONil (OPos a ONil) ONone)),
                                (nm "range", schema [] []
                                            (OFun ONil (OPos OInt (OPos OInt ONil)) (OList OInt))),
                                (nm "zip", schema [a,b] []
                                            (OFun ONil (OPos (OList a) (OPos (OList b) ONil)) (OList (OTuple (OPos a (OPos b ONil)))))),
                                (nm "__env__", schema [a] []
                                            (OFun ONil (OPos (ORecord a) ONil) (ORecord (
                                                OKwd (nm "create_external_api_process") 
                                                        (OSchema [[1],[2]] []
                                                            (OFun (syncFX (OVar [2])) (
                                                                OKwd (nm "executable_name") OStr (
                                                                OKwd (nm "args") (OList OStr) (
                                                                OKwd (nm "timeout") OInt (
                                                                OKwd (nm "node") OStr ONil)))) (OVar [1]))) (
                                                OKwd (nm "connect_external_api_process") (OFun (syncFX ONil) (
                                                                OKwd (nm "token") OStr ONil) OInt) ONil))))),
                                (nm "__get_current_placement__", schema [a] []
                                            (OFun (syncFX ONil) (OPos a ONil) (OSet OStr))),
                                (nm "__get_placement_constraints__", schema [a] []
                                            (OFun (syncFX ONil) (OPos a ONil) (OSet OStr))),
                                (nm "__set_placement_constraints__", schema [a] []
                                            (OFun (syncFX ONil) (OPos a (OPos (OSet OStr) ONil)) ONil)),
                                (nm "getattr", schema [a,b] []
                                            (OFun ONil (OPos (ORecord a) (OPos OStr ONil)) b)),
                                (nm "Exception", schema [a] []
                                            a),
                                (nm "ArithmeticError", schema [a] []
                                            a),
                                (nm "FloatingPointError", schema [a] []
                                            a),
                                (nm "OverflowError", schema [a] []
                                            a),
                                (nm "ZeroDivisionError", schema [a] []
                                            a),
                                (nm "AssertionError", schema [a] []
                                            a),
                                (nm "AttributeError", schema [a] []
                                            a),
                                (nm "LookupError", schema [a] []
                                            a),
                                (nm "IndexError", schema [a] []
                                            a),
                                (nm "KeyError", schema [a] []
                                            a),
                                (nm "ReferenceError", schema [a] []
                                            a),
                                (nm "TypeError", schema [a] []
                                            a),
                                (nm "ValueError", schema [a] []
                                            a),
                                (nm "StaleActorReferenceException", schema [a,b] []
                                            (OFun ONil (OPos a (OPos OStr b)) (ORecord (OKwd (nm "actor_id") a ONil))))
                              ]
  where a:b:c:_             = schemaOVars
        schema tvs cs t     = OSchema (unOVar tvs) cs t
        nm s                = Name l0 s


--------------------------------------------

type OTEnv                  = [(Name,OType)]

data OEnv                   = OEnv { venv :: [(Name, Maybe OType)], stvars :: [Name], ret :: Maybe OType }
                            deriving (Eq,Show)
instance Pretty OTEnv where
    pretty tenv             = vcat (map pr tenv)
      where pr (n,t)        = pretty n <+> colon <+> pretty t

instance MapSubst OEnv where
    mapsubst env            = do venv' <- mapsubst (venv env)
                                 ret' <- mapsubst (ret env)
                                 return env{ venv = venv', ret = ret' }
    oTyvars env             = oTyvars (venv env)++ oTyvars (ret env)


o_prune                     :: [Name] -> OTEnv -> OTEnv
o_prune vs                  = filter ((`notElem` vs) . fst)

o_emptyEnv                  :: OEnv
o_emptyEnv                  = OEnv { venv = [], stvars = [], ret = Nothing }

o_reserve                   :: [Name] -> OEnv -> OEnv
o_reserve vs env            = env { venv = [ (v, Nothing) | v <- vs ] ++ venv env }

o_define                    :: OTEnv -> OEnv -> OEnv
o_define te env             = env { venv = [ (v, Just t) | (v,t) <- te ] ++ venv env }

o_newstate                  :: [Name] -> OEnv -> OEnv
o_newstate vs env           = env { stvars = vs }

o_reserved                  :: OEnv -> Name -> Bool
o_reserved env v            = lookup v (venv env) == Just Nothing

o_findVar                   :: Name -> OEnv -> OType
o_findVar n env             = case lookup n (venv env) of
                                Nothing       -> nameNotFound n
                                Just Nothing  -> nameReserved n
                                Just (Just t) -> t

o_setReturn                 :: OType -> OEnv -> OEnv
o_setReturn t env           = env { ret = Just t }

o_getReturn                 :: OEnv -> OType
o_getReturn env             = fromJust $ ret env



