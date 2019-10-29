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
                            | NReserved
                            | NBlocked
                            deriving (Eq,Show,Read,Generic)

nVar                        :: Name -> Type -> TEnv
nVar n t                    = [(n, NVar (tSchema t) NoDecoration)]

nVar'                       :: Name -> TSchema -> TEnv
nVar' n sc                  = [(n, NVar sc NoDecoration)]

nVars                       :: TEnv -> [(Name, TSchema)]
nVars te                    = [ (n,sc) | (n, NVar sc _) <- te ]

mapVars                     :: (TSchema -> TSchema) -> TEnv -> TEnv
mapVars f te                = map g te
  where g (n, NVar sc d)    = (n, NVar (f sc) d)
        g (n, i)            = (n, i)


instance Data.Binary.Binary NameInfo

instance Pretty TEnv where
    pretty tenv                 = vcat (map pretty tenv)

instance Pretty Env where
    pretty env                  = vcat (map pretty (names env))

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
    pretty (n, NReserved)       = pretty n <+> text "(reserved)"
    pretty (n, NBlocked)        = pretty n <+> text "(blocked)"

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
    msubst NReserved            = return NReserved
    msubst NBlocked             = return NBlocked

    tyfree (NVar t dec)         = tyfree t
    tyfree (NSVar t)            = tyfree t
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
    unalias env NReserved           = NReserved
    unalias env NBlocked            = NBlocked

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

reserved                    :: Env -> Name -> Bool
reserved env n              = lookup n (names env) == Just NReserved

findName                    :: Name -> Env -> NameInfo
findName n env              = case lookup n (names env) of
                                Just (NAlias qn) -> findQName qn env
                                Just NReserved -> nameReserved n
                                Just NBlocked -> nameBlocked n
                                Just info -> info
                                Nothing -> nameNotFound n

findQName                   :: QName -> Env -> NameInfo
findQName (QName m n) env   = case lookup n (findMod (unalias env m) env) of
                                Just (NAlias qn) -> findQName qn env
                                Just i -> i
                                _ -> noItem m n
findQName (NoQual n) env    = findName n env

assignableType              :: Name -> Env -> Type
assignableType n env        = case findName n env of
                                NVar (TSchema _ [] t) d | d `notElem` mdec -> t
                                NSVar t -> t
                                _ -> err1 n "Not an assignable name:"
  where mdec                = [StaticMethod, ClassMethod, InstMethod]


findMod                     :: ModName -> Env -> TEnv
findMod m env               = case lookup m (modules env) of
                                Just te -> te

addMod                      :: ModName -> TEnv -> Env -> Env
addMod m te env             = env{ modules = (m,te) : modules env }


dropNames                   :: Env -> Env
dropNames env               = env{ names = names initEnv }

findType                    :: QName -> Env -> (Bool,[TBind],[TCon],TEnv)
findType n env              = case findQName n env of
                                NClass q us te -> (False,q,us,te)
                                NProto q us te -> (True,q,us,te)
                                _ -> err1 n "Not a class or protocol name:"


newTEnv vs                  = do ts <- newTVars (length vs)
                                 return $ vs `zip` [ NVar (tSchema t) NoDecoration | t <- ts ]


-- Instantiation -------------------------------------------------------------------------

monotypeOfName n env        = monotypeOfQName (NoQual n) env

monotypeOfQName qn env      = case findQName qn env of
                                NVar (TSchema _ [] t) _ -> t
                                NSVar t -> t
                                _ -> error ("(internal) Not a monotyped name:" ++ prstr qn)

typeOfName n env            = typeOfQName (NoQual n) env

typeOfQName qn env          = instN (findQName qn env)
  where
    instN (NVar sc _)       = instantiate env (openFX sc)
    instN (NSVar t)         = return t
    instN (NClass q _ _)    = instantiate env $ TSchema NoLoc q $ tAt $ TC qn $ map tVar $ tybound q
    instN _                 = err1 qn "Unexpected name..."

instantiate env (TSchema _ [] t)    = instwild t
instantiate env (TSchema _ q t)     = do tvs <- newTVars (length q)
                                         let s = tybound q `zip` tvs
                                         constrain $ constraintsOf (subst s q) env
                                         instwild (subst s t)

constraintsOf q env         = [ constraint env t u | TBind v us <- q, let t = tVar v, u <- us ]

constraint env t u@(TC n _) = case findQName n env of
                                NClass{} -> Sub t (tCon u)
                                NProto{} -> Impl t u

class InstWild a where
    instwild                    :: a -> TypeM a
    instwild a                  = return a

instance InstWild TSchema where
    instwild (TSchema l q t)    = TSchema l <$> mapM instwild q <*> instwild t

instance InstWild TBind where
    instwild (TBind tv us)      = TBind tv <$> mapM instwild us

instance InstWild TCon where
    instwild (TC c ts)          = TC c <$> mapM instwild ts

instance InstWild Type where
    instwild (TWild _)          = newTVar
    instwild (TCon l tc)        = TCon l <$> instwild tc
    instwild (TAt l tc)         = TAt l <$> instwild tc
    instwild (TFun l e p k t)   = TFun l <$> instwild e <*> instwild p <*> instwild k <*> instwild t
    instwild (TTuple l p)       = TTuple l <$> instwild p
    instwild (TRecord l k)      = TRecord l <$> instwild k
    instwild (TOpt l t)         = TOpt l <$> instwild t
    instwild (TRow l n t r)     = TRow l n <$> instwild t <*> instwild r
    instwild t                  = return t


-- Environment unification ---------------------------------------------------------------

unifyTEnv env tenvs []                  = return []
unifyTEnv env tenvs (v:vs)              = case [ ni | Just ni <- map (lookup v) tenvs] of
                                            [] -> unifyTEnv env tenvs vs
                                            [ni] -> ((v,ni):) <$> unifyTEnv env tenvs vs
                                            ni:nis -> do ni' <- unifN ni nis
                                                         ((v,ni'):) <$> unifyTEnv env tenvs vs
  where 
    unifN (NVar (TSchema _ [] t) d) nis = do mapM (unifV t d) nis
                                             return (NVar (tSchema t) d)
    unifN (NSVar t) nis                 = do mapM (unifSV t) nis
                                             return (NSVar t)
    unifN ni nis                        = notYet (loc v) (text "Merging of declarations")

    unifV t d (NVar (TSchema _ [] t') d')
      | d == d'                         = constrain [Equ t t']
      | otherwise                       = err1 v "Inconsistent decorations for"
    unifV t d ni                        = err1 v "Inconsistent bindings for"

    unifSV t (NSVar t')                 = constrain [Equ t t']
    unifSV t ni                         = err1 v "Inconsistent bindings for"



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
    imp (n, NVar t dec)     = Just (n, NVar t dec)
    imp _                   = Nothing                               -- cannot happen


-- Error handling ------------------------------------------------------------------------

data CheckerError                       = FileNotFound ModName
                                        | NameNotFound Name
                                        | NameReserved Name
                                        | NameBlocked Name
                                        | IllegalImport SrcLoc
                                        | DuplicateImport Name
                                        | NoItem ModName Name
                                        | OtherError SrcLoc String
                                        deriving (Show)

instance Control.Exception.Exception CheckerError

checkerError (FileNotFound n)           = (loc n, " Type interface file not found for " ++ render (pretty n))
checkerError (NameNotFound n)           = (loc n, " Name " ++ prstr n ++ " is not in scope")
checkerError (NameReserved n)           = (loc n, " Name " ++ prstr n ++ " is reserved but not yet defined")
checkerError (NameBlocked n)            = (loc n, " Name " ++ prstr n ++ " is currently not accessible")
checkerError (IllegalImport l)          = (l,     " Relative import not yet supported")
checkerError (DuplicateImport n)        = (loc n, " Duplicate import of name " ++ prstr n)
checkerError (NoItem m n)               = (loc n, " Module " ++ render (pretty m) ++ " does not export " ++ nstr n)
checkerError (OtherError l str)         = (l,str)

nameNotFound n                          = Control.Exception.throw $ NameNotFound n

nameReserved n                          = Control.Exception.throw $ NameReserved n

nameBlocked n                           = Control.Exception.throw $ NameBlocked n

fileNotFound n                          = Control.Exception.throw $ FileNotFound n

illegalImport l                         = Control.Exception.throw $ IllegalImport l

duplicateImport n                       = Control.Exception.throw $ DuplicateImport n

noItem m n                              = Control.Exception.throw $ NoItem m n

err l s                                 = Control.Exception.throw $ OtherError l s

err1 x s                                = err (loc x) (s ++ " " ++ prstr x)

err2 (x:_) s                            = err1 x s








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



