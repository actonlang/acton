-- Copyright (C) 2019-2021 Data Ductus AB
--
-- Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
--
-- 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
--
-- 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
--
-- 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

{-# LANGUAGE FlexibleInstances, FlexibleContexts, DeriveGeneric #-}
module Acton.Env where

import qualified Control.Exception
import qualified Data.Binary
import GHC.Generics (Generic)
import Data.Typeable
import Data.Char
import System.FilePath.Posix (joinPath,takeDirectory)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Environment (getExecutablePath)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad
import Control.Monad.Except
import Data.IORef
import qualified Data.HashMap.Strict as M
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as S

import Acton.Syntax
import Acton.Builtin
import Acton.Prim
import Acton.Printer
import Acton.Names
import Acton.Subst
import Acton.NameInfo
import Utils
import Pretty
import InterfaceFiles
import Prelude hiding ((<>))





mkEnv                       :: [FilePath] -> Env0 -> Module -> IO Env0
mkEnv spath env m           = getImps spath env (imps m)

-- Full environment -------------------------------------------------------------------------------

data EnvF x                 = EnvF {
                                activeNames:: TEnv,
                                closedNames:: TEnv,
                                hnames     :: HTEnv,
                                closedHNames:: HTEnv,
                                sigLocs    :: LocEnv,
                                closedSigLocs:: LocEnv,
                                defLocs    :: LocEnv,
                                closedDefLocs:: LocEnv,
                                activeStateNames:: [Name],
                                activeTypeVars:: [(Name, Kind, CCon)],
                                imports    :: [ModName],
                                improots   :: [Name],
                                modules    :: Map ModName ModuleInfo,
                                thismod    :: Maybe ModName,
                                context    :: [EnvCtx],
                                qlevel     :: Int,
                                envX       :: x
                              } deriving (Show)

type Env0                   = EnvF ()
type LocEnv                 = M.HashMap Name SrcLoc

-- activeNames may contain live unification variables; closedNames must not.
-- hnames is the full active-over-closed lookup index, while closedHNames is
-- the reusable closed-only part.

setX                        :: EnvF y -> x -> EnvF x
setX env x                  = EnvF { activeNames = activeNames env, closedNames = closedNames env,
                                     hnames = hnames env, closedHNames = closedHNames env,
                                     sigLocs = sigLocs env, closedSigLocs = closedSigLocs env,
                                     defLocs = defLocs env, closedDefLocs = closedDefLocs env,
                                     activeStateNames = activeStateNames env,
                                     activeTypeVars = activeTypeVars env,
                                     imports = imports env, improots = improots env,
                                     modules = modules env, thismod = thismod env,
                                     context = context env, qlevel = qlevel env, envX = x }

modX                        :: EnvF x -> (x -> x) -> EnvF x
modX env f                  = env{ envX = f (envX env) }


data EnvCtx                 = CtxDef | CtxAct | CtxClass | CtxLoop deriving (Eq,Show)

setInAct env                = env{ context = CtxAct : context env  }

setInDef env                = env{ context = CtxDef : context env }

setInClass env              = env{ context = CtxClass : context env  }

setInLoop env               = env{ context = CtxLoop : context env  }

onTop env                   = context env == []

contextIs env ctx           = case context env of c:_ -> c == ctx; _ -> False

contextHas env ctx          = ctx `elem` context env

inAct env                   = contextHas env CtxAct

inDef env                   = contextIs env CtxDef

inClass env                 = contextIs env CtxClass

inLoop env                  = contextIs env CtxLoop

-- Back passes rewrite imported interface entries. The conversion wraps each
-- module's lookup function, so only names that are actually demanded are
-- converted (and memoized). sources maps a generated name to the source
-- entries it may be derived from.
convertModules1             :: ((Name,NameInfo) -> (Name,NameInfo)) -> Env0 -> Env0
convertModules1 f env       = convertModules (const []) (\_ ni -> [f ni]) env

convertModules              :: (Name -> [Name]) -> (ModName -> (Name,NameInfo) -> TEnv) -> Env0 -> Env0
convertModules sources f env= env{ modules = Map.mapWithKey convMod (modules env) }
  where convMod m mi
          | m == mPrim      = mi
          | otherwise       = mi{ moduleLookupName = memoLookup lookupConverted }
          where lookupConverted n
                            = listToMaybe $ mapMaybe (lookupFrom n) (n : sources n)
                lookupFrom n src
                            = do i <- moduleLookupName mi src
                                 lookup n (f m (src,i))


-- Module interfaces -----------------------------------------------------------------------------

-- A ModuleInfo is the per-module view used for all lookups into dependency
-- modules. A locally compiled module backs it with its in-memory interface
-- TEnv; an imported module can back it with selective .tydb reads instead, so
-- a dependent module only pays for the names and indexes it actually demands.

data ModuleInfo             = ModuleInfo {
                                moduleImports     :: [ModName],
                                moduleDoc         :: Maybe String,
                                moduleLookupName :: Name -> Maybe NameInfo,
                                modulePublicNames :: [Name],
                                moduleConstructors:: TEnv,
                                moduleActors      :: [TCon],
                                moduleConAttr     :: Name -> [TCon],
                                moduleProtoAttr   :: Name -> [TCon],
                                moduleDescendants :: QName -> [TCon],
                                moduleProtoDescendants :: QName -> [TCon],
                                moduleWitnessesByProto :: QName -> [Witness],
                                moduleWitnessesByType  :: QName -> [Witness]
                              }

instance Show ModuleInfo where
    show mi                 = "ModuleInfo {imports = " ++ show (moduleImports mi) ++ "}"

modulePublicTEnv            :: ModuleInfo -> TEnv
modulePublicTEnv mi         = mapMaybe entry (modulePublicNames mi)
  where entry n             = fmap (\i -> (n,i)) (moduleLookupName mi n)

mkModuleInfo                :: ModName -> [ModName] -> TEnv -> Maybe String -> ModuleInfo
mkModuleInfo m ms te mdoc   = ModuleInfo {
                                moduleImports = ms,
                                moduleDoc = mdoc,
                                moduleLookupName = \n -> M.lookup n hte,
                                modulePublicNames = map fst pte,
                                moduleConstructors = [ ni | ni@(_,i) <- pte, isCons i ],
                                moduleActors = [ moduleTCon m ni | ni@(_,NAct{}) <- pte ],
                                moduleConAttr = \n -> Map.findWithDefault [] n conattrs,
                                moduleProtoAttr = \n -> Map.findWithDefault [] n protoattrs,
                                moduleDescendants = qkeyed descendants,
                                moduleProtoDescendants = qkeyed protodescs,
                                moduleWitnessesByProto = qkeyed witprotos,
                                moduleWitnessesByType = qkeyed wittypes
                              }
  where pte                 = publicTEnv te
        hte                 = convTEnv2HTEnv pte
        wits                = extWitnesses m [ ni | ni@(_,NExt{}) <- pte ]
        witprotos           = indexMap [ (tcname $ proto w, w) | w <- wits ]
        wittypes            = indexMap [ (qn, w) | w <- wits, Just qn <- [witnessTypeQName w] ]
        conattrs            = indexMap [ (a, moduleTCon m ni) | ni@(_,i) <- pte, isCon i, a <- dom (conTE i) ]
        protoattrs          = indexMap [ (a, moduleTCon m ni) | ni@(_,i@NProto{}) <- pte, a <- dom (conTE i) ]
        descendants         = indexMap [ (tcname c, moduleTCon m ni) | ni@(_,i@NClass{}) <- pte, (_,c) <- ancestry i ]
        protodescs          = indexMap [ (tcname c, moduleTCon m ni) | ni@(_,i@NProto{}) <- pte, (_,c) <- ancestry i ]
        qkeyed mp qn        = concat [ Map.findWithDefault [] qn' mp | qn' <- moduleQNameKeys m qn ]
        indexMap xs         = Map.fromListWith (++) [ (k, [v]) | (k,v) <- reverse xs ]
        ancestry (NClass _ us _ _) = us
        ancestry (NProto _ us _ _) = us
        ancestry _          = []
        conTE (NClass _ _ te' _) = te'
        conTE (NProto _ _ te' _) = te'
        conTE (NAct _ _ _ te' _) = te'
        conTE _             = []
        isCons NClass{}     = True
        isCons NProto{}     = True
        isCons NAct{}       = True
        isCons _            = False
        isCon NClass{}      = True
        isCon NAct{}        = True
        isCon _             = False

-- | Build a ModuleInfo backed by selective .tydb reads. Lookups run keyed
-- LMDB reads on demand (behind pure-looking functions) and memoize results;
-- a failed or corrupt read aborts compilation like any other interface cache
-- error.
mkTyFileModuleInfo          :: ModName -> [ModName] -> Maybe String -> InterfaceFiles.InterfaceDB -> ModuleInfo
mkTyFileModuleInfo m ms mdoc db
                            = ModuleInfo {
                                moduleImports = ms,
                                moduleDoc = mdoc,
                                moduleLookupName = memoLookup lookupName,
                                modulePublicNames = publicNames,
                                moduleConstructors = constructors,
                                moduleActors = actors,
                                moduleConAttr = memoLookup conAttr,
                                moduleProtoAttr = memoLookup protoAttr,
                                moduleDescendants = memoLookup descendants,
                                moduleProtoDescendants = memoLookup protoDescendants,
                                moduleWitnessesByProto = memoLookup witsByProto,
                                moduleWitnessesByType = memoLookup witsByType
                              }
  where lookupName n
          | not (isPublicName n) = Nothing
          | otherwise        = unsafePerformIO $ do
                                  mi <- InterfaceFiles.readInterfaceDBNameInfoMaybe db n
                                  return $ snd <$> mi
        publicNames          = unsafePerformIO $ InterfaceFiles.readInterfaceDBPublicNames db
        constructors         = unsafePerformIO $ InterfaceFiles.readInterfaceDBConstructors db
        actors               = map (moduleTCon m) $ unsafePerformIO $ InterfaceFiles.readInterfaceDBActors db
        conAttr n            = map (moduleTCon m) $ unsafePerformIO $ InterfaceFiles.readInterfaceDBConAttr db n
        protoAttr n          = map (moduleTCon m) $ unsafePerformIO $ InterfaceFiles.readInterfaceDBProtoAttr db n
        descendants qn       = [ moduleTCon m ni | qn' <- moduleQNameKeys m qn, ni@(_,NClass{}) <- readDesc qn' ]
        protoDescendants qn  = [ moduleTCon m ni | qn' <- moduleQNameKeys m qn, ni@(_,NProto{}) <- readDesc qn' ]
        readDesc qn          = unsafePerformIO $ InterfaceFiles.readInterfaceDBDescendants db qn
        -- The proto index stores extension records; one extension can carry
        -- witnesses for several protocols, so keep only the indexed protocol.
        witsByProto qn       = concat [ filter ((== qn') . tcname . proto) $
                                          extWitnesses m $
                                          unsafePerformIO $
                                          InterfaceFiles.readInterfaceDBExtByProto db qn'
                                      | qn' <- moduleQNameKeys m qn ]
        witsByType qn        = concat [ extWitnesses m $ unsafePerformIO $ InterfaceFiles.readInterfaceDBExtByType db qn' | qn' <- moduleQNameKeys m qn ]

moduleTCon                  :: ModName -> (Name, NameInfo) -> TCon
moduleTCon m (n, i)         = TC (GName m n) (wildargs i)

-- A module's interface records references to its own names unqualified, so
-- both the queried form and the module-local form must be tried.
moduleQNameKeys             :: ModName -> QName -> [QName]
moduleQNameKeys m qn@(NoQ n)= [qn, GName m n]
moduleQNameKeys m qn@(GName m' n)
  | m == m'                 = [qn, NoQ n]
moduleQNameKeys m qn@(QName m' n)
  | m == m'                 = [qn, NoQ n]
moduleQNameKeys _ qn        = [qn]

extWitnesses                :: ModName -> TEnv -> [Witness]
extWitnesses m exts         = foldl' add [] wits
  where wits                = [ WClass q (tCon c) p (GName m n) ws (length opts) | (n, NExt q c ps _ opts _) <- exts, (ws,p) <- ps ]
        add ws w
          | any (same w) ws = ws
          | otherwise       = w : ws
        same w w'           = tcname (proto w) == tcname (proto w') && wtype w == wtype w'

witnessTypeQName            :: Witness -> Maybe QName
witnessTypeQName w          = nameOf (wtype w)
  where nameOf (TCon _ c)   = Just (tcname c)
        nameOf (TVar _ v)   = Just (NoQ $ tvname v)
        nameOf _            = Nothing

-- | Memoize a pure-looking lookup function whose results come from on-demand
-- .tydb reads, so each key is read at most once per module handle.
memoLookup                  :: Ord k => (k -> v) -> k -> v
memoLookup f                = unsafePerformIO $ do
                                  memo <- newIORef Map.empty
                                  return $ \n -> unsafePerformIO $ do
                                      cache <- readIORef memo
                                      case Map.lookup n cache of
                                        Just r  -> return r
                                        Nothing -> do
                                          let r = f n
                                          Control.Exception.evaluate r
                                          atomicModifyIORef' memo $ \cache' ->
                                            case Map.lookup n cache' of
                                              Just r' -> (cache', r')
                                              Nothing -> (Map.insert n r cache', r)
{-# NOINLINE memoLookup #-}

instance (Pretty x) => Pretty (EnvF x) where
    pretty env                  = text "--- modules:"  $+$
                                  vcat (map pretty (Map.keys (modules env))) $+$
                                  text "--- active names" <+> pretty (thismod env) <+> parens (text (show (qlevel env))) <> colon $+$
                                  vcat (map pretty (activeNames env)) $+$
                                  text "--- closed names" <+> pretty (thismod env) <> colon $+$
                                  vcat (map pretty (closedNames env)) $+$
                                  text "--- imports" <+> pretty (thismod env) <> colon $+$
                                  vcat (map pretty (imports env)) $+$
                                  pretty (envX env) $+$
                                  text "."

closeDepVarsQ vs q
  | null vs'                        = nub vs
  | otherwise                       = closeDepVarsQ (vs'++vs) q
  where vs'                         = concat [ vfree us \\ vs | QBind v us <- q, v `elem` vs ]


-- Unalias --------------------------------------------------------------------------------------

class Unalias a where
    unalias                         :: EnvF x -> a -> a
    unalias env                     = id

instance (Unalias a) => Unalias [a] where
    unalias env                     = map (unalias env)

instance (Unalias a) => Unalias (Maybe a) where
    unalias env                     = fmap (unalias env)

instance Unalias ModName where
    unalias env m@(ModName ns)
      | inBuiltin env               = m
      | otherwise                   = case lookupName (head ns) env of
                                        Just (NMAlias m') -> m'
                                        Nothing | m `elem` (mPrim:mBuiltin:imports env) || modulePrefix m env -> m
                                        _ -> noModule m
instance Unalias QName where
    unalias env n0@(QName m n)      = case findModuleInfo m env of
                                        Just mi -> case moduleLookupName mi n of
                                                      Just (NAlias qn) -> setLoc (loc n0) qn
                                                      Just _ -> GName m' n
                                                      _ -> noItem m n
                                        Nothing -> error ("#### unalias fails for " ++ prstr (QName m n))
      where m'                      = unalias env m
    unalias env (NoQ n)
      | inBuiltin env               = GName mBuiltin n
      | otherwise                   = case lookupName n env of
                                        Just (NAlias qn) -> setLoc (loc n) qn
                                        _ -> case thismod env of Just m -> GName m n; _ -> NoQ n
    unalias env (GName m n)
--      | inBuiltin env, m==mBuiltin  = NoQ n
      | otherwise                   = GName m n

setLoc l (QName m n)                = QName (setModLoc l m) (setNameLoc l n)
setLoc l (GName m n)                = GName (setModLoc l m) (setNameLoc l n)
setLoc l (NoQ n)                    = NoQ (setNameLoc l n)

setNameLoc l (Name _ s)             = Name l s
setNameLoc _ n                      = n

setModLoc l (ModName ns)            = ModName (map (setNameLoc l) ns)


instance Unalias TSchema where
    unalias env (TSchema l q t)     = TSchema l (unalias env q) (unalias env t)

instance Unalias TCon where
    unalias env (TC qn ts)          = TC (unalias env qn) (unalias env ts)

instance Unalias QBind where
    unalias env (QBind tv cs)       = QBind tv (unalias env cs)

instance Unalias Type where
    unalias env (TCon l c)          = TCon l (unalias env c)
    unalias env (TFun l e p r t)    = TFun l (unalias env e) (unalias env p) (unalias env r) (unalias env t)
    unalias env (TTuple l p k)      = TTuple l (unalias env p) (unalias env k)
    unalias env (TOpt l t)          = TOpt l (unalias env t)
    unalias env (TRow l k n t r)    = TRow l k n (unalias env t) (unalias env r)
    unalias env (TStar l k r)       = TStar l k (unalias env r)
    unalias env t                   = t

instance Unalias NameInfo where
    unalias env (NVar t)            = NVar (unalias env t)
    unalias env (NSVar t)           = NSVar (unalias env t)
    unalias env (NDef t d doc)      = NDef (unalias env t) d doc
    unalias env (NSig t d doc)      = NSig (unalias env t) d doc
    unalias env (NAct q p k te doc) = NAct (unalias env q) (unalias env p) (unalias env k) (unalias env te) doc
    unalias env (NClass q us te doc)= NClass (unalias env q) (unalias env us) (unalias env te) doc
    unalias env (NProto q us te doc)= NProto (unalias env q) (unalias env us) (unalias env te) doc
    unalias env (NExt q c ps te opts doc)= NExt (unalias env q) (unalias env c) (unalias env ps) (unalias env te) opts doc
    unalias env (NTVar k c ps)      = NTVar k (unalias env c) (unalias env ps)
    unalias env (NAlias qn)         = NAlias (unalias env qn)
    unalias env (NMAlias m)         = NMAlias (unalias env m)
    unalias env NReserved           = NReserved

instance Unalias NModule where
    unalias env (NModule ms te doc) = NModule (unalias env ms) (unalias env te) doc

instance Unalias (Name,NameInfo) where
    unalias env (n,i)               = (n, unalias env i)

instance Unalias WTCon where
    unalias env (w,u)               = (unalias env w, unalias env u)

instance Unalias (Either QName QName) where
    unalias env (Left n)            = Left $ unalias env n
    unalias env (Right n)           = Right $ unalias env n


-- Env construction and modification -------------------------------------------------------------------------------------------

-- | Initialize the base environment; the builtin mode skips interface loading.
-- first variant is special case for compiling __builtin__.act
publicTEnv                 :: TEnv -> TEnv
publicTEnv                 = filter (isPublicName . fst)

initEnv                    :: FilePath -> Bool -> IO Env0
initEnv path True          = return $ EnvF{ activeNames = [],
                                            closedNames = [(nPrim,NMAlias mPrim)],
                                            hnames = hnamesFrom [(nPrim,NMAlias mPrim)],
                                            closedHNames = hnamesFrom [(nPrim,NMAlias mPrim)],
                                            sigLocs = M.empty,
                                            closedSigLocs = M.empty,
                                            defLocs = M.empty,
                                            closedDefLocs = M.empty,
                                            activeStateNames = [],
                                            activeTypeVars = [],
                                            imports = [],
                                            improots = [],
                                            modules = Map.singleton mPrim (mkModuleInfo mPrim [] primEnv Nothing),
                                            thismod = Nothing,
                                            context = [],
                                            qlevel = 0,
                                            envX = () }
initEnv path False         = do (_,nmod,_,_,_,_,_,_,_,_,_,_,_) <- InterfaceFiles.readFile (InterfaceFiles.interfacePath path (modName ["__builtin__"]))
                                let NModule _ envBuiltin builtinDocstring = nmod
                                    envBuiltinPublic = publicTEnv envBuiltin
                                    initialNames = [(nPrim,NMAlias mPrim), (nBuiltin,NMAlias mBuiltin)]
                                    env0 = EnvF{ activeNames = [],
                                                 closedNames = initialNames,
                                                 hnames = hnamesFrom initialNames,
                                                 closedHNames = hnamesFrom initialNames,
                                                 sigLocs = M.empty,
                                                 closedSigLocs = M.empty,
                                                 defLocs = M.empty,
                                                 closedDefLocs = M.empty,
                                                 activeStateNames = [],
                                                 activeTypeVars = [],
                                                 imports = [],
                                                 improots = [],
                                                 modules = Map.fromList [(mPrim, mkModuleInfo mPrim [] primEnv Nothing), (mBuiltin, mkModuleInfo mBuiltin [] envBuiltin builtinDocstring)],
                                                 thismod = Nothing,
                                                 context = [],
                                                 qlevel = 0,
                                                 envX = () }
                                    env = importAll mBuiltin (mkModuleInfo mBuiltin [] envBuiltinPublic builtinDocstring) env0
                                return env

withModulesFrom             :: EnvF x -> EnvF x -> EnvF x
env `withModulesFrom` env'  = env{modules = modules env'}

hnamesFrom                  :: TEnv -> HTEnv
hnamesFrom te               = extendNames te M.empty

extendNames                :: TEnv -> HTEnv -> HTEnv
extendNames te hte         = foldr add hte te
  where add (n,i) hte       = M.insert n i hte

extendSigLocs               :: TEnv -> LocEnv -> LocEnv
extendSigLocs te locs       = foldr add locs te
  where add (n, NSig{}) locs = M.insert n (loc n) locs
        add _ locs          = locs

extendDefLocs               :: TEnv -> LocEnv -> LocEnv
extendDefLocs te locs       = foldr add locs te
  where add (n, NDef{}) locs = M.insert n (loc n) locs
        add _ locs          = locs

stateNamesIn                :: TEnv -> [Name]
stateNamesIn te             = [ n | (n, NSVar _) <- te ]

typeVarsIn                  :: TEnv -> [(Name, Kind, CCon)]
typeVarsIn te               = [ (n, k, c) | (n, NTVar k c _) <- te ]

setActiveNames              :: TEnv -> EnvF x -> EnvF x
setActiveNames te env       = env{ activeNames = te,
                                   hnames = extendNames te (closedHNames env),
                                   sigLocs = extendSigLocs te (closedSigLocs env),
                                   defLocs = extendDefLocs te (closedDefLocs env),
                                   activeStateNames = stateNamesIn te,
                                   activeTypeVars = typeVarsIn te }

addActiveNames              :: TEnv -> EnvF x -> EnvF x
addActiveNames te env       = env{ activeNames = te ++ activeNames env,
                                   hnames = extendNames te (hnames env),
                                   sigLocs = extendSigLocs te (sigLocs env),
                                   defLocs = extendDefLocs te (defLocs env),
                                   activeStateNames = stateNamesIn te ++ activeStateNames env,
                                   activeTypeVars = typeVarsIn te ++ activeTypeVars env }

addClosedNames              :: TEnv -> EnvF x -> EnvF x
addClosedNames te env       = env{ closedNames = te ++ closedNames env,
                                   hnames = extendNames (activeNames env) hte,
                                   closedHNames = hte,
                                   sigLocs = extendSigLocs (activeNames env) slocs,
                                   closedSigLocs = slocs,
                                   defLocs = extendDefLocs (activeNames env) dlocs,
                                   closedDefLocs = dlocs }
  where hte                 = extendNames te (closedHNames env)
        slocs               = extendSigLocs te (closedSigLocs env)
        dlocs               = extendDefLocs te (closedDefLocs env)

lookupName                  :: Name -> EnvF x -> Maybe NameInfo
lookupName n env            = M.lookup n (hnames env)

reserve                     :: [Name] -> EnvF x -> EnvF x
reserve xs env
  | not $ null badSelf      = selfParamError (loc $ head badSelf)
  | otherwise               = addActiveNames te env
  where badSelf             = if inAct env then xs `intersect` [selfKW] else []
        te                  = [ (x, NReserved) | x <- uniqueNames xs ]

reserveClosed               :: [Name] -> EnvF x -> EnvF x
reserveClosed xs env
  | not $ null badSelf      = selfParamError (loc $ head badSelf)
  | otherwise               = addClosedNames te env
  where badSelf             = if inAct env then xs `intersect` [selfKW] else []
        te                  = [ (x, NReserved) | x <- uniqueNames xs ]

uniqueNames                 :: [Name] -> [Name]
uniqueNames ns              = reverse $ snd $ foldl' add (M.empty, []) ns
  where add (seen, out) n
          | M.member n seen = (seen, out)
          | otherwise       = (M.insert n () seen, n:out)

define                      :: TEnv -> EnvF x -> EnvF x
define te env
  | not $ null badSelf      = selfParamError (loc $ head badSelf)
  | otherwise               = addActiveNames te' env
  where badSelf             = if inAct env then dom te `intersect` [selfKW] else []
        te'                 = reverse te

defineClosed                :: TEnv -> EnvF x -> EnvF x
defineClosed te env
  | not $ null badSelf      = selfParamError (loc $ head badSelf)
  | otherwise               = addClosedNames te' env
  where badSelf             = if inAct env then dom te `intersect` [selfKW] else []
        te'                 = reverse te


addImport                   :: ModName -> EnvF x -> EnvF x
addImport m env
  | m `elem` imports env    = env
  | otherwise               = env{ imports = m : imports env }

addImpRoot m env            = env{ improots = n : improots env }
  where ModName (n:_)       = m

getImports env              = reverse (imports env)

defineTVars                 :: QBinds -> EnvF x -> EnvF x
defineTVars q env           = foldr f env (unalias env q)
  where f (QBind tv us) env = addActiveNames [ni] (env{ qlevel = qlevel env + 1 })
          where (c,ps)      = case us of u:us' | not $ isProto env (tcname u) -> (u,us'); _ -> (cValue,us)
                ni          = (tvname tv, NTVar (tvkind tv) c ps)

selfSubst n q               = vsubst [(tvSelf, tCon tc)]
  where tc                  = TC n (map tVar $ qbound q)

selfQuant n q               = QBind tvSelf [tc] : q
  where tc                  = TC n (map tVar $ qbound q)

setMod                      :: ModName -> EnvF x -> EnvF x
setMod m env                = env{ thismod = Just m }

addMod                      :: ModName -> [ModName] -> TEnv -> Maybe String -> EnvF x -> EnvF x
addMod m ms newte mdoc env  = addModuleInfo m (mkModuleInfo m ms newte mdoc) env

addModuleInfo               :: ModName -> ModuleInfo -> EnvF x -> EnvF x
addModuleInfo m mi env      = env{ modules = Map.insert m mi (modules env) }


-- General Env queries -----------------------------------------------------------------------------------------------------------

inBuiltin                   :: EnvF x -> Bool
inBuiltin env               = Map.size (modules env) == 1     -- mPrim only

stateScope                  :: EnvF x -> [Name]
stateScope env              = activeStateNames env

typeScope                   :: EnvF x -> [TVar]
typeScope env               = [ TV k n | (n, k, _) <- activeTypeVars env ]

lookupTypeVarKind           :: TVar -> EnvF x -> Maybe Kind
lookupTypeVarKind (TV _ n) env
                            = findKind (activeTypeVars env)
  where findKind []         = Nothing
        findKind ((n', k, _) : xs)
          | n == n'         = Just k
          | otherwise       = findKind xs

quantScope0                 :: EnvF x -> QBinds
quantScope0 env             = [ QBind (TV k n) (if c==cValue then ps else (c:ps)) | (n, NTVar k c ps) <- activeNames env ]

quantScope                  :: EnvF x -> QBinds
quantScope env              = [ q | q@(QBind tv _) <- quantScope0 env, tv /= tvSelf ]

tvarDescendants             :: EnvF x -> [TCon] -> [TVar]
tvarDescendants env cs      = [ TV k n | (n, k, c) <- activeTypeVars env, c `elem` cs ]

selfScopeSubst              :: EnvF x -> Substitution
selfScopeSubst env          = [ (TV k n, tCon c) | (n, NTVar k c ps) <- activeNames env, n == nSelf ]


-- Name queries -------------------------------------------------------------------------------------------------------------------

findQName                   :: QName -> EnvF x -> NameInfo
findQName n env             = case tryQName n env of
                                 Just i -> i
                                 Nothing -> nameNotFound (noq n)

tryQName                    :: QName -> EnvF x -> Maybe NameInfo
tryQName (QName m n) env    = case findModuleInfo m env of
                                Just mi -> case moduleLookupName mi n of
                                    Just (NAlias qn) -> tryQName qn env
                                    Just i -> Just i
                                    _ -> noItem m n
                                _ -> noModule m
tryQName (NoQ n) env        = case lookupName n env of
                                Just (NAlias qn) -> tryQName qn env
                                Just ni -> Just ni
                                Nothing -> Nothing
tryQName (GName m n) env
  | Just m == thismod env   = tryQName (NoQ n) env
  | inBuiltin env,
    m==mBuiltin             = tryQName (NoQ n) env
  | otherwise               = case lookupModuleInfo m env of
                                Just mi -> case moduleLookupName mi n of
                                    Just i -> Just i
                                    Nothing -> noItem m n -- error ("## Failed lookup of " ++ prstr n ++ " in module " ++ prstr m)
                                Nothing -> noModule m -- error ("## Failed lookup of module " ++ prstr m)

findSigLoc n env            = M.lookup n (sigLocs env)

findDefLoc n env            = M.lookup n (defLocs env)

findName n env              = findQName (NoQ n) env

lookupVar n env             = case lookupName n env of
                                Just (NVar t) -> Just t
                                _ -> Nothing

findModuleInfo              :: ModName -> EnvF x -> Maybe ModuleInfo  -- m is modname part of a QName, so we must check for aliasing
findModuleInfo m env | inBuiltin env, m==mBuiltin
                            = Just (builtinModuleInfo env)
findModuleInfo m@(ModName ns) env
                            = case lookupName (head ns) env of
                                Just (NMAlias (ModName m')) -> lookupModuleInfo (ModName $ m'++tail ns) env
                                Nothing | m `elem` (mPrim:mBuiltin:imports env) -> lookupModuleInfo m env
                                _ -> Nothing

lookupModuleInfo            :: ModName -> EnvF x -> Maybe ModuleInfo  -- m is modname part of a GName, so search directly for module
lookupModuleInfo m env
  | inBuiltin env, m==mBuiltin
                            = Just (builtinModuleInfo env)
  | otherwise               = Map.lookup m (modules env)

-- A parent package of a loaded module is itself a valid module path prefix,
-- which the old NModule tree represented implicitly.
modulePrefix                :: ModName -> EnvF x -> Bool
modulePrefix (ModName ns) env
                            = any prefix (Map.keys (modules env))
  where prefix (ModName ns')= ns `isPrefixOf` ns'

-- The builtin module is looked up through the active environment while it is
-- itself being compiled.
builtinModuleInfo           :: EnvF x -> ModuleInfo
builtinModuleInfo env       = (mkModuleInfo mBuiltin [] (activeNames env ++ closedNames env) Nothing){ moduleLookupName = \n -> lookupName n env }

isMod                       :: EnvF x -> [Name] -> Bool
isMod env ns@(n:_)          = (maybe False (const True) (findModuleInfo (ModName ns) env) || modulePrefix (ModName ns) env) && rooted
  where rooted              = n `elem` improots env || isMAlias n env

isMAlias                    :: Name -> EnvF x -> Bool
isMAlias n env              = case lookupName n env of
                                Just NMAlias{} -> True
                                _ -> False

isAlias                     :: Name -> EnvF x -> Bool
isAlias n env               = case lookupName n env of
                                Just NAlias{} -> True
                                _ -> False

kindOf env (TVar _ tv)      = tvkind tv
kindOf env (TUni _ uv)      = uvkind uv
kindOf env (TCon _ tc)      = tconKind (tcname tc) env
kindOf env TFun{}           = KType
kindOf env TTuple{}         = KType
kindOf env TOpt{}           = KType
kindOf env TNone{}          = KType
kindOf env TWild{}          = KWild
kindOf env r@TNil{}         = rkind r
kindOf env r@TRow{}         = rkind r
kindOf env r@TStar{}        = rkind r
kindOf env TFX{}            = KFX


tconKind                    :: QName -> EnvF x -> Kind
tconKind n env              = case findQName n env of
                                NAct q _ _ _ _ -> kind KType q
                                NClass q _ _ _ -> kind KType q
                                NProto q _ _ _ -> kind KProto q
                                NReserved    -> nameReserved n
                                _            -> notClassOrProto n
  where kind k []           = k
        kind k q            = KFun [ tvkind v | QBind v _ <- q ] k

actorSelf env               = case lookupName selfKW env of
                                Just (NVar (TCon _ tc)) | isActor env (tcname tc) -> True
                                _ -> False

isDef                       :: EnvF x -> QName -> Bool
isDef env n                 = case tryQName n env of
                                Just NDef{} -> True
                                _ -> False

isActor                     :: EnvF x -> QName -> Bool
isActor env n               = case tryQName n env of
                                Just NAct{} -> True
                                _ -> False

isClass                     :: EnvF x -> QName -> Bool
isClass env n               = case tryQName n env of
                                Just NClass{} -> True
                                _ -> False

isProto                     :: EnvF x -> QName -> Bool
isProto env n               = case tryQName n env of
                                Just NProto{} -> True
                                _ -> False

isDefOrClass                :: EnvF x -> QName -> Bool
isDefOrClass env n          = case tryQName n env of
                                Just NDef{} -> True
                                Just NClass{} -> True
                                _ -> False


-- TCon queries ------------------------------------------------------------------------------------------------------------------

findAttr'                   :: EnvF x -> TCon -> Name -> (TSchema, Maybe Deco)
findAttr' env tc n          = case findAttr env tc n of
                                  Just (_, sc, mbdec) -> (sc, mbdec)
                                  Nothing -> error ("#### findAttr' fails for " ++ prstr tc ++ " . " ++ prstr n)

splitTC                     :: EnvF x -> TCon -> (Substitution, TCon)
splitTC env (TC n ts)       = (qbound q `zip` ts, TC n $ map tVar $ qbound q)
  where (q,_,_)             = findConName n env

findAncestry                :: EnvF x -> TCon -> [WTCon]
findAncestry env tc         = ([],tc) : fst (findCon env tc)

findAncestor                :: EnvF x -> TCon -> QName -> Maybe (Expr->Expr,TCon)
findAncestor env p qn       = listToMaybe [ (wexpr ws, p') | (ws,p') <- findAncestry env p, tcname p' == qn ]

hasAncestor'                :: EnvF x -> QName -> QName -> Bool
hasAncestor' env qn qn'     = qn' `elem` [ tcname c' | (w,c') <- us ]
  where (_,us,_)            = findConName qn env

hasAncestor                 :: EnvF x -> TCon -> TCon -> Bool
hasAncestor env c c'        = hasAncestor' env (tcname c) (tcname c')

commonAncestors             :: EnvF x -> TCon -> TCon -> [TCon]
commonAncestors env c1 c2   = filter ((`elem` ns) . tcname) $ map snd (findAncestry env c1)
  where ns                  = map (tcname . snd) (findAncestry env c2)

directAncestors             :: EnvF x -> QName -> [QName]
directAncestors env qn      = [ tcname p | (ws,p) <- us, null $ catRight ws ]
  where (q,us,_)            = findConName qn env

allAncestors                :: EnvF x -> TCon -> [TCon]
allAncestors env tc         = reverse [ schematic' c | (_, c) <- us ]
  where (us,te)             = findCon env tc

allDescendants              :: EnvF x -> TCon -> [TCon]
allDescendants env tc       = concatMap imported (importedModuleInfos env) ++ local
  where imported mi         = moduleDescendants mi (tcname tc)
        local               = [ schematic' c | c <- localCons env, hasAncestor' env (tcname c) (tcname tc) ]

importedModuleInfos         :: EnvF x -> [ModuleInfo]
importedModuleInfos env
  | inBuiltin env           = []
  | otherwise               = [ mi | m <- transitiveImports env, Just mi <- [lookupModuleInfo m env] ]

localCons                   :: EnvF x -> [TCon]
localCons env               = local (reverse (closedNames env)) ++ local (reverse (activeNames env))
  where local te
          | inBuiltin env   = [ TC (GName mBuiltin n) (wildargs i) | (n,i) <- te, isCon i ]
          | otherwise       = [ TC (NoQ n) (wildargs i) | (n,i) <- te, isCon i ]
        isCon NClass{}      = True
        isCon NAct{}        = True
        isCon _             = False

findCon                     :: EnvF x -> TCon -> ([WTCon],TEnv)
findCon env (TC n ts)
  | map tVar tvs == ts      = (us, te)
  | otherwise               = (vsubst s us, vsubst s te)
  where (q,us,te)           = findConName n env
        tvs                 = qbound q
        s                   = tvs `zip` ts

findConName n env           = case findQName n env of
                                NAct q p k te _  -> (q, [], notHidden te)
                                NClass q us te _ -> (q, us, te)
                                NProto q us te _ -> (q, us, te)
                                NExt q c us te _ _ -> (q, us, te)
                                NReserved -> nameReserved n
                                i -> err1 n ("findConName: Class or protocol name expected, got " ++ show i ++ " --- ")

conAttrs                    :: EnvF x -> QName -> [Name]
conAttrs env qn             = dom te
  where (_,_,te)            = findConName qn env

attributes                  :: (WPath -> NameInfo -> Name -> Maybe a) -> EnvF x -> TCon -> [a]
attributes f env tc         = catMaybes [ f wp i n | n <- ns, let Just (wp,i) = lookup n aenv ]
  where ns                  = nub $ reverse $ dom aenv                                                                                  -- in offset order
        aenv                = [ (n,(wp,i)) | (wp,c) <- findAncestry env tc, let (_,te) = findCon env c, (n,i) <- reverse te ]           -- in override order

fullAttrEnv                 :: EnvF x -> TCon -> TEnv
fullAttrEnv                 = attributes f
  where f wp i n            = Just (n,i)

parentTEnv                  :: EnvF x -> [WTCon] -> TEnv
parentTEnv env us           = [ (n,i) | (_,c) <- us, let (_,te) = findCon env c, (n,i) <- reverse te ]                                  -- in override order

findAttr                    :: EnvF x -> TCon -> Name -> Maybe (Expr->Expr, TSchema, Maybe Deco)
findAttr env tc n           = go (findAncestry env tc)
  where go []               = Nothing
        go ((wp,c):cs)      = maybe (go cs) (attr wp) (findAttrInfoIn n te)
          where (_,te)      = findCon env c
        attr wp (NSig sc d _) = Just (wexpr wp, sc, Just d)
        attr wp (NDef sc d _) = Just (wexpr wp, sc, Just d)
        attr wp (NVar t)      = Just (wexpr wp, monotype t, Nothing)
        attr wp (NSVar t)     = Just (wexpr wp, monotype t, Nothing)
        attr _ i              = error ("#### findAttr: Attribute expected, got " ++ show i)

findAttrInfo'               :: EnvF x -> QName -> Name -> Maybe NameInfo
findAttrInfo' env qn n      = go (([],tc) : us)
  where go []               = Nothing
        go ((_,c):cs)       = maybe (go cs) Just (findAttrInfoIn n te)
          where (_,_,te)    = findConName (tcname c) env
        (q,us,_)            = findConName qn env
        tc                  = TC qn [ tVar v | QBind v _ <- q ]

findAttrInfoIn              :: Name -> TEnv -> Maybe NameInfo
findAttrInfoIn n            = scan Nothing
  where scan r []           = r
        scan r ((x,i):xs)
          | x == n          = scan (Just i) xs
          | otherwise       = scan r xs

-- attributes'                 :: (WPath -> NameInfo -> Name -> Maybe a) -> EnvF x -> QName -> [a]
-- attributes' f env qn        = catMaybes [ f wp i n | n <- ns, let Just (wp,i) = lookup n aenv ]
--   where ns                  = nub $ reverse $ dom aenv                                                                                  -- in offset order
--         aenv                = [ (n,(wp,i)) | (wp,c) <- ([],tc) : us, let (_,_,te) = findConName (tcname c) env, (n,i) <- reverse te ]   -- in override order
--         (q,us,_)            = findConName qn env
--         tc                  = TC qn [ tVar v | QBind v _ <- q ]
-- The above function is replaced by the following version, as suggested in mail from Johan 260429.

attributes'                 :: (WPath -> NameInfo -> Name -> Maybe a) -> EnvF x -> QName -> [a]
attributes' f env qn        = catMaybes [ f wp i n | n <- ns, let Just (wp,i) = lookup n aenv ]
  where ns                  = nub $ reverse $ dom aenv                                                                                  -- in offset order
        aenv                = attrEnv env qn

attrEnv                     :: EnvF x -> QName -> [(Name,(WPath,NameInfo))]
attrEnv env qn              = [ (n,(wp,i)) | (wp,c) <- ([],tc) : us, let (_,_,te) = findConName (tcname c) env, (n,i) <- reverse te ]   -- in override order
  where (q,us,_)            = findConName qn env
        tc                  = TC qn [ tVar v | QBind v _ <- q ]

findAttrSchemas             :: EnvF x -> QName -> TEnv
findAttrSchemas env qn      = [ (n,i) | n <- ns, let Just (_,i) = lookup n aenv ]
  where ns                  = nub $ dom aenv
        aenv                = reverse $ attrEnv env qn

inheritedAttrs              :: EnvF x -> QName -> [(QName,Name)]
inheritedAttrs              = attributes' f
  where f _ NSig{} _        = Nothing
        f wp _ n            = case reverse wp of Left w : _ -> Just (w,n); _ -> Nothing

allAttrs'                   :: EnvF x -> TCon -> [Name]
allAttrs' env tc            = allAttrs env (tcname tc)

allAttrs                    :: EnvF x -> QName -> [Name]
allAttrs                    = attributes' f
  where f _ _ n             = Just n

directAttrs                 :: EnvF x -> QName -> [Name]
directAttrs                 = attributes' f
  where f wp _ n            = if null (catRight wp) then Just n else Nothing

abstractAttrs               :: EnvF x -> QName -> [Name]
abstractAttrs env n         = attributes' f env n
  where f _ (NSig _ dec _) n  = if dec == Property then Nothing else Just n
        f _ _ _             = Nothing

closedAttr                  :: EnvF x -> TCon -> Name -> Bool
closedAttr env tc n         = maybe False isClosed (findAttrInfo' env (tcname tc) n)

closedAttrs                 :: EnvF x -> QName -> [Name]
closedAttrs                 = attributes' f
  where
    f _ i n | isClosed i    = Just n
    f _ _ n                 = Nothing

isClosed (NVar _)                   = True
isClosed (NSVar _)                  = True
isClosed (NSig _ Property _)          = True
isClosed (NSig sc _ _)
  | TFun{} <- sctype sc             = False
  | otherwise                       = True      -- 'closed' ~ 'not a function'
isClosed _                          = False


abstractClass env n         = not $ null (abstractAttrs env n)

abstractActor env n         = not $ null (abstractAttrs env n)

abstractAttr                :: EnvF x -> TCon -> Name -> Bool
abstractAttr env tc n       = maybe False isAbstract (findAttrInfo' env (tcname tc) n)
  where isAbstract (NSig _ dec _) = dec /= Property
        isAbstract _              = False

transitiveImports env       = mBuiltin : reverse (foldl trav [] (getImports env))
  where trav seen m
          | m `elem` seen   = seen
          | otherwise       = m : foldl trav seen ms
          where ms          = case lookupModuleInfo m env of Just mi -> moduleImports mi

-- Enumerating every imported type forces each module's constructor records;
-- solver queries go through the narrow per-query indexes instead.
allTypes                    :: (NameInfo -> Bool) -> EnvF x -> [TCon]
allTypes select env         = concatMap impcons mods ++ localcons
  where mods                = transitiveImports env
        local te
          | inBuiltin env   = [ TC (GName mBuiltin n) (wildargs i) | (n,i) <- te, select i ]
          | otherwise       = [ TC (NoQ n) (wildargs i) | (n,i) <- te, select i ]
        localcons
                            = local (reverse (closedNames env)) ++ local (reverse (activeNames env))
        impcons m           = [ TC (GName m n) (wildargs i) | (n,i) <- te, select i ]
          where Just te     = moduleConstructors <$> lookupModuleInfo m env

allCons                     :: EnvF x -> [TCon]
allCons env                 = allTypes isCon env
  where isCon NClass{}      = True
        isCon NAct{}        = True
        isCon _             = False

-- Actors declared in imported modules, from the per-module actor index.
importedActors              :: EnvF x -> [TCon]
importedActors env          = concatMap moduleActors (importedModuleInfos env)

allActors                   :: EnvF x -> [TCon]
allActors env               = importedActors env ++ localactors
  where local te
          | inBuiltin env   = [ TC (GName mBuiltin n) (wildargs i) | (n,i@NAct{}) <- te ]
          | otherwise       = [ TC (NoQ n) (wildargs i) | (n,i@NAct{}) <- te ]
        localactors         = local (reverse (closedNames env)) ++ local (reverse (activeNames env))

allProtos env               = allTypes isProto env
  where isProto NProto{}    = True
        isProto _           = False

-- The module indexes record attributes where they are declared, so imported
-- owners are completed with the descendants of every declaring constructor
-- (inheriting an attribute means having a declaring ancestor).
-- Constructors carrying an attribute, declared in imported modules. The
-- indexes record attributes where they are declared, so imported owners are
-- completed with the descendants of every declaring constructor (inheriting an
-- attribute means having a declaring ancestor).
importedConAttr             :: EnvF x -> Name -> [TCon]
importedConAttr env n       = nubBy (\c c' -> tcname c == tcname c') (conOwners ++ inherited)
  where mis                 = importedModuleInfos env
        conOwners           = concat [ moduleConAttr mi n | mi <- mis ]
        protoOwners         = concat [ moduleProtoAttr mi n | mi <- mis ]
        inherited           = concat [ moduleDescendants mi (tcname o) | o <- conOwners ++ protoOwners, mi <- mis ]

allConAttr                  :: EnvF x -> Name -> [TCon]
allConAttr env n            = importedConAttr env n ++ [ tc | tc <- localCons env, hasAttr env tc n ]

allConAttrUFree             :: EnvF x -> Name -> [TUni]
allConAttrUFree env n       = concat [ ufree sc | tc <- activeCons env, Just (_,sc,_) <- [findAttr env tc n] ]

activeConAttr               :: EnvF x -> Name -> [TCon]
activeConAttr env n         = [ tc | tc <- activeCons env, hasAttr env tc n ]

activeCons                  :: EnvF x -> [TCon]
activeCons env              = [ TC (localQName x) (wildargs i) | (x,i) <- activeNames env, isCon i ]
  where isCon NClass{}      = True
        isCon NAct{}        = True
        isCon _             = False
        localQName x
          | inBuiltin env   = GName mBuiltin x
          | otherwise       = NoQ x

-- Protocols carrying an attribute, declared in imported modules.
importedProtoAttr           :: EnvF x -> Name -> [PCon]
importedProtoAttr env n     = nubBy (\p p' -> tcname p == tcname p') (owners ++ inherited)
  where mis                 = importedModuleInfos env
        owners              = concat [ moduleProtoAttr mi n | mi <- mis ]
        inherited           = concat [ moduleProtoDescendants mi (tcname o) | o <- owners, mi <- mis ]

allPConAttr                 :: EnvF x -> Name -> [PCon]
allPConAttr env n           = importedProtoAttr env n ++ [ p | p <- localProtos env, hasAttr env p n ]

localProtos                 :: EnvF x -> [PCon]
localProtos env             = local (reverse (closedNames env)) ++ local (reverse (activeNames env))
  where local te
          | inBuiltin env   = [ TC (GName mBuiltin n) (wildargs i) | (n,i@NProto{}) <- te ]
          | otherwise       = [ TC (NoQ n) (wildargs i) | (n,i@NProto{}) <- te ]

hasAttr                     :: EnvF x -> TCon -> Name -> Bool
hasAttr env tc n            = maybe False (const True) (findAttrInfo' env (tcname tc) n)


-- TVar queries ------------------------------------------------------------------------------------------------------------------

findSelf                    :: EnvF x -> TCon
findSelf env                = case findName (tvname tvSelf) env of
                                NTVar _ c ps -> c

findTVBound                 :: EnvF x -> TVar -> CCon
findTVBound env tv          = case findName (tvname tv) env of
                                NTVar _ c ps -> c
                                _ -> err1 tv "Unknown type variable"

findTVAttr                  :: EnvF x -> TVar -> Name -> Maybe (Expr->Expr, TSchema, Maybe Deco)
findTVAttr env tv n         = findAttr env c n
  where c                   = findTVBound env tv

tvarWit                     :: TVar -> PCon -> Name
tvarWit tv p                = Internal Witness (nstr $ Derived (deriveQ $ tcname p) (tvname tv)) 0

-- Method resolution order ------------------------------------------------------------------------------------------------------

mro2                                    :: EnvF x -> [TCon] -> ([WTCon],[WTCon])
mro2 env []                             = ([], [])
mro2 env (u:us)
  | isActor env (tcname u)              = err1 u "Actor subclassing not allowed"
  | isProto env (tcname u)              = ([], mro env (u:us))
  | otherwise                           = (mro env [u], mro env us)

mro1 env us                             = mro env us

mro                                     :: EnvF x -> [TCon] -> [WTCon]
mro env us                              = merge [] $ map lin us' ++ [us']
  where
    us'                                 = case us of [] -> []; u:us -> ([Left (tcname u)],u) : [ ([Right (tcname u)],u) | u <- us ]

    lin                                 :: WTCon -> [WTCon]
    lin (w,u)                           = (w,u) : [ (w++w',u') | (w',u') <- us' ]
      where (us',_)                     = findCon env u

    merge                               :: [WTCon] -> [[WTCon]] -> [WTCon]
    merge out lists
      | null heads                      = reverse out
      | h:_ <- good                     = merge (h:out) [ if equal hd h then tl else hd:tl | (hd,tl) <- zip heads tails ]
      | otherwise                       = err2 (map snd heads) "Inconsistent resolution order for"
      where (heads,tails)               = unzip [ (hd,tl) | hd:tl <- lists ]
            good                        = [ h | h <- heads, all (absent h) tails]

    equal                               :: WTCon -> WTCon -> Bool
    equal (w1,u1) (w2,u2)
      | tcname u1 == tcname u2          = tcargs u1 == tcargs u2 || err2 [u1,u2] "Inconsistent protocol instantiations"
      | otherwise                       = False

    absent                              :: WTCon -> [WTCon] -> Bool
    absent (w,h) us                     = tcname h `notElem` map (tcname . snd) us


----------------------------------------------------------------------------------------------------------------------
-- castable predicate
----------------------------------------------------------------------------------------------------------------------

castable                                    :: EnvF x -> Type -> Type -> Bool
castable env (TWild _) t2                   = True
castable env t1 (TWild _)                   = True

castable env (TCon _ c1) (TCon _ c2)
  | tcname c1 == tcname c2,
    tcname c1 `elem` covariant              = all (uncurry $ castable env) (tcargs c1 `zip` tcargs c2)
  | Just (wf,c') <- search                  = tcargs c2 == tcargs c'
  where search                              = findAncestor env c1 (tcname c2)

castable env (TFun _ fx1 p1 k1 t1) (TFun _ fx2 p2 k2 t2)
                                            = castable env fx1 fx2 && castable env p2 p1 && castable env k2 k1 && castable env t1 t2

castable env (TTuple _ p1 k1) (TTuple _ p2 k2)
                                            = castable env p1 p2 && castable env k1 k2

castable env (TOpt _ t1) (TOpt _ t2)        = castable env t1 t2
castable env (TNone _) (TOpt _ t)           = True
castable env (TNone _) (TNone _)            = True

castable env (TFX _ fx1) (TFX _ fx2)        = castable' fx1 fx2
  where castable' FXPure   FXPure           = True
        castable' FXPure   FXMut            = True
        castable' FXPure   FXProc           = True
        castable' FXMut    FXMut            = True
        castable' FXMut    FXProc           = True
        castable' FXProc   FXProc           = True
        castable' FXAction FXAction         = True
        castable' FXAction FXProc           = True
        castable' fx1      fx2              = False

castable env (TNil _ k1) (TNil _ k2)
  | k1 == k2                                = True
castable env (TRow _ k1 n1 t1 r1) (TRow _ k2 n2 t2 r2)
  | k1 == k2 && n1 == n2                    = castable env t1 t2 && castable env r1 r2
castable env (TStar _ k1 r1) (TStar _ k2 r2)
  | k1 == k2                                = castable env r1 r2

castable env (TVar _ tv1) (TVar _ tv2)
  | tv1 == tv2                              = True

castable env t1@(TVar _ tv) t2              = castable env (tCon c) t2
  where c                                   = findTVBound env tv

castable env t1 t2@(TVar _ tv)              = False

castable env t1 (TOpt _ t2)                 = castable env t1 t2

castable env t1 t2                          = False


headcast env t1 t2                          = castable env (schematic t1) (schematic t2)


----------------------------------------------------------------------------------------------------------------------
-- GLB
----------------------------------------------------------------------------------------------------------------------

glb env (TWild _) t2                    = pure tWild
glb env t1 (TWild _)                    = pure tWild

glb env t1@TVar{} t2@TVar{}
  | t1 == t2                            = pure t1
glb env (TUni _ u) _                    = pure tWild
glb env _ (TUni _ u)                    = pure tWild

glb env (TCon _ c1) (TCon _ c2)
  | tcname c1 == tcname c2              = pure $ tCon c1
  | hasAncestor env c1 c2               = pure $ tCon c1
  | hasAncestor env c2 c1               = pure $ tCon c2

glb env (TFun _ e1 p1 k1 t1) (TFun _ e2 p2 k2 t2)
                                        = do e <- glb env e1 e2
                                             (p, k) <- lub2 env p1 k1 p2 k2
                                             t <- glb env t1 t2
                                             return (tFun e p k t)
glb env t1@(TTuple _ p1 k1) t2@(TTuple _ p2 k2)
                                        = do --traceM ("## GLB " ++ prstr t1 ++ " \\/ " ++ prstr t2)
                                             (p, k) <- glb2 env p1 k1 p2 k2
                                             --traceM ("## GLB " ++ prstr t1 ++ " \\/ " ++ prstr t2 ++ "  =  " ++ prstr (tTuple p k))
                                             return (tTuple p k)

glb env (TOpt _ t1) (TOpt _ t2)         = tOpt <$> glb env t1 t2
glb env (TNone _) t2                    = pure tNone
glb env t1 (TNone _)                    = pure tNone
glb env (TOpt _ t1) t2                  = glb env t1 t2
glb env t1 (TOpt _ t2)                  = glb env t1 t2

glb env t1@(TFX _ fx1) t2@(TFX _ fx2)
  | Just fx <- glfx fx1 fx2             = pure $ tTFX fx
  where glfx FXPure   FXPure            = Just FXPure
        glfx FXPure   FXMut             = Just FXPure
        glfx FXPure   FXProc            = Just FXPure
        glfx FXPure   FXAction          = Nothing
        glfx FXMut    FXPure            = Just FXPure
        glfx FXMut    FXMut             = Just FXMut
        glfx FXMut    FXProc            = Just FXMut
        glfx FXMut    FXAction          = Nothing
        glfx FXProc   FXPure            = Just FXPure
        glfx FXProc   FXMut             = Just FXMut
        glfx FXProc   FXProc            = Just FXProc
        glfx FXProc   FXAction          = Just FXAction
        glfx FXAction FXPure            = Nothing
        glfx FXAction FXMut             = Nothing
        glfx FXAction FXProc            = Just FXAction
        glfx FXAction FXAction          = Just FXAction

glb env (TNil _ k1) (TNil _ k2)
  | k1 == k2                            = pure $ tNil k1
glb env (TRow _ k1 n1 t1 r1) (TRow _ k2 n2 t2 r2)
  | k1 == k2 && n1 == n2                = tRow k1 n1 <$> glb env t1 t2 <*> glb env r1 r2
glb env (TStar _ k1 r1) (TStar _ k2 r2)
  | k1 == k2                            = tStar k1 <$> glb env r1 r2

glb env t1 t2                           = Nothing


glb2 env (TRow _ _ _ t1 p1) k1 (TRow _ _ _ t2 p2) k2
                                        = do t <- glb env t1 t2
                                             (p,k) <- glb2 env p1 k1 p2 k2
                                             return (posRow t p, k)
glb2 env p1@TRow{} k1 p2@TNil{} (TRow _ _ _ t2 k2)
                                        = glb2 env p1 k1 (posRow t2 p2) k2
glb2 env p1@TNil{} (TRow _ _ _ t1 k1) p2@TRow{} k2
                                        = glb2 env (posRow t1 p1) k1 p2 k2
glb2 env p1 k1 p2 k2                    = do p <- glb env p1 p2
                                             k <- glb env k1 k2
                                             return (p, k)


glbfold env ts                          = case filter (/= tWild) ts of
                                            [] -> pure tWild
                                            t:ts -> foldM (glb env) t ts


----------------------------------------------------------------------------------------------------------------------
-- LUB
----------------------------------------------------------------------------------------------------------------------

lub env (TWild _) t2                    = pure tWild
lub env t1 (TWild _)                    = pure tWild

lub env t1@TVar{} t2@TVar{}
  | t1 == t2                            = pure t1
lub env t (TVar _ v)                    = lub env t (tCon $ findTVBound env v)
lub env (TVar _ v) t                    = lub env (tCon $ findTVBound env v) t

lub env (TUni _ u) _                    = pure tWild
lub env _ (TUni _ u)                    = pure tWild

lub env (TCon _ c1) (TCon _ c2)
  | tcname c1 == tcname c2              = pure $ tCon c1
  | hasAncestor env c1 c2               = pure $ tCon c2
  | hasAncestor env c2 c1               = pure $ tCon c1
  | not $ null common                   = pure $ tCon $ head common
  where common                          = commonAncestors env c1 c2

lub env f1@(TFun _ e1 p1 k1 t1) f2@(TFun _ e2 p2 k2 t2)
                                        = do e <- lub env e1 e2
                                             (p,k) <- glb2 env p1 k1 p2 k2
                                             t <- lub env t1 t2
                                             --traceM ("## LUB " ++ prstr f1 ++ " /\\ " ++ prstr f2 ++ "  =  " ++ prstr (tFun e p k t))
                                             return $ tFun e p k t
lub env t1@(TTuple _ p1 k1) t2@(TTuple _ p2 k2)
                                        = do (p,k) <- lub2 env p1 k1 p2 k2
                                             --traceM ("## LUB " ++ prstr t1 ++ " /\\ " ++ prstr t2 ++ "  =  " ++ prstr (tTuple p k))
                                             return $ tTuple p k

lub env (TOpt _ t1) (TOpt _ t2)         = tOpt <$> lub env t1 t2
lub env (TNone _) (TNone _)             = pure tNone
lub env (TNone _) t2@TOpt{}             = pure t2
lub env t1@TOpt{} (TNone _)             = pure t1
lub env (TNone _) t2                    = pure $ tOpt t2
lub env t1 (TNone _)                    = pure $ tOpt t1
lub env (TOpt _ t1) t2                  = tOpt <$> lub env t1 t2
lub env t1 (TOpt _ t2)                  = tOpt <$> lub env t1 t2

lub env t1@(TFX _ fx1) t2@(TFX _ fx2)   = pure $ tTFX (lufx fx1 fx2)
  where lufx FXPure   FXPure            = FXPure
        lufx FXPure   FXMut             = FXMut
        lufx FXPure   FXProc            = FXProc
        lufx FXPure   FXAction          = FXProc
        lufx FXMut    FXPure            = FXMut
        lufx FXMut    FXMut             = FXMut
        lufx FXMut    FXProc            = FXProc
        lufx FXMut    FXAction          = FXProc
        lufx FXProc   FXPure            = FXProc
        lufx FXProc   FXMut             = FXProc
        lufx FXProc   FXProc            = FXProc
        lufx FXProc   FXAction          = FXProc
        lufx FXAction FXPure            = FXProc
        lufx FXAction FXMut             = FXProc
        lufx FXAction FXProc            = FXProc
        lufx FXAction FXAction          = FXAction

lub env (TNil _ k1) (TNil _ k2)
  | k1 == k2                            = pure $ tNil k1
lub env (TRow _ k1 n1 t1 r1) (TRow _ k2 n2 t2 r2)
  | k1 == k2 && n1 == n2                = tRow k1 n1 <$> lub env t1 t2 <*> lub env r1 r2
lub env (TStar _ k1 r1) (TStar _ k2 r2)
  | k1 == k2                            = tStar k1 <$> lub env r1 r2
lub env (TUnboxed _ t1) t2              = lub env t1 t2
lub env t1 (TUnboxed _ t2)              = lub env t1 t2
lub env t1 t2                           = Nothing



lub2 env (TRow _ _ _ t1 p1) k1 (TRow _ _ _ t2 p2) k2
                                        = do t <- lub env t1 t2
                                             (p,k) <- lub2 env p1 k1 p2 k2
                                             return (posRow t p, k)
lub2 env (TRow _ _ _ t1 p1) k1 p2@TNil{} k2@TRow{}
                                        = lub2 env p1 (kwdRow (label k2) t1 k1) p2 k2
lub2 env p1@TNil{} k1@TRow{} (TRow _ _ _ t2 p2) k2
                                        = lub2 env p1 k1 p2 (kwdRow (label k1) t2 k2)
lub2 env p1 k1 p2 k2                    = do p <- lub env p1 p2
                                             k <- lub env k1 k2
                                             return (p, k)


lubfold env ts                          = case filter (/= tWild) ts of
                                            [] -> pure tWild
                                            t:ts -> foldM (lub env) t ts


-- Control flow --------------------

data Flow                           = RET | BRK | CNT | SEQ deriving (Eq, Show)

class Flows a where
    flows                           :: a -> [Flow]

fallsthru x                         = SEQ `elem` flows x

brkseq flow                         = (if BRK `elem` flow then SEQ:flow else flow) \\ [BRK,CNT]


instance Flows a => Flows [a] where
    flows []                        = [SEQ]
    flows (s : ss)                  = flows s `seq` flows ss
      where f1 `seq` f2             = if SEQ `elem` f1 then (f1\\[SEQ])++f2 else f1

instance Flows Stmt where
    flows (Expr _ e)
      | e == eNotImpl               = []                -- Not tracked
      | Call _ (Var _ n) _ _ <- e,
        n == primRAISE              = []                -- Not tracked
    flows Raise{}                   = []                -- Not tracked
    flows Return{}                  = [RET]
    flows Break{}                   = [BRK]
    flows Continue{}                = [CNT]
    flows (If _ bs els)             = concatMap flows bs ++ flows els
    flows (While _ _ b els)         = brkseq (flows b) ++ flows els
    flows (For _ _ _ b els)         = brkseq (flows b) ++ flows els
    flows (With _ _ b)              = flows b
    flows (Try _ b hs els fin)      = flows (b++els) ++ concatMap flows hs ++ (flows fin \\ [SEQ])
    flows _                         = [SEQ]

instance Flows Branch where
    flows (Branch _ ss)             = flows ss

instance Flows Handler where
    flows (Handler _ ss)            = flows ss



-- Import handling (local definitions only) -------------------------------------------------------------------------

--getImps                         :: [FilePath] -> EnvF x -> [Import] -> IO (EnvF x)
getImps spath env []         = return env
getImps spath env (i:is)     = do env' <- impModule spath env i
                                  getImps spath env' is


--impModule                       :: [FilePath] -> EnvF x -> Import -> IO (EnvF x)
impModule spath env (Import _ ms)
                                = imp env ms
  where imp env []              = return env
        imp env (ModuleItem m as : is)
                                = do (env1,te) <- doImp spath env m
                                     let env2 = maybe (addImpRoot m) (\n->defineClosed [(n, NMAlias m)]) as env1
                                     imp env2 is
impModule spath env (FromImport _ (ModRef (0,Just m)) items)
                                = do (env1,te) <- doImp spath env m
                                     return $ importSome items m te env1
impModule spath env (FromImportAll _ (ModRef (0,Just m)))
                                = do (env1,te) <- doImp spath env m
                                     return $ importAll m te env1
impModule _ _ i                 = illegalImport (loc i)


subImp spath env []          = return env
subImp spath env (m:ms)      = do (env',_) <- doImp spath env m
                                  subImp spath env' ms

findTyFile spaths mn = go spaths
  where
    go []     = return Nothing
    go (p:ps) = do
      let fullPath = InterfaceFiles.interfacePath p mn
      exists <- InterfaceFiles.interfaceExists fullPath
      --traceM ("findTyFile: " ++ fullPath ++ " " ++ show exists ++ "\n")
      if exists
        then return (Just fullPath)
        else go ps

-- | Import a module, loading its .tydb and extending the environment.
doImp                        :: [FilePath] -> EnvF x -> ModName -> IO (EnvF x, ModuleInfo)
doImp spath env m            = do (env', mi, _) <- doImpSeen S.empty env m
                                  return (addImport m env', mi)
  where
    -- A cached module still needs its recorded import closure available in the
    -- environment. If the module is already loaded, its ModuleInfo carries
    -- that closure; otherwise we read it from the .tydb.
    doImpSeen seen env m
      | S.member m seen      =
          case lookupModuleInfo m env of
            Just mi -> return (env, mi, seen)
            Nothing -> fileNotFound m
      | otherwise            =
          let seen' = S.insert m seen in
          case lookupModuleInfo m env of
            Just mi -> do
              (env', seen'') <- subImpSeen seen' env (moduleImports mi)
              return (env', mi, seen'')
            Nothing -> do
              tyFile <- findTyFile spath m
              mdb <- maybe (return Nothing) InterfaceFiles.openInterfaceDBMaybe tyFile
              case mdb of
                Nothing -> fileNotFound m
                Just db -> do
                  (ms, mdoc) <- InterfaceFiles.readInterfaceDBModuleInfo db
                  (env', seen'') <- subImpSeen seen' env ms
                  let mi = mkTyFileModuleInfo m ms mdoc db
                  return (addModuleInfo m mi env', mi, seen'')

    subImpSeen seen env []   = return (env, seen)
    subImpSeen seen env (m:ms) = do
      (env', _, seen') <- doImpSeen seen env m
      subImpSeen seen' env' ms

importSome                  :: [ImportItem] -> ModName -> ModuleInfo -> EnvF x -> EnvF x
importSome items m mi env   = defineClosed (map pick items) env
  where
    pick (ImportItem n mbn) = case impName m mi n of
                                    Just i  -> (maybe n id mbn, i)
                                    Nothing -> noItem m n

importAll                   :: ModName -> ModuleInfo -> EnvF x -> EnvF x
importAll m mi env          = defineClosed (impNames m mi) env

impName                     :: ModName -> ModuleInfo -> Name -> Maybe NameInfo
impName m mi n              = case moduleLookupName mi n of
                                Just NAct{}   -> Just (NAlias (GName m n))
                                Just NClass{} -> Just (NAlias (GName m n))
                                Just NProto{} -> Just (NAlias (GName m n))
                                Just NExt{}   -> Nothing
                                Just NAlias{} -> Just (NAlias (GName m n))
                                Just NVar{}   -> Just (NAlias (GName m n))
                                Just NDef{}   -> Just (NAlias (GName m n))
                                _             -> Nothing

impNames                    :: ModName -> ModuleInfo -> TEnv
impNames m mi               = mapMaybe imp (modulePublicNames mi)
  where
    imp n                   = fmap (\i -> (n,i)) (impName m mi n)



-- Error handling ----------------------------------------------------------------------------------------------------

data CompilationError               = KindError SrcLoc Kind Kind
                                    | InfiniteKind SrcLoc KUni Kind
                                    | VariableFX TVar

                                    | FileNotFound ModName
                                    | NameNotFound Name
                                    | NameReserved QName
                                    | NameBlocked QName
                                    | NameUnexpected QName
                                    | TypedReassign Pattern
                                    | IllegalRedef Name
                                    | IllegalSigOverride Name
                                    | IllegalExtension QName
                                    | MissingSelf Name
                                    | IllegalImport SrcLoc
                                    | DuplicateImport Name
                                    | NoItem ModName Name
                                    | NoModule ModName
                                    | NoClassOrProto QName
                                    | DecorationMismatch Name TSchema Deco
                                    | SelfParamError SrcLoc
                                    | OtherError SrcLoc String
                                    deriving (Show)

instance Control.Exception.Exception CompilationError

instance HasLoc CompilationError where
    loc (KindError l _ _)           = l
    loc (InfiniteKind l _ _)        = l
    loc (VariableFX tv)             = loc tv

    loc (FileNotFound n)            = loc n
    loc (NameNotFound n)            = loc n
    loc (NameReserved n)            = loc n
    loc (NameBlocked n)             = loc n
    loc (NameUnexpected n)          = loc n
    loc (TypedReassign p)           = loc p
    loc (IllegalRedef n)            = loc n
    loc (IllegalSigOverride n)      = loc n
    loc (IllegalExtension n)        = loc n
    loc (MissingSelf n)             = loc n
    loc (IllegalImport l)           = l
    loc (DuplicateImport n)         = loc n
    loc (NoModule m)                = loc m
    loc (NoItem m n)                = loc n
    loc (NoClassOrProto n)          = loc n
    loc (DecorationMismatch n t d)  = loc n
    loc (SelfParamError l)          = l
    loc (OtherError l str)          = l


compilationError                    :: CompilationError -> [(SrcLoc, String)]
compilationError err                = [(loc err, render (expl err))]
  where
    expl (KindError l k1 k2)        = text "Expected a" <+> pretty k2 <> comma <+> text "actual kind is" <+> pretty k1
    expl (InfiniteKind l v k)       = text "Infinite kind inferred:" <+> pretty v <+> equals <+> pretty k
    expl (VariableFX tv)            = text "Effect annotation cannot be a variable:" <+> pretty tv

    expl (FileNotFound n)           = text "Type interface file not found for" <+> pretty n
    expl (NameNotFound n)           = text "Name" <+> pretty n <+> text "is not in scope"
    expl (NameReserved n)           = text "Name" <+> pretty n <+> text "is reserved but not yet defined"
    expl (NameBlocked n)            = text "Name" <+> pretty n <+> text "is currently not accessible"
    expl (NameUnexpected n)         = text "Unexpected variable name:" <+> pretty n
    expl (TypedReassign p)          = text "Type annotation on reassignment:" <+> pretty p
    expl (IllegalRedef n)           = text "Illegal redefinition of" <+> pretty n
    expl (IllegalSigOverride n)     = text "Illegal signature override:" <+> pretty n
    expl (IllegalExtension n)       = text "Illegal extension of" <+> pretty n
    expl (MissingSelf n)            = text "Missing 'self' parameter in definition of"
    expl (IllegalImport l)          = text "Relative import not yet supported"
    expl (DuplicateImport n)        = text "Duplicate import of name" <+> pretty n
    expl (NoModule m)               = text "Module" <+> pretty m <+> text "does not exist"
    expl (NoItem m n)               = text "Module" <+> pretty m <+> text "does not export" <+> pretty n
    expl (NoClassOrProto n)         = text "Class or protocol name expected, got" <+> pretty n
    expl (DecorationMismatch n t d) = text "Decoration for" <+> pretty n <+> text "does not match signature" <+> pretty d
    expl (SelfParamError l)         = text "'self' cannot be used as a parameter name in actors."
    expl (OtherError l str)         = text str


noKUnify l k1 k2                    = Control.Exception.throw $ KindError l k1 k2
infiniteKind l v k                  = Control.Exception.throw $ InfiniteKind l v k
variableFX tv                       = Control.Exception.throw $ VariableFX tv

nameNotFound n                      = Control.Exception.throw $ NameNotFound n
nameReserved n                      = Control.Exception.throw $ NameReserved n
nameBlocked n                       = Control.Exception.throw $ NameBlocked n
nameUnexpected n                    = Control.Exception.throw $ NameUnexpected n
typedReassign p                     = Control.Exception.throw $ TypedReassign p
illegalRedef n                      = Control.Exception.throw $ IllegalRedef n
illegalSigOverride n                = Control.Exception.throw $ IllegalSigOverride n
illegalExtension n                  = Control.Exception.throw $ IllegalExtension n
missingSelf n                       = Control.Exception.throw $ MissingSelf n
fileNotFound n                      = Control.Exception.throw $ FileNotFound n
illegalImport l                     = Control.Exception.throw $ IllegalImport l
duplicateImport n                   = Control.Exception.throw $ DuplicateImport n
noItem m n                          = Control.Exception.throw $ NoItem m n
noModule m                          = Control.Exception.throw $ NoModule m
notClassOrProto n                   = Control.Exception.throw $ NoClassOrProto n
decorationMismatch n t d            = Control.Exception.throw $ DecorationMismatch n t d
selfParamError l                    = Control.Exception.throw $ SelfParamError l
err l s                             = Control.Exception.throw $ OtherError l s

err0 xs s                           = err (loc $ head xs) s
err1 x s                            = err (loc x) (s ++ " " ++ prstr x)
err2 xs s                           = err (loc $ head xs) (s ++ " " ++ prstrs xs)
err3 l xs s                         = err l (s ++ " " ++ prstrs xs)

notYetExpr e                        = notYet (loc e) e

stripQual q                         = [ QBind v [] | QBind v us <- q ]


class Simp a where
    simp                            :: EnvF x -> a -> a

instance (Simp a) => Simp [a] where
    simp env                        = map (simp env)

instance Simp TSchema where
    simp env (TSchema l q t)        = TSchema l q' (vsubst s $ simp env' t)
      where (q', s)                 = simpQuant env (simp env' q) (vfree t)
            env'                    = defineTVars (stripQual q) env

simpQuant env q vs0                 = (vsubst s [ QBind v ps | QBind v ps <- q2, not $ null ps ], s)
  where (q1,q2)                     = partition isEX q
        isEX (QBind v [p])          = length (filter (==v) vs) == 1
        isEX _                      = False
        vs                          = concat [ vfree ps | QBind v ps <- q ] ++ vs0
        s                           = [ (v, tCon p) | QBind v [p] <- q1 ]                       -- Inline existentials

instance Simp QBind where
    simp env (QBind v ps)           = QBind v (simp env ps)

instance Simp WTCon where
    simp env (w, c)                 = (w, simp env c)

instance Simp (Name, NameInfo) where
    simp env (n, NSig sc dec doc)   = (n, NSig (simp env sc) dec doc)
    simp env (n, NDef sc dec doc)   = (n, NDef (simp env sc) dec doc)
    simp env (n, NVar t)            = (n, NVar (simp env t))
    simp env (n, NSVar t)           = (n, NSVar (simp env t))
    simp env (n, NClass q us te doc)= (n, NClass (simp env' q) (simp env' us) (simp env' te) doc)
      where env'                    = defineTVars (stripQual q) env
    simp env (n, NProto q us te doc)= (n, NProto (simp env' q) (simp env' us) (simp env' te) doc)
      where env'                    = defineTVars (stripQual q) env
    simp env (n, NExt q c us te opts doc)
                                    = (n, NExt q' (vsubst s $ simp env' c) (vsubst s $ simp env' us) (vsubst s $ simp env' te) opts doc)
      where (q', s)                 = simpQuant env (simp env' q) (vfree c ++ vfree us ++ vfree te)
            env'                    = defineTVars (stripQual q) env
    simp env (n, NAct q p k te doc) = (n, NAct (simp env' q) (simp env' p) (simp env' k) (simp env' $ notHidden te) doc)
      where env'                    = defineTVars (stripQual q) env
    simp env (n, i)                 = (n, i)

instance Simp Type where
    simp env (TCon l c)             = TCon l (simp env c)
    simp env (TFun l fx p k t)      = TFun l (simp env fx) (simp env p) (simp env k) (simp env t)
    simp env (TTuple l p k)         = TTuple l (simp env p) (simp env k)
    simp env (TOpt l t)             = TOpt l (simp env t)
    simp env (TRow l k n t r)       = TRow l k n (simp env t) (simp env r)
    simp env (TStar l k r)          = TStar l k (simp env r)
    simp env t                      = t

instance Simp TCon where
    simp env (TC n ts)              = TC (simp env n) (simp env ts)                             -- Simplify constructor names

instance Simp QName where
    simp env (GName m n)
      | inBuiltin env               = NoQ n                                                     -- Restore builtins
      | Just m == thismod env       = NoQ n                                                     -- Restore locals
    simp env n
      | Just n1 <- findIndexedAlias n = NoQ n1
      | Just n1 <- findAlias (activeNames env) = NoQ n1                                      -- Restore aliases
      | Just n1 <- findAlias (closedNames env) = NoQ n1                                      -- Restore aliases
      | otherwise                   = n
      where findIndexedAlias qn@(GName _ n1) = case lookupName n1 env of
                                                  Just (NAlias n2) | n2 == qn -> Just n1
                                                  _ -> Nothing
            findIndexedAlias qn@(QName _ n1) = case lookupName n1 env of
                                                  Just (NAlias n2) | n2 == qn -> Just n1
                                                  _ -> Nothing
            findIndexedAlias _               = Nothing
            findAlias ((n1, NAlias n2):te)
              | n2 == n             = Just n1
            findAlias (_:te)        = findAlias te
            findAlias []            = Nothing
