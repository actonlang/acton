{-# LANGUAGE FlexibleInstances, DeriveGeneric #-}
module Acton.Env where

import qualified Control.Exception
import qualified Data.Binary
import GHC.Generics (Generic)
import Data.Typeable
import System.FilePath.Posix (joinPath,takeDirectory)
import System.Directory (doesFileExist)
import System.Environment (getExecutablePath)
import Control.Monad

import Acton.Syntax
import Acton.Builtin
import Acton.Printer
import Acton.Names
import Acton.Subst
import Acton.TypeM
import Utils
import Pretty
import InterfaceFiles
import Prelude hiding ((<>))




mkEnv                       :: (FilePath,FilePath) -> Env0 -> [Import] -> IO Env0
mkEnv paths env imports     = getImps paths env imports

typeX0                      = TypeX { actorstate = Nothing,
                                      context = CtxTop,
                                      indecl = False }


type Schemas                = [(Name, TSchema)]

type TEnv                   = [(Name, NameInfo)]

data EnvF x                 = EnvF {
                                names      :: TEnv,
                                modules    :: [(ModName,TEnv)],
                                witnesses  :: [(QName,Witness)],
                                envX       :: x }

type Env0                   = EnvF ()


setX                        :: EnvF x -> y -> EnvF y
setX env x                  = EnvF { names = names env, modules = modules env, witnesses = witnesses env, envX = x }

modX                        :: EnvF x -> (x -> x) -> EnvF x
modX env f                  = env{ envX = f (envX env) }

data TypeX                  = TypeX {
                                actorstate :: Maybe Type,
                                context    :: EnvCtx,
                                indecl     :: Bool }

type Env                    = EnvF TypeX

data EnvCtx                 = CtxTop | CtxAct | CtxClass deriving (Eq,Show)

{-  TEnv principles:
    -   A TEnv is an association of NameInfo details to a list of names.
    -   NSig holds the schema of an explicit Signature, while NDef and NVar give schemas and types to names created by Defs and assignments.
    -   NClass, NProto, NExt and NAct represent class, protocol, extension and actor declarations. They each contain a TEnv of visible local attributes.
    -   Signatures must appear before the defs/assignments they describe, and every TEnv respects the order of the syntactic constructs binding each name.
    -   The attribute TEnvs of NClass, NProto, NExt and NAct are searched left-to-right, thus favoring (explicit) NSigs over (inferred) NDefs/NVars.
    -   The global inference TEnv (names env) is searched right-to-left, thereby prioritizing NDefs/NVars over NSigs, as well as any inner bindings in scope.
    -   The NameInfo assumption on a (recursive) Def is always an NDef, initialized to the corresponding NSig if present, or a fresh unquantified variable.
    -   The inferred schema for each def is checked to be no less general than the corresponding NDef assumption.
    -   Unquantified NDefs are generalized at the close of the outermost recursive declaration in scope.
    -   An NSig is always fully quantified, not possible to generalize
    -   To enable method override (and disable method signature override), the NSigs of parent class are inserted into the global env when checking a child class
    -   For the same reason, NDefs and NVars without an NSig of a parent class are inserted as NSigs when a child class is checked


-}

data NameInfo               = NVar      Type
                            | NSVar     Type
                            | NDef      TSchema Deco
                            | NSig      TSchema Deco
                            | NAct      QBinds PosRow KwdRow TEnv
                            | NClass    QBinds [WTCon] TEnv
                            | NProto    QBinds [WTCon] TEnv
                            | NExt      QName QBinds [WTCon] TEnv
                            | NTVar     Kind (Maybe TCon)
                            | NAlias    QName
                            | NMAlias   ModName
                            | NModule   TEnv
                            | NReserved
                            | NBlocked
                            deriving (Eq,Show,Read,Generic)

data Witness                = WClass    { binds::QBinds, proto::TCon, wname::QName, wsteps::[Maybe QName] }
                            | WInst     { proto::TCon, wname::QName, wsteps::[Maybe QName] }
                            deriving (Show)

type WTCon                  = ([Maybe QName],TCon)

instance Data.Binary.Binary NameInfo


-- Equality modulo qualified/unqualified type names

class QMatch a where
    qmatch                          :: EnvF x -> a -> a -> Bool

instance QMatch [Type] where
    qmatch env as bs                = and [ qmatch env a b | (a,b) <- as `zip` bs ]

instance QMatch (QName,Witness) where
    qmatch env (x,a) (y,b)          = qmatch env x y && m a b
      where m a@WClass{} b@WClass{} = qmatch env (tcname (proto a)) (tcname (proto b))
            m a@WInst{}  b@WInst{}  = qmatch env (tcname (proto a)) (tcname (proto b))
            m a          b          = False

instance QMatch QName where
    qmatch env a b                  = m (unalias env a) (unalias env b)
      where m a@QName{}  b@QName{}  = mname a == mname b && noq a == noq b
            m a@NoQ{}    b@NoQ{}    = noq a == noq b
            m a@NoQ{}    b@QName{}  = mname b == mBuiltin && noq a == noq b
            m a@QName{}  b@NoQ{}    = mname a == mBuiltin && noq a == noq b

instance QMatch TCon where
    qmatch env (TC a ts) (TC b us)  = qmatch env a b && qmatch env ts us

instance QMatch Type where
    qmatch env (TVar _ v1) (TVar _ v2)                      = univar v1 || univar v2 || v1 == v2
    qmatch env (TCon _ c1) (TCon _ c2)                      = qmatch env c1 c2
    qmatch env (TFun _ e1 p1 r1 t1) (TFun _ e2 p2 r2 t2)    = qmatch env e1 e2 && qmatch env p1 p2 && qmatch env r1 r2 && qmatch env t1 t2
    qmatch env (TTuple _ p1 r1) (TTuple _ p2 r2)            = qmatch env p1 p2 && qmatch env r1 r2
    qmatch env (TUnion _ u1) (TUnion _ u2)                  = qmatch env u1 u2
    qmatch env (TOpt _ t1) (TOpt _ t2)                      = qmatch env t1 t2
    qmatch env (TNone _) (TNone _)                          = True
    qmatch env (TWild _) (TWild _)                          = True
    qmatch env (TNil _ s1)  (TNil _ s2)                     = s1 == s2
    qmatch env (TRow _ s1 n1 t1 r1) (TRow _ s2 n2 t2 r2)    = s1 == s2 && n1 == n2 && qmatch env t1 t2 && qmatch env r1 r2
    qmatch env (TFX _ fx1) (TFX _ fx2)                      = qmatch env fx1 fx2
    qmatch env _ _                                          = False

instance QMatch FX where
    qmatch env FXPure FXPure        = True
    qmatch env (FXMut a) (FXMut b)  = qmatch env a b
    qmatch env (FXAct a) (FXAct b)  = qmatch env a b
    qmatch env FXAction FXAction    = True
    qmatch env _ _                  = False

instance QMatch [UType] where
    qmatch env as bs                = and [ any (qmatch env a) bs | a <- as ] && and [ any (qmatch env b) as | b <- bs ]

instance QMatch UType where
    qmatch env (UCon a) (UCon b)    = qmatch env a b
    qmatch env (ULit a) (ULit b)    = a == b
    qmatch env _ _                  = False

instance Pretty (QName,Witness) where
    pretty (n, WClass q p w ws) = text "WClass" <+> pretty n <+> nonEmpty brackets commaList q <+> parens (pretty p) <+>
                                  equals <+> pretty (wexpr ws (eCall (eQVar w) []))
    pretty (n, WInst p w ws)    = text "WInst" <+> pretty n <+> parens (pretty p) <+>
                                  equals <+> pretty (wexpr ws (eQVar w))
        
instance Pretty TEnv where
    pretty tenv                 = vcat (map pretty tenv)

instance (Pretty x) => Pretty (EnvF x) where
    pretty env                  = vcat (map pretty (names env)) $+$
                                  vcat (map pretty (witnesses env)) $+$
                                  text "---"  $+$
                                  pretty (envX env) $+$
                                  text "."

instance Pretty (Name,NameInfo) where
    pretty (n, NVar t)          = pretty n <+> colon <+> pretty t
    pretty (n, NSVar t)         = text "var" <+> pretty n <+> colon <+> pretty t
    pretty (n, NDef t d)        = prettyDec d $ pretty n <+> colon <+> pretty t
    pretty (n, NSig t d)        = prettyDec d $ pretty n <+> text ":::" <+> pretty t
    pretty (n, NAct q p k [])   = text "actor" <+> pretty n <> nonEmpty brackets commaList q <+>
                                  parens (prettyFunRow p k) <> colon <+> text "pass"
    pretty (n, NAct q p k te)   = text "actor" <+> pretty n <> nonEmpty brackets commaList q <+>
                                  parens (prettyFunRow p k) <> colon $+$ (nest 4 $ pretty te)
    pretty (n, NClass q us [])  = text "class" <+> pretty n <> nonEmpty brackets commaList q <+>
                                  nonEmpty parens commaList us <> colon <+> text "pass"
    pretty (n, NClass q us te)  = text "class" <+> pretty n <> nonEmpty brackets commaList q <+>
                                  nonEmpty parens commaList us <> colon $+$ (nest 4 $ pretty $ prioSig te)
    pretty (n, NProto q us [])  = text "protocol" <+> pretty n <> nonEmpty brackets commaList q <+>
                                  nonEmpty parens commaList us <> colon <+> text "pass"
    pretty (n, NProto q us te)  = text "protocol" <+> pretty n <> nonEmpty brackets commaList q <+>
                                  nonEmpty parens commaList us <> colon $+$ (nest 4 $ pretty $ prioSig te)
    pretty (w, NExt n [] ps te) = pretty w  <+> colon <+> text "extension" <+> pretty n <+> parens (commaList ps) <>
                                  colon $+$ (nest 4 $ pretty te) <> colon <+> text "pass"
    pretty (w, NExt n q ps te)  = pretty w  <+> colon <+> pretty q <+> text "=>" <+> text "extension" <+> pretty n <> 
                                  brackets (commaList $ tybound q) <+> parens (commaList ps) <>
                                  colon $+$ (nest 4 $ pretty te)
    pretty (n, NTVar k mba)     = pretty n <> maybe empty (parens . pretty) mba
    pretty (n, NAlias qn)       = text "alias" <+> pretty n <+> equals <+> pretty qn
    pretty (n, NMAlias m)       = text "module" <+> pretty n <+> equals <+> pretty m
    pretty (n, NModule te)      = text "module" <+> pretty n <> colon $+$ nest 4 (pretty te)
    pretty (n, NReserved)       = pretty n <+> text "(reserved)"
    pretty (n, NBlocked)        = pretty n <+> text "(blocked)"

instance Pretty WTCon where
--    pretty (ws,u)               = pretty u
--    pretty (ws,u)               = dotCat pretty (catMaybes ws) <+> colon <+> pretty u
    pretty (ws,u)               = dotCat prettyW ws <+> colon <+> pretty u
      where prettyW Nothing     = text "_"
            prettyW (Just n)    = pretty n

instance Pretty TypeX where
    pretty _                    = empty

instance Subst TypeX where
    msubst x                    = do as <- msubst (actorstate x)
                                     return x{ actorstate = as }
    tyfree x                    = tyfree (actorstate x)

instance (Subst x) => Subst (EnvF x) where
    msubst env                  = do ne <- msubst (names env)
                                     we <- msubst (witnesses env)
                                     ex <- msubst (envX env)
                                     return env{ names = ne, witnesses = we, envX = ex }
    tyfree env                  = tvarScope env ++ tyfree (names env) ++ tyfree (witnesses env) ++ tyfree (envX env)

instance Subst NameInfo where
    msubst (NVar t)             = NVar <$> msubst t
    msubst (NSVar t)            = NSVar <$> msubst t
    msubst (NDef t d)           = NDef <$> msubst t <*> return d
    msubst (NSig t d)           = NSig <$> msubst t <*> return d
    msubst (NAct q p k te)      = NAct <$> msubst q <*> msubst p <*> msubst k <*> msubst te
    msubst (NClass q us te)     = NClass <$> msubst q <*> msubst us <*> msubst te
    msubst (NProto q us te)     = NProto <$> msubst q <*> msubst us <*> msubst te
    msubst (NExt n q ps te)     = NExt n <$> msubst q <*> msubst ps <*> msubst te
    msubst (NTVar k mba)        = NTVar k <$> msubst mba
    msubst (NAlias qn)          = NAlias <$> return qn
    msubst (NMAlias m)          = NMAlias <$> return m
    msubst (NModule te)         = NModule <$> return te     -- actually msubst te, but te has no free variables (top-level)
    msubst NReserved            = return NReserved
    msubst NBlocked             = return NBlocked

    tyfree (NVar t)             = tyfree t
    tyfree (NSVar t)            = tyfree t
    tyfree (NDef t d)           = tyfree t
    tyfree (NSig t d)           = tyfree t
    tyfree (NAct q p k te)      = (tyfree q ++ tyfree p ++ tyfree k ++ tyfree te) \\ (tvSelf : tybound q)
    tyfree (NClass q us te)     = (tyfree q ++ tyfree us ++ tyfree te) \\ (tvSelf : tybound q)
    tyfree (NProto q us te)     = (tyfree q ++ tyfree us ++ tyfree te) \\ (tvSelf : tybound q)
    tyfree (NExt n q ps te)     = (tyfree q ++ tyfree ps ++ tyfree te) \\ (tvSelf : tybound q)
    tyfree (NTVar k mba)        = tyfree mba
    tyfree (NAlias qn)          = []
    tyfree (NMAlias qn)         = []
    tyfree (NModule te)         = []        -- actually tyfree te, but a module has no free variables on the top level
    tyfree NReserved            = []
    tyfree NBlocked             = []

instance Subst (QName,Witness) where
    msubst (n, w@WClass{})      = return (n, w)         -- A WClass (i.e., an extension) can't have any free type variables
    msubst (n, w@WInst{})       = do p <- msubst (proto w)
                                     return (n, w{ proto = p })
    
    tyfree (n, w@WClass{})      = []
    tyfree (n, w@WInst{})       = filter univar $ tyfree (proto w)
    

instance Subst WTCon where
    msubst (w,u)                = (,) <$> return w <*> msubst u
    
    tyfree (w,u)                = tyfree u

instance Polarity NameInfo where
    polvars (NVar t)                = polvars t
    polvars (NSVar t)               = polvars t
    polvars (NDef t d)              = polvars t
    polvars (NSig t d)              = polvars t
    polvars (NAct q p k te)         = (polvars q `polcat` polvars p `polcat` polvars k `polcat` polvars te) `polminus` (tvSelf : tybound q)
    polvars (NClass q us te)        = (polvars q `polcat` polvars us `polcat` polvars te) `polminus` (tvSelf : tybound q)
    polvars (NProto q us te)        = (polvars q `polcat` polvars us `polcat` polvars te) `polminus` (tvSelf : tybound q)
    polvars (NExt n q ps te)        = (polvars q `polcat` polvars ps `polcat` polvars te) `polminus` (tvSelf : tybound q)
    polvars (NTVar k mba)           = polvars mba
    polvars _                       = ([],[])

instance Polarity WTCon where
    polvars (w, c)                  = polvars c

instance Polarity (Name,NameInfo) where
    polvars (n, i)                  = polvars i

negself te                          = concat $ map nself te
  where nself (_, NSig t NoDec)     = filter (==tvSelf) (snd $ polvars t)
        nself (_, NDef t NoDec)     = filter (==tvSelf) (snd $ polvars t)
        nself (_, _)                = []


-------------------------------------------------------------------------------------------------------------------

class Unalias a where
    unalias                         :: EnvF x -> a -> a
    unalias env                     = id

instance (Unalias a) => Unalias [a] where
    unalias env                     = map (unalias env)

instance (Unalias a) => Unalias (Maybe a) where
    unalias env                     = fmap (unalias env)

instance Unalias ModName where
    unalias env m | m == mBuiltin   = m
    unalias env (ModName ns0)       = norm (names env) [] ns0
      where
        norm te pre []              = ModName (reverse pre)
        norm te pre (n:ns)          = case lookup n te of
                                        Just (NModule te') -> norm te' (n:pre) ns
                                        Just (NMAlias m) -> m
                                        _ -> noModule (ModName ns0)

instance Unalias QName where
    unalias env (QName m n)         = case lookup m' (modules env) of
                                        Just te -> case lookup n te of
                                                      Just (NAlias qn) -> qn
                                                      Just _ -> QName m' n
                                                      _ -> noItem m n
                                        Nothing | inBuiltin env -> QName m n
                                                | otherwise -> trace ("#### unalias fails for " ++ prstr (QName m n)) $ QName m n
      where m'                      = unalias env m
    unalias env (NoQ n)             = case lookup n (names env) of
                                        Just (NAlias qn) -> qn
                                        _ -> NoQ n
                                    
instance Unalias TSchema where
    unalias env (TSchema l q t)     = TSchema l (unalias env q) (unalias env t)

instance Unalias TCon where
    unalias env (TC qn ts)          = TC (unalias env qn) (unalias env ts)

instance Unalias QBind where
    unalias env (Quant tv cs)       = Quant tv (unalias env cs)

instance Unalias Type where
    unalias env (TCon l c)          = TCon l (unalias env c)
    unalias env (TFun l e p r t)    = TFun l (unalias env e) (unalias env p) (unalias env r) (unalias env t)
    unalias env (TTuple l p k)      = TTuple l (unalias env p) (unalias env k)
    unalias env (TOpt l t)          = TOpt l (unalias env t)
    unalias env (TRow l k n t r)    = TRow l k n (unalias env t) (unalias env r)
    unalias env t                   = t

instance Unalias NameInfo where
    unalias env (NVar t)            = NVar (unalias env t)
    unalias env (NSVar t)           = NSVar (unalias env t)
    unalias env (NDef t d)          = NDef (unalias env t) d
    unalias env (NSig t d)          = NSig (unalias env t) d
    unalias env (NAct q p k te)     = NAct (unalias env q) (unalias env p) (unalias env k) (unalias env te)
    unalias env (NClass q us te)    = NClass (unalias env q) (unalias env us) (unalias env te)
    unalias env (NProto q us te)    = NProto (unalias env q) (unalias env us) (unalias env te)
    unalias env (NExt n q ps te)    = NExt (unalias env n) (unalias env q) (unalias env ps) (unalias env te)
    unalias env (NTVar k mba)       = NTVar k (unalias env mba)
    unalias env (NAlias qn)         = NAlias (unalias env qn)
    unalias env (NModule te)        = NModule (unalias env te)
    unalias env NReserved           = NReserved
    unalias env NBlocked            = NBlocked

instance Unalias WTCon where
    unalias env (w,u)               = (unalias env w, unalias env u)

instance Unalias (Name,NameInfo) where
    unalias env (n,i)               = (n, unalias env i)

-- Union type handling -------------------------------------------------------------------------------------------------

uniLit (ULit l)             = True
uniLit _                    = False

uniCon env (TC n [])
  | qn `elem` uniCons       = Just $ UCon qn
  | qn `elem` uniCons'      = Just $ UCon qn
  where qn                  = unalias env n
uniCon env _                = Nothing

uniCons                     = [qnInt, qnFloat, qnBool, qnStr]
uniCons'                    = map NoQ [nInt, nFloat, nBool, nStr]

uniMax us                   = [ u | UCon u <- us ] == uniCons

uniElem us u@(ULit l)       = u `elem` us || UCon qnStr `elem` us
uniElem us u                = u `elem` us

uniConElem env us c
  | Just u <- uniCon env c  = uniElem us u
  | otherwise               = False

uniNorm env l us
  | not $ null dups         = err l ("Duplicate union element: " ++ prstr (head dups))
  | otherwise               = us1
  where us1                 = sort $ norm us
        dups                = duplicates us1
        norm []             = []
        norm (ULit l : us)  = ULit l : norm us
        norm (UCon n : us)  = case uniCon env (TC n []) of
                                Just u -> u : norm us
                                _ -> err1 n "Illegal union element:"

uniAbove us                             = [ ns | ns <- nss, length ns > 1 ]
  where nss                             = [ catMaybes [i,f,b,s] | i <- mb qnInt, f <- mb qnFloat, b <- mb qnBool, s <- mbStr ]
        mb qn | UCon qn `elem` us       = [Just qn]
              | otherwise               = [Nothing, Just qn]
        mbStr | not (null lits)         = [Just qnStr]
              | otherwise               = mb qnStr
          where lits                    = [ s | ULit s <- us ]

uniBelow us                             = [ ns | ns <- nss, length ns > 1 ]
  where nss                             = [ catMaybes [i,f,b,s] | i <- mb qnInt, f <- mb qnFloat, b <- mb qnBool, s <- mb qnStr ]
        mb qn | UCon qn `elem` us       = [Nothing, Just qn]
              | otherwise               = [Nothing]

-- TEnv filters --------------------------------------------------------------------------------------------------------

nSigs                       :: TEnv -> TEnv
nSigs te                    = [ (n,i) | (n, i@(NSig sc dec)) <- te ]

splitSigs                   :: TEnv -> (TEnv, TEnv)
splitSigs te                = partition isSig te
  where isSig (_, NSig{})   = True
        isSig _             = False

nTerms                      :: TEnv -> TEnv
nTerms te                   = [ (n,i) | (n,i) <- te, isTerm i ]
  where isTerm NDef{}       = True
        isTerm NVar{}       = True
        isTerm _            = False

noDefs                      :: TEnv -> TEnv
noDefs te                   = [ (n,i) | (n,i) <- te, not $ isDef i ]
  where isDef NDef{}        = True
        isDef NAct{}        = True
        isDef _             = False

sigTerms                    :: TEnv -> (TEnv, TEnv)
sigTerms te                 = (nSigs te, nTerms te)

propSigs                    :: TEnv -> TEnv
propSigs te                 = [ (n,i) | (n, i@(NSig sc dec)) <- te, isProp dec sc ]

isProp                      :: Deco -> TSchema -> Bool
isProp Property _           = True
isProp NoDec sc             = case sctype sc of TFun{} -> False; _ -> True
isProp _ _                  = False

nSchemas                    :: TEnv -> Schemas
nSchemas []                 = []
nSchemas ((n,NVar t):te)    = (n, monotype t) : nSchemas te
nSchemas ((n,NDef sc d):te) = (n, sc) : nSchemas te
nSchemas (_:te)             = nSchemas te

parentTEnv                  :: Env -> [WTCon] -> TEnv
parentTEnv env us           = concatMap (snd . findCon env . snd) us

splitTEnv                   :: [Name] -> TEnv -> (TEnv, TEnv)
splitTEnv vs te             = partition ((`elem` vs) . fst) te

prioSig                     :: TEnv -> TEnv
prioSig te                  = f [] te
  where
    f ns []                 = []
    f ns ((n,i@NSig{}):te)  = (n,i) : f (n:ns) te
    f ns ((n,i):te)
      | n `elem` ns         = f ns te
      | otherwise           = (n,i) : f ns te

unSig                       :: TEnv -> TEnv
unSig te                    = map f te
  where f (n, NSig (TSchema _ [] t) Property)   = (n, NVar t)
        f (n, NSig sc@(TSchema _ _ TFun{}) dec) = (n, NDef sc dec)
        f (n, NSig (TSchema _ _ t) _)           = (n, NVar t)
        f (n, i)                                = (n, i)


-- Env construction and modification -------------------------------------------------------------------------------------------


initEnv                    :: Bool -> IO Env0
initEnv nobuiltin           = if nobuiltin
                                then return $ EnvF{ names = [],
                                                    modules = [],
                                                    witnesses = [],
                                                    envX = () }
                                else do path <- getExecutablePath
                                        envBuiltin <- InterfaceFiles.readFile (joinPath [takeDirectory path,"__builtin__.ty"])
                                        let env0 = EnvF{ names = [(nBuiltin,NModule envBuiltin)],
                                                         modules = [(mBuiltin,envBuiltin)],
                                                         witnesses = [],
                                                         envX = () }
                                            env = importAll mBuiltin envBuiltin $ importWits mBuiltin envBuiltin $ env0
                                        return env

withModulesFrom             :: EnvF x -> EnvF x -> EnvF x
env `withModulesFrom` env'  = env{modules = modules env'}

setActorFX                  :: Type -> Env -> Env
setActorFX st env           = modX env $ \x -> x{ actorstate = Just st }


maybeSetActorFX             :: Type -> Env -> Env
maybeSetActorFX st env      = maybe (setActorFX st env) (const env) (actorstate $ envX env)       -- Only set if not already present

setInAct                    :: Env -> Env
setInAct env                = modX env $ \x -> x{ context = CtxAct }

setInClass                  :: Env -> Env
setInClass env              = modX env $ \x -> x{ context = CtxClass }

setInDecl                   :: Env -> Env
setInDecl env               = modX env $ \x -> x{ indecl = True }

addWit                      :: EnvF x -> (QName,Witness) -> EnvF x
addWit env cwit
  | exists                  = env
  | otherwise               = env{ witnesses = cwit : witnesses env }
  where exists              = any (qmatch env cwit) (witnesses env)

addMod                      :: ModName -> TEnv -> EnvF x -> EnvF x
addMod m te env             = env{ modules = (m,te) : modules env }

reserve                     :: [Name] -> EnvF x -> EnvF x
reserve xs env              = env{ names = [ (x, NReserved) | x <- nub xs ] ++ names env }

block                       :: [Name] -> EnvF x -> EnvF x
block xs env                = env{ names = [ (x, NBlocked) | x <- nub xs ] ++ names env }

define                      :: TEnv -> EnvF x -> EnvF x
define te env               = foldl addWit env1 ws
  where env1                = env{ names = reverse te ++ exclude (dom te) (names env) }
        ws                  = [ (c, WClass q p (NoQ w) ws) | (w, NExt c q ps te') <- te, (ws,p) <- ps ]

defineTVars                 :: QBinds -> EnvF x -> EnvF x
defineTVars q env           = foldr f env q
  where f (Quant tv us) env = foldl addWit env{ names = (tvname tv, NTVar (tvkind tv) mbc) : names env } wits
          where (mbc,ps)    = case mro2 env us of ([],_) -> (Nothing, us); _ -> (Just $ head us, tail us)   -- Just check that the mro exists, don't store it
                wits        = [ (NoQ (tvname tv), WInst p (NoQ $ tvarWit tv p0) wchain) | p0 <- ps, (wchain,p) <- findAncestry env p0 ]

defineSelfOpaque            :: EnvF x -> EnvF x
defineSelfOpaque env        = defineTVars [Quant tvSelf []] env

defineSelf                  :: QName -> QBinds -> EnvF x -> EnvF x
defineSelf qn q env         = defineTVars [Quant tvSelf [tc]] env
  where tc                  = TC qn [ tVar tv | Quant tv _ <- q ]

defineInst                  :: QName -> [WTCon] -> Name -> EnvF x -> EnvF x
defineInst n ps w env       = foldl addWit env wits
  where wits                = [ (n, WInst p (NoQ w) ws) | (ws,p) <- ps ]

defineMod                   :: ModName -> TEnv -> EnvF x -> EnvF x
defineMod m te env          = define [(n, defmod ns $ te1)] env
  where ModName (n:ns)      = m
        te1                 = case lookup n (names env) of Just (NModule te1) -> te1; _ -> []
        defmod [] te1       = NModule $ te
        defmod (n:ns) te1   = NModule $ (n, defmod ns te2) : exclude [n] te1
          where te2         = case lookup n te1 of Just (NModule te2) -> te2; _ -> []


-- General Env queries -----------------------------------------------------------------------------------------------------------

inBuiltin                   :: EnvF x -> Bool
inBuiltin env               = null $ modules env

actorFX                     :: Env -> SrcLoc -> Type
actorFX env l               = case actorstate (envX env) of
                                Just st -> fxAct st
                                Nothing -> err l "Actor scope expected"

onTop                       :: Env -> Bool
onTop env                   = context (envX env) == CtxTop

inAct                       :: Env -> Bool
inAct env                   = context (envX env) == CtxAct

inClass                     :: Env -> Bool
inClass env                 = context (envX env) == CtxClass

inDecl                      :: Env -> Bool
inDecl env                  = indecl $ envX env

stateScope                  :: EnvF x -> [Name]
stateScope env              = [ z | (z, NSVar _) <- names env ]

tvarScope                   :: EnvF x -> [TVar]
tvarScope env               = [ TV k n | (n, NTVar k _) <- names env ]

-- Name queries -------------------------------------------------------------------------------------------------------------------

findQName                   :: QName -> EnvF x -> NameInfo 
findQName (QName m n) env   = case maybeFindMod (unalias env m) env of
                                Just te -> case lookup n te of
                                    Just (NAlias qn) -> findQName qn env
                                    Just i -> i
                                    _ -> noItem m n
                                Nothing | inBuiltin env -> findQName (NoQ n) env
                                        | otherwise -> noModule m
findQName (NoQ n) env       = case lookup n (names env) of
                                Just (NAlias qn) -> findQName qn env
                                Just info -> info
                                Nothing -> nameNotFound n

findName n env              = findQName (NoQ n) env

maybeFindMod                :: ModName -> EnvF x -> Maybe TEnv
maybeFindMod (ModName ns) env = f ns (names env)
  where f [] te             = Just te
        f (n:ns) te         = case lookup n te of
                                Just (NModule te') -> f ns te'
                                Just (NMAlias m) -> maybeFindMod m env
                                _ -> Nothing

isMod                       :: EnvF x -> [Name] -> Bool
isMod env ns                = maybe False (const True) (maybeFindMod (ModName ns) env)


tconKind                    :: QName -> EnvF x -> Kind
tconKind n env              = case findQName n env of
                                NAct q _ _ _ -> kind KType q
                                NClass q _ _ -> kind KType q
                                NProto q _ _ -> kind KProto q
                                _            -> notClassOrProto n
  where kind k []           = k
        kind k q            = KFun [ tvkind v | Quant v _ <- q ] k

isActor                     :: EnvF x -> QName -> Bool
isActor env n               = case findQName n env of
                                NAct q p k te -> True
                                _ -> False

isClass                     :: EnvF x -> QName -> Bool
isClass env n               = case findQName n env of
                                NClass q us te -> True
                                _ -> False

isProto                     :: EnvF x -> QName -> Bool
isProto env n               = case findQName n env of
                                NProto q us te -> True
                                _ -> False

findWitness                 :: EnvF x -> QName -> (Witness->Bool) -> Maybe Witness
findWitness env cn f        = listToMaybe $ filter f $ allWitnesses env cn

allWitnesses                :: EnvF x -> QName -> [Witness]
allWitnesses env cn         = [ w | (c,w) <- witnesses env, qmatch env c cn ]

implProto                   :: EnvF x -> TCon -> Witness -> Bool
implProto env p w           = case w of
                                WClass{} -> qmatch env (tcname p) (tcname p')
                                WInst{}  -> qmatch env p p'
  where p'                  = proto w

hasAttr                     :: EnvF x -> Name -> Witness -> Bool
hasAttr env n w             = n `elem` conAttrs env (tcname $ proto w)

hasWitness                  :: EnvF x -> QName -> QName -> Bool
hasWitness env cn pn        =  not $ null $ findWitness env cn (qmatch env pn . tcname . proto)


-- TCon queries ------------------------------------------------------------------------------------------------------------------

findAttr                    :: EnvF x -> TCon -> Name -> Maybe (Expr->Expr,TSchema,Deco)
findAttr env tc n           = findIn [ (w,te') | (w,u) <- findAncestry env tc, let (_,te') = findCon env u ]
  where findIn ((w,te):tes) = case lookup n te of
                                Just (NSig sc d) -> Just (wexpr w, sc, d)
                                Just (NDef sc d) -> Just (wexpr w, sc, d)
                                Just (NVar t)    -> Just (wexpr w, monotype t, NoDec)
                                Nothing          -> findIn tes
        findIn []           = Nothing

findAttr'                   :: EnvF x -> TCon -> Name -> TSchema
findAttr' env tc n          = sc
  where Just (_, sc, _)     = findAttr env tc n

findAncestry                :: EnvF x -> TCon -> [WTCon]
findAncestry env tc         = ([Nothing],tc) : fst (findCon env tc)

findAncestor                :: EnvF x -> TCon -> QName -> Maybe (Expr->Expr,TCon)
findAncestor env p qn       = listToMaybe [ (wexpr ws, p') | (ws,p') <- findAncestry env p, qmatch env (tcname p') qn ]

hasAncestor'                :: EnvF x -> QName -> QName -> Bool
hasAncestor' env qn qn'     = any (qmatch env qn') [ tcname c' | (w,c') <- us ]
  where (_,us,_)            = findConName qn env

hasAncestor                 :: EnvF x -> TCon -> TCon -> Bool
hasAncestor env c c'        = hasAncestor' env (tcname c) (tcname c')

commonAncestors             :: EnvF x -> TCon -> TCon -> [TCon]
commonAncestors env c1 c2   = filter (\c -> any (qmatch env (tcname c)) ns) $ map snd (findAncestry env c1)
  where ns                  = map (tcname . snd) (findAncestry env c2)

directAncestors             :: EnvF x -> QName -> [QName]
directAncestors env qn      = [ tcname p | (ws,p) <- us, null $ catMaybes ws ]
  where (q,us,te)           = findConName qn env

allAncestors                :: EnvF x -> QName -> [QName]
allAncestors env qn         = map (tcname . snd) us
  where (q,us,te)           = findConName qn env

allDescendants              :: EnvF x -> QName -> [QName]
allDescendants env qn       = [ n | n <- allCons env, hasAncestor' env n qn ]

findCon                     :: EnvF x -> TCon -> ([WTCon],TEnv)
findCon env (TC n ts)
  | map tVar tvs == ts      = (us, te)
  | otherwise               = (subst s us, subst s te)
  where (q,us,te)           = findConName n env
        tvs                 = tybound q
        s                   = tvs `zip` ts
      
findConName n env           = case findQName n env of
                                NAct q p k te  -> (q,[],te)
                                NClass q us te -> (q,us,te)
                                NProto q us te -> (q,us,te)
                                NExt n q us te -> (q,us,te)
                                NReserved -> nameReserved n
                                i -> err1 n ("findConName: Class or protocol name expected, got " ++ show i ++ " --- ")

conAttrs                    :: EnvF x -> QName -> [Name]
conAttrs env qn             = dom te
  where (_,_,te)            = findConName qn env

directAttrs                 :: EnvF x -> QName -> [Name]
directAttrs env qn          = concat [ dom (nSigs te) | qn' <- qn : directAncestors env qn, let (_,_,te) = findConName qn' env ]

allAttrs                    :: EnvF x -> QName -> [Name]
allAttrs env qn             = concat [ conAttrs env qn' | qn' <- qn : allAncestors env qn ]

unfold env te               = map exp te
  where exp (n, NAlias qn)  = (n, findQName qn env)
        exp (n, i)          = (n, i)

allCons                     :: EnvF x -> [QName]
allCons env                 = [ NoQ n | (n,i) <- names env, con i ] ++ concat [ cons [n] te' | (n,NModule te') <- names env ]
  where con NClass{}        = True
        con NAct{}          = True
        con _               = False
        cons ns te          = [ QName (ModName ns) n | (n,i) <- te, con i ] ++ concat [ cons (ns++[n]) te' | (n,NModule te') <- te ]

allProtos                   :: EnvF x -> [QName]
allProtos env               = [ NoQ n | (n,i) <- names env, proto i ] ++ concat [ protos [n] te' | (n,NModule te') <- names env ]
  where proto NProto{}      = True
        proto _             = False
        protos ns te        = [ QName (ModName ns) n | (n,i) <- te, proto i ] ++ concat [ protos (ns++[n]) te' | (n,NModule te') <- te ]
        
allVars                     :: EnvF x -> Kind -> [TVar]
allVars env k               = [ TV k n | (n,NTVar k' _) <- names env, k == k' ]

-- TVar queries ------------------------------------------------------------------------------------------------------------------

findTVBound                 :: EnvF x -> TVar -> Maybe TCon
findTVBound env tv          = case findName (tvname tv) env of
                                NTVar _ mba -> mba
                                _ -> err1 tv "Unknown type variable"

findTVAttr                  :: EnvF x -> TVar -> Name -> Maybe (Expr->Expr,TSchema,Deco)
findTVAttr env tv n         = case findTVBound env tv of
                                Just a -> findAttr env a n
                                Nothing -> Nothing

tvarWit                     :: TVar -> TCon -> Name
tvarWit tv p                = Derived (name "w") $ Derived (deriveQ $ tcname p) (tvname tv)


-- Well-formed tycon applications -------------------------------------------------------------------------------------------------

class WellFormed a where
    wf                      :: EnvF x -> a -> Constraints

instance (WellFormed a) => WellFormed (Maybe a) where
    wf env                  = maybe [] (wf env)

instance (WellFormed a) => WellFormed [a] where
    wf env                  = concatMap (wf env)

instance (WellFormed a, WellFormed b) => WellFormed (a,b) where
    wf env (a,b)            = wf env a ++ wf env b

instance WellFormed TCon where
    wf env (TC n ts)        = wf env ts ++ subst s [ constr u (tVar v) | Quant v us <- q, u <- us ]
      where q               = case findQName n env of
                                NAct q p k te  -> q
                                NClass q us te -> q
                                NProto q us te -> q
                                NReserved -> nameReserved n
                                i -> err1 n ("wf: Class or protocol name expected, got " ++ show i)
            s               = tybound q `zip` ts
            constr u t      = if isProto env (tcname u) then Impl (name "_") t u else Cast t (tCon u)

wfProto                     :: Env -> TCon -> TypeM (Constraints, Constraints)
wfProto env (TC n ts)       = do cs <- instQuals env q ts
                                 return (wf env ts, cs)
  where q                   = case findQName n env of
                                NProto q us te -> q
                                NReserved -> nameReserved n
                                i -> err1 n ("wfProto: Protocol name expected, got " ++ show i)
            
instance WellFormed Type where
    wf env (TCon _ tc)      = wf env tc
    wf env (TFun _ x p k t) = wf env x ++ wf env p ++ wf env p ++ wf env k ++ wf env t
    wf env (TTuple _ p k)   = wf env p ++ wf env k
    wf env (TOpt _ t)       = wf env t
    wf env (TRow _ _ _ t r) = wf env t ++ wf env r
    wf env _                = []


instance WellFormed QBind where
    wf env (Quant v us)     = wf env us


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
    us'                                 = case us of [] -> []; u:us -> ([Nothing],u) : [ ([Just (tcname u)],u) | u <- us ]
    
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
      | headmatch                       = tcargs u1 == tcargs u2 || err2 [u1,u2] "Inconsistent protocol instantiations"
      | otherwise                       = False
      where headmatch                   = qmatch env (tcname u1) (tcname u2)

    absent                              :: WTCon -> [WTCon] -> Bool
    absent (w,h) us                     = tcname h `notElem` map (tcname . snd) us



-- Instantiation -------------------------------------------------------------------------------------------------------------------

instantiate                 :: EnvF x -> TSchema -> TypeM (Constraints, [Type], Type)
instantiate env (TSchema _ q t)
                            = do (cs, tvs) <- instQBinds env q
                                 let s = tybound q `zip` tvs
                                 return (cs, tvs, subst s t)

instQBinds                  :: EnvF x -> QBinds -> TypeM (Constraints, [Type])
instQBinds env q            = do ts <- newTVars [ tvkind v | Quant v _ <- q ]
                                 cs <- instQuals env q ts
                                 return (cs, ts)

instWitness                 :: EnvF x -> [Type] -> Witness -> TypeM (Constraints,TCon,Expr)        -- witnesses of cs already applied in e!
instWitness env ts wit      = case wit of
                                 WClass q p w ws -> do
                                    cs <- instQuals env q ts
                                    return (cs, subst (tybound q `zip` ts) p, wexpr ws (eCall (tApp (eQVar w) ts) $ wvars cs))
                                 WInst p w ws ->
                                    return ([], p, wexpr ws (eQVar w))

instQuals                   :: EnvF x -> QBinds -> [Type] -> TypeM Constraints
instQuals env q ts          = do let s = tybound q `zip` ts
                                 sequence [ constr (subst s (tVar v)) (subst s u) | Quant v us <- q, u <- us ]
  where constr t u@(TC n _)
          | isProto env n   = do w <- newWitness; return $ Impl w t u
          | otherwise       = return $ Cast t (tCon u)

wexpr                       :: [Maybe QName] -> Expr -> Expr
wexpr []                    = id
wexpr (Nothing : w)         = wexpr w
wexpr (Just n : w)          = wexpr w . (\e -> eDot e (noq n))

wvars                       :: Constraints -> [Expr]
wvars cs                    = [ eVar v | Impl v _ _ <- cs ]


----------------------------------------------------------------------------------------------------------------------
-- castable predicate
----------------------------------------------------------------------------------------------------------------------

castable                                    :: EnvF x -> Type -> Type -> Bool
castable env (TWild _) t2                   = True
castable env t1 (TWild _)                   = True

castable env (TCon _ c1) (TCon _ c2)
  | Just (wf,c') <- search                  = tcargs c1 == tcargs c'
  where search                              = findAncestor env c1 (tcname c2)

castable env (TFun _ fx1 p1 k1 t1) (TFun _ fx2 p2 k2 t2)
  | fx1 == fxAction , fx2 /= fxAction       = castable env fx1 fx2 && castable env p2 p1 && castable env k2 k1 && castable env (tMsg t1) t2
  | otherwise                               = castable env fx1 fx2 && castable env p2 p1 && castable env k2 k1 && castable env t1 t2

castable env (TTuple _ p1 k1) (TTuple _ p2 k2)
                                            = castable env p1 p2 && castable env k1 k2

castable env (TUnion _ us1) (TUnion _ us2)
  | all (uniElem us2) us1                   = True
castable env (TUnion _ us1) t2
  | all uniLit us1                          = t2 == tStr
castable env (TCon _ c1) (TUnion _ us2)
  | uniConElem env us2 c1                   = True

castable env (TOpt _ t1) (TOpt _ t2)        = castable env t1 t2
castable env (TNone _) (TOpt _ t)           = True
castable env (TNone _) (TNone _)            = True

castable env (TFX _ fx1) (TFX _ fx2)        = castable' fx1 fx2
  where castable' FXPure FXPure             = True
        castable' FXPure (FXMut _)          = True
        castable' FXPure (FXAct _)          = True
        castable' (FXMut t1) (FXMut t2)     = t1 == t2
        castable' (FXMut t1) (FXAct t2)     = t1 == t2
        castable' (FXAct t1) (FXAct t2)     = t1 == t2
        castable' FXAction FXAction         = True
        castable' FXAction (FXAct _)        = True
        castable' fx1 fx2                   = False

castable env (TNil _ k1) (TNil _ k2)
  | k1 == k2                                = True
castable env (TRow _ k n t1 r1) r2
  | Just (t2,r2') <- findInRow n r2         = t2 /= tWild && castable env t1 t2 && r2' /= tWild && castable env r1 r2'

castable env (TVar _ tv1) (TVar _ tv2)
  | tv1 == tv2                              = True

castable env t1@(TVar _ tv) t2
  | Just tc <- findTVBound env tv           = castable env (tCon tc) t2
  | otherwise                               = False

castable env t1 t2@(TVar _ tv)              = False

castable env t1 (TOpt _ t2)                 = castable env t1 t2

castable env t1 t2                          = False


findInRow n (TRow l k n' t r)
  | n == n'                                 = Just (t,r)
  | otherwise                               = case findInRow n r of
                                                Nothing -> Nothing
                                                Just (t',r') -> Just (t, TRow l k n' t r')
findInRow n (TVar _ _)                      = Just (tWild,tWild)
findInRow n (TNil _ _)                      = Nothing

maxtype env (t:ts)                          = maxt t ts
  where maxt top (t:ts)
          | castable env t top              = maxt top ts
          | otherwise                       = maxt t ts
        maxt top []                         = top


-- Import handling (local definitions only) ----------------------------------------------

getImps                         :: (FilePath,FilePath) -> EnvF x -> [Import] -> IO (EnvF x)
getImps ps env []               = return env
getImps ps env (i:is)           = do env' <- impModule ps env i
                                     getImps ps env' is


impModule                       :: (FilePath,FilePath) -> EnvF x -> Import -> IO (EnvF x)
impModule ps env (Import _ ms)  = imp env ms
  where imp env []              = return env
        imp env (ModuleItem m as : is)
                                = do (env1,te) <- doImp ps env m
                                     let env2 = maybe (defineMod m te env1) (\n->define [(n, NMAlias m)] env1) as
                                     imp (importWits m te env2) is
impModule ps env (FromImport _ (ModRef (0,Just m)) items)
                                = do (env1,te) <- doImp ps env m
                                     return $ importSome items m te $ importWits m te $ env1
impModule ps env (FromImportAll _ (ModRef (0,Just m)))
                                = do (env1,te) <- doImp ps env m
                                     return $ importAll m te $ importWits m te $ env1
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


importSome                  :: [ImportItem] -> ModName -> TEnv -> EnvF x -> EnvF x
importSome items m te env   = define (map pick items) env
  where 
    te1                     = impNames m te
    pick (ImportItem n mbn) = case lookup n te1 of
                                    Just i  -> (maybe n id mbn, i) 
                                    Nothing -> noItem m n

importAll                   :: ModName -> TEnv -> EnvF x -> EnvF x
importAll m te env          = define (impNames m te) env

impNames                    :: ModName -> TEnv -> TEnv
impNames m te               = mapMaybe imp te
  where 
    imp (n, NAct _ _ _ _)   = Just (n, NAlias (QName m n))
    imp (n, NClass _ _ _)   = Just (n, NAlias (QName m n))
    imp (n, NProto _ _ _)   = Just (n, NAlias (QName m n))
    imp (n, NExt _ _ _ _)   = Nothing
    imp (n, NAlias _)       = Just (n, NAlias (QName m n))
    imp (n, NVar t)         = Just (n, NAlias (QName m n))
    imp (n, NDef t d)       = Just (n, NAlias (QName m n))
    imp _                   = Nothing                               -- cannot happen

importWits                  :: ModName -> TEnv -> EnvF x -> EnvF x
importWits m te env         = foldl addWit env ws
  where ws                  = [ (c, WClass q p (QName m n) ws) | (n, NExt c q ps te') <- te, (ws,p) <- ps ]


-- Name generation ------------------------------------------------------------------------------------------------------------------

univar (TV _ (Internal Typevar _ _))    = True          -- Regular type variable generated by the type-checker
univar (TV _ (Internal Wildvar _ _))    = True          -- Type variable replacing a type wildcard, generated by the kind-checker
univar _                                = False


pNames                                  = map (Internal TypesPass "p") [0..]
kNames                                  = map (Internal TypesPass "k") [0..]
xNames                                  = map (Internal TypesPass "x") [0..]

newWitness                              = Internal Witness "" <$> newUnique

newTVarOfKind k                         = TVar NoLoc <$> TV k <$> Internal Typevar (str k) <$> newUnique
  where str KType                       = ""
        str KFX                         = "x"
        str PRow                        = "p"
        str KRow                        = "k"
        str _                           = ""

newTVars ks                             = mapM newTVarOfKind ks

newTVar                                 = newTVarOfKind KType

newActVar                               = TVar NoLoc <$> TV KType <$> Internal Actvar "" <$> newUnique

monotypeOf (TSchema _ [] t)             = t
monotypeOf sc                           = err1 sc "Monomorphic type expected"



headvar (Impl w (TVar _ v) p)       = v
headvar (Cast (TVar _ v) t)
  | univar v                        = v
headvar (Cast t (TVar _ v))         = v     -- ?
headvar (Sub w (TVar _ v) t)
  | univar v                        = v
headvar (Sub w t (TVar _ v))        = v     -- ?
headvar (Sel w (TVar _ v) n t)      = v
headvar (Mut (TVar _ v) n t)        = v
headvar (Seal w (TVar _ v) _ _ _)   = v
headvar (Seal w _ (TVar _ v) _ _)   = v     -- ?


-- Error handling ------------------------------------------------------------------------

data CheckerError                   = FileNotFound ModName
                                    | NameNotFound Name
                                    | NameReserved QName
                                    | NameBlocked QName
                                    | NameUnexpected QName
                                    | TypedReassign Pattern
                                    | IllegalRedef Name
                                    | IllegalExtension QName
                                    | MissingSelf Name
                                    | IllegalImport SrcLoc
                                    | DuplicateImport Name
                                    | NoItem ModName Name
                                    | NoModule ModName
                                    | NoClassOrProto QName
                                    | OtherError SrcLoc String
                                    deriving (Show)

data TypeError                      = TypeErrHmm            -- ...
                                    | RigidVariable TVar
                                    | InfiniteType TVar
                                    | ConflictingRow TVar
                                    | KwdNotFound Name
                                    | PosElemNotFound
                                    | DecorationMismatch Name TSchema Deco
                                    | EscapingVar [TVar] TSchema
                                    | NoSelStatic Name TCon
                                    | NoSelInstByClass Name TCon
                                    | NoMut Name
                                    | LackSig Name
                                    | LackDef Name
                                    | NoRed Constraint
                                    | NoSolve [Constraint]
                                    | NoUnify Type Type
                                    deriving (Show)

instance Control.Exception.Exception TypeError
instance Control.Exception.Exception CheckerError


instance HasLoc TypeError where
    loc (RigidVariable tv)          = loc tv
    loc (InfiniteType tv)           = loc tv
    loc (ConflictingRow tv)         = loc tv
    loc (KwdNotFound n)             = loc n
    loc (PosElemNotFound)           = NoLoc     -- TODO: supply position
    loc (DecorationMismatch n t d)  = loc n
    loc (EscapingVar tvs t)         = loc tvs
    loc (NoSelStatic n u)           = loc n
    loc (NoSelInstByClass n u)      = loc n
    loc (NoMut n)                   = loc n
    loc (LackSig n)                 = loc n
    loc (LackDef n)                 = loc n
    loc (NoRed c)                   = loc c
    loc (NoSolve cs)                = loc cs
    loc (NoUnify t1 t2)             = loc t1

typeError err                       = (loc err,render (expl err))
  where
    expl (RigidVariable tv)         = text "Type" <+> pretty tv <+> text "is rigid"
    expl (InfiniteType tv)          = text "Type" <+> pretty tv <+> text "is infinite"
    expl (ConflictingRow tv)        = text "Row" <+> pretty tv <+> text "has conflicting extensions"
    expl (KwdNotFound n)            = text "Keyword element" <+> quotes (pretty n) <+> text "is not found"
    expl (PosElemNotFound)          = text "Positional element is not found"
    expl (DecorationMismatch n t d) = text "Decoration for" <+> pretty n <+> text "does not match signature" <+> pretty (n,NSig t d)
    expl (EscapingVar tvs t)        = text "Type annotation" <+> pretty t <+> text "is too general, type variable" <+>
                                      pretty (head tvs) <+> text "escapes"
    expl (NoSelStatic n u)          = text "Static method" <+> pretty n <+> text "cannot be selected from" <+> pretty u <+> text "instance"
    expl (NoSelInstByClass n u)     = text "Instance attribute" <+> pretty n <+> text "cannot be selected from class" <+> pretty u
    expl (NoMut n)                  = text "Non @property attribute" <+> pretty n <+> text "cannot be mutated"
    expl (LackSig n)                = text "Declaration lacks accompanying signature"
    expl (LackDef n)                = text "Signature lacks accompanying definition"
    expl (NoRed c)                  = text "Cannot infer" <+> pretty c
    expl (NoSolve cs)               = text "Cannot solve" <+> commaSep pretty cs
    expl (NoUnify t1 t2)            = text "Cannot unify" <+> pretty t1 <+> text "and" <+> pretty t2


checkerError (FileNotFound n)       = (loc n, "Type interface file not found for " ++ prstr n)
checkerError (NameNotFound n)       = (loc n, "Name " ++ prstr n ++ " is not in scope")
checkerError (NameReserved n)       = (loc n, "Name " ++ prstr n ++ " is reserved but not yet defined")
checkerError (NameBlocked n)        = (loc n, "Name " ++ prstr n ++ " is currently not accessible")
checkerError (NameUnexpected n)     = (loc n, "Unexpected variable name: " ++ prstr n)
checkerError (TypedReassign p)      = (loc p, "Type annotation on reassignment: " ++ prstr p)
checkerError (IllegalRedef n)       = (loc n, "Illegal redefinition of " ++ prstr n)
checkerError (IllegalExtension n)   = (loc n, "Illegal extension of " ++ prstr n)
checkerError (MissingSelf n)        = (loc n, "Missing 'self' parameter in definition of")
checkerError (IllegalImport l)      = (l,     "Relative import not yet supported")
checkerError (DuplicateImport n)    = (loc n, "Duplicate import of name " ++ prstr n)
checkerError (NoModule m)           = (loc m, "Module " ++ prstr m ++ " does not exist")
checkerError (NoItem m n)           = (loc n, "Module " ++ prstr m ++ " does not export " ++ nstr n)
checkerError (NoClassOrProto n)     = (loc n, "Class or protocol name expected, got " ++ prstr n)
checkerError (OtherError l str)     = (l,str)

nameNotFound n                      = Control.Exception.throw $ NameNotFound n
nameReserved n                      = Control.Exception.throw $ NameReserved n
nameBlocked n                       = Control.Exception.throw $ NameBlocked n
nameUnexpected n                    = Control.Exception.throw $ NameUnexpected n
typedReassign p                     = Control.Exception.throw $ TypedReassign p
illegalRedef n                      = Control.Exception.throw $ IllegalRedef n
illegalExtension n                  = Control.Exception.throw $ IllegalExtension n
missingSelf n                       = Control.Exception.throw $ MissingSelf n
fileNotFound n                      = Control.Exception.throw $ FileNotFound n
illegalImport l                     = Control.Exception.throw $ IllegalImport l
duplicateImport n                   = Control.Exception.throw $ DuplicateImport n
noItem m n                          = Control.Exception.throw $ NoItem m n
noModule m                          = Control.Exception.throw $ NoModule m
notClassOrProto n                   = Control.Exception.throw $ NoClassOrProto n
err l s                             = Control.Exception.throw $ OtherError l s

err0 xs s                           = err (loc $ head xs) s
err1 x s                            = err (loc x) (s ++ " " ++ prstr x)
err2 xs s                           = err (loc $ head xs) (s ++ " " ++ prstrs xs)
err3 l xs s                         = err l (s ++ " " ++ prstrs xs)

notYetExpr e                        = notYet (loc e) e

rigidVariable tv                    = Control.Exception.throw $ RigidVariable tv
infiniteType tv                     = Control.Exception.throw $ InfiniteType tv
conflictingRow tv                   = Control.Exception.throw $ ConflictingRow tv
kwdNotFound n | n == name "_"       = Control.Exception.throw $ PosElemNotFound
              | otherwise           = Control.Exception.throw $ KwdNotFound n
decorationMismatch n t d            = Control.Exception.throw $ DecorationMismatch n t d
escapingVar tvs t                   = Control.Exception.throw $ EscapingVar tvs t
noSelStatic n u                     = Control.Exception.throw $ NoSelStatic n u
noSelInstByClass n u                = Control.Exception.throw $ NoSelInstByClass n u
noMut n                             = Control.Exception.throw $ NoMut n
lackSig ns                          = Control.Exception.throw $ LackSig (head ns)
lackDef ns                          = Control.Exception.throw $ LackDef (head ns)
noRed c                             = Control.Exception.throw $ NoRed c
noSolve cs                          = Control.Exception.throw $ NoSolve cs
noUnify t1 t2                       = Control.Exception.throw $ NoUnify t1 t2

