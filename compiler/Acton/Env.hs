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
import Acton.Prim
import Acton.Printer
import Acton.Names
import Acton.Subst
import Utils
import Pretty
import InterfaceFiles
import Prelude hiding ((<>))




mkEnv                       :: FilePath -> Env0 -> Module -> IO Env0
mkEnv prefix env m          = getImps prefix env (imps m)


type TEnv                   = [(Name, NameInfo)]

data EnvF x                 = EnvF {
                                names      :: TEnv,
                                modules    :: TEnv,
                                witnesses  :: [(QName,Witness)],
                                thismod    :: Maybe ModName,
                                stub       :: Bool,
                                envX       :: x }

type Env0                   = EnvF ()


setX                        :: EnvF y -> x -> EnvF x
setX env x                  = EnvF { names = names env, modules = modules env, witnesses = witnesses env, thismod = thismod env, stub = stub env, envX = x }

modX                        :: EnvF x -> (x -> x) -> EnvF x
modX env f                  = env{ envX = f (envX env) }


mapModules1                 :: ((Name,NameInfo) -> (Name,NameInfo)) -> Env0 -> Env0
mapModules1 f env           = mapModules (\_ ni -> [f ni]) env

mapModules                  :: (Env0 -> (Name,NameInfo) -> TEnv) -> Env0 -> Env0
mapModules f env            = walk env0 [] mods
  where env0                = env{ modules = [prim] }
        prim : mods         = modules env

        walk env ns []      = env
        walk env ns ((n,NModule te1):te)
                            = walk env2 ns te
          where env1        = env{ modules = app ns (modules env) [(n, NModule [])] }
                env2        = walk env1 (ns++[n]) te1
        walk env ns (ni:te) = walk env1 ns te
          where env1        = env{ modules = app ns (modules env) (f env ni) }

        app (n:ns) ((m,NModule te1):te) te'
          | n == m          = (m, NModule $ app ns te1 te') : te
        app ns (ni:te) te'  = ni : app ns te te'
        app ns [] te'       = te'


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
                            | NTVar     Kind TCon
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


-- Equality modulo qualified/unqualified type names and unifiable type variables

class QualEq a where
    qualEq                          :: EnvF x -> a -> a -> Bool

instance QualEq [Type] where
    qualEq env as bs                = and [ qualEq env a b | (a,b) <- as `zip` bs ]

instance QualEq (QName,Witness) where
    qualEq env (x,a) (y,b)          = qualEq env x y && m a b
      where m a@WClass{} b@WClass{} = qualEq env (tcname (proto a)) (tcname (proto b))
            m a@WInst{}  b@WInst{}  = qualEq env (tcname (proto a)) (tcname (proto b))
            m a          b          = False

instance QualEq QName where
    qualEq env a b                  = unalias env a == unalias env b

instance QualEq TCon where
    qualEq env (TC a ts) (TC b us)  = qualEq env a b && qualEq env ts us

instance QualEq Type where
    qualEq env (TVar _ v1) (TVar _ v2)                      = univar v1 || univar v2 || v1 == v2
    qualEq env (TCon _ c1) (TCon _ c2)                      = qualEq env c1 c2
    qualEq env (TFun _ e1 p1 r1 t1) (TFun _ e2 p2 r2 t2)    = qualEq env e1 e2 && qualEq env p1 p2 && qualEq env r1 r2 && qualEq env t1 t2
    qualEq env (TTuple _ p1 r1) (TTuple _ p2 r2)            = qualEq env p1 p2 && qualEq env r1 r2
    qualEq env (TUnion _ u1) (TUnion _ u2)                  = qualEq env u1 u2
    qualEq env (TOpt _ t1) (TOpt _ t2)                      = qualEq env t1 t2
    qualEq env (TNone _) (TNone _)                          = True
    qualEq env (TWild _) (TWild _)                          = True
    qualEq env (TNil _ s1)  (TNil _ s2)                     = s1 == s2
    qualEq env (TRow _ s1 n1 t1 r1) (TRow _ s2 n2 t2 r2)    = s1 == s2 && n1 == n2 && qualEq env t1 t2 && qualEq env r1 r2
    qualEq env (TFX _ fx1) (TFX _ fx2)                      = fx1 == fx2
    qualEq env _ _                                          = False

instance QualEq [UType] where
    qualEq env as bs                = and [ any (qualEq env a) bs | a <- as ] && and [ any (qualEq env b) as | b <- bs ]

instance QualEq UType where
    qualEq env (UCon a) (UCon b)    = qualEq env a b
    qualEq env (ULit a) (ULit b)    = a == b
    qualEq env _ _                  = False

instance Pretty (QName,Witness) where
    pretty (n, WClass q p w ws) = text "WClass" <+> pretty n <+> nonEmpty brackets commaList q <+> parens (pretty p) <+>
                                  equals <+> pretty (wexpr ws (eCall (eQVar w) []))
    pretty (n, WInst p w ws)    = text "WInst" <+> pretty n <+> parens (pretty p) <+>
                                  equals <+> pretty (wexpr ws (eQVar w))
        
instance Pretty TEnv where
    pretty tenv                 = vcat (map pretty tenv)

instance (Pretty x) => Pretty (EnvF x) where
    pretty env                  = text "--- modules:"  $+$
                                  vcat (map pretty (modules env)) $+$
                                  text "--- names" <+> pretty (thismod env) <> colon $+$
                                  vcat (map pretty (names env)) $+$
                                  text "--- witnesses:"  $+$
                                  vcat (map pretty (witnesses env)) $+$
                                  text "--- extra:"  $+$
                                  pretty (envX env) $+$
                                  text "."

instance Pretty () where
    pretty ()                   = empty

instance Pretty (Name,NameInfo) where
    pretty (n, NVar t)          = pretty n <+> colon <+> pretty t
    pretty (n, NSVar t)         = text "var" <+> pretty n <+> colon <+> pretty t
    pretty (n, NDef t d)        = prettyDec d $ pretty n <+> colon <+> pretty t
    pretty (n, NSig t d)        = prettyDec d $ pretty n <+> text ":::" <+> pretty t
    pretty (n, NAct q p k te)   = text "actor" <+> pretty n <> nonEmpty brackets commaList q <+>
                                  parens (prettyFunRow p k) <> colon $+$ (nest 4 $ prettyOrPass te)
    pretty (n, NClass q us te)  = text "class" <+> pretty n <> nonEmpty brackets commaList q <+>
                                  nonEmpty parens commaList us <> colon $+$ (nest 4 $ prettyOrPass $ normTEnv te)
    pretty (n, NProto q us te)  = text "protocol" <+> pretty n <> nonEmpty brackets commaList q <+>
                                  nonEmpty parens commaList us <> colon $+$ (nest 4 $ prettyOrPass $ normTEnv te)
    pretty (w, NExt n [] ps te) = pretty w  <+> colon <+> text "extension" <+> pretty n <+> parens (commaList ps) <>
                                  colon $+$ (nest 4 $ prettyOrPass te)
    pretty (w, NExt n q ps te)  = pretty w  <+> colon <+> pretty q <+> text "=>" <+> text "extension" <+> pretty n <> 
                                  brackets (commaList $ tybound q) <+> parens (commaList ps) <>
                                  colon $+$ (nest 4 $ prettyOrPass te)
    pretty (n, NTVar k c)       = pretty n <> parens (pretty c)
    pretty (n, NAlias qn)       = text "alias" <+> pretty n <+> equals <+> pretty qn
    pretty (n, NMAlias m)       = text "module" <+> pretty n <+> equals <+> pretty m
    pretty (n, NModule te)      = text "module" <+> pretty n <> colon $+$ nest 4 (pretty te)
    pretty (n, NReserved)       = pretty n <+> text "(reserved)"
    pretty (n, NBlocked)        = pretty n <+> text "(blocked)"

prettyOrPass te
  | isEmpty doc                 = text "pass"
  | otherwise                   = doc
  where doc                     = pretty te

instance Pretty WTCon where
--    pretty (ws,u)               = pretty u
--    pretty (ws,u)               = dotCat pretty (catMaybes ws) <+> colon <+> pretty u
    pretty (ws,u)               = dotCat prettyW ws <+> colon <+> pretty u
      where prettyW Nothing     = text "_"
            prettyW (Just n)    = pretty n

instance (Subst x) => Subst (EnvF x) where
    msubst env                  = do ne <- msubst (names env)
                                     we <- msubst (witnesses env)
                                     ex <- msubst (envX env)
                                     return env{ names = ne, witnesses = we, envX = ex }
    tyfree env                  = tvarScope0 env ++ tyfree (names env) ++ tyfree (witnesses env) ++ tyfree (envX env)

instance Subst NameInfo where
    msubst (NVar t)             = NVar <$> msubst t
    msubst (NSVar t)            = NSVar <$> msubst t
    msubst (NDef t d)           = NDef <$> msubst t <*> return d
    msubst (NSig t d)           = NSig <$> msubst t <*> return d
    msubst (NAct q p k te)      = NAct <$> msubst q <*> msubst p <*> msubst k <*> msubst te
    msubst (NClass q us te)     = NClass <$> msubst q <*> msubst us <*> msubst te
    msubst (NProto q us te)     = NProto <$> msubst q <*> msubst us <*> msubst te
    msubst (NExt n q ps te)     = NExt n <$> msubst q <*> msubst ps <*> msubst te
    msubst (NTVar k c)          = NTVar k <$> msubst c
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
    tyfree (NTVar k c)          = tyfree c
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
    polvars (NTVar k c)             = polvars c
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
    unalias env (ModName ns0)
      | inBuiltin env               = ModName ns0
      | otherwise                   = ModName $ f ns0 (names env)
      where f [] te                 = []
            f (n:ns) te             = case lookup n te of
                                        Just (NModule te') -> n : f ns te'
                                        Just (NMAlias (ModName m)) -> f (m++ns) (modules env)
                                        _ -> noModule (ModName ns0)

instance Unalias QName where
    unalias env (QName m n)         = case findMod m env of
                                        Just te -> case lookup n te of
                                                      Just (NAlias qn) -> qn
                                                      Just _ -> GName m' n
                                                      _ -> noItem m n
                                        Nothing -> error ("#### unalias fails for " ++ prstr (QName m n))
      where m'                      = unalias env m
    unalias env (NoQ n)             = case lookup n (names env) of
                                        Just (NAlias qn) -> qn
                                        _ -> case thismod env of Just m -> GName m n; _ -> NoQ n
    unalias env (GName m n)
      | inBuiltin env, m==mBuiltin  = NoQ n
      | otherwise                   = GName m n
                                    
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
    unalias env (TUnion l us)       = TUnion l (sort $ nub $ unalias env us)
    unalias env t                   = t

instance Unalias UType where
    unalias env (ULit l)            = ULit l
    unalias env (UCon c)            = UCon (unalias env c)

instance Unalias NameInfo where
    unalias env (NVar t)            = NVar (unalias env t)
    unalias env (NSVar t)           = NSVar (unalias env t)
    unalias env (NDef t d)          = NDef (unalias env t) d
    unalias env (NSig t d)          = NSig (unalias env t) d
    unalias env (NAct q p k te)     = NAct (unalias env q) (unalias env p) (unalias env k) (unalias env te)
    unalias env (NClass q us te)    = NClass (unalias env q) (unalias env us) (unalias env te)
    unalias env (NProto q us te)    = NProto (unalias env q) (unalias env us) (unalias env te)
    unalias env (NExt n q ps te)    = NExt (unalias env n) (unalias env q) (unalias env ps) (unalias env te)
    unalias env (NTVar k c)         = NTVar k (unalias env c)
    unalias env (NAlias qn)         = NAlias (unalias env qn)
    unalias env (NMAlias m)         = NMAlias (unalias env m)
    unalias env (NModule te)        = NModule (unalias env te)
    unalias env NReserved           = NReserved
    unalias env NBlocked            = NBlocked

instance Unalias (Name,NameInfo) where
    unalias env (n,i)               = (n, unalias env i)

instance Unalias WTCon where
    unalias env (w,u)               = (unalias env w, unalias env u)


globalize env                       = unalias env


-- Union type handling -------------------------------------------------------------------------------------------------

uniLit (ULit l)             = True
uniLit _                    = False

uniCon env (TC c ts)        = null ts && unalias env c `elem` uniCons

uniCons                     = [qnInt, qnFloat, qnBool, qnStr]
                              ++ map NoQ [ nInt, nFloat, nBool, nStr]       -- TODO: remove once global unaliased names are in place

uniElem env us u            = unalias env u `elem` us1 || uniLit u && uStr `elem` us1
  where us1                 = unalias env us

uniConElem env (TC c ts) us = null ts && uniElem env us (UCon c)

uniNub env []               = []
uniNub env (u:us)
  | uniElem env us u        = uniNub env us
  | otherwise               = u : uniNub env us

uniIntersect env us1 us2
  | uniElem env us1 uStr    = [ u | u <- us2, uniElem env us1 u ]
  | otherwise               = [ u | u <- us1, uniElem env us2 u ]

uniUnion env us1 us2        = sort $ uniNub env $ us1++us2


uniAbove env us                         = [ ns | ns <- nss, length ns > 1 ]
  where nss                             = [ catMaybes [i,f,b,s] | s <- mbStr, b <- mb qnBool, f <- mb qnFloat, i <- mb qnInt ]
        mb c | uniElem env us (UCon c)  = [Just c]
             | otherwise                = [Just c, Nothing]
        mbStr | not $ null lits         = [Just qnStr]
              | otherwise               = mb qnStr
          where lits                    = filter uniLit us

uniBelow env us                         = [ ns | ns <- nss, length ns > 1 ]
  where nss                             = [ catMaybes [i,f,b,s] | s <- mb qnStr, b <- mb qnBool, f <- mb qnFloat, i <- mb qnInt ]
        mb c | uniElem env us (UCon c)  = [Nothing, Just c]
             | otherwise                = [Nothing]

-- TEnv filters --------------------------------------------------------------------------------------------------------

nSigs                       :: TEnv -> TEnv
nSigs te                    = [ (n,i) | (n, i@(NSig sc dec)) <- te ]

splitSigs                   :: TEnv -> (TEnv, TEnv)
splitSigs te                = partition isSig te
  where isSig (_, NSig{})   = True
        isSig _             = False

nTerms                      :: TEnv -> TEnv
nTerms te                   = [ (n,i) | (n,i) <- te, keep i ]
  where keep NDef{}         = True
        keep NVar{}         = True
        keep _              = False

noDefs                      :: TEnv -> TEnv
noDefs te                   = [ (n,i) | (n,i) <- te, keep i ]
  where keep NDef{}         = False
        keep NAct{}         = False
        keep _              = True

noAliases                   :: TEnv -> TEnv
noAliases te                = [ (n,i) | (n,i) <- te, keep i ]
  where keep NAlias{}       = False
        keep NMAlias{}      = False
        keep _              = True

sigTerms                    :: TEnv -> (TEnv, TEnv)
sigTerms te                 = (nSigs te, nTerms te)

propSigs                    :: TEnv -> TEnv
propSigs te                 = [ (n,i) | (n, i@(NSig sc Property)) <- te ]

isProp                      :: Deco -> TSchema -> Bool
isProp Property _           = True
isProp NoDec sc             = case sctype sc of TFun{} -> False; _ -> True
isProp _ _                  = False

parentTEnv                  :: EnvF x -> [WTCon] -> TEnv
parentTEnv env us           = concatMap (snd . findCon env . snd) us

splitTEnv                   :: [Name] -> TEnv -> (TEnv, TEnv)
splitTEnv vs te             = partition ((`elem` vs) . fst) te

normTEnv                    :: TEnv -> TEnv
normTEnv te                 = f [] te
  where
    f ns []                 = []
    f ns ((n,i):te)
      | n `elem` ns         = f ns te
      | otherwise           = (n,i) : f (n:ns) te

unSig                       :: TEnv -> TEnv
unSig te                    = map f te
  where f (n, NSig (TSchema _ [] t) Property)   = (n, NVar t)
        f (n, NSig sc@(TSchema _ _ TFun{}) dec) = (n, NDef sc dec)
        f (n, NSig (TSchema _ _ t) _)           = (n, NVar t)
        f (n, i)                                = (n, i)


-- Env construction and modification -------------------------------------------------------------------------------------------


initEnv                    :: FilePath -> Bool -> Bool -> IO Env0
initEnv path stub True     = return $ EnvF{ names = [(nPrim,NMAlias mPrim)],
                                            modules = [(nPrim,NModule envPrim)],
                                            witnesses = [],
                                            thismod = Nothing,
                                            stub = stub,
                                            envX = () }
initEnv path stub False    = do envBuiltin <- InterfaceFiles.readFile (joinPath [path,"__builtin__.ty"])
                                let env0 = EnvF{ names = [(nPrim,NMAlias mPrim), (nBuiltin,NMAlias mBuiltin)],
                                                 modules = [(nPrim,NModule envPrim), (nBuiltin,NModule envBuiltin)],
                                                 witnesses = [],
                                                 thismod = Nothing,
                                                 stub = stub,
                                                 envX = () }
                                    env = importAll mBuiltin envBuiltin $ importWits mBuiltin envBuiltin $ env0
                                return env

envPrim                     = primMkEnv NClass NDef NVar NSig

withModulesFrom             :: EnvF x -> EnvF x -> EnvF x
env `withModulesFrom` env'  = env{modules = modules env'}

addWit                      :: EnvF x -> (QName,Witness) -> EnvF x
addWit env cwit
  | exists                  = env
  | otherwise               = env{ witnesses = cwit : witnesses env }
  where exists              = any (qualEq env cwit) (witnesses env)

reserve                     :: [Name] -> EnvF x -> EnvF x
reserve xs env              = env{ names = [ (x, NReserved) | x <- nub xs ] ++ names env }

block                       :: [Name] -> EnvF x -> EnvF x
block xs env                = env{ names = [ (x, NBlocked) | x <- nub xs ] ++ names env }

define                      :: TEnv -> EnvF x -> EnvF x
define te env               = foldl addWit env1 ws
  where env1                = env{ names = reverse te ++ exclude (names env) (dom te) }
        ws                  = [ (c, WClass q p (NoQ w) ws) | (w, NExt c q ps te') <- te, (ws,p) <- ps ]

defineTVars                 :: QBinds -> EnvF x -> EnvF x
defineTVars q env           = foldr f env q
  where f (Quant tv us) env = foldl addWit env{ names = (tvname tv, NTVar (tvkind tv) c) : names env } wits
          where (c,ps)      = case mro2 env us of ([],_) -> (cStruct, us); _ -> (head us, tail us)   -- Just check that the mro exists, don't store it
                wits        = [ (NoQ (tvname tv), WInst p (NoQ $ tvarWit tv p0) wchain) | p0 <- ps, (wchain,p) <- findAncestry env p0 ]

defineSelfOpaque            :: EnvF x -> EnvF x
defineSelfOpaque env        = defineTVars [Quant tvSelf []] env

defineSelf                  :: QName -> QBinds -> EnvF x -> EnvF x
defineSelf qn q env         = defineTVars [Quant tvSelf [tc]] env
  where tc                  = TC qn [ tVar tv | Quant tv _ <- q ]

defineInst                  :: QName -> [WTCon] -> Name -> EnvF x -> EnvF x
defineInst n ps w env       = foldl addWit env wits
  where wits                = [ (n, WInst p (NoQ w) ws) | (ws,p) <- ps ]

setMod                      :: ModName -> EnvF x -> EnvF x
setMod m env                = env{ thismod = Just m }

addMod                     :: ModName -> TEnv -> EnvF x -> EnvF x
addMod m newte env          = env{ modules = addM ns (modules env) }
  where
    ModName ns              = m
    addM [] te              = newte
    addM (n:ns) te          = update n ns te
    update n ns ((x,i):te)
      | n == x,
        NModule te1 <- i    = (n, NModule $ addM ns te1) : te
    update n ns (ni:te)     = ni : update n ns te
    update n ns []          = (n, NModule $ addM ns []) : []


-- General Env queries -----------------------------------------------------------------------------------------------------------

inBuiltin                   :: EnvF x -> Bool
inBuiltin env               = length (modules env) == 1     -- mPrim only

stateScope                  :: EnvF x -> [Name]
stateScope env              = [ z | (z, NSVar _) <- names env ]

tvarScope0                  :: EnvF x -> [TVar]
tvarScope0 env              = [ TV k n | (n, NTVar k _) <- names env ]

tvarScope                   :: EnvF x -> [TVar]
tvarScope env               = tvarScope0 env \\ [tvSelf]

quantScope                  :: EnvF x -> QBinds
quantScope env              = [ Quant (TV k n) (if c==cStruct then [] else [c]) | (n, NTVar k c) <- names env, n /= nSelf ]

selfSubst                   :: EnvF x -> Substitution
selfSubst env               = [ (TV k n, tCon c) | (n, NTVar k c) <- names env, n == nSelf ]


-- Name queries -------------------------------------------------------------------------------------------------------------------

findQName                   :: QName -> EnvF x -> NameInfo
findQName (QName m n) env   = case findMod m env of
                                Just te -> case lookup n te of
                                    Just (NAlias qn) -> findQName qn env
                                    Just i -> i
                                    _ -> noItem m n
                                _ -> noModule m
findQName (NoQ n) env       = case lookup n (names env) of
                                Just (NAlias qn) -> findQName qn env
                                Just info -> info
                                Nothing -> nameNotFound n
findQName (GName m n) env
  | Just m == thismod env   = findQName (NoQ n) env
  | inBuiltin env,
    m==mBuiltin             = findQName (NoQ n) env
  | otherwise               = case lookupMod m env of
                                Just te -> case lookup n te of
                                    Just i -> i
                                    Nothing -> noItem m n -- error ("## Failed lookup of " ++ prstr n ++ " in module " ++ prstr m)
                                Nothing -> noModule m -- error ("## Failed lookup of module " ++ prstr m)


findName n env              = findQName (NoQ n) env

findMod                     :: ModName -> EnvF x -> Maybe TEnv
findMod m env | inBuiltin env, m==mBuiltin
                            = Just (names env)
findMod (ModName (n:ns)) env = case lookup n (names env) of
                                Just (NMAlias (ModName ns')) -> lookupMod (ModName $ ns'++ns) env
                                _ -> Nothing

lookupMod                   :: ModName -> EnvF x -> Maybe TEnv
lookupMod m env | inBuiltin env, m==mBuiltin
                            = Just (names env)
lookupMod (ModName ns) env  = f ns (modules env)
  where f [] te             = Just te
        f (n:ns) te         = case lookup n te of
                                Just (NModule te') -> f ns te'
                                Just (NMAlias (ModName m)) -> lookupMod (ModName $ m++ns) env
                                _ -> Nothing

isMod                       :: EnvF x -> [Name] -> Bool
isMod env ns                = maybe False (const True) (findMod (ModName ns) env)

isAlias                     :: Name -> EnvF x -> Bool
isAlias n env               = case lookup n (names env) of
                                Just NAlias{} -> True
                                _ -> False

tconKind                    :: QName -> EnvF x -> Kind
tconKind n env              = case findQName n env of
                                NAct q _ _ _ -> kind KType q
                                NClass q _ _ -> kind KType q
                                NProto q _ _ -> kind KProto q
                                _            -> notClassOrProto n
  where kind k []           = k
        kind k q            = KFun [ tvkind v | Quant v _ <- q ] k

isDef                       :: EnvF x -> QName -> Bool
isDef env n                 = case findQName n env of
                                NDef _ _ -> True
                                _ -> False

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

isDefOrClass                :: EnvF x -> QName -> Bool
isDefOrClass env n          = case findQName n env of
                                NDef _ _ -> True
                                NClass _ _ _ -> True
                                _ -> False

findWitness                 :: EnvF x -> QName -> (Witness->Bool) -> Maybe Witness
findWitness env cn f        = listToMaybe $ filter f $ allWitnesses env cn

allWitnesses                :: EnvF x -> QName -> [Witness]
allWitnesses env cn         = [ w | (c,w) <- witnesses env, qualEq env c cn ]

allImpls                    :: EnvF x -> QName -> [TCon]
allImpls env qn             = reverse $ [ sat c | (c,w) <- witnesses env, qualEq env qn (tcname $ proto w) ]
  where sat c               = let (q,_,_) = findConName c env in TC c [ tWild | _ <- q ]

implProto                   :: EnvF x -> TCon -> Witness -> Bool
implProto env p w           = case w of
                                WClass{} -> qualEq env (tcname p) (tcname p')
                                WInst{}  -> qualEq env p p'
  where p'                  = proto w

implProto'                  :: EnvF x -> QName -> Witness -> Bool
implProto' env pn w         = case w of
                                WClass{} -> qualEq env pn (tcname $ proto w)
                                WInst{}  -> qualEq env pn (tcname $ proto w)

hasAttr                     :: EnvF x -> Name -> Witness -> Bool
hasAttr env n w             = n `elem` conAttrs env (tcname $ proto w)

hasWitness                  :: EnvF x -> QName -> QName -> Bool
hasWitness env cn pn        =  not $ null $ findWitness env cn (qualEq env pn . tcname . proto)

getWitness                  :: EnvF x -> QName -> TCon -> Witness
getWitness env cn p         = fromJust $ findWitness env cn (qualEq env (tcname p) . tcname . proto)

-- TCon queries ------------------------------------------------------------------------------------------------------------------

findAttr                    :: EnvF x -> TCon -> Name -> Maybe (Expr->Expr, TSchema, Maybe Deco)
findAttr env tc n           = fmap summarize $ findAttrInfo env tc n

summarize (w, NSig sc d)    = (w, sc, Just d)
summarize (w, NDef sc d)    = (w, sc, Just d)
summarize (w, NVar t)       = (w, monotype t, Nothing)
summarize (w, NSVar t)      = (w, monotype t, Nothing)

findAttrInfo                :: EnvF x -> TCon -> Name -> Maybe (Expr->Expr, NameInfo)
findAttrInfo env tc n       = findIn [ (w,u,te') | (w,u) <- findAncestry env tc, let (_,te') = findCon env u ]
  where findIn ((w,u,te):tes) = case lookup n te of
                                Just ni -> Just (wexpr w, ni)
                                Nothing -> findIn tes
        findIn []           = Nothing

abstractAttr                :: EnvF x -> TCon -> Name -> Bool
abstractAttr env tc n       = case lookup n $ snd $ splitSigs $ concat [ attrEnv env c | (_,c) <- findAncestry env tc ] of
                                Just i -> False
                                _  -> True

findAttr'                   :: EnvF x -> TCon -> Name -> (TSchema, Maybe Deco)
findAttr' env tc n          = case findAttr env tc n of
                                  Just (_, sc, mbdec) -> (sc, mbdec)
                                  Nothing -> error ("#### findAttr' fails for " ++ prstr tc ++ " . " ++ prstr n)

findAncestry                :: EnvF x -> TCon -> [WTCon]
findAncestry env tc         = ([Nothing],tc) : fst (findCon env tc)

findAncestor                :: EnvF x -> TCon -> QName -> Maybe (Expr->Expr,TCon)
findAncestor env p qn       = listToMaybe [ (wexpr ws, p') | (ws,p') <- findAncestry env p, qualEq env (tcname p') qn ]

hasAncestor'                :: EnvF x -> QName -> QName -> Bool
hasAncestor' env qn qn'     = any (qualEq env qn') [ tcname c' | (w,c') <- us ]
  where (_,us,_)            = findConName qn env

hasAncestor                 :: EnvF x -> TCon -> TCon -> Bool
hasAncestor env c c'        = hasAncestor' env (tcname c) (tcname c')

commonAncestors             :: EnvF x -> TCon -> TCon -> [TCon]
commonAncestors env c1 c2   = filter (\c -> any (qualEq env (tcname c)) ns) $ map snd (findAncestry env c1)
  where ns                  = map (tcname . snd) (findAncestry env c2)

directAncestors             :: EnvF x -> QName -> [QName]
directAncestors env qn      = [ tcname p | (ws,p) <- us, null $ catMaybes ws ]
  where (q,us,te)           = findConName qn env

allAncestors                :: EnvF x -> TCon -> [TCon]
allAncestors env tc         = [ TC n (map (const tWild) ts) | (_, TC n ts) <- us ]
  where (us,te)             = findCon env tc

allAncestors'               :: EnvF x -> QName -> [QName]
allAncestors' env qn        = map (tcname . snd) us
  where (q,us,te)           = findConName qn env

allDescendants              :: EnvF x -> TCon -> [TCon]
allDescendants env tc       = [ c | c <- allCons env, hasAncestor' env (tcname c) (tcname tc) ]

allDescendants'             :: EnvF x -> QName -> [QName]
allDescendants' env qn      = [ n | n <- allCons' env, hasAncestor' env n qn ]

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
allAttrs env qn             = concat [ conAttrs env qn' | qn' <- qn : allAncestors' env qn ]

attrEnv                     :: EnvF x -> TCon -> TEnv
attrEnv env c               = snd $ findCon env c

fullAttrEnv                 :: EnvF x -> TCon -> TEnv
fullAttrEnv env tc          = normTEnv $ init ++ concat (reverse tes)   -- reverse guarantees inherited methods are listed in original order
  where tes                 = [ attrEnv env c | (_,c) <- findAncestry env tc ]
        init                = take 1 $ filter ((==initKW) . fst) $ concat tes

inheritedAttrs              :: EnvF x -> QName -> [(QName,[Name])]
inheritedAttrs env n        = inh (dom $ snd $ splitSigs te) us
  where (_,us,te)           = findConName n env
        inh ns0 []          = []
        inh ns0 (u:us)
          | null ns1        = inh ns0 us
          | otherwise       = (c,ns1) : inh (ns1++ns0) us
          where c           = tcname (snd u)
                (_,_,te)    = findConName c env
                ns1         = (dom $ snd $ splitSigs te) \\ ns0

allCons                     :: EnvF x -> [TCon]
allCons env                 = reverse locals ++ concat [ cons m (lookupMod m env) | m <- moduleRefs (names env) ]
  where locals              = [ TC (NoQ n) (args i) | (n,i) <- names env, con i ]
        con NClass{}        = True
        con NAct{}          = True
        con _               = False
        cons m (Just te)    = [ TC (GName m n) (args i) | (n,i) <- te, con i ] ++ concat [ cons (modCat m n) (Just te') | (n,NModule te') <- te ]
        args (NClass q _ _) = [ tWild | _ <- q ]
        args (NAct q _ _ _) = [ tWild | _ <- q ]

allCons'                    :: EnvF x -> [QName]
allCons' env                = map tcname $ allCons env

allProtos                   :: EnvF x -> [TCon]
allProtos env               = reverse locals ++ concat [ protos m (lookupMod m env) | m <- moduleRefs (names env) ]
  where locals              = [ TC (NoQ n) (args i) | (n,i) <- names env, proto i ]
        proto NProto{}      = True
        proto _             = False
        protos m (Just te)  = [ TC (GName m n) (args i) | (n,i) <- te, proto i ] ++ concat [ protos (modCat m n) (Just te') | (n,NModule te') <- te ]
        args (NProto q _ _) = [ tWild | _ <- q ]

allProtos'                  :: EnvF x -> [QName]
allProtos' env              = map tcname $ allProtos env

allVars                     :: EnvF x -> Kind -> [TVar]
allVars env k               = [ TV k n | (n,NTVar k' _) <- names env, k == k' ]

wexpr                       :: [Maybe QName] -> Expr -> Expr
wexpr []                    = id
wexpr (Nothing : w)         = wexpr w
wexpr (Just n : w)          = wexpr w . (\e -> eDot e (witAttr n))

gname env n                 = unalias env (NoQ n)


-- TVar queries ------------------------------------------------------------------------------------------------------------------

findSelf                    :: EnvF x -> TCon
findSelf env                = case findName (tvname tvSelf) env of
                                NTVar _ c -> c

findTVBound                 :: EnvF x -> TVar -> TCon
findTVBound env tv          = case findName (tvname tv) env of
                                NTVar _ c -> c
                                _ -> err1 tv "Unknown type variable"

findTVAttr                  :: EnvF x -> TVar -> Name -> Maybe (Expr->Expr, TSchema, Maybe Deco)
findTVAttr env tv n         = findAttr env c n
  where c                   = findTVBound env tv

tvarWit                     :: TVar -> TCon -> Name
tvarWit tv p                = Derived (name "w") $ Derived (deriveQ $ tcname p) (tvname tv)


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
      where headmatch                   = qualEq env (tcname u1) (tcname u2)

    absent                              :: WTCon -> [WTCon] -> Bool
    absent (w,h) us                     = tcname h `notElem` map (tcname . snd) us



-- Import handling (local definitions only) ----------------------------------------------

getImps                         :: FilePath -> EnvF x -> [Import] -> IO (EnvF x)
getImps prefix env []           = return env
getImps prefix env (i:is)       = do env' <- impModule prefix env i
                                     getImps prefix env' is


impModule                       :: FilePath -> EnvF x -> Import -> IO (EnvF x)
impModule prefix env (Import _ ms)
                                = imp env ms
  where imp env []              = return env
        imp env (ModuleItem m as : is)
                                = do (env1,te) <- doImp prefix env m
                                     let ModName (m0:_) = m
                                         env2 = maybe (define [(m0, NMAlias $ ModName [m0])] env1) (\n->define [(n, NMAlias m)] env1) as
                                     imp (importWits m te env2) is
impModule prefix env (FromImport _ (ModRef (0,Just m)) items)
                                = do (env1,te) <- doImp prefix env m
                                     return $ importSome items m te $ importWits m te $ env1
impModule prefix env (FromImportAll _ (ModRef (0,Just m)))
                                = do (env1,te) <- doImp prefix env m
                                     return $ importAll m te $ importWits m te $ env1
impModule _ _ i                 = illegalImport (loc i)


moduleRefs te                   = nub $ [ m | (_,NMAlias m) <- te ] ++ [ m | (_,NAlias (GName m _)) <- te ]

subImp prefix env []            = return env
subImp prefix env (m:ms)        = do (env',_) <- doImp prefix env m
                                     subImp prefix env' ms

doImp prefix env m              = case lookupMod m env of
                                    Just te -> return (env, te)
                                    Nothing -> do
                                        found <- doesFileExist fpath
                                        unless found (fileNotFound m)
                                        te <- InterfaceFiles.readFile fpath
                                        env' <- subImp prefix env (moduleRefs te)
                                        return (addMod m te env', te)
  where fpath                   = joinPath (prefix : modPath m) ++ ".ty"


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
    imp (n, NAct _ _ _ _)   = Just (n, NAlias (GName m n))
    imp (n, NClass _ _ _)   = Just (n, NAlias (GName m n))
    imp (n, NProto _ _ _)   = Just (n, NAlias (GName m n))
    imp (n, NExt _ _ _ _)   = Nothing
    imp (n, NAlias _)       = Just (n, NAlias (GName m n))
    imp (n, NVar t)         = Just (n, NAlias (GName m n))
    imp (n, NDef t d)       = Just (n, NAlias (GName m n))
    imp _                   = Nothing                               -- cannot happen

importWits                  :: ModName -> TEnv -> EnvF x -> EnvF x
importWits m te env         = foldl addWit env ws
  where ws                  = [ (c, WClass q p (GName m n) ws) | (n, NExt c q ps te') <- te, (ws,p) <- ps ]




headvar (Impl w (TVar _ v) p)       = v

headvar (Cast (TVar _ v) (TVar _ v'))
  | not $ univar v                  = v'
headvar (Cast (TVar _ v) t)         = v
headvar (Cast t (TVar _ v))         = v     -- ?

headvar (Sub w (TVar _ v) (TVar _ v'))
  | not $ univar v                  = v'
headvar (Sub w (TVar _ v) t)        = v
headvar (Sub w t (TVar _ v))        = v     -- ?

headvar (Sel w (TVar _ v) n t)      = v

headvar (Mut (TVar _ v) n t)        = v


-- Error handling ----------------------------------------------------------------------------------------------------

data CompilationError               = KindError SrcLoc Kind Kind
                                    | InfiniteKind SrcLoc KVar Kind

                                    | FileNotFound ModName
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
                                    | DecorationMismatch Name TSchema Deco
                                    | OtherError SrcLoc String
                                    deriving (Show)

instance Control.Exception.Exception CompilationError

instance HasLoc CompilationError where
    loc (KindError l _ _)           = l
    loc (InfiniteKind l _ _)        = l

    loc (FileNotFound n)            = loc n
    loc (NameNotFound n)            = loc n
    loc (NameReserved n)            = loc n
    loc (NameBlocked n)             = loc n
    loc (NameUnexpected n)          = loc n
    loc (TypedReassign p)           = loc p
    loc (IllegalRedef n)            = loc n
    loc (IllegalExtension n)        = loc n
    loc (MissingSelf n)             = loc n
    loc (IllegalImport l)           = l
    loc (DuplicateImport n)         = loc n
    loc (NoModule m)                = loc m
    loc (NoItem m n)                = loc n
    loc (NoClassOrProto n)          = loc n
    loc (DecorationMismatch n t d)  = loc n
    loc (OtherError l str)          = l


compilationError                    :: CompilationError -> (SrcLoc, String)
compilationError err                = (loc err, render (expl err))
  where
    expl (KindError l k1 k2)        = text "Expected a" <+> pretty k2 <> comma <+> text "actual kind is" <+> pretty k1
    expl (InfiniteKind l v k)       = text "Infinite kind inferred:" <+> pretty v <+> equals <+> pretty k

    expl (FileNotFound n)           = text "Type interface file not found for" <+> pretty n
    expl (NameNotFound n)           = text "Name" <+> pretty n <+> text "is not in scope"
    expl (NameReserved n)           = text "Name" <+> pretty n <+> text "is reserved but not yet defined"
    expl (NameBlocked n)            = text "Name" <+> pretty n <+> text "is currently not accessible"
    expl (NameUnexpected n)         = text "Unexpected variable name:" <+> pretty n
    expl (TypedReassign p)          = text "Type annotation on reassignment:" <+> pretty p
    expl (IllegalRedef n)           = text "Illegal redefinition of" <+> pretty n
    expl (IllegalExtension n)       = text "Illegal extension of" <+> pretty n
    expl (MissingSelf n)            = text "Missing 'self' parameter in definition of"
    expl (IllegalImport l)          = text "Relative import not yet supported"
    expl (DuplicateImport n)        = text "Duplicate import of name" <+> pretty n
    expl (NoModule m)               = text "Module" <+> pretty m <+> text "does not exist"
    expl (NoItem m n)               = text "Module" <+> pretty m <+> text "does not export" <+> pretty n
    expl (NoClassOrProto n)         = text "Class or protocol name expected, got" <+> pretty n
    expl (DecorationMismatch n t d) = text "Decoration for" <+> pretty n <+> text "does not match signature" <+> pretty d
    expl (OtherError l str)         = text str


noKUnify l k1 k2                    = Control.Exception.throw $ KindError l k1 k2
infiniteKind l v k                  = Control.Exception.throw $ InfiniteKind l v k

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
decorationMismatch n t d            = Control.Exception.throw $ DecorationMismatch n t d
err l s                             = Control.Exception.throw $ OtherError l s

err0 xs s                           = err (loc $ head xs) s
err1 x s                            = err (loc x) (s ++ " " ++ prstr x)
err2 xs s                           = err (loc $ head xs) (s ++ " " ++ prstrs xs)
err3 l xs s                         = err l (s ++ " " ++ prstrs xs)

notYetExpr e                        = notYet (loc e) e

