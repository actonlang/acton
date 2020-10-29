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




mkEnv                       :: (FilePath,FilePath) -> Env0 -> Module -> IO Env0
mkEnv paths env m           = getImps paths env' (imps m)
  where env'                = env{ thismod = modname m }


type TEnv                   = [(Name, NameInfo)]

data EnvF x                 = EnvF {
                                names      :: TEnv,
                                modules    :: TEnv,
                                witnesses  :: [(QName,Witness)],
                                thismod    :: ModName,
                                envX       :: x }

type Env0                   = EnvF ()


setX                        :: EnvF y -> x -> EnvF x
setX env x                  = EnvF { names = names env, modules = modules env, witnesses = witnesses env, thismod = thismod env, envX = x }

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
      where m a@GName{}  b@GName{}  = mname a == mname b && noq a == noq b

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
    unalias env (QName m n)         = case lookupMod m env of
                                        Just te -> case lookup n te of
                                                      Just (NAlias qn) -> qn
                                                      Just _ -> GName m' n
                                                      _ -> noItem m n
                                        Nothing -> error ("#### unalias fails for " ++ prstr (QName m n))
      where m'                      = unalias env m
    unalias env (NoQ n)             = case lookup n (names env) of
                                        Just (NAlias qn) -> qn
                                        _ -> GName (thismod env) n
    unalias env (GName m n)         = GName m n
                                    
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

uniChk env us
  | not $ null bad          = err2 bad "Illegal union element:"
  | otherwise               = us
  where bad                 = [ c | UCon c <- us, unalias env c `notElem` uniCons ]


uniAbove env us                         = [ ns | ns <- nss, length ns > 1 ]
  where nss                             = [ catMaybes [i,f,b,s] | i <- mb qnInt, f <- mb qnFloat, b <- mb qnBool, s <- mbStr ]
        mb c | uniElem env us (UCon c)  = [Just c]
             | otherwise                = [Nothing, Just c]
        mbStr | not $ null lits         = [Just qnStr]
              | otherwise               = mb qnStr
          where lits                    = filter uniLit us

uniBelow env us                         = [ ns | ns <- nss, length ns > 1 ]
  where nss                             = [ catMaybes [i,f,b,s] | i <- mb qnInt, f <- mb qnFloat, b <- mb qnBool, s <- mb qnStr ]
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
nTerms te                   = [ (n,i) | (n,i) <- te, isTerm i ]
  where isTerm NDef{}       = True
        isTerm NVar{}       = True
        isTerm _            = False

noDefs                      :: TEnv -> TEnv
noDefs te                   = [ (n,i) | (n,i) <- te, not $ isDef i ]
  where isDef NDef{}        = True
        isDef NAct{}        = True
        isDef _             = False

noAliases                   :: TEnv -> TEnv
noAliases te                = [ (n,i) | (n,i) <- te, not $ isAlias i ]
  where isAlias NAlias{}    = True
        isAlias NMAlias{}   = True
        isAlias _           = False

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


initEnv                    :: Bool -> IO Env0
initEnv nobuiltin           = if nobuiltin
                                then return $ EnvF{ names = [(nPrim,NMAlias mPrim)],
                                                    modules = [(nPrim,NModule envPrim)],
                                                    witnesses = [],
                                                    thismod = ModName [],
                                                    envX = () }
                                else do path <- getExecutablePath
                                        envBuiltin <- InterfaceFiles.readFile (joinPath [takeDirectory path,"__builtin__.ty"])
                                        let env0 = EnvF{ names = [(nPrim,NMAlias mPrim), (nBuiltin,NMAlias mBuiltin)],
                                                         modules = [(nPrim,NModule envPrim), (nBuiltin,NModule envBuiltin)],
                                                         thismod = ModName [],
                                                         witnesses = [],
                                                         envX = () }
                                            env = importAll mBuiltin envBuiltin $ importWits mBuiltin envBuiltin $ env0
                                        return env

envPrim                     = primMkEnv NClass NDef NVar

withModulesFrom             :: EnvF x -> EnvF x -> EnvF x
env `withModulesFrom` env'  = env{modules = modules env'}

addWit                      :: EnvF x -> (QName,Witness) -> EnvF x
addWit env cwit
  | exists                  = env
  | otherwise               = env{ witnesses = cwit : witnesses env }
  where exists              = any (qmatch env cwit) (witnesses env)

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
  | m == thismod env        = findQName (NoQ n) env
  | otherwise               = case lookupMod m env of
                                Just te -> case lookup n te of
                                    Just i -> i
                                    Nothing -> error ("## Failed lookup of " ++ prstr n ++ " in module " ++ prstr m)
                                Nothing -> error ("## Failed lookup of module " ++ prstr m)


findName n env              = findQName (NoQ n) env

findMod m env | inBuiltin env, m==mBuiltin
                            = Just (names env)
findMod (ModName (n:ns)) env = case lookup n (names env) of
                                Just (NMAlias (ModName ns')) -> lookupMod (ModName $ ns'++ns) env
                                _ -> Nothing

lookupMod                   :: ModName -> EnvF x -> Maybe TEnv
lookupMod (ModName ns) env  = f ns (modules env)
  where f [] te             = Just te
        f (n:ns) te         = case lookup n te of
                                Just (NModule te') -> f ns te'
                                Just (NMAlias (ModName m)) -> lookupMod (ModName $ m++ns) env
                                _ -> Nothing

isMod                       :: EnvF x -> [Name] -> Bool
isMod env ns                = maybe False (const True) (findMod (ModName ns) env)


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

findAttr                    :: EnvF x -> TCon -> Name -> Maybe (Expr->Expr,TSchema,Maybe Deco)
findAttr env tc n           = findIn [ (w,u,te') | (w,u) <- findAncestry env tc, let (_,te') = findCon env u ]
  where findIn ((w,u,te):tes) = case lookup n te of
                                Just (NSig sc d) -> Just (wexpr w, sc, Just d)
                                Just (NDef sc d) -> Just (wexpr w, sc, Just d)
                                Just (NVar t)    -> Just (wexpr w, monotype t, Nothing)
                                Nothing          -> findIn tes
        findIn []           = Nothing

findAttr'                   :: EnvF x -> TCon -> Name -> (TSchema, Bool)
findAttr' env tc n          = case findAttr env tc n of
                                  Just (_, sc, mbdec) -> (sc, funAsClos mbdec)
                                  Nothing -> error ("#### findAttr' fails for " ++ prstr tc ++ " . " ++ prstr n)

funAsClos                   :: Maybe Deco -> Bool
funAsClos Nothing           = True
funAsClos (Just Property)   = True
funAsClos _                 = False

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

attrEnv                     :: EnvF x -> TCon -> TEnv
attrEnv env c               = snd $ findCon env c

fullAttrEnv                 :: EnvF x -> TCon -> TEnv
fullAttrEnv env tc          = normTEnv $ init ++ concat (reverse tes)
  where tes                 = [ attrEnv env c | (_,c) <- findAncestry env tc ]
        init                = take 1 $ filter ((==initKW) . fst) $ concat tes


allCons                     :: EnvF x -> [QName]
allCons env                 = [ gname env n | (n,i) <- names env, con i ] ++ concat [ cons m (lookupMod m env) | m <- moduleRefs (names env) ]
  where con NClass{}        = True
        con NAct{}          = True
        con _               = False
        cons m (Just te)    = [ GName m n | (n,i) <- te, con i ]

allProtos                   :: EnvF x -> [QName]
allProtos env               = [ gname env n | (n,i) <- names env, proto i ] ++ concat [ protos m (lookupMod m env) | m <- moduleRefs (names env) ]
  where proto NProto{}      = True
        proto _             = False
        protos m (Just te)  = [ GName m n | (n,i) <- te, proto i ]

allVars                     :: EnvF x -> Kind -> [TVar]
allVars env k               = [ TV k n | (n,NTVar k' _) <- names env, k == k' ]

wexpr                       :: [Maybe QName] -> Expr -> Expr
wexpr []                    = id
wexpr (Nothing : w)         = wexpr w
wexpr (Just n : w)          = wexpr w . (\e -> eDot e (witAttr n))

gname env n                 = GName (thismod env) n


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
      where headmatch                   = qmatch env (tcname u1) (tcname u2)

    absent                              :: WTCon -> [WTCon] -> Bool
    absent (w,h) us                     = tcname h `notElem` map (tcname . snd) us



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
                                     let ModName (m0:_) = m
                                         env2 = maybe (define [(m0, NMAlias $ ModName [m0])] env1) (\n->define [(n, NMAlias m)] env1) as
                                     imp (importWits m te env2) is
impModule ps env (FromImport _ (ModRef (0,Just m)) items)
                                = do (env1,te) <- doImp ps env m
                                     return $ importSome items m te $ importWits m te $ env1
impModule ps env (FromImportAll _ (ModRef (0,Just m)))
                                = do (env1,te) <- doImp ps env m
                                     return $ importAll m te $ importWits m te $ env1
impModule _ _ i                 = illegalImport (loc i)


moduleRefs te                   = nub $ [ m | (_,NMAlias m) <- te ] ++ [ m | (_,NAlias (GName m _)) <- te ]


subImp paths env []             = return env
subImp paths env (m:ms)         = do (env',_) <- doImp paths env m
                                     subImp paths env' ms

doImp (p,sysp) env m            = case lookupMod m env of
                                    Just te -> return (env, te)
                                    Nothing -> do
                                        found <- doesFileExist fpath
                                        if found
                                         then do te <- InterfaceFiles.readFile fpath
                                                 env' <- subImp (p,sysp) env (moduleRefs te)
                                                 return (addMod m te env', te)
                                         else do found <- doesFileExist fpath2
                                                 unless found (fileNotFound m)
                                                 te <- InterfaceFiles.readFile fpath2
                                                 env' <- subImp (p,sysp) env (moduleRefs te)
                                                 return (addMod m te env', te)
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

