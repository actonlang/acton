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
import System.FilePath.Posix (joinPath,takeDirectory)
import System.Directory (doesFileExist)
import System.Environment (getExecutablePath)
import Control.Monad
import Control.Monad.Except
import qualified Data.HashMap.Strict as M

import Acton.Syntax
import Acton.Builtin
import Acton.Prim
import Acton.Printer
import Acton.Names
import Acton.Subst
import Acton.Unify
import Acton.TypeM
import Utils
import Pretty
import InterfaceFiles
import Prelude hiding ((<>))





mkEnv                       :: [FilePath] -> Env0 -> Module -> IO Env0
mkEnv spath env m           = getImps spath env (imps m)


data EnvF x                 = EnvF {
                                names      :: TEnv,
                                imports    :: [ModName],
                                modules    :: TEnv,
                                hmodules   :: HTEnv,
                                witnesses  :: [Witness],
                                thismod    :: Maybe ModName,
                                context    :: [EnvCtx],
                                envX       :: x }

type Env0                   = EnvF ()


setX                        :: EnvF y -> x -> EnvF x
setX env x                  = EnvF { names = names env, imports = imports env, modules = modules env,
                                     hmodules = hmodules env, witnesses = witnesses env, thismod = thismod env,
                                     context = context env, envX = x }

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


mapModules1                 :: ((Name,NameInfo) -> (Name,NameInfo)) -> Env0 -> Env0
mapModules1 f env           = mapModules (\_ _ ni -> [f ni]) env

mapModules                  :: (Env0 -> ModName -> (Name,NameInfo) -> TEnv) -> Env0 -> Env0
mapModules f env            = env1 { hmodules = convTEnv2HTEnv (modules env1) }
  where env1                =  walk env0 [] mods
        env0                = env{ modules = [prim] }
        prim : mods         = modules env

        walk env ns []      = env
        walk env ns ((n,NModule te1 _):te)
                            = walk env2 ns te
          where env1        = env{ modules = app ns (modules env) [(n, NModule [] Nothing)] }
                env2        = walk env1 (ns++[n]) te1
        walk env ns (ni:te) = walk env1 ns te
          where env1        = env{ modules = app ns (modules env) (f env (ModName ns) ni) }

        app (n:ns) ((m,NModule te1 doc):te) te'
          | n == m          = (m, NModule (app ns te1 te') doc) : te
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

instance Pretty Witness where
    pretty (WClass q t p w ws _) = text "WClass" <+> prettyQual q <+> pretty t <+> parens (pretty p) <+>
                                      equals <+> pretty (wexpr ws (eCall (eQVar w) []))
    pretty (WInst q t p w ws)   = text "WInst" <+> prettyQual q <+> pretty t <+> parens (pretty p) <+>
                                      equals <+> pretty (wexpr ws (eQVar w))

instance Pretty TEnv where
    pretty tenv                 = vcat (map pretty $ normTEnv tenv)

instance (Pretty x) => Pretty (EnvF x) where
    pretty env                  = text "--- modules:"  $+$
                                  vcat (map pretty (modules env)) $+$
                                  text "--- names" <+> pretty (thismod env) <> colon $+$
                                  vcat (map pretty (names env)) $+$
                                  text "--- imports" <+> pretty (thismod env) <> colon $+$
                                  vcat (map pretty (imports env)) $+$
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
    pretty (n, NDef t d doc)    = prettyDec d $ pretty n <+> colon <+> pretty t $+$ nest 4 (prettyDocstring doc)
    pretty (n, NSig t d doc)    = prettyDec d $ pretty n <+> colon <+> pretty t $+$ nest 4 (prettyDocstring doc)
    pretty (n, NAct q p k te doc)
                                = text "actor" <+> pretty n <> nonEmpty brackets commaList q <+>
                                  parens (prettyFunRow p k) <> colon $+$ nest 4 (prettyDocstring doc) $+$ (nest 4 $ prettyOrPass te)
    pretty (n, NClass q us te doc)
                                = text "class" <+> pretty n <> nonEmpty brackets commaList q <+>
                                  nonEmpty parens commaList us <> colon $+$ nest 4 (prettyDocstring doc) $+$ (nest 4 $ prettyOrPass te)
    pretty (n, NProto q us te doc)
                                = text "protocol" <+> pretty n <> nonEmpty brackets commaList q <+>
                                  nonEmpty parens commaList us <> colon $+$ nest 4 (prettyDocstring doc) $+$ (nest 4 $ prettyOrPass te)
    pretty (w, NExt [] c ps te opts doc)
                                = {-pretty w  <+> colon <+> -}
                                  text "extension" <+> pretty c <+> parens (commaList ps) <>
                                  colon $+$ nest 4 (prettyDocstring doc) $+$ (nest 4 $ prettyOrPass te)
    pretty (w, NExt q c ps te opts doc)
                                = {-pretty w  <+> colon <+> -}
                                  text "extension" <+> pretty q <+> text "=>" <+> pretty c <+> parens (commaList ps) <>
                                  colon $+$ nest 4 (prettyDocstring doc) $+$ (nest 4 $ prettyOrPass te)
    pretty (n, NTVar k c)       = pretty n <> parens (pretty c)
    pretty (n, NAlias qn)       = text "alias" <+> pretty n <+> equals <+> pretty qn
    pretty (n, NMAlias m)       = text "module" <+> pretty n <+> equals <+> pretty m
    pretty (n, NModule te doc)  = text "module" <+> pretty n <> colon $+$ nest 4 (prettyDocstring doc) $+$ nest 4 (pretty te)
    pretty (n, NReserved)       = pretty n <+> text "(reserved)"

prettyOrPass te
  | isEmpty doc                 = text "pass"
  | otherwise                   = doc
  where doc                     = pretty te

prettyDocstring :: Maybe String -> Doc
prettyDocstring Nothing         = empty
prettyDocstring (Just docstring) = text "\"\"\"" <> text docstring <> text "\"\"\""

instance Pretty WTCon where
    pretty (ws,u)               = --dotCat prettyW ws <+> colon <+>
                                  pretty u
      where prettyW (Left n)    = text "L"
            prettyW (Right n)   = text "R"

instance (USubst x) => USubst (EnvF x) where
    usubst env                  = do ne <- usubst (names env)
                                     we <- usubst (witnesses env)
                                     ex <- usubst (envX env)
                                     return env{ names = ne, witnesses = we, envX = ex }

instance (UFree x) => UFree (EnvF x) where
    ufree env                   = ufree (names env) ++ ufree (witnesses env) ++ ufree (envX env)


-- VFree ----------------------------------------------------------------------------------------

instance VFree NameInfo where
    vfree (NVar t)              = vfree t
    vfree (NSVar t)             = vfree t
    vfree (NDef t d _)          = vfree t
    vfree (NSig t d _)          = vfree t
    vfree (NAct q p k te _)     = (vfree q ++ vfree p ++ vfree k ++ vfree te) \\ (tvSelf : qbound q)
    vfree (NClass q us te _)    = (vfree q ++ vfree us ++ vfree te) \\ (tvSelf : qbound q)
    vfree (NProto q us te _)    = (vfree q ++ vfree us ++ vfree te) \\ (tvSelf : qbound q)
    vfree (NExt q c ps te _ _)  = (vfree q ++ vfree c ++ vfree ps ++ vfree te) \\ (tvSelf : qbound q)
    vfree (NTVar k c)           = vfree c
    vfree (NAlias qn)           = []
    vfree (NMAlias qn)          = []
    vfree (NModule te doc)      = []        -- actually vfree te, but a module has no free variables on the top level
    vfree NReserved             = []

instance VFree WTCon where
    vfree (w,u)                 = vfree u


-- VSubst ---------------------------------------------------------------------------------------

instance VSubst NameInfo where
    vsubst s (NVar t)           = NVar (vsubst s t)
    vsubst s (NSVar t)          = NSVar (vsubst s t)
    vsubst s (NDef t d x)       = NDef (vsubst s t) d x
    vsubst s (NSig t d x)       = NSig (vsubst s t) d x
    vsubst s (NAct q p k te x)  = NAct (vsubst s q) (vsubst s p) (vsubst s k) (vsubst s te) x
    vsubst s (NClass q us te x) = NClass (vsubst s q) (vsubst s us) (vsubst s te) x
    vsubst s (NProto q us te x) = NProto (vsubst s q) (vsubst s us) (vsubst s te) x
    vsubst s (NExt q c ps te opts x) = NExt (vsubst s q) (vsubst s c) (vsubst s ps) (vsubst s te) opts x
    vsubst s (NTVar k c)        = NTVar k (vsubst s c)
    vsubst s (NAlias qn)        = NAlias qn
    vsubst s (NMAlias m)        = NMAlias m
    vsubst s (NModule te x)     = NModule te x          -- actually vsubst s te, but te has no free variables (top-level)
    vsubst s NReserved          = NReserved

instance VSubst WTCon where
    vsubst s (w,u)              = (w, vsubst s u)


-- UFree ----------------------------------------------------------------------------------------

instance UFree NameInfo where
    ufree (NVar t)              = ufree t
    ufree (NSVar t)             = ufree t
    ufree (NDef t d _)          = ufree t
    ufree (NSig t d _)          = ufree t
    ufree (NAct q p k te _)     = ufree q ++ ufree p ++ ufree k ++ ufree te
    ufree (NClass q us te _)    = ufree q ++ ufree us ++ ufree te
    ufree (NProto q us te _)    = ufree q ++ ufree us ++ ufree te
    ufree (NExt q c ps te _ _)  = ufree q ++ ufree c ++ ufree ps ++ ufree te
    ufree (NTVar k c)           = ufree c
    ufree (NAlias qn)           = []
    ufree (NMAlias qn)          = []
    ufree (NModule te doc)      = []        -- actually ufree te, but a module has no free variables on the top level
    ufree NReserved             = []

instance UFree WTCon where
    ufree (w,u)                 = ufree u

instance UFree Witness where
    ufree w@WClass{}            = []
    ufree w@WInst{}             = ufree (wtype w) ++ ufree (proto w)

instance Polarity NameInfo where
    polvars (NVar t)            = polvars t
    polvars (NSVar t)           = invvars t
    polvars (NDef t d _)        = polvars t
    polvars (NSig t d _)        = polvars t
    polvars (NAct q p k te _)   = polvars q `polcat` polneg (polvars p `polcat` polvars k) `polcat` polvars te
    polvars (NClass q us te _)  = polvars q `polcat` polvars us `polcat` polvars te
    polvars (NProto q us te _)  = polvars q `polcat` polvars us `polcat` polvars te
    polvars (NExt q c ps te _ _) = polvars q `polcat` polvars c `polcat` polvars ps `polcat` polvars te
    polvars (NTVar k c)         = polvars c
    polvars _                   = ([],[])


-- USubst ---------------------------------------------------------------------------------------

instance USubst NameInfo where
    usubst (NVar t)             = NVar <$> usubst t
    usubst (NSVar t)            = NSVar <$> usubst t
    usubst (NDef t d doc)       = NDef <$> usubst t <*> return d <*> return doc
    usubst (NSig t d doc)       = NSig <$> usubst t <*> return d <*> return doc
    usubst (NAct q p k te doc)  = NAct <$> usubst q <*> usubst p <*> usubst k <*> usubst te <*> return doc
    usubst (NClass q us te doc) = NClass <$> usubst q <*> usubst us <*> usubst te <*> return doc
    usubst (NProto q us te doc) = NProto <$> usubst q <*> usubst us <*> usubst te <*> return doc
    usubst (NExt q c ps te opts doc) = NExt <$> usubst q <*> usubst c <*> usubst ps <*> usubst te <*> return opts <*> return doc
    usubst (NTVar k c)          = NTVar k <$> usubst c
    usubst (NAlias qn)          = NAlias <$> return qn
    usubst (NMAlias m)          = NMAlias <$> return m
    usubst (NModule te doc)     = NModule <$> return te <*> return doc     -- actually usubst te, but te has no free variables (top-level)
    usubst NReserved            = return NReserved

instance USubst Witness where
    usubst w@WClass{}           = return w                      -- A WClass (i.e., an extension) can't have any free type variables
    usubst w@WInst{}            = do t <- usubst (wtype w)
                                     p <- usubst (proto w)
                                     return w{ wtype  = t, proto = p }

instance USubst WTCon where
    usubst (w,u)                = (,) <$> return w <*> usubst u


-- Polarity -------------------------------------------------------------------------------------

instance Polarity WTCon where
    polvars (w, c)                  = polvars c

instance Polarity (Name,NameInfo) where
    polvars (n, i)                  = polvars i


-- Tailvars -------------------------------------------------------------------------------------

instance Tailvars (Name, NameInfo) where
    tailvars (n, NVar t)            = tailvars t
    tailvars (n, NSVar t)           = tailvars t
    tailvars (n, NDef sc _ _)       = tailvars sc
    tailvars _                      = []


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
      | otherwise                   = case lookup (head ns) (names env) of
                                        Just (NMAlias m') -> m'
                                        Nothing | m `elem` imports env  -> m
                                        _ -> noModule m
instance Unalias QName where
    unalias env (QName m n)         = case findHMod m env of
                                        Just te -> case M.lookup n te of
                                                      Just (HNAlias qn) -> qn
                                                      Just _ -> GName m' n
                                                      _ -> noItem m n
                                        Nothing -> error ("#### unalias fails for " ++ prstr (QName m n))
      where m'                      = unalias env m
    unalias env (NoQ n)
      | inBuiltin env               = GName mBuiltin n
      | otherwise                   = case lookup n (names env) of
                                        Just (NAlias qn) -> qn
                                        _ -> case thismod env of Just m -> GName m n; _ -> NoQ n
    unalias env (GName m n)
--      | inBuiltin env, m==mBuiltin  = NoQ n
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
    unalias env (NTVar k c)         = NTVar k (unalias env c)
    unalias env (NAlias qn)         = NAlias (unalias env qn)
    unalias env (NMAlias m)         = NMAlias (unalias env m)
    unalias env (NModule te doc)    = NModule (unalias env te) doc
    unalias env NReserved           = NReserved

instance Unalias (Name,NameInfo) where
    unalias env (n,i)               = (n, unalias env i)

instance Unalias WTCon where
    unalias env (w,u)               = (unalias env w, unalias env u)

instance Unalias (Either QName QName) where
    unalias env (Left n)            = Left $ unalias env n
    unalias env (Right n)           = Right $ unalias env n


-- TEnv filters --------------------------------------------------------------------------------------------------------

nSigs                       :: TEnv -> TEnv
nSigs te                    = [ (n,i) | (n, i@(NSig sc dec _)) <- te, not $ isProp dec sc ]

propSigs                    :: TEnv -> TEnv
propSigs te                 = [ (n,i) | (n, i@(NSig sc dec _)) <- te, isProp dec sc ]

isProp                      :: Deco -> TSchema -> Bool
isProp Property _           = True
isProp NoDec sc             = case sctype sc of TFun{} -> False; _ -> True
isProp _ _                  = False

nTerms                      :: TEnv -> TEnv
nTerms te                   = [ (n,i) | (n,i) <- te, isNTerm i ]

isNTerm NDef{}              = True
isNTerm NVar{}              = True
isNTerm _                   = False

sigTerms                    :: TEnv -> (TEnv, TEnv)
sigTerms te                 = (nSigs te, nTerms te)

noDefs                      :: TEnv -> TEnv
noDefs te                   = [ (n,i) | (n,i) <- te, keep i ]
  where keep NDef{}         = False
        keep NAct{}         = False
        keep _              = True

normTEnv                    :: TEnv -> TEnv
normTEnv te                 = f [] te
  where
    f ns []                 = []
    f ns ((n,i):te)
      | n `elem` ns         = f ns te
      | otherwise           = (n,i) : f (n:ns) te

unSig                       :: TEnv -> TEnv
unSig te                    = map f te
  where f (n, NSig (TSchema _ [] t) Property _) = (n, NVar t)
        f (n, NSig sc@(TSchema _ _ TFun{}) dec doc)
                                                = (n, NDef sc dec doc)
        f (n, NSig (TSchema _ _ t) _ _)         = (n, NVar t)
        f (n, i)                                = (n, i)


-- Env construction and modification -------------------------------------------------------------------------------------------


-- first variant is special case for compiling __builtin__.act
initEnv                    :: FilePath -> Bool -> IO Env0
initEnv path True          = return $ EnvF{ names = [(nPrim,NMAlias mPrim)],
                                            imports = [mPrim],
                                            modules = [(nPrim,NModule primEnv Nothing)],
                                            hmodules = M.empty, 
                                            witnesses = primWits,
                                            thismod = Nothing,
                                            context = [],
                                            envX = () }
initEnv path False         = do (_,nmod,_,_,_,_,_,_) <- InterfaceFiles.readFile (joinPath [path,"__builtin__.ty"])
                                let NModule envBuiltin builtinDocstring = nmod
                                    env0 = EnvF{ names = [(nPrim,NMAlias mPrim), (nBuiltin,NMAlias mBuiltin)],
                                                 imports = [mPrim,mBuiltin],
                                                 modules = [(nPrim,NModule primEnv Nothing), (nBuiltin,NModule envBuiltin builtinDocstring)],
                                                 hmodules = M.empty,
                                                 witnesses = primWits,
                                                 thismod = Nothing,
                                                 context = [],
                                                 envX = () }
                                    env = importAll mBuiltin envBuiltin $ importWits mBuiltin envBuiltin $ env0
                                return env

withModulesFrom             :: EnvF x -> EnvF x -> EnvF x
env `withModulesFrom` env'  = env{modules = modules env'}

addWit                      :: EnvF x -> Witness -> EnvF x
addWit env wit
  | null same               = env{ witnesses = wit : witnesses env }
  | otherwise               = env
  where same                = [ w | w <- witsByPName env (tcname $ proto wit), wtype w == wtype wit ]

reserve                     :: [Name] -> EnvF x -> EnvF x
reserve xs env              = env{ names = [ (x, NReserved) | x <- nub xs ] ++ names env }
--reserve xs env
--  | not $ null badSelf      = selfParamError (loc $ head badSelf)
--  | otherwise               = env{ names = [ (x, NReserved) | x <- nub xs ] ++ names env }
--  where badSelf             = if inAct env then xs `intersect` [selfKW] else []

define                      :: TEnv -> EnvF x -> EnvF x
define te env               = foldl addWit env1 ws
--define te env
--  | not $ null badSelf      = selfParamError (loc $ head badSelf)
--  | otherwise               = foldl addWit env1 ws
  where env1                = env{ names = reverse te ++ names env }
        ws                  = [ WClass q (tCon c) p (NoQ w) ws (length opts) | (w, NExt q c ps te' opts _) <- te, (ws,p) <- ps ]
--        badSelf             = if inAct env then dom te `intersect` [selfKW] else []


addImport                   :: ModName -> EnvF x -> EnvF x
addImport m env             = env{ imports = m : imports env }

defineTVars                 :: QBinds -> EnvF x -> EnvF x
defineTVars q env           = foldr f env q
  where f (Quant tv us) env = foldl addWit env{ names = (tvname tv, NTVar (tvkind tv) c) : names env } wits
          where (c,ps)      = case mro2 env us of ([],_) -> (cValue, us); _ -> (head us, tail us)   -- Just check that the mro exists, don't store it
                wits        = [ WInst [] (tVar tv) p (NoQ $ tvarWit tv p0) wchain | p0 <- ps, (wchain,p) <- findAncestry env p0 ]

defineSelfOpaque            :: EnvF x -> EnvF x
defineSelfOpaque env        = defineTVars [Quant tvSelf []] env

defineSelf                  :: QName -> QBinds -> EnvF x -> EnvF x
defineSelf qn q env         = defineTVars [Quant tvSelf [tc]] env
  where tc                  = TC (unalias env qn) [ tVar tv | Quant tv _ <- q ]

defineInst                  :: TCon -> [WTCon] -> Name -> EnvF x -> EnvF x
defineInst c ps w env       = foldl addWit env wits
  where wits                = [ WInst [] (tCon c) p (NoQ w) ws | (ws,p) <- ps ]

setMod                      :: ModName -> EnvF x -> EnvF x
setMod m env                = env{ thismod = Just m }

addMod                     :: ModName -> TEnv -> Maybe String -> EnvF x -> EnvF x
addMod m newte mdoc env     = env{ modules = addM ns (modules env) }
  where
    ModName ns              = m
    addM [] te              = newte ++ te
    addM (n:ns) te          = update n ns te
    update n ns ((x,i):te)
      | n == x,
        NModule te1 doc <- i    = (n, NModule (addM ns te1) doc) : te
    update n ns (ni:te)     = ni : update n ns te
    update n ns []          = (n, NModule (addM ns []) mdoc) : []


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
quantScope env              = [ Quant (TV k n) (if c==cValue then [] else [c]) | (n, NTVar k c) <- names env, n /= nSelf ]

selfSubst                   :: EnvF x -> Substitution
selfSubst env               = [ (TV k n, tCon c) | (n, NTVar k c) <- names env, n == nSelf ]


-- Name queries -------------------------------------------------------------------------------------------------------------------

findQName                   :: QName -> EnvF x -> NameInfo
findQName n env             = case tryQName n env of
                                 Just i -> convHNameInfo2NameInfo i
                                 Nothing -> nameNotFound (noq n)

tryQName                    :: QName -> EnvF x -> Maybe HNameInfo
tryQName (QName m n) env    = case findHMod m env of
                                Just te -> case M.lookup n te of
                                    Just (HNAlias qn) -> tryQName qn env
                                    Just i -> Just i
                                    _ -> noItem m n
                                _ -> noModule m
tryQName (NoQ n) env        = case lookup n (names env) of
                                Just (NAlias qn) -> tryQName qn env
                                Just ni -> Just (convNameInfo2HNameInfo ni)
                                Nothing -> Nothing
tryQName (GName m n) env
  | Just m == thismod env   = tryQName (NoQ n) env
  | inBuiltin env,
    m==mBuiltin             = tryQName (NoQ n) env
  | otherwise               = case lookupHMod m env of
                                Just te -> case M.lookup n te of
                                    Just i -> Just i
                                    Nothing -> noItem m n -- error ("## Failed lookup of " ++ prstr n ++ " in module " ++ prstr m)
                                Nothing -> noModule m -- error ("## Failed lookup of module " ++ prstr m)

findSigLoc n env            = findSL (names env)
    where findSL ((n',t):ps)
             | NSig{} <- t, n==n' = Just (loc n')
             | otherwise          = findSL ps
          findSL []               = Nothing

findDefLoc n env            = findSL (names env)
    where findSL ((n',t):ps)
             | NDef{} <- t, n==n' = Just (loc n')
             | otherwise          = findSL ps
          findSL []               = Nothing

findName n env              = findQName (NoQ n) env

lookupVar n env             = case lookup n (names env) of
                                Just (NVar t) -> Just t
                                _ -> Nothing

findHMod                    :: ModName -> EnvF x -> Maybe HTEnv  -- m is modname part of a QName, so we must check for aliasing
findHMod m env | inBuiltin env, m==mBuiltin
                            = Just (convTEnv2HTEnv (names env))
findHMod m@(ModName ns) env = case lookup (head ns) (names env) of
                                Just (NMAlias m') -> lookupHMod m' env
                                Nothing | m `elem` imports env  -> lookupHMod m env
                                _ -> Nothing

lookupHMod                  :: ModName -> EnvF x -> Maybe HTEnv -- m is modname part of a GName, so search directly for module
lookupHMod m env | inBuiltin env, m==mBuiltin
                            = Just (convTEnv2HTEnv (names env))
lookupHMod (ModName ns) env = f ns (hmodules env)
  where f [] te
          | not (all isHNModule (M.toList te)) = Just te
          | otherwise              = Nothing
        f (n:ns) te         = case M.lookup n te of
                                Just (HNModule te' _) -> f ns te'
                                Just (HNMAlias (ModName m)) -> lookupHMod (ModName $ m++ns) env
                                _ -> Nothing
        isHNModule (_, HNModule{}) = True
        isHNModule _               = False

lookupMod                  :: ModName -> EnvF x -> Maybe TEnv 
lookupMod m env | inBuiltin env, m==mBuiltin
                            = Just (names env)
lookupMod (ModName ns) env  = f ns (modules env)
  where f [] te
          | not (all isNModule te) = Just te
          | otherwise              = Nothing
        f (n:ns) te         = case lookup n te of
                                Just (NModule te' _) -> f ns te'
                                Just (NMAlias (ModName m)) -> lookupMod (ModName $ m++ns) env
                                _ -> Nothing
        isNModule (_, NModule{})  = True
        isNModule _               = False


isMod                       :: EnvF x -> [Name] -> Bool
isMod env ns                = maybe False (const True) (findHMod (ModName ns) env)

isAlias                     :: Name -> EnvF x -> Bool
isAlias n env               = case lookup n (names env) of
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
        kind k q            = KFun [ tvkind v | Quant v _ <- q ] k

actorSelf env               = case lookup selfKW (names env) of
                                Just (NVar (TCon _ tc)) | isActor env (tcname tc) -> True
                                _ -> False

actorMethod env n0          = walk [] (names env)
  where
    walk ns ((n,NDef{}):te) = walk (n:ns) te
    walk ns ((n,NVar (TCon _ tc)):te)
      | n == selfKW         = isActor env (tcname tc) && n0 `elem` ns
    walk ns (_ : te)        = walk ns te
    walk ns []              = False

isDef                       :: EnvF x -> QName -> Bool
isDef env n                 = case tryQName n env of
                                Just HNDef{} -> True
                                _ -> False

isActor                     :: EnvF x -> QName -> Bool
isActor env n               = case tryQName n env of
                                Just HNAct{} -> True
                                _ -> False

isClass                     :: EnvF x -> QName -> Bool
isClass env n               = case tryQName n env of
                                Just HNClass{} -> True
                                _ -> False

isProto                     :: EnvF x -> QName -> Bool
isProto env n               = case tryQName n env of
                                Just HNProto{} -> True
                                _ -> False

isDefOrClass                :: EnvF x -> QName -> Bool
isDefOrClass env n          = case tryQName n env of
                                Just HNDef{} -> True
                                Just HNClass{} -> True
                                _ -> False

witsByPName                 :: EnvF x -> QName -> [Witness]
witsByPName env pn          = [ w | w <- witnesses env, tcname (proto w) == pn ]

witsByPNameAndType          :: EnvF x -> QName -> Type -> [Witness]
witsByPNameAndType env pn t = [ w | w <- witsByPName env pn, wtype w == t ]

witsByTName                 :: EnvF x -> QName -> [Witness]
witsByTName env tn          = [ w | w <- witnesses env, eqname (wtype w) ]
  where eqname (TCon _ c)   = tcname c == tn
        eqname (TVar _ v)   = NoQ (tvname v) == tn
        eqname _            = False


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
  where (q,us,te)           = findConName qn env

allAncestors                :: EnvF x -> TCon -> [TCon]
allAncestors env tc         = [ schematic' c | (_, c) <- us ]
  where (us,te)             = findCon env tc

allAncestors'               :: EnvF x -> QName -> [QName]
allAncestors' env qn        = map (tcname . snd) us
  where (q,us,te)           = findConName qn env

allDescendants              :: EnvF x -> TCon -> [TCon]
allDescendants env tc       = [ schematic' c | c <- allCons env, hasAncestor' env (tcname c) (tcname tc) ]

findCon                     :: EnvF x -> TCon -> ([WTCon],TEnv)
findCon env (TC n ts)
  | map tVar tvs == ts      = (us, te)
  | otherwise               = (vsubst s us, vsubst s te)
  where (q,us,te)           = findConName n env
        tvs                 = qbound q
        s                   = tvs `zip` ts

findConName n env           = case findQName n env of
                                NAct q p k te _  -> (q,[],te)
                                NClass q us te _ -> (q,us,te)
                                NProto q us te _ -> (q,us,te)
                                NExt q c us te _ _ -> (q,us,te)
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
findAttr env tc n           = listToMaybe $ attributes f env tc
  where f wp i x | x /= n   = Nothing
        f wp (NSig sc d _) x  = Just (wexpr wp, sc, Just d)
        f wp (NDef sc d _) x  = Just (wexpr wp, sc, Just d)
        f wp (NVar t)    x  = Just (wexpr wp, monotype t, Nothing)
        f wp (NSVar t)   x  = Just (wexpr wp, monotype t, Nothing)

attributes'                 :: (WPath -> NameInfo -> Name -> Maybe a) -> EnvF x -> QName -> [a]
attributes' f env qn        = catMaybes [ f wp i n | n <- ns, let Just (wp,i) = lookup n aenv ]
  where ns                  = nub $ reverse $ dom aenv                                                                                  -- in offset order
        aenv                = [ (n,(wp,i)) | (wp,c) <- ([],tc) : us, let (_,_,te) = findConName (tcname c) env, (n,i) <- reverse te ]   -- in override order
        (q,us,_)            = findConName qn env
        tc                  = TC qn [ tVar v | Quant v _ <- q ]

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
closedAttr env tc n         = n `elem` closedAttrs env (tcname tc)

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
abstractAttr env tc n       = n `elem` abstractAttrs env (tcname tc)


allCons                     :: EnvF x -> [CCon]
allCons env                 = reverse locals ++ concat [ cons m (lookupMod m env) | m <- moduleRefs env, m /= mPrim ]
  where locals
          | inBuiltin env   = cons mBuiltin (Just $ names env)
          | otherwise       = [ TC (NoQ n) (wildargs i) | (n,i) <- names env, con i ]
        con NClass{}        = True
        con NAct{}          = True
        con _               = False
        cons m (Just te)    = [ TC (GName m n) (wildargs i) | (n,i) <- te, con i ] ++ concat [ cons (modCat m n) (Just te') | (n,NModule te' _) <- te ]

allProtos                   :: EnvF x -> [PCon]
allProtos env               = reverse locals ++ concat [ protos m (lookupMod m env) | m <- moduleRefs env, m /= mPrim ]
  where locals
          | inBuiltin env   = protos mBuiltin (Just $ names env)
          | otherwise       = [ TC (NoQ n) (wildargs i) | (n,i) <- names env, proto i ]
        proto NProto{}      = True
        proto _             = False
        protos m (Just te)  = [ TC (GName m n) (wildargs i) | (n,i) <- te, proto i ] ++ concat [ protos (modCat m n) (Just te') | (n,NModule te' _) <- te ]

allConAttr                  :: EnvF x -> Name -> [Type]
allConAttr env n            = [ tCon tc | tc <- allCons env, n `elem` allAttrs' env tc ]

allConAttrUFree             :: EnvF x -> Name -> [TUni]
allConAttrUFree env n       = concat [ ufree $ fst $ findAttr' env tc n | tc <- allCons env, n `elem` allAttrs' env tc ]

allProtoAttr                :: EnvF x -> Name -> [Type]
allProtoAttr env n          = [ tCon p | p <- allProtos env, n `elem` allAttrs' env p ]


wexpr                       :: WPath -> Expr -> Expr
wexpr [] e                  = e
wexpr (Left _ : w) e        = wexpr w e
wexpr (Right n : w) e       = wexpr w $ eDot e (witAttr n)


-- TVar queries ------------------------------------------------------------------------------------------------------------------

findSelf                    :: EnvF x -> TCon
findSelf env                = case findName (tvname tvSelf) env of
                                NTVar _ c -> c

findTVBound                 :: EnvF x -> TVar -> CCon
findTVBound env tv          = case findName (tvname tv) env of
                                NTVar _ c -> c
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

----------------------------------------------------------------------------------------------------------------------
-- GLB
----------------------------------------------------------------------------------------------------------------------

glb env (TWild _) t2                    = pure t2
glb env t1 (TWild _)                    = pure t1

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


glbfold env []                          = pure tWild
glbfold env (t:ts)                      = foldM (glb env) t ts


----------------------------------------------------------------------------------------------------------------------
-- LUB
----------------------------------------------------------------------------------------------------------------------

lub env (TWild _) t2                    = pure t2
lub env t1 (TWild _)                    = pure t1

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


lubfold env []                          = pure tWild
lubfold env (t:ts)                      = foldM (lub env) t ts


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


-- Import handling (local definitions only) ----------------------------------------------

--getImps                         :: [FilePath] -> EnvF x -> [Import] -> IO (EnvF x)
getImps spath env []         = return env { hmodules = convTEnv2HTEnv (modules env) }
getImps spath env (i:is)     = do env' <- impModule spath env i
                                  getImps spath env' is


--impModule                       :: [FilePath] -> EnvF x -> Import -> IO (EnvF x)
impModule spath env (Import _ ms)
                                = imp env ms
  where imp env []              = return env
        imp env (ModuleItem m as : is)
                                = do (env1,te) <- doImp spath env m
                                     let env2 = maybe (addImport m) (\n->define [(n, NMAlias m)]) as env1
                                     imp (importWits m te env2) is
impModule spath env (FromImport _ (ModRef (0,Just m)) items)
                                = do (env1,te) <- doImp spath env m
                                     return $ importSome items m te $ importWits m te $ env1
impModule spath env (FromImportAll _ (ModRef (0,Just m)))
                                = do (env1,te) <- doImp spath env m
                                     return $ importAll m te $ importWits m te $ env1
impModule _ _ i                 = illegalImport (loc i)


moduleRefs env                  = nub $ imports env ++ [ m | (_,NMAlias m) <- names env ] ++ [ m | (_,NAlias (GName m _)) <- names env ]

moduleRefs1 env                 = moduleRefs env \\ [mPrim, mBuiltin]

subImp spath env []          = return env
subImp spath env (m:ms)      = do (env',_) <- doImp spath env m
                                  subImp spath env' ms

findTyFile spaths mn = go spaths
  where
    go []     = return Nothing
    go (p:ps) = do
      let fullPath = joinPath (p : modPath mn) ++ ".ty"
      exists <- doesFileExist fullPath
      --traceM ("findTyFile: " ++ fullPath ++ " " ++ show exists)
      if exists
        then return (Just fullPath)
        else go ps

doImp                        :: [FilePath] -> EnvF x -> ModName -> IO (EnvF x, TEnv)
doImp spath env m            = case lookupMod m env of
                                    Just te -> return (env, te)
                                    Nothing -> do
                                        tyFile <- findTyFile spath m
                                        case tyFile of
                                          Nothing -> fileNotFound m
                                          Just tyF -> do
                                            (ms,nmod,_,_,_,_,_,_) <- InterfaceFiles.readFile tyF
                                            env' <- subImp spath env ms
                                            let NModule te mdoc = nmod
                                            return (addMod m te mdoc env', te)

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
    imp (n, NAct _ _ _ _ _)   = Just (n, NAlias (GName m n))
    imp (n, NClass _ _ _ _)   = Just (n, NAlias (GName m n))
    imp (n, NProto _ _ _ _)   = Just (n, NAlias (GName m n))
    imp (n, NExt _ _ _ _ _ _) = Nothing
    imp (n, NAlias _)       = Just (n, NAlias (GName m n))
    imp (n, NVar t)         = Just (n, NAlias (GName m n))
    imp (n, NDef t d _)       = Just (n, NAlias (GName m n))
    imp _                   = Nothing                               -- cannot happen

importWits                  :: ModName -> TEnv -> EnvF x -> EnvF x
importWits m te env         = foldl addWit env ws
  where ws                  = [ WClass q (tCon c) p (GName m n) ws (length opts) | (n, NExt q c ps te' opts _) <- te, (ws,p) <- ps ]



headvar (Impl _ w (TUni _ u) p)     = u

headvar (Cast _ TVar{} (TUni _ u))  = u
headvar (Cast _ (TUni _ u) t)       = u
headvar (Cast _ t (TUni _ u))       = u     -- ?

headvar (Sub _ w TVar{} (TUni _ u)) = u
headvar (Sub _ w (TUni _ u) t)      = u
headvar (Sub _ w t (TUni _ u))      = u     -- ?

headvar (Sel _ w (TUni _ u) n t)    = u

headvar (Mut _ (TUni _ u) n t)      = u

headvar (Seal _ (TUni _ u))         = u


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
err l s                             = Control.Exception.throw $ OtherError l s

err0 xs s                           = err (loc $ head xs) s
err1 x s                            = err (loc x) (s ++ " " ++ prstr x)
err2 xs s                           = err (loc $ head xs) (s ++ " " ++ prstrs xs)
err3 l xs s                         = err l (s ++ " " ++ prstrs xs)

notYetExpr e                        = notYet (loc e) e

stripQual q                             = [ Quant v [] | Quant v us <- q ]


class Simp a where
    simp                            :: EnvF x -> a -> a

instance (Simp a) => Simp [a] where
    simp env                        = map (simp env)

instance Simp TSchema where
    simp env (TSchema l q t)        = TSchema l q' (vsubst s $ simp env' t)
      where (q', s)                 = simpQuant env (simp env' q) (vfree t)
            env'                    = defineTVars (stripQual q) env

simpQuant env q vs0                 = (vsubst s [ Quant v ps | Quant v ps <- q2, not $ null ps ], s)
  where (q1,q2)                     = partition isEX q
        isEX (Quant v [p])          = length (filter (==v) vs) == 1
        isEX _                      = False
        vs                          = concat [ vfree ps | Quant v ps <- q ] ++ vs0
        s                           = [ (v, tCon p) | Quant v [p] <- q1 ]                       -- Inline existentials

instance Simp QBind where
    simp env (Quant v ps)           = Quant v (simp env ps)

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
    simp env (n, NAct q p k te doc) = (n, NAct (simp env' q) (simp env' p) (simp env' k) (simp env' te) doc)
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
      | not $ null aliases          = NoQ $ head aliases                                        -- Restore aliases
      | otherwise                   = n
      where aliases                 = [ n1 | (n1, NAlias n2) <- names env, n2 == n ]
