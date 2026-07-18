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

{-# LANGUAGE FlexibleInstances, FlexibleContexts, DeriveGeneric, DeriveAnyClass #-}
module Acton.NameInfo where

import Control.DeepSeq
import qualified Data.Binary
import Data.Persist (Persist)
import GHC.Generics (Generic)
import Data.Typeable
import qualified Data.HashMap.Strict as M
import Prelude hiding ((<>))

import Utils
import Acton.Syntax
import Acton.Builtin
import Acton.Names
import Acton.Subst
import Acton.Printer


-- NameInfo -----------------------------------------------------------------------------------------------

{-  TEnv principles:
    -   A TEnv is an association of NameInfo details to a list of names.
    -   NSig holds the schema of an explicit Signature, while NDef and NVar give schemas and types to names created by Defs and assignments.
    -   NClass, NProto, NExt and NAct represent class, protocol, extension and actor declarations. They each contain a TEnv of visible local attributes.
    -   Signatures must appear before the defs/assignments they describe, and every TEnv respects the order of the syntactic constructs binding each name.
    -   The attribute TEnvs of NClass, NProto, NExt and NAct are searched left-to-right, thus favoring (explicit) NSigs over (inferred) NDefs/NVars.
    -   The global inference environment is searched from active names to closed names, thereby prioritizing NDefs/NVars over NSigs, as well as any inner bindings in scope.
    -   The NameInfo assumption on a (recursive) Def is always an NDef, initialized to the corresponding NSig if present, or a fresh unquantified variable.
    -   The inferred schema for each def is checked to be no less general than the corresponding NDef assumption.
    -   Unquantified NDefs are generalized at the close of the outermost recursive declaration in scope.
    -   An NSig is always fully quantified, not possible to generalize
    -   To enable method override (and disable method signature override), the NSigs of parent class are inserted into the global env when checking a child class
    -   For the same reason, NDefs and NVars without an NSig of a parent class are inserted as NSigs when a child class is checked
-}

type TEnv               = [(Name, NameInfo)]

data NameInfo           = NVar      Type
                        | NSVar     Type
                        | NDef      TSchema Deco (Maybe String)
                        | NSig      TSchema Deco (Maybe String)
                        | NAct      QBinds PosRow KwdRow TEnv (Maybe String)
                        | NClass    QBinds [WTCon] TEnv (Maybe String)
                        | NProto    QBinds [WTCon] TEnv (Maybe String)
                        | NType     QBinds Type (Maybe String)
                        | NExt      QBinds TCon [WTCon] TEnv [Name] (Maybe String)
                        | NTVar     Kind CCon [PCon]
                        | NAlias    QName
                        | NReserved
                        deriving (Eq,Show,Read,Generic,NFData)

data NModule            = NModule [ModName] TEnv (Maybe String)
                        deriving (Eq,Show,Read,Generic,NFData)

typeDecl (_,NDef{})     = False
typeDecl _              = True

type HTEnv            =  M.HashMap Name NameInfo

instance Data.Binary.Binary NameInfo
instance Persist NameInfo

-- | Strip all docstrings from NameInfo (and nested environments).
-- This is used when computing a public-interface hash so that
-- documentation-only edits do not cause dependents to rebuild.
stripDocsNI :: NameInfo -> NameInfo
stripDocsNI ni = case ni of
  NAct q p k te _     -> NAct q p k (map stripBind te) Nothing
  NClass q cs te _    -> NClass q cs (map stripBind te) Nothing
  NProto q ps te _    -> NProto q ps (map stripBind te) Nothing
  NType q t _         -> NType q t Nothing
  NExt q c ps te o _  -> NExt q c ps (map stripBind te) o Nothing
  NDef sc dec _       -> NDef sc dec Nothing
  NSig sc dec _       -> NSig sc dec Nothing
  other               -> other
  where
    stripBind (n, info) = (n, stripDocsNI info)

stripDocsNModule :: NModule -> NModule
stripDocsNModule (NModule ms te _) = NModule ms (map stripBind te) Nothing
  where
    stripBind (n, info) = (n, stripDocsNI info)

-- | Strip source locations from NameInfo (and nested syntax fragments).
-- This keeps public interface hashes independent from whitespace-only source
-- edits while preserving locations in the cached .tydb payload itself.
stripLocsNI :: NameInfo -> NameInfo
stripLocsNI ni = case ni of
  NVar t             -> NVar (stripLocsType t)
  NSVar t            -> NSVar (stripLocsType t)
  NDef sc dec doc    -> NDef (stripLocsTSchema sc) dec doc
  NSig sc dec doc    -> NSig (stripLocsTSchema sc) dec doc
  NAct q p k te doc  -> NAct (stripLocsQBinds q) (stripLocsType p) (stripLocsType k) (stripLocsTEnv te) doc
  NClass q cs te doc -> NClass (stripLocsQBinds q) (map stripLocsWTCon cs) (stripLocsTEnv te) doc
  NProto q ps te doc -> NProto (stripLocsQBinds q) (map stripLocsWTCon ps) (stripLocsTEnv te) doc
  NType q t doc      -> NType (stripLocsQBinds q) (stripLocsType t) doc
  NExt q c ps te o doc ->
    NExt (stripLocsQBinds q) (stripLocsTCon c) (map stripLocsWTCon ps) (stripLocsTEnv te) (map stripLocsName o) doc
  NTVar k c ps       -> NTVar k (stripLocsTCon c) (map stripLocsTCon ps)
  NAlias qn          -> NAlias (stripLocsQName qn)
  NReserved          -> NReserved
  where
    stripLocsTEnv :: TEnv -> TEnv
    stripLocsTEnv = map $ \(n, info) -> (stripLocsName n, stripLocsNI info)

    stripLocsTSchema :: TSchema -> TSchema
    stripLocsTSchema (TSchema _ q t) = TSchema NoLoc (stripLocsQBinds q) (stripLocsType t)

    stripLocsQBinds :: QBinds -> QBinds
    stripLocsQBinds = map stripLocsQBind

    stripLocsQBind :: QBind -> QBind
    stripLocsQBind (QBind tv cs) = QBind (stripLocsTVar tv) (map stripLocsTCon cs)

    stripLocsTVar :: TVar -> TVar
    stripLocsTVar (TV k n) = TV k (stripLocsName n)

    stripLocsTCon :: TCon -> TCon
    stripLocsTCon (TC qn ts) = TC (stripLocsQName qn) (map stripLocsType ts)

    stripLocsWTCon :: WTCon -> WTCon
    stripLocsWTCon (wpath, pcon) = (map stripLocsWStep wpath, stripLocsTCon pcon)

    stripLocsWStep :: Either QName QName -> Either QName QName
    stripLocsWStep (Left qn) = Left (stripLocsQName qn)
    stripLocsWStep (Right qn) = Right (stripLocsQName qn)

    stripLocsType :: Type -> Type
    stripLocsType t = case t of
      TUni _ u          -> TUni NoLoc u
      TVar _ tv         -> TVar NoLoc (stripLocsTVar tv)
      TCon _ tc         -> TCon NoLoc (stripLocsTCon tc)
      TFun _ fx p k r   -> TFun NoLoc (stripLocsType fx) (stripLocsType p) (stripLocsType k) (stripLocsType r)
      TTuple _ p k      -> TTuple NoLoc (stripLocsType p) (stripLocsType k)
      TOpt _ opt        -> TOpt NoLoc (stripLocsType opt)
      TNone _           -> TNone NoLoc
      TWild _           -> TWild NoLoc
      TNil _ k          -> TNil NoLoc k
      TRow _ k n ty row -> TRow NoLoc k (stripLocsName n) (stripLocsType ty) (stripLocsType row)
      TStar _ k row     -> TStar NoLoc k (stripLocsType row)
      TFX _ fx          -> TFX NoLoc fx

    stripLocsQName :: QName -> QName
    stripLocsQName qn = case qn of
      QName mn n -> QName (stripLocsModName mn) (stripLocsName n)
      NoQ n      -> NoQ (stripLocsName n)
      GName mn n -> GName (stripLocsModName mn) (stripLocsName n)

    stripLocsModName :: ModName -> ModName
    stripLocsModName (ModName ns) = ModName (map stripLocsName ns)

    stripLocsName :: Name -> Name
    stripLocsName n = case n of
      Name _ s       -> Name NoLoc s
      Derived n1 n2  -> Derived (stripLocsName n1) (stripLocsName n2)
      Internal{}     -> n

stripLocsNModule :: NModule -> NModule
stripLocsNModule (NModule ms te doc) = NModule ms (stripLocsTEnv te) doc
  where
    stripLocsTEnv :: TEnv -> TEnv
    stripLocsTEnv = map $ \(n, info) -> (stripLocsName n, stripLocsNI info)

    stripLocsName :: Name -> Name
    stripLocsName n = case n of
      Name _ s       -> Name NoLoc s
      Derived n1 n2  -> Derived (stripLocsName n1) (stripLocsName n2)
      Internal{}     -> n

instance Pretty TEnv where
    pretty tenv               = vcat (map pretty $ normTEnv tenv)
      where normTEnv te       = f [] te
              where
                  f ns []          = []
                  f ns ((n,i):te)
                    | n `elem` ns  = f ns te
                    | otherwise    = (n,i) : f (n:ns) te


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
    pretty (n, NType q t doc)   = text "type" <+> pretty n <> nonEmpty brackets commaList q <+> equals <+> pretty t $+$ nest 4 (prettyDocstring doc)
    pretty (w, NExt [] c ps te opts doc)
                                = {-pretty w  <+> colon <+> -}
                                  text "extension" <+> pretty c <+> parens (commaList ps) <>
                                  colon $+$ nest 4 (prettyDocstring doc) $+$ (nest 4 $ prettyOrPass te)
    pretty (w, NExt q c ps te opts doc)
                                = {-pretty w  <+> colon <+> -}
                                  text "extension" <+> pretty q <+> text "=>" <+> pretty c <+> parens (commaList ps) <>
                                  colon $+$ nest 4 (prettyDocstring doc) $+$ (nest 4 $ prettyOrPass te)
    pretty (n, NTVar k c ps)    = pretty n <> parens (commaList (c:ps))
    pretty (n, NAlias qn)       = text "alias" <+> pretty n <+> equals <+> pretty qn
    pretty (n, NReserved)       = pretty n <+> text "(reserved)"

prettyOrPass te
  | isEmpty doc                 = text "pass"
  | otherwise                   = doc
  where doc                     = pretty te

prettyDocstring :: Maybe String -> Doc
prettyDocstring Nothing         = empty
prettyDocstring (Just docstring) = text "\"\"\"" <> text docstring <> text "\"\"\""

instance VFree NameInfo where
    vfree (NVar t)              = vfree t
    vfree (NSVar t)             = vfree t
    vfree (NDef t d _)          = vfree t
    vfree (NSig t d _)          = vfree t
    vfree (NAct q p k te _)     = (vfree q ++ vfree p ++ vfree k ++ vfree te) \\ (tvSelf : qbound q)
    vfree (NClass q us te _)    = (vfree q ++ vfree us ++ vfree te) \\ (tvSelf : qbound q)
    vfree (NProto q us te _)    = (vfree q ++ vfree us ++ vfree te) \\ (tvSelf : qbound q)
    vfree (NType q t _)         = (vfree q ++ vfree t) \\ qbound q
    vfree (NExt q c ps te _ _)  = (vfree q ++ vfree c ++ vfree ps ++ vfree te) \\ (tvSelf : qbound q)
    vfree (NTVar k c ps)        = vfree c ++ vfree ps
    vfree (NAlias qn)           = []
    vfree NReserved             = []

instance VSubst NameInfo where
    vsubst s (NVar t)           = NVar (vsubst s t)
    vsubst s (NSVar t)          = NSVar (vsubst s t)
    vsubst s (NDef t d x)       = NDef (vsubst s t) d x
    vsubst s (NSig t d x)       = NSig (vsubst s t) d x
    vsubst s (NAct q p k te x)  = NAct (vsubst s q) (vsubst s p) (vsubst s k) (vsubst s te) x
    vsubst s (NClass q us te x) = NClass (vsubst s q) (vsubst s us) (vsubst s te) x
    vsubst s (NProto q us te x) = NProto (vsubst s q) (vsubst s us) (vsubst s te) x
    vsubst s (NType q t x)      = NType (vsubst s q) (vsubst s t) x
    vsubst s (NExt q c ps te opts x) = NExt (vsubst s q) (vsubst s c) (vsubst s ps) (vsubst s te) opts x
    vsubst s (NTVar k c ps)        = NTVar k (vsubst s c) (vsubst s ps)
    vsubst s (NAlias qn)        = NAlias qn
    vsubst s NReserved          = NReserved

instance UFree NameInfo where
    ufree (NVar t)              = ufree t
    ufree (NSVar t)             = ufree t
    ufree (NDef t d _)          = ufree t
    ufree (NSig t d _)          = ufree t
    ufree (NAct q p k te _)     = ufree q ++ ufree p ++ ufree k ++ ufree te
    ufree (NClass q us te _)    = ufree q ++ ufree us ++ ufree te
    ufree (NProto q us te _)    = ufree q ++ ufree us ++ ufree te
    ufree (NType q t _)         = ufree q ++ ufree t
    ufree (NExt q c ps te _ _)  = ufree q ++ ufree c ++ ufree ps ++ ufree te
    ufree (NTVar k c ps)        = ufree c ++ ufree ps
    ufree (NAlias qn)           = []
    ufree NReserved             = []

instance Polarity (Name,NameInfo) where
    polvars (n, i)              = polvars i

instance Polarity NameInfo where
    polvars (NVar t)            = polvars t
    polvars (NSVar t)           = invvars t
    polvars (NDef t d _)        = polvars t
    polvars (NSig t d _)        = polvars t
    polvars (NAct q p k te _)   = polvars q `polcat` polneg (polvars p `polcat` polvars k) `polcat` polvars te
    polvars (NClass q us te _)  = polvars q `polcat` polvars us `polcat` polvars te
    polvars (NProto q us te _)  = polvars q `polcat` polvars us `polcat` polvars te
    polvars (NType q t _)       = polvars q `polcat` polvars t
    polvars (NExt q c ps te _ _) = polvars q `polcat` polvars c `polcat` polvars ps `polcat` polvars te
    polvars (NTVar k c ps)      = polvars c `polcat` polvars ps
    polvars _                   = ([],[])

instance Tailvars (Name, NameInfo) where
    tailvars (n, NVar t)        = tailvars t
    tailvars (n, NSVar t)       = tailvars t
    tailvars (n, NDef sc _ _)   = tailvars sc
    tailvars _                  = []

wildargs i                      = [ tWild | _ <- nbinds i ]
  where
    nbinds (NAct q _ _ _ _)     = q
    nbinds (NClass q _ _ _)     = q
    nbinds (NProto q _ _ _)     = q
    nbinds (NType q _ _)        = q
    nbinds (NExt q _ _ _ _ _)   = q

-- TEnv filters --------------------------------------------------------------------------------------------------------

notHidden                   :: TEnv -> TEnv
notHidden te                = [ (n,i) | (n,i) <- te, visible n i ]
  where visible _ NSVar{}   = False
        visible n _         = not $ isHidden n


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
isNTerm NSVar{}             = True
isNTerm _                   = False

sigTerms                    :: TEnv -> (TEnv, TEnv)
sigTerms te                 = (nSigs te, nTerms te)

noDefs                      :: TEnv -> TEnv
noDefs te                   = [ (n,i) | (n,i) <- te, keep i ]
  where keep NDef{}         = False
        keep NAct{}         = False
        keep _              = True

unSig                       :: TEnv -> TEnv
unSig te                    = map f te
  where f (n, NSig (TSchema _ [] t) Property _) = (n, NVar t)
        f (n, NSig sc@(TSchema _ _ TFun{}) dec doc)
                                                = (n, NDef sc dec doc)
        f (n, NSig (TSchema _ _ t) _ _)         = (n, NVar t)
        f (n, i)                                = (n, i)


-- Witnesses -----------------------------------------------------------------------------------------------

data Witness            = WClass    { binds::QBinds, wtype::Type, proto::PCon, wname::QName, wsteps::WPath, wopts::Int }
                        | WInst     { binds::QBinds, wtype::Type, proto::PCon, wname::QName, wsteps::WPath }
                        deriving (Show)

type WPath              = [Either QName QName]

type WTCon              = (WPath,PCon)

wexpr                   :: WPath -> Expr -> Expr
wexpr [] e              = e
wexpr (Left _ : w) e    = wexpr w e
wexpr (Right n : w) e   = wexpr w $ eDot e (witAttr n)


instance Pretty Witness where
    pretty (WClass q t p w ws _)
                        = text "WClass" <+> prettyQual q <+> pretty t <+> parens (pretty p) <+>
                          equals <+> pretty (wexpr ws (eCall (eQVar w) []))
    pretty (WInst q t p w ws)
                        = text "WInst" <+> prettyQual q <+> pretty t <+> parens (pretty p) <+>
                          equals <+> pretty (wexpr ws (eQVar w))

instance UFree Witness where
    ufree w@WClass{}    = []
    ufree w@WInst{}     = ufree (wtype w) ++ ufree (proto w)


instance VFree WTCon where
    vfree (wpath, p)    = vfree p

instance UFree WTCon where
    ufree (wpath, p)    = ufree p

instance VSubst WTCon where
    vsubst s (w,u)      = (w, vsubst s u)

instance Vars NameInfo where
    freeQ ni = case ni of
      NVar t -> freeQ t
      NSVar t -> freeQ t
      NDef sc _ _ -> freeQ sc
      NSig sc _ _ -> freeQ sc
      NAct q p k te _ -> freeQ q ++ freeQ p ++ freeQ k ++ freeQTEnv te
      NClass q ws te _ -> freeQ q ++ freeQWTCons ws ++ freeQTEnv te
      NProto q ws te _ -> freeQ q ++ freeQWTCons ws ++ freeQTEnv te
      NType q t _ -> freeQ q ++ freeQ t
      NExt q c ws te _ _ -> freeQ q ++ freeQ c ++ freeQWTCons ws ++ freeQTEnv te
      NTVar _ c ps -> freeQ c ++ freeQ ps
      NAlias qn -> freeQ qn
      NReserved -> []
      where
        freeQTEnv :: TEnv -> [QName]
        freeQTEnv tenv = concatMap (freeQ . snd) tenv

        freeQWTCons :: [WTCon] -> [QName]
        freeQWTCons = concatMap freeQWTCon

        freeQWTCon :: WTCon -> [QName]
        freeQWTCon (wpath, pcon) = freeQWPath wpath ++ freeQ pcon

        freeQWPath :: WPath -> [QName]
        freeQWPath = concatMap step
          where
            step (Left qn) = freeQ qn
            step (Right qn) = freeQ qn

instance Pretty WTCon where
    pretty (wpath, p)   = pretty p

instance Polarity WTCon where
    polvars (w, c)      = polvars c
