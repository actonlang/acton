-- Copyright (C) 2019-2025 Data Ductus AB
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

{-# LANGUAGE FlexibleInstances #-}
module Acton.DocPrinter
    ( -- * Main documentation functions
      printAsciiDoc
    , printMdDoc
    , printHtmlDoc
    , generateDocIndex
      -- * Utility functions
    , extractDocstring
    ) where

import Utils
import Acton.Printer (Pretty(..), Doc, render, text, (<+>), ($+$), (<>), blank, vcat, empty,
                      nest, brackets, parens, colon, comma, punctuate, commaSep, commaList, isEmpty, hcat)
import Acton.Syntax
import Acton.Builtin (qnList, qnDict, qnSetT, nBuiltin)
import Acton.Prim
import Acton.NameInfo
import Data.List (nub)
import Prelude hiding ((<>))
import Data.List (intercalate, intersperse, sortBy)
import Data.Ord (comparing)
import Data.Char (isDigit)
import Data.Set (Set)
import qualified Data.Set as Set
import System.FilePath ((</>), (<.>), joinPath)


-- | Generate documentation from a module in Markdown format with types
docModuleWithTypes :: NameInfo -> Module -> Doc
docModuleWithTypes (NModule tenv mdocstring) (Module qn _ stmts) =
    -- Use module docstring from NModule
    let moduleDocstring = mdocstring
        (title, restDoc) = case moduleDocstring of
            Just ds -> splitDocstring ds
            Nothing -> ("", Nothing)
        header = if null title
                 then text "#" <+> pretty qn
                 else text "#" <+> text "`" <> pretty qn <> text "`:" <+> text title
        bodyDoc = case restDoc of
            Just body -> blank $+$ text body $+$ blank
            Nothing -> empty
    in header $+$ bodyDoc $+$ blank $+$ docTopLevelWithTypes tenv stmts

-- | Split docstring into first line (title) and rest
splitDocstring :: String -> (String, Maybe String)
splitDocstring s =
    let ls = lines s
    in case ls of
        [] -> ("", Nothing)
        [l] -> (l, Nothing)
        (l:rest) ->
            let remainder = dropWhile null rest  -- Skip blank lines after title
            in if null remainder
               then (l, Nothing)
               else (l, Just $ unlines remainder)


-- | Extract and document top-level definitions with types
docTopLevelWithTypes :: TEnv -> Suite -> Doc
docTopLevelWithTypes tenv stmts =
    let docs = concatMap (extractTopLevelWithTypes tenv) stmts
        separated = case docs of
            [] -> []
            [d] -> [d]
            (d:ds) -> d : map (blank $+$ blank $+$) ds
    in vcat separated


-- | Extract documentation-worthy top-level statements with types
extractTopLevelWithTypes :: TEnv -> Stmt -> [Doc]
extractTopLevelWithTypes tenv (Decl _ decls) = map (docDeclWithTypes tenv) decls
extractTopLevelWithTypes tenv (With _ _ body) = concatMap (extractTopLevelWithTypes tenv) body
extractTopLevelWithTypes _ _ = []

-- | Extract docstring from a suite (if it's the first statement and is a string)
extractDocstring :: Suite -> Maybe String
extractDocstring [] = Nothing
extractDocstring (Expr _ (Strings _ ss) : _) = Just (unescapeString $ concat ss)
extractDocstring _ = Nothing


-- | Extract docstring from NameInfo
extractNameDocstring :: NameInfo -> Maybe String
extractNameDocstring (NDef _ _ mdoc) = mdoc
extractNameDocstring (NSig _ _ mdoc) = mdoc
extractNameDocstring (NAct _ _ _ _ mdoc) = mdoc
extractNameDocstring (NClass _ _ _ mdoc) = mdoc
extractNameDocstring (NProto _ _ _ mdoc) = mdoc
extractNameDocstring (NExt _ _ _ _ _ mdoc) = mdoc
extractNameDocstring (NModule _ mdoc) = mdoc
extractNameDocstring _ = Nothing

-- | Unescape string literals (convert \n to actual newlines, etc.)
unescapeString :: String -> String
unescapeString [] = []
unescapeString ('\\':'n':rest) = '\n' : unescapeString rest
unescapeString ('\\':'t':rest) = '\t' : unescapeString rest
unescapeString ('\\':'r':rest) = '\r' : unescapeString rest
unescapeString ('\\':'\\':rest) = '\\' : unescapeString rest
unescapeString ('\\':'"':rest) = '"' : unescapeString rest
unescapeString ('\\':'\'':rest) = '\'' : unescapeString rest
unescapeString (c:rest) = c : unescapeString rest


-- | Document a declaration in Markdown format with types
docDeclWithTypes :: TEnv -> Decl -> Doc
docDeclWithTypes tenv (Def _ n q p k a b d x ddoc) =
    let -- Look up inferred type information
        (inferredType, qConstraints, mdocstring) = case lookup n tenv of
            Just info@(NDef schema _ _) -> (Just schema, getQBindsFromSchema schema, extractNameDocstring info)
            Just info@(NSig schema _ _) -> (Just schema, getQBindsFromSchema schema, extractNameDocstring info)
            _ -> (Nothing, [], Nothing)

        -- Create a mapping from ugly type vars to nice generic names
        uglyTypeVarsFromQBinds = collectUglyTypeVars qConstraints
        uglyTypeVarsFromType = case inferredType of
            Just (TSchema _ _ t) -> collectUglyTypeVarsFromType t
            _ -> []
        allUglyTypeVars = nub (uglyTypeVarsFromQBinds ++ uglyTypeVarsFromType)
        typeVarMapping = createTypeVarMapping allUglyTypeVars

        -- Process the type information with cleanup
        (paramsWithTypes, retType, cleanedQ) = case inferredType of
            Just (TSchema _ qConstraints (TFun _ _ posRow kwdRow resType)) ->
                let cleanPosRow = cleanupTypeVars typeVarMapping posRow
                    cleanKwdRow = cleanupTypeVars typeVarMapping kwdRow
                    cleanRetType = cleanupTypeVars typeVarMapping resType
                    cleanConstraints = cleanupQBinds typeVarMapping qConstraints
                in (enrichParamsMarkdown cleanPosRow cleanKwdRow p k, Just cleanRetType, if null q then cleanConstraints else q)
            _ -> (docParamsWithTypes p k, a, q)

        -- Use docstring from AST declaration
        docstr = case mdocstring of
            Just ds -> Just ds
            Nothing -> ddoc

        -- Use cleaned up constraints for generics display
        genericsDoc = if null cleanedQ
                      then empty
                      else docGenericsMarkdown cleanedQ

        header = text "##" <+> text "`" <> pretty n <> text "`" <> genericsDoc <> paramsWithTypes <>
                 docRetTypeFormatted retType
        docstrDoc = case docstr of
            Just ds -> blank $+$ text ds
            Nothing -> empty
    in header $+$ docstrDoc
  where
    isJust (Just _) = True
    isJust Nothing = False

    enrichParamsMarkdown :: PosRow -> KwdRow -> PosPar -> KwdPar -> Doc
    enrichParamsMarkdown posRow kwdRow p k =
        parens $ enrichPosParamsMarkdown posRow p <> kwdParSepLocal p k <> enrichKwdParamsMarkdown kwdRow k

    kwdParSepLocal :: PosPar -> KwdPar -> Doc
    kwdParSepLocal PosNIL KwdNIL = empty
    kwdParSepLocal PosNIL _ = empty
    kwdParSepLocal _ KwdNIL = empty
    kwdParSepLocal _ _ = text ", "

    enrichPosParamsMarkdown :: PosRow -> PosPar -> Doc
    enrichPosParamsMarkdown _ PosNIL = empty
    enrichPosParamsMarkdown posRow (PosPar n t e p) =
        -- Skip witness parameters
        if isWitnessParam n
        then enrichPosParamsMarkdown (advanceRow posRow) p
        else
            let inferredType = extractParamTypeFromRow (nstr n) posRow
                paramType = case inferredType of
                    Just it -> Just it
                    Nothing -> t
                param = pretty n <> formatType paramType <> formatDefault e
                nextParams = enrichPosParamsMarkdown (advanceRow posRow) p
            in case p of
                PosNIL -> param
                _ -> if isEmpty nextParams
                     then param
                     else param <> comma <+> nextParams
    enrichPosParamsMarkdown _ (PosSTAR n t) = text "*" <> pretty n <> formatType t

    enrichKwdParamsMarkdown :: KwdRow -> KwdPar -> Doc
    enrichKwdParamsMarkdown _ KwdNIL = empty
    enrichKwdParamsMarkdown kwdRow (KwdPar n t e k) =
        let inferredType = extractParamTypeFromRow (nstr n) kwdRow
            paramType = case inferredType of
                Just it -> Just it
                Nothing -> t
            param = pretty n <> formatType paramType <> formatDefault e
        in case k of
            KwdNIL -> param
            _ -> param <> comma <+> enrichKwdParamsMarkdown kwdRow k
    enrichKwdParamsMarkdown _ (KwdSTAR n t) = text "**" <> pretty n <> formatType t

    extractParamTypeFromRow :: String -> Type -> Maybe Type
    extractParamTypeFromRow _ (TNil _ _) = Nothing
    extractParamTypeFromRow name (TRow _ _ n t rest)
        | nstr n == name = Just t
        | otherwise = extractParamTypeFromRow name rest
    extractParamTypeFromRow _ _ = Nothing

    advanceRow :: Type -> Type
    advanceRow (TRow _ _ _ _ rest) = rest
    advanceRow t = t

docDeclWithTypes tenv (Actor _ n q p k b ddoc) =
    let mdocstring = case lookup n tenv of
            Just info -> extractNameDocstring info
            _ -> Nothing
        -- Use docstring from AST declaration
        docstr = case mdocstring of
            Just ds -> Just ds
            Nothing -> ddoc
        header = text "##" <+> text "*actor*" <+> text "`" <> pretty n <> text "`" <> docGenerics q <> docParamsWithTypes p k
        docstrDoc = case docstr of
            Just ds -> blank $+$ text ds
            Nothing -> empty
    in header $+$ docstrDoc

docDeclWithTypes tenv (Class _ n q a b ddoc) =
    let mdocstring = case lookup n tenv of
            Just info -> extractNameDocstring info
            _ -> Nothing
        -- Use docstring from AST declaration
        docstr = case mdocstring of
            Just ds -> Just ds
            Nothing -> ddoc
        header = text "##" <+> text "*class*" <+> text "`" <> pretty n <> text "`" <> docGenerics q <> docAncestors a
        docstrDoc = case docstr of
            Just ds -> blank $+$ text ds
            Nothing -> empty
        methods = docClassBodyWithTypes tenv b
    in header $+$ docstrDoc $+$
       (if isEmpty methods then empty else blank $+$ methods)

docDeclWithTypes tenv (Protocol _ n q a b ddoc) =
    let mdocstring = case lookup n tenv of
            Just info -> extractNameDocstring info
            _ -> Nothing
        -- Fall back to extracting from body if not in TEnv
        docstr = case mdocstring of
            Just ds -> Just ds
            Nothing -> ddoc
        header = text "##" <+> text "*protocol*" <+> text "`" <> pretty n <> text "`" <> docGenerics q <> docAncestors a
        docstrDoc = case docstr of
            Just ds -> blank $+$ text ds
            Nothing -> empty
        methods = docProtocolBodyWithTypes tenv b
    in header $+$ docstrDoc $+$
       (if isEmpty methods then empty else blank $+$ methods)

docDeclWithTypes tenv (Extension _ q c a b ddoc) =
    let mdocstring = Nothing  -- Extensions don't have their own docstrings in TEnv
        -- Use docstring from AST declaration
        docstr = ddoc
        header = text "##" <+> text "*extension*" <+> text "`" <> pretty c <> text "`" <> docGenerics q <> docAncestors a
        docstrDoc = case docstr of
            Just ds -> blank $+$ text ds
            Nothing -> empty
    in header $+$ docstrDoc

-- | Helper functions for Markdown generation
docGenerics :: QBinds -> Doc
docGenerics [] = empty
docGenerics q = brackets (commaList q)

-- | Document generics for Markdown format
docGenericsMarkdown :: QBinds -> Doc
docGenericsMarkdown [] = empty
docGenericsMarkdown qbs = brackets $ hcat $ punctuate comma $ map renderQBind qbs
  where
    renderQBind (QBind tv []) = pretty (tvname tv)
    renderQBind (QBind tv constraints) =
        pretty (tvname tv) <> parens (hcat $ punctuate comma $ map pretty constraints)

-- | Document generic constraints in ASCII format (showing type variable names and constraints)
docGenericsAscii :: QBinds -> Doc
docGenericsAscii [] = empty
docGenericsAscii qbs = brackets $ commaList qbs

docRetType :: Maybe Type -> Doc
docRetType Nothing = empty
docRetType (Just t) = text " ->" <+> pretty t

docRetTypeFormatted :: Maybe Type -> Doc
docRetTypeFormatted Nothing = empty
docRetTypeFormatted (Just t) = text " → " <> text "*" <> pretty (SimplifiedType t) <> text "*"

docAncestors :: Pretty a => [a] -> Doc
docAncestors [] = empty
docAncestors as = parens (commaList as)

-- | Format parameters with italicized types
docParamsWithTypes :: PosPar -> KwdPar -> Doc
docParamsWithTypes p k = parens $ docPosParFormatted p <> docKwdParSep p k <> docKwdParFormatted k
  where
    docKwdParSep PosNIL KwdNIL = empty
    docKwdParSep PosNIL _ = empty
    docKwdParSep _ KwdNIL = empty
    docKwdParSep _ _ = text ", "

docPosParFormatted :: PosPar -> Doc
docPosParFormatted PosNIL = empty
docPosParFormatted (PosPar n t e p) =
    let param = pretty n <> formatType t <> formatDefault e
    in case p of
        PosNIL -> param
        _ -> param <> comma <+> docPosParFormatted p
docPosParFormatted (PosSTAR n t) = text "*" <> pretty n <> formatType t

docKwdParFormatted :: KwdPar -> Doc
docKwdParFormatted KwdNIL = empty
docKwdParFormatted (KwdPar n t e k) =
    let param = pretty n <> formatType t <> formatDefault e
    in case k of
        KwdNIL -> param
        _ -> param <> comma <+> docKwdParFormatted k
docKwdParFormatted (KwdSTAR n t) = text "**" <> pretty n <> formatType t

formatType :: Maybe Type -> Doc
formatType Nothing = empty
formatType (Just t) = colon <+> text "*" <> pretty (SimplifiedType t) <> text "*"

formatDefault :: Maybe Expr -> Doc
formatDefault Nothing = empty
formatDefault (Just e) = text " = " <> pretty e



-- | Document class body with types - extract attributes and methods in Markdown
docClassBodyWithTypes :: TEnv -> Suite -> Doc
docClassBodyWithTypes tenv stmts =
    let (attrs, methods) = partitionClassMembersWithTypes tenv stmts
        attrsDoc = if null attrs
                   then empty
                   else text "**Attributes:**" $+$ blank $+$ vcat (punctuate blank attrs)
        methodsDoc = if null methods
                     then empty
                     else text "**Methods:**" $+$ blank $+$ vcat (punctuate blank methods)
    in case (isEmpty attrsDoc, isEmpty methodsDoc) of
        (True, True) -> empty
        (False, True) -> attrsDoc
        (True, False) -> methodsDoc
        (False, False) -> attrsDoc $+$ blank $+$ methodsDoc

-- | Document protocol body with types - extract method signatures in Markdown
docProtocolBodyWithTypes :: TEnv -> Suite -> Doc
docProtocolBodyWithTypes tenv stmts =
    let methods = concatMap (extractMethodsWithTypes tenv) stmts
    in if null methods
       then empty
       else text "**Methods:**" $+$ blank $+$ vcat (punctuate blank methods)


-- | Partition class members into attributes and methods with types
partitionClassMembersWithTypes :: TEnv -> Suite -> ([Doc], [Doc])
partitionClassMembersWithTypes tenv stmts = foldl partition ([], []) stmts
  where
    partition (attrs, methods) (Signature _ vs sc d) = (attrs ++ [docAttribute vs sc], methods)
    partition (attrs, methods) (Decl _ decls) = (attrs, methods ++ map (docMethodWithTypes tenv) decls)
    partition acc _ = acc

-- | Extract method documentation from statements with types
extractMethodsWithTypes :: TEnv -> Stmt -> [Doc]
extractMethodsWithTypes tenv (Decl _ decls) = map (docMethodWithTypes tenv) decls
extractMethodsWithTypes _ (Signature _ vs sc d) = [docMethodSignature vs sc]
extractMethodsWithTypes _ _ = []

-- | Document an attribute in Markdown
docAttribute :: [Name] -> TSchema -> Doc
docAttribute vs (TSchema _ _ t) =
    text "-" <+> text "`" <> commaList vs <> text "`:" <+> text "*" <> pretty (SimplifiedType t) <> text "*"


-- | Document a method signature in Markdown
docMethodSignature :: [Name] -> TSchema -> Doc
docMethodSignature vs (TSchema _ _ t) =
    text "-" <+> text "`" <> commaList vs <> text "`:" <+> text "*" <> pretty (SimplifiedType t) <> text "*"

-- | Document a method in Markdown with types
docMethodWithTypes :: TEnv -> Decl -> Doc
docMethodWithTypes tenv (Def _ n q p k a b _ _ ddoc) =
    let (inferredType, paramsWithTypes, retType) = case lookup n tenv of
            Just (NDef (TSchema _ _ t@(TFun _ _ posRow kwdRow resType)) _ _) ->
                (Just t, enrichParamsMarkdown posRow kwdRow p k, Just resType)
            Just (NSig (TSchema _ _ t@(TFun _ _ posRow kwdRow resType)) _ _) ->
                (Just t, enrichParamsMarkdown posRow kwdRow p k, Just resType)
            _ -> (Nothing, docParamsWithTypes p k, a)
        signature = text "-" <+> text "`" <> pretty n <> text "`" <> docGenerics q <> paramsWithTypes <>
                    docRetTypeFormatted (if isJust retType then retType else a)
        docstr = case ddoc of
            Just ds -> nest 2 (text ds)
            Nothing -> empty
    in signature $+$
       (if isEmpty docstr then empty else blank $+$ docstr)
  where
    isJust (Just _) = True
    isJust Nothing = False

    enrichParamsMarkdown :: PosRow -> KwdRow -> PosPar -> KwdPar -> Doc
    enrichParamsMarkdown posRow kwdRow p k =
        parens $ enrichPosParamsMarkdown posRow p <> kwdParSepLocal p k <> enrichKwdParamsMarkdown kwdRow k

    kwdParSepLocal :: PosPar -> KwdPar -> Doc
    kwdParSepLocal PosNIL KwdNIL = empty
    kwdParSepLocal PosNIL _ = empty
    kwdParSepLocal _ KwdNIL = empty
    kwdParSepLocal _ _ = text ", "

    enrichPosParamsMarkdown :: PosRow -> PosPar -> Doc
    enrichPosParamsMarkdown _ PosNIL = empty
    enrichPosParamsMarkdown posRow (PosPar n t e p) =
        -- Skip witness parameters
        if isWitnessParam n
        then enrichPosParamsMarkdown (advanceRow posRow) p
        else
            let inferredType = extractParamTypeFromRow (nstr n) posRow
                paramType = case inferredType of
                    Just it -> Just it
                    Nothing -> t
                param = pretty n <> formatType paramType <> formatDefault e
                nextParams = enrichPosParamsMarkdown (advanceRow posRow) p
            in case p of
                PosNIL -> param
                _ -> if isEmpty nextParams
                     then param
                     else param <> comma <+> nextParams
    enrichPosParamsMarkdown _ (PosSTAR n t) = text "*" <> pretty n <> formatType t

    enrichKwdParamsMarkdown :: KwdRow -> KwdPar -> Doc
    enrichKwdParamsMarkdown _ KwdNIL = empty
    enrichKwdParamsMarkdown kwdRow (KwdPar n t e k) =
        let inferredType = extractParamTypeFromRow (nstr n) kwdRow
            paramType = case inferredType of
                Just it -> Just it
                Nothing -> t
            param = pretty n <> formatType paramType <> formatDefault e
        in case k of
            KwdNIL -> param
            _ -> param <> comma <+> enrichKwdParamsMarkdown kwdRow k
    enrichKwdParamsMarkdown _ (KwdSTAR n t) = text "**" <> pretty n <> formatType t

    extractParamTypeFromRow :: String -> Type -> Maybe Type
    extractParamTypeFromRow _ (TNil _ _) = Nothing
    extractParamTypeFromRow name (TRow _ _ n t rest)
        | nstr n == name = Just t
        | otherwise = extractParamTypeFromRow name rest
    extractParamTypeFromRow _ _ = Nothing

    advanceRow :: Type -> Type
    advanceRow (TRow _ _ _ _ rest) = rest
    advanceRow t = t
docMethodWithTypes _ _ = empty

-- | Print documentation as Markdown with type information
printMdDoc :: NameInfo -> Module -> String
printMdDoc nmod m = render (docModuleWithTypes nmod m)

-- | Print documentation as ASCII with optional styling and type information
-- Parameters:
--   useStyle: Enable ANSI control codes (bold + color)
--   tenv: Type environment for enhanced type information
--   module: The module to document
printAsciiDoc :: Bool -> NameInfo -> Module -> String
printAsciiDoc useStyle nmod m =
    render (docModuleAsciiUnified useStyle nmod m) ++ "\n"


-- Terminal formatting codes
bold, underline, reset :: Bool -> String
bold True = "\ESC[1m"
bold False = ""
underline True = "\ESC[4m"
underline False = ""
reset True = "\ESC[0m"
reset False = ""

-- Color codes (only when color is enabled)
cyan, yellow, green, dim :: Bool -> String
cyan True = "\ESC[36m"
cyan False = ""
yellow True = "\ESC[33m"
yellow False = ""
green True = "\ESC[32m"
green False = ""
dim True = "\ESC[2m"
dim False = ""

-- | Generate ASCII documentation from a module with unified style and type handling
docModuleAsciiUnified :: Bool -> NameInfo -> Module -> Doc
docModuleAsciiUnified useStyle (NModule tenv mdocstring) (Module qn _ stmts) =
    -- Use module docstring from NModule
    let moduleDocstring = mdocstring
        (title, restDoc) = case moduleDocstring of
            Just ds -> splitDocstring ds
            Nothing -> ("", Nothing)
        -- Module header with visual emphasis
        header = text (bold useStyle) <> pretty qn <> text (reset useStyle) <>
                 (if null title then empty else text " - " <> text title)
        bodyDoc = case restDoc of
            Just body -> blank $+$ text body $+$ blank
            Nothing -> empty
    in header $+$ bodyDoc $+$ blank $+$ docTopLevelAsciiUnified useStyle tenv stmts

-- | Document top-level definitions with unified style and type handling
docTopLevelAsciiUnified :: Bool -> TEnv -> Suite -> Doc
docTopLevelAsciiUnified useStyle tenv stmts =
    let docs = concatMap (extractTopLevelUnified useStyle tenv) stmts
        separated = case docs of
            [] -> []
            [d] -> [d]
            (d:ds) -> d : map (blank $+$) ds
    in vcat separated

-- | Extract and document top-level declarations with unified handling
extractTopLevelUnified :: Bool -> TEnv -> Stmt -> [Doc]
extractTopLevelUnified useStyle tenv (Decl _ decls) = map (docDeclUnified useStyle tenv) decls
extractTopLevelUnified useStyle tenv (With _ _ body) = concatMap (extractTopLevelUnified useStyle tenv) body
extractTopLevelUnified _ _ _ = []







-- | Extract QBinds from a type schema
getQBindsFromSchema :: TSchema -> QBinds
getQBindsFromSchema (TSchema _ qb _) = qb

-- | Collect all ugly type variable names from QBinds
collectUglyTypeVars :: QBinds -> [Name]
collectUglyTypeVars qbs = [tvname tv | QBind tv _ <- qbs, isUglyName (tvname tv)]
  where
    isUglyName n = let name = nstr n
                   in length name > 2 && elem '_' name && all isDigit (drop 2 name)

-- | Collect all ugly type variable names from a Type
collectUglyTypeVarsFromType :: Type -> [Name]
collectUglyTypeVarsFromType (TVar _ tv) =
    let name = tvname tv
    in if isUglyName name then [name] else []
  where
    isUglyName n = let nameStr = nstr n
                   in length nameStr > 2 && elem '_' nameStr && all isDigit (drop 2 nameStr)
collectUglyTypeVarsFromType (TFun _ _ posRow kwdRow resType) =
    collectUglyTypeVarsFromType posRow ++ collectUglyTypeVarsFromType kwdRow ++ collectUglyTypeVarsFromType resType
collectUglyTypeVarsFromType (TCon _ (TC _ ts)) = concatMap collectUglyTypeVarsFromType ts
collectUglyTypeVarsFromType (TRow _ _ _ t rest) = collectUglyTypeVarsFromType t ++ collectUglyTypeVarsFromType rest
collectUglyTypeVarsFromType (TTuple _ posRow kwdRow) = collectUglyTypeVarsFromType posRow ++ collectUglyTypeVarsFromType kwdRow
collectUglyTypeVarsFromType (TOpt _ t) = collectUglyTypeVarsFromType t
collectUglyTypeVarsFromType _ = []

-- | Create a mapping from ugly names to nice generic names (A, B, C...)
createTypeVarMapping :: [Name] -> [(Name, Name)]
createTypeVarMapping uglyNames = zip uglyNames niceNames
  where
    niceNames = [Name NoLoc [c] | c <- ['A'..'Z']]

-- | Replace ugly type vars with nice names in a type
cleanupTypeVars :: [(Name, Name)] -> Type -> Type
cleanupTypeVars mapping (TVar l tv) =
    case lookup (tvname tv) mapping of
        Just niceName -> TVar l (TV (tvkind tv) niceName)
        Nothing -> TVar l tv
cleanupTypeVars mapping (TFun l fx posRow kwdRow resType) =
    TFun l fx (cleanupTypeVars mapping posRow) (cleanupTypeVars mapping kwdRow) (cleanupTypeVars mapping resType)
cleanupTypeVars mapping (TCon l (TC qn ts)) =
    TCon l (TC qn (map (cleanupTypeVars mapping) ts))
cleanupTypeVars mapping (TRow l k n t rest) =
    TRow l k n (cleanupTypeVars mapping t) (cleanupTypeVars mapping rest)
cleanupTypeVars mapping (TTuple l posRow kwdRow) =
    TTuple l (cleanupTypeVars mapping posRow) (cleanupTypeVars mapping kwdRow)
cleanupTypeVars mapping (TOpt l t) =
    TOpt l (cleanupTypeVars mapping t)
cleanupTypeVars _ t = t

-- | Replace ugly type vars with nice names in QBinds
cleanupQBinds :: [(Name, Name)] -> [QBind] -> [QBind]
cleanupQBinds mapping = map cleanupQBind
  where
    cleanupQBind (QBind tv tcons) =
        case lookup (tvname tv) mapping of
            Just niceName -> QBind (TV (tvkind tv) niceName) (map (cleanupTCon mapping) tcons)
            Nothing -> QBind tv (map (cleanupTCon mapping) tcons)

    cleanupTCon :: [(Name, Name)] -> TCon -> TCon
    cleanupTCon mapping (TC qn ts) = TC qn (map (cleanupTypeVars mapping) ts)

-- | Document a declaration with unified style and type handling
docDeclUnified :: Bool -> TEnv -> Decl -> Doc
docDeclUnified useStyle tenv decl@(Def _ n q p k a b d x ddoc) =
    let explicitGenerics = extractGenerics q
        -- Look up inferred type information
        (inferredType, qConstraints, docstringFromTEnv) = case lookup n tenv of
            Just info@(NDef schema _ _) -> (Just schema, getQBindsFromSchema schema, extractNameDocstring info)
            Just info@(NSig schema _ _) -> (Just schema, getQBindsFromSchema schema, extractNameDocstring info)
            _ -> (Nothing, [], Nothing)

        -- Create a mapping from ugly type vars to nice generic names
        uglyTypeVarsFromQBinds = collectUglyTypeVars qConstraints
        uglyTypeVarsFromType = case inferredType of
            Just (TSchema _ _ t) -> collectUglyTypeVarsFromType t
            _ -> []
        allUglyTypeVars = nub (uglyTypeVarsFromQBinds ++ uglyTypeVarsFromType)
        typeVarMapping = createTypeVarMapping allUglyTypeVars

        -- Transform the inferred types using the mapping
        (paramsWithTypes, retType) = case inferredType of
            Just (TSchema _ _ (TFun _ _ posRow kwdRow resType)) ->
                let cleanPosRow = cleanupTypeVars typeVarMapping posRow
                    cleanKwdRow = cleanupTypeVars typeVarMapping kwdRow
                    cleanRetType = cleanupTypeVars typeVarMapping resType
                in (enrichParamsStyledAsciiDecl useStyle cleanPosRow cleanKwdRow p k, Just cleanRetType)
            _ -> (docParamsStyledAscii useStyle useStyle p k, a)

        -- Use docstring from AST declaration
        docstr = case docstringFromTEnv of
            Just ds -> Just ds
            Nothing -> ddoc

        -- Show generics if we have them (either explicit or inferred)
        allGenerics = if null q && not (null typeVarMapping)
                      then map snd typeVarMapping  -- Use the nice names from mapping
                      else map (\(QBind tv _) -> tvname tv) q
        genericsDoc = if null allGenerics
                      then empty
                      else brackets $ hcat $ punctuate comma $ map pretty allGenerics

        header = text (bold useStyle) <> pretty n <> text (reset useStyle) <>
                 genericsDoc <> paramsWithTypes <> docRetTypeStyled useStyle useStyle retType
        docstrDoc = case docstr of
            Just ds -> nest 2 (text ds)
            Nothing -> empty
    in header $+$ (if isEmpty docstrDoc then empty else docstrDoc)
  where
    -- Check if a QBind contains ugly type variable names like T_638
    isUglyTypeVar :: QBind -> Bool
    isUglyTypeVar (QBind tv _) =
        let name = nstr (tvname tv)
        in length name > 2 && elem '_' name && all isDigit (drop 2 name)

    -- Check if a type contains ugly type variable names
    containsUglyTypeVar :: Type -> Bool
    containsUglyTypeVar (TVar _ tv) =
        let name = nstr (tvname tv)
        in length name > 2 && elem '_' name && all isDigit (drop 2 name)
    containsUglyTypeVar (TFun _ _ posRow kwdRow resType) =
        containsUglyTypeVar posRow || containsUglyTypeVar kwdRow || containsUglyTypeVar resType
    containsUglyTypeVar (TCon _ (TC _ ts)) = any containsUglyTypeVar ts
    containsUglyTypeVar (TRow _ _ _ t rest) = containsUglyTypeVar t || containsUglyTypeVar rest
    containsUglyTypeVar (TTuple _ posRow kwdRow) = containsUglyTypeVar posRow || containsUglyTypeVar kwdRow
    containsUglyTypeVar (TOpt _ t) = containsUglyTypeVar t
    containsUglyTypeVar _ = False

    isNothing :: Maybe a -> Bool
    isNothing Nothing = True
    isNothing _ = False

    -- Simple parameter display without types
    docParamsSimpleAscii :: PosPar -> KwdPar -> Doc
    docParamsSimpleAscii p k = parens $ docPosParSimple p <> docKwdParSepAscii p k <> docKwdParSimple k

    docPosParSimple :: PosPar -> Doc
    docPosParSimple PosNIL = empty
    docPosParSimple (PosPar n _ _ p) =
        let param = pretty n
        in case p of
            PosNIL -> param
            _ -> param <> comma <+> docPosParSimple p
    docPosParSimple (PosSTAR n _) = text "*" <> pretty n

    docKwdParSimple :: KwdPar -> Doc
    docKwdParSimple KwdNIL = empty
    docKwdParSimple (KwdPar n _ _ k) =
        let param = pretty n
        in case k of
            KwdNIL -> param
            _ -> param <> comma <+> docKwdParSimple k
    docKwdParSimple (KwdSTAR n _) = text "**" <> pretty n


    enrichParamsStyledAsciiDecl :: Bool -> PosRow -> KwdRow -> PosPar -> KwdPar -> Doc
    enrichParamsStyledAsciiDecl useStyle posRow kwdRow p k =
        parens $ enrichPosParamsStyledDecl useStyle posRow p <> docKwdParSepAscii p k <>
                 enrichKwdParamsStyledDecl useStyle kwdRow k

    enrichPosParamsStyledDecl :: Bool -> PosRow -> PosPar -> Doc
    enrichPosParamsStyledDecl _ _ PosNIL = empty
    enrichPosParamsStyledDecl useStyle posRow (PosPar n t e p) =
        -- Skip witness parameters
        if isWitnessParam n
        then enrichPosParamsStyledDecl useStyle (advanceRow posRow) p
        else
            let inferredType = extractParamTypeFromRow (nstr n) posRow
                paramType = case inferredType of
                    Just it -> Just it
                    Nothing -> t
                param = pretty n <> formatTypeStyled useStyle useStyle paramType <> formatDefault e
                nextParams = enrichPosParamsStyledDecl useStyle (advanceRow posRow) p
            in case p of
                PosNIL -> param
                _ -> if isEmpty nextParams
                     then param
                     else param <> comma <+> nextParams

    enrichKwdParamsStyledDecl :: Bool -> KwdRow -> KwdPar -> Doc
    enrichKwdParamsStyledDecl _ _ KwdNIL = empty
    enrichKwdParamsStyledDecl useStyle kwdRow (KwdPar n t e k) =
        -- Skip witness parameters
        if isWitnessParam n
        then enrichKwdParamsStyledDecl useStyle (advanceRow kwdRow) k
        else
            let inferredType = extractParamTypeFromKwdRow (nstr n) kwdRow
                paramType = case inferredType of
                    Just it -> Just it
                    Nothing -> t
                param = pretty n <> formatTypeStyled useStyle useStyle paramType <> formatDefault e
                nextParams = enrichKwdParamsStyledDecl useStyle (advanceRow kwdRow) k
            in case k of
                KwdNIL -> param
                _ -> if isEmpty nextParams
                     then param
                     else param <> comma <+> nextParams

docDeclUnified useStyle tenv (Actor _ n q p k b ddoc) =
    let (paramsWithTypes, docstringFromTEnv) = case lookup n tenv of
            Just info@(NAct _ posRow kwdRow _ mdoc) ->
                (enrichParamsStyledAsciiActr useStyle posRow kwdRow p k, mdoc)
            _ -> (docParamsStyledAscii useStyle useStyle p k, Nothing)
        -- Use docstring from AST declaration
        docstr = case docstringFromTEnv of
            Just ds -> Just ds
            Nothing -> ddoc
        header = text (cyan useStyle ++ "actor" ++ reset useStyle) <+>
                 text (bold useStyle) <> pretty n <> text (reset useStyle) <>
                 docGenerics q <> paramsWithTypes
        docstrDoc = case docstr of
            Just ds -> nest 2 (text ds)
            Nothing -> empty
    in header $+$ (if isEmpty docstrDoc then empty else docstrDoc)
  where
    enrichParamsStyledAsciiActr useStyle posRow kwdRow p k =
        parens $ enrichPosParamsStyledDecl useStyle posRow p <> docKwdParSepAscii p k <>
                 enrichKwdParamsStyledDecl useStyle kwdRow k

    enrichPosParamsStyledDecl _ _ PosNIL = empty
    enrichPosParamsStyledDecl useStyle posRow (PosPar n t e p) =
        if isWitnessParam n
        then enrichPosParamsStyledDecl useStyle (advanceRow posRow) p
        else
            let inferredType = extractParamTypeFromRow (nstr n) posRow
                paramType = case inferredType of
                    Just it -> Just it
                    Nothing -> t
                param = pretty n <> formatTypeStyled useStyle useStyle paramType <> formatDefault e
                nextParams = enrichPosParamsStyledDecl useStyle (advanceRow posRow) p
            in case p of
                PosNIL -> param
                _ -> if isEmpty nextParams
                     then param
                     else param <> comma <+> nextParams

    enrichKwdParamsStyledDecl _ _ KwdNIL = empty
    enrichKwdParamsStyledDecl useStyle kwdRow (KwdPar n t e k) =
        if isWitnessParam n
        then enrichKwdParamsStyledDecl useStyle (advanceRow kwdRow) k
        else
            let inferredType = extractParamTypeFromKwdRow (nstr n) kwdRow
                paramType = case inferredType of
                    Just it -> Just it
                    Nothing -> t
                param = pretty n <> formatTypeStyled useStyle useStyle paramType <> formatDefault e
                nextParams = enrichKwdParamsStyledDecl useStyle (advanceRow kwdRow) k
            in case k of
                KwdNIL -> param
                _ -> if isEmpty nextParams
                     then param
                     else param <> comma <+> nextParams

docDeclUnified useStyle tenv (Class _ n q a b ddoc) =
    let docstringFromTEnv = case lookup n tenv of
            Just info -> extractNameDocstring info
            _ -> Nothing
        -- Always get docstring from either TEnv or AST
        docstr = case docstringFromTEnv of
            Just ds -> Just ds
            Nothing -> ddoc
        header = text (cyan useStyle ++ "class" ++ reset useStyle) <+>
                 text (bold useStyle) <> pretty n <> text (reset useStyle) <>
                 docGenerics q <> docAncestors a
        docstrDoc = case docstr of
            Just ds -> nest 2 (text ds)
            Nothing -> empty
        -- Document methods and attributes
        (attrs, methods) = extractClassMembers b
        attrsDoc = if null attrs then empty else
            blank $+$ nest 2 (text (yellow useStyle ++ "Attributes:" ++ reset useStyle)) $+$
            vcat [nest 4 (docAttrUnified useStyle attr) | attr <- attrs]
        methodsDoc = if null methods then empty else
            blank $+$ nest 2 (text (yellow useStyle ++ "Methods:" ++ reset useStyle)) $+$
            vcat [nest 4 (docMethodUnified useStyle tenv n meth) | meth <- methods]
    in header $+$ (if isEmpty docstrDoc then empty else docstrDoc) $+$ attrsDoc $+$ methodsDoc
  where
    -- Document a single attribute
    docAttrUnified :: Bool -> (Name, Maybe Type) -> Doc
    docAttrUnified useStyle (name, mtype) =
        pretty name <>
        case mtype of
            Just t -> text ": " <> pretty (SimplifiedType t)
            Nothing -> empty

    -- Document a method signature
    docMethodUnified :: Bool -> TEnv -> Name -> (Name, QBinds, PosPar, KwdPar, Maybe Type, Maybe String) -> Doc
    docMethodUnified useStyle tenv className (methodName, q, p, k, retType, docstr) =
        let (paramsWithTypes, inferredRetType) =
                -- Try to get type info from TEnv
                case lookup className tenv of
                    Just (NClass _ _ methods _) ->
                        case lookup methodName methods of
                            Just info ->
                                case extractTypeFromNameInfo info of
                                    Just (TFun _ _ posRow kwdRow resType) ->
                                        (enrichParamsStyledAsciiDecl useStyle posRow kwdRow p k, Just resType)
                                    _ -> (docParamsStyledAscii useStyle useStyle p k, retType)
                            _ -> (docParamsStyledAscii useStyle useStyle p k, retType)
                    _ -> (docParamsStyledAscii useStyle useStyle p k, retType)
            -- Don't show return type for __init__ methods or methods that return None
            showRetType = case fromMaybe retType inferredRetType of
                          Just (TNone _) -> empty
                          Just t -> if nstr methodName == "__init__"
                                    then empty
                                    else docRetTypeStyled useStyle useStyle (Just t)
                          Nothing -> empty
            header = text (bold useStyle) <> pretty methodName <> text (reset useStyle) <>
                     docGenerics q <> paramsWithTypes <> showRetType
            docstrDoc = case docstr of
                Just ds -> text ds
                Nothing -> empty
        in header $+$ (if isEmpty docstrDoc then empty else nest 2 docstrDoc)
      where
        fromMaybe def Nothing = def
        fromMaybe _ (Just x) = Just x

        extractTypeFromNameInfo :: NameInfo -> Maybe Type
        extractTypeFromNameInfo (NDef (TSchema _ _ t) _ _) = Just t
        extractTypeFromNameInfo (NSig (TSchema _ _ t) _ _) = Just t
        extractTypeFromNameInfo _ = Nothing

        enrichParamsStyledAsciiDecl useStyle posRow kwdRow p k =
            parens $ enrichPosParamsStyledMeth useStyle posRow p <> docKwdParSepAscii p k <>
                     enrichKwdParamsStyledMeth useStyle kwdRow k

        enrichPosParamsStyledMeth _ _ PosNIL = empty
        enrichPosParamsStyledMeth useStyle posRow (PosPar n t e p) =
            if isWitnessParam n
            then enrichPosParamsStyledMeth useStyle (advanceRow posRow) p
            else
                let inferredType = extractParamTypeFromRow (nstr n) posRow
                    paramType = case inferredType of
                        Just it -> Just it
                        Nothing -> t
                    param = pretty n <> formatTypeStyled useStyle useStyle paramType <> formatDefault e
                    nextParams = enrichPosParamsStyledMeth useStyle (advanceRow posRow) p
                in case p of
                    PosNIL -> param
                    _ -> if isEmpty nextParams
                         then param
                         else param <> comma <+> nextParams

        enrichKwdParamsStyledMeth _ _ KwdNIL = empty
        enrichKwdParamsStyledMeth useStyle kwdRow (KwdPar n t e k) =
            if isWitnessParam n
            then enrichKwdParamsStyledMeth useStyle (advanceRow kwdRow) k
            else
                let inferredType = extractParamTypeFromKwdRow (nstr n) kwdRow
                    paramType = case inferredType of
                        Just it -> Just it
                        Nothing -> t
                    param = pretty n <> formatTypeStyled useStyle useStyle paramType <> formatDefault e
                    nextParams = enrichKwdParamsStyledMeth useStyle (advanceRow kwdRow) k
                in case k of
                    KwdNIL -> param
                    _ -> if isEmpty nextParams
                         then param
                         else param <> comma <+> nextParams

docDeclUnified useStyle tenv (Protocol _ n q a b ddoc) =
    let docstringFromTEnv = case lookup n tenv of
            Just info -> extractNameDocstring info
            _ -> Nothing
        -- Always get docstring from either TEnv or AST
        docstr = case docstringFromTEnv of
            Just ds -> Just ds
            Nothing -> ddoc
        header = text (cyan useStyle ++ "protocol" ++ reset useStyle) <+>
                 text (bold useStyle) <> pretty n <> text (reset useStyle) <>
                 docGenerics q <> docAncestors a
        docstrDoc = case docstr of
            Just ds -> nest 2 (text ds)
            Nothing -> empty
        -- Document protocol methods
        methods = extractProtocolMethods b
        methodsDoc = if null methods then empty else
            blank $+$ nest 2 (text (yellow useStyle ++ "Methods:" ++ reset useStyle)) $+$
            vcat (intersperse blank [nest 4 (docProtocolMethod meth) | meth <- methods])
    in header $+$ (if isEmpty docstrDoc then empty else docstrDoc) $+$ methodsDoc
  where
    -- Document a protocol method
    docProtocolMethod :: (Name, QBinds, PosPar, KwdPar, Maybe Type, Maybe String) -> Doc
    docProtocolMethod (methodName, q, p, k, retType, docstr) =
        -- Protocol methods are just signatures
        let header = pretty methodName <> text ":" <+>
                     case retType of
                         Just t -> pretty (SimplifiedType t)
                         Nothing -> text "()"
            docstrDoc = case docstr of
                Just ds -> nest 2 (text ds)
                Nothing -> empty
        in header $+$ (if isEmpty docstrDoc then empty else docstrDoc)

docDeclUnified useStyle tenv (Extension _ q c a b ddoc) =
    let docstringFromTEnv = Nothing  -- Extensions don't have docstrings in TEnv
        -- Get docstring from AST
        docstr = ddoc
        header = text (cyan useStyle ++ "extension" ++ reset useStyle) <+>
                 pretty c <> docAncestors a
        docstrDoc = case docstr of
            Just ds -> nest 2 (text ds)
            Nothing -> empty
    in header $+$ (if isEmpty docstrDoc then empty else docstrDoc)


-- | Document a declaration with man-page visual style and types

-- | Format parameters with visual styling
docParamsStyledAscii :: Bool -> Bool -> PosPar -> KwdPar -> Doc
docParamsStyledAscii useBold useColor p k = parens $ docPosParStyled useBold useColor p <> docKwdParSepAscii p k <> docKwdParStyled useBold useColor k

docPosParStyled :: Bool -> Bool -> PosPar -> Doc
docPosParStyled _ _ PosNIL = empty
docPosParStyled useBold useColor (PosPar n t e p) =
    let param = pretty n <> formatTypeStyled useBold useColor t <> formatDefault e
    in case p of
        PosNIL -> param
        _ -> param <> comma <+> docPosParStyled useBold useColor p
docPosParStyled useBold useColor (PosSTAR n t) = text "*" <> pretty n <> formatTypeStyled useBold useColor t

docKwdParStyled :: Bool -> Bool -> KwdPar -> Doc
docKwdParStyled _ _ KwdNIL = empty
docKwdParStyled useBold useColor (KwdPar n t e k) =
    let param = pretty n <> formatTypeStyled useBold useColor t <> formatDefault e
    in case k of
        KwdNIL -> param
        _ -> param <> comma <+> docKwdParStyled useBold useColor k
docKwdParStyled useBold useColor (KwdSTAR n t) = text "**" <> pretty n <> formatTypeStyled useBold useColor t

formatTypeStyled :: Bool -> Bool -> Maybe Type -> Doc
formatTypeStyled _ _ Nothing = empty
formatTypeStyled useBold useColor (Just t) =
    if useColor
    then colon <+> text (green True) <> pretty (SimplifiedType t) <> text (reset True)
    else colon <+> text (underline useBold) <> pretty (SimplifiedType t) <> text (reset useBold)

docRetTypeStyled :: Bool -> Bool -> Maybe Type -> Doc
docRetTypeStyled _ _ Nothing = empty
docRetTypeStyled useBold useColor (Just t) =
    if useColor
    then text " -> " <> text (green True) <> pretty (SimplifiedType t) <> text (reset True)
    else text " -> " <> text (underline useBold) <> pretty (SimplifiedType t) <> text (reset useBold)

-- | Document class body with visual styling
-- | Document class body with visual styling and types
docClassBodyStyledWithTypes :: Bool -> Bool -> TEnv -> Suite -> Doc
docClassBodyStyledWithTypes useBold useColor tenv stmts =
    let (attrs, methods) = partitionClassMembersStyledWithTypes useBold useColor tenv stmts
        attrsDoc = if null attrs
                   then empty
                   else nest 2 (text "Attributes:" $+$ vcat attrs)
        methodsDoc = if null methods
                     then empty
                     else nest 2 (text "Methods:" $+$ vcatWithSpacing methods)
    in case (isEmpty attrsDoc, isEmpty methodsDoc) of
        (True, True) -> empty
        (False, True) -> attrsDoc
        (True, False) -> methodsDoc
        (False, False) -> attrsDoc $+$ blank $+$ methodsDoc
  where
    vcatWithSpacing [] = empty
    vcatWithSpacing [m] = m
    vcatWithSpacing (m:ms) = m $+$ vcat (map addSpacing ms)

    addSpacing m = blank $+$ m

-- | Document protocol body with visual styling and types
docProtocolBodyStyledWithTypes :: Bool -> Bool -> TEnv -> Suite -> Doc
docProtocolBodyStyledWithTypes useBold useColor tenv stmts =
    let methods = concatMap (extractMethodsStyledWithTypes useBold useColor tenv) stmts
    in if null methods
       then empty
       else nest 2 (text "Methods:" $+$ vcatWithSpacing methods)
  where
    vcatWithSpacing [] = empty
    vcatWithSpacing [m] = m
    vcatWithSpacing (m:ms) = m $+$ vcat (map addSpacing ms)

    addSpacing m = blank $+$ m

-- | Partition class members into attributes and methods with styling
partitionClassMembersStyled :: Bool -> Bool -> Suite -> ([Doc], [Doc])
partitionClassMembersStyled useBold useColor stmts = foldl partition ([], []) stmts
  where
    partition (attrs, methods) (Signature _ vs sc d) = (attrs ++ [docAttributeStyled useBold useColor vs sc], methods)
    partition (attrs, methods) (Decl _ decls) = (attrs, methods ++ map (docMethodStyled useBold useColor) decls)
    partition acc _ = acc

-- | Partition class members into attributes and methods with styling and types
partitionClassMembersStyledWithTypes :: Bool -> Bool -> TEnv -> Suite -> ([Doc], [Doc])
partitionClassMembersStyledWithTypes useBold useColor tenv stmts = foldl partition ([], []) stmts
  where
    partition (attrs, methods) (Signature _ vs sc d) = (attrs ++ [docAttributeStyled useBold useColor vs sc], methods)
    partition (attrs, methods) (Decl _ decls) = (attrs, methods ++ map (docMethodStyledWithTypes useBold useColor tenv) decls)
    partition acc _ = acc

-- | Extract method documentation with styling
extractMethodsStyled :: Bool -> Bool -> Stmt -> [Doc]
extractMethodsStyled useBold useColor (Decl _ decls) = map (docMethodStyled useBold useColor) decls
extractMethodsStyled useBold useColor (Signature _ vs sc d) = [docMethodSignatureStyled useBold useColor vs sc]
extractMethodsStyled _ _ _ = []

-- | Extract method documentation with styling and types
extractMethodsStyledWithTypes :: Bool -> Bool -> TEnv -> Stmt -> [Doc]
extractMethodsStyledWithTypes useBold useColor tenv (Decl _ decls) = map (docMethodStyledWithTypes useBold useColor tenv) decls
extractMethodsStyledWithTypes useBold useColor _ (Signature _ vs sc d) = [docMethodSignatureStyled useBold useColor vs sc]
extractMethodsStyledWithTypes _ _ _ _ = []

-- | Document an attribute with styling
docAttributeStyled :: Bool -> Bool -> [Name] -> TSchema -> Doc
docAttributeStyled useBold useColor vs (TSchema _ _ t) =
    if useColor
    then nest 2 $ text "- " <> commaList vs <> text ": " <> text (green True) <> pretty (SimplifiedType t) <> text (reset True)
    else nest 2 $ text "- " <> commaList vs <> text ": " <> text (underline useBold) <> pretty (SimplifiedType t) <> text (reset useBold)

-- | Document a method with styling
docMethodStyled :: Bool -> Bool -> Decl -> Doc
docMethodStyled useBold useColor (Def _ n q p k a b _ _ ddoc) =
    let signature = nest 2 $ text "- " <> text (bold useBold) <> pretty n <> text (reset useBold) <>
                    docGenerics q <> docParamsStyledAscii useBold useColor p k <> docRetTypeStyled useBold useColor a
        docstr = case ddoc of
            Just ds -> nest 4 (text ds)
            Nothing -> empty
    in signature $+$
       (if isEmpty docstr then empty else docstr)
docMethodStyled _ _ _ = empty

-- | Document a method signature with styling
docMethodSignatureStyled :: Bool -> Bool -> [Name] -> TSchema -> Doc
docMethodSignatureStyled useBold useColor vs (TSchema _ _ t) =
    if useColor
    then nest 2 $ text "- " <> commaList vs <> text ": " <> text (green True) <> pretty (SimplifiedType t) <> text (reset True)
    else nest 2 $ text "- " <> commaList vs <> text ": " <> text (underline useBold) <> pretty (SimplifiedType t) <> text (reset useBold)

-- | Document a method with styling and types
docMethodStyledWithTypes :: Bool -> Bool -> TEnv -> Decl -> Doc
docMethodStyledWithTypes useBold useColor tenv (Def _ n q p k a b _ _ ddoc) =
    let (inferredType, paramsWithTypes, retType) = case lookup n tenv of
            Just (NDef (TSchema _ _ t@(TFun _ _ posRow kwdRow resType)) _ _) ->
                (Just t, enrichParamsStyledAscii useBold useColor posRow kwdRow p k, Just resType)
            Just (NSig (TSchema _ _ t@(TFun _ _ posRow kwdRow resType)) _ _) ->
                (Just t, enrichParamsStyledAscii useBold useColor posRow kwdRow p k, Just resType)
            _ -> (Nothing, docParamsStyledAscii useBold useColor p k, a)
        signature = nest 2 $ text "- " <> text (bold useBold) <> pretty n <> text (reset useBold) <>
                    docGenerics q <> paramsWithTypes <>
                    docRetTypeStyled useBold useColor (if isJust retType then retType else a)
        docstr = case ddoc of
            Just ds -> nest 4 (text ds)
            Nothing -> empty
    in signature $+$
       (if isEmpty docstr then empty else docstr)
  where
    isJust (Just _) = True
    isJust Nothing = False

    enrichParamsStyledAscii :: Bool -> Bool -> PosRow -> KwdRow -> PosPar -> KwdPar -> Doc
    enrichParamsStyledAscii useBold useColor posRow kwdRow p k =
        parens $ enrichPosParamsStyled useBold useColor posRow p <> docKwdParSepAscii p k <>
                 enrichKwdParamsStyled useBold useColor kwdRow k

    enrichPosParamsStyled :: Bool -> Bool -> PosRow -> PosPar -> Doc
    enrichPosParamsStyled _ _ _ PosNIL = empty
    enrichPosParamsStyled useBold useColor posRow (PosPar n t e p) =
        -- Skip witness parameters
        if isWitnessParam n
        then enrichPosParamsStyled useBold useColor (advanceRow posRow) p
        else
            let inferredType = extractParamTypeFromRow (nstr n) posRow
                paramType = case inferredType of
                    Just it -> Just it
                    Nothing -> t
                param = pretty n <> formatTypeStyled useBold useColor paramType <> formatDefault e
                nextParams = enrichPosParamsStyled useBold useColor (advanceRow posRow) p
            in case p of
                PosNIL -> param
                _ -> if isEmpty nextParams
                     then param
                     else param <> comma <+> nextParams
    enrichPosParamsStyled useBold useColor _ (PosSTAR n t) =
        text "*" <> pretty n <> formatTypeStyled useBold useColor t

    enrichKwdParamsStyled :: Bool -> Bool -> KwdRow -> KwdPar -> Doc
    enrichKwdParamsStyled _ _ _ KwdNIL = empty
    enrichKwdParamsStyled useBold useColor kwdRow (KwdPar n t e k) =
        let inferredType = extractParamTypeFromRow (nstr n) kwdRow
            paramType = case inferredType of
                Just it -> Just it
                Nothing -> t
            param = pretty n <> formatTypeStyled useBold useColor paramType <> formatDefault e
        in case k of
            KwdNIL -> param
            _ -> param <> comma <+> enrichKwdParamsStyled useBold useColor kwdRow k
    enrichKwdParamsStyled useBold useColor _ (KwdSTAR n t) =
        text "**" <> pretty n <> formatTypeStyled useBold useColor t

    extractParamTypeFromRow :: String -> Type -> Maybe Type
    extractParamTypeFromRow _ (TNil _ _) = Nothing
    extractParamTypeFromRow name (TRow _ _ n t rest)
        | nstr n == name = Just t
        | otherwise = extractParamTypeFromRow name rest
    extractParamTypeFromRow _ _ = Nothing

    advanceRow :: Type -> Type
    advanceRow (TRow _ _ _ _ rest) = rest
    advanceRow t = t
docMethodStyledWithTypes _ _ _ _ = empty

-- | Format parameters for ASCII
docParamsAscii :: PosPar -> KwdPar -> Doc
docParamsAscii p k = parens $ docPosParAscii p <> docKwdParSepAscii p k <> docKwdParAscii k

docKwdParSepAscii :: PosPar -> KwdPar -> Doc
docKwdParSepAscii PosNIL KwdNIL = empty
docKwdParSepAscii PosNIL _ = empty
docKwdParSepAscii _ KwdNIL = empty
docKwdParSepAscii _ _ = text ", "

docPosParAscii :: PosPar -> Doc
docPosParAscii PosNIL = empty
docPosParAscii (PosPar n t e p) =
    let param = pretty n <> formatTypeAscii t <> formatDefault e
    in case p of
        PosNIL -> param
        _ -> param <> comma <+> docPosParAscii p
docPosParAscii (PosSTAR n t) = text "*" <> pretty n <> formatTypeAscii t

docKwdParAscii :: KwdPar -> Doc
docKwdParAscii KwdNIL = empty
docKwdParAscii (KwdPar n t e k) =
    let param = pretty n <> formatTypeAscii t <> formatDefault e
    in case k of
        KwdNIL -> param
        _ -> param <> comma <+> docKwdParAscii k
docKwdParAscii (KwdSTAR n t) = text "**" <> pretty n <> formatTypeAscii t

formatTypeAscii :: Maybe Type -> Doc
formatTypeAscii Nothing = empty
formatTypeAscii (Just t) = colon <+> pretty (SimplifiedType t)

docRetTypeAscii :: Maybe Type -> Doc
docRetTypeAscii Nothing = empty
docRetTypeAscii (Just t) = text " -> " <> pretty (SimplifiedType t)


-- | Print documentation as HTML with type information
printHtmlDoc :: NameInfo -> Module -> String
printHtmlDoc nmod m = unlines
        [ "<!DOCTYPE html>"
        , "<html lang=\"en\">"
        , "<head>"
        , "  <meta charset=\"UTF-8\">"
        , "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">"
        , "  <title>" ++ moduleTitle m ++ "</title>"
        , "  <style>"
        , htmlStyles
        , "  </style>"
        , "  <script>"
        , htmlScript
        , "  </script>"
        , "</head>"
        , "<body>"
        , "  <div class=\"container\">"
        , render (docModuleHtmlWithTypes nmod m)
        , "  </div>"
        , "</body>"
        , "</html>"
        ]

-- | Get module title for HTML
moduleTitle :: Module -> String
moduleTitle (Module qn _ _) = render (pretty qn)

-- | CSS styles for HTML documentation
htmlStyles :: String
htmlStyles = unlines
    [ "    /* Light theme colors (default) */"
    , "    :root {"
    , "      --text-primary: #24292e;"
    , "      --text-secondary: #6a737d;"
    , "      --text-link: #0366d6;"
    , "      --bg-primary: #ffffff;"
    , "      --bg-secondary: #f6f8fa;"
    , "      --border: #e1e4e8;"
    , "      --code-bg: #f6f8fa;"
    , "      --type-color: #6f42c1;"
    , "      --keyword-color: #d73a49;"
    , "      --generic-color: #e36209;"
    , "      --generic-hover: #fb8532;"
    , "      --shadow: rgba(27, 31, 35, 0.04);"
    , "      --shadow-medium: rgba(27, 31, 35, 0.12);"
    , "    }"
    , "    "
    , "    /* Dark theme colors (activated by OS preference) */"
    , "    @media (prefers-color-scheme: dark) {"
    , "      :root {"
    , "        --text-primary: #c9d1d9;"
    , "        --text-secondary: #8b949e;"
    , "        --text-link: #58a6ff;"
    , "        --bg-primary: #0d1117;"
    , "        --bg-secondary: #161b22;"
    , "        --border: #30363d;"
    , "        --code-bg: #161b22;"
    , "        --type-color: #d2a8ff;"
    , "        --keyword-color: #ff7b72;"
    , "        --generic-color: #ffa657;"
    , "        --generic-hover: #ffb77c;"
    , "        --shadow: rgba(0, 0, 0, 0.3);"
    , "        --shadow-medium: rgba(0, 0, 0, 0.5);"
    , "      }"
    , "    }"
    , "    "
    , "    * { box-sizing: border-box; }"
    , "    "
    , "    body {"
    , "      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Helvetica, Arial, sans-serif;"
    , "      font-size: 16px;"
    , "      line-height: 1.5;"
    , "      color: var(--text-primary);"
    , "      background: var(--bg-primary);"
    , "      margin: 0;"
    , "      padding: 0;"
    , "    }"
    , "    "
    , "    .container {"
    , "      max-width: 960px;"
    , "      margin: 0 auto;"
    , "      padding: 2rem;"
    , "    }"
    , "    "
    , "    /* Typography */"
    , "    h1 {"
    , "      font-size: 2rem;"
    , "      font-weight: 600;"
    , "      margin: 0 0 1rem 0;"
    , "      padding-bottom: 0.3rem;"
    , "      border-bottom: 1px solid var(--border);"
    , "    }"
    , "    "
    , "    h2 {"
    , "      font-size: 1.25rem;"
    , "      font-weight: 600;"
    , "      margin: 2.5rem 0 1rem 0;"
    , "    }"
    , "    "
    , "    h3 {"
    , "      font-size: 1rem;"
    , "      font-weight: 600;"
    , "      color: var(--text-secondary);"
    , "      margin: 1.5rem 0 0.5rem 0;"
    , "      text-transform: uppercase;"
    , "      letter-spacing: 0.02em;"
    , "    }"
    , "    "
    , "    /* Indent ATTRIBUTES/METHODS headers under classes/actors */"
    , "    .declaration-block h3 {"
    , "      margin-left: 2rem;"
    , "    }"
    , "    "
    , "    p {"
    , "      margin: 0 0 1rem 0;"
    , "    }"
    , "    "
    , "    /* Code */"
    , "    code {"
    , "      font-family: 'SF Mono', Monaco, Consolas, 'Liberation Mono', 'Courier New', monospace;"
    , "      font-size: 0.875em;"
    , "      background: var(--code-bg);"
    , "      padding: 0.2em 0.4em;"
    , "      border-radius: 3px;"
    , "      border: 1px solid var(--border);"
    , "    }"
    , "    "
    , "    h2 code {"
    , "      background: none;"
    , "      padding: 0;"
    , "      font-size: 1em;"
    , "      border: none;"
    , "    }"
    , "    "
    , "    /* Type coloring */"
    , "    .type {"
    , "      color: var(--type-color);"
    , "    }"
    , "    "
    , "    /* Type links */"
    , "    .type-link {"
    , "      color: var(--type-color);"
    , "      text-decoration: none;"
    , "      border-bottom: 1px dotted var(--type-color);"
    , "    }"
    , "    "
    , "    .type-link:hover {"
    , "      text-decoration: none;"
    , "      border-bottom: 1px solid var(--type-color);"
    , "    }"
    , "    "
    , "    .keyword {"
    , "      color: var(--keyword-color);"
    , "      font-weight: 500;"
    , "      font-size: 1rem;"
    , "    }"
    , "    "
    , "    .param-name {"
    , "      font-weight: 500;"
    , "    }"
    , "    "
    , "    .default-value {"
    , "      color: var(--text-secondary);"
    , "    }"
    , "    "
    , "    /* Generic type parameters */"
    , "    .generic-type {"
    , "      color: var(--generic-color);"
    , "      cursor: pointer;"
    , "      transition: color 0.2s ease;"
    , "      position: relative;"
    , "      display: inline-block;"
    , "    }"
    , "    "
    , "    .generic-type:hover,"
    , "    .generic-type.highlight {"
    , "      color: var(--generic-hover);"
    , "      text-decoration: underline;"
    , "      text-decoration-style: dotted;"
    , "    }"
    , "    "
    , "    /* Tooltip for generic types - default position below */"
    , "    .generic-type[data-tooltip]:hover::after {"
    , "      content: attr(data-tooltip);"
    , "      display: block;"
    , "      position: absolute;"
    , "      top: 100%;"
    , "      left: 50%;"
    , "      transform: translateX(-50%);"
    , "      margin-top: 5px;"
    , "      padding: 8px 12px;"
    , "      background-color: rgba(0, 0, 0, 0.8);"
    , "      color: white;"
    , "      font-size: 0.75rem;"
    , "      font-weight: normal;"
    , "      white-space: pre-wrap;"
    , "      max-width: 700px;"
    , "      min-width: 200px;"
    , "      text-align: left;"
    , "      border-radius: 4px;"
    , "      pointer-events: none;"
    , "      opacity: 0;"
    , "      animation: fadeIn 0.2s ease-in-out forwards;"
    , "      z-index: 1000;"
    , "    }"
    , "    "
    , "    /* Tooltip positioned above when not near top */"
    , "    .generic-type[data-tooltip]:not(.tooltip-below):hover::after {"
    , "      top: auto;"
    , "      bottom: 100%;"
    , "      margin-top: 0;"
    , "      margin-bottom: 5px;"
    , "    }"
    , "    "
    , "    @keyframes fadeIn {"
    , "      from { opacity: 0; }"
    , "      to { opacity: 1; }"
    , "    }"
    , "    "
    , "    /* Documentation strings */"
    , "    .docstring {"
    , "      color: var(--text-secondary);"
    , "      margin: 0.5rem 0 1.5rem 0;"
    , "      padding-left: 1rem;"
    , "    }"
    , "    "
    , "    .module-doc {"
    , "      margin: 1rem 0 2rem 0;"
    , "      color: var(--text-primary);"
    , "      font-size: 1.1rem;"
    , "      line-height: 1.6;"
    , "    }"
    , "    "
    , "    /* Sections */"
    , "    .section {"
    , "      margin: 1.5rem 0;"
    , "      padding-left: 2rem;"
    , "    }"
    , "    "
    , "    /* Lists */"
    , "    ul {"
    , "      list-style: none;"
    , "      padding: 0;"
    , "      margin: 0;"
    , "    }"
    , "    "
    , "    li {"
    , "      margin: 0.5rem 0;"
    , "    }"
    , "    "
    , "    /* Attribute items */"
    , "    .attribute-item {"
    , "      margin: 0.5rem 0;"
    , "      padding-left: 1rem;"
    , "      border-left: 3px solid var(--border);"
    , "    }"
    , "    "
    , "    .attribute-item code {"
    , "      background: none;"
    , "      padding: 0;"
    , "      border: none;"
    , "    }"
    , "    "
    , "    /* Method items */"
    , "    .method-item {"
    , "      margin: 1rem 0;"
    , "    }"
    , "    "
    , "    .method-signature {"
    , "      font-family: 'SF Mono', Monaco, Consolas, monospace;"
    , "      font-size: 0.875rem;"
    , "      padding-left: 1rem;"
    , "      border-left: 3px solid var(--border);"
    , "    }"
    , "    "
    , "    .method-signature code {"
    , "      background: none;"
    , "      padding: 0;"
    , "      border: none;"
    , "    }"
    , "    "
    , "    .method-doc {"
    , "      color: var(--text-secondary);"
    , "      margin: 0.25rem 0 0 1.5rem;"
    , "      font-size: 0.875rem;"
    , "    }"
    , "    "
    , "    /* Attributes and methods sections */"
    , "    .attributes, .methods {"
    , "      margin: 1.5rem 0 1.5rem 2rem;"
    , "    }"
    , "    "
    , "    /* Declaration blocks - no visual boxes, just spacing */"
    , "    .declaration-block {"
    , "      margin: 2rem 0;"
    , "    }"
    , "    "
    , "    /* Module index styles */"
    , "    .module-list {"
    , "      list-style: none;"
    , "      padding: 0;"
    , "      margin: 0;"
    , "    }"
    , "    "
    , "    .module-item {"
    , "      margin: 0.5rem 0;"
    , "      padding: 0.75rem 1rem;"
    , "      background: var(--bg-secondary);"
    , "      border-radius: 6px;"
    , "      border: 1px solid var(--border);"
    , "      transition: all 0.2s ease;"
    , "    }"
    , "    "
    , "    .module-item:hover {"
    , "      border-color: var(--text-link);"
    , "      transform: translateY(-1px);"
    , "    }"
    , "    "
    , "    .module-link {"
    , "      text-decoration: none;"
    , "      color: var(--text-link);"
    , "      font-weight: 500;"
    , "      display: block;"
    , "    }"
    , "    "
    , "    .module-path {"
    , "      font-family: 'SF Mono', Monaco, Consolas, monospace;"
    , "      font-size: 0.875rem;"
    , "    }"
    , "    "
    , "    .module-doc {"
    , "      color: var(--text-secondary);"
    , "      font-size: 0.875rem;"
    , "      margin-top: 0.25rem;"
    , "    }"
    , "    "
    , "    /* Smooth transitions for theme changes */"
    , "    * {"
    , "      transition: background-color 0.3s ease, color 0.3s ease, border-color 0.3s ease;"
    , "    }"
    , "    "
    , "    /* Responsive */"
    , "    @media (max-width: 768px) {"
    , "      .container {"
    , "        padding: 1rem;"
    , "      }"
    , "      h1 {"
    , "        font-size: 1.75rem;"
    , "      }"
    , "      h2 {"
    , "        font-size: 1.25rem;"
    , "      }"
    , "    }"
    ]

-- | JavaScript for generic type hover effects
htmlScript :: String
htmlScript = unlines
    [ "    document.addEventListener('DOMContentLoaded', function() {"
    , "      const genericTypes = document.querySelectorAll('.generic-type');"
    , "      "
    , "      genericTypes.forEach(function(elem) {"
    , "        elem.addEventListener('mouseenter', function() {"
    , "          const typeName = elem.getAttribute('data-generic');"
    , "          "
    , "          // Check if tooltip would go off-screen at the top"
    , "          const rect = elem.getBoundingClientRect();"
    , "          const tooltipHeight = 200; // Approximate height"
    , "          if (rect.top < tooltipHeight) {"
    , "            elem.classList.add('tooltip-below');"
    , "          } else {"
    , "            elem.classList.remove('tooltip-below');"
    , "          }"
    , "          "
    , "          // Get the scope attribute - all generic types should have this"
    , "          const scope = elem.getAttribute('data-scope');"
    , "          "
    , "          // Only highlight elements with same type name AND same scope"
    , "          document.querySelectorAll('.generic-type[data-generic=\"' + typeName + '\"][data-scope=\"' + scope + '\"]').forEach(function(t) {"
    , "            t.classList.add('highlight');"
    , "          });"
    , "        });"
    , "        "
    , "        elem.addEventListener('mouseleave', function() {"
    , "          const typeName = elem.getAttribute('data-generic');"
    , "          document.querySelectorAll('.generic-type[data-generic=\"' + typeName + '\"].highlight').forEach(function(t) {"
    , "            t.classList.remove('highlight');"
    , "          });"
    , "        });"
    , "      });"
    , "    });"
    ]


-- | Generate HTML documentation from a module with type information
docModuleHtmlWithTypes :: NameInfo -> Module -> Doc
docModuleHtmlWithTypes (NModule tenv mdocstring) (Module modName _ stmts) =
    -- Use module docstring from NModule
    let moduleDocstring = mdocstring
        (title, restDoc) = case moduleDocstring of
            Just ds -> splitDocstring ds
            Nothing -> ("", Nothing)
        header = text "<h1>" <> text (render (pretty modName)) <>
                 (if null title then empty else text " - " <> text (htmlEscape title)) <> text "</h1>"
        bodyDoc = case restDoc of
            Just body -> text "<div class=\"module-doc\">" <> text (nl2br $ htmlEscape body) <> text "</div>"
            Nothing -> empty
        -- Collect class info from both local module and type environment
        classInfos = collectClassInfos modName tenv stmts
    in header $+$ bodyDoc $+$ docTopLevelHtmlWithTypesAndClassInfo tenv modName classInfos stmts
  where
    nl2br = intercalate "<br>" . lines

-- | Collect class information from local module and imported modules
collectClassInfos :: ModName -> TEnv -> Suite -> Set ClassInfo
collectClassInfos currentModule tenv stmts =
    let localClasses = Set.map (\n -> ClassInfo n currentModule True) (collectClassNames stmts)
        importedClasses = Set.fromList $ concatMap extractClassFromTEnv tenv
    in Set.union localClasses importedClasses
  where
    extractClassFromTEnv (n, NClass _ _ _ _) = [ClassInfo n currentModule False]  -- Mark as imported
    extractClassFromTEnv _ = []

-- | Collect all class names defined in the module
collectClassNames :: Suite -> Set Name
collectClassNames stmts = Set.fromList $ concatMap extractClassNames stmts
  where
    extractClassNames :: Stmt -> [Name]
    extractClassNames (Decl _ decls) = concatMap extractClassNamesFromDecl decls
    extractClassNames (With _ _ body) = concatMap extractClassNames body
    extractClassNames _ = []

    extractClassNamesFromDecl :: Decl -> [Name]
    extractClassNamesFromDecl (Class _ n _ _ _ _) = [n]
    extractClassNamesFromDecl _ = []

-- | Data type to track class locations for cross-module linking
data ClassInfo = ClassInfo
    { ciName :: Name
    , ciModule :: ModName
    , ciIsLocal :: Bool  -- True if in current module
    } deriving (Eq, Ord, Show)

-- | Build URL for a class reference
buildClassUrl :: ModName -> ClassInfo -> String
buildClassUrl currentModule ci
    | ciIsLocal ci = "#class-" ++ render (pretty (ciName ci))
    | otherwise =
        let targetPath = modPath (ciModule ci)
            currentPath = modPath currentModule
            relativePath = computeRelativePath currentPath targetPath
        in relativePath ++ "#class-" ++ render (pretty (ciName ci))
  where
    -- Compute relative path from current module to target module
    computeRelativePath :: [String] -> [String] -> String
    computeRelativePath current target =
        let upLevels = length current - 1  -- -1 because we're in a file, not a directory
            prefix = concat (replicate upLevels "../")
            targetFile = intercalate "/" target ++ ".html"
        in if upLevels == 0 then targetFile else prefix ++ targetFile

-- | Compute relative path between two module paths
computeRelativeModulePath :: ModName -> ModName -> String
computeRelativeModulePath currentModule targetModule =
    let currentPath = modPath currentModule
        targetPath = modPath targetModule
        -- Number of directories to go up from current module
        -- For modules in the root (length 1), we don't go up any levels
        -- For nested modules, we go up (length - 1) levels
        upLevels = if length currentPath > 1 then length currentPath - 1 else 0
        prefix = concat (replicate upLevels "../")
        targetFile = if null targetPath
                     then "index.html"  -- Root module
                     else intercalate "/" targetPath ++ ".html"
    in if upLevels == 0 then targetFile else prefix ++ targetFile

-- | Extract and document top-level definitions in HTML
docTopLevelHtml :: Suite -> Doc
docTopLevelHtml = docTopLevelHtmlWithTypes []

-- | Extract and document top-level definitions in HTML with types
docTopLevelHtmlWithTypes :: TEnv -> Suite -> Doc
docTopLevelHtmlWithTypes tenv stmts = docTopLevelHtmlWithTypesAndClasses tenv Set.empty stmts

-- | Extract and document top-level definitions in HTML with types and class links
docTopLevelHtmlWithTypesAndClasses :: TEnv -> Set Name -> Suite -> Doc
docTopLevelHtmlWithTypesAndClasses tenv classNames stmts =
    let docs = concatMap (extractTopLevelHtmlWithTypesAndClasses tenv classNames) stmts
        separated = case docs of
            [] -> []
            [d] -> [d]
            (d:ds) -> d : map (blank $+$) ds
    in vcat separated

-- | Extract and document top-level definitions in HTML with types and class info
docTopLevelHtmlWithTypesAndClassInfo :: TEnv -> ModName -> Set ClassInfo -> Suite -> Doc
docTopLevelHtmlWithTypesAndClassInfo tenv currentModule classInfos stmts =
    let docs = concatMap (extractTopLevelHtmlWithTypesAndClassInfo tenv currentModule classInfos) stmts
        separated = case docs of
            [] -> []
            [d] -> [d]
            (d:ds) -> d : map (blank $+$) ds
    in vcat separated

-- | Extract documentation-worthy top-level statements for HTML
extractTopLevelHtml :: Stmt -> [Doc]
extractTopLevelHtml = extractTopLevelHtmlWithTypes []

-- | Extract documentation-worthy top-level statements for HTML with types
extractTopLevelHtmlWithTypes :: TEnv -> Stmt -> [Doc]
extractTopLevelHtmlWithTypes tenv (Decl _ decls) = map (docDeclHtmlWithTypes tenv) decls
extractTopLevelHtmlWithTypes tenv (With _ _ body) = concatMap (extractTopLevelHtmlWithTypes tenv) body
extractTopLevelHtmlWithTypes _ _ = []

-- | Extract documentation-worthy top-level statements for HTML with types and class links
extractTopLevelHtmlWithTypesAndClasses :: TEnv -> Set Name -> Stmt -> [Doc]
extractTopLevelHtmlWithTypesAndClasses tenv classNames (Decl _ decls) = map (docDeclHtmlWithTypesAndClasses tenv classNames) decls
extractTopLevelHtmlWithTypesAndClasses tenv classNames (With _ _ body) = concatMap (extractTopLevelHtmlWithTypesAndClasses tenv classNames) body
extractTopLevelHtmlWithTypesAndClasses _ _ _ = []

-- | Extract documentation-worthy top-level statements for HTML with types and class info
extractTopLevelHtmlWithTypesAndClassInfo :: TEnv -> ModName -> Set ClassInfo -> Stmt -> [Doc]
extractTopLevelHtmlWithTypesAndClassInfo tenv currentModule classInfos (Decl _ decls) =
    map (docDeclHtmlWithTypesAndClassInfo tenv currentModule classInfos) decls
extractTopLevelHtmlWithTypesAndClassInfo tenv currentModule classInfos (With _ _ body) =
    concatMap (extractTopLevelHtmlWithTypesAndClassInfo tenv currentModule classInfos) body
extractTopLevelHtmlWithTypesAndClassInfo _ _ _ _ = []

-- | Escape HTML special characters
htmlEscape :: String -> String
htmlEscape [] = []
htmlEscape ('<':xs) = "&lt;" ++ htmlEscape xs
htmlEscape ('>':xs) = "&gt;" ++ htmlEscape xs
htmlEscape ('&':xs) = "&amp;" ++ htmlEscape xs
htmlEscape ('"':xs) = "&quot;" ++ htmlEscape xs
htmlEscape ('\'':xs) = "&#39;" ++ htmlEscape xs
htmlEscape (x:xs) = x : htmlEscape xs

-- | Extract generic type names from QBinds
extractGenerics :: QBinds -> Set Name
extractGenerics binds = Set.fromList [tvname tv | QBind tv _ <- binds]

-- | Check if a parameter name is a witness parameter
isWitnessParam :: Name -> Bool
isWitnessParam n = case nstr n of
    'W':'_':rest -> all isDigit rest && not (null rest)
    _ -> False
  where
    isDigit c = c >= '0' && c <= '9'

-- | Simplify a qualified name by removing builtin module prefix
simplifyQName :: QName -> QName
simplifyQName (GName m n) | modPath m == ["__builtin__"] = NoQ n
simplifyQName qn = qn

-- | Wrapper for simplified type pretty printing
newtype SimplifiedType = SimplifiedType Type

instance Pretty SimplifiedType where
    pretty (SimplifiedType t) = prettySimplifiedType t

-- | Pretty print a type with simplified qualified names
prettySimplifiedType :: Type -> Doc
prettySimplifiedType (TVar _ tv) = pretty tv
prettySimplifiedType (TCon _ tc) = prettySimplifiedTCon tc
prettySimplifiedType (TFun _ _ posrow kwdrow restype) =
    let args = prettySimplifiedFunArgs posrow kwdrow
    in if isEmpty args
       then prettySimplifiedType restype
       else args <+> text "->" <+> prettySimplifiedType restype
prettySimplifiedType (TTuple _ posrow kwdrow) =
    parens (prettySimplifiedTupleArgs posrow kwdrow)
prettySimplifiedType (TOpt _ t) = text "?" <> prettySimplifiedType t
prettySimplifiedType (TNone _) = text "None"
prettySimplifiedType (TWild _) = text "_"
prettySimplifiedType (TNil _ _) = empty
prettySimplifiedType (TRow _ _ label rtype rtail) =
    pretty label <> colon <+> prettySimplifiedType rtype <>
    case rtail of
        TNil _ _ -> empty
        _ -> comma <+> prettySimplifiedType rtail
prettySimplifiedType (TStar _ _ rtail) = text "*" <> prettySimplifiedType rtail
prettySimplifiedType (TFX _ fx) = pretty fx

prettySimplifiedTCon :: TCon -> Doc
prettySimplifiedTCon (TC qn ts)
    | qn == qnList && length ts == 1 = text "list" <> brackets (prettySimplifiedType (head ts))
    | qn == qnDict && length ts == 2 = text "dict" <> brackets (prettySimplifiedType (head ts) <> comma <+> prettySimplifiedType (ts !! 1))
    | qn == qnSetT && length ts == 1 = text "set" <> brackets (prettySimplifiedType (head ts))
    | otherwise = pretty (simplifyQName qn) <>
        if null ts then empty else brackets (hcat $ punctuate comma [pretty (SimplifiedType t) | t <- ts])
  where
    hcat [] = empty
    hcat [x] = x
    hcat (x:xs) = x <> hcat xs

prettySimplifiedFunArgs :: PosRow -> KwdRow -> Doc
prettySimplifiedFunArgs posrow kwdrow =
    let posArgs = prettySimplifiedPosRow posrow
        kwdArgs = prettySimplifiedKwdRow kwdrow
    in case (isEmpty posArgs, isEmpty kwdArgs) of
        (True, True) -> empty
        (False, True) -> parens posArgs
        (True, False) -> parens kwdArgs
        (False, False) -> parens (posArgs <> comma <+> kwdArgs)

prettySimplifiedTupleArgs :: PosRow -> KwdRow -> Doc
prettySimplifiedTupleArgs posrow kwdrow =
    let posArgs = prettySimplifiedPosRow posrow
        kwdArgs = prettySimplifiedKwdRow kwdrow
    in case (isEmpty posArgs, isEmpty kwdArgs) of
        (True, True) -> empty
        (False, True) -> posArgs
        (True, False) -> kwdArgs
        (False, False) -> posArgs <> comma <+> kwdArgs

prettySimplifiedPosRow :: PosRow -> Doc
prettySimplifiedPosRow (TNil _ _) = empty
prettySimplifiedPosRow (TRow _ _ _ rtype rtail) =
    prettySimplifiedType rtype <>
    case rtail of
        TNil _ _ -> empty
        _ -> comma <+> prettySimplifiedPosRow rtail
prettySimplifiedPosRow (TStar _ _ _) = text "*args"
prettySimplifiedPosRow t = prettySimplifiedType t

prettySimplifiedKwdRow :: KwdRow -> Doc
prettySimplifiedKwdRow (TNil _ _) = empty
prettySimplifiedKwdRow (TRow _ _ label rtype rtail) =
    pretty label <> colon <+> prettySimplifiedType rtype <>
    case rtail of
        TNil _ _ -> empty
        _ -> comma <+> prettySimplifiedKwdRow rtail
prettySimplifiedKwdRow (TStar _ _ _) = text "**kwargs"
prettySimplifiedKwdRow t = prettySimplifiedType t

-- | Render type with generic type highlighting
renderTypeWithGenerics :: Set Name -> Type -> String
renderTypeWithGenerics generics t = renderTypeWithGenericsAndConstraints generics [] t

-- | Render type with generic type highlighting and constraints
renderTypeWithGenericsAndConstraints :: Set Name -> QBinds -> Type -> String
renderTypeWithGenericsAndConstraints generics constraints t = renderTypeHtml generics constraints t
  where
    renderTypeHtml :: Set Name -> QBinds -> Type -> String
    renderTypeHtml gens cons (TVar _ tv)
        | Set.member (tvname tv) gens =
            let nameStr = render (pretty (tvname tv))
                -- Find constraints for this type variable
                tvConstraints = [preds | QBind qtv preds <- cons, tvname qtv == tvname tv]
                tooltip = case tvConstraints of
                    [] -> "Generic type " ++ nameStr ++ "\n\nThis is a placeholder for any type.\nAll " ++
                          nameStr ++ "s must be the same type."
                    (preds:_) -> if null preds
                        then "Generic type " ++ nameStr ++ "\n\nThis is a placeholder for any type.\nAll " ++
                             nameStr ++ "s must be the same type."
                        else "Generic type " ++ nameStr ++ "\n\nThis is a placeholder for any type.\nAll " ++
                             nameStr ++ "s must be the same type.\n\n" ++ nameStr ++ " must support:\n" ++
                             concatMap formatPredAsListItem preds
                formatPredAsListItem p = "  • " ++ formatConstraintNameLocal p ++ " protocol (" ++ formatExampleForPredLocal p ++ ")\n"
                formatConstraintNameLocal (TC qn _) = render . pretty . simplifyQName $ qn
                formatExampleForPredLocal (TC (NoQ (Name _ "Plus")) _) = "+ operator"
                formatExampleForPredLocal (TC (NoQ (Name _ "Minus")) _) = "- operator"
                formatExampleForPredLocal (TC (NoQ (Name _ "Times")) _) = "* operator"
                formatExampleForPredLocal (TC (NoQ (Name _ "Divide")) _) = "/ operator"
                formatExampleForPredLocal (TC (NoQ (Name _ "Eq")) _) = "== operator"
                formatExampleForPredLocal (TC (NoQ (Name _ "Ord")) _) = "< > operators"
                formatExampleForPredLocal (TC (NoQ (Name _ "Hash")) _) = "hash function"
                formatExampleForPredLocal _ = "protocol methods"
            in "<span class=\"generic-type\" data-generic=\"" ++ nameStr ++
               "\" data-tooltip=\"" ++ tooltip ++ "\">" ++ nameStr ++ "</span>"
        | otherwise =
            let simplified = render (pretty (SimplifiedType (TVar NoLoc tv)))
            in if simplified == render (pretty (tvname tv))
               then simplified  -- Already simplified
               else simplified
    renderTypeHtml gens cons (TCon _ tc) = renderTConHtmlSimplified gens cons tc
    renderTypeHtml gens cons (TFun _ _ posrow kwdrow restype) =
        let args = renderFunArgs gens cons posrow kwdrow
        in if null args
           then renderTypeHtml gens cons restype
           else args ++ " -> " ++ renderTypeHtml gens cons restype
    renderTypeHtml gens cons (TTuple _ posrow kwdrow) =
        "(" ++ renderTupleArgs gens cons posrow kwdrow ++ ")"
    renderTypeHtml gens cons (TOpt _ t) = "?" ++ renderTypeHtml gens cons t
    renderTypeHtml gens _ (TNone _) = "None"
    renderTypeHtml gens _ (TWild _) = "_"
    renderTypeHtml gens _ (TNil _ _) = ""
    renderTypeHtml gens cons (TRow _ _ label rtype rtail) =
        render (pretty label) ++ ": " ++ renderTypeHtml gens cons rtype ++
        case rtail of
            TNil _ _ -> ""
            _ -> ", " ++ renderTypeHtml gens cons rtail
    renderTypeHtml gens cons (TStar _ _ rtail) = "*" ++ renderTypeHtml gens cons rtail
    renderTypeHtml gens _ (TFX _ fx) = render (pretty fx)
    renderTypeHtml gens _ t = render (pretty (SimplifiedType t))  -- Fallback for any other type

    renderTConHtml :: Set Name -> QBinds -> TCon -> String
    renderTConHtml gens cons (TC qn ts)
        | qn == qnList && length ts == 1 = "[" ++ renderTypeHtml gens cons (head ts) ++ "]"
        | qn == qnDict && length ts == 2 = "{" ++ renderTypeHtml gens cons (head ts) ++ ": " ++ renderTypeHtml gens cons (ts !! 1) ++ "}"
        | qn == qnSetT && length ts == 1 = "{" ++ renderTypeHtml gens cons (head ts) ++ "}"
        | otherwise = render (pretty (simplifyQName qn)) ++
            if null ts then "" else "[" ++ intercalate ", " (map (renderTypeHtml gens cons) ts) ++ "]"

    renderTConHtmlSimplified :: Set Name -> QBinds -> TCon -> String
    renderTConHtmlSimplified gens cons (TC qn ts)
        | qn == qnList && length ts == 1 = "[" ++ renderTypeHtml gens cons (head ts) ++ "]"
        | qn == qnDict && length ts == 2 = "{" ++ renderTypeHtml gens cons (head ts) ++ ": " ++ renderTypeHtml gens cons (ts !! 1) ++ "}"
        | qn == qnSetT && length ts == 1 = "{" ++ renderTypeHtml gens cons (head ts) ++ "}"
        | otherwise = render (pretty (SimplifiedType (TCon NoLoc (TC qn ts))))

    renderFunArgs :: Set Name -> QBinds -> PosRow -> KwdRow -> String
    renderFunArgs gens cons posrow kwdrow =
        let posArgs = renderPosRow gens cons posrow
            kwdArgs = renderKwdRow gens cons kwdrow
        in case (posArgs, kwdArgs) of
            ("", "") -> ""
            (p, "") -> "(" ++ p ++ ")"
            ("", k) -> "(" ++ k ++ ")"
            (p, k) -> "(" ++ p ++ ", " ++ k ++ ")"

    renderTupleArgs :: Set Name -> QBinds -> PosRow -> KwdRow -> String
    renderTupleArgs gens cons posrow kwdrow =
        let posArgs = renderPosRow gens cons posrow
            kwdArgs = renderKwdRow gens cons kwdrow
        in case (posArgs, kwdArgs) of
            ("", "") -> ""
            (p, "") -> p
            ("", k) -> k
            (p, k) -> p ++ ", " ++ k

    renderPosRow :: Set Name -> QBinds -> PosRow -> String
    renderPosRow gens _ (TNil _ _) = ""
    renderPosRow gens cons (TRow _ _ _ rtype rtail) =
        renderTypeHtml gens cons rtype ++
        case rtail of
            TNil _ _ -> ""
            _ -> ", " ++ renderPosRow gens cons rtail
    renderPosRow gens _ (TStar _ _ _) = "*args"
    renderPosRow gens cons t = renderTypeHtml gens cons t

    renderKwdRow :: Set Name -> QBinds -> KwdRow -> String
    renderKwdRow gens _ (TNil _ _) = ""
    renderKwdRow gens cons (TRow _ _ label rtype rtail) =
        render (pretty label) ++ "=" ++ renderTypeHtml gens cons rtype ++
        case rtail of
            TNil _ _ -> ""
            _ -> ", " ++ renderKwdRow gens cons rtail
    renderKwdRow gens _ (TStar _ _ _) = "**kwargs"
    renderKwdRow gens cons t = renderTypeHtml gens cons t

-- | Render type with generic type highlighting, constraints and class links (with current module for relative paths)
renderTypeWithGenericsConstraintsAndClassesAndModule :: ModName -> Set Name -> QBinds -> Set Name -> Type -> String
renderTypeWithGenericsConstraintsAndClassesAndModule currentModule generics constraints classNames t =
    renderTypeHtmlWithClassesAndModule currentModule generics constraints classNames t
  where
    renderTypeHtmlWithClassesAndModule :: ModName -> Set Name -> QBinds -> Set Name -> Type -> String
    renderTypeHtmlWithClassesAndModule curMod gens cons classes (TVar _ tv)
        | Set.member (tvname tv) gens =
            let nameStr = render (pretty (tvname tv))
                -- Find constraints for this type variable
                tvConstraints = [preds | QBind qtv preds <- cons, tvname qtv == tvname tv]
                tooltip = case tvConstraints of
                    [] -> "Generic type " ++ nameStr ++ "\n\nThis is a placeholder for any type.\nAll " ++
                          nameStr ++ "s must be the same type."
                    (preds:_) -> if null preds
                        then "Generic type " ++ nameStr ++ "\n\nThis is a placeholder for any type.\nAll " ++
                             nameStr ++ "s must be the same type."
                        else "Generic type " ++ nameStr ++ "\n\nThis is a placeholder for any type.\nAll " ++
                             nameStr ++ "s must be the same type.\n\n" ++ nameStr ++ " must support:\n" ++
                             concatMap formatPredAsListItem preds
                formatPredAsListItem p = "  • " ++ formatConstraintNameLocal p ++ " protocol (" ++ formatExampleForPredLocal p ++ ")\n"
                formatConstraintNameLocal (TC qn _) = render . pretty . simplifyQName $ qn
                formatExampleForPredLocal (TC (NoQ (Name _ "Plus")) _) = "+ operator"
                formatExampleForPredLocal (TC (NoQ (Name _ "Minus")) _) = "- operator"
                formatExampleForPredLocal (TC (NoQ (Name _ "Times")) _) = "* operator"
                formatExampleForPredLocal (TC (NoQ (Name _ "Divide")) _) = "/ operator"
                formatExampleForPredLocal (TC (NoQ (Name _ "Eq")) _) = "== operator"
                formatExampleForPredLocal (TC (NoQ (Name _ "Ord")) _) = "< > operators"
                formatExampleForPredLocal (TC (NoQ (Name _ "Hash")) _) = "hash function"
                formatExampleForPredLocal _ = "protocol methods"
            in "<span class=\"generic-type\" data-generic=\"" ++ nameStr ++
               "\" data-tooltip=\"" ++ tooltip ++ "\">" ++ nameStr ++ "</span>"
        | otherwise =
            let simplified = render (pretty (SimplifiedType (TVar NoLoc tv)))
            in if simplified == render (pretty (tvname tv))
               then simplified  -- Already simplified
               else simplified
    renderTypeHtmlWithClassesAndModule curMod gens cons classes (TCon _ tc) =
        renderTConHtmlSimplifiedWithClassesAndModule curMod gens cons classes tc
    renderTypeHtmlWithClassesAndModule curMod gens cons classes (TFun _ _ posrow kwdrow restype) =
        let args = renderFunArgsWithClassesAndModule curMod gens cons classes posrow kwdrow
        in if null args
           then renderTypeHtmlWithClassesAndModule curMod gens cons classes restype
           else args ++ " -> " ++ renderTypeHtmlWithClassesAndModule curMod gens cons classes restype
    renderTypeHtmlWithClassesAndModule curMod gens cons classes (TTuple _ posrow kwdrow) =
        "(" ++ renderTupleArgsWithClassesAndModule curMod gens cons classes posrow kwdrow ++ ")"
    renderTypeHtmlWithClassesAndModule curMod gens cons classes (TOpt _ t) =
        "?" ++ renderTypeHtmlWithClassesAndModule curMod gens cons classes t
    renderTypeHtmlWithClassesAndModule _ gens _ _ (TNone _) = "None"
    renderTypeHtmlWithClassesAndModule _ gens _ _ (TWild _) = "_"
    renderTypeHtmlWithClassesAndModule _ gens _ _ (TNil _ _) = ""
    renderTypeHtmlWithClassesAndModule curMod gens cons classes (TRow _ _ label rtype rtail) =
        render (pretty label) ++ ": " ++ renderTypeHtmlWithClassesAndModule curMod gens cons classes rtype ++
        case rtail of
            TNil _ _ -> ""
            _ -> ", " ++ renderTypeHtmlWithClassesAndModule curMod gens cons classes rtail
    renderTypeHtmlWithClassesAndModule curMod gens cons classes (TStar _ _ rtail) =
        "*" ++ renderTypeHtmlWithClassesAndModule curMod gens cons classes rtail
    renderTypeHtmlWithClassesAndModule _ gens _ _ (TFX _ fx) = render (pretty fx)
    renderTypeHtmlWithClassesAndModule curMod gens _ classes t =
        render (pretty (SimplifiedType t))  -- Fallback for any other type

    renderTConHtmlSimplifiedWithClassesAndModule :: ModName -> Set Name -> QBinds -> Set Name -> TCon -> String
    renderTConHtmlSimplifiedWithClassesAndModule curMod gens cons classes (TC qn ts)
        | qn == qnList && length ts == 1 = "[" ++ renderTypeHtmlWithClassesAndModule curMod gens cons classes (head ts) ++ "]"
        | qn == qnDict && length ts == 2 =
            "{" ++ renderTypeHtmlWithClassesAndModule curMod gens cons classes (head ts) ++ ": " ++
            renderTypeHtmlWithClassesAndModule curMod gens cons classes (ts !! 1) ++ "}"
        | qn == qnSetT && length ts == 1 = "{" ++ renderTypeHtmlWithClassesAndModule curMod gens cons classes (head ts) ++ "}"
        | otherwise =
            let simpleName = simplifyQName qn
                nameStr = render (pretty simpleName)
                -- Generate link based on qualified name with relative paths
                linkStr = case qn of
                    NoQ n -> if Set.member n classes
                            then "<a href=\"#class-" ++ nameStr ++ "\" class=\"type-link\">" ++ nameStr ++ "</a>"
                            else nameStr
                    QName m n ->
                        -- This is a qualified name from another module
                        let relativePath = computeRelativeModulePath curMod m
                            url = relativePath ++ "#class-" ++ render (pretty n)
                        in "<a href=\"" ++ url ++ "\" class=\"type-link\">" ++ nameStr ++ "</a>"
                    GName m n ->
                        -- Global name (e.g., from __builtin__)
                        let modPathList = modPath m
                        in if modPathList == ["__builtin__"]
                           then nameStr  -- Don't link builtin types
                           else
                               let relativePath = computeRelativeModulePath curMod m
                                   url = relativePath ++ "#class-" ++ render (pretty n)
                               in "<a href=\"" ++ url ++ "\" class=\"type-link\">" ++ nameStr ++ "</a>"
            in linkStr ++ if null ts then "" else "[" ++ intercalate ", " (map (renderTypeHtmlWithClassesAndModule curMod gens cons classes) ts) ++ "]"

    renderFunArgsWithClassesAndModule :: ModName -> Set Name -> QBinds -> Set Name -> PosRow -> KwdRow -> String
    renderFunArgsWithClassesAndModule curMod gens cons classes posrow kwdrow =
        let posArgs = renderPosRowWithClassesAndModule curMod gens cons classes posrow
            kwdArgs = renderKwdRowWithClassesAndModule curMod gens cons classes kwdrow
        in case (posArgs, kwdArgs) of
            ("", "") -> ""
            (p, "") -> "(" ++ p ++ ")"
            ("", k) -> "(" ++ k ++ ")"
            (p, k) -> "(" ++ p ++ ", " ++ k ++ ")"

    renderTupleArgsWithClassesAndModule :: ModName -> Set Name -> QBinds -> Set Name -> PosRow -> KwdRow -> String
    renderTupleArgsWithClassesAndModule curMod gens cons classes posrow kwdrow =
        let posArgs = renderPosRowWithClassesAndModule curMod gens cons classes posrow
            kwdArgs = renderKwdRowWithClassesAndModule curMod gens cons classes kwdrow
        in case (posArgs, kwdArgs) of
            ("", "") -> ""
            (p, "") -> p
            ("", k) -> k
            (p, k) -> p ++ ", " ++ k

    renderPosRowWithClassesAndModule :: ModName -> Set Name -> QBinds -> Set Name -> PosRow -> String
    renderPosRowWithClassesAndModule _ _ _ _ (TNil _ _) = ""
    renderPosRowWithClassesAndModule curMod gens cons classes (TRow _ _ _ rtype rtail) =
        renderTypeHtmlWithClassesAndModule curMod gens cons classes rtype ++
        case rtail of
            TNil _ _ -> ""
            _ -> ", " ++ renderPosRowWithClassesAndModule curMod gens cons classes rtail
    renderPosRowWithClassesAndModule _ _ _ _ (TStar _ _ _) = "*args"
    renderPosRowWithClassesAndModule curMod gens cons classes t = renderTypeHtmlWithClassesAndModule curMod gens cons classes t

    renderKwdRowWithClassesAndModule :: ModName -> Set Name -> QBinds -> Set Name -> KwdRow -> String
    renderKwdRowWithClassesAndModule _ _ _ _ (TNil _ _) = ""
    renderKwdRowWithClassesAndModule curMod gens cons classes (TRow _ _ label rtype rtail) =
        render (pretty label) ++ "=" ++ renderTypeHtmlWithClassesAndModule curMod gens cons classes rtype ++
        case rtail of
            TNil _ _ -> ""
            _ -> ", " ++ renderKwdRowWithClassesAndModule curMod gens cons classes rtail
    renderKwdRowWithClassesAndModule _ _ _ _ (TStar _ _ _) = "**kwargs"
    renderKwdRowWithClassesAndModule curMod gens cons classes t = renderTypeHtmlWithClassesAndModule curMod gens cons classes t

-- | Render type with generic type highlighting, constraints, class links and scope
renderTypeWithGenericsConstraintsClassesModuleAndScope :: ModName -> Set Name -> QBinds -> Set Name -> String -> Type -> String
renderTypeWithGenericsConstraintsClassesModuleAndScope currentModule generics constraints classNames scope t =
    renderTypeHtmlWithClassesModuleAndScope currentModule generics constraints classNames scope t
  where
    renderTypeHtmlWithClassesModuleAndScope :: ModName -> Set Name -> QBinds -> Set Name -> String -> Type -> String
    renderTypeHtmlWithClassesModuleAndScope curMod gens cons classes scope (TVar _ tv)
        | Set.member (tvname tv) gens =
            let nameStr = render (pretty (tvname tv))
                -- Find constraints for this type variable
                tvConstraints = [preds | QBind qtv preds <- cons, tvname qtv == tvname tv]
                tooltip = case tvConstraints of
                    [] -> "Generic type " ++ nameStr ++ "\n\nThis is a placeholder for any type.\nAll " ++
                          nameStr ++ "s must be the same type."
                    (preds:_) -> if null preds
                        then "Generic type " ++ nameStr ++ "\n\nThis is a placeholder for any type.\nAll " ++
                             nameStr ++ "s must be the same type."
                        else "Generic type " ++ nameStr ++ "\n\nThis is a placeholder for any type.\nAll " ++
                             nameStr ++ "s must be the same type.\n\n" ++ nameStr ++ " must support:\n" ++
                             concatMap formatPredAsListItem preds
                formatPredAsListItem p = "  • " ++ formatConstraintNameLocal p ++ " protocol (" ++ formatExampleForPredLocal p ++ ")\n"
                formatConstraintNameLocal (TC qn _) = render . pretty . simplifyQName $ qn
                formatExampleForPredLocal (TC (NoQ (Name _ "Plus")) _) = "+ operator"
                formatExampleForPredLocal (TC (NoQ (Name _ "Minus")) _) = "- operator"
                formatExampleForPredLocal (TC (NoQ (Name _ "Times")) _) = "* operator"
                formatExampleForPredLocal (TC (NoQ (Name _ "Divide")) _) = "/ operator"
                formatExampleForPredLocal (TC (NoQ (Name _ "Eq")) _) = "== operator"
                formatExampleForPredLocal (TC (NoQ (Name _ "Ord")) _) = "< > operators"
                formatExampleForPredLocal (TC (NoQ (Name _ "Hash")) _) = "hash function"
                formatExampleForPredLocal _ = "protocol methods"
            in "<span class=\"generic-type\" data-generic=\"" ++ nameStr ++
               "\" data-scope=\"" ++ scope ++
               "\" data-tooltip=\"" ++ tooltip ++ "\">" ++ nameStr ++ "</span>"
        | otherwise =
            let simplified = render (pretty (SimplifiedType (TVar NoLoc tv)))
            in if simplified == render (pretty (tvname tv))
               then simplified  -- Already simplified
               else simplified
    renderTypeHtmlWithClassesModuleAndScope curMod gens cons classes scope (TCon _ tc) =
        renderTConHtmlSimplifiedWithClassesModuleAndScope curMod gens cons classes scope tc
    renderTypeHtmlWithClassesModuleAndScope curMod gens cons classes scope (TFun _ _ posrow kwdrow restype) =
        let args = renderFunArgsWithClassesModuleAndScope curMod gens cons classes scope posrow kwdrow
        in if null args
           then renderTypeHtmlWithClassesModuleAndScope curMod gens cons classes scope restype
           else args ++ " -> " ++ renderTypeHtmlWithClassesModuleAndScope curMod gens cons classes scope restype
    renderTypeHtmlWithClassesModuleAndScope curMod gens cons classes scope (TTuple _ posrow kwdrow) =
        "(" ++ renderTupleArgsWithClassesModuleAndScope curMod gens cons classes scope posrow kwdrow ++ ")"
    renderTypeHtmlWithClassesModuleAndScope curMod gens cons classes scope (TOpt _ t) =
        "?" ++ renderTypeHtmlWithClassesModuleAndScope curMod gens cons classes scope t
    renderTypeHtmlWithClassesModuleAndScope _ _ _ _ _ (TNone _) = "None"
    renderTypeHtmlWithClassesModuleAndScope _ _ _ _ _ (TWild _) = "_"
    renderTypeHtmlWithClassesModuleAndScope _ _ _ _ _ (TNil _ _) = ""
    renderTypeHtmlWithClassesModuleAndScope curMod gens cons classes scope (TRow _ _ label rtype rtail) =
        render (pretty label) ++ ": " ++ renderTypeHtmlWithClassesModuleAndScope curMod gens cons classes scope rtype ++
        case rtail of
            TNil _ _ -> ""
            _ -> ", " ++ renderTypeHtmlWithClassesModuleAndScope curMod gens cons classes scope rtail
    renderTypeHtmlWithClassesModuleAndScope curMod gens cons classes scope (TStar _ _ rtail) =
        "*" ++ renderTypeHtmlWithClassesModuleAndScope curMod gens cons classes scope rtail
    renderTypeHtmlWithClassesModuleAndScope _ _ _ _ _ (TFX _ fx) = render (pretty fx)
    renderTypeHtmlWithClassesModuleAndScope curMod gens _ classes scope t =
        render (pretty (SimplifiedType t))  -- Fallback for any other type

    renderTConHtmlSimplifiedWithClassesModuleAndScope :: ModName -> Set Name -> QBinds -> Set Name -> String -> TCon -> String
    renderTConHtmlSimplifiedWithClassesModuleAndScope curMod gens cons classes scope (TC qn ts)
        | qn == qnList && length ts == 1 = "[" ++ renderTypeHtmlWithClassesModuleAndScope curMod gens cons classes scope (head ts) ++ "]"
        | qn == qnDict && length ts == 2 =
            "{" ++ renderTypeHtmlWithClassesModuleAndScope curMod gens cons classes scope (head ts) ++ ": " ++
            renderTypeHtmlWithClassesModuleAndScope curMod gens cons classes scope (ts !! 1) ++ "}"
        | qn == qnSetT && length ts == 1 = "{" ++ renderTypeHtmlWithClassesModuleAndScope curMod gens cons classes scope (head ts) ++ "}"
        | otherwise =
            let simpleName = simplifyQName qn
                nameStr = render (pretty simpleName)
                -- Generate link based on qualified name with relative paths
                linkStr = case qn of
                    NoQ n -> if Set.member n classes
                            then "<a href=\"#class-" ++ nameStr ++ "\" class=\"type-link\">" ++ nameStr ++ "</a>"
                            else nameStr
                    QName m n ->
                        -- This is a qualified name from another module
                        let relativePath = computeRelativeModulePath curMod m
                            url = relativePath ++ "#class-" ++ render (pretty n)
                        in "<a href=\"" ++ url ++ "\" class=\"type-link\">" ++ nameStr ++ "</a>"
                    GName m n ->
                        -- Global name (e.g., from __builtin__)
                        let modPathList = modPath m
                        in if modPathList == ["__builtin__"]
                           then nameStr  -- Don't link builtin types
                           else
                               let relativePath = computeRelativeModulePath curMod m
                                   url = relativePath ++ "#class-" ++ render (pretty n)
                               in "<a href=\"" ++ url ++ "\" class=\"type-link\">" ++ nameStr ++ "</a>"
            in linkStr ++ if null ts then "" else "[" ++ intercalate ", " (map (renderTypeHtmlWithClassesModuleAndScope curMod gens cons classes scope) ts) ++ "]"

    renderFunArgsWithClassesModuleAndScope :: ModName -> Set Name -> QBinds -> Set Name -> String -> PosRow -> KwdRow -> String
    renderFunArgsWithClassesModuleAndScope curMod gens cons classes scope posrow kwdrow =
        let posArgs = renderPosRowWithClassesModuleAndScope curMod gens cons classes scope posrow
            kwdArgs = renderKwdRowWithClassesModuleAndScope curMod gens cons classes scope kwdrow
        in case (posArgs, kwdArgs) of
            ("", "") -> ""
            (p, "") -> "(" ++ p ++ ")"
            ("", k) -> "(" ++ k ++ ")"
            (p, k) -> "(" ++ p ++ ", " ++ k ++ ")"

    renderTupleArgsWithClassesModuleAndScope :: ModName -> Set Name -> QBinds -> Set Name -> String -> PosRow -> KwdRow -> String
    renderTupleArgsWithClassesModuleAndScope curMod gens cons classes scope posrow kwdrow =
        let posArgs = renderPosRowWithClassesModuleAndScope curMod gens cons classes scope posrow
            kwdArgs = renderKwdRowWithClassesModuleAndScope curMod gens cons classes scope kwdrow
        in case (posArgs, kwdArgs) of
            ("", "") -> ""
            (p, "") -> p
            ("", k) -> k
            (p, k) -> p ++ ", " ++ k

    renderPosRowWithClassesModuleAndScope :: ModName -> Set Name -> QBinds -> Set Name -> String -> PosRow -> String
    renderPosRowWithClassesModuleAndScope _ _ _ _ _ (TNil _ _) = ""
    renderPosRowWithClassesModuleAndScope curMod gens cons classes scope (TRow _ _ _ rtype rtail) =
        renderTypeHtmlWithClassesModuleAndScope curMod gens cons classes scope rtype ++
        case rtail of
            TNil _ _ -> ""
            _ -> ", " ++ renderPosRowWithClassesModuleAndScope curMod gens cons classes scope rtail
    renderPosRowWithClassesModuleAndScope _ _ _ _ _ (TStar _ _ _) = "*args"
    renderPosRowWithClassesModuleAndScope curMod gens cons classes scope t = renderTypeHtmlWithClassesModuleAndScope curMod gens cons classes scope t

    renderKwdRowWithClassesModuleAndScope :: ModName -> Set Name -> QBinds -> Set Name -> String -> KwdRow -> String
    renderKwdRowWithClassesModuleAndScope _ _ _ _ _ (TNil _ _) = ""
    renderKwdRowWithClassesModuleAndScope curMod gens cons classes scope (TRow _ _ label rtype rtail) =
        render (pretty label) ++ "=" ++ renderTypeHtmlWithClassesModuleAndScope curMod gens cons classes scope rtype ++
        case rtail of
            TNil _ _ -> ""
            _ -> ", " ++ renderKwdRowWithClassesModuleAndScope curMod gens cons classes scope rtail
    renderKwdRowWithClassesModuleAndScope _ _ _ _ _ (TStar _ _ _) = "**kwargs"
    renderKwdRowWithClassesModuleAndScope curMod gens cons classes scope t = renderTypeHtmlWithClassesModuleAndScope curMod gens cons classes scope t

-- | Render type with generic type highlighting, constraints and class links
renderTypeWithGenericsConstraintsAndClasses :: Set Name -> QBinds -> Set Name -> Type -> String
renderTypeWithGenericsConstraintsAndClasses generics constraints classNames t = renderTypeHtmlWithClasses generics constraints classNames t
  where
    renderTypeHtmlWithClasses :: Set Name -> QBinds -> Set Name -> Type -> String
    renderTypeHtmlWithClasses gens cons classes (TVar _ tv)
        | Set.member (tvname tv) gens =
            let nameStr = render (pretty (tvname tv))
                -- Find constraints for this type variable
                tvConstraints = [preds | QBind qtv preds <- cons, tvname qtv == tvname tv]
                tooltip = case tvConstraints of
                    [] -> "Generic type " ++ nameStr ++ "\n\nThis is a placeholder for any type.\nAll " ++
                          nameStr ++ "s must be the same type."
                    (preds:_) -> if null preds
                        then "Generic type " ++ nameStr ++ "\n\nThis is a placeholder for any type.\nAll " ++
                             nameStr ++ "s must be the same type."
                        else "Generic type " ++ nameStr ++ "\n\nThis is a placeholder for any type.\nAll " ++
                             nameStr ++ "s must be the same type.\n\n" ++ nameStr ++ " must support:\n" ++
                             concatMap formatPredAsListItem preds
                formatPredAsListItem p = "  • " ++ formatConstraintNameLocal p ++ " protocol (" ++ formatExampleForPredLocal p ++ ")\n"
                formatConstraintNameLocal (TC qn _) = render . pretty . simplifyQName $ qn
                formatExampleForPredLocal (TC (NoQ (Name _ "Plus")) _) = "+ operator"
                formatExampleForPredLocal (TC (NoQ (Name _ "Minus")) _) = "- operator"
                formatExampleForPredLocal (TC (NoQ (Name _ "Times")) _) = "* operator"
                formatExampleForPredLocal (TC (NoQ (Name _ "Divide")) _) = "/ operator"
                formatExampleForPredLocal (TC (NoQ (Name _ "Eq")) _) = "== operator"
                formatExampleForPredLocal (TC (NoQ (Name _ "Ord")) _) = "< > operators"
                formatExampleForPredLocal (TC (NoQ (Name _ "Hash")) _) = "hash function"
                formatExampleForPredLocal _ = "protocol methods"
            in "<span class=\"generic-type\" data-generic=\"" ++ nameStr ++
               "\" data-tooltip=\"" ++ tooltip ++ "\">" ++ nameStr ++ "</span>"
        | otherwise =
            let simplified = render (pretty (SimplifiedType (TVar NoLoc tv)))
            in if simplified == render (pretty (tvname tv))
               then simplified  -- Already simplified
               else simplified
    renderTypeHtmlWithClasses gens cons classes (TCon _ tc) = renderTConHtmlSimplifiedWithClasses gens cons classes tc
    renderTypeHtmlWithClasses gens cons classes (TFun _ _ posrow kwdrow restype) =
        let args = renderFunArgsWithClasses gens cons classes posrow kwdrow
        in if null args
           then renderTypeHtmlWithClasses gens cons classes restype
           else args ++ " -> " ++ renderTypeHtmlWithClasses gens cons classes restype
    renderTypeHtmlWithClasses gens cons classes (TTuple _ posrow kwdrow) =
        "(" ++ renderTupleArgsWithClasses gens cons classes posrow kwdrow ++ ")"
    renderTypeHtmlWithClasses gens cons classes (TOpt _ t) = "?" ++ renderTypeHtmlWithClasses gens cons classes t
    renderTypeHtmlWithClasses gens _ _ (TNone _) = "None"
    renderTypeHtmlWithClasses gens _ _ (TWild _) = "_"
    renderTypeHtmlWithClasses gens _ _ (TNil _ _) = ""
    renderTypeHtmlWithClasses gens cons classes (TRow _ _ label rtype rtail) =
        render (pretty label) ++ ": " ++ renderTypeHtmlWithClasses gens cons classes rtype ++
        case rtail of
            TNil _ _ -> ""
            _ -> ", " ++ renderTypeHtmlWithClasses gens cons classes rtail
    renderTypeHtmlWithClasses gens cons classes (TStar _ _ rtail) = "*" ++ renderTypeHtmlWithClasses gens cons classes rtail
    renderTypeHtmlWithClasses gens _ _ (TFX _ fx) = render (pretty fx)
    renderTypeHtmlWithClasses gens _ classes t = render (pretty (SimplifiedType t))  -- Fallback for any other type

    renderTConHtmlSimplifiedWithClasses :: Set Name -> QBinds -> Set Name -> TCon -> String
    renderTConHtmlSimplifiedWithClasses gens cons classes (TC qn ts)
        | qn == qnList && length ts == 1 = "[" ++ renderTypeHtmlWithClasses gens cons classes (head ts) ++ "]"
        | qn == qnDict && length ts == 2 = "{" ++ renderTypeHtmlWithClasses gens cons classes (head ts) ++ ": " ++ renderTypeHtmlWithClasses gens cons classes (ts !! 1) ++ "}"
        | qn == qnSetT && length ts == 1 = "{" ++ renderTypeHtmlWithClasses gens cons classes (head ts) ++ "}"
        | otherwise =
            let simpleName = simplifyQName qn
                nameStr = render (pretty simpleName)
                -- Generate link based on qualified name
                linkStr = case qn of
                    NoQ n -> if Set.member n classes
                            then "<a href=\"#class-" ++ nameStr ++ "\" class=\"type-link\">" ++ nameStr ++ "</a>"
                            else nameStr
                    QName m n ->
                        -- This is a qualified name from another module
                        let modPathList = modPath m
                            modPathStr = intercalate "/" modPathList
                            modFile = if null modPathStr
                                     then "index.html"  -- Root module
                                     else modPathStr ++ ".html"
                            url = modFile ++ "#class-" ++ render (pretty n)
                        in "<a href=\"" ++ url ++ "\" class=\"type-link\">" ++ nameStr ++ "</a>"
                    GName m n ->
                        -- Global name (e.g., from __builtin__)
                        let modPathList = modPath m
                        in if modPathList == ["__builtin__"]
                           then nameStr  -- Don't link builtin types
                           else
                               let modPathStr = intercalate "/" modPathList
                                   modFile = if null modPathStr
                                            then "index.html"
                                            else modPathStr ++ ".html"
                                   url = modFile ++ "#class-" ++ render (pretty n)
                               in "<a href=\"" ++ url ++ "\" class=\"type-link\">" ++ nameStr ++ "</a>"
            in linkStr ++ if null ts then "" else "[" ++ intercalate ", " (map (renderTypeHtmlWithClasses gens cons classes) ts) ++ "]"

    renderFunArgsWithClasses :: Set Name -> QBinds -> Set Name -> PosRow -> KwdRow -> String
    renderFunArgsWithClasses gens cons classes posrow kwdrow =
        let posArgs = renderPosRowWithClasses gens cons classes posrow
            kwdArgs = renderKwdRowWithClasses gens cons classes kwdrow
        in case (posArgs, kwdArgs) of
            ("", "") -> ""
            (p, "") -> "(" ++ p ++ ")"
            ("", k) -> "(" ++ k ++ ")"
            (p, k) -> "(" ++ p ++ ", " ++ k ++ ")"

    renderTupleArgsWithClasses :: Set Name -> QBinds -> Set Name -> PosRow -> KwdRow -> String
    renderTupleArgsWithClasses gens cons classes posrow kwdrow =
        let posArgs = renderPosRowWithClasses gens cons classes posrow
            kwdArgs = renderKwdRowWithClasses gens cons classes kwdrow
        in case (posArgs, kwdArgs) of
            ("", "") -> ""
            (p, "") -> p
            ("", k) -> k
            (p, k) -> p ++ ", " ++ k

    renderPosRowWithClasses :: Set Name -> QBinds -> Set Name -> PosRow -> String
    renderPosRowWithClasses gens _ _ (TNil _ _) = ""
    renderPosRowWithClasses gens cons classes (TRow _ _ _ rtype rtail) =
        renderTypeHtmlWithClasses gens cons classes rtype ++
        case rtail of
            TNil _ _ -> ""
            _ -> ", " ++ renderPosRowWithClasses gens cons classes rtail
    renderPosRowWithClasses gens _ _ (TStar _ _ _) = "*args"
    renderPosRowWithClasses gens cons classes t = renderTypeHtmlWithClasses gens cons classes t

    renderKwdRowWithClasses :: Set Name -> QBinds -> Set Name -> KwdRow -> String
    renderKwdRowWithClasses gens _ _ (TNil _ _) = ""
    renderKwdRowWithClasses gens cons classes (TRow _ _ label rtype rtail) =
        render (pretty label) ++ "=" ++ renderTypeHtmlWithClasses gens cons classes rtype ++
        case rtail of
            TNil _ _ -> ""
            _ -> ", " ++ renderKwdRowWithClasses gens cons classes rtail
    renderKwdRowWithClasses gens _ _ (TStar _ _ _) = "**kwargs"
    renderKwdRowWithClasses gens cons classes t = renderTypeHtmlWithClasses gens cons classes t


-- | Helper function to enrich parameter display with inferred types and constraints
enrichParamsWithTypesHtml :: Set Name -> QBinds -> PosRow -> KwdRow -> PosPar -> KwdPar -> Doc
enrichParamsWithTypesHtml generics constraints posRow kwdRow p k =
    let posParams = enrichPosParamsHtml generics constraints posRow p
        kwdParams = enrichKwdParamsHtml generics constraints kwdRow k
        sep = if isEmpty posParams || isEmpty kwdParams then empty else text ", "
    in text "(" <> posParams <> sep <> kwdParams <> text ")"

-- | Helper function to enrich parameter display with inferred types, constraints and class links
enrichParamsWithTypesHtmlAndClasses :: Set Name -> QBinds -> Set Name -> PosRow -> KwdRow -> PosPar -> KwdPar -> Doc
enrichParamsWithTypesHtmlAndClasses generics constraints classNames posRow kwdRow p k =
    let posParams = enrichPosParamsHtmlWithClasses generics constraints classNames posRow p
        kwdParams = enrichKwdParamsHtmlWithClasses generics constraints classNames kwdRow k
        sep = if isEmpty posParams || isEmpty kwdParams then empty else text ", "
    in text "(" <> posParams <> sep <> kwdParams <> text ")"

enrichPosParamsHtml :: Set Name -> QBinds -> PosRow -> PosPar -> Doc
enrichPosParamsHtml _ _ _ PosNIL = empty
enrichPosParamsHtml generics constraints posRow (PosPar n t e p) =
    -- Skip witness parameters
    if isWitnessParam n
    then enrichPosParamsHtml generics constraints (advanceRow posRow) p
    else
        let inferredType = extractParamTypeFromRow (nstr n) posRow
            paramType = case inferredType of
                Just it -> Just it
                Nothing -> t
            param = text "<span class=\"param-name\">" <> pretty n <> text "</span>" <>
                    formatTypeHtmlWithGenericsAndConstraints generics constraints paramType <> formatDefaultHtml e
            nextParams = enrichPosParamsHtml generics constraints (advanceRow posRow) p
        in if isEmpty nextParams
           then param
           else param <> text ", " <> nextParams
enrichPosParamsHtml generics constraints _ (PosSTAR n t) = text "*<span class=\"param-name\">" <> pretty n <> text "</span>" <> formatTypeHtmlWithGenericsAndConstraints generics constraints t

enrichKwdParamsHtml :: Set Name -> QBinds -> KwdRow -> KwdPar -> Doc
enrichKwdParamsHtml _ _ _ KwdNIL = empty
enrichKwdParamsHtml generics constraints kwdRow (KwdPar n t e k) =
    let inferredType = extractParamTypeFromRow (nstr n) kwdRow
        paramType = case inferredType of
            Just it -> Just it
            Nothing -> t
        param = text "<span class=\"param-name\">" <> pretty n <> text "</span>" <>
                formatTypeHtmlWithGenericsAndConstraints generics constraints paramType <> formatDefaultHtml e
    in case k of
        KwdNIL -> param
        _ -> param <> text ", " <> enrichKwdParamsHtml generics constraints kwdRow k
enrichKwdParamsHtml generics constraints _ (KwdSTAR n t) = text "**<span class=\"param-name\">" <> pretty n <> text "</span>" <> formatTypeHtmlWithGenericsAndConstraints generics constraints t

enrichPosParamsHtmlWithClasses :: Set Name -> QBinds -> Set Name -> PosRow -> PosPar -> Doc
enrichPosParamsHtmlWithClasses _ _ _ _ PosNIL = empty
enrichPosParamsHtmlWithClasses generics constraints classNames posRow (PosPar n t e p) =
    -- Skip witness parameters
    if isWitnessParam n
    then enrichPosParamsHtmlWithClasses generics constraints classNames (advanceRow posRow) p
    else
        let inferredType = extractParamTypeFromRow (nstr n) posRow
            paramType = case inferredType of
                Just it -> Just it
                Nothing -> t
            param = text "<span class=\"param-name\">" <> pretty n <> text "</span>" <>
                    formatTypeHtmlWithGenericsConstraintsAndClasses generics constraints classNames paramType <> formatDefaultHtml e
            nextParams = enrichPosParamsHtmlWithClasses generics constraints classNames (advanceRow posRow) p
        in if isEmpty nextParams
           then param
           else param <> text ", " <> nextParams
enrichPosParamsHtmlWithClasses generics constraints classNames _ (PosSTAR n t) = text "*<span class=\"param-name\">" <> pretty n <> text "</span>" <> formatTypeHtmlWithGenericsConstraintsAndClasses generics constraints classNames t

enrichKwdParamsHtmlWithClasses :: Set Name -> QBinds -> Set Name -> KwdRow -> KwdPar -> Doc
enrichKwdParamsHtmlWithClasses _ _ _ _ KwdNIL = empty
enrichKwdParamsHtmlWithClasses generics constraints classNames kwdRow (KwdPar n t e k) =
    let inferredType = extractParamTypeFromRow (nstr n) kwdRow
        paramType = case inferredType of
            Just it -> Just it
            Nothing -> t
        param = text "<span class=\"param-name\">" <> pretty n <> text "</span>" <>
                formatTypeHtmlWithGenericsConstraintsAndClasses generics constraints classNames paramType <> formatDefaultHtml e
    in case k of
        KwdNIL -> param
        _ -> param <> text ", " <> enrichKwdParamsHtmlWithClasses generics constraints classNames kwdRow k
enrichKwdParamsHtmlWithClasses generics constraints classNames _ (KwdSTAR n t) = text "**<span class=\"param-name\">" <> pretty n <> text "</span>" <> formatTypeHtmlWithGenericsConstraintsAndClasses generics constraints classNames t

-- Extract parameter type from row
extractParamTypeFromRow :: String -> Type -> Maybe Type
extractParamTypeFromRow _ (TNil _ _) = Nothing
extractParamTypeFromRow name (TRow _ _ n t rest)
    | nstr n == name = Just t
    | otherwise = extractParamTypeFromRow name rest
extractParamTypeFromRow _ _ = Nothing

-- Advance to next position in row
advanceRow :: Type -> Type
advanceRow (TRow _ _ _ _ rest) = rest
advanceRow t = t

-- Extract parameter type from keyword row
extractParamTypeFromKwdRow :: String -> Type -> Maybe Type
extractParamTypeFromKwdRow _ (TNil _ _) = Nothing
extractParamTypeFromKwdRow name (TRow _ _ n t rest)
    | nstr n == name = Just t
    | otherwise = extractParamTypeFromKwdRow name rest
extractParamTypeFromKwdRow _ _ = Nothing

-- | Document a declaration in HTML format
docDeclHtml :: Decl -> Doc
docDeclHtml = docDeclHtmlWithTypes []

-- | Document a declaration in HTML format with type information
docDeclHtmlWithTypes :: TEnv -> Decl -> Doc
docDeclHtmlWithTypes tenv = docDeclHtmlWithTypesAndClasses tenv Set.empty

-- | Document a declaration in HTML format with type information and class links
-- | Document a declaration in HTML with types and class info for cross-module linking
docDeclHtmlWithTypesAndClassInfo :: TEnv -> ModName -> Set ClassInfo -> Decl -> Doc
docDeclHtmlWithTypesAndClassInfo tenv currentModule classInfos decl =
    let classNames = Set.map ciName $ Set.filter ciIsLocal classInfos
    in docDeclHtmlWithTypesAndClassesAndModule tenv currentModule classNames decl

-- | Document a declaration in HTML with module context for proper cross-module links
docDeclHtmlWithTypesAndClassesAndModule :: TEnv -> ModName -> Set Name -> Decl -> Doc
docDeclHtmlWithTypesAndClassesAndModule tenv currentModule classNames decl =
    -- Update all type rendering to use the module-aware version
    case decl of
        Def _ n q p k a b d x ddoc -> docDefHtmlWithModule tenv currentModule classNames n q p k a b d ddoc
        Actor _ n q p k b ddoc -> docActorHtmlWithModule tenv currentModule classNames n q p k b ddoc
        Class _ n q a b ddoc ->
            let wtcons = map (\tc -> ([], tc)) a  -- PCon is just a type alias for TCon
            in docClassHtmlWithModule tenv currentModule classNames n q wtcons b ddoc
        Protocol _ n q a b ddoc ->
            let wtcons = map (\pc -> ([], pc)) a
            in docProtocolHtmlWithModule tenv currentModule classNames n q wtcons b ddoc
        Extension _ q c a b ddoc ->
            let wtcons = map (\pc -> ([], pc)) a
            in docExtensionHtmlWithModule tenv currentModule classNames q c wtcons b ddoc
  where
    docDefHtmlWithModule tenv curMod classNames n q p k a b d ddoc =
        let explicitGenerics = extractGenerics q
            funcScope = "def-" ++ nstr n
            -- Look up inferred type information
            (inferredType, qConstraints) = case lookup n tenv of
                Just (NDef schema _ _) -> (Just schema, getQBindsFromSchema schema)
                Just (NSig schema _ _) -> (Just schema, getQBindsFromSchema schema)
                _ -> (Nothing, [])

            -- Create a mapping from ugly type vars to nice generic names
            uglyTypeVarsFromQBinds = collectUglyTypeVars qConstraints
            uglyTypeVarsFromType = case inferredType of
                Just (TSchema _ _ t) -> collectUglyTypeVarsFromType t
                _ -> []
            allUglyTypeVars = nub (uglyTypeVarsFromQBinds ++ uglyTypeVarsFromType)
            typeVarMapping = createTypeVarMapping allUglyTypeVars

            -- Process the type information with cleanup
            (paramsWithTypes, inferredRetType, constraints, allGenerics) = case inferredType of
                Just (TSchema _ qConstraints (TFun _ _ posRow kwdRow retType)) ->
                    let cleanPosRow = cleanupTypeVars typeVarMapping posRow
                        cleanKwdRow = cleanupTypeVars typeVarMapping kwdRow
                        cleanRetType = cleanupTypeVars typeVarMapping retType
                        cleanConstraints = cleanupQBinds typeVarMapping qConstraints
                        inferredGenerics = extractGenerics cleanConstraints
                        mappedGenerics = if null q && not (null typeVarMapping)
                                         then Set.fromList (map snd typeVarMapping)
                                         else explicitGenerics
                        combinedGenerics = Set.union mappedGenerics inferredGenerics
                    in (enrichParamsWithTypesHtmlAndClassesModuleAndScope curMod combinedGenerics cleanConstraints classNames funcScope cleanPosRow cleanKwdRow p k, Just cleanRetType, cleanConstraints, combinedGenerics)
                _ -> (docParamsHtmlWithGenericsAndClassesModuleAndScope curMod explicitGenerics classNames funcScope p k, a, [], explicitGenerics)

            -- Use inferred return type if available, otherwise use annotation
            actualRetType = case inferredRetType of
                Just t -> docRetTypeHtmlWithGenericsConstraintsAndClassesModuleAndScope curMod allGenerics constraints classNames funcScope (Just t)
                Nothing -> docRetTypeHtmlWithGenericsConstraintsAndClassesModuleAndScope curMod allGenerics constraints classNames funcScope a

            -- Format constraints if present, or generics if no constraints
            (constraintsDoc, genericsDoc) = if null constraints
                then (empty, docGenericsHtmlWithHighlightAndScope allGenerics funcScope q)
                else (docConstraintsHtmlWithScope allGenerics funcScope constraints <> text " => ", empty)

            header = text "<h2 class=\"type-context\"><code>" <> pretty n <> text "</code>" <>
                     constraintsDoc <> genericsDoc <> paramsWithTypes <> actualRetType <> text "</h2>"
            docstr = case ddoc of
                Just ds -> text "<div class=\"docstring\">" <> text (nl2br $ htmlEscape ds) <> text "</div>"
                Nothing -> empty
        in header $+$ docstr

    docActorHtmlWithModule tenv curMod classNames n q p k b ddoc =
        let explicitGenerics = extractGenerics q
            -- Look up inferred type information for actors
            (paramsWithTypes, allGenerics, constraints) = case lookup n tenv of
                Just (NAct qConstraints posRow kwdRow _ _) ->
                    let inferredGenerics = extractGenerics qConstraints
                        combinedGenerics = Set.union explicitGenerics inferredGenerics
                    in (enrichParamsWithTypesHtmlAndClassesModule curMod combinedGenerics qConstraints classNames posRow kwdRow p k, combinedGenerics, qConstraints)
                _ -> (docParamsHtmlWithGenericsAndClassesModule curMod explicitGenerics classNames p k, explicitGenerics, [])

            -- Always use docConstraintsHtml for rich tooltips - it handles both constrained and unconstrained generics
            genericsDoc = if null q && null constraints
                         then empty  -- No generics at all
                         else if null constraints
                              then docConstraintsHtml allGenerics q
                              else docConstraintsHtml allGenerics constraints
            header = text "<h2 class=\"type-context\"><span class=\"keyword\">actor</span> <code>" <> pretty n <> text "</code>" <>
                     genericsDoc <> paramsWithTypes <> text "</h2>"
            docstr = case ddoc of
                Just ds -> text "<div class=\"docstring\">" <> text (nl2br $ htmlEscape ds) <> text "</div>"
                Nothing -> empty
            -- Process actor body to extract public constants, internal attributes and methods
            body = docActorBodyHtmlWithGenericsTypesAndClassesModule tenv curMod allGenerics classNames b
        in text "<div class=\"declaration-block\">" $+$
           header $+$ docstr $+$
           (if isEmpty body then empty else blank $+$ body) $+$
           text "</div>"

    docClassHtmlWithModule tenv curMod classNames n q a b ddoc =
        let generics = extractGenerics q
            -- Always use docConstraintsHtml for rich tooltips - it handles both constrained and unconstrained generics
            classScope = "class-" ++ nstr n
            genericsDoc = if null q
                         then empty  -- No generics at all
                         else docConstraintsHtmlWithScope generics classScope q
            header = text "<h2 id=\"class-" <> text (nstr n) <> text "\" class=\"type-context\"><span class=\"keyword\">class</span> <code>" <> pretty n <> text "</code>" <>
                     genericsDoc <> docAncestorsHtmlWithGenericsAndClassesModule curMod generics classNames a <> text "</h2>"
            docstr = case ddoc of
                Just ds -> text "<div class=\"docstring\">" <> text (nl2br $ htmlEscape ds) <> text "</div>"
                Nothing -> empty
            methods = docClassBodyHtmlWithGenericsTypesAndClassesModule tenv curMod generics classNames b
        in text "<div class=\"declaration-block\">" $+$
           header $+$ docstr $+$
           (if isEmpty methods then empty else blank $+$ methods) $+$
           text "</div>"

    docProtocolHtmlWithModule tenv curMod classNames n q a b ddoc =
        let generics = extractGenerics q
            -- Always use docConstraintsHtml for rich tooltips - it handles both constrained and unconstrained generics
            protocolScope = "protocol-" ++ nstr n
            genericsDoc = if null q
                         then empty  -- No generics at all
                         else docConstraintsHtmlWithScope generics protocolScope q
            header = text "<h2 class=\"type-context\"><span class=\"keyword\">protocol</span> <code>" <> pretty n <> text "</code>" <>
                     genericsDoc <> docAncestorsHtmlWithGenericsAndClassesModule curMod generics classNames a <> text "</h2>"
            docstr = case ddoc of
                Just ds -> text "<div class=\"docstring\">" <> text (nl2br $ htmlEscape ds) <> text "</div>"
                Nothing -> empty
            methods = docProtocolBodyHtmlWithGenericsTypesAndClassesModule tenv curMod generics classNames b
        in text "<div class=\"declaration-block\">" $+$
           header $+$ docstr $+$
           (if isEmpty methods then empty else blank $+$ methods) $+$
           text "</div>"

    docExtensionHtmlWithModule tenv curMod classNames q c a b ddoc =
        let generics = extractGenerics q
            -- Always use docConstraintsHtml for rich tooltips - it handles both constrained and unconstrained generics
            genericsDoc = if null q
                         then empty  -- No generics at all
                         else docConstraintsHtml generics q
            header = text "<h2 class=\"type-context\"><span class=\"keyword\">extension</span> <code>" <> pretty c <> text "</code>" <>
                     genericsDoc <> docAncestorsHtmlWithGenericsAndClassesModule curMod generics classNames a <> text "</h2>"
            docstr = case ddoc of
                Just ds -> text "<div class=\"docstring\">" <> text (nl2br $ htmlEscape ds) <> text "</div>"
                Nothing -> empty
            methods = docClassBodyHtmlWithGenericsTypesAndClassesModule tenv curMod generics classNames b
        in text "<div class=\"declaration-block\">" $+$
           header $+$ docstr $+$
           (if isEmpty methods then empty else blank $+$ methods) $+$
           text "</div>"

    nl2br = intercalate "<br>" . lines

docDeclHtmlWithTypesAndClasses :: TEnv -> Set Name -> Decl -> Doc
docDeclHtmlWithTypesAndClasses tenv classNames (Def _ n q p k a b d x ddoc) =
    let explicitGenerics = extractGenerics q
        -- Look up inferred type information
        (inferredType, qConstraints) = case lookup n tenv of
            Just (NDef schema _ _) -> (Just schema, getQBindsFromSchema schema)
            Just (NSig schema _ _) -> (Just schema, getQBindsFromSchema schema)
            _ -> (Nothing, [])

        -- Create a mapping from ugly type vars to nice generic names
        uglyTypeVarsFromQBinds = collectUglyTypeVars qConstraints
        uglyTypeVarsFromType = case inferredType of
            Just (TSchema _ _ t) -> collectUglyTypeVarsFromType t
            _ -> []
        allUglyTypeVars = nub (uglyTypeVarsFromQBinds ++ uglyTypeVarsFromType)
        typeVarMapping = createTypeVarMapping allUglyTypeVars

        -- Process the type information with cleanup
        (paramsWithTypes, inferredRetType, constraints, allGenerics) = case inferredType of
            Just (TSchema _ qConstraints (TFun _ _ posRow kwdRow retType)) ->
                let cleanPosRow = cleanupTypeVars typeVarMapping posRow
                    cleanKwdRow = cleanupTypeVars typeVarMapping kwdRow
                    cleanRetType = cleanupTypeVars typeVarMapping retType
                    cleanConstraints = cleanupQBinds typeVarMapping qConstraints
                    inferredGenerics = extractGenerics cleanConstraints
                    mappedGenerics = if null q && not (null typeVarMapping)
                                     then Set.fromList (map snd typeVarMapping)
                                     else explicitGenerics
                    combinedGenerics = Set.union mappedGenerics inferredGenerics
                in (enrichParamsWithTypesHtmlAndClasses combinedGenerics cleanConstraints classNames cleanPosRow cleanKwdRow p k, Just cleanRetType, cleanConstraints, combinedGenerics)
            _ -> (docParamsHtmlWithGenericsAndClasses explicitGenerics classNames p k, a, [], explicitGenerics)

        -- Use inferred return type if available, otherwise use annotation
        actualRetType = case inferredRetType of
            Just t -> docRetTypeHtmlWithGenericsConstraintsAndClasses allGenerics constraints classNames (Just t)
            Nothing -> docRetTypeHtmlWithGenericsConstraintsAndClasses allGenerics constraints classNames a

        -- Format constraints if present, or generics if no constraints
        (constraintsDoc, genericsDoc) = if null constraints
            then (empty, docGenericsHtmlWithHighlight allGenerics q)
            else (docConstraintsHtml allGenerics constraints <> text " => ", empty)

        header = text "<h2 class=\"type-context\"><code>" <> pretty n <> text "</code>" <>
                 constraintsDoc <> genericsDoc <> paramsWithTypes <> actualRetType <> text "</h2>"
        mdocstring = case lookup n tenv of
            Just info -> extractNameDocstring info
            _ -> Nothing
        docstr = case mdocstring of
            Just ds -> text "<div class=\"docstring\">" <> text (nl2br $ htmlEscape ds) <> text "</div>"
            Nothing -> empty
    in header $+$ docstr
  where
    nl2br = intercalate "<br>" . lines

docDeclHtmlWithTypesAndClasses tenv classNames (Actor _ n q p k b ddoc) =
    let explicitGenerics = extractGenerics q
        -- Look up inferred type information for actors
        (paramsWithTypes, allGenerics, constraints) = case lookup n tenv of
            Just (NAct qConstraints posRow kwdRow _ _) ->
                let inferredGenerics = extractGenerics qConstraints
                    combinedGenerics = Set.union explicitGenerics inferredGenerics
                in (enrichParamsWithTypesHtmlAndClasses combinedGenerics qConstraints classNames posRow kwdRow p k, combinedGenerics, qConstraints)
            _ -> (docParamsHtmlWithGenericsAndClasses explicitGenerics classNames p k, explicitGenerics, [])

        header = text "<h2 class=\"type-context\"><span class=\"keyword\">actor</span> <code>" <> pretty n <> text "</code>" <>
                 docGenericsHtmlWithHighlight allGenerics q <> paramsWithTypes <> text "</h2>"
        mdocstring = case lookup n tenv of
            Just info -> extractNameDocstring info
            _ -> Nothing
        docstr = case mdocstring of
            Just ds -> text "<div class=\"docstring\">" <> text (nl2br $ htmlEscape ds) <> text "</div>"
            Nothing -> empty
        -- Process actor body to extract attributes and methods
        body = docActorBodyHtmlWithGenericsTypesAndClasses tenv allGenerics classNames b
    in header $+$ docstr $+$
       (if isEmpty body then empty else blank $+$ body)
  where
    nl2br = intercalate "<br>" . lines

docDeclHtmlWithTypesAndClasses tenv classNames (Class _ n q a b ddoc) =
    let generics = extractGenerics q
        header = text "<h2 id=\"class-" <> text (nstr n) <> text "\" class=\"type-context\"><span class=\"keyword\">class</span> <code>" <> pretty n <> text "</code>" <>
                 docGenericsHtmlWithHighlight generics q <> docAncestorsHtmlWithGenericsAndClasses generics classNames a <> text "</h2>"
        mdocstring = case lookup n tenv of
            Just info -> extractNameDocstring info
            _ -> Nothing
        docstr = case mdocstring of
            Just ds -> text "<div class=\"docstring\">" <> text (nl2br $ htmlEscape ds) <> text "</div>"
            Nothing -> empty
        methods = docClassBodyHtmlWithGenericsTypesAndClasses tenv generics classNames b
    in header $+$ docstr $+$
       (if isEmpty methods then empty else blank $+$ methods)
  where
    nl2br = intercalate "<br>" . lines

docDeclHtmlWithTypesAndClasses tenv classNames (Protocol _ n q a b ddoc) =
    let generics = extractGenerics q
        protocolScope = "protocol-" ++ nstr n
        header = text "<h2 class=\"type-context\"><span class=\"keyword\">protocol</span> <code>" <> pretty n <> text "</code>" <>
                 docGenericsHtmlWithHighlightAndScope generics protocolScope q <> docAncestorsHtmlWithGenericsAndClasses generics classNames a <> text "</h2>"
        mdocstring = case lookup n tenv of
            Just info -> extractNameDocstring info
            _ -> Nothing
        docstr = case mdocstring of
            Just ds -> text "<div class=\"docstring\">" <> text (nl2br $ htmlEscape ds) <> text "</div>"
            Nothing -> empty
        methods = docProtocolBodyHtmlWithGenericsTypesAndClasses tenv generics classNames b
    in header $+$ docstr $+$
       (if isEmpty methods then empty else blank $+$ methods)
  where
    nl2br = intercalate "<br>" . lines

docDeclHtmlWithTypesAndClasses tenv classNames (Extension _ q c a b ddoc) =
    let generics = extractGenerics q
        header = text "<h2 class=\"type-context\"><span class=\"keyword\">extension</span> <code>" <> pretty c <> text "</code>" <>
                 docGenericsHtmlWithHighlight generics q <> docAncestorsHtmlWithGenericsAndClasses generics classNames a <> text "</h2>"
        mdocstring = Nothing  -- Extensions don't have their own docstrings in TEnv
        docstr = case mdocstring of
            Just ds -> text "<div class=\"docstring\">" <> text (nl2br $ htmlEscape ds) <> text "</div>"
            Nothing -> empty
    in header $+$ docstr
  where
    nl2br = intercalate "<br>" . lines

-- | Module-aware helper functions for type rendering in HTML
enrichParamsWithTypesHtmlAndClassesModule :: ModName -> Set Name -> QBinds -> Set Name -> PosRow -> KwdRow -> PosPar -> KwdPar -> Doc
enrichParamsWithTypesHtmlAndClassesModule curMod generics constraints classNames posRow kwdRow p k =
    text "(" <> enrichPosParamsHtmlModule curMod generics constraints classNames posRow p <>
    docKwdParSepHtml p k <>
    enrichKwdParamsHtmlModule curMod generics constraints classNames kwdRow k <> text ")"

-- | Enrich parameters with types from type environment in HTML format with scope
enrichParamsWithTypesHtmlAndClassesModuleAndScope :: ModName -> Set Name -> QBinds -> Set Name -> String -> PosRow -> KwdRow -> PosPar -> KwdPar -> Doc
enrichParamsWithTypesHtmlAndClassesModuleAndScope curMod generics constraints classNames scope posRow kwdRow p k =
    text "(" <> enrichPosParamsHtmlModuleAndScope curMod generics constraints classNames scope posRow p <>
    docKwdParSepHtml p k <>
    enrichKwdParamsHtmlModuleAndScope curMod generics constraints classNames scope kwdRow k <> text ")"

enrichPosParamsHtmlModule :: ModName -> Set Name -> QBinds -> Set Name -> PosRow -> PosPar -> Doc
enrichPosParamsHtmlModule _ _ _ _ _ PosNIL = empty
enrichPosParamsHtmlModule curMod generics constraints classNames posRow (PosPar n t e p) =
    -- Skip witness parameters
    if isWitnessParam n
    then enrichPosParamsHtmlModule curMod generics constraints classNames (advanceRow posRow) p
    else
        let inferredType = extractParamTypeFromRow (nstr n) posRow
            paramType = case inferredType of
                Just it -> Just it
                Nothing -> t
            param = text "<span class=\"param-name\">" <> pretty n <> text "</span>" <>
                    formatTypeHtmlModule curMod generics constraints classNames paramType <>
                    formatDefaultHtml e
            nextParams = enrichPosParamsHtmlModule curMod generics constraints classNames (advanceRow posRow) p
        in if isEmpty nextParams
           then param
           else param <> text ", " <> nextParams

enrichKwdParamsHtmlModule :: ModName -> Set Name -> QBinds -> Set Name -> KwdRow -> KwdPar -> Doc
enrichKwdParamsHtmlModule _ _ _ _ _ KwdNIL = empty
enrichKwdParamsHtmlModule curMod generics constraints classNames kwdRow (KwdPar n t e k) =
    let inferredType = extractParamTypeFromKwdRow (nstr n) kwdRow
        paramType = case inferredType of
            Just it -> Just it
            Nothing -> t
        param = text "<span class=\"param-name\">" <> pretty n <> text "</span>" <>
                formatTypeHtmlModule curMod generics constraints classNames paramType <>
                formatDefaultHtml e
        nextParams = enrichKwdParamsHtmlModule curMod generics constraints classNames kwdRow k
    in if isEmpty nextParams
       then param
       else param <> text ", " <> nextParams

-- | Enrich positional parameters with types from row and add scope
enrichPosParamsHtmlModuleAndScope :: ModName -> Set Name -> QBinds -> Set Name -> String -> PosRow -> PosPar -> Doc
enrichPosParamsHtmlModuleAndScope _ _ _ _ _ _ PosNIL = empty
enrichPosParamsHtmlModuleAndScope curMod generics constraints classNames scope posRow (PosSTAR n t) =
    let inferredType = extractParamTypeFromRow (nstr n) posRow
        paramType = case inferredType of
            Just it -> Just it
            Nothing -> t
    in text "*<span class=\"param-name\">" <> pretty n <> text "</span>" <>
       formatTypeHtmlModuleAndScope curMod generics constraints classNames scope paramType
enrichPosParamsHtmlModuleAndScope curMod generics constraints classNames scope posRow (PosPar n t e p) =
    -- Skip witness parameters
    if isWitnessParam n
    then enrichPosParamsHtmlModuleAndScope curMod generics constraints classNames scope (advanceRow posRow) p
    else
        let inferredType = extractParamTypeFromRow (nstr n) posRow
            paramType = case inferredType of
                Just it -> Just it
                Nothing -> t
            param = text "<span class=\"param-name\">" <> pretty n <> text "</span>" <>
                    formatTypeHtmlModuleAndScope curMod generics constraints classNames scope paramType <>
                    formatDefaultHtml e
            nextParams = enrichPosParamsHtmlModuleAndScope curMod generics constraints classNames scope (advanceRow posRow) p
        in if isEmpty nextParams
           then param
           else param <> text ", " <> nextParams

-- | Enrich keyword parameters with types from row and add scope
enrichKwdParamsHtmlModuleAndScope :: ModName -> Set Name -> QBinds -> Set Name -> String -> KwdRow -> KwdPar -> Doc
enrichKwdParamsHtmlModuleAndScope _ _ _ _ _ _ KwdNIL = empty
enrichKwdParamsHtmlModuleAndScope curMod generics constraints classNames scope kwdRow (KwdSTAR n t) =
    let inferredType = extractParamTypeFromKwdRow (nstr n) kwdRow
        paramType = case inferredType of
            Just it -> Just it
            Nothing -> t
    in text "**<span class=\"param-name\">" <> pretty n <> text "</span>" <>
       formatTypeHtmlModuleAndScope curMod generics constraints classNames scope paramType
enrichKwdParamsHtmlModuleAndScope curMod generics constraints classNames scope kwdRow (KwdPar n t e k) =
    let inferredType = extractParamTypeFromKwdRow (nstr n) kwdRow
        paramType = case inferredType of
            Just it -> Just it
            Nothing -> t
        param = text "<span class=\"param-name\">" <> pretty n <> text "</span>" <>
                formatTypeHtmlModuleAndScope curMod generics constraints classNames scope paramType <>
                formatDefaultHtml e
        nextParams = enrichKwdParamsHtmlModuleAndScope curMod generics constraints classNames scope kwdRow k
    in if isEmpty nextParams
       then param
       else param <> text ", " <> nextParams

formatTypeHtmlModule :: ModName -> Set Name -> QBinds -> Set Name -> Maybe Type -> Doc
formatTypeHtmlModule _ _ _ _ Nothing = empty
formatTypeHtmlModule curMod generics constraints classNames (Just t) =
    text ": <span class=\"type\">" <> text (renderTypeWithGenericsConstraintsAndClassesAndModule curMod generics constraints classNames t) <> text "</span>"

formatTypeHtmlModuleAndScope :: ModName -> Set Name -> QBinds -> Set Name -> String -> Maybe Type -> Doc
formatTypeHtmlModuleAndScope _ _ _ _ _ Nothing = empty
formatTypeHtmlModuleAndScope curMod generics constraints classNames scope (Just t) =
    text ": <span class=\"type\">" <> text (renderTypeWithGenericsConstraintsClassesModuleAndScope curMod generics constraints classNames scope t) <> text "</span>"

docParamsHtmlWithGenericsAndClassesModule :: ModName -> Set Name -> Set Name -> PosPar -> KwdPar -> Doc
docParamsHtmlWithGenericsAndClassesModule _ _ _ PosNIL KwdNIL = text "()"
docParamsHtmlWithGenericsAndClassesModule curMod generics classNames p k =
    text "(" <> docPosParamsHtmlWithGenericsAndClassesModule curMod generics classNames p <>
    docKwdParSepHtml p k <>
    docKwdParamsHtmlWithGenericsAndClassesModule curMod generics classNames k <> text ")"

docPosParamsHtmlWithGenericsAndClassesModule :: ModName -> Set Name -> Set Name -> PosPar -> Doc
docPosParamsHtmlWithGenericsAndClassesModule _ _ _ PosNIL = empty
docPosParamsHtmlWithGenericsAndClassesModule curMod generics classNames (PosPar n t e p) =
    -- Skip witness parameters
    if isWitnessParam n
    then docPosParamsHtmlWithGenericsAndClassesModule curMod generics classNames p
    else
        let param = text "<span class=\"param-name\">" <> pretty n <> text "</span>" <>
                    formatTypeHtmlModule curMod generics [] classNames t <>
                    formatDefaultHtml e
            nextParams = docPosParamsHtmlWithGenericsAndClassesModule curMod generics classNames p
        in if isEmpty nextParams
           then param
           else param <> text ", " <> nextParams
docPosParamsHtmlWithGenericsAndClassesModule curMod generics classNames (PosSTAR n t) =
    text "*<span class=\"param-name\">" <> pretty n <> text "</span>" <>
    formatTypeHtmlModule curMod generics [] classNames t

docKwdParamsHtmlWithGenericsAndClassesModule :: ModName -> Set Name -> Set Name -> KwdPar -> Doc
docKwdParamsHtmlWithGenericsAndClassesModule _ _ _ KwdNIL = empty
docKwdParamsHtmlWithGenericsAndClassesModule curMod generics classNames (KwdPar n t e k) =
    let param = text "<span class=\"param-name\">" <> pretty n <> text "</span>" <>
                formatTypeHtmlModule curMod generics [] classNames t <>
                formatDefaultHtml e
        nextParams = docKwdParamsHtmlWithGenericsAndClassesModule curMod generics classNames k
    in if isEmpty nextParams
       then param
       else param <> text ", " <> nextParams
docKwdParamsHtmlWithGenericsAndClassesModule curMod generics classNames (KwdSTAR n t) =
    text "**<span class=\"param-name\">" <> pretty n <> text "</span>" <>
    formatTypeHtmlModule curMod generics [] classNames t

-- | Document parameters with scope for generic type highlighting
docParamsHtmlWithGenericsAndClassesModuleAndScope :: ModName -> Set Name -> Set Name -> String -> PosPar -> KwdPar -> Doc
docParamsHtmlWithGenericsAndClassesModuleAndScope _ _ _ _ PosNIL KwdNIL = text "()"
docParamsHtmlWithGenericsAndClassesModuleAndScope curMod generics classNames scope p k =
    text "(" <> docPosParamsHtmlWithGenericsAndClassesModuleAndScope curMod generics classNames scope p <>
    docKwdParSepHtml p k <>
    docKwdParamsHtmlWithGenericsAndClassesModuleAndScope curMod generics classNames scope k <> text ")"

docPosParamsHtmlWithGenericsAndClassesModuleAndScope :: ModName -> Set Name -> Set Name -> String -> PosPar -> Doc
docPosParamsHtmlWithGenericsAndClassesModuleAndScope _ _ _ _ PosNIL = empty
docPosParamsHtmlWithGenericsAndClassesModuleAndScope curMod generics classNames scope (PosPar n t e p) =
    -- Skip witness parameters
    if isWitnessParam n
    then docPosParamsHtmlWithGenericsAndClassesModuleAndScope curMod generics classNames scope p
    else
        let param = text "<span class=\"param-name\">" <> pretty n <> text "</span>" <>
                    formatTypeHtmlModuleAndScope curMod generics [] classNames scope t <>
                    formatDefaultHtml e
            nextParams = docPosParamsHtmlWithGenericsAndClassesModuleAndScope curMod generics classNames scope p
        in if isEmpty nextParams
           then param
           else param <> text ", " <> nextParams
docPosParamsHtmlWithGenericsAndClassesModuleAndScope curMod generics classNames scope (PosSTAR n t) =
    text "*<span class=\"param-name\">" <> pretty n <> text "</span>" <>
    formatTypeHtmlModuleAndScope curMod generics [] classNames scope t

docKwdParamsHtmlWithGenericsAndClassesModuleAndScope :: ModName -> Set Name -> Set Name -> String -> KwdPar -> Doc
docKwdParamsHtmlWithGenericsAndClassesModuleAndScope _ _ _ _ KwdNIL = empty
docKwdParamsHtmlWithGenericsAndClassesModuleAndScope curMod generics classNames scope (KwdPar n t e k) =
    let param = text "<span class=\"param-name\">" <> pretty n <> text "</span>" <>
                formatTypeHtmlModuleAndScope curMod generics [] classNames scope t <>
                formatDefaultHtml e
        nextParams = docKwdParamsHtmlWithGenericsAndClassesModuleAndScope curMod generics classNames scope k
    in if isEmpty nextParams
       then param
       else param <> text ", " <> nextParams
docKwdParamsHtmlWithGenericsAndClassesModuleAndScope curMod generics classNames scope (KwdSTAR n t) =
    text "**<span class=\"param-name\">" <> pretty n <> text "</span>" <>
    formatTypeHtmlModuleAndScope curMod generics [] classNames scope t

docRetTypeHtmlWithGenericsConstraintsAndClassesModule :: ModName -> Set Name -> QBinds -> Set Name -> Maybe Type -> Doc
docRetTypeHtmlWithGenericsConstraintsAndClassesModule _ _ _ _ Nothing = empty
docRetTypeHtmlWithGenericsConstraintsAndClassesModule curMod generics constraints classNames (Just t) =
    text " → <span class=\"type\">" <> text (renderTypeWithGenericsConstraintsAndClassesAndModule curMod generics constraints classNames t) <> text "</span>"

-- | Document return type in HTML format with scope
docRetTypeHtmlWithGenericsConstraintsAndClassesModuleAndScope :: ModName -> Set Name -> QBinds -> Set Name -> String -> Maybe Type -> Doc
docRetTypeHtmlWithGenericsConstraintsAndClassesModuleAndScope _ _ _ _ _ Nothing = empty
docRetTypeHtmlWithGenericsConstraintsAndClassesModuleAndScope curMod generics constraints classNames scope (Just t) =
    text " → <span class=\"type\">" <> text (renderTypeWithGenericsConstraintsClassesModuleAndScope curMod generics constraints classNames scope t) <> text "</span>"

docAncestorsHtmlWithGenericsAndClassesModule :: ModName -> Set Name -> Set Name -> [WTCon] -> Doc
docAncestorsHtmlWithGenericsAndClassesModule _ _ _ [] = empty
docAncestorsHtmlWithGenericsAndClassesModule curMod generics classNames (a:_) =
    let (_, tc) = a
    in text "(<span class=\"type\">" <> text (renderTypeWithGenericsConstraintsAndClassesAndModule curMod generics [] classNames (TCon NoLoc tc)) <> text "</span>)"

-- | Extract class members (attributes and methods) from a class body
extractClassMembers :: Suite -> ([(Name, Maybe Type)], [(Name, QBinds, PosPar, KwdPar, Maybe Type, Maybe String)])
extractClassMembers stmts = foldr extractMember ([], []) stmts
  where
    extractMember (Assign _ [PVar _ n _] _) (attrs, methods) = ((n, Nothing) : attrs, methods)
    extractMember (VarAssign _ [PVar _ n ann] _) (attrs, methods) = ((n, ann) : attrs, methods)
    extractMember (Signature _ [n] (TSchema _ _ t) _) (attrs, methods) =
        case t of
            TFun {} -> (attrs, methods)  -- Function signatures are handled separately
            _ -> ((n, Just t) : attrs, methods)
    extractMember (Decl _ decls) (attrs, methods) =
        foldr extractDeclMember (attrs, methods) decls
    extractMember _ acc = acc

    extractDeclMember (Def _ n q p k ret body _ _ ddoc) (attrs, methods) =
        (attrs, (n, q, p, k, ret, ddoc) : methods)
    extractDeclMember _ acc = acc

-- | Extract actor members (public constants, internal attributes, and methods) from an actor body
extractActorMembers :: Suite -> ([(Name, Maybe Type)], [(Name, Maybe Type)], [(Name, QBinds, PosPar, KwdPar, Maybe Type, Maybe String)])
extractActorMembers stmts = foldr extractMember ([], [], []) stmts
  where
    extractMember (Assign _ [PVar _ n ann] _) (publics, internals, methods) = ((n, ann) : publics, internals, methods)
    extractMember (VarAssign _ [PVar _ n ann] _) (publics, internals, methods) = (publics, (n, ann) : internals, methods)
    extractMember (Signature _ [n] (TSchema _ _ t) _) (publics, internals, methods) =
        case t of
            TFun {} -> (publics, internals, methods)  -- Function signatures are handled separately
            _ -> ((n, Just t) : publics, internals, methods)  -- Type signatures for public constants
    extractMember (Decl _ decls) (publics, internals, methods) =
        foldr extractDeclMember (publics, internals, methods) decls
    extractMember _ acc = acc

    extractDeclMember (Def _ n q p k ret body _ _ ddoc) (publics, internals, methods) =
        (publics, internals, (n, q, p, k, ret, ddoc) : methods)
    extractDeclMember _ acc = acc

-- | Extract protocol methods from a protocol body
extractProtocolMethods :: Suite -> [(Name, QBinds, PosPar, KwdPar, Maybe Type, Maybe String)]
extractProtocolMethods stmts = foldr extractMethod [] stmts
  where
    extractMethod (Decl _ decls) methods = foldr extractDeclMethod methods decls
    extractMethod (Signature _ names (TSchema _ q t) _) methods =
        case t of
            TFun _ _ posRow kwdRow retType ->
                -- Convert signature to method format
                let (p, k) = rowsToParams posRow kwdRow
                in [(n, q, p, k, Just retType, Nothing) | n <- names] ++ methods
            _ -> [(n, q, PosNIL, KwdNIL, Just t, Nothing) | n <- names] ++ methods  -- Non-function signatures
    extractMethod _ methods = methods

    extractDeclMethod (Def _ n q p k ret body _ _ ddoc) methods =
        (n, q, p, k, ret, ddoc) : methods
    extractDeclMethod _ methods = methods

    -- Convert rows to parameters for protocol methods
    rowsToParams :: PosRow -> KwdRow -> (PosPar, KwdPar)
    rowsToParams posRow kwdRow = (PosNIL, KwdNIL)  -- Simplified for now

docClassBodyHtmlWithGenericsTypesAndClassesModule :: TEnv -> ModName -> Set Name -> Set Name -> Suite -> Doc
docClassBodyHtmlWithGenericsTypesAndClassesModule tenv curMod generics classNames body =
    let (attrs, methods) = extractClassMembers body
        attrDocs = if null attrs then empty else
                   text "<h3>Attributes</h3>" $+$
                   text "<div class=\"attributes\">" $+$
                   vcat (map (docAttributeHtmlWithGenericsAndClassesModule curMod generics classNames) attrs) $+$
                   text "</div>"
        methodDocs = if null methods then empty else
                     text "<h3>Methods</h3>" $+$
                     text "<div class=\"methods\">" $+$
                     vcat (map (docMethodHtmlWithGenericsTypesAndClassesModule tenv curMod generics classNames) methods) $+$
                     text "</div>"
    in attrDocs $+$ (if isEmpty attrDocs || isEmpty methodDocs then empty else blank) $+$ methodDocs

docProtocolBodyHtmlWithGenericsTypesAndClassesModule :: TEnv -> ModName -> Set Name -> Set Name -> Suite -> Doc
docProtocolBodyHtmlWithGenericsTypesAndClassesModule tenv curMod generics classNames body =
    let methods = extractProtocolMethods body
        methodDocs = if null methods then empty else
                     text "<h3>Required Methods</h3>" $+$
                     text "<div class=\"methods\">" $+$
                     vcat (map (docMethodHtmlWithGenericsTypesAndClassesModule tenv curMod generics classNames) methods) $+$
                     text "</div>"
    in methodDocs

docAttributeHtmlWithGenericsAndClassesModule :: ModName -> Set Name -> Set Name -> (Name, Maybe Type) -> Doc
docAttributeHtmlWithGenericsAndClassesModule curMod generics classNames (n, t) =
    text "<div class=\"attribute-item\"><code>" <> pretty n <> text "</code>" <>
    formatTypeHtmlModule curMod generics [] classNames t <> text "</div>"

docActorBodyHtmlWithGenericsTypesAndClassesModule :: TEnv -> ModName -> Set Name -> Set Name -> Suite -> Doc
docActorBodyHtmlWithGenericsTypesAndClassesModule tenv curMod generics classNames body =
    let (publics, internals, methods) = extractActorMembers body
        -- Enrich public constants with type information from TEnv
        enrichedPublics = map enrichAttribute publics
          where
            enrichAttribute (n, Nothing) =
                case lookup n tenv of
                    Just (NSig (TSchema _ _ t) _ _) -> (n, Just t)
                    Just (NDef (TSchema _ _ t) _ _) -> (n, Just t)
                    _ -> (n, Nothing)
            enrichAttribute attr = attr
        publicDocs = if null publics then empty else
                     text "<h3>Public Constants</h3>" $+$
                     text "<div class=\"attributes\">" $+$
                     vcat (map (docAttributeHtmlWithGenericsAndClassesModule curMod generics classNames) enrichedPublics) $+$
                     text "</div>"
        internalDocs = if null internals then empty else
                       text "<h3>Internal Attributes</h3>" $+$
                       text "<div class=\"attributes\">" $+$
                       vcat (map (docAttributeHtmlWithGenericsAndClassesModule curMod generics classNames) internals) $+$
                       text "</div>"
        methodDocs = if null methods then empty else
                     text "<h3>Methods</h3>" $+$
                     text "<div class=\"methods\">" $+$
                     vcat (map (docMethodHtmlWithGenericsTypesAndClassesModule tenv curMod generics classNames) methods) $+$
                     text "</div>"
    in publicDocs $+$ internalDocs $+$ methodDocs

docMethodHtmlWithGenericsTypesAndClassesModule :: TEnv -> ModName -> Set Name -> Set Name -> (Name, QBinds, PosPar, KwdPar, Maybe Type, Maybe String) -> Doc
docMethodHtmlWithGenericsTypesAndClassesModule tenv curMod generics classNames (n, q, p, k, ret, docstr) =
    let nl2br = intercalate "<br>" . lines
        inferredParams = case lookup n tenv of
            Just (NDef (TSchema _ _ (TFun _ _ posRow kwdRow retType)) _ _) ->
                (enrichParamsWithTypesHtmlAndClassesModule curMod generics [] classNames posRow kwdRow p k, Just retType)
            Just (NSig (TSchema _ _ (TFun _ _ posRow kwdRow retType)) _ _) ->
                (enrichParamsWithTypesHtmlAndClassesModule curMod generics [] classNames posRow kwdRow p k, Just retType)
            _ -> (docParamsHtmlWithGenericsAndClassesModule curMod generics classNames p k, ret)
        (params, retType) = inferredParams
        -- Extract method-specific generics
        methodGenerics = extractGenerics q
        allGenerics = Set.union generics methodGenerics
        genericsDoc = if null q
                     then empty
                     else docConstraintsHtml methodGenerics q
        methodSig = text "<div class=\"method-item\">" $+$
                    text "<div class=\"method-signature\"><code>" <> pretty n <> text "</code>" <>
                    genericsDoc <> params <> docRetTypeHtmlWithGenericsConstraintsAndClassesModule curMod allGenerics [] classNames retType <> text "</div>"
        methodDoc = case docstr of
            Just ds -> text "<div class=\"method-doc\">" <> text (nl2br $ htmlEscape ds) <> text "</div>"
            Nothing -> empty
    in methodSig $+$ methodDoc $+$ text "</div>"

-- | Helper functions for HTML generation
docGenericsHtml :: QBinds -> Doc
docGenericsHtml [] = empty
docGenericsHtml q = text "[" <> commaList q <> text "]"

-- | Render generics with highlighting for HTML
docGenericsHtmlWithHighlight :: Set Name -> QBinds -> Doc
docGenericsHtmlWithHighlight generics q = docGenericsHtmlWithHighlightAndScope generics "" q

-- | Render generics with highlighting and scope for HTML
docGenericsHtmlWithHighlightAndScope :: Set Name -> String -> QBinds -> Doc
docGenericsHtmlWithHighlightAndScope _ _ [] = empty
docGenericsHtmlWithHighlightAndScope generics scope q =
    let renderGeneric (QBind tv _) =
            let name = tvname tv
                nameStr = render (pretty name)
            in if Set.member name generics
               then text $ "<span class=\"generic-type\" data-generic=\"" ++ nameStr ++
                          "\" data-scope=\"" ++ scope ++
                          "\" data-tooltip=\"Generic type " ++ nameStr ++ "\n\nThis is a placeholder for any type.\nAll " ++
                          nameStr ++ "s must be the same type.\">" ++ nameStr ++ "</span>"
               else text nameStr
        genericsDoc = punctuate (text ", ") (map renderGeneric q)
    in text "[" <> hcat genericsDoc <> text "]"

-- | Document type constraints in HTML format
docConstraintsHtml :: Set Name -> QBinds -> Doc
docConstraintsHtml generics constraints = docConstraintsHtmlWithScope generics "" constraints

-- | Document type constraints in HTML format with scope
docConstraintsHtmlWithScope :: Set Name -> String -> QBinds -> Doc
docConstraintsHtmlWithScope _ _ [] = empty
docConstraintsHtmlWithScope generics scope constraints =
    let renderConstraint (QBind tv preds) =
            let name = tvname tv
                nameStr = render (pretty name)
                tooltip = if null preds
                    then "Generic type " ++ nameStr ++ "\n\nThis is a placeholder for any type.\nAll " ++
                         nameStr ++ "s must be the same type."
                    else "Generic type " ++ nameStr ++ "\n\nThis is a placeholder for any type.\nAll " ++
                         nameStr ++ "s must be the same type.\n\n" ++ nameStr ++ " must support:\n" ++
                         concatMap (\p -> "  • " ++ formatConstraintName p ++ " protocol (" ++ formatExampleForPred p ++ ")\n") preds
                nameDoc = if Set.member name generics
                    then text $ "<span class=\"generic-type\" data-generic=\"" ++ nameStr ++
                               "\" data-scope=\"" ++ scope ++
                               "\" data-tooltip=\"" ++ tooltip ++ "\">" ++ nameStr ++ "</span>"
                    else text nameStr
                formatConstraintName (TC qn _) = render . pretty . simplifyQName $ qn

                formatExampleForPred :: TCon -> String
                formatExampleForPred (TC (NoQ (Name _ "Plus")) _) = "+ operator"
                formatExampleForPred (TC (NoQ (Name _ "Minus")) _) = "- operator"
                formatExampleForPred (TC (NoQ (Name _ "Times")) _) = "* operator"
                formatExampleForPred (TC (NoQ (Name _ "Divide")) _) = "/ operator"
                formatExampleForPred (TC (NoQ (Name _ "Eq")) _) = "== operator"
                formatExampleForPred (TC (NoQ (Name _ "Ord")) _) = "< > operators"
                formatExampleForPred (TC (NoQ (Name _ "Hash")) _) = "hash function"
                formatExampleForPred _ = "protocol methods"

                formatExampleOperators :: [TCon] -> String
                formatExampleOperators preds = intercalate ", " $ map getExample preds
                  where
                    getExample (TC (NoQ (Name _ "Plus")) _) = "+ operator"
                    getExample (TC (NoQ (Name _ "Minus")) _) = "- operator"
                    getExample (TC (NoQ (Name _ "Times")) _) = "* operator"
                    getExample (TC (NoQ (Name _ "Divide")) _) = "/ operator"
                    getExample (TC (NoQ (Name _ "Eq")) _) = "== operator"
                    getExample (TC (NoQ (Name _ "Ord")) _) = "< > operators"
                    getExample (TC (NoQ (Name _ "Hash")) _) = "hash function"
                    getExample _ = "protocol methods"
                -- Format predicates/constraints
                predsDoc = if null preds
                    then empty
                    else text "(" <> hcat (punctuate (text ", ") (map formatConstraint preds)) <> text ")"
                formatConstraint (TC qn _) = text . render . pretty . simplifyQName $ qn
            in nameDoc <> predsDoc
        constraintsDoc = punctuate (text ", ") (map renderConstraint constraints)
    in text "[" <> hcat constraintsDoc <> text "]"

-- | Parameter rendering with generic type support
docParamsHtmlWithGenerics :: Set Name -> PosPar -> KwdPar -> Doc
docParamsHtmlWithGenerics generics p k =
    text "(" <> docPosParHtmlWithGenerics generics p <> docKwdParSepHtml p k <> docKwdParHtmlWithGenerics generics k <> text ")"

-- | Parameter rendering with generic type support and class links
docParamsHtmlWithGenericsAndClasses :: Set Name -> Set Name -> PosPar -> KwdPar -> Doc
docParamsHtmlWithGenericsAndClasses generics classNames p k =
    text "(" <> docPosParHtmlWithGenericsAndClasses generics classNames p <> docKwdParSepHtml p k <> docKwdParHtmlWithGenericsAndClasses generics classNames k <> text ")"

docPosParHtmlWithGenerics :: Set Name -> PosPar -> Doc
docPosParHtmlWithGenerics _ PosNIL = empty
docPosParHtmlWithGenerics generics (PosPar n t e p) =
    let param = text "<span class=\"param-name\">" <> pretty n <> text "</span>" <> formatTypeHtmlWithGenerics generics t <> formatDefaultHtml e
    in case p of
        PosNIL -> param
        _ -> param <> text ", " <> docPosParHtmlWithGenerics generics p
docPosParHtmlWithGenerics generics (PosSTAR n t) =
    text "*<span class=\"param-name\">" <> pretty n <> text "</span>" <> formatTypeHtmlWithGenerics generics t

docKwdParHtmlWithGenerics :: Set Name -> KwdPar -> Doc
docKwdParHtmlWithGenerics _ KwdNIL = empty
docKwdParHtmlWithGenerics generics (KwdPar n t e k) =
    let param = text "<span class=\"param-name\">" <> pretty n <> text "</span>" <> formatTypeHtmlWithGenerics generics t <> formatDefaultHtml e
    in case k of
        KwdNIL -> param
        _ -> param <> text ", " <> docKwdParHtmlWithGenerics generics k
docKwdParHtmlWithGenerics generics (KwdSTAR n t) =
    text "**<span class=\"param-name\">" <> pretty n <> text "</span>" <> formatTypeHtmlWithGenerics generics t

docPosParHtmlWithGenericsAndClasses :: Set Name -> Set Name -> PosPar -> Doc
docPosParHtmlWithGenericsAndClasses _ _ PosNIL = empty
docPosParHtmlWithGenericsAndClasses generics classNames (PosPar n t e p) =
    let param = text "<span class=\"param-name\">" <> pretty n <> text "</span>" <> formatTypeHtmlWithGenericsAndClasses generics classNames t <> formatDefaultHtml e
    in case p of
        PosNIL -> param
        _ -> param <> text ", " <> docPosParHtmlWithGenericsAndClasses generics classNames p
docPosParHtmlWithGenericsAndClasses generics classNames (PosSTAR n t) =
    text "*<span class=\"param-name\">" <> pretty n <> text "</span>" <> formatTypeHtmlWithGenericsAndClasses generics classNames t

docKwdParHtmlWithGenericsAndClasses :: Set Name -> Set Name -> KwdPar -> Doc
docKwdParHtmlWithGenericsAndClasses _ _ KwdNIL = empty
docKwdParHtmlWithGenericsAndClasses generics classNames (KwdPar n t e k) =
    let param = text "<span class=\"param-name\">" <> pretty n <> text "</span>" <> formatTypeHtmlWithGenericsAndClasses generics classNames t <> formatDefaultHtml e
    in case k of
        KwdNIL -> param
        _ -> param <> text ", " <> docKwdParHtmlWithGenericsAndClasses generics classNames k
docKwdParHtmlWithGenericsAndClasses generics classNames (KwdSTAR n t) =
    text "**<span class=\"param-name\">" <> pretty n <> text "</span>" <> formatTypeHtmlWithGenericsAndClasses generics classNames t

formatTypeHtmlWithGenerics :: Set Name -> Maybe Type -> Doc
formatTypeHtmlWithGenerics generics t = formatTypeHtmlWithGenericsAndConstraints generics [] t

formatTypeHtmlWithGenericsAndClasses :: Set Name -> Set Name -> Maybe Type -> Doc
formatTypeHtmlWithGenericsAndClasses generics classNames t = formatTypeHtmlWithGenericsConstraintsAndClasses generics [] classNames t

formatTypeHtmlWithGenericsAndConstraints :: Set Name -> QBinds -> Maybe Type -> Doc
formatTypeHtmlWithGenericsAndConstraints _ _ Nothing = empty
formatTypeHtmlWithGenericsAndConstraints generics constraints (Just t) =
    text ": <span class=\"type\">" <> text (renderTypeWithGenericsAndConstraints generics constraints t) <> text "</span>"

formatTypeHtmlWithGenericsConstraintsAndClasses :: Set Name -> QBinds -> Set Name -> Maybe Type -> Doc
formatTypeHtmlWithGenericsConstraintsAndClasses _ _ _ Nothing = empty
formatTypeHtmlWithGenericsConstraintsAndClasses generics constraints classNames (Just t) =
    text ": <span class=\"type\">" <> text (renderTypeWithGenericsConstraintsAndClasses generics constraints classNames t) <> text "</span>"

docRetTypeHtmlWithGenerics :: Set Name -> Maybe Type -> Doc
docRetTypeHtmlWithGenerics generics t = docRetTypeHtmlWithGenericsAndConstraints generics [] t

docRetTypeHtmlWithGenericsAndConstraints :: Set Name -> QBinds -> Maybe Type -> Doc
docRetTypeHtmlWithGenericsAndConstraints _ _ Nothing = empty
docRetTypeHtmlWithGenericsAndConstraints generics constraints (Just t) =
    text " → <span class=\"type\">" <> text (renderTypeWithGenericsAndConstraints generics constraints t) <> text "</span>"

docRetTypeHtmlWithGenericsConstraintsAndClasses :: Set Name -> QBinds -> Set Name -> Maybe Type -> Doc
docRetTypeHtmlWithGenericsConstraintsAndClasses _ _ _ Nothing = empty
docRetTypeHtmlWithGenericsConstraintsAndClasses generics constraints classNames (Just t) =
    text " → <span class=\"type\">" <> text (renderTypeWithGenericsConstraintsAndClasses generics constraints classNames t) <> text "</span>"

docAncestorsHtmlWithGenerics :: Pretty a => Set Name -> [a] -> Doc
docAncestorsHtmlWithGenerics _ [] = empty
docAncestorsHtmlWithGenerics _ as = text "(" <> commaList as <> text ")"

docAncestorsHtmlWithGenericsAndClasses :: Pretty a => Set Name -> Set Name -> [a] -> Doc
docAncestorsHtmlWithGenericsAndClasses _ _ [] = empty
docAncestorsHtmlWithGenericsAndClasses _ _ as = text "(" <> commaList as <> text ")"

-- | Original parameter rendering (without generics)
docParamsHtml :: PosPar -> KwdPar -> Doc
docParamsHtml p k = text "(" <> docPosParHtml p <> docKwdParSepHtml p k <> docKwdParHtml k <> text ")"

docKwdParSepHtml :: PosPar -> KwdPar -> Doc
docKwdParSepHtml PosNIL KwdNIL = empty
docKwdParSepHtml PosNIL _ = empty
docKwdParSepHtml _ KwdNIL = empty
docKwdParSepHtml _ _ = text ", "

docPosParHtml :: PosPar -> Doc
docPosParHtml PosNIL = empty
docPosParHtml (PosPar n t e p) =
    let param = text "<span class=\"param-name\">" <> pretty n <> text "</span>" <> formatTypeHtml t <> formatDefaultHtml e
    in case p of
        PosNIL -> param
        _ -> param <> text ", " <> docPosParHtml p
docPosParHtml (PosSTAR n t) = text "*<span class=\"param-name\">" <> pretty n <> text "</span>" <> formatTypeHtml t

docKwdParHtml :: KwdPar -> Doc
docKwdParHtml KwdNIL = empty
docKwdParHtml (KwdPar n t e k) =
    let param = text "<span class=\"param-name\">" <> pretty n <> text "</span>" <> formatTypeHtml t <> formatDefaultHtml e
    in case k of
        KwdNIL -> param
        _ -> param <> text ", " <> docKwdParHtml k
docKwdParHtml (KwdSTAR n t) = text "**<span class=\"param-name\">" <> pretty n <> text "</span>" <> formatTypeHtml t

formatTypeHtml :: Maybe Type -> Doc
formatTypeHtml Nothing = empty
formatTypeHtml (Just t) = text ": <span class=\"type\">" <> pretty (SimplifiedType t) <> text "</span>"

formatDefaultHtml :: Maybe Expr -> Doc
formatDefaultHtml Nothing = empty
formatDefaultHtml (Just e) = text " = <span class=\"default-value\">" <> pretty e <> text "</span>"

docRetTypeHtml :: Maybe Type -> Doc
docRetTypeHtml Nothing = empty
docRetTypeHtml (Just t) = text " → <span class=\"type\">" <> pretty (SimplifiedType t) <> text "</span>"

docAncestorsHtml :: Pretty a => [a] -> Doc
docAncestorsHtml [] = empty
docAncestorsHtml as = text "(" <> commaList as <> text ")"

-- | Document class body in HTML with generic type support
docClassBodyHtmlWithGenerics :: Set Name -> Suite -> Doc
docClassBodyHtmlWithGenerics = docClassBodyHtmlWithGenericsAndTypes []

-- | Document class body in HTML with generic type support and type environment
docClassBodyHtmlWithGenericsAndTypes :: TEnv -> Set Name -> Suite -> Doc
docClassBodyHtmlWithGenericsAndTypes tenv generics stmts = docClassBodyHtmlWithGenericsTypesAndClasses tenv generics Set.empty stmts

-- | Document class body in HTML with generic type support, type environment and class links
docClassBodyHtmlWithGenericsTypesAndClasses :: TEnv -> Set Name -> Set Name -> Suite -> Doc
docClassBodyHtmlWithGenericsTypesAndClasses tenv generics classNames stmts =
    let (attrs, methods) = partitionClassMembersHtmlWithGenericsTypesAndClasses tenv generics classNames stmts
        attrsDoc = if null attrs
                   then empty
                   else text "<div class=\"attributes\">" $+$
                        text "<h3>Attributes:</h3>" $+$
                        text "<ul>" $+$
                        vcat (map wrapLi attrs) $+$
                        text "</ul>" $+$
                        text "</div>"
        methodsDoc = if null methods
                     then empty
                     else text "<div class=\"methods\">" $+$
                          text "<h3>Methods:</h3>" $+$
                          text "<ul>" $+$
                          vcat methods $+$
                          text "</ul>" $+$
                          text "</div>"
    in case (isEmpty attrsDoc, isEmpty methodsDoc) of
        (True, True) -> empty
        (False, True) -> attrsDoc
        (True, False) -> methodsDoc
        (False, False) -> attrsDoc $+$ methodsDoc
  where
    wrapLi doc = text "<li>" <> doc <> text "</li>"

-- | Document actor body in HTML with generic type support, type environment and class links
docActorBodyHtmlWithGenericsTypesAndClasses :: TEnv -> Set Name -> Set Name -> Suite -> Doc
docActorBodyHtmlWithGenericsTypesAndClasses tenv generics classNames stmts =
    let (publicConstants, internalAttrs, methods) = partitionActorMembersHtmlWithGenericsTypesAndClasses tenv generics classNames stmts
        publicDoc = if null publicConstants
                    then empty
                    else text "<h3>Public Constants</h3>" $+$
                         text "<div class=\"attributes\">" $+$
                         vcat publicConstants $+$
                         text "</div>"
        internalDoc = if null internalAttrs
                      then empty
                      else text "<h3>Internal Attributes</h3>" $+$
                           text "<div class=\"attributes\">" $+$
                           vcat internalAttrs $+$
                           text "</div>"
        methodsDoc = if null methods
                     then empty
                     else text "<h3>Methods</h3>" $+$
                          text "<div class=\"methods\">" $+$
                          vcat (map renderMethod methods) $+$
                          text "</div>"
    in vcat $ filter (not . isEmpty) [publicDoc, internalDoc, methodsDoc]
  where
    renderMethod (n, q, p, k, ret, docstr) =
        let nl2br = intercalate "<br>" . lines
            methodGenerics = extractGenerics q
            allGenerics = Set.union generics methodGenerics
            inferredParams = case lookup n tenv of
                Just (NDef (TSchema _ _ (TFun _ _ posRow kwdRow retType)) _ _) ->
                    (enrichParamsWithTypesHtmlAndClasses allGenerics [] classNames posRow kwdRow p k, Just retType)
                Just (NSig (TSchema _ _ (TFun _ _ posRow kwdRow retType)) _ _) ->
                    (enrichParamsWithTypesHtmlAndClasses allGenerics [] classNames posRow kwdRow p k, Just retType)
                _ -> (docParamsHtmlWithGenericsAndClasses allGenerics classNames p k, ret)
            (params, retType) = inferredParams
            genericsDoc = if null q
                         then empty
                         else docConstraintsHtml methodGenerics q
            methodSig = text "<div class=\"method-item\">" $+$
                        text "<div class=\"method-signature\"><code>" <> pretty n <> text "</code>" <>
                        genericsDoc <> params <> docRetTypeHtmlWithGenericsConstraintsAndClasses allGenerics [] classNames retType <> text "</div>"
            methodDoc = case docstr of
                Just ds -> text "<div class=\"method-doc\">" <> text (nl2br $ htmlEscape ds) <> text "</div>"
                Nothing -> empty
        in methodSig $+$ methodDoc $+$ text "</div>"

-- | Partition actor members into public constants, internal attributes, and methods
partitionActorMembersHtmlWithGenericsTypesAndClasses :: TEnv -> Set Name -> Set Name -> Suite -> ([Doc], [Doc], [(Name, QBinds, PosPar, KwdPar, Maybe Type, Maybe String)])
partitionActorMembersHtmlWithGenericsTypesAndClasses tenv generics classNames stmts = foldl partition ([], [], []) stmts
  where
    partition (publics, internals, methods) (Assign _ [PVar _ n ann] _) =
        let enrichedType = case ann of
                Just t -> Just t
                Nothing -> case lookup n tenv of
                    Just (NSig (TSchema _ _ t) _ _) -> Just t
                    Just (NDef (TSchema _ _ t) _ _) -> Just t
                    _ -> Nothing
            doc = case enrichedType of
                Just t -> text "<div class=\"attribute-item\"><code>" <> pretty n <> text "</code>: <span class=\"type\">" <>
                          text (renderTypeWithGenericsConstraintsAndClasses generics [] classNames t) <> text "</span></div>"
                Nothing -> text "<div class=\"attribute-item\"><code>" <> pretty n <> text "</code></div>"
        in (publics ++ [doc], internals, methods)
    partition (publics, internals, methods) (VarAssign _ [PVar _ n ann] _) =
        let doc = case ann of
                Just t -> text "<div class=\"attribute-item\"><code>" <> pretty n <> text "</code>: <span class=\"type\">" <>
                          text (renderTypeWithGenericsConstraintsAndClasses generics [] classNames t) <> text "</span></div>"
                Nothing -> text "<div class=\"attribute-item\"><code>" <> pretty n <> text "</code></div>"
        in (publics, internals ++ [doc], methods)
    partition (publics, internals, methods) (Signature _ [n] (TSchema _ _ t) _) =
        case t of
            TFun {} -> (publics, internals, methods)  -- Function signatures are handled separately
            _ -> let doc = text "<div class=\"attribute-item\"><code>" <> pretty n <> text "</code>: <span class=\"type\">" <>
                          text (renderTypeWithGenericsConstraintsAndClasses generics [] classNames t) <> text "</span></div>"
                 in (publics ++ [doc], internals, methods)  -- Type signatures for public constants
    partition (publics, internals, methods) (Decl _ decls) =
        foldl extractDeclMember (publics, internals, methods) decls
    partition acc _ = acc

    extractDeclMember (publics, internals, methods) (Def _ n q p k ret body _ _ ddoc) =
        (publics, internals, methods ++ [(n, q, p, k, ret, ddoc)])
    extractDeclMember acc _ = acc

-- | Document protocol body in HTML with generic type support
docProtocolBodyHtmlWithGenerics :: Set Name -> Suite -> Doc
docProtocolBodyHtmlWithGenerics = docProtocolBodyHtmlWithGenericsAndTypes []

-- | Document protocol body in HTML with generic type support and type environment
docProtocolBodyHtmlWithGenericsAndTypes :: TEnv -> Set Name -> Suite -> Doc
docProtocolBodyHtmlWithGenericsAndTypes tenv generics stmts = docProtocolBodyHtmlWithGenericsTypesAndClasses tenv generics Set.empty stmts

-- | Document protocol body in HTML with generic type support, type environment and class links
docProtocolBodyHtmlWithGenericsTypesAndClasses :: TEnv -> Set Name -> Set Name -> Suite -> Doc
docProtocolBodyHtmlWithGenericsTypesAndClasses tenv generics classNames stmts =
    let methods = concatMap (extractMethodsHtmlWithGenericsTypesAndClasses tenv generics classNames) stmts
    in if null methods
       then empty
       else text "<div class=\"methods\">" $+$
            text "<h3>Methods:</h3>" $+$
            text "<ul>" $+$
            vcat methods $+$
            text "</ul>" $+$
            text "</div>"


-- | Partition class members for HTML with generics
partitionClassMembersHtmlWithGenerics :: Set Name -> Suite -> ([Doc], [Doc])
partitionClassMembersHtmlWithGenerics = partitionClassMembersHtmlWithGenericsAndTypes []

-- | Partition class members for HTML with generics and type environment
partitionClassMembersHtmlWithGenericsAndTypes :: TEnv -> Set Name -> Suite -> ([Doc], [Doc])
partitionClassMembersHtmlWithGenericsAndTypes tenv generics stmts = partitionClassMembersHtmlWithGenericsTypesAndClasses tenv generics Set.empty stmts

-- | Partition class members for HTML with generics, type environment and class links
partitionClassMembersHtmlWithGenericsTypesAndClasses :: TEnv -> Set Name -> Set Name -> Suite -> ([Doc], [Doc])
partitionClassMembersHtmlWithGenericsTypesAndClasses tenv generics classNames stmts = foldl partition ([], []) stmts
  where
    partition (attrs, methods) (Signature _ vs sc d) = (attrs ++ [docAttributeHtmlWithGenericsAndClasses generics classNames vs sc], methods)
    partition (attrs, methods) (VarAssign _ [PVar _ n ann] _) =
        case ann of
            Just t -> (attrs ++ [text "<div class=\"attribute-item\"><code>" <> pretty n <> text "</code>: <span class=\"type\">" <>
                      text (renderTypeWithGenericsConstraintsAndClasses generics [] classNames t) <> text "</span></div>"], methods)
            Nothing -> (attrs ++ [text "<div class=\"attribute-item\"><code>" <> pretty n <> text "</code></div>"], methods)
    partition (attrs, methods) (Assign _ [PVar _ n ann] _) =
        case ann of
            Just t -> (attrs ++ [text "<div class=\"attribute-item\"><code>" <> pretty n <> text "</code>: <span class=\"type\">" <>
                      text (renderTypeWithGenericsConstraintsAndClasses generics [] classNames t) <> text "</span></div>"], methods)
            Nothing -> (attrs ++ [text "<div class=\"attribute-item\"><code>" <> pretty n <> text "</code></div>"], methods)
    partition (attrs, methods) (Decl _ decls) = (attrs, methods ++ map (docMethodHtmlWithGenericsTypesAndClasses tenv generics classNames) decls)
    partition acc _ = acc

-- | Extract methods for HTML with generics
extractMethodsHtmlWithGenerics :: Set Name -> Stmt -> [Doc]
extractMethodsHtmlWithGenerics = extractMethodsHtmlWithGenericsAndTypes []

-- | Extract methods for HTML with generics and type environment
extractMethodsHtmlWithGenericsAndTypes :: TEnv -> Set Name -> Stmt -> [Doc]
extractMethodsHtmlWithGenericsAndTypes tenv generics stmts = extractMethodsHtmlWithGenericsTypesAndClasses tenv generics Set.empty stmts

-- | Extract methods for HTML with generics, type environment and class links
extractMethodsHtmlWithGenericsTypesAndClasses :: TEnv -> Set Name -> Set Name -> Stmt -> [Doc]
extractMethodsHtmlWithGenericsTypesAndClasses tenv generics classNames (Decl _ decls) = map (docMethodHtmlWithGenericsTypesAndClasses tenv generics classNames) decls
extractMethodsHtmlWithGenericsTypesAndClasses tenv generics classNames (Signature _ vs sc d) = [docMethodSignatureHtmlWithGenericsAndClasses generics classNames vs sc]
extractMethodsHtmlWithGenericsTypesAndClasses _ _ _ _ = []

-- | Document an attribute in HTML with generics
docAttributeHtmlWithGenerics :: Set Name -> [Name] -> TSchema -> Doc
docAttributeHtmlWithGenerics generics vs ts = docAttributeHtmlWithGenericsAndClasses generics Set.empty vs ts

-- | Document an attribute in HTML with generics and class links
docAttributeHtmlWithGenericsAndClasses :: Set Name -> Set Name -> [Name] -> TSchema -> Doc
docAttributeHtmlWithGenericsAndClasses generics classNames vs (TSchema _ _ t) =
    text "<code>" <> commaList vs <> text "</code>: <span class=\"type\">" <> text (renderTypeWithGenericsConstraintsAndClasses generics [] classNames t) <> text "</span>"

-- | Document a method in HTML with generics
docMethodHtmlWithGenerics :: Set Name -> Decl -> Doc
docMethodHtmlWithGenerics = docMethodHtmlWithGenericsAndTypes []

-- | Document a method in HTML with generics and type environment
docMethodHtmlWithGenericsAndTypes :: TEnv -> Set Name -> Decl -> Doc
docMethodHtmlWithGenericsAndTypes tenv generics = docMethodHtmlWithGenericsTypesAndClasses tenv generics Set.empty

-- | Document a method in HTML with generics, type environment and class links
docMethodHtmlWithGenericsTypesAndClasses :: TEnv -> Set Name -> Set Name -> Decl -> Doc
docMethodHtmlWithGenericsTypesAndClasses tenv generics classNames (Def _ n q p k a b _ _ ddoc) =
    let methodGenerics = Set.union generics (extractGenerics q)
        -- Look up inferred type information
        (paramsWithTypes, inferredRetType, constraints) = case lookup n tenv of
            Just (NDef (TSchema _ qConstraints (TFun _ _ posRow kwdRow retType)) _ _) ->
                (enrichParamsWithTypesHtmlAndClasses methodGenerics qConstraints classNames posRow kwdRow p k, Just retType, qConstraints)
            Just (NSig (TSchema _ qConstraints (TFun _ _ posRow kwdRow retType)) _ _) ->
                (enrichParamsWithTypesHtmlAndClasses methodGenerics qConstraints classNames posRow kwdRow p k, Just retType, qConstraints)
            _ -> (docParamsHtmlWithGenericsAndClasses methodGenerics classNames p k, a, [])

        -- Use inferred return type if available, otherwise use annotation
        actualRetType = case inferredRetType of
            Just t -> docRetTypeHtmlWithGenericsConstraintsAndClasses methodGenerics constraints classNames (Just t)
            Nothing -> docRetTypeHtmlWithGenericsConstraintsAndClasses methodGenerics constraints classNames a

        signature = text "<div class=\"method-signature type-context\"><code>" <> pretty n <> text "</code>" <>
                    docGenericsHtmlWithHighlight methodGenerics q <> paramsWithTypes <> actualRetType <> text "</div>"
        docstr = case ddoc of
            Just ds -> text "<div class=\"method-doc\">" <> text (nl2br $ htmlEscape ds) <> text "</div>"
            Nothing -> empty
    in text "<li class=\"method-item\">" <> signature <> docstr <> text "</li>"
  where
    nl2br = intercalate "<br>" . lines
docMethodHtmlWithGenericsTypesAndClasses _ _ _ _ = empty

-- | Document a method signature in HTML with generics
docMethodSignatureHtmlWithGenerics :: Set Name -> [Name] -> TSchema -> Doc
docMethodSignatureHtmlWithGenerics generics vs ts = docMethodSignatureHtmlWithGenericsAndClasses generics Set.empty vs ts

-- | Document a method signature in HTML with generics and class links
docMethodSignatureHtmlWithGenericsAndClasses :: Set Name -> Set Name -> [Name] -> TSchema -> Doc
docMethodSignatureHtmlWithGenericsAndClasses generics classNames vs (TSchema _ _ t) =
    text "<li class=\"type-context\"><code>" <> commaList vs <> text "</code>: <span class=\"type\">" <>
    text (renderTypeWithGenericsConstraintsAndClasses generics [] classNames t) <> text "</span></li>"

-- | Convert ModName to string representation
modNameToString :: ModName -> String
modNameToString (ModName names) = intercalate "." (map nstr names)

-- | Generate HTML documentation index for a list of modules
generateDocIndex :: FilePath -> [(ModName, Maybe String)] -> IO ()
generateDocIndex docDir tasks = do
    let indexFile = docDir </> "index.html"
        sortedTasks = sortBy (comparing (\(mn,_) -> modNameToString mn)) tasks
        moduleEntries = concatMap generateModuleEntry sortedTasks
        indexHtml = unlines $
            [ "<!DOCTYPE html>"
            , "<html lang=\"en\">"
            , "<head>"
            , "  <meta charset=\"UTF-8\">"
            , "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">"
            , "  <title>Acton Documentation Index</title>"
            , "  <style>"
            , htmlStyles
            , "  </style>"
            , "</head>"
            , "<body>"
            , "  <div class=\"container\">"
            , "    <h1>Acton Project Documentation</h1>"
            , "    <p>Documentation for all modules in this project.</p>"
            , "    <ul class=\"module-list\">"
            ] ++ moduleEntries ++
            [ "    </ul>"
            , "  </div>"
            , "</body>"
            , "</html>"
            ]
    writeFileUtf8Atomic indexFile indexHtml
  where
    generateModuleEntry :: (ModName, Maybe String) -> [String]
    generateModuleEntry (mn, mDoc) =
        let modPaths = modPath mn  -- Use different name to avoid shadowing
            modName = modNameToString mn
            htmlFile = if null modPaths
                       then "unnamed.html"
                       else joinPath (init modPaths) </> last modPaths <.> "html"
            docString = maybe "" (takeWhile (/= '\n')) mDoc
        in [ "      <li class=\"module-item\">"
           , "        <a href=\"" ++ htmlFile ++ "\" class=\"module-link\">"
           , "          <span class=\"module-path\">" ++ modName ++ "</span>"
           , "          <div class=\"module-doc\">" ++ htmlEscape docString ++ "</div>"
           , "        </a>"
           , "      </li>"
           ]
