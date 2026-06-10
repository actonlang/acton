{-# LANGUAGE ScopedTypeVariables #-}

module Acton.Completion
  ( Completion(..)
  , CompletionKind(..)
  , CallSignature(..)
  , HoverInfo(..)
  , SignatureParameter(..)
  , CallRequest(..)
  , ArgumentRequest(..)
  , MemberRequest(..)
  , argumentCompletions
  , argumentCompletionsWithEnv
  , callContextAt
  , callSignatures
  , callSignaturesWithEnv
  , completionImportKey
  , hoverInfo
  , hoverInfoWithEnv
  , memberContextAt
  , memberCompletions
  , memberCompletionsWithEnv
  , prepareCompletionEnv
  ) where

import qualified Control.Exception as E
import Control.Monad (foldM)
import qualified Control.Monad.Trans.State.Strict as St
import Data.Char (isAlpha, isAlphaNum, isSpace)
import Data.List (find, findIndex, intercalate, isPrefixOf, nubBy)
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.HashMap.Strict as HM
import qualified InterfaceFiles as IF
import Text.Megaparsec (eof, runParser)

import qualified Acton.Env as Env
import qualified Acton.NameInfo as I
import qualified Acton.Parser as P
import qualified Acton.Syntax as S
import Utils (prstr)

data CompletionKind
  = CompletionField
  | CompletionMethod
  | CompletionProperty
  | CompletionValue
  | CompletionKeyword
  deriving (Eq, Show)

data Completion = Completion
  { completionLabel :: String
  , completionKind :: CompletionKind
  , completionDetail :: Maybe String
  } deriving (Eq, Show)

data MemberRequest = MemberRequest
  { memberReceiver :: String
  , memberPrefix :: String
  } deriving (Eq, Show)

data CallRequest = CallRequest
  { callTarget :: [String]
  , callActiveParameter :: Int
  , callArgumentText :: String
  } deriving (Eq, Show)

data ArgumentRequest = ArgumentRequest
  { argumentTarget :: [String]
  , argumentPrefix :: String
  , argumentSuppliedKeywords :: [String]
  } deriving (Eq, Show)

data SignatureParameter = SignatureParameter
  { signatureParameterLabel :: String
  } deriving (Eq, Show)

data CallSignature = CallSignature
  { callSignatureLabel :: String
  , callSignatureParameters :: [SignatureParameter]
  , callSignatureActiveParameter :: Int
  } deriving (Eq, Show)

data HoverInfo = HoverInfo
  { hoverLabel :: String
  , hoverDetail :: String
  , hoverDocumentation :: Maybe String
  } deriving (Eq, Show)

data StringState = StringState Char Bool deriving (Eq, Show)

data SourceContext = SourceContext
  { sourceClass :: Maybe ClassContext
  , sourceDef :: Maybe DefContext
  , sourceLocalBindings :: [LocalBinding]
  } deriving (Eq, Show)

data ClassContext = ClassContext
  { className :: String
  , classParents :: [String]
  , classIndent :: Int
  } deriving (Eq, Show)

data DefContext = DefContext
  { defName :: String
  , defParams :: [Param]
  , defIndent :: Int
  } deriving (Eq, Show)

data Param = Param
  { paramName :: String
  , paramAnn :: Maybe String
  } deriving (Eq, Show)

data LocalBinding = LocalBinding
  { localName :: String
  , localClassName :: Maybe String
  , localClassIndent :: Maybe Int
  , localDefName :: String
  , localDefIndent :: Int
  , localKind :: LocalKind
  } deriving (Eq, Show)

data LocalKind
  = LocalAnnotated String
  | LocalCallResult [String]
  | LocalMemberPath [String]
  deriving (Eq, Show)

data HoverTarget = HoverTarget
  { hoverResolvedType :: S.Type
  , hoverResolvedDoc :: Maybe String
  } deriving (Eq, Show)

data KeywordParameter = KeywordParameter
  { keywordParameterName :: String
  , keywordParameterType :: S.Type
  } deriving (Eq, Show)

data Scope = ScopeClass ClassContext | ScopeDef DefContext deriving (Eq, Show)

data ScanState = ScanState
  { scanScopes :: [Scope]
  , scanLocals :: [LocalBinding]
  } deriving (Eq, Show)

memberCompletions :: Env.Env0 -> [FilePath] -> S.ModName -> FilePath -> String -> Int -> IO [Completion]
memberCompletions baseEnv searchPath modName fileName src cursor = do
  res <- E.try $ do
    case memberContextAt src cursor of
      Nothing -> return []
      Just req -> do
        env <- prepareCompletionEnv baseEnv searchPath modName fileName src
        let ctx = scanSourceContext src cursor
            comps = completeMember env ctx req
        E.evaluate (forceCompletions comps)
  case res of
    Left (_ :: E.SomeException) -> return []
    Right comps -> return comps

memberCompletionsWithEnv :: Env.Env0 -> String -> Int -> [Completion]
memberCompletionsWithEnv env src cursor =
  case memberContextAt src cursor of
    Nothing -> []
    Just req ->
      let ctx = scanSourceContext src cursor
      in completeMember env ctx req

argumentCompletions :: Env.Env0 -> [FilePath] -> S.ModName -> FilePath -> String -> Int -> IO [Completion]
argumentCompletions baseEnv searchPath modName fileName src cursor = do
  res <- E.try $ do
    case argumentContextAt src cursor of
      Nothing -> return []
      Just req -> do
        env <- prepareCompletionEnv baseEnv searchPath modName fileName src
        let ctx = scanSourceContext src cursor
            comps = completeArguments env ctx req
        E.evaluate (forceCompletions comps)
  case res of
    Left (_ :: E.SomeException) -> return []
    Right comps -> return comps

argumentCompletionsWithEnv :: Env.Env0 -> String -> Int -> [Completion]
argumentCompletionsWithEnv env src cursor =
  case argumentContextAt src cursor of
    Nothing -> []
    Just req ->
      let ctx = scanSourceContext src cursor
      in completeArguments env ctx req

callSignatures :: Env.Env0 -> [FilePath] -> S.ModName -> FilePath -> String -> Int -> IO [CallSignature]
callSignatures baseEnv searchPath modName fileName src cursor = do
  res <- E.try $ do
    case callContextAt src cursor of
      Nothing -> return []
      Just req -> do
        env <- prepareCompletionEnv baseEnv searchPath modName fileName src
        let ctx = scanSourceContext src cursor
            sigs = maybe [] (:[]) (callSignature env ctx req)
        E.evaluate (forceSignatures sigs)
  case res of
    Left (_ :: E.SomeException) -> return []
    Right sigs -> return sigs

callSignaturesWithEnv :: Env.Env0 -> String -> Int -> [CallSignature]
callSignaturesWithEnv env src cursor =
  case callContextAt src cursor of
    Nothing -> []
    Just req ->
      let ctx = scanSourceContext src cursor
      in maybe [] (:[]) (callSignature env ctx req)

hoverInfo :: Env.Env0 -> [FilePath] -> S.ModName -> FilePath -> String -> Int -> IO (Maybe HoverInfo)
hoverInfo baseEnv searchPath modName fileName src cursor = do
  res <- E.try $ do
    case hoverTargetAt src cursor of
      Nothing -> return Nothing
      Just parts -> do
        env <- prepareCompletionEnv baseEnv searchPath modName fileName src
        let ctx = scanSourceContext src cursor
            info = hoverInfoForParts env ctx parts
        E.evaluate (forceMaybeHover info)
  case res of
    Left (_ :: E.SomeException) -> return Nothing
    Right info -> return info

hoverInfoWithEnv :: Env.Env0 -> String -> Int -> Maybe HoverInfo
hoverInfoWithEnv env src cursor = do
  parts <- hoverTargetAt src cursor
  let ctx = scanSourceContext src cursor
  hoverInfoForParts env ctx parts

prepareCompletionEnv :: Env.Env0 -> [FilePath] -> S.ModName -> FilePath -> String -> IO Env.Env0
prepareCompletionEnv baseEnv searchPath modName fileName src = do
  imps <- parseImports fileName src
  completionEnv searchPath baseEnv modName imps

completionImportKey :: FilePath -> String -> IO String
completionImportKey fileName src = do
  imps <- parseImports fileName src
  return (show imps)

memberContextAt :: String -> Int -> Maybe MemberRequest
memberContextAt src cursor =
  case restAfterPrefix of
    '.':rest ->
      let receiver = reverse (takeWhile receiverChar rest)
          receiver' = trimDots receiver
      in if null receiver'
           then Nothing
           else Just MemberRequest
                  { memberReceiver = receiver'
                  , memberPrefix = reverse prefixRev
                  }
    _ -> Nothing
  where
    before = take (max 0 (min cursor (length src))) src
    (prefixRev, restAfterPrefix) = span identChar (reverse before)

    identChar c = isAlphaNum c || c == '_'
    receiverChar c = identChar c || c == '.' || c == '?'
    trimDots = reverse . dropWhile (== '.') . reverse . dropWhile (== '.')

hoverTargetAt :: String -> Int -> Maybe [String]
hoverTargetAt src cursor = do
  let cursor' = max 0 (min cursor (length src))
      before = take cursor' src
      after = drop cursor' src
      identPrefix = reverse (takeWhile identChar (reverse before))
      identSuffix = takeWhile identChar after
      ident = identPrefix ++ identSuffix
      identStart = cursor' - length identPrefix
  if not (validIdent ident)
    then Nothing
    else do
      let beforeIdent = take identStart src
          target =
            case reverse beforeIdent of
              '.':rest ->
                let receiver = reverse (takeWhile receiverChar rest)
                in receiver ++ "." ++ ident
              _ -> ident
          parts = receiverParts target
      if all validIdent parts && not (null parts)
        then Just parts
        else Nothing
  where
    receiverChar c = identChar c || c == '.' || c == '?'

callContextAt :: String -> Int -> Maybe CallRequest
callContextAt src cursor = do
  openIx <- activeOpenParen before
  let args = drop (openIx + 1) before
      calleeBeforeParen = take openIx before
      callee = reverse $
        takeWhile callPathChar $
        dropWhile isSpace $
        reverse calleeBeforeParen
      target = receiverParts callee
  if null target
    then Nothing
    else Just CallRequest
      { callTarget = target
      , callActiveParameter = activeArgIndex args
      , callArgumentText = args
      }
  where
    before = take (max 0 (min cursor (length src))) src
    callPathChar c = identChar c || c == '.' || c == '?'

argumentContextAt :: String -> Int -> Maybe ArgumentRequest
argumentContextAt src cursor = do
  req <- callContextAt src cursor
  prefix <- activeKeywordPrefix (callArgumentText req)
  return ArgumentRequest
    { argumentTarget = callTarget req
    , argumentPrefix = prefix
    , argumentSuppliedKeywords = suppliedKeywordNames (callArgumentText req)
    }

activeKeywordPrefix :: String -> Maybe String
activeKeywordPrefix args = do
  current <- lastMaybe (splitArgSegments args)
  if topLevelContains '=' current
    then Nothing
    else
      let prefix = reverse (takeWhile identChar (reverse current))
          beforePrefix = take (length current - length prefix) current
      in if all isSpace beforePrefix
           then Just prefix
           else Nothing

suppliedKeywordNames :: String -> [String]
suppliedKeywordNames args =
  mapMaybe keywordName (splitArgSegments args)
  where
    keywordName segment =
      let (lhs, eqPart) = breakArgTopLevel '=' segment
          nm = trim lhs
      in case eqPart of
           '=':_ | validIdent nm -> Just nm
           _ -> Nothing

splitArgSegments :: String -> [String]
splitArgSegments = reverse . map reverse . go 0 0 0 Nothing [[]]
  where
    go _ _ _ _ acc [] = acc
    go p b c str (x:xs) (ch:chs)
      | Just st <- str =
          go p b c (advanceString st ch) ((ch:x):xs) chs
      | isStringQuote ch = go p b c (Just (StringState ch False)) ((ch:x):xs) chs
      | ch == ',' && p == 0 && b == 0 && c == 0 = go p b c str ([]:x:xs) chs
      | ch == '(' = go (p + 1) b c str ((ch:x):xs) chs
      | ch == ')' = go (max 0 (p - 1)) b c str ((ch:x):xs) chs
      | ch == '[' = go p (b + 1) c str ((ch:x):xs) chs
      | ch == ']' = go p (max 0 (b - 1)) c str ((ch:x):xs) chs
      | ch == '{' = go p b (c + 1) str ((ch:x):xs) chs
      | ch == '}' = go p b (max 0 (c - 1)) str ((ch:x):xs) chs
      | otherwise = go p b c str ((ch:x):xs) chs
    go _ _ _ _ [] _ = []

topLevelContains :: Char -> String -> Bool
topLevelContains needle = go 0 0 0 Nothing
  where
    go _ _ _ _ [] = False
    go p b c str (ch:chs)
      | Just st <- str =
          go p b c (advanceString st ch) chs
      | isStringQuote ch = go p b c (Just (StringState ch False)) chs
      | ch == needle && p == 0 && b == 0 && c == 0 = True
      | ch == '(' = go (p + 1) b c str chs
      | ch == ')' = go (max 0 (p - 1)) b c str chs
      | ch == '[' = go p (b + 1) c str chs
      | ch == ']' = go p (max 0 (b - 1)) c str chs
      | ch == '{' = go p b (c + 1) str chs
      | ch == '}' = go p b (max 0 (c - 1)) str chs
      | otherwise = go p b c str chs

breakArgTopLevel :: Char -> String -> (String, String)
breakArgTopLevel needle = go 0 0 0 Nothing []
  where
    go _ _ _ _ acc [] = (reverse acc, [])
    go p b c str acc s@(ch:chs)
      | Just st <- str =
          go p b c (advanceString st ch) (ch:acc) chs
      | isStringQuote ch = go p b c (Just (StringState ch False)) (ch:acc) chs
      | ch == needle && p == 0 && b == 0 && c == 0 = (reverse acc, s)
      | ch == '(' = go (p + 1) b c str (ch:acc) chs
      | ch == ')' = go (max 0 (p - 1)) b c str (ch:acc) chs
      | ch == '[' = go p (b + 1) c str (ch:acc) chs
      | ch == ']' = go p (max 0 (b - 1)) c str (ch:acc) chs
      | ch == '{' = go p b (c + 1) str (ch:acc) chs
      | ch == '}' = go p b (max 0 (c - 1)) str (ch:acc) chs
      | otherwise = go p b c str (ch:acc) chs

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe xs = Just (last xs)

activeOpenParen :: String -> Maybe Int
activeOpenParen = listToMaybe . go 0 Nothing []
  where
    go _ _ stack [] = stack
    go ix str stack (ch:chs)
      | Just st <- str =
          go (ix + 1) (advanceString st ch) stack chs
      | isStringQuote ch =
          go (ix + 1) (Just (StringState ch False)) stack chs
      | ch == '(' =
          go (ix + 1) str (ix:stack) chs
      | ch == ')' =
          go (ix + 1) str (drop 1 stack) chs
      | otherwise =
          go (ix + 1) str stack chs

activeArgIndex :: String -> Int
activeArgIndex = go 0 0 0 0 Nothing
  where
    go arg _ _ _ _ [] = arg
    go arg p b c str (x:xs)
      | Just st <- str =
          go arg p b c (advanceString st x) xs
      | isStringQuote x = go arg p b c (Just (StringState x False)) xs
      | x == '(' = go arg (p + 1) b c str xs
      | x == ')' = go arg (max 0 (p - 1)) b c str xs
      | x == '[' = go arg p (b + 1) c str xs
      | x == ']' = go arg p (max 0 (b - 1)) c str xs
      | x == '{' = go arg p b (c + 1) str xs
      | x == '}' = go arg p b (max 0 (c - 1)) str xs
      | x == ',' && p == 0 && b == 0 && c == 0 = go (arg + 1) p b c str xs
      | otherwise = go arg p b c str xs

isStringQuote :: Char -> Bool
isStringQuote ch = ch == '"' || ch == '\''

advanceString :: StringState -> Char -> Maybe StringState
advanceString (StringState quote escaped) ch
  | escaped = Just (StringState quote False)
  | ch == '\\' = Just (StringState quote True)
  | ch == quote = Nothing
  | otherwise = Just (StringState quote False)

completeMember :: Env.Env0 -> SourceContext -> MemberRequest -> [Completion]
completeMember env ctx req =
  case resolveReceiverType env ctx (receiverParts (memberReceiver req)) of
    Nothing -> []
    Just typ -> attrsForType env typ (memberPrefix req)

completeArguments :: Env.Env0 -> SourceContext -> ArgumentRequest -> [Completion]
completeArguments env ctx req =
  case resolveCallableInfo env ctx (argumentTarget req) >>= typeOfInfo of
    Just typ ->
      case Env.unalias env typ of
        S.TFun _ _ _ kw _ ->
          [ Completion
              (keywordParameterName param ++ "=")
              CompletionKeyword
              (Just (displayType (keywordParameterType param)))
          | param <- keywordParameterRows kw
          , argumentPrefix req `isPrefixOf` keywordParameterName param
          , keywordParameterName param `notElem` argumentSuppliedKeywords req
          ]
        _ -> []
    _ -> []

callSignature :: Env.Env0 -> SourceContext -> CallRequest -> Maybe CallSignature
callSignature env ctx req = do
  info <- resolveCallableInfo env ctx (callTarget req)
  typ <- typeOfInfo info
  case Env.unalias env typ of
    S.TFun _ _ pos kw ret ->
      let params = signatureParameters pos kw
          active = if null params
                     then 0
                     else min (callActiveParameter req) (length params - 1)
          label = intercalate "." (callTarget req)
            ++ "("
            ++ intercalate ", " (map signatureParameterLabel params)
            ++ ") -> "
            ++ displayType ret
      in Just CallSignature
        { callSignatureLabel = label
        , callSignatureParameters = params
        , callSignatureActiveParameter = active
        }
    _ -> Nothing

hoverInfoForParts :: Env.Env0 -> SourceContext -> [String] -> Maybe HoverInfo
hoverInfoForParts env ctx parts = do
  target <- hoverTarget env ctx parts
  let label = intercalate "." parts
  return HoverInfo
    { hoverLabel = label
    , hoverDetail = label ++ ": " ++ displayType (hoverResolvedType target)
    , hoverDocumentation = hoverResolvedDoc target
    }

hoverTarget :: Env.Env0 -> SourceContext -> [String] -> Maybe HoverTarget
hoverTarget env ctx parts =
  memberHoverTarget env ctx parts
  <|> baseHoverTarget env ctx parts
  <|> globalHoverTarget env parts

memberHoverTarget :: Env.Env0 -> SourceContext -> [String] -> Maybe HoverTarget
memberHoverTarget env ctx parts = do
  (receiver, attr) <- unsnoc parts
  typ <- resolveReceiverType env ctx receiver
  tc <- typeTCon env typ
  info <- attrInfo env tc (S.name attr)
  attrTyp <- Env.unalias env <$> typeOfInfo info
  return HoverTarget
    { hoverResolvedType = attrTyp
    , hoverResolvedDoc = docOfInfo info <|> typeDoc env attrTyp
    }

baseHoverTarget :: Env.Env0 -> SourceContext -> [String] -> Maybe HoverTarget
baseHoverTarget env ctx parts = do
  typ <- resolveReceiverType env ctx parts
  return HoverTarget
    { hoverResolvedType = typ
    , hoverResolvedDoc = typeDoc env typ
    }

globalHoverTarget :: Env.Env0 -> [String] -> Maybe HoverTarget
globalHoverTarget env parts = do
  info <- lookupPathInfo env parts
  typ <- Env.unalias env <$> typeOfInfo info
  return HoverTarget
    { hoverResolvedType = typ
    , hoverResolvedDoc = docOfInfo info <|> typeDoc env typ
    }

resolveCallableInfo :: Env.Env0 -> SourceContext -> [String] -> Maybe I.NameInfo
resolveCallableInfo env ctx parts =
  memberCallableInfo env ctx parts <|> lookupPathInfo env parts

memberCallableInfo :: Env.Env0 -> SourceContext -> [String] -> Maybe I.NameInfo
memberCallableInfo env ctx parts = do
  (receiver, attr) <- unsnoc parts
  typ <- resolveReceiverType env ctx receiver
  tc <- typeTCon env typ
  attrInfo env tc (S.name attr)

unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc xs = Just (init xs, last xs)

signatureParameters :: S.Type -> S.Type -> [SignatureParameter]
signatureParameters pos kw =
  zipWith positional [1..] (positionalTypes pos) ++ keywordParameters kw
  where
    positional ix typ =
      SignatureParameter ("arg" ++ show (ix :: Int) ++ ": " ++ displayType typ)

positionalTypes :: S.Type -> [S.Type]
positionalTypes row =
  case row of
    S.TRow _ S.PRow _ typ rest -> typ : positionalTypes rest
    S.TStar _ S.PRow rest -> [S.tTupleP rest]
    _ -> []

keywordParameters :: S.Type -> [SignatureParameter]
keywordParameters row =
  case row of
    S.TRow _ S.KRow name typ rest ->
      SignatureParameter (S.rawstr name ++ ": " ++ displayType typ) : keywordParameters rest
    S.TStar _ S.KRow rest ->
      [SignatureParameter ("**kwargs: " ++ displayType (S.tTupleK rest))]
    _ -> []

keywordParameterRows :: S.Type -> [KeywordParameter]
keywordParameterRows row =
  case row of
    S.TRow _ S.KRow name typ rest ->
      KeywordParameter (S.rawstr name) typ : keywordParameterRows rest
    _ -> []

completionEnv :: [FilePath] -> Env.Env0 -> S.ModName -> [S.Import] -> IO Env.Env0
completionEnv searchPath baseEnv modName imps =
  Env.mkEnv searchPath baseEnv mod `E.catch` \(_ :: E.SomeException) ->
    shallowCompletionEnv searchPath baseEnv imps
  where
    mod = S.Module modName imps Nothing []

-- Completion should still work when an otherwise usable direct import records
-- stale transitive interfaces. Load only the imported modules themselves as a
-- fallback, which is enough for generated base classes and typed constructors.
shallowCompletionEnv :: [FilePath] -> Env.Env0 -> [S.Import] -> IO Env.Env0
shallowCompletionEnv searchPath baseEnv imps =
  refreshHModules <$> foldM (shallowImport searchPath) baseEnv imps

refreshHModules :: Env.Env0 -> Env.Env0
refreshHModules env =
  env { Env.hmodules = I.convTEnv2HTEnv (Env.modules env) }

shallowImport :: [FilePath] -> Env.Env0 -> S.Import -> IO Env.Env0
shallowImport searchPath env imp =
  case imp of
    S.Import _ items ->
      foldM (shallowModuleItem searchPath) env items
    S.FromImport _ (S.ModRef (0, Just m)) items ->
      shallowModule searchPath env m $ \mi env' ->
        Env.importSome items m mi env'
    S.FromImportAll _ (S.ModRef (0, Just m)) ->
      shallowModule searchPath env m $ \mi env' ->
        Env.importAll m mi env'
    _ ->
      return env

shallowModuleItem :: [FilePath] -> Env.Env0 -> S.ModuleItem -> IO Env.Env0
shallowModuleItem searchPath env (S.ModuleItem m as) =
  shallowModule searchPath env m $ \_ env' ->
    case as of
      Nothing -> Env.addImport m env'
      Just n -> Env.defineClosed [(n, I.NMAlias m)] env'

shallowModule
  :: [FilePath]
  -> Env.Env0
  -> S.ModName
  -> (Env.ModuleInfo -> Env.Env0 -> Env.Env0)
  -> IO Env.Env0
shallowModule searchPath env m applyImport = do
  loaded <- readModuleInterface searchPath m
  case loaded of
    Nothing -> return env
    Just (ms, te, mdoc) ->
      let env' = Env.addMod m ms te mdoc env
      in case Env.lookupModuleInfo m env' of
           Just mi -> return $ applyImport mi env'
           Nothing -> return env'

readModuleInterface :: [FilePath] -> S.ModName -> IO (Maybe ([S.ModName], I.TEnv, Maybe String))
readModuleInterface searchPath m = do
  mty <- Env.findTyFile searchPath m
  case mty of
    Nothing -> return Nothing
    Just ty -> do
      res <- (Just <$> IF.readFile ty) `E.catch` \(_ :: E.SomeException) ->
        return Nothing
      case res of
        Just (_, I.NModule imps te mdoc, _, _, _, _, _, _, _, _, _, _, _) ->
          return (Just (imps, te, mdoc))
        _ ->
          return Nothing

resolveReceiverType :: Env.Env0 -> SourceContext -> [String] -> Maybe S.Type
resolveReceiverType _ _ [] = Nothing
resolveReceiverType env ctx (base:attrs) = do
  baseType <- resolveBaseType env ctx base
  foldl step (Just baseType) attrs
  where
    step Nothing _ = Nothing
    step (Just typ) attr = do
      tc <- typeTCon env typ
      attrType env tc (S.name attr)

resolveBaseType :: Env.Env0 -> SourceContext -> String -> Maybe S.Type
resolveBaseType env ctx name =
  localBindingType env ctx name
  <|> explicitParamType env ctx name
  <|> inheritedParamType env ctx name

localBindingType :: Env.Env0 -> SourceContext -> String -> Maybe S.Type
localBindingType env ctx name =
  listToMaybe $ mapMaybe bindingType matching
  where
    matching =
      case sourceDef ctx of
        Nothing -> []
        Just def ->
          filter (sameDef def) $
            filter ((== name) . localName) (sourceLocalBindings ctx)

    sameDef def binding =
      localDefName binding == defName def &&
      localDefIndent binding == defIndent def &&
      localClassKey binding == sourceClassKey

    sourceClassKey =
      fmap (\cls -> (className cls, classIndent cls)) (sourceClass ctx)

    localClassKey binding =
      (,) <$> localClassName binding <*> localClassIndent binding

    bindingType binding =
      case localKind binding of
        LocalAnnotated ann -> parseTypeText env ann
        LocalCallResult callee -> callResultType env ctx callee
        LocalMemberPath path -> resolveReceiverType env ctx path

explicitParamType :: Env.Env0 -> SourceContext -> String -> Maybe S.Type
explicitParamType env ctx name = do
  def <- sourceDef ctx
  param <- find ((== name) . paramName) (defParams def)
  ann <- paramAnn param
  parseTypeText env ann

inheritedParamType :: Env.Env0 -> SourceContext -> String -> Maybe S.Type
inheritedParamType env ctx name = do
  cls <- sourceClass ctx
  def <- sourceDef ctx
  paramIx <- methodParamIndex name (defParams def)
  listToMaybe $ mapMaybe (parentParamType paramIx name (defName def)) (classParents cls)
  where
    parentParamType ix pname method parentText = do
      parentType <- parseTypeText env parentText
      parentTc <- typeTCon env parentType
      (_, schema, _) <- Env.findAttr env parentTc (S.name method)
      parentMod <- qnameModule (S.tcname parentTc)
      Env.unalias (Env.setMod parentMod env) <$> paramTypeFromSchema ix pname schema

    qnameModule qn =
      case qn of
        S.QName mn _ -> Just mn
        S.GName mn _ -> Just mn
        S.NoQ _ -> Nothing

methodParamIndex :: String -> [Param] -> Maybe Int
methodParamIndex name params = do
  ix <- findIndex ((== name) . paramName) params
  let selfOffset = case params of
                     Param "self" _ : _ -> 1
                     _ -> 0
      ix' = ix - selfOffset
  if ix' < 0 then Nothing else Just ix'

paramTypeFromSchema :: Int -> String -> S.TSchema -> Maybe S.Type
paramTypeFromSchema ix pname (S.TSchema _ _ typ) =
  case typ of
    S.TFun _ _ pos kwd _ ->
      positionalType ix pos <|> keywordType (S.name pname) kwd
    _ -> Nothing

positionalType :: Int -> S.Type -> Maybe S.Type
positionalType ix row =
  case row of
    S.TRow _ S.PRow _ typ rest
      | ix == 0 -> Just typ
      | otherwise -> positionalType (ix - 1) rest
    _ -> Nothing

keywordType :: S.Name -> S.Type -> Maybe S.Type
keywordType name row =
  case row of
    S.TRow _ S.KRow n typ rest
      | S.rawstr n == S.rawstr name -> Just typ
      | otherwise -> keywordType name rest
    _ -> Nothing

attrsForType :: Env.Env0 -> S.Type -> String -> [Completion]
attrsForType env typ prefix =
  case typeTCon env typ of
    Nothing -> []
    Just tc ->
      dedup $
        [ Completion (S.rawstr n) (kindOf info) (detailOf info)
        | (n, info) <- Env.fullAttrEnv env tc
        , prefix `isPrefixOf` S.rawstr n
        ]

attrType :: Env.Env0 -> S.TCon -> S.Name -> Maybe S.Type
attrType env tc attr =
  attrInfo env tc attr >>= typeOfInfo

attrInfo :: Env.Env0 -> S.TCon -> S.Name -> Maybe I.NameInfo
attrInfo env tc attr =
  snd <$> find ((== S.rawstr attr) . S.rawstr . fst) (Env.fullAttrEnv env tc)

typeOfInfo :: I.NameInfo -> Maybe S.Type
typeOfInfo info =
  case info of
    I.NVar t -> Just t
    I.NSVar t -> Just t
    I.NDef schema _ _ -> Just (S.sctype schema)
    I.NSig schema _ _ -> Just (S.sctype schema)
    _ -> Nothing

callResultType :: Env.Env0 -> SourceContext -> [String] -> Maybe S.Type
callResultType env ctx parts = do
  info <- resolveCallableInfo env ctx parts
  functionReturnType env info

lookupPathInfo :: Env.Env0 -> [String] -> Maybe I.NameInfo
lookupPathInfo _ [] = Nothing
lookupPathInfo env [n] =
  lookupQNameInfo env (S.NoQ (S.name n))
lookupPathInfo env parts =
  let modPart = init parts
      n = last parts
      qn = S.QName (S.ModName (map S.name modPart)) (S.name n)
  in lookupQNameInfo env qn

lookupQNameInfo :: Env.Env0 -> S.QName -> Maybe I.NameInfo
lookupQNameInfo env qn =
  case qn of
    S.NoQ n ->
      lookupNoQ n
    S.QName m n ->
      lookupModuleItem env (Env.findHMod m env) n
    S.GName m n
      | Just m == Env.thismod env ->
          lookupQNameInfo env (S.NoQ n)
      | otherwise ->
          lookupModuleItem env (Env.lookupHMod m env) n
  where
    lookupNoQ n              = resolve =<< Env.lookupName n env

    resolve (I.HNAlias qn')  = lookupQNameInfo env qn'
    resolve info             = Just (I.convHNameInfo2NameInfo info)

lookupModuleItem :: Env.Env0 -> Maybe I.HTEnv -> S.Name -> Maybe I.NameInfo
lookupModuleItem env mtenv n = do
  tenv <- mtenv
  info <- HM.lookup n tenv
  case info of
    I.HNAlias qn -> lookupQNameInfo env qn
    _ -> Just (I.convHNameInfo2NameInfo info)

functionReturnType :: Env.Env0 -> I.NameInfo -> Maybe S.Type
functionReturnType env info = do
  typ <- typeOfInfo info
  case typ of
    S.TFun _ _ _ _ ret -> Just (Env.unalias env ret)
    _ -> Nothing

kindOf :: I.NameInfo -> CompletionKind
kindOf info =
  case info of
    I.NSig schema dec _
      | dec == S.Property -> CompletionProperty
      | isFun (S.sctype schema) -> CompletionMethod
      | otherwise -> CompletionField
    I.NDef schema dec _
      | dec == S.Property -> CompletionProperty
      | isFun (S.sctype schema) -> CompletionMethod
      | otherwise -> CompletionValue
    I.NVar{} -> CompletionField
    I.NSVar{} -> CompletionField
    _ -> CompletionValue
  where
    isFun S.TFun{} = True
    isFun _ = False

detailOf :: I.NameInfo -> Maybe String
detailOf info =
  fmap displayType (typeOfInfo info)

docOfInfo :: I.NameInfo -> Maybe String
docOfInfo info =
  cleanDoc $
    case info of
      I.NDef _ _ doc -> doc
      I.NSig _ _ doc -> doc
      I.NAct _ _ _ _ doc -> doc
      I.NClass _ _ _ doc -> doc
      I.NProto _ _ _ doc -> doc
      I.NExt _ _ _ _ _ doc -> doc
      I.NModule _ _ doc -> doc
      _ -> Nothing

typeDoc :: Env.Env0 -> S.Type -> Maybe String
typeDoc env typ = do
  tc <- typeTCon env typ
  info <- lookupTConInfo env tc
  docOfInfo info

lookupTConInfo :: Env.Env0 -> S.TCon -> Maybe I.NameInfo
lookupTConInfo env tc =
  lookupQNameInfo env (S.tcname tc)

cleanDoc :: Maybe String -> Maybe String
cleanDoc Nothing = Nothing
cleanDoc (Just doc) =
  let doc' = trim doc
  in if null doc' then Nothing else Just doc'

displayType :: S.Type -> String
displayType = stripBuiltinPrefix . prstr

stripBuiltinPrefix :: String -> String
stripBuiltinPrefix [] = []
stripBuiltinPrefix s
  | builtinPrefix `isPrefixOf` s =
      stripBuiltinPrefix (drop (length builtinPrefix) s)
  | otherwise =
      head s : stripBuiltinPrefix (tail s)
  where
    builtinPrefix = "__builtin__."

typeTCon :: Env.Env0 -> S.Type -> Maybe S.TCon
typeTCon env typ =
  case typ of
    S.TCon _ tc -> Just tc
    S.TOpt _ inner -> typeTCon env inner
    S.TVar _ tv -> Just (Env.findTVBound env tv)
    _ -> Nothing

parseTypeText :: Env.Env0 -> String -> Maybe S.Type
parseTypeText env raw =
  case runParser (St.evalStateT (P.ttype <* eof) P.initState) "" raw of
    Left _ -> Nothing
    Right typ -> Just (Env.unalias env typ)

parseImports :: FilePath -> String -> IO [S.Import]
parseImports fileName src = do
  res <- E.try (P.parseModuleHeader fileName src)
  case res of
    Left (_ :: E.SomeException) -> return []
    Right (imps, _) -> return imps

scanSourceContext :: String -> Int -> SourceContext
scanSourceContext src cursor =
  let st = foldl scanLine (ScanState [] []) (contextLines src cursor)
      scopes = scanScopes st
  in SourceContext
       { sourceClass = listToMaybe [ c | ScopeClass c <- scopes ]
       , sourceDef = listToMaybe [ d | ScopeDef d <- scopes ]
       , sourceLocalBindings = scanLocals st
       }

scanLine :: ScanState -> String -> ScanState
scanLine st line
  | null stripped = st
  | "#" `isPrefixOf` stripped = st
  | otherwise =
      let indent = lineIndent line
          scopes' = popClosed indent (scanScopes st)
      in case parseClass indent stripped of
           Just cls -> st { scanScopes = ScopeClass cls : scopes' }
           Nothing ->
             case parseDef indent stripped of
               Just def -> st { scanScopes = ScopeDef def : scopes' }
               Nothing ->
                 st { scanScopes = scopes'
                    , scanLocals = scanLocal scopes' stripped (scanLocals st)
                    }
  where
    stripped = trimLeft line

scanLocal :: [Scope] -> String -> [LocalBinding] -> [LocalBinding]
scanLocal scopes stripped locals
  | Just def <- activeDef =
      case parseLocalBinding stripped of
        Just (nm, kind) ->
          LocalBinding
            { localName = nm
            , localClassName = className <$> activeClass
            , localClassIndent = classIndent <$> activeClass
            , localDefName = defName def
            , localDefIndent = defIndent def
            , localKind = kind
            } : locals
        Nothing -> locals
  | otherwise = locals
  where
    activeDef = listToMaybe [ d | ScopeDef d <- scopes ]
    activeClass = listToMaybe [ c | ScopeClass c <- scopes ]

popClosed :: Int -> [Scope] -> [Scope]
popClosed indent = dropWhile ((>= indent) . scopeIndent)

scopeIndent :: Scope -> Int
scopeIndent (ScopeClass cls) = classIndent cls
scopeIndent (ScopeDef def) = defIndent def

parseClass :: Int -> String -> Maybe ClassContext
parseClass indent line
  | "class " `isPrefixOf` line = do
      let rest = dropWhile isSpace (drop (length "class ") line)
          (nm, afterName) = span identChar rest
      if null nm
        then Nothing
        else Just ClassContext
               { className = nm
               , classParents = parseParents (skipTypeBinder afterName)
               , classIndent = indent
               }
  | otherwise = Nothing

parseParents :: String -> [String]
parseParents afterName =
  case dropWhile isSpace afterName of
    '(':rest ->
      case balancedContent '(' ')' rest of
        Just inside -> filter (not . null) (map trim (splitTopLevel ',' inside))
        Nothing -> []
    _ -> []

parseDef :: Int -> String -> Maybe DefContext
parseDef indent line = do
  afterDef <- afterDefKeyword line
  let (nm, afterName) = span identChar (dropWhile isSpace afterDef)
  paramsText <- case skipTypeBinder afterName of
                  '(':rest -> balancedContent '(' ')' rest
                  _ -> Nothing
  return DefContext
    { defName = nm
    , defParams = mapMaybe parseParam (splitTopLevel ',' paramsText)
    , defIndent = indent
    }

afterDefKeyword :: String -> Maybe String
afterDefKeyword line
  | "def " `isPrefixOf` line = Just (drop 4 line)
  | otherwise =
      case stripEffect line of
        Just rest | "def " `isPrefixOf` rest -> Just (drop 4 rest)
        _ -> Nothing
  where
    stripEffect s =
      case words s of
        w:_ | w `elem` ["mut", "proc", "pure", "action"] ->
              Just (dropWhile isSpace (drop (length w) s))
        _ -> Nothing

parseParam :: String -> Maybe Param
parseParam raw
  | null nm = Nothing
  | otherwise = Just Param { paramName = nm, paramAnn = ann }
  where
    s0 = trim raw
    s = dropWhile (== '*') s0
    (beforeDefault, _) = breakTopLevel '=' s
    (namePart, annPart) = breakTopLevel ':' beforeDefault
    nm = trim namePart
    ann = case annPart of
            ':' : rest ->
              let a = trim rest
              in if null a then Nothing else Just a
            _ -> Nothing

parseLocalBinding :: String -> Maybe (String, LocalKind)
parseLocalBinding line = do
  let (lhs, eqPart) = breakTopLevel '=' line
  rhs <- case eqPart of
           '=' : rest -> Just rest
           _ -> Nothing
  let (namePart, annPart) = breakTopLevel ':' lhs
      nm = trim namePart
  if not (validIdent nm)
    then Nothing
    else case annPart of
           ':' : annRaw ->
             let ann = trim annRaw
             in if null ann
                  then Nothing
                  else Just (nm, LocalAnnotated ann)
           _ ->
             case parseCallTarget rhs of
               Just callee -> Just (nm, LocalCallResult callee)
               Nothing -> do
                 path <- parseMemberPath rhs
                 return (nm, LocalMemberPath path)

parseCallTarget :: String -> Maybe [String]
parseCallTarget raw =
  let s = trim raw
      (callee, rest) = span callPathChar s
  in case (strictReceiverParts callee, dropWhile isSpace rest) of
       (Just parts, '(' : _) | validCallPath parts -> Just parts
       _ -> Nothing
  where
    callPathChar c = identChar c || c == '.' || c == '?'
    validCallPath parts = all validIdent parts && not (null parts)

parseMemberPath :: String -> Maybe [String]
parseMemberPath raw =
  let s = trim raw
      (path, rest) = span memberPathChar s
  in case strictReceiverParts path of
       Just parts | all isSpace rest && validMemberPath parts -> Just parts
       _ -> Nothing
  where
    memberPathChar c = identChar c || c == '.' || c == '?'
    validMemberPath parts = length parts > 1 && all validIdent parts

validIdent :: String -> Bool
validIdent [] = False
validIdent (c:cs) = (isAlpha c || c == '_') && all identChar cs

contextLines :: String -> Int -> [String]
contextLines src cursor =
  let before = take (max 0 (min cursor (length src))) src
      ls = lines before
  in case reverse before of
       '\n':_ -> ls
       _ -> case ls of
              [] -> []
              _ -> init ls

receiverParts :: String -> [String]
receiverParts =
  filter (not . null) . map stripOptionalMarker . splitOn '.'

strictReceiverParts :: String -> Maybe [String]
strictReceiverParts raw =
  let parts = map stripOptionalMarker (splitOn '.' raw)
  in if any null parts then Nothing else Just parts

stripOptionalMarker :: String -> String
stripOptionalMarker = reverse . dropWhile (== '?') . reverse

lineIndent :: String -> Int
lineIndent = go 0
  where
    go n (' ':xs) = go (n + 1) xs
    go n ('\t':xs) = go (n + 8) xs
    go n _ = n

identChar :: Char -> Bool
identChar c = isAlphaNum c || c == '_'

splitOn :: Char -> String -> [String]
splitOn sep = splitTopLevel sep

splitTopLevel :: Char -> String -> [String]
splitTopLevel sep = reverse . map reverse . go 0 0 0 [[]]
  where
    go _ _ _ acc [] = acc
    go p b c (x:xs) (ch:chs)
      | ch == sep && p == 0 && b == 0 && c == 0 = go p b c ([]:x:xs) chs
      | ch == '(' = go (p + 1) b c ((ch:x):xs) chs
      | ch == ')' = go (max 0 (p - 1)) b c ((ch:x):xs) chs
      | ch == '[' = go p (b + 1) c ((ch:x):xs) chs
      | ch == ']' = go p (max 0 (b - 1)) c ((ch:x):xs) chs
      | ch == '{' = go p b (c + 1) ((ch:x):xs) chs
      | ch == '}' = go p b (max 0 (c - 1)) ((ch:x):xs) chs
      | otherwise = go p b c ((ch:x):xs) chs
    go _ _ _ [] _ = []

breakTopLevel :: Char -> String -> (String, String)
breakTopLevel needle = go 0 0 0 []
  where
    go _ _ _ acc [] = (reverse acc, [])
    go p b c acc s@(ch:chs)
      | ch == needle && p == 0 && b == 0 && c == 0 = (reverse acc, s)
      | ch == '(' = go (p + 1) b c (ch:acc) chs
      | ch == ')' = go (max 0 (p - 1)) b c (ch:acc) chs
      | ch == '[' = go p (b + 1) c (ch:acc) chs
      | ch == ']' = go p (max 0 (b - 1)) c (ch:acc) chs
      | ch == '{' = go p b (c + 1) (ch:acc) chs
      | ch == '}' = go p b (max 0 (c - 1)) (ch:acc) chs
      | otherwise = go p b c (ch:acc) chs

skipTypeBinder :: String -> String
skipTypeBinder raw =
  case dropWhile isSpace raw of
    '[':rest ->
      case balancedContentRest '[' ']' rest of
        Just (_, after) -> dropWhile isSpace after
        Nothing -> dropWhile isSpace raw
    rest -> rest

balancedContent :: Char -> Char -> String -> Maybe String
balancedContent open close src =
  fst <$> balancedContentRest open close src

balancedContentRest :: Char -> Char -> String -> Maybe (String, String)
balancedContentRest open close = go 1 []
  where
    go 0 acc rest = Just (reverse acc, rest)
    go _ _ [] = Nothing
    go n acc (ch:chs)
      | ch == open = go (n + 1) (ch:acc) chs
      | ch == close =
          if n == 1
            then Just (reverse acc, chs)
            else go (n - 1) (ch:acc) chs
      | otherwise = go n (ch:acc) chs

trimLeft :: String -> String
trimLeft = dropWhile isSpace

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

dedup :: [Completion] -> [Completion]
dedup = nubBy (\a b -> completionLabel a == completionLabel b)

forceCompletions :: [Completion] -> [Completion]
forceCompletions xs =
  sum [ length (completionLabel c)
      + maybe 0 length (completionDetail c)
      + kindCode (completionKind c)
      | c <- xs
      ] `seq` xs
  where
    kindCode CompletionField = 1
    kindCode CompletionMethod = 2
    kindCode CompletionProperty = 3
    kindCode CompletionValue = 4
    kindCode CompletionKeyword = 5

forceSignatures :: [CallSignature] -> [CallSignature]
forceSignatures xs =
  sum [ length (callSignatureLabel sig)
      + callSignatureActiveParameter sig
      + sum (map (length . signatureParameterLabel) (callSignatureParameters sig))
      | sig <- xs
      ] `seq` xs

forceMaybeHover :: Maybe HoverInfo -> Maybe HoverInfo
forceMaybeHover info =
  case info of
    Nothing -> Nothing
    Just hover ->
      length (hoverLabel hover)
      + length (hoverDetail hover)
      + maybe 0 length (hoverDocumentation hover) `seq` info

(<|>) :: Maybe a -> Maybe a -> Maybe a
Nothing <|> b = b
a <|> _ = a
