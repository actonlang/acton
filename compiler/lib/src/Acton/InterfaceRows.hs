-- SPDX-License-Identifier: BSD-3-Clause

{-# LANGUAGE DeriveGeneric #-}
module Acton.InterfaceRows where

import Control.DeepSeq
import Control.Monad
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as M
import qualified Data.Persist as Persist
import qualified Data.Set as S
import GHC.Generics

import qualified Acton.Builtin as B
import qualified Acton.Names as Names
import qualified Acton.Syntax as A
import Utils


-- Stored rows --------------------------------------------------------------------------------

data MemberKey
  = Method A.Name
  | Attr A.Name
  | InitRest
  deriving (Show, Eq, Ord, Generic)

instance Persist.Persist MemberKey
instance NFData MemberKey

data ContainerHead
  = ActorHead SrcLoc A.Name A.QBinds A.PosPar A.KwdPar (Maybe String)
  | ClassHead SrcLoc A.Name A.QBinds [A.TCon] (Maybe String)
  | ProtocolHead SrcLoc A.Name A.QBinds [A.PCon] (Maybe String)
  | ExtensionHead SrcLoc A.QBinds A.TCon [A.PCon] (Maybe String)
  deriving (Show, Eq, Generic)

instance Persist.Persist ContainerHead
instance NFData ContainerHead

data PathStep
  = InBranch Int Int
  | InElse Int
  deriving (Show, Eq, Ord, Generic)

instance Persist.Persist PathStep
instance NFData PathStep

newtype SuitePath = SuitePath [PathStep]
  deriving (Show, Eq, Ord, Generic)

instance Persist.Persist SuitePath
instance NFData SuitePath

data StmtPlace = StmtPlace SuitePath Int
  deriving (Show, Eq, Ord, Generic)

instance Persist.Persist StmtPlace
instance NFData StmtPlace

data DeclPlace = DeclPlace SuitePath Int Int
  deriving (Show, Eq, Ord, Generic)

instance Persist.Persist DeclPlace
instance NFData DeclPlace

data PlacedStmt = PlacedStmt StmtPlace A.Stmt
  deriving (Show, Eq, Generic)

instance Persist.Persist PlacedStmt
instance NFData PlacedStmt

data PlacedMethod = PlacedMethod
  { placedMethodPlace   :: DeclPlace
  , placedMethodOrdinal :: Int
  , placedMethodDecl    :: A.Decl
  } deriving (Show, Eq, Generic)

instance Persist.Persist PlacedMethod
instance NFData PlacedMethod

data InitPlace
  = InitBody Int
  | InitSuite StmtPlace
  deriving (Show, Eq, Ord, Generic)

instance Persist.Persist InitPlace
instance NFData InitPlace

data InitFragment = InitFragment InitPlace A.Stmt
  deriving (Show, Eq, Generic)

instance Persist.Persist InitFragment
instance NFData InitFragment

data MethodSlot = MethodSlot
  { slotPlace   :: DeclPlace
  , slotName    :: A.Name
  , slotOrdinal :: Int
  , slotIsConstructor :: Bool
  , slotHeader  :: A.Decl
  } deriving (Show, Eq, Generic)

instance Persist.Persist MethodSlot
instance NFData MethodSlot

data ShapeDecl
  = InlineDecl A.Decl
  | MethodDecl MethodSlot
  deriving (Show, Eq, Generic)

instance Persist.Persist ShapeDecl
instance NFData ShapeDecl

data ShapeStmt
  = InlineStmt A.Stmt
  | DeclStmt SrcLoc [ShapeDecl]
  | IfStmt SrcLoc [(A.Expr, SuiteShape)] SuiteShape
  deriving (Show, Eq, Generic)

instance Persist.Persist ShapeStmt
instance NFData ShapeStmt

-- A suite shape stores only structural statements and method ABI slots. The
-- count validates eager reconstruction; omitted indexes belong to Attr or
-- InitRest rows and are not represented in the shape.
data SuiteShape = SuiteShape
  { suiteStmtCount :: Int
  , suiteStructure :: [(Int, ShapeStmt)]
  } deriving (Show, Eq, Generic)

instance Persist.Persist SuiteShape
instance NFData SuiteShape

data ContainerShape = ContainerShape
  { shapeName      :: A.Name
  , shapeHead      :: ContainerHead
  , shapeSuite     :: SuiteShape
  , shapeInitCount :: Maybe Int
  } deriving (Show, Eq, Generic)

instance Persist.Persist ContainerShape
instance NFData ContainerShape

data MemberContent
  = MethodContent [PlacedMethod]
  | AttrContent
      { attrNames :: [A.Name]
      , attrDeclarations :: [PlacedStmt]
      , attrInitializers :: [InitFragment]
      }
  | InitRestContent
      { restConstructor :: Maybe PlacedMethod
      , restInitializers :: [InitFragment]
      }
  deriving (Show, Eq, Generic)

instance Persist.Persist MemberContent
instance NFData MemberContent

data StoredStmt
  = StoredWhole [A.Name] A.Stmt
  | StoredDecls SrcLoc [StoredDecl]
  deriving (Show, Eq, Generic)

instance Persist.Persist StoredStmt
instance NFData StoredStmt

data StoredDecl
  = StoredInline A.Decl
  | StoredContainer A.Name
  deriving (Show, Eq, Generic)

instance Persist.Persist StoredDecl
instance NFData StoredDecl

storedStmtNames :: StoredStmt -> [A.Name]
storedStmtNames (StoredWhole owners _) = owners
storedStmtNames (StoredDecls _ decls) = map storedDeclName decls
  where
    storedDeclName (StoredInline decl) = Names.dname' decl
    storedDeclName (StoredContainer name) = name

data InterfaceRows = InterfaceRows
  { rowModuleName :: A.ModName
  , rowImports    :: [A.Import]
  , rowDoc        :: Maybe String
  , rowHasNotImpl :: Bool
  , rowStatements :: [StoredStmt]
  , rowShapes     :: M.Map A.Name ContainerShape
  , rowMembers    :: M.Map A.Name (M.Map MemberKey MemberContent)
  } deriving (Show, Eq, Generic)

instance NFData InterfaceRows

newtype RowError = RowError String deriving (Show, Eq)

type RowResult = Either RowError

data SelectionExpansion
  = SelectionComplete (S.Set MemberKey)
  | SelectionNeeds (S.Set MemberKey)
  deriving (Show, Eq)

rowError :: String -> RowResult a
rowError = Left . RowError


-- Shape paths --------------------------------------------------------------------------------

rootPath :: SuitePath
rootPath = SuitePath []

branchPath :: SuitePath -> Int -> Int -> SuitePath
branchPath (SuitePath path) stmt branch = SuitePath (path ++ [InBranch stmt branch])

elsePath :: SuitePath -> Int -> SuitePath
elsePath (SuitePath path) stmt = SuitePath (path ++ [InElse stmt])

methodHeader :: A.Decl -> A.Decl
methodHeader decl@A.Def{} = decl
    { A.pos = stripDefaultsP (A.pos decl)
    , A.kwd = stripDefaultsK (A.kwd decl)
    , A.dbody = []
    , A.ddoc = Nothing
    }
methodHeader decl = error ("methodHeader: " ++ show decl)

stripDefaultsP :: A.PosPar -> A.PosPar
stripDefaultsP (A.PosPar n typ _ rest) = A.PosPar n typ Nothing (stripDefaultsP rest)
stripDefaultsP p@A.PosSTAR{}           = p
stripDefaultsP A.PosNIL                = A.PosNIL

stripDefaultsK :: A.KwdPar -> A.KwdPar
stripDefaultsK (A.KwdPar n typ _ rest) = A.KwdPar n typ Nothing (stripDefaultsK rest)
stripDefaultsK p@A.KwdSTAR{}           = p
stripDefaultsK A.KwdNIL                = A.KwdNIL

-- Reconstruction -----------------------------------------------------------------------------

data RestoreMode = RestoreExact | RestoreSelected deriving Eq

data LoadedMembers = LoadedMembers
  { loadedMethods     :: M.Map (A.Name, Int) PlacedMethod
  , loadedConstructor :: Maybe PlacedMethod
  , loadedStatements  :: M.Map SuitePath (M.Map Int A.Stmt)
  , loadedInitBody    :: M.Map Int A.Stmt
  , loadedPrunableInit :: IntSet.IntSet
  }

emptyLoadedMembers :: LoadedMembers
emptyLoadedMembers = LoadedMembers M.empty Nothing M.empty M.empty IntSet.empty

restoreInterfaceRows :: InterfaceRows -> RowResult A.Module
restoreInterfaceRows rows = do
    validateInterfaceRows rows
    decls <- mapM (restoreStoredStmt RestoreExact rows M.empty M.empty M.empty) (rowStatements rows)
    return (A.Module (rowModuleName rows) (rowImports rows) (rowDoc rows) decls)

restoreExactContainer :: ContainerShape
                      -> M.Map MemberKey MemberContent
                      -> RowResult A.Decl
restoreExactContainer shape members =
    restoreContainer RestoreExact shape members (M.keysSet members) S.empty S.empty

-- The member map may contain only the directly read selected rows. Call
-- expandMemberSelection after each direct-read round until it returns
-- SelectionComplete, then pass that complete key set here. No unrelated Attr
-- row is enumerated by either operation.
restoreSelectedContainer :: ContainerShape
                         -> M.Map MemberKey MemberContent
                         -> S.Set MemberKey
                         -> S.Set A.Name
                         -> S.Set A.Name
                         -> RowResult A.Decl
restoreSelectedContainer = restoreContainer RestoreSelected

restoreStoredStmt :: RestoreMode
                  -> InterfaceRows
                  -> M.Map A.Name (S.Set MemberKey)
                  -> M.Map A.Name (S.Set A.Name)
                  -> M.Map A.Name (S.Set A.Name)
                  -> StoredStmt
                  -> RowResult A.Stmt
restoreStoredStmt _ _ _ _ _ (StoredWhole _ stmt) = return stmt
restoreStoredStmt mode rows interests staticInitializers instanceInitializers (StoredDecls l decls) =
    A.Decl l <$> mapM restore decls
  where
    restore (StoredInline decl) = return decl
    restore (StoredContainer name) = do
      shape <- maybe (rowError ("missing container shape " ++ A.rawstr name)) return
                 (M.lookup name (rowShapes rows))
      members <- maybe (rowError ("missing member rows " ++ A.rawstr name)) return
                   (M.lookup name (rowMembers rows))
      case mode of
        RestoreExact -> restoreContainer RestoreExact shape members (M.keysSet members) S.empty S.empty
        RestoreSelected ->
          restoreContainer RestoreSelected shape members
            (M.findWithDefault S.empty name interests)
            (M.findWithDefault S.empty name staticInitializers)
            (M.findWithDefault S.empty name instanceInitializers)

restoreContainer :: RestoreMode
                 -> ContainerShape
                 -> M.Map MemberKey MemberContent
                 -> S.Set MemberKey
                 -> S.Set A.Name
                 -> S.Set A.Name
                 -> RowResult A.Decl
restoreContainer mode shape members requested activeStaticInitializers activeInstanceInitializers = do
    validateContainerShape shape
    selected <- case mode of
      RestoreExact -> return requested
      RestoreSelected -> expandAttrGroups members requested
    let selectedAttrs = S.fromList [ n | Attr n <- S.toList selected ]
    when (mode == RestoreSelected &&
          not (activeStaticInitializers `S.isSubsetOf` selectedAttrs)) $
      rowError "active static attribute initializer has no selected Attr row"
    when (mode == RestoreSelected &&
          not (activeInstanceInitializers `S.isSubsetOf` selectedAttrs)) $
      rowError "active instance attribute initializer has no selected Attr row"
    loaded <- loadMembers mode shape members selected
      activeStaticInitializers activeInstanceInitializers
    validateLoadedMembers mode shape selected loaded
    suite <- restoreSuite mode rootPath (shapeSuite shape) loaded
    validateConstructorCoverage mode shape loaded
    return (restoreHead (shapeHead shape) suite)

expandMemberSelection :: M.Map MemberKey MemberContent
                      -> S.Set MemberKey
                      -> RowResult SelectionExpansion
expandMemberSelection members initial = go S.empty initial S.empty
  where
    go seen pending missing
      | S.null pending
      , S.null missing = return (SelectionComplete seen)
      | S.null pending = return (SelectionNeeds missing)
      | otherwise =
          let (key, rest) = S.deleteFindMin pending
          in if S.member key seen
               then go seen rest missing
               else case key of
                 Attr name -> case M.lookup key members of
                   Just content@AttrContent{attrNames=names} -> do
                     validateMemberContent key content
                     go (S.insert key seen)
                        (rest `S.union` S.fromList [ Attr n | n <- names ]) missing
                   Just _ -> rowError ("member kind mismatch for attribute " ++ A.rawstr name)
                   Nothing -> go (S.insert key seen) rest (S.insert key missing)
                 _ -> case M.lookup key members of
                   Just content -> do
                     validateMemberContent key content
                     go (S.insert key seen) rest missing
                   Nothing -> go (S.insert key seen) rest (S.insert key missing)

expandAttrGroups :: M.Map MemberKey MemberContent
                 -> S.Set MemberKey
                 -> RowResult (S.Set MemberKey)
expandAttrGroups members initial = do
    expansion <- expandMemberSelection members initial
    case expansion of
      SelectionComplete selected -> return selected
      SelectionNeeds missing ->
        rowError ("selected member rows still need direct reads: " ++
                  unwords (map memberLabel (S.toAscList missing)))

loadMembers :: RestoreMode
            -> ContainerShape
            -> M.Map MemberKey MemberContent
            -> S.Set MemberKey
            -> S.Set A.Name
            -> S.Set A.Name
            -> RowResult LoadedMembers
loadMembers mode shape members selected activeStaticInitializers activeInstanceInitializers = do
    loaded0 <- foldM load emptyLoadedMembers (S.toAscList selected)
    loaded <- case mode of
      RestoreExact -> return loaded0
      RestoreSelected -> pruneSelectedInitializers shape activeInstanceInitializers loaded0
    when (mode == RestoreExact) $ validateMethodRows members loaded
    return loaded
  where
    load acc key = case (key, M.lookup key members) of
      (Method name, Just (MethodContent methods)) ->
        foldM (insertMethod name) acc methods
      (Attr name, Just content@AttrContent{}) ->
        loadAttr
          (mode == RestoreExact || S.member name activeStaticInitializers)
          (mode == RestoreExact || S.member name activeInstanceInitializers)
          (isClassShape shape)
          name content acc
      (InitRest, Just content@InitRestContent{}) -> loadRest content acc
      (_, Just _) -> rowError ("member kind mismatch for " ++ memberLabel key)
      (_, Nothing) -> rowError ("missing member row " ++ memberLabel key)

insertMethod :: A.Name -> LoadedMembers -> PlacedMethod -> RowResult LoadedMembers
insertMethod expected loaded placed
  | declName /= expected = rowError ("method row/name mismatch for " ++ A.rawstr expected)
  | otherwise = do
      methods <- insertUnique (declName, placedMethodOrdinal placed) placed (loadedMethods loaded)
        ("duplicate method occurrence " ++ A.rawstr declName)
      return loaded { loadedMethods = methods }
  where
    declName = A.dname (placedMethodDecl placed)

loadAttr :: Bool -> Bool -> Bool -> A.Name -> MemberContent -> LoadedMembers -> RowResult LoadedMembers
loadAttr includeStatic includeInstance classSuite expected AttrContent{attrNames=names, attrDeclarations=decls, attrInitializers=inits} loaded
  | expected `notElem` names = rowError ("attribute row/name mismatch for " ++ A.rawstr expected)
  | otherwise = do
      statements <- foldM insertDecl (loadedStatements loaded) decls
      loadInitFragments True loaded{loadedStatements=statements}
        (filter includeInitializer inits)
  where
    includeInitializer (InitFragment (InitBody _) _) = includeInstance
    includeInitializer (InitFragment (InitSuite _) _)
      | classSuite = includeStatic
      | otherwise = includeInstance
    insertDecl acc (PlacedStmt place stmt) =
      insertSuiteStmt place stmt acc ("conflicting attribute declarations at " ++ show place)
loadAttr _ _ _ _ _ _ = rowError "loadAttr: non-attribute content"

isClassShape :: ContainerShape -> Bool
isClassShape shape = case shapeHead shape of
    ClassHead{} -> True
    _           -> False

loadRest :: MemberContent -> LoadedMembers -> RowResult LoadedMembers
loadRest InitRestContent{restConstructor=constructor, restInitializers=inits} loaded = do
    constructor' <- case (loadedConstructor loaded, constructor) of
      (Nothing, c) -> return c
      (Just old, Just new)
        | old == new -> return (Just old)
        | otherwise -> rowError "conflicting constructor rows"
      (old, Nothing) -> return old
    loadInitFragments False loaded{loadedConstructor=constructor'} inits
loadRest _ _ = rowError "loadRest: non-rest content"

loadInitFragments :: Bool -> LoadedMembers -> [InitFragment] -> RowResult LoadedMembers
loadInitFragments prunable = foldM load
  where
    load acc (InitFragment (InitBody i) stmt) = do
      body <- insertSame i stmt (loadedInitBody acc)
                ("conflicting constructor initializer " ++ show i)
      return acc
        { loadedInitBody = body
        , loadedPrunableInit = if prunable
            then IntSet.insert i (loadedPrunableInit acc)
            else loadedPrunableInit acc
        }
    load acc (InitFragment (InitSuite place) stmt) = do
      statements <- insertSuiteStmt place stmt (loadedStatements acc)
                      ("conflicting suite initializer at " ++ show place)
      return acc { loadedStatements = statements }

pruneSelectedInitializers :: ContainerShape
                          -> S.Set A.Name
                          -> LoadedMembers
                          -> RowResult LoadedMembers
pruneSelectedInitializers shape active loaded
  | IntSet.null (loadedPrunableInit loaded) = return loaded
  | otherwise = do
      self <- case [ name
                   | slot <- suiteMethodSlots (shapeSuite shape)
                   , slotIsConstructor slot
                   , Just name <- [A.selfPar (slotHeader slot)]
                   ] of
        [name] -> return name
        _ -> rowError "prunable constructor initializers have no unique self parameter"
      let prune index stmt
            | IntSet.member index (loadedPrunableInit loaded) =
                pruneConstructorInit self active stmt
            | otherwise = Just stmt
      return loaded
        { loadedInitBody = M.mapMaybeWithKey prune (loadedInitBody loaded) }

-- Project one declarative constructor statement onto the selected attributes.
-- The enclosing control flow is shared by every attribute initialized within
-- it; only writes to unselected receiver attributes are removed.
pruneConstructorInit :: A.Name -> S.Set A.Name -> A.Stmt -> Maybe A.Stmt
pruneConstructorInit self active stmt = case stmt of
    A.MutAssign _ target _
      | Just name <- selfTarget self target
      , S.notMember name active -> Nothing
    A.AugAssign _ target _ _
      | Just name <- selfTarget self target
      , S.notMember name active -> Nothing
    A.If l branches elseSuite -> Just $ A.If l
      [ A.Branch condition (pruneSuite body)
      | A.Branch condition body <- branches
      ]
      (pruneSuite elseSuite)
    A.While l condition body elseSuite ->
      Just $ A.While l condition (pruneSuite body) (pruneSuite elseSuite)
    A.For l pattern source body elseSuite ->
      Just $ A.For l pattern source (pruneSuite body) (pruneSuite elseSuite)
    A.Try l body handlers elseSuite finallySuite -> Just $ A.Try l
      (pruneSuite body)
      [ A.Handler exception (pruneSuite handlerBody)
      | A.Handler exception handlerBody <- handlers
      ]
      (pruneSuite elseSuite)
      (pruneSuite finallySuite)
    A.With l items body -> Just $ A.With l items (pruneSuite body)
    A.Data l pattern body -> Just $ A.Data l pattern (pruneSuite body)
    _ -> Just stmt
  where
    pruneSuite suite =
      [ projected
      | nested <- suite
      , Just projected <- [pruneConstructorInit self active nested]
      ]

selfTarget :: A.Name -> A.Target -> Maybe A.Name
selfTarget self (A.Dot _ (A.Var _ (A.NoQ receiver)) name)
  | receiver == self = Just name
selfTarget _ _ = Nothing

insertSuiteStmt :: StmtPlace
                -> A.Stmt
                -> M.Map SuitePath (M.Map Int A.Stmt)
                -> String
                -> RowResult (M.Map SuitePath (M.Map Int A.Stmt))
insertSuiteStmt (StmtPlace path i) stmt statements msg = do
    suite <- insertSame i stmt (M.findWithDefault M.empty path statements) msg
    return (M.insert path suite statements)

insertUnique :: Ord k => k -> a -> M.Map k a -> String -> RowResult (M.Map k a)
insertUnique key value values msg
  | M.member key values = rowError msg
  | otherwise = return (M.insert key value values)

insertSame :: (Ord k, Eq a) => k -> a -> M.Map k a -> String -> RowResult (M.Map k a)
insertSame key value values msg =
    case M.lookup key values of
      Nothing -> return (M.insert key value values)
      Just old | old == value -> return values
      Just _ -> rowError msg

restoreSuite :: RestoreMode
             -> SuitePath
             -> SuiteShape
             -> LoadedMembers
             -> RowResult A.Suite
restoreSuite mode path shape loaded = do
    structural <- foldM restore M.empty (suiteStructure shape)
    let placed = M.findWithDefault M.empty path (loadedStatements loaded)
    merged <- foldM insertPlaced structural (M.toList placed)
    when (mode == RestoreExact && M.keysSet merged /= S.fromList [0 .. suiteStmtCount shape - 1]) $
      rowError ("suite rows do not cover " ++ show path)
    return (map snd (M.toAscList merged))
  where
    restore acc (i, stmt) = do
      restored <- restoreShapeStmt mode path i stmt loaded
      case restored of
        Nothing -> return acc
        Just stmt' -> insertUnique i stmt' acc ("duplicate structural suite index " ++ show i)
    insertPlaced acc (i, stmt) =
      insertUnique i stmt acc ("structural/member statement overlap at " ++ show (StmtPlace path i))

restoreShapeStmt :: RestoreMode
                 -> SuitePath
                 -> Int
                 -> ShapeStmt
                 -> LoadedMembers
                 -> RowResult (Maybe A.Stmt)
restoreShapeStmt _ _ _ (InlineStmt stmt) _ = return (Just stmt)
restoreShapeStmt mode _ _ (DeclStmt l decls) loaded =
    Just . A.Decl l <$> mapM (restoreShapeDecl mode loaded) decls
restoreShapeStmt mode path i (IfStmt l branches elseShape) loaded = do
    branches' <- forM (zip [0..] branches) $ \(branch, (condition, shape)) ->
      A.Branch condition <$> restoreSuite mode (branchPath path i branch) shape loaded
    elseSuite <- restoreSuite mode (elsePath path i) elseShape loaded
    if mode == RestoreSelected && all (null . branchBody) branches' && null elseSuite
      then return Nothing
      else return (Just (A.If l branches' elseSuite))
  where
    branchBody (A.Branch _ body) = body

restoreShapeDecl :: RestoreMode -> LoadedMembers -> ShapeDecl -> RowResult A.Decl
restoreShapeDecl _ _ (InlineDecl decl) = return decl
restoreShapeDecl mode loaded (MethodDecl slot)
  | slotIsConstructor slot = restoreConstructorSlot mode loaded slot
  | otherwise = restoreMethodSlot mode loaded slot

restoreMethodSlot :: RestoreMode -> LoadedMembers -> MethodSlot -> RowResult A.Decl
restoreMethodSlot mode loaded slot =
    case M.lookup (slotName slot, slotOrdinal slot) (loadedMethods loaded) of
      Just placed -> validatePlacedMethod slot placed >> return (placedMethodDecl placed)
      Nothing
        | mode == RestoreExact -> rowError ("missing method body " ++ A.rawstr (slotName slot))
        | otherwise -> return (raisingStub (slotHeader slot))

restoreConstructorSlot :: RestoreMode -> LoadedMembers -> MethodSlot -> RowResult A.Decl
restoreConstructorSlot mode loaded slot =
    case loadedConstructor loaded of
      Just placed -> do
        validatePlacedMethod slot placed
        return (placedMethodDecl placed) { A.dbody = map snd (M.toAscList (loadedInitBody loaded)) }
      Nothing
        | mode == RestoreExact -> rowError "missing constructor row"
        | otherwise -> return (raisingStub (slotHeader slot))

validatePlacedMethod :: MethodSlot -> PlacedMethod -> RowResult ()
validatePlacedMethod slot placed
  | slotPlace slot /= placedMethodPlace placed = rowError ("method placement mismatch for " ++ A.rawstr name)
  | slotOrdinal slot /= placedMethodOrdinal placed = rowError ("method ordinal mismatch for " ++ A.rawstr name)
  | name /= A.dname decl = rowError ("method name mismatch for " ++ A.rawstr name)
  | slotHeader slot /= methodHeader decl = rowError ("method header mismatch for " ++ A.rawstr name)
  | otherwise = return ()
  where
    name = slotName slot
    decl = placedMethodDecl placed

raisingStub :: A.Decl -> A.Decl
raisingStub decl@A.Def{} = decl
    { A.dbody = [A.sRaise $ A.eCall (A.eQVar B.qnNotImplementedError)
        [A.Strings NoLoc ["unselected method"]]] }
raisingStub decl = error ("raisingStub: " ++ show decl)

restoreHead :: ContainerHead -> A.Suite -> A.Decl
restoreHead (ActorHead l n q p k doc) suite = A.Actor l n q p k suite doc
restoreHead (ClassHead l n q bases doc) suite = A.Class l n q bases suite doc
restoreHead (ProtocolHead l n q bases doc) suite = A.Protocol l n q bases suite doc
restoreHead (ExtensionHead l q con bases doc) suite = A.Extension l q con bases suite doc


-- Validation ---------------------------------------------------------------------------------

validateInterfaceRows :: InterfaceRows -> RowResult ()
validateInterfaceRows rows = do
    mapM_ validateStoredStmt (rowStatements rows)
    let refs =
          [ name
          | StoredDecls _ decls <- rowStatements rows
          , StoredContainer name <- decls
          ]
        shapeNames = M.keysSet (rowShapes rows)
        memberNames = M.keysSet (rowMembers rows)
    when (length refs /= S.size (S.fromList refs)) $
      rowError "duplicate container references"
    when (S.fromList refs /= shapeNames) $
      rowError "container references do not match shapes"
    when (shapeNames /= memberNames) $
      rowError "container shapes do not match member row owners"
    forM_ (M.toList (rowShapes rows)) $ \(name, shape) -> do
      when (name /= shapeName shape) $
        rowError ("container key/name mismatch for " ++ A.rawstr name)
      members <- maybe (rowError ("missing members for " ++ A.rawstr name)) return
                   (M.lookup name (rowMembers rows))
      validatePreparedContainer shape members

validateStoredStmt :: StoredStmt -> RowResult ()
validateStoredStmt (StoredWhole _ A.Decl{}) =
    rowError "top-level declaration stored as a whole statement"
validateStoredStmt (StoredWhole owners stmt)
  | length owners /= S.size (S.fromList owners) = rowError "duplicate whole-statement owners"
  | otherwise = return ()
validateStoredStmt (StoredDecls _ []) =
    rowError "empty stored declaration group"
validateStoredStmt (StoredDecls _ decls) = mapM_ validateStoredDecl decls
  where
    validateStoredDecl (StoredInline decl)
      | isContainerDecl decl = rowError "top-level container stored inline"
      | otherwise = return ()
    validateStoredDecl StoredContainer{} = return ()

validatePreparedContainer :: ContainerShape -> M.Map MemberKey MemberContent -> RowResult ()
validatePreparedContainer shape members = do
    validateContainerShape shape
    let slots = suiteMethodSlots (shapeSuite shape)
        ordinarySlots = [ slot | slot <- slots, not (slotIsConstructor slot) ]
        constructorSlots = [ slot | slot <- slots, slotIsConstructor slot ]
        slotKeys = [ (slotName slot, slotOrdinal slot) | slot <- ordinarySlots ]
        slotNames = S.fromList [ slotName slot | slot <- ordinarySlots ]
        methodNames = S.fromList [ name | Method name <- M.keys members ]
    when (hasDuplicates slotKeys) $
      rowError ("duplicate method ABI slots for " ++ A.rawstr (shapeName shape))
    forM_ (S.toList slotNames) $ \name ->
      let ordinals = [ slotOrdinal slot | slot <- ordinarySlots, slotName slot == name ]
      in when (ordinals /= [0 .. length ordinals - 1]) $
           rowError ("method ABI ordinals are not canonical for " ++ A.rawstr name)
    when (slotNames /= methodNames) $
      rowError ("method manifests do not match rows for " ++ A.rawstr (shapeName shape))
    case (shapeInitCount shape, constructorSlots, M.lookup InitRest members) of
      (Nothing, [], _) -> return ()
      (Just _, [_], Just InitRestContent{restConstructor=Just _}) -> return ()
      (Nothing, _ : _, _) -> rowError "constructor slot has no initializer count"
      (Just _, [], _) -> rowError "initializer count has no constructor slot"
      (_, _ : _ : _, _) -> rowError "multiple constructor slots"
      (_, [_], _) -> rowError "constructor slot has no InitRest row"
    forM_ (M.toList members) $ \(key, content) -> validateMemberContent key content
    forM_ (M.toList members) $ \(_, content) -> case content of
      AttrContent{attrNames=names} -> forM_ names $ \name ->
        case M.lookup (Attr name) members of
          Just AttrContent{} -> return ()
          _ -> rowError ("multi-name attribute group is missing " ++ A.rawstr name)
      _ -> return ()
    loaded <- loadMembers RestoreExact shape members (M.keysSet members) S.empty S.empty
    validateLoadedMembers RestoreExact shape (M.keysSet members) loaded
    validateConstructorCoverage RestoreExact shape loaded

validateContainerShape :: ContainerShape -> RowResult ()
validateContainerShape shape = do
    when (shapeName shape /= containerHeadName (shapeHead shape)) $
      rowError ("container head/name mismatch for " ++ A.rawstr (shapeName shape))
    validateSuiteShape rootPath (shapeSuite shape)

containerHeadName :: ContainerHead -> A.Name
containerHeadName (ActorHead _ name _ _ _ _) = name
containerHeadName (ClassHead _ name _ _ _) = name
containerHeadName (ProtocolHead _ name _ _ _) = name
containerHeadName head'@ExtensionHead{} = Names.dname' (restoreHead head' [])

validateSuiteShape :: SuitePath -> SuiteShape -> RowResult ()
validateSuiteShape path shape = do
    let entries = suiteStructure shape
        indices = map fst entries
    when (suiteStmtCount shape < 0) $
      rowError ("negative suite size at " ++ show path)
    when (indices /= IntSet.toAscList (IntSet.fromList indices)) $
      rowError ("non-canonical structural indexes at " ++ show path)
    when (any (\i -> i < 0 || i >= suiteStmtCount shape) indices) $
      rowError ("structural index outside suite at " ++ show path)
    forM_ entries $ \(i, stmt) -> validateShapeStmt path i stmt

validateShapeStmt :: SuitePath -> Int -> ShapeStmt -> RowResult ()
validateShapeStmt _ _ (InlineStmt A.If{}) = rowError "If stored as an inline shape statement"
validateShapeStmt _ _ (InlineStmt A.Decl{}) = rowError "Decl stored as an inline shape statement"
validateShapeStmt _ _ (InlineStmt _) = return ()
validateShapeStmt path i (DeclStmt _ decls) =
    forM_ (zip [0..] decls) $ \(j, decl) -> case decl of
      InlineDecl inline
        | isContainerDecl inline -> rowError "nested container stored inline"
        | otherwise -> return ()
      MethodDecl slot -> do
        when (slotPlace slot /= DeclPlace path i j) $
          rowError ("method slot placement mismatch for " ++ A.rawstr (slotName slot))
        when (slotOrdinal slot < 0) $
          rowError ("negative method ordinal for " ++ A.rawstr (slotName slot))
        case slotHeader slot of
          header@A.Def{}
            | A.dname header /= slotName slot ->
                rowError ("method slot/header name mismatch for " ++ A.rawstr (slotName slot))
            | methodHeader header == header -> return ()
            | otherwise -> rowError ("method slot is not a stripped header for " ++ A.rawstr (slotName slot))
          _ -> rowError ("non-method ABI slot for " ++ A.rawstr (slotName slot))
        when (slotIsConstructor slot && slotName slot /= B.initKW) $
          rowError "constructor slot is not __init__"
validateShapeStmt path i (IfStmt _ branches elseShape) = do
    forM_ (zip [0..] branches) $ \(branch, (_, shape)) ->
      validateSuiteShape (branchPath path i branch) shape
    validateSuiteShape (elsePath path i) elseShape

validateLoadedMembers :: RestoreMode
                      -> ContainerShape
                      -> S.Set MemberKey
                      -> LoadedMembers
                      -> RowResult ()
validateLoadedMembers mode shape selected loaded = do
    let slots = suiteMethodSlots (shapeSuite shape)
        ordinarySlots =
          [ ((slotName slot, slotOrdinal slot), slot)
          | slot <- slots
          , not (slotIsConstructor slot)
          ]
        expectedMethods = S.fromList (map fst ordinarySlots)
        slotMap = M.fromList ordinarySlots
        selectedNames = S.fromList [ name | Method name <- S.toList selected ]
        selectedMethods = S.filter (\(name, _) -> S.member name selectedNames) expectedMethods
        loadedMethodKeys = M.keysSet (loadedMethods loaded)
        expectedLoaded
          | mode == RestoreExact = expectedMethods
          | otherwise = selectedMethods
        shapes = suiteShapes rootPath (shapeSuite shape)
    when (loadedMethodKeys /= expectedLoaded) $
      rowError ("selected method rows do not match ABI slots for " ++ A.rawstr (shapeName shape))
    forM_ (M.toList (loadedMethods loaded)) $ \(key, placed) -> do
      slot <- maybe (rowError ("method row has no ABI slot " ++ show key)) return
                (M.lookup key slotMap)
      validatePlacedMethod slot placed
    case (loadedConstructor loaded, [ slot | slot <- slots, slotIsConstructor slot ]) of
      (Nothing, _) -> return ()
      (Just placed, [slot]) -> validatePlacedMethod slot placed
      (Just _, _) -> rowError "constructor row has no unique ABI slot"
    unless (all (validSuite shapes) (M.toList (loadedStatements loaded))) $
      rowError ("member statement has no suite placement in " ++ A.rawstr (shapeName shape))
    when (mode == RestoreExact) $
      validateSuiteCoverage rootPath (shapeSuite shape) (loadedStatements loaded)
    case shapeInitCount shape of
      Nothing -> unless (M.null (loadedInitBody loaded)) $
        rowError "constructor initializer has no constructor"
      Just count -> when (any (\i -> i < 0 || i >= count) (M.keys (loadedInitBody loaded))) $
        rowError "constructor initializer index is outside its body"
  where
    validSuite shapes (path, statements) =
      case M.lookup path shapes of
        Just shape -> all (\i -> i >= 0 && i < suiteStmtCount shape) (M.keys statements)
        Nothing -> False

validateSuiteCoverage :: SuitePath
                      -> SuiteShape
                      -> M.Map SuitePath (M.Map Int A.Stmt)
                      -> RowResult ()
validateSuiteCoverage path shape statements = do
    let structural = IntSet.fromList (map fst (suiteStructure shape))
        placed = IntSet.fromList (M.keys (M.findWithDefault M.empty path statements))
        expected = IntSet.fromList [0 .. suiteStmtCount shape - 1]
    unless (IntSet.null (structural `IntSet.intersection` placed)) $
      rowError ("structural/member overlap at " ++ show path)
    unless (structural `IntSet.union` placed == expected) $
      rowError ("suite rows do not cover " ++ show path)
    forM_ (suiteStructure shape) $ \(i, stmt) -> case stmt of
      IfStmt _ branches elseShape -> do
        forM_ (zip [0..] branches) $ \(branch, (_, branchShape)) ->
          validateSuiteCoverage (branchPath path i branch) branchShape statements
        validateSuiteCoverage (elsePath path i) elseShape statements
      _ -> return ()

suiteShapes :: SuitePath -> SuiteShape -> M.Map SuitePath SuiteShape
suiteShapes path shape = M.insert path shape nested
  where
    nested = M.unions
      [ inStmt i stmt | (i, stmt) <- suiteStructure shape ]
    inStmt i (IfStmt _ branches elseShape) = M.unions
      ( [ suiteShapes (branchPath path i branch) branchShape
        | (branch, (_, branchShape)) <- zip [0..] branches
        ] ++ [suiteShapes (elsePath path i) elseShape]
      )
    inStmt _ _ = M.empty

isContainerDecl :: A.Decl -> Bool
isContainerDecl A.Actor{}     = True
isContainerDecl A.Class{}     = True
isContainerDecl A.Protocol{}  = True
isContainerDecl A.Extension{} = True
isContainerDecl _             = False

validateMemberContent :: MemberKey -> MemberContent -> RowResult ()
validateMemberContent (Method name) (MethodContent methods)
  | null methods = rowError ("method row is empty for " ++ A.rawstr name)
  | any ((/= name) . A.dname . placedMethodDecl) methods =
      rowError ("method row/name mismatch for " ++ A.rawstr name)
  | map placedMethodOrdinal methods /= [0 .. length methods - 1] =
      rowError ("method ordinals are not canonical for " ++ A.rawstr name)
  | otherwise = return ()
validateMemberContent (Attr name) AttrContent{attrNames=names, attrDeclarations=decls, attrInitializers=inits}
  | null names = rowError ("attribute group is empty for " ++ A.rawstr name)
  | stableUnique names /= names = rowError ("attribute group is not canonical for " ++ A.rawstr name)
  | name `notElem` names = rowError ("attribute row/name mismatch for " ++ A.rawstr name)
  | hasDuplicates [ place | PlacedStmt place _ <- decls ] =
      rowError ("duplicate attribute declaration placement for " ++ A.rawstr name)
  | hasDuplicates [ place | InitFragment place _ <- inits ] =
      rowError ("duplicate attribute initializer placement for " ++ A.rawstr name)
  | otherwise = return ()
validateMemberContent InitRest InitRestContent{restConstructor=constructor, restInitializers=inits}
  | hasDuplicates [ place | InitFragment place _ <- inits ] =
      rowError "duplicate InitRest placement"
  | Just placed <- constructor
  , decl <- placedMethodDecl placed
  , not (isBodylessConstructor decl) =
      rowError "InitRest constructor is not a bodyless __init__ declaration"
  | otherwise = return ()
  where
    isBodylessConstructor decl@A.Def{} = A.dname decl == B.initKW && null (A.dbody decl)
    isBodylessConstructor _ = False
validateMemberContent key _ = rowError ("member kind mismatch for " ++ memberLabel key)

hasDuplicates :: Ord a => [a] -> Bool
hasDuplicates values = length values /= S.size (S.fromList values)

stableUnique :: Ord a => [a] -> [a]
stableUnique = go S.empty
  where
    go _ [] = []
    go seen (value:values)
      | S.member value seen = go seen values
      | otherwise = value : go (S.insert value seen) values

validateMethodRows :: M.Map MemberKey MemberContent -> LoadedMembers -> RowResult ()
validateMethodRows members loaded = do
    let expected = S.fromList
          [ (name, placedMethodOrdinal placed)
          | (Method name, MethodContent methods) <- M.toList members
          , placed <- methods
          ]
    when (expected /= M.keysSet (loadedMethods loaded)) $
      rowError "method rows contain duplicate or missing occurrences"

validateConstructorCoverage :: RestoreMode -> ContainerShape -> LoadedMembers -> RowResult ()
validateConstructorCoverage RestoreSelected _ _ = return ()
validateConstructorCoverage RestoreExact shape loaded =
    case shapeInitCount shape of
      Nothing -> unless (M.null (loadedInitBody loaded)) $
        rowError "constructor initializers have no constructor"
      Just count -> unless (M.keysSet (loadedInitBody loaded) == S.fromList [0 .. count - 1]) $
        rowError ("constructor rows do not cover " ++ A.rawstr (shapeName shape))

suiteMethodSlots :: SuiteShape -> [MethodSlot]
suiteMethodSlots shape = concatMap (inStmt . snd) (suiteStructure shape)
  where
    inStmt (InlineStmt _) = []
    inStmt (DeclStmt _ decls) = [ slot | MethodDecl slot <- decls ]
    inStmt (IfStmt _ branches elseShape) =
      concatMap (suiteMethodSlots . snd) branches ++ suiteMethodSlots elseShape

memberLabel :: MemberKey -> String
memberLabel (Method name) = "method:" ++ A.rawstr name
memberLabel (Attr name)   = "attr:" ++ A.rawstr name
memberLabel InitRest      = "init-rest"
