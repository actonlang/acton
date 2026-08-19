-- SPDX-License-Identifier: BSD-3-Clause

{-# LANGUAGE DeriveGeneric #-}

-- | Independently loadable typed syntax.
--
-- A container row is an ordinary Acton syntax skeleton: it keeps the
-- container and method headers, but replaces independently selectable
-- statements with numbered holes.  Method bodies, attribute declarations and
-- initializer statements live in member rows.  Reconstruction is therefore a
-- small syntax-tree traversal which fills selected holes and turns unselected
-- methods into ABI-preserving stubs.
--
-- This module defines and reconstructs the syntax rows.
-- 'Acton.Reachability' partitions typed syntax, records what each row uses,
-- and selects the rows needed by a build; 'InterfaceFiles' stores and reads
-- them by exact key.
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


-- Stored syntax -------------------------------------------------------------------------------

data MemberKey
  = Method A.Name
  | Attr A.Name
  | StaticInit A.Name
  | InstanceInit A.Name
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

data MethodSlot = MethodSlot
  { slotName          :: A.Name
  , slotOrdinal       :: Int
  , slotIsConstructor :: Bool
  , slotHeader        :: A.Decl
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
  | HoleStmt Int
  | DeclStmt SrcLoc [ShapeDecl]
  | IfStmt SrcLoc [(A.Expr, SuiteShape)] SuiteShape
  deriving (Show, Eq, Generic)

instance Persist.Persist ShapeStmt
instance NFData ShapeStmt

newtype SuiteShape = SuiteShape { suiteShape :: [ShapeStmt] }
  deriving (Show, Eq, Generic)

instance Persist.Persist SuiteShape
instance NFData SuiteShape

data ContainerShape = ContainerShape
  { shapeName  :: A.Name
  , shapeHead  :: ContainerHead
  , shapeSuite :: SuiteShape
  } deriving (Show, Eq, Generic)

instance Persist.Persist ContainerShape
instance NFData ContainerShape

-- | A numbered statement removed from either the container suite or the
-- declarative prefix of a class constructor.
data Fragment
  = SuiteFragment Int A.Stmt
  | ConstructorFragment Int A.Stmt
  deriving (Show, Eq, Generic)

instance Persist.Persist Fragment
instance NFData Fragment

data MemberContent
  = MethodContent [A.Decl]
  | AttrContent [Fragment]
  | InitializerContent [Fragment]
  | InitRestContent
      { restConstructor  :: Maybe A.Decl
      , restInitializers :: [Fragment]
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

rowError :: String -> RowResult a
rowError = Left . RowError


-- Reconstruction -------------------------------------------------------------------------------

data RestoreMode = RestoreExact | RestoreSelected deriving Eq

data LoadedMembers = LoadedMembers
  { loadedMethods      :: M.Map (A.Name, Int) A.Decl
  , loadedConstructor  :: Maybe A.Decl
  , loadedStatements   :: M.Map Int A.Stmt
  , loadedInitBody     :: M.Map Int A.Stmt
  , loadedPrunableInit :: IntSet.IntSet
  }

emptyLoadedMembers :: LoadedMembers
emptyLoadedMembers = LoadedMembers M.empty Nothing M.empty M.empty IntSet.empty

restoreInterfaceRows :: InterfaceRows -> RowResult A.Module
restoreInterfaceRows rows = do
    stmts <- mapM (restoreStoredStmt rows) (rowStatements rows)
    return (A.Module (rowModuleName rows) (rowImports rows) (rowDoc rows) stmts)

restoreStoredStmt :: InterfaceRows -> StoredStmt -> RowResult A.Stmt
restoreStoredStmt _ (StoredWhole _ stmt) = return stmt
restoreStoredStmt rows (StoredDecls l decls) = A.Decl l <$> mapM restore decls
  where
    restore (StoredInline decl) = return decl
    restore (StoredContainer name) = do
      shape <- required ("missing container shape " ++ A.rawstr name) (M.lookup name $ rowShapes rows)
      members <- required ("missing member rows " ++ A.rawstr name) (M.lookup name $ rowMembers rows)
      restoreExactContainer shape members

restoreExactContainer :: ContainerShape -> M.Map MemberKey MemberContent -> RowResult A.Decl
restoreExactContainer shape members =
    restoreContainer RestoreExact shape members (M.keysSet members)

restoreSelectedContainer :: ContainerShape
                         -> M.Map MemberKey MemberContent
                         -> S.Set MemberKey
                         -> RowResult A.Decl
restoreSelectedContainer = restoreContainer RestoreSelected

restoreContainer :: RestoreMode
                 -> ContainerShape
                 -> M.Map MemberKey MemberContent
                 -> S.Set MemberKey
                 -> RowResult A.Decl
restoreContainer mode shape members selected = do
    loaded0 <- foldM loadMember emptyLoadedMembers (S.toAscList selected)
    loaded <- case mode of
      RestoreExact -> return loaded0
      RestoreSelected -> pruneSelectedInitializers shape instanceInitializers loaded0
    suite <- restoreSuite mode (shapeSuite shape) loaded
    return (restoreHead (shapeHead shape) suite)
  where
    loadMember loaded key = case (key, M.lookup key members) of
      (Method name, Just (MethodContent decls)) ->
        foldM (insertMethod name) loaded (zip [0..] decls)
      (Attr _, Just (AttrContent declarations)) ->
        loadAttr mode selectedAttrs declarations loaded
      (StaticInit _, Just (InitializerContent fragments)) ->
        foldM (loadFragment $ const False) loaded fragments
      (InstanceInit _, Just (InitializerContent fragments)) ->
        foldM (loadFragment isConstructorFragment) loaded fragments
      (InitRest, Just content@InitRestContent{}) -> loadRest content loaded
      (_, Just _) -> rowError ("member kind mismatch for " ++ memberLabel key)
      (_, Nothing) -> rowError ("missing member row " ++ memberLabel key)
    selectedAttrs = S.fromList [ name | Attr name <- S.toAscList selected ]
    instanceInitializers = S.fromList [ name | InstanceInit name <- S.toAscList selected ]
    isConstructorFragment ConstructorFragment{} = True
    isConstructorFragment _                     = False

insertMethod :: A.Name -> LoadedMembers -> (Int, A.Decl) -> RowResult LoadedMembers
insertMethod name loaded (ordinal, decl)
  | A.dname decl /= name = rowError ("method row/name mismatch for " ++ A.rawstr name)
  | M.member (name, ordinal) (loadedMethods loaded) =
      rowError ("duplicate method occurrence " ++ A.rawstr name)
  | otherwise = return loaded
      { loadedMethods = M.insert (name, ordinal) decl (loadedMethods loaded) }

loadAttr :: RestoreMode
         -> S.Set A.Name
         -> [Fragment]
         -> LoadedMembers
         -> RowResult LoadedMembers
loadAttr mode selected declarations loaded =
    foldM (loadFragment $ const False) loaded (map narrow declarations)
  where
    narrow fragment
      | mode == RestoreSelected = narrowProperty selected fragment
      | otherwise               = fragment

narrowProperty :: S.Set A.Name -> Fragment -> Fragment
narrowProperty selected (SuiteFragment hole (A.Signature l names schema A.Property)) =
    SuiteFragment hole (A.Signature l (filter (`S.member` selected) names) schema A.Property)
narrowProperty _ fragment = fragment

loadRest :: MemberContent -> LoadedMembers -> RowResult LoadedMembers
loadRest content loaded = do
    constructor <- case (loadedConstructor loaded, restConstructor content) of
      (Nothing, new) -> return new
      (Just old, Just new)
        | old == new -> return (Just old)
        | otherwise -> rowError "conflicting constructor rows"
      (old, Nothing) -> return old
    foldM (loadFragment (const False)) loaded{loadedConstructor=constructor}
      (restInitializers content)

loadFragment :: (Fragment -> Bool) -> LoadedMembers -> Fragment -> RowResult LoadedMembers
loadFragment prunable loaded fragment = case fragment of
    SuiteFragment hole stmt -> do
      statements <- insertSame hole stmt (loadedStatements loaded)
        ("conflicting suite fragment " ++ show hole)
      return loaded { loadedStatements = statements }
    ConstructorFragment index stmt -> do
      body <- insertSame index stmt (loadedInitBody loaded)
        ("conflicting constructor fragment " ++ show index)
      return loaded
        { loadedInitBody = body
        , loadedPrunableInit = if prunable fragment
            then IntSet.insert index (loadedPrunableInit loaded)
            else loadedPrunableInit loaded
        }

pruneSelectedInitializers :: ContainerShape
                          -> S.Set A.Name
                          -> LoadedMembers
                          -> RowResult LoadedMembers
pruneSelectedInitializers shape active loaded
  | IntSet.null (loadedPrunableInit loaded) = return loaded
  | otherwise = do
      self <- case [ name
                   | slot <- methodSlots (shapeSuite shape)
                   , slotIsConstructor slot
                   , Just name <- [A.selfPar (slotHeader slot)]
                   ] of
        [name] -> return name
        _ -> rowError "prunable constructor initializers have no unique self parameter"
      return loaded
        { loadedInitBody = M.mapMaybeWithKey (project self) (loadedInitBody loaded) }
  where
    project self index stmt
      | IntSet.member index (loadedPrunableInit loaded) = pruneConstructorInit self active stmt
      | otherwise = Just stmt

restoreSuite :: RestoreMode -> SuiteShape -> LoadedMembers -> RowResult A.Suite
restoreSuite mode (SuiteShape shape) loaded = fmap concat $ mapM restore shape
  where
    restore (InlineStmt stmt) = return [stmt]
    restore (HoleStmt hole) = case M.lookup hole (loadedStatements loaded) of
      Just stmt -> return [stmt]
      Nothing
        | mode == RestoreExact -> rowError ("missing suite fragment " ++ show hole)
        | otherwise -> return []
    restore (DeclStmt l decls) = (:[]) . A.Decl l <$> mapM restoreDecl decls
    restore (IfStmt l branches elseShape) = do
      branches' <- mapM restoreBranch branches
      elseSuite <- restoreSuite mode elseShape loaded
      if mode == RestoreSelected && all (null . branchBody) branches' && null elseSuite
        then return []
        else return [A.If l branches' elseSuite]
    restoreDecl (InlineDecl decl) = return decl
    restoreDecl (MethodDecl slot)
      | slotIsConstructor slot = restoreConstructor slot
      | otherwise = restoreMethod slot
    restoreMethod slot = case M.lookup (slotName slot, slotOrdinal slot) (loadedMethods loaded) of
      Just decl -> return decl
      Nothing
        | mode == RestoreExact -> rowError ("missing method body " ++ A.rawstr (slotName slot))
        | otherwise -> return (raisingStub $ slotHeader slot)
    restoreConstructor slot = case loadedConstructor loaded of
      Just decl -> return decl { A.dbody = map snd (M.toAscList $ loadedInitBody loaded) }
      Nothing
        | mode == RestoreExact -> rowError "missing constructor row"
        | otherwise -> return (raisingStub $ slotHeader slot)
    restoreBranch (condition, body) = A.Branch condition <$> restoreSuite mode body loaded
    branchBody (A.Branch _ body) = body

methodSlots :: SuiteShape -> [MethodSlot]
methodSlots (SuiteShape stmts) = concatMap inStmt stmts
  where
    inStmt (DeclStmt _ decls) = [ slot | MethodDecl slot <- decls ]
    inStmt (IfStmt _ branches elseShape) =
      concatMap (methodSlots . snd) branches ++ methodSlots elseShape
    inStmt _ = []

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

pruneConstructorInit :: A.Name -> S.Set A.Name -> A.Stmt -> Maybe A.Stmt
pruneConstructorInit self active stmt = case stmt of
    A.MutAssign _ target _
      | Just name <- selfTarget self target
      , S.notMember name active -> Nothing
    A.AugAssign _ target _ _
      | Just name <- selfTarget self target
      , S.notMember name active -> Nothing
    A.If l branches elseSuite -> Just $ A.If l
      [ A.Branch condition (pruneSuite body) | A.Branch condition body <- branches ]
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

insertSame :: (Ord k, Eq a) => k -> a -> M.Map k a -> String -> RowResult (M.Map k a)
insertSame key value values msg = case M.lookup key values of
    Nothing -> return (M.insert key value values)
    Just old | old == value -> return values
    Just _ -> rowError msg

required :: String -> Maybe a -> RowResult a
required msg = maybe (rowError msg) return

memberLabel :: MemberKey -> String
memberLabel (Method name)       = "method " ++ A.rawstr name
memberLabel (Attr name)         = "attribute " ++ A.rawstr name
memberLabel (StaticInit name)   = "static initializer " ++ A.rawstr name
memberLabel (InstanceInit name) = "instance initializer " ++ A.rawstr name
memberLabel InitRest            = "constructor rest"
