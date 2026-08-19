-- SPDX-License-Identifier: BSD-3-Clause

-- | Turn reachability into partial modules for deferred back passes.
--
-- 'Reachability' closes the program roots over persisted
-- 'ReachabilityRows'.  This module supplies its exact TYDB lookups, then uses
-- the resulting selection to reconstruct partial Acton modules from
-- 'InterfaceRows'.  Selection and materialization each keep one read
-- transaction open per participating interface, and this module computes the
-- projected environments and hashes consumed by 'Compile'.
--
-- Reachability analysis is deliberately separate from syntax loading: this
-- module does not inspect source syntax or run compiler passes.  It is the IO
-- boundary between the pure selection and the ordinary deferred back-pass
-- pipeline.  A missing row is a compiler/cache error, never a request to widen
-- the selection.
module Acton.SelectiveBack
  ( InterfaceResolver
  , Interfaces
  , SelectedProgram
  , Projection(..)
  , SelectiveBackError(..)
  , selectInterfaces
  , selectedProgramSelection
  , selectedProgramInterfaces
  , materializeInterfaceProjections
  , projectImports
  , restrictEnvironmentPublicNames
  , projectionModuleInfo
  , loadInterfaceClosure
  , bindInterfaces
  , wholeModuleSeeds
  , rootSeeds
  , rootlessModules
  , notImplementedModules
  , interfaceSourceHash
  , interfaceImplementationHash
  , interfaceEnvironment
  , materializeWholeModule
  , selectedOpaqueHashes
  , projectionUniverseHash
  , projectionCodegenHash
  ) where

import Control.Exception (Exception, throwIO)
import Control.Monad (foldM, unless)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.Binary as Binary
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set

import qualified Acton.Hashing as Hashing
import qualified Acton.Builtin as Builtin
import qualified Acton.InterfaceRows as Rows
import qualified Acton.Env as Env
import qualified Acton.NameInfo as I
import qualified Acton.Prim as Prim
import qualified Acton.QuickType as QuickType
import qualified Acton.Reachability as Reach
import Acton.ReachabilityRows
import qualified Acton.Syntax as A
import qualified InterfaceFiles


type InterfaceResolver = A.ModName -> IO (Maybe FilePath)

data InterfaceFile = InterfaceFile
  { interfacePath       :: FilePath
  , interfaceInfo       :: InterfaceFiles.InterfaceSummary
  } deriving (Eq, Show)

newtype Interfaces = Interfaces
  { interfaceMap :: Map.Map A.ModName InterfaceFile
  } deriving (Eq, Show)

data SelectedProgram = SelectedProgram
  { selectedProgramSelection :: Reach.Selection
  , selectedProgramInterfaces :: Interfaces
  } deriving (Eq, Show)

data Projection = Projection
  { projectionModule       :: A.Module
  -- Compact inferred headers for materialized containers.  Their member
  -- environments are replaced with the selected members reconstructed from
  -- syntax; every other inferred field remains authoritative.
  , projectionHeaders      :: I.TEnv
  -- These headers have no materialized declaration body and are emitted as C
  -- forward declarations only.
  , projectionDeclarations :: I.TEnv
  , projectionTypeEnv      :: I.TEnv
  , projectionTopCount     :: Int
  , projectionMemberCount  :: Int
  } deriving (Eq, Show)

data SelectiveBackError
  = MissingInterfaceModule A.ModName
  | MissingPrimitiveName A.Name
  | MissingSelectedNameHash FilePath A.Name
  | InvalidSelectedHeader FilePath TopKey TopInfo
  | MissingSelectedHeader FilePath TopKey
  | MissingSelectedContainer FilePath TopKey
  | MismatchedSelectedContainer FilePath TopKey
  | MissingDeclarationHeader FilePath TopKey
  | InvalidDeclarationHeader FilePath TopKey TopInfo
  deriving (Eq, Show)

instance Exception SelectiveBackError


-- Selection --------------------------------------------------------------------------------------------

selectInterfaces :: Interfaces
                    -> Set.Set A.ModName
                    -> [ReachEdge]
                    -> IO (Either Reach.SelectionError SelectedProgram)
selectInterfaces interfaces selectableModules seeds = do
    result <- withInterfaceSessions interfaces (Map.keysSet $ interfaceMap interfaces) $ \sessions -> do
      moduleSeeds <- fmap (concatMap reachEdges) $ mapM
        (readModuleSummary sessions) (Set.toAscList selectableModules)
      Reach.selectProgram (lookups sessions) (seeds ++ moduleSeeds)
    return $ fmap
      (\selection -> SelectedProgram
        (classifyOpaqueDeclarations selection) interfaces)
      result
  where
    readModuleSummary sessions mn =
      fst <$> InterfaceFiles.readInterfaceSessionReachSummaries
        (requiredSession sessions mn) mn

    lookups sessions = Reach.ReachLookup
      { Reach.lookupTopRow = \key@(TopKey mn _) ->
          if Set.member mn selectableModules || mn == Builtin.mBuiltin
            then readOne sessions InterfaceFiles.readInterfaceSessionReachTopMaybe key
            else return (Just $ OpaqueTop mempty)
      , Reach.lookupMemberRow = \owner member ->
          readOne sessions
            (\session key -> InterfaceFiles.readInterfaceSessionReachMemberMaybe session key member) owner
      , Reach.lookupShapeRow = readOne sessions InterfaceFiles.readInterfaceSessionReachShapeMaybe
      , Reach.lookupSlotRow = \owner member ->
          readOne sessions
            (\session key -> InterfaceFiles.readInterfaceSessionReachSlotMaybe session key member) owner
      , Reach.lookupSurfaceSlots = readRequired sessions
          InterfaceFiles.readInterfaceSessionReachSlots
      , Reach.lookupReflectableAttrs =
          readOne sessions InterfaceFiles.readInterfaceSessionReachReflectionMaybe
      }

    readOne :: Map.Map A.ModName InterfaceFiles.InterfaceReadSession
            -> (InterfaceFiles.InterfaceReadSession -> TopKey -> IO (Maybe a))
            -> TopKey
            -> IO (Maybe a)
    readOne sessions readRow key@(TopKey mn _) =
      case Map.lookup mn sessions of
        Nothing -> return Nothing
        Just session -> readRow session key

    readRequired sessions readRows key@(TopKey mn _) =
      case Map.lookup mn sessions of
        Nothing -> return []
        Just session -> readRows session key

    classifyOpaqueDeclarations selection = selection
      { Reach.selectedDeclarations = localDeclarations
      , Reach.selectedRows = Reach.selectedRows selection `Set.union`
          Set.map Reach.OpaqueTopRow opaqueDeclarations
      }
      where
        (opaqueDeclarations,localDeclarations) = Set.partition
          (\(TopKey mn _) -> Set.notMember mn selectableModules)
          (Reach.selectedDeclarations selection)

-- Interface set ----------------------------------------------------------------------------------------

-- | Resolve the complete import closure once.  Compile holds the output locks
-- for these modules until the deferred back passes finish, so ordinary LMDB
-- read transactions are sufficient; there is no second generation-validation
-- protocol here.
loadInterfaceClosure :: InterfaceResolver -> Set.Set A.ModName -> IO Interfaces
loadInterfaceClosure resolve initial =
    Interfaces <$> go Map.empty (Set.toAscList $ Set.delete Prim.mPrim initial)
  where
    go loaded [] = return loaded
    go loaded (mn:pending)
      | mn == Prim.mPrim || Map.member mn loaded = go loaded pending
      | otherwise = do
          interface <- loadInterface resolve mn
          let imports = InterfaceFiles.summaryClosureImports (interfaceInfo interface)
          go (Map.insert mn interface loaded) (imports ++ pending)

loadInterface :: InterfaceResolver -> A.ModName -> IO InterfaceFile
loadInterface resolve mn = do
    path <- resolve mn >>= maybe (throwIO $ MissingInterfaceModule mn) return
    info <- InterfaceFiles.readInterfaceSummaryMaybe path
    case info of
      Just current
        | InterfaceFiles.summaryModuleName current == mn ->
            return (InterfaceFile path current)
      _ -> throwIO (MissingInterfaceModule mn)

-- Public hashes and ordered source imports make the generated-code key change
-- whenever a lazy module lookup can observe a different public interface.
bindInterfaces :: B.ByteString -> Interfaces -> B.ByteString
bindInterfaces base =
    SHA256.hash . BL.toStrict . Binary.encode . facts
  where
    facts interfaces =
      ( "selective-interfaces-v1" :: String
      , base
      , [ ( semanticModName mn
          , map semanticModName $ InterfaceFiles.summarySourceImports info
          , InterfaceFiles.summaryPublicHash info
          )
        | (mn,interface) <- Map.toAscList (interfaceMap interfaces)
        , let info = interfaceInfo interface
        ]
      )

wholeModuleSeeds :: Interfaces -> Set.Set A.ModName -> IO [ReachEdge]
wholeModuleSeeds interfaces modules =
    withInterfaceSessions interfaces selected $ \sessions ->
      fmap (concatMap reachEdges) $ mapM (readOne sessions) (Set.toAscList selected)
  where
    selected = Set.delete Prim.mPrim modules
    readOne sessions mn = snd <$>
      InterfaceFiles.readInterfaceSessionReachSummaries
        (requiredSession sessions mn) mn

rootSeeds :: Interfaces -> [(A.ModName,A.Name)] -> IO [ReachEdge]
rootSeeds interfaces candidates = fmap concat $ mapM rootSeed candidates
  where
    rootSeed (mn,root) = do
      interface <- requireInterface interfaces mn
      return
        [ Construct mn root
        | root `elem` InterfaceFiles.summaryRoots (interfaceInfo interface)
        ]

rootlessModules :: Interfaces -> Set.Set A.ModName
rootlessModules = Map.keysSet . Map.filter noRoots . interfaceMap
  where
    noRoots = null . InterfaceFiles.summaryRoots . interfaceInfo

notImplementedModules :: Interfaces -> Set.Set A.ModName
notImplementedModules = Map.keysSet . Map.filter hasNotImpl . interfaceMap
  where
    hasNotImpl = InterfaceFiles.summaryHasNotImpl . interfaceInfo

interfaceSourceHash :: Interfaces -> A.ModName -> IO B.ByteString
interfaceSourceHash interfaces mn =
    InterfaceFiles.summarySourceHash . interfaceInfo <$> requireInterface interfaces mn

interfaceImplementationHash :: Interfaces -> A.ModName -> IO B.ByteString
interfaceImplementationHash interfaces mn =
    InterfaceFiles.summaryImplementationHash . interfaceInfo <$> requireInterface interfaces mn

-- | Rebuild the deferred environment from the same explicit interface set used
-- for selection.  ModuleInfo performs its normal exact TYDB reads.
interfaceEnvironment :: Interfaces -> IO Env.Env0
interfaceEnvironment interfaces = do
    builtin <- requireInterface interfaces Builtin.mBuiltin
    builtinDB <- InterfaceFiles.openInterfaceDB (interfacePath builtin)
    (_, I.NModule _ builtinEnv builtinDoc) <-
      InterfaceFiles.readInterfaceDBIface builtinDB
    let base = Env.initEnvFromBuiltin builtinEnv builtinDoc
    foldM install base (Map.toAscList $ interfaceMap interfaces)
  where
    install env (mn,interface)
      | mn == Prim.mPrim || mn == Builtin.mBuiltin = return env
      | otherwise = do
          db <- InterfaceFiles.openInterfaceDB (interfacePath interface)
          let info = interfaceInfo interface
              moduleInfo = Env.mkTyFileModuleInfo mn
                (InterfaceFiles.summarySourceImports info)
                (InterfaceFiles.summaryDoc info)
                db
          return (Env.addModuleInfo mn moduleInfo env)

materializeWholeModule :: Interfaces
                       -> A.ModName
                       -> IO (A.Module,B.ByteString)
materializeWholeModule interfaces mn = do
    interface <- requireInterface interfaces mn
    (_imports,_nmod,typed,_sourceMeta,_sourceHash,_publicHash,implementationHash,
      _hashedImports,_depModules,_nameHashes,_roots,_tests,_doc) <-
        InterfaceFiles.readFile (interfacePath interface)
    unless (A.modname typed == mn) $
      throwIO (MissingInterfaceModule mn)
    return (typed,implementationHash)

requireInterface :: Interfaces -> A.ModName -> IO InterfaceFile
requireInterface interfaces mn = case Map.lookup mn (interfaceMap interfaces) of
    Just interface -> return interface
    Nothing -> throwIO (MissingInterfaceModule mn)

withInterfaceSessions :: Interfaces
                      -> Set.Set A.ModName
                      -> (Map.Map A.ModName InterfaceFiles.InterfaceReadSession -> IO a)
                      -> IO a
withInterfaceSessions interfaces modules action =
    open Map.empty (Set.toAscList $ Set.delete Prim.mPrim modules)
  where
    open sessions [] = action sessions
    open sessions (mn:rest) = do
      interface <- requireInterface interfaces mn
      InterfaceFiles.withInterfaceReadSession (interfacePath interface) $ \session ->
        open (Map.insert mn session sessions) rest

requiredSession :: Map.Map A.ModName InterfaceFiles.InterfaceReadSession
                -> A.ModName
                -> InterfaceFiles.InterfaceReadSession
requiredSession sessions mn = case Map.lookup mn sessions of
    Just session -> session
    Nothing -> error ("Missing interface session for " ++ show mn)

unionInterfaces :: Interfaces -> Interfaces -> Interfaces
unionInterfaces left right =
    Interfaces (Map.union (interfaceMap left) (interfaceMap right))

-- Materialization --------------------------------------------------------------------------------------

materializeInterfaceProjections :: [A.ModName]
                                -> SelectedProgram
                                -> IO [Projection]
materializeInterfaceProjections modules program =
    withInterfaceSessions interfaces (Set.fromList modules) $ \sessions ->
      mapM (materialize sessions) modules
  where
    interfaces = selectedProgramInterfaces program
    selection = selectedProgramSelection program

    materialize sessions mn = do
      interface <- requireInterface interfaces mn
      materializeProjection (interfacePath interface)
        (requiredSession sessions mn) mn selection

-- | Fingerprint opaque declarations, extending the interface set for
-- providers reached directly from external seeds.
selectedOpaqueHashes :: InterfaceResolver
                     -> SelectedProgram
                     -> IO (SelectedProgram,[(TopKey,B.ByteString)])
selectedOpaqueHashes resolve program = do
    opaqueInterfaces <- loadInterfaceClosure resolve missingModules
    let interfaces = selectedProgramInterfaces program `unionInterfaces` opaqueInterfaces
        capturedProgram = program{ selectedProgramInterfaces = interfaces }
    let opaqueModules = Set.fromList
          [ mn | TopKey mn _ <- opaqueTops, mn /= Prim.mPrim ]
    hashes <- withInterfaceSessions interfaces opaqueModules $ \sessions ->
      mapM (fingerprint interfaces sessions) opaqueTops
    return (capturedProgram,hashes)
  where
    selection = selectedProgramSelection program
    opaqueTops = Set.toAscList $ Reach.selectedOpaqueTops selection
    captured = interfaceMap $ selectedProgramInterfaces program
    missingModules = Set.fromList
      [ mn
      | TopKey mn _ <- opaqueTops
      , mn /= Prim.mPrim
      , Map.notMember mn captured
      ]

    fingerprint interfaces sessions key@(TopKey mn name)
      | mn == Prim.mPrim = case lookup name Prim.primEnv of
          Nothing -> throwIO (MissingPrimitiveName name)
          Just info -> case Map.lookup name $ Hashing.nameInfoHashes $ Map.singleton name info of
            Nothing -> throwIO (MissingPrimitiveName name)
            Just hash -> return (key,hash)
      | otherwise = do
          interface <- requireInterface interfaces mn
          let session = requiredSession sessions mn
              tyFile = interfacePath interface
          row <- InterfaceFiles.readInterfaceSessionNameHashMaybe session name
          info <- maybe (throwIO $ MissingSelectedNameHash tyFile name) return row
          return (key,InterfaceFiles.nhPubHash info)

materializeProjection :: FilePath
                      -> InterfaceFiles.InterfaceReadSession
                      -> A.ModName
                      -> Reach.Selection
                      -> IO Projection
materializeProjection tyFile session mn selection = do
    nameHashes <- mapM readNameHash selectedNames
    selectedModule <- InterfaceFiles.readInterfaceSessionSelection
      session nameHashes (Set.fromList selectedNames) memberInterests
    let typedModule = selectedModule
          { A.imps = projectImports selection (A.imps selectedModule) }
    headers <- catMaybes <$> mapM readSelectedHeader selectedKeys
    declarations <- mapM readDeclaration declarationKeys
    projectedEnv <- either throwIO return $
      mergeProjectionTEnv tyFile mn headers declarations
        (projectionSyntaxEnv $ A.mbody typedModule)
    return Projection
      { projectionModule = typedModule
      , projectionHeaders = headers
      , projectionDeclarations = declarations
      , projectionTypeEnv = projectedEnv
      , projectionTopCount = length selectedNames
      , projectionMemberCount = sum (map Set.size $ Map.elems memberInterests)
      }
  where
    selectedKeys =
      [ key
      | key@(TopKey moduleName _) <- Set.toAscList (Reach.selectedTops selection)
      , moduleName == mn
      ]
    selectedNames = [ name | TopKey _ name <- selectedKeys ]
    declarationKeys =
      [ key
      | key@(TopKey moduleName _) <- Set.toAscList (Reach.selectedDeclarations selection)
      , moduleName == mn
      ]
    memberInterests = Map.fromListWith Set.union
      ( [ (ownerName, Set.singleton member)
        | (TopKey moduleName ownerName, member) <- Set.toAscList (Reach.selectedMembers selection)
        , moduleName == mn
        ] ++
        [ (ownerName, Set.singleton $ Rows.StaticInit attr)
      | (TopKey moduleName ownerName, attr) <-
          Set.toAscList (Reach.selectedStaticInitializers selection)
      , moduleName == mn
        ] ++
        [ (ownerName, Set.singleton $ Rows.InstanceInit attr)
      | (TopKey moduleName ownerName, attr) <-
          Set.toAscList (Reach.selectedInstanceInitializers selection)
      , moduleName == mn
        ]
      )

    readNameHash name = do
      row <- InterfaceFiles.readInterfaceSessionNameHashMaybe session name
      maybe (throwIO $ MissingSelectedNameHash tyFile name) return row

    readSelectedHeader key@(TopKey _ name) = do
      row <- InterfaceFiles.readInterfaceSessionReachTop session key
      case row of
        LocalTop header _ -> return $ fmap ((,) name) header
        _ -> throwIO (InvalidSelectedHeader tyFile key row)

    readDeclaration key@(TopKey _ name) = do
      row <- InterfaceFiles.readInterfaceSessionReachTop session key
      case row of
        LocalTop (Just info) _ -> return (name, info)
        LocalTop Nothing _ -> throwIO (MissingDeclarationHeader tyFile key)
        _ -> throwIO (InvalidDeclarationHeader tyFile key row)

-- | Keep module imports, which name a qualifier or a wildcard, and project
-- explicit imports to the exact provider names retained by the global
-- selection. Wildcards are narrowed through 'restrictEnvironmentPublicNames'
-- so their ordinary importability rules remain unchanged. An empty explicit
-- import still carries the provider's module initialization dependency.
projectImports :: Reach.Selection -> [A.Import] -> [A.Import]
projectImports selection = map project
  where
    selected = Reach.selectedTops selection `Set.union`
      Reach.selectedDeclarations selection `Set.union`
      Reach.selectedOpaqueTops selection

    project importSpec = case importSpec of
      A.FromImport loc mn items ->
        A.FromImport loc mn (filter (retained mn) items)
      _ -> importSpec

    retained mn (A.ImportItem name _) =
      Set.member (TopKey mn name) selected


-- | Limit wildcard enumeration to names that the global selection has
-- already retained. The original ModuleInfo still decides whether each name
-- is importable, so extensions and other non-value entries keep their normal
-- wildcard behavior without loading unrelated public rows.
restrictEnvironmentPublicNames :: Reach.Selection -> Env.Env0 -> Env.Env0
restrictEnvironmentPublicNames selection env = env
    { Env.modules = Map.mapWithKey restrict (Env.modules env) }
  where
    selected = Reach.selectedTops selection `Set.union`
      Reach.selectedDeclarations selection `Set.union`
      Reach.selectedOpaqueTops selection
    byModule = Map.fromListWith Set.union
      [ (mn,Set.singleton name)
      | TopKey mn name <- Set.toAscList selected
      ]
    restrict mn info = info
      { Env.modulePublicNames = filter retained (Env.modulePublicNames info) }
      where retained name = Set.member name $ Map.findWithDefault Set.empty mn byModule

-- | Install the projected name environment while retaining the original
-- exact witness indexes.  Type-directed lookup of converted witness methods
-- still needs those indexes; the projected names decide which witnesses are
-- available to the back passes.
projectionModuleInfo :: Env.Env0 -> Projection -> Env.ModuleInfo
projectionModuleInfo env projection = projectedInfo
    { Env.moduleWitnessesByProto = Env.moduleWitnessesByProto original
    , Env.moduleWitnessesByType = Env.moduleWitnessesByType original
    }
  where
    typed = projectionModule projection
    mn = A.modname typed
    projected = projectionTypeEnv projection
    moduleEnv = Env.defineClosed projected $ Env.setMod mn env
    projectedInfo = Env.mkModuleInfo mn (A.importsOf typed)
      (Env.unalias moduleEnv projected) (A.mdoc typed)
    original = case Env.lookupModuleInfo mn env of
      Just info -> info
      Nothing -> error ("Missing module environment for selective projection " ++ show mn)

mergeProjectionTEnv :: FilePath
                    -> A.ModName
                    -> I.TEnv
                    -> I.TEnv
                    -> I.TEnv
                    -> Either SelectiveBackError I.TEnv
mergeProjectionTEnv tyFile mn headers declarations syntaxEnv = do
    mapM_ requireContainerHeader syntaxEnv
    mapM_ requireSelectedContainer headers
    merged <- mapM mergeBinding syntaxEnv
    return (merged ++ declarations)
  where
    headerMap = Map.fromList headers

    requireContainerHeader (name,info)
      | isContainerInfo info,
        Map.notMember name headerMap = Left (MissingSelectedHeader tyFile $ TopKey mn name)
      | otherwise = Right ()

    requireSelectedContainer (name,_) =
      case lookup name syntaxEnv of
        Nothing -> Left (MissingSelectedContainer tyFile $ TopKey mn name)
        Just _  -> Right ()

    mergeBinding binding@(name,syntaxInfo) =
      case Map.lookup name headerMap of
        Nothing -> Right binding
        Just headerInfo ->
          case mergeContainerInfo headerInfo syntaxInfo of
            Just info -> Right (name,info)
            Nothing -> Left (MismatchedSelectedContainer tyFile $ TopKey mn name)

    isContainerInfo info = case info of
      I.NClass{} -> True
      I.NProto{} -> True
      I.NAct{}   -> True
      I.NExt{}   -> True
      _          -> False

mergeContainerInfo :: I.NameInfo -> I.NameInfo -> Maybe I.NameInfo
mergeContainerInfo header syntaxInfo = case (header,syntaxInfo) of
    (I.NClass q bases [] doc, I.NClass _ _ members _) ->
      Just (I.NClass q bases members doc)
    (I.NProto q bases [] doc, I.NProto _ _ members _) ->
      Just (I.NProto q bases members doc)
    (I.NAct q pos kwd [] doc, I.NAct _ _ _ members _) ->
      Just (I.NAct q pos kwd members doc)
    (I.NExt q target bases [] opts doc, I.NExt _ _ _ members _ _) ->
      Just (I.NExt q target bases members opts doc)
    _ -> Nothing

projectionSyntaxEnv :: A.Suite -> I.TEnv
projectionSyntaxEnv = QuickType.envOfTopSuite


-- Hashing -----------------------------------------------------------------------------------------------

-- | One semantic key for the whole selected universe.  Consequently a change
-- in a selected consumer reruns every selected provider back pass, while an
-- edit confined to unmaterialized code is absent from the key.
projectionUniverseHash :: Reach.Selection
                       -> [Projection]
                       -> [(TopKey,B.ByteString)]
                       -> B.ByteString
projectionUniverseHash selection projections opaqueHashes =
    SHA256.hash $ BL.toStrict $ Binary.encode
      ( "selective-back-v4" :: String
      , Hashing.codegenIdentity
      , A.version
      , selectionProjectionFacts selection
      , sortOn fst
          [ ( semanticModName $ A.modname typed
            , Hashing.moduleProjectionHash typed (projectionTypeEnv projection)
            )
          | projection <- projections
          , let typed = projectionModule projection
          ]
      , [ (semanticModName mn,semanticName name,hash)
        | (TopKey mn name,hash) <- sortOn fst opaqueHashes
        ]
      )

-- A canonical, constructor-tagged encoding of every closure fact that can
-- alter materialization or generated declarations.  This deliberately avoids
-- Show: source locations and presentation changes are not semantic keys.
selectionProjectionFacts :: Reach.Selection
                         -> [(Int,A.ModName,A.Name,Int,Maybe A.Name)]
selectionProjectionFacts selection =
    map (topFact 0) (Set.toAscList $ Reach.selectedDeclarations selection) ++
    map (topFact 1) (Set.toAscList $ Reach.selectedTops selection) ++
    map (topFact 2) (Set.toAscList $ Reach.selectedOpaqueTops selection) ++
    map (memberFact 3) (Set.toAscList $ Reach.selectedMembers selection) ++
    map (nameFact 4) (Set.toAscList $ Reach.selectedAttrs selection) ++
    map (nameFact 5) (Set.toAscList $ Reach.selectedStaticInitializers selection) ++
    map (nameFact 6) (Set.toAscList $ Reach.selectedInstanceInitializers selection) ++
    map (generatedFact 7) (Set.toAscList $ Reach.selectedGenerated selection) ++
    map (topFact 8) (Set.toAscList $ Reach.selectedConstructed selection) ++
    map (topFact 9) (Set.toAscList $ Reach.selectedInitialized selection)
  where
    topFact tag (TopKey moduleName name) =
      (tag,semanticModName moduleName,semanticName name,0,Nothing)

    memberFact tag (key,member) =
      let (memberTag,memberName) = memberKey member
          (category,moduleName,owner,_,_) = topFact tag key
      in (category,moduleName,owner,memberTag,memberName)

    nameFact tag (key,name) =
      let (category,moduleName,owner,_,_) = topFact tag key
      in (category,moduleName,owner,0,Just $ semanticName name)

    generatedFact tag (key,ref) =
      let (refTag,refName) = memberRef ref
          (category,moduleName,owner,_,_) = topFact tag key
      in (category,moduleName,owner,refTag,refName)

    memberKey member = case member of
      Rows.Method name       -> (0,Just $ semanticName name)
      Rows.Attr name         -> (1,Just $ semanticName name)
      Rows.StaticInit name   -> (2,Just $ semanticName name)
      Rows.InstanceInit name -> (3,Just $ semanticName name)
      Rows.InitRest          -> (4,Nothing)

    memberRef ref = case ref of
      MethodRef name -> (0,Just $ semanticName name)
      AttrRef name   -> (1,Just $ semanticName name)

semanticModName :: A.ModName -> A.ModName
semanticModName (A.ModName names) = A.ModName (map semanticName names)

semanticName :: A.Name -> A.Name
semanticName (A.Name _ name) = A.name name
semanticName (A.Derived owner member) =
    A.Derived (semanticName owner) (semanticName member)
semanticName name@A.Internal{} = name

projectionCodegenHash :: B.ByteString -> A.ModName -> B.ByteString
projectionCodegenHash universe mn =
    SHA256.hash $ BL.toStrict $ Binary.encode
      ("selective-back-module-v4" :: String, universe, semanticModName mn)
