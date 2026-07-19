-- SPDX-License-Identifier: BSD-3-Clause

-- | Exact-key selection and materialization for deferred back passes.
--
-- Reachability is deliberately separate from syntax loading.  The closure
-- reads only exact-key semantic rows demanded by the worklist; once it has
-- converged, each module is reconstructed from selected syntax fragments and
-- compact headers.  A missing row is a compiler/cache error, never a request
-- to widen the selection.
module Acton.SelectiveBack
  ( InterfaceResolver
  , InterfaceSnapshots
  , SelectedProgram
  , Projection(..)
  , SelectiveBackError(..)
  , selectFromInterfaces
  , selectFromSnapshots
  , selectedProgramSelection
  , selectedProgramSnapshots
  , materializeInterfaceProjections
  , materializeInterfaceProjection
  , projectionModuleInfo
  , captureInterfaceSnapshots
  , captureInterfaceClosure
  , validateInterfaceSnapshots
  , bindInterfaceSnapshots
  , snapshotWholeModuleSeeds
  , snapshotRootSeeds
  , snapshotNotImplementedModules
  , snapshotSourceHash
  , snapshotImplementationHash
  , snapshotEnvironment
  , materializeWholeModule
  , captureSelectedOpaqueHashes
  , validateSelectedProgram
  , projectionTEnv
  , projectionUniverseHash
  , projectionCodegenHash
  ) where

import Control.Exception (Exception, finally, throwIO)
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
import Acton.ReachabilityRows
import Acton.ReachabilityTypes
import qualified Acton.SelectiveWorklist as Worklist
import qualified Acton.Syntax as A
import qualified InterfaceFiles


-- | Resolve every interface that claims a canonical module name.  Normal
-- compilations return zero or one path.  Returning several preserves enough
-- information for the worklist to report a precise ambiguity instead of
-- silently picking a provider.
type InterfaceResolver = A.ModName -> IO [FilePath]

data InterfaceSnapshot = InterfaceSnapshot
  { snapshotPath       :: FilePath
  , snapshotInfo       :: InterfaceFiles.ModuleSnapshot
  } deriving (Eq, Show)

newtype InterfaceSnapshots = InterfaceSnapshots
  { interfaceSnapshotMap :: Map.Map A.ModName InterfaceSnapshot
  } deriving (Eq, Show)

data SelectedProgram = SelectedProgram
  { selectedProgramSelection :: Worklist.Selection
  , selectedProgramSnapshots :: InterfaceSnapshots
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
  = AmbiguousInterfaceModule A.ModName [FilePath]
  | MissingInterfaceModule A.ModName
  | MissingPrimitiveName A.Name
  | MissingSelectedNameHash FilePath A.Name
  | InvalidSelectedHeader FilePath TopKey TopInfo
  | MissingSelectedHeader FilePath TopKey
  | MissingSelectedContainer FilePath TopKey
  | MismatchedSelectedContainer FilePath TopKey
  | MissingDeclarationHeader FilePath TopKey
  | InvalidDeclarationHeader FilePath TopKey TopInfo
  | InvalidInterfaceSnapshot A.ModName FilePath
  | ChangedInterfaceSnapshot A.ModName FilePath
  deriving (Eq, Show)

instance Exception SelectiveBackError


-- Selection --------------------------------------------------------------------------------------------

-- | Traverse bodies only for modules that will run a deferred back pass.
-- Every other module stays opaque for syntax materialization. Its persisted
-- semantic summary and shape/slot rows can still route exact runtime interest
-- into a selective provider.
selectFromInterfaces :: InterfaceResolver
                     -> Set.Set A.ModName
                     -> [ReachEdge]
                     -> IO (Either Worklist.SelectionError SelectedProgram)
selectFromInterfaces resolve selectableModules seeds = do
    snapshots <- captureInterfaceClosure resolve selectableModules
    selectFromSnapshots resolve snapshots selectableModules seeds

selectFromSnapshots :: InterfaceResolver
                    -> InterfaceSnapshots
                    -> Set.Set A.ModName
                    -> [ReachEdge]
                    -> IO (Either Worklist.SelectionError SelectedProgram)
selectFromSnapshots resolve snapshots selectableModules seeds = do
    result <- withSnapshotValidation resolve snapshots $ do
      moduleSeeds <- fmap (concatMap reachEdges) $ mapM
        (readModuleSummary snapshots) (Set.toAscList selectableModules)
      Worklist.selectProgramM (lookups snapshots) (seeds ++ moduleSeeds)
    return $ fmap
      (\selection -> SelectedProgram
        (classifyOpaqueDeclarations selection) snapshots)
      result
  where
    readModuleSummary snapshots mn =
      case lookupSnapshot mn snapshots of
        Nothing -> throwIO (MissingInterfaceModule mn)
        Just snapshot ->
          InterfaceFiles.readReachModule (snapshotPath snapshot) mn

    lookups snapshots = Worklist.ProgramLookup
      { Worklist.lookupTopRow = \key@(TopKey mn _) ->
          if Set.member mn selectableModules || mn == Builtin.mBuiltin
            then readOne snapshots InterfaceFiles.readReachTopMaybe key
            else return (Just $ OpaqueTop mempty)
      , Worklist.lookupMemberRow = \owner member ->
          readOne snapshots
            (\path key -> InterfaceFiles.readReachMemberMaybe path key member) owner
      , Worklist.lookupShapeRow = readOne snapshots InterfaceFiles.readReachShapeMaybe
      , Worklist.lookupSlotRow = \owner member ->
          readOne snapshots
            (\path key -> InterfaceFiles.readReachSlotMaybe path key member) owner
      , Worklist.lookupSurfaceSlots = readRequired snapshots
          InterfaceFiles.readReachSlots
      , Worklist.lookupReflectableAttrs =
          readOne snapshots InterfaceFiles.readReachReflectionMaybe
      }

    readOne :: InterfaceSnapshots
            -> (FilePath -> TopKey -> IO (Maybe a))
            -> TopKey
            -> IO (Maybe a)
    readOne snapshots readRow key@(TopKey mn _) =
      case lookupSnapshot mn snapshots of
        Nothing -> return Nothing
        Just snapshot -> readRow (snapshotPath snapshot) key

    readRequired snapshots readRows key@(TopKey mn _) =
      case lookupSnapshot mn snapshots of
        Nothing -> return []
        Just snapshot -> readRows (snapshotPath snapshot) key

    classifyOpaqueDeclarations selection = selection
      { Worklist.selectedDeclarations = localDeclarations
      , Worklist.selectedOpaqueTops =
          Worklist.selectedOpaqueTops selection `Set.union` opaqueDeclarations
      }
      where
        (opaqueDeclarations,localDeclarations) = Set.partition
          (\(TopKey mn _) -> Set.notMember mn selectableModules)
          (Worklist.selectedDeclarations selection)


-- Interface snapshots ----------------------------------------------------------------------------------

-- | Capture and validate every interface that an environment may read through
-- a lazy 'Env.ModuleInfo' callback.  Compile supplies the complete, explicit
-- module set before constructing that environment and retains this opaque set
-- for validation around the back pass.
captureInterfaceSnapshots :: InterfaceResolver
                          -> Set.Set A.ModName
                          -> IO InterfaceSnapshots
captureInterfaceSnapshots resolve modules = do
    snapshots <- InterfaceSnapshots . Map.fromList <$>
      mapM (captureSnapshot resolve)
        (Set.toAscList $ Set.delete Prim.mPrim modules)
    validateInterfaceSnapshots resolve snapshots
    return snapshots

-- | Follow closure imports captured in the same transaction as each module's
-- generation. If an already-read module changes while its descendants are
-- being followed, the final validation rejects the entire closure.
captureInterfaceClosure :: InterfaceResolver
                        -> Set.Set A.ModName
                        -> IO InterfaceSnapshots
captureInterfaceClosure resolve initial = do
    snapshots <- InterfaceSnapshots <$> go Map.empty
      (Set.toAscList $ Set.delete Prim.mPrim initial)
    validateInterfaceSnapshots resolve snapshots
    return snapshots
  where
    go captured [] = return captured
    go captured (mn:pending)
      | mn == Prim.mPrim || Map.member mn captured = go captured pending
      | otherwise = do
          pair@(_,snapshot) <- captureSnapshot resolve mn
          let imports = InterfaceFiles.msClosureImports (snapshotInfo snapshot)
          go (uncurry Map.insert pair captured) (imports ++ pending)

captureSnapshot :: InterfaceResolver
                -> A.ModName
                -> IO (A.ModName, InterfaceSnapshot)
captureSnapshot resolve mn = do
    path <- resolveInterface resolve mn
    current <- InterfaceFiles.readModuleSnapshotMaybe path
    case current of
      Just info | InterfaceFiles.msModuleName info == mn ->
        return (mn,InterfaceSnapshot path info)
      _ -> throwIO (InvalidInterfaceSnapshot mn path)

-- | Fail if any explicitly captured interface has moved, changed generation,
-- or changed hashes since capture.
validateInterfaceSnapshots :: InterfaceResolver -> InterfaceSnapshots -> IO ()
validateInterfaceSnapshots resolve =
    mapM_ validate . Map.toAscList . interfaceSnapshotMap
  where
    validate (mn,snapshot) = do
      paths <- Set.toAscList . Set.fromList <$> resolve mn
      case paths of
        [path] | path == snapshotPath snapshot -> do
          current <- InterfaceFiles.readModuleSnapshotMaybe path
          if current == Just (snapshotInfo snapshot)
            then return ()
            else changed mn snapshot
        _ -> changed mn snapshot

    changed mn snapshot =
      throwIO (ChangedInterfaceSnapshot mn $ snapshotPath snapshot)

-- | Bind a semantic back-pass hash to the public interfaces reachable through
-- its lazy ModuleInfo callbacks. Generations guard one run; public hashes and
-- ordered source imports make the generated-code key change across runs when a
-- callback result or the reconstructed import environment can change.
bindInterfaceSnapshots :: B.ByteString -> InterfaceSnapshots -> B.ByteString
bindInterfaceSnapshots base =
    SHA256.hash . BL.toStrict . Binary.encode . snapshotFacts
  where
    snapshotFacts snapshots =
      ( "selective-interface-snapshots-v2" :: String
      , base
      , [ ( semanticModName mn
          , map semanticModName $ InterfaceFiles.msSourceImports info
          , InterfaceFiles.msPublicHash info
          )
        | (mn,snapshot) <- Map.toAscList (interfaceSnapshotMap snapshots)
        , let info = snapshotInfo snapshot
        ]
      )

-- | Seed exact interest from modules that must emit a whole surface. Their
-- own bodies stay opaque to materialization, but every dependency used by
-- those bodies participates in the selective closure.
snapshotWholeModuleSeeds :: InterfaceResolver
                         -> InterfaceSnapshots
                         -> Set.Set A.ModName
                         -> IO [ReachEdge]
snapshotWholeModuleSeeds resolve snapshots modules =
    withSnapshotValidation resolve snapshots $ fmap concat $ mapM readOne
      (Set.toAscList $ Set.delete Prim.mPrim modules)
  where
    readOne mn = case lookupSnapshot mn snapshots of
      Nothing -> throwIO (MissingInterfaceModule mn)
      Just snapshot -> reachEdges <$>
        InterfaceFiles.readReachWholeModule (snapshotPath snapshot) mn

snapshotRootSeeds :: InterfaceSnapshots
                  -> [(A.ModName,A.Name)]
                  -> IO [ReachEdge]
snapshotRootSeeds snapshots candidates = fmap catMaybes $ mapM rootSeed candidates
  where
    rootSeed (mn,root) = case lookupSnapshot mn snapshots of
      Nothing -> throwIO (MissingInterfaceModule mn)
      Just snapshot -> return $
        if root `elem` InterfaceFiles.msRoots (snapshotInfo snapshot)
          then Just (Construct mn root)
          else Nothing

snapshotNotImplementedModules :: InterfaceSnapshots -> Set.Set A.ModName
snapshotNotImplementedModules = Map.keysSet . Map.filter hasNotImpl . interfaceSnapshotMap
  where
    hasNotImpl = InterfaceFiles.msHasNotImpl . snapshotInfo

snapshotSourceHash :: InterfaceSnapshots -> A.ModName -> IO B.ByteString
snapshotSourceHash snapshots mn = case lookupSnapshot mn snapshots of
    Nothing -> throwIO (MissingInterfaceModule mn)
    Just snapshot -> return $
      InterfaceFiles.msSourceHash (snapshotInfo snapshot)

snapshotImplementationHash :: InterfaceSnapshots
                           -> A.ModName
                           -> IO B.ByteString
snapshotImplementationHash snapshots mn = case lookupSnapshot mn snapshots of
    Nothing -> throwIO (MissingInterfaceModule mn)
    Just snapshot -> return $
      InterfaceFiles.msImplementationHash (snapshotInfo snapshot)

-- | Rebuild the deferred back-pass environment solely from captured,
-- generation-bound interfaces. No memoized lookup from front scheduling is
-- retained. __builtin__ is loaded eagerly through its bound handle so the
-- unqualified builtin environment belongs to the same snapshot as every
-- lazy module callback.
snapshotEnvironment :: InterfaceResolver
                    -> InterfaceSnapshots
                    -> IO Env.Env0
snapshotEnvironment resolve snapshots =
    withSnapshotValidation resolve snapshots $ do
      builtinSnapshot <- case lookupSnapshot Builtin.mBuiltin snapshots of
        Nothing -> throwIO (MissingInterfaceModule Builtin.mBuiltin)
        Just snapshot -> return snapshot
      builtinDB <- openSnapshotDB builtinSnapshot
      (_, I.NModule _ builtinEnv builtinDoc) <-
        InterfaceFiles.readInterfaceDBIface builtinDB
      let base = Env.initEnvFromBuiltin builtinEnv builtinDoc
      foldM install base (Map.toAscList $ interfaceSnapshotMap snapshots)
  where
    install env (mn,snapshot)
      | mn == Prim.mPrim || mn == Builtin.mBuiltin = return env
      | otherwise = do
          db <- openSnapshotDB snapshot
          let info = snapshotInfo snapshot
              moduleInfo = Env.mkTyFileModuleInfo mn
                (InterfaceFiles.msSourceImports info)
                (InterfaceFiles.msDoc info)
                db
          return (Env.addModuleInfo mn moduleInfo env)

    openSnapshotDB snapshot = InterfaceFiles.openInterfaceDBAtGeneration
      (snapshotPath snapshot)
      (InterfaceFiles.msGeneration $ snapshotInfo snapshot)

materializeWholeModule :: InterfaceResolver
                       -> InterfaceSnapshots
                       -> A.ModName
                       -> IO (A.Module,B.ByteString)
materializeWholeModule resolve snapshots mn = do
    snapshot <- case lookupSnapshot mn snapshots of
      Nothing -> throwIO (MissingInterfaceModule mn)
      Just captured -> return captured
    let localSnapshot = InterfaceSnapshots (Map.singleton mn snapshot)
    withSnapshotValidation resolve localSnapshot $ do
      (_imports,_nmod,typed,_sourceMeta,sourceHash,publicHash,implementationHash,
        _hashedImports,_depModules,_nameHashes,_roots,_tests,_doc) <-
          InterfaceFiles.readFile (snapshotPath snapshot)
      let info = snapshotInfo snapshot
          expected =
            ( InterfaceFiles.msSourceHash info
            , InterfaceFiles.msPublicHash info
            , InterfaceFiles.msImplementationHash info
            )
      unless (A.modname typed == mn &&
              (sourceHash,publicHash,implementationHash) == expected) $
        throwIO (ChangedInterfaceSnapshot mn $ snapshotPath snapshot)
      return (typed,implementationHash)

withSnapshotValidation :: InterfaceResolver
                       -> InterfaceSnapshots
                       -> IO a
                       -> IO a
withSnapshotValidation resolve snapshots action = do
    validateInterfaceSnapshots resolve snapshots
    action `finally` validateInterfaceSnapshots resolve snapshots

lookupSnapshot :: A.ModName -> InterfaceSnapshots -> Maybe InterfaceSnapshot
lookupSnapshot mn = Map.lookup mn . interfaceSnapshotMap

unionSnapshots :: InterfaceSnapshots -> InterfaceSnapshots -> InterfaceSnapshots
unionSnapshots left right = InterfaceSnapshots $
    Map.union (interfaceSnapshotMap left) (interfaceSnapshotMap right)

-- Materialization --------------------------------------------------------------------------------------

materializeInterfaceProjections :: InterfaceResolver
                                -> [A.ModName]
                                -> SelectedProgram
                                -> IO [Projection]
materializeInterfaceProjections resolve modules program =
    withSnapshotValidation resolve snapshots $ mapM materialize modules
  where
    snapshots = selectedProgramSnapshots program
    selection = selectedProgramSelection program

    materialize mn =
      case lookupSnapshot mn snapshots of
        Nothing -> throwIO (MissingInterfaceModule mn)
        Just snapshot -> materializeProjection
          (snapshotPath snapshot) mn selection

materializeInterfaceProjection :: InterfaceResolver
                               -> A.ModName
                               -> SelectedProgram
                               -> IO Projection
materializeInterfaceProjection resolve mn program = do
    projections <- materializeInterfaceProjections resolve [mn] program
    case projections of
      [projection] -> return projection
      _ -> error "Internal error: one selective module produced several projections"

-- | Fingerprint opaque declarations, extending the captured closure only for
-- providers reached directly from external seeds. Overlapping modules must
-- still validate at the original generation before the enlarged snapshot is
-- accepted.
captureSelectedOpaqueHashes :: InterfaceResolver
                            -> SelectedProgram
                            -> IO (SelectedProgram,[(TopKey,B.ByteString)])
captureSelectedOpaqueHashes resolve program = do
    opaqueSnapshots <- captureInterfaceClosure resolve missingModules
    let snapshots = selectedProgramSnapshots program `unionSnapshots` opaqueSnapshots
        capturedProgram = program{ selectedProgramSnapshots = snapshots }
    hashes <- withSnapshotValidation resolve snapshots $
      mapM (fingerprint snapshots) opaqueTops
    return (capturedProgram,hashes)
  where
    selection = selectedProgramSelection program
    opaqueTops = Set.toAscList $ Worklist.selectedOpaqueTops selection
    captured = interfaceSnapshotMap $ selectedProgramSnapshots program
    missingModules = Set.fromList
      [ mn
      | TopKey mn _ <- opaqueTops
      , mn /= Prim.mPrim
      , Map.notMember mn captured
      ]

    fingerprint snapshots key@(TopKey mn name)
      | mn == Prim.mPrim = case lookup name Prim.primEnv of
          Nothing -> throwIO (MissingPrimitiveName name)
          Just info -> case Map.lookup name $ Hashing.nameInfoHashes $ Map.singleton name info of
            Nothing -> throwIO (MissingPrimitiveName name)
            Just hash -> return (key,hash)
      | otherwise = do
          tyFile <- case lookupSnapshot mn snapshots of
            Nothing -> throwIO (MissingInterfaceModule mn)
            Just snapshot -> return (snapshotPath snapshot)
          row <- InterfaceFiles.readNameHashMaybe tyFile name
          info <- maybe (throwIO $ MissingSelectedNameHash tyFile name) return row
          return (key,InterfaceFiles.nhPubHash info)

-- | Fail when any interface used to select or materialize this program has
-- been replaced since its snapshot was captured.
validateSelectedProgram :: InterfaceResolver -> SelectedProgram -> IO ()
validateSelectedProgram resolve =
    validateInterfaceSnapshots resolve . selectedProgramSnapshots

resolveInterface :: InterfaceResolver -> A.ModName -> IO FilePath
resolveInterface resolve mn = do
    paths <- Set.toAscList . Set.fromList <$> resolve mn
    case paths of
      [] -> throwIO (MissingInterfaceModule mn)
      [path] -> return path
      _ -> throwIO (AmbiguousInterfaceModule mn paths)

materializeProjection :: FilePath
                      -> A.ModName
                      -> Worklist.Selection
                      -> IO Projection
materializeProjection tyFile mn selection = do
    nameHashes <- mapM readNameHash selectedNames
    typedModule <- InterfaceFiles.readModuleSelection
      tyFile nameHashes (Set.fromList selectedNames) memberInterests
      staticInitializerInterests instanceInitializerInterests
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
      | key@(TopKey moduleName _) <- Set.toAscList (Worklist.selectedTops selection)
      , moduleName == mn
      ]
    selectedNames = [ name | TopKey _ name <- selectedKeys ]
    declarationKeys =
      [ key
      | key@(TopKey moduleName _) <- Set.toAscList (Worklist.selectedDeclarations selection)
      , moduleName == mn
      ]
    memberInterests = Map.fromListWith Set.union
      [ (ownerName, Set.singleton member)
      | (TopKey moduleName ownerName, member) <- Set.toAscList (Worklist.selectedMembers selection)
      , moduleName == mn
      ]
    staticInitializerInterests = Map.fromListWith Set.union
      [ (ownerName, Set.singleton attr)
      | (TopKey moduleName ownerName, attr) <-
          Set.toAscList (Worklist.selectedStaticInitializers selection)
      , moduleName == mn
      ]
    instanceInitializerInterests = Map.fromListWith Set.union
      [ (ownerName, Set.singleton attr)
      | (TopKey moduleName ownerName, attr) <-
          Set.toAscList (Worklist.selectedInstanceInitializers selection)
      , moduleName == mn
      ]

    readNameHash name = do
      row <- InterfaceFiles.readNameHashMaybe tyFile name
      maybe (throwIO $ MissingSelectedNameHash tyFile name) return row

    readSelectedHeader key@(TopKey _ name) = do
      row <- InterfaceFiles.readReachTop tyFile key
      case row of
        LocalTop header _ -> return $ fmap ((,) name) header
        _ -> throwIO (InvalidSelectedHeader tyFile key row)

    readDeclaration key@(TopKey _ name) = do
      row <- InterfaceFiles.readReachTop tyFile key
      case row of
        LocalTop (Just info) _ -> return (name, info)
        LocalTop Nothing _ -> throwIO (MissingDeclarationHeader tyFile key)
        _ -> throwIO (InvalidDeclarationHeader tyFile key row)

-- | The projected module contributes the selected members reconstructed from
-- typed syntax.  Container headers retain the front pass's exact inferred
-- quantifiers, ancestry, actor parameters and extension options.
-- Declaration-only types contribute their compact, memberless headers so
-- selective imports resolve without loading an attribute manifest.
projectionTEnv :: Projection -> I.TEnv
projectionTEnv = projectionTypeEnv

-- | Install the projected name environment while retaining the original
-- exact witness indexes. Forwarding plans query those indexes to rediscover
-- imported witness objects; the selected names then decide which candidates
-- are actually usable.
projectionModuleInfo :: Env.Env0 -> Projection -> Env.ModuleInfo
projectionModuleInfo env projection = projectedInfo
    { Env.moduleWitnessesByProto = Env.moduleWitnessesByProto original
    , Env.moduleWitnessesByType = Env.moduleWitnessesByType original
    }
  where
    typed = projectionModule projection
    mn = A.modname typed
    projected = projectionTEnv projection
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
projectionUniverseHash :: Worklist.Selection
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
            , Hashing.moduleProjectionHash typed (projectionTEnv projection)
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
selectionProjectionFacts :: Worklist.Selection
                         -> [(Int,A.ModName,A.Name,Int,Maybe A.Name)]
selectionProjectionFacts selection =
    map (topFact 0) (Worklist.manifestDeclarations manifest) ++
    map (topFact 1) (Worklist.manifestTops manifest) ++
    map (topFact 2) (Worklist.manifestOpaqueTops manifest) ++
    map (memberFact 3) (Worklist.manifestMembers manifest) ++
    map (nameFact 4) (Worklist.manifestAttrs manifest) ++
    map (nameFact 5) (Worklist.manifestStaticInitializers manifest) ++
    map (nameFact 6) (Worklist.manifestInstanceInitializers manifest) ++
    map (generatedFact 7) (Worklist.manifestGenerated manifest) ++
    map (topFact 8) (Worklist.manifestConstructed manifest) ++
    map (topFact 9) (Worklist.manifestInitialized manifest)
  where
    manifest = Worklist.selectionManifest selection

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
      Rows.Method name -> (0,Just $ semanticName name)
      Rows.Attr name   -> (1,Just $ semanticName name)
      Rows.InitRest    -> (2,Nothing)

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
