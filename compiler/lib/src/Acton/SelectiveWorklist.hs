-- SPDX-License-Identifier: BSD-3-Clause

-- | Pure whole-program closure for selective back passes.
--
-- The front pass supplies exact, canonical reach summaries and already-computed
-- shape metadata.  Storage is exposed through exact-key callbacks, so closure
-- never enumerates a module or widens a missing lookup.  This module does not
-- materialize syntax, compute output hashes, or run a back pass.
module Acton.SelectiveWorklist
  ( TopKey(..)
  , TopInfo(..)
  , ShapeKind(..)
  , ConstructorDecl(..)
  , SlotDecl(..)
  , MemberInfo(..)
  , ShapeInfo(..)
  , SlotInfo(..)
  , ReflectableAttrs(..)
  , ProgramIndex(..)
  , ProgramLookup(..)
  , emptyProgramIndex
  , Selection(..)
  , SelectionManifest(..)
  , SelectionError(..)
  , selectProgram
  , selectProgramM
  , selectionManifest
  ) where

import Control.Monad (foldM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, except, runExceptT, throwE)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, modify')
import Data.Functor.Identity (runIdentity)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import Acton.Builtin (initKW)
import qualified Acton.InterfaceRows as Rows
import Acton.ReachabilityRows
import Acton.ReachabilityTypes (MemberRef(..), ReachEdge(..), ReachSummary, reachEdges)
import qualified Acton.Syntax as A


-- Program index -----------------------------------------------------------------------------------------

-- | A pure view of all rows that can participate in one selection universe.
-- Opaque dependencies should have explicit OpaqueTop/ShapeInfo entries.  A
-- missing reachable entry is therefore corruption or an incomplete index.
data ProgramIndex = ProgramIndex
  { programTops             :: Map.Map TopKey TopInfo
  , programMembers          :: Map.Map (TopKey, Rows.MemberKey) MemberInfo
  , programShapes           :: Map.Map TopKey ShapeInfo
  , programSlots            :: Map.Map (TopKey, MemberRef) SlotInfo
  , programReflectableAttrs :: Map.Map TopKey ReflectableAttrs
  } deriving (Eq, Show)

emptyProgramIndex :: ProgramIndex
emptyProgramIndex = ProgramIndex
  Map.empty Map.empty Map.empty Map.empty Map.empty

-- | On-demand access to the selection universe.  The selector memoizes each
-- exact-key result for the duration of one run.
data ProgramLookup m = ProgramLookup
  { lookupTopRow           :: TopKey -> m (Maybe TopInfo)
  , lookupMemberRow        :: TopKey -> Rows.MemberKey -> m (Maybe MemberInfo)
  , lookupShapeRow         :: TopKey -> m (Maybe ShapeInfo)
  , lookupSlotRow          :: TopKey -> MemberRef -> m (Maybe SlotInfo)
  , lookupSurfaceSlots     :: TopKey -> m [(MemberRef, SlotInfo)]
  , lookupReflectableAttrs :: TopKey -> m (Maybe ReflectableAttrs)
  }


-- Result ------------------------------------------------------------------------------------------------

data Selection = Selection
  { selectedDeclarations :: Set.Set TopKey
  , selectedTops         :: Set.Set TopKey
  , selectedOpaqueTops   :: Set.Set TopKey
  , selectedMembers      :: Set.Set (TopKey, Rows.MemberKey)
  , selectedAttrs        :: Set.Set (TopKey, A.Name)
  , selectedStaticInitializers :: Set.Set (TopKey, A.Name)
  , selectedInstanceInitializers :: Set.Set (TopKey, A.Name)
  , selectedGenerated    :: Set.Set (TopKey, MemberRef)
  , selectedConstructed :: Set.Set TopKey
  , selectedInitialized :: Set.Set TopKey
  } deriving (Eq, Show)

-- | Canonically ordered selection facts suitable as the semantic part of a
-- code-generation manifest.  Hashing remains a backend concern: it combines
-- this manifest with the selected rows' and projected shapes' hashes.
data SelectionManifest = SelectionManifest
  { manifestDeclarations :: [TopKey]
  , manifestTops         :: [TopKey]
  , manifestOpaqueTops   :: [TopKey]
  , manifestMembers      :: [(TopKey, Rows.MemberKey)]
  , manifestAttrs        :: [(TopKey, A.Name)]
  , manifestStaticInitializers :: [(TopKey, A.Name)]
  , manifestInstanceInitializers :: [(TopKey, A.Name)]
  , manifestGenerated    :: [(TopKey, MemberRef)]
  , manifestConstructed  :: [TopKey]
  , manifestInitialized  :: [TopKey]
  } deriving (Eq, Show)

selectionManifest :: Selection -> SelectionManifest
selectionManifest s = SelectionManifest
  { manifestDeclarations = Set.toAscList (selectedDeclarations s)
  , manifestTops = Set.toAscList (selectedTops s)
  , manifestOpaqueTops = Set.toAscList (selectedOpaqueTops s)
  , manifestMembers = Set.toAscList (selectedMembers s)
  , manifestAttrs = Set.toAscList (selectedAttrs s)
  , manifestStaticInitializers = Set.toAscList (selectedStaticInitializers s)
  , manifestInstanceInitializers = Set.toAscList (selectedInstanceInitializers s)
  , manifestGenerated = Set.toAscList (selectedGenerated s)
  , manifestConstructed = Set.toAscList (selectedConstructed s)
  , manifestInitialized = Set.toAscList (selectedInitialized s)
  }


-- Errors ------------------------------------------------------------------------------------------------

data SelectionError
  = MissingTop TopKey
  | MissingShape TopKey
  | InvalidShapeName TopKey TopKey
  | InvalidLineage TopKey [TopKey]
  | MissingMemberSummary TopKey Rows.MemberKey
  | MissingSlot TopKey MemberRef
  | MissingReflectableAttrs TopKey
  | InvalidStoredSlot TopKey MemberRef Rows.MemberKey
  | InvalidSlotKind TopKey MemberRef SlotDecl
  | AbstractMemberSelected TopKey MemberRef TopKey
  | AbstractClassConstructed TopKey [MemberRef]
  | ProtocolConstructed TopKey
  | MissingConstructor TopKey
  | DynamicSerializationRequiresWhole
  deriving (Eq, Show)


-- Worklist ----------------------------------------------------------------------------------------------

data LookupCache = LookupCache
  { cachedTops             :: Map.Map TopKey (Maybe TopInfo)
  , cachedMembers          :: Map.Map (TopKey, Rows.MemberKey) (Maybe MemberInfo)
  , cachedShapes           :: Map.Map TopKey (Maybe ShapeInfo)
  , cachedSlots            :: Map.Map (TopKey, MemberRef) (Maybe SlotInfo)
  , cachedSurfaceSlots     :: Map.Map TopKey [(MemberRef, SlotInfo)]
  , cachedReflectableAttrs :: Map.Map TopKey (Maybe ReflectableAttrs)
  }

emptyLookupCache :: LookupCache
emptyLookupCache = LookupCache
  Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty

type SelectM m = ExceptT SelectionError (StateT LookupCache m)

liftLookup :: Monad m => m a -> SelectM m a
liftLookup = lift . lift

loadTopRow :: Monad m => ProgramLookup m -> TopKey -> SelectM m (Maybe TopInfo)
loadTopRow lookups key = do
  cache <- lift get
  case Map.lookup key (cachedTops cache) of
    Just row -> return row
    Nothing -> do
      row <- liftLookup (lookupTopRow lookups key)
      lift $ modify' $ \c -> c{ cachedTops = Map.insert key row (cachedTops c) }
      return row

loadMemberRow :: Monad m => ProgramLookup m -> TopKey -> Rows.MemberKey -> SelectM m (Maybe MemberInfo)
loadMemberRow lookups owner member = do
  cache <- lift get
  case Map.lookup key (cachedMembers cache) of
    Just row -> return row
    Nothing -> do
      row <- liftLookup (lookupMemberRow lookups owner member)
      lift $ modify' $ \c -> c{ cachedMembers = Map.insert key row (cachedMembers c) }
      return row
  where key = (owner, member)

loadShapeRow :: Monad m => ProgramLookup m -> TopKey -> SelectM m (Maybe ShapeInfo)
loadShapeRow lookups key = do
  cache <- lift get
  case Map.lookup key (cachedShapes cache) of
    Just row -> return row
    Nothing -> do
      row <- liftLookup (lookupShapeRow lookups key)
      lift $ modify' $ \c -> c{ cachedShapes = Map.insert key row (cachedShapes c) }
      return row

loadSlotRow :: Monad m => ProgramLookup m -> TopKey -> MemberRef -> SelectM m (Maybe SlotInfo)
loadSlotRow lookups receiver ref = do
  cache <- lift get
  case Map.lookup key (cachedSlots cache) of
    Just row -> return row
    Nothing -> do
      row <- liftLookup (lookupSlotRow lookups receiver ref)
      lift $ modify' $ \c -> c{ cachedSlots = Map.insert key row (cachedSlots c) }
      return row
  where key = (receiver, ref)

loadSurfaceSlots :: Monad m => ProgramLookup m -> TopKey -> SelectM m [(MemberRef, SlotInfo)]
loadSurfaceSlots lookups receiver = do
  cache <- lift get
  case Map.lookup receiver (cachedSurfaceSlots cache) of
    Just slots -> return slots
    Nothing -> do
      slots <- liftLookup (lookupSurfaceSlots lookups receiver)
      lift $ modify' $ \c -> c
        { cachedSurfaceSlots = Map.insert receiver slots (cachedSurfaceSlots c) }
      return slots

loadReflectableAttrs :: Monad m => ProgramLookup m -> TopKey -> SelectM m (Maybe ReflectableAttrs)
loadReflectableAttrs lookups receiver = do
  cache <- lift get
  case Map.lookup receiver (cachedReflectableAttrs cache) of
    Just attrs -> return attrs
    Nothing -> do
      attrs <- liftLookup (lookupReflectableAttrs lookups receiver)
      lift $ modify' $ \c -> c
        { cachedReflectableAttrs = Map.insert receiver attrs (cachedReflectableAttrs c) }
      return attrs

data WorkItem
  = ReachWork ReachEdge
  | InitializeWork TopKey
  | ReflectWork TopKey TopKey
  deriving (Eq, Ord, Show)

data Work = Work
  { workQueue          :: Seq.Seq WorkItem
  , workNeeded         :: Set.Set TopKey
  , workDeclarations   :: Set.Set TopKey
  , workTops           :: Set.Set TopKey
  , workOpaqueTops     :: Set.Set TopKey
  , workMembers        :: Set.Set (TopKey, Rows.MemberKey)
  , workAttrs          :: Set.Set (TopKey, A.Name)
  , workStaticInitializers :: Set.Set (TopKey, A.Name)
  , workInstanceInitializers :: Set.Set (TopKey, A.Name)
  , workGenerated      :: Set.Set (TopKey, MemberRef)
  , workConstructed    :: Set.Set TopKey
  , workInitialized    :: Set.Set TopKey
  , workDispatches     :: Set.Set (TopKey, MemberRef)
  , workReflections    :: Set.Set TopKey
  , workDispatchPairs  :: Set.Set (TopKey, MemberRef, TopKey)
  , workReflectionPairs :: Set.Set (TopKey, TopKey)
  }

emptyWork :: [ReachEdge] -> Work
emptyWork seeds = Work
  { workQueue = Seq.fromList (map ReachWork seeds)
  , workNeeded = Set.empty
  , workDeclarations = Set.empty
  , workTops = Set.empty
  , workOpaqueTops = Set.empty
  , workMembers = Set.empty
  , workAttrs = Set.empty
  , workStaticInitializers = Set.empty
  , workInstanceInitializers = Set.empty
  , workGenerated = Set.empty
  , workConstructed = Set.empty
  , workInitialized = Set.empty
  , workDispatches = Set.empty
  , workReflections = Set.empty
  , workDispatchPairs = Set.empty
  , workReflectionPairs = Set.empty
  }

finish :: Work -> Selection
finish w = Selection
  { selectedDeclarations = workDeclarations w
  , selectedTops = workTops w
  , selectedOpaqueTops = workOpaqueTops w
  , selectedMembers = workMembers w
  , selectedAttrs = workAttrs w
  , selectedStaticInitializers = workStaticInitializers w
  , selectedInstanceInitializers = workInstanceInitializers w
  , selectedGenerated = workGenerated w
  , selectedConstructed = workConstructed w
  , selectedInitialized = workInitialized w
  }

selectProgram :: ProgramIndex -> [ReachEdge] -> Either SelectionError Selection
selectProgram index seeds = runIdentity (selectProgramM (indexLookup index) seeds)

selectProgramM :: Monad m => ProgramLookup m -> [ReachEdge] -> m (Either SelectionError Selection)
selectProgramM lookups seeds =
  evalStateT (runExceptT $ finish <$> drain lookups (emptyWork seeds)) emptyLookupCache

indexLookup :: Applicative m => ProgramIndex -> ProgramLookup m
indexLookup index = ProgramLookup
  { lookupTopRow = \key -> pure $ Map.lookup key (programTops index)
  , lookupMemberRow = \owner member ->
      pure $ Map.lookup (owner, member) (programMembers index)
  , lookupShapeRow = \key -> pure $ Map.lookup key (programShapes index)
  , lookupSlotRow = \receiver ref ->
      pure $ Map.lookup (receiver, ref) (programSlots index)
  , lookupSurfaceSlots = \receiver -> pure
      [ (ref,slot)
      | ((owner,ref),slot) <- Map.toAscList (programSlots index)
      , owner == receiver
      ]
  , lookupReflectableAttrs = \receiver ->
      pure $ Map.lookup receiver (programReflectableAttrs index)
  }

drain :: Monad m => ProgramLookup m -> Work -> SelectM m Work
drain lookups work =
  case Seq.viewl (workQueue work) of
    Seq.EmptyL -> return work
    item Seq.:< rest -> do
      work' <- process lookups item work{ workQueue = rest }
      drain lookups work'

process :: Monad m => ProgramLookup m -> WorkItem -> Work -> SelectM m Work
process lookups item work =
  case item of
    ReachWork edge -> processEdge lookups edge work
    InitializeWork receiver -> initializeReceiver lookups receiver work
    ReflectWork receiver concrete -> reflectConcrete lookups receiver concrete work

processEdge :: Monad m => ProgramLookup m -> ReachEdge -> Work -> SelectM m Work
processEdge lookups edge work =
  case edge of
    Declare mn n -> return (declareTop (TopKey mn n) work)
    Need mn n -> selectTop lookups (TopKey mn n) work
    Inherit mn n -> selectTop lookups (TopKey mn n) work
    Construct mn n -> constructShape lookups (TopKey mn n) work
    Direct mn n ref -> directMember lookups (TopKey mn n) ref work
    Dispatch mn n ref -> dispatchMember lookups (TopKey mn n) ref work
    Reflect mn n -> reflectShape lookups (TopKey mn n) work
    DynamicSerialization -> throwE DynamicSerializationRequiresWhole
    DeclareAttr mn n attr -> declareAttribute lookups (TopKey mn n) attr work


-- Top-level and shape lookups -----------------------------------------------------------------------------

lookupTop :: Monad m => ProgramLookup m -> TopKey -> SelectM m TopInfo
lookupTop lookups key = do
  mTop <- loadTopRow lookups key
  case mTop of
    Nothing -> throwE (MissingTop key)
    Just top -> return top

lookupShape :: Monad m => ProgramLookup m -> TopKey -> SelectM m ShapeInfo
lookupShape lookups key = do
  mShape <- loadShapeRow lookups key
  shape <- case mShape of
    Nothing -> throwE (MissingShape key)
    Just found -> return found
  except (validateShape key shape)

validateShape :: TopKey -> ShapeInfo -> Either SelectionError ShapeInfo
validateShape key shape
  | shapeName shape /= key = Left (InvalidShapeName key (shapeName shape))
  | null lineage || head lineage /= key || length lineage /= Set.size (Set.fromList lineage) =
      Left (InvalidLineage key lineage)
  | otherwise = Right shape
  where lineage = shapeLineage shape

selectTop :: Monad m => ProgramLookup m -> TopKey -> Work -> SelectM m Work
selectTop lookups key work
  | Set.member key (workNeeded work) = return work
  | otherwise = do
      top <- lookupTop lookups key
      let work0 = work
            { workNeeded = Set.insert key (workNeeded work)
            , workDeclarations = Set.delete key (workDeclarations work)
            }
      case top of
        OpaqueTop summary -> return $ enqueueSummary summary work0
          { workOpaqueTops = Set.insert key (workOpaqueTops work0) }
        LocalTop header summary -> do
          let work1 = enqueueSummary summary work0
                { workTops = Set.insert key (workTops work0) }
          case header of
            Nothing -> return work1
            Just _ -> do
              shape <- lookupShape lookups key
              case shapeLineage shape of
                _ : inherited -> return $ foldl (flip enqueueNeed) work1 inherited
                []            -> throwE (InvalidLineage key [])

enqueueNeed :: TopKey -> Work -> Work
enqueueNeed (TopKey mn n) = enqueueReach (Need mn n)

declareTop :: TopKey -> Work -> Work
declareTop key work
  | Set.member key (workNeeded work) = work
  | otherwise = work
      { workDeclarations = Set.insert key (workDeclarations work) }


-- Members ------------------------------------------------------------------------------------------------

lookupMemberInfo :: Monad m => ProgramLookup m -> TopKey -> Rows.MemberKey -> SelectM m MemberInfo
lookupMemberInfo lookups owner member = do
  mInfo <- loadMemberRow lookups owner member
  case mInfo of
    Nothing -> throwE (MissingMemberSummary owner member)
    Just info -> return info

lookupMemberInfoMaybe :: Monad m => ProgramLookup m -> TopKey -> Rows.MemberKey -> SelectM m (Maybe MemberInfo)
lookupMemberInfoMaybe = loadMemberRow

selectMember :: Monad m => ProgramLookup m -> TopKey -> Rows.MemberKey -> Work -> SelectM m Work
selectMember lookups owner member work
  | Set.member key (workMembers work) = return work
  | otherwise = do
      work0 <- selectTop lookups owner work
      info <- lookupMemberInfo lookups owner member
      return $ enqueueSummary (memberSummary info) work0
        { workMembers = Set.insert key (workMembers work0) }
  where key = (owner, member)

-- Retain an instance-property declaration for layout without retaining its
-- prunable constructor-prefix initialization. This is distinct from reading
-- the attribute, which enters through Direct and selects both obligations.
declareAttribute :: Monad m
                 => ProgramLookup m
                 -> TopKey
                 -> A.Name
                 -> Work
                 -> SelectM m Work
declareAttribute lookups receiver name work = do
  let ref = AttrRef name
  work0 <- selectTop lookups receiver work
  (owner, slot) <- resolveSlot lookups receiver ref
  case slot of
    AttributeSlot -> selectMember lookups owner (Rows.Attr name) work0
    _ -> throwE (InvalidSlotKind owner ref slot)

directMember :: Monad m => ProgramLookup m -> TopKey -> MemberRef -> Work -> SelectM m Work
directMember lookups receiver ref work = do
  work0 <- selectTop lookups receiver work
  case ref of
    MethodRef n | n == initKW ->
      return (enqueue (InitializeWork receiver) work0)
    _ -> do
      (owner, slot) <- resolveSlot lookups receiver ref
      selectSlot lookups receiver owner ref slot work0

selectSlot :: Monad m => ProgramLookup m -> TopKey -> TopKey -> MemberRef -> SlotDecl -> Work -> SelectM m Work
selectSlot lookups receiver owner ref slot work = do
  top <- lookupTop lookups owner
  work0 <- selectTop lookups owner work
  case (top, slot) of
    (_, AbstractSlot) -> throwE (AbstractMemberSelected receiver ref owner)
    (OpaqueTop{}, AttributeSlot) -> case ref of
      AttrRef n -> selectReceiverInitializer lookups receiver n work0
      _ -> throwE (InvalidSlotKind owner ref slot)
    (OpaqueTop{}, OpaqueSlot) -> case ref of
      AttrRef n -> selectReceiverInitializer lookups receiver n work0
      _ -> return work0
    (OpaqueTop{}, _) -> return work0
    (_, StoredSlot member) -> do
      except (validateStoredSlot owner ref member)
      selectMember lookups owner member work0
    (_, AttributeSlot) ->
      case ref of
        AttrRef n -> selectAttributeSlot lookups receiver owner n work0
        _ -> throwE (InvalidSlotKind owner ref slot)
    (_, GeneratedSlot summary) -> selectGenerated owner ref summary work0
    (_, OpaqueSlot) -> return work0

validateStoredSlot :: TopKey -> MemberRef -> Rows.MemberKey -> Either SelectionError ()
validateStoredSlot owner (MethodRef n) member@(Rows.Method n')
  | n == n' = Right ()
  | otherwise = Left (InvalidStoredSlot owner (MethodRef n) member)
validateStoredSlot owner ref member = Left (InvalidStoredSlot owner ref member)

selectGenerated :: Monad m => TopKey -> MemberRef -> ReachSummary -> Work -> SelectM m Work
selectGenerated owner ref summary work
  | Set.member key (workGenerated work) = return work
  | otherwise = return $ enqueueSummary summary work
      { workGenerated = Set.insert key (workGenerated work) }
  where key = (owner, ref)


-- Exact provider resolution -----------------------------------------------------------------------------

resolveSlot :: Monad m => ProgramLookup m -> TopKey -> MemberRef -> SelectM m (TopKey, SlotDecl)
resolveSlot lookups receiver ref = do
  mSlot <- loadSlotRow lookups receiver ref
  case mSlot of
    Nothing -> throwE (MissingSlot receiver ref)
    Just (SlotInfo owner slot) -> do
      except (validateSlot owner ref slot)
      return (owner, slot)

validateSlot :: TopKey -> MemberRef -> SlotDecl -> Either SelectionError ()
validateSlot owner ref (StoredSlot member) = validateStoredSlot owner ref member
validateSlot _ (AttrRef _) AttributeSlot = Right ()
validateSlot _ _ AbstractSlot = Right ()
validateSlot _ (MethodRef _) (GeneratedSlot _) = Right ()
validateSlot _ _ OpaqueSlot = Right ()
validateSlot owner ref slot = Left (InvalidSlotKind owner ref slot)

lookupReflectableRefs :: Monad m => ProgramLookup m -> TopKey -> SelectM m [MemberRef]
lookupReflectableRefs lookups receiver = do
  shape <- lookupShape lookups receiver
  names <- foldM load Set.empty (shapeLineage shape)
  return (map AttrRef $ Set.toAscList names)
  where
    load names owner = do
      mAttrs <- loadReflectableAttrs lookups owner
      attrs <- case mAttrs of
        Nothing -> throwE (MissingReflectableAttrs owner)
        Just found -> return (reflectableAttrs found)
      return (Set.union names $ Set.fromList attrs)

compatible :: Monad m => ProgramLookup m -> TopKey -> TopKey -> SelectM m Bool
compatible lookups receiver concrete = do
  shape <- lookupShape lookups concrete
  return (receiver `elem` shapeLineage shape)


-- Construction and initialization ----------------------------------------------------------------------

constructShape :: Monad m => ProgramLookup m -> TopKey -> Work -> SelectM m Work
constructShape lookups concrete work = do
  work0 <- selectTop lookups concrete work
  shape <- lookupShape lookups concrete
  case shapeKind shape of
    ProtocolShape -> throwE (ProtocolConstructed concrete)
    _ -> return ()
  let abstracts = shapeAbstracts shape
  if not (null abstracts)
    then throwE (AbstractClassConstructed concrete abstracts)
    else if Set.member concrete (workConstructed work0)
      then return work0
      else do
        let work1 = enqueue (InitializeWork concrete) work0
              { workConstructed = Set.insert concrete (workConstructed work0) }
        work2 <- retainOpaqueBarrierAttrs shape work1
        work3 <- retainWitnessSlots shape work2
        work4 <- foldM (replayDispatch lookups concrete) work3
          (Set.toAscList $ workDispatches work3)
        foldM (replayReflection lookups concrete) work4
          (Set.toAscList $ workReflections work4)
  where
    retainOpaqueBarrierAttrs shape selected = do
      lineage <- mapM classify (shapeLineage shape)
      if not (hasOpaqueBarrier lineage)
        then return selected
        else do
          slots <- loadSurfaceSlots lookups concrete
          foldM (retainBarrierAttr lineage) selected slots

    classify key = do
      top <- lookupTop lookups key
      return (key,top)

    retainBarrierAttr lineage selected (ref@(AttrRef _),SlotInfo provider slot)
      | crossesOpaque lineage provider = do
          providerTop <- lookupTop lookups provider
          case providerTop of
            LocalTop{} -> selectSlot lookups concrete provider ref slot selected
            OpaqueTop{} -> return selected
    retainBarrierAttr _ selected _ = return selected

    crossesOpaque lineage provider = case break ((== provider) . fst) lineage of
      (prefix,_:_) -> any (opaque . snd) prefix
      _            -> False

    hasOpaqueBarrier [] = False
    hasOpaqueBarrier ((_,top):rest) =
      opaque top && any (not . opaque . snd) rest || hasOpaqueBarrier rest

    opaque OpaqueTop{} = True
    opaque LocalTop{}  = False

    retainWitnessSlots shape selected
      | shapeKind shape /= WitnessShape = return selected
      | otherwise = do
          slots <- loadSurfaceSlots lookups concrete
          foldM retain selected slots

    retain selected (_,SlotInfo _ AbstractSlot) = return selected
    retain selected (ref,_) = directMember lookups concrete ref selected

resolveConstructor :: Monad m => ProgramLookup m -> TopKey -> SelectM m (TopKey, ConstructorDecl)
resolveConstructor lookups receiver = do
  shape <- lookupShape lookups receiver
  case shapeConstructor shape of
    Nothing -> throwE (MissingConstructor receiver)
    Just constructor -> return constructor

initializeReceiver :: Monad m => ProgramLookup m -> TopKey -> Work -> SelectM m Work
initializeReceiver lookups receiver work = do
  work0 <- selectTop lookups receiver work
  (provider, constructor) <- resolveConstructor lookups receiver
  activateConstructor lookups receiver provider constructor work0

activateConstructor :: Monad m
                    => ProgramLookup m
                    -> TopKey
                    -> TopKey
                    -> ConstructorDecl
                    -> Work
                    -> SelectM m Work
activateConstructor lookups receiver provider constructor work
  | Set.member receiver (workInitialized work) = return work
  | otherwise = do
      work0 <- selectTop lookups receiver work
      let work1 = work0{ workInitialized = Set.insert receiver (workInitialized work0) }
      providerTop <- lookupTop lookups provider
      work2 <- case constructor of
        StoredConstructor summary -> do
          let withSummary = enqueueSummary summary work1
          case providerTop of
            LocalTop{} -> selectMember lookups provider Rows.InitRest withSummary
            OpaqueTop{} -> return withSummary
        GeneratedConstructor summary -> case providerTop of
          LocalTop{} -> selectGenerated provider (MethodRef initKW) summary work1
          OpaqueTop{} -> return (enqueueSummary summary work1)
        InheritedConstructor summary ->
          initializeReceiver lookups provider (enqueueSummary summary work1)
        OpaqueConstructor -> selectTop lookups provider work1
      foldM (selectInitForField lookups receiver) work2 (Set.toAscList $ workAttrs work2)

selectAttr :: Monad m => ProgramLookup m -> TopKey -> A.Name -> Work -> SelectM m Work
selectAttr lookups owner attr work
  | Set.member field (workAttrs work) = return work
  | otherwise = do
      work0 <- selectTop lookups owner work
      info <- lookupMemberInfo lookups owner (Rows.Attr attr)
      work1 <- selectMember lookups owner (Rows.Attr attr) work0
      let work2 = work1{ workAttrs = Set.insert field (workAttrs work1) }
      work3 <- activateStaticInitializer lookups owner attr info work2
      foldM selectInit work3
        [ (initOwner, field) | initOwner <- Set.toAscList (workInitialized work2) ]
  where field = (owner, attr)
        selectInit w (initOwner, demanded) =
          selectInitForField lookups initOwner w demanded

selectAttributeSlot :: Monad m
                    => ProgramLookup m
                    -> TopKey
                    -> TopKey
                    -> A.Name
                    -> Work
                    -> SelectM m Work
selectAttributeSlot lookups receiver owner name work = do
  selected <- selectAttr lookups owner name work
  selectInitForField lookups receiver selected (owner,name)

selectReceiverInitializer :: Monad m
                          => ProgramLookup m
                          -> TopKey
                          -> A.Name
                          -> Work
                          -> SelectM m Work
selectReceiverInitializer lookups receiver name work = do
  top <- lookupTop lookups receiver
  case top of
    OpaqueTop{} -> return work
    LocalTop{} -> do
      mInfo <- lookupMemberInfoMaybe lookups receiver (Rows.Attr name)
      case mInfo of
        Nothing -> return work
        Just info -> do
          selected <- selectAttr lookups receiver name work
          activateInstanceInitializer lookups receiver name info selected

selectInitForField :: Monad m => ProgramLookup m -> TopKey -> Work -> (TopKey, A.Name) -> SelectM m Work
selectInitForField lookups initOwner work field@(fieldOwner, attr) = do
  applies <- compatible lookups fieldOwner initOwner
  if not applies
    then return work
    else do
      mInfo <- lookupMemberInfoMaybe lookups initOwner (Rows.Attr attr)
      case mInfo of
        Nothing -> return work
        Just info -> do
          (provider, slot) <- resolveSlot lookups initOwner (AttrRef attr)
          case slot of
            AttributeSlot | (provider, attr) == field ->
              activateInstanceInitializer lookups initOwner attr info work
            OpaqueSlot | (provider, attr) == field -> return work
            _ -> return work

activateStaticInitializer :: Monad m => ProgramLookup m -> TopKey -> A.Name -> MemberInfo -> Work -> SelectM m Work
activateStaticInitializer lookups owner attr info work =
  case memberStaticInitSummary info of
    Nothing -> return work
    Just summary
      | Set.member key (workStaticInitializers work) -> return work
      | otherwise -> do
          work0 <- selectMember lookups owner (Rows.Attr attr) work
          return $ enqueueSummary summary work0
            { workStaticInitializers = Set.insert key (workStaticInitializers work0) }
  where key = (owner, attr)

activateInstanceInitializer :: Monad m => ProgramLookup m -> TopKey -> A.Name -> MemberInfo -> Work -> SelectM m Work
activateInstanceInitializer lookups owner attr info work =
  case memberInstanceInitSummary info of
    Nothing -> return work
    Just summary
      | Set.member key (workInstanceInitializers work) -> return work
      | otherwise -> do
          work0 <- selectMember lookups owner (Rows.Attr attr) work
          return $ enqueueSummary summary work0
            { workInstanceInitializers = Set.insert key (workInstanceInitializers work0) }
  where key = (owner, attr)


-- Dynamic dispatch and reflection -----------------------------------------------------------------------

dispatchMember :: Monad m => ProgramLookup m -> TopKey -> MemberRef -> Work -> SelectM m Work
dispatchMember lookups receiver ref work = do
  work0 <- selectTop lookups receiver work
  work1 <- retainReceiverDeclaration lookups receiver ref work0
  let work2 = work1{ workDispatches = Set.insert (receiver, ref) (workDispatches work1) }
  foldM (dispatchToConstructed lookups receiver ref) work2 (Set.toAscList $ workConstructed work2)

-- Attribute declarations and their exact receiver initializer are required
-- even when no constructor is visible in the closed-world runtime set: an
-- opaque call can still return an instance of the static receiver. Overrides
-- remain tied to the concrete receivers replayed below.
retainReceiverDeclaration :: Monad m
                          => ProgramLookup m
                          -> TopKey
                          -> MemberRef
                          -> Work
                          -> SelectM m Work
retainReceiverDeclaration _ _ MethodRef{} work = return work
retainReceiverDeclaration lookups receiver ref@(AttrRef _) work = do
  (provider, slot) <- resolveSlot lookups receiver ref
  case slot of
    AttributeSlot -> selectSlot lookups receiver provider ref slot work
    OpaqueSlot -> selectSlot lookups receiver provider ref slot work
    _ -> return work

dispatchToConstructed :: Monad m => ProgramLookup m -> TopKey -> MemberRef -> Work -> TopKey -> SelectM m Work
dispatchToConstructed lookups receiver ref work concrete = do
  applies <- compatible lookups receiver concrete
  if applies then enqueueDispatchPair receiver ref concrete work else return work

replayDispatch :: Monad m => ProgramLookup m -> TopKey -> Work -> (TopKey, MemberRef) -> SelectM m Work
replayDispatch lookups concrete work (receiver, ref) =
  dispatchToConstructed lookups receiver ref work concrete

enqueueDispatchPair :: Monad m => TopKey -> MemberRef -> TopKey -> Work -> SelectM m Work
enqueueDispatchPair receiver ref concrete work
  | Set.member key (workDispatchPairs work) = return work
  | otherwise = return $ enqueueReach (edgeDirect concrete ref) work
      { workDispatchPairs = Set.insert key (workDispatchPairs work) }
  where key = (receiver, ref, concrete)

reflectShape :: Monad m => ProgramLookup m -> TopKey -> Work -> SelectM m Work
reflectShape lookups receiver work = do
  work0 <- selectTop lookups receiver work
  let work1 = work0{ workReflections = Set.insert receiver (workReflections work0) }
  foldM (reflectConstructed lookups receiver) work1 (Set.toAscList $ workConstructed work1)

reflectConstructed :: Monad m => ProgramLookup m -> TopKey -> Work -> TopKey -> SelectM m Work
reflectConstructed lookups receiver work concrete = do
  applies <- compatible lookups receiver concrete
  if applies then enqueueReflectionPair receiver concrete work else return work

replayReflection :: Monad m => ProgramLookup m -> TopKey -> Work -> TopKey -> SelectM m Work
replayReflection lookups concrete work receiver = reflectConstructed lookups receiver work concrete

enqueueReflectionPair :: Monad m => TopKey -> TopKey -> Work -> SelectM m Work
enqueueReflectionPair receiver concrete work
  | Set.member key (workReflectionPairs work) = return work
  | otherwise = return $ enqueue (ReflectWork receiver concrete) work
      { workReflectionPairs = Set.insert key (workReflectionPairs work) }
  where key = (receiver, concrete)

reflectConcrete :: Monad m => ProgramLookup m -> TopKey -> TopKey -> Work -> SelectM m Work
reflectConcrete lookups receiver concrete work = do
  applies <- compatible lookups receiver concrete
  if not applies
    then return work
    else do
      attrs <- lookupReflectableRefs lookups concrete
      return $ foldl (flip $ enqueueReach . edgeDirect concrete) work attrs


-- Queue helpers -----------------------------------------------------------------------------------------

enqueue :: WorkItem -> Work -> Work
enqueue item work = work{ workQueue = workQueue work Seq.|> item }

enqueueReach :: ReachEdge -> Work -> Work
enqueueReach = enqueue . ReachWork

enqueueSummary :: ReachSummary -> Work -> Work
enqueueSummary summary work = foldl (flip enqueueReach) work (reachEdges summary)

edgeDirect :: TopKey -> MemberRef -> ReachEdge
edgeDirect (TopKey mn n) = Direct mn n
