-- SPDX-License-Identifier: BSD-3-Clause

module Acton.ReachabilityRowsBuilder (prepareReachabilityRows) where

import qualified Acton.Builtin as Builtin
import qualified Acton.Env as Env
import qualified Acton.InterfaceRows as Rows
import qualified Acton.NameInfo as I
import qualified Acton.Names as Names
import qualified Acton.QuickType as QuickType
import qualified Acton.Reachability as Reach
import Acton.ReachabilityRows
import Acton.ReachabilityTypes
import qualified Acton.Syntax as A
import qualified Acton.WitnessForwarding as Forward

import Control.DeepSeq (force)
import Control.Monad (foldM, unless)
import qualified Data.HashMap.Strict as HashMap
import Data.List (foldl', partition)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


-- Preparation -------------------------------------------------------------------------------------------

prepareReachabilityRows :: Env.Env0 -> I.TEnv -> A.Module -> Rows.InterfaceRows -> Rows.RowResult ReachabilityRows
prepareReachabilityRows typeEnv sourceInterface (A.Module mn _ _ suite) stored = do
    Rows.validateInterfaceRows stored
    unless (Rows.rowModuleName stored == mn) $
      Rows.rowError "reachability/interface module mismatch"
    unless (length (Rows.rowStatements stored) == length suite) $
      Rows.rowError "reachability/interface top-level statement mismatch"
    let suiteEnv = QuickType.envOfTopSuite suite
        moduleEnv = Env.define suiteEnv (Env.setMod mn typeEnv)
        globals = Set.fromList (topNames suite)
        env0 = Reach.topReachEnv moduleEnv globals
        extensions = Map.fromListWith (flip (++))
          [ (extensionTarget ext,[ext])
          | ext <- extensionInfos moduleEnv mn suite
          ]
        forwarding = Forward.buildForwardContext moduleEnv
          [ n | (n,I.NClass{}) <- suiteEnv ]
        sourceInfo = Env.hnamesFrom sourceInterface
        prepareMetadata = prepareContainerMetadata moduleEnv forwarding mn stored extensions
    (prepared,_) <- foldM (prepareTop sourceInfo stored prepareMetadata)
      (emptyReachabilityRows, env0) (zip suite $ Rows.rowStatements stored)
    let whole = force (wholeSummary env0 prepared)
    whole `seq` return prepared{ reachWholeSummary = whole }


data ContainerPrepared = ContainerPrepared
  { preparedKind        :: ShapeKind
  , preparedBackendKind :: ShapeKind
  , preparedMembers     :: Map.Map Rows.MemberKey Rows.MemberContent
  , preparedAnalysis    :: ContainerAnalysis
  }

prepareTop :: I.HTEnv
           -> Rows.InterfaceRows
           -> (ReachabilityRows -> (TopKey,ContainerPrepared) -> Rows.RowResult ReachabilityRows)
           -> (ReachabilityRows, Reach.ReachEnv)
           -> (A.Stmt, Rows.StoredStmt)
           -> Rows.RowResult (ReachabilityRows, Reach.ReachEnv)
prepareTop sourceInfo stored prepareMetadata (rows,env) (stmt,storedStmt) = do
    let env' = Reach.advanceReachEnv (QuickType.envOf stmt) env
    case (stmt,storedStmt) of
      (A.Decl _ decls, Rows.StoredDecls _ storedDecls) -> do
        unless (length decls == length storedDecls) $
          Rows.rowError "reachability/interface declaration count mismatch"
        let declEnv = Reach.advanceReachEnv (QuickType.envOf decls) env
        rows' <- foldM (prepareTopDecl sourceInfo stored prepareMetadata declEnv)
          rows (zip decls storedDecls)
        return (rows',env')
      (_,Rows.StoredWhole owners storedWhole) -> do
        unless (stmt == storedWhole) $
          Rows.rowError "reachability/interface whole-statement mismatch"
        unless (owners == wholeStmtOwners stmt) $
          Rows.rowError "reachability/interface whole-statement owner mismatch"
        let summary = Reach.summarizeStmt env stmt
        rows' <- case owners of
          [] ->
            let summary' = force summary
                moduleSummary = reachModuleSummary rows <> summary'
            in summary' `seq` moduleSummary `seq`
               return rows{ reachModuleSummary = moduleSummary }
          names -> foldM (insertTopSummary Nothing summary) rows
            [ TopKey (Rows.rowModuleName stored) n | n <- names ]
        return (rows',env')
      _ -> Rows.rowError "reachability/interface top-level row kind mismatch"

prepareTopDecl :: I.HTEnv
               -> Rows.InterfaceRows
               -> (ReachabilityRows -> (TopKey,ContainerPrepared) -> Rows.RowResult ReachabilityRows)
               -> Reach.ReachEnv
               -> ReachabilityRows
               -> (A.Decl, Rows.StoredDecl)
               -> Rows.RowResult ReachabilityRows
prepareTopDecl sourceInfo stored prepareMetadata env rows (decl,storedDecl) = do
    let mn = Rows.rowModuleName stored
        name = Names.dname' decl
        key = TopKey mn name
        typeEnv = Reach.reachTypeEnv env
    case storedDecl of
      Rows.StoredInline storedInline -> do
        unless (decl == storedInline) $
          Rows.rowError ("reachability/interface inline declaration mismatch for " ++ A.rawstr name)
        rows' <- insertTopSummary Nothing (Reach.summarizeDecl env decl) rows key
        return rows'
      Rows.StoredContainer storedName -> do
        unless (storedName == name) $
          Rows.rowError ("reachability/interface container name mismatch for " ++ A.rawstr name)
        shape <- required ("missing container shape " ++ A.rawstr name) $
          Map.lookup name (Rows.rowShapes stored)
        members <- required ("missing container members " ++ A.rawstr name) $
          Map.lookup name (Rows.rowMembers stored)
        let kind = containerKind sourceInfo decl
            (header,bodyEnv) = Reach.summarizeDeclHeader env decl
            analysis = analyzeContainer shape bodyEnv (A.dbody decl)
        residual <- residualSummary analysis (Rows.shapeSuite shape)
        rows' <- insertTopSummary
          (compactDeclaration $ Env.unalias typeEnv $ Env.findQName (A.NoQ name) typeEnv)
          (header <> residual) rows key
        prepareMetadata rows'
          (key,ContainerPrepared kind (backendContainerKind decl) members analysis)


-- Lexically placed summaries ---------------------------------------------------------------------------

data PlacedStmtSummary = PlacedStmtSummary A.Stmt ReachSummary

data PlacedDeclSummary = PlacedDeclSummary
  { placedDeclaration :: A.Decl
  , placedHeaderSummary :: ReachSummary
  , placedFullSummary :: ReachSummary
  }

data PlacedInitSummary = PlacedInitSummary
  { initStatement :: A.Stmt
  , initEnvironment :: Reach.ReachEnv
  , initGuard :: ReachSummary
  , initSummary :: ReachSummary
  }

data ContainerAnalysis = ContainerAnalysis
  { analyzedStatements        :: Map.Map Rows.StmtPlace PlacedStmtSummary
  , analyzedDecls             :: Map.Map Rows.DeclPlace PlacedDeclSummary
  , analyzedConstructorHeader :: Maybe ReachSummary
  , analyzedConstructorSelf   :: Maybe A.Name
  , analyzedInitBody          :: Map.Map Int PlacedInitSummary
  }

emptyAnalysis :: ContainerAnalysis
emptyAnalysis = ContainerAnalysis Map.empty Map.empty Nothing Nothing Map.empty

analyzeContainer :: Rows.ContainerShape -> Reach.ReachEnv -> A.Suite -> ContainerAnalysis
analyzeContainer shape env suite =
    analyzeSuite constructor Rows.rootPath mempty env suite emptyAnalysis
  where constructor = constructorPlace (Rows.shapeSuite shape)

analyzeSuite :: Maybe Rows.DeclPlace
             -> Rows.SuitePath
             -> ReachSummary
             -> Reach.ReachEnv
             -> A.Suite
             -> ContainerAnalysis
             -> ContainerAnalysis
analyzeSuite constructor path guard env suite analysis0 =
    snd $ foldl' analyze (env,analysis0) (zip [0..] suite)
  where
    analyze (stmtEnv,analysis) (i,stmt) =
      let place = Rows.StmtPlace path i
          summary = guard <> Reach.summarizeStmt stmtEnv stmt
          analysis1 = analysis {
            analyzedStatements = Map.insert place (PlacedStmtSummary stmt summary) (analyzedStatements analysis)
          }
          analysis2 = analyzeNested constructor path guard stmtEnv i stmt analysis1
          env' = Reach.advanceReachEnv (QuickType.envOf stmt) stmtEnv
      in (env',analysis2)

analyzeNested :: Maybe Rows.DeclPlace
              -> Rows.SuitePath
              -> ReachSummary
              -> Reach.ReachEnv
              -> Int
              -> A.Stmt
              -> ContainerAnalysis
              -> ContainerAnalysis
analyzeNested constructor path guard env i stmt analysis = case stmt of
    A.If _ branches elseSuite -> analyzeElse afterBranches
      where
        conditions = [ Reach.summarizeCondition env e | A.Branch e _ <- branches ]
        prefixes = tail $ scanl (<>) mempty conditions
        branchData = zip3 [0..] branches prefixes
        afterBranches = foldl' analyzeBranch analysis branchData
        analyzeBranch acc (branch,A.Branch _ body,prefix) =
          analyzeSuite constructor (Rows.branchPath path i branch) (guard <> prefix) env body acc
        analyzeElse = analyzeSuite constructor (Rows.elsePath path i) (guard <> foldMap id conditions) env elseSuite
    A.Decl _ decls -> foldl' analyzeDecl analysis (zip [0..] decls)
      where
        declEnv = Reach.advanceReachEnv (QuickType.envOf decls) env
        analyzeDecl acc (j,decl) =
          let place = Rows.DeclPlace path i j
              (header,bodyEnv) = Reach.summarizeDeclHeader declEnv decl
              stubHeader = case decl of
                A.Def{} -> fst (Reach.summarizeDeclHeader
                  declEnv{ Reach.reachReflectiveOwner = False }
                  (Rows.methodHeader decl))
                _       -> header
              headerSummary = guard <> stubHeader
              full = guard <> header <> Reach.summarizeSuite bodyEnv (A.dbody decl)
              acc1 = acc {
                analyzedDecls = Map.insert place
                  (PlacedDeclSummary decl headerSummary full) (analyzedDecls acc)
              }
          in if Just place == constructor
               then analyzeConstructorBody guard bodyEnv (A.dbody decl)
                      (acc1
                        { analyzedConstructorHeader = Just (guard <> header)
                        , analyzedConstructorSelf = A.selfPar decl
                        })
               else acc1
    _ -> analysis

analyzeConstructorBody :: ReachSummary
                       -> Reach.ReachEnv
                       -> A.Suite
                       -> ContainerAnalysis
                       -> ContainerAnalysis
analyzeConstructorBody guard env suite analysis0 =
    snd $ foldl' analyze (env,analysis0) (zip [0..] suite)
  where
    analyze (stmtEnv,analysis) (i,stmt) =
      let summary = guard <> Reach.summarizeStmt stmtEnv stmt
          env' = Reach.advanceReachEnv (QuickType.envOf stmt) stmtEnv
      in (env',analysis {
           analyzedInitBody = Map.insert i
             (PlacedInitSummary stmt stmtEnv guard summary)
             (analyzedInitBody analysis)
         })

residualSummary :: ContainerAnalysis -> Rows.SuiteShape -> Rows.RowResult ReachSummary
residualSummary analysis = go Rows.rootPath
  where
    go path shape = foldMapM (inStmt path) (Rows.suiteStructure shape)

    inStmt path (i,Rows.InlineStmt stmt) = do
      PlacedStmtSummary original summary <- lookupStmt analysis (Rows.StmtPlace path i)
      unless (stmt == original) $
        Rows.rowError ("inline statement placement mismatch at " ++ show (Rows.StmtPlace path i))
      return summary
    inStmt path (i,Rows.DeclStmt _ decls) = do
      summaries <- mapM (inDecl path i) (zip [0..] decls)
      return (foldMap id summaries)
    inStmt path (i,Rows.IfStmt _ branches elseShape) = do
      branchSummaries <- mapM (uncurry $ inBranch path i) (zip [0..] branches)
      elseSummary <- go (Rows.elsePath path i) elseShape
      return (foldMap id branchSummaries <> elseSummary)

    inDecl path i (j,Rows.MethodDecl slot) = do
      PlacedDeclSummary original header _ <- lookupDecl analysis (Rows.DeclPlace path i j)
      unless (Rows.slotHeader slot == Rows.methodHeader original) $
        Rows.rowError ("method header placement mismatch at " ++ show (Rows.DeclPlace path i j))
      return header
    inDecl path i (j,Rows.InlineDecl decl) = do
      PlacedDeclSummary original _ summary <- lookupDecl analysis (Rows.DeclPlace path i j)
      unless (decl == original) $
        Rows.rowError ("inline declaration placement mismatch at " ++ show (Rows.DeclPlace path i j))
      return summary

    inBranch path i branch (_,shape) = go (Rows.branchPath path i branch) shape


-- Member summaries -------------------------------------------------------------------------------------

prepareMemberRows :: TopKey
                  -> ContainerPrepared
                  -> ReachabilityRows
                  -> Rows.RowResult ReachabilityRows
prepareMemberRows owner prepared rows =
    foldM add rows (Map.toAscList $ preparedMembers prepared)
  where
    analysis = preparedAnalysis prepared
    backendKind = preparedBackendKind prepared

    add acc (member,content) = do
      info <- memberInfo backendKind analysis member content
      insertMemberInfo owner member info acc

memberInfo :: ShapeKind
           -> ContainerAnalysis
           -> Rows.MemberKey
           -> Rows.MemberContent
           -> Rows.RowResult MemberInfo
memberInfo backendKind analysis member content = case content of
    Rows.MethodContent methods -> do
      summaries <- mapM methodSummary methods
      return (MemberInfo
        (foldMap snd summaries)
        Nothing Nothing)
    Rows.AttrContent _ declarations initializers -> do
      declarationSummaries <- mapM declarationSummary declarations
      staticSummaries <- mapM (initializerSummary Nothing) staticInitializers
      instanceSummaries <- mapM (initializerSummary attribute) instanceInitializers
      return (MemberInfo
        (foldMap id declarationSummaries)
        (nonEmptySummary staticSummaries)
        (nonEmptySummary instanceSummaries))
      where
        (staticInitializers,instanceInitializers) = partition isStaticInitializer initializers
        attribute = case member of
          Rows.Attr name -> Just name
          _              -> Nothing
        isStaticInitializer (Rows.InitFragment Rows.InitSuite{} _) = backendKind == ClassShape
        isStaticInitializer _                                      = False
    Rows.InitRestContent _ initializers -> do
      summaries <- mapM (initializerSummary Nothing) initializers
      return (MemberInfo (foldMap id summaries) Nothing Nothing)
  where
    methodSummary placed = do
      PlacedDeclSummary decl header summary <- lookupDecl analysis (Rows.placedMethodPlace placed)
      unless (decl == Rows.placedMethodDecl placed) $
        Rows.rowError ("method placement mismatch for " ++ A.rawstr (A.dname decl))
      return (header,summary)

    declarationSummary (Rows.PlacedStmt place stmt) = do
      PlacedStmtSummary original summary <- lookupStmt analysis place
      unless (stmt == original) $
        Rows.rowError ("attribute declaration placement mismatch at " ++ show place)
      return summary

    initializerSummary attribute (Rows.InitFragment place stmt) = case place of
      Rows.InitSuite stmtPlace -> do
        PlacedStmtSummary original summary <- lookupStmt analysis stmtPlace
        unless (stmt == original) $
          Rows.rowError ("initializer placement mismatch at " ++ show stmtPlace)
        return summary
      Rows.InitBody i -> do
        placed <- required ("missing constructor statement " ++ show i) $
          Map.lookup i (analyzedInitBody analysis)
        unless (stmt == initStatement placed) $
          Rows.rowError ("constructor initializer placement mismatch at " ++ show i)
        case attribute of
          Nothing -> return (initSummary placed)
          Just name -> do
            self <- required "constructor initializer has no self parameter"
              (analyzedConstructorSelf analysis)
            return $ case Rows.pruneConstructorInit self (Set.singleton name) stmt of
              Nothing -> mempty
              Just projected -> initGuard placed <>
                Reach.summarizeStmt (initEnvironment placed) projected


-- Shape, slot, and reflection rows ----------------------------------------------------------------------

prepareContainerMetadata :: Env.Env0
                         -> Forward.ForwardContext
                         -> A.ModName
                         -> Rows.InterfaceRows
                         -> Map.Map TopKey [ExtensionInfo]
                         -> ReachabilityRows
                         -> (TopKey,ContainerPrepared)
                         -> Rows.RowResult ReachabilityRows
prepareContainerMetadata env forwarding mn stored extensions rows (owner,prepared) = do
    rows1 <- prepareMemberRows owner prepared rows
    let targetExtensions = Map.findWithDefault [] owner extensions
    slots <- effectiveSlots env forwarding mn stored targetExtensions owner
    let abstracts = Set.toAscList $ Set.fromList
          [ ref | (ref,SlotInfo _ AbstractSlot) <- Map.toList slots ]
        reflectable = Set.toAscList $ Set.fromList
          [ name
          | (AttrRef name,SlotInfo provider AttributeSlot) <- Map.toList slots
          , provider == owner
          , not (Names.isWitness name)
          , reflectableProperty kind stored owner name
          ]
        kind = preparedKind prepared
        constructors = constructorInfo env kind owner prepared (constructorObligations kind owner slots)
        shape = ShapeInfo owner kind
                  (shapeLineageFor env targetExtensions owner)
                  constructors abstracts
    rows2 <- insertTopSummary Nothing
      (forwardingObligations slots <> inheritedValueObligations env owner)
      rows1 owner
    rows3 <- insertShapeInfo owner shape rows2
    rows4 <- foldM (insertSlotInfo owner) rows3 (Map.toAscList slots)
    insertReflectable owner (ReflectableAttrs reflectable) rows4

reflectableProperty :: ShapeKind
                    -> Rows.InterfaceRows
                    -> TopKey
                    -> A.Name
                    -> Bool
reflectableProperty kind stored owner name = case content of
    Just Rows.AttrContent
      { Rows.attrDeclarations = declarations
      , Rows.attrInitializers = initializers
      } ->
        not (null declarations) ||
        kind == ActorShape ||
        any constructorInitializer initializers
    _ -> False
  where
    content = Map.lookup (topName owner) (Rows.rowMembers stored) >>=
      Map.lookup (Rows.Attr name)
    constructorInitializer (Rows.InitFragment place _) = case place of
      Rows.InitBody{}  -> True
      Rows.InitSuite{} -> False


data ExtensionInfo = ExtensionInfo
  { extensionOwner     :: TopKey
  , extensionTarget    :: TopKey
  , extensionProtocols :: [TopKey]
  , extensionMembers   :: I.TEnv
  }

extensionInfos :: Env.Env0 -> A.ModName -> A.Suite -> [ExtensionInfo]
extensionInfos env mn suite =
    [ make decl
    | A.Decl _ decls <- suite
    , decl@A.Extension{} <- decls
    ]
  where
    make decl = case Env.findQName (A.NoQ name) env of
      I.NExt _ target protocols members _ _ -> ExtensionInfo
        (TopKey mn name)
        (topKey env $ A.tcname target)
        (stableTopKeys [ topKey env $ A.tcname p | (_,p) <- protocols ])
        members
      info -> error ("Acton.ReachabilityRowsBuilder: extension info expected for " ++ show name ++ ", got " ++ show info)
      where name = Names.dname' decl

effectiveSlots :: Env.Env0
               -> Forward.ForwardContext
               -> A.ModName
               -> Rows.InterfaceRows
               -> [ExtensionInfo]
               -> TopKey
               -> Rows.RowResult (Map.Map MemberRef SlotInfo)
effectiveSlots env forwarding mn stored extensions owner = do
    direct <- directSlots env owner
    let viaExtensions = foldl' addExtension Map.empty extensions
        physical = directPhysicalSlots mn stored owner
        concrete = addConcrete viaExtensions $ addConcrete physical direct
        plans = Map.fromList
          [ (Forward.forwardSlotName plan,plan)
          | plan <- Forward.forwardPlans env forwarding (containerTCon env owner)
          ]
    return (Map.mapWithKey (generatedSlot plans) concrete)
  where
    -- The type environment may expose an inherited protocol signature before
    -- the concrete declaration that implements it.  Concrete class/actor
    -- members win first; an extension fills a genuinely missing or abstract
    -- slot, but never replaces an already concrete provider.
    addConcrete additions slots0 = Map.foldlWithKey' add slots0 additions
      where
        add slots ref info = case Map.lookup ref slots of
          Nothing                         -> Map.insert ref info slots
          Just (SlotInfo _ AbstractSlot)  -> Map.insert ref info slots
          Just _                          -> slots

    addExtension slots ext = foldl' (addMember $ extensionOwner ext) slots (effectiveTEnv $ extensionMembers ext)
    addMember provider slots (name,info) =
      case infoMemberRef info name of
        Nothing -> slots
        Just ref
          | isConstructorRef ref -> slots
          | otherwise -> Map.insert ref (SlotInfo provider $ slotForInfo env provider ref info) slots

    generatedSlot plans (MethodRef name) (SlotInfo _ AbstractSlot)
      | Just plan <- Map.lookup name plans
          = SlotInfo owner (GeneratedSlot $ forwardingSummary env plan)
    generatedSlot _ _ info = info

directSlots :: Env.Env0
            -> TopKey
            -> Rows.RowResult (Map.Map MemberRef SlotInfo)
directSlots env owner = foldM add Map.empty names
  where
    qn = topQName owner
    ancestry = (qn,directEnv) :
      [ (A.tcname con,te)
      | (_,con) <- inherited
      , let (_,_,te) = Env.findConName (A.tcname con) env
      ]
    (_,inherited,directEnv) = Env.findConName qn env
    names = Env.uniqueNames [ n | (_,te) <- ancestry, (n,_) <- te ]

    add slots name = case firstProvider name ancestry of
      Nothing -> Rows.rowError ("missing effective provider for " ++ A.rawstr name)
      Just (providerQName,info) -> case infoMemberRef info name of
        Nothing -> return slots
        Just ref | isConstructorRef ref -> return slots
        Just ref -> do
          let provider = topKey env providerQName
          return $ Map.insert ref (SlotInfo provider $ slotForInfo env provider ref info) slots

directPhysicalSlots :: A.ModName
                    -> Rows.InterfaceRows
                    -> TopKey
                    -> Map.Map MemberRef SlotInfo
directPhysicalSlots mn stored owner
  | topModule owner /= mn = Map.empty
  | otherwise = case Map.lookup (topName owner) (Rows.rowMembers stored) of
      Nothing -> Map.empty
      Just members -> Map.fromList
        [ (ref,SlotInfo owner slot)
        | member <- Map.keys members
        , Just (ref,slot) <- [physical member]
        ]
  where
    physical (Rows.Method n)
      | n == Builtin.initKW = Nothing
      | otherwise          = Just (MethodRef n,physicalMethod n)
    physical (Rows.Attr n)  = Just (AttrRef n,physicalAttr)
    physical Rows.InitRest  = Nothing

    physicalMethod n
      | topModule owner == Builtin.mBuiltin = OpaqueSlot
      | otherwise = StoredSlot (Rows.Method n)
    physicalAttr
      | topModule owner == Builtin.mBuiltin = OpaqueSlot
      | otherwise = AttributeSlot

slotForInfo :: Env.Env0 -> TopKey -> MemberRef -> I.NameInfo -> SlotDecl
slotForInfo _ provider _ _ | topModule provider == Builtin.mBuiltin = OpaqueSlot
slotForInfo _ _ (MethodRef n) info = case info of
    I.NSig{}                       -> AbstractSlot
    I.NDef{}                       -> StoredSlot (Rows.Method n)
    _                              -> error ("Acton.ReachabilityRowsBuilder: method info expected, got " ++ show info)
slotForInfo env provider (AttrRef _) info = case info of
    I.NSig{}
      | concretePropertyOwner      -> AttributeSlot
      | otherwise                  -> AbstractSlot
    I.NVar{}                       -> AttributeSlot
    I.NSVar{}                      -> AttributeSlot
    _                              -> error ("Acton.ReachabilityRowsBuilder: attribute info expected, got " ++ show info)
  where
    concretePropertyOwner = case Env.findQName (topQName provider) env of
      I.NClass{} -> True
      I.NAct{}   -> True
      _          -> False

containerTCon :: Env.Env0 -> TopKey -> A.TCon
containerTCon env owner = A.TC qn (map A.tVar $ A.qbound q)
  where qn = case Env.thismod env of
          Just mn | mn == topModule owner -> A.NoQ (topName owner)
          _                               -> topQName owner
        (q,_,_) = Env.findConName qn env

forwardingSummary :: Env.Env0 -> Forward.ForwardPlan -> ReachSummary
forwardingSummary env plan = reachSummaryFromEdges
    (rootEdges ++ pathEdges ++ [directEdge provider (MethodRef $ Forward.forwardSlotName plan)])
  where root = topKey env $ A.tcname $ Forward.forwardProviderRoot plan
        provider = topKey env $ A.tcname $ Forward.forwardProviderType plan
        rootEdges = Need (topModule root) (topName root) :
          [ Construct (topModule root) (topName root)
          | topModule root /= Builtin.mBuiltin
          ]
        pathEdges =
          [ directEdge (topKey env $ A.tcname $ Forward.forwardStepOwner step)
              (AttrRef $ Forward.forwardStepField step)
          | step <- Forward.forwardProviderSteps plan
          ]

constructorInfo :: Env.Env0
                -> ShapeKind
                -> TopKey
                -> ContainerPrepared
                -> ReachSummary
                -> Maybe (TopKey,ConstructorDecl)
constructorInfo env kind owner prepared generated = case kind of
    ProtocolShape
      | hasInitRest -> Just (owner,StoredConstructor summary)
      | otherwise -> Nothing
    _ | topModule owner == Builtin.mBuiltin -> Just (owner,OpaqueConstructor)
    _ | hasInitRest -> Just (owner,StoredConstructor summary)
    WitnessShape -> Just (owner,GeneratedConstructor summary)
    _
      | Just provider <- inheritedProvider
          -> Just (provider,InheritedConstructor generated)
      | otherwise -> Just (owner,GeneratedConstructor summary)
  where
    hasInitRest = Map.member Rows.InitRest (preparedMembers prepared)
    header = maybe mempty id (analyzedConstructorHeader $ preparedAnalysis prepared)
    summary = header <> generated
    (_,inherited,_) = Env.findConName (topQName owner) env
    inheritedProvider = firstConcreteAncestor env inherited

-- Runtime value conversion and C-only iterable consumers can invoke these
-- slots after the typed tree has been summarized. Retaining them on a
-- constructed receiver keeps that hidden runtime surface explicit and
-- bounded; ordinary source calls remain exact member edges.
constructorObligations :: ShapeKind -> TopKey -> Map.Map MemberRef SlotInfo -> ReachSummary
constructorObligations kind owner slots = reachSummaryFromEdges
    [ directEdge owner ref
    | (ref@(MethodRef name),SlotInfo _ slot) <- Map.toAscList slots
    , slot /= AbstractSlot
    , name == Names.altInit ||
      name `elem` (Builtin.nextKW : Builtin.valueKWs) ||
      kind == ActorShape && name == Builtin.cleanupKW
    ]

-- Codegen emits every forwarding wrapper and installs it in the method table
-- whenever the container itself is emitted.  Its provider path is therefore a
-- top-level obligation, even when no selected call reaches the forwarded slot.
forwardingObligations :: Map.Map MemberRef SlotInfo -> ReachSummary
forwardingObligations = foldMap obligation
  where
    obligation (SlotInfo _ (GeneratedSlot summary)) = summary
    obligation _                                    = mempty

-- CodeGen initializes inherited NVar entries in every emitted class table,
-- even when no source expression reads the value. Keep those exact provider
-- slots with the container top. NSig properties are instance layout and are
-- deliberately not part of this class-table obligation. Internal witness
-- bindings are normalized into globals, locals, parameters, or instance
-- properties and never occupy class-table slots.
inheritedValueObligations :: Env.Env0 -> TopKey -> ReachSummary
inheritedValueObligations env owner = reachSummaryFromEdges
    [ directEdge owner (AttrRef name)
    | (provider,name) <- Env.inheritedAttrs env (topQName owner)
    , not (Names.isWitness name)
    , Just I.NVar{} <- [Env.findAttrInfo' env provider name]
    ]

directEdge :: TopKey -> MemberRef -> ReachEdge
directEdge (TopKey mn name) = Direct mn name

shapeLineageFor :: Env.Env0 -> [ExtensionInfo] -> TopKey -> [TopKey]
shapeLineageFor env extensions owner = stableTopKeys (owner : inherited ++ protocols)
  where
    (_,bases,_) = Env.findConName (topQName owner) env
    inherited = [ topKey env $ A.tcname con | (_,con) <- bases ]
    protocols = concatMap extensionProtocols extensions

-- Protocols and extensions have already become backend classes in the typed
-- tree. Their source interface entry is the remaining semantic provenance;
-- generated sibling classes have no source entry and retain their backend
-- class kind.
containerKind :: I.HTEnv -> A.Decl -> ShapeKind
containerKind sourceInfo decl = case HashMap.lookup (Names.dname' decl) sourceInfo of
    Just I.NAct{}   -> ActorShape
    Just I.NClass{} -> ClassShape
    Just I.NProto{} -> ProtocolShape
    Just I.NExt{}   -> WitnessShape
    Just info       -> error ("Acton.ReachabilityRowsBuilder: container interface expected, got " ++ show info)
    Nothing         -> backendContainerKind decl

backendContainerKind :: A.Decl -> ShapeKind
backendContainerKind A.Actor{}     = ActorShape
backendContainerKind A.Class{}     = ClassShape
backendContainerKind A.Protocol{}  = ProtocolShape
backendContainerKind A.Extension{} = WitnessShape
backendContainerKind decl          = error ("Acton.ReachabilityRowsBuilder: container expected, got " ++ show decl)


-- Utilities ---------------------------------------------------------------------------------------------

topNames :: A.Suite -> [A.Name]
topNames = Env.uniqueNames . concatMap stmtNames

stmtNames :: A.Stmt -> [A.Name]
stmtNames (A.Decl _ decls)   = map Names.dname' decls
stmtNames stmt               = wholeStmtOwners stmt

wholeStmtOwners :: A.Stmt -> [A.Name]
wholeStmtOwners              = Env.uniqueNames . map fst . QuickType.envOf

topKey :: Env.Env0 -> A.QName -> TopKey
topKey env qn = case Env.unalias env qn of
    A.GName mn n -> TopKey mn n
    A.QName mn n -> TopKey mn n
    A.NoQ n      -> case Env.thismod env of
      Just mn -> TopKey mn n
      Nothing -> error ("Acton.ReachabilityRowsBuilder: unscoped name " ++ A.rawstr n)

topQName :: TopKey -> A.QName
topQName (TopKey mn n) = A.GName mn n

topModule :: TopKey -> A.ModName
topModule (TopKey mn _) = mn

topName :: TopKey -> A.Name
topName (TopKey _ n) = n

effectiveTEnv :: I.TEnv -> I.TEnv
effectiveTEnv te =
    [ (n,info)
    | n <- Env.uniqueNames (map fst te)
    , Just info <- [Env.findAttrInfoIn n te]
    ]

firstProvider :: A.Name -> [(A.QName,I.TEnv)] -> Maybe (A.QName,I.NameInfo)
firstProvider _ [] = Nothing
firstProvider name ((provider,te):rest) = case Env.findAttrInfoIn name te of
    Just info -> Just (provider,info)
    Nothing   -> firstProvider name rest

firstConcreteAncestor :: Env.Env0 -> [(I.WPath,A.TCon)] -> Maybe TopKey
firstConcreteAncestor _ [] = Nothing
firstConcreteAncestor env ((_,con):rest) = case Env.findQName name env of
    I.NClass{} -> Just (topKey env name)
    I.NAct{}   -> Just (topKey env name)
    _          -> firstConcreteAncestor env rest
  where name = A.tcname con

infoMemberRef :: I.NameInfo -> A.Name -> Maybe MemberRef
infoMemberRef info name = case info of
    I.NDef{}                       -> Just (MethodRef name)
    I.NSig schema deco _
      | deco == A.Property        -> Just (AttrRef name)
      | A.TFun{} <- A.sctype schema
                                    -> Just (MethodRef name)
      | otherwise                 -> Nothing
    I.NVar{}                       -> Just (AttrRef name)
    I.NSVar{}                      -> Just (AttrRef name)
    _                              -> Nothing

isConstructorRef :: MemberRef -> Bool
isConstructorRef (MethodRef name) = name == Builtin.initKW
isConstructorRef _                = False

constructorPlace :: Rows.SuiteShape -> Maybe Rows.DeclPlace
constructorPlace shape = case [ Rows.slotPlace slot | slot <- methodSlots shape, Rows.slotIsConstructor slot ] of
    []      -> Nothing
    [place] -> Just place
    places  -> error ("Acton.ReachabilityRowsBuilder: multiple constructor slots " ++ show places)

methodSlots :: Rows.SuiteShape -> [Rows.MethodSlot]
methodSlots shape = concatMap (inStmt . snd) (Rows.suiteStructure shape)
  where
    inStmt (Rows.InlineStmt _) = []
    inStmt (Rows.DeclStmt _ decls) = [ slot | Rows.MethodDecl slot <- decls ]
    inStmt (Rows.IfStmt _ branches elseShape) =
      concatMap (methodSlots . snd) branches ++ methodSlots elseShape

lookupStmt :: ContainerAnalysis -> Rows.StmtPlace -> Rows.RowResult PlacedStmtSummary
lookupStmt analysis place = required ("missing statement placement " ++ show place) $
    Map.lookup place (analyzedStatements analysis)

lookupDecl :: ContainerAnalysis -> Rows.DeclPlace -> Rows.RowResult PlacedDeclSummary
lookupDecl analysis place = required ("missing declaration placement " ++ show place) $
    Map.lookup place (analyzedDecls analysis)

nonEmptySummary :: [ReachSummary] -> Maybe ReachSummary
nonEmptySummary [] = Nothing
nonEmptySummary summaries = Just (foldMap id summaries)

-- A whole-surface module can call selective providers from any emitted body.
-- Persist one compact aggregate so a whole boundary contributes exact
-- provider interest without forcing those providers whole as well.
wholeSummary :: Reach.ReachEnv -> ReachabilityRows -> ReachSummary
wholeSummary env rows =
    reachModuleSummary rows <>
    foldMap topSummary (Map.elems $ reachTopRows rows) <>
    foldMap memberInfoSummary (Map.elems $ reachMemberRows rows) <>
    foldMap shapeInfoSummary (Map.elems $ reachShapeRows rows) <>
    inheritedPropertyLayoutSummary <>
    foldMap propertyTypeSummary (Map.keys $ reachShapeRows rows)
  where
    topSummary (LocalTop _ summary) = summary
    topSummary (OpaqueTop summary)  = summary

    memberInfoSummary info =
      memberSummary info <>
      maybe mempty id (memberStaticInitSummary info) <>
      maybe mempty id (memberInstanceInitSummary info)

    shapeInfoSummary = maybe mempty (constructorSummary . snd) . shapeConstructor
    constructorSummary constructor = case constructor of
      StoredConstructor summary    -> summary
      GeneratedConstructor summary -> summary
      InheritedConstructor summary -> summary
      OpaqueConstructor            -> mempty

    -- A whole consumer keeps its complete inherited instance layout. Retain
    -- every ancestor property declaration, including one shadowed by a local
    -- redeclaration, so selectively generated ancestors keep the same prefix.
    inheritedPropertyLayoutSummary = Reach.reachSummaryFromEdges
      [ DeclareAttr (topModule ancestor) (topName ancestor) name
      | ((receiver,AttrRef name),SlotInfo _ AttributeSlot) <-
          Map.toAscList (reachSlotRows rows)
      , Just shape <- [Map.lookup receiver (reachShapeRows rows)]
      , ancestor <- drop 1 (shapeLineage shape)
      , declaresProperty ancestor name
      ]

    declaresProperty owner name = case Env.findQName (topQName owner) typeEnv of
      I.NClass _ _ members _     -> property members
      I.NAct _ _ _ members _     -> property members
      _                           -> False
      where
        property members = case lookup name members of
          Just (I.NSig _ A.Property _) -> True
          _                            -> False
        typeEnv = Reach.reachTypeEnv env

    -- Whole CodeGen emits every effective instance-property field in each
    -- class header, including inherited properties, whether or not the class
    -- is constructed. Their representation types therefore belong to the
    -- whole-module contract, but not to a selectively projected class top.
    propertyTypeSummary owner = foldMap propertyType
      (Env.fullAttrEnv (Reach.reachTypeEnv env) $
        containerTCon (Reach.reachTypeEnv env) owner)
    propertyType (_,I.NSig schema A.Property _) =
      Reach.summarizeType env (A.sctype schema)
    propertyType _ = mempty

stableTopKeys :: [TopKey] -> [TopKey]
stableTopKeys = go Set.empty
  where
    go _ [] = []
    go seen (key:keys)
      | Set.member key seen = go seen keys
      | otherwise = key : go (Set.insert key seen) keys

compactDeclaration :: I.NameInfo -> Maybe I.NameInfo
compactDeclaration info = case info of
    I.NClass q bases _ doc        -> Just (I.NClass q bases [] doc)
    I.NProto q bases _ doc        -> Just (I.NProto q bases [] doc)
    I.NAct q pos kwd _ doc        -> Just (I.NAct q pos kwd [] doc)
    I.NExt q target bases _ opts doc -> Just (I.NExt q target bases [] opts doc)
    _                             -> error
      ("Acton.ReachabilityRowsBuilder: container declaration expected, got " ++ show info)

insertTopSummary :: Maybe I.NameInfo
                 -> ReachSummary
                 -> ReachabilityRows
                 -> TopKey
                 -> Rows.RowResult ReachabilityRows
insertTopSummary declaration summary rows key = do
    let info
          | topModule key == Builtin.mBuiltin = OpaqueTop summary
          | otherwise = LocalTop declaration summary
    tops <- case Map.lookup key (reachTopRows rows) of
      Nothing -> return $ Map.insert key (force info) (reachTopRows rows)
      Just previous -> do
        merged <- mergeTopInfo key previous info
        return $ Map.insert key (force merged) (reachTopRows rows)
    return rows{ reachTopRows = tops }

mergeTopInfo :: TopKey -> TopInfo -> TopInfo -> Rows.RowResult TopInfo
mergeTopInfo _ (OpaqueTop summary) (OpaqueTop summary') =
    return (OpaqueTop $ summary <> summary')
mergeTopInfo key (LocalTop declaration summary) (LocalTop declaration' summary') =
    case (declaration,declaration') of
      (Just _,Just _) -> Rows.rowError ("duplicate container reach row " ++ show key)
      _ -> return $ LocalTop (pick declaration declaration') (summary <> summary')
  where
    pick (Just info) _ = Just info
    pick Nothing info  = info
mergeTopInfo key _ _ = Rows.rowError ("inconsistent top reach row " ++ show key)

insertMemberInfo :: TopKey -> Rows.MemberKey -> MemberInfo -> ReachabilityRows -> Rows.RowResult ReachabilityRows
insertMemberInfo owner member info rows = do
    members <- insertUnique (owner,member) (force info) (reachMemberRows rows)
      ("duplicate member reach row " ++ show (owner,member))
    return rows{ reachMemberRows = members }

insertShapeInfo :: TopKey -> ShapeInfo -> ReachabilityRows -> Rows.RowResult ReachabilityRows
insertShapeInfo owner info rows = do
    shapes <- insertUnique owner (force info) (reachShapeRows rows) ("duplicate shape reach row " ++ show owner)
    return rows{ reachShapeRows = shapes }

insertSlotInfo :: TopKey -> ReachabilityRows -> (MemberRef,SlotInfo) -> Rows.RowResult ReachabilityRows
insertSlotInfo receiver rows (ref,info) = do
    slots <- insertUnique (receiver,ref) (force info) (reachSlotRows rows)
      ("duplicate slot reach row " ++ show (receiver,ref))
    return rows{ reachSlotRows = slots }

insertReflectable :: TopKey -> ReflectableAttrs -> ReachabilityRows -> Rows.RowResult ReachabilityRows
insertReflectable receiver attrs rows = do
    reflected <- insertUnique receiver (force attrs) (reachReflectableRows rows)
      ("duplicate reflectable reach row " ++ show receiver)
    return rows{ reachReflectableRows = reflected }

insertUnique :: Ord k => k -> a -> Map.Map k a -> String -> Rows.RowResult (Map.Map k a)
insertUnique key value values msg
  | Map.member key values = Rows.rowError msg
  | otherwise = return (Map.insert key value values)

required :: String -> Maybe a -> Rows.RowResult a
required msg = maybe (Rows.rowError msg) return

foldMapM :: (Monoid b, Monad m) => (a -> m b) -> [a] -> m b
foldMapM f = foldM (\acc item -> (acc <>) <$> f item) mempty
