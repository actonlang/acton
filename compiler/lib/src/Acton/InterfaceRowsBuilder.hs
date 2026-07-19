-- SPDX-License-Identifier: BSD-3-Clause

module Acton.InterfaceRowsBuilder(prepareInterfaceRows) where

import Control.Monad
import qualified Data.IntSet as IntSet
import Data.List (foldl', sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import qualified Acton.Builtin as B
import qualified Acton.Env as Env
import Acton.InterfaceRows
import qualified Acton.NameInfo as I
import qualified Acton.Names as Names
import qualified Acton.QuickType as QuickType
import qualified Acton.Syntax as A
import qualified Acton.Types as Types


data ActorPlan = ActorPlan
  { actorLocals :: S.Set A.Name
  , actorParameterAttrs :: [A.Name]
  } deriving Eq

data ContainerKind
  = KActor ActorPlan
  | KClass (S.Set A.Name)
  | KProtocol
  | KExtension
  deriving Eq

data InitRoute
  = RouteTop
  | RouteAttrs [A.Name]
  | RouteRest
  deriving Eq

data AttrAccum = AttrAccum
  { accumNames :: [A.Name]
  , accumDecls :: [PlacedStmt]
  , accumInits :: [InitFragment]
  }

data PrepareState = PrepareState
  { preparedMethods     :: M.Map A.Name [PlacedMethod]
  , preparedAttrs       :: M.Map A.Name AttrAccum
  , preparedRest        :: [InitFragment]
  , preparedConstructor :: Maybe PlacedMethod
  , methodCounts        :: M.Map A.Name Int
  }

emptyPrepareState :: PrepareState
emptyPrepareState = PrepareState M.empty M.empty [] Nothing M.empty

prepareInterfaceRows :: Env.Env0 -> A.Module -> RowResult InterfaceRows
prepareInterfaceRows env (A.Module mn imps doc suite) = do
    (stmts, containers) <- mapAndUnzipM (prepareTopStmt env partitionEnv) suite
    let prepared = [ c | cs <- containers, c <- cs ]
        names = [ shapeName shape | (shape, _) <- prepared ]
    when (length names /= S.size (S.fromList names)) $
      rowError "duplicate top-level container names"
    return InterfaceRows
      { rowModuleName = mn
      , rowImports = imps
      , rowDoc = doc
      , rowHasNotImpl = A.hasNotImpl suite
      , rowStatements = stmts
      , rowShapes = M.fromList [ (shapeName shape, shape) | (shape, _) <- prepared ]
      , rowMembers = M.fromList [ (shapeName shape, members) | (shape, members) <- prepared ]
      }
  where
    partitionEnv = Env.define (QuickType.envOfTopSuite suite) (Env.setMod mn env)

prepareTopStmt :: Env.Env0 -> Env.Env0 -> A.Stmt -> RowResult (StoredStmt, [(ContainerShape, M.Map MemberKey MemberContent)])
prepareTopStmt semanticEnv backendEnv (A.Decl l decls) = do
    prepared <- mapM prepare decls
    return (StoredDecls l (map fst prepared), [ c | (_, Just c) <- prepared ])
  where
    prepare decl
      | isContainer decl = do
          container <- prepareContainer semanticEnv backendEnv decl
          return (StoredContainer (Names.dname' decl), Just container)
      | otherwise = return (StoredInline decl, Nothing)
prepareTopStmt _ _ stmt =
    return (StoredWhole (wholeStmtOwners stmt) stmt, [])

wholeStmtOwners :: A.Stmt -> [A.Name]
wholeStmtOwners = Env.uniqueNames . map fst . QuickType.envOf

isContainer :: A.Decl -> Bool
isContainer A.Actor{}     = True
isContainer A.Class{}     = True
isContainer A.Protocol{}  = True
isContainer A.Extension{} = True
isContainer _             = False

prepareContainer :: Env.Env0 -> Env.Env0 -> A.Decl -> RowResult (ContainerShape, M.Map MemberKey MemberContent)
prepareContainer semanticEnv backendEnv decl = do
    constructor <- classConstructor decl
    let (kind0, head', suite) = containerParts decl
        kind = case kind0 of
          KClass _ -> KClass (classAttrs semanticEnv backendEnv $ Names.dname' decl)
          _ -> kind0
        constructorPlace = fmap placedMethodPlace constructor
        initialState = case kind of
          KActor plan -> foldl' (flip ensureAttr) emptyPrepareState (actorParameterAttrs plan)
          _ -> emptyPrepareState
    (suiteShape, state0) <- prepareSuite (containerBodyEnv backendEnv decl)
      kind constructorPlace RouteTop rootPath suite initialState
    state1 <- case constructor of
      Nothing -> return state0
      Just placed -> prepareConstructor backendEnv suite placed state0
    let members = finishMembers state1
        initCount = length . A.dbody . placedMethodDecl <$> constructor
        shape = ContainerShape (Names.dname' decl) head' suiteShape initCount
    validatePreparedContainer shape members
    return (shape, members)

containerBodyEnv :: Env.Env0 -> A.Decl -> Env.Env0
containerBodyEnv env A.Actor{A.dname=n,A.qbinds=q,A.pos=p,A.kwd=k} =
    Env.define (QuickType.envOf p ++ QuickType.envOf k) $
    Env.setInAct $
    Env.define [(B.selfKW, I.NVar $ A.tCon tc)] $
    Env.defineTVars q env
  where tc = A.TC (A.NoQ n) (map A.tVar $ A.qbound q)
containerBodyEnv env _ = env

containerParts :: A.Decl -> (ContainerKind, ContainerHead, A.Suite)
containerParts (A.Actor l n q p k suite doc) =
    (KActor (makeActorPlan p k suite), ActorHead l n q p k doc, suite)
containerParts (A.Class l n q bases suite doc) =
    (KClass S.empty, ClassHead l n q bases doc, suite)
containerParts (A.Protocol l n q bases suite doc) =
    (KProtocol, ProtocolHead l n q bases doc, suite)
containerParts (A.Extension l q con bases suite doc) =
    (KExtension, ExtensionHead l q con bases doc, suite)
containerParts decl = error ("containerParts: " ++ show decl)

classAttrs :: Env.Env0 -> Env.Env0 -> A.Name -> S.Set A.Name
classAttrs semanticEnv backendEnv name = case Env.tryQName qn semanticEnv of
    -- The semantic class owns inferred source attributes; the backend class
    -- additionally owns converter-generated witness attributes.
    Just (I.NClass _ _ semanticMembers _) ->
      attrs semanticMembers `S.union` backendAttrs
    Just I.NProto{} -> backendAttrs
    Just I.NExt{}   -> backendAttrs
    Just info       -> expected info
    Nothing         -> backendAttrs
  where
    backendAttrs = case Env.findQName qn backendEnv of
      I.NClass _ _ members _ -> attrs members
      info                   -> expected info
    attrs members = S.fromList [ member | (member,info) <- members, isAttr info ]
    expected info = error ("InterfaceRowsBuilder: class info expected for " ++ show name ++
                           ", got " ++ show info)
    qn = A.NoQ name
    isAttr I.NVar{}  = True
    isAttr I.NSVar{} = True
    isAttr _         = False

makeActorPlan :: A.PosPar -> A.KwdPar -> A.Suite -> ActorPlan
makeActorPlan p k body = ActorPlan locals parameterAttrs
  where
    (liveVars,_) = QuickType.actorBindings p k body
    paramNames = Names.bound (p,k)
    locals = S.fromList (Env.uniqueNames (liveVars ++ Names.bound decls))
    parameterAttrs = filter (`S.member` locals) paramNames
    decls = filter A.isDecl body

actorStmtAttrs :: ActorPlan -> A.Stmt -> [A.Name]
actorStmtAttrs _ (A.VarAssign _ patterns _) = typedPatternNames patterns
actorStmtAttrs plan (A.Assign _ patterns _) =
    [ name | name <- typedPatternNames patterns, S.member name (actorLocals plan) ]
actorStmtAttrs plan stmt@A.If{} =
    [ name
    | name <- actorStmtNames stmt
    , S.member name (actorLocals plan)
    ]
actorStmtAttrs _ _ = []

actorStmtNames :: A.Stmt -> [A.Name]
actorStmtNames = Env.uniqueNames . Names.bound

prunableActorStmtAttrs :: Env.Env0 -> ActorPlan -> A.Stmt -> RowResult [A.Name]
prunableActorStmtAttrs env plan stmt@A.VarAssign{A.expr=expr}
  | pureActorExpr env expr = actorAttrsOnly plan stmt
  | otherwise = return []
prunableActorStmtAttrs env plan stmt@(A.Assign _ _ expr)
  | pureActorExpr env expr = actorAttrsOnly plan stmt
  | otherwise = return []
prunableActorStmtAttrs env plan stmt@(A.If _ branches elseSuite)
  | not (pureActorBranches env branches elseSuite) = return []
  | otherwise = case branches of
      [] -> rowError "actor initializer If has no branches"
      _ -> actorAttrsOnly plan stmt
prunableActorStmtAttrs _ _ _ = return []

actorAttrsOnly :: ActorPlan -> A.Stmt -> RowResult [A.Name]
actorAttrsOnly plan stmt
  | S.fromList attrs == S.fromList (actorStmtNames stmt) = return attrs
  | otherwise = return []
  where attrs = actorStmtAttrs plan stmt

pureActorExpr :: Env.Env0 -> A.Expr -> Bool
pureActorExpr _ A.NotImplemented{} = False
pureActorExpr env (A.Let _ suite expr) =
    pureActorSuite env suite &&
    pureActorExpr (Env.define (QuickType.envOf suite) env) expr
pureActorExpr env expr = QuickType.fxOf env expr == A.fxPure

pureActorBranches :: Env.Env0 -> [A.Branch] -> A.Suite -> Bool
pureActorBranches env branches elseSuite =
    all pureBranch branches && pureActorSuite env elseSuite
  where
    pureBranch (A.Branch condition body) =
      pureActorExpr env condition && pureActorSuite env body

pureActorSuite :: Env.Env0 -> A.Suite -> Bool
pureActorSuite _ [] = True
pureActorSuite env (stmt:rest) =
    pureActorStmt env stmt &&
    pureActorSuite (Env.define (QuickType.envOf stmt) env) rest

pureActorStmt :: Env.Env0 -> A.Stmt -> Bool
pureActorStmt env (A.Assign _ _ expr) = pureActorExpr env expr
pureActorStmt env (A.VarAssign _ _ expr) = pureActorExpr env expr
pureActorStmt env (A.If _ branches elseSuite) =
    pureActorBranches env branches elseSuite
pureActorStmt _ A.Pass{} = True
pureActorStmt _ A.Signature{} = True
pureActorStmt _ A.Decl{} = True
pureActorStmt _ _ = False

typedPatternNames :: [A.Pattern] -> [A.Name]
typedPatternNames = Env.uniqueNames . map fst . QuickType.envOf

classConstructor :: A.Decl -> RowResult (Maybe PlacedMethod)
classConstructor A.Class{A.dbody=suite} =
    case constructorDecls rootPath suite of
      [] -> return Nothing
      [(place@(DeclPlace path _ _), decl)]
        | path /= rootPath -> rowError "conditional __init__ declarations are not supported"
        | A.selfPar decl == Nothing -> rowError "__init__ has no self parameter"
        | otherwise -> return (Just (PlacedMethod place 0 decl))
      _ -> rowError "multiple __init__ declarations are not supported"
classConstructor _ = return Nothing

constructorDecls :: SuitePath -> A.Suite -> [(DeclPlace, A.Decl)]
constructorDecls path suite = concat
    [ inStmt i stmt | (i, stmt) <- zip [0..] suite ]
  where
    inStmt i (A.Decl _ decls) =
      [ (DeclPlace path i j, decl)
      | (j, decl@A.Def{}) <- zip [0..] decls
      , A.dname decl == B.initKW
      ]
    inStmt i (A.If _ branches elseSuite) =
      concat
        [ constructorDecls (branchPath path i branch) body
        | (branch, A.Branch _ body) <- zip [0..] branches
        ] ++ constructorDecls (elsePath path i) elseSuite
    inStmt _ _ = []

prepareSuite :: Env.Env0
             -> ContainerKind
             -> Maybe DeclPlace
             -> InitRoute
             -> SuitePath
             -> A.Suite
             -> PrepareState
             -> RowResult (SuiteShape, PrepareState)
prepareSuite env kind constructor route path suite state = do
    (entries, state', _) <- foldM prepare ([], state, env) (zip [0..] suite)
    return (SuiteShape (length suite) (reverse entries), state')
  where
    prepare (entries, acc, stmtEnv) (i, stmt) = do
      (entry, acc') <- prepareStmt stmtEnv kind constructor route path i stmt acc
      let nextEnv = Env.define (QuickType.envOf stmt) stmtEnv
      return (maybe entries (\e -> (i, e) : entries) entry, acc', nextEnv)

prepareStmt :: Env.Env0
            -> ContainerKind
            -> Maybe DeclPlace
            -> InitRoute
            -> SuitePath
            -> Int
            -> A.Stmt
            -> PrepareState
            -> RowResult (Maybe ShapeStmt, PrepareState)
prepareStmt env kind@(KActor plan) constructor RouteTop path i stmt state
  | not (A.isDecl stmt)
  , not (A.isSig stmt) = do
      attrs <- prunableActorStmtAttrs env plan stmt
      let state'
            | null attrs = foldl' (flip ensureAttr) state (actorStmtAttrs plan stmt)
            | otherwise = state
      prepareStmt env kind constructor
        (if null attrs then RouteRest else RouteAttrs (Env.uniqueNames attrs))
        path i stmt state'
prepareStmt env kind@(KClass attrs) constructor RouteTop path i stmt state
  | not (A.isDecl stmt)
  , not (A.isSig stmt) = do
      names <- classStmtAttrs attrs stmt
      if null names
        then prepareStmt env kind constructor RouteRest path i stmt state
        else prepareStmt env kind constructor (RouteAttrs names) path i stmt state
prepareStmt env kind constructor route path i (A.If l branches elseSuite) state = do
    (branchShapes, state1) <- foldM prepareBranch ([], state) (zip [0..] branches)
    (elseShape, state2) <- prepareSuite env kind constructor route (elsePath path i) elseSuite state1
    return (Just (IfStmt l (reverse branchShapes) elseShape), state2)
  where
    prepareBranch (shapes, acc) (branch, A.Branch condition body) = do
      (shape, acc') <- prepareSuite env kind constructor route (branchPath path i branch) body acc
      return ((condition, shape) : shapes, acc')
prepareStmt _ kind constructor _ path i (A.Decl l decls) state = do
    (shapes, state') <- foldM prepare ([], state) (zip [0..] decls)
    return (Just (DeclStmt l (reverse shapes)), state')
  where
    prepare (shapes, acc) (j, decl@A.Def{}) = do
      let place = DeclPlace path i j
      (slot, acc') <- prepareMethod constructor place decl acc
      return (MethodDecl slot : shapes, acc')
    prepare _ (_, decl) | isContainer decl =
      rowError "nested container declarations cannot be stored inline"
    prepare (shapes, acc) (_, decl) = return (InlineDecl decl : shapes, acc)
prepareStmt _ KActor{} _ (RouteAttrs names) path i stmt state =
    return
      ( Nothing
      , addAttrInitializerGroup names (InitFragment (InitSuite (StmtPlace path i)) stmt) state
      )
prepareStmt _ KClass{} _ (RouteAttrs names) path i stmt state =
    return
      ( Nothing
      , addAttrInitializerGroup names (InitFragment (InitSuite (StmtPlace path i)) stmt) state
      )
prepareStmt _ KActor{} _ RouteRest path i stmt state =
    return (Nothing, addRestInitializer (InitFragment (InitSuite (StmtPlace path i)) stmt) state)
prepareStmt _ _ _ _ path i stmt@(A.Signature _ names _ A.Property) state
  | null names = rowError "empty property signature"
  | otherwise =
      return (Nothing, addAttrDeclaration names (PlacedStmt (StmtPlace path i) stmt) state)
prepareStmt _ KActor{} _ RouteTop _ _ stmt@A.Signature{} state =
    return (Just (InlineStmt stmt), state)
prepareStmt _ _ _ _ _ _ stmt state = return (Just (InlineStmt stmt), state)

classStmtAttrs :: S.Set A.Name -> A.Stmt -> RowResult [A.Name]
classStmtAttrs attrs stmt@A.Assign{} = assignedAttrs attrs stmt
classStmtAttrs attrs stmt@A.VarAssign{} = assignedAttrs attrs stmt
classStmtAttrs attrs (A.If _ branches elseSuite) = case branches of
    [] -> rowError "class initializer If has no branches"
    _ -> return $ S.toAscList $ S.intersection attrs assigned
  where
    suites = [ body | A.Branch _ body <- branches ] ++ [elseSuite]
    assigned = S.fromList (concatMap Names.assigned suites)
classStmtAttrs _ _ = return []

assignedAttrs :: S.Set A.Name -> A.Stmt -> RowResult [A.Name]
assignedAttrs attrs stmt
  | null selected = return []
  | S.fromList bound == S.fromList selected = return (Env.uniqueNames selected)
  | otherwise = rowError "class assignment mixes attribute and non-attribute bindings"
  where
    bound = Names.bound stmt
    selected = filter (`S.member` attrs) bound

prepareMethod :: Maybe DeclPlace
              -> DeclPlace
              -> A.Decl
              -> PrepareState
              -> RowResult (MethodSlot, PrepareState)
prepareMethod constructor place decl
  | Just place == constructor = prepareConstructorSlot place decl
  | otherwise = prepareOrdinaryMethod place decl

prepareConstructorSlot :: DeclPlace -> A.Decl -> PrepareState -> RowResult (MethodSlot, PrepareState)
prepareConstructorSlot place decl state
  | preparedConstructor state /= Nothing = rowError "multiple constructor slots"
  | otherwise =
      let placed = PlacedMethod place 0 decl
          slot = MethodSlot place (A.dname decl) 0 True (methodHeader decl)
      in return (slot, state { preparedConstructor = Just placed })

prepareOrdinaryMethod :: DeclPlace -> A.Decl -> PrepareState -> RowResult (MethodSlot, PrepareState)
prepareOrdinaryMethod place decl state =
    return (slot, state')
  where
    name = A.dname decl
    ordinal = M.findWithDefault 0 name (methodCounts state)
    placed = PlacedMethod place ordinal decl
    slot = MethodSlot place name ordinal False (methodHeader decl)
    state' = state
      { preparedMethods = M.insertWith (++) name [placed] (preparedMethods state)
      , methodCounts = M.insert name (ordinal + 1) (methodCounts state)
      }

addAttrDeclaration :: [A.Name] -> PlacedStmt -> PrepareState -> PrepareState
addAttrDeclaration names decl state =
    state { preparedAttrs = foldl' add (preparedAttrs state) names }
  where
    group = Env.uniqueNames names
    add attrs name = M.alter (Just . update) name attrs
    update Nothing = AttrAccum group [decl] []
    update (Just old) = old
      { accumNames = Env.uniqueNames (accumNames old ++ group)
      , accumDecls = decl : accumDecls old
      }

ensureAttr :: A.Name -> PrepareState -> PrepareState
ensureAttr name state =
    state
      { preparedAttrs = M.alter (Just . maybe empty id) name (preparedAttrs state) }
  where
    empty = AttrAccum [name] [] []

addAttrInitializer :: A.Name -> InitFragment -> PrepareState -> PrepareState
addAttrInitializer name = addAttrInitializerGroup [name]

addAttrInitializerGroup :: [A.Name] -> InitFragment -> PrepareState -> PrepareState
addAttrInitializerGroup names fragment state =
    state { preparedAttrs = foldl' add (preparedAttrs state) group }
  where
    group = Env.uniqueNames names
    add attrs name = M.alter (Just . update) name attrs
    update Nothing = AttrAccum group [] [fragment]
    update (Just old) = old
      { accumNames = Env.uniqueNames (accumNames old ++ group)
      , accumInits = fragment : accumInits old
      }

addRestInitializer :: InitFragment -> PrepareState -> PrepareState
addRestInitializer fragment state =
    state { preparedRest = fragment : preparedRest state }

prepareConstructor :: Env.Env0 -> A.Suite -> PlacedMethod -> PrepareState -> RowResult PrepareState
prepareConstructor env classSuite placed state = do
    self <- maybe (rowError "__init__ has no self parameter") return (A.selfPar decl)
    let body = A.dbody decl
        (_, prefixLength) = Types.scanInitPrefix env self classSuite body
        indexed = zip [0..] body
        prefix = take prefixLength indexed
        uses = M.fromListWith IntSet.union
          [ (v, IntSet.singleton i)
          | (i, stmt) <- indexed
          , v <- S.toList (S.fromList (Names.free stmt))
          ]
        writeOwners = M.fromListWith S.union
          [ (i, S.singleton attr)
          | (i, stmt) <- prefix
          , attr <- selfAttributes self stmt
          ]
        owners = foldr (ownDefinition uses) writeOwners prefix
        groupIndices = M.fromListWith IntSet.union
          [ (attr, IntSet.singleton i)
          | (i, attrs) <- M.toList owners
          , attr <- S.toList attrs
          ]
        consumed = IntSet.unions (M.elems groupIndices)
        stmtByIndex = M.fromList indexed
        addGroup acc (attr, indices) = foldl'
          (\acc' i -> addAttrInitializer attr (InitFragment (InitBody i) (stmtByIndex M.! i)) acc')
          acc
          (IntSet.toAscList indices)
        withAttrs = foldl' addGroup state (M.toList groupIndices)
        rest =
          [ InitFragment (InitBody i) stmt
          | (i, stmt) <- indexed
          , not (IntSet.member i consumed)
          ]
        storedConstructor = placed
          { placedMethodDecl = decl { A.dbody = [] } }
    return withAttrs
      { preparedRest = reverse rest ++ preparedRest withAttrs
      , preparedConstructor = Just storedConstructor
      }
  where
    decl = placedMethodDecl placed

-- A local definition belongs to the same declarative attribute group as all
-- of its uses, provided it has no use outside already-owned prefix statements.
-- Folding backwards makes ownership flow through chains of local definitions.
ownDefinition :: M.Map A.Name IntSet.IntSet
              -> (Int, A.Stmt)
              -> M.Map Int (S.Set A.Name)
              -> M.Map Int (S.Set A.Name)
ownDefinition uses (i, A.Assign _ [A.PVar _ name _] _) owners
  | not (IntSet.null usedAt)
  , all (> i) (IntSet.toList usedAt)
  , Just attrs <- foldM collect S.empty (IntSet.toList usedAt)
  = M.insert i attrs owners
  where
    usedAt = M.findWithDefault IntSet.empty name uses
    collect attrs used = do
      usedAttrs <- M.lookup used owners
      return (attrs `S.union` usedAttrs)
ownDefinition _ _ owners = owners

selfAttributes :: A.Name -> A.Stmt -> [A.Name]
selfAttributes self stmt = Env.uniqueNames $ case stmt of
    A.MutAssign _ (A.Dot _ (A.Var _ (A.NoQ receiver)) attr) _
      | receiver == self -> [attr]
    A.AugAssign _ (A.Dot _ (A.Var _ (A.NoQ receiver)) attr) _ _
      | receiver == self -> [attr]
    A.If _ branches elseSuite ->
      concat
        [ concatMap (selfAttributes self) branch
        | A.Branch _ branch <- branches
        ] ++
      concatMap (selfAttributes self) elseSuite
    A.While _ _ body elseSuite ->
      concatMap (selfAttributes self) (body ++ elseSuite)
    A.For _ _ _ body elseSuite ->
      concatMap (selfAttributes self) (body ++ elseSuite)
    A.Try _ body handlers elseSuite finallySuite ->
      concatMap (selfAttributes self) body ++
      concat
        [ concatMap (selfAttributes self) handlerBody
        | A.Handler _ handlerBody <- handlers
        ] ++
      concatMap (selfAttributes self) (elseSuite ++ finallySuite)
    A.With _ _ body -> concatMap (selfAttributes self) body
    A.Data _ _ body -> concatMap (selfAttributes self) body
    _ -> []

finishMembers :: PrepareState -> M.Map MemberKey MemberContent
finishMembers state = M.fromList (methods ++ attrs ++ rest)
  where
    methods =
      [ (Method name, MethodContent (sortOn placedMethodOrdinal placed))
      | (name, placed) <- M.toList (preparedMethods state)
      ]
    attrs =
      [ ( Attr name
        , AttrContent
            (accumNames attr)
            (reverse (accumDecls attr))
            (reverse (accumInits attr))
        )
      | (name, attr) <- M.toList (preparedAttrs state)
      ]
    rest
      | preparedConstructor state == Nothing && null (preparedRest state) = []
      | otherwise =
          [ ( InitRest
            , InitRestContent
                (preparedConstructor state)
                (reverse (preparedRest state))
            )
          ]
