-- Copyright (C) 2026 Data Ductus AB
--
-- Parameter escape analysis.  This pass builds a conservative,
-- flow-insensitive value-flow graph for the functions in one typed module and
-- classifies a parameter as MayEscape when it can reach an escape sink. Calls
-- within that module are connected when their target is unambiguous; direct
-- imported calls can use interface summaries.  Dynamically dispatched calls
-- remain conservative unless they have one of the explicitly checked
-- contracts below.  The summaries are still an internal experiment, but the
-- Hashable.hash non-escape contract is enforced because B_hash relies on it
-- for automatic hasher storage.

module Acton.EscapeAnalysis
    ( Escape(..)
    , EscapeReason(..)
    , FunctionId(..)
    , ParameterSummary(..)
    , EscapeReport(..)
    , analyzeModule
    , analyzeModuleWithImports
    , interfaceSummaries
    , renderEscapeReport
    ) where

import Control.Monad (forM_, when)
import Control.Monad.State.Strict
import Data.List (intercalate)
import Data.Maybe (listToMaybe)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Utils
import Acton.Syntax
import Acton.Builtin (mBuiltin, qnHashable)
import Acton.Names (assigned, free, isWitness)


data Escape = NoEscape | MayEscape
    deriving (Eq, Ord, Show)

data EscapeReason
    = Returned SrcLoc
    | Stored SrcLoc
    | Raised SrcLoc
    | Yielded SrcLoc
    | Captured SrcLoc
    | Asynchronous SrcLoc
    | ImportedCall SrcLoc
    | UnknownCall SrcLoc
    | UnavailableBody SrcLoc
    deriving (Eq, Ord, Show)

newtype FunctionId = FunctionId [Name]
    deriving (Eq, Ord, Show)

data ParameterSummary = ParameterSummary
    { parameterName       :: Name
    , parameterEscape     :: Escape
    , parameterContract   :: Bool
    , parameterTrustedNative :: Bool
    , parameterDirectUses :: [EscapeReason]
    } deriving (Eq, Show)

data EscapeReport = EscapeReport
    { reportFunctions          :: [(FunctionId, [ParameterSummary])]
    , reportContractViolations :: [(FunctionId, Name)]
    , reportNodeCount          :: Int
    , reportEdgeCount          :: Int
    , reportImportedSummarized :: Int
    , reportImportedUnknown    :: Int
    , reportImportedNoEscapeArgs :: Int
    , reportImportedMayEscapeArgs :: Int
    } deriving (Eq, Show)


data Parameter = Parameter
    { paramName :: Name
    , paramType :: Maybe Type
    }

data FunctionInfo = FunctionInfo
    { functionId       :: FunctionId
    , functionName     :: Name
    , functionParams   :: [Parameter]
    , functionBody     :: Suite
    , functionContract :: S.Set Name
    , functionTrustedNative :: S.Set Name
    }

data Node = Node FunctionId Name
    deriving (Eq, Ord, Show)

data Facts = Facts
    { factEdges   :: M.Map Node (S.Set Node)
    , factEscapes :: M.Map Node (S.Set EscapeReason)
    , factImportedSummarized :: Int
    , factImportedUnknown :: Int
    , factImportedNoEscapeArgs :: Int
    , factImportedMayEscapeArgs :: Int
    }

emptyFacts :: Facts
emptyFacts = Facts M.empty M.empty 0 0 0 0

data Context = Context
    { contextFunction :: FunctionInfo
    , contextCallees  :: M.Map Name FunctionInfo
    , contextTypes    :: M.Map Name Type
    , contextBound    :: S.Set Name
    , contextImported :: QName -> Maybe [(Name, Escape)]
    }


analyzeModule :: Module -> EscapeReport
analyzeModule = analyzeModuleWithImports (const Nothing)

-- | Analyze a module using summaries for direct calls into imported modules.
-- A missing summary means unknown and is therefore handled conservatively.
analyzeModuleWithImports :: (QName -> Maybe [(Name, Escape)]) -> Module -> EscapeReport
analyzeModuleWithImports imported m = EscapeReport summaries violations nodeCount edgeCount
                                                    (factImportedSummarized facts)
                                                    (factImportedUnknown facts)
                                                    (factImportedNoEscapeArgs facts)
                                                    (factImportedMayEscapeArgs facts)
  where
    functions = collectFunctions (modname m == mBuiltin) [] [] (mbody m)
    callees = uniqueFunctions functions
    -- Witnesses introduced by type reconstruction are not necessarily local
    -- to the function that uses them.  A recursive declaration group may
    -- place a shared binding in one declaration body and use it from another,
    -- so retain those internal bindings in every function context as well as
    -- the parameters and locals below.
    moduleTypes = moduleWitnessTypes (mbody m)
    facts = execState (mapM_ (scanFunction moduleTypes callees imported) functions) emptyFacts
    escaping = escapingNodes facts
    summaries = map (summarize facts escaping) functions
    violations =
        [ (fid, parameterName p)
        | (fid, ps) <- summaries
        , p <- ps
        , parameterContract p
        , parameterEscape p == MayEscape
        ]
    allNodes = S.fromList
        ([ Node (functionId f) (paramName p) | f <- functions, p <- functionParams f ] ++
         M.keys (factEdges facts) ++
         concatMap S.toList (M.elems (factEdges facts)) ++
         M.keys (factEscapes facts))
    nodeCount = S.size allNodes
    edgeCount = sum (map S.size (M.elems (factEdges facts)))

-- | Location-independent representation suitable for an optional interface
-- sidecar. True means MayEscape; absence of a function or parameter is
-- deliberately interpreted as unknown by callers.
interfaceSummaries :: EscapeReport -> [([Name], [(Name, Bool)])]
interfaceSummaries report =
    [ (path, [ (parameterName p, parameterEscape p == MayEscape) | p <- ps ])
    | (FunctionId path, ps) <- reportFunctions report
    ]


renderEscapeReport :: EscapeReport -> String
renderEscapeReport report = unlines (header ++ concatMap renderFunction interestingFunctions ++ contractLines)
  where
    params = concatMap snd (reportFunctions report)
    noesc = length [ () | p <- params, parameterEscape p == NoEscape ]
    mayesc = length params - noesc
    contracts = length [ () | p <- params, parameterContract p ]
    trustedContracts = length [ () | p <- params, parameterTrustedNative p ]
    verifiedContracts = length
        [ ()
        | p <- params
        , parameterContract p
        , not (parameterTrustedNative p)
        , parameterEscape p == NoEscape
        ]
    violatedContracts = length (reportContractViolations report)
    header =
        [ "experimental parameter escape analysis"
        , "functions: " ++ show (length (reportFunctions report)) ++
          ", parameters: " ++ show (length params) ++
          ", noescape: " ++ show noesc ++
          ", mayescape: " ++ show mayesc
        , "graph nodes: " ++ show (reportNodeCount report) ++
          ", edges: " ++ show (reportEdgeCount report)
        , "Hashable.hash contracts: " ++ show contracts ++
          " (verified Acton " ++ show verifiedContracts ++
          ", trusted builtin native " ++ show trustedContracts ++
          ", violations " ++ show violatedContracts ++ ")"
        , "imported direct calls: summarized " ++ show (reportImportedSummarized report) ++
          ", conservative " ++ show (reportImportedUnknown report)
        , "imported summary arguments: noescape " ++ show (reportImportedNoEscapeArgs report) ++
          ", mayescape " ++ show (reportImportedMayEscapeArgs report)
        , "showing functions with a noescape parameter or a Hashable.hash contract: " ++
          show (length interestingFunctions) ++ " of " ++ show (length $ reportFunctions report)
        , ""
        ]
    interestingFunctions =
        [ entry
        | entry@(_,ps) <- reportFunctions report
        , any (\p -> parameterEscape p == NoEscape || parameterContract p) ps
        ]
    renderFunction (fid, ps) =
        (renderFunctionId fid ++ ":") : map (renderParameter "  ") ps ++ [""]
    renderParameter indent p =
        indent ++ nstr (parameterName p) ++ ": " ++ escapeName (parameterEscape p) ++
        contractLabel p ++
        case parameterDirectUses p of
          [] -> ""
          rs -> " {" ++ intercalate ", " (map reasonName rs) ++ "}"
    contractLabel p
      | not (parameterContract p) = ""
      | parameterTrustedNative p = " [Hashable.hash contract: trusted builtin native]"
      | parameterEscape p == NoEscape = " [Hashable.hash contract: verified Acton]"
      | otherwise = " [Hashable.hash contract: violation]"
    escapeName NoEscape = "noescape"
    escapeName MayEscape = "mayescape"
    contractLines =
        summaryLine ++ case reportContractViolations report of
          [] -> []
          bad -> "Hashable.hash contract violations or untrusted native implementations:" :
                 [ "  " ++ renderFunctionId fid ++ "." ++ nstr n | (fid,n) <- bad ]
      where
        summaryLine
          | contracts == 0 = []
          | otherwise =
              [ "Hashable.hash contracts: " ++ show verifiedContracts ++
                " verified from Acton bodies, " ++ show trustedContracts ++
                " trusted builtin native, " ++ show violatedContracts ++ " violations."
              ]

renderFunctionId :: FunctionId -> String
renderFunctionId (FunctionId ns) = intercalate "." (map nstr ns)

reasonName :: EscapeReason -> String
reasonName (Returned _)     = "returned"
reasonName (Stored _)       = "stored"
reasonName (Raised _)       = "raised"
reasonName (Yielded _)      = "yielded"
reasonName (Captured _)     = "captured"
reasonName (Asynchronous _) = "asynchronous"
reasonName (ImportedCall _)  = "escaping imported call"
reasonName (UnknownCall _)  = "unknown call"
reasonName (UnavailableBody _) = "unavailable body"


summarize :: Facts -> S.Set Node -> FunctionInfo -> (FunctionId, [ParameterSummary])
summarize facts escaping f = (functionId f, map one (functionParams f))
  where
    one p = ParameterSummary
        { parameterName = paramName p
        , parameterEscape = if node p `S.member` escaping then MayEscape else NoEscape
        , parameterContract = paramName p `S.member` functionContract f
        , parameterTrustedNative = paramName p `S.member` functionTrustedNative f
        , parameterDirectUses = S.toList $ M.findWithDefault S.empty (node p) (factEscapes facts)
        }
    node p = Node (functionId f) (paramName p)


-- Resolve only unambiguous direct function names.  Methods normally arrive as
-- Dot calls and are handled through protocol contracts instead.
uniqueFunctions :: [FunctionInfo] -> M.Map Name FunctionInfo
uniqueFunctions fs = M.mapMaybe one $ M.fromListWith (++) [ (functionName f, [f]) | f <- fs ]
  where
    one [f] = Just f
    one _   = Nothing


collectFunctions :: Bool -> [Name] -> [TCon] -> Suite -> [FunctionInfo]
collectFunctions trustedBuiltin path parents = concatMap stmt
  where
    stmt (Decl _ ds) = concatMap decl ds
    stmt (If _ bs els) = concatMap (collectFunctions trustedBuiltin path parents . branchBody) bs ++ collectFunctions trustedBuiltin path parents els
    stmt (While _ _ b els) = collectFunctions trustedBuiltin path parents b ++ collectFunctions trustedBuiltin path parents els
    stmt (For _ _ _ b els) = collectFunctions trustedBuiltin path parents b ++ collectFunctions trustedBuiltin path parents els
    stmt (Try _ b hs els fin) = collectFunctions trustedBuiltin path parents b ++ concatMap (collectFunctions trustedBuiltin path parents . handlerBody) hs ++ collectFunctions trustedBuiltin path parents els ++ collectFunctions trustedBuiltin path parents fin
    stmt (With _ _ b) = collectFunctions trustedBuiltin path parents b
    stmt (Data _ _ b) = collectFunctions trustedBuiltin path parents b
    stmt _ = []

    decl d@Def{} = function d : collectFunctions trustedBuiltin (path ++ [dname d]) [] (dbody d)
    decl d@Actor{} = actorFunction d : collectFunctions trustedBuiltin (path ++ [dname d]) [] (dbody d)
    decl (Class _ n _ bs b _) = collectFunctions trustedBuiltin (path ++ [n]) bs b
    decl (Protocol _ n _ bs b _) = collectFunctions trustedBuiltin (path ++ [n]) bs b
    decl (Extension _ _ _ bs b _) = collectFunctions trustedBuiltin (path ++ [extensionSegment bs]) bs b
    decl Typedef{} = []

    function d = FunctionInfo fid (dname d) params (dbody d) contracts trustedContracts
      where
        fid = FunctionId (path ++ [dname d])
        params = parameters (pos d) (kwd d)
        contracts = hashableHasherContract parents d params
        trustedContracts
          | trustedBuiltin && hasNotImpl (dbody d) = contracts
          | otherwise = S.empty

    actorFunction d = FunctionInfo (FunctionId (path ++ [dname d])) (dname d)
                                     (parameters (pos d) (kwd d)) (dbody d) S.empty S.empty

    extensionSegment (p:_) = Name NoLoc ("extension_" ++ nstr (qnameName $ tcname p))
    extensionSegment [] = Name NoLoc "extension"
    branchBody (Branch _ b) = b
    handlerBody (Handler _ b) = b

-- The experiment has one semantic parameter contract. Every implementation
-- of Hashable.hash must not retain its hasher argument. Acton bodies are
-- checked; an opaque implementation is accepted without a body only when it
-- belongs to the compiler-distributed __builtin__ module.
hashableHasherContract :: [TCon] -> Decl -> [Parameter] -> S.Set Name
hashableHasherContract parents d params
          | nstr (dname d) == "hash" && any isHashable parents =
              S.fromList [ paramName p | p <- params, maybe False isHasherType (paramType p) ]
          | otherwise = S.empty

isHashable :: TCon -> Bool
isHashable = (== qnHashable) . tcname

isHasherType :: Type -> Bool
isHasherType (TCon _ tc) = tcname tc == GName mBuiltin (name "hasher")
isHasherType (TOpt _ t) = isHasherType t
isHasherType (TUnboxed _ t) = isHasherType t
isHasherType _ = False

parameters :: PosPar -> KwdPar -> [Parameter]
parameters p k = posParameters p ++ kwdParameters k

posParameters :: PosPar -> [Parameter]
posParameters (PosPar n t _ p) = Parameter n t : posParameters p
posParameters (PosSTAR n t) = [Parameter n t]
posParameters PosNIL = []

kwdParameters :: KwdPar -> [Parameter]
kwdParameters (KwdPar n t _ k) = Parameter n t : kwdParameters k
kwdParameters (KwdSTAR n t) = [Parameter n t]
kwdParameters KwdNIL = []


-- The typed tree records inferred types on assignment patterns.  Retain these
-- for recognizing calls through protocol witnesses and builtin hasher values;
-- relying on a method's spelling alone would make the analysis unsound for an
-- unrelated user protocol that also happened to define `hash`.
localTypes :: Suite -> M.Map Name Type
localTypes = M.unions . map stmtTypes
  where
    stmtTypes (Assign _ ps _) = M.unions (map patternTypes ps)
    stmtTypes (VarAssign _ ps _) = M.unions (map patternTypes ps)
    stmtTypes (If _ bs els) = M.unions (map branchTypes bs ++ [localTypes els])
    stmtTypes (While _ _ b els) = localTypes b `M.union` localTypes els
    stmtTypes (For _ p _ b els) = patternTypes p `M.union` localTypes b `M.union` localTypes els
    stmtTypes (Try _ b hs els fin) = M.unions (localTypes b : map handlerTypes hs ++ [localTypes els, localTypes fin])
    stmtTypes (With _ items b) = M.unions (map itemTypes items ++ [localTypes b])
    stmtTypes (Data _ mbp b) = maybe M.empty patternTypes mbp `M.union` localTypes b
    stmtTypes (Signature _ ns sc _) = M.fromList [ (n, sctype sc) | n <- ns ]
    stmtTypes _ = M.empty

    branchTypes (Branch _ b) = localTypes b
    handlerTypes (Handler _ b) = localTypes b
    itemTypes (WithItem _ mbp) = maybe M.empty patternTypes mbp

-- Witness equations can be shared across an entire recursive declaration
-- group even though their binding is injected into one particular declaration
-- body.  Collect those compiler-generated bindings recursively so another
-- function in the group can still identify the selected protocol.  Do not
-- collect ordinary locals here: source names can collide between functions.
moduleWitnessTypes :: Suite -> M.Map Name Type
moduleWitnessTypes = M.unions . map stmtWitnessTypes
  where
    keepWitnesses = M.filterWithKey (\n _ -> isWitness n)

    stmtWitnessTypes (Assign _ ps _) = keepWitnesses $ M.unions (map patternTypes ps)
    stmtWitnessTypes (VarAssign _ ps _) = keepWitnesses $ M.unions (map patternTypes ps)
    stmtWitnessTypes (If _ bs els) = M.unions (map branchWitnessTypes bs ++ [moduleWitnessTypes els])
    stmtWitnessTypes (While _ _ b els) = moduleWitnessTypes b `M.union` moduleWitnessTypes els
    stmtWitnessTypes (For _ p _ b els) = keepWitnesses (patternTypes p) `M.union`
                                                moduleWitnessTypes b `M.union`
                                                moduleWitnessTypes els
    stmtWitnessTypes (Try _ b hs els fin) = M.unions (moduleWitnessTypes b : map handlerWitnessTypes hs ++
                                                       [moduleWitnessTypes els, moduleWitnessTypes fin])
    stmtWitnessTypes (With _ items b) = M.unions (map itemWitnessTypes items ++ [moduleWitnessTypes b])
    stmtWitnessTypes (Data _ mbp b) = maybe M.empty (keepWitnesses . patternTypes) mbp `M.union`
                                          moduleWitnessTypes b
    stmtWitnessTypes (Signature _ ns sc _) = M.fromList
        [ (n, sctype sc) | n <- ns, isWitness n ]
    stmtWitnessTypes (Decl _ ds) = M.unions (map declWitnessTypes ds)
    stmtWitnessTypes _ = M.empty

    branchWitnessTypes (Branch _ b) = moduleWitnessTypes b
    handlerWitnessTypes (Handler _ b) = moduleWitnessTypes b
    itemWitnessTypes (WithItem _ mbp) = maybe M.empty (keepWitnesses . patternTypes) mbp

    declWitnessTypes d@Def{} = moduleWitnessTypes (dbody d)
    declWitnessTypes d@Actor{} = moduleWitnessTypes (dbody d)
    declWitnessTypes (Class _ _ _ _ b _) = moduleWitnessTypes b
    declWitnessTypes (Protocol _ _ _ _ b _) = moduleWitnessTypes b
    declWitnessTypes (Extension _ _ _ _ b _) = moduleWitnessTypes b
    declWitnessTypes Typedef{} = M.empty

patternTypes :: Pattern -> M.Map Name Type
patternTypes (PWild _ _) = M.empty
patternTypes (PVar _ n mt) = maybe M.empty (M.singleton n) mt
patternTypes (PParen _ p) = patternTypes p
patternTypes (PTuple _ p k) = posPatternTypes p `M.union` kwdPatternTypes k
patternTypes (PList _ ps mbp) = M.unions (map patternTypes ps ++ [maybe M.empty patternTypes mbp])
patternTypes (PData _ _ _) = M.empty

posPatternTypes :: PosPat -> M.Map Name Type
posPatternTypes PosPatNil = M.empty
posPatternTypes (PosPat p ps) = patternTypes p `M.union` posPatternTypes ps
posPatternTypes (PosPatStar p) = patternTypes p

kwdPatternTypes :: KwdPat -> M.Map Name Type
kwdPatternTypes KwdPatNil = M.empty
kwdPatternTypes (KwdPat _ p ps) = patternTypes p `M.union` kwdPatternTypes ps
kwdPatternTypes (KwdPatStar p) = patternTypes p


scanFunction :: M.Map Name Type -> M.Map Name FunctionInfo -> (QName -> Maybe [(Name, Escape)]) -> FunctionInfo -> State Facts ()
scanFunction moduleTypes callees imported f = scanSuite (Context f callees types boundNames imported) (functionBody f)
  where
    types = M.fromList [ (paramName p,t) | p <- functionParams f, Just t <- [paramType p] ] `M.union`
            localTypes (functionBody f) `M.union`
            moduleTypes
    -- Acton resolves every assignment in a function as a local binding.  A
    -- parameter or assigned name can therefore shadow a same-named top-level
    -- or imported function and must not be resolved as that function here.
    boundNames = S.fromList (map paramName (functionParams f) ++ assigned (functionBody f))

scanSuite :: Context -> Suite -> State Facts ()
scanSuite c = mapM_ (scanStmt c)

scanStmt :: Context -> Stmt -> State Facts ()
scanStmt c stmt = case stmt of
    Expr l (NotImplemented _) -> markUnavailable c l
    Assign l _ (NotImplemented _) -> markUnavailable c l
    Expr _ e -> scanExpr c e
    Assign _ ps e -> scanExpr c e >> bindPatterns c ps e
    VarAssign _ ps e -> scanExpr c e >> bindPatterns c ps e
    MutAssign l target e -> do
        scanExpr c target
        scanExpr c e
        case localTarget target of
          Just n -> addFlows c e n
          Nothing -> addEscapeExpr c (Stored l) e
    AugAssign l target _ e -> do
        scanExpr c target
        scanExpr c e
        case localTarget target of
          Just n -> addFlows c e n
          Nothing -> addEscapeExpr c (Stored l) e
    Assert _ e mbe -> scanExpr c e >> mapM_ (scanExpr c) mbe
    Pass{} -> return ()
    Delete _ e -> scanExpr c e
    Return l mbe -> forM_ mbe $ \e -> scanExpr c e >> addEscapeExpr c (Returned l) e
    Raise l e -> scanExpr c e >> addEscapeExpr c (Raised l) e
    Break{} -> return ()
    Continue{} -> return ()
    If _ bs els -> mapM_ (scanBranch c) bs >> scanSuite c els
    While _ e b els -> scanExpr c e >> scanSuite c b >> scanSuite c els
    For _ p e b els -> do
        scanExpr c e
        bindPattern c p e
        scanSuite c b
        scanSuite c els
    Try _ b hs els fin -> scanSuite c b >> mapM_ (scanHandler c) hs >> scanSuite c els >> scanSuite c fin
    With l items b -> do
        forM_ items $ \(WithItem e mbp) -> do
            scanExpr c e
            addEscapeExpr c (UnknownCall l) e
            mapM_ (\p -> bindPattern c p e) mbp
        scanSuite c b
    Data _ mbp b -> mapM_ (\p -> bindPattern c p (Tuple NoLoc PosNil KwdNil)) mbp >> scanSuite c b
    After l _ e e' -> do
        scanExpr c e
        scanExpr c e'
        addEscapeNames c (Asynchronous l) (free e ++ free e')
    Signature{} -> return ()
    Decl l ds -> forM_ ds $ \d ->
        case d of
          Def{} -> addEscapeNames c (Captured l) (free d)
          Actor{} -> addEscapeNames c (Captured l) (free d)
          _ -> return ()

markUnavailable :: Context -> SrcLoc -> State Facts ()
markUnavailable c l =
    addEscapeNames c (UnavailableBody l) untrusted
  where
    f = contextFunction c
    untrusted =
        [ paramName p
        | p <- functionParams f
        , paramName p `S.notMember` functionTrustedNative f
        ]

scanBranch :: Context -> Branch -> State Facts ()
scanBranch c (Branch e b) = scanExpr c e >> scanSuite c b

scanHandler :: Context -> Handler -> State Facts ()
scanHandler c (Handler _ b) = scanSuite c b


scanExpr :: Context -> Expr -> State Facts ()
scanExpr c expression = case expression of
    Var{} -> return ()
    Int{} -> return ()
    Float{} -> return ()
    Imaginary{} -> return ()
    Bool{} -> return ()
    None{} -> return ()
    NotImplemented{} -> return ()
    Ellipsis{} -> return ()
    Strings{} -> return ()
    BStrings{} -> return ()
    Call l f p k -> do
        scanExpr c f
        mapM_ (scanExpr c) (posArgExpressions p)
        mapM_ (scanExpr c . snd) (kwdArgExpressions k)
        scanCall c l f p k
    Let _ ss e -> scanSuite c ss >> scanExpr c e
    TApp _ e _ -> scanExpr c e
    Async l e -> scanExpr c e >> addEscapeNames c (Asynchronous l) (free e)
    Await _ e -> scanExpr c e
    Index _ e i -> scanExpr c e >> scanExpr c i
    Slice _ e s -> scanExpr c e >> scanSliz c s
    Cond _ e test e' -> scanExpr c e >> scanExpr c test >> scanExpr c e'
    IsInstance _ e _ -> scanExpr c e
    BinOp _ e _ e' -> scanExpr c e >> scanExpr c e'
    CompOp _ e ops -> scanExpr c e >> mapM_ (scanOpArg c) ops
    UnOp _ _ e -> scanExpr c e
    Dot _ e _ -> scanExpr c e
    Rest _ e _ -> scanExpr c e
    DotI _ e _ -> scanExpr c e
    RestI _ e _ -> scanExpr c e
    Opt _ e _ -> scanExpr c e
    OptChain _ e -> scanExpr c e
    Lambda l p k e _ -> do
        let captures = free e `without` (parameterNames p k)
        addEscapeNames c (Captured l) captures
    Yield l mbe -> forM_ mbe $ \e -> scanExpr c e >> addEscapeExpr c (Yielded l) e
    YieldFrom l e -> scanExpr c e >> addEscapeExpr c (Yielded l) e
    Tuple _ p k -> mapM_ (scanExpr c) (posArgExpressions p) >> mapM_ (scanExpr c . snd) (kwdArgExpressions k)
    List _ es -> mapM_ (scanElem c) es
    ListComp _ e comp -> scanElem c e >> scanComp c comp
    Dict _ as -> mapM_ (scanAssoc c) as
    DictComp _ a comp -> scanAssoc c a >> scanComp c comp
    Set _ es -> mapM_ (scanElem c) es
    SetComp _ e comp -> scanElem c e >> scanComp c comp
    GeneratorExpr l e comp -> do
        scanElem c e
        scanComp c comp
        addEscapeNames c (Captured l) (free expression)
    Paren _ e -> scanExpr c e
    Box _ e -> scanExpr c e
    UnBox _ e -> scanExpr c e

scanElem :: Context -> Elem -> State Facts ()
scanElem c (Elem e) = scanExpr c e
scanElem c (Star e) = scanExpr c e

scanAssoc :: Context -> Assoc -> State Facts ()
scanAssoc c (Assoc k v) = scanExpr c k >> scanExpr c v
scanAssoc c (StarStar e) = scanExpr c e

scanOpArg :: Context -> OpArg -> State Facts ()
scanOpArg c (OpArg _ e) = scanExpr c e

scanSliz :: Context -> Sliz -> State Facts ()
scanSliz c (Sliz _ a b d) = mapM_ (scanExpr c) a >> mapM_ (scanExpr c) b >> mapM_ (scanExpr c) d

scanComp :: Context -> Comp -> State Facts ()
scanComp _ NoComp = return ()
scanComp c (CompFor _ p e rest) = scanExpr c e >> bindPattern c p e >> scanComp c rest
scanComp c (CompIf _ e rest) = scanExpr c e >> scanComp c rest


scanCall :: Context -> SrcLoc -> Expr -> PosArg -> KwdArg -> State Facts ()
scanCall c l f p k
  | Just callee <- directCallee c target = connectCall c l callee p k
  | Just summary <- importedCallee c target =
      countImportedSummarized >> connectImportedCall c l summary p k
  | Just (receiver, attr) <- methodTarget target,
    nstr attr == "hash", isHashableReceiver c receiver = hashCall
  | Just (receiver, attr) <- methodTarget target,
    nstr attr `elem` ["update", "finalize"], isHasherReceiver c receiver = return ()
  | Just _ <- importedTarget target = countImportedUnknown >> unknownCall
  | otherwise = unknownCall
  where
    target = unwrapCallTarget f
    pos = posArgExpressions p
    kwd = kwdArgExpressions k

    hashCall = do
        -- The final explicit argument of Hashable.hash is the hasher.  All
        -- other arguments remain conservative; only the hasher contract is
        -- assumed for this experiment.
        mapM_ (addEscapeExpr c (UnknownCall l)) (safeInit pos)
        mapM_ (\(n,e) -> if nstr n == "h" then return () else addEscapeExpr c (UnknownCall l) e) kwd

    unknownCall = do
        mapM_ (addEscapeExpr c (UnknownCall l)) pos
        mapM_ (addEscapeExpr c (UnknownCall l) . snd) kwd
        case methodTarget target of
          Just (receiver, _) -> addEscapeExpr c (UnknownCall l) receiver
          Nothing -> return ()

    countImportedSummarized = modify' $ \facts -> facts
        { factImportedSummarized = factImportedSummarized facts + 1 }
    countImportedUnknown = modify' $ \facts -> facts
        { factImportedUnknown = factImportedUnknown facts + 1 }

safeInit :: [a] -> [a]
safeInit [] = []
safeInit xs = init xs

connectCall :: Context -> SrcLoc -> FunctionInfo -> PosArg -> KwdArg -> State Facts ()
connectCall c l callee p k = do
    let ps = functionParams callee
        positional = posArgExpressions p
        paired = zip positional ps
    forM_ paired $ \(e,param) -> addCallFlows c e callee (paramName param)
    forM_ (drop (length ps) positional) $ \e -> addEscapeExpr c (UnknownCall (loc e)) e
    forM_ (kwdArgExpressions k) $ \(n,e) ->
        case listToMaybe [ param | param <- ps, paramName param == n ] of
          Just param -> addCallFlows c e callee (paramName param)
          Nothing -> addEscapeExpr c (UnknownCall (loc e)) e

connectImportedCall :: Context -> SrcLoc -> [(Name, Escape)] -> PosArg -> KwdArg -> State Facts ()
connectImportedCall c l summary p k = do
    let positional = posArgExpressions p
        paired = zip positional summary
        applySummary (e,(_,status)) = do
          countImportedArgument status
          when (status == MayEscape) (addEscapeExpr c (ImportedCall l) e)
    mapM_ applySummary paired
    -- An arity mismatch should not occur after type checking, but treating any
    -- excess conservatively keeps corrupt or partial summaries harmless.
    mapM_ (addEscapeExpr c (UnknownCall l)) (drop (length summary) positional)
    forM_ (kwdArgExpressions k) $ \(n,e) ->
        case lookup n summary of
          Just NoEscape -> countImportedArgument NoEscape
          Just MayEscape -> countImportedArgument MayEscape >> addEscapeExpr c (ImportedCall l) e
          Nothing -> addEscapeExpr c (UnknownCall l) e

countImportedArgument :: Escape -> State Facts ()
countImportedArgument status = modify' $ \facts ->
    case status of
      NoEscape -> facts { factImportedNoEscapeArgs = factImportedNoEscapeArgs facts + 1 }
      MayEscape -> facts { factImportedMayEscapeArgs = factImportedMayEscapeArgs facts + 1 }

directCallee :: Context -> Expr -> Maybe FunctionInfo
directCallee c (Var _ (NoQ n))
  | n `S.notMember` contextBound c = M.lookup n (contextCallees c)
directCallee _ _ = Nothing

importedCallee :: Context -> Expr -> Maybe [(Name, Escape)]
-- Unqualified imported names remain NoQ in the typed tree.  The caller owns
-- the environment needed to distinguish such an alias from an ordinary
-- local, so offer every Var to the lookup after local direct-call resolution
-- has had first refusal.
importedCallee c (Var _ qn@(NoQ n))
  | n `S.notMember` contextBound c = contextImported c qn
  | otherwise = Nothing
importedCallee c (Var _ qn) = contextImported c qn
importedCallee _ _ = Nothing

importedTarget :: Expr -> Maybe QName
importedTarget (Var _ qn@GName{}) = Just qn
importedTarget (Var _ qn@QName{}) = Just qn
importedTarget _ = Nothing

methodTarget :: Expr -> Maybe (Expr, Name)
methodTarget (Dot _ receiver attr) = Just (receiver, attr)
methodTarget _ = Nothing

unwrapCallTarget :: Expr -> Expr
unwrapCallTarget (TApp _ e _) = unwrapCallTarget e
unwrapCallTarget (Paren _ e) = unwrapCallTarget e
unwrapCallTarget (Call _ (Var _ qn) (PosArg e PosNil) KwdNil)
  | isWitness (qnameName qn) = unwrapCallTarget e
unwrapCallTarget e = e

isHashableReceiver :: Context -> Expr -> Bool
isHashableReceiver c receiver = any isHashableName (S.toList $ aliasNames receiver)
  where
    isHashableName n = maybe False isHashableType $ M.lookup n (contextTypes c)

isHashableType :: Type -> Bool
isHashableType (TCon _ tc) = tcname tc == qnHashable
isHashableType (TOpt _ t) = isHashableType t
isHashableType _ = False

isHasherReceiver :: Context -> Expr -> Bool
isHasherReceiver c e = any isHasherName (S.toList $ aliasNames e)
  where
    isHasherName n = maybe False isHasherType $ M.lookup n (contextTypes c)


bindPatterns :: Context -> [Pattern] -> Expr -> State Facts ()
bindPatterns c ps e = mapM_ (\p -> bindPattern c p e) ps

bindPattern :: Context -> Pattern -> Expr -> State Facts ()
bindPattern c p e = forM_ (patternNames p) (addFlows c e)

patternNames :: Pattern -> [Name]
patternNames (PWild _ _) = []
patternNames (PVar _ n _) = [n]
patternNames (PParen _ p) = patternNames p
patternNames (PTuple _ p k) = posPatternNames p ++ kwdPatternNames k
patternNames (PList _ ps mbp) = concatMap patternNames ps ++ maybe [] patternNames mbp
patternNames (PData _ n _) = [n]

posPatternNames :: PosPat -> [Name]
posPatternNames PosPatNil = []
posPatternNames (PosPat p ps) = patternNames p ++ posPatternNames ps
posPatternNames (PosPatStar p) = patternNames p

kwdPatternNames :: KwdPat -> [Name]
kwdPatternNames KwdPatNil = []
kwdPatternNames (KwdPat _ p ps) = patternNames p ++ kwdPatternNames ps
kwdPatternNames (KwdPatStar p) = patternNames p

localTarget :: Expr -> Maybe Name
localTarget (Var _ (NoQ n)) = Just n
localTarget (Paren _ e) = localTarget e
localTarget _ = Nothing


addFlows :: Context -> Expr -> Name -> State Facts ()
addFlows c e dst = forM_ (S.toList $ aliasNames e) $ \src -> addEdge (localNode c src) (localNode c dst)

addCallFlows :: Context -> Expr -> FunctionInfo -> Name -> State Facts ()
addCallFlows c e callee dst = forM_ (S.toList $ aliasNames e) $ \src ->
    addEdge (localNode c src) (Node (functionId callee) dst)

addEscapeExpr :: Context -> EscapeReason -> Expr -> State Facts ()
addEscapeExpr c reason e = addEscapeNames c reason (S.toList $ aliasNames e)

addEscapeNames :: Context -> EscapeReason -> [Name] -> State Facts ()
addEscapeNames c reason = mapM_ (\n -> addSink (localNode c n) reason)

localNode :: Context -> Name -> Node
localNode c = Node (functionId $ contextFunction c)

addEdge :: Node -> Node -> State Facts ()
addEdge from to = modify' $ \facts -> facts
    { factEdges = M.insertWith S.union from (S.singleton to) (factEdges facts) }

addSink :: Node -> EscapeReason -> State Facts ()
addSink n reason = modify' $ \facts -> facts
    { factEscapes = M.insertWith S.union n (S.singleton reason) (factEscapes facts) }


-- Names whose referenced value, or a value contained in it, may be represented
-- by the expression.  This is intentionally more conservative than ordinary
-- alias analysis for projections and containers.
aliasNames :: Expr -> S.Set Name
aliasNames expression = case expression of
    Var _ (NoQ n) -> S.singleton n
    Var{} -> S.empty
    Call{} -> S.empty
    Let _ _ e -> aliasNames e
    TApp _ e _ -> aliasNames e
    Async{} -> S.empty
    Await{} -> S.empty
    Index _ e _ -> aliasNames e
    Slice _ e _ -> aliasNames e
    Cond _ e _ e' -> aliasNames e `S.union` aliasNames e'
    IsInstance{} -> S.empty
    BinOp{} -> S.empty
    CompOp{} -> S.empty
    UnOp{} -> S.empty
    Dot _ e _ -> aliasNames e
    Rest _ e _ -> aliasNames e
    DotI _ e _ -> aliasNames e
    RestI _ e _ -> aliasNames e
    Opt _ e _ -> aliasNames e
    OptChain _ e -> aliasNames e
    Lambda{} -> S.fromList (free expression)
    Yield _ mbe -> maybe S.empty aliasNames mbe
    YieldFrom _ e -> aliasNames e
    Tuple _ p k -> aliases (posArgExpressions p ++ map snd (kwdArgExpressions k))
    List _ es -> S.unions (map elemAliases es)
    ListComp{} -> S.fromList (free expression)
    Dict _ as -> S.unions (map assocAliases as)
    DictComp{} -> S.fromList (free expression)
    Set _ es -> S.unions (map elemAliases es)
    SetComp{} -> S.fromList (free expression)
    GeneratorExpr{} -> S.fromList (free expression)
    Paren _ e -> aliasNames e
    Box _ e -> aliasNames e
    UnBox _ e -> aliasNames e
    _ -> S.empty
  where
    aliases = S.unions . map aliasNames
    elemAliases (Elem e) = aliasNames e
    elemAliases (Star e) = aliasNames e
    assocAliases (Assoc k v) = aliasNames k `S.union` aliasNames v
    assocAliases (StarStar e) = aliasNames e


escapingNodes :: Facts -> S.Set Node
escapingNodes facts = go initial initial
  where
    initial = M.keysSet (factEscapes facts)
    reverseEdges = M.fromListWith S.union
        [ (to, S.singleton from)
        | (from,tos) <- M.toList (factEdges facts)
        , to <- S.toList tos
        ]
    go seen work
      | S.null work = seen
      | otherwise =
          let (n,rest) = S.deleteFindMin work
              predecessors = M.findWithDefault S.empty n reverseEdges
              fresh = predecessors `S.difference` seen
          in go (seen `S.union` fresh) (rest `S.union` fresh)


posArgExpressions :: PosArg -> [Expr]
posArgExpressions PosNil = []
posArgExpressions (PosArg e p) = e : posArgExpressions p
posArgExpressions (PosStar e) = [e]

kwdArgExpressions :: KwdArg -> [(Name, Expr)]
kwdArgExpressions KwdNil = []
kwdArgExpressions (KwdArg n e k) = (n,e) : kwdArgExpressions k
kwdArgExpressions (KwdStar e) = [(Name NoLoc "**", e)]

parameterNames :: PosPar -> KwdPar -> [Name]
parameterNames p k = map paramName (parameters p k)

qnameName :: QName -> Name
qnameName (NoQ n) = n
qnameName (QName _ n) = n
qnameName (GName _ n) = n

without :: Ord a => [a] -> [a] -> [a]
without xs ys = filter (`S.notMember` excluded) xs
  where excluded = S.fromList ys
