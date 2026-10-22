-- Copyright (C) 2026 Acton contributors
-- SPDX-License-Identifier: BSD-3-Clause

{-# LANGUAGE OverloadedStrings #-}
-- A description graph, not an object cache. Canonical recipe IDs say nothing
-- about where a construction dominates its uses or when it may be evaluated.
module Acton.WitnessRegistry
    ( WitnessRegistry(..), Recipe(..), RecipeKind(..), Lifetime(..)
    , Capture(..), CaptureKind(..), Binding(..), Site(..), Use(..)
    , buildRegistry, buildFunctionRegistry, registrySummary
    ) where

import Control.Monad.State.Strict
import Control.Monad (foldM, forM, forM_)
import Data.Aeson (Value, object, (.=))
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Acton.Syntax hiding (expr, comp, target)
import Acton.Names (isWitness, bound)
import Acton.Subst (vfree, vsubst)
import Acton.WitnessKey (expressionHash)
import Utils (SrcLoc, prstr)

data Lifetime = Closed | ParameterDependent | LocalDependent | Unknown
    deriving (Eq, Ord, Show)

data CaptureKind = WitnessParameter | TypeParameter | LocalValue | Unresolved
    deriving (Eq, Ord, Show)

-- Binding identities distinguish equal spellings in different activations,
-- shadowed type variables, and successive assignments to a local variable.
data Capture = Capture
    { captureId :: Int, captureScope :: Int, captureName :: Name
    , captureKind :: CaptureKind
    } deriving (Eq, Ord, Show)

data RecipeKind = Constructor QName | Projection Name | Instantiation
                | RuntimeWitness | Opaque
    deriving (Eq, Show)

data Recipe = Recipe
    { recipeId :: Int, recipeKind :: RecipeKind, recipeExpression :: Expr
    , recipeDependencies :: S.Set Int, recipeCaptures :: S.Set Capture
    , recipeLifetime :: Lifetime
    } deriving (Show)

data Binding = Binding
    { bindingName :: Name, bindingType :: Maybe Type, bindingRecipe :: Int
    , bindingScope :: Int, bindingLocation :: SrcLoc
    } deriving (Show)

data Site = Site
    { siteRecipe :: Int, siteScope :: Int, siteLoopDepth :: Int
    , siteLocation :: SrcLoc, siteStatic :: Bool, siteExpression :: Expr
    } deriving (Show)

data Use = Use
    { useRecipe :: Int, useScope :: Int, useLocation :: SrcLoc
    } deriving (Show)

data WitnessRegistry = WitnessRegistry
    { registryRecipes :: IM.IntMap Recipe, registryBindings :: [Binding]
    , registrySites :: [Site], registryUses :: [Use], registryScopes :: Int
    } deriving (Show)

data Term = WitnessValue Int | CapturedValue Capture
data Lex = Lex
    { scope :: Int, depth :: Int, terms :: M.Map Name Term
    , types :: M.Map Name Capture
    }
data Build = Build
    { graph :: WitnessRegistry, buckets :: IM.IntMap [Int], serial :: Int
    , constructor :: QName -> Maybe QName, staticRecipe :: Expr -> Bool
    }
type BuildM = State Build

-- The caller supplies resolved implementation identities from the original
-- extension environment, including primitive/generated witness constructors.
-- Types stay on binding records. Constructor keys retain their explicit type
-- applications; type variables and dependency witnesses have binding identities.
buildRegistry :: (QName -> Maybe QName) -> (Expr -> Bool) -> Module -> WitnessRegistry
buildRegistry resolve isStatic m = build resolve isStatic $ suite initialLex (mbody m) >> return ()

-- Scope 1 is the function itself. Nested declarations/lambdas get other IDs;
-- their parameters and construction sites remain separate from this frame.
buildFunctionRegistry :: (QName -> Maybe QName) -> (Expr -> Bool) -> Decl -> WitnessRegistry
buildFunctionRegistry resolve isStatic d = build resolve isStatic $ decl initialLex d

initialLex :: Lex
initialLex = Lex 0 0 M.empty M.empty

build :: (QName -> Maybe QName) -> (Expr -> Bool) -> BuildM () -> WitnessRegistry
build resolve isStatic action = finish $ execState action initial
  where
    initial = Build (WitnessRegistry IM.empty [] [] [] 1) IM.empty 0 resolve isStatic
    finish s = let g = graph s in g
        { registryBindings = reverse (registryBindings g)
        , registrySites = reverse (registrySites g)
        , registryUses = reverse (registryUses g) }

fresh :: BuildM Int
fresh = do
    s <- get
    let n = serial s + 1
    put s{serial = n}
    return n

modifyGraph :: (WitnessRegistry -> WitnessRegistry) -> BuildM ()
modifyGraph f = modify $ \s -> s{graph = f (graph s)}

newScope :: Lex -> BuildM Lex
newScope env = do
    n <- fresh
    modifyGraph $ \g -> g{registryScopes = registryScopes g + 1}
    return env{scope = n, depth = 0}

capture :: Lex -> CaptureKind -> Name -> BuildM Capture
capture env k n = do i <- fresh; return (Capture i (scope env) n k)

bindValues :: Lex -> [Name] -> BuildM Lex
bindValues = foldM $ \env n -> do
    c <- capture env LocalValue n
    return env{terms = M.insert n (CapturedValue c) (terms env)}

bindParameters :: Lex -> [Name] -> BuildM Lex
bindParameters = foldM $ \env n -> do
    c <- capture env (if isWitness n then WitnessParameter else LocalValue) n
    return env{terms = M.insert n (CapturedValue c) (terms env)}

bindTypes :: Lex -> QBinds -> BuildM Lex
bindTypes = foldM $ \env (QBind (TV _ n) _) -> do
    c <- capture env TypeParameter n
    return env{types = M.insert n c (types env)}

life :: S.Set Capture -> Lifetime
life = S.foldl' (\a c -> max a $ case captureKind c of
    WitnessParameter -> ParameterDependent
    TypeParameter -> ParameterDependent
    LocalValue -> LocalDependent
    Unresolved -> Unknown) Closed

ref :: Int -> Expr
ref = eVar . Internal Witness "registry-recipe"

valueRef :: Capture -> Name
valueRef = Internal Witness "registry-binding" . captureId

-- Only closed and parameter-dependent descriptions are canonicalized. Local
-- data and unsupported computations may change between occurrences; keeping
-- them separate avoids implying an equivalence this stage has not proved.
intern :: RecipeKind -> Expr -> S.Set Int -> S.Set Capture -> BuildM Int
intern kind e deps caps = do
    s <- get
    let candidates = IM.findWithDefault [] (expressionHash e) (buckets s)
        reusable = life caps <= ParameterDependent
        matches i = let r = registryRecipes (graph s) IM.! i
                    in recipeKind r == kind && recipeExpression r == e
        existing = if reusable then filter matches candidates else []
    case existing of
      i:_ -> return i
      _ -> do
        i <- fresh
        let r = Recipe i kind e deps caps (life caps)
        modifyGraph $ \g -> g{registryRecipes = IM.insert i r (registryRecipes g)}
        -- Unsupported expressions share a fingerprint bucket. They are never
        -- candidates for reuse, so neither scan nor populate that bucket.
        if reusable
          then modify $ \b -> b{buckets = IM.insertWith (++) (expressionHash e) [i] (buckets b)}
          else return ()
        return i

data Resolved = Resolved Expr (S.Set Int) (S.Set Capture) (Maybe Int)

canonical :: Resolved -> Expr
canonical (Resolved e _ _ _) = e
dependencies :: Resolved -> S.Set Int
dependencies (Resolved _ ds _ _) = ds
captures :: Resolved -> S.Set Capture
captures (Resolved _ _ cs _) = cs
node :: Resolved -> Maybe Int
node (Resolved _ _ _ n) = n
combined :: [Resolved] -> (S.Set Int, S.Set Capture)
combined es = (S.unions (map dependencies es), S.unions (map captures es))

fromNode :: Int -> BuildM Resolved
fromNode i = do
    r <- gets ((IM.! i) . registryRecipes . graph)
    return (Resolved (ref i) (S.singleton i) (recipeCaptures r) (Just i))

makeNode :: RecipeKind -> Expr -> [Resolved] -> S.Set Capture -> BuildM Resolved
makeNode kind e es extra = do
    let (ds,cs) = combined es
    i <- intern kind e ds (S.union cs extra)
    fromNode i

typeArgs :: Lex -> [Type] -> BuildM ([Type], S.Set Capture)
typeArgs env ts = do
    pairs <- forM (S.toList $ S.fromList $ vfree ts) $ \v@(TV k n) -> do
        c <- maybe (capture env Unresolved n) return (M.lookup n (types env))
        return (v, tVar (TV k (valueRef c)), c)
    return (vsubst [(v,t) | (v,t,_) <- pairs] ts, S.fromList [c | (_,_,c) <- pairs])

headName :: Expr -> Maybe QName
headName (Var _ q) = Just q
headName (TApp _ e _) = headName e
headName (Paren _ e) = headName e
headName _ = Nothing

constructorHead :: Lex -> Expr -> BuildM (Maybe QName)
constructorHead env f = case headName f of
    Nothing -> return Nothing
    Just (NoQ n) | M.member n (terms env) -> return Nothing
    Just q -> gets (($ q) . constructor)

constructorFun :: Lex -> QName -> Expr -> BuildM (Expr, S.Set Capture)
constructorFun env q (TApp l f ts) = do
    (f',cs) <- constructorFun env q f
    (ts',cs') <- typeArgs env ts
    return (TApp l f' ts', S.union cs cs')
constructorFun env q (Paren _ f) = constructorFun env q f
constructorFun _ q _ = return (eQVar q, S.empty)

expr :: Lex -> Expr -> BuildM Resolved
expr env e@(Var l (NoQ n)) = case M.lookup n (terms env) of
    Just (WitnessValue i) -> do
        modifyGraph $ \g -> g{registryUses = Use i (scope env) l : registryUses g}
        fromNode i
    Just (CapturedValue c) | isWitness n -> do
        r <- makeNode RuntimeWitness (eVar (valueRef c)) [] (S.singleton c)
        forM_ (node r) $ \i -> modifyGraph $ \g -> g{registryUses = Use i (scope env) l : registryUses g}
        return r
    Just (CapturedValue c) -> return (Resolved (eVar (valueRef c)) S.empty (S.singleton c) Nothing)
    Nothing -> do
        c <- capture env Unresolved n
        return (Resolved e S.empty (S.singleton c) Nothing)
expr env e@Var{} = do
    c <- capture env Unresolved (noq $ var e)
    return (Resolved e S.empty (S.singleton c) Nothing)
expr env e@(Call l f p k) = do
    ctor <- constructorHead env f
    (p',ps) <- positional env p
    (k',ks) <- keywords env k
    case ctor of
      Just q -> do
        (f',cs) <- constructorFun env q f
        r <- makeNode (Constructor q) (Call l f' p' k') (ps++ks) cs
        let i = case node r of Just i -> i; Nothing -> error "registry: constructor without node"
        isStatic <- gets (($ e) . staticRecipe)
        modifyGraph $ \g -> g{registrySites = Site i (scope env) (depth env) l isStatic e : registrySites g}
        return r
      Nothing -> do
        f' <- expr env f
        c <- capture env Unresolved (name "call")
        let (ds,cs) = combined (f':ps++ks)
        return (Resolved (Call l (canonical f') p' k') ds (S.insert c cs) Nothing)
expr env (Paren _ e) = expr env e
expr env (TApp l e ts) = do
    r <- expr env e
    (ts',cs) <- typeArgs env ts
    case node r of
      Just _ -> makeNode Instantiation (TApp l (canonical r) ts') [r] cs
      Nothing -> return (Resolved (TApp l (canonical r) ts') (dependencies r) (S.union cs (captures r)) Nothing)
expr env e@(Dot l x n) = do
    r <- expr env x
    case node r of
      Just _ -> makeNode (Projection n) (Dot l (canonical r) n) [r] S.empty
      Nothing -> opaque env e [r]
expr env e@(Lambda _ p k x _) = do
    inner <- newScope env >>= (\a -> bindParameters a (bound p ++ bound k))
    r <- expr inner x
    opaque env e [r]
expr env e@(Let _ b x) = do
    inner <- suite env b
    r <- expr inner x
    opaque env e [r]
expr env e@(ListComp _ x c) = comprehension env e [element x] c
expr env e@(SetComp _ x c) = comprehension env e [element x] c
expr env e@(GeneratorExpr _ x c) = comprehension env e [element x] c
expr env e@(DictComp _ a c) = comprehension env e (association a) c
expr env e = do
    rs <- mapM (expr env) (children e)
    case e of
      Int{} -> literal
      Float{} -> literal
      Bool{} -> literal
      None{} -> literal
      Strings{} -> literal
      BStrings{} -> literal
      Tuple _ p k -> do
        (p',ps) <- positional env p
        (k',ks) <- keywords env k
        let (ds,cs) = combined (ps++ks)
        return (Resolved (Tuple (eloc e) p' k') ds cs Nothing)
      _ -> opaque env e rs
  where literal = return (Resolved e S.empty S.empty Nothing)

opaque :: Lex -> Expr -> [Resolved] -> BuildM Resolved
opaque env e rs = do
    c <- capture env Unresolved (name "expression")
    let (ds,cs) = combined rs
    return (Resolved e ds (S.insert c cs) Nothing)

positional :: Lex -> PosArg -> BuildM (PosArg, [Resolved])
positional env (PosArg e p) = do
    r <- expr env e; (p',rs) <- positional env p
    return (PosArg (canonical r) p',r:rs)
positional env (PosStar e) = do r <- expr env e; return (PosStar (canonical r),[r])
positional _ PosNil = return (PosNil,[])
keywords :: Lex -> KwdArg -> BuildM (KwdArg, [Resolved])
keywords env (KwdArg n e k) = do
    r <- expr env e; (k',rs) <- keywords env k
    return (KwdArg n (canonical r) k',r:rs)
keywords env (KwdStar e) = do r <- expr env e; return (KwdStar (canonical r),[r])
keywords _ KwdNil = return (KwdNil,[])

comprehension :: Lex -> Expr -> [Expr] -> Comp -> BuildM Resolved
comprehension env e es c = do
    (inner,rs) <- comp env c
    results <- mapM (expr inner) es
    opaque env e (rs++results)
  where
    comp a NoComp = return (a,[])
    comp a (CompIf _ x c) = do
        r <- expr a x; (a',rs) <- comp a c; return (a',r:rs)
    comp a (CompFor _ p x c) = do
        r <- expr a x
        a' <- bindValues a{depth = depth a + 1} (bound p)
        patternExprs a' p
        (a'',rs) <- comp a' c
        return (a'',r:rs)

suite :: Lex -> Suite -> BuildM Lex
suite = foldM stmt

stmt :: Lex -> Stmt -> BuildM Lex
stmt env (Assign l [PVar _ n t] e) | isWitness n = do
    r <- expr env e
    i <- case node r of
      Just i -> return i
      Nothing -> intern Opaque (canonical r) (dependencies r) (captures r)
    modifyGraph $ \g -> g{registryBindings = Binding n t i (scope env) l : registryBindings g}
    return env{terms = M.insert n (WitnessValue i) (terms env)}
stmt env s = case s of
    Assign _ ps e -> mapM_ (patternExprs env) ps >> expr env e >> bindValues env (bound ps)
    VarAssign _ ps e -> mapM_ (patternExprs env) ps >> expr env e >> bindValues env (bound ps)
    MutAssign _ a b -> expr env a >> expr env b >> bindValues env (target a)
    AugAssign _ a _ b -> expr env a >> expr env b >> bindValues env (target a)
    Expr _ e -> visit [e]
    Return _ e -> visit (maybe [] (:[]) e)
    Raise _ e -> visit [e]
    Assert _ e x -> visit (e : maybe [] (:[]) x)
    Delete _ e -> visit [e]
    After _ _ a b -> visit [a,b]
    If _ bs els -> do
        forM_ bs $ \(Branch e b) -> expr env e >> suite env b
        suite env els
        bindValues env (written [s])
    While _ e b els -> do
        inner <- bindValues env{depth = depth env + 1} (written b)
        expr inner e; suite inner b; suite env els
        bindValues env (written [s])
    For _ p e b els -> do
        expr env e
        inner <- bindValues env{depth = depth env + 1} (bound p ++ written b)
        patternExprs inner p
        suite inner b; suite env els
        bindValues env (written [s])
    Try _ b hs els fin -> do
        suite env b
        uncertain <- bindValues env (written b)
        forM_ hs $ \(Handler ex h) -> bindValues uncertain (bound ex) >>= (\a -> suite a h)
        suite uncertain els
        final <- bindValues env (written [s])
        suite final fin
        bindValues env (written [s])
    With _ is b -> do
        forM_ is $ \(WithItem e p) -> expr env e >> mapM_ (patternExprs env) p
        inner <- bindValues env (bound is)
        suite inner b
        bindValues env (written [s])
    Data _ p b -> do
        mapM_ (patternExprs env) p
        bindValues env (bound p) >>= (\a -> suite a b)
        bindValues env (written [s])
    Decl _ ds -> do
        -- Function declarations can shadow a constructor spelling. Converted
        -- extension classes themselves retain their constructor identity.
        inner <- bindValues env [n | Def{dname=n} <- ds]
        mapM_ (decl inner) ds
        return inner
    _ -> return env
  where visit es = mapM_ (expr env) es >> return env

target :: Expr -> [Name]
target (Var _ (NoQ n)) = [n]
target (Paren _ e) = target e
target _ = []

-- Names.bound deliberately omits several control-flow forms. For the registry
-- a join must invalidate every possibly reassigned binding, including loops
-- and exception handlers. No aliases are propagated through these joins yet.
written :: Suite -> [Name]
written = concatMap writes
  where
    writes (For _ p _ b els) = bound p ++ written b ++ written els
    writes (While _ _ b els) = written b ++ written els
    writes (If _ bs els) = concat [written b | Branch _ b <- bs] ++ written els
    writes (Try _ b hs els fin) = written b ++ concat [bound ex ++ written h | Handler ex h <- hs]
                                     ++ written els ++ written fin
    writes (With _ is b) = bound is ++ written b
    writes (Data _ p b) = bound p ++ written b
    writes (MutAssign _ a _) = target a
    writes (AugAssign _ a _ _) = target a
    writes s = bound s

patternExprs :: Lex -> Pattern -> BuildM ()
patternExprs env p = case p of
    PData _ _ es -> mapM_ (expr env) es
    PParen _ p -> patternExprs env p
    PTuple _ ps ks -> pos ps >> kwd ks
    PList _ ps tail -> mapM_ (patternExprs env) ps >> mapM_ (patternExprs env) tail
    _ -> return ()
  where
    pos (PosPat p ps) = patternExprs env p >> pos ps
    pos (PosPatStar p) = patternExprs env p
    pos PosPatNil = return ()
    kwd (KwdPat _ p ps) = patternExprs env p >> kwd ps
    kwd (KwdPatStar p) = patternExprs env p
    kwd KwdPatNil = return ()

decl :: Lex -> Decl -> BuildM ()
decl _ Typedef{} = return ()
decl env d = do
    inner <- newScope env >>= (\a -> bindTypes a (qbinds d))
    case d of
      Def{pos=p,kwd=k} -> bindParameters inner (bound p ++ bound k) >>= body
      Actor{pos=p,kwd=k} -> bindParameters inner (bound p ++ bound k) >>= body
      _ -> bindTypes inner [QBind tvSelf []] >>= body
  where body a = suite a (dbody d) >> return ()

element :: Elem -> Expr
element (Elem e) = e
element (Star e) = e
association :: Assoc -> [Expr]
association (Assoc a b) = [a,b]
association (StarStar e) = [e]

-- Defaults in declaration/type metadata are not evaluated here. Their expanded
-- call-site expressions are visited normally, just as in WitnessStats.
children :: Expr -> [Expr]
children e = case e of
    Async _ x -> [x]
    Await _ x -> [x]
    Index _ x y -> [x,y]
    Slice _ x (Sliz _ a b c) -> x : [y | Just y <- [a,b,c]]
    Cond _ a b c -> [a,b,c]
    IsInstance _ x _ -> [x]
    BinOp _ a _ b -> [a,b]
    CompOp _ x ops -> x : [y | OpArg _ y <- ops]
    UnOp _ _ x -> [x]
    Rest _ x _ -> [x]
    DotI _ x _ -> [x]
    RestI _ x _ -> [x]
    Opt _ x _ -> [x]
    OptChain _ x -> [x]
    Yield _ x -> maybe [] (:[]) x
    YieldFrom _ x -> [x]
    List _ xs -> map element xs
    Dict _ xs -> concatMap association xs
    Set _ xs -> map element xs
    Box _ x -> [x]
    UnBox _ x -> [x]
    _ -> []

registrySummary :: WitnessRegistry -> Value
registrySummary g = object
    [ "recipe_nodes" .= IM.size (registryRecipes g)
    , "constructor_recipes" .= length constructors
    , "constructor_sites" .= length (registrySites g)
    , "dependency_edges" .= sum (map (S.size . recipeDependencies) recipes)
    , "binding_roots" .= length (registryBindings g)
    , "uses" .= length (registryUses g), "scopes" .= registryScopes g
    , "constructor_recipes_by_lifetime" .= counts (map recipeLifetime constructors)
    , "constructor_sites_by_lifetime" .= counts (map siteLife $ registrySites g)
    , "nonstatic_sites_by_lifetime" .= counts (map siteLife nonstatic)
    , "nonstatic_recipes_by_lifetime" .= counts (map (recipeLifetime . at) $ S.toList nonstaticIds)
    , "nonstatic_recipe_scope_pairs_by_lifetime" .= counts
        [recipeLifetime (at i) | (i,_) <- S.toList $ S.fromList [(siteRecipe s,siteScope s) | s <- nonstatic]]
    , "nonstatic_loop_sites_by_lifetime" .= counts (map siteLife $ filter ((> 0) . siteLoopDepth) nonstatic)
    , "nonstatic_closed_constructors" .= M.fromListWith (+)
        [(prstr q, 1 :: Int) | s <- nonstatic, let r = at (siteRecipe s)
                            , recipeLifetime r == Closed, Constructor q <- [recipeKind r]]
    ]
  where
    at i = registryRecipes g IM.! i
    recipes = IM.elems (registryRecipes g)
    constructors = [r | r <- recipes, Constructor{} <- [recipeKind r]]
    nonstatic = filter (not . siteStatic) (registrySites g)
    nonstaticIds = S.fromList (map siteRecipe nonstatic)
    siteLife = recipeLifetime . at . siteRecipe
    label Closed = "closed"
    label ParameterDependent = "parameter_dependent"
    label LocalDependent = "local_dependent"
    label Unknown = "unknown"
    counts ls = M.fromList [(label k, length $ filter (== k) ls)
                          | k <- [Closed,ParameterDependent,LocalDependent,Unknown]] :: M.Map String Int
