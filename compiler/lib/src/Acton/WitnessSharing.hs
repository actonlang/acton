-- Copyright (C) 2026 Acton contributors
--
-- Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
--
-- 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
--
-- 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
--
-- 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- Witness descriptions and placement. The sections below contain structural
-- keys, the binding-aware registry, local sharing, module pooling, the empty
-- extension-hook check and opt-in diagnostics. They describe and reuse ordinary
-- witness objects; there is no runtime registry or alternate serialization.
module Acton.WitnessSharing
    ( witnessHash, expressionHash
    , WitnessRegistry(..), Recipe(..), RecipeKind(..), Lifetime(..)
    , Capture(..), CaptureKind(..), Binding(..), Site(..), Use(..)
    , buildRegistry, buildFunctionRegistry, registrySummary
    , shareWitnesses, shareFunction, constructorHeads, witnessConstructors
    , walkStmt, walkExpr, constructorCall
    , poolWitnesses, poolModule, poolWitnessesWithEmptyHook, poolModuleWithEmptyHook
    , emptyExtensionInit, witnessStats
    ) where

import Control.Monad (foldM, forM, forM_, when)
import Control.Monad.State.Strict
import Data.Aeson (Value, object, (.=))
import Data.Char (isAlphaNum, isSpace)
import Data.Functor.Identity
import Data.Hashable (hash)
import Data.List (foldl', isPrefixOf, isInfixOf)
import Data.Maybe (mapMaybe, isJust)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Acton.Syntax hiding (expr, comp, target)
import Acton.Builtin (initKW, mBuiltin)
import Acton.Names (assigned, bound, free, isWitness, altInit)
import Acton.Subst (vfree, vsubst)
import Acton.Env (Env0, lookupName, lookupModuleInfo, moduleLookupName, moduleName, thismod, setMod, define)
import Acton.NameInfo (NameInfo(..))
import Acton.Prim (tupleWits)
import Acton.QuickType (envOf)
import Utils (SrcLoc(..), prstr)

-- Structural keys ---------------------------------------------------------

-- A fingerprint selects a bucket; complete Type/Expr equality still decides reuse.
witnessHash :: Type -> Expr -> Int
witnessHash t e = hash (typeHash t, expressionHash e)

-- Follow Syntax's equality, rather than Show/pretty-printing: locations are
-- ignored, numeric lexemes matter, and defaults contribute only their presence.
-- Unsupported expression forms share a bucket. That preserves the old exact
-- comparison as a conservative fallback without imposing a new Eq instance on
-- the AST (Box/UnBox, for example, deliberately have no structural equality).
expressionHash :: Expr -> Int
expressionHash e = case e of
    Var _ n -> hash (0 :: Int, n)
    Int _ _ s -> hash (1 :: Int, s)
    Float _ _ s -> hash (2 :: Int, s)
    Imaginary _ _ s -> hash (3 :: Int, s)
    Bool _ b -> hash (4 :: Int, b)
    None{} -> 5
    NotImplemented{} -> 6
    Ellipsis{} -> 7
    Strings _ s -> hash (8 :: Int, s)
    BStrings _ s -> hash (9 :: Int, s)
    Call _ f p k -> hashNode 10 [expressionHash f, pos p, kwd k]
    TApp _ f ts -> hashNode 11 (expressionHash f : map typeHash ts)
    Dot _ x n -> hashNode 12 [expressionHash x, hash n]
    DotI _ x i -> hashNode 13 [expressionHash x, hash i]
    Cond _ a b c -> hashNode 14 (map expressionHash [a,b,c])
    Tuple _ p k -> hashNode 15 [pos p, kwd k]
    Paren _ x -> expressionHash x
    _ -> -1
  where
    pos (PosArg x p) = hashNode 0 [expressionHash x, pos p]
    pos (PosStar x) = hashNode 1 [expressionHash x]
    pos PosNil = 2
    kwd (KwdArg n x k) = hashNode 0 [hash n, expressionHash x, kwd k]
    kwd (KwdStar x) = hashNode 1 [expressionHash x]
    kwd KwdNil = 2

hashNode :: Int -> [Int] -> Int
hashNode tag xs = hash (tag, xs)

typeHash :: Type -> Int
typeHash t = case t of
    TUni _ u -> hashNode 0 [uvid u]
    TVar _ (TV _ n) -> hashNode 1 [hash n]
    TCon _ (TC n ts) -> hashNode 2 (hash n : map typeHash ts)
    TFun _ fx p k r -> hashNode 3 (map typeHash [fx,p,k,r])
    TTuple _ p k -> hashNode 4 (map typeHash [p,k])
    TOpt _ x -> hashNode 5 [typeHash x]
    TNone{} -> 6
    TWild{} -> 7
    TNil _ k -> hashNode 8 [hash (show k)]
    TRow _ k n x d r -> hashNode 9 [hash (show k), hash n, typeHash x, hash (isJust d), typeHash r]
    TStar _ k r -> hashNode 10 [hash (show k), typeHash r]
    TFX _ fx -> hashNode 11 [hash (show fx)]
    TUnboxed _ x -> hashNode 12 [typeHash x]

-- Description graph -------------------------------------------------------

-- Canonical recipe IDs describe dependencies, not legal construction locations.
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
    , currentModule :: Maybe ModName, globalWitnesses :: M.Map Name Int
    }
type BuildM = State Build

-- The caller supplies resolved implementation identities from the original
-- extension environment, including primitive/generated witness constructors.
-- Types stay on binding records. Constructor keys retain their explicit type
-- applications; type variables and dependency witnesses have binding identities.
buildRegistry :: (QName -> Maybe QName) -> (Expr -> Bool) -> Module -> WitnessRegistry
buildRegistry resolve isStatic m = build (Just $ modname m) resolve isStatic $ suite initialLex (mbody m) >> return ()

-- Scope 1 is the function itself. Nested declarations/lambdas get other IDs;
-- their parameters and construction sites remain separate from this frame.
buildFunctionRegistry :: (QName -> Maybe QName) -> (Expr -> Bool) -> Decl -> WitnessRegistry
buildFunctionRegistry resolve isStatic d = build Nothing resolve isStatic $ decl initialLex d

initialLex :: Lex
initialLex = Lex 0 0 M.empty M.empty

build :: Maybe ModName -> (QName -> Maybe QName) -> (Expr -> Bool) -> BuildM () -> WitnessRegistry
build mn resolve isStatic action = finish $ execState action initial
  where
    initial = Build (WitnessRegistry IM.empty [] [] [] 1) IM.empty 0 resolve isStatic mn M.empty
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
    when (scope env == 0) $ modify $ \s -> s{globalWitnesses = M.delete n (globalWitnesses s)}
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
    mn <- gets currentModule
    global <- case var e of
      GName m n | Just m == mn -> do
        found <- gets (M.lookup n . globalWitnesses)
        return $ case (scope env,found,M.lookup n $ terms env) of
          (0,Just i,Just (WitnessValue j)) | i == j -> Just i
          (0,_,_) -> Nothing
          _ -> found
      _ -> return Nothing
    case global of
      Just i -> do
        modifyGraph $ \g -> g{registryUses = Use i (scope env) (eloc e) : registryUses g}
        fromNode i
      Nothing -> do
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
    when (scope env == 0) $ modify $ \s -> s{globalWitnesses = M.insert n i (globalWitnesses s)}
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
    , "nonstatic_module_sites_by_lifetime" .= counts (map siteLife moduleSites)
    , "nonstatic_module_recipes_by_lifetime" .= counts
        [recipeLifetime (at i) | i <- S.toList $ S.fromList (map siteRecipe moduleSites)]
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
    moduleSites = filter ((== 0) . siteScope) nonstatic
    nonstaticIds = S.fromList (map siteRecipe nonstatic)
    siteLife = recipeLifetime . at . siteRecipe
    label Closed = "closed"
    label ParameterDependent = "parameter_dependent"
    label LocalDependent = "local_dependent"
    label Unknown = "unknown"
    counts ls = M.fromList [(label k, length $ filter (== k) ls)
                          | k <- [Closed,ParameterDependent,LocalDependent,Unknown]] :: M.Map String Int

-- Function-local sharing --------------------------------------------------

-- Run after boxing's dead-witness removal. Reuse only dominated constructions.
type ConstructorInfo = QName -> Maybe (QName, Int)

-- Only constructors found in generated witness equations are trusted here.
-- NClass/pure-init checks additionally exclude arbitrary helper calls. Keep
-- the names from before boxing: boxing may remove their original equations
-- while leaving inline constructions at their uses.
constructorHeads :: Suite -> S.Set QName
constructorHeads = S.fromList . concatMap collect
  where
    collect (Assign _ [PVar _ n _] e) | isWitness n = maybe [] (:[]) (root e)
    collect (Decl _ ds) = concat [concatMap collect (dbody d) | d <- ds, notTypedef d]
    collect (If _ bs els) = concat [concatMap collect b | Branch _ b <- bs] ++ concatMap collect els
    collect (While _ _ b els) = concatMap collect (b ++ els)
    collect (For _ _ _ b els) = concatMap collect (b ++ els)
    collect (Try _ b hs els fin) = concatMap collect (b ++ els ++ fin) ++ concat [concatMap collect h | Handler _ h <- hs]
    collect (With _ _ b) = concatMap collect b
    collect (Data _ _ b) = concatMap collect b
    collect _ = []
    root (Dot _ e _) = root e
    root (Paren _ e) = root e
    root (Call _ f _ _) = fst <$> callHead f
    root _ = Nothing
    notTypedef Typedef{} = False
    notTypedef _ = True

shareWitnesses :: Env0 -> S.Set QName -> (Expr -> Bool) -> Module -> Module
shareWitnesses env known isStatic m = m{mbody = map top (mbody m)}
  where
    info q = M.lookup q (witnessConstructors env known m)
    top (Decl l ds) = Decl l (map decl ds)
    top s = s
    decl d@Def{} = shareFunction info isStatic d
    decl d@Class{} = d{dbody = map top (dbody d)}
    decl d = d

witnessConstructors :: Env0 -> S.Set QName -> Module -> M.Map QName (QName,Int)
witnessConstructors env known m = constructors
  where
    env1 = define (envOf $ mbody m) (setMod (modname m) env)
    constructors = M.fromList $ concatMap entry (S.toList known)
    entry q = case lookupQ S.empty q of
      Just (q', NClass qs _ te _) | Just (NDef (TSchema _ _ TFun{effect=fx}) _ _) <- lookup initKW te
                                 , fx == fxPure -> [(q,(q',length qs)), (q',(q',length qs))]
      _ -> []
    lookupQ seen q | S.member q seen = Nothing
    lookupQ seen q = case raw q of
      Just (_, NAlias target) -> lookupQ (S.insert q seen) target
      result -> result
    raw q@(NoQ n) = (,) (maybe q (\mn -> GName mn n) (thismod env1)) <$> lookupName n env1
    raw (GName mn n) | Just mn == thismod env1 = raw (NoQ n)
    raw q = do
      mi <- lookupModuleInfo (mname q) env1
      ni <- moduleLookupName mi (noq q)
      return (GName (moduleName mi) (noq q),ni)

-- A plan uses exact original expressions to connect registry sites to the AST.
-- Ambiguous expressions (the same spelling referring to different bindings)
-- are excluded, even when one of those sites separately looks shareable.
data Plan = Plan { planRecipe :: Int, planType :: Type }
type Plans = IM.IntMap [(Expr,Plan)]
data Sharing = Sharing
    { available :: IM.IntMap Name, prefixRev :: Suite, nextName :: Int
    , reserved :: S.Set Name, definitions :: [(Name,Type,Expr)]
    , references :: M.Map Name Int
    }
type Rewrite = State Sharing

shareFunction :: ConstructorInfo -> (Expr -> Bool) -> Decl -> Decl
shareFunction info isStatic d@Def{}
  -- A witness receiver's fields can still change while it is initialized.
  | dname d `elem` [initKW,altInit] = d
  | IM.null plans = d
  | S.null retained = d
  | otherwise = d{dbody = substitute singles (prune body)}
  where
    locals = S.fromList (bound (pos d) ++ bound (kwd d) ++ assigned (dbody d))
    resolve q@(NoQ n) | S.member n locals = Nothing
    resolve q = fst <$> info q
    registry = buildFunctionRegistry resolve isStatic d
    at i = registryRecipes registry IM.! i
    sites = filter ((== 1) . siteScope) (registrySites registry)
    parameters = M.fromList (parTypes (pos d) ++ kwdTypes (kwd d))
    safeCapture c = case captureKind c of
      TypeParameter -> True
      WitnessParameter -> case M.lookup (captureName c) parameters of
        Just TCon{} -> True
        _ -> False
      _ -> False
    eligible s = not (siteStatic s) && recipeLifetime r <= ParameterDependent &&
                 all safeCapture (S.toList $ recipeCaptures r)
      where r = at (siteRecipe s)
    groups = foldl' add IM.empty sites
    add buckets s = IM.insertWith (++) (expressionHash $ siteExpression s) [s] buckets
    distinct ss = foldl' insert [] ss
      where insert [] s = [[s]]
            insert (xs:xss) s | siteExpression (head xs) == siteExpression s = (s:xs):xss
                              | otherwise = xs:insert xss s
    -- Unsupported expressions can share a fingerprint bucket. There is no
    -- reason to compare all its expressions when none can be shared.
    unambiguous = [s | bucket <- IM.elems groups, any eligible bucket, xs <- distinct bucket
                     , S.size (S.fromList $ map siteRecipe xs) == 1, s <- xs, eligible s]
    counts = IM.fromListWith (+) [(siteRecipe s,1::Int) | s <- unambiguous]
    plans = foldl' addPlan IM.empty unambiguous
    addPlan ps s
      | IM.findWithDefault 0 (siteRecipe s) counts < 2 = ps
      | Just _ <- lookupPlan ps (siteExpression s) = ps
      | Just (q,ts) <- constructorCall (siteExpression s)
      , Just (canonical,arity) <- info q, length ts == arity
      = IM.insertWith (++) (expressionHash $ siteExpression s)
          [(siteExpression s,Plan (siteRecipe s) (tCon $ TC canonical ts))] ps
      | otherwise = ps
    names = S.fromList (free d ++ bound (pos d) ++ bound (kwd d) ++
                       [n | QBind (TV _ n) _ <- qbinds d] ++ map bindingName (registryBindings registry))
    initial = Sharing IM.empty [] 0 names [] M.empty
    (body,done) = runState (rewriteSuite plans isStatic True (dbody d)) initial
    retained = M.keysSet $ M.filter (> 1) (references done)
    made = S.fromList [n | (n,_,_) <- definitions done]
    singles = foldl' expand M.empty (reverse $ definitions done)
    expand m (n,_,e) | S.member n retained = m
                     | otherwise = M.insert n (replace m e) m
    prune = pruneDefinitions (S.difference made retained)
shareFunction _ _ d = d

parTypes :: PosPar -> [(Name,Type)]
parTypes (PosPar n (Just t) _ p) = (n,t):parTypes p
parTypes (PosPar _ _ _ p) = parTypes p
parTypes _ = []
kwdTypes :: KwdPar -> [(Name,Type)]
kwdTypes (KwdPar n (Just t) _ p) = (n,t):kwdTypes p
kwdTypes (KwdPar _ _ _ p) = kwdTypes p
kwdTypes _ = []

callHead :: Expr -> Maybe (QName,[Type])
callHead (Var _ q) = Just (q,[])
callHead (TApp _ f ts) = do (q,_) <- callHead f; return (q,ts)
callHead (Paren _ f) = callHead f
callHead _ = Nothing
constructorCall :: Expr -> Maybe (QName,[Type])
constructorCall (Call _ f _ _) = callHead f
constructorCall _ = Nothing

lookupPlan :: Plans -> Expr -> Maybe Plan
lookupPlan plans e = lookupExpr (IM.findWithDefault [] (expressionHash e) plans)
  where lookupExpr [] = Nothing
        lookupExpr ((original,p):ps) | original == e = Just p
                                     | otherwise = lookupExpr ps

use :: Name -> Rewrite Expr
use n = do
    modify $ \s -> s{references = M.insertWith (+) n 1 (references s)}
    return (eVar n)

newBinding :: Plan -> Expr -> Rewrite Expr
newBinding p e = do
    s <- get
    let supply = [Internal Witness "shared" i | i <- [nextName s..]]
        n@(Internal _ _ i) = head (filter (`S.notMember` reserved s) supply)
        t = planType p
    put s{available = IM.insert (planRecipe p) n (available s), nextName = i+1
         ,reserved = S.insert n (reserved s), definitions = (n,t,e):definitions s
         ,prefixRev = Assign (eloc e) [pVar n t] e : prefixRev s}
    use n

-- Definitions created in a branch or loop body never become available after
-- it. Uses of an outer definition remain valid and contribute to its count.
region :: Rewrite a -> Rewrite a
region action = do
    outer <- get
    modify $ \s -> s{prefixRev = []}
    result <- action
    modify $ \s -> s{available = available outer, prefixRev = prefixRev outer}
    return result

rewriteSuite :: Plans -> (Expr -> Bool) -> Bool -> Suite -> Rewrite Suite
rewriteSuite _ _ _ [] = return []
rewriteSuite plans isStatic strict (s:ss) = do
    s' <- rewriteStmt plans isStatic strict s
    pre <- gets (reverse . prefixRev)
    modify $ \a -> a{prefixRev = []}
    ss' <- rewriteSuite plans isStatic strict ss
    return (pre ++ s':ss')

rewriteStmt :: Plans -> (Expr -> Bool) -> Bool -> Stmt -> Rewrite Stmt
rewriteStmt plans isStatic strict (If l bs els) = do
    bs' <- forM (zip [0::Int ..] bs) $ \(i,Branch e b) -> do
        e' <- rewriteExpr plans isStatic (strict && i == 0) e
        b' <- region (rewriteSuite plans isStatic strict b)
        return (Branch e' b')
    els' <- region (rewriteSuite plans isStatic strict els)
    return (If l bs' els')
rewriteStmt plans isStatic strict s = walkStmt (rewriteExpr plans isStatic strict)
                                        (region . rewriteSuite plans isStatic strict) s

rewriteExpr :: Plans -> (Expr -> Bool) -> Bool -> Expr -> Rewrite Expr
rewriteExpr _ isStatic _ e | isStatic e = return e
rewriteExpr plans isStatic strict e = case lookupPlan plans e of
    Just p -> do
        old <- gets (IM.lookup (planRecipe p) . available)
        case old of
          Just n -> use n
          Nothing | strict -> rebuild >>= newBinding p
          _ -> rebuild
    Nothing -> rebuild
  where
    visit = rewriteExpr plans isStatic strict
    conditional = rewriteExpr plans isStatic False
    rebuild = case e of
      Cond l a c b -> do c' <- visit c; a' <- conditional a; b' <- conditional b; return (Cond l a' c' b')
      BinOp l a op b | op == And || op == Or -> BinOp l <$> visit a <*> pure op <*> conditional b
      -- A Let can shadow names without introducing a registry execution scope.
      -- Leave it intact rather than applying an outer expression index there.
      Let{} -> return e
      _ -> walkExpr visit (region . rewriteSuite plans isStatic False) e

replace :: M.Map Name Expr -> Expr -> Expr
replace replacements e@(Var _ (NoQ n)) = M.findWithDefault e n replacements
replace replacements e = runIdentity $ walkExpr (Identity . replace replacements)
                                                (Identity . substitute replacements) e
substitute :: M.Map Name Expr -> Suite -> Suite
substitute replacements = map (runIdentity . walkStmt (Identity . replace replacements)
                                                     (Identity . substitute replacements))
pruneDefinitions :: S.Set Name -> Suite -> Suite
pruneDefinitions removed = concatMap prune
  where
    prune (Assign _ [PVar _ n _] _) | S.member n removed = []
    prune s = [runIdentity $ walkStmt (Identity . expr) (Identity . pruneDefinitions removed) s]
    expr e = runIdentity $ walkExpr (Identity . expr) (Identity . pruneDefinitions removed) e

-- These walks visit evaluated expressions only. Declaration/type/default
-- metadata and nested functions/lambdas belong to other execution contexts.
walkStmt :: Monad m => (Expr -> m Expr) -> (Suite -> m Suite) -> Stmt -> m Stmt
walkStmt e b s = case s of
    Assign l ps x -> Assign l ps <$> e x
    VarAssign l ps x -> VarAssign l ps <$> e x
    MutAssign l x y -> MutAssign l <$> e x <*> e y
    AugAssign l x op y -> AugAssign l <$> e x <*> pure op <*> e y
    Expr l x -> Expr l <$> e x
    Return l x -> Return l <$> mapM e x
    Raise l x -> Raise l <$> e x
    Assert l x y -> Assert l <$> e x <*> mapM e y
    Delete l x -> Delete l <$> e x
    After l now x y -> After l now <$> e x <*> e y
    If l bs els -> If l <$> mapM (\(Branch x ss) -> Branch <$> e x <*> b ss) bs <*> b els
    While l x ss els -> While l <$> e x <*> b ss <*> b els
    For l p x ss els -> For l p <$> e x <*> b ss <*> b els
    Try l ss hs els fin -> Try l <$> b ss <*> mapM (\(Handler ex ss) -> Handler ex <$> b ss) hs <*> b els <*> b fin
    With l is ss -> With l <$> mapM (\(WithItem x p) -> WithItem <$> e x <*> pure p) is <*> b ss
    Data l p ss -> Data l p <$> b ss
    _ -> return s

walkExpr :: Monad m => (Expr -> m Expr) -> (Suite -> m Suite) -> Expr -> m Expr
walkExpr e b x = case x of
    Call l f p k -> Call l <$> e f <*> pos p <*> kwd k
    TApp l f ts -> TApp l <$> e f <*> pure ts
    Let l ss x -> Let l <$> b ss <*> e x
    Paren l x -> Paren l <$> e x
    Async l x -> Async l <$> e x
    Await l x -> Await l <$> e x
    Index l x y -> Index l <$> e x <*> e y
    Slice l x (Sliz sl a c d) -> Slice l <$> e x <*> (Sliz sl <$> mapM e a <*> mapM e c <*> mapM e d)
    Cond l x y z -> Cond l <$> e x <*> e y <*> e z
    IsInstance l x q -> IsInstance l <$> e x <*> pure q
    BinOp l x op y -> BinOp l <$> e x <*> pure op <*> e y
    CompOp l x ops -> CompOp l <$> e x <*> mapM (\(OpArg op y) -> OpArg op <$> e y) ops
    UnOp l op x -> UnOp l op <$> e x
    Dot l x n -> Dot l <$> e x <*> pure n
    Rest l x n -> Rest l <$> e x <*> pure n
    DotI l x i -> DotI l <$> e x <*> pure i
    RestI l x i -> RestI l <$> e x <*> pure i
    Opt l x v -> Opt l <$> e x <*> pure v
    OptChain l x -> OptChain l <$> e x
    Yield l x -> Yield l <$> mapM e x
    YieldFrom l x -> YieldFrom l <$> e x
    Tuple l p k -> Tuple l <$> pos p <*> kwd k
    List l xs -> List l <$> mapM elem xs
    Set l xs -> Set l <$> mapM elem xs
    Dict l xs -> Dict l <$> mapM assoc xs
    Box t x -> Box t <$> e x
    UnBox t x -> UnBox t <$> e x
    _ -> return x
  where
    pos (PosArg x p) = PosArg <$> e x <*> pos p
    pos (PosStar x) = PosStar <$> e x
    pos PosNil = return PosNil
    kwd (KwdArg n x k) = KwdArg n <$> e x <*> kwd k
    kwd (KwdStar x) = KwdStar <$> e x
    kwd KwdNil = return KwdNil
    elem (Elem x) = Elem <$> e x
    elem (Star x) = Star <$> e x
    assoc (Assoc x y) = Assoc <$> e x <*> e y
    assoc (StarStar x) = StarStar <$> e x

-- Module pooling ----------------------------------------------------------

-- Materialize eligible closed DAGs after imports and local tables are ready.
type Constructors = M.Map QName (QName,Int)

poolWitnesses :: Env0 -> S.Set QName -> (Expr -> Maybe Expr) -> Module -> Module
poolWitnesses = poolWitnessesWithEmptyHook False

poolWitnessesWithEmptyHook :: Bool -> Env0 -> S.Set QName -> (Expr -> Maybe Expr) -> Module -> Module
poolWitnessesWithEmptyHook emptyHook env known staticValue m =
    poolModuleWithEmptyHook emptyHook (witnessConstructors env known m) staticValue m

poolModule :: Constructors -> (Expr -> Maybe Expr) -> Module -> Module
poolModule = poolModuleWithEmptyHook False

poolModuleWithEmptyHook :: Bool -> Constructors -> (Expr -> Maybe Expr) -> Module -> Module
poolModuleWithEmptyHook emptyHook constructors staticValue m
    | modname m == mBuiltin = m
    | IM.null globals = m
    | otherwise = m{mbody = initializers ++ rewrite (mbody m)}
  where
    isStatic = isJust . staticValue
    resolve q = fst <$> M.lookup q constructors
    registry = buildRegistry resolve isStatic m
    recipes = registryRecipes registry
    at i = recipes IM.! i
    -- Retain every original spelling's other interpretations when deciding
    -- ambiguity. A closed occurrence must never rewrite a captured one.
    groups = foldl' (\a s -> IM.insertWith (++) (expressionHash $ siteExpression s) [s] a)
                    IM.empty (registrySites registry)
    index = IM.map candidates groups
    candidates ss | any (\s -> recipeLifetime (at $ siteRecipe s) == Closed && not (siteStatic s)) ss
                            = mapMaybe unique (partitionExpressions ss)
                  | otherwise = []
    partitionExpressions = foldl' insert []
      where insert [] s = [[s]]
            insert (xs:xss) s | siteExpression (head xs) == siteExpression s = (s:xs):xss
                              | otherwise = xs:insert xss s
    unique xs | [i] <- S.toList (S.fromList $ map siteRecipe xs)
              , recipeLifetime (at i) == Closed
              , not (isStatic $ siteExpression $ head xs) = Just (siteExpression $ head xs,i)
              | otherwise = Nothing
    lookupSite locals e = case constructorCall e of
      Just (NoQ n,_) | S.member n locals -> Nothing
      _ -> lookup e (IM.findWithDefault [] (expressionHash e) index)

    -- Class-table expressions can call functions before the pool is ready.
    -- A C hook runs even earlier, before local tables. In that case a normal
    -- local witness constructor is already unusable (its allocator calls the
    -- table's __init__), so replacing that call after tables are ready is safe.
    -- Imported constructors and closure allocators have no such prerequisite.
    allowFunctions = all safeTable (mbody m)
    earlyHook = hasNotImpl (mbody m) && not emptyHook
    safeTable (Decl _ ds) = all safeDecl ds
    safeTable _ = True
    safeDecl Class{dbody=b} = all classStmt b
    safeDecl _ = True
    classStmt (Decl _ ds) = all safeDecl ds
    classStmt Signature{} = True
    classStmt Pass{} = True
    classStmt _ = False

    localClasses = M.fromList [(n,d) | Decl _ ds <- mbody m, d@Class{dname=n} <- ds]
    localFunctions = S.fromList [n | Decl _ ds <- mbody m, Def{dname=n} <- ds]
    localName (NoQ n) = Just n
    localName (GName mn n) | mn == modname m = Just n
    localName _ = Nothing
    ordinaryLocal e = case constructorCall e >>= (\(q,_) -> fst <$> M.lookup q constructors) >>= localName of
      Just (Internal LLiftPass _ _) -> False
      Just n -> M.member n localClasses
      _ -> False
    topNames = S.fromList [n | Assign _ [PVar _ n (Just _)] _ <- mbody m, isWitness n]
    bindingGroups = M.fromListWith (++) [(bindingName b,[b]) | b <- registryBindings registry
                     , bindingScope b == 0, S.member (bindingName b) topNames]
    topBindings = M.mapMaybe one bindingGroups
    one [b] | Just t@TCon{} <- bindingType b, null (vfree t) = Just b
    one _ = Nothing
    moduleValues = S.fromList (bound (mbody m)) S.\\ (M.keysSet localClasses `S.union` localFunctions)

    -- Constructor arguments are not its only initialization dependencies:
    -- lifting can put a witness used by __init__ into a module-global binding.
    -- Include these reads (and parent/local constructor initializers) in the
    -- ordering. Unknown module values and recursive initialization stay local.
    initNeeds = M.mapWithKey (\n _ -> needs S.empty n) localClasses
    needs seen n | S.member n seen = Nothing
    needs seen n = case M.lookup n localClasses of
      Nothing -> Just []
      Just d -> do
        let bodies = [f | Decl _ fs <- dbody d, f@Def{dname=k} <- fs, k `elem` [initKW,altInit]]
            names = S.unions (map runtimeNames bodies)
            values = S.toList (S.intersection names moduleValues)
            classes = S.toList (S.intersection names $ M.keysSet localClasses)
            knownNames = moduleValues `S.union` localFunctions `S.union` M.keysSet localClasses
        if null bodies || any (hasNotImpl . dbody) bodies || not (S.null $ names S.\\ knownNames) ||
           not (S.null $ S.intersection names localFunctions) then Nothing else do
          bs <- mapM (`M.lookup` topBindings) values
          inherited <- concat <$> mapM (needs $ S.insert n seen) classes
          return (map bindingRecipe bs ++ inherited)
    runtimeNames d = S.fromList $ mapMaybe runtimeName $ S.toList qs
      where qs = execState (mapM_ statement $ dbody d) S.empty
            locals = S.fromList (bound (pos d) ++ bound (kwd d) ++ assigned (dbody d))
            runtimeName (NoQ n) | S.member n locals = Nothing
            runtimeName q = case localName q of
              Just (Derived c k) | k `elem` [initKW,altInit], M.member c localClasses -> Just c
              n -> n
            statement s = walkStmt expression (mapM statement) s
            expression e = do
              case e of Var _ q -> modify (S.insert q); _ -> return ()
              walkExpr expression (mapM statement) e
    hidden (GName mn n) | mn == modname m = M.findWithDefault Nothing n initNeeds
    hidden (NoQ n) = M.findWithDefault (Just []) n initNeeds
    hidden _ = Just []
    edges = IM.map deps recipes
    deps r | recipeLifetime r /= Closed = Nothing
           | isStatic (recipeExpression r) = Just []
           | otherwise = case recipeKind r of
               Constructor q -> do
                 (_,arity) <- M.lookup q constructors
                 (_,ts) <- constructorCall (recipeExpression r)
                 if length ts /= arity then Nothing else
                   (S.toList (recipeDependencies r) ++) <$> hidden q
               Projection{} -> Just (S.toList $ recipeDependencies r)
               Instantiation -> Just (S.toList $ recipeDependencies r)
               _ -> Nothing

    -- First discover actual eligible occurrences. In particular, arguments of
    -- a static witness are discarded by C generation and must not allocate.
    (_,requested) = runState (walkModule find (mbody m)) S.empty
    find locals e = do
      case lookupSite locals e of
        Just i -> modify (S.insert i)
        _ -> return ()
      descend (find locals) e
    -- Memoized DFS rejects cycles/missing dependencies without penalizing the
    -- other roots. A second walk retains only nodes needed by surviving roots.
    verdicts = execState (mapM_ (valid S.empty) $ S.toList requested) IM.empty
    valid visiting i
      | S.member i visiting = return False
      | otherwise = do
          old <- gets (IM.lookup i)
          case old of
            Just b -> return b
            Nothing -> do
              ok <- case IM.lookup i edges of
                Just (Just ds) -> and <$> mapM (valid $ S.insert i visiting) ds
                _ -> return False
              modify (IM.insert i ok)
              return ok
    roots = filter (\i -> IM.findWithDefault False i verdicts) (S.toList requested)
    order = reverse $ snd $ execState (mapM_ ordered roots) (S.empty,[])
    ordered i = do
      seen <- gets fst
      if S.member i seen then return () else do
        modify (\(s,os) -> (S.insert i s,os))
        mapM_ ordered (maybe [] id $ IM.findWithDefault Nothing i edges)
        modify (\(s,os) -> (s,i:os))
    -- Static dependencies also need named witness bindings: C generation turns
    -- their RHS into an existing builtin object. Inlining Ctor() as an argument
    -- would instead call its allocator, even for a builtin static witness.
    pooled = [i | i <- order, Constructor{} <- [recipeKind $ at i]]
    bindings = IM.fromListWith (++) [(bindingRecipe b,[b]) | b <- M.elems topBindings]
    reserved = S.fromList (free (mbody m) ++ bound (mbody m) ++ map bindingName (registryBindings registry))
    (_,globals) = foldl' allocate (reserved,IM.empty) pooled
    allocate (used,gs) i = (S.insert n used,IM.insert i (n,t) gs)
      -- Preserve the concrete constructor type: callers can project embedded
      -- super-witness fields which are absent from a protocol-typed alias.
      where candidates = filter ((== Just (concrete i)) . bindingType) (IM.findWithDefault [] i bindings)
            (n,t) = case candidates of
              b:_ -> (bindingName b, maybe (error "pool: missing type") id (bindingType b))
              [] -> (head [n | k <- [i..], let n = Internal Witness "module" k, S.notMember n used], concrete i)
    concrete i = case constructorCall (recipeExpression $ at i) of
      Just (q,ts) -> tCon (TC q ts)
      _ -> error "pool: constructor recipe without call"
    global i = eQVar $ GName (modname m) (fst $ globals IM.! i)
    reference (Var _ (NoQ (Internal Witness "registry-recipe" i))) = Just i
    reference _ = Nothing
    expand useGlobals e
      | Just i <- reference e = case IM.lookup i globals of
          Just _ | useGlobals -> global i
          _ -> materialize useGlobals i
      | otherwise = runIdentity $ walkExpr (Identity . expand useGlobals) (Identity . id) e
    materialize useGlobals i
      | Just e' <- staticValue e = e'
      | otherwise = expand (useGlobals && not (isStatic e)) e
      where e = recipeExpression (at i)
    value i | IM.member i globals = global i
            | otherwise = materialize True i
    declarations i = ctor ++ aliases
      where ctor = case IM.lookup i globals of
              Just (n,t) -> [Assign NoLoc [pVar n t] (materialize True i)]
              Nothing -> []
            aliases = [Assign NoLoc [pVar (bindingName b) t] (value i)
                       | b <- IM.findWithDefault [] i bindings, Just t <- [bindingType b]
                       , maybe True ((/= bindingName b) . fst) (IM.lookup i globals)]
    initializers = concatMap declarations order
    moved = S.fromList [n | Assign _ [PVar _ n _] _ <- initializers]
    rewrite ss = filter (not . movedStmt) $ runIdentity $ walkModule (\locals e -> Identity $ replace locals e) ss
    movedStmt (Assign _ [PVar _ n _] _) = S.member n moved
    movedStmt _ = False
    replace locals e | Just i <- lookupSite locals e, IM.member i globals = global i
    replace locals e = runIdentity $ descend (Identity . replace locals) e

    -- Witness initializers retain their original constructors. Their global
    -- reads have explicit ordering edges; rewriting their calls could create
    -- hidden recursion through the pool being initialized.
    walkModule :: Monad f => (S.Set Name -> Expr -> f Expr) -> Suite -> f Suite
    walkModule = walkSuite Nothing S.empty
    walkSuite :: Monad f => Maybe Name -> S.Set Name -> (S.Set Name -> Expr -> f Expr) -> Suite -> f Suite
    walkSuite owner locals f = mapM statement
      where statement (Decl l ds) = Decl l <$> mapM (walkDecl owner locals f) ds
            statement s = walkStmt (f locals) (walkSuite owner locals f) s
    walkDecl :: Monad f => Maybe Name -> S.Set Name -> (S.Set Name -> Expr -> f Expr) -> Decl -> f Decl
    walkDecl _ outer f d@Class{} = do
      b <- mapM classBody (dbody d)
      return d{dbody=b}
      where classBody (Decl l ds) = Decl l <$> mapM (walkDecl (Just $ dname d) outer f) ds
            classBody s = return s
    walkDecl owner outer f d@Def{}
      | not allowFunctions = return d
      | maybe False (\n -> M.member (GName (modname m) n) constructors || M.member (NoQ n) constructors) owner
      , dname d `elem` [initKW,altInit] = return d
      | otherwise = do
          let locals = outer `S.union` S.fromList (bound (pos d) ++ bound (kwd d) ++ assigned (dbody d))
          b <- walkSuite Nothing locals (if earlyHook then restricted f else f) (dbody d)
          return d{dbody=b}
    walkDecl _ _ _ d = return d
    restricted :: Monad f => (S.Set Name -> Expr -> f Expr) -> S.Set Name -> Expr -> f Expr
    restricted f locals e
      | ordinaryLocal e = f locals e
      | otherwise = descend (restricted f locals) e
    descend :: Monad f => (Expr -> f Expr) -> Expr -> f Expr
    descend f e | isStatic e = return e
    descend _ e@Let{} = return e
    descend f e = walkExpr f (walkModule $ \_ -> f) e

-- Empty extension initialization ------------------------------------------

-- Recognize only preprocessing-free files with a literal empty initial hook.
emptyExtensionInit :: String -> String -> Bool
emptyExtensionInit hook source = case tokens (splice $ newlines source) of
    Just ts | not ("??" `isInfixOf` source), not (any forbidden ts) ->
        any (`isPrefixOf` ts)
            [["void",hook,"(",")","{","}"],
             ["void",hook,"(","void",")","{","}"]]
    _ -> False
  where
    -- No directives anywhere: a later macro can also change the generated
    -- call to the hook, after this file has been included. Reject GNU/MS
    -- startup attributes and assembly rather than interpreting their effects.
    forbidden t = t `elem` ["#","%:","asm","__asm","__asm__",
                            "__attribute","__attribute__","__declspec",
                            "constructor","init_priority"]
-- C removes line splices before recognizing comments and directives.
newlines :: String -> String
newlines ('\r':'\n':ss) = '\n':newlines ss
newlines ('\r':ss) = '\n':newlines ss
newlines (c:ss) = c:newlines ss
newlines [] = []

splice :: String -> String
splice ('\\':'\r':'\n':ss) = splice ss
splice ('\\':'\n':ss) = splice ss
splice (c:ss) = c:splice ss
splice [] = []

tokens :: String -> Maybe [String]
tokens [] = Just []
tokens ('/':'/':ss) = tokens (dropWhile (/= '\n') ss)
tokens ('/':'*':ss) = block ss >>= tokens
  where block ('*':'/':rest) = Just rest
        block (_:rest) = block rest
        block [] = Nothing
tokens (q:ss) | q == '"' || q == '\'' = do
    rest <- quoted q ss
    ("literal":) <$> tokens rest
  where quoted q ('\\':_:rest) = quoted q rest
        quoted q (c:rest) | c == q = Just rest
        quoted _ ('\n':_) = Nothing
        quoted q (_:rest) = quoted q rest
        quoted _ [] = Nothing
tokens ('%':':':ss) = ("%:":) <$> tokens ss
tokens s@(c:ss)
    | isSpace c = tokens ss
    | identifier c = let (word,rest) = span identifier s in (word:) <$> tokens rest
    | otherwise = ([c]:) <$> tokens ss
  where identifier c = isAlphaNum c || c == '_' || c == '$'

-- Diagnostics -------------------------------------------------------------

-- Opt-in syntax counts, with static-witness classification supplied by CodeGen.
-- Counts describe syntax, not execution frequency. Defaults in type/parameter
-- metadata are deliberately excluded: only their expansions at call sites run.
-- Keep the typed environment even when inspecting later, class-converted trees,
-- so imported and local extension constructors retain their identity.
witnessStats :: Env0 -> (Expr -> Bool) -> String -> Module -> Value
witnessStats env static stage m = object
    [ "kind" .= ("witness-stats" :: String), "version" .= (1 :: Int)
    , "module" .= prstr (modname m), "stage" .= stage
    , "bindings" .= length bs
    , "alias_bindings" .= length [() | (_, e) <- bs, alias e]
    , "lambda_bindings" .= length [() | (_, Lambda{}) <- bs]
    , "static_bindings" .= length [() | (_, e) <- bs, static e]
    , "constructor_expressions" .= length calls
    , "nonstatic_constructor_expressions" .= length dynamic
    , "nonstatic_constructor_expressions_in_loops" .= length [() | (d,_) <- dynamic, d > 0]
    , "distinct_constructor_expressions" .= uniqueCount [e | (_,(_,e)) <- calls]
    , "constructors" .= objectCounts calls
    , "nonstatic_constructors" .= M.fromListWith (+) [(prstr n, 1 :: Int) | (_,n) <- dynamic]
    , "registry" .= registrySummary (buildRegistry resolve static m)
    ]
  where
    (bs, es) = statsSuite 0 (mbody m)
    -- Witness RHS recipes also identify primitive/generated witness classes
    -- that are not NExts in the original environment.
    known = S.fromList ([n | (_,e) <- bs, Just n <- [recipeHead e]] ++ map snd tupleWits)
    recipeHead (Dot _ e _) = recipeHead e
    recipeHead (Paren _ e) = recipeHead e
    recipeHead (Call _ f _ _) = headName f
    recipeHead _ = Nothing
    lookupQ (NoQ n) = lookupName n env
    lookupQ (GName mn n) | Just mn == thismod env = lookupName n env
    lookupQ q = lookupModuleInfo (mname q) env >>= (\mi -> moduleLookupName mi (noq q))
    witness n = S.member n known || case lookupQ n of
        Just NExt{} -> True
        _ -> False
    -- Preserve selected implementation names. Qualified constructor references
    -- are already resolved by reconstruction; unknown calls stay conservative.
    resolve n | witness n = Just (canonicalName n)
              | otherwise = Nothing
    -- Primitive/generated constructors need not have an interface entry. Keep
    -- this lookup total, including the bootstrap builtin environment.
    canonicalName q@(NoQ n) = case lookupName n env of
        Just (NAlias target) -> target
        _ -> maybe q (\mn -> GName mn n) (thismod env)
    canonicalName (QName mn n) = case lookupModuleInfo mn env of
        Just mi -> case moduleLookupName mi n of
            Just (NAlias target) -> target
            _ -> GName (moduleName mi) n
        Nothing -> GName mn n
    canonicalName q = q
    headName (TApp _ e _) = headName e
    headName (Var _ n) = Just n
    headName (Paren _ e) = headName e
    headName _ = Nothing
    calls = [(d, (n,e)) | (d,e@Call{}) <- es, Just n <- [headName (fun e)], witness n]
    dynamic = [(d, n) | (d,(n,e)) <- calls, not (static e)]
    alias Var{} = True
    alias (TApp _ e _) = alias e
    alias (Dot _ e _) = alias e
    alias (Paren _ e) = alias e
    alias _ = False
    objectCounts xs = M.fromListWith (+) [(prstr n, 1 :: Int) | (_, (n,_)) <- xs]

-- A small explicit walk keeps declaration metadata and evaluated expressions
-- separate. A declaration starts a new execution context for loop counts.
type Sites = ([(Maybe Type, Expr)], [(Int, Expr)])

statsSuite :: Int -> Suite -> Sites
statsSuite d = foldMap (statsStmt d)

statsStmt :: Int -> Stmt -> Sites
statsStmt d s = case s of
    Assign _ ps e -> ([(t,e) | PVar _ n t <- ps, isWitness n], []) <> foldMap (statsPatternSites d) ps <> statsExpr d e
    Expr _ e -> statsExpr d e
    MutAssign _ a b -> statsExpr d a <> statsExpr d b
    AugAssign _ a _ b -> statsExpr d a <> statsExpr d b
    Assert _ e me -> statsExpr d e <> foldMap (statsExpr d) me
    Delete _ e -> statsExpr d e
    Return _ me -> foldMap (statsExpr d) me
    Raise _ e -> statsExpr d e
    If _ bs els -> foldMap (\(Branch e b) -> statsExpr d e <> statsSuite d b) bs <> statsSuite d els
    While _ e b els -> statsExpr (d+1) e <> statsSuite (d+1) b <> statsSuite d els
    For _ p e b els -> statsExpr d e <> statsPatternSites (d+1) p <> statsSuite (d+1) b <> statsSuite d els
    Try _ b hs els fin -> statsSuite d b <> foldMap (\(Handler _ b) -> statsSuite d b) hs <> statsSuite d els <> statsSuite d fin
    With _ is b -> foldMap (\(WithItem e p) -> statsExpr d e <> foldMap (statsPatternSites d) p) is <> statsSuite d b
    Data _ p b -> foldMap (statsPatternSites d) p <> statsSuite d b
    VarAssign _ ps e -> foldMap (statsPatternSites d) ps <> statsExpr d e
    After _ _ a b -> statsExpr d a <> statsExpr d b
    Decl _ ds -> foldMap decl ds
    _ -> mempty
  where
    decl Typedef{} = mempty
    decl x = statsSuite 0 (dbody x)

statsExpr :: Int -> Expr -> Sites
statsExpr d e = ([], [(d,e)]) <> case e of
    Call _ f p k -> statsExpr d f <> pos p <> kwd k
    Let _ b x -> statsSuite d b <> statsExpr d x
    TApp _ x _ -> statsExpr d x
    Async _ x -> statsExpr d x
    Await _ x -> statsExpr d x
    Index _ x y -> statsExpr d x <> statsExpr d y
    Slice _ x (Sliz _ a b c) -> statsExpr d x <> foldMap (foldMap (statsExpr d)) [a,b,c]
    Cond _ a b c -> foldMap (statsExpr d) [a,b,c]
    IsInstance _ x _ -> statsExpr d x
    BinOp _ x _ y -> statsExpr d x <> statsExpr d y
    CompOp _ x ops -> statsExpr d x <> foldMap (\(OpArg _ y) -> statsExpr d y) ops
    UnOp _ _ x -> statsExpr d x
    Dot _ x _ -> statsExpr d x
    Rest _ x _ -> statsExpr d x
    DotI _ x _ -> statsExpr d x
    RestI _ x _ -> statsExpr d x
    Opt _ x _ -> statsExpr d x
    OptChain _ x -> statsExpr d x
    Lambda _ _ _ x _ -> statsExpr 0 x
    Yield _ x -> foldMap (statsExpr d) x
    YieldFrom _ x -> statsExpr d x
    Tuple _ p k -> pos p <> kwd k
    List _ xs -> foldMap elem xs
    ListComp _ x c -> elemAt (d + compDepth c) x <> compAt d c
    Dict _ xs -> foldMap assoc xs
    DictComp _ x c -> assocAt (d + compDepth c) x <> compAt d c
    Set _ xs -> foldMap elem xs
    SetComp _ x c -> elemAt (d + compDepth c) x <> compAt d c
    GeneratorExpr _ x c -> elemAt (d + compDepth c) x <> compAt d c
    Paren _ x -> statsExpr d x
    Box _ x -> statsExpr d x
    UnBox _ x -> statsExpr d x
    _ -> mempty
  where
    pos (PosArg x p) = statsExpr d x <> pos p
    pos (PosStar x) = statsExpr d x
    pos PosNil = mempty
    kwd (KwdArg _ x k) = statsExpr d x <> kwd k
    kwd (KwdStar x) = statsExpr d x
    kwd KwdNil = mempty
    elem = elemAt d
    elemAt depth (Elem x) = statsExpr depth x
    elemAt depth (Star x) = statsExpr depth x
    assoc = assocAt d
    assocAt depth (Assoc x y) = statsExpr depth x <> statsExpr depth y
    assocAt depth (StarStar x) = statsExpr depth x
    compAt depth (CompFor _ p x c) = statsExpr depth x <> statsPatternSites (depth+1) p <> compAt (depth+1) c
    compAt depth (CompIf _ x c) = statsExpr depth x <> compAt depth c
    compAt _ NoComp = mempty
    compDepth (CompFor _ _ _ c) = 1 + compDepth c
    compDepth (CompIf _ _ c) = compDepth c
    compDepth NoComp = 0

-- Fingerprints keep the diagnostic linear on the usual constructor recipes;
-- exact equality protects the distinct count against hash collisions.
uniqueCount :: [Expr] -> Int
uniqueCount es = sum (map length (IM.elems (foldl' add IM.empty es)))
  where
    add buckets e = IM.alter (Just . insert e . maybe [] id) (expressionHash e) buckets
    insert e xs | e `elem` xs = xs
                | otherwise = e:xs

statsPatternSites :: Int -> Pattern -> Sites
statsPatternSites d p = case p of
    PData _ _ es -> foldMap (statsExpr d) es
    PParen _ p -> statsPatternSites d p
    PTuple _ p k -> pos p <> kwd k
    PList _ ps tail -> foldMap (statsPatternSites d) ps <> foldMap (statsPatternSites d) tail
    _ -> mempty
  where
    pos (PosPat p ps) = statsPatternSites d p <> pos ps
    pos (PosPatStar p) = statsPatternSites d p
    pos PosPatNil = mempty
    kwd (KwdPat _ p ps) = statsPatternSites d p <> kwd ps
    kwd (KwdPatStar p) = statsPatternSites d p
    kwd KwdPatNil = mempty
