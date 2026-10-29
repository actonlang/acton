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
-- keys, the binding-aware registry, local sharing, module pooling and opt-in
-- diagnostics. They describe and reuse ordinary witness objects; there is no
-- runtime registry or alternate serialization.
--
-- Both passes find construction sites by their spelling within a declaration.
-- Equal spellings can read different bindings: a default argument expanded at
-- several call sites assigns its witness names once per expansion, with the
-- types of that call. They still denote interchangeable witnesses. A
-- construction's spelling includes its type arguments, which fix the types
-- of the witnesses it is built from, and witnesses of the same protocol and
-- type are interchangeable. Types are erased at runtime, so type variables
-- stay in the spelling and capture nothing: a construction whose only
-- parameters are types builds the same object on every call.
module Acton.WitnessSharing
    ( witnessHash, expressionHash
    , WitnessRegistry(..), Recipe(..), RecipeKind(..), Lifetime(..)
    , Capture(..), CaptureKind(..), Binding(..), Site(..)
    , buildRegistry, buildFunctionRegistry
    , shareWitnesses, shareFunction, constructorHeads, witnessConstructors
    , poolWitnesses, poolModule, sharingStats
    ) where

import Control.Monad (foldM, forM, forM_, when)
import Control.Monad.State.Strict
import Data.Aeson (Value, object, (.=))
import Data.Functor.Identity
import Data.Graph (SCC(..), stronglyConnComp)
import Data.Hashable (hash)
import Data.List (foldl', partition)
import Data.Maybe (mapMaybe, isJust)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Acton.Syntax hiding (expr, comp, target)
import Acton.Builtin (initKW)
import Acton.Names (assigned, bound, free, isWitness, altInit)
import Acton.Env (Env0, lookupName, lookupModuleInfo, moduleLookupName, moduleName, thismod, setMod, define)
import Acton.NameInfo (NameInfo(..))
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
data Lifetime = Closed | ParameterDependent | Untracked
    deriving (Eq, Ord, Show)

data CaptureKind = WitnessParameter | UntrackedValue
    deriving (Eq, Ord, Show)

-- Binding identities distinguish equal spellings in different activations.
data Capture = Capture
    { captureId :: Int, captureName :: Name
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
    , bindingScope :: Int
    } deriving (Show)

data Site = Site
    { siteRecipe :: Int, siteScope :: Int, siteStatic :: Bool
    , siteExpression :: Expr, siteContext :: [Name]
    } deriving (Show)

data WitnessRegistry = WitnessRegistry
    { registryRecipes :: IM.IntMap Recipe, registryBindings :: [Binding]
    , registrySites :: [Site]
    } deriving (Show)

data Term = WitnessValue Int | CapturedValue Capture
data Lex = Lex
    { scope :: Int, terms :: M.Map Name Term
    , placementContext :: [Name]
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
initialLex = Lex 0 M.empty []

build :: Maybe ModName -> (QName -> Maybe QName) -> (Expr -> Bool) -> BuildM () -> WitnessRegistry
build mn resolve isStatic action = finish $ execState action initial
  where
    initial = Build (WitnessRegistry IM.empty [] []) IM.empty 0 resolve isStatic mn M.empty
    finish s = let g = graph s in g
        { registryBindings = reverse (registryBindings g)
        , registrySites = reverse (registrySites g) }

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
    return env{scope = n}

capture :: Lex -> CaptureKind -> Name -> BuildM Capture
capture _ k n = do i <- fresh; return (Capture i n k)

bindValues :: Lex -> [Name] -> BuildM Lex
bindValues = foldM $ \env n -> do
    c <- capture env UntrackedValue n
    when (scope env == 0) $ modify $ \s -> s{globalWitnesses = M.delete n (globalWitnesses s)}
    return env{terms = M.insert n (CapturedValue c) (terms env)}

bindParameters :: Lex -> [Name] -> BuildM Lex
bindParameters = foldM $ \env n -> do
    c <- capture env (if isWitness n then WitnessParameter else UntrackedValue) n
    return env{terms = M.insert n (CapturedValue c) (terms env)}

life :: S.Set Capture -> Lifetime
life = S.foldl' (\a c -> max a $ case captureKind c of
    WitnessParameter -> ParameterDependent
    UntrackedValue -> Untracked) Closed

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

headName :: Expr -> Maybe QName
headName (Var _ q) = Just q
headName (TApp _ e _) = headName e
headName (Paren _ e) = headName e
headName _ = Nothing

constructorHead :: Expr -> BuildM (Maybe QName)
constructorHead f = case headName f of
    Nothing -> return Nothing
    Just q -> gets (($ q) . constructor)

constructorFun :: QName -> Expr -> Expr
constructorFun q (TApp l f ts) = TApp l (constructorFun q f) ts
constructorFun q (Paren _ f) = constructorFun q f
constructorFun q _ = eQVar q

expr :: Lex -> Expr -> BuildM Resolved
expr env e@(Var _ (NoQ n)) = case M.lookup n (terms env) of
    Just (WitnessValue i) -> fromNode i
    Just (CapturedValue c) | isWitness n -> makeNode RuntimeWitness (eVar (valueRef c)) [] (S.singleton c)
    Just (CapturedValue c) -> return (Resolved (eVar (valueRef c)) S.empty (S.singleton c) Nothing)
    Nothing -> do
        c <- capture env UntrackedValue n
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
      Just i -> fromNode i
      Nothing -> do
        c <- capture env UntrackedValue (noq $ var e)
        return (Resolved e S.empty (S.singleton c) Nothing)
expr env e@(Call l f p k) = do
    ctor <- constructorHead f
    (p',ps) <- positional env p
    (k',ks) <- keywords env k
    case ctor of
      Just q -> do
        r <- makeNode (Constructor q) (Call l (constructorFun q f) p' k') (ps++ks) S.empty
        let i = case node r of Just i -> i; Nothing -> error "registry: constructor without node"
        isStatic <- gets (($ e) . staticRecipe)
        modifyGraph $ \g -> g{registrySites = Site i (scope env) isStatic e (placementContext env) : registrySites g}
        return r
      Nothing -> do
        f' <- expr env f
        c <- capture env UntrackedValue (name "call")
        let (ds,cs) = combined (f':ps++ks)
        return (Resolved (Call l (canonical f') p' k') ds (S.insert c cs) Nothing)
expr env (Paren _ e) = expr env e
expr env (TApp l e ts) = do
    r <- expr env e
    case node r of
      Just _ -> makeNode Instantiation (TApp l (canonical r) ts) [r] S.empty
      Nothing -> return (Resolved (TApp l (canonical r) ts) (dependencies r) (captures r) Nothing)
expr env e@(Dot l x n) = do
    r <- expr env x
    case node r of
      Just _ -> makeNode (Projection n) (Dot l (canonical r) n) [r] S.empty
      Nothing -> opaque env e [r]
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
    c <- capture env UntrackedValue (name "expression")
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

suite :: Lex -> Suite -> BuildM Lex
suite = foldM stmt

stmt :: Lex -> Stmt -> BuildM Lex
stmt env (Assign _ [PVar _ n t] e) | isWitness n = do
    r <- expr env e
    i <- case node r of
      Just i -> return i
      Nothing -> intern Opaque (canonical r) (dependencies r) (captures r)
    modifyGraph $ \g -> g{registryBindings = Binding n t i (scope env) : registryBindings g}
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
        suite env els >> return env
    While _ e b els -> expr env e >> suite env b >> suite env els >> return env
    For _ p e b els -> do
        expr env e
        inner <- bindValues env (bound p)
        patternExprs inner p
        suite inner b >> suite env els >> return env
    Try _ b hs els fin -> do
        suite env b
        forM_ hs $ \(Handler ex h) -> bindValues env (bound ex) >>= (\a -> suite a h)
        suite env els >> suite env fin >> return env
    With _ is b -> do
        forM_ is $ \(WithItem e p) -> expr env e >> mapM_ (patternExprs env) p
        bindValues env (bound is) >>= (\a -> suite a b) >> return env
    Data _ p b -> do
        mapM_ (patternExprs env) p
        bindValues env (bound p) >>= (\a -> suite a b) >> return env
    Decl _ ds -> mapM_ (decl env) ds >> return env
    _ -> return env
  where visit es = mapM_ (expr env) es >> return env

target :: Expr -> [Name]
target (Var _ (NoQ n)) = [n]
target (Paren _ e) = target e
target _ = []

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
    inner <- newScope env{placementContext = dname d : placementContext env}
    case d of
      Def{pos=p,kwd=k} -> bindParameters inner (bound p ++ bound k) >>= body
      Actor{pos=p,kwd=k} -> bindParameters inner (bound p ++ bound k) >>= body
      _ -> body inner
  where body a = suite a (dbody d) >> return ()

element :: Elem -> Expr
element (Elem e) = e
element (Star e) = e
association :: Assoc -> [Expr]
association (Assoc a b) = [a,b]
association (StarStar e) = [e]

-- Defaults in declaration/type metadata are not evaluated here. Their expanded
-- call-site expressions are visited normally.
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

-- Function-local sharing --------------------------------------------------

-- Run after boxing's dead-witness removal. Reuse only dominated constructions.
type ConstructorInfo = QName -> Maybe QName

-- Witness constructors are the classes that witness equations construct.
-- Keep the names from before boxing: boxing may remove their original
-- equations while leaving inline constructions at their uses.
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
    constructors = witnessConstructors env known m
    info q = M.lookup q constructors
    top (Decl l ds) = Decl l (map decl ds)
    top s = s
    decl d@Def{} = shareFunction info isStatic d
    decl d@Class{} = d{dbody = map top (dbody d)}
    decl d = d

witnessConstructors :: Env0 -> S.Set QName -> Module -> M.Map QName QName
witnessConstructors env known m = constructors
  where
    env1 = define (envOf $ mbody m) (setMod (modname m) env)
    constructors = M.fromList $ concatMap entry (S.toList known)
    entry q = case lookupQ S.empty q of
      Just (q', NClass{}) -> [(q,q'), (q',q')]
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
    registry = buildFunctionRegistry info isStatic d
    at i = registryRecipes registry IM.! i
    sites = filter ((== 1) . siteScope) (registrySites registry)
    parameters = M.fromList (parTypes (pos d) ++ kwdTypes (kwd d))
    safeCapture c = case captureKind c of
      WitnessParameter -> case M.lookup (captureName c) parameters of
        Just TCon{} -> True
        _ -> False
      _ -> False
    eligible s = not (siteStatic s) && recipeLifetime r <= ParameterDependent &&
                 all safeCapture (S.toList $ recipeCaptures r)
      where r = at (siteRecipe s)
    shared = filter eligible sites
    counts = IM.fromListWith (+) [(siteRecipe s,1::Int) | s <- shared]
    plans = foldl' addPlan IM.empty shared
    addPlan ps s
      | IM.findWithDefault 0 (siteRecipe s) counts < 2 = ps
      | Just _ <- lookupPlan ps (siteExpression s) = ps
      | Just (q,ts) <- constructorCall (siteExpression s)
      , Just canonical <- info q
      = IM.insertWith (++) (expressionHash $ siteExpression s)
          [(siteExpression s,Plan (siteRecipe s) (tCon $ TC canonical ts))] ps
      | otherwise = ps
    names = S.fromList (free d ++ bound (pos d) ++ bound (kwd d) ++
                       [n | QBind (TV _ n) _ <- qbinds d] ++ map bindingName (registryBindings registry))
    initial = Sharing IM.empty [] 0 names [] M.empty
    (body,done) = runState (rewriteSuite plans True (dbody d)) initial
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

rewriteSuite :: Plans -> Bool -> Suite -> Rewrite Suite
rewriteSuite _ _ [] = return []
rewriteSuite plans strict (s:ss) = do
    s' <- rewriteStmt plans strict s
    pre <- gets (reverse . prefixRev)
    modify $ \a -> a{prefixRev = []}
    ss' <- rewriteSuite plans strict ss
    return (pre ++ s':ss')

rewriteStmt :: Plans -> Bool -> Stmt -> Rewrite Stmt
rewriteStmt plans strict (If l bs els) = do
    bs' <- forM (zip [0::Int ..] bs) $ \(i,Branch e b) -> do
        e' <- rewriteExpr plans (strict && i == 0) e
        b' <- region (rewriteSuite plans strict b)
        return (Branch e' b')
    els' <- region (rewriteSuite plans strict els)
    return (If l bs' els')
rewriteStmt plans strict s = walkStmt (rewriteExpr plans strict) (region . rewriteSuite plans strict) s

rewriteExpr :: Plans -> Bool -> Expr -> Rewrite Expr
rewriteExpr plans strict e = case lookupPlan plans e of
    Just p -> do
        old <- gets (IM.lookup (planRecipe p) . available)
        case old of
          Just n -> use n
          Nothing | strict -> rebuild >>= newBinding p
          _ -> rebuild
    Nothing -> rebuild
  where
    visit = rewriteExpr plans strict
    conditional = rewriteExpr plans False
    rebuild = case e of
      Cond l a c b -> do c' <- visit c; a' <- conditional a; b' <- conditional b; return (Cond l a' c' b')
      BinOp l a op b | op == And || op == Or -> BinOp l <$> visit a <*> pure op <*> conditional b
      _ -> walkExpr visit e

replace :: M.Map Name Expr -> Expr -> Expr
replace replacements e@(Var _ (NoQ n)) = M.findWithDefault e n replacements
replace replacements e = runIdentity $ walkExpr (Identity . replace replacements) e
substitute :: M.Map Name Expr -> Suite -> Suite
substitute replacements = map (runIdentity . walkStmt (Identity . replace replacements)
                                                     (Identity . substitute replacements))
pruneDefinitions :: S.Set Name -> Suite -> Suite
pruneDefinitions removed = concatMap prune
  where
    prune (Assign _ [PVar _ n _] _) | S.member n removed = []
    prune s = [runIdentity $ walkStmt (Identity . expr) (Identity . pruneDefinitions removed) s]
    expr e = runIdentity $ walkExpr (Identity . expr) e

-- These walks visit evaluated expressions only. Declaration/type/default
-- metadata and nested functions belong to other execution contexts. Let
-- expressions, lambdas and comprehensions do not reach this stage: CPS and
-- lambda lifting replace them by statements and functions.
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

walkExpr :: Monad m => (Expr -> m Expr) -> Expr -> m Expr
walkExpr e x = case x of
    Call l f p k -> Call l <$> e f <*> pos p <*> kwd k
    TApp l f ts -> TApp l <$> e f <*> pure ts
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
type Constructors = M.Map QName QName

-- A pool dependency: a witness recipe, or the initializer of a class of the
-- module.
data PoolNode = RecipeNode Int | Initializer Name
    deriving (Eq, Ord)

poolWitnesses :: Env0 -> S.Set QName -> (Expr -> Maybe Expr) -> Module -> Module
poolWitnesses env known staticValue m = poolModule (witnessConstructors env known m) staticValue m

poolModule :: Constructors -> (Expr -> Maybe Expr) -> Module -> Module
poolModule constructors staticValue m
    | IM.null globals = m
    | otherwise = m{mbody = initializers ++ rewrite (mbody m)}
  where
    isStatic = isJust . staticValue
    resolve q = M.lookup q constructors
    registry = buildRegistry resolve isStatic m
    recipes = registryRecipes registry
    at i = recipes IM.! i
    -- Equal spellings in different declarations can refer to different
    -- bindings, notably a closed outer witness and a lifted helper parameter,
    -- so closed sites are indexed per declaration.
    index = M.fromListWith (IM.unionWith (++))
        [(siteContext s, IM.singleton (expressionHash e) [(e,siteRecipe s)])
        | s <- registrySites registry, let e = siteExpression s
        , recipeLifetime (at $ siteRecipe s) == Closed, not (siteStatic s)]
    lookupSite context e = lookup e (IM.findWithDefault [] (expressionHash e) (M.findWithDefault IM.empty context index))

    localClasses = M.fromList [(n,d) | Decl _ ds <- mbody m, d@Class{dname=n} <- ds]
    moduleNames = S.fromList (bound (mbody m))
    localName (NoQ n) = Just n
    localName (GName mn n) | mn == modname m = Just n
    localName _ = Nothing
    topNames = S.fromList [n | Assign _ [PVar _ n (Just _)] _ <- mbody m, isWitness n]
    bindingGroups = M.fromListWith (++) [(bindingName b,[b]) | b <- registryBindings registry
                     , bindingScope b == 0, S.member (bindingName b) topNames]
    topBindings = M.mapMaybe one bindingGroups
    one [b] | recipeLifetime (at $ bindingRecipe b) == Closed = Just b
    one _ = Nothing

    -- The pool runs before the module's statements, so a construction must
    -- follow everything it reads: the witnesses it is built from and, for a
    -- class of this module, what its initializer reads. A witness class is
    -- generated from a protocol or extension declaration. Its initializer
    -- stores its arguments, calls its parent's initializer and constructs its
    -- sibling witnesses. The module state it reads is the module's witnesses,
    -- the initializers of the module's classes, and the pooled witnesses
    -- that replace constructions in it.
    edges (RecipeNode i)
      | isStatic (recipeExpression r) = []
      | otherwise = map RecipeNode (S.toList $ recipeDependencies r) ++ owner (recipeKind r)
      where r = at i
            owner (Constructor q) | Just n <- localName q, M.member n localClasses = [Initializer n]
            owner _ = []
    edges (Initializer c) = map reads (S.toList $ S.intersection names moduleNames) ++
                            map RecipeNode (S.toList $ M.findWithDefault S.empty c initializerUses)
      where names = S.unions [runtimeNames f | Decl _ fs <- dbody (localClasses M.! c), f@Def{dname=k} <- fs, k `elem` [initKW,altInit]]
            reads n | M.member n localClasses = Initializer n
                    | Just b <- M.lookup n topBindings = RecipeNode (bindingRecipe b)
                    | otherwise = error ("witness pool: the initializer of " ++ prstr c ++ " reads " ++ prstr n)
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
              walkExpr expression e

    -- First discover actual eligible occurrences.
    (_, (requested,initializerUses)) = runState (walkModule find (mbody m)) (S.empty,M.empty)
    find context e = do
      case lookupSite context e of
        Just i -> modify $ \(rs,uses) ->
          (S.insert i rs, case context of
             [k,c] | k `elem` [initKW,altInit], M.member c localClasses -> M.insertWith S.union c (S.singleton i) uses
             _ -> uses)
        _ -> return ()
      walkExpr (find context) e
    -- Pooled witnesses are initialized in dependency order. Initializers can
    -- depend on each other in a cycle: two extensions whose witnesses need
    -- each other construct the other with themselves as an argument. A cycle
    -- through a witness would need that witness for its own construction.
    reachable = closure S.empty (map RecipeNode $ S.toList requested)
    closure seen [] = seen
    closure seen (n:ns) | S.member n seen = closure seen ns
                        | otherwise = closure (S.insert n seen) (edges n ++ ns)
    order = concatMap component $ stronglyConnComp [(n, n, edges n) | n <- S.toList reachable]
    component (AcyclicSCC n) = [n]
    component (CyclicSCC ns)
      | null [i | RecipeNode i <- ns] = ns
      | otherwise = error ("witness pool: cyclic witness construction in " ++ prstr (modname m))
    -- Static dependencies also need named witness bindings: C generation turns
    -- their RHS into an existing builtin object. Inlining Ctor() as an argument
    -- would instead call its allocator, even for a builtin static witness.
    recipeOrder = [i | RecipeNode i <- order]
    pooled = [i | i <- recipeOrder, Constructor{} <- [recipeKind $ at i]]
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
      | otherwise = runIdentity $ walkExpr (Identity . expand useGlobals) e
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
    initializers = concatMap declarations recipeOrder
    moved = S.fromList [n | Assign _ [PVar _ n _] _ <- initializers]
    rewrite ss = filter (not . movedStmt) $ runIdentity $ walkModule (\context e -> Identity $ replace context e) ss
    movedStmt (Assign _ [PVar _ n _] _) = S.member n moved
    movedStmt _ = False
    replace context e | Just i <- lookupSite context e, IM.member i globals = global i
    replace context e = runIdentity $ walkExpr (Identity . replace context) e

    -- Closed initializer sites use the same binding-aware lookup as other
    -- functions. Their pool reads have explicit dependency edges above; the
    -- graph must be valid before either a use or its constructor is rewritten.
    walkModule :: Monad f => ([Name] -> Expr -> f Expr) -> Suite -> f Suite
    walkModule = walkSuite []
    walkSuite :: Monad f => [Name] -> ([Name] -> Expr -> f Expr) -> Suite -> f Suite
    walkSuite context f = mapM statement
      where statement (Decl l ds) = Decl l <$> mapM (walkDecl context f) ds
            statement s = walkStmt (f context) (walkSuite context f) s
    walkDecl :: Monad f => [Name] -> ([Name] -> Expr -> f Expr) -> Decl -> f Decl
    walkDecl context f d@Class{} = do
      b <- mapM classBody (dbody d)
      return d{dbody=b}
      where classBody (Decl l ds) = Decl l <$> mapM (walkDecl (dname d:context) f) ds
            classBody s = return s
    walkDecl context f d@Def{} = do
      b <- walkSuite (dname d:context) f (dbody d)
      return d{dbody=b}
    walkDecl _ _ d = return d

-- Diagnostics -------------------------------------------------------------

-- For --witness-stats: the witness constructions of a module after sharing.
-- Those at module level run once. Those left in functions and methods run on
-- every call, and are counted by what they depend on.
sharingStats :: (QName -> Maybe QName) -> (Expr -> Bool) -> Module -> Value
sharingStats resolve isStatic m = object
    [ "module" .= prstr (modname m)
    , "module_level" .= length moduleLevel
    , "functions_closed" .= count Closed
    , "functions_parameter_dependent" .= count ParameterDependent
    , "functions_untracked" .= count Untracked
    ]
  where
    g = buildRegistry resolve isStatic m
    (moduleLevel, functions) = partition ((== 0) . siteScope) $ filter (not . siteStatic) (registrySites g)
    count l = length [() | s <- functions, recipeLifetime (registryRecipes g IM.! siteRecipe s) == l]
