-- Copyright (C) 2026 Acton contributors
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedStrings #-}

-- Reuse witnesses inside one generated function. Run after boxing's dead
-- witness removal, so an unused equation cannot become an eager allocation.
module Acton.WitnessSharing (shareWitnesses, shareFunction, constructorHeads) where

import Control.Monad (forM)
import Control.Monad.State.Strict
import Data.Functor.Identity
import Data.List (foldl')
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Acton.Syntax hiding (expr)
import Acton.Names (assigned, bound, free, isWitness, altInit)
import Acton.Builtin (initKW)
import Acton.Env (Env0, lookupName, lookupModuleInfo, moduleLookupName, moduleName, thismod, setMod, define)
import Acton.NameInfo (NameInfo(..))
import Acton.QuickType (envOf)
import Acton.WitnessKey (expressionHash)
import qualified Acton.WitnessRegistry as R

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
    info q = M.lookup q constructors
    top (Decl l ds) = Decl l (map decl ds)
    top s = s
    decl d@Def{} = shareFunction info isStatic d
    decl d@Class{} = d{dbody = map top (dbody d)}
    decl d = d

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
    registry = R.buildFunctionRegistry resolve isStatic d
    at i = R.registryRecipes registry IM.! i
    sites = filter ((== 1) . R.siteScope) (R.registrySites registry)
    parameters = M.fromList (parTypes (pos d) ++ kwdTypes (kwd d))
    safeCapture c = case R.captureKind c of
      R.TypeParameter -> True
      R.WitnessParameter -> case M.lookup (R.captureName c) parameters of
        Just TCon{} -> True
        _ -> False
      _ -> False
    eligible s = not (R.siteStatic s) && R.recipeLifetime r <= R.ParameterDependent &&
                 all safeCapture (S.toList $ R.recipeCaptures r)
      where r = at (R.siteRecipe s)
    groups = foldl' add IM.empty sites
    add buckets s = IM.insertWith (++) (expressionHash $ R.siteExpression s) [s] buckets
    distinct ss = foldl' insert [] ss
      where insert [] s = [[s]]
            insert (xs:xss) s | R.siteExpression (head xs) == R.siteExpression s = (s:xs):xss
                              | otherwise = xs:insert xss s
    -- Unsupported expressions can share a fingerprint bucket. There is no
    -- reason to compare all its expressions when none can be shared.
    unambiguous = [s | bucket <- IM.elems groups, any eligible bucket, xs <- distinct bucket
                     , S.size (S.fromList $ map R.siteRecipe xs) == 1, s <- xs, eligible s]
    counts = IM.fromListWith (+) [(R.siteRecipe s,1::Int) | s <- unambiguous]
    plans = foldl' addPlan IM.empty unambiguous
    addPlan ps s
      | IM.findWithDefault 0 (R.siteRecipe s) counts < 2 = ps
      | Just _ <- lookupPlan ps (R.siteExpression s) = ps
      | Just (q,ts) <- constructorCall (R.siteExpression s)
      , Just (canonical,arity) <- info q, length ts == arity
      = IM.insertWith (++) (expressionHash $ R.siteExpression s)
          [(R.siteExpression s,Plan (R.siteRecipe s) (tCon $ TC canonical ts))] ps
      | otherwise = ps
    names = S.fromList (free d ++ bound (pos d) ++ bound (kwd d) ++
                       [n | QBind (TV _ n) _ <- qbinds d] ++ map R.bindingName (R.registryBindings registry))
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
rewriteStmt plans strict s = walkStmt (rewriteExpr plans strict)
                                        (region . rewriteSuite plans strict) s

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
      -- A Let can shadow names without introducing a registry execution scope.
      -- Leave it intact rather than applying an outer expression index there.
      Let{} -> return e
      _ -> walkExpr visit (region . rewriteSuite plans False) e

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
