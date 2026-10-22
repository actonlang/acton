-- Copyright (C) 2026 Acton contributors
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- Closed witnesses have no activation-dependent state. Materialize their DAG
-- once, after imported modules and local method tables are ready, using the
-- ordinary module-global initializer machinery and ordinary serializable objects.
module Acton.WitnessPooling (poolWitnesses, poolModule) where

import Control.Monad.State.Strict
import Data.Functor.Identity
import Data.List (foldl')
import Data.Maybe (mapMaybe, isJust)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Acton.Syntax hiding (expr)
import Acton.Builtin (initKW, mBuiltin)
import Acton.Names (assigned, bound, free, isWitness, altInit)
import Acton.Subst (vfree)
import Acton.Env (Env0)
import Acton.WitnessKey (expressionHash)
import Utils (SrcLoc(..))
import qualified Acton.WitnessSharing as L
import qualified Acton.WitnessRegistry as R

type Constructors = M.Map QName (QName,Int)

poolWitnesses :: Env0 -> S.Set QName -> (Expr -> Maybe Expr) -> Module -> Module
poolWitnesses env known staticValue m = poolModule (L.witnessConstructors env known m) staticValue m

poolModule :: Constructors -> (Expr -> Maybe Expr) -> Module -> Module
poolModule constructors staticValue m
    | modname m == mBuiltin = m
    | IM.null globals = m
    | otherwise = m{mbody = initializers ++ rewrite (mbody m)}
  where
    isStatic = isJust . staticValue
    resolve q = fst <$> M.lookup q constructors
    registry = R.buildRegistry resolve isStatic m
    recipes = R.registryRecipes registry
    at i = recipes IM.! i
    -- Retain every original spelling's other interpretations when deciding
    -- ambiguity. A closed occurrence must never rewrite a captured one.
    groups = foldl' (\a s -> IM.insertWith (++) (expressionHash $ R.siteExpression s) [s] a)
                    IM.empty (R.registrySites registry)
    index = IM.map candidates groups
    candidates ss | any (\s -> R.recipeLifetime (at $ R.siteRecipe s) == R.Closed && not (R.siteStatic s)) ss
                            = mapMaybe unique (partitionExpressions ss)
                  | otherwise = []
    partitionExpressions = foldl' insert []
      where insert [] s = [[s]]
            insert (xs:xss) s | R.siteExpression (head xs) == R.siteExpression s = (s:xs):xss
                              | otherwise = xs:insert xss s
    unique xs | [i] <- S.toList (S.fromList $ map R.siteRecipe xs)
              , R.recipeLifetime (at i) == R.Closed
              , not (isStatic $ R.siteExpression $ head xs) = Just (R.siteExpression $ head xs,i)
              | otherwise = Nothing
    lookupSite locals e = case L.constructorCall e of
      Just (NoQ n,_) | S.member n locals -> Nothing
      _ -> lookup e (IM.findWithDefault [] (expressionHash e) index)

    -- Class-table expressions can call functions before the pool is ready.
    -- A C hook runs even earlier, before local tables. In that case a normal
    -- local witness constructor is already unusable (its allocator calls the
    -- table's __init__), so replacing that call after tables are ready is safe.
    -- Imported constructors and closure allocators have no such prerequisite.
    allowFunctions = all safeTable (mbody m)
    earlyHook = hasNotImpl (mbody m)
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
    ordinaryLocal e = case L.constructorCall e >>= (\(q,_) -> fst <$> M.lookup q constructors) >>= localName of
      Just (Internal LLiftPass _ _) -> False
      Just n -> M.member n localClasses
      _ -> False
    topNames = S.fromList [n | Assign _ [PVar _ n (Just _)] _ <- mbody m, isWitness n]
    bindingGroups = M.fromListWith (++) [(R.bindingName b,[b]) | b <- R.registryBindings registry
                     , R.bindingScope b == 0, S.member (R.bindingName b) topNames]
    topBindings = M.mapMaybe one bindingGroups
    one [b] | Just t@TCon{} <- R.bindingType b, null (vfree t) = Just b
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
          return (map R.bindingRecipe bs ++ inherited)
    runtimeNames d = S.fromList $ mapMaybe runtimeName $ S.toList qs
      where qs = execState (mapM_ statement $ dbody d) S.empty
            locals = S.fromList (bound (pos d) ++ bound (kwd d) ++ assigned (dbody d))
            runtimeName (NoQ n) | S.member n locals = Nothing
            runtimeName q = case localName q of
              Just (Derived c k) | k `elem` [initKW,altInit], M.member c localClasses -> Just c
              n -> n
            statement s = L.walkStmt expression (mapM statement) s
            expression e = do
              case e of Var _ q -> modify (S.insert q); _ -> return ()
              L.walkExpr expression (mapM statement) e
    hidden (GName mn n) | mn == modname m = M.findWithDefault Nothing n initNeeds
    hidden (NoQ n) = M.findWithDefault (Just []) n initNeeds
    hidden _ = Just []
    edges = IM.map deps recipes
    deps r | R.recipeLifetime r /= R.Closed = Nothing
           | isStatic (R.recipeExpression r) = Just []
           | otherwise = case R.recipeKind r of
               R.Constructor q -> do
                 (_,arity) <- M.lookup q constructors
                 (_,ts) <- L.constructorCall (R.recipeExpression r)
                 if length ts /= arity then Nothing else
                   (S.toList (R.recipeDependencies r) ++) <$> hidden q
               R.Projection{} -> Just (S.toList $ R.recipeDependencies r)
               R.Instantiation -> Just (S.toList $ R.recipeDependencies r)
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
    pooled = [i | i <- order, R.Constructor{} <- [R.recipeKind $ at i]]
    bindings = IM.fromListWith (++) [(R.bindingRecipe b,[b]) | b <- M.elems topBindings]
    reserved = S.fromList (free (mbody m) ++ bound (mbody m) ++ map R.bindingName (R.registryBindings registry))
    (_,globals) = foldl' allocate (reserved,IM.empty) pooled
    allocate (used,gs) i = (S.insert n used,IM.insert i (n,t) gs)
      -- Preserve the concrete constructor type: callers can project embedded
      -- super-witness fields which are absent from a protocol-typed alias.
      where candidates = filter ((== Just (concrete i)) . R.bindingType) (IM.findWithDefault [] i bindings)
            (n,t) = case candidates of
              b:_ -> (R.bindingName b, maybe (error "pool: missing type") id (R.bindingType b))
              [] -> (head [n | k <- [i..], let n = Internal Witness "module" k, S.notMember n used], concrete i)
    concrete i = case L.constructorCall (R.recipeExpression $ at i) of
      Just (q,ts) -> tCon (TC q ts)
      _ -> error "pool: constructor recipe without call"
    global i = eQVar $ GName (modname m) (fst $ globals IM.! i)
    reference (Var _ (NoQ (Internal Witness "registry-recipe" i))) = Just i
    reference _ = Nothing
    expand useGlobals e
      | Just i <- reference e = case IM.lookup i globals of
          Just _ | useGlobals -> global i
          _ -> materialize useGlobals i
      | otherwise = runIdentity $ L.walkExpr (Identity . expand useGlobals) (Identity . id) e
    materialize useGlobals i
      | Just e' <- staticValue e = e'
      | otherwise = expand (useGlobals && not (isStatic e)) e
      where e = R.recipeExpression (at i)
    value i | IM.member i globals = global i
            | otherwise = materialize True i
    declarations i = ctor ++ aliases
      where ctor = case IM.lookup i globals of
              Just (n,t) -> [Assign NoLoc [pVar n t] (materialize True i)]
              Nothing -> []
            aliases = [Assign NoLoc [pVar (R.bindingName b) t] (value i)
                       | b <- IM.findWithDefault [] i bindings, Just t <- [R.bindingType b]
                       , maybe True ((/= R.bindingName b) . fst) (IM.lookup i globals)]
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
            statement s = L.walkStmt (f locals) (walkSuite owner locals f) s
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
    descend f e = L.walkExpr f (walkModule $ \_ -> f) e
