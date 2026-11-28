-- Copyright (C) 2026 Data Ductus AB
module Acton.InitializationAnalysis (Safety(..), analyzeModule, analyzeModules) where

import Control.Monad (forM_, void)
import Control.Monad.Writer.Strict (Writer, execWriter, tell)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Acton.Syntax hiding (body)
import Utils (SrcLoc(..))
import Acton.Names (assigned, bound, free, isWitness)
import Acton.Prim (primCAST, primWRAP)

data Safety = Safety
    { parameterSafety :: M.Map QName [(Name, Bool)]
    , selfSafety :: M.Map QName Bool
    } deriving (Eq, Show)
data Variable = Local QName Name | Field QName Name | Self QName deriving (Eq, Ord)
data Rule = Bad Variable | Needs Variable Variable
data Parameter = Parameter { pname :: Name, positional :: Bool, variadic :: Bool }
data Constructor = Constructor
    { cname :: QName, actor :: Bool, parameters :: [Parameter], receiver :: Name
    , body :: Suite, methods :: S.Set Name, resolve :: QName -> QName
    , qualifiers :: S.Set ModName, unavailable :: Bool, sourceInitializer :: Bool
    }

analyzeModule :: Module -> Safety
analyzeModule m = analyzeModules [(m,id)]

analyzeModules :: [(Module, QName -> QName)] -> Safety
analyzeModules inputs = Safety (M.map result cs)
    (M.map (\c -> S.notMember (Self $ cname c) unsafe) $ M.filter (\c -> (actor c || sourceInitializer c) && not (unavailable c)) cs)
  where
    cs = M.map unique $ M.fromListWith (++)
        [(cname c,[c]) | (m,r) <- inputs, c <- collect m r (mbody m)]
    unique [c] = c
    unique (c:_) = c { unavailable = True }
    unique [] = error "empty constructor group"
    rules = concatMap (constraints cs) (M.elems cs)
    unsafe = fix (S.fromList [v | Bad v <- rules])
    fix bad = let next = S.union bad $ S.fromList [v | Needs v w <- rules, S.member w bad]
              in if next == bad then bad else fix next
    result c = [(pname p, S.notMember (variable c $ pname p) unsafe)
               | p <- parameters c, not (isWitness $ pname p)]

variable c n = (if actor c then Field else Local) (cname c) n
require :: S.Set Variable -> Variable -> Writer [Rule] ()
require vs target = tell [Needs v target | v <- S.toList vs]
reject :: S.Set Variable -> Writer [Rule] ()
reject vs = tell [Bad v | v <- S.toList vs]

collect m r = concatMap declaration . concatMap flatten
  where
    flatten (Decl _ ds) = ds
    flatten s = concat [concatMap flatten b | b <- suites s]
    declaration d@Actor{} = [make True d (params (pos d) (kwd d)) (name "self") (dbody d) False True]
    declaration d@Class{} = case [f | Decl _ ds <- dbody d, f@Def{} <- ds, nstr (dname f) == "__init__"] of
      [f] -> let ps = params (pos f) (kwd f)
                 self = case filter (not . isWitness . pname) ps of p:_ -> pname p; _ -> name "self"
             in [make False d (filter ((/=self) . pname) ps) self (dbody f) False (dloc f /= NoLoc)]
      _ -> [make False d [] (name "self") [] True False]
    declaration _ = []
    make a d ps self b missing source = Constructor (GName (modname m) (dname d)) a ps self b
        (S.fromList $ declared (dbody d) ++ declared b) r
        (S.fromList [fromMaybe mn alias | Import _ ms <- imps m, ModuleItem mn alias <- ms])
        (missing || any native b) source
    native (Expr _ NotImplemented{}) = True
    native (Assign _ _ NotImplemented{}) = True
    native _ = False

params ps ks = pp ps ++ kp ks
  where
    pp (PosPar n _ _ rest) = Parameter n True False : pp rest
    pp (PosSTAR n _) = [Parameter n True True]
    pp PosNIL = []
    kp (KwdPar n _ _ rest) = Parameter n False False : kp rest
    kp (KwdSTAR n _) = [Parameter n False True]
    kp KwdNIL = []

declared = concatMap names
  where
    names (Decl _ ds) = [dname d | d@Def{} <- ds]
    names s = concatMap declared (suites s)
suites s = case s of
    If _ bs es -> [b | Branch _ b <- bs] ++ [es]
    While _ _ b es -> [b,es]
    For _ _ _ b es -> [b,es]
    Try _ b hs es fin -> b : [x | Handler _ x <- hs] ++ [es,fin]
    With _ _ b -> [b]
    Data _ _ b -> [b]
    _ -> []

constraints :: M.Map QName Constructor -> Constructor -> [Rule]
constraints cs c = execWriter $ do
    forM_ (parameters c) $ \p -> require (S.singleton $ variable c $ pname p) self
    if unavailable c then reject (S.singleton self) else mapM_ stmt (body c)
  where
    self = Self (cname c)
    names = S.fromList (map pname (parameters c) ++ assigned (body c))
    refs n | n == receiver c || actor c && S.member n (methods c) = S.singleton self
           | S.member n names = S.singleton (variable c n)
           | otherwise = S.empty
    own e = case transparent e of Var _ (NoQ n) -> n == receiver c; _ -> False
    lookupCtor e = case transparent e of
      Var _ (NoQ n) | n == receiver c || S.member n (S.union names $ methods c) -> Nothing
      Var _ q -> M.lookup (canonical q) cs
      Dot _ base n | Just ns <- path base, S.member (modName ns) (qualifiers c),
                     S.notMember (name $ head ns) names -> M.lookup (canonical $ QName (modName ns) n) cs
      _ -> Nothing
    canonical q = case resolve c q of
      NoQ n -> GName (mname $ cname c) n
      QName m n -> GName m n
      q' -> q'
    path (Var _ (NoQ n)) = Just [nstr n]
    path (Dot _ e n) = (++[nstr n]) <$> path e
    path _ = Nothing
    inspect es = gather es >>= reject
    gather es = S.unions <$> mapM expr es
    bind vs (PVar _ n _) = require vs (variable c n)
    bind vs (PParen _ p) = bind vs p
    bind vs p = reject vs >> mapM_ (require vs . variable c) (bound p)
    stmt s = case s of
      Assign _ ps e -> expr e >>= \vs -> mapM_ (bind vs) ps
      VarAssign _ ps e -> expr e >>= \vs -> mapM_ (bind vs) ps
      MutAssign _ (Dot _ target n) e | own target -> expr e >>= \vs -> require vs (Field (cname c) n)
      MutAssign _ t e -> inspect [t,e]
      AugAssign _ t _ e -> inspect [t,e]
      Expr _ e -> void $ expr e
      If _ bs es -> mapM_ (\(Branch e b) -> inspect [e] >> mapM_ stmt b) bs >> mapM_ stmt es
      While _ e b es -> inspect [e] >> mapM_ stmt (b++es)
      For _ p e b es -> do vs <- expr e; reject vs; bind vs p; mapM_ stmt (b++es)
      With _ ws b -> do
        forM_ ws $ \(WithItem e p) -> do vs <- expr e; reject vs; mapM_ (bind vs) p
        mapM_ stmt b
      Try _ b hs es fin -> mapM_ stmt (b++concat [x | Handler _ x <- hs]++es++fin)
      Assert _ e me -> inspect (e:maybe [] (:[]) me)
      Return _ me -> inspect (maybe [] (:[]) me)
      Delete _ e -> inspect [e]
      After _ _ delay e -> inspect [delay,e]
      Data _ p b -> mapM_ (bind S.empty) p >> mapM_ stmt b
      _ -> return ()
    expr original = case transparent original of
      Var _ (NoQ n) -> return $ refs n
      Dot _ target n | own target -> return $ S.singleton $
          if S.member n (methods c) then self else Field (cname c) n
      Call _ f ps ks
        | Var _ q <- transparent f, q `elem` [primCAST,primWRAP] -> gather (arguments ps ks)
        | Just target <- lookupCtor f -> do
            let star = [p | p <- parameters target, positional p, variadic p]
                fixed = [p | p <- parameters target, not (variadic p), null star || positional p]
                kwstar = [p | p <- parameters target, not (positional p), variadic p]
                at i = if i < length fixed then Just (fixed !! i) else first star
                named n = first [p | p <- parameters target, pname p == n, not (variadic p)] `orElse` first kwstar
                forward (arg,mp) = do
                    vs <- expr arg
                    maybe (reject vs) (require vs . variable target . pname) mp
                    return vs
            vs <- mapM forward (zipWith (\i e -> (e,at i)) [0..] (posArgs ps) ++ [(e,named n) | (n,e) <- kwdArgs ks])
            inspect (expanded ps ks)
            return $ S.unions vs
      Lambda _ ps ks e _ -> do
        let ns = S.fromList (free e) `S.difference` S.fromList (bound (ps,ks))
            captured = S.unions [refs n | n <- S.toList ns]
            capturesSelf = actor c && not (S.null $ ns `S.intersection` S.union names (methods c))
        defaults <- gather (parameterDefaults ps ks)
        return $ S.union defaults (if capturesSelf then S.insert self captured else captured)
      Tuple _ ps ks -> inspect (expanded ps ks) >> gather (arguments ps ks)
      Cond _ x test y -> do
        inspect [test]
        vs <- mapM (\e -> do v <- expr e; if alias e then reject v else return (); return v) [x,y]
        return $ S.unions vs
      Let _ ss e -> mapM_ stmt ss >> expr e
      e -> do vs <- gather (children e); reject vs; return vs
    alias e = case transparent e of Var{} -> True; Dot _ target _ -> own target; _ -> False
    first (x:_) = Just x
    first [] = Nothing
    orElse Nothing y = y
    orElse x _ = x

transparent (TApp _ e _) = transparent e
transparent (Paren _ e) = transparent e
transparent (Box _ e) = transparent e
transparent (UnBox _ e) = transparent e
transparent e = e
parameterDefaults ps ks = pp ps ++ kp ks
  where
    pp (PosPar _ _ e rest) = maybe [] (:[]) e ++ pp rest
    pp _ = []
    kp (KwdPar _ _ e rest) = maybe [] (:[]) e ++ kp rest
    kp _ = []
posArgs (PosArg e rest) = e : posArgs rest
posArgs _ = []
kwdArgs (KwdArg n e rest) = (n,e) : kwdArgs rest
kwdArgs _ = []
expanded (PosArg _ rest) ks = expanded rest ks
expanded (PosStar e) ks = e : expanded PosNil ks
expanded PosNil (KwdArg _ _ rest) = expanded PosNil rest
expanded PosNil (KwdStar e) = [e]
expanded PosNil KwdNil = []
arguments ps ks = posArgs ps ++ map snd (kwdArgs ks) ++ expanded ps ks
children e = case e of
    Raise _ x _ -> [x]
    Call _ f ps ks -> f : arguments ps ks
    Async _ x -> [x]
    Await _ x -> [x]
    Index _ x i -> [x,i]
    Slice _ x (Sliz _ a b c) -> x : mapMaybe id [a,b,c]
    IsInstance _ x _ -> [x]
    BinOp _ x _ y -> [x,y]
    CompOp _ x ops -> x : [y | OpArg _ y <- ops]
    UnOp _ _ x -> [x]
    Dot _ x _ -> [x]
    Rest _ x _ -> [x]
    DotI _ x _ -> [x]
    RestI _ x _ -> [x]
    Opt _ x _ -> [x]
    OptChain _ x -> [x]
    Yield _ me -> maybe [] (:[]) me
    YieldFrom _ x -> [x]
    List _ es -> concatMap elemExprs es
    Set _ es -> concatMap elemExprs es
    Dict _ as -> concatMap assocExprs as
    ListComp _ el comp -> elemExprs el ++ compExprs comp
    SetComp _ el comp -> elemExprs el ++ compExprs comp
    DictComp _ a comp -> assocExprs a ++ compExprs comp
    GeneratorExpr _ el comp -> elemExprs el ++ compExprs comp
    _ -> []
  where
    elemExprs (Elem x) = [x]
    elemExprs (Star x) = [x]
    assocExprs (Assoc k v) = [k,v]
    assocExprs (StarStar x) = [x]
    compExprs NoComp = []
    compExprs (CompFor _ _ x rest) = x : compExprs rest
    compExprs (CompIf _ x rest) = x : compExprs rest
