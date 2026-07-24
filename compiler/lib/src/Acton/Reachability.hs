-- SPDX-License-Identifier: BSD-3-Clause

module Acton.Reachability
  ( MemberRef(..)
  , ReachEdge(..)
  , ReachSummary
  , ReachEnv(..)
  , ReachScope(..)
  , topReachEnv
  , localReachEnv
  , withReachOwner
  , advanceReachEnv
  , reachEdges
  , reachSummaryFromEdges
  , singletonReach
  , reflectReach
  , summarizeSuite
  , summarizeStmt
  , summarizeDecl
  , summarizeDeclHeader
  , summarizeExpr
  , summarizeCondition
  , summarizeType
  ) where

import qualified Acton.Builtin as Builtin
import qualified Acton.Env as Env
import qualified Acton.NameInfo as I
import qualified Acton.Names as Names
import qualified Acton.QuickType as QuickType
import qualified Acton.Prim as Prim
import qualified Acton.Subst as Subst
import qualified Acton.Syntax as A
import Acton.ReachabilityTypes

import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Set as Set


-- Lexical environment -----------------------------------------------------------------------------------

data ReachScope = TopScope | ContainerScope | LocalScope deriving (Eq, Show)

data ReachEnv = ReachEnv {
                    reachTypeEnv :: Env.Env0,
                    reachLocals  :: Set.Set A.Name,
                    reachGlobals :: Set.Set A.Name,
                    reachScope   :: ReachScope,
                    reachOwner   :: Maybe A.QName,
                    reachReflectiveOwner :: Bool,
                    reachImplicitAttrs :: Set.Set A.Name,
                    reachDeferredLocals :: Set.Set A.Name,
                    reachClassInitParams :: I.TEnv,
                    reachDirectMembers :: Map.Map A.Name (MemberRef,Bool)
                }

topReachEnv                       :: Env.Env0 -> Set.Set A.Name -> ReachEnv
topReachEnv env globals           = ReachEnv env Set.empty globals TopScope Nothing False Set.empty Set.empty [] Map.empty

localReachEnv                     :: Env.Env0 -> Set.Set A.Name -> ReachEnv
localReachEnv env globals         = ReachEnv env Set.empty globals LocalScope Nothing False Set.empty Set.empty [] Map.empty

withReachOwner                    :: A.QName -> ReachEnv -> ReachEnv
withReachOwner qn env             = env{ reachOwner = Just (A.GName m n) }
  where (m,n)                     = canonicalQName env qn

defineOnly                        :: I.TEnv -> ReachEnv -> ReachEnv
defineOnly te env                 = env{ reachTypeEnv = Env.define te (reachTypeEnv env) }

defineLocal                       :: I.TEnv -> ReachEnv -> ReachEnv
defineLocal te env                = (defineOnly te env){ reachLocals = Set.union (reachLocals env) (Set.fromList $ map fst te) }

defineBound                       :: (Names.Vars a, QuickType.EnvOf a) => a -> ReachEnv -> ReachEnv
defineBound syntax env            = (defineOnly (QuickType.envOf syntax) env) {
                                      reachLocals = Set.union (reachLocals env) (Set.fromList $ Names.bound syntax)
                                    }

advanceReachEnv                  :: I.TEnv -> ReachEnv -> ReachEnv
advanceReachEnv te env            = case reachScope env of
    TopScope                      -> defineOnly te env
    LocalScope                    -> defineLocal te env
    ContainerScope               -> (defineOnly te env) {
                                      reachLocals = Set.union (reachLocals env) deferred
                                    }
      where deferred              = Set.intersection (Set.fromList $ map fst te) (reachDeferredLocals env)

enterLocal                        :: ReachEnv -> ReachEnv
enterLocal env                    = env{ reachScope = LocalScope }

enterContainer                    :: ReachEnv -> ReachEnv
enterContainer env                = env{ reachScope = ContainerScope }

clearContainerBindings            :: ReachEnv -> ReachEnv
clearContainerBindings env        = env {
                                      reachImplicitAttrs = Set.empty,
                                      reachDeferredLocals = Set.empty,
                                      reachClassInitParams = [],
                                      reachDirectMembers = Map.empty
                                    }

classInitParams                   :: A.Suite -> I.TEnv
classInitParams body              =
    [ binding
    | A.Decl _ decls <- body
    , decl@A.Def{} <- decls
    , A.dname decl == Builtin.initKW
    , binding@(n,_) <- QuickType.envOf (A.pos decl)
    , Names.isWitness n
    ]

classEquationEnv                  :: ReachEnv -> [A.Pattern] -> ReachEnv
classEquationEnv env [A.PVar _ n (Just _)]
  | reachScope env == ContainerScope
  , Names.isWitness n             = defineLocal (reachClassInitParams env) env
classEquationEnv env _            = env

setDirectMembers                  :: A.Suite -> ReachEnv -> ReachEnv
setDirectMembers body env         = env{ reachDirectMembers = Map.fromList $ concatMap classify bodyEnv }
  where
    bodyEnv                       = QuickType.envOf body
    classify (n,I.NDef _ deco _)  = [(n,(MethodRef n,deco == A.Static))]
    classify (n,I.NSig sc deco _)
      | deco == A.Property        = [(n,(AttrRef n,deco == A.Static))]
      | A.TFun{} <- A.sctype sc   = [(n,(MethodRef n,deco == A.Static))]
    -- The Normalizer consumes class-scope witness equations before deciding
    -- whether they become globals, constructor locals, or instance fields.
    -- Keep that defining syntax row through its exact owner; explicit
    -- receiver.witness access remains ordinary instance dispatch.
    classify (n,I.NVar{})
      | Names.isWitness n         = [(n,(AttrRef n,True))]
    classify (n,I.NVar{})         = [(n,(AttrRef n,False))]
    classify (n,I.NSVar{})        = [(n,(AttrRef n,False))]
    classify _                    = []

defineTVars                       :: A.QBinds -> ReachEnv -> ReachEnv
defineTVars q env                 = env{ reachTypeEnv = Env.defineTVars q (reachTypeEnv env) }

-- Public walkers ----------------------------------------------------------------------------------------

summarizeSuite                    :: ReachEnv -> A.Suite -> ReachSummary
summarizeSuite env                = fst . walkSuite env

walkSuite                         :: ReachEnv -> A.Suite -> (ReachSummary, ReachEnv)
walkSuite env []                  = (mempty, env)
walkSuite env (s:ss)              = (one <> more, env'')
  where one                      = summarizeStmt env s
        env'                     = advanceReachEnv (QuickType.envOf s) env
        (more,env'')             = walkSuite env' ss

summarizeStmt                     :: ReachEnv -> A.Stmt -> ReachSummary
summarizeStmt env stmt            = case stmt of
    A.Expr _ e                    -> summarizeExpr env e
    A.Assign _ ps e               -> summarizeAssignmentPatterns env ps <>
                                      summarizeExpr (classEquationEnv env ps) e
    A.MutAssign _ t e             -> summarizeExpr env t <> summarizeExpr env e
    A.AugAssign _ t _ e           -> summarizeExpr env t <> summarizeExpr env e
    A.Assert _ e mbe              -> summarizeExpr env e <> booleanReach env e <>
                                      summarizeMaybe (summarizeExpr env) mbe
    A.Pass _                      -> mempty
    A.Delete _ t                  -> summarizeExpr env t
    A.Return _ mbe                -> summarizeMaybe (summarizeExpr env) mbe
    A.Raise _ e                   -> summarizeExpr env e
    A.Break _                     -> mempty
    A.Continue _                  -> mempty
    A.If _ bs els                 -> foldMap (summarizeBranch env) bs <> summarizeSuite env els
    A.While _ e b els             -> summarizeExpr env e <> booleanReach env e <>
                                      summarizeSuite env b <> summarizeSuite env els
    A.For _ p e b els             -> summarizeAssignmentPatterns env [p] <> summarizeExpr env e <> nextReach env e <>
                                      summarizeSuite (defineBound p env) b <>
                                      summarizeSuite env els
    A.Try _ b hs els fin          -> bodyReach <> foldMap (summarizeHandler env) hs <>
                                      summarizeSuite elseEnv els <> summarizeSuite env fin
      where bodyReach             = summarizeSuite env b
            elseEnv               = advanceReachEnv (QuickType.envOf b) env
    A.With _ items b              -> itemReach <> summarizeSuite bodyEnv b
      where (itemReach,bodyEnv)    = summarizeWithItems env items
    A.Data _ mbp b                -> summarizeMaybe (summarizePattern env) mbp <> summarizeSuite env b
    A.VarAssign _ ps e            -> summarizeAssignmentPatterns env ps <> summarizeExpr env e
    A.After _ e e'                -> summarizeExpr env e <> summarizeExpr env e'
    A.Signature _ _ sc _          -> summarizeTSchema env sc
    A.Decl _ ds                   -> foldMap (summarizeDecl declEnv) ds
      where declEnv               = advanceReachEnv (QuickType.envOf ds) env

summarizeDecl                     :: ReachEnv -> A.Decl -> ReachSummary
summarizeDecl env decl             = header <> summarizeSuite bodyEnv (A.declbody decl)
  where (header,bodyEnv)           = summarizeDeclHeader env decl

summarizeDeclHeader               :: ReachEnv -> A.Decl -> (ReachSummary,ReachEnv)
summarizeDeclHeader env decl       = case decl of
    A.Def _ n q p k a _ _ fx _    -> (reflect <> summarizeQBinds env q <> parReach <> kwdReach <>
                                      summarizeMaybe (summarizeType envQ) a <> summarizeType envQ fx,
                                      bodyEnv)
      where reflect
              | reachReflectiveOwner env,
                n == Builtin.getAttrKW,
                Just owner <- reachOwner env
                                    = reflectReach env owner
              | otherwise         = mempty
            envQ                  = defineTVars q ((enterLocal env){ reachReflectiveOwner = False })
            (parReach,envP)       = summarizePosPar envQ p
            (kwdReach,bodyEnv)    = summarizeKwdPar envP k
    A.Actor _ n q p k b _         -> (summarizeQBinds env q <> parReach <> kwdReach, bodyEnv)
      where (live,deferredNames)  = QuickType.actorBindings p k b
            attrs                 = Set.fromList live
            deferred              = Set.fromList deferredNames
            envQ                  = (setDirectMembers b $ withReachOwner (A.NoQ n) $ defineTVars q (enterContainer env)) {
                                      reachImplicitAttrs = attrs,
                                      reachDeferredLocals = deferred
                                    }
            selfType             = A.tCon $ A.TC (A.NoQ n) (map A.tVar $ A.qbound q)
            envSelf              = defineLocal [(Names.self, I.NVar selfType)] envQ
            (parReach,envP)       = summarizePosPar envSelf p
            (kwdReach,envK)       = summarizeKwdPar envP k
            bodyEnv               = envK{ reachLocals = Set.difference (reachLocals envK) attrs }
    A.Class _ n q cs b _          -> (summarizeQBinds env q <> foldMap (summarizeBaseTCon envQ) cs, bodyEnv)
      where envQ                  = defineTVars q (enterLocal env)
            bodyEnv               = (setDirectMembers b $ clearContainerBindings $
                                      withReachOwner (A.NoQ n) $
                                      defineTVars (Env.selfQuant (A.NoQ n) q) (enterContainer env)) {
                                        reachReflectiveOwner = True,
                                        reachClassInitParams = classInitParams b
                                      }
    A.Protocol _ n q ps b _       -> (summarizeQBinds env q <> foldMap (summarizeBaseTCon envQ) ps, bodyEnv)
      where envQ                  = defineTVars q (enterLocal env)
            bodyEnv               = setDirectMembers b $ clearContainerBindings $ withReachOwner (A.NoQ n) $
                                      defineTVars (Env.selfQuant (A.NoQ n) q) (enterContainer env)
    A.Typedef _ _ q t _           -> (summarizeQBinds env q <> summarizeType envQ t, envQ)
      where envQ                  = defineTVars q (enterLocal env)
    A.Extension _ q c ps b _      -> (summarizeQBinds env q <> summarizeBaseTCon envQ c <>
                                      foldMap (summarizeBaseTCon envQ) ps, bodyEnv)
      where envQ                  = defineTVars q (enterLocal env)
            bodyEnv               = setDirectMembers b $ clearContainerBindings $ withReachOwner (A.tcname c) $
                                      defineTVars (Env.selfQuant (A.tcname c) q) (enterContainer env)

summarizeExpr                     :: ReachEnv -> A.Expr -> ReachSummary
summarizeExpr env expr             = case expr of
    A.Var _ qn                     -> needValueQName env qn
    A.Int{}                        -> mempty
    A.Float{}                      -> mempty
    A.Imaginary{}                  -> mempty
    A.Bool{}                       -> mempty
    A.None{}                       -> mempty
    A.NotImplemented{}             -> mempty
    A.Ellipsis{}                   -> mempty
    A.Strings{}                    -> mempty
    A.BStrings{}                   -> mempty
    A.Call _ f ps ks               -> summarizeExpr env f <> summarizePosArg env ps <> summarizeKwdArg env ks <>
                                      maybe mempty (singletonReach . uncurry Construct) (constructorTarget env f)
    A.Let _ ss e                   -> summarizeSuite (enterLocal env) ss <> summarizeExpr env' e
      where env'                   = defineLocal (QuickType.envOf ss) (enterLocal env)
    A.TApp _ f ts                  -> summarizeExpr env f <> foldMap (summarizeType env) ts
    A.Async _ e                    -> summarizeExpr env e
    A.Await _ e                    -> summarizeExpr env e
    A.Index _ e ix                 -> summarizeExpr env e <> summarizeExpr env ix
    A.Slice _ e sl                 -> summarizeExpr env e <> summarizeSliz env sl
    A.Cond _ e c e'                -> summarizeExpr env e <> summarizeExpr env c <> booleanReach env c <>
                                      summarizeExpr env e'
    A.IsInstance _ e qn            -> summarizeExpr env e <> needTypeQName env qn
    A.BinOp _ e _ e'               -> summarizeExpr env e <> summarizeExpr env e'
    A.CompOp _ e ops               -> summarizeExpr env e <> foldMap (summarizeOpArg env) ops
    A.UnOp _ A.Not e               -> summarizeExpr env e <> booleanReach env e
    A.UnOp _ _ e                   -> summarizeExpr env e
    A.Dot _ e n                    -> summarizeReceiver env e <> memberSelection env e n
    A.Rest _ e _                   -> summarizeExpr env e
    A.DotI _ e _                   -> summarizeExpr env e
    A.RestI _ e _                  -> summarizeExpr env e
    A.Opt _ e _                    -> summarizeExpr env e
    A.OptChain _ e                 -> summarizeExpr env e
    A.Lambda _ p k e fx            -> parReach <> kwdReach <> summarizeExpr bodyEnv e <> summarizeType env fx
      where (parReach,envP)         = summarizePosPar (enterLocal env) p
            (kwdReach,bodyEnv)     = summarizeKwdPar envP k
    A.Yield _ mbe                  -> summarizeMaybe (summarizeExpr env) mbe
    A.YieldFrom _ e                -> summarizeExpr env e
    A.Tuple _ ps ks                -> summarizePosArg env ps <> summarizeKwdArg env ks
    A.List _ es                    -> foldMap (summarizeElem env) es
    A.ListComp _ e c               -> compReach <> summarizeElem compEnv e
      where (compReach,compEnv)     = summarizeComp (enterLocal env) c
    A.Dict _ as                    -> foldMap (summarizeAssoc env) as
    A.DictComp _ a c               -> compReach <> summarizeAssoc compEnv a
      where (compReach,compEnv)     = summarizeComp (enterLocal env) c
    A.Set _ es                     -> foldMap (summarizeElem env) es
    A.SetComp _ e c                -> compReach <> summarizeElem compEnv e
      where (compReach,compEnv)     = summarizeComp (enterLocal env) c
    A.Paren _ e                    -> summarizeExpr env e
    A.Box t e                      -> summarizeType env t <> summarizeExpr env e
    A.UnBox t e                    -> summarizeType env t <> summarizeExpr env e

summarizeCondition                :: ReachEnv -> A.Expr -> ReachSummary
summarizeCondition env expr        = summarizeExpr env expr <> booleanReach env expr


-- Selection classification ------------------------------------------------------------------------------

constructorTarget                  :: ReachEnv -> A.Expr -> Maybe (A.ModName,A.Name)
constructorTarget env (A.TApp _ f _) = constructorTarget env f
constructorTarget env (A.Paren _ f) = constructorTarget env f
constructorTarget env (A.Var _ qn) = case Env.findQName qn (reachTypeEnv env) of
    I.NClass{}                     -> target
    I.NAct{}                       -> target
    I.NExt{}                       -> target
    _                              -> Nothing
  where
    target                         = case canonicalQName env qn of
      key@(mn,_)
        | mn /= Prim.mPrim         -> Just key
      _                             -> Nothing
constructorTarget _ _              = Nothing

-- A class/actor value can be aliased or passed through a higher-order call and
-- invoked later. Treat the value escape as construction interest; direct
-- class-qualified member access uses summarizeReceiver below and remains a
-- static selection without construction.
needValueQName                    :: ReachEnv -> A.QName -> ReachSummary
needValueQName env qn              = needQName env qn <> valueEscape
  where
    target                         = canonicalQName env qn
    valueEscape
      | localOrMember              = mempty
      | fst target == Prim.mPrim   = mempty
      | target == (Builtin.mBuiltin,Builtin.nSerialize) = dynamic
      | target == (Builtin.mBuiltin,Builtin.nDeserialize) = dynamic
      | otherwise                  = case Env.tryQName qn (reachTypeEnv env) of
          Just I.NClass{}          -> construct
          Just I.NAct{}            -> construct
          Just I.NExt{}            -> construct
          _                        -> mempty
    localOrMember                  = case qn of
      A.NoQ n -> n == Names.selfKW' ||
                 Set.member n (reachLocals env) ||
                 isJust (ownerMember env n)
      _       -> False
    construct                      = singletonReach (uncurry Construct target)
    dynamic                        = singletonReach DynamicSerialization

summarizeReceiver                 :: ReachEnv -> A.Expr -> ReachSummary
summarizeReceiver env expr
  | Just _ <- directClassTarget env expr
                                    = staticReceiver expr
  | otherwise                       = summarizeExpr env expr
  where
    staticReceiver (A.Var _ qn)     = needQName env qn
    staticReceiver (A.TApp _ e ts)  = staticReceiver e <> foldMap (summarizeType env) ts
    staticReceiver (A.Paren _ e)    = staticReceiver e
    staticReceiver e                = summarizeExpr env e

memberSelection                    :: ReachEnv -> A.Expr -> A.Name -> ReachSummary
memberSelection env e n
  | n == Builtin.initKW,
    Just _ <- directProtocolTarget env e
                                    = mempty
  | Just owner <- directClassTarget env e
                                    = select Direct owner
  | Just owner <- constructedReceiverTarget env e
                                    = select Dispatch owner
  | otherwise                       = case typ of
      A.TCon _ tc                  -> select Dispatch (canonicalQName env $ A.tcname tc)
      A.TVar _ tv                  -> select Dispatch (canonicalQName env $ A.tcname $ Env.findTVBound (reachTypeEnv env) tv)
      A.TTuple _ _ k
        | n `elem` Builtin.valueKWs -> select Dispatch (canonicalQName env $ A.tcname Builtin.cValue)
        | tupleField n k           -> mempty
        | otherwise                -> reachError env ("tuple has no field " ++ A.rawstr n) e
      t                            -> reachError env ("impossible receiver type " ++ show t ++ " for ." ++ A.rawstr n) e
  where typ                       = expandReachType (reachTypeEnv env) $
                                    QuickType.typeOf (reachTypeEnv env) e
        select edge owner          = singletonReach (uncurry edge owner $ memberRef env owner n)

directClassTarget                 :: ReachEnv -> A.Expr -> Maybe (A.ModName,A.Name)
directClassTarget env (A.TApp _ e _) = directClassTarget env e
directClassTarget env (A.Paren _ e) = directClassTarget env e
directClassTarget env (A.Var _ qn) = case Env.findQName qn (reachTypeEnv env) of
    I.NClass{}                     -> Just (canonicalQName env qn)
    I.NProto{}                     -> Just (canonicalQName env qn)
    I.NExt{}                       -> Just (canonicalQName env qn)
    _                              -> Nothing
directClassTarget _ _              = Nothing

directProtocolTarget              :: ReachEnv -> A.Expr -> Maybe (A.ModName,A.Name)
directProtocolTarget env (A.TApp _ e _) = directProtocolTarget env e
directProtocolTarget env (A.Paren _ e) = directProtocolTarget env e
directProtocolTarget env (A.Var _ qn) = case Env.findQName qn (reachTypeEnv env) of
    I.NProto{}                     -> Just (canonicalQName env qn)
    _                              -> Nothing
directProtocolTarget _ _          = Nothing

constructedReceiverTarget        :: ReachEnv -> A.Expr -> Maybe (A.ModName,A.Name)
constructedReceiverTarget env (A.Call _ fun _ _) = constructorTarget env fun
constructedReceiverTarget env (A.Paren _ expr) = constructedReceiverTarget env expr
constructedReceiverTarget _ _     = Nothing

memberRef                         :: ReachEnv -> (A.ModName,A.Name) -> A.Name -> MemberRef
memberRef _ _ n | n == Builtin.initKW = MethodRef n
memberRef _ _ n@(A.Internal A.Witness _ _) = AttrRef n
memberRef env target@(m,c) n       = case Env.findAttrInfo' (reachTypeEnv env) (A.GName m c) n of
    Just info                      -> memberRefFromInfo env target n info
    Nothing                        -> reachError0 env ("missing member " ++ targetText m c ++ "." ++ A.rawstr n)

memberRefFromInfo                 :: ReachEnv -> (A.ModName,A.Name) -> A.Name -> I.NameInfo -> MemberRef
memberRefFromInfo env (m,c) n info = case info of
    I.NDef{}                       -> MethodRef n
    I.NSig sc dec _
      | dec == A.Property         -> AttrRef n
      | A.TFun{} <- A.sctype sc   -> MethodRef n
      | otherwise                 -> reachError0 env ("non-property, non-method signature for " ++ targetText m c ++ "." ++ A.rawstr n)
    I.NVar{}                       -> AttrRef n
    I.NSVar{}                      -> AttrRef n
    _                              -> reachError0 env ("non-member info " ++ show info ++ " for " ++ targetText m c ++ "." ++ A.rawstr n)

reflectReach                      :: ReachEnv -> A.QName -> ReachSummary
reflectReach env qn                = singletonReach (uncurry Reflect $ canonicalQName env qn)

needQName                         :: ReachEnv -> A.QName -> ReachSummary
needQName _ (A.NoQ n)
  | n == Names.selfKW'            = mempty
needQName env (A.NoQ n)
  | Set.member n (reachLocals env) = mempty
  | Just edge <- ownerMember env n
                                    = singletonReach edge
  | Set.member n (reachGlobals env) = singletonReach (uncurry Need $ canonicalQName env $ A.NoQ n)
  | Just I.NAlias{} <- rawInfo      = singletonReach (uncurry Need $ canonicalQName env $ A.NoQ n)
  | I.NVar{} <- info               = reachError0 env ("untracked local variable " ++ A.rawstr n)
  | I.NSVar{} <- info              = reachError0 env ("untracked state variable " ++ A.rawstr n)
  | otherwise                     = reachError0 env ("untracked unqualified name " ++ A.rawstr n ++ " with " ++ show info)
  where rawInfo                    = Env.lookupName n (reachTypeEnv env)
        info                       = Env.findQName (A.NoQ n) (reachTypeEnv env)
needQName env qn                   = singletonReach (uncurry Need $ canonicalQName env qn)

needTypeQName                     :: ReachEnv -> A.QName -> ReachSummary
needTypeQName env qn               = singletonReach (uncurry Need $ canonicalQName env qn)

inheritTypeQName                  :: ReachEnv -> A.QName -> ReachSummary
inheritTypeQName env qn            = singletonReach (uncurry Inherit $ canonicalQName env qn)

declareTypeQName                  :: ReachEnv -> A.QName -> ReachSummary
declareTypeQName env qn            = singletonReach (uncurry Declare $ canonicalQName env qn)

ownerMember                       :: ReachEnv -> A.Name -> Maybe ReachEdge
ownerMember env n                  = case reachOwner env of
    Just owner@(A.GName m c)
      | Set.member n (reachImplicitAttrs env)
                                    -> Just (Dispatch m c $ AttrRef n)
      | Just (ref,isStatic) <- Map.lookup n (reachDirectMembers env)
                                    -> Just ((if isStatic then Direct else Dispatch) m c ref)
      | Just memberInfo <- info,
        isMemberInfo memberInfo    -> Just (memberEdge memberInfo m c $ memberRefFromInfo env (m,c) n memberInfo)
      | otherwise                 -> Nothing
      where info                   = Env.findAttrInfo' (reachTypeEnv env) owner n
            isMemberInfo I.NDef{}  = True
            isMemberInfo I.NSig{}  = True
            isMemberInfo I.NVar{}  = True
            isMemberInfo I.NSVar{} = True
            isMemberInfo _         = False
            memberEdge (I.NDef _ A.Static _) = Direct
            memberEdge (I.NSig _ A.Static _) = Direct
            memberEdge _                       = Dispatch
    Just qn                        -> reachError0 env ("non-global owner " ++ show qn)
    Nothing                        -> Nothing

canonicalQName                    :: ReachEnv -> A.QName -> (A.ModName,A.Name)
canonicalQName env qn              = case Env.unalias (reachTypeEnv env) qn of
    A.GName m n                    -> (m,n)
    qn'                            -> reachError0 env ("non-global target " ++ show qn' ++ " from " ++ show qn)

tupleField                        :: A.Name -> A.Type -> Bool
tupleField n (A.TRow _ _ n' _ r)   = n == n' || tupleField n r
tupleField _ A.TStar{}             = True
tupleField _ A.TNil{}              = False
tupleField n r                     = error ("Acton.Reachability: impossible tuple row " ++ show r ++ " while finding " ++ A.rawstr n)

targetText                        :: A.ModName -> A.Name -> String
targetText (A.ModName ns) n        = concatMap ((++ ".") . A.rawstr) ns ++ A.rawstr n

reachError                        :: ReachEnv -> String -> A.Expr -> a
reachError env msg e               = reachError0 env (msg ++ " in " ++ show e)

reachError0                       :: ReachEnv -> String -> a
reachError0 env msg                = error ("Acton.Reachability: " ++ owner ++ msg)
  where owner                     = maybe "" (\qn -> "while walking " ++ show qn ++ ": ") (reachOwner env)


-- Remaining syntax --------------------------------------------------------------------------------------

summarizeBranch                   :: ReachEnv -> A.Branch -> ReachSummary
summarizeBranch env (A.Branch e b) = summarizeExpr env e <> summarizeSuite env b
                                      <> booleanReach env e

summarizeHandler                  :: ReachEnv -> A.Handler -> ReachSummary
summarizeHandler env (A.Handler ex b)
                                    = summarizeExcept env ex <> summarizeSuite bodyEnv b
  where bodyEnv                   = defineBound ex env

summarizeExcept                   :: ReachEnv -> A.Except -> ReachSummary
summarizeExcept _ A.ExceptAll{}    = mempty
summarizeExcept env (A.Except _ qn) = needTypeQName env qn
summarizeExcept env (A.ExceptAs _ qn _) = needTypeQName env qn

summarizeWithItems                :: ReachEnv -> [A.WithItem] -> (ReachSummary,ReachEnv)
summarizeWithItems env []          = (mempty,env)
summarizeWithItems env (item:items)= (one <> more,env'')
  where one                       = summarizeWithItem env item
        env'                      = defineBound item env
        (more,env'')              = summarizeWithItems env' items

summarizeWithItem                 :: ReachEnv -> A.WithItem -> ReachSummary
summarizeWithItem env (A.WithItem e p)
                                    = summarizeExpr env e <> summarizeMaybe (summarizePattern env) p

summarizePatterns                 :: ReachEnv -> [A.Pattern] -> ReachSummary
summarizePatterns env              = foldMap (summarizePattern env)

summarizeAssignmentPatterns       :: ReachEnv -> [A.Pattern] -> ReachSummary
summarizeAssignmentPatterns env ps = summarizePatterns env ps <> foldMap target ps
  where
    target pat = case pat of
      A.PVar _ name _
        | Set.member name (reachImplicitAttrs env)
        , Just (A.GName mn owner) <- reachOwner env
                                    -> singletonReach (Dispatch mn owner $ AttrRef name)
      A.PParen _ p                 -> target p
      A.PTuple _ pos kwd           -> posTargets pos <> kwdTargets kwd
      A.PList _ items rest         -> foldMap target items <> foldMap target rest
      _                            -> mempty

    posTargets pos = case pos of
      A.PosPat p rest              -> target p <> posTargets rest
      A.PosPatStar p               -> target p
      A.PosPatNil                  -> mempty

    kwdTargets kwd = case kwd of
      A.KwdPat _ p rest            -> target p <> kwdTargets rest
      A.KwdPatStar p               -> target p
      A.KwdPatNil                  -> mempty

summarizePattern                  :: ReachEnv -> A.Pattern -> ReachSummary
summarizePattern env pat           = case pat of
    A.PWild _ mt                   -> summarizeMaybe (summarizeType env) mt
    A.PVar _ _ mt                 -> summarizeMaybe (summarizeType env) mt
    A.PParen _ p                  -> summarizePattern env p
    A.PTuple _ ps ks              -> summarizePosPat env ps <> summarizeKwdPat env ks
    A.PList _ ps p                -> summarizePatterns env ps <> summarizeMaybe (summarizePattern env) p
    A.PData _ n ixs               -> needTypeQName env (A.NoQ n) <> foldMap (summarizeExpr env) ixs

summarizePosPar                   :: ReachEnv -> A.PosPar -> (ReachSummary,ReachEnv)
summarizePosPar env p              = case p of
    A.PosPar n mt me rest         -> (one <> more,env'')
      where one                   = summarizeMaybe (summarizeType env) mt <> summarizeMaybe (summarizeExpr env) me
            env'                  = defineLocal [(n,I.NVar $ typedParam env n mt)] env
            (more,env'')          = summarizePosPar env' rest
    A.PosSTAR n mt                -> (summarizeMaybe (summarizeType env) mt,
                                      defineLocal [(n,I.NVar $ typedParam env n mt)] env)
    A.PosNIL                      -> (mempty,env)

summarizeKwdPar                   :: ReachEnv -> A.KwdPar -> (ReachSummary,ReachEnv)
summarizeKwdPar env k              = case k of
    A.KwdPar n mt me rest         -> (one <> more,env'')
      where one                   = summarizeMaybe (summarizeType env) mt <> summarizeMaybe (summarizeExpr env) me
            env'                  = defineLocal [(n,I.NVar $ typedParam env n mt)] env
            (more,env'')          = summarizeKwdPar env' rest
    A.KwdSTAR n mt                -> (summarizeMaybe (summarizeType env) mt,
                                      defineLocal [(n,I.NVar $ typedParam env n mt)] env)
    A.KwdNIL                      -> (mempty,env)

typedParam                        :: ReachEnv -> A.Name -> Maybe A.Type -> A.Type
typedParam _ _ (Just t)            = t
typedParam env n Nothing           = reachError0 env ("untyped reconstructed parameter " ++ A.rawstr n)

summarizePosArg                   :: ReachEnv -> A.PosArg -> ReachSummary
summarizePosArg env ps             = case ps of
    A.PosArg e rest               -> summarizeExpr env e <> summarizePosArg env rest
    A.PosStar e                   -> summarizeExpr env e
    A.PosNil                      -> mempty

summarizeKwdArg                   :: ReachEnv -> A.KwdArg -> ReachSummary
summarizeKwdArg env ks             = case ks of
    A.KwdArg _ e rest             -> summarizeExpr env e <> summarizeKwdArg env rest
    A.KwdStar e                   -> summarizeExpr env e
    A.KwdNil                      -> mempty

summarizePosPat                   :: ReachEnv -> A.PosPat -> ReachSummary
summarizePosPat env ps             = case ps of
    A.PosPat p rest               -> summarizePattern env p <> summarizePosPat env rest
    A.PosPatStar p                -> summarizePattern env p
    A.PosPatNil                   -> mempty

summarizeKwdPat                   :: ReachEnv -> A.KwdPat -> ReachSummary
summarizeKwdPat env ks             = case ks of
    A.KwdPat _ p rest             -> summarizePattern env p <> summarizeKwdPat env rest
    A.KwdPatStar p                -> summarizePattern env p
    A.KwdPatNil                   -> mempty

summarizeElem                     :: ReachEnv -> A.Elem -> ReachSummary
summarizeElem env (A.Elem e)       = summarizeExpr env e
summarizeElem env (A.Star e)       = summarizeExpr env e

summarizeAssoc                    :: ReachEnv -> A.Assoc -> ReachSummary
summarizeAssoc env (A.Assoc k v)   = summarizeExpr env k <> summarizeExpr env v
summarizeAssoc env (A.StarStar e)  = summarizeExpr env e

summarizeOpArg                    :: ReachEnv -> A.OpArg -> ReachSummary
summarizeOpArg env (A.OpArg _ e)   = summarizeExpr env e

summarizeSliz                     :: ReachEnv -> A.Sliz -> ReachSummary
summarizeSliz env (A.Sliz _ a b c) = foldMap (summarizeExpr env) a <> foldMap (summarizeExpr env) b <> foldMap (summarizeExpr env) c

summarizeComp                     :: ReachEnv -> A.Comp -> (ReachSummary,ReachEnv)
summarizeComp env comp             = case comp of
    A.CompFor _ p e c             -> (summarizeExpr env e <> nextReach env e <>
                                      summarizePattern env p <> more,env'')
      where env'                  = defineBound p env
            (more,env'')          = summarizeComp env' c
    A.CompIf _ e c                -> (summarizeExpr env e <> booleanReach env e <> more,env')
      where (more,env')           = summarizeComp env c
    A.NoComp                      -> (mempty,env)

summarizeTSchema                  :: ReachEnv -> A.TSchema -> ReachSummary
summarizeTSchema env (A.TSchema _ q t)
                                    = summarizeQBinds env q <> summarizeType (defineTVars q env) t

summarizeQBinds                   :: ReachEnv -> A.QBinds -> ReachSummary
summarizeQBinds env                = foldMap summarize
  where summarize (A.QBind _ cs)  = foldMap (summarizeTCon env) cs

summarizeTCon                     :: ReachEnv -> A.TCon -> ReachSummary
summarizeTCon env (A.TC qn ts)     = typeReach <> foldMap (summarizeType env) ts
  where typeReach
          | typeAlias (reachTypeEnv env) qn
                                    = needTypeQName env qn
          | otherwise               = declareTypeQName env qn

typeAlias                         :: Env.Env0 -> A.QName -> Bool
typeAlias env                     = isJust . typeAliasInfo env

typeAliasInfo                     :: Env.Env0
                                  -> A.QName
                                  -> Maybe (A.QBinds,A.Type)
typeAliasInfo env qn               = case qn of
    A.NoQ name                    -> fromInfo (Env.lookupName name env)
    A.GName mn name
      | Just mn == Env.thismod env -> fromInfo (Env.lookupName name env)
      | otherwise                 -> fromModule mn name
    A.QName mn name               -> fromModule mn name
  where
    fromModule mn name             = case Env.lookupModuleInfo mn env of
      Just info                     -> fromInfo (Env.moduleLookupName info name)
      Nothing                       -> Nothing
    fromInfo (Just (I.NType q typ _))
                                    = Just (q,typ)
    fromInfo (Just (I.NAlias qn')) = typeAliasInfo env qn'
    fromInfo _                     = Nothing

expandReachType                   :: Env.Env0 -> A.Type -> A.Type
expandReachType env typ@(A.TCon _ (A.TC qn args))
                                    = case typeAliasInfo env qn of
    Just (q,target)                -> expandReachType env $
                                      Subst.vsubst (A.qbound q `zip` args) target
    Nothing                        -> typ
expandReachType _ typ              = typ

summarizeBaseTCon                 :: ReachEnv -> A.TCon -> ReachSummary
summarizeBaseTCon env (A.TC qn ts) = inheritTypeQName env qn <> foldMap (summarizeType env) ts

summarizeType                     :: ReachEnv -> A.Type -> ReachSummary
summarizeType env t                = case t of
    A.TUni{}                      -> mempty
    A.TVar{}                      -> mempty
    A.TCon _ tc                   -> summarizeTCon env tc
    A.TFun _ fx p k r             -> foldMap (summarizeType env) [fx,p,k,r]
    A.TTuple _ p k                -> summarizeType env p <> summarizeType env k
    A.TOpt _ t'                   -> summarizeType env t'
    A.TNone{}                     -> mempty
    A.TWild{}                     -> mempty
    A.TNil{}                      -> mempty
    A.TRow _ _ _ t' r            -> summarizeType env t' <> summarizeType env r
    A.TStar _ _ r                 -> summarizeType env r
    A.TFX{}                       -> mempty
    A.TUnboxed _ t'               -> summarizeType env t'

summarizeMaybe                    :: (a -> ReachSummary) -> Maybe a -> ReachSummary
summarizeMaybe _ Nothing           = mempty
summarizeMaybe f (Just x)          = f x


-- Post-front calls --------------------------------------------------------------------------------------

-- Normalizer makes truth conversion explicit.  Record precisely the method
-- it will introduce, while leaving an already-bool condition alone.
booleanReach                     :: ReachEnv -> A.Expr -> ReachSummary
booleanReach env expr
  | typ == Builtin.tBool          = mempty
  | A.TOpt _ inner <- typ
                                    = memberReachForType env inner Builtin.boolKW
  | A.BinOp _ left op right <- expr
  , op `elem` [A.And,A.Or]         = booleanReach env left <> booleanReach env right
  | otherwise                     = memberSelection env expr Builtin.boolKW
  where typ                       = expandReachType (reachTypeEnv env) $
                                    QuickType.typeOf (reachTypeEnv env) expr

-- Types has already inserted __iter__.  Normalizer stores that iterator and
-- calls __next__ for each iteration; its specialized range loop calls the
-- opaque primitive instead.
nextReach                        :: ReachEnv -> A.Expr -> ReachSummary
nextReach env expr
  | isRangeIterator env expr       = needQName env Prim.primUNext
  | otherwise                     = memberSelection env expr Builtin.nextKW

isRangeIterator                  :: ReachEnv -> A.Expr -> Bool
isRangeIterator env (A.Call _ fun (A.PosArg arg A.PosNil) A.KwdNil)
  | iteratorCall fun              = QuickType.typeOf (reachTypeEnv env) arg == Builtin.tRange
isRangeIterator env (A.Paren _ expr) = isRangeIterator env expr
isRangeIterator _ _               = False

iteratorCall                     :: A.Expr -> Bool
iteratorCall (A.Dot _ _ name)      = name == Builtin.iterKW
iteratorCall (A.TApp _ fun _)      = iteratorCall fun
iteratorCall _                     = False

memberReachForType               :: ReachEnv -> A.Type -> A.Name -> ReachSummary
memberReachForType env typ name    = case expandReachType (reachTypeEnv env) typ of
    A.TCon _ con                  -> dispatch (A.tcname con)
    A.TVar _ var                  -> dispatch (A.tcname $ Env.findTVBound (reachTypeEnv env) var)
    A.TTuple{}                    -> dispatch (A.tcname Builtin.cValue)
    _                             -> reachError0 env
                                      ("impossible generated receiver type " ++ show typ ++
                                       " for ." ++ A.rawstr name)
  where
    dispatch qn                   = singletonReach (uncurry Dispatch owner $ memberRef env owner name)
      where owner                 = canonicalQName env qn
