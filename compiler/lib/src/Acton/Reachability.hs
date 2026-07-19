-- SPDX-License-Identifier: BSD-3-Clause

-- | Extract and close reachability dependencies for selective compilation.
--
-- The front pass traverses typed syntax and records exact dependencies on
-- top-level names and qualified members.  'prepareReachabilityRows' assigns
-- those summaries to the independently loadable rows written to TYDB.  For a
-- deferred back pass, 'selectProgram' follows those persisted facts from the
-- executable roots and returns the complete selection of syntax rows.
--
-- 'ReachabilityRows' contains the persisted representation.  'SelectiveBack'
-- supplies exact TYDB reads and turns the resulting selection into partial
-- Acton modules.  This module performs no interface-file IO and does not run
-- compiler passes.
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
  , prepareReachabilityRows
  , TopKey(..)
  , TopInfo(..)
  , ShapeKind(..)
  , ConstructorDecl(..)
  , SlotDecl(..)
  , MemberInfo(..)
  , ShapeInfo(..)
  , SlotInfo(..)
  , ReflectableAttrs(..)
  , ReachLookup(..)
  , Selection(..)
  , emptySelection
  , SelectionError(..)
  , selectProgram
  ) where

import qualified Acton.Builtin as Builtin
import qualified Acton.Env as Env
import qualified Acton.InterfaceRows as Rows
import qualified Acton.NameInfo as I
import qualified Acton.Names as Names
import qualified Acton.QuickType as QuickType
import qualified Acton.Prim as Prim
import Acton.ReachabilityRows
import qualified Acton.Subst as Subst
import qualified Acton.Syntax as A
import qualified Acton.WitnessForwarding as Forward

import Control.DeepSeq (force)
import Control.Monad (foldM, unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, except, runExceptT, throwE)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, modify')
import qualified Data.HashMap.Strict as HashMap
import Data.List (foldl', partition)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Sequence as Seq
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


-- Preparation -------------------------------------------------------------------------------------------

prepareReachabilityRows :: Env.Env0 -> I.TEnv -> A.Module -> Rows.InterfaceRows -> Rows.RowResult ReachabilityRows
prepareReachabilityRows typeEnv sourceInterface (A.Module mn _ _ suite) stored = do
    unless (Rows.rowModuleName stored == mn) $
      Rows.rowError "reachability/interface module mismatch"
    unless (length (Rows.rowStatements stored) == length suite) $
      Rows.rowError "reachability/interface top-level statement mismatch"
    let suiteEnv = QuickType.envOfTopSuite suite
        moduleEnv = Env.define suiteEnv (Env.setMod mn typeEnv)
        globals = Set.fromList (topNames suite)
        env0 = topReachEnv moduleEnv globals
        extensions = Map.fromListWith (flip (++))
          [ (extensionTarget ext,[ext])
          | ext <- extensionInfos moduleEnv mn suite
          ]
        forwarding = Forward.buildForwardContext moduleEnv
          [ n | (n,I.NClass{}) <- suiteEnv ]
        sourceInfo = Env.hnamesFrom sourceInterface
    (prepared,_) <- foldM (prepareTop moduleEnv sourceInfo forwarding stored extensions)
      (emptyReachabilityRows, env0) (zip suite $ Rows.rowStatements stored)
    let whole = force (wholeSummary env0 prepared)
    whole `seq` return prepared{ reachWholeSummary = whole }


data ContainerPrepared = ContainerPrepared
  { preparedKind        :: ShapeKind
  , preparedBackendKind :: ShapeKind
  , preparedMembers     :: Map.Map Rows.MemberKey Rows.MemberContent
  , preparedAnalysis    :: ContainerAnalysis
  }

prepareTop :: Env.Env0
           -> I.HTEnv
           -> Forward.ForwardContext
           -> Rows.InterfaceRows
           -> Map.Map TopKey [ExtensionInfo]
           -> (ReachabilityRows, ReachEnv)
           -> (A.Stmt, Rows.StoredStmt)
           -> Rows.RowResult (ReachabilityRows, ReachEnv)
prepareTop moduleEnv sourceInfo forwarding stored extensions (rows,env) (stmt,storedStmt) = do
    let env' = advanceReachEnv (QuickType.envOf stmt) env
    case (stmt,storedStmt) of
      (A.Decl _ decls, Rows.StoredDecls _ storedDecls) -> do
        unless (length decls == length storedDecls) $
          Rows.rowError "reachability/interface declaration count mismatch"
        let declEnv = advanceReachEnv (QuickType.envOf decls) env
        rows' <- foldM
          (prepareTopDecl moduleEnv sourceInfo forwarding stored extensions declEnv)
          rows (zip decls storedDecls)
        return (rows',env')
      (_,Rows.StoredWhole owners _) -> do
        let summary = summarizeStmt env stmt
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

prepareTopDecl :: Env.Env0
               -> I.HTEnv
               -> Forward.ForwardContext
               -> Rows.InterfaceRows
               -> Map.Map TopKey [ExtensionInfo]
               -> ReachEnv
               -> ReachabilityRows
               -> (A.Decl, Rows.StoredDecl)
               -> Rows.RowResult ReachabilityRows
prepareTopDecl moduleEnv sourceInfo forwarding stored extensions env rows (decl,storedDecl) = do
    let mn = Rows.rowModuleName stored
        name = Names.dname' decl
        key = TopKey mn name
        typeEnv = reachTypeEnv env
    case storedDecl of
      Rows.StoredInline _ -> do
        rows' <- insertTopSummary Nothing (summarizeDecl env decl) rows key
        return rows'
      Rows.StoredContainer _ -> do
        shape <- required ("missing container shape " ++ A.rawstr name) $
          Map.lookup name (Rows.rowShapes stored)
        members <- required ("missing container members " ++ A.rawstr name) $
          Map.lookup name (Rows.rowMembers stored)
        let kind = containerKind sourceInfo decl
            (header,bodyEnv) = summarizeDeclHeader env decl
            analysis = analyzeContainer shape bodyEnv (A.dbody decl)
        residual <- residualSummary analysis (Rows.shapeSuite shape)
        rows' <- insertTopSummary
          (compactDeclaration $ Env.unalias typeEnv $ Env.findQName (A.NoQ name) typeEnv)
          (header <> residual) rows key
        prepareContainerMetadata moduleEnv forwarding stored extensions rows'
          (key,ContainerPrepared kind (backendContainerKind decl) members analysis)


-- Lexically placed summaries ---------------------------------------------------------------------------

data PlacedDeclSummary = PlacedDeclSummary
  { placedHeaderSummary :: ReachSummary
  , placedFullSummary :: ReachSummary
  }

data PlacedInitSummary = PlacedInitSummary
  { initEnvironment :: ReachEnv
  , initGuard :: ReachSummary
  , initSummary :: ReachSummary
  }

data ContainerAnalysis = ContainerAnalysis
  { analyzedStatements        :: Map.Map Rows.StmtPlace ReachSummary
  , analyzedDecls             :: Map.Map Rows.DeclPlace PlacedDeclSummary
  , analyzedConstructorHeader :: Maybe ReachSummary
  , analyzedConstructorSelf   :: Maybe A.Name
  , analyzedInitBody          :: Map.Map Int PlacedInitSummary
  }

emptyAnalysis :: ContainerAnalysis
emptyAnalysis = ContainerAnalysis Map.empty Map.empty Nothing Nothing Map.empty

analyzeContainer :: Rows.ContainerShape -> ReachEnv -> A.Suite -> ContainerAnalysis
analyzeContainer shape env suite =
    analyzeSuite constructor Rows.rootPath mempty env suite emptyAnalysis
  where constructor = constructorPlace (Rows.shapeSuite shape)

analyzeSuite :: Maybe Rows.DeclPlace
             -> Rows.SuitePath
             -> ReachSummary
             -> ReachEnv
             -> A.Suite
             -> ContainerAnalysis
             -> ContainerAnalysis
analyzeSuite constructor path guard env suite analysis0 =
    snd $ foldl' analyze (env,analysis0) (zip [0..] suite)
  where
    analyze (stmtEnv,analysis) (i,stmt) =
      let place = Rows.StmtPlace path i
          summary = guard <> summarizeStmt stmtEnv stmt
          analysis1 = analysis {
            analyzedStatements = Map.insert place summary (analyzedStatements analysis)
          }
          analysis2 = analyzeNested constructor path guard stmtEnv i stmt analysis1
          env' = advanceReachEnv (QuickType.envOf stmt) stmtEnv
      in (env',analysis2)

analyzeNested :: Maybe Rows.DeclPlace
              -> Rows.SuitePath
              -> ReachSummary
              -> ReachEnv
              -> Int
              -> A.Stmt
              -> ContainerAnalysis
              -> ContainerAnalysis
analyzeNested constructor path guard env i stmt analysis = case stmt of
    A.If _ branches elseSuite -> analyzeElse afterBranches
      where
        conditions = [ summarizeCondition env e | A.Branch e _ <- branches ]
        prefixes = tail $ scanl (<>) mempty conditions
        branchData = zip3 [0..] branches prefixes
        afterBranches = foldl' analyzeBranch analysis branchData
        analyzeBranch acc (branch,A.Branch _ body,prefix) =
          analyzeSuite constructor (Rows.branchPath path i branch) (guard <> prefix) env body acc
        analyzeElse = analyzeSuite constructor (Rows.elsePath path i) (guard <> foldMap id conditions) env elseSuite
    A.Decl _ decls -> foldl' analyzeDecl analysis (zip [0..] decls)
      where
        declEnv = advanceReachEnv (QuickType.envOf decls) env
        analyzeDecl acc (j,decl) =
          let place = Rows.DeclPlace path i j
              (header,bodyEnv) = summarizeDeclHeader declEnv decl
              stubHeader = case decl of
                A.Def{} -> fst (summarizeDeclHeader
                  declEnv{ reachReflectiveOwner = False }
                  (Rows.methodHeader decl))
                _       -> header
              headerSummary = guard <> stubHeader
              full = guard <> header <> summarizeSuite bodyEnv (A.dbody decl)
              acc1 = acc {
                analyzedDecls = Map.insert place
                  (PlacedDeclSummary headerSummary full) (analyzedDecls acc)
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
                       -> ReachEnv
                       -> A.Suite
                       -> ContainerAnalysis
                       -> ContainerAnalysis
analyzeConstructorBody guard env suite analysis0 =
    snd $ foldl' analyze (env,analysis0) (zip [0..] suite)
  where
    analyze (stmtEnv,analysis) (i,stmt) =
      let summary = guard <> summarizeStmt stmtEnv stmt
          env' = advanceReachEnv (QuickType.envOf stmt) stmtEnv
      in (env',analysis {
           analyzedInitBody = Map.insert i
             (PlacedInitSummary stmtEnv guard summary)
             (analyzedInitBody analysis)
         })

residualSummary :: ContainerAnalysis -> Rows.SuiteShape -> Rows.RowResult ReachSummary
residualSummary analysis = go Rows.rootPath
  where
    go path shape = foldMapM (inStmt path) (Rows.suiteStructure shape)

    inStmt path (i,Rows.InlineStmt _) = do
      lookupStmt analysis (Rows.StmtPlace path i)
    inStmt path (i,Rows.DeclStmt _ decls) = do
      summaries <- mapM (inDecl path i) (zip [0..] decls)
      return (foldMap id summaries)
    inStmt path (i,Rows.IfStmt _ branches elseShape) = do
      branchSummaries <- mapM (uncurry $ inBranch path i) (zip [0..] branches)
      elseSummary <- go (Rows.elsePath path i) elseShape
      return (foldMap id branchSummaries <> elseSummary)

    inDecl path i (j,Rows.MethodDecl _) = do
      PlacedDeclSummary header _ <- lookupDecl analysis (Rows.DeclPlace path i j)
      return header
    inDecl path i (j,Rows.InlineDecl _) = do
      PlacedDeclSummary _ summary <- lookupDecl analysis (Rows.DeclPlace path i j)
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
        (foldMap id summaries)
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
      PlacedDeclSummary _ summary <- lookupDecl analysis (Rows.placedMethodPlace placed)
      return summary

    declarationSummary (Rows.PlacedStmt place _) = do
      lookupStmt analysis place

    initializerSummary attribute (Rows.InitFragment place stmt) = case place of
      Rows.InitSuite stmtPlace -> do
        lookupStmt analysis stmtPlace
      Rows.InitBody i -> do
        placed <- required ("missing constructor statement " ++ show i) $
          Map.lookup i (analyzedInitBody analysis)
        case attribute of
          Nothing -> return (initSummary placed)
          Just name -> do
            self <- required "constructor initializer has no self parameter"
              (analyzedConstructorSelf analysis)
            return $ case Rows.pruneConstructorInit self (Set.singleton name) stmt of
              Nothing -> mempty
              Just projected -> initGuard placed <>
                summarizeStmt (initEnvironment placed) projected


-- Shape, slot, and reflection rows ----------------------------------------------------------------------

prepareContainerMetadata :: Env.Env0
                         -> Forward.ForwardContext
                         -> Rows.InterfaceRows
                         -> Map.Map TopKey [ExtensionInfo]
                         -> ReachabilityRows
                         -> (TopKey,ContainerPrepared)
                         -> Rows.RowResult ReachabilityRows
prepareContainerMetadata env forwarding stored extensions rows (owner,prepared) = do
    rows1 <- prepareMemberRows owner prepared rows
    let targetExtensions = Map.findWithDefault [] owner extensions
    slots <- effectiveSlots env forwarding (topModule owner) stored targetExtensions owner
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
      info -> error ("Acton.Reachability: extension info expected for " ++ show name ++ ", got " ++ show info)
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
    _                              -> error ("Acton.Reachability: method info expected, got " ++ show info)
slotForInfo env provider (AttrRef _) info = case info of
    I.NSig{}
      | concretePropertyOwner      -> AttributeSlot
      | otherwise                  -> AbstractSlot
    I.NVar{}                       -> AttributeSlot
    I.NSVar{}                      -> AttributeSlot
    _                              -> error ("Acton.Reachability: attribute info expected, got " ++ show info)
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
    Just info       -> error ("Acton.Reachability: container interface expected, got " ++ show info)
    Nothing         -> backendContainerKind decl

backendContainerKind :: A.Decl -> ShapeKind
backendContainerKind A.Actor{}     = ActorShape
backendContainerKind A.Class{}     = ClassShape
backendContainerKind A.Protocol{}  = ProtocolShape
backendContainerKind A.Extension{} = WitnessShape
backendContainerKind decl          = error ("Acton.Reachability: container expected, got " ++ show decl)


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
      Nothing -> error ("Acton.Reachability: unscoped name " ++ A.rawstr n)

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
    places  -> error ("Acton.Reachability: multiple constructor slots " ++ show places)

methodSlots :: Rows.SuiteShape -> [Rows.MethodSlot]
methodSlots shape = concatMap (inStmt . snd) (Rows.suiteStructure shape)
  where
    inStmt (Rows.InlineStmt _) = []
    inStmt (Rows.DeclStmt _ decls) = [ slot | Rows.MethodDecl slot <- decls ]
    inStmt (Rows.IfStmt _ branches elseShape) =
      concatMap (methodSlots . snd) branches ++ methodSlots elseShape

lookupStmt :: ContainerAnalysis -> Rows.StmtPlace -> Rows.RowResult ReachSummary
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
wholeSummary :: ReachEnv -> ReachabilityRows -> ReachSummary
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
    inheritedPropertyLayoutSummary = reachSummaryFromEdges
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
        typeEnv = reachTypeEnv env

    -- Whole CodeGen emits every effective instance-property field in each
    -- class header, including inherited properties, whether or not the class
    -- is constructed. Their representation types therefore belong to the
    -- whole-module contract, but not to a selectively projected class top.
    propertyTypeSummary owner = foldMap propertyType
      (Env.fullAttrEnv (reachTypeEnv env) $
        containerTCon (reachTypeEnv env) owner)
    propertyType (_,I.NSig schema A.Property _) =
      summarizeType env (A.sctype schema)
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
      ("Acton.Reachability: container declaration expected, got " ++ show info)

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


-- Selection ---------------------------------------------------------------------------------------------

-- | On-demand access to persisted reachability rows.  The selector memoizes each
-- exact-key result for the duration of one run.
data ReachLookup m = ReachLookup
  { lookupTopRow           :: TopKey -> m (Maybe TopInfo)
  , lookupMemberRow        :: TopKey -> Rows.MemberKey -> m (Maybe MemberInfo)
  , lookupShapeRow         :: TopKey -> m (Maybe ShapeInfo)
  , lookupSlotRow          :: TopKey -> MemberRef -> m (Maybe SlotInfo)
  , lookupSurfaceSlots     :: TopKey -> m [(MemberRef, SlotInfo)]
  , lookupReflectableAttrs :: TopKey -> m (Maybe ReflectableAttrs)
  }

data Selection = Selection
  { selectedDeclarations :: Set.Set TopKey
  , selectedTops         :: Set.Set TopKey
  , selectedOpaqueTops   :: Set.Set TopKey
  , selectedMembers      :: Set.Set (TopKey, Rows.MemberKey)
  , selectedAttrs        :: Set.Set (TopKey, A.Name)
  , selectedStaticInitializers :: Set.Set (TopKey, A.Name)
  , selectedInstanceInitializers :: Set.Set (TopKey, A.Name)
  , selectedGenerated    :: Set.Set (TopKey, MemberRef)
  , selectedConstructed :: Set.Set TopKey
  , selectedInitialized :: Set.Set TopKey
  } deriving (Eq, Show)

emptySelection :: Selection
emptySelection = Selection
  { selectedDeclarations = Set.empty
  , selectedTops = Set.empty
  , selectedOpaqueTops = Set.empty
  , selectedMembers = Set.empty
  , selectedAttrs = Set.empty
  , selectedStaticInitializers = Set.empty
  , selectedInstanceInitializers = Set.empty
  , selectedGenerated = Set.empty
  , selectedConstructed = Set.empty
  , selectedInitialized = Set.empty
  }


-- Errors ------------------------------------------------------------------------------------------------

data SelectionError
  = MissingTop TopKey
  | MissingShape TopKey
  | InvalidLineage TopKey [TopKey]
  | MissingMemberSummary TopKey Rows.MemberKey
  | MissingSlot TopKey MemberRef
  | MissingReflectableAttrs TopKey
  | InvalidStoredSlot TopKey MemberRef Rows.MemberKey
  | InvalidSlotKind TopKey MemberRef SlotDecl
  | AbstractMemberSelected TopKey MemberRef TopKey
  | AbstractClassConstructed TopKey [MemberRef]
  | ProtocolConstructed TopKey
  | MissingConstructor TopKey
  | DynamicSerializationRequiresWhole
  deriving (Eq, Show)


-- Worklist ----------------------------------------------------------------------------------------------

data LookupCache = LookupCache
  { cachedTops             :: Map.Map TopKey (Maybe TopInfo)
  , cachedMembers          :: Map.Map (TopKey, Rows.MemberKey) (Maybe MemberInfo)
  , cachedShapes           :: Map.Map TopKey (Maybe ShapeInfo)
  , cachedSlots            :: Map.Map (TopKey, MemberRef) (Maybe SlotInfo)
  , cachedSurfaceSlots     :: Map.Map TopKey [(MemberRef, SlotInfo)]
  , cachedReflectableAttrs :: Map.Map TopKey (Maybe ReflectableAttrs)
  }

emptyLookupCache :: LookupCache
emptyLookupCache = LookupCache
  Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty

type SelectM m = ExceptT SelectionError (StateT LookupCache m)

liftLookup :: Monad m => m a -> SelectM m a
liftLookup = lift . lift

loadTopRow :: Monad m => ReachLookup m -> TopKey -> SelectM m (Maybe TopInfo)
loadTopRow lookups key = do
  cache <- lift get
  case Map.lookup key (cachedTops cache) of
    Just row -> return row
    Nothing -> do
      row <- liftLookup (lookupTopRow lookups key)
      lift $ modify' $ \c -> c{ cachedTops = Map.insert key row (cachedTops c) }
      return row

loadMemberRow :: Monad m => ReachLookup m -> TopKey -> Rows.MemberKey -> SelectM m (Maybe MemberInfo)
loadMemberRow lookups owner member = do
  cache <- lift get
  case Map.lookup key (cachedMembers cache) of
    Just row -> return row
    Nothing -> do
      row <- liftLookup (lookupMemberRow lookups owner member)
      lift $ modify' $ \c -> c{ cachedMembers = Map.insert key row (cachedMembers c) }
      return row
  where key = (owner, member)

loadShapeRow :: Monad m => ReachLookup m -> TopKey -> SelectM m (Maybe ShapeInfo)
loadShapeRow lookups key = do
  cache <- lift get
  case Map.lookup key (cachedShapes cache) of
    Just row -> return row
    Nothing -> do
      row <- liftLookup (lookupShapeRow lookups key)
      lift $ modify' $ \c -> c{ cachedShapes = Map.insert key row (cachedShapes c) }
      return row

loadSlotRow :: Monad m => ReachLookup m -> TopKey -> MemberRef -> SelectM m (Maybe SlotInfo)
loadSlotRow lookups receiver ref = do
  cache <- lift get
  case Map.lookup key (cachedSlots cache) of
    Just row -> return row
    Nothing -> do
      row <- liftLookup (lookupSlotRow lookups receiver ref)
      lift $ modify' $ \c -> c{ cachedSlots = Map.insert key row (cachedSlots c) }
      return row
  where key = (receiver, ref)

loadSurfaceSlots :: Monad m => ReachLookup m -> TopKey -> SelectM m [(MemberRef, SlotInfo)]
loadSurfaceSlots lookups receiver = do
  cache <- lift get
  case Map.lookup receiver (cachedSurfaceSlots cache) of
    Just slots -> return slots
    Nothing -> do
      slots <- liftLookup (lookupSurfaceSlots lookups receiver)
      lift $ modify' $ \c -> c
        { cachedSurfaceSlots = Map.insert receiver slots (cachedSurfaceSlots c) }
      return slots

loadReflectableAttrs :: Monad m => ReachLookup m -> TopKey -> SelectM m (Maybe ReflectableAttrs)
loadReflectableAttrs lookups receiver = do
  cache <- lift get
  case Map.lookup receiver (cachedReflectableAttrs cache) of
    Just attrs -> return attrs
    Nothing -> do
      attrs <- liftLookup (lookupReflectableAttrs lookups receiver)
      lift $ modify' $ \c -> c
        { cachedReflectableAttrs = Map.insert receiver attrs (cachedReflectableAttrs c) }
      return attrs

data WorkItem
  = ReachWork ReachEdge
  | InitializeWork TopKey
  | ReflectWork TopKey TopKey
  deriving (Eq, Ord, Show)

data Work = Work
  { workQueue          :: Seq.Seq WorkItem
  , workNeeded         :: Set.Set TopKey
  , workDeclarations   :: Set.Set TopKey
  , workTops           :: Set.Set TopKey
  , workOpaqueTops     :: Set.Set TopKey
  , workMembers        :: Set.Set (TopKey, Rows.MemberKey)
  , workAttrs          :: Set.Set (TopKey, A.Name)
  , workStaticInitializers :: Set.Set (TopKey, A.Name)
  , workInstanceInitializers :: Set.Set (TopKey, A.Name)
  , workGenerated      :: Set.Set (TopKey, MemberRef)
  , workConstructed    :: Set.Set TopKey
  , workInitialized    :: Set.Set TopKey
  , workDispatches     :: Set.Set (TopKey, MemberRef)
  , workReflections    :: Set.Set TopKey
  , workDispatchPairs  :: Set.Set (TopKey, MemberRef, TopKey)
  , workReflectionPairs :: Set.Set (TopKey, TopKey)
  }

emptyWork :: [ReachEdge] -> Work
emptyWork seeds = Work
  { workQueue = Seq.fromList (map ReachWork seeds)
  , workNeeded = Set.empty
  , workDeclarations = Set.empty
  , workTops = Set.empty
  , workOpaqueTops = Set.empty
  , workMembers = Set.empty
  , workAttrs = Set.empty
  , workStaticInitializers = Set.empty
  , workInstanceInitializers = Set.empty
  , workGenerated = Set.empty
  , workConstructed = Set.empty
  , workInitialized = Set.empty
  , workDispatches = Set.empty
  , workReflections = Set.empty
  , workDispatchPairs = Set.empty
  , workReflectionPairs = Set.empty
  }

finish :: Work -> Selection
finish w = Selection
  { selectedDeclarations = workDeclarations w
  , selectedTops = workTops w
  , selectedOpaqueTops = workOpaqueTops w
  , selectedMembers = workMembers w
  , selectedAttrs = workAttrs w
  , selectedStaticInitializers = workStaticInitializers w
  , selectedInstanceInitializers = workInstanceInitializers w
  , selectedGenerated = workGenerated w
  , selectedConstructed = workConstructed w
  , selectedInitialized = workInitialized w
  }

selectProgram :: Monad m => ReachLookup m -> [ReachEdge] -> m (Either SelectionError Selection)
selectProgram lookups seeds =
  evalStateT (runExceptT $ finish <$> drain lookups (emptyWork seeds)) emptyLookupCache

drain :: Monad m => ReachLookup m -> Work -> SelectM m Work
drain lookups work =
  case Seq.viewl (workQueue work) of
    Seq.EmptyL -> return work
    item Seq.:< rest -> do
      work' <- process lookups item work{ workQueue = rest }
      drain lookups work'

process :: Monad m => ReachLookup m -> WorkItem -> Work -> SelectM m Work
process lookups item work =
  case item of
    ReachWork edge -> processEdge lookups edge work
    InitializeWork receiver -> initializeReceiver lookups receiver work
    ReflectWork receiver concrete -> reflectConcrete lookups receiver concrete work

processEdge :: Monad m => ReachLookup m -> ReachEdge -> Work -> SelectM m Work
processEdge lookups edge work =
  case edge of
    Declare mn n -> return (declareTop (TopKey mn n) work)
    Need mn n -> selectTop lookups (TopKey mn n) work
    Inherit mn n -> selectTop lookups (TopKey mn n) work
    Construct mn n -> constructShape lookups (TopKey mn n) work
    Direct mn n ref -> directMember lookups (TopKey mn n) ref work
    Dispatch mn n ref -> dispatchMember lookups (TopKey mn n) ref work
    Reflect mn n -> reflectShape lookups (TopKey mn n) work
    DynamicSerialization -> throwE DynamicSerializationRequiresWhole
    DeclareAttr mn n attr -> declareAttribute lookups (TopKey mn n) attr work


-- Top-level and shape lookups -----------------------------------------------------------------------------

lookupTop :: Monad m => ReachLookup m -> TopKey -> SelectM m TopInfo
lookupTop lookups key = do
  mTop <- loadTopRow lookups key
  case mTop of
    Nothing -> throwE (MissingTop key)
    Just top -> return top

lookupShape :: Monad m => ReachLookup m -> TopKey -> SelectM m ShapeInfo
lookupShape lookups key = do
  mShape <- loadShapeRow lookups key
  case mShape of
    Nothing -> throwE (MissingShape key)
    Just shape -> return shape

selectTop :: Monad m => ReachLookup m -> TopKey -> Work -> SelectM m Work
selectTop lookups key work
  | Set.member key (workNeeded work) = return work
  | otherwise = do
      top <- lookupTop lookups key
      let work0 = work
            { workNeeded = Set.insert key (workNeeded work)
            , workDeclarations = Set.delete key (workDeclarations work)
            }
      case top of
        OpaqueTop summary -> return $ enqueueSummary summary work0
          { workOpaqueTops = Set.insert key (workOpaqueTops work0) }
        LocalTop header summary -> do
          let work1 = enqueueSummary summary work0
                { workTops = Set.insert key (workTops work0) }
          case header of
            Nothing -> return work1
            Just _ -> do
              shape <- lookupShape lookups key
              case shapeLineage shape of
                owner : inherited
                  | owner == key -> return $ foldl (flip enqueueNeed) work1 inherited
                lineage -> throwE (InvalidLineage key lineage)

enqueueNeed :: TopKey -> Work -> Work
enqueueNeed (TopKey mn n) = enqueueReach (Need mn n)

declareTop :: TopKey -> Work -> Work
declareTop key work
  | Set.member key (workNeeded work) = work
  | otherwise = work
      { workDeclarations = Set.insert key (workDeclarations work) }


-- Members ------------------------------------------------------------------------------------------------

lookupMemberInfo :: Monad m => ReachLookup m -> TopKey -> Rows.MemberKey -> SelectM m MemberInfo
lookupMemberInfo lookups owner member = do
  mInfo <- loadMemberRow lookups owner member
  case mInfo of
    Nothing -> throwE (MissingMemberSummary owner member)
    Just info -> return info

lookupMemberInfoMaybe :: Monad m => ReachLookup m -> TopKey -> Rows.MemberKey -> SelectM m (Maybe MemberInfo)
lookupMemberInfoMaybe = loadMemberRow

selectMember :: Monad m => ReachLookup m -> TopKey -> Rows.MemberKey -> Work -> SelectM m Work
selectMember lookups owner member work
  | Set.member key (workMembers work) = return work
  | otherwise = do
      work0 <- selectTop lookups owner work
      info <- lookupMemberInfo lookups owner member
      return $ enqueueSummary (memberSummary info) work0
        { workMembers = Set.insert key (workMembers work0) }
  where key = (owner, member)

-- Retain an instance-property declaration for layout without retaining its
-- prunable constructor-prefix initialization. This is distinct from reading
-- the attribute, which enters through Direct and selects both obligations.
declareAttribute :: Monad m
                 => ReachLookup m
                 -> TopKey
                 -> A.Name
                 -> Work
                 -> SelectM m Work
declareAttribute lookups receiver name work = do
  let ref = AttrRef name
  work0 <- selectTop lookups receiver work
  (owner, slot) <- resolveSlot lookups receiver ref
  case slot of
    AttributeSlot -> selectMember lookups owner (Rows.Attr name) work0
    _ -> throwE (InvalidSlotKind owner ref slot)

directMember :: Monad m => ReachLookup m -> TopKey -> MemberRef -> Work -> SelectM m Work
directMember lookups receiver ref work = do
  work0 <- selectTop lookups receiver work
  case ref of
    MethodRef n | n == Builtin.initKW ->
      return (enqueue (InitializeWork receiver) work0)
    _ -> do
      (owner, slot) <- resolveSlot lookups receiver ref
      selectSlot lookups receiver owner ref slot work0

selectSlot :: Monad m => ReachLookup m -> TopKey -> TopKey -> MemberRef -> SlotDecl -> Work -> SelectM m Work
selectSlot lookups receiver owner ref slot work = do
  top <- lookupTop lookups owner
  work0 <- selectTop lookups owner work
  case (top, slot) of
    (_, AbstractSlot) -> throwE (AbstractMemberSelected receiver ref owner)
    (OpaqueTop{}, AttributeSlot) -> case ref of
      AttrRef n -> selectReceiverInitializer lookups receiver n work0
      _ -> throwE (InvalidSlotKind owner ref slot)
    (OpaqueTop{}, OpaqueSlot) -> case ref of
      AttrRef n -> selectReceiverInitializer lookups receiver n work0
      _ -> return work0
    (OpaqueTop{}, _) -> return work0
    (_, StoredSlot member) -> do
      except (validateStoredSlot owner ref member)
      selectMember lookups owner member work0
    (_, AttributeSlot) ->
      case ref of
        AttrRef n -> selectAttributeSlot lookups receiver owner n work0
        _ -> throwE (InvalidSlotKind owner ref slot)
    (_, GeneratedSlot summary) -> selectGenerated owner ref summary work0
    (_, OpaqueSlot) -> return work0

validateStoredSlot :: TopKey -> MemberRef -> Rows.MemberKey -> Either SelectionError ()
validateStoredSlot owner (MethodRef n) member@(Rows.Method n')
  | n == n' = Right ()
  | otherwise = Left (InvalidStoredSlot owner (MethodRef n) member)
validateStoredSlot owner ref member = Left (InvalidStoredSlot owner ref member)

selectGenerated :: Monad m => TopKey -> MemberRef -> ReachSummary -> Work -> SelectM m Work
selectGenerated owner ref summary work
  | Set.member key (workGenerated work) = return work
  | otherwise = return $ enqueueSummary summary work
      { workGenerated = Set.insert key (workGenerated work) }
  where key = (owner, ref)


-- Exact provider resolution -----------------------------------------------------------------------------

resolveSlot :: Monad m => ReachLookup m -> TopKey -> MemberRef -> SelectM m (TopKey, SlotDecl)
resolveSlot lookups receiver ref = do
  mSlot <- loadSlotRow lookups receiver ref
  case mSlot of
    Nothing -> throwE (MissingSlot receiver ref)
    Just (SlotInfo owner slot) -> do
      except (validateSlot owner ref slot)
      return (owner, slot)

validateSlot :: TopKey -> MemberRef -> SlotDecl -> Either SelectionError ()
validateSlot owner ref (StoredSlot member) = validateStoredSlot owner ref member
validateSlot _ (AttrRef _) AttributeSlot = Right ()
validateSlot _ _ AbstractSlot = Right ()
validateSlot _ (MethodRef _) (GeneratedSlot _) = Right ()
validateSlot _ _ OpaqueSlot = Right ()
validateSlot owner ref slot = Left (InvalidSlotKind owner ref slot)

lookupReflectableRefs :: Monad m => ReachLookup m -> TopKey -> SelectM m [MemberRef]
lookupReflectableRefs lookups receiver = do
  shape <- lookupShape lookups receiver
  names <- foldM load Set.empty (shapeLineage shape)
  return (map AttrRef $ Set.toAscList names)
  where
    load names owner = do
      mAttrs <- loadReflectableAttrs lookups owner
      attrs <- case mAttrs of
        Nothing -> throwE (MissingReflectableAttrs owner)
        Just found -> return (reflectableAttrs found)
      return (Set.union names $ Set.fromList attrs)

compatible :: Monad m => ReachLookup m -> TopKey -> TopKey -> SelectM m Bool
compatible lookups receiver concrete = do
  shape <- lookupShape lookups concrete
  return (receiver `elem` shapeLineage shape)


-- Construction and initialization ----------------------------------------------------------------------

constructShape :: Monad m => ReachLookup m -> TopKey -> Work -> SelectM m Work
constructShape lookups concrete work = do
  work0 <- selectTop lookups concrete work
  shape <- lookupShape lookups concrete
  case shapeKind shape of
    ProtocolShape -> throwE (ProtocolConstructed concrete)
    _ -> return ()
  let abstracts = shapeAbstracts shape
  if not (null abstracts)
    then throwE (AbstractClassConstructed concrete abstracts)
    else if Set.member concrete (workConstructed work0)
      then return work0
      else do
        let work1 = enqueue (InitializeWork concrete) work0
              { workConstructed = Set.insert concrete (workConstructed work0) }
        work2 <- retainOpaqueBarrierAttrs shape work1
        work3 <- retainWitnessSlots shape work2
        work4 <- foldM (replayDispatch lookups concrete) work3
          (Set.toAscList $ workDispatches work3)
        foldM (replayReflection lookups concrete) work4
          (Set.toAscList $ workReflections work4)
  where
    retainOpaqueBarrierAttrs shape selected = do
      lineage <- mapM classify (shapeLineage shape)
      if not (hasOpaqueBarrier lineage)
        then return selected
        else do
          slots <- loadSurfaceSlots lookups concrete
          foldM (retainBarrierAttr lineage) selected slots

    classify key = do
      top <- lookupTop lookups key
      return (key,top)

    retainBarrierAttr lineage selected (ref@(AttrRef _),SlotInfo provider slot)
      | crossesOpaque lineage provider = do
          providerTop <- lookupTop lookups provider
          case providerTop of
            LocalTop{} -> selectSlot lookups concrete provider ref slot selected
            OpaqueTop{} -> return selected
    retainBarrierAttr _ selected _ = return selected

    crossesOpaque lineage provider = case break ((== provider) . fst) lineage of
      (prefix,_:_) -> any (opaque . snd) prefix
      _            -> False

    hasOpaqueBarrier [] = False
    hasOpaqueBarrier ((_,top):rest) =
      opaque top && any (not . opaque . snd) rest || hasOpaqueBarrier rest

    opaque OpaqueTop{} = True
    opaque LocalTop{}  = False

    retainWitnessSlots shape selected
      | shapeKind shape /= WitnessShape = return selected
      | otherwise = do
          slots <- loadSurfaceSlots lookups concrete
          foldM retain selected slots

    retain selected (_,SlotInfo _ AbstractSlot) = return selected
    retain selected (ref,_) = directMember lookups concrete ref selected

resolveConstructor :: Monad m => ReachLookup m -> TopKey -> SelectM m (TopKey, ConstructorDecl)
resolveConstructor lookups receiver = do
  shape <- lookupShape lookups receiver
  case shapeConstructor shape of
    Nothing -> throwE (MissingConstructor receiver)
    Just constructor -> return constructor

initializeReceiver :: Monad m => ReachLookup m -> TopKey -> Work -> SelectM m Work
initializeReceiver lookups receiver work = do
  work0 <- selectTop lookups receiver work
  (provider, constructor) <- resolveConstructor lookups receiver
  activateConstructor lookups receiver provider constructor work0

activateConstructor :: Monad m
                    => ReachLookup m
                    -> TopKey
                    -> TopKey
                    -> ConstructorDecl
                    -> Work
                    -> SelectM m Work
activateConstructor lookups receiver provider constructor work
  | Set.member receiver (workInitialized work) = return work
  | otherwise = do
      work0 <- selectTop lookups receiver work
      let work1 = work0{ workInitialized = Set.insert receiver (workInitialized work0) }
      providerTop <- lookupTop lookups provider
      work2 <- case constructor of
        StoredConstructor summary -> do
          let withSummary = enqueueSummary summary work1
          case providerTop of
            LocalTop{} -> selectMember lookups provider Rows.InitRest withSummary
            OpaqueTop{} -> return withSummary
        GeneratedConstructor summary -> case providerTop of
          LocalTop{} -> selectGenerated provider (MethodRef Builtin.initKW) summary work1
          OpaqueTop{} -> return (enqueueSummary summary work1)
        InheritedConstructor summary ->
          initializeReceiver lookups provider (enqueueSummary summary work1)
        OpaqueConstructor -> selectTop lookups provider work1
      foldM (selectInitForField lookups receiver) work2 (Set.toAscList $ workAttrs work2)

selectAttr :: Monad m => ReachLookup m -> TopKey -> A.Name -> Work -> SelectM m Work
selectAttr lookups owner attr work
  | Set.member field (workAttrs work) = return work
  | otherwise = do
      work0 <- selectTop lookups owner work
      info <- lookupMemberInfo lookups owner (Rows.Attr attr)
      work1 <- selectMember lookups owner (Rows.Attr attr) work0
      let work2 = work1{ workAttrs = Set.insert field (workAttrs work1) }
      work3 <- activateStaticInitializer lookups owner attr info work2
      foldM selectInit work3
        [ (initOwner, field) | initOwner <- Set.toAscList (workInitialized work2) ]
  where field = (owner, attr)
        selectInit w (initOwner, demanded) =
          selectInitForField lookups initOwner w demanded

selectAttributeSlot :: Monad m
                    => ReachLookup m
                    -> TopKey
                    -> TopKey
                    -> A.Name
                    -> Work
                    -> SelectM m Work
selectAttributeSlot lookups receiver owner name work = do
  selected <- selectAttr lookups owner name work
  selectInitForField lookups receiver selected (owner,name)

selectReceiverInitializer :: Monad m
                          => ReachLookup m
                          -> TopKey
                          -> A.Name
                          -> Work
                          -> SelectM m Work
selectReceiverInitializer lookups receiver name work = do
  top <- lookupTop lookups receiver
  case top of
    OpaqueTop{} -> return work
    LocalTop{} -> do
      mInfo <- lookupMemberInfoMaybe lookups receiver (Rows.Attr name)
      case mInfo of
        Nothing -> return work
        Just info -> do
          selected <- selectAttr lookups receiver name work
          activateInstanceInitializer lookups receiver name info selected

selectInitForField :: Monad m => ReachLookup m -> TopKey -> Work -> (TopKey, A.Name) -> SelectM m Work
selectInitForField lookups initOwner work field@(fieldOwner, attr) = do
  applies <- compatible lookups fieldOwner initOwner
  if not applies
    then return work
    else do
      mInfo <- lookupMemberInfoMaybe lookups initOwner (Rows.Attr attr)
      case mInfo of
        Nothing -> return work
        Just info -> do
          (provider, slot) <- resolveSlot lookups initOwner (AttrRef attr)
          case slot of
            AttributeSlot | (provider, attr) == field ->
              activateInstanceInitializer lookups initOwner attr info work
            OpaqueSlot | (provider, attr) == field -> return work
            _ -> return work

activateStaticInitializer :: Monad m => ReachLookup m -> TopKey -> A.Name -> MemberInfo -> Work -> SelectM m Work
activateStaticInitializer lookups owner attr info work =
  case memberStaticInitSummary info of
    Nothing -> return work
    Just summary
      | Set.member key (workStaticInitializers work) -> return work
      | otherwise -> do
          work0 <- selectMember lookups owner (Rows.Attr attr) work
          return $ enqueueSummary summary work0
            { workStaticInitializers = Set.insert key (workStaticInitializers work0) }
  where key = (owner, attr)

activateInstanceInitializer :: Monad m => ReachLookup m -> TopKey -> A.Name -> MemberInfo -> Work -> SelectM m Work
activateInstanceInitializer lookups owner attr info work =
  case memberInstanceInitSummary info of
    Nothing -> return work
    Just summary
      | Set.member key (workInstanceInitializers work) -> return work
      | otherwise -> do
          work0 <- selectMember lookups owner (Rows.Attr attr) work
          return $ enqueueSummary summary work0
            { workInstanceInitializers = Set.insert key (workInstanceInitializers work0) }
  where key = (owner, attr)


-- Dynamic dispatch and reflection -----------------------------------------------------------------------

dispatchMember :: Monad m => ReachLookup m -> TopKey -> MemberRef -> Work -> SelectM m Work
dispatchMember lookups receiver ref work = do
  work0 <- selectTop lookups receiver work
  work1 <- retainReceiverDeclaration lookups receiver ref work0
  let work2 = work1{ workDispatches = Set.insert (receiver, ref) (workDispatches work1) }
  foldM (dispatchToConstructed lookups receiver ref) work2 (Set.toAscList $ workConstructed work2)

-- Attribute declarations and their exact receiver initializer are required
-- even when no constructor is visible in the closed-world runtime set: an
-- opaque call can still return an instance of the static receiver. Overrides
-- remain tied to the concrete receivers replayed below.
retainReceiverDeclaration :: Monad m
                          => ReachLookup m
                          -> TopKey
                          -> MemberRef
                          -> Work
                          -> SelectM m Work
retainReceiverDeclaration _ _ MethodRef{} work = return work
retainReceiverDeclaration lookups receiver ref@(AttrRef _) work = do
  (provider, slot) <- resolveSlot lookups receiver ref
  case slot of
    AttributeSlot -> selectSlot lookups receiver provider ref slot work
    OpaqueSlot -> selectSlot lookups receiver provider ref slot work
    _ -> return work

dispatchToConstructed :: Monad m => ReachLookup m -> TopKey -> MemberRef -> Work -> TopKey -> SelectM m Work
dispatchToConstructed lookups receiver ref work concrete = do
  applies <- compatible lookups receiver concrete
  if applies then enqueueDispatchPair receiver ref concrete work else return work

replayDispatch :: Monad m => ReachLookup m -> TopKey -> Work -> (TopKey, MemberRef) -> SelectM m Work
replayDispatch lookups concrete work (receiver, ref) =
  dispatchToConstructed lookups receiver ref work concrete

enqueueDispatchPair :: Monad m => TopKey -> MemberRef -> TopKey -> Work -> SelectM m Work
enqueueDispatchPair receiver ref concrete work
  | Set.member key (workDispatchPairs work) = return work
  | otherwise = return $ enqueueReach (edgeDirect concrete ref) work
      { workDispatchPairs = Set.insert key (workDispatchPairs work) }
  where key = (receiver, ref, concrete)

reflectShape :: Monad m => ReachLookup m -> TopKey -> Work -> SelectM m Work
reflectShape lookups receiver work = do
  work0 <- selectTop lookups receiver work
  let work1 = work0{ workReflections = Set.insert receiver (workReflections work0) }
  foldM (reflectConstructed lookups receiver) work1 (Set.toAscList $ workConstructed work1)

reflectConstructed :: Monad m => ReachLookup m -> TopKey -> Work -> TopKey -> SelectM m Work
reflectConstructed lookups receiver work concrete = do
  applies <- compatible lookups receiver concrete
  if applies then enqueueReflectionPair receiver concrete work else return work

replayReflection :: Monad m => ReachLookup m -> TopKey -> Work -> TopKey -> SelectM m Work
replayReflection lookups concrete work receiver = reflectConstructed lookups receiver work concrete

enqueueReflectionPair :: Monad m => TopKey -> TopKey -> Work -> SelectM m Work
enqueueReflectionPair receiver concrete work
  | Set.member key (workReflectionPairs work) = return work
  | otherwise = return $ enqueue (ReflectWork receiver concrete) work
      { workReflectionPairs = Set.insert key (workReflectionPairs work) }
  where key = (receiver, concrete)

reflectConcrete :: Monad m => ReachLookup m -> TopKey -> TopKey -> Work -> SelectM m Work
reflectConcrete lookups receiver concrete work = do
  applies <- compatible lookups receiver concrete
  if not applies
    then return work
    else do
      attrs <- lookupReflectableRefs lookups concrete
      return $ foldl (flip $ enqueueReach . edgeDirect concrete) work attrs


-- Queue helpers -----------------------------------------------------------------------------------------

enqueue :: WorkItem -> Work -> Work
enqueue item work = work{ workQueue = workQueue work Seq.|> item }

enqueueReach :: ReachEdge -> Work -> Work
enqueueReach = enqueue . ReachWork

enqueueSummary :: ReachSummary -> Work -> Work
enqueueSummary summary work = foldl (flip enqueueReach) work (reachEdges summary)

edgeDirect :: TopKey -> MemberRef -> ReachEdge
edgeDirect (TopKey mn n) = Direct mn n
