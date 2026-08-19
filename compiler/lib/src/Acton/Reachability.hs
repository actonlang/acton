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
  ( prepareInterfaceRows
  , prepareReachabilityRows
  , TopKey(..)
  , ReachLookup(..)
  , Selection(..)
  , SelectedRow(..)
  , selectedTops
  , selectedOpaqueTops
  , selectedMembers
  , selectedAttrs
  , selectedStaticInitializers
  , selectedInstanceInitializers
  , selectedGenerated
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
import qualified Acton.Types as Types

import Control.DeepSeq (force)
import Control.Monad (foldM, mapAndUnzipM, unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, except, runExceptT, throwE)
import Control.Monad.Trans.State.Strict (StateT, get, modify', runStateT)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntSet as IntSet
import Data.List (foldl', partition)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, mapMaybe)
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

advanceReachEnv                   :: I.TEnv -> ReachEnv -> ReachEnv
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

-- Reachability walk -----------------------------------------------------------------------------------

-- All structural recursion is Names.summ with the hooks below; the rest of
-- this section is the semantic content only: declaration headers, name and
-- receiver classification, and the Normalizer-anticipating condition and
-- iterator reach.
reachWalk                         :: Names.Walk ReachEnv ReachSummary
reachWalk                         = (Names.plainWalk :: Names.Walk ReachEnv ReachSummary) {
                                      Names.wSeq       = \env s -> advanceReachEnv (QuickType.envOf s) env,
                                      Names.wSuiteEnv  = \env b -> advanceReachEnv (QuickType.envOf b) env,
                                      Names.wDecls     = \env ds -> advanceReachEnv (QuickType.envOf ds) env,
                                      Names.wDecl      = summarizeDeclHeader,
                                      Names.wLocal     = enterLocal,
                                      Names.wLet       = \env ss -> defineLocal (QuickType.envOf ss) env,
                                      Names.wPar       = \env n mt -> defineLocal [(n, I.NVar $ typedParam env n mt)] env,
                                      Names.wPat       = flip defineBound,
                                      Names.wItem      = flip defineBound,
                                      Names.wExcept    = flip defineBound,
                                      Names.wQBinds    = flip defineTVars,
                                      Names.wAssignRhs = classEquationEnv,
                                      Names.wVar       = needValueQName,
                                      Names.wDot       = \env e n -> summarizeReceiver env e <> memberSelection env e n,
                                      Names.wCall      = \env f -> maybe mempty (singletonReach . uncurry Construct) (constructorTarget env f),
                                      Names.wCond      = booleanReach,
                                      Names.wIter      = nextReach,
                                      Names.wTarg      = assignTarget,
                                      Names.wTCon      = typeConReach,
                                      Names.wTypeName  = needTypeQName }

summReach                         :: Names.Summ a => ReachEnv -> a -> ReachSummary
summReach                         = Names.summ reachWalk

summarizeSuite                    :: ReachEnv -> A.Suite -> ReachSummary
summarizeSuite                    = Names.summSuite reachWalk

summarizeStmt                     :: ReachEnv -> A.Stmt -> ReachSummary
summarizeStmt                     = summReach

summarizeDecl                     :: ReachEnv -> A.Decl -> ReachSummary
summarizeDecl env decl             = header <> summarizeSuite bodyEnv (A.declbody decl)
  where (header,bodyEnv)           = summarizeDeclHeader env decl

summarizeType                     :: ReachEnv -> A.Type -> ReachSummary
summarizeType                     = summReach

summarizeCondition                :: ReachEnv -> A.Expr -> ReachSummary
summarizeCondition env expr        = summReach env expr <> booleanReach env expr

-- Assignment targets that are implicit actor attributes dispatch on the owner.
assignTarget                      :: ReachEnv -> A.Pattern -> ReachSummary
assignTarget env pat               = case pat of
    A.PVar _ name _
      | Set.member name (reachImplicitAttrs env)
      , Just (A.GName mn owner) <- reachOwner env
                                    -> singletonReach (Dispatch mn owner $ AttrRef name)
    A.PParen _ p                   -> assignTarget env p
    A.PTuple _ pos kwd             -> posTargets pos <> kwdTargets kwd
    A.PList _ items rest           -> foldMap (assignTarget env) items <> foldMap (assignTarget env) rest
    _                              -> mempty
  where
    posTargets pos = case pos of
      A.PosPat p rest              -> assignTarget env p <> posTargets rest
      A.PosPatStar p               -> assignTarget env p
      A.PosPatNil                  -> mempty
    kwdTargets kwd = case kwd of
      A.KwdPat _ p rest            -> assignTarget env p <> kwdTargets rest
      A.KwdPatStar p               -> assignTarget env p
      A.KwdPatNil                  -> mempty

-- A type constructor in ordinary type position is declaration-only interest;
-- an alias is code and stays a full Need.
typeConReach                      :: ReachEnv -> A.QName -> ReachSummary
typeConReach env qn
  | typeAlias (reachTypeEnv env) qn = needTypeQName env qn
  | otherwise                       = declareTypeQName env qn


summarizeDeclHeader               :: ReachEnv -> A.Decl -> (ReachSummary,ReachEnv)
summarizeDeclHeader env decl       = case decl of
    A.Def _ n q p k a _ _ fx _    -> (reflect <> summReach env q <> parReach <> kwdReach <>
                                      summReach envQ a <> summReach envQ fx,
                                      bodyEnv)
      where reflect
              | reachReflectiveOwner env,
                n == Builtin.getAttrKW,
                Just owner <- reachOwner env
                                    = reflectReach env owner
              | otherwise         = mempty
            envQ                  = defineTVars q ((enterLocal env){ reachReflectiveOwner = False })
            (parReach,envP)       = Names.summPosPar reachWalk envQ p
            (kwdReach,bodyEnv)    = Names.summKwdPar reachWalk envP k
    A.Actor _ n q p k b _         -> (summReach env q <> parReach <> kwdReach, bodyEnv)
      where (live,deferredNames)  = QuickType.actorBindings p k b
            attrs                 = Set.fromList live
            deferred              = Set.fromList deferredNames
            envQ                  = (setDirectMembers b $ withReachOwner (A.NoQ n) $ defineTVars q (enterContainer env)) {
                                      reachImplicitAttrs = attrs,
                                      reachDeferredLocals = deferred
                                    }
            selfType             = A.tCon $ A.TC (A.NoQ n) (map A.tVar $ A.qbound q)
            envSelf              = defineLocal [(Names.self, I.NVar selfType)] envQ
            (parReach,envP)       = Names.summPosPar reachWalk envSelf p
            (kwdReach,envK)       = Names.summKwdPar reachWalk envP k
            bodyEnv               = envK{ reachLocals = Set.difference (reachLocals envK) attrs }
    A.Class _ n q cs b _          -> (summReach env q <> foldMap (summarizeBaseTCon envQ) cs, bodyEnv)
      where envQ                  = defineTVars q (enterLocal env)
            bodyEnv               = (setDirectMembers b $ clearContainerBindings $
                                      withReachOwner (A.NoQ n) $
                                      defineTVars (Env.selfQuant (A.NoQ n) q) (enterContainer env)) {
                                        reachReflectiveOwner = True,
                                        reachClassInitParams = classInitParams b
                                      }
    A.Protocol _ n q ps b _       -> (summReach env q <> foldMap (summarizeBaseTCon envQ) ps, bodyEnv)
      where envQ                  = defineTVars q (enterLocal env)
            bodyEnv               = setDirectMembers b $ clearContainerBindings $ withReachOwner (A.NoQ n) $
                                      defineTVars (Env.selfQuant (A.NoQ n) q) (enterContainer env)
    A.Typedef _ _ q t _           -> (summReach env q <> summReach envQ t, envQ)
      where envQ                  = defineTVars q (enterLocal env)
    A.Extension _ q c ps b _      -> (summReach env q <> summarizeBaseTCon envQ c <>
                                      foldMap (summarizeBaseTCon envQ) ps, bodyEnv)
      where envQ                  = defineTVars q (enterLocal env)
            bodyEnv               = setDirectMembers b $ clearContainerBindings $ withReachOwner (A.tcname c) $
                                      defineTVars (Env.selfQuant (A.tcname c) q) (enterContainer env)


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
  | otherwise                       = summReach env expr
  where
    staticReceiver (A.Var _ qn)     = needQName env qn
    staticReceiver (A.TApp _ e ts)  = staticReceiver e <> foldMap (summReach env) ts
    staticReceiver (A.Paren _ e)    = staticReceiver e
    staticReceiver e                = summReach env e

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



typedParam                        :: ReachEnv -> A.Name -> Maybe A.Type -> A.Type
typedParam _ _ (Just t)            = t
typedParam env n Nothing           = reachError0 env ("untyped reconstructed parameter " ++ A.rawstr n)


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
summarizeBaseTCon env (A.TC qn ts) = needTypeQName env qn <> foldMap (summReach env) ts


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


-- Syntax partitioning ----------------------------------------------------------------------------------

data ActorPlan = ActorPlan
  { actorLocals         :: Set.Set A.Name
  , actorParameterAttrs :: [A.Name]
  } deriving Eq

data ContainerKind
  = KActor ActorPlan
  | KClass (Set.Set A.Name)
  | KProtocol
  | KExtension
  deriving Eq

data InitRoute = RouteTop | RouteAttrs [A.Name] | RouteRest deriving Eq

data AttrAccum = AttrAccum
  { accumDecls :: [Rows.Fragment]
  , accumInits :: [Rows.Fragment]
  }

data PrepareState = PrepareState
  { preparedMethods     :: Map.Map A.Name [A.Decl]
  , preparedAttrs       :: Map.Map A.Name AttrAccum
  , preparedRest        :: [Rows.Fragment]
  , preparedConstructor :: Maybe A.Decl
  , methodCounts        :: Map.Map A.Name Int
  , nextHole            :: Int
  }

emptyPrepareState :: PrepareState
emptyPrepareState = PrepareState Map.empty Map.empty [] Nothing Map.empty 0

prepareInterfaceRows :: Env.Env0 -> A.Module -> Rows.RowResult Rows.InterfaceRows
prepareInterfaceRows env (A.Module mn imps doc suite) = do
    (stmts, containers) <- mapAndUnzipM (prepareTopStmt env partitionEnv) suite
    let prepared = concat containers
        names = map (Rows.shapeName . fst) prepared
    when (length names /= Set.size (Set.fromList names)) $
      Rows.rowError "duplicate top-level container names"
    return Rows.InterfaceRows
      { Rows.rowModuleName = mn
      , Rows.rowImports = imps
      , Rows.rowDoc = doc
      , Rows.rowHasNotImpl = A.hasNotImpl suite
      , Rows.rowStatements = stmts
      , Rows.rowShapes = Map.fromList [ (Rows.shapeName shape, shape) | (shape, _) <- prepared ]
      , Rows.rowMembers = Map.fromList [ (Rows.shapeName shape, members) | (shape, members) <- prepared ]
      }
  where
    partitionEnv = Env.define (QuickType.envOfTopSuite suite) (Env.setMod mn env)

prepareTopStmt :: Env.Env0
               -> Env.Env0
               -> A.Stmt
               -> Rows.RowResult (Rows.StoredStmt, [(Rows.ContainerShape, Map.Map Rows.MemberKey Rows.MemberContent)])
prepareTopStmt semanticEnv backendEnv (A.Decl l decls) = do
    prepared <- mapM prepare decls
    return (Rows.StoredDecls l (map fst prepared), [ row | (_, Just row) <- prepared ])
  where
    prepare decl
      | isContainer decl = do
          row <- prepareContainer semanticEnv backendEnv decl
          return (Rows.StoredContainer (Names.dname' decl), Just row)
      | otherwise = return (Rows.StoredInline decl, Nothing)
prepareTopStmt _ _ stmt = return (Rows.StoredWhole (wholeStmtOwners stmt) stmt, [])

isContainer :: A.Decl -> Bool
isContainer A.Actor{}     = True
isContainer A.Class{}     = True
isContainer A.Protocol{}  = True
isContainer A.Extension{} = True
isContainer _             = False

prepareContainer :: Env.Env0
                 -> Env.Env0
                 -> A.Decl
                 -> Rows.RowResult (Rows.ContainerShape, Map.Map Rows.MemberKey Rows.MemberContent)
prepareContainer semanticEnv backendEnv decl = do
    constructor <- classConstructor decl
    let (kind0, head', suite) = containerParts decl
        kind = case kind0 of
          KClass _ -> KClass (classAttrs semanticEnv backendEnv $ Names.dname' decl)
          _ -> kind0
        initial = case kind of
          KActor plan -> foldl' (flip ensureAttr) emptyPrepareState (actorParameterAttrs plan)
          _ -> emptyPrepareState
    (shape, state0) <- prepareSuite (containerBodyEnv backendEnv decl)
      kind (isConstructor constructor) RouteTop suite initial
    state1 <- maybe (return state0) (prepareConstructor backendEnv suite state0) constructor
    return
      ( Rows.ContainerShape (Names.dname' decl) head' shape
      , finishMembers kind state1
      )
  where
    isConstructor Nothing _ = False
    isConstructor (Just target) candidate = target == candidate

containerBodyEnv :: Env.Env0 -> A.Decl -> Env.Env0
containerBodyEnv env A.Actor{A.dname=n,A.qbinds=q,A.pos=p,A.kwd=k} =
    Env.define (QuickType.envOf p ++ QuickType.envOf k) $
    Env.setInAct $
    Env.define [(Builtin.selfKW, I.NVar $ A.tCon tc)] $
    Env.defineTVars q env
  where tc = A.TC (A.NoQ n) (map A.tVar $ A.qbound q)
containerBodyEnv env _ = env

containerParts :: A.Decl -> (ContainerKind, Rows.ContainerHead, A.Suite)
containerParts (A.Actor l n q p k suite doc) =
    (KActor (makeActorPlan p k suite), Rows.ActorHead l n q p k doc, suite)
containerParts (A.Class l n q bases suite doc) =
    (KClass Set.empty, Rows.ClassHead l n q bases doc, suite)
containerParts (A.Protocol l n q bases suite doc) =
    (KProtocol, Rows.ProtocolHead l n q bases doc, suite)
containerParts (A.Extension l q con bases suite doc) =
    (KExtension, Rows.ExtensionHead l q con bases doc, suite)
containerParts decl = error ("containerParts: " ++ show decl)

classAttrs :: Env.Env0 -> Env.Env0 -> A.Name -> Set.Set A.Name
classAttrs semanticEnv backendEnv name = case Env.tryQName qn semanticEnv of
    Just (I.NClass _ _ semanticMembers _) -> attrs semanticMembers `Set.union` backendAttrs
    Just I.NProto{} -> backendAttrs
    Just I.NExt{}   -> backendAttrs
    Just info       -> expected info
    Nothing         -> backendAttrs
  where
    backendAttrs = case Env.findQName qn backendEnv of
      I.NClass _ _ members _ -> attrs members
      info                   -> expected info
    attrs members = Set.fromList [ member | (member,info) <- members, isAttr info ]
    isAttr I.NVar{}  = True
    isAttr I.NSVar{} = True
    isAttr _         = False
    expected info = error ("prepareInterfaceRows: class info expected for " ++ show name ++
                           ", got " ++ show info)
    qn = A.NoQ name

makeActorPlan :: A.PosPar -> A.KwdPar -> A.Suite -> ActorPlan
makeActorPlan p k body = ActorPlan locals parameterAttrs
  where
    (liveVars,_) = QuickType.actorBindings p k body
    paramNames = Names.bound (p,k)
    locals = Set.fromList (Env.uniqueNames (liveVars ++ Names.bound (filter A.isDecl body)))
    parameterAttrs = filter (`Set.member` locals) paramNames

prepareSuite :: Env.Env0
             -> ContainerKind
             -> (A.Decl -> Bool)
             -> InitRoute
             -> A.Suite
             -> PrepareState
             -> Rows.RowResult (Rows.SuiteShape, PrepareState)
prepareSuite env kind constructor route suite state = do
    (stmts, state', _) <- foldM prepare ([], state, env) suite
    return (Rows.SuiteShape (reverse stmts), state')
  where
    prepare (stmts, acc, stmtEnv) stmt = do
      (stored, acc') <- prepareStmt stmtEnv kind constructor route stmt acc
      return (stored : stmts, acc', Env.define (QuickType.envOf stmt) stmtEnv)

prepareStmt :: Env.Env0
            -> ContainerKind
            -> (A.Decl -> Bool)
            -> InitRoute
            -> A.Stmt
            -> PrepareState
            -> Rows.RowResult (Rows.ShapeStmt, PrepareState)
prepareStmt env kind@(KActor plan) constructor RouteTop stmt state
  | not (A.isDecl stmt), not (A.isSig stmt) = do
      attrs <- prunableActorStmtAttrs env plan stmt
      let state'
            | null attrs = foldl' (flip ensureAttr) state (actorStmtAttrs plan stmt)
            | otherwise = state
      prepareStmt env kind constructor
        (if null attrs then RouteRest else RouteAttrs (Env.uniqueNames attrs)) stmt state'
prepareStmt env kind@(KClass attrs) constructor RouteTop stmt state
  | not (A.isDecl stmt), not (A.isSig stmt) = do
      names <- classStmtAttrs attrs stmt
      prepareStmt env kind constructor
        (if null names then RouteRest else RouteAttrs names) stmt state
prepareStmt env kind constructor route (A.If l branches elseSuite) state = do
    (storedBranches, state1) <- foldM prepareBranch ([], state) branches
    (storedElse, state2) <- prepareSuite env kind constructor
      (branchRoute kind route elseSuite) elseSuite state1
    return (Rows.IfStmt l (reverse storedBranches) storedElse, state2)
  where
    prepareBranch (stored, acc) (A.Branch condition body) = do
      (body', acc') <- prepareSuite env kind constructor
        (branchRoute kind route body) body acc
      return ((condition, body') : stored, acc')
prepareStmt _ _ constructor _ (A.Decl l decls) state = do
    (stored, state') <- foldM prepare ([], state) decls
    return (Rows.DeclStmt l (reverse stored), state')
  where
    prepare (decls, acc) decl@A.Def{} = do
      let name = A.dname decl
          ordinal = Map.findWithDefault 0 name (methodCounts acc)
          slot = Rows.MethodSlot name ordinal (constructor decl) (Rows.methodHeader decl)
          acc'
            | constructor decl = acc { preparedConstructor = Just decl }
            | otherwise = acc
                { preparedMethods = Map.insertWith (flip (++)) name [decl] (preparedMethods acc)
                , methodCounts = Map.insert name (ordinal + 1) (methodCounts acc)
                }
      when (constructor decl && preparedConstructor acc /= Nothing) $
        Rows.rowError "multiple constructor slots"
      return (Rows.MethodDecl slot : decls, acc')
    prepare _ decl | isContainer decl = Rows.rowError "nested container declarations are not supported"
    prepare (decls, acc) decl = return (Rows.InlineDecl decl : decls, acc)
prepareStmt _ kind _ (RouteAttrs names) stmt state = do
    owners <- initializerStmtAttrs kind names stmt
    return (Rows.HoleStmt hole, addAttrInitializerGroup owners fragment state')
  where
    (hole, state') = allocateHole state
    fragment = Rows.SuiteFragment hole stmt
prepareStmt _ _ _ RouteRest stmt state =
    return (Rows.HoleStmt hole, addRestInitializer (Rows.SuiteFragment hole stmt) state')
  where
    (hole, state') = allocateHole state
prepareStmt _ _ _ _ stmt@(A.Signature _ names _ A.Property) state
  | null names = Rows.rowError "empty property signature"
  | otherwise =
      return (Rows.HoleStmt hole, addAttrDeclaration names (Rows.SuiteFragment hole stmt) state')
  where
    (hole, state') = allocateHole state
prepareStmt _ _ _ _ stmt state = return (Rows.InlineStmt stmt, state)

allocateHole :: PrepareState -> (Int, PrepareState)
allocateHole state = (nextHole state, state { nextHole = nextHole state + 1 })

-- A conditional keeps its shape in the container row, but each branch body is
-- stored only with the attributes assigned by that branch.  Statements which
-- assign a particular attribute narrow further to that attribute; shared
-- preparatory statements remain with the branch group.
branchRoute :: ContainerKind -> InitRoute -> A.Suite -> InitRoute
branchRoute kind (RouteAttrs names) suite =
    case filter (`Set.member` assigned) names of
      []       -> RouteRest
      selected -> RouteAttrs selected
  where assigned = Set.fromList (initializerSuiteAttrs kind suite)
branchRoute _ route _ = route

initializerSuiteAttrs :: ContainerKind -> A.Suite -> [A.Name]
initializerSuiteAttrs (KActor plan) suite =
    [ name | name <- Env.uniqueNames (Names.bound suite)
           , Set.member name (actorLocals plan)
    ]
initializerSuiteAttrs (KClass attrs) suite =
    [ name | name <- Env.uniqueNames (Names.assigned suite)
           , Set.member name attrs
    ]
initializerSuiteAttrs _ _ = []

initializerStmtAttrs :: ContainerKind -> [A.Name] -> A.Stmt -> Rows.RowResult [A.Name]
initializerStmtAttrs kind owners stmt = do
    assigned <- case kind of
      KActor plan -> return (actorStmtAttrs plan stmt)
      KClass attrs -> classStmtAttrs attrs stmt
      _ -> return []
    let assignedSet = Set.fromList assigned
        exact = filter (`Set.member` assignedSet) owners
    return (if null assigned then owners else exact)

actorStmtAttrs :: ActorPlan -> A.Stmt -> [A.Name]
actorStmtAttrs _ (A.VarAssign _ patterns _) = typedPatternNames patterns
actorStmtAttrs plan (A.Assign _ patterns _) =
    [ name | name <- typedPatternNames patterns, Set.member name (actorLocals plan) ]
actorStmtAttrs plan stmt@A.If{} =
    [ name | name <- actorStmtNames stmt, Set.member name (actorLocals plan) ]
actorStmtAttrs _ _ = []

actorStmtNames :: A.Stmt -> [A.Name]
actorStmtNames = Env.uniqueNames . Names.bound

prunableActorStmtAttrs :: Env.Env0 -> ActorPlan -> A.Stmt -> Rows.RowResult [A.Name]
prunableActorStmtAttrs env plan stmt@A.VarAssign{A.expr=expr}
  | pureActorExpr env expr = actorAttrsOnly plan stmt
prunableActorStmtAttrs env plan stmt@(A.Assign _ _ expr)
  | pureActorExpr env expr = actorAttrsOnly plan stmt
prunableActorStmtAttrs env plan stmt@(A.If _ branches elseSuite)
  | pureActorBranches env branches elseSuite = actorAttrsOnly plan stmt
prunableActorStmtAttrs _ _ _ = return []

actorAttrsOnly :: ActorPlan -> A.Stmt -> Rows.RowResult [A.Name]
actorAttrsOnly plan stmt
  | Set.fromList attrs == Set.fromList (actorStmtNames stmt) = return attrs
  | otherwise = return []
  where attrs = actorStmtAttrs plan stmt

pureActorExpr :: Env.Env0 -> A.Expr -> Bool
pureActorExpr _ A.NotImplemented{} = False
pureActorExpr env (A.Let _ suite expr) =
    pureActorSuite env suite && pureActorExpr (Env.define (QuickType.envOf suite) env) expr
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
pureActorStmt env (A.If _ branches elseSuite) = pureActorBranches env branches elseSuite
pureActorStmt _ A.Pass{} = True
pureActorStmt _ A.Signature{} = True
pureActorStmt _ A.Decl{} = True
pureActorStmt _ _ = False

typedPatternNames :: [A.Pattern] -> [A.Name]
typedPatternNames = Env.uniqueNames . map fst . QuickType.envOf

classStmtAttrs :: Set.Set A.Name -> A.Stmt -> Rows.RowResult [A.Name]
classStmtAttrs attrs stmt@A.Assign{} = assignedAttrs attrs stmt
classStmtAttrs attrs stmt@A.VarAssign{} = assignedAttrs attrs stmt
classStmtAttrs attrs (A.If _ branches elseSuite) = case branches of
    [] -> Rows.rowError "class initializer If has no branches"
    _ -> return $ Set.toAscList $ Set.intersection attrs assigned
  where
    suites = [ body | A.Branch _ body <- branches ] ++ [elseSuite]
    assigned = Set.fromList (concatMap Names.assigned suites)
classStmtAttrs _ _ = return []

assignedAttrs :: Set.Set A.Name -> A.Stmt -> Rows.RowResult [A.Name]
assignedAttrs attrs stmt
  | null selected = return []
  | Set.fromList bound == Set.fromList selected = return (Env.uniqueNames selected)
  | otherwise = Rows.rowError "class assignment mixes attribute and non-attribute bindings"
  where
    bound = Names.bound stmt
    selected = filter (`Set.member` attrs) bound

classConstructor :: A.Decl -> Rows.RowResult (Maybe A.Decl)
classConstructor A.Class{A.dbody=suite} = case direct ++ nested of
    [] -> return Nothing
    [decl]
      | null nested
      , A.selfPar decl /= Nothing -> return (Just decl)
      | null nested -> Rows.rowError "__init__ has no self parameter"
      | otherwise -> Rows.rowError "conditional __init__ declarations are not supported"
    _ -> Rows.rowError "multiple __init__ declarations are not supported"
  where
    direct =
      [ decl
      | A.Decl _ decls <- suite
      , decl@A.Def{} <- decls
      , A.dname decl == Builtin.initKW
      ]
    nested = concatMap nestedConstructors suite
classConstructor _ = return Nothing

nestedConstructors :: A.Stmt -> [A.Decl]
nestedConstructors (A.If _ branches elseSuite) =
    [ decl
    | suite <- [ body | A.Branch _ body <- branches ] ++ [elseSuite]
    , stmt <- suite
    , decl <- inStmt stmt
    ]
  where
    inStmt (A.Decl _ decls) = [ decl | decl@A.Def{} <- decls, A.dname decl == Builtin.initKW ]
    inStmt stmt = nestedConstructors stmt
nestedConstructors _ = []

addAttrDeclaration :: [A.Name] -> Rows.Fragment -> PrepareState -> PrepareState
addAttrDeclaration names fragment state =
    state { preparedAttrs = foldl' add (preparedAttrs state) group }
  where
    group = Env.uniqueNames names
    add attrs name = Map.alter (Just . update) name attrs
    update Nothing = AttrAccum [fragment] []
    update (Just old) = old { accumDecls = fragment : accumDecls old }

ensureAttr :: A.Name -> PrepareState -> PrepareState
ensureAttr name state =
    state { preparedAttrs = Map.alter (Just . maybe empty id) name (preparedAttrs state) }
  where empty = AttrAccum [] []

addAttrInitializerGroup :: [A.Name] -> Rows.Fragment -> PrepareState -> PrepareState
addAttrInitializerGroup names fragment state =
    state { preparedAttrs = foldl' add (preparedAttrs state) group }
  where
    group = Env.uniqueNames names
    add attrs name = Map.alter (Just . update) name attrs
    update Nothing = AttrAccum [] [fragment]
    update (Just old) = old { accumInits = fragment : accumInits old }

addRestInitializer :: Rows.Fragment -> PrepareState -> PrepareState
addRestInitializer fragment state = state { preparedRest = fragment : preparedRest state }

prepareConstructor :: Env.Env0 -> A.Suite -> PrepareState -> A.Decl -> Rows.RowResult PrepareState
prepareConstructor env classSuite state decl = do
    self <- maybe (Rows.rowError "__init__ has no self parameter") return (A.selfPar decl)
    let body = A.dbody decl
        (_, prefixLength) = Types.scanInitPrefix env self classSuite body
        indexed = zip [0..] body
        prefix = take prefixLength indexed
        uses = Map.fromListWith IntSet.union
          [ (v, IntSet.singleton i)
          | (i, stmt) <- indexed
          , v <- Set.toList (Set.fromList (Names.free stmt))
          ]
        writeOwners = Map.fromListWith Set.union
          [ (i, Set.singleton attr)
          | (i, stmt) <- prefix
          , attr <- selfAttributes self stmt
          ]
        owners = foldr (ownDefinition uses) writeOwners prefix
        groupIndices = Map.fromListWith IntSet.union
          [ (attr, IntSet.singleton i)
          | (i, attrs) <- Map.toList owners
          , attr <- Set.toList attrs
          ]
        consumed = IntSet.unions (Map.elems groupIndices)
        stmtByIndex = Map.fromList indexed
        addGroup acc (attr, indices) = foldl'
          (\acc' i -> addAttrInitializerGroup [attr]
            (Rows.ConstructorFragment i (stmtByIndex Map.! i)) acc')
          acc (IntSet.toAscList indices)
        withAttrs = foldl' addGroup state (Map.toList groupIndices)
        rest =
          [ Rows.ConstructorFragment i stmt
          | (i, stmt) <- indexed
          , not (IntSet.member i consumed)
          ]
    return withAttrs
      { preparedRest = reverse rest ++ preparedRest withAttrs
      , preparedConstructor = Just decl { A.dbody = [] }
      }

ownDefinition :: Map.Map A.Name IntSet.IntSet
              -> (Int, A.Stmt)
              -> Map.Map Int (Set.Set A.Name)
              -> Map.Map Int (Set.Set A.Name)
ownDefinition uses (i, A.Assign _ [A.PVar _ name _] _) owners
  | not (IntSet.null usedAt)
  , all (> i) (IntSet.toList usedAt)
  , Just attrs <- foldM collect Set.empty (IntSet.toList usedAt)
  = Map.insert i attrs owners
  where
    usedAt = Map.findWithDefault IntSet.empty name uses
    collect attrs used = Set.union attrs <$> Map.lookup used owners
ownDefinition _ _ owners = owners

selfAttributes :: A.Name -> A.Stmt -> [A.Name]
selfAttributes self stmt = Env.uniqueNames $ case stmt of
    A.MutAssign _ (A.Dot _ (A.Var _ (A.NoQ receiver)) attr) _
      | receiver == self -> [attr]
    A.AugAssign _ (A.Dot _ (A.Var _ (A.NoQ receiver)) attr) _ _
      | receiver == self -> [attr]
    A.If _ branches elseSuite ->
      concat [ concatMap (selfAttributes self) body | A.Branch _ body <- branches ] ++
      concatMap (selfAttributes self) elseSuite
    A.While _ _ body elseSuite -> concatMap (selfAttributes self) (body ++ elseSuite)
    A.For _ _ _ body elseSuite -> concatMap (selfAttributes self) (body ++ elseSuite)
    A.Try _ body handlers elseSuite finallySuite ->
      concatMap (selfAttributes self) body ++
      concat [ concatMap (selfAttributes self) hbody | A.Handler _ hbody <- handlers ] ++
      concatMap (selfAttributes self) (elseSuite ++ finallySuite)
    A.With _ _ body -> concatMap (selfAttributes self) body
    A.Data _ _ body -> concatMap (selfAttributes self) body
    _ -> []

finishMembers :: ContainerKind -> PrepareState -> Map.Map Rows.MemberKey Rows.MemberContent
finishMembers kind state = Map.fromList (methods ++ attrs ++ initializers ++ rest)
  where
    methods =
      [ (Rows.Method name, Rows.MethodContent decls)
      | (name, decls) <- Map.toList (preparedMethods state)
      ]
    attrs =
      [ ( Rows.Attr name
        , Rows.AttrContent (reverse $ accumDecls attr)
        )
      | (name, attr) <- Map.toList (preparedAttrs state)
      ]
    initializers = concat
      [ entries name (reverse $ accumInits attr)
      | (name,attr) <- Map.toList (preparedAttrs state)
      ]
    entries name fragments =
      [ (Rows.StaticInit name, Rows.InitializerContent static)
      | not (null static)
      ] ++
      [ (Rows.InstanceInit name, Rows.InitializerContent instance')
      | not (null instance')
      ]
      where
        (static,instance') = partitionFragments fragments
    partitionFragments fragments = case kind of
      KClass{} -> partition isSuiteFragment fragments
      KActor{} -> ([],fragments)
      _        -> ([],[])
    isSuiteFragment Rows.SuiteFragment{} = True
    isSuiteFragment _                    = False
    rest
      | preparedConstructor state == Nothing && null (preparedRest state) = []
      | otherwise =
          [ (Rows.InitRest, Rows.InitRestContent
              (preparedConstructor state)
              (reverse $ preparedRest state))
          ]


-- Preparation -------------------------------------------------------------------------------------------

-- Walk the typed module once and assign each dependency summary to the row
-- which stores that syntax.  The row skeleton is only consulted to locate the
-- separately stored parts of a container; it is never reconstructed here.
prepareReachabilityRows :: Env.Env0
                        -> I.TEnv
                        -> A.Module
                        -> Rows.InterfaceRows
                        -> Rows.RowResult ReachabilityRows
prepareReachabilityRows typeEnv sourceInterface typed@(A.Module mn _ _ suite) stored = do
    when (Rows.rowModuleName stored /= mn) $
      Rows.rowError "module name does not match interface rows"
    let suiteEnv = QuickType.envOfTopSuite suite
        moduleEnv = Env.define suiteEnv (Env.setMod mn typeEnv)
        globals = Set.fromList (topNames suite)
        env0 = topReachEnv moduleEnv globals
        extensions = Map.fromListWith (flip (++))
          [ (extensionTarget ext,[ext])
          | ext <- extensionInfos moduleEnv mn suite
          ]
        sourceInfo = Env.hnamesFrom sourceInterface
    (prepared,_) <- foldM
      (prepareTop moduleEnv sourceInfo stored extensions)
      (emptyReachabilityRows, env0)
      suite
    let whole = force (wholeSummary env0 prepared)
    typed `seq` whole `seq` return prepared{ reachWholeSummary = whole }


data ContainerPrepared = ContainerPrepared
  { preparedKind              :: ShapeKind
  , preparedMembers           :: Map.Map Rows.MemberKey Rows.MemberContent
  , preparedMemberInfo        :: Map.Map Rows.MemberKey MemberInfo
  , preparedConstructorHeader :: Maybe ReachSummary
  }

prepareTop :: Env.Env0
           -> I.HTEnv
           -> Rows.InterfaceRows
           -> Map.Map TopKey [ExtensionInfo]
           -> (ReachabilityRows, ReachEnv)
           -> A.Stmt
           -> Rows.RowResult (ReachabilityRows, ReachEnv)
prepareTop moduleEnv sourceInfo stored extensions (rows,env) stmt = do
    let env' = advanceReachEnv (QuickType.envOf stmt) env
    rows' <- case stmt of
      A.Decl _ decls ->
        foldM
          (prepareTopDecl moduleEnv sourceInfo stored extensions
            (advanceReachEnv (QuickType.envOf decls) env))
          rows
          decls
      _ ->
        let owners = wholeStmtOwners stmt
            summary = summarizeStmt env stmt
        in if null owners
             then return rows
               { reachModuleSummary = reachModuleSummary rows <> summary }
             else foldM (insertTopSummary Nothing summary) rows
               [ TopKey (Rows.rowModuleName stored) name | name <- owners ]
    return (rows',env')

prepareTopDecl :: Env.Env0
               -> I.HTEnv
               -> Rows.InterfaceRows
               -> Map.Map TopKey [ExtensionInfo]
               -> ReachEnv
               -> ReachabilityRows
               -> A.Decl
               -> Rows.RowResult ReachabilityRows
prepareTopDecl moduleEnv sourceInfo stored extensions env rows decl
  | not (isContainer decl) =
        insertTopSummary Nothing (summarizeDecl env decl) rows owner
  | otherwise = do
        shape <- required ("missing container shape " ++ A.rawstr name) $
          Map.lookup name (Rows.rowShapes stored)
        members <- required ("missing member rows " ++ A.rawstr name) $
          Map.lookup name (Rows.rowMembers stored)
        let kind = containerKind sourceInfo decl
            backendKind = backendContainerKind decl
            (_,_,suite) = containerParts decl
            (header,bodyEnv) = summarizeDeclHeader env decl
        analysis <- analyzeContainer backendKind bodyEnv shape members suite
        rows' <- insertTopSummary
          (compactDeclaration $ Env.unalias (reachTypeEnv env) $
            Env.findQName (A.NoQ name) (reachTypeEnv env))
          (header <> analyzedResidual analysis)
          rows
          owner
        prepareContainerMetadata moduleEnv stored extensions rows'
          (owner,ContainerPrepared
            kind
            members
            (analyzedMembers analysis)
            (analyzedConstructorHeader analysis))
  where
    name = Names.dname' decl
    owner = TopKey (Rows.rowModuleName stored) name


-- Direct row analysis -----------------------------------------------------------------------------------

data SummaryPart = BodyPart | StaticInitPart | InstanceInitPart deriving Eq

data HoleOwner = HoleOwner Rows.MemberKey SummaryPart

data ConstructorContext = ConstructorContext
  { constructorGuard :: ReachSummary
  , constructorEnv   :: ReachEnv
  , constructorSelf  :: Maybe A.Name
  , constructorBody  :: A.Suite
  }

data ContainerAnalysis = ContainerAnalysis
  { analyzedResidual          :: ReachSummary
  , analyzedMembers           :: Map.Map Rows.MemberKey MemberInfo
  , analyzedConstructorHeader :: Maybe ReachSummary
  }

data Scan = Scan
  { scanResidual    :: ReachSummary
  , scanMembers     :: Map.Map Rows.MemberKey MemberInfo
  , scanConstructor :: Maybe ConstructorContext
  }

emptyMemberInfo :: MemberInfo
emptyMemberInfo = MemberInfo mempty Nothing Nothing

analyzeContainer :: ShapeKind
                 -> ReachEnv
                 -> Rows.ContainerShape
                 -> Map.Map Rows.MemberKey Rows.MemberContent
                 -> A.Suite
                 -> Rows.RowResult ContainerAnalysis
analyzeContainer kind env shape members suite = do
    let suiteOwners = collectSuiteOwners members
        constructorOwners = collectConstructorOwners members
        initial = Scan mempty
          (Map.fromSet (const emptyMemberInfo) $ logicalMembers members)
          Nothing
    scan <- analyzeSuite kind suiteOwners mempty env
      (Rows.shapeSuite shape) suite initial
    scan' <- analyzeConstructor constructorOwners (scanConstructor scan) scan
    return ContainerAnalysis
      { analyzedResidual = scanResidual scan'
      , analyzedMembers = scanMembers scan'
      , analyzedConstructorHeader =
          constructorGuard <$> scanConstructor scan'
      }
  where
    logicalMembers = Set.fromList . map logicalMember . Map.keys
    logicalMember (Rows.StaticInit name)   = Rows.Attr name
    logicalMember (Rows.InstanceInit name) = Rows.Attr name
    logicalMember member                   = member

collectSuiteOwners :: Map.Map Rows.MemberKey Rows.MemberContent
                   -> Map.Map Int [HoleOwner]
collectSuiteOwners members = Map.fromListWith (++) $
    concatMap inMember (Map.toList members)
  where
    inMember (key,Rows.AttrContent declarations) =
      [ (hole,[HoleOwner key BodyPart])
      | Rows.SuiteFragment hole _ <- declarations
      ]
    inMember (Rows.StaticInit name,Rows.InitializerContent initializers) =
      [ (hole,[HoleOwner (Rows.Attr name) StaticInitPart])
      | Rows.SuiteFragment hole _ <- initializers
      ]
    inMember (Rows.InstanceInit name,Rows.InitializerContent initializers) =
      [ (hole,[HoleOwner (Rows.Attr name) InstanceInitPart])
      | Rows.SuiteFragment hole _ <- initializers
      ]
    inMember (key,Rows.InitRestContent _ initializers) =
      [ (hole,[HoleOwner key BodyPart])
      | Rows.SuiteFragment hole _ <- initializers
      ]
    inMember _ = []

collectConstructorOwners :: Map.Map Rows.MemberKey Rows.MemberContent
                         -> Map.Map Int [HoleOwner]
collectConstructorOwners members = Map.fromListWith (++) $
    concatMap inMember (Map.toList members)
  where
    inMember (Rows.InstanceInit name,Rows.InitializerContent initializers) =
      [ (index,[HoleOwner (Rows.Attr name) InstanceInitPart])
      | Rows.ConstructorFragment index _ <- initializers
      ]
    inMember (key,Rows.InitRestContent _ initializers) =
      [ (index,[HoleOwner key BodyPart])
      | Rows.ConstructorFragment index _ <- initializers
      ]
    inMember _ = []

analyzeSuite :: ShapeKind
             -> Map.Map Int [HoleOwner]
             -> ReachSummary
             -> ReachEnv
             -> Rows.SuiteShape
             -> A.Suite
             -> Scan
             -> Rows.RowResult Scan
analyzeSuite kind owners guard env (Rows.SuiteShape shape) suite scan0
  | length shape /= length suite = Rows.rowError "container suite does not match row shape"
  | otherwise = go env scan0 (zip shape suite)
  where
    go _ scan [] = return scan
    go stmtEnv scan ((stored,stmt):rest) = do
      scan' <- analyzeShapeStmt kind owners guard stmtEnv stored stmt scan
      go (advanceReachEnv (QuickType.envOf stmt) stmtEnv) scan' rest

analyzeShapeStmt :: ShapeKind
                 -> Map.Map Int [HoleOwner]
                 -> ReachSummary
                 -> ReachEnv
                 -> Rows.ShapeStmt
                 -> A.Stmt
                 -> Scan
                 -> Rows.RowResult Scan
analyzeShapeStmt _ _ guard env Rows.InlineStmt{} stmt scan =
    return scan { scanResidual = scanResidual scan <> guard <> summarizeStmt env stmt }
analyzeShapeStmt kind owners guard env (Rows.HoleStmt hole) stmt scan = do
    owned <- required ("missing suite fragment owner " ++ show hole) (Map.lookup hole owners)
    let summary = guard <> summarizeStmt env stmt
        part = if kind == ClassShape then StaticInitPart else InstanceInitPart
        use (HoleOwner key BodyPart) = addMemberSummary key BodyPart summary
        use (HoleOwner key _) = addMemberSummary key part summary
        members' = foldl' (flip use) (scanMembers scan) owned
    return scan { scanMembers = members' }
analyzeShapeStmt _ _ guard env (Rows.DeclStmt _ stored) (A.Decl _ decls) scan
  | length stored /= length decls = Rows.rowError "container declarations do not match row shape"
  | otherwise = foldM (analyzeDecl declEnv) scan (zip stored decls)
  where
    declEnv = advanceReachEnv (QuickType.envOf decls) env
    analyzeDecl declEnv' acc (Rows.InlineDecl _,decl) =
      return acc
        { scanResidual = scanResidual acc <> guard <> summarizeDecl declEnv' decl }
    analyzeDecl declEnv' acc (Rows.MethodDecl slot,decl)
      | Rows.slotName slot /= A.dname decl =
          Rows.rowError ("method row/name mismatch for " ++ A.rawstr (Rows.slotName slot))
      | otherwise = do
          let (header,bodyEnv) = summarizeDeclHeader declEnv' decl
              (stubHeader,_) = summarizeDeclHeader
                declEnv'{ reachReflectiveOwner = False }
                (Rows.methodHeader decl)
              acc' = acc
                { scanResidual = scanResidual acc <> guard <> stubHeader }
          if Rows.slotIsConstructor slot
            then case scanConstructor acc' of
              Nothing -> return acc'
                { scanConstructor = Just
                    (ConstructorContext (guard <> header) bodyEnv (A.selfPar decl) (A.dbody decl)) }
              Just _ -> Rows.rowError "multiple constructor slots"
            else return acc'
              { scanMembers = addMemberSummary
                  (Rows.Method $ Rows.slotName slot)
                  BodyPart
                  (guard <> header <> summarizeSuite bodyEnv (A.dbody decl))
                  (scanMembers acc')
              }
analyzeShapeStmt kind owners guard env (Rows.IfStmt _ storedBranches elseShape)
                 (A.If _ branches elseSuite) scan
  | length storedBranches /= length branches = Rows.rowError "container branches do not match row shape"
  | otherwise = do
    let conditions = [ summarizeCondition env condition | A.Branch condition _ <- branches ]
        prefixes = tail (scanl (<>) mempty conditions)
    scan1 <- foldM analyzeBranch scan (zip3 storedBranches branches prefixes)
    analyzeSuite kind owners (guard <> foldMap id conditions) env
      elseShape elseSuite scan1
  where
    analyzeBranch acc ((_,storedBody),A.Branch _ body,prefix) =
      analyzeSuite kind owners (guard <> prefix) env storedBody body acc
analyzeShapeStmt _ _ _ _ _ _ _ = Rows.rowError "container statement does not match row shape"

addMemberSummary :: Rows.MemberKey
                 -> SummaryPart
                 -> ReachSummary
                 -> Map.Map Rows.MemberKey MemberInfo
                 -> Map.Map Rows.MemberKey MemberInfo
addMemberSummary key part summary = Map.alter (Just . add . maybe emptyMemberInfo id) key
  where
    add info = case part of
      BodyPart -> info { memberSummary = memberSummary info <> summary }
      StaticInitPart -> info
        { memberStaticInitSummary = appendSummary summary (memberStaticInitSummary info) }
      InstanceInitPart -> info
        { memberInstanceInitSummary = appendSummary summary (memberInstanceInitSummary info) }

appendSummary :: ReachSummary -> Maybe ReachSummary -> Maybe ReachSummary
appendSummary summary Nothing = Just summary
appendSummary summary (Just old) = Just (old <> summary)

analyzeConstructor :: Map.Map Int [HoleOwner]
                   -> Maybe ConstructorContext
                   -> Scan
                   -> Rows.RowResult Scan
analyzeConstructor owners context scan
  | Map.null owners = return scan
  | otherwise = do
      constructor <- required "constructor fragments have no constructor slot" context
      snd <$> foldM (analyzeOne constructor)
        (constructorEnv constructor,scan)
        (zip [0..] $ constructorBody constructor)
  where
    analyzeOne constructor (env,acc) (index,stmt) = do
      owned <- required ("missing constructor fragment owner " ++ show index)
        (Map.lookup index owners)
      let guard = constructorGuard constructor
          raw = guard <> summarizeStmt env stmt
          add members (HoleOwner key BodyPart) =
            addMemberSummary key BodyPart raw members
          add members (HoleOwner key InstanceInitPart) =
            addMemberSummary key InstanceInitPart
              (projectedSummary constructor env stmt key) members
          add members (HoleOwner _ StaticInitPart) = members
          members' = foldl' add (scanMembers acc) owned
      return
        ( advanceReachEnv (QuickType.envOf stmt) env
        , acc { scanMembers = members' }
        )

    projectedSummary constructor env stmt (Rows.Attr name) =
      case constructorSelf constructor >>= project of
        Nothing -> mempty
        Just projected -> constructorGuard constructor <> summarizeStmt env projected
      where
        project self = Rows.pruneConstructorInit self (Set.singleton name) stmt
    projectedSummary _ _ _ _ = mempty


-- Member rows -------------------------------------------------------------------------------------------

prepareMemberRows :: TopKey
                  -> ContainerPrepared
                  -> ReachabilityRows
                  -> Rows.RowResult ReachabilityRows
prepareMemberRows owner prepared rows =
    foldM add rows (Map.toAscList $ preparedMemberInfo prepared)
  where
    add acc (member,info) = insertMemberInfo owner member info acc

-- Shape, slot, and reflection rows ----------------------------------------------------------------------

prepareContainerMetadata :: Env.Env0
                         -> Rows.InterfaceRows
                         -> Map.Map TopKey [ExtensionInfo]
                         -> ReachabilityRows
                         -> (TopKey,ContainerPrepared)
                         -> Rows.RowResult ReachabilityRows
prepareContainerMetadata env stored extensions rows (owner,prepared) = do
    rows1 <- prepareMemberRows owner prepared rows
    let targetExtensions = Map.findWithDefault [] owner extensions
    slots <- effectiveSlots env (topModule owner) stored targetExtensions owner
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
    rows2 <- insertTopSummary Nothing (inheritedValueObligations env owner) rows1 owner
    rows3 <- insertShapeInfo owner shape rows2
    rows4 <- foldM (insertSlotInfo owner) rows3 (Map.toAscList slots)
    insertReflectable owner (ReflectableAttrs reflectable) rows4

reflectableProperty :: ShapeKind
                    -> Rows.InterfaceRows
                    -> TopKey
                    -> A.Name
                    -> Bool
reflectableProperty kind stored owner name = case content of
    Just (Rows.AttrContent declarations) ->
        not (null declarations) ||
        kind == ActorShape ||
        any constructorInitializer initializers
    _ -> False
  where
    content = Map.lookup (topName owner) (Rows.rowMembers stored) >>=
      Map.lookup (Rows.Attr name)
    initializers = case Map.lookup (topName owner) (Rows.rowMembers stored) >>=
                                Map.lookup (Rows.InstanceInit name) of
      Just (Rows.InitializerContent fragments) -> fragments
      _                                        -> []
    constructorInitializer Rows.ConstructorFragment{} = True
    constructorInitializer Rows.SuiteFragment{}       = False


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
               -> A.ModName
               -> Rows.InterfaceRows
               -> [ExtensionInfo]
               -> TopKey
               -> Rows.RowResult (Map.Map MemberRef SlotInfo)
effectiveSlots env mn stored extensions owner = do
    direct <- directSlots env owner
    let viaExtensions = foldl' addExtension Map.empty extensions
        physical = directPhysicalSlots mn stored owner
        concrete = addConcrete viaExtensions $ addConcrete physical direct
    return concrete
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
    physical (Rows.Attr n)         = Just (AttrRef n,physicalAttr)
    physical Rows.StaticInit{}     = Nothing
    physical Rows.InstanceInit{}   = Nothing
    physical Rows.InitRest         = Nothing

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
    header = maybe mempty id (preparedConstructorHeader prepared)
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

-- | On-demand access to persisted reachability rows.  'selectProgram'
-- memoizes every exact-key result for the duration of one run.
data ReachLookup m = ReachLookup
  { lookupTopRow           :: TopKey -> m (Maybe TopInfo)
  , lookupMemberRow        :: TopKey -> Rows.MemberKey -> m (Maybe MemberInfo)
  , lookupShapeRow         :: TopKey -> m (Maybe ShapeInfo)
  , lookupSlotRow          :: TopKey -> MemberRef -> m (Maybe SlotInfo)
  , lookupSurfaceSlots     :: TopKey -> m [(MemberRef, SlotInfo)]
  , lookupReflectableAttrs :: TopKey -> m (Maybe ReflectableAttrs)
  }

data SelectedRow
  = TopRow TopKey
  | OpaqueTopRow TopKey
  | MemberRow TopKey Rows.MemberKey
  | AttrRow TopKey A.Name
  | StaticInitRow TopKey A.Name
  | InstanceInitRow TopKey A.Name
  | GeneratedRow TopKey MemberRef
  deriving (Eq,Ord,Show)

-- The result is a set of exact persisted or generated rows, plus the two
-- runtime facts needed while closing dispatch and initialization.
data Selection = Selection
  { selectedDeclarations :: Set.Set TopKey
  , selectedRows         :: Set.Set SelectedRow
  , selectedConstructed  :: Set.Set TopKey
  , selectedInitialized  :: Set.Set TopKey
  } deriving (Eq,Show)

emptySelection :: Selection
emptySelection = Selection Set.empty Set.empty Set.empty Set.empty

selectedTops :: Selection -> Set.Set TopKey
selectedTops = selectKeys top . selectedRows
  where
    top (TopRow key) = Just key
    top _            = Nothing

selectedOpaqueTops :: Selection -> Set.Set TopKey
selectedOpaqueTops = selectKeys top . selectedRows
  where
    top (OpaqueTopRow key) = Just key
    top _                  = Nothing

selectedMembers :: Selection -> Set.Set (TopKey,Rows.MemberKey)
selectedMembers = selectKeys member . selectedRows
  where
    member (MemberRow owner name) = Just (owner,name)
    member _                      = Nothing

selectedAttrs :: Selection -> Set.Set (TopKey,A.Name)
selectedAttrs = selectKeys attr . selectedRows
  where
    attr (AttrRow owner name) = Just (owner,name)
    attr _                    = Nothing

selectedStaticInitializers :: Selection -> Set.Set (TopKey,A.Name)
selectedStaticInitializers = selectKeys initializer . selectedRows
  where
    initializer (StaticInitRow owner name) = Just (owner,name)
    initializer _                          = Nothing

selectedInstanceInitializers :: Selection -> Set.Set (TopKey,A.Name)
selectedInstanceInitializers = selectKeys initializer . selectedRows
  where
    initializer (InstanceInitRow owner name) = Just (owner,name)
    initializer _                            = Nothing

selectedGenerated :: Selection -> Set.Set (TopKey,MemberRef)
selectedGenerated = selectKeys generated . selectedRows
  where
    generated (GeneratedRow owner member) = Just (owner,member)
    generated _                           = Nothing

selectKeys :: Ord a => (SelectedRow -> Maybe a) -> Set.Set SelectedRow -> Set.Set a
selectKeys project = Set.fromList . mapMaybe project . Set.toAscList

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

type SelectM m = ExceptT SelectionError (StateT LookupCache m)

data LookupCache = LookupCache
  { cachedTops        :: Map.Map TopKey (Maybe TopInfo)
  , cachedMembers     :: Map.Map (TopKey,Rows.MemberKey) (Maybe MemberInfo)
  , cachedShapes      :: Map.Map TopKey (Maybe ShapeInfo)
  , cachedSlots       :: Map.Map (TopKey,MemberRef) (Maybe SlotInfo)
  , cachedSurfaces    :: Map.Map TopKey [(MemberRef,SlotInfo)]
  , cachedReflections :: Map.Map TopKey (Maybe ReflectableAttrs)
  }

emptyLookupCache :: LookupCache
emptyLookupCache = LookupCache Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty

cachedLookup :: (Monad m, Ord key)
             => (LookupCache -> Map.Map key value)
             -> (Map.Map key value -> LookupCache -> LookupCache)
             -> (key -> m value)
             -> key
             -> SelectM m value
cachedLookup field replace readValue key = do
    cache <- lift get
    case Map.lookup key (field cache) of
      Just value -> return value
      Nothing -> do
        value <- lift (lift $ readValue key)
        lift $ modify' (replace $ Map.insert key value (field cache))
        return value

loadTopRow :: Monad m => ReachLookup m -> TopKey -> SelectM m (Maybe TopInfo)
loadTopRow lookups = cachedLookup cachedTops set (lookupTopRow lookups)
  where set rows cache = cache{cachedTops=rows}

loadMemberRow :: Monad m
              => ReachLookup m
              -> TopKey
              -> Rows.MemberKey
              -> SelectM m (Maybe MemberInfo)
loadMemberRow lookups owner member = cachedLookup cachedMembers set readOne (owner,member)
  where
    set rows cache = cache{cachedMembers=rows}
    readOne (key,row) = lookupMemberRow lookups key row

loadShapeRow :: Monad m => ReachLookup m -> TopKey -> SelectM m (Maybe ShapeInfo)
loadShapeRow lookups = cachedLookup cachedShapes set (lookupShapeRow lookups)
  where set rows cache = cache{cachedShapes=rows}

loadSlotRow :: Monad m
            => ReachLookup m
            -> TopKey
            -> MemberRef
            -> SelectM m (Maybe SlotInfo)
loadSlotRow lookups owner member = cachedLookup cachedSlots set readOne (owner,member)
  where
    set rows cache = cache{cachedSlots=rows}
    readOne (key,row) = lookupSlotRow lookups key row

loadSurfaceSlots :: Monad m
                 => ReachLookup m
                 -> TopKey
                 -> SelectM m [(MemberRef,SlotInfo)]
loadSurfaceSlots lookups = cachedLookup cachedSurfaces set (lookupSurfaceSlots lookups)
  where set rows cache = cache{cachedSurfaces=rows}

loadReflectableAttrs :: Monad m
                     => ReachLookup m
                     -> TopKey
                     -> SelectM m (Maybe ReflectableAttrs)
loadReflectableAttrs lookups = cachedLookup cachedReflections set (lookupReflectableAttrs lookups)
  where set rows cache = cache{cachedReflections=rows}

data WorkItem
  = ReachWork ReachEdge
  | InitializeWork TopKey
  | ReflectWork TopKey TopKey
  deriving (Eq, Ord, Show)

data Work = Work
  { workQueue           :: Seq.Seq WorkItem
  , workNeeded          :: Set.Set TopKey
  , workDeclarations    :: Set.Set TopKey
  , workRows            :: Set.Set SelectedRow
  , workConstructed     :: Set.Set TopKey
  , workInitialized     :: Set.Set TopKey
  , workDispatches      :: Set.Set (TopKey,MemberRef)
  , workReflections     :: Set.Set TopKey
  , workDispatchPairs   :: Set.Set (TopKey,MemberRef,TopKey)
  , workReflectionPairs :: Set.Set (TopKey,TopKey)
  }

emptyWork :: [ReachEdge] -> Work
emptyWork seeds = Work
  { workQueue = Seq.fromList (map ReachWork seeds)
  , workNeeded = Set.empty
  , workDeclarations = Set.empty
  , workRows = Set.empty
  , workConstructed = Set.empty
  , workInitialized = Set.empty
  , workDispatches = Set.empty
  , workReflections = Set.empty
  , workDispatchPairs = Set.empty
  , workReflectionPairs = Set.empty
  }

finish :: Work -> Selection
finish work = Selection
  { selectedDeclarations = workDeclarations work
  , selectedRows = workRows work
  , selectedConstructed = workConstructed work
  , selectedInitialized = workInitialized work
  }

hasRow :: SelectedRow -> Work -> Bool
hasRow row = Set.member row . workRows

keepRow :: SelectedRow -> Work -> Work
keepRow row work = work { workRows = Set.insert row (workRows work) }

selectedWorkAttrs :: Work -> [(TopKey,A.Name)]
selectedWorkAttrs work =
    [ (owner,name)
    | AttrRow owner name <- Set.toAscList (workRows work)
    ]

selectProgram :: Monad m => ReachLookup m -> [ReachEdge] -> m (Either SelectionError Selection)
selectProgram lookups seeds = fmap fst $
  runStateT (runExceptT $ finish <$> drain lookups (emptyWork seeds)) emptyLookupCache

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
        OpaqueTop summary ->
          return (enqueueSummary summary $ keepRow (OpaqueTopRow key) work0)
        LocalTop header summary -> do
          let work1 = enqueueSummary summary (keepRow (TopRow key) work0)
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
  | hasRow row work = return work
  | otherwise = do
      work0 <- selectTop lookups owner work
      info <- lookupMemberInfo lookups owner member
      return (enqueueSummary (memberSummary info) $ keepRow row work0)
  where row = MemberRow owner member

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
    (_, OpaqueSlot) -> return work0

validateStoredSlot :: TopKey -> MemberRef -> Rows.MemberKey -> Either SelectionError ()
validateStoredSlot owner (MethodRef n) member@(Rows.Method n')
  | n == n' = Right ()
  | otherwise = Left (InvalidStoredSlot owner (MethodRef n) member)
validateStoredSlot owner ref member = Left (InvalidStoredSlot owner ref member)

selectGenerated :: Monad m => TopKey -> MemberRef -> ReachSummary -> Work -> SelectM m Work
selectGenerated owner ref summary work
  | hasRow row work = return work
  | otherwise = return (enqueueSummary summary $ keepRow row work)
  where row = GeneratedRow owner ref


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
      foldM (selectInitForField lookups receiver) work2 (selectedWorkAttrs work2)

selectAttr :: Monad m => ReachLookup m -> TopKey -> A.Name -> Work -> SelectM m Work
selectAttr lookups owner attr work
  | hasRow row work = return work
  | otherwise = do
      work0 <- selectTop lookups owner work
      info <- lookupMemberInfo lookups owner (Rows.Attr attr)
      work1 <- selectMember lookups owner (Rows.Attr attr) work0
      let work2 = keepRow row work1
      work3 <- activateStaticInitializer lookups owner attr info work2
      foldM selectInit work3
        [ (initOwner, field) | initOwner <- Set.toAscList (workInitialized work2) ]
  where field = (owner, attr)
        row = AttrRow owner attr
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
      | hasRow row work -> return work
      | otherwise -> do
          work0 <- selectMember lookups owner (Rows.Attr attr) work
          return (enqueueSummary summary $ keepRow row work0)
  where row = StaticInitRow owner attr

activateInstanceInitializer :: Monad m => ReachLookup m -> TopKey -> A.Name -> MemberInfo -> Work -> SelectM m Work
activateInstanceInitializer lookups owner attr info work =
  case memberInstanceInitSummary info of
    Nothing -> return work
    Just summary
      | hasRow row work -> return work
      | otherwise -> do
          work0 <- selectMember lookups owner (Rows.Attr attr) work
          return (enqueueSummary summary $ keepRow row work0)
  where row = InstanceInitRow owner attr


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
