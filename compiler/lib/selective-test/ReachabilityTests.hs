-- SPDX-License-Identifier: BSD-3-Clause

{-# LANGUAGE ScopedTypeVariables #-}
module ReachabilityTests (tests) where

import qualified Acton.Builtin as Builtin
import qualified Acton.Converter as Converter
import qualified Acton.Env as Env
import qualified Acton.NameInfo as I
import qualified Acton.Names as Names
import qualified Acton.QuickType as QuickType
import qualified Acton.Reachability as Reach
import qualified Acton.Syntax as A

import Control.DeepSeq (force)
import qualified Control.Exception as E
import Data.List (isInfixOf)
import qualified Data.Set as Set
import Test.Syd
import Utils (SrcLoc(..))


tests :: Env.Env0 -> Spec
tests builtinEnv =
    let base = ordinaryEnv builtinEnv
        mn = A.modName ["reachability"]
        a = A.name "A"
        b = A.name "B"
        actor = A.name "Worker"
        witness = A.name "AWitness"
        function = A.name "plain"
        method = A.name "run"
        staticMethod = A.name "make"
        field = A.name "value"
        state = A.name "state"
        classInfo = I.NClass [] []
        methodInfo = I.NDef (A.monotype $ A.tFun A.fxPure A.posNil A.kwdNil A.tNone) A.NoDec Nothing
        staticInfo = I.NDef (A.monotype $ A.tFun A.fxPure A.posNil A.kwdNil A.tNone) A.Static Nothing
        attrInfo = I.NSig (A.monotype A.tWild) A.Property Nothing
        env = Env.addActiveNames
                [ (a, classInfo [(method,methodInfo),(staticMethod,staticInfo),(field,attrInfo),
                                 (Builtin.boolKW,methodInfo)] Nothing)
                , (b, classInfo [(method,methodInfo),(Builtin.nextKW,methodInfo)] Nothing)
                , (actor, I.NAct [] A.posNil A.kwdNil [(method,methodInfo)] Nothing)
                , (witness, I.NExt [] (A.TC (A.NoQ a) []) [] [(method,methodInfo)] [] Nothing)
                , (function, methodInfo)
                ] (Env.setMod mn base)
        globals = Set.fromList [a,b,actor,witness,function]
        local n t e = (Reach.localReachEnv (Env.define [(n,I.NVar t)] env) globals)
                        { Reach.reachLocals = Set.singleton n }
                        `Reach.summarizeExpr` e
        dot n x = A.Dot NoLoc (A.Var NoLoc $ A.NoQ x) n
        typeOf n = A.tCon $ A.TC (A.NoQ n) []
        edgeSet = Set.fromList . Reach.reachEdges

    in do
      describe "typed reachability" $ do
        it "distinguishes receivers even when every expression has NoLoc" $ do
          let x = A.name "x"
              y = A.name "y"
              tenv = Env.define [(x,I.NVar $ typeOf a),(y,I.NVar $ typeOf b)] env
              renv = (Reach.localReachEnv tenv globals)
                        { Reach.reachLocals = Set.fromList [x,y] }
              summary = Reach.summarizeExpr renv (A.Tuple NoLoc
                          (A.PosArg (dot method x) $ A.PosArg (dot method y) A.PosNil) A.KwdNil)
          edgeSet summary `shouldBe` Set.fromList
            [ Reach.Dispatch mn a (Reach.MethodRef method)
            , Reach.Dispatch mn b (Reach.MethodRef method)
            ]

        it "keeps direct class selection distinct from instance dispatch" $ do
          let x = A.name "x"
              direct = Reach.summarizeExpr (Reach.localReachEnv env globals) (dot method a)
              dispatch = local x (typeOf a) (dot method x)
          edgeSet direct `shouldBe` Set.fromList
            [ Reach.Need mn a
            , Reach.Direct mn a (Reach.MethodRef method)
            ]
          edgeSet dispatch `shouldBe` Set.singleton
            (Reach.Dispatch mn a $ Reach.MethodRef method)

        it "keeps direct static class member access independent of construction" $ do
          let summary = Reach.summarizeExpr
                (Reach.localReachEnv env globals) (dot staticMethod a)
          edgeSet summary `shouldBe` Set.fromList
            [ Reach.Need mn a
            , Reach.Direct mn a (Reach.MethodRef staticMethod)
            ]
          Set.member (Reach.Construct mn a) (edgeSet summary) `shouldBe` False

        it "records class, actor, and witness construction without treating ordinary calls as constructors" $ do
          let call n = A.Call NoLoc (A.Var NoLoc $ A.NoQ n) A.PosNil A.KwdNil
              classCall = Reach.summarizeExpr (Reach.localReachEnv env globals) (call a)
              actorCall = Reach.summarizeExpr (Reach.localReachEnv env globals) (call actor)
              witnessCall = Reach.summarizeExpr (Reach.localReachEnv env globals) (call witness)
              functionCall = Reach.summarizeExpr (Reach.localReachEnv env globals) (call function)
          edgeSet classCall `shouldBe` Set.fromList [Reach.Need mn a, Reach.Construct mn a]
          edgeSet actorCall `shouldBe` Set.fromList [Reach.Need mn actor, Reach.Construct mn actor]
          edgeSet witnessCall `shouldBe` Set.fromList [Reach.Need mn witness, Reach.Construct mn witness]
          edgeSet functionCall `shouldBe` Set.singleton (Reach.Need mn function)

        it "keeps primitive constructor helpers opaque" $ do
          let primModule = A.modName ["$"]
              eqOpt = A.name "EqOpt"
              call = A.Call NoLoc
                (A.TApp NoLoc (A.Var NoLoc $ A.GName primModule eqOpt) [A.tWild])
                A.PosNil A.KwdNil
          edgeSet (Reach.summarizeExpr (Reach.localReachEnv env globals) call)
            `shouldBe` Set.singleton
              (Reach.Need primModule eqOpt)

        it "records construction when a class or actor constructor is called through a local value" $ do
          let maker = A.name "maker"
              callMaker n fx =
                [ A.Assign NoLoc
                    [A.PVar NoLoc maker $ Just $ A.tFun fx A.posNil A.kwdNil $ typeOf n]
                    (A.Var NoLoc $ A.NoQ n)
                , A.Expr NoLoc $ A.Call NoLoc
                    (A.Var NoLoc $ A.NoQ maker) A.PosNil A.KwdNil
                ]
              classCall = Reach.summarizeSuite
                (Reach.localReachEnv env globals) (callMaker a A.fxPure)
              actorCall = Reach.summarizeSuite
                (Reach.localReachEnv env globals) (callMaker actor A.fxProc)
          edgeSet classCall `shouldBe` Set.fromList
            [Reach.Declare mn a, Reach.Need mn a, Reach.Construct mn a]
          edgeSet actorCall `shouldBe` Set.fromList
            [Reach.Declare mn actor, Reach.Need mn actor, Reach.Construct mn actor]

        it "marks direct and locally aliased serialization functions as dynamic" $ do
          let alias = A.name "codec"
              direct qn = Reach.summarizeExpr
                (Reach.localReachEnv env globals) (A.Var NoLoc qn)
              aliased qn = Reach.summarizeSuite (Reach.localReachEnv env globals)
                [ A.Assign NoLoc [A.PVar NoLoc alias $ Just A.tWild]
                    (A.Var NoLoc qn)
                , A.Expr NoLoc $ A.Var NoLoc $ A.NoQ alias
                ]
              expected n = Set.fromList
                [ Reach.Need Builtin.mBuiltin n
                , Reach.DynamicSerialization
                ]
          edgeSet (direct Builtin.qnSerialize) `shouldBe` expected Builtin.nSerialize
          edgeSet (direct Builtin.qnDeserialize) `shouldBe` expected Builtin.nDeserialize
          edgeSet (aliased Builtin.qnSerialize) `shouldBe` expected Builtin.nSerialize
          edgeSet (aliased Builtin.qnDeserialize) `shouldBe` expected Builtin.nDeserialize

        it "dispatches directly from a generated witness constructor call" $ do
          let receiver = A.Call NoLoc (A.Var NoLoc $ A.NoQ witness) A.PosNil A.KwdNil
              expr = A.Dot NoLoc receiver method
          edgeSet (Reach.summarizeExpr (Reach.localReachEnv env globals) expr)
            `shouldBe` Set.fromList
              [ Reach.Need mn witness
              , Reach.Construct mn witness
              , Reach.Dispatch mn witness (Reach.MethodRef method)
              ]

        it "keeps the knot-tied current witness local to its extension" $ do
          let decl = A.Extension NoLoc [] (A.TC (A.NoQ a) []) []
                [A.Expr NoLoc $ A.Var NoLoc $ A.NoQ Names.selfKW'] Nothing
          edgeSet (Reach.summarizeDecl (Reach.topReachEnv env globals) decl)
            `shouldBe` Set.singleton (Reach.Inherit mn a)

        it "records types referenced by aliases" $ do
          let alias = A.name "Alias"
              decl = A.Typedef NoLoc alias [] (typeOf a) Nothing
          edgeSet (Reach.summarizeDecl (Reach.topReachEnv env globals) decl)
            `shouldBe` Set.singleton (Reach.Declare mn a)

        it "keeps aliases whose names appear in types" $ do
          let alias = A.name "Alias"
              aliasEnv = Env.define [(alias,I.NType [] (typeOf a) Nothing)] env
              aliasGlobals = Set.insert alias globals
          edgeSet (Reach.summarizeType
              (Reach.topReachEnv aliasEnv aliasGlobals) (typeOf alias))
            `shouldBe` Set.singleton (Reach.Need mn alias)

        it "expands aliases before selecting receiver members" $ do
          let alias = A.name "Alias"
              x = A.name "x"
              aliasEnv = Env.define
                [ (alias,I.NType [] (typeOf a) Nothing)
                , (x,I.NVar $ typeOf alias)
                ] env
              renv = (Reach.localReachEnv aliasEnv $ Set.insert alias globals)
                { Reach.reachLocals = Set.singleton x }
          edgeSet (Reach.summarizeExpr renv $ dot method x)
            `shouldBe` Set.singleton
              (Reach.Dispatch mn a $ Reach.MethodRef method)

        it "recovers the result type of a runtime protocol witness call" $ do
          let runtimeWitness = A.Internal A.Witness "indexed" 0
              tupleType = A.tTupleK $
                A.kwdRow (A.name "type") Builtin.tStr $
                A.kwdRow (A.name "value") Builtin.tValue A.kwdNil
              optionsType = Builtin.tDict Builtin.tStr tupleType
              witnessType = A.tCon $ A.TC Builtin.qnIndexed
                [optionsType,Builtin.tStr,tupleType]
              tenv = Env.define [(runtimeWitness,I.NVar witnessType)] env
              call = A.Call NoLoc
                (A.Dot NoLoc (A.Var NoLoc $ A.NoQ runtimeWitness) Builtin.getitemKW)
                (A.PosArg (A.None NoLoc) $ A.PosArg (A.None NoLoc) A.PosNil)
                A.KwdNil
          QuickType.typeOf (Converter.convEnvProtos tenv) call `shouldBe` tupleType

        it "recovers a runtime witness result through a property field" $ do
          let holder = A.name "Holder"
              fieldName = A.name "indexed"
              localName = A.name "holder"
              tupleType = A.tTupleK $
                A.kwdRow (A.name "type") Builtin.tStr $
                A.kwdRow (A.name "value") Builtin.tValue A.kwdNil
              optionsType = Builtin.tDict Builtin.tStr tupleType
              witnessType = A.tCon $ A.TC Builtin.qnIndexed
                [optionsType,Builtin.tStr,tupleType]
              holderInfo = I.NClass [] [] [(fieldName,I.NVar witnessType)] Nothing
              holderType = A.tCon $ A.TC (A.NoQ holder) []
              tenv = Converter.convEnvProtos $ Env.define
                [ (holder,holderInfo)
                , (localName,I.NVar holderType)
                ] env
              method = A.Dot NoLoc
                (A.Dot NoLoc (A.Var NoLoc $ A.NoQ localName) fieldName)
                Builtin.getitemKW
              call = A.Call NoLoc method
                (A.PosArg (A.None NoLoc) $ A.PosArg (A.None NoLoc) A.PosNil)
                A.KwdNil
          QuickType.typeOf tenv call `shouldBe` tupleType

        it "ignores the synthetic protocol initializer member call" $ do
          let protocol = A.name "Protocol"
              protocolEnv = Env.define
                [(protocol,I.NProto [] [] [] Nothing)] env
              protocolGlobals = Set.insert protocol globals
              call = A.Call NoLoc
                (A.Dot NoLoc (A.Var NoLoc $ A.NoQ protocol) Builtin.initKW)
                A.PosNil A.KwdNil
          edgeSet (Reach.summarizeExpr
              (Reach.localReachEnv protocolEnv protocolGlobals) call)
            `shouldBe` Set.singleton (Reach.Need mn protocol)

        it "lets a typed local shadow a class name without a location side table" $ do
          let shadowed = (Reach.localReachEnv (Env.define [(a,I.NVar $ typeOf b)] env) globals)
                           { Reach.reachLocals = Set.singleton a }
              summary = Reach.summarizeExpr shadowed (dot method a)
          edgeSet summary `shouldBe` Set.singleton
            (Reach.Dispatch mn b $ Reach.MethodRef method)

        it "classifies stored attributes separately from methods" $ do
          let x = A.name "x"
              summary = local x (typeOf a) (dot field x)
          edgeSet summary `shouldBe` Set.singleton
            (Reach.Dispatch mn a $ Reach.AttrRef field)

        it "recognizes generated witness fields without an interface member" $ do
          let x = A.name "x"
              witnessField = A.Internal A.Witness "Indexed" 0
          edgeSet (local x (typeOf a) $ dot witnessField x)
            `shouldBe` Set.singleton
              (Reach.Dispatch mn a $ Reach.AttrRef witnessField)

        it "selects class-scope compiler witnesses through their exact owner" $ do
          let compilerWitness = A.Internal A.Witness "Field" 0
              body =
                [ A.Assign NoLoc
                    [A.PVar NoLoc compilerWitness $ Just $ typeOf a]
                    (A.None NoLoc)
                , A.Expr NoLoc $ dot method compilerWitness
                ]
              decl = A.Class NoLoc b [] [] body Nothing
          edgeSet (Reach.summarizeDecl (Reach.topReachEnv env globals) decl)
            `shouldBe` Set.fromList
              [ Reach.Declare mn a
              , Reach.Direct mn b (Reach.AttrRef compilerWitness)
              , Reach.Dispatch mn a (Reach.MethodRef method)
              ]

        it "keeps constructor witness parameters in class equation scope" $ do
          let initWitness = A.Internal A.Witness "Dependency" 0
              compilerWitness = A.Internal A.Witness "Computed" 0
              constructor = A.Def NoLoc Builtin.initKW []
                (A.PosPar Names.selfKW' (Just A.tSelf) Nothing $
                 A.PosPar initWitness (Just $ typeOf a) Nothing A.PosNIL)
                A.KwdNIL (Just A.tNone) [] A.NoDec A.fxPure Nothing
              body =
                [ A.Assign NoLoc
                    [A.PVar NoLoc compilerWitness $ Just $ typeOf a]
                    (dot method initWitness)
                , A.Decl NoLoc [constructor]
                ]
              decl = A.Class NoLoc b [] [] body Nothing
          edgeSet (Reach.summarizeDecl (Reach.topReachEnv env globals) decl)
            `shouldBe` Set.fromList
              [ Reach.Declare mn a
              , Reach.Dispatch mn a (Reach.MethodRef method)
              ]

        it "resolves a generic receiver through its exact class bound" $ do
          let x = A.name "x"
              tv = A.TV A.KType (A.name "T")
              tenv = Env.define [(x,I.NVar $ A.tVar tv)] $
                       Env.defineTVars [A.QBind tv [A.TC (A.NoQ a) []]] env
              renv = (Reach.localReachEnv tenv globals)
                       { Reach.reachLocals = Set.singleton x }
          edgeSet (Reach.summarizeExpr renv $ dot method x)
            `shouldBe` Set.singleton (Reach.Dispatch mn a $ Reach.MethodRef method)

        it "threads typed lexical bindings through a suite" $ do
          let x = A.name "x"
              suite =
                [ A.Assign NoLoc [A.PVar NoLoc x (Just $ typeOf a)] (A.None NoLoc)
                , A.Expr NoLoc (dot method x)
                ]
              summary = Reach.summarizeSuite (Reach.localReachEnv env globals) suite
          edgeSet summary `shouldBe` Set.fromList
            [ Reach.Declare mn a
            , Reach.Dispatch mn a (Reach.MethodRef method)
            ]

        it "declares signature types without selecting their class shells" $ do
          let arg = A.name "arg"
              f = A.name "typed"
              decl = A.Def NoLoc f []
                (A.PosPar arg (Just $ typeOf a) Nothing A.PosNIL)
                A.KwdNIL (Just $ typeOf b)
                [A.Return NoLoc $ Just $ A.None NoLoc]
                A.NoDec A.fxPure Nothing
              property = A.Signature NoLoc [field]
                (A.monotype $ typeOf b) A.Property
              summary = Reach.summarizeDecl (Reach.topReachEnv env globals) decl <>
                        Reach.summarizeStmt (Reach.topReachEnv env globals) property
          edgeSet summary `shouldBe` Set.fromList
            [Reach.Declare mn a,Reach.Declare mn b]

        it "keeps generic constraint arguments declaration-only" $ do
          let tv = A.TV A.KType (A.name "T")
              f = A.name "generic"
              decl = A.Def NoLoc f [A.QBind tv [A.TC (A.NoQ a) []]]
                A.PosNIL A.KwdNIL (Just A.tNone)
                [A.Return NoLoc $ Just $ A.None NoLoc]
                A.NoDec A.fxPure Nothing
          edgeSet (Reach.summarizeDecl (Reach.topReachEnv env globals) decl)
            `shouldBe` Set.singleton (Reach.Declare mn a)

        it "keeps bases and runtime class tests as strong dependencies" $ do
          let baseDecl = A.Class NoLoc b [] [A.TC (A.NoQ a) []] [] Nothing
              x = A.name "x"
              runtimeTest = local x (typeOf b) $
                A.IsInstance NoLoc (A.Var NoLoc $ A.NoQ x) (A.NoQ a)
              summary = Reach.summarizeDecl (Reach.topReachEnv env globals) baseDecl <>
                        runtimeTest
          edgeSet summary `shouldBe` Set.fromList
            [ Reach.Need mn a
            , Reach.Inherit mn a
            ]

        it "records Normalizer's exact truth conversion" $ do
          let x = A.name "condition"
              renv = (Reach.localReachEnv (Env.define [(x,I.NVar $ typeOf a)] env) globals)
                { Reach.reachLocals = Set.singleton x }
              stmt = A.If NoLoc
                [A.Branch (A.Var NoLoc $ A.NoQ x) [A.Pass NoLoc]] []
          edgeSet (Reach.summarizeStmt renv stmt) `shouldBe` Set.singleton
            (Reach.Dispatch mn a $ Reach.MethodRef Builtin.boolKW)
          edgeSet (Reach.summarizeStmt renv $ A.Assert NoLoc (A.Bool NoLoc True) Nothing)
            `shouldBe` Set.empty

        it "records truth conversion for optional call results" $ do
          let condition = A.name "condition"
              conditionInfo = I.NDef
                (A.monotype $ A.tFun A.fxPure A.posNil A.kwdNil $ A.tOpt $ typeOf a)
                A.NoDec Nothing
              conditionEnv = Env.define [(condition,conditionInfo)] env
              conditionGlobals = Set.insert condition globals
              call = A.Call NoLoc (A.Var NoLoc $ A.NoQ condition) A.PosNil A.KwdNil
              stmt = A.If NoLoc [A.Branch call [A.Pass NoLoc]] []
          edgeSet (Reach.summarizeStmt
              (Reach.localReachEnv conditionEnv conditionGlobals) stmt)
            `shouldBe` Set.fromList
              [ Reach.Need mn condition
              , Reach.Dispatch mn a (Reach.MethodRef Builtin.boolKW)
              ]

        it "records Normalizer's iterator next call only" $ do
          let iterator = A.name "iterator"
              item = A.name "item"
              iteratorType = A.tCon $ A.TC (A.NoQ b) [A.tWild]
              renv = (Reach.localReachEnv (Env.define [(iterator,I.NVar iteratorType)] env) globals)
                { Reach.reachLocals = Set.singleton iterator }
              stmt = A.For NoLoc
                (A.PVar NoLoc item $ Just A.tWild)
                (A.Var NoLoc $ A.NoQ iterator) [A.Pass NoLoc] []
              edges = edgeSet (Reach.summarizeStmt renv stmt)
          edges `shouldBe` Set.singleton
            (Reach.Dispatch mn b $ Reach.MethodRef Builtin.nextKW)
          Set.member (Reach.Dispatch mn b $ Reach.MethodRef Builtin.strKW) edges
            `shouldBe` False

        it "tracks an unannotated loop target inside its lexical body" $ do
          let iterator = A.name "iterator"
              item = A.name "item"
              tenv = Env.define
                [ (iterator,I.NVar $ A.tCon $ A.TC (A.NoQ b) [A.tWild])
                , (item,I.NVar $ typeOf a)
                ] env
              renv = (Reach.localReachEnv tenv globals) {
                Reach.reachLocals = Set.singleton iterator
              }
              stmt = A.For NoLoc
                (A.PVar NoLoc item Nothing)
                (A.Var NoLoc $ A.NoQ iterator)
                [A.Expr NoLoc $ dot method item] []
          edgeSet (Reach.summarizeStmt renv stmt) `shouldBe` Set.fromList
            [ Reach.Dispatch mn a (Reach.MethodRef method)
            , Reach.Dispatch mn b (Reach.MethodRef Builtin.nextKW)
            ]

        it "turns implicit class attributes and actor state into exact owner edges" $ do
          let classRenv = Reach.withReachOwner (A.NoQ a) $
                            Reach.localReachEnv (Env.define [(field,I.NVar A.tWild)] env) globals
              actorRenv = (Reach.withReachOwner (A.NoQ actor) $
                            Reach.localReachEnv (Env.define [(state,I.NSVar A.tWild)] env) globals) {
                              Reach.reachImplicitAttrs = Set.singleton state
                            }
          edgeSet (Reach.summarizeExpr classRenv $ A.Var NoLoc $ A.NoQ field)
            `shouldBe` Set.singleton (Reach.Dispatch mn a $ Reach.AttrRef field)
          edgeSet (Reach.summarizeExpr actorRenv $ A.Var NoLoc $ A.NoQ state)
            `shouldBe` Set.singleton (Reach.Dispatch mn actor $ Reach.AttrRef state)

        it "lets an implicit class attribute shadow an imported alias" $ do
          let shadowed = A.name "shadowed"
              dependency = A.name "dependency"
              depModule = A.modName ["dep"]
              aliasEnv = Env.addActiveNames
                [(shadowed,I.NAlias $ A.GName depModule dependency)] env
              ownerEnv = Env.addActiveNames
                [(a,classInfo [(shadowed,I.NVar A.tWild)] Nothing)] aliasEnv
              renv = Reach.withReachOwner (A.NoQ a) $
                Reach.localReachEnv
                  (Env.define [(shadowed,I.NVar A.tWild)] ownerEnv) globals
          edgeSet (Reach.summarizeExpr renv $ A.Var NoLoc $ A.NoQ shadowed)
            `shouldBe` Set.singleton (Reach.Dispatch mn a $ Reach.AttrRef shadowed)

        it "treats an unqualified method reference as dispatch on its owner" $ do
          let renv = Reach.withReachOwner (A.NoQ a) $ Reach.localReachEnv env globals
          edgeSet (Reach.summarizeExpr renv $ A.Var NoLoc $ A.NoQ method)
            `shouldBe` Set.singleton (Reach.Dispatch mn a $ Reach.MethodRef method)

        it "keeps an unqualified static method independent of construction" $ do
          let renv = Reach.withReachOwner (A.NoQ a) $ Reach.localReachEnv env globals
          edgeSet (Reach.summarizeExpr renv $ A.Var NoLoc $ A.NoQ staticMethod)
            `shouldBe` Set.singleton (Reach.Direct mn a $ Reach.MethodRef staticMethod)

        it "keeps a hidden actor method addressable inside its owner" $ do
          let hidden = A.name "_internal"
              caller = A.Def NoLoc method [] A.PosNIL A.KwdNIL (Just A.tNone)
                [A.Expr NoLoc $ A.Call NoLoc
                  (A.Var NoLoc $ A.NoQ hidden) A.PosNil A.KwdNil]
                A.NoDec A.fxAction Nothing
              implementation = A.Def NoLoc hidden [] A.PosNIL A.KwdNIL (Just A.tNone)
                [A.Return NoLoc $ Just $ A.None NoLoc]
                A.NoDec A.fxAction Nothing
              decl = A.Actor NoLoc actor [] A.PosNIL A.KwdNIL
                [A.Decl NoLoc [caller,implementation]] Nothing
          edgeSet (Reach.summarizeDecl (Reach.topReachEnv env globals) decl)
            `shouldBe` Set.singleton
              (Reach.Dispatch mn actor $ Reach.MethodRef hidden)

        it "uses actor lowering semantics for captured constructor parameters" $ do
          let config = A.name "config"
              bodyMethod = A.Def NoLoc method [] A.PosNIL A.KwdNIL (Just A.tNone)
                             [A.Expr NoLoc $ A.Var NoLoc $ A.NoQ config]
                             A.NoDec A.fxPure Nothing
              decl = A.Actor NoLoc actor []
                       (A.PosPar config (Just A.tWild) Nothing A.PosNIL) A.KwdNIL
                       [A.Decl NoLoc [bodyMethod]] Nothing
              summary = Reach.summarizeDecl (Reach.topReachEnv env globals) decl
          edgeSet summary `shouldBe` Set.singleton
            (Reach.Dispatch mn actor $ Reach.AttrRef config)

        it "keeps a non-captured hidden actor initializer binding local" $ do
          let hidden = A.name "_tmp"
              decl = A.Actor NoLoc actor [] A.PosNIL A.KwdNIL
                       [ A.Assign NoLoc [A.PVar NoLoc hidden $ Just A.tWild] (A.None NoLoc)
                       , A.Expr NoLoc $ A.Var NoLoc $ A.NoQ hidden
                       ] Nothing
              summary = Reach.summarizeDecl (Reach.topReachEnv env globals) decl
          edgeSet summary `shouldBe` Set.empty

        it "keeps nested actor initializer bindings local" $ do
          let iterator = A.name "iterator"
              item = A.name "item"
              tmp = A.name "tmp"
              iteratorType = A.tCon $ A.TC (A.NoQ b) [A.tWild]
              loop = A.For NoLoc
                (A.PVar NoLoc item $ Just A.tWild)
                (A.Var NoLoc $ A.NoQ iterator)
                [ A.Assign NoLoc [A.PVar NoLoc tmp $ Just $ typeOf a] (A.None NoLoc)
                , A.Expr NoLoc $ dot method tmp
                ] []
              decl = A.Actor NoLoc actor []
                (A.PosPar iterator (Just iteratorType) Nothing A.PosNIL)
                A.KwdNIL [loop] Nothing
              summary = Reach.summarizeDecl (Reach.topReachEnv env globals) decl
          edgeSet summary `shouldBe` Set.fromList
            [ Reach.Declare mn a
            , Reach.Declare mn b
            , Reach.Dispatch mn a (Reach.MethodRef method)
            , Reach.Dispatch mn b (Reach.MethodRef Builtin.nextKW)
            ]

        it "treats a visible actor initializer binding as an attribute" $ do
          let cached = A.name "cached"
              decl = A.Actor NoLoc actor [] A.PosNIL A.KwdNIL
                       [ A.Assign NoLoc [A.PVar NoLoc cached $ Just A.tWild] (A.None NoLoc)
                       , A.Expr NoLoc $ A.Var NoLoc $ A.NoQ cached
                       ] Nothing
              summary = Reach.summarizeDecl (Reach.topReachEnv env globals) decl
          edgeSet summary `shouldBe` Set.singleton
            (Reach.Dispatch mn actor $ Reach.AttrRef cached)

        it "keeps write-only actor state as an exact attribute target" $ do
          let setter = A.name "set"
              value = A.name "new_value"
              stateInit = A.VarAssign NoLoc
                [A.PVar NoLoc state $ Just Builtin.tInt]
                (A.Int NoLoc 0 "0")
              setterDecl = A.Def NoLoc setter []
                (A.PosPar value (Just Builtin.tInt) Nothing A.PosNIL)
                A.KwdNIL (Just A.tNone)
                [ A.Assign NoLoc [A.PVar NoLoc state $ Just Builtin.tInt]
                    (A.Var NoLoc $ A.NoQ value)
                ] A.NoDec A.fxAction Nothing
              decl = A.Actor NoLoc actor [] A.PosNIL A.KwdNIL
                [stateInit,A.Decl NoLoc [setterDecl]] Nothing
              summary = Reach.summarizeDecl (Reach.topReachEnv env globals) decl
          Set.member (Reach.Dispatch mn actor $ Reach.AttrRef state) (edgeSet summary)
            `shouldBe` True

        it "keeps an actor loop target as an exact state attribute" $ do
          let iterator = A.name "iterator"
              iteratorType = A.tCon $ A.TC (A.NoQ b) []
              stateInit = A.VarAssign NoLoc
                [A.PVar NoLoc state $ Just $ typeOf a]
                (A.None NoLoc)
              loop = A.For NoLoc
                (A.PVar NoLoc state $ Just $ typeOf a)
                (A.Var NoLoc $ A.NoQ iterator)
                [] []
              decl = A.Actor NoLoc actor []
                (A.PosPar iterator (Just iteratorType) Nothing A.PosNIL)
                A.KwdNIL [stateInit,loop] Nothing
              summary = Reach.summarizeDecl (Reach.topReachEnv env globals) decl
          Set.member (Reach.Dispatch mn actor $ Reach.AttrRef state) (edgeSet summary)
            `shouldBe` True

        it "records reflection only on the synthesized class member" $ do
          let self = A.name "self"
              arg = A.name "name"
              getter = A.Def NoLoc Builtin.getAttrKW []
                         (A.PosPar self (Just A.tSelf) Nothing $
                          A.PosPar arg (Just Builtin.tStr) Nothing A.PosNIL)
                         A.KwdNIL (Just $ A.tOpt Builtin.tValue) [A.Return NoLoc $ Just $ A.None NoLoc]
                         A.NoDec A.fxPure Nothing
              decl = A.Class NoLoc a [] [] [A.Decl NoLoc [getter]] Nothing
              summary = Reach.summarizeDecl (Reach.topReachEnv env globals) decl
          Set.member (Reach.Reflect mn a) (edgeSet summary) `shouldBe` True

        it "fails on an untracked local instead of inventing a module dependency" $ do
          let x = A.name "x"
              renv = Reach.localReachEnv (Env.define [(x,I.NVar $ typeOf a)] env) globals
          expectReachError "untracked local variable x" $
            Reach.summarizeExpr renv (A.Var NoLoc $ A.NoQ x)

        it "fails on an optional Dot receiver after reconstruction" $ do
          let x = A.name "x"
              renv = (Reach.localReachEnv (Env.define [(x,I.NVar $ A.tOpt $ typeOf a)] env) globals)
                       { Reach.reachLocals = Set.singleton x }
          expectReachError "impossible receiver type" $
            Reach.summarizeExpr renv (dot method x)

expectReachError :: String -> Reach.ReachSummary -> IO ()
expectReachError needle summary = do
    result <- E.try (E.evaluate $ force summary)
    case result of
      Left (err :: E.ErrorCall) -> E.displayException err `shouldSatisfy` isInfixOf needle
      Right _                   -> expectationFailure ("expected reachability error containing " ++ show needle)

ordinaryEnv :: Env.Env0 -> Env.Env0
ordinaryEnv = Env.addModuleInfo Builtin.mBuiltin $
    Env.mkModuleInfo Builtin.mBuiltin []
      [ (A.name "value",I.NClass [] [] [] Nothing)
      , (Builtin.nIndexed,I.NProto [A.QBind a [],A.QBind b []] []
          [(Builtin.getitemKW,I.NSig getitemSchema A.NoDec Nothing)] Nothing)
      ] Nothing
  where
    a = A.TV A.KType (A.name "A")
    b = A.TV A.KType (A.name "B")
    getitemSchema = A.monotype $
      A.tFun A.fxPure (A.posRow (A.tVar a) A.posNil) A.kwdNil (A.tVar b)
