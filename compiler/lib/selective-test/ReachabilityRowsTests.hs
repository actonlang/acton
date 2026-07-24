-- SPDX-License-Identifier: BSD-3-Clause

module ReachabilityRowsTests (buildFixture, tests) where

import qualified Acton.Builtin as Builtin
import qualified Acton.Env as Env
import qualified Acton.InterfaceRows as Rows
import qualified Acton.InterfaceRowsBuilder as RowsBuilder
import qualified Acton.NameInfo as I
import qualified Acton.Names as Names
import qualified Acton.QuickType as QuickType
import qualified Acton.ReachabilityRows as Reach
import qualified Acton.ReachabilityRowsBuilder as ReachBuilder
import qualified Acton.ReachabilityTypes as Edge
import qualified Acton.SelectiveWorklist as Select
import qualified Acton.Syntax as A

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.Syd
import Utils (SrcLoc(..))


tests :: (Fixture,Reach.ReachabilityRows) -> Spec
tests (fixture,rows) = spec
  where
    spec = do
      describe "persisted reachability rows" $ do
        it "keeps same-location typed receivers exact" $ do
          let summary = memberSummary rows (hostKey fixture) (Rows.Method $ useName fixture)
          edgeSet summary `shouldBe` Set.fromList
            [ Edge.Declare (moduleName fixture) (aName fixture)
            , Edge.Declare (moduleName fixture) (bName fixture)
            , Edge.Dispatch (moduleName fixture) (aName fixture) (Edge.MethodRef $ runName fixture)
            , Edge.Dispatch (moduleName fixture) (bName fixture) (Edge.MethodRef $ runName fixture)
            ]
          case Map.lookup (hostKey fixture) (Reach.reachTopRows rows) of
            Just (Reach.LocalTop (Just (I.NClass _ _ members _)) _) -> members `shouldBe` []
            other -> expectationFailure ("missing compact class declaration: " ++ show other)

        it "terminates class-qualified calls to another member of the same class" $ do
          base <- ordinaryEnv <$> Env.initEnv "" True
          let mn = A.modName ["self_referencing_reach_rows"]
              mathUtil = A.name "MathUtil"
              square = A.name "square"
              fourth = A.name "fourth"
              unused = A.name "unused"
              squareDep = A.name "square_dependency"
              unusedDep = A.name "unused_dependency"
              x = A.name "x"
              var name = A.Var NoLoc $ A.NoQ name
              call name args = A.Call NoLoc (var name) args A.KwdNil
              method name body = A.Def NoLoc name []
                (A.PosPar x (Just A.tWild) Nothing A.PosNIL) A.KwdNIL
                (Just A.tWild) body A.Static A.fxPure Nothing
              helper name = A.Def NoLoc name [] A.PosNIL A.KwdNIL
                (Just A.tWild) [A.Return NoLoc $ Just $ A.None NoLoc]
                A.NoDec A.fxPure Nothing
              squareDecl = method square
                [A.Return NoLoc $ Just $ call squareDep A.PosNil]
              fourthDecl = method fourth
                [ A.Return NoLoc $ Just $ A.Call NoLoc
                    (A.Dot NoLoc (var mathUtil) square)
                    (A.PosArg (var x) A.PosNil) A.KwdNil
                ]
              unusedDecl = method unused
                [A.Return NoLoc $ Just $ call unusedDep A.PosNil]
              mathDecl = A.Class NoLoc mathUtil [] []
                [A.Decl NoLoc [squareDecl,fourthDecl,unusedDecl]] Nothing
              modu = A.Module mn [] Nothing
                [A.Decl NoLoc [helper squareDep,helper unusedDep,mathDecl]]
              defInfo deco = I.NDef (A.monotype A.tWild) deco Nothing
              env = Env.addActiveNames
                [ (squareDep,defInfo A.NoDec)
                , (unusedDep,defInfo A.NoDec)
                , (mathUtil,I.NClass [] []
                    [ (square,defInfo A.Static)
                    , (fourth,defInfo A.Static)
                    , (unused,defInfo A.Static)
                    ] Nothing)
                ] (Env.setMod mn base)
              mathKey = Reach.TopKey mn mathUtil
              squareDepKey = Reach.TopKey mn squareDep
              index prepared = Select.ProgramIndex
                { Select.programTops = Reach.reachTopRows prepared
                , Select.programMembers = Reach.reachMemberRows prepared
                , Select.programShapes = Reach.reachShapeRows prepared
                , Select.programSlots = Reach.reachSlotRows prepared
                , Select.programReflectableAttrs = Reach.reachReflectableRows prepared
                }
          compact <- either (error . show) return
            (RowsBuilder.prepareInterfaceRows env modu)
          prepared <- either (error . show) return
            (ReachBuilder.prepareReachabilityRows env (sourceInterface modu) modu compact)
          result <- case Select.selectProgram (index prepared)
            [Edge.Direct mn mathUtil $ Edge.MethodRef fourth] of
              Left err -> expectationFailure ("selection failed: " ++ show err)
              Right selected -> return selected
          Select.selectedTops result `shouldBe` Set.fromList [mathKey,squareDepKey]
          Select.selectedMembers result `shouldBe` Set.fromList
            [ (mathKey,Rows.Method square)
            , (mathKey,Rows.Method fourth)
            ]

        it "declares types used only by unselected method ABI slots" $ do
          base <- ordinaryEnv <$> Env.initEnv "" True
          let mn = A.modName ["stub_header_reach_rows"]
              hidden = A.name "Hidden"
              host = A.name "Host"
              used = A.name "used"
              unused = A.name "unused"
              bodyOnly = A.name "body_only"
              self = A.name "self"
              value = A.name "value"
              hiddenType = A.tCon $ A.TC (A.NoQ hidden) []
              method name params result body = A.Def NoLoc name [] params
                A.KwdNIL (Just result) body A.NoDec A.fxPure Nothing
              selfPar rest = A.PosPar self (Just A.tSelf) Nothing rest
              hiddenDecl = A.Class NoLoc hidden [] [] [] Nothing
              bodyOnlyDecl = method bodyOnly A.PosNIL Builtin.tInt
                [A.Return NoLoc $ Just $ A.Int NoLoc 1 "1"]
              usedDecl = method used (selfPar A.PosNIL) Builtin.tInt
                [A.Return NoLoc $ Just $ A.Int NoLoc 2 "2"]
              unusedDecl = method unused
                (selfPar $ A.PosPar value (Just hiddenType) Nothing A.PosNIL)
                hiddenType
                [ A.Expr NoLoc $ A.Call NoLoc (A.Var NoLoc $ A.NoQ bodyOnly)
                    A.PosNil A.KwdNil
                , A.Return NoLoc $ Just $ A.Var NoLoc $ A.NoQ value
                ]
              hostDecl = A.Class NoLoc host [] []
                [A.Decl NoLoc [usedDecl,unusedDecl]] Nothing
              modu = A.Module mn [] Nothing
                [A.Decl NoLoc [hiddenDecl,bodyOnlyDecl,hostDecl]]
              defInfo = I.NDef (A.monotype A.tWild) A.NoDec Nothing
              env = Env.addActiveNames
                [ (hidden,I.NClass [] [] [] Nothing)
                , (bodyOnly,defInfo)
                , (host,I.NClass [] [] [(used,defInfo),(unused,defInfo)] Nothing)
                ] (Env.setMod mn base)
              hostEdges prepared = edgeSet $ topSummary prepared $ Reach.TopKey mn host
          compact <- either (error . show) return
            (RowsBuilder.prepareInterfaceRows env modu)
          prepared <- either (error . show) return
            (ReachBuilder.prepareReachabilityRows env (sourceInterface modu) modu compact)
          Set.member (Edge.Declare mn hidden) (hostEdges prepared) `shouldBe` True
          Set.member (Edge.Need mn bodyOnly) (hostEdges prepared) `shouldBe` False
          Set.member (Edge.Need mn bodyOnly)
            (edgeSet $ memberSummary prepared (Reach.TopKey mn host) $ Rows.Method unused)
            `shouldBe` True

        it "keeps constructor attribute and imperative-rest summaries separate" $ do
          let owner = hostKey fixture
              attr = memberInfo rows owner (Rows.Attr $ valueName fixture)
              rest = memberSummary rows owner Rows.InitRest
              attrEdges = maybe Set.empty edgeSet (Reach.memberInstanceInitSummary attr)
              restEdges = edgeSet rest
          Set.member (Edge.Construct (moduleName fixture) $ depName fixture) attrEdges `shouldBe` True
          Set.member (Edge.Need (moduleName fixture) $ helperName fixture) attrEdges `shouldBe` False
          Set.member (Edge.Need (moduleName fixture) $ helperName fixture) restEdges `shouldBe` True
          Set.member (Edge.Construct (moduleName fixture) $ depName fixture) restEdges `shouldBe` False
          case Reach.shapeConstructor (shapeInfo rows owner) of
            Just (provider,Reach.StoredConstructor _) -> provider `shouldBe` owner
            other -> expectationFailure ("unexpected constructor row " ++ show other)

        it "projects conditional constructor dependencies by attribute" $ do
          base <- ordinaryEnv <$> Env.initEnv "" True
          let mn = A.modName ["conditional_init_reach_rows"]
              cls = A.name "Payload"
              self = A.name "self"
              used = A.name "used"
              unused = A.name "unused"
              choose = A.name "choose"
              usedValue = A.name "used_value"
              unusedValue = A.name "unused_value"
              call name = A.Call NoLoc (A.Var NoLoc $ A.NoQ name) A.PosNil A.KwdNil
              set attr value = A.MutAssign NoLoc
                (A.Dot NoLoc (A.Var NoLoc $ A.NoQ self) attr) (call value)
              conditional = A.If NoLoc
                [A.Branch (call choose) [set used usedValue,set unused unusedValue]]
                [set used usedValue,set unused unusedValue]
              initDecl = A.Def NoLoc Builtin.initKW []
                (A.PosPar self (Just A.tSelf) Nothing A.PosNIL) A.KwdNIL
                (Just A.tNone) [conditional] A.NoDec A.fxPure Nothing
              valueDecl name result value = A.Def NoLoc name [] A.PosNIL A.KwdNIL
                (Just result) [A.Return NoLoc $ Just value] A.NoDec A.fxPure Nothing
              classDecl = A.Class NoLoc cls [] []
                [ A.Signature NoLoc [used] (A.monotype A.tWild) A.Property
                , A.Signature NoLoc [unused] (A.monotype A.tWild) A.Property
                , A.Decl NoLoc [initDecl]
                ] Nothing
              modu = A.Module mn [] Nothing
                [A.Decl NoLoc
                  [ valueDecl choose Builtin.tBool (A.Bool NoLoc True)
                  , valueDecl usedValue A.tWild (A.Int NoLoc 1 "1")
                  , valueDecl unusedValue A.tWild (A.Int NoLoc 2 "2")
                  , classDecl
                  ]]
              functionInfo result = I.NDef
                (A.monotype $ A.tFun A.fxPure A.posNil A.kwdNil result)
                A.NoDec Nothing
              classInfo = I.NClass [] []
                [ (Builtin.initKW,functionInfo A.tNone)
                , (used,I.NVar A.tWild)
                , (unused,I.NVar A.tWild)
                ] Nothing
              env = Env.addActiveNames
                [ (choose,functionInfo Builtin.tBool)
                , (usedValue,functionInfo A.tWild)
                , (unusedValue,functionInfo A.tWild)
                , (cls,classInfo)
                ] (Env.setMod mn base)
              owner = Reach.TopKey mn cls
              need name = Edge.Need mn name
          compact <- either (error . show) return
            (RowsBuilder.prepareInterfaceRows env modu)
          prepared <- either (error . show) return
            (ReachBuilder.prepareReachabilityRows env (sourceInterface modu) modu compact)
          let initEdges name = maybe Set.empty edgeSet $
                Reach.memberInstanceInitSummary $
                  memberInfo prepared owner (Rows.Attr name)
              usedEdges = initEdges used
              unusedEdges = initEdges unused
          Set.member (need choose) usedEdges `shouldBe` True
          Set.member (need usedValue) usedEdges `shouldBe` True
          Set.member (need unusedValue) usedEdges `shouldBe` False
          Set.member (need choose) unusedEdges `shouldBe` True
          Set.member (need unusedValue) unusedEdges `shouldBe` True
          Set.member (need usedValue) unusedEdges `shouldBe` False

        it "models captured actor parameters and visible Assign bindings as attributes" $ do
          let owner = workerKey fixture
              config = memberInfo rows owner (Rows.Attr $ configName fixture)
              cached = memberInfo rows owner (Rows.Attr $ cachedName fixture)
              method = memberSummary rows owner (Rows.Method $ runName fixture)
          Reach.memberSummary config `shouldBe` mempty
          Reach.memberInstanceInitSummary config `shouldBe` Nothing
          Set.member (Edge.Construct (moduleName fixture) $ depName fixture)
            (maybe Set.empty edgeSet $ Reach.memberInstanceInitSummary cached) `shouldBe` True
          edgeSet method `shouldBe` Set.fromList
            [ Edge.Dispatch (moduleName fixture) (workerName fixture) (Edge.AttrRef $ configName fixture)
            , Edge.Dispatch (moduleName fixture) (workerName fixture) (Edge.AttrRef $ cachedName fixture)
            ]

        it "puts reflection on the getAttr member and not the container top" $ do
          let owner = hostKey fixture
              reflection = Edge.Reflect (moduleName fixture) (hostName fixture)
          Set.member reflection (edgeSet $ memberSummary rows owner $ Rows.Method Builtin.getAttrKW)
            `shouldBe` True
          Set.member reflection (edgeSet $ topSummary rows owner) `shouldBe` False

        it "does not turn an ordinary call into a wildcard constructor" $ do
          let rest = edgeSet $ memberSummary rows (hostKey fixture) Rows.InitRest
          Set.member (Edge.Need (moduleName fixture) $ helperName fixture) rest `shouldBe` True
          Set.member (Edge.Construct (moduleName fixture) $ helperName fixture) rest `shouldBe` False

        it "records exact post-front constructor obligations without reflection" $ do
          let hostGenerated = constructorEdges rows (hostKey fixture)
              workerGenerated = constructorEdges rows (workerKey fixture)
              direct owner name = Edge.Direct (moduleName fixture) owner (Edge.MethodRef name)
          Set.member (direct (hostName fixture) Names.altInit) hostGenerated `shouldBe` True
          Set.member (direct (hostName fixture) $ A.name "__str__") hostGenerated `shouldBe` True
          Set.member (direct (hostName fixture) Builtin.getAttrKW) hostGenerated `shouldBe` False
          Set.member (direct (workerName fixture) Builtin.cleanupKW) workerGenerated `shouldBe` True

        it "keeps local stored and generated constructor inheritance explicit" $ do
          constructorKind rows (localKey fixture "StoredBase") `shouldBe`
            (localKey fixture "StoredBase", "stored")
          constructorKind rows (localKey fixture "StoredChild") `shouldBe`
            (localKey fixture "StoredBase", "inherited")
          constructorKind rows (localKey fixture "GeneratedBase") `shouldBe`
            (localKey fixture "GeneratedBase", "generated")
          constructorKind rows (localKey fixture "GeneratedChild") `shouldBe`
            (localKey fixture "GeneratedBase", "inherited")

        it "qualifies self-module references in compact container headers" $ do
          let child = localKey fixture "StoredChild"
              parent = A.GName (moduleName fixture) (A.name "StoredBase")
          case Map.lookup child (Reach.reachTopRows rows) of
            Just (Reach.LocalTop (Just (I.NClass _ ((_,base):_) _ _)) _) ->
              A.tcname base `shouldBe` parent
            other -> expectationFailure ("missing compact child header: " ++ show other)

        it "does not guess whether an inherited external constructor is stored" $ do
          constructorKind rows (localKey fixture "ExternalStoredChild") `shouldBe`
            (externalKey "ExternalStored", "inherited")
          constructorKind rows (localKey fixture "ExternalGeneratedChild") `shouldBe`
            (externalKey "ExternalGenerated", "inherited")

        it "lets a concrete extension provider replace an inherited protocol signature" $ do
          let owner = hostKey fixture
              ref = Edge.MethodRef (A.name "execute")
              provider = Reach.TopKey (moduleName fixture) (witnessName fixture)
          Map.lookup (owner,ref) (Reach.reachSlotRows rows) `shouldBe`
            Just (Reach.SlotInfo provider $ Reach.StoredSlot $ Rows.Method $ A.name "execute")
          Set.member ref (Set.fromList $ Reach.shapeAbstracts $ shapeInfo rows owner)
            `shouldBe` False

        it "partitions class NVar bindings into exact attribute rows" $ do
          let owner = hostKey fixture
              attr = A.name "class_value"
              info = memberInfo rows owner (Rows.Attr attr)
              initialized = maybe Set.empty edgeSet (Reach.memberStaticInitSummary info)
          Set.member (Edge.Construct (moduleName fixture) $ depName fixture) initialized
            `shouldBe` True
          Map.lookup (owner,Edge.AttrRef attr) (Reach.reachSlotRows rows) `shouldBe`
            Just (Reach.SlotInfo owner Reach.AttributeSlot)
          case Map.lookup owner (Reach.reachReflectableRows rows) of
            Just (Reach.ReflectableAttrs names) -> do
              Set.member attr (Set.fromList names) `shouldBe` False
              Set.member (valueName fixture) (Set.fromList names) `shouldBe` True
            Nothing -> expectationFailure "missing reflection row"

        it "keeps a direct class attribute's conditional initializer atomic" $ do
          base <- ordinaryEnv <$> Env.initEnv "" True
          let mn = A.modName ["conditional_static_init_rows"]
              cls = A.name "Settings"
              x = A.name "x"
              y = A.name "y"
              readX = A.name "read_x"
              choose = A.name "choose"
              initialX = A.name "initial_x"
              initialY = A.name "initial_y"
              branchX = A.name "branch_x"
              branchY = A.name "branch_y"
              call name = A.Call NoLoc (A.Var NoLoc $ A.NoQ name) A.PosNil A.KwdNil
              assign name value = A.Assign NoLoc
                [A.PVar NoLoc name $ Just A.tWild] (call value)
              setInitialX = assign x initialX
              setInitialY = assign y initialY
              setBranchX = assign x branchX
              setBranchY = assign y branchY
              conditional = A.If NoLoc
                [A.Branch (call choose) [setBranchX]] [setBranchY]
              classDecl = A.Class NoLoc cls [] []
                [setInitialX,setInitialY,conditional] Nothing
              valueDecl name result = A.Def NoLoc name [] A.PosNIL A.KwdNIL
                (Just result) [A.Return NoLoc $ Just $ A.None NoLoc]
                A.NoDec A.fxPure Nothing
              readDecl = A.Def NoLoc readX [] A.PosNIL A.KwdNIL (Just A.tWild)
                [A.Return NoLoc $ Just $ A.Dot NoLoc (A.Var NoLoc $ A.NoQ cls) x]
                A.NoDec A.fxPure Nothing
              declarations =
                [ valueDecl choose Builtin.tBool
                , valueDecl initialX A.tWild
                , valueDecl initialY A.tWild
                , valueDecl branchX A.tWild
                , valueDecl branchY A.tWild
                , classDecl
                , readDecl
                ]
              modu = A.Module mn [] Nothing [A.Decl NoLoc declarations]
              functionInfo result = I.NDef
                (A.monotype $ A.tFun A.fxPure A.posNil A.kwdNil result)
                A.NoDec Nothing
              classInfo = I.NClass [] []
                [(x,I.NVar A.tWild),(y,I.NVar A.tWild)] Nothing
              env = Env.addActiveNames
                [ (choose,functionInfo Builtin.tBool)
                , (initialX,functionInfo A.tWild)
                , (initialY,functionInfo A.tWild)
                , (branchX,functionInfo A.tWild)
                , (branchY,functionInfo A.tWild)
                , (cls,classInfo)
                , (readX,functionInfo A.tWild)
                ] (Env.setMod mn base)
              owner = Reach.TopKey mn cls
              directX = Edge.Direct mn cls (Edge.AttrRef x)
              need name = Edge.Need mn name
          compact <- either (error . show) return
            (RowsBuilder.prepareInterfaceRows env modu)
          prepared <- either (error . show) return
            (ReachBuilder.prepareReachabilityRows env (sourceInterface modu) modu compact)
          Set.member directX (edgeSet $ topSummary prepared $ Reach.TopKey mn readX)
            `shouldBe` True
          let staticEdges name = maybe Set.empty edgeSet $
                Reach.memberStaticInitSummary $
                  memberInfo prepared owner (Rows.Attr name)
              xEdges = staticEdges x
              yEdges = staticEdges y
          map (`Set.member` xEdges)
            [need initialX,need choose,need branchX,need branchY]
            `shouldBe` replicate 4 True
          map (`Set.member` yEdges)
            [need initialY,need choose,need branchX,need branchY]
            `shouldBe` replicate 4 True

        it "keeps ownerless top-level dependencies in the module summary" $ do
          base <- ordinaryEnv <$> Env.initEnv "" True
          let mn = A.modName ["ownerless_reach_rows"]
              env = Env.setMod mn base
              stmt = A.Expr NoLoc $ A.Var NoLoc $
                A.GName Builtin.mBuiltin (A.name "value")
              modu = A.Module mn [] Nothing [stmt]
          compact <- either (error . show) return
            (RowsBuilder.prepareInterfaceRows env modu)
          Rows.rowStatements compact `shouldBe` [Rows.StoredWhole [] stmt]
          prepared <- either (error . show) return
            (ReachBuilder.prepareReachabilityRows env (sourceInterface modu) modu compact)
          edgeSet (Reach.reachModuleSummary prepared) `shouldBe`
            Set.fromList
              [ Edge.Need Builtin.mBuiltin (A.name "value")
              , Edge.Construct Builtin.mBuiltin (A.name "value")
              ]
          Reach.reachTopRows prepared `shouldBe` Map.empty

        it "merges a top-level signature with its implementation" $ do
          base <- ordinaryEnv <$> Env.initEnv "" True
          let mn = A.modName ["split_top_reach_rows"]
              name = A.name "setting"
              signature = A.Signature NoLoc [name] (A.monotype A.tWild) A.NoDec
              assignment = A.Assign NoLoc
                [A.PVar NoLoc name $ Just A.tWild] (A.None NoLoc)
              modu = A.Module mn [] Nothing [signature,assignment]
              env = Env.addActiveNames [(name,I.NVar A.tWild)] $ Env.setMod mn base
              key = Reach.TopKey mn name
          compact <- either (error . show) return
            (RowsBuilder.prepareInterfaceRows env modu)
          prepared <- either (error . show) return
            (ReachBuilder.prepareReachabilityRows env (sourceInterface modu) modu compact)
          Map.lookup key (Reach.reachTopRows prepared) `shouldBe`
            Just (Reach.LocalTop Nothing mempty)

        it "persists discovered builtin keys as opaque" $ do
          base <- Env.initEnv "" True
          let name = A.name "reachability_builtin_fixture"
              method = A.name "method"
              self = A.name "self"
              schema = A.monotype $ A.tFun A.fxPure A.posNil A.kwdNil A.tNone
              methodInfo = I.NDef schema A.NoDec Nothing
              decl = simpleClass name method self
              modu = A.Module Builtin.mBuiltin [] Nothing [A.Decl NoLoc [decl]]
              env = Env.addActiveNames
                [(name,I.NClass [] [] [(method,methodInfo)] Nothing)]
                (Env.setMod Builtin.mBuiltin base)
              key = Reach.TopKey Builtin.mBuiltin name
          compact <- either (error . show) return
            (RowsBuilder.prepareInterfaceRows env modu)
          prepared <- either (error . show) return
            (ReachBuilder.prepareReachabilityRows env (sourceInterface modu) modu compact)
          case Map.lookup key (Reach.reachTopRows prepared) of
            Just Reach.OpaqueTop{} -> return ()
            other -> expectationFailure ("missing opaque builtin row: " ++ show other)
          Reach.shapeConstructor (shapeInfo prepared key) `shouldBe`
            Just (key,Reach.OpaqueConstructor)
          Map.lookup (key,Edge.MethodRef method) (Reach.reachSlotRows prepared) `shouldBe`
            Just (Reach.SlotInfo key Reach.OpaqueSlot)

        it "keeps source-backed builtin call contracts on opaque tops" $ do
          base <- ordinaryEnv <$> Env.initEnv "" True
          let function = A.name "next"
              iterator = A.name "iterator"
              call = A.Call NoLoc
                (A.Dot NoLoc (A.Var NoLoc $ A.NoQ iterator) Builtin.nextKW)
                A.PosNil A.KwdNil
              decl = A.Def NoLoc function []
                (A.PosPar iterator (Just $ Builtin.tIterator Builtin.tInt) Nothing A.PosNIL)
                A.KwdNIL (Just Builtin.tInt)
                [A.Return NoLoc $ Just call] A.NoDec A.fxPure Nothing
              modu = A.Module Builtin.mBuiltin [] Nothing [A.Decl NoLoc [decl]]
              methodInfo = I.NDef (A.monotype A.tWild) A.NoDec Nothing
              env = Env.addActiveNames
                [ (function,I.NDef (A.monotype A.tWild) A.NoDec Nothing)
                , (Builtin.nIterator,I.NClass [] []
                    [(Builtin.nextKW,methodInfo)] Nothing)
                ]
                (Env.setMod Builtin.mBuiltin base)
              key = Reach.TopKey Builtin.mBuiltin function
              expected = Edge.Dispatch Builtin.mBuiltin Builtin.nIterator
                (Edge.MethodRef Builtin.nextKW)
          compact <- either (error . show) return
            (RowsBuilder.prepareInterfaceRows env modu)
          prepared <- either (error . show) return
            (ReachBuilder.prepareReachabilityRows env (sourceInterface modu) modu compact)
          case Map.lookup key (Reach.reachTopRows prepared) of
            Just (Reach.OpaqueTop summary) ->
              Set.member expected (edgeSet summary) `shouldBe` True
            other -> expectationFailure ("missing builtin call contract: " ++ show other)

        it "uses the complete typed suite env for forward and generated classes" $ do
          base <- ordinaryEnv <$> Env.initEnv "" True
          let mn = A.modName ["forward_reach_rows"]
              className = A.name "Generated"
              makeName = A.name "make"
              classDecl = A.Class NoLoc className [] [] [] Nothing
              makeDecl = A.Def NoLoc makeName [] A.PosNIL A.KwdNIL
                (Just $ A.tCon $ A.TC (A.NoQ className) [])
                [ A.Return NoLoc $ Just $ A.Call NoLoc
                    (A.Var NoLoc $ A.NoQ className) A.PosNil A.KwdNil
                ] A.NoDec A.fxPure Nothing
              modu = A.Module mn [] Nothing [A.Decl NoLoc [makeDecl,classDecl]]
              env = Env.setMod mn base
              makeKey = Reach.TopKey mn makeName
          compact <- either (error . show) return
            (RowsBuilder.prepareInterfaceRows env modu)
          prepared <- either (error . show) return
            (ReachBuilder.prepareReachabilityRows env (sourceInterface modu) modu compact)
          edgeSet (topSummary prepared makeKey) `shouldBe` Set.fromList
            [ Edge.Declare mn className
            , Edge.Need mn className
            , Edge.Construct mn className
            ]

        it "keeps source protocol and extension kinds after backend conversion" $ do
          base <- ordinaryEnv <$> Env.initEnv "" True
          let mn = A.modName ["converted_container_kinds"]
              protocol = A.name "Protocol"
              witness = A.name "Witness"
              target = A.name "Target"
              dependency = A.name "dependency"
              self = A.name "self"
              witnessField = Names.witAttr (A.NoQ protocol)
              classWitness = A.Internal A.Witness "Class" 0
              protocolType = A.TC (A.NoQ protocol) []
              witnessInit = A.Assign NoLoc
                [A.PVar NoLoc classWitness $ Just A.tWild]
                (A.Var NoLoc $ A.NoQ dependency)
              dependencyDecl = A.Def NoLoc dependency [] A.PosNIL A.KwdNIL
                (Just A.tWild) [A.Return NoLoc $ Just $ A.None NoLoc]
                A.NoDec A.fxPure Nothing
              initDecl = A.Def NoLoc Builtin.initKW []
                (A.PosPar self (Just A.tSelf) Nothing A.PosNIL)
                A.KwdNIL (Just A.tNone) [A.Pass NoLoc]
                A.NoDec A.fxPure Nothing
              backend name bases body = A.Class NoLoc name [] bases body Nothing
              modu = A.Module mn [] Nothing
                [A.Decl NoLoc
                  [ dependencyDecl
                  , backend protocol []
                      [ A.Signature NoLoc [witnessField] (A.monotype A.tNone) A.Property
                      , A.Decl NoLoc [initDecl]
                      ]
                  , backend witness [protocolType] [witnessInit]
                  , backend target [] []
                  ]]
              backendInfo = I.NClass [] [] [] Nothing
              dependencyInfo = I.NDef (A.monotype A.tWild) A.NoDec Nothing
              protocolInfo = I.NClass [] []
                [ (witnessField,I.NSig (A.monotype A.tNone) A.Property Nothing)
                , (Builtin.initKW,I.NDef (A.monotype A.tWild) A.NoDec Nothing)
                ] Nothing
              env = Env.addActiveNames
                [ (dependency,dependencyInfo)
                , (protocol,protocolInfo)
                , (witness,I.NClass [] [([],protocolType)] [(classWitness,I.NVar A.tWild)] Nothing)
                , (target,backendInfo)
                ] (Env.setMod mn base)
              source =
                [ (protocol,I.NProto [] [] [] Nothing)
                , (witness,I.NExt [] (A.TC (A.NoQ target) []) [] [] [] Nothing)
                , (target,backendInfo)
                ]
              protocolKey = Reach.TopKey mn protocol
              witnessKey = Reach.TopKey mn witness
          compact <- either (error . show) return
            (RowsBuilder.prepareInterfaceRows env modu)
          prepared <- either (error . show) return
            (ReachBuilder.prepareReachabilityRows env source modu compact)
          Reach.shapeKind (shapeInfo prepared protocolKey) `shouldBe` Reach.ProtocolShape
          case Reach.shapeConstructor (shapeInfo prepared protocolKey) of
            Just (provider,Reach.StoredConstructor _) -> provider `shouldBe` protocolKey
            constructor -> expectationFailure
              ("protocol should retain its backend constructor: " ++ show constructor)
          case Map.lookup protocolKey (Reach.reachReflectableRows prepared) of
            Just (Reach.ReflectableAttrs names) ->
              Set.member witnessField (Set.fromList names) `shouldBe` False
            Nothing -> expectationFailure "missing protocol reflection row"
          Reach.shapeKind (shapeInfo prepared witnessKey) `shouldBe` Reach.WitnessShape
          let classWitnessInfo = memberInfo prepared witnessKey (Rows.Attr classWitness)
          maybe Set.empty edgeSet (Reach.memberStaticInitSummary classWitnessInfo)
            `shouldBe` Set.singleton (Edge.Need mn dependency)
          Reach.memberInstanceInitSummary classWitnessInfo `shouldBe` Nothing
          case Reach.shapeConstructor (shapeInfo prepared witnessKey) of
            Just (provider,Reach.GeneratedConstructor _) -> provider `shouldBe` witnessKey
            constructor -> expectationFailure
              ("witness should own its generated constructor: " ++ show constructor)

        it "preserves the expanded typed MRO including builtin value" $ do
          base <- ordinaryEnv <$> Env.initEnv "" True
          let mn = A.modName ["expanded_mro_rows"]
              baseName = A.name "Base"
              midName = A.name "Mid"
              leafName = A.name "Leaf"
              local name = A.TC (A.NoQ name) []
              baseDecl = A.Class NoLoc baseName [] [Builtin.cValue] [] Nothing
              midDecl = A.Class NoLoc midName []
                [local baseName,Builtin.cValue] [] Nothing
              leafDecl = A.Class NoLoc leafName []
                [local midName,local baseName,Builtin.cValue] [] Nothing
              modu = A.Module mn [] Nothing
                [A.Decl NoLoc [baseDecl,midDecl,leafDecl]]
              env = Env.setMod mn base
              leafKey = Reach.TopKey mn leafName
          compact <- either (error . show) return
            (RowsBuilder.prepareInterfaceRows env modu)
          prepared <- either (error . show) return
            (ReachBuilder.prepareReachabilityRows env (sourceInterface modu) modu compact)
          Reach.shapeLineage (shapeInfo prepared leafKey) `shouldBe`
            [ leafKey
            , Reach.TopKey mn midName
            , Reach.TopKey mn baseName
            , Reach.TopKey Builtin.mBuiltin (A.name "value")
            ]

        it "keeps an inherited class property concrete" $ do
          base <- ordinaryEnv <$> Env.initEnv "" True
          let mn = A.modName ["inherited_property_rows"]
              baseName = A.name "Base"
              childName = A.name "Child"
              property = A.name "value"
              schema = A.monotype A.tNone
              baseType = A.TC (A.NoQ baseName) []
              baseDecl = A.Class NoLoc baseName [] []
                [A.Signature NoLoc [property] schema A.Property] Nothing
              childDecl = A.Class NoLoc childName []
                [baseType,Builtin.cValue] [] Nothing
              modu = A.Module mn [] Nothing [A.Decl NoLoc [baseDecl,childDecl]]
              env = Env.addActiveNames
                [ (baseName,I.NClass [] []
                    [(property,I.NSig schema A.Property Nothing)] Nothing)
                , (childName,I.NClass []
                    [([],baseType),([],Builtin.cValue)] [] Nothing)
                ] (Env.setMod mn base)
              baseKey = Reach.TopKey mn baseName
              childKey = Reach.TopKey mn childName
          compact <- either (error . show) return
            (RowsBuilder.prepareInterfaceRows env modu)
          prepared <- either (error . show) return
            (ReachBuilder.prepareReachabilityRows env (sourceInterface modu) modu compact)
          Map.lookup (childKey,Edge.AttrRef property) (Reach.reachSlotRows prepared)
            `shouldBe` Just (Reach.SlotInfo baseKey Reach.AttributeSlot)
          Set.member (Edge.AttrRef property)
            (Set.fromList $ Reach.shapeAbstracts $ shapeInfo prepared childKey)
            `shouldBe` False
          Set.member (Edge.Direct mn childName $ Edge.AttrRef property)
            (edgeSet $ topSummary prepared childKey) `shouldBe` False

        it "records inherited property layout and types emitted only by whole headers" $ do
          base <- ordinaryEnv <$> Env.initEnv "" True
          let mn = A.modName ["whole_property_header_rows"]
              providerModule = A.modName ["whole_property_header_base"]
              fieldName = A.name "Field"
              baseName = A.name "Base"
              childName = A.name "Child"
              property = A.name "hidden"
              fieldType = A.tCon $ A.TC (A.GName providerModule fieldName) []
              baseType = A.TC (A.GName providerModule baseName) []
              schema = A.monotype fieldType
              childDecl = A.Class NoLoc childName [] [baseType] [] Nothing
              modu = A.Module mn [] Nothing [A.Decl NoLoc [childDecl]]
              providerInfo = Env.mkModuleInfo providerModule []
                [ (fieldName,I.NClass [] [] [] Nothing)
                , (baseName,I.NClass [] []
                    [(property,I.NSig schema A.Property Nothing)] Nothing)
                ] Nothing
              env = Env.addActiveNames
                [(childName,I.NClass [] [([],baseType)] [] Nothing)]
                (Env.addModuleInfo providerModule providerInfo $ Env.setMod mn base)
              childKey = Reach.TopKey mn childName
              expectedType = Edge.Declare providerModule fieldName
              expectedLayout = Edge.DeclareAttr providerModule baseName property
          compact <- either (error . show) return
            (RowsBuilder.prepareInterfaceRows env modu)
          prepared <- either (error . show) return
            (ReachBuilder.prepareReachabilityRows env (sourceInterface modu) modu compact)
          Set.member expectedType (edgeSet $ topSummary prepared childKey)
            `shouldBe` False
          Set.member expectedLayout (edgeSet $ topSummary prepared childKey)
            `shouldBe` False
          Set.member expectedType (edgeSet $ Reach.reachWholeSummary prepared)
            `shouldBe` True
          Set.member expectedLayout (edgeSet $ Reach.reachWholeSummary prepared)
            `shouldBe` True

        it "retains an ancestor property shadowed by a whole subclass" $ do
          base <- ordinaryEnv <$> Env.initEnv "" True
          let mn = A.modName ["whole_shadowed_property_rows"]
              providerModule = A.modName ["whole_shadowed_property_base"]
              baseName = A.name "Base"
              childName = A.name "Child"
              property = A.name "value"
              schema = A.monotype Builtin.tInt
              baseType = A.TC (A.GName providerModule baseName) []
              childDecl = A.Class NoLoc childName [] [baseType]
                [A.Signature NoLoc [property] schema A.Property] Nothing
              modu = A.Module mn [] Nothing [A.Decl NoLoc [childDecl]]
              providerInfo = Env.mkModuleInfo providerModule []
                [ (baseName,I.NClass [] []
                    [(property,I.NSig schema A.Property Nothing)] Nothing)
                ] Nothing
              env = Env.addActiveNames
                [ (childName,I.NClass [] [([],baseType)]
                    [(property,I.NSig schema A.Property Nothing)] Nothing)
                ] (Env.addModuleInfo providerModule providerInfo $ Env.setMod mn base)
              childKey = Reach.TopKey mn childName
              ancestorKey = Reach.TopKey providerModule baseName
              expected = Edge.DeclareAttr providerModule baseName property
          compact <- either (error . show) return
            (RowsBuilder.prepareInterfaceRows env modu)
          prepared <- either (error . show) return
            (ReachBuilder.prepareReachabilityRows env (sourceInterface modu) modu compact)
          Map.lookup (childKey,Edge.AttrRef property) (Reach.reachSlotRows prepared)
            `shouldBe` Just (Reach.SlotInfo childKey Reach.AttributeSlot)
          Reach.shapeLineage (shapeInfo prepared childKey)
            `shouldSatisfy` elem ancestorKey
          Set.member expected (edgeSet $ Reach.reachWholeSummary prepared)
            `shouldBe` True

        it "records inherited class-table values required by CodeGen" $ do
          base <- ordinaryEnv <$> Env.initEnv "" True
          let mn = A.modName ["whole_inherited_value_rows"]
              baseName = A.name "Base"
              childName = A.name "Child"
              value = A.name "value"
              witnessValue = Names.witAttr (A.NoQ $ A.name "Transient")
              baseType = A.TC (A.NoQ baseName) []
              valueStmt = A.Assign NoLoc
                [A.PVar NoLoc value $ Just Builtin.tInt]
                (A.Int NoLoc 1 "1")
              witnessStmt = A.Assign NoLoc
                [A.PVar NoLoc witnessValue $ Just Builtin.tValue]
                (A.None NoLoc)
              baseDecl = A.Class NoLoc baseName [] []
                [valueStmt,witnessStmt] Nothing
              childDecl = A.Class NoLoc childName [] [baseType] [] Nothing
              modu = A.Module mn [] Nothing [A.Decl NoLoc [baseDecl,childDecl]]
              env = Env.addActiveNames
                [ (baseName,I.NClass [] []
                    [ (value,I.NVar Builtin.tInt)
                    , (witnessValue,I.NVar Builtin.tValue)
                    ] Nothing)
                , (childName,I.NClass [] [([],baseType)] [] Nothing)
                ] (Env.setMod mn base)
              expected = Edge.Direct mn childName (Edge.AttrRef value)
              transient = Edge.Direct mn childName (Edge.AttrRef witnessValue)
          compact <- either (error . show) return
            (RowsBuilder.prepareInterfaceRows env modu)
          prepared <- either (error . show) return
            (ReachBuilder.prepareReachabilityRows env (sourceInterface modu) modu compact)
          Set.member expected (edgeSet $ topSummary prepared $ Reach.TopKey mn childName)
            `shouldBe` True
          Set.member expected (edgeSet $ Reach.reachWholeSummary prepared)
            `shouldBe` True
          Set.member transient (edgeSet $ topSummary prepared $ Reach.TopKey mn childName)
            `shouldBe` False

data Fixture = Fixture
  { moduleName :: A.ModName
  , aName      :: A.Name
  , bName      :: A.Name
  , depName    :: A.Name
  , hostName   :: A.Name
  , workerName :: A.Name
  , helperName :: A.Name
  , runName    :: A.Name
  , useName    :: A.Name
  , valueName  :: A.Name
  , configName :: A.Name
  , cachedName :: A.Name
  , witnessName :: A.Name
  }

hostKey :: Fixture -> Reach.TopKey
hostKey fixture = Reach.TopKey (moduleName fixture) (hostName fixture)

workerKey :: Fixture -> Reach.TopKey
workerKey fixture = Reach.TopKey (moduleName fixture) (workerName fixture)

localKey :: Fixture -> String -> Reach.TopKey
localKey fixture = Reach.TopKey (moduleName fixture) . A.name

externalModule :: A.ModName
externalModule = A.modName ["external_reach_rows"]

externalKey :: String -> Reach.TopKey
externalKey = Reach.TopKey externalModule . A.name

edgeSet :: Edge.ReachSummary -> Set.Set Edge.ReachEdge
edgeSet = Set.fromList . Edge.reachEdges

sourceInterface :: A.Module -> I.TEnv
sourceInterface = QuickType.envOfTopSuite . A.mbody

topSummary :: Reach.ReachabilityRows -> Reach.TopKey -> Edge.ReachSummary
topSummary rows key = case Map.lookup key (Reach.reachTopRows rows) of
    Just (Reach.LocalTop _ summary) -> summary
    other -> error ("missing local top row: " ++ show other)

memberInfo :: Reach.ReachabilityRows -> Reach.TopKey -> Rows.MemberKey -> Reach.MemberInfo
memberInfo rows owner member = case Map.lookup (owner,member) (Reach.reachMemberRows rows) of
    Just info -> info
    Nothing -> error ("missing member row: " ++ show (owner,member))

memberSummary :: Reach.ReachabilityRows -> Reach.TopKey -> Rows.MemberKey -> Edge.ReachSummary
memberSummary rows owner = Reach.memberSummary . memberInfo rows owner

shapeInfo :: Reach.ReachabilityRows -> Reach.TopKey -> Reach.ShapeInfo
shapeInfo rows owner = case Map.lookup owner (Reach.reachShapeRows rows) of
    Just shape -> shape
    Nothing -> error ("missing shape row: " ++ show owner)

constructorEdges :: Reach.ReachabilityRows -> Reach.TopKey -> Set.Set Edge.ReachEdge
constructorEdges rows owner = case Reach.shapeConstructor (shapeInfo rows owner) of
    Just (_,Reach.StoredConstructor summary) -> edgeSet summary
    Just (_,Reach.GeneratedConstructor summary) -> edgeSet summary
    other -> error ("missing concrete constructor: " ++ show other)

constructorKind :: Reach.ReachabilityRows -> Reach.TopKey -> (Reach.TopKey,String)
constructorKind rows owner = case Reach.shapeConstructor (shapeInfo rows owner) of
    Just (provider,Reach.StoredConstructor _)    -> (provider,"stored")
    Just (provider,Reach.GeneratedConstructor _) -> (provider,"generated")
    Just (provider,Reach.InheritedConstructor _) -> (provider,"inherited")
    Just (provider,Reach.OpaqueConstructor)       -> (provider,"opaque")
    other -> error ("missing concrete constructor: " ++ show other)


buildFixture :: IO (Fixture,Reach.ReachabilityRows)
buildFixture = do
    base <- ordinaryEnv <$> Env.initEnv "" True
    let fixture = Fixture
          { moduleName = A.modName ["reach_rows"]
          , aName = A.name "A"
          , bName = A.name "B"
          , depName = A.name "Dep"
          , hostName = A.name "Host"
          , workerName = A.name "Worker"
          , helperName = A.name "helper"
          , runName = A.name "run"
          , useName = A.name "use"
          , valueName = A.name "value"
          , configName = A.name "config"
          , cachedName = A.name "cached"
          , witnessName = Names.extensionName [protocolType] hostType
          }
        mn = moduleName fixture
        run = runName fixture
        use = useName fixture
        value = valueName fixture
        config = configName fixture
        cached = cachedName fixture
        self = A.name "self"
        x = A.name "x"
        y = A.name "y"
        hidden = A.name "_tmp"
        classValue = A.name "class_value"
        execute = A.name "execute"
        runnable = A.name "Runnable"
        storedBase = A.name "StoredBase"
        storedChild = A.name "StoredChild"
        generatedBase = A.name "GeneratedBase"
        generatedChild = A.name "GeneratedChild"
        externalStored = A.name "ExternalStored"
        externalGenerated = A.name "ExternalGenerated"
        externalStoredChild = A.name "ExternalStoredChild"
        externalGeneratedChild = A.name "ExternalGeneratedChild"
        methodSchema = A.monotype $ A.tFun A.fxPure A.posNil A.kwdNil A.tNone
        functionInfo = I.NDef methodSchema A.NoDec Nothing
        abstractInfo = I.NSig methodSchema A.NoDec Nothing
        classInfo bases members = I.NClass [] [ ([],base) | base <- bases ] members Nothing
        hostType = A.TC (A.NoQ $ hostName fixture) []
        protocolType = A.TC (A.NoQ runnable) []
        storedBaseType = A.TC (A.NoQ storedBase) []
        generatedBaseType = A.TC (A.NoQ generatedBase) []
        externalType name = A.TC (A.GName externalModule name) []
        aDecl = simpleClass (aName fixture) run self
        bDecl = simpleClass (bName fixture) run self
        depInit = A.Def NoLoc Builtin.initKW []
          (A.PosPar self (Just A.tSelf) Nothing A.PosNIL) A.KwdNIL
          (Just A.tNone) [A.Pass NoLoc] A.NoDec A.fxPure Nothing
        depDecl = A.Class NoLoc (depName fixture) [] []
          [A.Decl NoLoc [depInit]] Nothing
        useDecl = A.Def NoLoc use []
          (A.PosPar x (Just $ typeOf fixture $ aName fixture) Nothing $
           A.PosPar y (Just $ typeOf fixture $ bName fixture) Nothing A.PosNIL)
          A.KwdNIL (Just A.tNone)
          [ A.Expr NoLoc $ A.Dot NoLoc (A.Var NoLoc $ A.NoQ x) run
          , A.Expr NoLoc $ A.Dot NoLoc (A.Var NoLoc $ A.NoQ y) run
          ] A.NoDec A.fxPure Nothing
        initDecl = A.Def NoLoc Builtin.initKW []
          (A.PosPar self (Just A.tSelf) Nothing A.PosNIL) A.KwdNIL (Just A.tNone)
          [ A.MutAssign NoLoc
              (A.Dot NoLoc (A.Var NoLoc $ A.NoQ self) value)
              (A.Call NoLoc (A.Var NoLoc $ A.NoQ $ depName fixture) A.PosNil A.KwdNil)
          , A.Expr NoLoc $ A.Call NoLoc
              (A.Var NoLoc $ A.NoQ $ helperName fixture) A.PosNil A.KwdNil
          ] A.NoDec A.fxPure Nothing
        getter = A.Def NoLoc Builtin.getAttrKW []
          (A.PosPar self (Just A.tSelf) Nothing $
           A.PosPar (A.name "name") (Just Builtin.tStr) Nothing A.PosNIL)
          A.KwdNIL (Just $ A.tOpt Builtin.tValue)
          [A.Return NoLoc $ Just $ A.None NoLoc] A.NoDec A.fxPure Nothing
        altInit = A.Def NoLoc Names.altInit []
          (A.PosPar self (Just A.tSelf) Nothing A.PosNIL) A.KwdNIL (Just A.tNone)
          [A.Return NoLoc $ Just $ A.None NoLoc] A.NoDec A.fxPure Nothing
        strMethod = A.Def NoLoc (A.name "__str__") []
          (A.PosPar self (Just A.tSelf) Nothing A.PosNIL) A.KwdNIL (Just A.tNone)
          [A.Return NoLoc $ Just $ A.None NoLoc] A.NoDec A.fxPure Nothing
        hostDecl = A.Class NoLoc (hostName fixture) [] [protocolType]
          [ A.Signature NoLoc [value] (A.monotype A.tWild) A.Property
          , A.Assign NoLoc [A.PVar NoLoc classValue $ Just $ typeOf fixture $ depName fixture]
              (A.Call NoLoc (A.Var NoLoc $ A.NoQ $ depName fixture) A.PosNil A.KwdNil)
          , A.Decl NoLoc [initDecl,useDecl,getter,altInit,strMethod]
          ] Nothing
        protocolDecl = A.Protocol NoLoc runnable [] []
          [A.Signature NoLoc [execute] methodSchema A.NoDec] Nothing
        executeDecl = A.Def NoLoc execute []
          (A.PosPar self (Just A.tSelf) Nothing A.PosNIL) A.KwdNIL (Just A.tNone)
          [A.Return NoLoc $ Just $ A.None NoLoc] A.NoDec A.fxPure Nothing
        extensionDecl = A.Extension NoLoc [] hostType [protocolType]
          [A.Decl NoLoc [executeDecl]] Nothing
        storedInit = A.Def NoLoc Builtin.initKW []
          (A.PosPar self (Just A.tSelf) Nothing A.PosNIL) A.KwdNIL (Just A.tNone)
          [ A.Expr NoLoc $ A.Call NoLoc
              (A.Var NoLoc $ A.NoQ $ helperName fixture) A.PosNil A.KwdNil
          ] A.NoDec A.fxPure Nothing
        storedBaseDecl = A.Class NoLoc storedBase [] []
          [A.Decl NoLoc [storedInit]] Nothing
        storedChildDecl = A.Class NoLoc storedChild [] [storedBaseType] [] Nothing
        generatedBaseDecl = A.Class NoLoc generatedBase [] [] [] Nothing
        generatedChildDecl = A.Class NoLoc generatedChild [] [generatedBaseType] [] Nothing
        externalStoredChildDecl = A.Class NoLoc externalStoredChild []
          [externalType externalStored] [] Nothing
        externalGeneratedChildDecl = A.Class NoLoc externalGeneratedChild []
          [externalType externalGenerated] [] Nothing
        actorMethod = A.Def NoLoc run [] A.PosNIL A.KwdNIL (Just A.tNone)
          [ A.Expr NoLoc $ A.Tuple NoLoc
              (A.PosArg (A.Var NoLoc $ A.NoQ config) $
               A.PosArg (A.Var NoLoc $ A.NoQ cached) A.PosNil) A.KwdNil
          ] A.NoDec A.fxPure Nothing
        workerDecl = A.Actor NoLoc (workerName fixture) []
          (A.PosPar config (Just A.tWild) Nothing A.PosNIL) A.KwdNIL
          [ A.Assign NoLoc [A.PVar NoLoc cached $ Just $ typeOf fixture $ depName fixture]
              (A.Call NoLoc (A.Var NoLoc $ A.NoQ $ depName fixture) A.PosNil A.KwdNil)
          , A.Assign NoLoc [A.PVar NoLoc hidden $ Just A.tWild] (A.None NoLoc)
          , A.Decl NoLoc
              [ actorMethod
              , A.Def NoLoc Builtin.cleanupKW [] A.PosNIL A.KwdNIL (Just A.tNone)
                  [A.Return NoLoc $ Just $ A.None NoLoc] A.NoDec A.fxPure Nothing
              ]
          ] Nothing
        helperDecl = A.Def NoLoc (helperName fixture) [] A.PosNIL A.KwdNIL
          (Just A.tNone) [A.Return NoLoc $ Just $ A.None NoLoc] A.NoDec A.fxPure Nothing
        suite =
          [ A.Decl NoLoc
              [ aDecl,bDecl,depDecl,protocolDecl,hostDecl,extensionDecl
              , storedBaseDecl,storedChildDecl,generatedBaseDecl,generatedChildDecl
              , externalStoredChildDecl,externalGeneratedChildDecl
              , workerDecl,helperDecl
              ]
          ]
        modu = A.Module mn [] Nothing suite
        externalInfo = Env.mkModuleInfo externalModule []
          [ (externalStored,classInfo [] [(Builtin.initKW,functionInfo)])
          , (externalGenerated,classInfo [] [])
          ] Nothing
        env = Env.addActiveNames
          [ (aName fixture, classInfo [] [(run,functionInfo)])
          , (bName fixture, classInfo [] [(run,functionInfo)])
          , (depName fixture, classInfo [] [(Builtin.initKW,functionInfo)])
          , (runnable,I.NProto [] [] [(execute,abstractInfo)] Nothing)
          , (hostName fixture, classInfo [protocolType]
              [ (Builtin.initKW,functionInfo)
              , (use,functionInfo)
              , (Builtin.getAttrKW,functionInfo)
              , (Names.altInit,functionInfo)
              , (A.name "__str__",functionInfo)
              , (value,I.NVar A.tWild)
              , (classValue,I.NVar $ typeOf fixture $ depName fixture)
              ])
          , (witnessName fixture,I.NExt [] hostType [([],protocolType)]
              [(execute,functionInfo)] [] Nothing)
          , (storedBase,classInfo [] [(Builtin.initKW,functionInfo)])
          , (storedChild,classInfo [storedBaseType] [])
          , (generatedBase,classInfo [] [])
          , (generatedChild,classInfo [generatedBaseType] [])
          , (externalStoredChild,classInfo [externalType externalStored] [])
          , (externalGeneratedChild,classInfo [externalType externalGenerated] [])
          , (workerName fixture, I.NAct [] (A.posRow A.tWild A.posNil) A.kwdNil
              [ (run,functionInfo)
              , (Builtin.cleanupKW,functionInfo)
              , (cached,I.NVar $ typeOf fixture $ depName fixture)
              ] Nothing)
          , (helperName fixture,functionInfo)
          ] (Env.addModuleInfo externalModule externalInfo $ Env.setMod mn base)
    compact <- either (error . show) return (RowsBuilder.prepareInterfaceRows env modu)
    prepared <- either (error . show) return (ReachBuilder.prepareReachabilityRows env (sourceInterface modu) modu compact)
    return (fixture,prepared)

simpleClass :: A.Name -> A.Name -> A.Name -> A.Decl
simpleClass name run self = A.Class NoLoc name [] []
    [A.Decl NoLoc [A.Def NoLoc run []
      (A.PosPar self (Just A.tSelf) Nothing A.PosNIL) A.KwdNIL (Just A.tNone)
      [A.Return NoLoc $ Just $ A.None NoLoc] A.NoDec A.fxPure Nothing]] Nothing

typeOf :: Fixture -> A.Name -> A.Type
typeOf fixture name = A.tCon $ A.TC (A.GName (moduleName fixture) name) []

ordinaryEnv :: Env.Env0 -> Env.Env0
ordinaryEnv = Env.addModuleInfo Builtin.mBuiltin $
    Env.mkModuleInfo Builtin.mBuiltin []
      [(A.name "value",I.NClass [] [] [] Nothing)] Nothing
