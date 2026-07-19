-- SPDX-License-Identifier: BSD-3-Clause

module SelectiveWorklistTests (tests) where

import qualified Acton.Builtin as Builtin
import qualified Acton.InterfaceRows as Rows
import qualified Acton.NameInfo as I
import qualified Acton.ReachabilityTypes as Reach
import qualified Acton.SelectiveWorklist as Select
import qualified Acton.Syntax as A

import Control.Monad.State.Strict (State, modify', runState)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.Syd


tests :: Spec
tests = do
  describe "exact selective worklist" $ do
    it "is independent of seed queue order" $ do
      let index = methodHierarchy
          forward = [construct d, dispatch b run]
          reverseOrder = reverse forward
      first <- selected index forward
      second <- selected index reverseOrder
      Select.selectionManifest first `shouldBe` Select.selectionManifest second

    it "keeps a method and attribute with the same spelling distinct" $ do
      let x = A.name "same"
          cShape = classShape c [c]
            [ methodDecl c x
            , attrDecl c x
            ]
          index = mkIndex [cShape]
            [ methodRow c x mempty
            , attrRow c x mempty Nothing
            ] []
      result <- selected index [direct c (Reach.MethodRef x), direct c (Reach.AttrRef x)]
      Set.member (c, Rows.Method x) (Select.selectedMembers result) `shouldBe` True
      Set.member (c, Rows.Attr x) (Select.selectedMembers result) `shouldBe` True
      Set.member (c, x) (Select.selectedAttrs result) `shouldBe` True

    it "does not mix same-named methods on unrelated classes" $ do
      let aShape = classShape a [a] [methodDecl a run]
          bShape = classShape b [b] [methodDecl b run]
          index = mkIndex [aShape,bShape]
            [ methodRow a run mempty
            , methodRow b run mempty
            ] []
      result <- selected index [dispatch a run, construct a]
      Set.member (a, Rows.Method run) (Select.selectedMembers result) `shouldBe` True
      Set.member (b, Rows.Method run) (Select.selectedMembers result) `shouldBe` False

    it "activates a dispatched attribute initializer for an opaque result" $ do
      let x = A.name "field"
          cShape = classShape c [c] [attrDecl c x]
          index = mkIndex [cShape]
            [attrRow c x mempty (Just $ summary [need d])] [(d,mempty)]
      result <- selected index [dispatchRef c (Reach.AttrRef x)]
      Set.member (c, Rows.Attr x) (Select.selectedMembers result) `shouldBe` True
      Set.member (c, x) (Select.selectedAttrs result) `shouldBe` True
      Set.member (c, x) (Select.selectedInstanceInitializers result) `shouldBe` True
      Set.member d (Select.selectedTops result) `shouldBe` True

    it "activates a direct attribute initializer for an opaque result" $ do
      let x = A.name "field"
          cShape = classShape c [c] [attrDecl c x]
          index = mkIndex [cShape]
            [attrRow c x mempty (Just $ summary [need d])] [(d,mempty)]
      result <- selected index [direct c (Reach.AttrRef x)]
      Set.member (c, x) (Select.selectedInstanceInitializers result) `shouldBe` True
      Set.member d (Select.selectedTops result) `shouldBe` True

    it "retains an attribute declaration without its initializer" $ do
      let x = A.name "field"
          declarationDep = top "DeclarationDep"
          initializerDep = top "InitializerDep"
          cShape = classShape c [c] [attrDecl c x]
          index = mkIndex [cShape]
            [ attrRow c x
                (summary [need declarationDep])
                (Just $ summary [need initializerDep])
            ] [(declarationDep,mempty),(initializerDep,mempty)]
      result <- selected index [declareAttr c x]
      Set.member (c, Rows.Attr x) (Select.selectedMembers result) `shouldBe` True
      Set.member (c, x) (Select.selectedAttrs result) `shouldBe` False
      Set.member (c, x) (Select.selectedInstanceInitializers result) `shouldBe` False
      Set.member declarationDep (Select.selectedTops result) `shouldBe` True
      Set.member initializerDep (Select.selectedTops result) `shouldBe` False

    it "activates a local initializer for an inherited opaque attribute" $ do
      let x = A.name "field"
          opaque = top "OpaqueBase"
          cShape = classShape c [c,opaque]
            [(Reach.AttrRef x,Select.SlotInfo opaque Select.OpaqueSlot)]
          baseIndex = mkIndex [cShape]
            [attrRow c x mempty (Just $ summary [need d])] [(d,mempty)]
          index = baseIndex
            { Select.programTops = Map.insert opaque
                (Select.OpaqueTop mempty) (Select.programTops baseIndex)
            }
      directResult <- selected index [direct c (Reach.AttrRef x)]
      dispatchResult <- selected index [dispatchRef c (Reach.AttrRef x)]
      Set.member (c, x) (Select.selectedInstanceInitializers directResult)
        `shouldBe` True
      Set.member (c, x) (Select.selectedInstanceInitializers dispatchResult)
        `shouldBe` True

    it "retains a local initializer behind an opaque attribute provider" $ do
      let x = A.name "field"
          opaque = top "OpaqueBase"
          cShape = classShape c [c,opaque]
            [(Reach.AttrRef x,Select.SlotInfo opaque Select.AttributeSlot)]
          baseIndex = mkIndex [cShape]
            [ attrRow c x mempty (Just $ summary [need d])
            , attrRow opaque x (summary [need e]) Nothing
            ] [(d,mempty),(e,mempty)]
          index = baseIndex
            { Select.programTops = Map.insert opaque
                (Select.OpaqueTop mempty) (Select.programTops baseIndex)
            }
      directResult <- selected index [direct c (Reach.AttrRef x)]
      dispatchResult <- selected index [dispatchRef c (Reach.AttrRef x)]
      mapM_ (\result -> do
          Set.member (c, Rows.Attr x) (Select.selectedMembers result)
            `shouldBe` True
          Set.member (opaque, Rows.Attr x) (Select.selectedMembers result)
            `shouldBe` False
          Set.member (c, x) (Select.selectedInstanceInitializers result)
            `shouldBe` True
          Set.member d (Select.selectedTops result) `shouldBe` True
          Set.member e (Select.selectedTops result) `shouldBe` False)
        [directResult,dispatchResult]

    it "requests each reached row once and no unused rows" $ do
      let x = A.name "unusedAttr"
          cShape = classShape c [c] [methodDecl c run, attrDecl c x]
          eShape = classShape e [e] [methodDecl e run]
          index = mkIndex [cShape,eShape]
            [ methodRow c run mempty
            , attrRow c x mempty Nothing
            , methodRow e run mempty
            ] []
          lookups = countedLookup index
          seeds = replicate 3 (direct c $ Reach.MethodRef run)
          (outcome,counts) = runState (Select.selectProgramM lookups seeds) emptyCounts
      case outcome of
        Left err -> expectationFailure ("selection failed: " ++ show err)
        Right result -> Select.selectedOpaqueTops result `shouldBe` Set.empty
      counts `shouldBe` LookupCounts
        { countedTops = Map.singleton c 1
        , countedMembers = Map.singleton (c,Rows.Method run) 1
        , countedShapes = Map.empty
        , countedSlots = Map.singleton (c,Reach.MethodRef run) 1
        , countedSurfaces = Map.empty
        , countedReflections = Map.empty
        }

    it "does not request a shape for a plain Need" $ do
      let index = mkIndex [] [] [(c,mempty)]
          (outcome,counts) = runState
            (Select.selectProgramM (countedLookup index) [need c]) emptyCounts
      case outcome of
        Left err -> expectationFailure ("selection failed: " ++ show err)
        Right result -> Select.selectedTops result `shouldBe` Set.singleton c
      counts `shouldBe` emptyCounts
        { countedTops = Map.singleton c 1 }

    it "records declarations without row reads and lets Need supersede them" $ do
      let declarationOnly = Select.selectProgram Select.emptyProgramIndex [declare c]
          index = mkIndex [] [] [(c,mempty)]
      case declarationOnly of
        Left err -> expectationFailure ("selection failed: " ++ show err)
        Right result -> do
          Select.selectedDeclarations result `shouldBe` Set.singleton c
          Select.selectedTops result `shouldBe` Set.empty
      forward <- selected index [declare c,need c]
      reverseOrder <- selected index [need c,declare c]
      Select.selectionManifest forward `shouldBe` Select.selectionManifest reverseOrder
      Select.selectedDeclarations forward `shouldBe` Set.empty
      Select.selectedTops forward `shouldBe` Set.singleton c

    it "rejects a selected container whose shape row is missing" $ do
      let header = I.NClass [] [] [] Nothing
          index = Select.emptyProgramIndex
            { Select.programTops = Map.singleton c
                (Select.LocalTop (Just header) mempty)
            }
      Select.selectProgram index [need c] `shouldBe` Left (Select.MissingShape c)

    it "treats an opaque plain top as a terminal" $ do
      let opaque = top "Opaque"
          index = Select.emptyProgramIndex
            { Select.programTops = Map.singleton opaque (Select.OpaqueTop mempty) }
          (outcome,counts) = runState
            (Select.selectProgramM (countedLookup index) [need opaque]) emptyCounts
      case outcome of
        Left err -> expectationFailure ("selection failed: " ++ show err)
        Right result -> Select.selectedOpaqueTops result `shouldBe` Set.singleton opaque
      counts `shouldBe` emptyCounts
        { countedTops = Map.singleton opaque 1 }

    it "follows the semantic contract stored on an opaque top" $ do
      let opaque = top "OpaqueNext"
          helper = top "Helper"
          cShape = classShape c [c] [methodDecl c run]
          index0 = mkIndex [cShape]
            [methodRow c run $ summary [need helper]] [(helper,mempty)]
          index = index0
            { Select.programTops = Map.insert opaque
                (Select.OpaqueTop $ summary [dispatch c run])
                (Select.programTops index0)
            }
      result <- selected index [need opaque,construct c]
      Set.member (c,Rows.Method run) (Select.selectedMembers result) `shouldBe` True
      Set.member helper (Select.selectedTops result) `shouldBe` True

    it "terminates an opaque slot after exact provider resolution" $ do
      let opaque = top "Opaque"
          ref = Reach.MethodRef run
          shape = classShape opaque [opaque]
            [(ref,Select.SlotInfo opaque Select.OpaqueSlot)]
          index0 = mkIndex [shape] [] []
          index = index0
            { Select.programTops = Map.insert opaque (Select.OpaqueTop mempty)
                (Select.programTops index0) }
          (outcome,counts) = runState
            (Select.selectProgramM (countedLookup index) [direct opaque ref]) emptyCounts
      case outcome of
        Left err -> expectationFailure ("selection failed: " ++ show err)
        Right _ -> return ()
      counts `shouldBe` emptyCounts
        { countedTops = Map.singleton opaque 1
        , countedSlots = Map.singleton (opaque,ref) 1
        }

    it "terminates an inherited concrete slot at an opaque provider" $ do
      let opaque = top "OpaqueProvider"
          bodyDep = top "OpaqueBodyDep"
          ref = Reach.MethodRef run
          cShape = classShape c [c,opaque]
            [(ref,Select.SlotInfo opaque $ Select.StoredSlot $ Rows.Method run)]
          index0 = mkIndex [cShape]
            [methodRow opaque run $ summary [need bodyDep]]
            [(bodyDep,mempty)]
          index = index0
            { Select.programTops = Map.insert opaque (Select.OpaqueTop mempty)
                (Select.programTops index0)
            }
          (outcome,counts) = runState
            (Select.selectProgramM (countedLookup index) [direct c ref]) emptyCounts
      result <- case outcome of
        Left err -> expectationFailure ("selection failed: " ++ show err)
        Right selectedResult -> return selectedResult
      Select.selectedOpaqueTops result `shouldBe` Set.singleton opaque
      Set.member bodyDep (Select.selectedTops result) `shouldBe` False
      counts `shouldBe` emptyCounts
        { countedTops = Map.fromList [(c,1),(opaque,1)]
        , countedSlots = Map.singleton (c,ref) 1
        }

    it "routes opaque construction through selectable inherited layout and methods" $ do
      let padding = A.name "_padding"
          value = A.name "value"
          baseShape = classShape b [b]
            [ attrDecl b padding
            , attrDecl b value
            , methodDecl b run
            ]
          derivedShape = withConstructor
            (b,Select.InheritedConstructor mempty)
            (classShape d [d,b]
              [ attrDecl b padding
              , attrDecl b value
              , methodDecl b run
              ])
          index0 = mkIndex [baseShape,derivedShape]
            [ attrRow b padding mempty Nothing
            , attrRow b value mempty Nothing
            , methodRow b run mempty
            ] []
          index = index0
            { Select.programTops = Map.insert d (Select.OpaqueTop mempty)
                (Select.programTops index0) }
      result <- selected index [construct d,dispatch d run]
      Set.member (b,padding) (Select.selectedAttrs result) `shouldBe` True
      Set.member (b,value) (Select.selectedAttrs result) `shouldBe` True
      Set.member (b,Rows.Method run) (Select.selectedMembers result) `shouldBe` True
      Set.member b (Select.selectedInitialized result) `shouldBe` True

    it "retains selectable layout behind an opaque ancestor barrier" $ do
      let padding = A.name "_padding"
          baseShape = classShape b [b] [attrDecl b padding]
          middleShape = withConstructor
            (b,Select.InheritedConstructor mempty)
            (classShape e [e,b] [attrDecl b padding])
          childShape = withConstructor
            (e,Select.InheritedConstructor mempty)
            (classShape d [d,e,b] [attrDecl b padding])
          index0 = mkIndex [baseShape,middleShape,childShape]
            [attrRow b padding mempty Nothing] []
          index = index0
            { Select.programTops = Map.insert e (Select.OpaqueTop mempty)
                (Select.programTops index0) }
      result <- selected index [construct d]
      Set.member (b,padding) (Select.selectedAttrs result) `shouldBe` True
      Set.member b (Select.selectedInitialized result) `shouldBe` True

    it "keeps abstract inherited slots as errors at opaque providers" $ do
      let opaque = top "OpaqueProvider"
          ref = Reach.MethodRef run
          cShape = classShape c [c,opaque]
            [(ref,Select.SlotInfo opaque Select.AbstractSlot)]
          index0 = mkIndex [cShape] [] []
          index = index0
            { Select.programTops = Map.insert opaque (Select.OpaqueTop mempty)
                (Select.programTops index0)
            }
      Select.selectProgram index [direct c ref]
        `shouldBe` Left (Select.AbstractMemberSelected c ref opaque)

    it "dispatches through an opaque base using only the local subtype lineage" $ do
      let opaque = top "OpaqueBase"
          dShape = classShape d [d,opaque] [methodDecl d run]
          index0 = mkIndex [dShape] [methodRow d run mempty] []
          index = index0
            { Select.programTops = Map.insert opaque (Select.OpaqueTop mempty)
                (Select.programTops index0)
            }
          (outcome,counts) = runState
            (Select.selectProgramM (countedLookup index) [dispatch opaque run,construct d])
            emptyCounts
      result <- case outcome of
        Left err -> expectationFailure ("selection failed: " ++ show err)
        Right selectedResult -> return selectedResult
      Set.member (d,Rows.Method run) (Select.selectedMembers result) `shouldBe` True
      Map.member opaque (countedShapes counts) `shouldBe` False
      any ((== opaque) . fst) (Map.keys $ countedMembers counts) `shouldBe` False
      Map.member opaque (countedReflections counts) `shouldBe` False

    it "reports a missing exact slot" $ do
      let cShape = classShape c [c] []
          index = mkIndex [cShape] [] []
      Select.selectProgram index [direct c (Reach.MethodRef run)]
        `shouldBe` Left (Select.MissingSlot c (Reach.MethodRef run))

    it "resolves direct selection on the static class and dispatch on the runtime class" $ do
      directResult <- selected methodHierarchy [direct b (Reach.MethodRef run)]
      dispatchResult <- selected methodHierarchy [construct d, dispatch b run]
      Set.member (b, Rows.Method run) (Select.selectedMembers directResult) `shouldBe` True
      Set.member (d, Rows.Method run) (Select.selectedMembers directResult) `shouldBe` False
      Set.member (d, Rows.Method run) (Select.selectedMembers dispatchResult) `shouldBe` True
      Set.member (b, Rows.Method run) (Select.selectedMembers dispatchResult) `shouldBe` False

    it "replays a dispatch recorded before construction" $ do
      result <- selected methodHierarchy [dispatch b run, construct d]
      Set.member (d, Rows.Method run) (Select.selectedMembers result) `shouldBe` True

    it "chooses an override and an inherited provider for different runtime classes" $ do
      let bShape = classShape b [b] [methodDecl b run]
          dShape = classShape d [d,b] [methodDecl d run]
          eShape = classShape e [e,b] [methodDecl b run]
          index = mkIndex [bShape,dShape,eShape]
            [ methodRow b run mempty
            , methodRow d run mempty
            ] []
      result <- selected index [dispatch b run, construct d, construct e]
      Set.member (d, Rows.Method run) (Select.selectedMembers result) `shouldBe` True
      Set.member (b, Rows.Method run) (Select.selectedMembers result) `shouldBe` True

    it "resolves a witness provider outside the subtype lineage" $ do
      let protocol = top "Runnable"
          witness = top "RunnableWitness"
          protocolShape = protocolFixture protocol [protocol]
            [(Reach.MethodRef run, Select.SlotInfo protocol Select.AbstractSlot)]
          concreteShape = classShape c [c,protocol]
            [methodDecl witness run]
          index = mkIndex [protocolShape,concreteShape]
            [methodRow witness run mempty]
            [(witness,mempty)]
      result <- selected index [dispatch protocol run, construct c]
      Set.member witness (Select.selectedTops result) `shouldBe` True
      Set.member (witness, Rows.Method run) (Select.selectedMembers result)
        `shouldBe` True

    it "rejects construction with an unresolved abstract attribute" $ do
      let x = A.name "value"
          cShape = classShape c [c]
            [(Reach.AttrRef x, Select.SlotInfo c Select.AbstractSlot)]
          index = mkIndex [cShape] [] []
      Select.selectProgram index [construct c]
        `shouldBe` Left (Select.AbstractClassConstructed c [Reach.AttrRef x])

    it "replays reflection in either discovery order" $ do
      let x = A.name "value"
          bShape = classShape b [b] [attrDecl b x]
          dShape = classShape d [d,b] [attrDecl b x]
          index = mkIndex [bShape,dShape]
            [ attrRow b x mempty Nothing ] []
          forward = [reflect b, construct d]
      first <- selected index forward
      second <- selected index (reverse forward)
      Select.selectionManifest first `shouldBe` Select.selectionManifest second
      Set.member (b, Rows.Attr x) (Select.selectedMembers first) `shouldBe` True
      Set.member (b, x) (Select.selectedAttrs first) `shouldBe` True

    it "does not treat a directly invoked base initializer as base construction" $ do
      let bShape = classShape b [b] [methodDecl b run]
          dShape = classShape d [d,b] [methodDecl d run]
          baseInit = summary [direct b (Reach.MethodRef Builtin.initKW)]
          index = mkIndex [bShape,dShape]
            [ methodRow b run mempty
            , methodRow d run mempty
            , memberRow d Rows.InitRest baseInit Nothing
            ] []
      result <- selected index [dispatch b run, construct d]
      Set.member d (Select.selectedConstructed result) `shouldBe` True
      Set.member b (Select.selectedConstructed result) `shouldBe` False
      Set.member d (Select.selectedInitialized result) `shouldBe` True
      Set.member b (Select.selectedInitialized result) `shouldBe` True
      Set.member (d, Rows.Method run) (Select.selectedMembers result) `shouldBe` True
      Set.member (b, Rows.Method run) (Select.selectedMembers result) `shouldBe` False

    it "selects a witness call to its protocol backend initializer" $ do
      let protocol = top "Protocol"
          protocolInitDep = top "ProtocolInitDep"
          protocolShape = withConstructor
            (protocol,Select.StoredConstructor mempty)
            (protocolFixture protocol [protocol] [])
          witness0 = classShape d [d,protocol] []
          witnessShape = witness0
            { fixtureShape = (fixtureShape witness0)
                { Select.shapeKind = Select.WitnessShape }
            }
          witnessInit = summary
            [direct protocol (Reach.MethodRef Builtin.initKW)]
          index = mkIndex [protocolShape,witnessShape]
            [ memberRow protocol Rows.InitRest
                (summary [need protocolInitDep]) Nothing
            , memberRow d Rows.InitRest witnessInit Nothing
            ] [(protocolInitDep,mempty)]
      result <- selected index [construct d]
      Select.selectedConstructed result `shouldBe` Set.singleton d
      Select.selectedInitialized result `shouldBe` Set.fromList [d,protocol]
      Set.member (protocol,Rows.InitRest) (Select.selectedMembers result)
        `shouldBe` True
      Select.selectedTops result `shouldBe`
        Set.fromList [d,protocol,protocolInitDep]
      Select.selectProgram index [construct protocol]
        `shouldBe` Left (Select.ProtocolConstructed protocol)

    it "activates an Attr initializer only when its constructor owner runs" $ do
      let x = A.name "value"
          declarationDep = top "DeclarationDep"
          initializerDep = top "InitializerDep"
          bShape = classShape b [b] [attrDecl b x]
          dShape = classShape d [d,b] [attrDecl b x]
          attrInfo = attrRow b x
            (summary [need declarationDep])
            (Just $ summary [need initializerDep])
          index = mkIndex [bShape,dShape] [attrInfo]
            [ (declarationDep,mempty)
            , (initializerDep,mempty)
            ]
      declarationOnly <- selected index [reflect b, construct d]
      Set.member declarationDep (Select.selectedTops declarationOnly) `shouldBe` True
      Set.member initializerDep (Select.selectedTops declarationOnly) `shouldBe` False
      Set.member (b,x) (Select.selectedInstanceInitializers declarationOnly) `shouldBe` False

      withBaseInit <- selected index
        [ reflect b
        , construct d
        , direct b (Reach.MethodRef Builtin.initKW)
        ]
      Set.member initializerDep (Select.selectedTops withBaseInit) `shouldBe` True
      Set.member (b,x) (Select.selectedInstanceInitializers withBaseInit) `shouldBe` True

    it "activates a class attribute initializer without constructing an instance" $ do
      let x = A.name "class_value"
          initializerDep = top "StaticInitializerDep"
          bShape = classShape b [b] [attrDecl b x]
          attrInfo = staticAttrRow b x mempty
            (summary [need initializerDep])
          index = mkIndex [bShape] [attrInfo] [(initializerDep,mempty)]
      result <- selected index [direct b (Reach.AttrRef x)]
      Set.member initializerDep (Select.selectedTops result) `shouldBe` True
      Set.member (b,x) (Select.selectedStaticInitializers result) `shouldBe` True
      Set.member (b,x) (Select.selectedInstanceInitializers result) `shouldBe` False
      Set.member b (Select.selectedConstructed result) `shouldBe` False

    it "resolves inherited constructors through the provider shape" $ do
      let baseDep = top "BaseConstructorDep"
          derivedDep = top "DerivedConstructorDep"
          bShape = withConstructor
            (b,Select.GeneratedConstructor $ summary [need baseDep])
            (classShape b [b] [])
          dShape = withConstructor
            (b,Select.InheritedConstructor $ summary [need derivedDep])
            (classShape d [d,b] [])
          index = mkIndex [bShape,dShape] []
            [(baseDep,mempty),(derivedDep,mempty)]
      result <- selected index [construct d]
      Select.selectedInitialized result `shouldBe` Set.fromList [b,d]
      Set.member (b,Reach.MethodRef Builtin.initKW) (Select.selectedGenerated result)
        `shouldBe` True
      Set.member (d,Reach.MethodRef Builtin.initKW) (Select.selectedGenerated result)
        `shouldBe` False
      Set.member baseDep (Select.selectedTops result) `shouldBe` True
      Set.member derivedDep (Select.selectedTops result) `shouldBe` True

    it "selects a local inherited stored constructor body" $ do
      let baseDep = top "StoredBaseConstructorDep"
          bShape = withConstructor
            (b,Select.StoredConstructor $ summary [need baseDep])
            (classShape b [b] [])
          dShape = withConstructor
            (b,Select.InheritedConstructor mempty)
            (classShape d [d,b] [])
          index = mkIndex [bShape,dShape] [] [(baseDep,mempty)]
      result <- selected index [construct d]
      Select.selectedInitialized result `shouldBe` Set.fromList [b,d]
      Set.member (b,Rows.InitRest) (Select.selectedMembers result) `shouldBe` True
      Set.member baseDep (Select.selectedTops result) `shouldBe` True

    it "resolves cross-module inherited stored and generated constructors" $ do
      let providerModule = A.modName ["constructor_provider"]
          externalStored = Select.TopKey providerModule (A.name "Stored")
          externalGenerated = Select.TopKey providerModule (A.name "Generated")
          storedDep = top "ExternalStoredConstructorDep"
          generatedDep = top "ExternalGeneratedConstructorDep"
          storedShape = withConstructor
            (externalStored,Select.StoredConstructor $ summary [need storedDep])
            (classShape externalStored [externalStored] [])
          generatedShape = withConstructor
            (externalGenerated,Select.GeneratedConstructor $ summary [need generatedDep])
            (classShape externalGenerated [externalGenerated] [])
          dShape = withConstructor
            (externalStored,Select.InheritedConstructor mempty)
            (classShape d [d,externalStored] [])
          eShape = withConstructor
            (externalGenerated,Select.InheritedConstructor mempty)
            (classShape e [e,externalGenerated] [])
          index = mkIndex [storedShape,generatedShape,dShape,eShape] []
            [(storedDep,mempty),(generatedDep,mempty)]
      result <- selected index [construct d,construct e]
      Set.member (externalStored,Rows.InitRest) (Select.selectedMembers result)
        `shouldBe` True
      Set.member (externalGenerated,Reach.MethodRef Builtin.initKW)
        (Select.selectedGenerated result) `shouldBe` True
      Set.member storedDep (Select.selectedTops result) `shouldBe` True
      Set.member generatedDep (Select.selectedTops result) `shouldBe` True

    it "terminates inherited construction at an opaque builtin shape" $ do
      let builtin = Select.TopKey Builtin.mBuiltin Builtin.nObject
          builtinShape = withConstructor
            (builtin,Select.OpaqueConstructor)
            (classShape builtin [builtin] [])
          dShape = withConstructor
            (builtin,Select.InheritedConstructor mempty)
            (classShape d [d,builtin] [])
          index0 = mkIndex [builtinShape,dShape] [] []
          index = index0
            { Select.programTops = Map.insert builtin (Select.OpaqueTop mempty)
                (Select.programTops index0)
            }
      result <- selected index [construct d]
      Select.selectedInitialized result `shouldBe` Set.fromList [d,builtin]
      Set.member builtin (Select.selectedTops result) `shouldBe` False

    it "retains every concrete slot in a constructed witness dictionary" $ do
      let helper = A.name "helper"
          witnessShape0 = classShape c [c]
            [methodDecl c run,methodDecl c helper]
          witnessShape = witnessShape0
            { fixtureShape = (fixtureShape witnessShape0)
                { Select.shapeKind = Select.WitnessShape }
            }
          index = mkIndex [witnessShape]
            [methodRow c run mempty,methodRow c helper mempty] []
      result <- selected index [construct c]
      Select.selectedMembers result `shouldBe` Set.fromList
        [ (c,Rows.Method run)
        , (c,Rows.Method helper)
        , (c,Rows.InitRest)
        ]


-- Fixtures -----------------------------------------------------------------------------------------------

mn :: A.ModName
mn = A.modName ["worklist"]

top :: String -> Select.TopKey
top s = Select.TopKey mn (A.name s)

a, b, c, d, e :: Select.TopKey
a = top "A"
b = top "B"
c = top "C"
d = top "D"
e = top "E"

run :: A.Name
run = A.name "run"

methodHierarchy :: Select.ProgramIndex
methodHierarchy = mkIndex
  [ classShape b [b] [methodDecl b run]
  , classShape d [d,b] [methodDecl d run]
  ]
  [ methodRow b run mempty
  , methodRow d run mempty
  ]
  []

data ShapeFixture = ShapeFixture
  { fixtureShape       :: Select.ShapeInfo
  , fixtureSlots       :: [(Reach.MemberRef,Select.SlotInfo)]
  , fixtureReflectable :: [A.Name]
  }

classShape :: Select.TopKey
           -> [Select.TopKey]
           -> [(Reach.MemberRef,Select.SlotInfo)]
           -> ShapeFixture
classShape owner lineage slots = ShapeFixture shape slots reflectable
  where
    shape = Select.ShapeInfo
      { Select.shapeName = owner
      , Select.shapeKind = Select.ClassShape
      , Select.shapeLineage = lineage
      , Select.shapeConstructor = Just (owner,Select.StoredConstructor mempty)
      , Select.shapeAbstracts = Set.toAscList $ Set.fromList
          [ ref
          | (ref,Select.SlotInfo _ Select.AbstractSlot) <- slots
          ]
      }
    reflectable = [ name | (Reach.AttrRef name,_) <- slots ]

withConstructor :: (Select.TopKey,Select.ConstructorDecl) -> ShapeFixture -> ShapeFixture
withConstructor constructor fixture = fixture
  { fixtureShape = (fixtureShape fixture)
      { Select.shapeConstructor = Just constructor
      }
  }

protocolFixture :: Select.TopKey
                -> [Select.TopKey]
                -> [(Reach.MemberRef,Select.SlotInfo)]
                -> ShapeFixture
protocolFixture owner lineage slots = fixture
  { fixtureShape = (fixtureShape fixture)
      { Select.shapeKind = Select.ProtocolShape
      , Select.shapeConstructor = Nothing
      }
  }
  where fixture = classShape owner lineage slots

methodDecl :: Select.TopKey
           -> A.Name
           -> (Reach.MemberRef,Select.SlotInfo)
methodDecl owner n =
  (Reach.MethodRef n,Select.SlotInfo owner $ Select.StoredSlot $ Rows.Method n)

attrDecl :: Select.TopKey
         -> A.Name
         -> (Reach.MemberRef,Select.SlotInfo)
attrDecl owner n = (Reach.AttrRef n,Select.SlotInfo owner Select.AttributeSlot)

type MemberRow = (Select.TopKey,Rows.MemberKey,Select.MemberInfo)

memberRow :: Select.TopKey
          -> Rows.MemberKey
          -> Reach.ReachSummary
          -> Maybe Reach.ReachSummary
          -> MemberRow
memberRow owner key ordinary initializer =
  (owner,key,Select.MemberInfo ordinary Nothing initializer)

methodRow :: Select.TopKey -> A.Name -> Reach.ReachSummary -> MemberRow
methodRow owner n ordinary = memberRow owner (Rows.Method n) ordinary Nothing

attrRow :: Select.TopKey
        -> A.Name
        -> Reach.ReachSummary
        -> Maybe Reach.ReachSummary
        -> MemberRow
attrRow owner n = memberRow owner (Rows.Attr n)

staticAttrRow :: Select.TopKey
              -> A.Name
              -> Reach.ReachSummary
              -> Reach.ReachSummary
              -> MemberRow
staticAttrRow owner n ordinary initializer =
  (owner,Rows.Attr n,Select.MemberInfo ordinary (Just initializer) Nothing)

data LookupCounts = LookupCounts
  { countedTops        :: Map.Map Select.TopKey Int
  , countedMembers     :: Map.Map (Select.TopKey,Rows.MemberKey) Int
  , countedShapes      :: Map.Map Select.TopKey Int
  , countedSlots       :: Map.Map (Select.TopKey,Reach.MemberRef) Int
  , countedSurfaces    :: Map.Map Select.TopKey Int
  , countedReflections :: Map.Map Select.TopKey Int
  } deriving (Eq, Show)

emptyCounts :: LookupCounts
emptyCounts = LookupCounts
  Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty

countedLookup :: Select.ProgramIndex -> Select.ProgramLookup (State LookupCounts)
countedLookup index = Select.ProgramLookup
  { Select.lookupTopRow = \key -> do
      bump $ \counts -> counts
        { countedTops = Map.insertWith (+) key 1 (countedTops counts) }
      return $ Map.lookup key (Select.programTops index)
  , Select.lookupMemberRow = \owner member -> do
      bump $ \counts -> counts
        { countedMembers = Map.insertWith (+) (owner,member) 1 (countedMembers counts) }
      return $ Map.lookup (owner,member) (Select.programMembers index)
  , Select.lookupShapeRow = \key -> do
      bump $ \counts -> counts
        { countedShapes = Map.insertWith (+) key 1 (countedShapes counts) }
      return $ Map.lookup key (Select.programShapes index)
  , Select.lookupSlotRow = \receiver ref -> do
      bump $ \counts -> counts
        { countedSlots = Map.insertWith (+) (receiver,ref) 1 (countedSlots counts) }
      return $ Map.lookup (receiver,ref) (Select.programSlots index)
  , Select.lookupSurfaceSlots = \receiver -> do
      bump $ \counts -> counts
        { countedSurfaces = Map.insertWith (+) receiver 1 (countedSurfaces counts) }
      return
        [ (ref,slot)
        | ((owner,ref),slot) <- Map.toAscList (Select.programSlots index)
        , owner == receiver
        ]
  , Select.lookupReflectableAttrs = \receiver -> do
      bump $ \counts -> counts
        { countedReflections =
            Map.insertWith (+) receiver 1 (countedReflections counts) }
      return $ Map.lookup receiver (Select.programReflectableAttrs index)
  }
  where bump = modify'

mkIndex :: [ShapeFixture]
        -> [MemberRow]
        -> [(Select.TopKey,Reach.ReachSummary)]
        -> Select.ProgramIndex
mkIndex shapes givenMembers plainTops = Select.ProgramIndex
  { Select.programTops = Map.fromList
      ([ (Select.shapeName shape,Select.LocalTop Nothing mempty) | shape <- shapeRows ] ++
       [ (owner,Select.LocalTop Nothing reach) | (owner,reach) <- plainTops ])
  , Select.programMembers = Map.fromList
      [ ((owner,key),info) | (owner,key,info) <- members ]
  , Select.programShapes = Map.fromList
      [ (Select.shapeName shape,shape) | shape <- shapeRows ]
  , Select.programSlots = Map.fromList
      [ ((Select.shapeName $ fixtureShape shape,ref),info)
      | shape <- shapes
      , (ref,info) <- fixtureSlots shape
      ]
  , Select.programReflectableAttrs = Map.fromList
      [ (Select.shapeName $ fixtureShape shape,
          Select.ReflectableAttrs $ fixtureReflectable shape)
      | shape <- shapes
      ]
  }
  where
    shapeRows = map fixtureShape shapes
    supplied = Set.fromList [ (owner,key) | (owner,key,_) <- givenMembers ]
    constructorOwners = Set.fromList
      [ owner
      | shape <- shapeRows
      , Just (owner,Select.StoredConstructor _) <- [Select.shapeConstructor shape]
      ]
    defaults =
      [ memberRow owner Rows.InitRest mempty Nothing
      | owner <- Set.toAscList constructorOwners
      , Set.notMember (owner,Rows.InitRest) supplied
      ]
    members = givenMembers ++ defaults

summary :: [Reach.ReachEdge] -> Reach.ReachSummary
summary = Reach.reachSummaryFromEdges

need :: Select.TopKey -> Reach.ReachEdge
need (Select.TopKey m n) = Reach.Need m n

declare :: Select.TopKey -> Reach.ReachEdge
declare (Select.TopKey m n) = Reach.Declare m n

declareAttr :: Select.TopKey -> A.Name -> Reach.ReachEdge
declareAttr (Select.TopKey m n) = Reach.DeclareAttr m n

construct :: Select.TopKey -> Reach.ReachEdge
construct (Select.TopKey m n) = Reach.Construct m n

direct :: Select.TopKey -> Reach.MemberRef -> Reach.ReachEdge
direct (Select.TopKey m n) = Reach.Direct m n

dispatch :: Select.TopKey -> A.Name -> Reach.ReachEdge
dispatch (Select.TopKey m n) member = Reach.Dispatch m n (Reach.MethodRef member)

dispatchRef :: Select.TopKey -> Reach.MemberRef -> Reach.ReachEdge
dispatchRef (Select.TopKey m n) = Reach.Dispatch m n

reflect :: Select.TopKey -> Reach.ReachEdge
reflect (Select.TopKey m n) = Reach.Reflect m n

selected :: Select.ProgramIndex -> [Reach.ReachEdge] -> IO Select.Selection
selected index seeds =
  case Select.selectProgram index seeds of
    Left err -> expectationFailure ("selection failed: " ++ show err)
    Right result -> return result
