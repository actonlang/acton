-- SPDX-License-Identifier: BSD-3-Clause

module WitnessForwardingTests (tests) where

import qualified Acton.Builtin as Builtin
import qualified Acton.CodeGen as CodeGen
import qualified Acton.Converter as Converter
import qualified Acton.Env as Env
import qualified Acton.InterfaceRowsBuilder as InterfaceRows
import qualified Acton.NameInfo as I
import qualified Acton.QuickType as QuickType
import qualified Acton.ReachabilityRows as ReachRows
import qualified Acton.ReachabilityRowsBuilder as ReachBuilder
import qualified Acton.ReachabilityTypes as Reach
import qualified Acton.SelectiveBack as Selective
import qualified Acton.Syntax as A
import qualified Acton.WitnessForwarding as Forward

import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.Syd
import Utils (SrcLoc(..))


tests :: Spec
tests = describe "witness slot forwarding" $ do
    it "uses a sibling witness that covers the same protocol slot" $ do
      (env,target,provider,unrelated,n) <- localFixture
      let context = Forward.buildForwardContext env [unrelated,provider,A.noq $ A.tcname target]
      fmap (A.tcname . Forward.forwardProviderType) (Forward.forwardPlan env context target n)
        `shouldBe` Just (A.NoQ provider)

    it "does not use an unrelated class with the same concrete slot" $ do
      (env,target,_,unrelated,n) <- localFixture
      let context = Forward.buildForwardContext env [unrelated,A.noq $ A.tcname target]
      Forward.forwardPlan env context target n `shouldBe` Nothing

    it "reads projected imported providers through the exact witness index" $ do
      (env,target,provider,n,_) <- projectedImportedFixture
      let context = Forward.buildForwardContext env [A.noq $ A.tcname target]
      fmap (A.tcname . Forward.forwardProviderRoot) (Forward.forwardPlan env context target n)
        `shouldBe` Just provider

    it "emits forwarding wrappers from a converted selective projection" $ do
      (env,_,_,_,modu) <- projectedImportedFixture
      (_,_,implementation) <- CodeGen.generate env [] "" "" False modu "test"
      implementation `shouldSatisfy` isInfixOf "$forward"

    it "emits the forwarded witness constructor declaration and body" $ do
      (env,_,_,_,modu) <- projectedImportedFixture
      (_,header,implementation) <- CodeGen.generate env [] "" "" False modu "test"
      let constructor = "witness_forwarding_consumerQ_TargetD_IndexedHostG_new"
      header `shouldSatisfy` isInfixOf (constructor ++ "();")
      implementation `shouldSatisfy` isInfixOf (constructor ++ "() {")

    it "attaches forwarding provider construction to the target top" $ do
      (env,target,provider,modu) <- localReachabilityFixture
      stored <- either (error . show) return $
        InterfaceRows.prepareInterfaceRows env modu
      prepared <- either (error . show) return $
        ReachBuilder.prepareReachabilityRows env
          (QuickType.envOfTopSuite $ A.mbody modu) modu stored
      let targetKey = ReachRows.TopKey (A.modname modu) (A.noq $ A.tcname target)
          edges = topEdges prepared targetKey
      Set.member (Reach.Need (A.modname modu) provider) edges `shouldBe` True
      Set.member (Reach.Construct (A.modname modu) provider) edges `shouldBe` True

    it "emits declaration-only header types without C definitions" $ do
      base <- Env.initEnv "" True
      let mn = A.modName ["declaration_header"]
          n = A.name "ForwardOnly"
          builtin = Env.mkModuleInfo Builtin.mBuiltin [] [] Nothing
          env = Env.addModuleInfo Builtin.mBuiltin builtin base
          modu = A.Module mn [] Nothing []
          symbol = "declaration_headerQ_ForwardOnly"
      (_,header,implementation) <- CodeGen.generate env [n] "" "" False modu "test"
      header `shouldSatisfy` isInfixOf ("struct " ++ symbol ++ ";")
      header `shouldSatisfy` isInfixOf ("typedef struct " ++ symbol ++ " *" ++ symbol ++ ";")
      implementation `shouldNotSatisfy` isInfixOf symbol

    it "keeps normalized witnesses out of class tables" $ do
      base <- Env.initEnv "" True
      let mn = A.modName ["witness_class_table"]
          host = A.name "Host"
          value = A.name "value"
          witness = A.Internal A.Witness "Host" 1
          hostTc = A.TC (A.NoQ host) []
          initType = A.tFun A.fxPure
            (A.posRow (A.tCon hostTc) A.posNil) A.kwdNil A.tNone
          initInfo = I.NDef (A.monotype initType) A.NoDec Nothing
          initDef = A.Def NoLoc Builtin.initKW []
            (A.PosPar Builtin.selfKW (Just $ A.tCon hostTc) Nothing A.PosNIL)
            A.KwdNIL (Just A.tNone) [A.Pass NoLoc]
            A.NoDec A.fxPure Nothing
          assign name typ expr = A.Assign NoLoc
            [A.PVar NoLoc name $ Just typ] expr
          body =
            [ A.Decl NoLoc [initDef]
            , assign witness Builtin.tValue (A.None NoLoc)
            , assign value Builtin.tInt (A.Int NoLoc 1 "1")
            ]
          decl = A.Class NoLoc host [] [] body Nothing
          modu = A.Module mn [] Nothing [A.Decl NoLoc [decl]]
          info = I.NClass [] []
            [ (Builtin.initKW,initInfo)
            , (witness,I.NVar Builtin.tValue)
            , (value,I.NVar Builtin.tInt)
            ] Nothing
          env = Env.define [(host,info)] $ Env.setMod mn base
      (_,header,implementation) <- CodeGen.generate env [] "" "" False modu "test"
      header `shouldSatisfy` isInfixOf "int64_t value;"
      header `shouldNotSatisfy` isInfixOf "W_Host_1"
      implementation `shouldNotSatisfy` isInfixOf ".W_Host_1 ="


localFixture :: IO (Env.Env0,A.TCon,A.Name,A.Name,A.Name)
localFixture = do
    base <- Env.initEnv "" True
    let mn = A.modName ["witness_forwarding"]
        eq = A.name "EqSlot"
        hashable = A.name "HashableSlot"
        ordered = A.name "OrderedSlot"
        host = A.name "Host"
        target = A.Derived hashable host
        provider = A.Derived ordered host
        unrelated = A.Derived (A.name "Unrelated") host
        n = A.name "same"
        eqTc = A.TC (A.NoQ eq) []
        hashTc = A.TC (A.NoQ hashable) []
        orderedTc = A.TC (A.NoQ ordered) []
        classInfo bases members = I.NClass [] (A.leftpath bases) members Nothing
        rootInfo bases members = classInfo bases ((Builtin.initKW,concreteMethod) : members)
        local =
          [ (eq,classInfo [] [(n,abstractMethod)])
          , (hashable,classInfo [eqTc] [])
          , (ordered,classInfo [eqTc] [])
          , (target,rootInfo [hashTc,eqTc] [])
          , (provider,rootInfo [orderedTc,eqTc] [(n,concreteMethod)])
          , (unrelated,rootInfo [] [(n,concreteMethod)])
          ]
        builtin = Env.mkModuleInfo Builtin.mBuiltin [] [] Nothing
        env = Env.define local $ Env.setMod mn $ Env.addModuleInfo Builtin.mBuiltin builtin base
    return (env,A.TC (A.NoQ target) [],provider,unrelated,n)

localReachabilityFixture :: IO (Env.Env0,A.TCon,A.Name,A.Module)
localReachabilityFixture = do
    (env,target,provider,_,n) <- localFixture
    let mn = A.modName ["witness_forwarding"]
        A.Derived hashable host = A.noq $ A.tcname target
        A.Derived ordered _ = provider
        eq = A.name "EqSlot"
        eqTc = A.TC (A.NoQ eq) []
        hashTc = A.TC (A.NoQ hashable) []
        orderedTc = A.TC (A.NoQ ordered) []
        providerTc = A.TC (A.NoQ provider) []
        constructor tc = A.Def NoLoc Builtin.initKW []
          (A.PosPar Builtin.selfKW (Just $ A.tCon tc) Nothing A.PosNIL)
          A.KwdNIL (Just A.tNone) [A.Pass NoLoc] A.NoDec A.fxPure Nothing
        same = A.Def NoLoc n []
          (A.PosPar Builtin.selfKW (Just $ A.tCon providerTc) Nothing A.PosNIL)
          A.KwdNIL (Just A.tNone) [A.Pass NoLoc] A.NoDec A.fxPure Nothing
        targetDecl = A.Class NoLoc (A.noq $ A.tcname target) [] [hashTc,eqTc]
          [A.Decl NoLoc [constructor target]] Nothing
        providerDecl = A.Class NoLoc provider [] [orderedTc,eqTc]
          [A.Decl NoLoc [constructor providerTc,same]] Nothing
        modu = A.Module mn [] Nothing [A.Decl NoLoc [targetDecl,providerDecl]]
    return (env,target,provider,modu)

projectedImportedFixture :: IO (Env.Env0,A.TCon,A.QName,A.Name,A.Module)
projectedImportedFixture = do
    base <- Env.initEnv "" True
    let mn = A.modName ["witness_forwarding_consumer"]
        imported = A.modName ["witness_forwarding_provider"]
        eq = A.name "IndexedEq"
        host = A.name "IndexedHost"
        target = A.Derived (A.name "Target") host
        provider = A.Derived eq host
        n = A.name "same"
        hostTc = A.TC (A.GName Builtin.mBuiltin host) []
        eqTc = A.TC (A.GName Builtin.mBuiltin eq) [A.tCon hostTc]
        convertedEqTc = A.TC (A.tcname eqTc) [A.tCon hostTc,A.tCon hostTc]
        convertedMethodSchema = A.monotype $ A.tFun A.fxPure
          (A.posRow (A.tCon hostTc) A.posNil) A.kwdNil A.tNone
        targetTc = A.TC (A.NoQ target) []
        targetInit = A.Def NoLoc Builtin.initKW []
          (A.PosPar Builtin.selfKW (Just $ A.tCon targetTc) Nothing A.PosNIL)
          A.KwdNIL (Just A.tNone) [A.Pass NoLoc] A.NoDec A.fxPure Nothing
        targetDecl = A.Class NoLoc target [] [convertedEqTc]
          [A.Decl NoLoc [targetInit]] Nothing
        targetInfo = I.NClass [] (A.leftpath [convertedEqTc])
          [(Builtin.initKW,concreteMethod)] Nothing
        builtin = Env.mkModuleInfo Builtin.mBuiltin []
          [ (Builtin.nValue,I.NClass [] [] [] Nothing)
          , (host,I.NClass [] [] [] Nothing)
          , (eq,I.NClass [] [] [(n,I.NSig convertedMethodSchema A.NoDec Nothing)] Nothing)
          ] Nothing
        extensionInfo = I.NExt [] hostTc [([],eqTc)]
          [(n,concreteMethod)] [] Nothing
        indexed = Env.mkModuleInfo imported [] [(provider,extensionInfo)] Nothing
        originalEnv = Env.define [(target,targetInfo)] $
          Env.setMod mn $
          Env.addImport imported $
          Env.addModuleInfo imported indexed $
          Env.addModuleInfo Builtin.mBuiltin builtin base
        projection = Selective.Projection
          (A.Module imported [] Nothing []) [] [] [(provider,extensionInfo)] 1 1
        projected = Selective.projectionModuleInfo originalEnv projection
        env = Converter.convEnvProtos $
          Env.addModuleInfo imported projected originalEnv
        modu = A.Module mn
          [A.Import NoLoc [A.ModuleItem imported Nothing]]
          Nothing [A.Decl NoLoc [targetDecl]]
    return (env,targetTc,A.GName imported provider,n,modu)

methodSchema :: A.TSchema
methodSchema = A.monotype $ A.tFun A.fxPure A.posNil A.kwdNil A.tNone

abstractMethod :: I.NameInfo
abstractMethod = I.NSig methodSchema A.NoDec Nothing

concreteMethod :: I.NameInfo
concreteMethod = I.NDef methodSchema A.NoDec Nothing

topEdges :: ReachRows.ReachabilityRows -> ReachRows.TopKey -> Set.Set Reach.ReachEdge
topEdges rows key = case Map.lookup key (ReachRows.reachTopRows rows) of
    Just (ReachRows.LocalTop _ summary) -> Set.fromList $ Reach.reachEdges summary
    info -> error ("missing local top reach row " ++ show key ++ ": " ++ show info)
