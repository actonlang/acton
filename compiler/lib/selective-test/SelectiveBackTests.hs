-- SPDX-License-Identifier: BSD-3-Clause

module SelectiveBackTests (tests) where

import qualified Acton.Env as Env
import qualified Acton.InterfaceRows as Rows
import qualified Acton.InterfaceRowsBuilder as RowsBuilder
import qualified Acton.NameInfo as I
import qualified Acton.QuickType as QuickType
import qualified Acton.ReachabilityRows as ReachRows
import qualified Acton.ReachabilityRowsBuilder as ReachBuilder
import qualified Acton.ReachabilityTypes as Reach
import qualified Acton.SelectiveBack as Selective
import qualified Acton.SelectiveWorklist as Worklist
import qualified Acton.Syntax as A
import qualified InterfaceFiles

import qualified Control.Exception as E
import Control.Monad (when)
import qualified Data.ByteString.Char8 as B
import Data.IORef (atomicModifyIORef', newIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Syd
import Utils (SrcLoc(..))


tests :: Spec
tests = do
  describe "selective projection" $ do
    it "keeps projection hashes independent of declaration docs" $ do
      let before = projectionHash (functionModule (Just "before") [A.Pass NoLoc])
          after = projectionHash (functionModule (Just "after") [A.Pass NoLoc])
      before `shouldBe` after

    it "changes projection hashes for selected bodies and opaque inputs" $ do
      let passHash = projectionHash (functionModule Nothing [A.Pass NoLoc])
          returnHash = projectionHash
            (functionModule Nothing [A.Return NoLoc $ Just $ A.None NoLoc])
          opaqueA = Selective.projectionUniverseHash emptySelection []
            [(fixtureKey,B.pack "opaque-a")]
          opaqueB = Selective.projectionUniverseHash emptySelection []
            [(fixtureKey,B.pack "opaque-b")]
      passHash `shouldNotBe` returnHash
      opaqueA `shouldNotBe` opaqueB

    it "distinguishes structurally different names in projection hashes" $ do
      let plain = A.name "ownerD_part"
          derived = A.Derived (A.name "owner") (A.name "part")
          selected name = emptySelection
            { Worklist.selectedOpaqueTops =
                Set.singleton (ReachRows.TopKey fixtureModule name)
            }
          hash name = Selective.projectionUniverseHash (selected name) [] []
      A.nstr plain `shouldBe` A.nstr derived
      hash plain `shouldNotBe` hash derived

    it "projects explicit imports to selected provider names" $ do
      let provider = A.modName ["provider"]
          qualified = A.modName ["qualified"]
          wildcard = A.modName ["wildcard"]
          used = A.name "used"
          declared = A.name "declared"
          opaque = A.name "opaque"
          unused = A.name "unused"
          wildcardUsed = A.name "wildcard_used"
          wildcardPrivate = A.name "_wildcard_private"
          alias = A.name "alias"
          qualifiedAlias = A.modName ["q"]
          selection = emptySelection
            { Worklist.selectedTops = Set.singleton
                (ReachRows.TopKey provider used)
            , Worklist.selectedDeclarations = Set.singleton
                (ReachRows.TopKey provider declared)
            , Worklist.selectedOpaqueTops = Set.singleton
                (ReachRows.TopKey provider opaque)
                `Set.union` Set.fromList
                  [ ReachRows.TopKey wildcard wildcardUsed
                  , ReachRows.TopKey wildcard wildcardPrivate
                  ]
            }
          imports =
            [ A.Import NoLoc [A.ModuleItem qualified (Just qualifiedAlias)]
            , A.FromImport NoLoc provider
                [ A.ImportItem used Nothing
                , A.ImportItem declared Nothing
                , A.ImportItem opaque (Just alias)
                , A.ImportItem unused Nothing
                ]
            , A.FromImport NoLoc qualified [A.ImportItem unused Nothing]
            , A.FromImportAll NoLoc wildcard
            ]
      Selective.projectImports selection imports `shouldBe`
        [ A.Import NoLoc [A.ModuleItem qualified (Just qualifiedAlias)]
        , A.FromImport NoLoc provider
            [ A.ImportItem used Nothing
            , A.ImportItem declared Nothing
            , A.ImportItem opaque (Just alias)
            ]
        , A.FromImport NoLoc qualified []
        , A.FromImportAll NoLoc wildcard
        ]

    it "narrows wildcard enumeration without changing importability" $ do
      base <- Env.initEnv "" True
      let provider = A.modName ["wildcard_provider"]
          value = A.name "value"
          extension = A.name "Extension"
          unused = A.name "unused"
          target = A.TC (A.GName provider $ A.name "Target") []
          protocol = A.TC (A.GName provider $ A.name "Protocol") []
          info = Env.mkModuleInfo provider []
            [ (value,I.NVar A.tWild)
            , (extension,I.NExt [] target [([],protocol)] [] [] Nothing)
            , (unused,I.NVar A.tWild)
            ] Nothing
          selection = emptySelection
            { Worklist.selectedOpaqueTops = Set.fromList
                [ ReachRows.TopKey provider value
                , ReachRows.TopKey provider extension
                ]
            }
          narrowed = Selective.restrictEnvironmentPublicNames selection $
            Env.addModuleInfo provider info base
          narrowedInfo = case Env.lookupModuleInfo provider narrowed of
            Just moduleInfo -> moduleInfo
            Nothing -> error "missing narrowed module"
          imported = Env.importAll provider narrowedInfo narrowed
      Env.modulePublicNames narrowedInfo `shouldBe` [value,extension]
      Env.lookupName value imported `shouldBe`
        Just (I.NAlias $ A.GName provider value)
      Env.lookupName extension imported `shouldBe` Nothing

    it "retains exact imported witness indexes on a projected module" $ do
      base <- Env.initEnv "" True
      let protocol = A.TC (A.GName (A.modName ["protocols"]) $ A.name "Renderable") []
          host = A.TC (A.GName fixtureModule $ A.name "Host") []
          extension = I.NExt [] host [([],protocol)] [] [] Nothing
          original = Env.mkModuleInfo fixtureModule [] [(fixtureName,extension)] Nothing
          env = Env.addModuleInfo fixtureModule original base
          projection = Selective.Projection
            (A.Module fixtureModule [] Nothing []) [] [] [] 0 0
          installed = Selective.projectionModuleInfo env projection
      Env.modulePublicNames installed `shouldBe` []
      Env.moduleWitnessesByProto installed (A.tcname protocol)
        `shouldSatisfy` (not . null)
      Env.moduleWitnessesByType installed (A.tcname host)
        `shouldSatisfy` (not . null)

    it "rejects a selected container without its inferred header" $ do
      withSystemTempDirectory "selective-back" $ \dir -> do
        (mn,name,nmod,rows,reachRows) <- containerFixture
        let key = ReachRows.TopKey mn name
            brokenRows = reachRows
              { ReachRows.reachTopRows = Map.adjust dropHeader key
                  (ReachRows.reachTopRows reachRows)
              }
            nameHash = InterfaceFiles.NameHashInfo name
              (B.pack "src") (B.pack "pub") (B.pack "impl") (B.pack "own")
              [] [] [] [] []
            tyFile = dir </> "fixture.tydb"
        InterfaceFiles.writeFile (\_ -> return ()) tyFile InterfaceFiles.InterfaceContents
          { InterfaceFiles.ifcSourceHash = B.pack "src"
          , InterfaceFiles.ifcPublicHash = B.pack "pub"
          , InterfaceFiles.ifcImplementationHash = B.pack "impl"
          , InterfaceFiles.ifcModuleHashInfo = InterfaceFiles.emptyModuleHashInfo
          , InterfaceFiles.ifcSourceMeta = Nothing
          , InterfaceFiles.ifcImports = []
          , InterfaceFiles.ifcDependencies = []
          , InterfaceFiles.ifcNameHashes = [nameHash]
          , InterfaceFiles.ifcRoots = []
          , InterfaceFiles.ifcTests = []
          , InterfaceFiles.ifcDoc = Nothing
          , InterfaceFiles.ifcModule = nmod
          , InterfaceFiles.ifcRows = rows
          , InterfaceFiles.ifcReachabilityRows = brokenRows
          }
        selected <- Selective.selectFromInterfaces
          (\_ -> return [tyFile]) (Set.singleton mn) [Reach.Need mn name]
        program <- either (error . show) return selected
        outcome <- E.try
          (Selective.materializeInterfaceProjection (\_ -> return [tyFile]) mn program)
          :: IO (Either Selective.SelectiveBackError Selective.Projection)
        case outcome of
          Left (Selective.MissingSelectedHeader _ missing) -> missing `shouldBe` key
          Left err -> expectationFailure ("unexpected selective error: " ++ show err)
          Right projection -> expectationFailure
            ("missing container header produced projection: " ++ show projection)

    it "rejects ambiguous interface providers without widening" $ do
      outcome <- E.try $ Selective.selectFromInterfaces
        (\_ -> return ["/first.tydb","/second.tydb"])
        (Set.singleton fixtureModule) [Reach.Need fixtureModule fixtureName]
      case outcome of
        Left (Selective.AmbiguousInterfaceModule mn paths) -> do
          mn `shouldBe` fixtureModule
          paths `shouldBe` ["/first.tydb","/second.tydb"]
        Left err -> expectationFailure ("unexpected selective error: " ++ show err)
        Right result -> expectationFailure
          ("ambiguous interfaces produced selection: " ++ show result)

    it "classifies declarations outside deferred modules without row reads" $ do
      result <- Selective.selectFromInterfaces
        (\_ -> error "opaque declaration performed an interface read")
        Set.empty [Reach.Declare fixtureModule fixtureName]
      case result of
        Left err -> expectationFailure ("opaque declaration selection failed: " ++ show err)
        Right program -> do
          let selection = Selective.selectedProgramSelection program
          Worklist.selectedDeclarations selection `shouldBe` Set.empty
          Worklist.selectedOpaqueTops selection `shouldBe` Set.singleton fixtureKey

    it "rejects an interface generation change during selection" $ do
      withSystemTempDirectory "selective-back" $ \dir -> do
        (mn,name,nmod,rows,reachRows) <- containerFixture
        let tyFile = dir </> "fixture.tydb"
            write impl = writeContainerInterface tyFile impl nmod rows reachRows
        write (B.pack "impl-a")
        calls <- newIORef (0 :: Int)
        let resolve _ = do
              call <- atomicModifyIORef' calls $ \n -> (n + 1,n)
              when (call == 2) (write $ B.pack "impl-b")
              return [tyFile]
        outcome <- E.try $ Selective.selectFromInterfaces
          resolve (Set.singleton mn) [Reach.Need mn name]
        case outcome of
          Left (Selective.ChangedInterfaceSnapshot changed path) -> do
            changed `shouldBe` mn
            path `shouldBe` tyFile
          Left err -> expectationFailure ("unexpected selective error: " ++ show err)
          Right result -> expectationFailure
            ("changed interface produced selection: " ++ show result)

    it "validates every explicitly captured interface" $ do
      withSystemTempDirectory "selective-back" $ \dir -> do
        (mn,_name,nmod,rows,reachRows) <- containerFixture
        let tyFile = dir </> "fixture.tydb"
            stable _ = return [tyFile]
            write impl = writeContainerInterface tyFile impl nmod rows reachRows
        write (B.pack "impl-a")
        snapshots <- Selective.captureInterfaceSnapshots stable (Set.singleton mn)
        write (B.pack "impl-b")
        outcome <- E.try $
          Selective.validateInterfaceSnapshots stable snapshots
        case outcome of
          Left (Selective.ChangedInterfaceSnapshot changed path) -> do
            changed `shouldBe` mn
            path `shouldBe` tyFile
          Left err -> expectationFailure ("unexpected selective error: " ++ show err)
          Right () -> expectationFailure "changed explicit interface passed validation"

    it "binds lazy environments to public interface changes" $ do
      withSystemTempDirectory "selective-back" $ \dir -> do
        (mn,_name,nmod,rows,reachRows) <- containerFixture
        let tyFile = dir </> "fixture.tydb"
            stable _ = return [tyFile]
            write pub impl =
              writeContainerInterfaceHashes tyFile pub impl nmod rows reachRows
            bound snapshots = Selective.bindInterfaceSnapshots (B.pack "projection") snapshots
        write (B.pack "pub-a") (B.pack "impl-a")
        hashA <- bound <$> Selective.captureInterfaceSnapshots stable (Set.singleton mn)
        write (B.pack "pub-a") (B.pack "impl-b")
        hashImpl <- bound <$> Selective.captureInterfaceSnapshots stable (Set.singleton mn)
        write (B.pack "pub-b") (B.pack "impl-b")
        hashPub <- bound <$> Selective.captureInterfaceSnapshots stable (Set.singleton mn)
        hashImpl `shouldBe` hashA
        hashPub `shouldNotBe` hashA

    it "binds lazy environments to ordered source imports" $ do
      withSystemTempDirectory "selective-back" $ \dir -> do
        (mn,_name,nmod,rows,reachRows) <- containerFixture
        let tyFile = dir </> "fixture.tydb"
            stable _ = return [tyFile]
            first = A.modName ["first"]
            second = A.modName ["second"]
            write imports = writeContainerInterface
              tyFile (B.pack "impl") nmod
              rows{ Rows.rowImports =
                [A.Import NoLoc [A.ModuleItem imported Nothing] | imported <- imports]
              }
              reachRows
            bound snapshots = Selective.bindInterfaceSnapshots
              (B.pack "projection") snapshots
        write [first,second]
        hashFirst <- bound <$> Selective.captureInterfaceSnapshots
          stable (Set.singleton mn)
        write [second,first]
        hashSecond <- bound <$> Selective.captureInterfaceSnapshots
          stable (Set.singleton mn)
        hashSecond `shouldNotBe` hashFirst

    it "detects generation drift after an explicit ABA rewrite" $ do
      withSystemTempDirectory "selective-back" $ \dir -> do
        (mn,_name,nmod,rows,reachRows) <- containerFixture
        let tyFile = dir </> "fixture.tydb"
            stable _ = return [tyFile]
            write impl = writeContainerInterface tyFile impl nmod rows reachRows
        write (B.pack "impl-a")
        snapshots <- Selective.captureInterfaceSnapshots stable (Set.singleton mn)
        write (B.pack "impl-b")
        write (B.pack "impl-a")
        outcome <- E.try $
          Selective.validateInterfaceSnapshots stable snapshots
        case outcome of
          Left (Selective.ChangedInterfaceSnapshot changed path) -> do
            changed `shouldBe` mn
            path `shouldBe` tyFile
          Left err -> expectationFailure ("unexpected selective error: " ++ show err)
          Right () -> expectationFailure "ABA explicit interface rewrite passed validation"

    it "rejects an ABA interface rewrite with the original module hashes" $ do
      withSystemTempDirectory "selective-back" $ \dir -> do
        (mn,name,nmod,rows,reachRows) <- containerFixture
        let tyFile = dir </> "fixture.tydb"
            write impl = writeContainerInterface tyFile impl nmod rows reachRows
        write (B.pack "impl-a")
        calls <- newIORef (0 :: Int)
        let resolve _ = do
              call <- atomicModifyIORef' calls $ \n -> (n + 1,n)
              when (call == 2) $ do
                write (B.pack "impl-b")
                write (B.pack "impl-a")
              return [tyFile]
        outcome <- E.try $ Selective.selectFromInterfaces
          resolve (Set.singleton mn) [Reach.Need mn name]
        case outcome of
          Left (Selective.ChangedInterfaceSnapshot changed path) -> do
            changed `shouldBe` mn
            path `shouldBe` tyFile
          Left err -> expectationFailure ("unexpected selective error: " ++ show err)
          Right result -> expectationFailure
            ("ABA interface rewrite produced selection: " ++ show result)

    it "rejects an interface generation change during projection" $ do
      withSystemTempDirectory "selective-back" $ \dir -> do
        (mn,name,nmod,rows,reachRows) <- containerFixture
        let tyFile = dir </> "fixture.tydb"
            stable _ = return [tyFile]
            write impl = writeContainerInterface tyFile impl nmod rows reachRows
        write (B.pack "impl-a")
        selected <- Selective.selectFromInterfaces
          stable (Set.singleton mn) [Reach.Need mn name]
        program <- either (error . show) return selected
        calls <- newIORef (0 :: Int)
        let changing _ = do
              call <- atomicModifyIORef' calls $ \n -> (n + 1,n)
              when (call == 1) (write $ B.pack "impl-b")
              return [tyFile]
        outcome <- E.try $
          Selective.materializeInterfaceProjection changing mn program
        case outcome of
          Left (Selective.ChangedInterfaceSnapshot changed path) -> do
            changed `shouldBe` mn
            path `shouldBe` tyFile
          Left err -> expectationFailure ("unexpected selective error: " ++ show err)
          Right projection -> expectationFailure
            ("changed interface produced projection: " ++ show projection)

    it "rejects an interface generation change while reading opaque hashes" $ do
      withSystemTempDirectory "selective-back" $ \dir -> do
        (mn,name,nmod,rows,reachRows) <- containerFixture
        let tyFile = dir </> "fixture.tydb"
            write impl = writeContainerInterface tyFile impl nmod rows reachRows
        write (B.pack "impl-a")
        selected <- Selective.selectFromInterfaces
          (\_ -> error "opaque selection resolved an interface")
          Set.empty [Reach.Need mn name]
        program <- either (error . show) return selected
        calls <- newIORef (0 :: Int)
        let changing _ = do
              call <- atomicModifyIORef' calls $ \n -> (n + 1,n)
              when (call == 2) (write $ B.pack "impl-b")
              return [tyFile]
        outcome <- E.try $ snd <$>
          Selective.captureSelectedOpaqueHashes changing program
        case outcome of
          Left (Selective.ChangedInterfaceSnapshot changed path) -> do
            changed `shouldBe` mn
            path `shouldBe` tyFile
          Left err -> expectationFailure ("unexpected selective error: " ++ show err)
          Right hashes -> expectationFailure
            ("changed interface produced opaque hashes: " ++ show hashes)

    it "keeps opaque interfaces in the back-pass snapshot" $ do
      withSystemTempDirectory "selective-back" $ \dir -> do
        (mn,name,nmod,rows,reachRows) <- containerFixture
        let tyFile = dir </> "fixture.tydb"
            stable _ = return [tyFile]
            write impl = writeContainerInterface tyFile impl nmod rows reachRows
        write (B.pack "impl-a")
        selected <- Selective.selectFromInterfaces
          (\_ -> error "opaque selection resolved an interface")
          Set.empty [Reach.Need mn name]
        program0 <- either (error . show) return selected
        (program,_) <- Selective.captureSelectedOpaqueHashes stable program0
        write (B.pack "impl-b")
        outcome <- E.try $ Selective.validateSelectedProgram stable program
        case outcome of
          Left (Selective.ChangedInterfaceSnapshot changed path) -> do
            changed `shouldBe` mn
            path `shouldBe` tyFile
          Left err -> expectationFailure ("unexpected selective error: " ++ show err)
          Right () -> expectationFailure "changed opaque interface passed validation"


fixtureModule :: A.ModName
fixtureModule = A.modName ["selective_fixture"]

fixtureName :: A.Name
fixtureName = A.name "Thing"

fixtureKey :: ReachRows.TopKey
fixtureKey = ReachRows.TopKey fixtureModule fixtureName

emptySelection :: Worklist.Selection
emptySelection = case Worklist.selectProgram Worklist.emptyProgramIndex [] of
  Left err -> error (show err)
  Right selection -> selection

functionModule :: Maybe String -> A.Suite -> A.Module
functionModule doc body = A.Module fixtureModule [] Nothing
  [ A.Decl NoLoc
      [ A.Def NoLoc fixtureName [] A.PosNIL A.KwdNIL (Just A.tNone)
          body A.NoDec A.fxPure doc
      ]
  ]

projectionHash :: A.Module -> B.ByteString
projectionHash typed =
  Selective.projectionUniverseHash emptySelection [projection] []
  where
    projection = Selective.Projection
      { Selective.projectionModule = typed
      , Selective.projectionHeaders = []
      , Selective.projectionDeclarations = []
      , Selective.projectionTypeEnv = QuickType.envOfTopSuite (A.mbody typed)
      , Selective.projectionTopCount = 1
      , Selective.projectionMemberCount = 0
      }

dropHeader :: ReachRows.TopInfo -> ReachRows.TopInfo
dropHeader (ReachRows.LocalTop _ summary) = ReachRows.LocalTop Nothing summary
dropHeader info = info

writeContainerInterface :: FilePath
                        -> B.ByteString
                        -> I.NModule
                        -> Rows.InterfaceRows
                        -> ReachRows.ReachabilityRows
                        -> IO ()
writeContainerInterface tyFile impl nmod rows reachRows =
    writeContainerInterfaceHashes tyFile (B.pack "pub") impl nmod rows reachRows

writeContainerInterfaceHashes :: FilePath
                              -> B.ByteString
                              -> B.ByteString
                              -> I.NModule
                              -> Rows.InterfaceRows
                              -> ReachRows.ReachabilityRows
                              -> IO ()
writeContainerInterfaceHashes tyFile pub impl nmod rows reachRows =
    InterfaceFiles.writeFile (\_ -> return ()) tyFile InterfaceFiles.InterfaceContents
      { InterfaceFiles.ifcSourceHash = B.pack "src"
      , InterfaceFiles.ifcPublicHash = pub
      , InterfaceFiles.ifcImplementationHash = impl
      , InterfaceFiles.ifcModuleHashInfo = InterfaceFiles.emptyModuleHashInfo
      , InterfaceFiles.ifcSourceMeta = Nothing
      , InterfaceFiles.ifcImports = []
      , InterfaceFiles.ifcDependencies = []
      , InterfaceFiles.ifcNameHashes = [nameHash]
      , InterfaceFiles.ifcRoots = []
      , InterfaceFiles.ifcTests = []
      , InterfaceFiles.ifcDoc = Nothing
      , InterfaceFiles.ifcModule = nmod
      , InterfaceFiles.ifcRows = rows
      , InterfaceFiles.ifcReachabilityRows = reachRows
      }
  where
    nameHash = InterfaceFiles.NameHashInfo fixtureName
      (B.pack "src") pub impl (B.pack "own")
      [] [] [] [] []

containerFixture :: IO
  ( A.ModName
  , A.Name
  , I.NModule
  , Rows.InterfaceRows
  , ReachRows.ReachabilityRows
  )
containerFixture = do
  base <- Env.initEnv "" True
  let mn = fixtureModule
      name = fixtureName
      decl = A.Class NoLoc name [] [] [] Nothing
      typed = A.Module mn [] Nothing [A.Decl NoLoc [decl]]
      tenv = QuickType.envOfTopSuite (A.mbody typed)
      env = Env.define tenv $ Env.setMod mn base
      nmod = I.NModule [] tenv Nothing
  rows <- either (error . show) return $ RowsBuilder.prepareInterfaceRows env typed
  reachRows <- either (error . show) return $
    ReachBuilder.prepareReachabilityRows env tenv typed rows
  return (mn,name,nmod,rows,reachRows)
