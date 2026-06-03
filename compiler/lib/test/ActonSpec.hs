{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main (main) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.Async (async, mapConcurrently_, wait)
import Data.Char (toLower, isAlphaNum)

import qualified Acton.Parser as P
import qualified Acton.Syntax as S
import qualified Acton.Builtin as Builtin
import qualified Acton.NameInfo as I
import qualified Acton.Printer as AP
import qualified Acton.DocPrinter as DocP
import qualified Acton.Env
import Acton.Env (CompilationError(..))
import qualified Acton.Kinds
import qualified Acton.Types
import Acton.TypeEnv (TypeError(..),typeReport)
import qualified Acton.Normalizer
import qualified Acton.Deactorizer
import qualified Acton.CPS
import qualified Acton.LambdaLifter
import qualified Acton.Boxing
import qualified Acton.CodeGen
import qualified Acton.Diagnostics as Diag
import qualified Acton.Compile as Compile
import qualified Acton.CommandLineParser as C
import qualified Acton.Fingerprint as Fingerprint
import qualified Acton.Completion as Completion
import qualified InterfaceFiles
import Pretty (print, prettyText)
import qualified Pretty
import Test.Syd
import Test.Syd.Def.Golden (goldenTextFile)
import qualified Control.Monad.Trans.State.Strict as St
import Text.Megaparsec (ParseErrorBundle, PosState(..), bundleErrors, bundlePosState, errorOffset, reachOffset, runParser, errorBundlePretty, ShowErrorComponent(..))
import Text.Megaparsec.Pos (sourceLine, unPos)
import qualified Data.Text as T
import Data.List (isInfixOf, isPrefixOf, nub, sort)
import qualified Data.List.NonEmpty as NE
import Data.IORef
import Data.Bits (shiftL, (.|.))
import Error.Diagnose (printDiagnostic, prettyDiagnostic, WithUnicode(..), TabSize(..), defaultStyle, addReport, addFile)
import Error.Diagnose.Report (Report(..))
import Prettyprinter (unAnnotate, layoutPretty, defaultLayoutOptions)
import Prettyprinter.Render.Text (renderStrict)
import System.FilePath ((</>), joinPath, takeFileName, takeBaseName, takeDirectory, splitDirectories, takeExtension)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, setCurrentDirectory, listDirectory, doesDirectoryExist, doesFileExist)
import System.Posix.Files (setFileMode)
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Control.Monad (forM_, when, foldM)
import qualified Control.Exception as E
import Control.DeepSeq (rnf)
import Utils (SrcLoc(..), loc, prstr)
import qualified Acton.BuildSpec as BuildSpec
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8
import qualified Data.Aeson as Ae
import qualified System.IO.Unsafe


nameHash :: S.Name -> B8.ByteString -> B8.ByteString -> B8.ByteString -> InterfaceFiles.NameHashInfo
nameHash n src pub impl =
  InterfaceFiles.NameHashInfo
    { InterfaceFiles.nhName = n
    , InterfaceFiles.nhSrcHash = src
    , InterfaceFiles.nhPubHash = pub
    , InterfaceFiles.nhImplHash = impl
    , InterfaceFiles.nhPubLocalDeps = []
    , InterfaceFiles.nhImplLocalDeps = []
    , InterfaceFiles.nhPubDeps = []
    , InterfaceFiles.nhImplDeps = []
    }



main :: IO ()
main = do
  let sysTypesPath = ".." </> ".." </> "dist" </> "base" </> "out" </> "types"
  env0 <- Acton.Env.initEnv sysTypesPath False

  sydTest $ do
    describe "InterfaceFiles" $ do
      it "round-trips payloads and preserves ordered name entries" $ do
        withSystemTempDirectory "acton-iface" $ \dir -> do
          let mn = S.modName ["iface"]
              tyPath = dir </> "iface.tydb"
              firstName = S.name "first"
              secondName = S.name "second"
              firstishName = S.name "firstish"
              sourcePlainName = S.name "foo_bar"
              derivedName = S.Derived (S.name "encode") (S.name "witness")
              longName = S.name (replicate 401 'x')
              iface =
                [ (firstName, I.NVar S.tWild)
                , (secondName, I.NDef (S.tSchema [] S.tWild) S.NoDec Nothing)
                , (sourcePlainName, I.NVar S.tWild)
                , (derivedName, I.NVar S.tWild)
                , (longName, I.NVar S.tWild)
                ]
              nameHashes =
                [ nameHash derivedName "src4" "pub4" "impl4"
                , nameHash firstName "src1" "pub1" "impl1"
                , nameHash sourcePlainName "src3" "pub3" "impl3"
                , nameHash secondName "src2" "pub2" "impl2"
                , nameHash longName "src5" "pub5" "impl5"
                ]
              roots = [secondName, firstName]
              tests = ["test_second", "test_first"]
              nmod = I.NModule [] iface (Just "module docs")
              tmod = S.Module mn [] (Just "typed docs") []
          InterfaceFiles.writeFile tyPath "src" "pub" "impl" Nothing [] nameHashes roots tests (Just "module docs") nmod tmod
          InterfaceFiles.keyNameInfo firstName `shouldBe` "name-info/p/first"
          InterfaceFiles.keyNameInfo firstName `shouldNotBe` InterfaceFiles.keyNameInfo firstishName
          InterfaceFiles.keyNameHash secondName `shouldBe` "name-hash/p/second"
          InterfaceFiles.keyNameInfo sourcePlainName `shouldBe` "name-info/p/foo_bar"
          InterfaceFiles.keyNameInfo derivedName `shouldBe` "name-info/p/encodeD_witness"
          InterfaceFiles.keyNameInfo longName `shouldSatisfy` B8.isPrefixOf "name-info/h/"
          (_mods, I.NModule _ te mdoc, tmod', sourceMeta, srcHash, pubHash, implHash, imps, nameHashes', roots', tests', doc') <-
            InterfaceFiles.readFile tyPath
          te `shouldBe` iface
          mdoc `shouldBe` Just "module docs"
          tmod' `shouldBe` tmod
          sourceMeta `shouldBe` Nothing
          (srcHash, pubHash, implHash) `shouldBe` ("src", "pub", "impl")
          imps `shouldBe` []
          nameHashes' `shouldBe` nameHashes
          roots' `shouldBe` roots
          tests' `shouldBe` tests
          doc' `shouldBe` Just "module docs"

          (sourceMetaH, srcHashH, pubHashH, implHashH, impsH, nameHashesH, rootsH, testsH, docH) <-
            InterfaceFiles.readHeader tyPath
          sourceMetaH `shouldBe` Nothing
          (srcHashH, pubHashH, implHashH) `shouldBe` ("src", "pub", "impl")
          impsH `shouldBe` []
          nameHashesH `shouldBe` nameHashes
          rootsH `shouldBe` roots
          testsH `shouldBe` tests
          docH `shouldBe` Just "module docs"

      it "supports concurrent read-only access to one interface" $ do
        withSystemTempDirectory "acton-iface-concurrent" $ \dir -> do
          let mn = S.modName ["iface"]
              tyPath = dir </> "iface.tydb"
              firstName = S.name "first"
              iface = [(firstName, I.NVar S.tWild)]
              nameHashes = [nameHash firstName "src1" "pub1" "impl1"]
              nmod = I.NModule [] iface Nothing
              tmod = S.Module mn [] Nothing []
          InterfaceFiles.writeFile tyPath "src" "pub" "impl" Nothing [] nameHashes [] [] Nothing nmod tmod
          mapConcurrently_
            (\_ -> do
                (_sourceMetaH, srcHashH, pubHashH, implHashH, _impsH, nameHashesH, _rootsH, _testsH, _docH) <-
                  InterfaceFiles.readHeader tyPath
                (srcHashH, pubHashH, implHashH) `shouldBe` ("src", "pub", "impl")
                nameHashesH `shouldBe` nameHashes)
            [1..64 :: Int]

      it "copies interface data without carrying LMDB lock state" $ do
        withSystemTempDirectory "acton-iface-copy" $ \dir -> do
          let mn = S.modName ["iface"]
              srcPath = dir </> "iface.tydb"
              dstPath = dir </> "iface-copy.tydb"
              firstName = S.name "first"
              iface = [(firstName, I.NVar S.tWild)]
              nameHashes = [nameHash firstName "src1" "pub1" "impl1"]
              nmod = I.NModule [] iface Nothing
              tmod = S.Module mn [] Nothing []
          InterfaceFiles.writeFile srcPath "src" "pub" "impl" Nothing [] nameHashes [] [] Nothing nmod tmod
          InterfaceFiles.copyInterface srcPath dstPath
          doesFileExist (dstPath </> "data.mdb") `shouldReturn` True
          doesFileExist (dstPath </> "lock.mdb") `shouldReturn` False
          (_sourceMetaH, srcHashH, pubHashH, implHashH, _impsH, nameHashesH, _rootsH, _testsH, _docH) <-
            InterfaceFiles.readHeader dstPath
          (srcHashH, pubHashH, implHashH) `shouldBe` ("src", "pub", "impl")
          nameHashesH `shouldBe` nameHashes

      it "reads interfaces from read-only installed directories" $ do
        withSystemTempDirectory "acton-iface-readonly" $ \dir -> do
          let mn = S.modName ["iface"]
              tyPath = dir </> "iface.tydb"
              dataPath = tyPath </> "data.mdb"
              lockPath = tyPath </> "lock.mdb"
              firstName = S.name "first"
              iface = [(firstName, I.NVar S.tWild)]
              nameHashes = [nameHash firstName "src1" "pub1" "impl1"]
              nmod = I.NModule [] iface Nothing
              tmod = S.Module mn [] Nothing []
          InterfaceFiles.writeFile tyPath "src" "pub" "impl" Nothing [] nameHashes [] [] Nothing nmod tmod
          let restore = do
                setFileMode tyPath 0o755
                setFileMode dataPath 0o644
                setFileMode lockPath 0o644
          (do setFileMode tyPath 0o555
              setFileMode dataPath 0o444
              setFileMode lockPath 0o444
              (_sourceMetaH, srcHashH, pubHashH, implHashH, _impsH, nameHashesH, _rootsH, _testsH, _docH) <-
                InterfaceFiles.readHeader tyPath
              (srcHashH, pubHashH, implHashH) `shouldBe` ("src", "pub", "impl")
              nameHashesH `shouldBe` nameHashes)
            `E.finally` restore

      it "treats corrupt .tydb directories as cache misses" $ do
        withSystemTempDirectory "acton-iface-corrupt" $ \dir -> do
          let tyPath = dir </> "corrupt.tydb"
          createDirectoryIfMissing True tyPath
          B8.writeFile (tyPath </> "data.mdb") "garbage"
          InterfaceFiles.readHeaderMaybe tyPath `shouldReturn` Nothing
          InterfaceFiles.readFileMaybe tyPath `shouldReturn` Nothing

      it "treats version mismatches as cache misses" $ do
        withSystemTempDirectory "acton-iface-version" $ \dir -> do
          let mn = S.modName ["iface_version"]
              tyPath = dir </> "iface_version.tydb"
              nmod = I.NModule [] [] Nothing
              tmod = S.Module mn [] Nothing []
          InterfaceFiles.writeFileWithVersion (map (+ 1) S.version) tyPath "" "" "" Nothing [] [] [] [] Nothing nmod tmod
          InterfaceFiles.readHeaderMaybe tyPath `shouldReturn` Nothing
          InterfaceFiles.readFileMaybe tyPath `shouldReturn` Nothing

    describe "CompileScheduler" $ do
      it "waits for canceled generation cleanup before launching the replacement" $ do
        let gopts = C.GlobalOptions
              { C.color = C.Never
              , C.quiet = True
              , C.noProgress = False
              , C.timing = False
              , C.tty = False
              , C.verbose = False
              , C.verboseZig = False
              , C.jobs = 1
              }
        sched <- Compile.newCompileScheduler gopts 1
        oldStarted <- newEmptyMVar
        oldCleanupStarted <- newEmptyMVar
        releaseOldCleanup <- newEmptyMVar
        newStarted <- newEmptyMVar
        _ <- Compile.startCompile sched 0 $ \_ ->
          E.finally
            (putMVar oldStarted () >> threadDelay 10000000)
            (putMVar oldCleanupStarted () >> takeMVar releaseOldCleanup)
        takeMVar oldStarted
        nextCompile <- async $
          Compile.startCompile sched 0 $ \_ -> putMVar newStarted ()
        takeMVar oldCleanupStarted
        E.finally
          (timeout 100000 (takeMVar newStarted) `shouldReturn` Nothing)
          (putMVar releaseOldCleanup ())
        _ <- wait nextCompile
        timeout 1000000 (takeMVar newStarted) `shouldReturn` Just ()

    describe "Environment" $ do
      it "treats mismatched .tydb headers for loaded modules as stale" $ do
        withSystemTempDirectory "acton-env" $ \dir -> do
          let directMod = S.modName ["direct"]
              directTy = dir </> "direct.tydb"
              valueName = S.name "value"
              iface = [(valueName, I.NVar S.tWild)]
              directIface = I.NModule [] iface Nothing
              directModule = S.Module directMod [] Nothing []
              env1 = Acton.Env.addMod directMod [] iface Nothing env0
          InterfaceFiles.writeFile
            directTy
            B8.empty
            B8.empty
            B8.empty
            Nothing
            []
            []
            []
            []
            Nothing
            directIface
            directModule
          (_mods, nmod, tmod, sourceMeta, srcHash, pubHash, implHash, imps, nameHashes, roots, tests, mdoc) <-
            InterfaceFiles.readFile directTy
          InterfaceFiles.writeFileWithVersion
            (map (+ 1) S.version)
            directTy
            srcHash
            pubHash
            implHash
            sourceMeta
            imps
            nameHashes
            roots
            tests
            mdoc
            nmod
            tmod
          (_env2, te) <- Acton.Env.doImp [dir] env1 directMod
          map fst te `shouldBe` [valueName]

    describe "Pass 1: Parser" $ do

      it "reports rough parse progress by source offset" $ do
        progressRef <- newIORef []
        _ <- P.parseModule (S.modName ["progress"]) "progress.act" "x = 1\n" (Just $ \completed total ->
          modifyIORef' progressRef (++ [(completed, total)]))
        progress <- readIORef progressRef
        let completed = map fst progress
            monotonic xs = and (zipWith (<=) xs (drop 1 xs))
        progress `shouldSatisfy` (not . null)
        completed `shouldSatisfy` monotonic
        last progress `shouldBe` (6, 6)

      describe "Basic Syntax" $ do
        testParse env0 ["syntax1"]

      describe "Docstrings" $ do
        testParse env0 ["docstrings"]
        testDocstrings env0 "docstrings"

      describe "Module Structure" $ do
        describe "Module-level docstrings" $ do
          it "allows single-line docstring before import" $ do
            let input = "\"\"\"Module docstring\"\"\"\nimport math\n"
            case parseModuleTest input of
              Left err -> expectationFailure $ "Parse failed: " ++ err
              Right _ -> return ()

          it "allows multi-line docstring before import" $ do
            let input = "\"\"\"Module docstring\nwith multiple lines\"\"\"\nimport math\n"
            case parseModuleTest input of
              Left err -> expectationFailure $ "Parse failed: " ++ err
              Right _ -> return ()

        describe "Invalid module structure (before imports)" $ do
          testModuleParseError "module_var_before_import" "x = 42\nimport math\n"
          testModuleParseError "module_call_before_import" "print(\"hello\")\nimport math\n"
          testModuleParseError "module_number_before_import" "42\nimport math\n"
          testModuleParseError "module_list_before_import" "[1, 2, 3]\nimport math\n"
          testModuleParseError "module_dict_before_import" "{\"key\": \"value\"}\nimport math\n"
          testModuleParseError "module_if_before_import" "if True:\n    pass\nimport math\n"
          testModuleParseError "module_for_before_import" "for i in range(10):\n    pass\nimport math\n"
          testModuleParseError "module_class_before_import" "class Foo:\n    pass\nimport math\n"
          testModuleParseError "module_func_before_import" "def foo():\n    pass\nimport math\n"
          testModuleParseError "module_actor_before_import" "actor Foo():\n    pass\nimport math\n"

        describe "Module-level constants" $ do
          it "allows top-level declarations, signatures, and assignments" $ do
            expectModuleParseSuccess $ unlines
              [ "value : int"
              , "def helper() -> int:"
              , "    return 41"
              , "value = helper()"
              ]

          it "rejects top-level expression statements" $ do
            err <- expectModuleParseFailure $ unlines
              [ "value = 1"
              , "1 + 2"
              ]
            err `shouldContain` "Only declarations and assignments are allowed at the module top level"

          it "rejects top-level control flow" $ do
            err <- expectModuleParseFailure $ unlines
              [ "import math"
              , "if True:"
              , "    value = 1"
              ]
            err `shouldContain` "Only declarations and assignments are allowed at the module top level"

          it "rejects top-level reassignment" $ do
            err <- expectModuleParseFailure $ unlines
              [ "value = 1"
              , "value = 2"
              ]
            err `shouldContain` "Module top-level name 'value' cannot be assigned more than once"

          it "allows top-level destructuring with at least one bound name" $ do
            expectModuleParseSuccess "left, _ = (1, 2)\n"

          it "rejects duplicate names within one top-level assignment" $ do
            err <- expectModuleParseFailure "left, left = (1, 2)\n"
            err `shouldContain` "Module top-level name 'left' cannot be assigned more than once"

          it "rejects top-level wildcard-only assignment" $ do
            _ <- expectModuleParseFailure "_ = 1\n"
            return ()

          it "rejects top-level assignments with no bound names" $ do
            err <- expectModuleParseFailure "[] = []\n"
            err `shouldContain` "Module top-level assignments must bind at least one name"

          it "rejects top-level wildcard-only destructuring assignment" $ do
            err <- expectModuleParseFailure "[_, _] = [1, 2]\n"
            err `shouldContain` "Module top-level assignments must bind at least one name"

        describe "Chunked module parser" $ do
          it "matches the serial parser for mixed top-level forms" $ do
            expectChunkedParseMatchesSerial $ unlines
              [ "\"\"\"module docs\"\"\""
              , "import math"
              , "answer: int"
              , "answer = ("
              , "    40 +"
              , "    2"
              , ")"
              , "class Box:"
              , "    def get(self):"
              , "        return answer"
              , "actor Worker(env):"
              , "    def run(self):"
              , "        env.exit(0)"
              ]

          it "does not split on column-zero text inside strings" $ do
            expectChunkedParseMatchesSerial $ unlines
              [ "text = \"\"\""
              , "def not_a_chunk():"
              , "    pass"
              , "\"\"\""
              , "formatted = \"value {str('class also_not_a_chunk:')}\""
              , "def real():"
              , "    return text"
              ]

          it "matches triple-string quote run handling" $ do
            expectChunkedParseMatchesSerial $ unlines
              [ "quote_tail = \"\"\"foo\"\"\"\""
              , "two_quote_tail = \"\"\"foo\"\"\"\"\""
              , "formatted = \"\"\"value {str(\"class not_a_chunk:\")}\"\"\"\""
              , "def real():"
              , "    return formatted"
              ]

          it "preserves adjacent declaration grouping" $ do
            expectChunkedParseMatchesSerial $ unlines
              [ "def even(n):"
              , "    if n == 0:"
              , "        return True"
              , "    return odd(n - 1)"
              , "def odd(n):"
              , "    if n == 0:"
              , "        return False"
              , "    return even(n - 1)"
              , "value = even(2)"
              ]

          it "reports later chunk parse errors at original source lines" $ do
            let input = unlines
                  [ "ok = 1"
                  , "def broken():"
                  , "    if True"
                  , "        pass"
                  ]
                moduleName = S.modName ["chunked"]
            result <- E.try (P.parseModule moduleName "chunked.act" input Nothing)
            case result of
              Left (bundle :: ParseErrorBundle String P.CustomParseError) ->
                parseBundleErrorLine bundle `shouldBe` 3
              Right _ ->
                expectationFailure "Expected chunked parser to reject malformed input"

          it "returns diagnostics for chunk scanner failures" $ do
            let input = "    pass\n"
                moduleName = S.modName ["chunked"]
            result <- Compile.parseActSource Compile.defaultCompileOptions moduleName "chunked.act" input Nothing
            case result of
              Left [diagnostic] ->
                renderDiagnosticText diagnostic `shouldContain` "non-top-level text before first chunk"
              Left diagnostics ->
                expectationFailure ("Expected one diagnostic, got " ++ show (length diagnostics))
              Right _ ->
                expectationFailure "Expected scanner failure diagnostic"

          it "matches the serial parser for existing fixtures" $ do
            actFiles <- actFilesUnder ("test" </> "src")
            forM_ actFiles $ \actFile -> do
              input <- readFile actFile
              expectChunkedParseMatchesSerialFile actFile input

      describe "Numeric literals" $ do
        it "parses INT64_MIN as a single literal" $ do
          case parseStmtAst "a = -9223372036854775808" of
            Left err -> expectationFailure $ "Parse failed: " ++ err
            Right [S.Assign _ _ expr] -> case expr of
              S.Int _ ival lexeme -> do
                ival `shouldBe` (-9223372036854775808)
                lexeme `shouldBe` "-9223372036854775808"
              other -> expectationFailure $ "Expected Int literal, got " ++ show other
            Right other -> expectationFailure $ "Unexpected AST: " ++ show other

        it "keeps unary minus above exponentiation for -1**2" $ do
          case parseExprAst "-1**2" of
            Left err -> expectationFailure $ "Parse failed: " ++ err
            Right (S.UnOp _ S.UMinus inner) -> case inner of
              S.BinOp _ lhs op rhs -> do
                op `shouldBe` S.Pow
                case lhs of
                  S.Int _ ival lexeme -> do
                    ival `shouldBe` 1
                    lexeme `shouldBe` "1"
                  other -> expectationFailure $ "Expected left int literal, got " ++ show other
                case rhs of
                  S.Int _ ival lexeme -> do
                    ival `shouldBe` 2
                    lexeme `shouldBe` "2"
                  other -> expectationFailure $ "Expected right int literal, got " ++ show other
              other -> expectationFailure $ "Expected power expression, got " ++ show other
            Right other -> expectationFailure $ "Expected unary minus, got " ++ show other

        it "respects parentheses for (-1)**2" $ do
          case parseExprAst "(-1)**2" of
            Left err -> expectationFailure $ "Parse failed: " ++ err
            Right (S.BinOp _ lhs op rhs) -> do
              op `shouldBe` S.Pow
              case lhs of
                S.Paren _ (S.Int _ ival lexeme) -> do
                  ival `shouldBe` (-1)
                  lexeme `shouldBe` "-1"
                other -> expectationFailure $ "Expected parenthesized negative literal, got " ++ show other
              case rhs of
                S.Int _ ival lexeme -> do
                  ival `shouldBe` 2
                  lexeme `shouldBe` "2"
                other -> expectationFailure $ "Expected right int literal, got " ++ show other
            Right other -> expectationFailure $ "Expected power expression, got " ++ show other

      describe "Optional chaining" $ do
        it "parses optional chaining expression statements without crashing" $ do
          case parseStmtAst "l?.append(1)" of
            Left err -> expectationFailure $ "Parse failed: " ++ err
            Right [S.Expr _ expr@S.OptChain{}] -> loc expr `shouldNotBe` NoLoc
            Right other -> expectationFailure $ "Unexpected AST: " ++ show other

      describe "Completion" $ do
        it "completes inherited transform input attributes" $ do
          let env = completionFixtureEnv env0
              (src, cursor) = cursorSource $ unlines
                [ "import mini.layers.base_1 as base"
                , "import mini.layers.y_1 as y1"
                , ""
                , "class L3vpnEndpoint(base.L3vpnEndpoint):"
                , "    def transform(self, i, di):"
                , "        i.<CURSOR>"
                ]
          items <- Completion.memberCompletions env [] (S.modName ["rfs"]) "rfs.act" src cursor
          let labels = map Completion.completionLabel items
          forM_ ["interface_name", "vpn_name", "ipv4_address"] $ \label ->
            labels `shouldSatisfy` elem label

        it "completes explicitly typed transform input attributes" $ do
          let env = completionFixtureEnv env0
              inputTypeName = "y1.stratoweave_rfs__rfs__l3vpn_endpoint_entry"
              (src, cursor) = cursorSource $ unlines
                [ "import mini.layers.base_1 as base"
                , "import mini.layers.y_1 as y1"
                , ""
                , "class L3vpnEndpoint(base.L3vpnEndpoint):"
                , "    def transform(self, i: " ++ inputTypeName ++ ", di):"
                , "        i.v<CURSOR>"
                ]
          items <- Completion.memberCompletions env [] (S.modName ["rfs"]) "rfs.act" src cursor
          map Completion.completionLabel items `shouldBe` ["vpn_name"]

        it "completes annotated parameters inside generic functions" $ do
          let env = completionFixtureEnv env0
              inputTypeName = "y1.stratoweave_rfs__rfs__l3vpn_endpoint_entry"
              (src, cursor) = cursorSource $ unlines
                [ "import mini.layers.y_1 as y1"
                , ""
                , "def helper [T](i: " ++ inputTypeName ++ "):"
                , "    i.v<CURSOR>"
                ]
          items <- Completion.memberCompletions env [] (S.modName ["rfs"]) "rfs.act" src cursor
          map Completion.completionLabel items `shouldBe` ["vpn_name"]

        it "completes local values assigned from typed calls" $ do
          let env = completionFixtureEnv env0
              (src, cursor) = cursorSource $ unlines
                [ "import mini.layers.base_1 as base"
                , ""
                , "class L3vpnEndpoint(base.L3vpnEndpoint):"
                , "    def transform(self, i, di):"
                , "        o = base.o_root()"
                , "        o.<CURSOR>"
                ]
          items <- Completion.memberCompletions env [] (S.modName ["rfs"]) "rfs.act" src cursor
          map Completion.completionLabel items `shouldSatisfy` elem "netinfra"

        it "completes local values assigned from nested member calls" $ do
          let env = completionFixtureEnv env0
              (src, cursor) = cursorSource $ unlines
                [ "import mini.layers.base_1 as base"
                , ""
                , "class L3vpnEndpoint(base.L3vpnEndpoint):"
                , "    def transform(self, i, di):"
                , "        o = base.o_root()"
                , "        o_router = o.netinfra.router.create(i.name, id=i.id)"
                , "        o_router.<CURSOR>"
                ]
          items <- Completion.memberCompletions env [] (S.modName ["rfs"]) "rfs.act" src cursor
          map Completion.completionLabel items `shouldSatisfy` elem "router_id"

        it "completes local values assigned from member paths" $ do
          let env = completionFixtureEnv env0
              (src, cursor) = cursorSource $ unlines
                [ "import mini.layers.base_1 as base"
                , ""
                , "class L3vpnEndpoint(base.L3vpnEndpoint):"
                , "    def transform(self, i, di):"
                , "        o = base.o_root()"
                , "        o_router = o.netinfra.router.create(i.name, id=i.id)"
                , "        bc = o_router.base_config"
                , "        bc.<CURSOR>"
                ]
          items <- Completion.memberCompletions env [] (S.modName ["rfs"]) "rfs.act" src cursor
          map Completion.completionLabel items `shouldSatisfy` elem "asn"
          [ Completion.completionDetail item
            | item <- items
            , Completion.completionLabel item == "asn"
            ] `shouldBe` [Just "int"]

        it "uses inherited parameter types inside generic classes" $ do
          let env = completionFixtureEnv env0
              (src, cursor) = cursorSource $ unlines
                [ "import mini.layers.base_1 as base"
                , ""
                , "class L3vpnEndpoint[T](base.L3vpnEndpoint):"
                , "    def transform(self, i, di):"
                , "        i.<CURSOR>"
                ]
          items <- Completion.memberCompletions env [] (S.modName ["rfs"]) "rfs.act" src cursor
          map Completion.completionLabel items `shouldSatisfy` elem "interface_name"

        it "resolves inherited parameter types in the parent module" $ do
          let env = completionFixtureEnv env0
              (src, cursor) = cursorSource $ unlines
                [ "import mini.layers.base_1 as base"
                , ""
                , "class LocalEndpoint(base.LocalEndpoint):"
                , "    def transform(self, i):"
                , "        a = i.<CURSOR>"
                ]
          items <- Completion.memberCompletions env [] (S.modName ["rfs"]) "rfs.act" src cursor
          map Completion.completionLabel items `shouldSatisfy` elem "local_field"

        it "uses direct imports when transitive interfaces are stale" $ do
          withSystemTempDirectory "acton-completion" $ \dir -> do
            let directMod = S.modName ["direct"]
                staleMod = S.modName ["stale"]
                directTy = dir </> "direct.tydb"
                staleTy = dir </> "stale.tydb"
                inputName = S.name "LocalInput"
                routerName = S.name "Router"
                inputType = S.tCon (S.TC (S.NoQ inputName) [])
                transformType =
                  S.tFun S.fxMut
                    (S.posRow inputType S.posNil)
                    S.kwdNil
                    S.tWild
                transformInfo = I.NSig (S.tSchema [] transformType) S.NoDec Nothing
                inputClass = I.NClass [] [] [(S.name "local_field", I.NVar S.tWild)] Nothing
                routerClass = I.NClass [] [] [(S.name "transform", transformInfo)] Nothing
                directIface = I.NModule []
                  [ (routerName, routerClass)
                  , (inputName, inputClass)
                  ] Nothing
                directModule = S.Module directMod [] Nothing []
                (src, cursor) = cursorSource $ unlines
                  [ "import direct as base"
                  , ""
                  , "class Router(base.Router):"
                  , "    def transform(self, i):"
                  , "        i.<CURSOR>"
                  ]
            createDirectoryIfMissing True dir
            createDirectoryIfMissing True staleTy
            B8.writeFile (staleTy </> "data.mdb") "not a current ty db"
            InterfaceFiles.writeFile
              directTy
              B8.empty
              B8.empty
              B8.empty
              Nothing
              [(staleMod, B8.empty)]
              []
              []
              []
              Nothing
              directIface
              directModule
            items <- Completion.memberCompletions env0 [dir] (S.modName ["rfs"]) "rfs.act" src cursor
            map Completion.completionLabel items `shouldSatisfy` elem "local_field"

        it "shows signatures for nested member calls" $ do
          let env = completionFixtureEnv env0
              (src, cursor) = cursorSource $ unlines
                [ "import mini.layers.base_1 as base"
                , ""
                , "class L3vpnEndpoint(base.L3vpnEndpoint):"
                , "    def transform(self, i, di):"
                , "        o = base.o_root()"
                , "        o.netinfra.router.create(\"r1\", <CURSOR>)"
                ]
          sigs <- Completion.callSignatures env [] (S.modName ["rfs"]) "rfs.act" src cursor
          case sigs of
            [sig] -> do
              Completion.callSignatureLabel sig `shouldSatisfy` isPrefixOf "o.netinfra.router.create("
              Completion.callSignatureLabel sig `shouldSatisfy` not . isInfixOf "__builtin__."
              Completion.callSignatureActiveParameter sig `shouldBe` 1
              let params = map Completion.signatureParameterLabel (Completion.callSignatureParameters sig)
              params `shouldSatisfy` any (isPrefixOf "arg1:")
              params `shouldSatisfy` any (isPrefixOf "id:")
              params `shouldSatisfy` all (not . isInfixOf "__builtin__.")
            other ->
              expectationFailure $ "Unexpected signatures: " ++ show other

        it "ignores string parentheses when finding call signatures" $ do
          let env = completionFixtureEnv env0
          forM_ ["\"(\"", "\"x)\""] $ \arg -> do
            let (src, cursor) = cursorSource $ unlines
                  [ "import mini.layers.base_1 as base"
                  , ""
                  , "class L3vpnEndpoint(base.L3vpnEndpoint):"
                  , "    def transform(self, i, di):"
                  , "        o = base.o_root()"
                  , "        o.netinfra.router.create(" ++ arg ++ ", <CURSOR>)"
                  ]
            sigs <- Completion.callSignatures env [] (S.modName ["rfs"]) "rfs.act" src cursor
            case sigs of
              [sig] ->
                Completion.callSignatureActiveParameter sig `shouldBe` 1
              other ->
                expectationFailure $ "Unexpected signatures: " ++ show other

        it "completes keyword arguments for nested member calls" $ do
          let env = completionFixtureEnv env0
              (src, cursor) = cursorSource $ unlines
                [ "import mini.layers.base_1 as base"
                , ""
                , "class L3vpnEndpoint(base.L3vpnEndpoint):"
                , "    def transform(self, i, di):"
                , "        o = base.o_root()"
                , "        o.netinfra.router.create(\"r1\", <CURSOR>)"
                ]
          items <- Completion.argumentCompletions env [] (S.modName ["rfs"]) "rfs.act" src cursor
          map Completion.completionLabel items `shouldBe` ["id=", "role=", "mock="]
          map Completion.completionKind items `shouldBe` replicate 3 Completion.CompletionKeyword
          map Completion.completionDetail items `shouldBe` map Just ["int", "str", "bool"]

        it "filters supplied keyword argument completions" $ do
          let env = completionFixtureEnv env0
              (src, cursor) = cursorSource $ unlines
                [ "import mini.layers.base_1 as base"
                , ""
                , "class L3vpnEndpoint(base.L3vpnEndpoint):"
                , "    def transform(self, i, di):"
                , "        o = base.o_root()"
                , "        o.netinfra.router.create(\"r1\", id=1, <CURSOR>)"
                ]
          items <- Completion.argumentCompletions env [] (S.modName ["rfs"]) "rfs.act" src cursor
          map Completion.completionLabel items `shouldBe` ["role=", "mock="]

        it "prefixes keyword argument completions" $ do
          let env = completionFixtureEnv env0
              (src, cursor) = cursorSource $ unlines
                [ "import mini.layers.base_1 as base"
                , ""
                , "class L3vpnEndpoint(base.L3vpnEndpoint):"
                , "    def transform(self, i, di):"
                , "        o = base.o_root()"
                , "        o.netinfra.router.create(\"r1\", id=1, r<CURSOR>)"
                ]
          items <- Completion.argumentCompletions env [] (S.modName ["rfs"]) "rfs.act" src cursor
          map Completion.completionLabel items `shouldBe` ["role="]

        it "keeps escaped commas inside string arguments" $ do
          let env = completionFixtureEnv env0
              (src, cursor) = cursorSource $ unlines
                [ "import mini.layers.base_1 as base"
                , ""
                , "class L3vpnEndpoint(base.L3vpnEndpoint):"
                , "    def transform(self, i, di):"
                , "        o = base.o_root()"
                , "        o.netinfra.router.create(\"r1\", id=1, role=\"a\\\",b\", m<CURSOR>)"
                ]
          items <- Completion.argumentCompletions env [] (S.modName ["rfs"]) "rfs.act" src cursor
          map Completion.completionLabel items `shouldBe` ["mock="]

        it "does not complete keyword names inside argument values" $ do
          let env = completionFixtureEnv env0
              (src, cursor) = cursorSource $ unlines
                [ "import mini.layers.base_1 as base"
                , ""
                , "class L3vpnEndpoint(base.L3vpnEndpoint):"
                , "    def transform(self, i, di):"
                , "        o = base.o_root()"
                , "        o.netinfra.router.create(\"r1\", id=<CURSOR>)"
                ]
          items <- Completion.argumentCompletions env [] (S.modName ["rfs"]) "rfs.act" src cursor
          items `shouldBe` []

        it "hovers local values assigned from member paths" $ do
          let env = completionFixtureEnv env0
              (src, cursor) = cursorSource $ unlines
                [ "import mini.layers.base_1 as base"
                , ""
                , "class L3vpnEndpoint(base.L3vpnEndpoint):"
                , "    def transform(self, i, di):"
                , "        o = base.o_root()"
                , "        o_router = o.netinfra.router.create(i.name, id=i.id)"
                , "        bc = o_router.base_config"
                , "        bc<CURSOR>.asn = 1"
                ]
          info <- Completion.hoverInfo env [] (S.modName ["rfs"]) "rfs.act" src cursor
          case info of
            Just hover -> do
              Completion.hoverDetail hover `shouldSatisfy` isInfixOf "BaseConfig"
              Completion.hoverDocumentation hover `shouldBe` Just "Base router configuration."
            Nothing ->
              expectationFailure "Expected hover info for bc"

        it "hovers nested member fields" $ do
          let env = completionFixtureEnv env0
              (src, cursor) = cursorSource $ unlines
                [ "import mini.layers.base_1 as base"
                , ""
                , "class L3vpnEndpoint(base.L3vpnEndpoint):"
                , "    def transform(self, i, di):"
                , "        o = base.o_root()"
                , "        o_router = o.netinfra.router.create(i.name, id=i.id)"
                , "        bc = o_router.base_config"
                , "        bc.<CURSOR>asn = 1"
                ]
          info <- Completion.hoverInfo env [] (S.modName ["rfs"]) "rfs.act" src cursor
          case info of
            Just hover -> do
              Completion.hoverDetail hover `shouldBe` "bc.asn: int"
              Completion.hoverDocumentation hover `shouldBe` Just "Autonomous system number."
            Nothing ->
              expectationFailure "Expected hover info for bc.asn"

        it "hovers methods resolved through local call results" $ do
          let env = completionFixtureEnv env0
              (src, cursor) = cursorSource $ unlines
                [ "import mini.layers.base_1 as base"
                , ""
                , "class L3vpnEndpoint(base.L3vpnEndpoint):"
                , "    def transform(self, i, di):"
                , "        o = base.o_root()"
                , "        o.netinfra.router.<CURSOR>create(\"r1\", id=1)"
                ]
          info <- Completion.hoverInfo env [] (S.modName ["rfs"]) "rfs.act" src cursor
          case info of
            Just hover -> do
              Completion.hoverDetail hover `shouldSatisfy` isPrefixOf "o.netinfra.router.create:"
              Completion.hoverDetail hover `shouldSatisfy` isInfixOf "RouterEntry"
              Completion.hoverDetail hover `shouldSatisfy` not . isInfixOf "__builtin__."
              Completion.hoverDocumentation hover `shouldBe` Just "Create a router entry."
            Nothing ->
              expectationFailure "Expected hover info for create"

        it "ignores imported module path hovers" $ do
          let env = completionFixtureEnv env0
              (src, cursor) = cursorSource "import mini.layers.y_1<CURSOR>\n"
          result <- E.try (E.evaluate (Completion.hoverInfoWithEnv env src cursor))
          case result of
            Left (err :: E.SomeException) ->
              expectationFailure $ "Unexpected hover exception: " ++ E.displayException err
            Right info ->
              info `shouldBe` Nothing

      describe "String Interpolation" $ do
        -- Note: In these tests, we use Haskell string literals which require escaping.
        -- The test format is: testParseOutput <Haskell literal> <expected parser output>
        --
        -- Examples of what the Haskell escapes mean in actual Acton code:
        --   Haskell literal: "\"\""           → Acton code: ""
        --   Haskell literal: "\"hello\""      → Acton code: "hello"
        --   Haskell literal: "'world'"        → Acton code: 'world'
        --   Haskell literal: "f\"x = {x}\""   → Acton code: f"x = {x}"
        --   Haskell literal: "\"x = {x}\""    → Acton code: "x = {x}"
        --
        -- Interpolation is a core feature in standard Acton strings.
        -- Both "string {expr}" and f"string {expr}" support interpolation

        -- Basic functionality (simplest to more complex)
        describe "Basic string literals" $ do
          testParseOutput "\"\"" "\"\""
          testParseOutput "\"plain text\"" "\"plain text\""
          testParseOutput "'hello world'" "\"hello world\""  -- single quotes convert to double

        describe "Basic interpolation" $ do
          testParseOutput "\"hello {name}\"" "\"hello %s\" % str(name)"  -- default interpolation
          testParseOutput "f\"hello {name}!\"" "\"hello %s!\" % str(name)"  -- with f-prefix
          testParseOutput "'Value: {x}'" "\"Value: %s\" % str(x)"  -- single quotes work too

        describe "Multiple interpolations" $ do
          testParseOutput "f\"Hello {name}, your score is {score}\"" "\"Hello %s, your score is %s\" % (str(name), str(score))"
          testParseOutput "\"First: {a}, Second: {b}, Third: {c}\"" "\"First: %s, Second: %s, Third: %s\" % (str(a), str(b), str(c))"

        describe "Expressions in interpolation" $ do
          testParseOutput "\"Sum: {a + b}\"" "\"Sum: %s\" % str(a + b)"  -- simple expression
          testParseOutput "f\"Calculation: {a * b // 2}\"" "\"Calculation: %s\" % str(a * b // 2)"  -- complex expression
          testParseOutput "f\"Result: {func({a: b})}\"" "\"Result: %s\" % str(func({a: b}))"  -- nested braces

        describe "Formatting specifications" $ do
          -- Width
          testParseOutput "f\"{num:5}\"" "\"%5s\" % str(num)"
          testParseOutput "f\"{num:10}\"" "\"%10s\" % str(num)"

          -- Alignment
          testParseOutput "f\"{name:<9}\"" "\"%-9s\" % str(name)"           -- left
          testParseOutput "f\"{name:>10}\"" "\"%10s\" % str(name)"         -- right
          testParseOutput "f\"{name:^10}\"" "\"%s\" % str(name).center(10)" -- center

          -- Zero padding
          testParseOutput "f\"{num:05}\"" "\"%05d\" % num"
          testParseOutput "f\"{num:010}\"" "\"%010d\" % num"

          -- Floating point precision
          testParseOutput "f\"{pi:.2f}\"" "\"%.2f\" % pi"
          testParseOutput "f\"{pi:.4f}\"" "\"%.4f\" % pi"
          testParseOutput "f\"{pi:.0f}\"" "\"%.0f\" % pi"
          testParseOutput "f\"{pi:10.2f}\"" "\"%10.2f\" % pi"

          -- Combined formatting
          testParseOutput "f\"{a:>10}:{b:^10}:{c:<10}\"" "\"%10s:%s:%-10s\" % (str(a), str(b).center(10), str(c))"
          testParseOutput "f\"{num:+08.2f}\"" "\"%08.2f\" % num"

          -- Spaces in format spec (various positions allowed)
          testParseOutput "f\"{ num : 10 }\"" "\"%10s\" % str(num)"  -- spaces everywhere

        describe "String quote variations" $ do
          -- Triple quotes support interpolation by default
          testParseOutput "\"\"\"hello {name}!\"\"\"" "\"hello %s!\" % str(name)"  -- triple double quotes
          testParseOutput "'''multi-line\n{value}'''" "\"multi-line\\\\n%s\" % str(value)"  -- triple single quotes
          testParseOutput "f\"\"\"Name: {name}\nAge: {age}\"\"\"" "\"Name: %s\\\\nAge: %s\" % (str(name), str(age))"  -- f-prefix with multi-line
          testParseOutput "\"\"\"Plain text without interpolation\"\"\"" "\"Plain text without interpolation\""  -- no braces = no interpolation

        describe "Triple quotes with 4-5 quotes (ending with quotes)" $ do
          -- Triple double quotes with 4 quotes (string ends with single ")
          testParseOutput "\"\"\"\"foo\"\"\"" "\"\\\"foo\""  -- """"foo""" -> "foo (1 quote in content)
          testParseOutput "\"\"\"\"hello world\"\"\"\"" "\"\\\"hello world\\\"\""  -- """""hello world""""" -> ""hello world"" (2 quotes in content)
          testParseOutput "r\"\"\"\"raw quote\"\"\"" "\"\\\"raw quote\""  -- raw string with 4 quotes
          testParseOutput "r\"\"\"\"\"raw two quotes\"\"\"" "\"\\\"\\\"raw two quotes\""  -- raw string with 5 quotes

          -- Triple single quotes with 4 quotes (string ends with single ')
          testParseOutput "''''bar'''" "\"'bar\""  -- ''''bar''' -> 'bar (1 quote in content)
          testParseOutput "'''''test me'''''" "\"''test me''\""  -- '''''test me''''' -> ''test me'' (2 quotes in content)
          testParseOutput "r''''raw single'''" "\"'raw single\""  -- raw string with 4 quotes
          testParseOutput "r'''''raw two singles'''" "\"''raw two singles\""  -- raw string with 5 quotes

          -- Interpolation with trailing quotes
          testParseOutput "\"\"\"\"value: {x}\"\"\"" "\"\\\"value: %s\" % str(x)"  -- interpolation with trailing quote
          testParseOutput "\"\"\"\"\"formatted {y}\"\"\"\"\"" "\"\\\"\\\"formatted %s\\\"\\\"\" % str(y)"  -- 2 quotes before and after
          testParseOutput "f''''x = {x}'''" "\"'x = %s\" % str(x)"  -- f-string with 4 single quotes
          testParseOutput "f'''''vals: {a}, {b}'''''" "\"''vals: %s, %s''\" % (str(a), str(b))"  -- f-string with 5 single quotes

        describe "Triple quotes with 6+ quotes (error cases)" $ do
          -- Triple double quotes with 6+ quotes should fail
          testParseError "triple_double_6quotes" "\"\"\"foo\"\"\"\"\"\""  -- 3 opening, 6 closing
          testParseError "triple_double_7quotes" "\"\"\"bar\"\"\"\"\"\"\""  -- 3 opening, 7 closing
          testParseError "triple_double_8quotes" "\"\"\"test\"\"\"\"\"\"\"\""  -- 3 opening, 8 closing
          testParseError "triple_double_10quotes" "\"\"\"content\"\"\"\"\"\"\"\"\"\""  -- 3 opening, 10 closing

          -- Triple single quotes with 6+ quotes should fail
          testParseError "triple_single_6quotes" "'''foo''''''"  -- 3 opening, 6 closing
          testParseError "triple_single_7quotes" "'''bar'''''''"  -- 3 opening, 7 closing
          testParseError "triple_single_8quotes" "'''test''''''''"  -- 3 opening, 8 closing
          testParseError "triple_single_10quotes" "'''content''''''''''"  -- 3 opening, 10 closing

          -- Raw strings with 6+ quotes should fail
          testParseError "raw_triple_double_6quotes" "r\"\"\"raw\"\"\"\"\"\""  -- raw with 6 closing quotes
          testParseError "raw_triple_single_6quotes" "r'''raw''''''"  -- raw with 6 closing quotes

          -- F-strings with 6+ quotes should fail
          testParseError "fstring_triple_double_6quotes" "f\"\"\"x={x}\"\"\"\"\"\""  -- f-string with 6 closing quotes
          testParseError "fstring_triple_single_6quotes" "f'''x={x}''''''"  -- f-string with 6 closing quotes

        describe "Special characters and escaping" $ do
          -- Escaped braces
          testParseOutput "f\"something but {{{substituted}}}\"" "\"something but {%s}\" % str(substituted)"
          testParseOutput "\"{{a}}\"" "\"{a}\""  -- Escaped braces should not trigger interpolation
          testParseOutput "\"{{\"" "\"{\""  -- Just escaped opening brace
          testParseOutput "\"}}\"" "\"}\""  -- Just escaped closing brace
          testParseOutput "f\"{{hello}}\"" "\"{hello}\""  -- f-string with escaped braces
          testParseOutput "\"{{hello}} {world}\"" "\"{hello} %s\" % str(world)"  -- Mix of escaped and interpolated

          -- Escaped quotes
          testParseOutput "f\"hello \\\"thing\\\"\"" "\"hello \\\"thing\\\"\""
          testParseOutput "f'hello \\'thing\\''" "\"hello 'thing'\""
          testParseOutput "f\"Value: {x} with \\\"quotes\\\"\"" "\"Value: %s with \\\"quotes\\\"\" % str(x)"

          -- Unicode
          testParseOutput "f\"Hello, {name}! 你好!\"" "\"Hello, %s! \\20320\\22909!\" % str(name)"

        describe "Slice expressions in interpolation" $ do
          -- Basic slice patterns
          testParseOutput "\"slice: {arr[1:3]}\"" "\"slice: %s\" % str(arr[1:3])"  -- basic range
          testParseOutput "\"slice: {arr[:5]}\"" "\"slice: %s\" % str(arr[:5])"  -- from start
          testParseOutput "\"slice: {arr[2:]}\"" "\"slice: %s\" % str(arr[2:])"  -- to end

          -- Step parameter
          testParseOutput "\"step: {arr[1:10:2]}\"" "\"step: %s\" % str(arr[1:10:2])"  -- with step
          testParseOutput "\"reverse: {arr[::-1]}\"" "\"reverse: %s\" % str(arr[::-1])"  -- negative step

          -- Complex expressions
          testParseOutput "\"complex: {arr[i+1:j*2]}\"" "\"complex: %s\" % str(arr[i + 1:j * 2])"  -- expressions in slice
          testParseOutput "\"nested: {matrix[i][j:k]}\"" "\"nested: %s\" % str(matrix[i][j:k])"  -- nested indexing

        describe "Nested string interpolation" $ do
          testParseOutput "\"outer {\"inner {x}\"}\"" "\"outer %s\" % str(\"inner %s\" % str(x))"
          testParseOutput "\"outer {'inner {x}'}\"" "\"outer %s\" % str(\"inner %s\" % str(x))"
          testParseOutput "'outer {\"inner {x}\"}'" "\"outer %s\" % str(\"inner %s\" % str(x))"
          testParseOutput "\"level1 {\"level2 {\"level3 {x}\"}\"}\""  "\"level1 %s\" % str(\"level2 %s\" % str(\"level3 %s\" % str(x)))"
          testParseOutput "\"outer {{literal}} {\"inner {x}\"}\"" "\"outer {literal} %s\" % str(\"inner %s\" % str(x))"

        describe "Escape sequences" $ do
          -- Standard escapes (preserved during parsing)
          testParseOutput "\"\\n\\t\\r\"" "\"\\\\n\\\\t\\\\r\""

          -- Hex escapes (note splitting behavior when followed by hex chars)
          testParseOutput "\"\\x48ello\"" "\"\\\\x48\" \"ello\""  -- splits to prevent C reading too many hex digits
          testParseOutput "\"\\x41BC\"" "\"\\\\x41\" \"BC\""

          -- Unicode escapes
          testParseOutput "\"\\u0041\"" "\"\\\\u0041\""  -- 4-digit unicode
          testParseOutput "\"\\U00000041\"" "\"\\\\U00000041\""  -- 8-digit unicode

          -- Octal escapes
          testParseOutput "\"\\123\"" "\"\\\\123\""  -- 3-digit octal
          testParseOutput "\"\\7\"" "\"\\\\7\""  -- 1-digit octal

          -- Mixed with interpolation
          testParseOutput "f\"Hello \\n{name}\\t!\"" "\"Hello \\\\n%s\\\\t!\" % str(name)"

      -- ==== OTHER STRING LITERAL TESTS ====
      describe "Other String Literals" $ do

        describe "Raw strings (no interpolation)" $ do
          testParseOutput "r\"hello {world}\"" "\"hello {world}\""
          testParseOutput "r'test {x} value'" "\"test {x} value\""
          testParseOutput "r\"\"\"multi\nline {no} interpolation\"\"\"" "\"multi\\\\nline {no} interpolation\""
          testParseOutput "r\"path\\to\\file\"" "\"path\\\\\\\\to\\\\\\\\file\""
          testParseOutput "r\"regex: \\x[0-9a-f]+\"" "\"regex: \\\\\\\\x[0-9a-f]+\""
          testParseOutput "r\"test \\n \\t \\r\"" "\"test \\\\\\\\n \\\\\\\\t \\\\\\\\r\""

        describe "Bytes literals" $ do
          -- Basic bytes
          testParseOutput "b\"hello\"" " bhello"
          testParseOutput "b\"\"\"multi\nline\"\"\"" " bmulti\\nline"

          -- Bytes with hex escapes (note splitting behavior)
          testParseOutput "b\"\\x48ello\"" " b\\x48  bello"  -- splits hex from following text
          testParseOutput "b\"\\x48\\x65llo\"" " b\\x48\\x65llo"  -- multiple hex escapes

          -- Other escapes in bytes
          testParseOutput "b\"Hello\\nWorld\"" " bHello\\nWorld"

        describe "Raw bytes literals" $ do
          testParseOutput "rb\"hello\"" " bhello"
          testParseOutput "rb'world'" " bworld"
          testParseOutput "rb\"\"\"multi\nline\"\"\"" " bmulti\\nline"

        describe "Legacy percent formatting (not interpolated)" $ do
          testParseOutput "\"hello %s\" % name" "\"hello %s\" % name"
          testParseOutput "\"Value: %d\" % count" "\"Value: %d\" % count"

      -- ==== ERROR HANDLING TESTS ====
      describe "String Parsing Errors" $ do
        describe "Basic string errors" $ do
          testModuleParseError "unclosed_string" "a = \"hello"
          testModuleParseError "unclosed_string_triple" "z = 1\na = \"\"\"hello\nb = 3\ndef foo():\n    pass"
          testModuleParseError "unclosed_raw_string" "a = r\"hello"
          testModuleParseError "unclosed_bytes_string" "a = b\"hello"
          testModuleParseError "unclosed_raw_bytes_string" "a = rb\"hello"
          testModuleParseError "unclosed_raw_string_single" "a = r'hello"
          testModuleParseError "unclosed_bytes_string_single" "a = b'hello"
          testModuleParseError "unclosed_raw_bytes_string_single" "a = rb'hello"

        describe "Triple quote errors (6+ quotes)" $ do
          testModuleParseError "six_double_quotes" "a = \"\"\"\"\"\"test\"\"\"\"\"\""  -- 6 quotes each side
          testModuleParseError "six_single_quotes" "a = ''''''test''''''"  -- 6 quotes each side
          testModuleParseError "seven_double_quotes" "a = \"\"\"\"\"\"\"test\"\"\"\"\"\"\""  -- 7 quotes each side
          testModuleParseError "mixed_six_quotes_double" "a = \"\"\"\"\"\"mixed content with {interpolation}\"\"\"\"\"\""
          testModuleParseError "raw_six_quotes" "a = r\"\"\"\"\"\"raw test\"\"\"\"\"\""

        describe "Basic interpolation errors" $ do
          -- F-string errors
          testParseError "fstring_unclosed_brace" "f\"Unclosed brace: {name"
          testParseError "fstring_empty_expression" "f\"Empty expression {}\""
          testParseError "fstring_missing_expression" "f\"Missing expression {:10}\""
          testParseError "fstring_unbalanced_format" "f\"Unbalanced format {name:}:10}\""

          -- String interpolation errors (without f-prefix)
          testParseError "string_unclosed_brace" "\"Unclosed brace: {name"
          testParseError "tristring_unclosed_brace" "\"\"\"Unclosed brace: {name"
          testParseError "string_empty_expression" "\"Empty expression {}\""
          testParseError "string_missing_expression" "\"Missing expression {:10}\""

        describe "Format specification errors" $ do
          testParseError "fstring_invalid_format" "f\"Invalid format specifier {name:@Z}\""
          testParseError "fstring_invalid_alignment" "f\"{name:@10}\""
          testParseError "string_invalid_format" "\"{name:@Z}\""
          testParseError "fstring_empty_format_specifier" "f\"Empty format spec {x:}\""
          testParseError "fstring_missing_precision_digits_error" "f\"Missing precision digits {value:10.}\""
          testParseError "invalid_after_width" "\"value: {x:10@}\""
          testParseError "invalid_after_align" "\"value: {x:>10@}\""
          testParseError "invalid_after_precision" "\"value: {x:10.2@}\""
          testParseError "invalid_in_type_spec" "\"value: {x:10.2@f}\""

        describe "Nested interpolation errors" $ do
          testParseError "nested_unclosed_outer" "\"outer {\"inner"
          testParseError "nested_unclosed_inner" "\"outer {\"inner {x}\""
          testParseError "nested_empty_inner" "\"outer {\"inner {}\"}\""
          testParseError "nested_invalid_format" "\"outer {\"inner {x:@10}\"}\""
          testParseError "triple_nested_unclosed" "\"level1 {\"level2 {\"level3 {x"
          testParseError "mixed_quotes_unclosed" "\"outer {'inner {x"
          testParseError "escaped_brace_error" "\"outer {{broken {x}\""
          testParseError "nested_empty_format" "\"outer {\"inner {x:}\"}\""
          testParseError "nested_invalid_align" "\"outer {\"inner {x:@5}\"}\""
          testParseError "nested_bad_type_spec" "\"outer {\"inner {x:5@}\"}\""
          testParseError "deep_nesting_error" "\"a {\"b {\"c {\"d {\"e {f:@}\"}\"}\"}\"}\"}\""
          testParseError "alternating_quotes_error" "\"a {'b {\"c {'d {e'}\"}'}\""

        describe "Position-specific errors" $ do
          testParseError "error_at_start" "{x} at start"
          testParseError "error_at_end" "\"at end {x"
          testParseError "error_in_middle" "\"start {x:@} end\""
          testParseError "multiline_unclosed" "\"\"\"Line 1\nLine 2 {x\nLine 3\"\"\""
          testParseError "multiline_nested_error" "\"\"\"First line\n{\"inner\n  {broken}\n\"}\"\"\""
          testParseError "multiline_format_error" "\"\"\"\nValue: {x:@invalid}\nMore text\"\"\""
          testParseError "newline_in_expr" "\"value: {x\n}\""
          testParseError "tab_in_format" "\"value: {x:\t10}\""
          testParseError "unicode_format_error" "\"你好 {name:你}\""

        describe "Slice expression errors" $ do
          testParseError "slice_unclosed_bracket" "\"slice: {arr[1:3\""
          testParseError "slice_invalid_step" "\"slice: {arr[:::\""
          testParseError "slice_missing_bracket" "\"slice: {arr 1:3]}\""
          testParseError "slice_nested_error" "\"outer {arr[inner[}\""
          testParseError "slice_format_error" "\"slice: {arr[1:3]:@10}\""

      describe "Escape Sequence Errors" $ do

        describe "Hex escape errors" $ do
          testParseError "hex_incomplete_one_digit" "\"\\x4\""
          testParseError "hex_incomplete_no_digits" "\"\\x\""
          testParseError "hex_invalid_char" "\"\\xAG\""
          testParseError "hex_incomplete_in_interpolation" "f\"value: {x} and \\x4\""

        describe "Unicode escape errors" $ do
          testParseError "unicode_short_incomplete" "\"\\u123\""
          testParseError "unicode_short_no_digits" "\"\\u\""
          testParseError "unicode_short_invalid_char" "\"\\u123G\""
          testParseError "unicode_short_in_fstring" "f\"Hello \\u123\""
          testParseError "unicode_long_incomplete" "\"\\U1234567\""
          testParseError "unicode_long_no_digits" "\"\\U\""
          testParseError "unicode_long_invalid_char" "\"\\U1234567G\""
          testParseError "unicode_long_in_fstring" "f\"Hello \\U1234567\""

        describe "Octal escape errors" $ do
          testParseError "octal_out_of_range" "\"\\777\""
          testParseError "octal_invalid_first_digit" "\"\\8\""
          testParseError "octal_invalid_char" "\"\\12G\""
          testParseError "octal_out_of_range_in_fstring" "f\"value: {x} \\777\""

        describe "Unknown escape sequences" $ do
          testParseError "unknown_escape_p" "\"\\p\""
          testParseError "unknown_escape_z" "\"\\z\""
          testParseError "unknown_escape_in_fstring" "f\"unknown: \\q\""
          testParseError "unknown_escape_with_interpolation" "f\"value: {x} \\k\""

    describe "Documentation Generation" $ do
      testDocGen env0 ["bar", "foo"]

    describe "Pass 2: Kinds" $ do
      testKinds env0 ["deact"]

    describe "Pass 3: Types" $ do
      testTypes env0 ["deact"]
      testTypes env0 ["test_discovery"]

      testAttributesInitialization env0

      it "keeps generated names stable within unchanged defs" $ do
        let fooSource = unlines
              [ "def foo(xs: list[int], y: int):"
              , "    [xs][0][0] += y"
              ]
            warmSource = unlines
              [ "def warm(a):"
              , "    return (a + 1) * 2"
              , ""
              ]
        namesA <- typedDefGeneratedNames env0 "stable_names_a" fooSource "foo"
        namesB <- typedDefGeneratedNames env0 "stable_names_b" (warmSource ++ fooSource) "foo"
        namesA `shouldSatisfy` any ("W_foo_" `isPrefixOf`)
        namesA `shouldSatisfy` any ("V_foo_" `isPrefixOf`)
        namesA `shouldBe` namesB

      it "accepts docstrings and extension decls at top level" $ do
        let src = unlines
              [ "\"\"\"Module docstring\"\"\""
              , ""
              , "protocol Proto:"
              , "    f: () -> int"
              , ""
              , "class Cls:"
              , "    pass"
              , ""
              , "extension Cls(Proto):"
              , "    def f(self) -> int:"
              , "        return 0"
              ]
        tchecked <- typecheckSource env0 "tlname_doc_ext" src
        tchecked `shouldSatisfy` const True

      it "continues after a non-total top-level statement has been checked" $ do
        let src = unlines
              [ "value = 41"
              , ""
              , "def later() -> int:"
              , "    return value + 1"
              ]
        tchecked <- typecheckSource env0 "top_non_total_then_total" src
        tchecked `shouldSatisfy` const True

      it "reports inferred signatures for non-total top-level statements" $ do
        sigsRef <- liftIO $ newIORef []
        let src = "value = 41\n"
            moduleName = S.modName ["top_non_total_sig"]
            actFile = "<top_non_total_sig>"
            sysTypesPath = ".." </> ".." </> "dist" </> "base" </> "out" </> "types"
            onInferred names sig = modifyIORef' sigsRef ((names, sig) :)
        parsed <- liftIO $ P.parseModule moduleName actFile src Nothing
        env <- liftIO $ Acton.Env.mkEnv [sysTypesPath] env0 parsed
        kchecked <- liftIO $ Acton.Kinds.check env parsed
        _ <- liftIO $ Acton.Types.reconstruct Nothing (Just onInferred) env kchecked
        sigs <- liftIO $ reverse <$> readIORef sigsRef
        sigs `shouldSatisfy` any (\(names, sig) -> names == ["value"] && "value : int" `isInfixOf` sig)

      it "collects errors from independent total top-level statements" $ do
        let src = unlines
              [ "proc def first() -> int:"
              , "    return \"one\""
              , ""
              , "value = 1"
              , ""
              , "proc def second() -> int:"
              , "    return \"two\""
              ]
        result <- E.try (do
          tchecked <- typecheckSource env0 "top_total_errors" src
          E.evaluate (length (show tchecked))
          return ()) :: IO (Either Acton.Types.TypeErrors ())
        case result of
          Left (Acton.Types.TypeErrors errs) -> length errs `shouldBe` 2
          Right _ -> expectationFailure "Expected multiple type errors but type checking succeeded"

    describe "Import Semantics" $ do
      it "omits private names from the public interface" $ do
        (envA, parsedA) <- parseAct env0 "import_private_a"
        kcheckedA <- liftIO $ Acton.Kinds.check envA parsedA
        (nmodA, _, _, _) <- liftIO $ Acton.Types.reconstruct Nothing Nothing envA kcheckedA
        let I.NModule impsA tenvA mdocA = nmodA
            env1 = Acton.Env.addMod (S.modname parsedA) impsA tenvA mdocA env0

        (envB, _) <- parseAct env1 "import_private"
        case Acton.Env.lookupName (S.name "__foo") envB of
          Nothing -> pure ()
          Just _ -> expectationFailure "from import * should skip __foo"

        case Acton.Env.lookupName (S.name "public_value") envB of
          Just (I.HNAlias _) -> pure ()
          _ -> expectationFailure "from import * should include public_value"

      it "blocks qualified access to private names" $ do
        (envA, parsedA) <- parseAct env0 "import_private_a"
        kcheckedA <- liftIO $ Acton.Kinds.check envA parsedA
        (nmodA, _, _, _) <- liftIO $ Acton.Types.reconstruct Nothing Nothing envA kcheckedA
        let I.NModule impsA tenvA mdocA = nmodA
            publicTEnvA = Acton.Env.publicTEnv tenvA
            env1 = Acton.Env.addMod (S.modname parsedA) impsA publicTEnvA mdocA env0

        (envB, parsedB) <- parseAct env1 "import_private_qualified"
        kcheckedB <- liftIO $ Acton.Kinds.check envB parsedB
        result <- liftIO $ (E.try (do
          (_, tcheckedB, _, _) <- Acton.Types.reconstruct Nothing Nothing envB kcheckedB
          _ <- E.evaluate (rnf tcheckedB)
          pure ()
          ) :: IO (Either CompilationError ()))

        case result of
          Left NoItem{} -> pure ()
          Left err -> expectationFailure $ "Expected NoItem error, got " ++ show err
          Right _ -> expectationFailure "Expected type check failure for private name access"

    describe "Pass 4: Normalizer" $ do
      testNorm env0 ["deact"]

    describe "Pass 5: Deactorizer" $ do
      testDeact env0 ["deact", "deact_from_import"]

    describe "Pass 6: CPS" $ do
      testCps env0 ["cps_andor"]
      testCps env0 ["cps_volatiles"]
      testCps env0 ["cps_optchain"]

    describe "Pass 7: Lambda Lifting" $ do
      testLL env0 ["deact"]

    describe "Pass 8: Boxing" $ do
      testBoxing env0 ["deact"]

    describe "Pass 9: CodeGen" $ do
      testCodeGen env0 ["ints"]
      testCodeGen env0 ["deact"]
      testCodeGen env0 ["lines"]

    -- BuildSpec: parsing and update-in-place of Build.act (canonical layout)
    describe "BuildSpec" $ do
      it "parses canonical Build.act and dumps JSON" $ do
        let buildAct = unlines
              [ "# Canonical Build.act file"
              , "name = \"demo\""
              , "description = \"Demo project\""
              , "fingerprint = 0x1234abcd5678ef00"
              , ""
              , "# Dependencies section"
              , "dependencies = {"
              , "  \"a\": (path=\"deps/a\")"
              , "}"
              , ""
              , "# Zig dependencies section"
              , "zig_dependencies = {"
              , "  \"z\": ("
              , "        url=\"https://z\","
              , "        hash=\"abcd\","
              , "        artifacts=[\"z\"]"
              , "    )"
              , "}"
              , ""
              , "# bla bla bla"
              , "actor main(env: Env):"
              , "    pass"
              , ""
              ]
        case BuildSpec.parseBuildAct buildAct of
          Left err -> expectationFailure err
          Right (spec,_,_) -> do
            BuildSpec.fingerprint spec `shouldBe` "0x1234abcd5678ef00"
            let json = BuildSpec.encodeBuildSpecJSON spec
            case Ae.eitherDecode' json of
              Left err2 -> expectationFailure err2
              Right spec2 -> spec2 `shouldBe` spec

      it "errors when fingerprint prefix does not match name" $ do
        withSystemTempDirectory "acton-fp" $ \dir -> do
          let buildAct = unlines
                [ "name = \"demo\""
                , "fingerprint = 0x0000000000000000"
                , ""
                ]
          writeFile (dir </> "Build.act") buildAct
          res <- (E.try (Compile.loadBuildSpec dir) :: IO (Either Compile.ProjectError BuildSpec.BuildSpec))
          case res of
            Left (Compile.ProjectError msg) ->
              msg `shouldSatisfy` (isInfixOf "Fingerprint mismatch")
            Right _ ->
              expectationFailure "Expected fingerprint mismatch error"

      it "accepts matching fingerprint for name" $ do
        withSystemTempDirectory "acton-fp" $ \dir -> do
          let prefix = Fingerprint.fingerprintPrefixForName "demo"
              fp = Fingerprint.formatFingerprint ((fromIntegral prefix `shiftL` 32) .|. 0x1)
              buildAct = unlines
                [ "name = \"demo\""
                , "fingerprint = " ++ fp
                , ""
                ]
          writeFile (dir </> "Build.act") buildAct
          res <- (E.try (Compile.loadBuildSpec dir) :: IO (Either Compile.ProjectError BuildSpec.BuildSpec))
          case res of
            Left (Compile.ProjectError msg) ->
              expectationFailure ("Unexpected fingerprint error: " ++ msg)
            Right spec ->
              BuildSpec.fingerprint spec `shouldBe` fp

      it "errors when fingerprint is malformed" $ do
        withSystemTempDirectory "acton-fp" $ \dir -> do
          let buildAct = unlines
                [ "name = \"demo\""
                , "fingerprint = \"not-a-number\""
                , ""
                ]
          writeFile (dir </> "Build.act") buildAct
          res <- (E.try (Compile.loadBuildSpec dir) :: IO (Either Compile.ProjectError BuildSpec.BuildSpec))
          case res of
            Left (Compile.ProjectError msg) ->
              msg `shouldSatisfy` (isInfixOf "Invalid fingerprint")
            Right _ ->
              expectationFailure "Expected invalid fingerprint error"

      it "errors when fingerprint is quoted hex" $ do
        withSystemTempDirectory "acton-fp" $ \dir -> do
          let buildAct = unlines
                [ "name = \"demo\""
                , "fingerprint = \"0x1234abcd5678ef00\""
                , ""
                ]
          writeFile (dir </> "Build.act") buildAct
          res <- (E.try (Compile.loadBuildSpec dir) :: IO (Either Compile.ProjectError BuildSpec.BuildSpec))
          case res of
            Left (Compile.ProjectError msg) ->
              msg `shouldSatisfy` (isInfixOf "Invalid fingerprint")
            Right _ ->
              expectationFailure "Expected invalid fingerprint error"

      it "errors when fingerprint is decimal" $ do
        withSystemTempDirectory "acton-fp" $ \dir -> do
          let buildAct = unlines
                [ "name = \"demo\""
                , "fingerprint = 1234"
                , ""
                ]
          writeFile (dir </> "Build.act") buildAct
          res <- (E.try (Compile.loadBuildSpec dir) :: IO (Either Compile.ProjectError BuildSpec.BuildSpec))
          case res of
            Left (Compile.ProjectError msg) ->
              msg `shouldSatisfy` (isInfixOf "Invalid fingerprint")
            Right _ ->
              expectationFailure "Expected invalid fingerprint error"

      it "errors when name is missing" $ do
        withSystemTempDirectory "acton-fp" $ \dir -> do
          let buildAct = unlines
                [ "fingerprint = 0x1234abcd5678ef00"
                , ""
                ]
          writeFile (dir </> "Build.act") buildAct
          res <- (E.try (Compile.loadBuildSpec dir) :: IO (Either Compile.ProjectError BuildSpec.BuildSpec))
          case res of
            Left (Compile.ProjectError msg) ->
              msg `shouldSatisfy` (isInfixOf "Missing project name")
            Right _ ->
              expectationFailure "Expected missing project name error"

      it "errors when fingerprint is missing" $ do
        withSystemTempDirectory "acton-fp" $ \dir -> do
          let buildAct = unlines
                [ "name = \"demo\""
                , ""
                ]
          writeFile (dir </> "Build.act") buildAct
          res <- (E.try (Compile.loadBuildSpec dir) :: IO (Either Compile.ProjectError BuildSpec.BuildSpec))
          case res of
            Left (Compile.ProjectError msg) ->
              msg `shouldSatisfy` (isInfixOf "Missing fingerprint")
            Right _ ->
              expectationFailure "Expected missing fingerprint error"

      it "updates Build.act in-place, preserving comments and main actor" $ do
        let buildAct0 = unlines
              [ "# Canonical Build.act file"
              , "name = \"demo\""
              , "description = \"Demo project\""
              , "fingerprint = 0x1234abcd5678ef00"
              , ""
              , "# Dependencies section (keep my comments)"
              , "dependencies = {"
              , "  \"a\": (path=\"deps/a\")"
              , "}"
              , ""
              , "# Zig dependencies section"
              , "zig_dependencies = {"
              , "}"
              , ""
              , "# bla bla bla"
              , "actor main(env: Env):"
              , "    pass"
              , ""
              ]
        let prefix = Fingerprint.fingerprintPrefixForName "demo2"
            newFp = Fingerprint.formatFingerprint ((fromIntegral prefix `shiftL` 32) .|. 0x1)
            newJson = unlines
              [ "{"
              , "  \"name\": \"demo2\","
              , "  \"description\": \"Demo project v2\","
              , "  \"fingerprint\": \"" ++ newFp ++ "\","
              , "  \"dependencies\": {"
              , "    \"a\": {\"path\": \"deps/aa\"},"
              , "    \"b\": {\"url\": \"u\", \"hash\": \"h\"}"
              , "  },"
              , "  \"zig_dependencies\": {"
              , "    \"z\": {\"url\": \"zu\", \"hash\": \"zh\", \"artifacts\": [\"z\"]}"
              , "  }"
              , "}"
              ]
        case BuildSpec.updateBuildActFromJSON buildAct0 (BL.fromStrict (B8.pack newJson)) of
          Left err -> expectationFailure err
          Right buildAct1 -> do
            let expected = unlines
                  [ "# Canonical Build.act file"
                  , "name = \"demo2\""
                  , "description = \"Demo project v2\""
                  , "fingerprint = " ++ newFp
                  , ""
                  , "# Dependencies section (keep my comments)"
                  , "dependencies = {"
                  , "    \"a\": ("
                  , "        path=\"deps/aa\""
                  , "    ),"
                  , "    \"b\": ("
                  , "        url=\"u\","
                  , "        hash=\"h\""
                  , "    )"
                  , "}"
                  , ""
                  , "# Zig dependencies section"
                  , "zig_dependencies = {"
                  , "    \"z\": ("
                  , "        url=\"zu\","
                  , "        hash=\"zh\","
                  , "        artifacts=[\"z\"]"
                  , "    )"
                  , "}"
                  , ""
                  , "# bla bla bla"
                  , "actor main(env: Env):"
                  , "    pass"
                  , ""
                  ]
            buildAct1 `shouldBe` expected

      it "keeps hex fingerprints zero-padded when rewriting Build.act" $ do
        let buildAct0 = unlines
              [ "name = \"demo\""
              , "fingerprint = 0x056275683c869ace"
              , ""
              , "dependencies = {}"
              , ""
              , "zig_dependencies = {}"
              , ""
              ]
        case BuildSpec.parseBuildAct buildAct0 of
          Left err -> expectationFailure err
          Right (spec, _, _) ->
            case BuildSpec.updateBuildActFromJSON buildAct0 (BuildSpec.encodeBuildSpecJSON spec) of
              Left err -> expectationFailure err
              Right buildAct1 -> do
                buildAct1 `shouldSatisfy` (isInfixOf "fingerprint = 0x056275683c869ace")
                buildAct1 `shouldSatisfy` (not . isInfixOf "fingerprint = 0x56275683c869ace")

      it "rejects non-hex fingerprints in Build.act updates" $ do
        let buildAct0 = unlines
              [ "name = \"demo\""
              , "fingerprint = 0x1234abcd5678ef00"
              , ""
              , "dependencies = {}"
              , ""
              , "zig_dependencies = {}"
              , ""
              ]
            newJson = "{\"fingerprint\": \"1234\"}"
        case BuildSpec.updateBuildActFromJSON buildAct0 (BL.fromStrict (B8.pack newJson)) of
          Left err ->
            err `shouldSatisfy` (isInfixOf "64-bit hex literal")
          Right _ ->
            expectationFailure "Expected Build.act update to reject decimal fingerprint"

      it "parses Build.act with only dependencies (zig deps missing)" $ do
        let buildAct = unlines
              [ "# Only dependencies"
              , "name = \"demo\""
              , "fingerprint = 0x1234abcd5678ef00"
              , "dependencies = {"
              , "    \"a\": (path=\"deps/a\")"
              , "}"
              , ""
              , "actor main(env: Env):"
              , "    pass"
              , ""
              ]
        case BuildSpec.parseBuildAct buildAct of
          Left err -> expectationFailure err
          Right (spec,_,_) -> do
            -- deps present, zig deps empty
            BuildSpec.dependencies spec `shouldSatisfy` (not . null)
            BuildSpec.zig_dependencies spec `shouldSatisfy` null

      it "parses Build.act with only zig_dependencies (deps missing)" $ do
        let buildAct = unlines
              [ "# Only zig deps"
              , "name = \"demo\""
              , "fingerprint = 0x1234abcd5678ef00"
              , "zig_dependencies = {"
              , "    \"z\": ("
              , "        url=\"zu\","
              , "        hash=\"zh\","
              , "        artifacts=[\"z\"]"
              , "    )"
              , "}"
              , ""
              , "actor main(env: Env):"
              , "    pass"
              , ""
              ]
        case BuildSpec.parseBuildAct buildAct of
          Left err -> expectationFailure err
          Right (spec,_,_) -> do
            -- zig present, deps empty
            BuildSpec.zig_dependencies spec `shouldSatisfy` (not . null)
            BuildSpec.dependencies spec `shouldSatisfy` null

      it "parses Build.act with no dependency blocks" $ do
        let buildAct = unlines
              [ "# No deps here"
              , "name = \"demo\""
              , "fingerprint = 0x1234abcd5678ef00"
              , ""
              , "actor main(env: Env):"
              , "    pass"
              , ""
              ]
        case BuildSpec.parseBuildAct buildAct of
          Left err -> expectationFailure err
          Right (spec,_,_) -> do
            BuildSpec.dependencies spec `shouldSatisfy` null
            BuildSpec.zig_dependencies spec `shouldSatisfy` null

      it "appends missing zig_dependencies block when absent" $ do
        let buildAct0 = unlines
              [ "# Build with only dependencies"
              , "name = \"demo\""
              , "fingerprint = 0x1234abcd5678ef00"
              , "dependencies = {"
              , "    \"a\": (path=\"deps/a\")"
              , "}"
              , ""
              , "actor main(env: Env):"
              , "    pass"
              , ""
              ]
        let newJson = "{\n  \"zig_dependencies\": {\n    \"z\": {\"url\": \"zu\", \"hash\": \"zh\", \"artifacts\": [\"z\"]}\n  }\n}\n"
        case BuildSpec.updateBuildActFromJSON buildAct0 (BL.fromStrict (B8.pack newJson)) of
          Left err -> expectationFailure err
          Right buildAct1 -> do
            -- Original dependencies remain
            buildAct1 `shouldSatisfy` (isInfixOf $ unlines
              [ "    \"a\": ("
              , "        path=\"deps/a\""
              , "    )"
              ])
            -- Missing zig_dependencies appended with expected formatting
            buildAct1 `shouldSatisfy` (isInfixOf $ unlines
              [ "zig_dependencies = {"
              , "  \"z\": ("
              , "        url=\"zu\","
              , "        hash=\"zh\","
              , "        artifacts=[\"z\"]"
              , "    )"
              , "}"
              ])

      it "appends missing dependencies block when absent" $ do
        let buildAct0 = unlines
              [ "# Build with only zig deps"
              , "name = \"demo\""
              , "fingerprint = 0x1234abcd5678ef00"
              , "zig_dependencies = {"
              , "    \"z\": (url=\"zu\", hash=\"zh\", artifacts=[\"z\"])"
              , "}"
              , ""
              , "actor main(env: Env):"
              , "    pass"
              , ""
              ]
        let newJson = "{\n  \"dependencies\": {\n    \"a\": {\"path\": \"deps/a\"}\n  }\n}\n"
        case BuildSpec.updateBuildActFromJSON buildAct0 (BL.fromStrict (B8.pack newJson)) of
          Left err -> expectationFailure err
          Right buildAct1 -> do
            -- Original zig deps remain
            buildAct1 `shouldSatisfy` (isInfixOf $ unlines
              [ "    \"z\": ("
              , "        url=\"zu\","
              , "        hash=\"zh\","
              , "        artifacts=[\"z\"]"
              , "    )"
              ])
            -- Missing dependencies appended with expected formatting
            buildAct1 `shouldSatisfy` (isInfixOf $ unlines
              [ "dependencies = {"
              , "  \"a\": ("
              , "        path=\"deps/a\""
              , "    )"
              , "}"
              ])

      it "updates dependencies block with non-canonical whitespace, preserving outer line" $ do
        let buildAct0 = unlines
              [ "# Whitespace variant Build.act"
              , "name = \"demo\""
              , "fingerprint = 0x1234abcd5678ef00"
              , "dependencies  =   {"
              , ""
              , "  \"a\"  :  ( path = \"deps/a\" )"
              , "}"
              , ""
              , "actor main(env: Env):"
              , "    pass"
              , ""
              ]
        let newJson = "{\n  \"dependencies\": {\n    \"a\": {\"path\": \"deps/aa\"}\n  }\n}\n"
        case BuildSpec.updateBuildActFromJSON buildAct0 (BL.fromStrict (B8.pack newJson)) of
          Left err -> expectationFailure err
          Right buildAct1 -> do
            -- Outer label line (with extra spaces) is preserved
            buildAct1 `shouldSatisfy` (isInfixOf "dependencies  =   {")
            -- Inner body is canonicalised and updated
            buildAct1 `shouldSatisfy` (isInfixOf $ unlines
              [ "    \"a\": ("
              , "        path=\"deps/aa\""
              , "    )"
              ])

      it "updates only the overlaid dependency entry, preserving others" $ do
        let buildAct0 = unlines
              [ "# Multiple dependencies"
              , "name = \"demo\""
              , "fingerprint = 0x1234abcd5678ef00"
              , "dependencies = {"
              , "    \"a\": (path=\"deps/a\"),"
              , "    \"b\": (path=\"deps/b\")"
              , "}"
              , ""
              , "actor main(env: Env):"
              , "    pass"
              , ""
              ]
        let newJson = "{\n  \"dependencies\": {\n    \"b\": {\"path\": \"deps/bb\"}\n  }\n}\n"
        case BuildSpec.updateBuildActFromJSON buildAct0 (BL.fromStrict (B8.pack newJson)) of
          Left err -> expectationFailure err
          Right buildAct1 -> do
            -- JSON dependencies is treated as full set: only "b" remains, with updated path.
            buildAct1 `shouldSatisfy` (not . isInfixOf "    \"a\": (path=\"deps/a\"),")
            buildAct1 `shouldSatisfy` (isInfixOf $ unlines
              [ "    \"b\": ("
              , "        path=\"deps/bb\""
              , "    )"
              ])

      it "removes all dependencies and zig deps when new spec objects are empty" $ do
        let buildAct0 = unlines
              [ "# Canonical Build.act file"
              , "name = \"demo\""
              , "fingerprint = 0x1234abcd5678ef00"
              , ""
              , "# Dependencies section (keep my comments)"
              , "dependencies = {"
              , "    \"a\": (path=\"deps/a\")"
              , "}"
              , ""
              , "# Zig dependencies section"
              , "zig_dependencies = {"
              , "    \"z\": ("
              , "        url=\"zu\","
              , "        hash=\"zh\","
              , "        artifacts=[\"z\"]"
              , "    )"
              , "}"
              , ""
              , "# bla bla bla"
              , "actor main(env: Env):"
              , "    pass"
              , ""
              ]
        let newJson = "{\n  \"dependencies\": {},\n  \"zig_dependencies\": {}\n}\n"
        case BuildSpec.updateBuildActFromJSON buildAct0 (BL.fromStrict (B8.pack newJson)) of
          Left err -> expectationFailure err
          Right buildAct1 -> do
            let expected = unlines
                  [ "# Canonical Build.act file"
                  , "name = \"demo\""
                  , "fingerprint = 0x1234abcd5678ef00"
                  , ""
                  , "# Dependencies section (keep my comments)"
                  , "dependencies = {"
                  , "}"
                  , ""
                  , "# Zig dependencies section"
                  , "zig_dependencies = {"
                  , "}"
                  , ""
                  , "# bla bla bla"
                  , "actor main(env: Env):"
                  , "    pass"
                  , ""
                  ]
            buildAct1 `shouldBe` expected


-- Helper function to format custom parse errors consistently
formatCustomParseError :: String -> String -> SrcLoc -> P.CustomParseError -> String
formatCustomParseError filename input loc err =
  let diagnostic = Diag.customParseErrorDiagnostic "Syntax error" filename input loc err
      doc = prettyDiagnostic WithUnicode (TabSize 4) diagnostic
      layout = layoutPretty defaultLayoutOptions (unAnnotate doc)
  in T.unpack $ renderStrict layout

withTrailingNewline :: String -> String
withTrailingNewline s
  | null s = s
  | last s == '\n' = s
  | otherwise = s ++ "\n"

parseActon :: String -> Either String String
parseActon input =
  System.IO.Unsafe.unsafePerformIO $
    E.catch
      (E.evaluate $ case runParser (St.evalStateT P.stmt P.initState) "" inputWithNewline of
        Left err -> Left $ renderDiagnostic err
        Right result -> Right $ concatMap (Pretty.print) result)
      handleCustomParseException
  where
    inputWithNewline = withTrailingNewline input
    handleCustomParseException :: P.CustomParseException -> IO (Either String String)
    renderDiagnostic err =
      let diagnostic = Diag.parseDiagnosticFromBundle "test" inputWithNewline err
          doc = prettyDiagnostic WithUnicode (TabSize 4) diagnostic
          layout = layoutPretty defaultLayoutOptions (unAnnotate doc)
      in T.unpack $ renderStrict layout
    handleCustomParseException (P.CustomParseException loc err) =
      return $ Left $ formatCustomParseError "test" inputWithNewline loc err

-- Helper function to parse a full module (for testing module-level constructs)
parseModuleTest :: String -> Either String String
parseModuleTest input =
  System.IO.Unsafe.unsafePerformIO $
    E.catch
      (E.evaluate $ case runParser (St.evalStateT P.file_input P.initState) "test.act" inputWithNewline of
        Left err -> Left $ renderDiagnostic err
        Right (_imports, _mdoc, _suite) -> Right $ "Module parsed successfully")
      handleCustomParseException
  where
    inputWithNewline = withTrailingNewline input
    renderDiagnostic err =
      let diagnostic = Diag.parseDiagnosticFromBundle "test.act" inputWithNewline err
          doc = prettyDiagnostic WithUnicode (TabSize 4) diagnostic
          layout = layoutPretty defaultLayoutOptions (unAnnotate doc)
      in T.unpack $ renderStrict layout
    handleCustomParseException :: P.CustomParseException -> IO (Either String String)
    handleCustomParseException (P.CustomParseException loc err) =
      return $ Left $ formatCustomParseError "test.act" inputWithNewline loc err

expectChunkedParseMatchesSerial :: String -> Expectation
expectChunkedParseMatchesSerial input = do
  let actFile = "chunked.act"
  expectChunkedParseMatchesSerialFile actFile input

expectChunkedParseMatchesSerialFile :: FilePath -> String -> Expectation
expectChunkedParseMatchesSerialFile actFile input = do
  let moduleName = S.modName ["chunked"]
  serial <- P.parseModuleSerial moduleName actFile input Nothing
  chunked <- P.parseModule moduleName actFile input Nothing
  when (chunked /= serial) $
    expectationFailure ("Chunked parser AST differs from serial parser for " ++ actFile)

parseBundleErrorLine :: ParseErrorBundle String P.CustomParseError -> Int
parseBundleErrorLine bundle =
  let firstError = NE.head (bundleErrors bundle)
      (_, posState) = reachOffset (errorOffset firstError) (bundlePosState bundle)
  in unPos (sourceLine (pstateSourcePos posState))

renderDiagnosticText diagnostic =
  let doc = prettyDiagnostic WithUnicode (TabSize 4) diagnostic
      layout = layoutPretty defaultLayoutOptions (unAnnotate doc)
  in T.unpack $ renderStrict layout

actFilesUnder :: FilePath -> IO [FilePath]
actFilesUnder root = do
  entries <- sort <$> listDirectory root
  fmap concat $ mapM (\entry -> do
    let path = root </> entry
    isDir <- doesDirectoryExist path
    if isDir
      then actFilesUnder path
      else return [path | takeExtension path == ".act"]
    ) entries

expectModuleParseSuccess :: String -> IO ()
expectModuleParseSuccess input =
  case parseModuleTest input of
    Left err -> expectationFailure $ "Parse failed: " ++ err
    Right _ -> return ()

expectModuleParseFailure :: String -> IO String
expectModuleParseFailure input =
  case parseModuleTest input of
    Left err -> return err
    Right result -> expectationFailure $ "Expected parse failure, got: " ++ result

completionFixtureEnv :: Acton.Env.Env0 -> Acton.Env.Env0
completionFixtureEnv env0 =
  let y1Mod = S.modName ["mini", "layers", "y_1"]
      baseMod = S.modName ["mini", "layers", "base_1"]
      inputName = S.name "stratoweave_rfs__rfs__l3vpn_endpoint_entry"
      inputType = S.tCon (S.TC (S.GName y1Mod inputName) [])
      localInputName = S.name "LocalInput"
      localInputType = S.tCon (S.TC (S.NoQ localInputName) [])
      transformType =
        S.tFun S.fxMut
          (S.posRow inputType (S.posRow S.tWild S.posNil))
          S.kwdNil
          S.tWild
      transformInfo = I.NSig (S.tSchema [] transformType) S.NoDec Nothing
      localTransformType =
        S.tFun S.fxMut
          (S.posRow localInputType S.posNil)
          S.kwdNil
          S.tWild
      localTransformInfo = I.NSig (S.tSchema [] localTransformType) S.NoDec Nothing
      outputRootName = S.name "OutputRoot"
      outputRootType = S.tCon (S.TC (S.GName baseMod outputRootName) [])
      netinfraName = S.name "Netinfra"
      netinfraType = S.tCon (S.TC (S.GName baseMod netinfraName) [])
      routerCollectionName = S.name "RouterCollection"
      routerCollectionType = S.tCon (S.TC (S.GName baseMod routerCollectionName) [])
      routerEntryName = S.name "RouterEntry"
      routerEntryType = S.tCon (S.TC (S.GName baseMod routerEntryName) [])
      baseConfigName = S.name "BaseConfig"
      baseConfigType = S.tCon (S.TC (S.GName baseMod baseConfigName) [])
      oRootType = S.tFun S.fxPure S.posNil S.kwdNil outputRootType
      oRootInfo = I.NDef (S.tSchema [] oRootType) S.NoDec Nothing
      createType =
        S.tFun S.fxMut
          (S.posRow Builtin.tStr S.posNil)
          (S.kwdRow (S.name "id") Builtin.tInt
            (S.kwdRow (S.name "role") Builtin.tStr
              (S.kwdRow (S.name "mock") Builtin.tBool S.kwdNil)))
          routerEntryType
      createInfo = I.NDef (S.tSchema [] createType) S.NoDec (Just "Create a router entry.")
      baseClass = I.NClass [] [] [(S.name "transform", transformInfo)] (Just "Base transform class.")
      localBaseClass = I.NClass [] [] [(S.name "transform", localTransformInfo)] Nothing
      outputRootClass = I.NClass [] []
        [ fieldOf "netinfra" netinfraType
        , field "l3vpns"
        ] Nothing
      netinfraClass = I.NClass [] []
        [ fieldOf "router" routerCollectionType
        ] Nothing
      routerCollectionClass = I.NClass [] []
        [ (S.name "create", createInfo)
        ] Nothing
      routerEntryClass = I.NClass [] []
        [ field "router_id"
        , field "hostname"
        , fieldOf "base_config" baseConfigType
        ] Nothing
      baseConfigClass = I.NClass [] []
        [ docFieldOf "asn" Builtin.tInt "Autonomous system number."
        , fieldOf "ipv4_address" Builtin.tStr
        ] (Just "Base router configuration.")
      localInputClass = I.NClass [] []
        [ field "local_field"
        ] Nothing
      inputClass = I.NClass [] []
        [ field "interface_name"
        , field "vpn_name"
        , field "customer_name"
        , field "ipv4_address"
        ] (Just "L3VPN endpoint input.")
      field n = fieldOf n S.tWild
      fieldOf n typ = (S.name n, I.NVar typ)
      docFieldOf n typ doc = (S.name n, I.NSig (S.tSchema [] typ) S.NoDec (Just doc))
  in Acton.Env.addMod baseMod []
       [ (S.name "L3vpnEndpoint", baseClass)
       , (S.name "LocalEndpoint", localBaseClass)
       , (localInputName, localInputClass)
       , (outputRootName, outputRootClass)
       , (netinfraName, netinfraClass)
       , (routerCollectionName, routerCollectionClass)
       , (routerEntryName, routerEntryClass)
       , (baseConfigName, baseConfigClass)
       , (S.name "o_root", oRootInfo)
       ] Nothing $
     Acton.Env.addMod y1Mod [] [(inputName, inputClass)] Nothing env0

cursorSource :: String -> (String, Int)
cursorSource src =
  let marker = T.pack "<CURSOR>"
      (before, after) = T.breakOn marker (T.pack src)
  in if T.null after
       then (src, length src)
       else (T.unpack before ++ T.unpack (T.drop (T.length marker) after), T.length before)

parseStmtAst :: String -> Either String [S.Stmt]
parseStmtAst input =
  System.IO.Unsafe.unsafePerformIO $
    E.catch
      (E.evaluate $ case runParser (St.evalStateT P.stmt P.initState) "" inputWithNewline of
        Left err -> Left $ renderDiagnostic err
        Right result -> Right result)
      handleCustomParseException
  where
    inputWithNewline = withTrailingNewline input
    renderDiagnostic err =
      let diagnostic = Diag.parseDiagnosticFromBundle "test" inputWithNewline err
          doc = prettyDiagnostic WithUnicode (TabSize 4) diagnostic
          layout = layoutPretty defaultLayoutOptions (unAnnotate doc)
      in T.unpack $ renderStrict layout
    handleCustomParseException :: P.CustomParseException -> IO (Either String [S.Stmt])
    handleCustomParseException (P.CustomParseException loc err) =
      return $ Left $ formatCustomParseError "test" inputWithNewline loc err

parseExprAst :: String -> Either String S.Expr
parseExprAst input =
  System.IO.Unsafe.unsafePerformIO $
    E.catch
      (E.evaluate $ case runParser (St.evalStateT P.expr P.initState) "" inputWithNewline of
        Left err -> Left $ renderDiagnostic err
        Right result -> Right result)
      handleCustomParseException
  where
    inputWithNewline = withTrailingNewline input
    renderDiagnostic err =
      let diagnostic = Diag.parseDiagnosticFromBundle "test" inputWithNewline err
          doc = prettyDiagnostic WithUnicode (TabSize 4) diagnostic
          layout = layoutPretty defaultLayoutOptions (unAnnotate doc)
      in T.unpack $ renderStrict layout
    handleCustomParseException :: P.CustomParseException -> IO (Either String S.Expr)
    handleCustomParseException (P.CustomParseException loc err) =
      return $ Left $ formatCustomParseError "test" inputWithNewline loc err

-- Helper function to test module-level parser errors with golden files
testModuleParseError :: String -> String -> Spec
testModuleParseError testName input = do
  it testName $ do
    case parseModuleTest input of
      Left err -> goldenTextFile ("test/parser_golden/" ++ testName ++ ".golden") $
        return $ T.pack $ "ERROR: " ++ err
      Right result -> goldenTextFile ("test/parser_golden/" ++ testName ++ ".golden") $
        return $ T.pack $ "PARSED: " ++ result

-- Helper function to test parsing (just that it succeeds)
testParse :: Acton.Env.Env0 -> [String] -> Spec
testParse env0 modulePaths = do
  let dir = "test" </> "1-parse"

  modules <- runIO $ do
    let processModule (accEnv, accModules) modulePath = do
          (env, parsed) <- parseAct accEnv modulePath
          return (accEnv, accModules ++ [(takeFileName modulePath, parsed)])

    (_, modules) <- foldM processModule (env0, []) modulePaths
    return modules

  forM_ modules $ \(modName, parsed) ->
    genTests "Parse Check" dir modName parsed parsed

-- Helper function to test parsing with output validation
testParseOutput :: String -> String -> Spec
testParseOutput input expected = do
  it (show input) $ do
    case parseActon input of
      Left err -> expectationFailure $ "Parse failed: " ++ err
      Right output -> output `shouldBe` expected

-- Helper function to test parser errors with golden files
testParseError :: String -> String -> Spec
testParseError testName input = do
  it testName $ do
    case parseActon input of
      Left err -> goldenTextFile ("test/parser_golden/" ++ testName ++ ".golden") $
        return $ T.pack $ "ERROR: " ++ err
      Right result -> goldenTextFile ("test/parser_golden/" ++ testName ++ ".golden") $
        return $ T.pack $ "PARSED: " ++ result

-- Helper function for documentation golden tests
-- Takes a list of module paths in dependency order
-- Examples: ["bar", "foo"] or ["utils/math", "utils/strings", "app"]
-- Generates golden files for all formats: .txt (ASCII), .md (Markdown), .html (HTML)
testDocGen :: Acton.Env.Env0 -> [String] -> Spec
testDocGen env0 modulePaths = do
  oldDir <- runIO getCurrentDirectory
  let goldenDir = oldDir </> "test" </> "doc-golden"

  modules <- runIO $ do
    let processModule (accEnv, accModules) modulePath = do
          (env, parsed) <- parseAct accEnv modulePath
          kchecked <- Acton.Kinds.check env parsed
          (nmod, _, _, _) <- Acton.Types.reconstruct Nothing Nothing env kchecked
          let I.NModule imps moduleTypeEnv moduleDoc = nmod
          let newAccEnv = Acton.Env.addMod (S.modname parsed) imps moduleTypeEnv moduleDoc accEnv
          return (newAccEnv, accModules ++ [((takeFileName modulePath), parsed, nmod)])

    (finalEnv, modules) <- foldM processModule (env0, []) modulePaths
    return modules

  -- Generate documentation for each module
  forM_ modules $ \(modName, parsed, nmod) -> do
    describe modName $ do
      it "generates ASCII documentation (plain)" $ do
        let asciiDoc = DocP.printAsciiDoc False nmod parsed
        goldenTextFile (goldenDir </> modName ++ ".txt") $ return $ T.pack asciiDoc

      it "generates ASCII documentation (styled)" $ do
        let asciiDoc = DocP.printAsciiDoc True nmod parsed
        goldenTextFile (goldenDir </> modName ++ "-color.txt") $ return $ T.pack asciiDoc

      it "generates Markdown documentation" $ do
        let mdDoc = DocP.printMdDoc nmod parsed
        goldenTextFile (goldenDir </> modName ++ ".md") $ return $ T.pack mdDoc

      it "generates HTML documentation" $ do
        let htmlDoc = DocP.printHtmlDoc nmod parsed
        goldenTextFile (goldenDir </> modName ++ ".html") $ return $ T.pack htmlDoc

-- Parse an Acton module by module path
-- Examples: "foo" -> src/foo.act, module foo
--           "foo/bar" -> src/foo/bar.act, module foo.bar
parseAct env0 modulePath = do
  let moduleComponents = splitDirectories modulePath
      moduleName = S.modName moduleComponents
      act_file = "test" </> "src" </> modulePath ++ ".act"
      sysTypesPath = ".." </> ".." </> "dist" </> "base" </> "out" </> "types"

  src <- liftIO $ readFile act_file
  parsed <- liftIO $ P.parseModule moduleName act_file src Nothing
  env <- liftIO $ Acton.Env.mkEnv [sysTypesPath] env0 parsed
  return (env, parsed)

typecheckSource env0 modName src = do
  let moduleName = S.modName [modName]
      actFile = "<" ++ modName ++ ">"
      sysTypesPath = ".." </> ".." </> "dist" </> "base" </> "out" </> "types"
  parsed <- liftIO $ P.parseModule moduleName actFile src Nothing
  env <- liftIO $ Acton.Env.mkEnv [sysTypesPath] env0 parsed
  kchecked <- liftIO $ Acton.Kinds.check env parsed
  (_, tchecked, _, _) <- liftIO $ Acton.Types.reconstruct Nothing Nothing env kchecked
  return tchecked

typedDefGeneratedNames env0 modName src defName = do
  tchecked <- typecheckSource env0 modName src
  case findTypedDef defName tchecked of
    Just d ->
      pure $ generatedInternalNames (Pretty.print d)
    Nothing ->
      expectationFailure ("Definition not found: " ++ defName) >> pure []

findTypedDef defName (S.Module _ _ _ ss) = findInSuite ss
  where findInSuite [] = Nothing
        findInSuite (S.Decl _ ds : ss) =
          case [ d | d@(S.Def _ n _ _ _ _ _ _ _ _) <- ds, prstr n == defName ] of
            d : _ -> Just d
            [] -> findInSuite ss
        findInSuite (_ : ss) = findInSuite ss

generatedInternalNames s = sort . nub . filter isGenerated $ words $ map keep s
  where keep c
          | isAlphaNum c || c == '_' = c
          | otherwise                = ' '
        isGenerated n                = "W_" `isPrefixOf` n || "V_" `isPrefixOf` n


genTests pass_name dir testname input_data output_data = do
  let input_golden = dir </> testname ++ ".input"
      output_golden = dir </> testname ++ ".output"

  describe testname $ do
    it ("Check " ++ pass_name ++ " input") $ do
      goldenTextFile input_golden $ return $ T.pack $ Pretty.print input_data
    it ("Check " ++ pass_name ++ " output") $ do
      goldenTextFile output_golden $ return $ T.pack $ Pretty.print output_data

-- pass 2 Kinds check
testKinds :: Acton.Env.Env0 -> [String] -> Spec
testKinds env0 modulePaths = do
  let dir = "test" </> "2-kinds"

  modules <- runIO $ do
    let processModule (accEnv, accModules) modulePath = do
          (env, parsed) <- parseAct accEnv modulePath
          kchecked <- Acton.Kinds.check env parsed
          return (accEnv, accModules ++ [(takeFileName modulePath, parsed, kchecked)])

    (_, modules) <- foldM processModule (env0, []) modulePaths
    return modules

  forM_ modules $ \(modName, parsed, kchecked) ->
    genTests "Kinds Check" dir modName parsed kchecked

-- pass 3 Type check
testTypes :: Acton.Env.Env0 -> [String] -> Spec
testTypes env0 modulePaths = do
  let dir = "test" </> "3-types"

  modules <- runIO $ do
    let processModule (accEnv, accModules) modulePath = do
          (env, parsed) <- parseAct accEnv modulePath
          kchecked <- Acton.Kinds.check env parsed
          (nmod, tchecked, _, _) <- Acton.Types.reconstruct Nothing Nothing env kchecked
          let I.NModule imps tenv mdoc = nmod
          let newAccEnv = Acton.Env.addMod (S.modname parsed) imps tenv mdoc accEnv
          return (newAccEnv, accModules ++ [(takeFileName modulePath, kchecked, tchecked)])

    (_, modules) <- foldM processModule (env0, []) modulePaths
    return modules

  forM_ modules $ \(modName, kchecked, tchecked) ->
    genTests "Type Check" dir modName kchecked tchecked

-- pass 4 Normalizer
testNorm :: Acton.Env.Env0 -> [String] -> Spec
testNorm env0 modulePaths = do
  let dir = "test" </> "4-normalizer"

  modules <- runIO $ do
    let processModule (accEnv, accModules) modulePath = do
          (env, parsed) <- parseAct accEnv modulePath
          kchecked <- Acton.Kinds.check env parsed
          (nmod, tchecked, env0Typed, _) <- Acton.Types.reconstruct Nothing Nothing env kchecked
          let I.NModule imps tenv mdoc = nmod
          (normalized, normEnv) <- Acton.Normalizer.normalize env0Typed tchecked
          let newAccEnv = Acton.Env.addMod (S.modname parsed) imps tenv mdoc accEnv
          return (newAccEnv, accModules ++ [(takeFileName modulePath, tchecked, normalized)])

    (_, modules) <- foldM processModule (env0, []) modulePaths
    return modules

  forM_ modules $ \(modName, tchecked, normalized) ->
    genTests "Normalizer" dir modName tchecked normalized

-- pass 5 Deactorizer
testDeact :: Acton.Env.Env0 -> [String] -> Spec
testDeact env0 modulePaths = do
  let dir = "test" </> "5-deactorizer"

  modules <- runIO $ do
    let processModule (accEnv, accModules) modulePath = do
          (env, parsed) <- parseAct accEnv modulePath
          kchecked <- Acton.Kinds.check env parsed
          (nmod, tchecked, env0Typed, _) <- Acton.Types.reconstruct Nothing Nothing env kchecked
          let I.NModule imps tenv mdoc = nmod
          (normalized, normEnv) <- Acton.Normalizer.normalize env0Typed tchecked
          (deacted, deactEnv) <- Acton.Deactorizer.deactorize normEnv normalized
          let newAccEnv = Acton.Env.addMod (S.modname parsed) imps tenv mdoc accEnv
          return (newAccEnv, accModules ++ [(takeFileName modulePath, normalized, deacted)])

    (_, modules) <- foldM processModule (env0, []) modulePaths
    return modules

  forM_ modules $ \(modName, normalized, deacted) ->
    genTests "Deactorizer" dir modName normalized deacted

-- pass 6 CPS
testCps :: Acton.Env.Env0 -> [String] -> Spec
testCps env0 modulePaths = do
  let dir = "test" </> "6-cps"

  modules <- runIO $ do
    let processModule (accEnv, accModules) modulePath = do
          (env, parsed) <- parseAct accEnv modulePath
          kchecked <- Acton.Kinds.check env parsed
          (nmod, tchecked, env0Typed, _) <- Acton.Types.reconstruct Nothing Nothing env kchecked
          let I.NModule imps tenv mdoc = nmod
          (normalized, normEnv) <- Acton.Normalizer.normalize env0Typed tchecked
          (deacted, deactEnv) <- Acton.Deactorizer.deactorize normEnv normalized
          (cpstyled, _) <- Acton.CPS.convert deactEnv deacted
          let newAccEnv = Acton.Env.addMod (S.modname parsed) imps tenv mdoc accEnv
          return (newAccEnv, accModules ++ [(takeFileName modulePath, deacted, cpstyled)])

    (_, modules) <- foldM processModule (env0, []) modulePaths
    return modules

  forM_ modules $ \(modName, deacted, cpstyled) ->
    genTests "CPS" dir modName deacted cpstyled

-- pass 7 Lambda Lifting
testLL :: Acton.Env.Env0 -> [String] -> Spec
testLL env0 modulePaths = do
  let dir = "test" </> "7-lambdalifting"

  modules <- runIO $ do
    let processModule (accEnv, accModules) modulePath = do
          (env, parsed) <- parseAct accEnv modulePath
          kchecked <- Acton.Kinds.check env parsed
          (nmod, tchecked, env0Typed, _) <- Acton.Types.reconstruct Nothing Nothing env kchecked
          let I.NModule imps tenv mdoc = nmod
          (normalized, normEnv) <- Acton.Normalizer.normalize env0Typed tchecked
          (deacted, deactEnv) <- Acton.Deactorizer.deactorize normEnv normalized
          (cpstyled, cpsEnv) <- Acton.CPS.convert deactEnv deacted
          (lifted,liftEnv) <- Acton.LambdaLifter.liftModule cpsEnv cpstyled
          let newAccEnv = Acton.Env.addMod (S.modname parsed) imps tenv mdoc accEnv
          return (newAccEnv, accModules ++ [(takeFileName modulePath, cpstyled, lifted)])

    (_, modules) <- foldM processModule (env0, []) modulePaths
    return modules

  forM_ modules $ \(modName, cpstyled, lifted) ->
    genTests "Lambda Lifting" dir modName cpstyled lifted

-- pass 8 Boxing
testBoxing :: Acton.Env.Env0 -> [String] -> Spec
testBoxing env0 modulePaths = do
  let dir = "test" </> "8-boxing"

  modules <- runIO $ do
    let processModule (accEnv, accModules) modulePath = do
          (env, parsed) <- parseAct accEnv modulePath
          kchecked <- Acton.Kinds.check env parsed
          (nmod, tchecked, env0Typed, _) <- Acton.Types.reconstruct Nothing Nothing env kchecked
          let I.NModule imps tenv mdoc = nmod
          (normalized, normEnv) <- Acton.Normalizer.normalize env0Typed tchecked
          (deacted, deactEnv) <- Acton.Deactorizer.deactorize normEnv normalized
          (cpstyled, cpsEnv) <- Acton.CPS.convert deactEnv deacted
          (lifted,liftEnv) <- Acton.LambdaLifter.liftModule cpsEnv cpstyled
          boxed <- Acton.Boxing.doBoxing liftEnv lifted
          let newAccEnv = Acton.Env.addMod (S.modname parsed) imps tenv mdoc accEnv
          return (newAccEnv, accModules ++ [(takeFileName modulePath, lifted, boxed)])

    (_, modules) <- foldM processModule (env0, []) modulePaths
    return modules

  forM_ modules $ \(modName, lifted, boxed) ->
    genTests "Boxing" dir modName lifted boxed

-- pass 9 CodeGen
testCodeGen :: Acton.Env.Env0 -> [String] -> Spec
testCodeGen env0 modulePaths = do
  let dir = "test" </> "9-codegen"

  modules <- runIO $ do
    let processModule (accEnv, accModules) modulePath = do
          (env, parsed) <- parseAct accEnv modulePath
          kchecked <- Acton.Kinds.check env parsed
          (nmod, tchecked, env0Typed, _) <- Acton.Types.reconstruct Nothing Nothing env kchecked
          let I.NModule imps tenv mdoc = nmod
          (normalized, normEnv) <- Acton.Normalizer.normalize env0Typed tchecked
          (deacted, deactEnv) <- Acton.Deactorizer.deactorize normEnv normalized
          (cpstyled, cpsEnv) <- Acton.CPS.convert deactEnv deacted
          (lifted,liftEnv) <- Acton.LambdaLifter.liftModule cpsEnv cpstyled
          boxed <- Acton.Boxing.doBoxing liftEnv lifted
          let act_file = "test" </> "src" </> modulePath ++ ".act"
          srcText <- readFile act_file
          let srcbase = "test" </> "src" </> modulePath
          (n,h,c) <- Acton.CodeGen.generate liftEnv srcbase srcText True boxed "test-hash"
          let newAccEnv = Acton.Env.addMod (S.modname parsed) imps tenv mdoc accEnv
          return (newAccEnv, accModules ++ [(takeFileName modulePath, boxed, n, h, c)])

    (_, modules) <- foldM processModule (env0, []) modulePaths
    return modules

  forM_ modules $ \(modName, boxed, n, h, c) -> do
    let pass_name = "CodeGen"
    let input_golden = dir </> modName ++ ".input"
        h_golden = dir </> modName ++ ".h"
        c_golden = dir </> modName ++ ".c"

    describe modName $ do
      it ("Check " ++ pass_name ++ " input") $ do
        goldenTextFile input_golden $ return $ T.pack $ Pretty.print boxed
      it ("Check " ++ pass_name ++ " .h output") $ do
        goldenTextFile h_golden $ return $ T.pack $ Pretty.print h
      it ("Check " ++ pass_name ++ " .c output") $ do
        goldenTextFile c_golden $ return $ T.pack $ Pretty.print c


testDocstrings :: Acton.Env.Env0 -> String -> Spec
testDocstrings env0 testname = do
  (env, parsed) <- parseAct env0 testname

  kchecked <- liftIO $ Acton.Kinds.check env parsed
  (nmod, _, _, _) <- liftIO $ Acton.Types.reconstruct Nothing Nothing env kchecked
  let I.NModule _ tenv mdoc = nmod

  -- Extract docstrings from the parsed AST
  let S.Module _ _ _ stmts = parsed
      extractDeclDocstrings (S.Decl _ decls) = concatMap extractDocFromDecl decls
      extractDeclDocstrings _ = []

      extractDocFromDecl (S.Def _ n _ _ _ _ _ _ _ ddoc) = [(S.nstr n, ddoc)]
      extractDocFromDecl (S.Class _ n _ _ _ ddoc) = [(S.nstr n, ddoc)]
      extractDocFromDecl (S.Actor _ n _ _ _ _ ddoc) = [(S.nstr n, ddoc)]
      extractDocFromDecl (S.Protocol _ n _ _ _ ddoc) = [(S.nstr n, ddoc)]
      extractDocFromDecl (S.Extension _ _ _ _ _ ddoc) = [("extension", ddoc)]

      docstrings = concatMap extractDeclDocstrings stmts

  describe testname $ do
    -- Basic functionality tests
    it "extracts module docstrings" $ do
      case mdoc of
        Just doc -> do
          doc `shouldContain` "Test module"
          doc `shouldContain` "{braces}"
        Nothing -> expectationFailure "Module docstring not extracted"

    it "extracts function docstrings" $ do
      case lookup "test_function" docstrings of
        Just (Just doc) -> doc `shouldContain` "Test function"
        Just Nothing -> expectationFailure "Function should have docstring"
        Nothing -> expectationFailure "Function not found"

    it "handles functions without docstrings" $ do
      case lookup "no_docstring_function" docstrings of
        Just Nothing -> return ()  -- Expected: no docstring
        Just (Just _) -> expectationFailure "Function should not have docstring"
        Nothing -> expectationFailure "Function not found"

    it "extracts class docstrings" $ do
      case lookup "TestClass" docstrings of
        Just (Just doc) -> doc `shouldContain` "Test class"
        Just Nothing -> expectationFailure "Class should have docstring"
        Nothing -> expectationFailure "Class not found"

    it "extracts actor docstrings" $ do
      case lookup "TestActor" docstrings of
        Just (Just doc) -> doc `shouldContain` "Test actor"
        Just Nothing -> expectationFailure "Actor should have docstring"
        Nothing -> expectationFailure "Actor not found"

    it "extracts protocol docstrings" $ do
      case lookup "TestProtocol" docstrings of
        Just (Just doc) -> doc `shouldContain` "Test protocol"
        Just Nothing -> expectationFailure "Protocol should have docstring"
        Nothing -> expectationFailure "Protocol not found"

    it "extracts extension docstrings" $ do
      case lookup "extension" docstrings of
        Just (Just doc) -> doc `shouldContain` "Extension"
        Just Nothing -> expectationFailure "Extension should have docstring"
        Nothing -> expectationFailure "Extension not found"

    -- Edge case tests
    it "ignores non-first string statements" $ do
      case lookup "function_with_non_first_string" docstrings of
        Just Nothing -> return ()  -- Expected: no docstring
        Just (Just _) -> expectationFailure "Non-first string should not be docstring"
        Nothing -> expectationFailure "Function not found"

    it "extracts only first string as docstring" $ do
      case lookup "function_with_multiple_strings" docstrings of
        Just (Just doc) -> do
          doc `shouldContain` "First string is docstring"
          when ("Second string" `isInfixOf` doc) $
            expectationFailure "Later strings should not be in docstring"
        Just Nothing -> expectationFailure "Function should have docstring"
        Nothing -> expectationFailure "Function not found"

    it "does not treat f-prefixed strings as docstrings" $ do
      case lookup "function_with_f_prefix_docstring" docstrings of
        Just Nothing -> return ()
        Just (Just _) -> expectationFailure "f-prefixed string should not be docstring"
        Nothing -> expectationFailure "Function not found"

    it "handles single quote docstrings" $ do
      case lookup "function_with_single_quotes" docstrings of
        Just (Just doc) -> doc `shouldContain` "Single quote"
        Just Nothing -> expectationFailure "Function should have docstring"
        Nothing -> expectationFailure "Function not found"

    it "handles triple single quote docstrings" $ do
      case lookup "function_with_triple_single_quotes" docstrings of
        Just (Just doc) -> doc `shouldContain` "Triple quote"
        Just Nothing -> expectationFailure "Function should have docstring"
        Nothing -> expectationFailure "Function not found"

    it "handles mixed quotes in docstrings" $ do
      case lookup "function_with_mixed_quotes" docstrings of
        Just (Just doc) -> doc `shouldContain` "Mixed 'quotes'"
        Just Nothing -> expectationFailure "Function should have docstring"
        Nothing -> expectationFailure "Function not found"

    it "does not interpolate docstrings" $ do
      case lookup "function_with_braces_docstring" docstrings of
        Just (Just doc) -> doc `shouldContain` "{braces}"
        Just Nothing -> expectationFailure "Function should have docstring"
        Nothing -> expectationFailure "Function not found"

    it "handles empty docstrings" $ do
      case lookup "function_empty_docstring" docstrings of
        Just (Just doc) -> doc `shouldBe` ""
        Just Nothing -> expectationFailure "Function should have empty docstring"
        Nothing -> expectationFailure "Function not found"

    it "ignores strings in control flow" $ do
      case lookup "function_with_control_flow" docstrings of
        Just Nothing -> return ()  -- Expected: no docstring
        Just (Just _) -> expectationFailure "String in control flow should not be docstring"
        Nothing -> expectationFailure "Function not found"

    it "handles functions with just docstrings" $ do
      case lookup "function_just_docstring" docstrings of
        Just (Just doc) -> doc `shouldContain` "Just a docstring"
        Just Nothing -> expectationFailure "Function should have docstring"
        Nothing -> expectationFailure "Function not found"

-- Tests for uninitialized class attribute checking

testAttributesInitialization :: Acton.Env.Env0 -> Spec
testAttributesInitialization env0 = do
  describe "Class Attribute Initialization Check" $ do
    testTypeSuccess env0 "class_init_attrs/init_basic"
    testTypeSuccess env0 "class_init_attrs/init_inferred_only"
    testTypeError   env0 "class_init_attrs/uninit_basic"
    testTypeError   env0 "class_init_attrs/uninit_basic_inferred"

    testTypeSuccess env0 "class_init_attrs/init_multiple"
    testTypeError   env0 "class_init_attrs/uninit_partial"

    testTypeSuccess env0 "class_init_attrs/init_inherited"
    testTypeError   env0 "class_init_attrs/uninit_inherited"
    testTypeSuccess env0 "class_init_attrs/init_parent_call"
    testTypeSuccess env0 "class_init_attrs/init_mixed_parent_self"
    testTypeSuccess env0 "class_init_attrs/init_grandparent_call"
    testTypeError   env0 "class_init_attrs/uninit_no_parent_init"
    testTypeError   env0 "class_init_attrs/uninit_grandparent"
    testTypeError   env0 "class_init_attrs/uninit_grandparent2"
    testTypeSuccess env0 "class_init_attrs/init_no_init_uses_parent"

    testTypeSuccess env0 "class_init_attrs/init_conditional"

    testTypeSuccess env0 "class_init_attrs/init_nested_if"
    testTypeSuccess env0 "class_init_attrs/init_nested_if_raise"
    testTypeError   env0 "class_init_attrs/uninit_nested_if"
    testTypeError   env0 "class_init_attrs/uninit_elif_missing"

    testTypeSuccess env0 "class_init_attrs/init_try_except"
    testTypeSuccess env0 "class_init_attrs/init_try_finally"
    testTypeSuccess env0 "class_init_attrs/init_try_except_finally"
    testTypeError   env0 "class_init_attrs/uninit_try_except"
    testTypeSuccess env0 "class_init_attrs/init_try_except_raise"
    testTypeSuccess env0 "class_init_attrs/init_try_else_combined"
    testTypeSuccess env0 "class_init_attrs/init_multiple_except_handlers"
    testTypeSuccess env0 "class_init_attrs/init_except_raise_after_assignment"
    testTypeSuccess env0 "class_init_attrs/init_try_inside_if"
    testTypeSuccess env0 "class_init_attrs/init_constant_condition"
    testTypeSuccess env0 "class_init_attrs/init_raise_after_assignment"
    testTypeSuccess env0 "class_init_attrs/init_both_branches_with_raise"
    testTypeSuccess env0 "class_init_attrs/init_elif_raise_after_assignment"
    testTypeSuccess env0 "class_init_attrs/init_after_statements"

    testTypeSuccess env0 "class_init_attrs/init_self_attr_access"
    testTypeSuccess env0 "class_init_attrs/init_self_attr_in_expr"
    testTypeError   env0 "class_init_attrs/uninit_self_attr_access"
    testTypeSuccess env0 "class_init_attrs/init_self_attr_conditional"
    testTypeSuccess env0 "class_init_attrs/init_self_attr_in_loop"
    testTypeError   env0 "class_init_attrs/uninit_method_reference"

    testTypeSuccess env0 "class_init_attrs/init_function_call"
    testTypeSuccess env0 "class_init_attrs/init_nested_function_call"
    testTypeError   env0 "class_init_attrs/uninit_method_call"
    testTypeError   env0 "class_init_attrs/uninit_method_call_assign"
    testTypeSuccess env0 "class_init_attrs/init_actor"
    testTypeSuccess env0 "class_init_attrs/init_actor_call"

    testTypeSuccess env0 "class_init_attrs/init_notimplemented"
    testTypeSuccess env0 "class_init_attrs/init_notimpl_call"
    testTypeError   env0 "class_init_attrs/uninit_notimpl_call_other"
    testTypeError   env0 "class_init_attrs/uninit_augmented_assign"
    testTypeError   env0 "class_init_attrs/uninit_for_loop"
    testTypeSuccess env0 "class_init_attrs/init_after_loop"
    testTypeSuccess env0 "class_init_attrs/init_after_while"
    testTypeError   env0 "class_init_attrs/uninit_loop_references_self"
    testTypeError   env0 "class_init_attrs/uninit_init_in_method"
    testTypeError   env0 "class_init_attrs/uninit_return_early"

    testTypeError   env0 "class_init_attrs/uninit_self_reference"
    testTypeSuccess env0 "class_init_attrs/init_self_attr_reference"
    testTypeError   env0 "class_init_attrs/uninit_self_in_list"

    testTypeSuccess env0 "class_init_attrs/init_loop_break"
    testTypeSuccess env0 "class_init_attrs/init_loop_continue"
    testTypeSuccess env0 "class_init_attrs/init_assert"
    testTypeSuccess env0 "class_init_attrs/init_assert_with_self"
    testTypeError   env0 "class_init_attrs/uninit_assert_uninit_attr"
    testTypeSuccess env0 "class_init_attrs/init_delete"
    testTypeSuccess env0 "class_init_attrs/init_nested_function"
    testTypeError   env0 "class_init_attrs/uninit_nested_function_with_self"
    testTypeError   env0 "class_init_attrs/uninit_nested_function_escape"


-- Test a file that should produce a type error
-- Path can be "testname" or "subdir/testname"
testTypeError :: Acton.Env.Env0 -> String -> Spec
testTypeError env0 path = do
  let (subdir, testname) = case break (=='/') path of
                             (name, "") -> ("", name)
                             (dir, '/':name) -> (dir, name)
                             _ -> error $ "Invalid test path: " ++ path
      act_file = "test" </> "src" </> path ++ ".act"
      golden_file = "test" </> "3-types" </> path ++ ".golden"
      -- For error display, use just the basename like acton does
      display_file = testname ++ ".act"

  it testname $ do
    goldenTextFile golden_file $ liftIO $ do
      -- Read the source file for error formatting
      srcContent <- readFile act_file

      result <- E.try $ do
        (env, parsed) <- parseAct env0 path
        kchecked <- Acton.Kinds.check env parsed
        (nmod, tchecked, _, _) <- Acton.Types.reconstruct Nothing Nothing env kchecked
        -- Force evaluation to trigger any lazy exceptions
        E.evaluate $ length (show tchecked)
        return ()

      case result of
        Left (e :: E.SomeException) -> do
          -- Format the error like acton does
          let diagnostic = case E.fromException e :: Maybe TypeError of
                Just typeErr ->
                  -- Use the typeReport function to format all TypeError variants with richer diagnostics
                  let report = typeReport typeErr display_file srcContent
                      diag = addReport mempty report
                  in addFile diag display_file srcContent
                _ -> case E.fromException e :: Maybe CompilationError of
                  Just (IllegalSigOverride n) ->
                    Diag.actErrToDiagnostic "Compilation error" display_file srcContent (loc n) ("Illegal signature override: " ++ prettyText n)
                  Just (OtherError loc msg) ->
                    Diag.actErrToDiagnostic "Compilation error" display_file srcContent loc msg
                  Just compErr ->
                    -- For other compilation errors, use the default show instance
                    Diag.actErrToDiagnostic "Compilation error" display_file srcContent (loc compErr) (show compErr)
                  _ ->
                    -- For now, just use the default formatting for other errors
                    let diagnostic = addReport mempty $ Err (Just "error") (show e) [] []
                    in addFile diagnostic display_file srcContent

          -- Pretty print the diagnostic
          return $ T.pack $ show $
            unAnnotate (prettyDiagnostic WithoutUnicode (TabSize 4) diagnostic)
        Right _ ->
          return $ T.pack "ERROR: Expected type error but compilation succeeded"

-- Test a file that should type check successfully
-- Path can be "testname" or "subdir/testname"
testTypeSuccess :: Acton.Env.Env0 -> String -> Spec
testTypeSuccess env0 path = do
  let testname = takeBaseName path
      act_file = "test" </> "src" </> path ++ ".act"

  it testname $ do
    result <- E.try $ do
      (env, parsed) <- parseAct env0 path
      kchecked <- Acton.Kinds.check env parsed
      (nmod, tchecked, _, _) <- Acton.Types.reconstruct Nothing Nothing env kchecked
      -- Force evaluation to trigger any lazy exceptions
      E.evaluate $ length (show tchecked)
      return ()

    case result of
      Left (e :: E.SomeException) ->
        expectationFailure $ "Expected success but got error: " ++ show e
      Right _ ->
        return ()
