{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Control.Monad
import Data.Char (isAlphaNum, isSpace)
import Data.List
import Data.List.Split
import Data.Maybe (catMaybes)
import Data.Ord
import Data.Time.Clock.POSIX
import qualified Data.ByteString.Lazy.Char8 as LBS

import Control.Exception (catch, IOException)
import System.Directory
import System.Directory.Recursive
import System.Exit
import System.FilePath
import System.FilePath.Posix
import System.Process
import System.TimeIt
import System.IO.Temp (withSystemTempDirectory)

import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit

import qualified PkgCommands

-- The default is to build and run each test program with the expectation that
-- both compilation and running the program is successful as determined by exit
-- status of 0. If a test is expected to fail compilation, it should be named
-- __bf.act (for Build Failure). If a test is expected to fail at run time, name
-- the file with __rf.act (for Run Failure).

main = do
#if defined(darwin_HOST_OS)
    let segfault_exitcode = (ExitFailure (-11))
#else
    let segfault_exitcode = (ExitFailure 139)
#endif
    builtinsAutoTests <- createAutoTests "Builtins auto" "../../test/builtins_auto"
    coreLangAutoTests <- createAutoTests "Core language auto" "../../test/core_lang_auto"
    dbAutoTests <- createAutoTests "DB auto" "../../test/db_auto"
    exampleTests <- createTests "Examples" "../../examples" False [] (testBuild "" ExitSuccess)
    regressionTests <- createAutoTests "Regression auto" "../../test/regression_auto"
    regressionSegfaultTests <- createTests "Regression segfaults" "../../test/regression_segfault" False [] (testBuild "" ExitSuccess)
    rtsAutoTests <- createAutoTests "RTS auto" "../../test/rts_auto"
    stdlibAutoTests <- createAutoTests "stdlib auto" "../../test/stdlib_auto"
    syntaxErrorAutoTests <- createGoldenErrorAutoTests "syntax errors" "test/syntaxerrors"
    typeErrorAutoTests <- createGoldenErrorAutoTests "type errors" "test/typeerrors"
    defaultMain $ localOption timeout $ testGroup "Tests" $
      [ builtinsAutoTests
      , coreLangAutoTests
      , coreLangTests
      , dbAutoTests
      , compilerTests
      , actonProjTests
      , actonRootArgTests
      , exampleTests
      , regressionTests
      , regressionSegfaultTests
      , rtsAutoTests
      , rtsTests
      , stdlibTests
      , stdlibAutoTests
      , syntaxErrorAutoTests
      , typeErrorAutoTests
      , parseFlagTests
      , crossCompileTests
      , pkgCliTests
      ]
  where timeout :: Timeout
        timeout = mkTimeout (30*60*1000000)
        -- this normally doesn't take long on a local machine but in GitHub
        -- Actions CI, in particular the MacOS workers can be really slow so it
        -- has taken longer than 3 minutes, thus the rather lengthy timeout

coreLangTests =
  testGroup "Core language"
  [
    testCase "async context" $ do
        (returnCode, cmdOut, cmdErr) <- buildAndRun "" "" "../../test/core_lang/async-context.act"
        assertEqual "should compile" ExitSuccess returnCode
        assertEqual "should see 2 pongs" "pong\npong\n" cmdOut
  ]

compilerTests =
  testGroup "compiler tests"
  [
    testCase "target native" $ do
        testBuild "--target native" ExitSuccess False "test/project/simple"
  , testCase "mixed dots in qualified name" $ do
        (returnCode, cmdOut, cmdErr) <- readCreateProcessWithExitCode (shell $ "rm -rf ../../test/compiler/mixed-dots/out") ""
        testBuild "" ExitSuccess False "../../test/compiler/mixed-dots/"
        testBuild "" ExitSuccess False "../../test/compiler/mixed-dots/"
  , testCase "sub-modules and dashes" $ do
        (returnCode, cmdOut, cmdErr) <- readCreateProcessWithExitCode (shell $ "rm -rf ../../test/compiler/subdash/out") ""
        testBuild "" ExitSuccess False "../../test/compiler/subdash/"
        testBuild "" ExitSuccess False "../../test/compiler/subdash/"
  , testCase "deps" $ do
        (returnCode, cmdOut, cmdErr) <- readCreateProcessWithExitCode (shell $ "rm -rf ../../test/compiler/test_deps/build.zig*") ""
        (returnCode, cmdOut, cmdErr) <- readCreateProcessWithExitCode (shell $ "rm -rf ../../test/compiler/test_deps/out") ""
        (returnCode, cmdOut, cmdErr) <- readCreateProcessWithExitCode (shell $ "rm -rf ../../test/compiler/test_deps/deps/a/build.zig*") ""
        (returnCode, cmdOut, cmdErr) <- readCreateProcessWithExitCode (shell $ "rm -rf ../../test/compiler/test_deps/deps/a/out") ""
        runActon "build" ExitSuccess False "../../test/compiler/test_deps/"
  , testCase "build.zig.zon name capped for long project dir" $ do
        let prefix = "acton-long-project-name-12345678901234567890-"
        withSystemTempDirectory prefix $ \proj -> do
            let actFile = proj </> "acton-test.act"
            writeFile actFile $ unlines
              [ "#!/usr/bin/env runacton"
              , "actor main(env):"
              , "    print(\"Hello, world\")"
              , "    env.exit(0)"
              ]
            perms <- getPermissions actFile
            setPermissions actFile perms{ executable = True }
            runActon "build" ExitSuccess False proj
            zon <- readFile (proj </> "build.zig.zon")
            let nameVal =
                  let isNameLine l = ".name = ." `isPrefixOf` dropWhile isSpace l
                  in case find isNameLine (lines zon) of
                       Just line ->
                         case stripPrefix ".name = ." (dropWhile isSpace line) of
                           Just rest -> takeWhile (\c -> isAlphaNum c || c == '_') rest
                           Nothing -> ""
                       Nothing -> ""
            assertBool "build.zig.zon name should be non-empty" (not (null nameVal))
            assertBool "build.zig.zon name should be <= 32 chars" (length nameVal <= 32)

  , testCase "build without Build.act" $ do
        withSystemTempDirectory "acton-build" $ \proj -> do
            let actFile = proj </> "acton-test.act"
            writeFile actFile $ unlines
              [ "#!/usr/bin/env runacton"
              , "actor main(env):"
              , "    print(\"Hello, world\")"
              , "    env.exit(0)"
              ]
            perms <- getPermissions actFile
            setPermissions actFile perms{ executable = True }
            runActon "build" ExitSuccess False proj
            let bin = proj </> "out" </> "bin" </> "acton-test"
            exists <- doesFileExist bin
            assertBool "binary should exist" exists
            (returnCode, cmdOut, cmdErr) <- readCreateProcessWithExitCode (proc bin []){ cwd = Just proj } ""
            assertEqual "binary should run" ExitSuccess returnCode
            assertEqual "binary output" "Hello, world\n" cmdOut
  ]

parseFlagTests =
  testGroup "acton compiler flags"
  [
    flagGolden "parse flag prints AST" "test/parse/simple.golden" ["--quiet", "--parse"]
  , flagGolden "kinds flag prints kinds" "test/parse/simple.kinds.golden" ["--quiet", "--kinds"]
  , flagGolden "types flag prints types" "test/parse/simple.types.golden" ["--quiet", "--types"]
  , flagGolden "sigs flag prints signatures" "test/parse/simple.sigs.golden" ["--quiet", "--sigs"]
  , flagGolden "norm flag prints normalized" "test/parse/simple.norm.golden" ["--quiet", "--norm"]
  , flagGolden "deact flag prints deactorized" "test/parse/simple.deact.golden" ["--quiet", "--deact"]
  , flagGolden "cps flag prints CPS" "test/parse/simple.cps.golden" ["--quiet", "--cps"]
  , flagGolden "llift flag prints lifted" "test/parse/simple.llift.golden" ["--quiet", "--llift"]
  , flagGolden "box flag prints boxing" "test/parse/simple.box.golden" ["--quiet", "--box"]
  -- Avoid #line directives (absolute paths) so hgen/cgen goldens stay stable across machines
  , flagGolden "hgen flag prints header" "test/parse/simple.hgen.golden" ["--quiet", "--dbg-no-lines", "--hgen"]
  , flagGolden "cgen flag prints c" "test/parse/simple.cgen.golden" ["--quiet", "--dbg-no-lines", "--cgen"]
  , flagGolden "all flags combined" "test/parse/simple.all.golden"
        ["--quiet", "--parse", "--kinds", "--types", "--sigs", "--norm", "--deact", "--cps", "--llift", "--box", "--dbg-no-lines", "--hgen"]
  ]
  where
    flagGolden label golden flags =
      goldenVsString label golden $ do
        acton <- canonicalizePath "../../dist/bin/acton"
        sample <- canonicalizePath "test/parse/simple.act"
        (returnCode, cmdOut, cmdErr) <- readCreateProcessWithExitCode (proc acton (flags ++ [sample])) ""
        assertEqual ("acton " ++ unwords flags ++ " should succeed") ExitSuccess returnCode
        assertEqual ("acton " ++ unwords flags ++ " stderr") "" cmdErr
        return (LBS.pack cmdOut)

actonProjTests =
  testGroup "compiler project tests"
  [ testCase "simple project" $ do
        testBuild "" ExitSuccess False "test/project/simple"
        testBuild "" ExitSuccess False "test/project/simple"

  , testCase "with missing src/ dir" $ do
        testBuild "" (ExitFailure 1) False "test/project/missing_src"

  , testCase "qualified --root test.main" $ do
        testBuild "--root test.main" ExitSuccess False "test/project/qualified_root"

  -- after used to avoid races on files in same project dir as above test
  , after AllFinish "qualified_root" $
    testCase "unqualified --root main" $ do
        (returnCode, cmdOut, cmdErr) <- buildThing "--root main" "test/project/qualified_root"
        assertEqual "acton should error out" (ExitFailure 1) returnCode
        assertEqual "acton should report error" "acton: Project build requires a qualified root actor name, like foo.main\n" cmdErr

  , testCase "project with nested path deps" $ do
        let proj = "../../test/compiler/acton_proj_deps"
            depA = proj </> "deps/dep_a"
            depB = proj </> "deps/dep_b"
            wipe p = void $ readCreateProcessWithExitCode (shell $ "rm -rf " ++ p ++ "/build.zig " ++ p ++ "/build.zig.zon " ++ p ++ "/out") ""
        mapM_ wipe [proj, depA, depB]
        -- Build main project via acton; dependencies should be built automatically
        testBuild "" ExitSuccess False proj
        -- Run produced binary
        (cRun, _outRun, _errRun) <- readCreateProcessWithExitCode (shell "./out/bin/main"){ cwd = Just proj } ""
        assertEqual "project binary should run" ExitSuccess cRun
        zon <- readFile (proj </> "build.zig.zon")
        assertBool "build.zig.zon should declare dep_a" (".dep_a" `isInfixOf` zon)
        assertBool "build.zig.zon should declare dep_b" (".dep_b" `isInfixOf` zon)
  , testCase "builds dependencies even if unused in imports" $ do
        let proj = "../../test/compiler/unused_dep"
            depU = proj </> "deps/dep_unused"
            wipe p = void $ readCreateProcessWithExitCode (shell $ "rm -rf " ++ p ++ "/build.zig " ++ p ++ "/build.zig.zon " ++ p ++ "/out") ""
        mapM_ wipe [proj, depU]
        testBuild "" ExitSuccess False proj
        let depObj = depU </> "out/types/acton_empty.c"
        exists <- doesFileExist depObj
        assertBool "dependency output should be generated even if unused (dummy allowed)" exists
  , testCase "dep overrides propagate to build.zig.zon" $ do
        let proj = "../../test/compiler/dep_override"
            depA = proj </> "deps/dep_a"
            depB = proj </> "deps/dep_b"
            depC = proj </> "deps/dep_c"
            wipe p = void $ readCreateProcessWithExitCode (shell $ "rm -rf " ++ p ++ "/build.zig " ++ p ++ "/build.zig.zon " ++ p ++ "/out") ""
            expect needle hay msg = assertBool msg (needle `isInfixOf` hay)
            expectAny needles hay msg = assertBool msg (any (`isInfixOf` hay) needles)
        mapM_ wipe [proj, depA, depB, depC]
        runActon "build --dep dep_a=deps/dep_a --dep dep_b=deps/dep_b --dep ghost=deps/ghost" ExitSuccess False proj
        rootZon <- readFile (proj </> "build.zig.zon")
        depAZon <- readFile (depA </> "build.zig.zon")
        expect ".dep_a = .{" rootZon "root build.zig.zon should declare dep_a"
        expectAny ["dep_override/deps/dep_a", "dep_override\\deps\\dep_a"] rootZon "root build.zig.zon should use dep_a override path"
        expect ".dep_b = .{" rootZon "root build.zig.zon should declare dep_b"
        expectAny ["dep_override/deps/dep_b", "dep_override\\deps\\dep_b"] rootZon "root build.zig.zon should use dep_b override path transitively"
        assertBool "root build.zig.zon should not use cached dep_b hash path" (not ("dep_b-" `isInfixOf` rootZon))
        expect ".dep_c = .{" rootZon "root build.zig.zon should declare dep_c (non-overridden) transitively"
        expectAny ["dep_override/deps/dep_c", "dep_override\\deps\\dep_c"] rootZon "root build.zig.zon should keep dep_c path"
        assertBool "root build.zig.zon should not include undeclared ghost override" (not ("ghost" `isInfixOf` rootZon))
        expect ".dep_b = .{" depAZon "dep_a build.zig.zon should declare dep_b"
        expectAny ["dep_override/deps/dep_b", "dep_override\\deps\\dep_b", "../dep_b", "..\\dep_b"] depAZon "dep_a build.zig.zon should use dep_b override path"
        assertBool "dep_a build.zig.zon should not use cached dep_b hash path" (not ("dep_b-" `isInfixOf` depAZon))
        expect ".dep_c = .{" depAZon "dep_a build.zig.zon should declare dep_c"
        expectAny ["dep_override/deps/dep_c", "dep_override\\deps\\dep_c", "../dep_c", "..\\dep_c"] depAZon "dep_a build.zig.zon should keep dep_c path"
        assertBool "dep_a build.zig.zon should not include undeclared ghost override" (not ("ghost" `isInfixOf` depAZon))
  , testCase "dep override path must be an Acton project root" $ do
        withSystemTempDirectory "acton-invalid-dep-override" $ \proj -> do
          let srcDir = proj </> "src"
              buildAct = proj </> "Build.act"
              mainAct = srcDir </> "main.act"
          createDirectoryIfMissing True srcDir
          createDirectoryIfMissing True (proj </> "deps")
          writeFile buildAct $ unlines
            [ "name = \"invalid_dep_override\""
            , ""
            , "dependencies = {"
            , "  \"dep_a\": (path=\"deps/dep_a_missing\")"
            , "}"
            ]
          writeFile mainAct $ unlines
            [ "actor main(env):"
            , "    print(\"hello\")"
            , "    env.exit(0)"
            ]
          actonExe <- canonicalizePath "../../dist/bin/acton"
          (returnCode, _cmdOut, cmdErr) <- readCreateProcessWithExitCode (proc actonExe ["build", "--dep", "dep_a=deps"]){ cwd = Just proj } ""
          assertEqual "acton should fail for invalid --dep path" (ExitFailure 1) returnCode
          assertBool "error should mention bad dependency path" ("Dependency dep_a path is not an Acton project root" `isInfixOf` cmdErr)
          assertBool "error should mention required project files" ("Build.act" `isInfixOf` cmdErr && "build.act.json" `isInfixOf` cmdErr && "Acton.toml" `isInfixOf` cmdErr)
          assertBool "error should mention src requirement" ("src/" `isInfixOf` cmdErr)

  -- Verify pruning keeps binaries for modules that still have roots across build / test runs.
  , testCase "executable pruning" $ do
        let proj = "test/project/prune_executables"
        cleanOut proj
        -- Build once, then run tests: binaries should stay and both build/test
        -- root .c stubs should coexist.
        testBuild "" ExitSuccess False proj
        let binFoo = proj </> "out/bin/foo"
            binBar = proj </> "out/bin/bar"
            testRoot = proj </> "out/types/tests/simple.test_root.c"
            buildRoot = proj </> "out/types/foo.root.c"
        assertBool "foo binary should exist after build" =<< doesFileExist binFoo
        assertBool "bar binary should exist after build" =<< doesFileExist binBar
        runActon "test" ExitSuccess False proj
        assertBool "foo binary should exist after acton test" =<< doesFileExist binFoo
        assertBool "bar binary should exist after acton test" =<< doesFileExist binBar
        assertBool "build root.c should exist after acton test" =<< doesFileExist buildRoot
        assertBool "test_root.c should exist after acton test"  =<< doesFileExist testRoot

  -- Partial single-file rebuild must not prune other modules' outputs.
  , testCase "partial build leaves unrelated outputs" $ do
        let proj = "test/project/prune_partials"
            srcDir = proj </> "src"
            fooAct = srcDir </> "foo.act"
            barAct = srcDir </> "bar.act"
            fooContent = "actor main(env):\n    print(\"foo\")\n    env.exit(0)\n"
            barContent = "actor main(env):\n    print(\"bar\")\n    env.exit(0)\n"
            barC    = proj </> "out/types/bar.c"
            barH    = proj </> "out/types/bar.h"
            barTy   = proj </> "out/types/bar.ty"
        createDirectoryIfMissing True srcDir
        writeFile fooAct fooContent
        writeFile barAct barContent
        cleanOut proj
        -- Full build first to create bar artifacts.
        testBuild "" ExitSuccess False proj
        acton <- canonicalizePath "../../dist/bin/acton"
        -- Single-file build of foo must not prune outputs from other moduels
        -- since we don't have full visibility, so bar should remain
        (returnCode, _, _) <- readCreateProcessWithExitCode (proc acton ["--skip-build", "--always-build", "src/foo.act"]){ cwd = Just proj } ""
        assertEqual "acton single-file build should succeed" ExitSuccess returnCode
        mapM_ (\p -> doesFileExist p >>= assertBool (p ++ " should exist")) [barC, barH, barTy]

  -- Full rebuild should prune roots / bins when the source module is removed.
  , testCase "full build prunes stale roots and bins" $ do
        let proj = "test/project/prune_roots"
            barAct = proj </> "src/bar.act"
            barContent = "#\n# removed in test to trigger pruning\n\nactor main(env):\n    print(\"bar\")\n    env.exit(0)\n"
            barRoot = proj </> "out/types/bar.root.c"
            barBin  = proj </> "out/bin/bar"
        -- Start from a clean slate and write bar.act to ensure it exists
        cleanOut proj
        writeFile barAct barContent
        -- Initial build should emit bar root & bin.
        testBuild "" ExitSuccess False proj
        assertBool "bar root stub should exist after initial build" =<< doesFileExist barRoot
        assertBool "bar binary should exist after initial build" =<< doesFileExist barBin
        -- Delete source; full build should prune root/bin.
        removeFile barAct `catch` (\(_ :: IOException) -> return ())
        testBuild "" ExitSuccess False proj
        assertBool "bar root stub should be removed after source deletion" . not =<< doesFileExist barRoot
        assertBool "bar binary should be removed after source deletion" . not =<< doesFileExist barBin

  ]

actonRootArgTests =
  testGroup "compiler acton --root tests"
  [ testCase "qualified --root test.main" $
        testBuild "--root test.main" ExitSuccess False "test/root/test.act"
  , testCase "unqualified --root main" $
        testBuild "--root main" ExitSuccess False "test/root/test.act"
  , after AllFinish "qualified --root" $
    after AllFinish "unqualified --root" $
    testCase "discover root actor" $
        testBuildAndRun "" "" ExitSuccess False "test/root/test.act"
  ]


rtsTests =
  testGroup "RTS"
  [
      testCase "arg parsing: foo --bar --rts-verbose" $ do
          testBuildAndRun "" "foo --bar --rts-verbose" ExitSuccess False "../../test/rts/argv1.act"

  ,   testCase "arg parsing: --rts-verbose --rts-wthreads 7 foo --bar" $ do
          testBuildAndRun "" "--rts-verbose --rts-wthreads 7 foo --bar" ExitSuccess False "../../test/rts/argv2.act"

  ,   testCase "arg parsing: --rts-verbose --rts-wthreads=7 foo --bar" $ do
          testBuildAndRun "" "--rts-verbose --rts-wthreads=7 foo --bar" ExitSuccess False "../../test/rts/argv3.act"

  ,   testCase "arg parsing: --rts-wthreads 7 count" $ do
          testBuildThing "" ExitSuccess False "../../test/rts/argv4.act"
          (returnCode, cmdOut, cmdErr) <- runThing "--rts-verbose --rts-wthreads 7 foo --bar" "../../test/rts/argv4.act"
          assertEqual "RTS wthreads success retCode" ExitSuccess returnCode
          assertEqual "RTS wthreads output" True (isInfixOf "Using 7 worker threads" cmdErr)

  ,   testCase "arg parsing: --rts-wthreads 7 count" $ do
          testBuildThing "" ExitSuccess False "../../test/rts/argv5.act"
          (returnCode, cmdOut, cmdErr) <- runThing "--rts-verbose --rts-wthreads=7 foo --bar" "../../test/rts/argv5.act"
          assertEqual "RTS wthreads success retCode" ExitSuccess returnCode
          assertEqual "RTS wthreads output" True (isInfixOf "Using 7 worker threads" cmdErr)

  ,   testCase "arg parsing: --rts-verbose --rts-wthreads=7 -- foo --bar --rts-verbose" $ do
          testBuildAndRun "" "--rts-verbose --rts-wthreads=7 -- foo --bar --rts-verbose" ExitSuccess False "../../test/rts/argv6.act"

  ,   testCase "arg parsing: --rts-wthreads" $ do
          testBuildThing "" ExitSuccess False "../../test/rts/argv7.act"
          (returnCode, cmdOut, cmdErr) <- runThing "--rts-wthreads" "../../test/rts/argv7.act"
          assertEqual "RTS wthreads error retCode" (ExitFailure 1) returnCode
          assertEqual "RTS wthreads error cmdErr" "ERROR: --rts-wthreads requires an argument.\n" cmdErr
  ]

stdlibTests =
  testGroup "stdlib"
  [
      testCase "time" $ do
          epoch <- getCurrentTime >>= pure . utcTimeToPOSIXSeconds >>= pure . round
          testBuildAndRun "" (show epoch) ExitSuccess False "../../test/stdlib/test_time.act"
  ]

crossCompileTests =
  testGroup "cross-compilation tests"
  [
    testCase "build hello --target aarch64-macos-none --db" $ do
        runActon "build --target aarch64-macos-none --db" ExitSuccess False "../../test/compiler/hello/"
  , testCase "build hello --target aarch64-windows-gnu" $ do
        runActon "build --target aarch64-windows-gnu" ExitSuccess False "../../test/compiler/hello/"
  , testCase "build hello --target x86_64-macos-none --db" $ do
        runActon "build --target x86_64-macos-none --db" ExitSuccess False "../../test/compiler/hello/"
  , testCase "build hello --target x86_64-linux-gnu.2.27 --db" $ do
        runActon "build --target x86_64-linux-gnu.2.27 --db" ExitSuccess False "../../test/compiler/hello/"
  , testCase "build hello --target x86_64-linux-musl --db" $ do
        runActon "build --target x86_64-linux-musl --db" ExitSuccess False "../../test/compiler/hello/"
  , testCase "build hello --target x86_64-windows-gnu" $ do
        runActon "build --target x86_64-windows-gnu" ExitSuccess False "../../test/compiler/hello/"
  ]

pkgCliTests =
  testGroup "pkg CLI"
  [ testCase "parse github repo url" $ do
        case PkgCommands.parseGithubRepoUrl "https://github.com/actonlang/foo.git#main" of
          Left err -> assertFailure err
          Right info -> do
            assertEqual "owner" "actonlang" (PkgCommands.repoOwner info)
            assertEqual "repo" "foo" (PkgCommands.repoName info)
            assertEqual "ref" (Just "main") (PkgCommands.repoRef info)
  , testCase "pkg search matches prefix" $ do
        let pkg = PkgCommands.PackageEntry "foo" "desc" "https://github.com/actonlang/foo"
        ok <- PkgCommands.matchesAllTerms ["foo"] pkg
        assertBool "prefix matches" ok
        ok2 <- PkgCommands.matchesAllTerms ["bar"] pkg
        assertBool "non-prefix does not match" (not ok2)
  , testCase "decode package index filters invalid entries" $ do
        let body = "{\"packages\":[{\"name\":\"foo\",\"description\":\"desc\",\"repo_url\":\"https://github.com/actonlang/foo\"},{\"name\":\"bad\"}]}"
        case PkgCommands.decodePackageIndex (LBS.pack body) of
          Left err -> assertFailure err
          Right pkgs -> do
            assertEqual "one package" 1 (length pkgs)
            assertEqual "name" "foo" (PkgCommands.pkgName (head pkgs))
  ]

-- Creates testgroup from .act files found in specified directory
--createTests :: String -> String -> List -> TestTree
createTests name dir allExpFail fails testFunc = do
    actFiles <- findThings dir
    return $ testGroup name $ map (createTest allExpFail fails testFunc) actFiles

createTest allExpFail fails testFunc file = do
    let fileExpFail = elem fileBody fails
        expFail = if fileExpFail == True
                    then fileExpFail
                    else allExpFail
    failWrap (testFunc expFail) file expFail
  where (fileBody, fileExt) = splitExtension $ takeFileName file

createAutoTests name dir = do
    actFiles <- findThings dir
    return $ testGroup name $ map createAutoTest actFiles

createAutoTest file = do
    -- guesstimate how to run this test
    -- no suffix = compile and run test program and expect success (exit 0)
    -- __bf = build failure, expect acton to exit 1
    -- __rf = run failure: compile, run and expect exit 1
    let fileParts = splitOn "__" fileBody
        testExp   = if (length fileParts) == 2
                      then last fileParts
                      else ""
        testName  = if testExp == ""
                      then head fileParts
                      else (head fileParts) ++ " (" ++testExp ++ ")"
        testFunc  = case testExp of
                        "bf" -> testBuild ""
                        _    -> testBuildAndRunRepeat "" "" 2
        expRet    = case testExp of
                        "bf" -> (ExitFailure 1)
                        "rf" -> (ExitFailure 1)
                        _    -> ExitSuccess
    testCase testName $ testFunc expRet False file
  where (fileBody, fileExt) = splitExtension $ takeFileName file

createGoldenErrorAutoTests name dir = do
    actFiles <- findThings dir
    return $ testGroup name $ map (createGoldenErrorAutoTest dir) actFiles

createGoldenErrorAutoTest dir file = do
    let testName  = fileBody
        goldenFile = joinPath [dir, fileBody ++ ".golden"]
    goldenVsString testName goldenFile (getCompileError file)
  where (fileBody, fileExt) = splitExtension $ takeFileName file

getCompileError file = do
    (returnCode, cmdOut, cmdErr) <- buildThing "" file
    assertEqual "compile error retCode" (ExitFailure 1) returnCode
    return (LBS.pack cmdOut)

findThings dir = do
    items <- listDirectory dir
    let absItems = map ((dir ++ "/") ++) items
        actFiles = catMaybes $ map filterActFile absItems
    projDirs <- catMaybes <$> mapM isActProj absItems
    return $ actFiles ++ projDirs
  where filterActFile file =
          case fileExt of
              ".act" -> Just file
              _ -> Nothing
          where (fileBody, fileExt) = splitExtension $ takeFileName file

isActProj dir = do
    isDir <- doesDirectoryExist dir
    if isDir
      then do
          let projectFiles = ["Build.act", "build.act.json", "Acton.toml"]
          hasProjectFile <- or <$> mapM (\file -> doesFileExist $ dir ++ "/" ++ file) projectFiles
          hasSrcDir <- doesDirectoryExist $ dir ++ "/src"
          if hasProjectFile && hasSrcDir
            then return $ Just dir
            else return Nothing
      else return Nothing

failWrap testFunc thing True =
    expectFail $ testWrap testFunc thing
failWrap testFunc thing False =
    testWrap testFunc thing

testWrap testFunc thing =
    testCase fileBody $ testFunc thing
  where (fileBody, fileExt) = splitExtension $ takeFileName thing

cleanOut :: FilePath -> IO ()
cleanOut proj = removePathForcibly (proj </> "out") `catch` (\(_ :: IOException) -> return ())

-- Actual test functions
-- expRet refers to the return code of acton
testBuild opts expRet expFail thing = do
    testBuildThing opts expRet expFail thing

-- expFail & expRet refers to the acton program, we always assume compilation
-- with acton succeeds

testBuildAndRun buildOpts runOpts expRet expFail thing =
    testBuildAndRunRepeat buildOpts runOpts 1 expRet expFail thing

testBuildAndRunRepeat buildOpts runOpts numRuns expRet expFail thing = do
    testBuildThing buildOpts ExitSuccess False thing
    replicateM_ numRuns $ do
        (returnCode, cmdOut, cmdErr) <- runThing runOpts thing
        when (expFail == False && returnCode /= expRet) $
            putStrLn("\nERROR: when running application " ++ thing ++
                     ", the return code (" ++ show returnCode ++
                     ") not as expected (" ++ show expRet ++
                     ")\nSTDOUT:\n" ++ cmdOut ++ "STDERR:\n" ++ cmdErr)
        assertEqual ("application should return " ++ show expRet) expRet returnCode
--    (returnCode, cmdOut, cmdErr) <- runThing runOpts thing
--    iff (expFail == False && returnCode /= expRet) (
--        putStrLn("\nERROR: when running application " ++ thing ++ ", the return code (" ++ (show returnCode) ++ ") not as expected (" ++ (show expRet) ++ ")\nSTDOUT:\n" ++ cmdOut ++ "STDERR:\n" ++ cmdErr)
--        )
--    assertEqual ("application should return " ++ (show expRet)) expRet returnCode

buildAndRun :: String -> String -> FilePath -> IO (ExitCode, String, String)
buildAndRun buildOpts runOpts thing = do
    buildThing buildOpts thing
    (returnCode, cmdOut, cmdErr) <- runThing runOpts thing
    return (returnCode, cmdOut, cmdErr)

-- when in a project, expect the binary to be named the same as the project
-- without our ending, like __bf. For example, if the project is
-- import_actor__bf, then we will run ./import_actor in
-- import_actor__bf/rel/bin
runThing opts thing = do
    twd <- canonicalizePath $ takeDirectory thing
    isProj <- doesDirectoryExist thing
    projBinPath <- canonicalizePath $ thing ++ "/out/bin"
    let wd = if isProj then projBinPath else twd
    let exe = if isProj then binName else fileBody
    (returnCode, cmdOut, cmdErr) <- readCreateProcessWithExitCode (proc ("./" ++ exe) (words opts)){ cwd = Just wd } ""
    return (returnCode, cmdOut, cmdErr)
  where (fileBody, fileExt) = splitExtension $ takeFileName thing
        fileParts = splitOn "__" fileBody
        binName = head fileParts


testBuildThing opts expRet expFail thing = do
    (returnCode, cmdOut, cmdErr) <- buildThing opts thing
    iff (expFail == False && returnCode /= expRet) (
        putStrLn("\nERROR: when building " ++ thing ++ ", acton returned code (" ++ (show returnCode) ++ ") not as expected (" ++ (show expRet) ++ ")\nSTDOUT:\n" ++ cmdOut ++ "STDERR:\n" ++ cmdErr)
        )
    assertEqual ("acton should return " ++ (show expRet)) expRet returnCode


buildThing opts thing = do
    actonExe <- canonicalizePath "../../dist/bin/acton"
    proj <- doesDirectoryExist thing
    projPath <- canonicalizePath thing
    curDir <- getCurrentDirectory
    let wd = if proj then projPath else curDir
        args0 = if proj then ["build"] else [thing]
        args  = args0 ++ ["--always-build"] ++ words opts
    (returnCode, cmdOut, cmdErr) <- readCreateProcessWithExitCode (proc actonExe args){ cwd = Just wd } ""
    return (returnCode, cmdOut, cmdErr)


runActon opts expRet expFail proj = do
    actonExe <- canonicalizePath "../../dist/bin/acton"
    projPath <- canonicalizePath proj
    (returnCode, cmdOut, cmdErr) <- readCreateProcessWithExitCode (proc actonExe (words opts)){ cwd = Just projPath } ""
    iff (expFail == False && returnCode /= expRet) (
        putStrLn("\nERROR: when running acton " ++ opts ++ ", acton returned code (" ++ (show returnCode) ++ ") not as expected (" ++ (show expRet) ++ ")\nSTDOUT:\n" ++ cmdOut ++ "STDERR:\n" ++ cmdErr)
        )
    assertEqual ("acton should return " ++ (show expRet)) expRet returnCode


iff True m                      = m >> return ()
iff False _                     = return ()
