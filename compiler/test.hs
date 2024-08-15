{-# LANGUAGE CPP #-}
import Control.Monad
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Ord
import Data.Time.Clock.POSIX
import qualified Data.ByteString.Lazy.Char8 as LBS

import System.Directory
import System.Directory.Recursive
import System.Exit
import System.FilePath
import System.FilePath.Posix
import System.Process
import System.TimeIt

import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit


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
    builtinsAutoTests <- createAutoTests "Builtins auto" "../test/builtins_auto"
    coreLangAutoTests <- createAutoTests "Core language auto" "../test/core_lang_auto"
    dbAutoTests <- createAutoTests "DB auto" "../test/db_auto"
    exampleTests <- createTests "Examples" "../examples" False [] (testBuild "" ExitSuccess)
    regressionTests <- createAutoTests "Regression auto" "../test/regression_auto"
    regressionSegfaultTests <- createTests "Regression segfaults" "../test/regression_segfault" False [] (testBuild "" ExitSuccess)
    rtsAutoTests <- createAutoTests "RTS auto" "../test/rts_auto"
    stdlibAutoTests <- createAutoTests "stdlib auto" "../test/stdlib_auto"
    typeErrorAutoTests <- createGoldenErrorAutoTests "type errors" "../test/typeerrors"
    defaultMain $ localOption timeout $ testGroup "Tests" $
      [ builtinsAutoTests
      , coreLangAutoTests
      , coreLangTests
      , dbAutoTests
      , compilerTests
      , actoncProjTests
      , actoncRootArgTests
      , exampleTests
      , regressionTests
      , regressionSegfaultTests
      , rtsAutoTests
      , rtsTests
      , stdlibTests
      , stdlibAutoTests
      , typeErrorAutoTests
      , crossCompileTests
      ]
  where timeout :: Timeout
        timeout = mkTimeout (10*60*1000000)
        -- this normally doesn't take long on a local machine but in GitHub
        -- Actions CI, in particular the MacOS workers can be really slow so it
        -- has taken longer than 3 minutes, thus the rather lengthy timeout

coreLangTests =
  testGroup "Core language"
  [
    testCase "async context" $ do
        (returnCode, cmdOut, cmdErr) <- buildAndRun "" "" "../test/core_lang/async-context.act"
        assertEqual "should compile" ExitSuccess returnCode
        assertEqual "should see 2 pongs" "pong\npong\n" cmdOut
  ]

compilerTests =
  testGroup "compiler tests"
  [
    testCase "partial rebuild" $ do
        (returnCode, cmdOut, cmdErr) <- readCreateProcessWithExitCode (shell $ "rm -rf ../test/compiler/rebuild/out") ""
        testBuild "" ExitSuccess False "../test/compiler/rebuild/"
        (returnCode, cmdOut, cmdErr) <- readCreateProcessWithExitCode (shell $ "touch ../test/compiler/rebuild/src/rebuild.act") ""
        testBuild "" ExitSuccess False "../test/compiler/rebuild/"
  , testCase "rebuild with stdlib import" $ do
        (returnCode, cmdOut, cmdErr) <- readCreateProcessWithExitCode (shell $ "rm -rf ../test/compiler/rebuild-import/out") ""
        testBuild "" ExitSuccess False "../test/compiler/rebuild-import/"
        testBuild "" ExitSuccess False "../test/compiler/rebuild-import/"
  , testCase "sub-modules and dashes" $ do
        (returnCode, cmdOut, cmdErr) <- readCreateProcessWithExitCode (shell $ "rm -rf ../test/compiler/subdash/out") ""
        testBuild "" ExitSuccess False "../test/compiler/subdash/"
        testBuild "" ExitSuccess False "../test/compiler/subdash/"
  , testCase "deps" $ do
        (returnCode, cmdOut, cmdErr) <- readCreateProcessWithExitCode (shell $ "rm -rf ../test/compiler/test_deps/build*") ""
        (returnCode, cmdOut, cmdErr) <- readCreateProcessWithExitCode (shell $ "rm -rf ../test/compiler/test_deps/out") ""
        (returnCode, cmdOut, cmdErr) <- readCreateProcessWithExitCode (shell $ "rm -rf ../test/compiler/test_deps/deps/a/build*") ""
        (returnCode, cmdOut, cmdErr) <- readCreateProcessWithExitCode (shell $ "rm -rf ../test/compiler/test_deps/deps/a/out") ""
        runActon "build" ExitSuccess False "../test/compiler/test_deps/"
  ]

actoncProjTests =
  testGroup "compiler project tests"
  [ testCase "simple project" $ do
        testBuild "" ExitSuccess False "test/actonc/project/simple"
        testBuild "" ExitSuccess False "test/actonc/project/simple"

  , testCase "with missing src/ dir" $ do
        testBuild "" (ExitFailure 1) False "test/actonc/project/missing_src"

  , testCase "qualified --root test.main" $ do
        testBuild "--root test.main" ExitSuccess False "test/actonc/project/qualified_root"

  -- after used to avoid races on files in same project dir as above test
  , after AllFinish "qualified_root" $
    testCase "unqualified --root main" $ do
        (returnCode, cmdOut, cmdErr) <- buildThing "--root main" "test/actonc/project/qualified_root"
        assertEqual "actonc should error out" (ExitFailure 1) returnCode
        assertEqual "actonc should report error" "actonc: Project build requires a qualified root actor name, like foo.main\n" cmdErr
  ]

actoncRootArgTests =
  testGroup "compiler actonc --root tests"
  [ testCase "qualified --root test.main" $
        testBuild "--root test.main" ExitSuccess False "test/actonc/root/test.act"
  , testCase "unqualified --root main" $
        testBuild "--root main" ExitSuccess False "test/actonc/root/test.act"
  , after AllFinish "qualified --root" $
    after AllFinish "unqualified --root" $
    testCase "discover root actor" $
        testBuildAndRun "" "" ExitSuccess False "test/actonc/root/test.act"
  ]


rtsTests =
  testGroup "RTS"
  [
      testCase "arg parsing: foo --bar --rts-verbose" $ do
          testBuildAndRun "" "foo --bar --rts-verbose" ExitSuccess False "../test/rts/argv1.act"

  ,   testCase "arg parsing: --rts-verbose --rts-wthreads 7 foo --bar" $ do
          testBuildAndRun "" "--rts-verbose --rts-wthreads 7 foo --bar" ExitSuccess False "../test/rts/argv2.act"

  ,   testCase "arg parsing: --rts-verbose --rts-wthreads=7 foo --bar" $ do
          testBuildAndRun "" "--rts-verbose --rts-wthreads=7 foo --bar" ExitSuccess False "../test/rts/argv3.act"

  ,   testCase "arg parsing: --rts-wthreads 7 count" $ do
          testBuildThing "" ExitSuccess False "../test/rts/argv4.act"
          (returnCode, cmdOut, cmdErr) <- runThing "--rts-verbose --rts-wthreads 7 foo --bar" "../test/rts/argv4.act"
          assertEqual "RTS wthreads success retCode" ExitSuccess returnCode
          assertEqual "RTS wthreads output" True (isInfixOf "Using 7 worker threads" cmdErr)

  ,   testCase "arg parsing: --rts-wthreads 7 count" $ do
          testBuildThing "" ExitSuccess False "../test/rts/argv5.act"
          (returnCode, cmdOut, cmdErr) <- runThing "--rts-verbose --rts-wthreads=7 foo --bar" "../test/rts/argv5.act"
          assertEqual "RTS wthreads success retCode" ExitSuccess returnCode
          assertEqual "RTS wthreads output" True (isInfixOf "Using 7 worker threads" cmdErr)

  ,   testCase "arg parsing: --rts-verbose --rts-wthreads=7 -- foo --bar --rts-verbose" $ do
          testBuildAndRun "" "--rts-verbose --rts-wthreads=7 -- foo --bar --rts-verbose" ExitSuccess False "../test/rts/argv6.act"

  ,   testCase "arg parsing: --rts-wthreads" $ do
          testBuildThing "" ExitSuccess False "../test/rts/argv7.act"
          (returnCode, cmdOut, cmdErr) <- runThing "--rts-wthreads" "../test/rts/argv7.act"
          assertEqual "RTS wthreads error retCode" (ExitFailure 1) returnCode
          assertEqual "RTS wthreads error cmdErr" "ERROR: --rts-wthreads requires an argument.\n" cmdErr
  ]

stdlibTests =
  testGroup "stdlib"
  [
      testCase "time" $ do
          epoch <- getCurrentTime >>= pure . utcTimeToPOSIXSeconds >>= pure . round
          testBuildAndRun "" (show epoch) ExitSuccess False "../test/stdlib/test_time.act"
  ]

crossCompileTests =
  testGroup "cross-compilation tests"
  [
    testCase "build hello --target aarch64-macos-none" $ do
        testBuild "--target aarch64-macos-none" ExitSuccess False "../test/compiler/hello/"
  , testCase "build hello --target aarch64-windows-gnu" $ do
        testBuild "--target aarch64-windows-gnu" ExitSuccess False "../test/compiler/hello/"
  , testCase "build hello --target x86_64-macos-none" $ do
        testBuild "--target x86_64-macos-none" ExitSuccess False "../test/compiler/hello/"
  , testCase "build hello --target x86_64-linux-gnu.2.27" $ do
        testBuild "--target x86_64-linux-gnu.2.27" ExitSuccess False "../test/compiler/hello/"
  , testCase "build hello --target x86_64-linux-musl" $ do
        testBuild "--target x86_64-linux-musl" ExitSuccess False "../test/compiler/hello/"
  , testCase "build hello --target x86_64-windows-gnu" $ do
        testBuild "--target x86_64-windows-gnu" ExitSuccess False "../test/compiler/hello/"
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
    -- __bf = build failure, expect actonc to exit 1
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
    return $ testGroup name $ map createGoldenErrorAutoTest actFiles

createGoldenErrorAutoTest file = do
    let testName  = fileBody
        goldenFile = "../test/typeerrors/" ++ fileBody ++ ".golden"
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
          isProj <- doesFileExist $ dir ++ "/Acton.toml"
          if isProj
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

-- Actual test functions
-- expRet refers to the return code of actonc
testBuild opts expRet expFail thing = do
    testBuildThing opts expRet expFail thing

-- expFail & expRet refers to the acton program, we always assume compilation
-- with actonc succeeds

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
    let cmd = "./" ++ exe ++ " " ++ opts
    (returnCode, cmdOut, cmdErr) <- readCreateProcessWithExitCode (shell $ cmd){ cwd = Just wd } ""
    return (returnCode, cmdOut, cmdErr)
  where (fileBody, fileExt) = splitExtension $ takeFileName thing
        fileParts = splitOn "__" fileBody
        binName = head fileParts


testBuildThing opts expRet expFail thing = do
    (returnCode, cmdOut, cmdErr) <- buildThing opts thing
    iff (expFail == False && returnCode /= expRet) (
        putStrLn("\nERROR: when building " ++ thing ++ ", actonc returned code (" ++ (show returnCode) ++ ") not as expected (" ++ (show expRet) ++ ")\nSTDOUT:\n" ++ cmdOut ++ "STDERR:\n" ++ cmdErr)
        )
    assertEqual ("actonc should return " ++ (show expRet)) expRet returnCode


buildThing opts thing = do
    actonc <- canonicalizePath "../dist/bin/actonc"
    proj <- doesDirectoryExist thing
    projPath <- canonicalizePath thing
    curDir <- getCurrentDirectory
    let wd = if proj then projPath else curDir
    let actCmd    = (id actonc) ++ " " ++ (if proj then "build " else thing) ++ " --dev --always-build " ++ opts
    (returnCode, cmdOut, cmdErr) <- readCreateProcessWithExitCode (shell $ actCmd){ cwd = Just wd } ""
    return (returnCode, cmdOut, cmdErr)


runActon opts expRet expFail proj = do
    actonExe <- canonicalizePath "../dist/bin/acton"
    projPath <- canonicalizePath proj
    let actCmd    = (id actonExe) ++ " " ++ opts
    (returnCode, cmdOut, cmdErr) <- readCreateProcessWithExitCode (shell $ actCmd){ cwd = Just projPath } ""
    iff (expFail == False && returnCode /= expRet) (
        putStrLn("\nERROR: when running acton " ++ opts ++ ", acton returned code (" ++ (show returnCode) ++ ") not as expected (" ++ (show expRet) ++ ")\nSTDOUT:\n" ++ cmdOut ++ "STDERR:\n" ++ cmdErr)
        )
    assertEqual ("acton should return " ++ (show expRet)) expRet returnCode


iff True m                      = m >> return ()
iff False _                     = return ()
