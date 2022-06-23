{-# LANGUAGE CPP #-}
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Ord
import Data.Time.Clock.POSIX

import System.Directory
import System.Directory.Recursive
import System.Exit
import System.FilePath
import System.FilePath.Posix
import System.Process
import System.TimeIt

import Test.Tasty
import Test.Tasty.ExpectedFailure
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
    regressionSegfaultTests <- createTests "Regression segfaults" "../test/regression_segfault" False [] (testBuildAndRun "" "" segfault_exitcode)
    rtsAutoTests <- createAutoTests "RTS auto" "../test/rts_auto"
    stdlibAutoTests <- createAutoTests "stdlib auto" "../test/stdlib_auto"
    defaultMain $ testGroup "Tests" $
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
      , stdlibAutoTests
      , stdlibTests
      ]

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
        testBuild "" ExitSuccess True "../test/compiler/rebuild/"
        (returnCode, cmdOut, cmdErr) <- readCreateProcessWithExitCode (shell $ "touch ../test/compiler/rebuild/src/rebuild.act") ""
        testBuild "" ExitSuccess True "../test/compiler/rebuild/"
  ]

actoncProjTests =
  testGroup "compiler project tests"
  [ testCase "simple project" $ do
        testBuild "" ExitSuccess True "test/actonc/project/simple"

  , testCase "with missing src/ dir" $ do
        testBuild "" (ExitFailure 1) False "test/actonc/project/missing_src"

  , testCase "qualified --root test.main" $ do
        testBuild "build --root test.main" ExitSuccess False "test/actonc/project/qualified_root"

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
          epoch <- getCurrentTime >>= pure . (1000*) . utcTimeToPOSIXSeconds >>= pure . round
          testBuildAndRun "" (show epoch) ExitSuccess False "../test/stdlib/test_time.act"
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
                        _    -> testBuildAndRun "" ""
        expRet    = case testExp of
                        "bf" -> (ExitFailure 1)
                        "rf" -> (ExitFailure 1)
                        _    -> ExitSuccess
    testCase testName $ testFunc expRet False file
  where (fileBody, fileExt) = splitExtension $ takeFileName file


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
testBuildAndRun buildOpts runOpts expRet expFail thing = do
    testBuildThing buildOpts ExitSuccess False thing
    (returnCode, cmdOut, cmdErr) <- runThing runOpts thing
    iff (expFail == False && returnCode /= expRet) (
        putStrLn("\nERROR: when running application " ++ thing ++ ", the return code (" ++ (show returnCode) ++ ") not as expected (" ++ (show expRet) ++ ")\nSTDOUT:\n" ++ cmdOut ++ "STDERR:\n" ++ cmdErr)
        )
    assertEqual ("application should return " ++ (show expRet)) expRet returnCode

buildAndRun :: String -> String -> FilePath -> IO (ExitCode, String, String)
buildAndRun buildOpts runOpts thing = do
    buildThing buildOpts thing
    (returnCode, cmdOut, cmdErr) <- runThing runOpts thing
    return (returnCode, cmdOut, cmdErr)

runThing opts thing = do
    wd <- canonicalizePath $ takeDirectory thing
    let cmd = "./" ++ fileBody ++ " " ++ opts
    (returnCode, cmdOut, cmdErr) <- readCreateProcessWithExitCode (shell $ cmd){ cwd = Just wd } ""
    return (returnCode, cmdOut, cmdErr)
  where (fileBody, fileExt) = splitExtension $ takeFileName thing

-- TODO: thingify, it is probably file specific now
thingTestCase thing opts expRet expFail =
    testCase fileBody $ do
        testBuildThing opts expRet expFail thing
  where (fileBody, fileExt) = splitExtension $ takeFileName thing

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
    let actCmd    = (id actonc) ++ " " ++ if proj then "build " ++ opts else thing ++ " " ++ opts
    (returnCode, cmdOut, cmdErr) <- readCreateProcessWithExitCode (shell $ actCmd){ cwd = Just wd } ""
    return (returnCode, cmdOut, cmdErr)


iff True m                      = m >> return ()
iff False _                     = return ()
