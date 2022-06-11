import Data.List
import Data.Maybe
import Data.Ord

import System.Directory
import System.Directory.Recursive
import System.Exit
import System.FilePath
import System.FilePath.Posix
import System.Process

import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit


main = do
    exampleTests <- createTests "Examples" "../examples" False [] (testBuild "" ExitSuccess)
    regressionTests <- createTests "Regression (should succeed)" "../test/regression" False [] (testBuildAndRun "--root main" ExitSuccess)
    regressionBuildFailureTests <- createTests "Regression build failures" "../test/regression_build" True [] (testBuild "" ExitSuccess)
    regressionRunFailureTests <- createTests "Regression run time failures" "../test/regression_run" True [] (testBuildAndRun "--root main" ExitSuccess)
    regressionSegfaultTests <- createTests "Regression segfaults" "../test/regression_segfault" True [] (testBuildAndRun "--root main" ExitSuccess)
    defaultMain $ testGroup "Tests" $
      [ actoncProjTests
      , actoncRootArgTests
      , exampleTests
      , regressionTests
      , regressionBuildFailureTests
      , regressionRunFailureTests
      , regressionSegfaultTests
      ]

actoncProjTests =
  testGroup "actonc project tests"
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
  testGroup "actonc --root tests"
  [ testCase "qualified --root test.main" $
        testBuild "--root test.main" ExitSuccess False "test/actonc/root/test.act"
  , testCase "unqualified --root main" $
        testBuild "--root main" ExitSuccess False "test/actonc/root/test.act"
  ]



-- Creates testgroup from .act files found in specified directory
--createTests :: String -> String -> List -> TestTree
createTests name dir allExpFail fails testFunc = do
    actFiles <- findThings dir
    return $ testGroup name $ map (createTest allExpFail fails (testFunc allExpFail)) actFiles

createTest allExpFail fails testFunc file = do
    let fileExpFail = elem fileBody fails
        expFail = if fileExpFail == True
                    then fileExpFail
                    else allExpFail
    failWrap testFunc file expFail
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
testBuildAndRun opts expRet expFail thing = do
    testBuildThing opts ExitSuccess False thing
    wd <- canonicalizePath $ takeDirectory thing
    let cmd = "./" ++ fileBody
    (returnCode, cmdOut, cmdErr) <- readCreateProcessWithExitCode (shell $ cmd){ cwd = Just wd } ""
    iff (expFail == False && returnCode /= expRet) (
        putStrLn("\nERROR: application return code (" ++ (show returnCode) ++ ") not as expected (" ++ (show expRet) ++ ")\nSTDOUT:\n" ++ cmdOut ++ "STDERR:\n" ++ cmdErr)
        )
    assertEqual ("application should return " ++ (show expRet)) expRet returnCode
  where (fileBody, fileExt) = splitExtension $ takeFileName thing



-- TODO: thingify, it is probably file specific now
thingTestCase thing opts expRet expFail =
    testCase fileBody $ do
        testBuildThing opts expRet expFail thing
  where (fileBody, fileExt) = splitExtension $ takeFileName thing

testBuildThing opts expRet expFail thing = do
    (returnCode, cmdOut, cmdErr) <- buildThing opts thing
    iff (expFail == False && returnCode /= expRet) (
        putStrLn("\nERROR: actonc return code (" ++ (show returnCode) ++ ") not as expected (" ++ (show expRet) ++ ")\nSTDOUT:\n" ++ cmdOut ++ "STDERR:\n" ++ cmdErr)
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
