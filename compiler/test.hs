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
    exampleTests <- createTests "Examples" "../examples" False [] (testBuild "" False ExitSuccess)
    regressionTests <- createTests "Regression (should succeed)" "../test/regression" False [] (testBuildAndRun "--root main" False ExitSuccess)
    defaultMain $ testGroup "Tests" $
      [ actoncBasicTests
      , actoncProjTests
      , actoncRootArgTests
      , exampleTests
      , regressionTests
      ]

actoncBasicTests =
  testGroup "actonc basic tests"
  [ failWrap (testBuild "" True ExitSuccess) "test/actonc/regressions/import_actor" True
  , failWrap (testBuild "" True ExitSuccess) "test/actonc/regressions/abstract_actor_from_type_signature" True
  ]

actoncProjTests =
  testGroup "actonc project tests"
  [ testCase "simple project" $ do
        testBuild "" False ExitSuccess "test/actonc/project/simple"

  , testCase "with missing src/ dir" $ do
        testBuild "" False (ExitFailure 1) "test/actonc/project/missing_src"

  , testCase "qualified --root test.main" $ do
        testBuild "build --root test.main" False ExitSuccess "test/actonc/project/qualified_root"

  -- after used to avoid races on files in same project dir as above test
  , after AllFinish "qualified_root" $
    testCase "unqualified --root main" $ do
        (returnCode, cmdOut, cmdErr) <- buildThing "test/actonc/project/qualified_root" " --root main"
        assertEqual "actonc should error out" (ExitFailure 1) returnCode
        assertEqual "actonc should report error" "actonc: Project build requires a qualified root actor name, like foo.main\n" cmdErr
  ]

actoncRootArgTests =
  testGroup "actonc --root tests"
  [ testCase "qualified --root test.main" $
        testBuild "--root test.main" False ExitSuccess "test/actonc/root/test.act"
  , testCase "unqualified --root main" $
        testBuild "--root main" False ExitSuccess "test/actonc/root/test.act"
  ]



-- Creates testgroup from .act files found in specified directory
--createTests :: String -> String -> List -> TestTree
createTests name dir allExpFail fails testFunc = do
    actFiles <- findActFiles dir
    return $ testGroup name $ map (createTest allExpFail fails testFunc) actFiles

createTest allExpFail fails testFunc file = do
    let fileExpFail = elem fileBody fails
        expFail = if fileExpFail == True
                    then fileExpFail
                    else allExpFail
    failWrap testFunc file expFail
  where (fileBody, fileExt) = splitExtension $ takeFileName file

findActFiles dir = do
                    allFiles <- getFilesRecursive dir
                    let allFilesAbs = map (dir ++) allFiles
                        srcFiles = catMaybes $ map filterActFile allFiles
                    return srcFiles
  where filterActFile file =
          case fileExt of
              ".act" -> Just file
              _ -> Nothing
          where (fileBody, fileExt) = splitExtension $ takeFileName file

failWrap testFunc thing True =
    expectFail $ testWrap testFunc thing
failWrap testFunc thing False =
    testWrap testFunc thing

testWrap testFunc thing =
    testCase fileBody $ testFunc thing
  where (fileBody, fileExt) = splitExtension $ takeFileName thing

-- Actual test functions
-- expRet refers to the return code of actonc
testBuild opts expFail expRet thing = do
    testBuildThing thing opts expFail expRet

-- expFail & expRet refers to the acton program, we always assume compilation
-- with actonc succeeds
testBuildAndRun opts expFail expRet thing = do
    testBuildThing thing opts False ExitSuccess
    wd <- canonicalizePath $ takeDirectory thing
    let cmd = "./" ++ fileBody
    (returnCode, cmdOut, cmdErr) <- readCreateProcessWithExitCode (shell $ cmd){ cwd = Just wd } ""
    iff (expFail == False && returnCode /= expRet) (
        putStrLn("\nERROR: application return code (" ++ (show returnCode) ++ ") not as expected (" ++ (show expRet) ++ ")\nSTDOUT:\n" ++ cmdOut ++ "STDERR:\n" ++ cmdErr)
        )
    assertEqual ("application should return " ++ (show expRet)) expRet returnCode
  where (fileBody, fileExt) = splitExtension $ takeFileName thing



-- TODO: thingify, it is probably file specific now
thingTestCase thing opts expFail expRet =
    testCase fileBody $ do
        testBuildThing thing opts expFail expRet
  where (fileBody, fileExt) = splitExtension $ takeFileName thing

testBuildThing thing opts expFail expRet = do
    (returnCode, cmdOut, cmdErr) <- buildThing thing opts
    iff (expFail == False && returnCode /= expRet) (
        putStrLn("\nERROR: actonc return code (" ++ (show returnCode) ++ ") not as expected (" ++ (show expRet) ++ ")\nSTDOUT:\n" ++ cmdOut ++ "STDERR:\n" ++ cmdErr)
        )
    assertEqual ("actonc should return " ++ (show expRet)) expRet returnCode


buildThing thing opts = do
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
