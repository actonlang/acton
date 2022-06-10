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
    exampleTests <- testFiles "Examples" "../examples" []
    defaultMain $ testGroup "Tests" $
      [ actoncBasicTests
      , actoncProjTests
      , actoncRootArgTests
      , exampleTests
      ]

actoncBasicTests =
  testGroup "actonc basic tests"
  [ expectFail $ testCase "create imported actor" $ do
        (returnCode, cmdOut, cmdErr) <- buildProject "test/actonc/regressions/import_actor" "build"
        assertEqual "actonc should return success" ExitSuccess returnCode
  , expectFail $ testCase "instantiate concrete actor" $ do
        (returnCode, cmdOut, cmdErr) <- buildProject "test/actonc/regressions/abstract_actor_from_type_signature" "build"
        assertEqual "actonc should return success" ExitSuccess returnCode
  ]

actoncProjTests =
  testGroup "actonc project tests"
  [ testCase "simple project" $ do
        (returnCode, cmdOut, cmdErr) <- buildProject "test/actonc/project/simple" "build"
        assertEqual "actonc should return success" ExitSuccess returnCode

  , testCase "with missing src/ dir" $ do
        (returnCode, cmdOut, cmdErr) <- buildProject "test/actonc/project/missing_src" "build"
        assertEqual "actonc should error out" (ExitFailure 1) returnCode

  , testCase "qualified --root test.main" $ do
        (returnCode, cmdOut, cmdErr) <- buildProject "test/actonc/project/qualified_root" "build --root test.main"
        assertEqual "actonc should return success" ExitSuccess returnCode

  -- after used to avoid races on files in same project dir as above test
  , after AllFinish "qualified --root test.main" $
    testCase "unqualified --root main" $ do
        (returnCode, cmdOut, cmdErr) <- buildProject "test/actonc/project/qualified_root" "build --root main"
        assertEqual "actonc should error out" (ExitFailure 1) returnCode
        assertEqual "actonc should report error" "actonc: Project build requires a qualified root actor name, like foo.main\n" cmdErr
  ]

actoncRootArgTests =
  testGroup "actonc --root tests"
  [ testCase "qualified --root test.main" $
        testBuildFile "test/actonc/root/test.act" "--root test.main" False ExitSuccess
  , testCase "unqualified --root main" $
        testBuildFile "test/actonc/root/test.act" "--root main" False ExitSuccess
  ]


testFiles name dir fails = do
    actFiles <- findActFiles dir
    return $ testGroup name $ map (testFile fails) actFiles

testFile fails file = do
    let expFail = elem fileBody fails
    buildTestCase file expFail
  where (fileBody, fileExt) = splitExtension $ takeFileName file

findActFiles dir = do
                    allFiles <- getFilesRecursive dir
                    let srcFiles = catMaybes $ map filterActFile allFiles
                    return srcFiles
  where filterActFile file =
          case fileExt of
              ".act" -> Just file
              _ -> Nothing
          where (fileBody, fileExt) = splitExtension $ takeFileName file

buildTestCase file True =
    expectFail $ fileTestCase file True ExitSuccess
buildTestCase file False =
    fileTestCase file False ExitSuccess

fileTestCase file expFail expRet =
    testCase fileBody $ do
        testBuildFile file "" expFail expRet
  where (fileBody, fileExt) = splitExtension $ takeFileName file


testBuildFile file opts expFail expRet = do
    (returnCode, cmdOut, cmdErr) <- buildFile file opts
    iff (expFail == False && returnCode /= expRet) (
        putStrLn("\nERROR: actonc return code (" ++ (show returnCode) ++ ") not as expected (" ++ (show expRet) ++ ")\nSTDOUT:\n" ++ cmdOut ++ "STDERR:\n" ++ cmdErr)
        )
    assertEqual ("actonc should return " ++ (show expRet)) expRet returnCode


buildFile actFile opts = do
    wd <- canonicalizePath actFile
    actonc <- canonicalizePath "../dist/bin/actonc"
    let actCmd    = (id actonc) ++ " " ++ wd ++ " " ++ opts
    (returnCode, cmdOut, cmdErr) <- readCreateProcessWithExitCode (shell $ actCmd) ""
    return (returnCode, cmdOut, cmdErr)

buildProject projPath cmd = do
    wd <- canonicalizePath projPath
    actonc <- canonicalizePath "../dist/bin/actonc"
    let actCmd    = (id actonc) ++ " " ++ cmd
    (returnCode, cmdOut, cmdErr) <- readCreateProcessWithExitCode (shell $ actCmd){ cwd = Just wd } ""
    return (returnCode, cmdOut, cmdErr)

iff True m                      = m >> return ()
iff False _                     = return ()
