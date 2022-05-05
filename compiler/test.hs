import Filesystem.Path.CurrentOS
import System.Directory
import System.Exit
import System.Process

import Test.Tasty
import Test.Tasty.HUnit

import Data.List
import Data.Ord

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [
    actoncProjTests,
    actoncRootArgTests
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
  [ testCase "qualified --root test.main" $ do
        (returnCode, cmdOut, cmdErr) <- buildFile "test/actonc/root/test.act" "--root test.main"
        assertEqual "actonc should return success" ExitSuccess returnCode
  , testCase "unqualified --root main" $ do
        (returnCode, cmdOut, cmdErr) <- buildFile "test/actonc/root/test.act" "--root main"
        assertEqual "actonc should return success" ExitSuccess returnCode
  ]


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
