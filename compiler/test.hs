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
    exampleFiles <- findExamplesFiles "../examples"
    let exampleFilesExp = map exampleExpFail exampleFiles
        exampleTests = map genExampleTests exampleFilesExp
    defaultMain $ testGroup "Tests" $ [
        actoncProjTests,
        actoncRootArgTests,
        testGroup "Examples" exampleTests
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


findExamplesFiles dir = do
                    allFiles <- getFilesRecursive dir
                    let srcFiles = catMaybes $ map filterActFile allFiles
                    return srcFiles

filterActFile :: FilePath -> Maybe FilePath
filterActFile file =
    case fileExt of
        ".act" -> Just file
        _ -> Nothing
  where (fileBody, fileExt) = splitExtension $ takeFileName file

exampleExpFail :: FilePath -> (Bool, FilePath)
exampleExpFail file =
    case fileBody of
        --"my-failing-example" -> (True, file)
        _ -> (False, file)
  where (fileBody, fileExt) = splitExtension $ takeFileName file

genExampleTests tp =
    testExampleFile file fail
  where (fail, file) = tp

testExampleFile :: String -> Bool -> TestTree
testExampleFile file True =
    expectFail $ testCase fileBody $ do
        (returnCode, cmdOut, cmdErr) <- buildFile file ""
        assertEqual "actonc should return success" ExitSuccess returnCode
  where (fileBody, fileExt) = splitExtension $ takeFileName file
testExampleFile file False =
    testCase fileBody $ do
        (returnCode, cmdOut, cmdErr) <- buildFile file ""
        assertEqual "actonc should return success" ExitSuccess returnCode
  where (fileBody, fileExt) = splitExtension $ takeFileName file


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
