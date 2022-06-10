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
  [ buildTestCase "test/actonc/regressions/import_actor" "" True ExitSuccess
  , buildTestCase "test/actonc/regressions/abstract_actor_from_type_signature" "" True ExitSuccess
  ]

actoncProjTests =
  testGroup "actonc project tests"
  [buildTestCase "test/actonc/project/simple" "" False ExitSuccess
  , buildTestCase "test/actonc/project/missing_src" "" False (ExitFailure 1)
  , buildTestCase "test/actonc/project/qualified_root" "--root test.main" False ExitSuccess
  -- after used to avoid races on files in same project dir as above test
  , after AllFinish "qualified_root" $
    testCase "unqualified --root main" $ do
        (returnCode, cmdOut, cmdErr) <- buildThing "test/actonc/project/qualified_root" " --root main"
        assertEqual "actonc should error out" (ExitFailure 1) returnCode
        assertEqual "actonc should report error" "actonc: Project build requires a qualified root actor name, like foo.main\n" cmdErr
  ]

actoncRootArgTests =
  testGroup "actonc --root tests"
  [ buildTestCase "test/actonc/root/test.act" "--root test.main" False ExitSuccess
  , buildTestCase "test/actonc/root/test.act" "--root main" False ExitSuccess
  ]


testFiles name dir fails = do
    actFiles <- findActFiles dir
    return $ testGroup name $ map (testFile fails) actFiles

testFile fails file = do
    let expFail = elem fileBody fails
    buildTestCase file "" expFail ExitSuccess
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

buildTestCase thing opts True expRet =
    expectFail $ thingTestCase thing opts True expRet
buildTestCase thing opts False expRet =
    thingTestCase thing opts False expRet

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
