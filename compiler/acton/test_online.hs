import Control.Exception (finally)
import qualified Data.Map as M
import System.Directory (canonicalizePath)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import System.Process

import Test.Tasty
import Test.Tasty.HUnit

import Data.Bits (shiftL, (.|.))
import Data.Word (Word64)
import qualified Acton.BuildSpec as BuildSpec
import qualified Acton.Fingerprint as Fingerprint

main :: IO ()
main = defaultMain $ testGroup "Online tests" [pkgCliIntegrationTests]

fingerprintForName :: String -> String
fingerprintForName name =
  let prefix = Fingerprint.fingerprintPrefixForName name
      fp = (fromIntegral prefix `shiftL` 32) .|. (1 :: Word64)
  in Fingerprint.formatFingerprint fp

writeBuildAct :: FilePath -> String -> IO ()
writeBuildAct dir name = do
  let fp = fingerprintForName name
      content = unlines
        [ "name = \"" ++ name ++ "\""
        , "fingerprint = " ++ fp
        , ""
        , "dependencies = {}"
        , ""
        , "zig_dependencies = {}"
        , ""
        ]
  writeFile (dir </> "Build.act") content

readBuildSpecAct :: FilePath -> IO BuildSpec.BuildSpec
readBuildSpecAct path = do
  content <- readFile path
  case BuildSpec.parseBuildAct content of
    Left err -> assertFailure err >> error "unreachable"
    Right (spec, _, _) -> return spec

pkgCliIntegrationTests :: TestTree
pkgCliIntegrationTests =
  testGroup "pkg CLI integration"
  [ testCase "pkg add/upgrade/remove (network)" $ do
        withTempHome $ \_ ->
          withSystemTempDirectory "acton-pkg" $ \proj -> do
            let depName = "foo"
                repoUrl = "https://github.com/actonlang/foo"
                projName = "acton_pkg_test"
            writeBuildAct proj projName
            (codeAdd, outAdd, errAdd) <- runActonIn proj ["pkg", "add", depName, "--repo-url", repoUrl]
            assertExit "pkg add" ExitSuccess codeAdd outAdd errAdd
            spec1 <- readBuildSpecAct (proj </> "Build.act")
            dep1 <- requirePkgDep spec1 depName
            assertEqual "repo_url" (Just repoUrl) (BuildSpec.repo_url dep1)
            assertBool "url set" (hasText (BuildSpec.url dep1))
            assertBool "hash set" (hasText (BuildSpec.hash dep1))

            (codeUp, outUp, errUp) <- runActonIn proj ["pkg", "upgrade"]
            assertExit "pkg upgrade" ExitSuccess codeUp outUp errUp
            spec2 <- readBuildSpecAct (proj </> "Build.act")
            dep2 <- requirePkgDep spec2 depName
            assertEqual "repo_url after upgrade" (Just repoUrl) (BuildSpec.repo_url dep2)

            (codeRm, outRm, errRm) <- runActonIn proj ["pkg", "remove", depName]
            assertExit "pkg remove" ExitSuccess codeRm outRm errRm
            spec3 <- readBuildSpecAct (proj </> "Build.act")
            assertBool "dep removed" (M.notMember depName (BuildSpec.dependencies spec3))
  ]

withTempHome :: (FilePath -> IO a) -> IO a
withTempHome action =
  withSystemTempDirectory "acton-home" $ \home -> do
    oldHome <- lookupEnv "HOME"
    setEnv "HOME" home
    action home `finally` restoreHome oldHome
  where
    restoreHome Nothing = unsetEnv "HOME"
    restoreHome (Just val) = setEnv "HOME" val

runActonIn :: FilePath -> [String] -> IO (ExitCode, String, String)
runActonIn wd args = do
    actonExe <- canonicalizePath "../../dist/bin/acton"
    readCreateProcessWithExitCode (proc actonExe args){ cwd = Just wd } ""

assertExit :: String -> ExitCode -> ExitCode -> String -> String -> IO ()
assertExit label expected actual out err =
    if actual == expected
      then return ()
      else assertFailure $ unlines
        [ label ++ " exit"
        , "expected: " ++ show expected
        , " but got: " ++ show actual
        , "stdout:"
        , out
        , "stderr:"
        , err
        ]

requirePkgDep :: BuildSpec.BuildSpec -> String -> IO BuildSpec.PkgDep
requirePkgDep spec depName =
    case M.lookup depName (BuildSpec.dependencies spec) of
      Nothing -> assertFailure ("Missing dependency " ++ depName) >> error "unreachable"
      Just dep -> return dep

hasText :: Maybe String -> Bool
hasText val = maybe False (not . null) val
