import Control.Exception (finally)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map as M
import System.Directory (canonicalizePath)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import System.Process

import Test.Tasty
import Test.Tasty.HUnit

import qualified Acton.BuildSpec as BuildSpec

main :: IO ()
main = defaultMain $ testGroup "Online tests" [pkgCliIntegrationTests]

pkgCliIntegrationTests :: TestTree
pkgCliIntegrationTests =
  testGroup "pkg CLI integration"
  [ testCase "pkg add/upgrade/remove (network)" $ do
        withTempHome $ \_ ->
          withSystemTempDirectory "actonc-pkg" $ \proj -> do
            let depName = "foo"
                repoUrl = "https://github.com/actonlang/foo"
            (codeAdd, _outAdd, _errAdd) <- runActoncIn proj ["pkg", "add", depName, "--repo-url", repoUrl]
            assertEqual "pkg add exit" ExitSuccess codeAdd
            spec1 <- readBuildSpecJSON (proj </> "build.act.json")
            dep1 <- requirePkgDep spec1 depName
            assertEqual "repo_url" (Just repoUrl) (BuildSpec.repo_url dep1)
            assertBool "url set" (hasText (BuildSpec.url dep1))
            assertBool "hash set" (hasText (BuildSpec.hash dep1))

            (codeUp, _outUp, _errUp) <- runActoncIn proj ["pkg", "upgrade"]
            assertEqual "pkg upgrade exit" ExitSuccess codeUp
            spec2 <- readBuildSpecJSON (proj </> "build.act.json")
            dep2 <- requirePkgDep spec2 depName
            assertEqual "repo_url after upgrade" (Just repoUrl) (BuildSpec.repo_url dep2)

            (codeRm, _outRm, _errRm) <- runActoncIn proj ["pkg", "remove", depName]
            assertEqual "pkg remove exit" ExitSuccess codeRm
            spec3 <- readBuildSpecJSON (proj </> "build.act.json")
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

runActoncIn :: FilePath -> [String] -> IO (ExitCode, String, String)
runActoncIn wd args = do
    actonc <- canonicalizePath "../../dist/bin/actonc"
    readCreateProcessWithExitCode (proc actonc args){ cwd = Just wd } ""

readBuildSpecJSON :: FilePath -> IO BuildSpec.BuildSpec
readBuildSpecJSON path = do
    content <- LBS.readFile path
    case BuildSpec.parseBuildSpecJSON content of
      Left err -> assertFailure err >> error "unreachable"
      Right spec -> return spec

requirePkgDep :: BuildSpec.BuildSpec -> String -> IO BuildSpec.PkgDep
requirePkgDep spec depName =
    case M.lookup depName (BuildSpec.dependencies spec) of
      Nothing -> assertFailure ("Missing dependency " ++ depName) >> error "unreachable"
      Just dep -> return dep

hasText :: Maybe String -> Bool
hasText val = maybe False (not . null) val
