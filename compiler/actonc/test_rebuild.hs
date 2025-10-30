{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Monad (unless, when)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as TE
import           Data.Char (isDigit)
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Process
import           Data.Time.Clock        (getCurrentTime)
import           System.Directory       (setModificationTime)
import qualified Control.Exception as E
import           Control.Concurrent     (threadDelay)

import           Test.Tasty
import           Test.Tasty.Runners (NumThreads(..))
import           Test.Tasty (DependencyType(..), after, TestName)
import           Test.Tasty.Golden (goldenVsString)
import           Test.Tasty.HUnit

-- Paths ----------------------------------------------------------------------

projDir :: FilePath
projDir = "test" </> "rebuild"

srcDir :: FilePath
srcDir = projDir </> "src"

goldenDir :: FilePath
goldenDir = projDir </> "golden"

-- When running commands with cwd = projDir (compiler/actonc/test/rebuild),
-- the actonc binary at repo-root/dist/bin/actonc is four levels up.
actoncExe :: FilePath
actoncExe = ".." </> ".." </> ".." </> ".." </> "dist" </> "bin" </> "actonc"

-- Utils ----------------------------------------------------------------------

sanitize :: T.Text -> LBS.ByteString
sanitize = LBS.fromStrict . TE.encodeUtf8 . T.unlines . map redact . dropPaths . filter (not . isVolatile) . T.lines
  where
    isVolatile :: T.Text -> Bool
    isVolatile t =
      T.isInfixOf "zigCmd" t ||
      T.isInfixOf "Building project in" t

    -- Replace occurrences of durations like "0.184 s" with a stable token "0.000 s".
    redact :: T.Text -> T.Text
    redact = go
      where
        go t =
          case T.breakOn "." t of
            (pre, rest) | T.null rest -> pre
            (pre, rest) ->
              let afterDot = T.drop 1 rest
                  (digits, rest2) = T.span isDigit afterDot
                  intDigits = T.takeWhileEnd isDigit pre
              in if not (T.null intDigits)
                    && T.length digits == 3
                    && T.isPrefixOf " s" rest2
                   then
                     let leftPre = T.dropEnd (T.length intDigits) pre
                         restAfter = T.drop 2 rest2 -- drop " s"
                      in leftPre <> "0.000 s" <> go restAfter
                   else pre <> "." <> go afterDot

    -- Remove the verbose Paths: block (and its per-field lines) to avoid
    -- machine-specific absolute paths in goldens.
    dropPaths :: [T.Text] -> [T.Text]
    dropPaths [] = []
    dropPaths (l:ls)
      | T.isPrefixOf "Paths:" (T.stripStart l) = dropWhile isPathField ls
      | otherwise = l : dropPaths ls
      where
        isPathField :: T.Text -> Bool
        isPathField x = let s = T.stripStart x in any (`T.isPrefixOf` s)
                          [ "sysPath"
                          , "sysTypes"
                          , "projPath"
                          , "projOut"
                          , "projTypes"
                          , "binDir"
                          , "srcDir"
                          , "modName"
                          ]

writeFileUtf8 :: FilePath -> T.Text -> IO ()
writeFileUtf8 p t = do
  let tmp = p <.> "tmp"
  T.writeFile tmp t
  let handler :: IOError -> IO ()
      handler _ = do
        exists <- doesFileExist p
        let handler2 :: IOError -> IO (); handler2 _ = pure ()
        when exists $ E.catch (removeFile p) handler2
        renameFile tmp p
  E.catch (renameFile tmp p) handler

touch :: FilePath -> IO ()
touch p = do
  now <- getCurrentTime
  setModificationTime p now

ensureClean :: IO ()
ensureClean = do
  createDirectoryIfMissing True projDir
  -- Reset src dir fully to avoid leftover temp/lock files
  let handler :: IOError -> IO (); handler _ = pure ()
  E.catch (removeDirectoryRecursive srcDir) handler
  createDirectoryIfMissing True srcDir
  let toml = projDir </> "Acton.toml"
  exists <- doesFileExist toml
  unless exists $ writeFileUtf8 toml ""
  -- Remove previous build output if present
  let outDir = projDir </> "out"
  outExists <- doesDirectoryExist outDir
  when outExists $ E.catch (removeDirectoryRecursive outDir) handler
  -- Remove potential leftover project lock file
  let lockFile = projDir </> ".actonc.lock"
  lockExists <- doesFileExist lockFile
  when lockExists $ E.catch (removeFile lockFile) handler

runIn :: FilePath -> String -> IO (ExitCode, T.Text)
runIn cwd cmd = do
  (ec,out,err) <- readCreateProcessWithExitCode (shell cmd){ cwd = Just cwd } ""
  pure (ec, T.pack out <> T.pack err)

goldenBuild :: IO LBS.ByteString
goldenBuild = do
  let cmd = actoncExe ++ " build --color never --verbose"
  (_ec,out) <- runIn projDir cmd
  pure (sanitize out)

goldenBuildFile :: IO LBS.ByteString
goldenBuildFile = do
  let cmd = actoncExe ++ " src/c.act --color never --verbose"
  (_ec,out) <- runIn projDir cmd
  pure (sanitize out)

buildProject :: IO ()
buildProject = do
  let cmd = actoncExe ++ " build --color never"
  (ec,out) <- runIn projDir cmd
  case ec of
    ExitSuccess -> pure ()
    ExitFailure c -> assertFailure ("actonc build failed: " ++ show c ++ "\n" ++ T.unpack out)

runBinary :: String -> IO T.Text
runBinary name = do
  let bin = "out" </> "bin" </> name
  --exists <- doesFileExist bin
  --unless exists $ assertFailure ("Missing binary: " ++ bin)
  (ec,out,err) <- readCreateProcessWithExitCode (proc bin []){ cwd = Just (projDir) } ""
  case ec of
    ExitSuccess -> pure (T.pack out)
    ExitFailure c -> assertFailure ("Binary failed (exit " ++ show c ++ ") stderr: " ++ err)

-- Project scenario steps ------------------------------------------------------

p01_init :: TestTree
p01_init = testCase "01-init" $ do
  ensureClean
  writeFileUtf8 (srcDir </> "a.act") "aaa = 123\n"
  writeFileUtf8 (srcDir </> "b.act") $ T.unlines
    [ "import a"
    , "def baa():"
    , "    return a.aaa + 3"
    ]
  writeFileUtf8 (srcDir </> "c.act") $ T.unlines
    [ "import b"
    , ""
    , "actor main(env: Env):"
    , "    print(b.baa())"
    , "    env.exit(0)"
    ]

p02_initial_build :: TestTree
p02_initial_build = goldenVsString "02-initial-build.golden" (goldenDir </> "project_02-initial-build.golden") goldenBuild

p03_up_to_date :: TestTree
p03_up_to_date = goldenVsString "03-up-to-date.golden" (goldenDir </> "project_03-up-to-date.golden") goldenBuild

p04_touch_no_rebuild :: TestTree
p04_touch_no_rebuild = goldenVsString "04-touch-no-rebuild.golden" (goldenDir </> "project_04-touch-no-rebuild.golden") $ do
  touch (srcDir </> "a.act")
  goldenBuild

p05_run_126 :: TestTree
p05_run_126 = testCase "05-run 126" $ do
  out <- runBinary "c"
  out @?= "126\n"

p06_change_a_rebuild_all :: TestTree
p06_change_a_rebuild_all = goldenVsString "06-change-a-rebuild-all.golden" (goldenDir </> "project_06-change-a-rebuild-all.golden") $ do
  writeFileUtf8 (srcDir </> "a.act") "aaa = 124\n"
  goldenBuild

p07_run_127 :: TestTree
p07_run_127 = testCase "07-run 127" $ do
  out <- runBinary "c"
  out @?= "127\n"

p08_change_b_rebuild_bc :: TestTree
p08_change_b_rebuild_bc = goldenVsString "08-change-b-rebuild-bc.golden" (goldenDir </> "project_08-change-b-rebuild-bc.golden") $ do
  writeFileUtf8 (srcDir </> "b.act") $ T.unlines
    [ "import a"
    , "def baa():"
    , "    return a.aaa + 10"
    ]
  goldenBuild

p09_run_134 :: TestTree
p09_run_134 = testCase "09-run 134" $ do
  out <- runBinary "c"
  out @?= "134\n"

f01_init :: TestTree
f01_init = testCase "01-init" $ do
  ensureClean
  writeFileUtf8 (srcDir </> "a.act") "aaa = 123\n"
  writeFileUtf8 (srcDir </> "b.act") $ T.unlines
    [ "import a"
    , "def baa():"
    , "    return a.aaa + 3"
    ]
  writeFileUtf8 (srcDir </> "c.act") $ T.unlines
    [ "import b"
    , ""
    , "actor main(env: Env):"
    , "    print(b.baa())"
    , "    env.exit(0)"
    ]

f02_initial_build :: TestTree
f02_initial_build = goldenVsString "02-initial-build.golden" (goldenDir </> "file_02-initial-build.golden") goldenBuildFile

f03_up_to_date :: TestTree
f03_up_to_date = goldenVsString "03-up-to-date.golden" (goldenDir </> "file_03-up-to-date.golden") goldenBuildFile

f04_touch_no_rebuild :: TestTree
f04_touch_no_rebuild = goldenVsString "04-touch-no-rebuild.golden" (goldenDir </> "file_04-touch-no-rebuild.golden") $ do
  touch (srcDir </> "a.act")
  goldenBuildFile

f05_run_126 :: TestTree
f05_run_126 = testCase "05-run 126" $ do
  out <- runBinary "c"
  out @?= "126\n"

f06_change_a_rebuild_all :: TestTree
f06_change_a_rebuild_all = goldenVsString "06-change-a-rebuild-all.golden" (goldenDir </> "file_06-change-a-rebuild-all.golden") $ do
  writeFileUtf8 (srcDir </> "a.act") "aaa = 124\n"
  goldenBuildFile

f07_run_127 :: TestTree
f07_run_127 = testCase "07-run 127" $ do
  out <- runBinary "c"
  out @?= "127\n"

f08_change_b_rebuild_bc :: TestTree
f08_change_b_rebuild_bc = goldenVsString "08-change-b-rebuild-bc.golden" (goldenDir </> "file_08-change-b-rebuild-bc.golden") $ do
  writeFileUtf8 (srcDir </> "b.act") $ T.unlines
    [ "import a"
    , "def baa():"
    , "    return a.aaa + 10"
    ]
  goldenBuildFile

f09_run_134 :: TestTree
f09_run_134 = testCase "09-run 134" $ do
  out <- runBinary "c"
  out @?= "134\n"

-- Main -----------------------------------------------------------------------

main :: IO ()
main = defaultMain $ localOption (NumThreads 1) $ testGroup "rebuild"
  [ sequentialTestGroup "rebuild-project" AllSucceed
      [ p01_init
      , p02_initial_build
      , p03_up_to_date
      , p04_touch_no_rebuild
      , p05_run_126
      , p06_change_a_rebuild_all
      , p07_run_127
      , p08_change_b_rebuild_bc
      , p09_run_134
      ]
  , sequentialTestGroup "rebuild-file" AllSucceed
      [ f01_init
      , f02_initial_build
      , f03_up_to_date
      , f04_touch_no_rebuild
      , f05_run_126
      , f06_change_a_rebuild_all
      , f07_run_127
      , f08_change_b_rebuild_bc
      , f09_run_134
      ]
  ]
