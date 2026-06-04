{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Monad (unless, when)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as TE
import           Data.Char (isHexDigit, isSpace)
import           Data.Bits (shiftL, (.|.))
import           Data.Word (Word64)
import qualified Acton.Fingerprint as Fingerprint
import qualified Crypto.Hash.SHA256 as SHA256
import           Data.List (elemIndex, find, partition, sort, sortOn)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe, isJust)
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO (Handle)
import           System.Posix.Files (deviceID, fileID, fileSize, getFileStatus, modificationTimeHiRes, statusChangeTimeHiRes)
import           System.Process
import           System.Timeout (timeout)
import           Data.Time.Clock        (addUTCTime, getCurrentTime)
import           Data.Time.Clock.POSIX  (posixSecondsToUTCTime)
import           System.Directory       (setModificationTime)
import qualified Control.Exception as E
import           Control.Concurrent     (threadDelay)

import           Test.Tasty
import           Test.Tasty.Runners (NumThreads(..))
import           Test.Tasty (DependencyType(..), after, TestName)
import           Test.Tasty.Golden (goldenVsString)
import           Test.Tasty.HUnit
import           TestGolden (normalizeProgressTimingLine)
import qualified Acton.Compile as Compile
import qualified Acton.CommandLineParser as C
import qualified Acton.DocPrinter as DocP
import qualified Acton.SourceProvider as Source
import qualified Acton.NameInfo as I
import qualified Acton.Syntax as A
import qualified InterfaceFiles
import           Utils (SrcLoc(NoLoc), prstr)

-- Paths ----------------------------------------------------------------------

projDir :: FilePath
projDir = "test" </> "rebuild"

srcDir :: FilePath
srcDir = projDir </> "src"

goldenDir :: FilePath
goldenDir = projDir </> "golden"

casesProjDir :: FilePath
casesProjDir = "test" </> "incremental_cases"

casesSrcDir :: FilePath
casesSrcDir = casesProjDir </> "src"

casesProjName :: String
casesProjName = "incremental_cases"

-- When running commands under compiler/acton/test, the acton binary at
-- repo-root/dist/bin/acton is four levels up.
actonExe :: FilePath
actonExe = ".." </> ".." </> ".." </> ".." </> "dist" </> "bin" </> "acton"

-- | Build an acton command line with fixed job count.
actonCmd :: String -> String
actonCmd args = actonExe ++ " " ++ args ++ " --jobs 1"

-- Utils ----------------------------------------------------------------------

-- | Normalize build output for golden comparisons.
sanitize :: T.Text -> LBS.ByteString
sanitize = LBS.fromStrict
        . TE.encodeUtf8
        . T.unlines
        . reorderConcurrentProgressLines
        . map censorHashes
        . map normalizeProgressTimingLine
        . dropPaths
        . filter (not . isVolatile)
        . T.lines
  where
    isVolatile :: T.Text -> Bool
    isVolatile t =
      T.isInfixOf "zigCmd" t ||
      T.isInfixOf "Building project in" t ||
      T.isInfixOf "Building [cap" t

    -- Rewrite volatile hash literals in log lines to stable placeholders.
    -- We keep semantic position in deltas:
    --   old hash values  -> HASH1
    --   new hash values  -> HASH2
    -- Values like "missing" are intentionally left unchanged.
    censorHashes :: T.Text -> T.Text
    censorHashes txt = go 0
      where
        n = T.length txt

        go :: Int -> T.Text
        go i
          | i >= n = T.empty
          | isHexStart i =
              let j = spanHex i
                  run = T.take (j - i) (T.drop i txt)
              in if isHashRun run
                   then hashTag i j <> go j
                   else run <> go j
          | otherwise = T.singleton (T.index txt i) <> go (i + 1)

        isHexStart :: Int -> Bool
        isHexStart i = isHexDigit (T.index txt i)

        spanHex :: Int -> Int
        spanHex i
          | i < n && isHexDigit (T.index txt i) = spanHex (i + 1)
          | otherwise = i

        isHashRun :: T.Text -> Bool
        isHashRun h = let len = T.length h in len == 8 || len == 64

        hashTag :: Int -> Int -> T.Text
        hashTag i j
          | isRightValue i = "HASH2"
          | isLeftValue j = "HASH1"
          | otherwise = "HASH"

        isRightValue :: Int -> Bool
        isRightValue i =
          case skipSpacesLeft (i - 1) of
            Nothing -> False
            Just k
              | T.index txt k == '→' -> True
              | k >= 1 && T.index txt (k - 1) == '-' && T.index txt k == '>' -> True
              | otherwise -> False

        isLeftValue :: Int -> Bool
        isLeftValue j =
          case skipSpacesRight j of
            Nothing -> False
            Just k
              | T.index txt k == '→' -> True
              | k + 1 < n && T.index txt k == '-' && T.index txt (k + 1) == '>' -> True
              | otherwise -> False

        skipSpacesLeft :: Int -> Maybe Int
        skipSpacesLeft k
          | k < 0 = Nothing
          | isSpace (T.index txt k) = skipSpacesLeft (k - 1)
          | otherwise = Just k

        skipSpacesRight :: Int -> Maybe Int
        skipSpacesRight k
          | k >= n = Nothing
          | isSpace (T.index txt k) = skipSpacesRight (k + 1)
          | otherwise = Just k

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

    reorderConcurrentProgressLines :: [T.Text] -> [T.Text]
    reorderConcurrentProgressLines ls =
      let (progressLines, otherLines) = partition isConcurrentProgressLine ls
          (pre, post) = break isFinalLine otherLines
      in pre ++ sortOn progressLineKey progressLines ++ post
      where
        isConcurrentProgressLine t = isFrontOutputLine t || isBackLine t
        isFrontOutputLine t =
          any (`T.isInfixOf` t)
            [ "Wrote .tydb"
            , "Copied .tydb"
            , "Generated docs"
            ]
        isBackLine t =
          T.isPrefixOf "   Finished compilation of" t ||
          (T.isInfixOf "Compilation done" t && not (isFinalLine t)) ||
          isBackPassLine t
        isFinalLine t =
          T.isPrefixOf "   Finished final compilation" t ||
          T.isPrefixOf "   Finished final compilation step" t ||
          T.isInfixOf "Final compilation done" t ||
          T.isPrefixOf "  Skipping final build step" t
        isBackPassLine t =
          case T.words t of
            ("Back":_:"done:":_:_) -> True
            _ -> False
        progressLineKey t =
          case T.words t of
            (mn:"Wrote":".tydb":_) -> (mn, 0 :: Int, t)
            (mn:"Generated":"docs":_) -> (mn, 1 :: Int, t)
            (mn:"Copied":".tydb":_) -> (mn, 2 :: Int, t)
            ("Back":pass:"done:":mn:_) -> (mn, 10 + passIndex pass, t)
            (mn:"Compilation":"done":_) -> (mn, 100 :: Int, t)
            _ -> (t, 101, t)
        passIndex pass = fromMaybe 99 (pass `elemIndex` backPassOrder)
        backPassOrder = ["normalize", "deactorize", "cps", "llift", "boxing", "codegen", "render", "write"]

-- | Atomically write UTF-8 text to a file.
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

fingerprintForName :: String -> String
fingerprintForName name =
  let prefix = Fingerprint.fingerprintPrefixForName name
      fp = (fromIntegral prefix `shiftL` 32) .|. (1 :: Word64)
  in Fingerprint.formatFingerprint fp

-- | Update a file's modification time to now.
touch :: FilePath -> IO ()
touch p = do
  now <- getCurrentTime
  setModificationTime p now

-- | Reset the default rebuild project to a clean state.
ensureClean :: IO ()
ensureClean = do
  ensureCleanAt projDir

-- | Reset a project directory to a clean state.
ensureCleanAt :: FilePath -> IO ()
ensureCleanAt proj = do
  createDirectoryIfMissing True proj
  let src = proj </> "src"
  -- Reset src dir fully to avoid leftover temp/lock files
  let handler :: IOError -> IO (); handler _ = pure ()
  E.catch (removeDirectoryRecursive src) handler
  createDirectoryIfMissing True src
  let buildAct = proj </> "Build.act"
  exists <- doesFileExist buildAct
  unless exists $ writeBuildAct proj (takeFileName proj) []
  -- Remove previous build output if present
  let outDir = proj </> "out"
  outExists <- doesDirectoryExist outDir
  when outExists $ E.catch (removeDirectoryRecursive outDir) handler
  -- Remove potential leftover project lock file
  let lockFile = proj </> ".acton.lock"
  lockExists <- doesFileExist lockFile
  when lockExists $ E.catch (removeFile lockFile) handler
  let bgLockFile = proj </> ".acton.compile.lock"
  bgLockExists <- doesFileExist bgLockFile
  when bgLockExists $ E.catch (removeFile bgLockFile) handler

-- | Run a shell command in a directory and capture stdout+stderr.
runIn :: FilePath -> String -> IO (ExitCode, T.Text)
runIn cwd cmd = do
  (ec,out,err) <- readCreateProcessWithExitCode (shell cmd){ cwd = Just cwd } ""
  pure (ec, T.pack out <> T.pack err)

-- | Run a project build and return raw output.
buildOut :: IO T.Text
buildOut = do
  let cmd = actonCmd "build --color never --verbose"
  (_ec,out) <- runIn projDir cmd
  pure out

-- | Run a project build and return sanitized output.
goldenBuild :: IO LBS.ByteString
goldenBuild = do
  let cmd = actonCmd "build --color never --verbose"
  (_ec,out) <- runIn projDir cmd
  pure (sanitize out)

-- | Run a single-file build and return sanitized output.
goldenBuildFile :: IO LBS.ByteString
goldenBuildFile = do
  let cmd = actonCmd "src/c.act --color never --verbose"
  (_ec,out) <- runIn projDir cmd
  pure (sanitize out)

-- | Run a single-file build and return raw output.
buildOutFile :: IO T.Text
buildOutFile = do
  let cmd = actonCmd "src/c.act --color never --verbose"
  (_ec,out) <- runIn projDir cmd
  pure out

-- | Check whether build output reports typechecking for a module.
typechecked :: T.Text -> T.Text -> Bool
typechecked out modName =
  any (\line -> isTypecheckLine line && matchesModule line modName) (T.lines out)
  where
    isTypecheckLine line =
      "Finished type check of" `T.isInfixOf` line ||
      "Type check done" `T.isInfixOf` line

-- | Check whether build output reports compilation for a module.
compiled :: T.Text -> T.Text -> Bool
compiled out modName =
  any (\line -> isCompiledLine line && matchesModule line modName) (T.lines out)
  where
    isCompiledLine line =
      "Finished compilation of" `T.isInfixOf` line ||
      "Compilation done" `T.isInfixOf` line

-- | Match module labels across legacy "proj/mod" and new "proj.mod" formats.
matchesModule :: T.Text -> T.Text -> Bool
matchesModule line modName =
  let dotName = T.replace "/" "." modName
      leafName = T.takeWhileEnd (/= '/') modName
      tokens = T.words line
  in any (`elem` tokens) [modName, dotName, leafName]

-- | Remove a file if it exists.
removeIfExists :: FilePath -> IO ()
removeIfExists p = do
  exists <- doesFileExist p
  when exists $ removeFile p

-- | Remove a directory if it exists.
removeDirIfExists :: FilePath -> IO ()
removeDirIfExists p = do
  exists <- doesDirectoryExist p
  when exists $ removeDirectoryRecursive p

artifactExists :: FilePath -> IO Bool
artifactExists p
  | takeExtension p == ".tydb" = doesDirectoryExist p
  | otherwise = doesFileExist p

-- | Reset a project directory including deps and build outputs.
ensureCleanProjectAt :: FilePath -> IO ()
ensureCleanProjectAt proj = do
  ensureCleanAt proj
  removeDirIfExists (proj </> "deps")
  removeIfExists (proj </> "build.zig")
  removeIfExists (proj </> "build.zig.zon")
  removeIfExists (proj </> ".acton.compile.lock")
  removeIfExists (proj </> ".acton.lock")

withBackgroundCompilerLockHeld :: FilePath -> IO a -> IO a
withBackgroundCompilerLockHeld proj action = do
  mlock <- Compile.tryBackgroundCompilerLock proj
  case mlock of
    Nothing ->
      assertFailure ("failed to acquire .acton.compile.lock in " ++ proj)
    Just lock ->
      action `E.finally` Compile.releaseBackgroundCompilerLock lock

-- | Write a Build.act file with optional path deps.
writeBuildAct :: FilePath -> String -> [(String, FilePath)] -> IO ()
writeBuildAct proj name deps = do
  let header = [ T.pack ("name = \"" ++ name ++ "\"")
               , T.pack ("fingerprint = " ++ fingerprintForName name)
               ]
      depLines
        | null deps = []
        | otherwise =
            [ ""
            , "dependencies = {"
            ] ++
            [ T.pack ("  \"" ++ depName ++ "\": (path=\"" ++ depPath ++ "\")")
            | (depName, depPath) <- deps
            ] ++
            [ "}"
            ]
  writeFileUtf8 (proj </> "Build.act") (T.unlines (header ++ depLines))

-- | Create a dependency project under deps/.
ensureDepProject :: FilePath -> String -> IO FilePath
ensureDepProject proj depName = do
  let depDir = proj </> "deps" </> depName
  createDirectoryIfMissing True (depDir </> "src")
  writeBuildAct depDir depName []
  pure depDir

ensureCasesProject :: IO ()
ensureCasesProject = ensureCasesProjectWithDeps []

ensureCasesProjectWithDeps :: [(String, FilePath)] -> IO ()
ensureCasesProjectWithDeps deps = do
  ensureCleanProjectAt casesProjDir
  writeBuildAct casesProjDir casesProjName deps

-- | Replace the first line of a file with a new string.
rewriteFirstLine :: FilePath -> String -> IO ()
rewriteFirstLine path newLine = do
  content <- T.readFile path
  let rest = snd (T.breakOn "\n" content)
  T.writeFile path (T.pack newLine <> rest)

-- | Resolve the acton binary path.
actonPath :: IO FilePath
actonPath = canonicalizePath (".." </> ".." </> "dist" </> "bin" </> "acton")

-- | Run acton in a directory with fixed job count.
runActonIn :: FilePath -> [String] -> IO (ExitCode, T.Text)
runActonIn cwd args = do
  actonExe <- actonPath
  let args' = args ++ ["--jobs", "1"]
  (ec,out,err) <- readCreateProcessWithExitCode (proc actonExe args'){ cwd = Just cwd } ""
  pure (ec, T.pack out <> T.pack err)

-- | Assert an exit success and include output on failure.
assertExitSuccess :: String -> (ExitCode, T.Text) -> IO ()
assertExitSuccess label (ec, out) =
  case ec of
    ExitSuccess -> pure ()
    ExitFailure c ->
      assertFailure (label ++ " failed (exit " ++ show c ++ ")\n" ++ T.unpack out)

-- | Assert a specific exit failure and include output on mismatch.
assertExitFailure :: String -> Int -> (ExitCode, T.Text) -> IO ()
assertExitFailure label code (ec, out) =
  case ec of
    ExitFailure c | c == code -> pure ()
    ExitFailure c ->
      assertFailure (label ++ " failed (expected exit " ++ show code ++ ", got " ++ show c ++ ")\n" ++ T.unpack out)
    ExitSuccess ->
      assertFailure (label ++ " unexpectedly succeeded\n" ++ T.unpack out)

-- | Run a verbose build in a project directory and return raw output.
buildOutIn :: FilePath -> IO T.Text
buildOutIn proj = do
  res@(ec, out) <- runActonIn proj ["build", "--color", "never", "--verbose"]
  assertExitSuccess ("build in " ++ proj) res
  pure out

-- | Run a verbose build with extra args and return raw output.
buildOutInArgs :: FilePath -> [String] -> IO T.Text
buildOutInArgs proj args = do
  res@(ec, out) <- runActonIn proj (["build", "--color", "never", "--verbose"] ++ args)
  assertExitSuccess ("build in " ++ proj) res
  pure out

-- | Format a module label for output comparisons.
modLabel :: FilePath -> String -> T.Text
modLabel proj mod = T.pack (takeFileName proj ++ "/" ++ mod)

-- | Build the default rebuild project.
buildProject :: IO ()
buildProject = do
  let cmd = actonCmd "build --color never"
  (ec,out) <- runIn projDir cmd
  case ec of
    ExitSuccess -> pure ()
    ExitFailure c -> assertFailure ("acton build failed: " ++ show c ++ "\n" ++ T.unpack out)

-- | Read name hashes from a .tydb file.
readTyNameHashes :: FilePath -> IO [InterfaceFiles.NameHashInfo]
readTyNameHashes tyPath = do
  (_, _, _, _, _, _, _, _, nameHashes, _, _, _) <- InterfaceFiles.readFile tyPath
  pure nameHashes

-- | Read pub/impl dependency names for a binding in a .tydb file.
readTyDeps :: FilePath -> String -> IO ([String], [String])
readTyDeps tyPath nameLabel = do
  nameHashes <- readTyNameHashes tyPath
  let matchName nh = prstr (InterfaceFiles.nhName nh) == nameLabel
  case find matchName nameHashes of
    Nothing -> do
      assertFailure ("missing name " ++ nameLabel ++ " in " ++ tyPath)
      pure ([], [])
    Just nh -> do
      let pubDeps = sort (map (prstr . fst) (InterfaceFiles.nhPubDeps nh))
          implDeps = sort (map (prstr . fst) (InterfaceFiles.nhImplDeps nh))
      pure (pubDeps, implDeps)

-- | Read pub/impl local dependency names for a binding in a .tydb file.
readTyLocalDeps :: FilePath -> String -> IO ([String], [String])
readTyLocalDeps tyPath nameLabel = do
  nameHashes <- readTyNameHashes tyPath
  let matchName nh = prstr (InterfaceFiles.nhName nh) == nameLabel
  case find matchName nameHashes of
    Nothing -> do
      assertFailure ("missing name " ++ nameLabel ++ " in " ++ tyPath)
      pure ([], [])
    Just nh -> do
      let pubDeps = sort (map prstr (InterfaceFiles.nhPubLocalDeps nh))
          implDeps = sort (map prstr (InterfaceFiles.nhImplLocalDeps nh))
      pure (pubDeps, implDeps)

-- | Rewrite source hash and name-hash section of a .tydb file.
rewriteTySrcHashAndNameHashes :: FilePath -> B.ByteString -> ([InterfaceFiles.NameHashInfo] -> [InterfaceFiles.NameHashInfo]) -> IO ()
rewriteTySrcHashAndNameHashes tyPath srcHash' f = do
  (_mods, nmod, tmod, sourceMeta, _srcHash, pubHash, implHash, imps, nameHashes, roots, tests, mdoc) <- InterfaceFiles.readFile tyPath
  InterfaceFiles.writeFile tyPath srcHash' pubHash implHash sourceMeta imps (f nameHashes) roots tests mdoc nmod tmod

rewriteTySourceMeta :: FilePath -> Maybe InterfaceFiles.SourceFileMeta -> IO ()
rewriteTySourceMeta tyPath sourceMeta' = do
  (_mods, nmod, tmod, _sourceMeta, srcHash, pubHash, implHash, imps, nameHashes, roots, tests, mdoc) <- InterfaceFiles.readFile tyPath
  InterfaceFiles.writeFile tyPath srcHash pubHash implHash sourceMeta' imps nameHashes roots tests mdoc nmod tmod

rewriteTyVersion :: FilePath -> [Int] -> IO ()
rewriteTyVersion tyPath version' = do
  (_mods, nmod, tmod, sourceMeta, srcHash, pubHash, implHash, imps, nameHashes, roots, tests, mdoc) <- InterfaceFiles.readFile tyPath
  InterfaceFiles.writeFileWithVersion version' tyPath srcHash pubHash implHash sourceMeta imps nameHashes roots tests mdoc nmod tmod

readTySourceMeta :: FilePath -> IO (Maybe InterfaceFiles.SourceFileMeta)
readTySourceMeta tyPath = do
  (sourceMeta, _srcHash, _pubHash, _implHash, _imps, _nameHashes, _roots, _tests, _doc) <- InterfaceFiles.readHeader tyPath
  pure sourceMeta

sourceFileMetaForPath :: FilePath -> IO InterfaceFiles.SourceFileMeta
sourceFileMetaForPath path = do
  st <- getFileStatus path
  let mtimeNs = floor (toRational (modificationTimeHiRes st) * 1000000000)
      ctimeNs = floor (toRational (statusChangeTimeHiRes st) * 1000000000)
  pure InterfaceFiles.SourceFileMeta
    { InterfaceFiles.sfmMTimeNs = mtimeNs
    , InterfaceFiles.sfmCTimeNs = ctimeNs
    , InterfaceFiles.sfmSize = fromIntegral (fileSize st)
    , InterfaceFiles.sfmDevice = Just (fromIntegral (deviceID st))
    , InterfaceFiles.sfmInode = Just (fromIntegral (fileID st))
    }

setFileMTimeNs :: FilePath -> Integer -> IO ()
setFileMTimeNs path mtimeNs =
  setModificationTime path (posixSecondsToUTCTime (fromRational (toRational mtimeNs / 1000000000)))

-- | Drop a qualified dependency label from all stored pub/impl dep lists.
dropTyDepByLabel :: String -> [InterfaceFiles.NameHashInfo] -> [InterfaceFiles.NameHashInfo]
dropTyDepByLabel depLabel =
  map dropFromInfo
  where
    keepDep (qn, _h) = prstr qn /= depLabel
    dropFromInfo nh =
      nh { InterfaceFiles.nhPubDeps  = filter keepDep (InterfaceFiles.nhPubDeps nh)
         , InterfaceFiles.nhImplDeps = filter keepDep (InterfaceFiles.nhImplDeps nh)
         }

-- | Assert that a dependency list includes all expected names.
assertDepsContain :: String -> [String] -> [String] -> IO ()
assertDepsContain label expected actual =
  let missing = filter (`notElem` actual) expected
  in unless (null missing) $
       assertFailure (label ++ " missing deps: " ++ show missing ++ "\nactual: " ++ show actual)

-- | Run a project binary and capture stdout.
runBinaryIn :: FilePath -> String -> IO T.Text
runBinaryIn proj name = do
  let bin = "out" </> "bin" </> name
  (ec,out,err) <- readCreateProcessWithExitCode (proc bin []){ cwd = Just proj } ""
  case ec of
    ExitSuccess -> pure (T.pack out)
    ExitFailure c -> assertFailure ("Binary failed (exit " ++ show c ++ ") stderr: " ++ err)

runBinary :: String -> IO T.Text
runBinary = runBinaryIn projDir

-- Project scenario steps ------------------------------------------------------

p01_init :: TestTree
p01_init = testCase "01-init" $ do
  ensureClean
  writeFileUtf8 (srcDir </> "a.act") "aaa = 123\n"
  writeFileUtf8 (srcDir </> "b.act") $ T.unlines
    [ "import a"
    , "def baa():"
    , "    return a.aaa"
    , ""
    , "class DocInfo:"
    , "    def get(self) -> int:"
    , "        \"get value\""
    , "        return 1"
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

p05_run_123 :: TestTree
p05_run_123 = testCase "05-run 123" $ do
  out <- runBinary "c"
  out @?= "123\n"

p06_change_a_impl :: TestTree
p06_change_a_impl = goldenVsString "06-change-a-impl.golden" (goldenDir </> "project_06-change-a-impl.golden") $ do
  writeFileUtf8 (srcDir </> "a.act") "aaa = 124\n"
  out <- buildOut
  -- Expect only a.act to type check; b/c should re-run codegen only.
  assertBool "expected a.act to compile" (typechecked out "rebuild/a")
  assertBool "did not expect b.act to compile" (not (typechecked out "rebuild/b"))
  assertBool "did not expect c.act to compile" (not (typechecked out "rebuild/c"))
  assertBool "expected b.act to compile codegen" (compiled out "rebuild/b")
  assertBool "expected c.act to compile codegen" (compiled out "rebuild/c")
  pure (sanitize out)

p06_impl_change_fresh :: TestTree
p06_impl_change_fresh = testCase "06-impl-change-up-to-date" $ do
  out <- buildOut
  assertBool "did not expect a.act to compile" (not (typechecked out "rebuild/a"))
  assertBool "did not expect b.act to compile" (not (typechecked out "rebuild/b"))
  assertBool "did not expect c.act to compile" (not (typechecked out "rebuild/c"))
  assertBool "did not expect b.act to compile codegen" (not (compiled out "rebuild/b"))
  assertBool "did not expect c.act to compile codegen" (not (compiled out "rebuild/c"))

p07_run_124 :: TestTree
p07_run_124 = testCase "07-run 124" $ do
  out <- runBinary "c"
  out @?= "124\n"

p08_change_b_impl :: TestTree
p08_change_b_impl = goldenVsString "08-change-b-impl.golden" (goldenDir </> "project_08-change-b-impl.golden") $ do
  writeFileUtf8 (srcDir </> "b.act") $ T.unlines
    [ "import a"
    , "def baa():"
    , "    tmp = a.aaa"
    , "    return tmp"
    , ""
    , "class DocInfo:"
    , "    def get(self) -> int:"
    , "        \"get value\""
    , "        return 1"
    ]
  out <- buildOut
  -- Expect b.act to compile, but not a.act or c.act in project build
  assertBool "did not expect a.act to compile" (not (typechecked out "rebuild/a"))
  assertBool "expected b.act to compile" (typechecked out "rebuild/b")
  assertBool "did not expect c.act to compile" (not (typechecked out "rebuild/c"))
  pure (sanitize out)

p09_run_124 :: TestTree
p09_run_124 = testCase "09-run 124" $ do
  out <- runBinary "c"
  out @?= "124\n"

-- Public-hash behavior tests -----------------------------------------------

-- Changing public interface of an import should rebuild dependents that use it
p10_change_a_iface :: TestTree
p10_change_a_iface =
  goldenVsString "10-change-a-iface.golden"
                 (goldenDir </> "project_10-change-a-iface.golden") $ do
    -- Change the type of a.aaa (public signature change).
    writeFileUtf8 (srcDir </> "a.act") "aaa = \"125\"\n"
    out <- buildOut
    -- Should rebuild a.act and b.act; with import-augmented public hashing, b's
    -- public hash changes when a's interface changes
    assertBool "expected a.act to compile" (typechecked out "rebuild/a")
    assertBool "expected b.act to compile" (typechecked out "rebuild/b")
    -- c should rebuild too, since it imports b which has now changed since its import a changed..
    assertBool "expected c.act to compile" (typechecked out "rebuild/c")
    pure (sanitize out)

-- Docstring-only change in an imported module should not rebuild dependents
p11_change_b_doc :: TestTree
p11_change_b_doc =
  goldenVsString "11-change-b-doc.golden"
                 (goldenDir </> "project_11-change-b-doc.golden") $ do
    -- Change only the docstring of a method in b.act; this should recompile b
    -- (source changed) but not its dependent c (public hash is doc-free).
    writeFileUtf8 (srcDir </> "b.act") $ T.unlines
      [ "import a"
      , "def baa():"
      , "    tmp = a.aaa"
      , "    return tmp"
      , ""
      , "class DocInfo:"
      , "    def get(self) -> int:"
      , "        \"GET VALUE v2\""
      , "        return 1"
      ]
    out <- buildOut
    assertBool "expected b.act to compile" (typechecked out "rebuild/b")
    assertBool "did not expect c.act to compile" (not (typechecked out "rebuild/c"))
    pure (sanitize out)

p12_codegen_stale :: TestTree
p12_codegen_stale =
  goldenVsString "12-codegen-stale.golden"
                 (goldenDir </> "project_12-codegen-stale.golden") $ do
    let bBase = projDir </> "out" </> "types" </> "b"
    removeIfExists (bBase ++ ".c")
    removeIfExists (bBase ++ ".h")
    out <- buildOut
    assertBool "expected b.act to compile codegen" (compiled out "rebuild/b")
    assertBool "did not expect b.act to type check" (not (typechecked out "rebuild/b"))
    assertBool "did not expect a.act to type check" (not (typechecked out "rebuild/a"))
    assertBool "did not expect c.act to compile codegen" (not (compiled out "rebuild/c"))
    pure (sanitize out)

f01_init :: TestTree
f01_init = testCase "01-init" $ do
  ensureClean
  writeFileUtf8 (srcDir </> "a.act") "aaa = 123\n"
  writeFileUtf8 (srcDir </> "b.act") $ T.unlines
    [ "import a"
    , "def baa():"
    , "    return a.aaa"
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

f05_run_123 :: TestTree
f05_run_123 = testCase "05-run 123" $ do
  out <- runBinary "c"
  out @?= "123\n"

f06_change_a_impl :: TestTree
f06_change_a_impl = goldenVsString "06-change-a-impl.golden" (goldenDir </> "file_06-change-a-impl.golden") $ do
  writeFileUtf8 (srcDir </> "a.act") "aaa = 124\n"
  out <- buildOutFile
  -- Expect only a.act to compile in single-file build
  assertBool "expected a.act to compile" (typechecked out "rebuild/a")
  assertBool "did not expect b.act to compile" (not (typechecked out "rebuild/b"))
  assertBool "did not expect c.act to compile" (not (typechecked out "rebuild/c"))
  pure (sanitize out)

f07_run_124 :: TestTree
f07_run_124 = testCase "07-run 124" $ do
  out <- runBinary "c"
  out @?= "124\n"

f08_change_b_impl :: TestTree
f08_change_b_impl = goldenVsString "08-change-b-impl.golden" (goldenDir </> "file_08-change-b-impl.golden") $ do
  writeFileUtf8 (srcDir </> "b.act") $ T.unlines
    [ "import a"
    , "def baa():"
    , "    tmp = a.aaa"
    , "    return tmp"
    , ""
    , "class DocInfo:"
    , "    def get(self) -> int:"
    , "        \"get value\""
    , "        return 1"
    ]
  out <- buildOutFile
  -- Expect b.act to compile; c.act should not re-typecheck on impl-only changes.
  assertBool "did not expect a.act to compile" (not (typechecked out "rebuild/a"))
  assertBool "expected b.act to compile" (typechecked out "rebuild/b")
  assertBool "did not expect c.act to compile" (not (typechecked out "rebuild/c"))
  pure (sanitize out)

f09_run_124 :: TestTree
f09_run_124 = testCase "09-run 124" $ do
  out <- runBinary "c"
  out @?= "124\n"

-- Alt-output behavior --------------------------------------------------------

-- When requesting alternative output (e.g., --types), ensure the compiler runs
-- the relevant passes and prints the output even if the file is otherwise
-- up-to-date.
f10_alt_output :: TestTree
f10_alt_output = testCase "10-alt output" $ do
  -- Run build an extra time to make sure we are up to date
  _ <- buildOutFile

  -- Now request alternative output on the same file without modifying sources
  let cmd = actonCmd "src/c.act --color never --types"
  (_ec,out) <- runIn projDir cmd
  -- Expect a types dump header for module c
  assertBool "expected types dump for module c" (T.isInfixOf "== types: c" out)

-- Project case tests --------------------------------------------------------

p14_partial_rebuild :: TestTree
p14_partial_rebuild = testCase "14-partial rebuild" $ do
  ensureCasesProject
  writeFileUtf8 (casesSrcDir </> "rebuild.act") $ T.unlines
    [ "actor main(env: Env):"
    , "    env.exit(0)"
    ]
  res1 <- runActonIn casesProjDir ["build", "--color", "never"]
  assertExitSuccess "initial build" res1
  touch (casesSrcDir </> "rebuild.act")
  res2 <- runActonIn casesProjDir ["build", "--color", "never"]
  assertExitSuccess "rebuild after touch" res2

p15_rebuild_import :: TestTree
p15_rebuild_import = testCase "15-rebuild with stdlib import" $ do
  ensureCasesProject
  writeFileUtf8 (casesSrcDir </> "rebuild.act") $ T.unlines
    [ "import json"
    , ""
    , "actor main(env: Env):"
    , "    env.exit(0)"
    ]
  res1 <- runActonIn casesProjDir ["build", "--color", "never"]
  assertExitSuccess "initial build" res1
  res2 <- runActonIn casesProjDir ["build", "--color", "never"]
  assertExitSuccess "second build" res2

p16_dep_api_change :: TestTree
p16_dep_api_change = testCase "16-dependency API change triggers rebuild" $ do
  let originalContent = T.unlines
        [ "# Initial version"
        , "def calculate(x: int) -> int:"
        , "    return x * 2"
        ]
      modifiedContent = T.unlines
        [ "# Modified version"
        , "def calculate(x: int, y: int) -> int:"
        , "    return x * y"
        ]
  ensureCasesProjectWithDeps [("libfoo", "deps/libfoo")]
  depDir <- ensureDepProject casesProjDir "libfoo"
  let depSrc = depDir </> "src" </> "lib.act"
  writeFileUtf8 depSrc originalContent
  writeFileUtf8 (casesSrcDir </> "main.act") $ T.unlines
    [ "import libfoo"
    , ""
    , "actor main(env: Env):"
    , "    result = libfoo.calculate(21)"
    , "    print(\"Result: %d\" % result)"
    , "    env.exit(0)"
    ]
  res1 <- runActonIn casesProjDir ["build", "--color", "never"]
  assertExitSuccess "initial build" res1
  writeFileUtf8 depSrc modifiedContent
  res2 <- runActonIn casesProjDir ["build", "--color", "never"]
  assertExitFailure "rebuild after API change" 1 res2

p17_dep_impl_change :: TestTree
p17_dep_impl_change = testCase "17-dependency impl change triggers back job" $ do
  let originalContent = T.unlines
        [ "# Initial version"
        , "def calculate(x: int) -> int:"
        , "    return x * 2"
        ]
      modifiedContent = T.unlines
        [ "# Modified version"
        , "def calculate(x: int) -> int:"
        , "    return x * 3"
        ]
      modMain = modLabel casesProjDir "main"
  ensureCasesProjectWithDeps [("libfoo", "deps/libfoo")]
  depDir <- ensureDepProject casesProjDir "libfoo"
  let depSrc = depDir </> "src" </> "lib.act"
  writeFileUtf8 depSrc originalContent
  writeFileUtf8 (casesSrcDir </> "main.act") $ T.unlines
    [ "import libfoo"
    , ""
    , "actor main(env: Env):"
    , "    result = libfoo.calculate(21)"
    , "    print(\"Result: %d\" % result)"
    , "    env.exit(0)"
    ]
  res1 <- runActonIn casesProjDir ["build", "--color", "never"]
  assertExitSuccess "initial build" res1
  writeFileUtf8 depSrc modifiedContent
  res2@(ec2, out2) <- runActonIn casesProjDir ["build", "--color", "never", "--verbose"]
  assertExitSuccess "rebuild after impl change" res2
  assertBool "expected impl change log" (T.isInfixOf "impl changes in libfoo.calculate" out2)
  assertBool "did not expect main to type check" (not (typechecked out2 modMain))
  assertBool "expected main to compile codegen" (compiled out2 modMain)

-- Incremental/hash case coverage --------------------------------------------

p18_type_only_deps :: TestTree
p18_type_only_deps = testCase "18-type-only deps in signatures" $ do
  let proj = casesProjDir
      src = casesSrcDir
      modB = modLabel proj "b"
      modC = modLabel proj "c"
  ensureCasesProject
  writeFileUtf8 (src </> "a.act") $ T.unlines
    [ "class Foo:"
    , "    def get(self) -> int:"
    , "        return 1"
    ]
  writeFileUtf8 (src </> "b.act") $ T.unlines
    [ "import a"
    , ""
    , "def takes(x: a.Foo) -> int:"
    , "    return 0"
    ]
  writeFileUtf8 (src </> "c.act") $ T.unlines
    [ "actor main(env: Env):"
    , "    env.exit(0)"
    ]
  _ <- buildOutIn proj
  writeFileUtf8 (src </> "a.act") $ T.unlines
    [ "class Foo:"
    , "    def get(self) -> int:"
    , "        return 2"
    ]
  out1 <- buildOutIn proj
  assertBool "did not expect b.act to type check" (not (typechecked out1 modB))
  assertBool "did not expect c.act to type check" (not (typechecked out1 modC))
  writeFileUtf8 (src </> "a.act") $ T.unlines
    [ "class Foo:"
    , "    def get(self) -> float:"
    , "        return 2.0"
    ]
  out2 <- buildOutIn proj
  assertBool "expected b.act to type check" (typechecked out2 modB)
  assertBool "did not expect c.act to type check" (not (typechecked out2 modC))

p19_unused_import :: TestTree
p19_unused_import = testCase "19-unused import does not propagate" $ do
  let proj = casesProjDir
      src = casesSrcDir
      modB = modLabel proj "b"
      modC = modLabel proj "c"
  ensureCasesProject
  writeFileUtf8 (src </> "a.act") $ T.unlines
    [ "def foo() -> int:"
    , "    return 1"
    ]
  writeFileUtf8 (src </> "b.act") $ T.unlines
    [ "import a"
    , ""
    , "def bar() -> int:"
    , "    return a.foo()"
    ]
  writeFileUtf8 (src </> "c.act") $ T.unlines
    [ "import b"
    , ""
    , "actor main(env: Env):"
    , "    b.bar()"
    , "    env.exit(0)"
    ]
  _ <- buildOutIn proj
  writeFileUtf8 (src </> "b.act") $ T.unlines
    [ "import a"
    , "import testing"
    , ""
    , "def bar() -> int:"
    , "    return a.foo()"
    ]
  out <- buildOutIn proj
  assertBool "expected b.act to type check" (typechecked out modB)
  assertBool "did not expect c.act to type check" (not (typechecked out modC))

p20_add_remove_names :: TestTree
p20_add_remove_names = testCase "20-add/remove top-level names" $ do
  let proj = casesProjDir
      src = casesSrcDir
      modB = modLabel proj "b"
      modC = modLabel proj "c"
  ensureCasesProject
  writeFileUtf8 (src </> "a.act") "foo = 1\n"
  writeFileUtf8 (src </> "b.act") $ T.unlines
    [ "import a"
    , ""
    , "def bar() -> int:"
    , "    return a.foo"
    ]
  writeFileUtf8 (src </> "c.act") $ T.unlines
    [ "import b"
    , ""
    , "actor main(env: Env):"
    , "    b.bar()"
    , "    env.exit(0)"
    ]
  _ <- buildOutIn proj
  writeFileUtf8 (src </> "a.act") $ T.unlines
    [ "foo = 1"
    , "bar = 2"
    ]
  out1 <- buildOutIn proj
  let addDelta = "+bar"
  assertBool "expected +bar hash delta" (addDelta `T.isInfixOf` out1)
  assertBool "did not expect b.act to type check" (not (typechecked out1 modB))
  assertBool "did not expect c.act to type check" (not (typechecked out1 modC))
  writeFileUtf8 (src </> "a.act") "foo = 1\n"
  out2 <- buildOutIn proj
  let removeDelta = "-bar"
  assertBool "expected -bar hash delta" (removeDelta `T.isInfixOf` out2)
  assertBool "did not expect b.act to type check" (not (typechecked out2 modB))
  assertBool "did not expect c.act to type check" (not (typechecked out2 modC))

p21_recursive_group :: TestTree
p21_recursive_group = testCase "21-recursive group impl change propagates" $ do
  let proj = casesProjDir
      src = casesSrcDir
      modB = modLabel proj "b"
  ensureCasesProject
  writeFileUtf8 (src </> "a.act") $ T.unlines
    [ "def even(n: int) -> bool:"
    , "    if n == 0:"
    , "        return True"
    , "    else:"
    , "        return odd(n - 1)"
    , ""
    , "def odd(n: int) -> bool:"
    , "    if n == 0:"
    , "        return False"
    , "    else:"
    , "        return even(n - 1)"
    ]
  writeFileUtf8 (src </> "b.act") $ T.unlines
    [ "import a"
    , ""
    , "def use() -> bool:"
    , "    return a.even(2)"
    ]
  writeFileUtf8 (src </> "c.act") $ T.unlines
    [ "import b"
    , ""
    , "actor main(env: Env):"
    , "    b.use()"
    , "    env.exit(0)"
    ]
  _ <- buildOutIn proj
  writeFileUtf8 (src </> "a.act") $ T.unlines
    [ "def even(n: int) -> bool:"
    , "    if n == 0:"
    , "        return True"
    , "    else:"
    , "        return odd(n - 1)"
    , ""
    , "def odd(n: int) -> bool:"
    , "    if n == 0:"
    , "        return True"
    , "    else:"
    , "        return even(n - 1)"
    ]
  out <- buildOutIn proj
  assertBool "expected even impl delta" (T.isInfixOf "~even{impl" out)
  assertBool "expected odd src delta" (T.isInfixOf "~odd{src" out)
  assertBool "expected b.act to compile codegen" (compiled out modB)
  assertBool "did not expect b.act to type check" (not (typechecked out modB))

p22_multibind :: TestTree
p22_multibind = testCase "22-multi-binding updates per-name hashes" $ do
  let proj = casesProjDir
      src = casesSrcDir
      modB = modLabel proj "b"
  ensureCasesProject
  writeFileUtf8 (src </> "a.act") "x, y = 1, 2\n"
  writeFileUtf8 (src </> "b.act") $ T.unlines
    [ "import a"
    , ""
    , "def get() -> int:"
    , "    return a.x"
    ]
  writeFileUtf8 (src </> "c.act") $ T.unlines
    [ "import b"
    , ""
    , "actor main(env: Env):"
    , "    b.get()"
    , "    env.exit(0)"
    ]
  _ <- buildOutIn proj
  writeFileUtf8 (src </> "a.act") "x, y = 1, 3\n"
  out <- buildOutIn proj
  assertBool "expected x src delta" (T.isInfixOf "~x{src" out)
  assertBool "expected y src delta" (T.isInfixOf "~y{src" out)
  assertBool "expected b.act to compile codegen" (compiled out modB)

p23_codegen_mismatch :: TestTree
p23_codegen_mismatch = testCase "23-codegen hash mismatch triggers rebuild" $ do
  let proj = casesProjDir
      src = casesSrcDir
      modB = modLabel proj "b"
      bC = proj </> "out" </> "types" </> "b.c"
  ensureCasesProject
  writeFileUtf8 (src </> "a.act") $ T.unlines
    [ "def foo() -> int:"
    , "    return 1"
    ]
  writeFileUtf8 (src </> "b.act") $ T.unlines
    [ "import a"
    , ""
    , "def bar() -> int:"
    , "    return a.foo()"
    ]
  writeFileUtf8 (src </> "c.act") $ T.unlines
    [ "import b"
    , ""
    , "actor main(env: Env):"
    , "    b.bar()"
    , "    env.exit(0)"
    ]
  _ <- buildOutIn proj
  rewriteFirstLine bC "/* Acton impl hash: deadbeef */"
  out <- buildOutIn proj
  assertBool "expected codegen stale message" (T.isInfixOf "generated code out of date" out)
  assertBool "expected b.act to compile codegen" (compiled out modB)
  assertBool "did not expect b.act to type check" (not (typechecked out modB))

p24_codegen_equal_hash :: TestTree
p24_codegen_equal_hash = testCase "24-codegen equal hash mismatch formats single delta" $ do
  let proj = casesProjDir
      src = casesSrcDir
      modB = modLabel proj "b"
      bC = proj </> "out" </> "types" </> "b.c"
      bH = proj </> "out" </> "types" </> "b.h"
  ensureCasesProject
  writeFileUtf8 (src </> "a.act") $ T.unlines
    [ "def foo() -> int:"
    , "    return 1"
    ]
  writeFileUtf8 (src </> "b.act") $ T.unlines
    [ "import a"
    , ""
    , "def bar() -> int:"
    , "    return a.foo()"
    ]
  writeFileUtf8 (src </> "c.act") $ T.unlines
    [ "import b"
    , ""
    , "actor main(env: Env):"
    , "    b.bar()"
    , "    env.exit(0)"
    ]
  _ <- buildOutIn proj
  rewriteFirstLine bC "/* Acton impl hash: deadbeef */"
  rewriteFirstLine bH "/* Acton impl hash: deadbeef */"
  out <- buildOutIn proj
  assertBool "expected single-delta codegen message"
    (T.isInfixOf "generated code out of date {impl deadbeef ->" out)
  assertBool "expected b.act to compile codegen" (compiled out modB)
  assertBool "did not expect b.act to type check" (not (typechecked out modB))

p25_whitespace_change :: TestTree
p25_whitespace_change = testCase "25-whitespace-only change does not propagate" $ do
  let proj = casesProjDir
      src = casesSrcDir
      modB = modLabel proj "b"
      modC = modLabel proj "c"
  ensureCasesProject
  writeFileUtf8 (src </> "a.act") $ T.unlines
    [ "def foo() -> int:"
    , "    return 1"
    ]
  writeFileUtf8 (src </> "b.act") $ T.unlines
    [ "import a"
    , ""
    , "def bar() -> int:"
    , "    return a.foo()"
    ]
  writeFileUtf8 (src </> "c.act") $ T.unlines
    [ "import b"
    , ""
    , "actor main(env: Env):"
    , "    b.bar()"
    , "    env.exit(0)"
    ]
  _ <- buildOutIn proj
  writeFileUtf8 (src </> "a.act") $ T.unlines
    [ "# comment"
    , "def foo() -> int:"
    , "    return 1"
    ]
  out <- buildOutIn proj
  assertBool "did not expect b.act to type check" (not (typechecked out modB))
  assertBool "did not expect c.act to type check" (not (typechecked out modC))

p26_corrupt_ty_header :: TestTree
p26_corrupt_ty_header = testCase "26-corrupt .tydb header forces re-parse" $ do
  let proj = casesProjDir
      src = casesSrcDir
      modA = modLabel proj "a"
      tyA = proj </> "out" </> "types" </> "a.tydb"
  ensureCasesProject
  writeFileUtf8 (src </> "a.act") "aaa = 1\n"
  writeFileUtf8 (src </> "c.act") $ T.unlines
    [ "import a"
    , ""
    , "actor main(env: Env):"
    , "    print(a.aaa)"
    , "    env.exit(0)"
    ]
  _ <- buildOutIn proj
  removePathForcibly tyA
  createDirectoryIfMissing True tyA
  writeFileUtf8 (tyA </> "data.mdb") "garbage\n"
  out <- buildOutIn proj
  assertBool "expected a.act to type check after corrupt .tydb" (typechecked out modA)

p26_ty_version_mismatch :: TestTree
p26_ty_version_mismatch = testCase "26b-.tydb version mismatch forces re-parse" $ do
  let proj = casesProjDir
      src = casesSrcDir
      modA = modLabel proj "a"
      tyA = proj </> "out" </> "types" </> "a.tydb"
  ensureCasesProject
  writeFileUtf8 (src </> "a.act") "aaa = 1\n"
  writeFileUtf8 (src </> "c.act") $ T.unlines
    [ "import a"
    , ""
    , "actor main(env: Env):"
    , "    print(a.aaa)"
    , "    env.exit(0)"
    ]
  _ <- buildOutIn proj
  rewriteTyVersion tyA (map (+ 1) A.version)
  out <- buildOutIn proj
  assertBool "expected a.act to type check after .tydb version mismatch" (typechecked out modA)
  assertBool "did not expect raw .tydb version mismatch" $
    not (".tydb version mismatch" `T.isInfixOf` out)

p27_overlay_source_provider :: TestTree
p27_overlay_source_provider = testCase "27-overlay snapshots drive readModuleTask" $ do
  let proj = casesProjDir
      src = casesSrcDir
      actA = src </> "a.act"
  ensureCasesProject
  writeFileUtf8 actA "aaa = 1\n"
  writeFileUtf8 (src </> "c.act") $ T.unlines
    [ "import a"
    , ""
    , "actor main(env: Env):"
    , "    print(a.aaa)"
    , "    env.exit(0)"
    ]
  _ <- buildOutIn proj
  actAAbs <- canonicalizePath actA
  bytes <- B.readFile actAAbs
  let text = T.unpack (TE.decodeUtf8 bytes)
      snapSame = Source.SourceSnapshot
        { Source.ssText = text
        , Source.ssBytes = bytes
        , Source.ssIsOverlay = True
        }
      disk = Source.diskSourceProvider
      spSame = disk { Source.spReadOverlay = \path -> return (if path == actAAbs then Just snapSame else Nothing) }
      gopts = C.GlobalOptions
        { C.color = C.Never
        , C.quiet = True
        , C.noProgress = False
        , C.timing = False
        , C.tty = False
        , C.verbose = False
        , C.verboseZig = False
        , C.jobs = 1
        }
  paths <- Compile.findPaths actAAbs Compile.defaultCompileOptions
  taskSame <- Compile.readModuleTask spSame gopts Compile.defaultCompileOptions paths actAAbs
  case taskSame of
    Compile.TyTask{} -> pure ()
    _ -> assertFailure "expected TyTask when overlay matches header"
  let textDiff = "\"\"\"Overlay doc\"\"\"\naaa = 2\n"
      bytesDiff = TE.encodeUtf8 (T.pack textDiff)
      snapDiff = Source.SourceSnapshot
        { Source.ssText = textDiff
        , Source.ssBytes = bytesDiff
        , Source.ssIsOverlay = True
        }
      spDiff = disk { Source.spReadOverlay = \path -> return (if path == actAAbs then Just snapDiff else Nothing) }
  taskDiff <- Compile.readModuleTask spDiff gopts Compile.defaultCompileOptions paths actAAbs
  case taskDiff of
    Compile.ParseTask{ Compile.src = srcText, Compile.srcBytes = srcBytes } -> do
      srcText @?= textDiff
      srcBytes @?= bytesDiff
    _ -> assertFailure "expected ParseTask when overlay differs from header"
  docDiff <- Compile.readModuleDoc spDiff gopts Compile.defaultCompileOptions paths actAAbs
  case docDiff of
    Just (mn, Just doc) -> do
      mn @?= A.modName ["a"]
      doc @?= "Overlay doc"
    _ -> assertFailure "expected module doc from overlay header"

p28_protocol_extension_deps :: TestTree
p28_protocol_extension_deps = testCase "28-protocol/extension deps are recorded by name" $ do
  let proj = casesProjDir
      src = casesSrcDir
      tyA = proj </> "out" </> "types" </> "a.tydb"
      tyB = proj </> "out" </> "types" </> "b.tydb"
  ensureCasesProject
  writeFileUtf8 (src </> "a.act") $ T.unlines
    [ "protocol FooProto:"
    , "    foo : () -> int"
    , ""
    , "protocol BazProto:"
    , "    baz : () -> int"
    , ""
    , "protocol BarProto (FooProto, BazProto):"
    , "    bar : () -> int"
    , ""
    , "class Widget:"
    , "    def __init__(self):"
    , "        pass"
    , ""
    , "extension Widget(BarProto):"
    , "    def foo(self) -> int:"
    , "        return 0"
    , "    def baz(self) -> int:"
    , "        return 2"
    , "    def bar(self) -> int:"
    , "        return 1"
    , ""
    , "def uses_proto(x: BarProto) -> int:"
    , "    return 0"
    , ""
    , "def uses_class(x: Widget) -> int:"
    , "    return 0"
    ]
  writeFileUtf8 (src </> "b.act") $ T.unlines
    [ "import a"
    , ""
    , "def runner(x: a.BarProto) -> int:"
    , "    return a.uses_class(a.Widget())"
    ]
  writeFileUtf8 (src </> "c.act") $ T.unlines
    [ "import a"
    , "import b"
    , ""
    , "actor main(env: Env):"
    , "    b.runner(a.Widget())"
    , "    env.exit(0)"
    ]
  _ <- buildOutIn proj
  nameHashesA <- readTyNameHashes tyA
  let namesA = sort (map (prstr . InterfaceFiles.nhName) nameHashesA)
  assertBool "expected generated protocol sibling name" ("BazProtoD_BarProto" `elem` namesA)
  assertBool "expected generated extension name" ("BarProtoD_Widget" `elem` namesA)
  let idxName n = A.Name NoLoc n
  extensionsForWidget <- InterfaceFiles.readExtensionsByClass tyA (idxName "Widget")
  extensionsForBarProto <- InterfaceFiles.readExtensionsByProtocol tyA (idxName "BarProto")
  extensionsForFooProto <- InterfaceFiles.readExtensionsByProtocol tyA (idxName "FooProto")
  assertEqual "exact extensions by class" ["BarProtoD_Widget"] (sort (map prstr extensionsForWidget))
  assertEqual "exact extensions by protocol" ["BarProtoD_Widget"] (sort (map prstr extensionsForBarProto))
  assertEqual "exact extensions by inherited protocol" ["BarProtoD_Widget"] (sort (map prstr extensionsForFooProto))
  (_, nmod, _, _, _, _, _, _, _, _, _, _) <- InterfaceFiles.readFile tyA
  let I.NModule _ iface _ = nmod
      extMatch (n, _) = prstr n == "BarProtoD_Widget"
  case find extMatch iface of
    Just (_, I.NExt _ _ ps _ _ _) -> do
      let protoNames = sort [ prstr (A.tcname p) | (_, p) <- ps ]
      assertEqual "extension protocol mro" (sort ["a.BarProto", "a.BazProto", "a.FooProto"]) protoNames
    _ -> assertFailure "missing extension NameInfo for BarProtoD_Widget"
  (pubDeps, implDeps) <- readTyDeps tyB "runner"
  let expectedPub = sort ["__builtin__.int", "a.BarProto", "a.Widget", "a.uses_class"]
      expectedImpl = sort ["a.BarProto", "a.Widget", "a.uses_class"]
  assertEqual "runner pub deps" expectedPub pubDeps
  assertEqual "runner impl deps" expectedImpl implDeps

p29_protocol_impl_rebuild :: TestTree
p29_protocol_impl_rebuild = testCase "29-protocol impl change triggers back jobs and new binary" $ do
  let proj = casesProjDir
      src = casesSrcDir
      modA = modLabel proj "a"
      modB = modLabel proj "b"
      modC = modLabel proj "c"
  ensureCasesProject
  writeFileUtf8 (src </> "a.act") $ T.unlines
    [ "protocol FooProto:"
    , "    foo : () -> int"
    , ""
    , "protocol BazProto:"
    , "    baz : () -> int"
    , ""
    , "protocol BarProto (FooProto, BazProto):"
    , "    bar : () -> int"
    , ""
    , "class Widget:"
    , "    def __init__(self):"
    , "        pass"
    , ""
    , "extension Widget(BarProto):"
    , "    def foo(self) -> int:"
    , "        return 1"
    , "    def baz(self) -> int:"
    , "        return 2"
    , "    def bar(self) -> int:"
    , "        return 3"
    , ""
    , "def value() -> int:"
    , "    return Widget().bar()"
    ]
  writeFileUtf8 (src </> "b.act") $ T.unlines
    [ "import a"
    , ""
    , "def calc() -> int:"
    , "    return a.value()"
    ]
  writeFileUtf8 (src </> "c.act") $ T.unlines
    [ "import b"
    , ""
    , "actor main(env: Env):"
    , "    print(b.calc())"
    , "    env.exit(0)"
    ]
  _ <- buildOutIn proj
  out1 <- runBinaryIn proj "c"
  out1 @?= "3\n"
  writeFileUtf8 (src </> "a.act") $ T.unlines
    [ "protocol FooProto:"
    , "    foo : () -> int"
    , ""
    , "protocol BazProto:"
    , "    baz : () -> int"
    , ""
    , "protocol BarProto (FooProto, BazProto):"
    , "    bar : () -> int"
    , ""
    , "class Widget:"
    , "    def __init__(self):"
    , "        pass"
    , ""
    , "extension Widget(BarProto):"
    , "    def foo(self) -> int:"
    , "        return 1"
    , "    def baz(self) -> int:"
    , "        return 5"
    , "    def bar(self) -> int:"
    , "        return 6"
    , ""
    , "def value() -> int:"
    , "    return Widget().bar()"
    ]
  out2 <- buildOutIn proj
  assertBool "expected a.act to type check" (typechecked out2 modA)
  assertBool "did not expect b.act to type check" (not (typechecked out2 modB))
  assertBool "did not expect c.act to type check" (not (typechecked out2 modC))
  assertBool "expected b.act to compile codegen" (compiled out2 modB)
  assertBool "expected c.act to compile codegen" (compiled out2 modC)
  out2Run <- runBinaryIn proj "c"
  out2Run @?= "6\n"

p30_only_build :: TestTree
p30_only_build = testCase "30-only-build skips front passes" $ do
  let proj = casesProjDir
      src = casesSrcDir
      modA = modLabel proj "a"
      modB = modLabel proj "b"
      modC = modLabel proj "c"
  ensureCasesProject
  writeFileUtf8 (src </> "a.act") "aaa = 1\n"
  writeFileUtf8 (src </> "b.act") $ T.unlines
    [ "import a"
    , ""
    , "def bar() -> int:"
    , "    return a.aaa"
    ]
  writeFileUtf8 (src </> "c.act") $ T.unlines
    [ "import b"
    , ""
    , "actor main(env: Env):"
    , "    b.bar()"
    , "    env.exit(0)"
    ]
  _ <- buildOutIn proj
  writeFileUtf8 (src </> "a.act") "aaa = 2\n"
  res2@(ec2, out2) <- runActonIn proj ["build", "--color", "never", "--verbose", "--only-build"]
  assertExitSuccess "only-build" res2
  assertBool "did not expect a.act to type check" (not (typechecked out2 modA))
  assertBool "did not expect b.act to type check" (not (typechecked out2 modB))
  assertBool "did not expect c.act to type check" (not (typechecked out2 modC))
  assertBool "did not expect b.act to compile codegen" (not (compiled out2 modB))
  assertBool "did not expect c.act to compile codegen" (not (compiled out2 modC))

  writeFileUtf8 (src </> "b.act") $ T.unlines
    [ "import a"
    , ""
    , "def bar() -> int:"
    , "    return a.aaa"
    , ")"
    ]
  res3@(ec3, out3) <- runActonIn proj ["build", "--color", "never", "--verbose", "--only-build"]
  assertExitSuccess "only-build with invalid body" res3
  assertBool "did not expect invalid b.act to type check in only-build" (not (typechecked out3 modB))
  assertBool "did not expect c.act to type check after invalid b.act in only-build" (not (typechecked out3 modC))
  assertBool "did not expect parse diagnostics in only-build" (not ("Syntax error" `T.isInfixOf` out3))

p31_always_build :: TestTree
p31_always_build = testCase "31-always-build forces front passes" $ do
  let proj = casesProjDir
      src = casesSrcDir
      modA = modLabel proj "a"
      modB = modLabel proj "b"
      modC = modLabel proj "c"
  ensureCasesProject
  writeFileUtf8 (src </> "a.act") "aaa = 1\n"
  writeFileUtf8 (src </> "b.act") $ T.unlines
    [ "import a"
    , ""
    , "def bar() -> int:"
    , "    return a.aaa"
    ]
  writeFileUtf8 (src </> "c.act") $ T.unlines
    [ "import b"
    , ""
    , "actor main(env: Env):"
    , "    b.bar()"
    , "    env.exit(0)"
    ]
  _ <- buildOutIn proj
  out <- buildOutInArgs proj ["--always-build"]
  assertBool "expected a.act to type check" (typechecked out modA)
  assertBool "expected b.act to type check" (typechecked out modB)
  assertBool "expected c.act to type check" (typechecked out modC)

p32_project_alt_output :: TestTree
p32_project_alt_output = testCase "32-project --types emits output" $ do
  let proj = casesProjDir
      src = casesSrcDir
  ensureCasesProject
  writeFileUtf8 (src </> "a.act") "aaa = 1\n"
  writeFileUtf8 (src </> "c.act") $ T.unlines
    [ "import a"
    , ""
    , "actor main(env: Env):"
    , "    print(a.aaa)"
    , "    env.exit(0)"
    ]
  _ <- buildOutIn proj
  res@(ec, out) <- runActonIn proj ["build", "--color", "never", "--types"]
  assertExitSuccess "project types" res
  assertBool "expected types dump" (T.isInfixOf "== types:" out)

p33_comprehensive_hashes :: TestTree
p33_comprehensive_hashes = testCase "33-comprehensive hash propagation" $ do
  let proj = casesProjDir
      src = casesSrcDir
      modA = modLabel proj "a"
      modB = modLabel proj "b"
      modC = modLabel proj "c"
      tyA = proj </> "out" </> "types" </> "a.tydb"
      tyB = proj </> "out" </> "types" </> "b.tydb"
  ensureCasesProjectWithDeps [("libfoo", "deps/libfoo")]
  depDir <- ensureDepProject proj "libfoo"
  let depSrc = depDir </> "src" </> "lib.act"
      libfooSource depConst addBonus =
        let bonusLines =
              if addBonus
                then [ "    bonus : () -> int"
                     , "    def bonus(self) -> int:"
                     , "        return self.mega() + 1"
                     ]
                else []
        in T.unlines $ concat
            [ [ "protocol BaseProto:"
              , "    base : () -> int"
              , "    def base(self) -> int:"
              , "        return 1"
              , ""
              , "protocol ExtraProto:"
              , "    extra : () -> int"
              , "    def extra(self) -> int:"
              , "        return 2"
              , ""
              , "protocol MegaProto (BaseProto, ExtraProto):"
              , "    mega : () -> int"
              , "    def mega(self) -> int:"
              , "        return self.base() + self.extra()"
              ]
            , bonusLines
            , [ ""
              , "class DepClass:"
              , "    def __init__(self, v: int):"
              , "        self.v = v"
              , "    def base(self) -> int:"
              , "        return self.v"
              , "    def extra(self) -> int:"
              , "        return self.v + 1"
              , ""
              , "extension DepClass(MegaProto):"
              , "    def mega(self) -> int:"
              , "        return self.base() + self.extra()"
              , ""
              , "dep_const = " <> T.pack (show depConst)
              , ""
              , "def dep_value() -> int:"
              , "    return DepClass(dep_const).mega()"
              ]
            ]
  writeFileUtf8 depSrc (libfooSource 5 False)
  writeFileUtf8 (src </> "a.act") $ T.unlines
    [ "import libfoo"
    , ""
    , "const = 7"
    , "x, y = 1, 2"
    , ""
    , "def add(n: int) -> int:"
    , "    return n + const"
    , ""
    , "class Local:"
    , "    def __init__(self, v: int):"
    , "        self.v = v"
    , "    def get(self) -> int:"
    , "        return self.v"
    , ""
    , "actor Worker(v: int):"
    , "    pass"
    , ""
    , "def dep_call() -> int:"
    , "    return libfoo.dep_value()"
    ]
  writeFileUtf8 (src </> "b.act") $ T.unlines
    [ "import a"
    , "import libfoo"
    , ""
    , "def use_types(x: a.Local, w: a.Worker) -> int:"
    , "    return a.add(x.get())"
    , ""
    , "def use_values() -> int:"
    , "    return a.add(libfoo.dep_const) + a.dep_call()"
    , ""
    , "def use_method(x: a.Local) -> int:"
    , "    return x.get()"
    , ""
    , "def use_actor(w: a.Worker) -> int:"
    , "    return 0"
    ]
  writeFileUtf8 (src </> "c.act") $ T.unlines
    [ "import a"
    , "import b"
    , ""
    , "actor main(env: Env):"
    , "    l = a.Local(3)"
    , "    print(b.use_values() + b.use_method(l))"
    , "    env.exit(0)"
    ]
  _ <- buildOutIn proj
  let tyDep = depDir </> "out" </> "types" </> "libfoo.tydb"
  depNameHashes <- readTyNameHashes tyDep
  let depNames = sort (map (prstr . InterfaceFiles.nhName) depNameHashes)
  assertBool "expected derived proto name" ("ExtraProtoD_MegaProto" `elem` depNames)
  assertBool "expected extension name" ("MegaProtoD_DepClass" `elem` depNames)
  case find (\nh -> prstr (InterfaceFiles.nhName nh) == "ExtraProtoD_MegaProto") depNameHashes of
    Nothing -> assertFailure "missing ExtraProtoD_MegaProto in libfoo"
    Just nh ->
      assertBool "expected empty pub hash for derived proto" (B.null (InterfaceFiles.nhPubHash nh))
  (pubTypes, implTypes) <- readTyDeps tyB "use_types"
  assertDepsContain "use_types pub deps" ["a.Local", "a.Worker", "a.add"] pubTypes
  assertDepsContain "use_types impl deps" ["a.add"] implTypes
  (pubVals, implVals) <- readTyDeps tyB "use_values"
  assertDepsContain "use_values pub deps" ["a.add", "a.dep_call", "libfoo.dep_const"] pubVals
  assertDepsContain "use_values impl deps" ["a.add", "a.dep_call", "libfoo.dep_const"] implVals
  (pubActor, _) <- readTyDeps tyB "use_actor"
  assertDepsContain "use_actor pub deps" ["a.Worker"] pubActor
  (_, implDepCall) <- readTyDeps tyA "dep_call"
  assertDepsContain "dep_call impl deps" ["libfoo.dep_value"] implDepCall
  out1 <- runBinaryIn proj "c"
  out1 @?= "26\n"
  writeFileUtf8 depSrc (libfooSource 5 True)
  out2 <- buildOutIn proj
  assertBool "did not expect a.act to type check" (not (typechecked out2 modA))
  assertBool "did not expect b.act to type check" (not (typechecked out2 modB))
  assertBool "did not expect c.act to type check" (not (typechecked out2 modC))
  out2Run <- runBinaryIn proj "c"
  out2Run @?= "26\n"
  writeFileUtf8 depSrc (libfooSource 8 True)
  out3 <- buildOutIn proj
  assertBool "did not expect a.act to type check" (not (typechecked out3 modA))
  assertBool "did not expect b.act to type check" (not (typechecked out3 modB))
  assertBool "did not expect c.act to type check" (not (typechecked out3 modC))
  assertBool "expected a.act to compile codegen" (compiled out3 modA)
  assertBool "expected b.act to compile codegen" (compiled out3 modB)
  assertBool "expected c.act to compile codegen" (compiled out3 modC)
  out3Run <- runBinaryIn proj "c"
  out3Run @?= "35\n"

p34_removed_import_module :: TestTree
p34_removed_import_module = testCase "34-removed imported module fails before Zig" $ do
  let proj = casesProjDir
      src = casesSrcDir
      outA = proj </> "out" </> "types" </> "a"
  ensureCasesProject
  writeFileUtf8 (src </> "a.act") "aaa = 1\n"
  writeFileUtf8 (src </> "b.act") $ T.unlines
    [ "import a"
    , ""
    , "def bar() -> int:"
    , "    return a.aaa"
    ]
  writeFileUtf8 (src </> "c.act") $ T.unlines
    [ "import b"
    , ""
    , "actor main(env: Env):"
    , "    print(b.bar())"
    , "    env.exit(0)"
    ]
  _ <- buildOutIn proj
  removeFile (src </> "a.act")
  res@(ec, out) <- runActonIn proj ["build", "--color", "never", "--verbose"]
  assertExitFailure "build after deleting imported module" 1 res
  mapM_ (\ext -> do
    exists <- artifactExists (outA ++ ext)
    assertBool ("did not expect stale generated " ++ outA ++ ext) (not exists))
    [".tydb", ".c", ".h"]
  assertBool "expected missing import diagnostic"
    (T.isInfixOf "Type interface file not found or unreadable for a" out)
  assertBool "did not expect Zig build to run" (not (T.isInfixOf "zigCmd:" out))

p35_changed_path_keeps_unaffected_provider :: TestTree
p35_changed_path_keeps_unaffected_provider =
  testCase "35-changed-path incremental keeps unchanged providers" $ do
    let proj = casesProjDir
        src = casesSrcDir
        actA = src </> "a.act"
        actB = src </> "b.act"
        actC = src </> "c.act"
        actMain = src </> "main.act"
        gopts = C.GlobalOptions
          { C.color = C.Never
          , C.quiet = True
          , C.noProgress = False
          , C.timing = False
          , C.tty = False
          , C.verbose = False
          , C.verboseZig = False
          , C.jobs = 1
          }
        opts = Compile.defaultCompileOptions { C.only_build = True }
    ensureCasesProject
    writeFileUtf8 actA "aaa = 1\n"
    writeFileUtf8 actC "ccc = 10\n"
    writeFileUtf8 actB $ T.unlines
      [ "import a"
      , "import c"
      , ""
      , "def bar() -> int:"
      , "    return a.aaa + c.ccc"
      ]
    writeFileUtf8 actMain $ T.unlines
      [ "import b"
      , ""
      , "actor main(env: Env):"
      , "    print(b.bar())"
      , "    env.exit(0)"
      ]
    _ <- buildOutIn proj
    writeFileUtf8 actA "aaa = 2\n"
    actAAbs <- canonicalizePath actA
    actMainAbs <- canonicalizePath actMain
    sched <- Compile.newCompileScheduler gopts 1
    plan <- Compile.prepareCompilePlan
      Source.diskSourceProvider
      gopts
      sched
      opts
      [actMainAbs]
      False
      (Just [actAAbs])
    bTask <- case find (\t -> Compile.name (Compile.gtTask t) == A.modName ["b"]) (Compile.cpNeededTasks plan) of
      Just t -> pure t
      Nothing -> assertFailure "expected b in changed-path compile plan" >> fail "missing b task"
    let bProviders = Compile.gtImportProviders bTask
    assertBool "expected provider for changed import a" (M.member (A.modName ["a"]) bProviders)
    assertBool "expected provider for unchanged import c" (M.member (A.modName ["c"]) bProviders)

p35_dbp_changed_path_includes_provider :: TestTree
p35_dbp_changed_path_includes_provider =
  testCase "35-dbp changed-path incremental includes dbp consumers" $ do
    let proj = casesProjDir
        src = casesSrcDir
        actProvider = src </> "provider.act"
        actA = src </> "a.act"
        actB = src </> "b.act"
        actMain = src </> "main.act"
        gopts = C.GlobalOptions
          { C.color = C.Never
          , C.quiet = True
          , C.noProgress = False
          , C.timing = False
          , C.tty = False
          , C.verbose = False
          , C.verboseZig = False
          , C.jobs = 1
          }
        opts = Compile.defaultCompileOptions
          { C.dbp = ["provider"]
          , C.skip_build = True
          }
    ensureCasesProject
    writeFileUtf8 actProvider $ T.unlines
      [ "def used() -> int:"
      , "    return 1"
      , ""
      , "def other() -> int:"
      , "    return 2"
      ]
    writeFileUtf8 actA $ T.unlines
      [ "import provider"
      , ""
      , "def fa() -> int:"
      , "    return provider.used()"
      ]
    writeFileUtf8 actB $ T.unlines
      [ "import provider"
      , ""
      , "def fb() -> int:"
      , "    return provider.used()"
      ]
    writeFileUtf8 actMain $ T.unlines
      [ "import a"
      , "import b"
      , ""
      , "actor main(env: Env):"
      , "    print(a.fa() + b.fb())"
      , "    env.exit(0)"
      ]
    _ <- buildOutInArgs proj ["--skip-build", "--dbp", "provider"]
    writeFileUtf8 actA $ T.unlines
      [ "import provider"
      , ""
      , "def fa() -> int:"
      , "    return provider.other()"
      ]
    actAAbs <- canonicalizePath actA
    actMainAbs <- canonicalizePath actMain
    sched <- Compile.newCompileScheduler gopts 1
    plan <- Compile.prepareCompilePlan
      Source.diskSourceProvider
      gopts
      sched
      opts
      [actMainAbs]
      False
      (Just [actAAbs])
    let selectedMods = [ Compile.name (Compile.gtTask t) | t <- Compile.cpNeededTasks plan ]
    assertBool "expected changed-path plan to include DBP provider"
      (A.modName ["provider"] `elem` selectedMods)
    assertBool "expected unchanged sibling consumer to register DBP interest"
      (A.modName ["b"] `elem` selectedMods)

p36_removed_dep_name_triggers_front_refresh :: TestTree
p36_removed_dep_name_triggers_front_refresh =
  testCase "36-removed dep name forces front refresh" $ do
    let proj = casesProjDir
        src = casesSrcDir
    ensureCasesProjectWithDeps [("libfoo", "deps/libfoo")]
    depDir <- ensureDepProject proj "libfoo"
    let depSrc = depDir </> "src" </> "lib.act"
    writeFileUtf8 depSrc $ T.unlines
      [ "def foo() -> int:"
      , "    return 1"
      ]
    writeFileUtf8 (src </> "main.act") $ T.unlines
      [ "import libfoo"
      , ""
      , "actor main(env: Env):"
      , "    print(libfoo.foo())"
      , "    env.exit(0)"
      ]
    res1 <- runActonIn proj ["build", "--color", "never", "--skip-build"]
    assertExitSuccess "initial build" res1
    writeFileUtf8 depSrc $ T.unlines
      [ "def bar() -> int:"
      , "    return 2"
      ]
    res2@(_ec2, out2) <- runActonIn proj ["build", "--color", "never", "--verbose", "--skip-build"]
    assertExitFailure "rebuild after removing imported dep name" 1 res2
    assertBool "expected stale-cache log for missing dep hash"
      (T.isInfixOf "missing dep hashes in" out2 && T.isInfixOf "libfoo.foo" out2)
    assertBool "did not expect internal hash-missing diagnostic"
      (not (T.isInfixOf "Hash info missing for libfoo.foo" out2))

p37_impl_refresh_missing_dep_hashes_reruns_front :: TestTree
p37_impl_refresh_missing_dep_hashes_reruns_front =
  testCase "37-impl refresh missing dep hashes reruns front passes" $ do
    let proj = casesProjDir
        src = casesSrcDir
        modMain = modLabel proj "main"
    ensureCasesProjectWithDeps [("libfoo", "deps/libfoo")]
    depDir <- ensureDepProject proj "libfoo"
    let depSrc = depDir </> "src" </> "lib.act"
        tyMain = proj </> "out" </> "types" </> "main.tydb"
    writeFileUtf8 depSrc $ T.unlines
      [ "def foo() -> int:"
      , "    return 1"
      , ""
      , "def bar() -> int:"
      , "    return 2"
      ]
    writeFileUtf8 (src </> "main.act") $ T.unlines
      [ "import libfoo"
      , ""
      , "def value() -> int:"
      , "    return libfoo.foo() + libfoo.bar()"
      , ""
      , "actor main(env: Env):"
      , "    print(value())"
      , "    env.exit(0)"
      ]
    res1 <- runActonIn proj ["build", "--color", "never", "--skip-build"]
    assertExitSuccess "initial build" res1
    let mainV2 = T.unlines
          [ "import libfoo"
          , ""
          , "def value() -> int:"
          , "    return libfoo.foo()"
          , ""
          , "actor main(env: Env):"
          , "    print(value())"
          , "    env.exit(0)"
          ]
    writeFileUtf8 (src </> "main.act") mainV2
    rewriteTySrcHashAndNameHashes tyMain (SHA256.hash (TE.encodeUtf8 mainV2)) (dropTyDepByLabel "libfoo.bar")
    writeFileUtf8 depSrc $ T.unlines
      [ "def foo() -> int:"
      , "    return 10"
      ]
    res2@(_ec2, out2) <- runActonIn proj ["build", "--color", "never", "--verbose", "--skip-build"]
    assertExitSuccess "rebuild after impl refresh dep hash miss" res2
    assertBool ("expected impl-refresh fallback log\n" ++ T.unpack out2)
      (T.isInfixOf "impl refresh encountered unresolved dep hashes; rerunning front passes" out2)
    assertBool ("did not expect internal hash-missing diagnostic\n" ++ T.unpack out2)
      (not (T.isInfixOf "Hash info missing for libfoo.bar" out2))
    assertBool ("did not expect internal NoItem failure\n" ++ T.unpack out2)
      (not (T.isInfixOf "NoItem" out2))
    assertBool ("expected main.act to type check after fallback\n" ++ T.unpack out2) (typechecked out2 modMain)

p38_project_lock_blocks_build :: TestTree
p38_project_lock_blocks_build =
  testCase "38-project lock blocks concurrent build" $ do
    let proj = casesProjDir
        src = casesSrcDir
    ensureCasesProject
    writeFileUtf8 (src </> "main.act") $ T.unlines
      [ "actor main(env: Env):"
      , "    env.exit(0)"
      ]
    actonExe <- actonPath
    res <- Compile.withProjectLock proj $ do
      (_, mOut, mErr, ph) <- createProcess
        (proc actonExe ["build", "--color", "never", "--jobs", "1"])
          { cwd = Just proj
          , std_out = CreatePipe
          , std_err = CreatePipe
          }
      let outH = requirePipe "stdout" mOut
          errH = requirePipe "stderr" mErr
      (do
          threadDelay 500000
          mExit <- getProcessExitCode ph
          case mExit of
            Nothing -> pure ()
            Just ec -> assertFailure ("build should have blocked on .acton.lock, exited early with " ++ show ec)
          pure (ph, outH, errH))
        `E.onException` do
          terminateProcess ph
          _ <- waitForProcess ph
          pure ()
    let (ph, outH, errH) = res
    ec <- waitForProcess ph
    out <- T.hGetContents outH
    err <- T.hGetContents errH
    assertEqual "build should succeed after lock is released" ExitSuccess ec
    let combined = out <> err
    assertBool ("expected wait message while build was blocked\n" ++ T.unpack combined)
      (T.isInfixOf "Waiting for compiler lock in" combined)
  where
    requirePipe :: String -> Maybe Handle -> Handle
    requirePipe label =
      maybe (error ("missing " ++ label ++ " pipe for lock wait test")) id

p39_background_lock_does_not_block_build :: TestTree
p39_background_lock_does_not_block_build =
  testCase "39-background compiler lock does not block build" $ do
    let proj = casesProjDir
        src = casesSrcDir
    ensureCasesProject
    writeFileUtf8 (src </> "main.act") $ T.unlines
      [ "actor main(env: Env):"
      , "    env.exit(0)"
      ]
    withBackgroundCompilerLockHeld proj $ do
      res@(_ec, out) <- runActonIn proj ["build", "--color", "never", "--verbose"]
      assertExitSuccess "build should run under .acton.compile.lock" res
      assertBool ("expected build to rerun front passes while background lock is held\n" ++ T.unpack out)
        (typechecked out "main")

p40_background_lock_blocks_watch :: TestTree
p40_background_lock_blocks_watch =
  testCase "40-background compiler lock blocks watch" $ do
    let proj = casesProjDir
        src = casesSrcDir
    ensureCasesProject
    writeFileUtf8 (src </> "main.act") $ T.unlines
      [ "actor main(env: Env):"
      , "    env.exit(0)"
      ]
    withBackgroundCompilerLockHeld proj $ do
      actonExe <- actonPath
      (_, _, _, ph) <- createProcess (proc actonExe ["build", "--watch", "--color", "never", "--jobs", "1"]) { cwd = Just proj }
      (do
          mExit <- timeout 5000000 (waitForProcess ph)
          case mExit of
            Nothing -> do
              terminateProcess ph
              _ <- waitForProcess ph
              assertFailure "watch should fail within 5s when .acton.compile.lock is already held"
            Just ExitSuccess ->
              assertFailure "watch should not succeed when .acton.compile.lock is already held"
            Just (ExitFailure _) ->
              pure ())
        `E.onException` do
          terminateProcess ph
          _ <- waitForProcess ph
          pure ()

p41_stale_header_missing_import_reparses_source :: TestTree
p41_stale_header_missing_import_reparses_source =
  testCase "41-stale header missing import reparses source" $ do
    let proj = casesProjDir
        src = casesSrcDir
        actA = src </> "a.act"
        actB = src </> "b.act"
        tyB = proj </> "out" </> "types" </> "b.tydb"
        outA = proj </> "out" </> "types" </> "a"
        modB = modLabel proj "b"
    ensureCasesProject
    writeFileUtf8 actA "aaa = 1\n"
    writeFileUtf8 actB $ T.unlines
      [ "import a"
      , ""
      , "def bar() -> int:"
      , "    return a.aaa"
      ]
    writeFileUtf8 (src </> "main.act") $ T.unlines
      [ "import b"
      , ""
      , "actor main(env: Env):"
      , "    print(b.bar())"
      , "    env.exit(0)"
      ]
    _ <- buildOutIn proj
    writeFileUtf8 actB $ T.unlines
      [ "def bar() -> int:"
      , "    return 7"
      ]
    tyTime <- InterfaceFiles.interfaceModifiedTime tyB
    setModificationTime actB (addUTCTime (-10) tyTime)
    removeFile actA
    res@(_ec, out) <- runActonIn proj ["build", "--color", "never", "--verbose"]
    assertExitSuccess "build after stale header import removal" res
    mapM_ (\ext -> do
      exists <- artifactExists (outA ++ ext)
      assertBool ("did not expect stale generated " ++ outA ++ ext) (not exists))
      [".tydb", ".c", ".h"]
    assertBool "expected b.act to type check after stale header reparse"
      (typechecked out modB)
    assertBool ("did not expect stale import diagnostic\n" ++ T.unpack out)
      (not (T.isInfixOf "Type interface file not found or unreadable for a" out))

p42_metadata_drift_refreshes_header :: TestTree
p42_metadata_drift_refreshes_header =
  testCase "42-metadata drift refreshes header" $ do
    let proj = casesProjDir
        src = casesSrcDir
        actA = src </> "a.act"
        tyA = proj </> "out" </> "types" </> "a.tydb"
        modA = modLabel proj "a"
    ensureCasesProject
    writeFileUtf8 actA "aaa = 1\n"
    writeFileUtf8 (src </> "main.act") $ T.unlines
      [ "import a"
      , ""
      , "actor main(env: Env):"
      , "    print(a.aaa)"
      , "    env.exit(0)"
      ]
    _ <- buildOutIn proj
    metaBefore <- readTySourceMeta tyA
    threadDelay 1000000
    touch actA
    out <- buildOutIn proj
    metaAfter <- readTySourceMeta tyA
    currentMeta <- sourceFileMetaForPath actA
    assertBool "did not expect a.act to type check after metadata-only drift"
      (not (typechecked out modA))
    assertBool "expected .tydb header to carry source metadata"
      (isJust metaAfter)
    assertBool "expected metadata drift to update cached source metadata"
      (metaBefore /= metaAfter)
    metaAfter @?= Just currentMeta

p43_equal_act_ty_mtime_hashes_source :: TestTree
p43_equal_act_ty_mtime_hashes_source =
  testCase "43-equal act/ty mtimes still hash source" $ do
    let proj = casesProjDir
        src = casesSrcDir
        actA = src </> "a.act"
        actB = src </> "b.act"
        tyB = proj </> "out" </> "types" </> "b.tydb"
        tyBData = tyB </> "data.mdb"
        outA = proj </> "out" </> "types" </> "a"
        modB = modLabel proj "b"
    ensureCasesProject
    writeFileUtf8 actA "aaa = 1\n"
    writeFileUtf8 actB $ T.unlines
      [ "import a"
      , ""
      , "def bar() -> int:"
      , "    return a.aaa"
      ]
    writeFileUtf8 (src </> "main.act") $ T.unlines
      [ "import b"
      , ""
      , "actor main(env: Env):"
      , "    print(b.bar())"
      , "    env.exit(0)"
      ]
    _ <- buildOutIn proj
    writeFileUtf8 actB $ T.unlines
      [ "def bar() -> int:"
      , "    return 7"
      ]
    rewriteTySourceMeta tyB Nothing
    tyStatus <- getFileStatus tyBData
    let tyMTimeNs = floor (toRational (modificationTimeHiRes tyStatus) * 1000000000)
    setFileMTimeNs actB tyMTimeNs
    currentMeta <- sourceFileMetaForPath actB
    rewriteTySourceMeta tyB (Just currentMeta)
    setFileMTimeNs tyBData tyMTimeNs
    metaAfter <- readTySourceMeta tyB
    metaAfter @?= Just currentMeta
    removeFile actA
    res@(_ec, out) <- runActonIn proj ["build", "--color", "never", "--verbose"]
    assertExitSuccess "build after equal act/ty mtime stale header" res
    mapM_ (\ext -> do
      exists <- artifactExists (outA ++ ext)
      assertBool ("did not expect stale generated " ++ outA ++ ext) (not exists))
      [".tydb", ".c", ".h"]
    assertBool "expected b.act to type check after equal-mtime hash check"
      (typechecked out modB)
    assertBool ("did not expect stale import diagnostic\n" ++ T.unpack out)
      (not (T.isInfixOf "Type interface file not found or unreadable for a" out))

p44_provider_import_rename_reruns_dependent_front :: TestTree
p44_provider_import_rename_reruns_dependent_front =
  testCase "44-provider import rename reruns dependent front passes" $ do
    let proj = casesProjDir
        src = casesSrcDir
        actMain = src </> "main.act"
        actBase = src </> "base.act"
        oldPkgDir = src </> "oldpkg"
        newPkgDir = src </> "newpkg"
        modMain = modLabel proj "main"
        modBase = modLabel proj "base"
    ensureCasesProject
    createDirectoryIfMissing True oldPkgDir
    writeFileUtf8 actMain $ T.unlines
      [ "import base"
      , ""
      , "class Derived(base.Base):"
      , "    pass"
      , ""
      , "actor main(env: Env):"
      , "    env.exit(0)"
      ]
    writeFileUtf8 actBase $ T.unlines
      [ "import oldpkg.ttt as ttt"
      , ""
      , "class Base(ttt.TransformFunction):"
      , "    pass"
      ]
    writeFileUtf8 (src </> "oldpkg.act") ""
    writeFileUtf8 (oldPkgDir </> "ttt.act") $ T.unlines
      [ "class TransformFunction(object):"
      , "    pass"
      ]
    res1 <- runActonIn proj ["build", "--color", "never", "--skip-build"]
    assertExitSuccess "initial build before provider import rename" res1
    createDirectoryIfMissing True newPkgDir
    writeFileUtf8 actBase $ T.unlines
      [ "import newpkg.ttt as ttt"
      , ""
      , "class Base(ttt.TransformFunction):"
      , "    pass"
      ]
    writeFileUtf8 (src </> "newpkg.act") ""
    writeFileUtf8 (newPkgDir </> "ttt.act") $ T.unlines
      [ "class TransformFunction(object):"
      , "    pass"
      ]
    removeFile (src </> "oldpkg.act")
    removeFile (oldPkgDir </> "ttt.act")
    removeDirIfExists oldPkgDir
    res2@(_ec2, out2) <- runActonIn proj ["build", "--color", "never", "--verbose", "--skip-build"]
    assertExitSuccess "rebuild after provider import rename" res2
    assertBool ("expected base.act to type check after provider import rename\n" ++ T.unpack out2)
      (typechecked out2 modBase)
    assertBool ("expected main.act to type check after provider import rename\n" ++ T.unpack out2)
      (typechecked out2 modMain)
    assertBool ("did not expect stale transitive import diagnostic\n" ++ T.unpack out2)
      (not (T.isInfixOf "Type interface file not found or unreadable for oldpkg.ttt" out2))
    assertBool ("expected stale-cache log for missing transitive dep hash\n" ++ T.unpack out2)
      (T.isInfixOf "missing dep hashes in" out2 && T.isInfixOf "oldpkg.ttt.TransformFunction" out2)

p45_tydb_records_local_name_dependencies :: TestTree
p45_tydb_records_local_name_dependencies =
  testCase "45-tydb records local name dependencies" $ do
    let proj = casesProjDir
        src = casesSrcDir
        tyA = proj </> "out" </> "types" </> "a.tydb"
    ensureCasesProject
    writeFileUtf8 (src </> "a.act") $ T.unlines
      [ "def base() -> int:"
      , "    return 1"
      , ""
      , "def mid() -> int:"
      , "    return base()"
      , ""
      , "def top() -> int:"
      , "    return mid()"
      , ""
      , "class Box:"
      , "    def __init__(self, v: int):"
      , "        self.v = v"
      , "    def get(self) -> int:"
      , "        return self.v"
      , ""
      , "def make_box() -> Box:"
      , "    return Box(top())"
      , ""
      , "class Container:"
      , "    def __init__(self):"
      , "        pass"
      , ""
      , "def local_container_name(x: object) -> bool:"
      , "    return isinstance(x, Container)"
      ]
    writeFileUtf8 (src </> "main.act") $ T.unlines
      [ "import a"
      , ""
      , "actor main(env: Env):"
      , "    print(a.make_box().get())"
      , "    env.exit(0)"
      ]
    _ <- buildOutIn proj
    (topPub, topImpl) <- readTyLocalDeps tyA "top"
    assertDepsContain "top pub local deps" ["mid"] topPub
    assertDepsContain "top impl local deps" ["mid"] topImpl
    (midPub, midImpl) <- readTyLocalDeps tyA "mid"
    assertDepsContain "mid pub local deps" ["base"] midPub
    assertDepsContain "mid impl local deps" ["base"] midImpl
    (boxPub, boxImpl) <- readTyLocalDeps tyA "make_box"
    assertDepsContain "make_box pub local deps" ["Box", "top"] boxPub
    assertDepsContain "make_box impl local deps" ["Box", "top"] boxImpl
    (containerPub, containerImpl) <- readTyLocalDeps tyA "local_container_name"
    assertDepsContain "local_container_name pub local deps" ["Container"] containerPub
    assertDepsContain "local_container_name impl local deps" ["Container"] containerImpl

p46_huge_module_skips_doc_output :: TestTree
p46_huge_module_skips_doc_output =
  testCase "46-huge module skips doc output" $ do
    let opts = Compile.defaultCompileOptions
        docDir = casesProjDir </> "doc-index"
    assertBool "doc output should run at the threshold"
      (Compile.shouldGenerateDocOutput opts False Compile.docNameCountThreshold)
    assertBool "doc output should skip above the threshold"
      (not (Compile.shouldGenerateDocOutput opts False (Compile.docNameCountThreshold + 1)))
    assertBool "skip-build should skip doc output"
      (not (Compile.shouldGenerateDocOutput opts{ C.skip_build = True } False 1))
    assertBool "temporary builds should skip doc output"
      (not (Compile.shouldGenerateDocOutput opts True 1))
    removeDirIfExists docDir
    createDirectoryIfMissing True docDir
    DocP.generateDocIndex docDir [(A.modName ["huge"], Just "Huge module", False)]
    index <- T.readFile (docDir </> "index.html")
    assertBool "skipped module should remain visible"
      ("huge" `T.isInfixOf` index)
    assertBool "skipped module should not link to missing page"
      (not ("href=\"huge.html\"" `T.isInfixOf` index))

-- Main -----------------------------------------------------------------------

-- | Tasty entry point for incremental tests.
main :: IO ()
main = defaultMain $ localOption (NumThreads 1) $ testGroup "incremental"
  [ sequentialTestGroup "incremental-project" AllSucceed
      [ p01_init
      , p02_initial_build
      , p03_up_to_date
      , p04_touch_no_rebuild
      , p05_run_123
      , p06_change_a_impl
      , p06_impl_change_fresh
      , p07_run_124
      , p08_change_b_impl
      , p09_run_124
      , p10_change_a_iface
      , p11_change_b_doc
      , p12_codegen_stale
  ]
  , sequentialTestGroup "incremental-file" AllSucceed
      [ f01_init
      , f02_initial_build
      , f03_up_to_date
      , f04_touch_no_rebuild
      , f05_run_123
      , f06_change_a_impl
      , f07_run_124
      , f08_change_b_impl
      , f09_run_124
      , f10_alt_output
      ]
  , sequentialTestGroup "incremental-project-cases" AllSucceed
      [ p14_partial_rebuild
      , p15_rebuild_import
      , p16_dep_api_change
      , p17_dep_impl_change
      , p18_type_only_deps
      , p19_unused_import
      , p20_add_remove_names
      , p21_recursive_group
      , p22_multibind
      , p23_codegen_mismatch
      , p24_codegen_equal_hash
      , p25_whitespace_change
      , p26_corrupt_ty_header
      , p26_ty_version_mismatch
      , p27_overlay_source_provider
      , p28_protocol_extension_deps
      , p29_protocol_impl_rebuild
      , p30_only_build
      , p31_always_build
      , p32_project_alt_output
      , p33_comprehensive_hashes
      , p34_removed_import_module
      , p35_changed_path_keeps_unaffected_provider
      , p35_dbp_changed_path_includes_provider
      , p36_removed_dep_name_triggers_front_refresh
      , p37_impl_refresh_missing_dep_hashes_reruns_front
      , p38_project_lock_blocks_build
      , p39_background_lock_does_not_block_build
      , p40_background_lock_blocks_watch
      ]
  , sequentialTestGroup "incremental-cache-cases" AllSucceed
      [ p41_stale_header_missing_import_reparses_source
      , p42_metadata_drift_refreshes_header
      , p43_equal_act_ty_mtime_hashes_source
      , p44_provider_import_rename_reruns_dependent_front
      , p45_tydb_records_local_name_dependencies
      , p46_huge_module_skips_doc_output
      ]
  ]
