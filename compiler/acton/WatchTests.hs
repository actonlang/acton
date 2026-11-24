{-# LANGUAGE ScopedTypeVariables #-}
module WatchTests (watchTriggerTests, watchProcessTests, writeExecutable) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, catch, finally, mask, onException)
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.IORef
import qualified Data.Map as M
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..))
import Data.List (intercalate, isInfixOf, isPrefixOf, isSuffixOf, tails)
import Data.Maybe (isJust, mapMaybe)
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp (withSystemTempDirectory)
import System.Info (os)
import System.Posix.Files (createSymbolicLink)
import System.Posix.Signals
import System.Posix.Types (ProcessID)
import System.Process
import qualified System.Timeout as Timeout
import Text.Read (readMaybe)
import qualified System.FSNotify as FS

import Acton.Project (withProjectLock)
import qualified Acton.Fingerprint as Fingerprint
import ProcessUtil (stopProcessGroup)
import Watch
import Test.Tasty
import Test.Tasty.HUnit

-- | Which events start a build, without a process.
watchTriggerTests :: TestTree
watchTriggerTests = testGroup "watch triggers"
  [ testCase "a saved .act names its module" $
      srcWatchTrigger (FS.Modified "/p/src/a.act" t0 FS.IsFile) @?= Just (WatchSave "/p/src/a.act" (Just "/p/src/a.act"))
  , testCase "an added or removed .act asks for a full build once it changed" $ do
      srcWatchTrigger (FS.Added "/p/src/a.act" t0 FS.IsFile) @?= Just (WatchSave "/p/src/a.act" Nothing)
      srcWatchTrigger (FS.Removed "/p/src/a.act" t0 FS.IsFile) @?= Just (WatchSave "/p/src/a.act" Nothing)
  , testCase "a saved, added or removed .ext.c names the module it implements" $
      forM_ [FS.Modified, FS.Added, FS.Removed] $ \event ->
        srcWatchTrigger (event "/p/src/x/native.ext.c" t0 FS.IsFile)
          @?= Just (WatchSave "/p/src/x/native.ext.c" (Just "/p/src/x/native.act"))
  , testCase "another saved file asks for a full build once it changed" $
      srcWatchTrigger (FS.Modified "/p/src/inc/value.h" t0 FS.IsFile) @?= Just (WatchSave "/p/src/inc/value.h" Nothing)
  , testCase "files that editors leave next to the files they save are skipped" $
      forM_ [ "/p/src/.native.ext.c.swp", "/p/src/native.ext.c~", "/p/src/.#native.ext.c", "/p/src/#native.ext.c#"
            , "/p/src/4913", "/p/src/.DS_Store", "/p/src/native.ext.c___jb_tmp___", "/p/src/.#main.act" ] $ \path ->
        forM_ [FS.Added, FS.Modified] $ \event ->
          srcWatchTrigger (event path t0 FS.IsFile) @?= Nothing
  , testCase "--tydb copies are skipped" $ do
      srcWatchTrigger (FS.Added "/p/src/native.tydb/" t0 FS.IsDirectory) @?= Nothing
      srcWatchTrigger (FS.Modified "/p/src/native.tydb/data.mdb" t0 FS.IsFile) @?= Nothing
  , testCase "an added directory asks for a full build, a modified one for nothing" $ do
      srcWatchTrigger (FS.Added "/p/src/sub" t0 FS.IsDirectory) @?= Just WatchFull
      srcWatchTrigger (FS.Modified "/p/src/sub" t0 FS.IsDirectory) @?= Nothing
  , testCase "attribute changes are skipped" $
      srcWatchTrigger (FS.ModifiedAttributes "/p/src/native.ext.c" t0 FS.IsFile) @?= Nothing
  , testCase "a single-file watch looks at the file and its .ext.c" $ do
      fileWatchTrigger "/d/a.act" (FS.Modified "/d/a.act" t0 FS.IsFile) @?= Just (WatchSave "/d/a.act" (Just "/d/a.act"))
      fileWatchTrigger "/d/a.act" (FS.Modified "/d/a.ext.c" t0 FS.IsFile) @?= Just (WatchSave "/d/a.ext.c" (Just "/d/a.act"))
      fileWatchTrigger "/d/a.act" (FS.Modified "/d/a" t0 FS.IsFile) @?= Nothing
      fileWatchTrigger "/d/a.act" (FS.Modified "/d/value.h" t0 FS.IsFile) @?= Nothing
  , testCase "a save builds only when its file changed" $
      withSystemTempDirectory "acton-watch-stamps" $ \dir -> do
        stamps   <- newIORef M.empty
        notified <- newIORef []
        let path   = dir </> "native.ext.c"
            owner  = Just (dir </> "native.act")
            save   = dispatchWatchTrigger (shouldTriggerFile stamps) (\m -> modifyIORef notified (m :)) (WatchSave path owner)
        writeFile (dir </> "native.act") ""
        writeFile path "a"
        save
        save
        writeFile path "bb"
        save
        removeFile path
        save
        (@?= replicate 3 owner) . reverse =<< readIORef notified
  , testCase "a file that has not changed since the watch started builds nothing" $
      withSystemTempDirectory "acton-watch-record" $ \dir -> do
        stamps   <- newIORef M.empty
        notified <- newIORef []
        createDirectoryIfMissing True (dir </> "sub")
        writeFile (dir </> "main.act") "pass"
        writeFile (dir </> "sub" </> "value.h") "#define X 1"
        recordStamps stamps dir
        let save path = dispatchWatchTrigger (shouldTriggerFile stamps) (\m -> modifyIORef notified (m :)) (WatchSave path (Just path))
        save (dir </> "main.act")
        save (dir </> "sub" </> "value.h")
        (@?= []) =<< readIORef notified
        writeFile (dir </> "sub" </> "value.h") "#define X 22"
        save (dir </> "sub" </> "value.h")
        (@?= [Just (dir </> "sub" </> "value.h")]) =<< readIORef notified
  , testCase "a saved .ext.c without its module source asks for a full build" $
      withSystemTempDirectory "acton-watch-orphan" $ \dir -> do
        stamps   <- newIORef M.empty
        notified <- newIORef []
        let path = dir </> "common.ext.c"
        writeFile path "int x;"
        dispatchWatchTrigger (shouldTriggerFile stamps) (\m -> modifyIORef notified (m :))
          (WatchSave path (Just (dir </> "common.act")))
        (@?= [Nothing]) =<< readIORef notified
  ]
  where t0 = UTCTime (fromGregorian 2026 1 1) 0

watchProcessTests :: TestTree
watchProcessTests = testGroup "watch subprocesses"
  [ testCase "Zig inherits open stdin at EOF" $
      withProcessProject $ \acton proj system -> do
        writeExecutable (system </> "zig/zig")
          [ "if ! true 3<&0; then exit 1; fi"
          , "if read -r line; then exit 1; fi"
          , "touch stdin-checked"
          ]
        withActon acton proj ["build", "--syspath", system] $ \ph _ output -> do
          code <- await "build exit" (getProcessExitCode ph)
          logText <- output
          assertEqual logText ExitSuccess code
          assertBool "Zig should receive a valid stdin descriptor at EOF" =<< doesFileExist (proj </> "stdin-checked")
  , testCase "stopping build watch stops Zig descendants" $
      withProcessProject $ \acton proj system -> do
        writeExecutable (system </> "zig/zig") (childProcess True ++ waitingParent)
        withActon acton proj ["build", "--watch", "--syspath", system] $ \ph _ output -> do
          child <- awaitPid proj "child.pid"
          leader <- awaitPid proj "leader.pid"
          terminateProcess ph
          code <- await "build watch exit" (getProcessExitCode ph)
          assertEqual "watch should handle TERM" (ExitFailure 143) code
          assertStopped "Zig leader" leader
          assertStopped "Zig descendant" child
          assertBool "interrupted build should not report success" . not . isInfixOf "Final compilation done" =<< output
  , testGroup "successful Zig exit stops descendants"
      [ testCase pipes $ withProcessProject $ \acton proj system -> do
          writeExecutable (system </> "zig/zig") (childProcess redirected ++ ["exit 0"])
          withActon acton proj ["build", "--syspath", system] $ \ph _ output -> do
            child <- awaitPid proj "child.pid"
            assertStopped "Zig descendant" child
            code <- await "build exit" (getProcessExitCode ph)
            logText <- output
            assertEqual logText ExitSuccess code
      | (pipes, redirected) <- [("redirected pipes", True), ("inherited pipes", False)]
      ]
  , testCase "stopping test watch stops test descendants" $
      withProcessProject $ \acton proj system -> do
        writeExecutable (proj </> "out/bin/.test_main") (childProcess True ++ waitingParent)
        withActon acton proj ["test", "--watch", "--tty", "--no-cache", "--syspath", system] $ \ph _ output -> do
          child <- awaitPid proj "child.pid"
          leader <- awaitPid proj "leader.pid"
          terminateProcess ph
          code <- await "test watch exit" (getProcessExitCode ph)
          assertEqual "watch should handle TERM" (ExitFailure 143) code
          assertStopped "test leader" leader
          assertStopped "test descendant" child
          assertCursorRestored =<< output
  , testGroup "successful test exit stops descendants"
      [ testCase pipes $ withProcessProject $ \acton proj system -> do
          writeExecutable (proj </> "out/bin/.test_main")
            (childProcess redirected ++ [testInfo True, "exit 0"])
          withActon acton proj ["test", "--watch", "--no-cache", "--syspath", system] $ \ph _ output -> do
            child <- awaitPid proj "child.pid"
            assertStopped "test descendant" child
            await "successful test report" $ do
              logText <- output
              return $ if "All 1 tests passed" `isInfixOf` logText then Just () else Nothing
            assertEqual "watch should remain running" Nothing =<< getProcessExitCode ph
      | (pipes, redirected) <- [("redirected pipes", True), ("inherited pipes", False)]
      ]
  , testCase "failed final build skips retained test binary and recovers" $
      withProcessProject $ \acton proj system -> do
        writeFile (proj </> "fail-build") ""
        writeExecutable (system </> "zig/zig")
          [ "touch build-started"
          , "while [ ! -e finish-build ]; do sleep 0.01; done"
          , "if [ -e fail-build ]; then exit 1; fi"
          ]
        writeExecutable (proj </> "out/bin/.test_main") ["touch test-ran", testInfo True]
        withActon acton proj ["test", "--watch", "--no-cache", "--syspath", system] $ \ph _ output -> do
          awaitFile (proj </> "build-started")
          writeFile (proj </> "finish-build") ""
          await "failed build report" $ do
            logText <- output
            return $ if "compilation of generated Zig code failed" `isInfixOf` logText then Just () else Nothing
          -- The lock covers the complete generation, including any test run.
          awaitGeneration proj
          assertBool "failed build must not run the retained binary" . not =<< doesFileExist (proj </> "test-ran")
          assertEqual "watch should survive a failed build" Nothing =<< getProcessExitCode ph
          removeFile (proj </> "fail-build")
          appendFile (proj </> "src/main.act") "\n# Recover from failed build\n"
          awaitReports 1 output
          assertBool "the next successful build should run tests" =<< doesFileExist (proj </> "test-ran")
  , testCase "interrupted test generation retains both edited modules" $
      withProcessProject $ \acton proj system -> do
        writeFile (proj </> "src/other.act") $ unlines
          [ "import testing"
          , "def _test_ready():"
          , "    testing.assertEqual(1, 1)"
          ]
        writeExecutable (proj </> "out/bin/.test_main")
          [ "if [ -e interrupt-main ]; then"
          , "    touch main-started"
          , "    while [ ! -e finish-main ]; do sleep 0.01; done"
          , "fi"
          , "touch main-ran"
          , testInfo True
          ]
        writeExecutable (proj </> "out/bin/.test_other") ["touch other-ran", moduleTestInfo "other" True]
        withActon acton proj ["test", "--watch", "--no-cache", "--syspath", system] $ \ph _ output -> do
          awaitReports 1 output
          awaitGeneration proj
          forM_ ["main-ran", "other-ran"] $ \name -> removeFile (proj </> name)
          writeFile (proj </> "interrupt-main") ""
          appendFile (proj </> "src/main.act") "\n# First edit\n"
          awaitFile (proj </> "main-started")
          removeFile (proj </> "interrupt-main")
          appendFile (proj </> "src/other.act") "\n# Second edit interrupts the first test run\n"
          awaitReports 2 output
          awaitGeneration proj
          forM_ ["main-ran", "other-ran"] $ \name ->
            assertBool (name ++ " should run after the interrupted generation") =<< doesFileExist (proj </> name)
          assertEqual "watch should remain running" Nothing =<< getProcessExitCode ph
  , testCase "build watch rebuilds after a save of an .ext.c or a file it includes" $
      withProcessProject $ \acton proj system -> do
        writeNativeModule proj
        writeExecutable (system </> "zig/zig") ["echo run >> zig-runs"]
        withActon acton proj ["build", "--watch", "--verbose", "--syspath", system] $ \ph _ output -> do
          awaitZigRuns proj 1
          awaitGeneration proj
          appendFile (proj </> "src/native.ext.c") "\n// First edit\n"
          awaitZigRuns proj 2
          awaitGeneration proj
          logText <- output
          -- The first build reports it too.
          assertBool logText (count "Stale native: source changed" logText >= 2)
          appendFile (proj </> "src/native_value.h") "\n// Second edit\n"
          awaitZigRuns proj 3
          assertEqual "watch should remain running" Nothing =<< getProcessExitCode ph
  , testCase "test watch reruns the tests that use a module whose .ext.c is saved" $
      withProcessProject $ \acton proj system -> do
        writeNativeModule proj
        writeFile (proj </> "src/other.act") $ unlines
          [ "import testing"
          , "import native"
          , "def _test_ready():"
          , "    testing.assertEqual(native.answer(), 42)"
          ]
        writeExecutable (proj </> "out/bin/.test_main") ["touch main-ran", testInfo True]
        writeExecutable (proj </> "out/bin/.test_other") ["touch other-ran", moduleTestInfo "other" True]
        withActon acton proj ["test", "--watch", "--no-cache", "--syspath", system] $ \ph _ output -> do
          awaitReports 1 output
          awaitGeneration proj
          forM_ ["main-ran", "other-ran"] $ \name -> removeFile (proj </> name)
          appendFile (proj </> "src/native.ext.c") "\n// An edit of native's C code\n"
          awaitReports 2 output
          awaitGeneration proj
          assertBool "the tests of other, which imports native, should run" =<< doesFileExist (proj </> "other-ran")
          assertBool "the tests of main should not run" . not =<< doesFileExist (proj </> "main-ran")
          assertEqual "watch should remain running" Nothing =<< getProcessExitCode ph
  , testCase "build watch with --tydb settles" $
      withProcessProject $ \acton proj system -> do
        writeExecutable (system </> "zig/zig") ["echo run >> zig-runs"]
        -- --tydb writes a copy of each module's interface next to its
        -- source, which the watch must not take for an edit.
        withActon acton proj ["build", "--watch", "--tydb", "--syspath", system] $ \ph _ _ -> do
          awaitZigRuns proj 1
          awaitGeneration proj
          threadDelay 2000000
          runs <- zigRuns proj
          assertEqual "no build should follow the first" 1 runs
          assertEqual "watch should remain running" Nothing =<< getProcessExitCode ph
  , testCase "build watch reports an embedded file that changed while Zig ran" $
      withProcessProject $ \acton proj system -> do
        createDirectory (proj </> "data")
        writeFile (proj </> "data/text.txt") "one\n"
        writeFile (proj </> "src/page.act") "text = embed(\"data/text.txt\")\n"
        writeExecutable (system </> "zig/zig") ["echo 'another version' > data/text.txt"]
        withActon acton proj ["build", "--watch", "--syspath", system] $ \ph _ output -> do
          await "the report of the changed file" $ do
            logText <- output
            return $ if "data/text.txt changed while the build ran. Run the build again" `isInfixOf` logText then Just () else Nothing
          awaitGeneration proj
          assertBool "the build should not be reported as done" . not . isInfixOf "Final compilation done" =<< output
          assertEqual "watch should survive the changed file" Nothing =<< getProcessExitCode ph
  , testCase "one-shot stress Ctrl-C reports partial results" $
      withProcessProject $ \acton proj system -> do
        writeExecutable (proj </> "out/bin/.test_main")
          [ "trap 'exit 130' INT"
          , "echo '== Running test, iteration: 1'"
          , "echo '== Running test, iteration: 1' >&2"
          , testInfo False
          , "printf 'stdout before interrupt'"
          , "printf 'stderr before interrupt' >&2"
          , "echo $$ > leader.pid"
          , "while :; do sleep 1; done"
          ]
        withActon acton proj ["test", "stress", "--tty", "--show-log", "--syspath", system] $ \ph group output -> do
          _ <- awaitPid proj "leader.pid"
          signalProcessGroup sigINT group
          code <- await "interrupted stress exit" (getProcessExitCode ph)
          logText <- output
          assertEqual logText ExitSuccess code
          assertBool logText ("Stress run interrupted by user; showing partial results collected so far." `isInfixOf` logText)
          assertBool logText ("stdout before interrupt" `isInfixOf` logText && "stderr before interrupt" `isInfixOf` logText)
          assertCursorRestored logText
  , testCase "a final result survives later partial updates and output" $
      withProcessProject $ \acton proj system -> do
        writeExecutable (proj </> "out/bin/.test_main")
          [ testInfo True
          , "echo '{\"test_info\":{\"definition\":{\"module\":\"main\",\"name\":\"_test_ready\"},\"complete\":false,\"success\":false}}' >&2"
          , "printf 'stdout after final result'"
          , "printf 'stderr after final result' >&2"
          ]
        withActon acton proj ["test", "--no-cache", "--show-log", "--syspath", system] $ \ph _ output -> do
          code <- await "test exit" (getProcessExitCode ph)
          logText <- output
          assertEqual logText ExitSuccess code
          assertBool logText ("stdout after final result" `isInfixOf` logText && "stderr after final result" `isInfixOf` logText)
  , testCase "a crashed test retains preamble and unterminated output" $
      withProcessProject $ \acton proj system -> do
        writeExecutable (proj </> "out/bin/.test_main")
          [ "echo 'startup diagnostic' >&2"
          , "echo '== Running test, iteration: 1'"
          , "echo '== Running test, iteration: 1' >&2"
          , "printf 'stdout before crash'"
          , "printf 'stderr before crash' >&2"
          , "exit 1"
          ]
        withActon acton proj ["test", "--no-cache", "--json", "--syspath", system] $ \ph _ output -> do
          code <- await "failed test exit" (getProcessExitCode ph)
          logText <- output
          assertBool logText (code /= ExitSuccess)
          forM_ ["startup diagnostic", "stdout before crash", "stderr before crash"] $ \message ->
            assertBool logText (message `isInfixOf` logText)
  , testCase "live updates keep the cursor hidden and avoid repainting unchanged rows" $
      withProcessProject $ \acton proj system -> do
        writeExecutable (proj </> "out/bin/.test_main")
          [ testInfo False
          , "sleep 0.3"
          , testInfo False
          , "sleep 0.3"
          , testInfo True
          ]
        withActon acton proj ["test", "--tty", "--no-cache", "--color", "never", "--syspath", system] $ \ph _ output -> do
          code <- await "test exit" (getProcessExitCode ph)
          logText <- output
          assertEqual logText ExitSuccess code
          assertCursorRestored logText
          assertBool "cursor restoration is the last terminal operation" ("\ESC[?25h" `isSuffixOf` logText)
          let liveOutput = dropWhile (not . isPrefixOf "\ESC[?25l") (tails (testOutput logText))
              updates = case liveOutput of
                text:_ -> text
                [] -> ""
          assertBool updates (count "ready:" updates <= 3)
          assertBool "spinner updates move directly to column two" ("\ESC[1A\r " `isInfixOf` updates)
          assertBool "live updates do not erase the row before writing it" (not ("\ESC[2K" `isInfixOf` updates))
  , testGroup "non-interactive test reports do not change cursor visibility"
      [ testCase label $ withProcessProject $ \acton proj system -> do
          writeExecutable (proj </> "out/bin/.test_main") [testInfo True]
          withActon acton proj (["test", "--no-cache", "--syspath", system] ++ args) $ \ph _ output -> do
            code <- await "test exit" (getProcessExitCode ph)
            logText <- output
            assertEqual logText ExitSuccess code
            assertBool logText (not ("\ESC[?25" `isInfixOf` logText))
      | (label, args) <- [("redirected", []), ("quiet", ["--tty", "--quiet"]), ("JSON", ["--tty", "--json"])]
      ]
  , testGroup "test process exit overrides reported success"
      [ testCase label $ withProcessProject $ \acton proj system -> do
          forM_ [0, 70] $ \exitCode -> do
            writeExecutable (proj </> "out/bin/.test_main") [testInfo True, "exit " ++ show exitCode]
            (code, out, err) <- readCreateProcessWithExitCode
              (proc acton (["test", "--no-cache", "--syspath", system] ++ args)) { cwd = Just proj } ""
            let logText = out ++ err
                failed = exitCode /= 0
                summary = if null args
                  then if failed then "1 out of 1 tests errored" else "All 1 tests passed"
                  else if failed then "\"errors\":1" else "\"errors\":0"
            assertEqual logText (if failed then ExitFailure 2 else ExitSuccess) code
            assertBool logText (summary `isInfixOf` out)
            when failed $
              assertBool logText ("Test process exited with code 70" `isInfixOf` out)
      | (label, args) <- [("text", []), ("JSON", ["--json"])]
      ]
  , testCase "a failed test process restores the cursor" $
      withProcessProject $ \acton proj system -> do
        writeExecutable (proj </> "out/bin/.test_main") ["exit 1"]
        withActon acton proj ["test", "--tty", "--no-cache", "--syspath", system] $ \ph _ output -> do
          code <- await "failed test exit" (getProcessExitCode ph)
          logText <- output
          assertBool logText (code /= ExitSuccess)
          assertCursorRestored logText
  ]
  where
    -- Inherited child pipes prevent EOF after the leader exits; redirected
    -- pipes allow capture to finish but must not let the child outlive its owner.
    childProcess redirected =
      [ "echo $$ > leader.pid"
      , "sh -c 'trap \"\" TERM; echo $$ > child.pid; exec sleep 300' </dev/null" ++
          (if redirected then " >/dev/null 2>&1" else "") ++ " &"
      , "while [ ! -s child.pid ]; do sleep 0.01; done"
      ]
    waitingParent = ["trap '' TERM", "while :; do sleep 1; done"]
    testInfo = moduleTestInfo "main"
    count needle = length . filter (isPrefixOf needle) . tails
    -- The test output starts after the runner says how it uses the cache.
    testOutput logText = case dropWhile (\t -> not (any (`isPrefixOf` t) ["Skipping test result cache", "Not using cached test results"])) (tails logText) of
      text:_ -> text
      _ -> logText
    assertCursorRestored output = do
      let logText = testOutput output
      assertEqual "cursor is hidden once" 1 (count "\ESC[?25l" logText)
      assertEqual "cursor is restored once" 1 (count "\ESC[?25h" logText)
      assertBool "hide precedes restore"
        (length (dropWhile (not . isPrefixOf "\ESC[?25l") (tails logText)) >
         length (dropWhile (not . isPrefixOf "\ESC[?25h") (tails logText)))
    moduleTestInfo modName complete = "echo '{\"test_info\":{\"definition\":{\"module\":\"" ++ modName ++ "\",\"name\":\"_test_ready\"},\"complete\":" ++
      (if complete then "true" else "false") ++ ",\"success\":true,\"num_iterations\":1}}' >&2"
    awaitFile path = await path $ do
      exists <- doesFileExist path
      return $ if exists then Just () else Nothing
    awaitReports count output = await "successful test report" $ do
      logText <- output
      return $ if length (filter ("tests passed" `isInfixOf`) (lines logText)) >= count then Just () else Nothing
    awaitGeneration proj = await "watch generation completion" (Just <$> withProjectLock proj (return ()))
    zigRuns proj = length . lines <$> readOutput (proj </> "zig-runs")
    awaitZigRuns proj n = await ("Zig run " ++ show n) $ do
      runs <- zigRuns proj
      return $ if runs >= n then Just () else Nothing
    -- A module whose NotImplemented answer native.ext.c implements, with a
    -- value from native_value.h.
    writeNativeModule proj = do
      let prefix = "watch_processQ_nativeQ_"
      writeFile (proj </> "src/native.act") $ unlines
        [ "def answer() -> int:"
        , "    NotImplemented"
        ]
      writeFile (proj </> "src/native_value.h") "#define NATIVE_VALUE 42\n"
      writeFile (proj </> "src/native.ext.c") $ unlines
        [ "#include \"native_value.h\""
        , "void " ++ prefix ++ "__ext_init__() {"
        , "}"
        , "int64_t " ++ prefix ++ "answer() {"
        , "    return NATIVE_VALUE;"
        , "}"
        ]

-- Real Acton compilation supplies test metadata; replacing Zig and the test
-- binary with scripts makes process lifetime and signals deterministic.
withProcessProject :: (FilePath -> FilePath -> FilePath -> IO ()) -> IO ()
withProcessProject action =
    withSystemTempDirectory "acton-watch-process" $ \proj -> do
      acton <- canonicalizePath "../../dist/bin/acton"
      let dist = takeDirectory (takeDirectory acton)
          system = proj </> "system"
          name = "watch_process"
          fingerprint = Fingerprint.formatFingerprint
            (Fingerprint.updateFingerprintPrefix (Fingerprint.fingerprintPrefixForName name) 1)
      createDirectory system
      entries <- listDirectory dist
      forM_ (filter (/= "zig") entries) $ \entry ->
        createSymbolicLink (dist </> entry) (system </> entry)
      writeExecutable (system </> "zig/zig") ["exit 0"]
      createDirectory (proj </> "src")
      writeFile (proj </> "Build.act") ("name = " ++ show name ++ "\nfingerprint = " ++ fingerprint ++ "\n")
      writeFile (proj </> "src/main.act") $ unlines
        [ "import testing"
        , "def _test_ready():"
        , "    testing.assertEqual(1, 1)"
        , "actor main(env):"
        , "    proc def __startup__():"
        , "        env.exit(0)"
        ]
      action acton proj system `finally`
        forM_ ["child.pid", "leader.pid"] (\name -> do
          text <- readOutput (proj </> name)
          forM_ (readMaybe text :: Maybe ProcessID) $ \pid ->
            signalProcess sigKILL pid `catch` ignoreIO)

writeExecutable :: FilePath -> [String] -> IO ()
writeExecutable path body = do
    createDirectoryIfMissing True (takeDirectory path)
    writeFile path (unlines ("#!/bin/sh" : body))
    permissions <- getPermissions path
    setPermissions path permissions { executable = True }

withActon :: FilePath -> FilePath -> [String] -> (ProcessHandle -> ProcessID -> IO String -> IO a) -> IO a
withActon acton proj args action = mask $ \restore ->
    withFile (proj </> "compiler.log") WriteMode $ \output ->
      withCreateProcess (proc acton args)
        { cwd = Just proj, std_in = NoStream, std_out = UseHandle output
        , std_err = UseHandle output, create_group = True
        } $ \_ _ _ ph -> do
          Just pid <- getPid ph
          (restore (action ph pid (readOutput (proj </> "compiler.log")))
            `onException` restore (saveProcessDiagnostics acton proj args ph pid `catch` ignoreIO))
            `finally` stopProcessGroup ph (Just pid)

-- Save evidence before cleanup stops a hung compiler. CI uploads test/ on failure.
saveProcessDiagnostics :: FilePath -> FilePath -> [String] -> ProcessHandle -> ProcessID -> IO ()
saveProcessDiagnostics acton proj args ph pid = do
    let root = takeDirectory (takeDirectory (takeDirectory acton))
        dir = root </> "test/watch-process/out" </> takeFileName proj
        pidFiles = ["leader.pid", "child.pid"]
        capture name command arguments = do
          result <- Timeout.timeout 5000000 (readProcessWithExitCode command arguments "")
          writeFile (dir </> name) $ case result of
            Nothing -> "Diagnostic command timed out\n"
            Just (code, out, err) -> show code ++ "\n" ++ out ++ err
    createDirectoryIfMissing True dir
    forM_ ("compiler.log" : pidFiles) $ \name ->
      copyFile (proj </> name) (dir </> name) `catch` ignoreIO
    code <- getProcessExitCode ph
    writeFile (dir </> "command.txt") $ unlines
      ["cwd: " ++ proj, show (acton : args), "exit: " ++ show code]
    pids <- (pid :) . mapMaybe readMaybe <$> mapM (readOutput . (proj </>)) pidFiles
    capture "processes.txt" "ps"
      ["-o", "pid=,ppid=,pgid=,stat=,etime=,wchan=,comm=", "-p", intercalate "," (map show pids)]
    when (os == "darwin" && code == Nothing) $
      capture "sample.log" "/usr/bin/sample" [show pid, "2", "-file", dir </> "acton.sample.txt"]
    hPutStrLn stderr ("Watch process diagnostics saved in " ++ dir)

readOutput :: FilePath -> IO String
readOutput path = (BS.unpack <$> BS.readFile path) `catch` \(_ :: IOException) -> return ""

awaitPid :: FilePath -> FilePath -> IO ProcessID
awaitPid proj name = await name (readMaybe <$> readOutput (proj </> name))

await :: String -> IO (Maybe a) -> IO a
await label check = do
    result <- Timeout.timeout 120000000 loop
    case result of
      Just value -> return value
      Nothing -> assertFailure ("Timed out waiting for " ++ label)
  where
    loop = do
      result <- check
      maybe (threadDelay 20000 >> loop) return result

assertStopped :: String -> ProcessID -> Assertion
assertStopped label pid = do
    stopped <- Timeout.timeout 5000000 $ await label $ do
      (code, status, _) <- readProcessWithExitCode "ps" ["-o", "stat=", "-p", show pid] ""
      return $ if code /= ExitSuccess || 'Z' `elem` status then Just () else Nothing
    assertBool (label ++ " should stop with its owner") (isJust stopped)

ignoreIO :: IOException -> IO ()
ignoreIO _ = return ()
