{-# LANGUAGE ScopedTypeVariables #-}
module WatchTests (watchProcessTests) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, catch, finally, mask)
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, tails)
import Data.Maybe (isJust)
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (createSymbolicLink)
import System.Posix.Signals
import System.Posix.Types (ProcessID)
import System.Process
import qualified System.Timeout as Timeout
import Text.Read (readMaybe)

import Acton.Compile (withProjectLock)
import qualified Acton.Fingerprint as Fingerprint
import ProcessUtil (stopProcessGroup)
import Test.Tasty
import Test.Tasty.HUnit

watchProcessTests :: TestTree
watchProcessTests = testGroup "watch subprocesses"
  [ testCase "stopping build watch stops Zig descendants" $
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
    testOutput logText = case dropWhile (not . isPrefixOf "Skipping test result cache") (tails logText) of
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
        , "    env.exit(0)"
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
          restore (action ph pid (readOutput (proj </> "compiler.log"))) `finally` stopProcessGroup ph (Just pid)

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
