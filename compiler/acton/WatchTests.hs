{-# LANGUAGE ScopedTypeVariables #-}
module WatchTests (watchTriggerTests, watchProcessTests, buildAct, writeExecutable, withActon, awaitWithin, awaitGenerationWithin) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, catch, finally, mask, onException, try)
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.IORef
import qualified Data.Map as M
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), addUTCTime)
import Data.List (intercalate, isInfixOf, isPrefixOf, isSuffixOf, sort, tails)
import Data.Maybe (isJust, mapMaybe)
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp (withSystemTempDirectory)
import System.Info (os)
import System.Posix.Files (createSymbolicLink, fileSize, getFileStatus, modificationTimeHiRes, nullFileMode, ownerModes, setFileMode, statusChangeTimeHiRes)
import System.Posix.Signals
import System.Posix.Types (ProcessID)
import System.Process
import qualified System.Timeout as Timeout
import Text.Read (readMaybe)
import qualified System.FSNotify as FS

import Acton.Project (withProjectLock)
import qualified Acton.Fingerprint as Fingerprint
import qualified InterfaceFiles
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
  , testCase "a save at an embedded path asks for a full build once the file changed" $
      forM_ [FS.Modified, FS.Added, FS.Removed] $ \event ->
        embedWatchTrigger embedded (event "/p/web/app.js" t0 FS.IsFile) @?= Just (WatchSave "/p/web/app.js" Nothing)
  , testCase "a file beside an embedded file asks for nothing" $
      forM_ [FS.Modified, FS.Added, FS.Removed] $ \event ->
        forM_ ["/p/web/other.js", "/p/web/app.js.new", "/p/data/sub/other.txt"] $ \path ->
          embedWatchTrigger embedded (event path t0 FS.IsFile) @?= Nothing
  , testCase "a directory on the way to an embedded path asks for a full build when it appears or goes" $ do
      forM_ ["/p/data", "/p/data/", "/p/data/sub", "/p/data/sub/"] $ \dir -> do
        forM_ [FS.Added, FS.Removed, FS.WatchedDirectoryRemoved] $ \event ->
          embedWatchTrigger embedded (event dir t0 FS.IsDirectory) @?= Just WatchFull
        embedWatchTrigger embedded (FS.Modified dir t0 FS.IsDirectory) @?= Nothing
      forM_ ["/p/dat", "/p/data/other", "/p/data/sub/text.txt.d/"] $ \dir ->
        embedWatchTrigger embedded (FS.Added dir t0 FS.IsDirectory) @?= Nothing
  , testCase "a directory at the path of an embedded file asks for a full build" $
      embedWatchTrigger embedded (FS.Added "/p/web/app.js/" t0 FS.IsDirectory) @?= Just WatchFull
  , testCase "a file or a link at the path of a directory on the way asks for a full build" $
      embedWatchTrigger embedded (FS.Added "/p/data" t0 FS.IsFile) @?= Just WatchFull
  , testCase "an event of a file at a path where the build found no file to read asks for a full build" $
      forM_ [FS.Modified, FS.Added, FS.Removed, FS.ModifiedAttributes] $ \event ->
        embedWatchTrigger embedded (event "/p/data/sub/text.txt" t0 FS.IsFile) @?= Just WatchFull
  , testCase "attribute changes of an embedded file are skipped" $
      embedWatchTrigger embedded (FS.ModifiedAttributes "/p/web/app.js" t0 FS.IsFile) @?= Nothing
  , testCase "a save of a file that an input path names or stands for asks for a full build once the file changed" $
      forM_ [FS.Modified, FS.Added, FS.Removed] $ \event ->
        -- An input path that is written with a name that starts with a dot
        -- is taken as it is written.
        forM_ ["/p/web/app.js", "/p/web/sub/new.css", "/p/web/app.js~", "/p/data/schema.txt", "/p/web/.well-known/acme.txt"] $ \path ->
          inputWatchTrigger inputs (event path t0 FS.IsFile) @?= Just (WatchSave path Nothing)
  , testCase "a file that no input path stands for asks for nothing" $
      forM_ [FS.Modified, FS.Added, FS.Removed] $ \event -> do
        forM_ ["/p/web/.DS_Store", "/p/web/.cache/kept.txt", "/p/web/page.tydb/data.mdb", "/p/webs/app.js", "/p/other.txt", "/p/data/other.txt"] $ \path ->
          inputWatchTrigger inputs (event path t0 FS.IsFile) @?= Nothing
        inputWatchTrigger [] (event "/p/web/app.js" t0 FS.IsFile) @?= Nothing
  , testCase "a directory at or below an input path asks for a full build when it appears or goes" $ do
      forM_ ["/p/web", "/p/web/", "/p/web/sub", "/p/web/sub/"] $ \dir -> do
        forM_ [FS.Added, FS.Removed, FS.WatchedDirectoryRemoved] $ \event ->
          inputWatchTrigger inputs (event dir t0 FS.IsDirectory) @?= Just WatchFull
        inputWatchTrigger inputs (FS.Modified dir t0 FS.IsDirectory) @?= Nothing
      forM_ ["/p/web/.git", "/p/web/page.tydb/", "/p/data"] $ \dir ->
        inputWatchTrigger inputs (FS.Added dir t0 FS.IsDirectory) @?= Nothing
  , testCase "attribute changes of an input file are skipped" $
      inputWatchTrigger inputs (FS.ModifiedAttributes "/p/web/app.js" t0 FS.IsFile) @?= Nothing
  , testCase "the directories at or below a path that the input paths name or stand for" $
      withSystemTempDirectory "acton-watch-input-directories" $ \tmp -> do
        dir <- canonicalizePath tmp
        forM_ ["web/sub/deep", "web/.git/objects", "web/.well-known/sub", "web/page.tydb", "other/sub", "linked/sub"] $ \sub ->
          createDirectoryIfMissing True (dir </> sub)
        writeFile (dir </> "web/app.js") "let a = 1\n"
        createSymbolicLink (dir </> "linked") (dir </> "web/link")
        let at = fmap sort . inputDirectoriesAt (map (dir </>) ["web", "web/.well-known", "web/app.js", "missing"]) . (dir </>)
            under = map (dir </>)
        at "web" >>= (@?= under ["web", "web/.well-known", "web/.well-known/sub", "web/sub", "web/sub/deep"])
        at "web/sub" >>= (@?= under ["web/sub", "web/sub/deep"])
        forM_ ["web/app.js", "missing", "other", "web/.git"] $ \path ->
          at path >>= (@?= [])
  , testCase "the stamp of one file is recorded" $
      withSystemTempDirectory "acton-watch-record-file" $ \dir -> do
        stamps <- newIORef M.empty
        let path = dir </> "schema.txt"
        writeFile path "one\n"
        recordStamps stamps path
        shouldTriggerFile stamps path >>= (@?= False)
        writeFile path "three\n"
        shouldTriggerFile stamps path >>= (@?= True)
  , testCase "the stamp of a file is the stamp of its stat" $
      withSystemTempDirectory "acton-watch-stat" $ \dir -> do
        let path = dir </> "text.txt"
        writeFile path "one\n"
        stat <- statOf path
        (@?= Just (statStamp stat)) =<< readFileStamp path
  , testCase "a file that is another one than a build saw asks for one build" $
      withSystemTempDirectory "acton-watch-seen" $ \dir -> do
        stamps <- newIORef M.empty
        let path    = dir </> "text.txt"
            changed = changedSinceBuild stamps path
        -- No file, which is what the build saw.
        (@?= False) =<< changed Nothing
        -- A file that is created after the build looked, and the build that
        -- then sees it.
        writeFile path "one\n"
        one <- readFileStamp path
        (@?= True) =<< changed Nothing
        (@?= False) =<< changed one
        -- New content after a build looked.
        writeFile path "three\n"
        three <- readFileStamp path
        (@?= True) =<< changed one
        -- A file that embed cannot take is no file to each build.
        (@?= False) =<< changed Nothing
        -- New content that an event told of before: that event asked for a
        -- build.
        writeFile path "sixteen\n"
        (@?= True) =<< shouldTriggerFile stamps path
        (@?= False) =<< changed three
        -- A file that is removed after a build looked.
        sixteen <- readFileStamp path
        removeFile path
        (@?= True) =<< changed sixteen
        (@?= False) =<< changed Nothing
  , testCase "the embedded watch asks for a build when a file is another one than the build saw" $
      withSystemTempDirectory "acton-watch-embedded" $ \tmp -> do
        dir    <- canonicalizePath tmp
        builds <- newIORef (0 :: Int)
        stamps <- newIORef M.empty
        (look, _) <- embeddedWatch stamps (\_ _ -> return ()) (modifyIORef builds (+ 1)) assertFailure
        let path = dir </> "text.txt"
        writeFile path "one\n"
        one <- statOf path
        -- The build saw the file that is there.
        look (M.fromList [(path, Just one)])
        (@?= 0) =<< readIORef builds
        -- The file has new content since the build looked.
        writeFile path "three\n"
        look (M.fromList [(path, Just one)])
        (@?= 1) =<< readIORef builds
  , testCase "the embedded watch tells of the files that an input path stands for, also in a directory that appears" $
      withSystemTempDirectory "acton-watch-inputs" $ \tmp -> do
        dir      <- canonicalizePath tmp
        stamps   <- newIORef M.empty
        triggers <- newIORef []
        let onEvent toTrigger ev = forM_ (toTrigger ev) (\trigger -> modifyIORef triggers (trigger :))
            told trigger = await ("the trigger " ++ show trigger) ((\ts -> if trigger `elem` ts then Just () else Nothing) <$> readIORef triggers)
            saved path = WatchSave (dir </> path) Nothing
        createDirectory (dir </> "data")
        (_, watchInputs) <- embeddedWatch stamps onEvent (return ()) assertFailure
        watchInputs [dir </> "data", dir </> "later/"]
        -- The events of one directory arrive in the order of the writes.
        writeFile (dir </> "data/.hidden") "hidden\n"
        writeFile (dir </> "data/one.txt") "one\n"
        told (saved "data/one.txt")
        (@?= False) . elem (saved "data/.hidden") =<< readIORef triggers
        -- A directory that appears below an input path is watched when
        -- the trigger of its event is there.
        writeIORef triggers []
        createDirectory (dir </> "data/sub")
        told WatchFull
        writeFile (dir </> "data/sub/two.txt") "two\n"
        told (saved "data/sub/two.txt")
        -- A directory that appears at an input path.
        writeIORef triggers []
        createDirectory (dir </> "later")
        told WatchFull
        writeFile (dir </> "later/three.txt") "three\n"
        told (saved "later/three.txt")
        -- The paths that are given last are the input paths.
        watchInputs [dir </> "later"]
        writeFile (dir </> "data/four.txt") "four\n"
        writeFile (dir </> "later/five.txt") "five\n"
        told (saved "later/five.txt")
        (@?= False) . elem (saved "data/four.txt") =<< readIORef triggers
  ]
  where t0 = UTCTime (fromGregorian 2026 1 1) 0
        -- The input paths of the derived modules of a project.
        inputs = ["/p/web", "/p/data/schema.txt", "/p/web/.well-known"]
        -- What a build saw at the paths of two embedded files and of the
        -- directories on the way to them: it read web/app.js and found no
        -- file to read at data/sub/text.txt.
        embedded = M.fromList
          [ ("/p/web", Nothing), ("/p/web/app.js", Just (InterfaceFiles.SourceFileMeta 0 0 0 Nothing Nothing))
          , ("/p/data", Nothing), ("/p/data/sub", Nothing), ("/p/data/sub/text.txt", Nothing) ]
        -- The stat of a file as a plan records it.
        statOf path = do
          st <- getFileStatus path
          let nanoseconds = InterfaceFiles.posixNanoseconds
          return (InterfaceFiles.SourceFileMeta (nanoseconds (modificationTimeHiRes st)) (nanoseconds (statusChangeTimeHiRes st))
                    (toInteger (fileSize st)) Nothing Nothing)

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
  , testCase "build watch builds again when an embedded file has new content" $
      withProcessProject $ \acton proj system -> do
        let text = proj </> "data/text.txt"
        createDirectory (proj </> "data")
        writeFile text "one\n"
        writeFile (proj </> "src/page.act") "text = embed(\"data/text.txt\")\n"
        writeExecutable (system </> "zig/zig") ["echo run >> zig-runs"]
        withActon acton proj ["build", "--watch", "--verbose", "--syspath", system] $ \ph _ output -> do
          let compiles = count "Stale page: source changed" <$> output
          awaitZigRuns proj 1
          awaitGeneration proj
          -- A build of one changed module looks at the embedded files of
          -- the other modules too, so the watch goes on after it.
          appendFile (proj </> "src/main.act") "\n# An edit of other code\n"
          awaitZigRuns proj 2
          awaitGeneration proj
          first <- compiles
          replaceFile text "two two\n"
          awaitEmbeddedSize proj "watch_process" 8
          awaitGeneration proj
          second <- compiles
          assertBool "the module should be compiled again for the new content" (second > first)
          -- A file beside the embedded one, and then the same content with
          -- another modification time, which asks for a build that compiles
          -- nothing.
          runs <- zigRuns proj
          writeFile (proj </> "data/other.txt") "no module embeds this file\n"
          time <- getModificationTime text
          writeFile (text <.> "new") "two two\n"
          setModificationTime (text <.> "new") (addUTCTime (-30) time)
          renameFile (text <.> "new") text
          awaitZigRuns proj (runs + 1)
          awaitGeneration proj
          logText <- output
          assertEqual ("the module should not be compiled again for the same content\n" ++ logText) second (count "Stale page: source changed" logText)
          assertBool ("no event of data/other.txt should start a build\n" ++ logText) (not ("other.txt" `isInfixOf` logText))
          removeFile text
          await "the message for the removed file" $ do
            removed <- output
            return $ if "no file data/text.txt in " `isInfixOf` removed then Just () else Nothing
          assertEqual "watch should remain running" Nothing =<< getProcessExitCode ph
  , testCase "build watch builds again when a file that a later build embeds has new content" $
      withProcessProject $ \acton proj system -> do
        forM_ ["data", "more"] $ \dir -> createDirectory (proj </> dir)
        writeFile (proj </> "data/text.txt") "one\n"
        writeFile (proj </> "more/text.txt") "three\n"
        writeFile (proj </> "src/page.act") "text = embed(\"data/text.txt\")\n"
        writeExecutable (system </> "zig/zig") ["echo run >> zig-runs"]
        withActon acton proj ["build", "--watch", "--syspath", system] $ \ph _ _ -> do
          awaitZigRuns proj 1
          awaitGeneration proj
          -- The second build finds an embed of a file in a directory that
          -- the first build did not look at.
          appendFile (proj </> "src/page.act") "more = embed(\"more/text.txt\")\n"
          awaitEmbeddedSize proj "watch_process" 6
          awaitZigRuns proj 2
          awaitGeneration proj
          replaceFile (proj </> "more/text.txt") "sixteen sixteen\n"
          awaitEmbeddedSize proj "watch_process" 16
          assertEqual "watch should remain running" Nothing =<< getProcessExitCode ph
  , testCase "build watch builds again when a file in the last of 600 directories has new content" $
      withProcessProject $ \acton proj system -> do
        -- macOS gives a process events from about 500 watches, and a watch
        -- beyond them tells of nothing.
        let dirs = [ "data/d" ++ show n | n <- [100 .. 699 :: Int] ]
            text = last dirs </> "text.txt"
        forM_ dirs $ \dir -> do
          createDirectoryIfMissing True (proj </> dir)
          writeFile (proj </> dir </> "text.txt") "one\n"
        writeFile (proj </> "src/page.act") $ unlines
          [ "text" ++ show n ++ " = embed(\"" ++ dir ++ "/text.txt\")" | (n, dir) <- zip [1 :: Int ..] dirs ]
        writeExecutable (system </> "zig/zig") ["echo run >> zig-runs"]
        withActon acton proj ["build", "--watch", "--verbose", "--syspath", system] $ \ph _ output -> do
          awaitZigRuns proj 1
          awaitGeneration proj
          -- The watch is told of the file.  The watches of the directories
          -- start after the first build, and a write before the watch of
          -- its directory gives no event: the build then takes the new
          -- content when the watch compares the file with what the first
          -- build saw.  So the file is written until its event is there.
          await "the event for the file" $ do
            replaceFile (proj </> text) "two two\n"
            logText <- output
            return $ if any (\line -> "watch event" `isInfixOf` line && text `isInfixOf` line) (lines logText) then Just () else Nothing
          awaitEmbeddedSize proj "watch_process" 8
          assertEqual "watch should remain running" Nothing =<< getProcessExitCode ph
  , testCase "build watch builds again when a missing embedded file is created" $
      withProcessProject $ \acton proj system -> do
        writeFile (proj </> "src/page.act") "text = embed(\"data/sub/text.txt\")\n"
        writeExecutable (system </> "zig/zig") ["echo run >> zig-runs"]
        withActon acton proj ["build", "--watch", "--syspath", system] $ \ph _ output -> do
          await "the message for the missing file" $ do
            logText <- output
            return $ if "no file data/sub/text.txt in " `isInfixOf` logText then Just () else Nothing
          assertEqual "the first build should fail before Zig" 0 =<< zigRuns proj
          -- The first directory on the way to the file appears and goes.  No
          -- file appears, so only the event for that directory starts a
          -- build.
          await "a build for the directory that appears" $ do
            createDirectory (proj </> "data")
            removeDirectory (proj </> "data")
            logText <- output
            return $ if count "Building project in" logText >= 2 then Just () else Nothing
          createDirectoryIfMissing True (proj </> "data/sub")
          replaceFile (proj </> "data/sub/text.txt") "one\n"
          awaitEmbeddedSize proj "watch_process" 4
          awaitZigRuns proj 1
          awaitGeneration proj
          -- The directories of the file are watched since the build that
          -- found them.
          replaceFile (proj </> "data/sub/text.txt") "two two\n"
          awaitEmbeddedSize proj "watch_process" 8
          assertEqual "watch should remain running" Nothing =<< getProcessExitCode ph
  , testCase "build watch builds again when an embedded file changes before its directory is watched" $
      withProcessProject $ \acton proj system -> do
        createDirectory (proj </> "data")
        writeFile (proj </> "data/text.txt") "one\n"
        writeFile (proj </> "src/page.act") "text = embed(\"data/text.txt\")\n"
        -- The file gets new content while Zig runs in the first build, which
        -- is after the build looked at the file and before the watch looks
        -- at its directory.  The first build reports the file, and the watch
        -- builds again.
        writeExecutable (system </> "zig/zig")
          [ "if [ ! -e changed ]; then"
          , "    touch changed"
          , "    printf 'two two\\n' > text.new"
          , "    mv text.new data/text.txt"
          , "fi"
          ]
        withActon acton proj ["build", "--watch", "--syspath", system] $ \ph _ output -> do
          awaitEmbeddedSize proj "watch_process" 8
          await "a build with the new content" $ do
            logText <- output
            return $ if "Final compilation done" `isInfixOf` logText then Just () else Nothing
          logText <- output
          -- The build that reports the file is not reported as done: the
          -- next build starts between the report and the first build that
          -- is done.
          let reported = from "data/text.txt changed while the build ran. Run the build again" logText
              done     = from "Final compilation done" logText
          assertBool ("the first build should report the changed file\n" ++ logText) (not (null reported))
          assertBool ("the build that reports the changed file should not be reported as done\n" ++ logText)
            ("Building project in" `isInfixOf` take (length reported - length done) reported)
          assertEqual "watch should remain running" Nothing =<< getProcessExitCode ph
  , testCase "build watch builds again when a file that a local dependency embeds has new content" $
      withProcessProject $ \acton proj system -> do
        let lib = proj </> "lib"
        forM_ ["src", "data"] $ \dir -> createDirectoryIfMissing True (lib </> dir)
        writeFile (lib </> "Build.act") (buildAct "lib" [])
        writeFile (lib </> "data/text.txt") "one\n"
        writeFile (lib </> "src/page.act") "text = embed(\"data/text.txt\")\n"
        writeFile (proj </> "Build.act") (buildAct "watch_process" ["dependencies = {", "    \"lib\": (path=\"lib\")", "}"])
        writeFile (proj </> "src/main.act") $ unlines
          [ "import lib.page"
          , "actor main(env):"
          , "    proc def __startup__():"
          , "        print(lib.page.text)"
          , "        env.exit(0)"
          ]
        writeExecutable (system </> "zig/zig") ["echo run >> zig-runs"]
        withActon acton proj ["build", "--watch", "--syspath", system] $ \ph _ _ -> do
          awaitZigRuns proj 1
          awaitGeneration proj
          replaceFile (lib </> "data/text.txt") "two two\n"
          awaitEmbeddedSize lib "lib" 8
          awaitZigRuns proj 2
          assertEqual "watch should remain running" Nothing =<< getProcessExitCode ph
  , testCase "test watch reruns the tests that use a module whose embedded file has new content" $
      withProcessProject $ \acton proj system -> do
        createDirectory (proj </> "data")
        writeFile (proj </> "data/text.txt") "one\n"
        writeFile (proj </> "src/page.act") "text = embed(\"data/text.txt\")\n"
        writeFile (proj </> "src/other.act") $ unlines
          [ "import testing"
          , "import page"
          , "def _test_ready():"
          , "    testing.assertEqual(page.text[0:1], b\"o\")"
          ]
        writeExecutable (proj </> "out/bin/.test_main") ["touch main-ran", testInfo True]
        writeExecutable (proj </> "out/bin/.test_other") ["touch other-ran", moduleTestInfo "other" True]
        -- The results are cached, so a test runs again only when a name that
        -- it uses has another impl hash.
        withActon acton proj ["test", "--watch", "--syspath", system] $ \ph _ output -> do
          awaitReports 1 output
          awaitGeneration proj
          forM_ ["main-ran", "other-ran"] $ \name ->
            assertBool (name ++ " should run in the first test run") =<< doesFileExist (proj </> name)
          forM_ ["main-ran", "other-ran"] $ \name -> removeFile (proj </> name)
          replaceFile (proj </> "data/text.txt") "two two\n"
          -- The test of other uses the embedded content.
          awaitFile (proj </> "other-ran")
          awaitGeneration proj
          assertBool "the test of main should not run" . not =<< doesFileExist (proj </> "main-ran")
          assertEqual "watch should remain running" Nothing =<< getProcessExitCode ph
  , testCase "build watch builds every module again when a module source that another module embeds is saved" $
      withProcessProject $ \acton proj system -> do
        -- No module imports text, and page embeds its source.  The watch of
        -- src and the watch of the embedded files both see each save, in
        -- either order.
        let text = proj </> "src/text.act"
        writeFile text "value = 1\n"
        writeFile (proj </> "src/page.act") "text = embed(\"src/text.act\")\n"
        writeExecutable (system </> "zig/zig") ["echo run >> zig-runs"]
        withActon acton proj ["build", "--watch", "--syspath", system] $ \ph _ _ -> do
          awaitZigRuns proj 1
          awaitGeneration proj
          forM_ [1 .. 6 :: Int] $ \n -> do
            let source = "value = 1" ++ replicate n '0' ++ "\n"
            writeFile text source
            awaitEmbeddedSize proj "watch_process" (length source)
            awaitGeneration proj
          assertEqual "watch should remain running" Nothing =<< getProcessExitCode ph
  , testCase "build watch builds again when an embedded file is renamed to the spelling of its path" $
      withProcessProject $ \acton proj system -> do
        createDirectory (proj </> "data")
        writeFile (proj </> "data/text.txt") "one\n"
        writeFile (proj </> "src/page.act") "text = embed(\"data/Text.txt\")\n"
        writeExecutable (system </> "zig/zig") ["echo run >> zig-runs"]
        withActon acton proj ["build", "--watch", "--syspath", system] $ \ph _ output -> do
          -- The message names the path: no file there, or, on a file system
          -- that takes one spelling of a name for another, a file with
          -- another name.  The watch then builds once more for the file that
          -- it finds at the path.
          await "the messages for the path" $ do
            logText <- output
            return $ if count "data/Text.txt" logText >= 4 || "no file data/Text.txt in " `isInfixOf` logText then Just () else Nothing
          awaitGeneration proj
          assertEqual "the builds should fail before Zig" 0 =<< zigRuns proj
          renameFile (proj </> "data/text.txt") (proj </> "data/Text.txt")
          awaitEmbeddedSize proj "watch_process" 4
          awaitZigRuns proj 1
          assertEqual "watch should remain running" Nothing =<< getProcessExitCode ph
  , testCase "build watch keeps running when the directory of an embedded file cannot be read" $
      withProcessProject $ \acton proj system -> do
        let sub = proj </> "data/sub"
        createDirectoryIfMissing True sub
        writeFile (sub </> "text.txt") "one\n"
        writeFile (proj </> "src/page.act") "text = embed(\"data/sub/text.txt\")\n"
        writeExecutable (system </> "zig/zig") ["echo run >> zig-runs"]
        -- Linux refuses the watch of a directory without read permission.
        -- The build fails for a user who cannot list the directory, and
        -- succeeds for one who can read every file.
        setFileMode sub nullFileMode
        flip finally (setFileMode sub ownerModes) $
          withActon acton proj ["build", "--watch", "--syspath", system] $ \ph _ output -> do
            let builds = count "Building project in" <$> output
            await "the first build" $ do
              started <- builds
              return $ if started >= 1 then Just () else Nothing
            awaitGeneration proj
            first <- builds
            appendFile (proj </> "src/main.act") "\n# An edit\n"
            await "a build after the edit" $ do
              started <- builds
              return $ if started > first then Just () else Nothing
            awaitGeneration proj
            assertEqual "watch should remain running" Nothing =<< getProcessExitCode ph
  , testCase "the watch of a file without a project builds again for its embedded file" $
      withSystemTempDirectory "acton-watch-file" $ \tmp -> do
        acton <- canonicalizePath "../../dist/bin/acton"
        dir   <- canonicalizePath tmp
        writeFile (dir </> "prog.act") $ unlines
          [ "text = embed(\"text.txt\")"
          , "actor main(env):"
          , "    proc def __startup__():"
          , "        print(text)"
          , "        env.exit(0)"
          ]
        -- The log of the compiler is written to the same directory all the
        -- time, and no module embeds it.
        withActon acton dir ["prog.act", "--watch", "--color", "never"] $ \ph _ output -> do
          await "the message for the missing file" $ do
            logText <- output
            return $ if "no file text.txt in " `isInfixOf` logText then Just () else Nothing
          replaceFile (dir </> "text.txt") "one\n"
          awaitProgramOutput (dir </> "prog") "b'one\\n'\n"
          replaceFile (dir </> "text.txt") "two two\n"
          awaitProgramOutput (dir </> "prog") "b'two two\\n'\n"
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
    -- The text from the first place of a needle on, and no text without one.
    from needle = concat . take 1 . filter (isPrefixOf needle) . tails
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
    awaitGeneration = awaitGenerationWithin 120
    zigRuns proj = length . lines <$> readOutput (proj </> "zig-runs")
    awaitZigRuns proj n = await ("Zig run " ++ show n) $ do
      runs <- zigRuns proj
      return $ if runs >= n then Just () else Nothing
    -- The data C file of the module page of the project with the given root
    -- and name asserts the size of the file that the module embeds.
    awaitEmbeddedSize root name size = await ("an embedded file of " ++ show size ++ " bytes") $ do
      text <- readOutput (root </> "out/types" </> name </> "page.embed.c")
      return $ if (" == " ++ show size ++ ",") `isInfixOf` text then Just () else Nothing
    -- A build replaces the program while the watch runs, so a run of it can
    -- fail.
    awaitProgramOutput program expected = await (program ++ " to print " ++ show expected) $ do
      result <- Timeout.timeout 30000000 (try (readProcessWithExitCode program [] "") :: IO (Either IOException (ExitCode, String, String)))
      return $ case result of
        Just (Right (ExitSuccess, out, _)) | out == expected -> Just ()
        _                                                    -> Nothing
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
      createDirectory system
      entries <- listDirectory dist
      forM_ (filter (/= "zig") entries) $ \entry ->
        createSymbolicLink (dist </> entry) (system </> entry)
      writeExecutable (system </> "zig/zig") ["exit 0"]
      createDirectory (proj </> "src")
      writeFile (proj </> "Build.act") (buildAct "watch_process" [])
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

-- | The Build.act of the project 'name': its name, its fingerprint and the
-- given lines.
buildAct :: String -> [String] -> String
buildAct name rest = unlines $
    [ "name = " ++ show name
    , "fingerprint = " ++ Fingerprint.formatFingerprint (Fingerprint.updateFingerprintPrefix (Fingerprint.fingerprintPrefixForName name) 1)
    ] ++ rest

-- | Put content at a path in one step, so that no build sees a file that is
-- written in part.
replaceFile :: FilePath -> String -> IO ()
replaceFile path content = do
    writeFile (path <.> "new") content
    renameFile (path <.> "new") path

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

-- Wait until no generation of a watch runs in the project, for at most the
-- given number of seconds. A generation holds the lock of the project from
-- its start to its end.
awaitGenerationWithin :: Int -> FilePath -> IO ()
awaitGenerationWithin seconds proj =
    awaitWithin seconds "the end of the generation" (Just <$> withProjectLock proj (return ()))

await :: String -> IO (Maybe a) -> IO a
await = awaitWithin 120

-- Run the check every 20 ms until it gives a value. The test fails when the
-- check gives no value within the given number of seconds.
awaitWithin :: Int -> String -> IO (Maybe a) -> IO a
awaitWithin seconds label check = do
    result <- Timeout.timeout (seconds * 1000000) loop
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
