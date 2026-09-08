{-# LANGUAGE ScopedTypeVariables #-}
module ZigWatchTests (zigWatchTests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync, cancel, wait)
import Control.Exception (catch, IOException, bracket)
import Control.Monad
import Data.List (isInfixOf, intercalate)
import Data.List.Split (splitOn)
import System.Directory
import System.Exit
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import System.Process
import qualified System.Timeout as Timeout

import Test.Tasty
import Test.Tasty.HUnit
import qualified Acton.Fingerprint as Fingerprint
import qualified WatchTests
import qualified ZigWatch

zigWatchTests :: TestTree
zigWatchTests = testGroup "Zig watch session"
  [ testCase "real runner rebuilds and recovers without reloading its graph" $
      withSystemTempDirectory "acton-zig-build" $ \dir -> do
        zig <- canonicalizePath "../../dist/zig/zig"
        let graph = dir </> "build.zig"
            evaluated = dir </> "graph-evaluated"
            source = dir </> "probe.c"
            binary = dir </> "out" </> "bin" </> "probe"
            cp = (proc zig ["build", "--watch", "--watch-stdin",
                            "--prefix", dir </> "out", "--color", "off"]) { cwd = Just dir }
            checkOutput expected = do
              (result, _, _) <- readCreateProcessWithExitCode (proc binary []) ""
              assertEqual "executable observes latest compiled code" (ExitFailure expected) result
        writeFile graph $ unlines
          [ "const std = @import(\"std\");"
          , "pub fn build(b: *std.Build) void {"
          , "    std.Io.Dir.cwd().writeFile(b.graph.io, .{ .sub_path = \"graph-evaluated\", .data = \"loaded\" }) catch @panic(\"write\");"
          , "    const exe = b.addExecutable(.{ .name = \"probe\", .root_module = b.createModule(.{"
          , "        .target = b.graph.host, .optimize = .Debug, .link_libc = true,"
          , "    }) });"
          , "    exe.root_module.addCSourceFile(.{ .file = b.path(\"probe.c\"), .flags = &.{} });"
          , "    b.installArtifact(exe);"
          , "    const diagnostic = b.addSystemCommand(&.{ \"sh\", \"-c\", \"printf 'native output: café ACTON_ZIG_DONE 1 ok'\" });"
          , "    diagnostic.step.dependOn(&exe.step);"
          , "    b.getInstallStep().dependOn(&diagnostic.step);"
          , "    std.debug.print(\"graph output: café ACTON_ZIG_READY 99\", .{});"
          , "}"
          ]
        bracket ZigWatch.newSession ZigWatch.stopSession $ \session -> do
          let build generation expected = do
                result@(_, _, output) <- ZigWatch.build session generation cp [graph]
                assertEqual ("Zig build result:\n" ++ output) expected (code result)
                when (generation == 1) $
                  assertBool "retains unterminated UTF-8 graph diagnostics and protocol lookalikes"
                    ("graph output: café ACTON_ZIG_READY 99" `isInfixOf` output)
                when (expected == ExitSuccess) $
                  assertBool "retains unterminated native command diagnostics"
                    ("native output: café ACTON_ZIG_DONE 1 ok" `isInfixOf` output)
          writeFile source "int main(void) { return 11; }\n"
          build 1 ExitSuccess
          initial <- getModificationTime evaluated
          checkOutput 11
          writeFile source "int main(void) { return 12; }\n"
          build 2 ExitSuccess
          checkOutput 12
          writeFile source "#error deliberate watch build failure\n"
          build 3 (ExitFailure 1)
          writeFile source "int main(void) { return 13; }\n"
          build 4 ExitSuccess
          checkOutput 13
          removeFile binary
          build 5 ExitSuccess
          checkOutput 13
          assertEqual "all generations retain the same build graph" initial =<< getModificationTime evaluated
  , testCase "revalidates inputs discovered during the first build" $
      withSystemTempDirectory "acton-zig-first-inputs" $ \dir -> do
        zig <- canonicalizePath "../../dist/zig/zig"
        let graph = dir </> "build.zig"
            source = dir </> "probe.c"
            compiled = dir </> "compiled"
            release = dir </> "release"
            binary = dir </> "out" </> "bin" </> "probe"
            cp = (proc zig ["build", "--watch", "--watch-stdin",
                            "--prefix", dir </> "out", "--color", "off"]) { cwd = Just dir }
            checkOutput expected = do
              (result, _, _) <- readCreateProcessWithExitCode (proc binary []) ""
              assertEqual "executable observes compiled code" (ExitFailure expected) result
            awaitCompiled = do
              exists <- doesFileExist compiled
              unless exists (threadDelay 10000 >> awaitCompiled)
        writeFile graph $ unlines
          [ "const std = @import(\"std\");"
          , "pub fn build(b: *std.Build) void {"
          , "    const exe = b.addExecutable(.{ .name = \"probe\", .root_module = b.createModule(.{"
          , "        .target = b.graph.host, .optimize = .Debug, .link_libc = true,"
          , "    }) });"
          , "    exe.root_module.addCSourceFile(.{ .file = b.path(\"probe.c\"), .flags = &.{} });"
          , "    b.installArtifact(exe);"
          , "    const hold = b.addSystemCommand(&.{ \"sh\", \"-c\", \"touch compiled; while [ ! -e release ]; do sleep 0.01; done\" });"
          , "    hold.step.dependOn(&exe.step);"
          , "    b.getInstallStep().dependOn(&hold.step);"
          , "}"
          ]
        writeFile source "int main(void) { return 11; }\n"
        bracket ZigWatch.newSession ZigWatch.stopSession $ \session -> do
          withAsync (ZigWatch.build session 1 cp [graph]) $ \building -> do
            ready <- Timeout.timeout (120 * 1000000) awaitCompiled
            assertEqual "first build reaches the post-compile pause" (Just ()) ready
            writeFile source "int main(void) { return 12; }\n"
            writeFile release ""
            first@(_, _, output) <- wait building
            assertEqual ("first build succeeds:\n" ++ output) ExitSuccess (code first)
          checkOutput 11
          second@(_, _, output) <- ZigWatch.build session 2 cp [graph]
          assertEqual ("second build succeeds:\n" ++ output) ExitSuccess (code second)
          checkOutput 12
  , testCase "restarts native graphs for linked helper files and directories" $
      forM_ [False, True] $ \directoryLink ->
        withSystemTempDirectory "acton-zig-linked-helper" $ \dir -> do
          zig <- canonicalizePath "../../dist/zig/zig"
          let graph = dir </> "build.zig"
              native = dir </> "native"
              config = native </> "config"
              external = dir </> "external"
              helper = external </> "helper.zig"
              evaluated = dir </> "graph-evaluated"
              binary = dir </> "out" </> "bin" </> "probe"
              cp = (proc zig ["build", "--watch", "--watch-stdin",
                              "--prefix", dir </> "out", "--color", "off"]) { cwd = Just dir }
              writeZon path name dependencies = writeFile path $ unlines
                [ ".{ .name = ." ++ name ++ ", .version = \"0.0.0\","
                , "   .fingerprint = " ++ Fingerprint.formatFingerprint
                    (Fingerprint.updateFingerprintPrefix (Fingerprint.fingerprintPrefixForName name) 1) ++ ","
                , "   .dependencies = .{" ++ dependencies ++ "}, .paths = .{\"\"} }"
                ]
          createDirectory native
          createDirectory external
          if directoryLink then do
            createDirectoryLink external config
            createDirectoryLink external (external </> "cycle")
          else do
            createDirectory config
            createFileLink helper (config </> "helper.zig")
          writeFile helper "pub const value = 11;\n"
          writeZon (dir </> "build.zig.zon") "watch_root" ".native = .{ .path = \"native\" }"
          writeZon (native </> "build.zig.zon") "watch_native" ""
          writeFile graph $ unlines
            [ "const std = @import(\"std\");"
            , "pub fn build(b: *std.Build) void {"
            , "    std.Io.Dir.cwd().writeFile(b.graph.io, .{ .sub_path = \"graph-evaluated\", .data = \"loaded\" }) catch @panic(\"write\");"
            , "    b.installArtifact(b.dependency(\"native\", .{}).artifact(\"probe\"));"
            , "}"
            ]
          writeFile (native </> "build.zig") $ unlines
            [ "const std = @import(\"std\");"
            , "const helper = @import(\"config/helper.zig\");"
            , "pub fn build(b: *std.Build) void {"
            , "    const exe = b.addExecutable(.{ .name = \"probe\", .root_module = b.createModule(.{"
            , "        .target = b.graph.host, .optimize = .Debug, .link_libc = true,"
            , "    }) });"
            , "    exe.root_module.addCSourceFile(.{ .file = b.path(\"probe.c\"), .flags = &.{b.fmt(\"-DVALUE={d}\", .{helper.value})} });"
            , "    b.installArtifact(exe);"
            , "}"
            ]
          writeFile (native </> "probe.c") "int main(void) { return VALUE; }\n"
          bracket ZigWatch.newSession ZigWatch.stopSession $ \session -> do
            let build generation expected = do
                  result@(_, _, output) <- ZigWatch.build session generation cp [graph]
                  assertEqual ("Zig build result:\n" ++ output) ExitSuccess (code result)
                  (result, _, _) <- readCreateProcessWithExitCode (proc binary []) ""
                  assertEqual "native build uses current linked helper" (ExitFailure expected) result
            build 1 11
            initial <- getModificationTime evaluated
            build 2 11
            assertEqual "unchanged symlinks retain the graph" initial =<< getModificationTime evaluated
            writeFile helper "pub const value = 12;\n"
            build 3 12
            assertBool "editing the linked helper reloads the graph" . (/= initial) =<< getModificationTime evaluated
  , testCase "tracks native configuration in output and git directories" $
      withNativeGraph $ \session cp graph native evaluated binary -> do
        let build generation expected = do
              result@(_, _, output) <- ZigWatch.build session generation cp [graph]
              assertEqual ("native graph build:\n" ++ output) ExitSuccess (code result)
              (result, _, _) <- readCreateProcessWithExitCode (proc binary []) ""
              assertEqual "native graph sees current configuration" (ExitFailure expected) result
        build 1 11
        initial <- getModificationTime evaluated
        forM_ [2..4] $ \generation -> build generation 11
        assertEqual "generated cache/install writes retain the graph" initial =<< getModificationTime evaluated
        writeFile (native </> ".git" </> "config") "11"
        build 5 14
        configured <- getModificationTime evaluated
        assertBool "git configuration reloads the graph" (initial /= configured)
        forM_ (zip ["out", "zig-out", ".zig-cache"] [6..8]) $ \(directory, generation) -> do
          writeFile (native </> directory </> "helper.zig") "pub const value = 2;\n"
          build generation (generation + 9)
        stable <- getModificationTime evaluated
        build 9 17
        assertEqual "unchanged native inputs keep the latest graph" stable =<< getModificationTime evaluated
  , testCase "rejects graph inputs changed after compiling the runner" $
      withNativeGraph $ \session cp graph native evaluated binary -> do
        original <- canonicalizePath "../../dist/zig/lib/compiler/build_runner.zig"
        let dir = takeDirectory graph
            pausedRunner = dir </> "paused_runner.zig"
            held = dir </> "runner-started"
            release = dir </> "release"
            pause = "    _ = builder.run(&.{\"sh\", \"-c\", \"touch runner-started; while [ ! -e release ]; do sleep 0.01; done\"});\n"
            awaitHeld = do
              exists <- doesFileExist held
              unless exists (threadDelay 10000 >> awaitHeld)
        source <- readFile original
        writeFile pausedRunner (intercalate (pause ++ "    var targets =") (splitOn "    var targets =" source))
        copyFile (takeDirectory original </> "Watch.zig") (dir </> "Watch.zig")
        let paused = cp { cmdspec = case cmdspec cp of
                            RawCommand cmd args -> RawCommand cmd (args ++ ["--build-runner", pausedRunner])
                            other -> other }
        withAsync (ZigWatch.build session 1 paused [graph]) $ \building -> do
          ready <- Timeout.timeout (120 * 1000000) awaitHeld
          assertEqual "compiled runner reaches pause before graph evaluation" (Just ()) ready
          writeFile (native </> "out" </> "helper.zig") "pub const value = 2;\n"
          writeFile (native </> ".git" </> "config") "11"
          writeFile release ""
          result@(_, _, output) <- wait building
          assertEqual ("replacement build:\n" ++ output) ExitSuccess (code result)
        (result, _, _) <- readCreateProcessWithExitCode (proc binary []) ""
        assertEqual "first successful build uses the new compiled helper and configuration" (ExitFailure 15) result
        stable <- getModificationTime evaluated
        next <- ZigWatch.build session 2 paused [graph]
        assertEqual "warm build after startup retry" ExitSuccess (code next)
        assertEqual "replacement graph remains reusable" stable =<< getModificationTime evaluated
  , testCase "rechecks fetched packages and nested path dependencies before reuse" $
      withSystemTempDirectory "acton-zig-fetched-inputs" $ \dir -> do
        let graph = dir </> "build.zig"
            config = dir </> "configuration"
            evaluated = dir </> "evaluated"
            script = dir </> "runner.sh"
            cp = (proc "sh" [script, "--global-cache-dir", "cache"]) { cwd = Just dir }
        createDirectory config
        writeFile graph ""
        writeFile (dir </> "build.zig.zon") ".{ .dependencies = .{ .native = .{ .hash = \"package-hash\", .lazy = true } } }"
        writeFile (config </> "value") "11"
        writeFile script $ unlines
          [ "mkdir -p cache/p/package-hash"
          , "if [ ! -e cache/p/package-hash/build.zig.zon ]; then"
          , "  printf '.{ .dependencies = .{ .configuration = .{ .path = \"../../../configuration\" } } }' > cache/p/package-hash/build.zig.zon"
          , "fi"
          , "printf x >> evaluated"
          , "printf '\\000ACTON_ZIG %s ready 1\\000' \"$ACTON_ZIG_WATCH_TOKEN\""
          , "while read command generation; do"
          , "  cat configuration/value"
          , "  printf '\\000ACTON_ZIG %s done %s ok\\000' \"$ACTON_ZIG_WATCH_TOKEN\" \"$generation\""
          , "done"
          ]
        bracket ZigWatch.newSession ZigWatch.stopSession $ \session -> do
          first <- ZigWatch.build session 1 cp [graph]
          assertEqual "newly materialized package succeeds" ExitSuccess (code first)
          assertEqual "new package requires one startup retry" "xx" =<< readFile evaluated
          _ <- ZigWatch.build session 2 cp [graph]
          assertEqual "unchanged fetched dependency is reusable" "xx" =<< readFile evaluated
          writeFile (config </> "value") "12"
          _ <- ZigWatch.build session 3 cp [graph]
          assertEqual "nested path dependency reloads graph" "xxx" =<< readFile evaluated
          writeFile (config </> "build.zig.zon") "unused lazy manifest that Zig need not read"
          fourth <- ZigWatch.build session 4 cp [graph]
          fifth <- ZigWatch.build session 5 cp [graph]
          assertEqual "incomplete discovery leaves dependency validation to Zig" (ExitSuccess, ExitSuccess) (code fourth, code fifth)
          assertEqual "incomplete discovery does not reuse an unverified graph" "xxxxx" =<< readFile evaluated
          removeFile (config </> "build.zig.zon")
          createDirectory (config </> "build.zig.zon")
          unreadable <- ZigWatch.build session 6 cp [graph]
          assertEqual "unreadable unused manifests leave validation to Zig" ExitSuccess (code unreadable)
          assertEqual "unreadable inputs require a fresh graph" "xxxxxx" =<< readFile evaluated
  , testCase "reuses successful and failed generations and restarts changed graphs" $
      withRunner $ \session cp graph control -> do
        first <- ZigWatch.build session 1 cp [graph]
        assertEqual "initial build" ExitSuccess (code first)
        let (_, _, initialOutput) = first
        assertBool "preserves configuration diagnostics" ("configuration warning" `isInfixOf` initialOutput)
        second <- ZigWatch.build session 2 cp [graph]
        assertEqual "unchanged graph keeps its process" (runner first) (runner second)
        writeFile control "failed"
        failed <- ZigWatch.build session 3 cp [graph]
        assertEqual "build failure is returned" (ExitFailure 1) (code failed)
        writeFile control "ok"
        recovered <- ZigWatch.build session 4 cp [graph]
        assertEqual "failure recovery" ExitSuccess (code recovered)
        assertEqual "failure recovery keeps its process" (runner first) (runner recovered)
        writeFile graph "new graph"
        changed <- ZigWatch.build session 5 cp [graph]
        assertBool "changed graph restarts the process" (runner first /= runner changed)
        ZigWatch.stopSession session
        WatchTests.assertStopped "Zig runner" (read (runner changed))
  , testCase "accepts split frames and preserves UTF-8 and protocol lookalikes" $
      withRunner $ \session cp graph _ -> do
        first <- ZigWatch.build session 1 cp [graph]
        assertEqual "fragmented readiness and completion frames" ExitSuccess (code first)
        let (_, _, output) = first
        assertBool "decodes UTF-8 split across reads" ("configuration warning café" `isInfixOf` output)
        assertBool "ordinary protocol-looking text is a diagnostic"
          ("ACTON_ZIG_DONE 1 ok" `isInfixOf` output)
        assertBool "a frame with a different token is a diagnostic"
          ("\0ACTON_ZIG different-token done 1 ok\0" `isInfixOf` output)
        second <- ZigWatch.build session 2 cp [graph]
        assertEqual "fragmented frames do not restart the runner" (runner first) (runner second)
        let (_, _, nextOutput) = second
        assertBool "previous diagnostics do not leak into the next generation"
          (not ("configuration warning" `isInfixOf` nextOutput))
  , testCase "cancellation stops compiler children and permits the next generation" $
      withRunner $ \session cp graph control -> do
        writeFile control "hold"
        let childFile = takeDirectory control </> "child"
        within $ withAsync (ZigWatch.build session 1 cp [graph]) $ \building -> do
          let waitChild = do
                exists <- doesFileExist childFile
                child <- if exists then readFile childFile else return ""
                if null child then threadDelay 10000 >> waitChild else return child
          child <- waitChild
          cancel building
          WatchTests.assertStopped "Zig descendant" (read child)
        writeFile control "ok"
        recovered <- ZigWatch.build session 2 cp [graph]
        assertEqual "canceled session can restart" ExitSuccess (code recovered)
  , testCase "runner exit stops descendants and preserves buffered responses" $
      forM_ [(False, False), (True, False), (True, True)] $ \(ready, completed) ->
        withSystemTempDirectory "acton-zig-runner-exit" $ \dir -> do
          let script = unlines $
                (if ready then
                  [ "printf '\\000ACTON_ZIG %s ready 1\\000' \"$ACTON_ZIG_WATCH_TOKEN\""
                  , "read command generation"
                  ] else []) ++
                [ "sh -c 'trap \"\" TERM; echo $$ > child; exec sleep 60' &"
                , "while [ ! -s child ]; do sleep 0.01; done"
                , "printf 'runner failed: café ACTON_ZIG_DONE 1 ok'"
                ] ++
                (if completed then ["printf '\\000ACTON_ZIG %s done 1 ok\\000' \"$ACTON_ZIG_WATCH_TOKEN\""] else []) ++
                ["exit 0"]
          bracket ZigWatch.newSession ZigWatch.stopSession $ \session -> do
            result <- Timeout.timeout 3000000 $
              (Right <$> ZigWatch.build session 1 (shell script) { cwd = Just dir } [])
              `catch` \(err :: IOException) -> return (Left (show err))
            message <- case result of
              Nothing -> assertFailure "runner exit should fail promptly despite inherited output pipes"
                           >> return ""
              Just (Left message) -> assertBool "unfinished generation reports runner exit" (not completed) >> return message
              Just (Right (code, _, output)) -> do
                assertBool "only completed generations succeed" completed
                assertEqual "buffered completion frame is preserved" ExitSuccess code
                return output
            assertBool ("retains buffered diagnostics: " ++ message)
              ("runner failed: café ACTON_ZIG_DONE 1 ok" `isInfixOf` message)
            ZigWatch.stopSession session
            child <- readFile (dir </> "child")
            WatchTests.assertStopped "Zig descendant" (read child)
  , testCase "rejects unsupported protocol versions without waiting for a build" $
      bracket ZigWatch.newSession ZigWatch.stopSession $ \session -> do
        result <- within $
          (ZigWatch.build session 1 (shell "printf '\\000ACTON_ZIG %s ready 99\\000' \"$ACTON_ZIG_WATCH_TOKEN\"; sleep 60") [] >> return False)
          `catch` \(_ :: IOException) -> return True
        assertBool "unsupported version should fail" result
  ]
  where
    code (result, _, _) = result
    runner (_, _, output) = head [pid | ["PID", pid] <- map words (lines output)]
    within action = do
      result <- Timeout.timeout 10000000 action
      case result of
        Nothing -> assertFailure "Zig watch operation timed out" >> error "unreachable"
        Just value -> return value
    withNativeGraph action = withSystemTempDirectory "acton-zig-native-inputs" $ \dir -> do
      zig <- canonicalizePath "../../dist/zig/zig"
      let graph = dir </> "build.zig"
          native = dir </> "native"
          evaluated = dir </> "graph-evaluated"
          binary = native </> "out" </> "bin" </> "probe"
          cp = (proc zig ["build", "--watch", "--watch-stdin",
                          "--cache-dir", "native/generated-cache", "--global-cache-dir", "native/generated-global",
                          "--prefix", "native/out", "--color", "off"]) { cwd = Just dir }
          writeZon path name dependencies = writeFile path $ unlines
            [ ".{ .name = ." ++ name ++ ", .version = \"0.0.0\","
            , "   .fingerprint = " ++ Fingerprint.formatFingerprint
                (Fingerprint.updateFingerprintPrefix (Fingerprint.fingerprintPrefixForName name) 1) ++ ","
            , "   .dependencies = .{" ++ dependencies ++ "}, .paths = .{\"\"} }"
            ]
      forM_ ["out", "zig-out", ".zig-cache", ".git"] $ \directory ->
        createDirectoryIfMissing True (native </> directory)
      forM_ ["out", "zig-out", ".zig-cache"] $ \directory ->
        writeFile (native </> directory </> "helper.zig") "pub const value = 1;\n"
      writeFile (native </> ".git" </> "config") "8"
      writeZon (dir </> "build.zig.zon") "watch_root" ".native = .{ .path = \"native\" }"
      writeZon (native </> "build.zig.zon") "watch_native" ""
      writeFile graph $ unlines
        [ "const std = @import(\"std\");"
        , "pub fn build(b: *std.Build) void {"
        , "    std.Io.Dir.cwd().writeFile(b.graph.io, .{ .sub_path = \"graph-evaluated\", .data = \"loaded\" }) catch @panic(\"write\");"
        , "    b.installArtifact(b.dependency(\"native\", .{}).artifact(\"probe\"));"
        , "}"
        ]
      writeFile (native </> "build.zig") $ unlines
        [ "const std = @import(\"std\");"
        , "const helper = @import(\"out/helper.zig\").value + @import(\"zig-out/helper.zig\").value + @import(\".zig-cache/helper.zig\").value;"
        , "pub fn build(b: *std.Build) void {"
        , "    const config = b.build_root.handle.readFileAlloc(b.graph.io, \".git/config\", b.allocator, .limited(16)) catch @panic(\"read\");"
        , "    const value = std.fmt.parseUnsigned(u8, config, 10) catch @panic(\"config\");"
        , "    const exe = b.addExecutable(.{ .name = \"probe\", .root_module = b.createModule(.{ .target = b.graph.host, .optimize = .Debug, .link_libc = true }) });"
        , "    exe.root_module.addCSourceFile(.{ .file = b.path(\"probe.c\"), .flags = &.{b.fmt(\"-DVALUE={d}\", .{helper + value})} });"
        , "    b.installArtifact(exe);"
        , "}"
        ]
      writeFile (native </> "probe.c") "int main(void) { return VALUE; }\n"
      bracket ZigWatch.newSession ZigWatch.stopSession $ \session ->
        action session cp graph native evaluated binary
    withRunner action = withSystemTempDirectory "acton-zig-session" $ \dir -> do
      let script = dir </> "runner.sh"
          graph = dir </> "build.zig"
          control = dir </> "control"
      writeFile graph "initial graph"
      writeFile control "ok"
      writeFile script $ unlines
        [ "frame() {"
        , "  printf '\\000ACT'"
        , "  sleep 0.01"
        , "  printf 'ON_ZIG %s ' \"$ACTON_ZIG_WATCH_TOKEN\""
        , "  sleep 0.01"
        , "  printf '%s' \"$*\""
        , "  sleep 0.01"
        , "  printf '\\000'"
        , "}"
        , "printf 'configuration warning caf\\303'"
        , "sleep 0.01"
        , "printf '\\251 ACTON_ZIG_DONE 1 ok\\000ACTON_ZIG different-token done 1 ok\\000'"
        , "frame ready 1"
        , "while read command generation; do"
        , "  status=$(cat control)"
        , "  case \"$status\" in"
        , "    hold) sh -c 'trap \"\" TERM; echo $$ > child; exec sleep 60' >/dev/null 2>&1 & wait ;;"
        , "  esac"
        , "  printf '\\nPID %s\\n' \"$$\""
        , "  frame done \"$generation\" \"$status\""
        , "done"
        ]
      bracket ZigWatch.newSession ZigWatch.stopSession $ \session ->
        within (action session (proc "sh" [script]) { cwd = Just dir } graph control)
