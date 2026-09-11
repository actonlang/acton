module PerfMemoryTests (perfMemoryTests) where

import System.Directory (canonicalizePath)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Tasty
import Test.Tasty.HUnit

perfMemoryTests :: TestTree
perfMemoryTests = testGroup "live memory observation"
  [ nativeTest "host and process readings" "perf_memory_probe" ["perf_memory.c"]
  , nativeTest "Linux cgroup boundaries" "perf_memory_test" []
  ]
  where
    nativeTest label name extra = testCase label $
      withSystemTempDirectory "acton-perf-memory" $ \tmp -> do
        sources <- canonicalizePath "cbits"
        let binary = tmp </> name
            args = ["-Wall", "-Wextra", "-Werror", "-O2"]
              ++ map (sources </>) ((name ++ ".c") : extra) ++ ["-o", binary]
        (compiled, out, err) <- readProcessWithExitCode "cc" args ""
        assertEqual (out ++ err) ExitSuccess compiled
        (status, stdout, stderr) <- readProcessWithExitCode binary [tmp] ""
        assertEqual (stdout ++ stderr) ExitSuccess status
