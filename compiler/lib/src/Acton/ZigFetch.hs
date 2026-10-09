-- | Fetch package archives into Zig's global cache. Saving the dependency in a
--   temporary project makes Zig preserve the normalized archive root when it
--   recompresses the package for its cache.
module Acton.ZigFetch(fetchZigPackage) where

import qualified Acton.Zon as Zon
import Control.Exception (SomeAsyncException, SomeException, displayException, fromException, throwIO, try)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess(cwd, env), proc, readCreateProcessWithExitCode)

-- | Return the fetched package hash. Local package storage is scoped to the
--   temporary project; only the compressed global cache entry is retained.
fetchZigPackage :: FilePath -> FilePath -> FilePath -> IO (Either String String)
fetchZigPackage zigExe globalCache target = do
    res <- try fetch :: IO (Either SomeException (Either String String))
    case res of
      Left err
        | Just _ <- (fromException err :: Maybe SomeAsyncException) -> throwIO err
        | otherwise -> return (Left (displayException err))
      Right result -> return result
  where
    fetch = do
      createDirectoryIfMissing True (globalCache </> "tmp")
      withSystemTempDirectory "acton-zig-fetch" $ \tmp -> do
        writeFile (tmp </> "build.zig") $ unlines
          [ "const std = @import(\"std\");"
          , "pub fn build(b: *std.Build) void { _ = b; }"
          ]
        env0 <- getEnvironment
        let env1 = [("ZIG_GLOBAL_CACHE_DIR", globalCache), ("ZIG_LOCAL_PKG_DIR", tmp </> "zig-pkg")]
                   ++ filter ((`notElem` ["ZIG_GLOBAL_CACHE_DIR", "ZIG_LOCAL_PKG_DIR"]) . fst) env0
            cmd = (proc zigExe ["fetch", "--save=archive", target]) { cwd = Just tmp, env = Just env1 }
        (code, _, err) <- readCreateProcessWithExitCode cmd ""
        case code of
          ExitFailure _ -> return (Left err)
          ExitSuccess -> do
            edeps <- Zon.readZonDependencies (tmp </> "build.zig.zon")
            return $ case edeps of
              Left e -> Left e
              Right deps -> case lookup "archive" deps >>= Zon.zdHash of
                Just h | not (null h) -> Right h
                _ -> Left "Zig fetch did not save the archive package hash"
