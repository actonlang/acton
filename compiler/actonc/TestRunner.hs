module TestRunner
  ( TestMode(..)
  , listTestModules
  , listProjectTests
  , runProjectTests
  ) where

import qualified Acton.CommandLineParser as C
import Acton.Testing
import Acton.Compile
import TestFormat
import TestUI
import Control.Concurrent (forkIO)
import Control.Concurrent.Async
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Monad
import Data.IORef
import Data.Char (isSpace)
import Data.List (isPrefixOf, isSuffixOf, foldl')
import qualified Data.List
import Data.Maybe (catMaybes, listToMaybe)
import qualified Data.Map as M
import qualified Data.Set as Set
import System.Clock
import System.Directory
import System.Exit
import System.FilePath ((</>), (<.>), joinPath)
import System.IO (hClose, hGetContents, hGetLine, hIsEOF)
import System.Process
import Text.Printf
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKM
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Control.Exception (evaluate, onException)
import Data.Version (showVersion)
import qualified Paths_actonc

data TestMode = TestModeRun | TestModeList | TestModePerf deriving (Eq, Show)

data TestSpec = TestSpec
  { tsModule :: String
  , tsName :: String
  , tsDisplay :: String
  } deriving (Show)

data TestEvent
  = TestEventDone TestResult
  | TestEventRoom

data TestProgressCallbacks = TestProgressCallbacks
  { tpcOnLive :: TestResult -> IO ()
  , tpcOnDone :: TestResult -> IO ()
  , tpcOnFinal :: TestResult -> IO ()
  }

getVer :: String
getVer = showVersion Paths_actonc.version

printErrorAndExit :: String -> IO a
printErrorAndExit msg = do
    errorWithoutStackTrace msg
    exitFailure

moduleHeaderLine :: String -> String
moduleHeaderLine modName
  | null modName = "Tests"
  | otherwise = "Tests - module " ++ modName ++ ":"

-- | List compiled test modules by scanning the output bin directory.
listTestModules :: C.CompileOptions -> Paths -> IO [String]
listTestModules opts paths = do
    let dir = binDir paths
    exists <- doesDirectoryExist dir
    if not exists
      then return []
      else do
        entries <- listDirectory dir
        mods <- forM entries $ \entry -> do
          let base = stripExe entry
          if ".test_" `isPrefixOf` base
            then return (Just (drop (length ".test_") base))
            else return Nothing
        return (Data.List.sort (catMaybes mods))
  where
    stripExe name
      | ".exe" `isSuffixOf` name = take (length name - 4) name
      | otherwise = name

-- | Compute the test binary path for a module and target.
testBinaryPath :: C.CompileOptions -> Paths -> String -> FilePath
testBinaryPath opts paths modName =
    let base = ".test_" ++ modName
        exe = if C.target opts == "x86_64-windows-gnu" then base <.> "exe" else base
    in binDir paths </> exe

-- | List tests for selected modules and print them in a stable order.
listProjectTests :: C.CompileOptions -> Paths -> C.TestOptions -> [String] -> IO ()
listProjectTests opts paths topts modules = do
    let wantedModules = Data.List.sort (filterModules (C.testModules topts) modules)
    tests <- forM wantedModules $ \modName -> do
      names <- listModuleTests opts paths modName
      return (modName, names)
    if null tests
      then do
        putStrLn "No tests found"
        exitSuccess
      else do
        forM_ (Data.List.sortOn fst tests) $ \(modName, names) -> do
          when (not (null names)) $ do
            putStrLn ("Module " ++ modName ++ ":")
            forM_ (Data.List.sort names) $ \name -> do
              let display = displayTestName name
              if display /= name
                then putStrLn ("  " ++ display ++ " (" ++ name ++ ")")
                else putStrLn ("  " ++ display)
            putStrLn ""
        exitSuccess

-- | Run selected tests concurrently, stream results, and return an exit code.
runProjectTests :: Bool -> C.GlobalOptions -> C.CompileOptions -> Paths -> C.TestOptions -> TestMode -> [String] -> Int -> IO Int
runProjectTests useColorOut gopts opts paths topts mode modules maxParallel = do
    timeStart <- getTime Monotonic
    let wantedModules = Data.List.sort (filterModules (C.testModules topts) modules)
    testsByModule <- forM wantedModules $ \modName -> do
      names <- listModuleTests opts paths modName
      let wantedNames = Data.List.sort (filterTests (C.testNames topts) names)
      return (modName, wantedNames)
    let specs =
          [ TestSpec modName testName (displayTestName testName)
          | (modName, names) <- testsByModule
          , testName <- names
          ]
        allTests = [ (tsModule spec, tsName spec) | spec <- specs ]
    if null specs
      then do
        putStrLn "Nothing to test"
        return 0
      else do
        let maxNameLen = maximum (0 : map (length . tsDisplay) specs)
            nameWidth = max 20 (maxNameLen + 5)
            runContext = mkRunContext opts topts mode
            ctxHash = contextHashBytes runContext
        cache <- readTestCache (testCachePath paths) runContext
        testHashInfos <- buildTestHashInfos paths ctxHash testsByModule
        let cacheEntries = tcTests cache
        when (C.verbose gopts) $
          putStrLn (formatTestCacheContext ctxHash (testCachePath paths))
        let logCache = if C.verbose gopts then putStrLn else \_ -> return ()
        (cachedResults, _testsToRun) <- classifyCachedTests logCache cacheEntries testHashInfos allTests
        let showCached = C.testShowCached topts
        when (showCached && not (null cachedResults)) $
          putStrLn ("Using cached results for " ++ show (length cachedResults) ++ " tests")
        ui <- initTestProgressUI gopts nameWidth (C.testShowLog topts) useColorOut
        eventChan <- newChan
        let cachedMap = M.fromList [ (TestKey (trModule res) (trName res), res) | res <- cachedResults ]
            shouldShowCached res =
              let ok = trSuccess res == Just True && trException res == Nothing
              in showCached || not ok
            startSpec spec running results = do
              let key = TestKey (tsModule spec) (tsName spec)
                  display = tsDisplay spec
                  useColorLine = tpuUseColor ui
                  showLog = tpuShowLog ui
              case M.lookup key cachedMap of
                Just cachedRes -> do
                  let line = formatTestLineWith useColorLine formatTestStatus nameWidth display cachedRes
                      details = formatTestDetailLines useColorLine showLog cachedRes
                  if shouldShowCached cachedRes
                    then do
                      ok <- testUiAppendFinal ui key (tsModule spec) line
                      if not ok
                        then return Nothing
                        else do
                          inserted <- testUiInsertDetails ui key details
                          unless inserted $ queuePendingDetails ui key details
                          return (Just (running, cachedRes : results))
                    else return (Just (running, cachedRes : results))
                Nothing -> do
                  if running >= maxParallel
                    then return Nothing
                    else do
                      let initRes = TestResult
                            { trModule = tsModule spec
                            , trName = tsName spec
                            , trComplete = False
                            , trSuccess = Nothing
                            , trException = Nothing
                            , trOutput = Nothing
                            , trStdOut = Nothing
                            , trStdErr = Nothing
                            , trFlaky = False
                            , trNumFailures = 0
                            , trNumErrors = 0
                            , trNumIterations = 0
                            , trTestDuration = 0
                            , trRaw = Aeson.Null
                            , trCached = False
                            }
                          initLine = formatTestLineWith useColorLine formatTestStatusLive nameWidth display initRes
                      started <- testUiStart ui key (tsModule spec) initLine
                      if not started
                        then return Nothing
                        else do
                          let callbacks = testProgressCallbacks ui eventChan key display
                          void $ async $ do
                            res <- runModuleTestStreaming opts paths topts mode (tsModule spec) (tsName spec)
                                    (tpuEnabled ui) callbacks
                            writeChan eventChan (TestEventDone res)
                          return (Just (running + 1, results))
            startAvailable pending running results = do
              case pending of
                [] -> return ([], running, results)
                (spec:rest) -> do
                  mnext <- startSpec spec running results
                  case mnext of
                    Nothing -> return (pending, running, results)
                    Just (running', results') -> startAvailable rest running' results'
            loop pending running results = do
              flushPendingDetails ui
              (pending', running', results') <- startAvailable pending running results
              if null pending' && running' == 0
                then do
                  flushPendingDetails ui
                  return results'
                else do
                  evt <- readChan eventChan
                  case evt of
                    TestEventDone res -> loop pending' (running' - 1) (res : results')
                    TestEventRoom -> loop pending' running' results'
        results <- loop specs 0 []
        timeEnd <- getTime Monotonic
        let resultsRun = filter (not . trCached) results
        when (C.testGoldenUpdate topts) $
          updateGoldenFiles paths resultsRun
        when (C.testRecord topts) $
          writePerfData paths resultsRun
        let cacheEntries' = foldl' (updateTestCacheEntry testHashInfos) cacheEntries resultsRun
            newCache = TestCache
              { tcVersion = testCacheVersion
              , tcContext = runContext
              , tcTests = cacheEntries'
              }
        writeTestCache (testCachePath paths) newCache
        when (not (tpuEnabled ui)) $
          printTestResultsOrdered (tpuUseColor ui) (tpuShowLog ui) showCached nameWidth specs results
        printTestSummary (tpuUseColor ui) (timeEnd - timeStart) showCached results
  where
    mkRunContext opts' topts' mode' = TestRunContext
      { trcCompilerVersion = getVer
      , trcTarget = C.target opts'
      , trcOptimize = show (C.optimize opts')
      , trcMode = show mode'
      , trcArgs = testCmdArgs topts'
      }

-- | Filter module names based on CLI-provided allow lists.
filterModules :: [String] -> [String] -> [String]
filterModules [] mods = mods
filterModules wanted mods = filter (`elem` wanted) mods

-- | Filter test names, matching raw or display names.
filterTests :: [String] -> [String] -> [String]
filterTests [] names = names
filterTests wanted names =
    filter (\name -> name `elem` wanted || displayTestName name `elem` wanted) names

-- | Invoke a module test binary to list tests via JSON output.
listModuleTests :: C.CompileOptions -> Paths -> String -> IO [String]
listModuleTests opts paths modName = do
    let binPath = testBinaryPath opts paths modName
    exists <- doesFileExist binPath
    if not exists
      then return []
      else do
        let args = ["list", "--json"]
        (exitCode, _out, err) <- readProcessWithExitCodeCancelable (proc binPath args){ cwd = Just (projPath paths) }
        case exitCode of
          ExitSuccess -> do
            let values = parseJsonLines err
            return (extractTestList values)
          ExitFailure code ->
            printErrorAndExit ("Listing tests failed for module " ++ modName ++ " (exit " ++ show code ++ ")")

-- | Run a single test case and stream JSON updates.
runModuleTestStreaming :: C.CompileOptions
                       -> Paths
                       -> C.TestOptions
                       -> TestMode
                       -> String
                       -> String
                       -> Bool
                       -> TestProgressCallbacks
                       -> IO TestResult
runModuleTestStreaming opts paths topts mode modName testName allowLive callbacks = do
    let binPath = testBinaryPath opts paths modName
        cmd = ["test", testName] ++ (if mode == TestModePerf then ["perf"] else []) ++ testCmdArgs topts
    updatesRef <- newIORef []
    lineDoneRef <- newIORef False
    stdErrRef <- newIORef []
    let onUpdate res = do
          modifyIORef' updatesRef (\xs -> xs ++ [res])
          done <- readIORef lineDoneRef
          when (not done && allowLive) $ do
            if trComplete res
              then do
                tpcOnDone callbacks res
                writeIORef lineDoneRef True
              else tpcOnLive callbacks res
        addStdErr line = modifyIORef' stdErrRef (line :)
        onErrLine line =
          case parseJsonLine line of
            Nothing -> addStdErr line
            Just val -> case parseTestInfo val of
                          Just res -> onUpdate res
                          Nothing -> addStdErr line
    (exitCode, out, _err) <- readProcessWithExitCodeStreaming (proc binPath cmd){ cwd = Just (projPath paths) } onErrLine
    infos <- readIORef updatesRef
    stdErrLines <- reverse <$> readIORef stdErrRef
    let stdErrText = unlines stdErrLines
    let final = pickFinalTestInfo infos
        fallback = TestResult
          { trModule = modName
          , trName = testName
          , trComplete = False
          , trSuccess = Nothing
          , trException = Just "No test result received"
          , trOutput = Nothing
          , trStdOut = Nothing
          , trStdErr = Nothing
          , trFlaky = False
          , trNumFailures = 0
          , trNumErrors = 1
          , trNumIterations = 0
          , trTestDuration = 0
          , trRaw = Aeson.Null
          , trCached = False
          }
        res0 = maybe fallback id final
        mergedStd field captured =
          case field of
            Just txt | not (null txt) -> Just txt
            _ ->
              if null captured
                then field
                else Just captured
        res1 = res0
          { trStdOut = mergedStd (trStdOut res0) out
          , trStdErr = mergedStd (trStdErr res0) stdErrText
          }
        res = case exitCode of
                ExitSuccess -> res1
                ExitFailure code ->
                  res1 { trException = Just ("Test process exited with code " ++ show code) }
    done <- readIORef lineDoneRef
    if done
      then tpcOnFinal callbacks res
      else do
        tpcOnDone callbacks res
        tpcOnFinal callbacks res
    return res

testProgressCallbacks :: TestProgressUI -> Chan TestEvent -> TestKey -> String -> TestProgressCallbacks
testProgressCallbacks ui eventChan key display =
    let nameWidth = tpuNameWidth ui
        useColorOut = tpuUseColor ui
        showLog = tpuShowLog ui
        liveLine res = formatTestLineWith useColorOut formatTestStatusLive nameWidth display res
        finalLine res = formatTestLineWith useColorOut formatTestStatus nameWidth display res
        detailLines res = formatTestDetailLines useColorOut showLog res
    in TestProgressCallbacks
      { tpcOnLive = \res -> testUiUpdateLive ui key (liveLine res)
      , tpcOnDone = \res -> do
          removed <- testUiFinalize ui key (finalLine res)
          when removed $
            writeChan eventChan TestEventRoom
      , tpcOnFinal = \res -> do
          testUiUpdateFinal ui key (finalLine res)
          let details = detailLines res
          inserted <- testUiInsertDetails ui key details
          unless inserted $
            queuePendingDetails ui key details
      }

-- | Build test runner arguments from TestOptions limits.
testCmdArgs :: C.TestOptions -> [String]
testCmdArgs topts =
    let iter = C.testIter topts
    in if iter > 0
         then ["--max-iter", show iter, "--min-iter", show iter, "--max-time", show (10^6), "--min-time", "1"]
         else [ "--max-iter", show (C.testMaxIter topts)
              , "--min-iter", show (C.testMinIter topts)
              , "--max-time", show (C.testMaxTime topts)
              , "--min-time", show (C.testMinTime topts)
              ]

-- | Normalize test names by stripping prefixes and wrappers.
displayTestName :: String -> String
displayTestName name =
    let withoutPrefix =
          if "_test_" `isPrefixOf` name
            then drop 6 name
            else name
    in if "_wrapper" `isSuffixOf` withoutPrefix
         then take (length withoutPrefix - length "_wrapper") withoutPrefix
         else withoutPrefix

-- | Parse a single JSON line emitted by test binaries.
parseJsonLine :: String -> Maybe Aeson.Value
parseJsonLine line =
    let trimmed = dropWhile isSpace line
    in if null trimmed
         then Nothing
         else Aeson.decodeStrict' (TE.encodeUtf8 (T.pack trimmed))

-- | Parse newline-delimited JSON emitted by test binaries.
parseJsonLines :: String -> [Aeson.Value]
parseJsonLines txt =
    [ v
    | line <- lines txt
    , Just v <- [parseJsonLine line]
    ]

-- | Extract test names from a JSON list payload.
extractTestList :: [Aeson.Value] -> [String]
extractTestList values =
    case listToMaybe [ obj | Aeson.Object obj <- values, AesonKM.member (AesonKey.fromString "tests") obj ] of
      Just obj ->
        case AesonKM.lookup (AesonKey.fromString "tests") obj of
          Just (Aeson.Object testsObj) -> map AesonKey.toString (AesonKM.keys testsObj)
          _ -> []
      Nothing -> []

-- | Extract test result payloads from JSON events.
extractTestInfo :: [Aeson.Value] -> [TestResult]
extractTestInfo values =
    catMaybes (map parseTestInfo values)

-- | Parse a JSON value into a TestResult when test_info is present.
parseTestInfo :: Aeson.Value -> Maybe TestResult
parseTestInfo val =
    case val of
      Aeson.Object obj ->
        case AesonKM.lookup (AesonKey.fromString "test_info") obj of
          Just infoVal -> AesonTypes.parseMaybe parseTestInfoValue infoVal
          Nothing -> Nothing
      _ -> Nothing

-- | Aeson parser for the test_info object.
parseTestInfoValue :: Aeson.Value -> AesonTypes.Parser TestResult
parseTestInfoValue = Aeson.withObject "TestInfo" $ \o -> do
    def <- o Aeson..: AesonKey.fromString "definition"
    moduleName <- def Aeson..: AesonKey.fromString "module"
    name <- def Aeson..: AesonKey.fromString "name"
    complete <- o Aeson..: AesonKey.fromString "complete"
    success <- o Aeson..:? AesonKey.fromString "success"
    exception <- o Aeson..:? AesonKey.fromString "exception"
    output <- o Aeson..:? AesonKey.fromString "output"
    stdOut <- o Aeson..:? AesonKey.fromString "std_out"
    stdErr <- o Aeson..:? AesonKey.fromString "std_err"
    flaky <- o Aeson..:? AesonKey.fromString "flaky" Aeson..!= False
    numFailures <- o Aeson..:? AesonKey.fromString "num_failures" Aeson..!= 0
    numErrors <- o Aeson..:? AesonKey.fromString "num_errors" Aeson..!= 0
    numIterations <- o Aeson..:? AesonKey.fromString "num_iterations" Aeson..!= 0
    testDuration <- o Aeson..:? AesonKey.fromString "test_duration" Aeson..!= 0
    return TestResult
      { trModule = moduleName
      , trName = name
      , trComplete = complete
      , trSuccess = success
      , trException = exception
      , trOutput = output
      , trStdOut = stdOut
      , trStdErr = stdErr
      , trFlaky = flaky
      , trNumFailures = numFailures
      , trNumErrors = numErrors
      , trNumIterations = numIterations
      , trTestDuration = testDuration
      , trRaw = Aeson.Object o
      , trCached = False
      }

-- | Pick the final or last-seen TestResult from a stream.
pickFinalTestInfo :: [TestResult] -> Maybe TestResult
pickFinalTestInfo infos =
    case reverse infos of
      [] -> Nothing
      xs ->
        case listToMaybe [i | i <- xs, trComplete i] of
          Just i -> Just i
          Nothing -> Just (head xs)

-- | Print a summary line and return the failure/error exit code.
printTestSummary :: Bool -> TimeSpec -> Bool -> [TestResult] -> IO Int
printTestSummary useColor elapsed showCached results = do
    let total = length results
        failures = length [ r | r <- results, trSuccess r == Just False ]
        errors = length [ r | r <- results, trSuccess r == Nothing ]
        hiddenCachedSuccess = not showCached && any (\r -> trCached r && trSuccess r == Just True) results
        hasCached = any trCached results
    case total of
      0 -> do
        putStrLn "Nothing to test"
        return 0
      _ -> do
        putStrLn ""
        if errors > 0 && failures > 0
          then putStrLn (testColorApply useColor [testColorBold, testColorRed] (show errors ++ " error and " ++ show failures ++ " failure out of " ++ show total ++ " tests (" ++ fmtTime elapsed ++ ")"))
          else if errors > 0
            then putStrLn (testColorApply useColor [testColorBold, testColorRed] (show errors ++ " out of " ++ show total ++ " tests errored (" ++ fmtTime elapsed ++ ")"))
            else if failures > 0
              then putStrLn (testColorApply useColor [testColorBold, testColorRed] (show failures ++ " out of " ++ show total ++ " tests failed (" ++ fmtTime elapsed ++ ")"))
              else putStrLn (testColorApply useColor [testColorGreen] ("All " ++ show total ++ " tests passed (" ++ fmtTime elapsed ++ ")"))
        putStrLn ""
        when hasCached $
          putStrLn (if useColor then testColorYellow ++ "*" ++ testColorReset ++ " = cached test result" else "* = cached test result")
        when hiddenCachedSuccess $
          putStrLn "Cached succeesful tests are hidden. Cached failures/errors are shown. Use --show-cached to include cached successes."
        if errors > 0
          then return 2
          else if failures > 0
            then return 1
            else return 0

printTestResultsOrdered :: Bool -> Bool -> Bool -> Int -> [TestSpec] -> [TestResult] -> IO ()
printTestResultsOrdered useColor showLog showCached nameWidth specs results = do
    let resMap = M.fromList [ (TestKey (trModule res) (trName res), res) | res <- results ]
        isOk res = trSuccess res == Just True && trException res == Nothing
        shouldShow res = not (trCached res) || showCached || not (isOk res)
        formatLine spec res =
          formatTestLineWith useColor formatTestStatus nameWidth (tsDisplay spec) res
    let go _ _ [] = return ()
        go printedMods printedAny (spec:rest) =
          case M.lookup (TestKey (tsModule spec) (tsName spec)) resMap of
            Nothing -> go printedMods printedAny rest
            Just res ->
              if not (shouldShow res)
                then go printedMods printedAny rest
                else do
                  let modName = tsModule spec
                  printedMods' <-
                    if Set.member modName printedMods
                      then return printedMods
                      else do
                        when printedAny $ putStrLn ""
                        putStrLn (moduleHeaderLine modName)
                        return (Set.insert modName printedMods)
                  putStrLn (formatLine spec res)
                  mapM_ putStrLn (formatTestDetailLines useColor showLog res)
                  go printedMods' True rest
    go Set.empty False specs

-- | Update golden files from NotEqualError outputs.
updateGoldenFiles :: Paths -> [TestResult] -> IO ()
updateGoldenFiles paths results =
    forM_ results $ \res -> do
      case (trException res, trOutput res) of
        (Just exc, Just out)
          | "testing.NotEqualError: Test output does not match expected golden value" `isPrefixOf` exc -> do
              let fileName = displayTestName (trName res)
                  goldenDir = joinPath [projPath paths, "test", "golden", trModule res]
              createDirectoryIfMissing True goldenDir
              writeFile (goldenDir </> fileName) out
        _ -> return ()

-- | Write perf data JSON for the current test run.
writePerfData :: Paths -> [TestResult] -> IO ()
writePerfData paths results = do
    let addTest acc res =
          let modKey = AesonKey.fromString (trModule res)
              testKey = AesonKey.fromString (trName res)
              entry = case AesonKM.lookup modKey acc of
                        Just (Aeson.Object obj) -> obj
                        _ -> AesonKM.empty
              entry' = AesonKM.insert testKey (trRaw res) entry
              acc' = AesonKM.insert modKey (Aeson.Object entry') acc
          in acc'
        modulesObj = foldl' addTest AesonKM.empty results
        outVal = Aeson.Object modulesObj
        outPath = joinPath [projPath paths, "perf_data"]
    BL.writeFile outPath (Aeson.encode outVal)

-- | Run a process and stream stderr lines to a callback while capturing output.
readProcessWithExitCodeStreaming :: CreateProcess -> (String -> IO ()) -> IO (ExitCode, String, String)
readProcessWithExitCodeStreaming cp onErrLine = do
    let cp' = cp { std_in = NoStream, std_out = CreatePipe, std_err = CreatePipe }
    withCreateProcess cp' $ \_ mOut mErr ph -> do
      outVar <- newEmptyMVar
      errVar <- newEmptyMVar
      let readStdout mH var =
            case mH of
              Nothing -> putMVar var ""
              Just h -> do
                txt <- hGetContents h
                _ <- evaluate (length txt)
                hClose h
                putMVar var txt
          readStderr mH var =
            case mH of
              Nothing -> putMVar var ""
              Just h -> do
                txt <- readErrLines h
                hClose h
                putMVar var txt
          readErrLines h = go []
            where
              go acc = do
                eof <- hIsEOF h
                if eof
                  then return (unlines (reverse acc))
                  else do
                    line <- hGetLine h
                    onErrLine line
                    go (line : acc)
      _ <- forkIO $ readStdout mOut outVar
      _ <- forkIO $ readStderr mErr errVar
      code <- waitForProcess ph `onException` do
                terminateProcess ph
                void (waitForProcess ph)
      out <- takeMVar outVar
      err <- takeMVar errVar
      return (code, out, err)

-- | Run a process and capture output, canceling on exceptions.
readProcessWithExitCodeCancelable :: CreateProcess -> IO (ExitCode, String, String)
readProcessWithExitCodeCancelable cp = do
    let cp' = cp { std_in = NoStream, std_out = CreatePipe, std_err = CreatePipe }
    withCreateProcess cp' $ \_ mOut mErr ph -> do
      outVar <- newEmptyMVar
      errVar <- newEmptyMVar
      let readHandle mH var =
            case mH of
              Nothing -> putMVar var ""
              Just h -> do
                txt <- hGetContents h
                _ <- evaluate (length txt)
                hClose h
                putMVar var txt
      _ <- forkIO $ readHandle mOut outVar
      _ <- forkIO $ readHandle mErr errVar
      code <- waitForProcess ph `onException` do
                terminateProcess ph
                void (waitForProcess ph)
      out <- takeMVar outVar
      err <- takeMVar errVar
      return (code, out, err)

fmtTime :: TimeSpec -> String
fmtTime t =
    printf "%6.3f s" secs
  where
    secs :: Float
    secs = (fromIntegral (sec t)) + (fromIntegral (nsec t) / 1000000000)
