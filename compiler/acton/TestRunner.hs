module TestRunner
  ( TestMode(..)
  , listTestModules
  , listProjectTests
  , runProjectTests
  ) where

import qualified Acton.CommandLineParser as C
import Acton.Testing
import Acton.Compile
import qualified Acton.Syntax as A
import qualified InterfaceFiles
import TestFormat
import TestUI
import Control.Concurrent (forkIO)
import Control.Concurrent.Async
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Monad
import Data.IORef
import Data.Char (isSpace)
import Data.List (isPrefixOf, isSuffixOf, foldl', isInfixOf, intercalate)
import qualified Data.List
import Data.Maybe (catMaybes, listToMaybe, isJust)
import qualified Data.Map as M
import qualified Data.Set as Set
import System.Clock
import System.Directory
import System.Exit
import System.FilePath ((</>), (<.>), joinPath, takeExtension)
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
import Control.Exception (SomeException, displayException, evaluate, onException, try)
import qualified Text.Regex.TDFA as TDFA
import Data.Version (showVersion)
import qualified Paths_acton

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
getVer = showVersion Paths_acton.version

printErrorAndExit :: String -> IO a
printErrorAndExit msg = do
    errorWithoutStackTrace msg
    exitFailure

moduleHeaderLine :: String -> String
moduleHeaderLine modName
  | null modName = "Tests"
  | otherwise = "Tests - module " ++ modName ++ ":"

-- | List test modules by reading discovered tests from .ty headers.
listTestModules :: C.CompileOptions -> Paths -> IO [String]
listTestModules _opts paths = do
    srcFiles <- listActFilesRecursive (srcDir paths)
    mods <- forM srcFiles $ \file -> do
      mn <- moduleNameFromFile (srcDir paths) file
      tests <- readModuleTests paths mn
      return $ if null tests then Nothing else Just (modNameToString mn)
    return (Data.List.sort (catMaybes mods))

-- | Compute the test binary path for a module and target.
testBinaryPath :: C.CompileOptions -> Paths -> String -> FilePath
testBinaryPath opts paths modName =
    let base = ".test_" ++ modName
        exe = if isWindowsTarget (C.target opts) then base <.> "exe" else base
    in binDir paths </> exe

-- | Check whether a target triple refers to Windows.
isWindowsTarget :: String -> Bool
isWindowsTarget targetTriple =
    case break (== '-') targetTriple of
      (_, "") -> False
      (_, '-' : rest) ->
        let (os, _) = break (== '-') rest
        in os == "windows"

-- | List tests for selected modules and print them in a stable order.
listProjectTests :: C.CompileOptions -> Paths -> C.TestOptions -> [String] -> IO ()
listProjectTests opts paths topts modules = do
    let wantedModules = Data.List.sort (filterModules (C.testModules topts) modules)
    nameRegexes <- compileTestNameRegexes (C.testNames topts)
    tests <- forM wantedModules $ \modName -> do
      names <- listModuleTests opts paths modName
      return (modName, Data.List.sort (filterTests nameRegexes names))
    let nonEmpty = [ (modName, names) | (modName, names) <- tests, not (null names) ]
    if C.testJson topts
      then do
        let testObj raw =
              let display = displayTestName raw
              in Aeson.object
                   [ AesonKey.fromString "name" Aeson..= display
                   , AesonKey.fromString "raw_name" Aeson..= raw
                   ]
            moduleObj (modName, names) =
              Aeson.object
                [ AesonKey.fromString "name" Aeson..= modName
                , AesonKey.fromString "tests" Aeson..= map testObj names
                ]
            report = Aeson.object
              [ AesonKey.fromString "modules" Aeson..= map moduleObj (Data.List.sortOn fst nonEmpty) ]
        BL.putStr (Aeson.encode report)
        putStrLn ""
        exitSuccess
      else if null nonEmpty
        then do
          putStrLn "No tests found"
          exitSuccess
        else do
          forM_ (Data.List.sortOn fst nonEmpty) $ \(modName, names) -> do
            putStrLn ("Module " ++ modName ++ ":")
            forM_ names $ \name -> do
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
    let emitJson = C.testJson topts
    nameRegexes <- compileTestNameRegexes (C.testNames topts)
    let wantedModules = Data.List.sort (filterModules (C.testModules topts) modules)
    testsByModule <- forM wantedModules $ \modName -> do
      names <- listModuleTests opts paths modName
      let wantedNames = Data.List.sort (filterTests nameRegexes names)
      return (modName, wantedNames)
    let specs =
          [ TestSpec modName testName (displayTestName testName)
          | (modName, names) <- testsByModule
          , testName <- names
          ]
        allTests = [ (tsModule spec, tsName spec) | spec <- specs ]
    if null specs
      then do
        if emitJson
          then do
            timeEnd <- getTime Monotonic
            outputJsonReport (timeEnd - timeStart) []
            return 0
          else do
            putStrLn "Nothing to test"
            return 0
      else do
        let maxNameLen = maximum (0 : map (length . tsDisplay) specs)
            nameWidth = max 20 (maxNameLen + 5)
            runContext = mkRunContext opts topts mode
            ctxHash = contextHashBytes runContext
            useCache = not (C.testNoCache topts)
        cache <- readTestCache (testCachePath paths) runContext
        testHashInfos <- buildTestHashInfos paths ctxHash testsByModule
        let cacheEntries = tcTests cache
        when (C.verbose gopts) $
          putStrLn (formatTestCacheContext ctxHash (testCachePath paths))
        let logCache = if C.verbose gopts then putStrLn else \_ -> return ()
        cachedResults0 <-
          if useCache
            then do
              (cachedResults, _testsToRun) <-
                classifyCachedTests logCache cacheEntries testHashInfos allTests
              return cachedResults
            else return []
        cachedResults <-
          if C.testSnapshotUpdate topts
            then mapM (applySnapshotUpdate paths) cachedResults0
            else return cachedResults0
        let showCached = C.testShowCached topts
        when (not emitJson && not useCache) $
          putStrLn "Skipping test result cache (--no-cache); running all selected tests"
        when (not emitJson && showCached && not (null cachedResults)) $
          putStrLn ("Using cached results for " ++ show (length cachedResults) ++ " tests")
        ui <- initTestProgressUI gopts nameWidth (C.testShowLog topts) useColorOut
        let totalTests = length specs
        progressDoneRef <- newIORef 0
        let progressStep = do
              done <- atomicModifyIORef' progressDoneRef (\x -> let x' = x + 1 in (x', x'))
              let pct =
                    if totalTests <= 0
                      then 100
                      else min 100 ((done * 100) `div` totalTests)
              testUiProgressPercent ui pct
        testUiProgressPercent ui 0
        eventChan <- newChan
        let cachedMap = M.fromList [ (TestKey (trModule res) (trName res), res) | res <- cachedResults ]
            shouldShowCached res =
              let ok = trSuccess res == Just True && trException res == Nothing && not (trSkipped res)
              in showCached || not ok || trSnapshotUpdated res
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
                          progressStep
                          return (Just (running, cachedRes : results))
                    else do
                      progressStep
                      return (Just (running, cachedRes : results))
                Nothing -> do
                  if running >= maxParallel
                    then return Nothing
                    else do
                      let initRes = TestResult
                            { trModule = tsModule spec
                            , trName = tsName spec
                            , trComplete = False
                            , trSuccess = Nothing
                            , trSkipped = False
                            , trSkipReason = Nothing
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
                            , trSnapshotUpdated = False
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
                    TestEventDone res -> do
                      progressStep
                      loop pending' (running' - 1) (res : results')
                    TestEventRoom -> loop pending' running' results'
        results <- loop specs 0 []
        timeEnd <- getTime Monotonic
        writeSnapshotOutputs paths results
        let resultsRun =
              if C.testSnapshotUpdate topts
                then filter (\r -> not (trCached r) || trSnapshotUpdated r) results
                else filter (not . trCached) results
        when (C.testRecord topts) $
          writePerfData paths resultsRun
        let cacheEntries' = foldl' (updateTestCacheEntry testHashInfos) cacheEntries resultsRun
            newCache = TestCache
              { tcVersion = testCacheVersion
              , tcContext = runContext
              , tcTests = cacheEntries'
              }
        writeTestCache (testCachePath paths) newCache
        if emitJson
          then do
            outputJsonReport (timeEnd - timeStart) results
            testUiProgressClear ui
            return (testExitCode results)
          else do
            when (not (tpuEnabled ui)) $
              printTestResultsOrdered (tpuUseColor ui) (tpuShowLog ui) showCached nameWidth specs results
            _ <- printTestSummary (tpuUseColor ui) (timeEnd - timeStart) showCached results
            testUiProgressClear ui
            return (testExitCode results)
  where
    mkRunContext opts' topts' mode' = TestRunContext
      { trcCompilerVersion = getVer
      , trcTarget = C.target opts'
      , trcOptimize = show (C.optimize opts')
      , trcMode = show mode'
      , trcArgs = testCmdArgs topts'
      }

testExitCode :: [TestResult] -> Int
testExitCode results =
    let failures = length [ r | r <- results, trSuccess r == Just False ]
        errors = length [ r | r <- results, trSuccess r == Nothing ]
    in if errors > 0 then 2 else if failures > 0 then 1 else 0

outputJsonReport :: TimeSpec -> [TestResult] -> IO ()
outputJsonReport elapsed results = do
    let total = length results
        failures = length [ r | r <- results, trSuccess r == Just False ]
        errors = length [ r | r <- results, trSuccess r == Nothing ]
        skipped = length [ r | r <- results, trSkipped r ]
        elapsedMs :: Double
        elapsedMs =
          let secs :: Double
              secs = (fromIntegral (sec elapsed)) + (fromIntegral (nsec elapsed) / 1000000000)
          in secs * 1000
        isOk res = trSuccess res == Just True && trException res == Nothing && not (trSkipped res)
        formatCombinedOutput mOut mErr =
          let out = maybe "" id mOut
              err = maybe "" id mErr
          in if not (testOutputMeaningful out) && not (testOutputMeaningful err)
                then Nothing
                else
                  let chunks = dedupCombinedOutput out err
                      multi = length chunks > 1
                      rendered = concatMap (renderChunk multi) chunks
                      rendered' = stripTrailingBlanks rendered
                      joined = unlines rendered'
                  in if null (trim joined) then Nothing else Just joined
        testObj res =
          let status = formatTestStatus res
              includeOutput = not (isOk res)
              name = displayTestName (trName res)
              combinedOutput = if includeOutput then formatCombinedOutput (trStdOut res) (trStdErr res) else Nothing
          in Aeson.object
               [ AesonKey.fromString "module" Aeson..= trModule res
               , AesonKey.fromString "name" Aeson..= name
               , AesonKey.fromString "raw_name" Aeson..= trName res
               , AesonKey.fromString "status" Aeson..= status
               , AesonKey.fromString "cached" Aeson..= trCached res
               , AesonKey.fromString "flaky" Aeson..= trFlaky res
               , AesonKey.fromString "iterations" Aeson..= trNumIterations res
               , AesonKey.fromString "duration_ms" Aeson..= trTestDuration res
               , AesonKey.fromString "skipped" Aeson..= trSkipped res
               , AesonKey.fromString "skip_reason" Aeson..= trSkipReason res
               , AesonKey.fromString "exception" Aeson..= trException res
               , AesonKey.fromString "output" Aeson..= combinedOutput
               ]
        report = Aeson.object
          [ AesonKey.fromString "summary" Aeson..= Aeson.object
              [ AesonKey.fromString "total" Aeson..= total
              , AesonKey.fromString "failures" Aeson..= failures
              , AesonKey.fromString "errors" Aeson..= errors
              , AesonKey.fromString "skipped" Aeson..= skipped
              , AesonKey.fromString "elapsed_ms" Aeson..= elapsedMs
              ]
          , AesonKey.fromString "tests" Aeson..= map testObj results
          ]
    BL.putStr (Aeson.encode report)
    putStrLn ""

renderChunk :: Bool -> (String, Int) -> [String]
renderChunk multi (chunk, count) =
    let header =
          if multi
            then ["== " ++ show count ++ " test runs with this output:"]
            else []
        body = lines chunk
    in header ++ body ++ [""]

stripTrailingBlanks :: [String] -> [String]
stripTrailingBlanks = reverse . dropWhile null . reverse

testOutputMeaningful :: String -> Bool
testOutputMeaningful msgs =
    any (\line -> not (all isSpace line) && not ("== Running test," `isPrefixOf` line)) (lines msgs)

splitTestOutput :: String -> [String]
splitTestOutput buf =
    let ls = lines buf
        isMarker line = "== Running test, iteration:" `isInfixOf` stripAnsi (trim line)
        step (chunks, current, seenMarker) line
          | isMarker line =
              if seenMarker
                then (chunks ++ [trim current], "", True)
                else (chunks, "", True)
          | otherwise =
              let current' = if null current then line else current ++ "\n" ++ line
              in (chunks, current', seenMarker)
        (chunks0, current0, seenMarker) = foldl' step ([], "", False) ls
        chunks1 =
          if seenMarker
            then chunks0 ++ [trim current0]
            else if null (trim buf) then [] else [trim buf]
    in chunks1

renderIterationOutput :: String -> String -> String
renderIterationOutput out err =
    let out' = trim out
        err' = trim err
        renderSection label content =
          let body = intercalate "\n" (map ("  " ++) (lines content))
          in label ++ ":\n" ++ body
        parts = catMaybes
          [ if null out' then Nothing else Just (renderSection "STDOUT" out')
          , if null err' then Nothing else Just (renderSection "STDERR" err')
          ]
    in intercalate "\n" parts

dedupCombinedOutput :: String -> String -> [(String, Int)]
dedupCombinedOutput out err =
    let outChunks = splitTestOutput out
        errChunks = splitTestOutput err
        n = max (length outChunks) (length errChunks)
        getChunk xs i = if i < length xs then xs !! i else ""
        combined = [ renderIterationOutput (getChunk outChunks i) (getChunk errChunks i) | i <- [0..n-1] ]
        parts = filter (not . null . trim) combined
        stepCount (order, acc) chunk =
          let acc' = M.insertWith (+) chunk 1 acc
              order' = if M.member chunk acc then order else order ++ [chunk]
          in (order', acc')
        (order, acc) = foldl' stepCount ([], M.empty) parts
    in [ (chunk, M.findWithDefault 0 chunk acc) | chunk <- order ]

trim :: String -> String
trim s =
    let dropEnd = reverse . dropWhile isSpace . reverse
    in dropWhile isSpace (dropEnd s)

stripAnsi :: String -> String
stripAnsi [] = []
stripAnsi ('\ESC':'[':xs) = stripAnsi (dropAnsi xs)
stripAnsi (x:xs) = x : stripAnsi xs

dropAnsi :: String -> String
dropAnsi [] = []
dropAnsi (c:cs)
  | c == 'm' = cs
  | otherwise = dropAnsi cs

-- | Filter module names based on CLI-provided allow lists.
filterModules :: [String] -> [String] -> [String]
filterModules [] mods = mods
filterModules wanted mods = filter (`elem` wanted) mods

-- | Filter test names, matching raw or display names.
filterTests :: [TDFA.Regex] -> [String] -> [String]
filterTests [] names = names
filterTests regexes names =
    filter matches names
  where
    matches name =
      let display = displayTestName name
      in any (\re -> regexMatches re name || regexMatches re display) regexes

compileTestNameRegexes :: [String] -> IO [TDFA.Regex]
compileTestNameRegexes patterns =
    mapM compileRegex (filter (not . null) patterns)
  where
    compileRegex pattern = do
      let anchored = "^" ++ pattern ++ "$"
      res <- try (evaluate (TDFA.makeRegex anchored :: TDFA.Regex)) :: IO (Either SomeException TDFA.Regex)
      case res of
        Left err -> printErrorAndExit ("ERROR: Invalid regex '" ++ pattern ++ "': " ++ displayException err)
        Right re -> return re

regexMatches :: TDFA.Regex -> String -> Bool
regexMatches re text = isJust (TDFA.matchOnceText re text)

-- | Read the discovered tests for a module from its .ty header.
listModuleTests :: C.CompileOptions -> Paths -> String -> IO [String]
listModuleTests _opts paths modName =
    readModuleTests paths (modNameFromString modName)

-- | Read tests from a module's .ty header, returning [] on any error.
readModuleTests :: Paths -> A.ModName -> IO [String]
readModuleTests paths mn = do
    let tyFile = outBase paths mn ++ ".ty"
    exists <- doesFileExist tyFile
    if not exists
      then return []
      else do
        hdrE <- (try :: IO a -> IO (Either SomeException a)) $ InterfaceFiles.readHeader tyFile
        case hdrE of
          Left _ -> return []
          Right (_srcH, _ih, _implH, _imps, _nameHashes, _roots, tests, _doc) ->
            return tests

modNameFromString :: String -> A.ModName
modNameFromString s = A.modName (splitOnChar '.' s)

splitOnChar :: Char -> String -> [String]
splitOnChar ch input = case break (== ch) input of
  (chunk, []) -> [chunk]
  (chunk, _ : rest) -> chunk : splitOnChar ch rest

listActFilesRecursive :: FilePath -> IO [FilePath]
listActFilesRecursive dir = do
    exists <- doesDirectoryExist dir
    if not exists
      then return []
      else do
        entries <- listDirectory dir
        paths <- forM entries $ \entry -> do
          let path = dir </> entry
          isDir <- doesDirectoryExist path
          if isDir
            then listActFilesRecursive path
            else return [path]
        return (filter (\f -> takeExtension f == ".act") (concat paths))

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
          , trSkipped = False
          , trSkipReason = Nothing
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
          , trSnapshotUpdated = False
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
    res' <-
      if C.testSnapshotUpdate topts
        then applySnapshotUpdate paths res
        else return res
    done <- readIORef lineDoneRef
    if done
      then tpcOnFinal callbacks res'
      else do
        tpcOnDone callbacks res'
        tpcOnFinal callbacks res'
    return res'

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
        baseArgs =
          if iter > 0
            then ["--max-iter", show iter, "--min-iter", show iter, "--max-time", show (10^6), "--min-time", "1"]
            else [ "--max-iter", show (C.testMaxIter topts)
                 , "--min-iter", show (C.testMinIter topts)
                 , "--max-time", show (C.testMaxTime topts)
                 , "--min-time", show (C.testMinTime topts)
                 ]
        tagArgs = concatMap (\tag -> ["--tag", tag]) (C.testTags topts)
    in baseArgs ++ tagArgs

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
    skipped <- o Aeson..:? AesonKey.fromString "skipped" Aeson..!= False
    skipReason <- o Aeson..:? AesonKey.fromString "skip_reason"
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
      , trSkipped = skipped
      , trSkipReason = skipReason
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
      , trSnapshotUpdated = False
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
        skipped = length [ r | r <- results, trSkipped r ]
        hiddenCachedSuccess = not showCached && any (\r -> trCached r && trSuccess r == Just True && not (trSkipped r)) results
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
              else if skipped > 0
                then putStrLn (testColorApply useColor [testColorGreen] ("All " ++ show total ++ " tests passed, " ++ show skipped ++ " skipped (" ++ fmtTime elapsed ++ ")"))
                else putStrLn (testColorApply useColor [testColorGreen] ("All " ++ show total ++ " tests passed (" ++ fmtTime elapsed ++ ")"))
        putStrLn ""
        when hasCached $
          putStrLn (if useColor then testColorYellow ++ "*" ++ testColorReset ++ " = cached test result" else "* = cached test result")
        when hiddenCachedSuccess $
          putStrLn "Cached successful tests are hidden. Cached failures/errors are shown. Use --show-cached to include cached successes, or --no-cache to force rerunning selected tests."
        if errors > 0
          then return 2
          else if failures > 0
            then return 1
            else return 0

printTestResultsOrdered :: Bool -> Bool -> Bool -> Int -> [TestSpec] -> [TestResult] -> IO ()
printTestResultsOrdered useColor showLog showCached nameWidth specs results = do
    let resMap = M.fromList [ (TestKey (trModule res) (trName res), res) | res <- results ]
        isOk res = trSuccess res == Just True && trException res == Nothing && not (trSkipped res)
        shouldShow res = not (trCached res) || showCached || not (isOk res) || trSnapshotUpdated res
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

-- | Write snapshot outputs for all tests that produced output.
writeSnapshotOutputs :: Paths -> [TestResult] -> IO ()
writeSnapshotOutputs paths results =
    mapM_ (writeSnapshotOutput paths) results

writeSnapshotOutput :: Paths -> TestResult -> IO ()
writeSnapshotOutput paths res =
    case trOutput res of
      Just out -> do
        let fileName = displayTestName (trName res)
            outDir = joinPath [projPath paths, "snapshots", "output", trModule res]
        createDirectoryIfMissing True outDir
        writeFile (outDir </> fileName) out
      Nothing -> return ()

-- | Update snapshot expected files from NotEqualError outputs.
applySnapshotUpdate :: Paths -> TestResult -> IO TestResult
applySnapshotUpdate paths res =
    case (trException res, trOutput res) of
      (Just exc, Just out)
        | isSnapshotMismatch exc -> do
            let fileName = displayTestName (trName res)
                snapshotDir = joinPath [projPath paths, "snapshots", "expected", trModule res]
            createDirectoryIfMissing True snapshotDir
            writeFile (snapshotDir </> fileName) out
            return (markSnapshotUpdated res)
      _ -> return res

snapshotMismatchPrefix :: String
snapshotMismatchPrefix = "testing.NotEqualError: Test output does not match expected snapshot value"

isSnapshotMismatch :: String -> Bool
isSnapshotMismatch exc = snapshotMismatchPrefix `isPrefixOf` exc

markSnapshotUpdated :: TestResult -> TestResult
markSnapshotUpdated res = res
  { trSnapshotUpdated = True
  , trSuccess = Just True
  , trSkipped = False
  , trSkipReason = Nothing
  , trException = Nothing
  , trNumFailures = 0
  , trNumErrors = 0
  }

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

fmtTime :: TimeSpec -> String
fmtTime t =
    printf "%6.3f s" secs
  where
    secs :: Float
    secs = (fromIntegral (sec t)) + (fromIntegral (nsec t) / 1000000000)
