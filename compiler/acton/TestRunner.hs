module TestRunner
  ( TestMode(..)
  , listProjectTests
  , runProjectTests
  , selectTestSources
  ) where

import qualified Acton.CommandLineParser as C
import Acton.Testing
import Acton.Compile
import qualified Acton.Syntax as A
import qualified Acton.SourceProvider as Source
import qualified InterfaceFiles
import qualified FileUtil
import TestFormat
import TestOutput
import TestPerf
import TestUI
import Control.Applicative ((<|>))
import Control.Concurrent.Async
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Monad
import Data.IORef
import Data.Char (isSpace, isHexDigit, toLower)
import Data.List (isPrefixOf, isSuffixOf, foldl', isInfixOf, intercalate)
import qualified Data.List
import Data.Maybe (catMaybes, listToMaybe, isJust)
import qualified Data.Map as M
import qualified Data.Set as Set
import System.Clock
import System.Directory
import System.Exit
import System.Environment (getEnvironment)
import qualified System.Info as System
import System.FilePath ((</>), (<.>), joinPath)
import System.IO (hClose, hIsEOF)
import System.Process
import ProcessUtil (stopProcessGroup)
import Text.Printf
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKM
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime)
import Control.Exception (IOException, SomeException, SomeAsyncException, AsyncException(..), displayException, evaluate, mask, onException, try, fromException, throwIO, finally)
import TerminalSize (termFitAnsiRight)
import qualified Text.Regex.TDFA as TDFA
import Data.Version (showVersion)
import qualified Paths_acton

data TestMode = TestModeRun | TestModeList | TestModePerf | TestModeStress deriving (Eq, Show)

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

data StressWorkerRow = StressWorkerRow
  { swrId :: Int
  , swrSync :: Bool
  , swrIterations :: Int
  , swrDriftUs :: Int
  , swrDriftTotalUs :: Int
  , swrCalibrating :: Bool
  , swrPhaseResolutionUs :: Int
  , swrTargetSweepIters :: Int
  }

data StressPhaseLane = StressPhaseLane
  { splPhaseResolutionUs :: Int
  , splTargetSweepIters :: Int
  }

getVer :: String
getVer = showVersion Paths_acton.version

printErrorAndExit :: String -> IO a
printErrorAndExit msg = do
    errorWithoutStackTrace msg
    exitFailure

-- | Compute the test binary path for a module and target.
testBinaryPath :: C.CompileOptions -> Paths -> String -> FilePath
testBinaryPath opts paths modName =
    let base = ".test_" ++ modNameToString mn
        mn = dropProjPrefix paths (modNameFromString modName)
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

modulesOpt paths topts = [ proj ++ "." ++ m | m <- C.testModules topts ]
  where proj = projName paths

-- | Select compilation roots without changing the contents of module caches.
-- A cached test list can confirm a match, but cannot rule one out: a changed
-- import may change an inferred function type and make it eligible as a test.
selectTestSources :: C.GlobalOptions -> C.CompileOptions -> Paths -> C.TestOptions -> [FilePath] -> IO [FilePath]
selectTestSources gopts opts paths topts files = do
    start <- getTime Monotonic
    regexes <- compileTestNameRegexes (C.testNames topts)
    let wanted = modulesOpt paths topts
        matches = not . null . filterTests regexes
    sources <- filterM (selected wanted matches) files
    when (C.timing gopts && not (quiet gopts opts)) $ do
      end <- getTime Monotonic
      putStrLn ("Timing: test source selection " ++ fmtTime (diffTimeSpec end start))
    return sources
  where
    sp = Source.diskSourceProvider
    selected wanted matches file = do
      mn <- moduleNameFromFile (srcDir paths) (projName paths) file
      if not (null wanted) && modNameToString mn `notElem` wanted
        then return False
        else if null (C.testNames topts)
          then return True
          else do
            task <- readModuleTask sp gopts opts paths { modName = mn } file
            case task of
              TyTask{ tyTests = names } | matches names -> return True
              ParseTask{ src = source } -> fromSource mn matches file source
              ParseErrorTask{} -> return True
              _ -> Source.spReadFile sp file >>= fromSource mn matches file . Source.ssText
    fromSource mn matches file source = do
      parsed <- parseActSource opts mn file source Nothing
      -- Let the normal compilation path report parse failures.
      return $ either (const True) (matches . candidates . A.mbody) parsed
    candidates = concatMap names
    names (A.With _ _ ss) = candidates ss
    names (A.Decl _ ds) = concatMap declNames ds
    names _ = []
    declNames d@A.Def{}
      | "_test_" `isPrefixOf` A.nstr (A.dname d) = [A.nstr (A.dname d)]
    -- Actor parameter types are resolved later. Include possible wrapper
    -- names here and leave exact discovery to the type checker.
    declNames d@A.Actor{} =
      let n = A.nstr (A.dname d)
      in [if "_test_" `isPrefixOf` n then n ++ "_wrapper" else "_test_" ++ n]
    declNames _ = []

-- | List tests for selected modules and print them in a stable order.
listProjectTests :: C.CompileOptions -> Paths -> C.TestOptions -> [String] -> IO ()
listProjectTests opts paths topts modules = do
    let wantedModules = Data.List.sort (filterModules (modulesOpt paths topts) modules)
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
                [ AesonKey.fromString "name" Aeson..= displayModName paths modName
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
            putStrLn ("Module " ++ displayModName paths modName ++ ":")
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
    let wantedModules = Data.List.sort (filterModules (modulesOpt paths topts) modules)
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
            outputJsonReport paths mode M.empty (timeEnd - timeStart) []
            return 0
          else do
            putStrLn "Nothing to test"
            return 0
      else do
        let maxNameLen = maximum (0 : map (length . tsDisplay) specs)
            nameWidth = max 20 (maxNameLen + 5)
            runContext = mkRunContext opts topts mode
            ctxHash = contextHashBytes runContext
            useCache = not (C.testNoCache topts) && mode == TestModeRun
        perfData <- if mode == TestModePerf then readPerfData paths else return M.empty
        perfHostInfo <- if mode == TestModePerf then readPerfHostInfo opts topts else return AesonKM.empty
        let detailLines res =
              map staticLine (formatTestDetailLines useColorOut (C.testShowLog topts) res) ++
              if mode == TestModePerf
                then formatTestPerfLines useColorOut (lookupPerfData perfData res) res
                else []
        cache <-
          if useCache
            then readTestCache (testCachePath paths) runContext
            else return TestCache
              { tcVersion = testCacheVersion
              , tcContext = runContext
              , tcTests = M.empty
              }
        testHashInfos <-
          if useCache
            then buildTestHashInfos paths ctxHash testsByModule
            else return M.empty
        let cacheEntries =
              if useCache
                then tcTests cache
                else M.empty
        when (C.verbose gopts && useCache) $
          putStrLn (formatTestCacheContext ctxHash (testCachePath paths))
        let logCache = if C.verbose gopts then putStrLn else \_ -> return ()
        (cachedResults0, _testsToRun) <-
          if useCache
            then classifyCachedTests logCache cacheEntries testHashInfos allTests
            else return ([], allTests)
        cachedResults1 <-
          if useCache
            then filterReusableCachedSnapshotResults logCache paths cachedResults0
            else return []
        cachedResults <-
          if C.testSnapshotUpdate topts
            then mapM (applySnapshotUpdate paths) cachedResults1
            else return cachedResults1
        let showCached = C.testShowCached topts
        when (not emitJson && not useCache) $
          case mode of
            TestModePerf -> putStrLn "Skipping test result cache in perf mode; running all selected tests"
            TestModeStress -> putStrLn "Skipping test result cache in stress mode; running all selected tests"
            _ -> putStrLn "Skipping test result cache (--no-cache); running all selected tests"
        when (not emitJson && showCached && not (null cachedResults)) $
          putStrLn ("Using cached results for " ++ show (length cachedResults) ++ " tests")
        withTestProgressUI gopts nameWidth useColorOut $ \ui -> do
          let totalTests = length specs
          progressDoneRef <- newIORef 0
          workers <- newIORef []
          let (effectiveMinTime, effectiveMaxTime) = effectiveTestTiming mode topts
              expectedDurationMs =
                fromIntegral
                  (if effectiveMaxTime > 0
                     then effectiveMaxTime
                     else effectiveMinTime)
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
                    modDisplay = displayModName paths (tsModule spec)
                    useColorLine = tpuUseColor ui
                case M.lookup key cachedMap of
                  Just cachedRes -> do
                    let line = formatTestFinalLineRenderer useColorLine (mode == TestModePerf) expectedDurationMs nameWidth display cachedRes
                        details = detailLines cachedRes
                    if shouldShowCached cachedRes
                      then do
                        ok <- testUiAppendFinal ui key modDisplay line
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
                              , trNumSkipped = 0
                              , trNumFailures = 0
                              , trNumErrors = 0
                              , trNumIterations = 0
                              , trTestDuration = 0
                              , trRaw = Aeson.Null
                              , trSnapshotUpdated = False
                              , trCached = False
                              }
                            initLine = formatTestLiveLineRenderer useColorLine expectedDurationMs nameWidth display initRes
                        started <- testUiStart ui key modDisplay initLine
                        if not started
                          then return Nothing
                          else do
                            callbacks <- testProgressCallbacks ui eventChan key display (mode == TestModePerf) expectedDurationMs detailLines
                            let baselineScale = lookupPerfData perfData initRes >>= (\old -> perfBaselineScale old perfHostInfo)
                            mask $ \unmask -> do
                              worker <- async $ unmask $ mask $ \restore -> do
                                resE <- try (restore (runModuleTestStreaming opts paths topts mode perfHostInfo baselineScale (tsModule spec) (tsName spec)
                                                       (tpuEnabled ui) callbacks))
                                          :: IO (Either SomeException TestResult)
                                case resE of
                                  Right res ->
                                    writeChan eventChan (TestEventDone res)
                                  Left ex -> do
                                    let res = initRes
                                          { trComplete = True
                                          , trException = Just ("Test runner exception: " ++ displayException ex)
                                          , trNumErrors = 1
                                          }
                                    finishE <- try (restore (tpcOnDone callbacks res >> tpcOnFinal callbacks res))
                                                 :: IO (Either SomeException ())
                                    writeChan eventChan (TestEventDone res)
                                    case finishE of
                                      Left finishEx | isJust (fromException finishEx :: Maybe SomeAsyncException) -> throwIO finishEx
                                      _ -> return ()
                                    when (isJust (fromException ex :: Maybe SomeAsyncException)) $
                                      throwIO ex
                              atomicModifyIORef' workers (\ws -> (worker : ws, ()))
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
                        let pending'' =
                              if testResultInterrupted res
                                then []
                                else pending'
                        loop pending'' (running' - 1) (res : results')
                      TestEventRoom -> loop pending' running' results'
          results <- loop specs 0 [] `finally` (readIORef workers >>= mapConcurrently_ cancel)
          timeEnd <- getTime Monotonic
          writeSnapshotOutputs paths results
          let resultsRun =
                if C.testSnapshotUpdate topts
                  then filter (\r -> not (trCached r) || trSnapshotUpdated r) results
                  else filter (not . trCached) results
          when (C.testRecord topts) $
            writePerfData paths perfData resultsRun
          let cacheEntries' = foldl' (updateTestCacheEntry testHashInfos) cacheEntries resultsRun
              newCache = TestCache
                { tcVersion = testCacheVersion
                , tcContext = runContext
                , tcTests = cacheEntries'
                }
          when useCache $
            writeTestCache (testCachePath paths) newCache
          if emitJson
            then do
              outputJsonReport paths mode perfData (timeEnd - timeStart) results
              return (testExitCode results)
            else do
              when (not (tpuEnabled ui)) $
                printTestResultsOrdered paths (tpuUseColor ui) (mode == TestModePerf) showCached nameWidth detailLines specs results
              _ <- printTestSummary (tpuUseColor ui) (timeEnd - timeStart) showCached results
              return (testExitCode results)
  where
    mkRunContext opts' topts' mode' = TestRunContext
      { trcCompilerVersion = getVer
      , trcTarget = C.target opts'
      , trcOptimize = show (C.optimize opts')
      , trcMode = show mode'
      , trcArgs = testCmdArgs mode' topts'
      }

testExitCode :: [TestResult] -> Int
testExitCode results =
    let failures = length [ r | r <- results, trSuccess r == Just False ]
        errors = length [ r | r <- results, trSuccess r == Nothing ]
    in if errors > 0 then 2 else if failures > 0 then 1 else 0

outputJsonReport :: Paths -> TestMode -> PerfData -> TimeSpec -> [TestResult] -> IO ()
outputJsonReport paths mode baseline elapsed results = do
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
          in Aeson.object $
               [ AesonKey.fromString "module" Aeson..= displayModName paths (trModule res)
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
               ] ++
               [ AesonKey.fromString "performance" Aeson..= perfJson (lookupPerfData baseline res) res
               | mode == TestModePerf
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

-- | Read the discovered tests for a module from its .tydb header.
listModuleTests :: C.CompileOptions -> Paths -> String -> IO [String]
listModuleTests _opts paths modName =
    readModuleTests paths (modNameFromString modName)

-- | Read tests from a module's .tydb header, returning [] on any error.
readModuleTests :: Paths -> A.ModName -> IO [String]
readModuleTests paths mn = do
    let tyFile = tyDbPath paths mn
    exists <- InterfaceFiles.interfaceExists tyFile
    if not exists
      then return []
      else do
        hdrE <- (try :: IO a -> IO (Either SomeException a)) $ InterfaceFiles.readHeaderSummary tyFile
        case hdrE of
          Left _ -> return []
          Right (_sourceMeta, _srcH, _ih, _implH, _imps, _depModules, _nameCount, _roots, tests, _doc) ->
            return tests

modNameFromString :: String -> A.ModName
modNameFromString s = A.modName (splitOnChar '.' s)

-- | Render a module name for user-facing output, dropping the project-name
-- prefix so consumers (the VS Code extension, the --module flag, the on-disk
-- src/ layout) see the bare module name instead of e.g. "proj.mod".
displayModName :: Paths -> String -> String
displayModName paths = modNameToString . dropProjPrefix paths . modNameFromString

splitOnChar :: Char -> String -> [String]
splitOnChar ch input = case break (== ch) input of
  (chunk, []) -> [chunk]
  (chunk, _ : rest) -> chunk : splitOnChar ch rest

-- | Run a single test case and stream JSON updates.
runModuleTestStreaming :: C.CompileOptions
                       -> Paths
                       -> C.TestOptions
                       -> TestMode
                       -> Aeson.Object
                       -> Maybe Int
                       -> String
                       -> String
                       -> Bool
                       -> TestProgressCallbacks
                       -> IO TestResult
runModuleTestStreaming opts paths topts mode perfHostInfo baselineScale modName testName allowLive callbacks = do
    environment <- getEnvironment
    let binPath = testBinaryPath opts paths modName
        modeArgs =
          case mode of
            TestModePerf -> ["perf"]
            TestModeStress -> ["stress"]
            _ -> []
        cmd = ["test", testName] ++ modeArgs ++ testCmdArgs mode topts
          ++ maybe [] (\scale -> ["--scale", show scale]) (C.testScale topts <|> baselineScale)
    resultRef <- newIORef Nothing
    lineDoneRef <- newIORef False
    stdOutRef <- newIORef emptyTestOutput
    stdErrRef <- newIORef emptyTestOutput
    let onUpdate raw = do
          let res = validateScale (annotatePerfResult perfHostInfo raw)
          modifyIORef' resultRef $ \previous ->
            case previous of
              Just old | trComplete old && not (trComplete res) -> previous
              _ -> Just res
          done <- readIORef lineDoneRef
          when (not done && allowLive) $ do
            if trComplete res
              then do
                tpcOnDone callbacks res
                writeIORef lineDoneRef True
              else tpcOnLive callbacks res
        onOutLine line = modifyIORef' stdOutRef (appendTestOutput line)
        addStdErr line = modifyIORef' stdErrRef (appendTestOutput line)
        onErrLine line =
          case parseJsonLine line of
            Nothing -> addStdErr line
            Just val -> case parseTestInfo val of
                          Just res -> onUpdate res
                          Nothing -> addStdErr line
    let procSpec = (proc binPath cmd)
          { cwd = Just (projPath paths)
          , env = Just ((if mode == TestModePerf then [("ACTON_TEST_PERF", "1")] else [])
              ++ filter ((/= "ACTON_TEST_PERF") . fst) environment)
          , create_group = C.watch opts
          , delegate_ctlc = not (C.watch opts)
          }
    procRes <- try (readProcessWithExitCodeStreaming procSpec onOutLine onErrLine) :: IO (Either SomeException ExitCode)
    (exitCode, interruptedByUser) <-
      case procRes of
        Right code ->
          return (code, False)
        Left ex ->
          case fromException ex of
            Just UserInterrupt ->
              return (ExitFailure (-2), True)
            _ -> throwIO ex
    final <- readIORef resultRef
    stdOut <- readIORef stdOutRef
    stdErr <- readIORef stdErrRef
    let (out, stdErrText) = finishTestOutput stdOut stdErr
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
          , trNumSkipped = 0
          , trNumFailures = 0
          , trNumErrors = 1
          , trNumIterations = 0
          , trTestDuration = 0
          , trRaw = Aeson.Null
          , trSnapshotUpdated = False
          , trCached = False
          }
        -- The test binary self-reports its bare module name; the runner keys
        -- results, caches, and snapshots off the project-qualified discovery
        -- name, so pin trModule to the module we launched.
        res0 = (maybe fallback id final) { trModule = modName }
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
        interrupted = interruptedByUser || isInterruptExitCode exitCode
        incompleteSuccessExit = mode == TestModeStress && exitCode == ExitSuccess && not (trComplete res1)
        res
          | mode == TestModeStress && (interrupted || incompleteSuccessExit) = finalizeInterruptedStressResult res1
          | otherwise =
              case exitCode of
                ExitSuccess -> res1
                ExitFailure code ->
                  res1 { trException = Just ("Test process exited with code " ++ show code) }
    updated <-
      if C.testSnapshotUpdate topts
        then applySnapshotUpdate paths res
        else return res
    let res' = validateScale updated
    done <- readIORef lineDoneRef
    if done
      then tpcOnFinal callbacks res'
      else do
        tpcOnDone callbacks res'
        tpcOnFinal callbacks res'
    return res'
  where
    validateScale res
      | isJust (C.testScale topts), trComplete res, trSuccess res == Just True
      , not (trSkipped res), not (isJust (trException res))
      , Aeson.Object obj <- trRaw res, Just info <- perfInfo obj
      , AesonKM.lookup (AesonKey.fromString "loop") info == Just (Aeson.Bool False) =
          res { trSuccess = Just False
              , trException = Just "Explicit --scale requires a test that uses t.loop()"
              , trNumFailures = trNumFailures res + 1
              , trSnapshotUpdated = False
              }
      | otherwise = res

    isInterruptExitCode ExitSuccess = False
    isInterruptExitCode (ExitFailure code) = code == (-2) || code == 130

    finalizeInterruptedStressResult res =
      let success' =
            case trSuccess res of
              Just _ -> trSuccess res
              Nothing ->
                if trNumFailures res == 0 && trNumErrors res == 0
                  then Just True
                  else Nothing
          exception' =
            if trNumFailures res == 0 && trNumErrors res == 0
              then Nothing
              else trException res
      in res
         { trComplete = True
         , trSuccess = success'
         , trException = exception'
         , trRaw = markInterruptedRaw (trRaw res)
         }

    markInterruptedRaw raw =
      case raw of
        Aeson.Object o ->
          Aeson.Object (AesonKM.insert (AesonKey.fromString "interrupted") (Aeson.Bool True) o)
        _ ->
          Aeson.object [AesonKey.fromString "interrupted" Aeson..= True]

testProgressCallbacks :: TestProgressUI -> Chan TestEvent -> TestKey -> String -> Bool -> Double -> (TestResult -> [TestLine]) -> IO TestProgressCallbacks
testProgressCallbacks ui eventChan key display perfMode expectedDurationMs detailLines = do
    workerKeysRef <- newIORef M.empty
    let nameWidth = tpuNameWidth ui
        useColorOut = tpuUseColor ui
        liveLine res = formatTestLiveLineRenderer useColorOut expectedDurationMs nameWidth display res
        finalLine res = formatTestFinalLineRenderer useColorOut perfMode expectedDurationMs nameWidth display res
        workerLine done durationMs laneSpec row cols =
          let role = if swrSync row then "sync" else "drift"
              phase =
                if done
                  then "DONE"
                  else if swrCalibrating row
                    then "CAL "
                    else "RUN "
              iterations = swrIterations row
              rate = testsPerSecond iterations durationMs
              baseLine = printf "      w%-3d %-5s %s : %7d iters @ %7.1f/s cur=%6dus tot=%8dus"
                              (swrId row) role phase iterations rate (swrDriftUs row) (swrDriftTotalUs row)
              line = baseLine ++ renderStressPhaseLane useColorOut cols baseLine laneSpec row
          in termFitAnsiRight cols line
        workerKey wid = TestKey (tkModule key) (tkName key ++ "#worker" ++ show wid)
        updateStressWorkers done res = do
          let rows = stressWorkerRows res
              laneSpec = stressPhaseLaneSpec res
          unless (null rows) $ do
            existing <- readIORef workerKeysRef
            existing' <- foldM (\acc row -> do
              let wid = swrId row
                  line = workerLine done (trTestDuration res) laneSpec row
              case M.lookup wid acc of
                Just wk -> do
                  if done
                    then do
                      removed <- testUiFinalize ui wk line
                      when removed $
                        writeChan eventChan TestEventRoom
                    else
                      testUiUpdateLive ui wk line
                  return acc
                Nothing ->
                  if done
                    then return acc
                    else do
                      let wk = workerKey wid
                      started <- testUiStart ui wk (tkModule key) line
                      if started
                        then return (M.insert wid wk acc)
                        else return acc
              ) existing rows
            writeIORef workerKeysRef existing'
    return TestProgressCallbacks
      { tpcOnLive = \res -> testUiUpdateLive ui key (liveLine res)
          >> updateStressWorkers False res
      , tpcOnDone = \res -> do
          updateStressWorkers True res
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

stressWorkerRows :: TestResult -> [StressWorkerRow]
stressWorkerRows res =
    case trRaw res of
      Aeson.Object obj ->
        case AesonKM.lookup (AesonKey.fromString "stress_workers") obj of
          Just (Aeson.Array workers) ->
            catMaybes (map parseWorker (foldr (:) [] workers))
          _ -> []
      _ -> []
  where
    parseWorker val =
      case val of
        Aeson.Object o -> do
          wid <- lookupInt o "id"
          iterations <- lookupInt o "iterations"
          driftUs <- lookupIntDefault o "drift_us" 0
          driftTotalUs <- lookupIntDefault o "drift_total_us" 0
          syncW <- lookupBool o "sync"
          calibrating <- lookupBoolDefault o "calibrating" False
          phaseResolutionUs <- lookupIntDefault o "phase_resolution_us" 0
          targetSweepIters <- lookupIntDefault o "target_sweep_iters" 0
          return StressWorkerRow
            { swrId = wid
            , swrSync = syncW
            , swrIterations = iterations
            , swrDriftUs = driftUs
            , swrDriftTotalUs = driftTotalUs
            , swrCalibrating = calibrating
            , swrPhaseResolutionUs = phaseResolutionUs
            , swrTargetSweepIters = targetSweepIters
            }
        _ -> Nothing
    lookupInt o keyName =
      case AesonKM.lookup (AesonKey.fromString keyName) o of
        Just v -> AesonTypes.parseMaybe Aeson.parseJSON v
        _ -> Nothing
    lookupIntDefault o keyName defVal =
      case lookupInt o keyName of
        Just n -> Just n
        Nothing -> Just defVal
    lookupBool o keyName =
      case AesonKM.lookup (AesonKey.fromString keyName) o of
        Just v -> AesonTypes.parseMaybe Aeson.parseJSON v
        _ -> Nothing
    lookupBoolDefault o keyName defVal =
      case lookupBool o keyName of
        Just b -> Just b
        Nothing -> Just defVal

stressPhaseLaneSpec :: TestResult -> Maybe StressPhaseLane
stressPhaseLaneSpec res =
    case trRaw res of
      Aeson.Object obj -> do
        phaseResolutionMs <- lookupDouble obj "stress_phase_resolution_ms"
        targetSweepIters <- lookupInt obj "stress_target_sweep_iters"
        let phaseResolutionUs = max 0 (round (phaseResolutionMs * 1000.0))
        guard (phaseResolutionUs > 0 && targetSweepIters > 0)
        return StressPhaseLane
          { splPhaseResolutionUs = phaseResolutionUs
          , splTargetSweepIters = targetSweepIters
          }
      _ -> Nothing
  where
    lookupInt o keyName =
      case AesonKM.lookup (AesonKey.fromString keyName) o of
        Just v -> AesonTypes.parseMaybe Aeson.parseJSON v
        _ -> Nothing
    lookupDouble o keyName =
      case AesonKM.lookup (AesonKey.fromString keyName) o of
        Just v -> (AesonTypes.parseMaybe Aeson.parseJSON v :: Maybe Double)
        _ -> Nothing

resolveStressPhaseLane :: Maybe StressPhaseLane -> StressWorkerRow -> Maybe StressPhaseLane
resolveStressPhaseLane baseSpec row
  | swrPhaseResolutionUs row > 0 && swrTargetSweepIters row > 0 =
      Just StressPhaseLane
        { splPhaseResolutionUs = swrPhaseResolutionUs row
        , splTargetSweepIters = swrTargetSweepIters row
        }
  | otherwise = baseSpec

renderStressPhaseLane :: Bool -> Int -> String -> Maybe StressPhaseLane -> StressWorkerRow -> String
renderStressPhaseLane useColorOut cols baseLine baseSpec row
  | not useColorOut = ""
  | swrCalibrating row = ""
  | otherwise =
      case resolveStressPhaseLane baseSpec row of
        Just spec ->
          let avail = cols - length baseLine
          in case stressPhaseLaneWidth avail of
               Just laneWidth -> " " ++ stressPhaseLaneText laneWidth spec row
               Nothing -> ""
        Nothing -> ""

stressPhaseLaneWidth :: Int -> Maybe Int
stressPhaseLaneWidth avail
  | avail < 11 = Nothing
  | otherwise =
      let width = min 32 (avail - 3)
      in if width < 8 then Nothing else Just width

stressPhaseLaneText :: Int -> StressPhaseLane -> StressWorkerRow -> String
stressPhaseLaneText laneWidth spec row =
    "|" ++ concatMap renderCell [0 .. laneWidth - 1] ++ testColorReset ++ "|"
  where
    totalPhaseUs = fromIntegral (max 1 (splPhaseResolutionUs spec * splTargetSweepIters spec)) :: Double
    windowUs = fromIntegral (max 1 (splPhaseResolutionUs spec)) :: Double
    phaseStartUs
      | swrSync row = 0.0
      | otherwise = fromIntegral (swrDriftTotalUs row `mod` max 1 (splPhaseResolutionUs spec * splTargetSweepIters spec))
    centerUs = wrapPhase (phaseStartUs + (windowUs / 2.0))
    windowCells = fromIntegral laneWidth / fromIntegral (max 1 (splTargetSweepIters spec)) :: Double
    baselineBg = ansiBgReset
    edgeBg = ansiBg 17
    haloBg = ansiBg 18
    coreBg
      | windowCells < 0.35 = ansiBg 24
      | windowCells < 0.70 = ansiBg 24
      | otherwise = ansiBg 24

    renderCell idx =
      let cellStartUs = totalPhaseUs * fromIntegral idx / fromIntegral laneWidth
          cellEndUs = totalPhaseUs * fromIntegral (idx + 1) / fromIntegral laneWidth
          overlapFrac = circularOverlap phaseStartUs (phaseStartUs + windowUs) cellStartUs cellEndUs totalPhaseUs
          isCore = circularContains centerUs cellStartUs cellEndUs totalPhaseUs
          bg
            | isCore = coreBg
            | overlapFrac >= 0.66 = haloBg
            | overlapFrac > 0.0 = edgeBg
            | otherwise = baselineBg
      in bg ++ " "

    wrapPhase x
      | totalPhaseUs <= 0.0 = 0.0
      | otherwise =
          let wrapped = x - (fromIntegral (floor (x / totalPhaseUs)) * totalPhaseUs)
          in if wrapped < 0.0 then wrapped + totalPhaseUs else wrapped

    circularContains point start end total =
      overlapLinear start end point (point + 0.0001) total > 0.0

    circularOverlap start end cellStart cellEnd total =
      let segments = circularSegments start end total
          cellSegments = circularSegments cellStart cellEnd total
          overlapSum = sum [ overlapLinear' s1 e1 s2 e2 | (s1, e1) <- segments, (s2, e2) <- cellSegments ]
          cellWidth = max 0.000001 (cellEnd - cellStart)
      in overlapSum / cellWidth

    circularSegments start end total
      | total <= 0.0 = [(0.0, 1.0)]
      | otherwise =
          let start' = wrapPhase start
              end' = start' + (end - start)
          in if end' <= total
               then [(start', end')]
               else [(start', total), (0.0, end' - total)]

    overlapLinear start end point pointEnd total =
      let segments = circularSegments start end total
          pointSegments = circularSegments point pointEnd total
      in sum [ overlapLinear' s1 e1 s2 e2 | (s1, e1) <- segments, (s2, e2) <- pointSegments ]

    overlapLinear' start1 end1 start2 end2 =
      max 0.0 (min end1 end2 - max start1 start2)

    ansiBg code = "\ESC[48;5;" ++ show code ++ "m"
    ansiBgReset = "\ESC[49m"

testsPerSecond :: Int -> Double -> Double
testsPerSecond iterations durationMs
  | iterations <= 0 = 0
  | durationMs <= 0 = 0
  | otherwise = (fromIntegral iterations * 1000.0) / durationMs

effectiveTestTiming :: TestMode -> C.TestOptions -> (Int, Int)
effectiveTestTiming TestModePerf topts = (C.testTime topts, C.testTime topts)
effectiveTestTiming mode topts =
    let rawMinTime = C.testMinTime topts
        minTime =
          case mode of
            TestModeStress ->
              if not (C.testMinTimeSet topts)
                then 1000
                else rawMinTime
            _ -> rawMinTime
        rawMaxTime = C.testMaxTime topts
        modeDefaultMaxTime =
          case mode of
            TestModeRun -> minTime
            TestModeStress -> 5000
            _ -> 1000
        maxTime
          | C.testMaxTimeSet topts && rawMaxTime == 0 = 0
          | C.testMaxTimeSet topts = max rawMaxTime minTime
          | otherwise = modeDefaultMaxTime
    in (minTime, maxTime)

-- | Build test runner arguments from TestOptions limits.
testCmdArgs :: TestMode -> C.TestOptions -> [String]
testCmdArgs mode topts
  | mode == TestModePerf = ["--time", show (C.testTime topts)] ++ tagArgs
  | otherwise =
    let iter = C.testIter topts
        rawMaxIter = C.testMaxIter topts
        (minTime, maxTime) = effectiveTestTiming mode topts
        stressWorkerArgs
          | mode == TestModeStress && C.testStressWorkers topts > 0 =
              ["--stress-workers", show (C.testStressWorkers topts)]
          | otherwise = []
        maxIter
          | mode == TestModeStress && maxTime == 0 && not (C.testMaxIterSet topts) = 0
          | otherwise = rawMaxIter
        baseArgs =
          if iter > 0
            then ["--max-iter", show iter, "--min-iter", show iter, "--max-time", show (10^6), "--min-time", "1"]
            else [ "--max-iter", show maxIter
                 , "--min-iter", show (C.testMinIter topts)
                 , "--max-time", show maxTime
                 , "--min-time", show minTime
                 ]
    in baseArgs ++ stressWorkerArgs ++ tagArgs
  where
    tagArgs = concatMap (\tag -> ["--tag", tag]) (C.testTags topts)

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
parseJsonLine :: T.Text -> Maybe Aeson.Value
parseJsonLine line =
    let trimmed = T.stripStart line
    in case T.uncons trimmed of
         Just ('{', _) -> Aeson.decodeStrict' (TE.encodeUtf8 trimmed)
         _ -> Nothing

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
    numSkipped <- o Aeson..:? AesonKey.fromString "num_skipped" Aeson..!= 0
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
      , trNumSkipped = numSkipped
      , trNumFailures = numFailures
      , trNumErrors = numErrors
      , trNumIterations = numIterations
      , trTestDuration = testDuration
      , trRaw = Aeson.Object o
      , trSnapshotUpdated = False
      , trCached = False
      }

testResultInterrupted :: TestResult -> Bool
testResultInterrupted res =
  case trRaw res of
    Aeson.Object obj ->
      case AesonKM.lookup (AesonKey.fromString "interrupted") obj of
        Just v ->
          case AesonTypes.parseMaybe Aeson.parseJSON v of
            Just True -> True
            _ -> False
        _ -> False
    _ -> False

-- | Print a summary line and return the failure/error exit code.
printTestSummary :: Bool -> TimeSpec -> Bool -> [TestResult] -> IO Int
printTestSummary useColor elapsed showCached results = do
    let total = length results
        failures = length [ r | r <- results, trSuccess r == Just False ]
        errors = length [ r | r <- results, trSuccess r == Nothing ]
        skipped = length [ r | r <- results, trSkipped r ]
        hiddenCachedSuccess = not showCached && any (\r -> trCached r && trSuccess r == Just True && not (trSkipped r)) results
        interrupted = any testResultInterrupted results
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
        when interrupted $
          putStrLn "Stress run interrupted by user; showing partial results collected so far."
        when hiddenCachedSuccess $
          putStrLn "Cached successful tests are hidden. Cached failures/errors are shown. Use --show-cached to include cached successes, or --no-cache to force rerunning selected tests."
        if errors > 0
          then return 2
          else if failures > 0
            then return 1
            else return 0

printTestResultsOrdered :: Paths -> Bool -> Bool -> Bool -> Int -> (TestResult -> [TestLine]) -> [TestSpec] -> [TestResult] -> IO ()
printTestResultsOrdered paths useColor perfMode showCached nameWidth detailLines specs results = do
    let resMap = M.fromList [ (TestKey (trModule res) (trName res), res) | res <- results ]
        isOk res = trSuccess res == Just True && trException res == Nothing && not (trSkipped res)
        shouldShow res = not (trCached res) || showCached || not (isOk res) || trSnapshotUpdated res
        formatLine spec res =
          formatTestFinalLineRenderer useColor perfMode (trTestDuration res) nameWidth (tsDisplay spec) res maxBound
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
                        putStrLn (moduleHeaderLine (displayModName paths modName))
                        return (Set.insert modName printedMods)
                  putStrLn (formatLine spec res)
                  mapM_ (putStrLn . ($ maxBound)) (detailLines res)
                  mapM_ putStrLn (formatStressWorkerFinalLines useColor res)
                  go printedMods' True rest
    go Set.empty False specs

formatStressWorkerFinalLines :: Bool -> TestResult -> [String]
formatStressWorkerFinalLines useColorOut res =
    map renderRow (stressWorkerRows res)
  where
    durationMs = trTestDuration res
    laneSpec = stressPhaseLaneSpec res
    renderRow row =
      let role = if swrSync row then "sync" else "drift"
          iterations = swrIterations row
          rate = testsPerSecond iterations durationMs
          baseLine = printf "      w%-3d %-5s DONE : %7d iters @ %7.1f/s cur=%6dus tot=%8dus"
                            (swrId row) role iterations rate (swrDriftUs row) (swrDriftTotalUs row)
      in baseLine ++ renderStressPhaseLane useColorOut (maxBound :: Int) baseLine laneSpec row

-- | Write snapshot outputs for all tests that produced output.
writeSnapshotOutputs :: Paths -> [TestResult] -> IO ()
writeSnapshotOutputs paths results =
    mapM_ (writeSnapshotOutput paths) results

writeSnapshotOutput :: Paths -> TestResult -> IO ()
writeSnapshotOutput paths res =
    case trOutput res of
      Just out -> do
        let fileName = displayTestName (trName res)
            outDir = joinPath [projPath paths, "snapshots", "output", displayModName paths (trModule res)]
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
                snapshotDir = joinPath [projPath paths, "snapshots", "expected", displayModName paths (trModule res)]
            createDirectoryIfMissing True snapshotDir
            writeFile (snapshotDir </> fileName) out
            return (markSnapshotUpdated res)
      _ -> return res

-- | Reuse cached snapshot results only when the on-disk snapshot metadata still
-- | shows that the expected file predates the last produced output. Any
-- | uncertainty forces a rerun.
filterReusableCachedSnapshotResults :: (String -> IO ()) -> Paths -> [TestResult] -> IO [TestResult]
filterReusableCachedSnapshotResults logCache paths =
    fmap catMaybes . mapM keepIfReusable
  where
    keepIfReusable res =
      case trOutput res of
        Nothing -> return (Just res)
        Just _ -> do
          reusable <- snapshotMetadataAllowsCacheHit logCache paths res
          if reusable
            then return (Just res)
            else return Nothing

snapshotCacheLabel :: TestResult -> String
snapshotCacheLabel res = trModule res ++ "." ++ trName res

snapshotMetadataAllowsCacheHit :: (String -> IO ()) -> Paths -> TestResult -> IO Bool
snapshotMetadataAllowsCacheHit logCache paths res = do
    mExpected <- readFirstExistingSnapshotMeta expectedPaths
    case mExpected of
      Nothing -> miss "missing expected snapshot"
      Just (expectedSize, expectedMTime) -> do
        mOutput <- readSnapshotMeta outputPath
        case mOutput of
          Nothing -> miss "missing snapshot output"
          Just (outputSize, outputMTime)
            | expectedSize /= outputSize -> miss "snapshot size changed"
            | expectedMTime >= outputMTime -> miss "snapshot expected is newer than output"
            | otherwise -> return True
  where
    fileName = displayTestName (trName res)
    dispMod = displayModName paths (trModule res)
    expectedPaths =
      [ joinPath [projPath paths, "snapshots", "expected", dispMod, fileName]
      , joinPath [projPath paths, "test", "golden", dispMod, fileName]
      ]
    outputPath = joinPath [projPath paths, "snapshots", "output", dispMod, fileName]
    miss reason = do
      logCache ("[test-cache] " ++ snapshotCacheLabel res ++ " cache=miss (" ++ reason ++ ")")
      return False

readFirstExistingSnapshotMeta :: [FilePath] -> IO (Maybe (Integer, UTCTime))
readFirstExistingSnapshotMeta [] = return Nothing
readFirstExistingSnapshotMeta (path:rest) = do
    mMeta <- readSnapshotMeta path
    case mMeta of
      Just meta -> return (Just meta)
      Nothing -> readFirstExistingSnapshotMeta rest

readSnapshotMeta :: FilePath -> IO (Maybe (Integer, UTCTime))
readSnapshotMeta path = do
    exists <- doesFileExist path
    if not exists
      then return Nothing
      else do
        sizeE <- (try :: IO a -> IO (Either SomeException a)) $ getFileSize path
        timeE <- (try :: IO a -> IO (Either SomeException a)) $ getModificationTime path
        case (sizeE, timeE) of
          (Right size, Right mtime) -> return (Just (size, mtime))
          _ -> return Nothing

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
  , trNumSkipped = 0
  , trNumFailures = 0
  , trNumErrors = 0
  }

type PerfData = M.Map String (M.Map String Aeson.Value)

-- Keep raw machine identifiers out of recordings. An unknown identity disables
-- comparisons; a CPU model or OS version is not a machine identifier.
readPerfHostInfo :: C.CompileOptions -> C.TestOptions -> IO Aeson.Object
readPerfHostInfo opts topts = do
    identity <- try readIdentity :: IO (Either IOException (Maybe String))
    let machine = case identity of
          Right (Just raw) -> Just (BS.unpack (Base16.encode (SHA256.hash (BS.pack ("acton-perf-machine-v1:" ++ raw)))))
          _ -> Nothing
    return $ AesonKM.fromList
      [ (AesonKey.fromString "machine", Aeson.toJSON machine)
      , (AesonKey.fromString "build", Aeson.object
          [ AesonKey.fromString "optimize" Aeson..= show (C.optimize opts)
          , AesonKey.fromString "target" Aeson..= C.target opts
          , AesonKey.fromString "cpu" Aeson..= C.cpu opts
          , AesonKey.fromString "no_threads" Aeson..= C.no_threads opts
          , AesonKey.fromString "db" Aeson..= C.db opts
          , AesonKey.fromString "no_dbp" Aeson..= C.no_dbp opts
          ])
      , (AesonKey.fromString "tags", Aeson.toJSON (Set.toAscList (Set.fromList tags)))
      , (AesonKey.fromString "version", Aeson.toJSON ("3" :: String))
      , (AesonKey.fromString "gc", Aeson.toJSON ("natural" :: String))
      ]
  where
    tags = filter (not . null)
      [ dropWhile isSpace (reverse (dropWhile isSpace (reverse tag)))
      | raw <- C.testTags topts, tag <- splitOnChar ',' raw ]
    readIdentity = case System.os of
      "darwin" -> do
        (code, out, _) <- readProcessWithExitCode "/usr/sbin/ioreg" ["-rd1", "-c", "IOPlatformExpertDevice"] ""
        return $ if code /= ExitSuccess then Nothing else listToMaybe
          [ raw | line <- lines out, (key, '=' : value) <- [break (== '=') line]
                , "\"IOPlatformUUID\"" `isInfixOf` key, Just raw <- [normalize value] ]
      "linux" -> normalize . BS.unpack <$> BS.readFile "/etc/machine-id"
      _ -> return Nothing
    normalize value =
      let raw = map toLower (filter (\c -> not (isSpace c) && c /= '"' && c /= '-') value)
      in if length raw == 32 && all isHexDigit raw && any (/= '0') raw
           then Just raw else Nothing

annotatePerfResult :: Aeson.Object -> TestResult -> TestResult
annotatePerfResult host res = case trRaw res of
    Aeson.Object obj | Just info <- perfInfo obj ->
      let extra = AesonKM.filterWithKey (\key _ -> AesonKey.toString key `elem` ["machine", "build", "tags"]) host
      in res { trRaw = Aeson.Object (AesonKM.insert (AesonKey.fromString "perf_info") (Aeson.Object (extra `AesonKM.union` info)) obj) }
    _ -> res

-- | Read the baseline before running tests, including when updating it.
readPerfData :: Paths -> IO PerfData
readPerfData paths = do
    let path = projPath paths </> "perf_data"
    exists <- doesPathExist path
    if not exists
      then return M.empty
      else do
        result <- try (Aeson.eitherDecodeFileStrict path) :: IO (Either IOException (Either String PerfData))
        case result of
          Right (Right baseline) -> return baseline
          Right (Left err) -> printErrorAndExit ("Cannot read performance baseline " ++ path ++ ": " ++ err)
          Left err -> printErrorAndExit ("Cannot read performance baseline " ++ path ++ ": " ++ displayException err)

lookupPerfData :: PerfData -> TestResult -> Maybe Aeson.Object
lookupPerfData baseline res = do
    tests <- M.lookup (trModule res) baseline
    raw <- M.lookup (trName res) tests
    old <- AesonTypes.parseMaybe parseTestInfoValue raw
    testPerfData old

-- | Update successful measurements without discarding unselected or failed tests.
writePerfData :: Paths -> PerfData -> [TestResult] -> IO ()
writePerfData paths baseline results = do
    let measurements =
          [ (res, obj)
          | res <- results
          , Just obj <- [testPerfData res]
          ]
        addTest acc (res, obj) =
          M.insertWith M.union (trModule res) (M.singleton (trName res) (Aeson.Object obj)) acc
        updated = foldl' addTest baseline measurements
    unless (null measurements) $
      FileUtil.writeFile (projPath paths </> "perf_data")
        (T.unpack (TE.decodeUtf8 (BL.toStrict (Aeson.encode updated))))

-- | Drain both process streams through callbacks without retaining another copy.
readProcessWithExitCodeStreaming :: CreateProcess -> (T.Text -> IO ()) -> (T.Text -> IO ()) -> IO ExitCode
readProcessWithExitCodeStreaming cp onOutLine onErrLine = mask $ \restore -> do
    let cp' = cp { std_in = NoStream, std_out = CreatePipe, std_err = CreatePipe }
    withCreateProcess cp' $ \_ mOut mErr ph -> do
      pid <- getPid ph
      groupOwned <- newIORef True
      let stop = mask $ \_ -> do
            owned <- readIORef groupOwned
            when owned $ do
              stopProcessGroup ph pid
              writeIORef groupOwned False
          readLines onLine mH =
            case mH of
              Nothing -> return ()
              Just h -> do
                let go = do
                      eof <- hIsEOF h
                      unless eof $ TIO.hGetLine h >>= onLine >> go
                go
                hClose h
      let capture = restore $
            withAsync (readLines onOutLine mOut) $ \outReader ->
              withAsync (readLines onErrLine mErr) $ \errReader -> do
                code <- waitForProcess ph
                when (create_group cp') stop
                wait outReader
                wait errReader
                return code
      if create_group cp'
        then capture `finally` stop
        else capture `onException` (terminateProcess ph >> void (waitForProcess ph))

fmtTime :: TimeSpec -> String
fmtTime t =
    printf "%6.3f s" secs
  where
    secs :: Float
    secs = (fromIntegral (sec t)) + (fromIntegral (nsec t) / 1000000000)
