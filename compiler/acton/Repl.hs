{-# LANGUAGE CPP #-}
-- Copyright (C) 2026 Centor AB
--
-- Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
--
-- 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
--
-- 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
--
-- 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

module Repl(Hooks(..), runRepl) where

import Prelude hiding (readFile, writeFile)

import qualified Acton.Parser
import qualified Acton.Syntax as A
import qualified Acton.BuildSpec as BuildSpec
import qualified Acton.CommandLineParser as C
import Acton.Compile (ProjectError(..), findProjectDir)
import qualified Acton.Fingerprint as Fingerprint
import qualified PkgCommands
import FileUtil (readFile, writeFile, writeFileChanged, writeFileIfChanged)

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Exception (IOException, SomeException, finally, fromException, try)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Char (isSpace)
import qualified Data.List
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Time.Clock (UTCTime)
import System.Directory
import qualified System.Environment
import System.Exit
import System.FilePath ((</>), (<.>), joinPath, takeDirectory)
import System.IO (BufferMode(..), Handle, hClose, hGetLine, hIsTerminalDevice, hPutStr, hPutStrLn, hSetBuffering, stderr, stdin)
import qualified System.Info
import System.Process
import System.Timeout (timeout)
#ifndef mingw32_HOST_OS
import System.Posix.Signals (Signal, signalProcessGroup, sigKILL, sigTERM)
#endif
import qualified System.Console.Haskeline as HL

data Hooks = Hooks
  { hooksCacheDir                         :: IO FilePath
  , hooksWithPersistentScratchDirLock     :: FilePath -> (FilePath -> IO ()) -> IO ()
  , hooksCompileFiles                     :: C.GlobalOptions -> C.CompileOptions -> FilePath -> [FilePath] -> IO Bool
  }


data ReplState = ReplState
  { replImports      :: [String]
  , replDecls        :: [ReplDecl]
  , replReplay       :: [String]
  , replDeps         :: M.Map String BuildSpec.PkgDep
  , replUseProject   :: Bool
  }

data ReplDecl = ReplDecl [String] String

data ReplEval = ReplExpr String | ReplStmt String | ReplNoop

data ReplInput = ReplTop ReplTop | ReplEvalInput ReplEval

data ReplTop = ReplImport
             | ReplDeclTop [String]
             | ReplReplay

data ReplContext = ReplContext
  { replRoot           :: FilePath
  , replProject        :: Maybe FilePath
  , replMainSrcFile    :: FilePath
  , replSessionSrcFile :: FilePath
  , replEvalSrcFile    :: FilePath
  , replBinFile        :: FilePath
  , replGopts          :: C.GlobalOptions
  , replOpts           :: C.CompileOptions
  , replWarmup         :: MVar Bool
  , replUseZigWatch    :: Bool
  , replZigWatch       :: MVar (Maybe ReplZigWatch)
  , replHooks          :: Hooks
  }

data ReplZigWatch = ReplZigWatch
  { zwProcess      :: ProcessHandle
  , zwResults      :: Chan Bool
  }

data FileStamp = FileStamp UTCTime Integer deriving (Eq, Show)

runRepl :: Hooks -> C.GlobalOptions -> C.CompileOptions -> IO ()
runRepl hooks gopts opts = do
    curDir <- getCurrentDirectory
    mproj0 <- findProjectDir curDir
    mproj <- mapM canonicalizePath mproj0
    useZigWatch <- replZigWatchEnabled
    withReplRoot hooks opts mproj $ \scratchDir -> do
      warmup <- newEmptyMVar
      zigWatch <- newMVar Nothing
      let srcRoot = scratchDir </> "src"
          mainSrcFile = srcRoot </> replMainModuleName <.> "act"
          sessionSrcFile = srcRoot </> replSessionModuleName <.> "act"
          evalSrcFile = srcRoot </> replEvalModuleName <.> "act"
          opts' = normalizeReplCompileOptions opts
          gopts' = gopts { C.quiet = not (C.verbose gopts)
                         , C.noProgress = True
                         }
          binBase = if isWindowsOS (C.target opts') then replMainModuleName ++ ".exe" else replMainModuleName
          ctx = ReplContext
                { replRoot = scratchDir
                , replProject = mproj
                , replMainSrcFile = mainSrcFile
                , replSessionSrcFile = sessionSrcFile
                , replEvalSrcFile = evalSrcFile
                , replBinFile = scratchDir </> "out" </> "bin" </> binBase
                , replGopts = gopts'
                , replOpts = opts'
                , replWarmup = warmup
                , replUseZigWatch = useZigWatch
                , replZigWatch = zigWatch
                , replHooks = hooks
                }
      createDirectoryIfMissing True srcRoot
      startReplWarmup ctx
      unless (C.quiet gopts) $ do
        putStrLn "Acton REPL. Type :help for commands, :quit to exit."
        when (C.verbose gopts) $
          putStrLn ("REPL directory: " ++ scratchDir)
      cacheDir <- hooksCacheDir hooks
      HL.runInputT (replSettings cacheDir) (HL.withInterrupt (replLoop ctx emptyReplState))
        `finally` finishReplWarmup ctx

replZigWatchEnabled :: IO Bool
replZigWatchEnabled = do
    tty <- hIsTerminalDevice stdin
    enabled <- System.Environment.lookupEnv "ACTON_REPL_ZIG_WATCH"
    return (tty && enabled `elem` [Just "1", Just "true", Just "yes"])

normalizeReplCompileOptions :: C.CompileOptions -> C.CompileOptions
normalizeReplCompileOptions opts =
    opts { C.parse = False
         , C.parse_ast = False
         , C.kinds = False
         , C.types = False
         , C.sigs = False
         , C.norm = False
         , C.deact = False
         , C.cps = False
         , C.llift = False
         , C.box = False
         , C.hgen = False
         , C.cgen = False
         , C.ccmd = False
         , C.ty = False
         , C.root = replMainModuleName ++ ".main"
         , C.skip_build = False
         , C.only_build = False
         , C.watch = False
         , C.test = False
         }

startReplWarmup :: ReplContext -> IO ()
startReplWarmup ctx = do
    _ <- forkIO $ do
      res <- try (compileReplRunnerNow ctx emptyReplState ReplNoop) :: IO (Either SomeException Bool)
      ok <- case res of
              Left err -> hPutStrLn stderr (show err) >> return False
              Right ok -> return ok
      putMVar (replWarmup ctx) ok
    return ()

waitReplWarmup :: ReplContext -> IO Bool
waitReplWarmup ctx = readMVar (replWarmup ctx)

finishReplWarmup :: ReplContext -> IO ()
finishReplWarmup ctx = do
    void (readMVar (replWarmup ctx))
    stopReplZigWatch ctx

startReplZigWatch :: ReplContext -> IO Bool
startReplZigWatch ctx =
    if not (replUseZigWatch ctx)
      then return False
      else modifyMVar (replZigWatch ctx) $ \old -> do
        stopZigWatch old
        mnew <- startZigWatchProcess ctx
        case mnew of
          Just zw -> return (Just zw, True)
          Nothing -> return (Nothing, False)

stopReplZigWatch :: ReplContext -> IO ()
stopReplZigWatch ctx =
    modifyMVar_ (replZigWatch ctx) $ \old -> do
      stopZigWatch old
      return Nothing

stopZigWatch :: Maybe ReplZigWatch -> IO ()
stopZigWatch Nothing = return ()
stopZigWatch (Just zw) = do
    terminateZigWatch zw
    done <- timeout (5 * 1000 * 1000) (void (waitForProcess (zwProcess zw)))
    case done of
      Just _ -> return ()
      Nothing -> do
        forceZigWatch zw
        void (waitForProcess (zwProcess zw)) `catchIO` \_ -> return ()

#ifndef mingw32_HOST_OS
terminateZigWatch :: ReplZigWatch -> IO ()
terminateZigWatch zw = terminateZigWatchWith sigTERM zw

forceZigWatch :: ReplZigWatch -> IO ()
forceZigWatch zw = terminateZigWatchWith sigKILL zw

terminateZigWatchWith :: Signal -> ReplZigWatch -> IO ()
terminateZigWatchWith sig zw = do
    mpid <- getPid (zwProcess zw)
    case mpid of
      Just pid -> signalProcessGroup sig pid `catchIO` \_ -> terminateProcess (zwProcess zw)
      Nothing -> terminateProcess (zwProcess zw)
#else
terminateZigWatch :: ReplZigWatch -> IO ()
terminateZigWatch zw = terminateProcess (zwProcess zw)

forceZigWatch :: ReplZigWatch -> IO ()
forceZigWatch zw = terminateProcess (zwProcess zw)
#endif

startZigWatchProcess :: ReplContext -> IO (Maybe ReplZigWatch)
startZigWatchProcess ctx = do
    zigExe <- replZigExe ctx
    zigArgs <- replZigWatchArgs ctx
    zigEnv <- replZigEnv
    let cp = (proc zigExe zigArgs)
              { cwd = Just (replRoot ctx)
              , env = Just zigEnv
              , std_in = NoStream
              , std_out = CreatePipe
              , std_err = CreatePipe
              , create_group = True
              }
    res <- try (createProcess cp) :: IO (Either IOException (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle))
    case res of
      Left err -> do
        hPutStrLn stderr ("Failed to start Zig watch: " ++ show err)
        return Nothing
      Right (_, mout, merr, ph) -> do
        results <- newChan
        let zw = ReplZigWatch ph results
        forM_ mout $ \h -> do
          hSetBuffering h LineBuffering
          void (forkIO (readZigWatchOutput ctx zw h))
        forM_ merr $ \h -> do
          hSetBuffering h LineBuffering
          void (forkIO (readZigWatchOutput ctx zw h))
        void (forkIO $ do
          ec <- waitForProcess ph
          case ec of
            ExitSuccess -> return ()
            ExitFailure _ -> writeChan results False)
        return (Just zw)

readZigWatchOutput :: ReplContext -> ReplZigWatch -> Handle -> IO ()
readZigWatchOutput ctx zw h = go `finally` hClose h
  where
    go = do
      lineRes <- try (hGetLine h) :: IO (Either IOException String)
      case lineRes of
        Left _ -> return ()
        Right line -> do
          when (C.verboseZig (replGopts ctx)) $
            hPutStrLn stderr line
          case zigWatchSummary line of
            Nothing -> return ()
            Just ok -> writeChan (zwResults zw) ok
          go

zigWatchSummary :: String -> Maybe Bool
zigWatchSummary line
  | "Build Summary:" `isInfixOf` line =
      Just ("steps succeeded" `isInfixOf` line &&
            not ("failed" `isInfixOf` line) &&
            not ("error" `isInfixOf` line))
  | otherwise = Nothing

waitZigWatchSince :: ReplContext -> ReplZigWatch -> [(FilePath, Maybe FileStamp)] -> IO Bool
waitZigWatchSince ctx zw old = do
    res <- timeout zigWatchTimeoutMicros waitLoop
    case res of
      Just ok -> return ok
      Nothing -> do
        when (replWatchDebug ctx) $
          hPutStrLn stderr "Timed out waiting for Zig watch build; falling back to a normal build."
        return False
  where
    waitLoop = do
      changed <- outputStampsChanged old
      if changed
        then return True
        else do
          mres <- timeout (100 * 1000) (readChan (zwResults zw))
          case mres of
            Just False -> do
              when (not (C.quiet (replGopts ctx))) $
                hPutStrLn stderr "Zig watch build failed; falling back to a normal build."
              return False
            _ -> waitLoop

    outputStampsChanged stamps = do
      current <- mapM (\(path, _) -> do
                         stamp <- readFileStamp path
                         return (path, stamp)
                      ) stamps
      return (all stampChanged (zip stamps current))

    stampChanged ((_, oldStamp), (_, newStamp)) =
      case newStamp of
        Nothing -> False
        Just _  -> newStamp /= oldStamp

currentZigWatch :: ReplContext -> IO (Maybe ReplZigWatch)
currentZigWatch ctx = readMVar (replZigWatch ctx)

waitReplZigWatchChanged :: ReplContext -> Maybe ReplZigWatch -> [(FilePath, Maybe FileStamp)] -> IO Bool
waitReplZigWatchChanged _ Nothing _ = return False
waitReplZigWatchChanged ctx (Just zw) old = waitZigWatchSince ctx zw old

zigWatchTimeoutMicros :: Int
zigWatchTimeoutMicros = 250 * 1000

replWatchDebug :: ReplContext -> Bool
replWatchDebug ctx = C.verbose (replGopts ctx) || C.verboseZig (replGopts ctx)

catchIO :: IO a -> (IOException -> IO a) -> IO a
catchIO action handler = do
    res <- try action
    case res of
      Left err -> handler err
      Right ok -> return ok

replZigExe :: ReplContext -> IO FilePath
replZigExe ctx = do
    actonExe <- System.Environment.getExecutablePath
    sys <- canonicalizePath (if null (C.syspath (replOpts ctx))
                               then takeDirectory actonExe </> ".."
                               else C.syspath (replOpts ctx))
    return (sys </> "zig" </> "zig")

replZigEnv :: IO [(String, String)]
replZigEnv = do
    env0 <- System.Environment.getEnvironment
    homeDir <- getHomeDirectory
    let env1 = if System.Info.os == "darwin" && not (any ((== "DEVELOPER_DIR") . fst) env0)
               then ("DEVELOPER_DIR", "/dev/null") : env0
               else env0
        zigEnv = [ ("ZIG_LOCAL_CACHE_DIR", joinPath [homeDir, ".cache", "acton", "zig-local-cache"])
                 , ("ZIG_GLOBAL_CACHE_DIR", joinPath [homeDir, ".cache", "acton", "zig-global-cache"])
                 ]
    return (foldr (\(k, v) acc -> (k, v) : filter ((/= k) . fst) acc) env1 zigEnv)

replZigWatchArgs :: ReplContext -> IO [String]
replZigWatchArgs ctx = do
    homeDir <- getHomeDirectory
    let opts = replOpts ctx
        localCache = joinPath [homeDir, ".cache", "acton", "zig-local-cache"]
        globalCache = joinPath [homeDir, ".cache", "acton", "zig-global-cache"]
        noThreads = isWindowsOS (C.target opts) || C.no_threads opts
        baseArgs = [ "build"
                   , replEvalModuleName
                   , replSessionModuleName
                   , "--watch"
                   , "--debounce", "0"
                   , "-fincremental"
                   , "--summary", "all"
                   , "--color", "off"
                   , "--cache-dir", localCache
                   , "--global-cache-dir", globalCache
                   , "--prefix", replRoot ctx </> "out"
                   , "--prefix-exe-dir", "bin"
                   ]
        targetArgs = [ "-Dtarget=" ++ C.target opts ]
        cpuArgs =
          if C.cpu opts /= "" then ["-Dcpu=" ++ C.cpu opts]
          else case splitOn "-" (C.target opts) of
                 ("native":_)          -> []
                 ("aarch64":"macos":_) -> ["-Dcpu=apple_m1"]
                 ("aarch64":_)         -> ["-Dcpu=generic+crypto"]
                 ("x86_64":_)          -> ["-Dcpu=x86_64_v2+aes"]
                 _                     -> []
        optArgs = [ "-Doptimize=" ++ replOptimizeModeToZig (C.optimize opts) ]
        moduleArgs =
          [ "-Dacton_modules=out/types/" ++ replMainModuleName ++ ".c"
          , "-Dacton_root_stubs=out/types/" ++ replMainModuleName ++ ".root.c"
          , "-Dacton_libraries=" ++ Data.List.intercalate "|"
              [ replEvalModuleName ++ ":dynamic:out/types/" ++ replEvalModuleName ++ ".c:"
              , replSessionModuleName ++ ":dynamic:out/types/" ++ replSessionModuleName ++ ".c:"
              ]
          ]
        featureArgs = concat [ if C.db opts then ["-Ddb"] else []
                             , if noThreads then ["-Dno_threads"] else []
                             , if C.cpedantic opts then ["-Dcpedantic"] else []
                             ]
    return (baseArgs ++ targetArgs ++ cpuArgs ++ optArgs ++ moduleArgs ++ featureArgs)

replOptimizeModeToZig :: C.OptimizeMode -> String
replOptimizeModeToZig C.Debug        = "Debug"
replOptimizeModeToZig C.ReleaseSafe  = "ReleaseSafe"
replOptimizeModeToZig C.ReleaseSmall = "ReleaseSmall"
replOptimizeModeToZig C.ReleaseFast  = "ReleaseFast"

withReplRoot :: Hooks -> C.CompileOptions -> Maybe FilePath -> (FilePath -> IO ()) -> IO ()
withReplRoot hooks opts mproj action
  | C.tempdir opts /= "" = do
      createDirectoryIfMissing True (C.tempdir opts)
      let root = C.tempdir opts </> replExplicitRootName
      createDirectoryIfMissing True root
      action root
  | otherwise = do
      cacheDir <- hooksCacheDir hooks
      hooksWithPersistentScratchDirLock hooks (cacheDir </> "repl" </> replScratchKey mproj) action

replExplicitRootName :: FilePath
replExplicitRootName = ".acton-repl"

replScratchKey :: Maybe FilePath -> FilePath
replScratchKey Nothing = "global"
replScratchKey (Just p) = "project-" ++ drop 2 (Fingerprint.formatFingerprintPrefix (Fingerprint.fingerprintPrefixForName p))

replSettings :: FilePath -> HL.Settings IO
replSettings cacheDir =
    (HL.defaultSettings :: HL.Settings IO)
      { HL.historyFile = Just (cacheDir </> "repl_history")
      , HL.complete = replCompletion
      }

replCompletion :: HL.CompletionFunc IO
replCompletion =
    HL.completeWordWithPrev Nothing " \t" completeReplWord

completeReplWord :: String -> String -> IO [HL.Completion]
completeReplWord prevRev word
  | prevWords == [":dep"] = return (matching replDepCompletions)
  | null prevWords && ":" `isPrefixOf` word = return (matching replCommandCompletions)
  | otherwise = return []
  where
    prevWords = words (reverse prevRev)
    matching candidates =
      map HL.simpleCompletion (filter (word `isPrefixOf`) candidates)

replCommandCompletions :: [String]
replCommandCompletions =
    [ ":help", ":quit", ":q", ":reset", ":show", ":dep" ]

replDepCompletions :: [String]
replDepCompletions =
    [ "list", "add", "rm", "remove" ]

replMainModuleName :: String
replMainModuleName = "repl_main"

replSessionModuleName :: String
replSessionModuleName = "repl_session"

replEvalModuleName :: String
replEvalModuleName = "repl_eval"

isWindowsOS :: String -> Bool
isWindowsOS targetTriple = case splitOn "-" targetTriple of
    (_:os:_) -> os == "windows"
    _        -> False

emptyReplState :: ReplState
emptyReplState = ReplState [] [] [] M.empty False

renderReplBuildAct :: M.Map String BuildSpec.PkgDep -> Maybe FilePath -> String
renderReplBuildAct deps mproj =
    BuildSpec.renderBuildAct BuildSpec.BuildSpec
      { BuildSpec.specName = "acton_repl"
      , BuildSpec.specDescription = Nothing
      , BuildSpec.fingerprint = "0x81091a566daa4f54"
      , BuildSpec.dependencies = deps'
      , BuildSpec.zig_dependencies = M.empty
      , BuildSpec.libraries = M.fromList
          [ (replEvalModuleName, BuildSpec.Library [replEvalModuleName] "dynamic")
          , (replSessionModuleName, BuildSpec.Library [replSessionModuleName] "dynamic")
          ]
      }
  where
    deps' =
      case mproj of
        Nothing -> deps
        Just p ->
          M.insert "repl_project" replProjectDep deps
          where
            replProjectDep = BuildSpec.PkgDep
              { BuildSpec.url = Nothing
              , BuildSpec.hash = Nothing
              , BuildSpec.path = Just p
              , BuildSpec.repo_url = Nothing
              , BuildSpec.repo_ref = Nothing
              }

replLoop :: ReplContext -> ReplState -> HL.InputT IO ()
replLoop ctx st = do
    HL.handleInterrupt (liftIO (putStrLn "") >> replLoop ctx st) $ do
      mraw <- readReplInput "acton> "
      case mraw of
        Nothing -> liftIO (putStrLn "")
        Just raw -> do
          block <- readReplContinuation raw
          handleReplInput ctx st block

readReplInput :: String -> HL.InputT IO (Maybe String)
readReplInput prompt = HL.getInputLine prompt

readReplContinuation :: String -> HL.InputT IO String
readReplContinuation first
  | ":" `isPrefixOf` trim first = return first
  | replStartsBlock first = go [first]
  | otherwise = return first
  where
    go acc = do
      mline <- readReplInput "... "
      case mline of
        Nothing -> return (unlines (reverse acc))
        Just line
          | null (trim line) -> return (unlines (reverse acc))
          | otherwise -> go (line : acc)

handleReplInput :: ReplContext -> ReplState -> String -> HL.InputT IO ()
handleReplInput ctx st raw =
    case words (trim raw) of
      [] -> replLoop ctx st
      [":q"] -> return ()
      [":quit"] -> return ()
      [":help"] -> liftIO printReplHelp >> replLoop ctx st
      [":reset"] -> do
        ok <- liftIO $ compileReplRunner ctx emptyReplState
        if ok
          then do
            liftIO $ putStrLn "Session reset."
            replLoop ctx emptyReplState
          else do
            liftIO $ hPutStrLn stderr "Session reset failed."
            replLoop ctx st
      [":show"] -> do
        liftIO $ putStr (renderReplState st)
        replLoop ctx st
      ":dep":args ->
        handleReplDep ctx st args
      _ | ":" `isPrefixOf` trim raw -> do
            liftIO $ hPutStrLn stderr ("Unknown REPL command: " ++ trim raw)
            replLoop ctx st
        | otherwise -> do
            input <- liftIO $ classifyReplInput st raw
            case input of
              ReplTop top -> acceptReplTop ctx st raw top
              ReplEvalInput eval -> runReplEval ctx st eval

printReplHelp :: IO ()
printReplHelp = do
    putStrLn "Commands:"
    putStrLn "  :help          Show this help"
    putStrLn "  :quit, :q      Exit"
    putStrLn "  :reset         Clear retained imports, definitions, and dependencies"
    putStrLn "  :show          Show retained session source"
    putStrLn "  :dep list      Show online package dependencies"
    putStrLn "  :dep add NAME  Add an online package dependency"
    putStrLn "  :dep rm NAME   Remove an online package dependency"
    putStrLn ""
    putStrLn "Definitions, imports, actors, and classes are retained as session source."
    putStrLn "Top-level assignments are setup lines: they are replayed before each evaluation."
    putStrLn "Expressions display non-None results. Blocks ending in ':' read continuation lines until a blank line."

handleReplDep :: ReplContext -> ReplState -> [String] -> HL.InputT IO ()
handleReplDep ctx st args =
    case args of
      [] -> liftIO (printReplDeps st) >> replLoop ctx st
      ["list"] -> liftIO (printReplDeps st) >> replLoop ctx st
      ["add", depName] -> addReplDep ctx st depName
      ["rm", depName] -> removeReplDep ctx st depName
      ["remove", depName] -> removeReplDep ctx st depName
      _ -> do
        liftIO $ hPutStrLn stderr "Usage: :dep list | :dep add NAME | :dep rm NAME"
        replLoop ctx st

printReplDeps :: ReplState -> IO ()
printReplDeps st =
    if M.null (replDeps st)
      then putStrLn "No dependencies."
      else forM_ (M.toList (replDeps st)) $ \(name, dep) ->
             putStrLn (name ++ renderDepSource dep)
  where
    renderDepSource dep =
      case BuildSpec.repo_url dep of
        Just repo -> " (" ++ repo ++ ")"
        Nothing -> ""

addReplDep :: ReplContext -> ReplState -> String -> HL.InputT IO ()
addReplDep ctx st depName = do
    res <- liftIO (try (PkgCommands.resolveLibraryDependency depName) :: IO (Either ProjectError BuildSpec.PkgDep))
    case res of
      Left (ProjectError msg) -> do
        liftIO $ hPutStrLn stderr msg
        replLoop ctx st
      Right dep -> do
        let st' = st { replDeps = M.insert depName dep (replDeps st) }
        ok <- liftIO $ compileReplRunner ctx st'
        if ok
          then do
            liftIO $ putStrLn ("Added dependency " ++ depName)
            replLoop ctx st'
          else do
            liftIO $ hPutStrLn stderr ("Dependency " ++ depName ++ " was not added.")
            replLoop ctx st

removeReplDep :: ReplContext -> ReplState -> String -> HL.InputT IO ()
removeReplDep ctx st depName =
    if M.member depName (replDeps st)
      then do
        let st' = st { replDeps = M.delete depName (replDeps st) }
        ok <- liftIO $ compileReplRunner ctx st'
        if ok
          then do
            liftIO $ putStrLn ("Removed dependency " ++ depName)
            replLoop ctx st'
          else do
            liftIO $ hPutStrLn stderr ("Dependency " ++ depName ++ " was not removed.")
            replLoop ctx st
      else do
        liftIO $ putStrLn ("Dependency " ++ depName ++ " not found.")
        replLoop ctx st

acceptReplTop :: ReplContext -> ReplState -> String -> ReplTop -> HL.InputT IO ()
acceptReplTop ctx st src top = do
    let st' = addReplTop st top src
    okRunner <- liftIO $
      case top of
        ReplImport -> compileReplRunner ctx st'
        _ | replUseProject st' && not (replUseProject st) -> compileReplRunner ctx st'
        _ -> return True
    if not okRunner
      then do
        liftIO $ hPutStrLn stderr "Input was not added to the session."
        replLoop ctx st
      else case top of
        ReplImport -> replLoop ctx st'
        ReplReplay -> do
          ok <- liftIO $ compileReplEval ctx st' ReplNoop
          if ok
            then do
              ran <- liftIO $ runReplBinary ctx
              if ran
                then replLoop ctx st'
                else do
                  liftIO $ hPutStrLn stderr "Input was not added to the session."
                  replLoop ctx st
            else do
              liftIO $ hPutStrLn stderr "Input was not added to the session."
              replLoop ctx st
        _ -> do
          ok <- liftIO $ compileReplSession ctx st'
          if ok
            then replLoop ctx st'
            else do
              liftIO $ hPutStrLn stderr "Input was not added to the session."
              replLoop ctx st

runReplEval :: ReplContext -> ReplState -> ReplEval -> HL.InputT IO ()
runReplEval ctx st eval = do
    ok <- liftIO $ compileReplEval ctx st eval
    when ok (void (liftIO $ runReplBinary ctx))
    replLoop ctx st

compileReplSession :: ReplContext -> ReplState -> IO Bool
compileReplSession ctx st = do
    warmed <- waitReplWarmup ctx
    ensureReplBuildAct ctx st
    createDirectoryIfMissing True (takeDirectory (replSessionSrcFile ctx))
    sessionChanged <- writeFileChanged (replSessionSrcFile ctx) (renderReplSessionSource st)
    if warmed
      then do
        let opts = (replOpts ctx) { C.root = "", C.skip_build = replUseZigWatch ctx }
        if replUseZigWatch ctx
          then compileReplFilesWithZigWatch ctx opts
                 (if sessionChanged then [replSessionSrcFile ctx] else [])
                 (if sessionChanged then [replSessionModuleName] else [])
                 (compileReplFiles ctx (opts { C.skip_build = False }) [replSessionSrcFile ctx])
          else compileReplFiles ctx opts [replSessionSrcFile ctx]
      else compileReplRunnerNow ctx st ReplNoop

compileReplEval :: ReplContext -> ReplState -> ReplEval -> IO Bool
compileReplEval ctx st eval = do
    warmed <- waitReplWarmup ctx
    ensureReplBuildAct ctx st
    createDirectoryIfMissing True (takeDirectory (replEvalSrcFile ctx))
    sessionChanged <- writeFileChanged (replSessionSrcFile ctx) (renderReplSessionSource st)
    evalChanged <- writeFileChanged (replEvalSrcFile ctx) (renderReplEvalSource st eval)
    if warmed
      then do
        let opts = (replOpts ctx) { C.root = "", C.skip_build = replUseZigWatch ctx }
            srcFiles = (if sessionChanged then [replSessionSrcFile ctx] else []) ++
                       (if evalChanged then [replEvalSrcFile ctx] else [])
            mods = (if sessionChanged then [replSessionModuleName] else []) ++
                   (if evalChanged then [replEvalModuleName] else [])
        if replUseZigWatch ctx
          then compileReplFilesWithZigWatch ctx opts srcFiles mods
                 (compileReplEvalNoWatch ctx opts sessionChanged evalChanged)
          else do
            compileReplEvalNoWatch ctx opts sessionChanged evalChanged
      else compileReplRunnerNow ctx st eval

compileReplEvalNoWatch :: ReplContext -> C.CompileOptions -> Bool -> Bool -> IO Bool
compileReplEvalNoWatch ctx opts sessionChanged evalChanged = do
    let opts' = opts { C.skip_build = False }
    sessionOk <- if sessionChanged
                   then compileReplFiles ctx opts' [replSessionSrcFile ctx]
                   else return True
    if sessionOk && (sessionChanged || evalChanged)
      then compileReplFiles ctx opts' [replEvalSrcFile ctx]
      else return sessionOk

compileReplRunner :: ReplContext -> ReplState -> IO Bool
compileReplRunner ctx st = do
    _ <- waitReplWarmup ctx
    compileReplRunnerNow ctx st ReplNoop

compileReplRunnerNow :: ReplContext -> ReplState -> ReplEval -> IO Bool
compileReplRunnerNow ctx st eval = do
    stopReplZigWatch ctx
    ensureReplBuildAct ctx st
    createDirectoryIfMissing True (takeDirectory (replMainSrcFile ctx))
    writeFileIfChanged (replMainSrcFile ctx) (renderReplMainSource st)
    writeFileIfChanged (replSessionSrcFile ctx) (renderReplSessionSource st)
    writeFileIfChanged (replEvalSrcFile ctx) (renderReplEvalSource st eval)
    ok <- compileReplFiles ctx (replOpts ctx) [replMainSrcFile ctx]
    when (ok && replUseZigWatch ctx) (void (startReplZigWatch ctx))
    return ok

ensureReplBuildAct :: ReplContext -> ReplState -> IO ()
ensureReplBuildAct ctx st =
    writeFileIfChanged (replRoot ctx </> "Build.act") (renderReplBuildAct (replDeps st) mproj)
  where
    mproj
      | replUseProject st = replProject ctx
      | otherwise         = Nothing

compileReplFiles :: ReplContext -> C.CompileOptions -> [FilePath] -> IO Bool
compileReplFiles ctx opts srcFiles = do
    res <- try (hooksCompileFiles (replHooks ctx) (replGopts ctx) opts (replRoot ctx) srcFiles) :: IO (Either SomeException Bool)
    case res of
      Left err ->
        case fromException err of
          Just ExitSuccess -> return True
          Just (ExitFailure _) -> return False
          Nothing -> hPutStrLn stderr (show err) >> return False
      Right hadErrors -> return (not hadErrors)

compileReplFilesWithZigWatch :: ReplContext -> C.CompileOptions -> [FilePath] -> [String] -> IO Bool -> IO Bool
compileReplFilesWithZigWatch ctx opts srcFiles modNames fallback = do
    if null srcFiles
      then return True
      else do
        mwatch <- currentZigWatch ctx
        before <- readDynamicLibraryOutputStamps ctx modNames
        ok <- compileReplFiles ctx opts srcFiles
        if not ok
          then return False
          else do
            watched <- waitReplZigWatchChanged ctx mwatch before
            if watched
              then return True
              else do
                stopReplZigWatch ctx
                fallbackOk <- fallback
                when fallbackOk (void (startReplZigWatch ctx))
                return fallbackOk

readDynamicLibraryOutputStamps :: ReplContext -> [String] -> IO [(FilePath, Maybe FileStamp)]
readDynamicLibraryOutputStamps ctx modNames =
    mapM stampPath (map (dynamicLibraryOutputPath ctx) modNames)
  where
    stampPath path = do
      stamp <- readFileStamp path
      return (path, stamp)

dynamicLibraryOutputPath :: ReplContext -> String -> FilePath
dynamicLibraryOutputPath ctx modName =
    replRoot ctx </> "out" </> "lib" </> dynamicLibraryFileName (C.target (replOpts ctx)) modName

dynamicLibraryFileName :: String -> String -> FilePath
dynamicLibraryFileName target modName
  | isWindowsOS target = modName <.> "dll"
  | otherwise          = "lib" ++ modName <.> dynamicLibraryExtension target

dynamicLibraryExtension :: String -> String
dynamicLibraryExtension target
  | isMacOS target = "dylib"
  | otherwise      = "so"

isMacOS :: String -> Bool
isMacOS targetTriple
  | targetTriple == "native" = System.Info.os == "darwin"
  | otherwise = "macos" `elem` parts || "darwin" `elem` parts
  where
    parts = splitOn "-" targetTriple

readFileStamp :: FilePath -> IO (Maybe FileStamp)
readFileStamp path = do
    res <- (try $ do
      exists <- doesFileExist path
      if not exists
        then return Nothing
        else do
          t <- getModificationTime path
          sz <- getFileSize path
          return (Just (FileStamp t sz))
      ) :: IO (Either IOException (Maybe FileStamp))
    case res of
      Left _ -> return Nothing
      Right stamp -> return stamp

runReplBinary :: ReplContext -> IO Bool
runReplBinary ctx = do
    exists <- doesFileExist (replBinFile ctx)
    if not exists
      then hPutStrLn stderr ("REPL binary not found: " ++ replBinFile ctx) >> return False
      else do
        (ec, out, err) <- readCreateProcessWithExitCode (proc (replBinFile ctx) []){ cwd = Just (replRoot ctx) } ""
        putStr out
        hPutStr stderr err
        case ec of
          ExitSuccess -> return True
          ExitFailure code -> hPutStrLn stderr ("Process exited with status " ++ show code) >> return False

renderReplState :: ReplState -> String
renderReplState st =
    let body = replImports st ++ map replDeclSource (replDecls st) ++ replReplay st
    in if null body
         then "<empty>\n"
         else unlines (concatMap blockLines body)

renderReplSessionSource :: ReplState -> String
renderReplSessionSource st =
    unlines (concatMap blockLines (replImports st ++ map replDeclSource (replDecls st)))

renderReplMainSource :: ReplState -> String
renderReplMainSource st =
    unlines (concatMap blockLines (replImports st) ++
      [ "import " ++ replEvalModuleName
      , ""
      , "actor main(env: Env):"
      , "    try:"
      , "        " ++ replEvalModuleName ++ ".eval()"
      , "        env.exit(0)"
      , "    except Exception as exc:"
      , "        print(exc)"
      , "        env.exit(1)"
      ])

renderReplEvalPlaceholderSource :: String
renderReplEvalPlaceholderSource =
    unlines
      [ "from " ++ replSessionModuleName ++ " import *"
      , ""
      , "proc def eval():"
      , "    pass"
      ]

renderReplEvalSource :: ReplState -> ReplEval -> String
renderReplEvalSource st eval =
    renderReplEvalImports st
    ++ "proc def eval():\n"
    ++ renderReplEvalBody st eval

renderReplEvalImports :: ReplState -> String
renderReplEvalImports st =
    unlines (concatMap blockLines (replImports st) ++ ["from " ++ replSessionModuleName ++ " import *", ""])

renderReplEvalBody :: ReplState -> ReplEval -> String
renderReplEvalBody st eval =
    case replay ++ current of
      []    -> "    pass\n"
      lines -> unlines (map ("    " ++) lines)
  where
    replay = concatMap blockLines (replReplay st)
    current =
      case eval of
        ReplExpr src -> renderReplExprBody src
        ReplStmt src -> blockLines src
        ReplNoop    -> []

renderReplExprBody :: String -> [String]
renderReplExprBody src =
    [ replValueName ++ " = " ++ trim src
    , "if " ++ replValueName ++ " is not None:"
    , "    print(" ++ replValueName ++ ")"
    ]

replValueName :: String
replValueName = "__repl_value"

addReplTop :: ReplState -> ReplTop -> String -> ReplState
addReplTop st top src =
    case top of
      ReplImport -> st { replImports = replImports st ++ [src], replUseProject = True }
      ReplDeclTop ns -> st { replDecls = replaceReplDecl ns src (replDecls st) }
      ReplReplay -> st { replReplay = replReplay st ++ [src] }

replaceReplDecl :: [String] -> String -> [ReplDecl] -> [ReplDecl]
replaceReplDecl ns src decls
  | null ns   = decls ++ [ReplDecl ns src]
  | otherwise = filter (not . any (`elem` ns) . replDeclNames) decls ++ [ReplDecl ns src]

replDeclNames :: ReplDecl -> [String]
replDeclNames (ReplDecl ns _) = ns

replDeclSource :: ReplDecl -> String
replDeclSource (ReplDecl _ src) = src

classifyReplInput :: ReplState -> String -> IO ReplInput
classifyReplInput _ src = do
    moduleRes <- parseReplModule src
    case moduleRes of
      Right (A.Module _ is _ body)
        | not (null is) && null body -> return (ReplTop ReplImport)
        | null is && replAllDecls body ->
            return (ReplTop (ReplDeclTop (concatMap replStmtNames body)))
        | null is && replAllReplay body -> return (ReplTop ReplReplay)
      _ -> do
        exprRes <- parseReplExpr src
        case exprRes of
          Right _ -> return (ReplEvalInput (ReplExpr src))
          Left _  -> return (ReplEvalInput (ReplStmt src))

parseReplModule :: String -> IO (Either SomeException A.Module)
parseReplModule src =
    try (Acton.Parser.parseModule (A.modName [replEvalModuleName]) "repl_input.act" src Nothing)

parseReplExpr :: String -> IO (Either SomeException A.Expr)
parseReplExpr src =
    try (Acton.Parser.parseExpression "repl_input.act" src)

replAllDecls :: A.Suite -> Bool
replAllDecls ss = not (null ss) && all replIsDeclStmt ss

replIsDeclStmt :: A.Stmt -> Bool
replIsDeclStmt A.Signature{} = True
replIsDeclStmt A.Decl{}      = True
replIsDeclStmt _             = False

replAllReplay :: A.Suite -> Bool
replAllReplay ss = not (null ss) && all replIsReplayStmt ss

replIsReplayStmt :: A.Stmt -> Bool
replIsReplayStmt A.Assign{} = True
replIsReplayStmt _          = False

replStmtNames :: A.Stmt -> [String]
replStmtNames A.Signature{A.vars=ns} = map A.rawstr ns
replStmtNames A.Decl{A.decls=ds}     = map (A.rawstr . A.dname) ds
replStmtNames _                      = []

replStartsBlock :: String -> Bool
replStartsBlock src = ":" `isSuffixOf` trimRight src

blockLines :: String -> [String]
blockLines src = lines (trimRight src)

trim :: String -> String
trim = trimRight . dropWhile isSpace

trimRight :: String -> String
trimRight = Data.List.dropWhileEnd isSpace
