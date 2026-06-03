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
import Control.Concurrent.MVar
import Control.Exception (IOException, SomeException, finally, fromException, try)
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Char (isSpace)
import qualified Data.List
import Data.List (isPrefixOf, isSuffixOf)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import System.Directory
import System.Exit
import System.FilePath ((</>), (<.>), joinPath, takeDirectory)
import System.IO (hFlush, hIsTerminalDevice, hPutStr, hPutStrLn, isEOF, stderr, stdin, stdout)
import System.Process
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
  , replHooks          :: Hooks
  }

data ReplShell m = ReplShell
  { replReadLine        :: String -> m (Maybe String)
  , replHandleInterrupt :: m () -> m () -> m ()
  }

runRepl :: Hooks -> C.GlobalOptions -> C.CompileOptions -> IO ()
runRepl hooks gopts opts = do
    curDir <- getCurrentDirectory
    mproj0 <- findProjectDir curDir
    mproj <- mapM canonicalizePath mproj0
    withReplRoot hooks opts mproj $ \scratchDir -> do
      interactive <- hIsTerminalDevice stdin
      warmup <- if interactive
                  then newEmptyMVar
                  else newMVar False
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
                , replHooks = hooks
                }
      createDirectoryIfMissing True srcRoot
      when interactive (startReplWarmup ctx)
      when (interactive && not (C.quiet gopts)) $ do
        putStrLn "Acton REPL. Type :help for commands, :quit to exit."
        when (C.verbose gopts) $
          putStrLn ("REPL directory: " ++ scratchDir)
      cacheDir <- hooksCacheDir hooks
      (if interactive
         then HL.runInputT (replSettings cacheDir) (HL.withInterrupt (replLoop haskelineReplShell ctx emptyReplState))
         else replLoop plainReplShell ctx emptyReplState)
        `finally` finishReplWarmup ctx

haskelineReplShell :: ReplShell (HL.InputT IO)
haskelineReplShell = ReplShell
  { replReadLine = HL.getInputLine
  , replHandleInterrupt = HL.handleInterrupt
  }

plainReplShell :: ReplShell IO
plainReplShell = ReplShell
  { replReadLine = plainGetInputLine
  , replHandleInterrupt = \_ body -> body
  }

plainGetInputLine :: String -> IO (Maybe String)
plainGetInputLine prompt = do
    hPutStr stdout prompt
    hFlush stdout
    eof <- isEOF
    if eof
      then return Nothing
      else Just <$> getLine

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
         , C.tydb = False
         , C.root = replMainModuleName ++ ".main"
         , C.skip_build = False
         , C.only_build = False
         , C.watch = False
         , C.test = False
         }

startReplWarmup :: ReplContext -> IO ()
startReplWarmup ctx = do
    _ <- forkIO $ do
      res <- try (compileReplRunnerRaw ctx emptyReplState ReplNoop) :: IO (Either SomeException Bool)
      ok <- case res of
              Left err -> hPutStrLn stderr (show err) >> return False
              Right ok -> return ok
      putMVar (replWarmup ctx) ok
    return ()

waitReplWarmup :: ReplContext -> IO Bool
waitReplWarmup ctx = readMVar (replWarmup ctx)

finishReplWarmup :: ReplContext -> IO ()
finishReplWarmup ctx = void (readMVar (replWarmup ctx))

markReplWarmup :: ReplContext -> IO ()
markReplWarmup ctx =
    modifyMVar_ (replWarmup ctx) (\_ -> return True)

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

replLoop :: MonadIO m => ReplShell m -> ReplContext -> ReplState -> m ()
replLoop shell ctx st = do
    replHandleInterrupt shell (liftIO (putStrLn "") >> replLoop shell ctx st) $ do
      mraw <- readReplInput shell "acton> "
      case mraw of
        Nothing -> liftIO (putStrLn "")
        Just raw -> do
          block <- readReplContinuation shell raw
          handleReplInput shell ctx st block

readReplInput :: MonadIO m => ReplShell m -> String -> m (Maybe String)
readReplInput shell prompt = replReadLine shell prompt

readReplContinuation :: MonadIO m => ReplShell m -> String -> m String
readReplContinuation shell first
  | ":" `isPrefixOf` trim first = return first
  | replStartsBlock first = go [first]
  | otherwise = return first
  where
    go acc = do
      mline <- readReplInput shell "... "
      case mline of
        Nothing -> return (unlines (reverse acc))
        Just line
          | null (trim line) -> return (unlines (reverse acc))
          | otherwise -> go (line : acc)

handleReplInput :: MonadIO m => ReplShell m -> ReplContext -> ReplState -> String -> m ()
handleReplInput shell ctx st raw =
    case words (trim raw) of
      [] -> replLoop shell ctx st
      [":q"] -> return ()
      [":quit"] -> return ()
      [":help"] -> liftIO printReplHelp >> replLoop shell ctx st
      [":reset"] -> do
        ok <- liftIO $ compileReplRunner ctx emptyReplState
        if ok
          then do
            liftIO $ putStrLn "Session reset."
            replLoop shell ctx emptyReplState
          else do
            liftIO $ hPutStrLn stderr "Session reset failed."
            replLoop shell ctx st
      [":show"] -> do
        liftIO $ putStr (renderReplState st)
        replLoop shell ctx st
      ":dep":args ->
        handleReplDep shell ctx st args
      _ | ":" `isPrefixOf` trim raw -> do
            liftIO $ hPutStrLn stderr ("Unknown REPL command: " ++ trim raw)
            replLoop shell ctx st
        | otherwise -> do
            input <- liftIO $ classifyReplInput st raw
            case input of
              ReplTop top -> acceptReplTop shell ctx st raw top
              ReplEvalInput eval -> runReplEval shell ctx st eval

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

handleReplDep :: MonadIO m => ReplShell m -> ReplContext -> ReplState -> [String] -> m ()
handleReplDep shell ctx st args =
    case args of
      [] -> liftIO (printReplDeps st) >> replLoop shell ctx st
      ["list"] -> liftIO (printReplDeps st) >> replLoop shell ctx st
      ["add", depName] -> addReplDep shell ctx st depName
      ["rm", depName] -> removeReplDep shell ctx st depName
      ["remove", depName] -> removeReplDep shell ctx st depName
      _ -> do
        liftIO $ hPutStrLn stderr "Usage: :dep list | :dep add NAME | :dep rm NAME"
        replLoop shell ctx st

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

addReplDep :: MonadIO m => ReplShell m -> ReplContext -> ReplState -> String -> m ()
addReplDep shell ctx st depName = do
    res <- liftIO (try (PkgCommands.resolveLibraryDependency depName) :: IO (Either ProjectError BuildSpec.PkgDep))
    case res of
      Left (ProjectError msg) -> do
        liftIO $ hPutStrLn stderr msg
        replLoop shell ctx st
      Right dep -> do
        let st' = st { replDeps = M.insert depName dep (replDeps st) }
        ok <- liftIO $ compileReplRunner ctx st'
        if ok
          then do
            liftIO $ putStrLn ("Added dependency " ++ depName)
            replLoop shell ctx st'
          else do
            liftIO $ hPutStrLn stderr ("Dependency " ++ depName ++ " was not added.")
            replLoop shell ctx st

removeReplDep :: MonadIO m => ReplShell m -> ReplContext -> ReplState -> String -> m ()
removeReplDep shell ctx st depName =
    if M.member depName (replDeps st)
      then do
        let st' = st { replDeps = M.delete depName (replDeps st) }
        ok <- liftIO $ compileReplRunner ctx st'
        if ok
          then do
            liftIO $ putStrLn ("Removed dependency " ++ depName)
            replLoop shell ctx st'
          else do
            liftIO $ hPutStrLn stderr ("Dependency " ++ depName ++ " was not removed.")
            replLoop shell ctx st
      else do
        liftIO $ putStrLn ("Dependency " ++ depName ++ " not found.")
        replLoop shell ctx st

acceptReplTop :: MonadIO m => ReplShell m -> ReplContext -> ReplState -> String -> ReplTop -> m ()
acceptReplTop shell ctx st src top = do
    let st' = addReplTop st top src
    okRunner <- liftIO $
      case top of
        ReplImport -> compileReplRunner ctx st'
        _ | replUseProject st' && not (replUseProject st) -> compileReplRunner ctx st'
        _ -> return True
    if not okRunner
      then do
        liftIO $ hPutStrLn stderr "Input was not added to the session."
        replLoop shell ctx st
      else case top of
        ReplImport -> replLoop shell ctx st'
        ReplReplay -> do
          ok <- liftIO $ compileReplEval ctx st' ReplNoop
          if ok
            then do
              ran <- liftIO $ runReplBinary ctx
              if ran
                then replLoop shell ctx st'
                else do
                  liftIO $ hPutStrLn stderr "Input was not added to the session."
                  replLoop shell ctx st
            else do
              liftIO $ hPutStrLn stderr "Input was not added to the session."
              replLoop shell ctx st
        _ -> do
          ok <- liftIO $ compileReplSession ctx st'
          if ok
            then replLoop shell ctx st'
            else do
              liftIO $ hPutStrLn stderr "Input was not added to the session."
              replLoop shell ctx st

runReplEval :: MonadIO m => ReplShell m -> ReplContext -> ReplState -> ReplEval -> m ()
runReplEval shell ctx st eval = do
    ok <- liftIO $ compileReplEval ctx st eval
    when ok (void (liftIO $ runReplBinary ctx))
    replLoop shell ctx st

compileReplSession :: ReplContext -> ReplState -> IO Bool
compileReplSession ctx st = do
    warmed <- waitReplWarmup ctx
    ensureReplBuildAct ctx st
    createDirectoryIfMissing True (takeDirectory (replSessionSrcFile ctx))
    writeFileIfChanged (replSessionSrcFile ctx) (renderReplSessionSource st)
    if warmed
      then do
        let opts = (replOpts ctx) { C.root = "" }
        compileReplFiles ctx opts [replSessionSrcFile ctx]
      else compileReplRunnerNow ctx st ReplNoop

compileReplEval :: ReplContext -> ReplState -> ReplEval -> IO Bool
compileReplEval ctx st eval = do
    warmed <- waitReplWarmup ctx
    ensureReplBuildAct ctx st
    createDirectoryIfMissing True (takeDirectory (replEvalSrcFile ctx))
    sessionChanged <- writeFileChanged (replSessionSrcFile ctx) (renderReplSessionSource st)
    writeFile (replEvalSrcFile ctx) (renderReplEvalSource st eval)
    if warmed
      then do
        let opts = (replOpts ctx) { C.root = "" }
        sessionOk <- if sessionChanged
                       then compileReplFiles ctx opts [replSessionSrcFile ctx]
                       else return True
        if sessionOk
          then compileReplFiles ctx opts [replEvalSrcFile ctx]
          else return False
      else compileReplRunnerNow ctx st eval

compileReplRunner :: ReplContext -> ReplState -> IO Bool
compileReplRunner ctx st = do
    _ <- waitReplWarmup ctx
    compileReplRunnerNow ctx st ReplNoop

compileReplRunnerNow :: ReplContext -> ReplState -> ReplEval -> IO Bool
compileReplRunnerNow ctx st eval = do
    ok <- compileReplRunnerRaw ctx st eval
    when ok (markReplWarmup ctx)
    return ok

compileReplRunnerRaw :: ReplContext -> ReplState -> ReplEval -> IO Bool
compileReplRunnerRaw ctx st eval = do
    ensureReplBuildAct ctx st
    createDirectoryIfMissing True (takeDirectory (replMainSrcFile ctx))
    writeFileIfChanged (replMainSrcFile ctx) (renderReplMainSource st)
    writeFileIfChanged (replSessionSrcFile ctx) (renderReplSessionSource st)
    writeFileIfChanged (replEvalSrcFile ctx) (renderReplEvalSource st eval)
    compileReplFiles ctx (replOpts ctx) [replMainSrcFile ctx]

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
