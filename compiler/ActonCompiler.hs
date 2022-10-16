{-# LANGUAGE CPP #-}
-- Copyright (C) 2019-2021 Data Ductus AB
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

module Main where

import Prelude hiding (readFile, writeFile)

import qualified Acton.Parser
import qualified Acton.Syntax as A
import qualified Acton.CommandLineParser as C
import qualified Acton.Relabel
import qualified Acton.Env
import qualified Acton.QuickType
import qualified Acton.Kinds
import qualified Acton.Types
import qualified Acton.Solver
import qualified Acton.Normalizer
import qualified Acton.CPS
import qualified Acton.Deactorizer
import qualified Acton.LambdaLifter
import qualified Acton.CodeGen
import qualified Acton.Builtin
import Utils
import qualified Pretty
import qualified InterfaceFiles

import Control.Applicative
import Control.Exception (throw,catch,displayException,IOException,ErrorCall)
import Control.Monad
import Data.List.Split
import Data.Monoid ((<>))
import Data.Ord
import Data.Graph
import Data.Version (showVersion)
import qualified Data.List
import qualified Filesystem.Path.CurrentOS as Fsco
import System.Clock
import System.Directory.Recursive
import System.FileLock
import System.IO hiding (readFile, writeFile)
import System.IO.Temp
import System.Info
import System.Directory
import System.Exit
import System.Posix.Files
import System.Process
import System.FilePath.Posix
import qualified System.Environment
import qualified System.Exit
import qualified Paths_acton
import Text.Printf


main                     =  do arg <- C.parseCmdLine
                               case arg of
                                   C.VersionOpt opts       -> printVersion opts
                                   C.CmdOpt (C.New opts)   -> createProject (C.file opts)
                                   C.CmdOpt (C.Build opts) -> buildProject $ defaultOpts {C.dev = C.devB opts, C.root = C.rootB opts, C.quiet = C.quietB opts}
                                   C.CmdOpt (C.Cloud opts) -> undefined
                                   C.CmdOpt (C.Doc opts)   -> printDocs opts
                                   C.CompileOpt nms opts   -> compileFiles opts (catMaybes $ map filterActFile nms)

defaultOpts   = C.CompileOptions False False False False False False False False False False
                                 False False False False False False False False "" "" ""


-- Auxiliary functions ---------------------------------------------------------------------------------------

dump h txt      = putStrLn ("\n\n#################################### " ++ h ++ ":\n" ++ txt)

getModPath :: FilePath -> A.ModName -> FilePath
getModPath path mn =
     joinPath [path, joinPath $ init $ A.modPath mn]


printErrorAndExit msg = do
                  errorWithoutStackTrace msg
                  System.Exit.exitFailure
 
-- our own readFile & writeFile with hard-coded utf-8 encoding
readFile f = do
    h <- openFile f ReadMode
    hSetEncoding h utf8
    c <- hGetContents h
    return c

writeFile :: FilePath -> String -> IO ()
writeFile f c = do
    h <- openFile f WriteMode
    hSetEncoding h utf8
    hPutStr h c
    hClose h

fmtTime t =
    printf "%6.3f s" secs
  where
    secs :: Float
    secs = (fromIntegral(sec t)) + (fromIntegral (nsec t) / 1000000000)

-- Version handling ------------------------------------------------------------------------------------------

printVersion opts = do
    cv <-  getCcVer
    iff (C.version opts) (putStrLn (showVer cv))
    iff (C.numeric_version opts) (putStrLn getVer)
 
getVer          = showVersion Paths_acton.version
getVerExtra     = unwords ["compiled by", compilerName, showVersion compilerVersion, "on", os, arch]

getCcVer        = do
    verStr <- readProcess "cc" ["--version"] []
                `catch` handleNoCc                    -- NOTE: the error is not handled (but actonc would terminate anyhow)
    return $ unwords $ take 1 $ lines verStr
  where handleNoCc :: IOException -> IO String
        handleNoCc e = printErrorAndExit "ERROR: Unable to find cc (the C compiler)\nHINT: Ensure cc is in your PATH"


showVer cv      = "acton " ++ getVer ++ "\n" ++ getVerExtra ++ "\ncc: " ++ cv

printIce errMsg = do ccVer <- getCcVer
                     putStrLn(
                        "ERROR: internal compiler error: " ++ errMsg ++
                        "\nNOTE: this is likely a bug in actonc, please report this at:" ++
                        "\nNOTE: https://github.com/actonlang/acton/issues/new?template=ice.md" ++
                        "\nNOTE: acton " ++ getVer ++ " " ++ getVerExtra ++
                        "\nNOTE: cc: " ++ ccVer
                        )

-- Create a project ---------------------------------------------------------------------------------------------

createProject :: String -> IO ()
createProject name = do
    curDir <- getCurrentDirectory
    projDirExists <- doesDirectoryExist name
    iff (projDirExists) $
        printErrorAndExit ("Unable to create project " ++ name ++ ", directory already exists.")
    createDirectoryIfMissing True name
    writeFile (joinPath [ curDir, name, "Acton.toml" ]) ""
    paths <- findPaths (joinPath [ curDir, name, "Acton.toml" ]) defaultOpts
    writeFile (joinPath [ curDir, name, "README.org" ]) (
      "* " ++ name ++ "\n" ++ name ++ " is a cool Acton project!\n\n\n"
      ++ "** Compile\n\n#+BEGIN_SRC shell\nactonc build\n#+END_SRC\n\n\n"
      ++ "** Run\n\n#+BEGIN_SRC shell\nout/rel/bin/" ++ name ++ "\n#+END_SRC\n\n"
      )
    createDirectoryIfMissing True (srcDir paths)
    writeFile (joinPath [(srcDir paths), name ++ ".act"]) "#\n#\n\nactor main(env):\n    print(\"Hello World!\")\n    await async env.exit(0)\n"
    putStrLn("Created project " ++ name)
    putStrLn("Enter your new project directory with:\n  cd " ++ name)
    putStrLn("Compile:\n  actonc build")
    putStrLn("Run:\n  ./out/rel/bin/" ++ name)

-- Build a project -----------------------------------------------------------------------------------------------

buildProject :: C.CompileOptions -> IO ()
buildProject opts = do
                iff (not (null $ (C.root opts)) && (length $ splitOn "." (C.root opts)) == 1) $
                    printErrorAndExit("Project build requires a qualified root actor name, like foo.main")
                    
                -- find all .act files in src/ directory, parse into tasks and
                -- submit for compilation
                curDir <- getCurrentDirectory
                paths <- findPaths (joinPath [ curDir, "Acton.toml" ]) opts
                srcDirExists <- doesDirectoryExist (srcDir paths)
                if not srcDirExists
                  then printErrorAndExit"Missing src/ directory"
                  else do
                    -- grab project lock
                    lockFile (joinPath [projPath paths, ".actonc.lock"]) Exclusive
                    allFiles <- getFilesRecursive (srcDir paths)
                    let srcFiles = catMaybes $ map filterActFile allFiles
                    compileFiles opts srcFiles

-- Print documentation -------------------------------------------------------------------------------------------

printDocs :: C.DocOptions -> IO ()
printDocs opts = do
              iff (not (null $ C.signs opts)) $ do
                     let filename = C.signs opts
                         (fileBody,fileExt) = splitExtension $ takeFileName filename
                     case fileExt of
                            ".ty" -> do
                                paths <- findPaths filename defaultOpts
                                env0 <- Acton.Env.initEnv (sysTypes paths) False
                                Acton.Types.showTyFile (Acton.Env.setMod (modName paths) env0) filename
                            _     -> printErrorAndExit ("Unknown filetype: " ++ filename)
              iff (not (null $ C.full opts)) $
                   putStrLn "Full documentation not implemented"           -- issue #708


-- Compile Acton files ---------------------------------------------------------------------------------------------

compileFiles :: C.CompileOptions -> [String] -> IO ()
compileFiles opts srcFiles = do
    -- it is ok to get paths from just the first file here since at this point
    -- we only care about project level path stuff and all source files are
    -- known to be in the same project
    paths <- findPaths (head srcFiles) opts
    iff (not(C.quiet opts)) $ do
        if isTmp paths
          then
            putStrLn("Building file " ++ head srcFiles)
          else
            putStrLn("Building project in " ++ projPath paths)

    when (C.debug opts) $ do
        putStrLn ("  Paths:")
        putStrLn ("    sysPath  : " ++ sysPath paths)
        putStrLn ("    sysTypes : " ++ sysTypes paths)
        putStrLn ("    sysLib   : " ++ sysLib paths)
        putStrLn ("    projPath : " ++ projPath paths)
        putStrLn ("    projOut  : " ++ projOut paths)
        putStrLn ("    projTypes: " ++ projTypes paths)
        putStrLn ("    projLib  : " ++ projLib paths)
        putStrLn ("    binDir   : " ++ binDir paths)
        putStrLn ("    srcDir   : " ++ srcDir paths)
        iff (length srcFiles == 1) (putStrLn ("    modName  : " ++ prstr (modName paths)))
    tasks <- mapM (parseActFile opts paths) srcFiles
    -- figure out binTasks, currently just based on --root
    let rootParts = splitOn "." (C.root opts)
        rootMod   = init rootParts
        guessMod  = if length rootParts == 1 then modName paths else A.modName rootMod
        binTask   = BinTask False (prstr guessMod) (A.GName guessMod (A.name $ last rootParts))
        binTasks
          | null (C.root opts) = map (\t -> BinTask True (prstr (name t)) (A.GName (name t) (A.name "main"))) (filter (not . stubmode) tasks)
          | otherwise        = [binTask]
    compileTasks opts paths tasks binTasks

-- Paths handling -------------------------------------------------------------------------------------

data Paths      = Paths {
                    sysPath     :: FilePath,
                    sysTypes    :: FilePath,
                    sysLib      :: FilePath,
                    projPath    :: FilePath,
                    projOut     :: FilePath,
                    projTypes   :: FilePath,
                    projLib     :: FilePath,
                    binDir      :: FilePath,
                    srcDir      :: FilePath,
                    isTmp       :: Bool,
                    rmTmp       :: Bool,
                    fileExt     :: String,
                    modName     :: A.ModName
                  }

-- Given a FILE and optionally --syspath PATH:
-- 'sysPath' is the path to the system directory as given by PATH, defaulting to the actonc executable directory.
-- 'sysTypes' is directory "types" under 'sysPath'.
-- 'sysLib' is directory "lib" under 'sysPath'.
-- 'projPath' is the closest parent directory of FILE that contains an 'Acton.toml' file, or a temporary directory in "/tmp" if no such parent exists.
-- 'projOut' is directory "out" under 'projPath'.
-- 'projTypes' is directory "types" under 'projOut'.
-- 'projLib' is directory "lib" under 'projOut'.
-- 'binDir' is the directory prefix of FILE if 'projPath' is temporary, otherwise it is directory "bin" under 'projOut'
-- 'srcDir' is the directory prefix of FILE if 'projPath' is temporary, otherwise it is directory "src" under 'projPath'
-- 'fileExt' is file suffix of FILE.
-- 'modName' is the module name of FILE (its path after 'src' except 'fileExt', split at every '/')

srcFile                 :: Paths -> A.ModName -> FilePath
srcFile paths mn        = joinPath (srcDir paths : A.modPath mn) ++ ".act"

outBase                 :: Paths -> A.ModName -> FilePath
outBase paths mn        = joinPath (projTypes paths : A.modPath mn)

srcBase                 :: Paths -> A.ModName -> FilePath
srcBase paths mn        = joinPath (srcDir paths : A.modPath mn)

findPaths               :: FilePath -> C.CompileOptions -> IO Paths
findPaths actFile opts  = do execDir <- takeDirectory <$> System.Environment.getExecutablePath
                             sysPath <- canonicalizePath (if null $ C.syspath opts then execDir ++ "/.." else C.syspath opts)
                             let sysTypes = joinPath [sysPath, "types"]
                             let sysLib = joinPath [sysPath, "lib/" ++ if (C.dev opts) then "dev" else "rel"]
                             absSrcFile <- canonicalizePath actFile
                             (isTmp, rmTmp, projPath, dirInSrc) <- analyze (takeDirectory absSrcFile) []
                             let sysTypes = joinPath [sysPath, "types"]
                                 srcDir  = if isTmp then takeDirectory absSrcFile else joinPath [projPath, "src"]
                                 projOut = joinPath [projPath, "out"]
                                 projProfile = joinPath [projOut, if (C.dev opts) then "dev" else "rel"]
                                 projLib = joinPath [projProfile, "lib"]
                                 projTypes = joinPath [projOut, "types"]
                                 binDir  = if isTmp then srcDir else joinPath [projProfile, "bin"]
                                 modName = A.modName $ dirInSrc ++ [fileBody]
                             createDirectoryIfMissing True binDir
                             createDirectoryIfMissing True projOut
                             createDirectoryIfMissing True projTypes
                             createDirectoryIfMissing True projLib
                             createDirectoryIfMissing True (getModPath projTypes modName)
                             return $ Paths sysPath sysTypes sysLib projPath projOut projTypes projLib binDir srcDir isTmp rmTmp fileExt modName
  where (fileBody,fileExt) = splitExtension $ takeFileName actFile

        analyze "/" ds  = do let rmTmp = if (null $ C.tempdir opts) then True else False
                             tmp <- if (null $ C.tempdir opts) then createTempDirectory (joinPath ["/", "tmp"]) "actonc" else canonicalizePath (C.tempdir opts)
                             createDirectoryIfMissing True tmp
                             return (True, rmTmp, tmp, [])
        analyze pre ds  = do exists <- doesFileExist (joinPath [pre, "Acton.toml"])
                             if not exists 
                                then analyze (takeDirectory pre) (takeFileName pre : ds)
                                else do
-- TODO: reimplement this check where it makes sense
--                                    when (take 1 ds /= ["src"]) $ error ("************* Project source file is not in 'src' directory")
                                    return $ (False, False, pre, drop 1 ds)


-- Handling Acton files -----------------------------------------------------------------------------

filterActFile :: FilePath -> Maybe FilePath
filterActFile file =
    case fileExt of
        ".act" -> Just file
        _ -> Nothing
  where (fileBody, fileExt) = splitExtension $ takeFileName file

checkIsActonFile :: [Char] -> IO Bool
checkIsActonFile nm = do
  fileExist <- System.Directory.doesFileExist nm
  return (cmdExt == ".act" && fileExist)
  where (cmdBody,cmdExt) = splitExtension $ takeFileName nm


parseActFile :: C.CompileOptions -> Paths -> String -> IO CompileTask
parseActFile opts paths actFile = do
                    timeStart <- getTime Monotonic
                    paths <- findPaths actFile opts
                    src <- readFile actFile
                    timeRead <- getTime Monotonic
                    iff (C.timing opts) $ putStrLn("Reading file " ++ makeRelative (srcDir paths) actFile 
                                                   ++ ": " ++ fmtTime(timeRead - timeStart))
                    m <- Acton.Parser.parseModule (modName paths) actFile src
                      `catch` handle "Syntax error" Acton.Parser.parserError "" paths (modName paths)
                      `catch` handle "Context error" Acton.Parser.contextError src paths (modName paths)
                      `catch` handle "Indentation error" Acton.Parser.indentationError src paths (modName paths)
                      `catch` handle "Syntax error" Acton.Parser.failFastError src paths (modName paths)
                    iff (C.parse opts) $ dump "parse" (Pretty.print m)
                    timeParse <- getTime Monotonic
                    iff (C.timing opts) $ putStrLn("Parsing file " ++ makeRelative (srcDir paths) actFile
                                                                   ++ ": " ++ fmtTime(timeParse - timeRead))
                    stubMode <- detectStubMode paths actFile opts
                    return $ ActonTask (modName paths) src m stubMode
    where detectStubMode :: Paths -> String -> C.CompileOptions -> IO Bool
          detectStubMode paths srcfile opts = do
                    exists <- doesFileExist cFile
                    when (exists && C.debug opts) $ do putStrLn("Found matching C file (" ++ makeRelative (srcDir paths) cFile
                                                        ++ "), assuming stub compilation for " ++ makeRelative (srcDir paths) srcfile)
                    return ((takeFileName srcfile) == "__builtin__.act" || C.stub opts || exists)
              where cFile = replaceExtension srcfile ".c"


-- Compilation tasks, chasing imported modules, compilation and building executables -------------------------------------------

data CompileTask        = ActonTask { name :: A.ModName, src :: String, atree:: A.Module, stubmode :: Bool } deriving (Show)
data BinTask            = BinTask { isDefaultRoot :: Bool, binName :: String, rootActor :: A.QName } deriving (Show)

importsOf :: CompileTask -> [A.ModName]
importsOf t = A.importsOf (atree t)

compileTasks :: C.CompileOptions -> Paths -> [CompileTask] -> [BinTask] -> IO ()
compileTasks opts paths tasks binTasks
                       = do tasks <- chaseImportedFiles opts paths tasks
                            let sccs = stronglyConnComp  [(t,name t,importsOf t) | t <- tasks]
                                (as,cs) = Data.List.partition isAcyclic sccs
                            -- show modules to compile and in which order
                            --putStrLn(concatMap showTaskGraph sccs)
                            if null cs
                             then do env0 <- Acton.Env.initEnv (sysTypes paths) (modName paths == Acton.Builtin.mBuiltin)
                                     env1 <- foldM (doTask opts paths) env0 [t | AcyclicSCC t <- as]
                                     mapM (buildExecutable env1 opts paths) binTasks
                                     when (rmTmp paths) $ removeDirectoryRecursive (projPath paths)
                                     return ()
                              else printErrorAndExit ("Cyclic imports: "++concatMap showTaskGraph cs)
  where isAcyclic (AcyclicSCC _) = True
        isAcyclic _              = False
        showTaskGraph ts         = "\n"++concatMap (\t-> concat (intersperse "." (A.modPath (name t)))++" ") ts


chaseImportedFiles :: C.CompileOptions -> Paths -> [CompileTask] -> IO [CompileTask]
chaseImportedFiles opts paths itasks
                            = do
                                 let itasks_imps = concatMap importsOf itasks
                                 newtasks <- catMaybes <$> mapM (readAFile itasks) itasks_imps
                                 chaseRecursively (itasks ++ newtasks) (map name newtasks) (concatMap importsOf newtasks)

  where readAFile tasks mn  = case lookUp mn tasks of    -- read and parse file mn in the project directory, unless it is already in tasks 
                                 Just t -> return Nothing
                                 Nothing -> do let actFile = srcFile paths mn
                                               ok <- System.Directory.doesFileExist actFile
                                               if ok then do
                                                   task <- parseActFile opts paths actFile
                                                   return $ Just task
                                                 else return Nothing

        lookUp mn (t : ts)
          | name t == mn     = Just t
          | otherwise        = lookUp mn ts
        lookUp _ []          = Nothing
        
        chaseRecursively tasks mns []
                             = return tasks
        chaseRecursively tasks mns (imn : imns)
                             = if imn `elem` mns
                                then chaseRecursively tasks mns imns
                                else do t <- readAFile tasks imn
                                        chaseRecursively (maybe tasks (:tasks) t)
                                                         (imn:mns)
                                                         (imns ++ concatMap importsOf t)


doTask :: C.CompileOptions -> Paths -> Acton.Env.Env0 -> CompileTask -> IO Acton.Env.Env0
doTask opts paths env t@(ActonTask mn src m stubMode) = do
    iff (not (C.quiet opts))  (putStrLn("  Compiling " ++ makeRelative (srcDir paths) actFile
              ++ (if (C.dev opts) then " for development" else " for release")
              ++ (if stubMode then " in stub mode" else "")))

    timeStart <- getTime Monotonic
    -- run custom make target compilation for modules implemented in C
    -- Note how this does not include .ext.c style modules
    iff stubMode $ do
        runCustomMake paths mn
        timeCustomMake <- getTime Monotonic
        iff (C.timing opts) $ putStrLn("    Custom make           : " ++ fmtTime(timeCustomMake - timeStart))

    -- no need to list the cFile since it is intermediate; oFile is final output
    let outFiles = [tyFile, hFile] ++ if stubMode then [] else [oFile]
    ok <- checkUptoDate paths actFile outFiles (importsOf t)
    if ok && not (forceCompilation opts)
      then do
        iff (C.debug opts) (putStrLn ("    Skipping " ++ makeRelative (srcDir paths) actFile ++ " (files are up to date).") >> hFlush stdout)
        timeBeforeTy <- getTime Monotonic
        te <- InterfaceFiles.readFile tyFile
        timeReadTy <- getTime Monotonic
        iff (C.timing opts) $ putStrLn("Read .ty file " ++ makeRelative (projPath paths) tyFile ++ ": " ++ fmtTime(timeReadTy - timeBeforeTy))
        timeEnd <- getTime Monotonic
        iff (not (C.quiet opts)) $ putStrLn("   Already up to date, in   " ++ fmtTime(timeEnd - timeStart))
        return (Acton.Env.addMod mn te env)
      else do
        createDirectoryIfMissing True (getModPath (projTypes paths) mn)
        (env',te) <- runRestPasses opts paths env m stubMode
          `catch` handle "Compilation error" generalError src paths mn
          `catch` handle "Compilation error" Acton.Env.compilationError src paths mn
          `catch` handle "Type error" Acton.Types.typeError src paths mn
        timeEnd <- getTime Monotonic
        iff (not (C.quiet opts)) $ putStrLn("   Finished compilation in  " ++ fmtTime(timeEnd - timeStart))
        return (Acton.Env.addMod mn te env')
  where actFile             = srcFile paths mn
        outbase             = outBase paths mn
        tyFile              = outbase ++ ".ty"
        hFile               = outbase ++ ".h"
        cFile               = outbase ++ ".c"
        oFile               = joinPath [projLib paths, prstr mn] ++  ".o"
        forceCompilation :: C.CompileOptions -> Bool
        forceCompilation args = (C.parse args) || (C.kinds args) || (C.types args) || (C.sigs args)  ||  (C.norm args) 
                              || (C.deact args) || (C.cps args)  || (C.llift args) || (C.hgen args)  ||(C.cgen args)

runCustomMake paths mn = do
    -- copy header file in place, if it exists
    let srcH = replaceExtension actFile ".h"
    hSrcExist <- doesFileExist srcH
    let hFile = outbase ++ ".h"
    iff (hSrcExist) $ do
        hFileExist <- doesFileExist hFile
        if hFileExist
          then do
            hSame <- cmpFiles srcH hFile
            iff (not hSame) $ do
                copyFile srcH hFile
          else
            copyFile srcH hFile

    -- run custom make target, if we find a Makefile
    -- since we have no visibility into make target dependencies, we always
    -- run custom make targets
    let makeFile = projPath paths ++ "/Makefile"
    makeExist <- doesFileExist makeFile
    iff (makeExist) (do
      cExist <- doesFileExist $ replaceExtension actFile ".c"
      iff (cExist) (do
        let roFile = makeRelative (projPath paths) oFile
            aFile = joinPath [projLib paths, "libActonProject.a"]
            makeCmd = "make " ++ roFile
            arCmd = "ar rcs " ++ aFile ++ " " ++ oFile
        (returnCode, makeStdout, makeStderr) <- readCreateProcessWithExitCode (shell $ makeCmd ++ " && " ++ arCmd){ cwd = Just (projPath paths) } ""
        case returnCode of
            ExitSuccess -> return()
            ExitFailure _ -> do printIce "compilation of C code failed"
                                putStrLn $ "make stdout:\n" ++ makeStdout
                                putStrLn $ "make stderr:\n" ++ makeStderr
                                System.Exit.exitFailure)
                    )
  where actFile             = srcFile paths mn
        outbase             = outBase paths mn
        oFile               = joinPath [projLib paths, prstr mn] ++  ".o"
        cmpFiles a b        = liftM2 (==) (readFile a) (readFile b)


checkUptoDate :: Paths -> FilePath -> [FilePath] -> [A.ModName] -> IO Bool
checkUptoDate paths actFile outFiles imps = do
    srcFiles  <- filterM System.Directory.doesFileExist potSrcFiles
    outExists <- mapM System.Directory.doesFileExist outFiles
    if not (and outExists)
        then return False
        else do
            -- get the time of the last modified source file
            srcTime  <- head <$> sortBy (comparing Down) <$> mapM System.Directory.getModificationTime srcFiles
            outTiming <- mapM System.Directory.getModificationTime outFiles
            impsOK   <- mapM (impOK (head outTiming)) imps
            return (all (srcTime <) outTiming && and impsOK)
  where
        srcBase         = joinPath [takeDirectory actFile, takeBaseName actFile]
        srcCFile        = srcBase ++ ".c"
        srcHFile        = srcBase ++ ".h"
        extCFile        = srcBase ++ ".ext.c"
        -- except for actFile, these are *potential* source files which might
        -- not actually exist...
        potSrcFiles     = [actFile, extCFile, srcCFile, srcHFile]
        impOK iTime mn  = do
                             impFile <- findTy paths mn
                             impfileTime <- System.Directory.getModificationTime impFile
                             return (impfileTime < iTime)
        -- find .ty file by looking both in local project and in stdlib
        findTy paths mn = do
                             let localImpName = outBase paths mn ++ ".ty"
                                 stdlibImpName = joinPath (sysTypes paths : A.modPath mn) ++ ".ty"
                             projExist <- System.Directory.doesFileExist localImpName
                             stdlibExist <- System.Directory.doesFileExist stdlibImpName
                             let filePath = case (projExist, stdlibExist) of
                                   (True, True) -> localImpName
                                   (True, False) -> localImpName
                                   (False, True) -> stdlibImpName
                                   (False, False) -> error("ERROR: Unable to find interface file")
                             return filePath


runRestPasses :: C.CompileOptions -> Paths -> Acton.Env.Env0 -> A.Module -> Bool -> IO (Acton.Env.Env0, Acton.Env.TEnv)
runRestPasses opts paths env0 parsed stubMode = do
                      let outbase = outBase paths (A.modname parsed)
                      let absSrcBase = srcBase paths (A.modname parsed)
                      let relSrcBase = makeRelative (projPath paths) (srcBase paths (A.modname parsed))
                      let actFile = absSrcBase ++ ".act"

                      timeStart <- getTime Monotonic

                      envTmp <- Acton.Env.mkEnv (sysTypes paths) (projTypes paths) env0 parsed
                      let env = envTmp { Acton.Env.stub = stubMode }
                      timeEnv <- getTime Monotonic
                      iff (C.timing opts) $ putStrLn("    Pass: Make environment: " ++ fmtTime (timeEnv - timeStart))

                      kchecked <- Acton.Kinds.check env parsed
                      iff (C.kinds opts) $ dump "kinds" (Pretty.print kchecked)
                      timeKindsCheck <- getTime Monotonic
                      iff (C.timing opts) $ putStrLn("    Pass: Kinds check     : " ++ fmtTime (timeKindsCheck - timeEnv))

                      (iface,tchecked,typeEnv) <- Acton.Types.reconstruct outbase env kchecked
                      iff (C.types opts) $ dump "types" (Pretty.print tchecked)
                      iff (C.sigs opts) $ dump "sigs" (Pretty.vprint (A.imps tchecked) ++ "\n\n" ++ Pretty.vprint iface ++ "\n")
                      timeTypeCheck <- getTime Monotonic
                      iff (C.timing opts) $ putStrLn("    Pass: Type check      : " ++ fmtTime (timeTypeCheck - timeKindsCheck))

                      (normalized, normEnv) <- Acton.Normalizer.normalize typeEnv tchecked
                      iff (C.norm opts) $ dump "norm" (Pretty.print normalized)
                      --traceM ("#################### normalized env0:")
                      --traceM (Pretty.render (Pretty.pretty normEnv))
                      timeNormalized <- getTime Monotonic
                      iff (C.timing opts) $ putStrLn("    Pass: Normalizer      : " ++ fmtTime (timeNormalized - timeTypeCheck))

                      (deacted,deactEnv) <- Acton.Deactorizer.deactorize normEnv normalized
                      iff (C.deact opts) $ dump "deact" (Pretty.print deacted)
                      --traceM ("#################### deacted env0:")
                      --traceM (Pretty.render (Pretty.pretty deactEnv))
                      timeDeactorizer <- getTime Monotonic
                      iff (C.timing opts) $ putStrLn("    Pass: Deactorizer     : " ++ fmtTime (timeDeactorizer - timeNormalized))

                      (cpstyled,cpsEnv) <- Acton.CPS.convert deactEnv deacted
                      iff (C.cps opts) $ dump "cps" (Pretty.print cpstyled)
                      --traceM ("#################### cps'ed env0:")
                      --traceM (Pretty.render (Pretty.pretty cpsEnv))
                      timeCPS <- getTime Monotonic
                      iff (C.timing opts) $ putStrLn("    Pass: CPS             : " ++ fmtTime (timeCPS - timeDeactorizer))

                      (lifted,liftEnv) <- Acton.LambdaLifter.liftModule cpsEnv cpstyled
                      iff (C.llift opts) $ dump "llift" (Pretty.print lifted)
                      --traceM ("#################### lifteded env0:")
                      --traceM (Pretty.render (Pretty.pretty liftEnv))
                      timeLLift <- getTime Monotonic
                      iff (C.timing opts) $ putStrLn("    Pass: Lambda Lifting  : " ++ fmtTime (timeLLift - timeCPS))

                      (n,h,c) <- Acton.CodeGen.generate liftEnv relSrcBase lifted
                      timeCodeGen <- getTime Monotonic
                      iff (C.timing opts) $ putStrLn("    Pass: Generating code : " ++ fmtTime (timeCodeGen - timeLLift))

                      iff (C.hgen opts) $ do
                          putStrLn(h)
                          System.Exit.exitSuccess
                      iff (C.cgen opts) $ do
                          putStrLn(c)
                          System.Exit.exitSuccess

                      timeCodeWrite <- getTime Monotonic
                      iff (C.timing opts) $ putStrLn("    Pass: Writing code    : " ++ fmtTime (timeCodeWrite - timeCodeGen))

                      iff ((C.cgen opts) || (C.hgen opts)) System.Exit.exitSuccess

                      let pedantArg = if (C.cpedantic opts) then "-Werror" else ""

                      let hFile = outbase ++ ".h"
                          oFile = joinPath [projLib paths, n ++ ".o"]
                          aFile = joinPath [projLib paths, "libActonProject.a"]

                      iff (not stubMode) (do
                          -- cc is invoked with parent directory of project
                          -- directory as working directory, this is so that the
                          -- paths used in logging will reflect the project name
                          -- and the relative path within
                          let cFile = outbase ++ ".c"
                              hFile = outbase ++ ".h"
                              oFile = joinPath [projLib paths, n ++ ".o"]
                              aFile = joinPath [projLib paths, "libActonProject.a"]
                              buildF = joinPath [projPath paths, "build.sh"]
                              wd = takeFileName (projPath paths)
                              ccCmd = ("cc -Werror=return-type " ++ pedantArg ++
                                       (if (C.dev opts) then " -g " else "") ++
                                       " -c " ++
                                       " -I/opt/homebrew/include" ++
                                       " -I/usr/local/include" ++
                                       " -I/usr/include" ++
                                       " -I" ++ wd ++
                                       " -I" ++ wd ++ "/out" ++
                                       " -I" ++ sysPath paths ++
                                       " -o" ++ oFile ++
                                       " " ++ makeRelative (takeDirectory (projPath paths)) cFile)
                              arCmd = "ar rcs " ++ aFile ++ " " ++ oFile
                          writeFile hFile h
                          writeFile cFile c
                          iff (C.ccmd opts) $ do
                              putStrLn ccCmd
                              putStrLn arCmd
                          writeFile buildF $ unlines ["#!/bin/sh", "cd ..", ccCmd, arCmd]
                          (returnCode, ccStdout, ccStderr) <- readCreateProcessWithExitCode (shell $ ccCmd ++ " && " ++ arCmd){ cwd = Just (takeDirectory (projPath paths)) } ""
                          timeCC <- getTime Monotonic
                          iff (C.timing opts) $ putStrLn("    Pass: C compilation   : " ++ fmtTime (timeCC - timeCodeWrite))
                          case returnCode of
                              ExitSuccess -> return()
                              ExitFailure _ -> do printIce "compilation of generated C code failed"
                                                  putStrLn $ "cc stdout:\n" ++ ccStdout
                                                  putStrLn $ "cc stderr:\n" ++ ccStderr
                                                  System.Exit.exitFailure
                                         )

                      return (env0 `Acton.Env.withModulesFrom` env,iface)

handle errKind f src paths mn ex = do putStrLn ("\nERROR: Error when compiling " ++ (prstr mn) ++ " module: " ++ errKind)
                                      putStrLn (Acton.Parser.makeReport (f ex) src)
                                      removeIfExists (outbase++".ty")
                                      when (rmTmp paths) $ removeDirectoryRecursive (projPath paths)
                                      System.Exit.exitFailure
  where outbase        = outBase paths mn
        removeIfExists f = removeFile f `catch` handleExists
        handleExists :: IOException -> IO ()
        handleExists _ = return ()

buildExecutable env opts paths binTask
                         = case lookup n (fromJust (Acton.Env.lookupMod m env)) of
                               Just (Acton.Env.NAct [] (A.TRow _ _ _ t A.TNil{}) A.TNil{} _) 
                                   | prstr t == "Env" || prstr t == "None"
                                      || prstr t == "__builtin__.Env"|| prstr t == "__builtin__.None"-> do   -- !! To do: proper check of parameter type !!
                                      c <- Acton.CodeGen.genRoot env qn
                                      writeFile rootFile c
                                      iff (C.ccmd opts) $ do
                                          putStrLn ccCmd
                                      appendFile buildF ccCmd
                                      setFileMode buildF 0o755
                                      (returnCode, ccStdout, ccStderr) <- readCreateProcessWithExitCode (shell $ ccCmd) ""
                                      case returnCode of
                                          ExitSuccess -> iff (not (C.quiet opts)) $ putStrLn ("Building executable "++ makeRelative (projPath paths) binFile)
                                          ExitFailure _ -> do printIce "compilation of generated C code of the root actor failed"
                                                              putStrLn $ "cc stdout:\n" ++ ccStdout
                                                              putStrLn $ "cc stderr:\n" ++ ccStderr
                                                              System.Exit.exitFailure
                                      return ()
                                   | otherwise -> handle "Type error" Acton.Types.typeError "" paths m
                                     (Acton.Types.TypeError NoLoc ("Illegal type "++ prstr t ++ " of parameter to root actor " ++ prstr qn))
                               Just t -> handle "Type error" Acton.Types.typeError "" paths m (Acton.Types.TypeError NoLoc (prstr qn ++ " has not actor type."))
                               Nothing -> if not (isDefaultRoot binTask)
                                            then handle "Compilation error" Acton.Env.compilationError "" paths m (Acton.Env.NoItem m n)
                                            else return ()
  where mn                  = A.mname qn
        qn@(A.GName m n)    = (rootActor binTask)
        (sc,_)              = Acton.QuickType.schemaOf env (A.eQVar qn)
        buildF              = joinPath [projPath paths, "build.sh"]
        outbase             = outBase paths mn
        rootFile            = outbase ++ ".root.c"
        libFilesBase        = " -lActonProject -lActon -lActonDB -lprotobuf-c_a -lutf8proc_a -luv_a -lpthread -lm -ldl "
        libPathsBase        = " -L " ++ sysPath paths ++ "/lib -L" ++ sysLib paths ++ " -L" ++ projLib paths
#if defined(darwin_HOST_OS) && defined(aarch64_HOST_ARCH)
        libFiles            = libFilesBase
        libPaths            = libPathsBase ++ " -L/opt/homebrew/lib "
        ccArgs              = ""
#elif defined(darwin_HOST_OS) && defined(x86_64_HOST_ARCH)
        libFiles            = libFilesBase
        libPaths            = libPathsBase ++ " -L/usr/local/lib "
        ccArgs              = ""
-- Linux? and what else? maybe split
#else
        libFiles            = libFilesBase ++ " -luuid -lbsd_a -lmd_a "
        libPaths            = libPathsBase
        ccArgs              = " -no-pie "
#endif
        binFile             = joinPath [binDir paths, (binName binTask)]
        srcbase             = srcFile paths mn
        pedantArg           = if (C.cpedantic opts) then "-Werror" else ""
        ccCmd               = ("cc " ++ ccArgs ++ pedantArg ++
                               (if (C.dev opts) then " -g " else " -O3 ") ++
                               " -I" ++ projOut paths ++
                               " -I" ++ sysPath paths ++
                               " " ++ rootFile ++
                               " -o" ++ binFile ++
                               libPaths ++
                               libFiles ++ "\n")

