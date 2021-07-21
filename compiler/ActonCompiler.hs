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

import qualified Acton.Parser
import qualified Acton.Syntax as A

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

import Control.Exception (throw,catch,displayException,IOException,ErrorCall)
import Control.Monad
import Options.Applicative
import Data.Monoid ((<>))
import Data.Graph
import qualified Data.List
import System.IO
import System.Directory
import System.Process
import System.FilePath.Posix
import qualified System.Environment
import qualified System.Exit

data Args       = Args {
                    parse   :: Bool,
                    kinds   :: Bool,
                    types   :: Bool,
                    sigs    :: Bool,
                    norm    :: Bool,
                    deact   :: Bool,
                    cps     :: Bool,
                    llift   :: Bool,
                    hgen    :: Bool,
                    cgen    :: Bool,
                    verbose :: Bool,
                    stub    :: Bool,
                    syspath :: String,
                    root    :: String,
                    file    :: String
                }
                deriving Show

getArgs         = Args
                    <$> switch (long "parse"   <> help "Show the result of parsing")
                    <*> switch (long "kinds"   <> help "Show all the result after kind-checking")
                    <*> switch (long "types"   <> help "Show all inferred expression types")
                    <*> switch (long "sigs"    <> help "Show the inferred type signatures")
                    <*> switch (long "norm"    <> help "Show the result after syntactic normalization")
                    <*> switch (long "deact"   <> help "Show the result after deactorization")
                    <*> switch (long "cps"     <> help "Show the result after CPS conversion")
                    <*> switch (long "llift"   <> help "Show the result of lambda-lifting")
                    <*> switch (long "hgen"    <> help "Show the generated .h header")
                    <*> switch (long "cgen"    <> help "Show the generated .c code")
                    <*> switch (long "verbose" <> help "Print progress info during execution")
                    <*> switch (long "stub"    <> help "Stub (.ty) file generation only")
                    <*> strOption (long "path" <> metavar "TARGETDIR" <> value "" <> showDefault)
                    <*> strOption (long "root" <> value "" <> showDefault)
                    <*> argument str (metavar "FILE")

descr           = fullDesc <> progDesc "Compile an Acton source file with recompilation of imported modules as needed"
                    <> header "actonc - the Acton compiler"

main            = do args <- execParser (info (getArgs <**> helper) descr)
                     paths <- findPaths args
                     when (verbose args) $ do
                         putStrLn ("## sysPath: " ++ sysPath paths)
                         putStrLn ("## sysRoot: " ++ sysRoot paths)
                         putStrLn ("## srcRoot: " ++ srcRoot paths)
                         putStrLn ("## modPrefix: " ++ prstrs (modPrefix paths))
                         putStrLn ("## topMod: " ++ prstr (topMod paths))
                     let mn = topMod paths
                     (case ext paths of
                        ".act"   -> do let fName = file args
                                       src <- readFile fName
                                       tree <- Acton.Parser.parseModule mn fName src 
                                                        `catch` handle "Syntax error" Acton.Parser.parserError "" paths mn
                                                        `catch` handle "Context error" Acton.Parser.contextError src paths mn
                                                        `catch` handle "Indentation error" Acton.Parser.indentationError src paths mn
                                       iff (parse args) $ dump "parse" (Pretty.print tree)
                                       let task = ActonTask mn src tree
                                       chaseImportsAndCompile args paths task
                        ".ty"    -> do env0 <- Acton.Env.initEnv (sysRoot paths) False False
                                       Acton.Types.showTyFile (Acton.Env.setMod (topMod paths) env0) (file args)
                        _        -> error ("********************\nUnknown file extension "++ ext paths))
                               `catch` handle "IOException" (\exc -> (l0,displayException (exc :: IOException))) "" paths mn
                               `catch` handle "Error" (\exc -> (l0,displayException (exc :: ErrorCall))) "" paths mn


dump h txt      = putStrLn ("\n\n#################################### " ++ h ++ ":\n" ++ txt)

data Paths      = Paths {
                    sysPath     :: FilePath,
                    sysRoot     :: FilePath,
                    srcRoot     :: FilePath,
                    modPrefix   :: [String],
                    ext         :: String,
                    topMod      :: A.ModName
                  }

-- Given a FILE and optionally --path PATH:
-- 'sysPath' is the path to the system directory as given by PATH, defaulting to the actonc executable directory.
-- 'sysRoot' is the root of the system's private module tree (directory "modules" under 'sysPath').
-- 'srcRoot' is the root of a user's source tree (the longest directory prefix of FILE containing an ".acton" file)
-- 'modPrefix' is the module prefix of the source tree under 'srcRoot' (the directory name at 'srcRoot' split at every '.')
-- 'ext' is file suffix of FILE.
-- 'topMod' is the module name of FILE (its path after 'srcRoot' except 'ext', split at every '/')

srcFile                 :: Paths -> A.ModName -> Maybe FilePath
srcFile paths mn        = case stripPrefix (modPrefix paths) (A.modPath mn) of
                            Just ns -> Just $ joinPath (srcRoot paths : ns) ++ ".act"
                            Nothing -> Nothing

sysFile                 :: Paths -> A.ModName -> FilePath
sysFile paths mn        = joinPath (sysRoot paths : A.modPath mn)


touchDirs               :: FilePath -> A.ModName -> IO ()
touchDirs path mn       = touch path (init $ A.modPath mn)
  where 
    touch path []       = return ()
    touch path (d:dirs) = do found <- doesDirectoryExist path1
                             if found then touch path1 dirs
                              else do createDirectory path1
                                      touch path1 dirs
      where path1       = joinPath [path,d]

findPaths               :: Args -> IO Paths
findPaths args          = do execDir <- takeDirectory <$> System.Environment.getExecutablePath
                             sysPath <- canonicalizePath (if null $ syspath args then execDir else syspath args)
                             let sysRoot = joinPath [sysPath,"modules"]
                             absfile <- canonicalizePath (file args)
                             (srcRoot,subdirs) <- analyze (takeDirectory absfile) []
                             let modPrefix = if srcRoot == sysRoot then [] else split (takeFileName srcRoot)
                                 topMod = A.modName $ modPrefix++subdirs++[body]
                             touchDirs sysRoot topMod
                             return $ Paths sysPath sysRoot srcRoot modPrefix ext topMod
  where (body,ext)      = splitExtension $ takeFileName $ file args

        split           = foldr f [[]] where f c l@(x:xs) = if c == '.' then []:l else (c:x):xs

        analyze "/" ds  = error "********************\nNo .acton file found in any ancestor directory"
        analyze pre ds  = do exists <- doesFileExist (joinPath [pre, ".acton"])
                             if exists then return $ (pre, ds)
                              else analyze (takeDirectory pre) (takeFileName pre : ds)

data CompileTask        = ActonTask  {name :: A.ModName, src :: String, atree:: A.Module} deriving (Show)

importsOf :: CompileTask -> [A.ModName]
importsOf t = A.importsOf (atree t)

chaseImportsAndCompile :: Args -> Paths -> CompileTask -> IO ()
chaseImportsAndCompile args paths task
                       = do tasks <- chaseImportedFiles args paths (importsOf task) task
                            let sccs = stronglyConnComp  [(t,name t,importsOf t) | t <- tasks]
                                (as,cs) = Data.List.partition isAcyclic sccs
                            if null cs
                             then do env0 <- Acton.Env.initEnv (sysRoot paths) (stub args) (topMod paths == Acton.Builtin.mBuiltin)
                                     env1 <- foldM (doTask args paths) env0 [t | AcyclicSCC t <- as]
                                     buildExecutable env1 args paths task
                                         `catch` handle "Compilation error" Acton.Env.compilationError (src task) paths (name task)
                                         `catch` handle "Type error" Acton.Types.typeError (src task) paths (name task)
                                     return ()
                              else do error ("********************\nCyclic imports:"++concatMap showCycle cs)
                                      System.Exit.exitFailure
  where isAcyclic (AcyclicSCC _) = True
        isAcyclic _              = False
        showCycle (CyclicSCC ts) = "\n"++concatMap (\t-> concat (intersperse "." (A.modPath (name t)))++" ") ts

chaseImportedFiles :: Args -> Paths -> [A.ModName] -> CompileTask -> IO [CompileTask]
chaseImportedFiles args paths imps task
                            = do newtasks <- catMaybes <$> mapM (readAFile [task]) imps
                                 chaseRecursively (task:newtasks) (map name newtasks) (concatMap importsOf newtasks)

  where readAFile tasks mn  = case lookUp mn tasks of    -- read and parse file mn in the project directory, unless it is already in tasks 
                                 Just t -> return Nothing
                                 Nothing -> case srcFile paths mn of
                                               Nothing -> return Nothing
                                               Just actFile -> do
                                                    ok <- System.Directory.doesFileExist actFile
                                                    if ok then do
                                                        src <- readFile actFile
                                                        m <- Acton.Parser.parseModule mn actFile src
                                                        return $ Just $ ActonTask mn src m
                                                     else
                                                        return Nothing
  
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


doTask :: Args -> Paths -> Acton.Env.Env0 -> CompileTask -> IO Acton.Env.Env0
doTask args paths env t@(ActonTask mn src m)
                            = do ok <- checkUptoDate paths actFile tyFile [hFile, cFile] (importsOf t)
                                 if ok && mn /= topMod paths then do
                                          iff (verbose args) (putStrLn ("Skipping  "++ actFile ++ " (files are up to date)."))
                                          return env
                                  else do touchDirs (sysRoot paths) mn
                                          iff (verbose args) (putStr ("Compiling "++ actFile ++ "... ") >> hFlush stdout)
                                          (env',te) <- runRestPasses args paths env m
                                                           `catch` handle "Compilation error" generalError src paths mn
                                                           `catch` handle "Compilation error" Acton.Env.compilationError src paths mn
                                                           `catch` handle "Type error" Acton.Types.typeError src paths mn
                                          iff (verbose args) (putStrLn "Done.")
                                          return (Acton.Env.addMod mn te env')
  where Just actFile        = srcFile paths mn
        outbase             = sysFile paths mn
        tyFile              = outbase ++ ".ty"
        hFile               = outbase ++ ".h"
        cFile               = outbase ++ ".c"

checkUptoDate :: Paths -> FilePath -> FilePath -> [FilePath] -> [A.ModName] -> IO Bool
checkUptoDate paths actFile iFile outFiles imps
                        = do srcExists <- System.Directory.doesFileExist actFile
                             outExists <- mapM System.Directory.doesFileExist (iFile:outFiles)
                             if not (srcExists && and outExists) then return False
                              else do srcTime  <-  System.Directory.getModificationTime actFile
                                      outTimes <- mapM System.Directory.getModificationTime (iFile:outFiles)
                                      impsOK   <- mapM (impOK (head outTimes)) imps
                                      return (all (srcTime <) outTimes && and impsOK)
  where impOK iTime mn = do let impFile = sysFile paths mn ++ ".ty"
                            ok <- System.Directory.doesFileExist impFile
                            if ok then do impfileTime <- System.Directory.getModificationTime impFile
                                          return (impfileTime < iTime)
                             else error ("********************\nError: cannot find interface file "++impFile)


runRestPasses :: Args -> Paths -> Acton.Env.Env0 -> A.Module -> IO (Acton.Env.Env0, Acton.Env.TEnv)
runRestPasses args paths env0 parsed = do
                      let outbase = sysFile paths (A.modname parsed)
                      env <- Acton.Env.mkEnv (sysRoot paths) env0 parsed

                      kchecked <- Acton.Kinds.check env parsed
                      iff (kinds args) $ dump "kinds" (Pretty.print kchecked)

                      (iface,tchecked,typeEnv) <- Acton.Types.reconstruct outbase env kchecked
                      iff (types args) $ dump "types" (Pretty.print tchecked)
                      iff (sigs args) $ dump "sigs" (Pretty.vprint (A.imps tchecked) ++ "\n\n" ++ Pretty.vprint iface ++ "\n")

                      (normalized, normEnv) <- Acton.Normalizer.normalize typeEnv tchecked
                      iff (norm args) $ dump "norm" (Pretty.print normalized)
                      --traceM ("#################### normalized env0:")
                      --traceM (Pretty.render (Pretty.pretty normEnv))

                      (deacted,deactEnv) <- Acton.Deactorizer.deactorize normEnv normalized
                      iff (deact args) $ dump "deact" (Pretty.print deacted)
                      --traceM ("#################### deacted env0:")
                      --traceM (Pretty.render (Pretty.pretty deactEnv))

                      (cpstyled,cpsEnv) <- Acton.CPS.convert deactEnv deacted
                      iff (cps args) $ dump "cps" (Pretty.print cpstyled)
                      --traceM ("#################### cps'ed env0:")
                      --traceM (Pretty.render (Pretty.pretty cpsEnv))

                      (lifted,liftEnv) <- Acton.LambdaLifter.liftModule cpsEnv cpstyled
                      iff (llift args) $ dump "llift" (Pretty.print lifted)
                      --traceM ("#################### lifteded env0:")
                      --traceM (Pretty.render (Pretty.pretty liftEnv))

                      (n,h,c) <- Acton.CodeGen.generate liftEnv lifted
                      iff (hgen args) $ dump "hgen (.h)" h
                      iff (cgen args) $ dump "cgen (.c)" c

                      iff (not $ stub args) $ do
                          let libDir = joinPath [sysPath paths,"lib"]
                              cFile = outbase ++ ".c"
                              hFile = outbase ++ ".h"
                              oFile = joinPath [libDir,n++".o"]
                              aFile = joinPath [libDir,"libActon.a"]
                              gccCmd = "gcc -g -c -I/usr/include/kqueue -I" ++ sysPath paths ++ " -o" ++ oFile ++ " " ++ cFile
                              arCmd = "ar r " ++ aFile ++ " " ++ oFile
                          writeFile hFile h
                          writeFile cFile c
                          iff (cgen args) $ do
                              putStrLn gccCmd
                              putStrLn arCmd
                          createProcess (shell $ gccCmd ++ " && " ++ arCmd) >>= \(_,_,_,hdl) -> waitForProcess hdl

                      return (env0 `Acton.Env.withModulesFrom` env,iface)


handle errKind f src paths mn ex = do putStrLn ("\n******************** " ++ errKind)
                                      putStrLn (Acton.Parser.makeReport (f ex) src)
                                      removeIfExists (outbase++".ty")
                                      System.Exit.exitFailure
  where outbase        = sysFile paths mn
        removeIfExists f = removeFile f `catch` handleExists
        handleExists :: IOException -> IO ()
        handleExists _ = return ()


buildExecutable env args paths task
  | null $ root args        = return ()
  | otherwise               = case Acton.Env.findQName qn env of
                                  i@(Acton.Env.NAct [] (A.TRow _ _ _ t A.TNil{}) A.TNil{} _) -> do
                                      -- putStrLn ("## Env is " ++ prstr t)
                                      c <- Acton.CodeGen.genRoot env qn t
                                      writeFile rootFile c
                                      iff (cgen args) $ do
                                          putStrLn gccCmd
                                      createProcess (shell gccCmd) >>= \(_,_,_,hdl) -> waitForProcess hdl
                                      return ()
                                  _ ->
                                      error ("********************\nRoot " ++ prstr n ++ " : " ++ prstr sc ++ " is not instantiable")
  where n                   = A.name (root args)
        mn                  = name task
        qn                  = A.GName mn n
        (sc,_)              = Acton.QuickType.schemaOf env (A.eQVar qn)
        outbase             = sysFile paths mn
        rootFile            = outbase ++ ".root.c"
        libFilesBase        = " -L" ++ joinPath [sysPath paths,"lib"] ++ " -ldbclient -lremote -lcomm -ldb -lvc -lprotobuf-c -lActon -lm -lpthread -lutf8proc"
#if defined(linux_HOST_OS)
        libFiles            = libFilesBase ++  " -lkqueue"
#elif defined(darwin_HOST_OS)
        libFiles            = libFilesBase
-- Do we support anything else? Do a default clause for now...
#else
        libFiles            = libFilesBase
#endif
        binFile             = dropExtension srcbase
        Just srcbase        = srcFile paths mn
        gccCmd              = "gcc -g -I/usr/include/kqueue -I" ++ sysPath paths ++ " " ++ rootFile ++ " -o" ++ binFile ++ libFiles
