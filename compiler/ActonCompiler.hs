module Main where

import qualified Acton.Parser
import qualified Acton.Syntax as A

import qualified Acton.Relabel
import qualified Acton.Env
import qualified Acton.Kinds
import qualified Acton.Types
import qualified Acton.Solver
import qualified Acton.Normalizer
import qualified Acton.CPS
import qualified Acton.Deactorizer
import qualified Acton.LambdaLifter
import qualified Acton.CodeGen
{-
import qualified Yang.Syntax as Y
import qualified Yang.Parser
import qualified YangCompiler
-}
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
                    nobuiltin :: Bool,
                    syspath :: String,
                    files   :: [String]
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
                    <*> switch (long "hgen"    <> help "Show the generated .h file")
                    <*> switch (long "cgen"    <> help "Show the generated .c file")
                    <*> switch (long "verbose" <> help "Print progress info during execution")
                    <*> switch (long "nobuiltin" <> help "No builtin module (only for compiling __builtin__.act)")
                    <*> strOption (long "path" <> metavar "TARGETDIR" <> value "" <> showDefault)
                    <*> some (argument str (metavar "FILES"))

descr           = fullDesc <> progDesc "Compile Acton (and Yang) source files to native executables"
                    <> header "actonc - the Acton compiler"

main            = do args <- execParser (info (getArgs <**> helper) descr)
                     mapM_ (\str -> treatOneFile (args {files = [str]})) (files args)

treatOneFile args
                = do paths <- findPaths args
                     let mn = A.modName (modpath paths)
                     (case ext paths of
                        ".act"   -> (do (src,tree) <- Acton.Parser.parseModule mn (srcFile paths)
                                        iff (parse args) $ dump "parse" (Pretty.print tree)
                                        let task = ActonTask mn src tree
                                        chaseImportsAndCompile args paths task)
                                          `catch` handle Acton.Parser.parserError "" paths
                        ".types" -> compTypesFile paths
                        ".ty"    -> showTyFile paths
                        _        -> error ("********************\nUnknown file extension "++ ext paths))
                               `catch` handle (\exc -> (l0,displayException (exc :: IOException))) "" paths
                               `catch` handle (\exc -> (l0,displayException (exc :: ErrorCall))) "" paths
  where compTypesFile paths = do cont <- readFile (srcFile paths)
                                 let te = read cont :: Acton.Env.TEnv
                                 InterfaceFiles.writeFile (joinPath (projSrcRoot paths: modpath paths)++".ty") te

        showTyFile paths    = do te <- InterfaceFiles.readFile (srcFile paths)
                                 putStrLn ("**** Type environment in " ++ (srcFile paths) ++ " ****")
                                 putStrLn (Pretty.render (Pretty.pretty (te :: Acton.Env.TEnv)))


iff True m      = m >> return ()
iff False _     = return ()

dump h txt      = putStrLn ("\n\n#################################### " ++ h ++ ":\n" ++ txt)

data Paths      = Paths {projSrcRoot :: FilePath,
                         projSysRoot :: FilePath,
                         modpath     :: [String],
                         ext         :: String
                        }
                   
-- projSrcRoot is root of project in source tree (i.e. directory containing .acton file) 
-- projSysRoot is root of project in target tree (i.e. joinPath of syspath args  and content of .acton file)
-- modpath is path from root of project source tree to directory of source file
-- ext is file suffix.
-- Thus, source filename is joinPath (projSrcRoot:modpath) ++ ext and outfiles will be in joinPath (projSysRoot:modpath)
srcFile paths           = joinPath (projSrcRoot paths:modpath paths) ++ ext paths
outBase paths           = joinPath (projSysRoot paths:modpath paths)

checkDirs               :: FilePath -> [String] -> IO ()
checkDirs path []       = return ()
checkDirs path (d:dirs) = do found <- doesDirectoryExist path1
                             if found
                              then checkDirs path1 dirs
                              else do createDirectory path1
                                      checkDirs path1 dirs
  where path1           = joinPath [path,d]

findPaths               :: Args -> IO Paths
findPaths args          = do absfile <- canonicalizePath (head (files args))
                             (projRoot,dirsSrc,dirsTarget) <- findDirs absfile
                             sysRoot <- canonicalizePath (ifExists (syspath args) projRoot)
                             checkDirs sysRoot (dirsTarget++dirsSrc)
                             let modpath = dirsSrc ++ [body]
                             return $ Paths projRoot (joinPath (sysRoot : dirsTarget)) modpath ext
  where (body,ext)      = splitExtension $ takeFileName $ head (files args)

        ifExists "" p   = p
        ifExists p _    = p

        findDirs path   = diff [] (takeDirectory path)
          where diff dirs "/"   = error "********************\nNo .acton file found in any ancestor directory"
                diff dirs pre   = do x <- doesFileExist path
                                     if not x
                                      then diff (takeFileName pre : dirs) (takeDirectory pre)
                                      else do contents <- readFile path
                                              return $ (pre, dirs, concat $ map splitPath $ lines $ contents)
                   where path    = joinPath [pre, ".acton"]

runRestPasses args paths src env0 original = do
                      let outbase = outBase paths
                      env <- Acton.Env.mkEnv (projSysRoot paths,syspath args) env0 original

                      (do kchecked <- Acton.Kinds.check env original
                          iff (kinds args) $ dump "kinds" (Pretty.print kchecked)

                          (iface,tchecked,typeEnv) <- Acton.Types.reconstruct outbase env kchecked
                          iff (types args) $ dump "types" (Pretty.print tchecked)
                          iff (sigs args) $ dump "sigs" (Pretty.vprint iface)

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

                          (h,c) <- Acton.CodeGen.generate liftEnv lifted
                          writeFile (outbase ++ ".h") h
                          writeFile (outbase ++ ".c") c
                          createProcess (proc "gcc" ["-c", "-I"++syspath args, outbase ++ ".c", "-o"++outbase++".o"])
                          iff (hgen args) $ dump "hgen (.h)" h
                          iff (cgen args) $ dump "cgen (.c)" c

                          return (env0 `Acton.Env.withModulesFrom` env,iface)
                        ) 
                          `catch` handle generalError src paths
                          `catch` handle (Acton.Env.compilationError env) src paths


handle f src paths ex = do putStrLn "\n********************"
                           putStrLn (makeReport (f ex) (srcFile paths) src)
                           removeIfExists (outbase++".py")
                           removeIfExists (outbase++".ty")
                           System.Exit.exitFailure
  where outbase       = outBase paths
        removeIfExists f = removeFile f `catch` handleExists
        handleExists :: IOException -> IO ()
        handleExists _ = return ()

makeReport (loc, msg) file src = errReport (sp, msg) src
  where sp = Acton.Parser.extractSrcSpan loc file src


data CompileTask        = ActonTask  {name :: A.ModName, src :: String, atree:: A.Module} deriving (Show)

importsOf :: CompileTask -> [A.ModName]
importsOf t = A.importsOf (atree t)

chaseImportsAndCompile :: Args -> Paths -> CompileTask -> IO ()
chaseImportsAndCompile args paths task
                       = do tasks <- chaseImportedFiles args paths (importsOf task) [task]
                            let sccs = stronglyConnComp  [(t,name t,importsOf t) | t <- tasks]
                                (as,cs) = Data.List.partition isAcyclic sccs
                            if null cs
                             then do env0 <- Acton.Env.initEnv False
                                     foldM (doTask args paths) (env0,[]) [t | AcyclicSCC t <- as]
                                     return ()
                              else do error ("********************\nCyclic imports:"++concatMap showCycle cs)
                                      System.Exit.exitFailure
  where isAcyclic (AcyclicSCC _) = True
        isAcyclic _    = False
        showCycle (CyclicSCC ts) = "\n"++concatMap (\t-> concat (intersperse "." (A.modPath (name t)))++" ") ts

chaseImportedFiles :: Args -> Paths -> [A.ModName] -> [CompileTask] -> IO ([CompileTask])
chaseImportedFiles args paths imps tasks
                            = do newtasks <- mapM (readAFile tasks) imps
                                 let newtasks' = concat newtasks
                                 chaseRecursively (tasks++newtasks') (map name newtasks') (concatMap importsOf newtasks')

  where readAFile tasks mn  = do let ps = A.modPath mn  -- read and parse file qn in the project directory, unless it is already in tasks 
                                     srcBase = joinPath (projSrcRoot paths:ps)
                                 if head ps == "python"
                                  then return []
                                  else case lookUp mn tasks of
                                         Just t -> return []
                                         Nothing -> do let actFile = srcBase ++ ".act"
                                                       ok <- System.Directory.doesFileExist actFile
                                                       if ok then do (src,m) <- Acton.Parser.parseModule mn actFile
                                                                     return [ActonTask mn src m]
                                                        else return []
  
        lookUp mn (t : ts)
          | name t == mn     = Just t
          | otherwise        = lookUp mn ts
        lookUp _ []          = Nothing
        
        chaseRecursively tasks qns []
                             = return tasks
        chaseRecursively tasks qns (iqn : iqns)
                             = if iqn `elem` qns
                                then chaseRecursively tasks qns iqns
                                else do t <- readAFile tasks iqn
                                        chaseRecursively (if null t then tasks else head t : tasks)
                                                         (iqn:qns)
                                                         (iqns ++ concatMap importsOf t) 


doTask args paths ifaces@(env, yangifaces) t@(ActonTask qn src m)
                             = do ok <- checkUptoDate paths ".ty" actFile tyFile [hFile, cFile] (syspath args) (importsOf t)
                                  if ok then do iff (verbose args) (putStrLn ("Skipping  "++ actFile ++ " (files are up to date)."))
                                                return ifaces
                                   else do checkDirs (projSysRoot paths) (init (A.modPath qn))
                                           iff (verbose args) (putStr ("Compiling "++ actFile ++ "... ") >> hFlush stdout)
                                           (env',te) <- runRestPasses args (paths{modpath = A.modPath qn, ext = ".act"}) src env m
                                           iff (verbose args) (putStrLn "Done.")
                                           return (Acton.Env.addMod qn te env', yangifaces)
  where actFile             = joinPath (projSrcRoot paths : A.modPath qn)++ ".act"
        outBase             = joinPath (projSysRoot paths : A.modPath qn)
        tyFile              = outBase ++ ".ty"
        hFile               = outBase ++ ".h"
        cFile               = outBase ++ ".c"

checkUptoDate :: Paths -> String -> FilePath -> FilePath -> [FilePath] -> FilePath -> [A.ModName] -> IO Bool
checkUptoDate paths ext srcFile iFile outFiles libRoot imps
                        = do srcExists <- System.Directory.doesFileExist srcFile
                             outExists <- mapM System.Directory.doesFileExist (iFile:outFiles)
                             if not (srcExists && and outExists) then return False
                              else do srcTime  <-  System.Directory.getModificationTime srcFile
                                      outTimes <- mapM System.Directory.getModificationTime (iFile:outFiles)
                                      impsOK   <- mapM (impOK (head outTimes)) imps
                                      return (all (srcTime <) outTimes && and impsOK)
  where impOK iTime mn = do let impFile = joinPath (projSysRoot paths : A.modPath mn) ++ ext
                            ok <- System.Directory.doesFileExist impFile
                            if ok then do impfileTime <- System.Directory.getModificationTime impFile
                                          return (impfileTime < iTime)
                             else do let impSysFile = joinPath (libRoot : A.modPath mn) ++ ext
                                     ok <-  System.Directory.doesFileExist impSysFile
                                     if ok then do impfileTime <- System.Directory.getModificationTime impSysFile
                                                   return (impfileTime < iTime)
                                       else error ("********************\nError: cannot find interface file "++impFile)
