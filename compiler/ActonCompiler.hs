module Main where

import qualified Acton.Parser
import qualified Acton.Syntax as A

import qualified Acton.Types
import qualified Acton.Env
import qualified Acton.Relabel
{-
import qualified Acton.CPS
import qualified Acton.Translator
import qualified Backend.LambdaLifter
import qualified Backend.Persistable
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
import System.FilePath.Posix
import qualified System.Exit

data Args       = Args {
                    tokens  :: Bool,
                    parse   :: Bool,
                    imports :: Bool,
                    iface   :: Bool,
                    types   :: Bool,
--                    cps     :: Bool,
--                    python  :: Bool,
--                    llift   :: Bool,
--                    persist :: Bool,
--                    tracef  :: Bool,
--                    norm    :: Bool,
--                    expand  :: Bool,
                    make    :: Bool,
                    verbose :: Bool,
                    syspath :: String,
                    files   :: [String]
                }
                deriving Show

getArgs         = Args
                    <$> switch (long "tokens"  <> help "Show the result of lexing (Yang files only)")
                    <*> switch (long "parse"   <> help "Show the result of parsing")
                    <*> switch (long "imports" <> help "Show the contents of imported modules")
                    <*> switch (long "iface"   <> help "Show the inferred type interface (Acton files only)")
                    <*> switch (long "types"   <> help "Show all inferred expression types (Acton files only)")
--                    <*> switch (long "cps"     <> help "Show the result after CPS conversion (Acton files only)")
--                    <*> switch (long "python"  <> help "Show the initial Python translation (Acton files only)")
--                    <*> switch (long "llift"   <> help "Show the result of lambda-lifting (Acton files only)")
--                    <*> switch (long "persist" <> help "Show persistable datatype replacements (Acton files only)")
--                    <*> switch (long "trace"   <> help "Trace this module's functions and methods at run-time (Acton files only")
--                    <*> switch (long "norm"    <> help "Show the result after syntactic normalization (Yang files only)")
--                    <*> switch (long "expand"  <> help "Show the result after identifier expansion (Yang files only)")
                    <*> switch (long "make"    <> help "(Re-)compile recursively this and all imported modules as needed")
                    <*> switch (long "verbose" <> help "Print progress info during execution")
                    <*> strOption (long "path" <> metavar "TARGETDIR" <> value "" <> showDefault)
                    <*> some (argument str (metavar "FILES"))

descr           = fullDesc <> progDesc "Compile Acton (and Yang) source files to native executables"
                    <> header "actonc - the Acton compiler"

main            = do args <- execParser (info (getArgs <**> helper) descr)
                     mapM_ (\str -> treatOneFile (args {files = [str]})) (files args)

treatOneFile args
                = do paths <- findPaths args
                     let qn = qName (modpath paths)
                     (case ext paths of
                        ".act"   -> (do (src,tree) <- Acton.Parser.parseModule qn (srcFile paths)
                                        iff (parse args) $ dump "parse" (Pretty.print tree)
                                        if make args
                                         then do let task = ActonTask qn src tree
                                                 chaseImportsAndCompile args paths task
                                         else do runRestPasses args paths src [] tree
                                                 return ())
                                          `catch` handle Acton.Parser.parserError "" paths
{-
                        ".yang"  -> if make args
                                     then do let yangFile = srcFile paths
                                                 yangbody = last (modpath paths)
                                             m <- Yang.Parser.parseFile yangFile
                                             let task = YangTask (qName ["yang",yangbody]) m
                                             chaseImportsAndCompile args paths task
                                     else YangCompiler.compileYangFile qn (mkYangArgs args)
-}
                        ".types" -> compTypesFile paths
                        ".ty"    -> showTyFile paths
--                        ".yt"    -> showYtFile paths
                        _        -> error ("********************\nUnknown file extension "++ ext paths))
                               `catch` handle (\exc -> (l0,displayException (exc :: IOException))) "" paths
                               `catch` handle (\exc -> (l0,displayException (exc :: ErrorCall))) "" paths
  where compTypesFile paths = do cont <- readFile (srcFile paths)
                                 let t = read cont :: A.Type
                                 InterfaceFiles.writeFile (joinPath (projSrcRoot paths: modpath paths)++".ty") t

        showTyFile paths    = do t <- InterfaceFiles.readFile (srcFile paths)
                                 putStrLn ("**** Type environment in " ++ (srcFile paths) ++ " ****")
                                 putStrLn (Pretty.render (Pretty.pretty (Acton.Env.openRecord (t :: A.Type))))

--        showYtFile paths    = do t <- InterfaceFiles.readFile (srcFile paths)
--                                 putStrLn (Pretty.render (Pretty.pretty (t :: Y.Stmt)))



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

qName :: [String] ->  A.QName 
qName (str:strs)        = A.QName (mkN str) (map mkN strs)
   where mkN str        = A.Name undefined str

mPath ::   A.QName -> [String]
mPath (A.QName n ns)
                        = A.nstr n : map A.nstr ns

checkDirs :: FilePath -> [String] -> IO ()
checkDirs path []       = return ()
checkDirs path (d:dirs) = do found <- doesDirectoryExist path1
                             if found
                              then checkDirs path1 dirs
                              else do createDirectory path1
                                      writeFile (joinPath [path1,"__init__.py"]) ""
                                      checkDirs path1 dirs
  where path1           = joinPath [path,d]

findPaths :: Args -> IO Paths
findPaths args          = do absfile <- canonicalizePath (head (files args))
                             sysRoot <- canonicalizePath (syspath args)
                             (projRoot,dirsSrc,dirsTarget) <- findDirs absfile
                             checkDirs sysRoot (dirsTarget++dirsSrc)
                             let modpath = dirsSrc ++ [body]
                             return $ Paths projRoot (joinPath (sysRoot : dirsTarget)) modpath ext
  where (body,ext)      = splitExtension $ takeFileName $ head (files args)

        findDirs path   = diff [] (takeDirectory path)
          where diff dirs "/"   = error "********************\nNo .acton file found in any ancestor directory"
                diff dirs pre   = do x <- doesFileExist path
                                     if not x
                                      then diff (takeFileName pre : dirs) (takeDirectory pre)
                                      else do contents <- readFile path
                                              return $ (pre, dirs, concat $ map splitPath $ lines $ contents)
                   where path    = joinPath [pre, ".acton"]

runRestPasses args paths src ifaces tree = (do
                          let outbase = outBase paths
                          (ifaces',ienv) <- Acton.Env.mkEnv (projSysRoot paths,syspath args) ifaces tree
                          
                          (sigs,tyinfo) <- Acton.Types.reconstruct outbase ienv tree
                          iff (types args) $ dump "types" (Pretty.vprint tyinfo)
                          iff (iface args) $ dump "iface" (Pretty.vprint sigs)
{-    
                          cpsed <- Acton.CPS.convert [] tree
                          iff (cps args) $ dump "cps" (Pretty.print cpsed)
                          
                          py1 <- Acton.Translator.translate (modpath paths) tyinfo (tracef args) tree
                          iff (python args) $ dump "python" (Pretty.vprint py1)
        
                          py2 <- Backend.LambdaLifter.liftPy py1
                          iff (llift args) $ dump "llift" (Pretty.vprint py2)
  
                          py3 <- Backend.Persistable.replace py2
                          iff (persist args) $ dump "persist" (Pretty.vprint py3) 
    
                          writeFile (outbase ++ ".py") (Pretty.vprint py3)
                          unless (projSrcRoot paths == projSysRoot paths)
                             $ copyFileWithMetadata (joinPath (projSrcRoot paths: modpath paths) ++ ".act") (outbase ++ ".act")
-}
                          return (ifaces',Acton.Types.env2type sigs))       
                             `catch` handle Acton.Env.checkerError src paths
                             `catch` handle Acton.Types.typeError src paths


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

{-
mkYangArgs :: Args -> YangCompiler.Args 
mkYangArgs args         = YangCompiler.Args {
                            YangCompiler.tokens  = tokens args,
                            YangCompiler.parse   = parse args,
                            YangCompiler.imports = imports args,
                            YangCompiler.norm    = norm args,
                            YangCompiler.expand  = expand args,
                            YangCompiler.syspath = syspath args,
                            YangCompiler.files    = files args}
-}
                      

data CompileTask        = ActonTask  {name :: A.QName, src :: String, atree:: A.Module}
--                        | YangTask   {name :: A.QName, ytree :: Y.Stmt} 
                        | PythonTask {name :: A.QName} deriving (Show)

importsOf :: CompileTask -> [A.QName]
importsOf t@ActonTask{} = A.importsOf (atree t)
--importsOf t@YangTask{}  = map ((\str -> qName ["yang",str]) . Y.istr . Y.ident) is
--   where m              = ytree t
--         is             = Y.importsOf m ++ Y.includesOf m 
importsOf PythonTask{}  = []

chaseImportsAndCompile :: Args -> Paths -> CompileTask -> IO ()
chaseImportsAndCompile args paths task
                       = do tasks <- chaseImportedFiles args paths (importsOf task) [task]
                            let sccs = stronglyConnComp  [(t,name t,importsOf t) | t <- tasks]
                                (as,cs)        = Data.List.partition isAcyclic sccs
                            if null cs
                             then do foldM (doTask args paths) ([],[])
                                           ([PythonTask (qName ["python",body])| body <- pythonFiles] ++ [t | AcyclicSCC t <- as])
                                     return ()
                              else do error ("********************\nCyclic imports:"++concatMap showCycle cs)
                                      System.Exit.exitFailure
  where isAcyclic (AcyclicSCC _) = True
        isAcyclic _    = False
        showCycle (CyclicSCC ts) = "\n"++concatMap (\t-> concat (intersperse "." (mPath (name t)))++" ") ts
        pythonFiles    = ["dwdm", "getenv", "logging", "netconf_","power","re","timestamp","traceback","xmlparse", "xmlprint"]

chaseImportedFiles :: Args -> Paths -> [A.QName] -> [CompileTask] -> IO ([CompileTask])
chaseImportedFiles args paths imps tasks
                            = do newtasks <- mapM (readAFile tasks) imps
                                 let newtasks' = concat newtasks
                                 chaseRecursively (tasks++newtasks') (map name newtasks') (concatMap importsOf newtasks')

  where readAFile tasks qn  = do let ps = mPath qn  -- read and parse file qn in the project directory, unless it is already in tasks 
                                     srcBase = joinPath (projSrcRoot paths:ps)
                                 if head ps == "python"
                                  then return []
                                  else case lookUp qn tasks of
                                         Just t -> return []
                                         Nothing -> {- if head ps == "yang" 
                                                     then do let yangFile = srcBase ++ ".yang"
                                                             ok <- System.Directory.doesFileExist yangFile
                                                             if ok then do m <- Yang.Parser.parseFile yangFile
                                                                           return [YangTask qn m]
                                                              else return []
                                                     else -} 
                                                          do let actFile = srcBase ++ ".act"
                                                             ok <- System.Directory.doesFileExist actFile
                                                             if ok then do (src,m) <- Acton.Parser.parseModule qn actFile
                                                                           return [ActonTask qn src m]
                                                             else return []
  
        lookUp qn (t : ts)
          |name t == qn      = Just t
          |otherwise         = lookUp qn ts
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


doTask args paths ifaces@(actifaces,yangifaces) t@(ActonTask qn src m)
                             = do ok <- checkUptoDate paths ".ty" actFile tyFile [pyFile] (syspath args) (importsOf t)
                                  if ok then do iff (verbose args) (putStrLn ("Skipping  "++ actFile ++ " (files are up to date)."))
                                                return ifaces
                                   else do checkDirs (projSysRoot paths) (init (mPath qn))
                                           iff (verbose args) (putStr ("Compiling "++ actFile ++ "... ") >> hFlush stdout)
                                           (mods,t) <- runRestPasses args (paths{modpath = mPath qn, ext = ".act"}) src actifaces m
                                           iff (verbose args) (putStrLn "Done.")
                                           return ((qn,t):mods,yangifaces)
  where actFile             = joinPath (projSrcRoot paths : mPath qn)++ ".act"
        outBase             = joinPath (projSysRoot paths : mPath qn)
        tyFile              = outBase ++ ".ty"
        pyFile              = outBase ++ ".py"
{-        
doTask args paths ifaces@(actifaces,yangifaces) t@(YangTask qn m) = do
         let outFiles = case m of
                          Y.Module{} -> [actFile,pyFile,tyFile]
                          Y.Submodule{} -> []
         ok <- checkUptoDate paths ".yt" yangFile ytFile outFiles (syspath args) (importsOf t)
         if ok then do iff (verbose args) (putStrLn ("Skipping  "++ yangFile ++ " (files are up to date)."))
                       return ifaces
          else do checkDirs (projSysRoot paths) (init (mPath qn))
                  iff (verbose args) (putStr ("Compiling "++ yangFile ++ "... ") >> hFlush stdout)
                  (etree,yangifaces',mbt) <- YangCompiler.runRestPasses qn (mkYangArgs args) (yangFile,joinPath (projSysRoot paths:init (mPath qn)),outbase) yangifaces m
                  iff (verbose args) (putStrLn ("Done."))
                  case mbt of
                     Nothing -> return (actifaces,((id,Y.revisionOf etree),etree):((id,Nothing),etree):yangifaces')
                     Just m -> do 
                                iff (verbose args) (putStr ("Compiling "++  actFile ++ "... ") >> hFlush stdout)
                                let m1 = Acton.Relabel.relab m
                                (amods,t) <- runRestPasses args (Paths (projSysRoot paths) (projSysRoot paths) (mPath qn) ".act") "" actifaces m1
                                iff (verbose args) (putStrLn ("Done."))
                                return ((qn,t):amods,((id,Y.revisionOf etree),etree):((id,Nothing),etree):yangifaces')
   where yangFile = joinPath (projSrcRoot paths: mPath qn) ++ ".yang"
         outbase  = joinPath (projSysRoot paths: mPath qn)
         ytFile   = outbase ++ ".yt"
         tyFile   = outbase ++ ".ty"
         pyFile   = outbase ++ ".py"
         actFile  = outbase ++ ".act"
         [_,i]    = mPath qn
         id       = Y.Ident SpanEmpty i
-}
doTask args paths ifaces@(actifaces,yangifaces) (PythonTask qn)
                        = do ok <- checkUptoDate paths ".ty" typesFile tyFile [] (joinPath (syspath args : ["python"])) []
                             if ok then return ifaces --putStrLn ("Skipping  "++ typesFile ++ " (files are up to date).")
                              else do putStr ("Compiling "++  typesFile ++ "... ")
                                      cont <- readFile typesFile
                                      let t = read cont :: A.Type
                                      InterfaceFiles.writeFile tyFile t
                                      putStrLn ("Done.")
                                      return ((qn,t):actifaces,yangifaces)
   where pythonBase     = joinPath (syspath args : mPath qn)
         typesFile      =  pythonBase ++ ".types"
         tyFile         =  pythonBase ++ ".ty"
         
checkUptoDate :: Paths -> String -> FilePath -> FilePath -> [FilePath] -> FilePath -> [A.QName] -> IO Bool
checkUptoDate paths ext srcFile iFile outFiles libRoot imps
                        = do srcExists <- System.Directory.doesFileExist srcFile
                             outExists <- mapM System.Directory.doesFileExist (iFile:outFiles)
                             if not (srcExists && and outExists) then return False
                              else do srcTime  <-  System.Directory.getModificationTime srcFile
                                      outTimes <- mapM System.Directory.getModificationTime (iFile:outFiles)
                                      impsOK   <- mapM (impOK (head outTimes)) imps
                                      return (all (srcTime <) outTimes && and impsOK)
  where impOK iTime qn = do let impFile = joinPath (projSysRoot paths : mPath qn) ++ ext
                            ok <- System.Directory.doesFileExist impFile
                            if ok then do impfileTime <- System.Directory.getModificationTime impFile
                                          return (impfileTime < iTime)
                             else do let impSysFile = joinPath (libRoot : mPath qn) ++ ext
                                     ok <-  System.Directory.doesFileExist impSysFile
                                     if ok then do impfileTime <- System.Directory.getModificationTime impSysFile
                                                   return (impfileTime < iTime)
                                       else error ("********************\nError: cannot find interface file "++impFile)



-- Data.Graph requires that the type of keys for nodes is an instance of Ord 
instance Ord A.QName where
    compare n1 n2 = compare (mPath n1) (mPath n2)
    
