{-# LANGUAGE OverloadedStrings #-}
{-|
Derived modules.  A derived module is a module of a project that has no
file under src.  An entry of the derived_modules table of Build.act names
the modules, a function, input paths and arguments, and the function
returns the source of the modules from a description of each input file
and the arguments.  A description holds the path, the size and the SHA-256
of a file, and the function reads the content of a file through it.
The source is written under out/derived/src, from where a build discovers
and compiles it like any other module (Acton.Project).

updateDerivedModules brings the derived modules of every project of a build
up to date, dependencies first, before the build discovers its modules.
For a project it

  1. removes the record and the runner of each entry that is no longer in
     the table, and each file under out/derived/src that no entry names,
     except a --tydb copy, which a build writes beside the source file of a
     module (isInterfaceCopy),
  2. lists the input files of each entry and takes the SHA-256 of each,
  3. compiles the modules of the functions through the front passes, in
     the build's own context, and reads each function's type and
     implementation hash from the interface file of its module,
  4. compares each entry with the record of its last run,
  5. builds a runner for each entry that is out of date: a program whose
     root actor has std.build.derive call the function with the
     descriptions of the input files and the entry's arguments.  A runner
     that is built is used again while its source, the implementation hash
     of the function and the compiler version stay the same,
  6. writes the manifest of the run, which holds the path, the size and
     the SHA-256 of each input file, and runs the runner, which calls the
     function, reads an input file when the function asks for its content,
     and writes the returned modules to a temporary directory,
  7. checks that the returned modules are exactly the modules that the
     entry names, moves them into out/derived/src and writes the record.
     A file whose source did not change is left alone.

The driver is told each source file of a derived module that is written or
removed, when that is done.  A build under --watch that fails or is
canceled later then still knows of the file, and the build that follows it
compiles the module.

The caller holds the lock of the root project of the build.  Another build
can reach any other project at the same time, as its own root project or as
a dependency.  So the dependency projects are locked through the context of
the build (ccLockDependencies) before the derived modules of any project
are brought up to date, and the build holds these locks to its end.  The
build of a runner runs inside the build and takes no lock.

An entry is up to date when its record has the same compiler version,
function, implementation hash, result type, module names, arguments, input
paths and input hashes, and the files that the record lists are in place
with the recorded content.  A name's implementation hash covers its own
implementation and that of every name it uses, in every module
(Acton.Hashing).  So a change to code that the function neither calls nor
reads does not run it again, and a change of a constant that it reads
does.

The record also holds the stat of each input file and of each file that
the entry wrote, taken before the read that gave the file's SHA-256.  A
file that the record stands for by the stat rule (Compile.statRule) is not
read.  Any other file is read for its SHA-256.  When the entry is up to
date by these, the record is written again with the stats taken before the
reads, so the next build reads no file.  A write that lands during a read
leaves the file with a stat other than the recorded one, so the next build
reads the file again.  This is the rule that tells whether the source of a
module or an embedded file changed (Acton.Compile).  A build in which
nothing changed then costs one stat per file, whatever the files hold.

An input file is looked at through the file facts of the build
(Compile.fileFact), as an embedded file is.  So the build reads a file at
most once, also one that is an input file of several entries or that a
module embeds, and a later look that finds another stat stops the build:
the file changed while the build ran.  The function of an entry reads an
input file after the build took the SHA-256 of that file, so the build
looks at each input file of the entry again after the run, before it moves
a returned module into place (install).  The read of a description itself
fails when the content does not have the size and the SHA-256 of the
description (std.build.derive).

The runners of a project are the root modules of a project of their own
under out/derived/runner.  That project depends on the project and on
every project it reaches, each at the directory the build selected, so a
runner is built from the same sources as the build.  The caller builds a
runner, since only the CLI builds executables.

Files, for the entry schema of a project:

  out/derived/src/schema.act                  the source of the module
  out/derived/schema.json                     the record
  out/derived/runner/Build.act                the runner project
  out/derived/runner/src/runner/schema.act    the root actor of the runner
  out/derived/runner/out/bin/runner.schema    the runner
  out/derived/runner/schema.built             what the runner was built from
-}
module Acton.Derive
  ( Driver(..)
  , updateDerivedModules
  , standsFor
  ) where

import qualified Acton.BuildSpec as BuildSpec
import Acton.Builtin (mBuiltin)
import qualified Acton.CommandLineParser as C
import Acton.Compile
import qualified Acton.Embed as Embed
import qualified Acton.Env
import qualified Acton.Fingerprint as Fingerprint
import qualified Acton.Hashing as Hashing
import qualified Acton.NameInfo as I
import Acton.Project
import qualified Acton.SourceProvider as Source
import qualified Acton.Syntax as A
import Acton.Syntax (modNameToString)
import qualified InterfaceFiles
import Utils (prstr)

import Control.Exception (IOException, handle, mask_, try)
import Control.Monad
import Data.Aeson ((.:), (.=))
import Data.Bifunctor (first)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Char (GeneralCategory(Surrogate), generalCategory)
import Data.List (intercalate, isPrefixOf, nub, sort, stripPrefix, zip4)
import qualified Data.Map as M
import Data.Maybe (catMaybes, isJust, isNothing)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Clock (Clock(Monotonic), diffTimeSpec, getTime, toNanoSecs)
import System.Directory
import System.Directory.Recursive (getDirRecursive, getFilesRecursive)
import System.Exit (ExitCode(..))
import System.FilePath.Posix
import System.Posix.Files (getFileStatus, isRegularFile)
import System.Process (CreateProcess(cwd), proc, readCreateProcessWithExitCode)
import Text.Printf (printf)


-- Interface ----------------------------------------------------------------------------------------

-- | What the caller of updateDerivedModules provides.  driverWrote is
-- called each time source files of derived modules were written or removed,
-- and not once at the end: updateDerivedModules can fail, or be canceled
-- under --watch, after it wrote some of them, and no later build writes
-- them again.
data Driver = Driver
  { driverRun     :: Bool                       -- whether functions may run; if not, an entry that is out of date is an error
  , driverEvent   :: BuildEvent -> IO ()        -- the events from compiling the modules of the functions
  , driverBuild   :: FilePath -> IO Bool        -- build the executable of a root module of a runner project; False when the build failed
  , driverWrote   :: [FilePath] -> IO ()        -- source files of derived modules that were written or removed
  , driverLog     :: String -> IO ()            -- a line for the user
  , driverInfo    :: String -> IO ()            -- a line that only --verbose shows
  }

-- | Which of the two result types the function of an entry has: One
-- returns the source of one module as a str, Many the sources of several
-- modules as a dict from module name to source.
data Result = One | Many deriving (Eq, Show)

-- | Bring the derived modules of every project of a build up to date,
-- dependencies first.  The driver is told the source files of derived
-- modules that are written or removed (driverWrote).  Throws a ProjectError
-- when an entry cannot be brought up to date.  A scratch build has no
-- derived modules.  The projects of the Acton distribution that the build
-- reaches as dependencies are left alone: no build writes their outputs
-- (dependencyDirs).
updateDerivedModules :: Source.SourceProvider -> C.GlobalOptions -> CompileContext -> Driver -> IO ()
updateDerivedModules sp gopts cctx driver
  | isTmp (ccPathsRoot cctx)    = return ()
  | otherwise                   = do
      projects <- discoverProjects quietly (ccSysAbs cctx) (ccRootProj cctx) (C.dep_overrides (ccOpts cctx))
      let dirs = dependencyDirs cctx projects
      ccLockDependencies cctx dirs
      mapM_ (updateProject sp quietly cctx driver projects)
            (filter ((`elem` ccRootProj cctx : dirs) . projRoot) (dependenciesFirst projects (ccRootProj cctx)))
  where
    quietly           = gopts { C.quiet = True }

-- | The projects that the project at 'root' reaches, itself included, each
-- after the projects it depends on.
dependenciesFirst :: M.Map FilePath ProjCtx -> FilePath -> [ProjCtx]
dependenciesFirst projects root = reverse (snd (visit (Set.empty, []) root))
  where
    visit (seen, done) p
      | Set.member p seen = (seen, done)
      | otherwise         = case M.lookup p projects of
          Nothing  -> (Set.insert p seen, done)
          Just ctx -> let (seen', done') = foldl visit (Set.insert p seen, done) (map snd (projDeps ctx))
                      in (seen', ctx : done')


-- One project --------------------------------------------------------------------------------------

-- | An entry with what a run of it needs: the function, its result type,
-- the record that the entry has when it is up to date, without outputs,
-- its input files, the record of its last run, and what that record tells
-- about files.
data Job = Job
  { jobEntry    :: BuildSpec.DerivedEntry
  , jobFunction :: Function
  , jobResult   :: Result
  , jobWanted   :: Record
  , jobInputs   :: [Hashed]
  , jobStored   :: Maybe Record
  , jobKnown    :: Known
  }

jobLabel :: Job -> String
jobLabel = BuildSpec.derivedLabel . jobEntry

updateProject :: Source.SourceProvider -> C.GlobalOptions -> CompileContext -> Driver -> M.Map FilePath ProjCtx -> ProjCtx -> IO ()
updateProject sp gopts cctx driver projects ctx =
    if null entries then removeDerived >>= wrote else do
        removeRunnerOrphans root (runnerPackage projects ctx) entries
        wrote =<< removeOrphans root entries
        mapM_ (checkNoSource root) entries
        mapM_ (checkNoDependency root (map fst (projDeps ctx))) entries
        lasts     <- mapM (lastRun root . BuildSpec.derivedLabel) entries
        inputs    <- zipWithM (listInputs facts root . snd) lasts entries
        local     <- enumerateProjectModules ctx { projDerived = Set.empty }
        functions <- mapM (resolveFunction projects ctx paths local) entries
        compileFunctions sp gopts cctx driver ctx (zip entries functions)
        jobs      <- forM (zip4 entries functions inputs lasts) $ \(entry, function, ins, (stored, known)) -> do
                       (result, impl) <- readFunction root (ccOpts cctx) paths entry function
                       return (Job entry function result (wantedRecord (C.compilerIdentity gopts) entry result impl ins) ins stored known)
        stale     <- filterM (fmap isJust . outOfDate driver root) jobs
        unless (null stale || driverRun driver) $
          throwProjectError ("The derived module " ++ jobLabel (head stale) ++ " of " ++ root ++ " is out of date, and this build runs no function.\n"
                             ++ "Run acton build to bring it up to date.")
        made      <- forM stale $ \job -> do
                       start <- getTime Monotonic
                       files <- runJob driver projects ctx job
                       end   <- getTime Monotonic
                       return (job, files, fromIntegral (toNanoSecs (diffTimeSpec end start)) / 1e9 :: Double)
        forM_ made $ \(job, files, seconds) -> do
          -- A cancel of the build does not come between the move of a file
          -- and the call that tells the driver of it.
          mask_ (install facts root job files >>= wrote)
          -- The status takes 68 columns, the width that it has in the line
          -- of a module in the build log.  The label is not padded to the
          -- longest module label, since the build has no plan yet, and the
          -- line is not fitted to the width of the terminal, which only the
          -- CLI knows.
          driverLog driver (printf "   %s  %-68s  %7.3f s" (jobLabel job)
                                   (abbreviate 68 (derivedStatus (BuildSpec.derivedFunction (jobEntry job)) (length files))) seconds)
        removePathForcibly (tmpDir root)
  where
    root    = projRoot ctx
    paths   = projectPaths (ccOpts cctx) projects ctx
    entries = BuildSpec.derived_modules (projBuildSpec ctx)
    facts   = ccFiles cctx
    wrote   = driverWrote driver . filter isActFile
    -- A project without entries keeps nothing of the entries that it had.
    removeDerived = do
      files <- derivedFiles root
      removePathForcibly (projectDerivedDir root)
      return files

-- | What the line of a run says: the function, and how many modules it
-- returned when they are not just the one the line is labelled with.
derivedStatus :: String -> Int -> String
derivedStatus function 1     = "Derived by " ++ function
derivedStatus function count = "Derived " ++ show count ++ " modules by " ++ function

-- | A text cut to at most 'width' characters, the last three of them dots
-- when it is cut.
abbreviate :: Int -> String -> String
abbreviate width text
  | length text <= width = text
  | otherwise            = take (width - 3) text ++ "..."

-- | The files under the derived source directory of the project at 'root'.
derivedFiles :: FilePath -> IO [FilePath]
derivedFiles root = do
    exists <- doesDirectoryExist dir
    if exists then getFilesRecursive dir else return []
  where dir = projectDerivedSrcDir root

-- | The source file of the derived module 'name', dotted as an import
-- statement writes it, of the project at 'root'.
derivedSourceFile :: FilePath -> String -> FilePath
derivedSourceFile root name = derivedModuleFile root (A.dottedModName name)

-- | Remove the records of entries that are not in the table, and the files
-- under the derived source directory that no entry names.  A --tydb copy
-- stays: a build with --tydb writes it beside the source file of a module,
-- also of a derived module (isInterfaceCopy).  Returns the removed files.
removeOrphans :: FilePath -> [BuildSpec.DerivedEntry] -> IO [FilePath]
removeOrphans root entries = do
    exists <- doesDirectoryExist dir
    stored <- if exists then filter ((== ".json") . takeExtension) <$> listDirectory dir else return []
    mapM_ (removeFile . (dir </>)) (filter ((`notElem` map BuildSpec.derivedLabel entries) . dropExtension) stored)
    orphans <- filter (\file -> Set.notMember file named && not (isInterfaceCopy file)) <$> derivedFiles root
    mapM_ removeFile orphans
    return orphans
  where
    dir   = projectDerivedDir root
    named = Set.fromList [ derivedSourceFile root name | entry <- entries, name <- Set.toList (BuildSpec.derivedModules entry) ]

-- | A module that an entry names has no source file under src.
checkNoSource :: FilePath -> BuildSpec.DerivedEntry -> IO ()
checkNoSource root entry = forM_ (BuildSpec.derivedModules entry) $ \name -> do
    let file = joinPath (projectSrcDir root : A.dottedNames name) <.> "act"
    exists <- doesFileExist file
    when exists $
      throwProjectError ("The module " ++ name ++ " is a derived module in " ++ projectBuildAct root
                         ++ " and has the source file " ++ file ++ ".\nTake the module out of derived_modules or remove the file.")

-- | No module that an entry names has the name of a dependency of the
-- project as the first name of its dotted name.  Such a name names a module
-- of the dependency.
checkNoDependency :: FilePath -> [String] -> BuildSpec.DerivedEntry -> IO ()
checkNoDependency root deps entry = forM_ (BuildSpec.derivedModules entry) $ \name ->
    case A.dottedNames name of
      dep : _ | dep `elem` deps ->
        throwProjectError ("The module " ++ name ++ " is a derived module in " ++ projectBuildAct root
                           ++ ", and " ++ dep ++ " is a dependency of the project.\n"
                           ++ "The name " ++ name ++ " names a module of the dependency " ++ dep
                           ++ ". Give the derived module another name.")
      _ -> return ()


-- Input files --------------------------------------------------------------------------------------

-- | The input files of an entry in the project at 'root', each as the build
-- sees it (inputFile): a path that names a file is that file, and a path
-- that names a directory is every file below it that the path stands for
-- (standsFor).  The files are relative to the project root with / between
-- the components, each once, in sorted order.  Each component of an input
-- path is spelled as its directory lists it, as for a path of embed
-- (Embed.listedPath): an entry then names the same files on a file system
-- that takes one spelling of a name for another and on one that does not.
-- A path that names nothing is an error, and so is a file whose name is not
-- text.  Below a directory, every entry that is no directory counts as a
-- file, so a symbolic link that names nothing is an error too.  A file can
-- be removed between its listing and the look at it.  The files are then
-- listed once more, and a file that is no input file after that is an
-- error.
listInputs :: FileFacts -> FilePath -> Known -> BuildSpec.DerivedEntry -> IO [Hashed]
listInputs facts root known entry = do
    first <- listing
    case first of
      Right files -> return files
      Left _      -> either refuse return =<< listing
  where
    listing = do
      files <- Set.toAscList . Set.fromList . concat <$> mapM expand (BuildSpec.derivedInputs entry)
      -- A byte of a file name that is not text in the encoding of the
      -- locale is read as a character that stands for that byte alone, a
      -- surrogate.  The path of an input file is a str of the function,
      -- which cannot hold it, and a message cannot print it.
      forM_ (filter (any undecoded) files) $ \path ->
        refuse (map (\c -> if undecoded c then '?' else c) path ++ " has a name with bytes that are not text in the encoding of the locale, "
                ++ "shown as ?. Build in a locale with the encoding UTF-8, or rename the file")
      sequence <$> mapM (inputFile facts known root) files
    expand input = do
      let path = normalise (dropTrailingPathSeparator input)
          names what = "The input path " ++ BuildSpec.buildOptionString input ++ " of the derived module " ++ label ++ " names " ++ what ++ " in " ++ root
      listed <- Embed.listedPath (listedIn facts) root path
      isFile <- doesFileExist (root </> path)
      isDir  <- doesDirectoryExist (root </> path)
      case listed of
        _ | not (isFile || isDir)  -> throwProjectError (names "no file and no directory" ++ ".")
        Just named | named /= path -> throwProjectError (names ((if isDir then "the directory " else "the file ") ++ named)
                                                         ++ ", and the path is written " ++ path ++ ".")
        _ | isFile                 -> return [path]
          | otherwise              -> filter (standsFor path) . map (makeRelative root)
                                        <$> (filterM (fmap not . doesDirectoryExist) =<< getDirRecursive (root </> path))
    undecoded c = generalCategory c == Surrogate
    label       = BuildSpec.derivedLabel entry
    refuse why  = throwProjectError ("The derived module " ++ label ++ " in " ++ projectBuildAct root ++ " is not brought up to date: " ++ why ++ ".")

-- | Whether an input path that names the directory 'dir' stands for
-- 'file', a file below it: for every file except one whose name starts with
-- a dot, one below a directory under 'dir' whose name starts with a dot,
-- and a --tydb copy, which a build writes beside a source file.  A --tydb
-- copy is known by its name alone (isInterfaceCopy), so a file in a
-- directory with the extension .tydb and a file with that extension are
-- left out.  Editors and file managers leave files of the first
-- two kinds.  An input path that names such a file itself, or a directory
-- whose own name starts with a dot, is taken as it is written.  A listing
-- of the input files and a watch of the input paths both go by this rule,
-- so that the files whose change starts a build are the input files.
standsFor :: FilePath -> FilePath -> Bool
standsFor dir file = case stripPrefix (splitDirectories dir) (splitDirectories file) of
    Just below -> not (any ("." `isPrefixOf`) below || isInterfaceCopy (joinPath below))
    Nothing    -> False


-- Hashes and stats ---------------------------------------------------------------------------------

-- | A file with the SHA-256 of its content: its path relative to the
-- project root, its stat from before the content was read, the SHA-256,
-- and whether the file had to be read for it, which is so when the record
-- of its entry does not stand for it (statRule).
data Hashed = Hashed
  { hashedPath :: FilePath
  , hashedStat :: InterfaceFiles.SourceFileMeta
  , hashedHash :: String
  , hashedRead :: Bool
  }

-- | What the record of an entry's last run tells about files without
-- reading them: what it recorded of each input file and of each file that
-- the entry wrote, by path relative to the project root.
type Known = M.Map FilePath Recorded

-- | The record of an entry's last run, Nothing when it has none, and what
-- that record tells about files.  The stat of the record's file is taken
-- before the file is read, so that the time is never later than that of
-- the record that is read.
lastRun :: FilePath -> String -> IO (Maybe Record, Known)
lastRun root label = do
    stat   <- try (readSourceFileMeta file) :: IO (Either IOException InterfaceFiles.SourceFileMeta)
    stored <- readRecord file
    return (stored, case (stored, stat) of
      (Just r, Right meta) -> M.fromList [ (path, Recorded recorded sha (InterfaceFiles.sfmMTimeNs meta))
                                         | (path, sha) <- recInputs r ++ recOutputs r
                                         , Just recorded <- [M.lookup path (recStats r)] ]
      _                    -> M.empty)
  where file = recordFile root label

-- | The source file of a derived module of the project at 'root', relative
-- to the project root.
moduleFile :: FilePath -> String -> FilePath
moduleFile root name = makeRelative root (derivedSourceFile root name)

-- | A file with the given path, stat and SHA-256.  The file had to be read
-- when what its entry knows does not stand for it.
hashed :: Known -> FilePath -> InterfaceFiles.SourceFileMeta -> String -> Hashed
hashed known path meta sha = Hashed path meta sha (isNothing (statRule (M.lookup path known) meta))

-- | An input file of the project at 'root' as the build sees it, or why
-- the path names no input file: there is no regular file, or the file
-- cannot be read.  The look goes through the file facts of the build
-- (fileFact): the first look at a path reads the file unless the record of
-- the entry stands for it, and a later look reads nothing.  A later look at
-- a file that is not the one of the first look stops the build, since the
-- file changed while the build ran.
inputFile :: FileFacts -> Known -> FilePath -> FilePath -> IO (Either String Hashed)
inputFile facts known root path = handle (\(FileChanged _) -> throwProjectError (changedWhileBuilding path)) $ do
    status <- either (\e -> Left (unreadable e, Nothing)) regular <$> try (getFileStatus file)
    sha    <- handle (return . Left . unreadable) (fileFact facts (M.lookup path known) file status)
    return (hashed known path <$> first fst status <*> sha)
  where
    file       = root </> path
    unreadable = Embed.cannotBeRead path
    regular st
      | isRegularFile st = Right (fileMeta st)
      | otherwise        = Left (path ++ " is not a regular file", Just (fileMeta st))

-- | A file that an entry wrote, with the SHA-256 of its content.  The file
-- is not read when the record of the entry stands for it (statRule).  The
-- look does not go through the file facts of the build: the build itself
-- writes such a file.
writtenFile :: Known -> FilePath -> FilePath -> IO Hashed
writtenFile known root path = do
    meta <- readSourceFileMeta file
    hashed known path meta <$> maybe (hashOfFile file) return (statRule (M.lookup path known) meta)
  where file = root </> path

statsOf :: [Hashed] -> M.Map FilePath InterfaceFiles.SourceFileMeta
statsOf files = M.fromList [ (hashedPath f, hashedStat f) | f <- files ]


-- Functions ----------------------------------------------------------------------------------------

-- | The function of an entry.
data Function = Function
  { fnName   :: A.Name
  , fnModule :: A.ModName          -- its module, as interface files name it
  , fnImport :: String             -- its module, as a runner imports it
  , fnSource :: Maybe FilePath     -- the source file of its module, Nothing for a module that no project of the build has
  }

-- | Find the module of an entry's function as an import statement in the
-- entry's project would: a module of the project itself, 'local' being the
-- project's hand-written modules, else a module of a dependency that the
-- project declares, else a module that no project of the build has, which
-- is one of the Acton distribution or of a --searchpath directory.  A
-- module of the project that is written with the project's name in front
-- is refused, and so are a derived module of the project and a module of a
-- project of the build that the project does not declare as a dependency.
resolveFunction :: M.Map FilePath ProjCtx -> ProjCtx -> Paths -> [(FilePath, A.ModName)] -> BuildSpec.DerivedEntry -> IO Function
resolveFunction projects ctx paths local entry
  | Just file <- sourceOf own local     = return (Function name own (modNameToString own) (Just file))
  | isJust (sourceOf (A.modName path) local)
                                        = refuse (" with the name of its own project in front.\nWrite function=" ++ intercalate "." (drop 1 names) ++ ".")
  | any (`elem` derivedOwn) [written, moduleLabel paths (A.modName path)]
                                        = refuse (", which is a derived module of the same project.\nPut the function in a module of the project that has a file under src.")
  | dep : rest <- path
  , Just depRoot <- lookup dep (projDeps ctx)
  , Just depCtx <- M.lookup depRoot projects
                                        = do
      -- The derived modules of the dependency are listed here and not taken
      -- from depCtx, which has them as they were before this build brought
      -- the dependency up to date.
      derived <- derivedModuleNames depRoot
      modules <- enumerateProjectModules depCtx { projDerived = derived }
      let canonical = A.modName (if null rest then [dep, "lib"] else path)
      case sourceOf canonical modules of
        Just file -> return (Function name canonical written (Just file))
        Nothing   -> refuse (", which the dependency " ++ dep ++ " does not have.")
  | dep : _ <- path
  , dep /= "std"
  , dep `elem` undeclared               = refuse (" of the project " ++ dep ++ ", which is not a dependency in that Build.act.\nAdd " ++ dep ++ " to its dependencies.")
  | otherwise                           = return (Function name (A.modName path) written Nothing)
  where
    names      = A.dottedNames (BuildSpec.derivedFunction entry)
    path       = init names
    name       = A.name (last names)
    written    = intercalate "." path
    own        = addProjPrefix paths (A.modName path)
    derivedOwn = concatMap (Set.toList . BuildSpec.derivedModules) (BuildSpec.derived_modules (projBuildSpec ctx))
    undeclared = [ BuildSpec.specName (projBuildSpec p) | p <- M.elems projects, projRoot p `notElem` projRoot ctx : map snd (projDeps ctx) ]
    sourceOf mn modules = lookup mn [ (m, file) | (file, m) <- modules ]
    refuse why = throwProjectError ("The function " ++ BuildSpec.derivedFunction entry ++ " of the derived module " ++ BuildSpec.derivedLabel entry
                                    ++ " in " ++ projectBuildAct (projRoot ctx) ++ " names the module " ++ written ++ why)

-- | Compile the modules of the functions of a project's entries, and what
-- they import, through the front passes.  The plan is one of the build:
-- the same root project, dependency selection and options, without the
-- options that ask for another output.  It fails when a function imports a
-- derived module of its own project.  When a project of the build declares
-- a build library, the plan also holds every module of a library that
-- holds one of its modules (expandLibraries), and a derived module among
-- them is compiled as it is on disk.
compileFunctions :: Source.SourceProvider -> C.GlobalOptions -> CompileContext -> Driver -> ProjCtx -> [(BuildSpec.DerivedEntry, Function)] -> IO ()
compileFunctions sp gopts cctx driver ctx functions = unless (null sources) $ do
    plan <- planFetched sp gopts frontContext (PlanRequest sources SelectedFiles Nothing)
    let derivedOwn = maybe Set.empty projDerived (M.lookup root (cpProjMap plan))
        isOwnDerived t = tkProj (gtKey t) == root && Set.member (dropProjPrefix (gtPaths t) (tkMod (gtKey t))) derivedOwn
    forM_ (filter isOwnDerived (cpNeededTasks plan)) $ \t ->
      forM_ functions $ \(entry, function) ->
        forM_ (importChain (cpNeededTasks plan) (TaskKey root (fnModule function)) (gtKey t)) $ \chain ->
          throwProjectError ("The function " ++ BuildSpec.derivedFunction entry ++ " of the derived module " ++ BuildSpec.derivedLabel entry
                             ++ " in " ++ projectBuildAct root ++ " imports a derived module of its own project: "
                             ++ intercalate " imports " (map (moduleLabel (gtPaths t)) chain) ++ ".")
    result <- runStandalonePlan sp gopts plan (driverEvent driver)
    case result of
      Left failure     -> throwProjectError (compileFailureMessage failure)
      Right (_, True)  -> throwProjectError ("The derived modules of " ++ root ++ " are not brought up to date: the modules of their functions have errors.")
      Right (_, False) -> return ()
  where
    root         = projRoot ctx
    sources      = nub (catMaybes [ fnSource function | (_, function) <- functions ])
    frontOptions = (ccOpts cctx)
      { C.parse = False, C.parse_ast = False, C.kinds = False, C.types = False, C.escape_analysis = False
      , C.witness_stats = False, C.sigs = False, C.norm = False, C.deact = False, C.cps = False, C.llift = False
      , C.box = False, C.hgen = False, C.cgen = False, C.ccmd = False, C.tydb = False
      , C.skip_build = True, C.watch = False, C.root = "" }
    frontContext = cctx { ccOpts = frontOptions }

-- | The modules on a path of imports from one module to another among the
-- tasks, both ends included, or Nothing when there is no such path.
importChain :: [GlobalTask] -> TaskKey -> TaskKey -> Maybe [A.ModName]
importChain tasks from to = map tkMod <$> go (Set.singleton from) [[from]]
  where
    providers = M.fromList [ (gtKey t, M.elems (gtImportProviders t)) | t <- tasks ]
    go _ [] = Nothing
    go seen (chain@(k : _) : rest)
      | k == to   = Just (reverse chain)
      | otherwise = let next = filter (`Set.notMember` seen) (nub (M.findWithDefault [] k providers))
                    in go (foldr Set.insert seen next) (rest ++ map (: chain) next)
    go seen ([] : rest) = go seen rest

-- | The result type and the implementation hash of an entry's function,
-- read from the interface file of its module.  The module, the function,
-- and a result type of str or dict are required here.  The parameters of
-- the function are checked when its runner is compiled, since the runner
-- holds the call.  The interface file of a module that no project of the
-- build has is looked for in the --searchpath directories and in the Acton
-- distribution only.  The types directory of a project is left out for
-- such a module: an interface file there is of a module of that project,
-- and compileFunctions did not bring it up to date.
readFunction :: FilePath -> C.CompileOptions -> Paths -> BuildSpec.DerivedEntry -> Function -> IO (Result, String)
readFunction root opts paths entry function = do
    found <- Acton.Env.findTyFile dirs (fnModule function)
    tydb  <- maybe (bad ("the module " ++ written ++ " is not found in the project, in a dependency that the project declares "
                         ++ "or in the Acton distribution.")) return found
    db    <- InterfaceFiles.openInterfaceDB tydb
    info  <- InterfaceFiles.readInterfaceDBNameInfoMaybe db (fnName function)
    hash  <- InterfaceFiles.readNameHashMaybe tydb (fnName function)
    case (info, hash) of
      (Just (_, i), Just h) -> do
        result <- either (\why -> bad (why ++ "\n" ++ functionShape)) return (resultOf i)
        when (result == One && length (BuildSpec.derivedModules entry) > 1) $
          bad ("it returns str, the source of one module, and the entry names " ++ show (length (BuildSpec.derivedModules entry))
               ++ " modules.\nA function for several modules returns dict[str, str], from module name to source.")
        return (result, Hashing.hashHex (InterfaceFiles.nhImplHash h))
      _ -> bad ("the module " ++ written ++ " does not define " ++ A.nstr (fnName function) ++ ".")
  where
    written = intercalate "." (init (A.dottedNames (BuildSpec.derivedFunction entry)))
    dirs    = case fnSource function of
                Just _  -> searchPath paths
                Nothing -> C.searchpath opts ++ systemTypePaths (sysPath paths)
    bad msg = throwProjectError ("The function " ++ BuildSpec.derivedFunction entry ++ " of the derived module " ++ BuildSpec.derivedLabel entry
                                 ++ " in " ++ projectBuildAct root ++ " cannot be used: " ++ msg)

-- | What the function of an entry takes and returns.
functionShape :: String
functionShape = "The function of a derived module takes the descriptions of the input files, "
             ++ "a dict[str, std.build.derive.InputFile], and then the arguments of its entry.\n"
             ++ "It returns str, the source of one module, or dict[str, str], the sources of several modules by module name."

-- | Which of the two result types a name has, or else what is wrong with
-- the name, as the text that readFunction puts after "cannot be used: ".
resultOf :: I.NameInfo -> Either String Result
resultOf (I.NDef sc _ _)        = resultOfType (A.sctype sc)
resultOf (I.NSig sc _ _)        = resultOfType (A.sctype sc)
resultOf _                      = Left "it is not a function."

resultOfType :: A.Type -> Either String Result
resultOfType t@A.TFun{}
  | builtin "str" (A.restype t)   = Right One
  | builtin "dict" (A.restype t)  = Right Many
  where
    builtin s (A.TCon _ (A.TC qn _)) = A.nstr (A.noq qn) == s && case qn of
                                         A.NoQ _ -> True
                                         _       -> A.mname qn == mBuiltin
    builtin _ _                      = False
resultOfType t                    = Left ("it has the type\n  " ++ prstr t)


-- Records ------------------------------------------------------------------------------------------

-- | What the last run of an entry was made from, and the files that the
-- build wrote for it under out/derived/src, which are the source files of
-- the modules that the function returned.  An input file and a written
-- file are each listed by path relative to the project root, with the
-- SHA-256 of the content.  recStats holds the stat of each of these files
-- by path, taken before the read that gave the SHA-256.  In the file of a
-- record a stat is the list of its modification time, its status change
-- time, its size, its device and its inode.
data Record = Record
  { recCompiler :: String
  , recEntry    :: BuildSpec.DerivedEntry
  , recResult   :: String
  , recImpl     :: String
  , recInputs   :: [(FilePath, String)]
  , recOutputs  :: [(FilePath, String)]
  , recStats    :: M.Map FilePath InterfaceFiles.SourceFileMeta
  } deriving (Eq, Show)

instance Aeson.ToJSON Record where
  toJSON r = Aeson.object
    [ "compiler"       .= recCompiler r
    , "entry"          .= recEntry r
    , "result"         .= recResult r
    , "implementation" .= recImpl r
    , "inputs"         .= recInputs r
    , "outputs"        .= recOutputs r
    , "stats"          .= M.map fields (recStats r)
    ]
    where fields (InterfaceFiles.SourceFileMeta modified changed size device inode) = (modified, changed, size, device, inode)

instance Aeson.FromJSON Record where
  parseJSON = Aeson.withObject "Record" $ \o ->
    Record <$> o .: "compiler" <*> o .: "entry" <*> o .: "result" <*> o .: "implementation" <*> o .: "inputs" <*> o .: "outputs"
           <*> (M.map stat <$> o .: "stats")
    where stat (modified, changed, size, device, inode) = InterfaceFiles.SourceFileMeta modified changed size device inode

recordFile :: FilePath -> String -> FilePath
recordFile root label = projectDerivedDir root </> label <.> "json"

-- | The record in a file, Nothing when the file is missing or unreadable.
readRecord :: FilePath -> IO (Maybe Record)
readRecord file = do
    content <- try (B.readFile file) :: IO (Either IOException B.ByteString)
    return (either (const Nothing) Aeson.decodeStrict' content)

-- | Write the record of an entry in place of the one it has.
writeRecord :: FilePath -> String -> Record -> IO ()
writeRecord root label record = do
    BL.writeFile (file ++ ".tmp") (Aeson.encode record)
    renameFile (file ++ ".tmp") file
  where file = recordFile root label

-- | The record of an entry that is up to date, without its outputs.
wantedRecord :: String -> BuildSpec.DerivedEntry -> Result -> String -> [Hashed] -> Record
wantedRecord version entry result impl inputs = Record
    { recCompiler = version
    , recEntry    = entry
    , recResult   = case result of { One -> "str"; Many -> "dict" }
    , recImpl     = impl
    , recInputs   = [ (hashedPath f, hashedHash f) | f <- inputs ]
    , recOutputs  = []
    , recStats    = statsOf inputs
    }

-- | Why a job has to run, Nothing when its entry is up to date.  What was
-- found is reported under --verbose.  When the entry is up to date and a
-- file was read for its hash, the record is written again with the stats
-- of the files, so that the next build does not read them.
outOfDate :: Driver -> FilePath -> Job -> IO (Maybe String)
outOfDate driver root job = do
    verdict <- case jobStored job of
      Nothing -> return (Left "it has no record of a run")
      Just r
        | recCompiler r /= recCompiler wanted -> return (Left "the compiler version changed")
        | recEntry r    /= recEntry wanted    -> return (Left "its entry in Build.act changed")
        | recResult r   /= recResult wanted   -> return (Left "the result type of its function changed")
        | recImpl r     /= recImpl wanted     -> return (Left "the implementation of its function changed")
        | recInputs r   /= recInputs wanted   -> return (Left "its input files changed")
        | otherwise                           -> do
            outputs <- mapM current (recOutputs r)
            return (maybe (Left "its files under out/derived/src are not as its last run left them") (Right . (,) r) (sequence outputs))
    case verdict of
      Left reason -> do
        info ("out of date, " ++ reason)
        return (Just reason)
      Right (r, outputs) -> do
        let files = jobInputs job ++ outputs
            nread = length (filter hashedRead files)
        when (nread > 0) $ writeRecord root (jobLabel job) r { recStats = statsOf files }
        info ("up to date, " ++ show nread ++ " of " ++ show (length files) ++ " files read")
        return Nothing
  where
    wanted = jobWanted job
    info msg = driverInfo driver ("Derived module " ++ jobLabel job ++ " of " ++ root ++ ": " ++ msg)
    -- A written file with the recorded content, Nothing when it is missing
    -- or holds something else.
    current (path, hash) = do
      file <- try (writtenFile (jobKnown job) root path) :: IO (Either IOException Hashed)
      return (either (const Nothing) (\f -> if hashedHash f == hash then Just f else Nothing) file)


-- Runners ------------------------------------------------------------------------------------------

-- | The directory of the runner project of the project at 'root'.
runnerDir :: FilePath -> FilePath
runnerDir root = projectDerivedDir root </> "runner"

-- | The directory that the runs of the project at 'root' write to before
-- their modules are moved into place.
tmpDir :: FilePath -> FilePath
tmpDir root = projectDerivedDir root </> "tmp"

-- | The source of the root module of a job's runner: its root actor has
-- std.build.derive call the function with the descriptions of the input
-- files and the arguments of the entry.  The call is in a function with
-- the types that std.build.derive asks for, so that the compiler reports
-- a function of another type at the call.  The runner imports the two
-- modules under names of its own, so that no module name of a project is
-- the name of something else in the runner.
runnerSource :: Job -> String
runnerSource job = unlines
    [ "# Written by acton build.  Calls the function of the derived module " ++ jobLabel job ++ "."
    , "import std.build.derive as runner"
    , "import " ++ fnImport (jobFunction job) ++ " as fn"
    , ""
    , "proc def call(inputs: dict[str, runner.InputFile]) -> " ++ result ++ ":"
    , "    return " ++ functionCall ("fn." ++ A.nstr (fnName (jobFunction job))) job
    , ""
    , "actor main(env: Env):"
    , "    proc def __startup__():"
    , "        runner." ++ run ++ "(env, call)"
    ]
  where (run, result) = case jobResult job of
                          One  -> ("run_one", "str")
                          Many -> ("run_many", "dict[str, str]")

-- | The call of a job's function, written with 'function' for the function:
-- the descriptions of the input files, then the arguments of the entry as
-- keyword arguments.
functionCall :: String -> Job -> String
functionCall function job =
    function ++ "(" ++ intercalate ", " ("inputs" : [ k ++ "=" ++ BuildSpec.derivedArgLiteral v | (k, v) <- M.toList args ]) ++ ")"
  where args = BuildSpec.derivedArgs (jobEntry job)

-- | The first of 'name', 'name' with an underscore, and so on, that is not
-- among the taken names.
unusedName :: [String] -> String -> String
unusedName taken name = head (filter (`notElem` taken) (iterate (++ "_") name))

-- | The name and directory of every project that a project reaches, itself
-- included.  The first directory found for a name stays.
reachableProjects :: M.Map FilePath ProjCtx -> ProjCtx -> M.Map String FilePath
reachableProjects projects ctx = go (M.singleton (BuildSpec.specName (projBuildSpec ctx)) (projRoot ctx)) (projDeps ctx)
  where
    go found []                 = found
    go found ((name, dir) : rest)
      | M.member name found     = go found rest
      | otherwise               = go (M.insert name dir found) (rest ++ maybe [] projDeps (M.lookup dir projects))

-- | The directory under src of the runner project of 'ctx' that holds the
-- sources of its runners: runner, with underscores added until no project
-- that 'ctx' reaches has that name.
runnerPackage :: M.Map FilePath ProjCtx -> ProjCtx -> String
runnerPackage projects ctx = unusedName (M.keys (reachableProjects projects ctx)) "runner"

-- | Remove the source, the .built file and the executable of each runner
-- of the project at 'root' whose label no entry has.  When the runner
-- project has a source under another directory than 'package', the whole
-- runner project is removed: that directory can have the name of a project
-- that the runner project depends on, and a build refuses such a source.
removeRunnerOrphans :: FilePath -> String -> [BuildSpec.DerivedEntry] -> IO ()
removeRunnerOrphans root package entries = do
    exists  <- doesDirectoryExist src
    sources <- if exists then map (makeRelative src) . filter isActFile <$> getFilesRecursive src else return []
    if any ((/= [package]) . take 1 . splitDirectories) sources
      then removePathForcibly dir
      else forM_ sources $ \file -> do
             let label = intercalate "." (drop 1 (splitDirectories (dropExtension file)))
             unless (label `elem` map BuildSpec.derivedLabel entries) $
               mapM_ removePathForcibly [src </> file, dir </> label <.> "built", projectBinDir dir </> (package ++ "." ++ label)]
  where
    dir = runnerDir root
    src = dir </> "src"

-- | Run the runner of a job, and return the modules it wrote to the
-- temporary directory: for each, the source file that the module gets
-- under out/derived/src, relative to the project root, and the file that
-- the runner wrote.  They are exactly the modules that the entry names, or
-- the run is an error.  The runner is built first, unless the runner in
-- place was built from the same source, the same implementation hash of the
-- function and the same compiler version, for example when only an input
-- file of the entry changed.
runJob :: Driver -> M.Map FilePath ProjCtx -> ProjCtx -> Job -> IO [(FilePath, FilePath)]
runJob driver projects ctx job = do
    exists <- doesFileExist binary
    built  <- try (B.readFile stampFile) :: IO (Either IOException B.ByteString)
    unless (exists && built == Right stamp) $ do
      removePathForcibly stampFile
      createDirectoryIfMissing True (takeDirectory source)
      writeChanged (dir </> "Build.act") (BuildSpec.renderBuildAct spec)
      writeChanged source (runnerSource job)
      ok <- driverBuild driver source
      unless ok $
        throwProjectError ("The derived module " ++ label ++ " in " ++ projectBuildAct root ++ " is not brought up to date: "
                           ++ "the program that calls its function was not built.\nIts entry gives the call\n  "
                           ++ functionCall (BuildSpec.derivedFunction entry) job ++ "\n" ++ functionShape)
      B.writeFile stampFile stamp
    removePathForcibly out
    createDirectoryIfMissing True out
    BL.writeFile manifest (Aeson.encode (Aeson.object
      [ "root"    .= root
      , "out"     .= out
      , "modules" .= BuildSpec.derivedModules entry
      , "inputs"  .= [ Aeson.object [ "path" .= hashedPath f, "size" .= InterfaceFiles.sfmSize (hashedStat f), "sha256" .= hashedHash f ]
                     | f <- jobInputs job ]
      ]))
    (code, stdout, stderr) <- readCreateProcessWithExitCode (proc binary [manifest]) { cwd = Just root } ""
    when (code /= ExitSuccess) $
      throwProjectError ("The function " ++ BuildSpec.derivedFunction entry ++ " of the derived module " ++ label
                         ++ " in " ++ projectBuildAct root ++ " failed:\n" ++ stdout ++ stderr)
    files <- filter isActFile <$> getFilesRecursive out
    let modules  = sort [ (intercalate "." (splitDirectories (dropExtension (makeRelative out file))), file) | file <- files ]
        returned = Set.fromList (map fst modules)
        named    = BuildSpec.derivedModules entry
        listed title names = [ title ++ ": " ++ intercalate ", " (Set.toList names) | not (Set.null names) ]
    unless (returned == named) $
      throwProjectError (intercalate "\n" $
        [ "The function " ++ BuildSpec.derivedFunction entry ++ " of the derived module " ++ label ++ " in " ++ projectBuildAct root
          ++ " returned other modules than its entry names." ]
        ++ listed "Returned and not named" (returned `Set.difference` named)
        ++ listed "Named and not returned" (named `Set.difference` returned)
        ++ [ "An entry names exactly the modules that its function returns. Change the names of the entry or the function." ])
    return [ (moduleFile root name, file) | (name, file) <- modules ]
  where
    root      = projRoot ctx
    entry     = jobEntry job
    label     = jobLabel job
    wanted    = jobWanted job
    stamp     = TE.encodeUtf8 (T.pack (unlines [recCompiler wanted, recImpl wanted] ++ runnerSource job))
    stampFile = dir </> label <.> "built"
    reachable = reachableProjects projects ctx
    package   = runnerPackage projects ctx
    name      = unusedName (M.keys reachable) "acton_derive"
    dir       = runnerDir root
    source    = joinPath (dir : "src" : package : A.dottedNames label) <.> "act"
    binary    = projectBinDir dir </> (package ++ "." ++ label)
    out       = tmpDir root </> label </> "src"
    manifest  = tmpDir root </> label </> "manifest.json"
    spec      = (scratchBuildSpec root)
      { BuildSpec.specName     = name
      , BuildSpec.fingerprint  = Fingerprint.formatFingerprint (Fingerprint.updateFingerprintPrefix (Fingerprint.fingerprintPrefixForName name)
                                                                 (fromIntegral (Fingerprint.fingerprintPrefixForName root)))
      , BuildSpec.dependencies = M.map (\p -> BuildSpec.PkgDep Nothing Nothing (Just p) Nothing Nothing Nothing Nothing) reachable
      }

-- | Write a file unless it holds the content already.
writeChanged :: FilePath -> String -> IO ()
writeChanged file content = do
    old <- try (B.readFile file) :: IO (Either IOException B.ByteString)
    unless (old == Right new) (B.writeFile file new)
  where new = TE.encodeUtf8 (T.pack content)

-- | Move the modules of a run into the derived source directory and write
-- the record.  'files' has, for each module, the path that its source file
-- gets, relative to the project root, and the file in the temporary
-- directory.  The function ran after the build took the SHA-256 of the
-- input files.  So each input file is looked at again first (inputFile),
-- and one that is not the file of the first look stops the build before a
-- module is moved: the function may have made the modules from other
-- content than its descriptions are of.  A module whose file holds the same
-- source already is left alone, so the modules that import it are not
-- compiled again.  A file that is moved into place is read once more for
-- its stat: the stat is taken before that read, and it is recorded when the
-- read gives the SHA-256 of the source that the function returned.
-- Returns the files written.
install :: FileFacts -> FilePath -> Job -> [(FilePath, FilePath)] -> IO [FilePath]
install facts root job files = do
    mapM_ (inputFile facts (jobKnown job) root . hashedPath) (jobInputs job)
    done <- forM files $ \(path, file) -> do
      new <- hashOfFile file
      old <- try (writtenFile (jobKnown job) root path) :: IO (Either IOException Hashed)
      case old of
        Right kept | hashedHash kept == new -> return ((path, new), statsOf [kept], [])
        _ -> do
          createDirectoryIfMissing True (takeDirectory (root </> path))
          renameFile file (root </> path)
          placed <- writtenFile M.empty root path
          return ((path, new), statsOf [ placed | hashedHash placed == new ], [root </> path])
    writeRecord root (jobLabel job) (jobWanted job)
      { recOutputs = [ output | (output, _, _) <- done ]
      , recStats   = M.unions (statsOf (jobInputs job) : [ stats | (_, stats, _) <- done ])
      }
    return (concat [ written | (_, _, written) <- done ])
