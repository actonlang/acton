{-# LANGUAGE OverloadedStrings #-}
{-|
Derived modules.  A derived module is a module of a project that has no
file under src.  An entry of the derived_modules table of Build.act names
the modules, a function, input paths and arguments, and the function
returns the source of the modules from a description of each input file
and the arguments.  A description holds the path, the size and the SHA-256
of a file, and the function reads the content of a file through it.  An
entry names a module with a module name, or with a name that ends with .*
and stands for every module below it (BuildSpec.derivedCovers).
The source is written under out/derived/src, from where a build discovers
and compiles it like any other module (Acton.Project).

updateDerivedModules brings the derived modules of every project of a build
up to date, dependencies first, before the build discovers its modules.
For a project it

  1. removes the record and the runner of each entry that is no longer in
     the table, and each file under out/derived/src that no entry names.
     A --tydb copy, which a build writes beside the source file of a module
     (isInterfaceCopy), is removed only with that source file,
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
  7. checks that the function returned every module that the entry names
     with a module name, and no module that the entry does not name,
     removes the source files that the entry names and the run did not
     return, moves the returned modules into out/derived/src and writes
     the record.  A file whose source did not change is left alone.

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
function, implementation hash, result type, names, arguments, input paths
and input hashes, and the source files under out/derived/src that the
entry names are the files that the record lists, with the recorded
content.  A source file below a name that ends with .* that no run of the
entry returned makes the entry out of date, and the run that follows
removes it.  A name's implementation hash covers its own implementation
and that of every name it uses, in every module (Acton.Hashing).  So a
change to code that the function neither calls nor reads does not run it
again, and a change of a constant that it reads does.

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
looks at each input file of the entry again after the run, before it
removes a source file or moves a returned module into place (install).
The read of a description itself fails when the content does not have the
size and the SHA-256 of the description (std.build.derive).

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

The record and the files of the runner of an entry have its label in their
names, the smallest of its names (BuildSpec.derivedLabel), with the * of a
label written all (BuildSpec.derivedFileLabel).  The source file of a
module has the name of the module.  For the entry layers.*:

  out/derived/src/layers/base.act                the source of the module layers.base
  out/derived/layers.all.json                    the record
  out/derived/runner/src/runner/layers/all.act   the root actor of the runner
  out/derived/runner/out/bin/runner.layers.all   the runner
  out/derived/runner/layers.all.built            what the runner was built from
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
import Data.List (intercalate, isPrefixOf, nub, partition, sort, stripPrefix, zip4)
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
-- its input files, the record of its last run, what that record tells
-- about files, and the source files under out/derived/src that the entry
-- names (namesFile), by path relative to the project root.
data Job = Job
  { jobEntry    :: BuildSpec.DerivedEntry
  , jobFunction :: Function
  , jobResult   :: Result
  , jobWanted   :: Record
  , jobInputs   :: [Hashed]
  , jobStored   :: Maybe Record
  , jobKnown    :: Known
  , jobPresent  :: Set.Set FilePath
  }

-- | An entry as a message names it: its label and where the entry is.
entryText :: FilePath -> BuildSpec.DerivedEntry -> String
entryText root entry = BuildSpec.derivedLabel entry ++ " of derived_modules in " ++ projectBuildAct root

-- | The start of a message about the function of an entry.
functionText :: FilePath -> BuildSpec.DerivedEntry -> String
functionText root entry = "The function " ++ BuildSpec.derivedFunction entry ++ " of the entry " ++ entryText root entry

-- | Whether an entry has one name, a module name.
namesOneModule :: BuildSpec.DerivedEntry -> Bool
namesOneModule entry = Set.size (BuildSpec.derivedNames entry) == 1 && null (BuildSpec.derivedBelow entry)

updateProject :: Source.SourceProvider -> C.GlobalOptions -> CompileContext -> Driver -> M.Map FilePath ProjCtx -> ProjCtx -> IO ()
updateProject sp gopts cctx driver projects ctx =
    if null entries then removeDerived >>= wrote else do
        removeRunnerOrphans root (runnerPackage projects ctx) entries
        (removed, present) <- removeOrphans root entries
        wrote removed
        mapM_ (checkNoSource root) entries
        mapM_ (checkNoDependency root (map fst (projDeps ctx))) entries
        lasts     <- mapM (lastRun root) entries
        inputs    <- zipWithM (listInputs facts root . snd) lasts entries
        local     <- enumerateProjectModules ctx { projDerived = Set.empty }
        functions <- mapM (resolveFunction projects ctx paths local) entries
        compileFunctions sp gopts cctx driver ctx (zip entries functions)
        jobs      <- forM (zip4 entries functions inputs lasts) $ \(entry, function, ins, (stored, known)) -> do
                       (result, impl) <- readFunction root (ccOpts cctx) paths entry function
                       return (Job entry function result (wantedRecord (C.compilerIdentity gopts) entry result impl ins) ins stored known
                                   (Set.fromList (map (makeRelative root) (filter (namesFile root entry) present))))
        stale     <- filterM (fmap isJust . outOfDate driver root) jobs
        unless (null stale || driverRun driver) $
          throwProjectError ("The entry " ++ entryText root (jobEntry (head stale)) ++ " is out of date, and this build runs no function.\n"
                             ++ "Run acton build to bring it up to date.")
        made      <- forM stale $ \job -> do
                       start <- getTime Monotonic
                       files <- runJob driver projects ctx job
                       end   <- getTime Monotonic
                       return (job, files, fromIntegral (toNanoSecs (diffTimeSpec end start)) / 1e9 :: Double)
        forM_ made $ \(job, files, seconds) -> do
          -- A cancel of the build does not come between the move or the
          -- removal of a file and the call that tells the driver of it.
          mask_ (install facts root job files >>= wrote)
          -- The status takes 68 columns, the width that it has in the line
          -- of a module in the build log.  The label is not padded to the
          -- longest module label, since the build has no plan yet, and the
          -- line is not fitted to the width of the terminal, which only the
          -- CLI knows.
          driverLog driver (printf "   %s  %-68s  %7.3f s" (BuildSpec.derivedLabel (jobEntry job))
                                   (abbreviate 68 (derivedStatus (jobEntry job) (length files))) seconds)
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

-- | What the line of a run says: the function, and the number of modules
-- that it returned, unless the entry has one name, a module name.
derivedStatus :: BuildSpec.DerivedEntry -> Int -> String
derivedStatus entry count
  | namesOneModule entry = "Derived by " ++ function
  | count == 1           = "Derived 1 module by " ++ function
  | otherwise            = "Derived " ++ show count ++ " modules by " ++ function
  where function = BuildSpec.derivedFunction entry

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

-- | The name of the module whose source file is 'file' under the source
-- directory 'dir', dotted as an import statement writes it.
moduleOfFile :: FilePath -> FilePath -> String
moduleOfFile dir file = intercalate "." (splitDirectories (dropExtension (makeRelative dir file)))

-- | Whether an entry names a file under the derived source directory of
-- the project at 'root': the file is the source file of a module that the
-- entry names.  A file whose path is not the one of a source file of a
-- module, such as a.b/c.act or notes.txt, is the file of no module.
namesFile :: FilePath -> BuildSpec.DerivedEntry -> FilePath -> Bool
namesFile root entry file = derivedSourceFile root name == file && BuildSpec.derivedCovers entry name
  where name = moduleOfFile (projectDerivedSrcDir root) file

-- | Remove the records of entries that are not in the table, and the files
-- under the derived source directory that no entry names.  A --tydb copy
-- stays: a build with --tydb writes it beside the source file of a module,
-- also of a derived module (isInterfaceCopy), and it goes when that source
-- file is removed (removeBelow).  Returns the removed files, and the source
-- files that stay.
removeOrphans :: FilePath -> [BuildSpec.DerivedEntry] -> IO ([FilePath], [FilePath])
removeOrphans root entries = do
    exists <- doesDirectoryExist dir
    stored <- if exists then filter ((== ".json") . takeExtension) <$> listDirectory dir else return []
    mapM_ (removeFile . (dir </>)) (filter ((`notElem` map BuildSpec.derivedFileLabel entries) . dropExtension) stored)
    (kept, rest) <- partition (\file -> any (\entry -> namesFile root entry file) entries) <$> derivedFiles root
    let orphans = filter (not . isInterfaceCopy) rest
    mapM_ (removeBelow (projectDerivedSrcDir root)) orphans
    return (orphans, kept)
  where dir = projectDerivedDir root

-- | Remove a file below the directory 'top', with the --tydb copy of its
-- module when it is a source file, and then each directory between the
-- file and 'top' that holds nothing.  On a file system that ignores the
-- case of letters, a directory that stayed would keep its name.  A source
-- file that is written later to a directory whose name differs from that
-- name only in the case of letters would then be in the directory that
-- stayed, and its path would give the name of a module that no entry
-- names, or the label of a runner that no entry has.
removeBelow :: FilePath -> FilePath -> IO ()
removeBelow top file = do
    removeFile file
    when (isActFile file) $ removePathForcibly (file -<.> InterfaceFiles.interfaceExt)
    prune (takeDirectory (makeRelative top file))
  where
    -- 'dir' is relative to 'top', which is "."
    prune dir = unless (dir == ".") $ do
      left <- listDirectory (top </> dir)
      when (null left) $ do
        removeDirectory (top </> dir)
        prune (takeDirectory dir)

-- | A module that an entry names with a module name has no source file
-- under src, and src has no source file below a name that ends with .*.
checkNoSource :: FilePath -> BuildSpec.DerivedEntry -> IO ()
checkNoSource root entry = do
    forM_ (BuildSpec.derivedModules entry) $ \name -> do
      let file = srcPath name <.> "act"
      exists <- doesFileExist file
      when exists $
        throwProjectError ("The module " ++ name ++ " is a derived module in " ++ projectBuildAct root
                           ++ " and has the source file " ++ file ++ ".\nTake the module out of derived_modules or remove the file.")
    forM_ (BuildSpec.derivedBelow entry) $ \name -> do
      let dir = srcPath name
      exists <- doesDirectoryExist dir
      files  <- if exists then sort . filter isActFile <$> getFilesRecursive dir else return []
      forM_ (take 1 files) $ \file ->
        throwProjectError ("Every module below " ++ name ++ " is a derived module in " ++ projectBuildAct root ++ ", which names " ++ name ++ ".*"
                           ++ ", and the project has the source file " ++ file ++ ".\nMove the file out of " ++ dir
                           ++ ", or write names in place of " ++ name ++ ".* that leave out the module "
                           ++ moduleOfFile (projectSrcDir root) file ++ ".")
  where srcPath name = joinPath (projectSrcDir root : A.dottedNames name)

-- | No name of an entry has the name of a dependency of the project as the
-- first name of its dotted name.  Such a name names modules of the
-- dependency.
checkNoDependency :: FilePath -> [String] -> BuildSpec.DerivedEntry -> IO ()
checkNoDependency root deps entry = do
    forM_ (BuildSpec.derivedModules entry) $ \name -> forM_ (dependencyOf name) $ \dep ->
      throwProjectError ("The module " ++ name ++ " is a derived module in " ++ projectBuildAct root
                         ++ ", and " ++ dep ++ " is a dependency of the project.\n"
                         ++ "The name " ++ name ++ " names a module of the dependency " ++ dep
                         ++ ". Give the derived module another name.")
    forM_ (BuildSpec.derivedBelow entry) $ \name -> forM_ (dependencyOf name) $ \dep ->
      throwProjectError ("Every module below " ++ name ++ " is a derived module in " ++ projectBuildAct root ++ ", which names " ++ name ++ ".*"
                         ++ ", and " ++ dep ++ " is a dependency of the project.\n"
                         ++ "The name " ++ name ++ ".* stands for modules of the dependency " ++ dep
                         ++ ". Give the derived modules another name.")
  where dependencyOf name = filter (`elem` deps) (take 1 (A.dottedNames name))


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
          names what = "The input path " ++ BuildSpec.buildOptionString input ++ " of the entry " ++ entryText root entry ++ " names " ++ what ++ " in " ++ root
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
    refuse why  = throwProjectError ("The entry " ++ entryText root entry ++ " is not brought up to date: " ++ why ++ ".")

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
lastRun :: FilePath -> BuildSpec.DerivedEntry -> IO (Maybe Record, Known)
lastRun root entry = do
    stat   <- try (readSourceFileMeta file) :: IO (Either IOException InterfaceFiles.SourceFileMeta)
    stored <- readRecord file
    return (stored, case (stored, stat) of
      (Just r, Right meta) -> M.fromList [ (path, Recorded recorded sha (InterfaceFiles.sfmMTimeNs meta))
                                         | (path, sha) <- recInputs r ++ recOutputs r
                                         , Just recorded <- [M.lookup path (recStats r)] ]
      _                    -> M.empty)
  where file = recordFile root entry

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
  | any derivedOwn [written, moduleLabel paths (A.modName path)]
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
    derivedOwn m = any (`BuildSpec.derivedCovers` m) (BuildSpec.derived_modules (projBuildSpec ctx))
    undeclared = [ BuildSpec.specName (projBuildSpec p) | p <- M.elems projects, projRoot p `notElem` projRoot ctx : map snd (projDeps ctx) ]
    sourceOf mn modules = lookup mn [ (m, file) | (file, m) <- modules ]
    refuse why = throwProjectError (functionText (projRoot ctx) entry ++ " names the module " ++ written ++ why)

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
          throwProjectError (functionText root entry ++ " imports a derived module of its own project: "
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
        when (result == One && not (namesOneModule entry)) $
          bad ("it returns str, the source of one module, and its entry names " ++ BuildSpec.derivedNamesText (BuildSpec.derivedNames entry)
               ++ ".\nAn entry whose function returns str has one name, a module name. "
               ++ "A function for several modules returns dict[str, str], from module name to source.")
        return (result, Hashing.hashHex (InterfaceFiles.nhImplHash h))
      _ -> bad ("the module " ++ written ++ " does not define " ++ A.nstr (fnName function) ++ ".")
  where
    written = intercalate "." (init (A.dottedNames (BuildSpec.derivedFunction entry)))
    dirs    = case fnSource function of
                Just _  -> searchPath paths
                Nothing -> C.searchpath opts ++ systemTypePaths (sysPath paths)
    bad msg = throwProjectError (functionText root entry ++ " cannot be used: " ++ msg)

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

-- | The file of the record of an entry of the project at 'root'.
recordFile :: FilePath -> BuildSpec.DerivedEntry -> FilePath
recordFile root entry = projectDerivedDir root </> BuildSpec.derivedFileLabel entry <.> "json"

-- | The record in a file, Nothing when the file is missing or unreadable.
readRecord :: FilePath -> IO (Maybe Record)
readRecord file = do
    content <- try (B.readFile file) :: IO (Either IOException B.ByteString)
    return (either (const Nothing) Aeson.decodeStrict' content)

-- | Write a record in place of the one that its entry has.
writeRecord :: FilePath -> Record -> IO ()
writeRecord root record = do
    BL.writeFile (file ++ ".tmp") (Aeson.encode record)
    renameFile (file ++ ".tmp") file
  where file = recordFile root (recEntry record)

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
-- found is reported under --verbose.  The files of an entry under
-- out/derived/src are as its last run left them when the source files that
-- the entry names (jobPresent) are the files that the record lists, and
-- each has the recorded content.  When the entry is up to date and a file
-- was read for its hash, the record is written again with the stats of the
-- files, so that the next build does not read them.
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
        | Set.fromList (map fst (recOutputs r)) /= jobPresent job
                                              -> return (Left notAsLeft)
        | otherwise                           -> do
            outputs <- mapM current (recOutputs r)
            return (maybe (Left notAsLeft) (Right . (,) r) (sequence outputs))
    case verdict of
      Left reason -> do
        info ("out of date, " ++ reason)
        return (Just reason)
      Right (r, outputs) -> do
        let files = jobInputs job ++ outputs
            nread = length (filter hashedRead files)
        when (nread > 0) $ writeRecord root r { recStats = statsOf files }
        info ("up to date, " ++ show nread ++ " of " ++ show (length files) ++ " files read")
        return Nothing
  where
    wanted    = jobWanted job
    notAsLeft = "its files under out/derived/src are not as its last run left them"
    info msg  = driverInfo driver ("Entry " ++ entryText root (jobEntry job) ++ ": " ++ msg)
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
-- files and the arguments of the entry.  The runner imports the two
-- modules under names of its own, so that no module name of a project is
-- the name of something else in the runner.
runnerSource :: Job -> String
runnerSource job = unlines
    [ "# Written by acton build.  Calls the function of the entry " ++ BuildSpec.derivedLabel (jobEntry job) ++ " of derived_modules."
    , "import std.build.derive as runner"
    , "import " ++ fnImport (jobFunction job) ++ " as fn"
    , ""
    , "actor main(env: Env):"
    , "    run = runner." ++ show (jobResult job) ++ "(env, lambda inputs: " ++ functionCall ("fn." ++ A.nstr (fnName (jobFunction job))) job ++ ")"
    ]

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
-- of the project at 'root' whose label, as 'BuildSpec.derivedFileLabel'
-- gives it, no entry has, and each directory above the source that then
-- holds nothing.  When the runner project has a source under another
-- directory than 'package', the whole runner project is removed: that
-- directory can have the name of a project that the runner project
-- depends on, and a build refuses such a source.
removeRunnerOrphans :: FilePath -> String -> [BuildSpec.DerivedEntry] -> IO ()
removeRunnerOrphans root package entries = do
    exists  <- doesDirectoryExist src
    sources <- if exists then map (makeRelative src) . filter isActFile <$> getFilesRecursive src else return []
    if any ((/= [package]) . take 1 . splitDirectories) sources
      then removePathForcibly dir
      else forM_ sources $ \file -> do
             let label = intercalate "." (drop 1 (splitDirectories (dropExtension file)))
             unless (label `elem` map BuildSpec.derivedFileLabel entries) $ do
               removeBelow (src </> package) (src </> file)
               mapM_ removePathForcibly [dir </> label <.> "built", projectBinDir dir </> (package ++ "." ++ label)]
  where
    dir = runnerDir root
    src = dir </> "src"

-- | Run the runner of a job, and return the modules it wrote to the
-- temporary directory: for each, the source file that the module gets
-- under out/derived/src, relative to the project root, and the file that
-- the runner wrote.  They hold every module that the entry names with a
-- module name, and no module that the entry does not name, or the run is
-- an error.  The runner is built first, unless the runner in place was
-- built from the same source, the same implementation hash of the function
-- and the same compiler version, for example when only an input file of the
-- entry changed.
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
        throwProjectError ("The entry " ++ entryText root entry ++ " is not brought up to date: "
                           ++ "the program that calls its function was not built.\nIt gives the call\n  "
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
      throwProjectError (functionText root entry ++ " failed:\n" ++ stdout ++ stderr)
    files <- filter isActFile <$> getFilesRecursive out
    let modules  = sort [ (moduleOfFile out file, file) | file <- files ]
        returned = Set.fromList (map fst modules)
        extra    = Set.filter (not . BuildSpec.derivedCovers entry) returned
        missing  = BuildSpec.derivedModules entry `Set.difference` returned
        listed title names = [ title ++ ": " ++ intercalate ", " (Set.toList names) | not (Set.null names) ]
    unless (Set.null extra && Set.null missing) $
      throwProjectError (intercalate "\n" $
        [ functionText root entry ++ " returned other modules than the entry names." ]
        ++ listed "Returned and not named" extra
        ++ listed "Named and not returned" missing
        ++ [ m ++ ".* stands for every module below " ++ m ++ ", and not for the module " ++ m
             ++ ". To name both, write " ++ m ++ " and " ++ m ++ ".* in the entry."
           | m <- Set.toList extra, m `elem` BuildSpec.derivedBelow entry ]
        ++ [ "The function returns every module that its entry names with a module name, and no module that its entry does not name. "
             ++ "Change the names of the entry or the function." ])
    return [ (moduleFile root name, file) | (name, file) <- modules ]
  where
    root      = projRoot ctx
    entry     = jobEntry job
    fileLabel = BuildSpec.derivedFileLabel entry
    wanted    = jobWanted job
    stamp     = TE.encodeUtf8 (T.pack (unlines [recCompiler wanted, recImpl wanted] ++ runnerSource job))
    stampFile = dir </> fileLabel <.> "built"
    reachable = reachableProjects projects ctx
    package   = runnerPackage projects ctx
    name      = unusedName (M.keys reachable) "acton_derive"
    dir       = runnerDir root
    source    = joinPath (dir : "src" : package : A.dottedNames fileLabel) <.> "act"
    binary    = projectBinDir dir </> (package ++ "." ++ fileLabel)
    out       = tmpDir root </> fileLabel </> "src"
    manifest  = tmpDir root </> fileLabel </> "manifest.json"
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
-- file is removed or moved: the function may have made the modules from
-- other content than its descriptions are of.  Then each source file that
-- the entry names and the run did not return is removed (jobPresent): the
-- function returned its module in an earlier run, or the file is there from
-- something else.  It is removed before a module is moved, since on a file
-- system that ignores the case of letters it is also the file of a returned
-- module whose name differs from the name of its module only in the case
-- of letters.  A module whose file holds the same source already is left
-- alone, so the modules that import it are not compiled again.  A file that
-- is moved into place is read once more for its stat: the stat is taken
-- before that read, and it is recorded when the read gives the SHA-256 of
-- the source that the function returned.  Returns the files written and
-- the files removed.
install :: FileFacts -> FilePath -> Job -> [(FilePath, FilePath)] -> IO [FilePath]
install facts root job files = do
    mapM_ (inputFile facts (jobKnown job) root . hashedPath) (jobInputs job)
    mapM_ (removeBelow (projectDerivedSrcDir root)) left
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
    writeRecord root (jobWanted job)
      { recOutputs = [ output | (output, _, _) <- done ]
      , recStats   = M.unions (statsOf (jobInputs job) : [ stats | (_, stats, _) <- done ])
      }
    return (concat [ written | (_, _, written) <- done ] ++ left)
  where left = map (root </>) (Set.toList (jobPresent job `Set.difference` Set.fromList (map fst files)))
