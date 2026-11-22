{-|
Which file system events start a build under acton build --watch and acton
test --watch.

A project watch looks at its src directory.  A saved .act names the module
that changed.  A saved .ext.c names the module whose NotImplemented bodies it
implements, when that module's source exists, and any other saved file, such
as a header that an .ext.c includes, asks for a full build: only the build
can tell which module includes it.  So does an added or removed .act.  A
save counts only when the file's stamp, its modification time and size,
changed since the last one the watch saw, and the watch records the stamps
of the files there when it starts: the system may report a write from just
before the watch started after the first build has begun, and that build
already has it.  A directory event asks for a full build at once.  The watch skips
the files that editors and tools leave next to the files they save and the
--tydb copies that a build writes next to the sources, so that a build cannot
set itself off.

A single-file watch looks at the file and its .ext.c.  Its directory may hold
anything, including the binary that the build copies there.

An embedded file can lie anywhere in a project, and only a build tells which
files its modules embed.  So after each build, also one that failed, both
watches look at the paths that the build looked at for embed (embeddedWatch).
They watch the directory of each path and each directory above it, up to the
directory that the path counts from, as far as they exist: the watch of a
directory tells of the files and directories in it that appear, change or
go, and not every system lets it tell that the directory itself is removed.
On macOS one watch tells of a whole tree, and a process gets events from
about 500 watches, so there the watch of a directory stands for the watches
of the directories below it (Embed.watchDirectories).  A save at such a path
asks for a full build when the stamp of the file changed: only the build can
tell which modules embed the file.  So does a directory on the way to such a
path that appears or goes, and any event of a file at a path where the build
found no file to read: embed can refuse a file for its name or its
permissions, which its stamp does not show.  A build looks at a file before
the watch of its directory starts, so the watch then compares each file with
what the build saw of it.  A build looks at the input files of its derived
modules as it looks at embedded files, so these paths hold them too.

A project watch also looks at the input paths of the derived modules of the
project and of its local dependencies, which their Build.act files name.  A
build looks at an input file as it looks at an embedded file, so the watch
of the embedded files takes the input paths too: one watch with one set of
stamps looks at both kinds of file, and an event has one trigger.  It
watches the directory of each input path, and, where a watch tells of one
directory, each directory at or below an input path that the path stands
for (inputDirectoriesAt).  A save of a file that an input path names or
stands for asks for a full build when the stamp of the file changed, and so
does a directory there that appears or goes: only the build tells which
entries have the file as an input file.  A file below an input directory
that the path does not stand for, such as one whose name starts with a dot,
is no input file, and its events ask for nothing (Derive.standsFor).  The
watch of a directory is not stopped when no Build.act names a path there
any more: its events then ask for nothing.
-}
module Watch
  ( WatchTrigger(..)
  , srcWatchTrigger
  , fileWatchTrigger
  , embedWatchTrigger
  , inputWatchTrigger
  , inputDirectoriesAt
  , dispatchWatchTrigger
  , FileStamp(..)
  , readFileStamp
  , statStamp
  , shouldTriggerFile
  , changedSinceBuild
  , recordStamps
  , embeddedWatch
  ) where

import Control.Applicative ((<|>))
import Control.Exception (IOException, mask, onException, try)
import Control.Monad (filterM, forM_, unless, when)
import Data.IORef
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import System.Directory (doesDirectoryExist, doesFileExist, getFileSize, getModificationTime, listDirectory, pathIsSymbolicLink)
import System.FilePath (dropTrailingPathSeparator, normalise, (</>))
import qualified System.FSNotify as FS

import Acton.Derive (standsFor)
import qualified Acton.Embed as Embed
import Acton.ExtSource (extSourceFile, extSourceOwner)
import Acton.Project (isActFile, isInterfaceCopy, isScratchFile)
import InterfaceFiles (SourceFileMeta(..))

-- | What an event asks of the watch: a full build, or a check of the saved
-- file, which, when the file changed, builds the given module source as
-- changed, or everything when there is none.
data WatchTrigger
  = WatchFull
  | WatchSave FilePath (Maybe FilePath)
  deriving (Eq, Show)

-- | The trigger of an event under a project's src directory, if any.
srcWatchTrigger :: FS.Event -> Maybe WatchTrigger
srcWatchTrigger ev
  | isInterfaceCopy path                  = Nothing
  | FS.IsDirectory <- FS.eventIsDirectory ev
                                          = if structural ev then Just WatchFull else Nothing
  | FS.ModifiedAttributes{} <- ev         = Nothing
  | isScratchFile path                    = Nothing
  | isActFile path                        = Just (if structural ev then WatchSave path Nothing else WatchSave path (Just path))
  | Just owner <- extSourceOwner path     = Just (WatchSave path (Just owner))
  | otherwise                             = Just (WatchSave path Nothing)
  where path = FS.eventPath ev

-- | Whether an event tells that a file or a directory appeared or went.
structural :: FS.Event -> Bool
structural ev = case ev of
    FS.Added{}                   -> True
    FS.Removed{}                 -> True
    FS.WatchedDirectoryRemoved{} -> True
    FS.Unknown{}                 -> True
    _                            -> False

-- | The trigger of an event for a single-file watch of 'target', if any:
-- the file itself or its .ext.c changes the file's module.
fileWatchTrigger :: FilePath -> FS.Event -> Maybe WatchTrigger
fileWatchTrigger target ev
  | FS.ModifiedAttributes{} <- ev               = Nothing
  | path == target || path == extSourceFile target = Just (WatchSave path (Just target))
  | otherwise                                   = Nothing
  where path = normalise (FS.eventPath ev)

-- | The trigger of an event in a directory that is watched for embedded
-- files, if any, given what a build saw at the paths at which an event
-- tells of a change to an embedded file (Compile.embeddedPaths): a full
-- build when a directory appears or goes at such a path, and a check of the
-- file at the path for an event of a file that the build read.  Where the
-- build found no file to read, any event of a file asks for a full build:
-- no stamp tells whether embed can take the file that is there.  An event
-- can name a directory with a separator at its end.
embedWatchTrigger :: M.Map FilePath (Maybe SourceFileMeta) -> FS.Event -> Maybe WatchTrigger
embedWatchTrigger seen ev
  | M.notMember path seen                 = Nothing
  | FS.IsDirectory <- FS.eventIsDirectory ev
                                          = if structural ev then Just WatchFull else Nothing
  | Just Nothing <- M.lookup path seen    = Just WatchFull
  | FS.ModifiedAttributes{} <- ev         = Nothing
  | otherwise                             = Just (WatchSave path Nothing)
  where path = dropTrailingPathSeparator (FS.eventPath ev)

-- | The trigger of an event for the input paths of the derived modules, if
-- any: a check of a file that one of the paths names or stands for
-- (Derive.standsFor), which asks for a full build when the file changed,
-- since only the build tells which entries have the file as an input file.
-- A directory that one of the paths names or stands for asks for a full
-- build when it appears or goes.  An event can name a directory with a
-- separator at its end.
inputWatchTrigger :: [FilePath] -> FS.Event -> Maybe WatchTrigger
inputWatchTrigger inputs ev
  | not (any (`standsFor` path) inputs)   = Nothing
  | FS.IsDirectory <- FS.eventIsDirectory ev
                                          = if structural ev then Just WatchFull else Nothing
  | FS.ModifiedAttributes{} <- ev         = Nothing
  | otherwise                             = Just (WatchSave path Nothing)
  where path = dropTrailingPathSeparator (FS.eventPath ev)

-- | The directories at or below a path that one of the input paths names
-- or stands for (Derive.standsFor), none when the path names no directory.
-- Where a watch tells of one directory, these are the directories whose
-- watches tell of the files at or below the path that the input paths
-- stand for.  Linked directories below the path are not followed, as for
-- the stamps (recordStamps).
inputDirectoriesAt :: [FilePath] -> FilePath -> IO [FilePath]
inputDirectoriesAt inputs path = do
    isDir <- doesDirectoryExist path
    if isDir && any (`standsFor` path) inputs then (path :) <$> below path else return []
  where
    below dir = do
      names <- either (const []) id <$> (try (listDirectory dir) :: IO (Either IOException [FilePath]))
      concat <$> mapM (entry . (dir </>)) names
    entry sub = do
      isLink <- either (const True) id <$> (try (pathIsSymbolicLink sub) :: IO (Either IOException Bool))
      if isLink then return [] else inputDirectoriesAt inputs sub

-- | Act on a trigger: a full build at once, or a build when the saved file
-- changed, of the module source it names if that exists.  Otherwise, as for
-- an .ext.c that only other C code includes, the build is a full one.
dispatchWatchTrigger :: (FilePath -> IO Bool) -> (Maybe FilePath -> IO ()) -> WatchTrigger -> IO ()
dispatchWatchTrigger _ notify WatchFull                     = notify Nothing
dispatchWatchTrigger changed notify (WatchSave saved source) = do
    isChanged <- changed saved
    when isChanged $ do
      exists <- maybe (return False) doesFileExist source
      notify (if exists then source else Nothing)

-- | What tells one version of a file from the next: its modification time
-- and size.
data FileStamp = FileStamp UTCTime Integer deriving (Eq, Show)

-- | A file's stamp, Nothing when it does not exist or cannot be read.
readFileStamp :: FilePath -> IO (Maybe FileStamp)
readFileStamp path = do
    res <- try (do
      exists <- doesFileExist path
      if not exists
        then return Nothing
        else do
          stampTime <- getModificationTime path
          stampSize <- getFileSize path
          return (Just (FileStamp stampTime stampSize))
      ) :: IO (Either IOException (Maybe FileStamp))
    case res of
      Left _      -> return Nothing
      Right stamp -> return stamp

-- | The stamp of a file with the given stat, which is how a plan records
-- what it saw of a file.
statStamp :: SourceFileMeta -> FileStamp
statStamp meta = FileStamp (posixSecondsToUTCTime (fromInteger (sfmMTimeNs meta) / 1000000000)) (sfmSize meta)

-- | Record the stamp of a file, or the stamps of the files under a
-- directory, that the watch reacts to, so that a later event for a file
-- that has not changed since is ignored.  Linked directories under the
-- directory are not followed.
recordStamps :: IORef (M.Map FilePath FileStamp) -> FilePath -> IO ()
recordStamps stampsRef path = do
    isDir <- doesDirectoryExist path
    if isDir then under path else record path
  where
    under dir = do
      names <- either (const []) id <$> (try (listDirectory dir) :: IO (Either IOException [FilePath]))
      forM_ names $ \name -> do
        let entry = dir </> name
        isSub  <- doesDirectoryExist entry
        isLink <- either (const True) id <$> (try (pathIsSymbolicLink entry) :: IO (Either IOException Bool))
        if isSub
          then when (not isLink && not (isInterfaceCopy entry)) (under entry)
          else record entry
    record file = do
      stamp <- readFileStamp file
      forM_ stamp $ \s -> atomicModifyIORef' stampsRef (\stamps -> (M.insert file s stamps, ()))

-- | Whether a file changed since the stamp recorded for it, recording its
-- new one.  A missing file always counts as changed.
shouldTriggerFile :: IORef (M.Map FilePath FileStamp) -> FilePath -> IO Bool
shouldTriggerFile stampsRef path = do
    mstamp <- readFileStamp path
    atomicModifyIORef' stampsRef $ \stamps ->
      case mstamp of
        Nothing -> (M.delete path stamps, True)
        Just stamp ->
          case M.lookup path stamps of
            Just old | old == stamp -> (stamps, False)
            _ -> (M.insert path stamp stamps, True)

-- | Whether the file at a path is another one than a build saw there, given
-- the stamp of the file that the build saw, Nothing for no file.  The stamp
-- of the file is recorded.  A file with the stamp that was recorded before
-- does not count: a build saw that file, or the watch asked for a build
-- when it recorded the stamp.  So a file that embed cannot take, which each
-- build sees as no file, asks for one build.
changedSinceBuild :: IORef (M.Map FilePath FileStamp) -> FilePath -> Maybe FileStamp -> IO Bool
changedSinceBuild stampsRef path seen = do
    now      <- readFileStamp path
    recorded <- atomicModifyIORef' stampsRef (\stamps -> (M.alter (const now) path stamps, M.lookup path stamps))
    return (now /= seen && all ((/= now) . Just) recorded)

-- | The watch of the files that the modules of the builds of a watch embed
-- and of the input files of their derived modules.  It gives two actions.
--
-- The first is to run after each build that ended, with what the build saw
-- at the paths it looked at for embed (Compile.embeddedPaths).  Events at
-- these paths count from then on (embedWatchTrigger), and the directories
-- that hold them (Embed.watchDirectories) are watched unless they are
-- already.  The build looked at its files before these watches could tell
-- of a change, so a file that is another one than the build saw asks for a
-- full build (changedSinceBuild).  A build cannot run the action itself: it
-- would stop itself when it asks for the next build.
--
-- The second takes the input paths of the derived modules, as the Build.act
-- files name them, with the links of each resolved, as an event names a
-- path.  Events at these paths and at the files that they stand for count
-- from then on (inputWatchTrigger).  The directory of each path is watched
-- unless it is already, and, where a watch tells of one directory, so is
-- each directory at or below a path that the path stands for
-- (inputDirectoriesAt).  There a directory that appears at or below an
-- input path is watched when its event arrives.  The action records no
-- stamp: a caller records the stamps of the files at the input paths where
-- a build follows (recordStamps), and after a build the first action
-- compares each file with what the build saw.  A stamp that is recorded
-- with no build to follow would hide a file that changed after the last
-- build looked at it.
--
-- An event has one trigger, that of an embedded file or else that of an
-- input path, and the given function acts on it.  A watched directory that
-- is removed is forgotten where the system tells of it, so that the
-- directory that takes its place is watched later.  A directory that
-- cannot be watched is named in a line of the watch and tried again the
-- next time an action asks for it.
--
-- The stamps are those of the caller, and they are not those of its watch
-- of src.  A file that this watch looks at can also be a file that the
-- watch of src reacts to, and that watch asks for a build of one module:
-- with one set of stamps for both, the one that sees a save first would
-- take the change from the other.  The watch has a manager of its own and
-- stops none of its watches: on Linux the watches of one manager share the
-- kernel watch of a directory, and one of them that stops takes it from
-- the others.
embeddedWatch :: IORef (M.Map FilePath FileStamp)                        -- the stamps to compare a file with
              -> ((FS.Event -> Maybe WatchTrigger) -> FS.Event -> IO ()) -- acts on an event, given the function to its trigger
              -> IO ()                                                   -- asks for a full build
              -> (String -> IO ())                                       -- writes a line of the watch
              -> IO (M.Map FilePath (Maybe SourceFileMeta) -> IO (), [FilePath] -> IO ())
embeddedWatch stampsRef onEvent build say = do
    mgr        <- FS.startManager
    seenRef    <- newIORef M.empty
    inputsRef  <- newIORef []
    watchedRef <- newIORef S.empty
    let forget dir = atomicModifyIORef' watchedRef (\watched -> (S.delete dir watched, ()))
        watch dir = (if Embed.watchesTrees then FS.watchTree else FS.watchDir) mgr dir (const True) $ \ev -> do
          case ev of
            FS.WatchedDirectoryRemoved{} -> forget dir
            _                            -> return ()
          seen   <- readIORef seenRef
          inputs <- readIORef inputsRef
          when (inputWatchTrigger inputs ev == Just WatchFull) $
            watchBelow inputs [dropTrailingPathSeparator (FS.eventPath ev)]
          onEvent (\e -> embedWatchTrigger seen e <|> inputWatchTrigger inputs e) ev
        -- Watch each of the directories that is not watched.  A directory
        -- counts as watched before its watch starts, so that two threads
        -- do not both start one, and is forgotten when the watch does not
        -- start.  That also holds when the thread is stopped meanwhile: a
        -- build that the next one cancels can run the second action.
        watchEach dirs = forM_ (S.toList dirs) $ \dir -> mask $ \restore -> do
          fresh <- atomicModifyIORef' watchedRef (\watched -> (S.insert dir watched, S.notMember dir watched))
          when fresh $ do
            started <- try (restore (watch dir)) `onException` forget dir
            case started of
              Left e  -> forget dir >> say (Embed.cannotBeWatched dir e)
              Right _ -> return ()
        -- Where one watch tells of a whole tree, the watch of the directory
        -- of an input path tells of everything at and below the path.
        watchBelow inputs paths = unless Embed.watchesTrees $
          watchEach . S.fromList . concat =<< mapM (inputDirectoriesAt inputs) paths
        lookAtEmbedded seen = do
          writeIORef seenRef seen
          watchEach =<< Embed.watchDirectories (M.keysSet seen)
          changed <- filterM (\(path, stat) -> changedSinceBuild stampsRef path (statStamp <$> stat)) (M.toList seen)
          unless (null changed) build
        -- A watched directory is known by its path, which has no separator
        -- at its end.
        watchInputs inputs = do
          let paths = map dropTrailingPathSeparator inputs
          writeIORef inputsRef paths
          watchEach =<< Embed.watchDirectories (S.fromList paths)
          watchBelow paths paths
    return (lookAtEmbedded, watchInputs)
