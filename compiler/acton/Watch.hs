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
what the build saw of it.
-}
module Watch
  ( WatchTrigger(..)
  , srcWatchTrigger
  , fileWatchTrigger
  , embedWatchTrigger
  , dispatchWatchTrigger
  , FileStamp(..)
  , readFileStamp
  , statStamp
  , shouldTriggerFile
  , changedSinceBuild
  , recordStamps
  , embeddedWatch
  ) where

import Control.Exception (IOException, try)
import Control.Monad (filterM, forM_, unless, when)
import Data.IORef
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import System.Directory (doesDirectoryExist, doesFileExist, getFileSize, getModificationTime, listDirectory, pathIsSymbolicLink)
import System.FilePath (dropTrailingPathSeparator, normalise, (</>))
import qualified System.FSNotify as FS

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

-- | Record the stamps of the files under a directory that the watch reacts
-- to, so that a later event for a file that has not changed since is
-- ignored.  Linked directories are not followed.
recordStamps :: IORef (M.Map FilePath FileStamp) -> FilePath -> IO ()
recordStamps stampsRef dir = do
    names <- either (const []) id <$> (try (listDirectory dir) :: IO (Either IOException [FilePath]))
    forM_ names $ \name -> do
      let path = dir </> name
      isDir  <- doesDirectoryExist path
      isLink <- either (const True) id <$> (try (pathIsSymbolicLink path) :: IO (Either IOException Bool))
      if isDir
        then when (not isLink && not (isInterfaceCopy path)) (recordStamps stampsRef path)
        else do
          stamp <- readFileStamp path
          forM_ stamp $ \s -> atomicModifyIORef' stampsRef (\stamps -> (M.insert path s stamps, ()))

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

-- | The watch of the files that the builds of a watch embed: the action to
-- run after each build that ended, with what the build saw at the paths it
-- looked at for embed (Compile.embeddedPaths).  Events at these paths count
-- from then on (embedWatchTrigger), and the directories that hold them
-- (Embed.watchDirectories) are watched unless they are already.  A watched
-- directory that is removed is forgotten where the system tells of it, so
-- that the directory that takes its place is watched after a later build.
-- The build looked at its files before these watches could tell of a
-- change, so a file that is another one than the build saw asks for a full
-- build (changedSinceBuild).  A build cannot run the action itself: it
-- would stop itself when it asks for the next build.
--
-- The watch has stamps of its own.  An embedded file can also be a file
-- that the watch of src reacts to, and that watch asks for a build of one
-- module: with one set of stamps for both, the one that sees a save first
-- would take the change from the other.  The watch also has a manager of
-- its own: on Linux the watches of one manager share the kernel watch of a
-- directory, and one of them that stops takes it from the others.  A
-- directory that cannot be watched is named in a line of the watch and
-- tried again after the next build.
embeddedWatch :: (IORef (M.Map FilePath FileStamp) -> (FS.Event -> Maybe WatchTrigger) -> FS.Event -> IO ())
                                                                         -- acts on an event, given the stamps to compare a saved file with and the function to its trigger
              -> IO ()                                                   -- asks for a full build
              -> (String -> IO ())                                       -- writes a line of the watch
              -> IO (M.Map FilePath (Maybe SourceFileMeta) -> IO ())
embeddedWatch onEvent build say = do
    mgr        <- FS.startManager
    stampsRef  <- newIORef M.empty
    seenRef    <- newIORef M.empty
    watchedRef <- newIORef S.empty
    let forget dir = atomicModifyIORef' watchedRef (\watched -> (S.delete dir watched, ()))
        watch dir = (if Embed.watchesTrees then FS.watchTree else FS.watchDir) mgr dir (const True) $ \ev -> do
          case ev of
            FS.WatchedDirectoryRemoved{} -> forget dir
            _                            -> return ()
          seen <- readIORef seenRef
          onEvent stampsRef (embedWatchTrigger seen) ev
    return $ \seen -> do
      writeIORef seenRef seen
      dirs    <- Embed.watchDirectories (M.keysSet seen)
      fresh   <- atomicModifyIORef' watchedRef (\watched -> (S.union watched dirs, dirs S.\\ watched))
      forM_ (S.toList fresh) $ \dir -> do
        started <- try (watch dir)
        case started of
          Left e  -> forget dir >> say (Embed.cannotBeWatched dir e)
          Right _ -> return ()
      changed <- filterM (\(path, stat) -> changedSinceBuild stampsRef path (statStamp <$> stat)) (M.toList seen)
      unless (null changed) build
