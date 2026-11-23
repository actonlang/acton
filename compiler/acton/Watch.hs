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
-}
module Watch
  ( WatchTrigger(..)
  , srcWatchTrigger
  , fileWatchTrigger
  , dispatchWatchTrigger
  , FileStamp(..)
  , readFileStamp
  , shouldTriggerFile
  , recordStamps
  ) where

import Control.Exception (IOException, try)
import Control.Monad (forM_, when)
import Data.IORef
import qualified Data.Map as M
import Data.Time.Clock (UTCTime)
import System.Directory (doesDirectoryExist, doesFileExist, getFileSize, getModificationTime, listDirectory, pathIsSymbolicLink)
import System.FilePath (normalise, (</>))
import qualified System.FSNotify as FS

import Acton.ExtSource (extSourceFile, extSourceOwner)
import Acton.Project (isActFile, isInterfaceCopy, isScratchFile)

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
                                          = if structural then Just WatchFull else Nothing
  | FS.ModifiedAttributes{} <- ev         = Nothing
  | isScratchFile path                    = Nothing
  | isActFile path                        = Just (if structural then WatchSave path Nothing else WatchSave path (Just path))
  | Just owner <- extSourceOwner path     = Just (WatchSave path (Just owner))
  | otherwise                             = Just (WatchSave path Nothing)
  where
    path = FS.eventPath ev
    structural = case ev of
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
