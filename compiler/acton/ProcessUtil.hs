-- Watch builds and test workers own separate process groups so cancellation
-- can stop descendants even after their parent exits or closes its pipes.
module ProcessUtil (stopProcessGroup) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, catch, mask_)
import Control.Monad
import System.Posix.Signals
import System.Posix.Types (ProcessID)
import System.Process
import System.Timeout

stopProcessGroup :: ProcessHandle -> Maybe ProcessID -> IO ()
stopProcessGroup process pid = mask_ $ do
    let signal sig = forM_ pid $ \group ->
          signalProcessGroup sig group `catch` ignoreIO
        alive = case pid of
          Nothing -> return False
          Just group -> (signalProcessGroup nullSignal group >> return True)
                          `catch` \err -> ignoreIO err >> return False
        wait = do
          -- Reap the leader before probing: a zombie also keeps its group alive.
          void (getProcessExitCode process)
          running <- alive
          when running (threadDelay 10000 >> wait)
    signal sigTERM
    stopped <- timeout 1000000 wait
    when (stopped == Nothing) (signal sigKILL)
    void (waitForProcess process)
  where
    ignoreIO :: IOException -> IO ()
    ignoreIO _ = return ()
