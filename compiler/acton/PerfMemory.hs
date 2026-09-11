{-# LANGUAGE ForeignFunctionInterface #-}

module PerfMemory (MemoryStatus(..), readMemoryStatus) where

import Data.Word (Word64)
import Foreign (Ptr, alloca, allocaBytes, peek)
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CInt(..), CSize(..))

data MemoryStatus = MemoryStatus
  { memoryTotal :: Integer
  , memoryAvailable :: Integer
  , memoryProcess :: Integer
  } deriving (Eq, Show)

foreign import ccall safe "acton_perf_memory"
  c_perfMemory :: CInt -> Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> CString -> CSize -> IO CInt

-- | Current byte counts for the host and an optional process. Linux host values
-- respect visible cgroup limits; macOS headroom is a conservative VM estimate.
-- Nothing leaves memoryProcess at zero. A failed read, including an exited PID,
-- is an error: the caller must not continue a resource-limited run unguarded.
-- Sampling is best effort and cannot guarantee that an allocation avoids OOM.
readMemoryStatus :: Maybe Int -> IO (Either String MemoryStatus)
readMemoryStatus process
  | Just pid <- process, pid <= 0 || toInteger pid > toInteger (maxBound :: CInt) =
      return (Left "Invalid process ID for memory observation")
  | otherwise = alloca $ \total -> alloca $ \available -> alloca $ \used ->
      allocaBytes 512 $ \message -> do
        status <- c_perfMemory (maybe 0 fromIntegral process) total available used message 512
        if status /= 0
          then Left <$> peekCString message
          else do
            result <- MemoryStatus <$> (toInteger <$> peek total)
                                   <*> (toInteger <$> peek available)
                                   <*> (toInteger <$> peek used)
            return (Right result)
