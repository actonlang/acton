{-# LANGUAGE CApiFFI #-}

module TerminalSize
  ( TermSize
  , initTermSize
  , termSizeCurrent
  , termSizeSync
  , termSizeRead
  , termVisibleLength
  , termFitAnsiRight
  , termFitPlainLeft
  , termFitPlainRight
  , termRenderedRows
  , termRenderedRowsTotal
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (when)
import Data.IORef
import Data.List (isInfixOf)
import Foreign
import Foreign.C.Types
import System.Environment (lookupEnv)
import System.Posix.IO (stdOutput)
import System.Posix.Signals (Handler(..), installHandler)

data TermSize = TermSize
  { tsEnabled :: Bool
  , tsRowsRef :: IORef Int
  , tsColsRef :: IORef Int
  , tsDirtyRef :: IORef Bool
  }

data WinSize = WinSize
  { wsRows :: CUShort
  , wsCols :: CUShort
  , wsXPixel :: CUShort
  , wsYPixel :: CUShort
  }

instance Storable WinSize where
    sizeOf _ = 8
    alignment _ = alignment (undefined :: CUShort)
    peek ptr =
      WinSize
        <$> peekByteOff ptr 0
        <*> peekByteOff ptr 2
        <*> peekByteOff ptr 4
        <*> peekByteOff ptr 6
    poke ptr (WinSize rows cols xp yp) = do
      pokeByteOff ptr 0 rows
      pokeByteOff ptr 2 cols
      pokeByteOff ptr 4 xp
      pokeByteOff ptr 6 yp

foreign import capi unsafe "sys/ioctl.h value TIOCGWINSZ"
  c_TIOCGWINSZ :: CULong

foreign import capi unsafe "signal.h value SIGWINCH"
  c_SIGWINCH :: CInt

foreign import capi unsafe "sys/ioctl.h ioctl"
  c_ioctlWinsize :: CInt -> CULong -> Ptr WinSize -> IO CInt

defaultRows :: Int
defaultRows = 24

defaultCols :: Int
defaultCols = 80

initTermSize :: Bool -> IO TermSize
initTermSize enabled = do
    rows <- envOrDefault "LINES" defaultRows
    cols <- envOrDefault "COLUMNS" defaultCols
    rowsRef <- newIORef rows
    colsRef <- newIORef cols
    dirtyRef <- newIORef enabled
    let ts = TermSize
          { tsEnabled = enabled
          , tsRowsRef = rowsRef
          , tsColsRef = colsRef
          , tsDirtyRef = dirtyRef
          }
    when enabled $ do
      _ <- try (installHandler (fromIntegral c_SIGWINCH) (Catch (writeIORef dirtyRef True)) Nothing)
             :: IO (Either SomeException Handler)
      _ <- termSizeSync ts
      return ()
    return ts

termSizeSync :: TermSize -> IO (Bool, Int, Int)
termSizeSync ts
  | not (tsEnabled ts) = do
      (rows, cols) <- termSizeCurrent ts
      return (False, rows, cols)
  | otherwise = do
      dirty <- atomicModifyIORef' (tsDirtyRef ts) (\cur -> (False, cur))
      if not dirty
        then do
          rows <- readIORef (tsRowsRef ts)
          cols <- readIORef (tsColsRef ts)
          return (False, rows, cols)
        else do
          prevRows <- readIORef (tsRowsRef ts)
          prevCols <- readIORef (tsColsRef ts)
          msize <- queryTermSize
          case msize of
            Just (rows, cols) -> do
              writeIORef (tsRowsRef ts) rows
              writeIORef (tsColsRef ts) cols
              return (rows /= prevRows || cols /= prevCols, rows, cols)
            Nothing -> return (False, prevRows, prevCols)

termSizeCurrent :: TermSize -> IO (Int, Int)
termSizeCurrent ts = do
    rows <- readIORef (tsRowsRef ts)
    cols <- readIORef (tsColsRef ts)
    return (rows, cols)

termSizeRead :: TermSize -> IO (Int, Int)
termSizeRead ts = do
    (_, rows, cols) <- termSizeSync ts
    return (rows, cols)

termVisibleLength :: String -> Int
termVisibleLength = go 0
  where
    go acc [] = acc
    go acc ('\ESC':'[':xs) = go acc (dropAnsi xs)
    go acc (_:xs) = go (acc + 1) xs

termFitAnsiRight :: Int -> String -> String
termFitAnsiRight width s
  | width <= 0 = ""
  | termVisibleLength s <= width = s
  | otherwise =
      let trimmed = go width s
      in if "\ESC[" `isInfixOf` trimmed then trimmed ++ "\ESC[0m" else trimmed
  where
    go _ [] = []
    go n _
      | n <= 0 = []
    go n ('\ESC':'[':xs) =
      let (esc, rest) = spanAnsi xs
      in '\ESC' : '[' : esc ++ go n rest
    go n (x:xs) = x : go (n - 1) xs

termFitPlainRight :: Int -> String -> String
termFitPlainRight width s
  | width <= 0 = ""
  | length s <= width = s
  | otherwise = take width s

termFitPlainLeft :: Int -> String -> String
termFitPlainLeft width s
  | width <= 0 = ""
  | length s <= width = s
  | otherwise = drop (length s - width) s

termRenderedRows :: Int -> String -> Int
termRenderedRows width s
  | width <= 0 = 1
  | otherwise =
      let visible = max 0 (termVisibleLength s)
      in max 1 ((visible + width - 1) `div` width)

termRenderedRowsTotal :: Int -> [String] -> Int
termRenderedRowsTotal width = sum . map (termRenderedRows width)

queryTermSize :: IO (Maybe (Int, Int))
queryTermSize =
    alloca $ \ptr -> do
      rc <- c_ioctlWinsize (fromIntegral stdOutput) c_TIOCGWINSZ ptr
      if rc == -1
        then return Nothing
        else do
          WinSize rows cols _ _ <- peek ptr
          let rows' = fromIntegral rows
              cols' = fromIntegral cols
          if rows' > 0 && cols' > 0
            then return (Just (rows', cols'))
            else return Nothing

envOrDefault :: String -> Int -> IO Int
envOrDefault name fallback = do
    raw <- lookupEnv name
    case raw >>= readMaybeInt of
      Just n | n > 0 -> return n
      _ -> return fallback

readMaybeInt :: String -> Maybe Int
readMaybeInt s =
    case reads s of
      [(n, "")] -> Just n
      _ -> Nothing

dropAnsi :: String -> String
dropAnsi [] = []
dropAnsi (c:cs)
  | c >= '@' && c <= '~' = cs
  | otherwise = dropAnsi cs

spanAnsi :: String -> (String, String)
spanAnsi [] = ([], [])
spanAnsi (c:cs)
  | c >= '@' && c <= '~' = ([c], cs)
  | otherwise =
      let (prefix, rest) = spanAnsi cs
      in (c : prefix, rest)
