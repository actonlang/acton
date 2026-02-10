module ZigProgress
  ( ZigProgress(..)
  , ZigNode(..)
  , parseZigProgressMessages
  , zigProgressRatio
  , readZigProgressStream
  ) where

import Control.Exception (IOException, try)
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.List (foldl')
import qualified Data.Set as Set
import Data.Word (Word8, Word32)
import qualified System.Posix.IO.ByteString as PIOB
import System.Posix.Types (Fd)

data ZigNode = ZigNode
  { znCompleted :: Word32
  , znTotal :: Word32
  , znName :: String
  , znParent :: Maybe Int
  } deriving (Show)

data ZigProgress = ZigProgress
  { zpNodes :: [ZigNode]
  } deriving (Show)

readZigProgressStream :: Fd -> (ZigProgress -> IO ()) -> IO ()
readZigProgressStream fd onMsg = go BS.empty
  where
    go buf = do
      res <- (try (PIOB.fdRead fd 4096) :: IO (Either IOException BS.ByteString))
      case res of
        Left _ -> return ()
        Right chunk | BS.null chunk -> return ()
        Right chunk -> do
          let buf' = BS.append buf chunk
              (msgs, rest) = parseZigProgressMessages buf'
          mapM_ onMsg msgs
          go rest

parseZigProgressMessages :: BS.ByteString -> ([ZigProgress], BS.ByteString)
parseZigProgressMessages bs = go [] bs
  where
    go acc buf
      | BS.length buf < 1 = (reverse acc, buf)
      | otherwise =
          let len = fromIntegral (BS.index buf 0) :: Int
          in if len == 0xfe || len == 0xff
                then go acc (BS.drop 1 buf)
                else
                  let msgLen = 1 + (len * 49)
                  in if BS.length buf < msgLen
                        then (reverse acc, buf)
                        else
                          let msgBytes = BS.take msgLen buf
                              rest = BS.drop msgLen buf
                              msg = decodeMessage len msgBytes
                          in go (msg : acc) rest

decodeMessage :: Int -> BS.ByteString -> ZigProgress
decodeMessage len msgBytes =
    let nodeBytes = BS.take (len * 48) (BS.drop 1 msgBytes)
        parentBytes = BS.drop (1 + len * 48) msgBytes
        nodes = [ decodeNode i nodeBytes parentBytes | i <- [0..len - 1] ]
    in ZigProgress { zpNodes = nodes }

decodeNode :: Int -> BS.ByteString -> BS.ByteString -> ZigNode
decodeNode ix nodeBytes parentBytes =
    let base = ix * 48
        completed = word32le nodeBytes base
        total = word32le nodeBytes (base + 4)
        nameBytes = BS.take 40 (BS.drop (base + 8) nodeBytes)
        name = BSC.unpack (BS.takeWhile (/= 0) nameBytes)
        parentRaw = if ix < BS.length parentBytes
                      then BS.index parentBytes ix
                      else 0xff
        parent = if parentRaw == 0xff then Nothing else Just (fromIntegral parentRaw)
    in ZigNode
         { znCompleted = completed
         , znTotal = total
         , znName = name
         , znParent = parent
         }

word32le :: BS.ByteString -> Int -> Word32
word32le bs offset =
    let b0 = byteAt 0
        b1 = byteAt 1
        b2 = byteAt 2
        b3 = byteAt 3
    in (fromIntegral b0)
       .|. (fromIntegral b1 `shiftL` 8)
       .|. (fromIntegral b2 `shiftL` 16)
       .|. (fromIntegral b3 `shiftL` 24)
  where
    byteAt i =
      if offset + i < BS.length bs
        then BS.index bs (offset + i)
        else (0 :: Word8)

zigProgressRatio :: ZigProgress -> Maybe Double
zigProgressRatio progress =
    let nodes = zpNodes progress
        indexed = zip [0..] nodes
        totals =
          [ (i, clampTotal (znCompleted n) (znTotal n), znTotal n, znParent n)
          | (i, n) <- indexed
          , znTotal n > 0
          ]
        parentsWithTotals =
          Set.fromList [ p | (_, _, _, Just p) <- totals ]
        candidates =
          [ (c, t) | (i, c, t, _) <- totals, not (Set.member i parentsWithTotals) ]
        (sumC, sumT) = foldl' (\(cAcc, tAcc) (c, t) ->
                                 (cAcc + fromIntegral c, tAcc + fromIntegral t)) (0, 0) candidates
    in if sumT <= 0
         then Nothing
         else Just (min 1.0 (sumC / sumT))
  where
    clampTotal c t = if c > t then t else c
