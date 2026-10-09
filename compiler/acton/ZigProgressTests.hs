module ZigProgressTests (zigProgressTests) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Test.Tasty
import Test.Tasty.HUnit

import ZigProgress

zigProgressTests :: TestTree
zigProgressTests = testGroup "Zig progress"
  [ testCase "120-byte names keep node counts and parents aligned" $ do
      let (msgs, rest) = parseZigProgressMessages packet
      assertEqual "one complete packet" 1 (length msgs)
      assertEqual "no leftover bytes" BS.empty rest
      mapM_ assertNodes msgs
  , testCase "concatenated packets retain an incomplete tail" $ do
      let (prefix, suffix) = BS.splitAt 100 packet
          (msgs, rest) = parseZigProgressMessages (BS.concat [packet, packet, prefix])
      assertEqual "only complete packets are decoded" 2 (length msgs)
      assertEqual "incomplete packet is retained" prefix rest
      mapM_ assertNodes msgs
      let (lastMsgs, lastRest) = parseZigProgressMessages (BS.append rest suffix)
      assertEqual "buffered packet completes" 1 (length lastMsgs)
      assertEqual "no leftover bytes after completion" BS.empty lastRest
      mapM_ assertNodes lastMsgs
  ]
  where
    name = replicate 119 'x' ++ "y"
    -- Two little-endian counters, a 120-byte name field, then parent indices.
    packet = BS.concat
      [ BS.pack [2]
      , BS.pack [1, 0, 0, 0, 4, 0, 0, 0]
      , BSC.pack name
      , BS.pack [3, 0, 0, 0, 10, 0, 0, 0]
      , BSC.pack "child"
      , BS.replicate 115 0
      , BS.pack [0xff, 0]
      ]
    assertNodes progress =
      assertEqual "node contents"
        [(1, 4, name, Nothing), (3, 10, "child", Just 0)]
        [(znCompleted n, znTotal n, znName n, znParent n) | n <- zpNodes progress]
