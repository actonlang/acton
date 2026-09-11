{-# LANGUAGE OverloadedStrings #-}
module TestOutputTests (testOutputTests) where

import Data.List (foldl')
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import TestOutput

testOutputTests :: TestTree
testOutputTests = testGroup "test output capture"
  [ testCase "quiet iterations leave no captured logs" $ do
      let quiet = capture (concat (replicate 100000 ["", marker, "  "]))
      assertEqual "neither stream retains iteration markers" ("", "") (finishTestOutput quiet quiet)
  , testCase "sparse streams retain iteration pairing and repeated output" $ do
      let out = capture [marker, "first", marker, marker, marker, "both", marker, "both"]
          err = capture [marker, marker, "second", marker, marker, "error", marker, "error"]
      assertEqual "only jointly empty iterations disappear"
        [("first", ""), ("", "second"), ("both", "error"), ("both", "error")]
        (pairs (finishTestOutput out err))
  , testCase "preamble and unfinished final frames survive" $ do
      let out = capture ["startup", marker, "last line without newline"]
          err = capture [marker, "{\"unrelated\":true}"]
      assertEqual "diagnostics before the first invocation stay separate"
        [("startup", ""), ("last line without newline", "{\"unrelated\":true}")]
        (pairs (finishTestOutput out err))
  , testCase "marker-like payload and internal whitespace survive" $ do
      let payload = "trace: == Running test, iteration: 12\n\n  λ\n== Running test, iteration: unknown"
          out = capture (marker : T.lines payload)
          err = capture [marker]
          (outText, errText) = finishTestOutput out err
      assertBool "marker-like text is meaningful" (testOutputMeaningful (T.unpack payload))
      assertBool "an empty counterpart is not meaningful" (not (testOutputMeaningful errText))
      assertEqual "payload survives capture and reporting" [T.unpack payload] (splitTestOutput outText)
  , testCase "cached logs retain empty frames for pairing" $ do
      assertEqual "old cached streams split without dropping empty frames"
        ["", "second", ""] (splitTestOutput (T.unpack (T.unlines [marker, marker, "second", marker])))
  ]
  where
    -- Deliberately repeat the printed invocation ID, as stress workers do.
    marker = "== Running test, iteration: 1"
    capture = foldl' (flip appendTestOutput) emptyTestOutput
    pairs (out, err) = zip (splitTestOutput out) (splitTestOutput err)
