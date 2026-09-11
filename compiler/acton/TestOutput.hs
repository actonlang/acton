{-# LANGUAGE OverloadedStrings #-}
module TestOutput
  ( TestOutput
  , emptyTestOutput
  , appendTestOutput
  , finishTestOutput
  , testOutputMeaningful
  , splitTestOutput
  ) where

import Data.List (foldl')
import qualified Data.Text as T

-- Keep only nonempty frames. Marker ordinals, rather than invocation IDs,
-- preserve stream alignment when stress workers restart their numbering.
data TestOutput = TestOutput !Int ![T.Text] ![(Int, T.Text)]

emptyTestOutput :: TestOutput
emptyTestOutput = TestOutput 0 [] []

appendTestOutput :: T.Text -> TestOutput -> TestOutput
appendTestOutput line output@(TestOutput ordinal current frames)
  | isMarker line = TestOutput (ordinal + 1) [] (saveFrame output)
  | null current && T.null (T.strip line) = output
  | otherwise = TestOutput ordinal (line : current) frames

saveFrame :: TestOutput -> [(Int, T.Text)]
saveFrame (TestOutput ordinal current frames)
  | T.null body = frames
  | otherwise = (ordinal, body) : frames
  where body = T.strip (T.unlines (reverse current))

-- Emit aligned frames in the existing cached log format. An empty counterpart
-- is needed when only one stream has output; entirely empty iterations vanish.
finishTestOutput :: TestOutput -> TestOutput -> (String, String)
finishTestOutput out err =
    let frames = merge (reverse (saveFrame out)) (reverse (saveFrame err))
        render select = T.unpack (T.concat [marker <> T.pack (show n) <> "\n" <> select o e <> "\n" | (n, o, e) <- frames])
    in (render const, render (flip const))
  where
    merge [] es = [(n, "", e) | (n, e) <- es]
    merge os [] = [(n, o, "") | (n, o) <- os]
    merge os@((n, o):os') es@((m, e):es')
      | n < m = (n, o, "") : merge os' es
      | n > m = (m, "", e) : merge os es'
      | otherwise = (n, o, e) : merge os' es'

marker :: T.Text
marker = "== Running test, iteration: "

isMarker :: T.Text -> Bool
isMarker line =
    case T.stripPrefix marker (T.strip line) of
      Just number -> not (T.null number) && T.all (\c -> c >= '0' && c <= '9') number
      Nothing -> False

testOutputMeaningful :: String -> Bool
testOutputMeaningful = any (\line -> not (T.null (T.strip line)) && not (isMarker line)) . T.lines . T.pack

-- Both human and JSON reports must split the cached streams identically,
-- including empty counterparts used to align stdout with stderr.
splitTestOutput :: String -> [String]
splitTestOutput buf =
    let step (frames, current, seen) line
          | isMarker line = (if seen then body current : frames else frames, [], True)
          | otherwise = (frames, line : current, seen)
        body = T.unpack . T.strip . T.unlines . reverse
        (frames, current, seen) = foldl' step ([], [], False) (T.lines (T.pack buf))
    in if seen
         then reverse (body current : frames)
         else if null (body current) then [] else [body current]
