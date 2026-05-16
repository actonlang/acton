{-# LANGUAGE OverloadedStrings #-}

module TestGolden
  ( normalizeProgressTimingLine
  , normalizeProgressTimings
  ) where

import           Data.Char (isDigit)
import qualified Data.Text as T

-- | Replace trailing durations like "12.345 s" with a stable token.
normalizeProgressTimingLine :: T.Text -> T.Text
normalizeProgressTimingLine t =
  case T.stripSuffix " s" t of
    Nothing -> t
    Just pre ->
      let field = T.takeWhileEnd (\c -> isDigit c || c == '.') pre
          pre' = T.dropEnd (T.length field) pre
      in case T.splitOn "." field of
           [intPart, frac]
             | not (T.null intPart)
               && T.length frac == 3
               && T.all isDigit intPart
               && T.all isDigit frac ->
                 let base = "0.000"
                     padding = if T.length field > T.length base then " " else ""
                 in pre' <> padding <> base <> " s"
           _ -> t

normalizeProgressTimings :: String -> String
normalizeProgressTimings =
    unlines . map (T.unpack . normalizeProgressTimingLine . T.pack) . lines
