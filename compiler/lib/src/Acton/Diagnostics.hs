{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Bridge between Acton error representations and the diagnose pretty-printing library
module Acton.Diagnostics where

import Error.Diagnose.Diagnostic
import Error.Diagnose.Report
import Error.Diagnose.Position
import Error.Diagnose.Style
import Error.Diagnose (addReport, addFile, printDiagnostic, prettyDiagnostic)
import Error.Diagnose.Report (Note(..))

import Data.List (intersperse, isPrefixOf, isInfixOf, intercalate)
import Data.Maybe (fromMaybe)
import Control.Exception (Exception(..), SomeException)
import qualified Data.List.NonEmpty as NE
import Text.Read (readMaybe)
import Data.Char (isDigit, isSpace)
import qualified Data.Set as S

import Text.Megaparsec (PosState(..), reachOffset)
import Text.Megaparsec.Error (ParseErrorBundle(..), parseErrorPretty, bundleErrors, errorBundlePretty, ShowErrorComponent(..), ParseError(..), errorOffset, parseErrorTextPretty, ErrorFancy(..))
import Text.Megaparsec.Pos (SourcePos(..), unPos, sourceLine, sourceColumn, mkPos)
import qualified Text.Megaparsec.Error as ME

import Acton.Syntax
import SrcLocation
import Utils (SrcLoc(..))
import Acton.Parser (CustomParseError(..)) -- Import for custom error types


-- | Convert Megaparsec parse errors to diagnose format
-- Handles syntax errors from the parsing phase with rich error information
-- like expected/unexpected tokens and parse positions.
parseDiagnosticFromBundle :: String -> String -> ParseErrorBundle String CustomParseError -> Diagnostic String
parseDiagnosticFromBundle filename src bundle =
    let -- Extract the first error (most relevant)
        firstError = NE.head (bundleErrors bundle)
        -- Get the error offset
        offset = errorOffset firstError
        -- Get the position state and calculate source position
        posState = bundlePosState bundle
        (_, newPosState) = reachOffset offset posState
        sourcePos = pstateSourcePos newPosState
        line = unPos (sourceLine sourcePos)
        col = unPos (sourceColumn sourcePos)
        -- Get the error message
        msg = parseErrorTextPretty firstError

        -- For parse errors, we only have a single position, not a span
        -- Just highlight one character at the error position
        -- Create position span
        position = Position (line, col) (line, col + 1) filename

        -- Check if this is a custom error and add appropriate hint
        (hints, markerMsg) = case firstError of
                  ME.FancyError _ errs ->
                    case findCustomError (S.toList errs) of
                      Just (TypeVariableNameError name) ->
                          ([Hint ("Single upper case character (optionally followed by digits) are reserved for type variables. Use a longer name.")],
                           "invalid name '" ++ name ++ "'")
                      _ -> ([], msg)
                  _ -> ([], msg)

        findCustomError :: [ErrorFancy CustomParseError] -> Maybe CustomParseError
        findCustomError [] = Nothing
        findCustomError (ErrorCustom err : _) = Just err
        findCustomError (_ : rest) = findCustomError rest

        -- Create the report
        report = Err (Just "Parse error") msg [(position, This markerMsg)] hints
        diagnostic = addReport mempty report
    in addFile diagnostic filename src



-- | Convert Acton compiler errors to diagnose format
-- Handles post-parse errors (context, type, compilation) that use SrcLoc offsets.
-- Uses Megaparsec's reachOffset just for offset-to-line/column conversion.
errorDiagnosticWithLoc :: String -> String -> String -> SrcLoc -> String -> Diagnostic String
errorDiagnosticWithLoc errorKind filename src srcLoc msg =
    let (line, col, endCol) = case srcLoc of
            NoLoc -> (1, 1, 2)  -- Default if no location
            Loc startOffset endOffset ->
                -- Use Megaparsec's reachOffset to convert offset to position
                -- Note: We're borrowing Megaparsec's offset-to-line/column conversion
                -- utility here, but these are NOT Megaparsec errors - they're Acton's
                -- own errors that just store character offsets for efficiency.
                -- Create a minimal PosState for the conversion
                let initialState = PosState
                        { pstateInput = src
                        , pstateOffset = 0
                        , pstateSourcePos = SourcePos filename (mkPos 1) (mkPos 1)
                        , pstateTabWidth = mkPos 8
                        , pstateLinePrefix = ""
                        }
                    (_, startState) = reachOffset startOffset initialState
                    (_, endState) = reachOffset endOffset initialState
                    startPos = pstateSourcePos startState
                    endPos = pstateSourcePos endState
                in (unPos (sourceLine startPos), unPos (sourceColumn startPos), unPos (sourceColumn endPos))

        -- Create position span
        position = Position (line, col) (line, endCol) filename

        -- Create the report
        report = Err (Just errorKind) msg [(position, This msg)] []
        diagnostic = addReport mempty report
    in addFile diagnostic filename src

