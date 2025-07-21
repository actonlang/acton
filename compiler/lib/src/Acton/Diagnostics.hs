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
import Utils (SrcLoc(..), loc)
import Acton.Parser (CustomParseError(..), CustomParseException(..), ContextError(..), IndentationError(..), ctxMsg) -- Import for custom error types
import qualified Utils as U
import qualified Acton.Env as Env
import Pretty (render, pretty, text, (<+>), (<>), comma, equals)
import Prelude hiding ((<>))


-- | Convert CustomParseError to diagnostic components (error message and hints/notes)
customParseErrorToDiagnostic :: CustomParseError -> (String, [Note String])
customParseErrorToDiagnostic (TypeVariableNameError name)      = ("invalid name '" ++ name ++ "'",
                                                                  [Note "Single upper case character (optionally followed by digits) are reserved for type variables",
                                                                   Hint "Use a longer name"])
customParseErrorToDiagnostic (InvalidFormatSpecifier spec)     = ("invalid format specifier" ++ if null spec then "" else ": " ++ spec,
                                                                  [])
customParseErrorToDiagnostic TooManyQuotesError                = ("too many quote characters",
                                                                  [Note "Triple-quoted strings accept 3-5 quotes at the end"])
customParseErrorToDiagnostic UnclosedString                    = ("missing closing \"",
                                                                  [])
customParseErrorToDiagnostic (OtherError msg)                  = (msg,
                                                                  [])


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
        msg = parseErrorTextPretty firstError

        -- For parse errors, we only have a single position, not a span
        -- Just highlight one character at the error position
        -- Create position span
        position = Position (line, col) (line, col + 1) filename

        -- Check if this is a custom error and get the text message & hints / notes
        (prettyMsg, hints) = case firstError of
                  ME.FancyError _ errs ->
                    case findCustomError (S.toList errs) of
                      Just customErr -> customParseErrorToDiagnostic customErr
                      Nothing -> (msg, [])
                  _ -> (msg, [])

        findCustomError :: [ErrorFancy CustomParseError] -> Maybe CustomParseError
        findCustomError [] = Nothing
        findCustomError (ErrorCustom err : _) = Just err
        findCustomError (_ : rest) = findCustomError rest

        report = Err (Just "Parse error") msg [(position, This prettyMsg)] hints
        diagnostic = addReport mempty report
    in addFile diagnostic filename src


-- | Convert CustomParseException to diagnostic format directly
-- CustomParseExceptions are essentially an exception container for
-- CustomParseError, so extract the error and convert that
customParseExceptionToDiagnostic :: String -> String -> CustomParseException -> Diagnostic String
customParseExceptionToDiagnostic filename src (CustomParseException loc customErr) =
    customParseErrorDiagnostic "Syntax error" filename src loc customErr


-- | Convert CustomParseError to Diagnostic
-- This is used by tests to ensure consistent error formatting
customParseErrorDiagnostic :: String -> String -> String -> SrcLoc -> CustomParseError -> Diagnostic String
customParseErrorDiagnostic errKind filename src srcLoc customErr =
    let (msg, hints) = customParseErrorToDiagnostic customErr
        (line, col, endCol) = case srcLoc of
            NoLoc -> (1, 1, 2)
            Loc startOffset endOffset ->
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
        
        position = Position (line, col) (line, endCol) filename
        report = Err (Just errKind) msg [(position, This msg)] hints
        diagnostic = addReport mempty report
    in addFile diagnostic filename src

-- | Convert Acton compiler errors to diagnose format
-- Classic Acton errors consist of (loc, msg). Wrap in OtherError and convert to
-- Diagnostic.
-- One day we'll convert everything to more structured errors and get rid of
-- this function
actErrToDiagnostic errKind filename src srcLoc msg =
    customParseErrorDiagnostic errKind filename src srcLoc (OtherError msg)
