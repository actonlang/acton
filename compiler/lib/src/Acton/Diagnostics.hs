{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Acton.Diagnostics where

import Error.Diagnose.Diagnostic
import Error.Diagnose.Report
import Error.Diagnose.Position
import Error.Diagnose.Style
import Error.Diagnose (addReport, addFile, printDiagnostic, prettyDiagnostic)

import Data.List (intersperse, isPrefixOf, isInfixOf, intercalate)
import Data.Maybe (fromMaybe)
import Control.Exception (Exception(..), SomeException)
import qualified Data.List.NonEmpty as NE
import Text.Read (readMaybe)
import Data.Char (isDigit, isSpace)

import Text.Megaparsec (PosState(..), reachOffset)
import Text.Megaparsec.Error (ParseErrorBundle(..), parseErrorPretty, bundleErrors, errorBundlePretty, ShowErrorComponent(..), ParseError(..), errorOffset, parseErrorTextPretty)
import Text.Megaparsec.Pos (SourcePos(..), unPos, sourceLine, sourceColumn, mkPos)
import qualified Text.Megaparsec.Error as ME

import Acton.Syntax
import SrcLocation
import Utils (SrcLoc(..))
import Acton.Parser () -- Import for ShowErrorComponent String instance


-- | Create a Diagnose Diagnostic from a Megaparsec ParseErrorBundle using structured data
parseDiagnosticFromBundle :: String -> String -> ParseErrorBundle String String -> Diagnostic String
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

        -- Create the report
        report = Err (Just "Parse error") msg [(position, This msg)] []
        diagnostic = addReport mempty report
    in addFile diagnostic filename src



-- | Create a Diagnose Diagnostic for errors with structured location data
errorDiagnosticWithLoc :: String -> String -> String -> SrcLoc -> String -> Diagnostic String
errorDiagnosticWithLoc errorKind filename src srcLoc msg =
    let (line, col, endCol) = case srcLoc of
            NoLoc -> (1, 1, 2)  -- Default if no location
            Loc startOffset endOffset ->
                -- Use Megaparsec's reachOffset to convert offset to position
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

