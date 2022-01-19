{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Language.Python.Common.SrcLocation 
-- Copyright   : (c) 2009 Bernie Pope 
-- License     : BSD-style
-- Maintainer  : bjpop@csse.unimelb.edu.au
-- Stability   : experimental
-- Portability : ghc
--
-- Source location information for the Python lexer and parser. This module
-- provides single-point locations and spans, and conversions between them.
-----------------------------------------------------------------------------

module SrcLocation (
  -- * Construction 
  SrcLocation (..),
  SrcSpan (..),
  Span (..),
  spanning,
  (><),
  mkSrcSpan,
  combineSrcSpans,
  initialSrcLocation,
  spanStartPoint,
  -- * Modification
  incColumn, 
  decColumn,
  incLine,
  incTab,
  -- * Projection of components of a span
  spanFile,
  endCol,
  endRow,
  startCol,
  startRow,
  showLine
) where

import Pretty
import Data.Data
import Debug.Trace
import qualified Data.Binary
import GHC.Generics (Generic)
import Prelude hiding((<>))

-- | A location for a syntactic entity from the source code.
-- The location is specified by its filename, and starting row
-- and column. 
data SrcLocation = 
   Sloc { sloc_filename :: !String
        , sloc_row :: {-# UNPACK #-} !Int
        , sloc_column :: {-# UNPACK #-} !Int 
        } 
   | NoLocation
   deriving (Eq,Ord,Show,Typeable,Data)

instance Pretty SrcLocation where
   pretty = pretty . getSpan

-- | Types which have a span.
class Span a where
   getSpan :: a -> SrcSpan
   getSpan x = SpanEmpty

-- | Create a new span which encloses two spanned things.
spanning :: (Span a, Span b) => a -> b -> SrcSpan
spanning x y = combineSrcSpans (getSpan x) (getSpan y)

infixr 7 ><
(><) :: (Span a, Span b) => a -> b -> SrcSpan
a >< b = spanning a b


instance Span a => Span [a] where
   getSpan [] = SpanEmpty
   getSpan [x] = getSpan x 
   getSpan list@(x:xs) = combineSrcSpans (getSpan x) (getSpan (last list))

instance Span a => Span (Maybe a) where
   getSpan Nothing = SpanEmpty
   getSpan (Just x) = getSpan x

instance (Span a, Span b) => Span (Either a b) where
   getSpan (Left x) = getSpan x
   getSpan (Right x) = getSpan x

instance (Span a, Span b) => Span (a, b) where
   getSpan (x,y) = spanning x y

instance Span SrcSpan where
   getSpan = id

-- | Construct the initial source location for a file.
initialSrcLocation :: String -> SrcLocation
initialSrcLocation filename 
    = Sloc 
      { sloc_filename = filename
      , sloc_row = 1
      , sloc_column = 1
      }

-- | Decrement the column of a location, only if they are on the same row.
decColumn :: Int -> SrcLocation -> SrcLocation
decColumn n loc
   | n < col = loc { sloc_column = col - n }
   | otherwise = loc 
   where
   col = sloc_column loc

-- | Increment the column of a location. 
incColumn :: Int -> SrcLocation -> SrcLocation
incColumn n loc@(Sloc { sloc_column = col })
   = loc { sloc_column = col + n }

-- | Increment the column of a location by one tab stop.
incTab :: SrcLocation -> SrcLocation
incTab loc@(Sloc { sloc_column = col })
   = loc { sloc_column = newCol } 
   where
   newCol = col + 8 - (col - 1) `mod` 8

-- | Increment the line number (row) of a location by one.
incLine :: Int -> SrcLocation -> SrcLocation
incLine n loc@(Sloc { sloc_row = row }) 
   = loc { sloc_column = 1, sloc_row = row + n }

{-
Inspired heavily by compiler/basicTypes/SrcLoc.lhs 
A SrcSpan delimits a portion of a text file.  
-}

-- | Source location spanning a contiguous section of a file.
data SrcSpan
    -- | A span which starts and ends on the same line.
  = SpanCoLinear
    { span_filename     :: !String
    , span_row          :: {-# UNPACK #-} !Int
    , span_start_column :: {-# UNPACK #-} !Int
    , span_end_column   :: {-# UNPACK #-} !Int
    }
    -- | A span which starts and ends on different lines.
  | SpanMultiLine
    { span_filename     :: !String
    , span_start_row    :: {-# UNPACK #-} !Int
    , span_start_column :: {-# UNPACK #-} !Int
    , span_end_row      :: {-# UNPACK #-} !Int
    , span_end_column   :: {-# UNPACK #-} !Int
    }
    -- | A span which is actually just one point in the file.
  | SpanPoint
    { span_filename :: !String
    , span_row      :: {-# UNPACK #-} !Int
    , span_column   :: {-# UNPACK #-} !Int
    }
    -- | No span information.
  | SpanNested
    { span_outer :: !SrcSpan
    , span_inner :: !SrcSpan
    }
  | SpanEmpty 
   deriving (Eq,Ord,Typeable,Data,Read,Show,Generic)

instance Data.Binary.Binary SrcSpan

{-
instance Show SrcSpan where
    show _ = "_"

instance Read SrcSpan where
    readsPrec p (' ':s)  = readsPrec p s
    readsPrec p ('\t':s) = readsPrec p s
    readsPrec p ('\n':s) = readsPrec p s
    readsPrec _ ('_':s)  = [(SpanEmpty,s)]
    readsPrec _ s        = []
-}

instance Pretty SrcSpan where
   pretty span@(SpanCoLinear {}) = prettyMultiSpan span
   pretty span@(SpanMultiLine {}) = prettyMultiSpan span
   pretty span@(SpanPoint {})
      = text (span_filename span) <> colon <+>
        parens (pretty (span_row span) <> comma <> pretty (span_column span))
   pretty SpanNested{span_outer=o, span_inner=i} = pretty o <+> text "referencing" $$ pretty i
   pretty SpanEmpty = empty

prettyMultiSpan :: SrcSpan -> Doc 
prettyMultiSpan span 
  = text (spanFile span) <> colon <+>
    parens (pretty (startRow span) <> comma <> pretty (startCol span)) <> char '-' <>
    parens (pretty (endRow span) <> comma <> pretty (endCol span))

instance Span SrcLocation where
   getSpan loc@(Sloc {})
      = SpanPoint 
        { span_filename = sloc_filename loc
        , span_row = sloc_row loc
        , span_column = sloc_column loc
        }
   getSpan NoLocation = SpanEmpty 

-- | Make a point span from the start of a span
spanStartPoint :: SrcSpan -> SrcSpan
spanStartPoint SpanEmpty = SpanEmpty
spanStartPoint span = 
   SpanPoint 
   { span_filename = spanFile span
   , span_row = startRow span
   , span_column = startCol span
   }

-- | Make a span from two locations. Assumption: either the
-- arguments are the same, or the left one preceeds the right one.
mkSrcSpan :: SrcLocation -> SrcLocation -> SrcSpan
mkSrcSpan NoLocation _ = SpanEmpty
mkSrcSpan _ NoLocation = SpanEmpty 
mkSrcSpan loc1 loc2
  | line1 == line2 = 
       if col2 <= col1 
          then SpanPoint file line1 col1
          else SpanCoLinear file line1 col1 col2
  | otherwise = 
       SpanMultiLine file line1 col1 line2 col2
  where
  line1 = sloc_row loc1
  line2 = sloc_row loc2
  col1 = sloc_column loc1
  col2 = sloc_column loc2
  file = sloc_filename loc1

-- | Combines two 'SrcSpan' into one that spans at least all the characters
-- within both spans. Assumes the "file" part is the same in both inputs
combineSrcSpans :: SrcSpan -> SrcSpan -> SrcSpan
combineSrcSpans SpanEmpty r = r -- this seems more useful
combineSrcSpans l SpanEmpty = l
combineSrcSpans (SpanNested o1 i1) (SpanNested o2 i2)
  | o1 == o2 = SpanNested o1 (combineSrcSpans i1 i2)
combineSrcSpans (SpanNested o i) r = combineSrcSpans i r
combineSrcSpans l (SpanNested o i) = combineSrcSpans l i
combineSrcSpans start end
 = case row1 `compare` row2 of
     EQ -> case col1 `compare` col2 of
                EQ -> SpanPoint file row1 col1
                LT -> SpanCoLinear file row1 col1 col2
                GT -> SpanCoLinear file row1 col2 col1
     LT -> SpanMultiLine file row1 col1 row2 col2
     GT -> SpanMultiLine file row2 col2 row1 col1
  where
  row1 = startRow start
  col1 = startCol start
  row2 = endRow end
  col2 = endCol end
  file = spanFile start

spanFile :: SrcSpan -> String
spanFile (SpanNested o i) = spanFile o
spanFile SpanEmpty = error "spanFile called on empty span"
spanFile sp = span_filename sp

-- | Get the row of the start of a span.
startRow :: SrcSpan -> Int
startRow (SpanCoLinear { span_row = row }) = row
startRow (SpanMultiLine { span_start_row = row }) = row
startRow (SpanPoint { span_row = row }) = row
startRow (SpanNested o i) = startRow o
startRow SpanEmpty = error "startRow called on empty span"

-- | Get the row of the end of a span.
endRow :: SrcSpan -> Int
endRow (SpanCoLinear { span_row = row }) = row
endRow (SpanMultiLine { span_end_row = row }) = row
endRow (SpanPoint { span_row = row }) = row
endRow (SpanNested o i) = endRow o
endRow SpanEmpty = error "endRow called on empty span"

-- | Get the column of the start of a span.
startCol :: SrcSpan -> Int
startCol (SpanCoLinear { span_start_column = col }) = col 
startCol (SpanMultiLine { span_start_column = col }) = col 
startCol (SpanPoint { span_column = col }) = col 
startCol (SpanNested o i) = startCol o
startCol SpanEmpty = error "startCol called on empty span"

-- | Get the column of the end of a span.
endCol :: SrcSpan -> Int
endCol (SpanCoLinear { span_end_column = col }) = col 
endCol (SpanMultiLine { span_end_column = col }) = col 
endCol (SpanPoint { span_column = col }) = col 
endCol (SpanNested o i) = endCol o
endCol SpanEmpty = error "endCol called on empty span"

showLine :: SrcSpan -> String
showLine (SpanNested o i)   = showLine o ++ "->" ++ showLine i
showLine l                  = show (startRow l)
