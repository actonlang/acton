{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Language.Python.Common.Pretty
-- Copyright   : (c) 2009 Bernie Pope 
-- License     : BSD-style
-- Maintainer  : bjpop@csse.unimelb.edu.au
-- Stability   : experimental
-- Portability : ghc
--
-- Convenience class for pretty printing combinators.
-----------------------------------------------------------------------------

module Pretty (module TextPP, module Pretty) where

import Text.PrettyPrint as TextPP

--------------------------------------------------------------------------------

infixl 5 $++$

a $++$ b    = a $+$ blank $+$ b


print x     = render $ pretty x

vprint x    = render $ vpretty x

vpretty x   = vcat $ punctuate (text "\n") $ map pretty x


-- | All types which can be transformed into a 'Doc'.
class Pretty a where
   pretty :: a -> Doc

-- | Transform values into strings.
prettyText :: Pretty a => a -> String
prettyText = render . pretty

-- | Print just the prefix of something
prettyPrefix :: Pretty a => Int -> a -> Doc
prettyPrefix maxLen x
   | length fullText <= maxLen = pretty fullText
   | otherwise = pretty (take maxLen fullText) <+> text "..." 
   where
   fullText = prettyText x 

instance Pretty String where
   pretty s = text s

{-
This is not used in Acton compiler, so we reuse the name for the following function, which we have use for.

-- | Conditionally wrap parentheses around an item.
parensIf :: Pretty a => (a -> Bool) -> a -> Doc
parensIf test x = if test x then parens $ pretty x else pretty x 
-}

parensIf :: Bool -> Doc -> Doc
parensIf b e = if b then parens e else e


perhaps :: Pretty a => Maybe a -> Doc -> Doc
perhaps Nothing doc = empty
perhaps (Just {}) doc = doc 

dot = char '.'

blank = text ""

-- | A list of things separated by dots (and no spaces).
dotCat :: (a -> Doc) -> [a] -> Doc
dotCat f = hcat . punctuate dot . map f

-- | A list of things separated by commas.
commaSep :: (a -> Doc) -> [a] -> Doc
commaSep f = hsep . punctuate comma . map f

commaCat :: Pretty a => [a] -> Doc
commaCat = hcat . punctuate comma . map pretty

commaList :: Pretty a => [a] -> Doc
commaList = commaSep pretty 

commaIf :: [a] -> Doc
commaIf [] = empty
commaIf _  = comma

-- | A list of things separated by equals signs.
equalsList :: Pretty a => [a] -> Doc
equalsList = hsep . punctuate (space TextPP.<> equals) . map pretty

vmap :: (a -> Doc) -> [a] -> Doc
vmap f = vcat . map f

nonEmpty :: (Doc -> Doc) -> (t -> Doc) -> t -> Doc
nonEmpty f g x
  | isEmpty doc = doc
  | otherwise = f doc
  where doc = g x

prettyPair :: (Pretty a, Pretty b) => String -> (a, b) -> Doc
prettyPair str (a, b) = pretty a <+> nonEmpty (text str <+>) pretty b


instance Pretty Int where
  pretty = int

instance Pretty Integer where
  pretty = integer

instance Pretty Double where
   pretty = double

instance Pretty Bool where
  pretty True = text "True"
  pretty False = text "False"

instance Pretty a => Pretty (Maybe a) where
   pretty Nothing = empty
   pretty (Just x) = pretty x
