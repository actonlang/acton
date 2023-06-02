-- Copyright (C) 2019-2021 Data Ductus AB
--
-- Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
--
-- 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
--
-- 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
--
-- 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DeriveAnyClass #-}
module Utils(module Utils, module SrcLocation, module Data.List, module Data.Maybe, module Debug.Trace) where

import Debug.Trace
import Data.List hiding ((\\))
import Data.Maybe
import qualified Data.Binary
import qualified Control.Exception
import Data.Typeable
import GHC.Generics (Generic)
import SrcLocation
import Pretty
import Control.DeepSeq
import Prelude hiding((<>))

data SrcLoc                     = Loc Int Int | NoLoc deriving (Eq,Ord,Show,Read,Generic,NFData)

instance Data.Binary.Binary SrcLoc

instance Pretty SrcLoc where
    pretty (Loc l r)            = pretty l <> text "-" <> pretty r
    pretty NoLoc                = text "??"

Loc i _ `upto` Loc _ j          = Loc i j
NoLoc `upto` l                  = l
l `upto` NoLoc                  = l

class HasLoc a where
    loc                         :: a -> SrcLoc

instance HasLoc SrcLoc where
    loc                         = id

instance HasLoc a => HasLoc [a] where
    loc                         = foldl (\l x -> l `upto` loc x) NoLoc

instance HasLoc a => HasLoc (Maybe a) where
    loc                         = maybe NoLoc loc

xs \\ ys                        = [ x | x <- xs, x `notElem` ys ]       -- remove *all* occurrences of xs elements from ys

duplicates []                   = []
duplicates (x:xs)               = eqs ++ duplicates others
  where (eqs,others)            = partition (==x) xs

dom                             = map fst

rng                             = map snd

kvs `exclude` ks                = filter ((`notElem` ks) . fst) kvs

kvs `restrict` ks               = filter ((`elem` ks) . fst) kvs

mapFst f xs                     = [ (f a, b) | (a,b) <- xs ]

mapSnd f xs                     = [ (a, f b) | (a,b) <- xs ]

catLeft xs                      = [ x | Left x <- xs ]

catRight xs                     = [ x | Right x <- xs ]

l0                              = NoLoc

ignore t                        = return ()

prstr x                         = render (pretty x)

prstrs xs                       = render (hsep $ punctuate comma (map pretty xs))

prcat xs                        = render (vcat $ (map pretty xs))

traceF f x                      = trace (f x) x

tr s f x                        = trace (s ++ ": " ++ prstr x) $ f x

ptrace doc                      = trace (render doc)

ptraceM doc                     = traceM (render doc)

ptraceF f                       = traceF (render . f)

head_ tag []                    = error ("Prelude.head: empty list (" ++ show tag ++ ")")
head_ tag xs                    = head xs


errReport (SpanCoLinear file row start end,msg) src
                                = unlines [line1,line2,line3,line4,msg]
  where rowstr                  = show row
        line1                   = file ++ " "++show row++":"++show start++"-"++show end
        line2                   = replicate (length rowstr+1) ' '++"|"
        line3                   = rowstr ++ " |" ++ lines(src)!!!(row-1)
        line4                   = replicate (length rowstr+1) ' '++"|"++replicate (start-1) ' '++replicate (end-start+1) '^'
errReport (SpanMultiLine file srow scol erow ecol,msg) src
                                =unlines (line1:line2:quote++(if erow-srow<=2 then [] else [replicate (length erowstr) ' ' ++ " | ..."])++[line2,msg])
  where erowstr                 = show erow
        line1                   = file ++ " "++show srow++":"++show scol++"-"++show erow++":"++show ecol
        line2                   = replicate (length erowstr+1) ' '++"|"
        pad n                   = replicate (length erowstr - length nstr) ' ' ++ nstr
          where nstr            = show n
        quote                   = map line [srow..minimum [srow+2,erow,length ls]]
        ls                      = lines src
        line n                  = pad n ++ " |" ++ ls!!!(n-1)
errReport (SpanPoint file row col,msg) src
                                = unlines [line1,line2,line3,line4,msg]
  where rowstr                  = show row
        line1                   = file ++ " "++show row++":"++show col
        line2                   = replicate (length rowstr+1) ' '++"|"
        line3                   = rowstr ++ " |" ++ lines src!!!(row-1)
        line4                   = replicate (length rowstr+1) ' '++"|"++replicate (col-1) ' '++"^"
errReport (SpanEmpty,msg) _     = msg

lst !!! ix | ix >= length lst   = lst !! 0
           | otherwise          = lst !! ix

---------------------------------

data GeneralError               = InternalErr SrcLoc Doc
                                | NotYet SrcLoc Doc
                                deriving (Eq,Show,Typeable)

instance Control.Exception.Exception GeneralError

instance HasLoc GeneralError where
    loc (InternalErr l doc)     = l
    loc (NotYet l doc)          = l

internal loc x                  = Control.Exception.throw $ InternalErr loc (pretty x)
notYet loc x                    = Control.Exception.throw $ NotYet loc (pretty x)

generalError err                = (loc err,render (expl err))
  where
    expl (InternalErr _ doc)    = text "(internal)" <+> doc
    expl (NotYet _ doc)         = text "Not yet supported:" <+> doc

iff True m                      = m >> return ()
iff False _                     = return ()

