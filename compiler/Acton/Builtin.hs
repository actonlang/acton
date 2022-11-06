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

module Acton.Builtin where

import Utils
import Acton.Syntax
    
selfKW                              = name "self"

initKW                              = name "__init__"
fromiterKW                          = name "__fromiter__"
fromatomKW                          = name "__fromatom__"
lenKW                               = name "__len__"
enterKW                             = name "__enter__"
exitKW                              = name "__exit__"
iterKW                              = name "__iter__"
nextKW                              = name "__next__"
containsKW                          = name "__contains__"
containsnotKW                       = name "__containsnot__"
getitemKW                           = name "__getitem__"
setitemKW                           = name "__setitem__"
delitemKW                           = name "__delitem__"
getsliceKW                          = name "__getslice__"
setsliceKW                          = name "__setslice__"
delsliceKW                          = name "__delslice__"
appendKW                            = name "append"
boolKW                              = name "__bool__"
strKW                               = name "__str__"
reprKW                               = name "__repr__"
resumeKW                            = name "__resume__"

valueKWs                            = [boolKW, strKW, reprKW]

iaddKW                              = name "__iadd__"
isubKW                              = name "__isub__"
imulKW                              = name "__imul__"
ipowKW                              = name "__ipow__"
itruedivKW                          = name "__itruediv__"
imodKW                              = name "__imod__"
ifloordivKW                         = name "__ifloordiv__"
ilshiftKW                           = name "__ilshift__"
irshiftKW                           = name "__irshift__"
iorKW                               = name "__ior__"
ixorKW                              = name "__ixor__"
iandKW                              = name "__iand__"
imatmulKW                           = name "__imatmul__"

addKW                               = name "__add__"
subKW                               = name "__sub__"
mulKW                               = name "__mul__"
powKW                               = name "__pow__"
truedivKW                           = name "__truediv__"
modKW                               = name "__mod__"
floordivKW                          = name "__floordiv__"
lshiftKW                            = name "__lshift__"
rshiftKW                            = name "__rshift__"
orKW                                = name "__or__"
xorKW                               = name "__xor__"
andKW                               = name "__and__"
matmulKW                            = name "__matmul__"

posKW                               = name "__pos__"
negKW                               = name "__neg__"
invertKW                            = name "__invert__"

eqKW                                = name "__eq__"
neKW                                = name "__ne__"
ltKW                                = name "__lt__"
leKW                                = name "__le__"
gtKW                                = name "__gt__"
geKW                                = name "__ge__"
isKW                                = name "__is__"
isnotKW                             = name "__isnot__"

nBuiltin                            = name "__builtin__"
mBuiltin                            = ModName [nBuiltin]
gBuiltin n                          = GName mBuiltin n

nValue                              = name "value"
nAtom                               = name "atom"
nObject                             = name "object"
nInt                                = name "int"
nI64                                = name "i64"
nFloat                              = name "float"
nBool                               = name "bool"
nStr                                = name "str"
nRepr                               = name "repr"
nBytes                              = name "bytes"
nRef                                = name "Ref"
nMsg                                = name "Msg"
nBaseException                      = name "BaseException"
nException                          = name "Exception"
nStopIteration                      = name "StopIteration"
nValueError                         = name "ValueError"
---
nRange                              = name "range"
nLen                                = name "len"
nPrint                              = name "print"
nDict                               = name "dict"
nList                               = name "list"
nSetT                               = name "set"
nSlice                              = name "slice"
---
nSequence                           = name "Sequence"
nMapping                            = name "Mapping"
nSetP                               = name "Set"
nIndexed                            = name "Indexed"
nSliceable                          = name "Sliceable"
nHashable                           = name "Hashable"
nPlus                               = name "Plus"
nMinus                              = name "Minus"
nTimes                              = name "Times"
nDiv                                = name "Div"
nNumber                             = name "Number"
nReal                               = name "Real"
nRealFloat                          = name "RealFloat"
nRational                           = name "Rational"
nIntegral                           = name "Integral"
nLogical                            = name "Logical"
nMatrix                             = name "Matrix"
nEq                                 = name "Eq"
nOrd                                = name "Ord"
nIdentity                           = name "Identity"
nCollection                         = name "Collection"
nContainer                          = name "Container"
nIterable                           = name "Iterable"
nContextManager                     = name "ContextManager"
nShow                               = name "Show"

qnValue                             = gBuiltin nValue
qnAtom                              = gBuiltin nAtom
qnObject                            = gBuiltin nObject
qnInt                               = gBuiltin nInt
qnI64                               = gBuiltin nI64
qnFloat                             = gBuiltin nFloat
qnBool                              = gBuiltin nBool
qnStr                               = gBuiltin nStr
qnRepr                              = gBuiltin nRepr
qnBytes                             = gBuiltin nBytes
qnRef                               = gBuiltin nRef
qnMsg                               = gBuiltin nMsg
qnBaseException                     = gBuiltin nBaseException
qnException                         = gBuiltin nException
qnStopIteration                     = gBuiltin nStopIteration
qnValueError                        = gBuiltin nValueError
---
qnRange                             = gBuiltin nRange
qnPrint                             = gBuiltin nPrint
qnDict                              = gBuiltin nDict
qnList                              = gBuiltin nList
qnSetT                              = gBuiltin nSetT
qnSlice                             = gBuiltin nSlice
---
qnSequence                          = gBuiltin nSequence
qnMapping                           = gBuiltin nMapping
qnSetP                              = gBuiltin nSetP
qnIndexed                           = gBuiltin nIndexed
qnSliceable                         = gBuiltin nSliceable
qHashable                           = gBuiltin nHashable
qnPlus                              = gBuiltin nPlus
qnMinus                             = gBuiltin nMinus
qnTimes                             = gBuiltin nTimes
qnDiv                               = gBuiltin nDiv
qnNumber                            = gBuiltin nNumber
qnReal                              = gBuiltin nReal
qnRealFloat                         = gBuiltin nRealFloat
qnRational                          = gBuiltin nRational
qnIntegral                          = gBuiltin nIntegral
qnLogical                           = gBuiltin nLogical
qnMatrix                            = gBuiltin nMatrix
qnEq                                = gBuiltin nEq
qnOrd                               = gBuiltin nOrd
qnIdentity                          = gBuiltin nIdentity
qnCollection                        = gBuiltin nCollection
qnContainer                         = gBuiltin nContainer
qnIterable                          = gBuiltin nIterable
qnContextManager                    = gBuiltin nContextManager
qnShow                              = gBuiltin nShow

cValue                              = TC qnValue []
cAtom                               = TC qnAtom []
cObject                             = TC qnObject []
cInt                                = TC qnInt []
cI64                                = TC qnI64 []
cFloat                              = TC qnFloat []
cBool                               = TC qnBool []
cStr                                = TC qnStr []
cRepr                               = TC qnRepr []
cBytes                              = TC qnBytes []
cRef                                = TC qnRef []
cMsg a                              = TC qnMsg [a]
cList a                             = TC qnList [a]
cDict a b                           = TC qnDict [a,b]
cSet a                              = TC qnSetT [a]
cSlice                              = TC qnSlice []
cBaseException                      = TC qnBaseException []
cException                          = TC qnException []
cStopIteration                      = TC qnStopIteration []
cValueError                         = TC qnValueError []
---
pSequence a                         = TC qnSequence [a]
pMapping a b                        = TC qnMapping [a,b]
pSet a                              = TC qnSetP [a]
pIndexed a b                        = TC qnIndexed [a,b]
pSliceable a                        = TC qnSliceable [a]
pHashable                           = TC qHashable []
pPlus                               = TC qnPlus []
pMinus                              = TC qnMinus []
pTimes a                            = TC qnTimes [a]
pDiv a                              = TC qnDiv [a]
pNumber                             = TC qnNumber []
pReal                               = TC qnReal []
pRealFloat                          = TC qnRealFloat []
pRational                           = TC qnRational []
pIntegral                           = TC qnIntegral []
pLogical                            = TC qnLogical []
pMatrix                             = TC qnMatrix []
pEq                                 = TC qnEq []
pOrd                                = TC qnOrd []
pIdentity                           = TC qnIdentity []
pCollection a                       = TC qnCollection [a]
pContainer a                        = TC qnContainer [a]
pIterable a                         = TC qnIterable [a]
pContextManager                     = TC qnContextManager []
pShow                               = TC qnShow []

tValue                              = tCon cValue
tAtom                               = tCon cAtom
tObject                             = tCon cObject
tInt                                = tCon cInt
tI64                                = tCon cI64
tFloat                              = tCon cFloat
tBool                               = tCon cBool
tStr                                = tCon cStr
tBytes                              = tCon cBytes
tRef                                = tCon cRef
tMsg a                              = tCon (cMsg a)
tList a                             = tCon (cList a)
tDict a b                           = tCon (cDict a b)
tSet a                              = tCon (cSet a)
tSlice                              = tCon cSlice
tBaseException                      = tCon cBaseException
tException                          = tCon cException
tStopIteration                      = tCon cStopIteration
tValueError                         = tCon cValueError
---
tSequence a                         = tCon (pSequence a)
tMapping a b                        = tCon (pMapping a b)
tSetExist a                         = tCon (pSet a)
tCollection a                       = tCon (pCollection a)

nNumpy                              = name "numpy"
mNumpy                              = ModName [nNumpy]
gNumpy n                            = GName mNumpy n

ndgetsliceKW                        = name "__ndgetslice__"

nNDArray                            = name "ndarray"
nNDSelect                           = name "ndselect"
nNDIndex                            = name "ndindex"
nNDSlice                            = name "ndslice"

qnNDArray                           = gNumpy nNDArray
qnNDSelect                          = gNumpy nNDSelect
qnNDIndex                           = gNumpy nNDIndex
qnNDSlice                           = gNumpy nNDSlice

cNDArray a                          = TC qnNDArray [a]
cNDSelect                           = TC qnNDSelect []
cNDIndex                            = TC qnNDIndex []
cNDSlice                            = TC qnNDSlice []

tNDArray a                          = tCon (cNDArray a)
tNDSelect                           = tCon cNDSelect
tNDIndex                            = tCon cNDIndex
tNDSlice                            = tCon cNDSlice
