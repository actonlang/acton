module Acton.Builtin where

import Utils
import Acton.Syntax
    
selfKW                              = name "__self__"
enterKW                             = name "__enter__"
exitKW                              = name "__exit__"
iterKW                              = name "iter"
nextKW                              = name "__next__"

nBuiltin                            = name "__builtin__"
mBuiltin                            = ModName [nBuiltin]
qBuiltin n                          = QName mBuiltin n

nSequence                           = name "Sequence"
nMapping                            = name "Mapping"
nSet                                = name "Set"
nInt                                = name "int"
nFloat                              = name "float"
nBool                               = name "bool"
nStr                                = name "str"
nBytes                              = name "bytes"
nRef                                = name "Ref"
nMsg                                = name "Msg"
nException                          = name "Exception"
nBoolean                            = name "Boolean"
nIndexed                            = name "Indexed"
nSliceable                          = name "Sliceable"
nPlus                               = name "Plus"
nMinus                              = name "Minus"
nNumber                             = name "Number"
nReal                               = name "Real"
nIntegral                           = name "Integral"
nLogical                            = name "Logical"
nMatrix                             = name "Matrix"
nEq                                 = name "Eq"
nOrd                                = name "Ord"
nIdentity                           = name "Identity"
nCollection                         = name "Collection"
nContextManager                     = name "ContextManager"
nObject                             = name "object"
nStopIteration                      = name "StopIteration"
nValueError                         = name "ValueError"
nShow                               = name "Show"

qnSequence                          = qBuiltin nSequence
qnMapping                           = qBuiltin nMapping
qnSet                               = qBuiltin nSet
qnInt                               = qBuiltin nInt
qnFloat                             = qBuiltin nFloat
qnBool                              = qBuiltin nBool
qnStr                               = qBuiltin nStr
qnBytes                             = qBuiltin nBytes
qnRef                               = qBuiltin nRef
qnMsg                               = qBuiltin nMsg
qnException                         = qBuiltin nException
qnBoolean                           = qBuiltin nBoolean
qnIndexed                           = qBuiltin nIndexed
qnSliceable                         = qBuiltin nSliceable
qnPlus                              = qBuiltin nPlus
qnMinus                             = qBuiltin nMinus
qnNumber                            = qBuiltin nNumber
qnReal                              = qBuiltin nReal
qnIntegral                          = qBuiltin nIntegral
qnLogical                           = qBuiltin nLogical
qnMatrix                            = qBuiltin nMatrix
qnEq                                = qBuiltin nEq
qnOrd                               = qBuiltin nOrd
qnIdentity                          = qBuiltin nIdentity
qnCollection                        = qBuiltin nCollection
qnContextManager                    = qBuiltin nContextManager
qnObject                            = qBuiltin nObject
qnStopIteration                     = qBuiltin nStopIteration
qnValueError                        = qBuiltin nValueError
qnShow                              = qBuiltin nShow

cSequence a                         = TC qnSequence [a]
cMapping a b                        = TC qnMapping [a,b]
cSet a                              = TC qnSet [a]
cInt                                = TC qnInt []
cFloat                              = TC qnFloat []
cBool                               = TC qnBool []
cStr                                = TC qnStr []
cBytes                              = TC qnBytes []
cRef                                = TC qnRef []
cMsg a                              = TC qnMsg [a]
cException                          = TC qnException []
cBoolean                            = TC qnBoolean []
cIndexed a b                        = TC qnIndexed [a,b]
cSliceable                          = TC qnSliceable []
cPlus                               = TC qnPlus []
cMinus                              = TC qnMinus []
cNumber                             = TC qnNumber []
cReal                               = TC qnReal []
cIntegral                           = TC qnIntegral []
cLogical                            = TC qnLogical []
cMatrix                             = TC qnMatrix []
cEq                                 = TC qnEq []
cOrd                                = TC qnOrd []
cIdentity                           = TC qnIdentity []
cCollection a                       = TC qnCollection [a]
cContextManager                     = TC qnContextManager []
cObject                             = TC qnObject []
cStopIteration                      = TC qnStopIteration []
cValueError                         = TC qnValueError []
cShow                               = TC qnShow []

pSequence a                         = tCon (cSequence a)
pMapping a b                        = tCon (cMapping a b)
pSet a                              = tCon (cSet a)
tInt                                = tCon cInt
tFloat                              = tCon cFloat
tBool                               = tCon cBool
tStr                                = tCon cStr
tBytes                              = tCon cBytes
tRef                                = tCon cRef
tMsg a                              = tCon (cMsg a)
tException                          = tCon cException
pBoolean                            = tCon cBoolean
pIndexed a b                        = tCon (cIndexed a b)
pSliceable                          = tCon cSliceable
pPlus                               = tCon cPlus
pMinus                              = tCon cMinus
pNumber                             = tCon cNumber
pReal                               = tCon cReal
pIntegral                           = tCon cIntegral
pLogical                            = tCon cLogical
pMatrix                             = tCon cMatrix
pEq                                 = tCon cEq
pOrd                                = tCon cOrd
pIdentity                           = tCon cIdentity
pCollection a                       = tCon (cCollection a)
pContextManager                     = tCon cContextManager
tObject                             = tCon cObject
tStopIteration                      = tCon cStopIteration
tValueError                         = tCon cValueError
pShow                               = tCon cShow

tSeq                                = pSequence
tDict                               = pMapping
tSet                                = pSet

uniLit t (ULit l)                   = t == tStr
uniLit t _                          = False

uniElem us u@(ULit l)               = u `elem` us || UCon qnStr `elem` us
uniElem us u                        = u `elem` us

uniCon us qn                        = qn `elem` uniCons && UCon qn `elem` us

uniCons                             = [qnInt, qnFloat, qnBool, qnStr]
