module Acton.Builtin where

import Utils
import Acton.Syntax
    
nSelf                               = name "__self__"

nBuiltin                            = name "__builtin__"
qnBuiltin                           = QName nBuiltin []
qBuiltin n                          = QName nBuiltin [n]

nIterable                           = name "Iterable"
nSequence                           = name "Sequence"
nMapping                            = name "Mapping"
nSet                                = name "Set"
nInt                                = name "int"
nFloat                              = name "float"
nBool                               = name "bool"
nStr                                = name "str"
nRef                                = name "Ref"

qnIterable                          = qBuiltin nIterable
qnSequence                          = qBuiltin nSequence
qnMapping                           = qBuiltin nMapping
qnSet                               = qBuiltin nSet
qnInt                               = qBuiltin nInt
qnFloat                             = qBuiltin nFloat
qnBool                              = qBuiltin nBool
qnStr                               = qBuiltin nStr
qnRef                               = qBuiltin nRef

cIterable a                         = TC qnIterable [a]
cSequence a                         = TC qnSequence [a]
cMapping a b                        = TC qnMapping [a,b]
cSet a                              = TC qnSet [a]
cInt                                = TC qnInt []
cFloat                              = TC qnFloat []
cBool                               = TC qnBool []
cStr                                = TC qnStr []
cRef                                = TC qnRef []

pIterable a                         = tCon (cIterable a)
pSequence a                         = tCon (cSequence a)
pMapping a b                        = tCon (cMapping a b)
pSet a                              = tCon (cSet a)
tInt                                = tCon cInt
tFloat                              = tCon cFloat
tBool                               = tCon cBool
tStr                                = tCon cStr
tRef                                = tCon cRef