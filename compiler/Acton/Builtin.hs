module Acton.Builtin where

import Utils
import Acton.Syntax
    
nBuiltin                            = name "__builtin__"
qnBuiltin                           = QName nBuiltin []
qBuiltin n                          = QName nBuiltin [n]

nSequence                           = name "Sequence"
nMapping                            = name "Mapping"
nSet                                = name "Set"
nInt                                = name "int"
nFloat                              = name "float"
nBool                               = name "bool"
nStr                                = name "str"

qnSequence                          = qBuiltin nSequence
qnMapping                           = qBuiltin nMapping
qnSet                               = qBuiltin nSet
qnInt                               = qBuiltin nInt
qnFloat                             = qBuiltin nFloat
qnBool                              = qBuiltin nBool
qnStr                               = qBuiltin nStr

pSequence a                         = TCon NoLoc (TC qnSequence [a])
pMapping a b                        = TCon NoLoc (TC qnMapping [a,b])
pSet a                              = TCon NoLoc (TC qnSet [a])
tInt                                = TCon NoLoc (TC qnInt [])
tFloat                              = TCon NoLoc (TC qnFloat [])
tBool                               = TCon NoLoc (TC qnBool [])
tStr                                = TCon NoLoc (TC qnStr [])


