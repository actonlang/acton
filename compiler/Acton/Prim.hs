module Acton.Prim where

import Utils
import Pretty
import Acton.Syntax


contKW                              = prim "$_c"

prim s                              = Internal s 0 GenPass

nPrim                               = prim "PRIM"
mPrim                               = ModName [nPrim]
qPrim n                             = QName mPrim n

nIsNone                             = name "IsNone"
nASYNC                              = name "ASYNC"
nAFTER                              = name "AFTER"
nAWAIT                              = name "AWAIT"
nPUSH                               = name "PUSH"
nPOP                                = name "POP"
nRERAISE                            = name "RERAISE"
nRAISE                              = name "RAISE"
nRAISEFROM                          = name "RAISEFROM"
nCLOS                               = name "CLOS"
nCONT                               = name "_CONT"
nASSERT                             = name "ASSERT"
nPOSTPONE                           = name "POSTPONE"

primIsNone                          = qPrim nIsNone
primASYNC                           = qPrim nASYNC
primAFTER                           = qPrim nAFTER
primAWAIT                           = qPrim nAWAIT
primPUSH                            = qPrim nPUSH
primPOP                             = qPrim nPOP
primRERAISE                         = qPrim nRERAISE
primRAISE                           = qPrim nRAISE
primRAISEFROM                       = qPrim nRAISEFROM
primCLOS                            = qPrim nCLOS
primCONT                            = qPrim nCONT
primASSERT                          = qPrim nASSERT
