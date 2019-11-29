module Acton.Prim where

import Utils
import Pretty
import Acton.Syntax


contKW                              = prim "___c___"

prim s                              = Internal s 0 GenPass

nPrim                               = prim "___prim___"
mPrim                               = ModName [nPrim]
qPrim n                             = QName mPrim n

nIsNone                             = name "IsNone"
nASYNC                              = name "ASYNC"
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
primAWAIT                           = qPrim nAWAIT
primPUSH                            = qPrim nPUSH
primPOP                             = qPrim nPOP
primRERAISE                         = qPrim nRERAISE
primRAISE                           = qPrim nRAISE
primRAISEFROM                       = qPrim nRAISEFROM
primCLOS                            = qPrim nCLOS
primCONT                            = qPrim nCONT
primASSERT                          = qPrim nASSERT

genPrimName n                       = text (nstr n)
