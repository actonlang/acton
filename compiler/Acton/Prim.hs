module Acton.Prim where

import Utils
import Acton.Syntax


contKW                              = prim "___c___"

prim s                              = Internal s 0 GenPass

nPrim                               = prim "___prim___"
mPrim                               = ModName [nPrim]
qPrim n                             = QName mPrim (name n)

primIsNone                          = qPrim "IsNone"

primList                            = qPrim "list"
primDict                            = qPrim "dict"
primSet                             = qPrim "set"

primASYNC                           = qPrim "ASYNC"
primAWAIT                           = qPrim "AWAIT"

primPUSH                            = qPrim "PUSH"
primPOP                             = qPrim "POP"
primRERAISE                         = qPrim "RERAISE"
primRAISE                           = qPrim "RAISE"
primRAISEFROM                       = qPrim "RAISEFROM"

primCLOS                            = qPrim "CLOS"

primASSERT                          = qPrim "ASSERT"
