module Acton.Prim where

import Utils
import Acton.Syntax


contKW                              = prim "___c___"

prim s                              = Internal s 0 GenPass

nPrim                               = prim "___prim"
mPrim                               = ModName [nPrim]
qPrim n                             = QName mPrim (name n)

primIsNone                          = qPrim "IsNone"

primList                            = qPrim "list"
primDict                            = qPrim "dict"
primSet                             = qPrim "set"

primACTOR                           = qPrim "ACTOR"
primASYNC                           = qPrim "ASYNC"
primAWAIT                           = qPrim "AWAIT"

primPUSH                            = qPrim "PUSH"
primPOP                             = qPrim "POP"
primRAISE                           = qPrim "RAISE"

primCLOS                            = qPrim "CLOS"

primASSERT                          = qPrim "ASSERT"