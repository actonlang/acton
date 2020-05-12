module Acton.Prim where

import Utils
import Pretty
import Acton.Syntax

mPrim                               = ModName [name "$"]

qPrim s                             = QName mPrim (name s)

primIsNone                          = qPrim "IsNone"
primASYNC                           = qPrim "ASYNC"
primAFTER                           = qPrim "AFTER"
primAWAIT                           = qPrim "AWAIT"
primPUSH                            = qPrim "PUSH"
primPOP                             = qPrim "POP"
primRERAISE                         = qPrim "RERAISE"
primRAISE                           = qPrim "RAISE"
primRAISEFROM                       = qPrim "RAISEFROM"
primASSERT                          = qPrim "ASSERT"

primCLOS                            = qPrim "Clos"
primCONT                            = qPrim "Cont"
