module Acton.Prim where

import Utils
import Pretty
import Acton.Syntax

mPrim                               = ModName [name "$"]

qPrim s                             = QName mPrim (name s)

primActor                           = qPrim "Actor"         -- class $Actor (): pass
primR                               = qPrim "R"             -- class $R (): pass
primClos                            = qPrim "Clos"          -- class $Clos[X,P,T] ():
                                                            --    __init__  : () -> None
                                                            --    enter     : X(*P) -> T
primCont                            = qPrim "Cont"          -- class $Cont[X,T] ($Clos[X,(T,),$R]): pass

primASYNC                           = qPrim "ASYNC"         -- $ASYNC       : [S,T] => act[S]($Actor, act[S]()->T) -> T
primAFTER                           = qPrim "AFTER"         -- $AFTER       : [S,T] => act[S](int, act[S]()->T) -> None
primAWAIT                           = qPrim "AWAIT"         -- $AWAIT       : [S,T] => act[S](Msg[T]) -> T
primPUSH                            = qPrim "PUSH"          -- $PUSH        : [T] => pure ($Cont[T]) -> None
primPOP                             = qPrim "POP"           -- $POP         : pure () -> None
primRERAISE                         = qPrim "RERAISE"       -- $RERAISE     : pure () -> None
primRAISE                           = qPrim "RAISE"         -- $RAISE       : pure (Exception) -> None
primRAISEFROM                       = qPrim "RAISEFROM"     -- $RAISEFROM   : pure (Exception, Exception) -> None
primASSERT                          = qPrim "ASSERT"        -- $ASSERT      : pure (bool, ?str) -> None

primIsInstance                      = qPrim "ISINSTANCE"    -- $ISINSTANCE : pure (struct,_) -> bool

tClos x p t                         = tCon $ TC primClos [x,p,t]
tCont x t                           = tCon $ TC primCont [x,t]

{-
tASYNC                              = tFun fxS (posRow tActor $ posRow tCont1 posNil)
  where tActor                      = tCon $ TC primActor []
        s                           = tVar $ TV KType $ name "S"
        t                           = tVar $ TV KType $ name "T"
        fxS                         = fxAct s
        tCont1                      = tCont fxS posNil a
-}