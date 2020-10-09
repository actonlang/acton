module Acton.Prim where

import Utils
import Pretty
import Acton.Syntax
import Acton.Builtin

nPrim               = name "$"
mPrim               = ModName [nPrim]
qPrim s             = QName mPrim (name s)

primActor           = qPrim "Actor"
primR               = qPrim "R"
primClos            = qPrim "Clos"
primCont            = qPrim "Cont"

primASYNCf          = qPrim "ASYNCf"
primAFTERf          = qPrim "AFTERf"
primAWAITf          = qPrim "AWAITf"

primASYNCc          = qPrim "ASYNCc"
primAFTERc          = qPrim "AFTERc"
primAWAITc          = qPrim "AWAITc"

primASYNC           = qPrim "ASYNC"
primAFTER           = qPrim "AFTER"
primAWAIT           = qPrim "AWAIT"

primPUSHc           = qPrim "PUSHc"
primPUSH            = qPrim "PUSH"

primPOP             = qPrim "POP"
primRERAISE         = qPrim "RERAISE"
primRAISE           = qPrim "RAISE"
primRAISEFROM       = qPrim "RAISEFROM"
primASSERT          = qPrim "ASSERT"

primISINSTANCE      = qPrim "ISINSTANCE"
primCAST            = qPrim "CAST"

primRContc          = qPrim "R_CONTc"
primRCont           = qPrim "R_CONT"



tActor              = tCon $ TC primActor []
tR                  = tCon $ TC primR []
tClos x p t         = tCon $ TC primClos [x,p,t]
tCont x t           = tCon $ TC primCont [x,t]


primMkEnv cls def   = [ (noq primASYNCf,        def scASYNCf NoDec),
                        (noq primAFTERf,        def scAFTERf NoDec),
                        (noq primAWAITf,        def scAWAITf NoDec),

                        (noq primASYNCc,        def scASYNCc NoDec),
                        (noq primAFTERc,        def scAFTERc NoDec),
                        (noq primAWAITc,        def scAWAITc NoDec),

                        (noq primASYNC,         def scASYNC NoDec),
                        (noq primAFTER,         def scAFTER NoDec),
                        (noq primAWAIT,         def scAWAIT NoDec),
                        
                        (noq primPUSHc,         def scPUSHc NoDec),
                        (noq primPUSH,          def scPUSH NoDec),
                        
                        (noq primPOP,           def scPOP NoDec),
                        (noq primRERAISE,       def scRERAISE NoDec),
                        (noq primRAISE,         def scRAISE NoDec),
                        (noq primRAISEFROM,     def scRAISEFROM NoDec),
                        (noq primASSERT,        def scASSERT NoDec),
                        (noq primISINSTANCE,    def scISINSTANCE NoDec),
                        (noq primCAST,          def scCAST NoDec),
                        
                        (noq primActor,         clActor cls def),
                        (noq primR,             clR cls def),
                        (noq primClos,          clClos cls def),
                        (noq primCont,          clCont cls def),

                        (noq primRContc,        def scRContc NoDec),
                        (noq primRCont,         def scRCont NoDec)
                      ]

--  class $Actor (): pass
clActor cls def     = cls [] [] []

--  class $R (): pass
clR cls def         = cls [] [] []

--  class $Clos[X,P,A] ():
--      __init__    : () -> None
--      enter       : X(*P) -> A
clClos cls def      = cls [quant x, quant p, quant a] [] clTEnv
  where clTEnv      = [ (initKW, def scInit NoDec), (enterKW, def scEnter NoDec) ]
        scInit      = tSchema [] $ tFun fxPure posNil kwdNil tNone
        scEnter     = tSchema [] $ tFun (tVar x) (tVar p) kwdNil (tVar a)
        x           = TV KFX (name "X")
        p           = TV PRow (name "P")
        a           = TV KType (name "A")

--  class $Cont[X,T] ($Clos[X,(A,),$R]): pass
clCont cls def      = cls [quant x, quant a] [([Nothing],TC primClos [tVar x, posRow (tVar a) posNil, tR])] []
  where x           = TV KFX (name "X")
        a           = TV KType (name "A")

--  $ASYNCf         : [S,A] => act[S]($Actor, act[S]()->A) -> Msg[A]
scASYNCf            = tSchema [quant s, quant a] tASYNC
  where tASYNC      = tFun actS (posRow tActor $ posRow tFun' posNil) kwdNil (tMsg $ tVar a)
        s           = TV KType $ name "S"
        a           = TV KType $ name "A"
        tFun'       = tFun actS posNil kwdNil (tVar a)
        actS        = fxAct $ tVar s

--  $AFTERf         : [S,A] => act[S](int, act[S]()->A) -> Msg[A]
scAFTERf            = tSchema [quant s, quant a] tAFTER
  where tAFTER      = tFun actS (posRow tInt $ posRow tFun' posNil) kwdNil (tMsg $ tVar a)
        s           = TV KType $ name "S"
        a           = TV KType $ name "A"
        tFun'       = tFun actS posNil kwdNil (tVar a)
        actS        = fxAct $ tVar s

--  $AWAITf         : [S,A] => act[S](Msg[A]) -> A
scAWAITf            = tSchema [quant s, quant a] tAWAIT
  where tAWAIT      = tFun actS (posRow (tMsg $ tVar a) posNil) kwdNil (tVar a)
        s           = TV KType $ name "S"
        a           = TV KType $ name "T"
        actS        = fxAct $ tVar s


--  $ASYNCc         : [S,A] => mut[S]($Actor, mut[S](mut[S](A)->$R)->$R) -> Msg[A]
scASYNCc            = tSchema [quant s, quant a] tASYNC
  where tASYNC      = tFun mutS (posRow tActor $ posRow tCont' posNil) kwdNil (tMsg $ tVar a)
        s           = TV KType $ name "S"
        a           = TV KType $ name "A"
        tCont'      = tFun mutS (posRow tCont'' posNil) kwdNil tR
        tCont''     = tFun mutS (posRow (tVar a) posNil) kwdNil tR
        mutS        = fxMut $ tVar s

--  $AFTERc         : [S,A] => mut[S](int, mut[S](mut[S](A)->$R)->$R) -> Msg[A]
scAFTERc            = tSchema [quant s, quant a] tAFTER
  where tAFTER      = tFun mutS (posRow tInt $ posRow tCont' posNil) kwdNil (tMsg $ tVar a)
        s           = TV KType $ name "S"
        a           = TV KType $ name "A"
        tCont'      = tFun mutS (posRow tCont'' posNil) kwdNil tR
        tCont''     = tFun mutS (posRow (tVar a) posNil) kwdNil tR
        mutS        = fxMut $ tVar s

--  $AWAITc         : [S,A] => mut[S](Msg[A], mut[S](A)->$R) -> $R
scAWAITc            = tSchema [quant s, quant a] tAWAIT
  where tAWAIT      = tFun mutS (posRow (tMsg $ tVar a) $ posRow tCont' posNil) kwdNil tR
        s           = TV KType $ name "S"
        a           = TV KType $ name "A"
        tCont'      = tFun mutS (posRow (tVar a) posNil) kwdNil tR
        mutS        = fxMut $ tVar s


--  $ASYNC          : [S,A] => mut[S]($Actor, $Cont[mut[S],$Cont[mut[S],A]]) -> Msg[A]
scASYNC             = tSchema [quant s, quant a] tASYNC
  where tASYNC      = tFun mutS (posRow tActor $ posRow tCont' posNil) kwdNil (tMsg $ tVar a)
        s           = TV KType $ name "S"
        a           = TV KType $ name "A"
        tCont'      = tCont mutS tCont''
        tCont''     = tCont mutS (tVar a)
        mutS        = fxMut $ tVar s

--  $AFTER          : [S,A] => mut[S](int,    $Cont[mut[S],$Cont[mut[S],A]]) -> Msg[A]
scAFTER             = tSchema [quant s, quant a] tAFTER
  where tAFTER      = tFun mutS (posRow tInt $ posRow tCont' posNil) kwdNil (tMsg $ tVar a)
        s           = TV KType $ name "S"
        a           = TV KType $ name "A"
        tCont'      = tCont mutS tCont''
        tCont''     = tCont mutS (tVar a)
        mutS        = fxMut $ tVar s

--  $AWAIT          : [S,A] => mut[S](Msg[A], $Cont[mut[S],A])               -> $R
scAWAIT             = tSchema [quant s, quant a] tAWAIT
  where tAWAIT      = tFun mutS (posRow (tMsg $ tVar a) $ posRow tCont' posNil) kwdNil tR
        s           = TV KType $ name "S"
        a           = TV KType $ name "A"
        tCont'      = tCont mutS (tVar a)
        mutS        = fxMut $ tVar s



--  $PUSHc          : [X,A] => pure (X(A)->$R) -> None
scPUSHc             = tSchema [quant x, quant a] tPUSH
  where tPUSH       = tFun fxPure (posRow tCont' posNil) kwdNil tNone
        x           = TV KFX $ name "X"
        a           = TV KType $ name "A"
        tCont'      = tFun (tVar x) (posRow (tVar a) posNil) kwdNil tR

--  $PUSH           : [X,A] => pure ($Cont[X,A]) -> None
scPUSH              = tSchema [quant x, quant a] tPUSH
  where tPUSH       = tFun fxPure (posRow tCont' posNil) kwdNil tNone
        x           = TV KFX $ name "X" 
        a           = TV KType $ name "A"
        tCont'      = tCont (tVar x) (tVar a)



--  $POP            : pure () -> None
scPOP               = tSchema [] tPOP
  where tPOP        = tFun fxPure posNil kwdNil tNone

--  $RERAISE        : pure () -> None
scRERAISE           = tSchema [] tRERAISE
  where tRERAISE    = tFun fxPure posNil kwdNil tNone

--  $RAISE          : pure (Exception) -> None
scRAISE             = tSchema [] tRAISE
  where tRAISE      = tFun fxPure (posRow tException posNil) kwdNil tNone

--  $RAISEFROM      : pure (Exception, Exception) -> None
scRAISEFROM         = tSchema [] tRAISEFROM
  where tRAISEFROM  = tFun fxPure (posRow tException $ posRow tException posNil) kwdNil tNone

--  $ASSERT         : pure (bool, ?str) -> None
scASSERT           = tSchema [] tASSERT
  where tASSERT    = tFun fxPure (posRow tBool $ posRow (tOpt tStr) posNil) kwdNil tNone

--  $ISINSTANCE     : pure (struct,_) -> bool
scISINSTANCE        = tSchema [] tISINSTANCE
  where tISINSTANCE = tFun fxPure (posRow tStruct $ posRow tWild posNil) kwdNil tNone

--  $CAST           : [A,B] => (A) -> B
scCAST              = tSchema [quant a, quant b] tCAST
  where tCAST       = tFun fxPure (posRow (tVar a) posNil) kwdNil (tVar b)
        a           = TV KType $ name "A"
        b           = TV KType $ name "B"

--  $R_CONTc        : [X,A] => X(X(A)->$R, A) -> $R
scRContc            = tSchema [quant x, quant a] tRCont
  where tRCont      = tFun (tVar x) (posRow tCont' $ posRow (tVar a) posNil) kwdNil tR
        tCont'      = tFun (tVar x) (posRow (tVar a) posNil) kwdNil tR
        x           = TV KFX $ name "X" 
        a           = TV KType $ name "A"

--  $R_CONT         : [X,A] => X($Cont[X,A], A) -> $R
scRCont             = tSchema [quant x, quant a] tRCont
  where tRCont      = tFun (tVar x) (posRow tCont' $ posRow (tVar a) posNil) kwdNil tR
        tCont'      = tCont (tVar x) (tVar a)
        x           = TV KFX $ name "X" 
        a           = TV KType $ name "A"
