module Acton.Prim where

import Utils
import Pretty
import Acton.Syntax
import Acton.Builtin

nPrim               = name "$"
mPrim               = ModName [nPrim]
qPrim n             = QName mPrim n

primActor           = qPrim $ name "Actor"
primR               = qPrim $ name "R"
primClos            = qPrim $ name "Clos"

primASYNCf          = qPrim $ name "ASYNCf"
primAFTERf          = qPrim $ name "AFTERf"
primAWAITf          = qPrim $ name "AWAITf"

primASYNCc          = qPrim $ name "ASYNCc"
primAFTERc          = qPrim $ name "AFTERc"
primAWAITc          = qPrim $ name "AWAITc"

primASYNC           = qPrim $ name "ASYNC"
primAFTER           = qPrim $ name "AFTER"
primAWAIT           = qPrim $ name "AWAIT"

primPUSHc           = qPrim $ name "PUSHc"
primPUSH            = qPrim $ name "PUSH"

primPOP             = qPrim $ name "POP"
primRERAISE         = qPrim $ name "RERAISE"
primRAISE           = qPrim $ name "RAISE"
primRAISEFROM       = qPrim $ name "RAISEFROM"
primASSERT          = qPrim $ name "ASSERT"

primISINSTANCE      = qPrim $ name "ISINSTANCE"
primCAST            = qPrim $ name "CAST"

primRContc          = qPrim $ name "R_CONTc"
primRCont           = qPrim $ name "R_CONT"

primEqOpt           = qPrim $ name "EqOpt"
primIdentityOpt     = qPrim $ name "IdentityOpt"

primWEqNone         = qPrim $ name "wEqNone"
primWIdentityNone   = qPrim $ name "wIdentityNone"
primWEqUnion        = qPrim $ name "wEqUnion"
primWPlusInt        = qPrim $ name "wPlusInt"

primISNOTNONE       = qPrim $ name "ISNOTNONE"


tActor              = tCon $ TC primActor []
tR                  = tCon $ TC primR []
tClos x p t         = tCon $ TC primClos [x,p,t]
tCont x t           = tClos x (posRow t posNil) tR


primMkEnv cls def var   = [ (noq primASYNCf,        def scASYNCf NoDec),
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

                            (noq primRContc,        def scRContc NoDec),
                            (noq primRCont,         def scRCont NoDec),

                            (noq primEqOpt,         clEqOpt cls def),
                            (noq primIdentityOpt,   clIdentityOpt cls def),

                            (noq primWEqNone,       var tEqNone),
                            (noq primWIdentityNone, var tIdentityNone),
                            (noq primWEqUnion,      var tEqUnion),
                            (noq primWPlusInt,      var tPlusInt),

                            (noq primISNOTNONE,     def scISNOTNONE NoDec)
                            
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

--  $Cont[X,A]      = $Clos[X,(A,),$R]

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



--  $PUSHc          : [X] => pure (X(Exception)->$R) -> None
scPUSHc             = tSchema [quant x] tPUSH
  where tPUSH       = tFun fxPure (posRow tCont' posNil) kwdNil tNone
        x           = TV KFX $ name "X"
        tCont'      = tFun (tVar x) (posRow tException posNil) kwdNil tR

--  $PUSH           : [X] => pure ($Cont[X,Exception]) -> None
scPUSH              = tSchema [quant x] tPUSH
  where tPUSH       = tFun fxPure (posRow tCont' posNil) kwdNil tNone
        x           = TV KFX $ name "X" 
        a           = TV KType $ name "A"
        tCont'      = tCont (tVar x) tException



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


--  class $EqOpt[A] (Eq[?A]): pass
clEqOpt cls def     = cls [quant a] [([Nothing],TC qnEq [tOpt $ tVar a])] clTEnv
  where clTEnv      = [ (initKW, def scInit NoDec) ]
        scInit      = tSchema [] $ tFun fxPure (posRow (tCon $ TC qnEq [tVar a]) posNil) kwdNil tNone
        a           = TV KType (name "A")

--  class $EqOpt[A] (Eq[?A]): pass
clIdentityOpt cls def = cls [quant a] [([Nothing],TC qnIdentity [tOpt $ tVar a])] []
  where a           = TV KType (name "A")

--  w$EqNone        : Eq[None]
tEqNone             = tCon $ TC qnEq [tNone]

--  w$IdentityNone  : Identity[None]
tIdentityNone       = tCon $ TC qnIdentity [tNone]

--  $wEqUnion       : Eq[(int|float|bool|str)]
tEqUnion            = tCon $ TC qnEq [tUnion $ map UCon [qnInt,qnFloat,qnBool,qnStr]]

--  $wPlusInt       : Plus[int]
tPlusInt            = tCon $ TC qnPlus [tInt]


--  $ISNOTNONE      : [A] => pure (?A) -> bool
scISNOTNONE         = tSchema [quant a] tISNOTNONE
  where tISNOTNONE  = tFun fxPure (posRow (tOpt $ tVar a) posNil) kwdNil tBool
        a           = TV KType (name "A")
