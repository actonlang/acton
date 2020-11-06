module Acton.Prim where

import Utils
import Pretty
import Acton.Syntax
import Acton.Builtin

nPrim               = name "$"
mPrim               = ModName [nPrim]
gPrim s             = GName mPrim (name s)

primKW s            = name ("$" ++ s)

primActor           = gPrim "Actor"
primR               = gPrim "R"
primClos            = gPrim "Clos"
primCont            = gPrim "Cont"

primASYNCw          = gPrim "ASYNCw"

primASYNCf          = gPrim "ASYNCf"
primAFTERf          = gPrim "AFTERf"
primAWAITf          = gPrim "AWAITf"

primASYNCc          = gPrim "ASYNCc"
primAFTERc          = gPrim "AFTERc"
primAWAITc          = gPrim "AWAITc"

primASYNC           = gPrim "ASYNC"
primAFTER           = gPrim "AFTER"
primAWAIT           = gPrim "AWAIT"

primPUSHc           = gPrim "PUSHc"
primPUSH            = gPrim "PUSH"

primPOP             = gPrim "POP"
primRERAISE         = gPrim "RERAISE"
primRAISE           = gPrim "RAISE"
primRAISEFROM       = gPrim "RAISEFROM"
primASSERT          = gPrim "ASSERT"

primISINSTANCE      = gPrim "ISINSTANCE"
primCAST            = gPrim "CAST"

primRContc          = gPrim "R_CONTc"
primRCont           = gPrim "R_CONT"

primEqOpt           = gPrim "EqOpt"
primIdentityOpt     = gPrim "IdentityOpt"

primWEqNone         = gPrim "wEqNone"
primWIdentityNone   = gPrim "wIdentityNone"
primWEqUnion        = gPrim "wEqUnion"
primWPlusInt        = gPrim "wPlusInt"

primISNOTNONE       = gPrim "ISNOTNONE"

primSKIPRESc        = gPrim "SKIPRESc"
primSKIPRES         = gPrim "SKIPRES"


tActor              = tCon $ TC primActor []
tR                  = tCon $ TC primR []
tClos x p t         = tCon $ TC primClos [x,p,t]
tCont x t           = tCon $ TC primCont [x,posRow t posNil]


primMkEnv cls def var sig = 
                        [   (noq primASYNCw,        def scASYNCw NoDec),

                            (noq primASYNCf,        def scASYNCf NoDec),
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
                        
                            (noq primActor,         clActor cls def sig),
                            (noq primR,             clR cls def),
                            (noq primClos,          clClos cls def),
                            (noq primCont,          clCont cls def),

                            (noq primRContc,        def scRContc NoDec),
                            (noq primRCont,         def scRCont NoDec),

                            (noq primEqOpt,         clEqOpt cls def),
                            (noq primIdentityOpt,   clIdentityOpt cls def),

                            (noq primWEqNone,       var tEqNone),
                            (noq primWIdentityNone, var tIdentityNone),
                            (noq primWEqUnion,      var tEqUnion),
                            (noq primWPlusInt,      var tPlusInt),

                            (noq primISNOTNONE,     def scISNOTNONE NoDec),

                            (noq primSKIPRESc,      def scSKIPRESc NoDec),
                            (noq primSKIPRES,       def scSKIPRES NoDec)
                            
                      ]

--  class $Actor (): pass
clActor cls def sig = cls [] [] te
  where te          = [ (primKW "next",     sig (monotype tActor) Property),
                        (primKW "msg",      sig (monotype (tMsg tWild)) Property),
                        (primKW "outgoing", sig (monotype (tMsg tWild)) Property),
                        (primKW "catcher",  sig (monotype $ tCon $ TC (gPrim "Catcher") []) Property),
                        (primKW "msg_lock", sig (monotype $ tCon $ TC (gPrim "Lock") []) Property) ]
        

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

--  class $Clos[X,P] ($Clos[X,P,$R]):
--      pass
clCont cls def      = cls [quant x, quant p] [([Nothing],TC primClos [tVar x, tVar p, tR])] []
  where x           = TV KFX (name "X")
        p           = TV PRow (name "P")


--  $ASYNCw         : [S,A] => act[S](act[S]()->A) -> Msg[A]
scASYNCw            = tSchema [quant s, quant a] tASYNC
  where tASYNC      = tFun actS (posRow tFun' posNil) kwdNil (tMsg $ tVar a)
        s           = TV KType $ name "S"
        a           = TV KType $ name "A"
        tFun'       = tFun actS posNil kwdNil (tVar a)
        actS        = fxAct $ tVar s

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


--  $ASYNC          : [S,A] => mut[S]($Actor, $Cont[mut[S],($Cont[mut[S],A],)]) -> Msg[A]
scASYNC             = tSchema [quant s, quant a] tASYNC
  where tASYNC      = tFun mutS (posRow tActor $ posRow tCont' posNil) kwdNil (tMsg $ tVar a)
        s           = TV KType $ name "S"
        a           = TV KType $ name "A"
        tCont'      = tCont mutS tCont''
        tCont''     = tCont mutS (tVar a)
        mutS        = fxMut $ tVar s

--  $AFTER          : [S,A] => mut[S](int, $Cont[mut[S],($Cont[mut[S],A],)]) -> Msg[A]
scAFTER             = tSchema [quant s, quant a] tAFTER
  where tAFTER      = tFun mutS (posRow tInt $ posRow tCont' posNil) kwdNil (tMsg $ tVar a)
        s           = TV KType $ name "S"
        a           = TV KType $ name "A"
        tCont'      = tCont mutS tCont''
        tCont''     = tCont mutS (tVar a)
        mutS        = fxMut $ tVar s

--  $AWAIT          : [S,A] => mut[S](Msg[A], $Cont[mut[S],(A,)]) -> $R
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

--  $PUSH           : [X] => pure ($Cont[X,(Exception,)]) -> None
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

--  $R_CONT         : [X,A] => X($Cont[X,(A,)], A) -> $R
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

--  $SKIPRESc       : [X,A] => X(X(None)->$R) -> X(A)->$R
scSKIPRESc          = tSchema [] tSKIPRES
  where tSKIPRES    = tFun (tVar x) (posRow tCont' posNil) kwdNil tCont''
        tCont'      = tFun (tVar x) (posRow tNone posNil) kwdNil tR
        tCont''     = tFun (tVar x) (posRow (tVar a) posNil) kwdNil tR
        x           = TV KFX $ name "X"
        a           = TV KType $ name "A"

--  $SKIPRES        : [X,A] => X($Cont[X,(None,)]) -> $Cont[X,(A,)]
scSKIPRES           = tSchema [] tSKIPRES
  where tSKIPRES    = tFun (tVar x) (posRow tCont' posNil) kwdNil tCont''
        tCont'      = tCont (tVar x) tNone
        tCont''     = tCont (tVar x) (tVar a)
        x           = TV KFX $ name "X"
        a           = TV KType $ name "A"
