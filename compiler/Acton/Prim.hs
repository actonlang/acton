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
primCont            = gPrim "Cont"

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
primNEWACT          = gPrim "NEWACT"

primISINSTANCE      = gPrim "ISINSTANCE"
primCAST            = gPrim "CAST"
primCONSTCONT       = gPrim "CONSTCONT"

primFORMAT          = gPrim "FORMAT"

primRContc          = gPrim "R_CONTc"
primRCont           = gPrim "R_CONT"

primEqOpt           = gPrim "EqOpt"
primIdentityOpt     = gPrim "IdentityOpt"

primWEqNone         = gPrim "wEqNone"
primWIdentityNone   = gPrim "wIdentityNone"

primWIntegralInt    = gPrim "Integral$int$witness"

primISNOTNONE       = gPrim "ISNOTNONE"

primSKIPRESc        = gPrim "SKIPRESc"
primSKIPRES         = gPrim "SKIPRES"


tActor              = tCon $ TC primActor []
tR                  = tCon $ TC primR []
tCont x t           = tCon $ TC primCont [x,posRow t posNil]


primMkEnv cls def var sig = 
                        [   (noq primASYNCf,        def scASYNCf NoDec),
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
                            (noq primNEWACT,        def scNEWACT NoDec),

                            (noq primISINSTANCE,    def scISINSTANCE NoDec),
                            (noq primCAST,          def scCAST NoDec),
                            (noq primCONSTCONT,     def scCONSTCONT NoDec),

                            (noq primFORMAT,        def scFORMAT NoDec),
                        
                            (noq primActor,         clActor cls def sig),
                            (noq primR,             clR cls def),
                            (noq primCont,          clCont cls def),

                            (noq primRContc,        def scRContc NoDec),
                            (noq primRCont,         def scRCont NoDec),

                            (noq primEqOpt,         clEqOpt cls def),
                            (noq primIdentityOpt,   clIdentityOpt cls def),

                            (noq primWEqNone,       var tEqNone),
                            (noq primWIdentityNone, var tIdentityNone),
                            (noq primWIntegralInt,  var tIntegralInt),

                            (noq primISNOTNONE,     def scISNOTNONE NoDec),

                            (noq primSKIPRESc,      def scSKIPRESc NoDec),
                            (noq primSKIPRES,       def scSKIPRES NoDec)

                      ]

--  class $Actor (): pass
clActor cls def sig = cls [] [([Nothing],cStruct)] te
  where te          = [ (primKW "next",      sig (monotype tActor) Property),
                        (primKW "msg",       sig (monotype (tMsg tWild)) Property),
                        (primKW "outgoing",  sig (monotype (tMsg tWild)) Property),
                        (primKW "offspring", sig (monotype tActor) Property),
                        (primKW "waitsfor",  sig (monotype (tMsg tWild)) Property),
                        (primKW "catcher",   sig (monotype $ tCon $ TC (gPrim "Catcher") []) Property),
                        (primKW "msg_lock",  sig (monotype $ tCon $ TC (gPrim "Lock") []) Property),
                        (primKW "globkey",   sig (monotype tWild) Property),
                        (boolKW,             def (monotype $ tFun fxPure posNil kwdNil tBool) NoDec),
                        (strKW,              def (monotype $ tFun fxPure posNil kwdNil tStr) NoDec)
                      ]
        

--  class $R (): pass
clR cls def         = cls [] [] []

--  class $Cont[X,P] (function[X,P,(),$R]):
--      pass
clCont cls def      = cls [quant x, quant p] [([Nothing],TC qnFunction [tVar x, tVar p, kwdNil, tR]), ([Nothing,Nothing],cStruct)] []
  where x           = TV KFX (name "X")
        p           = TV PRow (name "P")


--  $ASYNCf         : [A] => async($Actor, action()->A) -> Msg[A]
scASYNCf            = tSchema [quant a] tASYNC
  where tASYNC      = tFun fxAction (posRow tActor $ posRow tFun' posNil) kwdNil (tMsg $ tVar a)
        a           = TV KType $ name "A"
        tFun'       = tFun fxAsync posNil kwdNil (tVar a)

--  $AFTERf         : [A] => action(int, action()->A) -> Msg[A]
scAFTERf            = tSchema [quant a] tAFTER
  where tAFTER      = tFun fxAction (posRow tInt $ posRow tFun' posNil) kwdNil (tMsg $ tVar a)
        a           = TV KType $ name "A"
        tFun'       = tFun fxAction posNil kwdNil (tVar a)

--  $AWAITf         : [A] => action(Msg[A]) -> A
scAWAITf            = tSchema [quant a] tAWAIT
  where tAWAIT      = tFun fxAction (posRow (tMsg $ tVar a) posNil) kwdNil (tVar a)
        a           = TV KType $ name "T"


--  $ASYNCc         : [A] => mut($Actor, mut(mut(A)->$R)->$R) -> Msg[A]
scASYNCc            = tSchema [quant a] tASYNC
  where tASYNC      = tFun fxMut (posRow tActor $ posRow tCont' posNil) kwdNil (tMsg $ tVar a)
        a           = TV KType $ name "A"
        tCont'      = tFun fxMut (posRow tCont'' posNil) kwdNil tR
        tCont''     = tFun fxMut (posRow (tVar a) posNil) kwdNil tR

--  $AFTERc         : [A] => mut(int, mut(mut(A)->$R)->$R) -> Msg[A]
scAFTERc            = tSchema [quant a] tAFTER
  where tAFTER      = tFun fxMut (posRow tInt $ posRow tCont' posNil) kwdNil (tMsg $ tVar a)
        a           = TV KType $ name "A"
        tCont'      = tFun fxMut (posRow tCont'' posNil) kwdNil tR
        tCont''     = tFun fxMut (posRow (tVar a) posNil) kwdNil tR

--  $AWAITc         : [A] => mut(Msg[A], mut(A)->$R) -> $R
scAWAITc            = tSchema [quant a] tAWAIT
  where tAWAIT      = tFun fxMut (posRow (tMsg $ tVar a) $ posRow tCont' posNil) kwdNil tR
        a           = TV KType $ name "A"
        tCont'      = tFun fxMut (posRow (tVar a) posNil) kwdNil tR


--  $ASYNC          : [A] => mut($Actor, $Cont[mut,($Cont[mut,A],)]) -> Msg[A]
scASYNC             = tSchema [quant a] tASYNC
  where tASYNC      = tFun fxMut (posRow tActor $ posRow tCont' posNil) kwdNil (tMsg $ tVar a)
        a           = TV KType $ name "A"
        tCont'      = tCont fxMut tCont''
        tCont''     = tCont fxMut (tVar a)

--  $AFTER          : [A] => mut(int, $Cont[mut,($Cont[mut,A],)]) -> Msg[A]
scAFTER             = tSchema [quant a] tAFTER
  where tAFTER      = tFun fxMut (posRow tInt $ posRow tCont' posNil) kwdNil (tMsg $ tVar a)
        a           = TV KType $ name "A"
        tCont'      = tCont fxMut tCont''
        tCont''     = tCont fxMut (tVar a)

--  $AWAIT          : [A] => mut(Msg[A], $Cont[mut,(A,)]) -> $R
scAWAIT             = tSchema [quant a] tAWAIT
  where tAWAIT      = tFun fxMut (posRow (tMsg $ tVar a) $ posRow tCont' posNil) kwdNil tR
        a           = TV KType $ name "A"
        tCont'      = tCont fxMut (tVar a)



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
scASSERT            = tSchema [] tASSERT
  where tASSERT     = tFun fxPure (posRow tBool $ posRow (tOpt tStr) posNil) kwdNil tNone

--  $NEWACT         : pure ($Actor) -> None
scNEWACT            = tSchema [] tNEWACT
  where tNEWACT     = tFun fxPure (posRow tActor posNil) kwdNil tNone

--  $ISINSTANCE     : pure (struct,_) -> bool
scISINSTANCE        = tSchema [] tISINSTANCE
  where tISINSTANCE = tFun fxPure (posRow tStruct $ posRow tWild posNil) kwdNil tNone

--  $CAST           : [A,B] => (A) -> B
scCAST              = tSchema [quant a, quant b] tCAST
  where tCAST       = tFun fxPure (posRow (tVar a) posNil) kwdNil (tVar b)
        a           = TV KType $ name "A"
        b           = TV KType $ name "B"

--  $CONSTCONT      : [X,A] => (A, $Cont[X,A]) -> $Cont[X,tNone]
scCONSTCONT         = tSchema [quant x, quant a] tCONSTCONT
  where tCONSTCONT  = tFun fxPure (posRow (tVar a) $ posRow tCont' posNil) kwdNil tCont''
        tCont'      = tCont (tVar x) (tVar a)
        tCont''     = tCont (tVar x) tNone
        x           = TV KFX $ name "X"
        a           = TV KType $ name "A"

--  $FORMAT         : [P] => (str, (*P)) -> str
scFORMAT            = tSchema [quant p] tFORMAT
  where tFORMAT     = tFun fxPure (posRow tStr $ posRow (tTuple (tVar p) kwdNil) posNil) kwdNil tStr
        p           = TV KType $ name "P"

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

--  $Integral$Int$witness : Integral[int]
tIntegralInt        = tCon $ TC qnIntegral [tInt]


--  $ISNOTNONE      : [A] => pure (?A) -> bool
scISNOTNONE         = tSchema [quant a] tISNOTNONE
  where tISNOTNONE  = tFun fxPure (posRow (tOpt $ tVar a) posNil) kwdNil tBool
        a           = TV KType (name "A")

--  $SKIPRESc       : [X,A] => X(X(None)->$R) -> X(A)->$R
scSKIPRESc          = tSchema [quant x, quant a] tSKIPRES
  where tSKIPRES    = tFun (tVar x) (posRow tCont' posNil) kwdNil tCont''
        tCont'      = tFun (tVar x) (posRow tNone posNil) kwdNil tR
        tCont''     = tFun (tVar x) (posRow (tVar a) posNil) kwdNil tR
        x           = TV KFX $ name "X"
        a           = TV KType $ name "A"

--  $SKIPRES        : [X,A] => X($Cont[X,(None,)]) -> $Cont[X,(A,)]
scSKIPRES           = tSchema [quant x, quant a] tSKIPRES
  where tSKIPRES    = tFun (tVar x) (posRow tCont' posNil) kwdNil tCont''
        tCont'      = tCont (tVar x) tNone
        tCont''     = tCont (tVar x) (tVar a)
        x           = TV KFX $ name "X"
        a           = TV KType $ name "A"
