-- Copyright (C) 2019-2021 Data Ductus AB
--
-- Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
--
-- 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
--
-- 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
--
-- 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

module Acton.Prim where

import Utils
import Pretty
import Acton.Syntax
import Acton.Builtin
import Acton.Names

nPrim               = name "$"
mPrim               = ModName [nPrim]
gPrim s             = GName mPrim (name s)

primKW s            = name ("$" ++ s)

primProc            = gPrim "proc"
primAction          = gPrim "action"
primMut             = gPrim "mut"
primPure            = gPrim "pure"

cProc r t           = TC primProc [r,t]
cAction r t         = TC primAction [r,t]
cMut r t            = TC primMut [r,t]
cPure r t           = TC primPure [r,t]

attr_call_          = name "__call__"
attr_exec_          = name "__exec__"
attr_asyn_          = name "__asyn__"
attr_eval_          = name "__eval__"

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
primRAISE           = gPrim "RAISE"
primASSERT          = gPrim "ASSERT"
primNEWACTOR        = gPrim "NEWACTOR"

primISINSTANCE      = gPrim "ISINSTANCE"
primCAST            = gPrim "CAST"
primCONSTCONT       = gPrim "CONSTCONT"

primFORMAT          = gPrim "FORMAT"

primRContc          = gPrim "R_CONTc"
primRCont           = gPrim "R_CONT"

primRFail           = gPrim "R_FAIL"

primEqOpt           = gPrim "EqOpt"
primIdentityOpt     = gPrim "IdentityOpt"

primWEqNone         = gPrim "wEqNone"
primWIdentityNone   = gPrim "wIdentityNone"

witIntegralInt      = GName mPrim $ Derived (deriveQ qnIntegral) $ Derived (deriveQ qnInt) suffixWitness
witIntegralI64      = GName mPrim $ Derived (deriveQ qnIntegral) $ Derived (deriveQ qnI64) suffixWitness
witSequenceList     = GName mPrim $ Derived (deriveQ qnSequence) $ Derived (deriveQ qnList) suffixWitness
witCollectionList   = GName mPrim $ Derived (deriveQ qnCollection) $ Derived (deriveQ qnList) suffixWitness

primISNOTNONE       = gPrim "ISNOTNONE"
primISNONE          = gPrim "ISNONE"

primSKIPRESc        = gPrim "SKIPRESc"
primSKIPRES         = gPrim "SKIPRES"

cActor              = TC primActor []
tActor              = tCon cActor
tR                  = tCon $ TC primR []
tCont t             = tCon $ TC primCont [t]

primWrappedP        = gPrim "Wrapped"
pWrapped x y        = TC primWrappedP [x,y]

primWrappedC        = gPrim "WrappedC"
tWrapped s x y      = tCon $ TC primWrappedC [s,x,y]

attrWrap            = name "wrap"
attrUnwrap          = name "unwrap"

primWrapProc        = gPrim "wWrapProc"
primWrapAction      = gPrim "wWrapAction"
primWrapMut         = gPrim "wWrapMut"
primWrapPure        = gPrim "wWrapPure"

primWRAP            = gPrim "WRAP"


primEnv             = [     (noq primASYNCf,        NDef scASYNCf NoDec),
                            (noq primAFTERf,        NDef scAFTERf NoDec),
                            (noq primAWAITf,        NDef scAWAITf NoDec),

                            (noq primASYNCc,        NDef scASYNCc NoDec),
                            (noq primAFTERc,        NDef scAFTERc NoDec),
                            (noq primAWAITc,        NDef scAWAITc NoDec),

                            (noq primASYNC,         NDef scASYNC NoDec),
                            (noq primAFTER,         NDef scAFTER NoDec),
                            (noq primAWAIT,         NDef scAWAIT NoDec),
                        
                            (noq primPUSHc,         NDef scPUSHc NoDec),
                            (noq primPUSH,          NDef scPUSH NoDec),
                        
                            (noq primPOP,           NDef scPOP NoDec),
                            (noq primRAISE,         NDef scRAISE NoDec),
                            (noq primASSERT,        NDef scASSERT NoDec),
                            (noq primNEWACTOR,      NDef scNEWACTOR NoDec),

                            (noq primISINSTANCE,    NDef scISINSTANCE NoDec),
                            (noq primCAST,          NDef scCAST NoDec),
                            (noq primCONSTCONT,     NDef scCONSTCONT NoDec),

                            (noq primFORMAT,        NDef scFORMAT NoDec),

                            (noq primProc,          clProc),
                            (noq primAction,        clAction),
                            (noq primMut,           clMut),
                            (noq primPure,          clPure),

                            (noq primActor,         clActor),
                            (noq primR,             clR),
                            (noq primCont,          clCont),

                            (noq primRContc,        NDef scRContc NoDec),
                            (noq primRCont,         NDef scRCont NoDec),
                            (noq primRFail,         NDef scRFail NoDec),

                            (noq primEqOpt,         clEqOpt),
                            (noq primIdentityOpt,   clIdentityOpt),

                            (noq primWEqNone,       NVar tEqNone),
                            (noq primWIdentityNone, NVar tIdentityNone),
                            (noq witIntegralInt,    NVar tIntegralInt),
                            (noq witSequenceList,   NVar tSequenceListWild),
                            (noq witCollectionList, NVar tCollectionListWild),

                            (noq primISNOTNONE,     NDef scISNOTNONE NoDec),
                            (noq primISNONE,        NDef scISNONE NoDec),

                            (noq primSKIPRESc,      NDef scSKIPRESc NoDec),
                            (noq primSKIPRES,       NDef scSKIPRES NoDec),

                            (noq primWrappedP,      proWrapped),
                            (noq primWrappedC,      clWrapped),
--                            (noq primWrapProc,      NVar $ tWrapped fxProc fxProc fxProc),
--                            (noq primWrapAction,    NVar $ tWrapped fxAction fxProc fxProc),

                            (noq primWRAP,          NDef scWRAP NoDec)
                      ]

tSequenceListWild   = tCon (TC qnSequence [tList tWild, tWild])
tCollectionListWild = tCon (TC qnCollection [tList tWild, tWild])

--  class $Cont[T] (value): pass
--      __call__    : proc(T) -> $R
clCont              = NClass [quant t] (leftpath [cValue]) te
  where te          = [ (attr_call_, NSig (monotype $ tFun fxProc (posRow (tVar t) posNil) kwdNil tR) NoDec) ]
        t           = TV KType (name "T")

--  class $proc[R,T] (value):
--      __call__    : proc($Cont[T], *R) -> $R
--      __exec__    : proc($Cont[value], *R) -> $R
clProc              = NClass [quant r, quant t] (leftpath [cValue]) te
  where te          = [ (attr_call_, NSig (monotype $ tFun fxProc (posRow (tVar t) (tVar r)) kwdNil tR) NoDec),
                        (attr_exec_, NSig (monotype $ tFun fxProc (posRow tValue (tVar r)) kwdNil tR) NoDec) ]
        r           = TV PRow (name "R")
        t           = TV KType (name "T")

--  class $action[R,T] ($proc[R,T], value):
--      __asyn__    : action(*R) -> T
clAction            = NClass [quant r, quant t] (leftpath [ cProc (tVar r) (tVar t), cValue]) te
  where te          = [ (attr_asyn_, NSig (monotype $ tFun fxAction (tVar r) kwdNil (tVar t)) NoDec) ]
        r           = TV PRow (name "R")
        t           = TV KType (name "T")


--  class $mut[R,T] ($proc[R,T], value):
--      __eval__    : mut(*R) -> T
clMut               = NClass [quant r, quant t] (leftpath [ cProc (tVar r) (tVar t), cValue]) te
  where te          = [ (attr_eval_, NSig (monotype $ tFun fxMut (tVar r) kwdNil (tVar t)) NoDec) ]
        r           = TV PRow (name "R")
        t           = TV KType (name "T")

--  class $pure[R,T] ($mut[R,T], $proc[R,T], value):
--      __eval__    : pure(*R) -> T
clPure              = NClass [quant r, quant t] (leftpath [ cMut (tVar r) (tVar t), cProc (tVar r) (tVar t), cValue]) te
  where te          = [ (attr_eval_, NSig (monotype $ tFun fxPure (tVar r) kwdNil (tVar t)) NoDec) ]
        r           = TV PRow (name "R")
        t           = TV KType (name "T")


--  class $Actor (): pass
clActor             = NClass [] (leftpath [cValue]) te
  where te          = [ (primKW "next",       NSig (monotype tActor) Property),
                        (primKW "msg",        NSig (monotype (tMsg tWild)) Property),
                        (primKW "outgoing",   NSig (monotype (tMsg tWild)) Property),
                        (primKW "waitsfor",   NSig (monotype (tMsg tWild)) Property),
                        (primKW "consume_hd", NSig (monotype $ tCon $ TC (gPrim "int64") []) Property),
                        (primKW "catcher",    NSig (monotype $ tCon $ TC (gPrim "Catcher") []) Property),
                        (primKW "msg_lock",   NSig (monotype $ tCon $ TC (gPrim "Lock") []) Property),
                        (primKW "globkey",    NSig (monotype $ tCon $ TC (gPrim "long") []) Property),
                        (primKW "affinity",   NSig (monotype $ tCon $ TC (gPrim "int64") []) Property),
                        (boolKW,              NDef (monotype $ tFun fxPure posNil kwdNil tBool) NoDec),
                        (strKW,               NDef (monotype $ tFun fxPure posNil kwdNil tStr) NoDec),
                        (reprKW,              NDef (monotype $ tFun fxPure posNil kwdNil tStr) NoDec),
                        (resumeKW,            NDef (monotype $ tFun fxMut posNil kwdNil tNone) NoDec)
                      ]
        

--  class $R (): pass
clR                 = NClass [] [] []

--  $ASYNCf         : [A] => action($Actor, proc()->A) -> A
scASYNCf            = tSchema [quant a] tASYNC
  where tASYNC      = tFun fxAction (posRow tActor $ posRow tFun' posNil) kwdNil (tVar a)
        a           = TV KType $ name "A"
        tFun'       = tFun fxProc posNil kwdNil (tVar a)

--  $AFTERf         : [A] => action(int, proc()->A) -> A
scAFTERf            = tSchema [quant a] tAFTER
  where tAFTER      = tFun fxAction (posRow tFloat $ posRow tFun' posNil) kwdNil (tVar a)
        a           = TV KType $ name "A"
        tFun'       = tFun fxProc posNil kwdNil (tVar a)

--  $AWAITf         : [A] => proc(Msg[A]) -> A
scAWAITf            = tSchema [quant a] tAWAIT
  where tAWAIT      = tFun fxProc (posRow (tMsg $ tVar a) posNil) kwdNil (tVar a)
        a           = TV KType $ name "T"


--  $ASYNCc         : [A] => action($Actor, proc(proc(A)->$R)->$R) -> A
scASYNCc            = tSchema [quant a] tASYNC
  where tASYNC      = tFun fxAction (posRow tActor $ posRow tCont' posNil) kwdNil (tVar a)
        a           = TV KType $ name "A"
        tCont'      = tFun fxProc (posRow tCont'' posNil) kwdNil tR
        tCont''     = tFun fxProc (posRow (tVar a) posNil) kwdNil tR

--  $AFTERc         : [A] => action(int, proc(proc(A)->$R)->$R) -> A
scAFTERc            = tSchema [quant a] tAFTER
  where tAFTER      = tFun fxAction (posRow tFloat $ posRow tCont' posNil) kwdNil (tVar a)
        a           = TV KType $ name "A"
        tCont'      = tFun fxProc (posRow tCont'' posNil) kwdNil tR
        tCont''     = tFun fxProc (posRow (tVar a) posNil) kwdNil tR

--  $AWAITc         : [A] => proc(proc(A)->$R, Msg[A]) -> $R
scAWAITc            = tSchema [quant a] tAWAIT
  where tAWAIT      = tFun fxProc (posRow tCont' $ posRow (tMsg $ tVar a) posNil) kwdNil tR
        a           = TV KType $ name "A"
        tCont'      = tFun fxProc (posRow (tVar a) posNil) kwdNil tR


--  $ASYNC          : [A] => action($Actor, $Cont[$Cont[A]]) -> A
scASYNC             = tSchema [quant a] tASYNC
  where tASYNC      = tFun fxAction (posRow tActor $ posRow tCont' posNil) kwdNil (tVar a)
        a           = TV KType $ name "A"
        tCont'      = tCont tCont''
        tCont''     = tCont (tVar a)

--  $AFTER          : [A] => action(int, $Cont[$Cont[A]]) -> A
scAFTER             = tSchema [quant a] tAFTER
  where tAFTER      = tFun fxAction (posRow tFloat $ posRow tCont' posNil) kwdNil (tVar a)
        a           = TV KType $ name "A"
        tCont'      = tCont tCont''
        tCont''     = tCont (tVar a)

--  $AWAIT          : [A] => proc($Cont[A], Msg[A]) -> $R
scAWAIT             = tSchema [quant a] tAWAIT
  where tAWAIT      = tFun fxProc (posRow tCont' $ posRow (tMsg $ tVar a) posNil) kwdNil tR
        a           = TV KType $ name "A"
        tCont'      = tCont (tVar a)



--  $PUSHc          : pure (proc(BaseException)->$R) -> None
scPUSHc             = tSchema [] tPUSH
  where tPUSH       = tFun fxPure (posRow tCont' posNil) kwdNil tNone
        tCont'      = tFun fxProc (posRow tBaseException posNil) kwdNil tR

--  $PUSH           : pure ($Cont[BaseException]) -> None
scPUSH              = tSchema [] tPUSH
  where tPUSH       = tFun fxPure (posRow tCont' posNil) kwdNil tNone
        a           = TV KType $ name "A"
        tCont'      = tCont tBaseException



--  $POP            : pure (int) -> None
scPOP               = tSchema [] tPOP
  where tPOP        = tFun fxPure (posRow tInt posNil) kwdNil tNone

--  $RAISE          : pure (BaseException) -> None
scRAISE             = tSchema [] tRAISE
  where tRAISE      = tFun fxPure (posRow tBaseException posNil) kwdNil tNone

--  $ASSERT         : pure (bool, ?str) -> None
scASSERT            = tSchema [] tASSERT
  where tASSERT     = tFun fxPure (posRow tBool $ posRow (tOpt tStr) posNil) kwdNil tNone

--  $NEWACTOR       : [A($Actor)] => pure () -> A
scNEWACTOR          = tSchema [Quant a [cActor]] tNEWACTOR
  where tNEWACTOR   = tFun fxPure posNil kwdNil (tVar a)
        a           = TV KType $ name "A"

--  $ISINSTANCE     : pure (struct,_) -> bool
scISINSTANCE        = tSchema [] tISINSTANCE
  where tISINSTANCE = tFun fxPure (posRow tValue $ posRow tWild posNil) kwdNil tNone

--  $CAST           : [A,B] => (A) -> B
scCAST              = tSchema [quant a, quant b] tCAST
  where tCAST       = tFun fxPure (posRow (tVar a) posNil) kwdNil (tVar b)
        a           = TV KType $ name "A"
        b           = TV KType $ name "B"

--  $CONSTCONT      : [A] => (A, $Cont[A]) -> $Cont[tNone]
scCONSTCONT         = tSchema [quant a] tCONSTCONT
  where tCONSTCONT  = tFun fxPure (posRow (tVar a) $ posRow tCont' posNil) kwdNil tCont''
        tCont'      = tCont (tVar a)
        tCont''     = tCont tNone
        a           = TV KType $ name "A"

--  $FORMAT         : [P] => (str, (*P)) -> str
scFORMAT            = tSchema [quant p] tFORMAT
  where tFORMAT     = tFun fxPure (posRow tStr $ posRow (tTuple (tVar p) kwdNil) posNil) kwdNil tStr
        p           = TV KType $ name "P"

--  $R_CONTc        : [A] => proc(proc(A)->$R, A) -> $R
scRContc            = tSchema [quant a] tRCont
  where tRCont      = tFun fxProc (posRow tCont' $ posRow (tVar a) posNil) kwdNil tR
        tCont'      = tFun fxProc (posRow (tVar a) posNil) kwdNil tR
        a           = TV KType $ name "A"

--  $R_CONT         : [A] => proc($Cont[A], A) -> $R
scRCont             = tSchema [quant a] tRCont
  where tRCont      = tFun fxProc (posRow tCont' $ posRow (tVar a) posNil) kwdNil tR
        tCont'      = tCont (tVar a)
        a           = TV KType $ name "A"

--  $R_FAIL         : proc(BaseException) -> $R
scRFail             = tSchema [] tRFail
  where tRFail      = tFun fxProc (posRow tBaseException posNil) kwdNil tR


--  class $EqOpt[A] (Eq[?A]): pass
clEqOpt             = NClass [quant a] (leftpath [TC qnEq [tOpt $ tVar a]]) clTEnv
  where clTEnv      = [ (initKW, NDef scInit NoDec) ]
        scInit      = tSchema [] $ tFun fxPure (posRow (tCon $ TC qnEq [tVar a]) posNil) kwdNil tNone
        a           = TV KType (name "A")

--  class $EqOpt[A] (Eq[?A]): pass
clIdentityOpt       = NClass [quant a] (leftpath [TC qnIdentity [tOpt $ tVar a]]) []
  where a           = TV KType (name "A")

--  w$EqNone        : Eq[None]
tEqNone             = tCon $ TC qnEq [tNone]

--  w$IdentityNone  : Identity[None]
tIdentityNone       = tCon $ TC qnIdentity [tNone]

--  $Integral$Int$witness : Integral[int]
tIntegralInt        = tCon $ TC qnIntegral [tInt]

--  $Integral$I64$witness : Integral[i64]
tIntegralI64        = tCon $ TC qnIntegral [tI64]


--  $ISNOTNONE      : [A] => pure (?A) -> bool
scISNOTNONE         = tSchema [quant a] tISNOTNONE
  where tISNOTNONE  = tFun fxPure (posRow (tOpt $ tVar a) posNil) kwdNil tBool
        a           = TV KType (name "A")

--  $ISNONE         : [A] => pure (?A) -> bool
scISNONE            = tSchema [quant a] tISNONE
  where tISNONE     = tFun fxPure (posRow (tOpt $ tVar a) posNil) kwdNil tBool
        a           = TV KType (name "A")

--  $SKIPRESc       : [A] => pure(proc(None)->$R) -> proc(A)->$R
scSKIPRESc          = tSchema [quant a] tSKIPRES
  where tSKIPRES    = tFun fxPure (posRow tCont' posNil) kwdNil tCont''
        tCont'      = tFun fxProc (posRow tNone posNil) kwdNil tR
        tCont''     = tFun fxProc (posRow (tVar a) posNil) kwdNil tR
        a           = TV KType $ name "A"

--  $SKIPRES        : [X,A] => pure($Cont[None]) -> $Cont[A]
scSKIPRES           = tSchema [quant a] tSKIPRES
  where tSKIPRES    = tFun fxPure (posRow tCont' posNil) kwdNil tCont''
        tCont'      = tCont tNone
        tCont''     = tCont (tVar a)
        a           = TV KType $ name "A"

--  $WRAP           : [A,B,C] => ($Actor, proc(*A,**B)->C) -> action(*A,**B)->C
scWRAP              = tSchema [quant a, quant b, quant c] tWRAP
  where tWRAP       = tFun0 [tActor, abcFun fxProc] (abcFun fxAction)
        abcFun fx   = tFun fx (tVar a) (tVar b) (tVar c)
        a           = TV KType (name "A")
        b           = TV KType (name "B")
        c           = TV KType (name "C")

--  protocol $Wrapped[X,Y]:
--      @static
--      wrap        : [A,B,C] => ($Actor, X(*A,**B)->C) -> Self(*A,**B)->C
--      @static
--      unwrap      : [A,B,C] => (Self(*A,**B)->C) -> Y(*A,**B)->C
proWrapped          = NProto [quant x, quant y] [] te
  where te          = [(attrWrap,scWrap), (attrUnwrap,scUnwrap)]
        scWrap      = NSig (tSchema q (tFun0 [tActor, fxFun tX] (fxFun tSelf)))  Static
        scUnwrap    = NSig (tSchema q (tFun0 [fxFun tSelf] (fxFun tY))) Static
        fxFun fx    = tFun fx (tVar a) (tVar b) (tVar c)
        tX          = tVar x
        tY          = tVar y
        tSelf       = tVar fxSelf
        q           = [quant a, quant b, quant c]
        x           = TV KFX (name "X")
        y           = TV KFX (name "Y")
        a           = TV PRow (name "A")
        b           = TV KRow (name "B")
        c           = TV KType (name "C")

--  class $WrappedC[S,X,Y]: pass
--      wrap        : [A,B,C] => ($Actor, X(*A,**B)->C) -> S(*A,**B)->C
--      unwrap      : [A,B,C] => (S(*A,**B)->C) -> X(*A,**B)->C
clWrapped           = NClass [quant s, quant x, quant y] [] te
  where te          = [(attrWrap,scWrap), (attrUnwrap,scUnwrap)]
        scWrap      = NDef (tSchema q (tFun0 [tActor, fxFun tX] (fxFun tS))) NoDec
        scUnwrap    = NDef (tSchema q (tFun0 [fxFun tS] (fxFun tY))) NoDec
        fxFun fx    = tFun fx (tVar a) (tVar b) (tVar c)
        tS          = tVar s
        tX          = tVar x
        tY          = tVar y
        q           = [quant a, quant b, quant c]
        s           = TV KFX (name "S")
        x           = TV KFX (name "X")
        y           = TV KFX (name "Y")
        a           = TV PRow (name "A")
        b           = TV KRow (name "B")
        c           = TV KType (name "C")



primWits            = [ WInst []        fxAction (pWrapped fxProc fxProc)   primWrapAction path,
                        WInst []        fxProc   (pWrapped fxProc fxProc)   primWrapProc path,
                        WInst []        fxMut    (pWrapped fxMut  fxMut)    primWrapMut path,
                        WInst [quant y] fxPure   (pWrapped fxPure (tVar y)) primWrapPure path
                      ]
  where path        = [Left (noQ "_")]
        y           = TV KFX (name "Y")
        
