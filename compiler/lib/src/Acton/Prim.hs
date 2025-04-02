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
primTup0            = gBuiltin (name "Tup0")
primTup1            = gBuiltin (name "Tup1")
primTup2            = gBuiltin (name "Tup2")
primTup3            = gBuiltin (name "Tup3")

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

primPUSH_Cc         = gPrim "PUSH_Cc"
primPUSHF_Cc        = gPrim "PUSHF_Cc"
primPUSH_C          = gPrim "PUSH_C"
primPUSHF_C         = gPrim "PUSHF_C"
primPOP_C           = gPrim "POP_C"
primDROP_C          = gPrim "DROP_C"

primPUSH            = gPrim "PUSH"
primPUSHF           = gPrim "PUSHF"
primPOP             = gPrim "POP"
primDROP            = gPrim "DROP"
primRAISE           = gPrim "RAISE"

primSEQ             = gPrim "SEQ"
primBRK             = gPrim "BRK"
primCNT             = gPrim "CNT"
primRET             = gPrim "RET"

tSEQ                = tCon $ TC primSEQ []
tBRK                = tCon $ TC primBRK []
tCNT                = tCon $ TC primCNT []
tRET                = tCon $ TC primRET []

primASSERT          = gPrim "ASSERT"
primNEWACTOR        = gPrim "NEWACTOR"
primGCfinalizer     = gPrim "GCfinalizer"

primISINSTANCE      = gPrim "ISINSTANCE"
primISINSTANCE0     = gPrim "ISINSTANCE0"
primCONSTCONT       = gPrim "CONSTCONT"
primCAST            = gPrim "CAST"

eCAST t t' e        = eCall (tApp (eQVar primCAST) [t,t']) [e]

primFORMAT          = gPrim "FORMAT"

primRContc          = gPrim "R_CONTc"
primRCont           = gPrim "R_CONT"

primRFail           = gPrim "R_FAIL"

primEqOpt           = gPrim "EqOpt"
primShowOpt         = gPrim "ShowOpt"
primBoolOpt         = gPrim "BoolOpt"
primIdentityOpt     = gPrim "IdentityOpt"

primShowTup0        = gPrim "ShowTup0"
primShowTup1        = gPrim "ShowTup1"
primShowTup2        = gPrim "ShowTup2"
primShowTup3        = gPrim "ShowTup3"

primIdentityActor   = gPrim "IdentityActor"

primWEqNone         = gPrim "wEqNone"
primWIdentityNone   = gPrim "wIdentityNone"

witIntegralInt      = GName mPrim $ Derived (deriveQ qnIntegral) $ Derived (deriveQ qnInt) suffixWitness
witIntegralI64      = GName mPrim $ Derived (deriveQ qnIntegral) $ Derived (deriveQ qnI64) suffixWitness
witSequenceList     = GName mPrim $ Derived (deriveQ qnSequence) $ Derived (deriveQ qnList) suffixWitness
witCollectionList   = GName mPrim $ Derived (deriveQ qnCollection) $ Derived (deriveQ qnList) suffixWitness

primISNOTNONE       = gPrim "ISNOTNONE"
primISNOTNONE0      = gPrim "ISNOTNONE0"
primISNONE          = gPrim "ISNONE"
primISNONE0         = gPrim "ISNONE0"

primSKIPRESc        = gPrim "SKIPRESc"
primSKIPRES         = gPrim "SKIPRES"

primBox             = gPrim "Box"
tBox t              = tCon $ TC primBox [t]

valKW               = name "val"

cActor              = TC primActor []
tActor              = tCon cActor
tR                  = tCon $ TC primR []
tCont t             = tCon $ TC primCont [t]

primWrappedP        = gPrim "Wrapped"
pWrapped x y        = TC primWrappedP [x,y]

primWrappedC        = gPrim "WrappedC"
tWrapped s x y      = tCon $ TC primWrappedC [s,x,y]

attrVal             = name "val"
attrWrap            = name "wrap"
attrUnwrap          = name "unwrap"

primWrapProc        = gPrim "wWrapProc"
primWrapAction      = gPrim "wWrapAction"
primWrapMut         = gPrim "wWrapMut"
primWrapPure        = gPrim "wWrapPure"

primWRAP            = gPrim "WRAP"

primMkSet           = gPrim "mkSet"
primMkDict          = gPrim "mkDict"

primEnv             = [     (noq primASYNCf,        NDef scASYNCf NoDec),
                            (noq primAFTERf,        NDef scAFTERf NoDec),
                            (noq primAWAITf,        NDef scAWAITf NoDec),

                            (noq primASYNCc,        NDef scASYNCc NoDec),
                            (noq primAFTERc,        NDef scAFTERc NoDec),
                            (noq primAWAITc,        NDef scAWAITc NoDec),

                            (noq primASYNC,         NDef scASYNC NoDec),
                            (noq primAFTER,         NDef scAFTER NoDec),
                            (noq primAWAIT,         NDef scAWAIT NoDec),

                            (noq primPUSH_Cc,       NDef scPUSH_Cc NoDec),
                            (noq primPUSHF_Cc,      NDef scPUSHF_Cc NoDec),
                            (noq primPUSH_C,        NDef scPUSH_C NoDec),
                            (noq primPUSHF_C,       NDef scPUSHF_C NoDec),
                            (noq primPOP_C,         NDef scPOP_C NoDec),
                            (noq primDROP_C,        NDef scDROP_C NoDec),

                            (noq primPUSH,          NDef scPUSH NoDec),
                            (noq primPUSHF,         NDef scPUSHF NoDec),
                            (noq primPOP,           NDef scPOP NoDec),
                            (noq primDROP,          NDef scDROP NoDec),
                            (noq primRAISE,         NDef scRAISE NoDec),

                            (noq primSEQ,           clSEQ),
                            (noq primBRK,           clBRK),
                            (noq primCNT,           clCNT),
                            (noq primRET,           clRET),

                            (noq primASSERT,        NDef scASSERT NoDec),
                            (noq primNEWACTOR,      NDef scNEWACTOR NoDec),
                            (noq primGCfinalizer,   NDef scGCfinalizer NoDec),

                            (noq primISINSTANCE,    NDef scISINSTANCE NoDec),
                            (noq primISINSTANCE0,   NDef scISINSTANCE NoDec),
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

                            (noq primBox,           clBox),

                            (noq primRContc,        NDef scRContc NoDec),
                            (noq primRCont,         NDef scRCont NoDec),
                            (noq primRFail,         NDef scRFail NoDec),

                            (noq primEqOpt,         clEqOpt),
                            (noq primShowOpt,       clShowOpt),
                            (noq primBoolOpt,       clBoolOpt),
                            (noq primShowTup0,      clShowTup0),
                            (noq primShowTup1,      clShowTup1),
                            (noq primShowTup2,      clShowTup2),
                            (noq primShowTup3,      clShowTup3),
--                            (noq primTup0,          clTup0),
--                            (noq primTup1,          clTup1),
--                            (noq primTup2,          clTup2),
--                            (noq primTup3,          clTup3),
                            (noq primIdentityOpt,   clIdentityOpt),
                            (noq primIdentityActor, clIdentityActor),
                            

                            (noq primWEqNone,       NVar tEqNone),
                            (noq primWIdentityNone, NVar tIdentityNone),
                            (noq witIntegralInt,    NVar tIntegralInt),
                            (noq witSequenceList,   NVar tSequenceListWild),
                            (noq witCollectionList, NVar tCollectionListWild),

                            (noq primISNOTNONE,     NDef scISNOTNONE NoDec),
                            (noq primISNONE,        NDef scISNONE NoDec),
                            (noq primISNOTNONE0,    NDef scISNOTNONE NoDec),
                            (noq primISNONE0,       NDef scISNONE NoDec),

                            (noq primSKIPRESc,      NDef scSKIPRESc NoDec),
                            (noq primSKIPRES,       NDef scSKIPRES NoDec),

                            (noq primWrappedP,      proWrapped),
                            (noq primWrappedC,      clWrapped),
--                            (noq primWrapProc,      NVar $ tWrapped fxProc fxProc fxProc),
--                            (noq primWrapAction,    NVar $ tWrapped fxAction fxProc fxProc),

                            (noq primWRAP,          NDef scWRAP NoDec),

                            (noq primMkSet,         NDef scMkSet NoDec),
                            (noq primMkDict,        NDef scMkDict NoDec)
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

--  class $Box[A] (object, value):
--      ref         : A
--      __init__    : (A) -> None
clBox               = NClass [quant a] (leftpath [cObject, cValue]) te
  where te          = [ (valKW,  NSig (monotype $ tVar a) Property),
                        (initKW, NDef (monotype $ tFun fxPure (posRow (tVar a) posNil) kwdNil tNone) NoDec) ]
        a           = TV KType (name "A")


--  class $Actor (): pass
clActor             = NClass [] (leftpath [cValue]) te
  where te          = [ (primKW "next",       NSig (monotype tActor) Property),
                        (primKW "msg",        NSig (monotype (tMsg tWild)) Property),
                        (primKW "msg_tail",   NSig (monotype (tMsg tWild)) Property),
                        (primKW "msg_lock",   NSig (monotype $ tCon $ TC (gPrim "Lock") []) Property),
                        (primKW "affinity",   NSig (monotype $ tCon $ TC (gPrim "int64") []) Property),
                        (primKW "outgoing",   NSig (monotype (tMsg tWild)) Property),
                        (primKW "waitsfor",   NSig (monotype (tMsg tWild)) Property),
                        (primKW "consume_hd", NSig (monotype $ tCon $ TC (gPrim "int64") []) Property),
                        (primKW "catcher",    NSig (monotype $ tCon $ TC (gPrim "Catcher") []) Property),
                        (primKW "globkey",    NSig (monotype $ tCon $ TC (gPrim "long") []) Property),
--                        (boolKW,              NDef (monotype $ tFun fxPure posNil kwdNil tBool) NoDec),
--                        (strKW,               NDef (monotype $ tFun fxPure posNil kwdNil tStr) NoDec),
--                        (reprKW,              NDef (monotype $ tFun fxPure posNil kwdNil tStr) NoDec),
                        (resumeKW,            NDef (monotype $ tFun fxMut posNil kwdNil tNone) NoDec),
                        (cleanupKW,           NDef (monotype $ tFun fxMut posNil kwdNil tNone) NoDec)
                      ]

--  class $SEQ (BaseException, value):
--      __init__ : () -> None
clSEQ               = NClass [] (leftpath [cBaseException, cValue]) te
  where te          = [ (initKW, NSig (monotype $ tFun fxPure posNil kwdNil tNone) NoDec) ]

--  class $BRK (BaseException, value):
--      __init__ : () -> None
clBRK               = NClass [] (leftpath [cBaseException, cValue]) te
  where te          = [ (initKW, NSig (monotype $ tFun fxPure posNil kwdNil tNone) NoDec) ]

--  class $CNT (BaseException, value):
--      __init__ : () -> None
clCNT               = NClass [] (leftpath [cBaseException, cValue]) te
  where te          = [ (initKW, NSig (monotype $ tFun fxPure posNil kwdNil tNone) NoDec) ]

--  class $RET (BaseException, value):
--      @property
--      val      : value
--      __init__ : (value) -> None
clRET               = NClass [] (leftpath [cBaseException, cValue]) te
  where te          = [ (attrVal, NSig (monotype tValue) Property),
                        (initKW,  NSig (monotype $ tFun fxPure (posRow tValue posNil) kwdNil tNone) NoDec) ]


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



--  $PUSH_Cc        : pure (proc(bool)->$R) -> None
scPUSH_Cc           = tSchema [] tPUSH_C
  where tPUSH_C     = tFun fxPure (posRow tCont' posNil) kwdNil tNone
        tCont'      = tFun fxProc (posRow tBool posNil) kwdNil tR

--  $PUSH_FCc       : pure (proc(bool)->$R) -> None
scPUSHF_Cc          = tSchema [] tPUSHF_C
  where tPUSHF_C    = tFun fxPure (posRow tCont' posNil) kwdNil tNone
        tCont'      = tFun fxProc (posRow tBool posNil) kwdNil tR

--  $PUSH_C         : pure ($Cont[bool]) -> None
scPUSH_C            = tSchema [] tPUSH_C
  where tPUSH_C     = tFun fxPure (posRow (tCont tBool) posNil) kwdNil tNone

--  $PUSHF_C        : pure ($Cont[bool]) -> None
scPUSHF_C           = tSchema [] tPUSHF_C
  where tPUSHF_C    = tFun fxPure (posRow (tCont tBool) posNil) kwdNil tNone

--  $POP_C          : pure () -> BaseExceptiom
scPOP_C             = tSchema [] tPOP_C
  where tPOP_C      = tFun fxPure posNil kwdNil tBaseException

--  $DROP_C         : pure () -> None
scDROP_C            = tSchema [] tDROP_C
  where tDROP_C     = tFun fxPure posNil kwdNil tNone

--  $PUSH           : () -> bool
scPUSH              = tSchema [] tPUSH
  where tPUSH       = tFun fxPure posNil kwdNil tBool

--  $PUSHF          : () -> bool
scPUSHF             = tSchema [] tPUSHF
  where tPUSHF      = tFun fxPure posNil kwdNil tBool

--  $POP            : () -> BaseException
scPOP               = tSchema [] tPOP
  where tPOP        = tFun fxPure posNil kwdNil tBaseException

--  $DROP           : () -> None
scDROP              = tSchema [] tDROP
  where tDROP       = tFun fxPure posNil kwdNil tNone

--  $RAISE          : (BaseException) -> None
scRAISE             = tSchema [] tRAISE
  where tRAISE      = tFun fxPure (posRow tBaseException posNil) kwdNil tNone


--  $ASSERT         : pure (bool, ?str) -> None
scASSERT            = tSchema [] tASSERT
  where tASSERT     = tFun fxPure (posRow tBool $ posRow (tOpt tStr) posNil) kwdNil tNone

--  $NEWACTOR       : [A($Actor)] => pure () -> A
scNEWACTOR          = tSchema [Quant a [cActor]] tNEWACTOR
  where tNEWACTOR   = tFun fxPure posNil kwdNil (tVar a)
        a           = TV KType $ name "A"

--  $GCfinalizer    : [A($Actor)] => pure (A) -> None
scGCfinalizer       = tSchema [Quant a [cActor]] tGCfin
  where tGCfin      = tFun fxPure (posRow (tVar a) posNil) kwdNil tNone
        a           = TV KType $ name "A"

--  $ISINSTANCE     : pure (struct, _) -> bool
scISINSTANCE        = tSchema [] tISINSTANCE
  where tISINSTANCE = tFun fxPure (posRow tValue $ posRow tWild posNil) kwdNil tNone

--  $CAST           : [A, B] => (A) -> B
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

--  class $ShowOpt[A] (Show[?A]): pass
clShowOpt           =  NClass [quant a] (leftpath [TC qnShow [tOpt $ tVar a]]) clTEnv
  where clTEnv      = [ (initKW, NDef scInit NoDec) ]
        scInit      = tSchema [] $ tFun fxPure (posRow (tCon $ TC qnShow [tVar a]) posNil) kwdNil tNone
        a           = TV KType (name "A")

--  class $BoolOpt[A] (Bool[?A]): pass
clBoolOpt           =  NClass [quant a] (leftpath [TC qnBool [tOpt $ tVar a]]) clTEnv
  where clTEnv      = [ (initKW, NDef scInit NoDec) ]
        scInit      = tSchema [] $ tFun fxPure (posRow (tCon $ TC qnBool [tVar a]) posNil) kwdNil tNone
        a           = TV KType (name "A")

clShowTup0          =  NClass [] (leftpath [TC qnShow [tCon (TC primTup0 $ [])]]) clTEnv
  where clTEnv      = [ (initKW, NDef scInit NoDec) ]
        scInit      = tSchema [] $ tFun fxPure posNil kwdNil tNone

clShowTup1          =  NClass [quant a] (leftpath [TC qnShow [tCon (TC primTup1 $ [tVar a])]]) clTEnv
  where clTEnv      = [ (initKW, NDef scInit NoDec) ]
        scInit      = tSchema [] $ tFun fxPure (posRow (tCon $ TC qnShow [tVar a]) posNil) kwdNil tNone
        a           = TV KType (name "A")

clShowTup2          =  NClass [quant a, quant b] (leftpath [TC qnShow [tCon (TC primTup2 $ [tVar a, tVar b])]]) clTEnv
  where clTEnv      = [ (initKW, NDef scInit NoDec) ]
        scInit      = tSchema [] $ tFun fxPure (posRow (tCon $ TC qnShow [tVar a]) (posRow (tCon $ TC qnShow [tVar b]) posNil)) kwdNil tNone
        a           = TV KType (name "A")
        b           = TV KType (name "B")

clShowTup3          =  NClass [quant a, quant b, quant c] (leftpath [TC qnShow [tCon (TC primTup3 $ [tVar a, tVar b, tVar c])]]) clTEnv
  where clTEnv      = [ (initKW, NDef scInit NoDec) ]
        scInit      = tSchema [] $ tFun fxPure (posRow (tCon $ TC qnShow [tVar a]) (posRow (tCon $ TC qnShow [tVar b]) (posRow (tCon $ TC qnShow [tVar c]) posNil))) kwdNil tNone
        a           = TV KType (name "A")
        b           = TV KType (name "B")
        c           = TV KType (name "C")
{-
clTup0              =  NClass [] (leftpath [cValue]) clTEnv
  where clTEnv      = [ (initKW, NDef scInit NoDec) ]
        scInit      = tSchema [] $ tFun fxPure posNil kwdNil tNone

clTup1              =  NClass [quant a] (leftpath [cValue]) clTEnv
  where clTEnv      = [ (initKW, NDef scInit NoDec) ]
        scInit      = tSchema [] $ tFun fxPure (posRow (tVar a) posNil) kwdNil tNone
        a           = TV KType (name "A")

clTup2              =  NClass [quant a, quant b] (leftpath [cValue]) clTEnv
  where clTEnv      = [ (initKW, NDef scInit NoDec) ]
        scInit      = tSchema [] $ tFun fxPure (posRow (tVar a) (posRow (tVar b) posNil)) kwdNil tNone
        a           = TV KType (name "A")
        b           = TV KType (name "B")

clTup3              =  NClass [quant a, quant b, quant c] (leftpath [cValue]) clTEnv
  where clTEnv      = [ (initKW, NDef scInit NoDec) ]
        scInit      = tSchema [] $ tFun fxPure (posRow (tVar a) (posRow (tVar b) (posRow (tVar c) posNil))) kwdNil tNone
        a           = TV KType (name "A")
        b           = TV KType (name "B")
        c           = TV KType (name "C")
-}
--  class $IdentityOpt[A] (Identity[?A]): pass
clIdentityOpt       = NClass [quant a] (leftpath [TC qnIdentity [tOpt $ tVar a]]) []    -- methods not modelled
  where a           = TV KType (name "A")

--  class $IdentityActor (Identity[$Actor]): pass
clIdentityActor     = NClass [] (leftpath [TC qnIdentity [tActor]]) []                  -- methods not modelled

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


--  $MkSet          : [A] => (Hashable[A], set[A]) -> set[A]
scMkSet             = tSchema [quant a] tMkSet
  where tMkSet      = tFun fxPure (posRow tHashableA (posRow (tSet (tVar a)) posNil)) kwdNil (tSet (tVar a))
        tHashableA  = tCon (TC qnHashable [tVar a])
        a           = TV KType $ name "A"
       
--  $MkDict         : [A] => (Hashable[A], dict[A]) -> dict[A]
scMkDict            = tSchema [quant a, quant b] tMkDict
  where tMkDict     = tFun fxPure (posRow tHashableA (posRow (tDict (tVar a) (tVar b)) posNil)) kwdNil (tDict (tVar a)(tVar b))
        tHashableA  = tCon (TC qnHashable [tVar a])
        a           = TV KType $ name "A"
        b           = TV KType $ name "B"
       
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
        
isPUSH (Call _ (Var _ x) _ _)   = x `elem` [primPUSH,primPUSHF]
isPUSH _                        = False

isPUSHF (Call _ (Var _ x) _ _)  = x == primPUSHF
isPUSHF _                       = False

isRAISE (Call _ (Var _ x) _ _)  = x == primRAISE
isRAISE _                       = False
