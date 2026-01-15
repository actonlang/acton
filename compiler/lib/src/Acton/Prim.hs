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
import Acton.NameInfo

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
primInstallFinalizer = gPrim "InstallFinalizer"

attr_finalizer      = name "GCfinalizer"

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

primIdentityActor   = gPrim "IdentityActor"

primWEqNone         = gPrim "wEqNone"

witIntegralInt      = GName mPrim $ Derived (deriveQ qnIntegral) $ Derived (deriveQ qnInt) suffixWitness
witIntegralBigint  = GName mPrim $ Derived (deriveQ qnIntegral) $ Derived (deriveQ qnBigint) suffixWitness

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
primAnnot           = gPrim "annot"

primUGetItem        = gPrim "listD_U__getitem__"
primUNext           = gPrim "rangeD_U__next__"

annot t_ann ann t e = eCall (tApp (eQVar primAnnot) [t_ann, t]) [ann, e]

unAnnot t_ann (Call _ (TApp _ (Var _ n) [t,_]) (PosArg w (PosArg e PosNil)) KwdNil)
  | n == primAnnot && t == t_ann    = (w, e)

primEnv             = [     (noq primASYNCf,        NDef scASYNCf NoDec Nothing),
                            (noq primAFTERf,        NDef scAFTERf NoDec Nothing),
                            (noq primAWAITf,        NDef scAWAITf NoDec Nothing),

                            (noq primASYNCc,        NDef scASYNCc NoDec Nothing),
                            (noq primAFTERc,        NDef scAFTERc NoDec Nothing),
                            (noq primAWAITc,        NDef scAWAITc NoDec Nothing),

                            (noq primASYNC,         NDef scASYNC NoDec Nothing),
                            (noq primAFTER,         NDef scAFTER NoDec Nothing),
                            (noq primAWAIT,         NDef scAWAIT NoDec Nothing),

                            (noq primPUSH_Cc,       NDef scPUSH_Cc NoDec Nothing),
                            (noq primPUSHF_Cc,      NDef scPUSHF_Cc NoDec Nothing),
                            (noq primPUSH_C,        NDef scPUSH_C NoDec Nothing),
                            (noq primPUSHF_C,       NDef scPUSHF_C NoDec Nothing),
                            (noq primPOP_C,         NDef scPOP_C NoDec Nothing),
                            (noq primDROP_C,        NDef scDROP_C NoDec Nothing),

                            (noq primPUSH,          NDef scPUSH NoDec Nothing),
                            (noq primPUSHF,         NDef scPUSHF NoDec Nothing),
                            (noq primPOP,           NDef scPOP NoDec Nothing),
                            (noq primDROP,          NDef scDROP NoDec Nothing),
                            (noq primRAISE,         NDef scRAISE NoDec Nothing),

                            (noq primSEQ,           clSEQ),
                            (noq primBRK,           clBRK),
                            (noq primCNT,           clCNT),
                            (noq primRET,           clRET),

                            (noq primASSERT,        NDef scASSERT NoDec Nothing),
                            (noq primNEWACTOR,      NDef scNEWACTOR NoDec Nothing),
                            (noq primInstallFinalizer, NDef scGCfinalizer NoDec Nothing),

                            (noq primISINSTANCE,    NDef scISINSTANCE NoDec Nothing),
                            (noq primISINSTANCE0,   NDef scISINSTANCE NoDec Nothing),
                            (noq primCAST,          NDef scCAST NoDec Nothing),
                            (noq primCONSTCONT,     NDef scCONSTCONT NoDec Nothing),

                            (noq primFORMAT,        NDef scFORMAT NoDec Nothing),

                            (noq primProc,          clProc),
                            (noq primAction,        clAction),
                            (noq primMut,           clMut),
                            (noq primPure,          clPure),

                            (noq primActor,         clActor),
                            (noq primR,             clR),
                            (noq primCont,          clCont),

                            (noq primBox,           clBox),

                            (noq primRContc,        NDef scRContc NoDec Nothing),
                            (noq primRCont,         NDef scRCont NoDec Nothing),
                            (noq primRFail,         NDef scRFail NoDec Nothing),

                            (noq primEqOpt,         clEqOpt),
                            (noq primIdentityActor, clIdentityActor),

                            (noq primWEqNone,       NVar tEqNone),
                            (noq witIntegralInt,    NVar tIntegralInt),

                            (noq primISNOTNONE,     NDef scISNOTNONE NoDec Nothing),
                            (noq primISNONE,        NDef scISNONE NoDec Nothing),
                            (noq primISNOTNONE0,    NDef scISNOTNONE NoDec Nothing),
                            (noq primISNONE0,       NDef scISNONE NoDec Nothing),

                            (noq primSKIPRESc,      NDef scSKIPRESc NoDec Nothing),
                            (noq primSKIPRES,       NDef scSKIPRES NoDec Nothing),

                            (noq primWrappedP,      proWrapped),
                            (noq primWrappedC,      clWrapped),
--                            (noq primWrapProc,      NVar $ tWrapped fxProc fxProc fxProc),
--                            (noq primWrapAction,    NVar $ tWrapped fxAction fxProc fxProc),

                            (noq primWRAP,          NDef scWRAP NoDec Nothing),

                            (noq primMkSet,         NDef scMkSet NoDec Nothing),
                            (noq primMkDict,        NDef scMkDict NoDec Nothing),
                            (noq primAnnot,         NDef scAnnot NoDec Nothing),
                            (noq primUGetItem,      NDef scUGetItem NoDec Nothing),
                            (noq primUNext,         NDef scUNext NoDec Nothing)
                      ]

--  class $Cont[T] (value): pass
--      __call__    : proc(T) -> $R
clCont              = NClass [qbind t] (leftpath [cValue]) te Nothing
  where te          = [ (attr_call_, NSig (monotype $ tFun fxProc (posRow (tVar t) posNil) kwdNil tR) NoDec Nothing) ]
        t           = TV KType (name "T")

--  class $proc[R,T] (value):
--      __call__    : proc($Cont[T], *R) -> $R
--      __exec__    : proc($Cont[value], *R) -> $R
clProc              = NClass [qbind r, qbind t] (leftpath [cValue]) te Nothing
  where te          = [ (attr_call_, NSig (monotype $ tFun fxProc (posRow (tVar t) (tVar r)) kwdNil tR) NoDec Nothing),
                        (attr_exec_, NSig (monotype $ tFun fxProc (posRow tValue (tVar r)) kwdNil tR) NoDec Nothing) ]
        r           = TV PRow (name "R")
        t           = TV KType (name "T")

--  class $action[R,T] ($proc[R,T], value):
--      __asyn__    : action(*R) -> T
clAction            = NClass [qbind r, qbind t] (leftpath [ cProc (tVar r) (tVar t), cValue]) te Nothing
  where te          = [ (attr_asyn_, NSig (monotype $ tFun fxAction (tVar r) kwdNil (tVar t)) NoDec Nothing) ]
        r           = TV PRow (name "R")
        t           = TV KType (name "T")


--  class $mut[R,T] ($proc[R,T], value):
--      __eval__    : mut(*R) -> T
clMut               = NClass [qbind r, qbind t] (leftpath [ cProc (tVar r) (tVar t), cValue]) te Nothing
  where te          = [ (attr_eval_, NSig (monotype $ tFun fxMut (tVar r) kwdNil (tVar t)) NoDec Nothing) ]
        r           = TV PRow (name "R")
        t           = TV KType (name "T")

--  class $pure[R,T] ($mut[R,T], $proc[R,T], value):
--      __eval__    : pure(*R) -> T
clPure              = NClass [qbind r, qbind t] (leftpath [ cMut (tVar r) (tVar t), cProc (tVar r) (tVar t), cValue]) te Nothing
  where te          = [ (attr_eval_, NSig (monotype $ tFun fxPure (tVar r) kwdNil (tVar t)) NoDec Nothing) ]
        r           = TV PRow (name "R")
        t           = TV KType (name "T")

--  class $Box[A] (object, value):
--      ref         : A
--      __init__    : (A) -> None
clBox               = NClass [qbind a] (leftpath [cObject, cValue]) te Nothing
  where te          = [ (valKW,  NSig (monotype $ tVar a) Property Nothing),
                        (initKW, NDef (monotype $ tFun fxPure (posRow (tVar a) posNil) kwdNil tNone) NoDec Nothing) ]
        a           = TV KType (name "A")


--  class $Actor (): pass
clActor             = NClass [] (leftpath [cValue]) te Nothing
  where te          = [ (primKW "next",       NSig (monotype tActor) Property Nothing),
                        (primKW "msg",        NSig (monotype (tMsg tWild)) Property Nothing),
                        (primKW "msg_tail",   NSig (monotype (tMsg tWild)) Property Nothing),
                        (primKW "msg_lock",   NSig (monotype $ tCon $ TC (gPrim "Lock") []) Property Nothing),
                        (primKW "affinity",   NSig (monotype $ tCon $ TC (gPrim "int64") []) Property Nothing),
                        (primKW "outgoing",   NSig (monotype (tMsg tWild)) Property Nothing),
                        (primKW "waitsfor",   NSig (monotype (tMsg tWild)) Property Nothing),
                        (primKW "consume_hd", NSig (monotype $ tCon $ TC (gPrim "int64") []) Property Nothing),
                        (primKW "catcher",    NSig (monotype $ tCon $ TC (gPrim "Catcher") []) Property Nothing),
                        (primKW "globkey",    NSig (monotype $ tCon $ TC (gPrim "long") []) Property Nothing),
                        (boolKW,              NDef (monotype $ tFun fxPure posNil kwdNil tBool) NoDec Nothing),
                        (strKW,               NDef (monotype $ tFun fxPure posNil kwdNil tStr) NoDec Nothing),
                        (reprKW,              NDef (monotype $ tFun fxPure posNil kwdNil tStr) NoDec Nothing),
                        (resumeKW,            NDef (monotype $ tFun fxMut posNil kwdNil tNone) NoDec Nothing),
                        (cleanupKW,           NDef (monotype $ tFun fxMut posNil kwdNil tNone) NoDec Nothing)
                      ]

--  class $SEQ (BaseException, value):
--      __init__ : () -> None
clSEQ               = NClass [] (leftpath [cBaseException, cValue]) te Nothing
  where te          = [ (initKW, NSig (monotype $ tFun fxPure posNil kwdNil tNone) NoDec Nothing) ]

--  class $BRK (BaseException, value):
--      __init__ : () -> None
clBRK               = NClass [] (leftpath [cBaseException, cValue]) te Nothing
  where te          = [ (initKW, NSig (monotype $ tFun fxPure posNil kwdNil tNone) NoDec Nothing) ]

--  class $CNT (BaseException, value):
--      __init__ : () -> None
clCNT               = NClass [] (leftpath [cBaseException, cValue]) te Nothing
  where te          = [ (initKW, NSig (monotype $ tFun fxPure posNil kwdNil tNone) NoDec Nothing) ]

--  class $RET (BaseException, value):
--      @property
--      val      : value
--      __init__ : (value) -> None
clRET               = NClass [] (leftpath [cBaseException, cValue]) te Nothing
  where te          = [ (attrVal, NSig (monotype tValue) Property Nothing),
                        (initKW,  NSig (monotype $ tFun fxPure (posRow tValue posNil) kwdNil tNone) NoDec Nothing) ]


--  class $R (): pass
clR                 = NClass [] [] [] Nothing

--  $ASYNCf         : [A] => action($Actor, proc()->A) -> A
scASYNCf            = tSchema [qbind a] tASYNC
  where tASYNC      = tFun fxAction (posRow tActor $ posRow tFun' posNil) kwdNil (tVar a)
        a           = TV KType $ name "A"
        tFun'       = tFun fxProc posNil kwdNil (tVar a)

--  $AFTERf         : [A] => action(int, proc()->A) -> A
scAFTERf            = tSchema [qbind a] tAFTER
  where tAFTER      = tFun fxAction (posRow tFloat $ posRow tFun' posNil) kwdNil (tVar a)
        a           = TV KType $ name "A"
        tFun'       = tFun fxProc posNil kwdNil (tVar a)

--  $AWAITf         : [A] => proc(Msg[A]) -> A
scAWAITf            = tSchema [qbind a] tAWAIT
  where tAWAIT      = tFun fxProc (posRow (tMsg $ tVar a) posNil) kwdNil (tVar a)
        a           = TV KType $ name "T"


--  $ASYNCc         : [A] => action($Actor, proc(proc(A)->$R)->$R) -> A
scASYNCc            = tSchema [qbind a] tASYNC
  where tASYNC      = tFun fxAction (posRow tActor $ posRow tCont' posNil) kwdNil (tVar a)
        a           = TV KType $ name "A"
        tCont'      = tFun fxProc (posRow tCont'' posNil) kwdNil tR
        tCont''     = tFun fxProc (posRow (tVar a) posNil) kwdNil tR

--  $AFTERc         : [A] => action(int, proc(proc(A)->$R)->$R) -> A
scAFTERc            = tSchema [qbind a] tAFTER
  where tAFTER      = tFun fxAction (posRow tFloat $ posRow tCont' posNil) kwdNil (tVar a)
        a           = TV KType $ name "A"
        tCont'      = tFun fxProc (posRow tCont'' posNil) kwdNil tR
        tCont''     = tFun fxProc (posRow (tVar a) posNil) kwdNil tR

--  $AWAITc         : [A] => proc(proc(A)->$R, Msg[A]) -> $R
scAWAITc            = tSchema [qbind a] tAWAIT
  where tAWAIT      = tFun fxProc (posRow tCont' $ posRow (tMsg $ tVar a) posNil) kwdNil tR
        a           = TV KType $ name "A"
        tCont'      = tFun fxProc (posRow (tVar a) posNil) kwdNil tR


--  $ASYNC          : [A] => action($Actor, $Cont[$Cont[A]]) -> A
scASYNC             = tSchema [qbind a] tASYNC
  where tASYNC      = tFun fxAction (posRow tActor $ posRow tCont' posNil) kwdNil (tVar a)
        a           = TV KType $ name "A"
        tCont'      = tCont tCont''
        tCont''     = tCont (tVar a)

--  $AFTER          : [A] => action(int, $Cont[$Cont[A]]) -> A
scAFTER             = tSchema [qbind a] tAFTER
  where tAFTER      = tFun fxAction (posRow tFloat $ posRow tCont' posNil) kwdNil (tVar a)
        a           = TV KType $ name "A"
        tCont'      = tCont tCont''
        tCont''     = tCont (tVar a)

--  $AWAIT          : [A] => proc($Cont[A], Msg[A]) -> $R
scAWAIT             = tSchema [qbind a] tAWAIT
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
scNEWACTOR          = tSchema [QBind a [cActor]] tNEWACTOR
  where tNEWACTOR   = tFun fxPure posNil kwdNil (tVar a)
        a           = TV KType $ name "A"

--  $GCfinalizer    : [A($Actor)] => pure (A) -> None
scGCfinalizer       = tSchema [QBind a [cActor]] tGCfin
  where tGCfin      = tFun fxPure (posRow (tVar a) posNil) kwdNil tNone
        a           = TV KType $ name "A"

--  $ISINSTANCE     : pure (struct, _) -> bool
scISINSTANCE        = tSchema [] tISINSTANCE
  where tISINSTANCE = tFun fxPure (posRow tValue $ posRow tWild posNil) kwdNil tNone

--  $CAST           : [A, B] => (A) -> B
scCAST              = tSchema [qbind a, qbind b] tCAST
  where tCAST       = tFun fxPure (posRow (tVar a) posNil) kwdNil (tVar b)
        a           = TV KType $ name "A"
        b           = TV KType $ name "B"

--  $CONSTCONT      : [A] => (A, $Cont[A]) -> $Cont[tNone]
scCONSTCONT         = tSchema [qbind a] tCONSTCONT
  where tCONSTCONT  = tFun fxPure (posRow (tVar a) $ posRow tCont' posNil) kwdNil tCont''
        tCont'      = tCont (tVar a)
        tCont''     = tCont tNone
        a           = TV KType $ name "A"

--  $FORMAT         : [P] => (str, (*P)) -> str
scFORMAT            = tSchema [qbind p] tFORMAT
  where tFORMAT     = tFun fxPure (posRow tStr $ posRow (tTuple (tVar p) kwdNil) posNil) kwdNil tStr
        p           = TV KType $ name "P"

--  $R_CONTc        : [A] => proc(proc(A)->$R, A) -> $R
scRContc            = tSchema [qbind a] tRCont
  where tRCont      = tFun fxProc (posRow tCont' $ posRow (tVar a) posNil) kwdNil tR
        tCont'      = tFun fxProc (posRow (tVar a) posNil) kwdNil tR
        a           = TV KType $ name "A"

--  $R_CONT         : [A] => proc($Cont[A], A) -> $R
scRCont             = tSchema [qbind a] tRCont
  where tRCont      = tFun fxProc (posRow tCont' $ posRow (tVar a) posNil) kwdNil tR
        tCont'      = tCont (tVar a)
        a           = TV KType $ name "A"

--  $R_FAIL         : proc(BaseException) -> $R
scRFail             = tSchema [] tRFail
  where tRFail      = tFun fxProc (posRow tBaseException posNil) kwdNil tR


--  class $EqOpt[A] (Eq[?A]): pass
clEqOpt             = NClass [qbind a] (leftpath [TC qnEq [tOpt $ tVar a]]) clTEnv Nothing
  where clTEnv      = [ (initKW, NDef scInit NoDec Nothing) ]
        scInit      = tSchema [] $ tFun fxPure (posRow (tCon $ TC qnEq [tVar a]) posNil) kwdNil tNone
        a           = TV KType (name "A")

--  class $IdentityActor (Identity[$Actor]): pass
clIdentityActor     = NClass [] (leftpath [TC qnIdentity [tActor]]) [] Nothing                 -- methods not modelled

--  w$EqNone        : Eq[None]
tEqNone             = tCon $ TC qnEq [tNone]

--  $Integral$Int$witness : Integral[int]
tIntegralInt        = tCon $ TC qnIntegral [tInt]

--  $Integral$Bigint$witness : Integral[bigint]
tIntegralBigint     = tCon $ TC qnIntegral [tBigint]


--  $ISNOTNONE      : [A] => pure (?A) -> bool
scISNOTNONE         = tSchema [qbind a] tISNOTNONE
  where tISNOTNONE  = tFun fxPure (posRow (tOpt $ tVar a) posNil) kwdNil tBool
        a           = TV KType (name "A")

--  $ISNONE         : [A] => pure (?A) -> bool
scISNONE            = tSchema [qbind a] tISNONE
  where tISNONE     = tFun fxPure (posRow (tOpt $ tVar a) posNil) kwdNil tBool
        a           = TV KType (name "A")

--  $SKIPRESc       : [A] => pure(proc(None)->$R) -> proc(A)->$R
scSKIPRESc          = tSchema [qbind a] tSKIPRES
  where tSKIPRES    = tFun fxPure (posRow tCont' posNil) kwdNil tCont''
        tCont'      = tFun fxProc (posRow tNone posNil) kwdNil tR
        tCont''     = tFun fxProc (posRow (tVar a) posNil) kwdNil tR
        a           = TV KType $ name "A"

--  $SKIPRES        : [X,A] => pure($Cont[None]) -> $Cont[A]
scSKIPRES           = tSchema [qbind a] tSKIPRES
  where tSKIPRES    = tFun fxPure (posRow tCont' posNil) kwdNil tCont''
        tCont'      = tCont tNone
        tCont''     = tCont (tVar a)
        a           = TV KType $ name "A"


--  $MkSet          : [A] => (Hashable[A], set[A]) -> set[A]
scMkSet             = tSchema [qbind a] tMkSet
  where tMkSet      = tFun fxPure (posRow tHashableA (posRow (tSet (tVar a)) posNil)) kwdNil (tSet (tVar a))
        tHashableA  = tCon (TC qnHashable [tVar a])
        a           = TV KType $ name "A"

--  $MkDict         : [A] => (Hashable[A], dict[A]) -> dict[A]
scMkDict            = tSchema [qbind a, qbind b] tMkDict
  where tMkDict     = tFun fxPure (posRow tHashableA (posRow (tDict (tVar a) (tVar b)) posNil)) kwdNil (tDict (tVar a)(tVar b))
        tHashableA  = tCon (TC qnHashable [tVar a])
        a           = TV KType $ name "A"
        b           = TV KType $ name "B"

--  $annot : [A,B] => (A,B) -> B
scAnnot             = tSchema [qbind a, qbind b] tAnnot
  where tAnnot      = tFun fxPure (posRow (tVar a) (posRow (tVar b) posNil)) kwdNil (tVar b)
        a           = TV KType $ name "A"
        b           = TV KType $ name "B"

-- $listD_U__getitem__ : [A] => (list[A], int) -> A
scUGetItem          = tSchema [qbind a] tUGetItem
  where tUGetItem   = tFun fxPure (posRow (tList (tVar a)) (posRow tInt posNil)) kwdNil (tVar a)
        a           = TV KType $ name "A"

-- $rangeD_U__next__ : [A] => (Iterator[A]) -> A   will only be used to replace B_IteratorD_range.__next__
scUNext             = tSchema [] tUNext
  where tUNext      = tFun fxMut (posRow (tIterator (tInt)) posNil) kwdNil tInt
        
--  $WRAP           : [A,B,C] => ($Actor, proc(*A,**B)->C) -> action(*A,**B)->C
scWRAP              = tSchema [qbind a, qbind b, qbind c] tWRAP
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
proWrapped          = NProto [qbind x, qbind y] [] te Nothing
  where te          = [(attrWrap,scWrap), (attrUnwrap,scUnwrap)]
        scWrap      = NSig (tSchema q (tFun0 [tActor, fxFun tX] (fxFun tSelf)))  Static Nothing
        scUnwrap    = NSig (tSchema q (tFun0 [fxFun tSelf] (fxFun tY))) Static Nothing
        fxFun fx    = tFun fx (tVar a) (tVar b) (tVar c)
        tX          = tVar x
        tY          = tVar y
        tSelf       = tVar fxSelf
        q           = [qbind a, qbind b, qbind c]
        x           = TV KFX (name "X")
        y           = TV KFX (name "Y")
        a           = TV PRow (name "A")
        b           = TV KRow (name "B")
        c           = TV KType (name "C")

--  class $WrappedC[S,X,Y]: pass
--      wrap        : [A,B,C] => ($Actor, X(*A,**B)->C) -> S(*A,**B)->C
--      unwrap      : [A,B,C] => (S(*A,**B)->C) -> X(*A,**B)->C
clWrapped           = NClass [qbind s, qbind x, qbind y] [] te Nothing
  where te          = [(attrWrap,scWrap), (attrUnwrap,scUnwrap)]
        scWrap      = NDef (tSchema q (tFun0 [tActor, fxFun tX] (fxFun tS))) NoDec Nothing
        scUnwrap    = NDef (tSchema q (tFun0 [fxFun tS] (fxFun tY))) NoDec Nothing
        fxFun fx    = tFun fx (tVar a) (tVar b) (tVar c)
        tS          = tVar s
        tX          = tVar x
        tY          = tVar y
        q           = [qbind a, qbind b, qbind c]
        s           = TV KFX (name "S")
        x           = TV KFX (name "X")
        y           = TV KFX (name "Y")
        a           = TV PRow (name "A")
        b           = TV KRow (name "B")
        c           = TV KType (name "C")



primWits            = [ WInst []        fxAction (pWrapped fxProc fxProc)   primWrapAction path,
                        WInst []        fxProc   (pWrapped fxProc fxProc)   primWrapProc path,
                        WInst []        fxMut    (pWrapped fxMut  fxMut)    primWrapMut path,
                        WInst [qbind y] fxPure   (pWrapped fxPure (tVar y)) primWrapPure path
                      ]
  where path        = [Left (noQ "_")]
        y           = TV KFX (name "Y")

isPUSH (Call _ (Var _ x) _ _)   = x `elem` [primPUSH,primPUSHF]
isPUSH _                        = False

isPUSHF (Call _ (Var _ x) _ _)  = x == primPUSHF
isPUSHF _                       = False

isRAISE (Call _ (Var _ x) _ _)  = x == primRAISE
isRAISE _                       = False


