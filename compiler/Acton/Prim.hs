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
primNEWACTOR        = gPrim "NEWACTOR"

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
primWI64Int         = gPrim "Integral$i64$witness"
primWSequenceList   = gPrim "Sequence$list$witness"
primWCollectionList = gPrim "Collection$list$witness"

primISNOTNONE       = gPrim "ISNOTNONE"
primISNONE          = gPrim "ISNONE"

primSKIPRESc        = gPrim "SKIPRESc"
primSKIPRES         = gPrim "SKIPRES"

primAct2Proc        = gPrim "Act2Proc"
primIdFX            = gPrim "IdFX"
primMapFX           = gPrim "MapFX"

cActor              = TC primActor []
tActor              = tCon cActor
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
                            (noq primNEWACTOR,      def scNEWACTOR NoDec),

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
                            (noq primWSequenceList, var tSequenceListWild),
                            (noq primWCollectionList,var tCollectionListWild),

                            (noq primISNOTNONE,     def scISNOTNONE NoDec),
                            (noq primISNONE,        def scISNONE NoDec),

                            (noq primSKIPRESc,      def scSKIPRESc NoDec),
                            (noq primSKIPRES,       def scSKIPRES NoDec),

                            (noq primAct2Proc,      def scAct2Proc NoDec),
                            (noq primIdFX,          def scIdFX NoDec),
                            (noq primMapFX,         def scMapFX NoDec)
                      ]

tSequenceListWild   = tCon (TC qnSequence [tList tWild, tWild])
tCollectionListWild = tCon (TC qnCollection [tList tWild, tWild])

--  class $Actor (): pass
clActor cls def sig = cls [] (leftpath [cValue]) te
  where te          = [ (primKW "next",       sig (monotype tActor) Property),
                        (primKW "msg",        sig (monotype (tMsg tWild)) Property),
                        (primKW "outgoing",   sig (monotype (tMsg tWild)) Property),
                        (primKW "waitsfor",   sig (monotype (tMsg tWild)) Property),
                        (primKW "consume_hd", sig (monotype $ tCon $ TC (gPrim "int64") []) Property),
                        (primKW "catcher",    sig (monotype $ tCon $ TC (gPrim "Catcher") []) Property),
                        (primKW "msg_lock",   sig (monotype $ tCon $ TC (gPrim "Lock") []) Property),
                        (primKW "globkey",    sig (monotype $ tCon $ TC (gPrim "long") []) Property),
                        (primKW "affinity",   sig (monotype $ tCon $ TC (gPrim "int64") []) Property),
                        (boolKW,              def (monotype $ tFun fxPure posNil kwdNil tBool) NoDec),
                        (strKW,               def (monotype $ tFun fxPure posNil kwdNil tStr) NoDec),
                        (reprKW,              def (monotype $ tFun fxPure posNil kwdNil tStr) NoDec),
                        (resumeKW,            def (monotype $ tFun fxPure posNil kwdNil tNone) NoDec)
                      ]
        

--  class $R (): pass
clR cls def         = cls [] [] []

--  class $Cont[X,P] (function[X,P,(),$R]):
--      pass
clCont cls def      = cls [quant x, quant p] (leftpath [TC qnFunction [tVar x, tVar p, kwdNil, tR], cValue]) []
  where x           = TV KFX (name "X")
        p           = TV PRow (name "P")


--  $ASYNCf         : [A] => action($Actor, proc()->A) -> Msg[A]
scASYNCf            = tSchema [quant a] tASYNC
  where tASYNC      = tFun fxAction (posRow tActor $ posRow tFun' posNil) kwdNil (tMsg $ tVar a)
        a           = TV KType $ name "A"
        tFun'       = tFun fxProc posNil kwdNil (tVar a)

--  $AFTERf         : [A] => proc(int, proc()->A) -> Msg[A]
scAFTERf            = tSchema [quant a] tAFTER
  where tAFTER      = tFun fxProc (posRow tFloat $ posRow tFun' posNil) kwdNil (tMsg $ tVar a)
        a           = TV KType $ name "A"
        tFun'       = tFun fxProc posNil kwdNil (tVar a)

--  $AWAITf         : [A] => proc(Msg[A]) -> A
scAWAITf            = tSchema [quant a] tAWAIT
  where tAWAIT      = tFun fxProc (posRow (tMsg $ tVar a) posNil) kwdNil (tVar a)
        a           = TV KType $ name "T"


--  $ASYNCc         : [A] => mut($Actor, mut(mut(A)->$R)->$R) -> Msg[A]
scASYNCc            = tSchema [quant a] tASYNC
  where tASYNC      = tFun fxMut (posRow tActor $ posRow tCont' posNil) kwdNil (tMsg $ tVar a)
        a           = TV KType $ name "A"
        tCont'      = tFun fxMut (posRow tCont'' posNil) kwdNil tR
        tCont''     = tFun fxMut (posRow (tVar a) posNil) kwdNil tR

--  $AFTERc         : [A] => mut(float, mut(mut(A)->$R)->$R) -> Msg[A]
scAFTERc            = tSchema [quant a] tAFTER
  where tAFTER      = tFun fxMut (posRow tFloat $ posRow tCont' posNil) kwdNil (tMsg $ tVar a)
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

--  $AFTER          : [A] => mut(float, $Cont[mut,($Cont[mut,A],)]) -> Msg[A]
scAFTER             = tSchema [quant a] tAFTER
  where tAFTER      = tFun fxMut (posRow tFloat $ posRow tCont' posNil) kwdNil (tMsg $ tVar a)
        a           = TV KType $ name "A"
        tCont'      = tCont fxMut tCont''
        tCont''     = tCont fxMut (tVar a)

--  $AWAIT          : [A] => mut(Msg[A], $Cont[mut,(A,)]) -> $R
scAWAIT             = tSchema [quant a] tAWAIT
  where tAWAIT      = tFun fxMut (posRow (tMsg $ tVar a) $ posRow tCont' posNil) kwdNil tR
        a           = TV KType $ name "A"
        tCont'      = tCont fxMut (tVar a)



--  $PUSHc          : [X] => pure (X(BaseException)->$R) -> None
scPUSHc             = tSchema [quant x] tPUSH
  where tPUSH       = tFun fxPure (posRow tCont' posNil) kwdNil tNone
        x           = TV KFX $ name "X"
        tCont'      = tFun (tVar x) (posRow tBaseException posNil) kwdNil tR

--  $PUSH           : [X] => pure ($Cont[X,(BaseException,)]) -> None
scPUSH              = tSchema [quant x] tPUSH
  where tPUSH       = tFun fxPure (posRow tCont' posNil) kwdNil tNone
        x           = TV KFX $ name "X" 
        a           = TV KType $ name "A"
        tCont'      = tCont (tVar x) tBaseException



--  $POP            : pure () -> None
scPOP               = tSchema [] tPOP
  where tPOP        = tFun fxPure posNil kwdNil tNone

--  $RERAISE        : pure () -> None
scRERAISE           = tSchema [] tRERAISE
  where tRERAISE    = tFun fxPure posNil kwdNil tNone

--  $RAISE          : pure (BaseException) -> None
scRAISE             = tSchema [] tRAISE
  where tRAISE      = tFun fxPure (posRow tBaseException posNil) kwdNil tNone

--  $RAISEFROM      : pure (BaseException, BaseException) -> None
scRAISEFROM         = tSchema [] tRAISEFROM
  where tRAISEFROM  = tFun fxPure (posRow tBaseException $ posRow tBaseException posNil) kwdNil tNone

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
clEqOpt cls def     = cls [quant a] (leftpath [TC qnEq [tOpt $ tVar a]]) clTEnv
  where clTEnv      = [ (initKW, def scInit NoDec) ]
        scInit      = tSchema [] $ tFun fxPure (posRow (tCon $ TC qnEq [tVar a]) posNil) kwdNil tNone
        a           = TV KType (name "A")

--  class $EqOpt[A] (Eq[?A]): pass
clIdentityOpt cls def = cls [quant a] (leftpath [TC qnIdentity [tOpt $ tVar a]]) []
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

--  $Act2Proc       : (action()->None) -> proc()->None
scAct2Proc          = tSchema [] tAct2Proc
  where tAct2Proc   = tFun fxPure (posRow tActFun posNil) kwdNil tProcFun
        tActFun     = tFun fxAction posNil kwdNil tNone
        tProcFun    = tFun fxProc posNil kwdNil tNone

--  $IdFX           : [X] => (X()->None) -> X()->None
scIdFX              = tSchema [quant x] tIdFX
  where tIdFX       = tFun fxPure (posRow tXFun posNil) kwdNil tXFun
        tXFun       = tFun (tVar x) posNil kwdNil tNone
        x           = TV KFX $ name "X"

--  $MapFX          : [X1,X2A,B,C] => ((X1()->None)->X2()->None, X1(*A,**B)->C) -> X2(*A,**B)->C
scMapFX             = tSchema [quant x1, quant x2, quant a, quant b, quant c] tMapFX
  where tMapFX      = tFun fxPure (posRow (fxFun (tVar x1) (tVar x2)) $ posRow (tRealFun x1) posNil) kwdNil (tRealFun x2)
        tRealFun x  = tFun (tVar x) (tVar a) (tVar b) (tVar c)
        x1          = TV KFX $ name "X1"
        x2          = TV KFX $ name "X2"
        a           = TV PRow $ name "A"
        b           = TV KRow $ name "B"
        c           = TV KType $ name "C"
