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
import qualified Data.Map.Strict as Map

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
primAFTER_NOWf      = gPrim "AFTER_NOWf"
primAWAITf          = gPrim "AWAITf"

primASYNCc          = gPrim "ASYNCc"
primAFTERc          = gPrim "AFTERc"
primAFTER_NOWc      = gPrim "AFTER_NOWc"
primAWAITc          = gPrim "AWAITc"

primASYNC           = gPrim "ASYNC"
primAFTER           = gPrim "AFTER"
primAFTER_NOW       = gPrim "AFTER_NOW"
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
primEqTuple         = gPrim "EqTuple"
primOrdTuple        = gPrim "OrdTuple"
primHashableTuple   = gPrim "HashableTuple"

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

primCell            = gPrim "Cell"
tCell t             = tCon $ TC primCell [t]

cellKW              = name "cell"

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

primRaiseValueError = gPrim "raiseValueError"
primUGetItem        = gPrim "listD_U__getitem__"
primUListAppend    = gPrim "listD_U_append"
primUListLen       = gPrim "listD_U__len"
primUIListLen      = gPrim "ilistD_U__len"
primUDictLen       = gPrim "dictD_U__len"
primUIDictLen      = gPrim "idictD_U__len"
primUSetLen        = gPrim "setD_U__len"
primUISetLen       = gPrim "isetD_U__len"
primUStrLen        = gPrim "strD_U__len"
primUBytesLen      = gPrim "bytesD_U__len"
primUBytearrayLen  = gPrim "bytearrayD_U__len"
primUBitsetLen     = gPrim "bitsetD_U__len"
primUListSetItem    = gPrim "listD_U__setitem__"
primUListDelItem    = gPrim "listD_U__delitem__"
primUIListGetItem   = gPrim "ilistD_U__getitem__"
primUStrGetItem     = gPrim "strD_U__getitem__"
primUStrEq          = gPrim "strD_U__eq__"
primUStrNe          = gPrim "strD_U__ne__"
primUStrAdd         = gPrim "strD_U__add__"
primUBytesGetItem   = gPrim "bytesD_U__getitem__"
primUBytearrayGetItem = gPrim "bytearrayD_U__getitem__"
primUBytearraySetItem = gPrim "bytearrayD_U__setitem__"
primUBytearrayDelItem = gPrim "bytearrayD_U__delitem__"
primUArrayGetInt    = gPrim "arrayD_U__getitem_int"
primUArrayGetFloat  = gPrim "arrayD_U__getitem_float"
primUArraySetInt    = gPrim "arrayD_U__setitem_int"
primUArraySetFloat  = gPrim "arrayD_U__setitem_float"
primUArrayLen       = gPrim "arrayD_U__len"
primUBitarrayGetItem = gPrim "bitarrayD_U__getitem__"
primUBitarraySetItem = gPrim "bitarrayD_U__setitem__"
primUBitarrayLen    = gPrim "bitarrayD_U__len"
primUBitsetContains = gPrim "bitsetD_U__contains__"
primUBitsetContainsNot = gPrim "bitsetD_U__containsnot__"
primUBitsetAdd      = gPrim "bitsetD_U_add"
primUBitsetDiscard  = gPrim "bitsetD_U_discard"
primUListGetSlice   = gPrim "listD_U__getslice__"
primUListIter       = gPrim "listD_U__iter__"
primUListReversed   = gPrim "listD_U__reversed__"
primUListAdd        = gPrim "listD_U__add__"
primUListMul        = gPrim "listD_U__mul__"
primUListZero       = gPrim "listD_U__zero__"
primUListFromIter   = gPrim "listD_U__fromiter__"
primUIListGetSlice  = gPrim "ilistD_U__getslice__"
primUIListIter      = gPrim "ilistD_U__iter__"
primUIListReversed  = gPrim "ilistD_U__reversed__"
primUIListAdd       = gPrim "ilistD_U__add__"
primUIListMul       = gPrim "ilistD_U__mul__"
primUIListZero      = gPrim "ilistD_U__zero__"
primUIListFromIter  = gPrim "ilistD_U__fromiter__"
primUListInsert     = gPrim "listD_U_insert"
primUListReverse    = gPrim "listD_U_reverse"
primUListDelSlice   = gPrim "listD_U__delslice__"
primUListSetSlice   = gPrim "listD_U__setslice__"
primUListIAdd       = gPrim "listD_U__iadd__"
primUListIMul       = gPrim "listD_U__imul__"
primUStrLt              = gPrim "strD_U__lt__"
primUStrLe              = gPrim "strD_U__le__"
primUStrGt              = gPrim "strD_U__gt__"
primUStrGe              = gPrim "strD_U__ge__"
primUStrHash            = gPrim "strD_U_hash"
primUStrZero            = gPrim "strD_U__zero__"
primUStrMul             = gPrim "strD_U__mul__"
primUStrFromIter        = gPrim "strD_U__fromiter__"
primUStrContains        = gPrim "strD_U__contains__"
primUStrContainsnot     = gPrim "strD_U__containsnot__"
primUStrIter            = gPrim "strD_U__iter__"
primUStrGetslice        = gPrim "strD_U__getslice__"
primUBytesEq            = gPrim "bytesD_U__eq__"
primUBytesNe            = gPrim "bytesD_U__ne__"
primUBytesLt            = gPrim "bytesD_U__lt__"
primUBytesLe            = gPrim "bytesD_U__le__"
primUBytesGt            = gPrim "bytesD_U__gt__"
primUBytesGe            = gPrim "bytesD_U__ge__"
primUBytesContains      = gPrim "bytesD_U__contains__"
primUBytesContainsnot   = gPrim "bytesD_U__containsnot__"
primUBytearrayEq        = gPrim "bytearrayD_U__eq__"
primUBytearrayNe        = gPrim "bytearrayD_U__ne__"
primUBytearrayLt        = gPrim "bytearrayD_U__lt__"
primUBytearrayLe        = gPrim "bytearrayD_U__le__"
primUBytearrayGt        = gPrim "bytearrayD_U__gt__"
primUBytearrayGe        = gPrim "bytearrayD_U__ge__"
primUBytearrayContains  = gPrim "bytearrayD_U__contains__"
primUBytearrayContainsnot= gPrim "bytearrayD_U__containsnot__"
primUBytesHash          = gPrim "bytesD_U_hash"
primUBytesFromIter      = gPrim "bytesD_U__fromiter__"
primUBytesIter          = gPrim "bytesD_U__iter__"
primUBytesGetslice      = gPrim "bytesD_U__getslice__"
primUBytesAdd           = gPrim "bytesD_U__add__"
primUBytesZero          = gPrim "bytesD_U__zero__"
primUBytesMul           = gPrim "bytesD_U__mul__"
primUBytearrayInsert    = gPrim "bytearrayD_U_insert"
primUBytearrayAppend    = gPrim "bytearrayD_U_append"
primUBytearrayReverse   = gPrim "bytearrayD_U_reverse"
primUBytearrayReversed  = gPrim "bytearrayD_U__reversed__"
primUBytearrayGetslice  = gPrim "bytearrayD_U__getslice__"
primUBytearraySetslice  = gPrim "bytearrayD_U__setslice__"
primUBytearrayDelslice  = gPrim "bytearrayD_U__delslice__"
primUBytearrayFromIter  = gPrim "bytearrayD_U__fromiter__"
primUBytearrayIter      = gPrim "bytearrayD_U__iter__"
primUBytearrayAdd       = gPrim "bytearrayD_U__add__"
primUBytearrayZero      = gPrim "bytearrayD_U__zero__"
primUBytearrayMul       = gPrim "bytearrayD_U__mul__"
primUBytearrayIadd      = gPrim "bytearrayD_U__iadd__"
primUBytearrayImul      = gPrim "bytearrayD_U__imul__"
primUBigintAdd           = gPrim "bigintD_U__add__"
primUBigintZero          = gPrim "bigintD_U__zero__"
primUBigintComplex       = gPrim "bigintD_U__complex__"
primUBigintFromatom      = gPrim "bigintD_U__fromatom__"
primUBigintMul           = gPrim "bigintD_U__mul__"
primUBigintPow           = gPrim "bigintD_U__pow__"
primUBigintNeg           = gPrim "bigintD_U__neg__"
primUBigintPos           = gPrim "bigintD_U__pos__"
primUBigintReal          = gPrim "bigintD_U_real"
primUBigintImag          = gPrim "bigintD_U_imag"
primUBigintAbs           = gPrim "bigintD_U__abs__"
primUBigintConjugate     = gPrim "bigintD_U_conjugate"
primUBigintFloat         = gPrim "bigintD_U__float__"
primUBigintTrunc         = gPrim "bigintD_U__trunc__"
primUBigintFloor         = gPrim "bigintD_U__floor__"
primUBigintCeil          = gPrim "bigintD_U__ceil__"
primUBigintRound         = gPrim "bigintD_U__round__"
primUBigintNumerator     = gPrim "bigintD_U_numerator"
primUBigintDenominator   = gPrim "bigintD_U_denominator"
primUBigintInt           = gPrim "bigintD_U__int__"
primUBigintIndex         = gPrim "bigintD_U__index__"
primUBigintDivmod        = gPrim "bigintD_U__divmod__"
primUBigintFloordiv      = gPrim "bigintD_U__floordiv__"
primUBigintMod           = gPrim "bigintD_U__mod__"
primUBigintLshift        = gPrim "bigintD_U__lshift__"
primUBigintRshift        = gPrim "bigintD_U__rshift__"
primUBigintInvert        = gPrim "bigintD_U__invert__"
primUBigintAnd           = gPrim "bigintD_U__and__"
primUBigintOr            = gPrim "bigintD_U__or__"
primUBigintXor           = gPrim "bigintD_U__xor__"
primUBigintSub           = gPrim "bigintD_U__sub__"
primUBigintTruediv       = gPrim "bigintD_U__truediv__"
primUBigintEq            = gPrim "bigintD_U__eq__"
primUBigintNe            = gPrim "bigintD_U__ne__"
primUBigintLt            = gPrim "bigintD_U__lt__"
primUBigintLe            = gPrim "bigintD_U__le__"
primUBigintGt            = gPrim "bigintD_U__gt__"
primUBigintGe            = gPrim "bigintD_U__ge__"
primUBigintHash          = gPrim "bigintD_U_hash"
primUComplexAdd          = gPrim "complexD_U__add__"
primUComplexZero         = gPrim "complexD_U__zero__"
primUComplexComplex      = gPrim "complexD_U__complex__"
primUComplexMul          = gPrim "complexD_U__mul__"
primUComplexFromatom     = gPrim "complexD_U__fromatom__"
primUComplexPow          = gPrim "complexD_U__pow__"
primUComplexNeg          = gPrim "complexD_U__neg__"
primUComplexPos          = gPrim "complexD_U__pos__"
primUComplexReal         = gPrim "complexD_U_real"
primUComplexImag         = gPrim "complexD_U_imag"
primUComplexAbs          = gPrim "complexD_U__abs__"
primUComplexConjugate    = gPrim "complexD_U_conjugate"
primUComplexTruediv      = gPrim "complexD_U__truediv__"
primUComplexSub          = gPrim "complexD_U__sub__"
primUComplexEq           = gPrim "complexD_U__eq__"
primUComplexNe           = gPrim "complexD_U__ne__"
primUComplexHash         = gPrim "complexD_U_hash"
primUNext           = gPrim "rangeD_U__next__"

annot t_ann ann t e = eCall (tApp (eQVar primAnnot) [t_ann, t]) [ann, e]

unAnnot t_ann (Call _ (TApp _ (Var _ n) [t,_]) (PosArg w (PosArg e PosNil)) KwdNil)
  | n == primAnnot && t == t_ann    = (w, e)

-- Compiler-private descriptions of concrete hash-container calls. The schemas
-- keep keys/elements boxed; CodeGen adapts the container to the shared C view.
-- Factories keep their protocol witness, which supplies Hashable before there
-- is a container. Dict comparisons likewise retain their value Eq witness.
data HashContainerABI = DictBase | SetTable | SetObject | SetBuild QName

data HashContainerWorker = HashContainerWorker {
    hcType :: QName, hcMethod :: Name, hcArity :: Int,
    hcName :: QName, hcSchema :: TSchema, hcCName :: String,
    hcABI :: HashContainerABI, hcNegate :: Bool, hcReverse :: Bool,
    hcDefault :: Bool, hcContainerArgs :: Int
    }

hashContainerWorker q attr arity = Map.lookup (q,attr,arity) hashContainerMethodMap

hashContainerPrimitive q = Map.lookup q hashContainerPrimitiveMap

hashContainerMethodMap = Map.fromList [ ((hcType w,hcMethod w,hcArity w),w) | w <- hashContainerWorkers ]
hashContainerPrimitiveMap = Map.fromList [ (hcName w,w) | w <- hashContainerWorkers ]

hashContainerWorkers = dictWorkers qnDict tDict True ++ dictWorkers qnIDict tIDict False ++
                       setWorkers qnSetT tSet True ++ setWorkers qnISetT tISet False
  where a = TV KType (name "A")
        b = TV KType (name "B")
        c = TV KType (name "C")
        at = tVar a
        bt = tVar b
        ct = tVar c
        pair = tTupleP (foldr posRow posNil [at,bt])
        worker q self qs attr fx args result native abi neg rev def =
            HashContainerWorker q (name attr) (length args)
                (gPrim (nstr (noq q) ++ "_base_" ++ attr))
                (tSchema qs $ tFun fx (foldr posRow posNil args) kwdNil result)
                native abi neg rev def (length $ takeWhile (== self) args)
        dictWorkers q cons mutable =
          let self = cons at bt
              make attr fx args result native def = worker q self [qbind a,qbind b]
                  attr fx args result native DictBase False False def
          in [ make "__getitem__" fxPure [self,at] bt "B_dict_base_getitem" False
             , make "__contains__" fxPure [self,at] tBool "B_dict_base_contains" False
             , (make "__containsnot__" fxPure [self,at] tBool "B_dict_base_contains" False){hcNegate=True}
             , make "get" fxPure [self,at] (tOpt bt) "B_dict_base_get" True
             , make "get_def" fxPure [self,at,bt] bt "B_dict_base_get" False
             , make "__iter__" fxPure [self] (tIterator at) "B_dict_base_key_iterator" False
             , make "keys" fxPure [self] (tIterator at) "B_dict_base_key_iterator" False
             , make "values" fxPure [self] (tIterator bt) "B_dict_base_values_iterator" False
             , make "items" fxPure [self] (tIterator pair) "B_dict_base_items_iterator" False
             ] ++ if not mutable then [] else
             [ make "__setitem__" fxMut [self,at,bt] tNone "B_dict_base_setitem" False
             , make "__delitem__" fxMut [self,at] tNone "B_dict_base_delitem" False
             , make "pop" fxMut [self,at] (tOpt bt) "B_dict_base_pop" True
             , make "pop_def" fxMut [self,at,bt] bt "B_dict_base_pop" False
             , make "popitem" fxMut [self] pair "B_dict_base_popitem" False
             , make "setdefault" fxMut [self,at,bt] (tOpt bt) "B_dict_base_setdefault" False
             , worker q self [qbind a,qbind b,qbind c] "update" fxMut
                 [self,tCon (TC qnIterable [ct,pair]),ct] tNone
                 "B_dict_base_update" DictBase False False False
             ]
        setWorkers q cons mutable =
          let self = cons at
              make attr fx args result native abi neg rev = worker q self [qbind a]
                  attr fx args result native abi neg rev False
          in [ make "__contains__" fxPure [self,at] tBool "B_set_table_contains" SetTable False False
             , make "__containsnot__" fxPure [self,at] tBool "B_set_table_contains" SetTable True False
             , make "__iter__" fxPure [self] (tIterator at) "B_set_base_iter" SetObject False False
             ] ++ [make attr fxPure [self,self] tBool native SetTable neg rev
                   | (attr,native,neg,rev) <-
                       [("__eq__","B_set_table_eq",False,False), ("__ne__","B_set_table_eq",True,False),
                        ("__gt__","B_set_table_gt",False,False), ("__ge__","B_set_table_ge",False,False),
                        ("__lt__","B_set_table_gt",False,True), ("__le__","B_set_table_ge",False,True),
                        ("isdisjoint","B_set_table_isdisjoint",False,False)]] ++
             [make attr fxPure [self,self] self native (SetBuild q) False False
                   | (attrs,native) <- [(["__or__","union"],"B_set_base_union"),
                        (["__and__","intersection"],"B_set_base_intersection"),
                        (["__sub__","difference"],"B_set_base_difference"),
                        (["__xor__","symmetric_difference"],"B_set_base_symmetric_difference")], attr <- attrs] ++
             if not mutable then
               [make "hash" fxPure [self,tCon (TC (gBuiltin (name "hasher")) [])] tNone
                   "B_set_hash_table" SetTable False False]
             else [make "add" fxMut [self,at] tNone "B_set_table_add" SetTable False False,
                   make "discard" fxMut [self,at] tNone "B_set_table_discard" SetTable False False,
                   make "pop" fxMut [self] at "B_set_table_pop" SetTable False False,
                   worker q self [qbind a,qbind c] "update" fxMut
                     [self,tCon (TC qnIterable [ct,at]),ct] tNone
                     "B_set_base_update" SetObject False False False] ++
               [make attr fxMut [self,self] self native SetObject False False
                   | (attr,native) <- [("__ior__","B_set_base_ior"), ("__iand__","B_set_base_iand"),
                        ("__isub__","B_set_base_isub"), ("__ixor__","B_set_base_ixor")]]

primEnv             = [ (noq (hcName w), NDef (hcSchema w) NoDec Nothing) | w <- hashContainerWorkers ] ++
                      [     (noq primASYNCf,        NDef scASYNCf NoDec Nothing),
                            (noq primAFTERf,        NDef scAFTERf NoDec Nothing),
                            (noq primAFTER_NOWf,    NDef scAFTERf NoDec Nothing),
                            (noq primAWAITf,        NDef scAWAITf NoDec Nothing),

                            (noq primASYNCc,        NDef scASYNCc NoDec Nothing),
                            (noq primAFTERc,        NDef scAFTERc NoDec Nothing),
                            (noq primAFTER_NOWc,    NDef scAFTERc NoDec Nothing),
                            (noq primAWAITc,        NDef scAWAITc NoDec Nothing),

                            (noq primASYNC,         NDef scASYNC NoDec Nothing),
                            (noq primAFTER,         NDef scAFTER NoDec Nothing),
                            (noq primAFTER_NOW,     NDef scAFTER NoDec Nothing),
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

                            (noq primCell,          clCell),

                            (noq primRContc,        NDef scRContc NoDec Nothing),
                            (noq primRCont,         NDef scRCont NoDec Nothing),
                            (noq primRFail,         NDef scRFail NoDec Nothing),

                            (noq primEqOpt,         clEqOpt),
                            (noq primEqTuple,       clEqTuple),
                            (noq primOrdTuple,      clOrdTuple),
                            (noq primHashableTuple, clHashableTuple),
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
                            (noq primRaiseValueError,NDef scRaiseValueError NoDec Nothing), 
                            (noq primUGetItem,      NDef scUGetItem NoDec Nothing),
                            (noq primUListAppend, NDef scUListAppend NoDec Nothing),
                            (noq primUListLen, NDef scUListLen NoDec Nothing),
                            (noq primUIListLen, NDef scUIListLen NoDec Nothing),
                            (noq primUDictLen, NDef scUDictLen NoDec Nothing),
                            (noq primUIDictLen, NDef scUIDictLen NoDec Nothing),
                            (noq primUSetLen, NDef scUSetLen NoDec Nothing),
                            (noq primUISetLen, NDef scUISetLen NoDec Nothing),
                            (noq primUStrLen, NDef scUStrLen NoDec Nothing),
                            (noq primUBytesLen, NDef scUBytesLen NoDec Nothing),
                            (noq primUBytearrayLen, NDef scUBytearrayLen NoDec Nothing),
                            (noq primUBitsetLen, NDef scUBitsetLen NoDec Nothing),
                            (noq primUListSetItem,  NDef scUListSetItem NoDec Nothing),
                            (noq primUListDelItem,  NDef scUListDelItem NoDec Nothing),
                            (noq primUIListGetItem, NDef scUIListGetItem NoDec Nothing),
                            (noq primUStrGetItem,   NDef scUStrGetItem NoDec Nothing),
                            (noq primUStrEq,        NDef scUStrCompare NoDec Nothing),
                            (noq primUStrNe,        NDef scUStrCompare NoDec Nothing),
                            (noq primUStrAdd,       NDef scUStrAdd NoDec Nothing),
                            (noq primUBytesGetItem, NDef scUBytesGetItem NoDec Nothing),
                            (noq primUBytearrayGetItem, NDef scUBytearrayGetItem NoDec Nothing),
                            (noq primUBytearraySetItem, NDef scUBytearraySetItem NoDec Nothing),
                            (noq primUBytearrayDelItem, NDef scUBytearrayDelItem NoDec Nothing),
                            (noq primUArrayGetInt, NDef scUArrayGetInt NoDec Nothing),
                            (noq primUArrayGetFloat, NDef scUArrayGetFloat NoDec Nothing),
                            (noq primUArraySetInt, NDef scUArraySetInt NoDec Nothing),
                            (noq primUArraySetFloat, NDef scUArraySetFloat NoDec Nothing),
                            (noq primUArrayLen, NDef scUArrayLen NoDec Nothing),
                            (noq primUBitarrayGetItem, NDef scUBitarrayGetItem NoDec Nothing),
                            (noq primUBitarraySetItem, NDef scUBitarraySetItem NoDec Nothing),
                            (noq primUBitarrayLen, NDef scUBitarrayLen NoDec Nothing),
                            (noq primUBitsetContains, NDef scUBitsetContains NoDec Nothing),
                            (noq primUBitsetContainsNot, NDef scUBitsetContains NoDec Nothing),
                            (noq primUBitsetAdd, NDef scUBitsetMutate NoDec Nothing),
                            (noq primUBitsetDiscard, NDef scUBitsetMutate NoDec Nothing),
                            (noq primUListGetSlice, NDef scUListGetSlice NoDec Nothing),
                            (noq primUListIter, NDef scUListIter NoDec Nothing),
                            (noq primUListReversed, NDef scUListReversed NoDec Nothing),
                            (noq primUListAdd, NDef scUListAdd NoDec Nothing),
                            (noq primUListMul, NDef scUListMul NoDec Nothing),
                            (noq primUListZero, NDef scUListZero NoDec Nothing),
                            (noq primUListFromIter, NDef scUListFromIter NoDec Nothing),
                            (noq primUIListGetSlice, NDef scUIListGetSlice NoDec Nothing),
                            (noq primUIListIter, NDef scUIListIter NoDec Nothing),
                            (noq primUIListReversed, NDef scUIListReversed NoDec Nothing),
                            (noq primUIListAdd, NDef scUIListAdd NoDec Nothing),
                            (noq primUIListMul, NDef scUIListMul NoDec Nothing),
                            (noq primUIListZero, NDef scUIListZero NoDec Nothing),
                            (noq primUIListFromIter, NDef scUIListFromIter NoDec Nothing),
                            (noq primUListInsert, NDef scUListInsert NoDec Nothing),
                            (noq primUListReverse, NDef scUListReverse NoDec Nothing),
                            (noq primUListDelSlice, NDef scUListDelSlice NoDec Nothing),
                            (noq primUListSetSlice, NDef scUListSetSlice NoDec Nothing),
                            (noq primUListIAdd, NDef scUListIAdd NoDec Nothing),
                            (noq primUListIMul, NDef scUListIMul NoDec Nothing),
                            (noq primUStrLt, NDef scUStrLt NoDec Nothing),
                            (noq primUStrLe, NDef scUStrLe NoDec Nothing),
                            (noq primUStrGt, NDef scUStrGt NoDec Nothing),
                            (noq primUStrGe, NDef scUStrGe NoDec Nothing),
                            (noq primUStrHash, NDef scUStrHash NoDec Nothing),
                            (noq primUStrZero, NDef scUStrZero NoDec Nothing),
                            (noq primUStrMul, NDef scUStrMul NoDec Nothing),
                            (noq primUStrFromIter, NDef scUStrFromIter NoDec Nothing),
                            (noq primUStrContains, NDef scUStrContains NoDec Nothing),
                            (noq primUStrContainsnot, NDef scUStrContainsnot NoDec Nothing),
                            (noq primUStrIter, NDef scUStrIter NoDec Nothing),
                            (noq primUStrGetslice, NDef scUStrGetslice NoDec Nothing),
                            (noq primUBytesEq, NDef scUBytesEq NoDec Nothing),
                            (noq primUBytesNe, NDef scUBytesNe NoDec Nothing),
                            (noq primUBytesLt, NDef scUBytesLt NoDec Nothing),
                            (noq primUBytesLe, NDef scUBytesLe NoDec Nothing),
                            (noq primUBytesGt, NDef scUBytesGt NoDec Nothing),
                            (noq primUBytesGe, NDef scUBytesGe NoDec Nothing),
                            (noq primUBytesContains, NDef scUBytesContains NoDec Nothing),
                            (noq primUBytesContainsnot, NDef scUBytesContainsnot NoDec Nothing),
                            (noq primUBytearrayEq, NDef scUBytearrayEq NoDec Nothing),
                            (noq primUBytearrayNe, NDef scUBytearrayNe NoDec Nothing),
                            (noq primUBytearrayLt, NDef scUBytearrayLt NoDec Nothing),
                            (noq primUBytearrayLe, NDef scUBytearrayLe NoDec Nothing),
                            (noq primUBytearrayGt, NDef scUBytearrayGt NoDec Nothing),
                            (noq primUBytearrayGe, NDef scUBytearrayGe NoDec Nothing),
                            (noq primUBytearrayContains, NDef scUBytearrayContains NoDec Nothing),
                            (noq primUBytearrayContainsnot, NDef scUBytearrayContainsnot NoDec Nothing),
                            (noq primUBytesHash, NDef scUBytesHash NoDec Nothing),
                            (noq primUBytesFromIter, NDef scUBytesFromIter NoDec Nothing),
                            (noq primUBytesIter, NDef scUBytesIter NoDec Nothing),
                            (noq primUBytesGetslice, NDef scUBytesGetslice NoDec Nothing),
                            (noq primUBytesAdd, NDef scUBytesAdd NoDec Nothing),
                            (noq primUBytesZero, NDef scUBytesZero NoDec Nothing),
                            (noq primUBytesMul, NDef scUBytesMul NoDec Nothing),
                            (noq primUBytearrayInsert, NDef scUBytearrayInsert NoDec Nothing),
                            (noq primUBytearrayAppend, NDef scUBytearrayAppend NoDec Nothing),
                            (noq primUBytearrayReverse, NDef scUBytearrayReverse NoDec Nothing),
                            (noq primUBytearrayReversed, NDef scUBytearrayReversed NoDec Nothing),
                            (noq primUBytearrayGetslice, NDef scUBytearrayGetslice NoDec Nothing),
                            (noq primUBytearraySetslice, NDef scUBytearraySetslice NoDec Nothing),
                            (noq primUBytearrayDelslice, NDef scUBytearrayDelslice NoDec Nothing),
                            (noq primUBytearrayFromIter, NDef scUBytearrayFromIter NoDec Nothing),
                            (noq primUBytearrayIter, NDef scUBytearrayIter NoDec Nothing),
                            (noq primUBytearrayAdd, NDef scUBytearrayAdd NoDec Nothing),
                            (noq primUBytearrayZero, NDef scUBytearrayZero NoDec Nothing),
                            (noq primUBytearrayMul, NDef scUBytearrayMul NoDec Nothing),
                            (noq primUBytearrayIadd, NDef scUBytearrayIadd NoDec Nothing),
                            (noq primUBytearrayImul, NDef scUBytearrayImul NoDec Nothing),
                            (noq primUBigintAdd, NDef scUBigintAdd NoDec Nothing),
                            (noq primUBigintZero, NDef scUBigintZero NoDec Nothing),
                            (noq primUBigintComplex, NDef scUBigintComplex NoDec Nothing),
                            (noq primUBigintFromatom, NDef scUBigintFromatom NoDec Nothing),
                            (noq primUBigintMul, NDef scUBigintMul NoDec Nothing),
                            (noq primUBigintPow, NDef scUBigintPow NoDec Nothing),
                            (noq primUBigintNeg, NDef scUBigintNeg NoDec Nothing),
                            (noq primUBigintPos, NDef scUBigintPos NoDec Nothing),
                            (noq primUBigintReal, NDef scUBigintReal NoDec Nothing),
                            (noq primUBigintImag, NDef scUBigintImag NoDec Nothing),
                            (noq primUBigintAbs, NDef scUBigintAbs NoDec Nothing),
                            (noq primUBigintConjugate, NDef scUBigintConjugate NoDec Nothing),
                            (noq primUBigintFloat, NDef scUBigintFloat NoDec Nothing),
                            (noq primUBigintTrunc, NDef scUBigintTrunc NoDec Nothing),
                            (noq primUBigintFloor, NDef scUBigintFloor NoDec Nothing),
                            (noq primUBigintCeil, NDef scUBigintCeil NoDec Nothing),
                            (noq primUBigintRound, NDef scUBigintRound NoDec Nothing),
                            (noq primUBigintNumerator, NDef scUBigintNumerator NoDec Nothing),
                            (noq primUBigintDenominator, NDef scUBigintDenominator NoDec Nothing),
                            (noq primUBigintInt, NDef scUBigintInt NoDec Nothing),
                            (noq primUBigintIndex, NDef scUBigintIndex NoDec Nothing),
                            (noq primUBigintDivmod, NDef scUBigintDivmod NoDec Nothing),
                            (noq primUBigintFloordiv, NDef scUBigintFloordiv NoDec Nothing),
                            (noq primUBigintMod, NDef scUBigintMod NoDec Nothing),
                            (noq primUBigintLshift, NDef scUBigintLshift NoDec Nothing),
                            (noq primUBigintRshift, NDef scUBigintRshift NoDec Nothing),
                            (noq primUBigintInvert, NDef scUBigintInvert NoDec Nothing),
                            (noq primUBigintAnd, NDef scUBigintAnd NoDec Nothing),
                            (noq primUBigintOr, NDef scUBigintOr NoDec Nothing),
                            (noq primUBigintXor, NDef scUBigintXor NoDec Nothing),
                            (noq primUBigintSub, NDef scUBigintSub NoDec Nothing),
                            (noq primUBigintTruediv, NDef scUBigintTruediv NoDec Nothing),
                            (noq primUBigintEq, NDef scUBigintEq NoDec Nothing),
                            (noq primUBigintNe, NDef scUBigintNe NoDec Nothing),
                            (noq primUBigintLt, NDef scUBigintLt NoDec Nothing),
                            (noq primUBigintLe, NDef scUBigintLe NoDec Nothing),
                            (noq primUBigintGt, NDef scUBigintGt NoDec Nothing),
                            (noq primUBigintGe, NDef scUBigintGe NoDec Nothing),
                            (noq primUBigintHash, NDef scUBigintHash NoDec Nothing),
                            (noq primUComplexAdd, NDef scUComplexAdd NoDec Nothing),
                            (noq primUComplexZero, NDef scUComplexZero NoDec Nothing),
                            (noq primUComplexComplex, NDef scUComplexComplex NoDec Nothing),
                            (noq primUComplexMul, NDef scUComplexMul NoDec Nothing),
                            (noq primUComplexFromatom, NDef scUComplexFromatom NoDec Nothing),
                            (noq primUComplexPow, NDef scUComplexPow NoDec Nothing),
                            (noq primUComplexNeg, NDef scUComplexNeg NoDec Nothing),
                            (noq primUComplexPos, NDef scUComplexPos NoDec Nothing),
                            (noq primUComplexReal, NDef scUComplexReal NoDec Nothing),
                            (noq primUComplexImag, NDef scUComplexImag NoDec Nothing),
                            (noq primUComplexAbs, NDef scUComplexAbs NoDec Nothing),
                            (noq primUComplexConjugate, NDef scUComplexConjugate NoDec Nothing),
                            (noq primUComplexTruediv, NDef scUComplexTruediv NoDec Nothing),
                            (noq primUComplexSub, NDef scUComplexSub NoDec Nothing),
                            (noq primUComplexEq, NDef scUComplexEq NoDec Nothing),
                            (noq primUComplexNe, NDef scUComplexNe NoDec Nothing),
                            (noq primUComplexHash, NDef scUComplexHash NoDec Nothing),
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

--  class $Cell[A] (object, value):
--      cell        : A
--      __init__    : (A) -> None
clCell              = NClass [qbind a] (leftpath [cObject, cValue]) te Nothing
  where te          = [ (cellKW, NSig (monotype $ tVar a) Property Nothing),
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
                        (primKW "globkey",    NSig (monotype $ tCon $ TC (gPrim "int64") []) Property Nothing),
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

--  class $EqTuple[W,P,K] (Eq[(*P,**K)]): pass, and likewise for Ord and Hashable.
--  W is the row of component witnesses for (*P,**K), passed to __init__ as one tuple
clEqTuple           = clTupleWit qnEq
clOrdTuple          = clTupleWit qnOrd
clHashableTuple     = clTupleWit qnHashable

clTupleWit qn       = NClass [qbind w, qbind p, qbind k] (leftpath [TC qn [tTuple (tVar p) (tVar k)]]) clTEnv Nothing
  where clTEnv      = [ (initKW, NDef scInit NoDec Nothing) ]
        scInit      = tSchema [] $ tFun fxPure (posRow (tTuple (tVar w) kwdNil) posNil) kwdNil tNone
        w           = TV PRow (name "W")
        p           = TV PRow (name "P")
        k           = TV KRow (name "K")

-- The protocols the solver can derive for a closed tuple type, by structural
-- induction over its components, and the prim witness classes implementing them.
tupleWits           :: [(QName, QName)]
tupleWits           = [ (qnEq, primEqTuple), (qnOrd, primOrdTuple), (qnHashable, primHashableTuple) ]

tupleProtos         :: [PCon]
tupleProtos         = [ TC n [] | (n,_) <- tupleWits ]

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

-- Witness-free append keeps the element boxed at this polymorphic boundary.
scUListAppend       = tSchema [qbind a] $ tFun fxMut (posRow (tList (tVar a)) (posRow (tVar a) posNil)) kwdNil tNone
  where a           = TV KType $ name "A"

-- Length workers return the existing native int representation.
scUListLen          = tSchema [qbind a] $ tFun fxPure (posRow (tList (tVar a)) posNil) kwdNil tInt
  where a           = TV KType $ name "A"

scUIListLen         = tSchema [qbind a] $ tFun fxPure (posRow (tIList (tVar a)) posNil) kwdNil tInt
  where a           = TV KType $ name "A"

scUDictLen          = tSchema [qbind a, qbind b] $ tFun fxPure (posRow (tDict (tVar a) (tVar b)) posNil) kwdNil tInt
  where a           = TV KType $ name "A"
        b           = TV KType $ name "B"

scUIDictLen         = tSchema [qbind a, qbind b] $ tFun fxPure (posRow (tIDict (tVar a) (tVar b)) posNil) kwdNil tInt
  where a           = TV KType $ name "A"
        b           = TV KType $ name "B"

scUSetLen           = tSchema [qbind a] $ tFun fxPure (posRow (tSet (tVar a)) posNil) kwdNil tInt
  where a           = TV KType $ name "A"

scUISetLen          = tSchema [qbind a] $ tFun fxPure (posRow (tISet (tVar a)) posNil) kwdNil tInt
  where a           = TV KType $ name "A"

scUStrLen           = monotype $ tFun fxPure (posRow tStr posNil) kwdNil tInt

scUBytesLen         = monotype $ tFun fxPure (posRow tBytes posNil) kwdNil tInt

scUBytearrayLen     = monotype $ tFun fxPure (posRow tBytearray posNil) kwdNil tInt

scUBitsetLen        = monotype $ tFun fxPure (posRow tBitset posNil) kwdNil tInt

scUListSetItem      = tSchema [qbind a] tUListSetItem
  where tUListSetItem = tFun fxMut (posRow (tList (tVar a)) (posRow tInt (posRow (tVar a) posNil))) kwdNil tNone
        a             = TV KType $ name "A"

scUListDelItem      = tSchema [qbind a] tUListDelItem
  where tUListDelItem = tFun fxMut (posRow (tList (tVar a)) (posRow tInt posNil)) kwdNil tNone
        a             = TV KType $ name "A"

scUIListGetItem     = tSchema [qbind a] tUIListGetItem
  where tUIListGetItem = tFun fxPure (posRow (tIList (tVar a)) (posRow tInt posNil)) kwdNil (tVar a)
        a              = TV KType $ name "A"

scUStrGetItem       = monotype $ tFun fxPure (posRow tStr (posRow tInt posNil)) kwdNil tStr

scUStrCompare       = monotype $ tFun fxPure (posRow tStr (posRow tStr posNil)) kwdNil tBool

scUStrAdd           = monotype $ tFun fxPure (posRow tStr (posRow tStr posNil)) kwdNil tStr

scUBytesGetItem     = monotype $ tFun fxPure (posRow tBytes (posRow tInt posNil)) kwdNil tInt

scUBytearrayGetItem = monotype $ tFun fxPure (posRow tBytearray (posRow tInt posNil)) kwdNil tInt

scUBytearraySetItem = monotype $ tFun fxMut (posRow tBytearray (posRow tInt (posRow tInt posNil))) kwdNil tNone

scUBytearrayDelItem = monotype $ tFun fxMut (posRow tBytearray (posRow tInt posNil)) kwdNil tNone

scUArrayGetInt      = monotype $ tFun fxPure (posRow (tArray tInt) (posRow tInt posNil)) kwdNil tInt

scUArrayGetFloat    = monotype $ tFun fxPure (posRow (tArray tFloat) (posRow tInt posNil)) kwdNil tFloat

scUArraySetInt      = monotype $ tFun fxMut (posRow (tArray tInt) (posRow tInt (posRow tInt posNil))) kwdNil tNone

scUArraySetFloat    = monotype $ tFun fxMut (posRow (tArray tFloat) (posRow tInt (posRow tFloat posNil))) kwdNil tNone

scUArrayLen         = tSchema [qbind a] $ tFun fxPure (posRow (tArray (tVar a)) posNil) kwdNil tInt
  where a           = TV KType $ name "A"

scUBitarrayGetItem  = monotype $ tFun fxPure (posRow tBitarray (posRow tInt posNil)) kwdNil tBool

scUBitarraySetItem  = monotype $ tFun fxMut (posRow tBitarray (posRow tInt (posRow tBool posNil))) kwdNil tNone

scUBitarrayLen      = monotype $ tFun fxPure (posRow tBitarray posNil) kwdNil tInt

scUBitsetContains   = monotype $ tFun fxPure (posRow tBitset (posRow tInt posNil)) kwdNil tBool

scUBitsetMutate     = monotype $ tFun fxMut (posRow tBitset (posRow tInt posNil)) kwdNil tNone

-- The element stays polymorphic (boxed) at Sequence worker boundaries.
sequenceWorkerSchema cons fx args result = tSchema [qbind a] $
    tFun fx (foldr posRow posNil (args elemT selfT)) kwdNil (result elemT selfT)
  where a           = TV KType $ name "A"
        elemT       = tVar a
        selfT       = cons elemT

sequenceFromIterSchema cons = tSchema [qbind a, qbind c] $
    tFun fxPure (posRow (tCon (TC qnIterable [tVar c,tVar a])) (posRow (tVar c) posNil)) kwdNil (cons (tVar a))
  where a           = TV KType $ name "A"
        c           = TV KType $ name "C"

scUListSetSlice     = tSchema [qbind a, qbind c] $
    tFun fxMut (foldr posRow posNil [tList (tVar a),tCon (TC qnIterable [tVar c,tVar a]),tSlice,tVar c]) kwdNil tNone
  where a           = TV KType $ name "A"
        c           = TV KType $ name "C"

scUListGetSlice         = sequenceWorkerSchema tList fxPure (\a s -> [s,tSlice]) (\a s -> s)
scUListIter             = sequenceWorkerSchema tList fxPure (\a s -> [s]) (\a s -> tIterator a)
scUListReversed         = sequenceWorkerSchema tList fxPure (\a s -> [s]) (\a s -> tIterator a)
scUListAdd              = sequenceWorkerSchema tList fxPure (\a s -> [s,s]) (\a s -> s)
scUListMul              = sequenceWorkerSchema tList fxPure (\a s -> [s,tInt]) (\a s -> s)
scUListZero             = sequenceWorkerSchema tList fxPure (\a s -> []) (\a s -> s)
scUListFromIter         = sequenceFromIterSchema tList
scUIListGetSlice        = sequenceWorkerSchema tIList fxPure (\a s -> [s,tSlice]) (\a s -> s)
scUIListIter            = sequenceWorkerSchema tIList fxPure (\a s -> [s]) (\a s -> tIterator a)
scUIListReversed        = sequenceWorkerSchema tIList fxPure (\a s -> [s]) (\a s -> tIterator a)
scUIListAdd             = sequenceWorkerSchema tIList fxPure (\a s -> [s,s]) (\a s -> s)
scUIListMul             = sequenceWorkerSchema tIList fxPure (\a s -> [s,tInt]) (\a s -> s)
scUIListZero            = sequenceWorkerSchema tIList fxPure (\a s -> []) (\a s -> s)
scUIListFromIter        = sequenceFromIterSchema tIList
scUListInsert           = sequenceWorkerSchema tList fxMut (\a s -> [s,tInt,a]) (\a s -> tNone)
scUListReverse          = sequenceWorkerSchema tList fxMut (\a s -> [s]) (\a s -> tNone)
scUListDelSlice         = sequenceWorkerSchema tList fxMut (\a s -> [s,tSlice]) (\a s -> tNone)
scUListIAdd             = sequenceWorkerSchema tList fxMut (\a s -> [s,s]) (\a s -> s)
scUListIMul             = sequenceWorkerSchema tList fxMut (\a s -> [s,tInt]) (\a s -> s)

-- Monomorphic workers keep only input witnesses used by their implementations.
scUStrLt                = monotype $ tFun fxPure (foldr posRow posNil [tStr, tStr]) kwdNil (tBool)
scUStrLe                = monotype $ tFun fxPure (foldr posRow posNil [tStr, tStr]) kwdNil (tBool)
scUStrGt                = monotype $ tFun fxPure (foldr posRow posNil [tStr, tStr]) kwdNil (tBool)
scUStrGe                = monotype $ tFun fxPure (foldr posRow posNil [tStr, tStr]) kwdNil (tBool)
scUStrHash              = monotype $ tFun fxPure (foldr posRow posNil [tStr, tCon (TC (gBuiltin (name "hasher")) [])]) kwdNil (tNone)
scUStrZero              = monotype $ tFun fxPure (foldr posRow posNil []) kwdNil (tStr)
scUStrMul               = monotype $ tFun fxPure (foldr posRow posNil [tStr, tInt]) kwdNil (tStr)
scUStrFromIter          = tSchema [qbind c] $ tFun fxPure (foldr posRow posNil [tCon (TC qnIterable [tVar c,tStr]), tVar c]) kwdNil (tStr)
  where c = TV KType $ name "C"
scUStrContains          = monotype $ tFun fxPure (foldr posRow posNil [tStr, tStr]) kwdNil (tBool)
scUStrContainsnot       = monotype $ tFun fxPure (foldr posRow posNil [tStr, tStr]) kwdNil (tBool)
scUStrIter              = monotype $ tFun fxPure (foldr posRow posNil [tStr]) kwdNil (tIterator tStr)
scUStrGetslice          = monotype $ tFun fxPure (foldr posRow posNil [tStr, tSlice]) kwdNil (tStr)
scUBytesEq              = monotype $ tFun fxPure (foldr posRow posNil [tBytes, tBytes]) kwdNil (tBool)
scUBytesNe              = monotype $ tFun fxPure (foldr posRow posNil [tBytes, tBytes]) kwdNil (tBool)
scUBytesLt              = monotype $ tFun fxPure (foldr posRow posNil [tBytes, tBytes]) kwdNil (tBool)
scUBytesLe              = monotype $ tFun fxPure (foldr posRow posNil [tBytes, tBytes]) kwdNil (tBool)
scUBytesGt              = monotype $ tFun fxPure (foldr posRow posNil [tBytes, tBytes]) kwdNil (tBool)
scUBytesGe              = monotype $ tFun fxPure (foldr posRow posNil [tBytes, tBytes]) kwdNil (tBool)
scUBytesContains        = monotype $ tFun fxPure (foldr posRow posNil [tBytes, tInt]) kwdNil (tBool)
scUBytesContainsnot     = monotype $ tFun fxPure (foldr posRow posNil [tBytes, tInt]) kwdNil (tBool)
scUBytearrayEq          = monotype $ tFun fxPure (foldr posRow posNil [tBytearray, tBytearray]) kwdNil (tBool)
scUBytearrayNe          = monotype $ tFun fxPure (foldr posRow posNil [tBytearray, tBytearray]) kwdNil (tBool)
scUBytearrayLt          = monotype $ tFun fxPure (foldr posRow posNil [tBytearray, tBytearray]) kwdNil (tBool)
scUBytearrayLe          = monotype $ tFun fxPure (foldr posRow posNil [tBytearray, tBytearray]) kwdNil (tBool)
scUBytearrayGt          = monotype $ tFun fxPure (foldr posRow posNil [tBytearray, tBytearray]) kwdNil (tBool)
scUBytearrayGe          = monotype $ tFun fxPure (foldr posRow posNil [tBytearray, tBytearray]) kwdNil (tBool)
scUBytearrayContains    = monotype $ tFun fxPure (foldr posRow posNil [tBytearray, tInt]) kwdNil (tBool)
scUBytearrayContainsnot = monotype $ tFun fxPure (foldr posRow posNil [tBytearray, tInt]) kwdNil (tBool)
scUBytesHash            = monotype $ tFun fxPure (foldr posRow posNil [tBytes, tCon (TC (gBuiltin (name "hasher")) [])]) kwdNil (tNone)
scUBytesFromIter        = tSchema [qbind c] $ tFun fxPure (foldr posRow posNil [tCon (TC qnIterable [tVar c,tInt]), tVar c]) kwdNil (tBytes)
  where c = TV KType $ name "C"
scUBytesIter            = monotype $ tFun fxPure (foldr posRow posNil [tBytes]) kwdNil (tIterator tInt)
scUBytesGetslice        = monotype $ tFun fxPure (foldr posRow posNil [tBytes, tSlice]) kwdNil (tBytes)
scUBytesAdd             = monotype $ tFun fxPure (foldr posRow posNil [tBytes, tBytes]) kwdNil (tBytes)
scUBytesZero            = monotype $ tFun fxPure (foldr posRow posNil []) kwdNil (tBytes)
scUBytesMul             = monotype $ tFun fxPure (foldr posRow posNil [tBytes, tInt]) kwdNil (tBytes)
scUBytearrayInsert      = monotype $ tFun fxMut (foldr posRow posNil [tBytearray, tInt, tInt]) kwdNil (tNone)
scUBytearrayAppend      = monotype $ tFun fxMut (foldr posRow posNil [tBytearray, tInt]) kwdNil (tNone)
scUBytearrayReverse     = monotype $ tFun fxMut (foldr posRow posNil [tBytearray]) kwdNil (tNone)
scUBytearrayReversed    = monotype $ tFun fxPure (foldr posRow posNil [tBytearray]) kwdNil (tIterator tInt)
scUBytearrayGetslice    = monotype $ tFun fxPure (foldr posRow posNil [tBytearray, tSlice]) kwdNil (tBytearray)
scUBytearraySetslice    = tSchema [qbind c] $ tFun fxMut (foldr posRow posNil [tBytearray, tCon (TC qnIterable [tVar c,tInt]), tSlice, tVar c]) kwdNil (tNone)
  where c = TV KType $ name "C"
scUBytearrayDelslice    = monotype $ tFun fxMut (foldr posRow posNil [tBytearray, tSlice]) kwdNil (tNone)
scUBytearrayFromIter    = tSchema [qbind c] $ tFun fxPure (foldr posRow posNil [tCon (TC qnIterable [tVar c,tInt]), tVar c]) kwdNil (tBytearray)
  where c = TV KType $ name "C"
scUBytearrayIter        = monotype $ tFun fxPure (foldr posRow posNil [tBytearray]) kwdNil (tIterator tInt)
scUBytearrayAdd         = monotype $ tFun fxPure (foldr posRow posNil [tBytearray, tBytearray]) kwdNil (tBytearray)
scUBytearrayZero        = monotype $ tFun fxPure (foldr posRow posNil []) kwdNil (tBytearray)
scUBytearrayMul         = monotype $ tFun fxPure (foldr posRow posNil [tBytearray, tInt]) kwdNil (tBytearray)
scUBytearrayIadd        = monotype $ tFun fxMut (foldr posRow posNil [tBytearray, tBytearray]) kwdNil (tBytearray)
scUBytearrayImul        = monotype $ tFun fxMut (foldr posRow posNil [tBytearray, tInt]) kwdNil (tBytearray)

-- Bigint and complex remain boxed; only fixed native scalar results are raw.
scUBigintAdd             = monotype $ tFun fxPure (foldr posRow posNil [tBigint, tBigint]) kwdNil (tBigint)
scUBigintZero            = monotype $ tFun fxPure (foldr posRow posNil []) kwdNil (tBigint)
scUBigintComplex         = monotype $ tFun fxPure (foldr posRow posNil [tBigint]) kwdNil (tComplex)
scUBigintFromatom        = monotype $ tFun fxPure (foldr posRow posNil [tAtom]) kwdNil (tBigint)
scUBigintMul             = monotype $ tFun fxPure (foldr posRow posNil [tBigint, tBigint]) kwdNil (tBigint)
scUBigintPow             = monotype $ tFun fxPure (foldr posRow posNil [tBigint, tBigint]) kwdNil (tBigint)
scUBigintNeg             = monotype $ tFun fxPure (foldr posRow posNil [tBigint]) kwdNil (tBigint)
scUBigintPos             = monotype $ tFun fxPure (foldr posRow posNil [tBigint]) kwdNil (tBigint)
scUBigintReal            = tSchema [qbind r] $ tFun fxPure (foldr posRow posNil [tBigint, tCon (TC qnReal [tVar r])]) kwdNil (tVar r)
  where r = TV KType $ name "R"
scUBigintImag            = tSchema [qbind r] $ tFun fxPure (foldr posRow posNil [tBigint, tCon (TC qnReal [tVar r])]) kwdNil (tVar r)
  where r = TV KType $ name "R"
scUBigintAbs             = tSchema [qbind r] $ tFun fxPure (foldr posRow posNil [tBigint, tCon (TC qnReal [tVar r])]) kwdNil (tVar r)
  where r = TV KType $ name "R"
scUBigintConjugate       = monotype $ tFun fxPure (foldr posRow posNil [tBigint]) kwdNil (tBigint)
scUBigintFloat           = monotype $ tFun fxPure (foldr posRow posNil [tBigint]) kwdNil (tFloat)
scUBigintTrunc           = tSchema [qbind r] $ tFun fxPure (foldr posRow posNil [tBigint, tCon (TC qnIntegral [tVar r])]) kwdNil (tVar r)
  where r = TV KType $ name "R"
scUBigintFloor           = tSchema [qbind r] $ tFun fxPure (foldr posRow posNil [tBigint, tCon (TC qnIntegral [tVar r])]) kwdNil (tVar r)
  where r = TV KType $ name "R"
scUBigintCeil            = tSchema [qbind r] $ tFun fxPure (foldr posRow posNil [tBigint, tCon (TC qnIntegral [tVar r])]) kwdNil (tVar r)
  where r = TV KType $ name "R"
scUBigintRound           = monotype $ tFun fxPure (foldr posRow posNil [tBigint, tOpt tInt]) kwdNil (tBigint)
scUBigintNumerator       = tSchema [qbind r] $ tFun fxPure (foldr posRow posNil [tBigint, tCon (TC qnIntegral [tVar r])]) kwdNil (tVar r)
  where r = TV KType $ name "R"
scUBigintDenominator     = tSchema [qbind r] $ tFun fxPure (foldr posRow posNil [tBigint, tCon (TC qnIntegral [tVar r])]) kwdNil (tVar r)
  where r = TV KType $ name "R"
scUBigintInt             = monotype $ tFun fxPure (foldr posRow posNil [tBigint]) kwdNil (tInt)
scUBigintIndex           = monotype $ tFun fxPure (foldr posRow posNil [tBigint]) kwdNil (tInt)
scUBigintDivmod          = monotype $ tFun fxPure (foldr posRow posNil [tBigint, tBigint]) kwdNil (tTuple (posRow tBigint (posRow tBigint posNil)) kwdNil)
scUBigintFloordiv        = monotype $ tFun fxPure (foldr posRow posNil [tBigint, tBigint]) kwdNil (tBigint)
scUBigintMod             = monotype $ tFun fxPure (foldr posRow posNil [tBigint, tBigint]) kwdNil (tBigint)
scUBigintLshift          = monotype $ tFun fxPure (foldr posRow posNil [tBigint, tInt]) kwdNil (tBigint)
scUBigintRshift          = monotype $ tFun fxPure (foldr posRow posNil [tBigint, tInt]) kwdNil (tBigint)
scUBigintInvert          = monotype $ tFun fxPure (foldr posRow posNil [tBigint]) kwdNil (tBigint)
scUBigintAnd             = monotype $ tFun fxPure (foldr posRow posNil [tBigint, tBigint]) kwdNil (tBigint)
scUBigintOr              = monotype $ tFun fxPure (foldr posRow posNil [tBigint, tBigint]) kwdNil (tBigint)
scUBigintXor             = monotype $ tFun fxPure (foldr posRow posNil [tBigint, tBigint]) kwdNil (tBigint)
scUBigintSub             = monotype $ tFun fxPure (foldr posRow posNil [tBigint, tBigint]) kwdNil (tBigint)
scUBigintTruediv         = monotype $ tFun fxPure (foldr posRow posNil [tBigint, tBigint]) kwdNil (tFloat)
scUBigintEq              = monotype $ tFun fxPure (foldr posRow posNil [tBigint, tBigint]) kwdNil (tBool)
scUBigintNe              = monotype $ tFun fxPure (foldr posRow posNil [tBigint, tBigint]) kwdNil (tBool)
scUBigintLt              = monotype $ tFun fxPure (foldr posRow posNil [tBigint, tBigint]) kwdNil (tBool)
scUBigintLe              = monotype $ tFun fxPure (foldr posRow posNil [tBigint, tBigint]) kwdNil (tBool)
scUBigintGt              = monotype $ tFun fxPure (foldr posRow posNil [tBigint, tBigint]) kwdNil (tBool)
scUBigintGe              = monotype $ tFun fxPure (foldr posRow posNil [tBigint, tBigint]) kwdNil (tBool)
scUBigintHash            = monotype $ tFun fxPure (foldr posRow posNil [tBigint, tCon (TC (gBuiltin (name "hasher")) [])]) kwdNil (tNone)
scUComplexAdd            = monotype $ tFun fxPure (foldr posRow posNil [tComplex, tComplex]) kwdNil (tComplex)
scUComplexZero           = monotype $ tFun fxPure (foldr posRow posNil []) kwdNil (tComplex)
scUComplexComplex        = monotype $ tFun fxPure (foldr posRow posNil [tComplex]) kwdNil (tComplex)
scUComplexMul            = monotype $ tFun fxPure (foldr posRow posNil [tComplex, tComplex]) kwdNil (tComplex)
scUComplexFromatom       = monotype $ tFun fxPure (foldr posRow posNil [tAtom]) kwdNil (tComplex)
scUComplexPow            = monotype $ tFun fxPure (foldr posRow posNil [tComplex, tComplex]) kwdNil (tComplex)
scUComplexNeg            = monotype $ tFun fxPure (foldr posRow posNil [tComplex]) kwdNil (tComplex)
scUComplexPos            = monotype $ tFun fxPure (foldr posRow posNil [tComplex]) kwdNil (tComplex)
scUComplexReal           = tSchema [qbind r] $ tFun fxPure (foldr posRow posNil [tComplex, tCon (TC qnReal [tVar r])]) kwdNil (tVar r)
  where r = TV KType $ name "R"
scUComplexImag           = tSchema [qbind r] $ tFun fxPure (foldr posRow posNil [tComplex, tCon (TC qnReal [tVar r])]) kwdNil (tVar r)
  where r = TV KType $ name "R"
scUComplexAbs            = tSchema [qbind r] $ tFun fxPure (foldr posRow posNil [tComplex, tCon (TC qnReal [tVar r])]) kwdNil (tVar r)
  where r = TV KType $ name "R"
scUComplexConjugate      = monotype $ tFun fxPure (foldr posRow posNil [tComplex]) kwdNil (tComplex)
scUComplexTruediv        = monotype $ tFun fxPure (foldr posRow posNil [tComplex, tComplex]) kwdNil (tComplex)
scUComplexSub            = monotype $ tFun fxPure (foldr posRow posNil [tComplex, tComplex]) kwdNil (tComplex)
scUComplexEq             = monotype $ tFun fxPure (foldr posRow posNil [tComplex, tComplex]) kwdNil (tBool)
scUComplexNe             = monotype $ tFun fxPure (foldr posRow posNil [tComplex, tComplex]) kwdNil (tBool)
scUComplexHash           = monotype $ tFun fxPure (foldr posRow posNil [tComplex, tCon (TC (gBuiltin (name "hasher")) [])]) kwdNil (tNone)

scRaiseValueError   = tSchema [qbind a] tRaiseValErr
  where tRaiseValErr= tFun fxPure (posRow tStr posNil) kwdNil (tVar a)
        a           = TV KType (name "A")
        
-- $rangeD_U__next__ : range -> int
-- Raw range iteration primitive used when normalizing for-loops over range.
scUNext             = tSchema [] tUNext
  where tUNext      = tFun fxMut (posRow tRange posNil) kwdNil tInt

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
