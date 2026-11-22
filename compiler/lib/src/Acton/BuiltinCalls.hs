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

module Acton.BuiltinCalls where

-- Compiler-private native builtin calls introduced by normalization/boxing.
-- These are implementation choices of builtin methods, separate from Prim's
-- core language/runtime operations. The two catalogues share the internal
-- namespace; Env combines them without making Prim depend on this module.
import Acton.Syntax
import Acton.Builtin
import Acton.NameInfo
import Acton.Prim (gPrim, mPrim, primBoolTuple)
import Utils (SrcLoc(..))
import qualified Data.Map.Strict as Map

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

-- Compiler-private descriptions of concrete builtin calls. The schemas keep
-- generic elements boxed; CodeGen adapts containers to their shared C views.
-- NativeCompare uses the sign of an ordinary C comparison function's result.
data BuiltinCallABI = DictBase | SetTable | SetObject | SetBuild QName
                    | ListBase | ListBuild QName | ListInplace | NativeArgs | NativeCompare String
                    | BoxedValue String | OptionalShow String

data BuiltinWorker = BuiltinWorker {
    bwType :: QName, bwMethod :: Name, bwArity :: Int,
    bwName :: QName, bwSchema :: TSchema, bwCName :: String,
    bwABI :: BuiltinCallABI, bwNegate :: Bool, bwReverse :: Bool,
    bwExtraArgs :: [String], bwContainerArgs :: Int
    }

hashContainerWorker q attr arity = Map.lookup (q,attr,arity) hashContainerMethodMap

builtinCallPrimitive q = Map.lookup q builtinCallPrimitiveMap

hashContainerMethodMap = Map.fromList [ ((bwType w,bwMethod w,bwArity w),w) | w <- hashContainerWorkers ]
builtinCallPrimitiveMap = Map.fromList [ (bwName w,w) | w <- builtinCallWorkers ]

builtinCallWorkers = hashContainerWorkers ++ listBaseWorkers ++ numericNativeWorkers ++ textNativeWorkers ++
                     map snd showBoolWorkers ++ map optionalShowWorker (filter ((== strKW) . bwMethod . snd) showBoolWorkers)

-- Key by implementation identity, not the receiver's apparent type. Inherited
-- root defaults consequently work for any user class, while a supplied generic
-- witness or a user override cannot be replaced merely because Self is known.
showBoolWorker impl attr = Map.lookup (impl,attr) showBoolMethodMap
showBoolMethodMap = Map.fromList [((q,bwMethod w),w) | (q,w) <- showBoolWorkers]
isShowBoolImplementation q = Map.member q showBoolImplementationMap
showBoolImplementationMap = Map.fromList [(q,()) | (q,_) <- showBoolWorkers]

showBoolWorkers = concatMap scalar
    ["value","object","int","i32","i16","i8","u64","u32","u16","u8","u1",
     "bigint","float","bool","complex","slice","range","hasher","bitarray","bitset","str","bytes","bytearray"] ++
    [ make (gBuiltin (Derived nBoolProtocol (name n))) (gBuiltin $ name n) boolKW ("B_" ++ n ++ "_bool") ("B_" ++ n)
      | n <- ["list","ilist","array","dict","idict","set","iset"] ] ++
    [ make (gBuiltin (Derived nShow (name n))) (gBuiltin $ name n) attr native ctype
      | n <- ["BaseException","Exception","IndexError","KeyError","FileNotFoundError"],
        let prefix = if n == "Exception" then "B_exception" else "B_" ++ n,
        let ctype = if n == "Exception" then "B_BaseException" else "B_" ++ n,
        (attr,native) <- [(strKW,prefix ++ "_str"),(reprKW,prefix ++ "_repr")] ] ++
    [ make primBoolTuple qnValue boolKW "B_tuple_bool" "B_tuple" ]
  where scalar n =
          let q = gBuiltin (name n)
              root = n `elem` ["value","object"]
              native attr = "B_" ++ (if n == "object" then "value" else n) ++ (if root then "D_" ++ nstr attr else
                              if attr == boolKW then "_bool" else if attr == strKW then "_str" else "_repr")
              showImpl = gBuiltin (Derived nShow (name n))
              boolImpl = gBuiltin (Derived nBoolProtocol (name n))
              ctype = "B_" ++ (if n == "object" then "value" else n)
          in [ make showImpl q strKW (native strKW) ctype,
               make showImpl q reprKW (if n == "object" then "B_valueD___repr__" else native reprKW)
                    (if n == "object" then "B_value" else "B_" ++ n),
               make boolImpl q boolKW (native boolKW) ctype ]
        make impl q attr native ctype = (impl, BuiltinWorker q attr 1
            (GName mPrim $ Derived (noq impl) (Derived (name "native") attr)) schema
            native (BoxedValue ctype) False False [] 1)
          where a = TV KType (name "A")
                -- A is deliberately generic: native scalar formatters consume
                -- a box. Only truth of known unboxable scalars is lowered raw.
                schema = tSchema [qbind a] $ tFun fxPure (posRow (tVar a) posNil) kwdNil
                           (if attr == boolKW then TUnboxed NoLoc tBool else tStr)

-- str's optional/default argument must be evaluated once and checked for None
-- before calling a statically selected native formatter. CodeGen also copies
-- a subclass result from str's identity formatter to the base str class.
optionalShowWorker (_,w) = w{ bwName = GName mPrim (Derived (noq $ bwName w) initKW),
                             bwABI = case bwABI w of BoxedValue t -> OptionalShow t }

-- Reuse the existing boxing schemas. List and ilist differ only in the result
-- class for allocating operations; input Iterable witnesses remain explicit.
-- The IR names are private: generated C calls the B_list_* entry points below.
listBaseWorkers =
    [ make qnList "__getitem__" 2 primUGetItem scUGetItem "B_list_base_getitem" ListBase 1 []
    , make qnIList "__getitem__" 2 primUIListGetItem scUIListGetItem "B_list_base_getitem" ListBase 1 []
    , make qnList "append" 2 primUListAppend scUListAppend "B_list_base_append" ListBase 1 []
    , make qnList "__setitem__" 3 primUListSetItem scUListSetItem "B_list_base_setitem" ListBase 1 []
    , make qnList "__delitem__" 2 primUListDelItem scUListDelItem "B_list_base_delitem" ListBase 1 []
    , make qnList "__len__" 1 primUListLen scUListLen "B_list_base_len" ListBase 1 []
    , make qnIList "__len__" 1 primUIListLen scUIListLen "B_list_base_len" ListBase 1 []
    , make qnList "__getslice__" 2 primUListGetSlice scUListGetSlice "B_list_base_getslice" (ListBuild qnList) 1 []
    , make qnIList "__getslice__" 2 primUIListGetSlice scUIListGetSlice "B_list_base_getslice" (ListBuild qnIList) 1 []
    , make qnList "__iter__" 1 primUListIter scUListIter "B_list_base_iter" ListBase 1 []
    , make qnIList "__iter__" 1 primUIListIter scUIListIter "B_list_base_iter" ListBase 1 []
    , make qnList "__reversed__" 1 primUListReversed scUListReversed "B_list_base_reversed" (ListBuild qnList) 1 []
    , make qnIList "__reversed__" 1 primUIListReversed scUIListReversed "B_list_base_reversed" (ListBuild qnIList) 1 []
    , make qnList "__add__" 2 primUListAdd scUListAdd "B_list_base_add" (ListBuild qnList) 2 []
    , make qnIList "__add__" 2 primUIListAdd scUIListAdd "B_list_base_add" (ListBuild qnIList) 2 []
    , make qnList "__mul__" 2 primUListMul scUListMul "B_list_base_mul" (ListBuild qnList) 1 []
    , make qnIList "__mul__" 2 primUIListMul scUIListMul "B_list_base_mul" (ListBuild qnIList) 1 []
    , make qnList "__zero__" 0 primUListZero scUListZero "B_list_base_new" (ListBuild qnList) 0 ["0"]
    , make qnIList "__zero__" 0 primUIListZero scUIListZero "B_list_base_new" (ListBuild qnIList) 0 ["0"]
    , make qnList "__fromiter__" 2 primUListFromIter scUListFromIter "B_listG_new" NativeArgs 0 []
    , make qnIList "__fromiter__" 2 primUIListFromIter scUIListFromIter "B_ilistG_new" NativeArgs 0 []
    , make qnList "insert" 3 primUListInsert scUListInsert "B_list_base_insert" ListBase 1 []
    , make qnList "reverse" 1 primUListReverse scUListReverse "B_list_base_reverse" ListBase 1 []
    , make qnList "__delslice__" 2 primUListDelSlice scUListDelSlice "B_list_base_delslice" ListBase 1 []
    , make qnList "__setslice__" 4 primUListSetSlice scUListSetSlice "B_list_base_setslice" ListBase 1 []
    , make qnList "__iadd__" 2 primUListIAdd scUListIAdd "B_list_base_iadd" ListInplace 2 []
    , make qnList "__imul__" 2 primUListIMul scUListIMul "B_list_base_irepeat" ListInplace 1 []
    ]
  where make q attr arity prim schema native abi count extra = BuiltinWorker
            q (name attr) arity prim schema native abi False False extra count

-- Numeric workers already perform the Acton allocation and arithmetic.
-- Keep their representation schemas and result-conversion witnesses intact;
-- only their native C entry point names change.
numericNativeWorkers =
    [ make qnBigint "__add__" 2 primUBigintAdd scUBigintAdd "B_bigint_add"
    , make qnBigint "__zero__" 0 primUBigintZero scUBigintZero "B_bigint_zero"
    , make qnBigint "__complex__" 1 primUBigintComplex scUBigintComplex "B_bigint_complex"
    , make qnBigint "__fromatom__" 1 primUBigintFromatom scUBigintFromatom "B_bigint_fromatom"
    , make qnBigint "__mul__" 2 primUBigintMul scUBigintMul "B_bigint_mul"
    , make qnBigint "__pow__" 2 primUBigintPow scUBigintPow "B_bigint_pow"
    , make qnBigint "__neg__" 1 primUBigintNeg scUBigintNeg "B_bigint_neg"
    , make qnBigint "__pos__" 1 primUBigintPos scUBigintPos "B_bigint_pos"
    , make qnBigint "real" 2 primUBigintReal scUBigintReal "B_bigint_real"
    , make qnBigint "imag" 2 primUBigintImag scUBigintImag "B_bigint_imag"
    , make qnBigint "__abs__" 2 primUBigintAbs scUBigintAbs "B_bigint_abs"
    , make qnBigint "conjugate" 1 primUBigintConjugate scUBigintConjugate "B_bigint_conjugate"
    , make qnBigint "__float__" 1 primUBigintFloat scUBigintFloat "B_bigint_float"
    , make qnBigint "__trunc__" 2 primUBigintTrunc scUBigintTrunc "B_bigint_trunc"
    , make qnBigint "__floor__" 2 primUBigintFloor scUBigintFloor "B_bigint_floor"
    , make qnBigint "__ceil__" 2 primUBigintCeil scUBigintCeil "B_bigint_ceil"
    , make qnBigint "__round__" 2 primUBigintRound scUBigintRound "B_bigint_round"
    , make qnBigint "numerator" 2 primUBigintNumerator scUBigintNumerator "B_bigint_numerator"
    , make qnBigint "denominator" 2 primUBigintDenominator scUBigintDenominator "B_bigint_denominator"
    , make qnBigint "__int__" 1 primUBigintInt scUBigintInt "B_bigint_int"
    , make qnBigint "__index__" 1 primUBigintIndex scUBigintIndex "B_bigint_index"
    , make qnBigint "__divmod__" 2 primUBigintDivmod scUBigintDivmod "B_bigint_divmod"
    , make qnBigint "__floordiv__" 2 primUBigintFloordiv scUBigintFloordiv "B_bigint_floordiv"
    , make qnBigint "__mod__" 2 primUBigintMod scUBigintMod "B_bigint_mod"
    , make qnBigint "__lshift__" 2 primUBigintLshift scUBigintLshift "B_bigint_lshift"
    , make qnBigint "__rshift__" 2 primUBigintRshift scUBigintRshift "B_bigint_rshift"
    , make qnBigint "__invert__" 1 primUBigintInvert scUBigintInvert "B_bigint_invert"
    , make qnBigint "__and__" 2 primUBigintAnd scUBigintAnd "B_bigint_and"
    , make qnBigint "__or__" 2 primUBigintOr scUBigintOr "B_bigint_or"
    , make qnBigint "__xor__" 2 primUBigintXor scUBigintXor "B_bigint_xor"
    , make qnBigint "__sub__" 2 primUBigintSub scUBigintSub "B_bigint_sub"
    , make qnBigint "__truediv__" 2 primUBigintTruediv scUBigintTruediv "B_bigint_truediv"
    , make qnBigint "__eq__" 2 primUBigintEq scUBigintEq "B_bigint_eq"
    , make qnBigint "__ne__" 2 primUBigintNe scUBigintNe "B_bigint_ne"
    , make qnBigint "__lt__" 2 primUBigintLt scUBigintLt "B_bigint_lt"
    , make qnBigint "__le__" 2 primUBigintLe scUBigintLe "B_bigint_le"
    , make qnBigint "__gt__" 2 primUBigintGt scUBigintGt "B_bigint_gt"
    , make qnBigint "__ge__" 2 primUBigintGe scUBigintGe "B_bigint_ge"
    , make qnBigint "hash" 2 primUBigintHash scUBigintHash "B_bigint_hash"
    , make qnComplex "__add__" 2 primUComplexAdd scUComplexAdd "B_complex_add"
    , make qnComplex "__zero__" 0 primUComplexZero scUComplexZero "B_complex_zero"
    , make qnComplex "__complex__" 1 primUComplexComplex scUComplexComplex "B_complex_complex"
    , make qnComplex "__mul__" 2 primUComplexMul scUComplexMul "B_complex_mul"
    , make qnComplex "__fromatom__" 1 primUComplexFromatom scUComplexFromatom "B_complex_fromatom"
    , make qnComplex "__pow__" 2 primUComplexPow scUComplexPow "B_complex_pow"
    , make qnComplex "__neg__" 1 primUComplexNeg scUComplexNeg "B_complex_neg"
    , make qnComplex "__pos__" 1 primUComplexPos scUComplexPos "B_complex_pos"
    , make qnComplex "real" 2 primUComplexReal scUComplexReal "B_complex_real"
    , make qnComplex "imag" 2 primUComplexImag scUComplexImag "B_complex_imag"
    , make qnComplex "__abs__" 2 primUComplexAbs scUComplexAbs "B_complex_abs"
    , make qnComplex "conjugate" 1 primUComplexConjugate scUComplexConjugate "B_complex_conjugate"
    , make qnComplex "__truediv__" 2 primUComplexTruediv scUComplexTruediv "B_complex_truediv"
    , make qnComplex "__sub__" 2 primUComplexSub scUComplexSub "B_complex_sub"
    , make qnComplex "__eq__" 2 primUComplexEq scUComplexEq "B_complex_eq"
    , make qnComplex "__ne__" 2 primUComplexNe scUComplexNe "B_complex_ne"
    , make qnComplex "hash" 2 primUComplexHash scUComplexHash "B_complex_hash"
    ]
  where make q attr arity prim schema native = BuiltinWorker
            q (name attr) arity prim schema native NativeArgs False False [] 0

-- Text operations have native typed implementations. Comparisons reuse the
-- equality/ordering worker instead of adding a forwarding C function; negate
-- or reverse only normalized arguments, each evaluated once. Iterable inputs
-- and the mutable bytearray allocation policy retain their existing ABI.
textNativeWorkers =
    [ make qnStr "__len__" 1 primUStrLen scUStrLen "B_str_len" NativeArgs
    , make qnBytes "__len__" 1 primUBytesLen scUBytesLen "B_bytes_len" NativeArgs
    , make qnBytearray "__len__" 1 primUBytearrayLen scUBytearrayLen "B_bytearray_len" NativeArgs
    , make qnStr "__getitem__" 2 primUStrGetItem scUStrGetItem "B_str_getitem" NativeArgs
    , make qnStr "__eq__" 2 primUStrEq scUStrCompare "B_str_eq" NativeArgs
    , (make qnStr "__ne__" 2 primUStrNe scUStrCompare "B_str_eq" NativeArgs){bwNegate=True}
    , make qnStr "__add__" 2 primUStrAdd scUStrAdd "B_str_add" NativeArgs
    , make qnBytes "__getitem__" 2 primUBytesGetItem scUBytesGetItem "B_bytes_getitem" NativeArgs
    , make qnBytearray "__getitem__" 2 primUBytearrayGetItem scUBytearrayGetItem "B_bytearray_getitem" NativeArgs
    , make qnBytearray "__setitem__" 3 primUBytearraySetItem scUBytearraySetItem "B_bytearray_setitem" NativeArgs
    , make qnBytearray "__delitem__" 2 primUBytearrayDelItem scUBytearrayDelItem "B_bytearray_delitem" NativeArgs
    , make qnStr "__lt__" 2 primUStrLt scUStrLt "B_str_compare" (NativeCompare "<")
    , make qnStr "__le__" 2 primUStrLe scUStrLe "B_str_compare" (NativeCompare "<=")
    , make qnStr "__gt__" 2 primUStrGt scUStrGt "B_str_compare" (NativeCompare ">")
    , make qnStr "__ge__" 2 primUStrGe scUStrGe "B_str_compare" (NativeCompare ">=")
    , make qnStr "hash" 2 primUStrHash scUStrHash "B_str_hash" NativeArgs
    , make qnStr "__zero__" 0 primUStrZero scUStrZero "B_str_zero" NativeArgs
    , make qnStr "__mul__" 2 primUStrMul scUStrMul "B_str_mul" NativeArgs
    , make qnStr "__fromiter__" 2 primUStrFromIter scUStrFromIter "B_str_fromiter" NativeArgs
    , make qnStr "__contains__" 2 primUStrContains scUStrContains "B_str_contains" NativeArgs
    , (make qnStr "__containsnot__" 2 primUStrContainsnot scUStrContainsnot "B_str_contains" NativeArgs){bwNegate=True}
    , make qnStr "__iter__" 1 primUStrIter scUStrIter "B_str_iter" NativeArgs
    , make qnStr "__getslice__" 2 primUStrGetslice scUStrGetslice "B_str_getslice" NativeArgs
    , make qnBytes "__eq__" 2 primUBytesEq scUBytesEq "B_bytes_eq" NativeArgs
    , (make qnBytes "__ne__" 2 primUBytesNe scUBytesNe "B_bytes_eq" NativeArgs){bwNegate=True}
    , make qnBytes "__lt__" 2 primUBytesLt scUBytesLt "B_bytes_lt" NativeArgs
    , (make qnBytes "__le__" 2 primUBytesLe scUBytesLe "B_bytes_lt" NativeArgs){bwNegate=True, bwReverse=True}
    , (make qnBytes "__gt__" 2 primUBytesGt scUBytesGt "B_bytes_lt" NativeArgs){bwReverse=True}
    , (make qnBytes "__ge__" 2 primUBytesGe scUBytesGe "B_bytes_lt" NativeArgs){bwNegate=True}
    , make qnBytes "__contains__" 2 primUBytesContains scUBytesContains "B_bytes_contains" NativeArgs
    , (make qnBytes "__containsnot__" 2 primUBytesContainsnot scUBytesContainsnot "B_bytes_contains" NativeArgs){bwNegate=True}
    , make qnBytearray "__eq__" 2 primUBytearrayEq scUBytearrayEq "B_bytearray_eq" NativeArgs
    , (make qnBytearray "__ne__" 2 primUBytearrayNe scUBytearrayNe "B_bytearray_eq" NativeArgs){bwNegate=True}
    , make qnBytearray "__lt__" 2 primUBytearrayLt scUBytearrayLt "B_bytearray_lt" NativeArgs
    , (make qnBytearray "__le__" 2 primUBytearrayLe scUBytearrayLe "B_bytearray_lt" NativeArgs){bwNegate=True, bwReverse=True}
    , (make qnBytearray "__gt__" 2 primUBytearrayGt scUBytearrayGt "B_bytearray_lt" NativeArgs){bwReverse=True}
    , (make qnBytearray "__ge__" 2 primUBytearrayGe scUBytearrayGe "B_bytearray_lt" NativeArgs){bwNegate=True}
    , make qnBytearray "__contains__" 2 primUBytearrayContains scUBytearrayContains "B_bytearray_contains" NativeArgs
    , (make qnBytearray "__containsnot__" 2 primUBytearrayContainsnot scUBytearrayContainsnot "B_bytearray_contains" NativeArgs){bwNegate=True}
    , make qnBytes "hash" 2 primUBytesHash scUBytesHash "B_bytes_hash" NativeArgs
    , make qnBytes "__fromiter__" 2 primUBytesFromIter scUBytesFromIter "B_bytesG_new" NativeArgs
    , make qnBytes "__iter__" 1 primUBytesIter scUBytesIter "B_bytes_iter" NativeArgs
    , make qnBytes "__getslice__" 2 primUBytesGetslice scUBytesGetslice "B_bytes_getslice" NativeArgs
    , make qnBytes "__add__" 2 primUBytesAdd scUBytesAdd "B_bytes_add" NativeArgs
    , make qnBytes "__zero__" 0 primUBytesZero scUBytesZero "B_bytes_zero" NativeArgs
    , make qnBytes "__mul__" 2 primUBytesMul scUBytesMul "B_bytes_mul" NativeArgs
    , make qnBytearray "insert" 3 primUBytearrayInsert scUBytearrayInsert "B_bytearray_insert" NativeArgs
    , make qnBytearray "append" 2 primUBytearrayAppend scUBytearrayAppend "B_bytearray_append" NativeArgs
    , make qnBytearray "reverse" 1 primUBytearrayReverse scUBytearrayReverse "B_bytearray_reverse" NativeArgs
    , make qnBytearray "__reversed__" 1 primUBytearrayReversed scUBytearrayReversed "B_bytearray_reversed" NativeArgs
    , make qnBytearray "__getslice__" 2 primUBytearrayGetslice scUBytearrayGetslice "B_bytearray_getslice" NativeArgs
    , make qnBytearray "__setslice__" 4 primUBytearraySetslice scUBytearraySetslice "B_bytearray_setslice" NativeArgs
    , make qnBytearray "__delslice__" 2 primUBytearrayDelslice scUBytearrayDelslice "B_bytearray_delslice" NativeArgs
    , make qnBytearray "__fromiter__" 2 primUBytearrayFromIter scUBytearrayFromIter "B_bytearray_fromiter" NativeArgs
    , make qnBytearray "__iter__" 1 primUBytearrayIter scUBytearrayIter "B_bytearray_iter" NativeArgs
    , make qnBytearray "__add__" 2 primUBytearrayAdd scUBytearrayAdd "B_bytearray_add" NativeArgs
    , make qnBytearray "__zero__" 0 primUBytearrayZero scUBytearrayZero "B_bytearray_zero" NativeArgs
    , make qnBytearray "__mul__" 2 primUBytearrayMul scUBytearrayMul "B_bytearray_mul" NativeArgs
    , make qnBytearray "__iadd__" 2 primUBytearrayIadd scUBytearrayIadd "B_bytearray_iadd" NativeArgs
    , make qnBytearray "__imul__" 2 primUBytearrayImul scUBytearrayImul "B_bytearray_imul" NativeArgs
    ]
  where make q attr arity prim schema native abi = BuiltinWorker
            q (name attr) arity prim schema native abi False False [] 0

-- Hash-container factories keep their protocol witness, which supplies
-- Hashable before there is a container. Dict comparisons retain value Eq.
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
            BuiltinWorker q (name attr) (length args)
                (gPrim (nstr (noq q) ++ "_base_" ++ attr))
                (tSchema qs $ tFun fx (foldr posRow posNil args) kwdNil result)
                native abi neg rev ["B_None" | def] (length $ takeWhile (== self) args)
        dictWorkers q cons mutable =
          let self = cons at bt
              make attr fx args result native def = worker q self [qbind a,qbind b]
                  attr fx args result native DictBase False False def
          in [ (make "__getitem__" fxPure [self,at,tCon (TC qnShow [at])] bt "B_dict_base_getitem" False){bwArity=2}
             , make "__contains__" fxPure [self,at] tBool "B_dict_base_contains" False
             , (make "__containsnot__" fxPure [self,at] tBool "B_dict_base_contains" False){bwNegate=True}
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

-- Keep the existing lookup precedence: table-driven native calls precede
-- core primitives, and the older direct native entries follow them.
withBuiltinCalls :: TEnv -> TEnv
withBuiltinCalls core =
    [ (noq (bwName w), NDef (bwSchema w) NoDec Nothing) | w <- builtinCallWorkers ] ++ core ++
    [ (noq primUDictLen, NDef scUDictLen NoDec Nothing)
    , (noq primUIDictLen, NDef scUIDictLen NoDec Nothing)
    , (noq primUSetLen, NDef scUSetLen NoDec Nothing)
    , (noq primUISetLen, NDef scUISetLen NoDec Nothing)
    , (noq primUBitsetLen, NDef scUBitsetLen NoDec Nothing)
    , (noq primUArrayGetInt, NDef scUArrayGetInt NoDec Nothing)
    , (noq primUArrayGetFloat, NDef scUArrayGetFloat NoDec Nothing)
    , (noq primUArraySetInt, NDef scUArraySetInt NoDec Nothing)
    , (noq primUArraySetFloat, NDef scUArraySetFloat NoDec Nothing)
    , (noq primUArrayLen, NDef scUArrayLen NoDec Nothing)
    , (noq primUBitarrayGetItem, NDef scUBitarrayGetItem NoDec Nothing)
    , (noq primUBitarraySetItem, NDef scUBitarraySetItem NoDec Nothing)
    , (noq primUBitarrayLen, NDef scUBitarrayLen NoDec Nothing)
    , (noq primUBitsetContains, NDef scUBitsetContains NoDec Nothing)
    , (noq primUBitsetContainsNot, NDef scUBitsetContains NoDec Nothing)
    , (noq primUBitsetAdd, NDef scUBitsetMutate NoDec Nothing)
    , (noq primUBitsetDiscard, NDef scUBitsetMutate NoDec Nothing)
    , (noq primUNext,         NDef scUNext NoDec Nothing)
    ]

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

-- $rangeD_U__next__ : range -> int
-- Raw range iteration primitive used when normalizing for-loops over range.
scUNext             = tSchema [] tUNext
  where tUNext      = tFun fxMut (posRow tRange posNil) kwdNil tInt
