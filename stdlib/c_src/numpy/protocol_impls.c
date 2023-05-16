/*
 * Copyright (C) 2019-2021 Data Ductus AB
 *
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

// numpyQ_IntegralD_ndarrayD_int /////////////////////////////////////////////////////////////////////////////////////////////

B_NoneType numpyQ_IntegralD_ndarrayD_intD___init__(numpyQ_IntegralD_ndarrayD_int wit) {
    wit->W_Logical = (B_Logical)$NEW(numpyQ_LogicalD_ndarrayD_int,(B_Integral)wit);
    wit->W_Minus = (B_Minus)$NEW(numpyQ_MinusD_ndarrayD_int,(B_Integral)wit);
    return B_None;
}; 

numpyQ_IntegralD_ndarrayD_int numpyQ_IntegralD_ndarrayD_intG_new() {
    numpyQ_IntegralD_ndarrayD_int res = malloc(sizeof (numpyQ_IntegralD_ndarrayD_int));
    res->$class = &numpyQ_IntegralD_ndarrayD_intG_methods;
    numpyQ_IntegralD_ndarrayD_intD___init__(res);
    return res;
}


void numpyQ_IntegralD_ndarrayD_intD___serialize__(numpyQ_IntegralD_ndarrayD_int wit, $Serial$state state) {
    $step_serialize(wit->W_Logical, state);
    $step_serialize(wit->W_Minus, state);
}

numpyQ_IntegralD_ndarrayD_int numpyQ_IntegralD_ndarrayD_intD___deserialize__(numpyQ_IntegralD_ndarrayD_int wit, $Serial$state state) {
    numpyQ_IntegralD_ndarrayD_int res = $DNEW(numpyQ_IntegralD_ndarrayD_int,state);
    res->W_Logical = (B_Logical)$step_deserialize(state);
    res->W_Minus = (B_Minus)$step_deserialize(state);
    return res;
}

numpyQ_ndarray numpyQ_IntegralD_ndarrayD_intD___add__(numpyQ_IntegralD_ndarrayD_int wit, numpyQ_ndarray a, numpyQ_ndarray b){
    return numpyQ_oper(numpyQ_PrimitiveD_intG_witness->$class->$add,a,b);
}

numpyQ_ndarray numpyQ_IntegralD_ndarrayD_intD___fromatom__(numpyQ_IntegralD_ndarrayD_int wit,B_atom a) {
    return numpyQ_fromatom((numpyQ_Primitive)numpyQ_PrimitiveD_intG_witness,a);
}

B_complex numpyQ_IntegralD_ndarrayD_intD___complx__(numpyQ_IntegralD_ndarrayD_int wit, numpyQ_ndarray a) {
    $RAISE((B_BaseException)$NEW(B_NotImplementedError,to$str("complex not implemented for ndarray")));
    return NULL;
}

numpyQ_ndarray numpyQ_IntegralD_ndarrayD_intD___mul__(numpyQ_IntegralD_ndarrayD_int wit, numpyQ_ndarray a, numpyQ_ndarray b) {
    return numpyQ_oper(numpyQ_PrimitiveD_intG_witness->$class->$mul,a,b);
}

numpyQ_ndarray numpyQ_IntegralD_ndarrayD_intD___pow__(numpyQ_IntegralD_ndarrayD_int wit, numpyQ_ndarray a, numpyQ_ndarray b) {
    return numpyQ_oper(numpyQ_PrimitiveD_intG_witness->$class->B_pow,a,b);
}

numpyQ_ndarray numpyQ_IntegralD_ndarrayD_intD___neg__(numpyQ_IntegralD_ndarrayD_int wit, numpyQ_ndarray a) {
    return numpyQ_func(numpyQ_PrimitiveD_intG_witness->$class->$neg,a);
}

numpyQ_ndarray numpyQ_IntegralD_ndarrayD_intD___pos__(numpyQ_IntegralD_ndarrayD_int wit, numpyQ_ndarray a) {
    return a;
}

$WORD numpyQ_IntegralD_ndarrayD_int$real(numpyQ_IntegralD_ndarrayD_int wit, numpyQ_ndarray a, B_Real wit2) {
    return a;
}
$WORD numpyQ_IntegralD_ndarrayD_int$imag(numpyQ_IntegralD_ndarrayD_int wit, numpyQ_ndarray a, B_Real wit2);
$WORD numpyQ_IntegralD_ndarrayD_intD___abs__(numpyQ_IntegralD_ndarrayD_int wit, numpyQ_ndarray a, B_Real wit2) {
    return numpyQ_func(numpyQ_PrimitiveD_intG_witness->$class->B_abs,a);
}
numpyQ_ndarray numpyQ_IntegralD_ndarrayD_int$conjugate(numpyQ_IntegralD_ndarrayD_int wit, numpyQ_ndarray a);
B_float numpyQ_IntegralD_ndarrayD_intD___float__ (numpyQ_IntegralD_ndarrayD_int wit, numpyQ_ndarray a);
$WORD numpyQ_IntegralD_ndarrayD_intD___trunc__ (numpyQ_IntegralD_ndarrayD_int wit, numpyQ_ndarray a, B_Integral wit2);
$WORD numpyQ_IntegralD_ndarrayD_intD___floor__ (numpyQ_IntegralD_ndarrayD_int wit, numpyQ_ndarray a, B_Integral wit2);
$WORD numpyQ_IntegralD_ndarrayD_intD___ceil__ (numpyQ_IntegralD_ndarrayD_int wit, numpyQ_ndarray a, B_Integral wit2);
numpyQ_ndarray numpyQ_IntegralD_ndarrayD_intD___round__ (numpyQ_IntegralD_ndarrayD_int wit, numpyQ_ndarray a, numpyQ_ndarray b);
$WORD numpyQ_IntegralD_ndarrayD_int$numerator (numpyQ_IntegralD_ndarrayD_int wit, numpyQ_ndarray a, B_Integral wit2);
$WORD numpyQ_IntegralD_ndarrayD_int$denominator (numpyQ_IntegralD_ndarrayD_int wit, numpyQ_ndarray a, B_Integral wit2);
numpyQ_ndarray numpyQ_IntegralD_ndarrayD_intD___int__ (numpyQ_IntegralD_ndarrayD_int wit, numpyQ_ndarray a);
numpyQ_ndarray numpyQ_IntegralD_ndarrayD_intD___index__ (numpyQ_IntegralD_ndarrayD_int wit, numpyQ_ndarray a);
B_tuple numpyQ_IntegralD_ndarrayD_intD___divmod__ (numpyQ_IntegralD_ndarrayD_int wit, numpyQ_ndarray a, numpyQ_ndarray b);
numpyQ_ndarray numpyQ_IntegralD_ndarrayD_intD___floordiv__ (numpyQ_IntegralD_ndarrayD_int wit, numpyQ_ndarray a, numpyQ_ndarray b) {
    return numpyQ_oper(B_l_floordiv,a,b);
}  
numpyQ_ndarray numpyQ_IntegralD_ndarrayD_intD___mod__ (numpyQ_IntegralD_ndarrayD_int wit, numpyQ_ndarray a, numpyQ_ndarray b);
numpyQ_ndarray numpyQ_IntegralD_ndarrayD_intD___lshift__ (numpyQ_IntegralD_ndarrayD_int wit, numpyQ_ndarray a, numpyQ_ndarray b);
numpyQ_ndarray numpyQ_IntegralD_ndarrayD_intD___rshift__ (numpyQ_IntegralD_ndarrayD_int wit, numpyQ_ndarray a, numpyQ_ndarray b);
numpyQ_ndarray numpyQ_IntegralD_ndarrayD_intD___invert__ (numpyQ_IntegralD_ndarrayD_int wit, numpyQ_ndarray a);

// numpyQ_LogicalD_ndarrayD_int //////////////////////////////////////////////////////////////////////////////

B_NoneType numpyQ_LogicalD_ndarrayD_intD___init__(numpyQ_LogicalD_ndarrayD_int wit, B_Integral W_Integral) {
    wit->W_Integral =  W_Integral;
    return B_None;
};

numpyQ_LogicalD_ndarrayD_int numpyQ_LogicalD_ndarrayD_intG_new(B_Integral W_Integral) {
    numpyQ_LogicalD_ndarrayD_int res = malloc(sizeof (numpyQ_LogicalD_ndarrayD_int));
    res->$class = &numpyQ_LogicalD_ndarrayD_intG_methods;
    numpyQ_LogicalD_ndarrayD_intD___init__(res, W_Integral);
    return res;
}
void numpyQ_LogicalD_ndarrayD_intD___serialize__(numpyQ_LogicalD_ndarrayD_int wit, $Serial$state state) {
    $step_serialize(wit->W_Integral, state);
}

numpyQ_LogicalD_ndarrayD_int numpyQ_LogicalD_ndarrayD_intD___deserialize__(numpyQ_LogicalD_ndarrayD_int wit, $Serial$state state) {
    numpyQ_LogicalD_ndarrayD_int res = $DNEW(numpyQ_LogicalD_ndarrayD_int,state);
    res->W_Integral = (B_Integral)$step_deserialize(state);
    return res;
}

numpyQ_ndarray numpyQ_LogicalD_ndarrayD_intD___and__ (numpyQ_LogicalD_ndarrayD_int wit, numpyQ_ndarray a, numpyQ_ndarray b) {
    return numpyQ_oper(numpyQ_PrimitiveD_intG_witness->$class->$band,a,b);
}
numpyQ_ndarray numpyQ_LogicalD_ndarrayD_intD___or__ (numpyQ_LogicalD_ndarrayD_int wit, numpyQ_ndarray a, numpyQ_ndarray b) {
    return numpyQ_oper(numpyQ_PrimitiveD_intG_witness->$class->$bor,a,b);
}
numpyQ_ndarray numpyQ_LogicalD_ndarrayD_intD___xor__ (numpyQ_LogicalD_ndarrayD_int wit, numpyQ_ndarray a, numpyQ_ndarray b) {
    return numpyQ_oper(numpyQ_PrimitiveD_intG_witness->$class->$bxor,a,b);
}

// numpyQ_MinusD_ndarrayD_int /////////////////////////////////////////////////////////////////////////////////

B_NoneType numpyQ_MinusD_ndarrayD_intD___init__(numpyQ_MinusD_ndarrayD_int wit, B_Integral W_Integral) {
    wit->W_Integral =  W_Integral;
    return B_None;
};

numpyQ_MinusD_ndarrayD_int numpyQ_MinusD_ndarrayD_intG_new(B_Integral W_Integral) {
    numpyQ_MinusD_ndarrayD_int res = malloc(sizeof (numpyQ_MinusD_ndarrayD_int));
    res->$class = &numpyQ_MinusD_ndarrayD_intG_methods;
    numpyQ_MinusD_ndarrayD_intD___init__(res, W_Integral);
    return res;
}

void numpyQ_MinusD_ndarrayD_intD___serialize__(numpyQ_MinusD_ndarrayD_int wit, $Serial$state state) {
    $step_serialize(wit->W_Integral, state);
}

numpyQ_MinusD_ndarrayD_int numpyQ_MinusD_ndarrayD_intD___deserialize__(numpyQ_MinusD_ndarrayD_int wit, $Serial$state state) {
    numpyQ_MinusD_ndarrayD_int res = $DNEW(numpyQ_MinusD_ndarrayD_int,state);
    res->W_Integral = (B_Integral)$step_deserialize(state);
    return res;
}

numpyQ_ndarray numpyQ_MinusD_ndarrayD_intD___sub__ (numpyQ_MinusD_ndarrayD_int wit, numpyQ_ndarray a, numpyQ_ndarray b) {
    return numpyQ_oper(numpyQ_PrimitiveD_intG_witness->$class->$sub,a,b);
}

// numpyQ_RealFloat$ndarray //////////////////////////////////////////////////////////////////////////////////////

numpyQ_RealD_ndarray numpyQ_RealFloat$ndarrayG_new(numpyQ_Primitive W_PrimitiveD_AD_numpy, B_RealFloat dummy) {
    numpyQ_RealD_ndarray res = malloc(sizeof (numpyQ_RealD_ndarray));
    res->$class = &numpyQ_RealD_ndarrayG_methods;
    numpyQ_RealD_ndarrayD___init__(res, W_PrimitiveD_AD_numpy);
    return res;
}

// numpyQ_RealD_ndarray /////////////////////////////////////////////////////////////////////////////////////////////

B_NoneType numpyQ_RealD_ndarrayD___init__(numpyQ_RealD_ndarray wit, numpyQ_Primitive W_PrimitiveD_AD_numpy) {
    wit->W_Minus = (B_Minus)$NEW(numpyQ_MinusD_ndarray,(B_Real)wit);
    wit->W_PrimitiveD_AD_RealD_ndarray =  W_PrimitiveD_AD_numpy;
    return B_None;
}; 

numpyQ_RealD_ndarray numpyQ_RealD_ndarrayG_new(numpyQ_Primitive W_PrimitiveD_AD_numpy) {
    numpyQ_RealD_ndarray res = malloc(sizeof (numpyQ_RealD_ndarray));
    res->$class = &numpyQ_RealD_ndarrayG_methods;
    numpyQ_RealD_ndarrayD___init__(res, W_PrimitiveD_AD_numpy);
    return res;
}


void numpyQ_RealD_ndarrayD___serialize__(numpyQ_RealD_ndarray wit, $Serial$state state) {
    $step_serialize(wit->W_Minus, state);
}

numpyQ_RealD_ndarray numpyQ_RealD_ndarrayD___deserialize__(numpyQ_RealD_ndarray wit, $Serial$state state) {
    numpyQ_RealD_ndarray res = $DNEW(numpyQ_RealD_ndarray,state);
    res->W_Minus = (B_Minus)$step_deserialize(state);
    return res;
}

numpyQ_ndarray numpyQ_RealD_ndarrayD___add__(numpyQ_RealD_ndarray wit, numpyQ_ndarray a, numpyQ_ndarray b){
    return numpyQ_oper(wit->W_PrimitiveD_AD_RealD_ndarray->$class->$add,a,b);
}

numpyQ_ndarray numpyQ_RealD_ndarrayD___fromatom__(numpyQ_RealD_ndarray wit,B_atom a) {
    return numpyQ_fromatom((numpyQ_Primitive)numpyQ_PrimitiveD_floatG_witness, a);
}

B_complex numpyQ_RealD_ndarrayD___complx__(numpyQ_RealD_ndarray wit, numpyQ_ndarray a) {
    $RAISE((B_BaseException)$NEW(B_NotImplementedError,to$str("complex not implemented for ndarray")));
    return NULL;
}

numpyQ_ndarray numpyQ_RealD_ndarrayD___mul__(numpyQ_RealD_ndarray wit, numpyQ_ndarray a, numpyQ_ndarray b) {
    return numpyQ_oper(wit->W_PrimitiveD_AD_RealD_ndarray->$class->$mul,a,b);
}

numpyQ_ndarray numpyQ_RealD_ndarrayD___pow__(numpyQ_RealD_ndarray wit, numpyQ_ndarray a, numpyQ_ndarray b) {
    return numpyQ_oper(wit->W_PrimitiveD_AD_RealD_ndarray->$class->B_pow,a,b);
}

numpyQ_ndarray numpyQ_RealD_ndarrayD___neg__(numpyQ_RealD_ndarray wit, numpyQ_ndarray a) {
    return numpyQ_func(wit->W_PrimitiveD_AD_RealD_ndarray->$class->$neg,a);
}

numpyQ_ndarray numpyQ_RealD_ndarrayD___pos__(numpyQ_RealD_ndarray wit, numpyQ_ndarray a) {
    return a;
}

$WORD numpyQ_RealD_ndarray$real(numpyQ_RealD_ndarray wit, numpyQ_ndarray a, B_Real wit2) {
    return a;
}
$WORD numpyQ_RealD_ndarray$imag(numpyQ_RealD_ndarray wit, numpyQ_ndarray a, B_Real wit2);
$WORD numpyQ_RealD_ndarrayD___abs__(numpyQ_RealD_ndarray wit, numpyQ_ndarray a, B_Real wit2) {
    return numpyQ_func(wit->W_PrimitiveD_AD_RealD_ndarray->$class->B_abs,a);
}
numpyQ_ndarray numpyQ_RealD_ndarray$conjugate(numpyQ_RealD_ndarray wit, numpyQ_ndarray a);
B_float numpyQ_RealD_ndarrayD___float__ (numpyQ_RealD_ndarray wit, numpyQ_ndarray a);
$WORD numpyQ_RealD_ndarrayD___trunc__ (numpyQ_RealD_ndarray wit, numpyQ_ndarray a, B_Integral wit2);
$WORD numpyQ_RealD_ndarrayD___floor__ (numpyQ_RealD_ndarray wit, numpyQ_ndarray a, B_Integral wit2);
$WORD numpyQ_RealD_ndarrayD___ceil__ (numpyQ_RealD_ndarray wit, numpyQ_ndarray a, B_Integral wit2);
numpyQ_ndarray numpyQ_RealD_ndarrayD___round__ (numpyQ_RealD_ndarray wit, numpyQ_ndarray a, numpyQ_ndarray b);

/*
  $WORD numpyQ_RealD_ndarray$numerator (numpyQ_RealD_ndarray wit, numpyQ_ndarray a, B_Integral wit2);
  $WORD numpyQ_RealD_ndarray$denominator (numpyQ_RealD_ndarray wit, numpyQ_ndarray a, B_Integral wit2);
  numpyQ_ndarray numpyQ_RealD_ndarrayD___int__ (numpyQ_RealD_ndarray wit, numpyQ_ndarray a);
  numpyQ_ndarray numpyQ_RealD_ndarrayD___index__ (numpyQ_RealD_ndarray wit, numpyQ_ndarray a);
  B_tuple numpyQ_RealD_ndarrayD___divmod__ (numpyQ_RealD_ndarray wit, numpyQ_ndarray a, numpyQ_ndarray b);
  numpyQ_ndarray numpyQ_RealD_ndarrayD___floordiv__ (numpyQ_RealD_ndarray wit, numpyQ_ndarray a, numpyQ_ndarray b) {
  return numpyQ_oper(wit->W_PrimitiveD_AD_RealD_ndarray->$class->$floodiv,a,b);
  }  
  numpyQ_ndarray numpyQ_RealD_ndarrayD___mod__ (numpyQ_RealD_ndarray wit, numpyQ_ndarray a, numpyQ_ndarray b);
  numpyQ_ndarray numpyQ_RealD_ndarrayD___lshift__ (numpyQ_RealD_ndarray wit, numpyQ_ndarray a, numpyQ_ndarray b);
  numpyQ_ndarray numpyQ_RealD_ndarrayD___rshift__ (numpyQ_RealD_ndarray wit, numpyQ_ndarray a, numpyQ_ndarray b);
  numpyQ_ndarray numpyQ_RealD_ndarrayD___invert__ (numpyQ_RealD_ndarray wit, numpyQ_ndarray a);
*/
 
// numpyQ_MinusD_ndarray /////////////////////////////////////////////////////////////////////////////////

B_NoneType numpyQ_MinusD_ndarrayD___init__(numpyQ_MinusD_ndarray wit, B_Real W_Real) {
    wit->W_Real =  W_Real;
    return B_None;
};

numpyQ_MinusD_ndarray numpyQ_MinusD_ndarrayG_new(B_Real W_Real) {
    numpyQ_MinusD_ndarray res = malloc(sizeof (numpyQ_MinusD_ndarray));
    res->$class = &numpyQ_MinusD_ndarrayG_methods;
    numpyQ_MinusD_ndarrayD___init__(res, W_Real);
    return res;
}

void numpyQ_MinusD_ndarrayD___serialize__(numpyQ_MinusD_ndarray wit, $Serial$state state) {
    $step_serialize(wit->W_Real, state);
}

numpyQ_MinusD_ndarray numpyQ_MinusD_ndarrayD___deserialize__(numpyQ_MinusD_ndarray wit, $Serial$state state) {
    numpyQ_MinusD_ndarray res = $DNEW(numpyQ_MinusD_ndarray,state);
    res->W_Real = (B_Real)$step_deserialize(state);
    return res;
}

numpyQ_ndarray numpyQ_MinusD_ndarrayD___sub__ (numpyQ_MinusD_ndarray wit, numpyQ_ndarray a, numpyQ_ndarray b) {
    return numpyQ_oper(((numpyQ_RealD_ndarray)wit->W_Real)-> W_PrimitiveD_AD_RealD_ndarray->$class->$sub,a,b);
}

// numpyQ_DivD_ndarrayD_int /////////////////////////////////////////////////////////////////////////////////

B_NoneType numpyQ_DivD_ndarrayD_intD___init__(numpyQ_DivD_ndarrayD_int wit) {
    return B_None;
};

numpyQ_DivD_ndarrayD_int numpyQ_DivD_ndarrayD_intG_new() {
    numpyQ_DivD_ndarrayD_int res = malloc(sizeof (numpyQ_DivD_ndarrayD_int));
    res->$class = &numpyQ_DivD_ndarrayD_intG_methods;
    return res;
}

void numpyQ_DivD_ndarrayD_intD___serialize__(numpyQ_DivD_ndarrayD_int wit, $Serial$state state) {
}

numpyQ_DivD_ndarrayD_int numpyQ_DivD_ndarrayD_intD___deserialize__(numpyQ_DivD_ndarrayD_int wit, $Serial$state state) {
    numpyQ_DivD_ndarrayD_int res = $DNEW(numpyQ_DivD_ndarrayD_int,state);
    return res;
}

numpyQ_ndarray numpyQ_DivD_ndarrayD_intD___truediv__ (numpyQ_DivD_ndarrayD_int wit, numpyQ_ndarray a, numpyQ_ndarray b) {
    numpyQ_ndarray res = numpyQ_oper(B_l_truediv,a,b);
    res->elem_type = DblType;
    return res;
}

// numpyQ_DivD_ndarrayD_float /////////////////////////////////////////////////////////////////////////////////

B_NoneType numpyQ_DivD_ndarrayD_floatD___init__(numpyQ_DivD_ndarrayD_float wit) {
    return B_None;
};

numpyQ_DivD_ndarrayD_float numpyQ_DivD_ndarrayD_floatG_new() {
    numpyQ_DivD_ndarrayD_float res = malloc(sizeof (numpyQ_DivD_ndarrayD_float));
    res->$class = &numpyQ_DivD_ndarrayD_floatG_methods;
    return res;
}

void numpyQ_DivD_ndarrayD_floatD___serialize__(numpyQ_DivD_ndarrayD_float wit, $Serial$state state) {
}

numpyQ_DivD_ndarrayD_float numpyQ_DivD_ndarrayD_floatD___deserialize__(numpyQ_DivD_ndarrayD_float wit, $Serial$state state) {
    numpyQ_DivD_ndarrayD_float res = $DNEW(numpyQ_DivD_ndarrayD_float,state);
    return res;
}

numpyQ_ndarray numpyQ_DivD_ndarrayD_floatD___truediv__ (numpyQ_DivD_ndarrayD_float wit, numpyQ_ndarray a, numpyQ_ndarray b) {
    return numpyQ_oper(B_d_truediv,a,b);
}

// Sliceable$ndarray ///////////////////////////////////////////////////////////////////////////////

B_NoneType numpyQ_SliceableD_ndarrayD___init__ (numpyQ_SliceableD_ndarray self, numpyQ_Primitive pwit) {
    self->pwit = pwit;
    return B_None;
}

void numpyQ_SliceableD_ndarrayD___serialize__(numpyQ_SliceableD_ndarray wit, $Serial$state state) {
}

numpyQ_SliceableD_ndarray numpyQ_SliceableD_ndarrayG_new(numpyQ_Primitive pwit) {
    numpyQ_SliceableD_ndarray res = malloc(sizeof(numpyQ_SliceableD_ndarray));
    res->$class = &numpyQ_SliceableD_ndarrayG_methods;
    numpyQ_SliceableD_ndarrayD___init__(res, pwit);
    return res;
}

numpyQ_SliceableD_ndarray numpyQ_SliceableD_ndarrayD___deserialize__(numpyQ_SliceableD_ndarray wit, $Serial$state state) {
    numpyQ_SliceableD_ndarray res = $DNEW(numpyQ_SliceableD_ndarray,state);
    return res;
}

numpyQ_ndarray numpyQ_SliceableD_ndarrayD___getitem__ (numpyQ_SliceableD_ndarray pwit, numpyQ_ndarray a, B_int i) {
    B_SequenceD_list wit = B_SequenceD_listG_witness;
    B_list lst = B_listG_new(NULL, NULL);
    wit->$class->append(wit, lst, numpyQ_ndindexG_new(i));
    return a->$class->__ndgetslice__(a, lst);
}

B_NoneType numpyQ_SliceableD_ndarrayD___setitem__ (numpyQ_SliceableD_ndarray wit, numpyQ_ndarray a, B_int i, $WORD val) {
    $RAISE((B_BaseException)$NEW(B_NotImplementedError,to$str("call to mutating method setitem on ndarray")));
    return B_None;
}

B_NoneType numpyQ_SliceableD_ndarrayD___delitem__ (numpyQ_SliceableD_ndarray wit, numpyQ_ndarray a, B_int i) {
    $RAISE((B_BaseException)$NEW(B_NotImplementedError,to$str("call to mutating method delitem on ndarray")));
    return B_None;
}

numpyQ_ndarray numpyQ_SliceableD_ndarrayD___getslice__ (numpyQ_SliceableD_ndarray pwit, numpyQ_ndarray a, B_slice slc) {
    B_SequenceD_list wit = B_SequenceD_listG_witness;
    B_list lst = B_listG_new(NULL, NULL);
    wit->$class->append(wit,lst, numpyQ_ndsliceG_new(slc));
    return a->$class->__ndgetslice__(a, lst);
}

B_NoneType numpyQ_SliceableD_ndarrayD___setslice__ (numpyQ_SliceableD_ndarray wit, numpyQ_ndarray a, B_Iterable wit2, B_slice slc, $WORD iter) {
    $RAISE((B_BaseException)$NEW(B_NotImplementedError,to$str("call to mutating method setslice on ndarray")));
    return B_None;
}

B_NoneType numpyQ_SliceableD_ndarrayD___delslice__ (numpyQ_SliceableD_ndarray wit, numpyQ_ndarray a, B_slice slc) {
    $RAISE((B_BaseException)$NEW(B_NotImplementedError,to$str("call to mutating method delslice on ndarray")));
    return B_None;
}

// numpyQ_CollectionD_ndarray ////////////////////////////////////////////////////////


B_NoneType numpyQ_CollectionD_ndarrayD___init__(numpyQ_CollectionD_ndarray self, numpyQ_Primitive pwit) {
    self->pwit = pwit;
    return B_None;
}
  
numpyQ_CollectionD_ndarray numpyQ_CollectionD_ndarrayG_new(numpyQ_Primitive pwit) {
    numpyQ_CollectionD_ndarray res = malloc(sizeof (numpyQ_CollectionD_ndarray));
    res->$class = &numpyQ_CollectionD_ndarrayG_methods;
    numpyQ_CollectionD_ndarrayD___init__(res, pwit);
    return res;
}

void numpyQ_CollectionD_ndarrayD___serialize__(numpyQ_CollectionD_ndarray wit, $Serial$state state) {
}

numpyQ_CollectionD_ndarray numpyQ_CollectionD_ndarrayD___deserialize__(numpyQ_CollectionD_ndarray wit, $Serial$state state) {
    numpyQ_CollectionD_ndarray res = $DNEW(numpyQ_CollectionD_ndarray,state);
    return res;
}


B_Iterator numpyQ_CollectionD_ndarrayD___iter__(numpyQ_CollectionD_ndarray self, numpyQ_ndarray a) {
    return (B_Iterator)numpyQ_IteratorD_ndarrayG_new(self->pwit,a);
}

numpyQ_ndarray numpyQ_CollectionD_ndarrayD___fromiter__(numpyQ_CollectionD_ndarray wit, B_Iterable iter) {
    return NULL;
}
B_int numpyQ_CollectionD_ndarrayD___len__(numpyQ_CollectionD_ndarray pwit, numpyQ_ndarray a) {
    B_SequenceD_list wit = B_SequenceD_listG_witness;
    return wit->$class->__getitem__(wit,a->shape,to$int(-1));
}


struct numpyQ_IntegralD_ndarrayD_int numpyQ_IntegralD_ndarrayD_int$instance;
struct numpyQ_LogicalD_ndarrayD_int numpyQ_LogicalD_ndarrayD_int$instance;
struct numpyQ_MinusD_ndarrayD_int numpyQ_MinusD_ndarrayD_int$instance;
struct numpyQ_RealD_ndarray numpyQ_RealD_ndarray$instance;
struct numpyQ_MinusD_ndarray numpyQ_MinusD_ndarray$instance;
struct numpyQ_SliceableD_ndarray numpyQ_SliceableD_ndarray$instance;

struct numpyQ_IntegralD_ndarrayD_intG_class numpyQ_IntegralD_ndarrayD_intG_methods = {
    0,
    "numpyQ_IntegralD_ndarrayD_int",
    UNASSIGNED,
    ($SuperG_class)&B_IntegralG_methods,
    numpyQ_IntegralD_ndarrayD_intD___init__,
    numpyQ_IntegralD_ndarrayD_intD___serialize__,
    numpyQ_IntegralD_ndarrayD_intD___deserialize__,
    (B_bool (*)(numpyQ_IntegralD_ndarrayD_int))$default__bool__,
    (B_str (*)(numpyQ_IntegralD_ndarrayD_int))$default__str__,
    (B_str (*)(numpyQ_IntegralD_ndarrayD_int))$default__str__,
    numpyQ_IntegralD_ndarrayD_intD___add__,
    numpyQ_IntegralD_ndarrayD_intD___add__,
    numpyQ_IntegralD_ndarrayD_intD___mul__,
    numpyQ_IntegralD_ndarrayD_intD___mul__,
    numpyQ_IntegralD_ndarrayD_intD___fromatom__,
    NULL,
    numpyQ_IntegralD_ndarrayD_intD___pow__,
    numpyQ_IntegralD_ndarrayD_intD___pow__,
    numpyQ_IntegralD_ndarrayD_intD___neg__,
    numpyQ_IntegralD_ndarrayD_intD___pos__,
    NULL,
    NULL,
    numpyQ_IntegralD_ndarrayD_intD___abs__,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    numpyQ_IntegralD_ndarrayD_intD___floordiv__ ,
    NULL,
    NULL,
    NULL,
    numpyQ_IntegralD_ndarrayD_intD___floordiv__ ,
    NULL,
    NULL,
    NULL,
    NULL
};

struct numpyQ_IntegralD_ndarrayD_int numpyQ_IntegralD_ndarrayD_int$instance = {&numpyQ_IntegralD_ndarrayD_intG_methods,
                                                                           (B_Logical)&numpyQ_LogicalD_ndarrayD_int$instance,  (B_Minus)&numpyQ_MinusD_ndarrayD_int$instance};
numpyQ_IntegralD_ndarrayD_int numpyQ_IntegralD_ndarrayD_intG_witness = &numpyQ_IntegralD_ndarrayD_int$instance;

struct numpyQ_LogicalD_ndarrayD_intG_class numpyQ_LogicalD_ndarrayD_intG_methods = {
    0,
    "numpyQ_LogicalD_ndarrayD_int",
    UNASSIGNED,
    ($SuperG_class)&B_LogicalG_methods,
    numpyQ_LogicalD_ndarrayD_intD___init__,
    numpyQ_LogicalD_ndarrayD_intD___serialize__,
    numpyQ_LogicalD_ndarrayD_intD___deserialize__,
    (B_bool (*)(numpyQ_LogicalD_ndarrayD_int))$default__bool__,
    (B_str (*)(numpyQ_LogicalD_ndarrayD_int))$default__str__,
    (B_str (*)(numpyQ_LogicalD_ndarrayD_int))$default__str__,
    numpyQ_LogicalD_ndarrayD_intD___and__,
    numpyQ_LogicalD_ndarrayD_intD___or__,
    numpyQ_LogicalD_ndarrayD_intD___xor__,
    numpyQ_LogicalD_ndarrayD_intD___and__,
    numpyQ_LogicalD_ndarrayD_intD___or__,
    numpyQ_LogicalD_ndarrayD_intD___xor__,
};

struct numpyQ_LogicalD_ndarrayD_int numpyQ_LogicalD_ndarrayD_int$instance = {&numpyQ_LogicalD_ndarrayD_intG_methods, (B_Integral)&numpyQ_IntegralD_ndarrayD_int$instance};
numpyQ_LogicalD_ndarrayD_int numpyQ_LogicalD_ndarrayD_intG_witness = &numpyQ_LogicalD_ndarrayD_int$instance;

struct numpyQ_MinusD_ndarrayD_intG_class numpyQ_MinusD_ndarrayD_intG_methods = {
    0,
    "numpyQ_MinusD_ndarrayD_int",
    UNASSIGNED,
    ($SuperG_class)&B_MinusG_methods,
    numpyQ_MinusD_ndarrayD_intD___init__,
    numpyQ_MinusD_ndarrayD_intD___serialize__,
    numpyQ_MinusD_ndarrayD_intD___deserialize__,
    (B_bool (*)(numpyQ_MinusD_ndarrayD_int))$default__bool__,
    (B_str (*)(numpyQ_MinusD_ndarrayD_int))$default__str__,
    (B_str (*)(numpyQ_MinusD_ndarrayD_int))$default__str__,
    numpyQ_MinusD_ndarrayD_intD___sub__,
    numpyQ_MinusD_ndarrayD_intD___sub__,
};
struct numpyQ_MinusD_ndarrayD_int numpyQ_MinusD_ndarrayD_int$instance = {&numpyQ_MinusD_ndarrayD_intG_methods,  (B_Integral)&numpyQ_IntegralD_ndarrayD_int$instance};
numpyQ_MinusD_ndarrayD_int numpyQ_MinusD_ndarrayD_intG_witness = &numpyQ_MinusD_ndarrayD_int$instance;

struct numpyQ_RealD_ndarrayG_class numpyQ_RealD_ndarrayG_methods = {
    0,
    "numpyQ_RealD_ndarray",
    UNASSIGNED,
    ($SuperG_class)&B_IntegralG_methods,
    numpyQ_RealD_ndarrayD___init__,
    numpyQ_RealD_ndarrayD___serialize__,
    numpyQ_RealD_ndarrayD___deserialize__,
    (B_bool (*)(numpyQ_RealD_ndarray))$default__bool__,
    (B_str (*)(numpyQ_RealD_ndarray))$default__str__,
    (B_str (*)(numpyQ_RealD_ndarray))$default__str__,
    numpyQ_RealD_ndarrayD___add__,
    numpyQ_RealD_ndarrayD___add__,
    numpyQ_RealD_ndarrayD___mul__,
    numpyQ_RealD_ndarrayD___mul__,
    numpyQ_RealD_ndarrayD___fromatom__,
    NULL,
    numpyQ_RealD_ndarrayD___pow__,
    numpyQ_RealD_ndarrayD___pow__,
    numpyQ_RealD_ndarrayD___neg__,
    numpyQ_RealD_ndarrayD___pos__,
    NULL,
    NULL,
    numpyQ_RealD_ndarrayD___abs__,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL
};
 
struct numpyQ_MinusD_ndarrayG_class numpyQ_MinusD_ndarrayG_methods = {
    0,
    "numpyQ_MinusD_ndarray",
    UNASSIGNED,
    ($SuperG_class)&B_MinusG_methods,
    numpyQ_MinusD_ndarrayD___init__,
    numpyQ_MinusD_ndarrayD___serialize__,
    numpyQ_MinusD_ndarrayD___deserialize__,
    (B_bool (*)(numpyQ_MinusD_ndarray))$default__bool__,
    (B_str (*)(numpyQ_MinusD_ndarray))$default__str__,
    (B_str (*)(numpyQ_MinusD_ndarray))$default__str__,
    numpyQ_MinusD_ndarrayD___sub__,
    numpyQ_MinusD_ndarrayD___sub__,
};

struct numpyQ_MinusD_ndarray numpyQ_MinusD_ndarray$instance = {&numpyQ_MinusD_ndarrayG_methods,  (B_Real)&numpyQ_RealD_ndarray$instance};
numpyQ_MinusD_ndarray numpyQ_MinusD_ndarrayG_witness = &numpyQ_MinusD_ndarray$instance;

struct numpyQ_DivD_ndarrayD_intG_class numpyQ_DivD_ndarrayD_intG_methods = {
    0,
    "numpyQ_DivD_ndarrayD_int",
    UNASSIGNED,
    ($SuperG_class)&B_MinusG_methods,
    numpyQ_DivD_ndarrayD_intD___init__,
    numpyQ_DivD_ndarrayD_intD___serialize__,
    numpyQ_DivD_ndarrayD_intD___deserialize__,
    (B_bool (*)(numpyQ_DivD_ndarrayD_int))$default__bool__,
    (B_str (*)(numpyQ_DivD_ndarrayD_int))$default__str__,
    (B_str (*)(numpyQ_DivD_ndarrayD_int))$default__str__,
    numpyQ_DivD_ndarrayD_intD___truediv__,
    numpyQ_DivD_ndarrayD_intD___truediv__,
};

struct numpyQ_DivD_ndarrayD_int numpyQ_DivD_ndarrayD_int$instance = {&numpyQ_DivD_ndarrayD_intG_methods};
numpyQ_DivD_ndarrayD_int numpyQ_DivD_ndarrayD_intG_witness = &numpyQ_DivD_ndarrayD_int$instance;

struct numpyQ_DivD_ndarrayD_floatG_class numpyQ_DivD_ndarrayD_floatG_methods = {
    0,
    "numpyQ_DivD_ndarrayD_float",
    UNASSIGNED,
    ($SuperG_class)&B_MinusG_methods,
    numpyQ_DivD_ndarrayD_floatD___init__,
    numpyQ_DivD_ndarrayD_floatD___serialize__,
    numpyQ_DivD_ndarrayD_floatD___deserialize__,
    (B_bool (*)(numpyQ_DivD_ndarrayD_float))$default__bool__,
    (B_str (*)(numpyQ_DivD_ndarrayD_float))$default__str__,
    (B_str (*)(numpyQ_DivD_ndarrayD_float))$default__str__,
    numpyQ_DivD_ndarrayD_floatD___truediv__,
    numpyQ_DivD_ndarrayD_floatD___truediv__,
};

struct numpyQ_DivD_ndarrayD_float numpyQ_DivD_ndarrayD_float$instance = {&numpyQ_DivD_ndarrayD_floatG_methods};
numpyQ_DivD_ndarrayD_float numpyQ_DivD_ndarrayD_floatG_witness = &numpyQ_DivD_ndarrayD_float$instance;

struct numpyQ_SliceableD_ndarrayG_class numpyQ_SliceableD_ndarrayG_methods = {
    0,
    "numpyQ_SliceableD_ndarray",
    UNASSIGNED,
    ($SuperG_class)&B_SliceableG_methods,
    numpyQ_SliceableD_ndarrayD___init__,
    numpyQ_SliceableD_ndarrayD___serialize__,
    numpyQ_SliceableD_ndarrayD___deserialize__,
    (B_bool (*)(numpyQ_SliceableD_ndarray))$default__bool__,
    (B_str (*)(numpyQ_SliceableD_ndarray))$default__str__,
    (B_str (*)(numpyQ_SliceableD_ndarray))$default__str__,
    numpyQ_SliceableD_ndarrayD___getitem__,
    numpyQ_SliceableD_ndarrayD___setitem__,
    numpyQ_SliceableD_ndarrayD___delitem__,
    numpyQ_SliceableD_ndarrayD___getslice__,
    numpyQ_SliceableD_ndarrayD___setslice__,
    numpyQ_SliceableD_ndarrayD___delslice__,
};
struct numpyQ_SliceableD_ndarray numpyQ_SliceableD_instance = {&numpyQ_SliceableD_ndarrayG_methods};
numpyQ_SliceableD_ndarray numpyQ_SliceableD_ndarrayG_witness = &numpyQ_SliceableD_instance;


struct numpyQ_CollectionD_ndarrayG_class numpyQ_CollectionD_ndarrayG_methods = {
    0,
    "numpyQ_CollectionD_ndarray",
    UNASSIGNED,
    ($SuperG_class)&B_CollectionG_methods,
    numpyQ_CollectionD_ndarrayD___init__,
    numpyQ_CollectionD_ndarrayD___serialize__,
    numpyQ_CollectionD_ndarrayD___deserialize__,
    (B_bool (*)(numpyQ_CollectionD_ndarray))$default__bool__,
    (B_str (*)(numpyQ_CollectionD_ndarray))$default__str__,
    (B_str (*)(numpyQ_CollectionD_ndarray))$default__str__,
    numpyQ_CollectionD_ndarrayD___iter__,
    numpyQ_CollectionD_ndarrayD___fromiter__,
    numpyQ_CollectionD_ndarrayD___len__
};

struct numpyQ_CollectionD_ndarray numpyQ_CollectionD_instance = {&numpyQ_CollectionD_ndarrayG_methods};
numpyQ_CollectionD_ndarray numpyQ_CollectionD_ndarrayG_witness = &numpyQ_CollectionD_instance;


// numpyQ_RealFunsD_mathD_ndarray ///////////////////////////////////////////////////////////

B_NoneType numpyQ_RealFunsD_mathD_ndarrayD___init__ (numpyQ_RealFunsD_mathD_ndarray W_self, numpyQ_Primitive W_Primitive, mathQ_RealFuns W_RealFuns) {
    W_self->W_PrimitiveD_AD_RealFuns$math$ndarray = W_Primitive;
    W_self-> W_RealFuns$mathD_AD_RealFuns$math$ndarray = W_RealFuns;
    return B_None;
}

B_NoneType numpyQ_RealFunsD_mathD_ndarrayD___serialize__(numpyQ_RealFunsD_mathD_ndarray wit, $Serial$state state) {
    $step_serialize(wit->W_PrimitiveD_AD_RealFuns$math$ndarray, state);
    $step_serialize(wit->W_RealFuns$mathD_AD_RealFuns$math$ndarray, state);
    return B_None;
}

numpyQ_RealFunsD_mathD_ndarray numpyQ_RealFunsD_mathD_ndarrayD___deserialize__(numpyQ_RealFunsD_mathD_ndarray wit, $Serial$state state) {
    numpyQ_RealFunsD_mathD_ndarray res = $DNEW(numpyQ_RealFunsD_mathD_ndarray,state);
    res->W_PrimitiveD_AD_RealFuns$math$ndarray = (numpyQ_Primitive)$step_deserialize(state);
    res->W_RealFuns$mathD_AD_RealFuns$math$ndarray = (mathQ_RealFuns)$step_deserialize(state);
    return res;
}

#define B8Fun(f,fB)  static union $Bytes8 fB(union $Bytes8 a) {union $Bytes8 res; res.d = f(a.d); return res;}

B8Fun(sqrt,sqrtB)
B8Fun(exp,expB)
B8Fun(log,logB)
B8Fun(sin,sinB)
B8Fun(cos,cosB)
B8Fun(tan,tanB)
B8Fun(asin,asinB)
B8Fun(acos,acosB)
B8Fun(atan,atanB)
B8Fun(sinh,sinhB)
B8Fun(cosh,coshB)
B8Fun(tanh,tanhB)
B8Fun(asinh,asinhB)
B8Fun(acosh,acoshB)
B8Fun(atanh,atanhB)
  
numpyQ_ndarray numpyQ_RealFunsD_mathD_ndarray$sqrt(numpyQ_RealFunsD_mathD_ndarray wit, numpyQ_ndarray a) {
    return numpyQ_func(sqrtB,a);
}
numpyQ_ndarray numpyQ_RealFunsD_mathD_ndarray$exp(numpyQ_RealFunsD_mathD_ndarray wit, numpyQ_ndarray a) {
    return numpyQ_func(expB,a);
}
numpyQ_ndarray numpyQ_RealFunsD_mathD_ndarray$log(numpyQ_RealFunsD_mathD_ndarray wit, numpyQ_ndarray a) {
    return numpyQ_func(logB,a);
}
numpyQ_ndarray numpyQ_RealFunsD_mathD_ndarray$sin(numpyQ_RealFunsD_mathD_ndarray wit, numpyQ_ndarray a) {
    return numpyQ_func(sinB,a);
}
numpyQ_ndarray numpyQ_RealFunsD_mathD_ndarray$cos(numpyQ_RealFunsD_mathD_ndarray wit, numpyQ_ndarray a) {
    return numpyQ_func(cosB,a);
}
numpyQ_ndarray numpyQ_RealFunsD_mathD_ndarray$tan(numpyQ_RealFunsD_mathD_ndarray wit, numpyQ_ndarray a) {
    return numpyQ_func(tanB,a);
}
numpyQ_ndarray numpyQ_RealFunsD_mathD_ndarray$asin(numpyQ_RealFunsD_mathD_ndarray wit, numpyQ_ndarray a) {
    return numpyQ_func(asinB,a);
}
numpyQ_ndarray numpyQ_RealFunsD_mathD_ndarray$acos(numpyQ_RealFunsD_mathD_ndarray wit, numpyQ_ndarray a) {
    return numpyQ_func(acosB,a);
}
numpyQ_ndarray numpyQ_RealFunsD_mathD_ndarray$atan(numpyQ_RealFunsD_mathD_ndarray wit, numpyQ_ndarray a) {
    return numpyQ_func(atanB,a);
}
numpyQ_ndarray numpyQ_RealFunsD_mathD_ndarray$sinh(numpyQ_RealFunsD_mathD_ndarray wit, numpyQ_ndarray a) {
    return numpyQ_func(sinhB,a);
}
numpyQ_ndarray numpyQ_RealFunsD_mathD_ndarray$cosh(numpyQ_RealFunsD_mathD_ndarray wit, numpyQ_ndarray a) {
    return numpyQ_func(coshB,a);
}
numpyQ_ndarray numpyQ_RealFunsD_mathD_ndarray$tanh(numpyQ_RealFunsD_mathD_ndarray wit, numpyQ_ndarray a) {
    return numpyQ_func(tanhB,a);
}
numpyQ_ndarray numpyQ_RealFunsD_mathD_ndarray$asinh(numpyQ_RealFunsD_mathD_ndarray wit, numpyQ_ndarray a) {
    return numpyQ_func(asinhB,a);
}
numpyQ_ndarray numpyQ_RealFunsD_mathD_ndarray$acosh(numpyQ_RealFunsD_mathD_ndarray wit, numpyQ_ndarray a) {
    return numpyQ_func(acoshB,a);
}
numpyQ_ndarray numpyQ_RealFunsD_mathD_ndarray$atanh(numpyQ_RealFunsD_mathD_ndarray wit, numpyQ_ndarray a) {
    return numpyQ_func(atanhB,a);
}

                      
numpyQ_RealFunsD_mathD_ndarray numpyQ_RealFunsD_mathD_ndarrayG_new(numpyQ_Primitive W_Primitive, mathQ_RealFuns W_RealFuns) {
    numpyQ_RealFunsD_mathD_ndarray $tmp = malloc(sizeof(numpyQ_RealFunsD_mathD_ndarray));
    $tmp->$class = &numpyQ_RealFunsD_mathD_ndarrayG_methods;
    numpyQ_RealFunsD_mathD_ndarrayG_methods.__init__($tmp, W_Primitive, W_RealFuns);
    return $tmp;
}
struct numpyQ_RealFunsD_mathD_ndarrayG_class numpyQ_RealFunsD_mathD_ndarrayG_methods = {
    0,
    "numpyQ_RealFunsD_mathD_ndarray",
    UNASSIGNED,
    ($SuperG_class)&mathQ_RealFunsG_methods,
    numpyQ_RealFunsD_mathD_ndarrayD___init__,
    numpyQ_RealFunsD_mathD_ndarrayD___serialize__,
    numpyQ_RealFunsD_mathD_ndarrayD___deserialize__,
    (B_bool (*)(numpyQ_RealFunsD_mathD_ndarray))$default__bool__,
    (B_str (*)(numpyQ_RealFunsD_mathD_ndarray))$default__str__,
    (B_str (*)(numpyQ_RealFunsD_mathD_ndarray))$default__str__,
    numpyQ_RealFunsD_mathD_ndarray$sqrt,        
    numpyQ_RealFunsD_mathD_ndarray$exp,        
    numpyQ_RealFunsD_mathD_ndarray$log,        
    numpyQ_RealFunsD_mathD_ndarray$sin,        
    numpyQ_RealFunsD_mathD_ndarray$cos,        
    numpyQ_RealFunsD_mathD_ndarray$tan,        
    numpyQ_RealFunsD_mathD_ndarray$asin,        
    numpyQ_RealFunsD_mathD_ndarray$acos,        
    numpyQ_RealFunsD_mathD_ndarray$atan,        
    numpyQ_RealFunsD_mathD_ndarray$sinh,        
    numpyQ_RealFunsD_mathD_ndarray$cosh,        
    numpyQ_RealFunsD_mathD_ndarray$tanh,        
    numpyQ_RealFunsD_mathD_ndarray$asinh,        
    numpyQ_RealFunsD_mathD_ndarray$acosh,        
    numpyQ_RealFunsD_mathD_ndarray$atanh     
};
 
