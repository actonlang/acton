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

// numpy$B_IntegralD_ndarrayB_int /////////////////////////////////////////////////////////////////////////////////////////////

void numpy$B_IntegralD_ndarrayB_intD___init__(numpy$B_IntegralD_ndarrayB_int wit) {
    wit->W_Logical = (B_Logical)$NEW(numpy$B_LogicalD_ndarrayB_int,(B_Integral)wit);
    wit->W_Minus = (B_Minus)$NEW(numpy$B_MinusD_ndarrayB_int,(B_Integral)wit);
}; 

numpy$B_IntegralD_ndarrayB_int numpy$B_IntegralD_ndarrayB_intG_new() {
    numpy$B_IntegralD_ndarrayB_int res = malloc(sizeof (struct numpy$B_IntegralD_ndarrayB_int));
    res->$class = &numpy$B_IntegralD_ndarrayB_intG_methods;
    numpy$B_IntegralD_ndarrayB_intD___init__(res);
    return res;
}


void numpy$B_IntegralD_ndarrayB_intD___serialize__(numpy$B_IntegralD_ndarrayB_int wit, $NoneType state) {
    $step_serialize(wit->W_Logical, state);
    $step_serialize(wit->W_Minus, state);
}

numpy$B_IntegralD_ndarrayB_int numpy$B_IntegralD_ndarrayB_intD___deserialize__(numpy$B_IntegralD_ndarrayB_int wit, $NoneType state) {
    numpy$B_IntegralD_ndarrayB_int res = $DNEW(numpy$B_IntegralD_ndarrayB_int,state);
    res->W_Logical = (B_Logical)$step_deserialize(state);
    res->W_Minus = (B_Minus)$step_deserialize(state);
    return res;
}

numpy$$ndarray numpy$B_IntegralD_ndarrayB_intD___add__(numpy$B_IntegralD_ndarrayB_int wit, numpy$$ndarray a, numpy$$ndarray b){
    return numpy$$oper(numpy$$PrimitiveB_intG_witness->$class->$add,a,b);
}

numpy$$ndarray numpy$B_IntegralD_ndarrayB_intD___fromatom__(numpy$B_IntegralD_ndarrayB_int wit,B_atom a) {
    return numpy$$fromatom((numpy$$Primitive)numpy$$PrimitiveB_intG_witness,a);
}

B_complex numpy$B_IntegralD_ndarrayB_intD___complx__(numpy$B_IntegralD_ndarrayB_int wit, numpy$$ndarray a) {
    $RAISE((B_BaseException)$NEW(B_NotImplementedError,to$str("complex not implemented for ndarray")));
    return NULL;
}

numpy$$ndarray numpy$B_IntegralD_ndarrayB_intD___mul__(numpy$B_IntegralD_ndarrayB_int wit, numpy$$ndarray a, numpy$$ndarray b) {
    return numpy$$oper(numpy$$PrimitiveB_intG_witness->$class->$mul,a,b);
}

numpy$$ndarray numpy$B_IntegralD_ndarrayB_intD___pow__(numpy$B_IntegralD_ndarrayB_int wit, numpy$$ndarray a, numpy$$ndarray b) {
    return numpy$$oper(numpy$$PrimitiveB_intG_witness->$class->$pow,a,b);
}

numpy$$ndarray numpy$B_IntegralD_ndarrayB_intD___neg__(numpy$B_IntegralD_ndarrayB_int wit, numpy$$ndarray a) {
    return numpy$$func(numpy$$PrimitiveB_intG_witness->$class->$neg,a);
}

numpy$$ndarray numpy$B_IntegralD_ndarrayB_intD___pos__(numpy$B_IntegralD_ndarrayB_int wit, numpy$$ndarray a) {
    return a;
}

$WORD numpy$B_IntegralD_ndarrayB_int$real(numpy$B_IntegralD_ndarrayB_int wit, numpy$$ndarray a, B_Real wit2) {
    return a;
}
$WORD numpy$B_IntegralD_ndarrayB_int$imag(numpy$B_IntegralD_ndarrayB_int wit, numpy$$ndarray a, B_Real wit2);
$WORD numpy$B_IntegralD_ndarrayB_intD___abs__(numpy$B_IntegralD_ndarrayB_int wit, numpy$$ndarray a, B_Real wit2) {
    return numpy$$func(numpy$$PrimitiveB_intG_witness->$class->$abs,a);
}
numpy$$ndarray numpy$B_IntegralD_ndarrayB_int$conjugate(numpy$B_IntegralD_ndarrayB_int wit, numpy$$ndarray a);
B_float numpy$B_IntegralD_ndarrayB_intD___float__ (numpy$B_IntegralD_ndarrayB_int wit, numpy$$ndarray a);
$WORD numpy$B_IntegralD_ndarrayB_intD___trunc__ (numpy$B_IntegralD_ndarrayB_int wit, numpy$$ndarray a, B_Integral wit2);
$WORD numpy$B_IntegralD_ndarrayB_intD___floor__ (numpy$B_IntegralD_ndarrayB_int wit, numpy$$ndarray a, B_Integral wit2);
$WORD numpy$B_IntegralD_ndarrayB_intD___ceil__ (numpy$B_IntegralD_ndarrayB_int wit, numpy$$ndarray a, B_Integral wit2);
numpy$$ndarray numpy$B_IntegralD_ndarrayB_intD___round__ (numpy$B_IntegralD_ndarrayB_int wit, numpy$$ndarray a, numpy$$ndarray b);
$WORD numpy$B_IntegralD_ndarrayB_int$numerator (numpy$B_IntegralD_ndarrayB_int wit, numpy$$ndarray a, B_Integral wit2);
$WORD numpy$B_IntegralD_ndarrayB_int$denominator (numpy$B_IntegralD_ndarrayB_int wit, numpy$$ndarray a, B_Integral wit2);
numpy$$ndarray numpy$B_IntegralD_ndarrayB_intD___int__ (numpy$B_IntegralD_ndarrayB_int wit, numpy$$ndarray a);
numpy$$ndarray numpy$B_IntegralD_ndarrayB_intD___index__ (numpy$B_IntegralD_ndarrayB_int wit, numpy$$ndarray a);
B_tuple numpy$B_IntegralD_ndarrayB_intD___divmod__ (numpy$B_IntegralD_ndarrayB_int wit, numpy$$ndarray a, numpy$$ndarray b);
numpy$$ndarray numpy$B_IntegralD_ndarrayB_intD___floordiv__ (numpy$B_IntegralD_ndarrayB_int wit, numpy$$ndarray a, numpy$$ndarray b) {
    return numpy$$oper(l$floordiv,a,b);
}  
numpy$$ndarray numpy$B_IntegralD_ndarrayB_intD___mod__ (numpy$B_IntegralD_ndarrayB_int wit, numpy$$ndarray a, numpy$$ndarray b);
numpy$$ndarray numpy$B_IntegralD_ndarrayB_intD___lshift__ (numpy$B_IntegralD_ndarrayB_int wit, numpy$$ndarray a, numpy$$ndarray b);
numpy$$ndarray numpy$B_IntegralD_ndarrayB_intD___rshift__ (numpy$B_IntegralD_ndarrayB_int wit, numpy$$ndarray a, numpy$$ndarray b);
numpy$$ndarray numpy$B_IntegralD_ndarrayB_intD___invert__ (numpy$B_IntegralD_ndarrayB_int wit, numpy$$ndarray a);

// numpy$B_LogicalD_ndarrayB_int //////////////////////////////////////////////////////////////////////////////

void numpy$B_LogicalD_ndarrayB_intD___init__(numpy$B_LogicalD_ndarrayB_int wit, B_Integral W_Integral) {
    wit->W_Integral =  W_Integral;
};

numpy$B_LogicalD_ndarrayB_int numpy$B_LogicalD_ndarrayB_intG_new(B_Integral W_Integral) {
    numpy$B_LogicalD_ndarrayB_int res = malloc(sizeof (struct numpy$B_LogicalD_ndarrayB_int));
    res->$class = &numpy$B_LogicalD_ndarrayB_intG_methods;
    numpy$B_LogicalD_ndarrayB_intD___init__(res, W_Integral);
    return res;
}
void numpy$B_LogicalD_ndarrayB_intD___serialize__(numpy$B_LogicalD_ndarrayB_int wit, $NoneType state) {
    $step_serialize(wit->W_Integral, state);
}

numpy$B_LogicalD_ndarrayB_int numpy$B_LogicalD_ndarrayB_intD___deserialize__(numpy$B_LogicalD_ndarrayB_int wit, $NoneType state) {
    numpy$B_LogicalD_ndarrayB_int res = $DNEW(numpy$B_LogicalD_ndarrayB_int,state);
    res->W_Integral = (B_Integral)$step_deserialize(state);
    return res;
}

numpy$$ndarray numpy$B_LogicalD_ndarrayB_intD___and__ (numpy$B_LogicalD_ndarrayB_int wit, numpy$$ndarray a, numpy$$ndarray b) {
    return numpy$$oper(numpy$$PrimitiveB_intG_witness->$class->$band,a,b);
}
numpy$$ndarray numpy$B_LogicalD_ndarrayB_intD___or__ (numpy$B_LogicalD_ndarrayB_int wit, numpy$$ndarray a, numpy$$ndarray b) {
    return numpy$$oper(numpy$$PrimitiveB_intG_witness->$class->$bor,a,b);
}
numpy$$ndarray numpy$B_LogicalD_ndarrayB_intD___xor__ (numpy$B_LogicalD_ndarrayB_int wit, numpy$$ndarray a, numpy$$ndarray b) {
    return numpy$$oper(numpy$$PrimitiveB_intG_witness->$class->$bxor,a,b);
}

// numpy$B_MinusD_ndarrayB_int /////////////////////////////////////////////////////////////////////////////////

void numpy$B_MinusD_ndarrayB_intD___init__(numpy$B_MinusD_ndarrayB_int wit, B_Integral W_Integral) {
    wit->W_Integral =  W_Integral;
};

numpy$B_MinusD_ndarrayB_int numpy$B_MinusD_ndarrayB_intG_new(B_Integral W_Integral) {
    numpy$B_MinusD_ndarrayB_int res = malloc(sizeof (struct numpy$B_MinusD_ndarrayB_int));
    res->$class = &numpy$B_MinusD_ndarrayB_intG_methods;
    numpy$B_MinusD_ndarrayB_intD___init__(res, W_Integral);
    return res;
}

void numpy$B_MinusD_ndarrayB_intD___serialize__(numpy$B_MinusD_ndarrayB_int wit, $NoneType state) {
    $step_serialize(wit->W_Integral, state);
}

numpy$B_MinusD_ndarrayB_int numpy$B_MinusD_ndarrayB_intD___deserialize__(numpy$B_MinusD_ndarrayB_int wit, $NoneType state) {
    numpy$B_MinusD_ndarrayB_int res = $DNEW(numpy$B_MinusD_ndarrayB_int,state);
    res->W_Integral = (B_Integral)$step_deserialize(state);
    return res;
}

numpy$$ndarray numpy$B_MinusD_ndarrayB_intD___sub__ (numpy$B_MinusD_ndarrayB_int wit, numpy$$ndarray a, numpy$$ndarray b) {
    return numpy$$oper(numpy$$PrimitiveB_intG_witness->$class->$sub,a,b);
}

// numpy$B_RealFloat$ndarray //////////////////////////////////////////////////////////////////////////////////////

numpy$B_RealD_ndarray numpy$B_RealFloat$ndarrayG_new(numpy$$Primitive W_PrimitiveD_AD_numpy, B_RealFloat dummy) {
    numpy$B_RealD_ndarray res = malloc(sizeof (struct numpy$B_RealD_ndarray));
    res->$class = &numpy$B_RealD_ndarrayG_methods;
    numpy$B_RealD_ndarrayD___init__(res, W_PrimitiveD_AD_numpy);
    return res;
}

// numpy$B_RealD_ndarray /////////////////////////////////////////////////////////////////////////////////////////////

void numpy$B_RealD_ndarrayD___init__(numpy$B_RealD_ndarray wit, numpy$$Primitive W_PrimitiveD_AD_numpy) {
    wit->W_Minus = (B_Minus)$NEW(numpy$B_MinusD_ndarray,(B_Real)wit);
    wit->W_PrimitiveD_AD_Real$ndarray =  W_PrimitiveD_AD_numpy;
}; 

numpy$B_RealD_ndarray numpy$B_RealD_ndarrayG_new(numpy$$Primitive W_PrimitiveD_AD_numpy) {
    numpy$B_RealD_ndarray res = malloc(sizeof (struct numpy$B_RealD_ndarray));
    res->$class = &numpy$B_RealD_ndarrayG_methods;
    numpy$B_RealD_ndarrayD___init__(res, W_PrimitiveD_AD_numpy);
    return res;
}


void numpy$B_RealD_ndarrayD___serialize__(numpy$B_RealD_ndarray wit, $NoneType state) {
    $step_serialize(wit->W_Minus, state);
}

numpy$B_RealD_ndarray numpy$B_RealD_ndarrayD___deserialize__(numpy$B_RealD_ndarray wit, $NoneType state) {
    numpy$B_RealD_ndarray res = $DNEW(numpy$B_RealD_ndarray,state);
    res->W_Minus = (B_Minus)$step_deserialize(state);
    return res;
}

numpy$$ndarray numpy$B_RealD_ndarrayD___add__(numpy$B_RealD_ndarray wit, numpy$$ndarray a, numpy$$ndarray b){
    return numpy$$oper(wit->W_PrimitiveD_AD_Real$ndarray->$class->$add,a,b);
}

numpy$$ndarray numpy$B_RealD_ndarrayD___fromatom__(numpy$B_RealD_ndarray wit,B_atom a) {
    return numpy$$fromatom((numpy$$Primitive)numpy$$PrimitiveB_floatG_witness, a);
}

B_complex numpy$B_RealD_ndarrayD___complx__(numpy$B_RealD_ndarray wit, numpy$$ndarray a) {
    $RAISE((B_BaseException)$NEW(B_NotImplementedError,to$str("complex not implemented for ndarray")));
    return NULL;
}

numpy$$ndarray numpy$B_RealD_ndarrayD___mul__(numpy$B_RealD_ndarray wit, numpy$$ndarray a, numpy$$ndarray b) {
    return numpy$$oper(wit->W_PrimitiveD_AD_Real$ndarray->$class->$mul,a,b);
}

numpy$$ndarray numpy$B_RealD_ndarrayD___pow__(numpy$B_RealD_ndarray wit, numpy$$ndarray a, numpy$$ndarray b) {
    return numpy$$oper(wit->W_PrimitiveD_AD_Real$ndarray->$class->$pow,a,b);
}

numpy$$ndarray numpy$B_RealD_ndarrayD___neg__(numpy$B_RealD_ndarray wit, numpy$$ndarray a) {
    return numpy$$func(wit->W_PrimitiveD_AD_Real$ndarray->$class->$neg,a);
}

numpy$$ndarray numpy$B_RealD_ndarrayD___pos__(numpy$B_RealD_ndarray wit, numpy$$ndarray a) {
    return a;
}

$WORD numpy$B_RealD_ndarray$real(numpy$B_RealD_ndarray wit, numpy$$ndarray a, B_Real wit2) {
    return a;
}
$WORD numpy$B_RealD_ndarray$imag(numpy$B_RealD_ndarray wit, numpy$$ndarray a, B_Real wit2);
$WORD numpy$B_RealD_ndarrayD___abs__(numpy$B_RealD_ndarray wit, numpy$$ndarray a, B_Real wit2) {
    return numpy$$func(wit->W_PrimitiveD_AD_Real$ndarray->$class->$abs,a);
}
numpy$$ndarray numpy$B_RealD_ndarray$conjugate(numpy$B_RealD_ndarray wit, numpy$$ndarray a);
B_float numpy$B_RealD_ndarrayD___float__ (numpy$B_RealD_ndarray wit, numpy$$ndarray a);
$WORD numpy$B_RealD_ndarrayD___trunc__ (numpy$B_RealD_ndarray wit, numpy$$ndarray a, B_Integral wit2);
$WORD numpy$B_RealD_ndarrayD___floor__ (numpy$B_RealD_ndarray wit, numpy$$ndarray a, B_Integral wit2);
$WORD numpy$B_RealD_ndarrayD___ceil__ (numpy$B_RealD_ndarray wit, numpy$$ndarray a, B_Integral wit2);
numpy$$ndarray numpy$B_RealD_ndarrayD___round__ (numpy$B_RealD_ndarray wit, numpy$$ndarray a, numpy$$ndarray b);

/*
  $WORD numpy$B_RealD_ndarray$numerator (numpy$B_RealD_ndarray wit, numpy$$ndarray a, B_Integral wit2);
  $WORD numpy$B_RealD_ndarray$denominator (numpy$B_RealD_ndarray wit, numpy$$ndarray a, B_Integral wit2);
  numpy$$ndarray numpy$B_RealD_ndarrayD___int__ (numpy$B_RealD_ndarray wit, numpy$$ndarray a);
  numpy$$ndarray numpy$B_RealD_ndarrayD___index__ (numpy$B_RealD_ndarray wit, numpy$$ndarray a);
  B_tuple numpy$B_RealD_ndarrayD___divmod__ (numpy$B_RealD_ndarray wit, numpy$$ndarray a, numpy$$ndarray b);
  numpy$$ndarray numpy$B_RealD_ndarrayD___floordiv__ (numpy$B_RealD_ndarray wit, numpy$$ndarray a, numpy$$ndarray b) {
  return numpy$$oper(wit->W_PrimitiveD_AD_Real$ndarray->$class->$floodiv,a,b);
  }  
  numpy$$ndarray numpy$B_RealD_ndarrayD___mod__ (numpy$B_RealD_ndarray wit, numpy$$ndarray a, numpy$$ndarray b);
  numpy$$ndarray numpy$B_RealD_ndarrayD___lshift__ (numpy$B_RealD_ndarray wit, numpy$$ndarray a, numpy$$ndarray b);
  numpy$$ndarray numpy$B_RealD_ndarrayD___rshift__ (numpy$B_RealD_ndarray wit, numpy$$ndarray a, numpy$$ndarray b);
  numpy$$ndarray numpy$B_RealD_ndarrayD___invert__ (numpy$B_RealD_ndarray wit, numpy$$ndarray a);
*/
 
// numpy$B_MinusD_ndarray /////////////////////////////////////////////////////////////////////////////////

void numpy$B_MinusD_ndarrayD___init__(numpy$B_MinusD_ndarray wit, B_Real W_Real) {
    wit->W_Real =  W_Real;
};

numpy$B_MinusD_ndarray numpy$B_MinusD_ndarrayG_new(B_Real W_Real) {
    numpy$B_MinusD_ndarray res = malloc(sizeof (struct numpy$B_MinusD_ndarray));
    res->$class = &numpy$B_MinusD_ndarrayG_methods;
    numpy$B_MinusD_ndarrayD___init__(res, W_Real);
    return res;
}

void numpy$B_MinusD_ndarrayD___serialize__(numpy$B_MinusD_ndarray wit, $NoneType state) {
    $step_serialize(wit->W_Real, state);
}

numpy$B_MinusD_ndarray numpy$B_MinusD_ndarrayD___deserialize__(numpy$B_MinusD_ndarray wit, $NoneType state) {
    numpy$B_MinusD_ndarray res = $DNEW(numpy$B_MinusD_ndarray,state);
    res->W_Real = (B_Real)$step_deserialize(state);
    return res;
}

numpy$$ndarray numpy$B_MinusD_ndarrayD___sub__ (numpy$B_MinusD_ndarray wit, numpy$$ndarray a, numpy$$ndarray b) {
    return numpy$$oper(((numpy$B_RealD_ndarray)wit->W_Real)-> W_PrimitiveD_AD_Real$ndarray->$class->$sub,a,b);
}

// numpy$B_DivD_ndarrayB_int /////////////////////////////////////////////////////////////////////////////////

void numpy$B_DivD_ndarrayB_intD___init__(numpy$B_DivD_ndarrayB_int wit) {
};

numpy$B_DivD_ndarrayB_int numpy$B_DivD_ndarrayB_intG_new() {
    numpy$B_DivD_ndarrayB_int res = malloc(sizeof (struct numpy$B_DivD_ndarrayB_int));
    res->$class = &numpy$B_DivD_ndarrayB_intG_methods;
    return res;
}

void numpy$B_DivD_ndarrayB_intD___serialize__(numpy$B_DivD_ndarrayB_int wit, $NoneType state) {
}

numpy$B_DivD_ndarrayB_int numpy$B_DivD_ndarrayB_intD___deserialize__(numpy$B_DivD_ndarrayB_int wit, $NoneType state) {
    numpy$B_DivD_ndarrayB_int res = $DNEW(numpy$B_DivD_ndarrayB_int,state);
    return res;
}

numpy$$ndarray numpy$B_DivD_ndarrayB_intD___truediv__ (numpy$B_DivD_ndarrayB_int wit, numpy$$ndarray a, numpy$$ndarray b) {
    numpy$$ndarray res = numpy$$oper(l$truediv,a,b);
    res->elem_type = DblType;
    return res;
}

// numpy$B_DivD_ndarrayB_float /////////////////////////////////////////////////////////////////////////////////

void numpy$B_DivD_ndarrayB_floatD___init__(numpy$B_DivD_ndarrayB_float wit) {
};

numpy$B_DivD_ndarrayB_float numpy$B_DivD_ndarrayB_floatG_new() {
    numpy$B_DivD_ndarrayB_float res = malloc(sizeof (struct numpy$B_DivD_ndarrayB_float));
    res->$class = &numpy$B_DivD_ndarrayB_floatG_methods;
    return res;
}

void numpy$B_DivD_ndarrayB_floatD___serialize__(numpy$B_DivD_ndarrayB_float wit, $NoneType state) {
}

numpy$B_DivD_ndarrayB_float numpy$B_DivD_ndarrayB_floatD___deserialize__(numpy$B_DivD_ndarrayB_float wit, $NoneType state) {
    numpy$B_DivD_ndarrayB_float res = $DNEW(numpy$B_DivD_ndarrayB_float,state);
    return res;
}

numpy$$ndarray numpy$B_DivD_ndarrayB_floatD___truediv__ (numpy$B_DivD_ndarrayB_float wit, numpy$$ndarray a, numpy$$ndarray b) {
    return numpy$$oper(d$truediv,a,b);
}

// Sliceable$ndarray ///////////////////////////////////////////////////////////////////////////////

void numpy$B_SliceableD_ndarrayD___init__ (numpy$B_SliceableD_ndarray self, numpy$$Primitive pwit) {
    self->pwit = pwit;
}

void numpy$B_SliceableD_ndarrayD___serialize__(numpy$B_SliceableD_ndarray wit, $NoneType state) {
}

numpy$B_SliceableD_ndarray numpy$B_SliceableD_ndarrayG_new(numpy$$Primitive pwit) {
    numpy$B_SliceableD_ndarray res = malloc(sizeof(struct numpy$B_SliceableD_ndarray));
    res->$class = &numpy$B_SliceableD_ndarrayG_methods;
    numpy$B_SliceableD_ndarrayD___init__(res, pwit);
    return res;
}

numpy$B_SliceableD_ndarray numpy$B_SliceableD_ndarrayD___deserialize__(numpy$B_SliceableD_ndarray wit, $NoneType state) {
    numpy$B_SliceableD_ndarray res = $DNEW(numpy$B_SliceableD_ndarray,state);
    return res;
}

numpy$$ndarray numpy$B_SliceableD_ndarrayD___getitem__ (numpy$B_SliceableD_ndarray wit, numpy$$ndarray a, B_int i) {
    B_list lst = B_listG_new(NULL, NULL);
    B_listD_append(lst, numpy$$ndindexG_new(i));
    return a->$class->__ndgetslice__(a, lst);
}

void numpy$B_SliceableD_ndarrayD___setitem__ (numpy$B_SliceableD_ndarray wit, numpy$$ndarray a, B_int i, $WORD val) {
    fprintf(stderr,"Internal error: call to mutating method setitem on ndarray");
    exit(-1);
}

void numpy$B_SliceableD_ndarrayD___delitem__ (numpy$B_SliceableD_ndarray wit, numpy$$ndarray a, B_int i) {
    fprintf(stderr,"Internal error: call to mutating method delitem on ndarray");
    exit(-1);
}

numpy$$ndarray numpy$B_SliceableD_ndarrayD___getslice__ (numpy$B_SliceableD_ndarray wit, numpy$$ndarray a, B_slice slc) {
    B_list lst = B_listG_new(NULL, NULL);
    B_listD_append(lst, numpy$$ndsliceG_new(slc));
    return a->$class->__ndgetslice__(a, lst);
}

void numpy$B_SliceableD_ndarrayD___setslice__ (numpy$B_SliceableD_ndarray wit, numpy$$ndarray a, B_Iterable wit2, B_slice slc, $WORD iter) {
    fprintf(stderr,"Internal error: call to mutating method setslice on ndarray");
    exit(-1);
}

void numpy$B_SliceableD_ndarrayD___delslice__ (numpy$B_SliceableD_ndarray wit, numpy$$ndarray a, B_slice slc) {
    fprintf(stderr,"Internal error: call to mutating method delslice on ndarray");
    exit(-1);
}

// numpy$B_CollectionD_ndarray ////////////////////////////////////////////////////////


void numpy$B_CollectionD_ndarrayD___init__(numpy$B_CollectionD_ndarray self, numpy$$Primitive pwit) {
    self->pwit = pwit;
}
  
numpy$B_CollectionD_ndarray numpy$B_CollectionD_ndarrayG_new(numpy$$Primitive pwit) {
    numpy$B_CollectionD_ndarray res = malloc(sizeof (struct numpy$B_CollectionD_ndarray));
    res->$class = &numpy$B_CollectionD_ndarrayG_methods;
    numpy$B_CollectionD_ndarrayD___init__(res, pwit);
    return res;
}

void numpy$B_CollectionD_ndarrayD___serialize__(numpy$B_CollectionD_ndarray wit, $NoneType state) {
}

numpy$B_CollectionD_ndarray numpy$B_CollectionD_ndarrayD___deserialize__(numpy$B_CollectionD_ndarray wit, $NoneType state) {
    numpy$B_CollectionD_ndarray res = $DNEW(numpy$B_CollectionD_ndarray,state);
    return res;
}


B_Iterator numpy$B_CollectionD_ndarrayD___iter__(numpy$B_CollectionD_ndarray self, numpy$$ndarray a) {
    return (B_Iterator)numpy$B_IteratorD_ndarrayG_new(self->pwit,a);
}

numpy$$ndarray numpy$B_CollectionD_ndarrayD___fromiter__(numpy$B_CollectionD_ndarray wit, B_Iterable iter) {
    return NULL;
}
B_int numpy$B_CollectionD_ndarrayD___len__(numpy$B_CollectionD_ndarray wit, numpy$$ndarray a) {
    return B_listD_getitem(a->shape,-1);
}


struct numpy$B_IntegralD_ndarrayB_int numpy$B_IntegralD_ndarrayB_int$instance;
struct numpy$B_LogicalD_ndarrayB_int numpy$B_LogicalD_ndarrayB_int$instance;
struct numpy$B_MinusD_ndarrayB_int numpy$B_MinusD_ndarrayB_int$instance;
struct numpy$B_RealD_ndarray numpy$B_RealD_ndarray$instance;
struct numpy$B_MinusD_ndarray numpy$B_MinusD_ndarray$instance;
struct numpy$B_SliceableD_ndarray numpy$B_SliceableD_ndarray$instance;

struct numpy$B_IntegralD_ndarrayB_intG_class numpy$B_IntegralD_ndarrayB_intG_methods = {
    "numpy$B_IntegralD_ndarrayB_int",
    UNASSIGNED,
    ($SuperG_class)&B_IntegralG_methods,
    numpy$B_IntegralD_ndarrayB_intD___init__,
    numpy$B_IntegralD_ndarrayB_intD___serialize__,
    numpy$B_IntegralD_ndarrayB_intD___deserialize__,
    (B_bool (*)(numpy$B_IntegralD_ndarrayB_int))$default__bool__,
    (B_str (*)(numpy$B_IntegralD_ndarrayB_int))$default__str__,
    (B_str (*)(numpy$B_IntegralD_ndarrayB_int))$default__str__,
    numpy$B_IntegralD_ndarrayB_intD___add__,
    (numpy$$ndarray (*)(numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray))$PlusD___iadd__,    
    numpy$B_IntegralD_ndarrayB_intD___mul__,
    (numpy$$ndarray (*)(numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray))B_TimesD___imul__ ,
    numpy$B_IntegralD_ndarrayB_intD___fromatom__,
    NULL,
    numpy$B_IntegralD_ndarrayB_intD___pow__,
    (numpy$$ndarray (*)(numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray))B_NumberD___ipow__ ,
    numpy$B_IntegralD_ndarrayB_intD___neg__,
    numpy$B_IntegralD_ndarrayB_intD___pos__,
    NULL,
    NULL,
    numpy$B_IntegralD_ndarrayB_intD___abs__,
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
    numpy$B_IntegralD_ndarrayB_intD___floordiv__ ,
    NULL,
    NULL,
    NULL,
    (numpy$$ndarray (*)(numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray))B_IntegralD___ifloordiv__,
    NULL,
    NULL,
    NULL,
    NULL
};

struct numpy$B_IntegralD_ndarrayB_int numpy$B_IntegralD_ndarrayB_int$instance = {&numpy$B_IntegralD_ndarrayB_intG_methods,
                                                                           (B_Logical)&numpy$B_LogicalD_ndarrayB_int$instance,  (B_Minus)&numpy$B_MinusD_ndarrayB_int$instance};
numpy$B_IntegralD_ndarrayB_int numpy$B_IntegralD_ndarrayB_intG_witness = &numpy$B_IntegralD_ndarrayB_int$instance;

struct numpy$B_LogicalD_ndarrayB_intG_class numpy$B_LogicalD_ndarrayB_intG_methods =  {
    "numpy$B_LogicalD_ndarrayB_int",
    UNASSIGNED,
    ($SuperG_class)&B_LogicalG_methods,
    numpy$B_LogicalD_ndarrayB_intD___init__,
    numpy$B_LogicalD_ndarrayB_intD___serialize__,
    numpy$B_LogicalD_ndarrayB_intD___deserialize__,
    (B_bool (*)(numpy$B_LogicalD_ndarrayB_int))$default__bool__,
    (B_str (*)(numpy$B_LogicalD_ndarrayB_int))$default__str__,
    (B_str (*)(numpy$B_LogicalD_ndarrayB_int))$default__str__,
    numpy$B_LogicalD_ndarrayB_intD___and__,
    numpy$B_LogicalD_ndarrayB_intD___or__,
    numpy$B_LogicalD_ndarrayB_intD___xor__,
    (numpy$$ndarray (*)(numpy$B_LogicalD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray))B_LogicalD___iand__,
    (numpy$$ndarray (*)(numpy$B_LogicalD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray))B_LogicalD___ior__,
    (numpy$$ndarray (*)(numpy$B_LogicalD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray))B_LogicalD___ixor__
};

struct numpy$B_LogicalD_ndarrayB_int numpy$B_LogicalD_ndarrayB_int$instance = {&numpy$B_LogicalD_ndarrayB_intG_methods, (B_Integral)&numpy$B_IntegralD_ndarrayB_int$instance};
numpy$B_LogicalD_ndarrayB_int numpy$B_LogicalD_ndarrayB_intG_witness = &numpy$B_LogicalD_ndarrayB_int$instance;

struct numpy$B_MinusD_ndarrayB_intG_class numpy$B_MinusD_ndarrayB_intG_methods = {
    "numpy$B_MinusD_ndarrayB_int",
    UNASSIGNED,
    ($SuperG_class)&B_MinusG_methods,
    numpy$B_MinusD_ndarrayB_intD___init__,
    numpy$B_MinusD_ndarrayB_intD___serialize__,
    numpy$B_MinusD_ndarrayB_intD___deserialize__,
    (B_bool (*)(numpy$B_MinusD_ndarrayB_int))$default__bool__,
    (B_str (*)(numpy$B_MinusD_ndarrayB_int))$default__str__,
    (B_str (*)(numpy$B_MinusD_ndarrayB_int))$default__str__,
    numpy$B_MinusD_ndarrayB_intD___sub__,
    (numpy$$ndarray (*)(numpy$B_MinusD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray))B_MinusD___isub__
};
struct numpy$B_MinusD_ndarrayB_int numpy$B_MinusD_ndarrayB_int$instance = {&numpy$B_MinusD_ndarrayB_intG_methods,  (B_Integral)&numpy$B_IntegralD_ndarrayB_int$instance};
numpy$B_MinusD_ndarrayB_int numpy$B_MinusD_ndarrayB_intG_witness = &numpy$B_MinusD_ndarrayB_int$instance;

struct numpy$B_RealD_ndarrayG_class numpy$B_RealD_ndarrayG_methods = {
    "numpy$B_RealD_ndarray",
    UNASSIGNED,
    ($SuperG_class)&B_IntegralG_methods,
    numpy$B_RealD_ndarrayD___init__,
    numpy$B_RealD_ndarrayD___serialize__,
    numpy$B_RealD_ndarrayD___deserialize__,
    (B_bool (*)(numpy$B_RealD_ndarray))$default__bool__,
    (B_str (*)(numpy$B_RealD_ndarray))$default__str__,
    (B_str (*)(numpy$B_RealD_ndarray))$default__str__,
    numpy$B_RealD_ndarrayD___add__,
    (numpy$$ndarray (*)(numpy$B_RealD_ndarray, numpy$$ndarray, numpy$$ndarray))$PlusD___iadd__,    
    numpy$B_RealD_ndarrayD___mul__,
    (numpy$$ndarray (*)(numpy$B_RealD_ndarray, numpy$$ndarray, numpy$$ndarray))B_TimesD___imul__ ,
    numpy$B_RealD_ndarrayD___fromatom__,
    NULL,
    numpy$B_RealD_ndarrayD___pow__,
    (numpy$$ndarray (*)(numpy$B_RealD_ndarray, numpy$$ndarray, numpy$$ndarray))B_NumberD___ipow__ ,
    numpy$B_RealD_ndarrayD___neg__,
    numpy$B_RealD_ndarrayD___pos__,
    NULL,
    NULL,
    numpy$B_RealD_ndarrayD___abs__,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL
};
 
struct numpy$B_MinusD_ndarrayG_class numpy$B_MinusD_ndarrayG_methods = {
    "numpy$B_MinusD_ndarray",
    UNASSIGNED,
    ($SuperG_class)&B_MinusG_methods,
    numpy$B_MinusD_ndarrayD___init__,
    numpy$B_MinusD_ndarrayD___serialize__,
    numpy$B_MinusD_ndarrayD___deserialize__,
    (B_bool (*)(numpy$B_MinusD_ndarray))$default__bool__,
    (B_str (*)(numpy$B_MinusD_ndarray))$default__str__,
    (B_str (*)(numpy$B_MinusD_ndarray))$default__str__,
    numpy$B_MinusD_ndarrayD___sub__,
    (numpy$$ndarray (*)(numpy$B_MinusD_ndarray, numpy$$ndarray, numpy$$ndarray))B_MinusD___isub__
};

struct numpy$B_MinusD_ndarray numpy$B_MinusD_ndarray$instance = {&numpy$B_MinusD_ndarrayG_methods,  (B_Real)&numpy$B_RealD_ndarray$instance};
numpy$B_MinusD_ndarray numpy$B_MinusD_ndarrayG_witness = &numpy$B_MinusD_ndarray$instance;

struct numpy$B_DivD_ndarrayB_intG_class numpy$B_DivD_ndarrayB_intG_methods = {
    "numpy$B_DivD_ndarrayB_int",
    UNASSIGNED,
    ($SuperG_class)&B_MinusG_methods,
    numpy$B_DivD_ndarrayB_intD___init__,
    numpy$B_DivD_ndarrayB_intD___serialize__,
    numpy$B_DivD_ndarrayB_intD___deserialize__,
    (B_bool (*)(numpy$B_DivD_ndarrayB_int))$default__bool__,
    (B_str (*)(numpy$B_DivD_ndarrayB_int))$default__str__,
    (B_str (*)(numpy$B_DivD_ndarrayB_int))$default__str__,
    numpy$B_DivD_ndarrayB_intD___truediv__,
    (numpy$$ndarray (*)(numpy$B_DivD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray))B_DivD___itruediv__
};

struct numpy$B_DivD_ndarrayB_int numpy$B_DivD_ndarrayB_int$instance = {&numpy$B_DivD_ndarrayB_intG_methods};
numpy$B_DivD_ndarrayB_int numpy$B_DivD_ndarrayB_intG_witness = &numpy$B_DivD_ndarrayB_int$instance;

struct numpy$B_DivD_ndarrayB_floatG_class numpy$B_DivD_ndarrayB_floatG_methods = {
    "numpy$B_DivD_ndarrayB_float",
    UNASSIGNED,
    ($SuperG_class)&B_MinusG_methods,
    numpy$B_DivD_ndarrayB_floatD___init__,
    numpy$B_DivD_ndarrayB_floatD___serialize__,
    numpy$B_DivD_ndarrayB_floatD___deserialize__,
    (B_bool (*)(numpy$B_DivD_ndarrayB_float))$default__bool__,
    (B_str (*)(numpy$B_DivD_ndarrayB_float))$default__str__,
    (B_str (*)(numpy$B_DivD_ndarrayB_float))$default__str__,
    numpy$B_DivD_ndarrayB_floatD___truediv__,
    (numpy$$ndarray (*)(numpy$B_DivD_ndarrayB_float, numpy$$ndarray, numpy$$ndarray))B_DivD___itruediv__
};

struct numpy$B_DivD_ndarrayB_float numpy$B_DivD_ndarrayB_float$instance = {&numpy$B_DivD_ndarrayB_floatG_methods};
numpy$B_DivD_ndarrayB_float numpy$B_DivD_ndarrayB_floatG_witness = &numpy$B_DivD_ndarrayB_float$instance;

struct numpy$B_SliceableD_ndarrayG_class numpy$B_SliceableD_ndarrayG_methods = {
    "numpy$B_SliceableD_ndarray",
    UNASSIGNED,
    ($SuperG_class)&B_SliceableG_methods,
    numpy$B_SliceableD_ndarrayD___init__,
    numpy$B_SliceableD_ndarrayD___serialize__,
    numpy$B_SliceableD_ndarrayD___deserialize__,
    (B_bool (*)(numpy$B_SliceableD_ndarray))$default__bool__,
    (B_str (*)(numpy$B_SliceableD_ndarray))$default__str__,
    (B_str (*)(numpy$B_SliceableD_ndarray))$default__str__,
    numpy$B_SliceableD_ndarrayD___getitem__,
    numpy$B_SliceableD_ndarrayD___setitem__,
    numpy$B_SliceableD_ndarrayD___delitem__,
    numpy$B_SliceableD_ndarrayD___getslice__,
    numpy$B_SliceableD_ndarrayD___setslice__,
    numpy$B_SliceableD_ndarrayD___delslice__,
};
struct numpy$B_SliceableD_ndarray numpy$B_SliceableD_instance = {&numpy$B_SliceableD_ndarrayG_methods};
numpy$B_SliceableD_ndarray numpy$B_SliceableD_ndarrayG_witness = &numpy$B_SliceableD_instance;


struct numpy$B_CollectionD_ndarrayG_class numpy$B_CollectionD_ndarrayG_methods = {
    "numpy$B_CollectionD_ndarray",
    UNASSIGNED,
    ($SuperG_class)&B_CollectionG_methods,
    numpy$B_CollectionD_ndarrayD___init__,
    numpy$B_CollectionD_ndarrayD___serialize__,
    numpy$B_CollectionD_ndarrayD___deserialize__,
    (B_bool (*)(numpy$B_CollectionD_ndarray))$default__bool__,
    (B_str (*)(numpy$B_CollectionD_ndarray))$default__str__,
    (B_str (*)(numpy$B_CollectionD_ndarray))$default__str__,
    numpy$B_CollectionD_ndarrayD___iter__,
    numpy$B_CollectionD_ndarrayD___fromiter__,
    numpy$B_CollectionD_ndarrayD___len__
};

struct numpy$B_CollectionD_ndarray numpy$B_CollectionD_instance = {&numpy$B_CollectionD_ndarrayG_methods};
numpy$B_CollectionD_ndarray numpy$B_CollectionD_ndarrayG_witness = &numpy$B_CollectionD_instance;


// numpy$B_RealFuns$math$ndarray ///////////////////////////////////////////////////////////

$NoneType numpy$B_RealFuns$math$ndarrayD___init__ (numpy$B_RealFuns$math$ndarray W_self, numpy$$Primitive W_Primitive, math$B_RealFuns W_RealFuns) {
    W_self->W_PrimitiveD_AD_RealFuns$math$ndarray = W_Primitive;
    W_self-> W_RealFuns$mathD_AD_RealFuns$math$ndarray = W_RealFuns;
    return $None;
}

$NoneType numpy$B_RealFuns$math$ndarrayD___serialize__(numpy$B_RealFuns$math$ndarray wit, $NoneType state) {
    $step_serialize(wit->W_PrimitiveD_AD_RealFuns$math$ndarray, state);
    $step_serialize(wit->W_RealFuns$mathD_AD_RealFuns$math$ndarray, state);
    return $None;
}

numpy$B_RealFuns$math$ndarray numpy$B_RealFuns$math$ndarrayD___deserialize__(numpy$B_RealFuns$math$ndarray wit, $NoneType state) {
    numpy$B_RealFuns$math$ndarray res = $DNEW(numpy$B_RealFuns$math$ndarray,state);
    res->W_PrimitiveD_AD_RealFuns$math$ndarray = (numpy$$Primitive)$step_deserialize(state);
    res->W_RealFuns$mathD_AD_RealFuns$math$ndarray = (math$B_RealFuns)$step_deserialize(state);
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
  
numpy$$ndarray numpy$B_RealFuns$math$ndarray$sqrt(numpy$B_RealFuns$math$ndarray wit, numpy$$ndarray a) {
    return numpy$$func(sqrtB,a);
}
numpy$$ndarray numpy$B_RealFuns$math$ndarray$exp(numpy$B_RealFuns$math$ndarray wit, numpy$$ndarray a) {
    return numpy$$func(expB,a);
}
numpy$$ndarray numpy$B_RealFuns$math$ndarray$log(numpy$B_RealFuns$math$ndarray wit, numpy$$ndarray a) {
    return numpy$$func(logB,a);
}
numpy$$ndarray numpy$B_RealFuns$math$ndarray$sin(numpy$B_RealFuns$math$ndarray wit, numpy$$ndarray a) {
    return numpy$$func(sinB,a);
}
numpy$$ndarray numpy$B_RealFuns$math$ndarray$cos(numpy$B_RealFuns$math$ndarray wit, numpy$$ndarray a) {
    return numpy$$func(cosB,a);
}
numpy$$ndarray numpy$B_RealFuns$math$ndarray$tan(numpy$B_RealFuns$math$ndarray wit, numpy$$ndarray a) {
    return numpy$$func(tanB,a);
}
numpy$$ndarray numpy$B_RealFuns$math$ndarray$asin(numpy$B_RealFuns$math$ndarray wit, numpy$$ndarray a) {
    return numpy$$func(asinB,a);
}
numpy$$ndarray numpy$B_RealFuns$math$ndarray$acos(numpy$B_RealFuns$math$ndarray wit, numpy$$ndarray a) {
    return numpy$$func(acosB,a);
}
numpy$$ndarray numpy$B_RealFuns$math$ndarray$atan(numpy$B_RealFuns$math$ndarray wit, numpy$$ndarray a) {
    return numpy$$func(atanB,a);
}
numpy$$ndarray numpy$B_RealFuns$math$ndarray$sinh(numpy$B_RealFuns$math$ndarray wit, numpy$$ndarray a) {
    return numpy$$func(sinhB,a);
}
numpy$$ndarray numpy$B_RealFuns$math$ndarray$cosh(numpy$B_RealFuns$math$ndarray wit, numpy$$ndarray a) {
    return numpy$$func(coshB,a);
}
numpy$$ndarray numpy$B_RealFuns$math$ndarray$tanh(numpy$B_RealFuns$math$ndarray wit, numpy$$ndarray a) {
    return numpy$$func(tanhB,a);
}
numpy$$ndarray numpy$B_RealFuns$math$ndarray$asinh(numpy$B_RealFuns$math$ndarray wit, numpy$$ndarray a) {
    return numpy$$func(asinhB,a);
}
numpy$$ndarray numpy$B_RealFuns$math$ndarray$acosh(numpy$B_RealFuns$math$ndarray wit, numpy$$ndarray a) {
    return numpy$$func(acoshB,a);
}
numpy$$ndarray numpy$B_RealFuns$math$ndarray$atanh(numpy$B_RealFuns$math$ndarray wit, numpy$$ndarray a) {
    return numpy$$func(atanhB,a);
}

                      
numpy$B_RealFuns$math$ndarray numpy$B_RealFuns$math$ndarrayG_new(numpy$$Primitive W_Primitive, math$B_RealFuns W_RealFuns) {
    numpy$B_RealFuns$math$ndarray $tmp = malloc(sizeof(struct numpy$B_RealFuns$math$ndarray));
    $tmp->$class = &numpy$B_RealFuns$math$ndarrayG_methods;
    numpy$B_RealFuns$math$ndarrayG_methods.__init__($tmp, W_Primitive, W_RealFuns);
    return $tmp;
}
struct numpy$B_RealFuns$math$ndarrayG_class numpy$B_RealFuns$math$ndarrayG_methods = {
    "numpy$B_RealFuns$math$ndarray",
    UNASSIGNED,
    ($SuperG_class)&math$B_RealFunsG_methods,
    numpy$B_RealFuns$math$ndarrayD___init__,
    numpy$B_RealFuns$math$ndarrayD___serialize__,
    numpy$B_RealFuns$math$ndarrayD___deserialize__,
    (B_bool (*)(numpy$B_RealFuns$math$ndarray))$default__bool__,
    (B_str (*)(numpy$B_RealFuns$math$ndarray))$default__str__,
    (B_str (*)(numpy$B_RealFuns$math$ndarray))$default__str__,
    numpy$B_RealFuns$math$ndarray$sqrt,        
    numpy$B_RealFuns$math$ndarray$exp,        
    numpy$B_RealFuns$math$ndarray$log,        
    numpy$B_RealFuns$math$ndarray$sin,        
    numpy$B_RealFuns$math$ndarray$cos,        
    numpy$B_RealFuns$math$ndarray$tan,        
    numpy$B_RealFuns$math$ndarray$asin,        
    numpy$B_RealFuns$math$ndarray$acos,        
    numpy$B_RealFuns$math$ndarray$atan,        
    numpy$B_RealFuns$math$ndarray$sinh,        
    numpy$B_RealFuns$math$ndarray$cosh,        
    numpy$B_RealFuns$math$ndarray$tanh,        
    numpy$B_RealFuns$math$ndarray$asinh,        
    numpy$B_RealFuns$math$ndarray$acosh,        
    numpy$B_RealFuns$math$ndarray$atanh     
};
 
