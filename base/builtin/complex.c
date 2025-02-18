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

B_complex toB_complex(complex double c) {
    B_complex res = acton_malloc(sizeof(struct B_complex));
    res->$class = &B_complexG_methods;
    res->val = c;
    return res;
}

B_complex B_complexG_new(B_Number wit, $WORD c) {
    return $NEW(B_complex,wit,c);
}

B_complex B_complexD_from_real_imag (B_Real wit1, B_Real wit2, $WORD real, $WORD imag) {
    double re = wit1->$class->__float__(wit1, real)->val;
    double im = wit2->$class->__float__(wit2, imag)->val;
    return toB_complex(re + im * _Complex_I);
}

B_NoneType B_complexD___init__(B_complex self, B_Number wit, $WORD c){
    self->val = wit->$class->__complx__(wit,c)->val;
    return B_None;
}

void B_complexD___serialize__(B_complex c,$Serial$state state) {
    $ROW row = $add_header(COMPLEX_ID,2,state);
    double re = creal(c->val);
    double im = cimag(c->val);
    memcpy(row->blob,&re,sizeof(double));
    memcpy(row->blob+1,&im,sizeof(double));
}

B_complex B_complexD___deserialize__(B_complex self, $Serial$state state) {
    $ROW this = state->row;
    state->row =this->next;
    state->row_no++;
    double re, im;
    memcpy(&re,this->blob,sizeof(double));
    memcpy(&im,this->blob+1,sizeof(double));
    return toB_complex(re + im * _Complex_I);
}

B_bool B_BoolD_complexD___bool__( B_BoolD_complex wit, B_complex n) {
    return toB_bool(n->val != 0.0);
}

B_str B_ShowD_complexD___str__(B_ShowD_complex wit, B_complex c) {
    return $FORMAT("%f + %f*I", creal(c->val), cimag(c->val));
}
  
B_str B_ShowD_complexD___repr__(B_ShowD_complex wit, B_complex c) {
    return $FORMAT("%f + %f*I", creal(c->val), cimag(c->val));
}
  
// B_NumberD_complex  ////////////////////////////////////////////////////////////////////////////////////////

B_complex B_NumberD_complexD___add__(B_NumberD_complex wit, B_complex a, B_complex b) {
    return toB_complex(a->val + b->val);
}  

B_complex B_NumberD_complexD___zero__(B_NumberD_complex wit) {
    return toB_complex(0.0);
}

B_complex B_NumberD_complexD___complex__ (B_NumberD_complex wit, B_complex c) {
    return c;
}

B_complex B_NumberD_complexD___mul__ (B_NumberD_complex wit, B_complex a, B_complex b){
    return toB_complex(a->val * b->val);
}

B_complex B_NumberD_complexD___fromatom__(B_NumberD_complex wit, B_atom a) {
    $RAISE((B_BaseException)$NEW(B_NotImplementedError,to$str("__fromatom__ not implemented for complex")));
    return B_None;
}

B_complex B_NumberD_complexD___pow__ (B_NumberD_complex wit, B_complex a, B_complex b) {
    return toB_complex(cpow(a->val,b->val));
}

B_complex B_NumberD_complexD___neg__ (B_NumberD_complex wit, B_complex c){
    return toB_complex(-c->val);
}

B_complex B_NumberD_complexD___pos__ (B_NumberD_complex wit, B_complex c) {
    return c;
}

$WORD B_NumberD_complexD_real (B_NumberD_complex wit, B_complex c, B_Real wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)to$float(creal(c->val)));
}

$WORD B_NumberD_complexD_imag (B_NumberD_complex wit, B_complex c, B_Real wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)to$float(cimag(c->val)));
}

$WORD B_NumberD_complexD___abs__ (B_NumberD_complex wit, B_complex c, B_Real wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)to$float(cabs(c->val)));
}

B_complex B_NumberD_complexD_conjugate (B_NumberD_complex wit, B_complex c) {
    return toB_complex(conj(c->val));
}

// B_DivD_complex /////////////////////////////////////////////////////////////////////////////////////////

B_complex B_DivD_complexD___truediv__ (B_DivD_complex wit, B_complex a, B_complex b) {
    if (b->val == 0.0) {
        RAISE(B_ZeroDivisionError, to_str_noc("complex truediv: divisor is zero"));
    }
    return toB_complex(a->val/b->val);
}

// B_MinusD_NumberD_complex  ////////////////////////////////////////////////////////////////////////////////////////

B_complex B_MinusD_NumberD_complexD___sub__(B_MinusD_NumberD_complex wit, B_complex a, B_complex b) {
    return toB_complex(a->val - b->val);
}  
// B_EqD_complex  ////////////////////////////////////////////////////////////////////////////////////////

B_bool B_EqD_complexD___eq__ (B_EqD_complex wit, B_complex a, B_complex b) {
    return toB_bool(creal(a->val) == creal(b->val) && cimag(a->val) == cimag(b->val));
}

B_bool B_EqD_complexD___ne__ (B_EqD_complex wit, B_complex a, B_complex b) {
    return toB_bool(!fromB_bool(B_EqD_complexD___eq__(wit,a,b)));
}


// B_HashableD_complex  ////////////////////////////////////////////////////////////////////////////////////////

B_bool B_HashableD_complexD___eq__(B_HashableD_complex wit, B_complex a, B_complex b) {
    return toB_bool(creal(a->val) == creal(b->val) && cimag(a->val) == cimag(b->val));
}

B_bool B_HashableD_complexD___ne__(B_HashableD_complex wit, B_complex a, B_complex b) {
    return toB_bool(!fromB_bool(B_HashableD_complexD___eq__(wit,a,b)));
}

B_int B_HashableD_complexD___hash__(B_HashableD_complex wit, B_complex a) {
    return to$int(B_complexD_hash(a));
}
 
