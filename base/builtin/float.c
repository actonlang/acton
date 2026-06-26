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

#include <math.h>

// General methods ///////////////////////////////////////////////////////////////////////

double B_floatG_new(B_atom a) {
    if ($ISINSTANCE0(a,B_int)) return (double)((B_int)a)->val;
    if ($ISINSTANCE0(a,B_i32)) return (double)((B_i32)a)->val;
    if ($ISINSTANCE0(a,B_i16)) return (double)((B_i8)a)->val;
    if ($ISINSTANCE0(a,B_i8)) return (double)((B_i16)a)->val;
    if ($ISINSTANCE0(a,B_u64)) return (double)((B_u64)a)->val;
    if ($ISINSTANCE0(a,B_u32)) return (double)((B_u32)a)->val;
    if ($ISINSTANCE0(a,B_u16)) return (double)((B_u16)a)->val;
    if ($ISINSTANCE0(a,B_u8)) return (double)((B_u8)a)->val;
    if ($ISINSTANCE0(a,B_u1)) return (double)((B_u1)a)->val;
    if ($ISINSTANCE0(a,B_bigint)) {
        zz_struct aval = ((B_bigint)a)->val;
        if (aval.size == 0) 
            return 0.0;
        if (labs(aval.size) > 16)
            $RAISE((B_BaseException)$NEW(B_ValueError,to$str("float(): int value too big for type float")));
        double pow = 1.0;  
        double res = 0.0;
        for (int i = 0; i<(labs(aval.size)); i++) {
            res += aval.n[i] * pow;
            pow *= 18446744073709551616.0; // literal is 2^64
        }
        return aval.size<0 ? -res : res;
    }
    if ($ISINSTANCE0(a,B_float)) return ((B_float)a)->val;
    if ($ISINSTANCE0(a,B_bool)) return (double)((B_bool)a)->val;
    if ($ISINSTANCE0(a,B_str)) {
        double x;
        int c;
        sscanf((char *)((B_str)a)->str,"%lf%n",&x,&c);
        if (c==((B_str)a)->nbytes)
            return x;
        else
            $RAISE((B_BaseException)$NEW(B_ValueError,to$str("float_fromatom(): invalid str literal for type float")));
    }
    fprintf(stderr,"internal error: float_fromatom: argument not of atomic type");
    exit(-1);

}

B_NoneType B_floatD___init__(B_float self, B_atom a){
    self->val = B_floatG_new(a);
    return B_None;
}

void B_floatD___serialize__(B_float self, $Serial$state state) {
    $val_serialize(FLOAT_ID,&self->val,state);
}

B_float B_floatD___deserialize__(B_float self, $Serial$state state) {
    $WORD w = $val_deserialize(state);
    double x;
    memcpy(&x,&w,sizeof($WORD));
    return to$float(x);
}

bool B_floatD___bool__(B_float x) {
    return x->val != 0.0;
}

B_str B_floatD___str__(B_float x) {
    return $FORMAT("%g", x->val);
}

B_str B_floatD___repr__(B_float x) {
    return $FORMAT("%g", x->val);
}

B_float to$float(double x) {
    B_float res = acton_malloc(sizeof(struct B_float));
    res->$class = &B_floatG_methods;
    res->val = x;
    return res;
}

B_float toB_float(double x) {
    B_float res = acton_malloc(sizeof(struct B_float));
    res->$class = &B_floatG_methods;
    res->val = x;
    return res;
}

double fromB_float(B_float x) {
    return x->val;
}


// B_RealFloatD_float /////////////////////////////////////////////////////////////////////////

B_float B_RealFloatD_floatD___add__(B_RealFloatD_float wit,  B_float a, B_float b) {
    return toB_float(a->val + b->val);
}  

B_float B_RealFloatD_floatD___zero__(B_RealFloatD_float wit) {
    return toB_float(0.0);
}

B_float B_RealFloatD_floatD___fromatom__(B_RealFloatD_float wit, B_atom a) {
    return toB_float(B_floatG_new(a));
}

B_complex B_RealFloatD_floatD___complex__(B_RealFloatD_float wit, B_float a) {
    return toB_complex(a->val);
}

B_float B_RealFloatD_floatD___mul__(B_RealFloatD_float wit,  B_float a, B_float b) {
    return toB_float(a->val * b->val);
}  

B_float B_RealFloatD_floatD___pow__(B_RealFloatD_float wit,  B_float a, B_float b) {
    if ( b->val < 0) {
        char errmsg[1024];
        snprintf(errmsg, sizeof(errmsg), "int.__pow__: negative exponent %f ",b->val);
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str(errmsg)));
    }
    return toB_float(exp(b->val * log(a->val)));
}

B_float B_RealFloatD_floatD___neg__(B_RealFloatD_float wit, B_float a) {
    return toB_float(-a->val);
}

B_float B_RealFloatD_floatD___pos__(B_RealFloatD_float wit, B_float a) {
    return a;
}

$WORD B_RealFloatD_floatD_real(B_RealFloatD_float wit, B_float a, B_Real wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)a);
}

$WORD B_RealFloatD_floatD_imag(B_RealFloatD_float wit, B_float a, B_Real wit2) {
    return B_RealFloatD_floatD___zero__(wit);
}

$WORD B_RealFloatD_floatD___abs__(B_RealFloatD_float wit, B_float a, B_Real wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)to$float(fabs(a->val)));
}

B_float B_RealFloatD_floatD_conjugate(B_RealFloatD_float wit, B_float a) {
    return a;
}
double B_RealFloatD_floatD___float__ (B_RealFloatD_float wit, B_float x) {
    return x->val;
}

$WORD B_RealFloatD_floatD___trunc__ (B_RealFloatD_float wit, B_float x, B_Integral wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)toB_int((long)trunc(x->val)));
}
  
$WORD B_RealFloatD_floatD___floor__ (B_RealFloatD_float wit, B_float x, B_Integral wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)toB_int((long)floor(x->val)));
}
  
$WORD B_RealFloatD_floatD___ceil__ (B_RealFloatD_float wit, B_float x, B_Integral wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)toB_int((long)ceil(x->val)));
}
  
B_float B_RealFloatD_floatD___round__ (B_RealFloatD_float wit, B_float x, B_int p) {
    double pval = p==NULL ? 0.0 : (double)p->val;
    double p10 = pow(10.0,pval);
    return toB_float(round(x->val * p10)/p10);
}
     
// B_MinusD_RealFloatD_float  ////////////////////////////////////////////////////////////////////////////////////////

 
B_float B_MinusD_RealFloatD_floatD___sub__(B_MinusD_RealFloatD_float wit,  B_float a, B_float b) {
    return toB_float(a->val - b->val);
}  

// B_DivD_float  ////////////////////////////////////////////////////////////////////////////////////////

B_float B_DivD_floatD___truediv__(B_DivD_float wit, B_float a, B_float b) {
    return toB_float(a->val / b->val);
}  

// B_OrdD_float  ////////////////////////////////////////////////////////////////////////////////////////

bool B_OrdD_floatD___eq__ (B_OrdD_float wit, B_float a, B_float b) {
    return a->val == b->val;
}

bool B_OrdD_floatD___ne__ (B_OrdD_float wit, B_float a, B_float b) {
    return a->val != b->val;
}

bool B_OrdD_floatD___lt__ (B_OrdD_float wit, B_float a, B_float b) {
    return a->val < b->val;
}

bool B_OrdD_floatD___le__ (B_OrdD_float wit, B_float a, B_float b) {
    return a->val <= b->val;
}

bool B_OrdD_floatD___gt__ (B_OrdD_float wit, B_float a, B_float b) {
    return a->val > b->val;
}

bool B_OrdD_floatD___ge__ (B_OrdD_float wit, B_float a, B_float b) {
    return a->val >= b->val;
}


// B_HashableD_float ///////////////////////////////////////////////////////////////////////////////////////////////////////

bool B_HashableD_floatD___eq__(B_HashableD_float wit, B_float a, B_float b) {
    return a->val == b->val;
}

bool B_HashableD_floatD___ne__(B_HashableD_float wit, B_float a, B_float b) {
    return a->val != b->val;
}

B_NoneType B_HashableD_floatD_hash(B_HashableD_float wit, B_float a, B_hasher h) {
    zig_hash_wyhash_update(h->_hasher, to$bytesD_len((char *)&a, 8));
    return B_None;
}
