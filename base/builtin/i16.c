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

// Auxiliary //////////////////////////////////////////////////////////////////////////////

// only called with e>=0.
int16_t i16_pow(int16_t a, int16_t e) {
    if (e == 0) return 1;
    if (e == 1) return a;
    if (e%2 == 0) return i16_pow(a*a,e/2);
    return a * i16_pow(a*a,e/2);
}

// General methods ///////////////////////////////////////////////////////////////////////

int16_t B_i16G_new(B_atom a, B_int base) {
    B_bigint b = B_bigintG_new(a, base);
    unsigned long n = b->val.n[0];
    long sz = b->val.size;
    if (labs(sz) > 1 || (sz==1 && n > 0x7ffffffful) || sz == -1 && n > 0x80000000ul) {
        char errmsg[1024];
        snprintf(errmsg, sizeof(errmsg), "i16(): value %s out of range for type i16",get_str(&b->val));
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str(errmsg)));
    }
    return (int16_t)(n*sz);
}


B_NoneType B_i16D___init__(B_i16 self, B_atom a, B_int base){
    self->val = B_i16G_new(a,base);
    return B_None;
}

void B_i16D___serialize__(B_i16 n, $Serial$state state) {
    $val_serialize(I16_ID,&n->val,state);
}

B_i16 B_i16D___deserialize__(B_i16 n, $Serial$state state) {
    return toB_i16((int16_t)(uintptr_t)$val_deserialize(state));
}

B_bool B_i16D___bool__(B_i16 n) {
    return toB_bool(n->val != 0);
}

B_str B_i16D___str__(B_i16 n) {
    return $FORMAT("%d", n->val);
}

B_str B_i16D___repr__(B_i16 n) {
    return $FORMAT("%d", n->val);
}

B_i16 toB_i16(int16_t i) {
    B_i16 res = acton_malloc(sizeof(struct B_i16));
    res->$class = &B_i16G_methods;
    res->val = i;
    return res;
}

int16_t fromB_i16(B_i16 w) {
    return w->val;
}

                  

// B_IntegralD_i16 /////////////////////////////////////////////////////////////////////////

 
B_i16 B_IntegralD_i16D___add__(B_IntegralD_i16 wit,  B_i16 a, B_i16 b) {
    return toB_i16(a->val+b->val);
}  

B_i16 B_IntegralD_i16D___zero__(B_IntegralD_i16 wit) {
    return toB_i16(0);
}

B_complex B_IntegralD_i16D___complex__(B_IntegralD_i16 wit, B_i16 a) {
    return toB_complex((double)(a->val));
}

B_i16 B_IntegralD_i16D___fromatom__(B_IntegralD_i16 wit, B_atom a) {
    return toB_i16(B_i16G_new(a,NULL));
}

B_i16 B_IntegralD_i16D___mul__(B_IntegralD_i16 wit,  B_i16 a, B_i16 b) {
    return toB_i16(a->val * b->val);
}  
  
B_i16 B_IntegralD_i16D___pow__(B_IntegralD_i16 wit,  B_i16 a, B_i16 b) {
    int16_t aval = a->val;
    int16_t bval = b->val;
    if ( bval < 0) {
        char errmsg[1024];
        snprintf(errmsg, sizeof(errmsg), "int.__pow__: negative exponent %d ",bval);
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str(errmsg)));
    }
    return toB_i16(i16_pow(aval,bval));
}

B_i16 B_IntegralD_i16D___neg__(B_IntegralD_i16 wit,  B_i16 a) {
    return toB_i16(-a->val);
}

B_i16 B_IntegralD_i16D___pos__(B_IntegralD_i16 wit,  B_i16 a) {
    return a;
}

$WORD B_IntegralD_i16D_real(B_IntegralD_i16 wit, B_i16 a, B_Real wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)(a));
}

$WORD B_IntegralD_i16D_imag(B_IntegralD_i16 wit, B_i16 a, B_Real wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)toB_i16(0));
}

$WORD B_IntegralD_i16D___abs__(B_IntegralD_i16 wit, B_i16 a, B_Real wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)toB_i16(abs(a->val)));
}

B_i16 B_IntegralD_i16D_conjugate(B_IntegralD_i16 wit,  B_i16 a) {
    return a;
}

double B_IntegralD_i16D___float__ (B_IntegralD_i16 wit, B_i16 n) {
    return (double)n->val;
}

$WORD B_IntegralD_i16D___trunc__ (B_IntegralD_i16 wit, B_i16 n, B_Integral wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)n);
}
  
$WORD B_IntegralD_i16D___floor__ (B_IntegralD_i16 wit, B_i16 n, B_Integral wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)n);
}
  
$WORD B_IntegralD_i16D___ceil__ (B_IntegralD_i16 wit, B_i16 n, B_Integral wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)n);
}
  
B_i16 B_IntegralD_i16D___round__ (B_IntegralD_i16 wit, B_i16 n, B_int p) {
    int16_t nval = n->val;
    if (nval<0)
        return toB_i16(-B_IntegralD_i16D___round__(wit,toB_i16(-nval),p)->val);
    int pval = p==NULL ? 0 : fromB_int(p);
    if (pval>=0)
        return n;
    int p10 = i16_pow(10,-pval);
    int res = nval/p10;
    if (nval%p10 * 2 > p10)
        res++; 
    return toB_i16(res * p10);
}
  
$WORD B_IntegralD_i16D_numerator (B_IntegralD_i16 wit, B_i16 n, B_Integral wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)n);
}
  
$WORD B_IntegralD_i16D_denominator (B_IntegralD_i16 wit, B_i16 n, B_Integral wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)toB_i16(1));
}
  
int64_t B_IntegralD_i16D___int__ (B_IntegralD_i16 wit, B_i16 n) {
    return (int64_t)n->val;
}

int64_t B_IntegralD_i16D___index__(B_IntegralD_i16 wit, B_i16 n) {
    return  (int64_t)n->val;
}

B_tuple B_IntegralD_i16D___divmod__(B_IntegralD_i16 wit, B_i16 a, B_i16 b) {
    int16_t n = a->val;
    int16_t d = b->val;
    return $NEWTUPLE(2, toB_i16(n/d), toB_i16(n%d));
}

B_i16 B_IntegralD_i16D___floordiv__(B_IntegralD_i16 wit, B_i16 a, B_i16 b) {
    if (b->val == 0)
        $RAISE((B_BaseException)$NEW(B_ZeroDivisionError, to$str("division by zero")));
    return toB_i16(a->val / b->val);
}

B_i16 B_IntegralD_i16D___mod__(B_IntegralD_i16 wit, B_i16 a, B_i16 b) {
    return toB_i16(a->val % b->val);
}

B_i16 B_IntegralD_i16D___lshift__(B_IntegralD_i16 wit,  B_i16 a, int64_t b) {
    return toB_i16(a->val << b);
}

B_i16 B_IntegralD_i16D___rshift__(B_IntegralD_i16 wit,  B_i16 a, int64_t b) {
    return toB_i16(a->val >> b);
}
 
B_i16 B_IntegralD_i16D___invert__(B_IntegralD_i16 wit,  B_i16 a) {
    return toB_i16( ~a->val);
}


// B_LogicalD_IntegralD_i16  ////////////////////////////////////////////////////////////////////////////////////////

B_i16 B_LogicalD_IntegralD_i16D___and__(B_LogicalD_IntegralD_i16 wit,  B_i16 a, B_i16 b) {
    return toB_i16(a->val & b->val);
}
                                                 
B_i16 B_LogicalD_IntegralD_i16D___or__(B_LogicalD_IntegralD_i16 wit,  B_i16 a, B_i16 b) {
    return toB_i16(a->val | b->val);
}
                                                 
B_i16 B_LogicalD_IntegralD_i16D___xor__(B_LogicalD_IntegralD_i16 wit,  B_i16 a, B_i16 b) {
    return toB_i16(a->val ^ b->val);
}  
 
// B_MinusD_IntegralD_i16  ////////////////////////////////////////////////////////////////////////////////////////

 
B_i16 B_MinusD_IntegralD_i16D___sub__(B_MinusD_IntegralD_i16 wit,  B_i16 a, B_i16 b) {
    return toB_i16(a->val - b->val);
}  

// B_DivD_i16  ////////////////////////////////////////////////////////////////////////////////////////

 
B_float B_DivD_i16D___truediv__ (B_DivD_i16 wit, B_i16 a, B_i16 b) {
    if (b->val == 0)
        $RAISE((B_BaseException)$NEW(B_ZeroDivisionError, to$str("division by zero")));
    return toB_float((double)a->val/(double)b->val);
}

// B_OrdD_i16  ////////////////////////////////////////////////////////////////////////////////////////

B_bool B_OrdD_i16D___eq__ (B_OrdD_i16 wit, B_i16 a, B_i16 b) {
    return toB_bool(a->val == b->val);
}

B_bool B_OrdD_i16D___ne__ (B_OrdD_i16 wit, B_i16 a, B_i16 b) {
    return toB_bool(a->val != b->val);
}

B_bool B_OrdD_i16D___lt__ (B_OrdD_i16 wit, B_i16 a, B_i16 b) {
    return toB_bool(a->val < b->val);
}

B_bool B_OrdD_i16D___le__ (B_OrdD_i16 wit, B_i16 a, B_i16 b) {
    return toB_bool(a->val <= b->val);
}

B_bool B_OrdD_i16D___gt__ (B_OrdD_i16 wit, B_i16 a, B_i16 b) {
    return toB_bool(a->val > b->val);
}

B_bool B_OrdD_i16D___ge__ (B_OrdD_i16 wit, B_i16 a, B_i16 b) {
    return toB_bool(a->val >= b->val);
}

// B_HashableD_i16 ///////////////////////////////////////////////////////////////////////////////////////////////////////

B_bool B_HashableD_i16D___eq__(B_HashableD_i16 wit, B_i16 a, B_i16 b) {
    return toB_bool(a->val == b->val);
}

B_bool B_HashableD_i16D___ne__(B_HashableD_i16 wit, B_i16 a, B_i16 b) {
    return toB_bool(a->val != b->val);
}

B_NoneType B_HashableD_i16D_hash(B_HashableD_i16 wit, B_i16 a, B_hasher h) {
    zig_hash_wyhash_update(h->_hasher, to$bytesD_len((char *)&(a), 4));
    return B_None;
}
