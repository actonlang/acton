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
uint8_t u8_pow(uint8_t a, uint8_t e) {
    if (e == 0) return 1;
    if (e == 1) return a;
    if (e%2 == 0) return u8_pow(a*a,e/2);
    return a * u8_pow(a*a,e/2);
}

// General methods ///////////////////////////////////////////////////////////////////////

B_u8 B_u8G_new(B_atom a, B_int base) {
    B_int b = B_intG_new(a, base);
    long sz = b->val.size;
    if (sz == 0) return toB_u8(0);
    unsigned long n = b->val.n[0];
    if (sz != 1 || n > UCHAR_MAX) {
        char errmsg[1024];
        snprintf(errmsg, sizeof(errmsg), "u8(): value %s out of range for type u8",get_str(&b->val));
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str(errmsg)));
    }
    return toB_u8(n);
}

B_NoneType B_u8D___init__(B_u8 self, B_atom a, B_int base){
    self->val = B_u8G_new(a,base)->val;
    return B_None;
}

void B_u8D___serialize__(B_u8 n, $Serial$state state) {
    $val_serialize(U8_ID,&n->val,state);
}

B_u8 B_u8D___deserialize__(B_u8 n, $Serial$state state) {
    return toB_u8((uint8_t)$val_deserialize(state));
}

B_bool B_u8D___bool__(B_u8 n) {
    return toB_bool(n->val != 0);
}

B_str B_u8D___str__(B_u8 n) {
    char *s;
    asprintf(&s,"%u",n->val);
    return to$str(s);
}

B_str B_u8D___repr__(B_u8 n) {
    char *s;
    asprintf(&s,"%u",n->val);
    return to$str(s);
}

B_u8 toB_u8(uint8_t i) {
    B_u8 res = acton_malloc(sizeof(struct B_u8));
    res->$class = &B_u8G_methods;
    res->val = i;
    return res;
}

uint8_t fromB_u8(B_u8 w) {
    return w->val;
}

                  

// B_IntegralD_u8 /////////////////////////////////////////////////////////////////////////

 
B_u8 B_IntegralD_u8D___add__(B_IntegralD_u8 wit,  B_u8 a, B_u8 b) {
    return toB_u8(a->val + b->val);
}  

B_u8 B_IntegralD_u8D___zero__(B_IntegralD_u8 wit) {
    return toB_u8(0);
}

B_complex B_IntegralD_u8D___complex__(B_IntegralD_u8 wit, B_u8 a) {
    return toB_complex((double)a->val);
}

B_u8 B_IntegralD_u8D___fromatom__(B_IntegralD_u8 wit, B_atom a) {
    return B_u8G_new(a,NULL);
}

B_u8 B_IntegralD_u8D___mul__(B_IntegralD_u8 wit,  B_u8 a, B_u8 b) {
    return toB_u8(a->val * b->val);
}  
  
B_u8 B_IntegralD_u8D___pow__(B_IntegralD_u8 wit,  B_u8 a, B_u8 b) {
    return toB_u8(u8_pow(a->val,b->val));
}

B_u8 B_IntegralD_u8D___neg__(B_IntegralD_u8 wit,  B_u8 a) {
    if (a->val > 0L)
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("u8.neg: cannot negate non-zero value in u8")));
    return a;
}

B_u8 B_IntegralD_u8D___pos__(B_IntegralD_u8 wit,  B_u8 a) {
    return a;
}

$WORD B_IntegralD_u8D_real(B_IntegralD_u8 wit, B_u8 a, B_Real wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)a);
}

$WORD B_IntegralD_u8D_imag(B_IntegralD_u8 wit, B_u8 a, B_Real wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)toB_u8(0L));
}

$WORD B_IntegralD_u8D___abs__(B_IntegralD_u8 wit, B_u8 a, B_Real wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)a);
}

B_u8 B_IntegralD_u8D_conjugate(B_IntegralD_u8 wit,  B_u8 a) {
    return a;
}

B_float B_IntegralD_u8D___float__ (B_IntegralD_u8 wit, B_u8 n) {
    return to$float((double)n->val);
}

$WORD B_IntegralD_u8D___trunc__ (B_IntegralD_u8 wit, B_u8 n, B_Integral wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)n);
}
  
$WORD B_IntegralD_u8D___floor__ (B_IntegralD_u8 wit, B_u8 n, B_Integral wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)n);
}
  
$WORD B_IntegralD_u8D___ceil__ (B_IntegralD_u8 wit, B_u8 n, B_Integral wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)n);
}
  
B_u8 B_IntegralD_u8D___round__ (B_IntegralD_u8 wit, B_u8 n, B_int p) {
    uint8_t nval = n->val;
    long pval = p==NULL ? 0 : from$int(p);
    if (pval>=0)
        return n;
    uint8_t p10 = u8_pow(10,-pval);
    uint8_t res = nval/p10;
    if (nval%p10 * 2 > p10)
        res++; 
    return toB_u8 (res * p10);
}
  
$WORD B_IntegralD_u8D_numerator (B_IntegralD_u8 wit, B_u8 n, B_Integral wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)n);
}
  
$WORD B_IntegralD_u8D_denominator (B_IntegralD_u8 wit, B_u8 n, B_Integral wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)toB_u8(1L));
}
  
B_int B_IntegralD_u8D___int__ (B_IntegralD_u8 wit, B_u8 n) {
    return B_intG_new((B_atom)n,NULL);
}

B_int B_IntegralD_u8D___index__(B_IntegralD_u8 wit, B_u8 n) {
    return B_intG_new((B_atom)n,NULL);
}

B_tuple B_IntegralD_u8D___divmod__(B_IntegralD_u8 wit, B_u8 a, B_u8 b) {
    int n = a->val;
    int d = b->val;
    return $NEWTUPLE(2, toB_u8(n/d), toB_u8(n%d));
}

B_u8 B_IntegralD_u8D___floordiv__(B_IntegralD_u8 wit, B_u8 a, B_u8 b) {
    if (b->val == 0)
        $RAISE((B_BaseException)$NEW(B_ZeroDivisionError, to$str("division by zero")));
    return toB_u8(a->val / b->val);
}

B_u8 B_IntegralD_u8D___mod__(B_IntegralD_u8 wit, B_u8 a, B_u8 b) {
    return toB_u8(a->val % b->val);
}

B_u8 B_IntegralD_u8D___lshift__(B_IntegralD_u8 wit,  B_u8 a, B_int b) {
    return toB_u8(a->val << from$int(b));
}

B_u8 B_IntegralD_u8D___rshift__(B_IntegralD_u8 wit,  B_u8 a, B_int b) {
    return toB_u8(a->val >> from$int(b));
}
 
B_u8 B_IntegralD_u8D___invert__(B_IntegralD_u8 wit,  B_u8 a) {
    return toB_u8(~a->val);
}


// B_LogicalD_IntegralD_u8  ////////////////////////////////////////////////////////////////////////////////////////

B_u8 B_LogicalD_IntegralD_u8D___and__(B_LogicalD_IntegralD_u8 wit,  B_u8 a, B_u8 b) {
    return toB_u8(a->val & b->val);
}
                                                 
B_u8 B_LogicalD_IntegralD_u8D___or__(B_LogicalD_IntegralD_u8 wit,  B_u8 a, B_u8 b) {
    return toB_u8(a->val | b->val);
}
                                                 
B_u8 B_LogicalD_IntegralD_u8D___xor__(B_LogicalD_IntegralD_u8 wit,  B_u8 a, B_u8 b) {
    return toB_u8(a->val ^ b->val);
}  
 
// B_MinusD_IntegralD_u8  ////////////////////////////////////////////////////////////////////////////////////////

 
B_u8 B_MinusD_IntegralD_u8D___sub__(B_MinusD_IntegralD_u8 wit,  B_u8 a, B_u8 b) {
    return toB_u8(a->val - b->val);
}  

// B_DivD_u8  ////////////////////////////////////////////////////////////////////////////////////////

 
B_float B_DivD_u8D___truediv__ (B_DivD_u8 wit, B_u8 a, B_u8 b) {
    if (b->val == 0)
        $RAISE((B_BaseException)$NEW(B_ZeroDivisionError, to$str("division by zero")));
    return to$float((double)a->val/(double)b->val);
}

// B_OrdD_u8  ////////////////////////////////////////////////////////////////////////////////////////

B_bool B_OrdD_u8D___eq__ (B_OrdD_u8 wit, B_u8 a, B_u8 b) {
    return toB_bool(a->val == b->val);
}

B_bool B_OrdD_u8D___ne__ (B_OrdD_u8 wit, B_u8 a, B_u8 b) {
    return toB_bool(a->val != b->val);
}

B_bool B_OrdD_u8D___lt__ (B_OrdD_u8 wit, B_u8 a, B_u8 b) {
    return toB_bool(a->val < b->val);
}

B_bool B_OrdD_u8D___le__ (B_OrdD_u8 wit, B_u8 a, B_u8 b) {
    return toB_bool(a->val <= b->val);
}

B_bool B_OrdD_u8D___gt__ (B_OrdD_u8 wit, B_u8 a, B_u8 b) {
    return toB_bool(a->val > b->val);
}

B_bool B_OrdD_u8D___ge__ (B_OrdD_u8 wit, B_u8 a, B_u8 b) {
    return toB_bool(a->val >= b->val);
}

// B_HashableD_u8 ///////////////////////////////////////////////////////////////////////////////////////////////////////

B_bool B_HashableD_u8D___eq__(B_HashableD_u8 wit, B_u8 a, B_u8 b) {
    return toB_bool(a->val == b->val);
}

B_bool B_HashableD_u8D___ne__(B_HashableD_u8 wit, B_u8 a, B_u8 b) {
    return toB_bool(a->val != b->val);
}

B_NoneType B_HashableD_u8D_hash(B_HashableD_u8 wit, B_u8 a, B_hasher h) {
    zig_hash_wyhash_update(h->_hasher, to$bytesD_len((char *)&(a->val),1));
    return B_None;
}
