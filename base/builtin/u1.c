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

#define U1_NORM(a)        ((uint8_t)((a) & 1))
#define U1_ADD(a,b)       U1_NORM((a) + (b))
#define U1_SUB(a,b)       U1_NORM((a) - (b))
#define U1_MUL(a,b)       U1_NORM((a) * (b))
#define U1_NEG(a)         U1_NORM(-(a))
#define U1_INVERT(a)      U1_NORM(~(a))
#define U1_AND(a,b)       U1_NORM((a) & (b))
#define U1_OR(a,b)        U1_NORM((a) | (b))
#define U1_XOR(a,b)       U1_NORM((a) ^ (b))
#define U1_LSHIFT(a,b)    U1_NORM((a) << (b))
#define U1_RSHIFT(a,b)    U1_NORM((a) >> (b))
#define U1_DIV(a,b)       ({ uint8_t _u1_b = (b); if (_u1_b == 0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError, to$str("division by zero"))); (double)(a)/(double)_u1_b; })
#define U1_FLOORDIV(a,b)  ({ uint8_t _u1_b = (b); if (_u1_b == 0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError, to$str("division by zero"))); U1_NORM((a) / _u1_b); })
#define U1_MOD(a,b)       ({ uint8_t _u1_b = (b); if (_u1_b == 0) $RAISE((B_BaseException)$NEW(B_ZeroDivisionError, to$str("division by zero"))); U1_NORM((a) % _u1_b); })

// only called with e>=0.
uint8_t u1_pow(uint8_t a, uint8_t e) {
    if (e == 0) return 1;
    if (e == 1) return a;
    if (e%2 == 0) return U1_NORM(int_pow(a*a,e/2));
    return U1_NORM(a * int_pow(a*a,e/2));
}

// General methods
///////////////////////////////////////////////////////////////////////

uint8_t B_u1G_new(B_atom a, B_int base) {  // base is optional
    B_bigint b = B_bigintG_new(a, base);
    unsigned long n = b->val.n[0];
    int sz = b->val.size;
    if (sz  > 1 || sz < 0 || (sz==1 && n > 1)) {
        char errmsg[1024];
        snprintf(errmsg, sizeof(errmsg), "u1(): value %s out of range for type u1",get_str(&b->val));
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str(errmsg)));
    }
    return (uint8_t)(n*sz);
}
 
B_NoneType B_u1D___init__(B_u1 self, B_atom a, B_int base){
    self->val = B_u1G_new(a,base);
    return B_None;
}

void B_u1D___serialize__(B_u1 n, $Serial$state state) {
    $val_serialize(INT_ID,&n->val,state);
}

B_u1 B_u1D___deserialize__(B_u1 n, $Serial$state state) {
    return toB_u1((long)$val_deserialize(state));
}

B_bool B_u1D___bool__(B_u1 n) {
    return toB_bool(n->val != 0);
}

B_str B_u1D___str__(B_u1 n) {
    return $FORMAT("%lld", n->val);
}

B_str B_u1D___repr__(B_u1 n) {
    return $FORMAT("%lld", n->val);
}

B_u1 toB_u1(uint8_t i) {
    B_u1 res = acton_malloc(sizeof(struct B_u1));
    res->$class = &B_u1G_methods;
    res->val = U1_NORM(i);
    return res;
}

uint8_t fromB_u1(B_u1 w) {
    return w->val;
}



// B_IntegralD_u1 /////////////////////////////////////////////////////////////////////////


B_u1 B_IntegralD_u1D___add__(B_IntegralD_u1 wit,  B_u1 a, B_u1 b) {
    return toB_u1(U1_ADD(a->val, b->val)); 
}

B_u1 B_IntegralD_u1D___zero__(B_IntegralD_u1 wit) {
    return toB_u1(0);
}

B_complex B_IntegralD_u1D___complex__(B_IntegralD_u1 wit, B_u1 a) {
    return toB_complex((double)(a->val));
}

B_u1 B_IntegralD_u1D___fromatom__(B_IntegralD_u1 wit, B_atom a) {
    return toB_u1(B_u1G_new(a,NULL));
}

B_u1 B_IntegralD_u1D___mul__(B_IntegralD_u1 wit,  B_u1 a, B_u1 b) {
    return toB_u1(U1_MUL(a->val, b->val));
}  
  
B_u1 B_IntegralD_u1D___pow__(B_IntegralD_u1 wit,  B_u1 a, B_u1 b) {
    uint8_t aval = a->val;
    uint8_t bval = b->val;
    return toB_u1(int_pow(aval,bval));
}

B_u1 B_IntegralD_u1D___neg__(B_IntegralD_u1 wit,  B_u1 a) {
    return toB_u1(U1_NEG(a->val));
}

B_u1 B_IntegralD_u1D___pos__(B_IntegralD_u1 wit,  B_u1 a) {
    return a;
}

$WORD B_IntegralD_u1D_real(B_IntegralD_u1 wit, B_u1 a, B_Real wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)(a));
}

$WORD B_IntegralD_u1D_imag(B_IntegralD_u1 wit, B_u1 a, B_Real wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)toB_u1(0LL));
}

$WORD B_IntegralD_u1D___abs__(B_IntegralD_u1 wit, B_u1 a, B_Real wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)toB_u1(a->val));
}

B_u1 B_IntegralD_u1D_conjugate(B_IntegralD_u1 wit,  B_u1 a) {
    return a;
}

double B_IntegralD_u1D___float__ (B_IntegralD_u1 wit, B_u1 n) {
    return (double)n->val;
}

$WORD B_IntegralD_u1D___trunc__ (B_IntegralD_u1 wit, B_u1 n, B_Integral wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)n);
}
  
$WORD B_IntegralD_u1D___floor__ (B_IntegralD_u1 wit, B_u1 n, B_Integral wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)n);
}
  
$WORD B_IntegralD_u1D___ceil__ (B_IntegralD_u1 wit, B_u1 n, B_Integral wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)n);
}
  
B_u1 B_IntegralD_u1D___round__ (B_IntegralD_u1 wit, B_u1 n, B_int p) {
    uint8_t nval = n->val;
    uint8_t pval = p==NULL ? 0 : fromB_int(p);
    if (pval>=0)
        return n;
    uint8_t p10 = int_pow(10,-pval);
    uint8_t res = nval/p10;
    if (nval%p10 * 2 > p10)
        res++; 
    return toB_u1(res * p10);
}
  
$WORD B_IntegralD_u1D_numerator (B_IntegralD_u1 wit, B_u1 n, B_Integral wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)n);
}
  
$WORD B_IntegralD_u1D_denominator (B_IntegralD_u1 wit, B_u1 n, B_Integral wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)toB_u1(1LL));
}
  
int64_t B_IntegralD_u1D___int__ (B_IntegralD_u1 wit, B_u1 n) {
    return n->val;
}

int64_t B_IntegralD_u1D___index__(B_IntegralD_u1 wit, B_u1 n) {
    return n->val;
}

B_tuple B_IntegralD_u1D___divmod__(B_IntegralD_u1 wit, B_u1 a, B_u1 b) {
    uint8_t n = a->val;
    uint8_t d = b->val;
    return $NEWTUPLE(2, toB_u1(U1_FLOORDIV(n, d)), toB_u1(U1_MOD(n, d)));
}

B_u1 B_IntegralD_u1D___floordiv__(B_IntegralD_u1 wit, B_u1 a, B_u1 b) {
    return toB_u1(U1_FLOORDIV(a->val, b->val));
}

B_u1 B_IntegralD_u1D___mod__(B_IntegralD_u1 wit, B_u1 a, B_u1 b) {
    return toB_u1(U1_MOD(a->val, b->val));
}

B_u1 B_IntegralD_u1D___lshift__(B_IntegralD_u1 wit,  B_u1 a, int64_t b) {
    return toB_u1(U1_LSHIFT(a->val, b));
}

B_u1 B_IntegralD_u1D___rshift__(B_IntegralD_u1 wit,  B_u1 a, int64_t b) {
    return toB_u1(U1_RSHIFT(a->val, b));
}
 
B_u1 B_IntegralD_u1D___invert__(B_IntegralD_u1 wit,  B_u1 a) {
    return toB_u1(U1_INVERT(a->val));
}


// B_LogicalD_IntegralD_u1  ////////////////////////////////////////////////////////////////////////////////////////

B_u1 B_LogicalD_IntegralD_u1D___and__(B_LogicalD_IntegralD_u1 wit,  B_u1 a, B_u1 b) {
    return toB_u1(U1_AND(a->val, b->val));
}
                                                 
B_u1 B_LogicalD_IntegralD_u1D___or__(B_LogicalD_IntegralD_u1 wit,  B_u1 a, B_u1 b) {
    return toB_u1(U1_OR(a->val, b->val));
}
                                                 
B_u1 B_LogicalD_IntegralD_u1D___xor__(B_LogicalD_IntegralD_u1 wit,  B_u1 a, B_u1 b) {
    return toB_u1(U1_XOR(a->val, b->val));
}  
 
// B_MinusD_IntegralD_u1  ////////////////////////////////////////////////////////////////////////////////////////

 
B_u1 B_MinusD_IntegralD_u1D___sub__(B_MinusD_IntegralD_u1 wit,  B_u1 a, B_u1 b) {
    return toB_u1(U1_SUB(a->val, b->val));
}  

// B_DivD_u1  ////////////////////////////////////////////////////////////////////////////////////////

 
B_float B_DivD_u1D___truediv__ (B_DivD_u1 wit, B_u1 a, B_u1 b) {
    return toB_float(U1_DIV(a->val, b->val));
}

// B_OrdD_u1  ////////////////////////////////////////////////////////////////////////////////////////

B_bool B_OrdD_u1D___eq__ (B_OrdD_u1 wit, B_u1 a, B_u1 b) {
    return toB_bool(a->val == b->val);
}

B_bool B_OrdD_u1D___ne__ (B_OrdD_u1 wit, B_u1 a, B_u1 b) {
    return toB_bool(a->val != b->val);
}

B_bool B_OrdD_u1D___lt__ (B_OrdD_u1 wit, B_u1 a, B_u1 b) {
    return toB_bool(a->val < b->val);
}

B_bool B_OrdD_u1D___le__ (B_OrdD_u1 wit, B_u1 a, B_u1 b) {
    return toB_bool(a->val <= b->val);
}

B_bool B_OrdD_u1D___gt__ (B_OrdD_u1 wit, B_u1 a, B_u1 b) {
    return toB_bool(a->val > b->val);
}

B_bool B_OrdD_u1D___ge__ (B_OrdD_u1 wit, B_u1 a, B_u1 b) {
    return toB_bool(a->val >= b->val);
}

// B_HashableD_u1 ///////////////////////////////////////////////////////////////////////////////////////////////////////

B_bool B_HashableD_u1D___eq__(B_HashableD_u1 wit, B_u1 a, B_u1 b) {
    return toB_bool(a->val == b->val);
}

B_bool B_HashableD_u1D___ne__(B_HashableD_u1 wit, B_u1 a, B_u1 b) {
    return toB_bool(a->val != b->val);
}

B_NoneType B_HashableD_u1D_hash(B_HashableD_u1 wit, B_u1 a, B_hasher h) {
    zig_hash_wyhash_update(h->_hasher,to$bytesD_len((char *)&(a->val),8));
    return B_None;
}
