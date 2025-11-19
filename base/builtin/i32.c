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
int i32_pow(int a, int e) {
    if (e == 0) return 1;
    if (e == 1) return a;
    if (e%2 == 0) return i32_pow(a*a,e/2);
    return a * i32_pow(a*a,e/2);
}

// General methods ///////////////////////////////////////////////////////////////////////

B_i32 B_i32G_new(B_atom a, B_int base) {
    B_int b = B_intG_new(a, base);
    unsigned long n = b->val.n[0];
    long sz = b->val.size;
    if (labs(sz) > 1 || (sz==1 && n > 0x7ffffffful) || sz == -1 && n > 0x80000000ul) {
        char errmsg[1024];
        snprintf(errmsg, sizeof(errmsg), "i32(): value %s out of range for type i32",get_str(&b->val));
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str(errmsg)));
    }
    return toB_i32((int)(n*sz));
}


B_NoneType B_i32D___init__(B_i32 self, B_atom a, B_int base){
    self->val = B_i32G_new(a,base)->val;
    return B_None;
}

void B_i32D___serialize__(B_i32 n, $Serial$state state) {
    $val_serialize(I32_ID,&n->val,state);
}

B_i32 B_i32D___deserialize__(B_i32 n, $Serial$state state) {
    return toB_i32((int)(uintptr_t)$val_deserialize(state));
}

B_bool B_i32D___bool__(B_i32 n) {
    return toB_bool(n->val != 0);
}

B_str B_i32D___str__(B_i32 n) {
    return $FORMAT("%d", n->val);
}

B_str B_i32D___repr__(B_i32 n) {
    return $FORMAT("%d", n->val);
}

B_i32 toB_i32(int i) {
    B_i32 res = acton_malloc(sizeof(struct B_i32));
    res->$class = &B_i32G_methods;
    res->val = i;
    return res;
}

int fromB_i32(B_i32 w) {
    return w->val;
}

                  

// B_IntegralD_i32 /////////////////////////////////////////////////////////////////////////

 
B_i32 B_IntegralD_i32D___add__(B_IntegralD_i32 wit,  B_i32 a, B_i32 b) {
    return toB_i32(a->val + b->val);
}  

B_i32 B_IntegralD_i32D___zero__(B_IntegralD_i32 wit) {
    return toB_i32(0);
}

B_complex B_IntegralD_i32D___complex__(B_IntegralD_i32 wit, B_i32 a) {
    return toB_complex((double)a->val);
}

B_i32 B_IntegralD_i32D___fromatom__(B_IntegralD_i32 wit, B_atom a) {
    return B_i32G_new(a,NULL);
}

B_i32 B_IntegralD_i32D___mul__(B_IntegralD_i32 wit,  B_i32 a, B_i32 b) {
    return toB_i32(a->val * b->val);
}  
  
B_i32 B_IntegralD_i32D___pow__(B_IntegralD_i32 wit,  B_i32 a, B_i32 b) {
    if ( b->val < 0) {
        // raise VALUEERROR;
        return NULL;
    }
    return toB_i32(i32_pow(a->val,b->val));
}

B_i32 B_IntegralD_i32D___neg__(B_IntegralD_i32 wit,  B_i32 a) {
    return toB_i32(-a->val);
}

B_i32 B_IntegralD_i32D___pos__(B_IntegralD_i32 wit,  B_i32 a) {
    return a;
}

$WORD B_IntegralD_i32D_real(B_IntegralD_i32 wit, B_i32 a, B_Real wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)a);
}

$WORD B_IntegralD_i32D_imag(B_IntegralD_i32 wit, B_i32 a, B_Real wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)toB_i32(0L));
}

$WORD B_IntegralD_i32D___abs__(B_IntegralD_i32 wit, B_i32 a, B_Real wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)toB_i32(labs(a->val)));
}

B_i32 B_IntegralD_i32D_conjugate(B_IntegralD_i32 wit,  B_i32 a) {
    return a;
}

B_float B_IntegralD_i32D___float__ (B_IntegralD_i32 wit, B_i32 n) {
    return to$float((double)n->val);
}

$WORD B_IntegralD_i32D___trunc__ (B_IntegralD_i32 wit, B_i32 n, B_Integral wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)n);
}
  
$WORD B_IntegralD_i32D___floor__ (B_IntegralD_i32 wit, B_i32 n, B_Integral wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)n);
}
  
$WORD B_IntegralD_i32D___ceil__ (B_IntegralD_i32 wit, B_i32 n, B_Integral wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)n);
}
  
B_i32 B_IntegralD_i32D___round__ (B_IntegralD_i32 wit, B_i32 n, B_int p) {
    int nval = n->val;
    if (nval<0)
        return toB_i32(-B_IntegralD_i32D___round__(wit,toB_i32(-nval),p)->val);
    int pval = p==NULL ? 0 : from$int(p);
    if (pval>=0)
        return n;
    int p10 = i32_pow(10,-pval);
    int res = nval/p10;
    if (nval%p10 * 2 > p10)
        res++; 
    return toB_i32 (res * p10);
}
  
$WORD B_IntegralD_i32D_numerator (B_IntegralD_i32 wit, B_i32 n, B_Integral wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)n);
}
  
$WORD B_IntegralD_i32D_denominator (B_IntegralD_i32 wit, B_i32 n, B_Integral wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)toB_i32(1L));
}
  
B_int B_IntegralD_i32D___int__ (B_IntegralD_i32 wit, B_i32 n) {
    return B_intG_new((B_atom)n,NULL);
}

B_int B_IntegralD_i32D___index__(B_IntegralD_i32 wit, B_i32 n) {
    return B_intG_new((B_atom)n,NULL);
}

B_tuple B_IntegralD_i32D___divmod__(B_IntegralD_i32 wit, B_i32 a, B_i32 b) {
    int n = a->val;
    int d = b->val;
    return $NEWTUPLE(2, toB_i32(n/d), toB_i32(n%d));
}

B_i32 B_IntegralD_i32D___floordiv__(B_IntegralD_i32 wit, B_i32 a, B_i32 b) {
    if (b->val == 0)
        $RAISE((B_BaseException)$NEW(B_ZeroDivisionError, to$str("division by zero")));
    return toB_i32(a->val / b->val);
}

B_i32 B_IntegralD_i32D___mod__(B_IntegralD_i32 wit, B_i32 a, B_i32 b) {
    return toB_i32(a->val % b->val);
}

B_i32 B_IntegralD_i32D___lshift__(B_IntegralD_i32 wit,  B_i32 a, B_int b) {
    return toB_i32(a->val << from$int(b));
}

B_i32 B_IntegralD_i32D___rshift__(B_IntegralD_i32 wit,  B_i32 a, B_int b) {
    return toB_i32(a->val >> from$int(b));
}
 
B_i32 B_IntegralD_i32D___invert__(B_IntegralD_i32 wit,  B_i32 a) {
    return toB_i32(~a->val);
}


// B_LogicalD_IntegralD_i32  ////////////////////////////////////////////////////////////////////////////////////////

B_i32 B_LogicalD_IntegralD_i32D___and__(B_LogicalD_IntegralD_i32 wit,  B_i32 a, B_i32 b) {
    return toB_i32(a->val & b->val);
}
                                                 
B_i32 B_LogicalD_IntegralD_i32D___or__(B_LogicalD_IntegralD_i32 wit,  B_i32 a, B_i32 b) {
    return toB_i32(a->val | b->val);
}
                                                 
B_i32 B_LogicalD_IntegralD_i32D___xor__(B_LogicalD_IntegralD_i32 wit,  B_i32 a, B_i32 b) {
    return toB_i32(a->val ^ b->val);
}  
 
// B_MinusD_IntegralD_i32  ////////////////////////////////////////////////////////////////////////////////////////

 
B_i32 B_MinusD_IntegralD_i32D___sub__(B_MinusD_IntegralD_i32 wit,  B_i32 a, B_i32 b) {
    return toB_i32(a->val - b->val);
}  

// B_DivD_i32  ////////////////////////////////////////////////////////////////////////////////////////

 
B_float B_DivD_i32D___truediv__ (B_DivD_i32 wit, B_i32 a, B_i32 b) {
    if (b->val == 0)
        $RAISE((B_BaseException)$NEW(B_ZeroDivisionError, to$str("division by zero")));
    return to$float((double)a->val/(double)b->val);
}

// B_OrdD_i32  ////////////////////////////////////////////////////////////////////////////////////////

B_bool B_OrdD_i32D___eq__ (B_OrdD_i32 wit, B_i32 a, B_i32 b) {
    return toB_bool(a->val == b->val);
}

B_bool B_OrdD_i32D___ne__ (B_OrdD_i32 wit, B_i32 a, B_i32 b) {
    return toB_bool(a->val != b->val);
}

B_bool B_OrdD_i32D___lt__ (B_OrdD_i32 wit, B_i32 a, B_i32 b) {
    return toB_bool(a->val < b->val);
}

B_bool B_OrdD_i32D___le__ (B_OrdD_i32 wit, B_i32 a, B_i32 b) {
    return toB_bool(a->val <= b->val);
}

B_bool B_OrdD_i32D___gt__ (B_OrdD_i32 wit, B_i32 a, B_i32 b) {
    return toB_bool(a->val > b->val);
}

B_bool B_OrdD_i32D___ge__ (B_OrdD_i32 wit, B_i32 a, B_i32 b) {
    return toB_bool(a->val >= b->val);
}

// B_HashableD_i32 ///////////////////////////////////////////////////////////////////////////////////////////////////////

B_bool B_HashableD_i32D___eq__(B_HashableD_i32 wit, B_i32 a, B_i32 b) {
    return toB_bool(a->val == b->val);
}

B_bool B_HashableD_i32D___ne__(B_HashableD_i32 wit, B_i32 a, B_i32 b) {
    return toB_bool(a->val != b->val);
}

B_NoneType B_HashableD_i32D_hash(B_HashableD_i32 wit, B_i32 a, B_hasher h) {
    zig_hash_wyhash_update(h->_hasher, to$bytesD_len((char *)&(a->val), 4));
    return B_None;
}
