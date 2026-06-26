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
long int_pow(long a, long e) {
    if (e == 0) return 1;
    if (e == 1) return a;
    if (e%2 == 0) return int_pow(a*a,e/2);
    return a * int_pow(a*a,e/2);
}

// General methods ///////////////////////////////////////////////////////////////////////

int64_t B_intG_new(B_atom a, B_int base) {  // base is optional
    B_bigint b = B_bigintG_new(a, base);
    unsigned long n = b->val.n[0];
    int sz = b->val.size;
    if (labs(sz) > 1 || (sz==1 && n > 0x7ffffffffffffffful) || sz == -1 && n > 0x8000000000000000ul) {
        char errmsg[1024];
        snprintf(errmsg, sizeof(errmsg), "int(): value %s out of range for type int",get_str(&b->val));
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str(errmsg)));
    }
    return n*sz;
}
 
B_NoneType B_intD___init__(B_int self, B_atom a, B_int base){
    self->val = B_intG_new(a,base);
    return B_None;
}

void B_intD___serialize__(B_int n, $Serial$state state) {
    $val_serialize(INT_ID,&n->val,state);
}

B_int B_intD___deserialize__(B_int n, $Serial$state state) {
    return toB_int((long)$val_deserialize(state));
}

bool B_intD___bool__(B_int n) {
    return n->val != 0;
}

B_str B_intD___str__(B_int n) {
    return $FORMAT("%lld", n->val);
}

B_str B_intD___repr__(B_int n) {
    return $FORMAT("%lld", n->val);
}

B_int toB_int(int64_t i) {
    B_int res = acton_malloc(sizeof(struct B_int));
    res->$class = &B_intG_methods;
    res->val = i;
    return res;
}

B_int to$int(int64_t n) {
    return toB_int(n);
}

int64_t fromB_int(B_int w) {
    return w->val;
}



// B_IntegralD_int /////////////////////////////////////////////////////////////////////////


B_int B_IntegralD_intD___add__(B_IntegralD_int wit,  B_int a, B_int b) {
    return toB_int(a->val + b->val); 
}

B_int B_IntegralD_intD___zero__(B_IntegralD_int wit) {
    return toB_int(0);
}

B_complex B_IntegralD_intD___complex__(B_IntegralD_int wit, B_int a) {
    return toB_complex((double)(a->val));
}

B_int B_IntegralD_intD___fromatom__(B_IntegralD_int wit, B_atom a) {
    return toB_int(B_intG_new(a,NULL));
}

B_int B_IntegralD_intD___mul__(B_IntegralD_int wit,  B_int a, B_int b) {
    return toB_int(a->val * b->val);
}  
  
B_int B_IntegralD_intD___pow__(B_IntegralD_int wit,  B_int a, B_int b) {
    int64_t aval = a->val;
    int64_t bval = b->val;
    if ( bval < 0) {
        char errmsg[1024];
        snprintf(errmsg, sizeof(errmsg), "int.__pow__: negative exponent %ld ",(long)bval);
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str(errmsg)));
    }
    return toB_int(int_pow(aval,bval));
}

B_int B_IntegralD_intD___neg__(B_IntegralD_int wit,  B_int a) {
    return toB_int(-a->val);
}

B_int B_IntegralD_intD___pos__(B_IntegralD_int wit,  B_int a) {
    return a;
}

$WORD B_IntegralD_intD_real(B_IntegralD_int wit, B_int a, B_Real wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)(a));
}

$WORD B_IntegralD_intD_imag(B_IntegralD_int wit, B_int a, B_Real wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)toB_int(0LL));
}

$WORD B_IntegralD_intD___abs__(B_IntegralD_int wit, B_int a, B_Real wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)toB_int(labs(a->val)));
}

B_int B_IntegralD_intD_conjugate(B_IntegralD_int wit,  B_int a) {
    return a;
}

double B_IntegralD_intD___float__ (B_IntegralD_int wit, B_int n) {
    return (double)n->val;
}

$WORD B_IntegralD_intD___trunc__ (B_IntegralD_int wit, B_int n, B_Integral wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)n);
}
  
$WORD B_IntegralD_intD___floor__ (B_IntegralD_int wit, B_int n, B_Integral wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)n);
}
  
$WORD B_IntegralD_intD___ceil__ (B_IntegralD_int wit, B_int n, B_Integral wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)n);
}
  
B_int B_IntegralD_intD___round__ (B_IntegralD_int wit, B_int n, B_int p) {
    int64_t nval = n->val;
    if (nval<0)
        return  toB_int(-B_IntegralD_intD___round__(wit,toB_int(-nval),p)->val);
    int64_t pval = p==NULL ? 0 : fromB_int(p);
    if (pval>=0)
        return n;
    int64_t p10 = int_pow(10,-pval);
    int64_t res = nval/p10;
    if (nval%p10 * 2 > p10)
        res++; 
    return toB_int(res * p10);
}
  
$WORD B_IntegralD_intD_numerator (B_IntegralD_int wit, B_int n, B_Integral wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)n);
}
  
$WORD B_IntegralD_intD_denominator (B_IntegralD_int wit, B_int n, B_Integral wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)toB_int(1LL));
}
  
int64_t B_IntegralD_intD___int__ (B_IntegralD_int wit, B_int n) {
    return n->val;
}

int64_t B_IntegralD_intD___index__(B_IntegralD_int wit, B_int n) {
    return n->val;
}

B_tuple B_IntegralD_intD___divmod__(B_IntegralD_int wit, B_int a, B_int b) {
    int64_t n = a->val;
    int64_t d = b->val;
    return $NEWTUPLE(2, toB_int(n/d), toB_int(n%d));
}

B_int B_IntegralD_intD___floordiv__(B_IntegralD_int wit, B_int a, B_int b) {
    if (b->val == 0)
        $RAISE((B_BaseException)$NEW(B_ZeroDivisionError, to$str("division by zero")));
    return toB_int(a->val / b->val);
}

B_int B_IntegralD_intD___mod__(B_IntegralD_int wit, B_int a, B_int b) {
    return toB_int(a->val % b->val);
}

B_int B_IntegralD_intD___lshift__(B_IntegralD_int wit,  B_int a, int64_t b) {
    return toB_int(a->val << b);
}

B_int B_IntegralD_intD___rshift__(B_IntegralD_int wit,  B_int a, int64_t b) {
    return toB_int(a->val >> b);
}
 
B_int B_IntegralD_intD___invert__(B_IntegralD_int wit,  B_int a) {
    return toB_int(~a->val);
}


// B_LogicalD_IntegralD_int  ////////////////////////////////////////////////////////////////////////////////////////

B_int B_LogicalD_IntegralD_intD___and__(B_LogicalD_IntegralD_int wit,  B_int a, B_int b) {
    return toB_int(a->val & b->val);
}
                                                 
B_int B_LogicalD_IntegralD_intD___or__(B_LogicalD_IntegralD_int wit,  B_int a, B_int b) {
    return toB_int(a->val | b->val);
}
                                                 
B_int B_LogicalD_IntegralD_intD___xor__(B_LogicalD_IntegralD_int wit,  B_int a, B_int b) {
    return toB_int(a->val ^ b->val);
}  
 
// B_MinusD_IntegralD_int  ////////////////////////////////////////////////////////////////////////////////////////

 
B_int B_MinusD_IntegralD_intD___sub__(B_MinusD_IntegralD_int wit,  B_int a, B_int b) {
    return toB_int(a->val - b->val);
}  

// B_DivD_int  ////////////////////////////////////////////////////////////////////////////////////////

 
B_float B_DivD_intD___truediv__ (B_DivD_int wit, B_int a, B_int b) {
    if (b->val == 0)
        $RAISE((B_BaseException)$NEW(B_ZeroDivisionError, to$str("division by zero")));
    return toB_float((double)a->val/(double)b->val);
}

// B_OrdD_int  ////////////////////////////////////////////////////////////////////////////////////////

bool B_OrdD_intD___eq__ (B_OrdD_int wit, B_int a, B_int b) {
    return a->val == b->val;
}

bool B_OrdD_intD___ne__ (B_OrdD_int wit, B_int a, B_int b) {
    return a->val != b->val;
}

bool B_OrdD_intD___lt__ (B_OrdD_int wit, B_int a, B_int b) {
    return a->val < b->val;
}

bool B_OrdD_intD___le__ (B_OrdD_int wit, B_int a, B_int b) {
    return a->val <= b->val;
}

bool B_OrdD_intD___gt__ (B_OrdD_int wit, B_int a, B_int b) {
    return a->val > b->val;
}

bool B_OrdD_intD___ge__ (B_OrdD_int wit, B_int a, B_int b) {
    return a->val >= b->val;
}

// B_HashableD_int ///////////////////////////////////////////////////////////////////////////////////////////////////////

bool B_HashableD_intD___eq__(B_HashableD_int wit, B_int a, B_int b) {
    return a->val == b->val;
}

bool B_HashableD_intD___ne__(B_HashableD_int wit, B_int a, B_int b) {
    return a->val != b->val;
}

B_NoneType B_HashableD_intD_hash(B_HashableD_int wit, B_int a, B_hasher h) {
    zig_hash_wyhash_update(h->_hasher,to$bytesD_len((char *)&(a->val),8));
    return B_None;
}
