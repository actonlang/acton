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
short i16_pow(short a, short e) {
    if (e == 0) return 1;
    if (e == 1) return a;
    if (e%2 == 0) return i16_pow(a*a,e/2);
    return a * i16_pow(a*a,e/2);
}

// General methods ///////////////////////////////////////////////////////////////////////

B_i16 B_i16G_new(B_atom a, B_int base) {
    B_int b = B_intG_new(a, base);
    unsigned long n = b->val.n[0];
    long sz = b->val.size;
    if (labs(sz) > 1 || (sz==1 && n > 0x7ffful) || sz == -1 && n > 0x8000ul) {
        char errmsg[1024];
        snprintf(errmsg, sizeof(errmsg), "i16(): value %s out of range for type i16",get_str(&b->val));
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str(errmsg)));
    }
    return toB_i16((short)(n*sz));
}

B_NoneType B_i16D___init__(B_i16 self, B_atom a, B_int base){
    self->val = B_i16G_new(a,base)->val;
    return B_None;
}

void B_i16D___serialize__(B_i16 n, $Serial$state state) {
    $val_serialize(INT_ID,&n->val,state);
}

B_i16 B_i16D___deserialize__(B_i16 n, $Serial$state state) {
    return toB_i16((short)(uintptr_t)$val_deserialize(state));
}

B_bool B_i16D___bool__(B_i16 n) {
    return toB_bool(n->val != 0);
}

B_str B_i16D___str__(B_i16 n) {
    return $FORMAT("%hd", n->val);
}

B_str B_i16D___repr__(B_i16 n) {
    return $FORMAT("%hd", n->val);
}

B_i16 toB_i16(short i) {
    B_i16 res = acton_malloc(sizeof(struct B_i16));
    res->$class = &B_i16G_methods;
    res->val = i;
    return res;
}

short fromB_i16(B_i16 w) {
    return w->val;
}

                  

// B_IntegralD_i16 /////////////////////////////////////////////////////////////////////////

 
B_i16 B_IntegralD_i16D___add__(B_IntegralD_i16 wit,  B_i16 a, B_i16 b) {
    return toB_i16(a->val + b->val);
}  

B_i16 B_IntegralD_i16D___zero__(B_IntegralD_i16 wit) {
    return toB_i16(0);
}

B_complex B_IntegralD_i16D___complex__(B_IntegralD_i16 wit, B_i16 a) {
    return toB_complex((double)a->val);
}

B_i16 B_IntegralD_i16D___fromatom__(B_IntegralD_i16 wit, B_atom a) {
    return B_i16G_new(a,NULL);
}

B_i16 B_IntegralD_i16D___mul__(B_IntegralD_i16 wit,  B_i16 a, B_i16 b) {
    return toB_i16(a->val * b->val);
}  
  
B_i16 B_IntegralD_i16D___pow__(B_IntegralD_i16 wit,  B_i16 a, B_i16 b) {
    if ( b->val < 0) {
        // raise VALUEERROR;
        return NULL;
    }
    return toB_i16(i16_pow(a->val,b->val));
}

B_i16 B_IntegralD_i16D___neg__(B_IntegralD_i16 wit,  B_i16 a) {
    return toB_i16(-a->val);
}

B_i16 B_IntegralD_i16D___pos__(B_IntegralD_i16 wit,  B_i16 a) {
    return a;
}

$WORD B_IntegralD_i16D_real(B_IntegralD_i16 wit, B_i16 a, B_Real wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)a);
}

$WORD B_IntegralD_i16D_imag(B_IntegralD_i16 wit, B_i16 a, B_Real wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)toB_i16(0L));
}

$WORD B_IntegralD_i16D___abs__(B_IntegralD_i16 wit, B_i16 a, B_Real wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)toB_i16(labs(a->val)));
}

B_i16 B_IntegralD_i16D_conjugate(B_IntegralD_i16 wit,  B_i16 a) {
    return a;
}

B_float B_IntegralD_i16D___float__ (B_IntegralD_i16 wit, B_i16 n) {
    return to$float((double)n->val);
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
    short nval = n->val;
    if (nval<0)
        return toB_i16(-B_IntegralD_i16D___round__(wit,toB_i16(-nval),p)->val);
    short pval = p==NULL ? 0 : from$int(p);
    if (pval>=0)
        return n;
    short p10 = i16_pow(10,-pval);
    short res = nval/p10;
    if (nval%p10 * 2 > p10)
        res++; 
    return toB_i16 (res * p10);
}
  
$WORD B_IntegralD_i16D_numerator (B_IntegralD_i16 wit, B_i16 n, B_Integral wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)n);
}
  
$WORD B_IntegralD_i16D_denominator (B_IntegralD_i16 wit, B_i16 n, B_Integral wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)toB_i16(1L));
}
  
B_int B_IntegralD_i16D___int__ (B_IntegralD_i16 wit, B_i16 n) {
    return B_intG_new((B_atom)n,NULL);
}

B_int B_IntegralD_i16D___index__(B_IntegralD_i16 wit, B_i16 n) {
    return B_intG_new((B_atom)n,NULL);
}

B_tuple B_IntegralD_i16D___divmod__(B_IntegralD_i16 wit, B_i16 a, B_i16 b) {
    if (b->val == 0)
        $RAISE((B_BaseException)$NEW(B_ZeroDivisionError, to$str("division by zero")));
    short n = a->val;
    short d = b->val;
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

B_i16 B_IntegralD_i16D___lshift__(B_IntegralD_i16 wit,  B_i16 a, B_int b) {
    return toB_i16(a->val << from$int(b));
}

B_i16 B_IntegralD_i16D___rshift__(B_IntegralD_i16 wit,  B_i16 a, B_int b) {
    return toB_i16(a->val >> from$int(b));
}
 
B_i16 B_IntegralD_i16D___invert__(B_IntegralD_i16 wit,  B_i16 a) {
    return toB_i16(~a->val);
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
    return to$float((double)a->val/(double)b->val);
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

B_int B_HashableD_i16D___hash__(B_HashableD_i16 wit, B_i16 a) {
    return to$int(B_i16D_hash(a));
}
