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
unsigned long ulongpow(unsigned long a, unsigned long e) {
    if (e == 0) return 1;
    if (e == 1) return a;
    if (e%2 == 0) return ulongpow(a*a,e/2);
    return a * ulongpow(a*a,e/2);
}

// General methods ///////////////////////////////////////////////////////////////////////

B_u64 B_u64G_new(B_atom a) {
    if ($ISINSTANCE(a,B_int)->val){
        zz_struct n = ((B_int)a)-> val;
        if (n.size < 0 || n.size > 1) {
            $RAISE((B_BaseException)$NEW(B_ValueError,to$str("u64(): int argument out of range")));
        }
        return toB_u64(n.size*n.n[0]);
    }
    if ($ISINSTANCE(a,B_i64)->val) {
        long x = ((B_i64)a)->val;
        if (x < 0) 
            $RAISE((B_BaseException)$NEW(B_ValueError,to$str("u64(): i64 argument out of range")));
        return toB_u64((unsigned long)x);
    }
    if ($ISINSTANCE(a,B_i32)->val) {
        int x = ((B_i32)a)->val;
        if (x < 0) 
            $RAISE((B_BaseException)$NEW(B_ValueError,to$str("u64(): i32 argument out of range")));
        return toB_u64((unsigned long)x);
    }
    if ($ISINSTANCE(a,B_i16)->val) {
        short x = ((B_i16)a)->val;
        if (x < 0) 
            $RAISE((B_BaseException)$NEW(B_ValueError,to$str("u64(): i16 argument out of range")));
        return toB_u64((unsigned long)x);
    }
    if ($ISINSTANCE(a,B_u64)->val) return (B_u64)a;
    if ($ISINSTANCE(a,B_u32)->val) return toB_u64((unsigned long)((B_u32)a)->val);
    if ($ISINSTANCE(a,B_u16)->val) return toB_u64((unsigned long)((B_u16)a)->val);
    if ($ISINSTANCE(a,B_float)->val) {
        long x = round(((B_float)a)->val);
        if (x<0)
           $RAISE((B_BaseException)$NEW(B_ValueError,to$str("u64(): negative float argument")));
        else
            return toB_u64((unsigned long)x);
    }
    if ($ISINSTANCE(a,B_bool)->val) return toB_u64((unsigned long)((B_bool)a)->val);
    if ($ISINSTANCE(a,B_str)->val) {
        unsigned long x;
        int c;
        sscanf((char *)((B_str)a)->str,"%lu%n",&x,&c);
        if (c==((B_str)a)->nbytes)
            return toB_u64(x);
        else 
            $RAISE((B_BaseException)$NEW(B_ValueError,to$str("u64(): invalid str value for type u64")));
    }
    fprintf(stderr,"internal error: B_u64G_new: argument not of atomic type");
    exit(-1);
}

B_NoneType B_u64D___init__(B_u64 self, B_atom a){
    self->val = B_u64G_new(a)->val;
    return B_None;
}

void B_u64D___serialize__(B_u64 n, $Serial$state state) {
    $val_serialize(INT_ID,&n->val,state);
}

B_u64 B_u64D___deserialize__(B_u64 n, $Serial$state state) {
    return toB_u64((long)$val_deserialize(state));
}

B_bool B_u64D___bool__(B_u64 n) {
    return toB_bool(n->val != 0);
}

B_str B_u64D___str__(B_u64 n) {
    char *s;
    asprintf(&s,"%lu",n->val);
    return to$str(s);
}

B_str B_u64D___repr__(B_u64 n) {
    char *s;
    asprintf(&s,"%lu",n->val);
    return to$str(s);
}

B_u64 toB_u64(unsigned long i) {
    B_u64 res = malloc(sizeof(struct B_u64));
    res->$class = &B_u64G_methods;
    res->val = i;
    return res;
}

unsigned long fromB_u64(B_u64 w) {
    return w->val;
}

                  

// B_IntegralD_u64 /////////////////////////////////////////////////////////////////////////

 
B_u64 B_IntegralD_u64D___add__(B_IntegralD_u64 wit,  B_u64 a, B_u64 b) {
    return toB_u64(a->val + b->val);
}  

B_complex B_IntegralD_u64D___complex__(B_IntegralD_u64 wit, B_u64 a) {
    return toB_complex((double)a->val);
}

B_u64 B_IntegralD_u64D___fromatom__(B_IntegralD_u64 wit, B_atom a) {
    return B_u64G_new(a);
}

B_u64 B_IntegralD_u64D___mul__(B_IntegralD_u64 wit,  B_u64 a, B_u64 b) {
    return toB_u64(a->val * b->val);
}  
  
B_u64 B_IntegralD_u64D___pow__(B_IntegralD_u64 wit,  B_u64 a, B_u64 b) {
    return toB_u64(ulongpow(a->val,b->val));
}

B_u64 B_IntegralD_u64D___neg__(B_IntegralD_u64 wit,  B_u64 a) {
    if (a->val > 0L)
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("u64.neg: cannot negate non-zero value in u64")));
    return a;
}

B_u64 B_IntegralD_u64D___pos__(B_IntegralD_u64 wit,  B_u64 a) {
    return a;
}

$WORD B_IntegralD_u64D_real(B_IntegralD_u64 wit, B_u64 a, B_Real wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)a);
}

$WORD B_IntegralD_u64D_imag(B_IntegralD_u64 wit, B_u64 a, B_Real wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)toB_u64(0L));
}

$WORD B_IntegralD_u64D___abs__(B_IntegralD_u64 wit, B_u64 a, B_Real wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)a);
}

B_u64 B_IntegralD_u64D_conjugate(B_IntegralD_u64 wit,  B_u64 a) {
    return a;
}

B_float B_IntegralD_u64D___float__ (B_IntegralD_u64 wit, B_u64 n) {
    return to$float((double)n->val);
}

$WORD B_IntegralD_u64D___trunc__ (B_IntegralD_u64 wit, B_u64 n, B_Integral wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)n);
}
  
$WORD B_IntegralD_u64D___floor__ (B_IntegralD_u64 wit, B_u64 n, B_Integral wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)n);
}
  
$WORD B_IntegralD_u64D___ceil__ (B_IntegralD_u64 wit, B_u64 n, B_Integral wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)n);
}
  
B_u64 B_IntegralD_u64D___round__ (B_IntegralD_u64 wit, B_u64 n, B_int p) {
    unsigned long nval = n->val;
    long pval = p==NULL ? 0 : from$int(p);
    if (pval>=0)
        return n;
    unsigned long p10 = ulongpow(10,-pval);
    unsigned long res = nval/p10;
    if (nval%p10 * 2 > p10)
        res++; 
    return toB_u64 (res * p10);
}
  
$WORD B_IntegralD_u64D_numerator (B_IntegralD_u64 wit, B_u64 n, B_Integral wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)n);
}
  
$WORD B_IntegralD_u64D_denominator (B_IntegralD_u64 wit, B_u64 n, B_Integral wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)toB_u64(1L));
}
  
B_int B_IntegralD_u64D___int__ (B_IntegralD_u64 wit, B_u64 n) {
    return B_intG_new((B_atom)n);
}

B_int B_IntegralD_u64D___index__(B_IntegralD_u64 wit, B_u64 n) {
    return B_intG_new((B_atom)n);
}

B_tuple B_IntegralD_u64D___divmod__(B_IntegralD_u64 wit, B_u64 a, B_u64 b) {
    int n = a->val;
    int d = b->val;
    return $NEWTUPLE(2, toB_u64(n/d), toB_u64(n%d));
}

B_u64 B_IntegralD_u64D___floordiv__(B_IntegralD_u64 wit, B_u64 a, B_u64 b) {
    return toB_u64(a->val / b->val);
}

B_u64 B_IntegralD_u64D___mod__(B_IntegralD_u64 wit, B_u64 a, B_u64 b) {
    return toB_u64(a->val % b->val);
}

B_u64 B_IntegralD_u64D___lshift__(B_IntegralD_u64 wit,  B_u64 a, B_int b) {
    return toB_u64(a->val << from$int(b));
}

B_u64 B_IntegralD_u64D___rshift__(B_IntegralD_u64 wit,  B_u64 a, B_int b) {
    return toB_u64(a->val >> from$int(b));
}
 
B_u64 B_IntegralD_u64D___invert__(B_IntegralD_u64 wit,  B_u64 a) {
    return toB_u64(~a->val);
}


// B_LogicalD_IntegralD_u64  ////////////////////////////////////////////////////////////////////////////////////////

B_u64 B_LogicalD_IntegralD_u64D___and__(B_LogicalD_IntegralD_u64 wit,  B_u64 a, B_u64 b) {
    return toB_u64(a->val & b->val);
}
                                                 
B_u64 B_LogicalD_IntegralD_u64D___or__(B_LogicalD_IntegralD_u64 wit,  B_u64 a, B_u64 b) {
    return toB_u64(a->val | b->val);
}
                                                 
B_u64 B_LogicalD_IntegralD_u64D___xor__(B_LogicalD_IntegralD_u64 wit,  B_u64 a, B_u64 b) {
    return toB_u64(a->val ^ b->val);
}  
 
// B_MinusD_IntegralD_u64  ////////////////////////////////////////////////////////////////////////////////////////

 
B_u64 B_MinusD_IntegralD_u64D___sub__(B_MinusD_IntegralD_u64 wit,  B_u64 a, B_u64 b) {
    return toB_u64(a->val - b->val);
}  

// B_DivD_u64  ////////////////////////////////////////////////////////////////////////////////////////

 
B_float B_DivD_u64D___truediv__ (B_DivD_u64 wit, B_u64 a, B_u64 b) {
    return to$float((double)a->val/(double)b->val);
}

// B_OrdD_u64  ////////////////////////////////////////////////////////////////////////////////////////

B_bool B_OrdD_u64D___eq__ (B_OrdD_u64 wit, B_u64 a, B_u64 b) {
    return toB_bool(a->val == b->val);
}

B_bool B_OrdD_u64D___ne__ (B_OrdD_u64 wit, B_u64 a, B_u64 b) {
    return toB_bool(a->val != b->val);
}

B_bool B_OrdD_u64D___lt__ (B_OrdD_u64 wit, B_u64 a, B_u64 b) {
    return toB_bool(a->val < b->val);
}

B_bool B_OrdD_u64D___le__ (B_OrdD_u64 wit, B_u64 a, B_u64 b) {
    return toB_bool(a->val <= b->val);
}

B_bool B_OrdD_u64D___gt__ (B_OrdD_u64 wit, B_u64 a, B_u64 b) {
    return toB_bool(a->val > b->val);
}

B_bool B_OrdD_u64D___ge__ (B_OrdD_u64 wit, B_u64 a, B_u64 b) {
    return toB_bool(a->val >= b->val);
}

// B_HashableD_u64 ///////////////////////////////////////////////////////////////////////////////////////////////////////

B_bool B_HashableD_u64D___eq__(B_HashableD_u64 wit, B_u64 a, B_u64 b) {
    return toB_bool(a->val == b->val);
}

B_bool B_HashableD_u64D___ne__(B_HashableD_u64 wit, B_u64 a, B_u64 b) {
    return toB_bool(a->val != b->val);
}

B_int B_HashableD_u64D___hash__(B_HashableD_u64 wit, B_u64 a) {
    return to$int(B_i64D_hash((B_i64)a));
}
