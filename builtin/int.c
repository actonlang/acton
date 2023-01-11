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
// long longpow(long a, long e) {
//   if (e == 0) return 1;
//  if (e == 1) return a;
//  if (e%2 == 0) return longpow(a*a,e/2);
//  return a * longpow(a*a,e/2);
//}

// General methods ///////////////////////////////////////////////////////////////////////

int B_setD_str(zz_ptr a, char *str);
char *$getB_str(zz_ptr n);

B_int $mallocB_int() {
    B_int res = malloc(sizeof(struct B_int));
    res->$class = &B_intG_methods;
    res->val.n = malloc(sizeof(unsigned long));
    res->val.size = 0;
    res->val.alloc = 1;
    return res;
}


B_int B_intG_new(B_atom a) {
    if ($ISINSTANCE(a,B_int)->val) return (B_int)a;
    if ($ISINSTANCE(a,B_i64)->val) {
        return toB_int(((B_i64)a)->val);
    }
    if ($ISINSTANCE(a,B_float)->val) {
        double aval = ((B_float)a)->val;
        int e;
        double m = frexp(aval,&e);
        if (e>52) {
            B_int c = toB_int((long)(m*4503599627370496.0)); // (1<< 52); 
            B_int d = toB_int(e-52);
            return  B_IntegralD_intD___lshift__(NULL,c,d);
        } else {
            long al = (long)aval;
            B_int res = toB_int(al);
            return res;
        }
    }
    if ($ISINSTANCE(a,B_bool)->val) return toB_int(((B_bool)a)->val);
    if ($ISINSTANCE(a,B_str)->val) {
        B_int res = $mallocB_int();
        res->$class = &B_intG_methods;
        int digits = B_setD_str(&res->val, (char *)((B_str)a)->str);
        if (digits>0)
            return res;
        else 
            $RAISE((B_BaseException)$NEW(B_ValueError,to$str("int(): string arg has no digit in prefix")));
    }
    fprintf(stderr,"internal error: B_intG_new: argument not of atomic type\n");
    exit(-1);
}

void B_intD_init(B_int self, B_atom a){
    self->val = B_intG_new(a)->val;
}

void B_intD_serialize(B_int self,$NoneType state) {
    B_int prevkey = (B_int)B_dictD_get(state->done,(B_Hashable)B_HashableD_WORDG_witness,self,NULL);
    if (prevkey) {
        long pk = fromB_int(prevkey);
        $val_serialize(-INT_ID,&pk,state);
        return;
    }
    int blobsize = 1 + labs(self->val.size);
    $ROW row = $add_header(INT_ID,blobsize,state);
    row->blob[0] = ($WORD)self->val.size;
    memcpy(&row->blob[1],self->val.n,labs(self->val.size)*sizeof(long));
}

B_int B_intD_deserialize(B_int res,$NoneType state) {
    $ROW this = state->row;
    state->row = this->next;
    state->row_no++;
    if (this->class_id < 0) {
        return (B_int)B_dictD_get(state->done,(B_Hashable)B_HashableD_intG_witness,toB_int((int)this->blob[0]),NULL);
    } else {
        if (!res)
            res = $mallocB_int();
        res->val.size = (long)this->blob[0];
        res->val.alloc = labs(res->val.size);
        res->val.n = malloc(res->val.alloc*sizeof(long));
        memcpy(res->val.n,&this->blob[1],res->val.alloc*sizeof(long));
        B_dictD_setitem(state->done,(B_Hashable)B_HashableD_intG_witness,toB_int(state->row_no-1),res);
        res->$class = &B_intG_methods;
        return res;
    }
}

B_bool B_intD_bool(B_int n) {
    return toB_bool(zz_cmpi(&n->val,0));
}

B_str B_intD_str(B_int n) {
    return to$str($getB_str(&n->val));
}
  
struct B_intG_class B_intG_methods = {
    "B_int",
    UNASSIGNED,
    ($SuperG_class)&B_atomG_methods,
    B_intD_init,
    B_intD_serialize,
    B_intD_deserialize,
    B_intD_bool,
    B_intD_str,
    B_intD_str
};

B_int zz$toB_int(zz_ptr n) {
    B_int res = $mallocB_int();
    res->$class = &B_intG_methods;
    res->val.n = n->n;
    res->val.size = n->size;
    res->val.alloc = n->alloc;
    return res;
}

// B_IntegralD_int /////////////////////////////////////////////////////////////////////////

void B_IntegralD_intD___serialize__(B_IntegralD_int self, $NoneType state) {
    $step_serialize(self->W_Logical, state);
    $step_serialize(self->W_Minus, state);
}

B_IntegralD_int B_IntegralD_intD___deserialize__(B_IntegralD_int self, $NoneType state) {
    B_IntegralD_int res = $DNEW(B_IntegralD_int,state);
    res->W_Logical = (B_Logical)$step_deserialize(state);
    res->W_Minus = (B_Minus)$step_deserialize(state);
    return res;
}

B_int B_IntegralD_intD___add__(B_IntegralD_int wit,  B_int a, B_int b) {
    B_int res = $mallocB_int();
    zz_add(&res->val,&a->val,&b->val);
    return res;
}

B_complex B_IntegralD_intD___complx__(B_IntegralD_int wit, B_int a) {
    fprintf(stderr,"Number.__complex__ not implemented for int");
    exit(1);
}

B_int B_IntegralD_intD___fromatom__(B_IntegralD_int wit, B_atom a) {
    return B_intG_new(a);
}

B_int B_IntegralD_intD___mul__(B_IntegralD_int wit,  B_int a, B_int b) {
    B_int res = $mallocB_int();
    zz_mul(&res->val,&a->val,&b->val);
    return res;
}  
  
B_int B_IntegralD_intD___pow__(B_IntegralD_int wit, B_int a, B_int b) {
    zz_ptr val_b = &b->val;
    if (zz_cmpi(val_b,0) < 0)
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("__pow__: exponent negative")));
    if (zz_cmpi(val_b,LONG_MAX) > 0)
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("__pow__: exponent out of range (> LONG_MAX)")));
    B_int res = $mallocB_int();
    zz_powi(&res->val,&a->val,val_b->n[0]); // __pow__ should have an int64 exponent in the Acton protocol
    return res;
}

B_int B_IntegralD_intD___neg__(B_IntegralD_int wit,  B_int a) {
    B_int res = $mallocB_int();
    zz_neg(&res->val,&a->val);
    return res;
}

B_int B_IntegralD_intD___pos__(B_IntegralD_int wit,  B_int a) {
    return a;
}

$WORD B_IntegralD_int$real(B_IntegralD_int wit, B_int a, B_Real wit2) {
    fprintf(stderr,"Number.__real__ not implemented for int");
    exit(1);
}

$WORD B_IntegralD_int$imag(B_IntegralD_int wit, B_int a, B_Real wit2) {
    fprintf(stderr,"Number.__imag__ not implemented for int");
    exit(1);
}

$WORD B_IntegralD_intD___abs__(B_IntegralD_int wit, B_int a, B_Real wit2) {
    B_int res = $mallocB_int();
    zz_set(&res->val,&a->val);
    res->val.size = labs(a->val.size);
    return wit2->$class->__fromatom__(wit2,(B_atom)res);
}

B_int B_IntegralD_intD___conjugate__(B_IntegralD_int wit,  B_int a) {
    return a;
}

B_float B_IntegralD_intD___float__ (B_IntegralD_int wit, B_int n) {
    return B_floatG_new((B_atom)n);
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
    zz_struct nval = n->val;
    if (nval.size < 0) {
        B_int n1 = $mallocB_int();
        zz_neg(&n1->val,&nval);
        B_int res = B_IntegralD_intD___round__(wit,n1,p);
        zz_neg(&res->val,&res->val);
        return res;
    }
    if (labs(p->val.size) >1)
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("__round__: precision out of range")));
    long pval = fromB_int(p);
    if (pval>=0)
        return n;
    B_int p10 = B_IntegralD_intD___pow__(NULL,toB_int(10), B_IntegralD_intD___neg__(NULL,p));
    return B_IntegralD_intD___mul__(NULL,n,p10);
}
  
$WORD B_IntegralD_int$numerator (B_IntegralD_int wit, B_int n, B_Integral wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)n);
}
  
$WORD B_IntegralD_int$denominator (B_IntegralD_int wit, B_int n, B_Integral wit2) {
    B_int res = toB_int(1L);
    return wit2->$class->__fromatom__(wit2,(B_atom)res);
}
  
B_int B_IntegralD_intD___int__ (B_IntegralD_int wit, B_int n) {
    return n;
}

B_int B_IntegralD_intD___index__(B_IntegralD_int wit, B_int n) {
    return n;
}

B_tuple B_IntegralD_intD___divmod__(B_IntegralD_int wit, B_int a, B_int b) {
    B_int q = $mallocB_int();
    B_int r = $mallocB_int();
    zz_divrem(&q->val,&r->val,&a->val,&b->val);
    return $NEWTUPLE(2, q, r);
}

B_int B_IntegralD_intD___floordiv__(B_IntegralD_int wit, B_int a, B_int b) {
    B_int res = $mallocB_int();
    zz_div(&res->val,&a->val,&b->val);
    return res;
}

B_int B_IntegralD_intD___mod__(B_IntegralD_int wit, B_int a, B_int b) {
    B_tuple t = B_IntegralD_intD___divmod__(wit,a,b);
    return t->components[1];
}

B_int B_IntegralD_intD___lshift__(B_IntegralD_int wit,  B_int a, B_int b) {
    zz_struct aval = a->val;
    long ma = aval.size;
    long bval = fromB_int(b);
    if (ma==0 || bval==0)
        return a;
    if (bval<0)
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("__lshift: negative shift count")));
    long shw = bval/64;
    long shb = bval%64;
    long mres = labs(ma) + shw + (shb > 0);
    B_int res = $mallocB_int();
    zz_ptr rval = &res->val;
    zz_init_fit(rval,mres);
    if (shb>0) {
        word_t ci = nn_shl(rval->n, aval.n, labs(ma), shb);
        if (ci>0)
            rval->n[labs(ma)] = ci;
    }
    if (shw>0) {
        for (int i = labs(ma); i >= 0; i--)
            rval->n[i+shw] = rval->n[i];
        for (int i = 0; i < shw; i++)
            rval->n[i] = 0;
    }
    mres = mres - (rval->n[mres-1]==0);
    mres = ma<0? -mres:mres;
    rval->size = mres;
    return res; 
}

B_int B_IntegralD_intD___rshift__(B_IntegralD_int wit,  B_int a, B_int b) {
    zz_struct aval = a->val;
    long ma = aval.size;
    long bval = fromB_int(b);
    if (ma==0 || bval==0)
        return a;
    if (bval<0)
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("__rshift: negative shift count")));
    B_int res = $mallocB_int();
    zz_ptr rval = &res->val;
    long shw = bval/64;
    long shb = bval%64;
    long mres = labs(ma) - shw;
    zz_init_fit(rval,mres);
    unsigned long tmp[mres];
    for (int i = 0; i < mres; i++)
        tmp[i] = aval.n[i+shw];
    word_t ci = nn_shr(rval->n, tmp, mres, shb);
    mres = mres - (rval->n[mres-1]==0);
    mres = ma<0?-mres:mres;
    res->val.size = mres;
    return res; 
}
 
B_int B_IntegralD_intD___invert__(B_IntegralD_int wit,  B_int a) {
    //return toB_i64(~a->val);
    fprintf(stderr,"Number.__invert__ not implemented for int\n");
    exit(1);
}


// LogicalB_int  ////////////////////////////////////////////////////////////////////////////////////////

void B_LogicalD_IntegralD_intD___serialize__(B_LogicalD_IntegralD_int self, $NoneType state) {
    //$step_serialize(self->W_Integral, state);
    fprintf(stderr,"Protocol Logical not implemented for int; use i64\n");
    exit(1);
}

B_LogicalD_IntegralD_int B_LogicalD_IntegralD_intD___deserialize__(B_LogicalD_IntegralD_int self, $NoneType state) {
    // B_LogicalD_IntegralD_i64 res = $DNEW(B_LogicalD_IntegralD_i64,state);
    //  res->W_Integral = (B_Integral)$step_deserialize(state);
    //  return res;
    fprintf(stderr,"Protocol Logical not implemented for int; use i64\n");
    exit(1);
}

B_int B_LogicalD_IntegralD_intD___and__(B_LogicalD_IntegralD_int wit,  B_int a, B_int b) {
    // return toB_i64(a->val & b->val);
    fprintf(stderr,"Protocol Logical not implemented for int; use i64\n");
    exit(1);
}
                                                 
B_int B_LogicalD_IntegralD_intD___or__(B_LogicalD_IntegralD_int wit,  B_int a, B_int b) {
    // return toB_i64(a->val | b->val);
    fprintf(stderr,"Protocol Logical not implemented for int; use i64\n");
    exit(1);
}
                                                 
B_int B_LogicalD_IntegralD_intD___xor__(B_LogicalD_IntegralD_int wit,  B_int a, B_int b) {
    // return toB_i64(a->val ^ b->val);
    fprintf(stderr,"Protocol Logical not implemented for int; use i64\n");
    exit(1);
}  
 
// B_MinusD_IntegralD_int  ////////////////////////////////////////////////////////////////////////////////////////

void B_MinusD_IntegralD_intD___serialize__(B_MinusD_IntegralD_int self, $NoneType state) {
    $step_serialize(self->W_Integral, state);
}

B_MinusD_IntegralD_int B_MinusD_IntegralD_intD___deserialize__(B_MinusD_IntegralD_int self, $NoneType state) {
    B_MinusD_IntegralD_int res = $DNEW(B_MinusD_IntegralD_int,state);
    res->W_Integral = (B_Integral)$step_deserialize(state);
    return res;
}

B_int B_MinusD_IntegralD_intD___sub__(B_MinusD_IntegralD_int wit,  B_int a, B_int b) {
    B_int res = $mallocB_int();
    zz_sub(&res->val,&a->val,&b->val);
    return res;
}


// B_DivD_int  ////////////////////////////////////////////////////////////////////////////////////////

void B_DivD_intD___serialize__(B_DivD_int self, $NoneType state) {
}

B_DivD_int B_DivD_intD___deserialize__(B_DivD_int self, $NoneType state) {
    B_DivD_int res = $DNEW(B_DivD_int,state);
    return res;
}

B_float B_DivD_intD___truediv__ (B_DivD_int wit, B_int a, B_int b) {
    zz_ptr aval = &a->val;
    zz_ptr bval = &b->val;
    B_int ared = $mallocB_int();
    B_int bred = $mallocB_int();
    B_int q = $mallocB_int();
    B_int r = $mallocB_int();
    B_int g = $mallocB_int();
    zz_gcd(&g->val,aval,bval);
    zz_div(&ared->val,aval,&g->val);
    zz_div(&bred->val,bval,&g->val);
    zz_divrem(&q->val,&r->val,&ared->val,&bred->val);
    return toB_float(B_floatG_new((B_atom)q)->val +  B_floatG_new((B_atom)r)->val/ B_floatG_new((B_atom)bred)->val);
}

// B_OrdD_int  ////////////////////////////////////////////////////////////////////////////////////////

void B_OrdD_intD___serialize__(B_OrdD_int self, $NoneType state) {
}

B_OrdD_int B_OrdD_intD___deserialize__(B_OrdD_int self, $NoneType state) {
    B_OrdD_int res = $DNEW(B_OrdD_int,state);
    return res;
}

B_bool B_OrdD_intD___eq__ (B_OrdD_int wit, B_int a, B_int b) {
    return toB_bool(zz_equal(&a->val,&b->val));
}

B_bool B_OrdD_intD___ne__ (B_OrdD_int wit, B_int a, B_int b) {
    return toB_bool(1-zz_equal(&a->val,&b->val));
}

B_bool B_OrdD_intD___lt__ (B_OrdD_int wit, B_int a, B_int b) {
    return toB_bool(zz_cmp(&a->val,&b->val) < 0);
}

B_bool B_OrdD_intD___le__ (B_OrdD_int wit, B_int a, B_int b) {
    return toB_bool(zz_cmp(&a->val,&b->val) <= 0);
}

B_bool B_OrdD_intD___gt__ (B_OrdD_int wit, B_int a, B_int b) {
    return toB_bool(zz_cmp(&a->val,&b->val) > 0);
}

B_bool B_OrdD_intD___ge__ (B_OrdD_int wit, B_int a, B_int b) {
    return toB_bool(zz_cmp(&a->val,&b->val) >= 0);
}

// B_HashableD_int ///////////////////////////////////////////////////////////////////////////////////////////////////////

void B_HashableD_intD___serialize__(B_HashableD_int self, $NoneType state) {
}

B_HashableD_int B_HashableD_intD___deserialize__(B_HashableD_int self, $NoneType state) {
    B_HashableD_int res = $DNEW(B_HashableD_int,state);
    return res;
}

B_bool B_HashableD_intD___eq__(B_HashableD_int wit, B_int a, B_int b) {
    return toB_bool(zz_equal(&a->val,&b->val));
}

B_bool B_HashableD_intD___ne__(B_HashableD_int wit, B_int a, B_int b) {
    return toB_bool(1-zz_equal(&a->val,&b->val));
}

B_int B_HashableD_intD___hash__(B_HashableD_int wit, B_int a) {
    //    B_int res = $mallocB_int();
    //    zz_ptr q = malloc(sizeof(zz_struct));
    //    zz_init_fit(q,1);
    //    zz_seti(&res->val,zz_divremi(q,&a->val,LONG_MAX/4));    // This hash algorithm should be reconsidered!!!

    return toB_int(B_i64D_hash(toB_i64(fromB_int(a))));
}

// Initialization ////////////////////////////////////////////////////////////////////////////////////////////////////////

void B_IntegralD_int_init(B_IntegralD_int wit) {
    wit-> W_Logical = (B_Logical)$NEW(B_LogicalD_IntegralD_int,(B_Integral)wit);
    wit-> W_Minus = (B_Minus)$NEW(B_MinusD_IntegralD_int,(B_Integral)wit);
};

void B_LogicalD_IntegralD_int_init(B_LogicalD_IntegralD_int wit, B_Integral W_Integral) {
    wit->W_Integral =  W_Integral;
}

void B_MinusD_IntegralD_int_init(B_MinusD_IntegralD_int wit, B_Integral W_Integral) {
    wit->W_Integral =  W_Integral;
}

void B_DivD_int_init(B_DivD_int wit) {
    return;
}

void B_OrdD_int_init(B_OrdD_int wit) {
    return;
}

void B_HashableD_int_init(B_HashableD_int wit) {
    return;
}

B_IntegralD_int B_IntegralD_intG_new() {
    return $NEW(B_IntegralD_int);
}

B_LogicalD_IntegralD_int B_LogicalD_IntegralD_intG_new(B_Integral wit) {
    return $NEW(B_LogicalD_IntegralD_int,wit);
}
  
B_MinusD_IntegralD_int B_MinusD_IntegralD_intG_new(B_Integral wit) {
    return $NEW(B_MinusD_IntegralD_int,wit);
}
  
B_OrdD_int B_OrdD_intG_new() {
    return $NEW(B_OrdD_int);
}

B_DivD_int B_DivD_intG_new() {
    return $NEW(B_DivD_int);
}

B_HashableD_int B_HashableD_intG_new() {
    return $NEW(B_HashableD_int);
}


struct B_IntegralD_int $IintegralB_intD_instance;
struct B_LogicalD_IntegralD_int B_LogicalD_IntegralD_int_instance;
struct B_MinusD_IntegralD_int B_MinusD_IntegralD_int_instance;
struct B_OrdD_int B_OrdD_int_instance;
struct B_DivD_int B_DivD_int_instance;
struct B_HashableD_int B_HashableD_int_instance;

struct B_IntegralD_intG_class B_IntegralD_intG_methods = {
    "B_IntegralD_int",
    UNASSIGNED,
    ($SuperG_class)&B_IntegralG_methods,
    B_IntegralD_int_init,
    B_IntegralD_intD___serialize__,
    B_IntegralD_intD___deserialize__,
    (B_bool (*)(B_IntegralD_int))$default__bool__,
    (B_str (*)(B_IntegralD_int))$default__str__,
    (B_str (*)(B_IntegralD_int))$default__str__,
    B_IntegralD_intD___add__,
    (B_int (*)(B_IntegralD_int, B_int, B_int))$PlusD___iadd__,
    B_IntegralD_intD___mul__,
    (B_int (*)(B_IntegralD_int, B_int, B_int))B_TimesD___imul__,
    B_IntegralD_intD___fromatom__,
    B_IntegralD_intD___complx__,
    B_IntegralD_intD___pow__,
    (B_int (*)(B_IntegralD_int, B_int, B_int))B_NumberD___ipow__,
    B_IntegralD_intD___neg__,
    B_IntegralD_intD___pos__,
    B_IntegralD_int$real,
    B_IntegralD_int$imag,
    B_IntegralD_intD___abs__,
    B_IntegralD_intD___conjugate__,
    B_IntegralD_intD___float__,
    B_IntegralD_intD___trunc__,
    B_IntegralD_intD___floor__,
    B_IntegralD_intD___ceil__,
    B_IntegralD_intD___round__,
    B_IntegralD_int$numerator,
    B_IntegralD_int$denominator,
    B_IntegralD_intD___int__,
    B_IntegralD_intD___index__,
    B_IntegralD_intD___divmod__,
    B_IntegralD_intD___floordiv__,
    B_IntegralD_intD___mod__,
    B_IntegralD_intD___lshift__,
    B_IntegralD_intD___rshift__,
    (B_int (*)(B_IntegralD_int, B_int, B_int))B_IntegralD___ifloordiv__,
    (B_int (*)(B_IntegralD_int, B_int, B_int))B_IntegralD___imod__,
    (B_int (*)(B_IntegralD_int, B_int, B_int))B_IntegralD___ilshift__,
    (B_int (*)(B_IntegralD_int, B_int, B_int))B_IntegralD___irshift__,
    B_IntegralD_intD___invert__
};

struct B_IntegralD_int B_IntegralD_int_instance = {&B_IntegralD_intG_methods, (B_Minus)&B_MinusD_IntegralD_int_instance, (B_Logical)&B_LogicalD_IntegralD_int_instance};
B_IntegralD_int B_IntegralD_intG_witness = &B_IntegralD_int_instance;

struct B_LogicalD_IntegralD_intG_class B_LogicalD_IntegralD_intG_methods =  {
    "B_LogicalD_IntegralD_int",
    UNASSIGNED,
    ($SuperG_class)&B_LogicalG_methods,
    B_LogicalD_IntegralD_int_init,
    B_LogicalD_IntegralD_intD___serialize__,
    B_LogicalD_IntegralD_intD___deserialize__,
    (B_bool (*)(B_LogicalD_IntegralD_int))$default__bool__,
    (B_str (*)(B_LogicalD_IntegralD_int))$default__str__,
    (B_str (*)(B_LogicalD_IntegralD_int))$default__str__,
    B_LogicalD_IntegralD_intD___and__,
    B_LogicalD_IntegralD_intD___or__,
    B_LogicalD_IntegralD_intD___xor__,
    (B_int (*)(B_LogicalD_IntegralD_int, B_int, B_int))B_LogicalD___iand__,
    (B_int (*)(B_LogicalD_IntegralD_int, B_int, B_int))B_LogicalD___ior__,
    (B_int (*)(B_LogicalD_IntegralD_int, B_int, B_int))B_LogicalD___ixor__
};

struct B_LogicalD_IntegralD_int B_LogicalD_IntegralD_int_instance = {&B_LogicalD_IntegralD_intG_methods, (B_Integral)&B_IntegralD_int_instance};
B_LogicalD_IntegralD_int B_LogicalD_IntegralD_intG_witness = &B_LogicalD_IntegralD_int_instance;

struct B_MinusD_IntegralD_intG_class B_MinusD_IntegralD_intG_methods = {
    "B_MinusD_IntegralD_int",
    UNASSIGNED,
    ($SuperG_class)&B_MinusG_methods,
    B_MinusD_IntegralD_int_init,
    B_MinusD_IntegralD_intD___serialize__,
    B_MinusD_IntegralD_intD___deserialize__,
    (B_bool (*)(B_MinusD_IntegralD_int))$default__bool__,
    (B_str (*)(B_MinusD_IntegralD_int))$default__str__,
    (B_str (*)(B_MinusD_IntegralD_int))$default__str__,
    B_MinusD_IntegralD_intD___sub__,
    (B_int (*)(B_MinusD_IntegralD_int, B_int, B_int))B_MinusD___isub__
};
struct B_MinusD_IntegralD_int B_MinusD_IntegralD_int_instance = {&B_MinusD_IntegralD_intG_methods, (B_Integral)&B_IntegralD_int_instance};
B_MinusD_IntegralD_int B_MinusD_IntegralD_intG_witness = &B_MinusD_IntegralD_int_instance;

struct B_OrdD_intG_class B_OrdD_intG_methods = {
    "B_OrdD_int",
    UNASSIGNED,
    ($SuperG_class)&B_OrdG_methods,
    B_OrdD_int_init,
    B_OrdD_intD___serialize__,
    B_OrdD_intD___deserialize__,
    (B_bool (*)(B_OrdD_int))$default__bool__,
    (B_str (*)(B_OrdD_int))$default__str__,
    (B_str (*)(B_OrdD_int))$default__str__,
    B_OrdD_intD___eq__,
    B_OrdD_intD___ne__,
    B_OrdD_intD___lt__,
    B_OrdD_intD___le__,
    B_OrdD_intD___gt__,
    B_OrdD_intD___ge__
};

struct B_OrdD_int B_OrdD_int_instance = {&B_OrdD_intG_methods};
B_OrdD_int B_OrdD_intG_witness = &B_OrdD_int_instance;

struct B_DivD_intG_class B_DivD_intG_methods = {
    "B_DivD_int",
    UNASSIGNED,
    ($SuperG_class)&B_DivG_methods,
    B_DivD_int_init,
    B_DivD_intD___serialize__,
    B_DivD_intD___deserialize__,
    (B_bool (*)(B_DivD_int))$default__bool__,
    (B_str (*)(B_DivD_int))$default__str__,
    (B_str (*)(B_DivD_int))$default__str__,
    B_DivD_intD___truediv__,
    (B_float (*)(B_DivD_int, B_int, B_int))B_DivD___itruediv__,
};

struct B_DivD_int B_DivD_int_instance = {&B_DivD_intG_methods};
B_DivD_int B_DivD_intG_witness = &B_DivD_int_instance;

struct B_HashableD_intG_class B_HashableD_intG_methods = {
    "B_HashableD_int",
    UNASSIGNED,
    ($SuperG_class)&B_HashableG_methods,
    B_HashableD_int_init,
    B_HashableD_intD___serialize__,
    B_HashableD_intD___deserialize__,
    (B_bool (*)(B_HashableD_int))$default__bool__,
    (B_str (*)(B_HashableD_int))$default__str__,
    (B_str (*)(B_HashableD_int))$default__str__,
    B_HashableD_intD___eq__,
    B_HashableD_intD___ne__,
    B_HashableD_intD___hash__
};

struct B_HashableD_int B_HashableD_int_instance = {&B_HashableD_intG_methods};
B_HashableD_int B_HashableD_intG_witness = &B_HashableD_int_instance;

long fromB_int(B_int n) { 
    long sz = n->val.size;
    if (sz==0) return 0;
    unsigned long res = n->val.n[0];
    if (res > LONG_MAX || labs(sz) > 1) {
        fprintf(stderr,"internal error: overflow in converting int to bounded int\n");
        exit(1);
    }
    return sz<0 ? -res : res;
}
            
B_int toB_int(long n) {
    B_int res = malloc(sizeof(struct B_int));
    res->$class = &B_intG_methods;
    res->val.n = malloc(sizeof(unsigned long));
    res->val.n[0] = n<0?-n:n;
    res->val.alloc = 1;
    if (n==0)
        res->val.size=0;
    else
        res->val.size = n>0?1:-1;
    return res;
}

// Conversion to strings /////////////////////////////////////////////////////////////////////////////

// These three constants must be changed for a 32 bit machine

int POW10INWORD = 18; // Largest power of 10 that fits in a signed long 
double CCCC = 9.805415291306852e-2;  // log2(WORD_BITS) - log2 (POW10INWORD) - log2 (log2(10))
char * fstr =  "%18lu";

int $getB_str0(bool ishead, zz_ptr n, zz_ptr dens[], int d, char *res, int pos) {
    if (d >= 0) {
        zz_ptr hi = malloc(sizeof(zz_struct));
        zz_ptr lo = malloc(sizeof(zz_struct));
        zz_init_fit(hi,dens[d]->size);
        zz_init_fit(lo,dens[d]->size);
        zz_divrem(hi, lo, n, dens[d]);
        if (hi->size==0 && ishead) {
            return $getB_str0(ishead, lo, dens, d-1, res, pos);
        } else {
            int newpos = $getB_str0(ishead, hi, dens, d-1, res, pos);        
            return $getB_str0(false, lo, dens, d-1, res, newpos);          
        }
    } else {
        char *buf = malloc(POW10INWORD);
        asprintf(&buf,"%lu",(unsigned long)n->n[0]);
        int len = strlen(buf);
        if (ishead) {
            memcpy(&res[pos], buf, len);
            return pos + len;
        } else {
            memcpy(&res[pos + POW10INWORD - len], buf, len);
            return pos + POW10INWORD;
        }
    }
}

char * $getB_str(zz_ptr nval) {
    if (nval->size == 0)
        return "0";
    long nlen = BSDNT_ABS(nval->size);
    zz_ptr npos = malloc(sizeof(zz_struct));
    zz_init_fit(npos,nlen);
    nn_copy(npos->n, nval->n, nlen);
    npos->size = nlen;
    int is_neg_n = nval->size < 0;
    int d;
    zz_ptr *dens;
    if (nlen == 1) {
        d = 0;
        dens = NULL;
    } else {
        d = ceil(log2((double)nlen) + CCCC);  //number of squarings
        dens = malloc(d * sizeof(zz_ptr));
        dens[0] = malloc(sizeof(zz_struct));
        zz_init_fit(dens[0], 1);
        zz_seti(dens[0], 10); 
        zz_powi(dens[0], dens[0], POW10INWORD);
        for (int i=1; i < d; i++) {
            dens[i] = malloc(sizeof(zz_struct));
            zz_init_fit(dens[i], 2 * dens[i-1]->size);
            zz_mul(dens[i], dens[i-1], dens[i-1]);
        }
    }
    // strlen is for most n one more than necessary; this is a precaution for values of n
    // where the ... in ceil(...) is very close to an integer. So we often waste one byte.
    int strlen = ceil(log10((float)npos->n[nlen - 1]) + (nlen - 1) * WORD_BITS * log10(2) + is_neg_n) + 2;
    char *res = malloc(strlen);    
    memset(res,'0', strlen);
    int pos = 0;
    if (is_neg_n) {
        res[0] = '-';
        pos++;
    }
    int newpos = $getB_str0(true, npos, dens, d-1, res, pos);
    res[newpos] = '\0';
    return res;
}

int B_setD_str0(zz_ptr a, char *nstr, int parts) {
    // assert(parts > 0);
    if (parts == 1) {
        unsigned long val;
        sscanf(nstr, fstr, &val); 
        zz_seti(a, val);
        return POW10INWORD;
    } else {
        int hi = parts/2;
        int lo = parts - hi;
        zz_ptr hires = malloc(sizeof(zz_struct));
        zz_ptr lores = malloc(sizeof(zz_struct));
        zz_init(hires);
        zz_init(lores);
        int hidigs = B_setD_str0(hires, nstr, hi);
        int lodigs = B_setD_str0(lores, &nstr[hi * POW10INWORD], lo);
        zz_seti(a, 10);
        zz_powi(a, a, POW10INWORD * lo);
        zz_mul(a, a, hires);
        zz_add(a, a, lores);
        return hidigs + lodigs;
    }
}

int B_setD_str(zz_ptr a, char *nstr) {
    int len = 0;
    while (isdigit(nstr[len]))
        len++;
    if (len == 0) {
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("int.__fromatom__: no digits in string prefix")));
    }
    int parts = len / POW10INWORD;
    int offset =  len % POW10INWORD;
    if (offset == 0)
        return B_setD_str0(a, nstr, parts);
    else {
        zz_ptr res0 = malloc(sizeof(zz_struct));
        zz_init(res0);
        char *buf = malloc(offset+1);
        memcpy(buf, nstr, offset);
        buf[offset] = '\0';
        unsigned long headval;
        int partdigits = 0;
        sscanf(buf, "%lu", &headval);
        if (parts > 0) {
            partdigits = B_setD_str0(res0, &nstr[offset], parts);
            zz_seti(a, 10);
            zz_powi(a, a, POW10INWORD * parts);
            zz_muli(a, a, headval);
            zz_add(a, a, res0);
        } else {
            zz_seti(a, headval);
        }
        return offset + partdigits;
    } 
}


// gcd functions from BSDNT //////////////////////////////////
B_int $gcd(B_int a, B_int b) {
    B_int res = $mallocB_int();
    zz_gcd(&res->val, &a->val, &b->val);
    return res;
}

B_tuple $xgcd(B_int a, B_int b) {
    B_int d = $mallocB_int();
    B_int s = $mallocB_int();
    B_int t = $mallocB_int();
    zz_xgcd(&d->val, &s->val, &t->val, &a->val, &b->val);
    return $NEWTUPLE(3, d, s, t);
}
    
