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

#define GC_THREADS 1
#include "gc.h"

// General methods ///////////////////////////////////////////////////////////////////////

int set_str(zz_ptr a, unsigned char *str, B_int intbase);

B_int malloc_int() {
    B_int res = acton_malloc(sizeof(struct B_int));
    res->$class = &B_intG_methods;
    res->val.n = acton_malloc_atomic(sizeof(unsigned long));
    res->val.size = 0;
    res->val.alloc = 1;
    return res;
}

void zz_malloc_fit(zz_ptr res, len_t m) {
    res->n = acton_malloc_atomic(sizeof(unsigned long) * m);
    res->size = 0;
    res->alloc = m;
}

B_int B_IntegralD_intD___lshift__(B_IntegralD_int wit,  B_int a, B_int b);

B_int B_intG_new(B_atom a, B_int base) {
    if(base) {
        if ($ISINSTANCE0(a,B_str)) {
            B_int res = malloc_int();
            res->$class = &B_intG_methods;
            set_str(&res->val, ((B_str)a)->str, base);
            return res;
        } else {
            char errmsg[1024];
            printf("in exception branch");
            snprintf(errmsg, sizeof(errmsg), "integer type constructor: base argument is only allowed when converting from a str");
            $RAISE($NEW(B_BaseException,to$str(errmsg)));
        }
    }
    if ($ISINSTANCE0(a,B_int)) return (B_int)a;
    if ($ISINSTANCE0(a,B_i64)) {
        return to$int(((B_i64)a)->val);
    }
    if ($ISINSTANCE0(a,B_i32)) {
        return to$int((long)((B_i32)a)->val);
    }
    if ($ISINSTANCE0(a,B_i16)) {
        return to$int((long)((B_i16)a)->val);
    }
    if ($ISINSTANCE0(a,B_u64)) {
        unsigned long v = ((B_u64)a)->val;
        if (v==0) 
            return to$int(0L);
        else {
            B_int res = malloc_int();
            res->val.size=1;
            res->val.n[0] = v;
            return res;
        }
    }
    if ($ISINSTANCE0(a,B_u32)) {
        unsigned int v = ((B_u32)a)->val;
        if (v==0) 
            return to$int(0L);
        else {
            B_int res = malloc_int();
            res->val.size=1;
            res->val.n[0] = (unsigned long)v;
            return res;
        }
    }
    if ($ISINSTANCE0(a,B_u16)) {
        unsigned short v = ((B_u16)a)->val;
        if (v==0) 
            return to$int(0L);
        else {
            B_int res = malloc_int();
            res->val.size=1;
            res->val.n[0] = (unsigned long)v;
            return res;
        }
    }
    if ($ISINSTANCE0(a,B_float)) {
        double aval = ((B_float)a)->val;
        int e;
        double m = frexp(aval,&e);
        if (e>52) {
            B_int c = to$int((long)(m*4503599627370496.0)); // (1<< 52); 
            B_int d = to$int(e-52);
            return  B_IntegralD_intD___lshift__(NULL,c,d);
        } else {
            long al = (long)aval;
            B_int res = to$int(al);
            return res;
        }
    }
    if ($ISINSTANCE0(a,B_bool)) return to$int(((B_bool)a)->val);
    if ($ISINSTANCE0(a,B_str)) {
        B_int res = malloc_int();
        res->$class = &B_intG_methods;
        set_str(&res->val, ((B_str)a)->str, base);
        return res;
    }
    fprintf(stderr,"internal error: B_intG_new: argument not of atomic type\n");
    exit(-1);
}

B_NoneType B_intD___init__(B_int self, B_atom a, B_int base){
    self->val = B_intG_new(a,base)->val;
    return B_None;
}

void B_intD___serialize__(B_int self,$Serial$state state) {
    B_int prevkey = (B_int)B_dictD_get(state->done,(B_Hashable)B_HashableD_WORDG_witness,self,NULL);
    if (prevkey) {
        long pk = from$int(prevkey);
        $val_serialize(-INT_ID,&pk,state);
        return;
    }
    B_dictD_setitem(state->done,(B_Hashable)B_HashableD_WORDG_witness,self,to$int(state->row_no));
    int blobsize = 1 + labs(self->val.size);
    $ROW row = $add_header(INT_ID,blobsize,state);
    row->blob[0] = ($WORD)self->val.size;
    memcpy(&row->blob[1],self->val.n,labs(self->val.size)*sizeof(long));
}

B_int B_intD___deserialize__(B_int res,$Serial$state state) {
    $ROW this = state->row;
    state->row = this->next;
    state->row_no++;
    if (this->class_id < 0) {
        return (B_int)B_dictD_get(state->done,(B_Hashable)B_HashableD_intG_witness,to$int((long)this->blob[0]),NULL);
    } else {
        if (!res)
            res = malloc_int();
        res->val.size = (long)this->blob[0];
        res->val.alloc = labs(res->val.size);
        res->val.n = acton_malloc(res->val.alloc*sizeof(long));
        memcpy(res->val.n,&this->blob[1],res->val.alloc*sizeof(long));
        B_dictD_setitem(state->done,(B_Hashable)B_HashableD_intG_witness,to$int(state->row_no-1),res);
        res->$class = &B_intG_methods;
        return res;
    }
}

B_bool B_intD___bool__(B_int n) {
    return toB_bool(zz_cmpi(&n->val,0));
}

B_str B_intD___str__(B_int n) {
    return to_str_noc(get_str(&n->val));
}

B_str B_intD___repr__(B_int n) {
    return to_str_noc(get_str(&n->val));
}
  
B_int zz$to$int(zz_ptr n) {
    B_int res = malloc_int();
    res->$class = &B_intG_methods;
    res->val.n = n->n;
    res->val.size = n->size;
    res->val.alloc = n->alloc;
    return res;
}

// B_IntegralD_int /////////////////////////////////////////////////////////////////////////

 
B_int B_IntegralD_intD___add__(B_IntegralD_int wit,  B_int a, B_int b) {
    B_int res = malloc_int();
    zz_add(&res->val,&a->val,&b->val);
    return res;
}

B_int B_IntegralD_intD___zero__(B_IntegralD_int wit) {
    return to$int(0);
}

B_complex B_IntegralD_intD___complex__(B_IntegralD_int wit, B_int a) {
    $RAISE((B_BaseException)$NEW(B_NotImplementedError, to$str("Number.__complex__ not implemented for int")));
    return NULL; // This is just to silence compiler warning, above RAISE will longjmp from here anyway
}

B_int B_IntegralD_intD___fromatom__(B_IntegralD_int wit, B_atom a) {
    return B_intG_new(a,NULL);
}

B_int B_IntegralD_intD___mul__(B_IntegralD_int wit,  B_int a, B_int b) {
    B_int res = malloc_int();
    zz_mul(&res->val,&a->val,&b->val);
    return res;
}  
  
B_int B_IntegralD_intD___pow__(B_IntegralD_int wit, B_int a, B_int b) {
    zz_ptr val_b = &b->val;
    if (zz_cmpi(val_b,0) < 0) {
        char errmsg[1024];
        snprintf(errmsg, sizeof(errmsg), "int.__pow__(): negative exponent: %s", get_str(val_b));
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str(errmsg)));
    }
    if (zz_cmpi(val_b,LONG_MAX) > 0) {
        char errmsg[1024];
        snprintf(errmsg, sizeof(errmsg), "int.__pow__(): exponent out of range (>LONG_MAX):  %s", get_str(val_b));
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str(errmsg)));
    }
    B_int res = malloc_int();
    if (val_b->size == 0)
         zz_seti(&res->val, 1);
     else     
         zz_powi(&res->val,&a->val,val_b->n[0]); // __pow__ should have an int64 exponent in the Acton protocol
    return res;
}

B_int B_IntegralD_intD___neg__(B_IntegralD_int wit,  B_int a) {
    B_int res = malloc_int();
    zz_neg(&res->val,&a->val);
    return res;
}

B_int B_IntegralD_intD___pos__(B_IntegralD_int wit,  B_int a) {
    return a;
}

$WORD B_IntegralD_intD_real(B_IntegralD_int wit, B_int a, B_Real wit2) {
    $RAISE((B_BaseException)$NEW(B_NotImplementedError,to$str("Number.__real__ not implemented for int")));
    return NULL; // This is just to silence compiler warning, above RAISE will longjmp from here anyway
}

$WORD B_IntegralD_intD_imag(B_IntegralD_int wit, B_int a, B_Real wit2) {
    $RAISE((B_BaseException)$NEW(B_NotImplementedError,to$str("Number.__imag__ not implemented for int")));
    return NULL; // This is just to silence compiler warning, above RAISE will longjmp from here anyway
}

$WORD B_IntegralD_intD___abs__(B_IntegralD_int wit, B_int a, B_Real wit2) {
    B_int res = malloc_int();
    zz_set(&res->val,&a->val);
    res->val.size = labs(a->val.size);
    return wit2->$class->__fromatom__(wit2,(B_atom)res);
}

B_int B_IntegralD_intD_conjugate(B_IntegralD_int wit,  B_int a) {
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

B_int B_IntegralD_intD___floordiv__(B_IntegralD_int wit, B_int a, B_int b);

B_int B_IntegralD_intD___round__ (B_IntegralD_int wit, B_int n, B_int p) {
    zz_struct nval = n->val;
    if (nval.size < 0) { 
        B_int n1 = malloc_int();
        zz_neg(&n1->val,&nval);
        B_int res = B_IntegralD_intD___round__(wit,n1,p);
        zz_neg(&res->val,&res->val);
        return res;
    }
    if (labs(p->val.size) >1) {
        char errmsg[1024];
        snprintf(errmsg, sizeof(errmsg), "int.__round__(): precision out of range: %s", get_str(&p->val));
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str(errmsg)));
    }
    long pval = from$int(p);
    if (pval>=0)
        return n;
    B_int p10 = B_IntegralD_intD___pow__(NULL,to$int(10), B_IntegralD_intD___neg__(NULL,p));
    B_int p10half = B_IntegralD_intD___floordiv__(NULL, p10,to$int(2));
    B_int n1 = B_IntegralD_intD___floordiv__(NULL,B_IntegralD_intD___add__(NULL,n,p10half),p10);
    return B_IntegralD_intD___mul__(NULL,n1,p10);
}
  
$WORD B_IntegralD_intD_numerator (B_IntegralD_int wit, B_int n, B_Integral wit2) {
    return wit2->$class->__fromatom__(wit2,(B_atom)n);
}
  
$WORD B_IntegralD_intD_denominator (B_IntegralD_int wit, B_int n, B_Integral wit2) {
    B_int res = to$int(1L);
    return wit2->$class->__fromatom__(wit2,(B_atom)res);
}
  
B_int B_IntegralD_intD___int__ (B_IntegralD_int wit, B_int n) {
    return n;
}

B_int B_IntegralD_intD___index__(B_IntegralD_int wit, B_int n) {
    return n;
}

B_tuple B_IntegralD_intD___divmod__(B_IntegralD_int wit, B_int a, B_int b) {
    if (b->val.size == 0){
        char errmsg[1024];
        snprintf(errmsg, sizeof(errmsg), "integer divmod: divisor is zero");
        $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str(errmsg)));
    }
    B_int q = malloc_int();
    B_int r = malloc_int();
    zz_divrem(&q->val,&r->val,&a->val,&b->val);
    return $NEWTUPLE(2, q, r);
}

B_int B_IntegralD_intD___floordiv__(B_IntegralD_int wit, B_int a, B_int b) {
    if (b->val.size == 0){
        char errmsg[1024];
        snprintf(errmsg, sizeof(errmsg), "integer floordiv: divisor is zero");
        $RAISE((B_BaseException)$NEW(B_ZeroDivisionError,to$str(errmsg)));
    }
     B_int res = malloc_int();
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
    long bval = from$int(b);
    if (ma==0 || bval==0)
        return a;
    if (bval<0) {
        char errmsg[1024];
        snprintf(errmsg, sizeof(errmsg), "int.__lshift__: negative shift count: %ld", bval);
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str(errmsg)));
    }
    long shw = bval/64;
    long shb = bval%64;
    long mres = labs(ma) + shw + (shb > 0);
    B_int res = malloc_int();
    zz_ptr rval = &res->val;
    zz_malloc_fit(rval,mres);
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
    long bval = from$int(b);
    if (ma==0 || bval==0)
        return a;
    if (bval<0)  {
        char errmsg[1024];
        snprintf(errmsg, sizeof(errmsg), "int.__rshift__: negative shift count: %ld", bval);
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str(errmsg)));
    }
    B_int res = malloc_int();
    zz_ptr rval = &res->val;
    long shw = bval/64;
    long shb = bval%64;
    long mres = labs(ma) - shw;
    zz_malloc_fit(rval,mres);
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
    B_int res0 = malloc_int();
    B_int res = malloc_int();
    B_int one = to$int(1);
    zz_neg(&res0->val,&a->val);
    zz_sub(&res->val,&res0->val,&one->val);
    return res;
}


// LogicalB_int  ////////////////////////////////////////////////////////////////////////////////////////

B_int B_LogicalD_IntegralD_intD___and__(B_LogicalD_IntegralD_int wit,  B_int a, B_int b) {
    zz_struct aval = a->val;
    zz_struct bval = b->val;
    B_IntegralD_int wit1 = (B_IntegralD_int)wit->W_Integral;
    if (aval.size>=0) {
        if (bval.size>=0) {
            if (aval.size < bval.size) {
                return  B_LogicalD_IntegralD_intD___and__(wit, b, a);
            } else {
                B_int res = malloc_int();
                zz_malloc_fit(&res->val,bval.size);
                res->val.size = 0;                 
                for (int i=bval.size-1; i>=0; i--) {
                    res->val.n[i] = aval.n[i] & bval.n[i];
                    if (res->val.size == 0 && res->val.n[i] != 0)
                        res->val.size = i+1;
                }
            }
        }
    /* } else if (bval.size>=0) { */
    /*     B_int a1 =  B_IntegralD_intD___invert__(wit1, a); */
    /*     return B_IntegralD_intD___invert__(wit1, B_LogicalD_IntegralD_intD___xor__(wit, a1, b)); */
    /* } else {  */
    /*     B_int a1 =  B_IntegralD_intD___invert__(wit1, a); */
    /*     B_int b1 =  B_IntegralD_intD___invert__(wit1, b); */
    /*     return B_LogicalD_IntegralD_intD___xor__(wit, a1, b1); */
    }
}

                                                 
B_int B_LogicalD_IntegralD_intD___or__(B_LogicalD_IntegralD_int wit,  B_int a, B_int b) {
    // return toB_i64(a->val | b->val);
    $RAISE((B_BaseException)$NEW(B_NotImplementedError,to$str("Protocol Logical not implemented for int; use i64\n")));
    return NULL; // This is just to silence compiler warning, above RAISE will longjmp from here anyway
}
                                                 
B_int B_LogicalD_IntegralD_intD___xor__(B_LogicalD_IntegralD_int wit,  B_int a, B_int b) {
    zz_struct aval = a->val;
    zz_struct bval = b->val;
    B_IntegralD_int wit1 = (B_IntegralD_int)wit->W_Integral;
    if (aval.size>=0) {
        if (bval.size>=0) {
            if (aval.size < bval.size) {
                return  B_LogicalD_IntegralD_intD___xor__(wit, b, a);
            } else {
                B_int res = malloc_int();
                zz_malloc_fit(&res->val,aval.size);
                res->val.size = aval.size > bval.size ? aval.size : 0;
                for (int i=bval.size-1; i>=0; i--) {
                    res->val.n[i] = aval.n[i] ^ bval.n[i];
                    if (res->val.size == 0 && res->val.n[i] != 0)
                        res->val.size = i+1;
                }
                for (int i=bval.size; i<aval.size; i++)
                    res->val.n[i] = aval.n[i];
                return res;
            }
        } else {
            B_int b1 =  B_IntegralD_intD___invert__(wit1, b);
            return B_IntegralD_intD___invert__(wit1, B_LogicalD_IntegralD_intD___xor__(wit, a, b1));
        }
    } else if (bval.size>=0) {
        B_int a1 =  B_IntegralD_intD___invert__(wit1, a);
        return B_IntegralD_intD___invert__(wit1, B_LogicalD_IntegralD_intD___xor__(wit, a1, b));
    } else { 
        B_int a1 =  B_IntegralD_intD___invert__(wit1, a);
        B_int b1 =  B_IntegralD_intD___invert__(wit1, b);
        return B_LogicalD_IntegralD_intD___xor__(wit, a1, b1);
    } 
}
 
// B_MinusD_IntegralD_int  ////////////////////////////////////////////////////////////////////////////////////////

B_int B_MinusD_IntegralD_intD___sub__(B_MinusD_IntegralD_int wit,  B_int a, B_int b) {
    B_int res = malloc_int();
    zz_sub(&res->val,&a->val,&b->val);
    return res;
}


// B_DivD_int  ////////////////////////////////////////////////////////////////////////////////////////

B_float B_DivD_intD___truediv__ (B_DivD_int wit, B_int a, B_int b) {
    if (zz_equal(&b->val, &to$int(0)->val))
        $RAISE((B_BaseException)$NEW(B_ZeroDivisionError, to$str("division by zero")));
    zz_ptr aval = &a->val;
    zz_ptr bval = &b->val;
    B_int ared = malloc_int();
    B_int bred = malloc_int();
    B_int q = malloc_int();
    B_int r = malloc_int();
    B_int g = malloc_int();
    zz_gcd(&g->val,aval,bval);
    zz_div(&ared->val,aval,&g->val);
    zz_div(&bred->val,bval,&g->val);
    zz_divrem(&q->val,&r->val,&ared->val,&bred->val);
    return to$float(B_floatG_new((B_atom)q)->val +  B_floatG_new((B_atom)r)->val/ B_floatG_new((B_atom)bred)->val);
}

// B_OrdD_int  ////////////////////////////////////////////////////////////////////////////////////////

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

B_bool B_HashableD_intD___eq__(B_HashableD_int wit, B_int a, B_int b) {
    return toB_bool(zz_equal(&a->val,&b->val));
}

B_bool B_HashableD_intD___ne__(B_HashableD_int wit, B_int a, B_int b) {
    return toB_bool(1-zz_equal(&a->val,&b->val));
}

B_int B_HashableD_intD___hash__(B_HashableD_int wit, B_int a) {
    long sz = a->val.size;
    if (sz==0) return to$int(sz);
    unsigned long res = a->val.n[0];
    if (res > LONG_MAX || labs(sz) > 1) 
        return to$int(B_i64D_hash(toB_i64((long)res & LONG_MAX)));
    else
        return to$int(B_i64D_hash(toB_i64(sz<0 ? -res : res)));
}
 
long from$int(B_int n) { 
    long sz = n->val.size;
    if (sz==0) return 0;
    unsigned long res = n->val.n[0];
    if (res > LONG_MAX || labs(sz) > 1) {
        fprintf(stderr,"internal error: overflow in converting int to bounded int\n");
        exit(1);
    }
    return sz<0 ? -res : res;
}
            
B_int to$int(long n) {
    if (n >= 0 && n < 256)
        return &B_int_strs[n];
    else {
        B_int res = malloc_int();
        res->val.n[0] = n > 0 ? n : (n == LONG_MIN ? 9223372036854775808 : -n);
        res->val.size = n < 0 ? -1 : n > 0;
        return res;
    }
}

B_int to$int2(char *str) {
    B_int res = malloc_int();
    res->$class = &B_intG_methods;
    set_str(&res->val, str, NULL);
    return res;
}


// Conversion to strings /////////////////////////////////////////////////////////////////////////////

// These four constants must be changed for a 32 bit machine
int WORDSIZE = 64;
int POW10INWORD = 18; // Largest power of 10 that fits in a signed long 
unsigned char POWINWORD[37] = {0,0,62,39,31,27,24,22,20,19,18,18,17,17,16,16,15,15,15,14,14,14,14,13,13,13,13,13,13,12,12,12,12,12,12,12,12}
; //POWINWORD[n] is largest power of b that fits in a signed word
double CCCC = 9.805415291306852e-2;  // log2(WORDSIZE) - log2 (POW10INWORD) - log2 (log2(10))

// n is a valid digit in base b iff digvalue[n] < b.
unsigned char digvalue[256] = {
    99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
     0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 99, 99, 99, 99, 99, 99,
    99, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
    25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 99, 99, 99, 99, 99,
    99, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
    25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
};


int get_str0(bool ishead, zz_ptr n, zz_ptr dens[], int d, char *res, int pos) {
    if (d >= 0) {
        zz_ptr hi = acton_malloc(sizeof(zz_struct));
        zz_ptr lo = acton_malloc(sizeof(zz_struct));
        zz_init_fit(hi,dens[d]->size);
        zz_init_fit(lo,dens[d]->size);
        zz_divrem(hi, lo, n, dens[d]);
        if (hi->size==0 && ishead) {
            return get_str0(ishead, lo, dens, d-1, res, pos);
        } else {
            int newpos = get_str0(ishead, hi, dens, d-1, res, pos);
            return get_str0(false, lo, dens, d-1, res, newpos);
        }
    } else {
        char buf[POW10INWORD + 1];
        sprintf(&buf, "%lu", (unsigned long)n->n[0]);
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


char * get_str(zz_ptr nval) {
    if (nval->size == 0)
        return "0";
    long nlen = BSDNT_ABS(nval->size);
    zz_ptr npos = acton_malloc(sizeof(zz_struct));
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
        dens = acton_malloc(d * sizeof(zz_ptr));
        dens[0] = acton_malloc(sizeof(zz_struct));
        zz_init_fit(dens[0], 1);
        zz_seti(dens[0], 10); 
        zz_powi(dens[0], dens[0], POW10INWORD);
        for (int i=1; i < d; i++) {
            dens[i] = acton_malloc(sizeof(zz_struct));
            zz_init_fit(dens[i], 2 * dens[i-1]->size);
            zz_mul(dens[i], dens[i-1], dens[i-1]);
        }
    }
    // strlen is for most n one more than necessary; this is a precaution for values of n where
    // the double value ... in ceil(...) is very close to an integer. So we often waste one byte.
    int strlen = ceil(log10((float)npos->n[nlen - 1]) + (nlen - 1) * WORD_BITS * log10(2) + is_neg_n) + 2;
    char *res = acton_malloc_atomic(strlen);
    memset(res,'0', strlen);
    int pos = 0;
    if (is_neg_n) {
        res[0] = '-';
        pos++;
    }
    int newpos = get_str0(true, npos, dens, d-1, res, pos);
    res[newpos] = '\0';
    return res;
}


int set_str0(zz_ptr a, char *nstr, unsigned char base, int parts) {
    // assert(parts > 0);
    if (parts == 1) {
        unsigned long val = 0;
        int i = 0;
        while (i < POWINWORD[base])
            val = val * base + digvalue[nstr[i++]];
        zz_seti(a, val);
        return POWINWORD[base];
    } else {
        int hi = parts/2;
        int lo = parts - hi;
        zz_ptr hires = acton_malloc(sizeof(zz_struct));
        zz_ptr lores = acton_malloc(sizeof(zz_struct));
        zz_init(hires);
        zz_init(lores);
        int hidigs = set_str0(hires, nstr, base, hi);
        int lodigs = set_str0(lores, &nstr[hi * POWINWORD[base]], base, lo);
        zz_seti(a, base);
        zz_powi(a, a, POWINWORD[base] * lo);
        zz_mul(a, a, hires);
        zz_add(a, a, lores);
        return hidigs + lodigs;
    }
}


int set_str(zz_ptr a, unsigned char *nstr, B_int intbase) {
    int pre = 0;
    int sgn = 1;
    while(isspace(nstr[pre])) pre++;   // should leading spaces be allowed?
    if(nstr[pre]=='+')
        pre++;
    else if (nstr[pre]=='-') {
        sgn = -1;
        pre++;
    }
    int len = 0;
    int pre_len = pre;
    unsigned char basefromstr = 0;
    if (nstr[pre]=='0') {
        pre++; 
        if (nstr[pre]=='x' || nstr[pre]=='X') {
            basefromstr = 16; pre++; pre_len += 2;
        } else if (nstr[pre]=='o' || nstr[pre]=='O') {
            basefromstr = 8; pre++; pre_len += 2;
        } else if (nstr[pre]=='b' || nstr[pre]=='B') {
            basefromstr = 2; pre++; pre_len += 2;
        } else
            len++;
    }
    unsigned char basefrompar = 0;
    if (!intbase)
        basefrompar = 0;
    else {
        long baseval = from$int(intbase);
        if (baseval < 2 || baseval > 36) {
            char errmsg[1024];
            snprintf(errmsg, sizeof(errmsg), "integer type constructor: base parameter %ld is out of range (must be between 2 and 36, inclusive)", baseval);
            $RAISE((B_BaseException)$NEW(B_ValueError,to$str(errmsg)));
        } else
            basefrompar = (unsigned char)baseval;
    }
    unsigned char base;
    if (basefrompar==0) 
        base = basefromstr ? basefromstr : 10;
    else if (basefromstr==0) 
        base = basefrompar;
    else if (basefromstr != basefrompar) {
        char errmsg[1024];
        snprintf(errmsg, sizeof(errmsg), "integer type constructor: base specified in str (%d) is in conflict with base in parameter base (%d)", basefromstr, basefrompar);
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str(errmsg)));
    } else
        base = basefromstr; // which is equal to basefrompar
    while (digvalue[nstr[pre]] < base) {
        len++;
        pre++;
    }
    if (len == 0 || nstr[pre] != 0) {
        char errmsg[1024];
        snprintf(errmsg, sizeof(errmsg), "integer type constructor: string \"%s\" cannot be interpreted as an int in base %d", nstr,base);
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str(errmsg)));
    }
    nstr += pre_len;
    
    int parts = len / POWINWORD[base];
    int offset =  len % POWINWORD[base];
    
    if (offset == 0) {
        return set_str0(a, nstr, base, parts);
        a->size *= sgn;
    } else {
        unsigned long headval = 0;
        int partdigits = 0;
        int i = 0;
        while (i < offset)
            headval = headval * base + digvalue[nstr[i++]];
        if (parts > 0) {
            zz_ptr res0 = acton_malloc(sizeof(zz_struct));
            zz_init(res0);
            partdigits = set_str0(res0, &nstr[offset], base, parts);
            zz_seti(a, base);
            zz_powi(a, a, POWINWORD[base] * parts);
            zz_muli(a, a, headval);
            zz_add(a, a, res0);
        } else {
            zz_seti(a, headval);
        }
        a->size *= sgn;
        return pre; // we shouldn't return chars consumed since we throw exception if whole string not consumed.
    } 
}


// gcd functions from BSDNT //////////////////////////////////
B_int $gcd(B_int a, B_int b) {
    B_int res = malloc_int();
    zz_gcd(&res->val, &a->val, &b->val);
    return res;
}

B_tuple $xgcd(B_int a, B_int b) {
    B_int d = malloc_int();
    B_int s = malloc_int();
    B_int t = malloc_int();
    zz_xgcd(&d->val, &s->val, &t->val, &a->val, &b->val);
    return $NEWTUPLE(3, d, s, t);
}
    
unsigned long B_int_longs[256] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
                                   10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
                                   20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
                                   30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
                                   40, 41, 42, 43, 44, 45, 46, 47, 48, 49,
                                   50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
                                   60, 61, 62, 63, 64, 65, 66, 67, 68, 69,
                                   70, 71, 72, 73, 74, 75, 76, 77, 78, 79,
                                   80, 81, 82, 83, 84, 85, 86, 87, 88, 89,
                                   90, 91, 92, 93, 94, 95, 96, 97, 98, 99,
                                   100, 101, 102, 103, 104, 105, 106, 107, 108, 109,
                                   110, 111, 112, 113, 114, 115, 116, 117, 118, 119,
                                   120, 121, 122, 123, 124, 125, 126, 127, 128, 129,
                                   130, 131, 132, 133, 134, 135, 136, 137, 138, 139,
                                   140, 141, 142, 143, 144, 145, 146, 147, 148, 149,
                                   150, 151, 152, 153, 154, 155, 156, 157, 158, 159,
                                   160, 161, 162, 163, 164, 165, 166, 167, 168, 169,
                                   170, 171, 172, 173, 174, 175, 176, 177, 178, 179,
                                   180, 181, 182, 183, 184, 185, 186, 187, 188, 189,
                                   190, 191, 192, 193, 194, 195, 196, 197, 198, 199,
                                   200, 201, 202, 203, 204, 205, 206, 207, 208, 209,
                                   210, 211, 212, 213, 214, 215, 216, 217, 218, 219,
                                   220, 221, 222, 223, 224, 225, 226, 227, 228, 229,
                                   230, 231, 232, 233, 234, 235, 236, 237, 238, 239,
                                   240, 241, 242, 243, 244, 245, 246, 247, 248, 249,
                                   250, 251, 252, 253, 254, 255 };

                           
struct B_int B_int_strs[256] =
                        {{&B_intG_methods, B_int_longs, 0, 1},
                         {&B_intG_methods, B_int_longs+1, 1, 1},
                         {&B_intG_methods, B_int_longs+2, 1, 1},
                         {&B_intG_methods, B_int_longs+3, 1, 1},
                         {&B_intG_methods, B_int_longs+4, 1, 1},
                         {&B_intG_methods, B_int_longs+5, 1, 1},
                         {&B_intG_methods, B_int_longs+6, 1, 1},
                         {&B_intG_methods, B_int_longs+7, 1, 1},
                         {&B_intG_methods, B_int_longs+8, 1, 1},
                         {&B_intG_methods, B_int_longs+9, 1, 1},
                         {&B_intG_methods, B_int_longs+10, 1, 1},
                         {&B_intG_methods, B_int_longs+11, 1, 1},
                         {&B_intG_methods, B_int_longs+12, 1, 1},
                         {&B_intG_methods, B_int_longs+13, 1, 1},
                         {&B_intG_methods, B_int_longs+14, 1, 1},
                         {&B_intG_methods, B_int_longs+15, 1, 1},
                         {&B_intG_methods, B_int_longs+16, 1, 1},
                         {&B_intG_methods, B_int_longs+17, 1, 1},
                         {&B_intG_methods, B_int_longs+18, 1, 1},
                         {&B_intG_methods, B_int_longs+19, 1, 1},
                         {&B_intG_methods, B_int_longs+20, 1, 1},
                         {&B_intG_methods, B_int_longs+21, 1, 1},
                         {&B_intG_methods, B_int_longs+22, 1, 1},
                         {&B_intG_methods, B_int_longs+23, 1, 1},
                         {&B_intG_methods, B_int_longs+24, 1, 1},
                         {&B_intG_methods, B_int_longs+25, 1, 1},
                         {&B_intG_methods, B_int_longs+26, 1, 1},
                         {&B_intG_methods, B_int_longs+27, 1, 1},
                         {&B_intG_methods, B_int_longs+28, 1, 1},
                         {&B_intG_methods, B_int_longs+29, 1, 1},
                         {&B_intG_methods, B_int_longs+30, 1, 1},
                         {&B_intG_methods, B_int_longs+31, 1, 1},
                         {&B_intG_methods, B_int_longs+32, 1, 1},
                         {&B_intG_methods, B_int_longs+33, 1, 1},
                         {&B_intG_methods, B_int_longs+34, 1, 1},
                         {&B_intG_methods, B_int_longs+35, 1, 1},
                         {&B_intG_methods, B_int_longs+36, 1, 1},
                         {&B_intG_methods, B_int_longs+37, 1, 1},
                         {&B_intG_methods, B_int_longs+38, 1, 1},
                         {&B_intG_methods, B_int_longs+39, 1, 1},
                         {&B_intG_methods, B_int_longs+40, 1, 1},
                         {&B_intG_methods, B_int_longs+41, 1, 1},
                         {&B_intG_methods, B_int_longs+42, 1, 1},
                         {&B_intG_methods, B_int_longs+43, 1, 1},
                         {&B_intG_methods, B_int_longs+44, 1, 1},
                         {&B_intG_methods, B_int_longs+45, 1, 1},
                         {&B_intG_methods, B_int_longs+46, 1, 1},
                         {&B_intG_methods, B_int_longs+47, 1, 1},
                         {&B_intG_methods, B_int_longs+48, 1, 1},
                         {&B_intG_methods, B_int_longs+49, 1, 1},
                         {&B_intG_methods, B_int_longs+50, 1, 1},
                         {&B_intG_methods, B_int_longs+51, 1, 1},
                         {&B_intG_methods, B_int_longs+52, 1, 1},
                         {&B_intG_methods, B_int_longs+53, 1, 1},
                         {&B_intG_methods, B_int_longs+54, 1, 1},
                         {&B_intG_methods, B_int_longs+55, 1, 1},
                         {&B_intG_methods, B_int_longs+56, 1, 1},
                         {&B_intG_methods, B_int_longs+57, 1, 1},
                         {&B_intG_methods, B_int_longs+58, 1, 1},
                         {&B_intG_methods, B_int_longs+59, 1, 1},
                         {&B_intG_methods, B_int_longs+60, 1, 1},
                         {&B_intG_methods, B_int_longs+61, 1, 1},
                         {&B_intG_methods, B_int_longs+62, 1, 1},
                         {&B_intG_methods, B_int_longs+63, 1, 1},
                         {&B_intG_methods, B_int_longs+64, 1, 1},
                         {&B_intG_methods, B_int_longs+65, 1, 1},
                         {&B_intG_methods, B_int_longs+66, 1, 1},
                         {&B_intG_methods, B_int_longs+67, 1, 1},
                         {&B_intG_methods, B_int_longs+68, 1, 1},
                         {&B_intG_methods, B_int_longs+69, 1, 1},
                         {&B_intG_methods, B_int_longs+70, 1, 1},
                         {&B_intG_methods, B_int_longs+71, 1, 1},
                         {&B_intG_methods, B_int_longs+72, 1, 1},
                         {&B_intG_methods, B_int_longs+73, 1, 1},
                         {&B_intG_methods, B_int_longs+74, 1, 1},
                         {&B_intG_methods, B_int_longs+75, 1, 1},
                         {&B_intG_methods, B_int_longs+76, 1, 1},
                         {&B_intG_methods, B_int_longs+77, 1, 1},
                         {&B_intG_methods, B_int_longs+78, 1, 1},
                         {&B_intG_methods, B_int_longs+79, 1, 1},
                         {&B_intG_methods, B_int_longs+80, 1, 1},
                         {&B_intG_methods, B_int_longs+81, 1, 1},
                         {&B_intG_methods, B_int_longs+82, 1, 1},
                         {&B_intG_methods, B_int_longs+83, 1, 1},
                         {&B_intG_methods, B_int_longs+84, 1, 1},
                         {&B_intG_methods, B_int_longs+85, 1, 1},
                         {&B_intG_methods, B_int_longs+86, 1, 1},
                         {&B_intG_methods, B_int_longs+87, 1, 1},
                         {&B_intG_methods, B_int_longs+88, 1, 1},
                         {&B_intG_methods, B_int_longs+89, 1, 1},
                         {&B_intG_methods, B_int_longs+90, 1, 1},
                         {&B_intG_methods, B_int_longs+91, 1, 1},
                         {&B_intG_methods, B_int_longs+92, 1, 1},
                         {&B_intG_methods, B_int_longs+93, 1, 1},
                         {&B_intG_methods, B_int_longs+94, 1, 1},
                         {&B_intG_methods, B_int_longs+95, 1, 1},
                         {&B_intG_methods, B_int_longs+96, 1, 1},
                         {&B_intG_methods, B_int_longs+97, 1, 1},
                         {&B_intG_methods, B_int_longs+98, 1, 1},
                         {&B_intG_methods, B_int_longs+99, 1, 1},
                         {&B_intG_methods, B_int_longs+100, 1, 1},
                         {&B_intG_methods, B_int_longs+101, 1, 1},
                         {&B_intG_methods, B_int_longs+102, 1, 1},
                         {&B_intG_methods, B_int_longs+103, 1, 1},
                         {&B_intG_methods, B_int_longs+104, 1, 1},
                         {&B_intG_methods, B_int_longs+105, 1, 1},
                         {&B_intG_methods, B_int_longs+106, 1, 1},
                         {&B_intG_methods, B_int_longs+107, 1, 1},
                         {&B_intG_methods, B_int_longs+108, 1, 1},
                         {&B_intG_methods, B_int_longs+109, 1, 1},
                         {&B_intG_methods, B_int_longs+110, 1, 1},
                         {&B_intG_methods, B_int_longs+111, 1, 1},
                         {&B_intG_methods, B_int_longs+112, 1, 1},
                         {&B_intG_methods, B_int_longs+113, 1, 1},
                         {&B_intG_methods, B_int_longs+114, 1, 1},
                         {&B_intG_methods, B_int_longs+115, 1, 1},
                         {&B_intG_methods, B_int_longs+116, 1, 1},
                         {&B_intG_methods, B_int_longs+117, 1, 1},
                         {&B_intG_methods, B_int_longs+118, 1, 1},
                         {&B_intG_methods, B_int_longs+119, 1, 1},
                         {&B_intG_methods, B_int_longs+120, 1, 1},
                         {&B_intG_methods, B_int_longs+121, 1, 1},
                         {&B_intG_methods, B_int_longs+122, 1, 1},
                         {&B_intG_methods, B_int_longs+123, 1, 1},
                         {&B_intG_methods, B_int_longs+124, 1, 1},
                         {&B_intG_methods, B_int_longs+125, 1, 1},
                         {&B_intG_methods, B_int_longs+126, 1, 1},
                         {&B_intG_methods, B_int_longs+127, 1, 1},
                         {&B_intG_methods, B_int_longs+128, 1, 1},
                         {&B_intG_methods, B_int_longs+129, 1, 1},
                         {&B_intG_methods, B_int_longs+130, 1, 1},
                         {&B_intG_methods, B_int_longs+131, 1, 1},
                         {&B_intG_methods, B_int_longs+132, 1, 1},
                         {&B_intG_methods, B_int_longs+133, 1, 1},
                         {&B_intG_methods, B_int_longs+134, 1, 1},
                         {&B_intG_methods, B_int_longs+135, 1, 1},
                         {&B_intG_methods, B_int_longs+136, 1, 1},
                         {&B_intG_methods, B_int_longs+137, 1, 1},
                         {&B_intG_methods, B_int_longs+138, 1, 1},
                         {&B_intG_methods, B_int_longs+139, 1, 1},
                         {&B_intG_methods, B_int_longs+140, 1, 1},
                         {&B_intG_methods, B_int_longs+141, 1, 1},
                         {&B_intG_methods, B_int_longs+142, 1, 1},
                         {&B_intG_methods, B_int_longs+143, 1, 1},
                         {&B_intG_methods, B_int_longs+144, 1, 1},
                         {&B_intG_methods, B_int_longs+145, 1, 1},
                         {&B_intG_methods, B_int_longs+146, 1, 1},
                         {&B_intG_methods, B_int_longs+147, 1, 1},
                         {&B_intG_methods, B_int_longs+148, 1, 1},
                         {&B_intG_methods, B_int_longs+149, 1, 1},
                         {&B_intG_methods, B_int_longs+150, 1, 1},
                         {&B_intG_methods, B_int_longs+151, 1, 1},
                         {&B_intG_methods, B_int_longs+152, 1, 1},
                         {&B_intG_methods, B_int_longs+153, 1, 1},
                         {&B_intG_methods, B_int_longs+154, 1, 1},
                         {&B_intG_methods, B_int_longs+155, 1, 1},
                         {&B_intG_methods, B_int_longs+156, 1, 1},
                         {&B_intG_methods, B_int_longs+157, 1, 1},
                         {&B_intG_methods, B_int_longs+158, 1, 1},
                         {&B_intG_methods, B_int_longs+159, 1, 1},
                         {&B_intG_methods, B_int_longs+160, 1, 1},
                         {&B_intG_methods, B_int_longs+161, 1, 1},
                         {&B_intG_methods, B_int_longs+162, 1, 1},
                         {&B_intG_methods, B_int_longs+163, 1, 1},
                         {&B_intG_methods, B_int_longs+164, 1, 1},
                         {&B_intG_methods, B_int_longs+165, 1, 1},
                         {&B_intG_methods, B_int_longs+166, 1, 1},
                         {&B_intG_methods, B_int_longs+167, 1, 1},
                         {&B_intG_methods, B_int_longs+168, 1, 1},
                         {&B_intG_methods, B_int_longs+169, 1, 1},
                         {&B_intG_methods, B_int_longs+170, 1, 1},
                         {&B_intG_methods, B_int_longs+171, 1, 1},
                         {&B_intG_methods, B_int_longs+172, 1, 1},
                         {&B_intG_methods, B_int_longs+173, 1, 1},
                         {&B_intG_methods, B_int_longs+174, 1, 1},
                         {&B_intG_methods, B_int_longs+175, 1, 1},
                         {&B_intG_methods, B_int_longs+176, 1, 1},
                         {&B_intG_methods, B_int_longs+177, 1, 1},
                         {&B_intG_methods, B_int_longs+178, 1, 1},
                         {&B_intG_methods, B_int_longs+179, 1, 1},
                         {&B_intG_methods, B_int_longs+180, 1, 1},
                         {&B_intG_methods, B_int_longs+181, 1, 1},
                         {&B_intG_methods, B_int_longs+182, 1, 1},
                         {&B_intG_methods, B_int_longs+183, 1, 1},
                         {&B_intG_methods, B_int_longs+184, 1, 1},
                         {&B_intG_methods, B_int_longs+185, 1, 1},
                         {&B_intG_methods, B_int_longs+186, 1, 1},
                         {&B_intG_methods, B_int_longs+187, 1, 1},
                         {&B_intG_methods, B_int_longs+188, 1, 1},
                         {&B_intG_methods, B_int_longs+189, 1, 1},
                         {&B_intG_methods, B_int_longs+190, 1, 1},
                         {&B_intG_methods, B_int_longs+191, 1, 1},
                         {&B_intG_methods, B_int_longs+192, 1, 1},
                         {&B_intG_methods, B_int_longs+193, 1, 1},
                         {&B_intG_methods, B_int_longs+194, 1, 1},
                         {&B_intG_methods, B_int_longs+195, 1, 1},
                         {&B_intG_methods, B_int_longs+196, 1, 1},
                         {&B_intG_methods, B_int_longs+197, 1, 1},
                         {&B_intG_methods, B_int_longs+198, 1, 1},
                         {&B_intG_methods, B_int_longs+199, 1, 1},
                         {&B_intG_methods, B_int_longs+200, 1, 1},
                         {&B_intG_methods, B_int_longs+201, 1, 1},
                         {&B_intG_methods, B_int_longs+202, 1, 1},
                         {&B_intG_methods, B_int_longs+203, 1, 1},
                         {&B_intG_methods, B_int_longs+204, 1, 1},
                         {&B_intG_methods, B_int_longs+205, 1, 1},
                         {&B_intG_methods, B_int_longs+206, 1, 1},
                         {&B_intG_methods, B_int_longs+207, 1, 1},
                         {&B_intG_methods, B_int_longs+208, 1, 1},
                         {&B_intG_methods, B_int_longs+209, 1, 1},
                         {&B_intG_methods, B_int_longs+210, 1, 1},
                         {&B_intG_methods, B_int_longs+211, 1, 1},
                         {&B_intG_methods, B_int_longs+212, 1, 1},
                         {&B_intG_methods, B_int_longs+213, 1, 1},
                         {&B_intG_methods, B_int_longs+214, 1, 1},
                         {&B_intG_methods, B_int_longs+215, 1, 1},
                         {&B_intG_methods, B_int_longs+216, 1, 1},
                         {&B_intG_methods, B_int_longs+217, 1, 1},
                         {&B_intG_methods, B_int_longs+218, 1, 1},
                         {&B_intG_methods, B_int_longs+219, 1, 1},
                         {&B_intG_methods, B_int_longs+220, 1, 1},
                         {&B_intG_methods, B_int_longs+221, 1, 1},
                         {&B_intG_methods, B_int_longs+222, 1, 1},
                         {&B_intG_methods, B_int_longs+223, 1, 1},
                         {&B_intG_methods, B_int_longs+224, 1, 1},
                         {&B_intG_methods, B_int_longs+225, 1, 1},
                         {&B_intG_methods, B_int_longs+226, 1, 1},
                         {&B_intG_methods, B_int_longs+227, 1, 1},
                         {&B_intG_methods, B_int_longs+228, 1, 1},
                         {&B_intG_methods, B_int_longs+229, 1, 1},
                         {&B_intG_methods, B_int_longs+230, 1, 1},
                         {&B_intG_methods, B_int_longs+231, 1, 1},
                         {&B_intG_methods, B_int_longs+232, 1, 1},
                         {&B_intG_methods, B_int_longs+233, 1, 1},
                         {&B_intG_methods, B_int_longs+234, 1, 1},
                         {&B_intG_methods, B_int_longs+235, 1, 1},
                         {&B_intG_methods, B_int_longs+236, 1, 1},
                         {&B_intG_methods, B_int_longs+237, 1, 1},
                         {&B_intG_methods, B_int_longs+238, 1, 1},
                         {&B_intG_methods, B_int_longs+239, 1, 1},
                         {&B_intG_methods, B_int_longs+240, 1, 1},
                         {&B_intG_methods, B_int_longs+241, 1, 1},
                         {&B_intG_methods, B_int_longs+242, 1, 1},
                         {&B_intG_methods, B_int_longs+243, 1, 1},
                         {&B_intG_methods, B_int_longs+244, 1, 1},
                         {&B_intG_methods, B_int_longs+245, 1, 1},
                         {&B_intG_methods, B_int_longs+246, 1, 1},
                         {&B_intG_methods, B_int_longs+247, 1, 1},
                         {&B_intG_methods, B_int_longs+248, 1, 1},
                         {&B_intG_methods, B_int_longs+249, 1, 1},
                         {&B_intG_methods, B_int_longs+250, 1, 1},
                         {&B_intG_methods, B_int_longs+251, 1, 1},
                         {&B_intG_methods, B_int_longs+252, 1, 1},
                         {&B_intG_methods, B_int_longs+253, 1, 1},
                         {&B_intG_methods, B_int_longs+254, 1, 1},
                         {&B_intG_methods, B_int_longs+255, 1, 1}};


