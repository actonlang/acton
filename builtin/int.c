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

int $set$str(zz_ptr a, char *str);
char *$get$str(zz_ptr n);

$int $malloc$int() {
    $int res = malloc(sizeof(struct $int));
    res->$class = &$int$methods;
    res->val.n = malloc(sizeof(unsigned long));
    res->val.size = 0;
    res->val.alloc = 1;
    return res;
}


$int $int$new($atom a) {
    if ($ISINSTANCE(a,$int)->val) return ($int)a;
    if ($ISINSTANCE(a,$i64)->val) {
        return to$int((($i64)a)->val);
    }
    if ($ISINSTANCE(a,$float)->val) {
        double aval = (($float)a)->val;
        int e;
        double m = frexp(aval,&e);
        if (e>52) {
            $int c = to$int((long)(m*4503599627370496.0)); // (1<< 52); 
            $int d = to$int(e-52);
            return  $Integral$int$__lshift__(NULL,c,d);
        } else {
            long al = (long)aval;
            $int res = to$int(al);
            return res;
        }
    }
    if ($ISINSTANCE(a,$bool)->val) return to$int((($bool)a)->val);
    if ($ISINSTANCE(a,$str)->val) {
        $int res = $malloc$int();
        res->$class = &$int$methods;
        int digits = $set$str(&res->val, (char *)(($str)a)->str);
        if (digits>0)
            return res;
        else 
            $RAISE(($BaseException)$NEW($ValueError,to$str("int(): string arg has no digit in prefix")));
    }
    fprintf(stderr,"internal error: $int$new: argument not of atomic type\n");
    exit(-1);
}

void $int_init($int self, $atom a){
    self->val = $int$new(a)->val;
}

void $int_serialize($int self,$Serial$state state) {
    $int prevkey = ($int)$dict_get(state->done,($Hashable)$Hashable$WORD$witness,self,NULL);
    if (prevkey) {
        long pk = from$int(prevkey);
        $val_serialize(-INT_ID,&pk,state);
        return;
    }
    int blobsize = 1 + labs(self->val.size);
    $ROW row = $add_header(INT_ID,blobsize,state);
    row->blob[0] = ($WORD)self->val.size;
    memcpy(&row->blob[1],self->val.n,labs(self->val.size)*sizeof(long));
}

$int $int_deserialize($int res,$Serial$state state) {
    $ROW this = state->row;
    state->row = this->next;
    state->row_no++;
    if (this->class_id < 0) {
        return ($int)$dict_get(state->done,($Hashable)$Hashable$int$witness,to$int((int)this->blob[0]),NULL);
    } else {
        if (!res)
            res = $malloc$int();
        res->val.size = (long)this->blob[0];
        res->val.alloc = labs(res->val.size);
        res->val.n = malloc(res->val.alloc*sizeof(long));
        memcpy(res->val.n,&this->blob[1],res->val.alloc*sizeof(long));
        $dict_setitem(state->done,($Hashable)$Hashable$int$witness,to$int(state->row_no-1),res);
        res->$class = &$int$methods;
        return res;
    }
}

$bool $int_bool($int n) {
    return to$bool(zz_cmpi(&n->val,0));
}

$str $int_str($int n) {
    return to$str($get$str(&n->val));
}
  
struct $int$class $int$methods = {
    "$int",
    UNASSIGNED,
    ($Super$class)&$atom$methods,
    $int_init,
    $int_serialize,
    $int_deserialize,
    $int_bool,
    $int_str,
    $int_str
};

$int zz$to$int(zz_ptr n) {
    $int res = $malloc$int();
    res->$class = &$int$methods;
    res->val.n = n->n;
    res->val.size = n->size;
    res->val.alloc = n->alloc;
    return res;
}

// $Integral$int /////////////////////////////////////////////////////////////////////////

void $Integral$int$__serialize__($Integral$int self, $Serial$state state) {
    $step_serialize(self->w$Logical, state);
    $step_serialize(self->w$Minus, state);
}

$Integral$int $Integral$int$__deserialize__($Integral$int self, $Serial$state state) {
    $Integral$int res = $DNEW($Integral$int,state);
    res->w$Logical = ($Logical)$step_deserialize(state);
    res->w$Minus = ($Minus)$step_deserialize(state);
    return res;
}

$int $Integral$int$__add__($Integral$int wit,  $int a, $int b) {
    $int res = $malloc$int();
    zz_add(&res->val,&a->val,&b->val);
    return res;
}

$complex $Integral$int$__complx__($Integral$int wit, $int a) {
    fprintf(stderr,"Number.__complex__ not implemented for int");
    exit(1);
}

$int $Integral$int$__fromatom__($Integral$int wit, $atom a) {
    return $int$new(a);
}

$int $Integral$int$__mul__($Integral$int wit,  $int a, $int b) {
    $int res = $malloc$int();
    zz_mul(&res->val,&a->val,&b->val);
    return res;
}  
  
$int $Integral$int$__pow__($Integral$int wit, $int a, $int b) {
    zz_ptr val_b = &b->val;
    if (zz_cmpi(val_b,0) < 0)
        $RAISE(($BaseException)$NEW($ValueError,to$str("__pow__: exponent negative")));
    if (zz_cmpi(val_b,LONG_MAX) > 0)
        $RAISE(($BaseException)$NEW($ValueError,to$str("__pow__: exponent out of range (> LONG_MAX)")));
    $int res = $malloc$int();
    zz_powi(&res->val,&a->val,val_b->n[0]); // __pow__ should have an int64 exponent in the Acton protocol
    return res;
}

$int $Integral$int$__neg__($Integral$int wit,  $int a) {
    $int res = $malloc$int();
    zz_neg(&res->val,&a->val);
    return res;
}

$int $Integral$int$__pos__($Integral$int wit,  $int a) {
    return a;
}

$WORD $Integral$int$real($Integral$int wit, $int a, $Real wit2) {
    fprintf(stderr,"Number.__real__ not implemented for int");
    exit(1);
}

$WORD $Integral$int$imag($Integral$int wit, $int a, $Real wit2) {
    fprintf(stderr,"Number.__imag__ not implemented for int");
    exit(1);
}

$WORD $Integral$int$__abs__($Integral$int wit, $int a, $Real wit2) {
    $int res = $malloc$int();
    zz_set(&res->val,&a->val);
    res->val.size = labs(a->val.size);
    return wit2->$class->__fromatom__(wit2,($atom)res);
}

$int $Integral$int$__conjugate__($Integral$int wit,  $int a) {
    return a;
}

$float $Integral$int$__float__ ($Integral$int wit, $int n) {
    return $float$new(($atom)n);
}

$WORD $Integral$int$__trunc__ ($Integral$int wit, $int n, $Integral wit2) {
    return wit2->$class->__fromatom__(wit2,($atom)n);
}
  
$WORD $Integral$int$__floor__ ($Integral$int wit, $int n, $Integral wit2) {
    return wit2->$class->__fromatom__(wit2,($atom)n);
}
  
$WORD $Integral$int$__ceil__ ($Integral$int wit, $int n, $Integral wit2) {
    return wit2->$class->__fromatom__(wit2,($atom)n);
}
  
$int $Integral$int$__round__ ($Integral$int wit, $int n, $int p) {
    zz_struct nval = n->val;
    if (nval.size < 0) {
        $int n1 = $malloc$int();
        zz_neg(&n1->val,&nval);
        $int res = $Integral$int$__round__(wit,n1,p);
        zz_neg(&res->val,&res->val);
        return res;
    }
    if (labs(p->val.size) >1)
        $RAISE(($BaseException)$NEW($ValueError,to$str("__round__: precision out of range")));
    long pval = from$int(p);
    if (pval>=0)
        return n;
    $int p10 = $Integral$int$__pow__(NULL,to$int(10), $Integral$int$__neg__(NULL,p));
    return $Integral$int$__mul__(NULL,n,p10);
}
  
$WORD $Integral$int$numerator ($Integral$int wit, $int n, $Integral wit2) {
    return wit2->$class->__fromatom__(wit2,($atom)n);
}
  
$WORD $Integral$int$denominator ($Integral$int wit, $int n, $Integral wit2) {
    $int res = to$int(1L);
    return wit2->$class->__fromatom__(wit2,($atom)res);
}
  
$int $Integral$int$__int__ ($Integral$int wit, $int n) {
    return n;
}

$int $Integral$int$__index__($Integral$int wit, $int n) {
    return n;
}

$tuple $Integral$int$__divmod__($Integral$int wit, $int a, $int b) {
    $int q = $malloc$int();
    $int r = $malloc$int();
    zz_divrem(&q->val,&r->val,&a->val,&b->val);
    return $NEWTUPLE(2, q, r);
}

$int $Integral$int$__floordiv__($Integral$int wit, $int a, $int b) {
    $int res = $malloc$int();
    zz_div(&res->val,&a->val,&b->val);
    return res;
}

$int $Integral$int$__mod__($Integral$int wit, $int a, $int b) {
    $tuple t = $Integral$int$__divmod__(wit,a,b);
    return t->components[1];
}

$int $Integral$int$__lshift__($Integral$int wit,  $int a, $int b) {
    zz_struct aval = a->val;
    long ma = aval.size;
    long bval = from$int(b);
    if (ma==0 || bval==0)
        return a;
    if (bval<0)
        $RAISE(($BaseException)$NEW($ValueError,to$str("__lshift: negative shift count")));
    long shw = bval/64;
    long shb = bval%64;
    long mres = labs(ma) + shw + (shb > 0);
    $int res = $malloc$int();
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

$int $Integral$int$__rshift__($Integral$int wit,  $int a, $int b) {
    zz_struct aval = a->val;
    long ma = aval.size;
    long bval = from$int(b);
    if (ma==0 || bval==0)
        return a;
    if (bval<0)
        $RAISE(($BaseException)$NEW($ValueError,to$str("__rshift: negative shift count")));
    $int res = $malloc$int();
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
 
$int $Integral$int$__invert__($Integral$int wit,  $int a) {
    //return to$i64(~a->val);
    fprintf(stderr,"Number.__invert__ not implemented for int\n");
    exit(1);
}


// Logical$int  ////////////////////////////////////////////////////////////////////////////////////////

void $Logical$int$__serialize__($Logical$int self, $Serial$state state) {
    //$step_serialize(self->w$Integral, state);
    fprintf(stderr,"Protocol Logical not implemented for int; use i64\n");
    exit(1);
}

$Logical$int $Logical$int$__deserialize__($Logical$int self, $Serial$state state) {
    // $Logical$i64 res = $DNEW($Logical$i64,state);
    //  res->w$Integral = ($Integral)$step_deserialize(state);
    //  return res;
    fprintf(stderr,"Protocol Logical not implemented for int; use i64\n");
    exit(1);
}

$int $Logical$int$__and__($Logical$int wit,  $int a, $int b) {
    // return to$i64(a->val & b->val);
    fprintf(stderr,"Protocol Logical not implemented for int; use i64\n");
    exit(1);
}
                                                 
$int $Logical$int$__or__($Logical$int wit,  $int a, $int b) {
    // return to$i64(a->val | b->val);
    fprintf(stderr,"Protocol Logical not implemented for int; use i64\n");
    exit(1);
}
                                                 
$int $Logical$int$__xor__($Logical$int wit,  $int a, $int b) {
    // return to$i64(a->val ^ b->val);
    fprintf(stderr,"Protocol Logical not implemented for int; use i64\n");
    exit(1);
}  
 
// $Minus$int  ////////////////////////////////////////////////////////////////////////////////////////

void $Minus$int$__serialize__($Minus$int self, $Serial$state state) {
    $step_serialize(self->w$Integral, state);
}

$Minus$int $Minus$int$__deserialize__($Minus$int self, $Serial$state state) {
    $Minus$int res = $DNEW($Minus$int,state);
    res->w$Integral = ($Integral)$step_deserialize(state);
    return res;
}

$int $Minus$int$__sub__($Minus$int wit,  $int a, $int b) {
    $int res = $malloc$int();
    zz_sub(&res->val,&a->val,&b->val);
    return res;
}


// $Div$int  ////////////////////////////////////////////////////////////////////////////////////////

void $Div$int$__serialize__($Div$int self, $Serial$state state) {
}

$Div$int $Div$int$__deserialize__($Div$int self, $Serial$state state) {
    $Div$int res = $DNEW($Div$int,state);
    return res;
}

$float $Div$int$__truediv__ ($Div$int wit, $int a, $int b) {
    zz_ptr aval = &a->val;
    zz_ptr bval = &b->val;
    $int ared = $malloc$int();
    $int bred = $malloc$int();
    $int q = $malloc$int();
    $int r = $malloc$int();
    $int g = $malloc$int();
    zz_gcd(&g->val,aval,bval);
    zz_div(&ared->val,aval,&g->val);
    zz_div(&bred->val,bval,&g->val);
    zz_divrem(&q->val,&r->val,&ared->val,&bred->val);
    return to$float($float$new(($atom)q)->val +  $float$new(($atom)r)->val/ $float$new(($atom)bred)->val);
}

// $Ord$int  ////////////////////////////////////////////////////////////////////////////////////////

void $Ord$int$__serialize__($Ord$int self, $Serial$state state) {
}

$Ord$int $Ord$int$__deserialize__($Ord$int self, $Serial$state state) {
    $Ord$int res = $DNEW($Ord$int,state);
    return res;
}

$bool $Ord$int$__eq__ ($Ord$int wit, $int a, $int b) {
    return to$bool(zz_equal(&a->val,&b->val));
}

$bool $Ord$int$__ne__ ($Ord$int wit, $int a, $int b) {
    return to$bool(1-zz_equal(&a->val,&b->val));
}

$bool $Ord$int$__lt__ ($Ord$int wit, $int a, $int b) {
    return to$bool(zz_cmp(&a->val,&b->val) < 0);
}

$bool $Ord$int$__le__ ($Ord$int wit, $int a, $int b) {
    return to$bool(zz_cmp(&a->val,&b->val) <= 0);
}

$bool $Ord$int$__gt__ ($Ord$int wit, $int a, $int b) {
    return to$bool(zz_cmp(&a->val,&b->val) > 0);
}

$bool $Ord$int$__ge__ ($Ord$int wit, $int a, $int b) {
    return to$bool(zz_cmp(&a->val,&b->val) >= 0);
}

// $Hashable$int ///////////////////////////////////////////////////////////////////////////////////////////////////////

void $Hashable$int$__serialize__($Hashable$int self, $Serial$state state) {
}

$Hashable$int $Hashable$int$__deserialize__($Hashable$int self, $Serial$state state) {
    $Hashable$int res = $DNEW($Hashable$int,state);
    return res;
}

$bool $Hashable$int$__eq__($Hashable$int wit, $int a, $int b) {
    return to$bool(zz_equal(&a->val,&b->val));
}

$bool $Hashable$int$__ne__($Hashable$int wit, $int a, $int b) {
    return to$bool(1-zz_equal(&a->val,&b->val));
}

$int $Hashable$int$__hash__($Hashable$int wit, $int a) {
    //    $int res = $malloc$int();
    //    zz_ptr q = malloc(sizeof(zz_struct));
    //    zz_init_fit(q,1);
    //    zz_seti(&res->val,zz_divremi(q,&a->val,LONG_MAX/4));    // This hash algorithm should be reconsidered!!!

    return to$int($i64_hash(to$i64(from$int(a))));
}

// Initialization ////////////////////////////////////////////////////////////////////////////////////////////////////////

void $Integral$int_init($Integral$int wit) {
    wit-> w$Logical = ($Logical)$NEW($Logical$int,($Integral)wit);
    wit-> w$Minus = ($Minus)$NEW($Minus$int,($Integral)wit);
};

void $Logical$int_init($Logical$int wit, $Integral w$Integral) {
    wit->w$Integral =  w$Integral;
}

void $Minus$int_init($Minus$int wit, $Integral w$Integral) {
    wit->w$Integral =  w$Integral;
}

void $Div$int_init($Div$int wit) {
    return;
}

void $Ord$int_init($Ord$int wit) {
    return;
}

void $Hashable$int_init($Hashable$int wit) {
    return;
}

$Integral$int $Integral$int$new() {
    return $NEW($Integral$int);
}

$Logical$int $Logical$int$new($Integral wit) {
    return $NEW($Logical$int,wit);
}
  
$Minus$int $Minus$int$new($Integral wit) {
    return $NEW($Minus$int,wit);
}
  
$Ord$int $Ord$int$new() {
    return $NEW($Ord$int);
}

$Div$int $Div$int$new() {
    return $NEW($Div$int);
}

$Hashable$int $Hashable$int$new() {
    return $NEW($Hashable$int);
}


struct $Integral$int $Iintegral$int_instance;
struct $Logical$int $Logical$int_instance;
struct $Minus$int $Minus$int_instance;
struct $Ord$int $Ord$int_instance;
struct $Div$int $Div$int_instance;
struct $Hashable$int $Hashable$int_instance;

struct $Integral$int$class $Integral$int$methods = {
    "$Integral$int",
    UNASSIGNED,
    ($Super$class)&$Integral$methods,
    $Integral$int_init,
    $Integral$int$__serialize__,
    $Integral$int$__deserialize__,
    ($bool (*)($Integral$int))$default__bool__,
    ($str (*)($Integral$int))$default__str__,
    ($str (*)($Integral$int))$default__str__,
    $Integral$int$__add__,
    ($int (*)($Integral$int, $int, $int))$Plus$__iadd__,
    $Integral$int$__mul__,
    ($int (*)($Integral$int, $int, $int))$Times$__imul__,
    $Integral$int$__fromatom__,
    $Integral$int$__complx__,
    $Integral$int$__pow__,
    ($int (*)($Integral$int, $int, $int))$Number$__ipow__,
    $Integral$int$__neg__,
    $Integral$int$__pos__,
    $Integral$int$real,
    $Integral$int$imag,
    $Integral$int$__abs__,
    $Integral$int$__conjugate__,
    $Integral$int$__float__,
    $Integral$int$__trunc__,
    $Integral$int$__floor__,
    $Integral$int$__ceil__,
    $Integral$int$__round__,
    $Integral$int$numerator,
    $Integral$int$denominator,
    $Integral$int$__int__,
    $Integral$int$__index__,
    $Integral$int$__divmod__,
    $Integral$int$__floordiv__,
    $Integral$int$__mod__,
    $Integral$int$__lshift__,
    $Integral$int$__rshift__,
    ($int (*)($Integral$int, $int, $int))$Integral$__ifloordiv__,
    ($int (*)($Integral$int, $int, $int))$Integral$__imod__,
    ($int (*)($Integral$int, $int, $int))$Integral$__ilshift__,
    ($int (*)($Integral$int, $int, $int))$Integral$__irshift__,
    $Integral$int$__invert__
};

struct $Integral$int $Integral$int_instance = {&$Integral$int$methods, ($Minus)&$Minus$int_instance, ($Logical)&$Logical$int_instance};
$Integral$int $Integral$int$witness = &$Integral$int_instance;

struct $Logical$int$class $Logical$int$methods =  {
    "$Logical$int",
    UNASSIGNED,
    ($Super$class)&$Logical$methods,
    $Logical$int_init,
    $Logical$int$__serialize__,
    $Logical$int$__deserialize__,
    ($bool (*)($Logical$int))$default__bool__,
    ($str (*)($Logical$int))$default__str__,
    ($str (*)($Logical$int))$default__str__,
    $Logical$int$__and__,
    $Logical$int$__or__,
    $Logical$int$__xor__,
    ($int (*)($Logical$int, $int, $int))$Logical$__iand__,
    ($int (*)($Logical$int, $int, $int))$Logical$__ior__,
    ($int (*)($Logical$int, $int, $int))$Logical$__ixor__
};

struct $Logical$int $Logical$int_instance = {&$Logical$int$methods, ($Integral)&$Integral$int_instance};
$Logical$int $Logical$int$witness = &$Logical$int_instance;

struct $Minus$int$class $Minus$int$methods = {
    "$Minus$int",
    UNASSIGNED,
    ($Super$class)&$Minus$methods,
    $Minus$int_init,
    $Minus$int$__serialize__,
    $Minus$int$__deserialize__,
    ($bool (*)($Minus$int))$default__bool__,
    ($str (*)($Minus$int))$default__str__,
    ($str (*)($Minus$int))$default__str__,
    $Minus$int$__sub__,
    ($int (*)($Minus$int, $int, $int))$Minus$__isub__
};
struct $Minus$int $Minus$int_instance = {&$Minus$int$methods, ($Integral)&$Integral$int_instance};
$Minus$int $Minus$int$witness = &$Minus$int_instance;

struct $Ord$int$class $Ord$int$methods = {
    "$Ord$int",
    UNASSIGNED,
    ($Super$class)&$Ord$methods,
    $Ord$int_init,
    $Ord$int$__serialize__,
    $Ord$int$__deserialize__,
    ($bool (*)($Ord$int))$default__bool__,
    ($str (*)($Ord$int))$default__str__,
    ($str (*)($Ord$int))$default__str__,
    $Ord$int$__eq__,
    $Ord$int$__ne__,
    $Ord$int$__lt__,
    $Ord$int$__le__,
    $Ord$int$__gt__,
    $Ord$int$__ge__
};

struct $Ord$int $Ord$int_instance = {&$Ord$int$methods};
$Ord$int $Ord$int$witness = &$Ord$int_instance;

struct $Div$int$class $Div$int$methods = {
    "$Div$int",
    UNASSIGNED,
    ($Super$class)&$Div$methods,
    $Div$int_init,
    $Div$int$__serialize__,
    $Div$int$__deserialize__,
    ($bool (*)($Div$int))$default__bool__,
    ($str (*)($Div$int))$default__str__,
    ($str (*)($Div$int))$default__str__,
    $Div$int$__truediv__,
    ($float (*)($Div$int, $int, $int))$Div$__itruediv__,
};

struct $Div$int $Div$int_instance = {&$Div$int$methods};
$Div$int $Div$int$witness = &$Div$int_instance;

struct $Hashable$int$class $Hashable$int$methods = {
    "$Hashable$int",
    UNASSIGNED,
    ($Super$class)&$Hashable$methods,
    $Hashable$int_init,
    $Hashable$int$__serialize__,
    $Hashable$int$__deserialize__,
    ($bool (*)($Hashable$int))$default__bool__,
    ($str (*)($Hashable$int))$default__str__,
    ($str (*)($Hashable$int))$default__str__,
    $Hashable$int$__eq__,
    $Hashable$int$__ne__,
    $Hashable$int$__hash__
};

struct $Hashable$int $Hashable$int_instance = {&$Hashable$int$methods};
$Hashable$int $Hashable$int$witness = &$Hashable$int_instance;

long from$int($int n) { 
    long sz = n->val.size;
    if (sz==0) return 0;
    unsigned long res = n->val.n[0];
    if (res > LONG_MAX || labs(sz) > 1) {
        fprintf(stderr,"internal error: overflow in converting int to bounded int\n");
        exit(1);
    }
    return sz<0 ? -res : res;
}
            
$int to$int(long n) {
    $int res = malloc(sizeof(struct $int));
    res->$class = &$int$methods;
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

int $get$str0(bool ishead, zz_ptr n, zz_ptr dens[], int d, char *res, int pos) {
    if (d >= 0) {
        zz_ptr hi = malloc(sizeof(zz_struct));
        zz_ptr lo = malloc(sizeof(zz_struct));
        zz_init_fit(hi,dens[d]->size);
        zz_init_fit(lo,dens[d]->size);
        zz_divrem(hi, lo, n, dens[d]);
        if (hi->size==0 && ishead) {
            return $get$str0(ishead, lo, dens, d-1, res, pos);
        } else {
            int newpos = $get$str0(ishead, hi, dens, d-1, res, pos);        
            return $get$str0(false, lo, dens, d-1, res, newpos);          
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

char * $get$str(zz_ptr nval) {
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
    int newpos = $get$str0(true, npos, dens, d-1, res, pos);
    res[newpos] = '\0';
    return res;
}

int $set$str0(zz_ptr a, char *nstr, int parts) {
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
        int hidigs = $set$str0(hires, nstr, hi);
        int lodigs = $set$str0(lores, &nstr[hi * POW10INWORD], lo);
        zz_seti(a, 10);
        zz_powi(a, a, POW10INWORD * lo);
        zz_mul(a, a, hires);
        zz_add(a, a, lores);
        return hidigs + lodigs;
    }
}

int $set$str(zz_ptr a, char *nstr) {
    int len = 0;
    while (isdigit(nstr[len]))
        len++;
    if (len == 0) {
        $RAISE(($BaseException)$NEW($ValueError,to$str("int.__fromatom__: no digits in string prefix")));
    }
    int parts = len / POW10INWORD;
    int offset =  len % POW10INWORD;
    if (offset == 0)
        return $set$str0(a, nstr, parts);
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
            partdigits = $set$str0(res0, &nstr[offset], parts);
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
