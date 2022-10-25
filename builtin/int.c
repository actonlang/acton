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

$int $int$new($atom a) {
    if ($ISINSTANCE(a,$int)->val) return ($int)a;
    if ($ISINSTANCE(a,$i64)->val) {
        return to$int((($i64)a)->val);
    }
    if ($ISINSTANCE(a,$float)->val) {
        double aval = (($float)a)->val;
        zz_ptr res = malloc(sizeof(zz_struct));
        zz_init(res);
        int e;
        double m = frexp(aval,&e);
        if (e>52) {
            long x = (long)(m*4503599627370496.0); // (1<< 52); 
            zz_seti(res,x);
            $int c = zz$to$int(res);
            $int d = to$int(e-52);
            return  $Integral$int$__lshift__(NULL,c,d);
        } else {
            long x = (long)aval;
            zz_seti(res,x);
            return zz$to$int(res);
        }
    }
    if ($ISINSTANCE(a,$bool)->val) return to$int((($bool)a)->val);
    if ($ISINSTANCE(a,$str)->val) {
        zz_ptr val = malloc(sizeof(zz_struct));
        zz_init(val);
        int digits = zz_set_str(val,(char *)(($str)a)->str);
        if (digits>0)
            return zz$to$int(val);
        else 
            $RAISE(($BaseException)$NEW($ValueError,to$str("int(): invalid str value for type int")));
    }
    fprintf(stderr,"internal error: $i64$new: argument not of atomic type");
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
    int blobsize = 1 + labs(self->val->size);
    $ROW row = $add_header(INT_ID,blobsize,state);
    row->blob[0] = ($WORD)self->val->size;
    memcpy(&row->blob[1],self->val->n,labs(self->val->size)*sizeof(long));
}

$int $int_deserialize($int res,$Serial$state state) {
    $ROW this = state->row;
    state->row = this->next;
    state->row_no++;
    if (this->class_id < 0) {
        return ($int)$dict_get(state->done,($Hashable)$Hashable$int$witness,to$int((int)this->blob[0]),NULL);
    } else {
        if (!res)
            res = malloc(sizeof($int));
        res->val = malloc(sizeof(zz_struct));
        res->val->size = (long)this->blob[0];
        res->val->alloc = labs(res->val->size);
        res->val->n = malloc(res->val->alloc*sizeof(long));
        memcpy(res->val->n,&this->blob[1],res->val->alloc*sizeof(long));
        $dict_setitem(state->done,($Hashable)$Hashable$int$witness,to$int(state->row_no-1),res);
        res->$class = &$int$methods;
        return res;
    }
}

$bool $int_bool($int n) {
    return to$bool(zz_cmpi(n->val,0));
}

$str $int_str($int n) {
    return to$str(zz_get_str(n->val));
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
    $int res = malloc(sizeof(struct $int));
    res->$class = &$int$methods;
    res->val = n;
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
    zz_ptr res = malloc(sizeof(zz_struct));
    zz_init(res);
    zz_add(res,a->val,b->val);
    return zz$to$int(res);
}

$int $Integral$int$__iadd__($Integral$int wit,  $int a, $int b) {
    zz_add(a->val,a->val,b->val);
    return a;
}  

$complex $Integral$int$__complx__($Integral$int wit, $int a) {
    fprintf(stderr,"Number.__complex__ not implemented for int");
    exit(1);
}

$int $Integral$int$__fromatom__($Integral$int wit, $atom a) {
    return $int$new(a);
}

$int $Integral$int$__mul__($Integral$int wit,  $int a, $int b) {
    zz_ptr res = malloc(sizeof(zz_struct));
    zz_init(res);
    zz_mul(res,a->val,b->val);
    return zz$to$int(res);
}  
  
$int $Integral$int$__imul__($Integral$int wit,  $int a, $int b) {
    zz_mul(a->val,a->val,b->val);
    return a;
}  

$int $Integral$int$__pow__($Integral$int wit, $int a, $int b) {
    zz_ptr val_b = b->val;
    if (zz_cmpi(val_b,0) < 0)
        $RAISE(($BaseException)$NEW($ValueError,to$str("__pow__: exponent negative")));
    if (zz_cmpi(val_b,LONG_MAX) > 0)
        $RAISE(($BaseException)$NEW($ValueError,to$str("__pow__: exponent out of range (> LONG_MAX)")));
    zz_ptr res = malloc(sizeof(zz_struct));
    zz_init(res);
    zz_powi(res,a->val,val_b->n[0]); // __pow__ should have an int64 exponent in the Acton protocol
    return zz$to$int(res);
}

$int $Integral$int$__neg__($Integral$int wit,  $int a) {
    zz_ptr res = malloc(sizeof(zz_struct));
    zz_init(res);
    zz_neg(res,a->val);
    return zz$to$int(res);
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
    zz_ptr res = malloc(sizeof(zz_struct));
    zz_init(res);
    zz_set(res,a->val);
    res->size = labs(a->val->size);
    return wit2->$class->__fromatom__(wit2,($atom)zz$to$int(res));
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
    zz_ptr nval = n->val;
    if (zz_cmpi(nval,0)<0) {
        zz_ptr n1 = malloc(sizeof(zz_struct));
        zz_init(n1);
        zz_neg(n1,nval);
        zz_ptr negrel =  $Integral$int$__round__ (wit,zz$to$int(n1),p)->val;
        zz_neg(negrel,negrel);
        return zz$to$int(negrel);
    }
    if (zz_cmpi(p->val,-LONG_MAX) < 0)
        $RAISE(($BaseException)$NEW($ValueError,to$str("__round__: precision out of range")));
    long pval = p==NULL ? 0 : p->val->n[0];
    if (pval>=0)
        return n;
    zz_ptr ten;
    zz_init(ten);
    zz_seti(ten,10);
    zz_ptr p10 = malloc(sizeof(zz_struct));
    zz_init(p10);
    zz_powi(p10,ten,-pval);
    zz_ptr q = malloc(sizeof(zz_struct));
    zz_ptr r = malloc(sizeof(zz_struct));
    zz_init(q);
    zz_init(r);
    zz_divrem(q,r,n->val,p10);
    zz_mul(q,q,p10);
    return zz$to$int(q);  // TODO: Ensure correct rounding: if r*2 > p10, add 1 to q before final multiplication
}
  
$WORD $Integral$int$numerator ($Integral$int wit, $int n, $Integral wit2) {
    return wit2->$class->__fromatom__(wit2,($atom)n);
}
  
$WORD $Integral$int$denominator ($Integral$int wit, $int n, $Integral wit2) {
    zz_ptr res = malloc(sizeof(zz_struct));
    zz_init(res);
    zz_seti(res,1L);
    return wit2->$class->__fromatom__(wit2,($atom)zz$to$int(res));
}
  
$int $Integral$int$__int__ ($Integral$int wit, $int n) {
    return n;
}

$int $Integral$int$__index__($Integral$int wit, $int n) {
    return n;
}

$tuple $Integral$int$__divmod__($Integral$int wit, $int a, $int b) {
    zz_ptr q = malloc(sizeof(zz_struct));
    zz_ptr r = malloc(sizeof(zz_struct));
    zz_init(q);
    zz_init(r);
    zz_divrem(q,r,a->val,b->val);
    return $NEWTUPLE(2, zz$to$int(q), zz$to$int(r));
}

$int $Integral$int$__floordiv__($Integral$int wit, $int a, $int b) {
    zz_ptr res = malloc(sizeof(zz_struct));
    zz_init(res);
    zz_div(res,a->val, b->val);
    return zz$to$int(res);
}

$int $Integral$int$__mod__($Integral$int wit, $int a, $int b) {
    zz_ptr q = malloc(sizeof(zz_struct));
    zz_init(q);
    zz_ptr r = malloc(sizeof(zz_struct));
    zz_init(r);
    zz_divrem(q,r,a->val,b->val);
    return zz$to$int(r);
}

$int $Integral$int$__lshift__($Integral$int wit,  $int a, $int b) {
    zz_ptr aval = a->val;
    long ma = aval->size;
    long bval = from$int(b);
    if (ma==0 || bval==0)
        return a;
    if (bval<0)
        $RAISE(($BaseException)$NEW($ValueError,to$str("__lshift: negative shift count")));
    zz_ptr res = malloc(sizeof(zz_struct));
    long shw = bval/64;
    long shb = bval%64;
    long mres = labs(ma) + shw + (shb > 0);
    zz_init_fit(res,mres);
    if (shb>0) {
        word_t ci = nn_shl(res->n, aval->n, labs(ma), shb);
        if (ci>0)
            res->n[labs(ma)] = ci;
    }
    if (shw>0) {
        for (int i = labs(ma); i >= 0; i--)
            res->n[i+shw] = res->n[i];
        for (int i = 0; i < shw; i++)
            res->n[i] = 0;
    }
    mres = mres - (res->n[mres-1]==0);
    mres = ma<0? -mres:mres;
    res->size = mres;
    return zz$to$int(res); 
}

$int $Integral$int$__rshift__($Integral$int wit,  $int a, $int b) {
    zz_ptr aval = a->val;
    long ma = aval->size;
    long bval = from$int(b);
    if (ma==0 || bval==0)
        return a;
    if (bval<0)
        $RAISE(($BaseException)$NEW($ValueError,to$str("__rshift: negative shift count")));
    zz_ptr res = malloc(sizeof(zz_struct));
    long shw = bval/64;
    long shb = bval%64;
    long mres = labs(ma) - shw;
    zz_init_fit(res,mres);
    unsigned long tmp[mres];
    for (int i = 0; i < mres; i++)
        tmp[i] = aval->n[i+shw];
    word_t ci = nn_shr(res->n, tmp, mres, shb);
    mres = mres - (res->n[mres-1]==0);
    mres = ma<0?-mres:mres;
    res->size = mres;
    return zz$to$int(res); 
}
 
$int $Integral$int$__invert__($Integral$int wit,  $int a) {
    //return to$i64(~a->val);
    fprintf(stderr,"Number.__invert__ not yet implemented for int");
    exit(1);
}


// Logical$int  ////////////////////////////////////////////////////////////////////////////////////////

void $Logical$int$__serialize__($Logical$int self, $Serial$state state) {
    //$step_serialize(self->w$Integral, state);
    fprintf(stderr,"Protocol Logical not yet implemented for int");
    exit(1);
}

$Logical$int $Logical$int$__deserialize__($Logical$int self, $Serial$state state) {
    // $Logical$i64 res = $DNEW($Logical$i64,state);
    //  res->w$Integral = ($Integral)$step_deserialize(state);
    //  return res;
    fprintf(stderr,"Protocol Logical not yet implemented for int");
    exit(1);
}

$int $Logical$int$__and__($Logical$int wit,  $int a, $int b) {
    // return to$i64(a->val & b->val);
    fprintf(stderr,"Protocol Logical not yet implemented for int");
    exit(1);
}
                                                 
$int $Logical$int$__or__($Logical$int wit,  $int a, $int b) {
    // return to$i64(a->val | b->val);
    fprintf(stderr,"Protocol Logical not yet implemented for int");
    exit(1);
}
                                                 
$int $Logical$int$__xor__($Logical$int wit,  $int a, $int b) {
    // return to$i64(a->val ^ b->val);
    fprintf(stderr,"Protocol Logical not yet implemented for int");
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
    zz_ptr res = malloc(sizeof(zz_struct));
    zz_init(res);
    zz_sub(res,a->val,b->val);
    return zz$to$int(res);
}

$int $Minus$int$__isub__($Minus$int wit,  $int a, $int b) {
    zz_sub(a->val,a->val,b->val);
    return a;
}  


// $Div$int  ////////////////////////////////////////////////////////////////////////////////////////

void $Div$int$__serialize__($Div$int self, $Serial$state state) {
}

$Div$int $Div$int$__deserialize__($Div$int self, $Serial$state state) {
    $Div$int res = $DNEW($Div$int,state);
    return res;
}

$float $Div$int$__truediv__ ($Div$int wit, $int a, $int b) {
    zz_ptr aval = a->val;
    zz_ptr bval = b->val;
    zz_ptr q = malloc(sizeof(zz_struct));
    zz_ptr r = malloc(sizeof(zz_struct));
    zz_ptr g = malloc(sizeof(zz_struct));
    zz_init(q);
    zz_init(r);
    zz_init(g);
    zz_gcd(g,aval,bval);
    zz_div(aval,aval,g);
    zz_div(bval,bval,g);
    zz_divrem(q,r,aval,bval);
    return to$float($float$new(($atom)zz$to$int(q))->val +  $float$new(($atom)zz$to$int(r))->val/ $float$new(($atom)zz$to$int(bval))->val);
}

// $Ord$int  ////////////////////////////////////////////////////////////////////////////////////////

void $Ord$int$__serialize__($Ord$int self, $Serial$state state) {
}

$Ord$int $Ord$int$__deserialize__($Ord$int self, $Serial$state state) {
    $Ord$int res = $DNEW($Ord$int,state);
    return res;
}

$bool $Ord$int$__eq__ ($Ord$int wit, $int a, $int b) {
    return to$bool(zz_equal(a->val,b->val));
}

$bool $Ord$int$__ne__ ($Ord$int wit, $int a, $int b) {
    return to$bool(1-zz_equal(a->val,b->val));
}

$bool $Ord$int$__lt__ ($Ord$int wit, $int a, $int b) {
    return to$bool(zz_cmp(a->val,b->val) < 0);
}

$bool $Ord$int$__le__ ($Ord$int wit, $int a, $int b) {
    return to$bool(zz_cmp(a->val,b->val) <= 0);
}

$bool $Ord$int$__gt__ ($Ord$int wit, $int a, $int b) {
    return to$bool(zz_cmp(a->val,b->val) > 0);
}

$bool $Ord$int$__ge__ ($Ord$int wit, $int a, $int b) {
    return to$bool(zz_cmp(a->val,b->val) >= 0);
}

// $Hashable$int ///////////////////////////////////////////////////////////////////////////////////////////////////////

void $Hashable$int$__serialize__($Hashable$int self, $Serial$state state) {
}

$Hashable$int $Hashable$int$__deserialize__($Hashable$int self, $Serial$state state) {
    $Hashable$int res = $DNEW($Hashable$int,state);
    return res;
}

$bool $Hashable$int$__eq__($Hashable$int wit, $int a, $int b) {
    return to$bool(zz_equal(a->val,b->val));
}

$bool $Hashable$int$__ne__($Hashable$int wit, $int a, $int b) {
    return to$bool(1-zz_equal(a->val,b->val));
}

$int $Hashable$int$__hash__($Hashable$int wit, $int a) {
    zz_ptr res = malloc(sizeof(zz_struct));
    zz_ptr q = malloc(sizeof(zz_struct));
    zz_init(res);
    zz_init(q);
    zz_seti(res,zz_divremi(q,a->val,LONG_MAX/4));    // This hash algorithm should be reconsidered!!!
    return zz$to$int(res);
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
    $Integral$int$__iadd__,
    $Integral$int$__mul__,
    $Integral$int$__imul__,
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
    ($int (*)($Minus$int, $int, $int))$Minus$__isub__,

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

long from$int($int n) {         // We don't check if n is too large to fit!!
    long sz = n->val->size;
    if (sz==0) return 0;
    long res = n->val->n[0];
    return sz<0 ? -res : res;
}
            
$int to$int(long n) {
    zz_ptr res = malloc(sizeof(zz_struct));
    zz_init(res);
    zz_seti(res,n);
    return zz$to$int(res);
}
