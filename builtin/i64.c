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
long longpow(long a, long e) {
  if (e == 0) return 1;
  if (e == 1) return a;
  if (e%2 == 0) return longpow(a*a,e/2);
  return a * longpow(a*a,e/2);
}

// General methods ///////////////////////////////////////////////////////////////////////

$i64 $i64$new($atom a) {
  if ($ISINSTANCE(a,$int)->val){
    zz_ptr n = (($int)a)-> val;
    if (labs(n->size)>1) {
      $RAISE(($BaseException)$NEW($ValueError,to$str("i64(): int argument out of range")));
    }
    return to$i64(n->size*n->n[0]);
  }
  if ($ISINSTANCE(a,$i64)->val) return ($i64)a;
  if ($ISINSTANCE(a,$float)->val) return to$i64(round((($float)a)->val));
  if ($ISINSTANCE(a,$bool)->val) return to$i64((($bool)a)->val);
  if ($ISINSTANCE(a,$str)->val) {
    long x;
    int c;
    sscanf((char *)(($str)a)->str,"%ld%n",&x,&c);
    if (c==(($str)a)->nbytes)
      return to$i64(x);
    else 
      $RAISE(($BaseException)$NEW($ValueError,to$str("int(): invalid str value for type int")));
  }
  fprintf(stderr,"internal error: $i64$new: argument not of atomic type");
  exit(-1);
}

void $i64_init($i64 self, $atom a){
  self->val = $i64$new(a)->val;
}

void $i64_serialize($i64 n,$Serial$state state) {
  $val_serialize(INT_ID,&n->val,state);
}

$i64 $i64_deserialize($i64 n,$Serial$state state) {
  return to$i64((long)$val_deserialize(state));
}

$bool $i64_bool($i64 n) {
  return to$bool(n->val != 0);
}

$str $i64_str($i64 n) {
  char *s;
  asprintf(&s,"%ld",n->val);
  return to$str(s);
}
  
struct $i64$class $i64$methods = {
    "$i64",
    UNASSIGNED,
    ($Super$class)&$atom$methods,
    $i64_init,
    $i64_serialize,
    $i64_deserialize,
    $i64_bool,
    $i64_str,
    $i64_str
};

$i64 to$i64(long i) {
  $i64 res = malloc(sizeof(struct $i64));
  res->$class = &$i64$methods;
  res->val = i;
  return res;
}

long from$i64($i64 w) {
  return w->val;
}

                  

// $Integral$i64 /////////////////////////////////////////////////////////////////////////

void $Integral$i64$__serialize__($Integral$i64 self, $Serial$state state) {
  $step_serialize(self->w$Logical, state);
  $step_serialize(self->w$Minus, state);
}

$Integral$i64 $Integral$i64$__deserialize__($Integral$i64 self, $Serial$state state) {
   $Integral$i64 res = $DNEW($Integral$i64,state);
   res->w$Logical = ($Logical)$step_deserialize(state);
   res->w$Minus = ($Minus)$step_deserialize(state);
   return res;
}

$i64 $Integral$i64$__add__($Integral$i64 wit,  $i64 a, $i64 b) {
  return to$i64(a->val + b->val);
}  

$complex $Integral$i64$__complx__($Integral$i64 wit, $i64 a) {
  return to$complex((double)a->val);
}

$i64 $Integral$i64$__fromatom__($Integral$i64 wit, $atom a) {
  return $i64$new(a);
}

$i64 $Integral$i64$__mul__($Integral$i64 wit,  $i64 a, $i64 b) {
  return to$i64(a->val * b->val);
}  
  
$i64 $Integral$i64$__pow__($Integral$i64 wit,  $i64 a, $i64 b) {
  if ( b->val < 0) {
    // raise VALUEERROR;
    return NULL;
  }
  return to$i64(longpow(a->val,b->val));
}

$i64 $Integral$i64$__neg__($Integral$i64 wit,  $i64 a) {
  return to$i64(-a->val);
}

$i64 $Integral$i64$__pos__($Integral$i64 wit,  $i64 a) {
  return a;
}

$WORD $Integral$i64$real($Integral$i64 wit, $i64 a, $Real wit2) {
  return wit2->$class->__fromatom__(wit2,($atom)a);
}

$WORD $Integral$i64$imag($Integral$i64 wit, $i64 a, $Real wit2) {
  return wit2->$class->__fromatom__(wit2,($atom)to$i64(0L));
}

$WORD $Integral$i64$__abs__($Integral$i64 wit, $i64 a, $Real wit2) {
  return wit2->$class->__fromatom__(wit2,($atom)to$i64(labs(a->val)));
}

$i64 $Integral$i64$__conjugate__($Integral$i64 wit,  $i64 a) {
  return a;
}

$float $Integral$i64$__float__ ($Integral$i64 wit, $i64 n) {
  return to$float((double)n->val);
}

$WORD $Integral$i64$__trunc__ ($Integral$i64 wit, $i64 n, $Integral wit2) {
  return wit2->$class->__fromatom__(wit2,($atom)n);
}
  
$WORD $Integral$i64$__floor__ ($Integral$i64 wit, $i64 n, $Integral wit2) {
  return wit2->$class->__fromatom__(wit2,($atom)n);
}
  
$WORD $Integral$i64$__ceil__ ($Integral$i64 wit, $i64 n, $Integral wit2) {
  return wit2->$class->__fromatom__(wit2,($atom)n);
}
  
$i64 $Integral$i64$__round__ ($Integral$i64 wit, $i64 n, $i64 p) {
  long nval = n->val;
  if (nval<0)
    return to$i64(-$Integral$i64$__round__(wit,to$i64(-nval),p)->val);
  long pval = p==NULL ? 0 : p->val;
  if (pval>=0)
    return n;
  long p10 = longpow(10,-pval);
  long res = nval/p10;
  if (nval%p10 * 2 > p10)
    res++; 
  return to$i64 (res * p10);
}
  
$WORD $Integral$i64$numerator ($Integral$i64 wit, $i64 n, $Integral wit2) {
  return wit2->$class->__fromatom__(wit2,($atom)n);
}
  
$WORD $Integral$i64$denominator ($Integral$i64 wit, $i64 n, $Integral wit2) {
  return wit2->$class->__fromatom__(wit2,($atom)to$i64(1L));
}
  
$i64 $Integral$i64$__int__ ($Integral$i64 wit, $int n) {
    return $i64$new(($atom)n);
}

$i64 $Integral$i64$__index__($Integral$i64 wit, $int n) {
    return $i64$new(($atom)n);
}

$tuple $Integral$i64$__divmod__($Integral$i64 wit, $i64 a, $i64 b) {
  int n = a->val;
  int d = b->val;
  return $NEWTUPLE(2, to$i64(n/d), to$i64(n%d));
}

$i64 $Integral$i64$__floordiv__($Integral$i64 wit, $i64 a, $i64 b) {
  return to$i64(a->val / b->val);
}

$i64 $Integral$i64$__mod__($Integral$i64 wit, $i64 a, $i64 b) {
  return to$i64(a->val % b->val);
}

$i64 $Integral$i64$__lshift__($Integral$i64 wit,  $i64 a, $int b) {
    return to$i64(a->val << from$int(b));
}

$i64 $Integral$i64$__rshift__($Integral$i64 wit,  $i64 a, $int b) {
  return to$i64(a->val >> from$int(b));
}
 
$i64 $Integral$i64$__invert__($Integral$i64 wit,  $i64 a) {
  return to$i64(~a->val);
}


// Logical$i64  ////////////////////////////////////////////////////////////////////////////////////////

void $Logical$i64$__serialize__($Logical$i64 self, $Serial$state state) {
  $step_serialize(self->w$Integral, state);
}

$Logical$i64 $Logical$i64$__deserialize__($Logical$i64 self, $Serial$state state) {
   $Logical$i64 res = $DNEW($Logical$i64,state);
   res->w$Integral = ($Integral)$step_deserialize(state);
   return res;
}

$i64 $Logical$i64$__and__($Logical$i64 wit,  $i64 a, $i64 b) {
  return to$i64(a->val & b->val);
}
                                                 
$i64 $Logical$i64$__or__($Logical$i64 wit,  $i64 a, $i64 b) {
  return to$i64(a->val | b->val);
}
                                                 
$i64 $Logical$i64$__xor__($Logical$i64 wit,  $i64 a, $i64 b) {
  return to$i64(a->val ^ b->val);
}  
 
// $Minus$i64  ////////////////////////////////////////////////////////////////////////////////////////

void $Minus$i64$__serialize__($Minus$i64 self, $Serial$state state) {
  $step_serialize(self->w$Integral, state);
}

$Minus$i64 $Minus$i64$__deserialize__($Minus$i64 self, $Serial$state state) {
   $Minus$i64 res = $DNEW($Minus$i64,state);
   res->w$Integral = ($Integral)$step_deserialize(state);
   return res;
}

$i64 $Minus$i64$__sub__($Minus$i64 wit,  $i64 a, $i64 b) {
  return to$i64(a->val - b->val);
}  

// $Div$i64  ////////////////////////////////////////////////////////////////////////////////////////

void $Div$i64$__serialize__($Div$i64 self, $Serial$state state) {
}

$Div$i64 $Div$i64$__deserialize__($Div$i64 self, $Serial$state state) {
   $Div$i64 res = $DNEW($Div$i64,state);
   return res;
}

$float $Div$i64$__truediv__ ($Div$i64 wit, $i64 a, $i64 b) {
  return to$float((double)a->val/(double)b->val);
}

// $Ord$i64  ////////////////////////////////////////////////////////////////////////////////////////

void $Ord$i64$__serialize__($Ord$i64 self, $Serial$state state) {
}

$Ord$i64 $Ord$i64$__deserialize__($Ord$i64 self, $Serial$state state) {
   $Ord$i64 res = $DNEW($Ord$i64,state);
   return res;
}

$bool $Ord$i64$__eq__ ($Ord$i64 wit, $i64 a, $i64 b) {
  return to$bool(a->val == b->val);
}

$bool $Ord$i64$__ne__ ($Ord$i64 wit, $i64 a, $i64 b) {
  return to$bool(a->val != b->val);
}

$bool $Ord$i64$__lt__ ($Ord$i64 wit, $i64 a, $i64 b) {
  return to$bool(a->val < b->val);
}

$bool $Ord$i64$__le__ ($Ord$i64 wit, $i64 a, $i64 b) {
  return to$bool(a->val <= b->val);
}

$bool $Ord$i64$__gt__ ($Ord$i64 wit, $i64 a, $i64 b) {
  return to$bool(a->val > b->val);
}

$bool $Ord$i64$__ge__ ($Ord$i64 wit, $i64 a, $i64 b) {
  return to$bool(a->val >= b->val);
}

// $Hashable$i64 ///////////////////////////////////////////////////////////////////////////////////////////////////////

void $Hashable$i64$__serialize__($Hashable$i64 self, $Serial$state state) {
}

$Hashable$i64 $Hashable$i64$__deserialize__($Hashable$i64 self, $Serial$state state) {
   $Hashable$i64 res = $DNEW($Hashable$i64,state);
   return res;
}

$bool $Hashable$i64$__eq__($Hashable$i64 wit, $i64 a, $i64 b) {
  return to$bool(a->val == b->val);
}

$bool $Hashable$i64$__ne__($Hashable$i64 wit, $i64 a, $i64 b) {
  return to$bool(a->val != b->val);
}

$int $Hashable$i64$__hash__($Hashable$i64 wit, $i64 a) {
  return to$int($i64_hash(a));
}

// Initialization ////////////////////////////////////////////////////////////////////////////////////////////////////////

void $Integral$i64_init($Integral$i64 wit) {
  wit-> w$Logical = ($Logical)$NEW($Logical$i64,($Integral)wit);
  wit-> w$Minus = ($Minus)$NEW($Minus$i64,($Integral)wit);
};

void $Logical$i64_init($Logical$i64 wit, $Integral w$Integral) {
  wit->w$Integral =  w$Integral;
}

void $Minus$i64_init($Minus$i64 wit, $Integral w$Integral) {
  wit->w$Integral =  w$Integral;
}

void $Div$i64_init($Div$i64 wit) {
  return;
}

void $Ord$i64_init($Ord$i64 wit) {
  return;
}

void $Hashable$i64_init($Hashable$i64 wit) {
  return;
}

$Integral$i64 $Integral$i64$new() {
  return $NEW($Integral$i64);
}

$Logical$i64 $Logical$i64$new($Integral wit) {
  return $NEW($Logical$i64,wit);
}
  
$Minus$i64 $Minus$i64$new($Integral wit) {
  return $NEW($Minus$i64,wit);
}
  
$Ord$i64 $Ord$i64$new() {
  return $NEW($Ord$i64);
}

$Div$i64 $Div$i64$new() {
  return $NEW($Div$i64);
}

$Hashable$i64 $Hashable$i64$new() {
  return $NEW($Hashable$i64);
}


struct $Integral$i64 $Integral$i64_instance;
struct $Logical$i64 $Logical$i64_instance;
struct $Minus$i64 $Minus$i64_instance;
struct $Ord$i64 $Ord$i64_instance;
struct $Div$i64 $Div$i64_instance;
struct $Hashable$i64 $Hashable$i64_instance;

struct $Integral$i64$class $Integral$i64$methods = {
    "$Integral$i64",
    UNASSIGNED,
    ($Super$class)&$Integral$methods,
    $Integral$i64_init,
    $Integral$i64$__serialize__,
    $Integral$i64$__deserialize__,
    ($bool (*)($Integral$i64))$default__bool__,
    ($str (*)($Integral$i64))$default__str__,
    ($str (*)($Integral$i64))$default__str__,
    $Integral$i64$__add__,
    ($i64 (*)($Integral$i64, $i64, $i64))$Plus$__iadd__,
    $Integral$i64$__mul__,
    ($i64 (*)($Integral$i64, $i64, $i64))$Times$__imul__,
    $Integral$i64$__fromatom__,
    $Integral$i64$__complx__,
    $Integral$i64$__pow__,
    ($i64 (*)($Integral$i64, $i64, $i64))$Number$__ipow__,
    $Integral$i64$__neg__,
    $Integral$i64$__pos__,
    $Integral$i64$real,
    $Integral$i64$imag,
    $Integral$i64$__abs__,
    $Integral$i64$__conjugate__,
    $Integral$i64$__float__,
    $Integral$i64$__trunc__,
    $Integral$i64$__floor__,
    $Integral$i64$__ceil__,
    $Integral$i64$__round__,
    $Integral$i64$numerator,
    $Integral$i64$denominator,
    $Integral$i64$__int__,
    $Integral$i64$__index__,
    $Integral$i64$__divmod__,
    $Integral$i64$__floordiv__,
    $Integral$i64$__mod__,
    $Integral$i64$__lshift__,
    $Integral$i64$__rshift__,
    ($i64 (*)($Integral$i64, $i64, $i64))$Integral$__ifloordiv__,
    ($i64 (*)($Integral$i64, $i64, $i64))$Integral$__imod__,
    ($i64 (*)($Integral$i64, $i64, $int))$Integral$__ilshift__,
    ($i64 (*)($Integral$i64, $i64, $int))$Integral$__irshift__,
    $Integral$i64$__invert__
};

struct $Integral$i64 $Integral$i64_instance = {&$Integral$i64$methods, ($Minus)&$Minus$i64_instance, ($Logical)&$Logical$i64_instance};
$Integral$i64 $Integral$i64$witness = &$Integral$i64_instance;

struct $Logical$i64$class $Logical$i64$methods =  {
    "$Logical$i64",
    UNASSIGNED,
    ($Super$class)&$Logical$methods,
    $Logical$i64_init,
    $Logical$i64$__serialize__,
    $Logical$i64$__deserialize__,
    ($bool (*)($Logical$i64))$default__bool__,
    ($str (*)($Logical$i64))$default__str__,
    ($str (*)($Logical$i64))$default__str__,
    $Logical$i64$__and__,
    $Logical$i64$__or__,
    $Logical$i64$__xor__,
    ($i64 (*)($Logical$i64, $i64, $i64))$Logical$__iand__,
    ($i64 (*)($Logical$i64, $i64, $i64))$Logical$__ior__,
    ($i64 (*)($Logical$i64, $i64, $i64))$Logical$__ixor__
};

struct $Logical$i64 $Logical$i64_instance = {&$Logical$i64$methods, ($Integral)&$Integral$i64_instance};
$Logical$i64 $Logical$i64$witness = &$Logical$i64_instance;

struct $Minus$i64$class $Minus$i64$methods = {
    "$Minus$i64",
    UNASSIGNED,
    ($Super$class)&$Minus$methods,
    $Minus$i64_init,
    $Minus$i64$__serialize__,
    $Minus$i64$__deserialize__,
    ($bool (*)($Minus$i64))$default__bool__,
    ($str (*)($Minus$i64))$default__str__,
    ($str (*)($Minus$i64))$default__str__,
    $Minus$i64$__sub__,
    ($i64 (*)($Minus$i64, $i64, $i64))$Minus$__isub__,

};
struct $Minus$i64 $Minus$i64_instance = {&$Minus$i64$methods, ($Integral)&$Integral$i64_instance};
$Minus$i64 $Minus$i64$witness = &$Minus$i64_instance;

struct $Ord$i64$class $Ord$i64$methods = {
    "$Ord$i64",
    UNASSIGNED,
    ($Super$class)&$Ord$methods,
    $Ord$i64_init,
    $Ord$i64$__serialize__,
    $Ord$i64$__deserialize__,
    ($bool (*)($Ord$i64))$default__bool__,
    ($str (*)($Ord$i64))$default__str__,
    ($str (*)($Ord$i64))$default__str__,
    $Ord$i64$__eq__,
    $Ord$i64$__ne__,
    $Ord$i64$__lt__,
    $Ord$i64$__le__,
    $Ord$i64$__gt__,
    $Ord$i64$__ge__
};

struct $Ord$i64 $Ord$i64_instance = {&$Ord$i64$methods};
$Ord$i64 $Ord$i64$witness = &$Ord$i64_instance;

struct $Div$i64$class $Div$i64$methods = {
    "$Div$i64",
    UNASSIGNED,
    ($Super$class)&$Div$methods,
    $Div$i64_init,
    $Div$i64$__serialize__,
    $Div$i64$__deserialize__,
    ($bool (*)($Div$i64))$default__bool__,
    ($str (*)($Div$i64))$default__str__,
    ($str (*)($Div$i64))$default__str__,
    $Div$i64$__truediv__,
    ($float (*)($Div$i64, $i64, $i64))$Div$__itruediv__,
};

struct $Div$i64 $Div$i64_instance = {&$Div$i64$methods};
$Div$i64 $Div$i64$witness = &$Div$i64_instance;

struct $Hashable$i64$class $Hashable$i64$methods = {
    "$Hashable$i64",
    UNASSIGNED,
    ($Super$class)&$Hashable$methods,
    $Hashable$i64_init,
    $Hashable$i64$__serialize__,
    $Hashable$i64$__deserialize__,
    ($bool (*)($Hashable$i64))$default__bool__,
    ($str (*)($Hashable$i64))$default__str__,
    ($str (*)($Hashable$i64))$default__str__,
    $Hashable$i64$__eq__,
    $Hashable$i64$__ne__,
    $Hashable$i64$__hash__
};

struct $Hashable$i64 $Hashable$i64_instance = {&$Hashable$i64$methods};
$Hashable$i64 $Hashable$i64$witness = &$Hashable$i64_instance;
