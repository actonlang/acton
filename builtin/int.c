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

$int $int$new($atom a) {
  if ($ISINSTANCE(a,$int)->val) return ($int)a;
  if ($ISINSTANCE(a,$float)->val) return to$int(round((($float)a)->val));
  if ($ISINSTANCE(a,$bool)->val) return to$int((($bool)a)->val);
  if ($ISINSTANCE(a,$str)->val) {
    long x;
    int c;
    sscanf((char *)(($str)a)->str,"%ld%n",&x,&c);
    if (c==(($str)a)->nbytes)
      return to$int(x);
    else 
      $RAISE(($BaseException)$NEW($ValueError,to$str("int(): invalid str value for type int")));
  }
  fprintf(stderr,"internal error: $int$new: argument not of atomic type");
  exit(-1);
}

void $int_init($int self, $atom a){
  self->val = $int$new(a)->val;
}

void $int_serialize($int n,$Serial$state state) {
  $val_serialize(INT_ID,&n->val,state);
}

$int $int_deserialize($int n,$Serial$state state) {
  return to$int((long)$val_deserialize(state));
}

$bool $int_bool($int n) {
  return to$bool(n->val != 0);
}

$str $int_str($int n) {
  char *s;
  asprintf(&s,"%ld",n->val);
  return to$str(s);
}
  
struct $int$class $int$methods = {
    "$int",
    UNASSIGNED,
    ($Super$class)&$atom$methods,
    $int_init,
    $int_serialize,
    $int_deserialize,
    $int_bool,
    $int_str
};

$int to$int(long i) {
  $int res = malloc(sizeof(struct $int));
  res->$class = &$int$methods;
  res->val = i;
  return res;
}

long from$int($int w) {
  return w->val;
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
  return to$int(a->val + b->val);
}  

$complex $Integral$int$__complx__($Integral$int wit, $int a) {
  return to$complex((double)a->val);
}

$int $Integral$int$__fromatom__($Integral$int wit, $atom a) {
  return $int$new(a);
}

$int $Integral$int$__mul__($Integral$int wit,  $int a, $int b) {
  return to$int(a->val * b->val);
}  
  
$int $Integral$int$__pow__($Integral$int wit,  $int a, $int b) {
  if ( b->val < 0) {
    // raise VALUEERROR;
    return NULL;
  }
  return to$int(longpow(a->val,b->val));
}

$int $Integral$int$__neg__($Integral$int wit,  $int a) {
  return to$int(-a->val);
}

$int $Integral$int$__pos__($Integral$int wit,  $int a) {
  return a;
}

$WORD $Integral$int$real($Integral$int wit, $int a, $Real wit2) {
  return wit2->$class->__fromatom__(wit2,($atom)a);
}

$WORD $Integral$int$imag($Integral$int wit, $int a, $Real wit2) {
  return wit2->$class->__fromatom__(wit2,($atom)to$int(0L));
}

$WORD $Integral$int$__abs__($Integral$int wit, $int a, $Real wit2) {
  return wit2->$class->__fromatom__(wit2,($atom)to$int(labs(a->val)));
}

$int $Integral$int$__conjugate__($Integral$int wit,  $int a) {
  return a;
}

$float $Integral$int$__float__ ($Integral$int wit, $int n) {
  return to$float((double)n->val);
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
  long nval = n->val;
  if (nval<0)
    return to$int(-$Integral$int$__round__(wit,to$int(-nval),p)->val);
  long pval = p==NULL ? 0 : p->val;
  if (pval>=0)
    return n;
  long p10 = longpow(10,-pval);
  long res = nval/p10;
  if (nval%p10 * 2 > p10)
    res++; 
  return to$int (res * p10);
}
  
$WORD $Integral$int$numerator ($Integral$int wit, $int n, $Integral wit2) {
  return wit2->$class->__fromatom__(wit2,($atom)n);
}
  
$WORD $Integral$int$denominator ($Integral$int wit, $int n, $Integral wit2) {
  return wit2->$class->__fromatom__(wit2,($atom)to$int(1L));
}
  
$int $Integral$int$__int__ ($Integral$int wit, $int n) {
  return n;
}

$int $Integral$int$__index__($Integral$int wit, $int n) {
  return n;
}

$tuple $Integral$int$__divmod__($Integral$int wit, $int a, $int b) {
  int n = a->val;
  int d = b->val;
  $WORD *comps = malloc(2*sizeof($WORD));
  comps[0] = to$int(n/d);
  comps[1] = to$int(n%d);
  return $NEW($tuple,2,comps);
}

$int $Integral$int$__floordiv__($Integral$int wit, $int a, $int b) {
  return to$int(a->val / b->val);
}

$int $Integral$int$__mod__($Integral$int wit, $int a, $int b) {
  return to$int(a->val % b->val);
}

$int $Integral$int$__lshift__($Integral$int wit,  $int a, $int b) {
  return to$int(a->val << b->val);
}

$int $Integral$int$__rshift__($Integral$int wit,  $int a, $int b) {
  return to$int(a->val >> b->val);
}
 
$int $Integral$int$__invert__($Integral$int wit,  $int a) {
  return to$int(~a->val);
}


// Logical$int  ////////////////////////////////////////////////////////////////////////////////////////

void $Logical$int$__serialize__($Logical$int self, $Serial$state state) {
  $step_serialize(self->w$Integral, state);
}

$Logical$int $Logical$int$__deserialize__($Logical$int self, $Serial$state state) {
   $Logical$int res = $DNEW($Logical$int,state);
   res->w$Integral = ($Integral)$step_deserialize(state);
   return res;
}

$int $Logical$int$__and__($Logical$int wit,  $int a, $int b) {
  return to$int(a->val & b->val);
}
                                                 
$int $Logical$int$__or__($Logical$int wit,  $int a, $int b) {
  return to$int(a->val | b->val);
}
                                                 
$int $Logical$int$__xor__($Logical$int wit,  $int a, $int b) {
  return to$int(a->val ^ b->val);
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
  return to$int(a->val - b->val);
}  

// $Div$int  ////////////////////////////////////////////////////////////////////////////////////////

void $Div$int$__serialize__($Div$int self, $Serial$state state) {
}

$Div$int $Div$int$__deserialize__($Div$int self, $Serial$state state) {
   $Div$int res = $DNEW($Div$int,state);
   return res;
}

$float $Div$int$__truediv__ ($Div$int wit, $int a, $int b) {
  return to$float((double)a->val/(double)b->val);
}

// $Ord$int  ////////////////////////////////////////////////////////////////////////////////////////

void $Ord$int$__serialize__($Ord$int self, $Serial$state state) {
}

$Ord$int $Ord$int$__deserialize__($Ord$int self, $Serial$state state) {
   $Ord$int res = $DNEW($Ord$int,state);
   return res;
}

$bool $Ord$int$__eq__ ($Ord$int wit, $int a, $int b) {
  return to$bool(a->val == b->val);
}

$bool $Ord$int$__ne__ ($Ord$int wit, $int a, $int b) {
  return to$bool(a->val != b->val);
}

$bool $Ord$int$__lt__ ($Ord$int wit, $int a, $int b) {
  return to$bool(a->val < b->val);
}

$bool $Ord$int$__le__ ($Ord$int wit, $int a, $int b) {
  return to$bool(a->val <= b->val);
}

$bool $Ord$int$__gt__ ($Ord$int wit, $int a, $int b) {
  return to$bool(a->val > b->val);
}

$bool $Ord$int$__ge__ ($Ord$int wit, $int a, $int b) {
  return to$bool(a->val == b->val);
}

// $Hashable$int ///////////////////////////////////////////////////////////////////////////////////////////////////////

void $Hashable$int$__serialize__($Hashable$int self, $Serial$state state) {
}

$Hashable$int $Hashable$int$__deserialize__($Hashable$int self, $Serial$state state) {
   $Hashable$int res = $DNEW($Hashable$int,state);
   return res;
}

$bool $Hashable$int$__eq__($Hashable$int wit, $int a, $int b) {
  return to$bool(a->val == b->val);
}

$bool $Hashable$int$__neq__($Hashable$int wit, $int a, $int b) {
  return to$bool(a->val != b->val);
}

$int $Hashable$int$__hash__($Hashable$int wit, $int a) {
  return to$int($int_hash(a));
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


struct $Integral$int $Integral$int_instance;
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
    $Hashable$int$__eq__,
    $Hashable$int$__neq__,
    $Hashable$int$__hash__
};

struct $Hashable$int $Hashable$int_instance = {&$Hashable$int$methods};
$Hashable$int $Hashable$int$witness = &$Hashable$int_instance;
