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

$complex to$complex(complex double c) {
  $complex res = malloc(sizeof(struct $complex));
  res->$class = &$complex$methods;
  res->val = c;
  return res;
}

$complex $complex$new($Number wit, $WORD c) {
  return $NEW($complex,wit,c);
}

void $complex_init($complex self, $Number wit, $WORD c){
  self->val = wit->$class->__complx__(wit,c)->val;
}

void $complex_serialize($complex c,$Serial$state state) {
  $ROW row = $add_header(COMPLEX_ID,2,state);
  double re = creal(c->val);
  double im = cimag(c->val);
  memcpy(row->blob,&re,sizeof(double));
  memcpy(row->blob+1,&im,sizeof(double));
}

$complex $complex_deserialize($complex self, $Serial$state state) {
  $ROW this = state->row;
  state->row =this->next;
  state->row_no++;
  double re, im;
  memcpy(&re,this->blob,sizeof(double));
  memcpy(&im,this->blob+1,sizeof(double));
  return to$complex(re + im * _Complex_I);
}

$bool $complex_bool($complex n) {
  return to$bool(n->val != 0.0);
}

$str $complex_str($complex c) {
  char *s;
  asprintf(&s,"%f + %f*I",creal(c->val),cimag(c->val));
  return to$str(s);
}
  
struct $complex$class $complex$methods = {"$complex",UNASSIGNED,($Super$class)&$value$methods,$complex_init,$complex_serialize,$complex_deserialize,$complex_bool,$complex_str,$complex_str};

// $Number$complex  ////////////////////////////////////////////////////////////////////////////////////////

void $Number$complex$__serialize__($Number$complex self, $Serial$state state) {
  $step_serialize(self->w$Minus, state);
}

$Number$complex $Number$complex$__deserialize__($Number$complex res, $Serial$state state) {
   if (!res)
      res = $DNEW($Number$complex,state);
   res->w$Minus = ($Minus)$step_deserialize(state);
   return res;
}

$complex $Number$complex$__add__($Number$complex wit, $complex a, $complex b) {
  return to$complex(a->val + b->val);
}  

$complex $Number$complex$__complx__ ($Number$complex wit, $complex c) {
  return c;
}

$complex $Number$complex$__mul__ ($Number$complex wit, $complex a, $complex b){
  return to$complex(a->val * b->val);
}

$complex $Number$complex$__pow__ ($Number$complex wit, $complex a, $complex b) {
  return to$complex(cpow(a->val,b->val));
}

$complex $Number$complex$__neg__ ($Number$complex wit, $complex c){
  return to$complex(-c->val);
}

$complex $Number$complex$__pos__ ($Number$complex wit, $complex c) {
  return c;
}

$WORD $Number$complex$real ($Number$complex wit, $complex c, $Real wit2) {
  return wit2->$class->__fromatom__(wit2,($atom)to$float(creal(c->val)));
}

$WORD $Number$complex$imag ($Number$complex wit, $complex c, $Real wit2) {
  return wit2->$class->__fromatom__(wit2,($atom)to$float(cimag(c->val)));
}

$WORD $Number$complex$__abs__ ($Number$complex wit, $complex c, $Real wit2) {
  return wit2->$class->__fromatom__(wit2,($atom)to$float(cabs(c->val)));
}

$complex $Number$complex$conjugate ($Number$complex wit, $complex c) {
  return to$complex(conj(c->val));
}

// $Div$complex /////////////////////////////////////////////////////////////////////////////////////////

void $Div$complex$__serialize__($Div$complex self, $Serial$state state) {
}

$Div$complex $Div$complex$__deserialize__($Div$complex self, $Serial$state state) {
   $Div$complex res = $DNEW($Div$complex,state);
   return res;
}

$complex $Div$complex$__truediv__ ($Div$complex wit, $complex a, $complex b) {
  return to$complex(a->val/b->val);
}

// $Minus$complex  ////////////////////////////////////////////////////////////////////////////////////////

void $Minus$complex$__serialize__($Minus$complex self, $Serial$state state) {
  $step_serialize(self->w$Number, state);
}

$Minus$complex $Minus$complex$__deserialize__($Minus$complex self, $Serial$state state) {
   $Minus$complex res = $DNEW($Minus$complex,state);
   res->w$Number = ($Number)$step_deserialize(state);
   return res;
}

$complex $Minus$complex$__sub__($Minus$complex wit, $complex a, $complex b) {
  return to$complex(a->val - b->val);
}  
// $Eq$complex  ////////////////////////////////////////////////////////////////////////////////////////

void $Eq$complex$__serialize__($Eq$complex self, $Serial$state state) {
}

$Eq$complex $Eq$complex$__deserialize__($Eq$complex self, $Serial$state state) {
   $Eq$complex res = $DNEW($Eq$complex,state);
   return res;
}

$bool $Eq$complex$__eq__ ($Eq$complex wit, $complex a, $complex b) {
  return to$bool(creal(a->val) == creal(b->val) && cimag(a->val) == cimag(b->val));
}

$bool $Eq$complex$__ne__ ($Eq$complex wit, $complex a, $complex b) {
  return to$bool(!from$bool($Eq$complex$__eq__(wit,a,b)));
}


// $Hashable$complex  ////////////////////////////////////////////////////////////////////////////////////////

void $Hashable$complex$__serialize__($Hashable$complex self, $Serial$state state) {
}

$Hashable$complex $Hashable$complex$__deserialize__($Hashable$complex self, $Serial$state state) {
   $Hashable$complex res = $DNEW($Hashable$complex,state);
   return res;
}

$bool $Hashable$complex$__eq__($Hashable$complex wit, $complex a, $complex b) {
  return to$bool(creal(a->val) == creal(b->val) && cimag(a->val) == cimag(b->val));
}

$bool $Hashable$complex$__ne__($Hashable$complex wit, $complex a, $complex b) {
  return to$bool(!from$bool($Hashable$complex$__eq__(wit,a,b)));
}

$int $Hashable$complex$__hash__($Hashable$complex wit, $complex a) {
  return to$int($complex_hash(a));
}

// init methods ////////////////////////////////////////////////////////////////////////////////////////////////

void $Number$complex_init ($Number$complex wit) {
  wit-> w$Minus = ($Minus)$NEW($Minus$complex,($Number)wit);
}

void $Minus$complex_init($Minus$complex wit, $Number w$Number) {
  wit->w$Number =  w$Number;
}

void $Eq$complex_init($Eq$complex wit) {
  return;
}

void $Div$complex_init($Div$complex wit) {
  return;
}

void $Hashable$complex_init($Hashable$complex wit) {
  return;
}

$Number$complex $Number$complex$new() {
  return $NEW($Number$complex);
}

$Minus$complex $Minus$complex$new($Number wit) {
  return $NEW($Minus$complex,wit);
}
  
$Eq$complex $Eq$complex$new() {
  return $NEW($Eq$complex);
}

$Hashable$complex $Hashable$complex$new() {
  return $NEW($Hashable$complex);
}


struct $Number$complex $Number$complex_instance;
struct $Minus$complex $Minus$complex_instance;
struct $Eq$complex $Eq$complex_instance;
struct $Hashable$complex $Hashable$complex_instance;

struct $Number$complex$class $Number$complex$methods = {
    "$Number$complex",
    UNASSIGNED,
    ($Super$class)&$Number$methods,
    $Number$complex_init,
    $Number$complex$__serialize__,
    $Number$complex$__deserialize__,
    ($bool (*)($Number$complex))$default__bool__,
    ($str (*)($Number$complex))$default__str__,
    ($str (*)($Number$complex))$default__str__,
    $Number$complex$__add__,
    ($complex (*)($Number$complex, $complex, $complex))$Plus$__iadd__,
    $Number$complex$__mul__,
    ($complex (*)($Number$complex, $complex, $complex))$Times$__imul__,
    NULL,        // fromatom
    $Number$complex$__complx__,
    $Number$complex$__pow__,
    ($complex (*)($Number$complex, $complex, $complex))$Number$__ipow__,
    $Number$complex$__neg__,
    $Number$complex$__pos__,
    $Number$complex$real,
    $Number$complex$imag,
    $Number$complex$__abs__,
    $Number$complex$conjugate
};
struct $Number$complex $Number$complex_instance = {&$Number$complex$methods, ($Minus)&$Minus$complex_instance};
$Number$complex $Number$complex$witness = &$Number$complex_instance;

struct $Div$complex$class $Div$complex$methods = {
    "$Div$complex",
    UNASSIGNED,
    ($Super$class)&$Div$methods,
    $Div$complex_init,
    $Div$complex$__serialize__,
    $Div$complex$__deserialize__,
    ($bool (*)($Div$complex))$default__bool__,
    ($str (*)($Div$complex))$default__str__,
    ($str (*)($Div$complex))$default__str__,
    $Div$complex$__truediv__,
    ($complex (*)($Div$complex, $complex, $complex))$Div$__itruediv__,
};

struct $Div$complex $Div$complex_instance = {&$Div$complex$methods};
$Div$complex $Div$complex$witness = &$Div$complex_instance;

struct $Minus$complex$class $Minus$complex$methods = {
    "$Minus$complex",
    UNASSIGNED,
    ($Super$class)&$Minus$methods,
    $Minus$complex_init,
    $Minus$complex$__serialize__,
    $Minus$complex$__deserialize__,
    ($bool (*)($Minus$complex))$default__bool__,
    ($str (*)($Minus$complex))$default__str__,
    ($str (*)($Minus$complex))$default__str__,
    $Minus$complex$__sub__,
    ($complex (*)($Minus$complex, $complex, $complex))$Minus$__isub__
};
struct $Minus$complex $Minus$complex_instance = {&$Minus$complex$methods, ($Number)&$Number$complex_instance};
$Minus$complex $Minus$complex$witness = &$Minus$complex_instance;

struct $Eq$complex$class $Eq$complex$methods = {
    "$Eq$complex",
    UNASSIGNED,
    ($Super$class)&$Eq$methods,
    $Eq$complex_init,
    $Eq$complex$__serialize__,
    $Eq$complex$__deserialize__,
    ($bool (*)($Eq$complex))$default__bool__,
    ($str (*)($Eq$complex))$default__str__,
    ($str (*)($Eq$complex))$default__str__,
    $Eq$complex$__eq__,
    $Eq$complex$__ne__
};
struct $Eq$complex $Eq$complex_instance = {&$Eq$complex$methods};
$Eq$complex $Eq$complex$witness = &$Eq$complex_instance;

struct $Hashable$complex$class $Hashable$complex$methods = {
    "$Hashable$complex",
    UNASSIGNED,
    ($Super$class)&$Hashable$methods,
    $Hashable$complex_init,
    $Hashable$complex$__serialize__,
    $Hashable$complex$__deserialize__,
    ($bool (*)($Hashable$complex))$default__bool__,
    ($str (*)($Hashable$complex))$default__str__,
    ($str (*)($Hashable$complex))$default__str__,
    $Hashable$complex$__eq__,
    $Hashable$complex$__ne__,
    $Hashable$complex$__hash__
};
 struct $Hashable$complex $Hashable$complex_instance = {&$Hashable$complex$methods};
 $Hashable$complex $Hashable$complex$witness = &$Hashable$complex_instance;
