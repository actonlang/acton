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

#include "math.h"


$NoneType mathQ_RealFunsD___init__ (mathQ_RealFuns W_self) {
    return $None;
}
mathQ_RealFuns mathQ_RealFunsG_new() {
    mathQ_RealFuns $tmp = malloc(sizeof(struct mathQ_RealFuns));
    $tmp->$class = &mathQ_RealFunsG_methods;
    mathQ_RealFunsG_methods.__init__($tmp);
    return $tmp;
}
struct mathQ_RealFunsG_class mathQ_RealFunsG_methods;
$NoneType mathQ_RealFunsD_floatD___init__ (mathQ_RealFunsD_float W_self) {
    mathQ_RealFunsG_methods.__init__((mathQ_RealFuns)W_self);
    return $None;
}

$NoneType mathQ_RealFunsD_floatD___serialize__(mathQ_RealFunsD_float wit, $Serial$state state) {
    return $None;
}

mathQ_RealFunsD_float mathQ_RealFunsD_floatD___deserialize__(mathQ_RealFunsD_float wit, $Serial$state state) {
    mathQ_RealFunsD_float res = $DNEW(mathQ_RealFunsD_float,state);
    return res;
}
B_float mathQ_RealFunsD_float$sqrt(mathQ_RealFunsD_float wit, B_float x) {
  return toB_float(sqrt(x->val));
}
B_float mathQ_RealFunsD_float$exp(mathQ_RealFunsD_float wit, B_float x) {
  return toB_float(exp(x->val));
}
B_float mathQ_RealFunsD_float$log(mathQ_RealFunsD_float wit, B_float x) {
  return toB_float(log(x->val));
}
B_float mathQ_RealFunsD_float$sin(mathQ_RealFunsD_float wit, B_float x) {
  return toB_float(sin(x->val));
}
B_float mathQ_RealFunsD_float$cos(mathQ_RealFunsD_float wit, B_float x) {
  return toB_float(cos(x->val));
}
B_float mathQ_RealFunsD_float$tan(mathQ_RealFunsD_float wit, B_float x) {
  return toB_float(tan(x->val));
}
B_float mathQ_RealFunsD_float$asin(mathQ_RealFunsD_float wit, B_float x) {
  return toB_float(asin(x->val));
}
B_float mathQ_RealFunsD_float$acos(mathQ_RealFunsD_float wit, B_float x) {
  return toB_float(acos(x->val));
}
B_float mathQ_RealFunsD_float$atan(mathQ_RealFunsD_float wit, B_float x) {
  return toB_float(atan(x->val));
}
B_float mathQ_RealFunsD_float$sinh(mathQ_RealFunsD_float wit, B_float x) {
  return toB_float(sinh(x->val));
}
B_float mathQ_RealFunsD_float$cosh(mathQ_RealFunsD_float wit, B_float x) {
  return toB_float(cosh(x->val));
}
B_float mathQ_RealFunsD_float$tanh(mathQ_RealFunsD_float wit, B_float x) {
  return toB_float(tanh(x->val));
}
B_float mathQ_RealFunsD_float$asinh(mathQ_RealFunsD_float wit, B_float x) {
  return toB_float(asinh(x->val));
}
B_float mathQ_RealFunsD_float$acosh(mathQ_RealFunsD_float wit, B_float x) {
  return toB_float(acosh(x->val));
}
B_float mathQ_RealFunsD_float$atanh(mathQ_RealFunsD_float wit, B_float x) {
  return toB_float(atanh(x->val));
}

                      
mathQ_RealFunsD_float mathQ_RealFunsD_floatG_new() {
    mathQ_RealFunsD_float $tmp = malloc(sizeof(struct mathQ_RealFunsD_float));
    $tmp->$class = &mathQ_RealFunsD_floatG_methods;
    mathQ_RealFunsD_floatG_methods.__init__($tmp);
    return $tmp;
}
struct mathQ_RealFunsD_floatG_class mathQ_RealFunsD_floatG_methods;
int math$$done$ = 0;
void math$D___init__ () {
    if (math$$done$) return;
    math$$done$ = 1;
    {
        mathQ_RealFunsG_methods.$GCINFO = "mathQ_RealFuns";
        mathQ_RealFunsG_methods.$superclass = NULL;
        mathQ_RealFunsG_methods.__init__ = mathQ_RealFunsD___init__;
        $register(&mathQ_RealFunsG_methods);
    }
    {
        mathQ_RealFunsD_floatG_methods.$GCINFO = "mathQ_RealFunsD_float";
        mathQ_RealFunsD_floatG_methods.$superclass = ($SuperG_class)&mathQ_RealFunsG_methods;
        mathQ_RealFunsD_floatG_methods.__serialize__ = mathQ_RealFunsD_floatD___serialize__,
        mathQ_RealFunsD_floatG_methods.__deserialize__ = mathQ_RealFunsD_floatD___deserialize__,
        mathQ_RealFunsD_floatG_methods.__bool__ = (B_bool (*)(mathQ_RealFunsD_float))$default__bool__,
        mathQ_RealFunsD_floatG_methods.__str__ = (B_str (*)(mathQ_RealFunsD_float))$default__str__,
        mathQ_RealFunsD_floatG_methods.__repr__ = (B_str (*)(mathQ_RealFunsD_float))$default__str__,
        mathQ_RealFunsD_floatG_methods.__init__ = mathQ_RealFunsD_floatD___init__;
        mathQ_RealFunsD_floatG_methods.sqrt = mathQ_RealFunsD_float$sqrt;        
        mathQ_RealFunsD_floatG_methods.exp = mathQ_RealFunsD_float$exp;        
        mathQ_RealFunsD_floatG_methods.log = mathQ_RealFunsD_float$log;        
        mathQ_RealFunsD_floatG_methods.sin = mathQ_RealFunsD_float$sin;        
        mathQ_RealFunsD_floatG_methods.cos = mathQ_RealFunsD_float$cos;        
        mathQ_RealFunsD_floatG_methods.tan = mathQ_RealFunsD_float$tan;        
        mathQ_RealFunsD_floatG_methods.asin = mathQ_RealFunsD_float$asin;        
        mathQ_RealFunsD_floatG_methods.acos = mathQ_RealFunsD_float$acos;        
        mathQ_RealFunsD_floatG_methods.atan = mathQ_RealFunsD_float$atan;        
        mathQ_RealFunsD_floatG_methods.sinh = mathQ_RealFunsD_float$sinh;        
        mathQ_RealFunsD_floatG_methods.cosh = mathQ_RealFunsD_float$cosh;        
        mathQ_RealFunsD_floatG_methods.tanh = mathQ_RealFunsD_float$tanh;        
        mathQ_RealFunsD_floatG_methods.asinh = mathQ_RealFunsD_float$asinh;        
        mathQ_RealFunsD_floatG_methods.acosh = mathQ_RealFunsD_float$acosh;        
        mathQ_RealFunsD_floatG_methods.atanh = mathQ_RealFunsD_float$atanh;        
        $register(&mathQ_RealFunsD_floatG_methods);
    }
}

$WORD math$$sqrt (mathQ_RealFuns wit, $WORD x) {
  return wit->$class->sqrt(wit,x);
}
$WORD math$$exp (mathQ_RealFuns wit, $WORD x) {
  return wit->$class->exp(wit,x);
}
$WORD math$$log (mathQ_RealFuns wit, $WORD x) {
  return wit->$class->log(wit,x);
}
$WORD math$$sin (mathQ_RealFuns wit, $WORD x) {
  return wit->$class->sin(wit,x);
}
$WORD math$$cos (mathQ_RealFuns wit, $WORD x) {
  return wit->$class->cos(wit,x);
}
$WORD math$$tan (mathQ_RealFuns wit, $WORD x) {
  return wit->$class->tan(wit,x);
}
$WORD math$$asin (mathQ_RealFuns wit, $WORD x) {
  return wit->$class->asin(wit,x);
}
$WORD math$$acos (mathQ_RealFuns wit, $WORD x) {
  return wit->$class->acos(wit,x);
}
$WORD math$$atan (mathQ_RealFuns wit, $WORD x) {
  return wit->$class->atan(wit,x);
}
$WORD math$$sinh (mathQ_RealFuns wit, $WORD x) {
  return wit->$class->sinh(wit,x);
}
$WORD math$$cosh (mathQ_RealFuns wit, $WORD x) {
  return wit->$class->cosh(wit,x);
}
$WORD math$$tanh (mathQ_RealFuns wit, $WORD x) {
  return wit->$class->tanh(wit,x);
}
$WORD math$$asinh (mathQ_RealFuns wit, $WORD x) {
  return wit->$class->asinh(wit,x);
}
$WORD math$$acosh (mathQ_RealFuns wit, $WORD x) {
  return wit->$class->acosh(wit,x);
}
$WORD math$$atanh (mathQ_RealFuns wit, $WORD x) {
  return wit->$class->atanh(wit,x);
}
