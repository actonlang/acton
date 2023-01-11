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


$NoneType math$B_RealFunsD___init__ (math$B_RealFuns W_self) {
    return $None;
}
math$B_RealFuns math$B_RealFunsG_new() {
    math$B_RealFuns $tmp = malloc(sizeof(struct math$B_RealFuns));
    $tmp->$class = &math$B_RealFunsG_methods;
    math$B_RealFunsG_methods.__init__($tmp);
    return $tmp;
}
struct math$B_RealFunsG_class math$B_RealFunsG_methods;
$NoneType math$B_RealFunsB_floatD___init__ (math$B_RealFunsB_float W_self) {
    math$B_RealFunsG_methods.__init__((math$B_RealFuns)W_self);
    return $None;
}

$NoneType math$B_RealFunsB_floatD___serialize__(math$B_RealFunsB_float wit, $NoneType state) {
    return $None;
}

math$B_RealFunsB_float math$B_RealFunsB_floatD___deserialize__(math$B_RealFunsB_float wit, $NoneType state) {
    math$B_RealFunsB_float res = $DNEW(math$B_RealFunsB_float,state);
    return res;
}
B_float math$B_RealFunsB_float$sqrt(math$B_RealFunsB_float wit, B_float x) {
  return toB_float(sqrt(x->val));
}
B_float math$B_RealFunsB_float$exp(math$B_RealFunsB_float wit, B_float x) {
  return toB_float(exp(x->val));
}
B_float math$B_RealFunsB_float$log(math$B_RealFunsB_float wit, B_float x) {
  return toB_float(log(x->val));
}
B_float math$B_RealFunsB_float$sin(math$B_RealFunsB_float wit, B_float x) {
  return toB_float(sin(x->val));
}
B_float math$B_RealFunsB_float$cos(math$B_RealFunsB_float wit, B_float x) {
  return toB_float(cos(x->val));
}
B_float math$B_RealFunsB_float$tan(math$B_RealFunsB_float wit, B_float x) {
  return toB_float(tan(x->val));
}
B_float math$B_RealFunsB_float$asin(math$B_RealFunsB_float wit, B_float x) {
  return toB_float(asin(x->val));
}
B_float math$B_RealFunsB_float$acos(math$B_RealFunsB_float wit, B_float x) {
  return toB_float(acos(x->val));
}
B_float math$B_RealFunsB_float$atan(math$B_RealFunsB_float wit, B_float x) {
  return toB_float(atan(x->val));
}
B_float math$B_RealFunsB_float$sinh(math$B_RealFunsB_float wit, B_float x) {
  return toB_float(sinh(x->val));
}
B_float math$B_RealFunsB_float$cosh(math$B_RealFunsB_float wit, B_float x) {
  return toB_float(cosh(x->val));
}
B_float math$B_RealFunsB_float$tanh(math$B_RealFunsB_float wit, B_float x) {
  return toB_float(tanh(x->val));
}
B_float math$B_RealFunsB_float$asinh(math$B_RealFunsB_float wit, B_float x) {
  return toB_float(asinh(x->val));
}
B_float math$B_RealFunsB_float$acosh(math$B_RealFunsB_float wit, B_float x) {
  return toB_float(acosh(x->val));
}
B_float math$B_RealFunsB_float$atanh(math$B_RealFunsB_float wit, B_float x) {
  return toB_float(atanh(x->val));
}

                      
math$B_RealFunsB_float math$B_RealFunsB_floatG_new() {
    math$B_RealFunsB_float $tmp = malloc(sizeof(struct math$B_RealFunsB_float));
    $tmp->$class = &math$B_RealFunsB_floatG_methods;
    math$B_RealFunsB_floatG_methods.__init__($tmp);
    return $tmp;
}
struct math$B_RealFunsB_floatG_class math$B_RealFunsB_floatG_methods;
int math$$done$ = 0;
void math$D___init__ () {
    if (math$$done$) return;
    math$$done$ = 1;
    {
        math$B_RealFunsG_methods.$GCINFO = "math$B_RealFuns";
        math$B_RealFunsG_methods.$superclass = NULL;
        math$B_RealFunsG_methods.__init__ = math$B_RealFunsD___init__;
        $register(&math$B_RealFunsG_methods);
    }
    {
        math$B_RealFunsB_floatG_methods.$GCINFO = "math$B_RealFunsB_float";
        math$B_RealFunsB_floatG_methods.$superclass = ($SuperG_class)&math$B_RealFunsG_methods;
        math$B_RealFunsB_floatG_methods.__serialize__ = math$B_RealFunsB_floatD___serialize__,
        math$B_RealFunsB_floatG_methods.__deserialize__ = math$B_RealFunsB_floatD___deserialize__,
        math$B_RealFunsB_floatG_methods.__bool__ = (B_bool (*)(math$B_RealFunsB_float))$default__bool__,
        math$B_RealFunsB_floatG_methods.__str__ = (B_str (*)(math$B_RealFunsB_float))$default__str__,
        math$B_RealFunsB_floatG_methods.__repr__ = (B_str (*)(math$B_RealFunsB_float))$default__str__,
        math$B_RealFunsB_floatG_methods.__init__ = math$B_RealFunsB_floatD___init__;
        math$B_RealFunsB_floatG_methods.sqrt = math$B_RealFunsB_float$sqrt;        
        math$B_RealFunsB_floatG_methods.exp = math$B_RealFunsB_float$exp;        
        math$B_RealFunsB_floatG_methods.log = math$B_RealFunsB_float$log;        
        math$B_RealFunsB_floatG_methods.sin = math$B_RealFunsB_float$sin;        
        math$B_RealFunsB_floatG_methods.cos = math$B_RealFunsB_float$cos;        
        math$B_RealFunsB_floatG_methods.tan = math$B_RealFunsB_float$tan;        
        math$B_RealFunsB_floatG_methods.asin = math$B_RealFunsB_float$asin;        
        math$B_RealFunsB_floatG_methods.acos = math$B_RealFunsB_float$acos;        
        math$B_RealFunsB_floatG_methods.atan = math$B_RealFunsB_float$atan;        
        math$B_RealFunsB_floatG_methods.sinh = math$B_RealFunsB_float$sinh;        
        math$B_RealFunsB_floatG_methods.cosh = math$B_RealFunsB_float$cosh;        
        math$B_RealFunsB_floatG_methods.tanh = math$B_RealFunsB_float$tanh;        
        math$B_RealFunsB_floatG_methods.asinh = math$B_RealFunsB_float$asinh;        
        math$B_RealFunsB_floatG_methods.acosh = math$B_RealFunsB_float$acosh;        
        math$B_RealFunsB_floatG_methods.atanh = math$B_RealFunsB_float$atanh;        
        $register(&math$B_RealFunsB_floatG_methods);
    }
}

$WORD math$$sqrt (math$B_RealFuns wit, $WORD x) {
  return wit->$class->sqrt(wit,x);
}
$WORD math$$exp (math$B_RealFuns wit, $WORD x) {
  return wit->$class->exp(wit,x);
}
$WORD math$$log (math$B_RealFuns wit, $WORD x) {
  return wit->$class->log(wit,x);
}
$WORD math$$sin (math$B_RealFuns wit, $WORD x) {
  return wit->$class->sin(wit,x);
}
$WORD math$$cos (math$B_RealFuns wit, $WORD x) {
  return wit->$class->cos(wit,x);
}
$WORD math$$tan (math$B_RealFuns wit, $WORD x) {
  return wit->$class->tan(wit,x);
}
$WORD math$$asin (math$B_RealFuns wit, $WORD x) {
  return wit->$class->asin(wit,x);
}
$WORD math$$acos (math$B_RealFuns wit, $WORD x) {
  return wit->$class->acos(wit,x);
}
$WORD math$$atan (math$B_RealFuns wit, $WORD x) {
  return wit->$class->atan(wit,x);
}
$WORD math$$sinh (math$B_RealFuns wit, $WORD x) {
  return wit->$class->sinh(wit,x);
}
$WORD math$$cosh (math$B_RealFuns wit, $WORD x) {
  return wit->$class->cosh(wit,x);
}
$WORD math$$tanh (math$B_RealFuns wit, $WORD x) {
  return wit->$class->tanh(wit,x);
}
$WORD math$$asinh (math$B_RealFuns wit, $WORD x) {
  return wit->$class->asinh(wit,x);
}
$WORD math$$acosh (math$B_RealFuns wit, $WORD x) {
  return wit->$class->acosh(wit,x);
}
$WORD math$$atanh (math$B_RealFuns wit, $WORD x) {
  return wit->$class->atanh(wit,x);
}
