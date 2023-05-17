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


B_NoneType mathQ_RealFunsD___init__ (mathQ_RealFuns W_self) {
    return B_None;
}
mathQ_RealFuns mathQ_RealFunsG_new() {
    mathQ_RealFuns $tmp = GC_MALLOC_EXPLICITLY_TYPED(sizeof(struct mathQ_RealFuns), mathQ_RealFunsG_methods.$GCdescr);
    $tmp->$class = &mathQ_RealFunsG_methods;
    mathQ_RealFunsG_methods.__init__($tmp);
    return $tmp;
}
struct mathQ_RealFunsG_class mathQ_RealFunsG_methods;
GC_word mathQ_RealFunsD_gcbm[GC_BITMAP_SIZE(struct mathQ_RealFuns)];
B_NoneType mathQ_RealFunsD_floatD___init__ (mathQ_RealFunsD_float W_self) {
    mathQ_RealFunsG_methods.__init__((mathQ_RealFuns)W_self);
    return B_None;
}

B_NoneType mathQ_RealFunsD_floatD___serialize__(mathQ_RealFunsD_float wit, $Serial$state state) {
    return B_None;
}

mathQ_RealFunsD_float mathQ_RealFunsD_floatD___deserialize__(mathQ_RealFunsD_float wit, $Serial$state state) {
    mathQ_RealFunsD_float res = $DNEW(mathQ_RealFunsD_float,state);
    return res;
}
B_float mathQ_RealFunsD_float$sqrt(mathQ_RealFunsD_float wit, B_float x) {
  return to$float(sqrt(x->val));
}
B_float mathQ_RealFunsD_float$exp(mathQ_RealFunsD_float wit, B_float x) {
  return to$float(exp(x->val));
}
B_float mathQ_RealFunsD_float$log(mathQ_RealFunsD_float wit, B_float x) {
  return to$float(log(x->val));
}
B_float mathQ_RealFunsD_float$sin(mathQ_RealFunsD_float wit, B_float x) {
  return to$float(sin(x->val));
}
B_float mathQ_RealFunsD_float$cos(mathQ_RealFunsD_float wit, B_float x) {
  return to$float(cos(x->val));
}
B_float mathQ_RealFunsD_float$tan(mathQ_RealFunsD_float wit, B_float x) {
  return to$float(tan(x->val));
}
B_float mathQ_RealFunsD_float$asin(mathQ_RealFunsD_float wit, B_float x) {
  return to$float(asin(x->val));
}
B_float mathQ_RealFunsD_float$acos(mathQ_RealFunsD_float wit, B_float x) {
  return to$float(acos(x->val));
}
B_float mathQ_RealFunsD_float$atan(mathQ_RealFunsD_float wit, B_float x) {
  return to$float(atan(x->val));
}
B_float mathQ_RealFunsD_float$sinh(mathQ_RealFunsD_float wit, B_float x) {
  return to$float(sinh(x->val));
}
B_float mathQ_RealFunsD_float$cosh(mathQ_RealFunsD_float wit, B_float x) {
  return to$float(cosh(x->val));
}
B_float mathQ_RealFunsD_float$tanh(mathQ_RealFunsD_float wit, B_float x) {
  return to$float(tanh(x->val));
}
B_float mathQ_RealFunsD_float$asinh(mathQ_RealFunsD_float wit, B_float x) {
  return to$float(asinh(x->val));
}
B_float mathQ_RealFunsD_float$acosh(mathQ_RealFunsD_float wit, B_float x) {
  return to$float(acosh(x->val));
}
B_float mathQ_RealFunsD_float$atanh(mathQ_RealFunsD_float wit, B_float x) {
  return to$float(atanh(x->val));
}

                      
mathQ_RealFunsD_float mathQ_RealFunsD_floatG_new() {
    mathQ_RealFunsD_float $tmp = GC_MALLOC_EXPLICITLY_TYPED(sizeof(struct mathQ_RealFunsD_float), mathQ_RealFunsD_floatG_methods.$GCdescr);
    $tmp->$class = &mathQ_RealFunsD_floatG_methods;
    mathQ_RealFunsD_floatG_methods.__init__($tmp);
    return $tmp;
}
struct mathQ_RealFunsD_floatG_class mathQ_RealFunsD_floatG_methods;
GC_word mathQ_RealFunsD_floatD_gcbm[GC_BITMAP_SIZE(struct mathQ_RealFunsD_float)];
int mathQ_done$ = 0;
void mathQ___init__ () {
    if (mathQ_done$) return;
    mathQ_done$ = 1;
    {
        memset(mathQ_RealFunsD_gcbm, 0xFF, sizeof(mathQ_RealFunsD_gcbm));
        mathQ_RealFunsG_methods.$GCdescr = GC_make_descriptor(mathQ_RealFunsD_gcbm, GC_WORD_LEN(struct mathQ_RealFuns));
        mathQ_RealFunsG_methods.$name = "mathQ_RealFuns";
        mathQ_RealFunsG_methods.$superclass = NULL;
        mathQ_RealFunsG_methods.__init__ = mathQ_RealFunsD___init__;
        $register(&mathQ_RealFunsG_methods);
    }
    {
        memset(mathQ_RealFunsD_floatD_gcbm, 0xFF, sizeof(mathQ_RealFunsD_floatD_gcbm));
        mathQ_RealFunsD_floatG_methods.$GCdescr = GC_make_descriptor(mathQ_RealFunsD_floatD_gcbm, GC_WORD_LEN(struct mathQ_RealFunsD_float));
        mathQ_RealFunsD_floatG_methods.$name = "mathQ_RealFunsD_float";
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

$WORD mathQ_sqrt (mathQ_RealFuns wit, $WORD x) {
  return wit->$class->sqrt(wit,x);
}
$WORD mathQ_exp (mathQ_RealFuns wit, $WORD x) {
  return wit->$class->exp(wit,x);
}
$WORD mathQ_log (mathQ_RealFuns wit, $WORD x) {
  return wit->$class->log(wit,x);
}
$WORD mathQ_sin (mathQ_RealFuns wit, $WORD x) {
  return wit->$class->sin(wit,x);
}
$WORD mathQ_cos (mathQ_RealFuns wit, $WORD x) {
  return wit->$class->cos(wit,x);
}
$WORD mathQ_tan (mathQ_RealFuns wit, $WORD x) {
  return wit->$class->tan(wit,x);
}
$WORD mathQ_asin (mathQ_RealFuns wit, $WORD x) {
  return wit->$class->asin(wit,x);
}
$WORD mathQ_acos (mathQ_RealFuns wit, $WORD x) {
  return wit->$class->acos(wit,x);
}
$WORD mathQ_atan (mathQ_RealFuns wit, $WORD x) {
  return wit->$class->atan(wit,x);
}
$WORD mathQ_sinh (mathQ_RealFuns wit, $WORD x) {
  return wit->$class->sinh(wit,x);
}
$WORD mathQ_cosh (mathQ_RealFuns wit, $WORD x) {
  return wit->$class->cosh(wit,x);
}
$WORD mathQ_tanh (mathQ_RealFuns wit, $WORD x) {
  return wit->$class->tanh(wit,x);
}
$WORD mathQ_asinh (mathQ_RealFuns wit, $WORD x) {
  return wit->$class->asinh(wit,x);
}
$WORD mathQ_acosh (mathQ_RealFuns wit, $WORD x) {
  return wit->$class->acosh(wit,x);
}
$WORD mathQ_atanh (mathQ_RealFuns wit, $WORD x) {
  return wit->$class->atanh(wit,x);
}
