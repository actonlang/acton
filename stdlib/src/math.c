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

$NoneType math$$RealFuns$__init__(math$$RealFuns w$self) {
    return $None;
}
math$$RealFuns math$$RealFuns$new() {
    math$$RealFuns $tmp = malloc(sizeof(struct math$$RealFuns));
    $tmp->$class = &math$$RealFuns$methods;
    math$$RealFuns$methods.__init__($tmp);
    return $tmp;
}
struct math$$RealFuns$class math$$RealFuns$methods;
$NoneType math$$RealFuns$float$__init__(math$$RealFuns$float w$self) {
    math$$RealFuns$methods.__init__((math$$RealFuns)w$self);
    return $None;
}

$NoneType math$$RealFuns$float$__serialize__(math$$RealFuns$float wit, $Serial$state state) {
    return $None;
}

math$$RealFuns$float math$$RealFuns$float$__deserialize__(math$$RealFuns$float wit, $Serial$state state) {
    math$$RealFuns$float res = $DNEW(math$$RealFuns$float, state);
    return res;
}
$float math$$RealFuns$float$sqrt(math$$RealFuns$float wit, $float x) {
    return to$float(sqrt(x->val));
}
$float math$$RealFuns$float$exp(math$$RealFuns$float wit, $float x) {
    return to$float(exp(x->val));
}
$float math$$RealFuns$float$log(math$$RealFuns$float wit, $float x) {
    return to$float(log(x->val));
}
$float math$$RealFuns$float$sin(math$$RealFuns$float wit, $float x) {
    return to$float(sin(x->val));
}
$float math$$RealFuns$float$cos(math$$RealFuns$float wit, $float x) {
    return to$float(cos(x->val));
}
$float math$$RealFuns$float$tan(math$$RealFuns$float wit, $float x) {
    return to$float(tan(x->val));
}
$float math$$RealFuns$float$asin(math$$RealFuns$float wit, $float x) {
    return to$float(asin(x->val));
}
$float math$$RealFuns$float$acos(math$$RealFuns$float wit, $float x) {
    return to$float(acos(x->val));
}
$float math$$RealFuns$float$atan(math$$RealFuns$float wit, $float x) {
    return to$float(atan(x->val));
}
$float math$$RealFuns$float$sinh(math$$RealFuns$float wit, $float x) {
    return to$float(sinh(x->val));
}
$float math$$RealFuns$float$cosh(math$$RealFuns$float wit, $float x) {
    return to$float(cosh(x->val));
}
$float math$$RealFuns$float$tanh(math$$RealFuns$float wit, $float x) {
    return to$float(tanh(x->val));
}
$float math$$RealFuns$float$asinh(math$$RealFuns$float wit, $float x) {
    return to$float(asinh(x->val));
}
$float math$$RealFuns$float$acosh(math$$RealFuns$float wit, $float x) {
    return to$float(acosh(x->val));
}
$float math$$RealFuns$float$atanh(math$$RealFuns$float wit, $float x) {
    return to$float(atanh(x->val));
}

math$$RealFuns$float math$$RealFuns$float$new() {
    math$$RealFuns$float $tmp = malloc(sizeof(struct math$$RealFuns$float));
    $tmp->$class = &math$$RealFuns$float$methods;
    math$$RealFuns$float$methods.__init__($tmp);
    return $tmp;
}
struct math$$RealFuns$float$class math$$RealFuns$float$methods;
int math$$done$ = 0;
void math$$__init__() {
    if (math$$done$)
        return;
    math$$done$ = 1;
    {
        math$$RealFuns$methods.$GCINFO = "math$$RealFuns";
        math$$RealFuns$methods.$superclass = NULL;
        math$$RealFuns$methods.__init__ = math$$RealFuns$__init__;
        $register(&math$$RealFuns$methods);
    }
    {
        math$$RealFuns$float$methods.$GCINFO = "math$$RealFuns$float";
        math$$RealFuns$float$methods.$superclass = ($Super$class)&math$$RealFuns$methods;
        math$$RealFuns$float$methods.__serialize__ = math$$RealFuns$float$__serialize__,
        math$$RealFuns$float$methods.__deserialize__ = math$$RealFuns$float$__deserialize__,
        math$$RealFuns$float$methods.__bool__ = ($bool(*)(math$$RealFuns$float))$default__bool__,
        math$$RealFuns$float$methods.__str__ = ($str(*)(math$$RealFuns$float))$default__str__,
        math$$RealFuns$float$methods.__init__ = math$$RealFuns$float$__init__;
        math$$RealFuns$float$methods.sqrt = math$$RealFuns$float$sqrt;
        math$$RealFuns$float$methods.exp = math$$RealFuns$float$exp;
        math$$RealFuns$float$methods.log = math$$RealFuns$float$log;
        math$$RealFuns$float$methods.sin = math$$RealFuns$float$sin;
        math$$RealFuns$float$methods.cos = math$$RealFuns$float$cos;
        math$$RealFuns$float$methods.tan = math$$RealFuns$float$tan;
        math$$RealFuns$float$methods.asin = math$$RealFuns$float$asin;
        math$$RealFuns$float$methods.acos = math$$RealFuns$float$acos;
        math$$RealFuns$float$methods.atan = math$$RealFuns$float$atan;
        math$$RealFuns$float$methods.sinh = math$$RealFuns$float$sinh;
        math$$RealFuns$float$methods.cosh = math$$RealFuns$float$cosh;
        math$$RealFuns$float$methods.tanh = math$$RealFuns$float$tanh;
        math$$RealFuns$float$methods.asinh = math$$RealFuns$float$asinh;
        math$$RealFuns$float$methods.acosh = math$$RealFuns$float$acosh;
        math$$RealFuns$float$methods.atanh = math$$RealFuns$float$atanh;
        $register(&math$$RealFuns$float$methods);
    }
}

$WORD math$$sqrt(math$$RealFuns wit, $WORD x) {
    return wit->$class->sqrt(wit, x);
}
$WORD math$$exp(math$$RealFuns wit, $WORD x) {
    return wit->$class->exp(wit, x);
}
$WORD math$$log(math$$RealFuns wit, $WORD x) {
    return wit->$class->log(wit, x);
}
$WORD math$$sin(math$$RealFuns wit, $WORD x) {
    return wit->$class->sin(wit, x);
}
$WORD math$$cos(math$$RealFuns wit, $WORD x) {
    return wit->$class->cos(wit, x);
}
$WORD math$$tan(math$$RealFuns wit, $WORD x) {
    return wit->$class->tan(wit, x);
}
$WORD math$$asin(math$$RealFuns wit, $WORD x) {
    return wit->$class->asin(wit, x);
}
$WORD math$$acos(math$$RealFuns wit, $WORD x) {
    return wit->$class->acos(wit, x);
}
$WORD math$$atan(math$$RealFuns wit, $WORD x) {
    return wit->$class->atan(wit, x);
}
$WORD math$$sinh(math$$RealFuns wit, $WORD x) {
    return wit->$class->sinh(wit, x);
}
$WORD math$$cosh(math$$RealFuns wit, $WORD x) {
    return wit->$class->cosh(wit, x);
}
$WORD math$$tanh(math$$RealFuns wit, $WORD x) {
    return wit->$class->tanh(wit, x);
}
$WORD math$$asinh(math$$RealFuns wit, $WORD x) {
    return wit->$class->asinh(wit, x);
}
$WORD math$$acosh(math$$RealFuns wit, $WORD x) {
    return wit->$class->acosh(wit, x);
}
$WORD math$$atanh(math$$RealFuns wit, $WORD x) {
    return wit->$class->atanh(wit, x);
}
