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

#include <limits.h>

int numpy$$done = 0;

void numpy$$__init__() {
    if (numpy$$done) return;
    numpy$$done = 1;
    $register_force(ATOM_ID,&numpy$$ndarray$methods);
    $register(&numpy$$Iterator$ndarray$methods);
    $register(&numpy$$Primitive$int$methods);
    $register(&numpy$$Primitive$float$methods);
    $register(&numpy$$ndselect$methods);
    $register(&numpy$$ndindex$methods);
    $register(&numpy$$ndslice$methods);
    $register(&numpy$$Integral$ndarray$int$methods);
    $register(&numpy$$Logical$ndarray$int$methods);
    $register(&numpy$$Minus$ndarray$int$methods);
    $register(&numpy$$Div$ndarray$int$methods);
    $register(&numpy$$Div$ndarray$float$methods);
    $register(&numpy$$Real$ndarray$methods);
    $register(&numpy$$Minus$ndarray$methods);
    $register(&numpy$$Sliceable$ndarray$methods);
    $register(&numpy$$Collection$ndarray$methods);
    $register(&numpy$$RealFuns$math$ndarray$methods);
    numpy$$newaxis = to$int(LONG_MIN);
}

