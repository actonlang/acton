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

int numpyQ_done = 0;

void numpy$D___init__() {
    if (numpyQ_done) return;
    numpyQ_done = 1;
    numpyQ_newaxis = toB_int(LONG_MIN);
    $register_force(ATOM_ID,&numpyQ_ndarrayG_methods);
    $register(&numpyQ_IteratorD_ndarrayG_methods);
    $register(&numpyQ_PrimitiveB_intG_methods);
    $register(&numpyQ_PrimitiveB_floatG_methods);
    $register(&numpyQ_ndselectG_methods);
    $register(&numpyQ_ndindexG_methods);
    $register(&numpyQ_ndsliceG_methods);
    $register(&numpyQ_IntegralD_ndarrayD_intG_methods);
    $register(&numpyQ_LogicalD_ndarrayD_intG_methods);
    $register(&numpyQ_MinusD_ndarrayD_intG_methods);
    $register(&numpyQ_DivD_ndarrayD_intG_methods);
    $register(&numpyQ_DivD_ndarrayD_floatG_methods);
    $register(&numpyQ_RealD_ndarrayG_methods);
    $register(&numpyQ_MinusD_ndarrayG_methods);
    $register(&numpyQ_SliceableD_ndarrayG_methods);
    $register(&numpyQ_CollectionD_ndarrayG_methods);
    $register(&numpyQ_RealFuns$math$ndarrayG_methods);
}

