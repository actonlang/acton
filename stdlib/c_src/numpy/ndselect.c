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

B_NoneType numpyQ_ndselectD___init__(numpyQ_ndselect self) {
    return B_None;
}

void numpyQ_ndselectD___serialize__(numpyQ_ndselect wit, $Serial$state state) {
}

numpyQ_ndselect numpyQ_ndselectD___deserialize__(numpyQ_ndselect wit, $Serial$state state) {
    numpyQ_ndselect res = $DNEW(numpyQ_ndselect,state);
    return res;
}

struct numpyQ_ndselectG_class numpyQ_ndselectG_methods = {
    0,
    "numpyQ_ndselect",
    UNASSIGNED,
    ($SuperG_class)&B_valueG_methods,
    numpyQ_ndselectD___init__,
    numpyQ_ndselectD___serialize__,
    numpyQ_ndselectD___deserialize__,
    (B_bool (*)(numpyQ_ndselect))$default__bool__,
    (B_str (*)(numpyQ_ndselect))$default__str__,
    (B_str (*)(numpyQ_ndselect))$default__str__
};

numpyQ_ndselect numpyQ_ndselectG_new() {
    numpyQ_ndselect $tmp = malloc(sizeof(numpyQ_ndselect));
    $tmp->$class = &numpyQ_ndselectG_methods;
    numpyQ_ndselectG_methods.__init__($tmp);
    return $tmp;
}





B_NoneType numpyQ_ndindexD___init__(numpyQ_ndindex self, B_int index) {
    self->index = index;
    return B_None;
}

void numpyQ_ndindexD___serialize__(numpyQ_ndindex self, $Serial$state state) {
    $step_serialize(self->index, state);
}

numpyQ_ndindex numpyQ_ndindexD___deserialize__(numpyQ_ndindex self, $Serial$state state) {
    numpyQ_ndindex res = $DNEW(numpyQ_ndindex,state);
    res->index = (B_int)$step_deserialize(state);
    return res;
}

struct numpyQ_ndindexG_class numpyQ_ndindexG_methods = {
    0,
    "numpyQ_ndindex",
    UNASSIGNED,
    ($SuperG_class)&B_valueG_methods,
    numpyQ_ndindexD___init__,
    numpyQ_ndindexD___serialize__,
    numpyQ_ndindexD___deserialize__,
    (B_bool (*)(numpyQ_ndindex))$default__bool__,
    (B_str (*)(numpyQ_ndindex))$default__str__,
    (B_str (*)(numpyQ_ndindex))$default__str__
};

numpyQ_ndindex numpyQ_ndindexG_new(B_int p$1) {
    numpyQ_ndindex $tmp = malloc(sizeof(numpyQ_ndindex));
    $tmp->$class = &numpyQ_ndindexG_methods;
    numpyQ_ndindexG_methods.__init__($tmp, p$1);
    return $tmp;
}




B_NoneType numpyQ_ndsliceD___init__(numpyQ_ndslice self, B_slice slc) {
    self->slc = slc;
    return B_None;
}

void numpyQ_ndsliceD___serialize__(numpyQ_ndslice self, $Serial$state state) {
    $step_serialize(self->slc, state);
}

numpyQ_ndslice numpyQ_ndsliceD___deserialize__(numpyQ_ndslice self, $Serial$state state) {
    numpyQ_ndslice res = $DNEW(numpyQ_ndslice,state);
    res->slc = (B_slice)$step_deserialize(state);
    return res;
}

struct numpyQ_ndsliceG_class numpyQ_ndsliceG_methods = {
    0,
    "numpyQ_ndslice",
    UNASSIGNED,
    ($SuperG_class)&B_valueG_methods,
    numpyQ_ndsliceD___init__,
    numpyQ_ndsliceD___serialize__,
    numpyQ_ndsliceD___deserialize__,
    (B_bool (*)(numpyQ_ndslice))$default__bool__,
    (B_str (*)(numpyQ_ndslice))$default__str__,
    (B_str (*)(numpyQ_ndslice))$default__str__
};

numpyQ_ndslice numpyQ_ndsliceG_new(B_slice p$1) {
    numpyQ_ndslice $tmp = malloc(sizeof(numpyQ_ndslice));
    $tmp->$class = &numpyQ_ndsliceG_methods;
    numpyQ_ndsliceG_methods.__init__($tmp, p$1);
    return $tmp;
}
