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

void numpy$$ndselectD___init__(numpy$$ndselect self) {
}

void numpy$$ndselectD___serialize__(numpy$$ndselect wit, $Serial$state state) {
}

numpy$$ndselect numpy$$ndselectD___deserialize__(numpy$$ndselect wit, $Serial$state state) {
    numpy$$ndselect res = $DNEW(numpy$$ndselect,state);
    return res;
}

struct numpy$$ndselectG_class numpy$$ndselectG_methods = {
    "numpy$$ndselect",
    UNASSIGNED,
    ($SuperG_class)&B_valueG_methods,
    numpy$$ndselectD___init__,
    numpy$$ndselectD___serialize__,
    numpy$$ndselectD___deserialize__,
    (B_bool (*)(numpy$$ndselect))$default__bool__,
    (B_str (*)(numpy$$ndselect))$default__str__,
    (B_str (*)(numpy$$ndselect))$default__str__
};

numpy$$ndselect numpy$$ndselectG_new() {
    numpy$$ndselect $tmp = malloc(sizeof(struct numpy$$ndselect));
    $tmp->$class = &numpy$$ndselectG_methods;
    numpy$$ndselectG_methods.__init__($tmp);
    return $tmp;
}





void numpy$$ndindexD___init__(numpy$$ndindex self, B_int index) {
    self->index = index;
}

void numpy$$ndindexD___serialize__(numpy$$ndindex self, $Serial$state state) {
    $step_serialize(self->index, state);
}

numpy$$ndindex numpy$$ndindexD___deserialize__(numpy$$ndindex self, $Serial$state state) {
    numpy$$ndindex res = $DNEW(numpy$$ndindex,state);
    res->index = (B_int)$step_deserialize(state);
    return res;
}

struct numpy$$ndindexG_class numpy$$ndindexG_methods = {
    "numpy$$ndindex",
    UNASSIGNED,
    ($SuperG_class)&B_valueG_methods,
    numpy$$ndindexD___init__,
    numpy$$ndindexD___serialize__,
    numpy$$ndindexD___deserialize__,
    (B_bool (*)(numpy$$ndindex))$default__bool__,
    (B_str (*)(numpy$$ndindex))$default__str__,
    (B_str (*)(numpy$$ndindex))$default__str__
};

numpy$$ndindex numpy$$ndindexG_new(B_int p$1) {
    numpy$$ndindex $tmp = malloc(sizeof(struct numpy$$ndindex));
    $tmp->$class = &numpy$$ndindexG_methods;
    numpy$$ndindexG_methods.__init__($tmp, p$1);
    return $tmp;
}




void numpy$$ndsliceD___init__(numpy$$ndslice self, B_slice slc) {
    self->slc = slc;
}

void numpy$$ndsliceD___serialize__(numpy$$ndslice self, $Serial$state state) {
    $step_serialize(self->slc, state);
}

numpy$$ndslice numpy$$ndsliceD___deserialize__(numpy$$ndslice self, $Serial$state state) {
    numpy$$ndslice res = $DNEW(numpy$$ndslice,state);
    res->slc = (B_slice)$step_deserialize(state);
    return res;
}

struct numpy$$ndsliceG_class numpy$$ndsliceG_methods = {
    "numpy$$ndslice",
    UNASSIGNED,
    ($SuperG_class)&B_valueG_methods,
    numpy$$ndsliceD___init__,
    numpy$$ndsliceD___serialize__,
    numpy$$ndsliceD___deserialize__,
    (B_bool (*)(numpy$$ndslice))$default__bool__,
    (B_str (*)(numpy$$ndslice))$default__str__,
    (B_str (*)(numpy$$ndslice))$default__str__
};

numpy$$ndslice numpy$$ndsliceG_new(B_slice p$1) {
    numpy$$ndslice $tmp = malloc(sizeof(struct numpy$$ndslice));
    $tmp->$class = &numpy$$ndsliceG_methods;
    numpy$$ndsliceG_methods.__init__($tmp, p$1);
    return $tmp;
}
