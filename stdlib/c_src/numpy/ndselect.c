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

void numpy$$ndselect$__init__(numpy$$ndselect self) {
}

void numpy$$ndselect$__serialize__(numpy$$ndselect wit, $Serial$state state) {
}

numpy$$ndselect numpy$$ndselect$__deserialize__(numpy$$ndselect wit, $Serial$state state) {
    numpy$$ndselect res = $DNEW(numpy$$ndselect,state);
    return res;
}

struct numpy$$ndselect$class numpy$$ndselect$methods = {
    "numpy$$ndselect",
    UNASSIGNED,
    ($Super$class)&$value$methods,
    numpy$$ndselect$__init__,
    numpy$$ndselect$__serialize__,
    numpy$$ndselect$__deserialize__,
    ($bool (*)(numpy$$ndselect))$default__bool__,
    ($str (*)(numpy$$ndselect))$default__str__,
};

numpy$$ndselect numpy$$ndselect$new() {
    numpy$$ndselect $tmp = malloc(sizeof(struct numpy$$ndselect));
    $tmp->$class = &numpy$$ndselect$methods;
    numpy$$ndselect$methods.__init__($tmp);
    return $tmp;
}





void numpy$$ndindex$__init__(numpy$$ndindex self, $int index) {
    self->index = index;
}

void numpy$$ndindex$__serialize__(numpy$$ndindex self, $Serial$state state) {
    $step_serialize(self->index, state);
}

numpy$$ndindex numpy$$ndindex$__deserialize__(numpy$$ndindex self, $Serial$state state) {
    numpy$$ndindex res = $DNEW(numpy$$ndindex,state);
    res->index = ($int)$step_deserialize(state);
    return res;
}

struct numpy$$ndindex$class numpy$$ndindex$methods = {
    "numpy$$ndindex",
    UNASSIGNED,
    ($Super$class)&$value$methods,
    numpy$$ndindex$__init__,
    numpy$$ndindex$__serialize__,
    numpy$$ndindex$__deserialize__,
    ($bool (*)(numpy$$ndindex))$default__bool__,
    ($str (*)(numpy$$ndindex))$default__str__,
};

numpy$$ndindex numpy$$ndindex$new($int p$1) {
    numpy$$ndindex $tmp = malloc(sizeof(struct numpy$$ndindex));
    $tmp->$class = &numpy$$ndindex$methods;
    numpy$$ndindex$methods.__init__($tmp, p$1);
    return $tmp;
}




void numpy$$ndslice$__init__(numpy$$ndslice self, $slice slc) {
    self->slc = slc;
}

void numpy$$ndslice$__serialize__(numpy$$ndslice self, $Serial$state state) {
    $step_serialize(self->slc, state);
}

numpy$$ndslice numpy$$ndslice$__deserialize__(numpy$$ndslice self, $Serial$state state) {
    numpy$$ndslice res = $DNEW(numpy$$ndslice,state);
    res->slc = ($slice)$step_deserialize(state);
    return res;
}

struct numpy$$ndslice$class numpy$$ndslice$methods = {
    "numpy$$ndslice",
    UNASSIGNED,
    ($Super$class)&$value$methods,
    numpy$$ndslice$__init__,
    numpy$$ndslice$__serialize__,
    numpy$$ndslice$__deserialize__,
    ($bool (*)(numpy$$ndslice))$default__bool__,
    ($str (*)(numpy$$ndslice))$default__str__,
};

numpy$$ndslice numpy$$ndslice$new($slice p$1) {
    numpy$$ndslice $tmp = malloc(sizeof(struct numpy$$ndslice));
    $tmp->$class = &numpy$$ndslice$methods;
    numpy$$ndslice$methods.__init__($tmp, p$1);
    return $tmp;
}
