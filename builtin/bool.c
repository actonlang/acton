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



// Serialization ///////////////////////////////////////////////////////////////////////

void B_boolD_init(B_bool self, B_value s){
    self->val = (s->$class->__bool__(s))->val;
}

B_bool B_boolD_bool(B_bool self) {
    return self;
}

B_str B_boolD_str(B_bool self) {
    if (self->val)
        return to$str("True");
    else
        return to$str("False");
}

void B_boolD_serialize(B_bool self, $NoneType state) {
    $val_serialize(BOOL_ID,&self->val,state);
}

B_bool B_boolD_deserialize(B_bool self, $NoneType state) {
    return toB_bool((long)$val_deserialize(state));
}

struct B_boolG_class B_boolG_methods = {
    "B_bool",
    UNASSIGNED,
    ($SuperG_class)&B_atomG_methods,
    B_boolD_init,
    B_boolD_serialize,
    B_boolD_deserialize,
    B_boolD_bool,
    B_boolD_str,
    B_boolD_str
};

B_bool B_boolG_new(B_value s) {
    return $NEW(B_bool, s);
}

B_bool toB_bool(long b) {
    B_bool res = malloc(sizeof(struct B_bool));
    res->$class = &B_boolG_methods;
    res->val = b;
    return res;
}
    
long fromB_bool(B_bool b) {
    return b->val;
}

struct B_bool $t = {&B_boolG_methods,1L};
struct B_bool $f = {&B_boolG_methods,0L};

B_bool $True = &$t;
B_bool $False = &$f;


B_bool $default__bool__(B_value self) {
    return $True;
}

// B_HashableD_bool ///////////////////////////////////////////////////////////////////////////////////////////////////////

struct B_HashableD_boolG_class B_HashableD_boolG_methods = {
    "B_HashableD_bool",
    UNASSIGNED,
    ($SuperG_class)&B_HashableG_methods,
    B_HashableD_boolD___init__,
    B_HashableD_boolD___serialize__,
    B_HashableD_boolD___deserialize__,
    (B_bool (*)(B_HashableD_bool))$default__bool__,
    (B_str (*)(B_HashableD_bool))$default__str__,
    (B_str (*)(B_HashableD_bool))$default__str__,
    B_HashableD_boolD___eq__,
    B_HashableD_boolD___ne__,
    B_HashableD_boolD___hash__
};

B_HashableD_bool B_HashableD_boolG_new() {
    return $NEW(B_HashableD_bool);
}

void B_HashableD_boolD___init__(B_HashableD_bool self) {
    return;
}
void B_HashableD_boolD___serialize__(B_HashableD_bool self, $NoneType state) {
}

B_HashableD_bool B_HashableD_boolD___deserialize__(B_HashableD_bool self, $NoneType state) {
    B_HashableD_bool res = $DNEW(B_HashableD_bool,state);
    return res;
}

B_bool B_HashableD_boolD___eq__(B_HashableD_bool wit, B_bool a, B_bool b) {
    return toB_bool(a->val == b->val);
}

B_bool B_HashableD_boolD___ne__(B_HashableD_bool wit, B_bool a, B_bool b) {
    return toB_bool(a->val != b->val);
}

B_int B_HashableD_boolD___hash__(B_HashableD_bool wit, B_bool a) {
    return toB_int(B_i64D_hash((B_i64)a));
}
struct B_HashableD_bool B_HashableD_bool_instance = {&B_HashableD_boolG_methods};
B_HashableD_bool B_HashableD_boolG_witness = &B_HashableD_bool_instance;

