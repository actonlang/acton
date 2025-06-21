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

B_NoneType B_boolD___init__(B_bool self, B_value s){
    self->val = (s->$class->__bool__(s))->val;
    return B_None;
}

B_bool B_boolD___bool__(B_bool self) {
    return self;
}

B_str B_boolD___str__(B_bool self) {
    if (self->val)
        return to$str("True");
    else
        return to$str("False");
}

B_str B_boolD___repr__(B_bool self) {
    if (self->val)
        return to$str("True");
    else
        return to$str("False");
}

void B_boolD___serialize__(B_bool self, $Serial$state state) {
    $val_serialize(BOOL_ID,&self->val,state);
}

B_bool B_boolD___deserialize__(B_bool self, $Serial$state state) {
    return toB_bool((long)$val_deserialize(state));
}

B_bool B_boolG_new(B_value s) {
    return $NEW(B_bool, s);
}

B_bool toB_bool(long b) {
    B_bool res = acton_malloc(sizeof(struct B_bool));
    res->$class = &B_boolG_methods;
    res->val = b;
    return res;
}
    
long fromB_bool(B_bool b) {
    return b->val;
}

struct B_bool $t = {&B_boolG_methods,1L};
struct B_bool $f = {&B_boolG_methods,0L};

B_bool B_True = &$t;
B_bool B_False = &$f;


B_bool $default__bool__(B_value self) {
    return B_True;
}

// B_HashableD_bool ///////////////////////////////////////////////////////////////////////////////////////////////////////

B_bool B_HashableD_boolD___eq__(B_HashableD_bool wit, B_bool a, B_bool b) {
    return toB_bool(a->val == b->val);
}

B_bool B_HashableD_boolD___ne__(B_HashableD_bool wit, B_bool a, B_bool b) {
    return toB_bool(a->val != b->val);
}

B_NoneType B_HashableD_boolD_hash(B_HashableD_bool wit, B_bool a, B_hasher h) {
    zig_hash_wyhash_update(h->_hasher, to$bytesD_len((char *)&(a->val), 8));
    return B_None;
}
