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

B_Iterator B_IterableD_Iterator_iter(B_IterableD_Iterator wit, B_Iterator self) {
    return self;
}

B_IterableD_Iterator B_IterableD_IteratorG_new() {
    return $NEW(B_IterableD_Iterator);
}

struct B_IterableD_IteratorG_class B_IterableD_IteratorG_methods = {
    "B_IterableD_Iterator",
    UNASSIGNED,
    ($SuperG_class)&B_IterableG_methods,
    (void (*)(B_IterableD_Iterator))$default__init__,
    B_IterableD_IteratorD___serialize__,
    B_IterableD_IteratorD___deserialize__,
    (B_bool (*)(B_IterableD_Iterator))$default__bool__,
    (B_str (*)(B_IterableD_Iterator))$default__str__,
    (B_str (*)(B_IterableD_Iterator))$default__str__,
    B_IterableD_Iterator_iter
};

struct B_IterableD_Iterator B_IterableD_Iterator_instance = {&B_IterableD_IteratorG_methods};
B_IterableD_Iterator B_IterableD_IteratorG_witness = &B_IterableD_Iterator_instance;


struct B_IteratorG_class B_IteratorG_methods = {"B_Iterator",UNASSIGNED,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL}; // B_Iterator is an abstract class
struct B_Iterator B_IteratorD_instance = {&B_IteratorG_methods};
struct B_Iterator *B_IteratorG_witness = &B_IteratorD_instance;

void B_IterableD_IteratorD___serialize__( B_IterableD_Iterator self, $Serial$state state) {
}

B_IterableD_Iterator B_IterableD_IteratorD___deserialize__(B_IterableD_Iterator self, $Serial$state state) {
    B_IterableD_Iterator res = $DNEW(B_IterableD_Iterator,state);
    return res;
}

$WORD $next(B_Iterator it) {
    return it->$class->__next__(it);
}
