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

#include "builtin.h"
#include "set_impl.h"
 

void B_set_serialize(B_set, $Serial$state);
B_set B_set_deserialize(B_set, $Serial$state);

struct B_OrdD_SetD_set B_OrdD_SetD_set_instance;
struct B_MinusD_SetD_set B_MinusD_SetD_set_instance;
struct B_LogicalD_SetD_set B_LogicalD_SetD_set_instance;

struct B_SetD_setG_class B_SetD_setG_methods = {
    "B_SetD_set",
    UNASSIGNED,
    ($SuperG_class)&B_SetG_methods,
    B_SetD_setD___init__,
    B_SetD_setD___serialize__,
    B_SetD_setD___deserialize__,
    (B_bool (*)(B_SetD_set))$default__bool__,
    (B_str (*)(B_SetD_set))$default__str__,
    (B_str (*)(B_SetD_set))$default__str__,
    B_SetD_setD___iter__,
    B_SetD_setD___fromiter__,
    B_SetD_setD___len__,
    B_SetD_setD___contains__,
    B_SetD_setD___containsnot__,
    B_SetD_set$isdisjoint,
    B_SetD_set$add,
    B_SetD_set$discard,
    B_SetD_set$pop
};   

struct B_OrdD_SetD_setG_class B_OrdD_SetD_setG_methods = {
    "B_OrdD_SetD_set",
    UNASSIGNED,
    ($SuperG_class)&B_OrdG_methods,
    B_OrdD_SetD_setD___init__,
    B_OrdD_SetD_setD___serialize__,
    B_OrdD_SetD_setD___deserialize__,
    (B_bool (*)(B_OrdD_SetD_set))$default__bool__,
    (B_str (*)(B_OrdD_SetD_set))$default__str__,
    (B_str (*)(B_OrdD_SetD_set))$default__str__,
    B_OrdD_SetD_setD___eq__,
    B_OrdD_SetD_setD___ne__,
    B_OrdD_SetD_setD___lt__,
    B_OrdD_SetD_setD___le__,
    B_OrdD_SetD_setD___gt__,
    B_OrdD_SetD_setD___ge__
};

struct B_MinusD_SetD_setG_class B_MinusD_SetD_setG_methods = {
    "B_MinusD_SetD_set",
    UNASSIGNED,
    ($SuperG_class)&B_MinusG_methods,
    B_MinusD_SetD_setD___init__,
    B_MinusD_SetD_setD___serialize__,
    B_MinusD_SetD_setD___deserialize__,
    (B_bool (*)(B_MinusD_SetD_set))$default__bool__,
    (B_str (*)(B_MinusD_SetD_set))$default__str__,
    (B_str (*)(B_MinusD_SetD_set))$default__str__,
    B_MinusD_SetD_setD___sub__,
    (B_set (*)(B_MinusD_SetD_set, B_set, B_set))B_MinusD___isub__,

};

struct B_LogicalD_SetD_setG_class B_LogicalD_SetD_setG_methods = {
    "B_LogicalD_SetD_set",
    UNASSIGNED,
    ($SuperG_class)&B_LogicalG_methods,
    B_LogicalD_SetD_setD___init__,
    B_LogicalD_SetD_setD___serialize__,
    B_LogicalD_SetD_setD___deserialize__,
    (B_bool (*)(B_LogicalD_SetD_set))$default__bool__,
    (B_str (*)(B_LogicalD_SetD_set))$default__str__,
    (B_str (*)(B_LogicalD_SetD_set))$default__str__,
    B_LogicalD_SetD_setD___and__,
    B_LogicalD_SetD_setD___or__,
    B_LogicalD_SetD_setD___xor__,
    (B_set (*)(B_LogicalD_SetD_set, B_set, B_set))B_LogicalD___iand__,
    (B_set (*)(B_LogicalD_SetD_set, B_set, B_set))B_LogicalD___ior__,
    (B_set (*)(B_LogicalD_SetD_set, B_set, B_set))B_LogicalD___ixor__
};

// B_Set

void B_SetD_setD___serialize__(B_SetD_set self, $Serial$state state) {
    $step_serialize(self->W_Ord, state);
    $step_serialize(self->W_Logical, state);
    $step_serialize(self->W_Minus, state);
    $step_serialize(self->W_EqD_AD_SetB_set, state);
    $step_serialize(self->W_HashableD_AD_SetB_set, state);
}

B_SetD_set B_SetD_setD___deserialize__(B_SetD_set self, $Serial$state state) {
    B_SetD_set res = $DNEW(B_SetD_set,state);
    res->W_Ord = (B_Ord)$step_deserialize(state);
    res->W_Logical = (B_Logical)$step_deserialize(state);
    res->W_Minus = (B_Minus)$step_deserialize(state);
    res->W_EqD_AD_SetB_set = (B_Eq)$step_deserialize(state);
    res->W_HashableD_AD_SetB_set = (B_Hashable)$step_deserialize(state);
    return res;
}


B_Iterator B_SetD_setD___iter__ (B_SetD_set wit, B_set set) {
    return B_set_iter(set);
}

B_set B_SetD_setD___fromiter__(B_SetD_set wit, B_Iterable wit2, $WORD iter) {
    return B_set_fromiter(wit->W_HashableD_AD_SetB_set,wit2->$class->__iter__(wit2,iter));
}

B_int B_SetD_setD___len__ (B_SetD_set wit, B_set set) {
    return toB_int(B_set_len(set));
}
B_bool B_SetD_setD___contains__ (B_SetD_set wit, B_set set, $WORD val) {
    return  toB_bool(B_set_contains(set,wit->W_HashableD_AD_SetB_set,val));
}

B_bool B_SetD_setD___containsnot__ (B_SetD_set wit, B_set set, $WORD val) {
    return  toB_bool(!B_set_contains(set,wit->W_HashableD_AD_SetB_set,val));
}

B_bool B_SetD_set$isdisjoint (B_SetD_set wit, B_set set, B_set other) {
    return toB_bool(B_set_isdisjoint(wit->W_HashableD_AD_SetB_set,set,other));
}

void B_SetD_set$add (B_SetD_set wit, B_set set, $WORD elem) {
    B_set_add(set,wit->W_HashableD_AD_SetB_set,elem);
}

void B_SetD_set$discard (B_SetD_set wit, B_set set, $WORD elem) {
    B_set_discard(set,wit->W_HashableD_AD_SetB_set,elem);
}

$WORD B_SetD_set$pop (B_SetD_set wit, B_set set) {
    return B_set_pop(set);
}

// B_Ord

void B_OrdD_SetD_setD___serialize__(B_OrdD_SetD_set self, $Serial$state state) {
    $step_serialize(self->W_Set, state);
}

B_OrdD_SetD_set B_OrdD_SetD_setD___deserialize__(B_OrdD_SetD_set self, $Serial$state state) {
    B_OrdD_SetD_set res = $DNEW(B_OrdD_SetD_set,state);
    res->W_Set = (B_Set)$step_deserialize(state);
    return res;
}

B_bool B_OrdD_SetD_setD___eq__ (B_OrdD_SetD_set wit, B_set a, B_set b) {
    return toB_bool(B_set_eq(((B_SetD_set)wit->W_Set)->W_HashableD_AD_SetB_set,a,b));
}
  
B_bool B_OrdD_SetD_setD___ne__ (B_OrdD_SetD_set wit, B_set a, B_set b) {
    return toB_bool(!B_set_eq(((B_SetD_set)wit->W_Set)->W_HashableD_AD_SetB_set,a,b));
}
  
B_bool B_OrdD_SetD_setD___lt__ (B_OrdD_SetD_set wit, B_set a, B_set b) {
    return toB_bool(B_set_lt(((B_SetD_set)wit->W_Set)->W_HashableD_AD_SetB_set,a,b));
}
  
B_bool B_OrdD_SetD_setD___le__ (B_OrdD_SetD_set wit, B_set a, B_set b) {
    return toB_bool(B_set_le(((B_SetD_set)wit->W_Set)->W_HashableD_AD_SetB_set,a,b));
}
  
B_bool B_OrdD_SetD_setD___gt__ (B_OrdD_SetD_set wit, B_set a, B_set b) {
    return toB_bool(B_set_gt(((B_SetD_set)wit->W_Set)->W_HashableD_AD_SetB_set,a,b));
}
  
B_bool B_OrdD_SetD_setD___ge__ (B_OrdD_SetD_set wit, B_set a, B_set b) {
    return toB_bool(B_set_ge(((B_SetD_set)wit->W_Set)->W_HashableD_AD_SetB_set,a,b));
}

// B_Minus

void B_LogicalD_SetD_setD___serialize__(B_LogicalD_SetD_set self, $Serial$state state) {
    $step_serialize(self->W_Set, state);
}

B_LogicalD_SetD_set B_LogicalD_SetD_setD___deserialize__(B_LogicalD_SetD_set self, $Serial$state state) {
    B_LogicalD_SetD_set res = $DNEW(B_LogicalD_SetD_set,state);
    res->W_Set = (B_Set)$step_deserialize(state);
    return res;
}

B_set B_MinusD_SetD_setD___sub__ (B_MinusD_SetD_set wit, B_set a, B_set b) {
    return B_set_difference(((B_SetD_set)wit->W_Set)->W_HashableD_AD_SetB_set,a,b);
}

// B_Logical

void B_MinusD_SetD_setD___serialize__(B_MinusD_SetD_set self, $Serial$state state) {
    $step_serialize(self->W_Set, state);
}

B_MinusD_SetD_set B_MinusD_SetD_setD___deserialize__(B_MinusD_SetD_set self, $Serial$state state) {
    B_MinusD_SetD_set res = $DNEW(B_MinusD_SetD_set,state);
    res->W_Set = (B_Set)$step_deserialize(state);
    return res;
}

B_set B_LogicalD_SetD_setD___and__(B_LogicalD_SetD_set wit, B_set a, B_set b) {
    return B_set_intersection(((B_SetD_set)wit->W_Set)->W_HashableD_AD_SetB_set,a,b);
}

B_set B_LogicalD_SetD_setD___or__ (B_LogicalD_SetD_set wit, B_set a, B_set b) {
    return B_set_union(((B_SetD_set)wit->W_Set)->W_HashableD_AD_SetB_set,a,b);
}

B_set B_LogicalD_SetD_setD___xor__(B_LogicalD_SetD_set wit, B_set a, B_set b) {
    return B_set_symmetric_difference(((B_SetD_set)wit->W_Set)->W_HashableD_AD_SetB_set,a,b);
}

// init and new

void B_OrdD_SetD_setD___init__(B_OrdD_SetD_set self, B_Set master) {
    self->W_Set = master;
}

void B_LogicalD_SetD_setD___init__(B_LogicalD_SetD_set self, B_Set master) {
    self->W_Set = master;
}

void B_MinusD_SetD_setD___init__(B_MinusD_SetD_set self, B_Set master) {
    self->W_Set = master;
}

B_SetD_set B_SetD_setG_new(B_Hashable h) {
    return $NEW(B_SetD_set, h);
}

void B_SetD_setD___init__(B_SetD_set self, B_Hashable h) {
    self->W_Ord = (B_Ord)$NEW(B_OrdD_SetD_set,(B_Set)self);
    self->W_Logical = (B_Logical)$NEW(B_LogicalD_SetD_set,(B_Set)self);
    self->W_Minus = (B_Minus)$NEW(B_MinusD_SetD_set,(B_Set)self);
    self->W_EqD_AD_SetB_set = (B_Eq)h;
    self->W_HashableD_AD_SetB_set = h;
}
   
