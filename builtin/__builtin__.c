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

$WORD B_EqD___ne__(B_Eq wit, $WORD a, $WORD b) {
    return toB_bool(!wit->$class->__eq__(wit,a,b)->val);
}

$WORD B_OrdD___gt__(B_Ord wit, $WORD a, $WORD b) {
    return wit->$class->__lt__(wit,b,a);
}

$WORD B_OrdD___ge__(B_Ord wit, $WORD a, $WORD b) {
    return wit->$class->__le__(wit,b,a);
}

$WORD $PlusD___iadd__ ($Plus wit, $WORD a, $WORD b) {
    return wit->$class->__add__(wit, a, b);
}

$WORD B_MinusD___isub__ (B_Minus wit, $WORD a, $WORD b) {
    return wit->$class->__sub__(wit, a, b);
}

$WORD B_LogicalD___iand__ (B_Logical wit, $WORD a, $WORD b) {
    return wit->$class->__and__(wit, a, b);
}

$WORD B_LogicalD___ior__ (B_Logical wit, $WORD a, $WORD b) {
    return wit->$class->__or__(wit, a, b);
}

$WORD B_LogicalD___ixor__ (B_Logical wit, $WORD a, $WORD b) {
    return wit->$class->__xor__(wit, a, b);
}

$WORD B_TimesD___imul__ (B_Times wit, $WORD a, $WORD b) {
    return wit->$class->__mul__(wit, a, b);
}

$WORD B_DivD___itruediv__ (B_Div wit, $WORD a, $WORD b) {
    return wit->$class->__truediv__(wit, a, b);
}

$WORD B_NumberD___ipow__ (B_Number wit, $WORD a, $WORD b) {
    return wit->$class->__pow__(wit, a, b);
}

$WORD B_IntegralD___ifloordiv__ (B_Integral wit, $WORD a, $WORD b) {
    return wit->$class->__floordiv__(wit, a, b);
}

$WORD B_IntegralD___imod__ (B_Integral wit, $WORD a, $WORD b) {
    return wit->$class->__mod__(wit, a, b);
}

$WORD B_IntegralD___ilshift__ (B_Integral wit, $WORD a, B_int b) {
    return wit->$class->__lshift__(wit, a, b);
}

$WORD B_IntegralD___irshift__ (B_Integral wit, $WORD a, B_int b) {
    return wit->$class->__rshift__(wit, a, b);
}

struct B_EqG_class B_EqG_methods = {"B_EqG_class", UNASSIGNED, NULL, (void (*)(B_Eq))$default__init__, NULL, NULL, (B_bool (*)(B_Eq))$default__bool__,  (B_str (*)(B_Eq))$default__str__, (B_str (*)(B_Eq))$default__str__,
                                NULL, NULL};

B_Eq B_EqG_new() {
    B_Eq res = malloc(sizeof(struct B_Eq));
    res->$class = &B_EqG_methods;
    return res;
}

struct B_OrdG_class B_OrdG_methods = {"B_OrdG_class", UNASSIGNED, ($SuperG_class)&B_EqG_methods, (void (*)(B_Ord))$default__init__, NULL, NULL, (B_bool (*)(B_Ord))$default__bool__,  (B_str (*)(B_Ord))$default__str__, (B_str (*)(B_Ord))$default__str__,
                                  NULL, NULL, NULL, NULL, NULL, NULL};

B_Ord B_OrdG_new() {
    B_Ord res = malloc(sizeof(struct B_Eq));
    res->$class = &B_OrdG_methods;
    return res;
}

struct B_LogicalG_class B_LogicalG_methods = {"B_LogicalG_class", UNASSIGNED, NULL, (void (*)(B_Logical))$default__init__, NULL, NULL, (B_bool (*)(B_Logical))$default__bool__,  (B_str (*)(B_Logical))$default__str__, (B_str (*)(B_Logical))$default__str__,
                                          NULL, NULL, NULL};

B_Logical B_LogicalG_new() {
    B_Logical res = malloc(sizeof(struct B_Logical));
    res->$class = &B_LogicalG_methods;
    return res;
}

struct $PlusG_class $PlusG_methods = {"$PlusG_class", UNASSIGNED, NULL, (void (*)($Plus))$default__init__, NULL, NULL, (B_bool (*)($Plus))$default__bool__,  (B_str (*)($Plus))$default__str__, (B_str (*)($Plus))$default__str__,
                                    NULL, $PlusD___iadd__};

$Plus $PlusG_new() {
    $Plus res = malloc(sizeof(struct $Plus));
    res->$class = &$PlusG_methods;
    return res;
}

struct B_TimesG_class B_TimesG_methods = {"B_TimesG_class", UNASSIGNED, NULL, (void (*)(B_Times))$default__init__, NULL, NULL, (B_bool (*)(B_Times))$default__bool__,  (B_str (*)(B_Times))$default__str__, (B_str (*)(B_Times))$default__str__,
                                      NULL, ($WORD (*)(B_Times,$WORD,$WORD))$PlusD___iadd__, B_TimesD___imul__};

B_Times B_TimesG_new() {
    B_Times res = malloc(sizeof(struct B_Times));
    res->$class = &B_TimesG_methods;
    return res;
}

struct B_DivG_class B_DivG_methods = {"B_DivG_class", UNASSIGNED, NULL, (void (*)(B_Div))$default__init__, NULL, NULL, (B_bool (*)(B_Div))$default__bool__,  (B_str (*)(B_Div))$default__str__, (B_str (*)(B_Div))$default__str__,
                                  NULL, B_DivD___itruediv__};

B_Div B_DivG_new() {
    B_Div res = malloc(sizeof(struct B_Div));
    res->$class = &B_DivG_methods;
    return res;
}

struct B_MinusG_class B_MinusG_methods = {"B_MinusG_class", UNASSIGNED, NULL, (void (*)(B_Minus))$default__init__, NULL, NULL, (B_bool (*)(B_Minus))$default__bool__,  (B_str (*)(B_Minus))$default__str__, (B_str (*)(B_Minus))$default__str__,
                                      NULL, B_MinusD___isub__};

B_Minus B_MinusG_new() {
    B_Minus res = malloc(sizeof(struct B_Minus));
    res->$class = &B_MinusG_methods;
    return res;
}

struct B_HashableG_class B_HashableG_methods = {"B_HashableG_class", UNASSIGNED, NULL, (void (*)(B_Hashable))$default__init__, NULL, NULL, (B_bool (*)(B_Hashable))$default__bool__,  (B_str (*)(B_Hashable))$default__str__, (B_str (*)(B_Hashable))$default__str__,
                                            NULL, NULL, NULL};

B_Hashable B_HashableG_new() {
    B_Hashable res = malloc(sizeof(struct B_Hashable));
    res->$class = &B_HashableG_methods;
    return res;
}

static void B_IndexedD___init__(B_Indexed self, B_Eq W_EqD_AD_Indexed) {
    self->W_EqD_AD_Indexed = W_EqD_AD_Indexed;
}

struct B_IndexedG_class B_IndexedG_methods = {"B_IndexedG_class", UNASSIGNED, NULL, B_IndexedD___init__, NULL, NULL, (B_bool (*)(B_Indexed))$default__bool__,  (B_str (*)(B_Indexed))$default__str__, (B_str (*)(B_Indexed))$default__str__, 
                                          NULL, NULL, NULL};

B_Indexed B_IndexedG_new(B_Eq W_EqD_AD_Indexed) {
    B_Indexed res = malloc(sizeof(struct B_Indexed));
    res->$class = &B_IndexedG_methods;
    res->W_EqD_AD_Indexed = W_EqD_AD_Indexed;
    return res;
}

struct B_SliceableG_class B_SliceableG_methods = {"B_SliceableG_class", UNASSIGNED, ($SuperG_class)&B_IndexedG_methods, (void (*)(B_Sliceable))$default__init__, NULL, NULL, (B_bool (*)(B_Sliceable))$default__bool__,  (B_str (*)(B_Sliceable))$default__str__,(B_str (*)(B_Sliceable))$default__str__,
                                              NULL, NULL, NULL, NULL, NULL, NULL};

B_Sliceable B_SliceableG_new() {
    B_Sliceable res = malloc(sizeof(struct B_Sliceable));
    res->$class = &B_SliceableG_methods;
    return res;
}

struct B_IterableG_class B_IterableG_methods = {"B_IterableG_class", UNASSIGNED, NULL, (void (*)(B_Iterable))$default__init__, NULL, NULL, (B_bool (*)(B_Iterable))$default__bool__,  (B_str (*)(B_Iterable))$default__str__,(B_str (*)(B_Iterable))$default__str__,
                                            NULL};

B_Iterable B_IterableG_new() {
    B_Iterable res = malloc(sizeof(struct B_Iterable));
    res->$class = &B_IterableG_methods;
    return res;
}

struct B_CollectionG_class B_CollectionG_methods = {"B_CollectionG_class", UNASSIGNED, ($SuperG_class)&B_IterableG_methods, (void (*)(B_Collection))$default__init__, NULL, NULL, (B_bool (*)(B_Collection))$default__bool__,  (B_str (*)(B_Collection))$default__str__, (B_str (*)(B_Collection))$default__str__,
                                                NULL, NULL, NULL};

B_Collection B_CollectionG_new() {
    B_Collection res = malloc(sizeof(struct B_Collection));
    res->$class = &B_CollectionG_methods;
    return res;
}

static void B_ContainerD___init__(B_Container self, B_Eq W_EqD_AD_Container) {
    self->W_EqD_AD_Container = W_EqD_AD_Container;
}

struct B_ContainerG_class B_ContainerG_methods = {"B_ContainerG_class", UNASSIGNED, ($SuperG_class)&B_CollectionG_methods, B_ContainerD___init__, NULL, NULL, (B_bool (*)(B_Container))$default__bool__,  (B_str (*)(B_Container))$default__str__, (B_str (*)(B_Container))$default__str__,
                                              NULL, NULL, NULL, NULL, NULL};

B_Container B_ContainerG_new(B_Eq W_EqD_AD_Container) {
    B_Container res = malloc(sizeof(struct B_Container));
    res->$class = &B_ContainerG_methods;
    res->W_EqD_AD_Container = W_EqD_AD_Container;
    return res;
}

static void B_SequenceD___init__(B_Sequence self) {
    self->W_Collection = B_CollectionG_new();
    self->W_Times = B_TimesG_new();
}

struct B_SequenceG_class B_SequenceG_methods = {"B_SequenceG_class", UNASSIGNED, ($SuperG_class)&B_SliceableG_methods, B_SequenceD___init__, NULL, NULL, (B_bool (*)(B_Sequence))$default__bool__,  (B_str (*)(B_Sequence))$default__str__,(B_str (*)(B_Sequence))$default__str__,
                                            NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL};

B_Sequence B_SequenceG_new() {
    B_Sequence res = malloc(sizeof(struct B_Sequence));
    res->$class = &B_SequenceG_methods;
    res->W_Collection = B_CollectionG_new();
    res->W_Times = B_TimesG_new();
    return res;
}

void B_MappingD___init__(B_Mapping self, B_Eq W_EqD_AD_Mapping) {
    self->W_Indexed = B_IndexedG_new(W_EqD_AD_Mapping);
    self->W_EqD_AD_Mapping = W_EqD_AD_Mapping;
}

struct B_MappingG_class B_MappingG_methods = {"B_MappingG_class", UNASSIGNED, ($SuperG_class)&B_ContainerG_methods, B_MappingD___init__, NULL, NULL, (B_bool (*)(B_Mapping))$default__bool__,  (B_str (*)(B_Mapping))$default__str__, (B_str (*)(B_Mapping))$default__str__,
                                          NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL};

B_Mapping B_MappingG_new(B_Eq W_EqD_AD_Mapping) {
    B_Mapping res = malloc(sizeof(struct B_Mapping));
    res->$class = &B_MappingG_methods;
    res->W_Indexed = B_IndexedG_new(W_EqD_AD_Mapping);
    res->W_EqD_AD_Mapping = W_EqD_AD_Mapping;
    return res;
}

void B_SetD___init__(B_Set self, B_Eq W_EqD_AD_Set) {
    self->W_Ord = B_OrdG_new();
    self->W_Logical = B_LogicalG_new();
    self->W_Minus = B_MinusG_new();
    self->W_EqD_AD_Set = W_EqD_AD_Set;
}
  
struct B_SetG_class B_SetG_methods = {"B_SetG_class", UNASSIGNED, ($SuperG_class)&B_ContainerG_methods, B_SetD___init__, NULL, NULL, (B_bool (*)(B_Set))$default__bool__,  (B_str (*)(B_Set))$default__str__, (B_str (*)(B_Set))$default__str__,
                                  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL};

B_Set B_SetG_new(B_Eq W_EqD_AD_Set) {
    B_Set res = malloc(sizeof(struct B_Set));
    res->$class = &B_SetG_methods;
    res->W_Ord = B_OrdG_new();
    res->W_Logical = B_LogicalG_new();
    res->W_Minus = B_MinusG_new();
    res->W_EqD_AD_Set = W_EqD_AD_Set;
    return res;
}

void B_NumberD___init__(B_Number self) {
    self->W_Minus = B_MinusG_new();
}

struct B_NumberG_class B_NumberG_methods = {"B_NumberG_class", UNASSIGNED, ($SuperG_class)&B_TimesG_methods, B_NumberD___init__, NULL, NULL, (B_bool (*)(B_Number))$default__bool__,  (B_str (*)(B_Number))$default__str__,(B_str (*)(B_Number))$default__str__,
                                        NULL, ($WORD (*)(B_Number,$WORD,$WORD))$PlusD___iadd__, NULL, ($WORD (*)(B_Number,$WORD,$WORD))$PlusD___iadd__, NULL, NULL, NULL, B_NumberD___ipow__, NULL, NULL, NULL, NULL, NULL, NULL};


B_Number B_NumberG_new() {
    B_Number res = malloc(sizeof(struct B_Number));
    res->$class = &B_NumberG_methods;
    res->W_Minus = B_MinusG_new();
    return res;
}


struct B_RealG_class B_RealG_methods = {"B_RealG_class", UNASSIGNED, ($SuperG_class)&B_NumberG_methods, (void (*)(B_Real))$default__init__, NULL, NULL, (B_bool (*)(B_Real))$default__bool__,  (B_str (*)(B_Real))$default__str__, (B_str (*)(B_Real))$default__str__,
                                    NULL, ($WORD (*)(B_Real,$WORD,$WORD))$PlusD___iadd__, NULL, ($WORD (*)(B_Real,$WORD,$WORD))B_TimesD___imul__, NULL, NULL, NULL, ($WORD (*)(B_Real,$WORD,$WORD))B_NumberD___ipow__, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL};


B_Real B_RealG_new() {
    B_Real res = malloc(sizeof(struct B_Real));
    res->$class = &B_RealG_methods;
    return res;
}

struct B_RationalG_class B_RationalG_methods = {"B_RationalG_class", UNASSIGNED, ($SuperG_class)&B_RealG_methods, (void (*)(B_Rational))$default__init__, NULL, NULL, (B_bool (*)(B_Rational))$default__bool__,  (B_str (*)(B_Rational))$default__str__, (B_str (*)(B_Rational))$default__str__,
                                            NULL, ($WORD (*)(B_Rational,$WORD,$WORD))$PlusD___iadd__, NULL,  ($WORD (*)(B_Rational,$WORD,$WORD))B_TimesD___imul__, NULL, NULL, NULL, ($WORD (*)(B_Rational,$WORD,$WORD))B_NumberD___ipow__, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL};


B_Rational B_RationalG_new() {
    B_Rational res = malloc(sizeof(struct B_Rational));
    res->$class = &B_RationalG_methods;
    return res;
}

void B_IntegralD___init__(B_Integral self) {
    self->W_Logical = B_LogicalG_new();
    self->W_Minus = B_MinusG_new();
}
  
struct B_IntegralG_class B_IntegralG_methods = {"B_IntegralG_class", UNASSIGNED, ($SuperG_class)&B_RationalG_methods, B_IntegralD___init__, NULL, NULL, (B_bool (*)(B_Integral))$default__bool__,  (B_str (*)(B_Integral))$default__str__, (B_str (*)(B_Integral))$default__str__,
                                            NULL,  ($WORD (*)(B_Integral,$WORD,$WORD))$PlusD___iadd__, NULL,  ($WORD (*)(B_Integral,$WORD,$WORD))B_TimesD___imul__, NULL, NULL, NULL,  ($WORD (*)(B_Integral,$WORD,$WORD))B_NumberD___ipow__, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, B_IntegralD___ifloordiv__, B_IntegralD___imod__, B_IntegralD___ilshift__, B_IntegralD___irshift__, NULL};

B_Integral B_IntegralG_new() {
    B_Integral res = malloc(sizeof(struct B_Integral));
    res->$class = &B_IntegralG_methods;
    res->W_Logical = B_LogicalG_new();
    res->W_Minus = B_MinusG_new();
    return res;
}

void $register_builtin_protocols() {
    $register(&B_EqG_methods);
    $register(&B_OrdG_methods);
    $register(&B_LogicalG_methods);
    $register(&$PlusG_methods);
    $register(&B_TimesG_methods);
    $register(&B_DivG_methods);
    $register(&B_MinusG_methods);
    $register(&B_HashableG_methods);
    $register(&B_IndexedG_methods);
    $register(&B_SliceableG_methods);
    $register(&B_IterableG_methods);
    $register(&B_CollectionG_methods);
    $register(&B_ContainerG_methods);
    $register(&B_SequenceG_methods);
    $register(&B_MappingG_methods);
    $register(&B_SetG_methods);
    $register(&B_NumberG_methods);
    $register(&B_RealG_methods);
    $register(&B_RationalG_methods);
    $register(&B_HashableD_boolG_methods);
    $register(&B_SequenceD_listG_methods);
    $register(&B_CollectionD_SequenceD_listG_methods);
    $register(&B_TimesD_SequenceD_listG_methods);
    $register(&B_OrdD_listG_methods);
    $register(&B_ContainerD_listG_methods);
    $register(&B_MappingD_dictG_methods);
    $register(&B_IndexedD_MappingD_dictG_methods);
    $register(&B_OrdD_dictG_methods);
    $register(&B_SetD_setG_methods);
    $register(&B_OrdD_SetD_setG_methods);
    $register(&B_LogicalD_SetD_setG_methods);
    $register(&B_MinusD_SetD_setG_methods);
    $register(&B_IterableD_IteratorG_methods);
    $register(&B_OrdD_strG_methods);
    $register(&B_ContainerD_strG_methods);
    $register(&B_SliceableD_strG_methods);
    $register(&B_TimesD_strG_methods);
    $register(&B_HashableD_strG_methods);
    $register(&B_IntegralD_i64G_methods);
    $register(&B_LogicalD_IntegralD_i64G_methods);
    $register(&B_MinusD_IntegralD_i64G_methods);
    $register(&B_OrdD_i64G_methods);
    $register(&B_HashableD_i64G_methods);
    $register(&B_RealD_floatG_methods);
    $register(&B_DivD_floatG_methods);
    $register(&B_MinusD_RealD_floatG_methods);
    $register(&B_OrdD_floatG_methods);
    $register(&B_HashableD_floatG_methods);
    $register(&B_NumberD_complexG_methods);
    $register(&B_DivD_complexG_methods);
    $register(&B_MinusD_NumberD_complexG_methods);
    $register(&B_EqD_complexG_methods);
    $register(&B_HashableD_complexG_methods);
    $register(&B_IterableD_rangeG_methods);
    $register(&B_IterableD_tupleG_methods);
    $register(&B_SliceableD_tupleG_methods);
    $register(&B_HashableD_tupleG_methods);
    $register(&B_OrdD_bytearrayG_methods);
    $register(&B_SequenceD_bytearrayG_methods);
    $register(&B_CollectionD_SequenceD_bytearrayG_methods);
    $register(&B_TimesD_SequenceD_bytearrayG_methods);
    $register(&B_ContainerD_bytearrayG_methods);
    $register(&B_HashableD_WORDG_methods);
  
}
