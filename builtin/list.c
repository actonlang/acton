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


struct B_SequenceD_list B_SequenceD_list_instance;
struct B_CollectionD_SequenceD_list B_CollectionD_SequenceD_list_instance;
struct B_TimesD_SequenceD_list B_TimesD_SequenceD_list_instance;

struct B_SequenceD_listG_class B_SequenceD_listG_methods = {
    "B_SequenceD_list", 
    UNASSIGNED,
    ($SuperG_class)&B_SequenceG_methods,
    B_SequenceD_listD___init__,
    B_SequenceD_listD___serialize__,
    B_SequenceD_listD___deserialize__,
    (B_bool (*)(B_SequenceD_list))$default__bool__,
    (B_str (*)(B_SequenceD_list))$default__str__,
    (B_str (*)(B_SequenceD_list))$default__str__,
    B_SequenceD_listD___getitem__,
    B_SequenceD_listD___setitem__,
    B_SequenceD_listD___delitem__,
    B_SequenceD_listD___getslice__,
    B_SequenceD_listD___setslice__,
    B_SequenceD_listD___delslice__,
    B_SequenceD_listD___reversed__,
    B_SequenceD_list$insert,
    B_SequenceD_list$append,
    B_SequenceD_list$reverse
};
struct B_SequenceD_list B_SequenceD_list_instance = { 
    &B_SequenceD_listG_methods, 
    (B_Collection)&B_CollectionD_SequenceD_list_instance,
    (B_Times)&B_TimesD_SequenceD_list_instance
};
B_SequenceD_list B_SequenceD_listG_witness = &B_SequenceD_list_instance;

struct B_CollectionD_SequenceD_listG_class B_CollectionD_SequenceD_listG_methods = {
    "B_CollectionD_SequenceD_list",
    UNASSIGNED,
    ($SuperG_class)&B_CollectionG_methods,
    B_CollectionD_SequenceD_listD___init__,
    B_CollectionD_SequenceD_listD___serialize__,
    B_CollectionD_SequenceD_listD___deserialize__,
    (B_bool (*)(B_CollectionD_SequenceD_list))$default__bool__,
    (B_str (*)(B_CollectionD_SequenceD_list))$default__str__,
    (B_str (*)(B_CollectionD_SequenceD_list))$default__str__,
    B_CollectionD_SequenceD_listD___iter__,
    B_CollectionD_SequenceD_listD___fromiter__,
    B_CollectionD_SequenceD_listD___len__
};
struct B_CollectionD_SequenceD_list B_CollectionD_SequenceD_list_instance = {
    &B_CollectionD_SequenceD_listG_methods,
    (B_Sequence)&B_SequenceD_list_instance
};

B_CollectionD_SequenceD_list B_CollectionD_SequenceD_listG_witness = &B_CollectionD_SequenceD_list_instance;


struct B_TimesD_SequenceD_listG_class B_TimesD_SequenceD_listG_methods = {
    "B_TimesD_SequenceD_list",
    UNASSIGNED,
    ($SuperG_class)&B_TimesG_methods,
    B_TimesD_SequenceD_listD___init__,
    B_TimesD_SequenceD_listD___serialize__,
    B_TimesD_SequenceD_listD___deserialize__,
    (B_bool (*)(B_TimesD_SequenceD_list))$default__bool__,
    (B_str (*)(B_TimesD_SequenceD_list))$default__str__,
    (B_str (*)(B_TimesD_SequenceD_list))$default__str__,
    B_TimesD_SequenceD_listD___add__,
    (B_list (*)(B_TimesD_SequenceD_list, B_list, B_list))B_PlusD___iadd__,
    B_TimesD_SequenceD_listD___mul__,
    (B_list (*)(B_TimesD_SequenceD_list, B_list, B_int))B_TimesD___imul__,
};
struct B_TimesD_SequenceD_list B_TimesD_SequenceD_list_instance = {
    &B_TimesD_SequenceD_listG_methods,
    (B_Sequence)&B_SequenceD_list_instance
};
B_TimesD_SequenceD_list B_TimesD_SequenceD_listG_witness = &B_TimesD_SequenceD_list_instance;

struct B_ContainerD_listG_class B_ContainerD_listG_methods = {
    "B_ContainerD_list",
    UNASSIGNED,
    ($SuperG_class)&B_ContainerG_methods,
    B_ContainerD_listD___init__,
    B_ContainerD_listD___serialize__,
    B_ContainerD_listD___deserialize__,
    (B_bool (*)(B_ContainerD_list))$default__bool__,
    (B_str (*)(B_ContainerD_list))$default__str__,
    (B_str (*)(B_ContainerD_list))$default__str__,
    (B_Iterator (*)(B_ContainerD_list, B_list))B_CollectionD_SequenceD_listD___iter__,
    (B_list (*)(B_ContainerD_list,B_Iterable,$WORD))B_CollectionD_SequenceD_listD___fromiter__,
    (B_int (*)(B_ContainerD_list, B_list))B_CollectionD_SequenceD_listD___len__,
    B_ContainerD_listD___contains__,
    B_ContainerD_listD___containsnot__
};

struct B_OrdD_listG_class B_OrdD_listG_methods = {
    "B_OrdD_list",
    UNASSIGNED,
    ($SuperG_class)&B_OrdG_methods,
    B_OrdD_listD___init__,
    B_OrdD_listD___serialize__,
    B_OrdD_listD___deserialize__,
    (B_bool (*)(B_OrdD_list))$default__bool__,
    (B_str (*)(B_OrdD_list))$default__str__,
    (B_str (*)(B_OrdD_list))$default__str__,
    B_OrdD_listD___eq__,
    B_OrdD_listD___ne__,
    B_OrdD_listD___lt__,
    B_OrdD_listD___le__,
    B_OrdD_listD___gt__,
    B_OrdD_listD___ge__
};


void B_OrdD_listD___serialize__(B_OrdD_list self, $Serial$state state) {
    $step_serialize(self->W_OrdD_AD_OrdB_list, state);
}

B_OrdD_list B_OrdD_listD___deserialize__(B_OrdD_list self, $Serial$state state) {
    B_OrdD_list res = $DNEW(B_OrdD_list,state);
    res->W_OrdD_AD_OrdB_list = (B_Ord)$step_deserialize(state);
    return res;
}

B_bool B_OrdD_listD___eq__ (B_OrdD_list w, B_list a, B_list b) {
    if (a->length != b->length) return B_False;                                
    B_Ord w2 = w->W_OrdD_AD_OrdB_list;
    for (int i = 0; i<a->length; i++)
        if ((w2->$class->__ne__(w2,a->data[i],b->data[i]))->val) return B_False;
    return B_True;
}

B_bool B_OrdD_listD___ne__ (B_OrdD_list w, B_list a, B_list b) {
    return toB_bool(!(w->$class->__eq__(w,a,b)->val));
}

B_bool B_OrdD_listD___lt__ (B_OrdD_list w, B_list a, B_list b) {
    int minl = a->length<b->length ? a->length : b->length;
    B_Ord wA = w->W_OrdD_AD_OrdB_list;
    int i=0;
    while (i<minl && wA->$class->__eq__(wA,a->data[i],b->data[i])) i++;
    if (i==a->length)
        return toB_bool(i<b->length);
    if (i==b->length)
        return B_False;
    return  wA->$class->__lt__(wA,a->data[i],b->data[i]);
}

B_bool B_OrdD_listD___le__ (B_OrdD_list w, B_list a, B_list b) {
    int minl = a->length<b->length ? a->length : b->length;
    B_Ord wA = w->W_OrdD_AD_OrdB_list;
    int i=0;
    while (i<minl && wA->$class->__eq__(wA,a->data[i],b->data[i])) i++;
    if (i==a->length)
        return toB_bool(i<=b->length);
    if (i==b->length)
        return B_False;
    return  wA->$class->__lt__(wA,a->data[i],b->data[i]);
}

B_bool B_OrdD_listD___gt__ (B_OrdD_list w, B_list a, B_list b) {
    return  B_OrdD_listD___lt__ (w,b,a);
}

B_bool B_OrdD_listD___ge__ (B_OrdD_list w, B_list a, B_list b) {
    return  B_OrdD_listD___le__ (w,b,a);
}

B_OrdD_list B_OrdD_listG_new(B_Ord e) {
    return $NEW(B_OrdD_list,e);
}

void B_OrdD_listD___init__(B_OrdD_list self, B_Ord e) {
    self->W_OrdD_AD_OrdB_list = e;
}
  
void B_TimesD_SequenceD_listD___serialize__(B_TimesD_SequenceD_list self, $Serial$state state) {
    $step_serialize(self->W_Sequence, state);
}

B_TimesD_SequenceD_list B_TimesD_SequenceD_listD___deserialize__(B_TimesD_SequenceD_list self, $Serial$state state) {
    B_TimesD_SequenceD_list res = $DNEW(B_TimesD_SequenceD_list,state);
    res->W_Sequence = (B_Sequence)$step_deserialize(state);
    return res;
}

B_list B_TimesD_SequenceD_listD___add__ (B_TimesD_SequenceD_list wit, B_list a, B_list b) {
    return B_listD_add(a,b);
}

B_list B_TimesD_SequenceD_listD___mul__ (B_TimesD_SequenceD_list wit, B_list a, B_int n) {
    return B_listD_mul(a,n);
}

void B_CollectionD_SequenceD_listD___serialize__(B_CollectionD_SequenceD_list self, $Serial$state state) {
    $step_serialize(self->W_Sequence, state);
}

B_CollectionD_SequenceD_list B_CollectionD_SequenceD_listD___deserialize__(B_CollectionD_SequenceD_list self, $Serial$state state) {
    B_CollectionD_SequenceD_list res = $DNEW(B_CollectionD_SequenceD_list,state);
    res->W_Sequence = (B_Sequence)$step_deserialize(state);
    return res;
}

B_Iterator B_CollectionD_SequenceD_listD___iter__(B_CollectionD_SequenceD_list wit, B_list self) {
    return B_listD_iter(self);
}
 
B_list B_CollectionD_SequenceD_listD___fromiter__ (B_CollectionD_SequenceD_list wit, B_Iterable wit2, $WORD iter) {
    return B_listD_fromiter(wit2->$class->__iter__(wit2,iter));
}

B_int B_CollectionD_SequenceD_listD___len__(B_CollectionD_SequenceD_list wit, B_list self) {
    return toB_int(B_listD_len(self));
}

void B_SequenceD_listD___serialize__(B_SequenceD_list self, $Serial$state state) {
    $step_serialize(self->W_Collection, state);
    $step_serialize(self->W_Times, state);
}

B_SequenceD_list B_SequenceD_listD___deserialize__(B_SequenceD_list self, $Serial$state state) {
    B_SequenceD_list res = $DNEW(B_SequenceD_list,state);
    res->W_Collection = (B_Collection)$step_deserialize(state);
    res->W_Times = (B_Times)$step_deserialize(state);
    return res;
}
  
  
$WORD B_SequenceD_listD___getitem__(B_SequenceD_list wit, B_list self, B_int ix) {
    return B_listD_getitem(self,fromB_int(ix));
}

void B_SequenceD_listD___setitem__(B_SequenceD_list wit, B_list self, B_int ix, $WORD val) {
    B_listD_setitem(self,fromB_int(ix),val);
}

void B_SequenceD_listD___delitem__(B_SequenceD_list wit, B_list self, B_int ix) {
    B_listD_delitem(self,fromB_int(ix));
}

B_list B_SequenceD_listD___getslice__(B_SequenceD_list wit, B_list self, B_slice slice) {
    return B_listD_getslice(self,slice);
}

void B_SequenceD_listD___setslice__(B_SequenceD_list wit, B_list self, B_Iterable wit2, B_slice slice, $WORD iter) {
    B_listD_setslice(self,slice,wit2->$class->__iter__(wit2,iter));
}

void B_SequenceD_listD___delslice__(B_SequenceD_list wit, B_list self, B_slice slice) {
    B_listD_delslice((B_list)self,slice);
}  

void B_ContainerD_listD___serialize__(B_ContainerD_list self, $Serial$state state) {
    $step_serialize(self->W_EqD_AD_ContainerB_list, state);
}

B_ContainerD_list B_ContainerD_listD___deserialize__(B_ContainerD_list self, $Serial$state state) {
    B_ContainerD_list res = $DNEW(B_ContainerD_list,state);
    res->W_EqD_AD_ContainerB_list = (B_Eq)$step_deserialize(state);
    return res;
}

B_bool B_ContainerD_listD___contains__(B_ContainerD_list wit, B_list self, $WORD elem) {
    return toB_bool(B_listD_contains(wit->W_EqD_AD_ContainerB_list,self,elem));
}
                 
B_bool B_ContainerD_listD___containsnot__(B_ContainerD_list wit, B_list self, $WORD elem) {
    return toB_bool(B_listD_containsnot(wit->W_EqD_AD_ContainerB_list,self,elem));
}

B_Iterator B_IterableD_listB_reversed__iter__(B_Iterable wit, $WORD lst) {
    return B_listD_reversed(lst);
}

B_Iterator B_SequenceD_listD___reversed__(B_SequenceD_list wit, B_list self) {
    return B_listD_reversed(self);
}

void B_SequenceD_list$insert(B_SequenceD_list wit, B_list self, B_int ix, $WORD elem) {
    B_listD_insert(self,fromB_int(ix),elem);
}

void B_SequenceD_list$append(B_SequenceD_list wit, B_list self, $WORD elem) {
    B_listD_append(self,elem);
}

void B_SequenceD_list$reverse(B_SequenceD_list wit, B_list self) {
    B_listD_reverse(self);
}

B_ContainerD_list B_ContainerD_listG_new(B_Eq e) {
    return $NEW(B_ContainerD_list,e);
}


void B_ContainerD_listD___init__(B_ContainerD_list self, B_Eq W_EqD_AD_ContainerB_list) {
    self->W_EqD_AD_ContainerB_list = W_EqD_AD_ContainerB_list;
}

void B_CollectionD_SequenceD_listD___init__(B_CollectionD_SequenceD_list self, B_Sequence master) {
    self->W_Sequence = master;
}

void B_TimesD_SequenceD_listD___init__(B_TimesD_SequenceD_list self, B_Sequence master) {
    self->W_Sequence = master;
}

B_SequenceD_list B_SequenceD_listG_new() {
    return $NEW(B_SequenceD_list);
}

B_CollectionD_SequenceD_list B_CollectionD_SequenceD_listG_new(B_Sequence master){
    return $NEW(B_CollectionD_SequenceD_list, master);
}
B_TimesD_SequenceD_list B_TimesD_SequenceD_listG_new(B_Sequence master) {
    return $NEW(B_TimesD_SequenceD_list, master);
}

void B_SequenceD_listD___init__(B_SequenceD_list self) {
    self->W_Collection = (B_Collection)B_CollectionD_SequenceD_listG_new((B_Sequence)self);
    self->W_Times = (B_Times)B_TimesD_SequenceD_listG_new((B_Sequence)self);
}
