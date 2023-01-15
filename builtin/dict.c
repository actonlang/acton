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


struct  B_MappingD_dictG_class B_MappingD_dictG_methods = {
    "B_MappingD_dict",
    UNASSIGNED,
    ($SuperG_class)&B_MappingG_methods,
    B_MappingD_dictD___init__,
    B_MappingD_dictD___serialize__,
    B_MappingD_dictD___deserialize__,
    (B_bool (*)(B_MappingD_dict))$default__bool__,
    (B_str (*)(B_MappingD_dict))$default__str__,
    (B_str (*)(B_MappingD_dict))$default__str__,
    B_MappingD_dictD___iter__,
    B_MappingD_dictD___fromiter__,
    B_MappingD_dictD___len__,
    B_MappingD_dictD___contains__,
    B_MappingD_dictD___containsnot__,
    B_MappingD_dictD_get,
    B_MappingD_dictD_keys,
    B_MappingD_dictD_values,
    B_MappingD_dictD_items,
    B_MappingD_dictD_update,
    B_MappingD_dictD_popitem,
    B_MappingD_dictD_setdefault
};

struct B_IndexedD_MappingD_dictG_class B_IndexedD_MappingD_dictG_methods = {
    "B_IndexedD_MappingD_dict",
    UNASSIGNED,
    ($SuperG_class)&B_IndexedG_methods,
    B_IndexedD_MappingD_dictD___init__,
    B_IndexedD_MappingD_dictD___serialize__,
    B_IndexedD_MappingD_dictD___deserialize__,
    (B_bool (*)(B_IndexedD_MappingD_dict))$default__bool__,
    (B_str (*)(B_IndexedD_MappingD_dict))$default__str__,
    (B_str (*)(B_IndexedD_MappingD_dict))$default__str__,
    B_IndexedD_MappingD_dictD___getitem__,
    B_IndexedD_MappingD_dictD___setitem__,
    B_IndexedD_MappingD_dictD___delitem__
};


struct B_OrdD_dictG_class B_OrdD_dictG_methods = {
    "B_OrdD_dict",
    UNASSIGNED,
    ($SuperG_class)&B_OrdG_methods,
    B_OrdD_dictD___init__,
    B_OrdD_dictD___serialize__,
    B_OrdD_dictD___deserialize__,
    (B_bool (*)(B_OrdD_dict))$default__bool__,
    (B_str (*)(B_OrdD_dict))$default__str__,
    (B_str (*)(B_OrdD_dict))$default__str__,
    B_OrdD_dictD___eq__,
    B_OrdD_dictD___ne__,
    B_OrdD_dictD___lt__,
    B_OrdD_dictD___le__,
    B_OrdD_dictD___gt__,
    B_OrdD_dictD___ge__
};


void B_OrdD_dictD___serialize__(B_OrdD_dict self, $Serial$state state) {
    $step_serialize(self->W_HashableD_AD_OrdD_dict, state);
    $step_serialize(self->W_EqD_BD_OrdD_dict, state);
}

B_OrdD_dict B_OrdD_dictD___deserialize__(B_OrdD_dict self, $Serial$state state) {
    B_OrdD_dict res = $DNEW(B_OrdD_dict,state);
    res->W_HashableD_AD_OrdD_dict = (B_Hashable)$step_deserialize(state);
    res->W_EqD_BD_OrdD_dict = (B_Eq)$step_deserialize(state);
    res->W_EqD_AD_OrdD_dict = (B_Eq)res->W_HashableD_AD_OrdD_dict;
    return res;
}

void B_OrdD_dictD___init__(B_OrdD_dict self, B_Hashable hA, B_Eq eB) {
    self->W_HashableD_AD_OrdD_dict = hA;
    self->W_EqD_AD_OrdD_dict = (B_Eq)hA;
    self->W_EqD_BD_OrdD_dict = eB;
}

B_OrdD_dict B_OrdD_dictG_new(B_Hashable hA, B_Eq eB) {
    return $NEW(B_OrdD_dict,hA,eB);
}

B_bool B_dictrel(bool directfalse,B_OrdD_dict w, B_dict a, B_dict b) {
    if (directfalse) {
        return $False;
    }; 
    B_Hashable wH = w->W_HashableD_AD_OrdD_dict;
    B_Eq wB = w->W_EqD_BD_OrdD_dict;
    B_MappingD_dict m = B_MappingD_dictG_new(wH);
    B_Iterator it = m->$class->keys(m,a);
    $WORD x,resa,resb;
    while ((x = $next(it))) {
        long h = fromB_int(wH->$class->__hash__(wH,x));
        int ixa = $lookdict(a, wH, h, x, &resa);
        int ixb = $lookdict(b, wH, h, x ,&resb);
        if (ixb<0 || wB->$class->__ne__(wB,resa,resb)->val) return $False;
    }
    return $True;
}

B_bool B_OrdD_dictD___eq__ (B_OrdD_dict w, B_dict a, B_dict b) {
    return B_dictrel(a->numelements != b->numelements,w,a,b);
}

B_bool B_OrdD_dictD___ne__ (B_OrdD_dict w, B_dict a, B_dict b) {
    return toB_bool(!(w->$class->__eq__(w,a,b)->val));
}

B_bool B_OrdD_dictD___lt__ (B_OrdD_dict w, B_dict a, B_dict b) {
    return B_dictrel(a->numelements >= b->numelements,w,a,b);
}

B_bool B_OrdD_dictD___le__ (B_OrdD_dict w, B_dict a, B_dict b) {
    return B_dictrel(a->numelements > b->numelements,w,a,b);
}

B_bool B_OrdD_dictD___gt__ (B_OrdD_dict w, B_dict a, B_dict b) {
    return toB_bool(!(w->$class->__lt__(w,b,a)->val));
}

B_bool B_OrdD_dictD___ge__ (B_OrdD_dict w, B_dict a, B_dict b) {
    return toB_bool(!(w->$class->__le__(w,b,a)->val));
}

void B_MappingD_dictD___serialize__(B_MappingD_dict self, $Serial$state state) {
    $step_serialize(self->W_Indexed, state);
    $step_serialize(self->W_EqD_AD_MappingB_dict, state);
    $step_serialize(self->W_HashableD_AD_MappingB_dict, state);
}

B_MappingD_dict B_MappingD_dictD___deserialize__(B_MappingD_dict self, $Serial$state state) {
    B_MappingD_dict res = $DNEW(B_MappingD_dict,state);
    res->W_Indexed = (B_Indexed)$step_deserialize(state);
    res->W_EqD_AD_MappingB_dict = (B_Eq)$step_deserialize(state);
    res->W_HashableD_AD_MappingB_dict = (B_Hashable)$step_deserialize(state);
    return res;
}

B_Iterator B_MappingD_dictD___iter__ (B_MappingD_dict wit, B_dict dict) {
    return B_dictD_iter(dict);
}

B_dict B_MappingD_dictD___fromiter__ (B_MappingD_dict wit, B_Iterable wit2, $WORD iter) {
    return B_dictD_fromiter(wit->W_HashableD_AD_MappingB_dict,wit2->$class->__iter__(wit2,iter));
}

B_int B_MappingD_dictD___len__ (B_MappingD_dict wit, B_dict dict) {
    return toB_int(B_dictD_len(dict));
}
  
B_bool B_MappingD_dictD___contains__ (B_MappingD_dict wit, B_dict dict, $WORD key) {
    return toB_bool(B_dictD_contains(dict,wit->W_HashableD_AD_MappingB_dict,key));
}

B_bool B_MappingD_dictD___containsnot__ (B_MappingD_dict wit, B_dict dict, $WORD key) {
    return toB_bool(!B_dictD_contains(dict,wit->W_HashableD_AD_MappingB_dict,key));
}

$WORD B_MappingD_dictD_get (B_MappingD_dict wit, B_dict dict, $WORD key, $WORD deflt) {
    return B_dictD_get(dict,wit->W_HashableD_AD_MappingB_dict,key,deflt);
}

B_Iterator B_MappingD_dictD_keys (B_MappingD_dict wit, B_dict dict) {
    return B_dictD_keys(dict);
}

B_Iterator B_MappingD_dictD_values (B_MappingD_dict wit, B_dict dict) {
    return B_dictD_values(dict);
}

B_Iterator B_MappingD_dictD_items (B_MappingD_dict wit, B_dict dict) {
    return B_dictD_items(dict);
}

void B_MappingD_dictD_update (B_MappingD_dict wit, B_dict dict, B_Iterable wit2, $WORD other) {
    B_dictD_update(dict,wit->W_HashableD_AD_MappingB_dict,wit2->$class->__iter__(wit2,other));
}

B_tuple B_MappingD_dictD_popitem (B_MappingD_dict wit, B_dict dict) {
    return B_dictD_popitem(dict, wit->W_HashableD_AD_MappingB_dict);
}

void B_MappingD_dictD_setdefault (B_MappingD_dict wit, B_dict dict, $WORD key, $WORD deflt) {
    B_dictD_setdefault(dict,wit->W_HashableD_AD_MappingB_dict,key,deflt);
}

void B_IndexedD_MappingD_dictD___serialize__(B_IndexedD_MappingD_dict self, $Serial$state state) {
    $step_serialize(self->W_Mapping, state);
    $step_serialize(self->W_EqD_AD_MappingB_dict, state);
    $step_serialize(self->W_HashableD_AD_MappingB_dict, state);
}

B_IndexedD_MappingD_dict B_IndexedD_MappingD_dictD___deserialize__(B_IndexedD_MappingD_dict self, $Serial$state state) {
    B_IndexedD_MappingD_dict res = $DNEW(B_IndexedD_MappingD_dict,state);
    res->W_Mapping = (B_Mapping)$step_deserialize(state);
    res->W_EqD_AD_MappingB_dict = (B_Eq)$step_deserialize(state);
    res->W_HashableD_AD_MappingB_dict = (B_Hashable)$step_deserialize(state);
    return res;
}

$WORD B_IndexedD_MappingD_dictD___getitem__ (B_IndexedD_MappingD_dict wit, B_dict dict, $WORD key) {
    return B_dictD_getitem(dict, ((B_MappingD_dict)wit->W_Mapping)->W_HashableD_AD_MappingB_dict,key);
}
void B_IndexedD_MappingD_dictD___setitem__ (B_IndexedD_MappingD_dict wit, B_dict dict, $WORD key, $WORD value) {
    B_dictD_setitem(dict,  ((B_MappingD_dict)wit->W_Mapping)->W_HashableD_AD_MappingB_dict,key,value);
}
void B_IndexedD_MappingD_dictD___delitem__ (B_IndexedD_MappingD_dict wit, B_dict dict, $WORD key) {
    B_dictD_delitem(dict,  ((B_MappingD_dict)wit->W_Mapping)->W_HashableD_AD_MappingB_dict,key);
}

B_MappingD_dict B_MappingD_dictG_new(B_Hashable h) {
    return $NEW(B_MappingD_dict, h);
}

void B_MappingD_dictD___init__(B_MappingD_dict self, B_Hashable h) {
    self->W_Indexed = (B_Indexed)$NEW(B_IndexedD_MappingD_dict,(B_Mapping)self,(B_Eq)h);
    self->W_EqD_AD_MappingB_dict = (B_Eq)h;
    self->W_HashableD_AD_MappingB_dict = h;
}

B_IndexedD_MappingD_dict B_IndexedD_MappingD_dictG_new(B_Mapping master, B_Eq e) {
    return $NEW(B_IndexedD_MappingD_dict, master, e);
}


void B_IndexedD_MappingD_dictD___init__(B_IndexedD_MappingD_dict self, B_Mapping master, B_Eq e) {
    self->W_Mapping = master;
    self->W_EqD_AD_MappingB_dict = e;
    self->W_HashableD_AD_MappingB_dict = (B_Hashable)e;
}

