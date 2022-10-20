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


struct $Sequence$list $Sequence$list_instance;
struct $Collection$list $Collection$list_instance;
struct $Times$list $Times$list_instance;

struct $Sequence$list$class $Sequence$list$methods = {
    "$Sequence$list", 
    UNASSIGNED,
    ($Super$class)&$Sequence$methods,
    $Sequence$list$__init__,
    $Sequence$list$__serialize__,
    $Sequence$list$__deserialize__,
    ($bool (*)($Sequence$list))$default__bool__,
    ($str (*)($Sequence$list))$default__str__,
    ($str (*)($Sequence$list))$default__str__,
    $Sequence$list$__getitem__,
    $Sequence$list$__setitem__,
    $Sequence$list$__delitem__,
    $Sequence$list$__getslice__,
    $Sequence$list$__setslice__,
    $Sequence$list$__delslice__,
    $Sequence$list$__reversed__,
    $Sequence$list$insert,
    $Sequence$list$append,
    $Sequence$list$reverse
};
struct $Sequence$list $Sequence$list_instance = { 
    &$Sequence$list$methods, 
    ($Collection)&$Collection$list_instance,
    ($Times)&$Times$list_instance
};
$Sequence$list $Sequence$list$witness = &$Sequence$list_instance;

struct $Collection$list$class $Collection$list$methods = {
    "$Collection$list",
    UNASSIGNED,
    ($Super$class)&$Collection$methods,
    $Collection$list$__init__,
    $Collection$list$__serialize__,
    $Collection$list$__deserialize__,
    ($bool (*)($Collection$list))$default__bool__,
    ($str (*)($Collection$list))$default__str__,
    ($str (*)($Collection$list))$default__str__,
    $Collection$list$__iter__,
    $Collection$list$__fromiter__,
    $Collection$list$__len__
};
struct $Collection$list $Collection$list_instance = {
    &$Collection$list$methods,
    ($Sequence)&$Sequence$list_instance
};

$Collection$list $Collection$list$witness = &$Collection$list_instance;


struct $Times$list$class $Times$list$methods = {
    "$Times$list",
    UNASSIGNED,
    ($Super$class)&$Times$methods,
    $Times$list$__init__,
    $Times$list$__serialize__,
    $Times$list$__deserialize__,
    ($bool (*)($Times$list))$default__bool__,
    ($str (*)($Times$list))$default__str__,
    ($str (*)($Times$list))$default__str__,
    $Times$list$__add__,
    ($list (*)($Times$list, $list, $list))$Plus$__iadd__,
    $Times$list$__mul__,
    ($list (*)($Times$list, $list, $int))$Times$__imul__,
};
struct $Times$list $Times$list_instance = {
    &$Times$list$methods,
    ($Sequence)&$Sequence$list_instance
};
$Times$list $Times$list$witness = &$Times$list_instance;

struct $Container$list$class $Container$list$methods = {
    "$Container$list",
    UNASSIGNED,
    ($Super$class)&$Container$methods,
    $Container$list$__init__,
    $Container$list$__serialize__,
    $Container$list$__deserialize__,
    ($bool (*)($Container$list))$default__bool__,
    ($str (*)($Container$list))$default__str__,
    ($str (*)($Container$list))$default__str__,
    ($Iterator (*)($Container$list, $list))$Collection$list$__iter__,
    ($list (*)($Container$list,$Iterable,$WORD))$Collection$list$__fromiter__,
    ($int (*)($Container$list, $list))$Collection$list$__len__,
    $Container$list$__contains__,
    $Container$list$__containsnot__
};

struct $Ord$list$class $Ord$list$methods = {
    "$Ord$list",
    UNASSIGNED,
    ($Super$class)&$Ord$methods,
    $Ord$list$__init__,
    $Ord$list$__serialize__,
    $Ord$list$__deserialize__,
    ($bool (*)($Ord$list))$default__bool__,
    ($str (*)($Ord$list))$default__str__,
    ($str (*)($Ord$list))$default__str__,
    $Ord$list$__eq__,
    $Ord$list$__ne__,
    $Ord$list$__lt__,
    $Ord$list$__le__,
    $Ord$list$__gt__,
    $Ord$list$__ge__
};


void $Ord$list$__serialize__($Ord$list self, $Serial$state state) {
    $step_serialize(self->w$Ord$A$Ord$list, state);
}

$Ord$list $Ord$list$__deserialize__($Ord$list self, $Serial$state state) {
    $Ord$list res = $DNEW($Ord$list,state);
    res->w$Ord$A$Ord$list = ($Ord)$step_deserialize(state);
    return res;
}

$bool $Ord$list$__eq__ ($Ord$list w, $list a, $list b) {
    if (a->length != b->length) return $False;                                
    $Ord w2 = w->w$Ord$A$Ord$list;
    for (int i = 0; i<a->length; i++)
        if ((w2->$class->__ne__(w2,a->data[i],b->data[i]))->val) return $False;
    return $True;
}

$bool $Ord$list$__ne__ ($Ord$list w, $list a, $list b) {
    return to$bool(!(w->$class->__eq__(w,a,b)->val));
}

$bool $Ord$list$__lt__ ($Ord$list w, $list a, $list b) {
    int minl = a->length<b->length ? a->length : b->length;
    $Ord wA = w->w$Ord$A$Ord$list;
    int i=0;
    while (i<minl && wA->$class->__eq__(wA,a->data[i],b->data[i])) i++;
    if (i==a->length)
        return to$bool(i<b->length);
    if (i==b->length)
        return $False;
    return  wA->$class->__lt__(wA,a->data[i],b->data[i]);
}

$bool $Ord$list$__le__ ($Ord$list w, $list a, $list b) {
    int minl = a->length<b->length ? a->length : b->length;
    $Ord wA = w->w$Ord$A$Ord$list;
    int i=0;
    while (i<minl && wA->$class->__eq__(wA,a->data[i],b->data[i])) i++;
    if (i==a->length)
        return to$bool(i<=b->length);
    if (i==b->length)
        return $False;
    return  wA->$class->__lt__(wA,a->data[i],b->data[i]);
}

$bool $Ord$list$__gt__ ($Ord$list w, $list a, $list b) {
    return  $Ord$list$__lt__ (w,b,a);
}

$bool $Ord$list$__ge__ ($Ord$list w, $list a, $list b) {
    return  $Ord$list$__le__ (w,b,a);
}

$Ord$list $Ord$list$new($Ord e) {
    return $NEW($Ord$list,e);
}

void $Ord$list$__init__($Ord$list self, $Ord e) {
    self->w$Ord$A$Ord$list = e;
}
  
void $Times$list$__serialize__($Times$list self, $Serial$state state) {
    $step_serialize(self->w$Sequence, state);
}

$Times$list $Times$list$__deserialize__($Times$list self, $Serial$state state) {
    $Times$list res = $DNEW($Times$list,state);
    res->w$Sequence = ($Sequence)$step_deserialize(state);
    return res;
}

$list $Times$list$__add__ ($Times$list wit, $list a, $list b) {
    return $list_add(a,b);
}

$list $Times$list$__mul__ ($Times$list wit, $list a, $int n) {
    return $list_mul(a,n);
}

void $Collection$list$__serialize__($Collection$list self, $Serial$state state) {
    $step_serialize(self->w$Sequence, state);
}

$Collection$list $Collection$list$__deserialize__($Collection$list self, $Serial$state state) {
    $Collection$list res = $DNEW($Collection$list,state);
    res->w$Sequence = ($Sequence)$step_deserialize(state);
    return res;
}

$Iterator $Collection$list$__iter__($Collection$list wit, $list self) {
    return $list_iter(self);
}
 
$list $Collection$list$__fromiter__ ($Collection$list wit, $Iterable wit2, $WORD iter) {
    return $list_fromiter(wit2->$class->__iter__(wit2,iter));
}

$int $Collection$list$__len__($Collection$list wit, $list self) {
    return to$int($list_len(self));
}

void $Sequence$list$__serialize__($Sequence$list self, $Serial$state state) {
    $step_serialize(self->w$Collection, state);
    $step_serialize(self->w$Times, state);
}

$Sequence$list $Sequence$list$__deserialize__($Sequence$list self, $Serial$state state) {
    $Sequence$list res = $DNEW($Sequence$list,state);
    res->w$Collection = ($Collection)$step_deserialize(state);
    res->w$Times = ($Times)$step_deserialize(state);
    return res;
}
  
  
$WORD $Sequence$list$__getitem__($Sequence$list wit, $list self, $int ix) {
    return $list_getitem(self,from$int(ix));
}

void $Sequence$list$__setitem__($Sequence$list wit, $list self, $int ix, $WORD val) {
    $list_setitem(self,from$int(ix),val);
}

void $Sequence$list$__delitem__($Sequence$list wit, $list self, $int ix) {
    $list_delitem(self,from$int(ix));
}

$list $Sequence$list$__getslice__($Sequence$list wit, $list self, $slice slice) {
    return $list_getslice(self,slice);
}

void $Sequence$list$__setslice__($Sequence$list wit, $list self, $Iterable wit2, $slice slice, $WORD iter) {
    $list_setslice(self,slice,wit2->$class->__iter__(wit2,iter));
}

void $Sequence$list$__delslice__($Sequence$list wit, $list self, $slice slice) {
    $list_delslice(($list)self,slice);
}  

void $Container$list$__serialize__($Container$list self, $Serial$state state) {
    $step_serialize(self->w$Eq$A$Container$list, state);
}

$Container$list $Container$list$__deserialize__($Container$list self, $Serial$state state) {
    $Container$list res = $DNEW($Container$list,state);
    res->w$Eq$A$Container$list = ($Eq)$step_deserialize(state);
    return res;
}

$bool $Container$list$__contains__($Container$list wit, $list self, $WORD elem) {
    return to$bool($list_contains(wit->w$Eq$A$Container$list,self,elem));
}
                 
$bool $Container$list$__containsnot__($Container$list wit, $list self, $WORD elem) {
    return to$bool($list_containsnot(wit->w$Eq$A$Container$list,self,elem));
}

$Iterator $Iterable$list$reversed__iter__($Iterable wit, $WORD lst) {
    return $list_reversed(lst);
}

$Iterator $Sequence$list$__reversed__($Sequence$list wit, $list self) {
    return $list_reversed(self);
}

void $Sequence$list$insert($Sequence$list wit, $list self, $int ix, $WORD elem) {
    $list_insert(self,from$int(ix),elem);
}

void $Sequence$list$append($Sequence$list wit, $list self, $WORD elem) {
    $list_append(self,elem);
}

void $Sequence$list$reverse($Sequence$list wit, $list self) {
    $list_reverse(self);
}

$Container$list $Container$list$new($Eq e) {
    return $NEW($Container$list,e);
}


void $Container$list$__init__($Container$list self, $Eq w$Eq$A$Container$list) {
    self->w$Eq$A$Container$list = w$Eq$A$Container$list;
}

void $Collection$list$__init__($Collection$list self, $Sequence master) {
    self->w$Sequence = master;
}

void $Times$list$__init__($Times$list self, $Sequence master) {
    self->w$Sequence = master;
}

$Sequence$list $Sequence$list$new() {
    return $NEW($Sequence$list);
}

$Collection$list $Collection$list$new($Sequence master){
    return $NEW($Collection$list, master);
}
$Times$list $Times$list$new($Sequence master) {
    return $NEW($Times$list, master);
}

void $Sequence$list$__init__($Sequence$list self) {
    self->w$Collection = ($Collection)$Collection$list$new(($Sequence)self);
    self->w$Times = ($Times)$Times$list$new(($Sequence)self);
}
