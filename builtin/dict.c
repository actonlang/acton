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


struct  $Mapping$dict$class $Mapping$dict$methods = {
    "$Mapping$dict",
    UNASSIGNED,
    ($Super$class)&$Mapping$methods,
    $Mapping$dict$__init__,
    $Mapping$dict$__serialize__,
    $Mapping$dict$__deserialize__,
    ($bool (*)($Mapping$dict))$default__bool__,
    ($str (*)($Mapping$dict))$default__str__,
    ($str (*)($Mapping$dict))$default__str__,
    $Mapping$dict$__iter__,
    $Mapping$dict$__fromiter__,
    $Mapping$dict$__len__,
    $Mapping$dict$__contains__,
    $Mapping$dict$__containsnot__,
    $Mapping$dict$get,
    $Mapping$dict$keys,
    $Mapping$dict$values,
    $Mapping$dict$items,
    $Mapping$dict$update,
    $Mapping$dict$popitem,
    $Mapping$dict$setdefault
};

struct $Indexed$dict$class $Indexed$dict$methods = {
    "$Indexed$dict",
    UNASSIGNED,
    ($Super$class)&$Indexed$methods,
    $Indexed$dict$__init__,
    $Indexed$dict$__serialize__,
    $Indexed$dict$__deserialize__,
    ($bool (*)($Indexed$dict))$default__bool__,
    ($str (*)($Indexed$dict))$default__str__,
    ($str (*)($Indexed$dict))$default__str__,
    $Indexed$dict$__getitem__,
    $Indexed$dict$__setitem__,
    $Indexed$dict$__delitem__
};

void $Mapping$dict$__serialize__($Mapping$dict self, $Serial$state state) {
  $step_serialize(self->w$Indexed, state);
  $step_serialize(self->w$Eq$A$Mapping$dict, state);
  $step_serialize(self->w$Hashable$A$Mapping$dict, state);
}

$Mapping$dict $Mapping$dict$__deserialize__($Mapping$dict self, $Serial$state state) {
   $Mapping$dict res = $DNEW($Mapping$dict,state);
   res->w$Indexed = ($Indexed)$step_deserialize(state);
   res->w$Eq$A$Mapping$dict = ($Eq)$step_deserialize(state);
   res->w$Hashable$A$Mapping$dict = ($Hashable)$step_deserialize(state);
   return res;
}

$Iterator $Mapping$dict$__iter__ ($Mapping$dict wit, $dict dict) {
  return $dict_iter(dict);
}

$dict $Mapping$dict$__fromiter__ ($Mapping$dict wit, $Iterable wit2, $WORD iter) {
  return $dict_fromiter(wit->w$Hashable$A$Mapping$dict,wit2->$class->__iter__(wit2,iter));
}

$int $Mapping$dict$__len__ ($Mapping$dict wit, $dict dict) {
  return to$int($dict_len(dict));
}
  
$bool $Mapping$dict$__contains__ ($Mapping$dict wit, $dict dict, $WORD key) {
  return to$bool($dict_contains(dict,wit->w$Hashable$A$Mapping$dict,key));
}

$bool $Mapping$dict$__containsnot__ ($Mapping$dict wit, $dict dict, $WORD key) {
  return to$bool(!$dict_contains(dict,wit->w$Hashable$A$Mapping$dict,key));
}

$WORD $Mapping$dict$get ($Mapping$dict wit, $dict dict, $WORD key, $WORD deflt) {
  return $dict_get(dict,wit->w$Hashable$A$Mapping$dict,key,deflt);
}

$Iterator $Mapping$dict$keys ($Mapping$dict wit, $dict dict) {
  return $dict_keys(dict);
}

$Iterator $Mapping$dict$values ($Mapping$dict wit, $dict dict) {
  return $dict_values(dict);
}

$Iterator $Mapping$dict$items ($Mapping$dict wit, $dict dict) {
  return $dict_items(dict);
}

void $Mapping$dict$update ($Mapping$dict wit, $dict dict, $Iterable wit2, $WORD other) {
  $dict_update(dict,wit->w$Hashable$A$Mapping$dict,wit2->$class->__iter__(wit2,other));
}

$tuple $Mapping$dict$popitem ($Mapping$dict wit, $dict dict) {
  return $dict_popitem(dict, wit->w$Hashable$A$Mapping$dict);
}

void $Mapping$dict$setdefault ($Mapping$dict wit, $dict dict, $WORD key, $WORD deflt) {
  $dict_setdefault(dict,wit->w$Hashable$A$Mapping$dict,key,deflt);
}

void $Indexed$dict$__serialize__($Indexed$dict self, $Serial$state state) {
  $step_serialize(self->w$Mapping, state);
  $step_serialize(self->w$Eq$A$Mapping$dict, state);
  $step_serialize(self->w$Hashable$A$Mapping$dict, state);
}

$Indexed$dict $Indexed$dict$__deserialize__($Indexed$dict self, $Serial$state state) {
   $Indexed$dict res = $DNEW($Indexed$dict,state);
   res->w$Mapping = ($Mapping)$step_deserialize(state);
   res->w$Eq$A$Mapping$dict = ($Eq)$step_deserialize(state);
   res->w$Hashable$A$Mapping$dict = ($Hashable)$step_deserialize(state);
   return res;
}

$WORD $Indexed$dict$__getitem__ ($Indexed$dict wit, $dict dict, $WORD key) {
  return $dict_getitem(dict, (($Mapping$dict)wit->w$Mapping)->w$Hashable$A$Mapping$dict,key);
}
void $Indexed$dict$__setitem__ ($Indexed$dict wit, $dict dict, $WORD key, $WORD value) {
  $dict_setitem(dict,  (($Mapping$dict)wit->w$Mapping)->w$Hashable$A$Mapping$dict,key,value);
}
void $Indexed$dict$__delitem__ ($Indexed$dict wit, $dict dict, $WORD key) {
  $dict_delitem(dict,  (($Mapping$dict)wit->w$Mapping)->w$Hashable$A$Mapping$dict,key);
}

$Mapping$dict $Mapping$dict$new($Hashable h) {
  return $NEW($Mapping$dict, h);
}

void $Mapping$dict$__init__($Mapping$dict self, $Hashable h) {
  self->w$Indexed = ($Indexed)$NEW($Indexed$dict,($Mapping)self,($Eq)h);
  self->w$Eq$A$Mapping$dict = ($Eq)h;
  self->w$Hashable$A$Mapping$dict = h;
}

$Indexed$dict $Indexed$dict$new($Mapping master, $Eq e) {
  return $NEW($Indexed$dict, master, e);
}


void $Indexed$dict$__init__($Indexed$dict self, $Mapping master, $Eq e) {
  self->w$Mapping = master;
  self->w$Eq$A$Mapping$dict = e;
  self->w$Hashable$A$Mapping$dict = ($Hashable)e;
}


