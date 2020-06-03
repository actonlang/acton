
struct  $Mapping$dict$class $Mapping$dict$methods = {"",UNASSIGNED,NULL,$Mapping$dict$__init__,$Mapping$dict$__iter__,
                                                     $Mapping$dict$__fromiter__, $Mapping$dict$__len__, $Mapping$dict$__contains__,
                                                     $Mapping$dict$__containsnot__, $Mapping$dict$get, $Mapping$dict$keys, $Mapping$dict$values,
                                                     $Mapping$dict$items, $Mapping$dict$update, $Mapping$dict$popitem, $Mapping$dict$setdefault};

struct $Indexed$dict$class   $Indexed$dict$methods = {"", UNASSIGNED,NULL,$Indexed$dict$__init__, $Indexed$dict$__getitem__, $Indexed$dict$__setitem__, $Indexed$dict$__delitem__};

$Iterator $Mapping$dict$__iter__ ($Mapping$dict wit, $dict dict) {
  return $dict_iter(dict);
}

$dict $Mapping$dict$__fromiter__ ($Mapping$dict wit, $Iterable$opaque iter) {
  return $dict_fromiter(wit->w$Hashable$Mapping,iter);
}

$int $Mapping$dict$__len__ ($Mapping$dict wit, $dict dict) {
  return to$int($dict_len(dict));
}
  
$bool $Mapping$dict$__contains__ ($Mapping$dict wit, $dict dict, $WORD key) {
  return to$bool($dict_contains(dict,wit->w$Hashable$Mapping,key));
}

$bool $Mapping$dict$__containsnot__ ($Mapping$dict wit, $dict dict, $WORD key) {
  return to$bool(!$dict_contains(dict,wit->w$Hashable$Mapping,key));
}

$WORD $Mapping$dict$get ($Mapping$dict wit, $dict dict, $WORD key, $WORD deflt) {
  return $dict_get(dict,wit->w$Hashable$Mapping,key,deflt);
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

void $Mapping$dict$update ($Mapping$dict wit, $dict dict, $Iterable$opaque other) {
  $dict_update(dict,wit->w$Hashable$Mapping,other);
}

$tuple $Mapping$dict$popitem ($Mapping$dict wit, $dict dict) {
  return $dict_popitem(dict, wit->w$Hashable$Mapping);
}

void $Mapping$dict$setdefault ($Mapping$dict wit, $dict dict, $WORD key, $WORD deflt) {
  $dict_setdefault(dict,wit->w$Hashable$Mapping,key,deflt);
}

$WORD $Indexed$dict$__getitem__ ($Indexed$dict wit, $dict dict, $WORD key) {
  return $dict_getitem(dict, wit->w$Mapping$dict->w$Hashable$Mapping,key);
}
void $Indexed$dict$__setitem__ ($Indexed$dict wit, $dict dict, $WORD key, $WORD value) {
  $dict_setitem(dict, wit->w$Mapping$dict->w$Hashable$Mapping,key,value);
}
void $Indexed$dict$__delitem__ ($Indexed$dict wit, $dict dict, $WORD key) {
  $dict_delitem(dict, wit->w$Mapping$dict->w$Hashable$Mapping,key);
}

void $Mapping$dict$__init__($Mapping$dict self, $Hashable h) {
  self->w$Indexed$Mapping = $NEW($Indexed$dict,self,($Eq)h);
  self->w$Hashable$Mapping = h;
}

void $Indexed$dict$__init__($Indexed$dict self, $Mapping$dict master, $Eq e) {
  self->w$Mapping$dict = master;
  self->w$Eq$A = e;
}


