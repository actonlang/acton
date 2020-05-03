void $dict_serialize($dict, $Mapping$dict, long*, $dict, struct $ROWLISTHEADER*);
$dict $dict_deserialize($Mapping$dict, $ROW*, $dict);


struct  $Mapping$dict$class $Mapping$dict$methods = {"",$Mapping$dict$__init__,$Mapping$dict$__iter__, $Mapping$dict$__fromiter__, $Mapping$dict$__len__, $Mapping$dict$__contains__,
                                                              $Mapping$dict$__containsnot__, $Mapping$dict$get, $Mapping$dict$keys, $Mapping$dict$values,
                                                              $Mapping$dict$items, $Mapping$dict$update, $Mapping$dict$popitem, $Mapping$dict$setdefault};

struct $Indexed$dict$class   $Indexed$dict$methods = {"", $Indexed$dict$__init__, $Indexed$dict$__getitem__, $Indexed$dict$__setitem__, $Indexed$dict$__delitem__};

$dict $dict_new($Mapping$dict wit) {
  return $new_dict(wit->_Hashable);
}

$Iterator $Mapping$dict$__iter__ ($Mapping$dict wit, $dict dict) {
  return $dict_iter(dict);
}

$dict $Mapping$dict$__fromiter__ ($Mapping$dict wit, $Iterable$opaque it) {
  $Iterator iter = NULL;
  if (it!=NULL)
    iter = it->proto->$class->__iter__(it->proto,it->impl);
  return $dict_fromiter(wit->_Hashable,iter);
}

$int $Mapping$dict$__len__ ($Mapping$dict wit, $dict dict) {
  return to$int($dict_len(dict));
}
  
$bool $Mapping$dict$__contains__ ($Mapping$dict wit, $dict dict, $WORD key) {
  return to$bool($dict_contains(dict,wit->_Hashable,key));
}

$bool $Mapping$dict$__containsnot__ ($Mapping$dict wit, $dict dict, $WORD key) {
  return to$bool(!$dict_contains(dict,wit->_Hashable,key));
}

$WORD $Mapping$dict$get ($Mapping$dict wit, $dict dict, $WORD key, $WORD deflt) {
  return $dict_get(dict,wit->_Hashable,key,deflt);
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
  $dict_update(dict,wit->_Hashable,other->proto->$class->__iter__(other->proto,other->impl));
}

$tup2_t $Mapping$dict$popitem ($Mapping$dict wit, $dict dict) {
  return $dict_popitem(dict, wit->_Hashable);
}

void $Mapping$dict$setdefault ($Mapping$dict wit, $dict dict, $WORD key, $WORD deflt) {
  $dict_setdefault(dict,wit->_Hashable,key,deflt);
}

$WORD $Indexed$dict$__getitem__ ($Indexed$dict wit, $dict dict, $WORD key) {
  return $dict_getitem(dict, wit->_Hashable,key);
}
void $Indexed$dict$__setitem__ ($Indexed$dict wit, $dict dict, $WORD key, $WORD value) {
  $dict_setitem(dict, wit->_Hashable,key,value);
}
void $Indexed$dict$__delitem__ ($Indexed$dict wit, $dict dict, $WORD key) {
  $dict_delitem(dict, wit->_Hashable,key);
}
 
struct $Mapping$dict *$Mapping$dict_new($Hashable h) {
  //return $NEW($Mapping$dict,h,($Eq)h);
   
  $Mapping$dict res = malloc(sizeof(struct $Mapping$dict));
  res->$class = &$Mapping$dict$methods;
  $Indexed$dict res2 = malloc(sizeof(struct $Indexed$dict));
  res->_Indexed = ($Indexed)res2;
  res->_Hashable = h;
  res2->$class = &$Indexed$dict$methods;
  res2->_Mapping = ($Mapping)res;
  res2->_Hashable = h;
  return res;
  
}

void $Mapping$dict$__init__($Mapping$dict self, $Hashable h, $Eq e) {
  self->_Indexed = ($Indexed)$NEW($Indexed$dict,($Mapping)self,h,e);
  self->_Eq = e;
  self->_Hashable = h;
}

void $Indexed$dict$__init__($Indexed$dict self, $Mapping master, $Hashable h, $Eq e) {
  self->_Mapping = master;
  self->_Hashable = h;
  self->_Eq = e;
}


