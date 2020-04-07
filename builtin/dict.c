
static struct  Mapping$dict$__class__ Mapping$dict_methods = {"",Mapping$dict$__iter__, Mapping$dict$__fromiter__, Mapping$dict$__len__, Mapping$dict$__contains__,
                                                              Mapping$dict$__containsnot__, Mapping$dict$get, Mapping$dict$keys, Mapping$dict$values,
                                                              Mapping$dict$items, Mapping$dict$update, Mapping$dict$popitem, Mapping$dict$setdefault};

static struct Indexed$dict$__class__   Indexed$dict_methods = {"", Indexed$dict$__getitem__, Indexed$dict$__setitem__, Indexed$dict$__delitem__};

$dict $dict_new(Mapping$dict wit) {
  return $new_dict(wit->_Hashable);
}

Iterator Mapping$dict$__iter__ (Mapping$dict wit, $dict dict) {
  return $dict_iter(dict);
}

$dict Mapping$dict$__fromiter__ (Mapping$dict wit, Iterable$opaque it) {
  Iterator iter = NULL;
  if (it!=NULL)
    iter = it->__proto__->__class__->__iter__(it->__proto__,it->__impl__);
  return $dict_fromiter(wit->_Hashable,iter);
}

$int Mapping$dict$__len__ (Mapping$dict wit, $dict dict) {
  return to$int($dict_len(dict));
}
  
$bool Mapping$dict$__contains__ (Mapping$dict wit, $dict dict, $WORD key) {
  return to$bool($dict_contains(dict,key));
}

$bool Mapping$dict$__containsnot__ (Mapping$dict wit, $dict dict, $WORD key) {
  return to$bool(!$dict_contains(dict,key));
}

$WORD Mapping$dict$get (Mapping$dict wit, $dict dict, $WORD key, $WORD deflt) {
  return $dict_get(dict,key,deflt);
}

Iterator Mapping$dict$keys (Mapping$dict wit, $dict dict) {
  return $dict_keys(dict);
}

Iterator Mapping$dict$values (Mapping$dict wit, $dict dict) {
  return $dict_values(dict);
}

Iterator Mapping$dict$items (Mapping$dict wit, $dict dict) {
  return $dict_items(dict);
}

None Mapping$dict$update (Mapping$dict wit, $dict dict, Iterable$opaque other) {
  $dict_update(dict,other->__proto__->__class__->__iter__(other->__proto__,other->__impl__));
}

$tup2_t Mapping$dict$popitem (Mapping$dict wit, $dict dict) {
  return $dict_popitem(dict);
}

None Mapping$dict$setdefault (Mapping$dict wit, $dict dict, $WORD key, $WORD deflt) {
  $dict_setdefault(dict,key,deflt);
}

$WORD Indexed$dict$__getitem__ (Indexed$dict wit, $dict dict, $WORD key) {
  return $dict_getitem(dict,key);
}
None Indexed$dict$__setitem__ (Indexed$dict wit, $dict dict, $WORD key, $WORD value) {
  $dict_setitem(dict,key,value);
}
None Indexed$dict$__delitem__ (Indexed$dict wit, $dict dict, $WORD key) {
  $dict_delitem(dict,key);
}
 
Mapping$dict Mapping$dict_new(Hashable h) {
  Mapping$dict res = malloc(sizeof(struct Mapping$dict));
  res->__class__ = &Mapping$dict_methods;
  Indexed$dict res2 = malloc(sizeof(struct Indexed$dict));
  res->_Indexed = (Indexed)res2;
  res->_Hashable = h;
  res2->__class__ = &Indexed$dict_methods;
  res2->_Mapping = (Mapping)res;
  res2->_Hashable = h;
  return res;
}
