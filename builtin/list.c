
struct $Sequence$list  $Sequence$list_instance;
struct $Collection$list $Collection$list_instance;
struct $Plus$list $Plus$list_instance;

struct $Sequence$list$class $Sequence$list$methods = {"",$Sequence$list$__getitem__, $Sequence$list$__setitem__, $Sequence$list$__delitem__,
                                                                  $Sequence$list$__getslice__, $Sequence$list$__setslice__, $Sequence$list$__delslice__,
                                                               $Sequence$list$__reversed__,$Sequence$list$insert,$Sequence$list$append,$Sequence$list$reverse};
struct $Sequence$list $Sequence$list_instance = {&$Sequence$list$methods, ($Collection)&$Collection$list_instance,($Plus)&$Plus$list_instance};
$Sequence$list $Sequence$list$witness = &$Sequence$list_instance;

struct $Collection$list$class $Collection$list$methods = {"",$Collection$list$__iter__,$Collection$list$__fromiter__,$Collection$list$__len__};
struct $Collection$list $Collection$list_instance = {&$Collection$list$methods,($Sequence)&$Sequence$list_instance};
$Collection$list $Collection$list$witness = &$Collection$list_instance;


struct $Plus$list$class $Plus$list$methods = {"", $Plus$list$__add__};
struct $Plus$list $Plus$list_instance = {&$Plus$list$methods, ($Sequence)&$Sequence$list_instance};
$Plus$list $Plus$list$witness = &$Plus$list_instance;

struct $Container$list$class $Container$list$methods = {"",($Iterator (*)($Container$list, $list))$Collection$list$__iter__,
                                                                 ($list (*)($Container$list, $Iterable$opaque))$Collection$list$__fromiter__,
                                                                 ($int (*)($Container$list, $list))$Collection$list$__len__,
                                                                  $Container$list$__contains__,$Container$list$__containsnot__};
 
$Container$list $Container$list_new($Eq _EqA) {
  $Container$list res = malloc(sizeof(struct $Container$list));
  res->$class = &$Container$list$methods;
  res->_Eq = _EqA;
  return res;
}



$list $Plus$list$__add__ ($Plus$list wit, $list a, $list b) {
  return $list_add(a,b);
}

$Iterator $Collection$list$__iter__($Collection$list wit, $list self) {
  return $list_iter(self);
}

$list $Collection$list$__fromiter__($Collection$list wit, $Iterable$opaque it) {
  $Iterator iter;
  if (it == NULL)
    iter = NULL;
  else
    iter = it->proto->$class->__iter__(it->proto, it->impl);
  return $list_fromiter(iter);
}
 
$int $Collection$list$__len__($Collection$list wit, $list self) {
  return to$int($list_len(self));
}

$WORD $Sequence$list$__getitem__($Sequence$list wit, $list self, $int ix) {
  return $list_getitem(self,from$int(ix));
}

$None $Sequence$list$__setitem__($Sequence$list wit, $list self, $int ix, $WORD val) {
  $list_setitem(self,from$int(ix),val);
}

$None $Sequence$list$__delitem__($Sequence$list wit, $list self, $int ix) {
  $list_delitem(self,from$int(ix));
}

$list $Sequence$list$__getslice__($Sequence$list wit, $list self, $Slice slice) {
  return $list_getslice(self,slice);
}

$None $Sequence$list$__setslice__($Sequence$list wit, $list self, $Slice slice, $Iterable$opaque it) {
  $list_setslice(self,slice,it->proto->$class->__iter__(it->proto,it->impl));
}

$None $Sequence$list$__delslice__($Sequence$list wit, $list self, $Slice slice) {
  $list_delslice(($list)self,slice);
}  

$bool $Container$list$__contains__($Container$list wit, $list self, $WORD elem) {
  return to$bool($list_contains(wit->_Eq,self,elem));
}
                 
$bool $Container$list$__containsnot__($Container$list wit, $list self, $WORD elem) {
  return to$bool($list_containsnot(wit->_Eq,self,elem));
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

