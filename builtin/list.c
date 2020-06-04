
struct $Sequence$list  $Sequence$list_instance;
struct $Collection$list $Collection$list_instance;
struct $Plus$list $Plus$list_instance;

struct $Sequence$list$class $Sequence$list$methods = {"", UNASSIGNED,NULL,$Sequence$list$__init__,$Sequence$list$__getitem__, $Sequence$list$__setitem__, $Sequence$list$__delitem__,
                                                                  $Sequence$list$__getslice__, $Sequence$list$__setslice__, $Sequence$list$__delslice__,
                                                               $Sequence$list$__reversed__,$Sequence$list$insert,$Sequence$list$append,$Sequence$list$reverse};
struct $Sequence$list $Sequence$list_instance = {&$Sequence$list$methods, &$Collection$list_instance,&$Plus$list_instance};
$Sequence$list $Sequence$list$witness = &$Sequence$list_instance;

struct $Collection$list$class $Collection$list$methods = {"",UNASSIGNED, NULL,$Collection$list$__init__,$Collection$list$__iter__,
                                                          $Collection$list$__fromiter__,$Collection$list$__len__};
struct $Collection$list $Collection$list_instance = {&$Collection$list$methods,&$Sequence$list_instance};
$Collection$list $Collection$list$witness = &$Collection$list_instance;


struct $Plus$list$class $Plus$list$methods = {"", UNASSIGNED,NULL,$Plus$list$__init__, $Plus$list$__add__};
struct $Plus$list $Plus$list_instance = {&$Plus$list$methods, &$Sequence$list_instance};
$Plus$list $Plus$list$witness = &$Plus$list_instance;

struct $Container$list$class $Container$list$methods = {"", UNASSIGNED,NULL,$Container$list$__init__,
                                                        ($Iterator (*)($Container$list, $list))$Collection$list$__iter__,
                                                        ($list (*)($Container$list,$Iterable$opaque))$Collection$list$__fromiter__,
                                                        ($int (*)($Container$list, $list))$Collection$list$__len__,
                                                        $Container$list$__contains__,$Container$list$__containsnot__};


$list $Plus$list$__add__ ($Plus$list wit, $list a, $list b) {
  return $list_add(a,b);
}

$Iterator $Collection$list$__iter__($Collection$list wit, $list self) {
  return $list_iter(self);
}
 
$list $Collection$list$__fromiter__ ($Collection$list wit, $Iterable$opaque iter) {
  return $list_fromiter(iter);
}

$int $Collection$list$__len__($Collection$list wit, $list self) {
  return to$int($list_len(self));
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

$list $Sequence$list$__getslice__($Sequence$list wit, $list self, $Slice slice) {
  return $list_getslice(self,slice);
}

void $Sequence$list$__setslice__($Sequence$list wit, $list self, $Slice slice, $Iterable$opaque it) {
  $list_setslice(self,slice,it->proto->$class->__iter__(it->proto,it->impl));
}

void $Sequence$list$__delslice__($Sequence$list wit, $list self, $Slice slice) {
  $list_delslice(($list)self,slice);
}  

$bool $Container$list$__contains__($Container$list wit, $list self, $WORD elem) {
  return to$bool($list_contains(wit->w$Eq$A,self,elem));
}
                 
$bool $Container$list$__containsnot__($Container$list wit, $list self, $WORD elem) {
  return to$bool($list_containsnot(wit->w$Eq$A,self,elem));
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

void $Container$list$__init__($Container$list self, $Eq w$Eq$A) {
  self->w$Eq$A = w$Eq$A;
}

void $Collection$list$__init__($Collection$list self, $Sequence$list master) {
  self->w$Sequence$list = master;
}

void $Plus$list$__init__($Plus$list self, $Sequence$list master) {
  self->w$Sequence$list = master;
}

void $Sequence$list$__init__($Sequence$list self) {
  self->w$Collection$Sequence = $NEW($Collection$list, self);
  self->w$Plus$Sequence = $NEW($Plus$list, self);
}
