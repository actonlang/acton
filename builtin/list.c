
struct $Sequence$list $Sequence$list_instance;
struct $Collection$list $Collection$list_instance;
struct $Plus$list $Plus$list_instance;

struct $Sequence$list$class $Sequence$list$methods = {
    "$Sequence$list", 
    UNASSIGNED,
    ($Super$class)&$Sequence$methods,
    $Sequence$list$__init__,
    $Sequence$list$__serialize__,
    $Sequence$list$__deserialize__,
    ($bool (*)($Sequence$list))$default__bool__,
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
    ($Plus)&$Plus$list_instance
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
    $Collection$list$__iter__,
    $Collection$list$__fromiter__,
    $Collection$list$__len__
};
struct $Collection$list $Collection$list_instance = {
    &$Collection$list$methods,
    ($Sequence)&$Sequence$list_instance
};
$Collection$list $Collection$list$witness = &$Collection$list_instance;


struct $Plus$list$class $Plus$list$methods = {
    "$Plus$list",
    UNASSIGNED,
    ($Super$class)&$Plus$methods,
    $Plus$list$__init__,
    $Plus$list$__serialize__,
    $Plus$list$__deserialize__,
    ($bool (*)($Plus$list))$default__bool__,
    ($str (*)($Plus$list))$default__str__,
    $Plus$list$__add__,
    ($list (*)($Plus$list, $list, $list))$Plus$__iadd__,
};
struct $Plus$list $Plus$list_instance = {
    &$Plus$list$methods,
    ($Sequence)&$Sequence$list_instance
};
$Plus$list $Plus$list$witness = &$Plus$list_instance;

struct $Container$list$class $Container$list$methods = {
    "$Container$list",
    UNASSIGNED,
    ($Super$class)&$Container$methods,
    $Container$list$__init__,
    $Container$list$__serialize__,
    $Container$list$__deserialize__,
    ($bool (*)($Container$list))$default__bool__,
    ($str (*)($Container$list))$default__str__,
    ($Iterator (*)($Container$list, $list))$Collection$list$__iter__,
    ($list (*)($Container$list,$Iterable,$WORD))$Collection$list$__fromiter__,
    ($int (*)($Container$list, $list))$Collection$list$__len__,
    $Container$list$__contains__,
    $Container$list$__containsnot__
};

void $Plus$list$__serialize__($Plus$list self, $Serial$state state) {
  $step_serialize(self->w$Sequence, state);
}

$Plus$list $Plus$list$__deserialize__($Serial$state state) {
   $Plus$list res = $DNEW($Plus$list,state);
   res->w$Sequence = ($Sequence)$step_deserialize(state);
   return res;
}

$list $Plus$list$__add__ ($Plus$list wit, $list a, $list b) {
  return $list_add(a,b);
}

void $Collection$list$__serialize__($Collection$list self, $Serial$state state) {
  $step_serialize(self->w$Sequence, state);
}

$Collection$list $Collection$list$__deserialize__($Serial$state state) {
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
  $step_serialize(self->w$Plus, state);
}

$Sequence$list $Sequence$list$__deserialize__($Serial$state state) {
   $Sequence$list res = $DNEW($Sequence$list,state);
   res->w$Collection = ($Collection)$step_deserialize(state);
   res->w$Plus = ($Plus)$step_deserialize(state);
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

$list $Sequence$list$__getslice__($Sequence$list wit, $list self, $Slice slice) {
  return $list_getslice(self,slice);
}

void $Sequence$list$__setslice__($Sequence$list wit, $Iterable wit2, $list self, $Slice slice, $WORD iter) {
  $list_setslice(self,slice,wit2->$class->__iter__(wit2,iter));
}

void $Sequence$list$__delslice__($Sequence$list wit, $list self, $Slice slice) {
  $list_delslice(($list)self,slice);
}  

void $Container$list$__serialize__($Container$list self, $Serial$state state) {
  $step_serialize(self->w$Eq$A$Container$list, state);
}

$Container$list $Container$list$__deserialize__($Serial$state state) {
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

void $Plus$list$__init__($Plus$list self, $Sequence master) {
  self->w$Sequence = master;
}

$Sequence$list $Sequence$list$new() {
  return $NEW($Sequence$list);
}

$Collection$list $Collection$list$new($Sequence master){
  return $NEW($Collection$list, master);
}
$Plus$list $Plus$list$new($Sequence master) {
  return $NEW($Plus$list, master);
}

void $Sequence$list$__init__($Sequence$list self) {
  self->w$Collection = ($Collection)$Collection$list$new(($Sequence)self);
  self->w$Plus = ($Plus)$Plus$list$new(($Sequence)self);
}
