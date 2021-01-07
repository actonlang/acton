#include "builtin.h"
#include "set_impl.h"
 

void $set_serialize($set, $Serial$state);
$set $set_deserialize($set, $Serial$state);

struct $Ord$set $Ord$set_instance;
struct $Minus$set $Minus$set_instance;
struct $Logical$set $Logical$set_instance;

struct $Set$set$class $Set$set$methods = {
    "$Set$set",
    UNASSIGNED,
    ($Super$class)&$Set$methods,
    $Set$set$__init__,
    $Set$set$__serialize__,
    $Set$set$__deserialize__,
    ($bool (*)($Set$set))$default__bool__,
    ($str (*)($Set$set))$default__str__,
    $Set$set$__iter__,
    $Set$set$__fromiter__,
    $Set$set$__len__,
    $Set$set$__contains__,
    $Set$set$__containsnot__,
    $Set$set$isdisjoint,
    $Set$set$add,
    $Set$set$discard,
    $Set$set$pop
};   

struct $Ord$set$class $Ord$set$methods = {
    "$Ord$set",
    UNASSIGNED,
    ($Super$class)&$Ord$methods,
    $Ord$set$__init__,
    $Ord$set$__serialize__,
    $Ord$set$__deserialize__,
    ($bool (*)($Ord$set))$default__bool__,
    ($str (*)($Ord$set))$default__str__,
    $Ord$set$__eq__,
    $Ord$set$__ne__,
    $Ord$set$__lt__,
    $Ord$set$__le__,
    $Ord$set$__gt__,
    $Ord$set$__ge__
};

struct $Minus$set$class $Minus$set$methods = {
    "$Minus$set",
    UNASSIGNED,
    ($Super$class)&$Minus$methods,
    $Minus$set$__init__,
    $Minus$set$__serialize__,
    $Minus$set$__deserialize__,
    ($bool (*)($Minus$set))$default__bool__,
    ($str (*)($Minus$set))$default__str__,
    $Minus$set$__sub__,
    ($set (*)($Minus$set, $set, $set))$Minus$__isub__,

};

struct $Logical$set$class $Logical$set$methods = {
    "$Logical$set",
    UNASSIGNED,
    ($Super$class)&$Logical$methods,
    $Logical$set$__init__,
    $Logical$set$__serialize__,
    $Logical$set$__deserialize__,
    ($bool (*)($Logical$set))$default__bool__,
    ($str (*)($Logical$set))$default__str__,
    $Logical$set$__and__,
    $Logical$set$__or__,
    $Logical$set$__xor__,
    ($set (*)($Logical$set, $set, $set))$Logical$__iand__,
    ($set (*)($Logical$set, $set, $set))$Logical$__ior__,
    ($set (*)($Logical$set, $set, $set))$Logical$__ixor__
};

// $Set

void $Set$set$__serialize__($Set$set self, $Serial$state state) {
  $step_serialize(self->w$Ord, state);
  $step_serialize(self->w$Logical, state);
  $step_serialize(self->w$Minus, state);
  $step_serialize(self->w$Eq$A$Set$set, state);
  $step_serialize(self->w$Hashable$A$Set$set, state);
}

$Set$set $Set$set$__deserialize__($Set$set self, $Serial$state state) {
   $Set$set res = $DNEW($Set$set,state);
   res->w$Ord = ($Ord)$step_deserialize(state);
   res->w$Logical = ($Logical)$step_deserialize(state);
   res->w$Minus = ($Minus)$step_deserialize(state);
   res->w$Eq$A$Set$set = ($Eq)$step_deserialize(state);
   res->w$Hashable$A$Set$set = ($Hashable)$step_deserialize(state);
   return res;
}


$Iterator $Set$set$__iter__ ($Set$set wit, $set set) {
  return $set_iter(set);
}

$set $Set$set$__fromiter__($Set$set wit, $Iterable wit2, $WORD iter) {
  return $set_fromiter(wit->w$Hashable$A$Set$set,wit2->$class->__iter__(wit2,iter));
}

$int $Set$set$__len__ ($Set$set wit, $set set) {
  return to$int($set_len(set));
}
$bool $Set$set$__contains__ ($Set$set wit, $set set, $WORD val) {
  return  to$bool($set_contains(set,wit->w$Hashable$A$Set$set,val));
}

$bool $Set$set$__containsnot__ ($Set$set wit, $set set, $WORD val) {
  return  to$bool(!$set_contains(set,wit->w$Hashable$A$Set$set,val));
}

$bool $Set$set$isdisjoint ($Set$set wit, $set set, $set other) {
  return to$bool($set_isdisjoint(wit->w$Hashable$A$Set$set,set,other));
}

void $Set$set$add ($Set$set wit, $set set, $WORD elem) {
   $set_add(set,wit->w$Hashable$A$Set$set,elem);
}

void $Set$set$discard ($Set$set wit, $set set, $WORD elem) {
  $set_discard(set,wit->w$Hashable$A$Set$set,elem);
}

$WORD $Set$set$pop ($Set$set wit, $set set) {
  return $set_pop(set);
}

// $Ord

void $Ord$set$__serialize__($Ord$set self, $Serial$state state) {
  $step_serialize(self->w$Set, state);
}

$Ord$set $Ord$set$__deserialize__($Ord$set self, $Serial$state state) {
   $Ord$set res = $DNEW($Ord$set,state);
   res->w$Set = ($Set)$step_deserialize(state);
   return res;
}

$bool $Ord$set$__eq__ ($Ord$set wit, $set a, $set b) {
  return to$bool($set_eq((($Set$set)wit->w$Set)->w$Hashable$A$Set$set,a,b));
}
  
$bool $Ord$set$__ne__ ($Ord$set wit, $set a, $set b) {
  return to$bool(!$set_eq((($Set$set)wit->w$Set)->w$Hashable$A$Set$set,a,b));
}
  
$bool $Ord$set$__lt__ ($Ord$set wit, $set a, $set b) {
  return to$bool($set_lt((($Set$set)wit->w$Set)->w$Hashable$A$Set$set,a,b));
}
  
$bool $Ord$set$__le__ ($Ord$set wit, $set a, $set b) {
  return to$bool($set_le((($Set$set)wit->w$Set)->w$Hashable$A$Set$set,a,b));
}
  
$bool $Ord$set$__gt__ ($Ord$set wit, $set a, $set b) {
  return to$bool($set_gt((($Set$set)wit->w$Set)->w$Hashable$A$Set$set,a,b));
}
  
$bool $Ord$set$__ge__ ($Ord$set wit, $set a, $set b) {
  return to$bool($set_ge((($Set$set)wit->w$Set)->w$Hashable$A$Set$set,a,b));
}

// $Minus

void $Logical$set$__serialize__($Logical$set self, $Serial$state state) {
  $step_serialize(self->w$Set, state);
}

$Logical$set $Logical$set$__deserialize__($Logical$set self, $Serial$state state) {
   $Logical$set res = $DNEW($Logical$set,state);
   res->w$Set = ($Set)$step_deserialize(state);
   return res;
}

$set $Minus$set$__sub__ ($Minus$set wit, $set a, $set b) {
  return $set_difference((($Set$set)wit->w$Set)->w$Hashable$A$Set$set,a,b);
}

// $Logical

void $Minus$set$__serialize__($Minus$set self, $Serial$state state) {
  $step_serialize(self->w$Set, state);
}

$Minus$set $Minus$set$__deserialize__($Minus$set self, $Serial$state state) {
   $Minus$set res = $DNEW($Minus$set,state);
   res->w$Set = ($Set)$step_deserialize(state);
   return res;
}

$set $Logical$set$__and__($Logical$set wit, $set a, $set b) {
  return $set_intersection((($Set$set)wit->w$Set)->w$Hashable$A$Set$set,a,b);
}

$set $Logical$set$__or__ ($Logical$set wit, $set a, $set b) {
  return $set_union((($Set$set)wit->w$Set)->w$Hashable$A$Set$set,a,b);
}

$set $Logical$set$__xor__($Logical$set wit, $set a, $set b) {
  return $set_symmetric_difference((($Set$set)wit->w$Set)->w$Hashable$A$Set$set,a,b);
}

// init and new

void $Ord$set$__init__($Ord$set self, $Set master) {
  self->w$Set = master;
}

void $Logical$set$__init__($Logical$set self, $Set master) {
  self->w$Set = master;
}

void $Minus$set$__init__($Minus$set self, $Set master) {
  self->w$Set = master;
}

$Set$set $Set$set$new($Hashable h) {
  return $NEW($Set$set, h);
}

void $Set$set$__init__($Set$set self, $Hashable h) {
  self->w$Ord = ($Ord)$NEW($Ord$set,($Set)self);
  self->w$Logical = ($Logical)$NEW($Logical$set,($Set)self);
  self->w$Minus = ($Minus)$NEW($Minus$set,($Set)self);
  self->w$Eq$A$Set$set = ($Eq)h;
  self->w$Hashable$A$Set$set = h;
}
   
