#include "builtin.h"
#include "set_impl.h"
 

void $set_serialize($set, $Serial$state);
$set $set_deserialize($Serial$state);

struct $Ord$set $Ord$set_instance;
struct $Minus$set $Minus$set_instance;
struct $Logical$set $Logical$set_instance;

struct $Set$set$class $Set$set$methods = {"", UNASSIGNED,NULL, $Set$set$__init__,$Set$set$__iter__, $Set$set$__len__, $Set$set$__contains__,
                                                   $Set$set$__containsnot__, $Set$set$isdisjoint, $Set$set$add, $Set$set$discard, $Set$set$pop};   

struct $Ord$set$class $Ord$set$methods = {"", UNASSIGNED,NULL, $Ord$set$__init__,$Ord$set$__eq__,$Ord$set$__ne__,$Ord$set$__lt__,$Ord$set$__le__,$Ord$set$__gt__,$Ord$set$__ge__};

struct $Minus$set$class $Minus$set$methods = {"", UNASSIGNED,NULL, $Minus$set$__init__,$Minus$set$__sub__};

struct $Logical$set$class $Logical$set$methods = {"", UNASSIGNED,NULL, $Logical$set$__init__,$Logical$set$__and__,$Logical$set$__or__,$Logical$set$__xor__};

$Iterator $Set$set$__iter__ ($Set$set wit, $set set) {
  return $set_iter(set);
}
   
$int $Set$set$__len__ ($Set$set wit, $set set) {
  return to$int($set_len(set));
}
$bool $Set$set$__contains__ ($Set$set wit, $set set, $WORD val) {
  return  to$bool($set_contains(set,wit->w$Hashable$Set,val));
}

$bool $Set$set$__containsnot__ ($Set$set wit, $set set, $WORD val) {
  return  to$bool(!$set_contains(set,wit->w$Hashable$Set,val));
}

$bool $Set$set$isdisjoint ($Set$set wit, $set set, $set other) {
  return to$bool($set_isdisjoint(wit->w$Hashable$Set,set,other));
}

void $Set$set$add ($Set$set wit, $set set, $WORD elem) {
   $set_add(set,wit->w$Hashable$Set,elem);
}

void $Set$set$discard ($Set$set wit, $set set, $WORD elem) {
  $set_discard(set,wit->w$Hashable$Set,elem);
}

$WORD $Set$set$pop ($Set$set wit, $set set) {
  return $set_pop(set);
}

$bool $Ord$set$__eq__ ($Ord$set wit, $set a, $set b) {
  return to$bool($set_eq(wit->w$Set$set->w$Hashable$Set,a,b));
}
  
$bool $Ord$set$__ne__ ($Ord$set wit, $set a, $set b) {
  return to$bool(!$set_eq(wit->w$Set$set->w$Hashable$Set,a,b));
}
  
$bool $Ord$set$__lt__ ($Ord$set wit, $set a, $set b) {
  return to$bool($set_lt(wit->w$Set$set->w$Hashable$Set,a,b));
}
  
$bool $Ord$set$__le__ ($Ord$set wit, $set a, $set b) {
  return to$bool($set_le(wit->w$Set$set->w$Hashable$Set,a,b));
}
  
$bool $Ord$set$__gt__ ($Ord$set wit, $set a, $set b) {
  return to$bool($set_gt(wit->w$Set$set->w$Hashable$Set,a,b));
}
  
$bool $Ord$set$__ge__ ($Ord$set wit, $set a, $set b) {
  return to$bool($set_ge(wit->w$Set$set->w$Hashable$Set,a,b));
}
  
$set $Minus$set$__sub__ ($Minus$set wit, $set a, $set b) {
  return $set_difference(wit->w$Set$set->w$Hashable$Set,a,b);
}

$set $Logical$set$__and__($Logical$set wit, $set a, $set b) {
  return $set_intersection(wit->w$Set$set->w$Hashable$Set,a,b);
}

$set $Logical$set$__or__ ($Logical$set wit, $set a, $set b) {
  return $set_union(wit->w$Set$set->w$Hashable$Set,a,b);
}

$set $Logical$set$__xor__($Logical$set wit, $set a, $set b) {
  return $set_symmetric_difference(wit->w$Set$set->w$Hashable$Set,a,b);
}
 
void $Ord$set$__init__($Ord$set self, $Set$set master) {
  self->w$Set$set = master;
}

void $Logical$set$__init__($Logical$set self, $Set$set master) {
  self->w$Set$set = master;
}

void $Minus$set$__init__($Minus$set self, $Set$set master) {
  self->w$Set$set = master;
}
                      
void $Set$set$__init__($Set$set self, $Hashable h) {
  self->w$Ord$Set = $NEW($Ord$set,self);
  self->w$Logical$Set = $NEW($Logical$set,self);
  self->w$Minus$Set = $NEW($Minus$set,self);
  self->w$Hashable$Set = h;
}
   
