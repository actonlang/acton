#include "builtin.h"
#include "set_impl.h"
 

void $set_serialize($set, $Mapping$dict, long*, $dict, $ROWLISTHEADER);
$set $set_deserialize($Mapping$dict, $ROW*, $dict);

static struct $Ord$set $Ord$set_instance;
static struct $Minus$set $Minus$set_instance;
static struct $Logical$set $Logical$set_instance;

static struct $Set$set$class $Set$set_methods = {"",  (void (*)($Set$set,$Hashable,$Eq))$default3__init__,$Set$set$__iter__, $Set$set$__fromiter__, $Set$set$__len__, $Set$set$__contains__,
                                                   $Set$set$__containsnot__, $Set$set$isdisjoint, $Set$set$add, $Set$set$discard, $Set$set$pop};   

static struct $Ord$set$class $Ord$set_methods = {"", (void (*)($Ord$set,$Set,$Hashable))$default3__init__,$Ord$set$__eq__,$Ord$set$__ne__,$Ord$set$__lt__,$Ord$set$__le__,$Ord$set$__gt__,$Ord$set$__ge__};

static struct $Minus$set$class $Minus$set_methods = {"", (void (*)($Minus$set,$Set,$Hashable))$default3__init__,$Minus$set$__sub__};

static struct $Logical$set$class $Logical$set_methods = {"", (void (*)($Logical$set,$Set,$Hashable))$default3__init__,$Logical$set$__and__,$Logical$set$__or__,$Logical$set$__xor__};

$Iterator $Set$set$__iter__ ($Set$set wit, $set set) {
  return $set_iter(set);
}
   
$set $Set$set$__fromiter__ ($Set$set wit, $Iterable$opaque it) {
  $Iterator iter = NULL;
  if (it!=NULL)
    iter = it->proto->$class->__iter__(it->proto,it->impl);
  return $set_fromiter(wit->_Hashable,iter);
}

$int $Set$set$__len__ ($Set$set wit, $set set) {
  return to$int($set_len(set));
}
$bool $Set$set$__contains__ ($Set$set wit, $set set, $WORD val) {
  return  to$bool($set_contains(set,wit->_Hashable,val));
}

$bool $Set$set$__containsnot__ ($Set$set wit, $set set, $WORD val) {
  return  to$bool(!$set_contains(set,wit->_Hashable,val));
}

$bool $Set$set$isdisjoint ($Set$set wit, $set set, $set other) {
  return to$bool($set_isdisjoint(wit->_Hashable,set,other));
}

void $Set$set$add ($Set$set wit, $set set, $WORD elem) {
   $set_add(set,wit->_Hashable,elem);
}

void $Set$set$discard ($Set$set wit, $set set, $WORD elem) {
  $set_discard(set,wit->_Hashable,elem);
}

$WORD $Set$set$pop ($Set$set wit, $set set) {
  return $set_pop(set);
}

$bool $Ord$set$__eq__ ($Ord$set wit, $set a, $set b) {
  return to$bool($set_eq(wit->_Hashable,a,b));
}
  
$bool $Ord$set$__ne__ ($Ord$set wit, $set a, $set b) {
  return to$bool(!$set_eq(wit->_Hashable,a,b));
}
  
$bool $Ord$set$__lt__ ($Ord$set wit, $set a, $set b) {
  return to$bool($set_lt(wit->_Hashable,a,b));
}
  
$bool $Ord$set$__le__ ($Ord$set wit, $set a, $set b) {
  return to$bool($set_le(wit->_Hashable,a,b));
}
  
$bool $Ord$set$__gt__ ($Ord$set wit, $set a, $set b) {
  return to$bool($set_gt(wit->_Hashable,a,b));
}
  
$bool $Ord$set$__ge__ ($Ord$set wit, $set a, $set b) {
  return to$bool($set_ge(wit->_Hashable,a,b));
}
  
$set $Minus$set$__sub__ ($Minus$set wit, $set a, $set b) {
  return $set_difference(wit->_Hashable,a,b);
}

$set $Logical$set$__and__($Logical$set wit, $set a, $set b) {
  return $set_intersection(wit->_Hashable,a,b);
}

$set $Logical$set$__or__ ($Logical$set wit, $set a, $set b) {
  return $set_union(wit->_Hashable,a,b);
}

$set $Logical$set$__xor__($Logical$set wit, $set a, $set b) {
  return $set_symmetric_difference(wit->_Hashable,a,b);
}
 
$Set$set $Set$set_new($Hashable h) {
  $Set$set res = malloc(sizeof(struct $Set$set));
  res->$class = &$Set$set_methods;
  $Ord$set res2 =  malloc(sizeof(struct $Ord$set));
  $Logical$set res3 =  malloc(sizeof(struct $Logical$set));
  $Minus$set res4 =  malloc(sizeof(struct $Minus$set));
  res->_Ord = ($Ord)res2;
  res->_Logical = ($Logical)res3;
  res->_Minus = ($Minus)res4;
  res->_Hashable = h;
  res2->$class = &$Ord$set_methods;
  res2->_Set = ($Set)res;
  res2->_Hashable = h;
  res3->$class = &$Logical$set_methods;
  res3->_Set = ($Set)res;
  res3->_Hashable = h;
  res4->$class = &$Minus$set_methods;
  res4->_Set = ($Set)res;
  res4->_Hashable = h;
  return res;
}
/*
void __init__($Set$set self, $Hashable h) {
  self->_Ord = ;
  self->_Logical = ;
  self->_Minus = ;
  self->_Hashable = ;
}
*/    
