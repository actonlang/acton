
$int to$int(long i) {
  $WORD res = malloc(sizeof(long));
  *(long*)res = i;
  return res;
}

long from$int($int w) {
  return *(long*) w;
}

$bool $int_eq_instance(Eq$__class__ cl, $WORD a, $WORD b) {
  return from$int(a) == from$int(b);
}

$bool $int_neq_instance(Eq$__class__ cl, $WORD a, $WORD b) {
  return from$int(a) != from$int(b);
}

$int $int_hash_instance(Eq_Hashable$__class__ cl, $WORD a) {
  return to$int($int_hash(a));
}

$WORD $int_add_instance(Plus$__class__ cl,  $WORD a, $WORD b) {
  return to$int(from$int(a) + from$int(b));
}  

struct Eq$__class__ Eq$int_struct = {"GC_Eq$int",$int_eq_instance,$int_neq_instance};
Eq$__class__ Eq$int_instance = &Eq$int_struct;

struct Eq_Hashable$__class__ Eq_Hashable$int_struct = {"GC_Hashable_Eq$int",$int_eq_instance,$int_neq_instance,$int_hash_instance};
Eq_Hashable$__class__ Eq_Hashable$int_instance = &Eq_Hashable$int_struct;

struct Plus$__class__ Plus$int_struct = {"GC_Plus$int",$int_add_instance};
Plus$__class__ Plus$int_instance = &Plus$int_struct;
