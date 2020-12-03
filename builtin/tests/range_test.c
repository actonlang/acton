#include "../builtin.h"
#include <stdio.h>

int main() {
  $register_builtin();
  $Iterable wit = ($Iterable)$Iterable$range$new();
  $range r1 = $range$new(to$int(2),to$int(10),to$int(3));
  $Iterator i1 = wit->$class->__iter__(wit,r1);
  $int n;
  while ((n = ($int)i1->$class->__next__(i1)))
    printf("%ld ",from$int(n));
  printf("\n");
  $range r2 = $range$new(to$int(50),to$int(10),to$int(-4));
  $serialize_file(($Serializable)r2,"range.bin");
  $range r3 = ($range)$deserialize_file("range.bin");
  $list lst = $list_fromiter(wit->$class->__iter__(wit,r3));
  $print(2,to$str("lst = "),lst);
  $set s = $set_fromiter(($Hashable)$Hashable$int$witness,wit->$class->__iter__(wit,r2));
  $Set$set wit2 = $Set$set$new(($Hashable)$Hashable$int$witness);
  $list lst2 = $list_fromiter(wit2->$class->__iter__(wit2,s));
  $print(2,to$str("lst2 = "),lst2);
}
