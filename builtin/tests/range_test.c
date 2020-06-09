#include "../builtin.h"
#include <stdio.h>

int main() {
  $register_builtin();
  $range r1 = $NEW($range,to$int(2),to$int(10),to$int(3));
  $Iterator i1 = $Iterable$range$witness->$class->__iter__($Iterable$range$witness,r1);
  $int n;
  while ((n = ($int)i1->$class->__next__(i1)))
    printf("%ld ",from$int(n));
  printf("\n");
  $range r2 = $NEW($range,to$int(50),to$int(10),to$int(-4));
  $serialize_file(($Serializable)r2,"range.bin");
  $range r3 = ($range)$deserialize_file("range.bin");                
  $list lst = $list_fromiter($Iterable$pack(($Iterable)$Iterable$range$witness,r3));
  $print($NEW($tuple,2,to$str("lst = "),lst));   
  $set s = $set_fromiter(($Hashable)$Hashable$int$witness,$Iterable$pack(($Iterable)$Iterable$range$witness,r2));
  $Set$set wit = $NEW($Set$set,($Hashable)$Hashable$int$witness);
  $list lst2 = $list_fromiter($Iterable$pack(($Iterable)wit,s));
  $print($NEW($tuple,2,to$str("lst2 = "),lst2));   
}
