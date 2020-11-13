#include "../builtin.h"
#include <stdio.h>

int main() {
  $register_builtin();
  $Iterable wit = ($Iterable)$Iterable$range$witness;
  $range r1 = $NEW($range,to$int(2),to$int(10),to$int(3));
  $Iterator i1 = wit->$class->__iter__(wit,r1);
  $int n;
  while ((n = ($int)i1->$class->__next__(i1)))
    printf("%ld ",from$int(n));
  printf("\n");
  $range r2 = $NEW($range,to$int(50),to$int(10),to$int(-4));
  $serialize_file(($Serializable)r2,"range.bin");
  $range r3 = ($range)$deserialize_file("range.bin");
  $list lst = $list_fromiter(wit->$class->__iter__(wit,r3));
  $print(2,to$str("lst = "),lst);
  $set s = $set_fromiter(($Hashable)$Hashable$int$witness,wit->$class->__iter__(wit,r2));
  $Set$set wit2 = $NEW($Set$set,($Hashable)$Hashable$int$witness);
  $list lst2 = $list_fromiter(wit2->$class->__iter__(wit2,s));
  $print(2,to$str("lst2 = "),lst2);
  int start = 2;
  int stop = 13;
  int step = 3;
  struct $Slice slc;
  slc.start = &start;
  slc.stop = &stop;
  slc.step = &step;
  $print(1,$Sequence$range$__getslice__($Sequence$range$witness,r2,&slc));
  $list lst3 = $NEW($list,($Sequence)$Sequence$range$witness,r2);
  $print(1,lst3);
}
