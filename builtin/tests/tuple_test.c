#include <stdio.h>
#include "../builtin.h"

int main() {
  $tuple tup1 = $NEW($tuple,3,to$int(7),to$str("A string"),to$float(3.14));
  $Sliceable$tuple wit = $Sliceable$tuple$witness;
  $print(2,to$str("tup1 = "),tup1);
  int start = 0;
  int stop = 3;
  int step = 2;
  struct $Slice slc;
  slc.start = &start;
  slc.stop = &stop;
  slc.step = &step;
  $float pi =  ($float)wit->$class->__getitem__(wit,tup1,to$int(2));
  printf("pi = %f\n",from$float(pi));
  $tuple tup2 = wit->$class->__getslice__(wit,tup1,&slc);
  $print(2,to$str("tup2 = "),tup2);
  $Hashable wits[] = {($Hashable)$Hashable$int$witness, ($Hashable)$Hashable$str$witness, ($Hashable)$Hashable$float$witness};
  $Hashable wit2 = ($Hashable)$NEW($Hashable$tuple,3, ($Hashable*)&wits);
  $dict d = $NEW($dict,wit2,NULL,NULL);
  $dict_setitem(d,wit2,tup1,to$int(13));
  $int n =  ($int)$dict_getitem(d,wit2,tup1);
  printf("n=%ld\n",from$int(n));
}
