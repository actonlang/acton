#include <stdio.h>
#include "../builtin.h"

int main() {
  $WORD comps1[] = {to$int(7),from$UTF8("A string"),to$float(3.14)};
  $tuple tup1 = $NEW($tuple,3,comps1);
  $Sliceable$tuple wit = $Sliceable$tuple$witness;
  printf("tup1=(%ld,%s,%f)\n",from$int(wit->$class->__getitem__(wit,tup1,to$int(0))),
         to$UTF8(wit->$class->__getitem__(wit,tup1,to$int(1))),from$float(wit->$class->__getitem__(wit,tup1,to$int(2))));
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
  printf("size of tup2 is %d; 2nd element is %f\n",tup2->size,from$float(wit->$class->__getitem__(wit,tup2,to$int(1))));
  $Hashable wits[] = {($Hashable)$Hashable$int$witness, ($Hashable)$Hashable$str$witness, ($Hashable)$Hashable$float$witness};
  $Hashable wit2 = ($Hashable)$NEW($Hashable$tuple,3, ($Hashable*)&wits);
  $dict d = $NEW($dict,wit2,NULL);
  $dict_setitem(d,wit2,tup1,to$int(13));
  $int n =  ($int)$dict_getitem(d,wit2,tup1);
  printf("n=%ld\n",from$int(n));
}
