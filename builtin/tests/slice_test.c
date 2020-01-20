#include <stdio.h>
#include <stdlib.h>

#include "../builtin.h"

/*
int main() {
 
  $list lst = $list_fromiter(NULL);
  for (long i=0; i<100; i++)
    $list_append(lst,toWord(i));
  long start = -1;
  long stop = 0;
  long step = -2;
  struct Slice slc;
  slc.start = &start;
  slc.stop = &stop;
  slc.step = &step;
  $list lst2 = $list_getslice(lst,&slc);
  $list lst3 = $list_fromiter(NULL);
  for (long i=100; i<110; i++)
    $list_append(lst3,toWord(i));
  start = 10;
  stop = 30;
  step = 2;
  $list_setslice(lst2,&slc,lst3);
  printlist(lst2);
}
*/
void printSequence(Sequence$__class__ cl, $WORD seq) {
  Indexed$__class__ cl1 = cl->Sliceable$__methods__->Indexed$__methods__;
  printf("[");
  long n = from$int(cl->Collection$__methods__->__len__(cl->Collection$__methods__,
                                                        seq));
  for (long i=0; i < n-1; i++) 
    printf("%ld, ",from$int(cl1->__getitem__(cl1,seq,to$int(i))));
  if (n > 0) 
    printf("%ld",from$int(cl1->__getitem__(cl1,seq,to$int(n-1))));
  printf("]\n");
}

int main() {
  Sequence$__class__ cl = Sequence$list_instance;
  $WORD lst = cl->Collection$__methods__->__fromiter__( cl->Collection$__methods__,NULL)->__impl__;
  for (long i=0; i<100; i++)
    cl->append(cl,lst,to$int(i));
  long start = -1;
  long stop = 0;
  long step = -2;
  struct Slice slc;
  slc.start = &start;
  slc.stop = &stop;
  slc.step = &step;
  $WORD lst2 = cl->Sliceable$__methods__->__getslice__(cl->Sliceable$__methods__,lst,&slc);
  Sequence lst3 = cl->Collection$__methods__->__fromiter__(cl->Collection$__methods__,NULL)->__impl__;
  for (long i=100; i<110; i++)
    cl->append(cl,lst3,to$int(i));
  start = 10;
  stop = 30;
  step = 2;
  cl->Sliceable$__methods__->__setslice__(cl->Sliceable$__methods__,lst2,&slc,Sequence$__pack__(cl,lst3));
  printSequence(cl,lst2);                                                       
}

