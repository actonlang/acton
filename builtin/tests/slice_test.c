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

void printSequence($Sequence$list wit, $WORD seq) {
  printf("[");
  long n = from$int(wit->w$Collection$Sequence->$class->__len__(wit->w$Collection$Sequence, seq));
  for (long i=0; i < n-1; i++) 
    printf("%ld, ",from$int(wit->$class->__getitem__(wit,seq,to$int(i))));
  if (n > 0) 
    printf("%ld",from$int(wit->$class->__getitem__(wit,seq,to$int(n-1))));
  printf("]\n");
}



int main() {
  $Sequence$list wit = $Sequence$list$witness;
  $list lst = wit->w$Collection$Sequence->$class->__fromiter__( wit->w$Collection$Sequence,NULL);
  for (long i=0; i<100; i++)
    wit->$class->append(wit,lst,to$int(i));
  int start = -1;
  int stop = 0;
  int step = -2;
  struct $Slice slc;
  slc.start = &start;
  slc.stop = &stop;
  slc.step = &step;
  $list lst2 = wit->$class->__getslice__(wit,lst,&slc);
  printSequence(wit,lst2);
  $list lst3 = wit->w$Collection$Sequence->$class->__fromiter__( wit->w$Collection$Sequence,NULL);
  for (long i=100; i<110; i++)
    wit->$class->append(wit,lst3,to$int(i));
  printSequence(wit,lst3);
  start = 10;
  stop = 30;
  step = 2;
  $Iterable$opaque it = $Iterable$pack(($Iterable)wit->w$Collection$Sequence,lst3);
  wit->$class->__setslice__(wit,lst2,&slc,it);
  printSequence(wit,lst2);                                                       
}
