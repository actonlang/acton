#include <stdio.h>
#include <stdlib.h>

#include "../builtin.h"

/*
int main() {
 
  $list lst = $NEW($list,NULL);
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
  $list lst3 = $NEW($list,NULL);
  for (long i=100; i<110; i++)
    $list_append(lst3,toWord(i));
  start = 10;
  stop = 30;
  step = 2;
  $list_setslice(lst2,&slc,lst3);
  printf("lst2 = %s\n",(lst2->$class->__str__(lst2))->str);
}
*/
 
int main() {
  $Sequence$list wit = $Sequence$list$witness;
  $list lst = $NEW($list,NULL);
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
  printf("lst2 = %s\n",(lst2->$class->__str__(lst2))->str);
  $list lst3 =  $NEW($list,NULL);
  for (long i=100; i<110; i++)
    wit->$class->append(wit,lst3,to$int(i));
  printf("lst3 = %s\n",(lst3->$class->__str__(lst3))->str);
  start = 10;
  stop = 30;
  step = 2;
  $Iterable$opaque it = $Iterable$pack(($Iterable)wit->w$Collection$Sequence,lst3);
  wit->$class->__setslice__(wit,lst2,&slc,it);
  printf("lst2 = %s\n",(lst2->$class->__str__(lst2))->str);
}
