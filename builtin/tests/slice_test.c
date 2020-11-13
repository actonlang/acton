#include "../builtin.h"
 
int main() {
  int start, stop, step;
  struct $Slice slc;
  slc.start = &start;
  slc.stop = &stop;
  slc.step = &step;
  
  start = -1;
  stop = 0;
  step = -2;
  $Sequence$list wit = $Sequence$list$witness;
  $list lst = $NEW($list,NULL,NULL);
  for (long i=0; i<100; i++)
    wit->$class->append(wit,lst,to$int(i));
  $list lst2 = wit->$class->__getslice__(wit,lst,&slc);
  $print(2,to$str("lst2 = "),lst2);
  $list lst3 =  $NEW($list,NULL,NULL);
  for (long i=100; i<110; i++)
    wit->$class->append(wit,lst3,to$int(i));
  $print(2,to$str("lst3 = "),lst3);
  /*
  start = 10;
  stop = 30;
  step = 2;
  wit->$class->__setslice__(wit,($Iterable)wit,lst2,&slc,lst3);
  $print(2,to$str("lst2 = "),lst2);
  $range r = $NEW($range,NULL,to$int(100000),NULL);
  $list lst4 = $NEW($list,($Sequence)$Sequence$range$witness,r);
  start = 0;
  stop = 100000;
  $Iterator it = $Iterable$range$witness->$class->__iter__($Iterable$range$witness,$NEW($range,to$int(1000),to$int(1),to$int(-1)));
  $int i;
  while((i = ($int)it->$class->__next__(it))) {
    step = i->val;
    $list_delslice(lst4,&slc);
  }
  $print(2,to$str("lst4 = "),lst4);
  */
}
