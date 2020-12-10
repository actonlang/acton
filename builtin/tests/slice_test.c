#include "../builtin.h"
 
int main() {

  $slice slc = $slice$new(to$int(-1),to$int(0),to$int(-2));
  $Sequence$list wit = $Sequence$list$witness;
  $list lst = $list$new(NULL,NULL);
  for (long i=0; i<100; i++)
    wit->$class->append(wit,lst,to$int(i));
  $list lst2 = wit->$class->__getslice__(wit,lst,slc);
  $print(2,to$str("lst2 = "),lst2);
  $list lst3 =  $list$new(NULL,NULL);
  for (long i=100; i<110; i++)
    wit->$class->append(wit,lst3,to$int(i));
  $print(2,to$str("lst3 = "),lst3);
  slc = $slice$new(to$int(10),to$int(30),to$int(2));
  wit->$class->__setslice__(wit,($Iterable)wit->w$Collection,lst2,slc,lst3);
  $print(2,to$str("lst2 = "),lst2);
  $range r = $NEW($range,to$int(10000),NULL,NULL);
  $list lst4 = wit->w$Collection->$class->__fromiter__(wit->w$Collection,($Iterable)$Iterable$range$witness,r);

  $Iterator it = $Iterable$range$witness->$class->__iter__($Iterable$range$witness,$NEW($range,to$int(1000),to$int(1),to$int(-1)));
  $int i;
  while((i = ($int)it->$class->__next__(it))) {
  slc = $slice$new(to$int(0),to$int(10000),i);
    $list_delslice(lst4,slc);
  }
  $print(2,to$str("lst4 = "),lst4);
}
