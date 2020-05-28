#include <stdio.h>
#include "../builtin.h"
 
int main() {
 $Iterable$opaque it = $Iterable$pack(($Iterable)$Iterable$range$witness,$NEW($range,to$int(1),to$int(100),to$int(1)));
 $list lst = $NEW($list,it);
  $Container$list wit2 = $NEW($Container$list,($Eq)$Hashable$int$witness);
  $bool b = wit2->$class->__contains__(wit2,lst,to$int(17));
  $bool c = wit2->$class->__contains__(wit2,lst,to$int(171));
  $bool d = wit2->$class->__contains__(wit2,lst,to$int(100));
  printf("results are %s, %s, %s\n",(b->$class->__str__(b))->str,(c->$class->__str__(c))->str,(d->$class->__str__(d))->str);
}
  
