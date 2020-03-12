#include <stddef.h>
#include <stdio.h>
#include "../builtin.h"

 
$list range(Sequence$list wit, long a, long b) {
  $list res = wit->_Collection->__class__->__fromiter__(wit->_Collection,NULL);
  for (long i=a; i<b; i++)
    wit->__class__->append(wit,res,to$int(i));
  return res;
}
 
int main() {
  $list lst = range(Sequence$list_new(),1,100);
  Container$list wit2 = Container$list_new((Eq)Hashable$int_new());
  $bool b = wit2->__class__->__contains__(wit2,lst,to$int(17));
  $bool c = wit2->__class__->__contains__(wit2,lst,to$int(171));
  $bool d = wit2->__class__->__contains__(wit2,lst,to$int(100));
  printf("results are %d, %d, %d\n",from$bool(b),from$bool(c),from$bool(d));
}
  
