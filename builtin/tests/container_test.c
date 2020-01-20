#include <stddef.h>
#include <stdio.h>
#include "../builtin.h"

Sequence fromto(long a, long b) {
  Sequence$__class__ cl = Sequence$list_instance;  
  $WORD res = cl->Collection$__methods__->__fromiter__( cl->Collection$__methods__,NULL)->__impl__;
  for (long i=a; i<b; i++)
    cl->append(cl,res,to$int(i));
  return Sequence$__pack__(cl,res);
}

/*
$bool $int__eq__(Eq$__class__ cl,$WORD a, $WORD b) {
  return *($int)a ==  *($int)b;
}

$bool $int__neq__(Eq$__class__ cl,$WORD a, $WORD b) {
  return *($int)a !=  *($int)b;
}

struct Eq$__class__ Eq$int_instance = {"...GC",$int__eq__,$int__neq__};
*/
int main() {
  $WORD lst = fromto(1,100)->__impl__;
  Container_Eq$__class__ cl = Container_Eq$list_instance(Eq$int_instance);
  $bool b = cl->__contains__(cl,lst,to$int(17));
  $bool c = cl->__contains__(cl,lst,to$int(171));
  $bool d = cl->__contains__(cl,lst,to$int(100));
  printf("results are %d, %d, %d\n",b,c,d);
}
  
