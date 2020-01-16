#include <stddef.h>
#include <stdio.h>
#include "../builtin.h"

void RAISE(exception e) {
  fprintf(stderr,"exception raised\n");
  exit(1);
}
long fromWord($WORD w) {
  return *(long*) w;
}

$WORD toWord(long i) {
  $WORD res = malloc(sizeof(long));
  *(long*)res = i;
  return res;
}

$list range(int a, int b) {
  $list res = $list_fromiter(NULL);
  for (long i = a; i<b; i++)
    $list_append(res,toWord(i));
  return res;
}

$bool $int__eq__($WORD a, $WORD b) {
  return *($int)a ==  *($int)b;
}

$bool $int__neq__($WORD a, $WORD b) {
  return *($int)a !=  *($int)b;
}

struct Eq$__class__ $int_Eq_class = {"...GC",$int__eq__,$int__neq__};

struct Eq Eq$int_instance = {"...GC",&$int_Eq_class,NULL};

int main() {
  list_instance_init();
  $list lst = range(1,100);
  Container_Eq$__class__ cl = Container_Eq$list_instance(&Eq$int_instance);
  Container_Eq lstc = Container_Eq$__pack__(cl,lst);
  $bool b = lstc->__class__->__contains__(lstc,toWord(17));
  $bool c = lstc->__class__->__contains__(lstc,toWord(171));
  $bool d = lstc->__class__->__contains__(lstc,toWord(100));
  printf("results are %d, %d, %d\n",b,c,d);
}
  
