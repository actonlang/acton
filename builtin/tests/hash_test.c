#include <stdio.h>
#include <stdlib.h>

#include "../builtin.h"

$float to$float(double x) {
  $float res = malloc(sizeof(double));
  *(double*)res = x;
  return res;
}

int main() {
  
  long x1 = 543;
  long x2 = -1;
  long x3 = -5;
  printf("hash of %ld is %ld\n",x1,$int_hash(&x1));
  printf("hash of %ld is %ld\n",x2,$int_hash(&x2));
  printf("hash of %ld is %ld\n",x3,$int_hash(&x3));
  printf("hash of %f is %ld\n",-2.0,$float_hash(to$float(-2.0)));
  printf("hash of %f is %ld\n",-1.0,$float_hash(to$float(-1.0)));
  printf("hash of %f is %ld\n",0.75,$float_hash(to$float(0.75)));
  printf("hash of '%s' is %ld\n","test",$string_hash("test",4));
  printf("hash of 'test' as $str is %ld\n",*Eq_Hashable$str_instance->__hash__(Eq_Hashable$str_instance,fromUTF8("test")));
}
