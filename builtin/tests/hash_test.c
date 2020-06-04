#include <stdio.h>
#include <stdlib.h>

#include "../builtin.h"

int main() {
  $Hashable wit = ($Hashable)$Hashable$str$witness;
  printf("hash of %d is %ld\n",543,$int_hash(to$int(543)));
  printf("hash of %d is %ld\n",-1,$int_hash(to$int(-1)));
  printf("hash of %d is %ld\n",-5,$int_hash(to$int(-5)));
  printf("hash of %f is %ld\n",-2.0,$float_hash(to$float(-2.0)));
  printf("hash of %f is %ld\n",-1.0,$float_hash(to$float(-1.0)));
  printf("hash of %f is %ld\n",0.75,$float_hash(to$float(0.75)));
  printf("hash of 'test' as $str is %ld\n",from$int(wit->$class->__hash__(wit,to$str("test"))));
}
