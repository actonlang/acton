#include "../builtin.h"
#include <stdio.h>

int main() {
  $int a = to$int(3);
  $int b = to$int(5);
  $Integral$int wit = $Integral$int$witness;
  $int c = wit->$class->__lshift__(wit,a,b);
  printf("3<<5=%ld\n",from$int(c));
  $Complex$int wit2 = $Complex$int$witness;
  $int d = wit2->$class->__mul__(wit2,a,wit2->w$Plus$Complex->$class->__add__(wit2->w$Plus$Complex,b,c));
  printf("3*(5+3<<5)=%ld\n",from$int(d));
}
