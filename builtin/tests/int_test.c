#include "../builtin.h"
#include <stdio.h>

int main() {
  $int a = to$int(3);
  $int b = to$int(5);
  $Integral$int wit = $Integral$int$witness;
  $int c = wit->$class->__lshift__(wit,a,b);
  printf("3<<5=%ld\n",from$int(c));
  $int d = wit->$class->__mul__(wit,a,wit->$class->__add__(wit,b,c));
  printf("3*(5+3<<5)=%ld\n",from$int(d));
  printf("round(1234567,-5)=%ld\n",$Integral$int$__round__($Integral$int$witness,to$int(1234567),to$int(-5))->val);
  printf("round(1234567,5)=%ld\n",$Integral$int$__round__($Integral$int$witness,to$int(1234567),to$int(5))->val);
}
