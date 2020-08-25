#include "../builtin.h"
#include <stdio.h>

int main() {
  $int a = to$int(3);
  $int b = to$int(5);
  $Integral$int wit = $Integral$int$witness;
  $int c = wit->$class->__lshift__(wit,a,b);
  printf("3<<5=%ld\n",from$int(c));
  $Number$int wit2 = $Number$int$witness;
  $int d = wit2->$class->__mul__(wit2,a,wit2->w$Plus$Number->$class->__add__(wit2->w$Plus$Number,b,c));
  printf("3*(5+3<<5)=%ld\n",from$int(d));
  printf("round(1234567,-5)=%ld\n",$Integral$int$__round__($Integral$int$witness,to$int(1234567),to$int(-5))->val);
  printf("round(1234567,5)=%ld\n",$Integral$int$__round__($Integral$int$witness,to$int(1234567),to$int(5))->val);
}
