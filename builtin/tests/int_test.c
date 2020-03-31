#include "../builtin.h"
#include <stdio.h>

int main() {
  $int a = to$int(3);
  $int b = to$int(5);
  Integral$int wit = Integral$int_new();
  $int c = wit->__class__->__lshift__(wit,a,b);
  printf("3<<5=%ld\n",from$int(c));
  Complex$int wit2 = Complex$int_new();
  $int d = wit2->__class__->__mul__(wit2,a,wit2->_Plus->__class__->__add__(wit2->_Plus,b,c));
  printf("3*(5+3<<5)=%ld\n",from$int(d));
}
