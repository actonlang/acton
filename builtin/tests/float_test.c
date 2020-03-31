#include "../builtin.h"
#include <stdio.h>

int main() {
  $float a = to$float(7.0);
  $float b = to$float(5.0);
  Real$float wit = Real$float_new();
  Complex$float wit2 = Complex$float_new();
  $float c = wit2->__class__->__truediv__(wit2,a,b);
  printf("7.0/5.0)=%f\n",from$float(c));
  Integral$opaque d = wit->__class__->__floor__(wit,wit2->__class__->__truediv__(wit2,a,b));
  printf("floor(7/5)=%ld\n",from$int(d->__impl__));
}
