#include "../builtin.h"
#include <stdio.h>

int main() {
  $float a = to$float(7.0);
  $float b = to$float(5.0);
  $Real$float wit = $Real$float$witness;
  $Complex$float wit2 = $Complex$float$witness;
  $float c = wit2->class->__truediv__(wit2,a,b);
  printf("7.0/5.0)=%f\n",from$float(c));
  $Integral$opaque d = wit->class->__floor__(wit,wit2->class->__truediv__(wit2,a,b));
  printf("floor(7/5)=%ld\n",from$int(d->impl));
}
