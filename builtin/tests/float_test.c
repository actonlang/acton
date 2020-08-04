#include "../builtin.h"
#include <stdio.h>

int main() {
  $float a = to$float(7.0);
  $float b = to$float(5.0);
  $Real$float wit = $Real$float$witness;
  $Complex$float wit2 = $Complex$float$witness;
  $float c = wit2->$class->__truediv__(wit2,a,b);
  printf("7.0/5.0)=%f\n",from$float(c));
  $Integral$opaque d = wit->$class->__floor__(wit,wit2->$class->__truediv__(wit2,a,b));
  printf("floor(7/5)=%ld\n",from$int(d->impl));
  printf("round(1234567.14,-5)=%f\n",$Real$float$__round__($Real$float$witness,to$float(1234567.14),to$int(-5))->val);
  printf("round(1.2345678,5)=%f\n",$Real$float$__round__($Real$float$witness,to$float(1.2345678),to$int(5))->val);
}
