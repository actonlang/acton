#include <stdio.h>

void RAISE(exception e) {
  //  fprintf(stderr,"exception raised\n");
  //exit(1);
}

//$complex to$complex(double re, double im) {
//  return re + I*im;
//}

$WORD next(Iterator iter) {
  return iter->__class__->__next__(iter);
}
