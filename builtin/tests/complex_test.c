#include "../builtin.h"
#include <stdio.h>

int main() {
  $complex a = to$complex(7.0 + _Complex_I * 2.0);
  $complex b = to$complex(5.0 + _Complex_I * 3.0);
  $Number$complex wit = $Number$complex$witness;
  $complex c = wit->$class->__mul__(wit,a,b);
  $complex c2 = wit->$class->__pow__(wit,a,b);
  $print($NEW($tuple,2,to$str("(7+2i)*(5+3i) = "),c));
  $print($NEW($tuple,2,to$str("(7+2i)**(5+3i) = "),c2));
}
