#include "../../builtin/builtin.h"
#include "../numpy.h"

int main() {
  $ndarray v = $ndarray_arange(to$int(60),NULL,NULL);
  $list newshape = $NEW($list,NULL,NULL);
  $list_append(newshape,to$int(3));
  $list_append(newshape,to$int(2));
  $list_append(newshape,to$int(2));
  $list_append(newshape,to$int(5));
  $ndarray a = $ndarray_reshape(v,newshape);
  $printobj("a.shape =",a->shape);
  $printobj("a.strides =",a->strides);
  $printobj("a =",a);
  $Integral$ndarray wit = $NEW($Integral$ndarray,($Primitive)$Primitive$int$witness);
  $ndarray b = wit->$class->__add__(wit,a, a);
  $printobj("b.shape =",b->shape);
  $printobj("b.strides =",b->strides);
  $printobj("b =",b);
}
