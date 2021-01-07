#include "../../builtin/builtin.h"
#include "../numpy.h"

int main() {
  numpy$$ndarray v = numpy$$ndarray_arange(to$int(0),to$int(60),to$int(1));
  $list newshape = $NEW($list,NULL,NULL);
  $list_append(newshape,to$int(3));
  $list_append(newshape,to$int(2));
  $list_append(newshape,to$int(2));
  $list_append(newshape,to$int(5));
  numpy$$ndarray a = numpy$$ndarray_reshape(v,newshape);
  $printobj("a.shape =",a->shape);
  $printobj("a.strides =",a->strides);
  $printobj("a =",a);
  numpy$$Integral$ndarray wit = $NEW(numpy$$Integral$ndarray,(numpy$$Primitive)numpy$$Primitive$int$witness);
  numpy$$ndarray b = wit->$class->__add__(wit,a, a);
  $printobj("b.shape =",b->shape);
  $printobj("b.strides =",b->strides);
  $printobj("b =",b);
}
