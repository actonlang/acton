#include "../../builtin/builtin.h"
#include "../ndarray.h"

int main() {
  $ndarray v = $ndarray_arange(to$int(60));
  $list newshape = $NEW($list,NULL,NULL);
  $list_append(newshape,to$int(3));
  $list_append(newshape,to$int(2));
  $list_append(newshape,to$int(2));
  $list_append(newshape,to$int(5));
  $ndarray a = $ndarray_reshape(v,newshape);
  printf("a.shape = %s\n",a->shape->$class->__str__(a->shape)->str);
  printf("a.strides = %s\n",a->strides->$class->__str__(a->strides)->str);
  printf("a=%s\n",a->$class->__str__(a)->str);
  $Plus$ndarray$int wit = $Plus$ndarray$int$witness;
  $ndarray b = wit->$class->__add__(wit,a, a);
  printf("b.shape = %s\n",b->shape->$class->__str__(b->shape)->str);
  printf("b.strides = %s\n",b->strides->$class->__str__(b->strides)->str);
  printf("b=%s\n",b->$class->__str__(b)->str);
}
