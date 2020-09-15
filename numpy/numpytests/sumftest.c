#include "../../builtin/builtin.h"
#include "../ndarray.h"

int main(int argc, char *argv[]) {
  long n;
  sscanf(argv[1],"%ld",&n);
  $ndarray x = $ndarray_linspace(to$float(0.0),to$float(1.0), to$int(n*n*n*n));
  //printf("x=%s\n",x->$class->__str__(x)->str);
  $list newshape = $NEW($list,NULL,NULL);
  $list_append(newshape,to$int(n));
  $list_append(newshape,to$int(n));
  $list_append(newshape,to$int(n));
  $list_append(newshape,to$int(n));
  $ndarray a = $ndarray_reshape(x,newshape);
  //printf("a.shape = %s\n",a->shape->$class->__str__(a->shape)->str);
  //printf("a.strides = %s\n",a->strides->$class->__str__(a->strides)->str);
  //printf("a=%s\n",x->$class->__str__(a)->str);
  printf("sumf(a) = %f\n",$ndarray_sumf(a)->val);
}
