#include "../../builtin/builtin.h"
#include "../ndarray.h"

int main() {
  $ndarray a = $ndarray_linspace(to$float(0.0),to$float(47.0),to$int(48));
  $ndarray b = $ndarray_linspace(to$float(0.0),to$float(2.0),to$int(3));
  $list shape = $NEW($list,NULL,NULL);
  $list_append(shape,to$int(4));
  $list_append(shape,to$int(4));
  $list_append(shape,to$int(3));
  $ndarray a1 = $ndarray_reshape(a,shape);
  $ndarray c = $ndarray_dot$float(a1,b);
  printf("%s\n",c->$class->__str__(c)->str);
}
