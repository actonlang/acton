#include "../../builtin/builtin.h"
#include "../ndarray.h"

int main(int argc, char *argv[]) {
  $ndarray a = $ndarray_arange(to$int(8));
  $list newshape = $NEW($list,NULL,NULL);
  $list_append(newshape,to$int(2));
  //  $list_append(newshape,to$int(5));
  $list_append(newshape,to$int(2));
  $list_append(newshape,to$int(2));
  $ndarray b = $ndarray_reshape(a,newshape);
  $ndarray c = $ndarray_copy(b);
  //printf("a=%s\n",a->$class->__str__(a)->str);
  printf("c=%s\n",c->$class->__str__(c)->str);
}
