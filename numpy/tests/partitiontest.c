#include "../../builtin/builtin.h"
#include "../ndarray.h"

int main(int argc, char *argv[]) {
  $list lst = $NEW($list,NULL,NULL);
  for (int i=0; i<1000; i++)
    $list_append(lst,to$int((long)rand()%200+100));
  $ndarray a = $ndarray_array(lst);
  printf("a=%s\n",a->$class->__str__(a)->str);
  $list newshape = $NEW($list,NULL,NULL);
  $list_append(newshape,to$int(10));
  $list_append(newshape,to$int(100));
  $ndarray c = $ndarray_reshape(a,newshape);
  printf("c=%s\n",c->$class->__str__(c)->str);
  $ndarray d =  $ndarray_partition(c,to$int(4));
  printf("d=%s\n",d->$class->__str__(d)->str);

  $ndarray e = $ndarray_linspace(to$float(0),to$float(1),to$int(101));
  $ndarray f = $ndarray_clip$float(e,to$float(0.2),to$float(0.6));
  printf("f=%s\n",f->$class->__str__(f)->str);
}
