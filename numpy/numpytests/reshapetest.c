#include "../../builtin/builtin.h"
#include "../ndarray.h"

int main() {
  $ndarray a = $ndarray_arange(to$int(60));
  printf("a.shape = %s\n",a->shape->$class->__str__(a->shape)->str);
  printf("a.strides = %s\n",a->strides->$class->__str__(a->strides)->str);
  printf("a=%s\n",a->$class->__str__(a)->str);
  $list newshape = $NEW($list,NULL,NULL);
  $list_append(newshape,to$int(3));
  $list_append(newshape,to$int(4));
  $list_append(newshape,to$int(5));
  $ndarray b = $ndarray_reshape(a,newshape);
  printf("b.shape = %s\n",b->shape->$class->__str__(b->shape)->str);
  printf("b.strides = %s\n",b->strides->$class->__str__(b->strides)->str);
  printf("b=%s\n",b->$class->__str__(b)->str);
  $list ix = $NEW($list,NULL,NULL);
  $list_append(ix,$NEW($Slice,to$int(2),NULL,to$int(3)));
  $ndarray c = $nd_getslice(a,ix);
  printf("c.shape = %s\n",c->shape->$class->__str__(c->shape)->str);
  printf("c.strides = %s\n",c->strides->$class->__str__(c->strides)->str);
  printf("c=%s\n",c->$class->__str__(c)->str);
  $list newshape2 = $NEW($list,NULL,NULL);
  $list_append(newshape2,to$int(4));
  $list_append(newshape2,to$int(5));
  $ndarray d = $ndarray_reshape(c,newshape2);
  printf("d.shape = %s\n",d->shape->$class->__str__(d->shape)->str);
  printf("d.strides = %s\n",d->strides->$class->__str__(d->strides)->str);
  printf("d=%s\n",d->$class->__str__(d)->str);
  $list newshape3 = $NEW($list,NULL,NULL);
  $list_append(newshape3,to$int(20));
  $ndarray e = $ndarray_reshape(d,newshape3);
  printf("e.shape = %s\n",e->shape->$class->__str__(e->shape)->str);
  printf("e.strides = %s\n",e->strides->$class->__str__(e->strides)->str);
  printf("e=%s\n",e->$class->__str__(e)->str);
  $list newshape4 = $NEW($list,NULL,NULL);
  $list_append(newshape4,to$int(60));
  $ndarray f = $ndarray_reshape(b,newshape4);
  printf("f.shape = %s\n",f->shape->$class->__str__(f->shape)->str);
  printf("f.strides = %s\n",f->strides->$class->__str__(f->strides)->str);
  printf("f=%s\n",f->$class->__str__(f)->str);

}
