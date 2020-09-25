#include "../../builtin/builtin.h"
#include "../numpy.h"

int main() {
  $ndarray a = $ndarray_arange(NULL,to$int(60),NULL);
  $printobj("a.shape =",a->shape);
  $printobj("a.strides =",a->strides);
  $printobj("a=",a);
  $list newshape = $NEW($list,NULL,NULL);
  $list_append(newshape,to$int(3));
  $list_append(newshape,to$int(4));
  $list_append(newshape,to$int(5));
  $ndarray b = $ndarray_reshape(a,newshape);
  //$printobj("b.shape =",b->shape);
  //$printobj("b.strides =",b->strides);
  $printobj("b =",b);
  $list ix = $NEW($list,NULL,NULL);
  $list_append(ix,$NEW($Slice,to$int(2),NULL,to$int(3)));
  $ndarray c = $ndarray_getslice(a,ix);
  $printobj("c.shape =",c->shape);
  $printobj("c.strides =",c->strides);
  $printobj("c =",c);
  $list newshape2 = $NEW($list,NULL,NULL);
  $list_append(newshape2,to$int(4));
  $list_append(newshape2,to$int(5));
  $ndarray d = $ndarray_reshape(c,newshape2);
  $printobj("d.shape =",d->shape);
  $printobj("d.strides =",d->strides);
  $printobj("d =",d);
  $list newshape3 = $NEW($list,NULL,NULL);
  $list_append(newshape3,to$int(20));
  $ndarray e = $ndarray_reshape(d,newshape3);
  $printobj("e.shape =\n",e->shape);
  $printobj("e.strides =",e->strides);
  $printobj("e =",e);
  $list newshape4 = $NEW($list,NULL,NULL);
  $list_append(newshape4,to$int(60));
  $ndarray f = $ndarray_reshape(b,newshape4);
  $printobj("f.shape =",f->shape);
  $printobj("f.strides =",f->strides);
  $printobj("f =",f);

}
