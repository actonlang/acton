#include "../../builtin/builtin.h"
#include "../numpy.h"

int main() {
  $register_builtin();
  numpy$$__init__();
  numpy$$ndarray a = numpy$$arange(to$int(0),to$int(60),to$int(1));
  $printobj("a.shape =",a->shape);
  $printobj("a.strides =",a->strides);
  $printobj("a=",a);
  $list newshape = $NEW($list,NULL,NULL);
  $list_append(newshape,to$int(3));
  $list_append(newshape,to$int(4));
  $list_append(newshape,to$int(5));
  numpy$$ndarray b = numpy$$reshape(a,newshape);
  //$printobj("b.shape =",b->shape);
  //$printobj("b.strides =",b->strides);
  $printobj("b =",b);
  $list ix = $NEW($list,NULL,NULL);
  $list_append(ix,numpy$$ndslice$new($NEW($slice,to$int(2),NULL,to$int(3))));
  numpy$$ndarray c = numpy$$ndarray$__ndgetslice__(a,ix);
  $printobj("c.shape =",c->shape);
  $printobj("c.strides =",c->strides);
  $printobj("c =",c);
  $list newshape2 = $NEW($list,NULL,NULL);
  $list_append(newshape2,to$int(4));
  $list_append(newshape2,to$int(5));
  numpy$$ndarray d = numpy$$reshape(c,newshape2);
  $printobj("d.shape =",d->shape);
  $printobj("d.strides =",d->strides);
  $printobj("d =",d);
  $list newshape3 = $NEW($list,NULL,NULL);
  $list_append(newshape3,to$int(20));
  numpy$$ndarray e = numpy$$reshape(d,newshape3);
  $printobj("e.shape =\n",e->shape);
  $printobj("e.strides =",e->strides);
  $printobj("e =",e);
  $list newshape4 = $NEW($list,NULL,NULL);
  $list_append(newshape4,to$int(60));
  numpy$$ndarray f = numpy$$reshape(b,newshape4);
  $printobj("f.shape =",f->shape);
  $printobj("f.strides =",f->strides);
  $printobj("f =",f);

}
