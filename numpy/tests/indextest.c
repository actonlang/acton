#include "../../builtin/builtin.h"
#include "../numpy.h"

int main() {
  $register_builtin();
  numpy$$__init__();
  numpy$$ndarray v = numpy$$ndarray_arange(to$int(0),to$int(60),to$int(1));
  $list newshape = $NEW($list,NULL,NULL);
  $list_append(newshape,to$int(3));
  $list_append(newshape,to$int(4));
  $list_append(newshape,to$int(5));
  numpy$$ndarray a = numpy$$ndarray_reshape(v,newshape);
  $printobj("a=",a);
  $list ix = $NEW($list,NULL,NULL);
  $list_append(ix,numpy$$ndindex$new(numpy$$newaxis));
  $Slice s1 = $NEW($Slice,NULL,NULL,NULL);
  $list_append(ix,numpy$$ndslice$new(s1));
  $list_append(ix,numpy$$ndindex$new(to$int(2)));
  numpy$$ndarray b = numpy$$ndarray$__ndgetslice__(a,ix);
  $printobj("b=",b);
  $list ix1 = $NEW($list,NULL,NULL);
  $list_append(ix1,numpy$$ndindex$new(to$int(1)));
  $Slice s2 = $NEW($Slice,to$int(1),NULL,NULL);
  $list_append(ix1,numpy$$ndslice$new(s2));
  $Slice s3 = $NEW($Slice,to$int(-1),to$int(0),to$int(-1));
  $list_append(ix1,numpy$$ndslice$new(s3));
  numpy$$ndarray c = numpy$$ndarray$__ndgetslice__(a,ix1);
  $printobj("c=",c);
  $list test = $list_copy(c->shape);
  numpy$$Primitive wit = (numpy$$Primitive)numpy$$Primitive$int$witness;
  numpy$$ndarray d = numpy$$ndarray_abs(wit,c);
  $printobj("d=",d);
  numpy$$ndarray e = numpy$$ndarray_fromatom(($Super)to$int(3));
  $printobj("e.shape =",e->shape);
  $printobj("e.strides =",e->strides);
  $printobj("e=",e);
  numpy$$Integral$ndarray wit2 = $NEW(numpy$$Integral$ndarray,wit);
  numpy$$ndarray f = wit2->$class->__add__(wit2,d,e);
  $printobj("f=",f);  
  numpy$$ndarray g = wit2->$class->__add__(wit2,a,e);
  $printobj("g.shape =",g->shape);
  $printobj("g.strides",g->strides);
  $printobj("g=",g);  
}
