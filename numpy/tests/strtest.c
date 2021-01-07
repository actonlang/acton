#include "../../builtin/builtin.h"
#include "../numpy.h"


int main() {
  $list elems = $NEW($list,NULL,NULL);
  $list_append(elems,to$float(2.0));
  $list_append(elems,to$float(3.0));
  $list_append(elems,to$float(5.0));
  $list_append(elems,to$float(7.0));
  numpy$$ndarray q = numpy$$ndarray_array((numpy$$Primitive)numpy$$Primitive$float$witness,elems);
  $printobj("q =",q);
  numpy$$ndarray a = numpy$$ndarray_arange(to$int(0),to$int(10),to$int(1));
  $printobj("a =",a);
  $list ix = $NEW($list,NULL,NULL);
  $list_append(ix,to$int(2));
  $list_append(ix,to$int(5));
  $printobj("a.reshape(2,5)=",numpy$$ndarray_reshape(a,ix));
  numpy$$ndarray b = numpy$$ndarray_linspace(to$float(0),to$float(1),to$int(5));
  $printobj("b =",b);
  
  $list ix1 = $NEW($list,NULL,NULL);
  $list_append(ix1,numpy$$ndindex$new(to$int(7)));
  numpy$$ndarray c = numpy$$ndarray$__ndgetslice__(a,ix1);
  $printobj("a[7] =",c);
  numpy$$ndarray d = numpy$$ndarray_fromatom(to$float(3.5));
  $printobj("d =",d);
}

