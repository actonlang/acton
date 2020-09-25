#include "../../builtin/builtin.h"
#include "../numpy.h"


int main() {
  $list elems = $NEW($list,NULL,NULL);
  $list_append(elems,to$float(2.0));
  $list_append(elems,to$float(3.0));
  $list_append(elems,to$float(5.0));
  $list_append(elems,to$float(7.0));
  $ndarray q = $ndarray_array(($Primitive)$Primitive$float$witness,elems);
  $printobj("q =",q);
  $ndarray a = $ndarray_arange(NULL,to$int(10),NULL);
  $printobj("a =",a);
  $list ix = $NEW($list,NULL,NULL);
  $list_append(ix,to$int(2));
  $list_append(ix,to$int(5));
  $printobj("a.reshape(2,5)=",$ndarray_reshape(a,ix));
  $ndarray b = $ndarray_linspace(to$float(0),to$float(1),to$int(5));
  $printobj("b =",b);
  
  $list ix1 = $NEW($list,NULL,NULL);
  $list_append(ix1,to$int(7));
  $ndarray c = $ndarray_getslice(a,ix1);
  $printobj("a[7] =",c);
  $ndarray d = $ndarray_fromatom(to$float(3.5));
  $printobj("d =",d);
}

