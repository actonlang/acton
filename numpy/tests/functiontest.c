#include "../../builtin/builtin.h"
#include "../numpy.h"

int main() {
  $ndarray v = $ndarray_arange(to$int(60),NULL,NULL);
  $list newshape = $NEW($list,NULL,NULL);
  $list_append(newshape,to$int(3));
  $list_append(newshape,to$int(4));
  $list_append(newshape,to$int(5));
  $ndarray a = $ndarray_reshape(v,newshape);
  $printobj("a=\n",a);
  $printobj("a.transpose() =\n",$ndarray_transpose(a,NULL));

  $list axes = $NEW($list,NULL,NULL);
  $list_append(axes,to$int(1));
  $list_append(axes,to$int(2));
  $list_append(axes,to$int(0));
  $printobj("a.transpose([1,2,0]) =\n",$ndarray_transpose(a,axes));
  $ndarray b = $ndarray_reshape($ndarray_arange(to$int(60),to$int(0),to$int(-1)),newshape);
  $printobj("b=\n",b);
  $Primitive wit = ($Primitive)$Primitive$int$witness;
  $ndarray c = $ndarray_sort(($Primitive)$Primitive$int$witness,b,NULL);
  $printobj("b.sort() =\n",c);
  $printobj("b.sort(-1) =\n",$ndarray_sort(wit,b,to$int(-1)));
  $printobj("a.clip(12,40) =\n",$ndarray_clip(wit,a,to$int(12),to$int(40)));
  $printobj("a.clip(17,None) =\n",$ndarray_clip(wit,a,to$int(17),NULL));
  $Iterable iter = ($Iterable)$NEW($Iterable$ndarray,wit);
  $Iterator it = iter->$class->__iter__(iter,c);
  $WORD nxt;
  while ((nxt = it->$class->__next__(it)))
    $printobj("",nxt);
}
