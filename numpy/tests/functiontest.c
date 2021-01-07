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
  $printobj("a=\n",a);
  $printobj("a.transpose() =\n",numpy$$ndarray_transpose(a,NULL));

  $list axes = $NEW($list,NULL,NULL);
  $list_append(axes,to$int(1));
  $list_append(axes,to$int(2));
  $list_append(axes,to$int(0));
  $printobj("a.transpose([1,2,0]) =\n",numpy$$ndarray_transpose(a,axes));
  numpy$$ndarray b = numpy$$ndarray_reshape(numpy$$ndarray_arange(to$int(60),to$int(0),to$int(-1)),newshape);
  $printobj("b=\n",b);
  numpy$$Primitive wit = (numpy$$Primitive)numpy$$Primitive$int$witness;
  numpy$$ndarray c = numpy$$ndarray_sort(wit,b,NULL);
  $printobj("b.sort() =\n",c);
  $printobj("b.sort(-1) =\n",numpy$$ndarray_sort(wit,b,to$int(-1)));
  $printobj("a.clip(12,40) =\n",numpy$$ndarray_clip(wit,a,to$int(12),to$int(40)));
  $printobj("a.clip(17,None) =\n",numpy$$ndarray_clip(wit,a,to$int(17),NULL));
  /*
  $Iterable iter = ($Iterable)$NEW(numpy$$Iterable$ndarray,wit);
  $Iterator it = iter->$class->__iter__(iter,c);
  $WORD nxt;
  while ((nxt = it->$class->__next__(it)))
    $printobj("",nxt);
  */
}
