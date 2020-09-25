#include "../../builtin/builtin.h"
#include "../numpy.h"

int main(int argc, char *argv[]) {
  long n;
  long iters;
  sscanf(argv[1],"%ld",&n);
  sscanf(argv[2],"%ld",&iters);
  for (int i=0;i<iters; i++) {
    $ndarray x = $ndarray_linspace(to$float((double)i),to$float((double)i+1), to$int(n));
    //printf("x=%s\n",x->$class->__str__(x)->str);
    $list ix = $NEW($list,NULL,NULL);
    $Slice s = $NEW($Slice,NULL,NULL,NULL);
    $list_append(ix,s);
    $list_append(ix,NULL);
    $Integral$ndarray wit = $NEW($Integral$ndarray,($Primitive)$Primitive$float$witness);
    $ndarray r = wit->$class->__add__(wit,$ndarray_getslice(x,ix),x);
    //printf("r->shape=%s\n",r->shape->$class->__str__(r->shape)->str);
    //printf("r->strides=%s\n",r->strides->$class->__str__(r->strides)->str);
    //printf("r=%s\n",r->$class->__str__(r)->str);
    $struct sm = ($struct)$ndarray_sum(($Primitive)$Primitive$float$witness,r,NULL);
    $printobj("sum(r) =",sm);
    free(r->data);
    free(r);
  }
}
        
