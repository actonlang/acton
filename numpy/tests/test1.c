#include "../../builtin/builtin.h"
#include "../numpy.h"

int main(int argc, char *argv[]) {
  long n;
  long iters;
  sscanf(argv[1],"%ld",&n);
  sscanf(argv[2],"%ld",&iters);
  for (int i=0;i<iters; i++) {
    numpy$$ndarray x = numpy$$linspace(to$float((double)i),to$float((double)i+1), to$int(n));
    //printf("x=%s\n",x->$class->__str__(x)->str);
    $list ix = $NEW($list,NULL,NULL);
    $slice s = $NEW($slice,NULL,NULL,NULL);
    $list_append(ix,numpy$$ndslice$new(s));
    $list_append(ix,numpy$$ndindex$new(numpy$$newaxis));
    numpy$$Integral$ndarray wit = $NEW(numpy$$Integral$ndarray,(numpy$$Primitive)numpy$$Primitive$float$witness);
    numpy$$ndarray r = wit->$class->__add__(wit,numpy$$ndarray$__ndgetslice__(x,ix),x);
    //printf("r->shape=%s\n",r->shape->$class->__str__(r->shape)->str);
    //printf("r->strides=%s\n",r->strides->$class->__str__(r->strides)->str);
    //printf("r=%s\n",r->$class->__str__(r)->str);
    $value sm = ($value)numpy$$sum((numpy$$Primitive)numpy$$Primitive$float$witness,r,NULL);
    $printobj("sum(r) =",sm);
    free(r->data);
    free(r);
  }
}
        
