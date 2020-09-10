#include "../../builtin/builtin.h"
#include "../ndarray.h"

int main(int argc, char *argv[]) {
  long n;
  long iters;
  sscanf(argv[1],"%ld",&n);
  sscanf(argv[2],"%ld",&iters);
  $ndarray x = $ndarray_linspace(to$float(0.0),to$float(1.0), to$int(n));
  $list ix = $NEW($list,NULL,NULL);
  $Slice s = $NEW($Slice,NULL,NULL,NULL);
  $list_append(ix,s);
  $list_append(ix,NULL);
  for (int i=0;i<iters; i++) {
    $ndarray r = $ndarray_oper1(plusf,$nd_getslice(x,ix),x);
    printf("%f\n",from$float($ndarray_sumf(r)));
    free(r->data);
    free(r);
  }
}
        
