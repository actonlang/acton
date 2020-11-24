#include "../../builtin/builtin.h"
#include "../numpy.h"

int main(int argc, char *argv[]) {
  long n;
  sscanf(argv[1],"%ld",&n);
  $ndarray x = $ndarray_linspace(to$float(0.0),to$float(1.0), to$int(n*n*n*n));
  $ndarray y = $ndarray_arange(to$int(n*n*n*n),NULL,NULL);
  //$printobj("x =",x);
  $list newshape = $NEW($list,NULL,NULL);
  $list_append(newshape,to$int(n));
  $list_append(newshape,to$int(n));
  $list_append(newshape,to$int(n));
  $list_append(newshape,to$int(n));
  $ndarray a = $ndarray_reshape(x,newshape);
  $ndarray b = $ndarray_reshape(y,newshape);
  //$printobj("a =",a);
  //for (int i = 0; i<100; i++) {
    $struct s = ($struct)$ndarray_sum(($Primitive)$Primitive$float$witness,a,NULL);
    $printobj("sum(a) =",s);
    $struct t = ($struct)$ndarray_sum(($Primitive)$Primitive$int$witness,b,NULL);
    //$printobj("b =",b);  
    $printobj("sum(b) =",t);
    //  }
}
