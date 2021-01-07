#include "../../builtin/builtin.h"
#include "../numpy.h"

int main(int argc, char *argv[]) {
  $register_builtin();
  numpy$$__init__();
  long n;
  sscanf(argv[1],"%ld",&n);
  numpy$$ndarray x = numpy$$ndarray_linspace(to$float(0.0),to$float(1.0), to$int(n*n*n*n));
  numpy$$ndarray y = numpy$$ndarray_arange(to$int(0),to$int(n*n*n*n),to$int(1));
  //$printobj("x =",x);
  $list newshape = $NEW($list,NULL,NULL);
  $list_append(newshape,to$int(n));
  $list_append(newshape,to$int(n));
  $list_append(newshape,to$int(n));
  $list_append(newshape,to$int(n));
  numpy$$ndarray a = numpy$$ndarray_reshape(x,newshape);
  numpy$$ndarray b = numpy$$ndarray_reshape(y,newshape);
  //$printobj("a =",a);
  //for (int i = 0; i<100; i++) {
    $struct s = ($struct)numpy$$ndarray_sum((numpy$$Primitive)numpy$$Primitive$float$witness,a,NULL);
    $printobj("sum(a) =",s);
    $struct t = ($struct)numpy$$ndarray_sum((numpy$$Primitive)numpy$$Primitive$int$witness,b,NULL);
    //$printobj("b =",b);  
    $printobj("sum(b) =",t);
    //  }
}
