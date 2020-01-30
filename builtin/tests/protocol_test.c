#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#include "../builtin.h"

 
void printSequence(Sequence$__class__ cl, $WORD seq) {
  Indexed$__class__ cl1 = cl->Sliceable$__methods__->Indexed$__methods__;
  printf("[");
  long n = from$int(cl->Collection$__methods__->__len__(cl->Collection$__methods__,
                                                        seq));
  for (long i=0; i < n-1; i++) 
    printf("%ld, ",from$int(cl1->__getitem__(cl1,seq,to$int(i))));
  if (n > 0) 
    printf("%ld",from$int(cl1->__getitem__(cl1,seq,to$int(n-1))));
  printf("]\n");
}

$list range(long a, long b) {
  Sequence$__class__ cl = Sequence$list_instance;  
  $WORD res = cl->Collection$__methods__->__fromiter__(cl->Collection$__methods__,NULL)->__impl__;
  for (long i=a; i<b; i++)
    cl->append(cl,res,to$int(i));
  return res;
}

/*
$list fromto(long a, long b) {
  $list res = $list_fromiter(NULL);
  for (long i = a; i<b; i++)
    $list_append(res,to$int(i));
  return res;
}
*/
// concat : (Sequence[A(Plus)],A) -> A
$WORD concat(Sequence$__class__ cl, Plus$__class__ cl1, $WORD s, $WORD zero) {
  $WORD res = zero;
  $int len = cl->Collection$__methods__->__len__(cl->Collection$__methods__,s);
  for (long i = 0; i < *len; i++) {
    Indexed$__class__ cl2 = cl->Sliceable$__methods__->Indexed$__methods__;
    $WORD nxt = cl2->__getitem__(cl2,s,to$int(i));
    res = cl1->__add__(cl1,res,nxt);
  }
  return res;
}
/*
$WORD $int_add(Plus$__class__ cl, $WORD a, $WORD b) {
  return to$int(from$int(a)+from$int(b));
}

struct Plus$__class__ Plus$int_struct = {"GC_Plus",$int_add};
Plus$__class__ Plus$int_instance = &Plus$int_struct;
*/
int main() {
  Sequence$__class__ cl = Sequence$list_instance;
  // first we use concat for list concatenation
  $WORD lst = cl->Collection$__methods__->__fromiter__(cl->Collection$__methods__,NULL)->__impl__;
  $WORD emptylist = cl->Collection$__methods__->__fromiter__(cl->Collection$__methods__,NULL)->__impl__;
  for (long i = 1; i< 10; i++) {
    cl->append(cl,lst,range(i,2*i));
  }
  printSequence(cl,concat(cl,Plus$list_instance,lst,emptylist));
  // and then to sum a list of integers
  $WORD lst2 = range(1,100);
  printf("1+2+...+99 = %ld\n",from$int(concat(cl,Plus$int_instance,lst2,to$int(0))));
  // and finally as a very complicated identity function for strings
  printf("result is '%s'\n",toUTF8(concat(Sequence$str_instance,Plus$str_instance,fromUTF8("Complicated identity function"),fromUTF8(""))));
}
                 
  
