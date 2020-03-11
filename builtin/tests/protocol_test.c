#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#include "../builtin.h"

 
void printSequence(Sequence wit, $WORD seq) {
  printf("[");
  long n = from$int(wit->_Collection->__class__->__len__(wit->_Collection, seq));
  for (long i=0; i < n-1; i++) 
    printf("%ld, ",from$int(wit->__class__->__getitem__(wit,seq,to$int(i))));
  if (n > 0) 
    printf("%ld",from$int(wit->__class__->__getitem__(wit,seq,to$int(n-1))));
  printf("]\n");
}

$list range(Sequence$list wit, long a, long b) {
  $list res = wit->_Collection->__class__->__fromiter__(wit->_Collection,NULL);
  for (long i=a; i<b; i++)
    wit->__class__->append(wit,res,to$int(i));
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
$WORD concat(Sequence wit1, Plus wit2, $WORD s, $WORD zero) {
  $WORD res = zero;
  $int len = wit1->_Collection->__class__->__len__(wit1->_Collection,s);
  for (long i = 0; i < from$int(len); i++) {
    $WORD nxt = wit1->__class__->__getitem__(wit1,s,to$int(i));
    res = wit2->__class__->__add__(wit2,res,nxt);
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
  Sequence wit = (Sequence)Sequence$list_new();
  // first we use concat for list concatenation
  $WORD lst = wit->_Collection->__class__->__fromiter__(wit->_Collection,NULL);
  $WORD emptylist = wit->_Collection->__class__->__fromiter__(wit->_Collection,NULL);
  for (long i = 1; i< 10; i++) {
    wit->__class__->append(wit,lst,range(wit,i,2*i));
  }
  printSequence(wit,concat(wit,wit->_Plus,lst,emptylist));
  // and then to sum a list of integers
  $WORD lst2 = range(wit,1,100);
  printf("1+2+...+99 = %ld\n",from$int(concat(wit,Plus$int_new(),lst2,to$int(0))));
  // and finally as a very complicated identity function for strings
  // printf("result is '%s'\n",toUTF8(concat(Sequence$str_new(),Plus$str_new(),fromUTF8("Complicated identity function"),fromUTF8(""))));
}
                 
  
