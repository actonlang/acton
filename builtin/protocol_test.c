#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#include "common.h"
#include "list.h"

//For now...
void RAISE(exception e) {
  fprintf(stderr,"exception raised\n");
  exit(1);
}

long fromWord($WORD w) {
  return *(long*) w;
}

$WORD toWord(long i) {
  $WORD res = malloc(sizeof(long));
  *(long*)res = i;
  return res;
}

void printlist($list lst) {
  $WORD w;
  printf("[");
  for (int i=0; i < *$list_len(lst)-1; i++) {
    w = $list_getitem(lst,i);
    printf("%ld, ",fromWord(w));
  }
  if (*$list_len(lst) > 0) {
    w = $list_getitem(lst,*$list_len(lst)-1);
    printf("%ld",fromWord(w));
  }
  printf("]\n");
}

$list fromto(int a, int b) {
  $list res = $list_fromiter(NULL);
  for (long i = a; i<b; i++)
    $list_append(res,toWord(i));
  return res;
}


// sum : Sequence[Sequence[A]]) -> Sequence[A]
//Note that we do only a special case. The code below does not cover the case where Sequence[A] is replaced by B(Plus)
//Note also that we cannot use __len__ to find the length of s, because we cannot pass Container. This must be fixed.
$WORD sum(int n, Sequence s) {
  $WORD res = s->__class__->Sliceable$__methods__->Indexed$__methods__->__getitem__((Indexed)s,toWord(0));
  // We cannot follow the chain past Container_Eq, since we have a function there 
  //  int len = s->__class__->Container_Eq$__methods__->Collection_$__methods__->__len__((Collection)s);
  for (int i = 1; i < n; i++) {
    $WORD nxt = s->__class__->Sliceable$__methods__->Indexed$__methods__->__getitem__((Indexed)s,toWord(i));
    res = s->__class__->Plus$__methods__->__add__(res,nxt);
  }
  return res;
}
 
int main() {
  list_instance_init();
  $list lst = $list_fromiter(NULL);
  for (int i = 1; i< 10; i++) {
     $list_append(lst,fromto(i,2*i));
  }
  Sequence s = Sequence$__pack__(Sequence_$list_instance,($WORD)lst);
  $list lst2 = ($list)sum(*$list_len(lst),s);
  printlist(lst2);
}
                 
  
