#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#include "../builtin.h"

//For now...
void RAISE(exception e) {
  fprintf(stderr,"exception raised\n");
  exit(1);
}

long fromWord($WORD w) {
  return *(long*)w;
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

$list fromto(long a, long b) {
  $list res = $list_fromiter(NULL);
  for (long i = a; i<b; i++)
    $list_append(res,toWord(i));
  return res;
}


// sum : Sequence[Sequence[A]]) -> Sequence[A]
$WORD sum(Sequence s) {
  $WORD res = s->__class__->Collection$__methods__->__fromiter__(NULL)->__impl__;
  $int len = s->__class__->Collection$__methods__->__len__((Collection)s);
  for (long i = 0; i < fromWord(len); i++) {
    $WORD nxt = s->__class__->Sliceable$__methods__->Indexed$__methods__->__getitem__((Indexed)s,toWord(i));
    res = s->__class__->Plus$__methods__->__add__(res,nxt);
  }
  return res;
}
 
int main() {
  list_instance_init();
  $list lst = $list_fromiter(NULL);
  for (long i = 1; i< 10; i++) {
     $list_append(lst,fromto(i,2*i));
  }
  Sequence s = Sequence$__pack__(Sequence$list_instance,($WORD)lst);
  $list lst2 = ($list)sum(s);
  printlist(lst2);
}
                 
  
