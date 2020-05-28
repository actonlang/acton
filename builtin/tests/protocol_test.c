#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#include "../builtin.h"

 
void printSequence($Sequence wit, $WORD seq) {
  printf("[");
  long n = from$int(wit->w$Collection$Sequence->$class->__len__(wit->w$Collection$Sequence, seq));
  for (long i=0; i < n-1; i++) 
    printf("%ld, ",from$int(wit->$class->__getitem__(wit,seq,to$int(i))));
  if (n > 0) 
    printf("%ld",from$int(wit->$class->__getitem__(wit,seq,to$int(n-1))));
  printf("]\n");
}

$list range($Sequence wit, long a, long b) {
  $list res = $NEW($list,NULL);
  for (long i=a; i<b; i++)
    wit->$class->append(wit,res,to$int(i));
  return res;
}


$WORD concat($Collection wit1, $Indexed wit2, $Plus wit3, $WORD s, $WORD zero) {
  $WORD res = zero;
  $int len = wit1->$class->__len__(wit1,s);
  for (long i = 0; i < from$int(len); i++) {
    $WORD nxt = wit2->$class->__getitem__(wit2,s,to$int(i));
    res = wit3->$class->__add__(wit3,res,nxt);
  }
  return res;
}
/*
$WORD $int_add(Plus$class cl, $WORD a, $WORD b) {
  return to$int(from$int(a)+from$int(b));
}

struct Plus$class Plus$int_struct = {"GC_Plus",$int_add};
Plus$class Plus$int_instance = &Plus$int_struct;
*/
int main() {
  $Sequence wit = ($Sequence)$Sequence$list$witness;
  // first we use concat for list concatenation
  $WORD lst = $NEW($list,NULL);
  $WORD emptylist = $NEW($list,NULL);
  for (long i = 1; i< 10; i++) {
    wit->$class->append(wit,lst,range(wit,i,2*i));
  }
  printSequence(wit,concat(wit->w$Collection$Sequence,($Indexed)wit,wit->w$Plus$Sequence,lst,emptylist));
  // and then to sum a list of integers
  $WORD lst2 = range(wit,1,100);
  printf("1+2+...+99 = %ld\n",from$int(concat(wit->w$Collection$Sequence,($Indexed)wit,($Plus)$Plus$int$witness,lst2,to$int(0))));
  // and finally as a very complicated identity function for strings
  printf("result is '%s'\n",to$UTF8(concat(($Collection)$Container$str$witness,($Indexed)$Sliceable$str$witness,($Plus)$Plus$str$witness,from$UTF8("Complicated identity function"),from$UTF8(""))));
}
                 
  
