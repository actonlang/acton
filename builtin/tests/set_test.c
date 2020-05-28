#include <stdio.h>
#include <stdlib.h>
#include "../builtin.h"



int main() {
  $Hashable hashwit = ($Hashable)$Hashable$int$witness;
  $Set$set wit= $NEW($Set$set,hashwit);
  $set s = $NEW($set,hashwit,NULL);
  $set s2 = $NEW($set,hashwit,NULL);
  printf("sets created\n");
  for (long i = 13; i < 1000; i++) {
    wit->$class->add(wit,s,to$int(i*i));
  }
  wit->$class->discard(wit,s,to$int(64));
  wit->$class->discard(wit,s,to$int(225));
  wit->$class->discard(wit,s,to$int(10000));
  int n = 0;
  for (long k = 0; k < 1000; k++)
    if (from$bool(wit->$class->__contains__(wit,s,to$int(k)))) {
      n++;
    }
  printf("#elements <1000 is %d (should be 18)\n",n);
  for (long i = 0; i < 500; i++) {
    wit->$class->add(wit,s2,to$int(i*i*i*i));
  }
  printf("size of s is %ld (should be 985)\n",from$int(wit->$class->__len__(wit,s)));
  printf("size of s2 is %ld (should be 500)\n",from$int(wit->$class->__len__(wit,s2)));
  $set s3 = wit->w$Logical$Set->$class->__and__(wit->w$Logical$Set,s,s2);
  printf("size of intersection is %ld (should be 27)\n",from$int(wit->$class->__len__(wit,s3)));
          
  printf("checking if intersection is lt both operands; returns %ld and %ld\n",from$bool(wit->w$Ord$Set->$class->__lt__(wit->w$Ord$Set,s3,s)),from$bool(wit->w$Ord$Set->$class->__lt__(wit->w$Ord$Set,s3,s2)));
  $Iterator iter = wit->$class->__iter__(wit,s3);
  $WORD w;
  printf("Iterating over intersection:\n");
  while((w = iter->$class->__next__(iter))) 
    printf("%ld\n",from$int(w));
  printf("Printing intersection with __str__ : %s\n",(s3->$class->__str__(s3))->str); 
  $set s4 = wit->w$Logical$Set->$class->__or__(wit->w$Logical$Set,s,s2);
  printf("size of union is %ld (should be 1458)\n",from$int(wit->$class->__len__(wit,s4)));
  printf("checking if union is gt both operands; returns %ld and %ld\n",from$bool(wit->w$Ord$Set->$class->__gt__(wit->w$Ord$Set,s4,s)),from$bool(wit->w$Ord$Set->$class->__gt__(wit->w$Ord$Set,s4,s2)));
  $set s5 = wit->w$Logical$Set->$class->__xor__(wit->w$Logical$Set,s,s2);
  printf("size of symmetric difference is %ld (should be 1431)\n",from$int(wit->$class->__len__(wit,s5)));
  printf("popping and printing again elements in intersection\n");
  while((w = wit->$class->pop(wit,s3)))
    printf("popped %ld\n",from$int(w));
  printf("size of intersection is now %ld\n",from$int(wit->$class->__len__(wit,s3)));
}
            
