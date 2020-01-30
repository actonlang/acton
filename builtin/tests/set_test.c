#include <stdio.h>
#include <stdlib.h>
#include "../builtin.h"
 

int main() {
  Set$__class__ cl = Set$set_instance;
  $set s = $set_new(Hashable$int_instance);
  $set s2 = $set_new(Hashable$int_instance);
  printf("sets created\n");
  for (long i = 13; i < 1000; i++) {
    cl->add(cl,s,to$int(i*i));
  }
  cl->discard(cl,s,to$int(64));
  cl->discard(cl,s,to$int(225));
  cl->discard(cl,s,to$int(10000));
  int n = 0;
  for (long k = 0; k < 1000; k++)
    if (cl->Container_Eq$__methods__->__contains__(cl->Container_Eq$__methods__,s,to$int(k))) {
      n++;
    }
  printf("#elements <1000 is %d (should be 18)\n",n);
  for (long i = 0; i < 500; i++) {
    cl->add(cl,s2,to$int(i*i*i*i));
  }
  Collection$__class__ cl1 = cl->Container_Eq$__methods__->Collection$__methods__;
  printf("size of s is %ld (should be 985)\n",*cl1->__len__(cl1,s));
  printf("size of s2 is %ld (should be 500)\n",*cl1->__len__(cl1,s2));
  $set s3 = cl->Logical$__methods__->__and__(cl->Logical$__methods__,s,s2);
  printf("size of intersection is %ld (should be 27)\n",*cl1->__len__(cl1,s3));
  printf("checking if intersection is lt both operands; returns %d and %d\n",cl->Ord$__methods__->__lt__(cl->Ord$__methods__,s3,s),cl->Ord$__methods__->__lt__(cl->Ord$__methods__,s3,s2));
  Iterator iter = Iterable$set_instance->__iter__(Iterable$set_instance,s3);
  $WORD w;
  printf("Iterating over intersection:\n");
  while((w = next(iter))) 
    printf("%ld\n",from$int(w));
  $set s4 = cl->Logical$__methods__->__or__(cl->Logical$__methods__,s,s2);
  printf("size of union is %ld (should be 1458)\n",*cl1->__len__(cl1,s4));
  printf("checking if union is gt both operands; returns %d and %d\n",cl->Ord$__methods__->__gt__(cl->Ord$__methods__,s4,s),cl->Ord$__methods__->__gt__(cl->Ord$__methods__,s4,s2));
  $set s5 = cl->Logical$__methods__->__xor__(cl->Logical$__methods__,s,s2);
  printf("size of symmetric difference is %ld\n",*cl1->__len__(cl1,s5));
  printf("popping and printing again elements in intersection\n");
  while((w = cl->pop(cl,s3)))
    printf("popped %ld\n",from$int(w));
  printf("size of intersection is now %ld\n",*cl1->__len__(cl1,s3));
   
        
}
            
