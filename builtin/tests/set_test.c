#include <stdio.h>
#include <stdlib.h>
#include "../builtin.h"
 

int main() {
  Set$set wit= Set$set_new((Hashable)Hashable$int_new());
  $set s = wit->__class__->__fromiter__(wit,NULL);
  $set s2 = wit->__class__->__fromiter__(wit,NULL);
  printf("sets created\n");
  for (long i = 13; i < 1000; i++) {
    wit->__class__->add(wit,s,to$int(i*i));
  }
  wit->__class__->discard(wit,s,to$int(64));
  wit->__class__->discard(wit,s,to$int(225));
  wit->__class__->discard(wit,s,to$int(10000));
  int n = 0;
  for (long k = 0; k < 1000; k++)
    if (wit->__class__->__contains__(wit,s,to$int(k))) {
      n++;
    }
  printf("#elements <1000 is %d (should be 18)\n",n);
  for (long i = 0; i < 500; i++) {
    wit->__class__->add(wit,s2,to$int(i*i*i*i));
  }
  printf("size of s is %ld (should be 985)\n",from$int(wit->__class__->__len__(wit,s)));
  printf("size of s2 is %ld (should be 500)\n",from$int(wit->__class__->__len__(wit,s2)));
  $set s3 = wit->_Logical->__class__->__and__(wit->_Logical,s,s2);
  printf("size of intersection is %ld (should be 27)\n",from$int(wit->__class__->__len__(wit,s3)));
  /*        
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
  */  
        
}
            
