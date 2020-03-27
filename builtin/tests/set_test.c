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
    if (from$bool(wit->__class__->__contains__(wit,s,to$int(k)))) {
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
          
  printf("checking if intersection is lt both operands; returns %d and %d\n",from$bool(wit->_Ord->__class__->__lt__(wit->_Ord,s3,s)),from$bool(wit->_Ord->__class__->__lt__(wit->_Ord,s3,s2)));
  Iterator iter = wit->__class__->__iter__(wit,s3);
  $WORD w;
  printf("Iterating over intersection:\n");
  while((w = iter->__class__->__next__(iter))) 
    printf("%ld\n",from$int(w));
 
  $set s4 = wit->_Logical->__class__->__or__(wit->_Logical,s,s2);
  printf("size of union is %ld (should be 1458)\n",from$int(wit->__class__->__len__(wit,s4)));
  printf("checking if union is gt both operands; returns %d and %d\n",from$bool(wit->_Ord->__class__->__gt__(wit->_Ord,s4,s)),from$bool(wit->_Ord->__class__->__gt__(wit->_Ord,s4,s2)));
  $set s5 = wit->_Logical->__class__->__xor__(wit->_Logical,s,s2);
  printf("size of symmetric difference is %ld\n",from$int(wit->__class__->__len__(wit,s5)));
  printf("popping and printing again elements in intersection\n");
  while((w = wit->__class__->pop(wit,s3)))
    printf("popped %ld\n",from$int(w));
  printf("size of intersection is now %ld\n",from$int(wit->__class__->__len__(wit,s3)));
}
            
