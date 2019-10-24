#include <stdio.h>
#include <stdlib.h>
#include "set.h"
#include "hash.h"
#include "iterator.h"

/* 
  If UNBOXED is defined, both keys and values are unboxed integers. 
  If not, they are boxed.

  Unboxed ints do not really work since key 0 is recognized as NULL...
*/

// #define UNBOXED

#ifdef UNBOXED
WORD toWord(long i) {
  return (WORD)i;
}

long fromWord(WORD w) {
  return (long) w;
}

#else

WORD toWord(long i) {
  WORD res = malloc(sizeof(int));
  *(long*)res = i;
  return res;
}

long fromWord(WORD w) {
  return *(long*) w;
}

#endif


int main() {
#ifdef UNBOXED
  set_t s = set_new(unboxed_Hashable);
  set_t s2 = set_new(unboxed_Hashable);
#else
  set_t s = set_new(long_Hashable);
  set_t s2 = set_new(long_Hashable);
#endif  
  printf("set created\n");
  for (long i = 0; i < 1000; i++) {
    set_add(s,toWord(i*i));
  }
  set_discard(s,toWord(64));
  set_discard(s,toWord(225));
  set_discard(s,toWord(10000));
  printf("set size is %d\n",set_len(s));
  int n = 0;
  for (long k = 0; k < 1000; k++)
    if (set_contains(s,toWord(k))) n++;
  printf("#elements <1000 is %d\n",n);
  for (long i = 0; i < 500; i++) {
    set_add(s2,toWord(i*i*i*i));
  }
  printf("size of s is %d\n",set_len(s));
  printf("size of s2 is %d\n",set_len(s2));
  set_t s3 = set_intersection(s,s2);
  printf("size of intersection is %d\n",set_len(s3));
  iterator_t iter = set_iter(s3);
  printf("checking if intersection is lt both operands; returns %d and %d\n",set_lt(s3,s),set_lt(s3,s2));
  WORD res;
  while(!iterator_next(iter,&res)) 
    printf("%ld\n",fromWord(res));
  set_t s4 = set_union(s,s2);
  printf("size of union is %d\n",set_len(s4));
  printf("checking if union is gt both operands; returns %d and %d\n",set_gt(s4,s),set_gt(s4,s2));
  set_t s5 = set_symmetric_difference(s,s2);
  printf("size of symmetric difference is %d\n",set_len(s5));
  printf("popping and printing again elements in intersection\n");
  while(!set_pop(s3,&res))
    printf("popped %ld\n",fromWord(res));
  printf("size of intersection is now %d\n",set_len(s3));
   
        
}
            
