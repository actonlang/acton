#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

#include "dict.h"
#include "iterator.h"
#include "hash.h"

WORD mkWord(long i) {
  WORD res = malloc(sizeof(int));
  *(long*)res = i;
  return res;
}

int main() {
  dict_t dict = dict_new(long_Hashable);
  int j;
  for (long i=1; i < 1000000; i++) {
    dict_setitem(dict,mkWord(i),mkWord(i+1));
  }
  WORD b;
  long r = 17;
  long s = 0;
  for (int j=1; j < 100000; j++) {
    r = r*r % 1000000;
    dict_getitem(dict,mkWord(r),&b);
    s += *(long*)b;       
  }
  printf("summed 100000 values; sum is %ld\n",s);
  iterator_t iter = dict_iter(dict);
  WORD res;
  int t1 = dict_contains(dict,mkWord(678));
  int t2 = dict_contains(dict,mkWord(-1));
  
  if (t1 && !t2)
    printf("contains test ok\n");
  else
    printf("contains test failed\n");
  res = NULL;
  for (long i=1; i<1000000; i++)
    if (i%100 > 0)
      dict_pop(dict,mkWord(i),&res);
  printf("Size of dict after popping is %ld\n",dict->numelements);

  long t = 0;
  while (!iterator_next(iter,&res)) 
    t += *(long*)res;
   printf("Sum of remaining keys is  %ld\n",t);
}
