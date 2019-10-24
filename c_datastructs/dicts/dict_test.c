#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

#include "dict.h"
#include "iterator.h"
#include "hash.h"

/* 
  If UNBOXED is defined, both keys and values are unboxed integers. 
  If not, they are boxed.
*/

#define UNBOXED

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
  dict_t dict = dict_new(unboxed_Hashable);
  dict_t other = dict_new(unboxed_Hashable);
#else
  dict_t dict = dict_new(long_Hashable);
  dict_t other = dict_new(long_Hashable);
#endif
  int j;
  for (long i=1; i < 1000000; i++) {
    dict_setitem(dict,toWord(i),toWord(i+1));
  }
  WORD b;
  long r = 17;
  long s = 0;
  for (int j=1; j < 100000; j++) {
    r = r*r % 1000000;
    dict_getitem(dict,toWord(r),&b);
    s += fromWord(b);
  }
  printf("in dict_test after summation; last value retrieved should be %ld, was %ld\n",r+1,fromWord(b));
  printf("Summed 100000 values; sum is %ld\n",s);
  int t1 = dict_contains(dict,toWord(678));
  int t2 = dict_contains(dict,toWord(-1));
  
  if (t1 && !t2)
    printf("contains test ok\n");
  else
    printf("contains test failed\n");
  WORD res = NULL;
  for (long i=1; i<1000000; i++)
    if (i%100 > 0)
      dict_pop(dict,toWord(i),&res);
  printf("Last popped is %ld\n",fromWord(res));
  printf("Size of dict after popping is %d\n",dict_len(dict));

  iterator_t iter = dict_iter(dict);
  long t = 0;
  while (!iterator_next(iter,&res))
    t += fromWord(res);
  printf("Sum of remaining keys is %ld\n",t);

  WORD deflt = toWord(666);
  dict_get(dict,toWord(100),&deflt);
  printf("dict_get on existing key 100; should return 101. Returned %ld\n",fromWord(deflt));
  deflt = toWord(666);
  dict_get(dict,toWord(37),&deflt);
  printf("dict_get on non-existing key; should return default value 666. Returned %ld\n",fromWord(deflt));

  for (long j = 11; j < 200; j+=20)
    dict_setitem(other,toWord(j),toWord(2*j));
  dict_update(dict,other);

  iterator_t items = dict_items(dict);
  WORD item;
  
  for (int k=0; k<10; k++) {
    if (!iterator_next(items,&item))
      printf("item #%d is: key=%ld, value=%ld\n",k,fromWord(((item_t)item)->key),fromWord(((item_t)item)->value));
  }

  if(!dict_popitem(dict,&item)) {
    printf("popitem gives: key=%ld, value=%ld\n",fromWord(((item_t)item)->key),fromWord(((item_t)item)->value));
  }
  if(!dict_popitem(dict,&item)) {
    printf("popitem gives: key=%ld, value=%ld\n",fromWord(((item_t)item)->key),fromWord(((item_t)item)->value));
  }
  printf("size of dictionary should be 10007; is %d\n",dict_len(dict));

}
