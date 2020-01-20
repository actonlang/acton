#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include "../builtin.h"

 
$WORD toWord(long i) {
  char *s;
  int n = asprintf(&s,"%lu",i);
  $str str = fromUTF8(s);
  return ($WORD)str;
}

long fromWord($WORD w) {
  unsigned char *str = toUTF8(($str)w);
  long x;
  sscanf((char *)str,"%lu",&x);
  return x;
}

int main() {
  $dict dict = $dict_new(Eq_Hashable$str_instance);
  $dict other = $dict_new(Eq_Hashable$str_instance);
  int j;
  for (long i=1; i < 1000000; i++) {
    $dict_setitem(dict,toWord(i),toWord(i+1));
  }
  $WORD b;
  long r = 17;
  long s = 0;
  for (int j=1; j < 100000; j++) {
    r = r*r % 1000000;
    b = $dict_getitem(dict,toWord(r));
    s += fromWord(b);
  }
  printf("in dict_test after summation; last value retrieved should be %ld, was %ld\n",r+1,fromWord(b));
  printf("Summed 100000 values; sum is %ld\n",s);
  int t1 = $dict_contains(dict,toWord(678));
  int t2 = $dict_contains(dict,toWord(-1));
  
  if (t1 && !t2)
    printf("contains test ok\n");
  else
    printf("contains test failed\n");
  $WORD res = NULL;
  for (long i=1; i<1000000; i++)
    if (i%100 > 0)
       $dict_delitem(dict,toWord(i));
  //printf("Last popped is %ld\n",fromWord(res));
  printf("Size of dict after popping is %ld\n",*$dict_len(dict));

  Iterator iter = Iterable$dict_instance->__iter__(Iterable$dict_instance,dict);
  //iterator_internal_t iter = $dict_iter(dict);
  long t = 0;
  Iterator$__class__ cl = iter->__class__;
  dict_iterator_state_t state = iter->__impl__;
  while ((res=iter->__class__->__next__(iter->__class__,iter->__impl__))) {
    t += fromWord(res);
  }
  printf("Sum of remaining keys is %ld\n",t);

  $WORD deflt = toWord(666);
  $WORD w = $dict_get(dict,toWord(100),deflt);
  printf("dict_get on existing key 100; should return 101. Returned %ld\n",fromWord(w));
  $dict_get(dict,toWord(37),&deflt);
  printf("dict_get on non-existing key; should return default value 666. Returned %ld\n",fromWord(deflt));

  for (long j = 11; j < 200; j+=20)
    $dict_setitem(other,toWord(j),toWord(2*j));
  $dict_update(dict,other);
  Iterator items = Mapping$dict_instance->items(Mapping$dict_instance,dict);
  $WORD item;
  for (int k=0; k<10; k++) {
    if ((item = items->__class__->__next__(items->__class__,items->__impl__)))
      printf("item #%d is: key=%ld, value=%ld\n",k,fromWord((($item_t)item)->key),fromWord((($item_t)item)->value));
  }

  if((item = $dict_popitem(dict))) {
    printf("popitem gives: key=%ld, value=%ld\n",fromWord((($item_t)item)->key),fromWord((($item_t)item)->value));
  }
  if((item = $dict_popitem(dict))) {
    printf("popitem gives: key=%ld, value=%ld\n",fromWord((($item_t)item)->key),fromWord((($item_t)item)->value));
  }
  printf("size of dictionary should be 10007; is %ld\n",*$dict_len(dict));

}
