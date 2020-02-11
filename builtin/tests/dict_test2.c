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
  $dict dict = $dict_new(Hashable$str_instance);
  $dict other = $dict_new(Hashable$str_instance);
  int j;
  //  for (long i=1; i < 1000000; i++) {
  //  Indexed$dict_instance->__setitem__(Indexed$dict_instance,dict,toWord(i),toWord(i+1));
  //}

  Indexed dict1 = Indexed$__pack__(Indexed$dict_instance,dict);
  for (long i=1; i < 1000000; i++)
    dict1->__class__->__setitem__(dict1->__class__,dict1->__impl__,toWord(i),toWord(i+1));
  $WORD b;
  long r = 17;
  long s = 0;
  for (int j=1; j < 100000; j++) {
    r = r*r % 1000000;
    b = Indexed$dict_instance->__getitem__(Indexed$dict_instance,dict,toWord(r));
    s += fromWord(b);
  }
  printf("in dict_test after summation; last value retrieved should be %ld, was %ld\n",r+1,fromWord(b));
  printf("Summed 100000 values; sum is %ld\n",s);
  int t1 = Container_Eq$dict_instance->__contains__(Container_Eq$dict_instance,dict,toWord(678));
  int t2 = Container_Eq$dict_instance->__contains__(Container_Eq$dict_instance,dict,toWord(-1));
  
  if (t1 && !t2)
    printf("contains test ok\n");
  else
    printf("contains test failed\n");
  $WORD res = NULL;
  for (long i=1; i<1000000; i++)
    if (i%100 > 0)
      Indexed$dict_instance->__delitem__(Indexed$dict_instance,dict,toWord(i));
  printf("Size of dict after popping is %ld\n",*Container_Eq$dict_instance->Collection$__methods__->__len__(Container_Eq$dict_instance->Collection$__methods__,dict));

  Iterator iter = Iterable$dict_instance->__iter__(Iterable$dict_instance,dict);
  long t = 0;
  while ((res=iter->__class__->__next__(iter->__class__,iter->__impl__))) {
    t += fromWord(res);
  }
  printf("Sum of remaining keys is %ld\n",t);

  $WORD deflt = toWord(666);
  $WORD w = Mapping$dict_instance->get(Mapping$dict_instance,dict,toWord(100),deflt);
  printf("dict_get on existing key 100; should return 101. Returned %ld\n",fromWord(w));
  $WORD w2 = Mapping$dict_instance->get(Mapping$dict_instance,dict,toWord(37),deflt);
  printf("dict_get on non-existing key; should return default value 666. Returned %ld\n",fromWord(deflt));

  for (long j = 11; j < 200; j+=20)
    Indexed$dict_instance->__setitem__(Indexed$dict_instance,other,toWord(j),toWord(2*j));
  Mapping$dict_instance->update(Mapping$dict_instance,dict,Mapping$__pack__(Mapping$dict_instance,other));
  Iterator items = Mapping$dict_instance->items(Mapping$dict_instance,dict);
  $WORD item;
  for (int k=0; k<10; k++) {
    if ((item = items->__class__->__next__(items->__class__,items->__impl__)))
      printf("item #%d is: key=%ld, value=%ld\n",k,fromWord((($item_t)item)->key),fromWord((($item_t)item)->value));
  }

  if((item = Mapping$dict_instance->popitem(Mapping$dict_instance,dict))) {
    printf("popitem gives: key=%ld, value=%ld\n",fromWord((($item_t)item)->key),fromWord((($item_t)item)->value));
  }
  if((item = Mapping$dict_instance->popitem(Mapping$dict_instance,dict))) {
    printf("popitem gives: key=%ld, value=%ld\n",fromWord((($item_t)item)->key),fromWord((($item_t)item)->value));
  }
  printf("size of dictionary should be 10007; is %ld\n",*Container_Eq$dict_instance->Collection$__methods__->__len__(Container_Eq$dict_instance->Collection$__methods__,dict));

}
