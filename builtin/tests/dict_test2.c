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

Iterable$opaque dict_iterable(Mapping$dict wit, $dict dict) {
  Iterable$__class__ cl = malloc(sizeof(struct  Iterable$__class__));
  cl->__iter__ = (Iterator (*)(Iterable, $WORD))wit->__class__->items;
  Iterable wit2 = malloc(sizeof(struct Iterable));
  wit2->__class__ = cl;
  return Iterable$__pack__(wit2,dict);
}
    
int main() {
  Mapping$dict wit = Mapping$dict_new((Hashable)Hashable$str_new());
  $dict dict = wit->__class__->__fromiter__(wit,NULL); 
  $dict other = wit->__class__->__fromiter__(wit,NULL);
  int j;

  for (long i=1; i < 1000000; i++)
    wit->_Indexed->__class__->__setitem__(wit->_Indexed,dict,toWord(i),toWord(i+1));
  
  $WORD b;
  long r = 17;
  long s = 0;
  for (int j=1; j < 100000; j++) {
    r = r*r % 100000;
    b = wit->_Indexed->__class__->__getitem__(wit->_Indexed,dict,toWord(r));
    s += fromWord(b);
  }
  printf("in dict_test after summation; last value retrieved should be %ld, was %ld\n",r+1,fromWord(b));
  printf("Retrieved and summed 100000 values; sum is %ld\n",s);
  
  long prefix[] = {3L};
  serialize_file((Serializable)dict,prefix,1,"test4.bin");

  printf("Wrote serialized dict to test4.bin\n");
/*
  long prefix2[10];
  int prefix2_size;
  $dict dict2 = ($dict)deserialize_file("test4.bin",prefix2,&prefix2_size);
  serialize_file((Serializable)dict2,prefix,1,"test5.bin");
  */
  int t1 = from$bool(wit->__class__->__contains__(wit,dict,toWord(678)));
  int t2 = from$bool(wit->__class__->__contains__(wit,dict,toWord(-1)));

  if (t1 && !t2)
    printf("contains test ok\n");
  else
    printf("contains test failed\n");
  
  $WORD res = NULL;
  for (long i=1; i<1000000; i++)
    if (i%100 > 0)
       wit->_Indexed->__class__->__delitem__( wit->_Indexed,dict,toWord(i));
  printf("Size of dict after popping is %ld\n",from$int(wit->__class__->__len__(wit,dict)));
  
  Iterator iter = wit->__class__->__iter__(wit,dict);
  long t = 0;
  while ((res=iter->__class__->__next__(iter))) {
    t += fromWord(res);
  }
  printf("Sum of remaining keys is %ld\n",t);
  
  $WORD deflt = toWord(666);
  $WORD w = wit->__class__->get(wit,dict,toWord(100),deflt);
  printf("dict_get on existing key 100; should return 101. Returned %ld\n",fromWord(w));
  $WORD w2 =  wit->__class__->get(wit,dict,toWord(37),deflt);
  printf("dict_get on non-existing key; should return default value 666. Returned %ld\n",fromWord(w2));

  for (long j = 11; j < 200; j+=20)
     wit->_Indexed->__class__->__setitem__( wit->_Indexed,other,toWord(j),toWord(2*j));

  Iterator items = wit->__class__->items(wit,dict);
  $WORD item;
  for (int k=0; k<10; k++) {
    if ((item = items->__class__->__next__(items))) {
      $str key = (($tup2_t)item)->a;
      $str val = (($tup2_t)item)->b;
      printf("item #%d is: key=%ld, value=%ld\n",k,fromWord(key),fromWord(val));
    }
  }

  wit->__class__->update(wit,dict,dict_iterable(wit,other));
  if((item = wit->__class__->popitem(wit,dict))) {
    printf("popitem gives: key=%ld, value=%ld\n",fromWord((($tup2_t)item)->a),fromWord((($tup2_t)item)->b));
  }
  if((item = wit->__class__->popitem(wit,dict))) {
     printf("popitem gives: key=%ld, value=%ld\n",fromWord((($tup2_t)item)->a),fromWord((($tup2_t)item)->b));
  }
  printf("size of dictionary should be 10007; is %ld\n",from$int(wit->__class__->__len__(wit,dict)));
}
