#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include "../builtin.h"


$WORD toWord(long i) {
  char *s;
  int n = asprintf(&s,"%lu",i);
  $str str = from$UTF8(s);
  return ($WORD)str;
}

long fromWord($WORD w) {
  unsigned char *str = to$UTF8(($str)w);
  long x;
  sscanf((char *)str,"%lu",&x);
  return x;
}

$Iterable$opaque dict_iterable($Mapping$dict wit, $dict dict) {
  $Iterable$class cl = malloc(sizeof(struct  $Iterable$class));
  cl->__iter__ = ($Iterator (*)($Iterable, $WORD))wit->class->items;
  $Iterable wit2 = malloc(sizeof(struct $Iterable));
  wit2->class = cl;
  return $Iterable$pack(wit2,dict);
}
    
int main() {
  $Mapping$dict wit = $Mapping$dict_new(($Hashable)$Hashable$str$witness);
  $dict dict = wit->class->__fromiter__(wit,NULL); 
  $dict other = wit->class->__fromiter__(wit,NULL);
  int j;

  for (long i=1; i < 1000000; i++)
    wit->_Indexed->class->__setitem__(wit->_Indexed,dict,toWord(i),toWord(i+1));
  
  $WORD b;
  long r = 17;
  long s = 0;
  for (int j=1; j < 100000; j++) {
    r = r*r % 100000;
    b = wit->_Indexed->class->__getitem__(wit->_Indexed,dict,toWord(r));
    s += fromWord(b);
  }
  printf("in dict_test after summation; last value retrieved should be %ld, was %ld\n",r+1,fromWord(b));
  printf("Retrieved and summed 100000 values; sum is %ld\n",s);
  
  long prefix[] = {3L};
/*
  $serialize_file(($Serializable)dict,prefix,1,"test4.bin");

 /printf("Wrote serialized dict to test4.bin\n");
  long prefix2[10];
  int prefix2_size;
  $dict dict2 = ($dict)$deserialize_file("test4.bin",prefix2,&prefix2_size);
  serialize_file(($Serializable)dict2,prefix,1,"test5.bin");
  */
  int t1 = from$bool(wit->class->__contains__(wit,dict,toWord(678)));
  int t2 = from$bool(wit->class->__contains__(wit,dict,toWord(-1)));

  if (t1 && !t2)
    printf("contains test ok\n");
  else
    printf("contains test failed\n");
  
  $WORD res = NULL;
  for (long i=1; i<1000000; i++)
    if (i%100 > 0)
       wit->_Indexed->class->__delitem__( wit->_Indexed,dict,toWord(i));
  printf("Size of dict after popping is %ld\n",from$int(wit->class->__len__(wit,dict)));
  
  $Iterator iter = wit->class->__iter__(wit,dict);
  long t = 0;
  while ((res=iter->class->__next__(iter))) {
    t += fromWord(res);
  }
  printf("Sum of remaining keys is %ld\n",t);
  
  $WORD deflt = toWord(666);
  $WORD w = wit->class->get(wit,dict,toWord(100),deflt);
  printf("dict_get on existing key 100; should return 101. Returned %ld\n",fromWord(w));
  $WORD w2 =  wit->class->get(wit,dict,toWord(37),deflt);
  printf("dict_get on non-existing key; should return default value 666. Returned %ld\n",fromWord(w2));

  for (long j = 11; j < 200; j+=20)
     wit->_Indexed->class->__setitem__( wit->_Indexed,other,toWord(j),toWord(2*j));

  $Iterator items = wit->class->items(wit,dict);
  $WORD item;
  for (int k=0; k<10; k++) {
    if ((item = items->class->__next__(items))) {
      $str key = (($tup2_t)item)->a;
      $str val = (($tup2_t)item)->b;
      printf("item #%d is: key=%ld, value=%ld\n",k,fromWord(key),fromWord(val));
    }
  }

  wit->class->update(wit,dict,dict_iterable(wit,other));
  if((item = wit->class->popitem(wit,dict))) {
    printf("popitem gives: key=%ld, value=%ld\n",fromWord((($tup2_t)item)->a),fromWord((($tup2_t)item)->b));
  }
  if((item = wit->class->popitem(wit,dict))) {
     printf("popitem gives: key=%ld, value=%ld\n",fromWord((($tup2_t)item)->a),fromWord((($tup2_t)item)->b));
  }
  printf("size of dictionary should be 10007; is %ld\n",from$int(wit->class->__len__(wit,dict)));
}
