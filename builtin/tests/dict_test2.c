/*
 * Copyright (C) 2019-2021 Data Ductus AB
 *
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include "../builtin.h"


$WORD toWord(long i) {
  char *s;
  int n = asprintf(&s,"%lu",i);
  B_str str = to$str(s);
  return ($WORD)str;
}

long fromWord($WORD w) {
  unsigned char *str = fromB_str((B_str)w);
  long x;
  sscanf((char *)str,"%lu",&x);
  return x;
}

B_Iterable dict_iterable(B_MappingD_dict wit) {
  B_IterableG_class cl = malloc(sizeof(struct  B_IterableG_class));
  cl->__iter__ = (B_Iterator (*)(B_Iterable, $WORD))wit->$class->items;
  B_Iterable wit2 = malloc(sizeof(struct B_Iterable));
  wit2->$class = cl;
  return wit2;
}
    
int main() {
  //$register_builtin();
  B_dict idict = $NEW(B_dict,(B_Hashable)B_HashableD_intG_witness,NULL,NULL);
  B_dictD_setitem(idict, (B_Hashable)B_HashableD_intG_witness, toB_int(-1), to$str("minus one"));
  B_dictD_setitem(idict, (B_Hashable)B_HashableD_intG_witness, toB_int(-2), to$str("minus two"));
  B_str i1 = (B_str)B_dictD_get(idict, (B_Hashable)B_HashableD_intG_witness, toB_int(-1), NULL);
  B_str i2 = (B_str)B_dictD_get(idict, (B_Hashable)B_HashableD_intG_witness, toB_int(-2), NULL);
  printf("value of -1: %s\n", i1->str);
  printf("value of -2: %s\n", i2->str);
        
  B_Hashable hashwit = (B_Hashable)B_HashableD_strG_witness;
  B_MappingD_dict wit = $NEW(B_MappingD_dict,hashwit);
  B_dict dict = $NEW(B_dict,hashwit,NULL,NULL); 
  B_dict other = $NEW(B_dict,hashwit,NULL,NULL);
  int j;

  for (long i=1; i < 1000000; i++) {
    wit->W_Indexed->$class->__setitem__(wit->W_Indexed,dict,toWord(i),toWord(i+1));
  }
  $WORD b;
  long r = 17;
  long s = 0;
  for (int j=1; j < 100000; j++) {
    r = r*r % 100000;
    b = wit->W_Indexed->$class->__getitem__(wit->W_Indexed,dict,toWord(r));
    s += fromWord(b);
  }
  printf("in dict_test after summation; last value retrieved should be %ld, was %ld\n",r+1,fromWord(b));
  printf("Retrieved and summed 100000 values; sum is %ld\n",s);
  
  /*
  $serialize_file(($Serializable)dict,"test4.bin");

  printf("Wrote serialized dict to test4.bin\n");
  B_dict dict2 = (B_dict)$deserialize_file("test4.bin");
  $serialize_file(($Serializable)dict2,"test5.bin");
  */  
  int t1 = fromB_bool(wit->$class->__contains__(wit,dict,toWord(678)));
  int t2 = fromB_bool(wit->$class->__contains__(wit,dict,toWord(-1)));

  if (t1 && !t2)
    printf("contains test ok\n");
  else
    printf("contains test failed\n");
  
  $WORD res = NULL;
  for (long i=1; i<1000000; i++)
    if (i%100 > 0)
       wit->W_Indexed->$class->__delitem__( wit->W_Indexed,dict,toWord(i));
  printf("Size of dict after popping is %ld\n",fromB_int(wit->$class->__len__(wit,dict)));
  
  B_Iterator iter = wit->$class->__iter__(wit,dict);
  long t = 0;
  while ((res=iter->$class->__next__(iter))) {
    t += fromWord(res);
  }
  printf("Sum of remaining keys is %ld\n",t);
  
  $WORD deflt = toWord(666);
  $WORD w = wit->$class->get(wit,dict,toWord(100),deflt);
  printf("dict_get on existing key 100; should return 101. Returned %ld\n",fromWord(w));
  $WORD w2 =  wit->$class->get(wit,dict,toWord(37),deflt);
  printf("dict_get on non-existing key; should return default value 666. Returned %ld\n",fromWord(w2));

  for (long j = 11; j < 200; j+=20)
     wit->W_Indexed->$class->__setitem__( wit->W_Indexed,other,toWord(j),toWord(2*j));

  B_Iterator items = wit->$class->items(wit,dict);
  B_tuple item;
  for (int k=0; k<10; k++) {
    if ((item = (B_tuple)items->$class->__next__(items))) {
      B_str key = item->components[0];
      B_str val = item->components[1];
      printf("item #%d is: key=%ld, value=%ld\n",k,fromWord(key),fromWord(val));
    }
  }

  wit->$class->update(wit,dict,dict_iterable(wit),other);
  if((item = wit->$class->popitem(wit,dict))) {
    printf("popitem gives: key=%ld, value=%ld\n",fromWord(item->components[0]),fromWord(item->components[1]));
  }
  if((item = wit->$class->popitem(wit,dict))) {
     printf("popitem gives: key=%ld, value=%ld\n",fromWord(item->components[0]),fromWord(item->components[1]));
  }
  printf("size of dictionary should be 10007; is %ld\n",fromB_int(wit->$class->__len__(wit,dict)));
}
