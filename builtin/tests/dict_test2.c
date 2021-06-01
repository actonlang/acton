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
  $str str = to$str(s);
  return ($WORD)str;
}

long fromWord($WORD w) {
  unsigned char *str = from$str(($str)w);
  long x;
  sscanf((char *)str,"%lu",&x);
  return x;
}

$Iterable dict_iterable($Mapping$dict wit) {
  $Iterable$class cl = malloc(sizeof(struct  $Iterable$class));
  cl->__iter__ = ($Iterator (*)($Iterable, $WORD))wit->$class->items;
  $Iterable wit2 = malloc(sizeof(struct $Iterable));
  wit2->$class = cl;
  return wit2;
}
    
int main() {
  //$register_builtin();
  $dict idict = $NEW($dict,($Hashable)$Hashable$int$witness,NULL,NULL);
  $dict_setitem(idict, ($Hashable)$Hashable$int$witness, to$int(-1), to$str("minus one"));
  $dict_setitem(idict, ($Hashable)$Hashable$int$witness, to$int(-2), to$str("minus two"));
  $str i1 = ($str)$dict_get(idict, ($Hashable)$Hashable$int$witness, to$int(-1), NULL);
  $str i2 = ($str)$dict_get(idict, ($Hashable)$Hashable$int$witness, to$int(-2), NULL);
  printf("value of -1: %s\n", i1->str);
  printf("value of -2: %s\n", i2->str);
        
  $Hashable hashwit = ($Hashable)$Hashable$str$witness;
  $Mapping$dict wit = $NEW($Mapping$dict,hashwit);
  $dict dict = $NEW($dict,hashwit,NULL,NULL); 
  $dict other = $NEW($dict,hashwit,NULL,NULL);
  int j;

  for (long i=1; i < 1000000; i++) {
    wit->w$Indexed->$class->__setitem__(wit->w$Indexed,dict,toWord(i),toWord(i+1));
  }
  $WORD b;
  long r = 17;
  long s = 0;
  for (int j=1; j < 100000; j++) {
    r = r*r % 100000;
    b = wit->w$Indexed->$class->__getitem__(wit->w$Indexed,dict,toWord(r));
    s += fromWord(b);
  }
  printf("in dict_test after summation; last value retrieved should be %ld, was %ld\n",r+1,fromWord(b));
  printf("Retrieved and summed 100000 values; sum is %ld\n",s);
  
  /*
  $serialize_file(($Serializable)dict,"test4.bin");

  printf("Wrote serialized dict to test4.bin\n");
  $dict dict2 = ($dict)$deserialize_file("test4.bin");
  $serialize_file(($Serializable)dict2,"test5.bin");
  */  
  int t1 = from$bool(wit->$class->__contains__(wit,dict,toWord(678)));
  int t2 = from$bool(wit->$class->__contains__(wit,dict,toWord(-1)));

  if (t1 && !t2)
    printf("contains test ok\n");
  else
    printf("contains test failed\n");
  
  $WORD res = NULL;
  for (long i=1; i<1000000; i++)
    if (i%100 > 0)
       wit->w$Indexed->$class->__delitem__( wit->w$Indexed,dict,toWord(i));
  printf("Size of dict after popping is %ld\n",from$int(wit->$class->__len__(wit,dict)));
  
  $Iterator iter = wit->$class->__iter__(wit,dict);
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
     wit->w$Indexed->$class->__setitem__( wit->w$Indexed,other,toWord(j),toWord(2*j));

  $Iterator items = wit->$class->items(wit,dict);
  $tuple item;
  for (int k=0; k<10; k++) {
    if ((item = ($tuple)items->$class->__next__(items))) {
      $str key = item->components[0];
      $str val = item->components[1];
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
  printf("size of dictionary should be 10007; is %ld\n",from$int(wit->$class->__len__(wit,dict)));
}
