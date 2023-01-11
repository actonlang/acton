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

#include "../builtin.h"
#include <utf8proc.h>

int main() {
  B_range r = B_rangeG_new(toB_int(50),toB_int(250),toB_int(50));
  B_list lst0 = B_listD_fromiter((B_Iterator)$NEW(B_IteratorB_range,r));
  B_bytearray b = B_bytearrayG_new((B_value)lst0);
  $print(1,b->$class->center(b,toB_int(25),NULL));
  B_range r2 = B_rangeG_new(toB_int(65),toB_int(91),NULL);
  B_range r3 = B_rangeG_new(toB_int(75),toB_int(77),NULL);
  B_list lst2 = B_listD_fromiter((B_Iterator)B_IteratorB_rangeG_new(r2));
  B_list lst3 = B_listD_fromiter((B_Iterator)B_IteratorB_rangeG_new(r3));
  B_bytearray b2 = $NEW(B_bytearray,(B_value)lst2);
  B_bytearray b3 = $NEW(B_bytearray,(B_value)lst3);
  B_Sequence wit = (B_Sequence)B_SequenceD_bytearrayG_witness;
  wit->$class->__delitem__(wit,b3,toB_int(0));
  wit->$class->__delitem__(wit,b3,toB_int(-1));
  B_int n = b2->$class->find(b2,b3,NULL,NULL);
  $print(5,b3,to$str(" occurs in "),b2,to$str(" at pos "),n);
  B_bytearray b4 = b->$class->center(b,toB_int(20),NULL);
  $print(1,b->$class->lstrip(b4,NULL));
  B_range rsep = $NEW(B_range,toB_int(70),toB_int(72),toB_int(5));
  B_bytearray sep = $NEW(B_bytearray,(B_value)rsep);
  $print(1,b2->$class->split(b2,sep,NULL));
  B_str s = to$str("line 1\nline 2\r\n\nBjörn");
  B_bytearray b5 = s->$class->encode(s);
  $print(1,$ascii(s));
  $print(1,b5);
  $print(1,b5->$class->splitlines(b5,NULL));
  $print(1,b5->$class->splitlines(b5,$True));
  B_bytearray b6 = toB_bytearray("abcdefgh");
  B_list lst = $NEW(B_list,(B_Sequence)B_SequenceD_bytearrayG_witness,b6);
  $print(1,lst);
  int start = 1;
  int stop = 6;
  int step = 2;
  struct B_slice slc;
  slc.start = &start;
  slc.stop = &stop;
  slc.step = &step;
  B_SequenceD_bytearrayG_witness->$class->__delslice__(B_SequenceD_bytearrayG_witness,b6,&slc);
  $print(1,b6);
  B_SequenceD_bytearrayG_witness->$class->append(B_SequenceD_bytearrayG_witness,b6,toB_int(65));
  B_SequenceD_bytearrayG_witness->$class->append(B_SequenceD_bytearrayG_witness,b6,toB_int(66));
  B_SequenceD_bytearrayG_witness->$class->append(B_SequenceD_bytearrayG_witness,b6,toB_int(67));
  $print(1,b6);
  for (int i=0; i<100000; i++)
    B_SequenceD_bytearrayG_witness->$class->append(B_SequenceD_bytearrayG_witness,b6,toB_int(65+i%26));
  start = 8;
  stop = 100000;
  for (int i = 26; i>1; i--) {
    step = i;
    B_SequenceD_bytearrayG_witness->$class->__delslice__(B_SequenceD_bytearrayG_witness,b6,&slc);
  }
  $print(1,b6);
    
}
