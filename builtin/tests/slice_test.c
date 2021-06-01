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
 
int main() {

  $slice slc = $slice$new(to$int(-1),to$int(0),to$int(-2));
  $Sequence$list wit = $Sequence$list$witness;
  $list lst = $list$new(NULL,NULL);
  for (long i=0; i<100; i++)
    wit->$class->append(wit,lst,to$int(i));
  $list lst2 = wit->$class->__getslice__(wit,lst,slc);
  $print(2,to$str("lst2 = "),lst2);
  $list lst3 =  $list$new(NULL,NULL);
  for (long i=100; i<110; i++)
    wit->$class->append(wit,lst3,to$int(i));
  $print(2,to$str("lst3 = "),lst3);
  slc = $slice$new(to$int(10),to$int(30),to$int(2));
  wit->$class->__setslice__(wit,($Iterable)wit->w$Collection,lst2,slc,lst3);
  $print(2,to$str("lst2 = "),lst2);
  $range r = $NEW($range,to$int(10000),NULL,NULL);
  $list lst4 = wit->w$Collection->$class->__fromiter__(wit->w$Collection,($Iterable)$Iterable$range$witness,r);

  $Iterator it = $Iterable$range$witness->$class->__iter__($Iterable$range$witness,$NEW($range,to$int(1000),to$int(1),to$int(-1)));
  $int i;
  while((i = ($int)it->$class->__next__(it))) {
  slc = $slice$new(to$int(0),to$int(10000),i);
    $list_delslice(lst4,slc);
  }
  $print(2,to$str("lst4 = "),lst4);
}
