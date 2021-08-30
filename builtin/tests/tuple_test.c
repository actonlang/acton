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

#include <stdio.h>
#include "../builtin.h"

int main() {
  $tuple tup1 = $NEWTUPLE(3,to$int(7),to$str("A string"),to$float(3.14));
  $Sliceable$tuple wit = $Sliceable$tuple$witness;
  $print(2,to$str("tup1 = "),tup1);
  int start = 0;
  int stop = 3;
  int step = 2;
  struct $slice slc;
  slc.start = &start;
  slc.stop = &stop;
  slc.step = &step;
  $float pi =  ($float)wit->$class->__getitem__(wit,tup1,to$int(2));
  printf("pi = %f\n",from$float(pi));
  $tuple tup2 = wit->$class->__getslice__(wit,tup1,&slc);
  $print(2,to$str("tup2 = "),tup2);
  $Hashable wits[] = {($Hashable)$Hashable$int$witness, ($Hashable)$Hashable$str$witness, ($Hashable)$Hashable$float$witness};
  $Hashable wit2 = ($Hashable)$NEW($Hashable$tuple,3, ($Hashable*)&wits);
  $dict d = $NEW($dict,wit2,NULL,NULL);
  $dict_setitem(d,wit2,tup1,to$int(13));
  $int n =  ($int)$dict_getitem(d,wit2,tup1);
  printf("n=%ld\n",from$int(n));
}
