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
  B_tuple tup1 = $NEWTUPLE(3,to$int(7),to$str("A string"),to$float(3.14));
  B_SliceableD_tuple wit = B_SliceableD_tupleG_witness;
  B_print(2,to$str("tup1 = "),tup1);
  int start = 0;
  int stop = 3;
  int step = 2;
  struct B_slice slc;
  slc.start = &start;
  slc.stop = &stop;
  slc.step = &step;
  B_float pi =  (B_float)wit->$class->__getitem__(wit,tup1,to$int(2));
  printf("pi = %f\n",fromB_float(pi));
  B_tuple tup2 = wit->$class->__getslice__(wit,tup1,&slc);
  B_print(2,to$str("tup2 = "),tup2);
  B_Hashable wits[] = {(B_Hashable)B_HashableD_intG_witness, (B_Hashable)B_HashableD_strG_witness, (B_Hashable)B_HashableD_floatG_witness};
  B_Hashable wit2 = (B_Hashable)$NEW(B_HashableD_tuple,3, (B_Hashable*)&wits);
  B_dict d = $NEW(B_dict,wit2,NULL,NULL);
  B_dictD_setitem(d,wit2,tup1,to$int(13));
  B_int n =  (B_int)B_dictD_getitem(d,wit2,tup1);
  printf("n=%ld\n",from$int(n));
}
