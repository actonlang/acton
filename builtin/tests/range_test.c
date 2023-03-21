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
#include <stdio.h>

int main() {
  $register_builtin();
  B_Iterable wit = (B_Iterable)B_IterableD_rangeG_new();
  B_range r1 = B_rangeG_new(to$int(2),to$int(10),to$int(3));
  B_Iterator i1 = wit->$class->__iter__(wit,r1);
  B_int n;
  while ((n = (B_int)i1->$class->__next__(i1)))
    printf("%ld ",from$int(n));
  printf("\n");
  B_range r2 = B_rangeG_new(to$int(50),to$int(10),to$int(-4));
  $serialize_file(($Serializable)r2,"range.bin");
  B_range r3 = (B_range)$deserialize_file("range.bin");
  B_list lst = B_listD_fromiter(wit->$class->__iter__(wit,r3));
  B_print(2,to$str("lst = "),lst);
  B_set s = B_set_fromiter((B_Hashable)B_HashableD_intG_witness,wit->$class->__iter__(wit,r2));
  B_SetD_set wit2 = B_SetD_setG_new((B_Hashable)B_HashableD_intG_witness);
  B_list lst2 = B_listD_fromiter(wit2->$class->__iter__(wit2,s));
  B_print(2,to$str("lst2 = "),lst2);
}
