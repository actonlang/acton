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
  $Iterable wit = ($Iterable)$Iterable$range$new();
  $range r1 = $range$new(to$int(2),to$int(10),to$int(3));
  $Iterator i1 = wit->$class->__iter__(wit,r1);
  $int n;
  while ((n = ($int)i1->$class->__next__(i1)))
    printf("%ld ",from$int(n));
  printf("\n");
  $range r2 = $range$new(to$int(50),to$int(10),to$int(-4));
  $serialize_file(($Serializable)r2,"range.bin");
  $range r3 = ($range)$deserialize_file("range.bin");
  $list lst = $list_fromiter(wit->$class->__iter__(wit,r3));
  $print(2,to$str("lst = "),lst);
  $set s = $set_fromiter(($Hashable)$Hashable$int$witness,wit->$class->__iter__(wit,r2));
  $Set$set wit2 = $Set$set$new(($Hashable)$Hashable$int$witness);
  $list lst2 = $list_fromiter(wit2->$class->__iter__(wit2,s));
  $print(2,to$str("lst2 = "),lst2);
}
