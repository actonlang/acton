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


// gcc -g list_serialize_test.c -L../../deps/instdir/lib -L../../lib/dev -I../../deps/instdir/include -lbsdnt -lActon -lutf8proc

#include "../builtin.h"
#include "../list_impl.h"
#include <stdio.h>

#define TESTSIZE 30L
 
int main() {
  $register_builtin();
  B_list lst2 = $NEW(B_list,NULL,NULL);
  for (long i = 0L; i < TESTSIZE; i++) {
    if (i%2L != 0L) {
      B_listD_append(lst2,B_listD_getitem(lst2,i/2L));
    } else {
      B_list sublst = $NEW(B_list,NULL,NULL);
      for (long j=0L; j < i; j++)
        B_listD_append(sublst,toB_int(j));
      B_listD_append(lst2,sublst);
    }
  }
  $print(1,lst2);
  $ROW row = $serialize(($Serializable)lst2,NULL);
  //  $write_serialized(row,"test2.bin");
  B_list lst3 = (B_list)$deserialize(row, NULL);
  //  $ROW row2 = $read_serialized("test2.bin");
  // $write_serialized(row2,"test3.bin");
  // B_list lst0 = B_listD_getitem(lst3,4);
  $print(1,lst3);
}
