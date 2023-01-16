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
#include "../dict_impl.h"
#include <stdio.h>

int main() {
  $register_builtin();
  B_Hashable wit = (B_Hashable)B_HashableD_strG_witness;
  B_Iterable wit2 = (B_Iterable)B_IterableD_rangeG_witness;
  B_str a = to$str("a");
  B_str b = to$str("b");
  B_dict dict = $NEW(B_dict,wit,NULL,NULL);
  B_Iterator it = wit2->$class->__iter__(wit2,$NEW(B_range,toB_int(0),toB_int(10),toB_int(1)));
  B_list lst = B_listD_fromiter(it);
  printf("lst = %s\n",(lst->$class->__str__(lst))->str);
  B_dictD_setitem(dict,wit, a,lst);
  B_dictD_setitem(dict,wit, b,lst);
  $ROW r = $serialize(($Serializable)dict,NULL);
  B_dict dict2 = (B_dict)$deserialize(r,NULL);
  B_print(1,dict);
  B_print(1,dict2);
  B_listD_setitem(B_dictD_getitem(dict2,wit,a),1,toB_int(7));
  printf("Sharing test (both values should have 2nd element changed to 7):\ndict2 = %s\n",(dict2->$class->__str__(dict2))->str);
}
