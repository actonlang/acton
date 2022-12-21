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
  $Hashable wit = ($Hashable)$Hashable$str$witness;
  $Iterable wit2 = ($Iterable)$Iterable$range$witness;
  $str a = to$str("a");
  $str b = to$str("b");
  $dict dict = $NEW($dict,wit,NULL,NULL);
  $Iterator it = wit2->$class->__iter__(wit2,$NEW($range,to$int(0),to$int(10),to$int(1)));
  $list lst = $list_fromiter(it);
  printf("lst = %s\n",(lst->$class->__str__(lst))->str);
  $dict_setitem(dict,wit, a,lst);
  $dict_setitem(dict,wit, b,lst);
  $ROW r = $serialize(($Serializable)dict,NULL);
  $dict dict2 = ($dict)$deserialize(r,NULL);
  $print(1,dict);
  $print(1,dict2);
  $list_setitem($dict_getitem(dict2,wit,a),1,to$int(7));
  printf("Sharing test (both values should have 2nd element changed to 7):\ndict2 = %s\n",(dict2->$class->__str__(dict2))->str);
}
