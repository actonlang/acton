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

B_bool $even($WORD n) {
  return toB_bool(((B_int)n)->val%2==0);
}


int main() {
  B_range r = $NEW(B_range,toB_int(10),toB_int(20),toB_int(1));
  B_Iterable wit = (B_Iterable)B_IterableD_rangeG_witness;
  B_Iterator it = wit->$class->__iter__(wit,r);
  B_print(2,to$str("lst = "), B_listD_fromiter(it));
  B_print(2,to$str("enumerate(lst,0) = "), B_listD_fromiter(B_enumerate(wit,r,0)));
  B_print(2,to$str("filter(even,lst) = "), B_listD_fromiter(B_filter(wit,$even,r)));
  B_print(2,to$str("map(even,lst) = "), B_listD_fromiter(B_map(wit,($WORD(*)($WORD))$even,r)));
  B_str chinese = to$str("但他呼吁进行全面调查");
  B_print(3,chinese,to$str(" in ascii is "),$ascii(chinese));
  B_print(2,to$str("2^32 in hex is "),$hex((B_Integral)B_IntegralD_intG_witness,toB_int(65536L*65536L)));
  B_print(2,to$str("chr(8707) is "), $chr((B_Integral)B_IntegralD_intG_witness,toB_int(8707)));
  B_print(2,to$str("ord('∃') is "), $ord(to$str("∃")));
}
