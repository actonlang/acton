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

$bool $even($WORD n) {
  return to$bool((($int)n)->val%2==0);
}


int main() {
  $range r = $NEW($range,to$int(10),to$int(20),to$int(1));
  $Iterable wit = ($Iterable)$Iterable$range$witness;
  $Iterator it = wit->$class->__iter__(wit,r);
  $print(2,to$str("lst = "), $list_fromiter(it));
  $print(2,to$str("enumerate(lst,0) = "), $list_fromiter($enumerate(wit,r,0)));
  $print(2,to$str("filter(even,lst) = "), $list_fromiter($filter(wit,$even,r)));
  $print(2,to$str("map(even,lst) = "), $list_fromiter($map(wit,($WORD(*)($WORD))$even,r)));
  $str chinese = to$str("但他呼吁进行全面调查");
  $print(3,chinese,to$str(" in ascii is "),$ascii(chinese));
  $print(2,to$str("2^32 in hex is "),$hex(($Integral)$Integral$int$witness,to$int(65536L*65536L)));
  $print(2,to$str("chr(8707) is "), $chr(($Integral)$Integral$int$witness,to$int(8707)));
  $print(2,to$str("ord('∃') is "), $ord(to$str("∃")));
}
