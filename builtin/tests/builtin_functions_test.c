#include "../builtin.h"
#include <stdio.h>

$bool $even($WORD n) {
  return to$bool((($int)n)->val%2==0);
}


int main() {
  $range r = $NEW($range,to$int(10),to$int(20),to$int(1));
  $Iterable wit = ($Iterable)$Iterable$range$witness;
  $Iterator it = wit->$class->__iter__(wit,r);
  $print($NEW($tuple,2,to$str("lst = "), $list_fromiter(it)));
  $print($NEW($tuple,2,to$str("enumerate(lst,0) = "), $list_fromiter($enumerate(wit,r,0))));
  $print($NEW($tuple,2,to$str("filter(even,lst) = "), $list_fromiter($filter(wit,$even,r))));
  $print($NEW($tuple,2,to$str("map(even,lst) = "), $list_fromiter($map(wit,($WORD(*)($WORD))$even,r))));
  $str chinese = to$str("但他呼吁进行全面调查");
  $print($NEW($tuple,3,chinese,to$str(" in ascii is "),$ascii(chinese)));
  $print($NEW($tuple,2,to$str("2^32 in hex is "),$hex(($Integral)$Integral$int$witness,to$int(65536L*65536L))));
  $print($NEW($tuple,2,to$str("chr(8707) is "), $chr(($Integral)$Integral$int$witness,to$int(8707))));
  $print($NEW($tuple,2,to$str("ord('∃') is "), $ord(to$str("∃"))));
}
