#include "../builtin.h"
#include <stdio.h>

$bool $even($WORD n) {
  return to$bool((($int)n)->val%2==0);
}

$Iterable$opaque mkIterable($Iterator it) {
  return $Iterable$pack(($Iterable)$Iterable$Iterator$witness,it);
}


int main() {
  $Iterable$opaque iter = $Iterable$pack(($Iterable)$NEW($Iterable$range),$NEW($range,to$int(10),to$int(20),to$int(1)));
  $print(tup2(to$str("lst = "), $list_fromiter(iter)));
  $print(tup2(to$str("enumerate(lst,0) = "), $list_fromiter(mkIterable($enumerate(iter,0)))));
  $print(tup2(to$str("filter(even,lst) = "), $list_fromiter(mkIterable($filter($even,iter)))));
  $print(tup2(to$str("map(even,lst) = "), $list_fromiter(mkIterable($map(($WORD(*)($WORD))$even,iter)))));
  $str chinese = to$str("但他呼吁进行全面调查");
  $print(tup3(chinese,to$str(" in ascii is "),$ascii(chinese)));
  /* $printf("%s\n",from$str($ascii(to$str("Björn"))));*/
  /* for (int i=-9; i<100; i++) { */
  /*   $Integral$opaque n = $Integral$pack(( $Integral)$Integral$int$witness,to$int(i)); */
  /*   $printf("%s\n",$hex(n)->str); */
  /* } */
  $Integral$opaque n = $Integral$pack(( $Integral)$Integral$int$witness,to$int(65536L*65536L));
  $print(tup2(to$str("2^32 in hex is "),$hex(n)));
  $Integral$opaque existsnum = $Integral$pack(( $Integral)$Integral$int$witness,to$int(8707));
  $print(tup2(to$str("chr(8707) is "), $chr(existsnum)));
  $print(tup2(to$str("ord('∃') is "), $ord(to$str("∃"))));

}
