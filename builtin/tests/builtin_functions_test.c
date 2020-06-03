#include "../builtin.h"
#include <stdio.h>

$bool $even($WORD n) {
  return to$bool((($int)n)->val%2==0);
}

$tuple tup2($WORD a, $WORD b) {
  $WORD comps[] = {a,b};
  return $NEW($tuple,2,comps);
}

$tuple tup3($WORD a, $WORD b, $WORD c) {
  $WORD comps[] = {a,b,c};
  return $NEW($tuple,3,comps);
}

$Iterable$opaque mkIterable($Iterator it) {
  return $Iterable$pack(($Iterable)$Iterable$Iterator$witness,it);
}


int main() {
  $Iterable$opaque iter = $Iterable$pack(($Iterable)$NEW($Iterable$range),$NEW($range,to$int(10),to$int(20),to$int(1)));
  print(tup2(from$UTF8("lst = "), $NEW($list,iter)));
  print(tup2(from$UTF8("enumerate(lst,0) = "), $NEW($list,mkIterable($enumerate(iter,0)))));
  print(tup2(from$UTF8("filter(even,lst) = "), $NEW($list,mkIterable($filter($even,iter)))));
  print(tup2(from$UTF8("map(even,lst) = "), $NEW($list,mkIterable($map(($WORD(*)($WORD))$even,iter)))));
  $str chinese = from$UTF8("但他呼吁进行全面调查");
  print(tup3(chinese,from$UTF8(" in ascii is "),$ascii(chinese)));
  /* printf("%s\n",to$UTF8($ascii(from$UTF8("Björn"))));*/
  /* for (int i=-9; i<100; i++) { */
  /*   $Integral$opaque n = $Integral$pack(( $Integral)$Integral$int$witness,to$int(i)); */
  /*   printf("%s\n",$hex(n)->str); */
  /* } */
  $Integral$opaque n = $Integral$pack(( $Integral)$Integral$int$witness,to$int(65536L*65536L));
  print(tup2(from$UTF8("2^32 in hex is "),$hex(n)));
  $Integral$opaque existsnum = $Integral$pack(( $Integral)$Integral$int$witness,to$int(8707));
  print(tup2(from$UTF8("chr(8707) is "), $chr(existsnum)));
  print(tup2(from$UTF8("ord('∃') is "), $ord(from$UTF8("∃"))));

}
