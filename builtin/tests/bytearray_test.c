#include "../builtin.h"
#include <utf8proc.h>

int main() {
  $range r = $NEW($range,to$int(50),to$int(250),to$int(50));
  $bytearray b = $NEW($bytearray,$Sequence$pack(($Sequence)$Sequence$range$witness,r));
  $print($NEW($tuple,1,b->$class->center(b,to$int(20),NULL)));
  $range r2 = $NEW($range,to$int(65),to$int(91),NULL);
  $range r3 = $NEW($range,to$int(75),to$int(77),NULL);
  $bytearray b2 = $NEW($bytearray,$Sequence$pack(($Sequence)$Sequence$range$witness,r2));
  $bytearray b3 = $NEW($bytearray,$Sequence$pack(($Sequence)$Sequence$range$witness,r3));
  $int n = b2->$class->find(b2,b3,NULL,NULL);
  $print($NEW($tuple,5,b3,to$str(" occurs in "),b2,to$str(" at pos "),n));
  $bytearray b4 = b->$class->center(b,to$int(20),NULL);
  $print($NEW($tuple,1,b->$class->lstrip(b4,NULL)));
  $range rsep = $NEW($range,to$int(70),to$int(72),to$int(5));
  $bytearray sep = $NEW($bytearray,$Sequence$pack(($Sequence)$Sequence$range$witness,rsep));
  $print($NEW($tuple,1,b2->$class->split(b2,sep,NULL)));
  $str s = to$str("line 1\nline 2\r\n\nBjörn");
  $bytearray b5 = s->$class->encode(s);
  $print($NEW($tuple,1,$ascii(s)));
  $print($NEW($tuple,1,b5));
  $print($NEW($tuple,1,b5->$class->splitlines(b5,NULL)));
  $print($NEW($tuple,1,b5->$class->splitlines(b5,$True)));
}
