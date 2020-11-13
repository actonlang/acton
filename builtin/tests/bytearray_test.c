#include "../builtin.h"
#include <utf8proc.h>

int main() {
  $range r = $NEW($range,to$int(50),to$int(250),to$int(50));
  $list lst0 = $list_fromiter(($Iterator)$NEW($Iterator$range,r));
  $bytearray b = $NEW($bytearray,($struct)lst0);
  $print(1,b->$class->center(b,to$int(25),NULL));
  $range r2 = $NEW($range,to$int(65),to$int(91),NULL);
  $range r3 = $NEW($range,to$int(75),to$int(77),NULL);
  $list lst2 = $list_fromiter(($Iterator)$NEW($Iterator$range,r2));
  $list lst3 = $list_fromiter(($Iterator)$NEW($Iterator$range,r3));
  $bytearray b2 = $NEW($bytearray,($struct)lst2);
  $bytearray b3 = $NEW($bytearray,($struct)lst3);
  $Sequence wit = ($Sequence)$Sequence$bytearray$witness;
  wit->$class->__delitem__(wit,b3,to$int(0));
  wit->$class->__delitem__(wit,b3,to$int(-1));
  $int n = b2->$class->find(b2,b3,NULL,NULL);
  $print(5,b3,to$str(" occurs in "),b2,to$str(" at pos "),n);
  $bytearray b4 = b->$class->center(b,to$int(20),NULL);
  $print(1,b->$class->lstrip(b4,NULL));
  $range rsep = $NEW($range,to$int(70),to$int(72),to$int(5));
  $bytearray sep = $NEW($bytearray,($struct)rsep);
  $print(1,b2->$class->split(b2,sep,NULL));
  $str s = to$str("line 1\nline 2\r\n\nBjörn");
  $bytearray b5 = s->$class->encode(s);
  $print(1,$ascii(s));
  $print(1,b5);
  $print(1,b5->$class->splitlines(b5,NULL));
  $print(1,b5->$class->splitlines(b5,$True));
  $bytearray b6 = to$bytearray("abcdefgh");
  $list lst = $NEW($list,($Sequence)$Sequence$bytearray$witness,b6);
  $print(1,lst);
  int start = 1;
  int stop = 6;
  int step = 2;
  struct $Slice slc;
  slc.start = &start;
  slc.stop = &stop;
  slc.step = &step;
  $Sequence$bytearray$witness->$class->__delslice__($Sequence$bytearray$witness,b6,&slc);
  $print(1,b6);
  $Sequence$bytearray$witness->$class->append($Sequence$bytearray$witness,b6,to$int(65));
  $Sequence$bytearray$witness->$class->append($Sequence$bytearray$witness,b6,to$int(66));
  $Sequence$bytearray$witness->$class->append($Sequence$bytearray$witness,b6,to$int(67));
  $print(1,b6);
  for (int i=0; i<100000; i++)
    $Sequence$bytearray$witness->$class->append($Sequence$bytearray$witness,b6,to$int(65+i%26));
  start = 8;
  stop = 100000;
  for (int i = 26; i>1; i--) {
    step = i;
    $Sequence$bytearray$witness->$class->__delslice__($Sequence$bytearray$witness,b6,&slc);
  }
  $print(1,b6);
    
}
