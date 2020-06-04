#include "../builtin.h"
#include "../list_impl.h"
#include <stdio.h>

#define TESTSIZE 30L
 
int main() {
  $register_builtin();
  $list lst2 = $NEW($list,NULL);
  for (long i = 0L; i < TESTSIZE; i++) {
    if (i%2L != 0L) {
      $list_append(lst2,$list_getitem(lst2,i/2L));
    } else {
      $list sublst = $NEW($list,NULL);
      for (long j=0L; j < i; j++)
        $list_append(sublst,to$int(j));
      $list_append(lst2,sublst);
    }
  }
  $ROW row = $serialize(($Serializable)lst2);
  $write_serialized(row,"test2.bin");
  $list lst3 = ($list)$deserialize(row);
  $ROW row2 = $read_serialized("test2.bin");
  $write_serialized(row2,"test3.bin");
  $list lst0 = $list_getitem(lst3,4);
  $print(tup1(lst3->$class->__str__(lst3)));
}
