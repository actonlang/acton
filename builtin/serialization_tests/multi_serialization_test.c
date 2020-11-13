#include "../builtin.h"

int main() {
  $register_builtin();
  $int a = to$int(17);
  $int b = to$int(36);
  $list lst = $NEW($list,NULL,NULL);
  $list_append(lst,a);
  $list_append(lst,b);
  // to serialize several objects, make a tuple.
  $serialize_file(($Serializable)$NEW($tuple,4,a,lst,b,lst),"test7.bin");
  $tuple t = ($tuple)$deserialize_file("test7.bin");
  $print(2,to$str("a1 = "),t->components[0]);
  $print(2,to$str("lst1="),t->components[1]);
  $print(2,to$str("b1 = "),t->components[2]);
  $print(2,to$str("lst2="),t->components[3]);
}
