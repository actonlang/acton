#include "../builtin.h"

int main() {
  $register_builtin();
  $Hashable wit = ($Hashable)$Hashable$str$witness;
  $str a = from$UTF8("a");
  $str b = from$UTF8("b");
  $dict dict = $NEW($dict,wit,NULL);
  $Iterable$opaque it = $Iterable$pack(($Iterable)$Iterable$range$witness,$NEW($range,to$int(0),to$int(10),to$int(1)));
  $list lst = $NEW($list,it);
  printf("lst = %s\n",(lst->$class->__str__(lst))->str);
  $dict_setitem(dict,wit, a,lst);
  $dict_setitem(dict,wit, b,lst);
  $serialize_file(($Serializable)dict,"test4.bin");
  $dict dict2 = ($dict)$deserialize_file("test4.bin");
  $serialize_file(($Serializable)dict2,"test5.bin");
  printf("dict2 = %s\n",(dict2->$class->__str__(dict2))->str);
  $list_setitem($dict_getitem(dict2,wit,a),1,to$int(7));
  printf("Sharing test (both values should have 2nd element changed to 7):\ndict2 = %s\n",(dict2->$class->__str__(dict2))->str);
}
