#include "../builtin.h"

int main() {
  $register_builtin();
  $str str1 = from$UTF8("A plain ASCII string 123,.%");
  $str str2 = from$UTF8("Some non-ASCII öé");
  $str str3 = from$UTF8("And some very non-ASCII: 围绕疫情我们有过不和谐的声音");
  $list lst = $NEW($list,NULL);
  $list_append(lst,str1);
  $list_append(lst,str2);
  $list_append(lst,str3);
  $serialize_file(($Serializable)lst,"test3.bin");
  $list lst2 = ($list)$deserialize_file("test3.bin");
  printf("%s\n",(lst2->$class->__str__(lst2))->str);
}
