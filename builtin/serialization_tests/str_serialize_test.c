#include "../builtin.h"

int main() {
  $str str1 = fromUTF8("A plain ASCII string 123,.%");
  $str str2 = fromUTF8("Some non-ASCII öé");
  $str str3 = fromUTF8("And some very non-ASCII: 围绕疫情我们有过不和谐的声音");
  $list lst = $list_fromiter(NULL);
  $list_append(lst,str1);
  $list_append(lst,str2);
  $list_append(lst,str3);
  long prefix[] = {0L};
  serialize_file((Serializable)lst,prefix,1,"test3.bin");
  long prefix2[5];
  int prefix2_size;
  
  $list lst2 = ($list)deserialize_file("test3.bin",prefix2,&prefix2_size);
  for (long i = 0; i < $list_len(lst2); i++) 
    printf("%s\n",toUTF8($list_getitem(lst2,i)));
}
