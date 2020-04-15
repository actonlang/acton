#include "../builtin.h"


$list range(int a, int b) {
  $list res = $list_fromiter(NULL);
  for (long i=a; i<b; i++)
    $list_append(res,to$int(i));
  return res;
}

int main() {
  $str a = fromUTF8("a");
  $str b = fromUTF8("b");
  $dict dict = $dict_fromiter((Hashable)Hashable$str_new(),NULL);
  $list lst = range(0,100);
  $dict_setitem(dict,a,lst);
  $dict_setitem(dict,b,lst);
  long prefix[] = {0L};
  serialize_file((Serializable)dict,prefix,1,"test4.bin");

  long prefix2[10];
  int prefix2_size;
  $dict dict2 = ($dict)deserialize_file("test4.bin",prefix2,&prefix2_size);
  serialize_file((Serializable)dict2,prefix,1,"test5.bin");
  printlist($dict_getitem(dict2,a));
  $list_setitem(lst,1,to$int(7));
  printlist($dict_getitem(dict2,b));
  $list_setitem($dict_getitem(dict2,a),1,to$int(7));
  printlist($dict_getitem(dict2,b));  
            
}
