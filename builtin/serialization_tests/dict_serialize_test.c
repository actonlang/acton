#include "../builtin.h"


$list range(int a, int b) {
  $register_builtin();
  $list res = $list_fromiter(NULL);
  for (long i=a; i<b; i++)
    $list_append(res,to$int(i));
  return res;
}

int main() {
  $Hashable wit = ($Hashable)$Hashable$str$witness;
  $str a = from$UTF8("a");
  $str b = from$UTF8("b");
  $dict dict = $dict_fromiter(wit,NULL);
  $list lst = range(0,50);
  $dict_setitem(dict,wit, a,lst);
  $dict_setitem(dict,wit, b,lst);
  $serialize_file(($Serializable)dict,"test4.bin");

  $dict dict2 = ($dict)$deserialize_file("test4.bin");
  $serialize_file(($Serializable)dict2,"test5.bin");
  $printlist($dict_getitem(dict2,wit, a));
  $list_setitem(lst,1,to$int(7));
  $printlist($dict_getitem(dict2,wit ,b));
  $list_setitem($dict_getitem(dict2,wit,a),1,to$int(7));
  $printlist($dict_getitem(dict2,wit,b));  
            
}
