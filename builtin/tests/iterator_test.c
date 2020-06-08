#include "../builtin.h"
#include <stdio.h>


$bool is_iterator($Super obj) {
  $Super$class c = obj->$class;
  while(c)
    if(c->$superclass == ($Super$class)&$Iterator$methods)
      return $True;
    else
      c = c->$superclass;
  return $False;
}
  
int main() {
  $register_builtin();
  $bool b1 = is_iterator(($Super)to$int(3));
  $dict d = $NEW($dict,($Hashable)$Hashable$int$witness,NULL);
  $Mapping$dict wit = $NEW($Mapping$dict,($Hashable)$Hashable$int$witness);
  $Indexed$dict wit2 = wit->w$Indexed$Mapping;
  wit2->$class->__setitem__(wit2,d,to$int(5),to$str("A string"));
  $Iterator it = wit->$class->__iter__(wit,d);
  $bool b2 = is_iterator(($Super)d);
  $bool b3 = is_iterator(($Super)it);
  printf("b1=%ld, b2=%ld, b3=%ld\n",from$bool(b1),from$bool(b2),from$bool(b3));
  $serialize_file(($Serializable)it,"iterator.bin");
  $Iterator it2 = ($Iterator)$deserialize_file("iterator.bin");
  $serialize_file(($Serializable)it2,"iterator2.bin");
  $int n = it2->$class->__next__(it2);
  printf("next is %ld\n",from$int(n));
}
  
