#include "../builtin.h"

int main() {
  $register_builtin();
  $int a = to$int(17);
  $int b = to$int(36);
  $list lst = $list_fromiter(NULL);
  $list_append(lst,a);
  $list_append(lst,b);
  $Mapping$dict wit = $NEW($Mapping$dict,($Hashable)$Hashable$WORD$witness);
  $dict done = $NEW($dict,($Hashable)$Hashable$WORD$witness,NULL);
  struct $ROWLISTHEADER accum = {NULL,NULL};
  long start_no = 0;
  $step_serialize(($Serializable)a,wit,&start_no,done,&accum);
  $step_serialize(($Serializable)lst,wit,&start_no,done,&accum);
  $step_serialize(($Serializable)b,wit,&start_no,done,&accum);
  $step_serialize(($Serializable)lst,wit,&start_no,done,&accum);
  $write_serialized(accum.fst,"test7.bin");
  $ROW row = $read_serialized("test7.bin");
  done = $NEW($dict,($Hashable)$Hashable$WORD$witness,NULL);
  wit = $NEW($Mapping$dict,($Hashable)$Hashable$int$witness);
  $int a1 = ($int)$step_deserialize(wit,&row,done);
  $list lst1 = ($list)$step_deserialize(wit,&row,done);
  $int b1 = ($int)$step_deserialize(wit,&row,done);
  $list lst2 = ($list)$step_deserialize(wit,&row,done);
  printf("a1 = %ld\n",from$int(a1));
  printf("lst1="); $printlist(lst1);
  printf("b1 = %ld\n",from$int(b1));
  printf("lst2="); $printlist(lst2);
}
