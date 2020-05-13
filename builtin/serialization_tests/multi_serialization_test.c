#include "../builtin.h"

int main() {
  $register_builtin();
  $int a = to$int(17);
  $int b = to$int(36);
  $list lst = $list_fromiter(NULL);
  $list_append(lst,a);
  $list_append(lst,b);
  $Serial$state state = malloc(sizeof(struct $Serial$state));
  state->done = $NEW($dict,($Hashable)$Hashable$WORD$witness,NULL);
  state->row_no = 0;
  state->row = NULL;
  state->fst = NULL;
  $step_serialize(($Serializable)a,state);
  $step_serialize(($Serializable)lst,state);
  $step_serialize(($Serializable)b,state);
  $step_serialize(($Serializable)lst,state);
  $write_serialized(state->fst,"test7.bin");
  $ROW row = $read_serialized("test7.bin");
  state->done = $NEW($dict,($Hashable)$Hashable$int$witness,NULL);
  state->row_no = 0;
  state->row = row;
  $int a1 = ($int)$step_deserialize(state);
  $list lst1 = ($list)$step_deserialize(state);
  $int b1 = ($int)$step_deserialize(state);
  $list lst2 = ($list)$step_deserialize(state);
  printf("a1 = %ld\n",from$int(a1));
  printf("lst1="); $printlist(lst1);
  printf("b1 = %ld\n",from$int(b1));
  printf("lst2="); $printlist(lst2);
}
