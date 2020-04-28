#include "../builtin.h"

int main() {
  $register_builtin();
  $int a = to$int(17);
  $int b = to$int(36);
  $list lst = $list_fromiter(NULL);
  $list_append(lst,a);
  $list_append(lst,b);
  $dict done = $new_dict();
  $Mapping$dict wit = $Mapping$dict_new(($Hashable)$Hashable$WORD$witness);
  $ROWLISTHEADER accum = malloc(sizeof(struct $ROWLISTHEADER));
  accum->fst = NULL;
  accum->last = NULL;
  long start_no = 0;
  a->$class->__serialize__(a,wit,&start_no,done,accum);
  lst->$class->__serialize__(lst,wit,&start_no,done,accum);
  b->$class->__serialize__(b,wit,&start_no,done,accum);
  lst->$class->__serialize__(lst,wit,&start_no,done,accum);
  $write_serialized(accum->fst,"test7.bin");
  $ROW row = $read_serialized("test7.bin");
  done = $new_dict();
  wit = $Mapping$dict_new(($Hashable)$Hashable$int$witness);
  $int a1 = a->$class->__deserialize__(wit,&row,done);
  $list lst1 = lst->$class->__deserialize__(wit,&row,done);
  $int b1 = a->$class->__deserialize__(wit,&row,done);
  $list lst2 = lst->$class->__deserialize__(wit,&row,done);
  printf("a1 = %ld\n",from$int(a1));
  printf("lst1="); $printlist(lst1);
  printf("b1 = %ld\n",from$int(b1));
  printf("lst2="); $printlist(lst2);
}
