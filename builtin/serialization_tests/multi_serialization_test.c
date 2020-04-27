#include "../builtin.h"

int main() {
  $init_serialization();
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
  long prefix[] = {0};
  a->$class->__serialize__(a,wit,($WORD*)prefix,1,done,accum);
  prefix[0] = 1;
  lst->$class->__serialize__(lst,wit,($WORD*)prefix,1,done,accum);
  prefix[0] = 2;
  b->$class->__serialize__(b,wit,($WORD*)prefix,1,done,accum);
  prefix[0] = 3;
  lst->$class->__serialize__(lst,wit,($WORD*)prefix,1,done,accum);
  $write_serialized(accum->fst,"test7.bin");
  $ROW row = $read_serialized("test7.bin");
  done = $new_dict();
  wit = $Mapping$dict_new(($Hashable)$Hashable$PREFIX$witness);
  int prefix_size;
  $int a1 = a->$class->__deserialize__(wit,&row,done);
  printf("a1 = %ld\n",from$int(a1));
  $list lst1 = lst->$class->__deserialize__(wit,&row,done);
  printf("lst1="); $printlist(lst1);
  $int b1 = a->$class->__deserialize__(wit,&row,done);
  printf("b1 = %ld\n",from$int(b1));
  $list lst2 = lst->$class->__deserialize__(wit,&row,done);
  printf("lst2="); $printlist(lst2);
}
