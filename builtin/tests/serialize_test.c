#include "../builtin.h"
#include "../list_impl.h"
#include <stdio.h>
 
int main() {
  /*
  $list lst = $list_fromiter(NULL);
  for (long i = 3L; i < 300L; i += 10L)
    $list_append(lst,to$int(i));
  long prefix[] = {9L,7L,3L};
  $ROW row = serialize((Serializable)lst,prefix,3);
  write_serialized(row,"test.bin");
  long prefix2[5];
  int prefix2_size;
  $dict done = $new_dict((Hashable)Hashable$PREFIX_new());
  $list lst2 = ($list)deserialize(&row,prefix2,&prefix2_size, done);
  printlist(lst2); 
  
  $list lst = $list_fromiter(NULL);
  for (long i = 3L; i < 3000000L; i += 10L)
    $list_append(lst,to$int(i));
  long prefix[] = {9L,7L,3L};
  serialize_file((Serializable)lst,prefix,3,"test.bin");
  */
  $list lst2 = $list_fromiter(NULL);
  for (long i = 0L; i < 30L; i++) {
    if (i%2L != 0L) {
      $list_append(lst2,$list_getitem(lst2,i/2L));
    } else {
      $list sublst = $list_fromiter(NULL);
      for (long j=0L; j < i; j++)
        $list_append(sublst,to$int(j));
      $list_append(lst2,sublst);
    }
  }
  long prefix2[] = {5L};
  $ROW row = serialize((Serializable)lst2,prefix2,1);
  long prefix[5];
  int prefix_size;
  write_serialized(row,"test2.bin");
  $list lst3 = ($list)deserialize(row,prefix,&prefix_size);
  for (int i=0; i<$list_len(lst3); i++) {
    printf("sublist %d is ",i);
    printlist($list_getitem(lst3,i));
  }
  $list_setitem($list_getitem(lst3,2),1,to$int(7));
  for (int i=0; i<$list_len(lst3); i++) {
    printf("sublist %d is ",i);
    printlist($list_getitem(lst3,i));
  }
  printf("prefix is ");
  for (int i = 0; i < prefix_size; i++)
    printf("%ld,",prefix[i]);
  printf("\n");
  $ROW row2 = read_serialized("test2.bin");
  $list lst4 = ($list)deserialize(row2,prefix,&prefix_size);
  for (int i=0; i<$list_len(lst4); i++) {
    printf("sublist %d is ",i);
    printlist($list_getitem(lst4,i));
  }
}
