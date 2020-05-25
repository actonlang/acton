#include "../builtin.h"
#include "../list_impl.h"
#include <stdio.h>

#define TESTSIZE 30L
 
int main() {
  $register_builtin();
  /*
  long prefix[] = {9L,7L,3L};
  $list lst = $list_fromiter(NULL);
  for (long i = 3L; i < 300L; i += 10L)
    $list_append(lst,to$int(i));
  long start_no = 0;
  $ROW row = $serialize(($Serializable)lst,&start_no);
  $write_serialized(row,"test.bin");
  $list lst2 = ($list)$deserialize_file("test.bin");
  $printlist(lst2);
  */
  $list lst2 = $NEW($list,NULL);
  for (long i = 0L; i < TESTSIZE; i++) {
    if (i%2L != 0L) {
      $list_append(lst2,$list_getitem(lst2,i/2L));
    } else {
      $list sublst = $NEW($list,NULL);
      for (long j=0L; j < i; j++)
        $list_append(sublst,to$int(j));
      $list_append(lst2,sublst);
    }
  }
  $ROW row = $serialize(($Serializable)lst2);
  $write_serialized(row,"test2.bin");
  $list lst3 = ($list)$deserialize(row);
  $ROW row2 = $read_serialized("test2.bin");
  $write_serialized(row2,"test3.bin");
  $list lst0 = $list_getitem(lst3,4);
  printf("%s\n",to$UTF8(lst3->$class->__str__(lst3)));
  
   /* for (int i=0; i<$list_len(lst3); i++) {  */
   /*   printf("sublist %d is ",i);  */
   /*   $printlist($list_getitem(lst3,i));  */
   /* }  */
  /* $list_setitem($list_getitem(lst3,2),1,to$int(7)); */
  /* for (int i=0; i<$list_len(lst3); i++) { */
  /*   printf("sublist %d is ",i); */
  /*   $printlist($list_getitem(lst3,i)); */
  /* } */
  /* $list lst4 = ($list)$deserialize(row2); */
  /*   for (int i=0; i<$list_len(lst4); i++) { */
  /*   printf("sublist %d is ",i); */
  /*   $printlist($list_getitem(lst4,i)); */
  /* } */
}

/*
Serialized file for TESTSIZE=6 in text notation. The list to be serialized is

[[],[],[0,1],[],[0,1,2,3],[0,1]] 

where all equal lists are shared, i.e. there are only three separate sublists, [] with three refs, [0,1] (two refs) and [0,1,2,3] (one ref).

Note that there are no newlines in serialized file, despite the presentation below.
Also, in each row the first three elements are stored as ints (4 bytes), the remaining as words (8 bytes)
 6 0 1 6         | the first 6 is class_id for type list; the second 6 is #elements in this list. Note that prefix is empty.
 6 1 1 0 0       | 1st element is a list(6); prefix is [0]; #elements is 0.
-6 1 1 1 0       | 2nd element is a shared list(-6); its prefix is [1] and it is the same as list with prefix [0]
 6 1 1 2 2       | 3rd element is a list; its prefix is [2] and #elements is 2.
 1 2 1 2 0 0     |    1st element of the sublist is an int; its prefix is [2,0] and its value is 0
 1 2 1 2 1 1     |    2nd element of sublist has prefix [2,1] and value 1.
-6 1 1 3 0       | 4th element is a shared list; prefix is [3] and it is the same as list with prefix [0].
 6 1 1 4 4       | 5th element is a list; prefix is [4] and #elements is 4.
 1 2 1 4 0 0     |    1st element of sublist is an int, has prefix [4,0] and value 0
 1 2 1 4 1 1     |    2nd element of sublist is an int, has prefix [4,1] and value 1
 1 2 1 4 2 2     |    3rd element of sublist is an int, has prefix [4,2] and value 2
 1 2 1 4 3 3     |    4th element of sublist is an int, has prefix [4,3] and value 3
-6 1 1 5 2       | 6th element is a shared list, with prefix [5] and is same as list with prefix [2].
*/

