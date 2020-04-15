#include <stdio.h>

#include "../builtin.h"

int main() {
  $set s = $set_fromiter((Hashable)Hashable$int_new(),NULL);
  for (long j = 0; j < 20; j++)
      $set_add(s,to$int(j*j));
  long prefix[10];
  int prefix_size;
  serialize_file((Serializable)s,prefix,0,"test6.bin");
  $set s1 = ($set)deserialize_file("test6.bin",prefix,&prefix_size);
  printf("size of s1 is %ld\n",$set_len(s1));
  long len1 = $set_len(s1);
  for (long i=0; i<len1; i++)
    printf("elem %ld is %ld\n",i,from$int($set_pop(s1)));
}
