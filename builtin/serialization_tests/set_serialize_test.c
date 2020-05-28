#include <stdio.h>

#include "../builtin.h"

int main() {
  $register_builtin();
  $Hashable wit = ($Hashable)$Hashable$int$witness;
  $set s = $NEW($set,wit,NULL);
  for (long j = 0; j < 100; j++)
    $set_add(s,wit,to$int(j*j));
  $serialize_file(($Serializable)s,"test6.bin");
  $set s1 = ($set)$deserialize_file("test6.bin");
  printf("size of s1 is %ld\n",$set_len(s1));
  long len1 = $set_len(s1);
  for (long i=0; i<len1; i++)
    printf("elem %ld is %ld\n",i,from$int($set_pop(s1)));
}
