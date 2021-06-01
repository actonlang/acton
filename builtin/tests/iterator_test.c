/*
 * Copyright (C) 2019-2021 Data Ductus AB
 *
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

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
  $dict d = $NEW($dict,($Hashable)$Hashable$int$witness,NULL,NULL);
  $Mapping wit = ($Mapping)$NEW($Mapping$dict,($Hashable)$Hashable$int$witness);
  $Indexed wit2 = wit->w$Indexed;
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
  
