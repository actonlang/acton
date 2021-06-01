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

#include <stdio.h>
#include <stddef.h>
#include "../builtin.h"

$int neg($int n) {
  return to$int(-from$int(n));
}

int *slcel($int n) {
  int *res = malloc(sizeof(int));
  *res = from$int(n);
  return res;
}

int main() {
  $Plus$str wit1 = $Plus$str$witness;
  $str prefix =  to$str("<rpc-reply xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.1\" message-id=\"' + str(message_id) + '\">");
  $str suffix =  to$str("</rpc-reply>");
  $str message = to$str("message");
  $str text = wit1->$class->__add__(wit1,prefix,wit1->$class->__add__(wit1,message,suffix));
  $bool a = text->$class->startswith(text,prefix,NULL,NULL);
  if (a && text->$class->endswith(text,suffix,NULL,NULL)) {
    struct $slice slc;
    $Container$str wit2 = $Container$str$witness;
    slc.start = slcel(wit2->$class->__len__(wit2,prefix));
    slc.stop =  slcel(neg(wit2->$class->__len__(wit2,suffix)));
    slc.step = NULL;
    $Sliceable$str wit3 = $Sliceable$str$witness;
    $str res = wit3->$class->__getslice__(wit3,text,&slc);
    printf("res is '%s'\n",from$str(res));
    $str s = to$str("/abc/def/g.xml/");
    $list lst = s->$class->split(s,to$str("/"),NULL);
    printf("list has been split");
    $Sequence$list wit4 = $Sequence$list$witness;
    $int len = wit4->w$Collection->$class->__len__(wit4->w$Collection,lst);
    for (long i=0; i < from$int(len); i++) {
      printf("  '%s'\n",from$str(wit4->$class->__getitem__(wit4,lst,to$int(i))));
    }
    $str space = to$str(" ");
    $str joined = space->$class->join(space,($Iterable)wit4->w$Collection,lst);
    printf("joined string is '%s'\n",from$str(joined));
  }
  $str chinese = to$str("但他呼吁进行全面调查");
  printf("chinese nbytes = %d, nchars = %d\n",chinese->nbytes,chinese->nchars);
  $str s = to$str("firstSEPsecondSEPthörd");
  $print(1,s->$class->partition(s,to$str("SEP")));
  $print(1,s->$class->rpartition(s,to$str("SEP")));
  $print(1,s->$class->capitalize(s));
  $print(1,s->$class->center(s,to$int(50),NULL));
}
