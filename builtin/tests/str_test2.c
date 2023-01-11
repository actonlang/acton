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

B_int neg(B_int n) {
  return toB_int(-fromB_int(n));
}

int *slcel(B_int n) {
  int *res = malloc(sizeof(int));
  *res = fromB_int(n);
  return res;
}

int main() {
  $PlusB_str wit1 = $PlusB_strG_witness;
  B_str prefix =  to$str("<rpc-reply xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.1\" message-id=\"' + str(message_id) + '\">");
  B_str suffix =  to$str("</rpc-reply>");
  B_str message = to$str("message");
  B_str text = wit1->$class->__add__(wit1,prefix,wit1->$class->__add__(wit1,message,suffix));
  B_bool a = text->$class->startswith(text,prefix,NULL,NULL);
  if (a && text->$class->endswith(text,suffix,NULL,NULL)) {
    struct B_slice slc;
    B_ContainerD_str wit2 = B_ContainerD_strG_witness;
    slc.start = slcel(wit2->$class->__len__(wit2,prefix));
    slc.stop =  slcel(neg(wit2->$class->__len__(wit2,suffix)));
    slc.step = NULL;
    B_SliceableD_str wit3 = B_SliceableD_strG_witness;
    B_str res = wit3->$class->__getslice__(wit3,text,&slc);
    printf("res is '%s'\n",fromB_str(res));
    B_str s = to$str("/abc/def/g.xml/");
    B_list lst = s->$class->split(s,to$str("/"),NULL);
    printf("list has been split");
    B_SequenceD_list wit4 = B_SequenceD_listG_witness;
    B_int len = wit4->W_Collection->$class->__len__(wit4->W_Collection,lst);
    for (long i=0; i < fromB_int(len); i++) {
      printf("  '%s'\n",fromB_str(wit4->$class->__getitem__(wit4,lst,toB_int(i))));
    }
    B_str space = to$str(" ");
    B_str joined = space->$class->join(space,(B_Iterable)wit4->W_Collection,lst);
    printf("joined string is '%s'\n",fromB_str(joined));
  }
  B_str chinese = to$str("但他呼吁进行全面调查");
  printf("chinese nbytes = %d, nchars = %d\n",chinese->nbytes,chinese->nchars);
  B_str s = to$str("firstSEPsecondSEPthörd");
  $print(1,s->$class->partition(s,to$str("SEP")));
  $print(1,s->$class->rpartition(s,to$str("SEP")));
  $print(1,s->$class->capitalize(s));
  $print(1,s->$class->center(s,toB_int(50),NULL));
}
