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
  $str prefix =  from$UTF8("<rpc-reply xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.1\" message-id=\"' + str(message_id) + '\">");
  $str suffix =  from$UTF8("</rpc-reply>");
  $str message = from$UTF8("message");
  $str text = wit1->$class->__add__(wit1,prefix,wit1->$class->__add__(wit1,message,suffix));
  $bool a = text->$class->startswith(text,prefix,NULL,NULL);
  if (a && text->$class->endswith(text,suffix,NULL,NULL)) {
    struct $Slice slc;
    $Container$str wit2 = $Container$str$witness;
    slc.start = slcel(wit2->$class->__len__(wit2,prefix));
    slc.stop =  slcel(neg(wit2->$class->__len__(wit2,suffix)));
    slc.step = NULL;
    $Sliceable$str wit3 = $Sliceable$str$witness;
    $str res = wit3->$class->__getslice__(wit3,text,&slc);
    printf("res is '%s'\n",to$UTF8(res));
    $str s = from$UTF8("/abc/def/g.xml/");
    $list lst = s->$class->split(s,from$UTF8("/"),NULL);
    printf("list has been split");
    $Sequence$list wit4 = $Sequence$list$witness;
    $int len = wit4->w$Collection$Sequence->$class->__len__(wit4->w$Collection$Sequence,lst);
    for (long i=0; i < from$int(len); i++) {
      $Sequence$list wit5 = $Sequence$list$witness;
      printf("  '%s'\n",to$UTF8(wit5->$class->__getitem__(wit5,lst,to$int(i))));
    }
  }
  $str chinese = from$UTF8("但他呼吁进行全面调查");
  printf("chinese nbytes = %d, nchars = %d\n",chinese->nbytes,chinese->nchars);
}
