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
  Plus$str wit1 = Plus$str_new();
  $str prefix =  fromUTF8("<rpc-reply xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.1\" message-id=\"' + str(message_id) + '\">");
  $str suffix =  fromUTF8("</rpc-reply>");
  $str message = fromUTF8("message");
  $str text = wit1->__class__->__add__(wit1,prefix,wit1->__class__->__add__(wit1,message,suffix));
  $bool a = text->__class__->startswith(text,prefix,NULL,NULL);
  if (a && text->__class__->endswith(text,suffix,NULL,NULL)) {
    struct Slice slc;
    Container$str wit2 = Container$str_new();
    slc.start = slcel(wit2->__class__->__len__(wit2,prefix));
    slc.stop =  slcel(neg(wit2->__class__->__len__(wit2,suffix)));
    slc.step = NULL;
    Sliceable$str wit3 = Sliceable$str_new();
    $str res = wit3->__class__->__getslice__(wit3,text,&slc);
    printf("res is '%s'\n",toUTF8(res));
    $str s = fromUTF8("/abc/def/g.xml/");
    $list lst = s->__class__->split(s,fromUTF8("/"),NULL);
    printf("list has been split");
    Collection$list wit4 = Collection$list_new();
    $int len = wit4->__class__->__len__(wit4,lst);
    for (long i=0; i < from$int(len); i++) {
      Sequence$list wit5 =  Sequence$list_new();
      printf("  '%s'\n",toUTF8(wit5->__class__->__getitem__(wit5,lst,to$int(i))));
    }
  }
  $str chinese = fromUTF8("但他呼吁进行全面调查");
  printf("chinese nbytes = %d, nchars = %d\n",chinese->nbytes,chinese->nchars);
}
