#include "str.h"
#include "list.h"
#include "common.h"
#include <stdio.h>
#include <stddef.h>

void RAISE(exception e) {
  fprintf(stderr,"exception raised\n");
  exit(1);
}
$int neg($int n) {
  $int res = malloc(sizeof(int));
  *res = -*n;
  return res;
}

int main() {
  $str prefix =  fromUTF8("<rpc-reply xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.1\" message-id=\"' + str(message_id) + '\">");
  $str suffix =  fromUTF8("</rpc-reply>");
  $str message = fromUTF8("message");
  $str text = $str_add(prefix,$str_add(message,suffix));
  $bool a = text->__class__->startswith(text,prefix,NULL,NULL);
  if (a && $str_endswith(text,suffix,NULL,NULL)) {
    struct  Slice slc;
    slc.start = $str_len(prefix);
    slc.stop = neg($str_len(suffix));
    slc.step = NULL;
    $str res = $str_getslice(text,&slc);
    printf("'%s'\n",toUTF8(res));
    $list lst = $str_split(fromUTF8("/abc/def/g.xml/"),fromUTF8("/"),NULL);
    for (int i=0; i < *$list_len(lst); i++)
      printf("  '%s'\n",toUTF8(($str)$list_getitem(lst,i)));
  }
}
