#include <stdio.h>
#include <stddef.h>
#include "../builtin.h"

void RAISE(exception e) {
  fprintf(stderr,"exception raised\n");
  exit(1);
}
$int neg($int n) {
  $int res = malloc(sizeof(int));
  *res = -*n;
  return res;
}

$WORD toWord(long i) {
  $WORD res = malloc(sizeof(long));
  *(long*)res = i;
  return res;
}

int main() {
  str_instance_init();
  list_instance_init();
  $str prefix =  fromUTF8("<rpc-reply xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.1\" message-id=\"' + str(message_id) + '\">");
  $str suffix =  fromUTF8("</rpc-reply>");
  $str message = fromUTF8("message");
  $str text = ($str)Plus$str_instance->__add__(prefix,Plus$str_instance->__add__(message,suffix));
  $bool a = text->__class__->startswith(text,prefix,NULL,NULL);
  if (a && text->__class__->endswith(text,suffix,NULL,NULL)) {
    struct  Slice slc;
    slc.start = Collection$str_instance->__len__(Collection$__pack__(Collection$str_instance,prefix));
    slc.stop =  neg(Collection$str_instance->__len__(Collection$__pack__(Collection$str_instance,suffix)));
    slc.step = NULL;
    $str res = ($str)Sliceable$str_instance->__getslice__(Sliceable$__pack__(Sliceable$str_instance,text),&slc)->__impl__;
    printf("res is '%s'\n",toUTF8(res));
    $str s = fromUTF8("/abc/def/g.xml/");
    $list lst = s->__class__->split(s,fromUTF8("/"),NULL);
    $int len = Collection$list_instance->__len__(Collection$__pack__(Collection$list_instance,lst));
    for (long i=0; i < *len; i++)
      printf("  '%s'\n",toUTF8(($str)Indexed$list_instance->__getitem__(Indexed$__pack__(Indexed$list_instance,lst),toWord(i))));
  }
  $str chinese = fromUTF8("但他呼吁进行全面调查");
  printf("chinese nbytes = %d, nchars = %d\n",chinese->__internal__->nbytes,chinese->__internal__->nchars);
}
