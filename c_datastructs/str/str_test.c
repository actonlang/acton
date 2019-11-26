#include "str.h"
#include <stdio.h>

int main() {
  str_t b = fromUTF8("Björn é 8!");
  printf("%ld\n",str_len(b));
  str_t bu;
  str_upper(b,&bu);
  printf("%s\n",toUTF8(bu));
  str_t bl;
  str_lower(bu,&bl);
  printf("%s\n",toUTF8(bl));

  str_t text = fromUTF8("This is a test string for searching for substrings using the Boyer-Moore-Horspool algorithm");
  str_t pattern = fromUTF8("pool");
  str_t pattern2 = fromUTF8("Boyer-Moore-Horspool");
  str_t text2 = fromUTF8("Här är en lång sträng för att testa text som är  icke-ASCII.");
  str_t pattern3 = fromUTF8("ASCII"); 
  str_t text4 = fromUTF8("häröver");
  int n = str_find(text2,pattern3,10,str_len(text2));
  printf("n is %d\n",n);
  printf("sum is %s\n",toUTF8(str_add(text2,pattern2)));
  printf("this should be true: %d\n",str_contains(text2,fromUTF8("ö")));
  str_t s;
  str_getitem(text2,23,&s);
  str_t text3 = fromUTF8("a\u0308o");
  printf("This should print 'ö': %s\n",toUTF8(s));
  iterator_t iter = str_reversed(text4);
  WORD nxt;
  while (!iterator_next(iter,&nxt))
    printf("%s\n",toUTF8((str_t)nxt));
  // printf("\n");
  str_ljust(text2,100,fromUTF8("ö"),&s);
  printf("len(s) = %ld\n",str_len(s));
  printf("a%sb\n",toUTF8(s));
  struct slice_struct slc = {7,100,1,-1};
  str_getslice(text4,&slc,&s);
  printf("Here is the slice: \'%s\'\n",toUTF8(s));
  printf("test isalpha: string is '%s', result is %d\n","abCDEÅÄÖéä",str_isalpha(fromUTF8("abCDEÅÄÖéä")));
  printf("test isalpha: string is '%s', result is %d\n","a ",str_isalpha(fromUTF8("a ")));
  printf("test isalpha: string is '%s', result is %d\n","",str_isalpha(fromUTF8("")));
  printf("test isalpha: string is '%s', result is %d\n","abC5w",str_isalpha(fromUTF8("abC5w")));
  printf("test isascii: string is '%s', result is %d\n","abCDEÅÄÖéä",str_isascii(fromUTF8("abCDEÅÄÖéä")));
  printf("test isascii: string is '%s', result is %d\n","a ",str_isascii(fromUTF8("a ")));
  printf("test isascii: string is '%s', result is %d\n","",str_isascii(fromUTF8("")));
  printf("test isascii: string is '%s', result is %d\n","abC5w",str_isascii(fromUTF8("abCw")));
  printf("test islower: string is '%s', result is %d\n","abCDEÅÄÖéä",str_islower(fromUTF8("abCDEÅÄÖéä")));
  printf("test islower: string is '%s', result is %d\n","a ",str_islower(fromUTF8("a ")));
  printf("test islower: string is '%s', result is %d\n","3 +",str_islower(fromUTF8("3 +")));
  printf("test isspace: string is '%s', result is %d\n","abé5w",str_isspace(fromUTF8("abé5w")));
  printf("test istitle: string is '%s', result is %d\n","A Very Bad Title",str_istitle(fromUTF8("A Very Bad Title")));
  printf("test istitle: string is '%s', result is %d\n","A Very bad Title",str_istitle(fromUTF8("A Very bad Title")));
  printf("test istitle: string is '%s', result is %d\n","A Very Bad TiTle",str_istitle(fromUTF8("A Very Bad TiTle")));
  printf("test istitle: string is '%s', result is %d\n","a Very Bad Title",str_istitle(fromUTF8("a Very Bad Title")));
  printf("test istitle: string is '%s', result is %d\n","A Very Bad8Title",str_istitle(fromUTF8("A Very Bad8Title")));
  printf("test isalnum: string is '%s', result is %d\n","abCDEÅÄÖéä",str_isalnum(fromUTF8("abCDEÅÄÖéä")));
  printf("test isalnum: string is '%s', result is %d\n","a ",str_isalnum(fromUTF8("a ")));
  printf("test isalnum: string is '%s', result is %d\n","",str_isalnum(fromUTF8("")));
  printf("test isalnum: string is '%s', result is %d\n","abC5w",str_isalnum(fromUTF8("abC5w")));
  printf("test isprintable: string is '%s', result is %d\n","abC 5w",str_isprintable(fromUTF8("abC 5w")));
  printf("test isprintable: string is '%s', result is %d\n","abC\t5w",str_isprintable(fromUTF8("ab\tC5w")));
  printf("test isprintable: string is '%s', result is %d\n","abC\n5w",str_isprintable(fromUTF8("ab\nC5w")));
  printf("test isprintable: string is '%s', result is %d\n","abC\r5w",str_isprintable(fromUTF8("ab\rC5w")));
  str_t text5 = fromUTF8("abcdefghefij");
  str_t sub = fromUTF8("ef");
  str_t sub2 = fromUTF8("");
  printf("testing find. Should get 4, got %d\n",str_find(text5,sub,-11,100000));
  printf("testing find. Should get 4, got %d\n",str_find(text5,sub,-8,100000));
  printf("testing find. Should get 4, got %d\n",str_find(text5,sub,-10,-3));
  printf("testing find. Should get -1, got %d\n",str_find(text5,sub,-7,-3));
  printf("testing rfind. Should get 8, got %d\n",str_rfind(text5,sub,-11,100000));
  printf("testing rfind. Should get 8, got %d\n",str_rfind(text5,sub,-8,100000));
  printf("testing rfind. Should get 4, got %d\n",str_rfind(text5,sub,-10,-3));
  printf("testing rfind. Should get -1, got %d\n",str_rfind(text5,sub,-7,-3));
  printf("testing count. Should get 4, got %d\n",str_count(fromUTF8("efsefefssssef"),sub,0,10000));
  printf("testing count. Should get 7, got %d\n",str_count(fromUTF8("efsefe"),sub2,0,10000));
  printf("testing zfill. Should get '-00042', got '%s'\n",toUTF8(str_zfill(fromUTF8("-42"),6)));
  printf("testing zfill. Should get 000042', got '%s'\n",toUTF8(str_zfill(fromUTF8("42"),6)));
  str_t s1,t,u;
  str_partition(text2,fromUTF8("är"),&s1,&t,&u);
  printf("partition: '%s','%s','%s'\n",toUTF8(s1),toUTF8(t),toUTF8(u));
  str_rpartition(text2,fromUTF8("är"),&s1,&t,&u);
  printf("rpartition: '%s','%s','%s'\n",toUTF8(s1),toUTF8(t),toUTF8(u));
  str_capitalize(text5,&s1);
  printf("Capitalize: '%s'\n",toUTF8(s1));
  printf("endswith. Should return 1, returns %d\n",str_endswith(text2,pattern3,0,-1));
  printf("startswith. Should return 1, returns %d\n",str_startswith(fromUTF8("abcdefghijk"),fromUTF8("bcdefghij"),1,1000));
  str_lstrip(fromUTF8("www.example.com"),fromUTF8("wcmo."),&s);
  printf("lstrip: %s\n",toUTF8(s));
  str_rstrip(fromUTF8("www.example.com"),fromUTF8("wcmo."),&s);
  printf("rstrip: %s\n",toUTF8(s));
  str_strip(fromUTF8("www.example.com"),fromUTF8("wcmo."),&s);
  printf("strip: %s\n",toUTF8(s));
  list_t lst;
  //str_split(fromUTF8("df dg hn hn  f    hhn"),NULL,4,&lst);
  str_splitlines(fromUTF8("df\ndg\nhn\nhn  f    \nhhn  "),&lst);
  iterator_t iter2 = list_iter(lst);
  WORD w;
  for (int i = 0; i<list_len(lst); i++) {
     list_getitem(lst,i,&w);
     printf("elem %d is '%s'\n",i,toUTF8((str_t)w));
  }
  
  //while(!iterator_next(iter2,&r))
  //  printf("elem is '%s'\n",toUTF8((str_t)r));
  printf("joined string is '%s'\n",toUTF8(str_join(fromUTF8("<>"),iter2)));
  str_replace(text,fromUTF8("for"),fromUTF8("FOR"),100,&s);
  printf("Replaced string is '%s'\n",toUTF8(s));
  printf("equality test %d\n",str_eq(text,text));
  printf("equality test %d\n",str_eq(text2,fromUTF8("Här är en lång sträng för att testa text som är  icke-ASCII.")));
  printf("hash test %lu\n",str_hash(text));
  printf("hash test %lu\n",str_hash(text2));
  printf("hash test %lu\n",str_hash(text3));
  str_expandtabs(fromUTF8("abcd\tf\tklmnre\t\na\tbcd"),8,&s);
  printf("expandtabs 'abcd\tf\tklmnre\t\na\tbcd' gives '%s'\n",toUTF8(s));
}
