#pragma once

#include "iterator.h"
#include "list.h"
//#include "slice.h"
//#include "hash.h"
#include <stddef.h>

struct $str;
typedef struct $str *$str;

typedef struct str_internal_t {
  int nbytes;              // length of str in bytes
  int nchars;              // length of str in Unicode chars
  unsigned char *str;      // str is UTF-8 encoded.
} *str_internal_t;

struct $str$__methods__;
typedef struct $str$__methods__ *$str$__methods__;

struct $str {
  $str$__methods__ __class__;
  str_internal_t __internal__;
};

struct $str$__methods__ {
  $str (*capitalize)($str s);
  $str (*center)($str s, int width, $str fill);                 // raises TYPEERROR if fill is not a single char
  $int (*count)($str s, $str sub, $int start, $int end);
  //void encode($str s, bytes_t *res);                          // only utf-8 encoding and strict error handling
  $bool (*endswith)($str s, $str suffix, $int start, $int end);
  $str (*expandtabs)($str s, int tabsize);     
  $int (*find)($str s, $str sub, $int start, $int end);         // returns -1 when not found
  $int (*index)($str s, $str sub, $int start, $int end);        // like find but raises VALUEERROR when not found
  $bool (*isalnum)($str s);                                     // not exactly as in Python; all chars c satisfy isalpha(c) or isdecimal(c)
  $bool (*isalpha)($str s);
  $bool (*isascii)($str s);
  $bool (*isdecimal)($str s);
  //$bool (*isdigit)($str s);                                    // not implemented; relies on property NT(numeric_type)
  //$bool (*isidentifier)($str s);                               // not implemented
  $bool (*islower)($str s);
  //$bool (*isnumeric)($str s);                                  // not implemented; relies on property NT(numeric_type)
  $bool (*isprintable)($str s);
  $bool (*isspace)($str s);
  $bool (*istitle)($str s);
  $bool (*isupper)($str s);
  $str (*join)($str sep, Iterator iter);
  $str (*ljust)($str s, int width, $str fill);                   // raises TYPEERROR if fill is not a single char
  $str (*lower)($str s);
  $str (*lstrip)($str s,$str cs);                                // cs may be NULL, then defaulting to whitespace removal.
  //maketrans not implemented
  void (*partition)($str s, $str sep, $str *ls, $str *ssep, $str *rs);
  $str (*replace)($str s, $str old, $str new, $int count);
  $int (*rfind)($str s, $str sub, $int start, $int end);         // returns -1 when not found
  $int (*rindex)($str s, $str sub, $int start, $int end);        // like rfind but raises VALUEERROR when not found
  $str (*rjust)($str s, int width, $str fill);                   // raises TYPEERROR if fill is not a single char
  void (*rpartition)($str s, $str sep, $str *ls, $str *ssep, $str *rs); 
  //int (*rsplit)($str s, $str sep, int maxsplit, list_t *res);  // not implemented sep may be NULL; then separation is indicated by a whitespace string
  $str (*rstrip)($str s,$str cs);                                //  cs may be NULL, then defaulting to whitespace removal.
  $list (*split)($str s, $str sep, $int maxsplit);               // raises VALUEERROR when separator is empty string
  $list (*splitlines)($str s);                                   // keepends parameter absent; only \n recognized as line separator
  $bool (*startswith)($str s, $str prefix, $int start, $int end); 
  $str (*strip)($str s,$str cs);                                 // cs may be NULL, then defaulting to whitespace removal.
// translate not implemented
  $str (*upper)($str s);
  $str (*$str_zfill)($str s, int width);
};

// Constructor; str must be a null-terminated, correctly UTF-8-encoded string.
// The constructor checks this and returns a $str value.
$str fromUTF8(char *str);

unsigned char *toUTF8($str str);

 
// Protocol instances ////////////////////////////////////////////////////////////////////////////////////////

Eq$__class__ Eq$str_instance;
Hashable_Eq$__class__ Hashable_Eq$str_instance;
Plus$__class__ Plus$str_instance;
Collection$__class__ Collection$str_instance;
Iterable$__class__ Iterable$str_instance;
Indexed$__class__ Indexed$str_instance;
Sliceable$__class__ Sliceable$str_instance;
Sequence$__class__ Sequence$str_instance;
Container_Eq$__class__ Container_Eq$str_instance;

void str_instance_init();

