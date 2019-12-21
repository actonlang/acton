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
  $str (*center)($str s, int width, $str fill);          // raises TYPEERROR if fill is not a single char
  $int (*count)($str s, $str sub, $int start, $int end);
  //void encode($str s, bytes_t *res);                   // only utf-8 encoding and strict error handling
  $bool (*endswith)($str s, $str suffix, $int start, $int end);
  $str (*expandtabs)($str s, int tabsize);     
  $int (*find)($str s, $str sub, $int start, $int end);        // returns -1 when not found
  $int (*index)($str s, $str sub, $int start, $int end);       // like find but raises VALUEERROR when not found
  $bool (*isalnum)($str s);                                 // not exactly as in Python; all chars c satisfy isalpha(c) or isdecimal(c)
  $bool (*isalpha)($str s);
  $bool (*isascii)($str s);
  $bool (*isdecimal)($str s);
  //$bool (*isdigit)($str s);                               // not implemented; relies on property NT(numeric_type)
  //$bool (*isidentifier)($str s);                          // not implemented
  $bool (*islower)($str s);
  //$bool (*isnumeric)($str s);                             // not implemented; relies on property NT(numeric_type)
  $bool (*isprintable)($str s);
  $bool (*isspace)($str s);
  $bool (*istitle)($str s);
  $bool (*isupper)($str s);
  $str (*join)($str sep, Iterator iter);
  $str (*ljust)($str s, int width, $str fill);              // raises TYPEERROR if fill is not a single char
  $str (*lower)($str s);
  $str (*lstrip)($str s,$str cs);                           // cs may be NULL, then defaulting to whitespace removal.
  //maketrans not implemented
  void (*partition)($str s, $str sep, $str *ls, $str *ssep, $str *rs);
  $str (*replace)($str s, $str old, $str new, $int count);
  $int (*rfind)($str s, $str sub, $int start, $int end);         // returns -1 when not found
  $int (*rindex)($str s, $str sub, $int start, $int end);        // like rfind but raises VALUEERROR when not found
  $str (*rjust)($str s, int width, $str fill);      // raises TYPEERROR if fill is not a single char
  void (*rpartition)($str s, $str sep, $str *ls, $str *ssep, $str *rs); 
  //int (*rsplit)($str s, $str sep, int maxsplit, list_t *res); // not implemented sep may be NULL; then separation is indicated by a whitespace string
  $str (*rstrip)($str s,$str cs);                            //  cs may be NULL, then defaulting to whitespace removal.
  $list (*split)($str s, $str sep, $int maxsplit);             // raises VALUEERROR when separator is empty string
  $list (*splitlines)($str s);                               // keepends parameter absent; only \n recognized as line separator
  $bool (*startswith)($str s, $str prefix, $int start, $int end); 
  $str (*strip)($str s,$str cs);                             // cs may be NULL, then defaulting to whitespace removal.
// translate not implenented
  $str (*upper)($str s);
  $str (*$str_zfill)($str s, int width);
};

//struct bytes_struct;
//typedef struct bytes_struct *bytes_t;

// Constructor; str must be a null-terminated, correctly UTF-8-encoded string.
// The constructor checks this and returns a $str vsalue.
$str fromUTF8(char *str);

unsigned char *toUTF8($str str);

$str $str_add($str s, $str t);

$int $str_len($str s);

Iterator $str_iter($str s);
Iterator $str_reversed($str s);

$bool $str_contains($str s, $str sub);

$str $str_getitem($str s, int i);                            // raises INDEXERROR if index out of range
$str $str_getslice($str s, Slice slc);                       // raises VALUEERROR if slice step is 0.

$str $str_capitalize($str s);
$str $str_center($str s, int width, $str fill);             // raises TYPEERROR if fill is not a single char
$int $str_count($str s, $str sub, $int start, $int end);
//void $str_encode($str s, bytes_t *res);                      // only utf-8 encoding and strict error handling
$bool $str_endswith($str s, $str suffix, $int start, $int end);
$str $str_expandtabs($str s, int tabsize);      
$int $str_find($str s, $str sub, $int start, $int end);        // returns -1 when not found
//format and format_map will be replace by other methods
$int $str_index($str s, $str sub, $int start, $int end);        // like find but returns VALUEERROR when not found
$bool $str_isalnum($str s);                                     // not exactly as in Python; all chars c satisfy $str_isalpha(c) or $str_isdecimal(c)
$bool $str_isalpha($str s);
$bool $str_isascii($str s);
$bool $str_isdecimal($str s);
$bool $str_isdigit($str s);                                      // not implemented; relies on property NT(numeric_type)
$bool $str_isidentifier($str s);                                 // not implemented
$bool $str_islower($str s);
$bool $str_isnumeric($str s);                                     // not implemented; relies on property NT(numeric_type)
$bool $str_isprintable($str s);
$bool $str_isspace($str s);
$bool $str_istitle($str s);
$bool $str_isupper($str s);
$str $str_join($str sep, Iterator iter);
$str $str_ljust($str s, int width, $str fill);                 // raises TYPEERROR if fill is not a single char
$str $str_lower($str s);
$str $str_lstrip($str s,$str cs);                               // cs may be NULL, then defaulting to whitespace removal.
//maketrans not implemented
void $str_partition($str s, $str sep, $str *ls, $str *ssep, $str *rs);
$str $str_replace($str s, $str old, $str new, $int count);
$int $str_rfind($str s, $str sub, $int start, $int end);           // returns -1 when not found
$int $str_rindex($str s, $str sub, $int start, $int end);          // like rfind but returns VALUEERROR when not found
$str $str_rjust($str s, int width, $str fill);                     // raises TYPEERROR if fill is not a single char
void $str_rpartition($str s, $str sep, $str *ls, $str *ssep, $str *rs); 
//$list $str_rsplit($str s, $str sep, int maxsplit);    // NOT IMPLEMENTED sep may be NULL; then separation is indicated by a whitespace string
$str $str_rstrip($str s,$str cs);                              //  cs may be NULL, then defaulting to whitespace removal.
$list $str_split($str s, $str sep, $int maxsplit);                // raises VALUEERROR when separator is empty string
$list $str_splitlines($str s);                                   // keepends parameter absent; only \n recognized as line separator
$bool $str_startswith($str s, $str prefix, int *start, int *end); 
$str $str_strip($str s,$str cs);                                  // cs may be NULL, then defaulting to whitespace removal.
// translate not implenented
$str $str_upper($str s);
$str $str_zfill($str s, int width);

int $str_eq($str a, $str b);
size_t $str_hash($str s);

//Hashable $str_Hashable;
