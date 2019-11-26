#pragma once

#include "iterator.h"
#include "list.h"
#include "slice.h"
#include "hash.h"
#include <stddef.h>

struct str_struct;
struct bytes_struct;

typedef struct str_struct *str_t;

typedef struct bytes_struct *bytes_t;

// Constructor; str must be a null-terminated, correctly UTF-8-encoded string.
// The constructor checks this and returns a str_t vsalue.
str_t fromUTF8(char *str);

unsigned char *toUTF8(str_t str);

str_t str_add(str_t s, str_t t);

long str_len(str_t s);

iterator_t str_iter(str_t s);
iterator_t str_reversed(str_t s);

int str_contains(str_t s, str_t sub);

int str_getitem(str_t s, int i, str_t *res);                 // returns INDEXERROR if index out of range
int str_getslice(str_t s, slice_t slc, str_t *res);          // returns VALUEERROR if slice step is 0.

void str_capitalize(str_t s, str_t *res);
int str_center(str_t s, int width, str_t fill, str_t *res);  // returns TYPEERROR if fill is not a single char
int str_count(str_t s, str_t sub, int start, int end);
void str_encode(str_t s, bytes_t *res);                      // only utf-8 encoding and strict error handling
int str_endswith(str_t s, str_t suffix, int start, int end);
void str_expandtabs(str_t s, int tabsize, str_t *res);        // NOT IMPLEMENTED
int str_find(str_t s, str_t sub, int start, int end);        // returns -1 when not found
//format and format_map will be replace by other methods
int str_index(str_t s, str_t sub, int start, int end);        // like find but returns VALUEERROR when not found
int str_isalnum(str_t s);                                     // not exactly as in Python; all chars c satisfy str_isalpha(c) or str_isdecimal(c)
int str_isalpha(str_t s);
int str_isascii(str_t s);
int str_isdecimal(str_t s);
int str_isdigit(str_t s);                                      // not implemented; relies on property NT(numeric_type)
int str_isidentifier(str_t s);                                 // not implemented
int str_islower(str_t s);
int str_isnumeric(str_t s);                                     // not implemented; relies on property NT(numeric_type)
int str_isprintable(str_t s);
int str_isspace(str_t s);
int str_istitle(str_t s);
int str_isupper(str_t s);
str_t str_join(str_t sep, iterator_t iter);
int str_ljust(str_t s, int width, str_t fill, str_t *res);      // returns TYPEERROR if fill is not a single char
int str_lower(str_t s, str_t *res);
void str_lstrip(str_t s,str_t cs, str_t *res);                   // cs may be NULL, then defaulting to whitespace removal.
//maketrans not implemented
void str_partition(str_t s, str_t sep, str_t *ls, str_t *ssep, str_t *rs);
void str_replace(str_t s, str_t old, str_t new, int count, str_t *res);
int str_rfind(str_t s, str_t sub, int start, int end);           // returns -1 when not found
int str_rindex(str_t s, str_t sub, int start, int end);          // like rfind but returns VALUEERROR when not found
int str_rjust(str_t s, int width, str_t fill, str_t *res);       // returns TYPEERROR if fill is not a single char
void str_rpartition(str_t s, str_t sep, str_t *ls, str_t *ssep, str_t *rs); 
int str_rsplit(str_t s, str_t sep, int maxsplit, list_t *res);    // NOT IMPLEMENTED sep may be NULL; then separation is indicated by a whitespace string
void str_rstrip(str_t s,str_t cs, str_t *res);                     //  cs may be NULL, then defaulting to whitespace removal.
int str_split(str_t s, str_t sep, int maxsplit, list_t *res);    // returns VALUEERROR when separator is empty string
int str_splitlines(str_t s, list_t *res);                          // keepends parameter absent; only \n recognized as line separator
int str_startswith(str_t s, str_t prefix, int start, int end); 
void str_strip(str_t s,str_t cs, str_t *res);                     // cs may be NULL, then defaulting to whitespace removal.
// translate not implenented
int str_upper(str_t s, str_t *res);
str_t str_zfill(str_t s, int width);

int str_eq(str_t a, str_t b);
size_t str_hash(str_t s);

Hashable str_Hashable;
