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

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <limits.h>
#include <ctype.h>
#include "utf8proc.h"

//  Method tables ///////////////////////////////////////////////////////////////

// General methods

void $str_init($str, $value);
$bool $str_bool($str);
$str $str_str($str);
void $str_serialize($str, $Serial$state);
$str $str_deserialize($str, $Serial$state);

// String-specific methods
$str $str_capitalize($str s);
$str $str_center($str s, $int width, $str fill);
$int $str_count($str s, $str sub, $int start, $int end);
$bytearray $str_encode($str s);
$bool $str_endswith($str s, $str suffix, $int start, $int end);
$str $str_expandtabs($str s, $int tabsize);
$int $str_find($str s, $str sub, $int start, $int end);
$int $str_index($str s, $str sub, $int start, $int end);
$bool $str_isalnum($str s);
$bool $str_isalpha($str s);
$bool $str_isascii($str s);
$bool $str_isdecimal($str s);
$bool $str_isdigit($str s);
$bool $str_isidentifier($str s);
$bool $str_islower($str s);
$bool $str_isnumeric($str s);
$bool $str_isprintable($str s);
$bool $str_isspace($str s);
$bool $str_istitle($str s);
$bool $str_isupper($str s);
$str $str_join($str sep, $Iterable wit, $WORD iter);
$str $str_ljust($str s, $int width, $str fill);
$str $str_lower($str s);
$str $str_lstrip($str s, $str cs);
$tuple $str_partition($str s, $str sep);
$str $str_replace($str s, $str old, $str new, $int count);
$int $str_rfind($str s, $str sub, $int start, $int end);
$int $str_rindex($str s, $str sub, $int start, $int end);
$str $str_rjust($str s, $int width, $str fill);
$tuple $str_rpartition($str s, $str sep);
$str $str_rstrip($str s, $str cs);
$list $str_split($str s, $str sep, $int maxsplit);
$list $str_splitlines($str s, $bool keepends);
$bool $str_startswith($str s, $str prefix, $int start, $int end);
$str $str_strip($str s, $str cs);
$str $str_upper($str s);
$str $str_zfill($str s, $int width);

struct $str$class $str$methods =
    {"$str", UNASSIGNED, ($Super$class)&$atom$methods, $str_init, $str_serialize, $str_deserialize, $str_bool, $str_str, $str_capitalize, $str_center, $str_count, $str_encode, $str_endswith,
     $str_expandtabs, $str_find, $str_index, $str_isalnum, $str_isalpha, $str_isascii, $str_isdecimal, $str_islower, $str_isprintable, $str_isspace,
     $str_istitle, $str_isupper, $str_join, $str_ljust, $str_lower, $str_lstrip, $str_partition, $str_replace, $str_rfind, $str_rindex, $str_rjust,
     $str_rpartition, $str_rstrip, $str_split, $str_splitlines, $str_startswith, $str_strip, $str_upper, $str_zfill};

// protocol methods; string implementation prototypes ///////////////////////////////////////////////////

int $str_eq($str, $str);
int $str_neq($str, $str);
int $str_lt($str, $str);
int $str_le($str, $str);
int $str_gt($str, $str);
int $str_ge($str, $str);

$Iterator $str_iter($str);

$str $str_fromiter($Iterable, $WORD);
$int $str_len($str str);

int $str_contains($str, $str);
int $str_containsnot($str, $str);

$str $str_getitem($str, int);
$str $str_getslice($str, $slice);

$str $str_add($str, $str);
$str $str_mul($str, $int);

// Protocol instances, using above prototypes

// Ord

void $Ord$str$__serialize__($Ord$str self, $Serial$state state) {
}

$Ord$str $Ord$str$__deserialize__($Ord$str self, $Serial$state state) {
    $Ord$str res = $DNEW($Ord$str, state);
    return res;
}

$Ord$str $Ord$str$new() {
    return $NEW($Ord$str);
}

$bool $Ord$str$__eq__($Ord$str wit, $str a, $str b) {
    return to$bool($str_eq(a, b));
}

$bool $Ord$str$__ne__($Ord$str wit, $str a, $str b) {
    return to$bool($str_neq(a, b));
}

$bool $Ord$str$__lt__($Ord$str wit, $str a, $str b) {
    return to$bool($str_lt(a, b));
}

$bool $Ord$str$__le__($Ord$str wit, $str a, $str b) {
    return to$bool($str_le(a, b));
}

$bool $Ord$str$__gt__($Ord$str wit, $str a, $str b) {
    return to$bool($str_gt(a, b));
}

$bool $Ord$str$__ge__($Ord$str wit, $str a, $str b) {
    return to$bool($str_ge(a, b));
}

// Container

void $Container$str$__serialize__($Container$str self, $Serial$state state) {
    $step_serialize(self->w$Eq$A$Container$str, state);
}

$Container$str $Container$str$__deserialize__($Container$str self, $Serial$state state) {
    $Container$str res = $DNEW($Container$str, state);
    res->w$Eq$A$Container$str = ($Eq)$step_deserialize(state);
    return res;
}

$Iterator $Container$str$__iter__($Container$str wit, $str str) {
    return $str_iter(str);
}

$str $Container$str$__fromiter__($Container$str wit, $Iterable wit2, $WORD iter) {
    return $str_join(to$str(""), wit2, iter);
}

$int $Container$str$__len__($Container$str wit, $str str) {
    return $str_len(str);
}

$bool $Container$str$__contains__($Container$str wit, $str str, $str sub) {
    return to$bool($str_contains(str, sub));
}

$bool $Container$str$__containsnot__($Container$str wit, $str str, $str sub) {
    return to$bool($str_containsnot(str, sub));
}

// Sliceable

void $Sliceable$str$__serialize__($Sliceable$str self, $Serial$state state) {
}

$Sliceable$str $Sliceable$str$__deserialize__($Sliceable$str self, $Serial$state state) {
    $Sliceable$str res = $DNEW($Sliceable$str, state);
    return res;
}

$Sliceable$str $Sliceable$str$new() {
    return $NEW($Sliceable$str);
}
$str $Sliceable$str$__getitem__($Sliceable$str wit, $str str, $int i) {
    return $str_getitem(str, from$int(i));
}

void $Sliceable$str$__setitem__($Sliceable$str wit, $str str, $int i, $str val) {
    fprintf(stderr, "Internal error: call to mutating method setitem on string");
    exit(-1);
}

void $Sliceable$str$__delitem__($Sliceable$str wit, $str str, $int i) {
    fprintf(stderr, "Internal error: call to mutating method delitem on string");
    exit(-1);
}

$str $Sliceable$str$__getslice__($Sliceable$str wit, $str str, $slice slc) {
    return $str_getslice(str, slc);
}

void $Sliceable$str$__setslice__($Sliceable$str wit, $str str, $Iterable wit2, $slice slc, $WORD iter) {
    fprintf(stderr, "Internal error: call to mutating method setslice on string");
    exit(-1);
}

void $Sliceable$str$__delslice__($Sliceable$str wit, $str str, $slice slc) {
    fprintf(stderr, "Internal error: call to mutating method delslice on string");
    exit(-1);
}

// Times

void $Times$str$__serialize__($Times$str self, $Serial$state state) {
}

$Times$str $Times$str$__deserialize__($Times$str self, $Serial$state state) {
    $Times$str res = $DNEW($Times$str, state);
    return res;
}

$str $Times$str$__add__($Times$str wit, $str a, $str b) {
    return $str_add(a, b);
}

$str $Times$str$__mul__($Times$str wit, $str a, $int n) {
    return $str_mul(a, n);
}

// Hashable

void $Hashable$str$__serialize__($Hashable$str self, $Serial$state state) {
}

$Hashable$str $Hashable$str$__deserialize__($Hashable$str self, $Serial$state state) {
    $Hashable$str res = $DNEW($Hashable$str, state);
    return res;
}

$bool $Hashable$str$__eq__($Hashable$str wit, $str a, $str b) {
    return to$bool($str_eq(a, b));
}

$Hashable$str $Hashable$str$new() {
    return $NEW($Hashable$str);
}
$bool $Hashable$str$__ne__($Hashable$str wit, $str a, $str b) {
    return to$bool($str_neq(a, b));
}

$int $Hashable$str$__hash__($Hashable$str wit, $str str) {
    return to$int($string_hash(str));
}

// Method tables for witness classes

struct $Ord$str$class $Ord$str$methods = {
    "$Ord$str",
    UNASSIGNED,
    ($Super$class)&$Ord$methods,
    (void (*)($Ord$str))$default__init__,
    $Ord$str$__serialize__,
    $Ord$str$__deserialize__,
    ($bool(*)($Ord$str))$default__bool__,
    ($str(*)($Ord$str))$default__str__,
    $Ord$str$__eq__,
    $Ord$str$__ne__,
    $Ord$str$__lt__,
    $Ord$str$__le__,
    $Ord$str$__gt__,
    $Ord$str$__ge__};
struct $Ord$str $Ord$str_instance = {&$Ord$str$methods};
$Ord$str $Ord$str$witness = &$Ord$str_instance;

struct $Container$str$class $Container$str$methods = {
    "$Container$str",
    UNASSIGNED,
    ($Super$class)&$Container$methods,
    $Container$str$__init__,
    $Container$str$__serialize__,
    $Container$str$__deserialize__,
    ($bool(*)($Container$str))$default__bool__,
    ($str(*)($Container$str))$default__str__,
    $Container$str$__iter__,
    NULL,
    $Container$str$__len__,
    $Container$str$__contains__,
    $Container$str$__containsnot__};
struct $Container$str $Container$str_instance = {&$Container$str$methods, ($Eq)&$Ord$str_instance};
$Container$str $Container$str$witness = &$Container$str_instance;

struct $Sliceable$str$class $Sliceable$str$methods = {
    "$Sliceable$str",
    UNASSIGNED,
    ($Super$class)&$Sliceable$methods,
    (void (*)($Sliceable$str))$default__init__,
    $Sliceable$str$__serialize__,
    $Sliceable$str$__deserialize__,
    ($bool(*)($Sliceable$str))$default__bool__,
    ($str(*)($Sliceable$str))$default__str__,
    $Sliceable$str$__getitem__,
    $Sliceable$str$__setitem__,
    $Sliceable$str$__delitem__,
    $Sliceable$str$__getslice__,
    $Sliceable$str$__setslice__,
    $Sliceable$str$__delslice__};
struct $Sliceable$str $Sliceable$str_instance = {&$Sliceable$str$methods};
$Sliceable$str $Sliceable$str$witness = &$Sliceable$str_instance;

struct $Times$str$class $Times$str$methods = {
    "$Times$str",
    UNASSIGNED,
    ($Super$class)&$Times$methods,
    (void (*)($Times$str))$default__init__,
    $Times$str$__serialize__,
    $Times$str$__deserialize__,
    ($bool(*)($Times$str))$default__bool__,
    ($str(*)($Times$str))$default__str__,
    $Times$str$__add__,
    ($str(*)($Times$str, $str, $str))$Plus$__iadd__,
    $Times$str$__mul__,
    ($str(*)($Times$str, $str, $int))$Times$__imul__,

};
struct $Times$str $Times$str_instance = {&$Times$str$methods};
$Times$str $Times$str$witness = &$Times$str_instance;

struct $Hashable$str$class $Hashable$str$methods = {
    "$Hashable$str",
    UNASSIGNED,
    ($Super$class)&$Hashable$methods,
    (void (*)($Hashable$str))$default__init__,
    $Hashable$str$__serialize__,
    $Hashable$str$__deserialize__,
    ($bool(*)($Hashable$str))$default__bool__,
    ($str(*)($Hashable$str))$default__str__,
    $Hashable$str$__eq__,
    $Hashable$str$__ne__,
    $Hashable$str$__hash__};
struct $Hashable$str $Hashable$str_instance = {&$Hashable$str$methods};
$Hashable$str $Hashable$str$witness = &$Hashable$str_instance;

void $Container$str$__init__($Container$str wit, $Eq w$Eq$A$Container$str) {
    wit->w$Eq$A$Container$str = w$Eq$A$Container$str;
}

// Auxiliaries, some used for both str and bytearray implementations ////////////////////////////////////////////////////////

static unsigned char nul = 0;

static struct $str null_struct = {&$str$methods, 0, 0, &nul};

static $str null_str = &null_struct;

static struct $str space_struct = {&$str$methods, 1, 1, (unsigned char *)" "};

static $str space_str = &space_struct;

static struct $str whitespace_struct = {&$str$methods, 6, 6, (unsigned char *)" \t\n\r\x0b\x0c"};

static $str whitespace_str = &whitespace_struct;

#define NEW_UNFILLED_STR(nm, nchrs, nbtes) \
    nm = malloc(sizeof(struct $str));      \
    (nm)->$class = &$str$methods;          \
    (nm)->nchars = nchrs;                  \
    (nm)->nbytes = nbtes;                  \
    (nm)->str = malloc((nm)->nbytes + 1);  \
    (nm)->str[(nm)->nbytes] = 0

#define NEW_UNFILLED_BYTEARRAY(nm, nbtes)   \
    nm = malloc(sizeof(struct $bytearray)); \
    (nm)->$class = &$bytearray$methods;     \
    (nm)->nbytes = nbtes;                   \
    (nm)->capacity = nbtes;                 \
    (nm)->str = malloc((nm)->nbytes + 1);   \
    (nm)->str[(nm)->nbytes] = 0

// Conversion to and from C strings

$str to$str(char *str) {
    int nbytes = 0;
    int nchars = 0;

    unsigned char *p = (unsigned char *)str;
    int cp, cpnbytes;
    while (1) {
        if (*p == '\0') {
            $str res;
            NEW_UNFILLED_STR(res, nchars, nbytes);
            memcpy(res->str, str, nbytes);
            return res;
        }
        cpnbytes = utf8proc_iterate(p, -1, &cp);
        if (cpnbytes < 0) {
            $RAISE(($BaseException)$NEW($ValueError, to$str("to$str: Unicode decode error")));
            return NULL;
        }
        nbytes += cpnbytes;
        nchars++;
        p += cpnbytes;
    }
}

unsigned char *from$str($str str) {
    return str->str;
}

// #bytes in UTF-8 to represent codepoint cp
static int byte_length(unsigned int cp) {
    if (cp < 0x80)
        return 1;
    else if (cp < 0x800)
        return 2;
    else if (cp < 0x10000)
        return 3;
    else
        return 4;
}

// #bytes in UTF-8 for char starting with byte c
static int byte_length2(unsigned char c) {
    if (c < 0x7f)
        return 1;
    else if (c < 0xdf)
        return 2;
    else if (c < 0xef)
        return 3;
    else
        return 4;
}

typedef int (*transform)(int codepoint);

// Mapping a codepoint transform over an entire string
// For the moment only used for str_upper and str_lower;
// maybe not worthwhile to keep.
static $str str_transform($str s, transform f) {
    int cp, cpu, cplen, cpulen;
    unsigned char *p = s->str;
    unsigned char buffer[4 * s->nchars];
    unsigned char *up = buffer;
    for (int i = 0; i < s->nchars; i++) {
        cplen = utf8proc_iterate(p, -1, &cp);
        cpu = f(cp);
        cpulen = utf8proc_encode_char(cpu, up);
        p += cplen;
        up += cpulen;
    }
    int nbytes = (int)(up - buffer);
    $str res;
    NEW_UNFILLED_STR(res, s->nchars, nbytes);
    memcpy(res->str, buffer, nbytes);
    return res;
}

// Find char position in text from byte position.
// Assume that i is first byte of a char in text.
static int char_no($str text, int i) {
    if (text->nbytes == text->nchars) // ASCII string
        return i;
    int res = 0;
    int k = 0;
    unsigned char *t = text->str;
    while (k < i) {
        k += byte_length2(t[k]);
        res++;
    }
    return res;
}

static unsigned char *skip_chars(unsigned char *start, int n, int isascii) {
    unsigned char *res = start;
    if (isascii)
        return start + n;
    if (n >= 0) {
        for (int i = 0; i < n; i++)
            res += byte_length2(*res);
    } else {
        for (int i = 0; i < -n; i++) {
            res--;
            while (*res >> 6 == 2)
                res--;
        }
    }
    return res;
}

// Find byte position in text from char position.
// Assume i is a valid char index in text
static int byte_no($str text, int i) {
    int res = 0;
    unsigned char *t = text->str;
    for (int k = 0; k < i; k++)
        res += byte_length2(t[k]);
    return res;
}

// Handles negative indices in getitem etc (slice notation)
static int get_index(int i, int nchars) {
    if (i >= 0) {
        if (i < nchars)
            return i;
    } else {
        if (i >= -nchars)
            return nchars + i;
    }
    $RAISE(($BaseException)$NEW($IndexError, to$str("indexing outside str")));
    return 0;
}

// Eliminates slice notation in find, index, count and other methods
// with optional start and end and adds defaults for omitted parameters.

static int fix_start_end(int nchars, $int *start, $int *end) {
    if (*start == NULL) {
        *start = malloc(sizeof(struct $int));
        *start = to$int(0);
    }
    long st = from$int(*start);
    if (st > nchars)
        return -1;
    if (st < 0)
        st += nchars;
    st = st < 0 ? 0 : st;
    *start = to$int(st);

    if (*end == NULL) {
        *end = malloc(sizeof(struct $int));
        *end = to$int(nchars);
    }
    long en = from$int(*end);
    if (en > nchars)
        en = nchars;
    else if (en < 0)
        en += nchars;
    en = en < 0 ? 0 : en;

    *end = to$int(en);
    return 0;
}

// Builds a new one-char string starting at p.
static $str mk_char(unsigned char *p) {
    $str res;
    NEW_UNFILLED_STR(res, 1, byte_length2(*p));
    for (int i = 0; i < res->nbytes; i++)
        res->str[i] = p[i];
    return res;
}

static int isspace_codepoint(int codepoint) {
    int cat = utf8proc_get_property(codepoint)->category;
    int bidi = utf8proc_get_property(codepoint)->bidi_class;
    return (cat == UTF8PROC_CATEGORY_ZS || (bidi >= UTF8PROC_BIDI_CLASS_B && bidi <= UTF8PROC_BIDI_CLASS_WS));
}

static int islinebreak_codepoint(int codepoint) {
    // category not useful; all the seven codepoints we handle are in category Other, control.
    return (codepoint <= 0x0a && codepoint <= 0x0d) ||
           (codepoint >= 0x1c && codepoint <= 0x1e);
    // For now we ignore the three codepoints below which are counted as linebreaks by
    // Python's splitlines for strings.
    //  || codepoint == 0x85 || codepoint == 0x2028 ||codepoint == 0x2029;
}

// The Boyer-Moore-Horspool algorithm for searching for pattern in text.
// For very short patterns, this should be replaced by brute force.
// Returns byte position in text where first occurrence of pattern starts,
// or -1 if it does not occur.
// Start search from the left end of text.
int bmh(unsigned char *text, unsigned char *pattern, int tbytes, int pbytes) {
    if (pbytes > tbytes)
        return -1;
    int skip[256];
    for (int i = 0; i < 256; i++)
        skip[i] = pbytes;
    for (int i = 0; i < pbytes - 1; i++)
        skip[(int)pattern[i]] = pbytes - i - 1;
    int k = pbytes - 1;
    int i, j;
    while (k < tbytes) {
        j = pbytes - 1;
        i = k;
        while (j >= 0 && text[i] == pattern[j]) {
            j--;
            i--;
        }
        if (j == -1)
            return i + 1;
        k += skip[(int)text[k]];
    }
    return -1;
}

// Start search from the right end of text.
static int rbmh(unsigned char *text, unsigned char *pattern, int tbytes, int pbytes) {
    if (pbytes > tbytes)
        return -1;
    int skip[256];
    for (int i = 0; i < 256; i++)
        skip[i] = pbytes;
    for (int i = pbytes - 1; i > 0; i--)
        skip[(int)pattern[i]] = i;
    int k = tbytes - pbytes;
    int i, j;
    while (k >= 0) {
        j = 0;
        i = k;
        while (j < pbytes && text[i] == pattern[j]) {
            j++;
            i++;
        }
        if (j == pbytes)
            return i - pbytes;
        k -= skip[(int)text[k]];
    }
    return -1;
}

// Protocol methods; string implementations /////////////////////////////////////////////////////////////////////////////
/* 
Note: We make str instances for Indexed and Sliceable even though these protocols 
include mutating methods. 
*/

// $Ord ///////////////////////////////////////////////////////////////////////////////////////////////

// TODO: We should consider how to normalize strings before comparisons

int $str_eq($str a, $str b) {
    return (strcmp((char *)a->str, (char *)b->str) == 0);
}

int $str_neq($str a, $str b) {
    return !$str_eq(a, b);
}

// The comparisons below do lexicographic byte-wise comparisons.
// Thus they do not in general reflect locale-dependent order conventions.

int $str_lt($str a, $str b) {
    return (strcmp((char *)a->str, (char *)b->str) < 0);
}

int $str_le($str a, $str b) {
    return (strcmp((char *)a->str, (char *)b->str) <= 0);
}

int $str_gt($str a, $str b) {
    return (strcmp((char *)a->str, (char *)b->str) > 0);
}

int $str_ge($str a, $str b) {
    return (strcmp((char *)a->str, (char *)b->str) >= 0);
}

// $Hashable ///////////////////////////////////////////////////////////////////////////////////

// hash function $string_hash defined in hash.c

// $Times /////////////////////////////////////////////////////////////////////////////////////////////

$Times$str $Times$str$new() {
    return $NEW($Times$str);
}

$str $str_add($str s, $str t) {
    $str res;
    NEW_UNFILLED_STR(res, s->nchars + t->nchars, s->nbytes + t->nbytes);
    memcpy(res->str, s->str, s->nbytes);
    memcpy(res->str + s->nbytes, t->str, t->nbytes);
    return res;
}

$str $str_mul($str a, $int n) {
    if (n->val <= 0)
        return to$str("");
    else {
        $str res;
        NEW_UNFILLED_STR(res, a->nchars * n->val, a->nbytes * n->val);
        for (int i = 0; i < n->val; i++)
            memcpy(res->str + i * a->nbytes, a->str, a->nbytes);
        return res;
    }
}

// Collection ///////////////////////////////////////////////////////////////////////////////////////

$int $str_len($str s) {
    $int res = to$int(s->nchars);
    return res;
}

// $Container ///////////////////////////////////////////////////////////////////////////

$Container$str $Container$str$new($Eq wit) {
    return $NEW($Container$str, wit);
}

int $str_contains($str s, $str sub) {
    return bmh(s->str, sub->str, s->nbytes, sub->nbytes) > 0;
}

int $str_containsnot($str s, $str sub) {
    return !$str_contains(s, sub);
}

// Iterable ///////////////////////////////////////////////////////////////////////////

$Iterator$str $Iterator$str$new($str str) {
    return $NEW($Iterator$str, str);
}

void $Iterator$str_init($Iterator$str self, $str str) {
    self->src = str;
    self->nxt = 0;
}

void $Iterator$str_serialize($Iterator$str self, $Serial$state state) {
    $step_serialize(self->src, state);
    $step_serialize(to$int(self->nxt), state);
}

$Iterator$str $Iterator$str$_deserialize($Iterator$str res, $Serial$state state) {
    if (!res)
        res = $DNEW($Iterator$str, state);
    res->src = ($str)$step_deserialize(state);
    res->nxt = from$int(($int)$step_deserialize(state));
    return res;
}

$bool $Iterator$str_bool($Iterator$str self) {
    return $True;
}

$str $Iterator$str_str($Iterator$str self) {
    char *s;
    asprintf(&s, "<str iterator object at %p>", self);
    return to$str(s);
}

// this is next function for forward iteration
static $str $Iterator$str_next($Iterator$str self) {
    unsigned char *p = &self->src->str[self->nxt];
    if (*p != 0) {
        self->nxt += byte_length2(*p);
        return mk_char(p);
    }
    return NULL;
}

$Iterator $str_iter($str str) {
    return ($Iterator)$NEW($Iterator$str, str);
}

struct $Iterator$str$class $Iterator$str$methods = {"$Iterator$str", UNASSIGNED, ($Super$class)&$Iterator$methods, $Iterator$str_init,
                                                    $Iterator$str_serialize, $Iterator$str$_deserialize,
                                                    $Iterator$str_bool, $Iterator$str_str, $Iterator$str_next};

// Indexed ///////////////////////////////////////////////////////////////////////////

$str $str_getitem($str s, int i) {
    unsigned char *p = s->str;
    int ix = get_index(i, s->nchars);
    p = skip_chars(p, ix, s->nchars == s->nbytes);
    return mk_char(p);
}

// Sliceable //////////////////////////////////////////////////////////////////////////////////////

$str $str_getslice($str s, $slice slc) {
    int isascii = s->nchars == s->nbytes;
    int nchars = s->nchars;
    int nbytes = 0;
    int start, stop, step, slen;
    normalize_slice(slc, nchars, &slen, &start, &stop, &step);
    //slice notation have been eliminated and default values applied.
    unsigned char buffer[4 * slen]; // very conservative buffer size.
    unsigned char *p = buffer;
    unsigned char *t = skip_chars(s->str, start, isascii);
    for (int i = 0; i < slen; i++) {
        int bytes = byte_length2(*t);
        for (int k = 0; k < bytes; k++) {
            p[nbytes] = *t;
            t++;
            nbytes++;
        }
        t = skip_chars(t, step - 1, isascii);
    }
    $str res;
    NEW_UNFILLED_STR(res, slen, nbytes);
    if (nbytes > 0)
        memcpy(res->str, buffer, nbytes);
    return res;
}

// General methods //////////////////////////////////////////////////////////////

$str $str$new($value s) {
    return $NEW($str, s);
}

void $str_init($str self, $value s) {
    $str res = s->$class->__str__(s);
    self->nchars = res->nchars;
    self->nbytes = res->nbytes;
    self->str = res->str;
}

$bool $str_bool($str s) {
    return to$bool(s->nchars > 0);
};

$str $str_str($str s) {
    return s;
}

void $str_serialize($str str, $Serial$state state) {
    int nWords = str->nbytes / sizeof($WORD) + 1; // # $WORDS needed to store str->str, including terminating 0.
    $ROW row = $add_header(STR_ID, 2 + nWords, state);
    long nbytes = (long)str->nbytes;           // We could pack nbytes and nchars in one $WORD,
    memcpy(row->blob, &nbytes, sizeof($WORD)); // but we should think of a better, general approach.
    long nchars = (long)str->nchars;
    memcpy(row->blob + 1, &nchars, sizeof($WORD));
    memcpy(row->blob + 2, str->str, nbytes + 1);
}

$str $str_deserialize($str self, $Serial$state state) {
    $ROW this = state->row;
    state->row = this->next;
    state->row_no++;
    $str res = malloc(sizeof(struct $str));
    long nbytes;
    memcpy(&nbytes, this->blob, sizeof($WORD));
    res->$class = &$str$methods;
    res->nbytes = (int)nbytes;
    long nchars;
    memcpy(&nchars, this->blob + 1, sizeof($WORD));
    res->nchars = (int)nchars;
    res->str = malloc(nbytes + 1);
    memcpy(res->str, this->blob + 2, nbytes + 1);
    return res;
}

// str-specific methods ////////////////////////////////////////////////////////

$str $str_capitalize($str s) {
    if (s->nchars == 0) {
        return null_str;
    }
    int cp, cpu, cplen, cpulen;
    unsigned char *p = s->str;
    unsigned char buffer[4 * s->nchars];
    unsigned char *up = buffer;
    for (int i = 0; i < s->nchars; i++) {
        cplen = utf8proc_iterate(p, -1, &cp);
        cpu = i == 0 ? utf8proc_totitle(cp) : utf8proc_tolower(cp);
        cpulen = utf8proc_encode_char(cpu, up);
        p += cplen;
        up += cpulen;
    }
    int nbytes = (int)(up - buffer);
    $str res;
    NEW_UNFILLED_STR(res, s->nchars, nbytes);
    memcpy(res->str, buffer, nbytes);
    return res;
}

$str $str_center($str s, $int width, $str fill) {
    if (!fill)
        fill = space_str;
    if (fill->nchars != 1) {
        $RAISE(($BaseException)$NEW($ValueError, to$str("center: fill string not single char")));
    }
    if (width->val <= s->nchars) {
        return s;
    }
    int pad = (width->val - s->nchars);
    int padleft = pad / 2; // Below we make use of the fact padright >= padleft.
    int padright = pad - padleft;
    int fillbytes = fill->nbytes;
    int sbytes = s->nbytes;
    $str res;
    NEW_UNFILLED_STR(res, width->val, sbytes + pad * fillbytes);
    unsigned char *c = fill->str;
    unsigned char *p = res->str;
    p += padleft * fillbytes + sbytes;
    for (int i = 0; i < padright; i++) {
        for (int j = 0; j < fillbytes; j++)
            p[j] = c[j];
        p += fillbytes;
    }
    p -= padright * fillbytes;
    memcpy(res->str, p, padleft * fillbytes);
    p -= sbytes;
    memcpy(p, s->str, sbytes);
    return res;
}

$int $str_count($str s, $str sub, $int start, $int end) {
    int isascii = s->nchars == s->nbytes;
    $int st = start;
    $int en = end;
    if (fix_start_end(s->nchars, &st, &en) < 0)
        return to$int(0);
    unsigned char *p = skip_chars(s->str, from$int(st), isascii);
    unsigned char *q = skip_chars(p, from$int(en) - from$int(st), isascii);
    int res = 0;
    int n = bmh(p, sub->str, q - p, sub->nbytes);
    while (n >= 0) {
        res++;
        p += n + (sub->nbytes > 0 ? sub->nbytes : 1);
        n = bmh(p, sub->str, q - p, sub->nbytes);
    }
    return to$int(res);
}

$bytearray $str_encode($str s) {
    $bytearray res;
    NEW_UNFILLED_BYTEARRAY(res, s->nbytes);
    memcpy(res->str, s->str, s->nbytes);
    return res;
}

$bool $str_endswith($str s, $str sub, $int start, $int end) {
    $int st = start;
    $int en = end;
    if (fix_start_end(s->nchars, &st, &en) < 0)
        return $False;
    int isascii = s->nchars == s->nbytes;
    unsigned char *p = skip_chars(s->str + s->nbytes, from$int(en) - s->nchars, isascii) - sub->nbytes;
    unsigned char *q = sub->str;
    for (int i = 0; i < sub->nbytes; i++) {
        if (*p == 0 || *p++ != *q++) {
            return $False;
        }
    }
    return $True;
}

$str $str_expandtabs($str s, $int tabsize) {
    int tabsz = tabsize ? (long)tabsize->val : 8;
    int pos = 0;
    int expanded = 0;
    tabsz = tabsz <= 0 ? 1 : tabsz;
    unsigned char buffer[tabsz * s->nchars];
    unsigned char *p = s->str;
    unsigned char *q = buffer;
    for (int i = 0; i < s->nchars; i++) {
        if (*p == '\t') {
            int n = tabsz - pos % tabsz;
            for (int j = 0; j < n; j++) {
                *q++ = ' ';
            }
            p++;
            expanded += n - 1;
            pos += n;
        } else if (*p == '\n' || *p == '\r') {
            *q++ = *p++;
            pos = 0;
        } else {
            for (int j = 0; j < byte_length2(*p); j++) {
                *q++ = *p++;
                pos++;
            }
        }
    }
    $str res;
    NEW_UNFILLED_STR(res, s->nchars + expanded, s->nbytes + expanded);
    memcpy(res->str, buffer, s->nbytes + expanded);
    return res;
}

$int $str_find($str s, $str sub, $int start, $int end) {
    int isascii = s->nchars == s->nbytes;
    $int st = start;
    $int en = end;
    if (fix_start_end(s->nchars, &st, &en) < 0)
        return to$int(-1);
    unsigned char *p = skip_chars(s->str, from$int(st), isascii);
    unsigned char *q = skip_chars(p, from$int(en) - from$int(st), isascii);
    int n = bmh(p, sub->str, q - p, sub->nbytes);
    if (n < 0)
        return to$int(-1);
    return to$int(char_no(s, n + p - s->str));
}

$int $str_index($str s, $str sub, $int start, $int end) {
    $int n = $str_find(s, sub, start, end);
    if (from$int(n) < 0) {
        $RAISE(($BaseException)$NEW($ValueError, to$str("index: substring not found")));
    }
    return n;
}

$bool $str_isalnum($str s) {
    unsigned char *p = s->str;
    int codepoint;
    int nbytes;
    if (s->nchars == 0)
        return $False;
    for (int i = 0; i < s->nchars; i++) {
        nbytes = utf8proc_iterate(p, -1, &codepoint);
        utf8proc_category_t cat = utf8proc_category(codepoint);
        if ((cat < UTF8PROC_CATEGORY_LU || cat > UTF8PROC_CATEGORY_LO) && cat != UTF8PROC_CATEGORY_ND)
            return $False;
        p += nbytes;
    }
    return $True;
}

$bool $str_isalpha($str s) {
    unsigned char *p = s->str;
    int codepoint;
    int nbytes;
    if (s->nchars == 0)
        return $False;
    for (int i = 0; i < s->nchars; i++) {
        nbytes = utf8proc_iterate(p, -1, &codepoint);
        utf8proc_category_t cat = utf8proc_category(codepoint);
        if (cat < UTF8PROC_CATEGORY_LU || cat > UTF8PROC_CATEGORY_LO)
            return $False;
        p += nbytes;
    }
    return $True;
}

$bool $str_isascii($str s) {
    unsigned char *p = s->str;
    for (int i = 0; i < s->nbytes; i++) {
        if (*p > 127)
            return $False;
        p++;
    }
    return $True;
}

$bool $str_isdecimal($str s) {
    unsigned char *p = s->str;
    int codepoint;
    int nbytes;
    if (s->nchars == 0)
        return $False;
    for (int i = 0; i < s->nchars; i++) {
        nbytes = utf8proc_iterate(p, -1, &codepoint);
        utf8proc_category_t cat = utf8proc_category(codepoint);
        if (cat != UTF8PROC_CATEGORY_ND)
            return $False;
        p += nbytes;
    }
    return $True;
}

$bool $str_islower($str s) {
    unsigned char *p = s->str;
    int codepoint;
    int nbytes;
    int has_cased = 0;
    if (s->nchars == 0)
        return $False;
    for (int i = 0; i < s->nchars; i++) {
        nbytes = utf8proc_iterate(p, -1, &codepoint);
        utf8proc_category_t cat = utf8proc_category(codepoint);
        if (cat == UTF8PROC_CATEGORY_LT || cat == UTF8PROC_CATEGORY_LU)
            return $False;
        if (cat == UTF8PROC_CATEGORY_LL)
            has_cased = 1;
        p += nbytes;
    }
    return to$bool(has_cased);
}

$bool $str_isprintable($str s) {
    unsigned char *p = s->str;
    int codepoint;
    int nbytes;
    if (s->nchars == 0)
        return $False;
    for (int i = 0; i < s->nchars; i++) {
        nbytes = utf8proc_iterate(p, -1, &codepoint);
        utf8proc_category_t cat = utf8proc_category(codepoint);
        if (cat >= UTF8PROC_CATEGORY_ZS && codepoint != 0x20)
            return $False;
        p += nbytes;
    }
    return $True;
}

$bool $str_isspace($str s) {
    unsigned char *p = s->str;
    int codepoint;
    int nbytes;
    if (s->nchars == 0)
        return $False;
    for (int i = 0; i < s->nchars; i++) {
        nbytes = utf8proc_iterate(p, -1, &codepoint);
        if (!isspace_codepoint(codepoint))
            return $False;
        p += nbytes;
    }
    return $True;
}

$bool $str_istitle($str s) {
    unsigned char *p = s->str;
    int codepoint;
    int nbytes;
    int hascased = 0;
    int incasedrun = 0;
    if (s->nchars == 0)
        return $False;
    for (int i = 0; i < s->nchars; i++) {
        nbytes = utf8proc_iterate(p, -1, &codepoint);
        utf8proc_category_t cat = utf8proc_category(codepoint);
        if (cat == UTF8PROC_CATEGORY_LU || cat == UTF8PROC_CATEGORY_LT) {
            hascased = 1;
            if (incasedrun)
                return $False;
            incasedrun = 1;
        } else if (cat == UTF8PROC_CATEGORY_LL) {
            hascased = 1;
            if (!incasedrun)
                return $False;
        } else
            incasedrun = 0;
        p += nbytes;
    }
    return to$bool(hascased);
}

$bool $str_isupper($str s) {
    unsigned char *p = s->str;
    int codepoint;
    int nbytes;
    int hascased = 0;
    if (s->nchars == 0)
        return $False;
    for (int i = 0; i < s->nchars; i++) {
        nbytes = utf8proc_iterate(p, -1, &codepoint);
        utf8proc_category_t cat = utf8proc_category(codepoint);
        if (cat == UTF8PROC_CATEGORY_LL)
            return $False;
        if (cat == UTF8PROC_CATEGORY_LU || cat == UTF8PROC_CATEGORY_LT)
            hascased = 1;
        p += nbytes;
    }
    return to$bool(hascased);
}

$str $str_join($str s, $Iterable wit, $WORD iter) {
    int totchars = 0;
    int totbytes = 0;
    $list lst = $list_fromiter(wit->$class->__iter__(wit, iter));
    $str nxt;
    int len = lst->length;
    for (int i = 0; i < len; i++) {
        nxt = ($str)lst->data[i];
        totchars += nxt->nchars;
        totbytes += nxt->nbytes;
    }
    if (len > 1) {
        totchars += (len - 1) * s->nchars;
        totbytes += (len - 1) * s->nbytes;
    }
    $str res;
    NEW_UNFILLED_STR(res, totchars, totbytes);
    if (len > 0) {
        nxt = ($str)lst->data[0];
        unsigned char *p = res->str;
        memcpy(p, nxt->str, nxt->nbytes);
        p += nxt->nbytes;
        for (int i = 1; i < len; i++) {
            nxt = ($str)lst->data[i];
            memcpy(p, s->str, s->nbytes);
            p += s->nbytes;
            memcpy(p, nxt->str, nxt->nbytes);
            p += nxt->nbytes;
        }
    }
    return res;
}

$str $str_ljust($str s, $int width, $str fill) {
    if (!fill)
        fill = space_str;
    if (fill->nchars != 1) {
        $RAISE(($BaseException)$NEW($ValueError, to$str("ljust: fill str not single char")));
    }
    if (width->val <= s->nchars) {
        return s;
    }
    int pad = (width->val - s->nchars);
    $str res;
    NEW_UNFILLED_STR(res, width->val, s->nbytes + pad * fill->nbytes);
    unsigned char *c = fill->str;
    unsigned char *p = res->str + s->nbytes;
    for (int i = 0; i < pad; i++) {
        for (int j = 0; j < fill->nbytes; j++)
            *p++ = c[j];
    }
    memcpy(res->str, s->str, s->nbytes);
    return res;
}

$str $str_lower($str s) {
    return str_transform(s, utf8proc_tolower);
}

$str $str_lstrip($str s, $str cs) {
    unsigned char *p = s->str;
    int i, nbytes;
    for (i = 0; i < s->nchars; i++) {
        $str c = mk_char(p);
        if (cs == NULL ? !$str_isspace(c) : bmh(cs->str, p, cs->nbytes, byte_length2(*p)) < 0)
            break;
        p += byte_length2(*p);
    }
    nbytes = s->nbytes + s->str - p;
    $str res;
    NEW_UNFILLED_STR(res, s->nchars - i, nbytes);
    memcpy(res->str, p, nbytes);
    return res;
}

$tuple $str_partition($str s, $str sep) {
    int n = from$int($str_find(s, sep, NULL, NULL));
    if (n < 0) {
        return $NEWTUPLE(3, s, null_str, null_str);
    } else {
        int nb = bmh(s->str, sep->str, s->nbytes, sep->nbytes);
        $str ls;
        NEW_UNFILLED_STR(ls, n, nb);
        memcpy(ls->str, s->str, nb);
        $str rs;
        int nbr = s->nbytes - sep->nbytes - nb;
        NEW_UNFILLED_STR(rs, s->nchars - n - sep->nchars, nbr);
        memcpy(rs->str, s->str + nb + sep->nbytes, nbr);
        return $NEWTUPLE(3, ls, sep, rs);
    }
}

$str $str_replace($str s, $str old, $str new, $int count) {
    if (count == NULL)
        count = to$int(INT_MAX);
    int c = from$int($str_count(s, old, NULL, NULL));
    int c0 = from$int(count) < c ? from$int(count) : c;
    if (c0 == 0) {
        return s;
    }
    int nbytes = s->nbytes + c0 * (new->nbytes - old->nbytes);
    int nchars = s->nchars + c0 * (new->nchars - old->nchars);
    $str res;
    NEW_UNFILLED_STR(res, nchars, nbytes);
    unsigned char *p = s->str;
    unsigned char *q = res->str;
    unsigned char *pold = old->str;
    unsigned char *pnew = new->str;
    int plen = s->nbytes;
    int n;
    for (int i = 0; i < c0; i++) {
        n = i > 0 && old->nbytes == 0 ? 1 : bmh(p, pold, plen, old->nbytes);
        if (n > 0) {
            memcpy(q, p, n);
            p += n;
            q += n;
        }
        memcpy(q, pnew, new->nbytes);
        p += old->nbytes;
        q += new->nbytes;
        plen -= n + old->nbytes;
    }
    if (plen > 0)
        memcpy(q, p, plen);
    return res;
}

$int $str_rfind($str s, $str sub, $int start, $int end) {
    int isascii = s->nchars == s->nbytes;
    $int st = start;
    $int en = end;
    if (fix_start_end(s->nchars, &st, &en) < 0)
        return to$int(-1);
    unsigned char *p = skip_chars(s->str, from$int(st), isascii);
    unsigned char *q = skip_chars(p, from$int(en) - from$int(st), isascii);
    int n = rbmh(p, sub->str, q - p, sub->nbytes);
    if (n < 0)
        return to$int(-1);
    return to$int(char_no(s, n + p - s->str));
}

$int $str_rindex($str s, $str sub, $int start, $int end) {
    $int n = $str_rfind(s, sub, start, end);
    if (from$int(n) < 0) {
        $RAISE(($BaseException)$NEW($ValueError, to$str("rindex: substring not found")));
    };
    return n;
}

$str $str_rjust($str s, $int width, $str fill) {
    if (!fill)
        fill = space_str;
    if (fill->nchars != 1) {
        $RAISE(($BaseException)$NEW($ValueError, to$str("rjust: fill string not single char")));
    }
    if (width->val <= s->nchars) {
        return s;
    }
    int pad = (width->val - s->nchars);
    $str res;
    NEW_UNFILLED_STR(res, width->val, s->nbytes + pad * fill->nbytes);
    unsigned char *c = fill->str;
    unsigned char *p = res->str;
    for (int i = 0; i < pad; i++) {
        for (int j = 0; j < fill->nbytes; j++)
            *p++ = c[j];
    }
    memcpy(p, s->str, s->nbytes);
    return res;
}

$tuple $str_rpartition($str s, $str sep) {
    int n = from$int($str_rfind(s, sep, NULL, NULL));
    if (n < 0) {
        return $NEWTUPLE(3, null_str, null_str, s);
    } else {
        int nb = rbmh(s->str, sep->str, s->nbytes, sep->nbytes);
        $str ls;
        NEW_UNFILLED_STR(ls, n, nb);
        memcpy(ls->str, s->str, nb);
        int nbr = s->nbytes - sep->nbytes - nb;
        $str rs;
        NEW_UNFILLED_STR(rs, s->nchars - n - sep->nchars, nbr);
        memcpy(rs->str, s->str + nb + sep->nbytes, nbr);
        return $NEWTUPLE(3, ls, sep, rs);
    }
}

$list $str_split($str s, $str sep, $int maxsplit) {
    $list res = $NEW($list, NULL, NULL);
    if (maxsplit == NULL || from$int(maxsplit) < 0)
        maxsplit = to$int(INT_MAX);
    int remaining = s->nchars;
    if (sep == NULL) {
        unsigned char *p = s->str;
        int nbytes, codepoint, wordlength;
        if (remaining == 0) {
            return res;
        }
        int inword = 0;
        unsigned char *q;
        while (remaining > 0) {
            nbytes = utf8proc_iterate(p, -1, &codepoint);
            if (!isspace_codepoint(codepoint)) {
                if (!inword) {
                    inword = 1;
                    q = p;
                    wordlength = 1;
                    if ($list_len(res) == from$int(maxsplit))
                        break; // we have now removed leading whitespace in remainder
                } else
                    wordlength++;
            } else {
                if (inword) {
                    inword = 0;
                    $str word;
                    NEW_UNFILLED_STR(word, wordlength, p - q);
                    memcpy(word->str, q, p - q);
                    $list_append(res, word);
                }
            }
            remaining--;
            p += nbytes;
        }
        // this if statement should be simplified; almost code duplication.
        if (remaining == 0) {
            if (inword) {
                $str word;
                NEW_UNFILLED_STR(word, wordlength, p - q);
                memcpy(word->str, q, p - q);
                $list_append(res, word);
            }
        } else {
            $str word;
            p = s->str + s->nbytes;
            NEW_UNFILLED_STR(word, remaining, p - q);
            memcpy(word->str, q, p - q);
            $list_append(res, word);
        }
        // $WORD w = list_getitem(res,0);
        return res;
    } else { // separator given
        if (sep->nchars == 0) {
            $RAISE(($BaseException)$NEW($ValueError, to$str("split: separator is empty string")));
        }
        if (remaining == 0) { // for some unfathomable reason, this is the behaviour of the Python method
            $list_append(res, null_str);
            return res;
        }
        $str ls, rs, ssep;
        rs = s;
        // Note: This builds many intermediate rs strings...
        while (rs->nchars > 0 && $list_len(res) < from$int(maxsplit)) {
            $tuple t = $str_partition(rs, sep);
            ssep = ($str)t->components[1];
            rs = ($str)t->components[2];
            $list_append(res, ($str)t->components[0]);
        }
        if (ssep->nchars > 0)
            $list_append(res, rs);
        return res;
    }
}

$list $str_splitlines($str s, $bool keepends) {
    if (!keepends)
        keepends = $False;
    $list res = $NEW($list, NULL, NULL);
    unsigned char *p = s->str;
    unsigned char *q = p;
    int nbytes, codepoint, linelength;
    if (s->nbytes == 0) {
        return res;
    }
    while (p < s->str + s->nbytes) {
        nbytes = utf8proc_iterate(p, -1, &codepoint);
        utf8proc_category_t cat = utf8proc_category(codepoint);
        if (!islinebreak_codepoint(codepoint)) {
            linelength++;
            p += nbytes;
        } else {
            // all the codepoints we count as linebreaks are ascii bytes, i.e. nbytes = 1.
            $str line;
            int winend = *p == '\r' && *(p + 1) == '\n';
            int size = p - q + (keepends->val ? 1 + winend : 0);
            NEW_UNFILLED_STR(line, linelength + (keepends->val ? 1 + winend : 0), size);
            memcpy(line->str, q, size);
            p += 1 + winend;
            q = p;
            $list_append(res, line);
        }
    }
    if (q < p) {
        $str line;
        NEW_UNFILLED_STR(line, linelength, p - q);
        memcpy(line->str, q, p - q);
        $list_append(res, line);
    }
    return res;
}

$str $str_rstrip($str s, $str cs) {
    unsigned char *p = s->str + s->nbytes;
    int i, nbytes;
    for (i = 0; i < s->nchars; i++) {
        p = skip_chars(p, -1, 0);
        $str c = mk_char(p);
        if (cs == NULL ? !$str_isspace(c) : rbmh(cs->str, p, cs->nbytes, byte_length2(*p)) < 0)
            break;
    }
    nbytes = p + byte_length2(*p) - s->str;
    $str res;
    NEW_UNFILLED_STR(res, s->nchars - i, nbytes);
    memcpy(res->str, s->str, nbytes);
    return res;
}

$bool $str_startswith($str s, $str sub, $int start, $int end) {
    $int st = start;
    $int en = end;
    if (fix_start_end(s->nchars, &st, &en) < 0)
        return $False;
    int isascii = s->nchars == s->nbytes;
    unsigned char *p = skip_chars(s->str, from$int(st), isascii);
    unsigned char *q = sub->str;
    for (int i = 0; i < sub->nbytes; i++) {
        if (*p == 0 || *p++ != *q++) {
            return $False;
        }
    }
    return $True;
}

$str $str_strip($str s, $str cs) {
    return $str_lstrip($str_rstrip(s, cs), cs);
}

$str $str_upper($str s) {
    return str_transform(s, utf8proc_toupper);
}

$str $str_zfill($str s, $int width) {
    int fill = width->val - s->nchars;
    if (fill < 0)
        return s;
    $str res;
    NEW_UNFILLED_STR(res, width->val, s->nbytes + fill);
    unsigned char *p = s->str;
    unsigned char *q = res->str;
    int hassign = (*p == '+' | *p == '-');
    if (hassign) {
        *q = *p;
        q++;
    }
    for (int i = 0; i < fill; i++)
        *q++ = '0';
    memcpy(res->str + hassign + fill, s->str + hassign, s->nbytes - hassign);
    return res;
}

// End of str implementation ////////////////////////////////////////////////////

// bytearray implementation //////////////////////////////////////////////////////////////////////////////

// Conversion to and from C strings

$bytearray to$bytearray(char *str) {
    $bytearray res;
    int len = strlen(str);
    NEW_UNFILLED_BYTEARRAY(res, len);
    memcpy(res->str, str, len);
    return res;
}

unsigned char *from$bytearray($bytearray b) {
    return b->str;
}

// Auxiliaries

static void expand_bytearray($bytearray b, int n) {
    if (b->capacity >= b->nbytes + n)
        return;
    int newcapacity = b->capacity == 0 ? 1 : b->capacity;
    while (newcapacity < b->nbytes + n)
        newcapacity <<= 1;
    unsigned char *newstr = b->str == NULL
                                ? malloc(newcapacity + 1)
                                : realloc(b->str, newcapacity + 1);
    if (newstr == NULL) {
        $RAISE(($BaseException)$NEW($MemoryError, to$str("memory allocation failed")));
    }
    b->str = newstr;
    b->capacity = newcapacity;
}

// General methods, prototypes
void $bytearray_init($bytearray, $value);
$bool $bytearray_bool($bytearray);
$str $bytearray_str($bytearray);
void $bytearray_serialize($bytearray, $Serial$state);
$bytearray $bytearray_deserialize($bytearray, $Serial$state);

// bytearray methods, prototypes

$bytearray $bytearray_capitalize($bytearray s);
$bytearray $bytearray_center($bytearray s, $int width, $bytearray fill);
$int $bytearray_count($bytearray s, $bytearray sub, $int start, $int end);
$str $bytearray_decode($bytearray s);
$bool $bytearray_endswith($bytearray s, $bytearray suffix, $int start, $int end);
$bytearray $bytearray_expandtabs($bytearray s, $int tabsize);
$int $bytearray_find($bytearray s, $bytearray sub, $int start, $int end);
$int $bytearray_index($bytearray s, $bytearray sub, $int start, $int end);
$bool $bytearray_isalnum($bytearray s);
$bool $bytearray_isalpha($bytearray s);
$bool $bytearray_isascii($bytearray s);
$bool $bytearray_isdecimal($bytearray s);
$bool $bytearray_isdigit($bytearray s);
$bool $bytearray_isidentifier($bytearray s);
$bool $bytearray_islower($bytearray s);
$bool $bytearray_isnumeric($bytearray s);
$bool $bytearray_isprintable($bytearray s);
$bool $bytearray_isspace($bytearray s);
$bool $bytearray_istitle($bytearray s);
$bool $bytearray_isupper($bytearray s);
$bytearray $bytearray_join($bytearray sep, $Iterable wit, $WORD iter);
$bytearray $bytearray_ljust($bytearray s, $int width, $bytearray fill);
$bytearray $bytearray_lower($bytearray s);
$bytearray $bytearray_lstrip($bytearray s, $bytearray cs);
$tuple $bytearray_partition($bytearray s, $bytearray sep);
$bytearray $bytearray_replace($bytearray s, $bytearray old, $bytearray new, $int count);
$int $bytearray_rfind($bytearray s, $bytearray sub, $int start, $int end);
$int $bytearray_rindex($bytearray s, $bytearray sub, $int start, $int end);
$bytearray $bytearray_rjust($bytearray s, $int width, $bytearray fill);
$tuple $bytearray_rpartition($bytearray s, $bytearray sep);
$bytearray $bytearray_rstrip($bytearray s, $bytearray cs);
$list $bytearray_split($bytearray s, $bytearray sep, $int maxsplit);
$list $bytearray_splitlines($bytearray s, $bool keepends);
$bool $bytearray_startswith($bytearray s, $bytearray prefix, $int start, $int end);
$bytearray $bytearray_strip($bytearray s, $bytearray cs);
$bytearray $bytearray_upper($bytearray s);
$bytearray $bytearray_zfill($bytearray s, $int width);

// Method table

struct $bytearray$class $bytearray$methods =
    {"$bytearray", UNASSIGNED, ($Super$class)&$value$methods, $bytearray_init, $bytearray_serialize, $bytearray_deserialize, $bytearray_bool,
     $bytearray_str, $bytearray_capitalize, $bytearray_center, $bytearray_count, $bytearray_decode, $bytearray_endswith,
     $bytearray_expandtabs, $bytearray_find, $bytearray_index,
     $bytearray_isalnum, $bytearray_isalpha, $bytearray_isascii, $bytearray_isdigit, $bytearray_islower, $bytearray_isspace,
     $bytearray_istitle, $bytearray_isupper, $bytearray_join, $bytearray_ljust, $bytearray_lower, $bytearray_lstrip, $bytearray_partition, $bytearray_replace,
     $bytearray_rfind, $bytearray_rindex, $bytearray_rjust,
     $bytearray_rpartition, $bytearray_rstrip, $bytearray_split, $bytearray_splitlines, $bytearray_startswith, $bytearray_strip, $bytearray_upper, $bytearray_zfill};

// Bytearray methods, implementations

static $bytearray $bytearray_copy($bytearray s) {
    $bytearray res;
    NEW_UNFILLED_BYTEARRAY(res, s->nbytes);
    res->nbytes = s->nbytes;
    memcpy(res->str, s->str, s->nbytes);
    return res;
}

$bytearray $bytearray_capitalize($bytearray s) {
    if (s->nbytes == 0) {
        return to$bytearray("");
    }
    $bytearray res;
    NEW_UNFILLED_BYTEARRAY(res, s->nbytes);
    for (int i = 0; i < s->nbytes; i++)
        res->str[i] = i == 0 ? toupper(s->str[i]) : tolower(s->str[i]);
    return res;
}

$bytearray $bytearray_center($bytearray s, $int width, $bytearray fill) {
    if (!fill)
        fill = to$bytearray(" ");
    if (fill->nbytes != 1) {
        $RAISE(($BaseException)$NEW($ValueError, to$str("center: fill bytearray not single char")));
    }
    if (width->val <= s->nbytes) {
        return $bytearray_copy(s);
    }
    int pad = (width->val - s->nbytes);
    int padleft = pad / 2;
    int padright = pad - padleft;
    int sbytes = s->nbytes;
    $bytearray res;
    NEW_UNFILLED_BYTEARRAY(res, width->val);
    unsigned char c = fill->str[0];
    unsigned char *p = res->str;
    p += padleft + sbytes;
    for (int i = 0; i < padright; i++) {
        p[i] = c;
    }
    memcpy(res->str, p, padleft);
    p -= sbytes;
    memcpy(p, s->str, sbytes);
    return res;
}

$int $bytearray_count($bytearray s, $bytearray sub, $int start, $int end) {
    $int st = start;
    $int en = end;
    if (fix_start_end(s->nbytes, &st, &en) < 0)
        return to$int(0);
    unsigned char *p = &s->str[st->val];
    unsigned char *q = &p[en->val - st->val];
    int res = 0;
    int n = bmh(p, sub->str, q - p, sub->nbytes);
    while (n >= 0) {
        res++;
        p += n + (sub->nbytes > 0 ? sub->nbytes : 1);
        n = bmh(p, sub->str, q - p, sub->nbytes);
    }
    return to$int(res);
}

$str $bytearray_decode($bytearray s) {
    return to$str((char *)s->str);
}

$bool $bytearray_endswith($bytearray s, $bytearray sub, $int start, $int end) {
    $int st = start;
    $int en = end;
    if (fix_start_end(s->nbytes, &st, &en) < 0)
        return $False;
    unsigned char *p = &s->str[en->val - sub->nbytes];
    unsigned char *q = sub->str;
    for (int i = 0; i < sub->nbytes; i++) {
        if (*p == 0 || *p++ != *q++) {
            return $False;
        }
    }
    return $True;
}

$bytearray $bytearray_expandtabs($bytearray s, $int tabsz) {
    int pos = 0;
    int expanded = 0;
    int tabsize = tabsz->val;
    tabsize = tabsize <= 0 ? 1 : tabsize;
    unsigned char buffer[tabsize * s->nbytes];
    unsigned char *p = s->str;
    unsigned char *q = buffer;
    for (int i = 0; i < s->nbytes; i++) {
        if (*p == '\t') {
            int n = tabsize - pos % tabsize;
            for (int j = 0; j < n; j++) {
                *q++ = ' ';
            }
            p++;
            expanded += n - 1;
            pos += n;
        } else if (*p == '\n' || *p == '\r') {
            *q++ = *p++;
            pos = 0;
        } else {
            for (int j = 0; j < byte_length2(*p); j++) {
                *q++ = *p++;
                pos++;
            }
        }
    }
    $bytearray res;
    NEW_UNFILLED_BYTEARRAY(res, s->nbytes + expanded);
    memcpy(res->str, buffer, s->nbytes + expanded);
    return res;
}

$int $bytearray_find($bytearray s, $bytearray sub, $int start, $int end) {
    $int st = start;
    $int en = end;
    if (fix_start_end(s->nbytes, &st, &en) < 0)
        return to$int(-1);
    unsigned char *p = &s->str[st->val];
    unsigned char *q = &s->str[en->val];
    int n = bmh(p, sub->str, q - p, sub->nbytes);
    if (n < 0)
        return to$int(-1);
    return to$int(n + p - s->str);
}

$int $bytearray_index($bytearray s, $bytearray sub, $int start, $int end) {
    $int n = $bytearray_find(s, sub, start, end);
    if (n->val < 0) {
        $RAISE(($BaseException)$NEW($ValueError, to$str("index: substring not found")));
    }
    return n;
}

$bool $bytearray_isalnum($bytearray s) {
    if (s->nbytes == 0)
        return $False;
    for (int i = 0; i < s->nbytes; i++) {
        unsigned char c = s->str[i];
        if (c < '0' || c > 'z' || (c > '9' && c < 'A') || (c > 'Z' && c < 'a'))
            return $False;
    }
    return $True;
}

$bool $bytearray_isalpha($bytearray s) {
    if (s->nbytes == 0)
        return $False;
    for (int i = 0; i < s->nbytes; i++) {
        unsigned char c = s->str[i];
        if (c < 'A' || c > 'z' || (c > 'Z' && c < 'a'))
            return $False;
    }
    return $True;
}

$bool $bytearray_isascii($bytearray s) {
    for (int i = 0; i < s->nbytes; i++) {
        unsigned char c = s->str[i];
        if (c > 0x7f)
            return $False;
    }
    return $True;
}

$bool $bytearray_isdigit($bytearray s) {
    if (s->nbytes == 0)
        return $False;
    for (int i = 0; i < s->nbytes; i++) {
        unsigned char c = s->str[i];
        if (c < '0' || c > '9')
            return $False;
    }
    return $True;
}

$bool $bytearray_islower($bytearray s) {
    int has_lower = 0;
    for (int i = 0; i < s->nbytes; i++) {
        unsigned char c = s->str[i];
        if (c >= 'A' && c <= 'Z')
            return $False;
        if (c >= 'a' && c <= 'z')
            has_lower = 1;
    }
    return to$bool(has_lower);
}

$bool $bytearray_isspace($bytearray s) {
    if (s->nbytes == 0)
        return $False;
    for (int i = 0; i < s->nbytes; i++) {
        unsigned char c = s->str[i];
        if (c != ' ' && c != '\t' && c != '\n' && c != '\r' && c != '\x0b' && c != '\f')
            return $False;
    }
    return $True;
}

$bool $bytearray_istitle($bytearray s) {
    if (s->nbytes == 0)
        return $False;
    int incasedrun = 0;
    for (int i = 0; i < s->nbytes; i++) {
        unsigned char c = s->str[i];
        if (c >= 'A' && c <= 'Z') {
            if (incasedrun)
                return $False;
            incasedrun = 1;
        } else if (c >= 'a' && c <= 'z') {
            if (!incasedrun)
                return $False;
        } else
            incasedrun = 0;
    }
    return $True;
}

$bool $bytearray_isupper($bytearray s) {
    int has_upper = 0;
    for (int i = 0; i < s->nbytes; i++) {
        unsigned char c = s->str[i];
        if (c >= 'a' && c <= 'z')
            return $False;
        if (c >= 'a' && c <= 'z')
            has_upper = 1;
    }
    return to$bool(has_upper);
}

$bytearray $bytearray_join($bytearray s, $Iterable wit, $WORD iter) {
    int totbytes = 0;
    $list lst = $list_fromiter(wit->$class->__iter__(wit, iter));
    $bytearray nxt;
    int len = lst->length;
    for (int i = 0; i < len; i++) {
        nxt = ($bytearray)lst->data[i];
        totbytes += nxt->nbytes;
    }
    if (len > 1) {
        totbytes += (len - 1) * s->nbytes;
    }
    $bytearray res;
    NEW_UNFILLED_BYTEARRAY(res, totbytes);
    if (len > 0) {
        nxt = ($bytearray)lst->data[0];
        unsigned char *p = res->str;
        memcpy(p, nxt->str, nxt->nbytes);
        p += nxt->nbytes;
        for (int i = 1; i < len; i++) {
            nxt = ($bytearray)lst->data[i];
            memcpy(p, s->str, s->nbytes);
            p += s->nbytes;
            memcpy(p, nxt->str, nxt->nbytes);
            p += nxt->nbytes;
        }
    }
    return res;
}

$bytearray $bytearray_ljust($bytearray s, $int width, $bytearray fill) {
    if (fill->nbytes != 1) {
        $RAISE(($BaseException)$NEW($ValueError, to$str("bytearray ljust: fill array not single char")));
    }
    if (width->val <= s->nbytes) {
        return $bytearray_copy(s);
    }
    $bytearray res;
    NEW_UNFILLED_BYTEARRAY(res, width->val);
    memcpy(res->str, s->str, s->nbytes);
    unsigned char c = fill->str[0];
    for (int i = s->nbytes; i < width->val; i++) {
        res->str[i] = c;
    }
    return res;
}

$bytearray $bytearray_lower($bytearray s) {
    $bytearray res;
    NEW_UNFILLED_BYTEARRAY(res, s->nbytes);
    for (int i = 0; i < s->nbytes; i++)
        res->str[i] = tolower(res->str[i]);
    return res;
}

$bytearray $bytearray_lstrip($bytearray s, $bytearray cs) {
    if (!cs)
        cs = to$bytearray(" \t\n\r\x0b\x0c");
    int nstrip = 0;
    for (int i = 0; i < s->nbytes; i++) {
        unsigned char c = s->str[i];
        int found = 0;
        for (int j = 0; j < cs->nbytes; j++)
            if (c == cs->str[j]) {
                found = 1;
                break;
            }
        if (!found)
            break;
        nstrip++;
    }
    $bytearray res;
    NEW_UNFILLED_BYTEARRAY(res, s->nbytes - nstrip);
    memcpy(res->str, s->str + nstrip, res->nbytes);
    return res;
}

$tuple $bytearray_partition($bytearray s, $bytearray sep) {
    int n = from$int($bytearray_find(s, sep, NULL, NULL));
    if (n < 0) {
        return $NEWTUPLE(3, s, to$bytearray(""), to$bytearray(""));
    } else {
        int nb = bmh(s->str, sep->str, s->nbytes, sep->nbytes);
        $bytearray ls;
        NEW_UNFILLED_BYTEARRAY(ls, nb);
        memcpy(ls->str, s->str, nb);
        $bytearray rs;
        int nbr = s->nbytes - sep->nbytes - nb;
        NEW_UNFILLED_BYTEARRAY(rs, nbr);
        memcpy(rs->str, s->str + nb + sep->nbytes, nbr);
        return $NEWTUPLE(3, ls, sep, rs);
    }
}

$bytearray $bytearray_replace($bytearray s, $bytearray old, $bytearray new, $int count) {
    if (count == NULL)
        count = to$int(INT_MAX);
    int c = from$int($bytearray_count(s, old, NULL, NULL));
    int c0 = from$int(count) < c ? from$int(count) : c;
    if (c0 == 0) {
        return $bytearray_copy(s);
    }
    int nbytes = s->nbytes + c0 * (new->nbytes - old->nbytes);
    $bytearray res;
    NEW_UNFILLED_BYTEARRAY(res, nbytes);
    unsigned char *p = s->str;
    unsigned char *q = res->str;
    unsigned char *pold = old->str;
    unsigned char *pnew = new->str;
    int plen = s->nbytes;
    int n;
    for (int i = 0; i < c0; i++) {
        n = i > 0 && old->nbytes == 0 ? 1 : bmh(p, pold, plen, old->nbytes);
        if (n > 0) {
            memcpy(q, p, n);
            p += n;
            q += n;
        }
        memcpy(q, pnew, new->nbytes);
        p += old->nbytes;
        q += new->nbytes;
        plen -= n + old->nbytes;
    }
    if (plen > 0)
        memcpy(q, p, plen);
    return res;
}

$int $bytearray_rfind($bytearray s, $bytearray sub, $int start, $int end) {
    $int st = start;
    $int en = end;
    if (fix_start_end(s->nbytes, &st, &en) < 0)
        return to$int(-1);
    unsigned char *p = &s->str[st->val];
    unsigned char *q = &s->str[en->val];
    int n = rbmh(p, sub->str, q - p, sub->nbytes);
    if (n < 0)
        return to$int(-1);
    return to$int(n + p - s->str);
}

$int $bytearray_rindex($bytearray s, $bytearray sub, $int start, $int end) {
    $int n = $bytearray_rfind(s, sub, start, end);
    if (from$int(n) < 0) {
        $RAISE(($BaseException)$NEW($ValueError, to$str("rindex for bytearray: substring not found")));
    };
    return n;
}

$bytearray $bytearray_rjust($bytearray s, $int width, $bytearray fill) {
    if (fill->nbytes != 1) {
        $RAISE(($BaseException)$NEW($ValueError, to$str("rjust: fill string not single char")));
    }
    if (width->val <= s->nbytes) {
        return $bytearray_copy(s);
    }
    int pad = (width->val - s->nbytes);
    $bytearray res;
    NEW_UNFILLED_BYTEARRAY(res, width->val);
    unsigned char c = fill->str[0];
    for (int i = 0; i < pad; i++) {
        res->str[i] = c;
    }
    memcpy(&res->str[pad], s->str, s->nbytes);
    return res;
}

$tuple $bytearray_rpartition($bytearray s, $bytearray sep) {
    int n = from$int($bytearray_rfind(s, sep, NULL, NULL));
    if (n < 0) {
        return $NEWTUPLE(3, to$bytearray(""), to$bytearray(""), s);
    } else {
        int nb = rbmh(s->str, sep->str, s->nbytes, sep->nbytes);
        $bytearray ls;
        NEW_UNFILLED_BYTEARRAY(ls, nb);
        memcpy(ls->str, s->str, nb);
        int nbr = s->nbytes - sep->nbytes - nb;
        $bytearray rs;
        NEW_UNFILLED_BYTEARRAY(rs, nbr);
        memcpy(rs->str, s->str + nb + sep->nbytes, nbr);
        return $NEWTUPLE(3, ls, sep, rs);
    }
}

$bytearray $bytearray_rstrip($bytearray s, $bytearray cs) {
    if (!cs)
        cs = to$bytearray(" \t\n\r\x0b\x0c");
    int nstrip = 0;
    for (int i = s->nbytes - 1; i >= 0; i--) {
        unsigned char c = s->str[i];
        int found = 0;
        for (int j = 0; j < cs->nbytes; j++)
            if (c == cs->str[j]) {
                found = 1;
                break;
            }
        if (!found)
            break;
        nstrip++;
    }
    $bytearray res;
    NEW_UNFILLED_BYTEARRAY(res, s->nbytes - nstrip);
    memcpy(res->str, s->str, res->nbytes);
    return res;
}

$list $bytearray_split($bytearray s, $bytearray sep, $int maxsplit) {
    $list res = $NEW($list, NULL, NULL);
    if (maxsplit == NULL || from$int(maxsplit) < 0)
        maxsplit = to$int(INT_MAX);
    if (sep == NULL) {
        unsigned char *p = s->str;
        if (s->nbytes == 0) {
            return res;
        }
        int inword = 0;
        unsigned char *q;
        while (p < s->str + s->nbytes) {
            if (*p != ' ' && *p != '\t' && *p != '\n' && *p != '\r' && *p != '\x0b' && *p != '\f') {
                if (!inword) {
                    inword = 1;
                    q = p;
                    if ($list_len(res) == from$int(maxsplit))
                        break; // we have now removed leading whitespace in remainder
                }
            } else {
                if (inword) {
                    inword = 0;
                    $bytearray word;
                    NEW_UNFILLED_BYTEARRAY(word, p - q);
                    memcpy(word->str, q, p - q);
                    $list_append(res, word);
                }
            }
            p++;
        }
        // this if statement should be simplified; almost code duplication.
        if (p < s->str + s->nbytes) { // we did not break out of the while loop
            if (inword) {
                $bytearray word;
                NEW_UNFILLED_BYTEARRAY(word, p - q);
                memcpy(word->str, q, p - q);
                $list_append(res, word);
            }
        } else {
            $bytearray word;
            p = s->str + s->nbytes;
            NEW_UNFILLED_BYTEARRAY(word, p - q);
            memcpy(word->str, q, p - q);
            $list_append(res, word);
        }
        return res;
    } else { // separator given
        if (sep->nbytes == 0) {
            $RAISE(($BaseException)$NEW($ValueError, to$str("split for bytearray: separator is empty string")));
        }
        if (s->nbytes == 0) { // for some unfathomable reason, this is the behaviour of the Python method
            $list_append(res, null_str);
            return res;
        }
        $bytearray ls, rs, ssep;
        rs = s;
        // Note: This builds many intermediate rs strings...
        while (rs->nbytes > 0 && $list_len(res) < from$int(maxsplit)) {
            $tuple t = $bytearray_partition(rs, sep);
            ssep = ($bytearray)t->components[1];
            rs = ($bytearray)t->components[2];
            $list_append(res, ($bytearray)t->components[0]);
        }
        if (ssep->nbytes > 0)
            $list_append(res, rs);
        return res;
    }
}

$list $bytearray_splitlines($bytearray s, $bool keepends) {
    if (!keepends)
        keepends = $False;
    $list res = $NEW($list, NULL, NULL);
    if (s->nbytes == 0) {
        return res;
    }
    int winend;
    unsigned char *p = s->str;
    unsigned char *q = p;
    while (p < s->str + s->nbytes) {
        if (*p != '\n' && *p != '\r') {
            p++;
        } else {
            $bytearray line;
            winend = *p == '\r' && *(p + 1) == '\n';
            int size = p - q + (keepends->val ? 1 + winend : 0);
            NEW_UNFILLED_BYTEARRAY(line, size);
            memcpy(line->str, q, size);
            p += 1 + winend;
            q = p;
            $list_append(res, line);
        }
    }
    if (q < p) {
        $bytearray line;
        NEW_UNFILLED_BYTEARRAY(line, p - q);
        memcpy(line->str, q, p - q);
        $list_append(res, line);
    }
    return res;
}

$bool $bytearray_startswith($bytearray s, $bytearray sub, $int start, $int end) {
    $int st = start;
    $int en = end;
    if (fix_start_end(s->nbytes, &st, &en) < 0)
        return $False;
    unsigned char *p = s->str + st->val;
    if (p + sub->nbytes >= s->str + s->nbytes)
        return $False;
    unsigned char *q = sub->str;
    for (int i = 0; i < sub->nbytes; i++) {
        if (p >= s->str + en->val || *p++ != *q++) {
            return $False;
        }
    }
    return $True;
}

$bytearray $bytearray_strip($bytearray s, $bytearray cs) {
    return $bytearray_lstrip($bytearray_rstrip(s, cs), cs);
}

$bytearray $bytearray_upper($bytearray s) {
    $bytearray res;
    NEW_UNFILLED_BYTEARRAY(res, s->nbytes);
    for (int i = 0; i < s->nbytes; i++)
        res->str[i] = toupper(res->str[i]);
    return res;
}

$bytearray $bytearray_zfill($bytearray s, $int width) {
    int fill = width->val - s->nbytes;
    if (fill < 0)
        return $bytearray_copy(s);
    $bytearray res;
    NEW_UNFILLED_BYTEARRAY(res, width->val);
    unsigned char *p = s->str;
    unsigned char *q = res->str;
    int hassign = (*p == '+' | *p == '-');
    if (hassign) {
        *q = *p;
        q++;
    }
    for (int i = 0; i < fill; i++)
        *q++ = '0';
    memcpy(res->str + hassign + fill, s->str + hassign, s->nbytes - hassign);
    return res;
}

// Protocol methods, prototypes for bytearrays

int $bytearray_eq($bytearray, $bytearray);
int $bytearray_neq($bytearray, $bytearray);
int $bytearray_lt($bytearray, $bytearray);
int $bytearray_le($bytearray, $bytearray);
int $bytearray_gt($bytearray, $bytearray);
int $bytearray_ge($bytearray, $bytearray);

$int $bytearray_getitem($bytearray, int);
void $bytearray_setitem($bytearray, int, int);
void $bytearray_delitem($bytearray, int);
$bytearray $bytearray_getslice($bytearray, $slice);
void $bytearray_setslice($bytearray, $slice, $Iterator);
void $bytearray_delslice($bytearray, $slice);
$Iterator $bytearray_reversed($bytearray);
void $bytearray_insert($bytearray, int, $int);
void $bytearray_append($bytearray, $int);
void $bytearray_reverse($bytearray);

$Iterator $bytearray_iter($bytearray);
$bytearray $bytearray_fromiter($Iterable, $WORD);
$int $bytearray_len($bytearray str);

$bytearray $bytearray_add($bytearray, $bytearray);
$bytearray $bytearray_mul($bytearray, $int);

int $bytearray_contains($bytearray, $int);
int $bytearray_containsnot($bytearray, $int);

// Protocol instances, using above prototypes

// Ord

void $Ord$bytearray$__serialize__($Ord$bytearray self, $Serial$state state) {
}

$Ord$bytearray $Ord$bytearray$__deserialize__($Ord$bytearray self, $Serial$state state) {
    $Ord$bytearray res = $DNEW($Ord$bytearray, state);
    return res;
}

$Ord$bytearray $Ord$bytearray$new() {
    return $NEW($Ord$bytearray);
}

$bool $Ord$bytearray$__eq__($Ord$bytearray wit, $bytearray a, $bytearray b) {
    return to$bool($bytearray_eq(a, b));
}

$bool $Ord$bytearray$__ne__($Ord$bytearray wit, $bytearray a, $bytearray b) {
    return to$bool($bytearray_neq(a, b));
}

$bool $Ord$bytearray$__lt__($Ord$bytearray wit, $bytearray a, $bytearray b) {
    return to$bool($bytearray_lt(a, b));
}

$bool $Ord$bytearray$__le__($Ord$bytearray wit, $bytearray a, $bytearray b) {
    return to$bool($bytearray_le(a, b));
}

$bool $Ord$bytearray$__gt__($Ord$bytearray wit, $bytearray a, $bytearray b) {
    return to$bool($bytearray_gt(a, b));
}

$bool $Ord$bytearray$__ge__($Ord$bytearray wit, $bytearray a, $bytearray b) {
    return to$bool($bytearray_ge(a, b));
}

// Sequence

void $Sequence$bytearray$__serialize__($Sequence$bytearray self, $Serial$state state) {
    $step_serialize(self->w$Collection, state);
    $step_serialize(self->w$Times, state);
}

$Sequence$bytearray $Sequence$bytearray$__deserialize__($Sequence$bytearray self, $Serial$state state) {
    $Sequence$bytearray res = $DNEW($Sequence$bytearray, state);
    res->w$Collection = ($Collection)$step_deserialize(state);
    res->w$Times = ($Times)$step_deserialize(state);
    return res;
}

$Sequence$bytearray $Sequence$bytearray$new() {
    return $NEW($Sequence$bytearray);
}

$int $Sequence$bytearray$__getitem__($Sequence$bytearray wit, $bytearray self, $int ix) {
    return $bytearray_getitem(self, from$int(ix));
}

void $Sequence$bytearray$__setitem__($Sequence$bytearray wit, $bytearray self, $int ix, $int val) {
    $bytearray_setitem(self, from$int(ix), from$int(val));
}

void $Sequence$bytearray$__delitem__($Sequence$bytearray wit, $bytearray self, $int ix) {
    $bytearray_delitem(self, from$int(ix));
}

$bytearray $Sequence$bytearray$__getslice__($Sequence$bytearray wit, $bytearray self, $slice slc) {
    return $bytearray_getslice(self, slc);
}

void $Sequence$bytearray$__setslice__($Sequence$bytearray wit, $bytearray self, $Iterable wit2, $slice slc, $WORD iter) {
    $bytearray_setslice(self, slc, wit2->$class->__iter__(wit2, iter));
}

void $Sequence$bytearray$__delslice__($Sequence$bytearray wit, $bytearray self, $slice slc) {
    $bytearray_delslice(self, slc);
}

$Iterator $Sequence$bytearray$__reversed__($Sequence$bytearray wit, $bytearray self) {
    return $bytearray_reversed(self);
}

void $Sequence$bytearray$insert($Sequence$bytearray wit, $bytearray self, $int ix, $int val) {
    $bytearray_insert(self, ix->val, val);
}

void $Sequence$bytearray$append($Sequence$bytearray wit, $bytearray self, $int val) {
    $bytearray_append(self, val);
}

void $Sequence$bytearray$reverse($Sequence$bytearray wit, $bytearray self) {
    $bytearray_reverse(self);
}

// Collection

void $Collection$bytearray$__serialize__($Collection$bytearray self, $Serial$state state) {
    $step_serialize(self->w$Sequence, state);
}

$Collection$bytearray $Collection$bytearray$__deserialize__($Collection$bytearray self, $Serial$state state) {
    $Collection$bytearray res = $DNEW($Collection$bytearray, state);
    res->w$Sequence = ($Sequence)$step_deserialize(state);
    return res;
}

$Collection$bytearray $Collection$bytearray$new($Sequence wit) {
    return $NEW($Collection$bytearray, wit);
}

$Iterator $Collection$bytearray$__iter__($Collection$bytearray wit, $bytearray str) {
    return $bytearray_iter(str);
}

$bytearray $Collection$bytearray$__fromiter__($Collection$bytearray wit, $Iterable wit2, $WORD iter) {
    return $bytearray_join(to$bytearray(""), wit2, iter);
}

$int $Collection$bytearray$__len__($Collection$bytearray wit, $bytearray str) {
    return $bytearray_len(str);
}

// Times

void $Times$bytearray$__serialize__($Times$bytearray self, $Serial$state state) {
    $step_serialize(self->w$Sequence, state);
}

$Times$bytearray $Times$bytearray$__deserialize__($Times$bytearray self, $Serial$state state) {
    $Times$bytearray res = $DNEW($Times$bytearray, state);
    res->w$Sequence = ($Sequence)$step_deserialize(state);
    return res;
}

$Times$bytearray $Times$bytearray$new($Sequence wit) {
    return $NEW($Times$bytearray, wit);
}

$bytearray $Times$bytearray$__add__($Times$bytearray wit, $bytearray a, $bytearray b) {
    return $bytearray_add(a, b);
}

$bytearray $Times$bytearray$__mul__($Times$bytearray wit, $bytearray a, $int n) {
    return $bytearray_mul(a, n);
}

// Container

void $Container$bytearray$__serialize__($Container$bytearray self, $Serial$state state) {
    $step_serialize(self->w$Eq$A$Container$bytearray, state);
}

$Container$bytearray $Container$bytearray$__deserialize__($Container$bytearray self, $Serial$state state) {
    $Container$bytearray res = $DNEW($Container$bytearray, state);
    res->w$Eq$A$Container$bytearray = ($Eq)$step_deserialize(state);
    return res;
}

$Container$bytearray $Container$bytearray$new($Eq wit) {
    return $NEW($Container$bytearray, wit);
}

$Iterator $Container$bytearray$__iter__($Container$bytearray wit, $bytearray str) {
    return $bytearray_iter(str);
}

$bytearray $Container$bytearray$__fromiter__($Container$bytearray wit, $Iterable wit2, $WORD iter) {
    return $bytearray_join(to$bytearray(""), wit2, iter);
}

$int $Container$bytearray$__len__($Container$bytearray wit, $bytearray str) {
    return $bytearray_len(str);
}

$bool $Container$bytearray$__contains__($Container$bytearray wit, $bytearray self, $int n) {
    return to$bool($bytearray_contains(self, n));
}

$bool $Container$bytearray$__containsnot__($Container$bytearray wit, $bytearray self, $int n) {
    return to$bool(!$bytearray_contains(self, n));
}

// Method tables for witness classes

struct $Sequence$bytearray $Sequence$bytearray_instance;
struct $Collection$bytearray $Collection$bytearray_instance;
struct $Times$bytearray $Times$bytearray_instance;

struct $Ord$bytearray$class $Ord$bytearray$methods = {
    "$Ord$bytearray",
    UNASSIGNED,
    ($Super$class)&$Ord$methods,
    (void (*)($Ord$bytearray))$default__init__,
    $Ord$bytearray$__serialize__,
    $Ord$bytearray$__deserialize__,
    ($bool(*)($Ord$bytearray))$default__bool__,
    ($str(*)($Ord$bytearray))$default__str__,
    $Ord$bytearray$__eq__, $Ord$bytearray$__ne__,
    $Ord$bytearray$__lt__, $Ord$bytearray$__le__,
    $Ord$bytearray$__gt__, $Ord$bytearray$__ge__};
struct $Ord$bytearray $Ord$bytearray_instance = {&$Ord$bytearray$methods};
$Ord$bytearray $Ord$bytearray$witness = &$Ord$bytearray_instance;

struct $Sequence$bytearray$class $Sequence$bytearray$methods = {
    "$Sequence$bytearray",
    UNASSIGNED,
    ($Super$class)&$Sequence$methods,
    $Sequence$bytearray$__init__,
    $Sequence$bytearray$__serialize__,
    $Sequence$bytearray$__deserialize__,
    ($bool(*)($Sequence$bytearray))$default__bool__,
    ($str(*)($Sequence$bytearray))$default__str__,
    $Sequence$bytearray$__getitem__,
    $Sequence$bytearray$__setitem__,
    $Sequence$bytearray$__delitem__,
    $Sequence$bytearray$__getslice__,
    $Sequence$bytearray$__setslice__,
    $Sequence$bytearray$__delslice__,
    $Sequence$bytearray$__reversed__,
    $Sequence$bytearray$insert,
    $Sequence$bytearray$append,
    $Sequence$bytearray$reverse};
struct $Sequence$bytearray $Sequence$bytearray_instance = {
    &$Sequence$bytearray$methods,
    ($Collection)&$Collection$bytearray_instance,
    ($Times)&$Times$bytearray_instance};
$Sequence$bytearray $Sequence$bytearray$witness = &$Sequence$bytearray_instance;

struct $Collection$bytearray$class $Collection$bytearray$methods = {
    "$Collection$bytearray",
    UNASSIGNED,
    ($Super$class)&$Collection$methods,
    $Collection$bytearray$__init__,
    $Collection$bytearray$__serialize__,
    $Collection$bytearray$__deserialize__,
    ($bool(*)($Collection$bytearray))$default__bool__,
    ($str(*)($Collection$bytearray))$default__str__,
    $Collection$bytearray$__iter__,
    $Collection$bytearray$__fromiter__,
    $Collection$bytearray$__len__};
struct $Collection$bytearray $Collection$bytearray_instance = {&$Collection$bytearray$methods, ($Sequence)&$Sequence$bytearray_instance};
$Collection$bytearray $Collection$bytearray$witness = &$Collection$bytearray_instance;

struct $Times$bytearray$class $Times$bytearray$methods = {
    "$Times$bytearray",
    UNASSIGNED,
    ($Super$class)&$Times$methods,
    $Times$bytearray$__init__,
    $Times$bytearray$__serialize__,
    $Times$bytearray$__deserialize__,
    ($bool(*)($Times$bytearray))$default__bool__,
    ($str(*)($Times$bytearray))$default__str__,
    $Times$bytearray$__add__,
    ($bytearray(*)($Times$bytearray, $bytearray, $bytearray))$Plus$__iadd__,
    $Times$bytearray$__mul__,
    ($bytearray(*)($Times$bytearray, $bytearray, $int))$Times$__imul__,
};
struct $Times$bytearray $Times$bytearray_instance = {&$Times$bytearray$methods};
$Times$bytearray $Times$bytearray$witness = &$Times$bytearray_instance;

struct $Container$bytearray$class $Container$bytearray$methods = {
    "$Container$bytearray",
    UNASSIGNED,
    ($Super$class)&$Container$methods,
    $Container$bytearray$__init__,
    $Container$bytearray$__serialize__,
    $Container$bytearray$__deserialize__,
    ($bool(*)($Container$bytearray))$default__bool__,
    ($str(*)($Container$bytearray))$default__str__,
    $Container$bytearray$__iter__,
    $Container$bytearray$__len__,
    $Container$bytearray$__contains__,
    $Container$bytearray$__containsnot__};
struct $Container$bytearray $Container$bytearray_instance = {&$Container$bytearray$methods, ($Eq)&$Ord$bytearray_instance};
$Container$bytearray $Container$bytearray$witness = &$Container$bytearray_instance;

// init methods for witness classes

void $Collection$bytearray$__init__($Collection$bytearray self, $Sequence master) {
    self->w$Sequence = master;
}

void $Times$bytearray$__init__($Times$bytearray self, $Sequence master) {
    self->w$Sequence = master;
}

void $Sequence$bytearray$__init__($Sequence$bytearray self) {
    self->w$Collection = ($Collection)$NEW($Collection$bytearray, ($Sequence)self);
    self->w$Times = ($Times)$NEW($Times$bytearray, ($Sequence)self);
}

void $Container$bytearray$__init__($Container$bytearray wit, $Eq w$Eq$A$Container$bytearray) {
    wit->w$Eq$A$Container$bytearray = w$Eq$A$Container$bytearray;
}

// protocol methods for bytearrays, implementations

// Eq

int $bytearray_eq($bytearray a, $bytearray b) {
    return strcmp((char *)a->str, (char *)b->str) == 0;
}

int $bytearray_neq($bytearray a, $bytearray b) {
    return strcmp((char *)a->str, (char *)b->str) != 0;
}

// Ord

int $bytearray_lt($bytearray a, $bytearray b) {
    return strcmp((char *)a->str, (char *)b->str) < 0;
}

int $bytearray_le($bytearray a, $bytearray b) {
    return strcmp((char *)a->str, (char *)b->str) <= 0;
}

int $bytearray_gt($bytearray a, $bytearray b) {
    return strcmp((char *)a->str, (char *)b->str) > 0;
}

int $bytearray_ge($bytearray a, $bytearray b) {
    return strcmp((char *)a->str, (char *)b->str) >= 0;
}

// Indexed

$int $bytearray_getitem($bytearray self, int ix) {
    int ix0 = ix < 0 ? self->nbytes + ix : ix;
    if (ix0 < 0 || ix0 >= self->nbytes)
        $RAISE(($BaseException)$NEW($IndexError, to$str("getitem for bytearray: indexing outside array")));
    return to$int((long)self->str[ix0]);
}

void $bytearray_setitem($bytearray self, int ix, int val) {
    int ix0 = ix < 0 ? self->nbytes + ix : ix;
    if (ix0 < 0 || ix0 >= self->nbytes)
        $RAISE(($BaseException)$NEW($IndexError, to$str("setitem for bytearray: indexing outside array")));
    if (val < 0 || val > 255)
        $RAISE(($BaseException)$NEW($ValueError, to$str("setitem for bytearray: value outside [0..255]")));
    self->str[ix0] = (unsigned char)val;
}

void $bytearray_delitem($bytearray self, int ix) {
    int len = self->nbytes;
    int ix0 = ix < 0 ? len + ix : ix;
    if (ix0 < 0 || ix0 >= len)
        $RAISE(($BaseException)$NEW($IndexError, to$str("delitem for bytearray: indexing outside array")));
    memmove(self->str + ix0, self->str + (ix0 + 1), len - (ix0 + 1));
    self->nbytes--;
}

// Sliceable

$bytearray $bytearray_getslice($bytearray self, $slice slc) {
    int len = self->nbytes;
    int start, stop, step, slen;
    normalize_slice(slc, len, &slen, &start, &stop, &step);
    $bytearray res;
    NEW_UNFILLED_BYTEARRAY(res, slen);
    int t = start;
    for (int i = 0; i < slen; i++) {
        $int w;
        w = $bytearray_getitem(self, t);
        $bytearray_append(res, w);
        t += step;
    }
    return res;
}

void $bytearray_setslice($bytearray self, $slice slc, $Iterator it) {
    int len = self->nbytes;
    $bytearray other;
    NEW_UNFILLED_BYTEARRAY(other, 0);
    $WORD w;
    while ((w = it->$class->__next__(it)))
        $bytearray_append(other, ($int)w);
    int olen = other->nbytes;
    int start, stop, step, slen;
    normalize_slice(slc, len, &slen, &start, &stop, &step);
    if (step != 1 && olen != slen) {
        $RAISE(($BaseException)$NEW($ValueError, to$str("setslice for bytearray: illegal slice")));
    }
    int copy = olen <= slen ? olen : slen;
    int t = start;
    for (int i = 0; i < copy; i++) {
        self->str[t] = other->str[i];
        t += step;
    }
    if (olen == slen)
        return;
    // now we know that step=1
    if (olen < slen) {
        memmove(self->str + start + copy,
                self->str + start + slen,
                len - (start + slen));
        self->nbytes -= slen - olen;
        return;
    } else {
        expand_bytearray(self, olen - slen);
        int rest = len - (start + copy);
        int incr = olen - slen;
        memmove(self->str + start + copy + incr,
                self->str + start + copy,
                rest);
        for (int i = copy; i < olen; i++)
            self->str[start + i] = other->str[i];
        self->nbytes += incr;
    }
}

void $bytearray_delslice($bytearray self, $slice slc) {
    int len = self->nbytes;
    int start, stop, step, slen;
    normalize_slice(slc, len, &slen, &start, &stop, &step);
    if (slen == 0)
        return;
    unsigned char *p = self->str + start;
    for (int i = 0; i < slen - 1; i++) {
        memmove(p, p + i + 1, step - 1);
        p += step - 1;
    }
    memmove(p, p + slen, len - 1 - (start + step * (slen - 1)));
    self->nbytes -= slen;
    self->str[self->nbytes] = '\0';
}

// Sequence

$Iterator $bytearray_reversed($bytearray self) {
    $bytearray copy = $bytearray_copy(self);
    $bytearray_reverse(copy);
    return $bytearray_iter(copy);
}

void $bytearray_insert($bytearray self, int ix, $int elem) {
    int len = self->nbytes;
    expand_bytearray(self, 1);
    int ix0 = ix < 0 ? (len + ix < 0 ? 0 : len + ix) : (ix < len ? ix : len);
    memmove(self->str + (ix0 + 1),
            self->str + ix0,
            len - ix0 + 1); // +1 to move also terminating '\0'
    self->str[ix0] = (unsigned char)elem->val & 0xff;
    self->nbytes++;
}

void $bytearray_append($bytearray self, $int elem) {
    expand_bytearray(self, 1);
    self->str[self->nbytes++] = (unsigned char)elem->val & 0xff;
    self->str[self->nbytes] = '\0';
}

void $bytearray_reverse($bytearray self) {
    int len = self->nbytes;
    for (int i = 0; i < len / 2; i++) {
        unsigned char tmp = self->str[i];
        self->str[i] = self->str[len - 1 - i];
        self->str[len - 1 - i] = tmp;
    }
}

// Iterable

static $int $Iterator$bytearray_next($Iterator$bytearray self) {
    return self->nxt >= self->src->nbytes ? NULL : to$int(self->src->str[self->nxt++]);
}

void $Iterator$bytearray_init($Iterator$bytearray self, $bytearray b) {
    self->src = b;
    self->nxt = 0;
}

$bool $Iterator$bytearray_bool($Iterator$bytearray self) {
    return $True;
}

$str $Iterator$bytearray_str($Iterator$bytearray self) {
    char *s;
    asprintf(&s, "<bytearray iterator object at %p>", self);
    return to$str(s);
}

void $Iterator$bytearray_serialize($Iterator$bytearray self, $Serial$state state) {
    $step_serialize(self->src, state);
    $step_serialize(to$int(self->nxt), state);
}

$Iterator$bytearray $Iterator$bytearray$_deserialize($Iterator$bytearray res, $Serial$state state) {
    if (!res)
        res = $DNEW($Iterator$bytearray, state);
    res->src = ($bytearray)$step_deserialize(state);
    res->nxt = from$int(($int)$step_deserialize(state));
    return res;
}

struct $Iterator$bytearray$class $Iterator$bytearray$methods = {
    "",
    UNASSIGNED,
    ($Super$class)&$Iterator$methods,
    $Iterator$bytearray_init,
    $Iterator$bytearray_serialize,
    $Iterator$bytearray$_deserialize,
    $Iterator$bytearray_bool,
    $Iterator$bytearray_str,
    $Iterator$bytearray_next};

$Iterator $bytearray_iter($bytearray self) {
    return ($Iterator)$NEW($Iterator$bytearray, self);
}

// Collection

$bytearray $bytearray_fromiter($Iterable wit, $WORD iter) {
    $bytearray res;
    NEW_UNFILLED_BYTEARRAY(res, 0);
    $Iterator it = wit->$class->__iter__(wit, iter);
    $WORD nxt;
    while ((nxt = it->$class->__next__(it))) {
        $bytearray_append(res, ($int)nxt);
    }
    return res;
}

$int $bytearray_len($bytearray self) {
    return to$int(self->nbytes);
}

// Times

$bytearray $bytearray_add($bytearray a, $bytearray b) {
    $bytearray res;
    NEW_UNFILLED_BYTEARRAY(res, a->nbytes + b->nbytes);
    memcpy(res->str, a->str, a->nbytes);
    memcpy(res->str + a->nbytes, b->str, b->nbytes);
    return res;
}

$bytearray $bytearray_mul($bytearray a, $int n) {
    if (n->val <= 0)
        return to$bytearray("");
    else {
        $bytearray res;
        NEW_UNFILLED_BYTEARRAY(res, a->nbytes * n->val);
        for (int i = 0; i < n->val; i++)
            memcpy(res->str + i * a->nbytes, a->str, a->nbytes);
        return res;
    }
}
// Container

int $bytearray_contains($bytearray self, $int c) {
    for (int i = 0; i < self->nbytes; i++) {
        if (self->str[i] == (unsigned char)c->val)
            return 1;
    }
    return 0;
}

int $bytearray_containsnot($bytearray self, $int c) {
    return !$bytearray_contains(self, c);
}

// General methods, implementations

void $bytearray_init($bytearray, $value);
void $bytearray_serialize($bytearray, $Serial$state);
$bytearray $bytearray_deserialize($bytearray, $Serial$state);
$bool $bytearray_bool($bytearray);
$str $bytearray_str($bytearray);

$bytearray $bytearray$new($value s) {
    return $NEW($bytearray, s);
}

void $bytearray_init($bytearray self, $value s) {
    if (!s) {
        self->nbytes = 0;
        self->str = NULL;
        return;
    }
    if ($ISINSTANCE(s, $str)->val) {
        $str str = ($str)s;
        $bytearray b = str->$class->encode(str);
        self->nbytes = b->nbytes;
        self->str = b->str;
    } else if ($ISINSTANCE(s, $list)->val) { // must be a list of ints in range 0..255
        $list lst = ($list)s;
        int len = lst->length;
        self->nbytes = len;
        self->str = malloc(len + 1);
        self->str[len] = 0;
        if (len > 0 && !($ISINSTANCE(lst->data[0], $int)->val))
            $RAISE(($BaseException)$NEW($ValueError, to$str("illegal argument to bytearray constructor")));
        for (int i = 0; i < len; i++) {
            long v = (($int)lst->data[i])->val;
            if (v < 0 || v > 255)
                $RAISE(($BaseException)$NEW($ValueError, to$str("illegal argument to bytearray constructor")));
            self->str[i] = v;
        }
    } else if ($ISINSTANCE(s, $int)->val) {
        $int n = ($int)s;
        if (n->val < 0)
            $RAISE(($BaseException)$NEW($ValueError, to$str("illegal argument to bytearray constructor")));
        self->nbytes = n->val;
        self->str = malloc(n->val);
        memset(self->str, 0, n->val);
    } else
        $RAISE(($BaseException)$NEW($ValueError, to$str("illegal argument to bytearray constructor")));
}

$bool $bytearray_bool($bytearray s) {
    return to$bool(s->nbytes > 0);
};

$str $bytearray_str($bytearray s) {
    $str bs;
    NEW_UNFILLED_STR(bs, s->nbytes, s->nbytes);
    bs->str = s->str;     // bs may not be a correctly UTF8-encoded string
    $str as = $ascii(bs); // but we can use $ascii on it anyhow.
    $str res;
    int n = as->nbytes + 14; // "bytearray(b'" + "')"
    NEW_UNFILLED_STR(res, n, n);
    memcpy(res->str, "bytearray(b'", 12);
    memcpy(&res->str[12], as->str, as->nbytes);
    memcpy(&res->str[n - 2], "')", 2);
    return res;
}

void $bytearray_serialize($bytearray str, $Serial$state state) {
    int nWords = str->nbytes / sizeof($WORD) + 1; // # $WORDS needed to store str->str, including terminating 0.
    $ROW row = $add_header(BYTEARRAY_ID, 1 + nWords, state);
    long nbytes = (long)str->nbytes;
    memcpy(row->blob, &nbytes, sizeof($WORD));
    memcpy(row->blob + 1, str->str, nbytes + 1);
}

$bytearray $bytearray_deserialize($bytearray res, $Serial$state state) {
    $ROW this = state->row;
    state->row = this->next;
    state->row_no++;
    if (!res)
        res = malloc(sizeof(struct $bytearray));
    long nbytes;
    memcpy(&nbytes, this->blob, sizeof($WORD));
    res->$class = &$bytearray$methods;
    res->nbytes = (int)nbytes;
    res->str = malloc(nbytes + 1);
    memcpy(res->str, this->blob + 1, nbytes + 1);
    return res;
}

// End of bytearray implementation ////////////////////////////////////////////////

// Builtin functions involving strings /////////////////////////////////////////////

$str $ascii($str s) {
    unsigned char *hexdigits = (unsigned char *)"0123456789abcdef";
    int printable = 0;
    int escaped = 0; // Backslash, single and double quote
    int non_printable = 0;
    unsigned char c;
    for (int i = 0; i < s->nbytes; i++) {
        c = s->str[i];
        if ((c < 32 || c > 126) && c != '\t' && c != '\r' && c != '\n')
            non_printable++;
        else if (c == '\\' || c == '\'' || c == '"' || c == '\n' || c == '\t' || c == '\r')
            escaped++;
        else
            printable++;
    }
    int nbytes = printable + 2 * escaped + 4 * non_printable;
    $str res;
    NEW_UNFILLED_STR(res, nbytes, nbytes);
    unsigned char *p = res->str;
    for (int i = 0; i < s->nbytes; i++) {
        c = s->str[i];
        if ((c < 32 || c > 126) && c != '\t' && c != '\r' && c != '\n') {
            *p = '\\';
            p++;
            *p = 'x';
            p++;
            *p = hexdigits[c >> 4];
            p++;
            *p = hexdigits[c & 0xf];
            p++;
        } else
            switch (c) {
                case '\\':
                case '\'':
                case '\"':
                    *p = '\\';
                    p++;
                    *p = c;
                    p++;
                    break;
                case '\t':
                    *p = '\\';
                    p++;
                    *p = 't';
                    p++;
                    break;
                case '\n':
                    *p = '\\';
                    p++;
                    *p = 'n';
                    p++;
                    break;
                case '\r':
                    *p = '\\';
                    p++;
                    *p = 'r';
                    p++;
                    break;
                default:
                    *p = c;
                    p++;
            }
    }
    return res;
}

$str $bin($Integral wit, $WORD n) {
    long v = wit->$class->__int__(wit, n)->val;
    int sign = v < 0;
    int nbits = 1;
    unsigned long u = labs(v);
    if (u & 0xffffffff00000000) {
        u >>= 32;
        nbits += 32;
    }
    if (u & 0x00000000ffff0000) {
        u >>= 16;
        nbits += 16;
    }
    if (u & 0x000000000000ff00) {
        u >>= 8;
        nbits += 8;
    }
    if (u & 0x00000000000000f0) {
        u >>= 4;
        nbits += 4;
    }
    if (u & 0x000000000000000c) {
        u >>= 2;
        nbits += 2;
    }
    if (u & 0x0000000000000002) {
        u >>= 1;
        nbits += 1;
    }
    $str res;
    int nbytes = sign + 2 + nbits;
    NEW_UNFILLED_STR(res, nbytes, nbytes);
    unsigned char *p = res->str;
    if (sign) {
        *p = '-';
        p++;
    }
    *p = '0';
    p++;
    *p = 'b';
    p++;
    u = labs(v);
    for (int i = nbits - 1; i >= 0; i--) {
        *p = u & (1L << i) ? '1' : '0';
        p++;
    }
    return res;
}

$str $chr($Integral wit, $WORD n) {
    long v = wit->$class->__int__(wit, n)->val;
    if (v >= 0x110000)
        $RAISE(($BaseException)$NEW($ValueError, to$str("chr: argument is not a valid Unicode code point")));
    unsigned char code[4];
    int nbytes = utf8proc_encode_char((int)v, (unsigned char *)&code);
    if (nbytes == 0)
        $RAISE(($BaseException)$NEW($ValueError, to$str("chr: argument is not a valid Unicode code point")));
    $str res;
    NEW_UNFILLED_STR(res, 1, nbytes);
    for (int i = 0; i < nbytes; i++)
        res->str[i] = code[i];
    return res;
}

$str $hex($Integral wit, $WORD n) {
    unsigned char *hexdigits = (unsigned char *)"0123456789abcdef";
    long v = wit->$class->__int__(wit, n)->val;
    int sign = v < 0;
    int nhexs = 1;
    unsigned long u = labs(v);
    if (u & 0xffffffff00000000) {
        u >>= 32;
        nhexs += 8;
    }
    if (u & 0x00000000ffff0000) {
        u >>= 16;
        nhexs += 4;
    }
    if (u & 0x000000000000ff00) {
        u >>= 8;
        nhexs += 2;
    }
    if (u & 0x00000000000000f0) {
        u >>= 4;
        nhexs += 1;
    }
    $str res;
    int nbytes = sign + 2 + nhexs;
    NEW_UNFILLED_STR(res, nbytes, nbytes);
    unsigned char *p = res->str;
    if (sign) {
        *p = '-';
        p++;
    }
    *p = '0';
    p++;
    *p = 'x';
    p++;
    u = labs(v);
    for (int i = nhexs - 1; i >= 0; i--) {
        *p = hexdigits[(u >> (4 * i)) & 0xf];
        p++;
    }
    return res;
}

$int $ord($str c) {
    if (c->nchars != 1)
        $RAISE(($BaseException)$NEW($ValueError, to$str("ord: argument is not a single Unicode char")));
    int cp;
    int cpnbytes = utf8proc_iterate(c->str, -1, &cp);
    if (cpnbytes < 0)
        $RAISE(($BaseException)$NEW($ValueError, to$str("ord: argument is not a single Unicode char")));
    return to$int(cp);
}

// Auxiliary function used in __str__ for collections ////////////////////////////

$str $str_join_par(char lpar, $list elems, char rpar) {
    char *s = ", ";
    int len = elems->length;
    int totchars = 2; //parens
    int totbytes = 2;
    $str nxt;
    for (int i = 0; i < len; i++) {
        nxt = ($str)elems->data[i];
        totchars += nxt->nchars;
        totbytes += nxt->nbytes;
    }
    if (len > 1) {
        totchars += (len - 1) * 2; // 2 is length of ", "
        totbytes += (len - 1) * 2;
    }
    $str res;
    NEW_UNFILLED_STR(res, totchars, totbytes);
    res->str[0] = lpar;
    res->str[totbytes - 1] = rpar;
    if (len > 0) {
        unsigned char *p = res->str + 1;
        nxt = elems->data[0];
        memcpy(p, nxt->str, nxt->nbytes);
        p += nxt->nbytes;
        for (int i = 1; i < len; i++) {
            nxt = ($str)elems->data[i];
            memcpy(p, s, 2);
            p += 2;
            memcpy(p, nxt->str, nxt->nbytes);
            p += nxt->nbytes;
        }
    }
    return res;
}

$str $default__str__($value self) {
    char *s;
    asprintf(&s, "<%s object at %p>", self->$class->$GCINFO, self);
    return to$str(s);
}
