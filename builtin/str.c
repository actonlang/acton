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

void B_strD_init(B_str, B_value);
B_bool B_strD_bool(B_str);
B_str B_strD_str(B_str);
B_str B_strD_repr(B_str);
void B_strD_serialize(B_str,$Serial$state);
B_str B_strD_deserialize(B_str,$Serial$state);

// String-specific methods
B_str B_strD_capitalize(B_str s);
B_str B_strD_center(B_str s, B_int width, B_str fill);
B_int B_strD_count(B_str s, B_str sub, B_int start, B_int end);
B_bytes B_strD_encode(B_str s);
B_bool B_strD_endswith(B_str s, B_str suffix, B_int start, B_int end);
B_str B_strD_expandtabs(B_str s, B_int tabsize);      
B_int B_strD_find(B_str s, B_str sub, B_int start, B_int end);
B_int B_strD_index(B_str s, B_str sub, B_int start, B_int end);
B_bool B_strD_isalnum(B_str s);
B_bool B_strD_isalpha(B_str s);
B_bool B_strD_isascii(B_str s);
B_bool B_strD_isdecimal(B_str s);
B_bool B_strD_isdigit(B_str s);
B_bool B_strD_isidentifier(B_str s);
B_bool B_strD_islower(B_str s);
B_bool B_strD_isnumeric(B_str s);
B_bool B_strD_isprintable(B_str s);
B_bool B_strD_isspace(B_str s);
B_bool B_strD_istitle(B_str s);
B_bool B_strD_isupper(B_str s);
B_str B_strD_join(B_str sep, B_Iterable wit, $WORD iter);
B_str B_strD_ljust(B_str s, B_int width, B_str fill); 
B_str B_strD_lower(B_str s);
B_str B_strD_lstrip(B_str s,B_str cs); 
B_tuple B_strD_partition(B_str s, B_str sep);
B_str B_strD_replace(B_str s, B_str old, B_str new, B_int count);
B_int B_strD_rfind(B_str s, B_str sub, B_int start, B_int end);
B_int B_strD_rindex(B_str s, B_str sub, B_int start, B_int end);
B_str B_strD_rjust(B_str s, B_int width, B_str fill);  
B_tuple B_strD_rpartition(B_str s, B_str sep); 
B_str B_strD_rstrip(B_str s,B_str cs);
B_list B_strD_split(B_str s, B_str sep, B_int maxsplit);  
B_list B_strD_splitlines(B_str s, B_bool keepends); 
B_bool B_strD_startswith(B_str s, B_str prefix, B_int start, B_int end); 
B_str B_strD_strip(B_str s,B_str cs);
B_str B_strD_upper(B_str s);
B_str B_strD_zfill(B_str s, B_int width);

struct B_strG_class B_strG_methods =
    {"B_str",UNASSIGNED,($SuperG_class)&B_atomG_methods, B_strD_init, B_strD_serialize, B_strD_deserialize, B_strD_bool, B_strD_str, B_strD_repr, B_strD_capitalize, B_strD_center, B_strD_count, B_strD_encode, B_strD_endswith,
     B_strD_expandtabs, B_strD_find, B_strD_index, B_strD_isalnum, B_strD_isalpha, B_strD_isascii, B_strD_isdecimal, B_strD_islower, B_strD_isprintable, B_strD_isspace,
     B_strD_istitle, B_strD_isupper, B_strD_join, B_strD_ljust, B_strD_lower, B_strD_lstrip, B_strD_partition, B_strD_replace, B_strD_rfind, B_strD_rindex, B_strD_rjust,
     B_strD_rpartition, B_strD_rstrip, B_strD_split, B_strD_splitlines, B_strD_startswith, B_strD_strip, B_strD_upper, B_strD_zfill};


// protocol methods; string implementation prototypes ///////////////////////////////////////////////////

int B_strD_eq(B_str,B_str);
int B_strD_neq(B_str,B_str);
int B_strD_lt(B_str,B_str);
int B_strD_le(B_str,B_str);
int B_strD_gt(B_str,B_str);
int B_strD_ge(B_str,B_str);

B_Iterator B_strD_iter(B_str);

B_str B_strD_fromiter(B_Iterable, $WORD);
B_int B_strD_len(B_str str);

int B_strD_contains (B_str, B_str);
int B_strD_containsnot (B_str, B_str);

B_str B_strD_getitem(B_str, int);
B_str B_strD_getslice(B_str, B_slice);
 
B_str B_strD_add(B_str, B_str);
B_str B_strD_mul(B_str, B_int);

// Protocol instances, using above prototypes 

// Ord

void B_OrdD_strD___serialize__(B_OrdD_str self, $Serial$state state) {
}

B_OrdD_str B_OrdD_strD___deserialize__(B_OrdD_str self, $Serial$state state) {
    B_OrdD_str res = $DNEW(B_OrdD_str,state);
    return res;
}

B_OrdD_str B_OrdD_strG_new() {
    return $NEW(B_OrdD_str);
}

B_bool B_OrdD_strD___eq__ (B_OrdD_str wit, B_str a, B_str b) {
    return toB_bool(B_strD_eq(a,b));
}

B_bool B_OrdD_strD___ne__ (B_OrdD_str wit, B_str a, B_str b) {
    return  toB_bool(B_strD_neq(a,b));
}

B_bool B_OrdD_strD___lt__ (B_OrdD_str wit, B_str a, B_str b) {
    return toB_bool(B_strD_lt(a,b));
}

B_bool B_OrdD_strD___le__ (B_OrdD_str wit, B_str a, B_str b){
    return toB_bool(B_strD_le(a,b));
}

B_bool B_OrdD_strD___gt__ (B_OrdD_str wit, B_str a, B_str b){
    return toB_bool(B_strD_gt(a,b));
}

B_bool B_OrdD_strD___ge__ (B_OrdD_str wit, B_str a, B_str b){
    return toB_bool(B_strD_ge(a,b));
}

// Container

void B_ContainerD_strD___serialize__(B_ContainerD_str self, $Serial$state state) {
}

B_ContainerD_str B_ContainerD_strD___deserialize__(B_ContainerD_str self, $Serial$state state) {
    return $DNEW(B_ContainerD_str,state);
}

B_Iterator B_ContainerD_strD___iter__ (B_ContainerD_str wit, B_str str) {
    return B_strD_iter(str);
}

B_str B_ContainerD_strD___fromiter__ (B_ContainerD_str wit, B_Iterable wit2, $WORD iter) {
    return B_strD_join(to$str(""),wit2,iter);
}

B_int B_ContainerD_strD___len__ (B_ContainerD_str wit, B_str str) {
    return B_strD_len(str);
}

B_bool B_ContainerD_strD___contains__ (B_ContainerD_str wit, B_str str, B_str sub) {
    return toB_bool(B_strD_contains(str, sub));
}

B_bool B_ContainerD_strD___containsnot__ (B_ContainerD_str wit, B_str str, B_str sub) {
    return toB_bool(B_strD_containsnot(str, sub));
}  

// Sliceable

void B_SliceableD_strD___serialize__(B_SliceableD_str self, $Serial$state state) {
}

B_SliceableD_str B_SliceableD_strD___deserialize__(B_SliceableD_str self, $Serial$state state) {
    B_SliceableD_str res = $DNEW(B_SliceableD_str,state);
    return res;
}

B_SliceableD_str B_SliceableD_strG_new() {
    return $NEW(B_SliceableD_str);
}
B_str B_SliceableD_strD___getitem__ (B_SliceableD_str wit, B_str str, B_int i) {
    return B_strD_getitem(str,fromB_int(i));
}

void B_SliceableD_strD___setitem__ (B_SliceableD_str wit, B_str str, B_int i, B_str val) {
    fprintf(stderr,"Internal error: call to mutating method setitem on string");
    exit(-1);
}

void B_SliceableD_strD___delitem__ (B_SliceableD_str wit, B_str str, B_int i) {
    fprintf(stderr,"Internal error: call to mutating method delitem on string");
    exit(-1);
}

B_str B_SliceableD_strD___getslice__ (B_SliceableD_str wit, B_str str, B_slice slc) {
    return B_strD_getslice(str,slc);
}

void B_SliceableD_strD___setslice__ (B_SliceableD_str wit, B_str str, B_Iterable wit2, B_slice slc, $WORD iter) {
    fprintf(stderr,"Internal error: call to mutating method setslice on string");
    exit(-1);
}

void B_SliceableD_strD___delslice__ (B_SliceableD_str wit, B_str str, B_slice slc) {
    fprintf(stderr,"Internal error: call to mutating method delslice on string");
    exit(-1);
}

// Times

void B_TimesD_strD___serialize__(B_TimesD_str self, $Serial$state state) {
}

B_TimesD_str B_TimesD_strD___deserialize__(B_TimesD_str self, $Serial$state state) {
    B_TimesD_str res = $DNEW(B_TimesD_str,state);
    return res;
}

B_str B_TimesD_strD___add__ (B_TimesD_str wit, B_str a, B_str b) {
    return B_strD_add(a,b);
}

B_str B_TimesD_strD___mul__ (B_TimesD_str wit, B_str a, B_int n) {
    return B_strD_mul(a,n);
}

// Hashable

void B_HashableD_strD___serialize__(B_HashableD_str self, $Serial$state state) {
}

B_HashableD_str B_HashableD_strD___deserialize__(B_HashableD_str self, $Serial$state state) {
    B_HashableD_str res = $DNEW(B_HashableD_str,state);
    return res;
}

B_bool B_HashableD_strD___eq__ (B_HashableD_str wit, B_str a, B_str b) {
    return toB_bool(B_strD_eq(a,b));
}

B_HashableD_str B_HashableD_strG_new() {
    return $NEW(B_HashableD_str);
}
B_bool B_HashableD_strD___ne__ (B_HashableD_str wit, B_str a, B_str b) {
    return toB_bool(B_strD_neq(a,b));
}

B_int B_HashableD_strD___hash__(B_HashableD_str wit, B_str str) {
    return toB_int(B_string_hash(str));
}


// Method tables for witness classes

struct B_OrdD_strG_class  B_OrdD_strG_methods = {
    "B_OrdD_str",
    UNASSIGNED,
    ($SuperG_class)&B_OrdG_methods,
    (void (*)(B_OrdD_str))$default__init__,
    B_OrdD_strD___serialize__,
    B_OrdD_strD___deserialize__,
    (B_bool (*)(B_OrdD_str))$default__bool__,
    (B_str (*)(B_OrdD_str))$default__str__,
    (B_str (*)(B_OrdD_str))$default__str__,
    B_OrdD_strD___eq__,
    B_OrdD_strD___ne__,
    B_OrdD_strD___lt__,
    B_OrdD_strD___le__,
    B_OrdD_strD___gt__,
    B_OrdD_strD___ge__
};
struct B_OrdD_str B_OrdD_str_instance = {&B_OrdD_strG_methods};
B_OrdD_str B_OrdD_strG_witness = &B_OrdD_str_instance;

struct B_ContainerD_strG_class  B_ContainerD_strG_methods = {
    "B_ContainerD_str",
    UNASSIGNED,
    ($SuperG_class)&B_ContainerG_methods,
    B_ContainerD_strD___init__,
    B_ContainerD_strD___serialize__,
    B_ContainerD_strD___deserialize__,
    (B_bool (*)(B_ContainerD_str))$default__bool__,
    (B_str (*)(B_ContainerD_str))$default__str__,
    (B_str (*)(B_ContainerD_str))$default__str__,
    B_ContainerD_strD___iter__,
    NULL,
    B_ContainerD_strD___len__,
    B_ContainerD_strD___contains__,
    B_ContainerD_strD___containsnot__
};
struct B_ContainerD_str B_ContainerD_str_instance = {&B_ContainerD_strG_methods};
B_ContainerD_str B_ContainerD_strG_witness = &B_ContainerD_str_instance;


struct B_SliceableD_strG_class  B_SliceableD_strG_methods = {
    "B_SliceableD_str",
    UNASSIGNED,
    ($SuperG_class)&B_SliceableG_methods,
    (void (*)(B_SliceableD_str))$default__init__,
    B_SliceableD_strD___serialize__,
    B_SliceableD_strD___deserialize__,
    (B_bool (*)(B_SliceableD_str))$default__bool__,
    (B_str (*)(B_SliceableD_str))$default__str__,
    (B_str (*)(B_SliceableD_str))$default__str__,
    B_SliceableD_strD___getitem__,
    B_SliceableD_strD___setitem__,
    B_SliceableD_strD___delitem__,
    B_SliceableD_strD___getslice__,
    B_SliceableD_strD___setslice__,
    B_SliceableD_strD___delslice__
};
struct B_SliceableD_str B_SliceableD_str_instance = {&B_SliceableD_strG_methods};
B_SliceableD_str B_SliceableD_strG_witness = &B_SliceableD_str_instance;

struct B_TimesD_strG_class  B_TimesD_strG_methods = {
    "B_TimesD_str",
    UNASSIGNED,
    ($SuperG_class)&B_TimesG_methods,
    (void (*)(B_TimesD_str))$default__init__,
    B_TimesD_strD___serialize__,
    B_TimesD_strD___deserialize__,
    (B_bool (*)(B_TimesD_str))$default__bool__,
    (B_str (*)(B_TimesD_str))$default__str__,
    (B_str (*)(B_TimesD_str))$default__str__,
    B_TimesD_strD___add__,
    (B_str (*)(B_TimesD_str, B_str, B_str))$PlusD___iadd__,
    B_TimesD_strD___mul__,
    (B_str (*)(B_TimesD_str, B_str, B_int))B_TimesD___imul__,

};
struct B_TimesD_str B_TimesD_str_instance = {&B_TimesD_strG_methods};
B_TimesD_str B_TimesD_strG_witness = &B_TimesD_str_instance;

struct B_HashableD_strG_class  B_HashableD_strG_methods = {
    "B_HashableD_str",
    UNASSIGNED,
    ($SuperG_class)&B_HashableG_methods,
    (void (*)(B_HashableD_str))$default__init__,
    B_HashableD_strD___serialize__,
    B_HashableD_strD___deserialize__,
    (B_bool (*)(B_HashableD_str))$default__bool__,
    (B_str (*)(B_HashableD_str))$default__str__,
    (B_str (*)(B_HashableD_str))$default__str__,
    B_HashableD_strD___eq__,
    B_HashableD_strD___ne__,
    B_HashableD_strD___hash__
};
struct B_HashableD_str B_HashableD_str_instance = {&B_HashableD_strG_methods};
B_HashableD_str B_HashableD_strG_witness = &B_HashableD_str_instance;

 
void B_ContainerD_strD___init__ (B_ContainerD_str wit) {
}

// Auxiliaries, some used for both str and bytearray implementations ////////////////////////////////////////////////////////

static unsigned char nul = 0;

static struct B_str null_struct = {&B_strG_methods,0,0,&nul};

static B_str null_str = &null_struct;

static struct B_str space_struct = {&B_strG_methods,1,1,(unsigned char *)" "};

static B_str space_str = &space_struct;

static struct B_str whitespace_struct = {&B_strG_methods,6,6,(unsigned char *)" \t\n\r\x0b\x0c"};

static B_str whitespace_str = &whitespace_struct;

#define NEW_UNFILLED_STR(nm,nchrs,nbtes)        \
    nm = malloc(sizeof(struct B_str));           \
    (nm)->$class = &B_strG_methods;               \
    (nm)->nchars = nchrs;                       \
    (nm)->nbytes = nbtes;                       \
    (nm)->str = malloc((nm)->nbytes + 1);       \
    (nm)->str[(nm)->nbytes] = 0

#define NEW_UNFILLED_BYTEARRAY(nm,nbtes)        \
    nm = malloc(sizeof(struct B_bytearray));     \
    (nm)->$class = &B_bytearrayG_methods;         \
    (nm)->nbytes = nbtes;                       \
    (nm)->capacity = nbtes;                     \
    (nm)->str = malloc((nm)->nbytes + 1);       \
    (nm)->str[(nm)->nbytes] = 0

#define NEW_UNFILLED_BYTES(nm,nbtes)            \
    nm = malloc(sizeof(struct B_bytes));         \
    (nm)->$class = &B_bytesG_methods;             \
    (nm)->nbytes = nbtes;                       \
    (nm)->str = malloc(nbtes + 1);              \
    (nm)->str[nbtes] = 0

// Conversion to and from C strings

B_str to$str(char *str) { 
    int nbytes = 0;
    int nchars = 0;

    unsigned char *p = (unsigned char*)str;
    int cp, cpnbytes;
    while(1) {
        if (*p == '\0') {
            B_str res;
            NEW_UNFILLED_STR(res,nchars, nbytes);
            memcpy(res->str,str,nbytes);
            return res;
        }
        cpnbytes = utf8proc_iterate(p,-1,&cp);
        if (cpnbytes < 0) {
            $RAISE((B_BaseException)$NEW(B_ValueError,to$str("to$str: Unicode decode error")));
            return NULL;
        }
        nbytes += cpnbytes;
        nchars++;
        p += cpnbytes;
    }
}

unsigned char *fromB_str(B_str str) {
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
static B_str str_transform(B_str s, transform f) {
    int cp, cpu, cplen, cpulen;
    unsigned char *p = s->str;
    unsigned char buffer[4*s->nchars];
    unsigned char *up = buffer;
    for (int i=0; i < s->nchars; i++) {
        cplen = utf8proc_iterate(p,-1,&cp);
        cpu = f(cp);
        cpulen = utf8proc_encode_char(cpu,up);
        p+=cplen;
        up += cpulen;
    }
    int nbytes = (int)(up-buffer);
    B_str res;
    NEW_UNFILLED_STR(res,s->nchars,nbytes);
    memcpy(res->str,buffer,nbytes);
    return res;
}

// Find char position in text from byte position.
// Assume that i is first byte of a char in text.
static int char_no(B_str text,int i) {
    if (text->nbytes == text->nchars) // ASCII string
        return i;
    int res = 0;
    int k=0;
    unsigned char *t = text->str;
    while (k<i) {
        k += byte_length2(t[k]);
        res++;
    }
    return res;
}

static unsigned char *skip_chars(unsigned char* start,int n, int isascii) {
    unsigned char *res = start;
    if (isascii)
        return start+n;
    if (n >= 0) {
        for (int i=0; i<n; i++)
            res += byte_length2(*res);
    } else {
        for (int i= 0; i<-n; i++) {
            res--;
            while (*res >> 6 == 2) res--;
        }
    }
    return res;
}


// Find byte position in text from char position.
// Assume i is a valid char index in text
static int byte_no(B_str text, int i) {
    int res = 0;
    unsigned char *t = text->str;
    for (int k=0; k<i; k++)
        res += byte_length2(t[k]);
    return res;
}

// Handles negative indices in getitem etc (slice notation) 
static int get_index(int i, int nchars) {
    if (i >= 0) {
        if (i<nchars)
            return i;
    } else {
        if (i >= -nchars)
            return nchars+i;
    }
    $RAISE((B_BaseException)$NEW(B_IndexError,to$str("indexing outside str")));
    return 0;
}

 
// Eliminates slice notation in find, index, count and other methods
// with optional start and end and adds defaults for omitted parameters.

static int fix_start_end(int nchars, B_int *start, B_int *end) {
    if (*start==NULL) {
        *start = malloc(sizeof(struct B_int));
        *start = toB_int(0);
    }
    int st = fromB_int(*start);
    if (st > nchars)
        return -1;
    if (st < 0) 
        st += nchars;
    st = st < 0 ? 0 : st;
    *start = toB_int(st);

    if (*end==NULL) {
        *end = malloc(sizeof(struct B_int));
        *end = toB_int(nchars);
    }
    int en = fromB_int(*end);
    if (en > nchars)   
        en = nchars;      
    else if (en < 0) 
        en += nchars;     
    en = en < 0 ? 0 : en;    

    *end = toB_int(en);
    return 0;
}

// Builds a new one-char string starting at p.
static B_str mk_char(unsigned char *p) {
    B_str res;
    NEW_UNFILLED_STR(res,1,byte_length2(*p));
    for (int i=0; i<res->nbytes; i++)
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
int bmh( unsigned char *text, unsigned char *pattern, int tbytes, int pbytes) {
    if (pbytes>tbytes) return -1;
    int skip[256];
    for (int i=0; i<256; i++) skip[i] = pbytes;
    for (int i=0; i<pbytes-1; i++)
        skip[(int)pattern[i]] = pbytes-i-1;
    int k = pbytes-1;
    int i, j;
    while (k<tbytes) {
        j = pbytes-1;
        i = k;
        while (j >=0 && text[i] == pattern[j]) {
            j--; i--;
        }
        if (j==-1) return i+1;
        k += skip[(int)text[k]];
    }
    return -1;
}

// Start search from the right end of text.
static int rbmh( unsigned char *text, unsigned char *pattern, int tbytes, int pbytes) {
    if (pbytes>tbytes) return -1;
    int skip[256];
    for (int i=0; i<256; i++) skip[i] = pbytes;
    for (int i=pbytes-1; i>0; i--)
        skip[(int)pattern[i]] = i;
    int k = tbytes - pbytes;
    int i, j;
    while (k >= 0) {
        j = 0;
        i = k;
        while (j < pbytes && text[i] == pattern[j]) {
            j++; i++;
        }
        if (j==pbytes) return i-pbytes;
        k -= skip[(int)text[k]];
    }
    return -1;
}

// Protocol methods; string implementations /////////////////////////////////////////////////////////////////////////////
/* 
   Note: We make str instances for Indexed and Sliceable even though these protocols 
   include mutating methods. 
*/

// B_Ord ///////////////////////////////////////////////////////////////////////////////////////////////


// TODO: We should consider how to normalize strings before comparisons

int B_strD_eq(B_str a, B_str b) {
    return (strcmp((char *)a->str,(char *)b->str)==0);
}
         
int B_strD_neq(B_str a, B_str b) {
    return !B_strD_eq(a,b);
}

// The comparisons below do lexicographic byte-wise comparisons.
// Thus they do not in general reflect locale-dependent order conventions.
 
int B_strD_lt(B_str a, B_str b) {
    return (strcmp((char *)a->str,(char *)b->str) < 0);
}
 
int B_strD_le(B_str a, B_str b) {
    return (strcmp((char *)a->str,(char *)b->str) <= 0);
}
 
int B_strD_gt(B_str a, B_str b) {
    return (strcmp((char *)a->str,(char *)b->str) > 0);
}
 
int B_strD_ge(B_str a, B_str b) {
    return (strcmp((char *)a->str,(char *)b->str) >= 0);
}
 
// B_Hashable ///////////////////////////////////////////////////////////////////////////////////

// hash function B_string_hash defined in hash.c

// B_Times /////////////////////////////////////////////////////////////////////////////////////////////

B_TimesD_str B_TimesD_strG_new() {
    return $NEW(B_TimesD_str);
}
 
B_str B_strD_add(B_str s, B_str t) {
    B_str res;
    NEW_UNFILLED_STR(res,s->nchars + t->nchars,s->nbytes + t->nbytes);
    memcpy(res->str,s->str,s->nbytes);
    memcpy(res->str+s->nbytes,t->str,t->nbytes);
    return res;
}

B_str B_strD_mul (B_str a, B_int n) {
    int nval = fromB_int(n);
    if (nval <= 0)
        return to$str("");
    else {
        B_str res;
        NEW_UNFILLED_STR(res,a->nchars * nval, a->nbytes * nval);
        for (int i=0; i<nval; i++)
            memcpy(res->str + i*a->nbytes,a->str,a->nbytes);
        return res;
    }
}

// Collection ///////////////////////////////////////////////////////////////////////////////////////

B_int B_strD_len(B_str s) {
    B_int res = toB_int(s->nchars);
    return res;
}

// B_Container ///////////////////////////////////////////////////////////////////////////

 
B_ContainerD_str B_ContainerD_strG_new() {
    return $NEW(B_ContainerD_str);
}

int B_strD_contains(B_str s, B_str sub) {
    return bmh(s->str,sub->str,s->nbytes,sub->nbytes) > 0;
}

int B_strD_containsnot(B_str s, B_str sub) {
    return !B_strD_contains(s,sub);
}

// Iterable ///////////////////////////////////////////////////////////////////////////

B_IteratorB_str B_IteratorB_strG_new(B_str str) {
    return $NEW(B_IteratorB_str, str);
}

void B_IteratorB_strD_init(B_IteratorB_str self, B_str str) {
    self->src = str;
    self->nxt = 0;
}

void B_IteratorB_strD_serialize(B_IteratorB_str self,$Serial$state state) {
    $step_serialize(self->src,state);
    $step_serialize(toB_int(self->nxt),state);
}


B_IteratorB_str B_IteratorB_str$_deserialize(B_IteratorB_str res, $Serial$state state) {
    if (!res)
        res = $DNEW(B_IteratorB_str,state);
    res->src = (B_str)$step_deserialize(state);
    res->nxt = fromB_int((B_int)$step_deserialize(state));
    return res;
}

B_bool B_IteratorB_strD_bool(B_IteratorB_str self) {
    return $True;
}

B_str B_IteratorB_strD_str(B_IteratorB_str self) {
    char *s;
    asprintf(&s,"<str iterator object at %p>",self);
    return to$str(s);
}

// this is next function for forward iteration
static B_str B_IteratorB_strD_next(B_IteratorB_str self) {
    unsigned char *p = &self->src->str[self->nxt];
    if (*p != 0) {
        self->nxt +=byte_length2(*p);
        return mk_char(p);
    }
    return NULL;
}

B_Iterator B_strD_iter(B_str str) {
    return (B_Iterator)$NEW(B_IteratorB_str,str);
}

struct B_IteratorB_strG_class B_IteratorB_strG_methods = {"B_IteratorB_str",UNASSIGNED,($SuperG_class)&B_IteratorG_methods, B_IteratorB_strD_init,
                                                    B_IteratorB_strD_serialize, B_IteratorB_str$_deserialize,
                                                    B_IteratorB_strD_bool, B_IteratorB_strD_str, B_IteratorB_strD_str, B_IteratorB_strD_next};


// Indexed ///////////////////////////////////////////////////////////////////////////

B_str B_strD_getitem(B_str s, int i) {
    unsigned char *p = s->str;
    int ix = get_index(i,s->nchars);
    p = skip_chars(p,ix,s->nchars == s->nbytes);
    return mk_char(p);
}
 
// Sliceable //////////////////////////////////////////////////////////////////////////////////////

B_str B_strD_getslice(B_str s, B_slice slc) {
    int isascii = s->nchars == s->nbytes;
    int nchars = s->nchars;
    int nbytes = 0;
    int start, stop, step, slen;
    normalize_slice(slc, nchars, &slen, &start, &stop, &step);
    //slice notation have been eliminated and default values applied.
    unsigned char buffer[4*slen]; // very conservative buffer size.
    unsigned char *p = buffer;
    unsigned char *t = skip_chars(s->str,start,isascii);
    for (int i=0; i<slen; i++) {
        int bytes = byte_length2(*t);
        for (int k=0; k<bytes;k++) {
            p[nbytes] = *t;
            t++; nbytes++;
        }
        t = skip_chars(t,step-1,isascii);
    }
    B_str res;
    NEW_UNFILLED_STR(res,slen,nbytes);
    if (nbytes > 0)
        memcpy(res->str,buffer,nbytes);
    return res;
}



// General methods ////////////////////////////////////////////////////////////// 

B_str B_strG_new(B_value s) {
    return $NEW(B_str, s);
}

void B_strD_init(B_str self, B_value s) {
    B_str res = s->$class->__str__(s);
    self->nchars = res->nchars;
    self->nbytes = res->nbytes;
    self->str = res->str;
}

B_bool B_strD_bool(B_str s) {
    return toB_bool(s->nchars > 0);
};

B_str B_strD_str(B_str s) {
    return s;
}

B_str B_strD_repr(B_str s) {
    B_str $res = NEW_UNFILLED_STR($res,s->nchars+2,s->nbytes+2);
    $res->str[0] = '"';
    $res->str[$res->nbytes-1] = '"';
    memcpy($res->str+1, s->str,s->nbytes);
    return $res;
}


void B_strD_serialize(B_str str,$Serial$state state) {
    int nWords = str->nbytes/sizeof($WORD) + 1;         // # $WORDS needed to store str->str, including terminating 0.
    $ROW row = $add_header(STR_ID,2+nWords,state);
    long nbytes = (int)str->nbytes;                    // We could pack nbytes and nchars in one $WORD, 
    memcpy(row->blob,&nbytes,sizeof($WORD));            // but we should think of a better, general approach.
    long nchars = (int)str->nchars;
    memcpy(row->blob+1,&nchars,sizeof($WORD));
    memcpy(row->blob+2,str->str,nbytes+1);
}

B_str B_strD_deserialize(B_str self, $Serial$state state) {
    $ROW this = state->row;
    state->row =this->next;
    state->row_no++;
    B_str res = malloc(sizeof(struct B_str));
    long nbytes;
    memcpy(&nbytes,this->blob,sizeof($WORD));
    res->$class = &B_strG_methods;
    res->nbytes = (int)nbytes;
    long nchars;
    memcpy(&nchars,this->blob+1,sizeof($WORD));
    res->nchars = (int)nchars;
    res->str = malloc(nbytes+1);
    memcpy(res->str,this->blob+2,nbytes+1);
    return res;
}

 
// str-specific methods ////////////////////////////////////////////////////////

B_str B_strD_capitalize(B_str s) {
    if (s->nchars==0) {
        return null_str;
    }
    int cp, cpu, cplen, cpulen;
    unsigned char *p = s->str;
    unsigned char buffer[4*s->nchars];
    unsigned char *up = buffer;
    for (int i=0; i < s->nchars; i++) {
        cplen = utf8proc_iterate(p,-1,&cp);
        cpu = i==0? utf8proc_totitle(cp) : utf8proc_tolower(cp);
        cpulen = utf8proc_encode_char(cpu,up);
        p+=cplen;
        up += cpulen;
    }
    long nbytes = (long)(up-buffer);
    B_str res;
    NEW_UNFILLED_STR(res,s->nchars,nbytes);
    memcpy(res->str,buffer,nbytes);
    return res;
}

B_str B_strD_center(B_str s, B_int width, B_str fill) {
    int wval = fromB_int(width);
    if (!fill)
        fill = space_str;
    if (fill->nchars != 1) {
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("center: fill string not single char")));
    }
    if (wval <= s->nchars) {
        return s;
    }
    int pad = (wval-s->nchars);
    int padleft = pad/2; // Below we make use of the fact padright >= padleft.
    int padright = pad-padleft;
    int fillbytes = fill->nbytes;
    int sbytes = s->nbytes;
    B_str res;
    NEW_UNFILLED_STR(res, wval,sbytes+pad*fillbytes);
    unsigned char *c = fill->str;
    unsigned char *p = res->str;
    p += padleft*fillbytes+sbytes;
    for (int i = 0; i<padright; i++) {
        for (int j = 0; j < fillbytes; j++) 
            p[j] = c[j];
        p += fillbytes;
    }
    p -= padright*fillbytes;
    memcpy(res->str,p,padleft*fillbytes);
    p -= sbytes;
    memcpy(p,s->str,sbytes);
    return res;
}


B_int B_strD_count(B_str s, B_str sub, B_int start, B_int end) {
    int isascii = s->nchars == s->nbytes;
    B_int st = start;
    B_int en = end;
    if (fix_start_end(s->nchars,&st,&en) < 0) return toB_int(0);
    unsigned char *p = skip_chars(s->str,fromB_int(st),isascii);
    unsigned char *q = skip_chars(p,fromB_int(en)-fromB_int(st),isascii);
    int res = 0;
    int n = bmh(p,sub->str,q-p,sub->nbytes);
    while (n>=0) {
        res++;
        p += n + (sub->nbytes>0 ? sub->nbytes : 1);
        n = bmh(p,sub->str,q-p,sub->nbytes);
    }
    return toB_int(res);
}

B_bytes B_strD_encode(B_str s) {
    B_bytes res;
    NEW_UNFILLED_BYTES(res,s->nbytes);
    memcpy(res->str,s->str,s->nbytes);
    return res;
}

B_bool B_strD_endswith(B_str s, B_str sub, B_int start, B_int end) {
    B_int st = start;
    B_int en = end;
    if (fix_start_end(s->nchars,&st,&en) < 0) return $False;
    int isascii = s->nchars==s->nbytes;
    unsigned char *p = skip_chars(s->str + s->nbytes,fromB_int(en) - s->nchars,isascii) - sub->nbytes;
    unsigned char *q = sub->str;
    for (int i=0; i<sub->nbytes; i++) {
        if (*p == 0 || *p++ != *q++) {
            return $False;
        }
    }
    return $True;
}

B_str B_strD_expandtabs(B_str s, B_int tabsize){
    int tabsz = tabsize?fromB_int(tabsize):8;
    int pos = 0;
    int expanded = 0;
    tabsz = tabsz <= 0 ? 1 : tabsz;
    unsigned char buffer[tabsz * s->nchars];
    unsigned char *p = s->str;
    unsigned char *q = buffer;
    for (int i=0; i<s->nchars; i++) {
        if (*p == '\t') {
            int n = tabsz - pos % tabsz;
            for (int j=0; j < n; j++) {
                *q++ = ' ';
            }
            p++;
            expanded += n-1;
            pos+=n;
        } else if (*p=='\n' || *p == '\r') {
            *q++ = *p++;
            pos = 0;
        } else {
            for (int j=0; j< byte_length2(*p); j++) {
                *q++ = *p++;
                pos++;
            }
        }
    }
    B_str res;
    NEW_UNFILLED_STR(res,s->nchars+expanded,s->nbytes+expanded);
    memcpy(res->str,buffer,s->nbytes+expanded);
    return res;
}

B_int B_strD_find(B_str s, B_str sub, B_int start, B_int end) {
    int isascii = s->nchars == s->nbytes;
    B_int st = start;
    B_int en = end;
    if (fix_start_end(s->nchars,&st,&en) < 0) return toB_int(-1);
    unsigned char *p = skip_chars(s->str,fromB_int(st),isascii);
    unsigned char *q = skip_chars(p,fromB_int(en)-fromB_int(st),isascii);
    int n = bmh(p,sub->str,q-p,sub->nbytes);
    if (n<0) return toB_int(-1);
    return toB_int(char_no(s,n+p-s->str));
}

B_int B_strD_index(B_str s, B_str sub, B_int start, B_int end) {
    B_int n = B_strD_find(s,sub,start,end);
    if (fromB_int(n)<0) {
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("index: substring not found")));
    }
    return n;
}

B_bool B_strD_isalnum(B_str s) {
    unsigned char *p = s->str;
    int codepoint;
    int nbytes;
    if (s->nchars == 0)
        return $False;
    for (int i=0; i < s->nchars; i++) {
        nbytes = utf8proc_iterate(p,-1,&codepoint);
        utf8proc_category_t cat = utf8proc_category(codepoint);
        if ((cat <  UTF8PROC_CATEGORY_LU || cat >  UTF8PROC_CATEGORY_LO) && cat != UTF8PROC_CATEGORY_ND)
            return $False;
        p += nbytes;
    }
    return $True;
}

B_bool B_strD_isalpha(B_str s) {
    unsigned char *p = s->str;
    int codepoint;
    int nbytes;
    if (s->nchars == 0)
        return $False;
    for (int i=0; i < s->nchars; i++) {
        nbytes = utf8proc_iterate(p,-1,&codepoint);
        utf8proc_category_t cat = utf8proc_category(codepoint);
        if (cat <  UTF8PROC_CATEGORY_LU || cat >  UTF8PROC_CATEGORY_LO)
            return $False;
        p += nbytes;
    }
    return $True;
}

B_bool B_strD_isascii(B_str s) {
    unsigned char *p = s->str;
    for (int i=0; i < s->nbytes; i++) {
        if (*p > 127)
            return $False;
        p++;
    }
    return $True;
}

B_bool B_strD_isdecimal(B_str s) {
    unsigned char *p = s->str;
    int codepoint;
    int nbytes;
    if (s->nchars == 0)
        return $False;
    for (int i=0; i < s->nchars; i++) {
        nbytes = utf8proc_iterate(p,-1,&codepoint);
        utf8proc_category_t cat = utf8proc_category(codepoint);
        if (cat != UTF8PROC_CATEGORY_ND)
            return $False;
        p += nbytes;
    }
    return $True;
}

B_bool B_strD_islower(B_str s) {
    unsigned char *p = s->str;
    int codepoint;
    int nbytes;
    int has_cased = 0;
    if (s->nchars == 0)
        return $False;
    for (int i=0; i < s->nchars; i++) {
        nbytes = utf8proc_iterate(p,-1,&codepoint);
        utf8proc_category_t cat = utf8proc_category(codepoint);
        if (cat == UTF8PROC_CATEGORY_LT|| cat == UTF8PROC_CATEGORY_LU)
            return $False;
        if (cat == UTF8PROC_CATEGORY_LL)
            has_cased = 1;
        p += nbytes;
    }
    return toB_bool(has_cased);
}

B_bool B_strD_isprintable(B_str s) {
    unsigned char *p = s->str;
    int codepoint;
    int nbytes;
    if (s->nchars == 0)
        return $False;
    for (int i=0; i < s->nchars; i++) {
        nbytes = utf8proc_iterate(p,-1,&codepoint);
        utf8proc_category_t cat = utf8proc_category(codepoint);
        if (cat >= UTF8PROC_CATEGORY_ZS && codepoint != 0x20)
            return $False;
        p += nbytes;
    }
    return $True;
}

B_bool B_strD_isspace(B_str s) {
    unsigned char *p = s->str;
    int codepoint;
    int nbytes;
    if (s->nchars == 0)
        return $False;
    for (int i=0; i < s->nchars; i++) {
        nbytes = utf8proc_iterate(p,-1,&codepoint);
        if (!isspace_codepoint(codepoint))
            return $False;
        p += nbytes;
    }
    return $True;
}

B_bool B_strD_istitle(B_str s) {
    unsigned char *p = s->str;
    int codepoint;
    int nbytes;
    int hascased = 0;
    int incasedrun = 0;
    if (s->nchars == 0)
        return $False;
    for (int i=0; i < s->nchars; i++) {
        nbytes = utf8proc_iterate(p,-1,&codepoint);
        utf8proc_category_t cat = utf8proc_category(codepoint);
        if (cat == UTF8PROC_CATEGORY_LU || cat == UTF8PROC_CATEGORY_LT ) {
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
    return toB_bool(hascased);
}

B_bool B_strD_isupper(B_str s) {
    unsigned char *p = s->str;
    int codepoint;
    int nbytes;
    int hascased = 0;
    if (s->nchars == 0)
        return $False;
    for (int i=0; i < s->nchars; i++) {
        nbytes = utf8proc_iterate(p,-1,&codepoint);
        utf8proc_category_t cat = utf8proc_category(codepoint);
        if (cat == UTF8PROC_CATEGORY_LL)
            return $False;
        if (cat == UTF8PROC_CATEGORY_LU || cat == UTF8PROC_CATEGORY_LT)
            hascased = 1;
        p += nbytes;
    }
    return toB_bool(hascased);
}

B_str B_strD_join(B_str s, B_Iterable wit, $WORD iter) {
    int totchars = 0;
    int totbytes = 0;
    B_list lst = B_listD_fromiter(wit->$class->__iter__(wit,iter));
    B_str nxt;
    int len = lst->length;
    for (int i=0; i<len; i++) {
        nxt = (B_str)lst->data[i];
        totchars += nxt->nchars;
        totbytes += nxt->nbytes;
    }
    if (len > 1) {
        totchars += (len-1) * s->nchars;
        totbytes += (len-1) * s->nbytes;
    }
    B_str res;
    NEW_UNFILLED_STR(res,totchars,totbytes);
    if (len > 0) {
        nxt = (B_str)lst->data[0];
        unsigned char *p = res->str;
        memcpy(p,nxt->str,nxt->nbytes);
        p += nxt->nbytes;
        for (int i=1; i<len; i++) {
            nxt = (B_str)lst->data[i];
            memcpy(p,s->str,s->nbytes);
            p += s->nbytes;
            memcpy(p,nxt->str,nxt->nbytes);
            p += nxt->nbytes;
        }
    }
    return res;
}

B_str B_strD_ljust(B_str s, B_int width, B_str fill) {
    if (!fill) fill = space_str;
    if (fill->nchars != 1) {
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("ljust: fill str not single char")));
    }
    int wval = fromB_int(width);
    if (wval <= s->nchars) {
        return s;
    }
    int pad = (wval-s->nchars);
    B_str res;
    NEW_UNFILLED_STR(res,wval, s->nbytes+pad*fill->nbytes);
    unsigned char *c = fill->str;
    unsigned char *p = res->str + s->nbytes;
    for (int i = 0; i<pad; i++) {
        for (int j = 0; j < fill->nbytes; j++) 
            *p++ = c[j];
    }
    memcpy(res->str,s->str,s->nbytes);
    return res;
}

B_str B_strD_lower(B_str s) {
    return str_transform(s,utf8proc_tolower);
}

B_str B_strD_lstrip(B_str s, B_str cs) {
    unsigned char *p = s->str;
    int i, nbytes;
    for (i=0; i<s->nchars; i++) {
        B_str c = mk_char(p);
        if (cs == NULL ?  !B_strD_isspace(c) :
            bmh(cs->str,p,cs->nbytes,byte_length2(*p)) < 0) 
            break;
        p += byte_length2(*p);
    }
    nbytes = s->nbytes + s->str - p;
    B_str res;
    NEW_UNFILLED_STR(res,s->nchars-i,nbytes);
    memcpy(res->str,p,nbytes);
    return res;
}

B_tuple B_strD_partition(B_str s, B_str sep) {
    int n = fromB_int(B_strD_find(s,sep,NULL,NULL));
    if (n<0) {
        return $NEWTUPLE(3,s,null_str,null_str);
    } else {
        int nb = bmh(s->str,sep->str,s->nbytes,sep->nbytes);
        B_str ls;
        NEW_UNFILLED_STR(ls,n,nb);
        memcpy(ls->str,s->str,nb);
        B_str rs;
        int nbr = s->nbytes - sep->nbytes - nb;
        NEW_UNFILLED_STR(rs,s->nchars-n-sep->nchars,nbr);
        memcpy(rs->str,s->str+nb+sep->nbytes,nbr);
        return $NEWTUPLE(3,ls,sep,rs);
    }
}

B_str B_strD_replace(B_str s, B_str old, B_str new, B_int count) {
    if (count==NULL)
        count = toB_int(INT_MAX);
    int c = fromB_int(B_strD_count(s,old,NULL,NULL));
    int c0 = fromB_int(count) < c ? fromB_int(count) : c;
    if (c0==0){
        return s;
    }
    int nbytes = s->nbytes + c0*(new->nbytes-old->nbytes);
    int nchars = s->nchars+c0*(new->nchars-old->nchars);
    B_str res;
    NEW_UNFILLED_STR(res,nchars,nbytes);
    unsigned char *p = s->str;
    unsigned char *q = res->str;
    unsigned char *pold = old->str;
    unsigned char *pnew = new->str;
    int plen = s->nbytes;
    int n;
    for (int i=0; i<c0; i++) {
        n = i>0 && old->nbytes==0 ? 1 : bmh(p,pold,plen,old->nbytes);
        if (n>0) {
            memcpy(q,p,n);
            p+=n; q+=n;
        }
        memcpy(q,pnew,new->nbytes);
        p += old->nbytes;
        q += new->nbytes;
        plen -= n+old->nbytes;
    }
    if (plen>0)
        memcpy(q,p,plen);
    return res;
}
      

B_int B_strD_rfind(B_str s, B_str sub, B_int start, B_int end) {
    int isascii = s->nchars == s->nbytes;
    B_int st = start;
    B_int en = end;
    if (fix_start_end(s->nchars,&st,&en) < 0) return toB_int(-1);
    unsigned char *p = skip_chars(s->str,fromB_int(st),isascii);
    unsigned char *q = skip_chars(p,fromB_int(en)-fromB_int(st),isascii);
    int n = rbmh(p,sub->str,q-p,sub->nbytes);
    if (n<0) return toB_int(-1);
    return toB_int(char_no(s,n+p-s->str));
}


B_int B_strD_rindex(B_str s, B_str sub, B_int start, B_int end) {
    B_int n = B_strD_rfind(s,sub,start,end);
    if (fromB_int(n)<0) {
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("rindex: substring not found")));
    };
    return n;
}

B_str B_strD_rjust(B_str s, B_int width, B_str fill) {
    if (!fill) fill = space_str;
    if (fill->nchars != 1) {
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("rjust: fill string not single char")));
    }
    int wval = fromB_int(width);
    if (wval <= s->nchars) {
        return s;
    }
    int pad = (wval-s->nchars);
    B_str res;
    NEW_UNFILLED_STR(res,wval,s->nbytes+pad*fill->nbytes);
    unsigned char *c = fill->str;
    unsigned char *p = res->str;
    for (int i = 0; i<pad; i++) {
        for (int j = 0; j < fill->nbytes; j++) 
            *p++ = c[j];
    }
    memcpy(p,s->str,s->nbytes);
    return res;
}
                                
B_tuple B_strD_rpartition(B_str s, B_str sep) {
    int n = fromB_int(B_strD_rfind(s,sep,NULL,NULL));
    if (n<0) {
        return $NEWTUPLE(3,null_str,null_str,s);
    } else {
        int nb = rbmh(s->str,sep->str,s->nbytes,sep->nbytes);
        B_str ls;
        NEW_UNFILLED_STR(ls,n,nb);
        memcpy(ls->str,s->str,nb);
        int nbr = s->nbytes - sep->nbytes - nb;
        B_str rs;    
        NEW_UNFILLED_STR(rs,s->nchars-n-sep->nchars,nbr);
        memcpy(rs->str,s->str+nb+sep->nbytes,nbr);
        return  $NEWTUPLE(3,ls,sep,rs);
    }
}


B_list B_strD_split(B_str s, B_str sep, B_int maxsplit) {
    B_list res = $NEW(B_list,NULL,NULL);
    if (maxsplit == NULL || fromB_int(maxsplit) < 0) maxsplit = toB_int(INT_MAX); 
    int remaining = s->nchars;
    if (sep == NULL) {
        unsigned char *p = s->str;
        int nbytes, codepoint, wordlength;
        if (remaining==0) {
            return res;
        }
        wordlength = 0;
        int inword = 0;
        unsigned char *q;
        while (remaining > 0) {
            nbytes = utf8proc_iterate(p,-1,&codepoint);
            if (!isspace_codepoint(codepoint)) {
                if (!inword) {
                    inword = 1;
                    q = p;
                    wordlength = 1;
                    if (B_listD_len(res) == fromB_int(maxsplit))
                        break; // we have now removed leading whitespace in remainder
                } else
                    wordlength++;
            } else {
                if (inword) {
                    inword = 0;
                    B_str word;
                    NEW_UNFILLED_STR(word,wordlength,p-q);
                    memcpy(word->str,q,p-q);
                    B_listD_append(res,word);
                    wordlength = 0;
                }
            }
            remaining--;
            p += nbytes;
        }
        // this if statement should be simplified; almost code duplication.
        if (remaining == 0) {
            if (inword) {
                B_str word;
                NEW_UNFILLED_STR(word,wordlength,p-q);
                memcpy(word->str,q,p-q);
                B_listD_append(res,word);
            }
        } else {
            B_str word;
            p = s->str+s->nbytes;
            NEW_UNFILLED_STR(word,remaining,p-q);
            memcpy(word->str,q,p-q);
            B_listD_append(res,word);
        }
        // $WORD w = list_getitem(res,0);
        return res;
    } else { // separator given
        if (sep->nchars==0) {
            $RAISE((B_BaseException)$NEW(B_ValueError,to$str("split: separator is empty string")));
        }
        if (remaining==0) { // for some unfathomable reason, this is the behaviour of the Python method
            B_listD_append(res,null_str);
            return res;
        }
        B_str ls, rs, ssep;
        rs = s;
        // Note: This builds many intermediate rs strings...
        while (rs->nchars>0 && B_listD_len(res) < fromB_int(maxsplit)) {
            B_tuple t = B_strD_partition(rs,sep);
            ssep = (B_str)t->components[1];
            rs =  (B_str)t->components[2];
            B_listD_append(res,(B_str)t->components[0]);
        }
        if (ssep->nchars>0)
            B_listD_append(res,rs);
        return res;
    }
}
 
B_list B_strD_splitlines(B_str s, B_bool keepends) {
    if (!keepends)
        keepends = $False;
    B_list res = $NEW(B_list,NULL,NULL);
    unsigned char *p = s->str;
    unsigned char *q = p;
    int nbytes, codepoint, linelength;
    if (s->nbytes==0) {
        return res;
    }
    linelength = 0;
    while (p < s->str + s->nbytes) {
        nbytes = utf8proc_iterate(p,-1,&codepoint);
        utf8proc_category_t cat = utf8proc_category(codepoint);
        if (!islinebreak_codepoint(codepoint)) {
            linelength++;
            p += nbytes;
        } else {
            // all the codepoints we count as linebreaks are ascii bytes, i.e. nbytes = 1.
            B_str line;
            int winend = *p=='\r' && *(p+1)=='\n';
            int size = p-q + (keepends->val ? 1 + winend : 0);
            NEW_UNFILLED_STR(line,linelength + (keepends->val ? 1 + winend : 0),size);
            memcpy(line->str,q,size);
            p += 1 + winend;
            q = p;
            B_listD_append(res,line);
            linelength = 0;
        }
    }
    if (q < p) {
        B_str line;
        NEW_UNFILLED_STR(line,linelength,p-q);
        memcpy(line->str,q,p-q);
        B_listD_append(res,line);
    }
    return res;
} 

B_str B_strD_rstrip(B_str s, B_str cs) {
    unsigned char *p = s->str + s->nbytes;
    int i, nbytes;
    for (i=0; i<s->nchars; i++) {
        p = skip_chars(p,-1,0);
        B_str c = mk_char(p);
        if (cs == NULL ?  !B_strD_isspace(c) :
            rbmh(cs->str,p,cs->nbytes,byte_length2(*p)) < 0) 
            break;
    }
    nbytes = p + byte_length2(*p) - s->str;
    B_str res;
    NEW_UNFILLED_STR(res,s->nchars-i,nbytes);
    memcpy(res->str,s->str,nbytes);
    return res;
}

B_bool B_strD_startswith(B_str s, B_str sub, B_int start, B_int end) {
    B_int st = start;
    B_int en = end;
    if (fix_start_end(s->nchars,&st,&en) < 0) return $False;
    int isascii = s->nchars==s->nbytes;
    unsigned char *p = skip_chars(s->str,fromB_int(st),isascii);
    unsigned char *q = sub->str;
    for (int i=0; i<sub->nbytes; i++) {
        if (*p == 0 || *p++ != *q++) {
            return $False;
        }
    }
    return $True;
}


B_str B_strD_strip(B_str s, B_str cs) {
    return B_strD_lstrip(B_strD_rstrip(s,cs),cs);
}

B_str B_strD_upper(B_str s) {
    return str_transform(s,utf8proc_toupper);
}

B_str B_strD_zfill(B_str s, B_int width) {
    int wval = fromB_int(width);
    int fill = wval - s->nchars;
    if (fill < 0)
        return s;
    B_str res;
    NEW_UNFILLED_STR(res,wval,s->nbytes+fill);
    unsigned char *p = s->str;
    unsigned char *q = res->str;
    int hassign = (*p=='+' | *p=='-');
    if (hassign) {
        *q = *p;
        q++;
    }
    for (int i=0; i < fill; i++) 
        *q++ = '0';
    memcpy(res->str+hassign+fill,s->str+hassign,s->nbytes-hassign);
    return res;
}


// End of str implementation ////////////////////////////////////////////////////



// bytearray implementation //////////////////////////////////////////////////////////////////////////////

// Conversion to and from C strings

B_bytearray toB_bytearray(char *str) {
    B_bytearray res;
    int len = strlen(str);
    NEW_UNFILLED_BYTEARRAY(res,len);
    memcpy(res->str,str,len);
    return res;
}

unsigned char *fromB_bytearray(B_bytearray b) {
    return b->str;
}

// Auxiliaries

static void expand_bytearray(B_bytearray b,int n) {
    if (b->capacity >= b->nbytes + n)
        return;
    int newcapacity = b->capacity==0 ? 1 : b->capacity;
    while (newcapacity < b->nbytes+n)
        newcapacity <<= 1;
    unsigned char *newstr = b->str==NULL
        ? malloc(newcapacity+1)
        : realloc(b->str,newcapacity+1);
    if (newstr == NULL) {
        $RAISE((B_BaseException)$NEW(B_MemoryError,to$str("memory allocation failed")));
    }
    b->str = newstr;
    b->capacity = newcapacity;
}  


// General methods, prototypes
void B_bytearrayD_init(B_bytearray, B_bytes);
B_bool B_bytearrayD_bool(B_bytearray);
B_str B_bytearrayD_str(B_bytearray);
void B_bytearrayD_serialize(B_bytearray,$Serial$state);
B_bytearray B_bytearrayD_deserialize(B_bytearray,$Serial$state);


// bytearray methods, prototypes

B_bytearray B_bytearrayD_capitalize(B_bytearray s);
B_bytearray B_bytearrayD_center(B_bytearray s, B_int width, B_bytearray fill);
B_int B_bytearrayD_count(B_bytearray s, B_bytearray sub, B_int start, B_int end);
B_str B_bytearrayD_decode(B_bytearray s);
B_bool B_bytearrayD_endswith(B_bytearray s, B_bytearray suffix, B_int start, B_int end);
B_bytearray B_bytearrayD_expandtabs(B_bytearray s, B_int tabsize);      
B_int B_bytearrayD_find(B_bytearray s, B_bytearray sub, B_int start, B_int end);
B_int B_bytearrayD_index(B_bytearray s, B_bytearray sub, B_int start, B_int end);
B_bool B_bytearrayD_isalnum(B_bytearray s);
B_bool B_bytearrayD_isalpha(B_bytearray s);
B_bool B_bytearrayD_isascii(B_bytearray s);
B_bool B_bytearrayD_isdecimal(B_bytearray s);
B_bool B_bytearrayD_isdigit(B_bytearray s);
B_bool B_bytearrayD_isidentifier(B_bytearray s);
B_bool B_bytearrayD_islower(B_bytearray s);
B_bool B_bytearrayD_isnumeric(B_bytearray s);
B_bool B_bytearrayD_isprintable(B_bytearray s);
B_bool B_bytearrayD_isspace(B_bytearray s);
B_bool B_bytearrayD_istitle(B_bytearray s);
B_bool B_bytearrayD_isupper(B_bytearray s);
B_bytearray B_bytearrayD_join(B_bytearray sep, B_Iterable wit, $WORD iter);
B_bytearray B_bytearrayD_ljust(B_bytearray s, B_int width, B_bytearray fill); 
B_bytearray B_bytearrayD_lower(B_bytearray s);
B_bytearray B_bytearrayD_lstrip(B_bytearray s,B_bytearray cs); 
B_tuple B_bytearrayD_partition(B_bytearray s, B_bytearray sep);
B_bytearray B_bytearrayD_replace(B_bytearray s, B_bytearray old, B_bytearray new, B_int count);
B_int B_bytearrayD_rfind(B_bytearray s, B_bytearray sub, B_int start, B_int end);
B_int B_bytearrayD_rindex(B_bytearray s, B_bytearray sub, B_int start, B_int end);
B_bytearray B_bytearrayD_rjust(B_bytearray s, B_int width, B_bytearray fill);  
B_tuple B_bytearrayD_rpartition(B_bytearray s, B_bytearray sep); 
B_bytearray B_bytearrayD_rstrip(B_bytearray s,B_bytearray cs);
B_list B_bytearrayD_split(B_bytearray s, B_bytearray sep, B_int maxsplit);  
B_list B_bytearrayD_splitlines(B_bytearray s, B_bool keepends); 
B_bool B_bytearrayD_startswith(B_bytearray s, B_bytearray prefix, B_int start, B_int end); 
B_bytearray B_bytearrayD_strip(B_bytearray s,B_bytearray cs);
B_bytearray B_bytearrayD_upper(B_bytearray s);
B_bytearray B_bytearrayD_zfill(B_bytearray s, B_int width);

// Method table

struct B_bytearrayG_class B_bytearrayG_methods =
    {"B_bytearray",UNASSIGNED,($SuperG_class)&B_valueG_methods, B_bytearrayD_init, B_bytearrayD_serialize, B_bytearrayD_deserialize, B_bytearrayD_bool,
     B_bytearrayD_str, B_bytearrayD_str, B_bytearrayD_capitalize, B_bytearrayD_center, B_bytearrayD_count,  B_bytearrayD_decode, B_bytearrayD_endswith,
     B_bytearrayD_expandtabs, B_bytearrayD_find, B_bytearrayD_index,
     B_bytearrayD_isalnum, B_bytearrayD_isalpha, B_bytearrayD_isascii, B_bytearrayD_isdigit, B_bytearrayD_islower, B_bytearrayD_isspace,
     B_bytearrayD_istitle, B_bytearrayD_isupper, B_bytearrayD_join, B_bytearrayD_ljust, B_bytearrayD_lower, B_bytearrayD_lstrip, B_bytearrayD_partition, B_bytearrayD_replace,
     B_bytearrayD_rfind, B_bytearrayD_rindex, B_bytearrayD_rjust,
     B_bytearrayD_rpartition, B_bytearrayD_rstrip, B_bytearrayD_split, B_bytearrayD_splitlines, B_bytearrayD_startswith, B_bytearrayD_strip, B_bytearrayD_upper, B_bytearrayD_zfill};

// Bytearray methods, implementations

static B_bytearray B_bytearrayD_copy(B_bytearray s) {
    B_bytearray res;
    NEW_UNFILLED_BYTEARRAY(res,s->nbytes);
    res->nbytes = s->nbytes;
    memcpy(res->str,s->str,s->nbytes);
    return res;
}

B_bytearray B_bytearrayD_capitalize(B_bytearray s) {
    if (s->nbytes==0) {
        return toB_bytearray("");
    }
    B_bytearray res;
    NEW_UNFILLED_BYTEARRAY(res,s->nbytes);
    res->str[0] = toupper(s->str[0]);
    for (int i=1; i<s->nbytes; i++) 
        res->str[i] = tolower(s->str[i]);
    return res;
}

B_bytearray B_bytearrayD_center(B_bytearray s, B_int width, B_bytearray fill) {
    if (!fill) fill = toB_bytearray(" ");
    if (fill->nbytes != 1) {
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("center: fill bytearray not single char")));
    }
    int wval = fromB_int(width);
    if (wval <= s->nbytes) {
        return B_bytearrayD_copy(s);
    }
    int pad = (wval-s->nbytes);
    int padleft = pad/2; 
    int padright = pad-padleft;
    int sbytes = s->nbytes;
    B_bytearray res;
    NEW_UNFILLED_BYTEARRAY(res, wval);
    unsigned char c = fill->str[0];
    unsigned char *p = res->str;
    p += padleft+sbytes;
    for (int i = 0; i<padright; i++) {
        p[i] = c;
    }
    memcpy(res->str,p,padleft);
    p -= sbytes;
    memcpy(p,s->str,sbytes);
    return res;
}

B_int B_bytearrayD_count(B_bytearray s, B_bytearray sub, B_int start, B_int end) {
    B_int st = start;
    B_int en = end;
    if (fix_start_end(s->nbytes,&st,&en) < 0) return toB_int(0);
    int stval = fromB_int(st);
    int enval = fromB_int(en);
    unsigned char *p = &s->str[stval];
    unsigned char *q = &p[enval-stval];
    int res = 0;
    int n = bmh(p,sub->str,q-p,sub->nbytes);
    while (n>=0) {
        res++;
        p += n + (sub->nbytes>0 ? sub->nbytes : 1);
        n = bmh(p,sub->str,q-p,sub->nbytes);
    }
    return toB_int(res);
}

B_str B_bytearrayD_decode(B_bytearray s) {
    return to$str((char*)s->str);
}

B_bool B_bytearrayD_endswith(B_bytearray s, B_bytearray sub, B_int start, B_int end) {
    B_int st = start;
    B_int en = end;
    if (fix_start_end(s->nbytes,&st,&en) < 0) return $False;
    int enval = fromB_int(en);
    unsigned char *p = &s->str[enval-sub->nbytes];
    unsigned char *q = sub->str;
    for (int i=0; i<sub->nbytes; i++) {
        if (*p == 0 || *p++ != *q++) {
            return $False;
        }
    }
    return $True;
}

B_bytearray B_bytearrayD_expandtabs(B_bytearray s, B_int tabsz){
    int pos = 0;
    int expanded = 0;
    int tabsize = fromB_int(tabsz);
    tabsize = tabsize <= 0 ? 1 : tabsize;
    unsigned char buffer[tabsize * s->nbytes];
    unsigned char *p = s->str;
    unsigned char *q = buffer;
    for (int i=0; i<s->nbytes; i++) {
        if (*p == '\t') {
            int n = tabsize - pos % tabsize;
            for (int j=0; j < n; j++) {
                *q++ = ' ';
            }
            p++;
            expanded += n-1;
            pos+=n;
        } else if (*p=='\n' || *p == '\r') {
            *q++ = *p++;
            pos = 0;
        } else {
            for (int j=0; j< byte_length2(*p); j++) {
                *q++ = *p++;
                pos++;
            }
        }
    }
    B_bytearray res;
    NEW_UNFILLED_BYTEARRAY(res,s->nbytes+expanded);
    memcpy(res->str,buffer,s->nbytes+expanded);
    return res;
}

B_int B_bytearrayD_find(B_bytearray s, B_bytearray sub, B_int start, B_int end) {
    B_int st = start;
    B_int en = end;
    if (fix_start_end(s->nbytes,&st,&en) < 0) return toB_int(-1);
    unsigned char *p = &s->str[fromB_int(start)];
    unsigned char *q = &s->str[fromB_int(end)];
    int n = bmh(p,sub->str,q-p,sub->nbytes);
    if (n<0) return toB_int(-1);
    return toB_int(n+p-s->str);
}


B_int B_bytearrayD_index(B_bytearray s, B_bytearray sub, B_int start, B_int end) {
    B_int n = B_bytearrayD_find(s,sub,start,end);
    if (fromB_int(n)<0) {
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("index: substring not found")));
    }
    return n;
}

B_bool B_bytearrayD_isalnum(B_bytearray s) {
    if (s->nbytes==0)
        return $False;
    for (int i=0; i<s->nbytes; i++) {
        unsigned char c = s->str[i];
        if (c < '0' || c > 'z' || (c > '9' && c < 'A') || (c > 'Z' && c < 'a'))
            return $False;
    }
    return $True;
}

B_bool B_bytearrayD_isalpha(B_bytearray s) {
    if (s->nbytes==0)
        return $False;
    for (int i=0; i<s->nbytes; i++) {
        unsigned char c = s->str[i];
        if (c < 'A' || c > 'z' || (c > 'Z' && c < 'a'))
            return $False;
    }
    return $True;
}

B_bool B_bytearrayD_isascii(B_bytearray s) {
    for (int i=0; i<s->nbytes; i++) {
        unsigned char c = s->str[i];
        if (c > 0x7f)
            return $False;
    }
    return $True;
}

B_bool B_bytearrayD_isdigit(B_bytearray s) {
    if (s->nbytes==0)
        return $False;
    for (int i=0; i<s->nbytes; i++) {
        unsigned char c = s->str[i];
        if (c<'0' || c > '9')
            return $False;
    }
    return $True;
}
 

B_bool B_bytearrayD_islower(B_bytearray s) {
    int has_lower = 0;
    for (int i=0; i < s->nbytes; i++) {
        unsigned char c = s->str[i];
        if (c >= 'A' && c <= 'Z')
            return $False;
        if (c >= 'a' && c <= 'z')
            has_lower = 1;
    }
    return toB_bool(has_lower);
}

B_bool B_bytearrayD_isspace(B_bytearray s) {
    if (s->nbytes==0)
        return $False;
    for (int i=0; i<s->nbytes; i++) {
        unsigned char c = s->str[i];
        if (c !=' ' && c != '\t' && c != '\n' && c != '\r' && c != '\x0b' && c != '\f')
            return $False;
    }
    return $True;
}

B_bool B_bytearrayD_istitle(B_bytearray s) {
    if (s->nbytes==0)
        return $False;
    int incasedrun = 0;
    for (int i=0; i < s->nbytes; i++) {
        unsigned char c = s->str[i];
        if (c >='A' && c <= 'Z') {
            if (incasedrun)
                return $False;
            incasedrun = 1;
        } else if (c >='a' && c <= 'z') {
            if (!incasedrun)
                return $False;
        } else
            incasedrun = 0;
    }
    return $True;
}

B_bool B_bytearrayD_isupper(B_bytearray s) {
    int has_upper = 0;
    for (int i=0; i < s->nbytes; i++) {
        unsigned char c = s->str[i];
        if (c >= 'a' && c <= 'z')
            return $False;
        if (c >= 'a' && c <= 'z')
            has_upper = 1;
    }
    return toB_bool(has_upper);
}

B_bytearray B_bytearrayD_join(B_bytearray s, B_Iterable wit, $WORD iter) {
    int totbytes = 0;
    B_list lst = B_listD_fromiter(wit->$class->__iter__(wit,iter));
    B_bytearray nxt;
    int len = lst->length;
    for (int i=0; i<len; i++) {
        nxt = (B_bytearray)lst->data[i];
        totbytes += nxt->nbytes;
    }
    if (len > 1) {
        totbytes += (len-1) * s->nbytes;
    }
    B_bytearray res;
    NEW_UNFILLED_BYTEARRAY(res,totbytes);
    if (len > 0) {
        nxt = (B_bytearray)lst->data[0];
        unsigned char *p = res->str;
        memcpy(p,nxt->str,nxt->nbytes);
        p += nxt->nbytes;
        for (int i=1; i<len; i++) {
            nxt = (B_bytearray)lst->data[i];
            memcpy(p,s->str,s->nbytes);
            p += s->nbytes;
            memcpy(p,nxt->str,nxt->nbytes);
            p += nxt->nbytes;
        }
    }
    return res;
}

B_bytearray B_bytearrayD_ljust(B_bytearray s, B_int width, B_bytearray fill) {
    int wval = fromB_int(width);
    if (fill->nbytes != 1) {
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("bytearray ljust: fill array not single char")));
    }
    if (wval <= s->nbytes) {
        return B_bytearrayD_copy(s);
    }
    B_bytearray res;
    NEW_UNFILLED_BYTEARRAY(res,wval);
    memcpy(res->str,s->str,s->nbytes);
    unsigned char c = fill->str[0];
    for (int i = s->nbytes; i<wval; i++) {
        res->str[i] = c;
    }
    return res;
}

B_bytearray B_bytearrayD_lower(B_bytearray s) {
    B_bytearray res;
    NEW_UNFILLED_BYTEARRAY(res,s->nbytes);
    for (int i=0; i< s->nbytes; i++)
        res->str[i] = tolower(res->str[i]);
    return res;
}

B_bytearray B_bytearrayD_lstrip(B_bytearray s, B_bytearray cs) {
    if (!cs)
        cs = toB_bytearray(" \t\n\r\x0b\x0c");
    int nstrip = 0;
    for (int i=0; i<s->nbytes; i++) {
        unsigned char c = s->str[i];
        int found = 0;
        for (int j=0; j<cs->nbytes; j++)
            if (c == cs->str[j]) {
                found = 1;
                break;
            }
        if (!found)
            break;
        nstrip++;
    }
    B_bytearray res;
    NEW_UNFILLED_BYTEARRAY(res,s->nbytes-nstrip);
    memcpy(res->str,s->str+nstrip,res->nbytes);       
    return res;
}


B_tuple B_bytearrayD_partition(B_bytearray s, B_bytearray sep) {
    int n = fromB_int(B_bytearrayD_find(s,sep,NULL,NULL));
    if (n<0) {
        return $NEWTUPLE(3,s,toB_bytearray(""),toB_bytearray(""));
    } else {
        int nb = bmh(s->str,sep->str,s->nbytes,sep->nbytes);
        B_bytearray ls;
        NEW_UNFILLED_BYTEARRAY(ls,nb);
        memcpy(ls->str,s->str,nb);
        B_bytearray rs;
        int nbr = s->nbytes - sep->nbytes - nb;
        NEW_UNFILLED_BYTEARRAY(rs,nbr);
        memcpy(rs->str,s->str+nb+sep->nbytes,nbr);
        return $NEWTUPLE(3,ls,sep,rs);
    }
}


B_bytearray B_bytearrayD_replace(B_bytearray s, B_bytearray old, B_bytearray new, B_int count) {
    if (count==NULL)
        count = toB_int(INT_MAX);
    int c = fromB_int(B_bytearrayD_count(s,old,NULL,NULL));
    int c0 = fromB_int(count) < c ? fromB_int(count) : c;
    if (c0==0){
        return B_bytearrayD_copy(s);
    }
    int nbytes = s->nbytes + c0*(new->nbytes-old->nbytes);
    B_bytearray res;
    NEW_UNFILLED_BYTEARRAY(res,nbytes);
    unsigned char *p = s->str;
    unsigned char *q = res->str;
    unsigned char *pold = old->str;
    unsigned char *pnew = new->str;
    int plen = s->nbytes;
    int n;
    for (int i=0; i<c0; i++) {
        n = i>0 && old->nbytes==0 ? 1 : bmh(p,pold,plen,old->nbytes);
        if (n>0) {
            memcpy(q,p,n);
            p+=n; q+=n;
        }
        memcpy(q,pnew,new->nbytes);
        p += old->nbytes;
        q += new->nbytes;
        plen -= n+old->nbytes;
    }
    if (plen>0)
        memcpy(q,p,plen);
    return res;
}
      

B_int B_bytearrayD_rfind(B_bytearray s, B_bytearray sub, B_int start, B_int end) {
    B_int st = start;
    B_int en = end;
    if (fix_start_end(s->nbytes,&st,&en) < 0) return toB_int(-1);
    unsigned char *p = &s->str[fromB_int(st)];
    unsigned char *q = &s->str[fromB_int(en)];
    int n = rbmh(p,sub->str,q-p,sub->nbytes);
    if (n<0) return toB_int(-1);
    return toB_int(n+p-s->str);
}


B_int B_bytearrayD_rindex(B_bytearray s, B_bytearray sub, B_int start, B_int end) {
    B_int n = B_bytearrayD_rfind(s,sub,start,end);
    if (fromB_int(n)<0) {
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("rindex for bytearray: substring not found")));
    };
    return n;
}

B_bytearray B_bytearrayD_rjust(B_bytearray s, B_int width, B_bytearray fill) {
    int wval = fromB_int(width);
    if (fill->nbytes != 1) {
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("rjust: fill string not single char")));
    }
    if (wval <= s->nbytes) {
        return B_bytearrayD_copy(s);
    }
    int pad = (wval-s->nbytes);
    B_bytearray res;
    NEW_UNFILLED_BYTEARRAY(res,wval);
    unsigned char c = fill->str[0];
    for (int i = 0; i<pad; i++) {
        res->str[i] = c;
    }
    memcpy(&res->str[pad],s->str,s->nbytes);
    return res;
}
                                
B_tuple B_bytearrayD_rpartition(B_bytearray s, B_bytearray sep) {
    int n = fromB_int(B_bytearrayD_rfind(s,sep,NULL,NULL));
    if (n<0) {
        return $NEWTUPLE(3,toB_bytearray(""),toB_bytearray(""),s);
    } else {
        int nb = rbmh(s->str,sep->str,s->nbytes,sep->nbytes);
        B_bytearray ls;
        NEW_UNFILLED_BYTEARRAY(ls,nb);
        memcpy(ls->str,s->str,nb);
        int nbr = s->nbytes - sep->nbytes - nb;
        B_bytearray rs;    
        NEW_UNFILLED_BYTEARRAY(rs,nbr);
        memcpy(rs->str,s->str+nb+sep->nbytes,nbr);
        return  $NEWTUPLE(3,ls,sep,rs);
    }
}

B_bytearray B_bytearrayD_rstrip(B_bytearray s, B_bytearray cs) {
    if (!cs)
        cs = toB_bytearray(" \t\n\r\x0b\x0c");
    int nstrip = 0;
    for (int i=s->nbytes-1; i>=0; i--) {
        unsigned char c = s->str[i];
        int found = 0;
        for (int j=0; j<cs->nbytes; j++)
            if (c == cs->str[j]) {
                found = 1;
                break;
            }
        if (!found)
            break;
        nstrip++;
    }
    B_bytearray res;
    NEW_UNFILLED_BYTEARRAY(res,s->nbytes-nstrip);
    memcpy(res->str,s->str,res->nbytes);       
    return res;
}
 
B_list B_bytearrayD_split(B_bytearray s, B_bytearray sep, B_int maxsplit) {
    B_list res = $NEW(B_list,NULL,NULL);
    if (maxsplit == NULL || fromB_int(maxsplit) < 0) maxsplit = toB_int(INT_MAX); 
    if (sep == NULL) {
        unsigned char *p = s->str;
        if (s->nbytes==0) {
            return res;
        }
        int inword = 0;
        unsigned char *q;
        while (p < s->str + s->nbytes) {
            if  (*p !=' ' && *p != '\t' && *p != '\n' && *p != '\r' && *p != '\x0b' && *p != '\f') {
                if (!inword) {
                    inword = 1;
                    q = p;
                    if (B_listD_len(res) == fromB_int(maxsplit))
                        break; // we have now removed leading whitespace in remainder
                } 
            } else {
                if (inword) {
                    inword = 0;
                    B_bytearray word;
                    NEW_UNFILLED_BYTEARRAY(word,p-q);
                    memcpy(word->str,q,p-q);
                    B_listD_append(res,word);
                }
            }
            p++;
        }
        // this if statement should be simplified; almost code duplication.
        if (p < s->str + s->nbytes) { // we did not break out of the while loop
            if (inword) {
                B_bytearray word;
                NEW_UNFILLED_BYTEARRAY(word,p-q);
                memcpy(word->str,q,p-q);
                B_listD_append(res,word);
            }
        } else {
            B_bytearray word;
            p = s->str+s->nbytes;
            NEW_UNFILLED_BYTEARRAY(word,p-q);
            memcpy(word->str,q,p-q);
            B_listD_append(res,word);
        }
        return res;
    } else { // separator given
        if (sep->nbytes==0) {
            $RAISE((B_BaseException)$NEW(B_ValueError,to$str("split for bytearray: separator is empty string")));
        }
        if (s->nbytes==0) { // for some unfathomable reason, this is the behaviour of the Python method
            B_listD_append(res,null_str);
            return res;
        }
        B_bytearray ls, rs, ssep;
        rs = s;
        // Note: This builds many intermediate rs strings...
        while (rs->nbytes>0 && B_listD_len(res) < fromB_int(maxsplit)) {
            B_tuple t = B_bytearrayD_partition(rs,sep);
            ssep = (B_bytearray)t->components[1];
            rs =  (B_bytearray)t->components[2];
            B_listD_append(res,(B_bytearray)t->components[0]);
        }
        if (ssep->nbytes>0)
            B_listD_append(res,rs);
        return res;
    }
}

B_list B_bytearrayD_splitlines(B_bytearray s, B_bool keepends) {
    if (!keepends)
        keepends = $False;
    B_list res = $NEW(B_list,NULL,NULL);
    if (s->nbytes==0) {
        return res;
    }
    int winend;
    unsigned char *p = s->str;
    unsigned char *q = p;
    while (p < s->str + s->nbytes) {
        if (*p != '\n' && *p != '\r') {
            p++;
        } else {
            B_bytearray line;
            winend = *p=='\r' && *(p+1)=='\n';
            int size = p-q + (keepends->val ? 1 + winend : 0);
            NEW_UNFILLED_BYTEARRAY(line,size);
            memcpy(line->str,q,size);
            p+= 1 + winend;
            q = p;
            B_listD_append(res,line);
        }
    }
    if (q < p) {
        B_bytearray line;
        NEW_UNFILLED_BYTEARRAY(line,p-q);
        memcpy(line->str,q,p-q);
        B_listD_append(res,line);
    }
    return res;
} 

B_bool B_bytearrayD_startswith(B_bytearray s, B_bytearray sub, B_int start, B_int end) {
    B_int st = start;
    B_int en = end;
    if (fix_start_end(s->nbytes,&st,&en) < 0) return $False;
    unsigned char *p = s->str + fromB_int(st);
    if (p+sub->nbytes >= s->str+s->nbytes) return $False;
    unsigned char *q = sub->str;
    for (int i=0; i<sub->nbytes; i++) {
        if (p >= s->str + fromB_int(en) || *p++ != *q++) {
            return $False;
        }
    }
    return $True;
}


B_bytearray B_bytearrayD_strip(B_bytearray s, B_bytearray cs) {
    return B_bytearrayD_lstrip(B_bytearrayD_rstrip(s,cs),cs);
}

B_bytearray B_bytearrayD_upper(B_bytearray s) {
    B_bytearray res;
    NEW_UNFILLED_BYTEARRAY(res,s->nbytes);
    for (int i=0; i< s->nbytes; i++)
        res->str[i] = toupper(res->str[i]);
    return res;
}

B_bytearray B_bytearrayD_zfill(B_bytearray s, B_int width) {
    int wval = fromB_int(width);
    int fill = wval - s->nbytes;
    if (fill < 0)
        return B_bytearrayD_copy(s);
    B_bytearray res;
    NEW_UNFILLED_BYTEARRAY(res,wval);
    unsigned char *p = s->str;
    unsigned char *q = res->str;
    int hassign = (*p=='+' | *p=='-');
    if (hassign) {
        *q = *p;
        q++;
    }
    for (int i=0; i < fill; i++) 
        *q++ = '0';
    memcpy(res->str+hassign+fill,s->str+hassign,s->nbytes-hassign);
    return res;
}

// Protocol methods, prototypes for bytearrays


int B_bytearrayD_eq(B_bytearray,B_bytearray);
int B_bytearrayD_neq(B_bytearray,B_bytearray);
int B_bytearrayD_lt(B_bytearray,B_bytearray);
int B_bytearrayD_le(B_bytearray,B_bytearray);
int B_bytearrayD_gt(B_bytearray,B_bytearray);
int B_bytearrayD_ge(B_bytearray,B_bytearray);

B_int B_bytearrayD_getitem(B_bytearray, int);
void B_bytearrayD_setitem(B_bytearray, int, int);
void B_bytearrayD_delitem(B_bytearray, int);
B_bytearray B_bytearrayD_getslice(B_bytearray, B_slice);
void B_bytearrayD_setslice(B_bytearray, B_slice, B_Iterator);
void B_bytearrayD_delslice(B_bytearray, B_slice);
B_Iterator B_bytearrayD_reversed(B_bytearray);
void B_bytearrayD_insert(B_bytearray, int, B_int);
void B_bytearrayD_append(B_bytearray, B_int);
void B_bytearrayD_reverse(B_bytearray);

B_Iterator B_bytearrayD_iter(B_bytearray);
B_bytearray B_bytearrayD_fromiter(B_Iterable, $WORD);
B_int B_bytearrayD_len(B_bytearray str);

B_bytearray B_bytearrayD_add(B_bytearray, B_bytearray);
B_bytearray B_bytearrayD_mul(B_bytearray, B_int);

int B_bytearrayD_contains (B_bytearray, B_int);
int B_bytearrayD_containsnot (B_bytearray, B_int);




// Protocol instances, using above prototypes 


// Ord

void B_OrdD_bytearrayD___serialize__(B_OrdD_bytearray self, $Serial$state state) {
}

B_OrdD_bytearray B_OrdD_bytearrayD___deserialize__(B_OrdD_bytearray self, $Serial$state state) {
    B_OrdD_bytearray res = $DNEW(B_OrdD_bytearray,state);
    return res;
}

B_OrdD_bytearray B_OrdD_bytearrayG_new() {
    return $NEW(B_OrdD_bytearray);
}

B_bool B_OrdD_bytearrayD___eq__ (B_OrdD_bytearray wit, B_bytearray a, B_bytearray b) {
    return toB_bool(B_bytearrayD_eq(a,b));
}

B_bool B_OrdD_bytearrayD___ne__ (B_OrdD_bytearray wit, B_bytearray a, B_bytearray b) {
    return  toB_bool(B_bytearrayD_neq(a,b));
}

B_bool B_OrdD_bytearrayD___lt__ (B_OrdD_bytearray wit, B_bytearray a, B_bytearray b) {
    return toB_bool(B_bytearrayD_lt(a,b));
}

B_bool B_OrdD_bytearrayD___le__ (B_OrdD_bytearray wit, B_bytearray a, B_bytearray b){
    return toB_bool(B_bytearrayD_le(a,b));
}

B_bool B_OrdD_bytearrayD___gt__ (B_OrdD_bytearray wit, B_bytearray a, B_bytearray b){
    return toB_bool(B_bytearrayD_gt(a,b));
}

B_bool B_OrdD_bytearrayD___ge__ (B_OrdD_bytearray wit, B_bytearray a, B_bytearray b){
    return toB_bool(B_bytearrayD_ge(a,b));
}

// Sequence

void B_SequenceD_bytearrayD___serialize__(B_SequenceD_bytearray self, $Serial$state state) {
    $step_serialize(self->W_Collection, state);
    $step_serialize(self->W_Times, state);
}

B_SequenceD_bytearray B_SequenceD_bytearrayD___deserialize__(B_SequenceD_bytearray self, $Serial$state state) {
    B_SequenceD_bytearray res = $DNEW(B_SequenceD_bytearray,state);
    res->W_Collection = (B_Collection)$step_deserialize(state);
    res->W_Times = (B_Times)$step_deserialize(state);
    return res;
}

B_SequenceD_bytearray B_SequenceD_bytearrayG_new() {
    return $NEW(B_SequenceD_bytearray);
}

B_int B_SequenceD_bytearrayD___getitem__ (B_SequenceD_bytearray wit, B_bytearray self, B_int ix) {
    return B_bytearrayD_getitem(self,fromB_int(ix));
}

void B_SequenceD_bytearrayD___setitem__ (B_SequenceD_bytearray wit, B_bytearray self, B_int ix, B_int val) {
    B_bytearrayD_setitem(self,fromB_int(ix),fromB_int(val));
}

void B_SequenceD_bytearrayD___delitem__ (B_SequenceD_bytearray wit, B_bytearray self, B_int ix) {
    B_bytearrayD_delitem(self,fromB_int(ix));
}

B_bytearray B_SequenceD_bytearrayD___getslice__ (B_SequenceD_bytearray wit, B_bytearray self, B_slice slc) {
    return B_bytearrayD_getslice(self,slc);
}

void B_SequenceD_bytearrayD___setslice__ (B_SequenceD_bytearray wit,  B_bytearray self, B_Iterable wit2, B_slice slc, $WORD iter) {
    B_bytearrayD_setslice(self,slc,wit2->$class->__iter__(wit2,iter));
}

void B_SequenceD_bytearrayD___delslice__ (B_SequenceD_bytearray wit, B_bytearray self, B_slice slc) {
    B_bytearrayD_delslice(self,slc);
}

B_Iterator B_SequenceD_bytearrayD___reversed__(B_SequenceD_bytearray wit, B_bytearray self) {
    return B_bytearrayD_reversed(self);
}

void B_SequenceD_bytearray$insert(B_SequenceD_bytearray wit, B_bytearray self, B_int ix, B_int val) {
    B_bytearrayD_insert(self, fromB_int(ix), val);
}

void B_SequenceD_bytearray$append(B_SequenceD_bytearray wit, B_bytearray self, B_int val) {
    B_bytearrayD_append(self, val);
}

void B_SequenceD_bytearray$reverse(B_SequenceD_bytearray wit, B_bytearray self) {
    B_bytearrayD_reverse(self);
}


// Collection

void B_CollectionD_SequenceD_bytearrayD___serialize__(B_CollectionD_SequenceD_bytearray self, $Serial$state state) {
    $step_serialize(self->W_Sequence, state);
}

B_CollectionD_SequenceD_bytearray B_CollectionD_SequenceD_bytearrayD___deserialize__(B_CollectionD_SequenceD_bytearray self, $Serial$state state) {
    B_CollectionD_SequenceD_bytearray res = $DNEW(B_CollectionD_SequenceD_bytearray,state);
    res->W_Sequence = (B_Sequence)$step_deserialize(state);
    return res;
}

B_CollectionD_SequenceD_bytearray B_CollectionD_SequenceD_bytearrayG_new(B_Sequence wit) {
    return $NEW(B_CollectionD_SequenceD_bytearray,wit);
}

B_Iterator B_CollectionD_SequenceD_bytearrayD___iter__ (B_CollectionD_SequenceD_bytearray wit, B_bytearray str) {
    return B_bytearrayD_iter(str);
}

B_bytearray B_CollectionD_SequenceD_bytearrayD___fromiter__ (B_CollectionD_SequenceD_bytearray wit, B_Iterable wit2, $WORD iter) {
    return B_bytearrayD_join(toB_bytearray(""),wit2,iter);
}

B_int B_CollectionD_SequenceD_bytearrayD___len__ (B_CollectionD_SequenceD_bytearray wit, B_bytearray str) {
    return B_bytearrayD_len(str);
}

// Times

void B_TimesD_SequenceD_bytearrayD___serialize__(B_TimesD_SequenceD_bytearray self, $Serial$state state) {
    $step_serialize(self->W_Sequence, state);
}

B_TimesD_SequenceD_bytearray B_TimesD_SequenceD_bytearrayD___deserialize__(B_TimesD_SequenceD_bytearray self, $Serial$state state) {
    B_TimesD_SequenceD_bytearray res = $DNEW(B_TimesD_SequenceD_bytearray,state);
    res->W_Sequence = (B_Sequence)$step_deserialize(state);
    return res;
}

B_TimesD_SequenceD_bytearray B_TimesD_SequenceD_bytearrayG_new(B_Sequence wit) {
    return $NEW(B_TimesD_SequenceD_bytearray,wit);
}

B_bytearray B_TimesD_SequenceD_bytearrayD___add__ (B_TimesD_SequenceD_bytearray wit, B_bytearray a, B_bytearray b) {
    return B_bytearrayD_add(a,b);
}

B_bytearray B_TimesD_SequenceD_bytearrayD___mul__ (B_TimesD_SequenceD_bytearray wit, B_bytearray a, B_int n) {
    return B_bytearrayD_mul(a,n);
}

// Container

void B_ContainerD_bytearrayD___serialize__(B_ContainerD_bytearray self, $Serial$state state) {
}

B_ContainerD_bytearray B_ContainerD_bytearrayD___deserialize__(B_ContainerD_bytearray self, $Serial$state state) {
    return $DNEW(B_ContainerD_bytearray,state);
}

B_ContainerD_bytearray B_ContainerD_bytearrayG_new() {
    return $NEW(B_ContainerD_bytearray);
}

B_Iterator B_ContainerD_bytearrayD___iter__ (B_ContainerD_bytearray wit, B_bytearray str) {
    return B_bytearrayD_iter(str);
}

B_bytearray B_ContainerD_bytearrayD___fromiter__ (B_ContainerD_bytearray wit, B_Iterable wit2, $WORD iter) {
    return B_bytearrayD_join(toB_bytearray(""),wit2,iter);
}

B_int B_ContainerD_bytearrayD___len__ (B_ContainerD_bytearray wit, B_bytearray str) {
    return B_bytearrayD_len(str);
}

B_bool B_ContainerD_bytearrayD___contains__(B_ContainerD_bytearray wit, B_bytearray self, B_int n) {
    return toB_bool(B_bytearrayD_contains(self,n));
}

B_bool B_ContainerD_bytearrayD___containsnot__(B_ContainerD_bytearray wit, B_bytearray self, B_int n) {
    return  toB_bool(!B_bytearrayD_contains(self,n));
}


// Method tables for witness classes

struct B_SequenceD_bytearray  B_SequenceD_bytearray_instance;
struct B_CollectionD_SequenceD_bytearray B_CollectionD_SequenceD_bytearray_instance;
struct B_TimesD_SequenceD_bytearray B_TimesD_SequenceD_bytearray_instance;


struct B_OrdD_bytearrayG_class  B_OrdD_bytearrayG_methods = {
    "B_OrdD_bytearray",
    UNASSIGNED,
    ($SuperG_class)&B_OrdG_methods,
    (void (*)(B_OrdD_bytearray))$default__init__,
    B_OrdD_bytearrayD___serialize__,
    B_OrdD_bytearrayD___deserialize__,
    (B_bool (*)(B_OrdD_bytearray))$default__bool__,
    (B_str (*)(B_OrdD_bytearray))$default__str__,
    (B_str (*)(B_OrdD_bytearray))$default__str__,
    B_OrdD_bytearrayD___eq__, B_OrdD_bytearrayD___ne__,
    B_OrdD_bytearrayD___lt__, B_OrdD_bytearrayD___le__,
    B_OrdD_bytearrayD___gt__, B_OrdD_bytearrayD___ge__
};
struct B_OrdD_bytearray B_OrdD_bytearray_instance = {&B_OrdD_bytearrayG_methods};
B_OrdD_bytearray B_OrdD_bytearrayG_witness = &B_OrdD_bytearray_instance;

struct B_SequenceD_bytearrayG_class B_SequenceD_bytearrayG_methods = {
    "B_SequenceD_bytearray",
    UNASSIGNED,
    ($SuperG_class)&B_SequenceG_methods,
    B_SequenceD_bytearrayD___init__,
    B_SequenceD_bytearrayD___serialize__,
    B_SequenceD_bytearrayD___deserialize__,
    (B_bool (*)(B_SequenceD_bytearray))$default__bool__,
    (B_str (*)(B_SequenceD_bytearray))$default__str__,
    (B_str (*)(B_SequenceD_bytearray))$default__str__,
    B_SequenceD_bytearrayD___getitem__,
    B_SequenceD_bytearrayD___setitem__,
    B_SequenceD_bytearrayD___delitem__,
    B_SequenceD_bytearrayD___getslice__,
    B_SequenceD_bytearrayD___setslice__,
    B_SequenceD_bytearrayD___delslice__,
    B_SequenceD_bytearrayD___reversed__,
    B_SequenceD_bytearray$insert,
    B_SequenceD_bytearray$append,
    B_SequenceD_bytearray$reverse
};
struct B_SequenceD_bytearray B_SequenceD_bytearray_instance = {
    &B_SequenceD_bytearrayG_methods,
    (B_Collection)&B_CollectionD_SequenceD_bytearray_instance,
    (B_Times)&B_TimesD_SequenceD_bytearray_instance
};
B_SequenceD_bytearray B_SequenceD_bytearrayG_witness = &B_SequenceD_bytearray_instance;

struct B_CollectionD_SequenceD_bytearrayG_class B_CollectionD_SequenceD_bytearrayG_methods = {
    "B_CollectionD_SequenceD_bytearray",
    UNASSIGNED,
    ($SuperG_class)&B_CollectionG_methods,
    B_CollectionD_SequenceD_bytearrayD___init__,
    B_CollectionD_SequenceD_bytearrayD___serialize__,
    B_CollectionD_SequenceD_bytearrayD___deserialize__,
    (B_bool (*)(B_CollectionD_SequenceD_bytearray))$default__bool__,
    (B_str (*)(B_CollectionD_SequenceD_bytearray))$default__str__,
    (B_str (*)(B_CollectionD_SequenceD_bytearray))$default__str__,
    B_CollectionD_SequenceD_bytearrayD___iter__,
    B_CollectionD_SequenceD_bytearrayD___fromiter__,
    B_CollectionD_SequenceD_bytearrayD___len__
};
struct B_CollectionD_SequenceD_bytearray B_CollectionD_SequenceD_bytearray_instance = {&B_CollectionD_SequenceD_bytearrayG_methods,(B_Sequence)&B_SequenceD_bytearray_instance};
B_CollectionD_SequenceD_bytearray B_CollectionD_SequenceD_bytearrayG_witness = &B_CollectionD_SequenceD_bytearray_instance;

struct B_TimesD_SequenceD_bytearrayG_class  B_TimesD_SequenceD_bytearrayG_methods = {
    "B_TimesD_SequenceD_bytearray",
    UNASSIGNED,
    ($SuperG_class)&B_TimesG_methods,
    B_TimesD_SequenceD_bytearrayD___init__,
    B_TimesD_SequenceD_bytearrayD___serialize__,
    B_TimesD_SequenceD_bytearrayD___deserialize__,
    (B_bool (*)(B_TimesD_SequenceD_bytearray))$default__bool__,
    (B_str (*)(B_TimesD_SequenceD_bytearray))$default__str__,
    (B_str (*)(B_TimesD_SequenceD_bytearray))$default__str__,
    B_TimesD_SequenceD_bytearrayD___add__,
    (B_bytearray (*)(B_TimesD_SequenceD_bytearray, B_bytearray, B_bytearray))$PlusD___iadd__,
    B_TimesD_SequenceD_bytearrayD___mul__,
    (B_bytearray (*)(B_TimesD_SequenceD_bytearray, B_bytearray, B_int))B_TimesD___imul__,
};
struct B_TimesD_SequenceD_bytearray B_TimesD_SequenceD_bytearray_instance = {&B_TimesD_SequenceD_bytearrayG_methods};
B_TimesD_SequenceD_bytearray B_TimesD_SequenceD_bytearrayG_witness = &B_TimesD_SequenceD_bytearray_instance;

struct B_ContainerD_bytearrayG_class B_ContainerD_bytearrayG_methods = {
    "B_ContainerD_bytearray",
    UNASSIGNED,
    ($SuperG_class)&B_ContainerG_methods,
    B_ContainerD_bytearrayD___init__,
    B_ContainerD_bytearrayD___serialize__,
    B_ContainerD_bytearrayD___deserialize__,
    (B_bool (*)(B_ContainerD_bytearray))$default__bool__,
    (B_str (*)(B_ContainerD_bytearray))$default__str__,
    (B_str (*)(B_ContainerD_bytearray))$default__str__,
    B_ContainerD_bytearrayD___iter__,
    B_ContainerD_bytearrayD___len__,
    B_ContainerD_bytearrayD___contains__,
    B_ContainerD_bytearrayD___containsnot__
};
struct B_ContainerD_bytearray B_ContainerD_bytearray_instance = {&B_ContainerD_bytearrayG_methods};
B_ContainerD_bytearray B_ContainerD_bytearrayG_witness = &B_ContainerD_bytearray_instance;

// init methods for witness classes

void B_CollectionD_SequenceD_bytearrayD___init__(B_CollectionD_SequenceD_bytearray self, B_Sequence master) {
    self->W_Sequence = master;
}

void B_TimesD_SequenceD_bytearrayD___init__(B_TimesD_SequenceD_bytearray self, B_Sequence master) {
    self->W_Sequence = master;
}

void B_SequenceD_bytearrayD___init__(B_SequenceD_bytearray self) {
    self->W_Collection = (B_Collection)$NEW(B_CollectionD_SequenceD_bytearray, (B_Sequence)self);
    self->W_Times = (B_Times)$NEW(B_TimesD_SequenceD_bytearray, (B_Sequence)self);
}

void B_ContainerD_bytearrayD___init__ (B_ContainerD_bytearray wit) {
}


// protocol methods for bytearrays, implementations

// Eq

int B_bytearrayD_eq(B_bytearray a,B_bytearray b) {
    return strcmp((char *)a->str,(char *)b->str)==0;
}

int B_bytearrayD_neq(B_bytearray a,B_bytearray b) {
    return strcmp((char *)a->str,(char *)b->str)!=0;
}

// Ord

int B_bytearrayD_lt(B_bytearray a,B_bytearray b) {
    return strcmp((char *)a->str,(char *)b->str)<0;
}
 
int B_bytearrayD_le(B_bytearray a,B_bytearray b) {
    return strcmp((char *)a->str,(char *)b->str)<=0;
}

int B_bytearrayD_gt(B_bytearray a,B_bytearray b) {
    return strcmp((char *)a->str,(char *)b->str)>0;
}

int B_bytearrayD_ge(B_bytearray a,B_bytearray b) {
    return strcmp((char *)a->str,(char *)b->str)>=0;
}

// Indexed

B_int B_bytearrayD_getitem(B_bytearray self, int ix) {
    int ix0 = ix < 0 ? self->nbytes + ix : ix;
    if (ix0<0 || ix0 >= self->nbytes)
        $RAISE((B_BaseException)$NEW(B_IndexError,to$str("getitem for bytearray: indexing outside array")));
    return toB_int((int)self->str[ix0]);
}
    
void B_bytearrayD_setitem(B_bytearray self, int ix, int val) {
    int ix0 = ix < 0 ? self->nbytes + ix : ix;
    if (ix0<0 || ix0 >= self->nbytes)
        $RAISE((B_BaseException)$NEW(B_IndexError,to$str("setitem for bytearray: indexing outside array")));
    if (val<0 || val>255)
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("setitem for bytearray: value outside [0..255]")));
    self->str[ix0] = (unsigned char)val;
}
  
void B_bytearrayD_delitem(B_bytearray self, int ix) {
    int len = self->nbytes;
    int ix0 = ix < 0 ? len + ix : ix;
    if (ix0 < 0 || ix0 >= len)
        $RAISE((B_BaseException)$NEW(B_IndexError,to$str("delitem for bytearray: indexing outside array")));
    memmove(self->str + ix0,self->str + (ix0 + 1),len-(ix0+1));
    self->nbytes--;
}

// Sliceable

B_bytearray B_bytearrayD_getslice(B_bytearray self, B_slice slc) {
    int len = self->nbytes;
    int start, stop, step, slen;
    normalize_slice(slc, len, &slen, &start, &stop, &step);
    B_bytearray res;
    NEW_UNFILLED_BYTEARRAY(res,slen);
    int t = start;
    for (int i=0; i<slen; i++) {
        B_int w = B_bytearrayD_getitem(self,t);
        B_bytearrayD_setitem(res,i,fromB_int(w));
        t += step;
    }
    return res;
}

void B_bytearrayD_setslice(B_bytearray self, B_slice slc, B_Iterator it) {
    int len = self->nbytes;
    B_bytearray other;
    NEW_UNFILLED_BYTEARRAY(other,0);
    $WORD w;
    while ((w=it->$class->__next__(it)))
        B_bytearrayD_append(other,(B_int)w);
    int olen = other->nbytes; 
    int start, stop, step, slen;
    normalize_slice(slc, len, &slen, &start, &stop, &step);
    if (step != 1 && olen != slen) {
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("setslice for bytearray: illegal slice")));
    }
    int copy = olen <= slen ? olen : slen;
    int t = start;
    for (int i= 0; i<copy; i++) {
        self->str[t] = other->str[i];
        t += step;
    }
    if (olen == slen)
        return;
    // now we know that step=1
    if (olen < slen) {
        memmove(self->str + start + copy,
                self->str + start + slen,
                len-(start+slen));
        self->nbytes-=slen-olen;
        return;
    } else {
        expand_bytearray(self,olen-slen);
        int rest = len - (start+copy);
        int incr = olen - slen;
        memmove(self->str + start + copy + incr,
                self->str + start + copy,
                rest);
        for (int i = copy; i < olen; i++)
            self->str[start+i] = other->str[i];
        self->nbytes += incr;
    }
}

void B_bytearrayD_delslice(B_bytearray self, B_slice slc) {
    int len = self->nbytes;
    int start, stop, step, slen;
    normalize_slice(slc, len, &slen, &start, &stop, &step);
    if (slen==0) return;
    unsigned char *p = self->str + start;
    for (int i=0; i<slen-1; i++) {
        memmove(p,p+i+1,step-1);
        p+=step-1;
    }
    memmove(p,p+slen,len-1-(start+step*(slen-1)));
    self->nbytes-=slen;
    self->str[self->nbytes] = '\0';
}

// Sequence

B_Iterator B_bytearrayD_reversed(B_bytearray self) {
    B_bytearray copy = B_bytearrayD_copy(self);
    B_bytearrayD_reverse(copy);
    return B_bytearrayD_iter(copy);
}

void B_bytearrayD_insert(B_bytearray self, int ix, B_int elem) {
    int len = self->nbytes;
    expand_bytearray(self,1);
    int ix0 = ix < 0 ? (len+ix < 0 ? 0 : len+ix) : (ix < len ? ix : len);
    memmove(self->str + (ix0 + 1),
            self->str + ix0 ,
            len - ix0 + 1); // +1 to move also terminating '\0'
    self->str[ix0] = (unsigned char)fromB_int(elem) & 0xff;
    self->nbytes++;
}

void B_bytearrayD_append(B_bytearray self, B_int elem) {
    expand_bytearray(self,1);
    self->str[self->nbytes++] = (unsigned char)fromB_int(elem) & 0xff;
    self->str[self->nbytes] = '\0';
}

void B_bytearrayD_reverse(B_bytearray self) {
    int len = self->nbytes;
    for (int i = 0; i < len/2; i++) {
        unsigned char tmp = self->str[i];
        self->str[i] = self->str[len-1-i];
        self->str[len-1-i] = tmp;
    }
}

// Iterable

static B_int B_IteratorB_bytearrayD_next(B_IteratorB_bytearray self) {
    return self->nxt >= self->src->nbytes ? NULL : toB_int(self->src->str[self->nxt++]);
}

void B_IteratorB_bytearrayD_init(B_IteratorB_bytearray self, B_bytearray b) {
    self->src = b;
    self->nxt = 0;
}

B_bool B_IteratorB_bytearrayD_bool(B_IteratorB_bytearray self) {
    return $True;
}

B_str B_IteratorB_bytearrayD_str(B_IteratorB_bytearray self) {
    char *s;
    asprintf(&s,"<bytearray iterator object at %p>",self);
    return to$str(s);
}

void B_IteratorB_bytearrayD_serialize(B_IteratorB_bytearray self,$Serial$state state) {
    $step_serialize(self->src,state);
    $step_serialize(toB_int(self->nxt),state);
}

B_IteratorB_bytearray B_IteratorB_bytearray$_deserialize(B_IteratorB_bytearray res, $Serial$state state) {
    if(!res)
        res = $DNEW(B_IteratorB_bytearray,state);
    res->src = (B_bytearray)$step_deserialize(state);
    res->nxt = fromB_int((B_int)$step_deserialize(state));
    return res;
}

struct B_IteratorB_bytearrayG_class B_IteratorB_bytearrayG_methods = {
    "",
    UNASSIGNED,
    ($SuperG_class)&B_IteratorG_methods,
    B_IteratorB_bytearrayD_init,
    B_IteratorB_bytearrayD_serialize,
    B_IteratorB_bytearray$_deserialize,
    B_IteratorB_bytearrayD_bool,
    B_IteratorB_bytearrayD_str,
    B_IteratorB_bytearrayD_str,
    B_IteratorB_bytearrayD_next
};

B_Iterator B_bytearrayD_iter(B_bytearray self) {
    return (B_Iterator)$NEW(B_IteratorB_bytearray,self);
}

// Collection
  
B_bytearray B_bytearrayD_fromiter(B_Iterable wit, $WORD iter) {
    B_bytearray res;
    NEW_UNFILLED_BYTEARRAY(res,0);
    B_Iterator it = wit->$class->__iter__(wit,iter);
    $WORD nxt;
    while ((nxt = it->$class->__next__(it))) {
        B_bytearrayD_append(res,(B_int)nxt);
    }
    return res;
}

B_int B_bytearrayD_len(B_bytearray self) {
    return toB_int(self->nbytes);
}

// Times
 
B_bytearray B_bytearrayD_add(B_bytearray a, B_bytearray b) {
    B_bytearray res;
    NEW_UNFILLED_BYTEARRAY(res,a->nbytes+b->nbytes);
    memcpy(res->str,a->str,a->nbytes);
    memcpy(res->str+a->nbytes,b->str,b->nbytes);
    return res;
}

B_bytearray B_bytearrayD_mul (B_bytearray a, B_int n) {
    int nval = fromB_int(n);
    if (nval <= 0)
        return toB_bytearray("");
    else {
        B_bytearray res;
        NEW_UNFILLED_BYTEARRAY(res, a->nbytes * nval);
        for (int i=0; i<nval; i++)
            memcpy(res->str + i*a->nbytes,a->str,a->nbytes);
        return res;
    }
}
// Container
 
int B_bytearrayD_contains (B_bytearray self, B_int c) {
    for (int i=0; i < self->nbytes; i++) {
        if (self->str[i] == (unsigned char)fromB_int(c))
            return 1;
    }
    return 0;
}

int B_bytearrayD_containsnot (B_bytearray self, B_int c) {
    return !B_bytearrayD_contains(self,c);
}


// General methods, implementations

void B_bytearrayD_init(B_bytearray, B_bytes);
void B_bytearrayD_serialize(B_bytearray,$Serial$state);
B_bytearray B_bytearrayD_deserialize(B_bytearray,$Serial$state);
B_bool B_bytearrayD_bool(B_bytearray);
B_str B_bytearrayD_str(B_bytearray);

B_bytearray B_bytearrayG_new(B_bytes b) {
    return $NEW(B_bytearray, b);
}

void B_bytearrayD_init(B_bytearray self, B_bytes b) {
    int len = b->nbytes;
    self->nbytes = len;
    self->capacity = len;
    self->str = malloc(len+1);
    memcpy(self->str,b->str,len+1);
}
 
B_bool B_bytearrayD_bool(B_bytearray s) {
    return toB_bool(s->nbytes > 0);
};

B_str B_bytearrayD_str(B_bytearray s) {
    B_str bs;
    NEW_UNFILLED_STR(bs,s->nbytes,s->nbytes);
    bs->str = s->str;        // bs may not be a correctly UTF8-encoded string
    B_str as = $ascii(bs);    // but we can use $ascii on it anyhow.
    B_str res;
    int n = as->nbytes + 14; // "bytearray(b'" + "')"
    NEW_UNFILLED_STR(res,n,n);
    memcpy(res->str, "bytearray(b'",12);
    memcpy(&res->str[12],as->str,as->nbytes);
    memcpy(&res->str[n-2],"')",2);
    return res;
}


void B_bytearrayD_serialize(B_bytearray str,$Serial$state state) {
    int nWords = str->nbytes/sizeof($WORD) + 1;         // # $WORDS needed to store str->str, including terminating 0.
    $ROW row = $add_header(BYTEARRAY_ID,1+nWords,state);
    long nbytes = (long)str->nbytes;                    
    memcpy(row->blob,&nbytes,sizeof($WORD));            
    memcpy(row->blob+1,str->str,nbytes+1);
}

B_bytearray B_bytearrayD_deserialize(B_bytearray res, $Serial$state state) {
    $ROW this = state->row;
    state->row =this->next;
    state->row_no++;
    if(!res)
        res = malloc(sizeof(struct B_bytearray));
    long nbytes;
    memcpy(&nbytes,this->blob,sizeof($WORD));
    res->$class = &B_bytearrayG_methods;
    res->nbytes = (long)nbytes;
    res->str = malloc(nbytes+1);
    memcpy(res->str,this->blob+1,nbytes+1);
    return res;
}

// End of bytearray implementation ////////////////////////////////////////////////

// bytes implementation ///////////////////////////////////////////////////////////


// bytes implementation ///////////////////////////////////////////////////////////


// Conversion to and from C strings

B_bytes toB_bytes(char *str) {
    B_bytes res;
    int len = strlen(str);
    NEW_UNFILLED_BYTES(res,len);
    memcpy(res->str,str,len);
    return res;
}

B_bytes toB_bytesD_len(char *str, int len) {
    B_bytes res;
    NEW_UNFILLED_BYTES(res, len);
    memcpy(res->str, str, len);
    return res;
}

unsigned char *fromB_bytes(B_bytes b) {
    return b->str;
}

// Auxiliaries
/*
  static void expand_bytes(B_bytes b,int n) {
  if (b->capacity >= b->nbytes + n)
  return;
  int newcapacity = b->capacity==0 ? 1 : b->capacity;
  while (newcapacity < b->nbytes+n)
  newcapacity <<= 1;
  unsigned char *newstr = b->str==NULL
  ? malloc(newcapacity+1)
  : realloc(b->str,newcapacity+1);
  if (newstr == NULL) {
  $RAISE((B_BaseException)$NEW(B_MemoryError,to$str("memory allocation failed")));
  }
  b->str = newstr;
  b->capacity = newcapacity;
  }  
*/

// Object methods for bytes, prototypes
void B_bytesD_init(B_bytes, B_Iterable,$WORD);
B_bool B_bytesD_bool(B_bytes);
B_str B_bytesD_str(B_bytes);
void B_bytesD_serialize(B_bytes,$Serial$state);
B_bytes B_bytesD_deserialize(B_bytes,$Serial$state);


// bytes methods, prototypes

B_bytes B_bytesD_capitalize(B_bytes s);
B_bytes B_bytesD_center(B_bytes s, B_int width, B_bytes fill);
B_int B_bytesD_count(B_bytes s, B_bytes sub, B_int start, B_int end);
B_str B_bytesD_decode(B_bytes s);
B_bool B_bytesD_endswith(B_bytes s, B_bytes suffix, B_int start, B_int end);
B_bytes B_bytesD_expandtabs(B_bytes s, B_int tabsize);      
B_int B_bytesD_find(B_bytes s, B_bytes sub, B_int start, B_int end);
B_int B_bytesD_index(B_bytes s, B_bytes sub, B_int start, B_int end);
B_bool B_bytesD_isalnum(B_bytes s);
B_bool B_bytesD_isalpha(B_bytes s);
B_bool B_bytesD_isascii(B_bytes s);
B_bool B_bytesD_isdecimal(B_bytes s);
B_bool B_bytesD_isdigit(B_bytes s);
B_bool B_bytesD_isidentifier(B_bytes s);
B_bool B_bytesD_islower(B_bytes s);
B_bool B_bytesD_isnumeric(B_bytes s);
B_bool B_bytesD_isprintable(B_bytes s);
B_bool B_bytesD_isspace(B_bytes s);
B_bool B_bytesD_istitle(B_bytes s);
B_bool B_bytesD_isupper(B_bytes s);
B_bytes B_bytesD_join(B_bytes sep, B_Iterable wit, $WORD iter);
B_bytes B_bytesD_ljust(B_bytes s, B_int width, B_bytes fill); 
B_bytes B_bytesD_lower(B_bytes s);
B_bytes B_bytesD_lstrip(B_bytes s,B_bytes cs); 
B_tuple B_bytesD_partition(B_bytes s, B_bytes sep);
B_bytes B_bytesD_removeprefix(B_bytes s, B_bytes prefix);
B_bytes B_bytesD_removesuffix(B_bytes s, B_bytes suffix);
B_bytes B_bytesD_replace(B_bytes s, B_bytes old, B_bytes new, B_int count);
B_int B_bytesD_rfind(B_bytes s, B_bytes sub, B_int start, B_int end);
B_int B_bytesD_rindex(B_bytes s, B_bytes sub, B_int start, B_int end);
B_bytes B_bytesD_rjust(B_bytes s, B_int width, B_bytes fill);  
B_tuple B_bytesD_rpartition(B_bytes s, B_bytes sep); 
B_bytes B_bytesD_rstrip(B_bytes s,B_bytes cs);
B_list B_bytesD_split(B_bytes s, B_bytes sep, B_int maxsplit);  
B_list B_bytesD_splitlines(B_bytes s, B_bool keepends); 
B_bool B_bytesD_startswith(B_bytes s, B_bytes prefix, B_int start, B_int end); 
B_bytes B_bytesD_strip(B_bytes s,B_bytes cs);
B_bytes B_bytesD_upper(B_bytes s);
B_bytes B_bytesD_zfill(B_bytes s, B_int width);

// Method table

struct B_bytesG_class B_bytesG_methods =
    {"B_bytes",UNASSIGNED,($SuperG_class)&B_valueG_methods, B_bytesD_init, B_bytesD_serialize, B_bytesD_deserialize, B_bytesD_bool,
     B_bytesD_str,  B_bytesD_str, B_bytesD_capitalize, B_bytesD_center, B_bytesD_count,  B_bytesD_decode, B_bytesD_endswith,
     B_bytesD_expandtabs, B_bytesD_find, B_bytesD_index,
     B_bytesD_isalnum, B_bytesD_isalpha, B_bytesD_isascii, B_bytesD_isdigit, B_bytesD_islower, B_bytesD_isspace,
     B_bytesD_istitle, B_bytesD_isupper, B_bytesD_join, B_bytesD_ljust, B_bytesD_lower, B_bytesD_lstrip, B_bytesD_partition,
     B_bytesD_replace, B_bytesD_rfind, B_bytesD_rindex, B_bytesD_rjust,
     B_bytesD_rpartition, B_bytesD_rstrip, B_bytesD_split, B_bytesD_splitlines, B_bytesD_startswith, B_bytesD_strip, B_bytesD_upper, B_bytesD_zfill};

/*
  struct B_bytesG_class B_bytesG_methods =
  {"B_bytes",UNASSIGNED,($SuperG_class)&B_valueG_methods, B_bytesD_init, B_bytesD_serialize, B_bytesD_deserialize, B_bytesD_bool,
  B_bytesD_str,
  (B_bytes (*)(B_bytes))B_bytearrayD_capitalize,
  (B_bytes (*)(B_bytes,B_int,B_bytes))B_bytearrayD_center,
  (B_int (*)(B_bytes,B_bytes,B_int,B_int))B_bytearrayD_count,
  (B_str (*)(B_bytes))B_bytearrayD_decode,
  (B_bool (*)(B_bytes,B_bytes,B_int,B_int))B_bytearrayD_endswith,
  (B_bytes (*)(B_bytes,B_int))B_bytearrayD_expandtabs,
  (B_int (*)(B_bytes,B_bytes,B_int,B_int))B_bytearrayD_find,
  (B_int (*)(B_bytes,B_bytes,B_int,B_int))B_bytearrayD_index,
  (B_bool (*)(B_bytes))B_bytearrayD_isalnum,
  (B_bool (*)(B_bytes))B_bytearrayD_isalpha,
  (B_bool (*)(B_bytes))B_bytearrayD_isascii,
  (B_bool (*)(B_bytes))B_bytearrayD_isdigit,
  (B_bool (*)(B_bytes))B_bytearrayD_islower,
  (B_bool (*)(B_bytes))B_bytearrayD_isspace,
  (B_bool (*)(B_bytes))B_bytearrayD_istitle,
  (B_bool (*)(B_bytes))B_bytearrayD_isupper,
  (B_bytes (*)(B_bytes,B_Iterable,$WORD))B_bytearrayD_join,
  (B_bytes (*)(B_bytes,B_int,B_bytes))B_bytearrayD_ljust,
  (B_bytes (*)(B_bytes))B_bytearrayD_lower,
  (B_bytes (*)(B_bytes,B_bytes))B_bytearrayD_lstrip,
  (B_tuple (*)(B_bytes,B_bytes))B_bytearrayD_partition,
  (B_bytes (*)(B_bytes,B_bytes,B_bytes,B_int))B_bytearrayD_replace,
  (B_int (*)(B_bytes,B_bytes,B_int,B_int))B_bytearrayD_rfind,
  (B_int (*)(B_bytes,B_bytes,B_int,B_int))B_bytearrayD_rindex,
  (B_bytes (*)(B_bytes,B_int,B_bytes))B_bytearrayD_rjust,
  (B_tuple (*)(B_bytes,B_bytes))B_bytearrayD_rpartition,
  (B_bytes (*)(B_bytes,B_bytes))B_bytearrayD_rstrip,
  (B_list (*)(B_bytes,B_bytes,B_int))B_bytearrayD_split,
  (B_list (*)(B_bytes,B_bool))B_bytearrayD_splitlines,
  (B_bool (*)(B_bytes,B_bytes,B_int,B_int))B_bytearrayD_startswith,
  (B_bytes (*)(B_bytes,B_bytes))B_bytearrayD_strip,
  (B_bytes (*)(B_bytes))B_bytearrayD_upper,
  (B_bytes (*)(B_bytes,B_int))B_bytearrayD_zfill};
*/


// Bytes methods, implementations


static B_bytes B_bytesD_copy(B_bytes s) {
    B_bytes res;
    NEW_UNFILLED_BYTES(res,s->nbytes);
    res->nbytes = s->nbytes;
    memcpy(res->str,s->str,s->nbytes);
    return res;
}

 
B_bytes B_bytesD_capitalize(B_bytes s) {
    if (s->nbytes==0) {
        return s;
    }
    B_bytes res;
    NEW_UNFILLED_BYTES(res,s->nbytes);
    res->str[0] = toupper(s->str[0]);
    for (int i = 1; i < s->nbytes; i++) 
        res->str[i] = tolower(s->str[i]);
    return res;
}

B_bytes B_bytesD_center(B_bytes s, B_int width, B_bytes fill) {
    int wval = fromB_int(width);
    if (!fill) fill = toB_bytes(" ");
    if (fill->nbytes != 1) {
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("center: fill bytes not single char")));
    }
    if (wval <= s->nbytes) {
        return s;
    }
    int pad = (wval-s->nbytes);
    int padleft = pad/2; 
    int padright = pad-padleft;
    int sbytes = s->nbytes;
    B_bytes res;
    NEW_UNFILLED_BYTES(res, wval);
    unsigned char c = fill->str[0];
    unsigned char *p = res->str;
    p += padleft+sbytes;
    for (int i = 0; i<padright; i++) {
        p[i] = c;
    }
    memcpy(res->str,p,padleft);
    p -= sbytes;
    memcpy(p,s->str,sbytes);
    return res;
}

B_int B_bytesD_count(B_bytes s, B_bytes sub, B_int start, B_int end) {
    B_int st = start;
    B_int en = end;
    if (fix_start_end(s->nbytes,&st,&en) < 0) return toB_int(0);
    int stval = fromB_int(st);
    unsigned char *p = &s->str[stval];
    unsigned char *q = &p[fromB_int(en)-stval];
    int res = 0;
    int n = bmh(p,sub->str,q-p,sub->nbytes);
    while (n>=0) {
        res++;
        p += n + (sub->nbytes>0 ? sub->nbytes : 1);
        n = bmh(p,sub->str,q-p,sub->nbytes);
    }
    return toB_int(res);
}

B_str B_bytesD_decode(B_bytes s) {
    return to$str((char*)s->str);
}

B_bool B_bytesD_endswith(B_bytes s, B_bytes sub, B_int start, B_int end) {
    B_int st = start;
    B_int en = end;
    if (fix_start_end(s->nbytes,&st,&en) < 0) return $False;
    unsigned char *p = &s->str[fromB_int(en)-sub->nbytes];
    unsigned char *q = sub->str;
    for (int i=0; i<sub->nbytes; i++) {
        if (*p == 0 || *p++ != *q++) {
            return $False;
        }
    }
    return $True;
}

B_bytes B_bytesD_expandtabs(B_bytes s, B_int tabsz){
    int pos = 0;
    int expanded = 0;
    int tabsize = fromB_int(tabsz);
    tabsize = tabsize <= 0 ? 1 : tabsize;
    unsigned char buffer[tabsize * s->nbytes];
    unsigned char *p = s->str;
    unsigned char *q = buffer;
    for (int i=0; i<s->nbytes; i++) {
        if (*p == '\t') {
            int n = tabsize - pos % tabsize;
            for (int j=0; j < n; j++) {
                *q++ = ' ';
            }
            p++;
            expanded += n-1;
            pos+=n;
        } else if (*p=='\n' || *p == '\r') {
            *q++ = *p++;
            pos = 0;
        } else {
            for (int j=0; j< byte_length2(*p); j++) {
                *q++ = *p++;
                pos++;
            }
        }
    }
    B_bytes res;
    NEW_UNFILLED_BYTES(res,s->nbytes+expanded);
    memcpy(res->str,buffer,s->nbytes+expanded);
    return res;
}

B_int B_bytesD_find(B_bytes s, B_bytes sub, B_int start, B_int end) {
    B_int st = start;
    B_int en = end;
    if (fix_start_end(s->nbytes,&st,&en) < 0) return toB_int(-1);
    unsigned char *p = &s->str[fromB_int(st)];
    unsigned char *q = &s->str[fromB_int(en)];
    int n = bmh(p,sub->str,q-p,sub->nbytes);
    if (n<0) return toB_int(-1);
    return toB_int(n+p-s->str);
}


B_int B_bytesD_index(B_bytes s, B_bytes sub, B_int start, B_int end) {
    B_int n = B_bytesD_find(s,sub,start,end);
    if (fromB_int(n)<0) {
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("index: substring not found")));
    }
    return n;
}

B_bool B_bytesD_isalnum(B_bytes s) {
    if (s->nbytes==0)
        return $False;
    for (int i=0; i<s->nbytes; i++) {
        unsigned char c = s->str[i];
        if (c < '0' || c > 'z' || (c > '9' && c < 'A') || (c > 'Z' && c < 'a'))
            return $False;
    }
    return $True;
}

B_bool B_bytesD_isalpha(B_bytes s) {
    if (s->nbytes==0)
        return $False;
    for (int i=0; i<s->nbytes; i++) {
        unsigned char c = s->str[i];
        if (c < 'A' || c > 'z' || (c > 'Z' && c < 'a'))
            return $False;
    }
    return $True;
}

B_bool B_bytesD_isascii(B_bytes s) {
    for (int i=0; i<s->nbytes; i++) {
        unsigned char c = s->str[i];
        if (c > 0x7f)
            return $False;
    }
    return $True;
}

B_bool B_bytesD_isdigit(B_bytes s) {
    if (s->nbytes==0)
        return $False;
    for (int i=0; i<s->nbytes; i++) {
        unsigned char c = s->str[i];
        if (c<'0' || c > '9')
            return $False;
    }
    return $True;
}
 

B_bool B_bytesD_islower(B_bytes s) {
    int has_lower = 0;
    for (int i=0; i < s->nbytes; i++) {
        unsigned char c = s->str[i];
        if (c >= 'A' && c <= 'Z')
            return $False;
        if (c >= 'a' && c <= 'z')
            has_lower = 1;
    }
    return toB_bool(has_lower);
}

B_bool B_bytesD_isspace(B_bytes s) {
    if (s->nbytes==0)
        return $False;
    for (int i=0; i<s->nbytes; i++) {
        unsigned char c = s->str[i];
        if (c !=' ' && c != '\t' && c != '\n' && c != '\r' && c != '\x0b' && c != '\f')
            return $False;
    }
    return $True;
}

B_bool B_bytesD_istitle(B_bytes s) {
    if (s->nbytes==0)
        return $False;
    int incasedrun = 0;
    for (int i=0; i < s->nbytes; i++) {
        unsigned char c = s->str[i];
        if (c >='A' && c <= 'Z') {
            if (incasedrun)
                return $False;
            incasedrun = 1;
        } else if (c >='a' && c <= 'z') {
            if (!incasedrun)
                return $False;
        } else
            incasedrun = 0;
    }
    return $True;
}

B_bool B_bytesD_isupper(B_bytes s) {
    int has_upper = 0;
    for (int i=0; i < s->nbytes; i++) {
        unsigned char c = s->str[i];
        if (c >= 'a' && c <= 'z')
            return $False;
        if (c >= 'a' && c <= 'z')
            has_upper = 1;
    }
    return toB_bool(has_upper);
}

B_bytes B_bytesD_join(B_bytes s, B_Iterable wit, $WORD iter) {
    int totbytes = 0;
    B_list lst = B_listD_fromiter(wit->$class->__iter__(wit,iter));
    B_bytes nxt;
    int len = lst->length;
    for (int i=0; i<len; i++) {
        nxt = (B_bytes)lst->data[i];
        totbytes += nxt->nbytes;
    }
    if (len > 1) {
        totbytes += (len-1) * s->nbytes;
    }
    B_bytes res;
    NEW_UNFILLED_BYTES(res,totbytes);
    if (len > 0) {
        nxt = (B_bytes)lst->data[0];
        unsigned char *p = res->str;
        memcpy(p,nxt->str,nxt->nbytes);
        p += nxt->nbytes;
        for (int i=1; i<len; i++) {
            nxt = (B_bytes)lst->data[i];
            memcpy(p,s->str,s->nbytes);
            p += s->nbytes;
            memcpy(p,nxt->str,nxt->nbytes);
            p += nxt->nbytes;
        }
    }
    return res;
}

B_bytes B_bytesD_ljust(B_bytes s, B_int width, B_bytes fill) {
    int wval = fromB_int(width);
    if (fill->nbytes != 1) {
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("bytes ljust: fill array not single char")));
    }
    if (wval <= s->nbytes) {
        return B_bytesD_copy(s);
    }
    B_bytes res;
    NEW_UNFILLED_BYTES(res,wval);
    memcpy(res->str,s->str,s->nbytes);
    unsigned char c = fill->str[0];
    for (int i = s->nbytes; i<wval; i++) {
        res->str[i] = c;
    }
    return res;
}

B_bytes B_bytesD_lower(B_bytes s) {
    B_bytes res;
    NEW_UNFILLED_BYTES(res,s->nbytes);
    for (int i=0; i< s->nbytes; i++)
        res->str[i] = tolower(res->str[i]);
    return res;
}

B_bytes B_bytesD_lstrip(B_bytes s, B_bytes cs) {
    if (!cs)
        cs = toB_bytes(" \t\n\r\x0b\x0c");
    int nstrip = 0;
    for (int i=0; i<s->nbytes; i++) {
        unsigned char c = s->str[i];
        int found = 0;
        for (int j=0; j<cs->nbytes; j++)
            if (c == cs->str[j]) {
                found = 1;
                break;
            }
        if (!found)
            break;
        nstrip++;
    }
    B_bytes res;
    NEW_UNFILLED_BYTES(res,s->nbytes-nstrip);
    memcpy(res->str,s->str+nstrip,res->nbytes);       
    return res;
}


B_tuple B_bytesD_partition(B_bytes s, B_bytes sep) {
    int n = fromB_int(B_bytesD_find(s,sep,NULL,NULL));
    if (n<0) {
        return $NEWTUPLE(3,s,toB_bytes(""),toB_bytes(""));
    } else {
        int nb = bmh(s->str,sep->str,s->nbytes,sep->nbytes);
        B_bytes ls;
        NEW_UNFILLED_BYTES(ls,nb);
        memcpy(ls->str,s->str,nb);
        B_bytes rs;
        int nbr = s->nbytes - sep->nbytes - nb;
        NEW_UNFILLED_BYTES(rs,nbr);
        memcpy(rs->str,s->str+nb+sep->nbytes,nbr);
        return $NEWTUPLE(3,ls,sep,rs);
    }
}


B_bytes B_bytesD_removeprefix(B_bytes s, B_bytes prefix) {
    int bytes_to_remove;
    if (prefix->nbytes > s->nbytes || bcmp(s->str,prefix->str,prefix->nbytes))
        bytes_to_remove = 0;
    else
        bytes_to_remove = prefix->nbytes;
    B_bytes res;
    int resbytes = s->nbytes - bytes_to_remove;
    NEW_UNFILLED_BYTES(res,resbytes);
    memcpy(res->str,s->str+bytes_to_remove,resbytes);
    return res;
}

B_bytes B_bytesD_removesuffix(B_bytes s, B_bytes suffix) {
    int bytes_to_remove;
    if (suffix->nbytes > s->nbytes || bcmp(s->str+s->nbytes-suffix->nbytes,suffix->str,suffix->nbytes))
        bytes_to_remove = 0;
    else
        bytes_to_remove = suffix->nbytes;
    B_bytes res;
    int resbytes = s->nbytes - bytes_to_remove;
    NEW_UNFILLED_BYTES(res,resbytes);
    memcpy(res->str,s->str,resbytes);
    return res;
}
B_bytes B_bytesD_replace(B_bytes s, B_bytes old, B_bytes new, B_int count) {
    if (count==NULL)
        count = toB_int(INT_MAX);
    int c = fromB_int(B_bytesD_count(s,old,NULL,NULL));
    int c0 = fromB_int(count) < c ? fromB_int(count) : c;
    if (c0==0){
        return B_bytesD_copy(s);
    }
    int nbytes = s->nbytes + c0*(new->nbytes-old->nbytes);
    B_bytes res;
    NEW_UNFILLED_BYTES(res,nbytes);
    unsigned char *p = s->str;
    unsigned char *q = res->str;
    unsigned char *pold = old->str;
    unsigned char *pnew = new->str;
    int plen = s->nbytes;
    int n;
    for (int i=0; i<c0; i++) {
        n = i>0 && old->nbytes==0 ? 1 : bmh(p,pold,plen,old->nbytes);
        if (n>0) {
            memcpy(q,p,n);
            p+=n; q+=n;
        }
        memcpy(q,pnew,new->nbytes);
        p += old->nbytes;
        q += new->nbytes;
        plen -= n+old->nbytes;
    }
    if (plen>0)
        memcpy(q,p,plen);
    return res;
}
      

B_int B_bytesD_rfind(B_bytes s, B_bytes sub, B_int start, B_int end) {
    B_int st = start;
    B_int en = end;
    if (fix_start_end(s->nbytes,&st,&en) < 0) return toB_int(-1);
    unsigned char *p = &s->str[fromB_int(st)];
    unsigned char *q = &s->str[fromB_int(en)];
    int n = rbmh(p,sub->str,q-p,sub->nbytes);
    if (n<0) return toB_int(-1);
    return toB_int(n+p-s->str);
}


B_int B_bytesD_rindex(B_bytes s, B_bytes sub, B_int start, B_int end) {
    B_int n = B_bytesD_rfind(s,sub,start,end);
    if (fromB_int(n)<0) {
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("rindex for bytes: substring not found")));
    };
    return n;
}

B_bytes B_bytesD_rjust(B_bytes s, B_int width, B_bytes fill) {
    if (fill->nbytes != 1) {
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("rjust: fill string not single char")));
    }
    int wval = fromB_int(width); 
    if (wval <= s->nbytes) {
        return B_bytesD_copy(s);
    }
    int pad = (wval-s->nbytes);
    B_bytes res;
    NEW_UNFILLED_BYTES(res,wval);
    unsigned char c = fill->str[0];
    for (int i = 0; i<pad; i++) {
        res->str[i] = c;
    }
    memcpy(&res->str[pad],s->str,s->nbytes);
    return res;
}
                                
B_tuple B_bytesD_rpartition(B_bytes s, B_bytes sep) {
    int n = fromB_int(B_bytesD_rfind(s,sep,NULL,NULL));
    if (n<0) {
        return $NEWTUPLE(3,toB_bytes(""),toB_bytes(""),s);
    } else {
        int nb = rbmh(s->str,sep->str,s->nbytes,sep->nbytes);
        B_bytes ls;
        NEW_UNFILLED_BYTES(ls,nb);
        memcpy(ls->str,s->str,nb);
        int nbr = s->nbytes - sep->nbytes - nb;
        B_bytes rs;    
        NEW_UNFILLED_BYTES(rs,nbr);
        memcpy(rs->str,s->str+nb+sep->nbytes,nbr);
        return  $NEWTUPLE(3,ls,sep,rs);
    }
}

B_bytes B_bytesD_rstrip(B_bytes s, B_bytes cs) {
    if (!cs)
        cs = toB_bytes(" \t\n\r\x0b\x0c");
    int nstrip = 0;
    for (int i=s->nbytes-1; i>=0; i--) {
        unsigned char c = s->str[i];
        int found = 0;
        for (int j=0; j<cs->nbytes; j++)
            if (c == cs->str[j]) {
                found = 1;
                break;
            }
        if (!found)
            break;
        nstrip++;
    }
    B_bytes res;
    NEW_UNFILLED_BYTES(res,s->nbytes-nstrip);
    memcpy(res->str,s->str,res->nbytes);       
    return res;
}
 
B_list B_bytesD_split(B_bytes s, B_bytes sep, B_int maxsplit) {
    B_list res = $NEW(B_list,NULL,NULL);
    if (maxsplit == NULL || fromB_int(maxsplit) < 0) maxsplit = toB_int(INT_MAX); 
    if (sep == NULL) {
        unsigned char *p = s->str;
        if (s->nbytes==0) {
            return res;
        }
        int inword = 0;
        unsigned char *q;
        while (p < s->str + s->nbytes) {
            if  (*p !=' ' && *p != '\t' && *p != '\n' && *p != '\r' && *p != '\x0b' && *p != '\f') {
                if (!inword) {
                    inword = 1;
                    q = p;
                    if (B_listD_len(res) == fromB_int(maxsplit))
                        break; // we have now removed leading whitespace in remainder
                } 
            } else {
                if (inword) {
                    inword = 0;
                    B_bytes word;
                    NEW_UNFILLED_BYTES(word,p-q);
                    memcpy(word->str,q,p-q);
                    B_listD_append(res,word);
                }
            }
            p++;
        }
        // this if statement should be simplified; almost code duplication.
        if (p < s->str + s->nbytes) { // we did not break out of the while loop
            if (inword) {
                B_bytes word;
                NEW_UNFILLED_BYTES(word,p-q);
                memcpy(word->str,q,p-q);
                B_listD_append(res,word);
            }
        } else {
            B_bytes word;
            p = s->str+s->nbytes;
            NEW_UNFILLED_BYTES(word,p-q);
            memcpy(word->str,q,p-q);
            B_listD_append(res,word);
        }
        return res;
    } else { // separator given
        if (sep->nbytes==0) {
            $RAISE((B_BaseException)$NEW(B_ValueError,to$str("split for bytes: separator is empty string")));
        }
        if (s->nbytes==0) { // for some unfathomable reason, this is the behaviour of the Python method
            B_listD_append(res,null_str);
            return res;
        }
        B_bytes ls, rs, ssep;
        rs = s;
        // Note: This builds many intermediate rs strings...
        while (rs->nbytes>0 && B_listD_len(res) < fromB_int(maxsplit)) {
            B_tuple t = B_bytesD_partition(rs,sep);
            ssep = (B_bytes)t->components[1];
            rs =  (B_bytes)t->components[2];
            B_listD_append(res,(B_bytes)t->components[0]);
        }
        if (ssep->nbytes>0)
            B_listD_append(res,rs);
        return res;
    }
}

B_list B_bytesD_splitlines(B_bytes s, B_bool keepends) {
    if (!keepends)
        keepends = $False;
    B_list res = $NEW(B_list,NULL,NULL);
    if (s->nbytes==0) {
        return res;
    }
    int winend;
    unsigned char *p = s->str;
    unsigned char *q = p;
    while (p < s->str + s->nbytes) {
        if (*p != '\n' && *p != '\r') {
            p++;
        } else {
            B_bytes line;
            winend = *p=='\r' && *(p+1)=='\n';
            int size = p-q + (keepends->val ? 1 + winend : 0);
            NEW_UNFILLED_BYTES(line,size);
            memcpy(line->str,q,size);
            p+= 1 + winend;
            q = p;
            B_listD_append(res,line);
        }
    }
    if (q < p) {
        B_bytes line;
        NEW_UNFILLED_BYTES(line,p-q);
        memcpy(line->str,q,p-q);
        B_listD_append(res,line);
    }
    return res;
} 

B_bool B_bytesD_startswith(B_bytes s, B_bytes sub, B_int start, B_int end) {
    B_int st = start;
    B_int en = end;
    if (fix_start_end(s->nbytes,&st,&en) < 0) return $False;
    unsigned char *p = s->str + fromB_int(st);
    if (p+sub->nbytes >= s->str+s->nbytes) return $False;
    unsigned char *q = sub->str;
    for (int i=0; i<sub->nbytes; i++) {
        if (p >= s->str + fromB_int(en) || *p++ != *q++) {
            return $False;
        }
    }
    return $True;
}


B_bytes B_bytesD_strip(B_bytes s, B_bytes cs) {
    return B_bytesD_lstrip(B_bytesD_rstrip(s,cs),cs);
}

B_bytes B_bytesD_upper(B_bytes s) {
    B_bytes res;
    NEW_UNFILLED_BYTES(res,s->nbytes);
    for (int i=0; i< s->nbytes; i++)
        res->str[i] = toupper(res->str[i]);
  
    return res;
}

B_bytes B_bytesD_zfill(B_bytes s, B_int width) {
    int wval = fromB_int(width);
    int fill = wval - s->nbytes;
    if (fill < 0)
        return B_bytesD_copy(s);
    B_bytes res;
    NEW_UNFILLED_BYTES(res,wval);
    unsigned char *p = s->str;
    unsigned char *q = res->str;
    int hassign = (*p=='+' | *p=='-');
    if (hassign) {
        *q = *p;
        q++;
    }
    for (int i=0; i < fill; i++) 
        *q++ = '0';
    memcpy(res->str+hassign+fill,s->str+hassign,s->nbytes-hassign);
    return res;
}

// protocol methods; string implementation prototypes ///////////////////////////////////////////////////

int B_bytesD_eq(B_bytes,B_bytes);
int B_bytesD_neq(B_bytes,B_bytes);
int B_bytesD_lt(B_bytes,B_bytes);
int B_bytesD_le(B_bytes,B_bytes);
int B_bytesD_gt(B_bytes,B_bytes);
int B_bytesD_ge(B_bytes,B_bytes);

B_Iterator B_bytesD_iter(B_bytes);

B_bytes B_bytesD_fromiter(B_Iterable, $WORD);
B_int B_bytesD_len(B_bytes str);

int B_bytesD_contains (B_bytes, B_bytes);
int B_bytesD_containsnot (B_bytes, B_bytes);

B_int B_bytesD_getitem(B_bytes, int);
B_bytes B_bytesD_getslice(B_bytes, B_slice);
 
B_bytes B_bytesD_add(B_bytes, B_bytes);
B_bytes B_bytesD_mul(B_bytes, B_int);

// Protocol instances, using above prototypes 

// Ord

void B_OrdD_bytesD___serialize__(B_OrdD_bytes self, $Serial$state state) {
}

B_OrdD_bytes B_OrdD_bytesD___deserialize__(B_OrdD_bytes self, $Serial$state state) {
    B_OrdD_bytes res = $DNEW(B_OrdD_bytes,state);
    return res;
}

B_OrdD_bytes B_OrdD_bytesG_new() {
    return $NEW(B_OrdD_bytes);
}

B_bool B_OrdD_bytesD___eq__ (B_OrdD_bytes wit, B_bytes a, B_bytes b) {
    return toB_bool(B_bytesD_eq(a,b));
}

B_bool B_OrdD_bytesD___ne__ (B_OrdD_bytes wit, B_bytes a, B_bytes b) {
    return  toB_bool(B_bytesD_neq(a,b));
}

B_bool B_OrdD_bytesD___lt__ (B_OrdD_bytes wit, B_bytes a, B_bytes b) {
    return toB_bool(B_bytesD_lt(a,b));
}

B_bool B_OrdD_bytesD___le__ (B_OrdD_bytes wit, B_bytes a, B_bytes b){
    return toB_bool(B_bytesD_le(a,b));
}

B_bool B_OrdD_bytesD___gt__ (B_OrdD_bytes wit, B_bytes a, B_bytes b){
    return toB_bool(B_bytesD_gt(a,b));
}

B_bool B_OrdD_bytesD___ge__ (B_OrdD_bytes wit, B_bytes a, B_bytes b){
    return toB_bool(B_bytesD_ge(a,b));
}

// Container

void B_ContainerD_bytesD___serialize__(B_ContainerD_bytes self, $Serial$state state) {
}

B_ContainerD_bytes B_ContainerD_bytesD___deserialize__(B_ContainerD_bytes self, $Serial$state state) {
    return  $DNEW(B_ContainerD_bytes,state);
}

B_Iterator B_ContainerD_bytesD___iter__ (B_ContainerD_bytes wit, B_bytes str) {
    return B_bytesD_iter(str);
}

B_bytes B_ContainerD_bytesD___fromiter__ (B_ContainerD_bytes wit, B_Iterable wit2, $WORD iter) {
    return B_bytesD_join(toB_bytes(""),wit2,iter);
}

B_int B_ContainerD_bytesD___len__ (B_ContainerD_bytes wit, B_bytes str) {
    return B_bytesD_len(str);
}

B_bool B_ContainerD_bytesD___contains__ (B_ContainerD_bytes wit, B_bytes str, B_bytes sub) {
    return toB_bool(B_bytesD_contains(str, sub));
}

B_bool B_ContainerD_bytesD___containsnot__ (B_ContainerD_bytes wit, B_bytes str, B_bytes sub) {
    return toB_bool(B_bytesD_containsnot(str, sub));
}  

// Sliceable

void B_SliceableD_bytesD___serialize__(B_SliceableD_bytes self, $Serial$state state) {
}

B_SliceableD_bytes B_SliceableD_bytesD___deserialize__(B_SliceableD_bytes self, $Serial$state state) {
    B_SliceableD_bytes res = $DNEW(B_SliceableD_bytes,state);
    return res;
}

B_SliceableD_bytes B_SliceableD_bytesG_new() {
    return $NEW(B_SliceableD_bytes);
}
B_int B_SliceableD_bytesD___getitem__ (B_SliceableD_bytes wit, B_bytes str, B_int i) {
    return B_bytesD_getitem(str,fromB_int(i));
}

void B_SliceableD_bytesD___setitem__ (B_SliceableD_bytes wit, B_bytes str, B_int i, B_bytes val) {
    fprintf(stderr,"Internal error: call to mutating method setitem on string");
    exit(-1);
}

void B_SliceableD_bytesD___delitem__ (B_SliceableD_bytes wit, B_bytes str, B_int i) {
    fprintf(stderr,"Internal error: call to mutating method delitem on string");
    exit(-1);
}

B_bytes B_SliceableD_bytesD___getslice__ (B_SliceableD_bytes wit, B_bytes str, B_slice slc) {
    return B_bytesD_getslice(str,slc);
}

void B_SliceableD_bytesD___setslice__ (B_SliceableD_bytes wit, B_bytes str, B_Iterable wit2, B_slice slc, $WORD iter) {
    fprintf(stderr,"Internal error: call to mutating method setslice on string");
    exit(-1);
}

void B_SliceableD_bytesD___delslice__ (B_SliceableD_bytes wit, B_bytes str, B_slice slc) {
    fprintf(stderr,"Internal error: call to mutating method delslice on string");
    exit(-1);
}

// Times

void B_TimesD_bytesD___serialize__(B_TimesD_bytes self, $Serial$state state) {
}

B_TimesD_bytes B_TimesD_bytesD___deserialize__(B_TimesD_bytes self, $Serial$state state) {
    B_TimesD_bytes res = $DNEW(B_TimesD_bytes,state);
    return res;
}

B_bytes B_TimesD_bytesD___add__ (B_TimesD_bytes wit, B_bytes a, B_bytes b) {
    return B_bytesD_add(a,b);
}

B_bytes B_TimesD_bytesD___mul__ (B_TimesD_bytes wit, B_bytes a, B_int n) {
    return B_bytesD_mul(a,n);
}

// Hashable

void B_HashableD_bytesD___serialize__(B_HashableD_bytes self, $Serial$state state) {
}

B_HashableD_bytes B_HashableD_bytesD___deserialize__(B_HashableD_bytes self, $Serial$state state) {
    B_HashableD_bytes res = $DNEW(B_HashableD_bytes,state);
    return res;
}

B_bool B_HashableD_bytesD___eq__ (B_HashableD_bytes wit, B_bytes a, B_bytes b) {
    return toB_bool(B_bytesD_eq(a,b));
}

B_HashableD_bytes B_HashableD_bytesG_new() {
    return $NEW(B_HashableD_bytes);
}
B_bool B_HashableD_bytesD___ne__ (B_HashableD_bytes wit, B_bytes a, B_bytes b) {
    return toB_bool(B_bytesD_neq(a,b));
}

B_int B_HashableD_bytesD___hash__(B_HashableD_bytes wit, B_bytes str) {
    return toB_int(B_bytesD_hash(str));
}


// Method tables for witness classes

struct B_OrdD_bytesG_class  B_OrdD_bytesG_methods = {
    "B_OrdD_bytes",
    UNASSIGNED,
    ($SuperG_class)&B_OrdG_methods,
    (void (*)(B_OrdD_bytes))$default__init__,
    B_OrdD_bytesD___serialize__,
    B_OrdD_bytesD___deserialize__,
    (B_bool (*)(B_OrdD_bytes))$default__bool__,
    (B_str (*)(B_OrdD_bytes))$default__str__,
    (B_str (*)(B_OrdD_bytes))$default__str__,
    B_OrdD_bytesD___eq__,
    B_OrdD_bytesD___ne__,
    B_OrdD_bytesD___lt__,
    B_OrdD_bytesD___le__,
    B_OrdD_bytesD___gt__,
    B_OrdD_bytesD___ge__
};
struct B_OrdD_bytes B_OrdD_bytes_instance = {&B_OrdD_bytesG_methods};
B_OrdD_bytes B_OrdD_bytesG_witness = &B_OrdD_bytes_instance;

struct B_ContainerD_bytesG_class  B_ContainerD_bytesG_methods = {
    "B_ContainerD_bytes",
    UNASSIGNED,
    ($SuperG_class)&B_ContainerG_methods,
    B_ContainerD_bytesD___init__,
    B_ContainerD_bytesD___serialize__,
    B_ContainerD_bytesD___deserialize__,
    (B_bool (*)(B_ContainerD_bytes))$default__bool__,
    (B_str (*)(B_ContainerD_bytes))$default__str__,
    (B_str (*)(B_ContainerD_bytes))$default__str__,
    B_ContainerD_bytesD___iter__,
    NULL,
    B_ContainerD_bytesD___len__,
    B_ContainerD_bytesD___contains__,
    B_ContainerD_bytesD___containsnot__
};
struct B_ContainerD_bytes B_ContainerD_bytes_instance = {&B_ContainerD_bytesG_methods};
B_ContainerD_bytes B_ContainerD_bytesG_witness = &B_ContainerD_bytes_instance;


struct B_SliceableD_bytesG_class  B_SliceableD_bytesG_methods = {
    "B_SliceableD_bytes",
    UNASSIGNED,
    ($SuperG_class)&B_SliceableG_methods,
    (void (*)(B_SliceableD_bytes))$default__init__,
    B_SliceableD_bytesD___serialize__,
    B_SliceableD_bytesD___deserialize__,
    (B_bool (*)(B_SliceableD_bytes))$default__bool__,
    (B_str (*)(B_SliceableD_bytes))$default__str__,
    (B_str (*)(B_SliceableD_bytes))$default__str__,
    B_SliceableD_bytesD___getitem__,
    B_SliceableD_bytesD___setitem__,
    B_SliceableD_bytesD___delitem__,
    B_SliceableD_bytesD___getslice__,
    B_SliceableD_bytesD___setslice__,
    B_SliceableD_bytesD___delslice__
};
struct B_SliceableD_bytes B_SliceableD_bytes_instance = {&B_SliceableD_bytesG_methods};
B_SliceableD_bytes B_SliceableD_bytesG_witness = &B_SliceableD_bytes_instance;

struct B_TimesD_bytesG_class  B_TimesD_bytesG_methods = {
    "B_TimesD_bytes",
    UNASSIGNED,
    ($SuperG_class)&B_TimesG_methods,
    (void (*)(B_TimesD_bytes))$default__init__,
    B_TimesD_bytesD___serialize__,
    B_TimesD_bytesD___deserialize__,
    (B_bool (*)(B_TimesD_bytes))$default__bool__,
    (B_str (*)(B_TimesD_bytes))$default__str__,
    (B_str (*)(B_TimesD_bytes))$default__str__,
    B_TimesD_bytesD___add__,
    (B_bytes (*)(B_TimesD_bytes, B_bytes, B_bytes))$PlusD___iadd__,
    B_TimesD_bytesD___mul__,
    (B_bytes (*)(B_TimesD_bytes, B_bytes, B_int))B_TimesD___imul__,

};
struct B_TimesD_bytes B_TimesD_bytes_instance = {&B_TimesD_bytesG_methods};
B_TimesD_bytes B_TimesD_bytesG_witness = &B_TimesD_bytes_instance;

struct B_HashableD_bytesG_class  B_HashableD_bytesG_methods = {
    "B_HashableD_bytes",
    UNASSIGNED,
    ($SuperG_class)&B_HashableG_methods,
    (void (*)(B_HashableD_bytes))$default__init__,
    B_HashableD_bytesD___serialize__,
    B_HashableD_bytesD___deserialize__,
    (B_bool (*)(B_HashableD_bytes))$default__bool__,
    (B_str (*)(B_HashableD_bytes))$default__str__,
    (B_str (*)(B_HashableD_bytes))$default__str__,
    B_HashableD_bytesD___eq__,
    B_HashableD_bytesD___ne__,
    B_HashableD_bytesD___hash__
};
struct B_HashableD_bytes B_HashableD_bytes_instance = {&B_HashableD_bytesG_methods};
B_HashableD_bytes B_HashableD_bytesG_witness = &B_HashableD_bytes_instance;

 
void B_ContainerD_bytesD___init__ (B_ContainerD_bytes wit) {
}


// Protocol methods; bytes implementations /////////////////////////////////////////////////////////////////////////////
/* 
   Note: We make bytes instances for Indexed and Sliceable even though these protocols 
   include mutating methods. 
*/

// B_Ord ///////////////////////////////////////////////////////////////////////////////////////////////


// TODO: We should consider how to normalize strings before comparisons

int B_bytesD_eq(B_bytes a, B_bytes b) {
    return (strcmp((char *)a->str,(char *)b->str)==0);
}
         
int B_bytesD_neq(B_bytes a, B_bytes b) {
    return !B_bytesD_eq(a,b);
}

// The comparisons below do lexicographic byte-wise comparisons.
// Thus they do not in general reflect locale-dependent order conventions.
 
int B_bytesD_lt(B_bytes a, B_bytes b) {
    return (strcmp((char *)a->str,(char *)b->str) < 0);
}
 
int B_bytesD_le(B_bytes a, B_bytes b) {
    return (strcmp((char *)a->str,(char *)b->str) <= 0);
}
 
int B_bytesD_gt(B_bytes a, B_bytes b) {
    return (strcmp((char *)a->str,(char *)b->str) > 0);
}
 
int B_bytesD_ge(B_bytes a, B_bytes b) {
    return (strcmp((char *)a->str,(char *)b->str) >= 0);
}
 
// B_Hashable ///////////////////////////////////////////////////////////////////////////////////

// hash function B_string_hash defined in hash.c

// B_Times /////////////////////////////////////////////////////////////////////////////////////////////

B_TimesD_bytes B_TimesD_bytesG_new() {
    return $NEW(B_TimesD_bytes);
}
 
B_bytes B_bytesD_add(B_bytes s, B_bytes t) {
    B_bytes res;
    NEW_UNFILLED_BYTES(res,s->nbytes + t->nbytes);
    memcpy(res->str,s->str,s->nbytes);
    memcpy(res->str+s->nbytes,t->str,t->nbytes);
    return res;
}

B_bytes B_bytesD_mul (B_bytes a, B_int n) {
    int nval = fromB_int(n);
    if (nval <= 0)
        return toB_bytes("");
    else {
        B_bytes res;
        NEW_UNFILLED_BYTES(res, a->nbytes * nval);
        for (int i=0; i<nval; i++)
            memcpy(res->str + i*a->nbytes,a->str,a->nbytes);
        return res;
    }
}

// Collection ///////////////////////////////////////////////////////////////////////////////////////

B_int B_bytesD_len(B_bytes s) {
    B_int res = toB_int(s->nbytes);
    return res;
}

// B_Container ///////////////////////////////////////////////////////////////////////////

 
B_ContainerD_bytes B_ContainerD_bytesG_new() {
    return $NEW(B_ContainerD_bytes);
}

int B_bytesD_contains(B_bytes s, B_bytes sub) {
    return bmh(s->str,sub->str,s->nbytes,sub->nbytes) > 0;
}

int B_bytesD_containsnot(B_bytes s, B_bytes sub) {
    return !B_bytesD_contains(s,sub);
}

// Iterable ///////////////////////////////////////////////////////////////////////////

B_IteratorB_bytes B_IteratorB_bytesG_new(B_bytes str) {
    return $NEW(B_IteratorB_bytes, str);
}

void B_IteratorB_bytesD_init(B_IteratorB_bytes self, B_bytes str) {
    self->src = str;
    self->nxt = 0;
}

void B_IteratorB_bytesD_serialize(B_IteratorB_bytes self,$Serial$state state) {
    $step_serialize(self->src,state);
    $step_serialize(toB_int(self->nxt),state);
}


B_IteratorB_bytes B_IteratorB_bytes$_deserialize(B_IteratorB_bytes res, $Serial$state state) {
    if (!res)
        res = $DNEW(B_IteratorB_bytes,state);
    res->src = (B_bytes)$step_deserialize(state);
    res->nxt = fromB_int((B_int)$step_deserialize(state));
    return res;
}

B_bool B_IteratorB_bytesD_bool(B_IteratorB_bytes self) {
    return $True;
}

B_str B_IteratorB_bytesD_str(B_IteratorB_bytes self) {
    char *s;
    asprintf(&s,"<bytes iterator object at %p>",self);
    return to$str(s);
}

// this is next function for forward iteration
static B_int B_IteratorB_bytesD_next(B_IteratorB_bytes self) {
    return self->nxt >= self->src->nbytes ? NULL : toB_int(self->src->str[self->nxt++]);
}

B_Iterator B_bytesD_iter(B_bytes str) {
    return (B_Iterator)$NEW(B_IteratorB_bytes,str);
}

struct B_IteratorB_bytesG_class B_IteratorB_bytesG_methods = {"B_IteratorB_bytes",UNASSIGNED,($SuperG_class)&B_IteratorG_methods, B_IteratorB_bytesD_init,
                                                        B_IteratorB_bytesD_serialize, B_IteratorB_bytes$_deserialize,
                                                        B_IteratorB_bytesD_bool, B_IteratorB_bytesD_str,  B_IteratorB_bytesD_str, B_IteratorB_bytesD_next};


// Indexed ///////////////////////////////////////////////////////////////////////////

B_int B_bytesD_getitem(B_bytes self, int ix) {
    int ix0 = ix < 0 ? self->nbytes + ix : ix;
    if (ix0<0 || ix0 >= self->nbytes)
        $RAISE((B_BaseException)$NEW(B_IndexError,to$str("getitem for bytes: indexing outside array")));
    return toB_int((int)self->str[ix0]);
}
 
// Sliceable //////////////////////////////////////////////////////////////////////////////////////

B_bytes B_bytesD_getslice(B_bytes s, B_slice slc) {
    int start, stop, step, slen;
    normalize_slice(slc, s->nbytes, &slen, &start, &stop, &step);
    //slice notation has been eliminated and default values applied
    B_bytes res;
    NEW_UNFILLED_BYTES(res,slen);
    int t = start;
    for (int i=0; i<slen; i++) {
        res->str[i] = s->str[t];
        t += step;
    }
    return res;
}



// General methods ////////////////////////////////////////////////////////////// 

B_bytes B_bytesG_new(B_Iterable iter, $WORD wit) {
    return $NEW(B_bytes, iter, wit);
}

void B_bytesD_init(B_bytes self, B_Iterable wit, $WORD iter) {
    B_list lst = B_listD_fromiter(wit->$class->__iter__(wit,iter));
    int len = lst->length;
    self->nbytes = len;
    self->str = malloc(len+1);
    self->str[len] = 0;
    for (int i=0; i< len; i++) {
        int n = fromB_int((B_int)lst->data[i]);
        if (0<=n && n <= 255)
            self->str[i] = n;
        else
            $RAISE((B_BaseException)$NEW(B_ValueError,to$str("bytes constructor: element outside [0..255]")));
    }
}

B_bool B_bytesD_bool(B_bytes s) {
    return toB_bool(s->nbytes > 0);
};

B_str B_bytesD_str(B_bytes s) {
    int lens = s->nbytes+3;
    B_str str;
    NEW_UNFILLED_STR(str,lens,lens);
    unsigned char *p = str->str;
    /*
      The function $ascii escapes all occurrences of quotes. We do not want that 
      for the delimiter pair, so we use the hackish solution to first set p[1] and
      p[lens-1] to space, then call $ascii and afterwards introduce the delimiting quotes.
    */
    p[0] = 'b';
    p[1] = ' ';
    p[lens-1] = ' ';
    memcpy(p+2,s->str,lens-3);
    B_str res = $ascii(str);
    res->str[1] = '"';
    res->str[res->nbytes-1] = '"';
    return res;
}


void B_bytesD_serialize(B_bytes str,$Serial$state state) {
    int nWords = str->nbytes/sizeof($WORD) + 1;         // # $WORDS needed to store str->str, including terminating 0.
    $ROW row = $add_header(STR_ID,1+nWords,state);
    long nbytes = (long)str->nbytes;                    
    memcpy(row->blob,&nbytes,sizeof($WORD));            
    memcpy(row->blob+1,str->str,nbytes+1);
}

B_bytes B_bytesD_deserialize(B_bytes self, $Serial$state state) {
    $ROW this = state->row;
    state->row =this->next;
    state->row_no++;
    B_bytes res = malloc(sizeof(struct B_bytes));
    long nbytes;
    memcpy(&nbytes,this->blob,sizeof($WORD));
    res->$class = &B_bytesG_methods;
    res->nbytes = (long)nbytes;
    res->str = malloc(nbytes+1);
    memcpy(res->str,this->blob+2,nbytes+1);
    return res;
}
  

// Builtin functions involving strings /////////////////////////////////////////////

B_str $ascii(B_str s) {
    unsigned char *hexdigits = (unsigned char *)"0123456789abcdef";
    int printable = 0;
    int escaped = 0; // Backslash, single and double quote
    int non_printable = 0;
    unsigned char c;
    for (int i=0; i<s->nbytes; i++) {
        c = s->str[i];
        if ((c < 32 || c > 126) && c != '\t' && c != '\r' && c != '\n')
            non_printable++;
        else if (c=='\\' || c=='\'' || c=='"' || c=='\n' || c=='\t' || c=='\r')
            escaped++;
        else 
            printable++;
    }
    int nbytes = printable+2*escaped+4*non_printable;
    B_str res;
    NEW_UNFILLED_STR(res,nbytes,nbytes);
    unsigned char *p =res->str;
    for (int i=0; i<s->nbytes; i++) {
        c = s->str[i];
        if ((c < 32 || c > 126) && c != '\t' && c != '\r' && c != '\n') {
            *p = '\\'; p++;
            *p = 'x'; p++;
            *p = hexdigits[c >> 4]; p++;
            *p = hexdigits[c & 0xf]; p++;
        } else switch (c) {
            case '\\':
            case '\'':
            case '\"':
                *p = '\\'; p++;
                *p = c; p++;
                break;
            case '\t':
                *p = '\\'; p++;
                *p = 't'; p++;
                break;
            case '\n':
                *p = '\\'; p++;
                *p = 'n'; p++;
                break;
            case '\r':
                *p = '\\'; p++;
                *p = 'r'; p++;
                break;
            default:        
                *p = c; p++;
            }
    }
    return res;
}
   
B_str $bin(B_Integral wit, $WORD n) {
    long v = fromB_int(wit->$class->__int__(wit,n));
    int sign = v<0;
    int nbits = 1;
    unsigned long u = labs(v);
    if (u & 0xffffffff00000000) {
        u >>= 32; nbits += 32;
    }
    if (u & 0x00000000ffff0000) {
        u >>= 16; nbits += 16;
    }
    if (u & 0x000000000000ff00) {
        u >>= 8; nbits += 8;
    }
    if (u & 0x00000000000000f0) {
        u >>= 4; nbits += 4;
    }
    if (u & 0x000000000000000c) {
        u >>= 2; nbits += 2;
    }
    if (u & 0x0000000000000002) {
        u >>= 1; nbits += 1;
    }
    B_str res;
    int nbytes = sign+2+nbits;
    NEW_UNFILLED_STR(res,nbytes,nbytes);
    unsigned char *p = res->str;
    if (sign) {
        *p = '-'; p++;
    }
    *p = '0'; p++;
    *p = 'b'; p++;
    u = labs(v);
    for (int i = nbits-1; i>=0; i--) {
        *p = u & (1L << i) ? '1' : '0'; p++;
    }
    return res;
}  

B_str $chr(B_Integral wit, $WORD n) {
    long v = fromB_int(wit->$class->__int__(wit,n));
    if (v >=  0x110000)
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("chr: argument is not a valid Unicode code point")));
    unsigned char code[4];
    int nbytes = utf8proc_encode_char((int)v,(unsigned char*)&code);
    if (nbytes==0)
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("chr: argument is not a valid Unicode code point")));
    B_str res;
    NEW_UNFILLED_STR(res,1,nbytes);
    for (int i=0; i<nbytes; i++)
        res->str[i] = code[i];
    return res;
}

B_str $hex(B_Integral wit, $WORD n) {
    unsigned char *hexdigits = (unsigned char *)"0123456789abcdef";
    long v =  fromB_int(wit->$class->__int__(wit,n));
    int sign = v<0;
    int nhexs = 1;
    unsigned long u = labs(v);
    if (u & 0xffffffff00000000) {
        u >>= 32; nhexs += 8;
    }
    if (u & 0x00000000ffff0000) {
        u >>= 16; nhexs += 4;
    }
    if (u & 0x000000000000ff00) {
        u >>= 8; nhexs += 2;
    }
    if (u & 0x00000000000000f0) {
        u >>= 4; nhexs += 1;
    }
    B_str res;
    int nbytes = sign+2+nhexs;
    NEW_UNFILLED_STR(res,nbytes,nbytes);
    unsigned char *p = res->str;
    if (sign) {
        *p = '-'; p++;
    }
    *p = '0'; p++;
    *p = 'x'; p++;
    u = labs(v);
    for (int i = nhexs-1; i>=0; i--) {
        *p = hexdigits[(u>>(4*i)) & 0xf]; p++;
    }
    return res;
}

B_int $ord(B_str c) {
    if(c->nchars != 1)
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("ord: argument is not a single Unicode char")));
    int cp;
    int cpnbytes = utf8proc_iterate(c->str,-1,&cp);
    if (cpnbytes < 0)
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("ord: argument is not a single Unicode char")));
    return toB_int(cp);
}

// Auxiliary function used in __str__ for collections ////////////////////////////

B_str B_strD_join_par(char lpar, B_list elems, char rpar) {
    char *s = ", ";
    int len = elems->length;
    int totchars = 2;  //parens
    int totbytes = 2;
    B_str nxt;
    for (int i=0; i<len; i++) {
        nxt = (B_str)elems->data[i];
        totchars += nxt->nchars;
        totbytes += nxt->nbytes;
    }
    if (len > 1) {
        totchars += (len-1) * 2; // 2 is length of ", "
        totbytes += (len-1) * 2; 
    }
    B_str res;
    NEW_UNFILLED_STR(res,totchars,totbytes);
    res->str[0] = lpar;
    res->str[totbytes-1] = rpar;
    if (len > 0) {
        unsigned char *p = res->str+1;
        nxt = elems->data[0];
        memcpy(p,nxt->str,nxt->nbytes);
        p += nxt->nbytes;
        for (int i=1; i<len; i++) {
            nxt = (B_str)elems->data[i];
            memcpy(p,s,2);
            p += 2;
            memcpy(p,nxt->str,nxt->nbytes);
            p += nxt->nbytes;
        }
    }
    return res;
}

B_str $default__str__(B_value self) {
    char *s;
    asprintf(&s,"<%s object at %p>",self->$class->$GCINFO,self);
    return to$str(s);
}

