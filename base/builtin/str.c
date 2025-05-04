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

#include <strings.h>

#define GC_THREADS 1
#include "gc.h"

#include "utf8proc.h"

// Auxiliaries, some used for both str and bytearray implementations ////////////////////////////////////////////////////////

static unsigned char nul = 0;

static struct B_str null_struct = {&B_strG_methods,0,0,&nul};

static B_str null_str = &null_struct;

static struct B_str space_struct = {&B_strG_methods,1,1,(unsigned char *)" "};

static B_str space_str = &space_struct;

static struct B_str whitespace_struct = {&B_strG_methods,6,6,(unsigned char *)" \t\n\r\x0b\x0c"};

static B_str whitespace_str = &whitespace_struct;

static struct B_bytes null_bytes_struct = {&B_bytesG_methods,0,&nul};

static B_bytes null_bytes = &null_bytes_struct;

static struct B_bytes space_bytes_struct = {&B_bytesG_methods,1,(unsigned char *)" "};

static B_bytes space_bytes = &space_bytes_struct;

static struct B_bytes whitespace_bytes_struct = {&B_bytesG_methods,6,(unsigned char *)" \t\n\r\x0b\x0c"};

static B_bytes whitespace_bytes = &whitespace_bytes_struct;

// We avoid returning the bytearray singleton from bytearray methods, this is
// just used internally as a default value for the fill character.
static struct B_bytearray space_bytearray_struct = {&B_bytearrayG_methods,1,(unsigned char *)" ",1};

static B_bytearray space_bytearray = &space_bytearray_struct;

static struct B_bytearray whitespace_bytearray_struct = {&B_bytearrayG_methods,6,(unsigned char *)" \t\n\r\x0b\x0c",6};

static B_bytearray whitespace_bytearray = &whitespace_bytearray_struct;

#define NEW_UNFILLED_STR(nm,nchrs,nbtes)        \
    assert(nbtes >= nchrs);                     \
    nm = acton_malloc(sizeof(struct B_str));           \
    (nm)->$class = &B_strG_methods;               \
    (nm)->nchars = nchrs;                       \
    (nm)->nbytes = nbtes;                       \
    (nm)->str = acton_malloc_atomic(nbtes + 1);       \
    (nm)->str[nbtes] = 0

#define NEW_UNFILLED_BYTEARRAY(nm,nbtes)        \
    nm = acton_malloc(sizeof(struct B_bytearray));     \
    (nm)->$class = &B_bytearrayG_methods;         \
    (nm)->nbytes = nbtes;                       \
    (nm)->capacity = nbtes;                     \
    (nm)->str = acton_malloc_atomic(nbtes + 1);       \
    (nm)->str[nbtes] = 0

#define NEW_UNFILLED_BYTES(nm,nbtes)            \
    nm = acton_malloc(sizeof(struct B_bytes));         \
    (nm)->$class = &B_bytesG_methods;             \
    (nm)->nbytes = nbtes;                       \
    (nm)->str = acton_malloc_atomic(nbtes + 1);              \
    (nm)->str[nbtes] = 0

// Conversion to and from C strings

B_str to$str(char *str) { 
    B_str res;
    int nbytes = 0;
    int nchars = 0;
    bool isascii = true;
    unsigned char *p = (unsigned char*)str;
    while (*p++ != 0)
        if (*p >= 0x80) {
            isascii = false;
            break;
        }
    if (isascii) {
        nbytes = p - (unsigned char*)str - 1;
        NEW_UNFILLED_STR(res, nbytes, nbytes);
        memcpy(res->str, str, nbytes);
        return res;
    }
    p = (unsigned char*)str;
    int cp, cpnbytes;
    while(1) {
        if (*p == '\0') {
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

// No-copy version
B_str to_str_noc(char *str) {
    B_str res = acton_malloc(sizeof(struct B_str));
    res->$class = &B_strG_methods;
    res->nbytes = strlen(str);
    res->nchars = res->nbytes;
    res->str = (unsigned char*)str;

    bool isascii = true;
    unsigned char *p = (unsigned char*)str;
    while (*p++ != 0)
        if (*p >= 0x80) {
            isascii = false;
            break;
        }
    p = (unsigned char*)str;
    int cp, cpnbytes;
    if (!isascii) {
        res->nchars = 0;
        while (1) {
            if (*p == '\0')
                break;
            cpnbytes = utf8proc_iterate(p, -1, &cp);
            if (cpnbytes < 0) {
                $RAISE((B_BaseException)$NEW(B_ValueError,to$str("to_str_noc: Unicode decode error")));
                return NULL;
            }
            p += cpnbytes;
            res->nchars++;
        }
    }
    return res;
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
    if (s->nchars == 0) {
        return null_str;
    }
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
    $RAISE((B_BaseException)$NEW(B_IndexError, to$int(i), to$str("index outside str")));
    return 0;
}

 
// Eliminates slice notation in find, index, count and other methods
// with optional start and end and adds defaults for omitted parameters.

static int fix_start_end(int nchars, B_int *start, B_int *end) {
    if (*start==NULL) {
        *start = acton_malloc(sizeof(struct B_int));
        *start = to$int(0);
    } else {
        int st = from$int(*start);
        if (st > nchars)
            return -1;
        if (st < 0) 
            st += nchars+1;
        st = st < 0 ? 0 : st;
        *start = to$int(st);
    }
    if (*end==NULL) {
        *end = acton_malloc(sizeof(struct B_int));
        *end = to$int(nchars);
    } else {
        int en = from$int(*end);
        if (en > nchars)   
            en = nchars;      
        else if (en < 0) 
            en += nchars+1;     
        en = en < 0 ? 0 : en;    
        
        *end = to$int(en);
    }
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

int equal_bytes(unsigned char *p, unsigned char *q, int len) {
    int i;
    for (i=0; i<len; i++) {
        if (p[i] != q[i]) break;
    }
    return i==len;
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

struct byte_counts {
    int printable, squotes, dquotes, escaped, non_printable, non_ascii;
};
        

struct byte_counts byte_count(unsigned char *s, int len) {
   struct byte_counts res = {0,0,0,0,0,0};
    unsigned char c;
    for (int i=0; i<len; i++) {
        c = s[i];
        if (c=='\\' || c=='\n' || c=='\t' || c=='\r')
            res.escaped++;
        else if (c < 32 || c == 127)
            res.non_printable++;
        else if (c=='\'')
            res.squotes++;
        else if (c=='"')
            res.dquotes++;
        else if (c<127)
            res.printable++;
        else
            res.non_ascii++;
    }
    return res;
}

void escape_str(unsigned char *out, unsigned char *in, int outlen, int inlen, int max_esc, bool esc_squote) {
    unsigned char *hexdigits = (unsigned char *)"0123456789abcdef";
    unsigned char *p = out;
    for (int i=0; i<inlen; i++) {
        unsigned char c = in[i];
        if ((c < 32 && c != '\t' && c != '\r' && c != '\n') || ( c > 126 && c <= max_esc)) {
            *p = '\\'; p++;
            *p = 'x'; p++;
            *p = hexdigits[c >> 4]; p++;
            *p = hexdigits[c & 0xf]; p++;
        } else {
            switch (c) {
            case '\\':
                *p = '\\'; p++;
                *p = '\\'; p++;
                break;
            case '\'':
                if (esc_squote) {
                    *p = '\\'; p++;
                }
                *p = '\''; p++;
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
    }
}



// General methods ////////////////////////////////////////////////////////////// 

B_str B_strG_new(B_value s) {
    return $NEW(B_str, s);
}

B_NoneType B_strD___init__(B_str self, B_value s) {
    // If s is None (C NULL) we use the "None" string.
    if (s == NULL) {
        self->nchars = 4;
        self->nbytes = 4;
        self->str = (unsigned char *)"None";
        return B_None;
    }
    B_str res = s->$class->__str__(s);
    self->nchars = res->nchars;
    self->nbytes = res->nbytes;
    self->str = res->str;
    return B_None;
}

B_bool B_strD___bool__(B_str s) {
    return toB_bool(s->nchars > 0);
};

B_str B_strD___str__(B_str s) {
    return s;
}


B_str B_strD___repr__(B_str s) {
    struct byte_counts bs = byte_count(s->str, s->nbytes);
    int newbytes = 2+bs.escaped+3*bs.non_printable+(bs.squotes>0 && bs.dquotes>0 ? bs.squotes : 0);
    B_str res;
    NEW_UNFILLED_STR(res,s->nchars+newbytes, s->nbytes+newbytes);
    escape_str(res->str+1,s->str,res->nbytes-1,s->nbytes,127,bs.squotes>0 && bs.dquotes>0);
    if (bs.dquotes==0 && bs.squotes>0) {
        res->str[0] = '"';
        res->str[res->nbytes-1] = '"';
    } else {
        res->str[0] = '\'';
        res->str[res->nbytes-1] = '\'';
    }        
    return res;
}

void B_strD___serialize__(B_str str,$Serial$state state) {
    int nWords = str->nbytes/sizeof($WORD) + 1;         // # $WORDS needed to store str->str, including terminating 0.
    $ROW row = $add_header(STR_ID,2+nWords,state);
    long nbytes = (int)str->nbytes;                    // We could pack nbytes and nchars in one $WORD, 
    memcpy(row->blob,&nbytes,sizeof($WORD));            // but we should think of a better, general approach.
    long nchars = (int)str->nchars;
    memcpy(row->blob+1,&nchars,sizeof($WORD));
    memcpy(row->blob+2,str->str,nbytes+1);
}

B_str B_strD___deserialize__(B_str self, $Serial$state state) {
    $ROW this = state->row;
    state->row =this->next;
    state->row_no++;
    B_str res = acton_malloc(sizeof(struct B_str));
    long nbytes;
    memcpy(&nbytes,this->blob,sizeof($WORD));
    res->$class = &B_strG_methods;
    res->nbytes = (int)nbytes;
    long nchars;
    memcpy(&nchars,this->blob+1,sizeof($WORD));
    res->nchars = (int)nchars;
    res->str = acton_malloc_atomic(nbytes+1);
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
    int wval = from$int(width);
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
    if (fix_start_end(s->nchars,&st,&en) < 0) return to$int(0);
    unsigned char *p = skip_chars(s->str,from$int(st),isascii);
    unsigned char *q = skip_chars(p,from$int(en)-from$int(st),isascii);
    int res = 0;
    int n = bmh(p,sub->str,q-p,sub->nbytes);
    while (n>=0) {
        res++;
        p += n + (sub->nbytes>0 ? sub->nbytes : 1);
        n = bmh(p,sub->str,q-p,sub->nbytes);
    }
    return to$int(res);
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
    if (fix_start_end(s->nchars,&st,&en) < 0) return B_False;
    int isascii = s->nchars==s->nbytes;
    unsigned char *p = skip_chars(s->str + s->nbytes,from$int(en) - s->nchars,isascii) - sub->nbytes;
    unsigned char *q = sub->str;
    for (int i=0; i<sub->nbytes; i++) {
        if (*p == 0 || *p++ != *q++) {
            return B_False;
        }
    }
    return B_True;
}

B_str B_strD_expandtabs(B_str s, B_int tabsize){
    if (s->nchars == 0) {
        return null_str;
    }
    int tabsz = tabsize?from$int(tabsize):8;
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
    if (fix_start_end(s->nchars,&st,&en) < 0) return to$int(-1);
    unsigned char *p = skip_chars(s->str,from$int(st),isascii);
    unsigned char *q = skip_chars(p,from$int(en)-from$int(st),isascii);
    int n = bmh(p,sub->str,q-p,sub->nbytes);
    if (n<0) return to$int(-1);
    return to$int(char_no(s,n+p-s->str));
}

B_int B_strD_index(B_str s, B_str sub, B_int start, B_int end) {
    B_int n = B_strD_find(s,sub,start,end);
    if (from$int(n)<0) {
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("index: substring not found")));
    }
    return n;
}

B_bool B_strD_isalnum(B_str s) {
    unsigned char *p = s->str;
    int codepoint;
    int nbytes;
    if (s->nchars == 0)
        return B_False;
    for (int i=0; i < s->nchars; i++) {
        nbytes = utf8proc_iterate(p,-1,&codepoint);
        utf8proc_category_t cat = utf8proc_category(codepoint);
        if ((cat <  UTF8PROC_CATEGORY_LU || cat >  UTF8PROC_CATEGORY_LO) && cat != UTF8PROC_CATEGORY_ND)
            return B_False;
        p += nbytes;
    }
    return B_True;
}

B_bool B_strD_isalpha(B_str s) {
    unsigned char *p = s->str;
    int codepoint;
    int nbytes;
    if (s->nchars == 0)
        return B_False;
    for (int i=0; i < s->nchars; i++) {
        nbytes = utf8proc_iterate(p,-1,&codepoint);
        utf8proc_category_t cat = utf8proc_category(codepoint);
        if (cat <  UTF8PROC_CATEGORY_LU || cat >  UTF8PROC_CATEGORY_LO)
            return B_False;
        p += nbytes;
    }
    return B_True;
}

B_bool B_strD_isascii(B_str s) {
    unsigned char *p = s->str;
    for (int i=0; i < s->nbytes; i++) {
        if (*p > 127)
            return B_False;
        p++;
    }
    return B_True;
}

B_bool B_strD_isdecimal(B_str s) {
    unsigned char *p = s->str;
    int codepoint;
    int nbytes;
    if (s->nchars == 0)
        return B_False;
    for (int i=0; i < s->nchars; i++) {
        nbytes = utf8proc_iterate(p,-1,&codepoint);
        utf8proc_category_t cat = utf8proc_category(codepoint);
        if (cat != UTF8PROC_CATEGORY_ND)
            return B_False;
        p += nbytes;
    }
    return B_True;
}

B_bool B_strD_islower(B_str s) {
    unsigned char *p = s->str;
    int codepoint;
    int nbytes;
    int has_cased = 0;
    if (s->nchars == 0)
        return B_False;
    for (int i=0; i < s->nchars; i++) {
        nbytes = utf8proc_iterate(p,-1,&codepoint);
        utf8proc_category_t cat = utf8proc_category(codepoint);
        if (cat == UTF8PROC_CATEGORY_LT|| cat == UTF8PROC_CATEGORY_LU)
            return B_False;
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
        return B_False;
    for (int i=0; i < s->nchars; i++) {
        nbytes = utf8proc_iterate(p,-1,&codepoint);
        utf8proc_category_t cat = utf8proc_category(codepoint);
        if (cat >= UTF8PROC_CATEGORY_ZS && codepoint != 0x20)
            return B_False;
        p += nbytes;
    }
    return B_True;
}

B_bool B_strD_isspace(B_str s) {
    unsigned char *p = s->str;
    int codepoint;
    int nbytes;
    if (s->nchars == 0)
        return B_False;
    for (int i=0; i < s->nchars; i++) {
        nbytes = utf8proc_iterate(p,-1,&codepoint);
        if (!isspace_codepoint(codepoint))
            return B_False;
        p += nbytes;
    }
    return B_True;
}

B_bool B_strD_istitle(B_str s) {
    unsigned char *p = s->str;
    int codepoint;
    int nbytes;
    int hascased = 0;
    int incasedrun = 0;
    if (s->nchars == 0)
        return B_False;
    for (int i=0; i < s->nchars; i++) {
        nbytes = utf8proc_iterate(p,-1,&codepoint);
        utf8proc_category_t cat = utf8proc_category(codepoint);
        if (cat == UTF8PROC_CATEGORY_LU || cat == UTF8PROC_CATEGORY_LT ) {
            hascased = 1;
            if (incasedrun)
                return B_False;
            incasedrun = 1;
        } else if (cat == UTF8PROC_CATEGORY_LL) {
            hascased = 1;
            if (!incasedrun)
                return B_False;
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
        return B_False;
    for (int i=0; i < s->nchars; i++) {
        nbytes = utf8proc_iterate(p,-1,&codepoint);
        utf8proc_category_t cat = utf8proc_category(codepoint);
        if (cat == UTF8PROC_CATEGORY_LL)
            return B_False;
        if (cat == UTF8PROC_CATEGORY_LU || cat == UTF8PROC_CATEGORY_LT)
            hascased = 1;
        p += nbytes;
    }
    return toB_bool(hascased);
}

B_str B_strD_join(B_str s, B_Iterable wit, $WORD iter) {
    int totchars = 0;
    int totbytes = 0;
    B_CollectionD_SequenceD_list wit2 = B_CollectionD_SequenceD_listG_witness;
    B_list lst = wit2->$class->__fromiter__(wit2,wit,iter);
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
    int wval = from$int(width);
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
    if (s->nchars == 0) return s;
    if (cs==NULL) cs = whitespace_str;
    unsigned char *p = s->str;
    int i, k;
    for (i = 0; i < s->nchars; i++) {
        unsigned char *q = cs->str;
        for (k = 0; k < cs->nchars; k++) {
            if (equal_bytes(p,q,byte_length2(*q))) 
                break;
            else
                q +=  byte_length2(*q);
        }    
        if (k == cs->nchars) break;
        p +=  byte_length2(*p);
    }
    B_str res;
    NEW_UNFILLED_STR(res,s->nchars-i,s->str+s->nbytes-p);
    memcpy(res->str,p,res->nbytes);
    return res;
}
 
B_tuple B_strD_partition(B_str s, B_str sep) {
    int n = from$int(B_strD_find(s,sep,NULL,NULL));
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
        count = to$int(INT_MAX);
    int c = from$int(B_strD_count(s,old,NULL,NULL));
    int c0 = from$int(count) < c ? from$int(count) : c;
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
    if (fix_start_end(s->nchars,&st,&en) < 0) return to$int(-1);
    unsigned char *p = skip_chars(s->str,from$int(st),isascii);
    unsigned char *q = skip_chars(p,from$int(en)-from$int(st),isascii);
    int n = rbmh(p,sub->str,q-p,sub->nbytes);
    if (n<0) return to$int(-1);
    return to$int(char_no(s,n+p-s->str));
}


B_int B_strD_rindex(B_str s, B_str sub, B_int start, B_int end) {
    B_int n = B_strD_rfind(s,sub,start,end);
    if (from$int(n)<0) {
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("rindex: substring not found")));
    };
    return n;
}

B_str B_strD_rjust(B_str s, B_int width, B_str fill) {
    if (!fill) fill = space_str;
    if (fill->nchars != 1) {
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("rjust: fill string not single char")));
    }
    int wval = from$int(width);
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
    int n = from$int(B_strD_rfind(s,sep,NULL,NULL));
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
    B_SequenceD_list wit = B_SequenceD_listG_witness;
    if (maxsplit == NULL || from$int(maxsplit) < 0) maxsplit = to$int(INT_MAX); 
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
                    if (res->length == from$int(maxsplit))
                        break; // we have now removed leading whitespace in remainder
                } else
                    wordlength++;
            } else {
                if (inword) {
                    inword = 0;
                    B_str word;
                    NEW_UNFILLED_STR(word,wordlength,p-q);
                    memcpy(word->str,q,p-q);
                    wit->$class->append(wit,res,word);
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
                wit->$class->append(wit,res,word);
            }
        } else {
            B_str word;
            p = s->str+s->nbytes;
            NEW_UNFILLED_STR(word,remaining,p-q);
            memcpy(word->str,q,p-q);
            wit->$class->append(wit,res,word);
        }
        // $WORD w = list_getitem(res,0);
        return res;
    } else { // separator given
        if (sep->nchars==0) {
            $RAISE((B_BaseException)$NEW(B_ValueError,to$str("split: separator is empty string")));
        }
        if (remaining==0) { // for some unfathomable reason, this is the behaviour of the Python method
            wit->$class->append(wit,res,null_str);
            return res;
        }
        B_str ls, rs, ssep;
        rs = s;
        // Note: This builds many intermediate rs strings...
        while (rs->nchars>0 && res->length < from$int(maxsplit)) {
            B_tuple t = B_strD_partition(rs,sep);
            ssep = (B_str)t->components[1];
            rs =  (B_str)t->components[2];
            wit->$class->append(wit,res,(B_str)t->components[0]);
        }
        if (ssep->nchars>0)
            wit->$class->append(wit,res,rs);
        return res;
    }
}
 
B_list B_strD_splitlines(B_str s, B_bool keepends) {
    B_SequenceD_list wit = B_SequenceD_listG_witness;
    if (!keepends)
        keepends = B_False;
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
            wit->$class->append(wit,res,line);
            linelength = 0;
        }
    }
    if (q < p) {
        B_str line;
        NEW_UNFILLED_STR(line,linelength,p-q);
        memcpy(line->str,q,p-q);
        wit->$class->append(wit,res,line);
    }
    return res;
} 

B_str B_strD_rstrip(B_str s, B_str cs) {
    if (s->nchars == 0) return s;
    if (cs==NULL) cs = whitespace_str;
    unsigned char *p = s->str + s->nbytes;
    int i, k;
    for (i = 0; i < s->nchars; i++) {
        unsigned char *q = cs->str;
        p = skip_chars(p,-1,0);
        for (k = 0; k < cs->nchars; k++) {
            if (equal_bytes(p,q,byte_length2(*q))) 
                break;
            else
                q += byte_length2(*q);
        }    
        if (k == cs->nchars) break;
    }
    p = skip_chars(p,1,0);
    B_str res;
    NEW_UNFILLED_STR(res,s->nchars-i,p-s->str);
    memcpy(res->str,s->str,res->nbytes);
    return res;
}

B_bool B_strD_startswith(B_str s, B_str sub, B_int start, B_int end) {
    B_int st = start;
    B_int en = end;
    if (fix_start_end(s->nchars,&st,&en) < 0) return B_False;
    int isascii = s->nchars==s->nbytes;
    unsigned char *p = skip_chars(s->str,from$int(st),isascii);
    unsigned char *q = sub->str;
    for (int i=0; i<sub->nbytes; i++) {
        if (*p == 0 || *p++ != *q++) {
            return B_False;
        }
    }
    return B_True;
}


B_str B_strD_strip(B_str s, B_str cs) {
    return B_strD_lstrip(B_strD_rstrip(s,cs),cs);
}

B_str B_strD_upper(B_str s) {
    return str_transform(s,utf8proc_toupper);
}

B_str B_strD_zfill(B_str s, B_int width) {
    int wval = from$int(width);
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



// Protocol methods; string implementations /////////////////////////////////////////////////////////////////////////////
/* 
   Note: We make str instances for Indexed and Sliceable even though these protocols 
   include mutating methods. 
*/

// B_Ord ///////////////////////////////////////////////////////////////////////////////////////////////


// TODO: We should consider how to normalize strings before comparisons

 
// The comparisons below do lexicographic byte-wise comparisons.
// Thus they do not in general reflect locale-dependent order conventions.
 
B_bool B_OrdD_strD___eq__ (B_OrdD_str wit, B_str a, B_str b) {
    return toB_bool(strcmp((char *)a->str,(char *)b->str) == 0);
}
 
B_bool B_OrdD_strD___ne__ (B_OrdD_str wit, B_str a, B_str b) {
    return toB_bool(strcmp((char *)a->str,(char *)b->str) != 0);
}
 
B_bool B_OrdD_strD___lt__ (B_OrdD_str wit, B_str a, B_str b) {
    return toB_bool(strcmp((char *)a->str,(char *)b->str) < 0);
}

B_bool B_OrdD_strD___le__ (B_OrdD_str wit, B_str a, B_str b) {
    return toB_bool(strcmp((char *)a->str,(char *)b->str) <= 0);
}
 
B_bool B_OrdD_strD___gt__ (B_OrdD_str wit, B_str a, B_str b) {
    return toB_bool(strcmp((char *)a->str,(char *)b->str) > 0);
}

B_bool B_OrdD_strD___ge__ (B_OrdD_str wit, B_str a, B_str b) {
    return toB_bool(strcmp((char *)a->str,(char *)b->str) >= 0);
}
  
// B_Hashable ///////////////////////////////////////////////////////////////////////////////////

B_bool B_HashableD_strD___eq__ (B_HashableD_str wit, B_str a, B_str b) {
    return toB_bool(strcmp((char *)a->str,(char *)b->str) == 0);
}
 
B_bool B_HashableD_strD___ne__ (B_HashableD_str wit, B_str a, B_str b) {
    return toB_bool(strcmp((char *)a->str,(char *)b->str) != 0);
}
 
B_u64 B_HashableD_strD___hash__(B_HashableD_str wit, B_str a) {
    return toB_u64(zig_hash_wyhash_hash(0,to$bytes((char *)a->str)));
}

B_NoneType B_HashableD_strD_putBytes(B_HashableD_str wit, B_str a, B_hasher h) {
    zig_hash_wyhash_update(h->_hasher,to$bytes((char *)a->str));
    return B_None;
}
// B_Times /////////////////////////////////////////////////////////////////////////////////////////////

B_str B_TimesD_strD___add__ (B_TimesD_str wit, B_str s, B_str t) {
    B_str res;
    NEW_UNFILLED_STR(res,s->nchars + t->nchars,s->nbytes + t->nbytes);
    memcpy(res->str,s->str,s->nbytes);
    memcpy(res->str+s->nbytes,t->str,t->nbytes);
    return res;
}

B_str B_TimesD_strD___zero__ (B_TimesD_str wit) {
    return null_str;
}

B_str B_TimesD_strD___mul__ (B_TimesD_str wit, B_str a, B_int n) {
    int nval = from$int(n);
    if (nval <= 0)
        return null_str;
    else {
        B_str res;
        NEW_UNFILLED_STR(res,a->nchars * nval, a->nbytes * nval);
        for (int i=0; i<nval; i++)
            memcpy(res->str + i*a->nbytes,a->str,a->nbytes);
        return res;
    }
}

// Collection ///////////////////////////////////////////////////////////////////////////////////////


B_str B_ContainerD_strD___fromiter__ (B_ContainerD_str wit, B_Iterable wit2, $WORD iter) {
    return B_strD_join(null_str,wit2,iter);
}

B_int B_ContainerD_strD___len__ (B_ContainerD_str wit, B_str s){
    return to$int(s->nchars);
}

// B_Container ///////////////////////////////////////////////////////////////////////////

 
B_bool B_ContainerD_strD___contains__ (B_ContainerD_str wit, B_str s, B_str sub) {
    return toB_bool(bmh(s->str,sub->str,s->nbytes,sub->nbytes) > 0);
}

B_bool B_ContainerD_strD___containsnot__ (B_ContainerD_str wit, B_str s, B_str sub) {
    return toB_bool(!B_ContainerD_strD___contains__(wit, s, sub)->val);
}

// Iterable ///////////////////////////////////////////////////////////////////////////

// first define Iterator class

B_IteratorB_str B_IteratorB_strG_new(B_str str) {
    return $NEW(B_IteratorB_str, str);
}

B_NoneType B_IteratorB_strD_init(B_IteratorB_str self, B_str str) {
    self->src = str;
    self->nxt = 0;
    return B_None;
}

void B_IteratorB_strD_serialize(B_IteratorB_str self,$Serial$state state) {
    $step_serialize(self->src,state);
    $step_serialize(to$int(self->nxt),state);
}


B_IteratorB_str B_IteratorB_str$_deserialize(B_IteratorB_str res, $Serial$state state) {
    if (!res)
        res = $DNEW(B_IteratorB_str,state);
    res->src = (B_str)$step_deserialize(state);
    res->nxt = from$int((B_int)$step_deserialize(state));
    return res;
}

B_bool B_IteratorB_strD_bool(B_IteratorB_str self) {
    return B_True;
}

B_str B_IteratorB_strD_str(B_IteratorB_str self) {
    return $FORMAT("<str iterator object at %p>", self);
}

// this is next function for forward iteration
static B_str B_IteratorB_strD_next(B_IteratorB_str self) {
    unsigned char *p = &self->src->str[self->nxt];
    if (*p != 0) {
        self->nxt +=byte_length2(*p);
        return mk_char(p);
    }
    $RAISE ((B_BaseException)$NEW(B_StopIteration, to$str("str iterator terminated")));
    return NULL; // to avoid compiler warning
}


struct B_IteratorB_strG_class B_IteratorB_strG_methods = {"B_IteratorB_str",UNASSIGNED,($SuperG_class)&B_IteratorG_methods, B_IteratorB_strD_init,
                                                    B_IteratorB_strD_serialize, B_IteratorB_str$_deserialize,
                                                    B_IteratorB_strD_bool, B_IteratorB_strD_str, B_IteratorB_strD_str, B_IteratorB_strD_next};

// now, define __iter__

B_Iterator B_ContainerD_strD___iter__ (B_ContainerD_str wit, B_str s) {
    return (B_Iterator)$NEW(B_IteratorB_str,s);
}


// Indexed ///////////////////////////////////////////////////////////////////////////

B_str B_SliceableD_strD___getitem__ (B_SliceableD_str wit, B_str s, B_int i) {
    unsigned char *p = s->str;
    int ix = get_index(from$int(i),s->nchars);
    p = skip_chars(p,ix,s->nchars == s->nbytes);
    return mk_char(p);
}

B_NoneType B_SliceableD_strD___setitem__ (B_SliceableD_str wit, B_str str, B_int i, B_str val) {
    $RAISE((B_BaseException)$NEW(B_NotImplementedError,to$str("call to mutating method setitem on string")));
    return B_None;
}

B_NoneType B_SliceableD_strD___delitem__ (B_SliceableD_str wit, B_str str, B_int i) {
    $RAISE((B_BaseException)$NEW(B_NotImplementedError,to$str("call to mutating method delitem on string")));
    return B_None;
}

// Sliceable //////////////////////////////////////////////////////////////////////////////////////

B_str B_SliceableD_strD___getslice__ (B_SliceableD_str wit, B_str s, B_slice slc) {
    int isascii = s->nchars == s->nbytes;
    int nchars = s->nchars;
    int nbytes = 0;
    long start, stop, step, slen;
    normalize_slice(slc, nchars, &slen, &start, &stop, &step);
    if (slen == 0) {
        return null_str;
    }
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

B_NoneType B_SliceableD_strD___setslice__ (B_SliceableD_str wit, B_str str, B_Iterable wit2, B_slice slc, $WORD iter) {
    $RAISE((B_BaseException)$NEW(B_NotImplementedError,to$str("call to mutating method setslice on string")));
    return B_None;
}

B_NoneType B_SliceableD_strD___delslice__ (B_SliceableD_str wit, B_str str, B_slice slc) {
    $RAISE((B_BaseException)$NEW(B_NotImplementedError,to$str("call to mutating method delslice on string")));
    return B_None;
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


B_bytearray to$bytearrayD_len(char *str, int len) {
    B_bytearray res;
    NEW_UNFILLED_BYTEARRAY(res, len);
    memcpy(res->str, str, len);
    return res;
}

B_bytearray actBytearrayFromCString(char *str) {
    B_bytearray res;
    int len = strlen(str);
    NEW_UNFILLED_BYTEARRAY(res,len);
    memcpy(res->str,str,len);
    return res;
}

B_bytearray actBytearrayFromCStringNoCopy(char *str) {
    B_bytearray res = acton_malloc(sizeof(struct B_bytearray));
    res->$class = &B_bytearrayG_methods;
    res->nbytes = strlen(str);
    res->str = (unsigned char*)str;
    return res;
}

B_bytearray actBytearrayFromCStringLength(char *str, int len) {
    B_bytearray res;
    NEW_UNFILLED_BYTEARRAY(res, len);
    memcpy(res->str, str, len);
    return res;
}

B_bytearray actBytearrayFromCStringLengthNoCopy(char *str, int length) {
    B_bytearray res = acton_malloc(sizeof(struct B_bytearray));
    res->$class = &B_bytearrayG_methods;
    res->nbytes = length;
    res->str = (unsigned char*)str;
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
        ? acton_malloc_atomic(newcapacity+1)
        : acton_realloc(b->str,newcapacity+1);
    if (newstr == NULL) {
        $RAISE((B_BaseException)$NEW(B_MemoryError,to$str("memory allocation failed")));
    }
    b->str = newstr;
    b->capacity = newcapacity;
}  

static B_bytearray B_bytearrayD_copy(B_bytearray s) {
    B_bytearray res;
    NEW_UNFILLED_BYTEARRAY(res,s->nbytes);
    res->nbytes = s->nbytes;
    memcpy(res->str,s->str,s->nbytes);
    return res;
}

 
// General methods 

B_bytearray B_bytearrayG_new(B_bytes b) {
    return $NEW(B_bytearray, b);
}

B_NoneType B_bytearrayD___init__(B_bytearray self, B_bytes b) {
    int len = b->nbytes;
    self->nbytes = len;
    self->capacity = len;
    self->str = acton_malloc_atomic(len+1);
    memcpy(self->str,b->str,len+1);
    return B_None;
}
 
B_bool B_bytearrayD___bool__(B_bytearray s) {
    return toB_bool(s->nbytes > 0);
};

B_str B_bytearrayD___str__(B_bytearray s) {
    struct byte_counts bs = byte_count(s->str, s->nbytes);
    int newbytes = 14+bs.escaped+3*bs.non_printable+(bs.squotes>0 && bs.dquotes>0 ? bs.squotes : 0)+3*bs.non_ascii;
    B_str res;
    int nbytes = s->nbytes+newbytes;
    NEW_UNFILLED_STR(res,nbytes,nbytes);
    escape_str(res->str+12,s->str,res->nbytes-12,s->nbytes,255,bs.squotes>0 && bs.dquotes>0);
    if (bs.dquotes==0 && bs.squotes>0) {
        res->str[11] = '"';
        res->str[res->nbytes-2] = '"';
    } else {
        res->str[11] = '\'';
        res->str[res->nbytes-2] = '\'';
    }        
    memcpy(res->str, "bytearray(b",11);
    res->str[res->nbytes-1] = ')';
    return res;
}

B_str B_bytearrayD___repr__(B_bytearray s) {
    return B_bytearrayD___str__(s);
}

void B_bytearrayD___serialize__(B_bytearray str,$Serial$state state) {
    int nWords = str->nbytes/sizeof($WORD) + 1;         // # $WORDS needed to store str->str, including terminating 0.
    $ROW row = $add_header(BYTEARRAY_ID,1+nWords,state);
    long nbytes = (long)str->nbytes;                    
    memcpy(row->blob,&nbytes,sizeof($WORD));            
    memcpy(row->blob+1,str->str,nbytes+1);
}

B_bytearray B_bytearrayD___deserialize__(B_bytearray res, $Serial$state state) {
    $ROW this = state->row;
    state->row =this->next;
    state->row_no++;
    if(!res)
        res = acton_malloc(sizeof(struct B_bytearray));
    long nbytes;
    memcpy(&nbytes,this->blob,sizeof($WORD));
    res->$class = &B_bytearrayG_methods;
    res->nbytes = (long)nbytes;
    res->str = acton_malloc_atomic(nbytes+1);
    memcpy(res->str,this->blob+1,nbytes+1);
    return res;
}

// bytearray methods

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
    if (!fill) fill = space_bytearray;
    if (fill->nbytes != 1) {
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("center: fill bytearray not single char")));
    }
    int wval = from$int(width);
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
    if (fix_start_end(s->nbytes,&st,&en) < 0) return to$int(0);
    int stval = from$int(st);
    int enval = from$int(en);
    unsigned char *p = &s->str[stval];
    unsigned char *q = &p[enval-stval];
    int res = 0;
    int n = bmh(p,sub->str,q-p,sub->nbytes);
    while (n>=0) {
        res++;
        p += n + (sub->nbytes>0 ? sub->nbytes : 1);
        n = bmh(p,sub->str,q-p,sub->nbytes);
    }
    return to$int(res);
}

B_str B_bytearrayD_decode(B_bytearray s) {
    return to$str((char*)s->str);
}

B_bool B_bytearrayD_endswith(B_bytearray s, B_bytearray sub, B_int start, B_int end) {
    B_int st = start;
    B_int en = end;
    if (fix_start_end(s->nbytes,&st,&en) < 0) return B_False;
    int enval = from$int(en);
    unsigned char *p = &s->str[enval-sub->nbytes];
    unsigned char *q = sub->str;
    for (int i=0; i<sub->nbytes; i++) {
        if (*p == 0 || *p++ != *q++) {
            return B_False;
        }
    }
    return B_True;
}

B_bytearray B_bytearrayD_expandtabs(B_bytearray s, B_int tabsz){
    if (s->nbytes == 0) {
        return toB_bytearray("");
    }
    int pos = 0;
    int expanded = 0;
    int tabsize = from$int(tabsz);
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
    if (fix_start_end(s->nbytes,&st,&en) < 0) return to$int(-1);
    unsigned char *p = &s->str[from$int(st)];
    unsigned char *q = &s->str[from$int(en)];
    int n = bmh(p,sub->str,q-p,sub->nbytes);
    if (n<0) return to$int(-1);
    return to$int(n+p-s->str);
}

B_bytearray B_bytearrayD_from_hex(B_str s) {
    // Each byte is represented by 2 hex chars
    int strlen = s->nbytes;  // Changed from len to nbytes
    if (strlen % 2 != 0) {
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("from_hex: hex string must have even length")));
    }

    int bytelen = strlen / 2;
    char *result = acton_malloc_atomic(bytelen);

    for (int i = 0; i < strlen; i += 2) {
        char high = s->str[i];
        char low = s->str[i + 1];

        // Convert hex chars to values 0-15
        int high_val, low_val;

        // Handle high nibble
        if (high >= '0' && high <= '9')
            high_val = high - '0';
        else if (high >= 'a' && high <= 'f')
            high_val = high - 'a' + 10;
        else if (high >= 'A' && high <= 'F')
            high_val = high - 'A' + 10;
        else {
            $RAISE((B_BaseException)$NEW(B_ValueError,to$str("from_hex: invalid hex character")));
        }

        // Handle low nibble
        if (low >= '0' && low <= '9')
            low_val = low - '0';
        else if (low >= 'a' && low <= 'f')
            low_val = low - 'a' + 10;
        else if (low >= 'A' && low <= 'F')
            low_val = low - 'A' + 10;
        else {
            $RAISE((B_BaseException)$NEW(B_ValueError,to$str("from_hex: invalid hex character")));
        }

        // Combine into byte
        result[i/2] = (high_val << 4) | low_val;
    }

    return actBytearrayFromCStringLengthNoCopy(result, bytelen);
}

B_str B_bytearrayD_hex(B_bytearray s) {
    // Each byte becomes 2 hex chars, so output length is 2 * number of bytes
    int len = s->nbytes * 2;
    char *result = acton_malloc_atomic(len);

    // Hex digit lookup table
    const char hex_digits[] = "0123456789abcdef";

    // Convert each byte to two hex digits
    for (int i = 0; i < s->nbytes; i++) {
        unsigned char byte = s->str[i];
        result[i*2] = hex_digits[byte >> 4];     // High nibble
        result[i*2 + 1] = hex_digits[byte & 0xf]; // Low nibble
    }

    // Convert to Acton string without copying
    return to_str_noc(result);
}


B_int B_bytearrayD_index(B_bytearray s, B_bytearray sub, B_int start, B_int end) {
    B_int n = B_bytearrayD_find(s,sub,start,end);
    if (from$int(n)<0) {
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("index: substring not found")));
    }
    return n;
}

B_bool B_bytearrayD_isalnum(B_bytearray s) {
    if (s->nbytes==0)
        return B_False;
    for (int i=0; i<s->nbytes; i++) {
        unsigned char c = s->str[i];
        if (c < '0' || c > 'z' || (c > '9' && c < 'A') || (c > 'Z' && c < 'a'))
            return B_False;
    }
    return B_True;
}

B_bool B_bytearrayD_isalpha(B_bytearray s) {
    if (s->nbytes==0)
        return B_False;
    for (int i=0; i<s->nbytes; i++) {
        unsigned char c = s->str[i];
        if (c < 'A' || c > 'z' || (c > 'Z' && c < 'a'))
            return B_False;
    }
    return B_True;
}

B_bool B_bytearrayD_isascii(B_bytearray s) {
    for (int i=0; i<s->nbytes; i++) {
        unsigned char c = s->str[i];
        if (c > 0x7f)
            return B_False;
    }
    return B_True;
}

B_bool B_bytearrayD_isdigit(B_bytearray s) {
    if (s->nbytes==0)
        return B_False;
    for (int i=0; i<s->nbytes; i++) {
        unsigned char c = s->str[i];
        if (c<'0' || c > '9')
            return B_False;
    }
    return B_True;
}
 

B_bool B_bytearrayD_islower(B_bytearray s) {
    int has_lower = 0;
    for (int i=0; i < s->nbytes; i++) {
        unsigned char c = s->str[i];
        if (c >= 'A' && c <= 'Z')
            return B_False;
        if (c >= 'a' && c <= 'z')
            has_lower = 1;
    }
    return toB_bool(has_lower);
}

B_bool B_bytearrayD_isspace(B_bytearray s) {
    if (s->nbytes==0)
        return B_False;
    for (int i=0; i<s->nbytes; i++) {
        unsigned char c = s->str[i];
        if (c !=' ' && c != '\t' && c != '\n' && c != '\r' && c != '\x0b' && c != '\f')
            return B_False;
    }
    return B_True;
}

B_bool B_bytearrayD_istitle(B_bytearray s) {
    if (s->nbytes==0)
        return B_False;
    int incasedrun = 0;
    for (int i=0; i < s->nbytes; i++) {
        unsigned char c = s->str[i];
        if (c >='A' && c <= 'Z') {
            if (incasedrun)
                return B_False;
            incasedrun = 1;
        } else if (c >='a' && c <= 'z') {
            if (!incasedrun)
                return B_False;
        } else
            incasedrun = 0;
    }
    return B_True;
}

B_bool B_bytearrayD_isupper(B_bytearray s) {
    int has_upper = 0;
    for (int i=0; i < s->nbytes; i++) {
        unsigned char c = s->str[i];
        if (c >= 'a' && c <= 'z')
            return B_False;
        if (c >= 'a' && c <= 'z')
            has_upper = 1;
    }
    return toB_bool(has_upper);
}

B_bytearray B_bytearrayD_join(B_bytearray s, B_Iterable wit, $WORD iter) {
    int totbytes = 0;
    B_CollectionD_SequenceD_list wit2 = B_CollectionD_SequenceD_listG_witness;
    B_list lst = wit2->$class->__fromiter__(wit2,wit,iter);
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
    if (!fill)
        fill = space_bytearray;
    int wval = from$int(width);
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
        cs = whitespace_bytearray;
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
    int n = from$int(B_bytearrayD_find(s,sep,NULL,NULL));
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
        count = to$int(INT_MAX);
    int c = from$int(B_bytearrayD_count(s,old,NULL,NULL));
    int c0 = from$int(count) < c ? from$int(count) : c;
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
    if (fix_start_end(s->nbytes,&st,&en) < 0) return to$int(-1);
    unsigned char *p = &s->str[from$int(st)];
    unsigned char *q = &s->str[from$int(en)];
    int n = rbmh(p,sub->str,q-p,sub->nbytes);
    if (n<0) return to$int(-1);
    return to$int(n+p-s->str);
}


B_int B_bytearrayD_rindex(B_bytearray s, B_bytearray sub, B_int start, B_int end) {
    B_int n = B_bytearrayD_rfind(s,sub,start,end);
    if (from$int(n)<0) {
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("rindex for bytearray: substring not found")));
    };
    return n;
}

B_bytearray B_bytearrayD_rjust(B_bytearray s, B_int width, B_bytearray fill) {
    if (!fill)
        fill = space_bytearray;
    int wval = from$int(width);
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
    int n = from$int(B_bytearrayD_rfind(s,sep,NULL,NULL));
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
        cs = whitespace_bytearray;
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
    B_SequenceD_list wit = B_SequenceD_listG_witness;
    if (maxsplit == NULL || from$int(maxsplit) < 0) maxsplit = to$int(INT_MAX); 
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
                    if (res->length == from$int(maxsplit))
                        break; // we have now removed leading whitespace in remainder
                } 
            } else {
                if (inword) {
                    inword = 0;
                    B_bytearray word;
                    NEW_UNFILLED_BYTEARRAY(word,p-q);
                    memcpy(word->str,q,p-q);
                    wit->$class->append(wit,res,word);
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
                 wit->$class->append(wit,res,word);
            }
        } else {
            B_bytearray word;
            p = s->str+s->nbytes;
            NEW_UNFILLED_BYTEARRAY(word,p-q);
            memcpy(word->str,q,p-q);
             wit->$class->append(wit,res,word);
        }
        return res;
    } else { // separator given
        if (sep->nbytes==0) {
            $RAISE((B_BaseException)$NEW(B_ValueError,to$str("split for bytearray: separator is empty string")));
        }
        if (s->nbytes==0) { // for some unfathomable reason, this is the behaviour of the Python method
            wit->$class->append(wit,res,null_str);
            return res;
        }
        B_bytearray ls, rs, ssep;
        rs = s;
        // Note: This builds many intermediate rs strings...
        while (rs->nbytes>0 && res->length < from$int(maxsplit)) {
            B_tuple t = B_bytearrayD_partition(rs,sep);
            ssep = (B_bytearray)t->components[1];
            rs =  (B_bytearray)t->components[2];
             wit->$class->append(wit,res,(B_bytearray)t->components[0]);
        }
        if (ssep->nbytes>0)
            wit->$class->append(wit,res,rs);
        return res;
    }
}

B_list B_bytearrayD_splitlines(B_bytearray s, B_bool keepends) {
    if (!keepends)
        keepends = B_False;
    B_SequenceD_list wit = B_SequenceD_listG_witness;
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
            wit->$class->append(wit,res,line);
        }
    }
    if (q < p) {
        B_bytearray line;
        NEW_UNFILLED_BYTEARRAY(line,p-q);
        memcpy(line->str,q,p-q);
        wit->$class->append(wit,res,line);
    }
    return res;
} 

B_bool B_bytearrayD_startswith(B_bytearray s, B_bytearray sub, B_int start, B_int end) {
    B_int st = start;
    B_int en = end;
    if (fix_start_end(s->nbytes,&st,&en) < 0) return B_False;
    unsigned char *p = s->str + from$int(st);
    if (sub->nbytes > 0 && p+sub->nbytes >= s->str+s->nbytes) return B_False;
    unsigned char *q = sub->str;
    for (int i=0; i<sub->nbytes; i++) {
        if (p >= s->str + from$int(en) || *p++ != *q++) {
            return B_False;
        }
    }
    return B_True;
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
    int wval = from$int(width);
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

 
// Ord


B_bool B_OrdD_bytearrayD___eq__ (B_OrdD_bytearray wit, B_bytearray a, B_bytearray b) {
    return toB_bool(strcmp((char *)a->str,(char *)b->str)==0);
}

B_bool B_OrdD_bytearrayD___ne__ (B_OrdD_bytearray wit, B_bytearray a, B_bytearray b) {
    return  toB_bool(strcmp((char *)a->str,(char *)b->str)!=0);
}

B_bool B_OrdD_bytearrayD___lt__ (B_OrdD_bytearray wit, B_bytearray a, B_bytearray b) {
    return toB_bool(strcmp((char *)a->str,(char *)b->str)<0);
}

B_bool B_OrdD_bytearrayD___le__ (B_OrdD_bytearray wit, B_bytearray a, B_bytearray b){
    return toB_bool(strcmp((char *)a->str,(char *)b->str)<=0);
}

B_bool B_OrdD_bytearrayD___gt__ (B_OrdD_bytearray wit, B_bytearray a, B_bytearray b){
    return toB_bool(strcmp((char *)a->str,(char *)b->str)>0);
}

B_bool B_OrdD_bytearrayD___ge__ (B_OrdD_bytearray wit, B_bytearray a, B_bytearray b){
    return toB_bool(strcmp((char *)a->str,(char *)b->str)>=0);
}

// Container

// Iterable

static B_int B_IteratorB_bytearrayD_next(B_IteratorB_bytearray self) {
    if (self->nxt >= self->src->nbytes)
        $RAISE ((B_BaseException)$NEW(B_StopIteration, to$str("bytearray iterator terminated")));
    return to$int(self->src->str[self->nxt++]);
}

B_NoneType B_IteratorB_bytearrayD_init(B_IteratorB_bytearray self, B_bytearray b) {
    self->src = b;
    self->nxt = 0;
    return B_None;
}

B_bool B_IteratorB_bytearrayD_bool(B_IteratorB_bytearray self) {
    return B_True;
}

B_str B_IteratorB_bytearrayD_str(B_IteratorB_bytearray self) {
    return $FORMAT("<bytearray iterator object at %p>", self);
}

void B_IteratorB_bytearrayD_serialize(B_IteratorB_bytearray self,$Serial$state state) {
    $step_serialize(self->src,state);
    $step_serialize(to$int(self->nxt),state);
}

B_IteratorB_bytearray B_IteratorB_bytearray$_deserialize(B_IteratorB_bytearray res, $Serial$state state) {
    if(!res)
        res = $DNEW(B_IteratorB_bytearray,state);
    res->src = (B_bytearray)$step_deserialize(state);
    res->nxt = from$int((B_int)$step_deserialize(state));
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

B_Iterator B_ContainerD_bytearrayD___iter__ (B_ContainerD_bytearray wit, B_bytearray str) {
    return (B_Iterator)$NEW(B_IteratorB_bytearray,str);
}

B_bytearray B_ContainerD_bytearrayD___fromiter__ (B_ContainerD_bytearray wit, B_Iterable wit2, $WORD iter) {
    return B_bytearrayD_join(toB_bytearray(""),wit2,iter);
}

B_int B_ContainerD_bytearrayD___len__ (B_ContainerD_bytearray wit, B_bytearray str) {
    return to$int(str->nbytes);
}

B_bool B_ContainerD_bytearrayD___contains__(B_ContainerD_bytearray wit, B_bytearray self, B_int n) {
    long res = 0;
    for (int i=0; i < self->nbytes; i++) {
        if (self->str[i] == (unsigned char)from$int(n)) {
            res = 1;
            break;
        }
    }
    return toB_bool(res);
}

B_bool B_ContainerD_bytearrayD___containsnot__(B_ContainerD_bytearray wit, B_bytearray self, B_int n) {
    return  toB_bool(!B_ContainerD_bytearrayD___contains__(wit,self,n)->val);
}

// Sequence

B_int B_SequenceD_bytearrayD___getitem__ (B_SequenceD_bytearray wit, B_bytearray self, B_int n) {
    long ix = from$int(n);
    long ix0 = ix < 0 ? self->nbytes + ix : ix;
    if (ix0<0 || ix0 >= self->nbytes)
        $RAISE((B_BaseException)$NEW(B_IndexError, to$int(ix0), to$str("getitem: index outside bytearray")));
    return to$int((long)self->str[ix0]);
}

B_NoneType B_SequenceD_bytearrayD___setitem__ (B_SequenceD_bytearray wit, B_bytearray self, B_int n, B_int v) {
    long ix = from$int(n);
    long val = from$int(v);
    long ix0 = ix < 0 ? self->nbytes + ix : ix;
    if (ix0<0 || ix0 >= self->nbytes)
        $RAISE((B_BaseException)$NEW(B_IndexError, to$int(ix0), to$str("setitem: index outside bytearray")));
    if (val<0 || val>255)
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("setitem for bytearray: value outside [0..255]")));
    self->str[ix0] = (unsigned char)val;
    return B_None;
}

B_NoneType B_SequenceD_bytearrayD___delitem__ (B_SequenceD_bytearray wit, B_bytearray self, B_int n) {
    long ix = from$int(n);
    int len = self->nbytes;
    long ix0 = ix < 0 ? len + ix : ix;
    if (ix0 < 0 || ix0 >= len)
        $RAISE((B_BaseException)$NEW(B_IndexError, to$int(ix0), to$str("delitem: index outside bytearray")));
    memmove(self->str + ix0,self->str + (ix0 + 1),len-(ix0+1));
    self->nbytes--;
    return B_None;
}

B_NoneType B_SequenceD_bytearrayD_insert(B_SequenceD_bytearray wit, B_bytearray self, B_int n, B_int elem) {
    long ix = from$int(n);
    int len = self->nbytes;
    expand_bytearray(self,1);
    int ix0 = ix < 0 ? (len+ix < 0 ? 0 : len+ix) : (ix < len ? ix : len);
    memmove(self->str + (ix0 + 1),
            self->str + ix0 ,
            len - ix0 + 1); // +1 to move also terminating '\0'
    self->str[ix0] = (unsigned char)from$int(elem) & 0xff;
    self->nbytes++;
    return B_None;
}

B_NoneType B_SequenceD_bytearrayD_append(B_SequenceD_bytearray wit, B_bytearray self, B_int elem) {
    expand_bytearray(self,1);
    self->str[self->nbytes++] = (unsigned char)from$int(elem) & 0xff;
    self->str[self->nbytes] = '\0';
    return B_None;
}

B_NoneType B_SequenceD_bytearrayD_reverse(B_SequenceD_bytearray wit, B_bytearray self) {
    int len = self->nbytes;
    for (int i = 0; i < len/2; i++) {
        unsigned char tmp = self->str[i];
        self->str[i] = self->str[len-1-i];
        self->str[len-1-i] = tmp;
    }
    return B_None;
}

B_Iterator B_SequenceD_bytearrayD___reversed__(B_SequenceD_bytearray wit, B_bytearray self) {
    B_bytearray copy = B_bytearrayD_copy(self);
    B_SequenceD_bytearrayD_reverse(wit,copy);
    return B_ContainerD_bytearrayD___iter__ (NULL, copy);
}

B_bytearray B_SequenceD_bytearrayD___getslice__ (B_SequenceD_bytearray wit, B_bytearray self, B_slice slc) {
    int len = self->nbytes;
    long start, stop, step, slen;
    normalize_slice(slc, len, &slen, &start, &stop, &step);
    B_bytearray res;
    NEW_UNFILLED_BYTEARRAY(res,slen);
    long t = start;
    for (int i=0; i<slen; i++) {
        B_int w = B_SequenceD_bytearrayD___getitem__(wit, self, to$int(t));
        B_SequenceD_bytearrayD___setitem__(wit, res , to$int(i), w);
        t += step;
    }
    return res;
}

B_NoneType B_SequenceD_bytearrayD___setslice__ (B_SequenceD_bytearray wit,  B_bytearray self, B_Iterable wit2, B_slice slc, $WORD iter) {
    B_Iterator it = wit2->$class->__iter__(wit2,iter);
    int len = self->nbytes;
    B_bytearray other;    
    NEW_UNFILLED_BYTEARRAY(other,0);
    $WORD w;
    while ((w=it->$class->__next__(it)))
        B_SequenceD_bytearrayD_append(wit, other,(B_int)w);
    int olen = other->nbytes; 
    long start, stop, step, slen;
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
        return B_None;
    // now we know that step=1
    if (olen < slen) {
        memmove(self->str + start + copy,
                self->str + start + slen,
                len-(start+slen));
        self->nbytes-=slen-olen;
        return B_None;
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
    return B_None;
}

B_NoneType B_SequenceD_bytearrayD___delslice__ (B_SequenceD_bytearray wit,  B_bytearray self, B_slice slc) {
    int len = self->nbytes;
    long start, stop, step, slen;
    normalize_slice(slc, len, &slen, &start, &stop, &step);
    if (slen==0) return B_None;
    unsigned char *p = self->str + start;
    for (int i=0; i<slen-1; i++) {
        memmove(p,p+i+1,step-1);
        p+=step-1;
    }
    memmove(p,p+slen,len-1-(start+step*(slen-1)));
    self->nbytes-=slen;
    self->str[self->nbytes] = '\0';
    return B_None;
}

// Collection


B_Iterator B_CollectionD_SequenceD_bytearrayD___iter__ (B_CollectionD_SequenceD_bytearray wit, B_bytearray str) {
    return (B_Iterator)$NEW(B_IteratorB_bytearray,str);
}

B_bytearray B_CollectionD_SequenceD_bytearrayD___fromiter__ (B_CollectionD_SequenceD_bytearray wit, B_Iterable wit2, $WORD iter) {
    return B_bytearrayD_join(toB_bytearray(""),wit2,iter);
}

B_int B_CollectionD_SequenceD_bytearrayD___len__ (B_CollectionD_SequenceD_bytearray wit, B_bytearray str) {
    return to$int(str->nbytes);
}

// Times

B_bytearray B_TimesD_SequenceD_bytearrayD___add__ (B_TimesD_SequenceD_bytearray wit, B_bytearray a, B_bytearray b) {
    B_bytearray res;
    NEW_UNFILLED_BYTEARRAY(res,a->nbytes+b->nbytes);
    memcpy(res->str,a->str,a->nbytes);
    memcpy(res->str+a->nbytes,b->str,b->nbytes);
    return res;
}

B_bytearray B_TimesD_SequenceD_bytearrayD___zero__ (B_TimesD_SequenceD_bytearray wit) {
    return toB_bytearray("");
}

B_bytearray B_TimesD_SequenceD_bytearrayD___mul__ (B_TimesD_SequenceD_bytearray wit, B_bytearray a, B_int n) {
    int nval = from$int(n);
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
 
// End of bytearray implementation ////////////////////////////////////////////////



// bytes implementation ///////////////////////////////////////////////////////////


// Conversion to and from C strings

B_bytes to$bytes(char *str) {
    B_bytes res;
    int len = strlen(str);
    NEW_UNFILLED_BYTES(res,len);
    memcpy(res->str,str,len);
    return res;
}

B_bytes to$bytesD_len(char *str, int len) {
    B_bytes res;
    NEW_UNFILLED_BYTES(res, len);
    memcpy(res->str, str, len);
    return res;
}

B_bytes actBytesFromCString(char *str) {
    B_bytes res;
    int len = strlen(str);
    NEW_UNFILLED_BYTES(res,len);
    memcpy(res->str,str,len);
    return res;
}

B_bytes actBytesFromCStringNoCopy(char *str) {
    B_bytes res = acton_malloc(sizeof(struct B_bytes));
    res->$class = &B_bytesG_methods;
    res->nbytes = strlen(str);
    res->str =  (unsigned char*)str;
    return res;
}

B_bytes actBytesFromCStringLength(char *str, int len) {
    B_bytes res;
    NEW_UNFILLED_BYTES(res, len);
    memcpy(res->str, str, len);
    return res;
}

B_bytes actBytesFromCStringLengthNoCopy(char *str, int length) {
    B_bytes res = acton_malloc(sizeof(struct B_bytes));
    res->$class = &B_bytesG_methods;
    res->nbytes = length;
    res->str =  (unsigned char*)str;
    return res;
}

unsigned char *fromB_bytes(B_bytes b) {
    return b->str;
}

// Auxiliaries

static B_bytes B_bytesD_copy(B_bytes s) {
    B_bytes res;
    NEW_UNFILLED_BYTES(res,s->nbytes);
    res->nbytes = s->nbytes;
    memcpy(res->str,s->str,s->nbytes);
    return res;
}

 

// Bytes methods, implementations

// General methods ////////////////////////////////////////////////////////////// 

B_bytes B_bytesG_new(B_Iterable iter, $WORD wit) {
    return $NEW(B_bytes, iter, wit);
}

B_NoneType B_bytesD___init__(B_bytes self, B_Iterable wit, $WORD iter) {
    B_CollectionD_SequenceD_list wit2 = B_CollectionD_SequenceD_listG_witness;
    B_list lst = wit2->$class->__fromiter__(wit2,wit,iter);
    int len = lst->length;
    self->nbytes = len;
    self->str = acton_malloc_atomic(len+1);
    self->str[len] = 0;
    for (int i=0; i< len; i++) {
        int n = from$int((B_int)lst->data[i]);
        if (0<=n && n <= 255)
            self->str[i] = n;
        else
            $RAISE((B_BaseException)$NEW(B_ValueError,to$str("bytes constructor: element outside [0..255]")));
    }
    return B_None;
}

B_bool B_bytesD___bool__(B_bytes s) {
    return toB_bool(s->nbytes > 0);
};

B_str B_bytesD___str__(B_bytes s) {
    struct byte_counts bs = byte_count(s->str, s->nbytes);
    int newbytes = 3+bs.escaped+3*bs.non_printable+(bs.dquotes>0 && bs.dquotes>0 ? bs.squotes : 0)+3*bs.non_ascii;
    B_str res;
    int nbytes = s->nbytes+newbytes;
    NEW_UNFILLED_STR(res,nbytes,nbytes);
    escape_str(res->str+2,s->str,res->nbytes-2,s->nbytes,255,bs.squotes>0 && bs.dquotes>0);
    if (bs.dquotes==0 && bs.squotes>0) {
        res->str[1] = '"';
        res->str[res->nbytes-1] = '"';
    } else {
        res->str[1] = '\'';
        res->str[res->nbytes-1] = '\'';
    }        
    res->str[0] = 'b';
    return res;
}

B_str B_bytesD___repr__(B_bytes s) {
    return  B_bytesD___str__(s);
}

void B_bytesD___serialize__(B_bytes str,$Serial$state state) {
    int nWords = str->nbytes/sizeof($WORD) + 1;         // # $WORDS needed to store str->str, including terminating 0.
    $ROW row = $add_header(STR_ID,1+nWords,state);
    long nbytes = (long)str->nbytes;                    
    memcpy(row->blob,&nbytes,sizeof($WORD));            
    memcpy(row->blob+1,str->str,nbytes+1);
}

B_bytes B_bytesD___deserialize__(B_bytes self, $Serial$state state) {
    $ROW this = state->row;
    state->row =this->next;
    state->row_no++;
    B_bytes res = acton_malloc(sizeof(struct B_bytes));
    long nbytes;
    memcpy(&nbytes,this->blob,sizeof($WORD));
    res->$class = &B_bytesG_methods;
    res->nbytes = (long)nbytes;
    res->str = acton_malloc_atomic(nbytes+1);
    memcpy(res->str,this->blob+2,nbytes+1);
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
    int wval = from$int(width);
    if (!fill) fill = to$bytes(" ");
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
    if (fix_start_end(s->nbytes,&st,&en) < 0) return to$int(0);
    int stval = from$int(st);
    unsigned char *p = &s->str[stval];
    unsigned char *q = &p[from$int(en)-stval];
    int res = 0;
    int n = bmh(p,sub->str,q-p,sub->nbytes);
    while (n>=0) {
        res++;
        p += n + (sub->nbytes>0 ? sub->nbytes : 1);
        n = bmh(p,sub->str,q-p,sub->nbytes);
    }
    return to$int(res);
}

B_str B_bytesD_decode(B_bytes s) {
    return to$str((char*)s->str);
}

B_bool B_bytesD_endswith(B_bytes s, B_bytes sub, B_int start, B_int end) {
    B_int st = start;
    B_int en = end;
    if (fix_start_end(s->nbytes,&st,&en) < 0) return B_False;
    unsigned char *p = &s->str[from$int(en)-sub->nbytes];
    unsigned char *q = sub->str;
    for (int i=0; i<sub->nbytes; i++) {
        if (*p == 0 || *p++ != *q++) {
            return B_False;
        }
    }
    return B_True;
}

B_bytes B_bytesD_expandtabs(B_bytes s, B_int tabsz){
    if (s->nbytes == 0) {
        return null_bytes;
    }
    int pos = 0;
    int expanded = 0;
    int tabsize = from$int(tabsz);
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
    if (fix_start_end(s->nbytes,&st,&en) < 0) return to$int(-1);
    unsigned char *p = &s->str[from$int(st)];
    unsigned char *q = &s->str[from$int(en)];
    int n = bmh(p,sub->str,q-p,sub->nbytes);
    if (n<0) return to$int(-1);
    return to$int(n+p-s->str);
}

B_bytes B_bytesD_from_hex(B_str s) {
    if (s->nbytes == 0)
        return null_bytes;
    // Each byte is represented by 2 hex chars
    int strlen = s->nbytes;  // Changed from len to nbytes
    if (strlen % 2 != 0) {
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("from_hex: hex string must have even length")));
    }

    int bytelen = strlen / 2;
    char *result = acton_malloc_atomic(bytelen);

    for (int i = 0; i < strlen; i += 2) {
        char high = s->str[i];
        char low = s->str[i + 1];

        // Convert hex chars to values 0-15
        int high_val, low_val;

        // Handle high nibble
        if (high >= '0' && high <= '9')
            high_val = high - '0';
        else if (high >= 'a' && high <= 'f')
            high_val = high - 'a' + 10;
        else if (high >= 'A' && high <= 'F')
            high_val = high - 'A' + 10;
        else {
            $RAISE((B_BaseException)$NEW(B_ValueError,to$str("from_hex: invalid hex character")));
        }

        // Handle low nibble
        if (low >= '0' && low <= '9')
            low_val = low - '0';
        else if (low >= 'a' && low <= 'f')
            low_val = low - 'a' + 10;
        else if (low >= 'A' && low <= 'F')
            low_val = low - 'A' + 10;
        else {
            $RAISE((B_BaseException)$NEW(B_ValueError,to$str("from_hex: invalid hex character")));
        }

        // Combine into byte
        result[i/2] = (high_val << 4) | low_val;
    }

    return actBytesFromCStringLengthNoCopy(result, bytelen);
}

B_str B_bytesD_hex(B_bytes s) {
    if (s->nbytes == 0)
        return null_str;
    // Each byte becomes 2 hex chars, so output length is 2 * number of bytes
    int len = s->nbytes * 2;
    char *result = acton_malloc_atomic(len);

    // Hex digit lookup table
    const char hex_digits[] = "0123456789abcdef";

    // Convert each byte to two hex digits
    for (int i = 0; i < s->nbytes; i++) {
        unsigned char byte = s->str[i];
        result[i*2] = hex_digits[byte >> 4];     // High nibble
        result[i*2 + 1] = hex_digits[byte & 0xf]; // Low nibble
    }

    // Convert to Acton string without copying
    return to_str_noc(result);
}

B_int B_bytesD_index(B_bytes s, B_bytes sub, B_int start, B_int end) {
    B_int n = B_bytesD_find(s,sub,start,end);
    if (from$int(n)<0) {
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("index: substring not found")));
    }
    return n;
}

B_bool B_bytesD_isalnum(B_bytes s) {
    if (s->nbytes==0)
        return B_False;
    for (int i=0; i<s->nbytes; i++) {
        unsigned char c = s->str[i];
        if (c < '0' || c > 'z' || (c > '9' && c < 'A') || (c > 'Z' && c < 'a'))
            return B_False;
    }
    return B_True;
}

B_bool B_bytesD_isalpha(B_bytes s) {
    if (s->nbytes==0)
        return B_False;
    for (int i=0; i<s->nbytes; i++) {
        unsigned char c = s->str[i];
        if (c < 'A' || c > 'z' || (c > 'Z' && c < 'a'))
            return B_False;
    }
    return B_True;
}

B_bool B_bytesD_isascii(B_bytes s) {
    for (int i=0; i<s->nbytes; i++) {
        unsigned char c = s->str[i];
        if (c > 0x7f)
            return B_False;
    }
    return B_True;
}

B_bool B_bytesD_isdigit(B_bytes s) {
    if (s->nbytes==0)
        return B_False;
    for (int i=0; i<s->nbytes; i++) {
        unsigned char c = s->str[i];
        if (c<'0' || c > '9')
            return B_False;
    }
    return B_True;
}
 

B_bool B_bytesD_islower(B_bytes s) {
    int has_lower = 0;
    for (int i=0; i < s->nbytes; i++) {
        unsigned char c = s->str[i];
        if (c >= 'A' && c <= 'Z')
            return B_False;
        if (c >= 'a' && c <= 'z')
            has_lower = 1;
    }
    return toB_bool(has_lower);
}

B_bool B_bytesD_isspace(B_bytes s) {
    if (s->nbytes==0)
        return B_False;
    for (int i=0; i<s->nbytes; i++) {
        unsigned char c = s->str[i];
        if (c !=' ' && c != '\t' && c != '\n' && c != '\r' && c != '\x0b' && c != '\f')
            return B_False;
    }
    return B_True;
}

B_bool B_bytesD_istitle(B_bytes s) {
    if (s->nbytes==0)
        return B_False;
    int incasedrun = 0;
    for (int i=0; i < s->nbytes; i++) {
        unsigned char c = s->str[i];
        if (c >='A' && c <= 'Z') {
            if (incasedrun)
                return B_False;
            incasedrun = 1;
        } else if (c >='a' && c <= 'z') {
            if (!incasedrun)
                return B_False;
        } else
            incasedrun = 0;
    }
    return B_True;
}

B_bool B_bytesD_isupper(B_bytes s) {
    int has_upper = 0;
    for (int i=0; i < s->nbytes; i++) {
        unsigned char c = s->str[i];
        if (c >= 'a' && c <= 'z')
            return B_False;
        if (c >= 'a' && c <= 'z')
            has_upper = 1;
    }
    return toB_bool(has_upper);
}

B_bytes B_bytesD_join(B_bytes s, B_Iterable wit, $WORD iter) {
    int totbytes = 0;
    B_CollectionD_SequenceD_list wit2 = B_CollectionD_SequenceD_listG_witness;
    B_list lst = wit2->$class->__fromiter__(wit2,wit,iter);
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
    if (!fill)
        fill = space_bytes;
    int wval = from$int(width);
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
        cs = whitespace_bytes;
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
    int n = from$int(B_bytesD_find(s,sep,NULL,NULL));
    if (n<0) {
        return $NEWTUPLE(3,s,to$bytes(""),to$bytes(""));
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
    if (prefix->nbytes > s->nbytes || memcmp(s->str,prefix->str,prefix->nbytes))
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
    if (suffix->nbytes > s->nbytes || memcmp(s->str+s->nbytes-suffix->nbytes,suffix->str,suffix->nbytes))
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
        count = to$int(INT_MAX);
    int c = from$int(B_bytesD_count(s,old,NULL,NULL));
    int c0 = from$int(count) < c ? from$int(count) : c;
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
    if (fix_start_end(s->nbytes,&st,&en) < 0) return to$int(-1);
    unsigned char *p = &s->str[from$int(st)];
    unsigned char *q = &s->str[from$int(en)];
    int n = rbmh(p,sub->str,q-p,sub->nbytes);
    if (n<0) return to$int(-1);
    return to$int(n+p-s->str);
}


B_int B_bytesD_rindex(B_bytes s, B_bytes sub, B_int start, B_int end) {
    B_int n = B_bytesD_rfind(s,sub,start,end);
    if (from$int(n)<0) {
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("rindex for bytes: substring not found")));
    };
    return n;
}

B_bytes B_bytesD_rjust(B_bytes s, B_int width, B_bytes fill) {
    if (!fill)
        fill = space_bytes;
    if (fill->nbytes != 1) {
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("rjust: fill string not single char")));
    }
    int wval = from$int(width); 
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
    int n = from$int(B_bytesD_rfind(s,sep,NULL,NULL));
    if (n<0) {
        return $NEWTUPLE(3,to$bytes(""),to$bytes(""),s);
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
        cs = whitespace_bytes;
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
    B_SequenceD_list wit = B_SequenceD_listG_witness;
    if (maxsplit == NULL || from$int(maxsplit) < 0) maxsplit = to$int(INT_MAX); 
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
                    if (res->length == from$int(maxsplit))
                        break; // we have now removed leading whitespace in remainder
                } 
            } else {
                if (inword) {
                    inword = 0;
                    B_bytes word;
                    NEW_UNFILLED_BYTES(word,p-q);
                    memcpy(word->str,q,p-q);
                    wit->$class->append(wit,res,word);
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
                wit->$class->append(wit,res,word);
            }
        } else {
            B_bytes word;
            p = s->str+s->nbytes;
            NEW_UNFILLED_BYTES(word,p-q);
            memcpy(word->str,q,p-q);
           wit->$class->append(wit,res,word);
        }
        return res;
    } else { // separator given
        if (sep->nbytes==0) {
            $RAISE((B_BaseException)$NEW(B_ValueError,to$str("split for bytes: separator is empty string")));
        }
        if (s->nbytes==0) { // for some unfathomable reason, this is the behaviour of the Python method
            wit->$class->append(wit,res,null_str);
            return res;
        }
        B_bytes ls, rs, ssep;
        rs = s;
        // Note: This builds many intermediate rs strings...
        while (rs->nbytes>0 && res->length < from$int(maxsplit)) {
            B_tuple t = B_bytesD_partition(rs,sep);
            ssep = (B_bytes)t->components[1];
            rs =  (B_bytes)t->components[2];
            wit->$class->append(wit,res,(B_bytes)t->components[0]);
        }
        if (ssep->nbytes>0)
            wit->$class->append(wit,res,rs);
        return res;
    }
}

B_list B_bytesD_splitlines(B_bytes s, B_bool keepends) {
    if (!keepends)
        keepends = B_False;
    B_list res = $NEW(B_list,NULL,NULL);
    B_SequenceD_list wit = B_SequenceD_listG_witness;
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
            wit->$class->append(wit,res,line);
        }
    }
    if (q < p) {
        B_bytes line;
        NEW_UNFILLED_BYTES(line,p-q);
        memcpy(line->str,q,p-q);
        wit->$class->append(wit,res,line);
    }
    return res;
} 

B_bool B_bytesD_startswith(B_bytes s, B_bytes sub, B_int start, B_int end) {
    B_int st = start;
    B_int en = end;
    if (fix_start_end(s->nbytes,&st,&en) < 0) return B_False;
    unsigned char *p = s->str + from$int(st);
    if (sub->nbytes > 0 && p+sub->nbytes >= s->str+s->nbytes) return B_False;
    unsigned char *q = sub->str;
    for (int i=0; i<sub->nbytes; i++) {
        if (p >= s->str + from$int(en) || *p++ != *q++) {
            return B_False;
        }
    }
    return B_True;
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
    int wval = from$int(width);
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

// protocol methods ///////////////////////////////////////////////////
 
// Ord

 
B_bool B_OrdD_bytesD___eq__ (B_OrdD_bytes wit, B_bytes a, B_bytes b) {
    if (a->nbytes != b->nbytes)
        return B_False;
    for (int i=0; i < a->nbytes; i++)
        if (a->str[i] != b->str[i])
            return B_False;
    return B_True;
}

B_bool B_OrdD_bytesD___ne__ (B_OrdD_bytes wit, B_bytes a, B_bytes b) {
    return  toB_bool(!B_OrdD_bytesD___eq__(wit,a,b)->val);
}

B_bool B_OrdD_bytesD___lt__ (B_OrdD_bytes wit, B_bytes a, B_bytes b) {
    int minl = a->nbytes<b->nbytes ? a->nbytes : b->nbytes;
    int i=0;
    while (i<minl && a->str[i]==b->str[i]) i++;
    if (i==a->nbytes)
        return toB_bool(i<b->nbytes);
    if (i==b->nbytes)
        return B_False;
    return toB_bool(a->str[i]<b->str[i]);
}

B_bool B_OrdD_bytesD___le__ (B_OrdD_bytes wit, B_bytes a, B_bytes b){
    return toB_bool(!B_OrdD_bytesD___lt__(wit,b,a)->val);
}

B_bool B_OrdD_bytesD___gt__ (B_OrdD_bytes wit, B_bytes a, B_bytes b){
    return B_OrdD_bytesD___lt__(wit,b,a);
}

B_bool B_OrdD_bytesD___ge__ (B_OrdD_bytes wit, B_bytes a, B_bytes b){
    return toB_bool(!B_OrdD_bytesD___lt__(wit,a,b)->val);
}

// Container

// Iterable ///////////////////////////////////////////////////////////////////////////

B_IteratorB_bytes B_IteratorB_bytesG_new(B_bytes str) {
    return $NEW(B_IteratorB_bytes, str);
}

B_NoneType B_IteratorB_bytesD_init(B_IteratorB_bytes self, B_bytes str) {
    self->src = str;
    self->nxt = 0;
    return B_None;
}

void B_IteratorB_bytesD_serialize(B_IteratorB_bytes self,$Serial$state state) {
    $step_serialize(self->src,state);
    $step_serialize(to$int(self->nxt),state);
}


B_IteratorB_bytes B_IteratorB_bytes$_deserialize(B_IteratorB_bytes res, $Serial$state state) {
    if (!res)
        res = $DNEW(B_IteratorB_bytes,state);
    res->src = (B_bytes)$step_deserialize(state);
    res->nxt = from$int((B_int)$step_deserialize(state));
    return res;
}

B_bool B_IteratorB_bytesD_bool(B_IteratorB_bytes self) {
    return B_True;
}

B_str B_IteratorB_bytesD_str(B_IteratorB_bytes self) {
    return $FORMAT("<bytes iterator object at %p>", self);
}

// this is next function for forward iteration
static B_int B_IteratorB_bytesD_next(B_IteratorB_bytes self) {
    if (self->nxt >= self->src->nbytes)
        $RAISE ((B_BaseException)$NEW(B_StopIteration, to$str("bytes iterator terminated")));
    return to$int(self->src->str[self->nxt++]);
}

struct B_IteratorB_bytesG_class B_IteratorB_bytesG_methods = {"B_IteratorB_bytes",UNASSIGNED,($SuperG_class)&B_IteratorG_methods, B_IteratorB_bytesD_init,
                                                        B_IteratorB_bytesD_serialize, B_IteratorB_bytes$_deserialize,
                                                        B_IteratorB_bytesD_bool, B_IteratorB_bytesD_str,  B_IteratorB_bytesD_str, B_IteratorB_bytesD_next};

B_Iterator B_ContainerD_bytesD___iter__ (B_ContainerD_bytes wit, B_bytes str) {
    return (B_Iterator)$NEW(B_IteratorB_bytes,str);
}

B_bytes B_ContainerD_bytesD___fromiter__ (B_ContainerD_bytes wit, B_Iterable wit2, $WORD iter) {
    return B_bytesD_join(to$bytes(""),wit2,iter);
}

B_int B_ContainerD_bytesD___len__ (B_ContainerD_bytes wit, B_bytes str) {
    return to$int(str->nbytes);
}

B_bool B_ContainerD_bytesD___contains__ (B_ContainerD_bytes wit, B_bytes str, B_int n) {
    long res = 0;
    for (int i=0; i < str->nbytes; i++) {
        if (str->str[i] == (unsigned char)from$int(n)) {
            res = 1;
            break;
        }
    }
    return toB_bool(res);
}

B_bool B_ContainerD_bytesD___containsnot__ (B_ContainerD_bytes wit, B_bytes str, B_int n) {
    return toB_bool(!B_ContainerD_bytesD___contains__(wit, str, n)->val);
}  

// Sliceable

B_int B_SliceableD_bytesD___getitem__ (B_SliceableD_bytes wit, B_bytes str, B_int n) {
    long ix = from$int(n);
    long ix0 = ix < 0 ? str->nbytes + ix : ix;
    if (ix0<0 || ix0 >= str->nbytes)
        $RAISE((B_BaseException)$NEW(B_IndexError, to$int(ix0), to$str("getitem: index outside bytesarray")));
    return to$int((long)str->str[ix0]);
}

B_NoneType B_SliceableD_bytesD___setitem__ (B_SliceableD_bytes wit, B_bytes str, B_int i, B_int val) {
    $RAISE((B_BaseException)$NEW(B_NotImplementedError,to$str("call to mutating method setitem on bytes")));
    return B_None;
}

B_NoneType B_SliceableD_bytesD___delitem__ (B_SliceableD_bytes wit, B_bytes str, B_int i) {
    $RAISE((B_BaseException)$NEW(B_NotImplementedError,to$str("call to mutating method delitem on bytes")));
    return B_None;
}

B_bytes B_SliceableD_bytesD___getslice__ (B_SliceableD_bytes wit, B_bytes str, B_slice slc) {
    long start, stop, step, slen;
    normalize_slice(slc, str->nbytes, &slen, &start, &stop, &step);
    //slice notation has been eliminated and default values applied
    B_bytes res;
    NEW_UNFILLED_BYTES(res,slen);
    int t = start;
    for (int i=0; i<slen; i++) {
        res->str[i] = str->str[t];
        t += step;
    }
    return res;
}

B_NoneType B_SliceableD_bytesD___setslice__ (B_SliceableD_bytes wit, B_bytes str, B_Iterable wit2, B_slice slc, $WORD iter) {
    $RAISE((B_BaseException)$NEW(B_NotImplementedError,to$str("call to mutating method setslice on bytes")));
    return B_None;
}

B_NoneType B_SliceableD_bytesD___delslice__ (B_SliceableD_bytes wit, B_bytes str, B_slice slc) {
    $RAISE((B_BaseException)$NEW(B_NotImplementedError,to$str("call to mutating method delslice on bytes")));
    return B_None;
}

// Times

 
B_bytes B_TimesD_bytesD___add__ (B_TimesD_bytes wit, B_bytes s, B_bytes t) {
    B_bytes res;
    NEW_UNFILLED_BYTES(res,s->nbytes + t->nbytes);
    memcpy(res->str,s->str,s->nbytes);
    memcpy(res->str+s->nbytes,t->str,t->nbytes);
    return res;
}

B_bytes B_TimesD_bytesD___zero__ (B_TimesD_bytes wit) {
    return to$bytes("");
}

B_bytes B_TimesD_bytesD___mul__ (B_TimesD_bytes wit, B_bytes a, B_int n) {
    int nval = from$int(n);
    if (nval <= 0)
        return to$bytes("");
    else {
        B_bytes res;
        NEW_UNFILLED_BYTES(res, a->nbytes * nval);
        for (int i=0; i<nval; i++)
            memcpy(res->str + i*a->nbytes,a->str,a->nbytes);
        return res;
    }
}

// Hashable


B_bool B_HashableD_bytesD___eq__ (B_HashableD_bytes wit, B_bytes a, B_bytes b) {
    if (a->nbytes != b->nbytes)
        return B_False;
    for (int i=0; i < a->nbytes; i++)
        if (a->str[i] != b->str[i])
            return B_False;
    return B_True;
}

B_bool B_HashableD_bytesD___ne__ (B_HashableD_bytes wit, B_bytes a, B_bytes b) {
    return  toB_bool(!B_HashableD_bytesD___eq__(wit,a,b)->val);
}

B_u64 B_HashableD_bytesD___hash__(B_HashableD_bytes wit, B_bytes a) {
    return toB_u64(zig_hash_wyhash_hash(0,a));
}

B_NoneType B_HashableD_bytesD_putBytes(B_HashableD_bytes wit, B_bytes a, B_hasher h) {
    zig_hash_wyhash_update(h->_hasher,a);
    return B_None;
}

// Builtin functions involving strings /////////////////////////////////////////////

B_str B_ascii(B_value v) {
    B_str s  = v->$class->__str__(v);
    struct byte_counts bs = byte_count(s->str, s->nbytes);
    //    printf("%d %d %d %d %d %d\n",bs.escaped,bs.squotes,bs.dquotes,bs.printable,bs.non_printable,bs.non_ascii);
    int newbytes = 2+bs.escaped+3*bs.non_printable+(bs.squotes>0 && bs.dquotes>0 ? bs.squotes : 0)+3*bs.non_ascii;
    B_str res;
    NEW_UNFILLED_STR(res,s->nchars+newbytes,s->nbytes+newbytes);
    escape_str(res->str+1,s->str,res->nbytes-1,s->nbytes,255,bs.squotes>0 && bs.dquotes>0);
    if (bs.dquotes==0 && bs.squotes>0) {
        res->str[0] = '"';
        res->str[res->nbytes-1] = '"';
    } else {
        res->str[0] = '\'';
        res->str[res->nbytes-1] = '\'';
    }        
    return res;
}
 
B_str B_bin(B_Integral wit, $WORD n) {
    long v = from$int(wit->$class->__int__(wit,n));
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

B_str B_chr(B_Integral wit, $WORD n) {
    long v = from$int(wit->$class->__int__(wit,n));
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

B_str B_hex(B_Integral wit, $WORD n) {
    unsigned char *hexdigits = (unsigned char *)"0123456789abcdef";
    long v =  from$int(wit->$class->__int__(wit,n));
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

B_int B_ord(B_str c) {
    if(c->nchars != 1)
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("ord: argument is not a single Unicode char")));
    int cp;
    int cpnbytes = utf8proc_iterate(c->str,-1,&cp);
    if (cpnbytes < 0)
        $RAISE((B_BaseException)$NEW(B_ValueError,to$str("ord: argument is not a single Unicode char")));
    return to$int(cp);
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
    return $FORMAT("<%s object at %p>", self->$class->$GCINFO, self);
}

 

// Static witnesses


/*
struct B_OrdD_strG_class  B_OrdD_strG_methods = {
    "B_OrdD_str",
    UNASSIGNED,
    ($SuperG_class)&B_OrdG_methods,
    (B_NoneType (*)(B_OrdD_str))$default__init__,
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
    (B_NoneType (*)(B_ContainerD_str))$default__init__,
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
    (B_NoneType (*)(B_SliceableD_str))$default__init__,
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
    (B_NoneType (*)(B_TimesD_str))$default__init__,
    B_TimesD_strD___serialize__,
    B_TimesD_strD___deserialize__,
    (B_bool (*)(B_TimesD_str))$default__bool__,
    (B_str (*)(B_TimesD_str))$default__str__,
    (B_str (*)(B_TimesD_str))$default__str__,
    B_TimesD_strD___add__,
    (B_str (*)(B_TimesD_str, B_str, B_str))B_PlusD___iadd__,
    B_TimesD_strD___mul__,
    (B_str (*)(B_TimesD_str, B_str, B_int))B_TimesD___imul__,

};

struct B_TimesD_str B_TimesD_str_instance = {&B_TimesD_strG_methods};
B_TimesD_str B_TimesD_strG_witness = &B_TimesD_str_instance;

struct B_HashableD_strG_class  B_HashableD_strG_methods = {
    "B_HashableD_str",
    UNASSIGNED,
    ($SuperG_class)&B_HashableG_methods,
    (B_NoneType (*)(B_HashableD_str))$default__init__,
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

struct B_SequenceD_bytearray  B_SequenceD_bytearray_instance;
struct B_CollectionD_SequenceD_bytearray B_CollectionD_SequenceD_bytearray_instance;
struct B_TimesD_SequenceD_bytearray B_TimesD_SequenceD_bytearray_instance;


struct B_OrdD_bytearrayG_class  B_OrdD_bytearrayG_methods = {
    "B_OrdD_bytearray",
    UNASSIGNED,
    ($SuperG_class)&B_OrdG_methods,
    (B_NoneType (*)(B_OrdD_bytearray))$default__init__,
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
    (B_NoneType (*)(B_SequenceD_bytearray))$default__init__,
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
    (B_Eq)&B_OrdD_intG_methods,
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
    (B_bytearray (*)(B_TimesD_SequenceD_bytearray, B_bytearray, B_bytearray))B_PlusD___iadd__,
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
    B_ContainerD_bytearrayD___fromiter__,
    B_ContainerD_bytearrayD___len__,
    B_ContainerD_bytearrayD___contains__,
    B_ContainerD_bytearrayD___containsnot__
};

struct B_ContainerD_bytearray B_ContainerD_bytearray_instance = {&B_ContainerD_bytearrayG_methods};
B_ContainerD_bytearray B_ContainerD_bytearrayG_witness = &B_ContainerD_bytearray_instance;



struct B_OrdD_bytesG_class  B_OrdD_bytesG_methods = {
    "B_OrdD_bytes",
    UNASSIGNED,
    ($SuperG_class)&B_OrdG_methods,
    (B_NoneType (*)(B_OrdD_bytes))$default__init__,
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
    (B_NoneType (*)(B_SliceableD_bytes))$default__init__,
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
    (B_NoneType (*)(B_TimesD_bytes))$default__init__,
    B_TimesD_bytesD___serialize__,
    B_TimesD_bytesD___deserialize__,
    (B_bool (*)(B_TimesD_bytes))$default__bool__,
    (B_str (*)(B_TimesD_bytes))$default__str__,
    (B_str (*)(B_TimesD_bytes))$default__str__,
    B_TimesD_bytesD___add__,
    (B_bytes (*)(B_TimesD_bytes, B_bytes, B_bytes))B_PlusD___iadd__,
    B_TimesD_bytesD___mul__,
    (B_bytes (*)(B_TimesD_bytes, B_bytes, B_int))B_TimesD___imul__,

};

struct B_TimesD_bytes B_TimesD_bytes_instance = {&B_TimesD_bytesG_methods};
B_TimesD_bytes B_TimesD_bytesG_witness = &B_TimesD_bytes_instance;

struct B_HashableD_bytesG_class  B_HashableD_bytesG_methods = {
    "B_HashableD_bytes",
    UNASSIGNED,
    ($SuperG_class)&B_HashableG_methods,
    (B_NoneType (*)(B_HashableD_bytes))$default__init__,
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

*/
