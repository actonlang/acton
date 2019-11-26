#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <limits.h>
#include "str.h"
#include "utf8proc.h"
#include "acterror.h"
#include "hash.h"

struct str_struct {
  int nbytes;              // length of str in bytes
  int nchars;              // length of str in Unicode chars
  unsigned char *str;      // str is UTF-8 encoded.
};

struct bytes_struct {
  int nbytes;              // length of str in bytes
  unsigned char *str;      // str is UTF-8 encoded.
};

static unsigned char nul = 0;

static struct str_struct null_struct = {0,0,&nul};

static str_t null_str = &null_struct;

#define NEW_STR(nm,nchrs,nbtes)         \
        nm = malloc(sizeof(struct str_struct)); \
        (nm)->nchars = nchrs;            \
        (nm)->nbytes = nbtes;            \
        (nm)->str = malloc((nm)->nbytes + 1);    \
        (nm)->str[(nm)->nbytes] = 0
 

// This constructor
// - checks that the argument is a null-terminated, correctly UTF-8 encoded string,
// - computes and stores both # bytes (excluding the terminating 0) and # Unicode characters.
str_t fromUTF8(char *str) {
  int nbytes = 0;
  int nchars = 0;

  unsigned char *p = (unsigned char*)str;
  int cp, cpnbytes;
  while(1) {
    if (*p == '\0') {
      str_t res = malloc(sizeof(struct str_struct));
      res->nbytes = nbytes;
      res->nchars = nchars;
      res->str = (unsigned char*)str;
      return res;
    }
    cpnbytes = utf8proc_iterate(p,-1,&cp);
    if (cpnbytes < 0)
      return NULL; //UnicodeDecodeError

    nbytes += cpnbytes;
    nchars++;
    p += cpnbytes;

  }
}

unsigned char *toUTF8(str_t str) {
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
static int str_transform(str_t s, transform f, str_t *res) {
  int cp, cpu, cplen, cpulen;
  int ulen = 1;
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
  NEW_STR(*res,s->nchars,nbytes);
  memcpy((*res)->str,buffer,nbytes);
  return 0;
}

// Find char position in text from byte position.
// Assume that i is first byte of a char in text.
static int char_no(str_t text,int i) {
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
static int byte_no(str_t text, int i) {
  int res = 0;
  unsigned char *t = text->str;
  for (int k=0; k<i; k++)
    res += byte_length2(t[k]);
  return res;
}

// Handles negative indices in getitem etc (slice notation) 
static int get_index(int i, int nchars) {
  if (i >= 0)
    return i<nchars ? i : INDEXERROR;
  else
    return i >= -nchars ? nchars+i : INDEXERROR;
}

/* Normalize slice notation, so that
- if step == 0, VALUEERROR is returned

- Otherwise, 
   - on input, nchars must be the # of elements in the sequence being sliced-
   - on output 
       - 0 <= start < nchars is the starting position
       - 0 <= stop < nchars is the ending position (*non-inclusive*!)
       - step is the step size
       - nchars is the # of elements in *the slice*. 
*/
static int normalize_slice(slice_t slc, int *nchars, int *start, int *stop, int *step) {
  *step = slc->step;
  if (slc->has & HAS_STEP) {
    if (*step == 0)
      return VALUEERROR;
  } else
    *step = 1;
  *start = slc->start;
  *start = slc->has & HAS_START ? (*start >=0 ? (*start < *nchars  ? *start : *nchars-1 + (*step > 0))
                                              : (*start > -*nchars ? *nchars+*start : 0))
                                : (*step > 0 ? 0 : *nchars-1);
  *stop = slc->stop;
  *stop = slc->has & HAS_STOP ? (*stop >= 0 ? (*stop < *nchars  ? *stop : *nchars-1 + (*step > 0)) 
                                            : (*stop > -*nchars ? *nchars+*stop : 0))
                              : (*step > 0 ? *nchars : -1);
  if ((*step > 0 && *start >= *stop) || (*step < 0 && *start <= *stop))
    *nchars = 0;
  else
    *nchars = (*stop-*start)/ *step + ((*stop-*start)%*step != 0);
  return 0;
}

// Eliminates slice notation in find, index, count and other methods
// with optional start and end. Assumes that if no start value was given,
// *start = 0 and that if no end value was given, *end >= s->nchars.
static int fix_start_end(str_t s, int *start, int *end) {
  if (*start > s->nchars)
    return -1;
  if (*start < 0) {    
    *start += s->nchars;   
    if (*start < 0)  
      *start = 0;  
  }
  if (*end > s->nchars)   
   *end = s->nchars;      
  else if (*end < 0) { 
    *end += s->nchars;     
    if (*end < 0)    
      *end = 0;    
  }
  return 0;
}

// Builds a new one-char string starting at p.
static str_t mk_char(unsigned char *p) {
  str_t r;
  NEW_STR(r,1,byte_length2(*p));
  for (int i=0; i<r->nbytes; i++)
    r->str[i] = p[i];
  return r;
}

static int isspace_codepoint(int codepoint) {
    int cat = utf8proc_get_property(codepoint)->category;
    int bidi = utf8proc_get_property(codepoint)->bidi_class;
    return (cat == UTF8PROC_CATEGORY_ZS || (bidi >= UTF8PROC_BIDI_CLASS_B && bidi <= UTF8PROC_BIDI_CLASS_WS));
}

static int islinebreak_codepoint(int codepoint) {
  return codepoint == 0x0a;
}

// The Boyer-Moore-Horspool algorithm for searching for pattern in text.
// For very short patterns, this should be replaced by brute force.
// Returns byte position in text where first occurrence of pattern starts,
// or -1 if it does not occur.
static int bmh( unsigned char *text, unsigned char *pattern, int tbytes, int pbytes) {
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

/* ******** str methods ******************* */

str_t str_add(str_t s, str_t t) {
  str_t res;
  NEW_STR(res,s->nchars + t->nchars,s->nbytes + t->nbytes);
  memcpy(res->str,s->str,s->nbytes);
  memcpy(res->str+s->nbytes,t->str,t->nbytes);
  return res;
}

long str_len(str_t s) {
  return (long)s->nchars;
}

int str_contains(str_t s, str_t sub) {
  return bmh(s->str,sub->str,s->nbytes,sub->nbytes) > 0;
}

typedef struct str_iterator_struct {
  unsigned char *nxt;
  int remaining;
} *str_iterator_state_t; 

int str_next(iterator_t iter,WORD *res) {
  str_iterator_state_t state = iter->state;
  if (state->remaining==0)
    return STOPITERATION;
  else {
    *res = (WORD)mk_char(state->nxt);
    state->nxt +=byte_length2(*state->nxt);
    state->remaining--;
    return 0;
  }
}

int str_reversed_next(iterator_t iter,WORD *res) {
  str_iterator_state_t state = iter->state;
  if (state->remaining==0)
    return STOPITERATION;
  else {
    *res = (WORD)mk_char(state->nxt);
    state->nxt = skip_chars(state->nxt,-1,0);
    state->remaining--;
    return 0;
  }
}


iterator_t str_iter(str_t s) {
  str_iterator_state_t state = malloc(sizeof(struct str_iterator_struct));
  iterator_t it = malloc(sizeof(struct iterator_struct));
  state->nxt = s->str;
  state->remaining = s->nchars;
  it->state = (WORD)state;
  it->next = str_next;
  return it;
}

iterator_t str_reversed(str_t s) {
  str_iterator_state_t state = malloc(sizeof(struct str_iterator_struct));
  iterator_t it = malloc(sizeof(struct iterator_struct));
  state->remaining = s->nchars;
  if (state->remaining > 0)
    state->nxt = skip_chars(s->str+s->nbytes,-1,0);
  it->state = (WORD)state;
  it->next = str_reversed_next;
  return it;
}

int str_getitem(str_t s, int i, str_t *res) {
  unsigned char *p = s->str;
  int ix = get_index(i,s->nchars);
  if (ix < 0) return ix;
  p = skip_chars(p,ix,s->nchars == s->nbytes);
  *res = mk_char(p);
  return 0;
}

int str_getslice(str_t s, slice_t slc, str_t *res) {
  int isascii = s->nchars == s->nbytes;
  int nchars = s->nchars;
  int nbytes = 0;
  int start, stop, step;
  int r = normalize_slice(slc, &nchars, &start, &stop, &step);
  if (r<0) return r;
 //slice notation have been eliminated and default values applied.
  unsigned char buffer[4*nchars];       // very conservative buffer size.
  unsigned char *p = buffer;
  unsigned char *t = skip_chars(s->str,start,isascii);
  for (int i=0; i<nchars; i++) {
    int bytes = byte_length2(*t);
    for (int k=0; k<bytes;k++) {
      p[nbytes] = *t;
      t++; nbytes++;
    }
    t = skip_chars(t,step-1,isascii);
  }
  NEW_STR(*res,nchars,nbytes);
  if (nbytes > 0)
    memcpy((*res)->str,buffer,nbytes);
  return 0;
}

void str_capitalize(str_t s, str_t *res) {
  if (s->nchars==0) {
    *res = null_str;
    return;
  }
  unsigned char *p = s->str;
  int cp;
  int cplen = utf8proc_iterate(p,-1,&cp);
  int cpt = utf8proc_totitle(cp);
  int nbytes = s->nbytes - cplen + byte_length(cpt);
  NEW_STR(*res,s->nchars,nbytes);
  int cpulen = utf8proc_encode_char(cpt,(*res)->str);
  memcpy((*res)->str+cpulen,s->str+cplen,s->nbytes-cplen);
}

int str_center(str_t s, int width, str_t fill, str_t *res) {
  if (fill->nchars != 1)
    return TYPEERROR;
  if (width <= s->nchars) {
    *res = s;
    return 0;
  }
  int pad = (width-s->nchars);
  int padleft = pad/2; // Below we make use of the fact padright >= padleft.
  int padright = pad-padleft;
  NEW_STR(*res, width,s->nbytes+pad*fill->nbytes);
  unsigned char *c = fill->str;
  unsigned char *p = (*res)->str;
  p += padleft*fill->nbytes+s->nbytes;
  for (int i = 0; i<padright; i++) {
    for (int j = 0; j < fill->nbytes; j++) 
      p[j] = c[j];
    p += fill->nbytes;
  }
  p -= padright*fill->nbytes;
  memcpy((*res)->str,p,padleft*fill->nbytes);
  p -= s->nbytes;
  memcpy(p,s->str,s->nbytes);
  return 0;
}

int str_count(str_t s, str_t sub, int start, int end) {
  int isascii = s->nchars == s->nbytes;
  if (fix_start_end(s,&start,&end) < 0) return 0;
  unsigned char *p = skip_chars(s->str,start,isascii);
  unsigned char *q = skip_chars(p,end-start,isascii);
  int res = 0;
  int n = bmh(p,sub->str,q-p,sub->nbytes);
  while (n>=0) {
    res++;
    p += n + (sub->nbytes>0 ? sub->nbytes : 1);
    n = bmh(p,sub->str,q-p,sub->nbytes);
  }
  return res;
}

int str_endswith(str_t s, str_t sub, int start, int end) {
  if (fix_start_end(s,&start,&end) < 0) return 0;
  int isascii = s->nchars==s->nbytes;
  unsigned char *p = skip_chars(s->str + s->nbytes,end - s->nchars,isascii) - sub->nbytes;
  unsigned char *q = sub->str;
  for (int i=0; i<sub->nbytes; i++) {
    if (*p == 0 || *p++ != *q++) {
      return 0;
    }
  }
   return 1;
}

void str_expandtabs(str_t s, int tabsize, str_t *res){
  int pos = 0;
  int expanded = 0;
  tabsize = tabsize <= 0 ? 1 : tabsize;
  unsigned char buffer[tabsize * s->nchars];
  unsigned char *p = s->str;
  unsigned char *q = buffer;
  for (int i=0; i<s->nchars; i++) {
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
  NEW_STR(*res,s->nchars+expanded,s->nbytes+expanded);
  memcpy((*res)->str,buffer,s->nbytes+expanded);
}

int str_find(str_t s, str_t sub, int start, int end) {
  int isascii = s->nchars == s->nbytes;
  if (fix_start_end(s,&start,&end) < 0) return -1;
  unsigned char *p = skip_chars(s->str,start,isascii);
  unsigned char *q = skip_chars(p,end-start,isascii);
  int n = bmh(p,sub->str,q-p,sub->nbytes);
  if (n<0) return -1;
  return char_no(s,n+p-s->str);
}

int str_index(str_t s, str_t sub, int start, int end) {
  int n = str_find(s,sub,start,end);
  if (n<0)
    return VALUEERROR;
  else
    return n;
}

int str_isalnum(str_t s) {
  unsigned char *p = s->str;
  int codepoint;
  int nbytes;
  if (s->nchars == 0)
    return 0;
  for (int i=0; i < s->nchars; i++) {
    nbytes = utf8proc_iterate(p,-1,&codepoint);
    utf8proc_category_t cat = utf8proc_category(codepoint);
    if ((cat <  UTF8PROC_CATEGORY_LU || cat >  UTF8PROC_CATEGORY_LO) && cat != UTF8PROC_CATEGORY_ND)
      return 0;
    p += nbytes;
  }
  return 1;
}

int str_isalpha(str_t s) {
  unsigned char *p = s->str;
  int codepoint;
  int nbytes;
  if (s->nchars == 0)
    return 0;
  for (int i=0; i < s->nchars; i++) {
    nbytes = utf8proc_iterate(p,-1,&codepoint);
    utf8proc_category_t cat = utf8proc_category(codepoint);
    if (cat <  UTF8PROC_CATEGORY_LU || cat >  UTF8PROC_CATEGORY_LO)
      return 0;
    p += nbytes;
  }
  return 1;
}

int str_isascii(str_t s) {
  unsigned char *p = s->str;
  for (int i=0; i < s->nbytes; i++) {
    if (*p > 127)
      return 0;
    p++;
  }
  return 1;
}

int str_isdecimal(str_t s) {
  unsigned char *p = s->str;
  int codepoint;
  int nbytes;
  if (s->nchars == 0)
    return 0;
  for (int i=0; i < s->nchars; i++) {
    nbytes = utf8proc_iterate(p,-1,&codepoint);
    utf8proc_category_t cat = utf8proc_category(codepoint);
    if (cat != UTF8PROC_CATEGORY_ND)
      return 0;
    p += nbytes;
  }
  return 1;
}

int str_islower(str_t s) {
  unsigned char *p = s->str;
  int codepoint;
  int nbytes;
  int has_cased = 0;
  if (s->nchars == 0)
    return 0;
  for (int i=0; i < s->nchars; i++) {
    nbytes = utf8proc_iterate(p,-1,&codepoint);
    utf8proc_category_t cat = utf8proc_category(codepoint);
    if (cat == UTF8PROC_CATEGORY_LT|| cat == UTF8PROC_CATEGORY_LU)
      return 0;
    if (cat == UTF8PROC_CATEGORY_LL)
      has_cased = 1;
    p += nbytes;
  }
  return has_cased;
}

int str_isprintable(str_t s) {
  unsigned char *p = s->str;
  int codepoint;
  int nbytes;
  for (int i=0; i < s->nchars; i++) {
    nbytes = utf8proc_iterate(p,-1,&codepoint);
    utf8proc_category_t cat = utf8proc_category(codepoint);
    if (cat >= UTF8PROC_CATEGORY_ZS && codepoint != 0x20)
      return 0;
    p += nbytes;
  }
  return 1;
}

int str_isspace(str_t s) {
  unsigned char *p = s->str;
  int codepoint;
  int nbytes;
  if (s->nchars == 0)
    return 0;
  for (int i=0; i < s->nchars; i++) {
    nbytes = utf8proc_iterate(p,-1,&codepoint);
    if (!isspace_codepoint(codepoint))
      return 0;
    p += nbytes;
  }
  return 1;
}

int str_istitle(str_t s) {
  unsigned char *p = s->str;
  int codepoint;
  int nbytes;
  int hascased = 0;
  int incasedrun = 0;
  if (s->nchars == 0)
    return 0;
  for (int i=0; i < s->nchars; i++) {
    nbytes = utf8proc_iterate(p,-1,&codepoint);
    utf8proc_category_t cat = utf8proc_category(codepoint);
    if (cat == UTF8PROC_CATEGORY_LU || cat == UTF8PROC_CATEGORY_LT ) {
      hascased = 1;
      if (incasedrun)
        return 0;
      incasedrun = 1;
    } else if (cat == UTF8PROC_CATEGORY_LL) {
      hascased = 1;
      if (!incasedrun)
        return 0;
    } else
        incasedrun = 0;
    p += nbytes;
  }
  return hascased;
}

int str_isupper(str_t s) {
  unsigned char *p = s->str;
  int codepoint;
  int nbytes;
  int hascased = 0;
  if (s->nchars == 0)
    return 0;
  for (int i=0; i < s->nchars; i++) {
    nbytes = utf8proc_iterate(p,-1,&codepoint);
    utf8proc_category_t cat = utf8proc_category(codepoint);
    if (cat == UTF8PROC_CATEGORY_LL)
      return 0;
    if (cat == UTF8PROC_CATEGORY_LU || cat == UTF8PROC_CATEGORY_LT)
      hascased = 1;
    p += nbytes;
  }
  return hascased;
}

//creates many intermediate strings...
str_t str_join(str_t s, iterator_t iter) {
  str_t res;
  WORD nxt;
  if(!iterator_next(iter,&nxt))
    res = (str_t)nxt;
  else
    return null_str;
  while(!iterator_next(iter,&nxt))
    res = str_add(str_add(res,s),(str_t)nxt);
  return res;
}

int str_ljust(str_t s, int width, str_t fill, str_t *res) {
  if (fill->nchars != 1)
    return TYPEERROR;
  if (width <= s->nchars) {
    *res = s;
    return 0;
  }
  int pad = (width-s->nchars);
  NEW_STR(*res,width, s->nbytes+pad*fill->nbytes);
  unsigned char *c = fill->str;
  unsigned char *p = (*res)->str + s->nbytes;
  for (int i = 0; i<pad; i++) {
    for (int j = 0; j < fill->nbytes; j++) 
      *p++ = c[j];
  }
  memcpy((*res)->str,s->str,s->nbytes);
  return 0;
}

int str_lower(str_t s, str_t *res) {
  return str_transform(s,utf8proc_tolower,res);
}

void str_lstrip(str_t s, str_t cs, str_t *res) {
  unsigned char *p = s->str;
  int i, nbytes;
  for (i=0; i<s->nchars; i++) {
    str_t c = mk_char(p);
    if (cs == NULL ?  !str_isspace(c) :
      bmh(cs->str,p,cs->nbytes,byte_length2(*p)) < 0) 
      break;
    p += byte_length2(*p);
  }
  nbytes = s->nbytes + s->str - p;
  NEW_STR(*res,s->nchars-i,nbytes);
  memcpy((*res)->str,p,nbytes);
}

void str_partition(str_t s, str_t sep, str_t *ls, str_t *ssep, str_t *rs) {
  int n = str_find(s,sep,0,s->nchars);
  if (n<0) {
    *ls = s; *ssep = null_str; *rs = null_str;
  } else {
    int nb = bmh(s->str,sep->str,s->nbytes,sep->nbytes);
    NEW_STR(*ls,n,nb);
    memcpy((*ls)->str,s->str,nb);
    int nbr = s->nbytes - sep->nbytes - nb;
    NEW_STR(*rs,s->nchars-n-sep->nchars,nbr);
    memcpy((*rs)->str,s->str+nb+sep->nbytes,nbr);
    *ssep = sep;
  }
}

void str_replace(str_t s, str_t old, str_t new, int count, str_t *res) {
  int c = str_count(s,old,0,INT_MAX);
  c = count < c ? count : c;
  if (c==0){
    *res = s;
    return;
  }
  int nbytes = s->nbytes + c*(new->nbytes-old->nbytes);
  int nchars = s->nchars+c*(new->nchars-old->nchars);
  NEW_STR(*res,nchars,nbytes);
  unsigned char *p = s->str;
  unsigned char *q = (*res)->str;
  unsigned char *pold = old->str;
  unsigned char *pnew = new->str;
  int plen = s->nbytes;
  int n;
  for (int i=0; i<c; i++) {
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
}
      

int str_rfind(str_t s, str_t sub, int start, int end) {
  int isascii = s->nchars == s->nbytes;
  if (fix_start_end(s,&start,&end) < 0) return -1;
  unsigned char *p = skip_chars(s->str,start,isascii);
  unsigned char *q = skip_chars(p,end-start,isascii);
  int n = rbmh(p,sub->str,q-p,sub->nbytes);
  if (n<0) return -1;
  return char_no(s,n+p-s->str);
}


int str_rindex(str_t s, str_t sub, int start, int end) {
  int n = str_rfind(s,sub,start,end);
  if (n<0)
    return VALUEERROR;
  else
    return n;
}

int str_rjust(str_t s, int width, str_t fill, str_t *res) {
  if (fill->nchars != 1)
    return TYPEERROR;
  if (width <= s->nchars) {
    *res = s;
    return 0;
  }
  int pad = (width-s->nchars);
  NEW_STR(*res,width,s->nbytes+pad*fill->nbytes);
  unsigned char *c = fill->str;
  unsigned char *p = (*res)->str;
  for (int i = 0; i<pad; i++) {
    for (int j = 0; j < fill->nbytes; j++) 
      *p++ = c[j];
  }
  memcpy(p,s->str,s->nbytes);
  return 0;
}
                                 
void str_rpartition(str_t s, str_t sep, str_t *ls, str_t *ssep, str_t *rs) {
  int n = str_rfind(s,sep,0,s->nchars);
  if (n<0) {
    *ls = null_str; *ssep = null_str; *rs = s;
  } else {
    int nb = rbmh(s->str,sep->str,s->nbytes,sep->nbytes);
    NEW_STR(*ls,n,nb);
    memcpy((*ls)->str,s->str,nb);
    int nbr = s->nbytes - sep->nbytes - nb;
    NEW_STR(*rs,s->nchars-n-sep->nchars,nbr);
    memcpy((*rs)->str,s->str+nb+sep->nbytes,nbr);
    *ssep = sep;
  }
}

int str_split(str_t s, str_t sep, int maxsplit, list_t *res) {
  *res = list_new(5);
  if (maxsplit < 0) maxsplit = INT_MAX; 
  int remaining = s->nchars;
  if (sep == NULL) {
    unsigned char *p = s->str;
    int nbytes, codepoint, wordlength;
    if (remaining==0) {
      return 0;
    }
    int inword = 0;
    unsigned char *q;
    while (remaining > 0) {
      nbytes = utf8proc_iterate(p,-1,&codepoint);
      if (!isspace_codepoint(codepoint)) {
        if (!inword) {
          inword = 1;
          q = p;
          wordlength = 1;
          if (list_len(*res) == maxsplit)
            break; // we have now removed leading whitespace in remainder
        } else
          wordlength++;
      } else {
          if (inword) {
            inword = 0;
            str_t word;
            NEW_STR(word,wordlength,p-q);
            memcpy(word->str,q,p-q);
            list_append(*res,word);
          }
      }
      remaining--;
      p += nbytes;
    }
    // this if statement should be simplified; almost code duplication.
    if (remaining == 0) {
      if (inword) {
        str_t word;
        NEW_STR(word,wordlength,p-q);
        memcpy(word->str,q,p-q);
        list_append(*res,word);
      }
    } else {
      str_t word;
      p = s->str+s->nbytes;
      NEW_STR(word,remaining,p-q);
      memcpy(word->str,q,p-q);
      list_append(*res,word);
    }
    WORD w;
    list_getitem(*res,0,&w);
    return 0;
  } else { // separator given
    if (sep->nchars==0)
      return VALUEERROR;
    if (remaining==0) { // for some unfathomable reason, this is the behaviour of the Python method
      list_append(*res,null_str);
      return 0;
    }
    str_t ls, rs, ssep;
    rs = s;
    // Note: This builds many intermediate rs strings...
    while (rs->nchars>0 && list_len(*res) < maxsplit) {
     str_partition(rs,sep,&ls,&ssep,&rs);
     list_append(*res,ls);
    }
    if (rs->nchars>0)
      list_append(*res,rs);
    return 0;
  }
}

int str_splitlines(str_t s, list_t *res) {
  *res = list_new(5);
  int remaining = s->nchars;
  unsigned char *p = s->str;
  int nbytes, codepoint, wordlength;
  if (remaining==0) {
    return 0;
  }
  int inword = 0;
  unsigned char *q;
  while (remaining > 0) {
    nbytes = utf8proc_iterate(p,-1,&codepoint);
    utf8proc_category_t cat = utf8proc_category(codepoint);
    if (!islinebreak_codepoint(codepoint)) {
      if (!inword) {
        inword = 1;
        q = p;
        wordlength = 1;
      } else
        wordlength++;
    } else {
      if (inword) {
        inword = 0;
        str_t word;
        NEW_STR(word,wordlength,p-q);
        memcpy(word->str,q,p-q);
        list_append(*res,word);
      }
    }
    remaining--;
    p += nbytes;
  }
  if (inword) {
    str_t word;
    NEW_STR(word,wordlength,p-q);
    memcpy(word->str,q,p-q);
    list_append(*res,word);
  }
  return 0;
} 


void str_rstrip(str_t s, str_t cs, str_t *res) {
  unsigned char *p = s->str + s->nbytes;
  int i, nbytes;
  for (i=0; i<s->nchars; i++) {
    p = skip_chars(p,-1,0);
    str_t c = mk_char(p);
    if (cs == NULL ?  !str_isspace(c) :
      rbmh(cs->str,p,cs->nbytes,byte_length2(*p)) < 0) 
      break;
  }
  nbytes = p + byte_length2(*p) - s->str;
  NEW_STR(*res,s->nchars-i,nbytes);
  memcpy((*res)->str,s->str,nbytes);
}

int str_startswith(str_t s, str_t sub, int start, int end) {
  if (fix_start_end(s,&start,&end) < 0) return 0;
  int isascii = s->nchars==s->nbytes;
  unsigned char *p = skip_chars(s->str,start,isascii);
  unsigned char *q = sub->str;
  for (int i=0; i<sub->nbytes; i++) {
    if (*p == 0 || *p++ != *q++) {
      return 0;
    }
  }
  return 1;
}
 
void str_strip(str_t s, str_t cs, str_t *res) {
  str_t r1;
  str_rstrip(s,cs,&r1);
  str_lstrip(r1,cs,res);
}

int str_upper(str_t s, str_t *res) {
  return str_transform(s,utf8proc_toupper,res);
}

str_t str_zfill(str_t s, int width) {
  int fill = width - s->nchars;
  if (fill < 0)
    return s;
  str_t res;
  NEW_STR(res,width,s->nbytes+fill);
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
    
int str_eq(str_t a, str_t b) {
  return !strcmp((char *)a->str,(char *)b->str);
}

size_t str_hash(str_t s) {
  return bytes_hash(s->str,s->nbytes);
}

static int str_eqW(WORD a, WORD b) {
  return !strcmp((char *)((str_t)a)->str,(char *)((str_t)b)->str);
}

static size_t str_hashW(WORD w) {
  return bytes_hash(((str_t)w)->str,((str_t)w)->nbytes);
}

static struct Hashable_struct str_h = {str_eqW, str_hashW};

Hashable str_Hashable = &str_h;
