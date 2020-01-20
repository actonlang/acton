#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <limits.h>
#include "str.h"
#include "utf8proc.h"
#include "hash.h"

//  Method tables ///////////////////////////////////////////////////////////////

// String-specific methods
/*
$bool $str_contains($str s, $str sub);

$WORD $str_getitem($str s, int i);
$str $str_getslice($str s, Slice slc);
*/
$str $str_capitalize($str s);
$str $str_center($str s, int width, $str fill);
$int $str_count($str s, $str sub, $int start, $int end);
//void $str_encode($str s, bytes_t *res);
$bool $str_endswith($str s, $str suffix, $int start, $int end);
$str $str_expandtabs($str s, int tabsize);      
$int $str_find($str s, $str sub, $int start, $int end);
//format and format_map will be replace by other methods
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
$str $str_join($str sep, Iterator iter);
$str $str_ljust($str s, int width, $str fill); 
$str $str_lower($str s);
$str $str_lstrip($str s,$str cs); 
void $str_partition($str s, $str sep, $str *ls, $str *ssep, $str *rs);
$str $str_replace($str s, $str old, $str new, $int count);
$int $str_rfind($str s, $str sub, $int start, $int end);
$int $str_rindex($str s, $str sub, $int start, $int end);
$str $str_rjust($str s, int width, $str fill);  
void $str_rpartition($str s, $str sep, $str *ls, $str *ssep, $str *rs); 
$str $str_rstrip($str s,$str cs);
$list $str_split($str s, $str sep, $int maxsplit);  
$list $str_splitlines($str s); 
$bool $str_startswith($str s, $str prefix, $int start, $int end); 
$str $str_strip($str s,$str cs);
$str $str_upper($str s);
$str $str_zfill($str s, int width);

static struct $str$__methods__ $str_table =
   {$str_capitalize, $str_center, $str_count, $str_endswith, $str_expandtabs, $str_find, $str_index, $str_isalnum, $str_isalpha, $str_isascii,
    $str_isdecimal, $str_islower, $str_isprintable, $str_isspace, $str_istitle, $str_isupper, $str_join, $str_ljust, $str_lower, $str_lstrip,
    $str_partition, $str_replace, $str_rfind, $str_rindex, $str_rjust, $str_rpartition, $str_rstrip, $str_split, $str_splitlines, $str_startswith,
    $str_strip, $str_upper, $str_zfill};

static $str$__methods__ $str_methods = &$str_table;

// Protocol instances

$bool $str_eq_instance(Eq$__class__ cl,$WORD a, $WORD b); 
$bool $str_neq_instance(Eq$__class__ cl, $WORD a, $WORD b);

$int $str_hash_instance(Eq_Hashable$__class__ cl, $WORD self);

$WORD $str_add_instance(Plus$__class__ cl, $WORD a, $WORD b);

Iterator $str_iter_instance(Iterable$__class__ cl,$WORD self);

$WORD $str_next_instance(Iterator$__class__ cl, $WORD self);

Collection $str_fromiter_instance(Collection$__class__ cl, Iterable it);
$int $str_len_instance(Collection$__class__ cl, $WORD self);

$WORD $str_getitem_instance(Indexed$__class__ cl, $WORD self, $WORD ix); 
void $str_setitem_instance(Indexed$__class__ cl, $WORD self, $WORD ix, $WORD val);
void $str_delitem_instance(Indexed$__class__ cl, $WORD self, $WORD ix);

$WORD $str_getslice_instance(Sliceable$__class__ cl, $WORD self, Slice slice);
void $str_setslice_instance(Sliceable$__class__ cl, $WORD self, Slice slice, Sequence it); 
void $str_delslice_instance(Sliceable$__class__ cl, $WORD self, Slice slice);

Iterable $str_reversed_instance(Sequence$__class__ cl, $WORD self);
Iterator $str_iter_reversed_instance(Iterable$__class__ cl, $WORD self);
$WORD $str_next_reversed_instance(Iterator$__class__ cl, $WORD self);
void $str_insert_instance(Sequence$__class__ cl, $WORD self, $int ix, $WORD elem);
void $str_append_instance(Sequence$__class__ cl, $WORD self, $WORD elem);
void $str_reverse_instance(Sequence$__class__ cl, $WORD self);

$bool $str_contains_instance (Container_Eq$__class__ cl, $WORD self, $WORD elem);
$bool $str_containsnot_instance (Container_Eq$__class__ cl, $WORD self, $WORD elem);

 
static struct Eq$__class__  Eq$str_struct = {"GC_Eq", $str_eq_instance, $str_neq_instance};
Eq$__class__ Eq$str_instance = &Eq$str_struct;

static struct Eq_Hashable$__class__ Eq_Hashable$str_struct = {"GC_Hashable__Eq", $str_eq_instance, $str_neq_instance, $str_hash_instance};
Eq_Hashable$__class__ Eq_Hashable$str_instance = &Eq_Hashable$str_struct;

static struct Plus$__class__ Plus$str_struct = {"GC_Plus",$str_add_instance};
Plus$__class__ Plus$str_instance = &Plus$str_struct;

static struct Iterator$__class__ Iterator$str_struct = {"GC_Iterator",$str_next_instance};
Iterator$__class__ Iterator$str_instance = &Iterator$str_struct;

static struct Iterable$__class__ Iterable$str_struct = {"GC_Iterable", $str_iter_instance};
Iterable$__class__ Iterable$str_instance = &Iterable$str_struct;

static struct Iterator$__class__ Iterator$str_reversed_struct = {"GC_Iterator",$str_next_reversed_instance};
Iterator$__class__ Iterator$str_reversed_instance = &Iterator$str_reversed_struct;

static struct Iterable$__class__ Iterable$str_reversed_struct = {"GC_Iterable", $str_iter_reversed_instance};
Iterable$__class__ Iterable$str_reversed_instance = &Iterable$str_reversed_struct;

static struct Collection$__class__ Collection$str_struct = {"GC_Collection",&Iterable$str_struct,$str_fromiter_instance,$str_len_instance};
Collection$__class__ Collection$str_instance = &Collection$str_struct;

static struct Indexed$__class__ Indexed$str_struct = {"GC_Indexed", $str_getitem_instance, $str_setitem_instance, $str_delitem_instance};
Indexed$__class__ Indexed$str_instance = &Indexed$str_struct;

static struct Sliceable$__class__ Sliceable$str_struct = {"GC_Sliceable", &Indexed$str_struct, $str_getslice_instance, $str_setslice_instance, $str_delslice_instance};
Sliceable$__class__ Sliceable$str_instance = &Sliceable$str_struct;

static struct Sequence$__class__ Sequence$str_struct = {"GC_Sequence",&Sliceable$str_struct, &Collection$str_struct, &Plus$str_struct,
                                                         $str_reversed_instance,$str_insert_instance,$str_append_instance,$str_reverse_instance};
Sequence$__class__ Sequence$str_instance = &Sequence$str_struct;

static struct Container_Eq$__class__ Container_Eq$str_struct = {"GC_Container_Eq",&Collection$str_struct, $str_contains_instance,$str_containsnot_instance,&Eq$str_struct};

Container_Eq$__class__ Container_Eq$str_instance = &Container_Eq$str_struct;


static unsigned char nul = 0;

static struct str_internal_t null_struct = {"GC_NUL",0,0,&nul};

static str_internal_t null_str = &null_struct;


#define NEW_INTERNAL(nm,nchrs,nbtes)         \
nm = malloc(sizeof(struct str_internal_t)); \
(nm)->nchars = nchrs;            \
(nm)->nbytes = nbtes;            \
(nm)->str = malloc((nm)->nbytes + 1);    \
(nm)->str[(nm)->nbytes] = 0

#define NEW_STR(nm,internal)  \
$str nm; \
nm = malloc(sizeof(struct $str)); \
(nm)->__class__ = $str_methods; \
(nm)->__internal__ = internal; \
return nm;
         
$str mk_str(str_internal_t internal) {
  NEW_STR(res,internal)
}
         
$str fromUTF8(char *str) {
  int nbytes = 0;
  int nchars = 0;

  unsigned char *p = (unsigned char*)str;
  int cp, cpnbytes;
  while(1) {
    if (*p == '\0') {
      str_internal_t internal = malloc(sizeof(struct str_internal_t));
      internal->nbytes = nbytes;
      internal->nchars = nchars;
      internal->str = (unsigned char*)str;
      NEW_STR(res,internal);
    }
    cpnbytes = utf8proc_iterate(p,-1,&cp);
    if (cpnbytes < 0)
      return NULL; //UnicodeDecodeError

    nbytes += cpnbytes;
    nchars++;
    p += cpnbytes;

  }
}

unsigned char *toUTF8($str str) {
  return str->__internal__->str;
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
  str_internal_t si = s->__internal__;
  int cp, cpu, cplen, cpulen;
  int ulen = 1;
  unsigned char *p = si->str;
  unsigned char buffer[4*si->nchars];
  unsigned char *up = buffer;
  for (int i=0; i < si->nchars; i++) {
    cplen = utf8proc_iterate(p,-1,&cp);
    cpu = f(cp);
    cpulen = utf8proc_encode_char(cpu,up);
    p+=cplen;
    up += cpulen;
  }
  int nbytes = (int)(up-buffer);
  str_internal_t r;
  NEW_INTERNAL(r,si->nchars,nbytes);
  memcpy(r->str,buffer,nbytes);
  NEW_STR(res,r);
}

// Find char position in text from byte position.
// Assume that i is first byte of a char in text.
static int char_no(str_internal_t text,int i) {
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
static int byte_no(str_internal_t text, int i) {
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
  exception e;
  MKEXCEPTION(e,INDEXERROR);
  RAISE(e);
  return 0;
}

 
// Eliminates slice notation in find, index, count and other methods
// with optional start and end and adds defaults for omitted parameters.

static int fix_start_end(int nchars, long **start, long **end) {
  if (*start==NULL) {
    *start = to$int(0);
  }
  if (**start > nchars)
    return -1;
  if (**start < 0) {    
    **start += nchars;   
    if (**start < 0)  
      **start = 0;  
  }
  if (*end==NULL) {
    *end = to$int(nchars);
  }
  if (**end > nchars)   
   **end = nchars;      
  else if (**end < 0) { 
    **end += nchars;     
    if (**end < 0)    
      **end = 0;    
  }
  return 0;
}

// Builds a new one-char string starting at p.
static $str mk_char(unsigned char *p) {
  str_internal_t r;
  NEW_INTERNAL(r,1,byte_length2(*p));
  for (int i=0; i<r->nbytes; i++)
    r->str[i] = p[i];
  NEW_STR(res,r);
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

// Protocol instances /////////////////////////////////////////////////////////////////////////////
/* 
Note: We make str instances for Collection, Indexed, Sliceable and Sequence even though these protocols 
include mutating methods. These methods raise NOTIMPLEMENTED.
*/

// Eq ///////////////////////////////////////////////////////////////////////////////////////////////
$bool $str_eq(Eq$__class__ cl, $str a, $str b) {
  return !strcmp((char *)a->__internal__->str,(char *)b->__internal__->str);
}
         
$bool $str_neq(Eq$__class__ cl, $str a, $str b) {
  return !$str_eq(cl,a,b);
}

// instance methods

$bool $str_eq_instance(Eq$__class__ cl,$WORD a, $WORD b) {
  return $str_eq(cl,($str)a,($str)b);
}

$bool $str_neq_instance(Eq$__class__ cl, $WORD a, $WORD b) {
  return $str_neq(cl,($str)a,($str)b);
}

// Eq_Hashable ///////////////////////////////////////////////////////////////////////////////////


// hash function $string_hash defined in hash.c

// instance method

$int $str_hash_instance(Eq_Hashable$__class__ cl, $WORD self) {
  $int res = malloc(sizeof(long));
  str_internal_t s = (($str)self)->__internal__;
  *res = $string_hash(s->str,s->nbytes);
  return res;
}

// Plus /////////////////////////////////////////////////////////////////////////////////////////////

$str $str_add($str s, $str t) {
  str_internal_t r, si = s->__internal__, ti = t->__internal__;
  NEW_INTERNAL(r,si->nchars + ti->nchars,si->nbytes + ti->nbytes);
  memcpy(r->str,si->str,si->nbytes);
  memcpy(r->str+si->nbytes,ti->str,ti->nbytes);
  NEW_STR(res,r);
}

// instance method

$WORD $str_add_instance(Plus$__class__ cl, $WORD a, $WORD b) {
  return ($WORD)$str_add(($str)a,($str)b);
}


// Collection ///////////////////////////////////////////////////////////////////////////////////////

$str $str_fromiter(Iterable it) {
  exception e;
  MKEXCEPTION(e,NOTIMPLEMENTED);
  RAISE(e);
  return NULL;
}
         
         
$int $str_len($str s) {
  $int res = to$int(s->__internal__->nchars);
  return res;
}

// instance methods

Collection $str_fromiter_instance(Collection$__class__ cl, Iterable it) {
  return Collection$__pack__(Collection$str_instance,($WORD)$str_fromiter(it));
}

$int $str_len_instance(Collection$__class__ cl, $WORD self) {
  return $str_len(($str)self);
}

// Container ///////////////////////////////////////////////////////////////////////////

$bool $str_contains($str s, $str sub) {
  str_internal_t si = s->__internal__;
  str_internal_t subi = sub->__internal__;
  return bmh(si->str,subi->str,si->nbytes,subi->nbytes) > 0;
}

$bool $str_containsnot($str s, $str sub) {
  return !$str_contains(s,sub);
}

// instance methods

$bool $str_contains_instance (Container_Eq$__class__ cl, $WORD self, $WORD elem) {
  return $str_contains(($str)self,elem);
}

$bool $str_containsnot_instance (Container_Eq$__class__ cl, $WORD self, $WORD elem) {
  return $str_containsnot(($str)self,elem);
}

// Iterable ///////////////////////////////////////////////////////////////////////////

typedef struct str_iterator_struct {
  char *$GCINFO;
  unsigned char *nxt;
  int remaining;
} *str_iterator_state_t; 

static str_iterator_state_t $str_state_of($str s) {
  str_iterator_state_t state = malloc(sizeof(struct str_iterator_struct));
  state->$GCINFO = "iterator_state";
  state->nxt = s->__internal__->str;
  state->remaining = s->__internal__->nchars;
  return state;
}

// this is next function for forward iteration
static $WORD $str_iterator_next(str_iterator_state_t state) {
  $WORD res;
  if (state->remaining==0) {
    exception e;
    MKEXCEPTION(e,STOPITERATION);
    RAISE(e);
  } else {
    res = ($WORD)mk_char(state->nxt);
    state->nxt +=byte_length2(*state->nxt);
    state->remaining--;
  }
  return res;
}
 
// instance methods

Iterator $str_iter_instance(Iterable$__class__ cl, $WORD self) {
  $str s = ($str)self;
  return Iterator$__pack__(Iterator$str_instance,$str_state_of(s));
}

$WORD $str_next_instance(Iterator$__class__ cl, $WORD self) {
  return  $str_iterator_next(self);
}

// Indexed ///////////////////////////////////////////////////////////////////////////

$WORD $str_getitem($str s, int i) {
  str_internal_t si = s->__internal__;
  unsigned char *p = si->str;
  int ix = get_index(i,si->nchars);
  p = skip_chars(p,ix,si->nchars == si->nbytes);
  return mk_char(p);
}

void $str_setitem($str s, int ix, $WORD val) {
    exception e;
    MKEXCEPTION(e,NOTIMPLEMENTED);
    RAISE(e);
}

void $str_delitem($str s,int ix) {
    exception e;
    MKEXCEPTION(e,NOTIMPLEMENTED);
    RAISE(e);
}

// instance methods

$WORD $str_getitem_instance(Indexed$__class__ cl, $WORD self, $WORD ix) {
  $WORD w = $str_getitem(($str)self,*(long*)ix);
  return w;
}

void $str_setitem_instance(Indexed$__class__ cl, $WORD self, $WORD ix, $WORD val){
  $str_setitem(($str)self,*(int*)ix,val);
}

void $str_delitem_instance(Indexed$__class__ cl, $WORD self, $WORD ix) {
  $str_delitem(($str)self,*(int*)ix);
}

// Sliceable //////////////////////////////////////////////////////////////////////////////////////

$str $str_getslice($str s, Slice slc) {
  str_internal_t si = s->__internal__;
  int isascii = si->nchars == si->nbytes;
  int nchars = si->nchars;
  int nbytes = 0;
  int start, stop, step, slen;
  normalize_slice(slc, nchars, &slen, &start, &stop, &step);
 //slice notation have been eliminated and default values applied.
  unsigned char buffer[4*slen];       // very conservative buffer size.
  unsigned char *p = buffer;
  unsigned char *t = skip_chars(si->str,start,isascii);
  for (int i=0; i<slen; i++) {
    int bytes = byte_length2(*t);
    for (int k=0; k<bytes;k++) {
      p[nbytes] = *t;
      t++; nbytes++;
    }
    t = skip_chars(t,step-1,isascii);
  }
  str_internal_t r;
  NEW_INTERNAL(r,slen,nbytes);
  if (nbytes > 0)
    memcpy(r->str,buffer,nbytes);
  NEW_STR(res,r);
}


// instance methods

$WORD $str_getslice_instance(Sliceable$__class__ cl, $WORD self, Slice slice) {
  return $str_getslice(($str)self,slice);
}
  
void $str_setslice_instance(Sliceable$__class__ cl, $WORD self, Slice slice, Sequence it) {
    exception e;
    MKEXCEPTION(e,NOTIMPLEMENTED);
    RAISE(e);

}
  
void $str_delslice_instance(Sliceable$__class__ cl, $WORD self, Slice slice) {
    exception e;
    MKEXCEPTION(e,NOTIMPLEMENTED);
    RAISE(e);
}

// Sequence /////////////////////////////////////////////////////////////////////////////

// for reversed iteration, rather than making a reversed copy of the string, we iterate from the end towards the start

static $WORD $str_reversed_next(str_iterator_state_t state) {
  $WORD res;
  if (state->remaining==0) {
    exception e;
    MKEXCEPTION(e,STOPITERATION);
    RAISE(e);
  } else {
    res = ($WORD)mk_char(state->nxt);
    state->nxt = skip_chars(state->nxt,-1,0);
    state->remaining--;
  }
  return res;
}
 
void $str_append($str s, $WORD val) {
    exception e;
    MKEXCEPTION(e,NOTIMPLEMENTED);
    RAISE(e);
}


void $str_insert($str s, int ix, $WORD val) {
    exception e;
    MKEXCEPTION(e,NOTIMPLEMENTED);
    RAISE(e);
}

void $str_reverse($str s) {
    exception e;
    MKEXCEPTION(e,NOTIMPLEMENTED);
    RAISE(e);
}

  // instance methods

Iterator $str_iter_reversed_instance(Iterable$__class__ cl, $WORD self) {
  $str s = ($str)self;
  return Iterator$__pack__(Iterator$str_reversed_instance,$str_state_of(s));
}

$WORD $str_next_reversed_instance(Iterator$__class__ cl, $WORD self) {
  return  $str_reversed_next(self);
}

Iterable $str_reversed_instance(Sequence$__class__ cl, $WORD self) {
  return Iterable$__pack__(Iterable$str_reversed_instance,($str)self);
}

void $str_insert_instance(Sequence$__class__ cl, $WORD self, $int ix, $WORD elem) {
  $str_insert(($str)self,*ix,elem);
}
void $str_append_instance(Sequence$__class__ cl, $WORD self, $WORD elem) {
  $str_append(($str)self,elem);
}

void $str_reverse_instance(Sequence$__class__ cl, $WORD self) {
  $str_reverse(($str)self);
}

// str-specific methods ////////////////////////////////////////////////////////

$str $str_capitalize($str s) {
  str_internal_t si = s->__internal__;
  if (si->nchars==0) {
    NEW_STR(res,null_str);
  }
  unsigned char *p = si->str;
  int cp;
  int cplen = utf8proc_iterate(p,-1,&cp);
  int cpt = utf8proc_totitle(cp);
  int nbytes = si->nbytes - cplen + byte_length(cpt);
  str_internal_t r;
  NEW_INTERNAL(r,si->nchars,nbytes);
  int cpulen = utf8proc_encode_char(cpt,r->str);
  memcpy(r->str+cpulen,si->str+cplen,si->nbytes-cplen);
  NEW_STR(res,r);
}

$str $str_center($str s, int width, $str fill) {
  str_internal_t filli = fill->__internal__;
  if (filli->nchars != 1) {
    exception e;
    MKEXCEPTION(e,TYPEERROR);
    RAISE(e);
  }
  str_internal_t si = s->__internal__;
  if (width <= si->nchars) {
    return s;
  }
  int pad = (width-si->nchars);
  int padleft = pad/2; // Below we make use of the fact padright >= padleft.
  int padright = pad-padleft;
  int fillbytes = filli->nbytes;
  int sbytes = si->nbytes;
  str_internal_t r;
  NEW_INTERNAL(r, width,sbytes+pad*fillbytes);
  unsigned char *c = filli->str;
  unsigned char *p = r->str;
  p += padleft*fillbytes+sbytes;
  for (int i = 0; i<padright; i++) {
    for (int j = 0; j < fillbytes; j++) 
      p[j] = c[j];
    p += fillbytes;
  }
  p -= padright*fillbytes;
  memcpy(r->str,p,padleft*fillbytes);
  p -= sbytes;
  memcpy(p,si->str,sbytes);
  NEW_STR(res,r);
}


$int $str_count($str s, $str sub, $int start, $int end) {
  str_internal_t si = s->__internal__;
  str_internal_t subi = sub->__internal__;
  int isascii = si->nchars == si->nbytes;
  $int st = start;
  $int en = end;
  if (fix_start_end(si->nchars,&st,&en) < 0) return to$int(0);
  unsigned char *p = skip_chars(si->str,*st,isascii);
  unsigned char *q = skip_chars(p,*en-*st,isascii);
  int res = 0;
  int n = bmh(p,subi->str,q-p,subi->nbytes);
  while (n>=0) {
    res++;
    p += n + (subi->nbytes>0 ? subi->nbytes : 1);
    n = bmh(p,subi->str,q-p,subi->nbytes);
  }
  return to$int(res);
}

$bool $str_endswith($str s, $str sub, $int start, $int end) {
  str_internal_t si = s->__internal__;
  str_internal_t subi = sub->__internal__;
  $int st = start;
  $int en = end;
  if (fix_start_end(si->nchars,&st,&en) < 0) return 0;
  int isascii = si->nchars==si->nbytes;
  unsigned char *p = skip_chars(si->str + si->nbytes,*en - si->nchars,isascii) - subi->nbytes;
  unsigned char *q = subi->str;
  for (int i=0; i<subi->nbytes; i++) {
    if (*p == 0 || *p++ != *q++) {
      return 0;
    }
  }
  return 1;
}

$str $str_expandtabs($str s, int tabsize){
  str_internal_t si = s->__internal__;
  int pos = 0;
  int expanded = 0;
  tabsize = tabsize <= 0 ? 1 : tabsize;
  unsigned char buffer[tabsize * si->nchars];
  unsigned char *p = si->str;
  unsigned char *q = buffer;
  for (int i=0; i<si->nchars; i++) {
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
  str_internal_t r;
  NEW_INTERNAL(r,si->nchars+expanded,si->nbytes+expanded);
  memcpy(r->str,buffer,si->nbytes+expanded);
  NEW_STR(res,r);
}

$int $str_find($str s, $str sub, $int start, $int end) {
  str_internal_t si = s->__internal__;
  str_internal_t subi = sub->__internal__;
  int isascii = si->nchars == si->nbytes;
  $int st = start;
  $int en = end;
  if (fix_start_end(si->nchars,&st,&en) < 0) return to$int(-1);
  unsigned char *p = skip_chars(si->str,*st,isascii);
  unsigned char *q = skip_chars(p,*en-*st,isascii);
  int n = bmh(p,subi->str,q-p,subi->nbytes);
  if (n<0) return to$int(-1);
  return to$int(char_no(si,n+p-si->str));
}

$int $str_index($str s, $str sub, $int start, $int end) {
  $int n = $str_find(s,sub,start,end);
  if (*n<0) {
    exception e;
    MKEXCEPTION(e,VALUEERROR);
    RAISE(e);
  }
  return n;
}

$bool $str_isalnum($str s) {
  str_internal_t si = s->__internal__;
  unsigned char *p = si->str;
  int codepoint;
  int nbytes;
  if (si->nchars == 0)
    return 0;
  for (int i=0; i < si->nchars; i++) {
    nbytes = utf8proc_iterate(p,-1,&codepoint);
    utf8proc_category_t cat = utf8proc_category(codepoint);
    if ((cat <  UTF8PROC_CATEGORY_LU || cat >  UTF8PROC_CATEGORY_LO) && cat != UTF8PROC_CATEGORY_ND)
      return 0;
    p += nbytes;
  }
  return 1;
}

$bool $str_isalpha($str s) {
  str_internal_t si = s->__internal__;
  unsigned char *p = si->str;
  int codepoint;
  int nbytes;
  if (si->nchars == 0)
    return 0;
  for (int i=0; i < si->nchars; i++) {
    nbytes = utf8proc_iterate(p,-1,&codepoint);
    utf8proc_category_t cat = utf8proc_category(codepoint);
    if (cat <  UTF8PROC_CATEGORY_LU || cat >  UTF8PROC_CATEGORY_LO)
      return 0;
    p += nbytes;
  }
  return 1;
}

$bool $str_isascii($str s) {
  str_internal_t si = s->__internal__;
  unsigned char *p = si->str;
  for (int i=0; i < si->nbytes; i++) {
    if (*p > 127)
      return 0;
    p++;
  }
  return 1;
}

$bool $str_isdecimal($str s) {
  str_internal_t si = s->__internal__;
  unsigned char *p = si->str;
  int codepoint;
  int nbytes;
  if (si->nchars == 0)
    return 0;
  for (int i=0; i < si->nchars; i++) {
    nbytes = utf8proc_iterate(p,-1,&codepoint);
    utf8proc_category_t cat = utf8proc_category(codepoint);
    if (cat != UTF8PROC_CATEGORY_ND)
      return 0;
    p += nbytes;
  }
  return 1;
}

$bool $str_islower($str s) {
  str_internal_t si = s->__internal__;
  unsigned char *p = si->str;
  int codepoint;
  int nbytes;
  int has_cased = 0;
  if (si->nchars == 0)
    return 0;
  for (int i=0; i < si->nchars; i++) {
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

$bool $str_isprintable($str s) {
  str_internal_t si = s->__internal__;
  unsigned char *p = si->str;
  int codepoint;
  int nbytes;
  if (si->nchars == 0)
    return 0;
  for (int i=0; i < si->nchars; i++) {
    nbytes = utf8proc_iterate(p,-1,&codepoint);
    utf8proc_category_t cat = utf8proc_category(codepoint);
    if (cat >= UTF8PROC_CATEGORY_ZS && codepoint != 0x20)
      return 0;
    p += nbytes;
  }
  return 1;
}

$bool $str_isspace($str s) {
  str_internal_t si = s->__internal__;
  unsigned char *p = si->str;
  int codepoint;
  int nbytes;
  if (si->nchars == 0)
    return 0;
  for (int i=0; i < si->nchars; i++) {
    nbytes = utf8proc_iterate(p,-1,&codepoint);
    if (!isspace_codepoint(codepoint))
      return 0;
    p += nbytes;
  }
  return 1;
}

$bool $str_istitle($str s) {
  str_internal_t si = s->__internal__;
  unsigned char *p = si->str;
  int codepoint;
  int nbytes;
  int hascased = 0;
  int incasedrun = 0;
  if (si->nchars == 0)
    return 0;
  for (int i=0; i < si->nchars; i++) {
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

$bool $str_isupper($str s) {
  str_internal_t si = s->__internal__;
  unsigned char *p = si->str;
  int codepoint;
  int nbytes;
  int hascased = 0;
  if (si->nchars == 0)
    return 0;
  for (int i=0; i < si->nchars; i++) {
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
$str $str_join($str s, Iterator iter) {
  $str res;
  $str nxt  = ($str)iter->__class__->__next__(iter->__class__,iter);
  //  if(!iterator_next(iter)             //BEWARE: must catch STOPITERATION!!!!
  //   res = (str_t)nxt;
  // else
  //  return null_str;

  while(1) {
    nxt =  ($str)iter->__class__->__next__(iter->__class__,iter);
    res = ($str)$str_add($str_add(res,s),nxt);
  }
  return res;
}

$str $str_ljust($str s, int width, $str fill) {
  str_internal_t si = s->__internal__;
  str_internal_t filli = fill->__internal__;
  if (filli->nchars != 1) {
    exception e;
    MKEXCEPTION(e,TYPEERROR);
    RAISE(e);
  }
  if (width <= si->nchars) {
    return s;
  }
  int pad = (width-si->nchars);
  str_internal_t r;
  NEW_INTERNAL(r,width, si->nbytes+pad*filli->nbytes);
  unsigned char *c = filli->str;
  unsigned char *p = r->str + si->nbytes;
  for (int i = 0; i<pad; i++) {
    for (int j = 0; j < filli->nbytes; j++) 
      *p++ = c[j];
  }
  memcpy(r->str,si->str,si->nbytes);
  NEW_STR(res,r);
}

$str $str_lower($str s) {
  return str_transform(s,utf8proc_tolower);
}

$str $str_lstrip($str s, $str cs) {
  str_internal_t si = s->__internal__;
  str_internal_t csi = cs->__internal__;
  unsigned char *p = si->str;
  int i, nbytes;
  for (i=0; i<si->nchars; i++) {
    $str c = mk_char(p);
    if (cs == NULL ?  !$str_isspace(c) :
      bmh(csi->str,p,csi->nbytes,byte_length2(*p)) < 0) 
      break;
    p += byte_length2(*p);
  }
  nbytes = si->nbytes + si->str - p;
  str_internal_t r;
  NEW_INTERNAL(r,si->nchars-i,nbytes);
  memcpy(r->str,p,nbytes);
  NEW_STR(res,r);
}

void $str_partition($str s, $str sep, $str *ls, $str *ssep, $str *rs) {
  str_internal_t si = s->__internal__;
  str_internal_t sepi = sep->__internal__;
  $int n = $str_find(s,sep,NULL,NULL);
  if (*n<0) {
    *ls = s; *ssep = mk_str(null_str); *rs = mk_str(null_str);
  } else {
    int nb = bmh(si->str,sepi->str,si->nbytes,sepi->nbytes);
    str_internal_t r1;
    NEW_INTERNAL(r1,*n,nb);
    memcpy(r1->str,si->str,nb);
    *ls = mk_str(r1);
    str_internal_t r2;
    int nbr = si->nbytes - sepi->nbytes - nb;
    NEW_INTERNAL(r2,si->nchars-*n-sepi->nchars,nbr);
    memcpy(r2->str,si->str+nb+sepi->nbytes,nbr);
    *rs = mk_str(r2);
    *ssep = sep;
  }
}

$str $str_replace($str s, $str old, $str new, $int count) {
  if (count==NULL)
    count = to$int(INT_MAX);
  $int c = $str_count(s,old,NULL,NULL);
  int c0 = *count < *c ? *count : *c;
  if (c0==0){
    return s;
  }
  str_internal_t si = s->__internal__;
  str_internal_t newi = new->__internal__;
  str_internal_t oldi = old->__internal__;
  int nbytes = si->nbytes + c0*(newi->nbytes-oldi->nbytes);
  int nchars = si->nchars+c0*(newi->nchars-oldi->nchars);
  str_internal_t r;
  NEW_INTERNAL(r,nchars,nbytes);
  unsigned char *p = si->str;
  unsigned char *q = r->str;
  unsigned char *pold = oldi->str;
  unsigned char *pnew = newi->str;
  int plen = si->nbytes;
  int n;
  for (int i=0; i<c0; i++) {
    n = i>0 && oldi->nbytes==0 ? 1 : bmh(p,pold,plen,oldi->nbytes);
    if (n>0) {
      memcpy(q,p,n);
      p+=n; q+=n;
    }
    memcpy(q,pnew,newi->nbytes);
    p += oldi->nbytes;
    q += newi->nbytes;
    plen -= n+oldi->nbytes;
  }
  if (plen>0)
    memcpy(q,p,plen);
  NEW_STR(res,r);
}
      

$int $str_rfind($str s, $str sub, $int start, $int end) {
  str_internal_t si = s->__internal__;
  str_internal_t subi = sub->__internal__;
  int isascii = si->nchars == si->nbytes;
  $int st = start;
  $int en = end;
  if (fix_start_end(si->nchars,&st,&en) < 0) return to$int(-1);
  unsigned char *p = skip_chars(si->str,*st,isascii);
  unsigned char *q = skip_chars(p,*en-*st,isascii);
  int n = rbmh(p,subi->str,q-p,subi->nbytes);
  if (n<0) return to$int(-1);
  return to$int(char_no(si,n+p-si->str));
}


$int $str_rindex($str s, $str sub, $int start, $int end) {
  $int n = $str_rfind(s,sub,start,end);
  if (*n<0) {
    exception e;
    MKEXCEPTION(e,VALUEERROR);
    RAISE(e);
  };
  return n;
}

$str $str_rjust($str s, int width, $str fill) {
  str_internal_t si = s->__internal__;
  str_internal_t filli = fill->__internal__;
  if (filli->nchars != 1) {
    exception e;
    MKEXCEPTION(e,TYPEERROR);
    RAISE(e);
  }
  if (width <= si->nchars) {
    return s;
  }
  int pad = (width-si->nchars);
  str_internal_t r;
  NEW_INTERNAL(r,width,si->nbytes+pad*filli->nbytes);
  unsigned char *c = filli->str;
  unsigned char *p = r->str;
  for (int i = 0; i<pad; i++) {
    for (int j = 0; j < filli->nbytes; j++) 
      *p++ = c[j];
  }
  memcpy(p,si->str,si->nbytes);
  NEW_STR(res,r);
}
                                
void $str_rpartition($str s, $str sep, $str *ls, $str *ssep, $str *rs) {
  str_internal_t si = s->__internal__;
  str_internal_t sepi = sep->__internal__;
  $int n = $str_rfind(s,sep,NULL,NULL);
  if (*n<0) {
    *ls =mk_str( null_str); *ssep = mk_str(null_str); *rs = s;
  } else {
    int nb = rbmh(si->str,sepi->str,si->nbytes,sepi->nbytes);
    str_internal_t r1;
    NEW_INTERNAL(r1,*n,nb);
    memcpy(r1->str,si->str,nb);
    *ls = mk_str(r1);
    int nbr = si->nbytes - sepi->nbytes - nb;
    str_internal_t r2;    
    NEW_INTERNAL(r2,si->nchars-*n-sepi->nchars,nbr);
    memcpy(r2->str,si->str+nb+sepi->nbytes,nbr);
    *rs = mk_str(r2);
    *ssep = sep;
  }
}


$list $str_split($str s, $str sep, $int maxsplit) {
  $list res = $list_fromiter(NULL);
  str_internal_t si = s->__internal__;
  if (maxsplit == NULL || *maxsplit < 0) maxsplit = to$int(INT_MAX); 
  int remaining = si->nchars;
  if (sep == NULL) {
    unsigned char *p = si->str;
    int nbytes, codepoint, wordlength;
    if (remaining==0) {
      return res;
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
          if (*$list_len(res) == *maxsplit)
            break; // we have now removed leading whitespace in remainder
        } else
          wordlength++;
      } else {
          if (inword) {
            inword = 0;
            str_internal_t word;
            NEW_INTERNAL(word,wordlength,p-q);
            memcpy(word->str,q,p-q);
            $list_append(res,mk_str(word));
          }
      }
      remaining--;
      p += nbytes;
    }
    // this if statement should be simplified; almost code duplication.
    if (remaining == 0) {
      if (inword) {
        str_internal_t word;
        NEW_INTERNAL(word,wordlength,p-q);
        memcpy(word->str,q,p-q);
        $list_append(res,mk_str(word));
      }
    } else {
      str_internal_t word;
      p = si->str+si->nbytes;
      NEW_INTERNAL(word,remaining,p-q);
      memcpy(word->str,q,p-q);
      $list_append(res,mk_str(word));
    }
    // $WORD w = list_getitem(res,0);
    return res;
  } else { // separator given
    str_internal_t sepi = sep->__internal__;
    if (sepi->nchars==0) {
      exception e;
      MKEXCEPTION(e,VALUEERROR);
      RAISE(e);
    }
    if (remaining==0) { // for some unfathomable reason, this is the behaviour of the Python method
      $list_append(res,mk_str(null_str));
      return res;
    }
    $str ls, rs, ssep;
    rs = s;
    // Note: This builds many intermediate rs strings...
    while (rs->__internal__->nchars>0 && *$list_len(res) < *maxsplit) {
     $str_partition(rs,sep,&ls,&ssep,&rs);
     $list_append(res,ls);
    }
    if (ssep->__internal__->nchars>0)
      $list_append(res,rs);
    return res;
  }
}

$list $str_splitlines($str s) {
  $list res = $list_fromiter(NULL);
  str_internal_t si = s->__internal__;
  int remaining = si->nchars;
  unsigned char *p = si->str;
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
        str_internal_t word;
        NEW_INTERNAL(word,wordlength,p-q);
        memcpy(word->str,q,p-q);
        $list_append(res,mk_str(word));
      }
    }
    remaining--;
    p += nbytes;
  }
  if (inword) {
    str_internal_t word;
    NEW_INTERNAL(word,wordlength,p-q);
    memcpy(word->str,q,p-q);
    $list_append(res,mk_str(word));
  }
  return res;
} 


$str $str_rstrip($str s, $str cs) {
  str_internal_t si = s->__internal__;
  str_internal_t csi = cs->__internal__;
  unsigned char *p = si->str + si->nbytes;
  int i, nbytes;
  for (i=0; i<si->nchars; i++) {
    p = skip_chars(p,-1,0);
    $str c = mk_char(p);
    if (cs == NULL ?  !$str_isspace(c) :
      rbmh(csi->str,p,csi->nbytes,byte_length2(*p)) < 0) 
      break;
  }
  nbytes = p + byte_length2(*p) - si->str;
  str_internal_t r;
  NEW_INTERNAL(r,si->nchars-i,nbytes);
  memcpy(r->str,si->str,nbytes);
  NEW_STR(res,r);
}

$bool $str_startswith($str s, $str sub, $int start, $int end) {
  str_internal_t si = s->__internal__;
  str_internal_t subi = sub->__internal__;
  $int st = start;
  $int en = end;
  if (fix_start_end(si->nchars,&st,&en) < 0) return 0;
  int isascii = si->nchars==si->nbytes;
  unsigned char *p = skip_chars(si->str,*st,isascii);
  unsigned char *q = subi->str;
  for (int i=0; i<subi->nbytes; i++) {
    if (*p == 0 || *p++ != *q++) {
      return 0;
    }
  }
  return 1;
}


$str $str_strip($str s, $str cs) {
  return $str_lstrip($str_rstrip(s,cs),cs);
}

$str $str_upper($str s) {
  return str_transform(s,utf8proc_toupper);
}

$str $str_zfill($str s, int width) {
  str_internal_t si = s->__internal__;
  int fill = width - si->nchars;
  if (fill < 0)
    return s;
  str_internal_t r;
  NEW_INTERNAL(r,width,si->nbytes+fill);
  unsigned char *p = si->str;
  unsigned char *q = r->str;
  int hassign = (*p=='+' | *p=='-');
  if (hassign) {
    *q = *p;
    q++;
  }
  for (int i=0; i < fill; i++) 
    *q++ = '0';
  memcpy(r->str+hassign+fill,si->str+hassign,si->nbytes-hassign);
  NEW_STR(res,r);
}
 
