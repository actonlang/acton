#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <limits.h>
#include "builtin.h"
#include "utf8proc.h"


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

$int $str_hash_instance(Hashable$__class__ cl, $WORD self);

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

static struct Hashable$__class__ Hashable$str_struct = {"GC_Hashable__Eq", &Eq$str_struct, $str_hash_instance};
Hashable$__class__ Hashable$str_instance = &Hashable$str_struct;

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

static struct $str null_struct = {"GC_NUL",&$str_table,0,0,&nul};

static $str null_str = &null_struct;


#define NEW_UNFILLED(nm,nchrs,nbtes)         \
nm = malloc(sizeof(struct $str)); \
(nm)->__class__ = $str_methods; \
(nm)->nchars = nchrs;            \
(nm)->nbytes = nbtes;            \
(nm)->str = malloc((nm)->nbytes + 1);    \
(nm)->str[(nm)->nbytes] = 0

$str fromUTF8(char *str) {
  int nbytes = 0;
  int nchars = 0;

  unsigned char *p = (unsigned char*)str;
  int cp, cpnbytes;
  while(1) {
    if (*p == '\0') {
      $str res = malloc(sizeof(struct $str));
      res->__class__ = $str_methods;
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

unsigned char *toUTF8($str str) {
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
  $str res;
  NEW_UNFILLED(res,s->nchars,nbytes);
  memcpy(res->str,buffer,nbytes);
  return res;
}

// Find char position in text from byte position.
// Assume that i is first byte of a char in text.
static int char_no($str text,int i) {
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
static int byte_no($str text, int i) {
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
  $str res;
  NEW_UNFILLED(res,1,byte_length2(*p));
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
  return !strcmp((char *)a->str,(char *)b->str);
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

// Hashable ///////////////////////////////////////////////////////////////////////////////////


// hash function $string_hash defined in hash.c

// instance method

$int $str_hash_instance(Hashable$__class__ cl, $WORD self) {
  $int res = malloc(sizeof(long));
  $str s = ($str)self;
  *res = $string_hash(s->str,s->nbytes);
  return res;
}

// Plus /////////////////////////////////////////////////////////////////////////////////////////////

$str $str_add($str s, $str t) {
  $str res;
  NEW_UNFILLED(res,s->nchars + t->nchars,s->nbytes + t->nbytes);
  memcpy(res->str,s->str,s->nbytes);
  memcpy(res->str+s->nbytes,t->str,t->nbytes);
  return res;
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
  $int res = to$int(s->nchars);
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
  return bmh(s->str,sub->str,s->nbytes,sub->nbytes) > 0;
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
  state->nxt = s->str;
  state->remaining = s->nchars;
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
  unsigned char *p = s->str;
  int ix = get_index(i,s->nchars);
  p = skip_chars(p,ix,s->nchars == s->nbytes);
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
  int isascii = s->nchars == s->nbytes;
  int nchars = s->nchars;
  int nbytes = 0;
  int start, stop, step, slen;
  normalize_slice(slc, nchars, &slen, &start, &stop, &step);
 //slice notation have been eliminated and default values applied.
  unsigned char buffer[4*slen];       // very conservative buffer size.
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
  $str res;
  NEW_UNFILLED(res,slen,nbytes);
  if (nbytes > 0)
    memcpy(res->str,buffer,nbytes);
 return res;
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
  if (s->nchars==0) {
    return null_str;
  }
  unsigned char *p = s->str;
  int cp;
  int cplen = utf8proc_iterate(p,-1,&cp);
  int cpt = utf8proc_totitle(cp);
  int nbytes = s->nbytes - cplen + byte_length(cpt);
  $str res;
  NEW_UNFILLED(res,s->nchars,nbytes);
  int cpulen = utf8proc_encode_char(cpt,res->str);
  memcpy(res->str+cpulen,s->str+cplen,s->nbytes-cplen);
  return res;
}

$str $str_center($str s, int width, $str fill) {
  if (fill->nchars != 1) {
    exception e;
    MKEXCEPTION(e,TYPEERROR);
    RAISE(e);
  }
  if (width <= s->nchars) {
    return s;
  }
  int pad = (width-s->nchars);
  int padleft = pad/2; // Below we make use of the fact padright >= padleft.
  int padright = pad-padleft;
  int fillbytes = fill->nbytes;
  int sbytes = s->nbytes;
  $str res;
  NEW_UNFILLED(res, width,sbytes+pad*fillbytes);
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


$int $str_count($str s, $str sub, $int start, $int end) {
  int isascii = s->nchars == s->nbytes;
  $int st = start;
  $int en = end;
  if (fix_start_end(s->nchars,&st,&en) < 0) return to$int(0);
  unsigned char *p = skip_chars(s->str,*st,isascii);
  unsigned char *q = skip_chars(p,*en-*st,isascii);
  int res = 0;
  int n = bmh(p,sub->str,q-p,sub->nbytes);
  while (n>=0) {
    res++;
    p += n + (sub->nbytes>0 ? sub->nbytes : 1);
    n = bmh(p,sub->str,q-p,sub->nbytes);
  }
  return to$int(res);
}

$bool $str_endswith($str s, $str sub, $int start, $int end) {
  $int st = start;
  $int en = end;
  if (fix_start_end(s->nchars,&st,&en) < 0) return 0;
  int isascii = s->nchars==s->nbytes;
  unsigned char *p = skip_chars(s->str + s->nbytes,*en - s->nchars,isascii) - sub->nbytes;
  unsigned char *q = sub->str;
  for (int i=0; i<sub->nbytes; i++) {
    if (*p == 0 || *p++ != *q++) {
      return 0;
    }
  }
  return 1;
}

$str $str_expandtabs($str s, int tabsize){
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
  $str res;
  NEW_UNFILLED(res,s->nchars+expanded,s->nbytes+expanded);
  memcpy(res->str,buffer,s->nbytes+expanded);
  return res;
}

$int $str_find($str s, $str sub, $int start, $int end) {
  int isascii = s->nchars == s->nbytes;
  $int st = start;
  $int en = end;
  if (fix_start_end(s->nchars,&st,&en) < 0) return to$int(-1);
  unsigned char *p = skip_chars(s->str,*st,isascii);
  unsigned char *q = skip_chars(p,*en-*st,isascii);
  int n = bmh(p,sub->str,q-p,sub->nbytes);
  if (n<0) return to$int(-1);
  return to$int(char_no(s,n+p-s->str));
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

$bool $str_isalpha($str s) {
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

$bool $str_isascii($str s) {
  unsigned char *p = s->str;
  for (int i=0; i < s->nbytes; i++) {
    if (*p > 127)
      return 0;
    p++;
  }
  return 1;
}

$bool $str_isdecimal($str s) {
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

$bool $str_islower($str s) {
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

$bool $str_isprintable($str s) {
  unsigned char *p = s->str;
  int codepoint;
  int nbytes;
  if (s->nchars == 0)
    return 0;
  for (int i=0; i < s->nchars; i++) {
    nbytes = utf8proc_iterate(p,-1,&codepoint);
    utf8proc_category_t cat = utf8proc_category(codepoint);
    if (cat >= UTF8PROC_CATEGORY_ZS && codepoint != 0x20)
      return 0;
    p += nbytes;
  }
  return 1;
}

$bool $str_isspace($str s) {
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

$bool $str_istitle($str s) {
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

$bool $str_isupper($str s) {
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
  if (fill->nchars != 1) {
    exception e;
    MKEXCEPTION(e,TYPEERROR);
    RAISE(e);
  }
  if (width <= s->nchars) {
    return s;
  }
  int pad = (width-s->nchars);
  $str res;
  NEW_UNFILLED(res,width, s->nbytes+pad*fill->nbytes);
  unsigned char *c = fill->str;
  unsigned char *p = res->str + s->nbytes;
  for (int i = 0; i<pad; i++) {
    for (int j = 0; j < fill->nbytes; j++) 
      *p++ = c[j];
  }
  memcpy(res->str,s->str,s->nbytes);
  return res;
}

$str $str_lower($str s) {
  return str_transform(s,utf8proc_tolower);
}

$str $str_lstrip($str s, $str cs) {
  unsigned char *p = s->str;
  int i, nbytes;
  for (i=0; i<s->nchars; i++) {
    $str c = mk_char(p);
    if (cs == NULL ?  !$str_isspace(c) :
      bmh(cs->str,p,cs->nbytes,byte_length2(*p)) < 0) 
      break;
    p += byte_length2(*p);
  }
  nbytes = s->nbytes + s->str - p;
  $str res;
  NEW_UNFILLED(res,s->nchars-i,nbytes);
  memcpy(res->str,p,nbytes);
  return res;
}

void $str_partition($str s, $str sep, $str *ls, $str *ssep, $str *rs) {
  $int n = $str_find(s,sep,NULL,NULL);
  if (*n<0) {
    *ls = s; *ssep = null_str; *rs = null_str;
  } else {
    int nb = bmh(s->str,sep->str,s->nbytes,sep->nbytes);
    $str res1;
    NEW_UNFILLED(res1,*n,nb);
    memcpy(res1->str,s->str,nb);
    *ls = res1;
    $str res2;
    int nbr = s->nbytes - sep->nbytes - nb;
    NEW_UNFILLED(res2,s->nchars-*n-sep->nchars,nbr);
    memcpy(res2->str,s->str+nb+sep->nbytes,nbr);
    *rs = res2;
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
  int nbytes = s->nbytes + c0*(new->nbytes-old->nbytes);
  int nchars = s->nchars+c0*(new->nchars-old->nchars);
  $str res;
  NEW_UNFILLED(res,nchars,nbytes);
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
      

$int $str_rfind($str s, $str sub, $int start, $int end) {
  int isascii = s->nchars == s->nbytes;
  $int st = start;
  $int en = end;
  if (fix_start_end(s->nchars,&st,&en) < 0) return to$int(-1);
  unsigned char *p = skip_chars(s->str,*st,isascii);
  unsigned char *q = skip_chars(p,*en-*st,isascii);
  int n = rbmh(p,sub->str,q-p,sub->nbytes);
  if (n<0) return to$int(-1);
  return to$int(char_no(s,n+p-s->str));
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
  if (fill->nchars != 1) {
    exception e;
    MKEXCEPTION(e,TYPEERROR);
    RAISE(e);
  }
  if (width <= s->nchars) {
    return s;
  }
  int pad = (width-s->nchars);
  $str res;
  NEW_UNFILLED(res,width,s->nbytes+pad*fill->nbytes);
  unsigned char *c = fill->str;
  unsigned char *p = res->str;
  for (int i = 0; i<pad; i++) {
    for (int j = 0; j < fill->nbytes; j++) 
      *p++ = c[j];
  }
  memcpy(p,s->str,s->nbytes);
  return res;
}
                                
void $str_rpartition($str s, $str sep, $str *ls, $str *ssep, $str *rs) {
  $int n = $str_rfind(s,sep,NULL,NULL);
  if (*n<0) {
    *ls = null_str; *ssep = null_str; *rs = s;
  } else {
    int nb = rbmh(s->str,sep->str,s->nbytes,sep->nbytes);
    $str res1;
    NEW_UNFILLED(res1,*n,nb);
    memcpy(res1->str,s->str,nb);
    *ls = res1;
    int nbr = s->nbytes - sep->nbytes - nb;
    $str res2;    
    NEW_UNFILLED(res2,s->nchars-*n-sep->nchars,nbr);
    memcpy(res2->str,s->str+nb+sep->nbytes,nbr);
    *rs = res2;
    *ssep = sep;
  }
}


$list $str_split($str s, $str sep, $int maxsplit) {
  $list res = $list_fromiter(NULL);
  if (maxsplit == NULL || *maxsplit < 0) maxsplit = to$int(INT_MAX); 
  int remaining = s->nchars;
  if (sep == NULL) {
    unsigned char *p = s->str;
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
            $str word;
            NEW_UNFILLED(word,wordlength,p-q);
            memcpy(word->str,q,p-q);
            $list_append(res,word);
          }
      }
      remaining--;
      p += nbytes;
    }
    // this if statement should be simplified; almost code duplication.
    if (remaining == 0) {
      if (inword) {
        $str word;
        NEW_UNFILLED(word,wordlength,p-q);
        memcpy(word->str,q,p-q);
        $list_append(res,word);
      }
    } else {
      $str word;
      p = s->str+s->nbytes;
      NEW_UNFILLED(word,remaining,p-q);
      memcpy(word->str,q,p-q);
      $list_append(res,word);
    }
    // $WORD w = list_getitem(res,0);
    return res;
  } else { // separator given
    if (sep->nchars==0) {
      exception e;
      MKEXCEPTION(e,VALUEERROR);
      RAISE(e);
    }
    if (remaining==0) { // for some unfathomable reason, this is the behaviour of the Python method
      $list_append(res,null_str);
      return res;
    }
    $str ls, rs, ssep;
    rs = s;
    // Note: This builds many intermediate rs strings...
    while (rs->nchars>0 && *$list_len(res) < *maxsplit) {
     $str_partition(rs,sep,&ls,&ssep,&rs);
     $list_append(res,ls);
    }
    if (ssep->nchars>0)
      $list_append(res,rs);
    return res;
  }
}

$list $str_splitlines($str s) {
  $list res = $list_fromiter(NULL);
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
        $str word;
        NEW_UNFILLED(word,wordlength,p-q);
        memcpy(word->str,q,p-q);
        $list_append(res,word);
      }
    }
    remaining--;
    p += nbytes;
  }
  if (inword) {
    $str word;
    NEW_UNFILLED(word,wordlength,p-q);
    memcpy(word->str,q,p-q);
    $list_append(res,word);
  }
  return res;
} 


$str $str_rstrip($str s, $str cs) {
  unsigned char *p = s->str + s->nbytes;
  int i, nbytes;
  for (i=0; i<s->nchars; i++) {
    p = skip_chars(p,-1,0);
    $str c = mk_char(p);
    if (cs == NULL ?  !$str_isspace(c) :
      rbmh(cs->str,p,cs->nbytes,byte_length2(*p)) < 0) 
      break;
  }
  nbytes = p + byte_length2(*p) - s->str;
  $str res;
  NEW_UNFILLED(res,s->nchars-i,nbytes);
  memcpy(res->str,s->str,nbytes);
  return res;
}

$bool $str_startswith($str s, $str sub, $int start, $int end) {
  $int st = start;
  $int en = end;
  if (fix_start_end(s->nchars,&st,&en) < 0) return 0;
  int isascii = s->nchars==s->nbytes;
  unsigned char *p = skip_chars(s->str,*st,isascii);
  unsigned char *q = sub->str;
  for (int i=0; i<sub->nbytes; i++) {
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
  int fill = width - s->nchars;
  if (fill < 0)
    return s;
  $str res;
  NEW_UNFILLED(res,width,s->nbytes+fill);
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
 
