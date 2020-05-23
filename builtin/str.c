#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <limits.h>
#include "builtin.h"
#include "utf8proc.h"


//  Method tables ///////////////////////////////////////////////////////////////

// String-specific methods

void $str_init($str, char*);
void $str_serialize($str,$Serial$state);
$str $str_deserialize($Serial$state);
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
$str $str_join($str sep, $Iterable$opaque it);
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

struct $str$class $str$methods =
  {"",UNASSIGNED,NULL,$str_init, $str_serialize, $str_deserialize, $str_capitalize, $str_center, $str_count, $str_endswith, $str_expandtabs, $str_find, $str_index, $str_isalnum, $str_isalpha,
   $str_isascii, $str_isdecimal, $str_islower, $str_isprintable, $str_isspace, $str_istitle, $str_isupper, $str_join, $str_ljust, $str_lower, $str_lstrip,
   $str_partition, $str_replace, $str_rfind, $str_rindex, $str_rjust, $str_rpartition, $str_rstrip, $str_split, $str_splitlines, $str_startswith, $str_strip,
   $str_upper, $str_zfill};

//static $str$methods $str_methods = &$str_table;

// Implementations of protocol methods

int $str_eq($str,$str);
int $str_neq($str,$str);
int $str_lt($str,$str);
int $str_le($str,$str);
int $str_gt($str,$str);
int $str_ge($str,$str);

$Iterator $str_iter($str);

$str $str_fromiter($Iterator);
$int $str_len($str str);

int $str_contains ($str, $str);
int $str_containsnot ($str, $str);

$str $str_getitem($str, int);
$str $str_getslice($str, $Slice);
 
$str $str_add($str, $str);

// Protocol instances

$bool $Ord$str$__eq__ ($Ord$str wit, $str a, $str b) {
  return to$bool($str_eq(a,b));
}

$bool $Ord$str$__ne__ ($Ord$str wit, $str a, $str b) {
  return  to$bool($str_neq(a,b));
}

$bool $Ord$str$__lt__ ($Ord$str wit, $str a, $str b) {
  return to$bool($str_lt(a,b));
}

$bool $Ord$str$__le__ ($Ord$str wit, $str a, $str b){
  return to$bool($str_le(a,b));
}

$bool $Ord$str$__gt__ ($Ord$str wit, $str a, $str b){
  return to$bool($str_gt(a,b));
}

$bool $Ord$str$__ge__ ($Ord$str wit, $str a, $str b){
  return to$bool($str_ge(a,b));
}

$Iterator $Container$str$__iter__ ($Container$str wit, $str str) {
  return $str_iter(str);
}

$str $Container$str$__fromiter__ ($Container$str wit, $Iterable$opaque it) {
  return $str_fromiter(it->proto->$class->__iter__(it->proto,it->impl));
}

$int $Container$str$__len__ ($Container$str wit, $str str) {
  return $str_len(str);
}

$bool $Container$str$__contains__ ($Container$str wit, $str str, $str sub) {
  return to$bool($str_contains(str, sub));
}

$bool $Container$str$__containsnot__ ($Container$str wit, $str str, $str sub) {
  return to$bool($str_containsnot(str, sub));
}  

$str $Sliceable$str$__getitem__ ($Sliceable$str wit, $str str, $int i) {
  return $str_getitem(str,from$int(i));
}

// this should be an internal error instead; calls are prevented by typechecking.
void $Sliceable$str$__setitem__ ($Sliceable$str wit, $str str, $int i, $str val) {
    RAISE(($BaseException)$NEW($NotImplementedError,from$UTF8("setitem: str is immutable")));
}

void $Sliceable$str$__delitem__ ($Sliceable$str wit, $str str, $int i) {
    RAISE(($BaseException)$NEW($NotImplementedError,from$UTF8("delitem: str is immutable")));
}

$str $Sliceable$str$__getslice__ ($Sliceable$str wit, $str str, $Slice slc) {
  return $str_getslice(str,slc);
}

void $Sliceable$str$__setslice__ ($Sliceable$str wit, $str str, $Slice slc, $Iterable$opaque it) {
    RAISE(($BaseException)$NEW($NotImplementedError,from$UTF8("setslice: str is immutable")));
}

void $Sliceable$str$__delslice__ ($Sliceable$str wit, $str str, $Slice slc) {
    RAISE(($BaseException)$NEW($NotImplementedError,from$UTF8("delslice: str is immutable")));
}

$str $Plus$str$__add__ ($Plus$str wit, $str a, $str b) {
  return $str_add(a,b);
}

$bool $Hashable$str$__eq__ ($Hashable$str wit, $str a, $str b) {
  return to$bool($str_eq(a,b));
}

$bool $Hashable$str$__ne__ ($Hashable$str wit, $str a, $str b) {
  return to$bool($str_neq(a,b));
}

$int $Hashable$str$__hash__($Hashable$str wit, $str str) {
  return to$int($string_hash(str->str,str->nbytes));
}

$int $Hashable$str$__keyinfo__($Hashable$str wit) {
  return to$int(STR_ID);
}
 
struct $Ord$str$class  $Ord$str$methods = {"", UNASSIGNED, NULL,(void (*)($Ord$str))$default__init__,$Ord$str$__eq__, $Ord$str$__ne__, $Ord$str$__lt__, $Ord$str$__le__, $Ord$str$__gt__, $Ord$str$__ge__};
struct $Ord$str $Ord$str_instance = {&$Ord$str$methods};
$Ord$str $Ord$str$witness = &$Ord$str_instance;

struct $Container$str$class  $Container$str$methods = {"",UNASSIGNED, NULL,$Container$str$__init__,$Container$str$__iter__, $Container$str$__fromiter__, $Container$str$__len__, $Container$str$__containsnot__};
struct $Container$str $Container$str_instance = {&$Container$str$methods,($Eq)&$Ord$str_instance};
$Container$str $Container$str$witness = &$Container$str_instance;

struct $Sliceable$str$class  $Sliceable$str$methods = {"", UNASSIGNED,NULL,(void (*)($Sliceable$str))$default__init__,$Sliceable$str$__getitem__, $Sliceable$str$__setitem__, $Sliceable$str$__delitem__,
                                                                    $Sliceable$str$__getslice__, $Sliceable$str$__setslice__, $Sliceable$str$__delslice__};
struct $Sliceable$str $Sliceable$str_instance = {&$Sliceable$str$methods};
$Sliceable$str $Sliceable$str$witness = &$Sliceable$str_instance;

struct $Plus$str$class  $Plus$str$methods = {"", UNASSIGNED,NULL,(void (*)($Plus$str))$default__init__,$Plus$str$__add__};
struct $Plus$str $Plus$str_instance = {&$Plus$str$methods};
$Plus$str $Plus$str$witness = &$Plus$str_instance;

struct $Hashable$str$class  $Hashable$str$methods = {"", UNASSIGNED,NULL,(void (*)($Hashable$str))$default__init__, $Hashable$str$__eq__, $Hashable$str$__ne__, $Hashable$str$__hash__};
struct $Hashable$str $Hashable$str_instance = {&$Hashable$str$methods};
$Hashable$str $Hashable$str$witness = &$Hashable$str_instance;

void $Container$str$__init__ ($Container$str wit, $Eq w$Eq$A) {
  wit->w$Eq$A = w$Eq$A;
}

/*
$Ord$str $Ord$str_new() {
  return $Ord$str$witness;
}

$Hashable$str $Hashable$str_new() {
  return $Hashable$str$witness;
}

$Plus$str $Plus$str_new() {
  return $Plus$str$witness;
}

$Sliceable$str $Sliceable$str_new() {
  return $Sliceable$str$witness;
}

$Container$str $Container$str_new() {
  return $Container$str$witness;
}
*/
static unsigned char nul = 0;

static struct $str null_struct = {&$str$methods,0,0,&nul};

static $str null_str = &null_struct;


#define NEW_UNFILLED(nm,nchrs,nbtes)         \
nm = malloc(sizeof(struct $str)); \
(nm)->$class = &$str$methods; \
(nm)->nchars = nchrs;            \
(nm)->nbytes = nbtes;            \
(nm)->str = malloc((nm)->nbytes + 1);    \
(nm)->str[(nm)->nbytes] = 0

$str from$UTF8(char *str) {
  int nbytes = 0;
  int nchars = 0;

  unsigned char *p = (unsigned char*)str;
  int cp, cpnbytes;
  while(1) {
    if (*p == '\0') {
      $str res = malloc(sizeof(struct $str));
      res->$class = &$str$methods;
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

unsigned char *to$UTF8($str str) {
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
  RAISE(($BaseException)$NEW($IndexError,from$UTF8("indexing outside str")));
  return 0;
}

 
// Eliminates slice notation in find, index, count and other methods
// with optional start and end and adds defaults for omitted parameters.

static int fix_start_end(int nchars, $int *start, $int *end) {
  if (*start==NULL) {
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

  if (*end==NULL) {
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
Note: We make str instances for Indexed and Sliceable even though these protocols 
include mutating methods. These methods raise NOTIMPLEMENTED.
*/

// $Ord ///////////////////////////////////////////////////////////////////////////////////////////////


int $str_eq($str a, $str b) {
  return (a->nbytes==b->nbytes && !strcmp((char *)a->str,(char *)b->str));
}
         
int $str_neq($str a, $str b) {
  return !$str_eq(a,b);
}

// the comparisons below are OK only for ASCII! Not clear how to do this for UTF-8.
 
int $str_lt($str a, $str b) {
  return (strcmp((char *)a->str,(char *)b->str) < 0);
}
 
int $str_le($str a, $str b) {
  return (strcmp((char *)a->str,(char *)b->str) <= 0);
}
 
int $str_gt($str a, $str b) {
  return (strcmp((char *)a->str,(char *)b->str) > 0);
}
 
int $str_ge($str a, $str b) {
  return (strcmp((char *)a->str,(char *)b->str) >= 0);
}
 

// $Hashable ///////////////////////////////////////////////////////////////////////////////////


// hash function $string_hash defined in hash.c

// $Plus /////////////////////////////////////////////////////////////////////////////////////////////

$str $str_add($str s, $str t) {
  $str res;
  NEW_UNFILLED(res,s->nchars + t->nchars,s->nbytes + t->nbytes);
  memcpy(res->str,s->str,s->nbytes);
  memcpy(res->str+s->nbytes,t->str,t->nbytes);
  return res;
}

// Collection ///////////////////////////////////////////////////////////////////////////////////////

// this should be eliminated
$str $str_fromiter($Iterator it) {
  return NULL;
}
         
         
$int $str_len($str s) {
  $int res = to$int(s->nchars);
  return res;
}

// $Container ///////////////////////////////////////////////////////////////////////////

int $str_contains($str s, $str sub) {
  return bmh(s->str,sub->str,s->nbytes,sub->nbytes) > 0;
}

int $str_containsnot($str s, $str sub) {
  return !$str_contains(s,sub);
}

// Iterable ///////////////////////////////////////////////////////////////////////////

void $Iterator$str_init($Iterator$str self, $str str) {
  self->src = str;
  self->nxt = 0;
}

void $Iterator$str_serialize($Iterator$str self,$Serial$state state) {
  $step_serialize(self->src,state);
  $step_serialize(to$int(self->nxt),state);
}


$Iterator$str $Iterator$str$_deserialize($Serial$state state) {
   $Iterator$str res = $DNEW($Iterator$str,state);
   res->src = ($str)$step_deserialize(state);
   res->nxt = from$int(($int)$step_deserialize(state));
   return res;
}


// this is next function for forward iteration
static $str $Iterator$str_next($Iterator$str self) {
  unsigned char *p = &self->src->str[self->nxt];
  if (*p != 0) {
    self->nxt +=byte_length2(*p);
    return mk_char(p);
  }
  return NULL;
}

$Iterator $str_iter($str str) {
  return ($Iterator)$NEW($Iterator$str,str);
}

struct $Iterator$str$class $Iterator$str$methods = {"",UNASSIGNED,($Super$class)&$Iterator$methods, $Iterator$str_init,
                                                      $Iterator$str_serialize, $Iterator$str$_deserialize, $Iterator$str_next};


// Indexed ///////////////////////////////////////////////////////////////////////////

$str $str_getitem($str s, int i) {
  unsigned char *p = s->str;
  int ix = get_index(i,s->nchars);
  p = skip_chars(p,ix,s->nchars == s->nbytes);
  return mk_char(p);
}
 
// Sliceable //////////////////////////////////////////////////////////////////////////////////////

$str $str_getslice($str s, $Slice slc) {
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

// Serialization ////////////////////////////////////////////////////////////// 

void $str_init($str self, char *str) {
  int nbytes = 0;
  int nchars = 0;

  unsigned char *p = (unsigned char*)str;
  int cp, cpnbytes;
  while(1) {
    if (*p == '\0') {
      self->nbytes = nbytes;
      self->nchars = nchars;
      self->str = (unsigned char*)str;
      return;
    }
    cpnbytes = utf8proc_iterate(p,-1,&cp);
    if (cpnbytes < 0)
      return; //UnicodeDecodeError

    nbytes += cpnbytes;
    nchars++;
    p += cpnbytes;

  }
}

void $str_serialize($str str,$Serial$state state) {
  int nWords = str->nbytes/sizeof($WORD) + 1; // # $WORDS needed to store str->str, including terminating 0.
  $ROW row = $add_header(STR_ID,2+nWords,state);
  long nbytes = (long)str->nbytes;                    // We could pack nbytes and nchars in one $WORD, 
  memcpy(row->blob,&nbytes,sizeof($WORD));// but we should think of a better, general approach.
  long nchars = (long)str->nchars;
  memcpy(row->blob+1,&nchars,sizeof($WORD));
  memcpy(row->blob+2,str->str,nbytes+1);
}

$str $str_deserialize($Serial$state state) {
  $ROW this = state->row;
  state->row =this->next;
  state->row_no++;
  $str res = malloc(sizeof(struct $str));
  long nbytes;
  memcpy(&nbytes,this->blob,sizeof($WORD));
  res->$class = &$str$methods;
  res->nbytes = (int)nbytes;
  long nchars;
  memcpy(&nchars,this->blob+1,sizeof($WORD));
  res->nchars = (int)nchars;
  res->str = malloc(nbytes+1);
  memcpy(res->str,this->blob+2,nbytes+1);
  return res;
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
    RAISE(($BaseException)$NEW($ValueError,from$UTF8("center: fill string not single char")));
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

$bool $str_endswith($str s, $str sub, $int start, $int end) {
  $int st = start;
  $int en = end;
  if (fix_start_end(s->nchars,&st,&en) < 0) return $false;
  int isascii = s->nchars==s->nbytes;
  unsigned char *p = skip_chars(s->str + s->nbytes,from$int(en) - s->nchars,isascii) - sub->nbytes;
  unsigned char *q = sub->str;
  for (int i=0; i<sub->nbytes; i++) {
    if (*p == 0 || *p++ != *q++) {
      return $false;
    }
  }
  return $true;
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
  unsigned char *p = skip_chars(s->str,from$int(st),isascii);
  unsigned char *q = skip_chars(p,from$int(en)-from$int(st),isascii);
  int n = bmh(p,sub->str,q-p,sub->nbytes);
  if (n<0) return to$int(-1);
  return to$int(char_no(s,n+p-s->str));
}

$int $str_index($str s, $str sub, $int start, $int end) {
  $int n = $str_find(s,sub,start,end);
  if (from$int(n)<0) {
    RAISE(($BaseException)$NEW($ValueError,from$UTF8("index: substring not found")));
  }
  return n;
}

$bool $str_isalnum($str s) {
  unsigned char *p = s->str;
  int codepoint;
  int nbytes;
  if (s->nchars == 0)
    return $false;
  for (int i=0; i < s->nchars; i++) {
    nbytes = utf8proc_iterate(p,-1,&codepoint);
    utf8proc_category_t cat = utf8proc_category(codepoint);
    if ((cat <  UTF8PROC_CATEGORY_LU || cat >  UTF8PROC_CATEGORY_LO) && cat != UTF8PROC_CATEGORY_ND)
      return $false;
    p += nbytes;
  }
  return $true;
}

$bool $str_isalpha($str s) {
  unsigned char *p = s->str;
  int codepoint;
  int nbytes;
  if (s->nchars == 0)
    return $false;
  for (int i=0; i < s->nchars; i++) {
    nbytes = utf8proc_iterate(p,-1,&codepoint);
    utf8proc_category_t cat = utf8proc_category(codepoint);
    if (cat <  UTF8PROC_CATEGORY_LU || cat >  UTF8PROC_CATEGORY_LO)
      return $false;
    p += nbytes;
  }
  return $true;
}

$bool $str_isascii($str s) {
  unsigned char *p = s->str;
  for (int i=0; i < s->nbytes; i++) {
    if (*p > 127)
      return $false;
    p++;
  }
  return $true;
}

$bool $str_isdecimal($str s) {
  unsigned char *p = s->str;
  int codepoint;
  int nbytes;
  if (s->nchars == 0)
    return $false;
  for (int i=0; i < s->nchars; i++) {
    nbytes = utf8proc_iterate(p,-1,&codepoint);
    utf8proc_category_t cat = utf8proc_category(codepoint);
    if (cat != UTF8PROC_CATEGORY_ND)
      return $false;
    p += nbytes;
  }
  return $true;
}

$bool $str_islower($str s) {
  unsigned char *p = s->str;
  int codepoint;
  int nbytes;
  int has_cased = 0;
  if (s->nchars == 0)
    return $false;
  for (int i=0; i < s->nchars; i++) {
    nbytes = utf8proc_iterate(p,-1,&codepoint);
    utf8proc_category_t cat = utf8proc_category(codepoint);
    if (cat == UTF8PROC_CATEGORY_LT|| cat == UTF8PROC_CATEGORY_LU)
      return $false;
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
    return $false;
  for (int i=0; i < s->nchars; i++) {
    nbytes = utf8proc_iterate(p,-1,&codepoint);
    utf8proc_category_t cat = utf8proc_category(codepoint);
    if (cat >= UTF8PROC_CATEGORY_ZS && codepoint != 0x20)
      return $false;
    p += nbytes;
  }
  return $true;
}

$bool $str_isspace($str s) {
  unsigned char *p = s->str;
  int codepoint;
  int nbytes;
  if (s->nchars == 0)
    return $false;
  for (int i=0; i < s->nchars; i++) {
    nbytes = utf8proc_iterate(p,-1,&codepoint);
    if (!isspace_codepoint(codepoint))
      return $false;
    p += nbytes;
  }
  return $true;
}

$bool $str_istitle($str s) {
  unsigned char *p = s->str;
  int codepoint;
  int nbytes;
  int hascased = 0;
  int incasedrun = 0;
  if (s->nchars == 0)
    return $false;
  for (int i=0; i < s->nchars; i++) {
    nbytes = utf8proc_iterate(p,-1,&codepoint);
    utf8proc_category_t cat = utf8proc_category(codepoint);
    if (cat == UTF8PROC_CATEGORY_LU || cat == UTF8PROC_CATEGORY_LT ) {
      hascased = 1;
      if (incasedrun)
        return $false;
      incasedrun = 1;
    } else if (cat == UTF8PROC_CATEGORY_LL) {
      hascased = 1;
      if (!incasedrun)
        return $false;
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
    return $false;
  for (int i=0; i < s->nchars; i++) {
    nbytes = utf8proc_iterate(p,-1,&codepoint);
    utf8proc_category_t cat = utf8proc_category(codepoint);
    if (cat == UTF8PROC_CATEGORY_LL)
      return $false;
    if (cat == UTF8PROC_CATEGORY_LU || cat == UTF8PROC_CATEGORY_LT)
      hascased = 1;
    p += nbytes;
  }
  return to$bool(hascased);
}

$str $str_join($str s, $Iterable$opaque it) {
  $Iterator iter = it->proto->$class->__iter__(it->proto,it->impl);
  int len = 0;
  int totchars = 0;
  int totbytes = 0;
  $str nxt;
  while ((nxt = ($str)iter->$class->__next__(iter))) {
    len ++;
    totchars += nxt->nchars;
    totbytes += nxt->nbytes;
  }
  if (len > 1) {
    totchars += (len-1) * s->nchars;
    totbytes += (len-1) * s->nbytes;
  }
  $str res;
  NEW_UNFILLED(res,totchars,totbytes);
  if (len > 0) {
    unsigned char *p = res->str;
    iter = it->proto->$class->__iter__(it->proto,it->impl);
    nxt = ($str)iter->$class->__next__(iter);
    memcpy(p,nxt->str,nxt->nbytes);
    p += nxt->nbytes;
    while ((nxt = ($str)iter->$class->__next__(iter))) {
      memcpy(p,s->str,s->nbytes);
      p += s->nbytes;
      memcpy(p,nxt->str,nxt->nbytes);
      p += nxt->nbytes;
    }
  }
  return res;
}

$str $str_ljust($str s, int width, $str fill) {
  if (fill->nchars != 1) {
    RAISE(($BaseException)$NEW($ValueError,from$UTF8("ljust: fill str not single char")));
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
  int n = from$int($str_find(s,sep,NULL,NULL));
  if (n<0) {
    *ls = s; *ssep = null_str; *rs = null_str;
  } else {
    int nb = bmh(s->str,sep->str,s->nbytes,sep->nbytes);
    $str res1;
    NEW_UNFILLED(res1,n,nb);
    memcpy(res1->str,s->str,nb);
    *ls = res1;
    $str res2;
    int nbr = s->nbytes - sep->nbytes - nb;
    NEW_UNFILLED(res2,s->nchars-n-sep->nchars,nbr);
    memcpy(res2->str,s->str+nb+sep->nbytes,nbr);
    *rs = res2;
    *ssep = sep;
  }
}

$str $str_replace($str s, $str old, $str new, $int count) {
  if (count==NULL)
    count = to$int(INT_MAX);
  int c = from$int($str_count(s,old,NULL,NULL));
  int c0 = from$int(count) < c ? from$int(count) : c;
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
  unsigned char *p = skip_chars(s->str,from$int(st),isascii);
  unsigned char *q = skip_chars(p,from$int(en)-from$int(st),isascii);
  int n = rbmh(p,sub->str,q-p,sub->nbytes);
  if (n<0) return to$int(-1);
  return to$int(char_no(s,n+p-s->str));
}


$int $str_rindex($str s, $str sub, $int start, $int end) {
  $int n = $str_rfind(s,sub,start,end);
  if (from$int(n)<0) {
    RAISE(($BaseException)$NEW($ValueError,from$UTF8("rindex: substring not found")));
  };
  return n;
}

$str $str_rjust($str s, int width, $str fill) {
  if (fill->nchars != 1) {
    RAISE(($BaseException)$NEW($ValueError,from$UTF8("rjust: fill string not single char")));
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
  int n = from$int($str_rfind(s,sep,NULL,NULL));
  if (n<0) {
    *ls = null_str; *ssep = null_str; *rs = s;
  } else {
    int nb = rbmh(s->str,sep->str,s->nbytes,sep->nbytes);
    $str res1;
    NEW_UNFILLED(res1,n,nb);
    memcpy(res1->str,s->str,nb);
    *ls = res1;
    int nbr = s->nbytes - sep->nbytes - nb;
    $str res2;    
    NEW_UNFILLED(res2,s->nchars-n-sep->nchars,nbr);
    memcpy(res2->str,s->str+nb+sep->nbytes,nbr);
    *rs = res2;
    *ssep = sep;
  }
}


$list $str_split($str s, $str sep, $int maxsplit) {
  $list res = $list_fromiter(NULL);
  if (maxsplit == NULL || from$int(maxsplit) < 0) maxsplit = to$int(INT_MAX); 
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
          if ($list_len(res) == from$int(maxsplit))
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
    RAISE(($BaseException)$NEW($ValueError,from$UTF8("split: separator is empty string")));
    }
    if (remaining==0) { // for some unfathomable reason, this is the behaviour of the Python method
      $list_append(res,null_str);
      return res;
    }
    $str ls, rs, ssep;
    rs = s;
    // Note: This builds many intermediate rs strings...
    while (rs->nchars>0 && $list_len(res) < from$int(maxsplit)) {
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
    return res;
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
  unsigned char *p = skip_chars(s->str,from$int(st),isascii);
  unsigned char *q = sub->str;
  for (int i=0; i<sub->nbytes; i++) {
    if (*p == 0 || *p++ != *q++) {
      return $false;
    }
  }
  return $true;
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
 
