
// auxiliaries ////////////////////////////////////////////////////////////////////////////

#define NEW_UNFILLED_BYTEARRAY(nm,nbtes)         \
nm = malloc(sizeof(struct $bytearray)); \
(nm)->$class = &$bytearray$methods; \
(nm)->nbytes = nbtes;            \
(nm)->str = malloc((nm)->nbytes + 1);    \
(nm)->str[(nm)->nbytes] = 0



// bytearray //////////////////////////////////////////////////////////////////////////////



$NoneType $bytearray_setitem($bytearray b, $int n, $int v) {
  b->str[n->val] = (unsigned char)(v->val & 0xff);
  return $None;
}

// General methods

void $bytearray_init($bytearray, $Sequence$opaque);
void $bytearray_serialize($bytearray,$Serial$state);
$bytearray $bytearray_deserialize($Serial$state);
$bool $bytearray_bool($bytearray);
$str $bytearray_str($bytearray);

void $bytearray_init($bytearray self, $Sequence$opaque s) {
  if (!s) {
    self->nbytes = 0;
    self->str = NULL;
    return;
  }
  $Collection wit = s->proto->w$Collection$Sequence;
  self->nbytes = wit->$class->__len__(wit,s->impl)->val;
  self->str = malloc(self->nbytes+1);
  
  $Iterator iter = wit->$class->__iter__(wit,s->impl);
  for (int i=0; i<self->nbytes; i++)
    $bytearray_setitem(self,to$int(i),iter->$class->__next__(iter));
  self->str[self->nbytes] = 0;
}


$bool $bytearray_bool($bytearray s) {
  return to$bool(s->nbytes > 0);
};

$str $bytearray_str($bytearray s) {
  $str bs;
  NEW_UNFILLED_STR(bs,s->nbytes,s->nbytes);
  bs->str = s->str;        // bs may not be a correctly UTF8-encoded string
  $str as = $ascii(bs);    // but we can use $ascii on it anyhow.
  $str res;
  int n = as->nbytes + 14; // "bytearray(b'" + "')"
  NEW_UNFILLED_STR(res,n,n);
  memcpy(res->str, "bytearray(b'",12);
  memcpy(&res->str[12],as->str,as->nbytes);
  memcpy(&res->str[n-2],"')",2);
  return res;
}


void $bytearray_serialize($bytearray str,$Serial$state state) {
  int nWords = str->nbytes/sizeof($WORD) + 1;         // # $WORDS needed to store str->str, including terminating 0.
  $ROW row = $add_header(BYTEARRAY_ID,1+nWords,state);
  long nbytes = (long)str->nbytes;                    
  memcpy(row->blob,&nbytes,sizeof($WORD));            
  memcpy(row->blob+1,str->str,nbytes+1);
}

$bytearray $bytearray_deserialize($Serial$state state) {
  $ROW this = state->row;
  state->row =this->next;
  state->row_no++;
  $bytearray res = malloc(sizeof(struct $bytearray));
  long nbytes;
  memcpy(&nbytes,this->blob,sizeof($WORD));
  res->$class = &$bytearray$methods;
  res->nbytes = (int)nbytes;
  res->str = malloc(nbytes+1);
  memcpy(res->str,this->blob+1,nbytes+1);
  return res;
}

// Eq ////////////////////////////////////////////////////////////////////////////////////////////

int $bytearray_eq($bytearray a, $bytearray b) {
  return (a->nbytes==b->nbytes && !strcmp((char *)a->str,(char *)b->str));
}
         
int $bytearray_neq($bytearray a, $bytearray b) {
  return !$bytearray_eq(a,b);
}

// Ord ///////////////////////////////////////////////////////////////////////////////////////////

int $bytearray_lt($bytearray a, $bytearray b) {
  return (strcmp((char *)a->str,(char *)b->str) < 0);
}
 
int $bytearray_le($bytearray a, $bytearray b) {
  return (strcmp((char *)a->str,(char *)b->str) <= 0);
}
 
int $bytearray_gt($bytearray a, $bytearray b) {
  return (strcmp((char *)a->str,(char *)b->str) > 0);
}
 
int $bytearray_ge($bytearray a, $bytearray b) {
  return (strcmp((char *)a->str,(char *)b->str) >= 0);
}

// $Plus /////////////////////////////////////////////////////////////////////////////////////////////

$bytearray $bytearray_add($bytearray s, $bytearray t) {
  $bytearray res;
  NEW_UNFILLED_BYTEARRAY(res,s->nbytes + t->nbytes);
  memcpy(res->str,s->str,s->nbytes);
  memcpy(res->str+s->nbytes,t->str,t->nbytes);
  return res;
}

// Collection ///////////////////////////////////////////////////////////////////////////////////////
         
$int $bytearray_len($bytearray s) {
  $int res = to$int(s->nbytes);
  return res;
}

// $Container ///////////////////////////////////////////////////////////////////////////

int $bytearray_contains($bytearray s, $int elem) {
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

$bool $Iterator$str_bool($Iterator$str self) {
  return $True;
}

$str $Iterator$str_str($Iterator$str self) {
  char *s;
  asprintf(&s,"<str iterator object at %p>",self);
  return to$str(s);
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

struct $Iterator$str$class $Iterator$str$methods = {"",UNASSIGNED,($Super$class)&$Iterator$methods, $Iterator$str_init, $Iterator$str_serialize, $Iterator$str$_deserialize,
                                                      $Iterator$str_bool, $Iterator$str_str, $Iterator$str_next};


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
  NEW_UNFILLED_STR(res,slen,nbytes);
  if (nbytes > 0)
    memcpy(res->str,buffer,nbytes);
 return res;
}

// bytearray methods ///////////////////////////////////////////////////////////////////////

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
$bytearray $bytearray_join($bytearray sep, $Iterable$opaque it);
$bytearray $bytearray_ljust($bytearray s, $int width, $bytearray fill); 
$bytearray $bytearray_lower($bytearray s);
$bytearray $bytearray_lstrip($bytearray s,$bytearray cs); 
$tuple $bytearray_partition($bytearray s, $bytearray sep);
$bytearray $bytearray_replace($bytearray s, $bytearray old, $bytearray new, $int count);
$int $bytearray_rfind($bytearray s, $bytearray sub, $int start, $int end);
$int $bytearray_rindex($bytearray s, $bytearray sub, $int start, $int end);
$bytearray $bytearray_rjust($bytearray s, $int width, $bytearray fill);  
$tuple $bytearray_rpartition($bytearray s, $bytearray sep); 
$bytearray $bytearray_rstrip($bytearray s,$bytearray cs);
$list $bytearray_split($bytearray s, $bytearray sep, $int maxsplit);  
$list $bytearray_splitlines($bytearray s, $bool keepends); 
$bool $bytearray_startswith($bytearray s, $bytearray prefix, $int start, $int end); 
$bytearray $bytearray_strip($bytearray s,$bytearray cs);
$bytearray $bytearray_upper($bytearray s);
$bytearray $bytearray_zfill($bytearray s, $int width);

struct $bytearray$class $bytearray$methods =
  {"",UNASSIGNED,($Super$class)&$struct$methods, $bytearray_init, $bytearray_serialize, $bytearray_deserialize, $bytearray_bool,
   $bytearray_str, $bytearray_capitalize, $bytearray_center, $bytearray_count,  $bytearray_decode, $bytearray_endswith,
   $bytearray_expandtabs, $bytearray_find, $bytearray_index,
   $bytearray_isalnum, $bytearray_isalpha, $bytearray_isascii, $bytearray_isdigit, $bytearray_islower, $bytearray_isspace,
   $bytearray_istitle, $bytearray_isupper, $bytearray_join, $bytearray_ljust, $bytearray_lower, $bytearray_lstrip, $bytearray_partition, $bytearray_replace,
   $bytearray_rfind, $bytearray_rindex, $bytearray_rjust,
   $bytearray_rpartition, $bytearray_rstrip, $bytearray_split, $bytearray_splitlines, $bytearray_startswith, $bytearray_strip, $bytearray_upper, $bytearray_zfill};

$bytearray $bytearray$new($Sequence$opaque s) {
    return $NEW($bytearray, s);
}


// Implementations of protocol methods for bytearray

int $bytearray_eq($bytearray,$bytearray) {
int $bytearray_neq($bytearray,$bytearray);
int $bytearray_lt($bytearray,$bytearray);
int $bytearray_le($bytearray,$bytearray);
int $bytearray_gt($bytearray,$bytearray);
int $bytearray_ge($bytearray,$bytearray);

$Iterator $bytearray_iter($bytearray);

$bytearray $bytearray_fromiter($Iterable$opaque);
$int $bytearray_len($bytearray bytearray);

int $bytearray_contains ($bytearray, $int);
int $bytearray_containsnot ($bytearray, $int);

int $bytearray_getitem($bytearray, int);
void $bytearray_setitem($bytearray,int,$int);
void $bytearray_delitem($bytearray,int);

$bytearray $bytearray_getslice($bytearray, $Slice);
void $bytearray_setslice($bytearray, $Slice, $Iterable$opaque);
void $bytearray_delslice($bytearray, $Slice);

void $bytearray_append($bytearray, int);
$Iterator $bytearray_reversed($bytearray);
void $bytearray_insert($bytearray, int, $WORD);
void $bytearray_reverse($bytearray);

 
$str  $str_add($str, $str);

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

$str $Container$str$__fromiter__ ($Container$str wit, $Iterable$opaque iter) {
  return $str_join(to$str(""),iter);
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

void $Sliceable$str$__setitem__ ($Sliceable$str wit, $str str, $int i, $str val) {
  fprintf(stderr,"Internal error: call to mutating method setitem on string");
  exit(-1);
}

void $Sliceable$str$__delitem__ ($Sliceable$str wit, $str str, $int i) {
  fprintf(stderr,"Internal error: call to mutating method delitem on string");
  exit(-1);
}

$str $Sliceable$str$__getslice__ ($Sliceable$str wit, $str str, $Slice slc) {
  return $str_getslice(str,slc);
}

void $Sliceable$str$__setslice__ ($Sliceable$str wit, $str str, $Slice slc, $Iterable$opaque it) {
  fprintf(stderr,"Internal error: call to mutating method setslice on string");
  exit(-1);
}

void $Sliceable$str$__delslice__ ($Sliceable$str wit, $str str, $Slice slc) {
  fprintf(stderr,"Internal error: call to mutating method delslice on string");
  exit(-1);
}

$str $Plus$str$__add__ ($Plus$str wit, $str a, $str b) {
  return $str_add(a,b);
}

static $bytearray $bytearray_copy($bytearray s) {
  $bytearray res;
  NEW_UNFILLED_BYTEARRAY(res,s->nbytes);
  res->nbytes = s->nbytes;
  memcpy(res->str,s->str,s->nbytes);
  return res;
}

$bytearray $bytearray_capitalize($bytearray s) {
  if (s->nbytes==0) {
    return null_bytearray;
  }
  $bytearray res;
  NEW_UNFILLED_BYTEARRAY(res,s->nbytes);
  for (int i=0; i<s->nbytes; i++) 
    res->str[i] = i==0 ? toupper(s->str[i]) : tolower(s->str[i]);
   return res;
}

$bytearray $bytearray_center($bytearray s, $int width, $bytearray fill) {
  if (!fill) fill = space_bytearray;
  if (fill->nbytes != 1) {
    RAISE(($BaseException)$NEW($ValueError,to$str("center: fill bytearray not single char")));
  }
  if (width->val <= s->nbytes) {
    return $bytearray_copy(s);
  }
  int pad = (width->val-s->nbytes);
  int padleft = pad/2; 
  int padright = pad-padleft;
  int sbytes = s->nbytes;
  $bytearray res;
  NEW_UNFILLED_BYTEARRAY(res, width->val);
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


$int $bytearray_count($bytearray s, $bytearray sub, $int start, $int end) {
  $int st = start;
  $int en = end;
  if (fix_start_end(s->nbytes,&st,&en) < 0) return to$int(0);
  unsigned char *p = &s->str[st->val];
  unsigned char *q = &p[en->val-st->val];
  int res = 0;
  int n = bmh(p,sub->str,q-p,sub->nbytes);
  while (n>=0) {
    res++;
    p += n + (sub->nbytes>0 ? sub->nbytes : 1);
    n = bmh(p,sub->str,q-p,sub->nbytes);
  }
  return to$int(res);
}

$str $bytearray_decode($bytearray s) {
  return to$str((char*)s->str);
}

$bool $bytearray_endswith($bytearray s, $bytearray sub, $int start, $int end) {
  $int st = start;
  $int en = end;
  if (fix_start_end(s->nbytes,&st,&en) < 0) return $False;
  unsigned char *p = &s->str[en->val-sub->nbytes];
  unsigned char *q = sub->str;
  for (int i=0; i<sub->nbytes; i++) {
    if (*p == 0 || *p++ != *q++) {
      return $False;
    }
  }
  return $True;
}

$bytearray $bytearray_expandtabs($bytearray s, $int tabsz){
  int pos = 0;
  int expanded = 0;
  int tabsize = tabsz->val;
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
  $bytearray res;
  NEW_UNFILLED_BYTEARRAY(res,s->nbytes+expanded);
  memcpy(res->str,buffer,s->nbytes+expanded);
  return res;
}

$int $bytearray_find($bytearray s, $bytearray sub, $int start, $int end) {
  $int st = start;
  $int en = end;
  if (fix_start_end(s->nbytes,&st,&en) < 0) return to$int(-1);
  unsigned char *p = &s->str[st->val];
  unsigned char *q = &s->str[en->val];
  int n = bmh(p,sub->str,q-p,sub->nbytes);
  if (n<0) return to$int(-1);
  return to$int(n+p-s->str);
}


$int $bytearray_index($bytearray s, $bytearray sub, $int start, $int end) {
  $int n = $bytearray_find(s,sub,start,end);
  if (n->val<0) {
    RAISE(($BaseException)$NEW($ValueError,to$str("index: substring not found")));
  }
  return n;
}

$bool $bytearray_isalnum($bytearray s) {
  if (s->nbytes==0)
    return $False;
  for (int i=0; i<s->nbytes; i++) {
    unsigned char c = s->str[i];
    if (c < '0' || c > 'z' || (c > '9' && c < 'A') || (c > 'Z' && c < 'a'))
      return $False;
  }
  return $True;
}

$bool $bytearray_isalpha($bytearray s) {
  if (s->nbytes==0)
    return $False;
  for (int i=0; i<s->nbytes; i++) {
    unsigned char c = s->str[i];
    if (c < 'A' || c > 'z' || (c > 'Z' && c < 'a'))
      return $False;
  }
  return $True;
}

$bool $bytearray_isascii($bytearray s) {
  for (int i=0; i<s->nbytes; i++) {
    unsigned char c = s->str[i];
    if (c > 0x7f)
      return $False;
  }
  return $True;
}

$bool $bytearray_isdigit($bytearray s) {
  if (s->nbytes==0)
    return $False;
  for (int i=0; i<s->nbytes; i++) {
    unsigned char c = s->str[i];
    if (c<'0' || c > '9')
      return $False;
  }
  return $True;
}
 

$bool $bytearray_islower($bytearray s) {
  int has_lower = 0;
  for (int i=0; i < s->nbytes; i++) {
    unsigned char c = s->str[i];
    if (c >= 'A' && c <= 'Z')
      return $False;
    if (c >= 'a' && c <= 'z')
      has_lower = 1;
  }
  return to$bool(has_lower);
}

/*
$bool $bytearray_isprintable($bytearray s) {
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
*/

$bool $bytearray_isspace($bytearray s) {
  if (s->nbytes==0)
    return $False;
  for (int i=0; i<s->nbytes; i++) {
    unsigned char c = s->str[i];
    if (c !=' ' && c != '\t' && c != '\n' && c != '\r' && c != '\x0b' && c != '\f')
      return $False;
  }
  return $True;
}

$bool $bytearray_istitle($bytearray s) {
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

$bool $bytearray_isupper($bytearray s) {
  int has_upper = 0;
  for (int i=0; i < s->nbytes; i++) {
    unsigned char c = s->str[i];
    if (c >= 'a' && c <= 'z')
      return $False;
    if (c >= 'a' && c <= 'z')
      has_upper = 1;
  }
  return to$bool(has_upper);
}

$bytearray $bytearray_join($bytearray s, $Iterable$opaque it) {
  int totbytes = 0;
  $list lst = $list_fromiter(it);
  $bytearray nxt;
  int len = lst->length;
  for (int i=0; i<len; i++) {
    nxt = ($bytearray)lst->data[i];
    totbytes += nxt->nbytes;
  }
  if (len > 1) {
    totbytes += (len-1) * s->nbytes;
  }
  $bytearray res;
  NEW_UNFILLED_BYTEARRAY(res,totbytes);
  if (len > 0) {
    nxt = ($bytearray)lst->data[0];
    unsigned char *p = res->str;
    memcpy(p,nxt->str,nxt->nbytes);
    p += nxt->nbytes;
    for (int i=1; i<len; i++) {
      nxt = ($bytearray)lst->data[i];
      memcpy(p,s->str,s->nbytes);
      p += s->nbytes;
      memcpy(p,nxt->str,nxt->nbytes);
      p += nxt->nbytes;
    }
  }
  return res;
}

$bytearray $bytearray_ljust($bytearray s, $int width, $bytearray fill) {
  if (fill->nbytes != 1) {
    RAISE(($BaseException)$NEW($ValueError,to$str("bytearray ljust: fill array not single char")));
  }
  if (width->val <= s->nbytes) {
    return $bytearray_copy(s);
  }
  $bytearray res;
  NEW_UNFILLED_BYTEARRAY(res,width->val);
  memcpy(res->str,s->str,s->nbytes);
  unsigned char c = fill->str[0];
  for (int i = s->nbytes; i<width->val; i++) {
      res->str[i] = c;
  }
  return res;
}

$bytearray $bytearray_lower($bytearray s) {
  $bytearray res;
  NEW_UNFILLED_BYTEARRAY(res,s->nbytes);
  for (int i=0; i< s->nbytes; i++)
    res->str[i] = tolower(res->str[i]);
  return res;
}

$bytearray $bytearray_lstrip($bytearray s, $bytearray cs) {
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
  $bytearray res;
  NEW_UNFILLED_BYTEARRAY(res,s->nbytes-nstrip);
  memcpy(res->str,s->str+nstrip,res->nbytes);       
  return res;
}


$tuple $bytearray_partition($bytearray s, $bytearray sep) {
  int n = from$int($bytearray_find(s,sep,NULL,NULL));
  if (n<0) {
    return $NEW($tuple,3,s,null_bytearray,null_bytearray);
  } else {
    int nb = bmh(s->str,sep->str,s->nbytes,sep->nbytes);
    $bytearray ls;
    NEW_UNFILLED_BYTEARRAY(ls,nb);
    memcpy(ls->str,s->str,nb);
    $bytearray rs;
    int nbr = s->nbytes - sep->nbytes - nb;
    NEW_UNFILLED_BYTEARRAY(rs,nbr);
    memcpy(rs->str,s->str+nb+sep->nbytes,nbr);
    return $NEW($tuple,3,ls,sep,rs);
  }
}


$bytearray $bytearray_replace($bytearray s, $bytearray old, $bytearray new, $int count) {
  if (count==NULL)
    count = to$int(INT_MAX);
  int c = from$int($bytearray_count(s,old,NULL,NULL));
  int c0 = from$int(count) < c ? from$int(count) : c;
  if (c0==0){
    return $bytearray_copy(s);
  }
  int nbytes = s->nbytes + c0*(new->nbytes-old->nbytes);
  $bytearray res;
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
      

$int $bytearray_rfind($bytearray s, $bytearray sub, $int start, $int end) {
  $int st = start;
  $int en = end;
  if (fix_start_end(s->nbytes,&st,&en) < 0) return to$int(-1);
  unsigned char *p = &s->str[st->val];
  unsigned char *q = &s->str[en->val];
  int n = rbmh(p,sub->str,q-p,sub->nbytes);
  if (n<0) return to$int(-1);
  return to$int(n+p-s->str);
}


$int $bytearray_rindex($bytearray s, $bytearray sub, $int start, $int end) {
  $int n = $bytearray_rfind(s,sub,start,end);
  if (from$int(n)<0) {
    RAISE(($BaseException)$NEW($ValueError,to$str("rindex for bytearray: substring not found")));
  };
  return n;
}

$bytearray $bytearray_rjust($bytearray s, $int width, $bytearray fill) {
  if (fill->nbytes != 1) {
    RAISE(($BaseException)$NEW($ValueError,to$str("rjust: fill string not single char")));
  }
  if (width->val <= s->nbytes) {
    return $bytearray_copy(s);
  }
  int pad = (width->val-s->nbytes);
  $bytearray res;
  NEW_UNFILLED_BYTEARRAY(res,width->val);
  unsigned char c = fill->str[0];
  for (int i = 0; i<pad; i++) {
      res->str[i] = c;
  }
  memcpy(&res->str[pad],s->str,s->nbytes);
  return res;
}
                                
$tuple $bytearray_rpartition($bytearray s, $bytearray sep) {
  int n = from$int($bytearray_rfind(s,sep,NULL,NULL));
  if (n<0) {
    return $NEW($tuple,3,null_str,null_str,s);
  } else {
    int nb = rbmh(s->str,sep->str,s->nbytes,sep->nbytes);
    $bytearray ls;
    NEW_UNFILLED_BYTEARRAY(ls,nb);
    memcpy(ls->str,s->str,nb);
    int nbr = s->nbytes - sep->nbytes - nb;
    $bytearray rs;    
    NEW_UNFILLED_BYTEARRAY(rs,nbr);
    memcpy(rs->str,s->str+nb+sep->nbytes,nbr);
    return  $NEW($tuple,3,ls,sep,rs);
  }
}

$bytearray $bytearray_rstrip($bytearray s, $bytearray cs) {
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
  $bytearray res;
  NEW_UNFILLED_BYTEARRAY(res,s->nbytes-nstrip);
  memcpy(res->str,s->str,res->nbytes);       
  return res;
}
 
$list $bytearray_split($bytearray s, $bytearray sep, $int maxsplit) {
  $list res = $NEW($list,NULL);
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
          if ($list_len(res) == from$int(maxsplit))
            break; // we have now removed leading whitespace in remainder
        } 
      } else {
          if (inword) {
            inword = 0;
            $bytearray word;
            NEW_UNFILLED_BYTEARRAY(word,p-q);
            memcpy(word->str,q,p-q);
            $list_append(res,word);
          }
      }
      p++;
    }
    // this if statement should be simplified; almost code duplication.
    if (p < s->str + s->nbytes) { // we did not break out of the while loop
      if (inword) {
        $bytearray word;
        NEW_UNFILLED_BYTEARRAY(word,p-q);
        memcpy(word->str,q,p-q);
        $list_append(res,word);
      }
    } else {
      $bytearray word;
      p = s->str+s->nbytes;
      NEW_UNFILLED_BYTEARRAY(word,p-q);
      memcpy(word->str,q,p-q);
      $list_append(res,word);
    }
    return res;
  } else { // separator given
    if (sep->nbytes==0) {
    RAISE(($BaseException)$NEW($ValueError,to$str("split for bytearray: separator is empty string")));
    }
    if (s->nbytes==0) { // for some unfathomable reason, this is the behaviour of the Python method
      $list_append(res,null_str);
      return res;
    }
    $bytearray ls, rs, ssep;
    rs = s;
    // Note: This builds many intermediate rs strings...
    while (rs->nbytes>0 && $list_len(res) < from$int(maxsplit)) {
     $tuple t = $bytearray_partition(rs,sep);
     ssep = ($bytearray)t->components[1];
     rs =  ($bytearray)t->components[2];
     $list_append(res,($bytearray)t->components[0]);
    }
    if (ssep->nbytes>0)
      $list_append(res,rs);
    return res;
  }
}

$list $bytearray_splitlines($bytearray s, $bool keepends) {
  if (!keepends)
    keepends = $False;
  $list res = $NEW($list,NULL);
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
      $bytearray line;
      winend = *p=='\r' && *(p+1)=='\n';
      int size = p-q + (keepends->val ? 1 + winend : 0);
      NEW_UNFILLED_BYTEARRAY(line,size);
      memcpy(line->str,q,size);
      p+= 1 + winend;
      q = p;
      $list_append(res,line);
    }
  }
  if (q < p) {
    $bytearray line;
    NEW_UNFILLED_BYTEARRAY(line,p-q);
    memcpy(line->str,q,p-q);
    $list_append(res,line);
  }
  return res;
} 

$bool $bytearray_startswith($bytearray s, $bytearray sub, $int start, $int end) {
  $int st = start;
  $int en = end;
  if (fix_start_end(s->nbytes,&st,&en) < 0) return $False;
  unsigned char *p = s->str + st->val;
  if (p+sub->nbytes >= s->str+s->nbytes) return $False;
  unsigned char *q = sub->str;
  for (int i=0; i<sub->nbytes; i++) {
    if (p >= s->str + en->val || *p++ != *q++) {
      return $False;
    }
  }
  return $True;
}


$bytearray $bytearray_strip($bytearray s, $bytearray cs) {
  return $bytearray_lstrip($bytearray_rstrip(s,cs),cs);
}

$bytearray $bytearray_upper($bytearray s) {
  $bytearray res;
  NEW_UNFILLED_BYTEARRAY(res,s->nbytes);
  for (int i=0; i< s->nbytes; i++)
    res->str[i] = toupper(res->str[i]);
  return res;
}

$bytearray $bytearray_zfill($bytearray s, $int width) {
  int fill = width->val - s->nbytes;
  if (fill < 0)
    return $bytearray_copy(s);
  $bytearray res;
  NEW_UNFILLED_BYTEARRAY(res,width->val);
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

