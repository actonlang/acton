// General methods //////////////////////////////////////////////////////////////////////////

void $list_init($list lst, $Sequence$opaque seq) {
  int capacity = 4;
  lst->data = malloc(capacity*sizeof($WORD));
  if (lst->data == NULL) {
    RAISE(($BaseException)$NEW($MemoryError,to$str("memory allocation failed")));
  }
  lst->length = 0;
  lst->capacity = capacity;
  if (seq) {
    $Iterator iter = seq->proto->w$Collection$Sequence->$class->__iter__(seq->proto->w$Collection$Sequence,seq->impl);
    $WORD nxt;
    while((nxt = iter->$class->__next__(iter))) {
      $list_append(lst,nxt);
    }
  }
}  
  
$bool $list_bool($list self) {
  return to$bool(self->length>0);
}

$str $list_str($list self) {
  $list s2 = $list_new(self->length);
  for (int i=0; i< self->length; i++) {
    $struct elem = ($struct)self->data[i];
    $list_append(s2,elem->$class->__str__(elem));
  }
  return $str_join_par('[',s2,']');
}

void $list_serialize($list self,$Serial$state state) {
  $int prevkey = ($int)$dict_get(state->done,($Hashable)$Hashable$WORD$witness,self,NULL);
  if (prevkey) {
    $val_serialize(-LIST_ID,&prevkey->val,state);
    return;
  }
  $dict_setitem(state->done,($Hashable)$Hashable$WORD$witness,self,to$int(state->row_no));
  long len = (long)self->length;
  $val_serialize(LIST_ID,&len,state);
  for (int i=0; i<self->length; i++) {
    $step_serialize(self->data[i],state);
  }
}
 
$list $list_deserialize($Serial$state state) {
  $ROW this = state->row;
  state->row = this->next;
  state->row_no++;
  if (this->class_id < 0) {
    return ($list)$dict_get(state->done,($Hashable)$Hashable$int$witness,to$int((long)this->blob[0]),NULL);
  } else {
    $list res = $list_new((int)(long)this->blob[0]);
    $dict_setitem(state->done,($Hashable)$Hashable$int$witness,to$int(state->row_no-1),res);
    res->length = res->capacity;
    for (int i = 0; i < res->length; i++) 
      res->data[i] = $step_deserialize(state);
    return res;
  }
}
     
    
struct $list$class $list$methods = {"",UNASSIGNED,($Super$class)&$object$methods, $list_init, $list_serialize,$list_deserialize, $list_bool, $list_str, $list_copy};
 

// Auxiliary functions /////////////////////////////////////////////////////////////////////////////////////////////////////
 

//prints a $list[$int]
/* void $printlist($list lst) { */
/*   $WORD w; */
/*   printf("["); */
/*   for (int i=0; i < $list_len(lst)-1; i++) { */
/*     w = $list_getitem(lst,i); */
/*     printf("%ld, ",from$int(w)); */
/*   } */
/*   if ($list_len(lst) > 0) { */
/*     w = $list_getitem(lst,$list_len(lst)-1); */
/*     printf("%ld",from$int(w)); */
/*   } */
/*   printf("]\n"); */
/* } */

static inline int min(int a, int b) {
    if (a > b)
        return b;
    return a;
}

static inline int max(int a, int b) {
    if (a > b)
        return a;
    return b;
}

// For now, expansion doubles capacity. 
static void expand($list lst,int n) {
   if (lst->capacity >= lst->length + n)
     return;
   int newcapacity = lst->capacity==0 ? 1 : lst->capacity;
   while (newcapacity < lst->length+n)
     newcapacity <<= 1;
   $WORD* newptr = lst->data==NULL
     ? malloc(newcapacity*sizeof($WORD))
     : realloc(lst->data,newcapacity*sizeof($WORD));
   if (newptr == NULL) {
    RAISE(($BaseException)$NEW($MemoryError,to$str("memory allocation failed")));
   }
   lst->data = newptr;
   lst->capacity = newcapacity;
}  

$list $list_new(int capacity) {
  if (capacity < 0) {
    fprintf(stderr,"Internal error list_new: negative capacity");
    exit(-1);
  } 
  $list lst = malloc(sizeof(struct $list));
  if (lst == NULL) {
     RAISE(($BaseException)$NEW($MemoryError,to$str("memory allocation failed")));
  }
  if (capacity>0) {
    lst->data = malloc(capacity*sizeof($WORD));
    if (lst->data == NULL) {
       RAISE(($BaseException)$NEW($MemoryError,to$str("memory allocation failed")));
    }
  } else {
    lst->data = NULL;
  }
  lst->length = 0;
  lst->capacity = capacity;
  lst->$class = &$list$methods; 
  return lst;
}

// Plus /////////////////////////////////////////////////////////////////////////////////////////////

$list $list_add($list lst, $list other) {
  int lstlen = lst->length;
  int otherlen = other->length;
  int reslen = lstlen + otherlen;
  $list res = $list_new(reslen);
  memcpy(res->data,lst->data,lstlen*sizeof($WORD));
  memcpy(res->data+lstlen,other->data,otherlen*sizeof($WORD));
  res->length = reslen;
  return res;
}
 
// Collection ///////////////////////////////////////////////////////////////////////////////////////

$list $list_fromiter($Iterable$opaque iter) {
  $list res = $list_new(4);
  if (iter) {
    $Iterator it = iter->proto->$class->__iter__(iter->proto,iter->impl);
    $WORD nxt;
    while ((nxt = it->$class->__next__(it))) {
      $list_append(res,nxt);
    }
  }
  return res;
}

long $list_len($list lst) {
  return (long)lst->length;
}

// Container ///////////////////////////////////////////////////////////////////////////

int $list_contains($Eq w, $list lst, $WORD elem) {
  for (int i=0; i < lst->length; i++) {
    if (from$bool(w->$class->__eq__(w,elem,lst->data[i])))
      return 1;
  }
  return 0;
}

int $list_containsnot($Eq w, $list lst, $WORD elem) {
  return !$list_contains(w,lst,elem);
}

// Iterable ///////////////////////////////////////////////////////////////////////////


static $WORD $Iterator$list_next($Iterator$list self) {
  return self->nxt >= self->src->length ? NULL : self->src->data[self->nxt++];
}

void $Iterator$list_init($Iterator$list self, $list lst) {
  self->src = lst;
  self->nxt = 0;
}

$bool $Iterator$list_bool($Iterator$list self) {
  return $True;
}

$str $Iterator$list_str($Iterator$list self) {
  char *s;
  asprintf(&s,"<list iterator object at %p>",self);
  return to$str(s);
}

void $Iterator$list_serialize($Iterator$list self,$Serial$state state) {
  $step_serialize(self->src,state);
  $step_serialize(to$int(self->nxt),state);
}

$Iterator$list $Iterator$list$_deserialize($Serial$state state) {
   $Iterator$list res = $DNEW($Iterator$list,state);
   res->src = ($list)$step_deserialize(state);
   res->nxt = from$int(($int)$step_deserialize(state));
   return res;
}

struct $Iterator$list$class $Iterator$list$methods = {"",UNASSIGNED,($Super$class)&$Iterator$methods, $Iterator$list_init,
                                                      $Iterator$list_serialize, $Iterator$list$_deserialize,$Iterator$list_bool,$Iterator$list_str,$Iterator$list_next};

$Iterator $list_iter($list lst) {
  return ($Iterator)$NEW($Iterator$list,lst);
}

// Indexed ///////////////////////////////////////////////////////////////////////////

$WORD $list_getitem($list lst, int ix) {
  int len = lst->length;
  int ix0 = ix < 0 ? len + ix : ix;
  if (ix0 < 0 || ix0 >= len) {
    RAISE(($BaseException)$NEW($IndexError,to$str("getitem: indexing outside list")));
  }
  return lst->data[ix0];
}

void $list_setitem($list lst, int ix, $WORD val) {
  int len = lst->length;
  int ix0 = ix < 0 ? len + ix : ix;
  if (ix0 < 0 || ix0 >= len) {
    RAISE(($BaseException)$NEW($IndexError,to$str("setitem: indexing outside list")));
  }
  lst->data[ix0] = val;
}

void $list_delitem($list lst,int ix) {
  int len = lst->length;
  int ix0 = ix < 0 ? len + ix : ix;
  if(ix0 < 0 || ix0 >= len) {
    RAISE(($BaseException)$NEW($IndexError,to$str("delitem: indexing outside list")));
  }
  memmove(lst->data + ix0,
          lst->data + (ix0 + 1),
          (len-(ix0+1))*sizeof($WORD));
  lst->length--;
}
 

// Sliceable //////////////////////////////////////////////////////////////////////////////////////

$list $list_getslice($list lst, $Slice slc) {
  int len = lst->length;
  int start, stop, step, slen;
  normalize_slice(slc, len, &slen, &start, &stop, &step);
  //slice notation have been eliminated and default values applied.
  // slen now is the length of the slice
  $list rlst = $list_new(slen);
  int t = start;
  for (int i=0; i<slen; i++) {
    $WORD w;
    w = $list_getitem(lst,t);
    $list_append(rlst,w);
    t += step;
  }
  return rlst;
}

void $list_setslice($list lst, $Slice slc, $Iterator it) {
  int len = lst->length;
  $list other = $list_new(0);
  $WORD w;
  while((w=it->$class->__next__(it)))
    $list_append(other,w);
  int olen = other->length; 
  int start, stop, step, slen;
  normalize_slice(slc, len, &slen, &start, &stop, &step);
  if (step != 1 && olen != slen) {
    RAISE(($BaseException)$NEW($ValueError,to$str("setslice: illegal slice")));
  }
  int copy = olen <= slen ? olen : slen;
  int t = start;
  for (int i= 0; i<copy; i++) {
    lst->data[t] = other->data[i];
    t += step;
  }
  if (olen == slen)
    return;
  // now we know that step=1
  if (olen < slen) {
    memmove(lst->data + start + copy,
            lst->data + start + slen,
            (len-(start+slen))*sizeof($WORD));
     lst->length-=slen-olen;
     return;
  } else {
    expand(lst,olen-slen);
    int rest = len - (start+copy);
    int incr = olen - slen;
    memmove(lst->data + start + copy + incr,
            lst->data + start + copy,
            rest*sizeof($WORD));
    for (int i = copy; i < olen; i++)
      lst->data[start+i] = other->data[i];
    lst->length += incr;
  }
}

void $list_delslice($list lst, $Slice slc) {
  int len = lst->length;
  int start, stop, step, slen;
  normalize_slice(slc, len, &len, &start, &stop, &step);
  if (slen==0) return;
  for (int ix = start+step*(slen-1); ix>= start; ix -= step)
    $list_delitem(lst,ix);
}

// Sequence /////////////////////////////////////////////////////////////////////////////

void $list_append($list lst, $WORD val) {
  expand(lst,1);
  lst->data[lst->length++] = val;
}

static $WORD $Iterator$list_reversed_next($Iterator$list self) {
  return self->nxt < 0 ? NULL : self->src->data[self->nxt--];
}

$Iterator $list_reversed($list lst){
  $list copy = $list_copy(lst);
  $list_reverse(copy);
  return $list_iter(copy);
}

void $list_insert($list lst, int ix, $WORD val) {
  int len = lst->length;
  expand(lst,1);
  int ix0 = ix < 0 ? max(len+ix,0) : min(ix,len);
  memmove(lst->data + (ix0 + 1),
          lst->data + ix0 ,
          (len - ix0) * sizeof($WORD));
  lst->data[ix0] = val;
  lst->length++;
}

// In place reversal
void $list_reverse($list lst) {
  int len = lst->length;
  for (int i = 0; i < len/2; i++) {
    $WORD tmp = lst->data[i];
    lst->data[i] = lst->data[len-1-i];
    lst->data[len-1-i] = tmp;
  }
}
 
// List-specific methods /////////////////////////////////////////////////////////////////////

$list $list_copy($list lst) {
  int len = lst->length;
  $list res = $list_new(len);
  res->length = len;
  memcpy(res->data,lst->data,len*sizeof($WORD));
  return res;
}
/*                   
int list_sort(list_t lst, int (*cmp)(WORD,WORD)) {
  return heapsort(lst->data, lst->length, sizeof(WORD), cmp);
}
*/
 
