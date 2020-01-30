#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "list.h"


$list $list_copy($list lst);
$list $list_add($list lst, $list other);
$list $list_fromiter(Iterable it);
$int $list_len($list lst);
$bool $list_contains($list lst, $WORD elem, int (*eq)(Eq$__class__,$WORD,$WORD));
$bool $list_containsnot($list lst, $WORD elem, int (*eq)(Eq$__class__,$WORD,$WORD));
$WORD $list_getitem($list lst, int ix);
void $list_setitem($list lst, int ix, $WORD val);
void $list_delitem($list lst,int ix);
$list $list_getslice($list lst, Slice slc);
void $list_setslice($list lst, Slice slc, $list other);
void $list_delslice($list lst, Slice slc);
void $list_append($list lst, $WORD val);
Iterable $list_reversed($list lst);
void $list_insert($list lst, int ix, $WORD val);
void $list_reverse($list lst);


//  Method tables ///////////////////////////////////////////////////////////////

$WORD $list_add_instance(Plus$__class__ cl, $WORD a, $WORD b);

Iterator $list_iter_instance(Iterable$__class__ cl,$WORD self);

$WORD $list_next_instance(Iterator$__class__ cl, $WORD self);

Collection $list_fromiter_instance(Collection$__class__ cl, Iterable it);
$int $list_len_instance(Collection$__class__ cl, $WORD self);

$WORD $list_getitem_instance(Indexed$__class__ cl, $WORD self, $WORD ix); 
void $list_setitem_instance(Indexed$__class__ cl, $WORD self, $WORD ix, $WORD val);
void $list_delitem_instance(Indexed$__class__ cl, $WORD self, $WORD ix);

$WORD $list_getslice_instance(Sliceable$__class__ cl, $WORD self, Slice slice);
void $list_setslice_instance(Sliceable$__class__ cl, $WORD self, Slice slice, Sequence it); 
void $list_delslice_instance(Sliceable$__class__ cl, $WORD self, Slice slice);

Iterable $list_reversed_instance(Sequence$__class__ cl, $WORD self);
void $list_insert_instance(Sequence$__class__ cl, $WORD self, $int ix, $WORD elem);
void $list_append_instance(Sequence$__class__ cl, $WORD self, $WORD elem);
void $list_reverse_instance(Sequence$__class__ cl, $WORD self);

static struct Plus$__class__ Plus$list_struct = {"GC_Plus",$list_add_instance};
Plus$__class__ Plus$list_instance = &Plus$list_struct;

static struct Iterator$__class__ Iterator$list_struct = {"GC_Iterator",$list_next_instance};
Iterator$__class__ Iterator$list_instance = &Iterator$list_struct;


static struct Iterable$__class__ Iterable$list_struct = {"GC_Iterable", $list_iter_instance};
Iterable$__class__ Iterable$list_instance = &Iterable$list_struct;

static struct Collection$__class__ Collection$list_struct = {"GC_Collection",&Iterable$list_struct,$list_fromiter_instance,$list_len_instance};
Collection$__class__ Collection$list_instance = &Collection$list_struct;

static struct Indexed$__class__ Indexed$list_struct = {"GC_Indexed", $list_getitem_instance, $list_setitem_instance, $list_delitem_instance};
Indexed$__class__ Indexed$list_instance = &Indexed$list_struct;

static struct Sliceable$__class__ Sliceable$list_struct = {"GC_Sliceable", &Indexed$list_struct, $list_getslice_instance, $list_setslice_instance, $list_delslice_instance};
Sliceable$__class__ Sliceable$list_instance = &Sliceable$list_struct;

static struct Sequence$__class__ Sequence$list_struct = {"GC_Sequence",&Sliceable$list_struct, &Collection$list_struct, &Plus$list_struct,
                                                         $list_reversed_instance,$list_insert_instance,$list_append_instance,$list_reverse_instance};
Sequence$__class__ Sequence$list_instance = &Sequence$list_struct;

Container_Eq$__class__ Container_Eq$list_instance(Eq$__class__ eqA);

// Types /////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct list_internal_t {
  char *GCINFO;
  $WORD *data;
  int length;
  int capacity;
} *list_internal_t;

typedef struct $list$__methods__ {
  $list (*copy)($list self);
  //  $int (*sort)($list self, int (*cmp)($WORD,$WORD));
} *$list$__methods__;

struct $list {
  char *GCINFO;
  $list$__methods__ __class__;
  list_internal_t __internal__;
};


// List methods ///////////////////////////////////////////////////////////////////////////////////////////////


static struct $list$__methods__ $list_table = {$list_copy};
$list$__methods__ $list_methods = &$list_table;
 

// Auxiliary functions /////////////////////////////////////////////////////////////////////////////////////////////////////
 

//prints a list[$int]
void printlist($list lst) {
  $WORD w;
  printf("[");
  for (int i=0; i < *$list_len(lst)-1; i++) {
    w = $list_getitem(lst,i);
    printf("%ld, ",from$int(w));
  }
  if (*$list_len(lst) > 0) {
    w = $list_getitem(lst,*$list_len(lst)-1);
    printf("%ld",from$int(w));
  }
  printf("]\n");
}

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
static void expand(list_internal_t lst,int n) {
   if (lst->capacity >= lst->length + n)
     return;
   int newcapacity = lst->capacity==0 ? 1 : lst->capacity;
   while (newcapacity < lst->length+n)
     newcapacity <<= 1;
   $WORD* newptr = lst->data==NULL
     ? malloc(newcapacity*sizeof($WORD))
     : realloc(lst->data,newcapacity*sizeof($WORD));
   if (newptr == NULL) {
     exception e;
     MKEXCEPTION(e,MEMORYERROR);
     RAISE(e);
   }
   lst->data = newptr;
   lst->capacity = newcapacity;
}  

static $list list_new(int capacity) {
  if (capacity < 0) {
    exception e;
    MKEXCEPTION(e,VALUEERROR);
    RAISE(e);
  } 
  list_internal_t lst = malloc(sizeof(struct list_internal_t));
  if (lst == NULL) {
     exception e;
     MKEXCEPTION(e,MEMORYERROR);
     RAISE(e);
  }
  if (capacity>0) {
    lst->data = malloc(capacity*sizeof($WORD));
    if (lst->data == NULL) {
      exception e;
      MKEXCEPTION(e,MEMORYERROR);
      RAISE(e);
    }
  } else {
    lst->data = NULL;
  }
  lst->length = 0;
  lst->capacity = capacity;
  $list res;
  res = malloc(sizeof(struct $list));
  res->__class__ = $list_methods; 
  res->__internal__ = lst;
  return res;
}

// Plus /////////////////////////////////////////////////////////////////////////////////////////////

$list $list_add($list lst, $list other) {
  int lstlen = lst->__internal__->length;
  int otherlen = other->__internal__->length;
  int reslen = lstlen + otherlen;
  $list res = list_new(reslen);
  list_internal_t resinternal = res->__internal__;
  memcpy(resinternal->data,lst->__internal__->data,lstlen*sizeof($WORD));
  memcpy(resinternal->data+lstlen,other->__internal__->data,otherlen*sizeof($WORD));
  resinternal->length = reslen;
  return res;
}

// instance method

$WORD $list_add_instance(Plus$__class__ cl, $WORD a, $WORD b) {
    return $list_add(($list)a,($list)b);
}

// Collection ///////////////////////////////////////////////////////////////////////////////////////


$list $list_fromiter(Iterable it) {
  $list res = list_new(0);
  if (it==NULL) {
    return res;
  }
  Iterator iter = it->__class__->__iter__(Iterable$list_instance, it);
  while(1) {
    $WORD nxt = iter->__class__->__next__(Iterator$list_instance, iter);
    $list_append(res,nxt);
  }                                         // try/except to stop loop when next raises STOPITERATION.
  return res;
}
  
$int $list_len($list lst) {
  long *res = malloc(sizeof(long));
  *res = (long)lst->__internal__->length;
  return res;
}

// instance methods

Collection $list_fromiter_instance(Collection$__class__ cl, Iterable it) {
  return Collection$__pack__(Collection$list_instance,$list_fromiter(it));
}

$int $list_len_instance(Collection$__class__ cl, $WORD self) {
  return $list_len(($list)self);
}

// Container ///////////////////////////////////////////////////////////////////////////

$bool $list_contains($list lst, $WORD elem, $bool (*eq)(Eq$__class__,$WORD,$WORD)) {
  for (int i=0; i < lst->__internal__->length; i++) {
    if (eq(NULL,elem,lst->__internal__->data[i]))
      return 1;
  }
  return 0;
}

$bool $list_containsnot($list lst, $WORD elem, $bool (*eq)(Eq$__class__,$WORD,$WORD)) {
  return !$list_contains(lst,elem,eq);
}

// instance methods

$bool $list_contains_instance (Container_Eq$__class__ cl, $WORD self, $WORD elem) {
  return $list_contains(($list)self,elem,cl->eqA->__eq__);
}

$bool $list_containsnot_instance (Container_Eq$__class__ cl, $WORD self, $WORD elem) {
  return $list_containsnot(($list)self,elem,cl->eqA->__neq__);
}


// Iterable ///////////////////////////////////////////////////////////////////////////

typedef struct list_iterator_state_t {
  char *$GCINFO;
  list_internal_t src;
  int nxt;
} *list_iterator_state_t; 

static $WORD $list_iterator_next( list_iterator_state_t state) {
  if (state->nxt > state->src->length) {
    exception e;
    MKEXCEPTION(e,STOPITERATION);
    RAISE(e);
  }
  return state->src->data[state->nxt++];
}

/*
iterator_internal_t $list_iter(list_iterator_state_t state) {
  iterator_internal_t iter = malloc(sizeof(struct iterator_internal_t));
  iter->state = state;
  iter->next = $list_iterator_next;
  return iter;
}
*/
static list_iterator_state_t $list_state_of($list lst) {
  list_iterator_state_t state = malloc(sizeof(struct list_iterator_state_t));
  state->src = lst->__internal__;
  state->nxt = 0;
  return state;
}

// instance methods

 Iterator $list_iter_instance(Iterable$__class__ cl,$WORD self) {
  return Iterator$__pack__(Iterator$list_instance,$list_state_of(($list)self));
}

 $WORD $list_next_instance(Iterator$__class__ cl, $WORD self) {
  return  $list_iterator_next(self);
}

// Indexed ///////////////////////////////////////////////////////////////////////////

$WORD $list_getitem($list lst, int ix) {
  int len = lst->__internal__->length;
  int ix0 = ix < 0 ? len + ix : ix;
  if (ix0 < 0 || ix0 >= len) {
     exception e;
    MKEXCEPTION(e,INDEXERROR);
    RAISE(e);
  }
  return lst->__internal__->data[ix0];
}

void $list_setitem($list lst, int ix, $WORD val) {
  int len = lst->__internal__->length;
  int ix0 = ix < 0 ? len + ix : ix;
  if (ix0 < 0 || ix0 >= len) {
    exception e;
    MKEXCEPTION(e,INDEXERROR);
    RAISE(e);
  }
  lst->__internal__->data[ix0] = val;
}

void $list_delitem($list lst,int ix) {
  list_internal_t internal = lst->__internal__;
  int len = internal->length;
  int ix0 = ix < 0 ? len + ix : ix;
  if(ix0 < 0 || ix0 >= len) {
    exception e;
    MKEXCEPTION(e,INDEXERROR);
    RAISE(e);
  }
  memmove(internal->data + ix0,
          internal->data + (ix0 + 1),
          (len-(ix0+1))*sizeof($WORD));
  internal->length--;
}

// instance methods

$WORD $list_getitem_instance(Indexed$__class__ cl, $WORD self, $WORD ix) {
  $WORD w = $list_getitem(($list)self,*(long*)ix);
  return w;
}

void $list_setitem_instance(Indexed$__class__ cl, $WORD self, $WORD ix, $WORD val){
  $list_setitem(($list)self,*(int*)ix,val);
}

void $list_delitem_instance(Indexed$__class__ cl, $WORD self, $WORD ix) {
  $list_delitem(($list)self,*(int*)ix);
}

// Sliceable //////////////////////////////////////////////////////////////////////////////////////

$list $list_getslice($list lst, Slice slc) {
  int len = lst->__internal__->length;
  int start, stop, step, slen;
  normalize_slice(slc, len, &slen, &start, &stop, &step);
  //slice notation have been eliminated and default values applied.
  // slen now is the length of the slice
  $list rlst = list_new(slen);
  int t = start;
  for (int i=0; i<slen; i++) {
    $WORD w;
    w = $list_getitem(lst,t);
    $list_append(rlst,w);
    t += step;
  }
  return rlst;
}

void $list_setslice($list lst, Slice slc, $list other) {
  list_internal_t internal = lst->__internal__;
  list_internal_t ointernal = other->__internal__;
  int len = internal->length; 
  int olen = ointernal->length; 
  int start, stop, step, slen;
  normalize_slice(slc, len, &slen, &start, &stop, &step);
  if (step != 1 && olen != slen) {
    exception e;
    MKEXCEPTION(e,VALUEERROR);
    RAISE(e);
  }
  int copy = olen <= slen ? olen : slen;
  int t = start;
  for (int i= 0; i<copy; i++) {
    internal->data[t] = ointernal->data[i];
    t += step;
  }
  if (olen == slen)
    return;
  // now we know that step=1
  if (olen < slen) {
    memmove(internal->data + start + copy,
            internal->data + start + slen,
            (len-(start+slen))*sizeof($WORD));
     internal->length-=slen-olen;
     return;
  } else {
    expand(internal,olen-slen);
    int rest = len - (start+copy);
    int incr = olen - slen;
    memmove(internal->data + start + copy + incr,
            internal->data + start + copy,
            rest*sizeof($WORD));
    for (int i = copy; i < olen; i++)
      internal->data[start+i] = ointernal->data[i];
    internal->length += incr;
  }
}


void $list_delslice($list lst, Slice slc) {
  int len = lst->__internal__->length;
  int start, stop, step, slen;
  normalize_slice(slc, len, &len, &start, &stop, &step);
  if (slen==0) return;
  for (int ix = start+step*(slen-1); ix>= start; ix -= step)
    $list_delitem(lst,ix);
}

// instance methods

// This auxiliary function is needed until we can change last param of setslice to an Iterable
$list seq2list(Sequence s) {
  Collection$__class__ cl =  s->__class__->Collection$__methods__;
  $int len = cl->__len__(cl,s->__impl__);
  $list res = list_new(*len);
  for(int i = 0; i<*len; i++) {
    Indexed$__class__ cl1 = s->__class__->Sliceable$__methods__->Indexed$__methods__;
    $list_append(res,cl1->__getitem__(cl1,s->__impl__,to$int(i)));
  }
  return res;
}

$WORD $list_getslice_instance(Sliceable$__class__ cl, $WORD self, Slice slice) {
  return $list_getslice(($list)self,slice);
}

void $list_setslice_instance(Sliceable$__class__ cl, $WORD self, Slice slice, Sequence it) {
  $list_setslice(($list)self,slice,/*$list_fromiter(it)*/seq2list(it));
}

void $list_delslice_instance(Sliceable$__class__ cl, $WORD self, Slice slice) {
  $list_delslice(($list)self,slice);
}

// Sequence /////////////////////////////////////////////////////////////////////////////

void $list_append($list lst, $WORD val) {
   expand(lst->__internal__,1);
  lst->__internal__->data[lst->__internal__->length++] = val;
}

Iterable $list_reversed($list lst){
  $list copy = $list_copy(lst);
  $list_reverse(copy);
  return Iterable$__pack__(Iterable$list_instance,$list_state_of(copy));
}

void $list_insert($list lst, int ix, $WORD val) {
  list_internal_t internal = lst->__internal__;
  int len = internal->length;
  expand(internal,1);
  int ix0 = ix < 0 ? max(len+ix,0) : min(ix,len);
  memmove(internal->data + (ix0 + 1),
          internal->data + ix0 ,
          (len - ix0) * sizeof($WORD));
  internal->data[ix0] = val;
  internal->length++;
}

// In place reversal
void $list_reverse($list lst) {
  list_internal_t internal = lst->__internal__;
  int len = internal->length;
  for (int i = 0; i < len/2; i++) {
    $WORD tmp = internal->data[i];
    internal->data[i] = internal->data[len-1-i];
    internal->data[len-1-i] = tmp;
  }
}

// instance methods

Iterable $list_reversed_instance(Sequence$__class__ cl, $WORD self) {
  return $list_reversed(($list)self);
}

void $list_insert_instance(Sequence$__class__ cl, $WORD self, $int ix, $WORD elem) {
  $list_insert(($list)self,*ix,elem);
}
void $list_append_instance(Sequence$__class__ cl, $WORD self, $WORD elem) {
  $list_append(($list)self,elem);
}

void $list_reverse_instance(Sequence$__class__ cl, $WORD self) {
  $list_reverse(($list)self);
}

// List-specific methods /////////////////////////////////////////////////////////////////////

$list $list_copy($list lst) {
  int len = lst->__internal__->length;
  $list res = list_new(len);
  res->__internal__->length = len;
  memcpy(res->__internal__->data,lst->__internal__->data,len*sizeof($WORD));
  return res;
}
/*                   
int list_sort(list_t lst, int (*cmp)(WORD,WORD)) {
  return heapsort(lst->data, lst->length, sizeof(WORD), cmp);
}
*/

// Instance initialization ////////////////////////////////////////////////////////////////////

Container_Eq$__class__ Container_Eq$list_instance(Eq$__class__ eqA) {
  Container_Eq$__class__ res = malloc(sizeof(struct Container_Eq$__class__));
  res->$GCINFO = "GC_Container_Eq";
  res->Collection$__methods__ = Collection$list_instance;
  res->__contains__ = $list_contains_instance;
  res->__containsnot__ = $list_containsnot_instance;
  res->eqA = eqA;
  return res;
}

 
