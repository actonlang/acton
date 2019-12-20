#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "list.h"
#include "iterator.h"

//  Method tables ///////////////////////////////////////////////////////////////

static struct $list$__methods__ table = {$list_copy};
$list$__methods__ methods = &table;
 
static struct Plus$__class__ Plus_$list_struct;
Plus$__class__ Plus_$list_instance;

static struct Iterator$__class__ Iterator_$list_struct;
Iterator$__class__ Iterator_$list_instance;

static struct Iterable$__class__ Iterable_$list_struct;
Iterable$__class__ Iterable_$list_instance;

static struct Collection$__class__ Collection_$list_struct;
Collection$__class__ Collection_$list_instance;

static struct Indexed$__class__ Indexed_$list_struct;
Indexed$__class__ Indexed_$list_instance;

static struct Sliceable$__class__ Sliceable_$list_struct;
Sliceable$__class__ Sliceable_$list_instance;

static struct Sequence$__class__ Sequence_$list_struct;
Sequence$__class__ Sequence_$list_instance;

Container_Eq$__class__ Container_Eq$list_instance(Eq eqA);

// Auxiliary functions /////////////////////////////////////////////////////////////////////////////////////////////////////

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
  res = malloc(sizeof($list));
  res->__class__ = methods; 
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

$WORD $list_add_instance($WORD a, $WORD b) {
  return ($WORD)$list_add(($list)a,($list)b);
}

// Collection ///////////////////////////////////////////////////////////////////////////////////////


$list $list_fromiter(Iterable it) {
  $list res = list_new(0);
  if (it==NULL) {
    return res;
  }
  Iterator iter = it->__class__->__iter__(it);
  while(1) {
    $WORD nxt = iter->__class__->__next__(iter);
    $list_append(res,nxt);
  }                                         // try/except to stop loop when next raises STOPITERATION.
  return res;
}
  
$int $list_len($list lst) {
  int *res = malloc(sizeof(int));
  *res = lst->__internal__->length;
  return res;
}

// instance methods

Collection $list_fromiter_instance(Iterable it) {
  $list res = $list_fromiter(it);
  return Collection$__pack__(Collection_$list_instance,($WORD)res);
}

$int $list_len_instance(Collection self) {
  return $list_len(($list)self->__impl__);
}

// Container ///////////////////////////////////////////////////////////////////////////

$bool $list_contains($list lst, $WORD elem, int (*eq)($WORD,$WORD)) {
  for (int i=0; i < lst->__internal__->length; i++)
    if (eq(elem,lst->__internal__->data[i]))
      return 1;
  return 0;
}

$bool $list_containsnot($list lst, $WORD elem, int (*eq)($WORD,$WORD)) {
  return !$list_contains(lst,elem,eq);
}

// instance methods

$bool $list_contains_instance (Container_Eq self, $WORD elem) {
  return $list_contains(($list)self->__impl__,elem,self->__class__->eqA->__class__->__eq__);
}

$bool $list_containsnot_instance (Container_Eq self, $WORD elem) {
  return $list_containsnot(($list)self->__impl__,elem,self->__class__->eqA->__class__->__eq__);
}


// Iterable ///////////////////////////////////////////////////////////////////////////

typedef struct list_iterator_state_t {
  char *$GCINFO;
  list_internal_t src;
  int nxt;
} *list_iterator_state_t; 

static $WORD $list_iterator_next(iterator_internal_t iter) {
  list_iterator_state_t state = iter->state;
  if (state->nxt > state->src->length) {
    exception e;
    MKEXCEPTION(e,STOPITERATION);
    RAISE(e);
  }
  return state->src->data[state->nxt++];
}

iterator_internal_t $list_iter(list_iterator_state_t state) {
  iterator_internal_t iter = malloc(sizeof(struct iterator_internal_t));
  iter->state = state;
  iter->next = $list_iterator_next;
  return iter;
}

list_iterator_state_t state_of($list lst) {
  list_iterator_state_t state = malloc(sizeof(struct list_iterator_state_t));
  state->src = lst->__internal__;
  state->nxt = 0;
  return state;
}

// instance method

Iterator $list_iter_instance(Iterable self) {
  $list lst = ($list)self->__impl__;
  return Iterator$__pack__(Iterator_$list_instance,state_of(lst));
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

$WORD $list_getitem_instance(Indexed self, $WORD ix) {
  $WORD w = $list_getitem(($list)self->__impl__,*(long*)ix);
  return w;
}

void $list_setitem_instance(Indexed self, $WORD ix, $WORD val){
  $list_setitem(($list)self->__impl__,*(int*)ix,val);
}

void $list_delitem_instance(Indexed self, $WORD ix) {
  $list_delitem(($list)self->__impl__,*(int*)ix);
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

Sequence $list_getslice_instance(Sliceable self, Slice slice) {
  $list res = $list_getslice(($list)self->__impl__,slice);
  return Sequence$__pack__(Sequence_$list_instance,($WORD)res);
}
  
void $list_setslice_instance(Sliceable self, Slice slice, Iterable it) {
  $list_setslice(($list)self->__impl__,slice,$list_fromiter(it));
}
  
void $list_delslice_instance(Sliceable self, Slice slice) {
  $list_delslice(($list)self->__impl__,slice);
}

// Sequence /////////////////////////////////////////////////////////////////////////////

void $list_append($list lst, $WORD val) {
   expand(lst->__internal__,1);
  lst->__internal__->data[lst->__internal__->length++] = val;
}

Iterable $list_reversed($list lst){
  $list copy = $list_copy(lst);
  $list_reverse(copy);
  return Iterable$__pack__(Iterable_$list_instance,($WORD)copy);
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

Iterable $list_reversed_instance(Sequence self) {
  return $list_reversed(($list)self);
}

void $list_insert_instance(Sequence self, $int ix, $WORD elem) {
  $list_insert(($list)self,*ix,elem);
}
void $list_append_instance(Sequence self, $WORD elem) {
  $list_append(($list)self,elem);
}

void $list_reverse_instance(Sequence self) {
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

Container_Eq$__class__ Container_Eq$list_instance(Eq eqA) {
  Container_Eq$__class__ res = malloc(sizeof(Container_Eq$__class__));
  res->$GCINFO = "GC_Container_Eq";
  res->Collection$__methods__ = Collection_$list_instance;
  res->__contains__ = $list_contains_instance;
  res->__containsnot__ = $list_containsnot_instance;
  res->eqA = eqA;
  return res;
}
 
void list_instance_init() {
  Plus_$list_struct.$GCINFO = "GC_Plus";
  Plus_$list_struct.__add__ = $list_add_instance;

  Plus_$list_instance = &Plus_$list_struct;

  Iterable_$list_struct.$GCINFO = "GC_Iterable";
  Iterable_$list_struct.__iter__ = $list_iter_instance;
  
  Iterable_$list_instance = &Iterable_$list_struct;

  Collection_$list_struct.$GCINFO = "GC_Collection";
  Collection_$list_struct.Iterable$__methods__ = Iterable_$list_instance;
  Collection_$list_struct.__fromiter__ = $list_fromiter_instance;
  Collection_$list_struct.__len__ = $list_len_instance;

  Collection_$list_instance = &Collection_$list_struct;

  Indexed_$list_struct.$GCINFO = "GC_Indexed";
  Indexed_$list_struct.__getitem__ = $list_getitem_instance;
  Indexed_$list_struct.__setitem__ = $list_setitem_instance;
  Indexed_$list_struct.__delitem__ = $list_delitem_instance;

  Indexed_$list_instance = &Indexed_$list_struct;
  
  Sliceable_$list_struct.$GCINFO = "GC_Sliceable";
  Sliceable_$list_struct.Indexed$__methods__ = Indexed_$list_instance;
  Sliceable_$list_struct.__getslice__ = $list_getslice_instance;
  Sliceable_$list_struct.__setslice__ = $list_setslice_instance;
  Sliceable_$list_struct.__delslice__ = $list_delslice_instance;

  Sliceable_$list_instance = &Sliceable_$list_struct;

  Sequence_$list_struct.$GCINFO = "GC_Sequence";
  Sequence_$list_struct.Plus$__methods__ = Plus_$list_instance;
  Sequence_$list_struct.Sliceable$__methods__ = Sliceable_$list_instance;
  // Sequence_$list_struct.Container$__methods__ = Container_$list_instance;
  Sequence_$list_struct.append = $list_append_instance;
  Sequence_$list_struct.insert = $list_insert_instance;
  Sequence_$list_struct.__reversed__ = $list_reversed_instance;
  Sequence_$list_struct.reverse = $list_reverse_instance;

  Sequence_$list_instance = &Sequence_$list_struct;
}
