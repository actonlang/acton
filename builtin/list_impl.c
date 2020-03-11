#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "common.h"
#include "int.h"
#include "list_impl.h"


 
typedef struct $list$__methods__ {
  $list (*copy)($list self);
  //  $int (*sort)($list self, int (*cmp)($WORD,$WORD));
} *$list$__methods__;

struct $list {
  char *GCINFO;
  $list$__methods__ __class__;
  $WORD *data;
  int length;
  int capacity;
};


// List methods ///////////////////////////////////////////////////////////////////////////////////////////////


static struct $list$__methods__ $list_table = {$list_copy};
$list$__methods__ $list_methods = &$list_table;
 

// Auxiliary functions /////////////////////////////////////////////////////////////////////////////////////////////////////
 

//prints a list[$int]
void printlist($list lst) {
  $WORD w;
  printf("[");
  for (int i=0; i < $list_len(lst)-1; i++) {
    w = $list_getitem(lst,i);
    printf("%ld, ",from$int(w));
  }
  if ($list_len(lst) > 0) {
    w = $list_getitem(lst,$list_len(lst)-1);
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
  $list lst = malloc(sizeof(struct $list));
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
  lst->__class__ = $list_methods; 
  return lst;
}

// Plus /////////////////////////////////////////////////////////////////////////////////////////////

$list $list_add($list lst, $list other) {
  int lstlen = lst->length;
  int otherlen = other->length;
  int reslen = lstlen + otherlen;
  $list res = list_new(reslen);
  memcpy(res->data,lst->data,lstlen*sizeof($WORD));
  memcpy(res->data+lstlen,other->data,otherlen*sizeof($WORD));
  res->length = reslen;
  return res;
}
 
// Collection ///////////////////////////////////////////////////////////////////////////////////////


$list $list_fromiter(Iterator iter) {
  $list res = list_new(0);
  if (iter==NULL) {
    return res;
  }
  while (1) {
    $WORD nxt = iter->__class__->__next__(iter);
    $list_append(res,nxt);
  }                                         // try/except to stop loop when next raises STOPITERATION.
  return res;
}


long $list_len($list lst) {
  return (long)lst->length;
}

// Container ///////////////////////////////////////////////////////////////////////////

int $list_contains($list lst, $WORD elem, $bool (*eq)($WORD,$WORD)) {
  for (int i=0; i < lst->length; i++) {
    if (from$bool(eq(elem,lst->data[i])))
      return 1;
  }
  return 0;
}

int $list_containsnot($list lst, $WORD elem, $bool (*eq)($WORD,$WORD)) {
  return !$list_contains(lst,elem,eq);
}

  

// Iterable ///////////////////////////////////////////////////////////////////////////
typedef struct Iterator$list {
  char *$GCINFO;
  $WORD(*__next__)($WORD self);
  $list src;
  int nxt;
} *Iterator$list; 

static $WORD $list_iterator_next($WORD self) {
  Iterator$list state = (Iterator$list) ((Iterator)self)->__class__;
  if (state->nxt >= state->src->length) {
    exception e;
    MKEXCEPTION(e,STOPITERATION);
    RAISE(e);
  }
  return state->src->data[state->nxt++];
}

Iterator $list_iter($list lst) {
  Iterator$list iter = malloc(sizeof(struct Iterator$list));
  iter->__next__ = $list_iterator_next;
  iter->src = lst;
  iter->nxt = 0;
  Iterator res = malloc(sizeof(struct Iterator));
  res->__class__ = (Iterator$__class__)iter;
  return res;
}

// Indexed ///////////////////////////////////////////////////////////////////////////

$WORD $list_getitem($list lst, int ix) {
  int len = lst->length;
  int ix0 = ix < 0 ? len + ix : ix;
  if (ix0 < 0 || ix0 >= len) {
     exception e;
    MKEXCEPTION(e,INDEXERROR);
    RAISE(e);
  }
  return lst->data[ix0];
}

void $list_setitem($list lst, int ix, $WORD val) {
  int len = lst->length;
  int ix0 = ix < 0 ? len + ix : ix;
  if (ix0 < 0 || ix0 >= len) {
    exception e;
    MKEXCEPTION(e,INDEXERROR);
    RAISE(e);
  }
  lst->data[ix0] = val;
}

void $list_delitem($list lst,int ix) {
  int len = lst->length;
  int ix0 = ix < 0 ? len + ix : ix;
  if(ix0 < 0 || ix0 >= len) {
    exception e;
    MKEXCEPTION(e,INDEXERROR);
    RAISE(e);
  }
  memmove(lst->data + ix0,
          lst->data + (ix0 + 1),
          (len-(ix0+1))*sizeof($WORD));
  lst->length--;
}
 

// Sliceable //////////////////////////////////////////////////////////////////////////////////////

$list $list_getslice($list lst, Slice slc) {
  int len = lst->length;
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

void $list_setslice($list lst, Slice slc, Iterator it) {
  int len = lst->length;
  $list other = list_new(0);
  $WORD w;
  while((w=it->__class__->__next__(it)))
    $list_append(other,w);
  int olen = other->length; 
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


void $list_delslice($list lst, Slice slc) {
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


Iterator $list_reversed($list lst){
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
  $list res = list_new(len);
  res->length = len;
  memcpy(res->data,lst->data,len*sizeof($WORD));
  return res;
}
/*                   
int list_sort(list_t lst, int (*cmp)(WORD,WORD)) {
  return heapsort(lst->data, lst->length, sizeof(WORD), cmp);
}
*/
 
