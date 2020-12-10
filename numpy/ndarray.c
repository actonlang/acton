#include <limits.h>

//Select element #n in lst which is a list[int].
#define $LONGELEM(lst,n)   ((($int)lst->data[n])->val)

numpy$$array_iterator_state $mk_iterator(numpy$$ndarray a);
union $Bytes8 *iter_next(numpy$$array_iterator_state it);


// Auxiliary functions ///////////////////////////////////////////////////////////////////////////////

// method for creating ndarray structs.
// res->offset gets default value 0 (may be unsuitable when allocate_data = false)

static numpy$$ndarray $newarray(enum ElemType typ, long ndim,long size,$list shape,$list strides,bool allocate_data) {
  numpy$$ndarray res = malloc(sizeof(struct numpy$$ndarray));
  res->$class = &numpy$$ndarray$methods;
  res->elem_type = typ;
  res->ndim = ndim;
  res->size = size;
  res->offset = 0;
  res->shape = shape;
  res->strides = strides;
  if (allocate_data) res->data = malloc(size * $elem_size(typ) * sizeof(union $Bytes8));
  return res;
}

long $prod($list lst) {
  long res = 1;
  for (int i = 0; i<lst->length; i++)
    res *= $LONGELEM(lst,i);
  return res;
}

bool $is_contiguous(numpy$$ndarray a) {
  if (a->offset != 0) return false;
  long size = $LONGELEM(a->strides,a->ndim-1);
  if (size != 1) return false;
  for (int i = a->ndim-2; i>=0; i--) {
    size *= $LONGELEM(a->shape,i+1);
    if (size != $LONGELEM(a->strides,i)) {
      return false;
    }
  }
  return true;
}

$list $mk_strides($list shape) {
  long size = 1;
  $list res = $list_new(shape->length);
  res->length = shape->length;
  for (int i = shape->length-1; i>=0; i--) {
    long s = from$int($list_getitem(shape, i));
    $list_setitem(res,i,to$int(s > 1 ? size : 0));
    size *= s;
  }
  return res;
}

// Superclass methods /////////////////////////////////////////////////////////////////////////

void numpy$$ndarray$__init__(numpy$$ndarray self, $WORD w) {
    numpy$$ndarray r = numpy$$fromatom(w);
    memcpy(self,r,sizeof(struct numpy$$ndarray));
}

void  numpy$$ndarray$__serialize__(numpy$$ndarray self, $Serial$state state) {
  $int prevkey = ($int)$dict_get(state->done,($Hashable)$Hashable$WORD$witness,self,NULL);
  if (prevkey) {
    $val_serialize(-self->$class->$class_id,&prevkey->val,state);
    return;
  }
  $dict_setitem(state->done,($Hashable)$Hashable$WORD$witness,self,to$int(state->row_no));
  int blobsize = 5 + self->size;
  $ROW row = $add_header(self->$class->$class_id,blobsize,state);
  row->blob[0] = ($WORD)self->elem_type;
  row->blob[1] = ($WORD)self->ndim;
  row->blob[2] = ($WORD)self->size;
  row->blob[3] = ($WORD)self->offset;
  row->blob[4] = ($WORD)self->elem_size;
  memcpy(&row->blob[5],self->data,self->size*sizeof($WORD));
  $step_serialize(self->shape, state);
  $step_serialize(self->strides, state);
}

numpy$$ndarray numpy$$ndarray$__deserialize__($Serial$state state) {
  $ROW this = state->row;
  state->row = this->next;
  state->row_no++;
  if (this->class_id < 0) {
    return $dict_get(state->done,($Hashable)$Hashable$int$witness,to$int((long)this->blob[0]),NULL);
  } else {
    numpy$$ndarray res = malloc(sizeof(struct numpy$$ndarray));
    $dict_setitem(state->done,($Hashable)$Hashable$int$witness,to$int(state->row_no-1),res);
    res->$class = &numpy$$ndarray$methods;
    res->elem_type = (enum ElemType)this->blob[0];
    res->ndim = (long)this->blob[1];
    res->size = (long)this->blob[2];
    res->offset = (long)this->blob[3];
    res->elem_size = (long)this->blob[4];
    res->data = malloc(res->size*sizeof($WORD));
    memcpy(res->data, &this->blob[5],res->size*sizeof($WORD));
    res->shape = $step_deserialize(state);
    res->strides= $step_deserialize(state);
    return res;
  }
}

$bool  numpy$$ndarray$__bool__(numpy$$ndarray a) {
  if (a->size > 1) 
    RAISE(($BaseException)$NEW($ValueError,to$str("__bool__ undefined for ndarrays with more than one element")));
  switch (a->elem_type) {
  case LongType:
    return to$bool(a->data[a->offset].l != 0);
  case DblType:
    return to$bool(a->data[a->offset].d != 0.0);
  }
}

numpy$$ndarray numpy$$ndarray$__ndgetslice__(numpy$$ndarray a, $list ix);

$str numpy$$ndarray$__str__(numpy$$ndarray a) {
  if (a->ndim==0) {
    switch (a->elem_type) {
    case LongType:
      return l$prim_str(a->data[a->offset]);
    case DblType:
      return d$prim_str(a->data[a->offset]);
    }
  } else {
    $list strs = $NEW($list,NULL,NULL);
    $list ix = $list_new(1);
    ix->length = 1;
    for (long i = 0; i< $LONGELEM(a->shape,0); i++) {
      ix->data[0] = numpy$$ndindex$new(to$int(i));
      numpy$$ndarray b = numpy$$ndarray$__ndgetslice__(a,ix);
      $list_append(strs,numpy$$ndarray$__str__(b));
    }
    $str s = $str_join_par('[',strs,']');
    $Plus wit = ($Plus)$Plus$str$witness;
    return wit->$class->__add__(wit,s,to$str("\n"));
  }
}

//ndarray methods /////////////////////////////////////////////////////////////////////////////////

// reshape attempts to present a new view, but may have to copy data.

numpy$$ndarray numpy$$ndarray$reshape(numpy$$ndarray a, $list newshape) {
  long size = $prod(newshape);
  if (a->size != size)
    RAISE(($BaseException)$NEW($ValueError,to$str("wrong number of array elements for reshape")));
  if (a->shape->length == newshape->length) {
    // Check if newshape is actually equal to a->shape.
    int sameshape = 1;
    int i = 0;
    while (sameshape && i < newshape->length) {
      if ($LONGELEM(a->shape,i) != $LONGELEM(newshape,i))
        sameshape = 0;
      i++;
    }
    if (sameshape)
      return a;
  }
  // Check if we can reuse a->data or we need to build a fresh data array.
  // We only reuse a->data if the indices in a->data of the elements of a form an arithmetic sequence.
  int needcopy = 0;
  long currstride = $LONGELEM(a->strides,a->strides->length-1);
  int i = a->strides->length-2;
  while (!needcopy && i >= 0) {
    long nextstride = $LONGELEM(a->strides,i);
    if (nextstride != currstride *  $LONGELEM(a->shape,i+1))
      needcopy = 1;
    else {
      i--;
      currstride = nextstride;
    }
  }
  // Compute strides list of result. Computation starts from last element, which is computed
  // differently if we reuse a->data than if we make a fresh array.
  $list newstrides = $list_new(newshape->length);
  newstrides->length = newshape->length;
  newstrides->data[newstrides->length-1] = needcopy ? to$int(1) : a->strides->data[a->strides->length-1];
  for (int i = newstrides->length-2; i>=0; i--)
    newstrides->data[i] = to$int($LONGELEM(newstrides,i+1) * $LONGELEM(newshape,i+1));
  // Build result
  numpy$$ndarray res = $newarray(a->elem_type,newshape->length,a->size,newshape,newstrides,false);
  if (!needcopy) {
    res->offset = a->offset;
    res->data = a->data;
    return res;
  } else {
    res->data = malloc(size * sizeof(union $Bytes8));
    union $Bytes8 *ixres = res->data;
    union $Bytes8 *ixa;
    numpy$$array_iterator_state ita = $mk_iterator(a);
    while ((ixa = iter_next(ita))) {
      *ixres = *ixa;
      ixres++;
    }
    return res;
  }
}  

// permutes axes in a to the order given by axes.
// If second argument is NULL, reverse order of axes.
// Does not copy data; returns a new view.
numpy$$ndarray numpy$$ndarray$transpose(numpy$$ndarray a, $list axes) {
  $list newshape, newstrides;
  if (!axes) {
    newshape = $list_copy(a->shape);
    $list_reverse(newshape);
    newstrides = $list_copy(a->strides);
    $list_reverse(newstrides);
  } else {
    bool is_perm = true;
    if (axes->length != a->shape->length) is_perm = false;
    long i = axes->length-1;
    while (is_perm && i >= 0) {
      if (!$list_contains(($Eq)$Ord$int$witness,axes,to$int(i))) is_perm = false;
      i--;
    }
    if (!is_perm)
      RAISE(($BaseException)$NEW($ValueError,to$str("transpose: axes argument must be permutation of [0..a.ndim-1]")));
    newshape = $list_new(axes->length);
    newstrides = $list_new(axes->length);
    newshape->length = axes->length;
    newstrides->length = axes->length;
    for (long i = 0; i<axes->length; i++) {
      long n = (($int)axes->data[i])->val;
      newshape->data[i] = a->shape->data[n];
      newstrides->data[i] = a->strides->data[n];
    }
  }
  numpy$$ndarray res = $newarray(a->elem_type,a->ndim,a->size,newshape,newstrides,false);
  res->data = a->data;
  return res;
}

numpy$$ndarray numpy$$ndarray$flatten(numpy$$ndarray a) {
  $list newshape = $NEW($list,NULL,NULL);
  $list_append(newshape,to$int(a->size));
  return numpy$$ndarray$reshape(a,newshape);
}

// Makes a contiguous deep copy of its argument with stride of last dimension == 1.

numpy$$ndarray numpy$$ndarray$copy(numpy$$ndarray a) {
  numpy$$ndarray res = $newarray(a->elem_type,a->ndim,a->size,a->shape,$mk_strides(a->shape),true);
  if ($is_contiguous(a))
    memcpy(res->data,a->data,a->size*sizeof($WORD)); // Hackish; what is the proper size to use?
  else {
    numpy$$array_iterator_state it = $mk_iterator(a);
    union $Bytes8 *ixres, *ixa;
    ixres = res->data;
    while ((ixa = iter_next(it))) {
      *ixres = *ixa;
      ixres++;
    }
  }
  return res;
}

// basic slicing 

numpy$$ndarray numpy$$ndarray$__ndgetslice__(numpy$$ndarray a, $list ix) {
  // assert length of ix > 0
  int nulls = 0;
  int ints = 0;
  int slices = 0;
  // first analyze index list contents
  for (int i=0; i < ix->length; i++) {
    numpy$$ndselect ixi = (numpy$$ndselect)ix->data[i];
    if ($ISINSTANCE(ixi,numpy$$ndindex)->val) {
      if (((numpy$$ndindex)ixi)->index==numpy$$newaxis)
        nulls++;
      else
        ints++;
    } else if ($ISINSTANCE(ixi,numpy$$ndslice)->val)
      slices++;
    else {
      fprintf(stderr,"internal error: unexpected type of ndarray index element\n");
      exit(-1);
    }
  }
  if (ints+slices > a->ndim)
    RAISE(($BaseException)$NEW($ValueError,to$str("indexing too many dimensions")));
  int ndim = a->ndim + nulls - ints;
  numpy$$ndarray res = $newarray(a->elem_type,ndim,0,$list_new(ndim),$list_new(ndim),false); // size 0 is temporary
  res->shape->length = ndim;
  res->strides->length = ndim;
  res->data = a->data;
  int offset = a->offset;
  $slice allslice = $NEW($slice,NULL,NULL,NULL);
  int ixpos = ix->length-1;
  int apos = a->ndim-1;
  int respos = ndim-1;
  int untouched = a->ndim - (ints + slices);
  numpy$$ndselect currindex;
  long currstride = a->elem_size;
  while (ixpos>=0) {
    if (untouched-- > 0)
      currindex = (numpy$$ndselect)numpy$$ndslice$new(allslice);
    else
      currindex = (numpy$$ndselect)ix->data[ixpos--];
    if ($ISINSTANCE(currindex,numpy$$ndindex)->val) {
      if (((numpy$$ndindex)currindex)->index == numpy$$newaxis) {
        $list_setitem(res->strides,respos,to$int(0));
        $list_setitem(res->shape,respos--,to$int(1));
      } else {
        currstride = $LONGELEM(a->strides,apos--);
        offset += currstride * (((numpy$$ndindex)currindex)->index->val);
      }
    } else if ($ISINSTANCE(currindex,numpy$$ndslice)->val) {
        int slen,start,stop,step;
        normalize_slice(((numpy$$ndslice)currindex)->slc,$LONGELEM(a->shape,apos),&slen,&start,&stop,&step);
        currstride = $LONGELEM(a->strides,apos);
        offset += start * currstride;
        $list_setitem(res->strides,respos,to$int($LONGELEM(a->strides,apos--) * step));
        $list_setitem(res->shape,respos--,to$int(slen));
      } else {
        fprintf(stderr,"internal error: unexpected type of ndarray index element\n");
        exit(-1);
    }
  }
  res->offset = offset;
  res->size = $prod(res->shape);
  return res;
}

struct numpy$$ndarray$class numpy$$ndarray$methods = {
    "numpy$$ndarray",
    UNASSIGNED,
    ($Super$class)&$struct$methods,
    numpy$$ndarray$__init__,
    numpy$$ndarray$__serialize__,
    numpy$$ndarray$__deserialize__,
    numpy$$ndarray$__bool__,
    numpy$$ndarray$__str__,
    numpy$$ndarray$reshape,
    numpy$$ndarray$transpose,
    numpy$$ndarray$copy,
    numpy$$ndarray$__ndgetslice__
};

// Iterating over an ndarray //////////////////////////////////////////////////////////////////////////////

// Aims to be as fast as possible; used in many methods below and in protocol implementations 
numpy$$array_iterator_state $mk_iterator(numpy$$ndarray a) {
  numpy$$array_iterator_state res = malloc(sizeof(struct numpy$$array_iterator_state));
  if (a->ndim==0)
    RAISE(($BaseException)$NEW($ValueError,to$str("cannot make iterator for 0-dim array")));
  res->ndim1 = a->ndim-1;
  for (long i=res->ndim1; i>=0; i--) {
    res->shape[i]   = (($int)$list_getitem(a->shape,i))->val;
    res->strides[i] = (($int)$list_getitem(a->strides,i))->val;
    res->index[i] = 0;
  }
  for (long i=res->ndim1; i>=1; i--) {
    res->jumps[i] = res->strides[i-1] - res->shape[i]*res->strides[i];
  }
  res->lastshapepos = -1;//index[res->ndim1]--; //place at "position -1"
  res->lastshapelength = res->shape[res->ndim1];
  res->currentstride = res->strides[res->ndim1];
  res->current = &a->data[a->offset]-res->currentstride;
  return res;
}

union $Bytes8 *iter_next(numpy$$array_iterator_state it) {
  it->current += it->currentstride;
  if (++it->lastshapepos == it->lastshapelength) { 
    it->lastshapepos = 0;
    if (it->ndim1==0)
      it->current = NULL;
    else {
      long i = it->ndim1;
      it->current += it->jumps[i]; 
      i--;
      it->index[i]++;
      while (it->index[i]==it->shape[i]) {
        if (i==0) {
          it->current = NULL;
          break;
        } else {
          it->index[i] = 0;
          it->current += it->jumps[i];
          it->index[i-1]++;
          i--;
        }
      }
    }
  }
  return it->current;      
}
 
// Auxiliary function for mapping functions and operators over an ndarray ///////////////////////////////////////////

numpy$$ndarray numpy$$func(union $Bytes8(*f)(union $Bytes8),numpy$$ndarray a) {
  $list resstrides = $mk_strides(a->shape);
  numpy$$ndarray res = $newarray(a->elem_type,a->ndim,a->size,a->shape,resstrides,true); 
  union $Bytes8 *ixres = res->data;
  union $Bytes8 *ixa;
  numpy$$array_iterator_state it = $mk_iterator(a);
  while ((ixa = iter_next(it))) {
    *ixres = f(*ixa);
    ixres++;
  }
  return res;
}

// broadcasting to adapt two arrays to common shape before mapping anoperator.

// returns the common extended shape and, as outparams, iterators for the two extended operands

static numpy$$ndarray numpy$$broadcast(numpy$$ndarray a1, numpy$$ndarray a2, numpy$$array_iterator_state *it1, numpy$$array_iterator_state *it2) {
  int len;
  $list resshape, shape1, shape2, strides1, strides2;
  shape1 = $list_copy(a1->shape);
  shape2 = $list_copy(a2->shape);
  strides1 = $list_copy(a1->strides);
  strides2 = $list_copy(a2->strides);
  if (a1->ndim < a2->ndim) {
    for (int i=0; i< a2->ndim - a1->ndim; i++) {
      
      $list_insert(shape1,0,to$int(1));
      $list_insert(strides1,0,to$int(0));
    }
    len = a2->ndim;
  } else if (a2->ndim < a1->ndim) {
    for (int i=0; i< a1->ndim-a2->ndim; i++) {
      $list_insert(shape2,0,to$int(1));
      $list_insert(strides2,0,to$int(0));
    }
    len = a1->ndim;
  } else
    len = a1->ndim;
  resshape = $list_new(len);
  resshape->length = len;
  for (int i=0; i<len; i++) {
    if ($LONGELEM(shape1,i) == $LONGELEM(shape2,i))
      resshape->data[i] = shape1->data[i];
    else if ($LONGELEM(shape1,i) == 1)
      resshape->data[i] = shape2->data[i];
    else if ($LONGELEM(shape2,i) == 1)
      resshape->data[i] = shape1->data[i];
    else
      RAISE(($BaseException)$NEW($ValueError,to$str("ndarray broadcasting: shapes do not match")));
  }
  numpy$$ndarray res = $newarray(a1->elem_type,len,$prod(resshape),resshape,$mk_strides(resshape),true);
  numpy$$ndarray tmparr = $newarray(a1->elem_type,len,res->size,res->shape,strides1,false); // only for making iterators
  tmparr->offset = a1->offset; 
  tmparr->data = a1->data;
  *it1 = $mk_iterator(tmparr);
  tmparr->strides = strides2;
  tmparr->offset = a2->offset;
  tmparr->data = a2->data;
  *it2 = $mk_iterator(tmparr);
  return res;
}
 
numpy$$ndarray numpy$$oper(union $Bytes8 (*f)(union $Bytes8, union $Bytes8),numpy$$ndarray a, numpy$$ndarray b) {
  union $Bytes8 *ix1, *ix2, *ixres;
  long stride1, stride2, len;
  numpy$$array_iterator_state it1, it2;
  numpy$$ndarray res = numpy$$broadcast(a,b,&it1,&it2);
  ixres = res->data;
  while ((ix1 = iter_next(it1))) {
      ix2 = iter_next(it2);
      *ixres = f(*ix1,*ix2);
      ixres++;
  }
  return res; 
}



// fromatom /////////////////////////////////////////////////////////////////////////////

// The ndarray constructor takes an atomic argument and builds a 0-dimensional array.

numpy$$ndarray numpy$$fromatom($WORD a) {
  if ($ISINSTANCE(($Super)a,$int)->val) {
    numpy$$ndarray res = $newarray(LongType,0,1,$NEW($list,NULL,NULL),$NEW($list,NULL,NULL),true);
    res->data->l = (($int)a)->val;
    return res;
  }
  if ($ISINSTANCE(($Super)a,$float)->val) {
    numpy$$ndarray res = $newarray(DblType,0,1,$NEW($list,NULL,NULL),$NEW($list,NULL,NULL),true);
    res->data->d = (($float)a)->val;
    return res;
  }
  if ($ISINSTANCE(($Super)a,$bool)->val) return NULL;
  if ($ISINSTANCE(($Super)a,$str)->val) return NULL;
  fprintf(stderr,"internal error: fromatom: argument not of atomic type");
  exit(-1);
}

// Functions to create arrays /////////////////////////////////////////////////////////////

// n evenly spaced floats between a and b

numpy$$ndarray numpy$$linspace($float a, $float b, $int n) {
  $list shape = $NEW($list,NULL,NULL);
  $list_append(shape,n);
  $list strides = $NEW($list,NULL,NULL);
  $list_append(strides,to$int(1));
  numpy$$ndarray res = $newarray(DblType,1,n->val,shape,strides,true);
  double step = (b->val - a->val)/(n->val-1);
  for (long i = 0; i<n->val; i++) {
    res->data[i].d = a->val + i * step;
  }
  return res;
}

// array of ints described by a range

numpy$$ndarray numpy$$arange($int start, $int stop, $int step) {
  $range r = $NEW($range,start,stop,step);
  long len = (r->stop - r->start)/r->step;
  $list shape = $NEW($list,NULL,NULL);
  $list_append(shape,to$int(len));
  $list strides = $NEW($list,NULL,NULL);
  $list_append(strides,to$int(1));
  numpy$$ndarray res = $newarray(LongType,1,len,shape,strides,true);
  long elem = r->start;
  for (int i=0; i < len; i++) {
    res->data[i].l = elem;
    elem += r->step;
  }
  return res;
}  

// make an array from a list

numpy$$ndarray numpy$$array(numpy$$Primitive wit, $list elems) {
  $list shape = $NEW($list,NULL,NULL);
  $list_append(shape,to$int(elems->length));
  $list strides = $NEW($list,NULL,NULL);
  $list_append(strides,to$int(1));
  if (elems->length == 0)
    RAISE(($BaseException)$NEW($ValueError,to$str("function numpy.array cannot create empty ndarray")));
  numpy$$ndarray res = $newarray(wit->$class->elem_type,1,elems->length,shape,strides,true);
  for (int i=0; i<elems->length; i++) 
    res->data[i] = wit->$class->from$obj(elems->data[i]);
  return res;
}

// create an array with given shape and all elements of given value

numpy$$ndarray numpy$$full(numpy$$Primitive wit, $list shape, $WORD val) {
  $list strides = $mk_strides(shape);
  long size = $prod(shape);
  numpy$$ndarray res = $newarray(wit->$class->elem_type,shape->length,size,shape,strides,true);
  for (int i=0; i<size; i++)
    res->data[i] = wit->$class->from$obj(val);
  return res;
}

#define MAX 1000000000
// This is a simpleminded pseudo-random number generator; should be replaced. (srand is called in numpy$$__init__)
numpy$$ndarray numpy$$unirand($float a, $float b, $int n) {
  if (a->val >= b->val)
     RAISE(($BaseException)$NEW($ValueError,to$str("lower limit not smaller than upper in numpy.unirand")));
  $list shape = $NEW($list,NULL,NULL);
  $list_append(shape,n);
  $list strides = $NEW($list,NULL,NULL);
  $list_append(strides,to$int(1));
  double s = (b->val - a->val);
  numpy$$ndarray res = $newarray(DblType,1,n->val,shape,strides,true);
  for (int i = 0; i<n->val; i++)
    res->data[i].d = a->val + s * (double)arc4random_uniform(MAX)/(double)MAX;
  return res;
}

// Functions over arrays /////////////////////////////////////////////////

// Most of these are yet only defined with default parameters.

numpy$$ndarray numpy$$partition(numpy$$Primitive wit, numpy$$ndarray a, $int k) {
  numpy$$ndarray res = numpy$$ndarray$copy(a);
  res->ndim--;
  numpy$$array_iterator_state it = $mk_iterator(res); //gives an iterator that successively selects start of each last dimension column.
  res->ndim++;
  for (int i=0; i < $LONGELEM(res->shape,res->ndim-2); i++) {
    union $Bytes8 *start =iter_next(it);
    quickselect(start,0,$LONGELEM(res->shape,res->ndim-1)-1,k->val,wit->$class->$lt);
  }
  return res;
}

numpy$$ndarray numpy$$sort(numpy$$Primitive wit, numpy$$ndarray a, $int axis) {
  numpy$$ndarray res = numpy$$ndarray$copy(a);
  if (!axis) {
    quicksort(res->data,0,res->size-1,wit->$class->$lt);
    $list newshape = $list_new(1);
    $list_append(newshape,to$int(a->size));
    $list newstrides = $list_new(1);
    $list_append(newstrides,to$int(1));
    res->shape = newshape;
    res->strides = newstrides;
    res->ndim = 1;
  } else if (axis->val == -1 || axis->val == a->ndim-1) {
    res->ndim--;
    numpy$$array_iterator_state it = $mk_iterator(res); //gives an iterator that successively selects start of each last dimension column.
    res->ndim++;
    union $Bytes8 *start;
    while ((start = iter_next(it))) 
      quicksort(start,0,$LONGELEM(res->shape,res->ndim-1)-1,wit->$class->$lt);
  } else
    RAISE(($BaseException)$NEW($ValueError,to$str("sorting only implemented for complete array or along last axis")));
  return res;
}


numpy$$ndarray numpy$$clip(numpy$$Primitive wit, numpy$$ndarray a, $WORD low, $WORD high) {
  numpy$$ndarray res = numpy$$ndarray$copy(a);
  numpy$$array_iterator_state it = $mk_iterator(res);
  union $Bytes8 lo, hi, x, *ix, *ixres = res->data;
  if (low) lo = wit->$class->from$obj(low);
  if (high) hi = wit->$class->from$obj(high);
  while((ix = iter_next(it))) {
    x = *ix;
    if (low && wit->$class->$lt(x,lo)) 
      *ixres = lo;
    else if (high && wit->$class->$lt(hi,x))
      *ixres = hi;
    else
      *ixres = x;
    ixres++;
  }
  return res;
}

union $Bytes8 $dot1dim(numpy$$Primitive wit, union $Bytes8 *a, union $Bytes8 *b, long size, long stridea, long strideb) {
  // a has stride 1 since it is copied except for dot(a,b) when both a and b are 1dim
  union $Bytes8 res = (union $Bytes8)0L;
  union $Bytes8 *ixa, *ixb, *ixaend;
  ixa = a;
  ixaend = ixa + size*stridea;
  ixb = b;
   while (ixa < ixaend) {
     wit->$class->$iadd(&res,wit->$class->$mul(*ixa,*ixb));
    ixa += stridea;
    ixb += strideb;
  }
  return res;
}
 
numpy$$ndarray numpy$$dot(numpy$$Primitive wit, numpy$$ndarray a, numpy$$ndarray b) {
  numpy$$ndarray res;
  if (a->ndim==0 || b->ndim==0) {
    // following  Python's numpy, we multiply elementwise...
    numpy$$Integral$ndarray wit2 = $NEW(numpy$$Integral$ndarray,wit);
    res = wit2->$class->__mul__(wit2,a,b);
  } else if (b->ndim==1) {
    long len = $LONGELEM(b->shape,0);
    if  ($LONGELEM(a->shape,a->ndim-1) != len)
      RAISE(($BaseException)$NEW($ValueError,to$str("array sizes in numpy.dot do not match")));
    long stridea = $LONGELEM(a->strides,a->ndim-1);
    long strideb = $LONGELEM(b->strides,0);
    $list newshape = $list_getslice(a->shape,$NEW($slice,NULL,to$int(-1),NULL));
    res = $newarray(a->elem_type,a->ndim-1,$prod(newshape),newshape,$mk_strides(newshape),true);
    if (a->ndim==1) {
      res->data[0] = $dot1dim(wit, &a->data[a->offset],&b->data[b->offset],len,stridea,strideb);
    } else {
      union $Bytes8 *ixres = res->data, *ixa;
      a->ndim--;
      numpy$$array_iterator_state ita = $mk_iterator(a);
      a->ndim++;
      while ((ixa = iter_next(ita))) {
        *ixres = $dot1dim(wit,ixa,b->data,len,stridea,strideb);
        ixres++;
      }
    }
  } else { //b->ndim > 1. Following Python we should do a sum product over last axis of a and 2nd to last axis of b ??????
    RAISE(($BaseException)$NEW($ValueError,to$str("numpy.dot not implemented for ndim of second arg > 1")));    
  } 
  return res; 
}

union $Bytes8 $sum1dim(numpy$$Primitive wit, union $Bytes8 *a, long size, long stridea) { 
  union $Bytes8 res = (union $Bytes8)0L;
  union $Bytes8 *ixa, *ixaend;
  ixa = a;
  ixaend = ixa + size*stridea;
   while (ixa < ixaend) {
     wit->$class->$iadd(&res,*ixa);
    ixa += stridea;
  }
  return res;
}


numpy$$ndarray numpy$$sum(numpy$$Primitive wit, numpy$$ndarray a, $int axis) {
  numpy$$ndarray res;
  if(!axis) {
    union $Bytes8 resd = (union $Bytes8) 0L;
    if ($is_contiguous(a)) {
      for (int i=0; i<a->size; i++)
        wit->$class->$iadd(&resd,a->data[i]); 
    } else {
      numpy$$array_iterator_state it = $mk_iterator(a);
      union $Bytes8 *ixa;
      while ((ixa = iter_next(it)))
        wit->$class->$iadd(&resd,*ixa);
    }
   res = $newarray(a->elem_type,0,1,$NEW($list,NULL,NULL),$NEW($list,NULL,NULL),true);
   res->data[0] = resd;
   return res;
   } else if (axis->val == -1 || axis->val == a->ndim-1) {
    // for now, assume summing along last axis
    long len = $LONGELEM(a->shape,a->ndim-1);
    long stridea = $LONGELEM(a->strides,a->ndim-1);
    $list newshape = $list_getslice(a->shape,$NEW($slice,NULL,to$int(-1),NULL));
    res = $newarray(a->elem_type,a->ndim-1,$prod(newshape),newshape,$mk_strides(newshape),true);
    a->ndim--;
    numpy$$array_iterator_state ita = $mk_iterator(a);
    a->ndim++;
    union $Bytes8 *ixa;
    union $Bytes8 *ixres = res->data;
    while ((ixa = iter_next(ita))) {
      *ixres = $sum1dim(wit,ixa,len,stridea);
      ixres++;
    }
  } else
     RAISE(($BaseException)$NEW($ValueError,to$str("summing only implemented for complete array or along last axis")));
    return res;
}         

numpy$$ndarray numpy$$abs(numpy$$Primitive wit, numpy$$ndarray a) {
  return numpy$$func(wit->$class->$abs,a);
}

$WORD numpy$$scalar (numpy$$Primitive wit, numpy$$ndarray a) {
  if (a->ndim > 0)
     RAISE(($BaseException)$NEW($ValueError,to$str("scalar only for zero-dim arrays")));
  return wit->$class->to$obj(a->data[0]);
}


// Iterator over ndarrays ////////////////////////////////////////////////////////////////////////

void numpy$$Iterator$init(numpy$$Iterator$ndarray self, numpy$$Primitive pwit, numpy$$ndarray a) {
  self->pwit = pwit;
  self->it = $mk_iterator(a);
}

numpy$$Iterator$ndarray numpy$$Iterator$ndarray$new(numpy$$Primitive pwit, numpy$$ndarray a) {
  numpy$$Iterator$ndarray res = malloc(sizeof(struct numpy$$Iterator$ndarray ));
  res->$class = &numpy$$Iterator$ndarray$methods;
  numpy$$Iterator$init(res,pwit,a);
  return res;
}

$bool numpy$$Iterator$bool(numpy$$Iterator$ndarray self) {
  return $True;
}

$str numpy$$Iterator$str(numpy$$Iterator$ndarray self) {
  char *s;
  asprintf(&s,"<ndarray iterator object at %p>",self);
  return to$str(s);
}

$WORD numpy$$Iterator$ndarray$__next__(numpy$$Iterator$ndarray self) {
  union $Bytes8 *n = iter_next(self->it);
  return n ? self->pwit->$class->to$obj(*n) : NULL;
}

void numpy$$Iterator$$serialize(numpy$$Iterator$ndarray self,$Serial$state state) {
  RAISE(($BaseException)$NEW($ValueError,to$str("(de)serialization not implemented for ndarray iterators")));
}

numpy$$Iterator$ndarray numpy$$Iterator$ndarray$_deserialize($Serial$state state) {
  RAISE(($BaseException)$NEW($ValueError,to$str("(de)serialization not implemented for ndarray iterators")));
   return NULL;
}

struct numpy$$Iterator$ndarray$class numpy$$Iterator$ndarray$methods = {"",UNASSIGNED,($Super$class)&$Iterator$methods, numpy$$Iterator$init,
                                                      numpy$$Iterator$$serialize, numpy$$Iterator$ndarray$_deserialize,numpy$$Iterator$bool,numpy$$Iterator$str,numpy$$Iterator$ndarray$__next__};

$int numpy$$newaxis;
