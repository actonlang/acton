
//Select element #n in lst which is a list[int].
#define $LONGELEM(lst,n)   ((($int)lst->data[n])->val)

$array_iterator_state $mk_iterator($ndarray a);
union $Bytes8 *iter_next($array_iterator_state it);


// Auxiliary functions ///////////////////////////////////////////////////////////////////////////////

// method for creating ndarray structs.
// res->offset gets default value 0 (may be unsuitable when allocate_data = false)

static $ndarray $newarray(enum ElemType typ, long ndim,long size,$list shape,$list strides,bool allocate_data) {
  $ndarray res = malloc(sizeof(struct $ndarray));
  res->$class = &$ndarray$methods;
  res->elem_type = typ;
  res->ndim = ndim;
  res->size = size;
  res->offset = 0;
  res->shape = shape;
  res->strides = strides;
  if (allocate_data) res->data = malloc(size * elem_size(typ) * sizeof(union $Bytes8));
  return res;
}

long $prod($list lst) {
  long res = 1;
  for (int i = 0; i<lst->length; i++)
    res *= $LONGELEM(lst,i);
  return res;
}

bool $is_contiguous($ndarray a) {
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

void $ndarray__init__($ndarray a, $WORD w) {
  $ndarray r = $ndarray_fromatom(w);
  memcpy(a,r,sizeof(struct $ndarray));
}


$str $ndarray__str__($ndarray a) {
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
      ix->data[0] = to$int(i);
      $ndarray b = $ndarray_getslice(a,ix);
      $list_append(strs,$ndarray__str__(b));
    }
    $str s = $str_join_par('[',strs,']');
    $Plus wit = ($Plus)$Plus$str$witness;
    return wit->$class->__add__(wit,s,to$str("\n"));
  }
}

//ndarray methods /////////////////////////////////////////////////////////////////////////////////

// reshape attempts to present a new view, but may have to copy data.

$ndarray $ndarray_reshape($ndarray a, $list newshape) {
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
  $ndarray res = $newarray(a->elem_type,newshape->length,a->size,newshape,newstrides,false);
  if (!needcopy) {
    res->offset = a->offset;
    res->data = a->data;
    return res;
  } else {
    res->data = malloc(size * sizeof(union $Bytes8));
    union $Bytes8 *ixres = res->data;
    union $Bytes8 *ixa;
    $array_iterator_state ita = $mk_iterator(a);
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
$ndarray $ndarray_transpose($ndarray a, $list axes) {
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
  $ndarray res = $newarray(a->elem_type,a->ndim,a->size,newshape,newstrides,false);
  res->data = a->data;
  return res;
}

$ndarray $ndarray_flatten($ndarray a) {
  $list newshape = $NEW($list,NULL,NULL);
  $list_append(newshape,to$int(a->size));
  return $ndarray_reshape(a,newshape);
}

// Makes a contiguous deep copy of its argument with stride of last dimension == 1.

$ndarray $ndarray_copy($ndarray a) {
  $ndarray res = $newarray(a->elem_type,a->ndim,a->size,a->shape,$mk_strides(a->shape),true);
  $array_iterator_state it = $mk_iterator(a);
  union $Bytes8 *ixres, *ixa;
  ixres = res->data;
  while ((ixa = iter_next(it))) {
    *ixres = *ixa;
    ixres++;
  }  
  return res;
}

// basic slicing 

$ndarray $ndarray_getslice($ndarray a, $list ix) {
  // assert length of ix > 0
  int nulls = 0;
  int ints = 0;
  int slices = 0;
  // first analyze index list contents
  for (int i=0; i < ix->length; i++) {
    $Initializable ixi = ($Initializable)ix->data[i];
    if (!ixi)
      nulls++;
    else {
      if ($ISINSTANCE(ixi,$int)->val)
        ints++; 
      else
        if ($ISINSTANCE(ixi,$Slice)->val)
          slices++;
        else {
          fprintf(stderr,"internal error: unexpected type of ndarray index element\n");
          exit(-1);
        }
    }
  }
  if (ints+slices > a->ndim)
    RAISE(($BaseException)$NEW($ValueError,to$str("indexing too many dimensions")));
  int ndim = a->ndim + nulls - ints;
  $ndarray res = $newarray(a->elem_type,ndim,0,$list_new(ndim),$list_new(ndim),false); // size 0 is temporary
  res->shape->length = ndim;
  res->strides->length = ndim;
  res->data = a->data;
  int offset = a->offset;
  $Slice allSlice = $NEW($Slice,NULL,NULL,NULL);
  int ixpos = ix->length-1;
  int apos = a->ndim-1;
  int respos = ndim-1;
  int untouched = a->ndim - (ints + slices);
  $Initializable currindex;
  long currstride = a->elem_size;
  while (ixpos>=0) {
    if (untouched-- > 0)
      currindex =  ($Initializable)allSlice;
    else
      currindex = ($Initializable)ix->data[ixpos--];
    if (!currindex) {
      $list_setitem(res->strides,respos,to$int(0));
      $list_setitem(res->shape,respos--,to$int(1));
    } else {
      if ($ISINSTANCE(currindex,$int)->val) {
        currstride = $LONGELEM(a->strides,apos--);
        offset += currstride * ((($int)currindex)->val);
      } else if ($ISINSTANCE(currindex,$Slice)->val) {
        int slen,start,stop,step;
        normalize_slice(($Slice)currindex,$LONGELEM(a->shape,apos),&slen,&start,&stop,&step);
        currstride = $LONGELEM(a->strides,apos);
        offset += start * currstride;
        $list_setitem(res->strides,respos,to$int($LONGELEM(a->strides,apos--) * step));
        $list_setitem(res->shape,respos--,to$int(slen));
      } else {
        fprintf(stderr,"internal error: unexpected type of ndarray index element\n");
        exit(-1);
      }
    }
  }
  res->offset = offset;
  res->size = $prod(res->shape);
  return res;
}

struct $ndarray$class $ndarray$methods = {"",UNASSIGNED,($Super$class)&$struct$methods,$ndarray__init__,NULL,NULL,NULL,$ndarray__str__,$ndarray_reshape,$ndarray_transpose,$ndarray_copy,$ndarray_getslice};

// Iterating over an ndarray //////////////////////////////////////////////////////////////////////////////

// Aims to be as fast as possible; used in many methods below and in protocol implementations 
$array_iterator_state $mk_iterator($ndarray a) {
  $array_iterator_state res = malloc(sizeof(struct $array_iterator_state));
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

union $Bytes8 *iter_next($array_iterator_state it) {
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

$ndarray $ndarray_func(union $Bytes8(*f)(union $Bytes8),$ndarray a) {
  $list resstrides = $mk_strides(a->shape);
  $ndarray res = $newarray(a->elem_type,a->ndim,a->size,a->shape,resstrides,true); 
  union $Bytes8 *ixres = res->data;
  union $Bytes8 *ixa;
  $array_iterator_state it = $mk_iterator(a);
  while ((ixa = iter_next(it))) {
    *ixres = f(*ixa);
    ixres++;
  }
  return res;
}

// broadcasting to adapt two arrays to common shape before mapping anoperator.

// returns the common extended shape and, as outparams, iterators for the two extended operands

$ndarray $ndarray_broadcast($ndarray a1, $ndarray a2, $array_iterator_state *it1, $array_iterator_state *it2) {
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
  $ndarray res = $newarray(a1->elem_type,len,$prod(resshape),resshape,$mk_strides(resshape),true);
  $ndarray tmparr = $newarray(a1->elem_type,len,res->size,res->shape,strides1,false); // only for making iterators
  tmparr->offset = a1->offset; 
  tmparr->data = a1->data;
  *it1 = $mk_iterator(tmparr);
  tmparr->strides = strides2;
  tmparr->offset = a2->offset;
  tmparr->data = a2->data;
  *it2 = $mk_iterator(tmparr);
  return res;
}
 
$ndarray $ndarray_oper(union $Bytes8 (*f)(union $Bytes8, union $Bytes8),$ndarray a, $ndarray b) {
  union $Bytes8 *ix1, *ix2, *ixres;
  long stride1, stride2, len;
  $array_iterator_state it1, it2;
  $ndarray res = $ndarray_broadcast(a,b,&it1,&it2);
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

$ndarray $ndarray_fromatom($WORD a) {
  if ($ISINSTANCE(($Super)a,$int)->val) {
    $ndarray res = $newarray(LongType,0,1,$NEW($list,NULL,NULL),$NEW($list,NULL,NULL),true);
    res->data->l = (($int)a)->val;
    return res;
  }
  if ($ISINSTANCE(($Super)a,$float)->val) {
    $ndarray res = $newarray(DblType,0,1,$NEW($list,NULL,NULL),$NEW($list,NULL,NULL),true);
    res->data->d = (($float)a)->val;
    return res;
  }
  if ($ISINSTANCE(($Super)a,$bool)->val) return NULL;
  if ($ISINSTANCE(($Super)a,$str)->val) return NULL;
  fprintf(stderr,"internal error: ndarray_fromatom: argument not of atomic type");
  exit(-1);
}

// Functions to create arrays /////////////////////////////////////////////////////////////

// n evenly spaced floats between a and b

$ndarray $ndarray_linspace($float a, $float b, $int n) {
  $list shape = $NEW($list,NULL,NULL);
  $list_append(shape,n);
  $list strides = $NEW($list,NULL,NULL);
  $list_append(strides,to$int(1));
  $ndarray res = $newarray(DblType,1,n->val,shape,strides,true);
  double step = (b->val - a->val)/(n->val-1);
  for (long i = 0; i<n->val; i++) {
    res->data[i].d = a->val + i * step;
  }
  return res;
}

// array of ints described by a range

$ndarray $ndarray_arange($int start, $int stop, $int step) {
  $Collection$range wit = ($Collection$range)$Sequence$range$witness->w$Collection;
  $range r = $NEW($range,start,stop,step);
  $int len = wit->$class->__len__(wit,r);
  $list shape = $NEW($list,NULL,NULL);
  $list_append(shape,len);
  $list strides = $NEW($list,NULL,NULL);
  $list_append(strides,to$int(1));
  $ndarray res = $newarray(LongType,1,len->val,shape,strides,true);
  $Iterator it = wit->$class->__iter__(wit,r);
  $WORD elem;
  int i=0;
  while ((elem = it->$class->__next__(it))) {
    res->data[i].l = (($int)elem)->val;
    i++;
  }
  return res;
}  

// make an array from a list

$ndarray $ndarray_array($Primitive wit, $list elems) {
  $list shape = $NEW($list,NULL,NULL);
  $list_append(shape,to$int(elems->length));
  $list strides = $NEW($list,NULL,NULL);
  $list_append(strides,to$int(1));
  if (elems->length == 0)
    RAISE(($BaseException)$NEW($ValueError,to$str("function array cannot create empty ndarray")));
  $ndarray res = $newarray(wit->$class->elem_type,1,elems->length,shape,strides,true);
  for (int i=0; i<elems->length; i++) 
    res->data[i] = wit->$class->from$obj($list_getitem(elems,i));
  return res;
}


// Functions over arrays /////////////////////////////////////////////////

// Most of these are yet only defined with default parameters.

$ndarray $ndarray_partition($Primitive wit, $ndarray a, $int k) {
  $ndarray res = $ndarray_copy(a);
  res->ndim--;
  $array_iterator_state it = $mk_iterator(res); //gives an iterator that successively selects start of each last dimension column.
  res->ndim++;
  for (int i=0; i < $LONGELEM(res->shape,res->ndim-2); i++) {
    union $Bytes8 *start =iter_next(it);
    quickselect(start,0,$LONGELEM(res->shape,res->ndim-1)-1,k->val,wit->$class->$lt);
  }
  return res;
}

$ndarray $ndarray_sort($Primitive wit, $ndarray a, $int axis) {
  $ndarray res = $ndarray_copy(a);
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
    $array_iterator_state it = $mk_iterator(res); //gives an iterator that successively selects start of each last dimension column.
    res->ndim++;
    union $Bytes8 *start;
    while ((start = iter_next(it))) 
      quicksort(start,0,$LONGELEM(res->shape,res->ndim-1)-1,wit->$class->$lt);
  } else
    RAISE(($BaseException)$NEW($ValueError,to$str("sorting only implemented for complete array or along last axis")));
  return res;
}


$ndarray $ndarray_clip($Primitive wit, $ndarray a, $WORD low, $WORD high) {
  $ndarray res = $ndarray_copy(a);
  $array_iterator_state it = $mk_iterator(res);
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

union $Bytes8 $dot1dim($Primitive wit, union $Bytes8 *a, union $Bytes8 *b, long size, long stridea, long strideb) {
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
 
$ndarray $ndarray_dot($Primitive wit, $ndarray a, $ndarray b) {
  $ndarray res;
  if (a->ndim==0 || b->ndim==0) {
    // following  Python's numpy, we multiply elementwise...
    $Integral$ndarray wit2 = $NEW($Integral$ndarray,wit);
    res = wit2->$class->__mul__(wit2,a,b);
  } else if (b->ndim==1) {
    long len = $LONGELEM(b->shape,0);
    if  ($LONGELEM(a->shape,a->ndim-1) != len)
      RAISE(($BaseException)$NEW($ValueError,to$str("array sizes in numpy.dot do not match")));
    long stridea = $LONGELEM(a->strides,a->ndim-1);
    long strideb = $LONGELEM(b->strides,0);
    $list newshape = $list_getslice(a->shape,$NEW($Slice,NULL,to$int(-1),NULL));
    res = $newarray(a->elem_type,a->ndim-1,$prod(newshape),newshape,$mk_strides(newshape),true);
    if (a->ndim==1) {
      res->data[0] = $dot1dim(wit, &a->data[a->offset],&b->data[b->offset],len,stridea,strideb);
    } else {
      union $Bytes8 *ixres = res->data, *ixa;
      a->ndim--;
      $array_iterator_state ita = $mk_iterator(a);
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

union $Bytes8 $sum1dim($Primitive wit, union $Bytes8 *a, long size, long stridea) { 
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


$ndarray $ndarray_sum($Primitive wit, $ndarray a, $int axis) {
  $ndarray res;
  if(!axis) {
    union $Bytes8 resd = (union $Bytes8) 0L;
   $array_iterator_state it = $mk_iterator(a);
   union $Bytes8 *ixa;
   while ((ixa = iter_next(it)))
     wit->$class->$iadd(&resd,*ixa); 
   res = $newarray(a->elem_type,0,1,$NEW($list,NULL,NULL),$NEW($list,NULL,NULL),true);
   res->data[0] = resd;
   return res;
   } else if (axis->val == -1 || axis->val == a->ndim-1) {
    // for now, assume summing along last axis
    long len = $LONGELEM(a->shape,a->ndim-1);
    long stridea = $LONGELEM(a->strides,a->ndim-1);
    $list newshape = $list_getslice(a->shape,$NEW($Slice,NULL,to$int(-1),NULL));
    res = $newarray(a->elem_type,a->ndim-1,$prod(newshape),newshape,$mk_strides(newshape),true);
    a->ndim--;
    $array_iterator_state ita = $mk_iterator(a);
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

$ndarray $ndarray_abs($Primitive wit, $ndarray a) {
  return $ndarray_func(wit->$class->$abs,a);
}

$WORD $ndarray_scalar ($Primitive wit, $ndarray a) {
  if (a->ndim > 0)
     RAISE(($BaseException)$NEW($ValueError,to$str("scalar only for zero-dim arrays")));
  return wit->$class->to$obj(a->data[0]);
}


// Iterator over ndarrays ////////////////////////////////////////////////////////////////////////

void $Iterator$ndarray_init($Iterator$ndarray self, $Primitive pwit, $ndarray a) {
  self->pwit = pwit;
  self->it = $mk_iterator(a);
}

$bool $Iterator$ndarray_bool($Iterator$ndarray self) {
  return $True;
}

$str $Iterator$ndarray_str($Iterator$ndarray self) {
  char *s;
  asprintf(&s,"<ndarray iterator object at %p>",self);
  return to$str(s);
}

$WORD $Iterator$ndarray$__next__($Iterator$ndarray self) {
  union $Bytes8 *n = iter_next(self->it);
  return n ? self->pwit->$class->to$obj(*n) : NULL;
}

void $Iterator$ndarray_serialize($Iterator$ndarray self,$Serial$state state) {
  RAISE(($BaseException)$NEW($ValueError,to$str("(de)serialization not implemented for ndarrays")));
}

$Iterator$ndarray $Iterator$ndarray$_deserialize($Serial$state state) {
  RAISE(($BaseException)$NEW($ValueError,to$str("(de)serialization not implemented for ndarrays")));
   return NULL;
}

struct $Iterator$ndarray$class $Iterator$ndarray$methods = {"",UNASSIGNED,($Super$class)&$Iterator$methods, $Iterator$ndarray_init,
                                                      $Iterator$ndarray_serialize, $Iterator$ndarray$_deserialize,$Iterator$ndarray_bool,$Iterator$ndarray_str,$Iterator$ndarray$__next__};

