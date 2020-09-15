#include "ndarray.h"

//find element #n in lst which is a list[int].
#define $LONGELEM(lst,n)   ((($int)lst->data[n])->val)

// Some arithmetic operators. To be replaced by number protocol implementations. ////////////////////////

union $Bytes8 mul2(union $Bytes8 x) {
  union $Bytes8 res;
  res.l = 2 * x.l;
  return res;
}
        
// Auxiliary method for creating ndarray structs.
// Note that elem_size and offset have default values and that data = NULL;

static $ndarray $nullarray(long ndim, to$obj_converter to$obj, from$obj_converter from$obj, $list shape, $list strides) {
  $ndarray res = malloc(sizeof(struct $ndarray));
  res->$class = &$ndarray$methods;
  res->ndim = ndim;
  res->elem_size = 1;
  res->to$obj = to$obj;
  res->from$obj = from$obj;
  res->offset = 0;
  res->shape = shape;
  res->strides = strides;
  res->data = NULL;
  return res;
}

long $prod($list lst) {
  long res = 1;
  for (int i = 0; i<lst->length; i++)
    res *= $LONGELEM(lst,i);
  return res;
}
  
void $ndarray__init__($ndarray a, $WORD w) {
  $ndarray r = $ndarray_fromatom(w);
  memcpy(a,r,sizeof(struct $ndarray));
}


$str $ndarray__str__($ndarray a) {
  if (a->ndim==0) {
    $struct obj = a->to$obj(a->data[a->offset]);
    return obj->$class->__str__(obj);
  } else {
    $list strs = $NEW($list,NULL,NULL);
    $list ix = $list_new(1);
    ix->length = 1;
    for (long i = 0; i< $LONGELEM(a->shape,0); i++) {
      ix->data[0] = to$int(i);
      $ndarray b = $nd_getslice(a,ix);
      $list_append(strs,$ndarray__str__(b));
    }
    return  $str_join_par('[',strs,']');
  }
}

struct $ndarray$class $ndarray$methods = {"",UNASSIGNED,($Super$class)&$struct$methods,$ndarray__init__,NULL,NULL,NULL,$ndarray__str__};

// reshaping an ndarray ///////////////////////////////////////////////////////////////////////////////////

static void $fill_new_array(long startdim, union $Bytes8 **resdata, union $Bytes8 **adata, $ndarray a) {
  long aincr0;
  switch (a->ndim-startdim) {
  case 0:
    **resdata = **adata;
    break;
  case 1:
    aincr0 = $LONGELEM(a->strides,startdim);
    for (long i = 0; i < $LONGELEM(a->shape,startdim); i++) {
      **resdata = **adata;
      *adata += aincr0;
      *resdata += 1;
    }
    break;
  default:
    aincr0 = $LONGELEM(a->strides,startdim) - $LONGELEM(a->shape,startdim + 1) * $LONGELEM(a->strides,startdim + 1);
    for (long i = 0; i < $LONGELEM(a->shape,startdim); i++) {
      $fill_new_array(startdim+1,resdata,adata,a);
      *adata += aincr0;
    }
  }
}

$ndarray $ndarray_reshape($ndarray a, $list newshape) {
  long size = $prod(newshape);
  if ($prod(a->shape) != size)
    RAISE(($BaseException)$NEW($ValueError,to$str("wrong number of array elements for reshape")));
  if (a->shape->length == newshape->length) {
    // Check if newshape is actually equal to a->shape.
    int sameshape = 1;
    int i = 0;
    while (sameshape && i < newshape->length)
      if ($LONGELEM(a->shape,i) != $LONGELEM(newshape,i))
        sameshape = 0;
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
  // Compute strides array of result. Computation starts from last element, which is computed
  // differently if we reuse a->data than if we make a fresh array.
  $list newstrides = $list_new(newshape->length);
  newstrides->length = newshape->length;
  newstrides->data[newstrides->length-1] = needcopy ? to$int(1) : a->strides->data[a->strides->length-1];
  for (int i = newstrides->length-2; i>=0; i--)
    newstrides->data[i] = to$int($LONGELEM(newstrides,i+1) * $LONGELEM(newshape,i+1));
  // Build result
  $ndarray res = $nullarray(newshape->length,a->to$obj,a->from$obj,newshape,newstrides);
  res->elem_size = a->elem_size;
  if (!needcopy) {
    res->offset = a->offset;
    res->data = a->data;
    return res;
  } else {
    res->offset = 0;
    res->data = malloc(size * sizeof(union $Bytes8));
    union $Bytes8 *resdata = &res->data[0];
    union $Bytes8 *adata = &a->data[a->offset];
    $fill_new_array(0,&resdata,&adata,a);
    return res;
  }
}  

// basic slicing ////////////////////////////////////////////////////////////////////////////////////////////

$ndarray $nd_getslice($ndarray a, $list ix) {
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
      if ($ISINSTANCE(ixi,$int))
        ints++; 
      else
        if ($ISINSTANCE(ixi,$Slice))
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
  $ndarray res = $nullarray(ndim,a->to$obj,a->from$obj,$list_new(ndim),$list_new(ndim));
  res->shape->length = ndim;
  res->strides->length = ndim;
  res->elem_size= a->elem_size;
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
      if ($ISINSTANCE(currindex,$int)) {
        currstride = $LONGELEM(a->strides,apos--);
        offset += currstride * ((($int)currindex)->val);
      } else if ($ISINSTANCE(currindex,$Slice)) {
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
  return res;
}

// auxiliary function $ndarray func1 for distributing unary functions over an ndarray ////////////////////////////////////////

static void $fill_func_array(union $Bytes8(*f)(union $Bytes8),long startdim, union $Bytes8 **resdata, union $Bytes8 **adata, $ndarray a) {
  long aincr0;
  switch (a->ndim-startdim) {
  case 0:
    **resdata = f(**adata);
    break;
  case 1:
    aincr0 = $LONGELEM(a->strides,startdim);
    for (long i = 0; i < $LONGELEM(a->shape,startdim); i++) {
      **resdata = f(**adata);
      *adata += aincr0;
      *resdata += 1;
    }
    break;
  default:
    aincr0 = $LONGELEM(a->strides,startdim) - $LONGELEM(a->shape,startdim + 1) * $LONGELEM(a->strides,startdim + 1);
    for (long i = 0; i < $LONGELEM(a->shape,startdim); i++) {
      $fill_func_array(f,startdim+1,resdata,adata,a);
      *adata += aincr0;
    }
  }
}

$ndarray $ndarray_func1(union $Bytes8(*f)(union $Bytes8),$ndarray a) { // for functions where arg and result are eight bytes
  long size = 1;
  $list resstrides = $list_new(a->shape->length);
  resstrides->length = a->shape->length;
  for (int i = a->shape->length-1; i>=0; i--) {
    long s = from$int($list_getitem(a->shape, i));
    $list_setitem(resstrides,i,to$int(s > 1 ? size : 0));
    size *= s;
  }
  $ndarray res = $nullarray(a->ndim,a->to$obj,a->from$obj,$list_copy(a->shape),resstrides); // do we need to copy the shape?
  res->data = malloc(size * sizeof(union $Bytes8));
  union $Bytes8 *resdata = &res->data[0];
  union $Bytes8 *adata = &a->data[a->offset];
  $fill_func_array(f,0,&resdata,&adata,a);
  return res;
}


// broadcasting ////////////////////////////////////////////////////////////////////////////////////////

// on input, shapei and stridesi must be copies of the shapes and strides of operands.
// return value is common extended shape.
// on exit, stridesi have been extended with zeros in new dimensions.
$list $nd_broadcast($list shape1, $list shape2, $list strides1, $list strides2) {
  int len1 = shape1->length;
  int len2 = shape2->length;
  int len;
  if (len1 < len2) {
    for (int i=0; i< len2-len1; i++) {
      $list_insert(shape1,0,to$int(1));
      $list_insert(strides1,0,to$int(0));
    }
    len = len2;
  } else if (len2 < len1) {
    for (int i=0; i< len1-len2; i++) {
      $list_insert(shape2,0,to$int(1));
      $list_insert(strides2,0,to$int(0));
    }
    len = len1;
  } else
    len = len1;
  $list res = $list_new(len);
  res->length = len;
  for (int i=0; i<len; i++) {
    if ($LONGELEM(shape1,i) == $LONGELEM(shape2,i))
      res->data[i] = shape1->data[i];
    else if ($LONGELEM(shape1,i) == 1)
      res->data[i] = shape2->data[i];
    else if ($LONGELEM(shape2,i) == 1)
      res->data[i] = shape1->data[i];
    else
      RAISE(($BaseException)$NEW($ValueError,to$str("ndarray broadcasting: shapes do not match")));
  }
  return res;
}

// auxiliary method $ndarray_oper1 for binary operators over ndarrays //////////////////////////////////


static void $fill_oper1_array(union $Bytes8 (*f)(union $Bytes8,union $Bytes8), long startdim,
                      union $Bytes8 **resdata, union $Bytes8 **adata, union $Bytes8 **bdata,
                      $list shape, $list astrides, $list bstrides) {
  long shape1, aincr0, bincr0;
  switch (shape->length - startdim) {
    case 0:
      **resdata = f(**adata,**bdata);
      break;
    case 1:
      aincr0 = $LONGELEM(astrides,startdim);
      bincr0 = $LONGELEM(bstrides,startdim);
      for (long i = 0; i < $LONGELEM(shape,startdim); i++) {
        **resdata = f(**adata,**bdata);
        //(*rd).d = (*ad).d + (*bd).d;
        *resdata += 1;
        *adata += aincr0;
        *bdata += bincr0;
      }
      break;
  //case 2:
  //case 3: we tried a looping base case up to 3 dim's before turning to recursion, but found no evidence that it's worth the extra code.
    default:
      shape1 = $LONGELEM(shape,startdim + 1);
      aincr0 = $LONGELEM(astrides,startdim) - shape1 * $LONGELEM(astrides,startdim + 1);
      bincr0 = $LONGELEM(bstrides,startdim) - shape1 * $LONGELEM(bstrides,startdim + 1);
      for (int i=0; i < $LONGELEM(shape,startdim); i++) {
        $fill_oper1_array(f,startdim+1,resdata,adata,bdata,shape,astrides,bstrides);
        *adata += aincr0;
        *bdata += bincr0;
      }
    }
}

$ndarray $ndarray_oper1(union $Bytes8 (*f)(union $Bytes8,union $Bytes8),$ndarray a, $ndarray b) {
  $list tmpshapea = $list_copy(a->shape);
  $list tmpstridesa = $list_copy(a->strides);
  $list tmpshapeb = $list_copy(b->shape);
  $list tmpstridesb = $list_copy(b->strides);
  $list resshape = $nd_broadcast(tmpshapea, tmpshapeb, tmpstridesa, tmpstridesb);
  long size = 1;
  $list resstrides = $list_new(resshape->length);
  resstrides->length = resshape->length;
  for (int i = resshape->length-1; i>=0; i--) {
    long s = from$int($list_getitem(resshape, i));
    $list_setitem(resstrides,i,to$int(s > 1 ? size : 0));
    size *= s;
  }
  $ndarray res = $nullarray(resshape->length,a->to$obj,a->from$obj,resshape,resstrides);
  res->data = malloc(size * sizeof(union $Bytes8));
  union $Bytes8 *resdata = &res->data[0];
  union $Bytes8 *adata = &a->data[a->offset];
  union $Bytes8 *bdata = &b->data[b->offset];
  $fill_oper1_array(f,0,&resdata,&adata,&bdata,resshape,tmpstridesa,tmpstridesb);
  return res;
}

// ndarray methods ////////////////////////////////////////////////////////////////////////////////

$ndarray $ndarray_fromatom($Super a) {
  if ($ISINSTANCE(a,$int)) {
    $ndarray res = $nullarray(0,(to$obj_converter)to$int,(from$obj_converter)from$int,$NEW($list,NULL,NULL),$NEW($list,NULL,NULL));
    res->data = malloc(sizeof(union $Bytes8));
    res->data->l = (($int)a)->val;
    return res;
  }
  if ($ISINSTANCE(a,$float)) {
    $ndarray res = $nullarray(0,(to$obj_converter)to$float,(from$obj_converter)from$float,$NEW($list,NULL,NULL),$NEW($list,NULL,NULL));
    res->data = malloc(sizeof(union $Bytes8));
    res->data->d = (($float)a)->val;
    return res;
  }
  if ($ISINSTANCE(a,$bool)) return NULL;
  if ($ISINSTANCE(a,$str)) return NULL;
  fprintf(stderr,"internal error: ndarray_fromatom: argument not of atomic type");
  exit(-1);
}

$ndarray $ndarray_linspace($float a, $float b, $int n) {
  $list shape = $NEW($list,NULL,NULL);
  $list_append(shape,n);
  $list strides = $NEW($list,NULL,NULL);
  $list_append(strides,to$int(1));
  $ndarray res = $nullarray(1,(to$obj_converter)to$float,(from$obj_converter)from$float,shape,strides);
  res->data = malloc(n->val*sizeof(union $Bytes8));
  double step = (b->val - a->val)/(n->val-1);
  for (long i = 0; i<n->val; i++) {
    res->data[i].d = a->val + i * step;
  }
  return res;
}

$ndarray $ndarray_arange($int n) {
  $list shape = $NEW($list,NULL,NULL);
  $list_append(shape,n);
  $list strides = $NEW($list,NULL,NULL);
  $list_append(strides,to$int(1));
  $ndarray res = $nullarray(1,(to$obj_converter)to$int,(from$obj_converter)from$int,shape,strides);
  res->data = malloc(n->val*sizeof(union $Bytes8));
  for (long i = 0; i<n->val; i++) {
    res->data[i].l = i;
  }
  return res;
}

$float $ndarray_sumf($ndarray a) {
  double res = 0;
  long aincr0;
  $list ix;
  switch (a->ndim) {
  case 0:
    return to$float(a->data[a->offset].d);
    break;
  case 1:
    aincr0 = $LONGELEM(a->strides,0);
    long ixa = a->offset;
    for (long i = 0; i< $LONGELEM(a->shape,0); i++) {
      res += a->data[ixa].d;
      ixa += aincr0;
    }
    return to$float(res);
    break;
  default:
    ix = $NEW($list,NULL,NULL);
    $list_append(ix,to$int(0));
    for (int i=0; i < $LONGELEM(a->shape,0); i++) {
      $list_setitem(ix,0,to$int(i));
      res += $ndarray_sumf($nd_getslice(a,ix))->val;
    }
    return to$float(res);
  }
}

// extension ndarray[int] Plus /////////////////////////////////////////////////////////////////////////////////

void $Plus$ndarray$int$__init__ ($Plus$ndarray$int self) {
  return;
}

static union $Bytes8 plusi(union $Bytes8 a, union $Bytes8 b) {
  union $Bytes8 res;
  res.l = a.l + b.l;
  return res;
}

$ndarray $Plus$ndarray$int$__add__ ($Plus$ndarray$int wit, $ndarray a, $ndarray b) {
  return $ndarray_oper1(plusi,a,b);
}

struct $Plus$ndarray$int$class $Plus$ndarray$int$methods = {"",UNASSIGNED,NULL,$Plus$ndarray$int$__init__,$Plus$ndarray$int$__add__};
struct $Plus$ndarray$int  $Plus$ndarray$int_instance = {&$Plus$ndarray$int$methods};
$Plus$ndarray$int $Plus$ndarray$int$witness = &$Plus$ndarray$int_instance;


// extension ndarray[float] Plus /////////////////////////////////////////////////////////////////////////////////

void $Plus$ndarray$float$__init__ ($Plus$ndarray$float self) {
  return;
}

static union $Bytes8 plusd(union $Bytes8 a, union $Bytes8 b) {
  union $Bytes8 res;
  res.d = a.d + b.d;
  return res;
}

$ndarray $Plus$ndarray$float$__add__ ($Plus$ndarray$float wit, $ndarray a, $ndarray b) {
  return $ndarray_oper1(plusd,a,b);
}

struct $Plus$ndarray$float$class $Plus$ndarray$float$methods = {"",UNASSIGNED,NULL,$Plus$ndarray$float$__init__,$Plus$ndarray$float$__add__};
struct $Plus$ndarray$float  $Plus$ndarray$float_instance = {&$Plus$ndarray$float$methods};
$Plus$ndarray$float $Plus$ndarray$float$witness = &$Plus$ndarray$float_instance;


/*

Replacing the above def of __add__ with the one below reduces execution time of test1.c by almost 20%. 
The code below is just the code of $ndarray_oper1 with plusd inlined.

static void $fill_add_array(long startdim,
                      union $Bytes8 **resdata, union $Bytes8 **adata, union $Bytes8 **bdata,
                      $list shape, $list astrides, $list bstrides) {
  long shape1, aincr0, bincr0;
  switch (shape->length - startdim) {
    case 0:
      (**resdata).d = (**adata).d + (**bdata).d;
      break;
    case 1:
      aincr0 = $LONGELEM(astrides,startdim);
      bincr0 = $LONGELEM(bstrides,startdim);
      for (long i = 0; i < $LONGELEM(shape,startdim); i++) {
        (**resdata).d = (**adata).d + (**bdata).d;
        *resdata += 1;
        *adata += aincr0;
        *bdata += bincr0;
      }
      break;
  //case 2:
  //case 3: we tried a looping base case up to 3 dim's before turning to recursion, but found no evidence that it's worth the extra code.
    default:
      shape1 = $LONGELEM(shape,startdim + 1);
      aincr0 = $LONGELEM(astrides,startdim) - shape1 * $LONGELEM(astrides,startdim + 1);
      bincr0 = $LONGELEM(bstrides,startdim) - shape1 * $LONGELEM(bstrides,startdim + 1);
      for (int i=0; i < $LONGELEM(shape,startdim); i++) {
        $fill_add_array(startdim+1,resdata,adata,bdata,shape,astrides,bstrides);
        *adata += aincr0;
        *bdata += bincr0;
      }
    }
}

// for operators where args and result are each eight bytes
$ndarray $Plus$ndarray$float$__add__ ($Plus$ndarray$float wit, $ndarray a, $ndarray b) {
  $list tmpshapea = $list_copy(a->shape);
  $list tmpstridesa = $list_copy(a->strides);
  $list tmpshapeb = $list_copy(b->shape);
  $list tmpstridesb = $list_copy(b->strides);
  $list resshape = $nd_broadcast(tmpshapea, tmpshapeb, tmpstridesa, tmpstridesb);
  long size = 1;
  $list resstrides = $list_new(resshape->length);
  resstrides->length = resshape->length;
  for (int i = resshape->length-1; i>=0; i--) {
    long s = from$int($list_getitem(resshape, i));
    $list_setitem(resstrides,i,to$int(s > 1 ? size : 0));
    size *= s;
  }
  $ndarray res = $nullarray(resshape->length,a->to$obj,a->from$obj,resshape,resstrides);
  res->data = malloc(size * sizeof(union $Bytes8));
  union $Bytes8 *resdata = &res->data[0];
  union $Bytes8 *adata = &a->data[a->offset];
  union $Bytes8 *bdata = &b->data[b->offset];
  $fill_add_array(0,&resdata,&adata,&bdata,resshape,tmpstridesa,tmpstridesb);
  return res;
}
*/
