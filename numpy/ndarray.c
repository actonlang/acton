#include "ndarray.h"

// Some arithmetic operators. To be replaced by number protocol implementations. ////////////////////////

union $Bytes8 mul2(union $Bytes8 x) {
  union $Bytes8 res;
  res.l = 2 * x.l;
  return res;
}
        
union $Bytes8 plusi(union $Bytes8 a, union $Bytes8 b) {
  union $Bytes8 res;
  res.l = a.l + b.l;
  return res;
}

union $Bytes8 plusf(union $Bytes8 a, union $Bytes8 b) {
  union $Bytes8 res;
  res.d = a.d + b.d;
  return res;
}


// Building Acton objects from array elements

$WORD $float_obj(union $Bytes8 b) {
  return to$float(b.d);
}

$WORD $int_obj(union $Bytes8 b) {
  return to$float(b.l);
}

// ndarray methods /////////////////////////////////////////////////////////////////////////////////////////

void $ndarray__init__($ndarray a, $WORD w) {
  $ndarray r = $ndarray_fromatom(w);
  memcpy(a,r,sizeof(struct $ndarray));
}


$str $ndarray__str__($ndarray a) {
  if (a->ndim==0) {
    union $Bytes8 content = ((union $Bytes8*)a->data)[a->offset];
    $struct obj = a->elem_obj(content);
    return obj->$class->__str__(obj);
  } else {
    $list strs = $NEW($list,NULL,NULL);
    $list ix = $NEW($list,NULL,NULL);
    $list_append(ix,to$int(0));
    for (long i = 0; i< (($int)a->shape->data[0])->val; i++) {
        $list_setitem(ix,0,to$int(i));
        $ndarray b = $nd_getslice(a,ix);
        $list_append(strs,$ndarray__str__(b));
      }
    return  $str_join_par('[',strs,']');
  }
}

struct $ndarray$class $ndarray$methods = {"",UNASSIGNED,($Super$class)&$struct$methods,$ndarray__init__,NULL,NULL,NULL,$ndarray__str__};


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
  $list strides = $list_new(ndim);
  $list shape   = $list_new(ndim);
  shape->length = ndim;   // hmm; shape is empty now, but we know that we will add ndim elements below...
  strides->length = ndim; //    strides    -"-
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
      $list_setitem(strides,respos,to$int(0));
      $list_setitem(shape,respos--,to$int(1));
     } else {
      if ($ISINSTANCE(currindex,$int)) {
        currstride = (($int)a->strides->data[apos--])->val;
        offset += currstride * ((($int)currindex)->val);
      } else if ($ISINSTANCE(currindex,$Slice)) {
        int slen,start,stop,step;
        normalize_slice( ($Slice)currindex,(($int)a->shape->data[apos])->val,&slen,&start,&stop,&step);
        currstride = (($int)a->strides->data[apos])->val;
        offset += start * currstride;
        $list_setitem(strides,respos,to$int((($int)a->strides->data[apos--])->val * step));
        $list_setitem(shape,respos--,to$int(slen));
      } else {
        fprintf(stderr,"internal error: unexpected type of ndarray index element\n");
        exit(-1);
      }
    }
  }
  $ndarray res = malloc(sizeof(struct $ndarray));
  res->$class = &$ndarray$methods;
  res->ndim = ndim;
  res->elem_size = a->elem_size;
  res->offset = offset;
  res->elem_obj = a->elem_obj;
  res->shape = shape;
  res->strides = strides;
  res->data = a->data;
  return res;
}

$ndarray $ndarray_fromatom($Super a) {
  if ($ISINSTANCE(a,$int)) {
    $ndarray res = malloc(sizeof(struct $ndarray));
    res->$class = &$ndarray$methods;
    res->ndim = 0;
    res->elem_size = 1;
    res->offset = 0;
    res->elem_obj = $int_obj;
    res->shape = $NEW($list,NULL,NULL);
    res->strides = $NEW($list,NULL,NULL);
    res->data = malloc(sizeof(union $Bytes8));
    res->data->l = (($int)a)->val;
    return res;
  }
  if ($ISINSTANCE(a,$float)) {
    $ndarray res = malloc(sizeof(struct $ndarray));
    res->$class = &$ndarray$methods;
    res->ndim = 0;
    res->elem_size = 1;
    res->offset = 0;
    res->elem_obj = $float_obj;
    res->shape = $NEW($list,NULL,NULL);
    res->strides = $NEW($list,NULL,NULL);
    res->data = malloc(sizeof(union $Bytes8));
    res->data->d = (($float)a)->val;
    return res;
  }
  if ($ISINSTANCE(a,$bool)) return NULL;
  if ($ISINSTANCE(a,$str)) return NULL;
  fprintf(stderr,"internal error: ndarray_fromatom: argument not of atomic type");
  exit(-1);
}

$ndarray $ndarray_func1(union $Bytes8(*f)(union $Bytes8),$ndarray a) { // for functions where arg and result are one WORD
  long size = 1;
  $list resstrides = $list_new(a->shape->length);
  resstrides->length = a->shape->length;
  for (int i = a->shape->length-1; i>=0; i--) {
    long s = from$int($list_getitem(a->shape, i));
    $list_setitem(resstrides,i,to$int(s > 1 ? size : 0));
    size *= s;
  }
  $ndarray res = malloc(sizeof(struct $ndarray));
  res->$class = &$ndarray$methods;
  res->ndim = a->ndim;
  res->elem_size = 1;
  res->elem_obj = a->elem_obj;
  res->offset = 0;
  res->shape = $list_copy(a->shape); // do we need to copy?
  res->strides = resstrides;
  res->data = malloc(size * sizeof(union $Bytes8));
  long ixa=a->offset;
  long ixres = 0;
  long aincr0, aincr1;
  switch (a->ndim) {
  case 0:
    res->data[ixres] = f(a->data[ixa]);
    break;
  case 1:
    aincr0 = from$int(a->strides->data[0]);
    for (long i = 0; i < from$int(a->shape->data[0]); i++) {
      res->data[ixres] = f(a->data[ixa]);
      ixa += aincr0;
      ixres++;
    }
    break;
  case 2:
    aincr0 = from$int(a->strides->data[0]);
    aincr1 = from$int(a->strides->data[1]);
    for (long i = 0; i < from$int(a->shape->data[0]); i++) {
      ixa = a->offset + i * aincr0;
      for  (long j = 0; j < from$int(a->shape->data[1]); j++) {
        res->data[ixres] = f(a->data[ixa]);
        ixa += aincr1;
        ixres++;
      }
    }
    break;
  default:
    fprintf(stderr,"internal error; ndarray function application only for ndim <=2\n");
    exit(-1);
  }
  return res;
}

// on input, shapei and stridesi must be copies of the shape and strides of the operands.
// on output, the shorter has been extended with leading ones resp zeros.
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
  }
  $list res = $list_new(len);
  res->length = len;
  for (int i=0; i<len; i++) {
    if (from$int(shape1->data[i]) == from$int(shape2->data[i]))
      res->data[i] = shape1->data[i];
    else if (from$int(shape1->data[i]) == 1)
      res->data[i] = shape2->data[i];
    else if (from$int(shape2->data[i]) == 1)
      res->data[i] = shape1->data[i];
    else
      RAISE(($BaseException)$NEW($ValueError,to$str("ndarray broadcasting: shapes do not match")));
  }
  return res;
}

// for operators where args and result are one WORD
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
  $ndarray res = malloc(sizeof(struct $ndarray));
  res->$class = &$ndarray$methods;
  res->ndim = resshape->length;
  res->elem_size = 1;
  res->elem_obj = a->elem_obj;
  res->offset = 0;
  res->shape = resshape;
  res->strides = resstrides;
  res->data = malloc(size * sizeof($WORD));
  long ixa = a->offset;
  long ixb = b->offset;
  long ixres = 0;
  long aincr0, aincr1, bincr0, bincr1;
  switch (res->ndim) {
  case 0:
    res->data[ixres] = f(a->data[ixa],b->data[ixb]);
    break;
  case 1:
    aincr0 = from$int(tmpstridesa->data[0]);
    bincr0 = from$int(tmpstridesb->data[0]);
    for (long i = 0; i < from$int(resshape->data[0]); i++) {
      res->data[ixres] = f(a->data[ixa],b->data[ixb]);
      ixa += aincr0;
      ixb += bincr0;
      ixres++;
    }
    break;
  case 2:
    aincr0 = from$int(tmpstridesa->data[0]);
    aincr1 = from$int(tmpstridesa->data[1]);
    bincr0 = from$int(tmpstridesb->data[0]);
    bincr1 = from$int(tmpstridesb->data[1]);
    for (long i = 0; i < from$int(resshape->data[0]); i++) {
      ixa = a->offset + i * aincr0;
      ixb = b->offset + i * bincr0;
      for  (long j = 0; j < from$int(resshape->data[1]); j++) {
        res->data[ixres] = f(a->data[ixa],b->data[ixb]);
        ixa += aincr1;
        ixb += bincr1;
        ixres++;
      }
    }
    break;
  default:
    fprintf(stderr,"internal error; ndarray function application yet only for ndim <=2\n");
    exit(-1);
  }
  return res;
}

$ndarray $ndarray_linspace($float a, $float b, $int n) {
  $ndarray res = malloc(sizeof(struct $ndarray));
  res->$class = &$ndarray$methods;
  res->ndim = 1;
  res->elem_size = 1;
  res->elem_obj = $float_obj;
  res->offset = 0;
  $list shape = $NEW($list,NULL,NULL);
  $list_append(shape,n);
  res->shape = shape;
  $list strides = $NEW($list,NULL,NULL);
  $list_append(strides,to$int(1));
  res->strides = strides;
  res->data = malloc(n->val*sizeof($WORD));
  double step = (b->val - a->val)/(n->val-1);
  for (long i = 0; i<n->val; i++) {
    ((double*)res->data)[i] = a->val + i * step;
  }
  return res;
}

$ndarray $ndarray_range($int n) {
  $ndarray res = malloc(sizeof(struct $ndarray));
  res->$class = &$ndarray$methods;
  res->ndim = 1;
  res->elem_size = 1;
  res->elem_obj = $int_obj;
  res->offset = 0;
  $list shape = $NEW($list,NULL,NULL);
  $list_append(shape,n);
  res->shape = shape;
  $list strides = $NEW($list,NULL,NULL);
  $list_append(strides,to$int(1));
  res->strides = strides;
  res->data = malloc(n->val*sizeof($WORD));
  for (long i = 0; i<n->val; i++) {
    ((long*)res->data)[i] = i;
  }
  return res;
}

$float $ndarray_sumf($ndarray a) {
  double res = 0;
  long ix = a->offset;
  long aincr0, aincr1;
  switch (a->ndim) {
  case 0:
    memcpy(&res,a->data,sizeof(double));
    return to$float(res);
  case 1:
    aincr0 = from$int(a->strides->data[0]);
    for (int i=0; i< from$int(a->shape->data[0]); i++) {
      double d;
      memcpy(&d,&a->data[ix],sizeof(double));
      res += d;
      ix += aincr0;
    }
    return to$float(res);
  case 2:
    aincr0 = from$int(a->strides->data[0]);
    aincr1 = from$int(a->strides->data[1]);
    for (int i=0; i< from$int(a->shape->data[0]); i++) {
      ix = a->offset + i * aincr0;
      for  (long j = 0; j < from$int(a->shape->data[1]); j++) {
        res += ((double*)a->data)[ix];
        ix += aincr1;
      }
    }
    return to$float(res);
  default:
    fprintf(stderr,"internal error; sumf only for ndim <=2\n");
    exit(-1);    
  }
}


