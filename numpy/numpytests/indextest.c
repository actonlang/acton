#include "../../builtin/builtin.h"
#include "../ndarray.h"

int main() {
  $ndarray a = malloc(sizeof(struct $ndarray));
  a->$class = &$ndarray$methods;
  a->ndim = 3;
  a->elem_size = 1;
  a->offset = 0;
  a->elem_obj = $int_obj;
  $list shape = $NEW($list,NULL,NULL);
  $list strides = $NEW($list,NULL,NULL);
  $list_append(shape,to$int(3));
  $list_append(shape,to$int(4));
  $list_append(shape,to$int(5));
  $list_append(strides,to$int(20));
  $list_append(strides,to$int(5));
  $list_append(strides,to$int(1));
  a->shape = shape;
  a->strides = strides;
  a->data = malloc(60 * sizeof(union $Bytes8));
  for (long i= 0; i<60; i++)
    a->data[i].l = i+3;
  printf("a=%s\n",from$str(a->$class->__str__(a)));
  $list ix = $NEW($list,NULL,NULL);
  $list_append(ix,NULL);
  $Slice s1 = $NEW($Slice,NULL,NULL,NULL);
  $list_append(ix,s1);
  $list_append(ix,to$int(2));
  $ndarray b = $nd_getslice(a,ix);
  printf("b=%s\n",from$str(b->$class->__str__(b)));
  $list ix1 = $NEW($list,NULL,NULL);
  $list_append(ix1,to$int(1));
  $Slice s2 = $NEW($Slice,to$int(1),NULL,NULL);
  $list_append(ix1,s2);
  $Slice s3 = $NEW($Slice,to$int(-1),to$int(0),to$int(-1));
  $list_append(ix1,s3);
  $ndarray c = $nd_getslice(a,ix1);
  printf("c=%s\n",from$str(c->$class->__str__(c)));
  $list test = $list_copy(c->shape);
  $ndarray d = $ndarray_func1(mul2,c);
  printf("d=%s\n",from$str(d->$class->__str__(d)));
  $ndarray e = $ndarray_fromatom(($Super)to$int(3));
  $ndarray f = $ndarray_oper1(plusi,d,e);
  printf("f=%s\n",from$str(f->$class->__str__(f)));  
}
