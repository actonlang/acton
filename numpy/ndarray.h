#include "../builtin/builtin.h"

struct $ndarray;
typedef struct $ndarray *$ndarray;

struct $ndarray$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($ndarray,$WORD);
  void (*__serialize__)($ndarray,$Serial$state); 
  $ndarray (*__deserialize__)($Serial$state);
  $bool (*__bool__)($ndarray);
  $str (*__str__)($ndarray);
};

// Generic type for elements in the C array that holds ndarray data).
 
union $Bytes8 {
  long l;
  double d;
  $WORD w;
};

struct $ndarray {
  struct $ndarray$class *$class;
  long ndim;
  long elem_size;
  long offset;
  $WORD (*elem_obj)(union $Bytes8);
  $list shape;
  $list strides;
  union $Bytes8 *data;
};

$WORD $float_obj(union $Bytes8);
$WORD $int_obj(union $Bytes8);

extern struct $ndarray$class $ndarray$methods;

/*
extern struct $Real$ndarray$class $Real$ndarray$methods;
extern struct $Number$ndarray$class $Number$ndarray$methods;
extern struct $Plus$ndarray$class $Plus$ndarray$methods;
extern struct $Minus$ndarray$class $Minus$ndarray$methods;
extern struct $Hashable$ndarray$class $Hashable$ndarray$methods;

extern struct $Real$ndarray *$Real$ndarray$witness;
extern struct $Number$ndarray *$Number$ndarray$witness;
extern struct $Plus$ndarray *$Plus$ndarray$witness;
extern struct $Minus$ndarray *$Minus$ndarray$witness;

$ndarray $ndarray_fromatom($Super a);
*/

$ndarray $array($Sequence wit, $WORD w);
$ndarray $nd_getslice($ndarray a, $list ix);
$ndarray $ndarray_fromatom($Super a);

$ndarray $ndarray_func1(union $Bytes8(*f)(union $Bytes8),$ndarray a);
$ndarray $ndarray_oper1(union $Bytes8(*f)(union $Bytes8,union $Bytes8),$ndarray a, $ndarray b);

$ndarray $ndarray_linspace($float a, $float b, $int n);
$ndarray $ndarray_range($int n);

$float $ndarray_sumf($ndarray a);

union $Bytes8 mul2(union $Bytes8 x);
union $Bytes8 plusi(union $Bytes8 a, union $Bytes8 b);
union $Bytes8 plusf(union $Bytes8 a, union $Bytes8 b);
