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
 
typedef $WORD (*to$obj_converter) (union $Bytes8);
typedef union $Bytes8 (*from$obj_converter) ($WORD);

struct $ndarray {
  struct $ndarray$class *$class;
  long ndim;
  long elem_size;
  long offset;
  to$obj_converter to$obj;
  from$obj_converter from$obj;
  $list shape;
  $list strides;
  union $Bytes8 *data;
};

extern struct $ndarray$class $ndarray$methods;

$ndarray $ndarray_fromatom($Super a);

$ndarray $nd_getslice($ndarray a, $list ix);
$ndarray $ndarray_fromatom($Super a);

$ndarray $ndarray_reshape($ndarray a, $list newshape);

$ndarray $ndarray_func1(union $Bytes8(*f)(union $Bytes8),$ndarray a);
$ndarray $ndarray_oper1(union $Bytes8(*f)(union $Bytes8,union $Bytes8),$ndarray a, $ndarray b);

$ndarray $ndarray_linspace($float a, $float b, $int n);
$ndarray $ndarray_arange($int n);

$float $ndarray_sumf($ndarray a);

union $Bytes8 mul2(union $Bytes8 x);

// $Plus$ndarray$int  ////////////////////////////////////////////////////////////

struct $Plus$ndarray$int;
typedef struct $Plus$ndarray$int *$Plus$ndarray$int;

struct $Plus$ndarray$int$class;
typedef struct $Plus$ndarray$int$class *$Plus$ndarray$int$class;

struct $Plus$ndarray$int {
  $Plus$ndarray$int$class $class;
};

struct $Plus$ndarray$int$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($Plus$ndarray$int);
  $ndarray (*__add__)($Plus$ndarray$int, $ndarray, $ndarray);
};

void $Plus$ndarray$int$__init__ ($Plus$ndarray$int);
$ndarray $Plus$ndarray$int$__add__ ($Plus$ndarray$int, $ndarray, $ndarray);

extern struct $Plus$ndarray$int *$Plus$ndarray$int$witness;

// $Plus$ndarray$float  ////////////////////////////////////////////////////////////

struct $Plus$ndarray$float;
typedef struct $Plus$ndarray$float *$Plus$ndarray$float;

struct $Plus$ndarray$float$class;
typedef struct $Plus$ndarray$float$class *$Plus$ndarray$float$class;

struct $Plus$ndarray$float {
  $Plus$ndarray$float$class $class;
};

struct $Plus$ndarray$float$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($Plus$ndarray$float);
  $ndarray (*__add__)($Plus$ndarray$float, $ndarray, $ndarray);
};

void $Plus$ndarray$float$__init__ ($Plus$ndarray$float);
$ndarray $Plus$ndarray$float$__add__ ($Plus$ndarray$float, $ndarray, $ndarray);

extern struct $Plus$ndarray$float *$Plus$ndarray$float$witness;




/*
  extern struct $Real$ndarray$class $Real$ndarray$methods;
  extern struct $Number$ndarray$class $Number$ndarray$methods;
  extern struct $Plus$ndarray$class $Plus$ndarray$methods;
  extern struct $Minus$ndarray$class $Minus$ndarray$methods;
  extern struct $Hashable$ndarray$class $Hashable$ndarray$methods;

  extern struct $Real$ndarray *$Real$ndarray$witness;
  extern struct $Number$ndarray *$Number$ndarray$witness;
  extern struct $Minus$ndarray *$Minus$ndarray$witness;

  $ndarray $ndarray_fromatom($Super a);
*/
