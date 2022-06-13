#pragma once
#include "builtin/builtin.h"

#include "math.h"

struct numpy$$ndselect;
typedef struct numpy$$ndselect *numpy$$ndselect;
struct numpy$$ndselect$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__) (numpy$$ndselect);
    void (*__serialize__) (numpy$$ndselect, $Serial$state);
    numpy$$ndselect (*__deserialize__) (numpy$$ndselect, $Serial$state);
    $bool (*__bool__) (numpy$$ndselect);
    $str (*__str__) (numpy$$ndselect);
    $str (*__repr__) (numpy$$ndselect);
};
struct numpy$$ndselect {
    struct numpy$$ndselect$class *$class;
};
extern struct numpy$$ndselect$class numpy$$ndselect$methods;
numpy$$ndselect numpy$$ndselect$new();
struct numpy$$ndindex;
typedef struct numpy$$ndindex *numpy$$ndindex;
struct numpy$$ndindex$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__) (numpy$$ndindex, $int);
    void (*__serialize__) (numpy$$ndindex, $Serial$state);
    numpy$$ndindex (*__deserialize__) (numpy$$ndindex, $Serial$state);
    $bool (*__bool__) (numpy$$ndindex);
    $str (*__str__) (numpy$$ndindex);
    $str (*__repr__) (numpy$$ndindex);
};
struct numpy$$ndindex {
    struct numpy$$ndindex$class *$class;
    $int index;
};
extern struct numpy$$ndindex$class numpy$$ndindex$methods;
numpy$$ndindex numpy$$ndindex$new($int);


struct numpy$$ndslice;
typedef struct numpy$$ndslice *numpy$$ndslice;
struct numpy$$ndslice$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__) (numpy$$ndslice, $slice);
    void (*__serialize__) (numpy$$ndslice, $Serial$state);
    numpy$$ndslice (*__deserialize__) (numpy$$ndslice, $Serial$state);
    $bool (*__bool__) (numpy$$ndslice);
    $str (*__str__) (numpy$$ndslice);
    $str (*__repr__) (numpy$$ndslice);
};
struct numpy$$ndslice {
    struct numpy$$ndslice$class *$class;
    $slice slc;
};
extern struct numpy$$ndslice$class numpy$$ndslice$methods;
numpy$$ndslice numpy$$ndslice$new($slice);


// The bulk of data in an ndarray is stored in a C array of union $Bytes8 data.
// Each ndarray also holds the address of an $UnboxedFunctions struct, containing conversion
// functions to and from boxed data, and operators on unboxed data.
// This file provides the necessary type definitions and $UnboxedFunctions structs for the
// two Acton types supported at the moment, int and float (i.e., boxed long and double).



struct numpy$$Primitive;
typedef struct numpy$$Primitive *numpy$$Primitive;

struct numpy$$Primitive$class;
typedef struct numpy$$Primitive$class *numpy$$Primitive$class;

struct numpy$$Primitive$int;
typedef struct numpy$$Primitive$int *numpy$$Primitive$int;

struct numpy$$Primitive$int$class;
typedef struct numpy$$Primitive$int$class *numpy$$Primitive$int$class;

struct numpy$$Primitive$float;
typedef struct numpy$$Primitive$float *numpy$$Primitive$float;

struct numpy$$Primitive$float$class;
typedef struct numpy$$Primitive$float$class *numpy$$Primitive$float$class;

struct numpy$$Primitive {
    numpy$$Primitive$class $class;
};

union $Bytes8 {
  long l;
  double d;
};

enum ElemType {LongType,DblType};

int $elem_size(enum ElemType typ);

struct numpy$$Primitive$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)(numpy$$Primitive);
  void (*__serialize__)(numpy$$Primitive,$Serial$state);
  numpy$$Primitive (*__deserialize__)(numpy$$Primitive,$Serial$state);
  $bool (*__bool__)(numpy$$Primitive);
  $str (*__str__)(numpy$$Primitive);
  $str (*__repr__)(numpy$$Primitive);
  enum ElemType elem_type;
  $WORD (*to$obj)(union $Bytes8);
  union $Bytes8 (*from$obj)($WORD);
  $str (*$prim_str)(union $Bytes8);
  union $Bytes8 (*$add)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$sub)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$mul)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$truediv)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$floordiv)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$mod)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$land)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$lor)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$band)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$bor)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$bxor)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$lsh)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$rsh)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$pow)(union $Bytes8, union $Bytes8);
  void (*$iadd)(union $Bytes8*, union $Bytes8);
  void (*$isub)(union $Bytes8*, union $Bytes8);
  void (*$imul)(union $Bytes8*, union $Bytes8);
  void (*$itruediv)(union $Bytes8*, union $Bytes8);
  void (*$ifloordiv)(union $Bytes8*, union $Bytes8);
  void (*$imod)(union $Bytes8*, union $Bytes8);
  void (*$iband)(union $Bytes8*, union $Bytes8);
  void (*$ibor)(union $Bytes8*, union $Bytes8);
  void (*$ibxor)(union $Bytes8*, union $Bytes8);
  void (*$ilsh)(union $Bytes8*, union $Bytes8);
  void (*$irsh)(union $Bytes8*, union $Bytes8);
  bool (*$eq)(union $Bytes8, union $Bytes8);
  bool (*$neq)(union $Bytes8, union $Bytes8);
  bool (*$lt)(union $Bytes8, union $Bytes8);
  bool (*$le)(union $Bytes8, union $Bytes8);
  bool (*$gt)(union $Bytes8, union $Bytes8);
  bool (*$ge)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$abs)(union $Bytes8);
  union $Bytes8 (*$neg)(union $Bytes8);
  union $Bytes8 (*$lnot)(union $Bytes8);
  union $Bytes8 (*$bnot)(union $Bytes8);
};

$str l$prim_str(union $Bytes8 n);
$str d$prim_str(union $Bytes8 n);

// Primitive instance for int ///////////////////////////////////////////////////////////////

struct numpy$$Primitive$int {
    numpy$$Primitive$int$class $class;
};

struct numpy$$Primitive$int$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)(numpy$$Primitive$int);
  void (*__serialize__)(numpy$$Primitive$int,$Serial$state);
  numpy$$Primitive$int (*__deserialize__)(numpy$$Primitive$int,$Serial$state);
  $bool (*__bool__)(numpy$$Primitive$int);
  $str (*__str__)(numpy$$Primitive$int);
  $str (*__repr__)(numpy$$Primitive$int);
  enum ElemType elem_type;
  $WORD (*to$obj)(union $Bytes8);
  union $Bytes8 (*from$obj)($WORD);
  $str (*$prim_str)(union $Bytes8);
  union $Bytes8 (*$add)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$sub)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$mul)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$truediv)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$floordiv)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$mod)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$land)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$lor)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$band)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$bor)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$bxor)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$lsh)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$rsh)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$pow)(union $Bytes8, union $Bytes8);
  void (*$iadd)(union $Bytes8*, union $Bytes8);
  void (*$isub)(union $Bytes8*, union $Bytes8);
  void (*$imul)(union $Bytes8*, union $Bytes8);
  void (*$itruediv)(union $Bytes8*, union $Bytes8);
  void (*$ifloordiv)(union $Bytes8*, union $Bytes8);
  void (*$imod)(union $Bytes8*, union $Bytes8);
  void (*$iband)(union $Bytes8*, union $Bytes8);
  void (*$ibor)(union $Bytes8*, union $Bytes8);
  void (*$ibxor)(union $Bytes8*, union $Bytes8);
  void (*$ilsh)(union $Bytes8*, union $Bytes8);
  void (*$irsh)(union $Bytes8*, union $Bytes8);
  bool (*$eq)(union $Bytes8, union $Bytes8);
  bool (*$neq)(union $Bytes8, union $Bytes8);
  bool (*$lt)(union $Bytes8, union $Bytes8);
  bool (*$le)(union $Bytes8, union $Bytes8);
  bool (*$gt)(union $Bytes8, union $Bytes8);
  bool (*$ge)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$abs)(union $Bytes8);
  union $Bytes8 (*$neg)(union $Bytes8);
  union $Bytes8 (*$lnot)(union $Bytes8);
  union $Bytes8 (*$bnot)(union $Bytes8);
};

// Primitive instance for float ///////////////////////////////////////////////////////////////

struct numpy$$Primitive$float {
  numpy$$Primitive$float$class $class;
};

struct numpy$$Primitive$float$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)(numpy$$Primitive$float);
  void (*__serialize__)(numpy$$Primitive$float,$Serial$state);
  numpy$$Primitive$float (*__deserialize__)(numpy$$Primitive$float,$Serial$state);
  $bool (*__bool__)(numpy$$Primitive$float);
  $str (*__str__)(numpy$$Primitive$float);
  $str (*__repr__)(numpy$$Primitive$float);
  enum ElemType elem_type;
  $WORD (*to$obj)(union $Bytes8);
  union $Bytes8 (*from$obj)($WORD);
  $str (*$prim_str)(union $Bytes8);
  union $Bytes8 (*$add)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$sub)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$mul)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$truediv)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$floordiv)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$mod)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$land)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$lor)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$band)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$bor)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$bxor)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$lsh)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$rsh)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$pow)(union $Bytes8, union $Bytes8);
  void (*$iadd)(union $Bytes8*, union $Bytes8);
  void (*$isub)(union $Bytes8*, union $Bytes8);
  void (*$imul)(union $Bytes8*, union $Bytes8);
  void (*$itruediv)(union $Bytes8*, union $Bytes8);
  void (*$ifloordiv)(union $Bytes8*, union $Bytes8);
  void (*$imod)(union $Bytes8*, union $Bytes8);
  void (*$iband)(union $Bytes8*, union $Bytes8);
  void (*$ibor)(union $Bytes8*, union $Bytes8);
  void (*$ibxor)(union $Bytes8*, union $Bytes8);
  void (*$ilsh)(union $Bytes8*, union $Bytes8);
  void (*$irsh)(union $Bytes8*, union $Bytes8);
  bool (*$eq)(union $Bytes8, union $Bytes8);
  bool (*$neq)(union $Bytes8, union $Bytes8);
  bool (*$lt)(union $Bytes8, union $Bytes8);
  bool (*$le)(union $Bytes8, union $Bytes8);
  bool (*$gt)(union $Bytes8, union $Bytes8);
  bool (*$ge)(union $Bytes8, union $Bytes8);
  union $Bytes8 (*$abs)(union $Bytes8);
  union $Bytes8 (*$neg)(union $Bytes8);
  union $Bytes8 (*$lnot)(union $Bytes8);
  union $Bytes8 (*$bnot)(union $Bytes8);
};

// Witnesses and creation ////////////////////////////////////////////////////////////////////////////

numpy$$Primitive$int numpy$$Primitive$int$new();
numpy$$Primitive$float numpy$$Primitive$float$new();

extern struct numpy$$Primitive$int$class  numpy$$Primitive$int$methods;
extern struct numpy$$Primitive$float$class  numpy$$Primitive$float$methods;

extern struct numpy$$Primitive$int *numpy$$Primitive$int$witness;
extern struct numpy$$Primitive$float *numpy$$Primitive$float$witness;




struct numpy$$ndarray;
typedef struct numpy$$ndarray *numpy$$ndarray;

struct numpy$$ndarray$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)(numpy$$ndarray,$WORD);
  void (*__serialize__)(numpy$$ndarray,$Serial$state); 
  numpy$$ndarray (*__deserialize__)(numpy$$ndarray,$Serial$state);
  $bool (*__bool__)(numpy$$ndarray);
  $str (*__str__)(numpy$$ndarray);
  numpy$$ndarray (*reshape)(numpy$$ndarray,$list);
  numpy$$ndarray (*transpose)(numpy$$ndarray,$list);
  numpy$$ndarray (*flatten)(numpy$$ndarray);
  numpy$$ndarray (*copy)(numpy$$ndarray);
  numpy$$ndarray (*__ndgetslice__)(numpy$$ndarray,$list);
};

struct numpy$$ndarray {
  struct numpy$$ndarray$class *$class;
  enum ElemType elem_type;
  long ndim;
  $int size;         // # of elements; equal to product of elements in shape.
  long offset;
  long elem_size;
  $list shape;
  $list strides;
  union $Bytes8 *data;
};

extern struct numpy$$ndarray$class numpy$$ndarray$methods;

// iterating over an ndarray //////////////////////////////////////////

#define MAX_NDIM 16

typedef struct numpy$$array_iterator_state {
  union $Bytes8 *current;
  long currentstride;
  long lastshapepos;
  long lastshapelength;
  long ndim1;    // ndim-1
  long shape[MAX_NDIM];
  long strides[MAX_NDIM];
  long jumps[MAX_NDIM];
  long index[MAX_NDIM];
} *numpy$$array_iterator_state;



typedef struct numpy$$Iterator$ndarray *numpy$$Iterator$ndarray; ;

struct numpy$$Iterator$ndarray$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)(numpy$$Iterator$ndarray, numpy$$Primitive, numpy$$ndarray);
  void (*__serialize__)(numpy$$Iterator$ndarray,$Serial$state);
  numpy$$Iterator$ndarray (*__deserialize__)(numpy$$Iterator$ndarray,$Serial$state);
  $bool (*__bool__)(numpy$$Iterator$ndarray);
  $str (*__str__)(numpy$$Iterator$ndarray);
  $WORD (*__next__)(numpy$$Iterator$ndarray);
};

numpy$$ndarray numpy$$ndarray$new($WORD);

struct numpy$$Iterator$ndarray {
  struct numpy$$Iterator$ndarray$class *$class;
  numpy$$Primitive pwit;
  numpy$$array_iterator_state it;
};

extern struct  numpy$$Iterator$ndarray$class  numpy$$Iterator$ndarray$methods;

numpy$$Iterator$ndarray numpy$$Iterator$ndarray$new(numpy$$Primitive,numpy$$ndarray);

// Intended argument to constructor

numpy$$ndarray numpy$$fromatom($atom a);

//numpy$$ndarray numpy$$ndarray_func(union $Bytes8(*f)(union $Bytes8),numpy$$ndarray a);
//numpy$$ndarray numpy$$ndarray_oper(union $Bytes8 (*f)(union $Bytes8, union $Bytes8), numpy$$ndarray a, numpy$$ndarray b);

// Methods in ndarray class //////////////////////////////////////////////

numpy$$ndarray numpy$$ndarray$reshape(numpy$$ndarray,$list);
numpy$$ndarray numpy$$ndarray$transpose(numpy$$ndarray,$list);
numpy$$ndarray numpy$$ndarray$flatten(numpy$$ndarray);
numpy$$ndarray numpy$$ndarray$copy(numpy$$ndarray);
numpy$$ndarray numpy$$ndarray$__ndgetslice__(numpy$$ndarray,$list);

// Functions to create ndarrays /////////////////////////////////////////

numpy$$ndarray numpy$$linspace($float a, $float b, $int n);
numpy$$ndarray numpy$$arange($int start, $int stop, $int step);
numpy$$ndarray numpy$$array(numpy$$Primitive wit, $list elems);
numpy$$ndarray numpy$$full(numpy$$Primitive wit, $list shape, $WORD val);
numpy$$ndarray numpy$$unirandint($int a, $int b, $int n);
numpy$$ndarray numpy$$unirandfloat($float a, $float b, $int n);
numpy$$ndarray numpy$$tile(numpy$$Primitive wit, numpy$$ndarray a, $int n);
numpy$$ndarray numpy$$roll(numpy$$Primitive wit, numpy$$ndarray a, $int n);
numpy$$ndarray numpy$$concatenate(numpy$$Primitive wit, $list as);
numpy$$ndarray numpy$$zeros(numpy$$Primitive wit, $int n);

// Various utilities /////////////////////////////////////////////////////

numpy$$ndarray numpy$$sum(numpy$$Primitive wit, numpy$$ndarray a, $int axis);
numpy$$ndarray numpy$$partition(numpy$$Primitive wit, numpy$$ndarray a, $int k);
numpy$$ndarray numpy$$sort(numpy$$Primitive wit, numpy$$ndarray a, $int axis);
numpy$$ndarray numpy$$clip(numpy$$Primitive wit, numpy$$ndarray a, $WORD low, $WORD high);
numpy$$ndarray numpy$$dot(numpy$$Primitive wit, numpy$$ndarray a, numpy$$ndarray b);
numpy$$ndarray numpy$$abs(numpy$$Primitive wit, numpy$$ndarray a);
numpy$$ndarray numpy$$mean(numpy$$Primitive wit, numpy$$ndarray a, $int axis);
$WORD numpy$$scalar(numpy$$Primitive wit, numpy$$ndarray a);

// newaxis //////////////////////////////////////////////////////////

extern $int numpy$$newaxis;

struct numpy$$Integral$ndarray$int;
typedef struct numpy$$Integral$ndarray$int *numpy$$Integral$ndarray$int;

struct numpy$$Integral$ndarray$int$class;
typedef struct numpy$$Integral$ndarray$int$class *numpy$$Integral$ndarray$int$class;

struct numpy$$Logical$ndarray$int;
typedef struct numpy$$Logical$ndarray$int *numpy$$Logical$ndarray$int;

struct numpy$$Logical$ndarray$int$class;
typedef struct numpy$$Logical$ndarray$int$class *numpy$$Logical$ndarray$int$class;

struct numpy$$Minus$ndarray$int;
typedef struct numpy$$Minus$ndarray$int *numpy$$Minus$ndarray$int;

struct numpy$$Minus$ndarray$int$class;
typedef struct numpy$$Minus$ndarray$int$class *numpy$$Minus$ndarray$int$class;

struct numpy$$Real$ndarray;
typedef struct numpy$$Real$ndarray *numpy$$Real$ndarray;

struct numpy$$Real$ndarray$class;
typedef struct numpy$$Real$ndarray$class *numpy$$Real$ndarray$class;

struct numpy$$Minus$ndarray;
typedef struct numpy$$Minus$ndarray *numpy$$Minus$ndarray;

struct numpy$$Minus$ndarray$class;
typedef struct numpy$$Minus$ndarray$class *numpy$$Minus$ndarray$class;

struct numpy$$Div$ndarray$int;
typedef struct numpy$$Div$ndarray$int *numpy$$Div$ndarray$int;

struct numpy$$Div$ndarray$int$class;
typedef struct numpy$$Div$ndarray$int$class *numpy$$Div$ndarray$int$class;

struct numpy$$Div$ndarray$float;
typedef struct numpy$$Div$ndarray$float *numpy$$Div$ndarray$float;

struct numpy$$Div$ndarray$float$class;
typedef struct numpy$$Div$ndarray$float$class *numpy$$Div$ndarray$float$class;

struct numpy$$Collection$ndarray;
typedef struct numpy$$Collection$ndarray *numpy$$Collection$ndarray;

struct numpy$$Collection$ndarray$class;
typedef struct numpy$$Collection$ndarray$class *numpy$$Collection$ndarray$class;

// numpy$$Integral$ndarray$int ////////////////////////////////////////////////////////////

struct numpy$$Integral$ndarray$int {
    numpy$$Integral$ndarray$int$class $class;
    $Logical w$Logical;
    $Minus w$Minus;
};

struct numpy$$Integral$ndarray$int$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)(numpy$$Integral$ndarray$int);
    void (*__serialize__)(numpy$$Integral$ndarray$int,$Serial$state);
    numpy$$Integral$ndarray$int (*__deserialize__)(numpy$$Integral$ndarray$int,$Serial$state);
    $bool (*__bool__)(numpy$$Integral$ndarray$int);
    $str (*__str__)(numpy$$Integral$ndarray$int);
    numpy$$ndarray (*__add__)(numpy$$Integral$ndarray$int, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__iadd__)(numpy$$Integral$ndarray$int, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__mul__)(numpy$$Integral$ndarray$int, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__imul__)(numpy$$Integral$ndarray$int, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__fromatom__)(numpy$$Integral$ndarray$int,$atom);
    $complex (*__complx__)(numpy$$Integral$ndarray$int, numpy$$ndarray);
    numpy$$ndarray (*__pow__)(numpy$$Integral$ndarray$int, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__ipow__)(numpy$$Integral$ndarray$int, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__neg__)(numpy$$Integral$ndarray$int, numpy$$ndarray);
    numpy$$ndarray (*__pos__)(numpy$$Integral$ndarray$int, numpy$$ndarray);
    $WORD (*real)(numpy$$Integral$ndarray$int, numpy$$ndarray, $Real);
    $WORD (*imag)(numpy$$Integral$ndarray$int, numpy$$ndarray, $Real);
    $WORD (*__abs__)(numpy$$Integral$ndarray$int, numpy$$ndarray, $Real);
    numpy$$ndarray (*conjugate)(numpy$$Integral$ndarray$int, numpy$$ndarray);
    $float (*__float__)(numpy$$Integral$ndarray$int, numpy$$ndarray);
    $WORD (*__trunc__)(numpy$$Integral$ndarray$int, numpy$$ndarray, $Integral);
    $WORD (*__floor__)(numpy$$Integral$ndarray$int, numpy$$ndarray, $Integral);
    $WORD (*__ceil__)(numpy$$Integral$ndarray$int, numpy$$ndarray, $Integral);
    numpy$$ndarray (*__round__)(numpy$$Integral$ndarray$int, numpy$$ndarray, numpy$$ndarray);
    $WORD (*numerator)(numpy$$Integral$ndarray$int, numpy$$ndarray, $Integral);
    $WORD (*denominator)(numpy$$Integral$ndarray$int, numpy$$ndarray, $Integral);
    numpy$$ndarray (*__int__)(numpy$$Integral$ndarray$int, numpy$$ndarray);
    numpy$$ndarray (*__index__)(numpy$$Integral$ndarray$int, numpy$$ndarray);
    $tuple (*__divmod__)(numpy$$Integral$ndarray$int, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__floordiv__)(numpy$$Integral$ndarray$int, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__mod__)(numpy$$Integral$ndarray$int, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__lshift__)(numpy$$Integral$ndarray$int, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__rshift__)(numpy$$Integral$ndarray$int, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__ifloordiv__)(numpy$$Integral$ndarray$int, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__imod__)(numpy$$Integral$ndarray$int, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__ilshift__)(numpy$$Integral$ndarray$int, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__irshift__)(numpy$$Integral$ndarray$int, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__invert__)(numpy$$Integral$ndarray$int, numpy$$ndarray);
};

void numpy$$Integral$ndarray$int$__init__ (numpy$$Integral$ndarray$int);
void numpy$$Integral$ndarray$int$__serialize__(numpy$$Integral$ndarray$int,$Serial$state);
numpy$$Integral$ndarray$int numpy$$Integral$ndarray$int$__deserialize__(numpy$$Integral$ndarray$int,$Serial$state);
numpy$$ndarray numpy$$Integral$ndarray$int$__add__(numpy$$Integral$ndarray$int, numpy$$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$$Integral$ndarray$int$__iadd__(numpy$$Integral$ndarray$int, numpy$$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$$Integral$ndarray$int$__fromatom__(numpy$$Integral$ndarray$int,$atom);
$complex numpy$$Integral$ndarray$int$__complx__(numpy$$Integral$ndarray$int, numpy$$ndarray);
numpy$$ndarray numpy$$Integral$ndarray$int$__mul__(numpy$$Integral$ndarray$int, numpy$$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$$Integral$ndarray$int$__pow__(numpy$$Integral$ndarray$int, numpy$$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$$Integral$ndarray$int$__neg__(numpy$$Integral$ndarray$int, numpy$$ndarray);
numpy$$ndarray numpy$$Integral$ndarray$int$__pos__(numpy$$Integral$ndarray$int, numpy$$ndarray);
$WORD numpy$$Integral$ndarray$int$real(numpy$$Integral$ndarray$int, numpy$$ndarray, $Real);
$WORD numpy$$Integral$ndarray$int$imag(numpy$$Integral$ndarray$int, numpy$$ndarray, $Real);
$WORD numpy$$Integral$ndarray$int$__abs__(numpy$$Integral$ndarray$int, numpy$$ndarray, $Real);
numpy$$ndarray numpy$$Integral$ndarray$int$conjugate(numpy$$Integral$ndarray$int, numpy$$ndarray);
$float numpy$$Integral$ndarray$int$__float__ (numpy$$Integral$ndarray$int, numpy$$ndarray);
$WORD numpy$$Integral$ndarray$int$__trunc__ (numpy$$Integral$ndarray$int, numpy$$ndarray, $Integral);
$WORD numpy$$Integral$ndarray$int$__floor__ (numpy$$Integral$ndarray$int, numpy$$ndarray, $Integral);
$WORD numpy$$Integral$ndarray$int$__ceil__ (numpy$$Integral$ndarray$int, numpy$$ndarray, $Integral);
numpy$$ndarray numpy$$Integral$ndarray$int$__round__ (numpy$$Integral$ndarray$int, numpy$$ndarray, numpy$$ndarray);
$WORD numpy$$Integral$ndarray$int$numerator (numpy$$Integral$ndarray$int, numpy$$ndarray, $Integral);
$WORD numpy$$Integral$ndarray$int$denominator (numpy$$Integral$ndarray$int, numpy$$ndarray, $Integral);
numpy$$ndarray numpy$$Integral$ndarray$int$__int__ (numpy$$Integral$ndarray$int, numpy$$ndarray);
numpy$$ndarray numpy$$Integral$ndarray$int$__index__ (numpy$$Integral$ndarray$int, numpy$$ndarray);
$tuple numpy$$Integral$ndarray$int$__divmod__ (numpy$$Integral$ndarray$int, numpy$$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$$Integral$ndarray$int$__floordiv__ (numpy$$Integral$ndarray$int, numpy$$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$$Integral$ndarray$int$__mod__ (numpy$$Integral$ndarray$int, numpy$$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$$Integral$ndarray$int$__lshift__ (numpy$$Integral$ndarray$int, numpy$$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$$Integral$ndarray$int$__rshift__ (numpy$$Integral$ndarray$int, numpy$$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$$Integral$ndarray$int$__invert__ (numpy$$Integral$ndarray$int, numpy$$ndarray);

// numpy$$Logical$ndarray$int ////////////////////////////////////////////////////////////

struct numpy$$Logical$ndarray$int {
    numpy$$Logical$ndarray$int$class $class;
    $Integral w$Integral;
};

struct numpy$$Logical$ndarray$int$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)(numpy$$Logical$ndarray$int, $Integral);
    void (*__serialize__)(numpy$$Logical$ndarray$int,$Serial$state);
    numpy$$Logical$ndarray$int (*__deserialize__)(numpy$$Logical$ndarray$int,$Serial$state);
    $bool (*__bool__)(numpy$$Logical$ndarray$int);
    $str (*__str__)(numpy$$Logical$ndarray$int);
    numpy$$ndarray (*__and__)(numpy$$Logical$ndarray$int, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__or__)(numpy$$Logical$ndarray$int, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__xor__)(numpy$$Logical$ndarray$int, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__iand__)(numpy$$Logical$ndarray$int, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__ior__)(numpy$$Logical$ndarray$int, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__ixor__)(numpy$$Logical$ndarray$int, numpy$$ndarray, numpy$$ndarray);
};

void numpy$$Logical$ndarray$int$__init__ (numpy$$Logical$ndarray$int, $Integral);
void numpy$$Logical$ndarray$int$__serialize__(numpy$$Logical$ndarray$int,$Serial$state);
numpy$$Logical$ndarray$int numpy$$Logical$ndarray$int$__deserialize__(numpy$$Logical$ndarray$int,$Serial$state);
numpy$$ndarray numpy$$Logical$ndarray$int$__and__ (numpy$$Logical$ndarray$int, numpy$$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$$Logical$ndarray$int$__or__ (numpy$$Logical$ndarray$int, numpy$$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$$Logical$ndarray$int$__xor__ (numpy$$Logical$ndarray$int, numpy$$ndarray, numpy$$ndarray);

// numpy$$Minus$ndarray$int ////////////////////////////////////////////////////////////

struct numpy$$Minus$ndarray$int {
    numpy$$Minus$ndarray$int$class $class;
    $Integral w$Integral;
};

struct numpy$$Minus$ndarray$int$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)(numpy$$Minus$ndarray$int, $Integral);
    void (*__serialize__)(numpy$$Minus$ndarray$int,$Serial$state);
    numpy$$Minus$ndarray$int (*__deserialize__)(numpy$$Minus$ndarray$int,$Serial$state);
    $bool (*__bool__)(numpy$$Minus$ndarray$int);
    $str (*__str__)(numpy$$Minus$ndarray$int);
    numpy$$ndarray (*__sub__)(numpy$$Minus$ndarray$int, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__isub__)(numpy$$Minus$ndarray$int, numpy$$ndarray, numpy$$ndarray);
};

void numpy$$Minus$ndarray$int$__init__ (numpy$$Minus$ndarray$int, $Integral);
void numpy$$Minus$ndarray$int$__serialize__(numpy$$Minus$ndarray$int,$Serial$state);
numpy$$Minus$ndarray$int numpy$$Minus$ndarray$int$__deserialize__(numpy$$Minus$ndarray$int,$Serial$state);
numpy$$ndarray numpy$$Minus$ndarray$int$__sub__ (numpy$$Minus$ndarray$int, numpy$$ndarray, numpy$$ndarray);

// numpy$$Real$ndarray ////////////////////////////////////////////////////////////

struct numpy$$Real$ndarray {
    numpy$$Real$ndarray$class $class;
    $Minus w$Minus;
    numpy$$Primitive w$Primitive$A$Real$ndarray;
};

struct numpy$$Real$ndarray$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
  void (*__init__)(numpy$$Real$ndarray, numpy$$Primitive);
    void (*__serialize__)(numpy$$Real$ndarray,$Serial$state);
    numpy$$Real$ndarray (*__deserialize__)(numpy$$Real$ndarray,$Serial$state);
    $bool (*__bool__)(numpy$$Real$ndarray);
    $str (*__str__)(numpy$$Real$ndarray);
    numpy$$ndarray (*__add__)(numpy$$Real$ndarray, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__iadd__)(numpy$$Real$ndarray, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__mul__)(numpy$$Real$ndarray, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__imul__)(numpy$$Real$ndarray, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__fromatom__)(numpy$$Real$ndarray,$atom);
    $complex (*__complx__)(numpy$$Real$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__pow__)(numpy$$Real$ndarray, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__ipow__)(numpy$$Real$ndarray, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__neg__)(numpy$$Real$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__pos__)(numpy$$Real$ndarray, numpy$$ndarray);
    $WORD (*real)(numpy$$Real$ndarray, numpy$$ndarray, $Real);
    $WORD (*imag)(numpy$$Real$ndarray, numpy$$ndarray, $Real);
    $WORD (*__abs__)(numpy$$Real$ndarray, numpy$$ndarray, $Real);
    numpy$$ndarray (*conjugate)(numpy$$Real$ndarray, numpy$$ndarray);
    $float (*__float__)(numpy$$Real$ndarray, numpy$$ndarray);
    $WORD (*__trunc__)(numpy$$Real$ndarray, numpy$$ndarray, $Integral);
    $WORD (*__floor__)(numpy$$Real$ndarray, numpy$$ndarray, $Integral);
    $WORD (*__ceil__)(numpy$$Real$ndarray, numpy$$ndarray, $Integral);
    numpy$$ndarray (*__round__)(numpy$$Real$ndarray, numpy$$ndarray, numpy$$ndarray);
};

void numpy$$Real$ndarray$__init__ (numpy$$Real$ndarray,numpy$$Primitive);
void numpy$$Real$ndarray$__serialize__(numpy$$Real$ndarray,$Serial$state);
numpy$$Real$ndarray numpy$$Real$ndarray$__deserialize__(numpy$$Real$ndarray,$Serial$state);
numpy$$ndarray numpy$$Real$ndarray$__add__(numpy$$Real$ndarray, numpy$$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$$Real$ndarray$__iadd__(numpy$$Real$ndarray, numpy$$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$$Real$ndarray$__fromatom__(numpy$$Real$ndarray,$atom);
$complex numpy$$Real$ndarray$__complx__(numpy$$Real$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$$Real$ndarray$__mul__(numpy$$Real$ndarray, numpy$$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$$Real$ndarray$__pow__(numpy$$Real$ndarray, numpy$$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$$Real$ndarray$__neg__(numpy$$Real$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$$Real$ndarray$__pos__(numpy$$Real$ndarray, numpy$$ndarray);
$WORD numpy$$Real$ndarray$real(numpy$$Real$ndarray, numpy$$ndarray, $Real);
$WORD numpy$$Real$ndarray$imag(numpy$$Real$ndarray, numpy$$ndarray, $Real);
$WORD numpy$$Real$ndarray$__abs__(numpy$$Real$ndarray, numpy$$ndarray, $Real);
numpy$$ndarray numpy$$Real$ndarray$conjugate(numpy$$Real$ndarray, numpy$$ndarray);
$float numpy$$Real$ndarray$__float__ (numpy$$Real$ndarray, numpy$$ndarray);
$WORD numpy$$Real$ndarray$__trunc__ (numpy$$Real$ndarray, numpy$$ndarray, $Integral);
$WORD numpy$$Real$ndarray$__floor__ (numpy$$Real$ndarray, numpy$$ndarray, $Integral);
$WORD numpy$$Real$ndarray$__ceil__ (numpy$$Real$ndarray, numpy$$ndarray, $Integral);
numpy$$ndarray numpy$$Real$ndarray$__round__ (numpy$$Real$ndarray, numpy$$ndarray, numpy$$ndarray);

// numpy$$Minus$ndarray ////////////////////////////////////////////////////////////

struct numpy$$Minus$ndarray {
    numpy$$Minus$ndarray$class $class;
    $Real w$Real;
};

struct numpy$$Minus$ndarray$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)(numpy$$Minus$ndarray, $Real);
    void (*__serialize__)(numpy$$Minus$ndarray,$Serial$state);
    numpy$$Minus$ndarray (*__deserialize__)(numpy$$Minus$ndarray,$Serial$state);
    $bool (*__bool__)(numpy$$Minus$ndarray);
    $str (*__str__)(numpy$$Minus$ndarray);
    numpy$$ndarray (*__sub__)(numpy$$Minus$ndarray, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__isub__)(numpy$$Minus$ndarray, numpy$$ndarray, numpy$$ndarray);
};

void numpy$$Minus$ndarray$__init__ (numpy$$Minus$ndarray, $Real);
void numpy$$Minus$ndarray$__serialize__(numpy$$Minus$ndarray,$Serial$state);
numpy$$Minus$ndarray numpy$$Minus$ndarray$__deserialize__(numpy$$Minus$ndarray,$Serial$state);
numpy$$ndarray numpy$$Minus$ndarray$__sub__ (numpy$$Minus$ndarray, numpy$$ndarray, numpy$$ndarray);

// numpy$$Div$ndarray$int ////////////////////////////////////////////////////////////

struct numpy$$Div$ndarray$int {
    numpy$$Div$ndarray$int$class $class;
    $Real w$Real;
};

struct numpy$$Div$ndarray$int$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)(numpy$$Div$ndarray$int);
    void (*__serialize__)(numpy$$Div$ndarray$int,$Serial$state);
    numpy$$Div$ndarray$int (*__deserialize__)(numpy$$Div$ndarray$int,$Serial$state);
    $bool (*__bool__)(numpy$$Div$ndarray$int);
    $str (*__str__)(numpy$$Div$ndarray$int);
    numpy$$ndarray (*__truediv__)(numpy$$Div$ndarray$int, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__itruediv__)(numpy$$Div$ndarray$int, numpy$$ndarray, numpy$$ndarray);
};

void numpy$$Div$ndarray$int$__init__ (numpy$$Div$ndarray$int);
void numpy$$Div$ndarray$int$__serialize__(numpy$$Div$ndarray$int,$Serial$state);
numpy$$Div$ndarray$int numpy$$Div$ndarray$int$__deserialize__(numpy$$Div$ndarray$int,$Serial$state);
numpy$$ndarray numpy$$Div$ndarray$int$__truediv__ (numpy$$Div$ndarray$int, numpy$$ndarray, numpy$$ndarray);

// numpy$$Div$ndarray$float ////////////////////////////////////////////////////////////

struct numpy$$Div$ndarray$float {
    numpy$$Div$ndarray$float$class $class;
    $Real w$Real;
};

struct numpy$$Div$ndarray$float$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)(numpy$$Div$ndarray$float);
    void (*__serialize__)(numpy$$Div$ndarray$float,$Serial$state);
    numpy$$Div$ndarray$float (*__deserialize__)(numpy$$Div$ndarray$float,$Serial$state);
    $bool (*__bool__)(numpy$$Div$ndarray$float);
    $str (*__str__)(numpy$$Div$ndarray$float);
    numpy$$ndarray (*__truediv__)(numpy$$Div$ndarray$float, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__itruediv__)(numpy$$Div$ndarray$float, numpy$$ndarray, numpy$$ndarray);
};

void numpy$$Div$ndarray$float$__init__ (numpy$$Div$ndarray$float);
void numpy$$Div$ndarray$float$__serialize__(numpy$$Div$ndarray$float,$Serial$state);
numpy$$Div$ndarray$float numpy$$Div$ndarray$float$__deserialize__(numpy$$Div$ndarray$float,$Serial$state);
numpy$$ndarray numpy$$Div$ndarray$float$__truediv__ (numpy$$Div$ndarray$float, numpy$$ndarray, numpy$$ndarray);

// numpy$$Sliceable$ndarray /////////////////////////////////////////////////////////////////

struct numpy$$Sliceable$ndarray;
typedef struct numpy$$Sliceable$ndarray *numpy$$Sliceable$ndarray;

struct numpy$$Sliceable$ndarray$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__) (numpy$$Sliceable$ndarray);
    void (*__serialize__) (numpy$$Sliceable$ndarray, $Serial$state);
    numpy$$Sliceable$ndarray (*__deserialize__) (numpy$$Sliceable$ndarray, $Serial$state);
    $bool (*__bool__)(numpy$$Sliceable$ndarray);
    $str (*__str__)(numpy$$Sliceable$ndarray);
    numpy$$ndarray (*__getitem__) (numpy$$Sliceable$ndarray, numpy$$ndarray, $int);
    void (*__setitem__) (numpy$$Sliceable$ndarray, numpy$$ndarray, $int, $WORD);
    void (*__delitem__) (numpy$$Sliceable$ndarray, numpy$$ndarray, $int);
    numpy$$ndarray (*__getslice__) (numpy$$Sliceable$ndarray, numpy$$ndarray, $slice);
    void (*__setslice__) (numpy$$Sliceable$ndarray, numpy$$ndarray, $Iterable, $slice, $WORD);
    void (*__delslice__) (numpy$$Sliceable$ndarray, numpy$$ndarray, $slice);
};
struct numpy$$Sliceable$ndarray {
    struct numpy$$Sliceable$ndarray$class *$class;
};


// numpy$$Collection$ndarray ////////////////////////////////////////////////////////////

struct numpy$$Collection$ndarray {
  numpy$$Collection$ndarray$class $class;
  numpy$$Primitive pwit;
};

struct numpy$$Collection$ndarray$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    void (*__init__)(numpy$$Collection$ndarray, numpy$$Primitive);
    void (*__serialize__)(numpy$$Collection$ndarray,$Serial$state);
    numpy$$Collection$ndarray (*__deserialize__)(numpy$$Collection$ndarray,$Serial$state);
    $bool (*__bool__)(numpy$$Collection$ndarray);
    $str (*__str__)(numpy$$Collection$ndarray);
    $Iterator (*__iter__)(numpy$$Collection$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__fromiter__)(numpy$$Collection$ndarray, $Iterable);
    $int (*__len__)(numpy$$Collection$ndarray, numpy$$ndarray);
};

void numpy$$Collection$ndarray$__init__ (numpy$$Collection$ndarray, numpy$$Primitive);
void numpy$$Collection$ndarray$__serialize__(numpy$$Collection$ndarray,$Serial$state);
numpy$$Collection$ndarray numpy$$Collection$ndarray$__deserialize__(numpy$$Collection$ndarray,$Serial$state);
$Iterator numpy$$Collection$ndarray$__iter__ (numpy$$Collection$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$$Collection$ndarray$__fromiter__(numpy$$Collection$ndarray, $Iterable);
$int numpy$$Collection$ndarray$__len__(numpy$$Collection$ndarray, numpy$$ndarray);

// numpy$$RealFloat$ndarray ////////////////////////////////////////////////////////

#define numpy$$RealFloat$ndarray (($Real)numpy$$Real$ndarray)
#define numpy$$RealFloat$ndarray$new(...) ($Real)numpy$$Real$ndarray$new(__VA_ARGS__)

// numpy$$RealFuns$math$ndarray ////////////////////////////////////////////////////

struct numpy$$RealFuns$math$ndarray;
typedef struct numpy$$RealFuns$math$ndarray *numpy$$RealFuns$math$ndarray;
struct numpy$$RealFuns$math$ndarray$class {
    char *$GCINFO;
    int $class_id;
    $Super$class $superclass;
    $NoneType (*__init__) (numpy$$RealFuns$math$ndarray, numpy$$Primitive, math$$RealFuns);
    $NoneType (*__serialize__) (numpy$$RealFuns$math$ndarray, $Serial$state);
    numpy$$RealFuns$math$ndarray (*__deserialize__) (numpy$$RealFuns$math$ndarray, $Serial$state);
    $bool (*__bool__)(numpy$$RealFuns$math$ndarray);
    $str (*__str__)(numpy$$RealFuns$math$ndarray);
    numpy$$ndarray (*sqrt) (numpy$$RealFuns$math$ndarray, numpy$$ndarray);
    numpy$$ndarray (*exp) (numpy$$RealFuns$math$ndarray, numpy$$ndarray);
    numpy$$ndarray (*log) (numpy$$RealFuns$math$ndarray, numpy$$ndarray);
    numpy$$ndarray (*sin) (numpy$$RealFuns$math$ndarray, numpy$$ndarray);
    numpy$$ndarray (*cos) (numpy$$RealFuns$math$ndarray, numpy$$ndarray);
    numpy$$ndarray (*tan) (numpy$$RealFuns$math$ndarray, numpy$$ndarray);
    numpy$$ndarray (*asin) (numpy$$RealFuns$math$ndarray, numpy$$ndarray);
    numpy$$ndarray (*acos) (numpy$$RealFuns$math$ndarray, numpy$$ndarray);
    numpy$$ndarray (*atan) (numpy$$RealFuns$math$ndarray, numpy$$ndarray);
    numpy$$ndarray (*sinh) (numpy$$RealFuns$math$ndarray, numpy$$ndarray);
    numpy$$ndarray (*cosh) (numpy$$RealFuns$math$ndarray, numpy$$ndarray);
    numpy$$ndarray (*tanh) (numpy$$RealFuns$math$ndarray, numpy$$ndarray);
    numpy$$ndarray (*asinh) (numpy$$RealFuns$math$ndarray, numpy$$ndarray);
    numpy$$ndarray (*acosh) (numpy$$RealFuns$math$ndarray, numpy$$ndarray);
    numpy$$ndarray (*atanh) (numpy$$RealFuns$math$ndarray, numpy$$ndarray);
};
struct numpy$$RealFuns$math$ndarray {
    struct numpy$$RealFuns$math$ndarray$class *$class;
    numpy$$Primitive w$Primitive$A$RealFuns$math$ndarray;
  math$$RealFuns w$RealFuns$math$A$RealFuns$math$ndarray;
};
extern struct numpy$$RealFuns$math$ndarray$class numpy$$RealFuns$math$ndarray$methods;


// method tables /////////////////////////////////////////////////////////////////

extern struct numpy$$Integral$ndarray$int$class numpy$$Integral$ndarray$int$methods;
extern struct numpy$$Logical$ndarray$int$class numpy$$Logical$ndarray$int$methods;
extern struct numpy$$Minus$ndarray$int$class numpy$$Minus$ndarray$int$methods;
extern struct numpy$$Real$ndarray$class numpy$$Real$ndarray$methods;
extern struct numpy$$Minus$ndarray$class numpy$$Minus$ndarray$methods;
extern struct numpy$$Div$ndarray$int$class numpy$$Div$ndarray$int$methods;
extern struct numpy$$Div$ndarray$float$class numpy$$Div$ndarray$float$methods;
extern struct numpy$$Sliceable$ndarray$class numpy$$Sliceable$ndarray$methods;
extern struct numpy$$Collection$ndarray$class numpy$$Collection$ndarray$methods;

numpy$$Integral$ndarray$int numpy$$Integral$ndarray$int$new();
numpy$$Logical$ndarray$int numpy$$Logical$ndarray$int$new($Integral);
numpy$$Minus$ndarray$int numpy$$Minus$ndarray$int$new($Integral);
numpy$$Real$ndarray numpy$$Real$ndarray$new();
numpy$$Minus$ndarray numpy$$Minus$ndarray$new($Real);
numpy$$Div$ndarray$int numpy$$Div$ndarray$int$new();
numpy$$Div$ndarray$float numpy$$Div$ndarray$float$new();
numpy$$Sliceable$ndarray numpy$$Sliceable$ndarray$new();
numpy$$Collection$ndarray numpy$$Collection$ndarray$new(numpy$$Primitive);
numpy$$RealFuns$math$ndarray numpy$$RealFuns$math$ndarray$new(numpy$$Primitive, math$$RealFuns);


void numpy$$__init__();

void quickselect(union $Bytes8 *a, int left, int right, int k, bool (*lt)(union $Bytes8,union $Bytes8));
void quicksort(union $Bytes8 *a, int left, int right, bool (*lt)(union $Bytes8,union $Bytes8));
