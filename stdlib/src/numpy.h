#pragma once
#include "builtin/builtin.h"

#include "math.h"

struct numpy$$ndselect;
typedef struct numpy$$ndselect *numpy$$ndselect;
struct numpy$$ndselectG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__) (numpy$$ndselect);
    void (*__serialize__) (numpy$$ndselect, $Serial$state);
    numpy$$ndselect (*__deserialize__) (numpy$$ndselect, $Serial$state);
    B_bool (*__bool__) (numpy$$ndselect);
    B_str (*__str__) (numpy$$ndselect);
    B_str (*__repr__) (numpy$$ndselect);
};
struct numpy$$ndselect {
    struct numpy$$ndselectG_class *$class;
};
extern struct numpy$$ndselectG_class numpy$$ndselectG_methods;
numpy$$ndselect numpy$$ndselectG_new();
struct numpy$$ndindex;
typedef struct numpy$$ndindex *numpy$$ndindex;
struct numpy$$ndindexG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__) (numpy$$ndindex, B_int);
    void (*__serialize__) (numpy$$ndindex, $Serial$state);
    numpy$$ndindex (*__deserialize__) (numpy$$ndindex, $Serial$state);
    B_bool (*__bool__) (numpy$$ndindex);
    B_str (*__str__) (numpy$$ndindex);
    B_str (*__repr__) (numpy$$ndindex);
};
struct numpy$$ndindex {
    struct numpy$$ndindexG_class *$class;
    B_int index;
};
extern struct numpy$$ndindexG_class numpy$$ndindexG_methods;
numpy$$ndindex numpy$$ndindexG_new(B_int);


struct numpy$$ndslice;
typedef struct numpy$$ndslice *numpy$$ndslice;
struct numpy$$ndsliceG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__) (numpy$$ndslice, B_slice);
    void (*__serialize__) (numpy$$ndslice, $Serial$state);
    numpy$$ndslice (*__deserialize__) (numpy$$ndslice, $Serial$state);
    B_bool (*__bool__) (numpy$$ndslice);
    B_str (*__str__) (numpy$$ndslice);
    B_str (*__repr__) (numpy$$ndslice);
};
struct numpy$$ndslice {
    struct numpy$$ndsliceG_class *$class;
    B_slice slc;
};
extern struct numpy$$ndsliceG_class numpy$$ndsliceG_methods;
numpy$$ndslice numpy$$ndsliceG_new(B_slice);


// The bulk of data in an ndarray is stored in a C array of union $Bytes8 data.
// Each ndarray also holds the address of an $UnboxedFunctions struct, containing conversion
// functions to and from boxed data, and operators on unboxed data.
// This file provides the necessary type definitions and $UnboxedFunctions structs for the
// two Acton types supported at the moment, int and float (i.e., boxed long and double).



struct numpy$$Primitive;
typedef struct numpy$$Primitive *numpy$$Primitive;

struct numpy$$PrimitiveG_class;
typedef struct numpy$$PrimitiveG_class *numpy$$PrimitiveG_class;

struct numpy$$PrimitiveB_int;
typedef struct numpy$$PrimitiveB_int *numpy$$PrimitiveB_int;

struct numpy$$PrimitiveB_intG_class;
typedef struct numpy$$PrimitiveB_intG_class *numpy$$PrimitiveB_intG_class;

struct numpy$$PrimitiveB_float;
typedef struct numpy$$PrimitiveB_float *numpy$$PrimitiveB_float;

struct numpy$$PrimitiveB_floatG_class;
typedef struct numpy$$PrimitiveB_floatG_class *numpy$$PrimitiveB_floatG_class;

struct numpy$$Primitive {
    numpy$$PrimitiveG_class $class;
};

union $Bytes8 {
  long l;
  double d;
};

enum ElemType {LongType,DblType};

int $elem_size(enum ElemType typ);

struct numpy$$PrimitiveG_class {
  char *$GCINFO;
  int $class_id;
  $SuperG_class $superclass;
  void (*__init__)(numpy$$Primitive);
  void (*__serialize__)(numpy$$Primitive,$Serial$state);
  numpy$$Primitive (*__deserialize__)(numpy$$Primitive,$Serial$state);
  B_bool (*__bool__)(numpy$$Primitive);
  B_str (*__str__)(numpy$$Primitive);
  B_str (*__repr__)(numpy$$Primitive);
  enum ElemType elem_type;
  $WORD (*to$obj)(union $Bytes8);
  union $Bytes8 (*from$obj)($WORD);
  B_str (*$prim_str)(union $Bytes8);
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

B_str l$prim_str(union $Bytes8 n);
B_str d$prim_str(union $Bytes8 n);

// Primitive instance for int ///////////////////////////////////////////////////////////////

struct numpy$$PrimitiveB_int {
    numpy$$PrimitiveB_intG_class $class;
};

struct numpy$$PrimitiveB_intG_class {
  char *$GCINFO;
  int $class_id;
  $SuperG_class $superclass;
  void (*__init__)(numpy$$PrimitiveB_int);
  void (*__serialize__)(numpy$$PrimitiveB_int,$Serial$state);
  numpy$$PrimitiveB_int (*__deserialize__)(numpy$$PrimitiveB_int,$Serial$state);
  B_bool (*__bool__)(numpy$$PrimitiveB_int);
  B_str (*__str__)(numpy$$PrimitiveB_int);
  B_str (*__repr__)(numpy$$PrimitiveB_int);
  enum ElemType elem_type;
  $WORD (*to$obj)(union $Bytes8);
  union $Bytes8 (*from$obj)($WORD);
  B_str (*$prim_str)(union $Bytes8);
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

struct numpy$$PrimitiveB_float {
  numpy$$PrimitiveB_floatG_class $class;
};

struct numpy$$PrimitiveB_floatG_class {
  char *$GCINFO;
  int $class_id;
  $SuperG_class $superclass;
  void (*__init__)(numpy$$PrimitiveB_float);
  void (*__serialize__)(numpy$$PrimitiveB_float,$Serial$state);
  numpy$$PrimitiveB_float (*__deserialize__)(numpy$$PrimitiveB_float,$Serial$state);
  B_bool (*__bool__)(numpy$$PrimitiveB_float);
  B_str (*__str__)(numpy$$PrimitiveB_float);
  B_str (*__repr__)(numpy$$PrimitiveB_float);
  enum ElemType elem_type;
  $WORD (*to$obj)(union $Bytes8);
  union $Bytes8 (*from$obj)($WORD);
  B_str (*$prim_str)(union $Bytes8);
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

numpy$$PrimitiveB_int numpy$$PrimitiveB_intG_new();
numpy$$PrimitiveB_float numpy$$PrimitiveB_floatG_new();

extern struct numpy$$PrimitiveB_intG_class  numpy$$PrimitiveB_intG_methods;
extern struct numpy$$PrimitiveB_floatG_class  numpy$$PrimitiveB_floatG_methods;

extern struct numpy$$PrimitiveB_int *numpy$$PrimitiveB_intG_witness;
extern struct numpy$$PrimitiveB_float *numpy$$PrimitiveB_floatG_witness;

// numpy$$ndarray /////////////////////////////////////////////////////////////////////////////////

struct numpy$$ndarray;
typedef struct numpy$$ndarray *numpy$$ndarray;

struct numpy$$ndarrayG_class {
  char *$GCINFO;
  int $class_id;
  $SuperG_class $superclass;
    void (*__init__)(numpy$$ndarray,numpy$$Primitive,B_atom);
  void (*__serialize__)(numpy$$ndarray,$Serial$state); 
  numpy$$ndarray (*__deserialize__)(numpy$$ndarray,$Serial$state);
  B_bool (*__bool__)(numpy$$ndarray);
  B_str (*__str__)(numpy$$ndarray);
  B_str (*__repr__)(numpy$$ndarray);
  numpy$$ndarray (*reshape)(numpy$$ndarray,B_list);
  numpy$$ndarray (*transpose)(numpy$$ndarray,B_list);
  numpy$$ndarray (*flatten)(numpy$$ndarray);
  numpy$$ndarray (*copy)(numpy$$ndarray);
  numpy$$ndarray (*__ndgetslice__)(numpy$$ndarray,B_list);
};

struct numpy$$ndarray {
  struct numpy$$ndarrayG_class *$class;
  enum ElemType elem_type;
  long ndim;
  B_int size;         // # of elements; equal to product of elements in shape.
  long offset;
  long elem_size;
  B_list shape;
  B_list strides;
  union $Bytes8 *data;
};

extern struct numpy$$ndarrayG_class numpy$$ndarrayG_methods;

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



typedef struct numpy$B_IteratorD_ndarray *numpy$B_IteratorD_ndarray; ;

struct numpy$B_IteratorD_ndarrayG_class {
  char *$GCINFO;
  int $class_id;
  $SuperG_class $superclass;
  void (*__init__)(numpy$B_IteratorD_ndarray, numpy$$Primitive, numpy$$ndarray);
  void (*__serialize__)(numpy$B_IteratorD_ndarray,$Serial$state);
  numpy$B_IteratorD_ndarray (*__deserialize__)(numpy$B_IteratorD_ndarray,$Serial$state);
  B_bool (*__bool__)(numpy$B_IteratorD_ndarray);
  B_str (*__str__)(numpy$B_IteratorD_ndarray);
  B_str (*__repr__)(numpy$B_IteratorD_ndarray);
  $WORD (*__next__)(numpy$B_IteratorD_ndarray);
};

numpy$$ndarray numpy$$ndarrayG_new(numpy$$Primitive, B_atom);

struct numpy$B_IteratorD_ndarray {
  struct numpy$B_IteratorD_ndarrayG_class *$class;
  numpy$$Primitive pwit;
  numpy$$array_iterator_state it;
};

extern struct  numpy$B_IteratorD_ndarrayG_class  numpy$B_IteratorD_ndarrayG_methods;

numpy$B_IteratorD_ndarray numpy$B_IteratorD_ndarrayG_new(numpy$$Primitive,numpy$$ndarray);

// Intended argument to constructor

numpy$$ndarray numpy$$fromatom(numpy$$Primitive, B_atom);

//numpy$$ndarray numpy$$ndarray_func(union $Bytes8(*f)(union $Bytes8),numpy$$ndarray a);
//numpy$$ndarray numpy$$ndarray_oper(union $Bytes8 (*f)(union $Bytes8, union $Bytes8), numpy$$ndarray a, numpy$$ndarray b);

// Methods in ndarray class //////////////////////////////////////////////

numpy$$ndarray numpy$$ndarray$reshape(numpy$$ndarray,B_list);
numpy$$ndarray numpy$$ndarray$transpose(numpy$$ndarray,B_list);
numpy$$ndarray numpy$$ndarray$flatten(numpy$$ndarray);
numpy$$ndarray numpy$$ndarray$copy(numpy$$ndarray);
numpy$$ndarray numpy$$ndarrayD___ndgetslice__(numpy$$ndarray,B_list);

// Functions to create ndarrays /////////////////////////////////////////

numpy$$ndarray numpy$$linspace(B_float a, B_float b, B_int n);
numpy$$ndarray numpy$$arange(B_int start, B_int stop, B_int step);
numpy$$ndarray numpy$$array(numpy$$Primitive wit, B_list elems);
numpy$$ndarray numpy$$full(numpy$$Primitive wit, B_list shape, $WORD val);
numpy$$ndarray numpy$$unirandint(B_int a, B_int b, B_int n);
numpy$$ndarray numpy$$unirandfloat(B_float a, B_float b, B_int n);
numpy$$ndarray numpy$$tile(numpy$$Primitive wit, numpy$$ndarray a, B_int n);
numpy$$ndarray numpy$$roll(numpy$$Primitive wit, numpy$$ndarray a, B_int n);
numpy$$ndarray numpy$$concatenate(numpy$$Primitive wit, B_list as);
numpy$$ndarray numpy$$zeros(numpy$$Primitive wit, B_int n);

// Various utilities /////////////////////////////////////////////////////

numpy$$ndarray numpy$$sum(numpy$$Primitive wit, numpy$$ndarray a, B_int axis);
numpy$$ndarray numpy$$partition(numpy$$Primitive wit, numpy$$ndarray a, B_int k);
numpy$$ndarray numpy$$sort(numpy$$Primitive wit, numpy$$ndarray a, B_int axis);
numpy$$ndarray numpy$$clip(numpy$$Primitive wit, numpy$$ndarray a, $WORD low, $WORD high);
numpy$$ndarray numpy$$dot(numpy$$Primitive wit, numpy$$ndarray a, numpy$$ndarray b);
numpy$$ndarray numpy$$abs(numpy$$Primitive wit, numpy$$ndarray a);
numpy$$ndarray numpy$$mean(numpy$$Primitive wit, numpy$$ndarray a, B_int axis);
$WORD numpy$$scalar(numpy$$Primitive wit, numpy$$ndarray a);

// newaxis //////////////////////////////////////////////////////////

extern B_int numpy$G_newaxis;

struct numpy$B_IntegralD_ndarrayB_int;
typedef struct numpy$B_IntegralD_ndarrayB_int *numpy$B_IntegralD_ndarrayB_int;

struct numpy$B_IntegralD_ndarrayB_intG_class;
typedef struct numpy$B_IntegralD_ndarrayB_intG_class *numpy$B_IntegralD_ndarrayB_intG_class;

struct numpy$B_LogicalD_ndarrayB_int;
typedef struct numpy$B_LogicalD_ndarrayB_int *numpy$B_LogicalD_ndarrayB_int;

struct numpy$B_LogicalD_ndarrayB_intG_class;
typedef struct numpy$B_LogicalD_ndarrayB_intG_class *numpy$B_LogicalD_ndarrayB_intG_class;

struct numpy$B_MinusD_ndarrayB_int;
typedef struct numpy$B_MinusD_ndarrayB_int *numpy$B_MinusD_ndarrayB_int;

struct numpy$B_MinusD_ndarrayB_intG_class;
typedef struct numpy$B_MinusD_ndarrayB_intG_class *numpy$B_MinusD_ndarrayB_intG_class;

struct numpy$B_RealD_ndarray;
typedef struct numpy$B_RealD_ndarray *numpy$B_RealD_ndarray;

struct numpy$B_RealD_ndarrayG_class;
typedef struct numpy$B_RealD_ndarrayG_class *numpy$B_RealD_ndarrayG_class;

struct numpy$B_MinusD_ndarray;
typedef struct numpy$B_MinusD_ndarray *numpy$B_MinusD_ndarray;

struct numpy$B_MinusD_ndarrayG_class;
typedef struct numpy$B_MinusD_ndarrayG_class *numpy$B_MinusD_ndarrayG_class;

struct numpy$B_DivD_ndarrayB_int;
typedef struct numpy$B_DivD_ndarrayB_int *numpy$B_DivD_ndarrayB_int;

struct numpy$B_DivD_ndarrayB_intG_class;
typedef struct numpy$B_DivD_ndarrayB_intG_class *numpy$B_DivD_ndarrayB_intG_class;

struct numpy$B_DivD_ndarrayB_float;
typedef struct numpy$B_DivD_ndarrayB_float *numpy$B_DivD_ndarrayB_float;

struct numpy$B_DivD_ndarrayB_floatG_class;
typedef struct numpy$B_DivD_ndarrayB_floatG_class *numpy$B_DivD_ndarrayB_floatG_class;

struct numpy$B_CollectionD_ndarray;
typedef struct numpy$B_CollectionD_ndarray *numpy$B_CollectionD_ndarray;

struct numpy$B_CollectionD_ndarrayG_class;
typedef struct numpy$B_CollectionD_ndarrayG_class *numpy$B_CollectionD_ndarrayG_class;

struct numpy$B_SliceableD_ndarray;
typedef struct numpy$B_SliceableD_ndarray *numpy$B_SliceableD_ndarray;

struct numpy$B_SliceableD_ndarrayG_class;
typedef struct numpy$B_SliceableD_ndarrayG_class *numpy$B_SliceableD_ndarrayG_class;

// numpy$B_IntegralD_ndarrayB_int ////////////////////////////////////////////////////////////

struct numpy$B_IntegralD_ndarrayB_int {
    numpy$B_IntegralD_ndarrayB_intG_class $class;
    B_Logical W_Logical;
    B_Minus W_Minus;
};

struct numpy$B_IntegralD_ndarrayB_intG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(numpy$B_IntegralD_ndarrayB_int);
    void (*__serialize__)(numpy$B_IntegralD_ndarrayB_int,$Serial$state);
    numpy$B_IntegralD_ndarrayB_int (*__deserialize__)(numpy$B_IntegralD_ndarrayB_int,$Serial$state);
    B_bool (*__bool__)(numpy$B_IntegralD_ndarrayB_int);
    B_str (*__str__)(numpy$B_IntegralD_ndarrayB_int);
    B_str (*__repr__)(numpy$B_IntegralD_ndarrayB_int);
    numpy$$ndarray (*__add__)(numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__iadd__)(numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__mul__)(numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__imul__)(numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__fromatom__)(numpy$B_IntegralD_ndarrayB_int,B_atom);
    B_complex (*__complx__)(numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray);
    numpy$$ndarray (*__pow__)(numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__ipow__)(numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__neg__)(numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray);
    numpy$$ndarray (*__pos__)(numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray);
    $WORD (*real)(numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray, B_Real);
    $WORD (*imag)(numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray, B_Real);
    $WORD (*__abs__)(numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray, B_Real);
    numpy$$ndarray (*conjugate)(numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray);
    B_float (*__float__)(numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray);
    $WORD (*__trunc__)(numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray, B_Integral);
    $WORD (*__floor__)(numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray, B_Integral);
    $WORD (*__ceil__)(numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray, B_Integral);
    numpy$$ndarray (*__round__)(numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray);
    $WORD (*numerator)(numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray, B_Integral);
    $WORD (*denominator)(numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray, B_Integral);
    numpy$$ndarray (*__int__)(numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray);
    numpy$$ndarray (*__index__)(numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray);
    B_tuple (*__divmod__)(numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__floordiv__)(numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__mod__)(numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__lshift__)(numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__rshift__)(numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__ifloordiv__)(numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__imod__)(numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__ilshift__)(numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__irshift__)(numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__invert__)(numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray);
};

void numpy$B_IntegralD_ndarrayB_intD___init__ (numpy$B_IntegralD_ndarrayB_int);
void numpy$B_IntegralD_ndarrayB_intD___serialize__(numpy$B_IntegralD_ndarrayB_int,$Serial$state);
numpy$B_IntegralD_ndarrayB_int numpy$B_IntegralD_ndarrayB_intD___deserialize__(numpy$B_IntegralD_ndarrayB_int,$Serial$state);
numpy$$ndarray numpy$B_IntegralD_ndarrayB_intD___add__(numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$B_IntegralD_ndarrayB_intD___iadd__(numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$B_IntegralD_ndarrayB_intD___fromatom__(numpy$B_IntegralD_ndarrayB_int,B_atom);
B_complex numpy$B_IntegralD_ndarrayB_intD___complx__(numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray);
numpy$$ndarray numpy$B_IntegralD_ndarrayB_intD___mul__(numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$B_IntegralD_ndarrayB_intD___pow__(numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$B_IntegralD_ndarrayB_intD___neg__(numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray);
numpy$$ndarray numpy$B_IntegralD_ndarrayB_intD___pos__(numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray);
$WORD numpy$B_IntegralD_ndarrayB_int$real(numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray, B_Real);
$WORD numpy$B_IntegralD_ndarrayB_int$imag(numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray, B_Real);
$WORD numpy$B_IntegralD_ndarrayB_intD___abs__(numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray, B_Real);
numpy$$ndarray numpy$B_IntegralD_ndarrayB_int$conjugate(numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray);
B_float numpy$B_IntegralD_ndarrayB_intD___float__ (numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray);
$WORD numpy$B_IntegralD_ndarrayB_intD___trunc__ (numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray, B_Integral);
$WORD numpy$B_IntegralD_ndarrayB_intD___floor__ (numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray, B_Integral);
$WORD numpy$B_IntegralD_ndarrayB_intD___ceil__ (numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray, B_Integral);
numpy$$ndarray numpy$B_IntegralD_ndarrayB_intD___round__ (numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray);
$WORD numpy$B_IntegralD_ndarrayB_int$numerator (numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray, B_Integral);
$WORD numpy$B_IntegralD_ndarrayB_int$denominator (numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray, B_Integral);
numpy$$ndarray numpy$B_IntegralD_ndarrayB_intD___int__ (numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray);
numpy$$ndarray numpy$B_IntegralD_ndarrayB_intD___index__ (numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray);
B_tuple numpy$B_IntegralD_ndarrayB_intD___divmod__ (numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$B_IntegralD_ndarrayB_intD___floordiv__ (numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$B_IntegralD_ndarrayB_intD___mod__ (numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$B_IntegralD_ndarrayB_intD___lshift__ (numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$B_IntegralD_ndarrayB_intD___rshift__ (numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$B_IntegralD_ndarrayB_intD___invert__ (numpy$B_IntegralD_ndarrayB_int, numpy$$ndarray);

// numpy$B_LogicalD_ndarrayB_int ////////////////////////////////////////////////////////////

struct numpy$B_LogicalD_ndarrayB_int {
    numpy$B_LogicalD_ndarrayB_intG_class $class;
    B_Integral W_Integral;
};

struct numpy$B_LogicalD_ndarrayB_intG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(numpy$B_LogicalD_ndarrayB_int, B_Integral);
    void (*__serialize__)(numpy$B_LogicalD_ndarrayB_int,$Serial$state);
    numpy$B_LogicalD_ndarrayB_int (*__deserialize__)(numpy$B_LogicalD_ndarrayB_int,$Serial$state);
    B_bool (*__bool__)(numpy$B_LogicalD_ndarrayB_int);
    B_str (*__str__)(numpy$B_LogicalD_ndarrayB_int);
    B_str (*__repr__)(numpy$B_LogicalD_ndarrayB_int);
    numpy$$ndarray (*__and__)(numpy$B_LogicalD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__or__)(numpy$B_LogicalD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__xor__)(numpy$B_LogicalD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__iand__)(numpy$B_LogicalD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__ior__)(numpy$B_LogicalD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__ixor__)(numpy$B_LogicalD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray);
};

void numpy$B_LogicalD_ndarrayB_intD___init__ (numpy$B_LogicalD_ndarrayB_int, B_Integral);
void numpy$B_LogicalD_ndarrayB_intD___serialize__(numpy$B_LogicalD_ndarrayB_int,$Serial$state);
numpy$B_LogicalD_ndarrayB_int numpy$B_LogicalD_ndarrayB_intD___deserialize__(numpy$B_LogicalD_ndarrayB_int,$Serial$state);
numpy$$ndarray numpy$B_LogicalD_ndarrayB_intD___and__ (numpy$B_LogicalD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$B_LogicalD_ndarrayB_intD___or__ (numpy$B_LogicalD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$B_LogicalD_ndarrayB_intD___xor__ (numpy$B_LogicalD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray);

// numpy$B_MinusD_ndarrayB_int ////////////////////////////////////////////////////////////

struct numpy$B_MinusD_ndarrayB_int {
    numpy$B_MinusD_ndarrayB_intG_class $class;
    B_Integral W_Integral;
};

struct numpy$B_MinusD_ndarrayB_intG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(numpy$B_MinusD_ndarrayB_int, B_Integral);
    void (*__serialize__)(numpy$B_MinusD_ndarrayB_int,$Serial$state);
    numpy$B_MinusD_ndarrayB_int (*__deserialize__)(numpy$B_MinusD_ndarrayB_int,$Serial$state);
    B_bool (*__bool__)(numpy$B_MinusD_ndarrayB_int);
    B_str (*__str__)(numpy$B_MinusD_ndarrayB_int);
    B_str (*__repr__)(numpy$B_MinusD_ndarrayB_int);
    numpy$$ndarray (*__sub__)(numpy$B_MinusD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__isub__)(numpy$B_MinusD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray);
};

void numpy$B_MinusD_ndarrayB_intD___init__ (numpy$B_MinusD_ndarrayB_int, B_Integral);
void numpy$B_MinusD_ndarrayB_intD___serialize__(numpy$B_MinusD_ndarrayB_int,$Serial$state);
numpy$B_MinusD_ndarrayB_int numpy$B_MinusD_ndarrayB_intD___deserialize__(numpy$B_MinusD_ndarrayB_int,$Serial$state);
numpy$$ndarray numpy$B_MinusD_ndarrayB_intD___sub__ (numpy$B_MinusD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray);

// numpy$B_RealD_ndarray ////////////////////////////////////////////////////////////

struct numpy$B_RealD_ndarray {
    numpy$B_RealD_ndarrayG_class $class;
    B_Minus W_Minus;
    numpy$$Primitive W_PrimitiveD_AD_Real$ndarray;
};

struct numpy$B_RealD_ndarrayG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
  void (*__init__)(numpy$B_RealD_ndarray, numpy$$Primitive);
    void (*__serialize__)(numpy$B_RealD_ndarray,$Serial$state);
    numpy$B_RealD_ndarray (*__deserialize__)(numpy$B_RealD_ndarray,$Serial$state);
    B_bool (*__bool__)(numpy$B_RealD_ndarray);
    B_str (*__str__)(numpy$B_RealD_ndarray);
    B_str (*__repr__)(numpy$B_RealD_ndarray);
    numpy$$ndarray (*__add__)(numpy$B_RealD_ndarray, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__iadd__)(numpy$B_RealD_ndarray, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__mul__)(numpy$B_RealD_ndarray, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__imul__)(numpy$B_RealD_ndarray, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__fromatom__)(numpy$B_RealD_ndarray,B_atom);
    B_complex (*__complx__)(numpy$B_RealD_ndarray, numpy$$ndarray);
    numpy$$ndarray (*__pow__)(numpy$B_RealD_ndarray, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__ipow__)(numpy$B_RealD_ndarray, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__neg__)(numpy$B_RealD_ndarray, numpy$$ndarray);
    numpy$$ndarray (*__pos__)(numpy$B_RealD_ndarray, numpy$$ndarray);
    $WORD (*real)(numpy$B_RealD_ndarray, numpy$$ndarray, B_Real);
    $WORD (*imag)(numpy$B_RealD_ndarray, numpy$$ndarray, B_Real);
    $WORD (*__abs__)(numpy$B_RealD_ndarray, numpy$$ndarray, B_Real);
    numpy$$ndarray (*conjugate)(numpy$B_RealD_ndarray, numpy$$ndarray);
    B_float (*__float__)(numpy$B_RealD_ndarray, numpy$$ndarray);
    $WORD (*__trunc__)(numpy$B_RealD_ndarray, numpy$$ndarray, B_Integral);
    $WORD (*__floor__)(numpy$B_RealD_ndarray, numpy$$ndarray, B_Integral);
    $WORD (*__ceil__)(numpy$B_RealD_ndarray, numpy$$ndarray, B_Integral);
    numpy$$ndarray (*__round__)(numpy$B_RealD_ndarray, numpy$$ndarray, numpy$$ndarray);
};

void numpy$B_RealD_ndarrayD___init__ (numpy$B_RealD_ndarray,numpy$$Primitive);
void numpy$B_RealD_ndarrayD___serialize__(numpy$B_RealD_ndarray,$Serial$state);
numpy$B_RealD_ndarray numpy$B_RealD_ndarrayD___deserialize__(numpy$B_RealD_ndarray,$Serial$state);
numpy$$ndarray numpy$B_RealD_ndarrayD___add__(numpy$B_RealD_ndarray, numpy$$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$B_RealD_ndarrayD___iadd__(numpy$B_RealD_ndarray, numpy$$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$B_RealD_ndarrayD___fromatom__(numpy$B_RealD_ndarray,B_atom);
B_complex numpy$B_RealD_ndarrayD___complx__(numpy$B_RealD_ndarray, numpy$$ndarray);
numpy$$ndarray numpy$B_RealD_ndarrayD___mul__(numpy$B_RealD_ndarray, numpy$$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$B_RealD_ndarrayD___pow__(numpy$B_RealD_ndarray, numpy$$ndarray, numpy$$ndarray);
numpy$$ndarray numpy$B_RealD_ndarrayD___neg__(numpy$B_RealD_ndarray, numpy$$ndarray);
numpy$$ndarray numpy$B_RealD_ndarrayD___pos__(numpy$B_RealD_ndarray, numpy$$ndarray);
$WORD numpy$B_RealD_ndarray$real(numpy$B_RealD_ndarray, numpy$$ndarray, B_Real);
$WORD numpy$B_RealD_ndarray$imag(numpy$B_RealD_ndarray, numpy$$ndarray, B_Real);
$WORD numpy$B_RealD_ndarrayD___abs__(numpy$B_RealD_ndarray, numpy$$ndarray, B_Real);
numpy$$ndarray numpy$B_RealD_ndarray$conjugate(numpy$B_RealD_ndarray, numpy$$ndarray);
B_float numpy$B_RealD_ndarrayD___float__ (numpy$B_RealD_ndarray, numpy$$ndarray);
$WORD numpy$B_RealD_ndarrayD___trunc__ (numpy$B_RealD_ndarray, numpy$$ndarray, B_Integral);
$WORD numpy$B_RealD_ndarrayD___floor__ (numpy$B_RealD_ndarray, numpy$$ndarray, B_Integral);
$WORD numpy$B_RealD_ndarrayD___ceil__ (numpy$B_RealD_ndarray, numpy$$ndarray, B_Integral);
numpy$$ndarray numpy$B_RealD_ndarrayD___round__ (numpy$B_RealD_ndarray, numpy$$ndarray, numpy$$ndarray);

// numpy$B_MinusD_ndarray ////////////////////////////////////////////////////////////

struct numpy$B_MinusD_ndarray {
    numpy$B_MinusD_ndarrayG_class $class;
    B_Real W_Real;
};

struct numpy$B_MinusD_ndarrayG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(numpy$B_MinusD_ndarray, B_Real);
    void (*__serialize__)(numpy$B_MinusD_ndarray,$Serial$state);
    numpy$B_MinusD_ndarray (*__deserialize__)(numpy$B_MinusD_ndarray,$Serial$state);
    B_bool (*__bool__)(numpy$B_MinusD_ndarray);
    B_str (*__str__)(numpy$B_MinusD_ndarray);
    B_str (*__repr__)(numpy$B_MinusD_ndarray);
    numpy$$ndarray (*__sub__)(numpy$B_MinusD_ndarray, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__isub__)(numpy$B_MinusD_ndarray, numpy$$ndarray, numpy$$ndarray);
};

void numpy$B_MinusD_ndarrayD___init__ (numpy$B_MinusD_ndarray, B_Real);
void numpy$B_MinusD_ndarrayD___serialize__(numpy$B_MinusD_ndarray,$Serial$state);
numpy$B_MinusD_ndarray numpy$B_MinusD_ndarrayD___deserialize__(numpy$B_MinusD_ndarray,$Serial$state);
numpy$$ndarray numpy$B_MinusD_ndarrayD___sub__ (numpy$B_MinusD_ndarray, numpy$$ndarray, numpy$$ndarray);

// numpy$B_DivD_ndarrayB_int ////////////////////////////////////////////////////////////

struct numpy$B_DivD_ndarrayB_int {
    numpy$B_DivD_ndarrayB_intG_class $class;
    B_Real W_Real;
};

struct numpy$B_DivD_ndarrayB_intG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(numpy$B_DivD_ndarrayB_int);
    void (*__serialize__)(numpy$B_DivD_ndarrayB_int,$Serial$state);
    numpy$B_DivD_ndarrayB_int (*__deserialize__)(numpy$B_DivD_ndarrayB_int,$Serial$state);
    B_bool (*__bool__)(numpy$B_DivD_ndarrayB_int);
    B_str (*__str__)(numpy$B_DivD_ndarrayB_int);
    B_str (*__repr__)(numpy$B_DivD_ndarrayB_int);
    numpy$$ndarray (*__truediv__)(numpy$B_DivD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__itruediv__)(numpy$B_DivD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray);
};

void numpy$B_DivD_ndarrayB_intD___init__ (numpy$B_DivD_ndarrayB_int);
void numpy$B_DivD_ndarrayB_intD___serialize__(numpy$B_DivD_ndarrayB_int,$Serial$state);
numpy$B_DivD_ndarrayB_int numpy$B_DivD_ndarrayB_intD___deserialize__(numpy$B_DivD_ndarrayB_int,$Serial$state);
numpy$$ndarray numpy$B_DivD_ndarrayB_intD___truediv__ (numpy$B_DivD_ndarrayB_int, numpy$$ndarray, numpy$$ndarray);

// numpy$B_DivD_ndarrayB_float ////////////////////////////////////////////////////////////

struct numpy$B_DivD_ndarrayB_float {
    numpy$B_DivD_ndarrayB_floatG_class $class;
    B_Real W_Real;
};

struct numpy$B_DivD_ndarrayB_floatG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(numpy$B_DivD_ndarrayB_float);
    void (*__serialize__)(numpy$B_DivD_ndarrayB_float,$Serial$state);
    numpy$B_DivD_ndarrayB_float (*__deserialize__)(numpy$B_DivD_ndarrayB_float,$Serial$state);
    B_bool (*__bool__)(numpy$B_DivD_ndarrayB_float);
    B_str (*__str__)(numpy$B_DivD_ndarrayB_float);
    B_str (*__repr__)(numpy$B_DivD_ndarrayB_float);
    numpy$$ndarray (*__truediv__)(numpy$B_DivD_ndarrayB_float, numpy$$ndarray, numpy$$ndarray);
    numpy$$ndarray (*__itruediv__)(numpy$B_DivD_ndarrayB_float, numpy$$ndarray, numpy$$ndarray);
};

void numpy$B_DivD_ndarrayB_floatD___init__ (numpy$B_DivD_ndarrayB_float);
void numpy$B_DivD_ndarrayB_floatD___serialize__(numpy$B_DivD_ndarrayB_float,$Serial$state);
numpy$B_DivD_ndarrayB_float numpy$B_DivD_ndarrayB_floatD___deserialize__(numpy$B_DivD_ndarrayB_float,$Serial$state);
numpy$$ndarray numpy$B_DivD_ndarrayB_floatD___truediv__ (numpy$B_DivD_ndarrayB_float, numpy$$ndarray, numpy$$ndarray);

// numpy$B_SliceableD_ndarray /////////////////////////////////////////////////////////////////

struct numpy$B_SliceableD_ndarray;
typedef struct numpy$B_SliceableD_ndarray *numpy$B_SliceableD_ndarray;

struct numpy$B_SliceableD_ndarrayG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__) (numpy$B_SliceableD_ndarray, numpy$$Primitive);
    void (*__serialize__) (numpy$B_SliceableD_ndarray, $Serial$state);
    numpy$B_SliceableD_ndarray (*__deserialize__) (numpy$B_SliceableD_ndarray, $Serial$state);
    B_bool (*__bool__)(numpy$B_SliceableD_ndarray);
    B_str (*__str__)(numpy$B_SliceableD_ndarray);
    B_str (*__repr__)(numpy$B_SliceableD_ndarray);
    numpy$$ndarray (*__getitem__) (numpy$B_SliceableD_ndarray, numpy$$ndarray, B_int);
    void (*__setitem__) (numpy$B_SliceableD_ndarray, numpy$$ndarray, B_int, $WORD);
    void (*__delitem__) (numpy$B_SliceableD_ndarray, numpy$$ndarray, B_int);
    numpy$$ndarray (*__getslice__) (numpy$B_SliceableD_ndarray, numpy$$ndarray, B_slice);
    void (*__setslice__) (numpy$B_SliceableD_ndarray, numpy$$ndarray, B_Iterable, B_slice, $WORD);
    void (*__delslice__) (numpy$B_SliceableD_ndarray, numpy$$ndarray, B_slice);
};
struct numpy$B_SliceableD_ndarray {
    numpy$B_SliceableD_ndarrayG_class $class;
    numpy$$Primitive pwit;
};


// numpy$B_CollectionD_ndarray ////////////////////////////////////////////////////////////

struct numpy$B_CollectionD_ndarray {
  numpy$B_CollectionD_ndarrayG_class $class;
  numpy$$Primitive pwit;
};

struct numpy$B_CollectionD_ndarrayG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(numpy$B_CollectionD_ndarray, numpy$$Primitive);
    void (*__serialize__)(numpy$B_CollectionD_ndarray,$Serial$state);
    numpy$B_CollectionD_ndarray (*__deserialize__)(numpy$B_CollectionD_ndarray,$Serial$state);
    B_bool (*__bool__)(numpy$B_CollectionD_ndarray);
    B_str (*__str__)(numpy$B_CollectionD_ndarray);
    B_str (*__repr__)(numpy$B_CollectionD_ndarray);
    B_Iterator (*__iter__)(numpy$B_CollectionD_ndarray, numpy$$ndarray);
    numpy$$ndarray (*__fromiter__)(numpy$B_CollectionD_ndarray, B_Iterable);
    B_int (*__len__)(numpy$B_CollectionD_ndarray, numpy$$ndarray);
};

void numpy$B_CollectionD_ndarrayD___init__ (numpy$B_CollectionD_ndarray, numpy$$Primitive);
void numpy$B_CollectionD_ndarrayD___serialize__(numpy$B_CollectionD_ndarray,$Serial$state);
numpy$B_CollectionD_ndarray numpy$B_CollectionD_ndarrayD___deserialize__(numpy$B_CollectionD_ndarray,$Serial$state);
B_Iterator numpy$B_CollectionD_ndarrayD___iter__ (numpy$B_CollectionD_ndarray, numpy$$ndarray);
numpy$$ndarray numpy$B_CollectionD_ndarrayD___fromiter__(numpy$B_CollectionD_ndarray, B_Iterable);
B_int numpy$B_CollectionD_ndarrayD___len__(numpy$B_CollectionD_ndarray, numpy$$ndarray);

// numpy$B_RealFloat$ndarray ////////////////////////////////////////////////////////

#define numpy$B_RealFloat$ndarray ((B_Real)numpy$B_RealD_ndarray)
numpy$B_RealD_ndarray numpy$B_RealFloat$ndarrayG_new(numpy$$Primitive,B_RealFloat); // (B_Real)numpy$B_RealD_ndarrayG_new(__VA_ARGS__)

// numpy$B_RealFuns$math$ndarray ////////////////////////////////////////////////////

struct numpy$B_RealFuns$math$ndarray;
typedef struct numpy$B_RealFuns$math$ndarray *numpy$B_RealFuns$math$ndarray;
struct numpy$B_RealFuns$math$ndarrayG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    $NoneType (*__init__) (numpy$B_RealFuns$math$ndarray, numpy$$Primitive, mathQ_RealFuns);
    $NoneType (*__serialize__) (numpy$B_RealFuns$math$ndarray, $Serial$state);
    numpy$B_RealFuns$math$ndarray (*__deserialize__) (numpy$B_RealFuns$math$ndarray, $Serial$state);
    B_bool (*__bool__)(numpy$B_RealFuns$math$ndarray);
    B_str (*__str__)(numpy$B_RealFuns$math$ndarray);
    B_str (*__repr__)(numpy$B_RealFuns$math$ndarray);
    numpy$$ndarray (*sqrt) (numpy$B_RealFuns$math$ndarray, numpy$$ndarray);
    numpy$$ndarray (*exp) (numpy$B_RealFuns$math$ndarray, numpy$$ndarray);
    numpy$$ndarray (*log) (numpy$B_RealFuns$math$ndarray, numpy$$ndarray);
    numpy$$ndarray (*sin) (numpy$B_RealFuns$math$ndarray, numpy$$ndarray);
    numpy$$ndarray (*cos) (numpy$B_RealFuns$math$ndarray, numpy$$ndarray);
    numpy$$ndarray (*tan) (numpy$B_RealFuns$math$ndarray, numpy$$ndarray);
    numpy$$ndarray (*asin) (numpy$B_RealFuns$math$ndarray, numpy$$ndarray);
    numpy$$ndarray (*acos) (numpy$B_RealFuns$math$ndarray, numpy$$ndarray);
    numpy$$ndarray (*atan) (numpy$B_RealFuns$math$ndarray, numpy$$ndarray);
    numpy$$ndarray (*sinh) (numpy$B_RealFuns$math$ndarray, numpy$$ndarray);
    numpy$$ndarray (*cosh) (numpy$B_RealFuns$math$ndarray, numpy$$ndarray);
    numpy$$ndarray (*tanh) (numpy$B_RealFuns$math$ndarray, numpy$$ndarray);
    numpy$$ndarray (*asinh) (numpy$B_RealFuns$math$ndarray, numpy$$ndarray);
    numpy$$ndarray (*acosh) (numpy$B_RealFuns$math$ndarray, numpy$$ndarray);
    numpy$$ndarray (*atanh) (numpy$B_RealFuns$math$ndarray, numpy$$ndarray);
};
struct numpy$B_RealFuns$math$ndarray {
    struct numpy$B_RealFuns$math$ndarrayG_class *$class;
    numpy$$Primitive W_PrimitiveD_AD_RealFuns$math$ndarray;
  mathQ_RealFuns W_RealFuns$mathD_AD_RealFuns$math$ndarray;
};
extern struct numpy$B_RealFuns$math$ndarrayG_class numpy$B_RealFuns$math$ndarrayG_methods;


// method tables /////////////////////////////////////////////////////////////////

extern struct numpy$B_IntegralD_ndarrayB_intG_class numpy$B_IntegralD_ndarrayB_intG_methods;
extern struct numpy$B_LogicalD_ndarrayB_intG_class numpy$B_LogicalD_ndarrayB_intG_methods;
extern struct numpy$B_MinusD_ndarrayB_intG_class numpy$B_MinusD_ndarrayB_intG_methods;
extern struct numpy$B_RealD_ndarrayG_class numpy$B_RealD_ndarrayG_methods;
extern struct numpy$B_MinusD_ndarrayG_class numpy$B_MinusD_ndarrayG_methods;
extern struct numpy$B_DivD_ndarrayB_intG_class numpy$B_DivD_ndarrayB_intG_methods;
extern struct numpy$B_DivD_ndarrayB_floatG_class numpy$B_DivD_ndarrayB_floatG_methods;
extern struct numpy$B_SliceableD_ndarrayG_class numpy$B_SliceableD_ndarrayG_methods;
extern struct numpy$B_CollectionD_ndarrayG_class numpy$B_CollectionD_ndarrayG_methods;

numpy$B_IntegralD_ndarrayB_int numpy$B_IntegralD_ndarrayB_intG_new();
numpy$B_LogicalD_ndarrayB_int numpy$B_LogicalD_ndarrayB_intG_new(B_Integral);
numpy$B_MinusD_ndarrayB_int numpy$B_MinusD_ndarrayB_intG_new(B_Integral);
numpy$B_RealD_ndarray numpy$B_RealD_ndarrayG_new(numpy$$Primitive);
numpy$B_MinusD_ndarray numpy$B_MinusD_ndarrayG_new(B_Real);
numpy$B_DivD_ndarrayB_int numpy$B_DivD_ndarrayB_intG_new();
numpy$B_DivD_ndarrayB_float numpy$B_DivD_ndarrayB_floatG_new();
numpy$B_SliceableD_ndarray numpy$B_SliceableD_ndarrayG_new(numpy$$Primitive);
numpy$B_CollectionD_ndarray numpy$B_CollectionD_ndarrayG_new(numpy$$Primitive);
numpy$B_RealFuns$math$ndarray numpy$B_RealFuns$math$ndarrayG_new(numpy$$Primitive, mathQ_RealFuns);


void numpy$D___init__();

void quickselect(union $Bytes8 *a, int left, int right, int k, bool (*lt)(union $Bytes8,union $Bytes8));
void quicksort(union $Bytes8 *a, int left, int right, bool (*lt)(union $Bytes8,union $Bytes8));
