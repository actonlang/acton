#pragma once
#include "builtin/builtin.h"

#include "math.h"

struct numpyQ_ndselect;
typedef struct numpyQ_ndselect *numpyQ_ndselect;
struct numpyQ_ndselectG_class {
    GC_descr $GCdescr;
    char *$name;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__) (numpyQ_ndselect);
    void (*__serialize__) (numpyQ_ndselect, $Serial$state);
    numpyQ_ndselect (*__deserialize__) (numpyQ_ndselect, $Serial$state);
    B_bool (*__bool__) (numpyQ_ndselect);
    B_str (*__str__) (numpyQ_ndselect);
    B_str (*__repr__) (numpyQ_ndselect);
};
struct numpyQ_ndselect {
    struct numpyQ_ndselectG_class *$class;
};
extern GC_word numpyQ_ndselectD_gcbm[GC_BITMAP_SIZE(struct numpyQ_ndselect)];
extern struct numpyQ_ndselectG_class numpyQ_ndselectG_methods;
numpyQ_ndselect numpyQ_ndselectG_new();
struct numpyQ_ndindex;
typedef struct numpyQ_ndindex *numpyQ_ndindex;
struct numpyQ_ndindexG_class {
    GC_descr $GCdescr;
    char *$name;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__) (numpyQ_ndindex, B_int);
    void (*__serialize__) (numpyQ_ndindex, $Serial$state);
    numpyQ_ndindex (*__deserialize__) (numpyQ_ndindex, $Serial$state);
    B_bool (*__bool__) (numpyQ_ndindex);
    B_str (*__str__) (numpyQ_ndindex);
    B_str (*__repr__) (numpyQ_ndindex);
};
struct numpyQ_ndindex {
    struct numpyQ_ndindexG_class *$class;
    B_int index;
};
extern GC_word numpyQ_ndindexD_gcbm[GC_BITMAP_SIZE(struct numpyQ_ndindex)];
extern struct numpyQ_ndindexG_class numpyQ_ndindexG_methods;
numpyQ_ndindex numpyQ_ndindexG_new(B_int);


struct numpyQ_ndslice;
typedef struct numpyQ_ndslice *numpyQ_ndslice;
struct numpyQ_ndsliceG_class {
    GC_descr $GCdescr;
    char *$name;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__) (numpyQ_ndslice, B_slice);
    void (*__serialize__) (numpyQ_ndslice, $Serial$state);
    numpyQ_ndslice (*__deserialize__) (numpyQ_ndslice, $Serial$state);
    B_bool (*__bool__) (numpyQ_ndslice);
    B_str (*__str__) (numpyQ_ndslice);
    B_str (*__repr__) (numpyQ_ndslice);
};
struct numpyQ_ndslice {
    struct numpyQ_ndsliceG_class *$class;
    B_slice slc;
};
extern GC_word numpyQ_ndsliceD_gcbm[GC_BITMAP_SIZE(struct numpyQ_ndslice)];
extern struct numpyQ_ndsliceG_class numpyQ_ndsliceG_methods;
numpyQ_ndslice numpyQ_ndsliceG_new(B_slice);


// The bulk of data in an ndarray is stored in a C array of union $Bytes8 data.
// Each ndarray also holds the address of an $UnboxedFunctions struct, containing conversion
// functions to and from boxed data, and operators on unboxed data.
// This file provides the necessary type definitions and $UnboxedFunctions structs for the
// two Acton types supported at the moment, int and float (i.e., boxed long and double).



struct numpyQ_Primitive;
typedef struct numpyQ_Primitive *numpyQ_Primitive;

struct numpyQ_PrimitiveG_class;
typedef struct numpyQ_PrimitiveG_class *numpyQ_PrimitiveG_class;

struct numpyQ_PrimitiveD_int;
typedef struct numpyQ_PrimitiveD_int *numpyQ_PrimitiveD_int;

struct numpyQ_PrimitiveD_intG_class;
typedef struct numpyQ_PrimitiveD_intG_class *numpyQ_PrimitiveD_intG_class;

struct numpyQ_PrimitiveD_float;
typedef struct numpyQ_PrimitiveD_float *numpyQ_PrimitiveD_float;

struct numpyQ_PrimitiveD_floatG_class;
typedef struct numpyQ_PrimitiveD_floatG_class *numpyQ_PrimitiveD_floatG_class;

struct numpyQ_Primitive {
    numpyQ_PrimitiveG_class $class;
};
extern GC_word numpyQ_PrimitiveD_gcbm[GC_BITMAP_SIZE(struct numpyQ_Primitive)];

union $Bytes8 {
  long l;
  double d;
};

enum ElemType {LongType,DblType};

int $elem_size(enum ElemType typ);

struct numpyQ_PrimitiveG_class {
  GC_descr $GCdescr;
  char *$name;
  int $class_id;
  $SuperG_class $superclass;
  B_NoneType (*__init__)(numpyQ_Primitive);
  void (*__serialize__)(numpyQ_Primitive,$Serial$state);
  numpyQ_Primitive (*__deserialize__)(numpyQ_Primitive,$Serial$state);
  B_bool (*__bool__)(numpyQ_Primitive);
  B_str (*__str__)(numpyQ_Primitive);
  B_str (*__repr__)(numpyQ_Primitive);
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
  union $Bytes8 (*B_pow)(union $Bytes8, union $Bytes8);
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
  union $Bytes8 (*B_abs)(union $Bytes8);
  union $Bytes8 (*$neg)(union $Bytes8);
  union $Bytes8 (*$lnot)(union $Bytes8);
  union $Bytes8 (*$bnot)(union $Bytes8);
};

B_str B_l_prim_str(union $Bytes8 n);
B_str B_l_prim_str(union $Bytes8 n);

// Primitive instance for int ///////////////////////////////////////////////////////////////

struct numpyQ_PrimitiveD_int {
    numpyQ_PrimitiveD_intG_class $class;
};
extern GC_word numpyQ_PrimitiveD_intD_gcbm[GC_BITMAP_SIZE(struct numpyQ_PrimitiveD_int)];

struct numpyQ_PrimitiveD_intG_class {
  GC_descr $GCdescr;
  char *$name;
  int $class_id;
  $SuperG_class $superclass;
  B_NoneType (*__init__)(numpyQ_PrimitiveD_int);
  void (*__serialize__)(numpyQ_PrimitiveD_int,$Serial$state);
  numpyQ_PrimitiveD_int (*__deserialize__)(numpyQ_PrimitiveD_int,$Serial$state);
  B_bool (*__bool__)(numpyQ_PrimitiveD_int);
  B_str (*__str__)(numpyQ_PrimitiveD_int);
  B_str (*__repr__)(numpyQ_PrimitiveD_int);
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
  union $Bytes8 (*B_pow)(union $Bytes8, union $Bytes8);
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
  union $Bytes8 (*B_abs)(union $Bytes8);
  union $Bytes8 (*$neg)(union $Bytes8);
  union $Bytes8 (*$lnot)(union $Bytes8);
  union $Bytes8 (*$bnot)(union $Bytes8);
};

// Primitive instance for float ///////////////////////////////////////////////////////////////

struct numpyQ_PrimitiveD_float {
  numpyQ_PrimitiveD_floatG_class $class;
};
extern GC_word numpyQ_PrimitiveD_floatD_gcbm[GC_BITMAP_SIZE(struct numpyQ_PrimitiveD_float)];

struct numpyQ_PrimitiveD_floatG_class {
  GC_descr $GCdescr;
  char *$name;
  int $class_id;
  $SuperG_class $superclass;
  B_NoneType (*__init__)(numpyQ_PrimitiveD_float);
  void (*__serialize__)(numpyQ_PrimitiveD_float,$Serial$state);
  numpyQ_PrimitiveD_float (*__deserialize__)(numpyQ_PrimitiveD_float,$Serial$state);
  B_bool (*__bool__)(numpyQ_PrimitiveD_float);
  B_str (*__str__)(numpyQ_PrimitiveD_float);
  B_str (*__repr__)(numpyQ_PrimitiveD_float);
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
  union $Bytes8 (*B_pow)(union $Bytes8, union $Bytes8);
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
  union $Bytes8 (*B_abs)(union $Bytes8);
  union $Bytes8 (*$neg)(union $Bytes8);
  union $Bytes8 (*$lnot)(union $Bytes8);
  union $Bytes8 (*$bnot)(union $Bytes8);
};

// Witnesses and creation ////////////////////////////////////////////////////////////////////////////

numpyQ_PrimitiveD_int numpyQ_PrimitiveD_intG_new();
numpyQ_PrimitiveD_float numpyQ_PrimitiveD_floatG_new();

extern struct numpyQ_PrimitiveD_intG_class  numpyQ_PrimitiveD_intG_methods;
extern struct numpyQ_PrimitiveD_floatG_class  numpyQ_PrimitiveD_floatG_methods;

extern struct numpyQ_PrimitiveD_int *numpyQ_PrimitiveD_intG_witness;
extern struct numpyQ_PrimitiveD_float *numpyQ_PrimitiveD_floatG_witness;

// numpyQ_ndarray /////////////////////////////////////////////////////////////////////////////////

struct numpyQ_ndarray;
typedef struct numpyQ_ndarray *numpyQ_ndarray;

struct numpyQ_ndarrayG_class {
  GC_descr $GCdescr;
  char *$name;
  int $class_id;
  $SuperG_class $superclass;
  B_NoneType (*__init__)(numpyQ_ndarray,numpyQ_Primitive,B_atom);
  void (*__serialize__)(numpyQ_ndarray,$Serial$state); 
  numpyQ_ndarray (*__deserialize__)(numpyQ_ndarray,$Serial$state);
  B_bool (*__bool__)(numpyQ_ndarray);
  B_str (*__str__)(numpyQ_ndarray);
  B_str (*__repr__)(numpyQ_ndarray);
  numpyQ_ndarray (*reshape)(numpyQ_ndarray,B_list);
  numpyQ_ndarray (*transpose)(numpyQ_ndarray,B_list);
  numpyQ_ndarray (*flatten)(numpyQ_ndarray);
  numpyQ_ndarray (*copy)(numpyQ_ndarray);
  numpyQ_ndarray (*__ndgetslice__)(numpyQ_ndarray,B_list);
};

struct numpyQ_ndarray {
  struct numpyQ_ndarrayG_class *$class;
  enum ElemType elem_type;
  long ndim;
  B_int size;         // # of elements; equal to product of elements in shape.
  long offset;
  long elem_size;
  B_list shape;
  B_list strides;
  union $Bytes8 *data;
};
extern GC_word numpyQ_ndarrayD_gcbm[GC_BITMAP_SIZE(struct numpyQ_ndarray)];

extern struct numpyQ_ndarrayG_class numpyQ_ndarrayG_methods;

// iterating over an ndarray //////////////////////////////////////////

#define MAX_NDIM 16

typedef struct numpyQ_array_iterator_state {
  union $Bytes8 *current;
  long currentstride;
  long lastshapepos;
  long lastshapelength;
  long ndim1;    // ndim-1
  long shape[MAX_NDIM];
  long strides[MAX_NDIM];
  long jumps[MAX_NDIM];
  long index[MAX_NDIM];
} *numpyQ_array_iterator_state;



typedef struct numpyQ_IteratorD_ndarray *numpyQ_IteratorD_ndarray; ;

struct numpyQ_IteratorD_ndarrayG_class {
  GC_descr $GCdescr;
  char *$name;
  int $class_id;
  $SuperG_class $superclass;
  B_NoneType (*__init__)(numpyQ_IteratorD_ndarray, numpyQ_Primitive, numpyQ_ndarray);
  void (*__serialize__)(numpyQ_IteratorD_ndarray,$Serial$state);
  numpyQ_IteratorD_ndarray (*__deserialize__)(numpyQ_IteratorD_ndarray,$Serial$state);
  B_bool (*__bool__)(numpyQ_IteratorD_ndarray);
  B_str (*__str__)(numpyQ_IteratorD_ndarray);
  B_str (*__repr__)(numpyQ_IteratorD_ndarray);
  $WORD (*__next__)(numpyQ_IteratorD_ndarray);
};

numpyQ_ndarray numpyQ_ndarrayG_new(numpyQ_Primitive, B_atom);

struct numpyQ_IteratorD_ndarray {
  struct numpyQ_IteratorD_ndarrayG_class *$class;
  numpyQ_Primitive pwit;
  numpyQ_array_iterator_state it;
};
extern GC_word numpyQ_IteratorD_ndarrayD_gcbm[GC_BITMAP_SIZE(struct numpyQ_IteratorD_ndarray)];

extern struct  numpyQ_IteratorD_ndarrayG_class  numpyQ_IteratorD_ndarrayG_methods;

numpyQ_IteratorD_ndarray numpyQ_IteratorD_ndarrayG_new(numpyQ_Primitive,numpyQ_ndarray);

// Intended argument to constructor

numpyQ_ndarray numpyQ_fromatom(numpyQ_Primitive, B_atom);

//numpyQ_ndarray numpyQ_ndarray_func(union $Bytes8(*f)(union $Bytes8),numpyQ_ndarray a);
//numpyQ_ndarray numpyQ_ndarray_oper(union $Bytes8 (*f)(union $Bytes8, union $Bytes8), numpyQ_ndarray a, numpyQ_ndarray b);

// Methods in ndarray class //////////////////////////////////////////////

numpyQ_ndarray numpyQ_ndarray$reshape(numpyQ_ndarray,B_list);
numpyQ_ndarray numpyQ_ndarray$transpose(numpyQ_ndarray,B_list);
numpyQ_ndarray numpyQ_ndarray$flatten(numpyQ_ndarray);
numpyQ_ndarray numpyQ_ndarray$copy(numpyQ_ndarray);
numpyQ_ndarray numpyQ_ndarrayD___ndgetslice__(numpyQ_ndarray,B_list);

// Functions to create ndarrays /////////////////////////////////////////

numpyQ_ndarray numpyQ_linspace(B_float a, B_float b, B_int n);
numpyQ_ndarray numpyQ_arange(B_int start, B_int stop, B_int step);
numpyQ_ndarray numpyQ_array(numpyQ_Primitive wit, B_list elems);
numpyQ_ndarray numpyQ_full(numpyQ_Primitive wit, B_list shape, $WORD val);
numpyQ_ndarray numpyQ_unirandint(B_int a, B_int b, B_int n);
numpyQ_ndarray numpyQ_unirandfloat(B_float a, B_float b, B_int n);
numpyQ_ndarray numpyQ_tile(numpyQ_Primitive wit, numpyQ_ndarray a, B_int n);
numpyQ_ndarray numpyQ_roll(numpyQ_Primitive wit, numpyQ_ndarray a, B_int n);
numpyQ_ndarray numpyQ_concatenate(numpyQ_Primitive wit, B_list as);
numpyQ_ndarray numpyQ_zeros(numpyQ_Primitive wit, B_int n);

// Various utilities /////////////////////////////////////////////////////

numpyQ_ndarray numpyQ_sum(numpyQ_Primitive wit, numpyQ_ndarray a, B_int axis);
numpyQ_ndarray numpyQ_partition(numpyQ_Primitive wit, numpyQ_ndarray a, B_int k);
numpyQ_ndarray numpyQ_sort(numpyQ_Primitive wit, numpyQ_ndarray a, B_int axis);
numpyQ_ndarray numpyQ_clip(numpyQ_Primitive wit, numpyQ_ndarray a, $WORD low, $WORD high);
numpyQ_ndarray numpyQ_dot(numpyQ_Primitive wit, numpyQ_ndarray a, numpyQ_ndarray b);
numpyQ_ndarray numpyQ_abs(numpyQ_Primitive wit, numpyQ_ndarray a);
numpyQ_ndarray numpyQ_mean(numpyQ_Primitive wit, numpyQ_ndarray a, B_int axis);
$WORD numpyQ_scalar(numpyQ_Primitive wit, numpyQ_ndarray a);

// newaxis //////////////////////////////////////////////////////////

extern B_int numpy$G_newaxis;

struct numpyQ_IntegralD_ndarrayD_int;
typedef struct numpyQ_IntegralD_ndarrayD_int *numpyQ_IntegralD_ndarrayD_int;

struct numpyQ_IntegralD_ndarrayD_intG_class;
typedef struct numpyQ_IntegralD_ndarrayD_intG_class *numpyQ_IntegralD_ndarrayD_intG_class;

struct numpyQ_LogicalD_ndarrayD_int;
typedef struct numpyQ_LogicalD_ndarrayD_int *numpyQ_LogicalD_ndarrayD_int;

struct numpyQ_LogicalD_ndarrayD_intG_class;
typedef struct numpyQ_LogicalD_ndarrayD_intG_class *numpyQ_LogicalD_ndarrayD_intG_class;

struct numpyQ_MinusD_ndarrayD_int;
typedef struct numpyQ_MinusD_ndarrayD_int *numpyQ_MinusD_ndarrayD_int;

struct numpyQ_MinusD_ndarrayD_intG_class;
typedef struct numpyQ_MinusD_ndarrayD_intG_class *numpyQ_MinusD_ndarrayD_intG_class;

struct numpyQ_RealD_ndarray;
typedef struct numpyQ_RealD_ndarray *numpyQ_RealD_ndarray;

struct numpyQ_RealD_ndarrayG_class;
typedef struct numpyQ_RealD_ndarrayG_class *numpyQ_RealD_ndarrayG_class;

struct numpyQ_MinusD_ndarray;
typedef struct numpyQ_MinusD_ndarray *numpyQ_MinusD_ndarray;

struct numpyQ_MinusD_ndarrayG_class;
typedef struct numpyQ_MinusD_ndarrayG_class *numpyQ_MinusD_ndarrayG_class;

struct numpyQ_DivD_ndarrayD_int;
typedef struct numpyQ_DivD_ndarrayD_int *numpyQ_DivD_ndarrayD_int;

struct numpyQ_DivD_ndarrayD_intG_class;
typedef struct numpyQ_DivD_ndarrayD_intG_class *numpyQ_DivD_ndarrayD_intG_class;

struct numpyQ_DivD_ndarrayD_float;
typedef struct numpyQ_DivD_ndarrayD_float *numpyQ_DivD_ndarrayD_float;

struct numpyQ_DivD_ndarrayD_floatG_class;
typedef struct numpyQ_DivD_ndarrayD_floatG_class *numpyQ_DivD_ndarrayD_floatG_class;

struct numpyQ_CollectionD_ndarray;
typedef struct numpyQ_CollectionD_ndarray *numpyQ_CollectionD_ndarray;

struct numpyQ_CollectionD_ndarrayG_class;
typedef struct numpyQ_CollectionD_ndarrayG_class *numpyQ_CollectionD_ndarrayG_class;

struct numpyQ_SliceableD_ndarray;
typedef struct numpyQ_SliceableD_ndarray *numpyQ_SliceableD_ndarray;

struct numpyQ_SliceableD_ndarrayG_class;
typedef struct numpyQ_SliceableD_ndarrayG_class *numpyQ_SliceableD_ndarrayG_class;

// numpyQ_IntegralD_ndarrayD_int ////////////////////////////////////////////////////////////

struct numpyQ_IntegralD_ndarrayD_int {
    numpyQ_IntegralD_ndarrayD_intG_class $class;
    B_Logical W_Logical;
    B_Minus W_Minus;
};
extern GC_word numpyQ_IntegralD_ndarrayD_intD_gcbm[GC_BITMAP_SIZE(struct numpyQ_IntegralD_ndarrayD_int)];

struct numpyQ_IntegralD_ndarrayD_intG_class {
    GC_descr $GCdescr;
    char *$name;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__)(numpyQ_IntegralD_ndarrayD_int);
    void (*__serialize__)(numpyQ_IntegralD_ndarrayD_int,$Serial$state);
    numpyQ_IntegralD_ndarrayD_int (*__deserialize__)(numpyQ_IntegralD_ndarrayD_int,$Serial$state);
    B_bool (*__bool__)(numpyQ_IntegralD_ndarrayD_int);
    B_str (*__str__)(numpyQ_IntegralD_ndarrayD_int);
    B_str (*__repr__)(numpyQ_IntegralD_ndarrayD_int);
    numpyQ_ndarray (*__add__)(numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray, numpyQ_ndarray);
    numpyQ_ndarray (*__iadd__)(numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray, numpyQ_ndarray);
    numpyQ_ndarray (*__mul__)(numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray, numpyQ_ndarray);
    numpyQ_ndarray (*__imul__)(numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray, numpyQ_ndarray);
    numpyQ_ndarray (*__fromatom__)(numpyQ_IntegralD_ndarrayD_int,B_atom);
    B_complex (*__complx__)(numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray);
    numpyQ_ndarray (*__pow__)(numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray, numpyQ_ndarray);
    numpyQ_ndarray (*__ipow__)(numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray, numpyQ_ndarray);
    numpyQ_ndarray (*__neg__)(numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray);
    numpyQ_ndarray (*__pos__)(numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray);
    $WORD (*real)(numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray, B_Real);
    $WORD (*imag)(numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray, B_Real);
    $WORD (*__abs__)(numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray, B_Real);
    numpyQ_ndarray (*conjugate)(numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray);
    B_float (*__float__)(numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray);
    $WORD (*__trunc__)(numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray, B_Integral);
    $WORD (*__floor__)(numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray, B_Integral);
    $WORD (*__ceil__)(numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray, B_Integral);
    numpyQ_ndarray (*__round__)(numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray, numpyQ_ndarray);
    $WORD (*numerator)(numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray, B_Integral);
    $WORD (*denominator)(numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray, B_Integral);
    numpyQ_ndarray (*__int__)(numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray);
    numpyQ_ndarray (*__index__)(numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray);
    B_tuple (*__divmod__)(numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray, numpyQ_ndarray);
    numpyQ_ndarray (*__floordiv__)(numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray, numpyQ_ndarray);
    numpyQ_ndarray (*__mod__)(numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray, numpyQ_ndarray);
    numpyQ_ndarray (*__lshift__)(numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray, numpyQ_ndarray);
    numpyQ_ndarray (*__rshift__)(numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray, numpyQ_ndarray);
    numpyQ_ndarray (*__ifloordiv__)(numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray, numpyQ_ndarray);
    numpyQ_ndarray (*__imod__)(numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray, numpyQ_ndarray);
    numpyQ_ndarray (*__ilshift__)(numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray, numpyQ_ndarray);
    numpyQ_ndarray (*__irshift__)(numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray, numpyQ_ndarray);
    numpyQ_ndarray (*__invert__)(numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray);
};

B_NoneType numpyQ_IntegralD_ndarrayD_intD___init__ (numpyQ_IntegralD_ndarrayD_int);
void numpyQ_IntegralD_ndarrayD_intD___serialize__(numpyQ_IntegralD_ndarrayD_int,$Serial$state);
numpyQ_IntegralD_ndarrayD_int numpyQ_IntegralD_ndarrayD_intD___deserialize__(numpyQ_IntegralD_ndarrayD_int,$Serial$state);
numpyQ_ndarray numpyQ_IntegralD_ndarrayD_intD___add__(numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray, numpyQ_ndarray);
numpyQ_ndarray numpyQ_IntegralD_ndarrayD_intD___iadd__(numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray, numpyQ_ndarray);
numpyQ_ndarray numpyQ_IntegralD_ndarrayD_intD___fromatom__(numpyQ_IntegralD_ndarrayD_int,B_atom);
B_complex numpyQ_IntegralD_ndarrayD_intD___complx__(numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray);
numpyQ_ndarray numpyQ_IntegralD_ndarrayD_intD___mul__(numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray, numpyQ_ndarray);
numpyQ_ndarray numpyQ_IntegralD_ndarrayD_intD___pow__(numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray, numpyQ_ndarray);
numpyQ_ndarray numpyQ_IntegralD_ndarrayD_intD___neg__(numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray);
numpyQ_ndarray numpyQ_IntegralD_ndarrayD_intD___pos__(numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray);
$WORD numpyQ_IntegralD_ndarrayD_int$real(numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray, B_Real);
$WORD numpyQ_IntegralD_ndarrayD_int$imag(numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray, B_Real);
$WORD numpyQ_IntegralD_ndarrayD_intD___abs__(numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray, B_Real);
numpyQ_ndarray numpyQ_IntegralD_ndarrayD_int$conjugate(numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray);
B_float numpyQ_IntegralD_ndarrayD_intD___float__ (numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray);
$WORD numpyQ_IntegralD_ndarrayD_intD___trunc__ (numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray, B_Integral);
$WORD numpyQ_IntegralD_ndarrayD_intD___floor__ (numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray, B_Integral);
$WORD numpyQ_IntegralD_ndarrayD_intD___ceil__ (numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray, B_Integral);
numpyQ_ndarray numpyQ_IntegralD_ndarrayD_intD___round__ (numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray, numpyQ_ndarray);
$WORD numpyQ_IntegralD_ndarrayD_int$numerator (numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray, B_Integral);
$WORD numpyQ_IntegralD_ndarrayD_int$denominator (numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray, B_Integral);
numpyQ_ndarray numpyQ_IntegralD_ndarrayD_intD___int__ (numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray);
numpyQ_ndarray numpyQ_IntegralD_ndarrayD_intD___index__ (numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray);
B_tuple numpyQ_IntegralD_ndarrayD_intD___divmod__ (numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray, numpyQ_ndarray);
numpyQ_ndarray numpyQ_IntegralD_ndarrayD_intD___floordiv__ (numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray, numpyQ_ndarray);
numpyQ_ndarray numpyQ_IntegralD_ndarrayD_intD___mod__ (numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray, numpyQ_ndarray);
numpyQ_ndarray numpyQ_IntegralD_ndarrayD_intD___lshift__ (numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray, numpyQ_ndarray);
numpyQ_ndarray numpyQ_IntegralD_ndarrayD_intD___rshift__ (numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray, numpyQ_ndarray);
numpyQ_ndarray numpyQ_IntegralD_ndarrayD_intD___invert__ (numpyQ_IntegralD_ndarrayD_int, numpyQ_ndarray);

// numpyQ_LogicalD_ndarrayD_int ////////////////////////////////////////////////////////////

struct numpyQ_LogicalD_ndarrayD_int {
    numpyQ_LogicalD_ndarrayD_intG_class $class;
    B_Integral W_Integral;
};
extern GC_word numpyQ_LogicalD_ndarrayD_intD_gcbm[GC_BITMAP_SIZE(struct numpyQ_LogicalD_ndarrayD_int)];

struct numpyQ_LogicalD_ndarrayD_intG_class {
    GC_descr $GCdescr;
    char *$name;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__)(numpyQ_LogicalD_ndarrayD_int, B_Integral);
    void (*__serialize__)(numpyQ_LogicalD_ndarrayD_int,$Serial$state);
    numpyQ_LogicalD_ndarrayD_int (*__deserialize__)(numpyQ_LogicalD_ndarrayD_int,$Serial$state);
    B_bool (*__bool__)(numpyQ_LogicalD_ndarrayD_int);
    B_str (*__str__)(numpyQ_LogicalD_ndarrayD_int);
    B_str (*__repr__)(numpyQ_LogicalD_ndarrayD_int);
    numpyQ_ndarray (*__and__)(numpyQ_LogicalD_ndarrayD_int, numpyQ_ndarray, numpyQ_ndarray);
    numpyQ_ndarray (*__or__)(numpyQ_LogicalD_ndarrayD_int, numpyQ_ndarray, numpyQ_ndarray);
    numpyQ_ndarray (*__xor__)(numpyQ_LogicalD_ndarrayD_int, numpyQ_ndarray, numpyQ_ndarray);
    numpyQ_ndarray (*__iand__)(numpyQ_LogicalD_ndarrayD_int, numpyQ_ndarray, numpyQ_ndarray);
    numpyQ_ndarray (*__ior__)(numpyQ_LogicalD_ndarrayD_int, numpyQ_ndarray, numpyQ_ndarray);
    numpyQ_ndarray (*__ixor__)(numpyQ_LogicalD_ndarrayD_int, numpyQ_ndarray, numpyQ_ndarray);
};

B_NoneType numpyQ_LogicalD_ndarrayD_intD___init__ (numpyQ_LogicalD_ndarrayD_int, B_Integral);
void numpyQ_LogicalD_ndarrayD_intD___serialize__(numpyQ_LogicalD_ndarrayD_int,$Serial$state);
numpyQ_LogicalD_ndarrayD_int numpyQ_LogicalD_ndarrayD_intD___deserialize__(numpyQ_LogicalD_ndarrayD_int,$Serial$state);
numpyQ_ndarray numpyQ_LogicalD_ndarrayD_intD___and__ (numpyQ_LogicalD_ndarrayD_int, numpyQ_ndarray, numpyQ_ndarray);
numpyQ_ndarray numpyQ_LogicalD_ndarrayD_intD___or__ (numpyQ_LogicalD_ndarrayD_int, numpyQ_ndarray, numpyQ_ndarray);
numpyQ_ndarray numpyQ_LogicalD_ndarrayD_intD___xor__ (numpyQ_LogicalD_ndarrayD_int, numpyQ_ndarray, numpyQ_ndarray);

// numpyQ_MinusD_ndarrayD_int ////////////////////////////////////////////////////////////

struct numpyQ_MinusD_ndarrayD_int {
    numpyQ_MinusD_ndarrayD_intG_class $class;
    B_Integral W_Integral;
};
extern GC_word numpyQ_MinusD_ndarrayD_intD_gcbm[GC_BITMAP_SIZE(struct numpyQ_MinusD_ndarrayD_int)];

struct numpyQ_MinusD_ndarrayD_intG_class {
    GC_descr $GCdescr;
    char *$name;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__)(numpyQ_MinusD_ndarrayD_int, B_Integral);
    void (*__serialize__)(numpyQ_MinusD_ndarrayD_int,$Serial$state);
    numpyQ_MinusD_ndarrayD_int (*__deserialize__)(numpyQ_MinusD_ndarrayD_int,$Serial$state);
    B_bool (*__bool__)(numpyQ_MinusD_ndarrayD_int);
    B_str (*__str__)(numpyQ_MinusD_ndarrayD_int);
    B_str (*__repr__)(numpyQ_MinusD_ndarrayD_int);
    numpyQ_ndarray (*__sub__)(numpyQ_MinusD_ndarrayD_int, numpyQ_ndarray, numpyQ_ndarray);
    numpyQ_ndarray (*__isub__)(numpyQ_MinusD_ndarrayD_int, numpyQ_ndarray, numpyQ_ndarray);
};

B_NoneType numpyQ_MinusD_ndarrayD_intD___init__ (numpyQ_MinusD_ndarrayD_int, B_Integral);
void numpyQ_MinusD_ndarrayD_intD___serialize__(numpyQ_MinusD_ndarrayD_int,$Serial$state);
numpyQ_MinusD_ndarrayD_int numpyQ_MinusD_ndarrayD_intD___deserialize__(numpyQ_MinusD_ndarrayD_int,$Serial$state);
numpyQ_ndarray numpyQ_MinusD_ndarrayD_intD___sub__ (numpyQ_MinusD_ndarrayD_int, numpyQ_ndarray, numpyQ_ndarray);

// numpyQ_RealD_ndarray ////////////////////////////////////////////////////////////

struct numpyQ_RealD_ndarray {
    numpyQ_RealD_ndarrayG_class $class;
    B_Minus W_Minus;
    numpyQ_Primitive W_PrimitiveD_AD_RealD_ndarray;
};
extern GC_word numpyQ_RealD_ndarrayD_gcbm[GC_BITMAP_SIZE(struct numpyQ_RealD_ndarray)];

struct numpyQ_RealD_ndarrayG_class {
    GC_descr $GCdescr;
    char *$name;
    int $class_id;
    $SuperG_class $superclass;
  B_NoneType (*__init__)(numpyQ_RealD_ndarray, numpyQ_Primitive);
    void (*__serialize__)(numpyQ_RealD_ndarray,$Serial$state);
    numpyQ_RealD_ndarray (*__deserialize__)(numpyQ_RealD_ndarray,$Serial$state);
    B_bool (*__bool__)(numpyQ_RealD_ndarray);
    B_str (*__str__)(numpyQ_RealD_ndarray);
    B_str (*__repr__)(numpyQ_RealD_ndarray);
    numpyQ_ndarray (*__add__)(numpyQ_RealD_ndarray, numpyQ_ndarray, numpyQ_ndarray);
    numpyQ_ndarray (*__iadd__)(numpyQ_RealD_ndarray, numpyQ_ndarray, numpyQ_ndarray);
    numpyQ_ndarray (*__mul__)(numpyQ_RealD_ndarray, numpyQ_ndarray, numpyQ_ndarray);
    numpyQ_ndarray (*__imul__)(numpyQ_RealD_ndarray, numpyQ_ndarray, numpyQ_ndarray);
    numpyQ_ndarray (*__fromatom__)(numpyQ_RealD_ndarray,B_atom);
    B_complex (*__complx__)(numpyQ_RealD_ndarray, numpyQ_ndarray);
    numpyQ_ndarray (*__pow__)(numpyQ_RealD_ndarray, numpyQ_ndarray, numpyQ_ndarray);
    numpyQ_ndarray (*__ipow__)(numpyQ_RealD_ndarray, numpyQ_ndarray, numpyQ_ndarray);
    numpyQ_ndarray (*__neg__)(numpyQ_RealD_ndarray, numpyQ_ndarray);
    numpyQ_ndarray (*__pos__)(numpyQ_RealD_ndarray, numpyQ_ndarray);
    $WORD (*real)(numpyQ_RealD_ndarray, numpyQ_ndarray, B_Real);
    $WORD (*imag)(numpyQ_RealD_ndarray, numpyQ_ndarray, B_Real);
    $WORD (*__abs__)(numpyQ_RealD_ndarray, numpyQ_ndarray, B_Real);
    numpyQ_ndarray (*conjugate)(numpyQ_RealD_ndarray, numpyQ_ndarray);
    B_float (*__float__)(numpyQ_RealD_ndarray, numpyQ_ndarray);
    $WORD (*__trunc__)(numpyQ_RealD_ndarray, numpyQ_ndarray, B_Integral);
    $WORD (*__floor__)(numpyQ_RealD_ndarray, numpyQ_ndarray, B_Integral);
    $WORD (*__ceil__)(numpyQ_RealD_ndarray, numpyQ_ndarray, B_Integral);
    numpyQ_ndarray (*__round__)(numpyQ_RealD_ndarray, numpyQ_ndarray, numpyQ_ndarray);
};

B_NoneType numpyQ_RealD_ndarrayD___init__ (numpyQ_RealD_ndarray,numpyQ_Primitive);
void numpyQ_RealD_ndarrayD___serialize__(numpyQ_RealD_ndarray,$Serial$state);
numpyQ_RealD_ndarray numpyQ_RealD_ndarrayD___deserialize__(numpyQ_RealD_ndarray,$Serial$state);
numpyQ_ndarray numpyQ_RealD_ndarrayD___add__(numpyQ_RealD_ndarray, numpyQ_ndarray, numpyQ_ndarray);
numpyQ_ndarray numpyQ_RealD_ndarrayD___iadd__(numpyQ_RealD_ndarray, numpyQ_ndarray, numpyQ_ndarray);
numpyQ_ndarray numpyQ_RealD_ndarrayD___fromatom__(numpyQ_RealD_ndarray,B_atom);
B_complex numpyQ_RealD_ndarrayD___complx__(numpyQ_RealD_ndarray, numpyQ_ndarray);
numpyQ_ndarray numpyQ_RealD_ndarrayD___mul__(numpyQ_RealD_ndarray, numpyQ_ndarray, numpyQ_ndarray);
numpyQ_ndarray numpyQ_RealD_ndarrayD___pow__(numpyQ_RealD_ndarray, numpyQ_ndarray, numpyQ_ndarray);
numpyQ_ndarray numpyQ_RealD_ndarrayD___neg__(numpyQ_RealD_ndarray, numpyQ_ndarray);
numpyQ_ndarray numpyQ_RealD_ndarrayD___pos__(numpyQ_RealD_ndarray, numpyQ_ndarray);
$WORD numpyQ_RealD_ndarray$real(numpyQ_RealD_ndarray, numpyQ_ndarray, B_Real);
$WORD numpyQ_RealD_ndarray$imag(numpyQ_RealD_ndarray, numpyQ_ndarray, B_Real);
$WORD numpyQ_RealD_ndarrayD___abs__(numpyQ_RealD_ndarray, numpyQ_ndarray, B_Real);
numpyQ_ndarray numpyQ_RealD_ndarray$conjugate(numpyQ_RealD_ndarray, numpyQ_ndarray);
B_float numpyQ_RealD_ndarrayD___float__ (numpyQ_RealD_ndarray, numpyQ_ndarray);
$WORD numpyQ_RealD_ndarrayD___trunc__ (numpyQ_RealD_ndarray, numpyQ_ndarray, B_Integral);
$WORD numpyQ_RealD_ndarrayD___floor__ (numpyQ_RealD_ndarray, numpyQ_ndarray, B_Integral);
$WORD numpyQ_RealD_ndarrayD___ceil__ (numpyQ_RealD_ndarray, numpyQ_ndarray, B_Integral);
numpyQ_ndarray numpyQ_RealD_ndarrayD___round__ (numpyQ_RealD_ndarray, numpyQ_ndarray, numpyQ_ndarray);

// numpyQ_MinusD_ndarray ////////////////////////////////////////////////////////////

struct numpyQ_MinusD_ndarray {
    numpyQ_MinusD_ndarrayG_class $class;
    B_Real W_Real;
};
extern GC_word numpyQ_MinusD_ndarrayD_gcbm[GC_BITMAP_SIZE(struct numpyQ_MinusD_ndarray)];

struct numpyQ_MinusD_ndarrayG_class {
    GC_descr $GCdescr;
    char *$name;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__)(numpyQ_MinusD_ndarray, B_Real);
    void (*__serialize__)(numpyQ_MinusD_ndarray,$Serial$state);
    numpyQ_MinusD_ndarray (*__deserialize__)(numpyQ_MinusD_ndarray,$Serial$state);
    B_bool (*__bool__)(numpyQ_MinusD_ndarray);
    B_str (*__str__)(numpyQ_MinusD_ndarray);
    B_str (*__repr__)(numpyQ_MinusD_ndarray);
    numpyQ_ndarray (*__sub__)(numpyQ_MinusD_ndarray, numpyQ_ndarray, numpyQ_ndarray);
    numpyQ_ndarray (*__isub__)(numpyQ_MinusD_ndarray, numpyQ_ndarray, numpyQ_ndarray);
};

B_NoneType numpyQ_MinusD_ndarrayD___init__ (numpyQ_MinusD_ndarray, B_Real);
void numpyQ_MinusD_ndarrayD___serialize__(numpyQ_MinusD_ndarray,$Serial$state);
numpyQ_MinusD_ndarray numpyQ_MinusD_ndarrayD___deserialize__(numpyQ_MinusD_ndarray,$Serial$state);
numpyQ_ndarray numpyQ_MinusD_ndarrayD___sub__ (numpyQ_MinusD_ndarray, numpyQ_ndarray, numpyQ_ndarray);

// numpyQ_DivD_ndarrayD_int ////////////////////////////////////////////////////////////

struct numpyQ_DivD_ndarrayD_int {
    numpyQ_DivD_ndarrayD_intG_class $class;
    B_Real W_Real;
};
extern GC_word numpyQ_DivD_ndarrayD_intD_gcbm[GC_BITMAP_SIZE(struct numpyQ_DivD_ndarrayD_int)];

struct numpyQ_DivD_ndarrayD_intG_class {
    GC_descr $GCdescr;
    char *$name;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__)(numpyQ_DivD_ndarrayD_int);
    void (*__serialize__)(numpyQ_DivD_ndarrayD_int,$Serial$state);
    numpyQ_DivD_ndarrayD_int (*__deserialize__)(numpyQ_DivD_ndarrayD_int,$Serial$state);
    B_bool (*__bool__)(numpyQ_DivD_ndarrayD_int);
    B_str (*__str__)(numpyQ_DivD_ndarrayD_int);
    B_str (*__repr__)(numpyQ_DivD_ndarrayD_int);
    numpyQ_ndarray (*__truediv__)(numpyQ_DivD_ndarrayD_int, numpyQ_ndarray, numpyQ_ndarray);
    numpyQ_ndarray (*__itruediv__)(numpyQ_DivD_ndarrayD_int, numpyQ_ndarray, numpyQ_ndarray);
};

B_NoneType numpyQ_DivD_ndarrayD_intD___init__ (numpyQ_DivD_ndarrayD_int);
void numpyQ_DivD_ndarrayD_intD___serialize__(numpyQ_DivD_ndarrayD_int,$Serial$state);
numpyQ_DivD_ndarrayD_int numpyQ_DivD_ndarrayD_intD___deserialize__(numpyQ_DivD_ndarrayD_int,$Serial$state);
numpyQ_ndarray numpyQ_DivD_ndarrayD_intD___truediv__ (numpyQ_DivD_ndarrayD_int, numpyQ_ndarray, numpyQ_ndarray);

// numpyQ_DivD_ndarrayD_float ////////////////////////////////////////////////////////////

struct numpyQ_DivD_ndarrayD_float {
    numpyQ_DivD_ndarrayD_floatG_class $class;
    B_Real W_Real;
};
extern GC_word numpyQ_DivD_ndarrayD_floatD_gcbm[GC_BITMAP_SIZE(struct numpyQ_DivD_ndarrayD_float)];

struct numpyQ_DivD_ndarrayD_floatG_class {
    GC_descr $GCdescr;
    char *$name;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__)(numpyQ_DivD_ndarrayD_float);
    void (*__serialize__)(numpyQ_DivD_ndarrayD_float,$Serial$state);
    numpyQ_DivD_ndarrayD_float (*__deserialize__)(numpyQ_DivD_ndarrayD_float,$Serial$state);
    B_bool (*__bool__)(numpyQ_DivD_ndarrayD_float);
    B_str (*__str__)(numpyQ_DivD_ndarrayD_float);
    B_str (*__repr__)(numpyQ_DivD_ndarrayD_float);
    numpyQ_ndarray (*__truediv__)(numpyQ_DivD_ndarrayD_float, numpyQ_ndarray, numpyQ_ndarray);
    numpyQ_ndarray (*__itruediv__)(numpyQ_DivD_ndarrayD_float, numpyQ_ndarray, numpyQ_ndarray);
};

B_NoneType numpyQ_DivD_ndarrayD_floatD___init__ (numpyQ_DivD_ndarrayD_float);
void numpyQ_DivD_ndarrayD_floatD___serialize__(numpyQ_DivD_ndarrayD_float,$Serial$state);
numpyQ_DivD_ndarrayD_float numpyQ_DivD_ndarrayD_floatD___deserialize__(numpyQ_DivD_ndarrayD_float,$Serial$state);
numpyQ_ndarray numpyQ_DivD_ndarrayD_floatD___truediv__ (numpyQ_DivD_ndarrayD_float, numpyQ_ndarray, numpyQ_ndarray);

// numpyQ_SliceableD_ndarray /////////////////////////////////////////////////////////////////

struct numpyQ_SliceableD_ndarray;
typedef struct numpyQ_SliceableD_ndarray *numpyQ_SliceableD_ndarray;

struct numpyQ_SliceableD_ndarrayG_class {
    GC_descr $GCdescr;
    char *$name;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__) (numpyQ_SliceableD_ndarray, numpyQ_Primitive);
    void (*__serialize__) (numpyQ_SliceableD_ndarray, $Serial$state);
    numpyQ_SliceableD_ndarray (*__deserialize__) (numpyQ_SliceableD_ndarray, $Serial$state);
    B_bool (*__bool__)(numpyQ_SliceableD_ndarray);
    B_str (*__str__)(numpyQ_SliceableD_ndarray);
    B_str (*__repr__)(numpyQ_SliceableD_ndarray);
    numpyQ_ndarray (*__getitem__) (numpyQ_SliceableD_ndarray, numpyQ_ndarray, B_int);
    B_NoneType (*__setitem__) (numpyQ_SliceableD_ndarray, numpyQ_ndarray, B_int, $WORD);
    B_NoneType (*__delitem__) (numpyQ_SliceableD_ndarray, numpyQ_ndarray, B_int);
    numpyQ_ndarray (*__getslice__) (numpyQ_SliceableD_ndarray, numpyQ_ndarray, B_slice);
    B_NoneType (*__setslice__) (numpyQ_SliceableD_ndarray, numpyQ_ndarray, B_Iterable, B_slice, $WORD);
    B_NoneType (*__delslice__) (numpyQ_SliceableD_ndarray, numpyQ_ndarray, B_slice);
};
struct numpyQ_SliceableD_ndarray {
    numpyQ_SliceableD_ndarrayG_class $class;
    numpyQ_Primitive pwit;
};
extern GC_word numpyQ_SliceableD_ndarrayD_gcbm[GC_BITMAP_SIZE(struct numpyQ_SliceableD_ndarray)];


// numpyQ_CollectionD_ndarray ////////////////////////////////////////////////////////////

struct numpyQ_CollectionD_ndarray {
  numpyQ_CollectionD_ndarrayG_class $class;
  numpyQ_Primitive pwit;
};
extern GC_word numpyQ_CollectionD_ndarrayD_gcbm[GC_BITMAP_SIZE(struct numpyQ_CollectionD_ndarray)];

struct numpyQ_CollectionD_ndarrayG_class {
    GC_descr $GCdescr;
    char *$name;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__)(numpyQ_CollectionD_ndarray, numpyQ_Primitive);
    void (*__serialize__)(numpyQ_CollectionD_ndarray,$Serial$state);
    numpyQ_CollectionD_ndarray (*__deserialize__)(numpyQ_CollectionD_ndarray,$Serial$state);
    B_bool (*__bool__)(numpyQ_CollectionD_ndarray);
    B_str (*__str__)(numpyQ_CollectionD_ndarray);
    B_str (*__repr__)(numpyQ_CollectionD_ndarray);
    B_Iterator (*__iter__)(numpyQ_CollectionD_ndarray, numpyQ_ndarray);
    numpyQ_ndarray (*__fromiter__)(numpyQ_CollectionD_ndarray, B_Iterable);
    B_int (*__len__)(numpyQ_CollectionD_ndarray, numpyQ_ndarray);
};

B_NoneType numpyQ_CollectionD_ndarrayD___init__ (numpyQ_CollectionD_ndarray, numpyQ_Primitive);
void numpyQ_CollectionD_ndarrayD___serialize__(numpyQ_CollectionD_ndarray,$Serial$state);
numpyQ_CollectionD_ndarray numpyQ_CollectionD_ndarrayD___deserialize__(numpyQ_CollectionD_ndarray,$Serial$state);
B_Iterator numpyQ_CollectionD_ndarrayD___iter__ (numpyQ_CollectionD_ndarray, numpyQ_ndarray);
numpyQ_ndarray numpyQ_CollectionD_ndarrayD___fromiter__(numpyQ_CollectionD_ndarray, B_Iterable);
B_int numpyQ_CollectionD_ndarrayD___len__(numpyQ_CollectionD_ndarray, numpyQ_ndarray);

// numpyQ_RealFloat$ndarray ////////////////////////////////////////////////////////

#define numpyQ_RealFloat$ndarray ((B_Real)numpyQ_RealD_ndarray)
numpyQ_RealD_ndarray numpyQ_RealFloat$ndarrayG_new(numpyQ_Primitive,B_RealFloat); // (B_Real)numpyQ_RealD_ndarrayG_new(__VA_ARGS__)

// numpyQ_RealFunsD_mathD_ndarray ////////////////////////////////////////////////////

struct numpyQ_RealFunsD_mathD_ndarray;
typedef struct numpyQ_RealFunsD_mathD_ndarray *numpyQ_RealFunsD_mathD_ndarray;
struct numpyQ_RealFunsD_mathD_ndarrayG_class {
    GC_descr $GCdescr;
    char *$name;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__) (numpyQ_RealFunsD_mathD_ndarray, numpyQ_Primitive, mathQ_RealFuns);
    B_NoneType (*__serialize__) (numpyQ_RealFunsD_mathD_ndarray, $Serial$state);
    numpyQ_RealFunsD_mathD_ndarray (*__deserialize__) (numpyQ_RealFunsD_mathD_ndarray, $Serial$state);
    B_bool (*__bool__)(numpyQ_RealFunsD_mathD_ndarray);
    B_str (*__str__)(numpyQ_RealFunsD_mathD_ndarray);
    B_str (*__repr__)(numpyQ_RealFunsD_mathD_ndarray);
    numpyQ_ndarray (*sqrt) (numpyQ_RealFunsD_mathD_ndarray, numpyQ_ndarray);
    numpyQ_ndarray (*exp) (numpyQ_RealFunsD_mathD_ndarray, numpyQ_ndarray);
    numpyQ_ndarray (*log) (numpyQ_RealFunsD_mathD_ndarray, numpyQ_ndarray);
    numpyQ_ndarray (*sin) (numpyQ_RealFunsD_mathD_ndarray, numpyQ_ndarray);
    numpyQ_ndarray (*cos) (numpyQ_RealFunsD_mathD_ndarray, numpyQ_ndarray);
    numpyQ_ndarray (*tan) (numpyQ_RealFunsD_mathD_ndarray, numpyQ_ndarray);
    numpyQ_ndarray (*asin) (numpyQ_RealFunsD_mathD_ndarray, numpyQ_ndarray);
    numpyQ_ndarray (*acos) (numpyQ_RealFunsD_mathD_ndarray, numpyQ_ndarray);
    numpyQ_ndarray (*atan) (numpyQ_RealFunsD_mathD_ndarray, numpyQ_ndarray);
    numpyQ_ndarray (*sinh) (numpyQ_RealFunsD_mathD_ndarray, numpyQ_ndarray);
    numpyQ_ndarray (*cosh) (numpyQ_RealFunsD_mathD_ndarray, numpyQ_ndarray);
    numpyQ_ndarray (*tanh) (numpyQ_RealFunsD_mathD_ndarray, numpyQ_ndarray);
    numpyQ_ndarray (*asinh) (numpyQ_RealFunsD_mathD_ndarray, numpyQ_ndarray);
    numpyQ_ndarray (*acosh) (numpyQ_RealFunsD_mathD_ndarray, numpyQ_ndarray);
    numpyQ_ndarray (*atanh) (numpyQ_RealFunsD_mathD_ndarray, numpyQ_ndarray);
};
struct numpyQ_RealFunsD_mathD_ndarray {
    struct numpyQ_RealFunsD_mathD_ndarrayG_class *$class;
    numpyQ_Primitive W_PrimitiveD_AD_RealFuns$math$ndarray;
  mathQ_RealFuns W_RealFuns$mathD_AD_RealFuns$math$ndarray;
};
extern GC_word numpyQ_RealFunsD_mathD_ndarrayD_gcbm[GC_BITMAP_SIZE(struct numpyQ_RealFunsD_mathD_ndarray)];
extern struct numpyQ_RealFunsD_mathD_ndarrayG_class numpyQ_RealFunsD_mathD_ndarrayG_methods;


// method tables /////////////////////////////////////////////////////////////////

extern struct numpyQ_IntegralD_ndarrayD_intG_class numpyQ_IntegralD_ndarrayD_intG_methods;
extern struct numpyQ_LogicalD_ndarrayD_intG_class numpyQ_LogicalD_ndarrayD_intG_methods;
extern struct numpyQ_MinusD_ndarrayD_intG_class numpyQ_MinusD_ndarrayD_intG_methods;
extern struct numpyQ_RealD_ndarrayG_class numpyQ_RealD_ndarrayG_methods;
extern struct numpyQ_MinusD_ndarrayG_class numpyQ_MinusD_ndarrayG_methods;
extern struct numpyQ_DivD_ndarrayD_intG_class numpyQ_DivD_ndarrayD_intG_methods;
extern struct numpyQ_DivD_ndarrayD_floatG_class numpyQ_DivD_ndarrayD_floatG_methods;
extern struct numpyQ_SliceableD_ndarrayG_class numpyQ_SliceableD_ndarrayG_methods;
extern struct numpyQ_CollectionD_ndarrayG_class numpyQ_CollectionD_ndarrayG_methods;

numpyQ_IntegralD_ndarrayD_int numpyQ_IntegralD_ndarrayD_intG_new();
numpyQ_LogicalD_ndarrayD_int numpyQ_LogicalD_ndarrayD_intG_new(B_Integral);
numpyQ_MinusD_ndarrayD_int numpyQ_MinusD_ndarrayD_intG_new(B_Integral);
numpyQ_RealD_ndarray numpyQ_RealD_ndarrayG_new(numpyQ_Primitive);
numpyQ_MinusD_ndarray numpyQ_MinusD_ndarrayG_new(B_Real);
numpyQ_DivD_ndarrayD_int numpyQ_DivD_ndarrayD_intG_new();
numpyQ_DivD_ndarrayD_float numpyQ_DivD_ndarrayD_floatG_new();
numpyQ_SliceableD_ndarray numpyQ_SliceableD_ndarrayG_new(numpyQ_Primitive);
numpyQ_CollectionD_ndarray numpyQ_CollectionD_ndarrayG_new(numpyQ_Primitive);
numpyQ_RealFunsD_mathD_ndarray numpyQ_RealFunsD_mathD_ndarrayG_new(numpyQ_Primitive, mathQ_RealFuns);


void numpyQ___init__();

void quickselect(union $Bytes8 *a, int left, int right, int k, bool (*lt)(union $Bytes8,union $Bytes8));
void quicksort(union $Bytes8 *a, int left, int right, bool (*lt)(union $Bytes8,union $Bytes8));
