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
  $ndarray (*reshape)($ndarray,$list);
  $ndarray (*transpose)($ndarray,$list);
  $ndarray (*copy)($ndarray);
  $ndarray (*__nd_getslice__)($ndarray,$list);
};

struct $ndarray {
  struct $ndarray$class *$class;
  enum ElemType elem_type;
  long ndim;
  long size;         // # of elements; equal to product of elements in shape.
  long offset;
  long elem_size;
  $list shape;
  $list strides;
  union $Bytes8 *data;
};

extern struct $ndarray$class $ndarray$methods;

// iterating over an ndarray //////////////////////////////////////////

#define MAX_NDIM 16

typedef struct $array_iterator_state {
  union $Bytes8 *current;
  long currentstride;
  long lastshapepos;
  long lastshapelength;
  long ndim1;    // ndim-1
  long shape[MAX_NDIM]; 
  long strides[MAX_NDIM];
  long jumps[MAX_NDIM];
  long index[MAX_NDIM];
} *$array_iterator_state;


typedef struct $Iterator$ndarray *$Iterator$ndarray; ;

struct $Iterator$ndarray$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($Iterator$ndarray, $Primitive, $ndarray);
  void (*__serialize__)($Iterator$ndarray,$Serial$state);
  $Iterator$ndarray (*__deserialize__)($Serial$state);
  $bool (*__bool__)($Iterator$ndarray);
  $str (*__str__)($Iterator$ndarray);
  $WORD (*__next__)($Iterator$ndarray);
};

struct $Iterator$ndarray {
  struct $Iterator$ndarray$class *$class;
  $Primitive pwit;
  $array_iterator_state it;
};

extern struct  $Iterator$ndarray$class  $Iterator$ndarray$methods;

// Intended argument to constructor

$ndarray $ndarray_fromatom($WORD a);

//$ndarray $ndarray_func(union $Bytes8(*f)(union $Bytes8),$ndarray a);
//$ndarray $ndarray_oper(union $Bytes8 (*f)(union $Bytes8, union $Bytes8), $ndarray a, $ndarray b);

// Methods in ndarray class //////////////////////////////////////////////

$ndarray $ndarray_reshape($ndarray,$list);
$ndarray $ndarray_transpose($ndarray,$list);
$ndarray $ndarray_copy($ndarray);
$ndarray $ndarray_getslice($ndarray,$list);

// Functions to create ndarrays /////////////////////////////////////////

$ndarray $ndarray_linspace($float a, $float b, $int n);
$ndarray $ndarray_arange($int start, $int stop, $int step);
$ndarray $ndarray_array($Primitive wit, $list elems);

// Various utilities /////////////////////////////////////////////////////

$ndarray $ndarray_sum($Primitive wit, $ndarray a, $int axis);
$ndarray $ndarray_partition($Primitive wit, $ndarray a, $int k);
$ndarray $ndarray_sort($Primitive wit, $ndarray a, $int axis);
$ndarray $ndarray_clip($Primitive wit, $ndarray a, $WORD low, $WORD high);
$ndarray $ndarray_dot($Primitive wit, $ndarray a, $ndarray b);
$ndarray $ndarray_abs($Primitive wit, $ndarray a);
