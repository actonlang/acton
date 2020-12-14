struct numpy$$ndarray;
typedef struct numpy$$ndarray *numpy$$ndarray;

struct numpy$$ndarray$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)(numpy$$ndarray,$WORD);
  void (*__serialize__)(numpy$$ndarray,$Serial$state); 
  numpy$$ndarray (*__deserialize__)($Serial$state);
  $bool (*__bool__)(numpy$$ndarray);
  $str (*__str__)(numpy$$ndarray);
  numpy$$ndarray (*reshape)(numpy$$ndarray,$list);
  numpy$$ndarray (*transpose)(numpy$$ndarray,$list);
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
  numpy$$Iterator$ndarray (*__deserialize__)($Serial$state);
  $bool (*__bool__)(numpy$$Iterator$ndarray);
  $str (*__str__)(numpy$$Iterator$ndarray);
  $WORD (*__next__)(numpy$$Iterator$ndarray);
};

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

numpy$$ndarray numpy$$reshape(numpy$$ndarray,$list);
numpy$$ndarray numpy$$transpose(numpy$$ndarray,$list);
numpy$$ndarray numpy$$copy(numpy$$ndarray);
numpy$$ndarray numpy$$ndarray$__ndgetslice__(numpy$$ndarray,$list);

// Functions to create ndarrays /////////////////////////////////////////

numpy$$ndarray numpy$$linspace($float a, $float b, $int n);
numpy$$ndarray numpy$$arange($int start, $int stop, $int step);
numpy$$ndarray numpy$$array(numpy$$Primitive wit, $list elems);
numpy$$ndarray numpy$$full(numpy$$Primitive wit, $list shape, $WORD val);
numpy$$ndarray numpy$$unirandint($int a, $int b, $int n);
numpy$$ndarray numpy$$unirandfloat($float a, $float b, $int n);

// Various utilities /////////////////////////////////////////////////////

numpy$$ndarray numpy$$sum(numpy$$Primitive wit, numpy$$ndarray a, $int axis);
numpy$$ndarray numpy$$partition(numpy$$Primitive wit, numpy$$ndarray a, $int k);
numpy$$ndarray numpy$$sort(numpy$$Primitive wit, numpy$$ndarray a, $int axis);
numpy$$ndarray numpy$$clip(numpy$$Primitive wit, numpy$$ndarray a, $WORD low, $WORD high);
numpy$$ndarray numpy$$dot(numpy$$Primitive wit, numpy$$ndarray a, numpy$$ndarray b);
numpy$$ndarray numpy$$abs(numpy$$Primitive wit, numpy$$ndarray a);
$WORD numpy$$scalar(numpy$$Primitive wit, numpy$$ndarray a);

// newaxis //////////////////////////////////////////////////////////

extern $int numpy$$newaxis;
