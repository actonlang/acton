struct B_tupleG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__)(B_tuple,int,...);
    void (*__serialize__)(B_tuple,$Serial$state); 
    B_tuple (*__deserialize__)(B_tuple,$Serial$state);
};

struct B_tuple {
    struct B_tupleG_class *$class;
    int size;
    $WORD *components;
};

extern struct B_tupleG_class B_tupleG_methods;
B_tuple B_tupleG_new(int,...);

#define $NEWTUPLE(B_len, ...)  ({ B_tuple $t = acton_malloc(sizeof(struct B_tuple)+B_len*sizeof($WORD)); \
            $t->$class = &B_tupleG_methods;                               \
            $t->$class->__init__($t, B_len, __VA_ARGS__);                \
            $t; })

#define $NEWTUPLE0  ({ B_tuple $t = acton_malloc(sizeof(struct B_tuple));       \
            $t->$class = &B_tupleG_methods;                               \
            $t->$class->__init__($t,0);                                  \
            $t; })



// struct definitions for tuple protocol instances are not in __builtin__.h
// (They cannot be expressed in __builtin__.act for lack of syntax)

struct B_IterableD_tuple;
typedef struct B_IterableD_tuple *B_IterableD_tuple;

struct B_IterableD_tupleG_class;
typedef struct B_IterableD_tupleG_class *B_IterableD_tupleG_class;

struct B_SliceableD_tuple;
typedef struct B_SliceableD_tuple *B_SliceableD_tuple;

struct B_SliceableD_tupleG_class;
typedef struct B_SliceableD_tupleG_class *B_SliceableD_tupleG_class;

struct B_HashableD_tuple;
typedef struct B_HashableD_tuple *B_HashableD_tuple;

struct B_HashableD_tupleG_class;
typedef struct B_HashableD_tupleG_class *B_HashableD_tupleG_class;

// B_BoolD_tuple ////////////////////////////////////////////////////////////////

struct B_BoolD_tuple;
typedef struct B_BoolD_tuple *B_BoolD_tuple;

struct B_BoolD_tupleG_class;
typedef struct B_BoolD_tupleG_class *B_BoolD_tupleG_class;

extern struct B_BoolD_tupleG_class B_BoolD_tupleG_methods;
B_BoolD_tuple B_BoolD_tupleG_new();

// B_ShowD_tuple ////////////////////////////////////////////////////////////////

struct B_ShowD_tuple;
typedef struct B_ShowD_tuple *B_ShowD_tuple;

struct B_ShowD_tupleG_class;
typedef struct B_ShowD_tupleG_class *B_ShowD_tupleG_class;

extern struct B_ShowD_tupleG_class B_ShowD_tupleG_methods;
B_ShowD_tuple B_ShowD_tupleG_new();

// B_IterableD_tuple ////////////////////////////////////////////////////////////

struct B_IterableD_tuple {
    B_IterableD_tupleG_class $class;
};

struct B_IterableD_tupleG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__)(B_IterableD_tuple);
    void (*__serialize__)(B_IterableD_tuple,$Serial$state);
    B_IterableD_tuple (*__deserialize__)(B_IterableD_tuple,$Serial$state);
    B_Iterator (*__iter__)(B_IterableD_tuple, B_tuple);
};

B_NoneType B_IterableD_tupleD___init__ (B_IterableD_tuple);
void B_IterableD_tupleD___serialize__(B_IterableD_tuple, $Serial$state);
B_IterableD_tuple B_IterableD_tupleD___deserialize__(B_IterableD_tuple, $Serial$state);
B_Iterator B_IterableD_tupleD___iter__ (B_IterableD_tuple, B_tuple);

extern struct B_IterableD_tupleG_class B_IterableD_tupleG_methods;
B_IterableD_tuple B_IterableD_tupleG_new();

// B_SliceableD_tuple ////////////////////////////////////////////////////////////

// all methods except getitem and getslice will raise NotImplementedError

struct B_SliceableD_tuple {
    B_SliceableD_tupleG_class $class;
};

struct B_SliceableD_tupleG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__)(B_SliceableD_tuple);
    void (*__serialize__)(B_SliceableD_tuple,$Serial$state);
    B_SliceableD_tuple (*__deserialize__)(B_SliceableD_tuple,$Serial$state);
    $WORD (*__getitem__)(B_SliceableD_tuple, B_tuple, B_int);
    B_NoneType (*__setitem__)(B_SliceableD_tuple, B_tuple, B_int, $WORD);
    B_NoneType (*__delitem__)(B_SliceableD_tuple, B_tuple, B_int);
    B_tuple (*__getslice__)(B_SliceableD_tuple, B_tuple, B_slice);
    B_NoneType (*__setslice__)(B_SliceableD_tuple, B_tuple, B_Iterable, B_slice, $WORD);
    B_NoneType (*__delslice__)(B_SliceableD_tuple, B_tuple, B_slice);
};

B_NoneType B_SliceableD_tupleD___init__ (B_SliceableD_tuple);
void B_SliceableD_tupleD___serialize__(B_SliceableD_tuple, $Serial$state);
B_SliceableD_tuple B_SliceableD_tupleD___deserialize__(B_SliceableD_tuple, $Serial$state);
$WORD B_SliceableD_tupleD___getitem__ (B_SliceableD_tuple, B_tuple, B_int);
B_NoneType B_SliceableD_tupleD___setitem__ (B_SliceableD_tuple, B_tuple, B_int, $WORD);
B_NoneType B_SliceableD_tupleD___delitem__ (B_SliceableD_tuple, B_tuple, B_int);
B_tuple B_SliceableD_tupleD___getslice__ (B_SliceableD_tuple, B_tuple, B_slice);
B_NoneType B_SliceableD_tupleD___setslice__ (B_SliceableD_tuple, B_tuple, B_Iterable, B_slice, $WORD);
B_NoneType B_SliceableD_tupleD___delslice__ (B_SliceableD_tuple, B_tuple, B_slice);

extern struct B_SliceableD_tupleG_class B_SliceableD_tupleG_methods;
B_SliceableD_tuple B_SliceableD_tupleG_new();

// B_HashableD_tuple ////////////////////////////////////////////////////////////

struct B_HashableD_tuple {
    B_HashableD_tupleG_class $class;
    int W_HashableB_tuple$size;
    B_Hashable *W_Hashable;
};

struct B_HashableD_tupleG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__)(B_HashableD_tuple,int,B_Hashable*);
    void (*__serialize__)(B_HashableD_tuple,$Serial$state);
    B_HashableD_tuple (*__deserialize__)(B_HashableD_tuple,$Serial$state);
    B_bool (*__eq__)(B_HashableD_tuple, B_tuple, B_tuple);
    B_bool (*__ne__)(B_HashableD_tuple, B_tuple, B_tuple);
    B_int (*__hash__)(B_HashableD_tuple, B_tuple);
};
  
B_NoneType B_HashableD_tupleD___init__ (B_HashableD_tuple,int,B_Hashable*);
void B_HashableD_tupleD___serialize__(B_HashableD_tuple, $Serial$state);
B_HashableD_tuple B_HashableD_tupleD___deserialize__(B_HashableD_tuple, $Serial$state);
B_bool B_HashableD_tupleD___eq__ (B_HashableD_tuple, B_tuple, B_tuple);
B_bool B_HashableD_tupleD___ne__ (B_HashableD_tuple, B_tuple, B_tuple);
B_int B_HashableD_tupleD___hash__ (B_HashableD_tuple, B_tuple);

extern struct B_HashableD_tupleG_class B_HashableD_tupleG_methods;
B_HashableD_tuple B_HashableD_tupleG_new();

extern struct B_IterableD_tupleG_class B_IterableD_tupleG_methods;
B_IterableD_tuple B_IterableD_tupleG_new();
extern struct B_SliceableD_tupleG_class B_SliceableD_tupleG_methods;
B_SliceableD_tuple B_SliceableD_tupleG_new();
extern struct B_HashableD_tupleG_class B_HashableD_tupleG_methods;
B_HashableD_tuple B_HashableD_tupleG_new();

extern struct B_HashableD_tuple *B_HashableD_tuple_new(int,B_Hashable*);

// Iterators over tuples ///////////////////////////////////////////////////////

typedef struct B_IteratorD_tuple *B_IteratorD_tuple;

struct B_IteratorD_tupleG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__)(B_IteratorD_tuple, B_tuple);
    void (*__serialize__)(B_IteratorD_tuple,$Serial$state);
    B_IteratorD_tuple (*__deserialize__)(B_IteratorD_tuple,$Serial$state);
    $WORD(*__next__)(B_IteratorD_tuple);
};

struct B_IteratorD_tuple {
    struct B_IteratorD_tupleG_class *$class;
    B_tuple src;
    int nxt;
};

extern struct B_IteratorD_tupleG_class B_IteratorD_tupleG_methods;
B_IteratorD_tuple B_IteratorD_tupleG_new(B_tuple);
