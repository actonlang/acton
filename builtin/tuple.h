struct B_tupleG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_tuple,int,...);
    void (*__serialize__)(B_tuple,$NoneType); 
    B_tuple (*__deserialize__)(B_tuple,$NoneType);
    B_bool (*__bool__)(B_tuple);
    B_str (*__str__)(B_tuple);
    B_str (*__repr__)(B_tuple);
};

struct B_tuple {
    struct B_tupleG_class *$class;
    int size;
    $WORD *components;
};

extern struct B_tupleG_class B_tupleG_methods;
B_tuple B_tupleG_new(int,...);

#define $NEWTUPLE($len, ...)  ({ B_tuple $t = malloc(sizeof(struct B_tuple)+$len*sizeof($WORD)); \
            $t->$class = &B_tupleG_methods;                               \
            $t->$class->__init__($t, $len, __VA_ARGS__);                \
            $t; })

#define $NEWTUPLE0  ({ B_tuple $t = malloc(sizeof(struct B_tuple));       \
            $t->$class = &B_tupleG_methods;                               \
            $t->$class->__init__($t);                                   \
            $t; })

extern struct B_IterableD_tupleG_class B_IterableD_tupleG_methods;
B_IterableD_tuple B_IterableD_tupleG_new();
extern struct B_SliceableD_tupleG_class B_SliceableD_tupleG_methods;
B_SliceableD_tuple B_SliceableD_tupleG_new();
extern struct B_HashableD_tupleG_class B_HashableD_tupleG_methods;
B_HashableD_tuple B_HashableD_tupleG_new();

extern struct B_IterableD_tuple *B_IterableD_tupleG_witness;
extern struct B_SliceableD_tuple *B_SliceableD_tupleG_witness;
extern struct B_HashableD_tuple *B_HashableD_tuple_new(int,B_Hashable*);

// Iterators over tuples ///////////////////////////////////////////////////////

typedef struct B_IteratorB_tuple *B_IteratorB_tuple;

struct B_IteratorB_tupleG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_IteratorB_tuple, B_tuple);
    void (*__serialize__)(B_IteratorB_tuple,$NoneType);
    B_IteratorB_tuple (*__deserialize__)(B_IteratorB_tuple,$NoneType);
    B_bool (*__bool__)(B_IteratorB_tuple);
    B_str (*__str__)(B_IteratorB_tuple);
    B_str (*__repr__)(B_IteratorB_tuple);
    $WORD(*__next__)(B_IteratorB_tuple);
};

struct B_IteratorB_tuple {
    struct B_IteratorB_tupleG_class *$class;
    B_tuple src;
    int nxt;
};

extern struct B_IteratorB_tupleG_class B_IteratorB_tupleG_methods;
B_IteratorB_tuple B_IteratorB_tupleG_new(B_tuple);
