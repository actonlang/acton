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
    $WORD components[];
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

// Component formatting uses the Show evidence carried by the tuple witness.
B_str B_tuple_str(B_tuple self, B_tuple witnesses);
bool B_tuple_bool(B_tuple self);
