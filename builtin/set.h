struct B_setG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_set, B_Hashable, B_Iterable, $WORD);
    void (*__serialize__)(B_set, $NoneType);
    B_set (*__deserialize__)(B_set, $NoneType);
    B_bool (*__bool__)(B_set);
    B_str (*__str__)(B_set);
    B_str (*__repr__)(B_set);
    B_set(*copy)(B_set, B_Hashable);
};

typedef struct {
    $WORD key;
    long hash;    
} B_setentry;

typedef struct B_set {
    struct B_setG_class *$class;
    long numelements;    // nr of elements in B_set
    long fill;           // numelements + #dummy entries
    long mask;
    long finger;                       // Search finger for pop() 
    B_setentry *table;                  // the hashtable
} *B_set;


extern struct B_setG_class B_setG_methods;
B_set B_setG_new(B_Hashable, B_Iterable, $WORD);

extern struct B_SetD_setG_class B_SetD_setG_methods;
B_SetD_set B_SetD_setG_new(B_Hashable);
extern struct B_OrdD_SetD_setG_class B_OrdD_SetD_setG_methods;
//B_OrdD_SetD_set B_OrdD_SetD_setG_new(B_SetD_set);
extern struct B_MinusD_SetD_setG_class B_MinusD_SetD_setG_methods;
//B_MinusD_SetD_set B_MinusD_SetD_setG_new(B_SetD_set);
extern struct B_LogicalD_SetD_setG_class B_LogicalD_SetD_setG_methods;
//B_LogicalD_SetD_set B_LogicalD_SetD_setG_new(B_SetD_set);

extern struct B_SetD_set *B_SetD_set_new(B_Hashable);

// Iterators over sets ///////////////////////////////////////////////////////

typedef struct B_IteratorD_set *B_IteratorD_set; ;

struct B_IteratorD_setG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_IteratorD_set, B_set);
    void (*__serialize__)(B_IteratorD_set, $NoneType);
    B_IteratorD_set (*__deserialize__)(B_IteratorD_set, $NoneType);
    B_bool (*__bool__)(B_IteratorD_set);
    B_str (*__str__)(B_IteratorD_set);
    B_str (*__repr__)(B_IteratorD_set);
    $WORD(*__next__)(B_IteratorD_set);
};

struct B_IteratorD_set {
    struct B_IteratorD_setG_class *$class;
    B_set src;
    int nxt;
};

extern struct  B_IteratorD_setG_class  B_IteratorD_setG_methods;
B_IteratorD_set B_IteratorD_setG_new(B_set);
