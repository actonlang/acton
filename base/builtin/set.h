
typedef struct {
    $WORD key;
    long hash;    
} B_setentry;

struct B_set {
    struct B_setG_class *$class;
    long numelements;    // nr of elements in B_set
    long fill;           // numelements + #dummy entries
    long mask;
    long finger;                       // Search finger for pop() 
    B_setentry *table;                  // the hashtable
};


// Iterators over sets ///////////////////////////////////////////////////////

typedef struct B_IteratorD_set *B_IteratorD_set; ;

struct B_IteratorD_setG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_IteratorD_set, B_set);
    void (*__serialize__)(B_IteratorD_set, $Serial$state);
    B_IteratorD_set (*__deserialize__)(B_IteratorD_set, $Serial$state);
    $WORD(*__next__)(B_IteratorD_set);
};

struct B_IteratorD_set {
    struct B_IteratorD_setG_class *$class;
    B_set src;
    int nxt;
};

extern struct  B_IteratorD_setG_class  B_IteratorD_setG_methods;
B_IteratorD_set B_IteratorD_setG_new(B_set);

void B_set_add_entry(B_set set, B_Hashable hashwit, $WORD key, long hash);
