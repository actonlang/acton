
struct B_range {
    struct B_rangeG_class *$class;
    long start;
    long stop;
    long step;
    $WORD(* box)(long);
};

// Iterators over ranges ///////////////////////////////////////////////////////

typedef struct B_IteratorD_range *B_IteratorD_range;

struct B_IteratorD_rangeG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_IteratorD_range, B_range);
    void (*__serialize__)(B_IteratorD_range,$Serial$state);
    B_IteratorD_range (*__deserialize__)(B_IteratorD_range,$Serial$state);
    B_bool (*__bool__)(B_IteratorD_range);
    B_str (*__str__)(B_IteratorD_range);
    B_str (*__repr__)(B_IteratorD_range);
    $WORD(*__next__)(B_IteratorD_range);
};

struct B_IteratorD_range {
    struct B_IteratorD_rangeG_class *$class;
    long nxt;
    long step;
    long remaining;
    $WORD(* box)(long);
};

extern struct B_IteratorD_rangeG_class  B_IteratorD_rangeG_methods;
B_IteratorD_range B_IteratorD_rangeG_new(B_range);
