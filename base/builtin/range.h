
struct B_range {
    struct B_rangeG_class *$class;
    long start;
    long stop;
    long step;
};

// Iterators over ranges ///////////////////////////////////////////////////////

typedef struct B_IteratorB_range *B_IteratorB_range;

struct B_IteratorB_rangeG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_IteratorB_range, B_range);
    void (*__serialize__)(B_IteratorB_range,$Serial$state);
    B_IteratorB_range (*__deserialize__)(B_IteratorB_range,$Serial$state);
    $WORD(*__next__)(B_IteratorB_range);
};

struct B_IteratorB_range {
    struct B_IteratorB_rangeG_class *$class;
    long nxt;
    long step;
    long remaining;
};

extern struct B_IteratorB_rangeG_class  B_IteratorB_rangeG_methods;
B_IteratorB_range B_IteratorB_rangeG_new(B_range);
