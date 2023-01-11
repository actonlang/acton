struct B_rangeG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_range, B_int, B_int, B_int);
    void (*__serialize__)(B_range,$NoneType);
    B_range (*__deserialize__)(B_range,$NoneType);
    B_bool (*__bool__)(B_range);
    B_str (*__str__)(B_range);
    B_str (*__repr__)(B_range);
};

struct B_range {
    struct B_rangeG_class *$class;
    long start;
    long stop;
    long step;
};


extern struct B_rangeG_class B_rangeG_methods;
B_range B_rangeG_new(B_int, B_int, B_int);

extern struct B_IterableD_rangeG_class B_IterableD_rangeG_methods;
B_IterableD_range B_IterableD_rangeG_new();
extern B_IterableD_range B_IterableD_rangeG_witness;

// Iterators over ranges ///////////////////////////////////////////////////////

typedef struct B_IteratorB_range *B_IteratorB_range;

struct B_IteratorB_rangeG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_IteratorB_range, B_range);
    void (*__serialize__)(B_IteratorB_range,$NoneType);
    B_IteratorB_range (*__deserialize__)(B_IteratorB_range,$NoneType);
    B_bool (*__bool__)(B_IteratorB_range);
    B_str (*__str__)(B_IteratorB_range);
    B_str (*__repr__)(B_IteratorB_range);
    $WORD(*__next__)(B_IteratorB_range);
};

struct B_IteratorB_range {
    struct B_IteratorB_rangeG_class *$class;
    B_range src;
    int nxt;
};

extern struct B_IteratorB_rangeG_class  B_IteratorB_rangeG_methods;
B_IteratorB_range B_IteratorB_rangeG_new(B_range);
extern B_IteratorB_range B_IteratorB_rangeG_witness;
