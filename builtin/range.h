
struct B_range {
    struct B_rangeG_class *$class;
    long start;
    long stop;
    long step;
};
extern GC_word B_rangeD_gcbm[GC_BITMAP_SIZE(struct B_range)];

// Iterators over ranges ///////////////////////////////////////////////////////

typedef struct B_IteratorB_range *B_IteratorB_range;

struct B_IteratorB_rangeG_class {
    GC_descr $GCdescr;
    char *$name;
    int $class_id;
    $SuperG_class $superclass;
    void (*__init__)(B_IteratorB_range, B_range);
    void (*__serialize__)(B_IteratorB_range,$Serial$state);
    B_IteratorB_range (*__deserialize__)(B_IteratorB_range,$Serial$state);
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
extern GC_word B_IteratorB_rangeD_gcbm[GC_BITMAP_SIZE(struct B_IteratorB_range)];

extern struct B_IteratorB_rangeG_class  B_IteratorB_rangeG_methods;
B_IteratorB_range B_IteratorB_rangeG_new(B_range);
