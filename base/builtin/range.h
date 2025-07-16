
struct B_range {
    struct B_rangeG_class *$class;
    int64_t nxt;
    int64_t step;
    int64_t remaining;
    $WORD(* box)(int64_t);
};

int64_t $rangeD_U__next__(B_range);
