
struct B_range {
    struct B_rangeG_class *$class;
    int64_t nxt;
    int64_t step;
    int64_t remaining;
};

int64_t $rangeD_U__next__(B_range self);
bool B_rangeD___next_maybe__(B_range self, $WORD *out);
bool $rangeD_U__next_maybe__(B_range self, int64_t *out);
