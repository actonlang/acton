
struct B_range {
    struct B_rangeG_class *$class;
    int64_t nxt;
    int64_t step;
    int64_t remaining;
};

void $rangeD_U_init(B_range self, int64_t start, int64_t stop, int64_t step);
B_range $rangeD_U_new(int64_t start, int64_t stop, int64_t step);
bool $rangeD_U__next_i64(B_range self, int64_t *out);
bool B_rangeD___next__(B_range self, $WORD *out);
