#include <stdlib.h>

void type_test_aliasingQ___ext_init__(void) {
}

static B_int fresh_box(int64_t value) {
    B_int boxed = malloc(sizeof(*boxed));
    boxed->$class = &B_intG_methods;
    boxed->val = value;
    return boxed;
}

bool type_test_aliasingQ_fresh_box_is_int(int64_t n) {
    // Keep allocation, concrete initialization and the generic read visible
    // together even on platforms where the build does not enable LTO.
    B_int boxed = fresh_box(n);
    bool result = $ISINSTANCE0(boxed, B_int);
    free(boxed);
    return result;
}
