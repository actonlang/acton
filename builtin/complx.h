/*
 * This file is deliberately named complx.h to avoid collisions under gcc or
 * clang. Details are murky, is it with the standard complex.h? (Path should be
 * different, no?) Björn should know the details..
 */
#include <complex.h>

struct B_complex {
    struct B_complexG_class *$class;
    complex double val;
};
extern GC_word B_complexD_gcbm[GC_BITMAP_SIZE(struct B_complex)];

B_complex toB_complex(complex double c);

