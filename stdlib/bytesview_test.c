/* bytesview_test.c */
    
#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "bytesview.h"

void report(bytesview_t *bv, char *name) {
    printf("%s.n() = %ld\n", name, act_bv_n(bv));
    printf("%s.tostr() = %s\n", name, act_bv_tostr(bv));
    printf("%s.show():\n", name);
    act_bv_show(bv);
    printf("\n");
}


int main() {

    char *by = "0123456789abcdef";

    bytesview_data_buffer_t b = { ._start = 0, ._end = 16, ._bytes = (uint8_t *) by };

    size_t l = strlen(by);
    
    bytesview_t *bv1, *bv2, *bv3, *bv4;

    size_t i;
    
    bv1 = act_bv_fromstr(by);
    report(bv1, "bv1");

    bv2 = act_bv_frombuf(&b);
    report(bv2, "bv2");

    bv3 = act_bv_append(bv2, by, l);
    report(bv3, "bv3");
    
    bv4 = act_bv_append(bv1, "x", 1);
    bv4 = act_bv_append(bv4, by, 16);
    bv4 = act_bv_append(bv4, "y", 1);
    bv4 = act_bv_append(bv4, by, 16);
    bv4 = act_bv_append(bv4, "z", 1);
    bv4 = act_bv_append(bv4, by, 16);
    bv4 = act_bv_append(bv4, "w", 1);
    report(bv4, "bv4");

    printf("consume test\n");
    printf("============\n");
    i = 0;
    while (i < 100) {
        bv3 = act_bv_consume(bv4, i);
        printf("b'%s'\n", act_bv_tostr(bv3));
        i++;
    }

    raise_IncompleteReadError("Just testing!");
    return 0;
}
