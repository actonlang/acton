#include "rts/common.h"
#include "out/types/b.h"
B_NoneType bQ_foo () {
    ((B_NoneType (*) (B_tuple, B_str, B_str, B_bool, B_bool))B_print)($NEWTUPLE(1, to$str("b")), B_None, B_None, B_None, B_None);
    return B_None;
}
int bQ_done$ = 0;
void bQ___init__ () {
    if (bQ_done$) return;
    bQ_done$ = 1;
}