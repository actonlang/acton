#include "rts/common.h"
#include "out/types/a.h"
int aQ_done$ = 0;
void aQ___init__ () {
    if (aQ_done$) return;
    aQ_done$ = 1;
    bQ___init__();
}