#include "rts/common.h"

void bootstrap_constructionQ___ext_init__(void) {}

B_NoneType bootstrap_constructionQ_check_constructor(void) {
    // Bootstrap constructors execute before any actor processes a message.
    assert(GET_SELF() != NULL);
    assert(GET_SELF()->B_Msg == NULL);
    return B_None;
}
