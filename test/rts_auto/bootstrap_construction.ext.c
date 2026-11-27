#include "rts/common.h"

void bootstrap_constructionQ___ext_init__(void) {}

B_NoneType bootstrap_constructionQ_check_constructor(void) {
    // Bootstrap constructors execute before any actor processes a message.
    assert(GET_SELF() != NULL);
    assert(GET_SELF()->B_Msg == NULL);
    return B_None;
}

B_NoneType bootstrap_constructionQ_check_base_initializer(void) {
    B_Actor actor = GET_SELF();
    B_Msg message = actor->B_Msg;
    int64_t key = actor->$globkey;
    int64_t affinity = actor->$affinity;
    assert(message != NULL);
    B_ActorD___init__(actor);
    assert(actor->B_Msg == message);
    assert(actor->$globkey == key);
    assert(actor->$affinity == affinity);
    return B_None;
}
