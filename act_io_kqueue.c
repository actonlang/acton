#include "act_platform.h"
#ifdef USE_KQUEUE
#include "act_io_kqueue.h"

act_io_env_t* act_io_env_create() {
    return NULL;
}

void act_io_env_release(
    act_io_env_t* env) {
}

#else
typedef int iso_c_forbids_an_empty_translation_unit;
#endif // USE_KQUEUE