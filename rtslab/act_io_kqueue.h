#pragma once
#include "act_platform.h"
#ifdef USE_KQUEUE

#include "act_io.h"

#include <sys/event.h>

typedef struct kevent event_t;
typedef struct kevent event_spec_t;

// TODO: ...

#endif // USE_KQUEUE
