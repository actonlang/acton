#pragma once
#include "act_platform.h"
#ifdef USE_EPOLL

#include "act_io.h"

#include <sys/epoll.h>

typedef struct epoll_event event_t;
typedef struct epoll_event event_spec_t;

struct act_io_env_t {
    int epoll_fd;
};

// TODO: ...

#endif // USE_EPOLL
