#pragma once

#include "platform.h"

#include <stdint.h>
#include <unistd.h>

#include <netinet/in.h>

#ifdef USE_EPOLL
    #include <sys/epoll.h>
#endif
#ifdef USE_KQUEUE
    #include <sys/event.h>
#endif
#include <sys/socket.h>
#include <sys/types.h>

#define CORE_MAX_EVENTS 1
#define LISTEN_BACKLOG_DEFAULT 65536
#define ECHO_BUFFER_SIZE 65536

// typedef enum {
//     LISTEN,
//     CONNECTION,
// } TASK_TYPE;

#if defined(USE_EPOLL)
typedef struct epoll_event event_t;
typedef struct epoll_event event_spec_t;
#elif defined(USE_KQUEUE)
typedef struct kevent event_t;
typedef struct kevent event_spec_t;
#endif

typedef struct task_t task_t;
typedef struct core_loop_data_t core_loop_data_t;

typedef struct {
    size_t begin;
    size_t end;
    uint8_t buffer[ECHO_BUFFER_SIZE];
} socket_task_data_buffer_t;

typedef struct {
    event_spec_t event_spec;
    int fd;
    struct sockaddr_in sock_addr;
    socket_task_data_buffer_t* data;
} socket_task_data_t;

typedef struct task_t {
    // TASK_TYPE type;
    void (*handler)(core_loop_data_t*, event_t*, task_t*);
    union {
        socket_task_data_t* socket;
    };
} task_t;

#define CUCKOO_TABLE_NAME fd_task_map
#define CUCKOO_KEY_TYPE int
#define CUCKOO_MAPPED_TYPE task_t*
#include "deps/libcuckoo/libcuckoo-c/cuckoo_table_template.h"
#undef CUCKOO_TABLE_NAME
#undef CUCKOO_KEY_TYPE
#undef CUCKOO_MAPPED_TYPE

typedef struct {
    int selector_fd;
    fd_task_map* fd_tasks;
} core_shared_data_t;

typedef struct core_loop_data_t {
    int id;
    event_t in_events[CORE_MAX_EVENTS];
    pthread_t thread;
    core_shared_data_t* shared;
} core_loop_data_t;
