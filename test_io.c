#define _GNU_SOURCE
#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <pthread.h>
#include <sched.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

#include "test_io.h"

#ifdef IS_GNU_LINUX
    #include <sys/sysinfo.h>
#endif

#ifdef IS_FREEBSD
    #include <pthread_np.h>
    #include <sys/cpuset.h>
    #include <sys/param.h>
#endif

int set_non_blocking(int fd) {
    int retval = fcntl(fd, F_GETFL);
    if (retval != -1) {
        retval = fcntl(fd, F_SETFL, retval | O_NONBLOCK);
    }
    return retval;
}

// Common
void remove_fd_task(core_shared_data_t* shared_data, int fd, void (*destroy_task)(core_shared_data_t* shared_data, task_t* task), task_t* task) {
    if (task == NULL) {
        fd_task_map_find(shared_data->fd_tasks, &fd, &task);
    }
    fd_task_map_erase(shared_data->fd_tasks, &fd);
    if (task != NULL) {
        destroy_task(shared_data, task);
    }
}

void destroy_socket_task(core_shared_data_t* shared_data, task_t* task) {
    if (task != NULL) {
        if (task->socket != NULL) {
#if defined(USE_EPOLL)
            epoll_ctl(shared_data->selector_fd, EPOLL_CTL_DEL, task->socket->fd, NULL);
#elif defined(USE_KQUEUE)
            task->socket->event_spec.flags = EV_DELETE;
            kevent(shared_data->selector_fd, &task->socket->event_spec, 1, NULL, 0, NULL);
#endif
            if (task->socket->fd != 0) {
                close(task->socket->fd);
            }
            if (task->socket->data != NULL) {
                free(task->socket->data);
            }
            free(task->socket);
        }
        free(task);
    }
}

// TCP connection
void destroy_tcp_connection_task(core_shared_data_t* shared_data, task_t* task);

void handle_tcp_connection_task(core_loop_data_t* core_data, event_t* event, task_t* task) {
    int fd = task->socket->fd;
    event_spec_t* event_spec = &task->socket->event_spec;
    socket_task_data_buffer_t* data = task->socket->data;
    ssize_t rxtx_count = 0;

    while (true) {
#if defined(USE_EPOLL)
        if (event_spec->events & EPOLLOUT) {
#elif defined(USE_KQUEUE)
        if (event_spec->filter == EVFILT_WRITE) {
#endif
            rxtx_count = write(fd, &data->buffer[data->begin], data->end - data->begin);
            if (rxtx_count > 0) {
                data->begin += rxtx_count;
                if (data->end == data->begin) {
                    data->begin = 0;
                    data->end = 0;
#if defined(USE_EPOLL)
                    event_spec->events |= EPOLLIN;
                    event_spec->events &= ~EPOLLOUT;
#elif defined(USE_KQUEUE)
                    event_spec->filter = EVFILT_READ;
#endif
                }
            }
        }
#if defined(USE_EPOLL)
        else if (event_spec->events & EPOLLIN) {
#elif defined(USE_KQUEUE)
        else if (event_spec->filter == EVFILT_READ) {
#endif
            rxtx_count = read(fd, &data->buffer[data->begin], ECHO_BUFFER_SIZE - (data->end - data->begin));
            if (rxtx_count > 0) {
                data->end += rxtx_count;
#if defined(USE_EPOLL)
                event_spec->events |= EPOLLOUT;
                event_spec->events &= ~EPOLLIN;
#elif defined(USE_KQUEUE)
                event_spec->filter = EVFILT_WRITE;
#endif
            }
        }
        if ((rxtx_count == 0 && errno != EINTR) || rxtx_count < 0) {
            break;
        }
    }

    if (rxtx_count < 0 && (errno == EAGAIN || errno == EWOULDBLOCK)) {
#if defined(USE_EPOLL)
        if (epoll_ctl(core_data->shared->selector_fd, EPOLL_CTL_MOD, fd, event_spec) == 0) {
            return;
        }
#elif defined(USE_KQUEUE)
        // TODO: Could use opaque user data to avoid a task map lookup
        // TODO: Test if EV_DISPATCH -> EV_ENABLE is faster than EV_ONESHOT -> EV_ADD
        if (kevent(core_data->shared->selector_fd, &task->socket->event_spec, 1, NULL, 0, NULL) == 0) {
            return;
        }
#endif
    }

    // Release connection
    remove_fd_task(core_data->shared, fd, &destroy_tcp_connection_task, task);
}

void create_tcp_connection_task(core_shared_data_t* shared_data, int fd, struct sockaddr_in* sockaddr) {
    task_t* task = malloc(sizeof(task_t));
    if (task != NULL) {
        task->handler = &handle_tcp_connection_task;
        task->socket = malloc(sizeof(*task->socket));
        if (task->socket != NULL) {
            task->socket->fd = fd;
            task->socket->sock_addr = *sockaddr;
            task->socket->data = malloc(sizeof(*task->socket->data));
            if (task->socket->data != NULL) {
                bzero(task->socket->data, sizeof(*task->socket->data));
                if (fd_task_map_insert(shared_data->fd_tasks, &task->socket->fd, &task)) {
#if defined(USE_EPOLL)
                    task->socket->event_spec.events = EPOLLIN | EPOLLONESHOT;
                    task->socket->event_spec.data.fd = task->socket->fd; // TODO: Remove duplicate
                    if (epoll_ctl(shared_data->selector_fd, EPOLL_CTL_ADD, task->socket->fd, &task->socket->event_spec) == 0) {
                        return;
                    }
#elif defined(USE_KQUEUE)
                    // TODO: Could use opaque user data to avoid a task map lookup
                    // TODO: Test if EV_DISPATCH -> EV_ENABLE is faster than EV_ONESHOT -> EV_ADD
                    EV_SET(&task->socket->event_spec, task->socket->fd, EVFILT_READ, EV_ADD | EV_ONESHOT, 0, 0, NULL);
                    if (kevent(shared_data->selector_fd, &task->socket->event_spec, 1, NULL, 0, NULL) == 0) {
                        return;
                    }
#endif
                }
            }
        }
    }

    destroy_tcp_connection_task(shared_data, task);
}

void destroy_tcp_connection_task(core_shared_data_t* shared_data, task_t* task) {
    destroy_socket_task(shared_data, task);
}

// TCP listener
void destroy_tcp_listen_task(core_shared_data_t* shared_data, task_t* task);

void handle_tcp_listen_task(core_loop_data_t* core_data, event_t* event, task_t* task) {
    struct sockaddr_in sockaddr;

    while (true) {
        socklen_t socklen = sizeof(sockaddr);
        int fd = accept(task->socket->fd, (struct sockaddr*)&sockaddr, &socklen);
        if (fd > 0) {
            set_non_blocking(fd);
            create_tcp_connection_task(core_data->shared, fd, &sockaddr);
        }
        else {
            break;
        }
    }
}

int create_tcp_listen_task(core_shared_data_t* shared_data, sa_family_t family, in_addr_t address, in_port_t port) {
    int retval = -1;
    task_t* task = malloc(sizeof(task_t));
    if (task != NULL) {
        task->handler = &handle_tcp_listen_task;
        task->socket = malloc(sizeof(*task->socket));
        if (task->socket != NULL) {
            bzero(task->socket, sizeof(*task->socket));
            task->socket->fd = socket(family, SOCK_STREAM, IPPROTO_TCP);
            set_non_blocking(task->socket->fd);
            if (task->socket->fd != 0) {
                task->socket->sock_addr.sin_family = family;
                task->socket->sock_addr.sin_addr.s_addr = address;
                task->socket->sock_addr.sin_port = htons(port);

                retval = bind(task->socket->fd, (struct sockaddr*)&task->socket->sock_addr, sizeof(task->socket->sock_addr));
                if (retval == 0) {
                    retval = listen(task->socket->fd, LISTEN_BACKLOG_DEFAULT);
                    if (retval == 0) {
                        if (fd_task_map_insert(shared_data->fd_tasks, &task->socket->fd, &task)) {
#if defined(USE_EPOLL)
                            task->socket->event_spec.events = EPOLLIN | EPOLLEXCLUSIVE;
                            task->socket->event_spec.data.fd = task->socket->fd; // TODO: Remove duplicate
                            retval = epoll_ctl(shared_data->selector_fd, EPOLL_CTL_ADD, task->socket->fd, &task->socket->event_spec);
#elif defined(USE_KQUEUE)
                            // TODO: Could use opaque user data to avoid a task map lookup
                            // TODO: See if thunder herd can be avoided WITHOUT impairing balancing across threads with EV_DISPATCH, EV_CLEAR, or ...
                            EV_SET(&task->socket->event_spec, task->socket->fd, EVFILT_READ, EV_ADD, 0, 0, NULL);
                            retval = kevent(shared_data->selector_fd, &task->socket->event_spec, 1, NULL, 0, NULL);
#endif
                            if (retval == 0) {
                                return task->socket->fd;
                            }
                        }
                    }
                }
            }
        }
    }

    destroy_tcp_listen_task(shared_data, task);
    return retval;
}

void destroy_tcp_listen_task(core_shared_data_t* shared_data, task_t* task) {
    destroy_socket_task(shared_data, task);
}

//
void* core_loop(void* arg) {
    core_loop_data_t* data = (core_loop_data_t*)arg;

    while (1) {
#if defined(USE_EPOLL)
        int ready = epoll_wait(data->shared->selector_fd, data->in_events, CORE_MAX_EVENTS, -1);
#elif defined(USE_KQUEUE)
        // TODO: Pass changelist from previous task into the following kevent call.
        int ready = kevent(data->shared->selector_fd, NULL, 0, data->in_events, CORE_MAX_EVENTS, NULL);
#endif
        for (int i = 0; i < ready; i++) {
            event_t* event = &data->in_events[i];
            task_t* fd_task = NULL;
#if defined(USE_EPOLL)
            int fd = event->data.fd;
#elif defined(USE_KQUEUE)
            int fd = (int)event->ident;
#endif
            if (!fd_task_map_find(data->shared->fd_tasks, &fd, &fd_task)) {
                assert(false);
            }
            fd_task->handler(data, event, fd_task);
        }
    }

    return NULL;
}

//
int main(int argc, char const *argv[]) {
    int retval = 0;

    // Ignore SIGPIPE
    signal(SIGPIPE, SIG_IGN);

    // Prepare shared data, epoll ...
#if defined(USE_EPOLL)
    int selector_fd = epoll_create(1);
#elif defined(USE_KQUEUE)
    int selector_fd = kqueue();
#endif
    assert(selector_fd >= 0);

    core_shared_data_t shared_data = {
        .selector_fd = selector_fd,
        .fd_tasks = fd_task_map_init(1024)
    };

    // Setup core threads
#if defined(IS_GNU_LINUX)
    int core_count = get_nprocs();
#elif defined(IS_FREEBSD)
    //int core_count = sysconf(_SC_NPROCESSORS_ONLN);
    int core_count = 1;
#endif
    core_loop_data_t* cores = malloc(core_count * sizeof(core_loop_data_t));

    for (int i = 0; i < core_count; i++) {
        core_loop_data_t* core_data = &cores[i];
        core_data->id = i;
        core_data->shared = &shared_data;

        pthread_attr_t attr;
        pthread_attr_init(&attr);

#if defined(IS_GNU_LINUX) || defined(IS_FREEBSD)
        // Prepare cpu affinity
#if defined(IS_GNU_LINUX)
        cpu_set_t cpuset;
#elif defined(IS_FREEBSD)
        cpuset_t cpuset;
#endif
        CPU_ZERO(&cpuset);
        CPU_SET(i, &cpuset);
        pthread_attr_setaffinity_np(&attr, sizeof(cpuset), &cpuset);
#endif

        retval = pthread_create(&core_data->thread, &attr, &core_loop, core_data);

        pthread_attr_destroy(&attr);

        if (retval != 0) {
            assert(false);
        }
    }

    // Create listener
    create_tcp_listen_task(&shared_data, AF_INET, INADDR_ANY, 8086);

    // Wait for core threads
    for (int i = 0; i < core_count; i++) {
        retval = pthread_join(cores[i].thread, NULL);
        assert(retval == 0);
    }

    // Cleanup shared data
    retval = close(selector_fd);
    assert(retval == 0);

    fd_task_map_free(shared_data.fd_tasks);

    // Cleanup core data
    free(cores);

    return 0;
}
