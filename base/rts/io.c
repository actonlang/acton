#ifdef ACTON_THREADS
#define GC_THREADS 1
#endif
#include <gc.h>

#include "io.h"

#include <uv.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "log.h"
#include "log.h"

extern char rts_exit;

uv_loop_t *get_uv_loop() {
    WorkerCtx wctx = GET_WCTX();
    return (uv_loop_t *)wctx->uv_loop;
}

// Read buffer for the libuv read callbacks, one per thread, from libc malloc.
// libuv calls the alloc callback and then the read callback for the same
// buffer, on the thread that runs the loop, and every read callback copies
// the data out before it returns, so the next read can reuse the buffer.
static _Thread_local char *io_read_buf = NULL;
static _Thread_local size_t io_read_buf_size = 0;

void alloc_buffer(uv_handle_t *handle, size_t size, uv_buf_t *buf) {
    if (io_read_buf_size < size) {
        char *p = realloc(io_read_buf, size);
        if (p == NULL) {
            // libuv passes UV_ENOBUFS to the read callback
            *buf = uv_buf_init(NULL, 0);
            return;
        }
        io_read_buf = p;
        io_read_buf_size = size;
    }
    *buf = uv_buf_init(io_read_buf, size);
}

int sockaddr_to_addr_port(const struct sockaddr *addr, char *addrbuf, size_t addrbuf_len, int *port) {
    if (addr == NULL) {
        return UV_EINVAL;
    }
    if (addr->sa_family == AF_INET) {
        const struct sockaddr_in *addr4 = (const struct sockaddr_in *)addr;
        int r = uv_ip4_name(addr4, addrbuf, addrbuf_len);
        if (r != 0) {
            return r;
        }
        *port = (int)ntohs(addr4->sin_port);
        return 0;
    } else if (addr->sa_family == AF_INET6) {
        const struct sockaddr_in6 *addr6 = (const struct sockaddr_in6 *)addr;
        int r = uv_ip6_name(addr6, addrbuf, addrbuf_len);
        if (r != 0) {
            return r;
        }
        if (addr6->sin6_scope_id != 0) {
            size_t len = strlen(addrbuf);
            char zone[UV_IF_NAMESIZE];
            size_t zone_len = sizeof(zone);
            if (uv_if_indextoname(addr6->sin6_scope_id, zone, &zone_len) != 0) {
                snprintf(zone, sizeof(zone), "%u", (unsigned)addr6->sin6_scope_id);
            }
            snprintf(addrbuf + len, addrbuf_len - len, "%%%s", zone);
        }
        *port = (int)ntohs(addr6->sin6_port);
        return 0;
    }
    return UV_EINVAL;
}
