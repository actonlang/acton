#ifdef ACTON_THREADS
#define GC_THREADS 1
#endif
#include <gc.h>

#include "io.h"

#include <uv.h>
#include <stdlib.h>
#include <unistd.h>

#include "log.h"
#include "log.h"

extern char rts_exit;

uv_loop_t *get_uv_loop() {
    WorkerCtx wctx = GET_WCTX();
    return (uv_loop_t *)wctx->uv_loop;
}

// Read buffer for the libuv read callbacks, one per thread, from libc malloc.
// It must not be GC memory: the kernel writes into it in read() and
// recvmsg(), and when the collector has write-protected that page to track
// writes (mprotect dirty tracking, used for incremental collection), the
// system call fails with EFAULT instead of faulting into the collector's
// handler. libuv calls the alloc callback and then the read callback for
// the same buffer, on the thread that runs the loop, and every read
// callback copies the data out before it returns, so the next read can
// reuse the buffer.
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

int io_stream_write(uv_stream_t *stream, char *data, size_t len) {
    uv_buf_t bufs[io_nbufs(len)];
    unsigned int nbufs = io_bufs(bufs, data, len);
    for (unsigned int i = 0; i < nbufs; i++) {
        uv_write_t *req = (uv_write_t *)acton_malloc(sizeof(uv_write_t));
        int r = uv_write(req, stream, &bufs[i], 1, NULL);
        if (r < 0)
            return r;
    }
    return 0;
}
