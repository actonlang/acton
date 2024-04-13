#ifdef ACTON_THREADS
#define GC_THREADS 1
#endif
#include <gc.h>

#include "io.h"

#include <uv.h>
#include <unistd.h>

#include "log.h"
#include "log.h"

extern char rts_exit;

uv_loop_t *get_uv_loop() {
    WorkerCtx wctx = GET_WCTX();
    return (uv_loop_t *)wctx->uv_loop;
}

void alloc_buffer(uv_handle_t *handle, size_t size, uv_buf_t *buf) {
    *buf = uv_buf_init((char*) acton_malloc_atomic(size), size);
}
