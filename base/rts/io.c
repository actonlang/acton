#include "io.h"

#include <uv.h>
#include <unistd.h>

#define GC_THREADS 1
#include "gc.h"

#include "log.h"
#include "log.h"

extern char rts_exit;

uv_loop_t *get_uv_loop() {
    return (uv_loop_t *)pthread_getspecific(pkey_uv_loop);
}

void alloc_buffer(uv_handle_t *handle, size_t size, uv_buf_t *buf) {
    *buf = uv_buf_init((char*) GC_MALLOC_ATOMIC(size), size);
}
