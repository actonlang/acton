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

#ifdef ACTON_THREADS
#define GET_UV_LOOP() (uv_loop_t *)pthread_getspecific(pkey_uv_loop)
#else
#define GET_UV_LOOP() aux_uv_loop
#endif
uv_loop_t *get_uv_loop() {
    return GET_UV_LOOP();
}

void alloc_buffer(uv_handle_t *handle, size_t size, uv_buf_t *buf) {
    *buf = uv_buf_init((char*) acton_malloc_atomic(size), size);
}
