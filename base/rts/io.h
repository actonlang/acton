#pragma once

#ifdef __linux__
#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif
#endif

#define GC_THREADS 1
#include <gc.h>

#include <uv.h>

#ifdef __gnu_linux__
    #define IS_GNU_LINUX
#elif  __APPLE__ && __MACH__
    #define IS_MACOS
#endif

extern uv_loop_t *aux_uv_loop;
uv_loop_t *get_uv_loop();

void alloc_buffer(uv_handle_t *handle, size_t size, uv_buf_t *buf);

// uv_buf_init takes the length of a buffer as an unsigned int and tlsuv
// returns how much it wrote as an int, so data is handed to them in pieces
// of at most this many bytes.
#define IO_MAX_BUF_LEN ((size_t)1 << 30)

// The number of pieces io_bufs splits len bytes into, at least one.
static inline unsigned int io_nbufs(size_t len) {
    return len == 0 ? 1 : (unsigned int)((len + IO_MAX_BUF_LEN - 1) / IO_MAX_BUF_LEN);
}

// Point bufs, which has room for io_nbufs(len) buffers, at the pieces of
// data[0..len] in order and return how many there are.
static inline unsigned int io_bufs(uv_buf_t *bufs, char *data, size_t len) {
    unsigned int n = io_nbufs(len);
    for (unsigned int i = 0; i < n; i++) {
        size_t off = (size_t)i * IO_MAX_BUF_LEN;
        size_t piece = len - off < IO_MAX_BUF_LEN ? len - off : IO_MAX_BUF_LEN;
        bufs[i] = uv_buf_init(data + off, (unsigned int)piece);
    }
    return n;
}

// Write data[0..len] to stream with one uv_write per piece, since macOS
// fails a writev of more than INT_MAX bytes in all. Returns 0, or the error
// of the first piece that could not be queued.
int io_stream_write(uv_stream_t *stream, char *data, size_t len);
