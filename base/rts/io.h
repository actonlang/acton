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
