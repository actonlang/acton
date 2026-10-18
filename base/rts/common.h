#pragma once
#include <stddef.h>
#include <stdlib.h>

#define GC_THREADS 1
#ifdef _WIN32
#include <winsock2.h>
#endif
#include <gc.h>

typedef void *(*acton_malloc_func)(size_t size);
typedef void *(*acton_malloc_func)(size_t size);
typedef void *(*acton_realloc_func)(void* ptr, size_t size);
typedef void *(*acton_calloc_func)(size_t count, size_t size);
typedef void (*acton_free_func)(void* ptr);
typedef char *(*acton_strdup_func)(const char* s);
typedef char *(*acton_strndup_func)(const char* s, size_t n);

void acton_init_malloc();

// malloc_func, malloc_atomic_func, calloc_func, strdup_func and strndup_func
// must return a new object, because the acton_* functions that call them are
// declared with GC_ATTR_MALLOC below.
int acton_replace_allocator(acton_malloc_func malloc_func,
                            acton_malloc_func malloc_atomic_func,
                            acton_realloc_func realloc_func,
                            acton_calloc_func calloc_func,
                            acton_free_func free_func,
                            acton_strdup_func strdup_func,
                            acton_strndup_func strndup_func);


// acton_malloc and the other allocation functions that use acton__allocator
// call the allocator, the C library or the collector, through its function
// pointers, so the attributes of malloc, GC_malloc and the others do not reach
// the callers. Each declaration here states the attributes of the allocator
// functions it calls, as gc.h declares them for GC_malloc and the others.
//
// GC_ATTR_MALLOC says that the returned pointer aliases no pointer that exists
// when the function returns, and that the new object holds no pointers to
// existing objects. A store into the new object therefore changes no other
// memory. Without that, a byte store may change any memory, including the
// fields the caller loaded s->str and res->str from, so a loop like
// res->str[i] = s->str[t] has to load s->str and res->str again after every
// byte. With it, the caller keeps loaded pointers and values in registers
// across the stores, and the compiler can vectorize a loop that copies into the
// new object. GC_ATTR_ALLOC_SIZE and GC_ATTR_CALLOC_SIZE give the size of the
// returned object, which __builtin_object_size reports.
GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1) void *acton_malloc(size_t size);
GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1) void *acton_malloc_atomic(size_t size);
// The returned object may be the old one, and it holds the old contents with
// their pointers to existing objects, so only its size is declared.
GC_ATTR_ALLOC_SIZE(2) void *acton_realloc(void* ptr, size_t size);
// Like acton_realloc, for a buffer that holds no pointers to GC memory: a new
// buffer (ptr NULL) is allocated pointer-free
GC_ATTR_ALLOC_SIZE(2) void *acton_realloc_atomic(void* ptr, size_t size);
GC_ATTR_MALLOC GC_ATTR_CALLOC_SIZE(1, 2)
void *acton_calloc(size_t count, size_t size);
void acton_free(void* ptr);
GC_ATTR_MALLOC char *acton_strdup(const char *s);
GC_ATTR_MALLOC char *acton_strndup(const char *s, size_t n);

int acton_sync_pause_begin(void);
void acton_sync_pause_end(void);

// The acton_gc_* functions call the collector directly, from common.c. Their
// callers see only these declarations, which state the attributes of GC_malloc
// and the other collector functions behind them.
GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1) void *acton_gc_malloc(size_t size);
GC_ATTR_MALLOC GC_ATTR_ALLOC_SIZE(1) void *acton_gc_malloc_atomic(size_t size);
GC_ATTR_ALLOC_SIZE(2) void *acton_gc_realloc(void* ptr, size_t size);
GC_ATTR_MALLOC GC_ATTR_CALLOC_SIZE(1, 2)
void *acton_gc_calloc(size_t count, size_t size);
void acton_gc_free(void* ptr);
GC_ATTR_MALLOC char *acton_gc_strdup(const char *s);
GC_ATTR_MALLOC char *acton_gc_strndup(const char *s, size_t n);
