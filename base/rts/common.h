#pragma once
#include <dlfcn.h>
#include <stddef.h>
#include <stdlib.h>

extern void *(*real_malloc)(size_t);
extern void *(*real_malloc_atomic)(size_t);
extern void *(*real_realloc)(void *, size_t);
extern void *(*real_calloc)(size_t, size_t);
extern void (*real_free)(void *);
extern char *(*real_strdup)(const char *);
extern char *(*real_strndup)(const char *, size_t);
int resolve_real_malloc();

typedef void *(*acton_malloc_func)(size_t size);
typedef void *(*acton_malloc_func)(size_t size);
typedef void *(*acton_realloc_func)(void* ptr, size_t size);
typedef void *(*acton_calloc_func)(size_t count, size_t size);
typedef void (*acton_free_func)(void* ptr);
typedef char *(*acton_strdup_func)(const char* s);
typedef char *(*acton_strndup_func)(const char* s, size_t n);

void acton_init_malloc();

int acton_replace_allocator(acton_malloc_func malloc_func,
                            acton_malloc_func malloc_atomic_func,
                            acton_realloc_func realloc_func,
                            acton_calloc_func calloc_func,
                            acton_free_func free_func,
                            acton_strdup_func strdup_func,
                            acton_strndup_func strndup_func);


void *acton_malloc(size_t size);
void *acton_malloc_atomic(size_t size);
void *acton_realloc(void* ptr, size_t size);
void *acton_calloc(size_t count, size_t size);
void acton_free(void* ptr);
char *acton_strdup(const char *s);
char *acton_strndup(const char *s, size_t n);
