#include <stdlib.h>
#include <errno.h>

#ifdef __linux__
#include <dlfcn.h>
#endif

#define LIBXML_STATIC
#include <libxml/xmlmemory.h>
#include <tlsuv/tlsuv.h>

#include "rts/common.h"

#if defined(_WIN32) || defined(_WIN64)

#include <math.h>

// strndup() is not available on Windows
char *strndup( const char *s1, size_t n)
{
    char *copy= (char*)malloc( n+1 );
    memcpy( copy, s1, n );
    copy[n] = 0;
    return copy;
};
#endif

typedef struct {
    acton_malloc_func malloc;
    acton_malloc_func malloc_atomic;
    acton_realloc_func realloc;
    acton_calloc_func calloc;
    acton_free_func free;
    acton_strdup_func strdup;
    acton_strndup_func strndup;
} acton__allocator_t;

static acton__allocator_t acton__allocator = {
    malloc,
    malloc,
    realloc,
    calloc,
    free,
    strdup,
    strndup
};

void acton_noop_free(void *ptr) {
}

void *GC_calloc(size_t count, size_t size) {
    return GC_malloc(count*size);
}

static void *uv_gc_malloc(size_t size) {
    return GC_malloc_uncollectable(size);
}
static void *uv_gc_calloc(size_t count, size_t size) {
    return GC_malloc_uncollectable(count * size);
}

void acton_init_alloc() {
    // Allocator regimes for the native IO stack:
    //
    // tlsuv and mbedtls live on the libc heap with real malloc/free. tlsuv
    // objects (the per-connection TLS engine especially) are reachable only
    // through libc-allocated structs owned by net.ext.c, and Boehm GC does
    // not scan libc memory: a GC-allocated engine looks unreachable and is
    // collected mid-connection, leaving a dangling pointer behind. Real
    // malloc/free ties their lifetimes to explicit close calls (with GC
    // finalizers via __cleanup__ as a safety net) instead of GC reachability.
    //
    // libuv's internal allocations use GC_malloc_uncollectable + GC_free:
    // they must be GC-SCANNED because they are load-bearing GC roots - the
    // loop's watchers array holds the only reference to idle GC-allocated uv
    // handles (and through handle->data, their actors) once an application
    // retains a connection only via its callbacks. They must also never be
    // collected, because tlsuv now stores libuv allocations (e.g. the
    // getaddrinfo request buffer) inside its own libc structs where the
    // collector cannot see them. libuv pairs every internal allocation with
    // a free, so GC_free reclaims them deterministically.
    //
    // uv handles and requests allocated by Acton code remain ordinary GC
    // memory; libuv never frees those (caller-owned).
    //
    // mbedtls needs no setup: with MBEDTLS_PLATFORM_MEMORY its allocator
    // hooks default to calloc/free, which is the regime we want, and nothing
    // may re-point them mid-run - mbedtls allocates and frees on widely
    // separated code paths, so an allocator change frees objects through the
    // wrong regime. (acton_replace_allocator used to re-point them on every
    // call.) tlsuv_set_allocator passes its calloc/free pair through to
    // mbedtls and also re-points libuv (tlsuv src/alloc.c), so it runs first
    // and uv_replace_allocator then overrides libuv.
    tlsuv_set_allocator(malloc,
                        realloc,
                        calloc,
                        free);

    uv_replace_allocator(uv_gc_malloc,
                         GC_realloc,
                         uv_gc_calloc,
                         GC_free);
}

int acton_replace_allocator(acton_malloc_func malloc_func,
                            acton_malloc_func malloc_atomic_func,
                            acton_realloc_func realloc_func,
                            acton_calloc_func calloc_func,
                            acton_free_func free_func,
                            acton_strdup_func strdup_func,
                            acton_strndup_func strndup_func) {
    if (malloc_func == NULL || malloc_atomic_func == NULL ||
        realloc_func == NULL || calloc_func == NULL ||
        free_func == NULL || strdup_func == NULL ||
        strndup_func == NULL
        ) {
        return -1;
    }

    acton__allocator.malloc = malloc_func;
    acton__allocator.malloc_atomic = malloc_atomic_func;
    acton__allocator.realloc = realloc_func;
    acton__allocator.calloc = calloc_func;
    acton__allocator.free = free_func;
    acton__allocator.strdup = strdup_func;
    acton__allocator.strndup = strndup_func;

    bsdnt_replace_allocator(acton__allocator.malloc,
                            acton__allocator.realloc,
                            acton__allocator.free);

    xmlMemSetup(acton__allocator.free,
                acton__allocator.malloc,
                acton__allocator.realloc,
                acton__allocator.strdup);

    return 0;
}

void* acton_malloc(size_t size) {
    return acton__allocator.calloc(1, size);
}

void* acton_malloc_atomic(size_t size) {
    return acton__allocator.malloc_atomic(size);
}

void* acton_realloc(void* ptr, size_t size) {
    return acton__allocator.realloc(ptr, size);
}


void* acton_calloc(size_t count, size_t size) {
    return acton__allocator.calloc(count, size);
}

void acton_free(void* ptr) {
    int saved_errno;

    /* The system allocator the assumption that errno is not modified but custom
     * allocators may not be so careful.
     */
    saved_errno = errno;
    acton__allocator.free(ptr);
    errno = saved_errno;
}

char *acton_strdup(const char *s) {
    return acton__allocator.strdup(s);
}

char *acton_strndup(const char *s, size_t n) {
    return acton__allocator.strndup(s, n);
}


void* acton_gc_malloc(size_t size) {
    return GC_malloc(size);
}

void* acton_gc_malloc_atomic(size_t size) {
    return GC_malloc_atomic(size);
}

void* acton_gc_realloc(void* ptr, size_t size) {
    return GC_realloc(ptr, size);
}

void* acton_gc_calloc(size_t count, size_t size) {
    return GC_calloc(count, size);
}

void acton_gc_free(void* ptr) {
    return GC_free(ptr);
}

char *acton_gc_strdup(const char *s) {
    return GC_strdup(s);
}

char *acton_gc_strndup(const char *s, size_t n) {
    return GC_strndup(s, n);
}
