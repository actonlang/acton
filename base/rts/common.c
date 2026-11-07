#include <stdlib.h>
#include <errno.h>

#ifdef __linux__
#include <dlfcn.h>
#endif

#include <tlsuv/tlsuv.h>

#include "rts/common.h"
#include <acton_gc_config.h>
#include <stdio.h>
#include <gc/gc_mark.h>

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

// The collector's dirty bits in incremental mode, set by
// acton_init_write_barrier(). The RTS calls it right after GC_INIT, before it
// allocates a GC object, starts a thread or initializes a module, so no
// barrier runs before it. NULL in ordinary mode.
volatile uintptr_t *acton_gc_dirty_bits = NULL;

// Called after GC_INIT, before any thread but the main one runs. The
// collector's table is at the same address for the life of the process. The
// collector reads the bits only in incremental mode, which GC_INIT chooses
// (the generational mode of the RTS, or GC_ENABLE_INCREMENTAL set by hand),
// and nothing changes the mode afterwards. In ordinary mode the barrier gets
// no table and sets no bits.
void acton_init_write_barrier(void) {
    unsigned log_block = 0, log_entries = 0;
    volatile GC_word *bits = GC_get_dirty_bits(&log_block, &log_entries);
    if (bits == NULL) {
        fprintf(stderr, "Acton RTS: the write barrier needs a collector built with the manual dirty-bit mode\n");
        exit(1);
    }
    if (log_block != ACTON_GC_LOG_BLOCK || log_entries != ACTON_GC_LOG_PHT) {
        fprintf(stderr, "Acton RTS: the write barrier was built for GC blocks of 2^%u bytes and 2^%u dirty bits, the collector has 2^%u and 2^%u\n",
                (unsigned)ACTON_GC_LOG_BLOCK, (unsigned)ACTON_GC_LOG_PHT,
                log_block, log_entries);
        exit(1);
    }
    if (GC_is_incremental_mode())
        acton_gc_dirty_bits = (volatile uintptr_t *)bits;
}

#ifdef ACTON_WBAR_DEBUG
void acton_wbar_check(const void *obj, const void *slot) {
    void *base = GC_base((void *)slot);
    if (obj != NULL && base != NULL && base != obj) {
        fprintf(stderr, "Acton RTS: write barrier for slot %p names object %p, the slot is in %p\n",
                slot, obj, base);
        abort();
    }
}
#endif

// The collector reads the dirty bits only in incremental mode (generational
// mode). There, memory that code without the write barrier writes pointers
// into comes from a collector kind that every read rescans
// (GC_set_kind_always_rescanned): libuv writes into the uv loops, handles and
// requests, inside API calls and inside uv_run. libuv's own allocations are
// uncollectable, which the collector also rescans at every read. The kind has
// no thread-local free lists, so every allocation from it takes the allocator
// lock, once per uv request such as a TCP write. In ordinary mode the
// functions below return ordinary GC memory instead, which uv requests get
// from the thread's own free lists. GC_INIT sets the mode, before the first
// of these allocations, and nothing changes it afterwards.
static int acton_rescanned_kind = -1;

static void acton_init_rescanned_kind(void) {
    acton_rescanned_kind = (int)GC_new_kind(GC_new_free_list(), 0 | GC_DS_LENGTH, 1, 1);
    GC_set_kind_always_rescanned(acton_rescanned_kind, 1);
}

static void *gc_malloc_rescanned(size_t size) {
    return GC_generic_malloc(size, acton_rescanned_kind);
}

void *acton_malloc_rescanned(size_t size) {
    // C library memory while the allocator is the C library's, ordinary GC
    // memory in ordinary mode
    if (acton__allocator.calloc != GC_calloc || !GC_is_incremental_mode())
        return acton_malloc(size);
    return gc_malloc_rescanned(size);
}

// In incremental mode the object starts up to align - 1 bytes before the
// returned pointer, which keeps it alive: the collector recognizes interior
// pointers.
void *acton_gc_malloc_rescanned_aligned(size_t align, size_t size) {
    if (!GC_is_incremental_mode())
        return GC_memalign(align, size);
    char *p = gc_malloc_rescanned(size + align - 1);
    if (p == NULL)
        return NULL;
    return (void *)(((uintptr_t)p + align - 1) & ~((uintptr_t)align - 1));
}

// libuv also allocates with realloc(NULL, n), for example the watchers array
// of a loop, which holds pointers into the uv handles. GC_realloc(NULL, n)
// returns ordinary GC memory, so in incremental mode such memory comes from
// the rescanned kind. GC_realloc keeps the kind of an existing object.
static void *uv_gc_realloc(void *ptr, size_t size) {
    if (ptr == NULL && GC_is_incremental_mode())
        return gc_malloc_rescanned(size);
    return GC_realloc(ptr, size);
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
    // uv handles and requests allocated by Acton code are GC memory from
    // acton_malloc_rescanned; libuv never frees those (caller-owned).
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

    acton_init_rescanned_kind();
    uv_replace_allocator(uv_gc_malloc,
                         uv_gc_realloc,
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

    // bsdnt only allocates limb arrays and digit strings, never pointers
    bsdnt_replace_allocator(acton__allocator.malloc_atomic,
                            acton_realloc_atomic,
                            acton__allocator.free);

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

// GC_realloc gives the new copy the kind of the old object, so growing a
// pointer-free buffer keeps it pointer-free. Only realloc(NULL, n) has no
// object to take the kind from and would return scanned memory, so a new
// buffer comes from malloc_atomic instead.
void* acton_realloc_atomic(void* ptr, size_t size) {
    if (ptr == NULL)
        return acton__allocator.malloc_atomic(size);
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
