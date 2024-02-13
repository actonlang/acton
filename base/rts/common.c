#include <stdlib.h>
#include <errno.h>

#include <mbedtls/platform.h>
#include <libxml/xmlmemory.h>
#include <tlsuv/tlsuv.h>

void *(*real_malloc)(size_t) = NULL;
void *(*real_realloc)(void *, size_t) = NULL;
void *(*real_calloc)(size_t, size_t) = NULL;
void (*real_free)(void *) = NULL;
char *(*real_strdup)(const char *) = NULL;
char *(*real_strndup)(const char *, size_t) = NULL;

int resolve_real_malloc() {
    real_malloc = dlsym(RTLD_NEXT, "malloc");
    if (!real_malloc) {
        fprintf(stderr, "Error in `dlsym`: %s\n", dlerror());
        return 0;
    }
    real_realloc = dlsym(RTLD_NEXT, "realloc");
    if (!real_realloc) {
        fprintf(stderr, "Error in `dlsym`: %s\n", dlerror());
        return 0;
    }
    real_calloc = dlsym(RTLD_NEXT, "calloc");
    if (!real_calloc) {
        fprintf(stderr, "Error in `dlsym`: %s\n", dlerror());
        return 0;
    }
    real_free = dlsym(RTLD_NEXT, "free");
    if (!real_free) {
        fprintf(stderr, "Error in `dlsym`: %s\n", dlerror());
        return 0;
    }
    real_strdup = dlsym(RTLD_NEXT, "strdup");
    if (!real_strdup) {
        fprintf(stderr, "Error in `dlsym`: %s\n", dlerror());
        return 0;
    }
    real_strndup = dlsym(RTLD_NEXT, "strndup");
    if (!real_strndup) {
        fprintf(stderr, "Error in `dlsym`: %s\n", dlerror());
        return 0;
    }
    return 1;
}

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

void *GC_calloc(size_t count, size_t size) {
    return GC_malloc(count*size);
}

void acton_init_alloc() {
    // UV & TLSUV are used for IO, which is always done on the GC-heap. We don't
    // have any constants related to UV, so we can always use the GC
    uv_replace_allocator(GC_malloc,
                         GC_realloc,
                         GC_calloc,
                         GC_free);

    tlsuv_replace_allocator(GC_malloc,
                            GC_realloc,
                            GC_calloc,
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

    mbedtls_platform_set_calloc_free(acton__allocator.calloc,
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
