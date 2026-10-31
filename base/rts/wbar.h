#pragma once

// Write barrier for generational and incremental collection.
//
// In generational mode (ACTON_GC_MODE=generational) a minor collection
// rescans only the marked objects of the heap blocks whose dirty bit is set,
// and a collection marked in slices rescans at its final stop the marked
// objects of the blocks dirtied since its first stop. The program sets the
// bits itself: after storing a GC heap pointer into a GC heap object, it sets
// the bit of the block that holds the slot. The bits are the collector's own
// table (its manual dirty-bit mode), one bit per heap block, hashed on the
// address: ACTON_GC_LOG_BLOCK is the log2 of the block size and
// ACTON_GC_LOG_PHT the log2 of the number of bits. The RTS checks both
// against the collector when it starts.
//
// The rule, at every read of the dirty bits by the collector: a marked
// object that holds a pointer to an unmarked object lies in a dirty block,
// or in a block of a kind that is always rescanned. The collector reads the
// bits only with every thread stopped, and it marks only with every thread
// stopped, so the barrier needs no memory fence.
//
// No barrier is needed for a store:
// - into an object that the storing code allocated and has held in a local
//   variable since (the world-stopped mark that starts a collection and
//   each final mark push the roots, which dirties the block of every
//   object a stack or register references, and the collector dirties every
//   block it hands out while a collection is in progress). This covers the
//   constructors and __init__ of classes, closures and continuations. It
//   does not cover objects reached through the held object, an object
//   loaded back from the heap, or the result of realloc. An explicit
//   __init__ or G_init call on an object other than the self of the
//   calling __init__ or G_init sets the barrier on that object first
//   (acton_wbar_object);
// - of NULL, None, a static object or an integer;
// - into a stack object, a C global, malloc memory or pointer-free GC
//   memory;
// - into memory from acton_malloc_rescanned(), which every read rescans
//   (memory that a C library writes pointers into, such as libuv handles).
//
// Before pointers are copied or moved in bulk into a GC object (memcpy,
// memmove, copy loops, sorting, reversing), ACTON_WBAR_RANGE covers the
// destination. A step of a sliced collection may scan a large object in
// pieces between two moves, and steps do not scan stacks, so the bits must
// be set before the first move.
//
// Every macro takes obj, the start of the GC object that holds the slot, or
// NULL where the code knows only a struct inside the object. The barrier
// that only sets the bit ignores it, but a barrier filtered on the mark bit
// of obj (which would set the bit unconditionally for NULL) and the checks of
// ACTON_WBAR_DEBUG use it. obj must have no side effects.

#include <stddef.h>
#include <stdint.h>
#include <acton_gc_config.h>

#ifdef __cplusplus
extern "C" {
#endif

// The collector's dirty bits (GC_get_dirty_bits), set by the RTS right after
// GC_INIT. In ordinary mode nothing reads them.
extern volatile uintptr_t *acton_gc_dirty_bits;

#ifdef ACTON_WBAR_DEBUG
// Exits with an error if slot is in a GC object other than obj.
void acton_wbar_check(const void *obj, const void *slot);
#define ACTON_WBAR_CHECK_(obj, slot) acton_wbar_check((obj), (slot))
#else
#define ACTON_WBAR_CHECK_(obj, slot) ((void)0)
#endif

#ifdef __cplusplus
}
#endif

static inline __attribute__((always_inline)) volatile uintptr_t *
acton_wbar_word(const void *slot, uintptr_t *mask) {
    uintptr_t i = ((uintptr_t)slot >> ACTON_GC_LOG_BLOCK)
                  & (((uintptr_t)1 << ACTON_GC_LOG_PHT) - 1);
    *mask = (uintptr_t)1 << (i % (8 * sizeof(uintptr_t)));
    return acton_gc_dirty_bits + i / (8 * sizeof(uintptr_t));
}

// Set the bit of the block of slot, with an atomic OR only if it is clear.
static inline __attribute__((always_inline)) volatile uintptr_t *
acton_wbar_dirty(const void *slot) {
    uintptr_t m;
    volatile uintptr_t *w = acton_wbar_word(slot, &m);
    if (!(*w & m))
        __atomic_fetch_or(w, m, __ATOMIC_RELAXED);
    return w;
}

// After the store of v into *slot, in the object obj. The collector stops a
// thread at an instruction boundary, and either order of the store and the
// bit is correct as long as v stays live until both are done: a stop before
// the bit is set marks v from the registers, and the bit set after the
// thread resumes serves the next read. The asm keeps v in a register up to
// the bit; it reads the table word, so it comes after the OR.
#define ACTON_WBAR(obj, slot, v) do {                                   \
        ACTON_WBAR_CHECK_((obj), (slot));                               \
        volatile uintptr_t *acton_wbar_w_ =                             \
            acton_wbar_dirty((const void *)(slot));                     \
        __asm__ __volatile__("" : : "r"(v), "m"(*acton_wbar_w_));       \
    } while (0)

// Before n bytes of pointers are copied or moved into obj at dst: every
// block of the range. The memory clobber keeps the copy after the bits.
static inline void acton_wbar_range(const void *obj, const void *dst, size_t n) {
    uintptr_t p = (uintptr_t)dst;
    uintptr_t end = p + n;
    (void)obj;
    if (n == 0)
        return;
    ACTON_WBAR_CHECK_(obj, dst);
    for (p &= ~(((uintptr_t)1 << ACTON_GC_LOG_BLOCK) - 1); p < end;
         p += (uintptr_t)1 << ACTON_GC_LOG_BLOCK)
        acton_wbar_dirty((const void *)p);
    __asm__ __volatile__("" : : : "memory");
}
#define ACTON_WBAR_RANGE(obj, dst, n)                                   \
    acton_wbar_range((const void *)(obj), (const void *)(dst), (size_t)(n))

// Before an explicit __init__ or G_init call on obj, unless obj is the self
// of the calling __init__ or G_init: every block of the first n bytes of
// obj, the struct of the class named in the call. The method stores into obj
// without barriers and does not keep the values live, so the bits come
// first. They last until the next read of the bits, at a stop that pushes
// the roots, and such a stop during the call finds obj in a register or on
// the stack, as the argument and then the method's self, and sets the bits
// again for the next read. The self of the calling method needs no barrier:
// a constructor still holds it, or this function covered it before the
// calling method ran, since a struct begins with the fields of its parent
// class. Returns obj.
static inline void *acton_wbar_object(void *obj, size_t n) {
    acton_wbar_range(obj, obj, n);
    return obj;
}

// lv = v, then the barrier, for a field lv of obj. v is evaluated first,
// then the address of lv, once, as in the plain assignment that clang
// emits: when evaluating v changes the object that lv names, the store goes
// to the new object.
#define ACTON_STORE(obj, lv, v) do {                                    \
        __typeof__(lv) acton_store_v_ = (v);                            \
        __typeof__(&(lv)) acton_store_p_ = &(lv);                       \
        *acton_store_p_ = acton_store_v_;                               \
        ACTON_WBAR((obj), acton_store_p_, acton_store_v_);              \
    } while (0)

// recv->attr = v, then the barrier: the attribute stores of generated code.
// v first, then recv, each once, like ACTON_STORE.
#define ACTON_STORE_ATTR(recv, attr, v) do {                            \
        __typeof__((recv)->attr) acton_store_v_ = (v);                  \
        __typeof__(recv) acton_store_r_ = (recv);                       \
        acton_store_r_->attr = acton_store_v_;                          \
        ACTON_WBAR(acton_store_r_, &acton_store_r_->attr, acton_store_v_); \
    } while (0)
