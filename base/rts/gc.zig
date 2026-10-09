const std = @import("std");
const assert = std.debug.assert;
const testing = std.testing;
const mem = std.mem;
const Allocator = std.mem.Allocator;

extern fn GC_is_init_called() c_int;
extern fn GC_init() void;
extern fn GC_set_all_interior_pointers(c_int) void;
extern fn GC_get_heap_size() usize;
extern fn GC_disable() void;
extern fn GC_enable() void;
extern fn GC_gcollect() void;
extern fn GC_collect_a_little() c_int;
extern fn GC_set_find_leak(c_int) void;
extern fn GC_malloc(usize) ?*anyopaque;
extern fn GC_malloc_atomic(usize) ?*anyopaque;
extern fn GC_free(?*anyopaque) void;
extern fn GC_size(?*const anyopaque) usize;

/// Returns the Allocator used for APIs in Zig
pub fn allocator() Allocator {
    // Initialize libgc
    if (GC_is_init_called() == 0) {
        GC_init();
    }

    return Allocator{
        .ptr = undefined,
        .vtable = &gc_allocator_vtable,
    };
}

/// Returns an Allocator for pointer-free data. Its memory is not scanned by
/// the collector and not zeroed, so it must never hold pointers to GC memory.
pub fn atomicAllocator() Allocator {
    if (GC_is_init_called() == 0) {
        GC_init();
    }

    return Allocator{
        .ptr = undefined,
        .vtable = &gc_atomic_allocator_vtable,
    };
}

/// Enable or disable interior pointers.
/// If used, this must be called before the first allocator() call.
pub fn setAllInteriorPointers(enable_interior_pointers: bool) void {
    GC_set_all_interior_pointers(@intFromBool(enable_interior_pointers));
}

/// Returns the current heap size of used memory.
pub fn getHeapSize() u64 {
    return GC_get_heap_size();
}

/// Disable garbage collection.
pub fn disable() void {
    GC_disable();
}

/// Enables garbage collection. GC is enabled by default so this is
/// only useful if you called disable earlier.
pub fn enable() void {
    GC_enable();
}

// Performs a full, stop-the-world garbage collection. With leak detection
// enabled this will output any leaks as well.
pub fn collect() void {
    GC_gcollect();
}

/// Perform some garbage collection. Returns zero when work is done.
pub fn collectLittle() u8 {
    return @as(u8, @intCast(GC_collect_a_little()));
}

/// Enables leak-finding mode. See the libgc docs for more details.
pub fn setFindLeak(v: bool) void {
    return GC_set_find_leak(@intFromBool(v));
}

// TODO(mitchellh): there are so many more functions to add here
// from gc.h, just add em as they're useful.

/// GcAllocator is an implementation of std.mem.Allocator that uses
/// libgc under the covers. This means that all memory allocated with
/// this allocated doesn't need to be explicitly freed (but can be).
///
/// The GC is a singleton that is globally shared. Multiple GcAllocators
/// do not allocate separate pages of memory; they share the same underlying
/// pages.
///
// NOTE(mitchellh): this is basically just a copy of the standard CAllocator
// since libgc has a malloc/free-style interface. There are very slight differences
// due to API differences but overall the same.
pub const GcAllocator = struct {
    fn alloc(
        _: *anyopaque,
        len: usize,
        alignment: mem.Alignment,
        return_address: usize,
    ) ?[*]u8 {
        _ = return_address;
        assert(len > 0);
        return alignedAlloc(len, alignment, false);
    }

    fn allocAtomic(
        _: *anyopaque,
        len: usize,
        alignment: mem.Alignment,
        return_address: usize,
    ) ?[*]u8 {
        _ = return_address;
        assert(len > 0);
        return alignedAlloc(len, alignment, true);
    }

    fn resize(
        _: *anyopaque,
        buf: []u8,
        alignment: mem.Alignment,
        new_len: usize,
        return_address: usize,
    ) bool {
        _ = alignment;
        _ = return_address;
        if (new_len <= buf.len) {
            return true;
        }

        const full_len = alignedAllocSize(buf.ptr);
        if (new_len <= full_len) {
            return true;
        }

        return false;
    }

    fn free(
        _: *anyopaque,
        buf: []u8,
        alignment: mem.Alignment,
        return_address: usize,
    ) void {
        _ = alignment;
        _ = return_address;
        alignedFree(buf.ptr);
    }

    fn getHeader(ptr: [*]u8) *[*]u8 {
        return @as(*[*]u8, @ptrFromInt(@intFromPtr(ptr) - @sizeOf(usize)));
    }

    fn alignedAlloc(len: usize, alignment: mem.Alignment, comptime atomic: bool) ?[*]u8 {
        const alignment_bytes = alignment.toByteUnits();
        const size = len + alignment_bytes - 1 + @sizeOf(usize);

        // Thin wrapper around regular malloc, overallocate to account for
        // alignment padding and store the orignal malloc()'ed pointer before
        // the aligned address.
        const raw_ptr = if (atomic) GC_malloc_atomic(size) else GC_malloc(size);
        const unaligned_ptr = @as([*]u8, @ptrCast(raw_ptr orelse return null));
        const unaligned_addr = @intFromPtr(unaligned_ptr);
        const aligned_addr = mem.alignForward(usize, unaligned_addr + @sizeOf(usize), alignment_bytes);
        const aligned_ptr = unaligned_ptr + (aligned_addr - unaligned_addr);
        getHeader(aligned_ptr).* = unaligned_ptr;

        return aligned_ptr;
    }

    fn alignedFree(ptr: [*]u8) void {
        const unaligned_ptr = getHeader(ptr).*;
        GC_free(unaligned_ptr);
    }

    fn alignedAllocSize(ptr: [*]u8) usize {
        const unaligned_ptr = getHeader(ptr).*;
        const delta = @intFromPtr(ptr) - @intFromPtr(unaligned_ptr);
        return GC_size(unaligned_ptr) - delta;
    }

    fn remap(
        _: *anyopaque,
        _: []u8,
        _: mem.Alignment,
        _: usize,
        _: usize,
    ) ?[*]u8 {
        return null;
    }
};

const gc_allocator_vtable = Allocator.VTable{
    .alloc = GcAllocator.alloc,
    .resize = GcAllocator.resize,
    .remap = GcAllocator.remap,
    .free = GcAllocator.free,
};

const gc_atomic_allocator_vtable = Allocator.VTable{
    .alloc = GcAllocator.allocAtomic,
    .resize = GcAllocator.resize,
    .remap = GcAllocator.remap,
    .free = GcAllocator.free,
};

test "GcAllocator" {
    const alloc = allocator();

    try std.heap.testAllocator(alloc);
    try std.heap.testAllocatorAligned(alloc);
    try std.heap.testAllocatorLargeAlignment(alloc);
    try std.heap.testAllocatorAlignedShrink(alloc);
}

test "heap size" {
    // No garbage so should be 0
    try testing.expect(collectLittle() == 0);

    // Force a collection should work
    collect();

    try testing.expect(getHeapSize() > 0);
}
