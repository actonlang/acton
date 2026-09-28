const std = @import("std");
const expect = std.testing.expect;

pub const B_str = ?*str;
const B_BaseException = opaque {};
const B_ValueError = opaque {};
const B_MemoryError = opaque {};

extern fn actStrFromCString(str: [*:0]const u8) B_str;
extern fn B_ValueErrorG_new(B_str) ?*B_ValueError;
extern fn B_MemoryErrorG_new(B_str) ?*B_MemoryError;
extern fn @"$RAISE"(?*B_BaseException) void;
extern fn acton_malloc(size: usize) ?*anyopaque;
extern fn acton_malloc_atomic(size: usize) ?*anyopaque;

// B_bytes
pub const bytes = extern struct {
    class: usize,
    nbytes: i64,              // length of str in bytes
    str: [*]const u8            // nbytes bytes, not NUL-terminated
};

// Allocate a bytes object with room for nbytes payload bytes, the same way
// NEW_UNFILLED_BYTES in builtin/str.c does it: the object comes from
// acton_malloc (scanned, it holds the str pointer), the payload from
// acton_malloc_atomic (pointer-free) and is exactly nbytes long, with no
// terminator. An empty result still gets a real buffer from the collector
// instead of the dangling pointer a zero-length Zig allocation yields. The
// caller fills str[0..nbytes]; class is normally taken from an existing
// bytes value.
pub fn new_bytes(class: usize, nbytes: usize) *bytes {
    const res: *bytes = @ptrCast(@alignCast(acton_malloc(@sizeOf(bytes)) orelse {
        raise_MemoryError("OOM while allocating bytes");
        unreachable;
    }));
    const buf: [*]u8 = @ptrCast(acton_malloc_atomic(nbytes) orelse {
        raise_MemoryError("OOM while allocating bytes");
        unreachable;
    });
    res.* = .{
        .class = class,
        .nbytes = @intCast(nbytes),
        .str = buf,
    };
    return res;
}

// B_NoneType
pub const none = extern struct {
    class: usize,
};

// B_str
pub const str = extern struct {
    class: usize,
    nbytes: i64,              // length of str in bytes
    nchars: i64,              // length of str in Unicode chars
    str: [*:0]const u8            // str is UTF-8 encoded.
};

// This is the equivalent of the expanded macro in C:
//   $NEW(B_ValueError,actStrFromCString(message))
pub fn new_ValueError(message: [:0]const u8) ?*B_ValueError {
    return B_ValueErrorG_new(actStrFromCString(message.ptr));
}

// This is the equivalent of the function call in C:
//   $RAISE((B_BaseException)$NEW(B_ValueError,actStrFromCString(message)))
pub fn raise_ValueError(message: [:0]const u8) void {
    const error_ptr = new_ValueError(message);
    // @ptrCast is used to cast the pointer to the correct type expected by the C function
    @"$RAISE"(@ptrCast(error_ptr));
    // RAISE does not return, it does a longjmp, so this code is unreachable
    unreachable;
}

// This is the equivalent of the expanded macro in C:
//  $NEW(B_MemoryError,actStrFromCString(message))
pub fn new_MemoryError(message: [:0]const u8) ?*B_MemoryError {
    return B_MemoryErrorG_new(actStrFromCString(message.ptr));
}

// This is the equivalent of the function call in C:
//   $RAISE((B_BaseException)$NEW(B_MemoryError,actStrFromCString(message)))
pub fn raise_MemoryError(message: [:0]const u8) void {
    const error_ptr = new_MemoryError(message);
    // @ptrCast is used to cast the pointer to the correct type expected by the C function
    @"$RAISE"(@ptrCast(error_ptr));
    // RAISE does not return, it does a longjmp, so this code is unreachable
    unreachable;
}

test "str struct" {
    // Check that our struct is the same size as the C struct, by using @typeInfo
    // B_str is a pointer to a C struct, so we need to "dereference" the pointer
    // type to get to the struct type, then check the size of the nbytes field
    try expect(@sizeOf(@FieldType(str, "nbytes")) == 8);
    try expect(@sizeOf(@FieldType(str, "nchars")) == 8);
    try expect(@sizeOf(str) == 32); // 8 + 8 + 8 + 8
//    std.debug.print("size of str: {d}\n", .{ @sizeOf(str) });
//    std.debug.print("size of B_str: {d}\n", .{ @sizeOf(B_str) });
//    std.debug.print("size of imported c_acton.B_str: {d}\n", .{ @sizeOf(c_acton.B_str.*) });
    //try expect(@sizeOf(c_acton.B_str) == 8);
    //try expect(@sizeOf(str) == @sizeOf(c_acton.B_str));
}
