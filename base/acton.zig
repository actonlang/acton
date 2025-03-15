const std = @import("std");
const expect = std.testing.expect;
const c_acton = @cImport({
    @cInclude("builtin/builtin.h");
});
const gc = @import("rts/gc.zig");

// B_bytes
pub const bytes = extern struct {
    class: usize,
    nbytes: i32,              // length of str in bytes
    str: [*:0]const u8            // str is UTF-8 encoded.
};

// B_NoneType
pub const none = extern struct {
    class: usize,
};

// B_str
pub const str = extern struct {
    class: usize,
    nbytes: i32,              // length of str in bytes
    nchars: i32,              // length of str in Unicode chars
    str: [*:0]const u8            // str is UTF-8 encoded.
};

// B_ValueError
pub const ValueError = extern struct {
    class: *c_acton.B_ValueErrorG_class,
    error_message: c_acton.B_str,
};

// This is the equivalent of the expanded macro in C:
//   $NEW(B_ValueError,to$str(message))
pub fn new_ValueError(message: []const u8) *c_acton.B_ValueError {
    const alloc = gc.allocator();

    const error_ptr = alloc.create(ValueError) catch @panic("OOM");
    const c_error_message = @constCast(message.ptr);
    const error_message_ptr = c_acton.to_str_noc(c_error_message);
    error_ptr.* = .{ .class = @ptrCast(&c_acton.B_ValueErrorG_methods), .error_message = null };
    if (error_ptr.class.__init__) |init_fn| {
        _ = init_fn(@ptrCast(error_ptr), error_message_ptr);
    }
    return @ptrCast(error_ptr);
}

// This is the equivalent of the function call in C:
//   $RAISE((B_BaseException)$NEW(B_ValueError,to$str(message)))
pub fn raise_ValueError(message: []const u8) void {
    const error_ptr = new_ValueError(message);
    // @ptrCast is used to cast the pointer to the correct type expected by the C function
    c_acton.@"$RAISE"(@ptrCast(error_ptr));
    // RAISE does not return, it does a longjmp, so this code is unreachable
    unreachable;
}

test "str struct" {
    // Check that our struct is the same size as the C struct, by using @typeInfo
    // B_str is a pointer to a C struct, so we need to "dereference" the pointer
    // type to get to the struct type, then check the size of the nbytes field
    switch (@typeInfo(c_acton.B_str)) {
        .Pointer => |info| switch (info.size) {
            .C => switch (@typeInfo(info.child)) {
                .Struct => |child_info| {
                    inline for (child_info.fields) |field| {
                        //std.debug.print("struct B_str field: {s} size: {d}\n", .{field.name, @sizeOf(field.type)});
                        if (std.mem.eql(u8, field.name, "nbytes")) {
                            try expect(@sizeOf(field.type) == 4);
                        }
                        if (std.mem.eql(u8, field.name, "nchars")) {
                            try expect(@sizeOf(field.type) == 4);
                        }
                    }
                },
                else => {
                    @compileError("Unhandled type: {s}" ++ @typeName(info.child));
                }
            },
            else => {
                @compileError("Unhandled type:");
            }
        },
        else =>
            std.debug.print("unexpected type\n", .{}),
    }
    try expect(@sizeOf(str) == 24); // 8 + 4 + 4 + 8
//    std.debug.print("size of str: {d}\n", .{ @sizeOf(str) });
//    std.debug.print("size of B_str: {d}\n", .{ @sizeOf(B_str) });
//    std.debug.print("size of imported c_acton.B_str: {d}\n", .{ @sizeOf(c_acton.B_str.*) });
    //try expect(@sizeOf(c_acton.B_str) == 8);
    //try expect(@sizeOf(str) == @sizeOf(c_acton.B_str));
}
