const std = @import("std");

const acton = @import("acton.zig");
const gc = @import("rts/gc.zig");

export fn base64Q_encode(data: *acton.str) callconv(.C) *acton.str {
    const alloc = gc.allocator();
    const encoder = std.base64.standard.Encoder;
    const data_len: usize = @intCast(data.nchars);
    const out_len = encoder.calcSize(data_len);
    const buffer = alloc.alloc(u8, out_len) catch @panic("OOM");
    const encoded = encoder.encode(buffer, std.mem.span(data.str));

    const res = alloc.create(acton.str) catch @panic("OOM");
    res.* = .{
        .class = data.class,
        .nbytes = @intCast(out_len),
        .nchars = @intCast(out_len),
        .str = @as([*:0]const u8, @ptrCast(encoded.ptr))
    };
    return res;
}

export fn base64Q_decode(data: *acton.str) callconv(.C) *acton.str {
    const alloc = gc.allocator();
    const decoder = std.base64.standard.Decoder;
    const data_len: usize = @intCast(data.nchars);
    const out_len = decoder.calcSizeUpperBound(data_len) catch unreachable;
    const buffer = alloc.alloc(u8, out_len) catch @panic("OOM");
    decoder.decode(buffer, std.mem.span(data.str)) catch unreachable;

    const res = alloc.create(acton.str) catch @panic("OOM");
    res.* = .{
        .class = data.class,
        .nbytes = @intCast(out_len),
        .nchars = @intCast(out_len),
        .str = @as([*:0]const u8, @ptrCast(buffer.ptr))
    };
    return res;
}
