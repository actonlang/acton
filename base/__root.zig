const std = @import("std");

const acton = @import("acton.zig");
const gc = @import("rts/gc.zig");

export fn base64Q_encode(data: *acton.bytes) callconv(.C) *acton.bytes {
    const alloc = gc.allocator();
    const encoder = std.base64.standard.Encoder;
    // For possible Unicode input, bytes and chars may not be 1:1
    const data_len: usize = @intCast(data.nbytes);
    const out_len = encoder.calcSize(data_len);
    const buffer = alloc.alloc(u8, out_len) catch @panic("OOM");
    const encoded = encoder.encode(buffer, std.mem.span(data.str));

    const res = alloc.create(acton.bytes) catch @panic("OOM");
    res.* = .{
        .class = data.class,
        .nbytes = @intCast(out_len),
        .str = @as([*:0]const u8, @ptrCast(encoded.ptr))
    };
    return res;
}

export fn base64Q_decode(data: *acton.bytes) callconv(.C) *acton.bytes {
    const alloc = gc.allocator();
    const decoder = std.base64.standard.Decoder;
    // Convert null-terminated string to slice for decoder
    const data_slice = std.mem.sliceTo(data.str, 0);
    // And then compute the exact number of bytes we need to decode, without padding
    const out_len = decoder.calcSizeForSlice(data_slice) catch {
        acton.raise_value_error("Invalid base64 input data");
        unreachable;
    };
    const buffer = alloc.alloc(u8, out_len) catch @panic("OOM");
    decoder.decode(buffer, std.mem.span(data.str)) catch unreachable;

    const res = alloc.create(acton.bytes) catch @panic("OOM");
    res.* = .{
        .class = data.class,
        .nbytes = @intCast(out_len),
        .str = @as([*:0]const u8, @ptrCast(buffer.ptr))
    };
    return res;
}

export fn zig_crypto_hash_md5_init() callconv(.C) *std.crypto.hash.Md5 {
    const alloc = gc.allocator();
    const hasher_ptr = alloc.create(std.crypto.hash.Md5) catch {
        unreachable("OOM while allocating Md5 hasher");
    };
    hasher_ptr.* = std.crypto.hash.Md5.init(.{});

    return hasher_ptr;
}

export fn zig_crypto_hash_md5_update(hasher: *std.crypto.hash.Md5, data: *acton.bytes) callconv(.C) void {
    hasher.update(std.mem.span(data.str));
}

export fn zig_crypto_hash_md5_finalize(hasher: *std.crypto.hash.Md5, output: *acton.bytes) callconv(.C) void {
    const digest_len = 16;
    const out_slice: *[16]u8 = @as([*]u8, @ptrCast(@constCast(output.str)))[0..digest_len];
    hasher.final(out_slice);
}
