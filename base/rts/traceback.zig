const std = @import("std");
const builtin = @import("builtin");
const acton = @import("../acton.zig");

extern fn actBytesFromCStringLengthCopy(text: [*]const u8, len: i64) ?*acton.bytes;
extern "kernel32" fn RtlPcToFileHeader(pc: *const anyopaque, base: *?*anyopaque) callconv(.winapi) ?*anyopaque;

// The first address is in $RAISE. Its caller is the innermost frame we report.
export fn zig_capture_traceback(frames: [*]usize, capacity: usize) usize {
    if (!std.debug.sys_can_stack_trace) return 0;
    const trace = std.debug.captureCurrentStackTrace(.{ .first_address = @returnAddress() }, frames[0..capacity]);
    return trace.return_addresses.len;
}

export fn zig_format_traceback(frames: [*]const usize, count: usize) ?*acton.bytes {
    if (!std.debug.sys_can_stack_trace or count < 2) return null;
    const io = std.Options.debug_io;
    var arena: std.heap.ArenaAllocator = .init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var text: std.Io.Writer.Allocating = .init(allocator);
    var symbols: std.ArrayList(std.debug.Symbol) = .empty;
    frame: for (frames[1..count]) |ret| {
        // Move the return address back inside the call instruction.
        const pc = if (builtin.cpu.arch.isSparc()) ret else ret -| 1;
        if (std.debug.SelfInfo == void) {
            text.writer.print("  0x{x}\n", .{pc}) catch return null;
            continue;
        }
        const info = std.debug.getSelfDebugInfo() catch return null;
        symbols.clearRetainingCapacity();
        // A symbol-table lookup can succeed even when the line information is missing.
        info.getSymbols(io, allocator, allocator, pc, true, &symbols) catch {};
        if (symbols.items.len == 0) symbols.append(allocator, .unknown) catch return null;
        for (symbols.items) |symbol| {
            const name = symbol.name orelse "?";
            if (std.mem.eql(u8, std.mem.trimStart(u8, name, "_"), "wt_work_cb")) break :frame;
            if (symbol.source_location) |loc| {
                text.writer.print("  {s}:{d} in {s}\n", .{ loc.file_name, loc.line, name }) catch return null;
            } else {
                // Undo address randomization so the matching binary can resolve the address offline.
                const slide = if (builtin.os.tag == .windows) base: {
                    var image: ?*anyopaque = undefined;
                    break :base @intFromPtr(RtlPcToFileHeader(@ptrFromInt(pc), &image));
                } else info.getModuleSlide(io, pc) catch 0;
                const module = info.getModuleName(io, pc) catch "?";
                text.writer.print("  0x{x} in {s} ({s})\n", .{ pc -| slide, name, module }) catch return null;
            }
        }
    }
    const result = text.written();
    // The C reporter passes the byte length to printf as a precision.
    if (result.len == 0 or result.len > std.math.maxInt(c_int)) return null;
    return actBytesFromCStringLengthCopy(result.ptr, @intCast(result.len));
}
