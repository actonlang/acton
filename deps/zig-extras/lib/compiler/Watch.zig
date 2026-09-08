// Acton publishes C and headers concurrently. A controlled watch session builds
// only after publication finishes, and acknowledges completed installation.
const std = @import("std");
const Io = std.Io;
const Step = std.Build.Step;
const Allocator = std.mem.Allocator;

pub fn token(b: *std.Build) ![]const u8 {
    const value = b.graph.environ_map.get("ACTON_ZIG_WATCH_TOKEN") orelse return error.MissingWatchToken;
    if (value.len != 32) return error.InvalidWatchToken;
    for (value) |byte| if (!std.ascii.isHex(byte)) return error.InvalidWatchToken;
    return value;
}

fn frame(io: Io, nonce: []const u8, message: []const u8) !void {
    var buffer: [192]u8 = undefined;
    const bytes = try std.fmt.bufPrint(&buffer, "\x00ACTON_ZIG {s} {s}\x00", .{ nonce, message });
    try Io.File.stderr().writeStreamingAll(io, bytes);
}

pub fn ready(io: Io, nonce: []const u8) !void {
    try frame(io, nonce, "ready 1");
}

pub fn request(reader: *Io.Reader) !?u64 {
    const line = (try reader.takeDelimiter('\n')) orelse return null;
    if (!std.mem.startsWith(u8, line, "build ")) return error.InvalidWatchRequest;
    return try std.fmt.parseUnsigned(u64, line[6..], 10);
}

pub const State = struct {
    inputs: std.AutoHashMapUnmanaged(*Step, u64) = .empty,
    outputs: std.AutoHashMapUnmanaged(*Step, u64) = .empty,
    before: std.AutoHashMapUnmanaged(*Step, u64) = .empty,

    pub fn deinit(state: *State, gpa: Allocator) void {
        state.inputs.deinit(gpa);
        state.outputs.deinit(gpa);
        state.before.deinit(gpa);
    }

    pub fn prepare(state: *State, gpa: Allocator, steps: []const *Step) !void {
        var scratch = std.heap.ArenaAllocator.init(gpa);
        defer scratch.deinit();
        const arena = scratch.allocator();
        state.before.clearRetainingCapacity();
        for (steps) |step| {
            const stamp = try inputStamp(arena, step);
            try state.before.put(gpa, step, stamp);
            if (state.inputs.get(step)) |old| {
                const output_stamp = try outputStamp(arena, step);
                const changed = stamp != old or output_stamp != state.outputs.get(step).?;
                const retry = switch (step.state) {
                    .failure, .dependency_failure, .skipped, .skipped_oom => true,
                    else => false,
                };
                // Installation is cheap and also restores deleted out/ files.
                const check = switch (step.id) {
                    .install_artifact, .install_file, .install_dir, .run, .custom => true,
                    else => false,
                };
                if (changed or retry or check) {
                    _ = step.invalidateResult(gpa);
                } else if (step.state == .success) {
                    step.result_cached = true;
                    step.result_duration_ns = null;
                }
            }
        }
    }

    pub fn finish(state: *State, gpa: Allocator, steps: []const *Step) !void {
        var scratch = std.heap.ArenaAllocator.init(gpa);
        defer scratch.deinit();
        const arena = scratch.allocator();
        for (steps) |step| {
            // Newly discovered inputs must be checked on the next request too:
            // their post-build stamps cannot prove which contents were built.
            try state.inputs.put(gpa, step, state.before.get(step).?);
            try state.outputs.put(gpa, step, try outputStamp(arena, step));
        }
    }
};

fn inputStamp(arena: Allocator, step: *Step) !u64 {
    const b = step.owner;
    var stamp: u64 = 0;
    for (step.inputs.table.keys(), step.inputs.table.values()) |dir, files| {
        for (files.items) |file| {
            const path = try std.fs.path.resolve(arena, &.{ dir.root_dir.path orelse ".", dir.sub_path, file });
            var hash = std.hash.Wyhash.init(0);
            hash.update(path);
            std.hash.autoHash(&hash, try stampPath(arena, b.graph.io, path));
            stamp +%= hash.final();
        }
    }
    return stamp;
}

fn outputStamp(arena: Allocator, step: *Step) !u64 {
    const path: ?[]const u8 = switch (step.id) {
        .compile => if (step.cast(Step.Compile).?.generated_bin) |file| file.path else null,
        .write_file => step.cast(Step.WriteFile).?.generated_directory.path,
        .config_header => step.cast(Step.ConfigHeader).?.generated_dir.path,
        .options => step.cast(Step.Options).?.generated_file.path,
        .translate_c => step.cast(Step.TranslateC).?.output_file.path,
        .objcopy => step.cast(Step.ObjCopy).?.output_file.path,
        else => null,
    };
    return if (path) |p| try stampPath(arena, step.owner.graph.io, p) else 0;
}

fn stampPath(arena: Allocator, io: Io, path: []const u8) anyerror!u64 {
    var ancestors: std.StringHashMapUnmanaged(void) = .empty;
    defer ancestors.deinit(arena);
    return stampPathRecursive(arena, io, path, &ancestors);
}

fn stampPathRecursive(arena: Allocator, io: Io, path: []const u8, ancestors: *std.StringHashMapUnmanaged(void)) anyerror!u64 {
    const link = Io.Dir.cwd().statFile(io, path, .{ .follow_symlinks = false }) catch |err| switch (err) {
        error.FileNotFound, error.NotDir => return 0,
        else => return err,
    };
    var hash = std.hash.Wyhash.init(0);
    const stat = if (link.kind == .sym_link) blk: {
        std.hash.autoHash(&hash, .{ link.inode, link.size, link.mtime.nanoseconds, link.ctime.nanoseconds, link.kind });
        break :blk Io.Dir.cwd().statFile(io, path, .{}) catch |err| switch (err) {
            error.FileNotFound, error.NotDir => return hash.final(),
            else => return err,
        };
    } else link;
    std.hash.autoHash(&hash, .{ stat.inode, stat.size, stat.mtime.nanoseconds, stat.ctime.nanoseconds, stat.kind });
    if (stat.kind == .directory) {
        // Follow linked directories too, but stop paths back to an ancestor.
        const canonical = try Io.Dir.cwd().realPathFileAlloc(io, path, arena);
        const seen = try ancestors.getOrPut(arena, canonical);
        if (seen.found_existing) return hash.final();
        defer _ = ancestors.remove(canonical);
        var dir = try Io.Dir.cwd().openDir(io, path, .{ .iterate = true });
        defer dir.close(io);
        var it = dir.iterate();
        var children: u64 = 0;
        while (try it.next(io)) |entry| {
            const child = try std.fs.path.join(arena, &.{ path, entry.name });
            var item = std.hash.Wyhash.init(0);
            item.update(entry.name);
            const stamp = try stampPathRecursive(arena, io, child, ancestors);
            std.hash.autoHash(&item, stamp);
            children +%= item.final();
        }
        std.hash.autoHash(&hash, children);
    }
    return hash.final();
}

pub fn done(io: Io, nonce: []const u8, generation: u64, steps: []const *Step) !void {
    const success = for (steps) |step| switch (step.state) {
        .success, .skipped => {},
        else => break false,
    } else true;
    var buffer: [96]u8 = undefined;
    const message = try std.fmt.bufPrint(&buffer, "done {d} {s}", .{ generation, if (success) "ok" else "failed" });
    try frame(io, nonce, message);
}
