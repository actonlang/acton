const std = @import("std");
const builtin = @import("builtin");
const print = @import("std").debug.print;
const tgt = @import("builtin").target;

// Helper functions for Zig 0.13/0.14 compatibility
fn targetIsDarwin(t: std.Target) bool {
    const is_zig_0_14 = comptime builtin.zig_version.order(std.SemanticVersion.parse("0.14.0") catch unreachable) != .lt;
    if (is_zig_0_14) {
        return t.os.tag.isDarwin();
    } else {
        return t.isDarwin();
    }
}

fn targetIsBSD(t: std.Target) bool {
    const is_zig_0_14 = comptime builtin.zig_version.order(std.SemanticVersion.parse("0.14.0") catch unreachable) != .lt;
    if (is_zig_0_14) {
        return t.os.tag.isBSD();
    } else {
        return t.isBSD();
    }
}

pub fn build(b: *std.Build) void {
    const optimize = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});
    const t = target.result;

    const lib = b.addLibrary(.{
        .name = "uv",
        .linkage = .static,
        .root_module = b.createModule(.{
            .target = target,
            .optimize = optimize,
        }),
    });

    var flags = std.ArrayList([]const u8).empty;
    defer flags.deinit(b.allocator);

    if (t.os.tag == .windows) {
        flags.appendSlice(b.allocator, &.{
            "-D_FILE_OFFSET_BITS=64",
            "-D_LARGEFILE_SOURCE",
        }) catch unreachable;
        if (optimize == .Debug) {
            flags.appendSlice(b.allocator, &.{
                "-U_DEBUG",
                "-DNDEBUG",
            }) catch unreachable;
        }
    }

    if (t.os.tag == .linux) {
        flags.appendSlice(b.allocator, &.{
            "-D_GNU_SOURCE",
            "-D_POSIX_C_SOURCE=200112",
        }) catch unreachable;
    }

    if (targetIsDarwin(t)) {
        flags.appendSlice(b.allocator, &.{
            "-D_DARWIN_UNLIMITED_SELECT=1",
            "-D_DARWIN_USE_64_BIT_INODE=1",
        }) catch unreachable;
    }

    lib.root_module.addCSourceFiles(.{ .files = &.{
        "src/fs-poll.c",
        "src/idna.c",
        "src/inet.c",
        "src/random.c",
        "src/strscpy.c",
        "src/strtok.c",
        "src/threadpool.c",
        "src/timer.c",
        "src/uv-common.c",
        "src/uv-data-getter-setters.c",
        "src/version.c",
    }, .flags = flags.items });

    if (t.os.tag == .windows) {
        lib.root_module.addCSourceFiles(.{ .files = &.{ "src/win/async.c", "src/win/core.c", "src/win/detect-wakeup.c", "src/win/dl.c", "src/win/error.c", "src/win/fs.c", "src/win/fs-event.c", "src/win/getaddrinfo.c", "src/win/getnameinfo.c", "src/win/handle.c", "src/win/loop-watcher.c", "src/win/pipe.c", "src/win/thread.c", "src/win/poll.c", "src/win/process.c", "src/win/process-stdio.c", "src/win/signal.c", "src/win/snprintf.c", "src/win/stream.c", "src/win/tcp.c", "src/win/tty.c", "src/win/udp.c", "src/win/util.c", "src/win/winapi.c", "src/win/winsock.c" }, .flags = flags.items });
    } else {
        lib.root_module.addCSourceFiles(.{ .files = &.{
            "src/unix/async.c",
            "src/unix/core.c",
            "src/unix/dl.c",
            "src/unix/fs.c",
            "src/unix/getaddrinfo.c",
            "src/unix/getnameinfo.c",
            "src/unix/loop-watcher.c",
            "src/unix/loop.c",
            "src/unix/pipe.c",
            "src/unix/poll.c",
            "src/unix/process.c",
            "src/unix/random-devurandom.c",
            "src/unix/signal.c",
            "src/unix/stream.c",
            "src/unix/tcp.c",
            "src/unix/thread.c",
            "src/unix/tty.c",
            "src/unix/udp.c",
        }, .flags = flags.items });
    }

    if (t.os.tag == .linux or targetIsDarwin(t)) {
        lib.root_module.addCSourceFiles(.{ .files = &.{
            "src/unix/proctitle.c",
        }, .flags = flags.items });
    }

    if (t.os.tag == .linux) {
        lib.root_module.addCSourceFiles(.{ .files = &.{
            "src/unix/linux.c",
            "src/unix/procfs-exepath.c",
            "src/unix/random-getrandom.c",
            "src/unix/random-sysctl-linux.c",
        }, .flags = flags.items });
    }

    if (targetIsDarwin(t) or
        targetIsBSD(t))
    {
        lib.root_module.addCSourceFiles(.{ .files = &.{
            "src/unix/bsd-ifaddrs.c",
            "src/unix/kqueue.c",
        }, .flags = flags.items });
    }

    if (targetIsDarwin(t) or t.os.tag == .openbsd) {
        lib.root_module.addCSourceFiles(.{ .files = &.{
            "src/unix/random-getentropy.c",
        }, .flags = flags.items });
    }

    if (targetIsDarwin(t)) {
        lib.root_module.addCSourceFiles(.{ .files = &.{
            "src/unix/darwin-proctitle.c",
            "src/unix/darwin.c",
            "src/unix/fsevents.c",
        }, .flags = flags.items });
    }

    lib.root_module.addIncludePath(b.path("src"));
    lib.root_module.addIncludePath(b.path("include"));
    if (t.os.tag == .windows) {
        lib.root_module.linkSystemLibrary("psapi", .{});
        lib.root_module.linkSystemLibrary("user32", .{});
        lib.root_module.linkSystemLibrary("advapi32", .{});
        lib.root_module.linkSystemLibrary("iphlpapi", .{});
        lib.root_module.linkSystemLibrary("userenv", .{});
        lib.root_module.linkSystemLibrary("ws2_32", .{});
        lib.root_module.linkSystemLibrary("dbghelp", .{});
        lib.root_module.linkSystemLibrary("ole32", .{});
        lib.root_module.linkSystemLibrary("uuid", .{});
    }
    lib.root_module.link_libc = true;

    lib.installHeadersDirectory(b.path("include/uv"), "uv", .{});
    lib.installHeader(b.path("include/uv.h"), "uv.h");

    b.installArtifact(lib);
}
