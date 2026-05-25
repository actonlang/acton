const std = @import("std");
const builtin = @import("builtin");
const print = @import("std").debug.print;

// Helper functions for Zig 0.13/0.14 compatibility
fn targetIsDarwin(t: std.Target) bool {
    const is_zig_0_14 = comptime builtin.zig_version.order(std.SemanticVersion.parse("0.14.0") catch unreachable) != .lt;
    if (is_zig_0_14) {
        return t.os.tag.isDarwin();
    } else {
        return t.isDarwin();
    }
}

pub fn build(b: *std.Build) void {
    const optimize = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});
    const t = target.result;

    var flags = std.ArrayList([]const u8).empty;
    defer flags.deinit(b.allocator);

    flags.appendSlice(b.allocator, &.{ "-DHAVE_UNISTD_H", "-DUNUSED=" }) catch |err| {
        std.log.err("Error appending iterable dir: {}", .{err});
        std.process.exit(1);
    };

    if (targetIsDarwin(t) or t.os.tag == .windows) {
        flags.appendSlice(b.allocator, &.{
            "-DHAVE_DECL_FPUTS_UNLOCKED=0",
            "-DHAVE_DECL_FPUTC_UNLOCKED=0",
            "-DHAVE_DECL_FWRITE_UNLOCKED=0",
            "-DHAVE_DECL_PROGRAM_INVOCATION_NAME=0",
        }) catch |err| {
            std.log.err("Error appending iterable dir: {}", .{err});
            std.process.exit(1);
        };
    }

    const lib = b.addLibrary(.{
        .name = "argp",
        .linkage = .static,
        .root_module = b.createModule(.{
            .target = target,
            .optimize = optimize,
        }),
    });

    lib.root_module.addCSourceFiles(.{ .files = &.{
        "argp-ba.c",
        "argp-eexst.c",
        "argp-fmtstream.c",
        "argp-help.c",
        "argp-parse.c",
        "argp-pv.c",
        "argp-pvh.c",
    }, .flags = flags.items });

    if (targetIsDarwin(t)) {
        lib.root_module.addCSourceFiles(.{ .files = &.{
            "strchrnul.c",
        }, .flags = flags.items });
    }

    lib.root_module.addIncludePath(b.path("."));
    lib.root_module.link_libc = true;

    lib.installHeader(b.path("argp.h"), "argp.h");
    b.installArtifact(lib);
}
