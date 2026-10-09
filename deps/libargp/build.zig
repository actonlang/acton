const std = @import("std");
const print = @import("std").debug.print;

pub fn build(b: *std.Build) void {
    const optimize = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});
    const enable_lto = optimize != .debug and target.result.os.tag != .macos;
    const t = target.result;

    var flags = std.ArrayList([]const u8).empty;
    defer flags.deinit(b.allocator);

    flags.appendSlice(b.allocator, &.{ "-DHAVE_UNISTD_H", "-DUNUSED=" }) catch |err| {
        std.log.err("Error appending iterable dir: {}", .{err});
        std.process.exit(1);
    };

    if (t.os.tag.isDarwin() or t.os.tag == .windows) {
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
    if (enable_lto) lib.lto = .thin;

    lib.root_module.addCSourceFiles(.{ .files = &.{
        "argp-ba.c",
        "argp-eexst.c",
        "argp-fmtstream.c",
        "argp-help.c",
        "argp-parse.c",
        "argp-pv.c",
        "argp-pvh.c",
    }, .flags = flags.items });

    if (t.os.tag.isDarwin()) {
        lib.root_module.addCSourceFiles(.{ .files = &.{
            "strchrnul.c",
        }, .flags = flags.items });
    }

    lib.root_module.addIncludePath(b.path("."));
    lib.root_module.link_libc = true;

    lib.installHeader(b.path("argp.h"), "argp.h");
    b.installArtifact(lib);
}
