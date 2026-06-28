const std = @import("std");
const print = @import("std").debug.print;

pub fn build(b: *std.Build) void {
    const optimize = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});
    const enable_lto = optimize != .Debug and target.result.os.tag != .macos;

    const lib = b.addLibrary(.{
        .name = "yyjson",
        .linkage = .static,
        .root_module = b.createModule(.{
            .target = target,
            .optimize = optimize,
        }),
    });
    if (enable_lto) lib.lto = .full;

    const source_files = [_][]const u8{
        "yyjson.c",
    };

    lib.root_module.addCSourceFiles(.{
        .files = &source_files,
        .flags = &[_][]const u8{}
    });
    lib.root_module.link_libc = true;
    lib.installHeader(b.path("yyjson.h"), "yyjson.h");
    b.installArtifact(lib);
}
