const std = @import("std");
const print = @import("std").debug.print;

pub fn build(b: *std.Build) void {
    const optimize = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});

    const lib = b.addStaticLibrary(.{
        .name = "netstring",
        .target = target,
        .optimize = optimize,
    });

    const source_files = [_][]const u8{
        "netstring.c",
    };

    lib.addCSourceFiles(.{
        .files = &source_files,
        .flags = &[_][]const u8{}
    });
    lib.linkLibC();
    lib.installHeader(b.path("netstring.h"), "netstring.h");
    b.installArtifact(lib);
}
