const std = @import("std");
const print = @import("std").debug.print;

pub fn build(b: *std.Build) void {
    const optimize = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});

    const lib = b.addStaticLibrary(.{
        .name = "asprintf",
        .target = target,
        .optimize = optimize,
    });

    const source_files = [_][]const u8{
        "asprintf.c",
    };

    lib.addCSourceFiles(.{
        .files = &source_files,
        .flags = &[_][]const u8{}
    });
    lib.linkLibC();
    b.installFile("asprintf.h", "include/asprintf.h");
    b.installArtifact(lib);
}
