const std = @import("std");
const print = @import("std").debug.print;

pub fn build(b: *std.build.Builder) void {
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

    lib.addCSourceFiles(&source_files, &[_][]const u8{});
    lib.linkLibC();
    b.installFile("netstring.h", "include/netstring.h");
    b.installArtifact(lib);
}
