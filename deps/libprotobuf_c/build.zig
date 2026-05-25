const std = @import("std");
const print = @import("std").debug.print;

pub fn build(b: *std.Build) void {
    const optimize = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});

    const lib = b.addLibrary(.{
        .name = "protobuf-c",
        .linkage = .static,
        .root_module = b.createModule(.{
            .target = target,
            .optimize = optimize,
        }),
    });

    lib.root_module.addCSourceFiles(.{
        .files = &.{
            "protobuf-c/protobuf-c.c",
        },
        .flags = &[_][]const u8{
            "-fno-sanitize=undefined",
        }
    });
    lib.root_module.link_libc = true;
    lib.installHeader(b.path("protobuf-c/protobuf-c.h"), "protobuf-c/protobuf-c.h");
    b.installArtifact(lib);
}
