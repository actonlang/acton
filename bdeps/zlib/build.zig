const std = @import("std");

pub fn build(b: *std.Build) void {
    const optimize = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});

    // Upstream zlib source, fetched via build.zig.zon.
    const src = b.dependency("zlib_src", .{});

    const lib = b.addLibrary(.{
        .name = "z",
        .linkage = .static,
        .root_module = b.createModule(.{
            .target = target,
            .optimize = optimize,
        }),
    });

    const source_files = [_][]const u8{
        "adler32.c",
        "compress.c",
        "crc32.c",
        "deflate.c",
        "gzclose.c",
        "gzlib.c",
        "gzread.c",
        "gzwrite.c",
        "infback.c",
        "inffast.c",
        "inflate.c",
        "inftrees.c",
        "trees.c",
        "uncompr.c",
        "zutil.c",
    };

    lib.root_module.addCSourceFiles(.{
        .root = src.path("."),
        .files = &source_files,
        .flags = &[_][]const u8{
            "-DHAVE_HIDDEN",
            "-D_LARGEFILE64_SOURCE=1",
        },
    });
    lib.root_module.link_libc = true;

    lib.installHeader(src.path("zlib.h"), "zlib.h");
    lib.installHeader(src.path("zconf.h"), "zconf.h");

    b.installArtifact(lib);
}
