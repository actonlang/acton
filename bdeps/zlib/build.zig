const std = @import("std");

pub fn build(b: *std.Build) void {
    const optimize = b.standardOptimizeOption(.{});
    var target = b.standardTargetOptions(.{});

    // On a native macOS build zig defaults the deployment target to the build
    // host's SDK version. That tags the archive with whatever the build machine
    // happens to run and makes ld64 warn "object built for newer macOS version"
    // when GHC later links acton against an older minos. Pin a conservative floor
    // so the archive is reproducible across hosts and links cleanly. An explicit
    // -Dtarget=...-macos.<ver> still wins.
    if (target.result.os.tag == .macos and target.query.os_version_min == null) {
        var query = target.query;
        query.os_version_min = .{ .semver = .{ .major = 11, .minor = 0, .patch = 0 } };
        target = b.resolveTargetQuery(query);
    }

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
