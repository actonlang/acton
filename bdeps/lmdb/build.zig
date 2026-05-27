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

    const source = b.dependency("source", .{});

    const lib = b.addLibrary(.{
        .name = "lmdb",
        .linkage = .static,
        .root_module = b.createModule(.{
            .target = target,
            .optimize = optimize,
        }),
    });

    lib.root_module.link_libc = true;
    lib.root_module.addIncludePath(source.path("libraries/liblmdb"));
    const c_flags = [_][]const u8{"-DMDB_MAXKEYSIZE=512"};
    lib.root_module.addCSourceFile(.{ .file = source.path("libraries/liblmdb/mdb.c"), .flags = &c_flags });
    lib.root_module.addCSourceFile(.{ .file = source.path("libraries/liblmdb/midl.c"), .flags = &c_flags });
    lib.installHeader(source.path("libraries/liblmdb/lmdb.h"), "lmdb.h");

    b.installArtifact(lib);
}
