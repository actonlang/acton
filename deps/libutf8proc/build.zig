const std = @import("std");
const print = @import("std").debug.print;

pub fn build(b: *std.Build) void {
    const optimize = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});

    const build_shared_libs = b.option(bool, "BUILD_SHARED_LIBS",
                "Build shared libraries (otherwise static ones)") orelse true;

    const linkage: std.builtin.LinkMode = if (build_shared_libs) .dynamic else .static;
    const lib = b.addLibrary(.{
        .name = "utf8proc",
        .linkage = linkage,
        .root_module = b.createModule(.{
            .target = target,
            .optimize = optimize,
        }),
    });

    var flags = std.ArrayList([]const u8).empty;
    defer flags.deinit(b.allocator);

    if (!build_shared_libs) {
        flags.append(b.allocator, "-DUTF8PROC_STATIC") catch unreachable;
    }

    lib.root_module.addCSourceFile(.{
        .file = b.path("utf8proc.c"),
        .flags = flags.items
    });
    lib.root_module.link_libc = true;
    lib.installHeader(b.path("utf8proc.h"), "utf8proc.h");
    b.installArtifact(lib);
}
