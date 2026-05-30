const std = @import("std");

// Builds a minimal static libtinfo.a: just enough of the terminfo API to
// satisfy the symbols GHC's `terminfo` boot package pulls into the acton
// compiler link. The implementation lives in tinfo.c; the capability
// name->index tables are generated from the upstream ncurses Caps file so the
// indices match compiled terminfo databases. We do not build the rest of
// ncurses (curses/screen handling) since none of it is linked.
pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const src = b.dependency("ncurses_src", .{});

    // caps_table.h: built and run on the host, reading include/Caps.
    const gencaps = b.addExecutable(.{
        .name = "gencaps",
        .root_module = b.createModule(.{
            .target = b.graph.host,
            .optimize = .ReleaseSafe,
        }),
    });
    gencaps.root_module.link_libc = true;
    gencaps.root_module.addCSourceFile(.{ .file = b.path("gencaps.c") });
    const run = b.addRunArtifact(gencaps);
    run.addFileArg(src.path("include/Caps"));
    const caps_h = run.captureStdOut(.{});

    const gen = b.addWriteFiles();
    _ = gen.addCopyFile(caps_h, "caps_table.h");

    const lib = b.addLibrary(.{
        .linkage = .static,
        .name = "tinfo",
        .root_module = b.createModule(.{
            .target = target,
            .optimize = optimize,
        }),
    });
    const mod = lib.root_module;
    mod.link_libc = true;
    mod.addIncludePath(gen.getDirectory());
    mod.addCSourceFile(.{
        .file = b.path("tinfo.c"),
        .flags = &.{ "-std=c11", "-Wall" },
    });

    b.installArtifact(lib);
}
