// Acton Base System Builder
// Performs the final build of the Acton base system by compiling the generated C code.

const std = @import("std");
const print = @import("std").debug.print;
const ArrayList = std.ArrayList;

pub const FilePath = struct {
    filename: []const u8,
    full_path: []const u8,
    dir: []const u8,
    file_path: []const u8,
};

// We have an absolute path we want to get to, but we have to provide it as a
// relative path from the current position. The easiest way to do this is to go
// up the directory tree until we're at the root, and then the absolute path is
// relative to the root and can be used. It would be more elegant to figure out
// if there are actual commonalities between the paths and only traverse
// upwards as far as necessary.

fn joinPath(allocator: std.mem.Allocator, base: []const u8, relative: []const u8) []const u8 {
    const path = allocator.alloc(u8, base.len + relative.len + 1) catch @panic("OOM");
    _ = std.fmt.bufPrint(path, "{s}/{s}", .{base, relative}) catch @panic("Error joining paths");
    return path;
}

// The ACTON_GC_REQUIRED_VDB value (a GC_VDB_* constant, or 0 for automatic
// selection) for gc_dirty_tracking_backend, which must be supported by target.
fn gcRequiredVdb(t: std.Target, backend: []const u8) u8 {
    const required_vdb: u8 = if (std.mem.eql(u8, backend, "auto")) 0
        else if (std.mem.eql(u8, backend, "soft_dirty")) 0x40
        else if (std.mem.eql(u8, backend, "userfaultfd")) 0x80
        else {
            std.log.err("gc_dirty_tracking_backend must be auto, soft_dirty or userfaultfd", .{});
            std.process.exit(1);
        };
    if (required_vdb != 0 and (t.os.tag != .linux or !t.abi.isGnu())) {
        std.log.err("gc_dirty_tracking_backend={s} requires a Linux GNU target", .{backend});
        std.process.exit(1);
    }
    if (required_vdb == 0x80) {
        const supported_arch = switch (t.cpu.arch) {
            .x86, .x86_64, .aarch64 => true,
            else => false,
        };
        const glibc_version = t.os.versionRange().gnuLibCVersion() orelse
            std.SemanticVersion{ .major = 0, .minor = 0, .patch = 0 };
        if (!supported_arch or glibc_version.order(.{ .major = 2, .minor = 34, .patch = 0 }) == .lt) {
            std.log.err("gc_dirty_tracking_backend=userfaultfd requires x86, x86_64 or aarch64 and glibc 2.34 or newer", .{});
            std.process.exit(1);
        }
    }
    return required_vdb;
}

// The C compiler flags of the collector sources in lib.
fn cSourceFlags(lib: *std.Build.Step.Compile) []const []const u8 {
    for (lib.root_module.link_objects.items) |link_object| {
        switch (link_object) {
            .c_source_files => |c_source_files| return c_source_files.flags,
            else => {},
        }
    }
    @panic("no C sources in libgc");
}

pub fn build(b: *std.Build) void {
    const io = b.graph.io;
    const buildroot_path = b.build_root.join(b.allocator, &.{}) catch unreachable;
    const optimize = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});
    const enable_lto = optimize != .Debug and target.result.os.tag != .macos;
    const cpedantic = b.option(bool, "cpedantic", "") orelse false;
    const use_db = b.option(bool, "db", "") orelse false;
    const no_threads = b.option(bool, "no_threads", "") orelse false;
    const gc_use_mark_bits = b.option(bool, "gc_use_mark_bits", "Use packed GC mark bits") orelse false;
    const gc_mark_bit_per_object = b.option(bool, "gc_mark_bit_per_object", "Track GC marks per object") orelse false;
    const gc_dirty_tracking_backend = b.option([]const u8, "gc_dirty_tracking_backend", "GC dirty tracking backend: auto, soft_dirty, userfaultfd") orelse "auto";
    const gc_page_hash_table_log2 = b.option(u8, "gc_page_hash_table_log2", "Log2 of GC page-hash entries (0 keeps the default)") orelse 0;
    const gc_disable_thp = b.option(bool, "gc_disable_thp", "Disable transparent huge pages for GC memory on Linux") orelse false;

    if (gc_disable_thp and target.result.os.tag != .linux) {
        std.log.err("gc_disable_thp requires a Linux target", .{});
        std.process.exit(1);
    }
    // Validate here, before the collector's own option checks, so that the
    // diagnostics name the Build.act options.
    const gc_required_vdb = gcRequiredVdb(target.result, gc_dirty_tracking_backend);
    if (gc_page_hash_table_log2 > 30) {
        std.log.err("gc_page_hash_table_log2 must be between 1 and 30, or 0 for the default", .{});
        std.process.exit(1);
    }
    // Must match the collector options in backend/build.zig, so that both
    // resolve to the same libgc.
    const gc_enable_threads = !target.result.cpu.arch.isWasm();
    const gc_enable_mprotect_vdb = gcEnableMprotectVdb(target.result);

    const projpath_outtypes = joinPath(b.allocator, buildroot_path, "out/types");

    print("Acton Base Builder\nBuilding in {s}\n", .{buildroot_path});

    const dep_libbsdnt = b.dependency("libbsdnt", .{
        .target = target,
        .optimize = optimize,
    });

    const dep_libgc = b.dependency("libgc", .{
        .target = target,
        .optimize = optimize,
        .linkage = .static,
        .enable_threads = gc_enable_threads,
        .enable_large_config = true,
        .enable_mmap = true,
        .enable_mark_bits = gc_use_mark_bits,
        .enable_mark_bit_per_obj = gc_mark_bit_per_object,
        .dirty_tracking_backend = gc_dirty_tracking_backend,
        .page_hash_table_log2 = gc_page_hash_table_log2,
        .enable_mprotect_vdb = gc_enable_mprotect_vdb,
    });
    const libgc = dep_libgc.artifact("gc");
    if (enable_lto) libgc.lto = .thin;

    // acton_gc_config.h describes the collector configuration to the RTS and
    // to C extensions.
    const gc_config_files = b.addWriteFiles();
    const gc_config_h = gc_config_files.add("acton_gc_config.h", b.fmt(
        \\#ifndef ACTON_GC_CONFIG_H
        \\#define ACTON_GC_CONFIG_H
        \\#include <gc.h>
        \\#define ACTON_GC_DIRTY_TRACKING_BACKEND "{s}"
        \\#define ACTON_GC_REQUIRED_VDB {d}
        \\#define ACTON_GC_THREADS {d}
        \\#ifdef __cplusplus
        \\extern "C" {{
        \\#endif
        \\GC_API unsigned GC_CALL acton_gc_get_page_hash_table_log2(void);
        \\#ifdef __cplusplus
        \\}}
        \\#endif
        \\#endif
        \\
    , .{ gc_dirty_tracking_backend, gc_required_vdb, @intFromBool(gc_enable_threads) }));
    // The effective page-hash size depends on the collector defaults and its
    // configuration macros, so read it from the collector's private header,
    // compiled with the C flags of libgc itself.
    const gc_config = b.addObject(.{
        .name = "acton_gc_config",
        .root_module = b.createModule(.{
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    if (enable_lto) gc_config.lto = .thin;
    gc_config.root_module.addCSourceFile(.{
        .file = gc_config_files.add("acton_gc_config.c",
            \\#include "private/gc_priv.h"
            \\GC_API unsigned GC_CALL acton_gc_get_page_hash_table_log2(void) {
            \\    return LOG_PHT_ENTRIES;
            \\}
            \\
        ),
        .flags = cSourceFlags(libgc),
    });
    gc_config.root_module.addIncludePath(dep_libgc.path("include"));

    const dep_libmbedtls = b.dependency("libmbedtls", .{
        .target = target,
        .optimize = optimize,
    });

    const dep_libnetstring = b.dependency("libnetstring", .{
        .target = target,
        .optimize = optimize,
    });

    const dep_libpcre2 = b.dependency("libpcre2", .{
        .target = target,
        .optimize = optimize,
        .linkage = .static,
    });

    const dep_libsnappy_c = b.dependency("libsnappy", .{
        .target = target,
        .optimize = optimize,
    });

    const dep_libtlsuv = b.dependency("libtlsuv", .{
        .target = target,
        .optimize = optimize,
        .http = false,
        .keychain = false,
    });

    const dep_libprotobuf_c = b.dependency("libprotobuf_c", .{
        .target = target,
        .optimize = optimize,
    });

    const dep_libutf8proc = b.dependency("libutf8proc", .{
        .target = target,
        .optimize = optimize,
        .BUILD_SHARED_LIBS = false,
    });

    const dep_libuv = b.dependency("libuv", .{
        .target = target,
        .optimize = optimize,
    });

    const dep_libxml2 = b.dependency("libxml2", .{
        .target = target,
        .optimize = optimize,
    });

    const dep_libyyjson = b.dependency("libyyjson", .{
        .target = target,
        .optimize = optimize,
    });

    var iter_dir = b.build_root.handle.openDir(
        io,
        "out/types/",
        .{
            .iterate = true
        },
    ) catch |err| {
        std.log.err("Error opening iterable dir: {}", .{err});
        std.process.exit(1);
    };

    var c_files = ArrayList([]const u8).empty;
    var root_c_files = ArrayList(*FilePath).empty;
    defer c_files.deinit(b.allocator);
    defer root_c_files.deinit(b.allocator);
    var walker = iter_dir.walk(b.allocator) catch |err| {
        std.log.err("Error walking dir: {}", .{err});
        std.process.exit(1);
    };
    defer walker.deinit();

    // Find all .c files
    while (true) {
        const next_result = walker.next(io) catch |err| {
            std.log.err("Error getting next: {}", .{err});
            std.process.exit(1);
        };
        if (next_result) |entry| {
            if (entry.kind == .file) {
                if (std.mem.endsWith(u8, entry.basename, ".c")) {
                    const fPath = b.allocator.create(FilePath) catch |err| {
                        std.log.err("Error allocating FilePath entry: {}", .{err});
                        std.process.exit(1);
                    };
                    const full_path = joinPath(b.allocator, projpath_outtypes, entry.path);
                    const entry_dir_rel = std.fs.path.dirname(entry.path) orelse ".";
                    const dir = joinPath(b.allocator, projpath_outtypes, entry_dir_rel);
                    fPath.full_path = full_path;
                    fPath.dir = dir;
                    fPath.filename = b.allocator.dupe(u8, entry.basename) catch |err| {
                        std.log.err("Error allocating filename entry: {}", .{err});
                        std.process.exit(1);
                    };
                    const file_path = b.allocator.alloc(u8, entry.path.len + 1) catch |err| {
                        std.log.err("Error allocating file_path entry: {}", .{err});
                        std.process.exit(1);
                    };
                    file_path[0] = '/';
                    @memcpy(file_path[1..], entry.path);
                    fPath.file_path = file_path;

                    print("-- filename : {s}\n", .{fPath.filename});
                    print("   full_path: {s}\n", .{fPath.full_path});
                    print("   dir      : {s}\n", .{fPath.dir});
                    print("   file_path: {s}\n", .{fPath.file_path});

                    if (std.mem.endsWith(u8, entry.basename, ".root.c")) {
                        root_c_files.append(b.allocator, fPath) catch |err| {
                            std.log.err("Error appending to root .c files: {}", .{err});
                            std.process.exit(1);
                        };
                    } else {
                        // Store relative path from build root, not absolute path
                        const rel_path = b.allocator.alloc(u8, 9 + fPath.file_path.len) catch |err| {
                            std.log.err("Error allocating relative path: {}", .{err});
                            std.process.exit(1);
                        };
                        @memcpy(rel_path[0..9], "out/types");
                        @memcpy(rel_path[9..], fPath.file_path);
                        c_files.append(b.allocator, rel_path) catch |err| {
                            std.log.err("Error appending to .c files: {}", .{err});
                            std.process.exit(1);
                        };
                    }
                }
            }
        } else {
            break;
        }
    }

    var flags = std.ArrayList([]const u8).empty;
    defer flags.deinit(b.allocator);
    flags.append(b.allocator, "-DUTF8PROC_STATIC") catch unreachable;

    var file_prefix_map = std.ArrayList(u8).empty;
    defer file_prefix_map.deinit(b.allocator);
    const file_prefix_path_path = std.fs.path.dirname(buildroot_path) orelse buildroot_path;
    file_prefix_map.appendSlice(b.allocator, "-ffile-prefix-map=") catch unreachable;
    file_prefix_map.appendSlice(b.allocator, file_prefix_path_path) catch unreachable;
    file_prefix_map.appendSlice(b.allocator, "/=") catch unreachable;
    flags.append(b.allocator, file_prefix_map.items) catch unreachable;
    flags.append(b.allocator, "-Wno-error=parentheses-equality") catch unreachable;
    flags.append(b.allocator, "-Wno-parentheses-equality") catch unreachable;
    if (cpedantic) {
        flags.append(b.allocator, "-Werror") catch unreachable;
    }

    if (optimize == .Debug) {
        print("Debug build\n", .{});
        flags.appendSlice(b.allocator, &.{
            "-DDEV",
        }) catch |err| {
            std.log.err("Error appending flags: {}", .{err});
            std.process.exit(1);
        };
    }

    if (gc_disable_thp) {
        flags.append(b.allocator, "-DACTON_GC_DISABLE_THP") catch unreachable;
    }

    if (use_db) {
        print("Building with DB backend support\n", .{});
        flags.append(b.allocator, "-DACTON_DB") catch unreachable;
    }

    if (no_threads) {
        print("No threads\n", .{});
    } else {
        print("Threads enabled\n", .{});
        flags.appendSlice(b.allocator, &.{
            "-DACTON_THREADS",
        }) catch |err| {
            std.log.err("Error appending flags: {}", .{err});
            std.process.exit(1);
        };
    }

    flags.appendSlice(b.allocator, &.{
        "-fwrapv",
    }) catch unreachable;

    const libActon = b.addLibrary(.{
        .name = "Acton",
        .linkage = .static,
        .root_module = b.createModule(.{
            .root_source_file = b.path("__root.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    if (enable_lto) libActon.lto = .thin;
    for (c_files.items) |entry| {
        libActon.root_module.addCSourceFile(.{ .file = b.path(entry), .flags = flags.items });
    }
    libActon.installHeadersDirectory(b.path("builtin"), "builtin", .{});

    // TODO: We should install out/types/*.h with installHeadersDirectory, but
    // it's not working, we get cache issue where out-of-date files are being
    // used. It works the first time but changes to the headers are often not
    // picked up. Iterating and installing each file separately seems to work.
    // Rather surprisingly, I don't see the same issue with the header files
    // from builtin above, but if errors were to occur, we could do the same
    // for those files as well. Obviously, this is not ideal and we should
    // investigate further, find the root cause and address it.
    libActon.installHeadersDirectory(b.path("out/types"), "out/types", .{});

    var hiter_dir = b.build_root.handle.openDir(io, "out/types/", .{ .iterate = true }) catch unreachable;
    var hwalker = hiter_dir.walk(b.allocator) catch unreachable;
    defer hwalker.deinit();

    // Find all .h files
    while (true) {
        const next_result = hwalker.next(io) catch unreachable;
        if (next_result) |entry| {
            if (entry.kind == .file) {
                if (std.mem.endsWith(u8, entry.basename, ".h")) {
                    const file_path = std.fs.path.join(b.allocator, &.{ "out/types", entry.path }) catch unreachable;
                    libActon.installHeader(b.path(file_path), file_path);
                }
            }
        } else {
            break;
        }
    }


    libActon.installHeader(b.path("rts/common.h"), "rts/common.h");
    libActon.installHeader(b.path("rts/q.h"), "rts/q.h");
    libActon.installHeader(b.path("rts/rts.h"), "rts/rts.h");
    libActon.installHeader(b.path("rts/log.h"), "rts/log.h");
    libActon.installHeader(b.path("rts/perf.h"), "rts/perf.h");

    libActon.root_module.addIncludePath(.{ .cwd_relative = buildroot_path });
    libActon.root_module.addIncludePath(dep_libtlsuv.path("include"));
    libActon.root_module.addIncludePath(gc_config_files.getDirectory());
    libActon.root_module.addObject(gc_config);
    libActon.installHeader(gc_config_h, "acton_gc_config.h");

    if (use_db) {
        const libactondb_dep = b.dependency("actondb", .{
            .target = target,
            .optimize = optimize,
            .gc_use_mark_bits = gc_use_mark_bits,
            .gc_mark_bit_per_object = gc_mark_bit_per_object,
            .gc_dirty_tracking_backend = gc_dirty_tracking_backend,
            .gc_page_hash_table_log2 = gc_page_hash_table_log2,
        });
        libActon.root_module.linkLibrary(libactondb_dep.artifact("ActonDB"));
    }

    libActon.root_module.linkLibrary(dep_libbsdnt.artifact("bsdnt"));
    libActon.root_module.linkLibrary(libgc);
    libActon.root_module.linkLibrary(dep_libmbedtls.artifact("mbedcrypto"));
    libActon.root_module.linkLibrary(dep_libmbedtls.artifact("mbedtls"));
    libActon.root_module.linkLibrary(dep_libmbedtls.artifact("mbedx509"));
    libActon.root_module.linkLibrary(dep_libnetstring.artifact("netstring"));
    libActon.root_module.linkLibrary(dep_libpcre2.artifact("pcre2-8"));
    libActon.root_module.linkLibrary(dep_libprotobuf_c.artifact("protobuf-c")); // TODO: remove, once telemetrify/prw is fixed
    libActon.root_module.linkLibrary(dep_libsnappy_c.artifact("snappy-c"));
    libActon.root_module.linkLibrary(dep_libtlsuv.artifact("tlsuv"));
    libActon.root_module.linkLibrary(dep_libutf8proc.artifact("utf8proc"));
    libActon.root_module.linkLibrary(dep_libuv.artifact("uv"));
    libActon.root_module.linkLibrary(dep_libxml2.artifact("xml2"));
    libActon.root_module.linkLibrary(dep_libyyjson.artifact("yyjson"));

    libActon.installLibraryHeaders(dep_libbsdnt.artifact("bsdnt"));
    libActon.installLibraryHeaders(libgc);
    libActon.installLibraryHeaders(dep_libprotobuf_c.artifact("protobuf-c")); // TODO: remove, once telemetrify/prw is fixed
    libActon.installLibraryHeaders(dep_libuv.artifact("uv"));

    libActon.root_module.link_libc = true;
    b.installArtifact(libActon);

    const base_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("acton.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    if (enable_lto) base_tests.lto = .thin;
    base_tests.root_module.addIncludePath(.{ .cwd_relative = buildroot_path });
    base_tests.root_module.linkLibrary(dep_libbsdnt.artifact("bsdnt"));
    base_tests.root_module.linkLibrary(libgc);
    base_tests.root_module.link_libc = true;
    const run_base_tests = b.addRunArtifact(base_tests);
    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&run_base_tests.step);
}

// x86_64 macOS builds leave out mprotect-based dirty tracking. Under
// Rosetta 2, incremental collection with it hangs when 12 or more threads
// run; it is untested on Intel Macs. Incremental mode then treats every
// page as dirty, as all macOS builds did before.
pub fn gcEnableMprotectVdb(t: std.Target) bool {
    return !(t.os.tag.isDarwin() and t.cpu.arch == .x86_64);
}
