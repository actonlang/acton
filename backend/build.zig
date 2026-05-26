const std = @import("std");
const print = @import("std").debug.print;
const ArrayList = std.ArrayList;

pub fn build(b: *std.Build) void {
    const optimize = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});
    const no_threads = b.option(bool, "no_threads", "") orelse false;
    const only_actondb = b.option(bool, "only_actondb", "") orelse false;

    const dep_libargp = b.dependency("libargp", .{
        .target = target,
        .optimize = optimize,
    });

    const dep_libgc = b.dependency("libgc", .{
        .target = target,
        .optimize = optimize,
        .BUILD_SHARED_LIBS = false,
        .enable_large_config = true,
        .enable_mmap = true,
    });

    const dep_libnetstring = b.dependency("libnetstring", .{
        .target = target,
        .optimize = optimize,
    });

    const dep_libprotobuf_c = b.dependency("libprotobuf_c", .{
        .target = target,
        .optimize = optimize,
    });

    const dep_libuuid = b.dependency("libuuid", .{
        .target = target,
        .optimize = optimize,
    });

    const dep_libyyjson = b.dependency("libyyjson", .{
        .target = target,
        .optimize = optimize,
    });

    const libactondb_sources = [_][]const u8 {
        "comm.c",
        "hash_ring.c",
        "queue_callback.c",
        "db.c",
        "queue.c",
        "queue_groups.c",
//        "log.c",
        "skiplist.c",
        "txn_state.c",
        "txns.c",
        "client_api.c",
        "failure_detector/db_messages.pb-c.c",
        "failure_detector/cells.c",
        "failure_detector/db_queries.c",
        "failure_detector/fd.c",
        "failure_detector/vector_clock.c",
    };

    var flags = std.ArrayList([]const u8).empty;
    defer flags.deinit(b.allocator);
    flags.append(b.allocator, "-fno-sanitize=undefined") catch unreachable;

    var file_prefix_map = std.ArrayList(u8).empty;
    defer file_prefix_map.deinit(b.allocator);
    const buildroot_path = b.build_root.join(b.allocator, &.{}) catch unreachable;
    const file_prefix_path_path = std.fs.path.dirname(buildroot_path) orelse buildroot_path;
    file_prefix_map.appendSlice(b.allocator, "-ffile-prefix-map=") catch unreachable;
    file_prefix_map.appendSlice(b.allocator, file_prefix_path_path) catch unreachable;
    file_prefix_map.appendSlice(b.allocator, "/=") catch unreachable;
    flags.append(b.allocator, file_prefix_map.items) catch unreachable;

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

    const libactondb = b.addLibrary(.{
        .name = "ActonDB",
        .linkage = .static,
        .root_module = b.createModule(.{
            .target = target,
            .optimize = optimize,
        }),
    });
    libactondb.root_module.addCSourceFiles(.{
        .files = &libactondb_sources,
        .flags = flags.items
    });
    libactondb.root_module.addCMacro("LOG_USER_COLOR", "");
    libactondb.root_module.linkLibrary(dep_libgc.artifact("gc"));
    libactondb.root_module.linkLibrary(dep_libprotobuf_c.artifact("protobuf-c"));
    libactondb.root_module.linkLibrary(dep_libuuid.artifact("uuid"));
    libactondb.root_module.link_libc = true;
    libactondb.root_module.link_libcpp = true;
    libactondb.installLibraryHeaders(dep_libprotobuf_c.artifact("protobuf-c"));
    libactondb.installLibraryHeaders(dep_libuuid.artifact("uuid"));
    if (!only_actondb) {
        b.installArtifact(libactondb);
    }

    const actondb = b.addExecutable(.{
        .name = "actondb",
        .root_module = b.createModule(.{
            .target = target,
            .optimize = optimize,
        }),
    });
    actondb.root_module.addCSourceFile(.{ .file = b.path("actondb.c"), .flags = &[_][]const u8{
        "-fno-sanitize=undefined",
    }});
    actondb.root_module.addCSourceFile(.{ .file = b.path("log.c"), .flags = flags.items });
    actondb.root_module.linkLibrary(libactondb);
    actondb.root_module.linkLibrary(dep_libargp.artifact("argp"));
    actondb.root_module.linkLibrary(dep_libnetstring.artifact("netstring"));
    actondb.root_module.linkLibrary(dep_libprotobuf_c.artifact("protobuf-c"));
    actondb.root_module.linkLibrary(dep_libyyjson.artifact("yyjson"));
    actondb.root_module.linkLibrary(dep_libuuid.artifact("uuid"));
    actondb.root_module.link_libc = true;
    actondb.root_module.link_libcpp = true;
    b.installArtifact(actondb);
}
