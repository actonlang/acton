const std = @import("std");
const print = @import("std").debug.print;
const ArrayList = std.ArrayList;

pub fn build(b: *std.Build) void {
    const optimize = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});
    const no_threads = b.option(bool, "no_threads", "") orelse false;
    const syspath_include = b.option([]const u8, "syspath_include", "") orelse "";

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

    var flags = std.ArrayList([]const u8).init(b.allocator);
    defer flags.deinit();
    flags.append("-fno-sanitize=undefined") catch unreachable;

    var file_prefix_map = std.ArrayList(u8).init(b.allocator);
    defer file_prefix_map.deinit();
    const file_prefix_path = b.build_root.handle.openDir("..", .{}) catch unreachable;
    const file_prefix_path_path = file_prefix_path.realpathAlloc(b.allocator, ".") catch unreachable;
    file_prefix_map.appendSlice("-ffile-prefix-map=") catch unreachable;
    file_prefix_map.appendSlice(file_prefix_path_path) catch unreachable;
    file_prefix_map.appendSlice("/=") catch unreachable;
    flags.append(file_prefix_map.items) catch unreachable;

    if (no_threads) {
        print("No threads\n", .{});
    } else {
        print("Threads enabled\n", .{});
        flags.appendSlice(&.{
            "-DACTON_THREADS",
        }) catch |err| {
            std.log.err("Error appending flags: {}", .{err});
            std.posix.exit(1);
        };
    }

    const libactondb = b.addStaticLibrary(.{
        .name = "ActonDB",
        .target = target,
        .optimize = optimize,
    });
    libactondb.addCSourceFiles(.{
        .files = &libactondb_sources,
        .flags = flags.items
    });
    libactondb.defineCMacro("LOG_USER_COLOR", "");
    libactondb.addIncludePath(.{ .cwd_relative= syspath_include });
    libactondb.linkLibrary(dep_libgc.artifact("gc"));
    libactondb.linkLibrary(dep_libprotobuf_c.artifact("protobuf-c"));
    libactondb.linkLibrary(dep_libuuid.artifact("uuid"));
    libactondb.linkLibrary(dep_libuuid.artifact("uuid"));
    libactondb.linkLibC();
    libactondb.linkLibCpp();
    b.installArtifact(libactondb);

    const actondb = b.addExecutable(.{
        .name = "actondb",
        .target = target,
        .optimize = optimize,
    });
    actondb.addCSourceFile(.{ .file = b.path("actondb.c"), .flags = &[_][]const u8{
        "-fno-sanitize=undefined",
    }});
    actondb.addCSourceFile(.{ .file = b.path("log.c"), .flags = flags.items });
    actondb.addIncludePath(.{ .cwd_relative= syspath_include });
    actondb.addLibraryPath(b.path("../lib"));
    actondb.linkLibrary(libactondb);
    actondb.linkLibrary(dep_libargp.artifact("argp"));
    actondb.linkLibrary(dep_libnetstring.artifact("netstring"));
    actondb.linkLibrary(dep_libyyjson.artifact("yyjson"));
    actondb.linkLibC();
    actondb.linkLibCpp();
    b.installArtifact(actondb);
}
