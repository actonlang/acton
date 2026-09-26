const std = @import("std");
const print = @import("std").debug.print;
const ArrayList = std.ArrayList;

pub fn build(b: *std.Build) void {
    const optimize = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});
    const enable_lto = optimize != .Debug and target.result.os.tag != .macos;
    const no_threads = b.option(bool, "no_threads", "") orelse false;
    const only_actondb = b.option(bool, "only_actondb", "") orelse false;
    const gc_use_mark_bits = b.option(bool, "gc_use_mark_bits", "Use packed GC mark bits") orelse false;
    const gc_mark_bit_per_object = b.option(bool, "gc_mark_bit_per_object", "Track GC marks per object") orelse false;
    const gc_dirty_tracking_backend = b.option([]const u8, "gc_dirty_tracking_backend", "GC dirty tracking backend: auto, soft_dirty, userfaultfd") orelse "auto";
    const gc_page_hash_table_log2 = b.option(u8, "gc_page_hash_table_log2", "Log2 of GC page-hash entries (0 keeps the default)") orelse 0;
    const gc_heap_growth_divisor = b.option(u32, "gc_heap_growth_divisor", "Limit automatic GC heap growth to the heap size divided by this (0 keeps the fixed increment)") orelse 0;
    const gc_alloc_budget_percent = b.option(u32, "gc_alloc_budget_percent", "Collect after allocating this percentage of the live data (0 keeps the free space divisor policy)") orelse 0;
    const gc_block_size = b.option(u32, "gc_block_size", "GC heap block size in bytes: a power of two from 4096 to 65536 (0 keeps the default)") orelse 0;
    const gc_mark_range_stealing = b.option(bool, "gc_mark_range_stealing", "Let parallel GC markers claim ranges of the global mark stack") orelse false;
    const gc_initial_mark_stack_size = b.option(u32, "gc_initial_mark_stack_size", "Initial number of GC mark stack entries: a power of two, 4096 at least (0 keeps the default)") orelse 0;
    const gc_no_end_padding = b.option(bool, "gc_no_end_padding", "Do not pad GC objects by a byte to keep them alive through pointers just past their end") orelse false;
    const gc_thread_local_size_limit = b.option(u32, "gc_thread_local_size_limit", "Largest GC object size in bytes served from thread-local free lists: a multiple of 16 up to half the block size (0 keeps the default)") orelse 0;

    const dep_libargp = b.dependency("libargp", .{
        .target = target,
        .optimize = optimize,
    });

    // Must match the collector options in base/build.zig, so that both
    // resolve to the same libgc.
    const dep_libgc = b.dependency("libgc", .{
        .target = target,
        .optimize = optimize,
        .linkage = .static,
        .enable_threads = !target.result.cpu.arch.isWasm(),
        .enable_large_config = true,
        .enable_mmap = true,
        .enable_mark_bits = gc_use_mark_bits,
        .enable_mark_bit_per_obj = gc_mark_bit_per_object,
        .dirty_tracking_backend = gc_dirty_tracking_backend,
        .page_hash_table_log2 = gc_page_hash_table_log2,
        .heap_growth_divisor = gc_heap_growth_divisor,
        .alloc_budget_percent = gc_alloc_budget_percent,
        .block_size = gc_block_size,
        .enable_mark_range_stealing = gc_mark_range_stealing,
        .initial_mark_stack_size = gc_initial_mark_stack_size,
        .enable_end_padding = !gc_no_end_padding,
        .tiny_freelists = gcTinyFreelists(target.result, gc_thread_local_size_limit),
        .enable_mprotect_vdb = !(target.result.os.tag.isDarwin() and target.result.cpu.arch == .x86_64),
    });
    const libgc = dep_libgc.artifact("gc");
    if (enable_lto) libgc.lto = .thin;

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
    if (enable_lto) libactondb.lto = .thin;
    libactondb.root_module.addCSourceFiles(.{
        .files = &libactondb_sources,
        .flags = flags.items
    });
    libactondb.root_module.addCMacro("LOG_USER_COLOR", "");
    libactondb.root_module.linkLibrary(libgc);
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
    if (enable_lto) actondb.lto = .thin;
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

// Same as gcTinyFreelists in base/build.zig.
fn gcTinyFreelists(t: std.Target, size_limit: u32) u32 {
    if (size_limit == 0) return 0;
    return size_limit / (2 * (t.ptrBitWidth() / 8)) + 1;
}
