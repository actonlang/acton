// Acton Standard Library Builder
// Builds the generated C code for the std project.

const std = @import("std");
const print = @import("std").debug.print;
const ArrayList = std.ArrayList;

pub const FilePath = struct {
    filename: []const u8,
    full_path: []const u8,
    dir: []const u8,
    file_path: []const u8,
};

fn joinPath(allocator: std.mem.Allocator, base: []const u8, relative: []const u8) []const u8 {
    const path = allocator.alloc(u8, base.len + relative.len + 1) catch @panic("OOM");
    _ = std.fmt.bufPrint(path, "{s}/{s}", .{base, relative}) catch @panic("Error joining paths");
    return path;
}

pub fn build(b: *std.Build) void {
    const io = b.graph.io;
    const buildroot_path = b.build_root.join(b.allocator, &.{}) catch unreachable;
    const optimize = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});
    const enable_lto = optimize != .Debug and target.result.os.tag != .macos;
    const db = b.option(bool, "db", "") orelse false;
    const no_threads = b.option(bool, "no_threads", "") orelse false;

    print("Acton Standard Library Builder\nBuilding in {s}\n", .{buildroot_path});

    const actonbase_dep = b.dependency("base", .{
        .target = target,
        .optimize = optimize,
        .no_threads = no_threads,
        .db = db,
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
    defer c_files.deinit(b.allocator);
    var walker = iter_dir.walk(b.allocator) catch |err| {
        std.log.err("Error walking dir: {}", .{err});
        std.process.exit(1);
    };
    defer walker.deinit();

    while (true) {
        const next_result = walker.next(io) catch |err| {
            std.log.err("Error getting next: {}", .{err});
            std.process.exit(1);
        };
        if (next_result) |entry| {
            if (entry.kind == .file and std.mem.endsWith(u8, entry.basename, ".c")) {
                if (std.mem.endsWith(u8, entry.basename, ".root.c")) continue;
                if (std.mem.endsWith(u8, entry.basename, ".test_root.c")) continue;

                const file_path = std.fs.path.join(b.allocator, &.{ "out/types", entry.path }) catch |err| {
                    std.log.err("Error allocating C source path: {}", .{err});
                    std.process.exit(1);
                };
                c_files.append(b.allocator, file_path) catch |err| {
                    std.log.err("Error appending C source path: {}", .{err});
                    std.process.exit(1);
                };
            }
        } else {
            break;
        }
    }

    if (c_files.items.len == 0) {
        const dummy_rel = "out/types/acton_empty.c";
        b.build_root.handle.createDirPath(io, "out/types") catch |err| {
            std.log.err("Error creating out/types directory: {}", .{err});
            std.process.exit(1);
        };
        const dummy_file = b.build_root.handle.createFile(io, dummy_rel, .{}) catch |err| {
            std.log.err("Error creating dummy C file: {}", .{err});
            std.process.exit(1);
        };
        var write_buffer: [64]u8 = undefined;
        var dummy_writer = dummy_file.writer(io, &write_buffer);
        dummy_writer.interface.writeAll("int acton_empty(void) { return 0; }\n") catch |err| switch (err) {
            error.WriteFailed => {
                std.log.err("Error writing dummy C file: {}", .{dummy_writer.err.?});
                std.process.exit(1);
            },
        };
        dummy_writer.flush() catch |err| {
            std.log.err("Error flushing dummy C file: {}", .{err});
            std.process.exit(1);
        };
        dummy_file.close(io);
        c_files.append(b.allocator, dummy_rel) catch |err| {
            std.log.err("Error appending dummy C file path: {}", .{err});
            std.process.exit(1);
        };
    }

    var flags = std.ArrayList([]const u8).empty;
    defer flags.deinit(b.allocator);

    var file_prefix_map = std.ArrayList(u8).empty;
    defer file_prefix_map.deinit(b.allocator);
    const file_prefix_path_path = std.fs.path.dirname(buildroot_path) orelse buildroot_path;
    file_prefix_map.appendSlice(b.allocator, "-ffile-prefix-map=") catch unreachable;
    file_prefix_map.appendSlice(b.allocator, file_prefix_path_path) catch unreachable;
    file_prefix_map.appendSlice(b.allocator, "/=") catch unreachable;
    flags.append(b.allocator, file_prefix_map.items) catch unreachable;

    if (optimize == .Debug) {
        print("Debug build\n", .{});
        flags.appendSlice(b.allocator, &.{
            "-DDEV",
        }) catch |err| {
            std.log.err("Error appending flags: {}", .{err});
            std.process.exit(1);
        };
    }

    if (db)
        flags.appendSlice(b.allocator, &.{"-DACTON_DB",}) catch unreachable;

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
        "-fno-sanitize=signed-integer-overflow",
    }) catch unreachable;

    const libActonProject = b.addLibrary(.{
        .name = "ActonProject",
        .linkage = .static,
        .root_module = b.createModule(.{
            .target = target,
            .optimize = optimize,
        }),
    });
    if (enable_lto) libActonProject.lto = .full;

    for (c_files.items) |entry| {
        libActonProject.root_module.addCSourceFile(.{ .file = b.path(entry), .flags = flags.items });
    }

    libActonProject.root_module.addIncludePath(b.path("."));
    libActonProject.root_module.addIncludePath(b.path("../base"));

    libActonProject.root_module.linkLibrary(actonbase_dep.artifact("Acton"));
    libActonProject.root_module.linkLibrary(dep_libpcre2.artifact("pcre2-8"));
    libActonProject.root_module.linkLibrary(dep_libsnappy_c.artifact("snappy-c"));
    libActonProject.root_module.linkLibrary(dep_libxml2.artifact("xml2"));
    libActonProject.root_module.linkLibrary(dep_libyyjson.artifact("yyjson"));

    libActonProject.installHeadersDirectory(b.path("out/types"), "out/types", .{});

    var hiter_dir = b.build_root.handle.openDir(io, "out/types/", .{ .iterate = true }) catch unreachable;
    var hwalker = hiter_dir.walk(b.allocator) catch unreachable;
    defer hwalker.deinit();

    while (true) {
        const next_result = hwalker.next(io) catch unreachable;
        if (next_result) |entry| {
            if (entry.kind == .file and std.mem.endsWith(u8, entry.basename, ".h")) {
                const file_path = std.fs.path.join(b.allocator, &.{ "out/types", entry.path }) catch unreachable;
                libActonProject.installHeader(b.path(file_path), file_path);
            }
        } else {
            break;
        }
    }

    libActonProject.root_module.link_libc = true;
    libActonProject.root_module.link_libcpp = true;
    b.installArtifact(libActonProject);
}
