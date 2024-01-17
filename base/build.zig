// Acton Project Builder
// Performs the final build of the project by compiling the generated C code.

const std = @import("std");
const print = @import("std").debug.print;
const ArrayList = std.ArrayList;

pub const FilePath = struct {
    filename: []const u8,
    full_path: []const u8,
    dir: []const u8,
    file_path: []const u8,
};

pub fn build(b: *std.build.Builder) void {
    const buildroot_path = b.build_root.handle.realpathAlloc(b.allocator, ".") catch unreachable;
    print("Acton Base Builder\nBuilding in {s}\n", .{buildroot_path});
    const optimize = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});
    const cpedantic = b.option(bool, "cpedantic", "") orelse false;
    const use_prebuilt = b.option(bool, "use_prebuilt", "") orelse false;
    const projpath = b.option([]const u8, "projpath", "") orelse "";
    const projpath_outtypes = b.option([]const u8, "projpath_outtypes", "") orelse "";
    const syspath = b.option([]const u8, "syspath", "") orelse "";
    const syspath_base = b.option([]const u8, "syspath_base", "") orelse "";
    const syspath_include = b.option([]const u8, "syspath_include", "") orelse "";
    const syspath_lib = b.option([]const u8, "syspath_lib", "") orelse "";
    const syspath_libreldev = b.option([]const u8, "syspath_libreldev", "") orelse "";
    const syspath_backend = b.option([]const u8, "syspath_backend", "") orelse "";
    const libactondeps = b.option([]const u8, "libactondeps", "") orelse "";
    const libactongc = b.option([]const u8, "libactongc", "") orelse "";
    _ = use_prebuilt;
    _ = libactongc;
    _ = libactondeps;
    _ = syspath_backend;
    _ = syspath_libreldev;
    _ = syspath_lib;
    _ = syspath;

    var iter_dir = b.build_root.handle.openIterableDir(
        "out/types/",
        .{},
    ) catch |err| {
        std.log.err("Error opening iterable dir: {}", .{err});
        std.os.exit(1);
    };

    var c_files = ArrayList([]const u8).init(b.allocator);
    var root_c_files = ArrayList(*FilePath).init(b.allocator);
    var walker = iter_dir.walk(b.allocator) catch |err| {
        std.log.err("Error walking dir: {}", .{err});
        std.os.exit(1);
    };
    defer walker.deinit();

    // Find all .c files
    while (true) {
        const next_result = walker.next() catch |err| {
            std.log.err("Error getting next: {}", .{err});
            std.os.exit(1);
        };
        if (next_result) |entry| {
            if (entry.kind == .file) {
                if (std.mem.endsWith(u8, entry.basename, ".c")) {
                    const fPath = b.allocator.create(FilePath) catch |err| {
                        std.log.err("Error allocating FilePath entry: {}", .{err});
                        std.os.exit(1);
                    };
                    const full_path = entry.dir.realpathAlloc(b.allocator, entry.basename) catch |err| {
                        std.log.err("Error getting dir name: {}", .{err});
                        std.os.exit(1);
                    };
                    const dir = entry.dir.realpathAlloc(b.allocator, ".") catch |err| {
                        std.log.err("Error getting dir name: {}", .{err});
                        std.os.exit(1);
                    };
                    fPath.full_path = full_path;
                    fPath.dir = dir;
                    fPath.filename = b.allocator.dupe(u8, entry.basename) catch |err| {
                        std.log.err("Error allocating filename entry: {}", .{err});
                        std.os.exit(1);
                    };
                    const file_path = b.allocator.alloc(u8, full_path.len - projpath_outtypes.len) catch |err| {
                        std.log.err("Error allocating file_path entry: {}", .{err});
                        std.os.exit(1);
                    };
                    @memcpy(file_path, full_path[projpath_outtypes.len..]);
                    fPath.file_path = file_path;

                    print("-- filename : {s}\n", .{fPath.filename});
                    print("   full_path: {s}\n", .{fPath.full_path});
                    print("   dir      : {s}\n", .{fPath.dir});
                    print("   file_path: {s}\n", .{fPath.file_path});

                    if (std.mem.endsWith(u8, entry.basename, ".root.c")) {
                        root_c_files.append(fPath) catch |err| {
                            std.log.err("Error appending to root .c files: {}", .{err});
                            std.os.exit(1);
                        };
                    } else {
                        c_files.append(fPath.full_path) catch |err| {
                            std.log.err("Error appending to .c files: {}", .{err});
                            std.os.exit(1);
                        };
                    }
                }
            }
        } else {
            break;
        }
    }

    var flags = std.ArrayList([]const u8).init(b.allocator);
    defer flags.deinit();

    var file_prefix_map = std.ArrayList(u8).init(b.allocator);
    defer file_prefix_map.deinit();
    const file_prefix_path = b.build_root.handle.openDir("..", .{}) catch unreachable;
    const file_prefix_path_path = file_prefix_path.realpathAlloc(b.allocator, ".") catch unreachable;
    file_prefix_map.appendSlice("-ffile-prefix-map=") catch unreachable;
    file_prefix_map.appendSlice(file_prefix_path_path) catch unreachable;
    file_prefix_map.appendSlice("/=") catch unreachable;
    flags.append(file_prefix_map.items) catch unreachable;
    if (cpedantic) {
        flags.append("-Werror") catch unreachable;
    }

    if (optimize == .Debug) {
        print("Debug build\n", .{});
        flags.appendSlice(&.{
            "-DDEV",
        }) catch |err| {
            std.log.err("Error appending flags: {}", .{err});
            std.os.exit(1);
        };
    }

    const libActon = b.addStaticLibrary(.{
        .name = "Acton",
        .target = target,
        .optimize = optimize,
    });
    for (c_files.items) |entry| {
        libActon.addCSourceFile(.{ .file = .{ .path = entry }, .flags = flags.items });
    }

    libActon.addIncludePath(.{ .path = projpath });
    libActon.addIncludePath(.{ .path = syspath_base });
    libActon.addIncludePath(.{ .path = syspath_include });
    libActon.addIncludePath(.{ .path = "../inc" }); // hack hack for stdlib TODO: sort out
    libActon.addIncludePath(.{ .path = "../deps/instdir/include" }); // hack hack for stdlib TODO: sort out
    libActon.linkLibC();
    libActon.linkLibCpp();
    b.installArtifact(libActon);
}
