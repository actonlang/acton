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
fn relJoinPath(allocator: std.mem.Allocator, dots: []const u8, base: []const u8, relative: []const u8) []const u8 {
    const path = allocator.alloc(u8, dots.len + base.len + relative.len + 1) catch @panic("OOM");
    _ = std.fmt.bufPrint(path, "{s}{s}/{s}", .{dots, base, relative}) catch @panic("Error joining paths");
    return path;
}

fn joinPath(allocator: std.mem.Allocator, base: []const u8, relative: []const u8) []const u8 {
    const path = allocator.alloc(u8, base.len + relative.len + 1) catch @panic("OOM");
    _ = std.fmt.bufPrint(path, "{s}/{s}", .{base, relative}) catch @panic("Error joining paths");
    return path;
}

fn dotsToRoot(allocator: std.mem.Allocator, cwd: []const u8) []const u8 {
    // Split up the path into its components, separated by std.fs.path.sep
    var parts = std.mem.splitScalar(u8, cwd, std.fs.path.sep);
    var num_parts: u16 = 0;
    while (parts.next()) |_| {
        num_parts += 1;
    }
    num_parts -= 1;
    var dotpath = allocator.alloc(u8, 3*num_parts) catch @panic("OOM");
    var i: u16 = 0;
    while (i < num_parts) : (i += 1) {
        dotpath[i*3+0] = '.';
        dotpath[i*3+1] = '.';
        dotpath[i*3+2] = std.fs.path.sep;
    }
    return dotpath;
}


pub fn build(b: *std.Build) void {
    const buildroot_path = b.build_root.handle.realpathAlloc(b.allocator, ".") catch unreachable;
    const dots_to_root = dotsToRoot(b.allocator, buildroot_path);
    defer b.allocator.free(dots_to_root);
    const optimize = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});
    const cpedantic = b.option(bool, "cpedantic", "") orelse false;
    const use_db = b.option(bool, "db", "") orelse false;
    const no_threads = b.option(bool, "no_threads", "") orelse false;
    const syspath = b.option([]const u8, "syspath", "") orelse "";
    const libactondeps = b.option([]const u8, "libactondeps", "") orelse "";
    const libactongc = b.option([]const u8, "libactongc", "") orelse "";
    _ = libactongc;
    _ = libactondeps;

    const projpath_outtypes = joinPath(b.allocator, buildroot_path, "out/types");
    const syspath_base = relJoinPath(b.allocator, dots_to_root, syspath, "base");
    const syspath_include = joinPath(b.allocator, syspath, "depsout/include");

    print("Acton Base Builder\nBuilding in {s}\n", .{buildroot_path});

    const dep_libutf8proc = b.dependency("libutf8proc", .{
        .target = target,
        .optimize = optimize,
        .BUILD_SHARED_LIBS = false,
    });

    var iter_dir = b.build_root.handle.openDir(
        "out/types/",
        .{
            .iterate = true
        },
    ) catch |err| {
        std.log.err("Error opening iterable dir: {}", .{err});
        std.posix.exit(1);
    };

    var c_files = ArrayList([]const u8).init(b.allocator);
    var root_c_files = ArrayList(*FilePath).init(b.allocator);
    var walker = iter_dir.walk(b.allocator) catch |err| {
        std.log.err("Error walking dir: {}", .{err});
        std.posix.exit(1);
    };
    defer walker.deinit();

    // Find all .c files
    while (true) {
        const next_result = walker.next() catch |err| {
            std.log.err("Error getting next: {}", .{err});
            std.posix.exit(1);
        };
        if (next_result) |entry| {
            if (entry.kind == .file) {
                if (std.mem.endsWith(u8, entry.basename, ".c")) {
                    const fPath = b.allocator.create(FilePath) catch |err| {
                        std.log.err("Error allocating FilePath entry: {}", .{err});
                        std.posix.exit(1);
                    };
                    const full_path = entry.dir.realpathAlloc(b.allocator, entry.basename) catch |err| {
                        std.log.err("Error getting dir name: {}", .{err});
                        std.posix.exit(1);
                    };
                    const dir = entry.dir.realpathAlloc(b.allocator, ".") catch |err| {
                        std.log.err("Error getting dir name: {}", .{err});
                        std.posix.exit(1);
                    };
                    fPath.full_path = full_path;
                    fPath.dir = dir;
                    fPath.filename = b.allocator.dupe(u8, entry.basename) catch |err| {
                        std.log.err("Error allocating filename entry: {}", .{err});
                        std.posix.exit(1);
                    };
                    const file_path = b.allocator.alloc(u8, full_path.len - projpath_outtypes.len) catch |err| {
                        std.log.err("Error allocating file_path entry: {}", .{err});
                        std.posix.exit(1);
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
                            std.posix.exit(1);
                        };
                    } else {
                        c_files.append(fPath.full_path) catch |err| {
                            std.log.err("Error appending to .c files: {}", .{err});
                            std.posix.exit(1);
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
    flags.append("-DUTF8PROC_STATIC") catch unreachable;

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
            std.posix.exit(1);
        };
    }

    if (use_db) {
        print("Building with DB backend support\n", .{});
        flags.append("-DACTON_DB") catch unreachable;
    }

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

    const libActon = b.addStaticLibrary(.{
        .name = "Acton",
        .target = target,
        .optimize = optimize,
    });
    for (c_files.items) |entry| {
        libActon.addCSourceFile(.{ .file = .{ .cwd_relative = entry }, .flags = flags.items });
    }

    libActon.addIncludePath(.{ .cwd_relative = buildroot_path });
    libActon.addIncludePath(.{ .cwd_relative = syspath_base });
    libActon.addIncludePath(.{ .cwd_relative = syspath_include });
    libActon.addIncludePath(b.path("../inc")); // hack hack for stdlib TODO: sort out
    libActon.addIncludePath(b.path("../deps/instdir/include")); // hack hack for stdlib TODO: sort out

    libActon.linkLibrary(dep_libutf8proc.artifact("utf8proc"));

    libActon.linkLibC();
    libActon.linkLibCpp();
    b.installArtifact(libActon);
}
