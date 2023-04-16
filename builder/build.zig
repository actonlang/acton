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
    const optimize = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});
    const projpath_out = b.option([]const u8, "projpath_out", "") orelse "";
    const projpath_outtypes = b.option([]const u8, "projpath_outtypes", "") orelse "";
    const syspath = b.option([]const u8, "syspath", "") orelse "";
    const syspath_include = b.option([]const u8, "syspath_include", "") orelse "";
    const syspath_lib = b.option([]const u8, "syspath_lib", "") orelse "";
    const syspath_libreldev = b.option([]const u8, "syspath_libreldev", "") orelse "";
    const libactondeps = b.option([]const u8, "libactondeps", "") orelse "";
    const libactongc = b.option([]const u8, "libactongc", "") orelse "";
    const wd = b.option([]const u8, "wd", "") orelse "";

    var iter_dir = std.fs.cwd().openIterableDir(
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
            if (entry.kind == .File) {
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

    const libActonProject = b.addStaticLibrary(.{
        .name = "ActonProject",
        .target = target,
        .optimize = optimize,
    });
    const cflags = [_][]const u8{
        wd
    };
    for (c_files.items) |entry| {
        libActonProject.addCSourceFile(entry, &cflags);
    }

    libActonProject.addIncludePath(projpath_out);
    libActonProject.addIncludePath(".");
    libActonProject.addIncludePath(syspath);
    libActonProject.addIncludePath(syspath_include);
    libActonProject.addIncludePath("../deps/instdir/include"); // hack hack for stdlib TODO: sort out
    libActonProject.linkLibC();
    b.installArtifact(libActonProject);

    for (root_c_files.items) |entry| {
        // Get the binary name, by removing .root.c from end and having it relative to the projpath_outtypes
        const binname = b.allocator.alloc(u8, entry.file_path.len-8) catch |err| {
            std.log.err("Error allocating binname: {}", .{err});
            std.os.exit(1);
        };
        @memcpy(binname, entry.file_path[1..(entry.file_path.len-7)]);

        // Replace / with . in the binary name
        for (binname) |*ch| {
            if (ch.* == '/') ch.* = '.';
        }
        print("Building executable from: {s} -> {s}\n", .{entry.full_path, binname});

        const executable = b.addExecutable(.{
            .name = binname,
            .target = target,
            .optimize = optimize,
        });
        executable.addCSourceFile(entry.full_path, &[_][]const u8{});
        executable.addIncludePath(projpath_out);
        executable.addIncludePath(syspath);
        executable.addIncludePath(syspath_include);
        executable.addLibraryPath("out/rel/lib/");
        executable.addLibraryPath(syspath_libreldev);
        executable.addLibraryPath(syspath_lib);
        executable.linkLibrary(libActonProject);
        executable.linkSystemLibraryName("Acton");
        executable.linkSystemLibraryName(libactondeps);
        executable.linkSystemLibraryName("ActonDB");
        executable.linkSystemLibraryName(libactongc);
        executable.linkLibC();
        b.installArtifact(executable);
    }
}
