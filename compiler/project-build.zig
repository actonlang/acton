
const std = @import("std");
const print = @import("std").debug.print;
const ArrayList = std.ArrayList;

pub fn build(b: *std.build.Builder) void {
    const optimize = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});
    const projpath = b.option([]const u8, "projpath", "") orelse "";
    const syspath = b.option([]const u8, "syspath", "") orelse "";
    const reldev = b.option([]const u8, "reldev", "") orelse "";
    const binsrc = b.option([]const u8, "binsrc", "") orelse "";
    const binname = b.option([]const u8, "binname", "") orelse "";
    const wd = b.option([]const u8, "wd", "") orelse "";
    var syspath_include_buf: [1024]u8 = undefined;
    const syspath_include = std.fmt.bufPrint(&syspath_include_buf, "{s}/include/", .{ syspath }) catch {
        std.log.err("Error concatenating strings: ", .{});
        std.os.exit(1);
    };
    var syspath_lib_buf: [1024]u8 = undefined;
    var syspath_libreldev_buf: [1024]u8 = undefined;
    const syspath_lib = std.fmt.bufPrint(&syspath_lib_buf, "{s}/lib/", .{ syspath }) catch {
        std.log.err("Error concatenating strings: ", .{});
        std.os.exit(1);
    };
    const syspath_libreldev = std.fmt.bufPrint(&syspath_libreldev_buf, "{s}/lib/{s}/", .{ syspath, reldev }) catch {
        std.log.err("Error concatenating strings: ", .{});
        std.os.exit(1);
    };

    var iter_dir = std.fs.cwd().openIterableDir(
        "out/types/",
        .{},
    ) catch |err| {
        std.log.err("Error opening iterable dir: {}", .{err});
        std.os.exit(1);
    };

    var c_files = ArrayList([]u8).init(b.allocator);
    var root_c_files = ArrayList([]u8).init(b.allocator);
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
                    const fname = b.allocator.alloc(u8, 1024) catch |err| {
                        std.log.err("Error allocating fname: {}", .{err});
                        std.os.exit(1);
                    };
                    @memset(fname, 0);
                    _ = entry.dir.realpath(entry.basename, fname) catch |err| {
                        std.log.err("Error doing realpath: {}", .{err});
                        std.os.exit(1);
                    };

                    if (std.mem.endsWith(u8, entry.basename, ".root.c")) {
                        root_c_files.append(fname) catch |err| {
                            std.log.err("Error appending to root .c files: {}", .{err});
                            std.os.exit(1);
                        };
                    } else {
                        c_files.append(fname) catch |err| {
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

    var projpath_out_buf: [1024]u8 = undefined;
    const projpath_out = std.fmt.bufPrint(&projpath_out_buf, "{s}/out", .{ projpath }) catch {
        std.log.err("Error concatenating strings: ", .{});
        std.os.exit(1);
    };
    libActonProject.addIncludePath(projpath_out);

    libActonProject.addIncludePath(".");
    libActonProject.addIncludePath(syspath);
    libActonProject.addIncludePath(syspath_include);
    libActonProject.addIncludePath("../deps/instdir/include"); // hack hack for stdlib
    libActonProject.linkLibC();
    b.installArtifact(libActonProject);

    if (!(std.mem.eql(u8, binname, "") and std.mem.eql(u8, binsrc, ""))) {
        const executable = b.addExecutable(.{
            .name = binname,
            .target = target,
            .optimize = optimize,
        });
        var absbinsrc_buf: [1024]u8 = undefined;
        const absbinsrc = std.fmt.bufPrint(&absbinsrc_buf, "{s}/{s}", .{ projpath, binsrc }) catch {
            std.log.err("Error concatenating strings: ", .{});
            std.os.exit(1);
        };
        executable.addCSourceFile(absbinsrc, &[_][]const u8{});
        executable.addIncludePath(projpath_out);
        executable.addIncludePath(syspath);
        executable.addIncludePath(syspath_include);
        executable.addLibraryPath("out/rel/lib/");
        executable.addLibraryPath(syspath_libreldev);
        executable.addLibraryPath(syspath_lib);
        executable.linkLibrary(libActonProject);
        executable.linkSystemLibraryName("Acton");
        executable.linkSystemLibraryName("ActonDeps");
        executable.linkSystemLibraryName("ActonDB");
        executable.linkSystemLibraryName("actongc");
        executable.linkLibC();
        b.installArtifact(executable);
    }
}
