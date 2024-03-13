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
    test_root: bool
};

// We have an absolute path we want to get to, but we have to provide it as a
// relative path from the current position. The easiest way to do this is to go
// up the directory tree until we're at the root, and then the absolute path is
// relative to the root and can be used. It would be more elegant to figure out
// if there are actual commonalities between the paths and only traverse
// upwards as far as necessary.
fn joinPath(allocator: std.mem.Allocator, dots: []const u8, base: []const u8, relative: []const u8) []const u8 {
    const path = allocator.alloc(u8, dots.len + base.len + relative.len + 1) catch @panic("OOM");
    _ = std.fmt.bufPrint(path, "{s}{s}/{s}", .{dots, base, relative}) catch @panic("Error joining paths");
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
    const buildroot_path = b.build_root.handle.realpathAlloc(b.allocator, ".") catch @panic("ASD");
    const dots_to_root = dotsToRoot(b.allocator, buildroot_path);
    defer b.allocator.free(dots_to_root);
    print("Acton Project Builder\nBuilding in {s}\n", .{buildroot_path});
    const optimize = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});
    const db = b.option(bool, "db", "") orelse false;
    const use_prebuilt = b.option(bool, "use_prebuilt", "") orelse false;
    const projpath = b.option([]const u8, "projpath", "") orelse "";
    const projpath_outtypes = b.option([]const u8, "projpath_outtypes", "") orelse "";
    const syspath = b.option([]const u8, "syspath", "") orelse "";
    const syspath_backend = b.option([]const u8, "syspath_backend", "") orelse "";
    const syspath_base = b.option([]const u8, "syspath_base", "") orelse "";
    const syspath_include = b.option([]const u8, "syspath_include", "") orelse "";
    const syspath_lib = b.option([]const u8, "syspath_lib", "") orelse "";
    const syspath_libreldev = b.option([]const u8, "syspath_libreldev", "") orelse "";

    const libactondb_dep = b.anonymousDependency(syspath_backend, @import("backendbuild.zig"), .{
        .target = target,
        .optimize = optimize,
        .syspath_include = syspath_include,
    });

    const actonbase_dep = b.anonymousDependency(syspath_base, @import("basebuild.zig"), .{
        .target = target,
        .optimize = optimize,
        .db = db,
        .syspath_include = syspath_include,
    });

    const dep_libgc = b.anonymousDependency(joinPath(b.allocator, dots_to_root, syspath, "deps/libgc"), @import("deps/libgc/build.zig"), .{
        .target = target,
        .optimize = optimize,
        .BUILD_SHARED_LIBS = false,
        .enable_redirect_malloc = true,
        .enable_ignore_free = true,
        .enable_large_config = true,
        .enable_mmap = true,
    });


    // -- ActonDeps ------------------------------------------------------------
    const dep_libargp = b.anonymousDependency(joinPath(b.allocator, dots_to_root, syspath, "deps/libargp"), @import("deps/libargp/build.zig"), .{
        .target = target,
        .optimize = optimize,
    });

    const dep_libbsdnt = b.anonymousDependency(joinPath(b.allocator, dots_to_root, syspath, "deps/libbsdnt"), @import("deps/libbsdnt/build.zig"), .{
        .target = target,
        .optimize = optimize,
    });

    const dep_libmbedtls = b.anonymousDependency(joinPath(b.allocator, dots_to_root, syspath, "deps/mbedtls"), @import("deps/mbedtls/build.zig"), .{
        .target = target,
        .optimize = optimize,
    });

    const dep_libnetstring = b.anonymousDependency(joinPath(b.allocator, dots_to_root, syspath, "deps/libnetstring"), @import("deps/libnetstring/build.zig"), .{
        .target = target,
        .optimize = optimize,
    });

    const dep_libpcre2 = b.anonymousDependency(joinPath(b.allocator, dots_to_root, syspath, "deps/pcre2"), @import("deps/pcre2/build.zig"), .{
        .target = target,
        .optimize = optimize,
    });

    const dep_libprotobuf_c = b.anonymousDependency(joinPath(b.allocator, dots_to_root, syspath, "deps/libprotobuf_c"), @import("deps/libprotobuf_c/build.zig"), .{
        .target = target,
        .optimize = optimize,
    });

    const dep_libtlsuv = b.anonymousDependency(joinPath(b.allocator, dots_to_root, syspath, "deps/tlsuv"), @import("deps/tlsuv/build.zig"), .{
        .target = target,
        .optimize = optimize,
    });

    const dep_libutf8proc = b.anonymousDependency(joinPath(b.allocator, dots_to_root, syspath, "deps/libutf8proc"), @import("deps/libutf8proc/build.zig"), .{
        .target = target,
        .optimize = optimize,
    });

    const dep_libuuid = b.anonymousDependency(joinPath(b.allocator, dots_to_root, syspath, "deps/libuuid"), @import("deps/libuuid/build.zig"), .{
        .target = target,
        .optimize = optimize,
    });

    const dep_libuv = b.anonymousDependency(joinPath(b.allocator, dots_to_root, syspath, "deps/libuv"), @import("deps/libuv/build.zig"), .{
        .target = target,
        .optimize = optimize,
    });

    const dep_libxml2 = b.anonymousDependency(joinPath(b.allocator, dots_to_root, syspath, "deps/libxml2"), @import("deps/libxml2/build.zig"), .{
        .target = target,
        .optimize = optimize,
    });

    const dep_libyyjson = b.anonymousDependency(joinPath(b.allocator, dots_to_root, syspath, "deps/libyyjson"), @import("deps/libyyjson/build.zig"), .{
        .target = target,
        .optimize = optimize,
    });

    const dep_libsnappy_c = b.anonymousDependency(joinPath(b.allocator, dots_to_root, syspath, "deps/libsnappy_c"), @import("deps/libsnappy_c/build.zig"), .{
        .target = target,
        .optimize = optimize,
    });

    // -- ActonDeps ------------------------------------------------------------

    var iter_dir = b.build_root.handle.openDir(
        "out/types/",
        .{
            .iterate = true
        },
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
                        fPath.test_root = false;
                        root_c_files.append(fPath) catch |err| {
                            std.log.err("Error appending to root .c files: {}", .{err});
                            std.os.exit(1);
                        };
                    } else if (std.mem.endsWith(u8, entry.basename, ".test_root.c")) {
                        fPath.test_root = true;
                        root_c_files.append(fPath) catch |err| {
                            std.log.err("Error appending to test_root .c files: {}", .{err});
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

    if (optimize == .Debug) {
        print("Debug build\n", .{});
        flags.appendSlice(&.{
            "-DDEV",
        }) catch |err| {
            std.log.err("Error appending flags: {}", .{err});
            std.os.exit(1);
        };
    }

    if (db)
        flags.appendSlice(&.{"-DACTON_DB",}) catch unreachable;

    for (c_files.items) |entry| {
        libActonProject.addCSourceFile(.{ .file = .{ .path = entry }, .flags = flags.items });
    }

    libActonProject.addIncludePath(.{ .path = projpath });
    libActonProject.addIncludePath(.{ .path = syspath_base });
    libActonProject.addIncludePath(.{ .path = syspath_include });
    libActonProject.linkLibC();
    libActonProject.linkLibCpp();
    b.installArtifact(libActonProject);

    for (root_c_files.items) |entry| {
        // Get the binary name, by removing .root.c from end and having it relative to the projpath_outtypes
        var nlen = ".root.c".len;
        if (entry.test_root)
            nlen = ".test_root.c".len - ".test_".len;
        nlen += 1; // for the null terminator
        const binname = b.allocator.alloc(u8, entry.file_path.len-nlen) catch |err| {
            std.log.info("Error allocating binname: {}", .{err});
            std.os.exit(1);
        };
        if (entry.test_root) {
            // Write '.test_' to start of binname
            const buf = std.fmt.allocPrint(b.allocator, ".test_{s}", .{entry.file_path[1..entry.file_path.len - ".test_root.c".len]}) catch |err| {
                std.log.err("Error allocating binname: {}", .{err});
                std.os.exit(1);
            };
            @memcpy(binname, buf);
        } else {
            @memcpy(binname, entry.file_path[1..(entry.file_path.len - ".root.c".len)]);
        }
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
        //_ = syspath;
        executable.addCSourceFile(.{ .file = .{ .path = entry.full_path }, .flags = flags.items });
        executable.addIncludePath(.{ .path = projpath });
        executable.addIncludePath(.{ .path = syspath_base });
        executable.addIncludePath(.{ .path = syspath_include });
        executable.addLibraryPath(.{ .path = "out/rel/lib/" });
        executable.addLibraryPath(.{ .path = syspath_libreldev });
        executable.addLibraryPath(.{ .path = syspath_lib });
        executable.linkLibrary(libActonProject);

        // Do not use prebuilt based on the use_prebuilt flag, but also do not
        // use prebuilt when there are custom options, like --db
        // Also see below.
        if (!use_prebuilt or db) {
            executable.linkLibrary(actonbase_dep.artifact("Acton"));
        } else {
            executable.linkSystemLibrary("Acton");
        }
        if (use_prebuilt) {
            if (db) {
                executable.linkSystemLibrary("ActonDB");
                executable.linkSystemLibrary("argp");
                executable.linkSystemLibrary("uuid");
            }
            executable.linkSystemLibrary("bsdnt");
            executable.linkSystemLibrary("mbedcrypto");
            executable.linkSystemLibrary("mbedtls");
            executable.linkSystemLibrary("mbedx509");
            executable.linkSystemLibrary("netstring");
            executable.linkSystemLibrary("pcre2");
            executable.linkSystemLibrary("protobuf-c");
            executable.linkSystemLibrary("snappy-c");
            executable.linkSystemLibrary("tlsuv");
            executable.linkSystemLibrary("utf8proc");
            executable.linkSystemLibrary("uv");
            executable.linkSystemLibrary("xml2");
            executable.linkSystemLibrary("yyjson");

            executable.linkSystemLibrary("actongc");
        } else {
            if (db) {
                executable.linkLibrary(libactondb_dep.artifact("ActonDB"));
                executable.linkLibrary(dep_libargp.artifact("argp"));
                executable.linkLibrary(dep_libuuid.artifact("uuid"));
            }
            executable.linkLibrary(dep_libbsdnt.artifact("bsdnt"));
            executable.linkLibrary(dep_libmbedtls.artifact("mbedcrypto"));
            executable.linkLibrary(dep_libmbedtls.artifact("mbedtls"));
            executable.linkLibrary(dep_libmbedtls.artifact("mbedx509"));
            executable.linkLibrary(dep_libnetstring.artifact("netstring"));
            executable.linkLibrary(dep_libpcre2.artifact("pcre2"));
            executable.linkLibrary(dep_libprotobuf_c.artifact("protobuf-c"));
            executable.linkLibrary(dep_libsnappy_c.artifact("snappy-c"));
            executable.linkLibrary(dep_libtlsuv.artifact("tlsuv"));
            executable.linkLibrary(dep_libutf8proc.artifact("utf8proc"));
            executable.linkLibrary(dep_libuv.artifact("uv"));
            executable.linkLibrary(dep_libxml2.artifact("xml2"));
            executable.linkLibrary(dep_libyyjson.artifact("yyjson"));

            executable.linkLibrary(dep_libgc.artifact("gc"));
        }

        executable.linkLibC();
        executable.linkLibCpp();
        b.installArtifact(executable);
    }
}
