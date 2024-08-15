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
    const buildroot_path = b.build_root.handle.realpathAlloc(b.allocator, ".") catch @panic("ASD");
    const dots_to_root = dotsToRoot(b.allocator, buildroot_path);
    defer b.allocator.free(dots_to_root);
    const optimize = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});
    const db = b.option(bool, "db", "") orelse false;
    const only_lib = b.option(bool, "only_lib", "") orelse false;
    const no_threads = b.option(bool, "no_threads", "") orelse false;
    const syspath = b.option([]const u8, "syspath", "") orelse "";
    const arg_deps_path = b.option([]const u8, "deps_path", "") orelse "";

    const deps_path = if (arg_deps_path.len > 0) arg_deps_path else joinPath(b.allocator, buildroot_path, "deps");

    const projpath_outtypes = joinPath(b.allocator, buildroot_path, "out/types");
    const syspath_base = relJoinPath(b.allocator, dots_to_root, syspath, "base");
    const syspath_include = joinPath(b.allocator, syspath, "depsout/include");
    const syspath_lib = joinPath(b.allocator, syspath, "depsout/lib");

    print("Acton Project Builder - building {s}\nDeps path: {s}\n", .{buildroot_path, deps_path});

    var iter_dir = b.build_root.handle.openDir(
        "out/types/", .{ .iterate = true },
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
                        fPath.test_root = false;
                        root_c_files.append(fPath) catch |err| {
                            std.log.err("Error appending to root .c files: {}", .{err});
                            std.posix.exit(1);
                        };
                    } else if (std.mem.endsWith(u8, entry.basename, ".test_root.c")) {
                        fPath.test_root = true;
                        root_c_files.append(fPath) catch |err| {
                            std.log.err("Error appending to test_root .c files: {}", .{err});
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
            std.posix.exit(1);
        };
    }

    if (db)
        flags.appendSlice(&.{"-DACTON_DB",}) catch unreachable;

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

    for (c_files.items) |entry| {
        libActonProject.addCSourceFile(.{ .file = .{ .cwd_relative = entry }, .flags = flags.items });
    }

    libActonProject.addIncludePath(.{ .cwd_relative = buildroot_path });

    // project dependencies
    print("Checking for dependencies in: {s}\n", .{deps_path});
    const deps_dir = std.fs.cwd().openDir(deps_path, .{ .iterate = true });
    if (deps_dir) |dir| {
        //defer dir.close();
        var deps_walker = dir.iterate();
        while (deps_walker.next() catch unreachable) |dep_entry| {

            if (dep_entry.kind == .directory) {
                std.debug.print("Found sub-directory: {s}\n", .{dep_entry.name});
                const dep_path = joinPath(b.allocator, deps_path, dep_entry.name);
                libActonProject.addIncludePath(b.path(dep_path));
            }
        }
    } else |err| {
        std.debug.print("Failed to open directory: {}\n", .{err});
    }

    libActonProject.addIncludePath(.{ .cwd_relative = syspath_base });
    libActonProject.addIncludePath(.{ .cwd_relative = syspath_include });
    libActonProject.linkLibC();
    libActonProject.linkLibCpp();
    b.installArtifact(libActonProject);

    if (!only_lib) {
        const libactondb_dep = b.dependency("actondb", .{
            .target = target,
            .optimize = optimize,
            .syspath_include = syspath_include,
        });

        const actonbase_dep = b.dependency("base", .{
            .target = target,
            .optimize = optimize,
            .no_threads = no_threads,
            .db = db,
            .syspath = syspath,
        });

        const dep_libgc = b.dependency("libgc", .{
            .target = target,
            .optimize = optimize,
            .BUILD_SHARED_LIBS = false,
            .enable_large_config = true,
            .enable_mmap = true,
        });

        // -- ActonDeps ------------------------------------------------------------
        const dep_libargp = b.dependency("libargp", .{
            .target = target,
            .optimize = optimize,
        });

        const dep_libbsdnt = b.dependency("libbsdnt", .{
            .target = target,
            .optimize = optimize,
        });

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

        const dep_libprotobuf_c = b.dependency("libprotobuf_c", .{
            .target = target,
            .optimize = optimize,
        });

        const dep_libtlsuv = b.dependency("libtlsuv", .{
            .target = target,
            .optimize = optimize,
        });

        const dep_libutf8proc = b.dependency("libutf8proc", .{
            .target = target,
            .optimize = optimize,
        });

        const dep_libuuid = b.dependency("libuuid", .{
            .target = target,
            .optimize = optimize,
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

        const dep_libsnappy_c = b.dependency("libsnappy", .{
            .target = target,
            .optimize = optimize,
        });

        for (root_c_files.items) |entry| {
            // Get the binary name, by removing .root.c from end and having it relative to the projpath_outtypes
            var nlen = ".root.c".len;
            if (entry.test_root)
                nlen = ".test_root.c".len - ".test_".len;
            nlen += 1; // for the null terminator
            const binname = b.allocator.alloc(u8, entry.file_path.len-nlen) catch |err| {
                std.log.info("Error allocating binname: {}", .{err});
                std.posix.exit(1);
            };
            if (entry.test_root) {
                // Write '.test_' to start of binname
                const buf = std.fmt.allocPrint(b.allocator, ".test_{s}", .{entry.file_path[1..entry.file_path.len - ".test_root.c".len]}) catch |err| {
                    std.log.err("Error allocating binname: {}", .{err});
                    std.posix.exit(1);
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
            executable.addCSourceFile(.{ .file = .{ .cwd_relative = entry.full_path }, .flags = flags.items });
            executable.addIncludePath(.{ .cwd_relative = buildroot_path });
            executable.addIncludePath(.{ .cwd_relative = syspath_base });
            executable.addIncludePath(.{ .cwd_relative = syspath_include });
            executable.addIncludePath(.{ .cwd_relative = syspath_lib });
            executable.linkLibrary(libActonProject);

            // project dependencies
            if (deps_dir) |dir| {
                //defer dir.close();
                var deps_walker = dir.iterate();
                while (deps_walker.next() catch unreachable) |dep_entry| {

                    if (dep_entry.kind == .directory) {
                        std.debug.print("Found sub-directory: {s}\n", .{dep_entry.name});
                        const dep_path = joinPath(b.allocator, deps_path, dep_entry.name);
                        executable.addIncludePath(b.path(dep_path));
                        const dep_path_rel = joinPath(b.allocator, "deps", dep_entry.name);
                        const dep_dep = b.dependency(dep_path_rel, .{
                            .target = target,
                            .optimize = optimize,
                            .only_lib = true,
                            .syspath = syspath,
                            .deps_path = deps_path,
                        });
                        executable.linkLibrary(dep_dep.artifact("ActonProject"));
                    }
                }
            } else |err| {
                std.debug.print("Failed to open directory: {}\n", .{err});
            }

            executable.linkLibrary(actonbase_dep.artifact("Acton"));
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
            executable.linkLibrary(dep_libpcre2.artifact("pcre2-8"));
            executable.linkLibrary(dep_libprotobuf_c.artifact("protobuf-c"));
            executable.linkLibrary(dep_libsnappy_c.artifact("snappy-c"));
            executable.linkLibrary(dep_libtlsuv.artifact("tlsuv"));
            executable.linkLibrary(dep_libutf8proc.artifact("utf8proc"));
            executable.linkLibrary(dep_libuv.artifact("uv"));
            executable.linkLibrary(dep_libxml2.artifact("xml2"));
            executable.linkLibrary(dep_libyyjson.artifact("yyjson"));
            executable.linkLibrary(dep_libgc.artifact("gc"));

            executable.linkLibC();
            executable.linkLibCpp();
            b.installArtifact(executable);
        }
    }
}
