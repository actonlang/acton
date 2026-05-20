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

pub fn build(b: *std.Build) void {
    const io = b.graph.io;
    const buildroot_path = b.build_root.join(b.allocator, &.{}) catch @panic("ASD");
    const optimize = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});
    const db = b.option(bool, "db", "") orelse false;
    const no_threads = b.option(bool, "no_threads", "") orelse false;
    const acton_libraries = b.option([]const u8, "acton_libraries", "") orelse "";
    const acton_modules = b.option([]const u8, "acton_modules", "") orelse {
        std.log.err("Missing required build option -Dacton_modules=...", .{});
        std.process.exit(1);
    };
    const acton_root_stubs = b.option([]const u8, "acton_root_stubs", "") orelse {
        std.log.err("Missing required build option -Dacton_root_stubs=...", .{});
        std.process.exit(1);
    };

    print("Acton Project Builder - building {s}\n", .{buildroot_path});

    const actonbase_dep = b.dependency("base", .{
        .target = target,
        .optimize = optimize,
        .no_threads = no_threads,
        .db = db,
    });

    // Dependencies from Build.act

    var c_files = ArrayList([]const u8).empty;
    var root_c_files = ArrayList(*FilePath).empty;
    defer c_files.deinit(b.allocator);
    defer root_c_files.deinit(b.allocator);

    var item_it = std.mem.splitScalar(u8, acton_modules, ',');
    while (item_it.next()) |raw_item| {
        const item = std.mem.trim(u8, raw_item, " \t\r");
        if (item.len == 0) continue;
        if (!std.mem.endsWith(u8, item, ".c")) continue;
        if (std.mem.endsWith(u8, item, ".root.c")) continue;
        if (std.mem.endsWith(u8, item, ".test_root.c")) continue;
        b.build_root.handle.access(io, item, .{}) catch |err| switch (err) {
            error.FileNotFound => {
                std.log.warn("Skipping missing selected C source: {s}", .{item});
                continue;
            },
            else => {
                std.log.err("Error checking selected C source ({s}): {}", .{ item, err });
                std.process.exit(1);
            },
        };
        const rel = b.allocator.dupe(u8, item) catch |err| {
            std.log.err("Error allocating selected C source path: {}", .{err});
            std.process.exit(1);
        };
        c_files.append(b.allocator, rel) catch |err| {
            std.log.err("Error appending selected C source path: {}", .{err});
            std.process.exit(1);
        };
    }

    var root_it = std.mem.splitScalar(u8, acton_root_stubs, ',');
    while (root_it.next()) |raw_item| {
        const item = std.mem.trim(u8, raw_item, " \t\r");
        if (item.len == 0) continue;
        const is_test_root = std.mem.endsWith(u8, item, ".test_root.c");
        const is_root = std.mem.endsWith(u8, item, ".root.c");
        if (!is_root and !is_test_root) continue;
        if (!std.mem.startsWith(u8, item, "out/types/")) {
            std.log.err("Invalid root stub path (expected under out/types): {s}", .{item});
            std.process.exit(1);
        }
        b.build_root.handle.access(io, item, .{}) catch |err| switch (err) {
            error.FileNotFound => {
                std.log.warn("Skipping missing selected root stub: {s}", .{item});
                continue;
            },
            else => {
                std.log.err("Error checking selected root stub ({s}): {}", .{ item, err });
                std.process.exit(1);
            },
        };
        const fPath = b.allocator.create(FilePath) catch |err| {
            std.log.err("Error allocating root FilePath entry: {}", .{err});
            std.process.exit(1);
        };
        fPath.full_path = joinPath(b.allocator, buildroot_path, item);
        const item_dir = std.fs.path.dirname(item) orelse ".";
        fPath.dir = joinPath(b.allocator, buildroot_path, item_dir);
        fPath.filename = b.allocator.dupe(u8, std.fs.path.basename(item)) catch |err| {
            std.log.err("Error allocating root filename entry: {}", .{err});
            std.process.exit(1);
        };
        fPath.file_path = b.allocator.dupe(u8, item["out/types".len..]) catch |err| {
            std.log.err("Error allocating root file_path entry: {}", .{err});
            std.process.exit(1);
        };
        fPath.test_root = is_test_root;
        root_c_files.append(b.allocator, fPath) catch |err| {
            std.log.err("Error appending selected root stub: {}", .{err});
            std.process.exit(1);
        };
    }

    if (c_files.items.len == 0) {
        const dummy_rel = "out/types/acton_empty.c";
        const dummy_abs = joinPath(b.allocator, buildroot_path, dummy_rel);
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
        std.log.info("No generated C sources; added dummy {s}", .{dummy_abs});
    }

    const libActonProject = b.addLibrary(.{
        .name = "ActonProject",
        .linkage = .static,
        .root_module = b.createModule(.{
            .target = target,
            .optimize = optimize,
        }),
    });
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

    for (c_files.items) |entry| {
        libActonProject.root_module.addCSourceFile(.{ .file = b.path(entry), .flags = flags.items });
    }

    libActonProject.root_module.addIncludePath(b.path("."));

    var explicit_libraries = ArrayList(*std.Build.Step.Compile).empty;
    var explicit_static_libraries = ArrayList(*std.Build.Step.Compile).empty;
    defer explicit_libraries.deinit(b.allocator);
    defer explicit_static_libraries.deinit(b.allocator);

    var lib_it = std.mem.splitScalar(u8, acton_libraries, '|');
    while (lib_it.next()) |raw_lib| {
        const lib_spec = std.mem.trim(u8, raw_lib, " \t\r");
        if (lib_spec.len == 0) continue;

        var field_it = std.mem.splitScalar(u8, lib_spec, ':');
        const lib_name = std.mem.trim(u8, field_it.next() orelse "", " \t\r");
        const lib_linkage = std.mem.trim(u8, field_it.next() orelse "", " \t\r");
        const lib_sources = std.mem.trim(u8, field_it.next() orelse "", " \t\r");
        if (lib_name.len == 0 or lib_sources.len == 0) continue;

        const lib_dynamic = std.mem.eql(u8, lib_linkage, "dynamic");
        const libActonExplicit = b.addLibrary(.{
            .name = lib_name,
            .linkage = if (lib_dynamic) .dynamic else .static,
            .root_module = b.createModule(.{
                .target = target,
                .optimize = optimize,
            }),
        });
        if (lib_dynamic) {
            libActonExplicit.linker_allow_shlib_undefined = true;
        }

        var source_it = std.mem.splitScalar(u8, lib_sources, ',');
        while (source_it.next()) |raw_source| {
            const source = std.mem.trim(u8, raw_source, " \t\r");
            if (source.len == 0) continue;
            libActonExplicit.root_module.addCSourceFile(.{ .file = b.path(source), .flags = flags.items });
        }
        libActonExplicit.root_module.addIncludePath(b.path("."));
        libActonExplicit.root_module.include_dirs.append(b.allocator, .{ .other_step = actonbase_dep.artifact("Acton") }) catch |err| {
            std.log.err("Error appending base include path for explicit library: {}", .{err});
            std.process.exit(1);
        };
        explicit_libraries.append(b.allocator, libActonExplicit) catch |err| {
            std.log.err("Error appending explicit library: {}", .{err});
            std.process.exit(1);
        };
        if (!lib_dynamic) {
            explicit_static_libraries.append(b.allocator, libActonExplicit) catch |err| {
                std.log.err("Error appending explicit static library: {}", .{err});
                std.process.exit(1);
            };
        }
    }

    // lib: link with dependencies / get headers from Build.act

    libActonProject.root_module.linkLibrary(actonbase_dep.artifact("Acton"));
    libActonProject.root_module.link_libc = true;
    libActonProject.root_module.link_libcpp = true;
    b.installArtifact(libActonProject);

    for (explicit_static_libraries.items) |libActonExplicit| {
        libActonExplicit.root_module.linkLibrary(actonbase_dep.artifact("Acton"));
    }

    for (explicit_libraries.items) |libActonExplicit| {
        libActonExplicit.root_module.link_libc = true;
        libActonExplicit.root_module.link_libcpp = true;
        b.installArtifact(libActonExplicit);
    }

    // Register the produced header files in out/types using
    // libActonProject.installHeader / .installHeaderDirectory
    // TODO: We should install out/types/*.h with installHeadersDirectory, but
    // it's not working, we get cache issue where out-of-date files are being
    // used. It works the first time but changes to the headers are often not
    // picked up. Iterating and installing each file separately seems to work.
    // Rather surprisingly, I don't see the same issue with the header files
    // from builtin above, but if errors were to occur, we could do the same
    // for those files as well. Obviously, this is not ideal and we should
    // investigate further, find the root cause and address it.
    libActonProject.installHeadersDirectory(b.path("out/types"), "out/types", .{});

    var hiter_dir = b.build_root.handle.openDir(io, "out/types/", .{ .iterate = true }) catch unreachable;
    var hwalker = hiter_dir.walk(b.allocator) catch unreachable;
    defer hwalker.deinit();

    // Find all .h files
    while (true) {
        const next_result = hwalker.next(io) catch unreachable;
        if (next_result) |entry| {
            if (entry.kind == .file) {
                if (std.mem.endsWith(u8, entry.basename, ".h")) {
                    const file_path = std.fs.path.join(b.allocator, &.{ "out/types", entry.path }) catch unreachable;
                    libActonProject.installHeader(b.path(file_path), file_path);
                }
            }
        } else {
            break;
        }
    }

    if (root_c_files.items.len > 0) {
        const maybe_actondb_dep = if (db) b.dependency("actondb", .{
            .target = target,
            .optimize = optimize,
        }) else null;

        for (root_c_files.items) |entry| {
            // Get the binary name, by removing .root.c from end and having it relative to the projpath_outtypes
            var nlen = ".root.c".len;
            if (entry.test_root)
                nlen = ".test_root.c".len - ".test_".len;
            nlen += 1; // for the null terminator
            const binname = b.allocator.alloc(u8, entry.file_path.len-nlen) catch |err| {
                std.log.info("Error allocating binname: {}", .{err});
                std.process.exit(1);
            };
            if (entry.test_root) {
                // Write '.test_' to start of binname
                const buf = std.fmt.allocPrint(b.allocator, ".test_{s}", .{entry.file_path[1..entry.file_path.len - ".test_root.c".len]}) catch |err| {
                    std.log.err("Error allocating binname: {}", .{err});
                    std.process.exit(1);
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
                .root_module = b.createModule(.{
                    .target = target,
                    .optimize = optimize,
                }),
            });
            // Build relative path for the executable source file
            const exe_rel_path = b.allocator.alloc(u8, 9 + entry.file_path.len) catch @panic("OOM");
            @memcpy(exe_rel_path[0..9], "out/types");
            @memcpy(exe_rel_path[9..], entry.file_path);
            executable.root_module.addCSourceFile(.{ .file = b.path(exe_rel_path), .flags = flags.items });
            executable.root_module.addIncludePath(b.path("."));
            executable.root_module.linkLibrary(libActonProject);
            for (explicit_libraries.items) |libActonExplicit| {
                if (libActonExplicit.linkage == .dynamic) {
                    executable.root_module.addObjectFile(libActonExplicit.getEmittedBin());
                } else {
                    executable.root_module.linkLibrary(libActonExplicit);
                }
            }
            if (explicit_libraries.items.len > 0) {
                switch (target.result.os.tag) {
                    .macos => {
                        executable.root_module.addRPathSpecial("@executable_path/../lib");
                        executable.root_module.addRPathSpecial("@executable_path/lib");
                        executable.root_module.addRPathSpecial("@executable_path");
                    },
                    .linux => {
                        executable.root_module.addRPathSpecial("$ORIGIN/../lib");
                        executable.root_module.addRPathSpecial("$ORIGIN/lib");
                        executable.root_module.addRPathSpecial("$ORIGIN");
                    },
                    else => {},
                }
            }

            executable.root_module.linkLibrary(actonbase_dep.artifact("Acton"));
            if (maybe_actondb_dep) |actondb_dep| {
                executable.root_module.linkLibrary(actondb_dep.artifact("ActonDB"));
            }

            // exe: link with dependencies / get headers from Build.act

            executable.root_module.link_libc = true;
            executable.root_module.link_libcpp = true;
            b.installArtifact(executable);
        }
    }
}
