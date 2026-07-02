const std = @import("std");

// macOS: shared pthread mutexes inside lock.mdb instead of upstream's POSIX
// named semaphores, whose names live in a global kernel table
// (kern.posix.sem.max, default 10000) where entries stranded by killed
// processes and deleted caches survive until reboot - eventually failing
// every locking mdb_env_open on the machine with ENOSPC ("No space left on
// device"). macOS has no robust mutexes, but named semaphores are not robust
// either, so behaviour on a crashed lock holder is unchanged. Upstream made
// the same flip for FreeBSD >= 11.
//
// mdb.c hard-selects the semaphores on Apple systems before any -D override
// is consulted, tripping its "Ambiguous shared-lock implementation" guard
// when building with -DMDB_USE_POSIX_MUTEX. Rather than carrying a fork or a
// vendored copy for one line, keep the fetched tarball pristine and guard
// that selection at build time; the exact-match replacement fails the build
// loudly if upstream ever changes the block.
const sem_selection =
    "#elif defined(__APPLE__) || defined (BSD) || defined(__FreeBSD_kernel__)\n" ++
    "# define MDB_USE_POSIX_SEM\t1\n";
const sem_selection_guarded =
    "#elif defined(__APPLE__) || defined (BSD) || defined(__FreeBSD_kernel__)\n" ++
    "# ifndef MDB_USE_POSIX_MUTEX\n" ++
    "#  define MDB_USE_POSIX_SEM\t1\n" ++
    "# endif\n";

pub fn build(b: *std.Build) void {
    const optimize = b.standardOptimizeOption(.{});
    var target = b.standardTargetOptions(.{});

    // On a native macOS build zig defaults the deployment target to the build
    // host's SDK version. That tags the archive with whatever the build machine
    // happens to run and makes ld64 warn "object built for newer macOS version"
    // when GHC later links acton against an older minos. Pin a conservative floor
    // so the archive is reproducible across hosts and links cleanly. An explicit
    // -Dtarget=...-macos.<ver> still wins.
    if (target.result.os.tag == .macos and target.query.os_version_min == null) {
        var query = target.query;
        query.os_version_min = .{ .semver = .{ .major = 11, .minor = 0, .patch = 0 } };
        target = b.resolveTargetQuery(query);
    }

    const source = b.dependency("source", .{});

    const lib = b.addLibrary(.{
        .name = "lmdb",
        .linkage = .static,
        .root_module = b.createModule(.{
            .target = target,
            .optimize = optimize,
        }),
    });

    lib.root_module.link_libc = true;
    lib.root_module.addIncludePath(source.path("libraries/liblmdb"));
    const c_flags = [_][]const u8{"-DMDB_MAXKEYSIZE=512"};
    const c_flags_macos = [_][]const u8{ "-DMDB_MAXKEYSIZE=512", "-DMDB_USE_POSIX_MUTEX=1", "-DMDB_USE_ROBUST=0" };
    const flags: []const []const u8 = if (target.result.os.tag == .macos) &c_flags_macos else &c_flags;
    lib.root_module.addCSourceFile(.{ .file = patchedMdbC(b, source), .flags = flags });
    lib.root_module.addCSourceFile(.{ .file = source.path("libraries/liblmdb/midl.c"), .flags = flags });
    lib.installHeader(source.path("libraries/liblmdb/lmdb.h"), "lmdb.h");

    b.installArtifact(lib);
}

fn patchedMdbC(b: *std.Build, source: *std.Build.Dependency) std.Build.LazyPath {
    const raw = source.builder.build_root.handle.readFileAlloc(b.graph.io, "libraries/liblmdb/mdb.c", b.allocator, .limited(1 << 22)) catch |err|
        std.debug.panic("lmdb: reading upstream mdb.c: {}", .{err});
    if (std.mem.count(u8, raw, sem_selection) != 1)
        @panic("lmdb: upstream mdb.c changed its lock-implementation selection; update the patch in build.zig");
    const patched = std.mem.replaceOwned(u8, b.allocator, raw, sem_selection, sem_selection_guarded) catch @panic("OOM");
    return b.addWriteFiles().add("mdb.c", patched);
}
