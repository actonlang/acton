const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    if (target.result.os.tag != .linux and target.result.os.tag != .macos) {
        std.log.err("malloc=mimalloc requires a Linux or macOS target", .{});
        std.process.exit(1);
    }

    const module = b.createModule(.{
        .target = target,
        .optimize = optimize,
        .link_libc = true,
        .pic = true,
    });
    // An object, linked before the application libraries, reliably overrides
    // malloc without relying on archive extraction order. Keep one allocator
    // in the final executable, not a copy in each Acton library.
    const lib = b.addObject(.{
        .name = "mimalloc",
        .root_module = module,
    });
    if (optimize != .Debug and target.result.os.tag != .macos) lib.lto = .thin;

    var flags = std.ArrayList([]const u8).empty;
    defer flags.deinit(b.allocator);
    flags.appendSlice(b.allocator, &.{ "-DMI_MALLOC_OVERRIDE", "-fno-builtin-malloc" }) catch unreachable;
    if (optimize != .Debug) flags.appendSlice(b.allocator, &.{ "-DNDEBUG", "-DMI_DEBUG=0" }) catch unreachable;
    if (target.result.os.tag == .macos) {
        // Register the allocator with Darwin's malloc zones as well, so
        // allocations exchanged with system libraries use compatible frees.
        flags.append(b.allocator, "-DMI_OSX_ZONE=1") catch unreachable;
    } else {
        flags.append(b.allocator, "-D_GNU_SOURCE") catch unreachable;
        module.linkSystemLibrary("pthread", .{});
        module.linkSystemLibrary("rt", .{});
        if (target.result.abi.isMusl()) {
            flags.appendSlice(b.allocator, &.{ "-DMI_LIBC_MUSL=1", "-ftls-model=local-dynamic" }) catch unreachable;
        } else {
            flags.append(b.allocator, "-ftls-model=initial-exec") catch unreachable;
        }
    }
    module.addIncludePath(b.path("include"));
    module.addCSourceFile(.{ .file = b.path("src/static.c"), .flags = flags.items });
    b.getInstallStep().dependOn(&b.addInstallArtifact(lib, .{ .dest_dir = .{ .override = .lib } }).step);
}
