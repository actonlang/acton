const std = @import("std");
const print = @import("std").debug.print;

pub fn build(b: *std.build.Builder) void {
    const optimize = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});

    const lib = b.addStaticLibrary(.{
        .name = "uuid",
        .target = target,
        .optimize = optimize,
    });

    const source_files = [_][]const u8{
        "src/clear.c",
        "src/compare.c",
        "src/copy.c",
        "src/gen_uuid.c",
        "src/isnull.c",
        "src/pack.c",
        "src/parse.c",
        "src/predefined.c",
        "src/unpack.c",
        "src/unparse.c",
        "src/uuid_time.c",
        // common
        "src/md5.c",
        "src/sha1.c",
        "src/randutils.c",
    };

    lib.addCSourceFiles(&source_files, &[_][]const u8{
        "-DHAVE_NANOSLEEP",
        "-DHAVE_SYS_FILE_H",
        "-DUSLEEP",
    });
    lib.addIncludePath(.{ .path = "include" });
    lib.linkLibC();
    b.installFile("src/uuid.h", "include/uuid/uuid.h");
    b.installArtifact(lib);
}
