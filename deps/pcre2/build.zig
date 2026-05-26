const std = @import("std");

pub const CodeUnitWidth = enum {
    @"8",
    @"16",
    @"32",
};

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const linkage = b.option(std.builtin.LinkMode, "linkage", "whether to statically or dynamically link the library") orelse @as(std.builtin.LinkMode, if (target.result.isGnuLibC()) .dynamic else .static);
    const codeUnitWidth = b.option(CodeUnitWidth, "code-unit-width", "Sets the code unit width") orelse .@"8";

    const copyFiles = b.addWriteFiles();
    _ = copyFiles.addCopyFile(b.path("src/config.h.generic"), "config.h");
    _ = copyFiles.addCopyFile(b.path("src/pcre2.h.generic"), "pcre2.h");

    const lib = b.addLibrary(.{
        .name = b.fmt("pcre2-{s}", .{@tagName(codeUnitWidth)}),
        .linkage = linkage,
        .root_module = b.createModule(.{
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
    });

    if (linkage == .static) {
        try lib.root_module.c_macros.append(b.allocator, "-DPCRE2_STATIC");
    }

    lib.root_module.addCMacro("PCRE2_CODE_UNIT_WIDTH", @tagName(codeUnitWidth));

    var cflags = std.ArrayList([]const u8).empty;
    defer cflags.deinit(b.allocator);
    try cflags.appendSlice(b.allocator, &.{
        "-DHAVE_CONFIG_H",
        "-DPCRE2_STATIC",
        "-DMAX_VARLOOKBEHIND=255",
        "-DSUPPORT_UNICODE",
    });
    switch (codeUnitWidth) {
        .@"8" => try cflags.append(b.allocator, "-DSUPPORT_PCRE2_8"),
        .@"16" => try cflags.append(b.allocator, "-DSUPPORT_PCRE2_16"),
        .@"32" => try cflags.append(b.allocator, "-DSUPPORT_PCRE2_32"),
    }

    lib.root_module.addCSourceFile(.{
        .file = copyFiles.addCopyFile(b.path("src/pcre2_chartables.c.dist"), "pcre2_chartables.c"),
        .flags = cflags.items,
    });

    lib.root_module.addIncludePath(b.path("src"));
    lib.root_module.addIncludePath(copyFiles.getDirectory());

    lib.root_module.addCSourceFiles(.{
        .files = &.{
            "src/pcre2_auto_possess.c",
            "src/pcre2_chkdint.c",
            "src/pcre2_compile.c",
            "src/pcre2_config.c",
            "src/pcre2_context.c",
            "src/pcre2_convert.c",
            "src/pcre2_dfa_match.c",
            "src/pcre2_error.c",
            "src/pcre2_extuni.c",
            "src/pcre2_find_bracket.c",
            "src/pcre2_maketables.c",
            "src/pcre2_match.c",
            "src/pcre2_match_data.c",
            "src/pcre2_newline.c",
            "src/pcre2_ord2utf.c",
            "src/pcre2_pattern_info.c",
            "src/pcre2_script_run.c",
            "src/pcre2_serialize.c",
            "src/pcre2_string_utils.c",
            "src/pcre2_study.c",
            "src/pcre2_substitute.c",
            "src/pcre2_substring.c",
            "src/pcre2_tables.c",
            "src/pcre2_ucd.c",
            "src/pcre2_valid_utf.c",
            "src/pcre2_xclass.c",
        },
        .flags = cflags.items,
    });

    lib.installHeader(b.path("src/pcre2.h.generic"), "pcre2.h");
    b.installArtifact(lib);
}
