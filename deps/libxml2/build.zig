const std = @import("std");

const version = "2.15.4";
const version_number = 21504;

pub fn build(b: *std.Build) void {
    const optimize = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});
    const enable_lto = optimize != .Debug and target.result.os.tag != .macos;
    const t = target.result;

    const lib = b.addLibrary(.{
        .name = "xml2",
        .linkage = .static,
        .root_module = b.createModule(.{
            .target = target,
            .optimize = optimize,
        }),
    });
    if (enable_lto) lib.lto = .thin;
    lib.root_module.addIncludePath(b.path("include"));

    // getentropy() arrived in glibc 2.25; libxml2 falls back to a seed
    // from the time and addresses without it.
    const have_getentropy = switch (t.os.tag) {
        .windows => false,
        .linux => !t.isGnuLibC() or
            t.os.version_range.linux.glibc.order(.{ .major = 2, .minor = 25, .patch = 0 }) != .lt,
        else => true,
    };

    const config_header = b.addConfigHeader(.{
        .style = .{ .cmake = b.path("config.h.cmake.in") },
        .include_path = "config.h",
    }, .{
        .HAVE_DECL_GETENTROPY = have_getentropy,
        .HAVE_DECL_GLOB = t.os.tag != .windows,
        .HAVE_DECL_MMAP = t.os.tag != .windows,
        .HAVE_FUNC_ATTRIBUTE_DESTRUCTOR = null,
        .HAVE_DLOPEN = null,
        .HAVE_LIBHISTORY = null,
        .HAVE_LIBREADLINE = null,
        .HAVE_SHLLOAD = null,
        .HAVE_STDINT_H = true,
        .XML_SYSCONFDIR = null,
        // Keeps the random state for dictionary seeds per thread, so that
        // parsing writes no shared state without thread support.
        .XML_THREAD_LOCAL = "_Thread_local",
    });
    lib.root_module.addConfigHeader(config_header);

    // Only the core parser and tree: none of the optional modules, as the
    // xml module needs nothing else.
    const version_header = b.addConfigHeader(.{
        .style = .{ .autoconf_at = b.path("include/libxml/xmlversion.h.in") },
        .include_path = "libxml/xmlversion.h",
    }, .{
        .VERSION = version,
        .LIBXML_VERSION_NUMBER = version_number,
        .LIBXML_VERSION_EXTRA = "",
        .MODULE_EXTENSION = ".so",
        .WITH_C14N = false,
        .WITH_CATALOG = false,
        .WITH_DEBUG = false,
        .WITH_HTML = false,
        .WITH_HTTP = false,
        .WITH_ICONV = false,
        .WITH_ICU = false,
        .WITH_ISO8859X = false,
        .WITH_MODULES = false,
        .WITH_OUTPUT = false,
        .WITH_PATTERN = false,
        .WITH_PUSH = false,
        .WITH_READER = false,
        .WITH_REGEXPS = false,
        .WITH_RELAXNG = false,
        .WITH_SAX1 = false,
        .WITH_SCHEMAS = false,
        .WITH_SCHEMATRON = false,
        .WITH_THREAD_ALLOC = false,
        .WITH_THREADS = false,
        .WITH_VALID = false,
        .WITH_WRITER = false,
        .WITH_XINCLUDE = false,
        .WITH_XPATH = false,
        .WITH_XPTR = false,
        .WITH_ZLIB = false,
    });
    lib.root_module.addConfigHeader(version_header);
    lib.installConfigHeader(version_header);

    const source_files = [_][]const u8{
        "buf.c",
        "chvalid.c",
        "dict.c",
        "encoding.c",
        "entities.c",
        "error.c",
        "globals.c",
        "hash.c",
        "list.c",
        "parser.c",
        "parserInternals.c",
        "SAX2.c",
        "threads.c",
        "tree.c",
        "uri.c",
        "valid.c",
        "xmlIO.c",
        "xmlmemory.c",
        "xmlstring.c",
    };

    lib.root_module.addCSourceFiles(.{ .files = &source_files, .flags = &.{"-DLIBXML_STATIC"} });
    lib.root_module.link_libc = true;
    // xmlInitParser seeds its random numbers with BCryptGenRandom.
    if (t.os.tag == .windows) lib.root_module.linkSystemLibrary("bcrypt", .{});
    lib.installHeadersDirectory(b.path("include/libxml"), "libxml", .{ .include_extensions = &.{".h"} });

    b.installArtifact(lib);
}
