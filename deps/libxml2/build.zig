const std = @import("std");
const print = @import("std").debug.print;

pub fn build(b: *std.Build) void {
    const optimize = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});
    const t = target.result;

    var flags = std.ArrayList([]const u8).empty;
    defer flags.deinit(b.allocator);

    const lib = b.addLibrary(.{
        .name = "xml2",
        .linkage = .static,
        .root_module = b.createModule(.{
            .target = target,
            .optimize = optimize,
        }),
    });
    lib.addIncludePath(b.path("include"));

    const libxml_version = "2.12.0";

    const config_header = b.addConfigHeader(
        .{
            .style = .blank,
        },
        .{
            .ATTRIBUTE_DESTRUCTOR = false,
            .HAVE_ARPA_INET_H = if (t.os.tag == .windows) null else true,
            .HAVE_DLFCN_H = true,
            .HAVE_DLOPEN = true,
            .HAVE_DL_H = true,
            .HAVE_FCNTL_H = true,
            .HAVE_FTIME = true,
            .HAVE_GETTIMEOFDAY = true,
            .HAVE_INTTYPES_H = true,
            .HAVE_ISASCII = true,
            .HAVE_LIBHISTORY = false,
            .HAVE_LIBREADLINE = false,
            .HAVE_MMAP = true,
            .HAVE_MUNMAP = true,
            .HAVE_NETDB_H = true,
            .HAVE_NETINET_IN_H = true,
            .HAVE_POLL_H = true,
            .HAVE_PTHREAD_H = true,
            .HAVE_RAND_R = if (t.os.tag == .windows) null else true,
            .HAVE_SHLLOAD = true,
            .HAVE_STAT = true,
            .HAVE_STDINT_H = true,
            .HAVE_SYS_MMAN_H = true,
            .HAVE_SYS_SELECT_H = true,
            .HAVE_SYS_SOCKET_H = true,
            .HAVE_SYS_STAT_H = true,
            .HAVE_SYS_TIMEB_H = true,
            .HAVE_SYS_TIME_H = true,
            .HAVE_UNISTD_H = true,
            .HAVE_VA_COPY = true,
            .HAVE_ZLIB_H = false,
            .HAVE___VA_COPY = true,
            .LT_OBJDIR = ".libs/",
            .SUPPORT_IP6 = false,
            .VA_LIST_IS_ARRAY = false,
            .VERSION = libxml_version,
        },
    );
    lib.addConfigHeader(config_header);

    flags.appendSlice(b.allocator, &.{
        "-DLIBXML_STATIC",
        "-DLIBXML_VERSION=201200",
        "-DLIBXML_VERSION_STRING=\"" ++ libxml_version ++ "\"",
        "-DLIBXML_VERSION_EXTRA=",
    }) catch unreachable;

    const source_files = [_][]const u8{
        "buf.c",
        "c14n.c",
        "catalog.c",
        "chvalid.c",
        "debugXML.c",
        "dict.c",
        "encoding.c",
        "entities.c",
        "error.c",
        "globals.c",
        "hash.c",
        "HTMLparser.c",
        "HTMLtree.c",
        "legacy.c",
        "list.c",
        "nanoftp.c",
        "nanohttp.c",
        "parser.c",
        "parserInternals.c",
        "pattern.c",
        "relaxng.c",
        "SAX.c",
        "SAX2.c",
        "schematron.c",
        "threads.c",
        "tree.c",
        "uri.c",
        "valid.c",
        "xinclude.c",
        "xlink.c",
        "xmlIO.c",
        "xmlmemory.c",
        "xmlmodule.c",
        "xmlreader.c",
        "xmlregexp.c",
        "xmlsave.c",
        "xmlschemas.c",
        "xmlschemastypes.c",
        "xmlstring.c",
        "xmlunicode.c",
        "xmlwriter.c",
        "xpath.c",
        "xpointer.c",
        "xzlib.c",
    };

    lib.addCSourceFiles(.{
        .files = &source_files,
        .flags = flags.items
    });
    lib.linkLibC();
    lib.installHeadersDirectory(b.path("include/libxml"), "libxml", .{});

    b.installArtifact(lib);
}
